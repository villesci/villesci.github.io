#!/usr/bin/env python3
from __future__ import annotations
import os
import re
import sys
import json
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List

import requests
import yaml

ORCID_API = "https://pub.orcid.org/v3.0"
CROSSREF_API = "https://api.crossref.org/works"
OVERRIDES_PATH = Path(__file__).parent / "overrides.yaml"


def safe_get(d: Dict[str, Any], *keys, default=None):
    cur = d
    for k in keys:
        if not isinstance(cur, dict) or k not in cur:
            return default
        cur = cur[k]
    return cur


def normalize_doi(raw: str | None) -> str | None:
    if not raw:
        return None
    raw = raw.strip()
    raw = re.sub(r"^https?://(dx\.)?doi\.org/", "", raw, flags=re.I)
    return raw.lower() or None


def fetch_orcid_works(orcid_id: str, headers: Dict[str, str]) -> List[Dict[str, Any]]:
    r = requests.get(f"{ORCID_API}/{orcid_id}/works", headers=headers, timeout=30)
    r.raise_for_status()
    data = r.json()
    works: List[Dict[str, Any]] = []

    for group in data.get("group", []):
        summary = (group.get("work-summary") or [{}])[0]
        title = safe_get(summary, "title", "title", "value", default="Untitled")
        pub_date = safe_get(summary, "publication-date", default={}) or {}
        ext_ids = safe_get(summary, "external-ids", "external-id", default=[]) or []

        doi, url_value = None, None
        for ext in ext_ids:
            typ = (ext.get("external-id-type") or "").lower()
            val = ext.get("external-id-value")
            if typ == "doi" and val:
                doi = normalize_doi(val)
            if typ in {"uri", "url"} and val and not url_value:
                url_value = val

        def to_int(v):
            return int(v) if v and str(v).isdigit() else None

        works.append(
            {
                "title": title,
                "year": to_int(safe_get(pub_date, "year", "value")),
                "month": to_int(safe_get(pub_date, "month", "value")),
                "day": to_int(safe_get(pub_date, "day", "value")),
                "type": summary.get("type"),
                "doi": doi,
                "url": url_value,
            }
        )
    return works


def enrich_crossref(works: List[Dict[str, Any]], email: str | None = None) -> List[Dict[str, Any]]:
    s = requests.Session()
    ua = "villesci-publications-bot/0.2"
    if email:
        ua += f" ({email})"
    s.headers.update({"User-Agent": ua})

    for w in works:
        doi = w.get("doi")
        if not doi:
            continue

        # Retry transient failures (rate limiting, timeouts, 5xx) a couple of
        # times before giving up -- a single blip here used to silently leave
        # journal/authors/title blank for the rest of that entry's life,
        # since nothing else ever re-attempts the lookup.
        msg = None
        for attempt in range(3):
            try:
                r = s.get(f"{CROSSREF_API}/{doi}", timeout=30)
                if r.status_code == 200:
                    msg = r.json().get("message", {})
                    break
                if r.status_code in (429, 500, 502, 503, 504):
                    time.sleep(2 * (attempt + 1))
                    continue
                break  # permanent error (e.g. 404 - bad/unregistered DOI)
            except requests.RequestException:
                time.sleep(2 * (attempt + 1))

        if msg is None:
            print(f"Warning: CrossRef lookup failed for DOI {doi}; keeping ORCID data only", file=sys.stderr)
            continue

        try:
            authors = []
            for a in msg.get("author", []):
                given = a.get("given", "")
                family = a.get("family", "")
                full = " ".join(x for x in [given, family] if x).strip()
                if full:
                    authors.append(full)

            container = msg.get("container-title", [])
            journal = container[0] if container else None

            # CrossRef preserves inline markup (e.g. <i>Genus species</i> for
            # taxon names) that ORCID's plain-text title strips out, so prefer
            # it when available. publications.qmd renders p.title as HTML.
            titles = msg.get("title") or []
            if titles and titles[0]:
                w["title"] = titles[0]

            date_parts = safe_get(msg, "issued", "date-parts", default=[])
            if (not w.get("year")) and date_parts and date_parts[0]:
                w["year"] = int(date_parts[0][0])

            w["authors"] = authors
            w["journal"] = journal
            w["volume"] = msg.get("volume")
            w["url"] = w.get("url") or msg.get("URL")
        except Exception as e:
            print(f"Warning: failed to parse CrossRef response for DOI {doi}: {e}", file=sys.stderr)
    return works


def sort_key(w: Dict[str, Any]):
    return (w.get("year") or 0, w.get("month") or 0, w.get("day") or 0, (w.get("title") or "").lower())


def doi_key(doi: str | None) -> str:
    return normalize_doi(doi) or ""


def load_overrides() -> Dict[str, Any]:
    """Manual corrections layered on top of the auto-fetched ORCID/CrossRef
    data (see cite/overrides.yaml). Re-applied on every run so they survive
    the next scheduled refresh."""
    empty = {"type_overrides": {}, "superseded_by": {}, "exclude": set()}
    if not OVERRIDES_PATH.exists():
        return empty
    with open(OVERRIDES_PATH, "r", encoding="utf-8") as f:
        raw = yaml.safe_load(f) or {}
    return {
        "type_overrides": {
            doi_key(k): v for k, v in (raw.get("type_overrides") or {}).items()
        },
        "superseded_by": {
            doi_key(k): doi_key(v) for k, v in (raw.get("superseded_by") or {}).items()
        },
        "exclude": {doi_key(d) for d in (raw.get("exclude") or [])},
        "url_overrides": dict(raw.get("url_overrides") or {}),
        "manual_fields": dict(raw.get("manual_fields") or {}),
    }


def apply_overrides(items: List[Dict[str, Any]], overrides: Dict[str, Any]) -> List[Dict[str, Any]]:
    by_doi = {doi_key(it.get("doi")): it for it in items if it.get("doi")}

    # Force a specific type (e.g. a conference abstract ORCID mis-tagged as "preprint").
    for it in items:
        override_type = overrides["type_overrides"].get(doi_key(it.get("doi")))
        if override_type:
            it["type"] = override_type

    # Attach a manual link for entries with no DOI/URL at all (old ORCID
    # records that predate DOIs, etc.), matched by exact title text.
    for it in items:
        if it.get("url"):
            continue
        manual_url = overrides["url_overrides"].get((it.get("title") or "").strip())
        if manual_url:
            it["url"] = manual_url

    # Fill in metadata CrossRef has no way to supply -- usually because the
    # entry has no DOI to look up, so enrich_crossref() never touches it.
    # Matched by exact ORCID title text; only fills fields that are still
    # empty, so it never clobbers auto-fetched data. This is also how a
    # correctly-formatted (e.g. italicized) title gets attached for entries
    # ORCID only ever gives us as plain text.
    for it in items:
        manual = overrides["manual_fields"].get((it.get("title") or "").strip())
        if not manual:
            continue
        for field, value in manual.items():
            if not it.get(field):
                it[field] = value

    # A preprint that has since been published: attach a "preprint" link to
    # the published entry, then drop the standalone preprint from the list.
    drop = set(overrides["exclude"])
    for preprint_doi, published_doi in overrides["superseded_by"].items():
        drop.add(preprint_doi)
        target = by_doi.get(published_doi)
        if target:
            target["preprint_doi"] = preprint_doi
            target["preprint_url"] = f"https://doi.org/{preprint_doi}"

    return [it for it in items if doi_key(it.get("doi")) not in drop]


def normalize_orcid_id(raw: str) -> str:
    """Accept a bare ORCID iD or a full https://orcid.org/... URL, with
    stray quotes/whitespace tolerated, and return just the iD."""
    candidate = raw.strip().strip("'\"").strip()
    match = re.search(r"\d{4}-\d{4}-\d{4}-\d{3}[\dX]", candidate, flags=re.I)
    if not match:
        raise SystemExit(
            f"ORCID_ID does not look like a valid ORCID iD: {candidate!r}"
        )
    return match.group(0)


def main():
    raw_orcid_id = os.getenv("ORCID_ID")
    if not raw_orcid_id:
        raise SystemExit("Missing ORCID_ID")
    orcid_id = normalize_orcid_id(raw_orcid_id)
    email = os.getenv("EMAIL_FOR_POLITE_POOLING")

    headers = {
        "Accept": "application/json",
        "User-Agent": f"villesci.github.io-publications/0.2 ({email or 'no-contact'})",
    }

    works = fetch_orcid_works(orcid_id, headers)
    works = enrich_crossref(works, email)

    items = []
    for w in works:
        doi = w.get("doi")
        items.append(
            {
                "title": w.get("title"),
                "authors": w.get("authors", []),
                "year": w.get("year"),
                "journal": w.get("journal"),
                "volume": w.get("volume"),
                "doi": doi,
                "doi_url": f"https://doi.org/{doi}" if doi else None,
                "url": w.get("url"),
                "type": w.get("type"),
            }
        )

    items = apply_overrides(items, load_overrides())

    items.sort(key=sort_key, reverse=True)

    payload = {
        "updated_at": datetime.now(timezone.utc).isoformat(),
        "source": {"orcid_id": orcid_id},
        "items": items,
    }

    os.makedirs("_data", exist_ok=True)

    with open("_data/publications.json", "w", encoding="utf-8") as f:
        json.dump(payload, f, ensure_ascii=False, indent=2)

    # optional YAML mirror if you still want Jekyll data access
    with open("_data/publications.yml", "w", encoding="utf-8") as f:
        yaml.safe_dump(payload, f, sort_keys=False, allow_unicode=True)

    print(f"Wrote {len(items)} publications to _data/publications.json")


if __name__ == "__main__":
    main()
