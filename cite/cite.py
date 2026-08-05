#!/usr/bin/env python3
from __future__ import annotations
import os
import re
import json
from datetime import datetime, timezone
from typing import Any, Dict, List

import requests
import yaml

ORCID_API = "https://pub.orcid.org/v3.0"
CROSSREF_API = "https://api.crossref.org/works"


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
        try:
            r = s.get(f"{CROSSREF_API}/{doi}", timeout=30)
            if r.status_code != 200:
                continue
            msg = r.json().get("message", {})
            authors = []
            for a in msg.get("author", []):
                given = a.get("given", "")
                family = a.get("family", "")
                full = " ".join(x for x in [given, family] if x).strip()
                if full:
                    authors.append(full)

            container = msg.get("container-title", [])
            journal = container[0] if container else None

            date_parts = safe_get(msg, "issued", "date-parts", default=[])
            if (not w.get("year")) and date_parts and date_parts[0]:
                w["year"] = int(date_parts[0][0])

            w["authors"] = authors
            w["journal"] = journal
            w["url"] = w.get("url") or msg.get("URL")
        except Exception:
            continue
    return works


def sort_key(w: Dict[str, Any]):
    return (w.get("year") or 0, w.get("month") or 0, w.get("day") or 0, (w.get("title") or "").lower())


def main():
    orcid_id = os.getenv("ORCID_ID")
    if not orcid_id:
        raise SystemExit("Missing ORCID_ID")
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
                "doi": doi,
                "doi_url": f"https://doi.org/{doi}" if doi else None,
                "url": w.get("url"),
                "type": w.get("type"),
            }
        )

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
