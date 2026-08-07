# Journal cover thumbnails

Drop a cover image here to have it show up automatically on the
[Publications](../../publications.qmd) page, replacing the generic type icon
for every entry in that journal.

**Naming:** slugify the journal name exactly as it appears in
`_data/publications.json` — lowercase, non-alphanumeric runs collapsed to a
single hyphen, no leading/trailing hyphens. For example:

| Journal name                     | Expected filename                |
|-----------------------------------|-----------------------------------|
| Journal of Animal Ecology         | `journal-of-animal-ecology.jpg`   |
| Nature Climate Change             | `nature-climate-change.jpg`       |
| Proceedings of the Royal Society B: Biological Sciences | `proceedings-of-the-royal-society-b-biological-sciences.jpg` |

**Publishing in the same journal again, in a different year/volume?** If you
want that entry to use different cover art than the journal's default image,
drop in a more specific file and it's picked up automatically — the page
tries these from most to least specific, per entry:

1. `<slug>-<year>-<volume>.jpg` — e.g. `journal-of-animal-ecology-2024-93.jpg`
   (volume comes from CrossRef when available)
2. `<slug>-<year>.jpg` — e.g. `journal-of-animal-ecology-2024.jpg`
3. `<slug>.jpg` — the plain, journal-wide default

If two entries land in the same journal *and* year and volume isn't known,
there's no automatic way to tell them apart by filename alone. Name the
image whatever you like (e.g. `journal-of-animal-ecology-2024a.jpg`,
`...2024b.jpg`) and point that specific entry at it with a `cover` override
in [`cite/overrides.yaml`](../../cite/overrides.yaml)'s `manual_fields`
(set it to the filename stem, no extension).

**Preprints, conference papers, and anything else with no journal** fall back
to a generic image keyed by entry type instead of journal name:

| Type              | Expected filename        |
|-------------------|---------------------------|
| preprint           | `preprint.jpg`            |
| conference-paper   | `conference-paper.jpg`    |
| other              | `other.jpg`                |

That's a blanket default for *every* entry of that type, so use it for
something generic (e.g. the preprint server's logo, if all your preprints
land on the same one). If one specific entry needs different art (e.g. a
conference paper's own logo rather than a generic conference icon), use the
`cover` override below instead — it takes priority over everything else.

`.jpg` is tried first, then `.png`, at each step above. If nothing matches,
the entry just shows its type icon — nothing breaks.

There is no reliable, ToS-safe way to auto-crawl publisher sites for cover
art (no universal API, and scraping issue pages is fragile and often against
publisher terms), so this is manually curated. A portrait crop of the
journal's logo or a representative cover works well; aim for roughly a
3:4 aspect ratio (e.g. 200x264px) since it's displayed in a rounded 44x58
tile.
