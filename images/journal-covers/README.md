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

`.jpg` is tried first, then `.png`. If neither exists, the entry just shows
its type icon — nothing breaks.

There is no reliable, ToS-safe way to auto-crawl publisher sites for cover
art (no universal API, and scraping issue pages is fragile and often against
publisher terms), so this is manually curated. A portrait crop of the
journal's logo or a representative cover works well; aim for roughly a
3:4 aspect ratio (e.g. 200x264px) since it's displayed in a rounded 44x58
tile.
