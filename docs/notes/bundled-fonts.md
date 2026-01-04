# Bundled fonts

Bundled renders (CI and `FASTR_USE_BUNDLED_FONTS=1`) must not depend on platform font discovery.
To keep pageset runs hermetic and fast:

- Default fallbacks come from small, subsetted Noto families plus FastRenderEmoji and a bundled
  math face. Noto subsets are regenerated via `python tests/fixtures/fonts/generate_bundled_noto_subsets.py`;
  the emoji fixture is regenerated via `python tests/fixtures/fonts/generate_fastrender_emoji_font.py`
  after installing the pinned Python dependencies in `tests/fixtures/fonts/requirements.txt` (see
  `tests/fixtures/fonts/README.md`), which pins timestamps and freezes CJK weight to 400 before
  subsetting.
- Keep binaries lean: prefer script-specific subsets over shipping whole families, and document the
  ranges covered in `tests/fixtures/fonts/README.md` when adding coverage.
- Generic families should continue to resolve sensibly in bundled-only mode (`sans-serif`/`serif`/
  `monospace` → text faces, `emoji` → FastRenderEmoji, `math` → math-capable face).
- Avoid adding test dependencies on system fonts; bundled fixtures should be enough for common
  scripts (Latin, Greek, Cyrillic, Arabic, Indic, CJK, symbols) during CI and pageset runs.

## Auditing pageset glyph coverage

To make bundled-font subset decisions data-driven, use the offline coverage audit tool:

```bash
# Scan all cached pageset HTML under fetches/html/ using bundled fonts only.
cargo run --release --bin bundled_font_coverage -- --pageset

# Machine-readable JSON (useful for diffs across commits).
cargo run --release --bin bundled_font_coverage -- --pageset --json > coverage.json

# Include inline CSS pseudo-element strings (`content: "..."`).
cargo run --release --bin bundled_font_coverage -- --pageset --include-css-content
```

The report lists every unique Unicode codepoint found in visible DOM text nodes that does not have
any glyph in `FontDatabase::shared_bundled()` (including the bundled emoji face when enabled).

Note: this is strictly a glyph *coverage* check. It does not validate correct shaping, script
tagging, or segmentation for complex scripts (Arabic/Indic/etc) or emoji sequences (see Task 93).

## Pageset audit snapshot

The canonical (cached) pageset HTML lives under `fetches/html/`. For repeatable diffs while working
on bundled glyph coverage, keep the JSON output in an untracked local directory:

```bash
mkdir -p tmp
cargo run --release --bin bundled_font_coverage -- --pageset --json \
  > tmp/bundled_font_coverage.json
cargo run --release --bin bundled_font_coverage -- --pageset --include-css-content --json \
  > tmp/bundled_font_coverage_with_css.json
```

### Example snapshot (will drift)

The exact numbers will vary with the cached pageset HTML in your local `fetches/html/` directory and
the current bundled font set; rerun the commands above for current results. The list below is kept
as an example of the kinds of issues this audit surfaces.

One snapshot scanned **136** cached pages (13 cached HTML files were missing:
`bloomberg.com`, `britannica.com`, `economist.com`, `etsy.com`, `medium.com`, `npmjs.com`,
`openai.com`, `quora.com`, `reddit.com`, `reuters.com`, `tripadvisor.com`, `wsj.com`, `yelp.com`).

Compared to older snapshots, pages like `wikipedia.org`/`bbc.com` no longer dominate the uncovered
list: their language samples (Armenian/Georgian/Ethiopic/Lao/Tibetan/etc) are covered by bundled
Noto script subsets.

- **DOM text only:** **14** uncovered codepoints.
  - `techmeme.com`: 9 (emoji not present in `FastRenderEmoji.ttf`, plus the keycap mark `U+20E3`)
  - `ebay.com`: 4 (emoji, plus `U+FE0F` which is a variation selector used in emoji sequences)
  - `google.com`: 1 (`U+1F34C` "🍌")
  - A few other pages have only `U+FE0F` (e.g. `aliexpress.com`, `buzzfeed.com`, `espn.com`,
    `neverssl.com`)
- **With `--include-css-content`:** **224** uncovered codepoints.
  - `howtogeek.com`: 208, almost entirely Private Use Area glyphs (`U+E900..`) from icon-font
    pseudo-elements (`content: "\e9xx"`). These are expected to be drawn by site-provided webfonts
    rather than the bundled fallback fonts.
  - Remaining uncovered codepoints largely match the DOM-text-only scan (emoji + variation
    selectors). The other notable outlier is `craigslist.org` (one PUA codepoint: `U+EB23`).

### Pageset-motivated fixes validated by the audit

The pageset caches contain:

- Telugu strings such as `తెలుగు` (e.g. `fetches/html/wikipedia.org.html`,
  `fetches/html/linkedin.com.html`)
- Myanmar strings such as `မြန်မာဘာသာ` (e.g. `fetches/html/wikipedia.org.html`)
- The 🇺🇸 emoji sequence `U+1F1FA U+1F1F8` (e.g. `fetches/html/shopify.com.html`)

After bundling `NotoSansTelugu-subset.ttf`, `NotoSansMyanmar-subset.ttf`, and extending
`FastRenderEmoji.ttf` with a GSUB ligature for 🇺🇸, the audit reports **zero uncovered codepoints**
in the Telugu/Myanmar blocks and does not report the regional indicator codepoints used by 🇺🇸.

The audit also previously identified ~40 additional emoji codepoints present across the pageset
(Craigslist/eBay/ESPN/Buzzfeed/etc). The FastRenderEmoji generator maps those codepoints onto the
existing fixture glyphs so bundled-only runs do not render tofu for common emoji; keep that mapping
updated as the pageset changes.

### Follow-ups (keep pageset-driven)

If pageset diffs show that the remaining missing DOM-text emoji are significant, extend
`FastRenderEmoji.ttf`/its generator to cover them (at the time of writing: `🍌`, `⚾`, `💎`, `💜`,
`💩`, `🔴`, `🙏`, `🤣`, `🤮`, `🥳`, `🧵`, `🪬`). Note that the remaining non-emoji uncovered codepoints are
emoji-related marks (`U+20E3`, `U+FE0F`) which are not meaningful standalone glyphs.

For `--include-css-content`, large runs of PUA codepoints typically imply an icon font; they should
only be addressed if we confirm that the pageset run is not fetching/using the page’s webfonts.
