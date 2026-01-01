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

### Summary (current `main`)

The audit currently scans **146** cached pages (3 cached HTML files are missing:
`nhk.or.jp`, `tesco.com`, `washingtonpost.com`).

- **DOM text only:** **177** uncovered codepoints.
  - `wikipedia.org`: 121 (mostly Wikipedia language list samples for scripts we do not bundle yet,
    e.g. Armenian `U+0531` "Ա", Tibetan `U+0F40` "ཀ", etc.)
  - `bbc.com`: 36 (language links using Gurmukhi/Gujarati/Sinhala/Ethiopic, e.g. `Punjabi ਪੰਜਾਬੀ`,
    `Gujarati ગુજરાતીમાં`, `Sinhala සිංහල`, `Amharic ዜና በአማርኛ`).
  - `sina.com.cn`: 5 (mojibake/control codepoints like `U+0081`).
- **With `--include-css-content`:** **388** uncovered codepoints.
  - `howtogeek.com`: 209, almost entirely Private Use Area glyphs (`U+E900..`) from icon-font
    pseudo-elements (`content: "\e9xx"`). These are expected to be drawn by site-provided webfonts
    rather than the bundled fallback fonts.
  - Remaining uncovered codepoints largely match the DOM-text-only scan (`wikipedia.org`/`bbc.com`).

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
existing fixture glyphs so bundled-only runs do not render tofu for common emoji.

### Follow-ups (keep pageset-driven)

If pageset diffs show that the remaining missing scripts are significant, consider adding new Noto
subsets + `Script` variants (e.g. Gurmukhi/Gujarati/Sinhala/Ethiopic) rather than expanding the
core Latin font.

For `--include-css-content`, large runs of PUA codepoints typically imply an icon font; they should
only be addressed if we confirm that the pageset run is not fetching/using the page’s webfonts.
