# Bundled fonts

Bundled renders (CI and `FASTR_USE_BUNDLED_FONTS=1`) must not depend on platform font discovery.
To keep pageset runs hermetic and fast:

- Default fallbacks come from small, subsetted Noto families plus FastRenderEmoji and a bundled
  math face. Subsets are regenerated via `python tests/fixtures/fonts/generate_bundled_noto_subsets.py`
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
