# Bundled fonts

Bundled renders (CI and `FASTR_USE_BUNDLED_FONTS=1`) must not depend on platform font discovery.
To keep pageset runs hermetic and fast:

- Default fallbacks come from small, subsetted Noto families plus FastRenderEmoji and a bundled
  math face. Subsets are regenerated via `python tests/fixtures/fonts/generate_bundled_noto_subsets.py`,
  which pins timestamps and freezes CJK weight to 400 before subsetting.
- Keep binaries lean: prefer script-specific subsets over shipping whole families, and document the
  ranges covered in `tests/fixtures/fonts/README.md` when adding coverage.
- Generic families should continue to resolve sensibly in bundled-only mode (`sans-serif`/`serif`/
  `monospace` → text faces, `emoji` → FastRenderEmoji, `math` → math-capable face).
- Avoid adding test dependencies on system fonts; bundled fixtures should be enough for common
  scripts (Latin, Greek, Cyrillic, Arabic, Indic, CJK, symbols) during CI and pageset runs.
