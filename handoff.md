## Summary

- Reworked fragmentation planning to honor break hints (`before/after/inside`), widows/orphans, and avoid-inside blocks while propagating fragment metadata. Fragments now account for fragmentainer gaps and are translated per fragmentainer for correct stacking, with pagination-aware config and fragmentainer-height fallback slices when no explicit break candidates exist.
- Implemented `text-wrap: balance`, `pretty`, and `stable` handling that re-runs line construction with measured width factors and raggedness scoring, penalizing hyphenated endings/short last lines. Stability uses a conservative 90% effective inline width and skips balancing when floats shorten widths; regression tests cover Latin/CJK, hyphenation on/off, pretty, and stable modes.
- Added full parsing/resolution for `color-contrast()` (with optional `vs` reference defaulting to currentColor) using WCAG contrast with alpha compositing, plus `color(from …)` relative color syntax across srgb/srgb-linear/hsl/hwb/lab/lch/oklab/oklch/xyz with conversions, channel defaults, and currentColor-aware resolution. Tests cover unit, integration, gradients, and cascade cases.
- Introduced an accessibility tree builder that computes implicit/explicit roles, accessible names/descriptions, state flags (focusable, disabled, required/invalid, visited/pressed, checked/selected), heading levels, and form values while skipping hidden nodes. New APIs expose the tree and JSON output, with tests covering common controls, landmarks, tables, labels, and hidden filtering.
- Added `font-display` descriptor support with async web-font loading that respects block/swap/fallback/optional deadlines, swaps in loaded fonts while avoiding long stalls, filters by `unicode-range`, bumps font generation to invalidate shaping, and exposes pluggable fetchers plus `wait_for_pending_web_fonts`. Tests cover display phases, unicode-range filtering, block waiting, and failure fallback.
- Implemented CSS Media Queries Level 5 features (scripting, update frequency, light-level, display-mode) with env overrides and builder helpers, dynamic viewport units (dvw/dvh/dvmin/dvmax), MediaContext fingerprinting updates, and media link extraction that respects media type (skips print styles in screen contexts). Tests cover MQ5 evaluation/invalids, env overrides, dynamic viewport units, cache invalidation, and print stylesheet handling.
- Added parsing/matching for `:has()` using relative selectors with proper specificity and `:scope` anchoring, plus traversal support for relative combinators and feature-query reporting. Regression coverage includes combinators, `:is`/`:not` interactions, and specificity.

## Commits

- `6d404bf` Rework fragmentation handling and tests
- `cb9b24c` Add pagination config and improve fragmentation planning
- `d0f48b5` Implement text-wrap balance, pretty, and stable
- `e10517b` Refine balance helpers and gate test utilities
- `c51ae16` Update handoff with final details
- `2c46d71` Add color-contrast and relative color support
- `2311c93` Add tests for new color syntaxes
- `c7bf57e` Allow whitespace before relative color from keyword
- `574de39` Document color-contrast and relative color work
- `2a039ea` Add accessibility tree builder and API
- `7a2085e` Add accessibility tree tests
- `f1c0507` Add handoff summary
- `cff1950` Support font-display and unicode-range aware web fonts
- `71466ae` Make web fonts load asynchronously respecting block period
- `b16d4a2` Add handoff summary
- `bbe4a10` Add block-display sync wait and failure regression tests
- `02fb894` Update handoff with new tests
- `3ffe5a2` Add MQ5 media features and dynamic viewport units
- `0968d8d` Add media level 5 and cache tests
- `c744b29` Respect media type when extracting linked stylesheets
- `f944732` Add handoff summary
- `79112ff` Add support for :has() selectors and feature queries
- `69a385d` Add regression tests for relational :has() selectors
- `703252b` Add handoff notes

## Testing

- Not run (workers ran `cargo test fragmentation --quiet`, `cargo test text_wrap_ -- --nocapture`, `cargo test color_contrast -- --nocapture` / `cargo test`, `cargo test accessibility_`, `cargo test font_loader -- --nocapture`, `cargo test media_level5_features_evaluate -- --nocapture`, `cargo test extract_css_links_skips_print_only_for_screen -- --nocapture`, `cargo test has_selector_test -- --nocapture`, and `cargo test supports_selector_test -- --nocapture`).

## Notes / Caveats

- Fragmentation remains a post-layout pass; column-count options are not yet integrated beyond metadata/translation. Fragmentainer coordinates are offset by `(fragmentainer_size + gap) * index`.
- Relative color `calc()` support is minimal (single numeric/percentage values only), only convertible spaces are allowed, and `color-contrast()` lacks `to` thresholds. Contrast evaluation composites transparent colors against white when selecting options.
- Accessibility tree flattens non-semantic containers without names/roles, skips hidden nodes (`display:none`/`visibility:hidden`/`aria-hidden`/`hidden`) for inclusion and label resolution, and uses the first cascade pass (no container-query reruns).
- Font-display timing constants are shortened for tests (block/auto ~300ms; fallback block 100ms + 400ms swap; optional 100ms). Block/auto wait only through the block window, loads continue asynchronously, and initial shaping may use fallbacks until web fonts finish loading.
- Fast-reject bloom filtering for relative selectors is disabled for `:has()` relative traversal to avoid false negatives; caching remains in place.
