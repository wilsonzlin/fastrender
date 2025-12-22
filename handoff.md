## Summary

- Reworked fragmentation planning to honor break hints (`before/after/inside`), widows/orphans, and avoid-inside blocks while propagating fragment metadata. Fragments now account for fragmentainer gaps and are translated per fragmentainer for correct stacking, with pagination-aware config and fragmentainer-height fallback slices when no explicit break candidates exist.
- Implemented `text-wrap: balance`, `pretty`, and `stable` handling that re-runs line construction with measured width factors and raggedness scoring, penalizing hyphenated endings/short last lines. Stability uses a conservative 90% effective inline width and skips balancing when floats shorten widths; regression tests cover Latin/CJK, hyphenation on/off, pretty, and stable modes.
- Added full parsing/resolution for `color-contrast()` (with optional `vs` reference defaulting to currentColor) using WCAG contrast with alpha compositing, plus `color(from …)` relative color syntax across srgb/srgb-linear/hsl/hwb/lab/lch/oklab/oklch/xyz with conversions, channel defaults, and currentColor-aware resolution. Tests cover unit, integration, gradients, and cascade cases.

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

## Testing

- Not run (workers ran `cargo test fragmentation --quiet`, `cargo test text_wrap_ -- --nocapture`, and `cargo test color_contrast -- --nocapture` / `cargo test`).

## Notes / Caveats

- Fragmentation remains a post-layout pass; column-count options are not yet integrated beyond metadata/translation. Fragmentainer coordinates are offset by `(fragmentainer_size + gap) * index`.
- Relative color `calc()` support is minimal (single numeric/percentage values only), only convertible spaces are allowed, and `color-contrast()` lacks `to` thresholds. Contrast evaluation composites transparent colors against white when selecting options.
