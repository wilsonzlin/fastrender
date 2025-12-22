## Summary

- Reworked fragmentation planning to honor break hints (`before/after/inside`), widows/orphans, and avoid-inside blocks while propagating fragment metadata. Fragments now account for fragmentainer gaps and are translated per fragmentainer for correct stacking.
- Added pagination-aware layout config and plumbed fragmentainer block-size constraints so pagination can fall back to fragmentainer-height slices when no explicit break candidates exist.
- Implemented paragraph-level handling for `text-wrap: balance`, `pretty`, and `stable`, re-running line construction with measured width factors and raggedness scoring while penalizing hyphenated endings and short last lines. Stability uses a conservative 90% effective inline width; balancing is skipped when floats shorten widths to avoid oscillation. Regression tests now cover Latin/CJK, hyphenation on/off, pretty, stable, and gated helpers to avoid dead code warnings.

## Commits

- `6d404bf` Rework fragmentation handling and tests
- `cb9b24c` Add pagination config and improve fragmentation planning
- `d0f48b5` Implement text-wrap balance, pretty, and stable
- `e10517b` Refine balance helpers and gate test utilities
- `c51ae16` Update handoff with final details

## Testing

- Not run (workers ran `cargo test fragmentation --quiet` and `cargo test text_wrap_ -- --nocapture`).

## Notes / Caveats

- Fragmentation remains a post-layout pass; column-count options are not yet integrated beyond metadata/translation. Fragmentainer coordinates are offset by `(fragmentainer_size + gap) * index`.
