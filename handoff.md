## Summary

- Rewrote `src/layout/fragmentation.rs` to plan fragmentainer breaks using style hints (`break-before/after/inside`, widows/orphans) and added fragmentainer gaps plus metadata propagation. Fragments are now translated per fragmentainer to give absolute stacking positions and avoid splitting avoid/widow/orphan blocks when possible.
- Updated fragmentation regression tests to cover gaps, forced breaks, avoid-inside rules, widows/orphans, and positioned content placement. Added multicolumn regressions for span segments and nested column containers.

## Commits

- `6d404bf` Rework fragmentation handling and tests

## Testing

- `cargo test fragmentation --quiet`

## Notes / Caveats

- Fragmentation still operates as a post-layout pass; column-count options are not yet integrated beyond metadata/translation. Fragmentainer coordinates are now offset by `(fragmentainer_size + gap) * index`, which may affect consumers expecting zero-based origins.
