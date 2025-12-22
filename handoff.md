## Summary

- Implemented paragraph-level handling for `text-wrap: balance`, `pretty`, and `stable` in the inline formatting context. Balancing re-runs line construction with measured width factors, scoring raggedness per paragraph, penalizing hyphenated endings and short last lines, and equalizing first-line width for balance/pretty/stable.
- Added stability mode that lays out with a conservative 90% effective inline width to keep breakpoints steady across small width changes while respecting bidi and replaced items; balancing is skipped when floats shorten widths to avoid oscillation.
- Added helper utilities for measuring raggedness/hyphenation plus regression tests (Latin, CJK, hyphenation on/off, pretty, stable) validating balanced behavior and deterministic stable breaks. Test-only helpers are now gated to avoid dead-code warnings.

## Testing

- `cargo test text_wrap_ -- --nocapture`

## Notes

- Only `src/layout/contexts/inline/mod.rs` was touched. Scratchpad remains untracked.
