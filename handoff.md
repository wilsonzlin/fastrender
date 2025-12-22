## Summary

- Implemented paragraph-level handling for `text-wrap: balance`, `pretty`, and `stable` in the inline formatting context. Balancing now re-runs line construction with measured width factors, scoring raggedness per paragraph, penalizing hyphenated endings and short last lines, and equalizing first-line width for balance/pretty/stable.
- Added stability mode that always lays out using a conservative 90% effective inline width to keep breakpoints steady across small width changes, while still respecting bidi/replaced items. Balancing is skipped when floats shorten widths to avoid oscillation.
- Added helper utilities for measuring line raggedness/hyphenation and new regression tests (Latin, CJK, hyphenation on/off, pretty, stable) validating balanced behavior and deterministic stable breaks.

## Testing

- `cargo test text_wrap_ -- --nocapture`

## Notes

- Only `src/layout/contexts/inline/mod.rs` was touched. Scratchpad remains untracked.
