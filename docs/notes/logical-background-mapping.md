# Logical background-position/size mapping

Logical `background-position-*` and `background-size-*` longhands map to physical axes per `writing-mode`, including sideways modes.

## Where this is implemented

- Parsing and mapping: `src/style/properties.rs` (`background-position-inline/block`, `background-size-inline/block`)
- Defaults: `BackgroundLayer` in `src/style/types.rs`

## Regression coverage

- Tests: `tests/style/background_position_logical_test.rs`
  - Covers inline/block position/size for `horizontal-tb`, `vertical-rl`, `sideways-lr`, `sideways-rl`
  - Also covers `background-position-x/y` longhands
  - `background-size-y` is deprecated in specs, but is still parsed/mapped and tested here

## Verification

- `cargo test --quiet --test style_tests -- background_position_logical`
