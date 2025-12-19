## Logical background-position/size mapping

- Current state: Implemented. Logical background position and size longhands map to physical axes per `writing-mode`, including sideways modes. Regression coverage lives in `tests/style/background_position_logical_test.rs` (inline/block position/size for horizontal-tb, vertical-rl, and sideways cases).
- Relevant code: parsing and mapping in `src/style/properties.rs` (background-position-inline/block, background-size-inline/block); defaults in `BackgroundLayer` in `src/style/types.rs`.
- Verification: `cargo test --quiet --test style_tests -- background_position_logical`.
- Status: DONE (see commits 50712c0 and f125d7a/e771e85).
