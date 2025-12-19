## Logical background-position/size mapping

- Current state: Implemented. Logical background position and size longhands map to physical axes per `writing-mode`, including sideways modes. Regression coverage lives in `tests/style/background_position_logical_test.rs` (inline/block position/size for horizontal-tb, vertical-rl, sideways-lr/sideways-rl, and background-position-x/y longhands; background-size covered alongside position). `background-size-y` is deprecated in specs but still parsed/mapped and tested.
- Relevant code: parsing and mapping in `src/style/properties.rs` (background-position-inline/block, background-size-inline/block); defaults in `BackgroundLayer` in `src/style/types.rs`. Tests in `tests/style/background_position_logical_test.rs` (including sideways-lr/sideways-rl coverage) and `tests/style_tests.rs` harness.
- Verification: `cargo test --quiet --test style_tests -- background_position_logical`.
- Status: DONE (see commits 50712c0 and f125d7a/e771e85; sideways-rl tests landed in 8364dee).
