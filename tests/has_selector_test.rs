// Dedicated integration test target for :has() selector behavior.
//
// Most style regression tests live under `tests/style/` and are pulled into the single
// `style_tests` crate for faster compilation. The task-level verification command for this change
// expects `cargo test --test has_selector_test`, so we compile the existing style test module as a
// standalone integration test crate here.
#[path = "style/has_selector_test.rs"]
mod has_selector_test;
