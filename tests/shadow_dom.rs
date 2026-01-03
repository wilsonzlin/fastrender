// Standalone harness for Shadow DOM unit tests.
//
// Most tree-related tests live under `tests/tree/` and are aggregated by
// `tests/tree_tests.rs`. Having a dedicated integration test crate allows
// running `cargo test --test shadow_dom` for faster iteration on slotting logic.

#[path = "tree/shadow_dom.rs"]
mod shadow_dom;
