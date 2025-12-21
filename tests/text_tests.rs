// Aggregator for text/shaping regression tests under tests/text/.
// Cargo only executes integration test crates at the root of `tests/`, so this
// harness pulls the nested modules into a single test crate.

mod text;
