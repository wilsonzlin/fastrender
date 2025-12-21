// Aggregator for style regression tests under tests/style/.
// Cargo only executes integration test crates at the root of `tests/`, so this
// harness pulls the nested modules into a single test crate.

mod style;
