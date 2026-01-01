// Aggregator for CLI integration tests under tests/bin/.
// Cargo only executes integration test crates at the root of `tests/`, so this
// harness pulls the nested modules into a single test crate.

mod bin;
mod test_support;
