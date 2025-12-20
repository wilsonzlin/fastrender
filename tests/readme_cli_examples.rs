//! Guard important README CLI examples from accidental removal.

use std::fs;

#[test]
fn fetch_and_render_example_is_present() {
    let content = fs::read_to_string("README.md").expect("README.md should exist at repository root");
    assert!(content.contains("./target/release/fetch_and_render https://example.com/"),
            "README CLI example for fetch_and_render is missing or altered");
}
