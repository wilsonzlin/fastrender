//! Guard against accidental README.md deletion or empty file.

#[test]
fn readme_exists_and_is_nonempty() {
    let content = std::fs::read_to_string("README.md").expect("README.md should exist at the repository root");
    assert!(!content.trim().is_empty(), "README.md should not be empty");
}
