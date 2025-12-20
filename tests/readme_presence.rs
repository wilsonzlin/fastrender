//! Ensure the repository README stays present. This guards against accidental
//! delete/revert churn during development.

use std::path::Path;

#[test]
fn readme_is_present() {
    let readme = Path::new(env!("CARGO_MANIFEST_DIR")).join("README.md");
    assert!(readme.exists(), "README.md should exist at the repository root");
}
