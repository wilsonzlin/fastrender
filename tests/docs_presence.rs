//! Guardrails for internal documentation.

use std::path::Path;

#[test]
fn docs_home_is_present() {
    let docs_home = Path::new(env!("CARGO_MANIFEST_DIR")).join("docs/README.md");
    assert!(docs_home.exists(), "docs/README.md should exist (internal wiki home)");
    let content = std::fs::read_to_string(&docs_home).expect("read docs/README.md");
    assert!(
        !content.trim().is_empty(),
        "docs/README.md should not be empty (internal wiki home)"
    );
}
