//! Guardrail to ensure conformance targets are documented and enforced.

use std::path::Path;

#[test]
fn conformance_doc_is_present_and_non_empty() {
  let conformance = Path::new(env!("CARGO_MANIFEST_DIR")).join("docs/conformance.md");
  assert!(
    conformance.exists(),
    "docs/conformance.md should exist as the conformance source of truth"
  );

  let content = std::fs::read_to_string(&conformance).expect("read docs/conformance.md");
  assert!(
    !content.trim().is_empty(),
    "docs/conformance.md should not be empty"
  );
}
