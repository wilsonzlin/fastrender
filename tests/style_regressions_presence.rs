//! Guards against accidental deletion of critical style regression fixtures.

use std::path::Path;

#[test]
fn background_position_logical_regression_is_present() {
  let path =
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/style/background_position_logical_test.rs");
  assert!(
    path.exists(),
    "background_position_logical_test.rs must exist"
  );
  let len = path
    .metadata()
    .expect("stat background_position_logical_test.rs")
    .len();
  assert!(
    len > 0,
    "background_position_logical_test.rs should not be empty"
  );
}
