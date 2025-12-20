//! Guard against accidental deletion of the fetch_and_render exit regression.

use std::path::Path;

#[test]
fn fetch_and_render_exit_regression_is_present() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fetch_and_render_exit_test.rs");
    assert!(path.exists(), "tests/fetch_and_render_exit_test.rs must exist");
    let len = path
        .metadata()
        .expect("stat tests/fetch_and_render_exit_test.rs")
        .len();
    assert!(len > 0, "tests/fetch_and_render_exit_test.rs should not be empty");
}
