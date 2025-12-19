use std::process::Command;

use tempfile::TempDir;

#[test]
fn fetch_pages_exits_non_zero_when_filter_matches_nothing() {
    let temp = TempDir::new().expect("tempdir");

    let status = Command::new(env!("CARGO_BIN_EXE_fetch_pages"))
        .current_dir(temp.path())
        .args(["--pages", "definitely-not-here"])
        .status()
        .expect("run fetch_pages");

    assert_eq!(
        status.code(),
        Some(1),
        "expected failure exit code when filter matches nothing"
    );
}

#[test]
fn fetch_pages_errors_on_unknown_option() {
    let temp = TempDir::new().expect("tempdir");

    let status = Command::new(env!("CARGO_BIN_EXE_fetch_pages"))
        .current_dir(temp.path())
        .arg("--definitely-not-a-flag")
        .status()
        .expect("run fetch_pages");

    assert!(
        !status.success(),
        "expected non-zero exit when unknown option is provided (got {:?})",
        status.code()
    );
}
