use std::fs;
use std::process::Command;

use tempfile::TempDir;

#[test]
fn render_pages_exits_non_zero_when_cache_missing() {
    let temp = TempDir::new().expect("tempdir");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        .status()
        .expect("run render_pages");

    assert_eq!(status.code(), Some(1), "expected failure exit code");
}

#[test]
fn render_pages_exits_non_zero_when_filter_matches_nothing() {
    let temp = TempDir::new().expect("tempdir");
    let html_dir = temp.path().join("fetches/html");
    fs::create_dir_all(&html_dir).expect("create html dir");
    fs::write(html_dir.join("foo.html"), "<!doctype html><title>Foo</title>").expect("write html");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        .args(["--pages", "bar"])
        .status()
        .expect("run render_pages");

    assert_eq!(
        status.code(),
        Some(1),
        "expected failure exit code for unmatched filter"
    );
}

#[test]
fn render_pages_errors_on_unknown_option() {
    let temp = TempDir::new().expect("tempdir");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        .arg("--definitely-not-a-flag")
        .status()
        .expect("run render_pages");

    assert!(
        !status.success(),
        "expected non-zero exit when unknown option is provided (got {:?})",
        status.code()
    );
}

#[test]
fn render_pages_accepts_positional_filters() {
    let temp = TempDir::new().expect("tempdir");
    let html_dir = temp.path().join("fetches/html");
    fs::create_dir_all(&html_dir).expect("create html dir");
    fs::write(html_dir.join("foo.html"), "<!doctype html><title>Foo</title>").expect("write foo");
    fs::write(html_dir.join("bar.html"), "<!doctype html><title>Bar</title>").expect("write bar");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        // positional arg should act as a filter matching foo
        .arg("foo")
        .status()
        .expect("run render_pages with positional filter");

    assert_eq!(status.code(), Some(0), "expected success when positional filter matches");

    let render_dir = temp.path().join("fetches/renders");
    assert!(render_dir.join("foo.png").exists(), "expected foo.png to be rendered");
    assert!(!render_dir.join("bar.png").exists(), "expected bar.png to be skipped");
}
