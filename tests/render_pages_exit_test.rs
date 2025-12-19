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
fn render_pages_errors_on_unknown_option_even_with_cache() {
    let temp = TempDir::new().expect("tempdir");
    let html_dir = temp.path().join("fetches/html");
    fs::create_dir_all(&html_dir).expect("create html dir");
    fs::write(html_dir.join("foo.html"), "<!doctype html><title>Foo</title>").expect("write html");

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
fn render_pages_normalizes_pages_filter_host_case() {
    let temp = TempDir::new().expect("tempdir");
    let html_dir = temp.path().join("fetches/html");
    fs::create_dir_all(&html_dir).expect("create html dir");

    // Cached html uses lowercase stem; filter uses uppercase host to ensure normalization works.
    fs::write(html_dir.join("example.com.html"), "<!doctype html><title>Foo</title>").expect("write html");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        .args(["--pages", "EXAMPLE.COM"])
        .status()
        .expect("run render_pages");

    assert!(status.success(), "expected render_pages to succeed");
    // The renderer should have produced a PNG in fetches/renders using the normalized stem.
    assert!(temp.path().join("fetches/renders/example.com.png").is_file());
}
