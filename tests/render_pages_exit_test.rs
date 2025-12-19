use std::fs;
use std::process::Command;

use fastrender::resource::url_to_filename;
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

#[test]
fn render_pages_normalizes_full_url_filter() {
    let temp = TempDir::new().expect("tempdir");
    let html_dir = temp.path().join("fetches/html");
    fs::create_dir_all(&html_dir).expect("create html dir");

    // Cached html uses lowercase stem; filter uses a full URL with uppercase scheme/host and trailing slash.
    fs::write(html_dir.join("example.com.html"), "<!doctype html><title>Foo</title>").expect("write html");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        .args(["--pages", "HTTPS://EXAMPLE.COM/"])
        .status()
        .expect("run render_pages");

    assert!(status.success(), "expected render_pages to succeed");
    assert!(temp.path().join("fetches/renders/example.com.png").is_file());
}

#[test]
fn render_pages_filters_multiple_pages() {
    let temp = TempDir::new().expect("tempdir");
    let html_dir = temp.path().join("fetches/html");
    fs::create_dir_all(&html_dir).expect("create html dir");

    fs::write(html_dir.join("foo.html"), "<!doctype html><title>Foo</title>").expect("write foo html");
    fs::write(html_dir.join("bar.html"), "<!doctype html><title>Bar</title>").expect("write bar html");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        .args(["--pages", "foo,bar"])
        .status()
        .expect("run render_pages");

    assert!(status.success(), "expected render_pages to succeed for matching filters");
    assert!(temp.path().join("fetches/renders/foo.png").is_file());
    assert!(temp.path().join("fetches/renders/bar.png").is_file());
}

#[test]
fn render_pages_accepts_full_url_with_query() {
    let temp = TempDir::new().expect("tempdir");
    let html_dir = temp.path().join("fetches/html");
    fs::create_dir_all(&html_dir).expect("create html dir");

    let url = "https://Example.com/foo?Bar=Baz";
    let stem = url_to_filename(url);
    fs::write(html_dir.join(format!("{}.html", stem)), "<!doctype html><title>Foo</title>")
        .expect("write html");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        .args(["--pages", url])
        .status()
        .expect("run render_pages");

    assert!(status.success(), "expected render_pages to succeed for full URL filter");
    assert!(
        temp.path()
            .join("fetches/renders")
            .join(format!("{}.png", stem))
            .is_file()
    );
}

#[test]
fn render_pages_normalizes_positional_filters() {
    let temp = TempDir::new().expect("tempdir");
    let html_dir = temp.path().join("fetches/html");
    fs::create_dir_all(&html_dir).expect("create html dir");

    fs::write(html_dir.join("example.com.html"), "<!doctype html><title>Foo</title>").expect("write html");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        .arg("EXAMPLE.COM")
        .status()
        .expect("run render_pages");

    assert!(status.success(), "expected render_pages to succeed for positional filter");
    assert!(temp.path().join("fetches/renders/example.com.png").is_file());
}

#[test]
fn render_pages_combines_multiple_pages_options() {
    let temp = TempDir::new().expect("tempdir");
    let html_dir = temp.path().join("fetches/html");
    fs::create_dir_all(&html_dir).expect("create html dir");

    fs::write(html_dir.join("foo.html"), "<!doctype html><title>Foo</title>").expect("write foo html");
    fs::write(html_dir.join("baz.html"), "<!doctype html><title>Baz</title>").expect("write baz html");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        .args(["--pages", "foo", "--pages", "BAZ"])
        .status()
        .expect("run render_pages");

    assert!(status.success(), "expected render_pages to succeed for combined filters");
    let renders = temp.path().join("fetches/renders");
    assert!(renders.join("foo.png").is_file());
    assert!(renders.join("baz.png").is_file());
}

#[test]
fn render_pages_combines_pages_options_and_positionals() {
    let temp = TempDir::new().expect("tempdir");
    let html_dir = temp.path().join("fetches/html");
    fs::create_dir_all(&html_dir).expect("create html dir");

    fs::write(html_dir.join("foo.html"), "<!doctype html><title>Foo</title>").expect("write foo html");
    fs::write(html_dir.join("bar.html"), "<!doctype html><title>Bar</title>").expect("write bar html");
    fs::write(html_dir.join("baz.html"), "<!doctype html><title>Baz</title>").expect("write baz html");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        .args(["--pages", "foo", "--pages", "BAR", "baz"])
        .status()
        .expect("run render_pages");

    assert!(status.success(), "expected render_pages to succeed for combined filters");
    let renders = temp.path().join("fetches/renders");
    assert!(renders.join("foo.png").is_file());
    assert!(renders.join("bar.png").is_file());
    assert!(renders.join("baz.png").is_file());
    assert!(renders.join("_summary.log").is_file(), "expected summary log to be written");
}

#[test]
fn render_pages_combines_pages_and_positional_with_full_urls() {
    let temp = TempDir::new().expect("tempdir");
    let html_dir = temp.path().join("fetches/html");
    fs::create_dir_all(&html_dir).expect("create html dir");

    fs::write(html_dir.join("example.com.html"), "<!doctype html><title>Example</title>").expect("write html");
    fs::write(html_dir.join("foo.com.html"), "<!doctype html><title>Foo</title>").expect("write foo html");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        .args(["--pages", "HTTPS://EXAMPLE.COM/", "foo.com"])
        .status()
        .expect("run render_pages");

    assert!(status.success(), "expected render_pages to succeed for mixed filters");
    let renders = temp.path().join("fetches/renders");
    assert!(renders.join("example.com.png").is_file());
    assert!(renders.join("foo.com.png").is_file());
}

#[test]
fn render_pages_ignores_non_matching_filters_if_some_match() {
    let temp = TempDir::new().expect("tempdir");
    let html_dir = temp.path().join("fetches/html");
    fs::create_dir_all(&html_dir).expect("create html dir");

    fs::write(html_dir.join("foo.html"), "<!doctype html><title>Foo</title>").expect("write foo html");

    let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
        .current_dir(temp.path())
        .args(["--pages", "foo,missing"])
        .status()
        .expect("run render_pages");

    assert!(status.success(), "expected render_pages to succeed with partial matches");
    let renders = temp.path().join("fetches/renders");
    assert!(renders.join("foo.png").is_file());
    // No PNG is expected for the missing filter entry.
    assert!(!renders.join("missing.png").exists());
}
