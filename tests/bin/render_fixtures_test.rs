use image::GenericImageView;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

fn write_fixture(root: &std::path::Path, stem: &str, index_html: &str) -> std::path::PathBuf {
  let dir = root.join(stem);
  fs::create_dir_all(&dir).expect("create fixture dir");
  fs::write(dir.join("index.html"), index_html).expect("write index.html");
  dir
}

#[test]
fn render_fixtures_writes_png_output() {
  let temp = TempDir::new().expect("tempdir");
  let fixtures_dir = temp.path().join("fixtures");
  let out_dir = temp.path().join("out");
  fs::create_dir_all(&fixtures_dir).expect("create fixtures dir");

  write_fixture(
    &fixtures_dir,
    "basic",
    "<!doctype html><html><body>ok</body></html>",
  );

  let status = Command::new(env!("CARGO_BIN_EXE_render_fixtures"))
    .current_dir(temp.path())
    .args([
      "--fixtures-dir",
      fixtures_dir.to_str().unwrap(),
      "--out-dir",
      out_dir.to_str().unwrap(),
      "--fixtures",
      "basic",
      "--viewport",
      "64x64",
      "--jobs",
      "1",
      "--timeout",
      "2",
    ])
    .status()
    .expect("run render_fixtures");

  assert!(status.success(), "expected render_fixtures to succeed");
  assert!(out_dir.join("basic.png").is_file(), "expected PNG output");
  assert!(out_dir.join("basic.log").is_file(), "expected per-fixture log");
}

#[test]
fn render_fixtures_blocks_http_subresources() {
  let temp = TempDir::new().expect("tempdir");
  let fixtures_dir = temp.path().join("fixtures");
  let out_dir = temp.path().join("out");
  fs::create_dir_all(&fixtures_dir).expect("create fixtures dir");

  write_fixture(
    &fixtures_dir,
    "blocked",
    "<!doctype html><html><body><img src=\"http://example.com/a.png\"></body></html>",
  );

  let status = Command::new(env!("CARGO_BIN_EXE_render_fixtures"))
    .current_dir(temp.path())
    .args([
      "--fixtures-dir",
      fixtures_dir.to_str().unwrap(),
      "--out-dir",
      out_dir.to_str().unwrap(),
      "--fixtures",
      "blocked",
      "--viewport",
      "64x64",
      "--jobs",
      "1",
      "--timeout",
      "2",
    ])
    .status()
    .expect("run render_fixtures");

  assert!(
    !status.success(),
    "expected render_fixtures to fail when http subresources are referenced"
  );

  let log = fs::read_to_string(out_dir.join("blocked.log")).expect("read log");
  assert!(
    log.contains("http://example.com/a.png"),
    "log should mention blocked URL"
  );
}

#[test]
fn render_fixtures_resolves_relative_stylesheets_from_base_url() {
  let temp = TempDir::new().expect("tempdir");
  let fixtures_dir = temp.path().join("fixtures");
  let out_dir = temp.path().join("out");
  fs::create_dir_all(&fixtures_dir).expect("create fixtures dir");

  let fixture_dir = write_fixture(
    &fixtures_dir,
    "relative_css",
    r#"<!doctype html>
<html>
  <head>
    <link rel="stylesheet" href="support/style.css">
  </head>
  <body></body>
</html>"#,
  );
  fs::create_dir_all(fixture_dir.join("support")).expect("create support dir");
  fs::write(
    fixture_dir.join("support/style.css"),
    "html, body { margin: 0; width: 100%; height: 100%; background: rgb(255, 0, 0); }",
  )
  .expect("write style.css");

  let status = Command::new(env!("CARGO_BIN_EXE_render_fixtures"))
    .current_dir(temp.path())
    .args([
      "--fixtures-dir",
      fixtures_dir.to_str().unwrap(),
      "--out-dir",
      out_dir.to_str().unwrap(),
      "--fixtures",
      "relative_css",
      "--viewport",
      "32x32",
      "--jobs",
      "1",
      "--timeout",
      "2",
    ])
    .status()
    .expect("run render_fixtures");

  assert!(
    status.success(),
    "expected render_fixtures to succeed with relative stylesheet"
  );

  let png_bytes = fs::read(out_dir.join("relative_css.png")).expect("read png");
  let image = image::load_from_memory(&png_bytes)
    .expect("decode png")
    .to_rgba8();
  let pixel = image.get_pixel(0, 0).0;
  assert!(
    pixel[0] > 200 && pixel[1] < 50 && pixel[2] < 50,
    "expected red-ish background pixel from stylesheet (got {:?})",
    pixel
  );
}

#[test]
fn render_fixtures_writes_snapshot_outputs() {
  let temp = TempDir::new().expect("tempdir");
  let fixtures_dir = temp.path().join("fixtures");
  let out_dir = temp.path().join("out");
  fs::create_dir_all(&fixtures_dir).expect("create fixtures dir");

  write_fixture(
    &fixtures_dir,
    "snapshot",
    "<!doctype html><html><body style=\"margin:0;background:rgb(0,255,0)\"></body></html>",
  );

  let status = Command::new(env!("CARGO_BIN_EXE_render_fixtures"))
    .current_dir(temp.path())
    .args([
      "--fixtures-dir",
      fixtures_dir.to_str().unwrap(),
      "--out-dir",
      out_dir.to_str().unwrap(),
      "--fixtures",
      "snapshot",
      "--viewport",
      "32x32",
      "--jobs",
      "1",
      "--timeout",
      "2",
      "--write-snapshot",
    ])
    .status()
    .expect("run render_fixtures");

  assert!(
    status.success(),
    "expected render_fixtures to succeed with --write-snapshot"
  );

  assert!(
    out_dir.join("snapshot/snapshot.json").is_file(),
    "expected snapshot.json output"
  );
  assert!(
    out_dir.join("snapshot/diagnostics.json").is_file(),
    "expected diagnostics.json output"
  );
}

