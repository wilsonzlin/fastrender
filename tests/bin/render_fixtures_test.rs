use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn render_fixtures_renders_single_fixture_to_png() {
  let tmp = TempDir::new().expect("tempdir");
  let fixtures_root = tmp.path().join("fixtures");
  let fixture_dir = fixtures_root.join("simple");
  fs::create_dir_all(&fixture_dir).expect("create fixture dir");
  fs::write(
    fixture_dir.join("index.html"),
    r#"<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <style>
      body { margin: 0; background: white; }
      .box { width: 80px; height: 60px; background: rgb(255, 0, 0); }
    </style>
  </head>
  <body>
    <div class="box"></div>
  </body>
</html>"#,
  )
  .expect("write index.html");

  let out_dir = tmp.path().join("out");
  let output = Command::new(env!("CARGO_BIN_EXE_render_fixtures"))
    .arg("--fixtures-root")
    .arg(&fixtures_root)
    .arg("--out-dir")
    .arg(&out_dir)
    .args(["--only", "simple"])
    .args(["--jobs", "1"])
    .args(["--viewport", "200x150"])
    .output()
    .expect("run render_fixtures");

  assert!(
    output.status.success(),
    "render_fixtures should succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let png_path = out_dir.join("simple.png");
  assert!(png_path.is_file(), "expected output PNG to exist");
  let bytes = fs::read(&png_path).expect("read png");
  assert!(
    bytes.starts_with(b"\x89PNG"),
    "expected PNG header, got {} bytes",
    bytes.len()
  );
}

#[test]
fn render_fixtures_fails_on_http_subresource() {
  let tmp = TempDir::new().expect("tempdir");
  let fixtures_root = tmp.path().join("fixtures");
  let fixture_dir = fixtures_root.join("remote_img");
  fs::create_dir_all(&fixture_dir).expect("create fixture dir");
  fs::write(
    fixture_dir.join("index.html"),
    r#"<!doctype html>
<html>
  <body>
    <img src="https://example.com/x.png">
  </body>
</html>"#,
  )
  .expect("write index.html");

  let out_dir = tmp.path().join("out");
  let output = Command::new(env!("CARGO_BIN_EXE_render_fixtures"))
    .arg("--fixtures-root")
    .arg(&fixtures_root)
    .arg("--out-dir")
    .arg(&out_dir)
    .args(["--only", "remote_img"])
    .args(["--jobs", "1"])
    .args(["--viewport", "200x150"])
    .output()
    .expect("run render_fixtures");

  assert!(
    !output.status.success(),
    "expected render_fixtures to fail when http(s) is referenced"
  );

  assert!(
    !out_dir.join("remote_img.png").exists(),
    "should not write PNG for failed fixture"
  );

  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    stderr.contains("blocked http/https"),
    "expected stderr to mention blocked http/https\nstderr:\n{stderr}"
  );
  assert!(
    stderr.contains("https://example.com/x.png"),
    "expected stderr to include blocked URL\nstderr:\n{stderr}"
  );
}
