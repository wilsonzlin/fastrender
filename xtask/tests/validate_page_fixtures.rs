use std::fs;
use std::process::Command;

use tempfile::tempdir;

fn run_validate(fixtures_root: &std::path::Path) -> std::process::Output {
  Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args([
      "validate-page-fixtures",
      "--fixtures-root",
      fixtures_root.to_str().expect("fixtures root path"),
    ])
    .output()
    .expect("run cargo xtask validate-page-fixtures")
}

fn combined_output(output: &std::process::Output) -> String {
  let mut text = String::new();
  text.push_str(&String::from_utf8_lossy(&output.stdout));
  text.push_str(&String::from_utf8_lossy(&output.stderr));
  text
}

#[test]
fn validate_page_fixtures_fails_on_remote_img_in_index_html() {
  let dir = tempdir().expect("tempdir");
  let fixtures_root = dir.path();
  let fixture_dir = fixtures_root.join("example");
  fs::create_dir_all(&fixture_dir).expect("create fixture dir");

  fs::write(
    fixture_dir.join("index.html"),
    r#"<!doctype html><img src="https://example.com/x.png">"#,
  )
  .expect("write index.html");

  let output = run_validate(fixtures_root);
  assert!(
    !output.status.success(),
    "expected validator to fail, got: {:?}",
    output.status
  );
  let text = combined_output(&output);
  assert!(
    text.contains("example/index.html") && text.contains("https://example.com/x.png"),
    "expected output to mention failing file + URL; got:\n{text}"
  );
}

#[test]
fn validate_page_fixtures_fails_on_remote_img_in_embedded_html_asset() {
  let dir = tempdir().expect("tempdir");
  let fixtures_root = dir.path();
  let fixture_dir = fixtures_root.join("example");
  let assets_dir = fixture_dir.join("assets");
  fs::create_dir_all(&assets_dir).expect("create fixture dir");

  fs::write(
    fixture_dir.join("index.html"),
    r#"<!doctype html><iframe src="assets/frame.html"></iframe>"#,
  )
  .expect("write index.html");
  fs::write(
    assets_dir.join("frame.html"),
    r#"<!doctype html><img src="https://example.com/frame.png">"#,
  )
  .expect("write frame.html");

  let output = run_validate(fixtures_root);
  assert!(
    !output.status.success(),
    "expected validator to fail, got: {:?}",
    output.status
  );
  let text = combined_output(&output);
  assert!(
    text.contains("example/assets/frame.html") && text.contains("https://example.com/frame.png"),
    "expected output to mention embedded HTML asset + URL; got:\n{text}"
  );
}

#[test]
fn validate_page_fixtures_passes_for_clean_fixture() {
  let dir = tempdir().expect("tempdir");
  let fixtures_root = dir.path();
  let fixture_dir = fixtures_root.join("example");
  let assets_dir = fixture_dir.join("assets");
  fs::create_dir_all(&assets_dir).expect("create fixture dir");

  fs::write(
    fixture_dir.join("index.html"),
    r#"<!doctype html><img src="assets/local.png">"#,
  )
  .expect("write index.html");
  fs::write(assets_dir.join("local.png"), b"").expect("write local.png");

  let output = run_validate(fixtures_root);
  assert!(
    output.status.success(),
    "expected validator to succeed, got: {:?}\n{}",
    output.status,
    combined_output(&output)
  );
}

