use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn render_pages_writes_diagnostics_json() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("example.com.html"),
    "<!doctype html><title>Example</title>",
  )
  .expect("write html");

  let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
    .current_dir(temp.path())
    .arg("--diagnostics-json")
    .status()
    .expect("run render_pages");

  assert!(status.success(), "render_pages should succeed");
  let diag_path = temp
    .path()
    .join("fetches/renders/example.com.diagnostics.json");
  assert!(diag_path.is_file(), "diagnostics JSON should be written");

  let contents = fs::read_to_string(diag_path).expect("read diagnostics");
  let json: Value = serde_json::from_str(&contents).expect("parse diagnostics JSON");
  assert_eq!(json["status"], "ok");
}

#[test]
fn render_pages_writes_summary_dump() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("example.com.html"),
    "<!doctype html><title>Example</title>",
  )
  .expect("write html");

  let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
    .current_dir(temp.path())
    .args(["--dump-intermediate", "summary"])
    .status()
    .expect("run render_pages");

  assert!(status.success(), "render_pages should succeed");
  let summary_path = temp
    .path()
    .join("fetches/renders/example.com.intermediate.json");
  assert!(summary_path.is_file(), "summary JSON should be written");
  let contents = fs::read_to_string(summary_path).expect("read summary");
  let json: Value = serde_json::from_str(&contents).expect("parse summary");
  assert!(
    json.get("dom_nodes").is_some(),
    "summary should include dom_nodes count"
  );
}

#[test]
fn render_pages_skips_full_dump_on_success_when_only_failures() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("example.com.html"),
    "<!doctype html><title>Example</title>",
  )
  .expect("write html");

  let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
    .current_dir(temp.path())
    .args(["--dump-intermediate", "full", "--only-failures"])
    .status()
    .expect("run render_pages");

  assert!(status.success(), "render_pages should succeed");
  let dom_path = temp.path().join("fetches/renders/example.com.dom.json");
  assert!(
    !dom_path.exists(),
    "full dump should be skipped for successful renders when only_failures is set"
  );
}
