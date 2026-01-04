use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn render_pages_respects_out_dir_flag() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");

  fs::write(
    html_dir.join("example.com.html"),
    "<!doctype html><html><head><title>Example</title></head><body>ok</body></html>",
  )
  .expect("write html");

  let out_dir = temp.path().join("custom/renders");

  let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
    .current_dir(temp.path())
    .args([
      "--pages",
      "example.com",
      "--jobs",
      "1",
      "--diagnostics-json",
      "--dump-intermediate",
      "full",
    ])
    .arg("--out-dir")
    .arg(&out_dir)
    .status()
    .expect("run render_pages");

  assert!(status.success(), "expected render_pages to succeed");
  assert!(out_dir.join("example.com.png").is_file());
  assert!(out_dir.join("example.com.log").is_file());
  assert!(out_dir.join("example.com.stderr.log").is_file());
  assert!(out_dir.join("example.com.result.json").is_file());
  assert!(out_dir.join("example.com.diagnostics.json").is_file());
  assert!(out_dir.join("example.com.intermediate.json").is_file());
  assert!(out_dir.join("example.com.snapshot.json").is_file());
  assert!(out_dir.join("_summary.log").is_file());

  assert!(
    !temp.path().join("fetches/renders").exists(),
    "default output directory should not be created when --out-dir is set"
  );
}
