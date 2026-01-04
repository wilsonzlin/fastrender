use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn render_pages_respects_out_dir_for_outputs_and_logs() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("example.com.html"),
    "<!doctype html><title>Example</title>",
  )
  .expect("write html");

  let out_dir = temp.path().join("custom_renders");

  let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
    .current_dir(temp.path())
    .args([
      "--pages",
      "example.com",
      "--diagnostics-json",
      "--dump-intermediate",
      "full",
    ])
    .arg("--out-dir")
    .arg(&out_dir)
    .status()
    .expect("run render_pages");

  assert!(status.success(), "render_pages should succeed");
  assert!(
    out_dir.join("example.com.png").is_file(),
    "expected PNG output under --out-dir"
  );
  assert!(
    out_dir.join("example.com.log").is_file(),
    "expected per-page log under --out-dir"
  );
  assert!(
    out_dir.join("example.com.stderr.log").is_file(),
    "expected worker stderr log under --out-dir"
  );
  assert!(
    out_dir.join("example.com.diagnostics.json").is_file(),
    "expected diagnostics JSON under --out-dir"
  );
  assert!(
    out_dir.join("example.com.intermediate.json").is_file(),
    "expected intermediate summary under --out-dir"
  );
  assert!(
    out_dir.join("example.com.snapshot.json").is_file(),
    "expected pipeline snapshot under --out-dir"
  );
  assert!(
    out_dir.join("_summary.log").is_file(),
    "expected summary log under --out-dir"
  );
  assert!(
    !temp.path().join("fetches/renders").exists(),
    "default fetches/renders should not be created when --out-dir is set"
  );
}
