use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn render_pages_worker_times_out_without_hanging() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("slow.html"),
    "<!doctype html><title>Slow</title>",
  )
  .expect("write slow html");
  fs::write(html_dir.join("ok.html"), "<!doctype html><title>Ok</title>").expect("write ok html");

  let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
    .current_dir(temp.path())
    .args(["--jobs", "1", "--timeout", "1", "--diagnostics-json"])
    .env("FASTR_TEST_RENDER_DELAY_MS", "1500")
    .env("FASTR_TEST_RENDER_DELAY_STEM", "slow")
    .status()
    .expect("run render_pages");

  assert_eq!(
    status.code(),
    Some(1),
    "expected render_pages to report the timeout as a failure"
  );

  let render_dir = temp.path().join("fetches/renders");
  let slow_log = fs::read_to_string(render_dir.join("slow.log")).expect("read slow log");
  assert!(
    slow_log.contains("TIMEOUT"),
    "slow log should record the timeout: {slow_log}"
  );

  let diag_path = render_dir.join("slow.diagnostics.json");
  let diag: Value =
    serde_json::from_str(&fs::read_to_string(&diag_path).expect("read diag")).expect("parse diag");
  assert_eq!(diag["status"], "timeout");

  assert!(
    render_dir.join("ok.png").is_file(),
    "other pages should still render"
  );

  let stderr_log = fs::read_to_string(render_dir.join("slow.stderr.log")).unwrap_or_default();
  assert!(
    stderr_log.contains("hard timeout"),
    "stderr log should include the hard timeout note"
  );
}
