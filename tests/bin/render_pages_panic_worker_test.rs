use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn render_pages_recovers_from_worker_panic() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("panic.html"),
    "<!doctype html><title>Panic</title>",
  )
  .expect("write panic html");
  fs::write(html_dir.join("ok.html"), "<!doctype html><title>Ok</title>").expect("write ok html");

  let status = Command::new(env!("CARGO_BIN_EXE_render_pages"))
    .current_dir(temp.path())
    .args(["--jobs", "2", "--timeout", "2", "--diagnostics-json"])
    .env("FASTR_TEST_RENDER_PANIC_STEM", "panic")
    .status()
    .expect("run render_pages");

  assert_eq!(
    status.code(),
    Some(1),
    "expected render_pages to report the panic as a failure"
  );

  let render_dir = temp.path().join("fetches/renders");
  let panic_log = fs::read_to_string(render_dir.join("panic.log")).expect("read panic log");
  assert!(
    panic_log.contains("CRASH"),
    "panic log should record the crash: {panic_log}"
  );
  let diag_path = render_dir.join("panic.diagnostics.json");
  let diag: Value =
    serde_json::from_str(&fs::read_to_string(&diag_path).expect("read diag")).expect("parse diag");
  assert_eq!(diag["status"], "crash");

  assert!(
    render_dir.join("ok.png").is_file(),
    "other pages should still render"
  );
}
