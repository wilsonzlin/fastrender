use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn pageset_progress_killed_after_done_populates_timeout_stage() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("done.html"),
    "<!doctype html><title>Done</title><body>ok</body>",
  )
  .expect("write html");

  let progress_dir = temp.path().join("progress");
  let log_dir = temp.path().join("logs");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    .env("FASTR_TEST_SKIP_PROGRESS_SENTINEL", "1")
    // The worker sleeps after committing progress so the parent hits the hard timeout while the
    // page is already "done". Keep the sleep well above the hard timeout so the test isn't
    // sensitive to render speed variations in debug builds / CI.
    .env("FASTR_TEST_POST_DONE_SLEEP_MS", "5000")
    .args([
      "run",
      "--jobs",
      "1",
      "--timeout",
      // 1s is too tight in debug builds and can flake when the renderer hasn't reached paint yet.
      // We still want a short test, but need enough headroom to reliably hit the post-done sleep.
      "2",
      // Keep the render fast/deterministic so the timeout is driven by the post-done sleep, not by
      // heavy debug-mode painting.
      "--viewport",
      "64x64",
      "--diagnostics",
      "none",
      "--progress-dir",
      progress_dir.to_str().expect("progress dir"),
      "--log-dir",
      log_dir.to_str().expect("log dir"),
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress");

  assert!(status.success(), "pageset_progress exited with {status:?}");

  let progress_raw =
    fs::read_to_string(progress_dir.join("done.json")).expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  assert_eq!(progress["status"], "timeout");
  assert_ne!(progress["timeout_stage"], Value::Null);
  assert_ne!(progress["hotspot"], "unknown");
  assert_eq!(progress["timeout_stage"], "paint");
  assert_eq!(progress["hotspot"], "paint");
}
