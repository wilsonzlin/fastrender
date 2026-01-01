use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn pageset_progress_worker_fast_exits_after_writing_progress() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("fast_exit.html"),
    "<!doctype html><title>Fast</title><body>ok</body>",
  )
  .expect("write html");

  let progress_dir = temp.path().join("progress");
  let log_dir = temp.path().join("logs");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    // The worker uses `std::process::exit` after writing progress artifacts to
    // bypass Drop-based cleanup. Keep the hard timeout below the injected drop
    // delay so this test fails if destructors run.
    .env("FASTR_TEST_WORKER_DROP_DELAY_MS", "5000")
    .args([
      "run",
      "--jobs",
      "1",
      "--timeout",
      "2",
      // Reduce paint workload so debug builds can complete before the timeout (the test is about
      // worker exit behavior, not rendering a large viewport).
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
    fs::read_to_string(progress_dir.join("fast_exit.json")).expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  assert_eq!(progress["status"], "ok");

  let stderr_contents =
    fs::read_to_string(log_dir.join("fast_exit.stderr.log")).unwrap_or_default();
  assert!(
    !stderr_contents.contains("parent killed worker"),
    "worker should exit before parent kill; stderr log contained:\n{stderr_contents}"
  );
}
