use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn pageset_progress_worker_abort_overwrites_stale_progress() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("abort.html"),
    "<!doctype html><title>Abort</title><body>ok</body>",
  )
  .expect("write html");

  let progress_dir = temp.path().join("progress");
  let log_dir = temp.path().join("logs");
  fs::create_dir_all(&progress_dir).expect("create progress dir");

  // Simulate a stale progress file from a previous run (the worker will abort before writing
  // progress for this invocation).
  fs::write(
    progress_dir.join("abort.json"),
    r#"{
  "url": "https://example.com/",
  "status": "error",
  "total_ms": null,
  "stages_ms": { "fetch": 0.0, "css": 0.0, "cascade": 0.0, "layout": 0.0, "paint": 0.0 },
  "notes": "",
  "auto_notes": "stale error",
  "hotspot": "fetch",
  "failure_stage": null,
  "timeout_stage": null,
  "last_good_commit": "",
  "last_regression_commit": ""
}"#,
  )
  .expect("write stale progress");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    .env("FASTR_TEST_WORKER_ABORT_AFTER_MS", "0")
    .env("FASTR_TEST_WORKER_ABORT_STEM", "abort")
    .args([
      "run",
      "--jobs",
      "1",
      "--timeout",
      "2",
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
    fs::read_to_string(progress_dir.join("abort.json")).expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  assert_eq!(progress["status"], "panic");
  let notes = progress["auto_notes"].as_str().unwrap_or_default();
  assert!(
    notes.contains("without writing progress"),
    "expected synthesized note, got:\n{notes}"
  );
  assert!(
    !notes.contains("stale error"),
    "expected stale note to be overwritten, got:\n{notes}"
  );
}

