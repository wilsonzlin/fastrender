use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn pageset_progress_crash_overwrites_stale_progress() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("stale.html"),
    "<!doctype html><title>Stale</title><body>ok</body>",
  )
  .expect("write html");

  let progress_dir = temp.path().join("progress");
  let log_dir = temp.path().join("logs");
  fs::create_dir_all(&progress_dir).expect("create progress dir");

  // Seed a progress file that should *not* be trusted if the worker dies before writing the
  // per-run sentinel.
  fs::write(
    progress_dir.join("stale.json"),
    r#"{
  "url": "https://example.com/",
  "status": "timeout",
  "total_ms": 123.0,
  "stages_ms": { "fetch": 0.0, "css": 0.0, "cascade": 0.0, "layout": 0.0, "paint": 0.0 },
  "notes": "",
  "hotspot": "layout",
  "failure_stage": null,
  "timeout_stage": "layout",
  "last_good_commit": "",
  "last_regression_commit": ""
}
"#,
  )
  .expect("write stale progress json");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    .env("FASTR_TEST_WORKER_ABORT_AFTER_MS", "0")
    .env("FASTR_TEST_WORKER_ABORT_STEM", "stale")
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
    fs::read_to_string(progress_dir.join("stale.json")).expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  assert_eq!(progress["status"], "panic");
  assert!(
    progress
      .get("auto_notes")
      .and_then(Value::as_str)
      .unwrap_or("")
      .contains("without writing progress"),
    "expected auto_notes to mention missing progress: {}",
    progress_raw
  );
}
