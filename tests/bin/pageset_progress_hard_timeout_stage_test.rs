use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn pageset_progress_hard_timeout_populates_timeout_stage_from_heartbeat() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("slow.html"),
    "<!doctype html><title>Slow</title>",
  )
  .expect("write slow html");

  let progress_dir = temp.path().join("progress");
  let log_dir = temp.path().join("logs");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    .env("FASTR_TEST_RENDER_DELAY_MS", "2000")
    .env("FASTR_TEST_RENDER_DELAY_STEM", "slow")
    .args([
      "run",
      "--jobs",
      "1",
      "--timeout",
      "1",
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
    fs::read_to_string(progress_dir.join("slow.json")).expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  assert_eq!(progress["status"], "timeout");
  assert_eq!(progress["timeout_stage"], "dom_parse");
  assert_eq!(progress["hotspot"], "fetch");
}
