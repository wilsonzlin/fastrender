use serde_json::Value;
use std::fs;
use std::process::Command;
use std::time::{Duration, Instant};
use tempfile::tempdir;

const HANGING_HTML: &str = include_str!("../fixtures/pageset_progress/hanging_subresource.html");

#[test]
fn pageset_progress_worker_respects_fetch_timeout_budget() {
  let temp = tempdir().expect("create temp dir");
  let cache_path = temp.path().join("hanging.html");
  fs::write(&cache_path, HANGING_HTML).expect("write cache html");
  let progress_path = temp.path().join("progress.json");
  let log_path = temp.path().join("worker.log");

  let start = Instant::now();
  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("FASTR_USE_BUNDLED_FONTS", "0")
    .args([
      "worker",
      "--cache-path",
      cache_path.to_str().unwrap(),
      "--stem",
      "hanging",
      "--progress-path",
      progress_path.to_str().unwrap(),
      "--log-path",
      log_path.to_str().unwrap(),
      "--viewport",
      "800x600",
      "--dpr",
      "1.0",
      "--user-agent",
      "pageset-progress-fetch-timeout-test",
      "--accept-language",
      "en-US",
      "--diagnostics",
      "none",
      "--timeout",
      "1",
      "--soft-timeout-ms",
      "100",
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker with fetch timeout");
  let elapsed = start.elapsed();

  assert!(status.success(), "worker exited with {status:?}");
  assert!(
    elapsed < Duration::from_secs(3),
    "worker should respect fetch timeout, took {elapsed:?}"
  );

  let progress_raw = fs::read_to_string(&progress_path).expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  let status_str = progress["status"].as_str().unwrap_or_default();
  assert_ne!(
    status_str, "ok",
    "worker should record failure when subresource fetch times out"
  );
  let notes = progress["notes"]
    .as_str()
    .unwrap_or_default()
    .to_ascii_lowercase();
  assert!(
    notes.contains("timeout") || notes.contains("timed out"),
    "progress notes should mention timeout, got: {notes}"
  );
}
