use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn pageset_progress_soft_timeout_populates_stage_buckets() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  // Make DOM parsing reliably slower than the configured soft timeout so we deterministically
  // time out in the `dom_parse` stage without relying on the unit-test-only
  // `FASTR_TEST_RENDER_DELAY_MS` hook (which does not apply to CLI binaries).
  let mut body = String::new();
  for _ in 0..200_000 {
    body.push_str("<div>slow</div>");
  }
  fs::write(
    html_dir.join("soft_timeout.html"),
    format!("<!doctype html><title>Soft timeout</title><body>{body}</body>"),
  )
  .expect("write html");

  let progress_dir = temp.path().join("progress");
  let log_dir = temp.path().join("logs");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .current_dir(temp.path())
    .args([
      "run",
      "--jobs",
      "1",
      "--timeout",
      "2",
      "--soft-timeout-ms",
      "10",
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

  let progress_raw = fs::read_to_string(progress_dir.join("soft_timeout.json"))
    .expect("read soft timeout progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  assert_eq!(progress["status"], "timeout");
  let auto_notes = progress["auto_notes"].as_str().unwrap_or_default();
  assert!(
    auto_notes.contains("timeout at"),
    "expected cooperative timeout auto_notes, got: {auto_notes:?}"
  );

  assert_eq!(progress["timeout_stage"], "dom_parse");
  assert_eq!(progress["hotspot"], "fetch");

  let stages = &progress["stages_ms"];
  let fetch = stages["fetch"].as_f64().unwrap_or(0.0);
  let css = stages["css"].as_f64().unwrap_or(0.0);
  let cascade = stages["cascade"].as_f64().unwrap_or(0.0);
  let layout = stages["layout"].as_f64().unwrap_or(0.0);
  let paint = stages["paint"].as_f64().unwrap_or(0.0);
  let sum = fetch + css + cascade + layout + paint;

  assert!(sum > 0.0, "expected non-zero stages_ms, got: {stages:?}");
  assert!(
    (sum - progress["total_ms"].as_f64().unwrap_or(0.0)).abs() < 1.0,
    "expected stages_ms sum ({sum}) to match total_ms ({:?})",
    progress["total_ms"]
  );
  assert!(
    fetch >= css && fetch >= cascade && fetch >= layout && fetch >= paint,
    "expected fetch bucket to dominate, got: {stages:?}"
  );
}
