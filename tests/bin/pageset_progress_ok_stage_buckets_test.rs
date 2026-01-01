use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn pageset_progress_ok_populates_stage_buckets_from_timeline() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("ok.html"),
    "<!doctype html><title>OK</title><body>ok</body>",
  )
  .expect("write ok html");

  let progress_dir = temp.path().join("progress");
  let log_dir = temp.path().join("logs");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    .args([
      "run",
      "--jobs",
      "1",
      "--timeout",
      "2",
      // Ensure ok-page stage buckets are populated from the heartbeat timeline, not structured
      // diagnostics stats.
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
    fs::read_to_string(progress_dir.join("ok.json")).expect("read ok progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");

  assert_eq!(progress["status"], "ok");
  let total_ms = progress["total_ms"].as_f64().expect("total_ms");

  let stages = &progress["stages_ms"];
  let fetch = stages["fetch"].as_f64().unwrap_or(0.0);
  let css = stages["css"].as_f64().unwrap_or(0.0);
  let cascade = stages["cascade"].as_f64().unwrap_or(0.0);
  let layout = stages["layout"].as_f64().unwrap_or(0.0);
  let paint = stages["paint"].as_f64().unwrap_or(0.0);
  let sum = fetch + css + cascade + layout + paint;

  assert!(sum > 0.0, "expected non-zero stages_ms, got: {stages:?}");
  assert!(
    (sum - total_ms).abs() < 1.0,
    "expected stages_ms sum ({sum}) to match total_ms ({total_ms})",
  );
  for (name, value) in [
    ("fetch", fetch),
    ("css", css),
    ("cascade", cascade),
    ("layout", layout),
    ("paint", paint),
  ] {
    assert!(
      value <= total_ms + 1e-6,
      "expected stage bucket {name} ({value}) to not exceed total_ms ({total_ms})",
    );
  }
}

