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

  let stages = &progress["stages_ms"];
  let fetch = stages["fetch"].as_f64().expect("fetch stage ms");
  assert!(
    fetch > 0.0,
    "expected fetch stage to be > 0ms, got {fetch} (stages_ms={stages:?})"
  );
  assert_eq!(stages["css"].as_f64().expect("css stage ms"), 0.0);
  assert_eq!(stages["cascade"].as_f64().expect("cascade stage ms"), 0.0);
  assert_eq!(stages["layout"].as_f64().expect("layout stage ms"), 0.0);
  assert_eq!(stages["paint"].as_f64().expect("paint stage ms"), 0.0);

  let total_ms = progress["total_ms"].as_f64().expect("total_ms");
  let sum = fetch
    + stages["css"].as_f64().unwrap()
    + stages["cascade"].as_f64().unwrap()
    + stages["layout"].as_f64().unwrap()
    + stages["paint"].as_f64().unwrap();
  assert!(
    (sum - total_ms).abs() < 1.0,
    "expected stages_ms to sum to total_ms (~{total_ms}), got {sum} (stages_ms={stages:?})"
  );
}
