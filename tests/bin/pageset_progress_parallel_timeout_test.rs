use serde_json::Value;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::thread;
use std::time::{Duration, Instant};
use tempfile::tempdir;

#[test]
fn pageset_progress_worker_times_out_cooperatively_under_parallel_layout_and_paint() {
  let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests/pages/fixtures/timeout_parallel_spread/index.html");
  assert!(fixture.is_file(), "missing fixture {}", fixture.display());

  let temp = tempdir().expect("temp dir");
  let progress_path = temp.path().join("progress.json");
  let stage_path = temp.path().join("stage.txt");
  let log_path = temp.path().join("worker.log");

  let mut cmd = Command::new(env!("CARGO_BIN_EXE_pageset_progress"));
  cmd
    // Force Rayon fan-out so deadline propagation is exercised on worker threads.
    .env("RAYON_NUM_THREADS", "2")
    .env("FASTR_PAINT_THREADS", "2")
    .env("FASTR_PAINT_PARALLEL", "on")
    .args([
      "worker",
      "--cache-path",
      fixture.to_str().expect("fixture path"),
      "--stem",
      "timeout_parallel_spread",
      "--progress-path",
      progress_path.to_str().expect("progress path"),
      "--log-path",
      log_path.to_str().expect("log path"),
      "--stage-path",
      stage_path.to_str().expect("stage path"),
      "--viewport",
      "1024x1024",
      "--dpr",
      "1.0",
      "--user-agent",
      "pageset-progress-parallel-timeout-test",
      "--accept-language",
      "en-US",
      "--diagnostics",
      "none",
      "--timeout",
      "2",
      "--soft-timeout-ms",
      "500",
      "--layout-parallel",
      "on",
      "--layout-parallel-min-fanout",
      "1",
      "--layout-parallel-max-threads",
      "2",
      "--bundled-fonts",
    ]);

  let mut child = cmd.spawn().expect("spawn pageset_progress worker");
  let start = Instant::now();
  let hard_limit = Duration::from_secs(5);
  let status = loop {
    if let Some(status) = child.try_wait().expect("try_wait worker") {
      break status;
    }
    if start.elapsed() > hard_limit {
      let _ = child.kill();
      let _ = child.wait();
      panic!("worker exceeded hard limit of {hard_limit:?}");
    }
    thread::sleep(Duration::from_millis(20));
  };

  assert!(status.success(), "worker exited with {status:?}");

  let progress_raw = fs::read_to_string(&progress_path).expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");

  assert_eq!(progress["status"], "timeout");
  assert_eq!(progress["timeout_stage"], "paint");
  let auto_notes = progress["auto_notes"].as_str().unwrap_or_default();
  assert!(
    auto_notes.contains("timeout at paint"),
    "expected structured timeout notes, got: {auto_notes}"
  );
  assert!(
    !auto_notes.contains("hard timeout"),
    "expected cooperative timeout, got: {auto_notes}"
  );

  let stage = fs::read_to_string(&stage_path)
    .expect("read stage heartbeat")
    .trim()
    .to_string();
  assert!(
    stage.starts_with("paint_"),
    "expected paint heartbeat at timeout, got: {stage}"
  );

  let buckets = progress["stages_ms"]
    .as_object()
    .expect("stages_ms object");
  let paint_ms = buckets
    .get("paint")
    .and_then(|v| v.as_f64())
    .unwrap_or(0.0);
  assert!(
    paint_ms > 0.0,
    "expected non-zero paint bucket on timeout, got: {paint_ms} (raw {progress_raw})"
  );
}
