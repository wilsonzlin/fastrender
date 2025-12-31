use std::fs;
use std::process::Command;

use serde_json::Value;
use tempfile::tempdir;

#[test]
fn worker_writes_dumps_for_failures() {
  let temp = tempdir().expect("create temp dir");
  let cache_path = temp.path().join("page.html");
  fs::write(
    &cache_path,
    "<!doctype html><html><body><div>slow</div></body></html>",
  )
  .expect("write cache html");
  let progress_path = temp.path().join("progress.json");
  let log_path = temp.path().join("worker.log");
  let dump_dir = temp.path().join("dumps");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("FASTR_TEST_RENDER_DELAY_MS", "20")
    .env("FASTR_TEST_RENDER_DELAY_STEM", "dump")
    .args([
      "worker",
      "--cache-path",
      cache_path.to_str().unwrap(),
      "--stem",
      "dump",
      "--progress-path",
      progress_path.to_str().unwrap(),
      "--log-path",
      log_path.to_str().unwrap(),
      "--viewport",
      "800x600",
      "--dpr",
      "1.0",
      "--user-agent",
      "pageset-progress-test",
      "--accept-language",
      "en-US",
      "--diagnostics",
      "none",
      "--soft-timeout-ms",
      "1",
      "--dump-failures",
      "summary",
      "--dump-dir",
      dump_dir.to_str().unwrap(),
      "--dump-timeout",
      "5",
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker with dumps");

  assert!(status.success(), "worker should exit successfully");

  let progress_raw = fs::read_to_string(&progress_path).expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  assert_ne!(
    progress["status"], "ok",
    "first render should report a failure to trigger dumps"
  );

  let snapshot_path = dump_dir.join("dump").join("snapshot.json");
  assert!(
    snapshot_path.is_file(),
    "expected snapshot.json to be written for failing page"
  );
  let snapshot_raw = fs::read_to_string(&snapshot_path).expect("read snapshot json");
  let snapshot: Value = serde_json::from_str(&snapshot_raw).expect("parse snapshot json");
  assert!(
    snapshot["schema_version"].is_string(),
    "snapshot should include a schema version"
  );
  assert!(
    !snapshot_raw.contains(temp.path().to_str().unwrap()),
    "snapshot should not leak absolute paths"
  );
}
