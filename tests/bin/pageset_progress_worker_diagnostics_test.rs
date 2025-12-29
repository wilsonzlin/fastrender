use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn pageset_progress_worker_writes_diagnostics_stats() {
  let temp = TempDir::new().expect("tempdir");
  let cache_path = temp.path().join("example.html");
  fs::write(
    &cache_path,
    "<!doctype html><html><body><p>diagnostics</p></body></html>",
  )
  .expect("write html");

  let progress_path = temp.path().join("progress.json");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    .args([
      "worker",
      "--cache-path",
      cache_path.to_str().unwrap(),
      "--stem",
      "example",
      "--progress-path",
      progress_path.to_str().unwrap(),
      "--viewport",
      "800x600",
      "--dpr",
      "1.0",
      "--user-agent",
      "test-agent",
      "--accept-language",
      "en-US",
      "--diagnostics",
      "verbose",
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker");

  assert!(status.success(), "worker should succeed");

  let contents = fs::read_to_string(&progress_path).expect("read progress");
  let json: Value = serde_json::from_str(&contents).expect("parse progress JSON");
  assert_eq!(json["status"], "ok", "worker should record ok status");
  let stats = json
    .get("diagnostics")
    .and_then(|d| d.get("stats"))
    .expect("progress should include diagnostics.stats");
  assert!(
    stats.get("counts").is_some(),
    "stats should include counts summary"
  );
  assert!(
    stats["counts"]["dom_nodes"].as_u64().unwrap_or(0) > 0,
    "dom_nodes count should be recorded"
  );
}
