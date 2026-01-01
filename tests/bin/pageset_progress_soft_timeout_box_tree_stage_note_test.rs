use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn pageset_progress_soft_timeout_in_box_tree_includes_stage_note() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");

  // Ensure the box-tree generator traverses at least 256 nodes so it performs a periodic deadline
  // check while StageHeartbeat is set to `box_tree`.
  let mut body = String::new();
  for _ in 0..300 {
    body.push_str("<div></div>");
  }
  fs::write(
    html_dir.join("box_tree_soft_timeout.html"),
    format!(
      "<!doctype html><title>Box tree timeout</title><body>{body}</body>"
    ),
  )
  .expect("write html");

  let progress_dir = temp.path().join("progress");
  let log_dir = temp.path().join("logs");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    // Delay once StageHeartbeat reaches `box_tree` so the next cooperative deadline check in box
    // generation trips the renderer timeout.
    .env("FASTR_TEST_BOX_TREE_DELAY_MS", "700")
    .args([
      "run",
      "--jobs",
      "1",
      "--timeout",
      "3",
      "--soft-timeout-ms",
      "500",
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

  let progress_raw = fs::read_to_string(progress_dir.join("box_tree_soft_timeout.json"))
    .expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  assert_eq!(progress["status"], "timeout");
  // `box_tree` timeouts still collapse into the `cascade` stage bucket.
  assert_eq!(progress["timeout_stage"], "cascade");
  assert_eq!(progress["hotspot"], "cascade");
  let auto_notes = progress["auto_notes"].as_str().unwrap_or_default();
  assert!(
    auto_notes.contains("stage: box_tree"),
    "expected stage note to mention box_tree, got: {auto_notes:?}"
  );
}
