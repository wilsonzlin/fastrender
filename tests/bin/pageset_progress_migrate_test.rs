use serde_json::Value;
use std::fs;
use std::process::Command;

#[test]
fn pageset_progress_migrate_moves_legacy_auto_notes_out_of_notes() {
  let tmp = tempfile::tempdir().expect("temp dir");
  let progress_dir = tmp.path().join("progress");
  fs::create_dir_all(&progress_dir).expect("create progress dir");

  let progress_path = progress_dir.join("example.com.json");
  let legacy = serde_json::json!({
    "url": "https://example.com",
    "status": "timeout",
    "total_ms": null,
    "stages_ms": { "fetch": 0.0, "css": 0.0, "cascade": 0.0, "layout": 0.0, "paint": 0.0 },
    "notes": "manual blocker\nhard timeout after 5.00s\nstage: layout",
    "hotspot": "layout",
    "failure_stage": null,
    "timeout_stage": null,
    "last_good_commit": "",
    "last_regression_commit": ""
  });
  fs::write(
    &progress_path,
    serde_json::to_string_pretty(&legacy).expect("serialize legacy json"),
  )
  .expect("write legacy json");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args([
      "migrate",
      "--progress-dir",
      progress_dir.to_str().expect("progress dir utf-8"),
    ])
    .status()
    .expect("run pageset_progress migrate");
  assert!(status.success(), "migrate should succeed");

  let migrated_raw = fs::read_to_string(&progress_path).expect("read migrated json");
  let migrated: Value = serde_json::from_str(&migrated_raw).expect("parse migrated json value");
  assert_eq!(
    migrated.get("notes"),
    Some(&Value::String("manual blocker".into())),
    "manual notes should remain in `notes`"
  );
  assert_eq!(
    migrated.get("auto_notes"),
    Some(&Value::String(
      "hard timeout after 5.00s\nstage: layout".into()
    )),
    "machine diagnostics should be moved into `auto_notes`"
  );
}
