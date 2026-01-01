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

#[test]
fn pageset_progress_migrate_recomputes_stage_buckets_from_stats() {
  let tmp = tempfile::tempdir().expect("temp dir");
  let progress_dir = tmp.path().join("progress");
  let html_dir = tmp.path().join("html");
  fs::create_dir_all(&progress_dir).expect("create progress dir");
  fs::create_dir_all(&html_dir).expect("create html dir");

  // Mark the cache as present so `migrate` doesn't apply the missing-cache placeholder logic.
  fs::write(html_dir.join("example.com.html"), "<!doctype html>").expect("write cache placeholder");

  let progress_path = progress_dir.join("example.com.json");
  let progress = serde_json::json!({
    "url": "https://example.com",
    "status": "ok",
    "total_ms": 123.0,
    // Deliberately store bogus buckets so migrate must recompute from stats.
    "stages_ms": { "fetch": 0.0, "css": 0.0, "cascade": 0.0, "layout": 0.0, "paint": 0.0 },
    "notes": "",
    "hotspot": "unknown",
    "failure_stage": null,
    "timeout_stage": null,
    "last_good_commit": "",
    "last_regression_commit": "",
    "diagnostics": {
      "stats": {
        "timings": {
          "html_decode_ms": 1.0,
          "dom_parse_ms": 2.0,
          "dom_meta_viewport_ms": 0.5,
          "dom_clone_ms": 0.25,
          "dom_top_layer_ms": 0.75,
          "css_inlining_ms": 3.0,
          "css_parse_ms": 4.0,
          "cascade_ms": 5.0,
          "box_tree_ms": 6.0,
          "layout_ms": 7.0,
          // Subsystem timings (not wall-clock stage time).
          "text_fallback_ms": 1000.0,
          "text_shape_ms": 8.0,
          "paint_build_ms": 9.0,
          "paint_optimize_ms": 10.0,
          "paint_rasterize_ms": 11.0,
          "text_rasterize_ms": 12.0,
          "encode_ms": 13.0
        }
      }
    }
  });
  fs::write(
    &progress_path,
    serde_json::to_string_pretty(&progress).expect("serialize progress json"),
  )
  .expect("write progress json");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args([
      "migrate",
      "--progress-dir",
      progress_dir.to_str().expect("progress dir utf-8"),
      "--html-dir",
      html_dir.to_str().expect("html dir utf-8"),
    ])
    .status()
    .expect("run pageset_progress migrate");
  assert!(status.success(), "migrate should succeed");

  let migrated_raw = fs::read_to_string(&progress_path).expect("read migrated json");
  let migrated: Value = serde_json::from_str(&migrated_raw).expect("parse migrated json value");
  let stages = migrated.get("stages_ms").expect("stages_ms object");
  assert_eq!(stages["fetch"].as_f64(), Some(4.5));
  assert_eq!(stages["css"].as_f64(), Some(7.0));
  assert_eq!(stages["cascade"].as_f64(), Some(11.0));
  // `text_*` subsystem timings should not affect wall-clock stage buckets.
  assert_eq!(stages["layout"].as_f64(), Some(7.0));
  assert_eq!(stages["paint"].as_f64(), Some(43.0));
  assert_eq!(
    migrated.get("hotspot"),
    Some(&Value::String("paint".into())),
    "hotspot should be recomputed from migrated stage buckets"
  );
}
