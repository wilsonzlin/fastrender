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
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
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
        },
        "counts": {},
        "cascade": {},
        "layout": {},
        "paint": {},
        "resources": {
          "fetch_counts": {}
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
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
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
  let fetch = stages["fetch"].as_f64().expect("fetch stage");
  let css = stages["css"].as_f64().expect("css stage");
  let cascade = stages["cascade"].as_f64().expect("cascade stage");
  let box_tree = stages["box_tree"].as_f64().expect("box_tree stage");
  let layout = stages["layout"].as_f64().expect("layout stage");
  let paint = stages["paint"].as_f64().expect("paint stage");
  let sum = fetch + css + cascade + box_tree + layout + paint;
  assert!(
    (sum - 123.0).abs() < 1.0,
    "expected migrated stages_ms sum ({sum}) to match total_ms (123.0)"
  );
  // `text_*` subsystem timings should not affect wall-clock stage buckets (only the stage wall
  // timers are used, then rescaled to total_ms for share calculations).
  let raw = [4.5, 7.0, 5.0, 6.0, 7.0, 43.0];
  let raw_sum: f64 = raw.iter().sum();
  let scale = 123.0 / raw_sum;
  assert!((fetch - raw[0] * scale).abs() < 1e-6);
  assert!((css - raw[1] * scale).abs() < 1e-6);
  assert!((cascade - raw[2] * scale).abs() < 1e-6);
  assert!((box_tree - raw[3] * scale).abs() < 1e-6);
  assert!((layout - raw[4] * scale).abs() < 1e-6);
  assert!((paint - raw[5] * scale).abs() < 1e-6);
  assert_eq!(
    migrated.get("hotspot"),
    Some(&Value::String("paint".into())),
    "hotspot should be recomputed from migrated stage buckets"
  );
}

#[test]
fn pageset_progress_migrate_preserves_existing_stage_buckets_when_present() {
  let tmp = tempfile::tempdir().expect("temp dir");
  let progress_dir = tmp.path().join("progress");
  let html_dir = tmp.path().join("html");
  fs::create_dir_all(&progress_dir).expect("create progress dir");
  fs::create_dir_all(&html_dir).expect("create html dir");

  fs::write(html_dir.join("example.com.html"), "<!doctype html>").expect("write cache placeholder");

  let progress_path = progress_dir.join("example.com.json");
  let progress = serde_json::json!({
    "url": "https://example.com",
    "status": "ok",
    "total_ms": 100.0,
    // Non-zero buckets should remain intact so a migrate run doesn't destroy stage-timeline
    // attribution (e.g. cache/redirect overhead) captured in committed progress artifacts.
    "stages_ms": { "fetch": 60.0, "css": 10.0, "cascade": 30.0, "layout": 0.0, "paint": 0.0 },
    "notes": "",
    "hotspot": "fetch",
    "failure_stage": null,
    "timeout_stage": null,
    "last_good_commit": "",
    "last_regression_commit": "",
    "diagnostics": {
      "stats": {
        "timings": {
          "dom_parse_ms": 1.0,
          "css_parse_ms": 1.0,
          "cascade_ms": 80.0
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
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
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
  assert!((stages["fetch"].as_f64().unwrap() - 60.0).abs() < 1e-6);
  assert!((stages["css"].as_f64().unwrap() - 10.0).abs() < 1e-6);
  assert!((stages["cascade"].as_f64().unwrap() - 30.0).abs() < 1e-6);
  assert!((stages["box_tree"].as_f64().unwrap() - 0.0).abs() < 1e-6);
  assert!((stages["layout"].as_f64().unwrap() - 0.0).abs() < 1e-6);
  assert!((stages["paint"].as_f64().unwrap() - 0.0).abs() < 1e-6);
  assert_eq!(
    migrated.get("hotspot"),
    Some(&Value::String("cascade".into())),
    "hotspot should be recomputed from stats even when stages_ms already exists",
  );
}

#[test]
fn pageset_progress_migrate_rewrites_legacy_cpu_sum_keys() {
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
          "css_parse_ms": 3.0,
          "cascade_ms": 4.0,
          "box_tree_ms": 5.0,
          "layout_ms": 6.0,
          "paint_build_ms": 7.0,
          "paint_rasterize_ms": 8.0,
          // Legacy CPU-sum timing keys should deserialize into `*_cpu_ms` and reserialize with the
          // new field names.
          "text_fallback_ms": 10.0,
          "text_shape_ms": 11.0,
          "text_rasterize_ms": 12.0
        },
        "counts": {},
        "cascade": {},
        "layout": {
          // Legacy Taffy perf counter keys should deserialize into `*_compute_cpu_ms` and reserialize
          // with the new field names.
          "taffy_flex_compute_ms": 123.0,
          "taffy_grid_compute_ms": 456.0
        },
        "paint": {},
        "resources": {
          "fetch_counts": {}
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
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
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
  let timings = &migrated["diagnostics"]["stats"]["timings"];
  assert_eq!(timings["text_fallback_cpu_ms"].as_f64(), Some(10.0));
  assert_eq!(timings["text_shape_cpu_ms"].as_f64(), Some(11.0));
  assert_eq!(timings["text_rasterize_cpu_ms"].as_f64(), Some(12.0));
  assert!(
    timings.get("text_fallback_ms").is_none()
      && timings.get("text_shape_ms").is_none()
      && timings.get("text_rasterize_ms").is_none(),
    "legacy text_* keys should not be emitted after migration"
  );

  let layout = &migrated["diagnostics"]["stats"]["layout"];
  assert_eq!(layout["taffy_flex_compute_cpu_ms"].as_f64(), Some(123.0));
  assert_eq!(layout["taffy_grid_compute_cpu_ms"].as_f64(), Some(456.0));
  assert!(
    layout.get("taffy_flex_compute_ms").is_none() && layout.get("taffy_grid_compute_ms").is_none(),
    "legacy taffy_*_compute_ms keys should not be emitted after migration"
  );
}
