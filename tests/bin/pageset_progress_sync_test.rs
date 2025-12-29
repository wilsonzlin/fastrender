use fastrender::pageset;
use serde_json::Value;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

fn run_sync(progress_dir: &PathBuf, html_dir: &PathBuf, prune: bool) -> std::process::ExitStatus {
  let mut cmd = Command::new(env!("CARGO_BIN_EXE_pageset_progress"));
  cmd.args([
    "sync",
    "--progress-dir",
    progress_dir.to_str().unwrap(),
    "--html-dir",
    html_dir.to_str().unwrap(),
  ]);
  if prune {
    cmd.arg("--prune");
  }
  cmd.status().expect("run pageset_progress sync")
}

#[test]
fn pageset_progress_sync_creates_placeholder_for_each_page() {
  let tmp = tempfile::tempdir().expect("temp dir");
  let progress_dir = tmp.path().join("progress");
  let html_dir = tmp.path().join("html");
  fs::create_dir_all(&html_dir).unwrap();

  let status = run_sync(&progress_dir, &html_dir, false);
  assert!(status.success(), "sync should succeed");

  let mut stems: Vec<String> = fs::read_dir(&progress_dir)
    .expect("read progress dir")
    .filter_map(|entry| {
      let entry = entry.ok()?;
      if entry
        .path()
        .extension()
        .and_then(|e| e.to_str())
        .map(|ext| ext != "json")
        .unwrap_or(true)
      {
        return None;
      }
      entry
        .path()
        .file_stem()
        .and_then(|s| s.to_str())
        .map(|s| s.to_string())
    })
    .collect();
  stems.sort();

  let mut expected: Vec<String> = pageset::pageset_entries()
    .into_iter()
    .map(|entry| entry.stem)
    .collect();
  expected.sort();

  assert_eq!(
    stems, expected,
    "sync should emit one JSON per pageset stem"
  );
}

#[test]
fn pageset_progress_sync_marks_missing_cache_as_error() {
  let tmp = tempfile::tempdir().expect("temp dir");
  let progress_dir = tmp.path().join("progress");
  let html_dir = tmp.path().join("html");
  fs::create_dir_all(&html_dir).unwrap();

  let status = run_sync(&progress_dir, &html_dir, false);
  assert!(status.success(), "sync should succeed");

  let target = pageset::pageset_entries()
    .into_iter()
    .next()
    .expect("pageset entries exist");
  let progress_path = progress_dir.join(format!("{}.json", target.stem));
  let updated: Value =
    serde_json::from_str(&fs::read_to_string(&progress_path).unwrap()).expect("json value");
  assert_eq!(
    updated.get("status"),
    Some(&Value::String("error".into())),
    "missing caches should be marked as errors"
  );
  assert_eq!(
    updated.get("notes"),
    Some(&Value::String("missing cache".into())),
    "missing caches should be called out in notes"
  );
  assert_eq!(
    updated.get("hotspot"),
    Some(&Value::String("fetch".into())),
    "missing caches should be treated as fetch issues"
  );
  assert_eq!(
    updated.get("url"),
    Some(&Value::String(target.url.to_string())),
    "sync should use the canonical pageset URL"
  );
  assert!(
    updated.get("total_ms").is_some_and(Value::is_null),
    "placeholder entries should not claim timings"
  );
  assert_eq!(
    updated.get("stages_ms"),
    Some(&serde_json::json!({
      "fetch": 0.0,
      "css": 0.0,
      "cascade": 0.0,
      "layout": 0.0,
      "paint": 0.0
    })),
    "placeholder entries should reset stage timings"
  );
}

#[test]
fn pageset_progress_sync_prunes_stale_files() {
  let tmp = tempfile::tempdir().expect("temp dir");
  let progress_dir = tmp.path().join("progress");
  let html_dir = tmp.path().join("html");
  fs::create_dir_all(&progress_dir).unwrap();
  fs::create_dir_all(&html_dir).unwrap();

  let stale_path = progress_dir.join("stale.json");
  fs::write(
    &stale_path,
    r#"{"url":"https://stale.test","status":"error"}"#,
  )
  .unwrap();

  let status = run_sync(&progress_dir, &html_dir, true);
  assert!(status.success(), "sync --prune should succeed");

  assert!(
    !stale_path.exists(),
    "stale progress files should be removed when --prune is set"
  );

  let json_files = fs::read_dir(&progress_dir)
    .unwrap()
    .filter(|entry| {
      if let Ok(entry) = entry {
        let path = entry.path();
        return path.extension().and_then(|ext| ext.to_str()) == Some("json");
      }
      false
    })
    .count();
  assert_eq!(
    json_files,
    pageset::pageset_entries().len(),
    "prune should leave exactly one file per pageset entry"
  );
}

#[test]
fn pageset_progress_sync_preserves_manual_fields() {
  let tmp = tempfile::tempdir().expect("temp dir");
  let progress_dir = tmp.path().join("progress");
  let html_dir = tmp.path().join("html");
  fs::create_dir_all(&progress_dir).unwrap();
  fs::create_dir_all(&html_dir).unwrap();

  let target = pageset::pageset_entries()
    .into_iter()
    .find(|entry| entry.url.contains("example.com"))
    .expect("example.com in pageset");

  let progress_path = progress_dir.join(format!("{}.json", target.stem));
  let original = serde_json::json!({
    "url": "https://old.example.com",
    "status": "ok",
    "total_ms": 123.0,
    "stages_ms": { "fetch": 1.0, "css": 2.0, "cascade": 3.0, "layout": 4.0, "paint": 5.0 },
    "notes": "manual note",
    "hotspot": "layout",
    "last_good_commit": "deadbee",
    "last_regression_commit": "badc0de"
  });
  fs::write(
    &progress_path,
    serde_json::to_string_pretty(&original).unwrap(),
  )
  .unwrap();

  let status = run_sync(&progress_dir, &html_dir, false);
  assert!(status.success(), "sync should succeed");

  let updated: Value =
    serde_json::from_str(&fs::read_to_string(&progress_path).unwrap()).expect("json value");
  assert_eq!(
    updated.get("notes"),
    Some(&Value::String("manual note".into()))
  );
  assert_eq!(
    updated.get("hotspot"),
    Some(&Value::String("layout".into()))
  );
  assert_eq!(
    updated.get("last_good_commit"),
    Some(&Value::String("deadbee".into()))
  );
  assert_eq!(
    updated.get("last_regression_commit"),
    Some(&Value::String("badc0de".into()))
  );
  assert_eq!(
    updated.get("url"),
    Some(&Value::String(target.url.to_string()))
  );
  assert_eq!(
    updated.get("status"),
    Some(&Value::String("error".into())),
    "missing caches should reset status to error even when manual fields are present"
  );
  assert!(
    updated.get("total_ms").is_some_and(Value::is_null),
    "missing caches should clear stale timings"
  );
  assert_eq!(
    updated.get("stages_ms"),
    Some(&serde_json::json!({
      "fetch": 0.0,
      "css": 0.0,
      "cascade": 0.0,
      "layout": 0.0,
      "paint": 0.0
    })),
    "missing caches should clear stale stage timings"
  );
}
