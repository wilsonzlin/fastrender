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

#[test]
fn pageset_progress_worker_writes_fetch_error_summary_for_ok_pages() {
  let temp = TempDir::new().expect("tempdir");
  let cache_path = temp.path().join("example.html");
  fs::write(
    &cache_path,
    r#"<!doctype html><html><body><img src="missing.png" width="10" height="10"></body></html>"#,
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

  let summary = json
    .get("diagnostics")
    .and_then(|d| d.get("fetch_error_summary"))
    .expect("progress should include diagnostics.fetch_error_summary");
  assert!(
    summary.get("total").and_then(|v| v.as_u64()).unwrap_or(0) > 0,
    "fetch_error_summary should report at least one error"
  );
  let samples = summary
    .get("samples")
    .and_then(|v| v.as_array())
    .expect("fetch_error_summary.samples should be an array");
  assert!(
    !samples.is_empty(),
    "fetch_error_summary.samples should be non-empty"
  );
  let first = &samples[0];
  assert_eq!(first["kind"], "Image", "sample kind should be recorded");
  assert!(
    first["url"]
      .as_str()
      .is_some_and(|url| url.contains("missing.png")),
    "sample url should include missing.png"
  );
  assert!(
    first["message"]
      .as_str()
      .is_some_and(|m| !m.trim().is_empty()),
    "sample message should be non-empty"
  );
}

#[test]
fn pageset_progress_worker_truncates_fetch_error_sample_urls() {
  let temp = TempDir::new().expect("tempdir");
  let cache_path = temp.path().join("example.html");
  let long_query = "a".repeat(600);
  let html = format!(
    r#"<!doctype html><html><body><img src="missing.png?{long_query}" width="10" height="10"></body></html>"#
  );
  fs::write(&cache_path, html).expect("write html");

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
  let summary = json
    .get("diagnostics")
    .and_then(|d| d.get("fetch_error_summary"))
    .expect("progress should include diagnostics.fetch_error_summary");
  let samples = summary
    .get("samples")
    .and_then(|v| v.as_array())
    .expect("fetch_error_summary.samples should be an array");
  let first = &samples[0];
  let url = first["url"].as_str().expect("sample url should be string");
  assert!(
    url.contains("missing.png"),
    "truncated url should preserve the missing.png prefix"
  );
  assert!(
    url.chars().count() <= 240,
    "truncated url should stay within the progress URL size cap"
  );
  assert!(
    url.contains('…'),
    "truncated url should include an ellipsis marker"
  );
}
