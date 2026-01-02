use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn pageset_progress_worker_renders_cached_http_error_status_by_default() {
  let temp = TempDir::new().expect("tempdir");
  let cache_path = temp.path().join("example.html");
  fs::write(
    &cache_path,
    "<!doctype html><html><body><p>cached status</p></body></html>",
  )
  .expect("write html");

  // Simulate a `fetch_pages` sidecar with an error status (e.g. Cloudflare challenge pages).
  let meta_path = cache_path.with_extension("html.meta");
  fs::write(
    &meta_path,
    "content-type: text/html; charset=utf-8\nstatus: 403\nurl: https://example.test/\n",
  )
  .expect("write meta");

  let progress_path = temp.path().join("progress.json");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
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
      "none",
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker");

  assert!(status.success(), "worker should exit successfully");

  let contents = fs::read_to_string(&progress_path).expect("read progress");
  let json: Value = serde_json::from_str(&contents).expect("parse progress JSON");
  assert_eq!(json["status"], "ok", "expected cached status to be non-fatal");
  assert!(
    json["auto_notes"]
      .as_str()
      .unwrap_or_default()
      .contains("cached_html_status=403"),
    "expected cached status note to be recorded, got: {}",
    json["auto_notes"]
  );
}

#[test]
fn pageset_progress_worker_can_fail_on_cached_http_error_status() {
  let temp = TempDir::new().expect("tempdir");
  let cache_path = temp.path().join("example.html");
  fs::write(
    &cache_path,
    "<!doctype html><html><body><p>cached status</p></body></html>",
  )
  .expect("write html");

  let meta_path = cache_path.with_extension("html.meta");
  fs::write(
    &meta_path,
    "content-type: text/html; charset=utf-8\nstatus: 403\nurl: https://example.test/\n",
  )
  .expect("write meta");

  let progress_path = temp.path().join("progress.json");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
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
      "none",
      "--bundled-fonts",
      "--fail-on-cached-http-error-status",
    ])
    .status()
    .expect("run pageset_progress worker");

  assert!(status.success(), "worker should exit successfully");

  let contents = fs::read_to_string(&progress_path).expect("read progress");
  let json: Value = serde_json::from_str(&contents).expect("parse progress JSON");
  assert_eq!(
    json["status"], "error",
    "expected cached status to be fatal when strict flag is enabled"
  );
}
