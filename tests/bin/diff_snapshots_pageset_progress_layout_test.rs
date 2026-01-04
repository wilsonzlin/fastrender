use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::tempdir;

#[test]
fn diff_snapshots_discovers_pageset_progress_layout() {
  let temp = tempdir().expect("create temp dir");
  let cache_path = temp.path().join("page.html");
  fs::write(
    &cache_path,
    "<!doctype html><html><body><div>slow</div></body></html>",
  )
  .expect("write cache html");

  let progress_path = temp.path().join("progress.json");
  let log_path = temp.path().join("worker.log");
  let before_dir = temp.path().join("before");
  let after_dir = temp.path().join("after");
  let stem = "dump";

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .env("FASTR_TEST_RENDER_DELAY_MS", "20")
    .env("FASTR_TEST_RENDER_DELAY_STEM", stem)
    .args([
      "worker",
      "--cache-path",
      cache_path.to_str().unwrap(),
      "--stem",
      stem,
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
      before_dir.to_str().unwrap(),
      "--dump-timeout",
      "5",
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker with dumps");

  assert!(status.success(), "worker should exit successfully");

  let before_snapshot = before_dir.join(stem).join("snapshot.json");
  assert!(
    before_snapshot.is_file(),
    "expected snapshot.json to be written under <stem>/"
  );

  let after_stem_dir = after_dir.join(stem);
  fs::create_dir_all(&after_stem_dir).expect("create after dump dir");
  fs::copy(&before_snapshot, after_stem_dir.join("snapshot.json")).expect("copy snapshot");

  let report_json = temp.path().join("report.json");
  let report_html = temp.path().join("report.html");
  let status = Command::new(env!("CARGO_BIN_EXE_diff_snapshots"))
    .args([
      "--before",
      before_dir.to_str().unwrap(),
      "--after",
      after_dir.to_str().unwrap(),
      "--json",
      report_json.to_str().unwrap(),
      "--html",
      report_html.to_str().unwrap(),
    ])
    .status()
    .expect("run diff_snapshots");

  assert!(status.success(), "diff_snapshots should exit successfully");

  let report: Value =
    serde_json::from_str(&fs::read_to_string(&report_json).expect("read report json"))
      .expect("parse report json");
  assert_eq!(
    report["totals"]["discovered"].as_u64(),
    Some(1),
    "expected one discovered snapshot"
  );
  assert_eq!(
    report["totals"]["matched"].as_u64(),
    Some(1),
    "expected snapshot pair to be matched"
  );
}

