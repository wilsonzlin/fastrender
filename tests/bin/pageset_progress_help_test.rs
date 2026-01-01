use std::fs;
use std::process::Command;

use serde_json::Value;
use tempfile::tempdir;

#[test]
fn pageset_progress_help_exits_success() {
  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .arg("--help")
    .status()
    .expect("run pageset_progress --help");
  assert!(status.success(), "expected success for --help");
}

#[test]
fn pageset_progress_run_help_exits_success() {
  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args(["run", "--help"])
    .status()
    .expect("run pageset_progress run --help");
  assert!(status.success(), "expected success for run --help");
}

#[test]
fn pageset_progress_migrate_help_mentions_progress_dir_flag() {
  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args(["migrate", "--help"])
    .output()
    .expect("run pageset_progress migrate --help");
  assert!(
    output.status.success(),
    "expected success for migrate --help"
  );

  let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
  assert!(
    stdout.contains("--progress-dir"),
    "migrate help should mention --progress-dir"
  );
  assert!(
    stdout.contains("--html-dir"),
    "migrate help should mention --html-dir"
  );
}

#[test]
fn pageset_progress_run_help_mentions_font_flags() {
  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args(["run", "--help"])
    .output()
    .expect("run pageset_progress run --help");
  assert!(output.status.success(), "expected success for run --help");

  let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
  assert!(
    stdout.contains("--bundled-fonts"),
    "run help should mention --bundled-fonts"
  );
  assert!(
    stdout.contains("--font-dir <DIR>"),
    "run help should mention --font-dir <DIR>"
  );
}

#[test]
fn pageset_progress_worker_bundled_fonts_smoke_test() {
  let temp = tempdir().expect("create temp dir");
  let cache_path = temp.path().join("page.html");
  fs::write(
    &cache_path,
    "<!doctype html><html><body><p>Hello bundled fonts</p></body></html>",
  )
  .expect("write cache html");
  let progress_path = temp.path().join("progress.json");
  let log_path = temp.path().join("worker.log");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("FASTR_USE_BUNDLED_FONTS", "0")
    .args([
      "worker",
      "--cache-path",
      cache_path.to_str().unwrap(),
      "--stem",
      "smoke",
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
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker");

  assert!(
    status.success(),
    "worker should exit successfully with bundled fonts"
  );

  let progress_raw = fs::read_to_string(&progress_path).expect("read progress json");
  let progress: Value =
    serde_json::from_str(&progress_raw).expect("parse progress json written by worker");
  assert_eq!(
    progress["status"], "ok",
    "expected worker progress to report ok status"
  );
}
