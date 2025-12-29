use std::path::PathBuf;
use std::process::Command;

fn fixtures_dir() -> PathBuf {
  PathBuf::from("tests/fixtures/pageset_progress")
}

#[test]
fn pageset_progress_report_outputs_summary() {
  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args(["report", "--progress-dir", fixtures_dir().to_str().unwrap()])
    .output()
    .expect("run pageset_progress report");
  assert!(output.status.success(), "expected success for report");

  let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
  assert!(
    stdout.contains("Status counts (4 pages):"),
    "missing status header"
  );
  assert!(stdout.contains("ok: 2"));
  assert!(stdout.contains("timeout: 1"));
  assert!(stdout.contains("panic: 1"));
  assert!(stdout.contains("Slowest pages (top 4 of 4 with timings):"));
  assert!(stdout.contains("1. timeout_page (timeout"));
  assert!(stdout.contains("2. slow_ok (ok"));
  assert!(stdout.contains("Failure hotspots (timeout/panic/error):"));
  assert!(stdout.contains("layout: 1"));
  assert!(stdout.contains("unknown: 1"));
  assert!(stdout.contains("Top-slow hotspots (top 4):"));
  assert!(stdout.contains("layout: 3"));
  assert!(stdout.contains("Stage timings (ok pages with timings: 2):"));
  assert!(
    stdout.contains("totals_ms: fetch=55.00 css=110.00 cascade=220.00 layout=3040.00 paint=545.00")
  );
}

#[test]
fn pageset_progress_report_fail_on_bad_exits_non_zero() {
  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args([
      "report",
      "--progress-dir",
      fixtures_dir().to_str().unwrap(),
      "--fail-on-bad",
    ])
    .status()
    .expect("run pageset_progress report --fail-on-bad");
  assert!(
    !status.success(),
    "expected non-zero exit for timeout/panic when --fail-on-bad is set"
  );
}
