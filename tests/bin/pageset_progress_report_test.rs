use serde_json::json;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::tempdir;

fn fixtures_dir() -> PathBuf {
  PathBuf::from("tests/fixtures/pageset_progress")
}

fn stats_fixtures_dir() -> PathBuf {
  PathBuf::from("tests/fixtures/pageset_progress_stats")
}

fn write_progress(
  dir: &Path,
  stem: &str,
  status: &str,
  total_ms: Option<f64>,
  stages: (f64, f64, f64, f64, f64),
) {
  let progress = json!({
    "url": format!("https://{stem}.example.com/"),
    "status": status,
    "total_ms": total_ms,
    "stages_ms": {
      "fetch": stages.0,
      "css": stages.1,
      "cascade": stages.2,
      "layout": stages.3,
      "paint": stages.4
    },
    "notes": "",
    "hotspot": "",
    "last_good_commit": "",
    "last_regression_commit": ""
  });
  let path = dir.join(format!("{stem}.json"));
  fs::write(&path, serde_json::to_string_pretty(&progress).unwrap())
    .unwrap_or_else(|_| panic!("write {}", path.display()));
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
  assert!(stdout.contains("Failure stages (timeout/panic/error):"));
  assert!(stdout.contains("unknown: 2"));
  assert!(stdout.contains("Top-slow hotspots (top 4):"));
  assert!(stdout.contains("layout: 3"));
  assert!(stdout.contains("Stage timings (ok pages with timings: 2):"));
  assert!(
    stdout.contains("totals_ms: fetch=55.00 css=110.00 cascade=220.00 layout=3040.00 paint=545.00")
  );
}

#[test]
fn pageset_progress_report_outputs_stats_when_verbose() {
  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args([
      "report",
      "--progress-dir",
      stats_fixtures_dir().to_str().unwrap(),
      "--top",
      "1",
      "--verbose-stats",
    ])
    .output()
    .expect("run pageset_progress report --verbose-stats");
  assert!(output.status.success(), "expected success for report");

  let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
  assert!(stdout.contains("Slowest pages (top 1 of 1 with timings):"));
  assert!(stdout.contains("stats:"));
  assert!(stdout.contains("nodes: dom=1200 styled=1100 boxes=900 fragments=1500"));
  assert!(stdout.contains(
    "cascade: nodes=1100 candidates=6000 matches=2000 selector=320.50ms declaration=210.25ms"
  ));
  assert!(stdout.contains(
    "layout: layout_cache lookups=450 hits=300 stores=120 evictions=10 | intrinsic lookups=50 hits=20 | parallel enabled=true items=12 threads=2"
  ));
  assert!(stdout.contains("paint: display_items=800 optimized_items=500 culled_items=120"));
  assert!(stdout.contains(
    "resources: fetches doc=1 css=4 img=6 font=2 other=0 | image_cache hits=5 misses=3 | resource_cache fresh_hits=10 stale_hits=2 revalidated_hits=1 misses=3 bytes=12.1KiB | inflight waits=7 ms=45.67ms | disk_cache hits=4 misses=1 bytes=66.3KiB lock_waits=2 lock_wait=3.21ms ms=12.34ms | network fetches=5 bytes=4.1KiB ms=67.89ms"
  ));
  assert!(stdout.contains("Resource totals (pages with stats: 1):"));
  assert!(stdout.contains("Top network fetch time (top 1 of 1 with stats):"));
  assert!(stdout.contains("Top inflight wait time (top 1 of 1 with stats):"));
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

#[test]
fn pageset_progress_report_compares_and_detects_regressions() {
  let baseline = tempdir().expect("baseline dir");
  let current = tempdir().expect("current dir");

  write_progress(
    baseline.path(),
    "regress_slow",
    "ok",
    Some(100.0),
    (10.0, 10.0, 20.0, 40.0, 20.0),
  );
  write_progress(
    baseline.path(),
    "becomes_timeout",
    "ok",
    Some(80.0),
    (10.0, 10.0, 20.0, 20.0, 20.0),
  );
  write_progress(
    baseline.path(),
    "improves_fast",
    "ok",
    Some(300.0),
    (50.0, 50.0, 70.0, 80.0, 50.0),
  );
  write_progress(
    baseline.path(),
    "removed_page",
    "ok",
    Some(120.0),
    (20.0, 20.0, 20.0, 40.0, 20.0),
  );

  write_progress(
    current.path(),
    "regress_slow",
    "ok",
    Some(160.0),
    (10.0, 20.0, 40.0, 60.0, 30.0),
  );
  write_progress(
    current.path(),
    "becomes_timeout",
    "timeout",
    Some(5000.0),
    (1000.0, 1000.0, 1000.0, 1000.0, 1000.0),
  );
  write_progress(
    current.path(),
    "improves_fast",
    "ok",
    Some(150.0),
    (20.0, 30.0, 30.0, 40.0, 30.0),
  );
  write_progress(
    current.path(),
    "new_page",
    "ok",
    Some(70.0),
    (20.0, 10.0, 10.0, 20.0, 10.0),
  );

  let comparison = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args([
      "report",
      "--progress-dir",
      current.path().to_str().unwrap(),
      "--compare",
      baseline.path().to_str().unwrap(),
      "--top",
      "3",
    ])
    .output()
    .expect("run pageset_progress report with compare");
  assert!(
    comparison.status.success(),
    "expected success comparing progress dirs"
  );
  let stdout = String::from_utf8(comparison.stdout).expect("stdout is utf-8");
  assert!(
    stdout.contains("Status transitions"),
    "missing transitions header"
  );
  assert!(
    stdout.contains("ok -> timeout: 1"),
    "missing ok -> timeout transition"
  );
  assert!(
    stdout.contains("ok -> missing: 1"),
    "missing ok -> missing transition"
  );
  assert!(
    stdout.contains("missing -> ok: 1"),
    "missing missing -> ok transition"
  );
  assert!(
    stdout.contains("Regressions vs baseline (top 2 of 2 with timings):"),
    "missing regression header"
  );
  assert!(
    stdout.contains("becomes_timeout (ok -> timeout) Δtotal=+4920.00ms"),
    "missing timeout regression entry"
  );
  assert!(
    stdout.contains("regress_slow (ok -> ok) Δtotal=+60.00ms (+60.00%) stages_ms=fetch:+0.00 css:+10.00 cascade:+20.00"),
    "missing stage deltas for regression"
  );
  assert!(
    stdout.contains("Improvements vs baseline (top 1 of 1 with timings):"),
    "missing improvement header"
  );
  assert!(
    stdout.contains("improves_fast (ok -> ok) Δtotal=-150.00ms (-50.00%) stages_ms=fetch:-30.00 css:-20.00 cascade:-40.00 layout:-40.00 paint:-20.00"),
    "missing improvement entry"
  );

  let failure = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args([
      "report",
      "--progress-dir",
      current.path().to_str().unwrap(),
      "--compare",
      baseline.path().to_str().unwrap(),
      "--fail-on-regression",
      "--regression-threshold-percent",
      "20",
    ])
    .output()
    .expect("run pageset_progress report --fail-on-regression");
  assert!(
    !failure.status.success(),
    "expected non-zero exit for regressions"
  );
  let stderr = String::from_utf8(failure.stderr).expect("stderr is utf-8");
  assert!(
    stderr.contains("becomes_timeout: ok -> timeout"),
    "missing ok->timeout failure reason"
  );
  assert!(
    stderr.contains("regress_slow: Δtotal=+60.00ms (+60.00%)"),
    "missing slowdown failure reason"
  );
}
