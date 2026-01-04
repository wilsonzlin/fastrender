use serde_json::json;
use std::fs;
use std::path::Path;
use std::process::Command;
use tempfile::tempdir;

fn write_progress(dir: &Path, stem: &str, total_ms: f64, diff_percent: f64, diff_pixels: u64) {
  let progress = json!({
    "url": format!("https://{stem}.example.com/"),
    "status": "ok",
    "total_ms": total_ms,
    "stages_ms": {
      "fetch": 1.0,
      "css": 1.0,
      "cascade": 1.0,
      "layout": total_ms - 3.0,
      "paint": 0.0
    },
    "notes": "",
    "hotspot": "layout",
    "last_good_commit": "",
    "last_regression_commit": "",
    "accuracy": {
      "baseline": "chrome",
      "diff_pixels": diff_pixels,
      "diff_percent": diff_percent,
      "perceptual": 0.1234,
      "tolerance": 0,
      "max_diff_percent": 0.0,
      "computed_at_commit": "deadbeef"
    }
  });
  let path = dir.join(format!("{stem}.json"));
  fs::write(&path, serde_json::to_string_pretty(&progress).unwrap())
    .unwrap_or_else(|_| panic!("write {}", path.display()));
}

fn extract_accuracy_lines(stdout: &str) -> Vec<&str> {
  let mut lines = stdout.lines();
  while let Some(line) = lines.next() {
    if line.starts_with("Worst accuracy pages") {
      break;
    }
  }
  lines
    .take_while(|line| line.starts_with("  ") && !line.trim().is_empty())
    .collect()
}

#[test]
fn pageset_progress_report_ranks_by_accuracy() {
  let dir = tempdir().expect("tempdir");
  write_progress(dir.path(), "match_page", 10.0, 0.0, 0);
  write_progress(dir.path(), "minor_diff", 11.0, 10.0, 400);
  write_progress(dir.path(), "major_diff", 12.0, 25.0, 1000);

  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      dir.path().to_str().unwrap(),
      "--top",
      "3",
      "--rank-accuracy",
    ])
    .output()
    .expect("run pageset_progress report --rank-accuracy");
  assert!(output.status.success(), "expected success for report");
  let stdout = String::from_utf8(output.stdout).expect("stdout utf-8");

  let lines = extract_accuracy_lines(&stdout);
  assert!(lines.len() >= 3, "expected 3 ranking lines, got:\n{stdout}");
  assert!(
    lines[0].contains("major_diff diff=25.0000%"),
    "expected major_diff to rank first, got: {}",
    lines[0]
  );
  assert!(
    lines[1].contains("minor_diff diff=10.0000%"),
    "expected minor_diff to rank second, got: {}",
    lines[1]
  );
  assert!(
    lines[2].contains("match_page diff=0.0000%"),
    "expected match_page to rank third, got: {}",
    lines[2]
  );
}

#[test]
fn pageset_progress_report_accuracy_filters_work() {
  let dir = tempdir().expect("tempdir");
  write_progress(dir.path(), "match_page", 10.0, 0.0, 0);
  write_progress(dir.path(), "minor_diff", 11.0, 10.0, 400);
  write_progress(dir.path(), "major_diff", 12.0, 25.0, 1000);

  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      dir.path().to_str().unwrap(),
      "--top",
      "10",
      "--only-diff",
    ])
    .output()
    .expect("run pageset_progress report --only-diff");
  assert!(output.status.success(), "expected success for report");
  let stdout = String::from_utf8(output.stdout).expect("stdout utf-8");
  assert!(stdout.contains("major_diff diff=25.0000%"));
  assert!(stdout.contains("minor_diff diff=10.0000%"));
  assert!(
    !stdout.contains("match_page diff=0.0000%"),
    "expected match_page to be filtered out by --only-diff"
  );

  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      dir.path().to_str().unwrap(),
      "--top",
      "10",
      "--min-diff-percent",
      "20",
    ])
    .output()
    .expect("run pageset_progress report --min-diff-percent");
  assert!(output.status.success(), "expected success for report");
  let stdout = String::from_utf8(output.stdout).expect("stdout utf-8");
  assert!(stdout.contains("major_diff diff=25.0000%"));
  assert!(
    !stdout.contains("minor_diff diff=10.0000%"),
    "expected minor_diff to be filtered out by --min-diff-percent"
  );
}

#[test]
fn pageset_progress_report_verbose_includes_accuracy_block() {
  let dir = tempdir().expect("tempdir");
  // Make this the slowest page so it appears in the `--top 1` list.
  write_progress(dir.path(), "major_diff", 12.0, 25.0, 1000);
  write_progress(dir.path(), "minor_diff", 11.0, 10.0, 400);

  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      dir.path().to_str().unwrap(),
      "--top",
      "1",
      "--verbose",
    ])
    .output()
    .expect("run pageset_progress report --verbose");
  assert!(output.status.success(), "expected success for report");
  let stdout = String::from_utf8(output.stdout).expect("stdout utf-8");

  assert!(
    stdout.contains("accuracy: baseline=chrome"),
    "expected verbose report to include accuracy block, got:\n{stdout}"
  );
}

