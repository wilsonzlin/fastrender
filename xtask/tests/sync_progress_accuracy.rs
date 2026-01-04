use serde_json::json;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::tempdir;

fn repo_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .expect("xtask crate should live under the workspace root")
    .to_path_buf()
}

fn git_head_sha() -> String {
  let output = Command::new("git")
    .arg("rev-parse")
    .arg("HEAD")
    .current_dir(repo_root())
    .output()
    .expect("git rev-parse HEAD");
  assert!(output.status.success(), "git rev-parse should succeed");
  String::from_utf8_lossy(&output.stdout).trim().to_string()
}

fn write_progress(dir: &Path, stem: &str) {
  let progress = json!({
    "url": format!("https://{stem}.example.com/"),
    "status": "ok",
    "total_ms": 1.0,
    "stages_ms": {
      "fetch": 0.1,
      "css": 0.1,
      "cascade": 0.1,
      "layout": 0.7,
      "paint": 0.0
    },
    "notes": "",
    "hotspot": "layout",
    "last_good_commit": "",
    "last_regression_commit": ""
  });
  let path = dir.join(format!("{stem}.json"));
  fs::write(
    &path,
    format!("{}\n", serde_json::to_string_pretty(&progress).unwrap()),
  )
  .unwrap_or_else(|_| panic!("write {}", path.display()));
}

fn write_report(path: &Path) {
  let report = json!({
    "tolerance": 2,
    "max_diff_percent": 0.5,
    "results": [{
      "name": "a",
      "status": "diff",
      "metrics": {
        "pixel_diff": 123,
        "total_pixels": 1000,
        "diff_percentage": 12.3456789,
        "perceptual_distance": 0.1234567
      }
    }]
  });
  fs::write(path, serde_json::to_string_pretty(&report).unwrap())
    .unwrap_or_else(|_| panic!("write {}", path.display()));
}

#[test]
fn sync_progress_accuracy_writes_accuracy_block() {
  let temp = tempdir().expect("tempdir");
  let progress_dir = temp.path().join("progress");
  fs::create_dir_all(&progress_dir).expect("create progress dir");
  write_progress(&progress_dir, "a");

  let report_path = temp.path().join("report.json");
  write_report(&report_path);

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .arg("sync-progress-accuracy")
    .arg("--report")
    .arg(&report_path)
    .arg("--progress-dir")
    .arg(&progress_dir)
    .output()
    .expect("run xtask sync-progress-accuracy");

  assert!(
    output.status.success(),
    "expected sync-progress-accuracy to succeed.\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let updated_raw =
    fs::read_to_string(progress_dir.join("a.json")).expect("read updated progress file");
  let updated: serde_json::Value = serde_json::from_str(&updated_raw).expect("parse updated json");
  let accuracy = updated
    .get("accuracy")
    .expect("accuracy field should exist");

  assert_eq!(
    accuracy.get("baseline").and_then(|v| v.as_str()),
    Some("chrome")
  );
  assert_eq!(
    accuracy.get("diff_pixels").and_then(|v| v.as_u64()),
    Some(123)
  );
  assert_eq!(
    accuracy.get("diff_percent").and_then(|v| v.as_f64()),
    Some(12.3457)
  );
  assert_eq!(
    accuracy.get("perceptual").and_then(|v| v.as_f64()),
    Some(0.1235)
  );
  assert_eq!(accuracy.get("tolerance").and_then(|v| v.as_u64()), Some(2));
  assert_eq!(
    accuracy.get("max_diff_percent").and_then(|v| v.as_f64()),
    Some(0.5)
  );
  let sha = git_head_sha();
  assert_eq!(
    accuracy.get("computed_at_commit").and_then(|v| v.as_str()),
    Some(sha.as_str())
  );
}

#[test]
fn sync_progress_accuracy_dry_run_does_not_write() {
  let temp = tempdir().expect("tempdir");
  let progress_dir = temp.path().join("progress");
  fs::create_dir_all(&progress_dir).expect("create progress dir");
  write_progress(&progress_dir, "a");

  let report_path = temp.path().join("report.json");
  write_report(&report_path);

  let original = fs::read_to_string(progress_dir.join("a.json")).expect("read original progress");

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .arg("sync-progress-accuracy")
    .arg("--dry-run")
    .arg("--report")
    .arg(&report_path)
    .arg("--progress-dir")
    .arg(&progress_dir)
    .output()
    .expect("run xtask sync-progress-accuracy --dry-run");

  assert!(
    output.status.success(),
    "expected dry-run to succeed.\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let after = fs::read_to_string(progress_dir.join("a.json")).expect("read progress after dry run");
  assert_eq!(after, original, "dry-run should not modify progress JSON");
}
