use image::RgbaImage;
use serde_json::Value;
use std::fs;
use std::path::Path;
use std::process::Command;

fn write_color_png(path: &Path, color: [u8; 4]) {
  let img = RgbaImage::from_pixel(2, 2, image::Rgba(color));
  img.save(path).expect("save png");
}

#[test]
fn diff_renders_reports_matches() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let before = tmp.path().join("before");
  let after = tmp.path().join("after");
  fs::create_dir_all(&before).unwrap();
  fs::create_dir_all(&after).unwrap();

  write_color_png(&before.join("page.png"), [10, 20, 30, 255]);
  write_color_png(&after.join("page.png"), [10, 20, 30, 255]);

  let status = Command::new(env!("CARGO_BIN_EXE_diff_renders"))
    .current_dir(tmp.path())
    .args([
      "--before",
      before.to_str().unwrap(),
      "--after",
      after.to_str().unwrap(),
      "--max-diff-percent",
      "0",
    ])
    .status()
    .expect("run diff_renders");

  assert!(
    status.success(),
    "expected success, got {:?}",
    status.code()
  );

  let json_path = tmp.path().join("diff_report.json");
  assert!(json_path.exists(), "json report missing");
  let report: Value = serde_json::from_str(&fs::read_to_string(&json_path).unwrap()).unwrap();
  assert_eq!(
    report["totals"]["differences"].as_u64(),
    Some(0),
    "expected no differences"
  );
  assert_eq!(
    report["results"][0]["status"], "match",
    "expected match status in report"
  );

  assert!(
    tmp.path().join("diff_report.html").exists(),
    "html report missing"
  );
}

#[test]
fn diff_renders_exits_non_zero_on_diff() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let before = tmp.path().join("before");
  let after = tmp.path().join("after");
  fs::create_dir_all(&before).unwrap();
  fs::create_dir_all(&after).unwrap();

  write_color_png(&before.join("page.png"), [255, 0, 0, 255]);
  write_color_png(&after.join("page.png"), [0, 0, 255, 255]);

  let status = Command::new(env!("CARGO_BIN_EXE_diff_renders"))
    .current_dir(tmp.path())
    .args([
      "--before",
      before.to_str().unwrap(),
      "--after",
      after.to_str().unwrap(),
    ])
    .status()
    .expect("run diff_renders");

  assert!(
    !status.success(),
    "expected failure exit code, got {:?}",
    status.code()
  );

  let json_path = tmp.path().join("diff_report.json");
  assert!(json_path.exists(), "json report missing");
  let report: Value = serde_json::from_str(&fs::read_to_string(&json_path).unwrap()).unwrap();
  assert_eq!(
    report["totals"]["differences"].as_u64(),
    Some(1),
    "expected one failing diff"
  );
  assert_eq!(report["results"][0]["status"], "diff");

  let html_path = tmp.path().join("diff_report.html");
  assert!(html_path.exists(), "html report missing");

  let diff_path = report["results"][0]["diff"]
    .as_str()
    .expect("diff path missing");
  assert!(
    tmp.path().join(diff_path).exists(),
    "diff image missing at {}",
    diff_path
  );
}

#[test]
fn diff_renders_respects_shard_and_env_tolerance() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let before = tmp.path().join("before");
  let after = tmp.path().join("after");
  fs::create_dir_all(&before).unwrap();
  fs::create_dir_all(&after).unwrap();

  write_color_png(&before.join("a.png"), [10, 10, 10, 255]);
  write_color_png(&after.join("a.png"), [12, 12, 12, 255]);
  write_color_png(&before.join("b.png"), [0, 0, 0, 255]);
  write_color_png(&after.join("b.png"), [255, 255, 255, 255]);

  let status = Command::new(env!("CARGO_BIN_EXE_diff_renders"))
    .current_dir(tmp.path())
    .env("FIXTURE_TOLERANCE", "5")
    .args([
      "--before",
      before.to_str().unwrap(),
      "--after",
      after.to_str().unwrap(),
      "--shard",
      "0/2",
    ])
    .status()
    .expect("run diff_renders");

  assert!(
    status.success(),
    "expected success with tolerance and shard"
  );

  let report: Value = serde_json::from_str(
    &fs::read_to_string(tmp.path().join("diff_report.json")).expect("read json"),
  )
  .unwrap();
  assert_eq!(report["totals"]["processed"].as_u64(), Some(1));
  assert_eq!(report["totals"]["shard_skipped"].as_u64(), Some(1));
  assert_eq!(report["results"][0]["status"], "match");
}
