use image::RgbaImage;
use serde_json::Value;
use std::fs;
use std::path::Path;
use std::process::Command;

fn write_color_png(path: &Path, color: [u8; 4]) {
  let img = RgbaImage::from_pixel(2, 2, image::Rgba(color));
  img.save(path).expect("save png");
}

fn diff_renders_cmd(tmp_dir: &Path) -> Command {
  let mut cmd = Command::new(env!("CARGO_BIN_EXE_diff_renders"));
  cmd.current_dir(tmp_dir);
  // Keep tests hermetic even if the caller has fixture env vars set.
  cmd.env_remove("FIXTURE_TOLERANCE");
  cmd.env_remove("FIXTURE_MAX_DIFFERENT_PERCENT");
  cmd.env_remove("FIXTURE_FUZZY");
  cmd.env_remove("FIXTURE_IGNORE_ALPHA");
  cmd.env_remove("FIXTURE_MAX_PERCEPTUAL_DISTANCE");
  cmd
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

  let status = diff_renders_cmd(tmp.path()).args([
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

  let status = diff_renders_cmd(tmp.path()).args([
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

  let status = diff_renders_cmd(tmp.path())
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

#[test]
fn diff_renders_supports_recursive_directories() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let before = tmp.path().join("before");
  let after = tmp.path().join("after");
  fs::create_dir_all(before.join("a")).unwrap();
  fs::create_dir_all(after.join("a")).unwrap();

  write_color_png(&before.join("a").join("x.png"), [255, 0, 0, 255]);
  write_color_png(&after.join("a").join("x.png"), [0, 0, 255, 255]);

  let status = diff_renders_cmd(tmp.path()).args([
      "--before",
      before.to_str().unwrap(),
      "--after",
      after.to_str().unwrap(),
      "--max-diff-percent",
      "100",
    ])
    .status()
    .expect("run diff_renders");

  assert!(status.success(), "expected success with max diff percent");

  let report: Value = serde_json::from_str(
    &fs::read_to_string(tmp.path().join("diff_report.json")).expect("read json"),
  )
  .unwrap();

  let entry = report["results"]
    .as_array()
    .expect("results array")
    .iter()
    .find(|e| e["name"] == "a/x")
    .expect("expected recursive entry name a/x");

  let diff_path = entry["diff"].as_str().expect("diff path missing");
  assert!(
    tmp.path().join(diff_path).exists(),
    "diff image missing at {}",
    diff_path
  );
  assert!(
    tmp
      .path()
      .join("diff_report_files")
      .join("diffs")
      .join("a")
      .join("x.png")
      .exists(),
    "expected diff image at diff_report_files/diffs/a/x.png"
  );
}

#[test]
fn diff_renders_supports_file_to_file_diffs() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let before_dir = tmp.path().join("before");
  let after_dir = tmp.path().join("after");
  fs::create_dir_all(&before_dir).unwrap();
  fs::create_dir_all(&after_dir).unwrap();

  let before = before_dir.join("page.png");
  let after = after_dir.join("page.png");
  write_color_png(&before, [0, 255, 0, 255]);
  write_color_png(&after, [0, 0, 0, 255]);

  let status = diff_renders_cmd(tmp.path()).args([
      "--before",
      before.to_str().unwrap(),
      "--after",
      after.to_str().unwrap(),
    ])
    .status()
    .expect("run diff_renders");

  assert!(
    !status.success(),
    "expected failure exit code for file diff"
  );

  let report: Value = serde_json::from_str(
    &fs::read_to_string(tmp.path().join("diff_report.json")).expect("read json"),
  )
  .unwrap();

  assert_eq!(report["totals"]["discovered"].as_u64(), Some(1));
  assert_eq!(report["results"][0]["name"], "page");
  assert_eq!(report["results"][0]["status"], "diff");

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
fn diff_renders_reports_perceptual_metric() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let before = tmp.path().join("before");
  let after = tmp.path().join("after");
  fs::create_dir_all(&before).unwrap();
  fs::create_dir_all(&after).unwrap();

  write_color_png(&before.join("page.png"), [0, 0, 0, 255]);
  write_color_png(&after.join("page.png"), [20, 20, 20, 255]);

  let status = diff_renders_cmd(tmp.path())
    .args([
      "--before",
      before.to_str().unwrap(),
      "--after",
      after.to_str().unwrap(),
      "--max-diff-percent",
      "100",
      "--max-perceptual-distance",
      "1",
    ])
    .status()
    .expect("run diff_renders");
  assert!(status.success());

  let report: Value = serde_json::from_str(
    &fs::read_to_string(tmp.path().join("diff_report.json")).expect("read json"),
  )
  .unwrap();
  let dist = report["results"][0]["metrics"]["perceptual_distance"]
    .as_f64()
    .expect("perceptual_distance missing");
  assert!(dist > 0.0);
}

#[test]
fn diff_renders_enforces_max_perceptual_distance() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let before = tmp.path().join("before");
  let after = tmp.path().join("after");
  fs::create_dir_all(&before).unwrap();
  fs::create_dir_all(&after).unwrap();

  write_color_png(&before.join("page.png"), [0, 0, 0, 255]);
  write_color_png(&after.join("page.png"), [255, 255, 255, 255]);

  let status = diff_renders_cmd(tmp.path())
    .args([
      "--before",
      before.to_str().unwrap(),
      "--after",
      after.to_str().unwrap(),
      "--max-diff-percent",
      "100",
      "--max-perceptual-distance",
      "0",
    ])
    .status()
    .expect("run diff_renders");

  assert!(!status.success(), "expected non-zero exit due to perceptual fail");

  let report: Value = serde_json::from_str(
    &fs::read_to_string(tmp.path().join("diff_report.json")).expect("read json"),
  )
  .unwrap();
  assert_eq!(report["totals"]["differences"].as_u64(), Some(1));
  assert_eq!(report["results"][0]["status"], "diff");
}

#[test]
fn diff_renders_sorts_by_perceptual_distance() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let before = tmp.path().join("before");
  let after = tmp.path().join("after");
  fs::create_dir_all(&before).unwrap();
  fs::create_dir_all(&after).unwrap();

  // Both entries differ in 100% of pixels, but with very different perceptual distance.
  write_color_png(&before.join("a.png"), [0, 0, 0, 255]);
  write_color_png(&after.join("a.png"), [10, 10, 10, 255]);
  write_color_png(&before.join("b.png"), [0, 0, 0, 255]);
  write_color_png(&after.join("b.png"), [255, 255, 255, 255]);

  let status = diff_renders_cmd(tmp.path())
    .args([
      "--before",
      before.to_str().unwrap(),
      "--after",
      after.to_str().unwrap(),
      "--max-diff-percent",
      "100",
      "--max-perceptual-distance",
      "1",
      "--sort-by",
      "perceptual",
    ])
    .status()
    .expect("run diff_renders");
  assert!(status.success());

  let report: Value = serde_json::from_str(
    &fs::read_to_string(tmp.path().join("diff_report.json")).expect("read json"),
  )
  .unwrap();
  assert_eq!(report["sort_by"], "perceptual");
  assert_eq!(report["results"][0]["name"], "b");
}
