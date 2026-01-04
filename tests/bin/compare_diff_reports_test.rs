use serde_json::{json, Value};
use std::fs;
use std::path::Path;
use std::process::Command;

fn write_json(path: &Path, value: &Value) {
  fs::write(path, serde_json::to_string_pretty(value).unwrap()).unwrap();
}

fn compare_cmd(tmp_dir: &Path) -> Command {
  let mut cmd = Command::new(env!("CARGO_BIN_EXE_compare_diff_reports"));
  cmd.current_dir(tmp_dir);
  cmd
}

fn basic_report(entries: Vec<Value>) -> Value {
  json!({
    "before_dir": "chrome",
    "after_dir": "fastrender",
    "tolerance": 0,
    "max_diff_percent": 0.0,
    "ignore_alpha": false,
    "results": entries,
    "totals": {
      "discovered": entries.len(),
      "processed": entries.len(),
      "matches": 0,
      "within_threshold": 0,
      "differences": 0,
      "missing": 0,
      "errors": 0,
      "shard_skipped": 0
    }
  })
}

#[test]
fn compare_diff_reports_pairs_and_classifies_entries() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let baseline_path = tmp.path().join("baseline.json");
  let new_path = tmp.path().join("new.json");
  let out_json = tmp.path().join("delta.json");
  let out_html = tmp.path().join("delta.html");

  let baseline = basic_report(vec![
    json!({
      "name": "a",
      "status": "diff",
      "metrics": {
        "pixel_diff": 10,
        "total_pixels": 100,
        "diff_percentage": 10.0,
        "perceptual_distance": 0.5
      }
    }),
    json!({
      "name": "b",
      "status": "diff",
      "metrics": {
        "pixel_diff": 0,
        "total_pixels": 100,
        "diff_percentage": 0.0,
        "perceptual_distance": 0.0
      }
    }),
    json!({
      "name": "c",
      "status": "missing_after",
      "error": "Missing in after input"
    }),
    json!({
      "name": "e",
      "status": "diff",
      "metrics": {
        "pixel_diff": 50,
        "total_pixels": 100,
        "diff_percentage": 50.0,
        "perceptual_distance": 0.9
      }
    }),
  ]);
  let new_report = basic_report(vec![
    json!({
      "name": "a",
      "status": "diff",
      "metrics": {
        "pixel_diff": 5,
        "total_pixels": 100,
        "diff_percentage": 5.0,
        "perceptual_distance": 0.25
      }
    }),
    json!({
      "name": "b",
      "status": "diff",
      "metrics": {
        "pixel_diff": 1,
        "total_pixels": 100,
        "diff_percentage": 1.0,
        "perceptual_distance": 0.1
      }
    }),
    json!({
      "name": "c",
      "status": "error",
      "error": "Diff failed"
    }),
    json!({
      "name": "d",
      "status": "diff",
      "metrics": {
        "pixel_diff": 2,
        "total_pixels": 100,
        "diff_percentage": 2.0,
        "perceptual_distance": 0.2
      }
    }),
  ]);

  write_json(&baseline_path, &baseline);
  write_json(&new_path, &new_report);

  let status = compare_cmd(tmp.path())
    .args([
      "--baseline",
      baseline_path.to_str().unwrap(),
      "--new",
      new_path.to_str().unwrap(),
      "--json",
      out_json.to_str().unwrap(),
      "--html",
      out_html.to_str().unwrap(),
    ])
    .status()
    .expect("run compare_diff_reports");

  assert!(
    status.success(),
    "expected success, got {:?}",
    status.code()
  );
  assert!(out_json.exists(), "missing delta json");
  assert!(out_html.exists(), "missing delta html");

  let report: Value = serde_json::from_str(&fs::read_to_string(&out_json).unwrap()).unwrap();
  let results = report["results"].as_array().expect("results array");

  let find = |name: &str| {
    results
      .iter()
      .find(|entry| entry["name"] == name)
      .unwrap_or_else(|| panic!("missing entry {name}"))
  };

  let a = find("a");
  assert_eq!(a["classification"], "improved");
  assert_eq!(a["diff_percentage_delta"].as_f64(), Some(-5.0));

  let b = find("b");
  assert_eq!(b["classification"], "regressed");
  assert_eq!(b["diff_percentage_delta"].as_f64(), Some(1.0));

  let c = find("c");
  assert_eq!(c["classification"], "regressed");
  assert!(c.get("diff_percentage_delta").is_none());

  let d = find("d");
  assert_eq!(d["classification"], "missing_in_baseline");

  let e = find("e");
  assert_eq!(e["classification"], "missing_in_new");
}

#[test]
fn compare_diff_reports_can_gate_on_regressions() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let baseline_path = tmp.path().join("baseline.json");
  let new_path = tmp.path().join("new.json");

  let baseline = basic_report(vec![json!({
    "name": "a",
    "status": "diff",
    "metrics": {
      "pixel_diff": 0,
      "total_pixels": 100,
      "diff_percentage": 0.0,
      "perceptual_distance": 0.0
    }
  })]);
  let new_report = basic_report(vec![json!({
    "name": "a",
    "status": "diff",
    "metrics": {
      "pixel_diff": 1,
      "total_pixels": 100,
      "diff_percentage": 0.5,
      "perceptual_distance": 0.1
    }
  })]);

  write_json(&baseline_path, &baseline);
  write_json(&new_path, &new_report);

  let out_json_fail = tmp.path().join("delta_fail.json");
  let out_html_fail = tmp.path().join("delta_fail.html");
  let status = compare_cmd(tmp.path())
    .args([
      "--baseline",
      baseline_path.to_str().unwrap(),
      "--new",
      new_path.to_str().unwrap(),
      "--json",
      out_json_fail.to_str().unwrap(),
      "--html",
      out_html_fail.to_str().unwrap(),
      "--fail-on-regression",
      "--regression-threshold-percent",
      "0",
    ])
    .status()
    .expect("run compare_diff_reports");

  assert!(!status.success(), "expected failure exit code");
  assert!(out_json_fail.exists(), "delta json should still be written");

  let out_json_pass = tmp.path().join("delta_pass.json");
  let out_html_pass = tmp.path().join("delta_pass.html");
  let status = compare_cmd(tmp.path())
    .args([
      "--baseline",
      baseline_path.to_str().unwrap(),
      "--new",
      new_path.to_str().unwrap(),
      "--json",
      out_json_pass.to_str().unwrap(),
      "--html",
      out_html_pass.to_str().unwrap(),
      "--fail-on-regression",
      "--regression-threshold-percent",
      "1",
    ])
    .status()
    .expect("run compare_diff_reports");

  assert!(status.success(), "expected success with threshold");
}

#[test]
fn compare_diff_reports_requires_matching_config_by_default() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let baseline_path = tmp.path().join("baseline.json");
  let new_path = tmp.path().join("new.json");
  let out_json_fail = tmp.path().join("delta_fail.json");
  let out_html_fail = tmp.path().join("delta_fail.html");
  let out_json_ok = tmp.path().join("delta_ok.json");
  let out_html_ok = tmp.path().join("delta_ok.html");

  let mut baseline = basic_report(vec![]);
  baseline["tolerance"] = json!(0);
  let mut new_report = basic_report(vec![]);
  new_report["tolerance"] = json!(5);

  write_json(&baseline_path, &baseline);
  write_json(&new_path, &new_report);

  let status = compare_cmd(tmp.path())
    .args([
      "--baseline",
      baseline_path.to_str().unwrap(),
      "--new",
      new_path.to_str().unwrap(),
      "--json",
      out_json_fail.to_str().unwrap(),
      "--html",
      out_html_fail.to_str().unwrap(),
    ])
    .status()
    .expect("run compare_diff_reports");

  assert!(
    !status.success(),
    "expected non-zero exit for config mismatch"
  );

  assert!(out_json_fail.exists(), "delta json should still be written");
  assert!(out_html_fail.exists(), "delta html should still be written");
  let report: Value =
    serde_json::from_str(&fs::read_to_string(&out_json_fail).expect("read delta json")).unwrap();
  assert_eq!(report["config_mismatches"].as_array().unwrap().len(), 1);
  assert_eq!(report["config_mismatches"][0]["field"], "tolerance");

  let status = compare_cmd(tmp.path())
    .args([
      "--baseline",
      baseline_path.to_str().unwrap(),
      "--new",
      new_path.to_str().unwrap(),
      "--json",
      out_json_ok.to_str().unwrap(),
      "--html",
      out_html_ok.to_str().unwrap(),
      "--allow-config-mismatch",
    ])
    .status()
    .expect("run compare_diff_reports");

  assert!(status.success(), "expected success when mismatch allowed");

  let report: Value = serde_json::from_str(&fs::read_to_string(&out_json_ok).unwrap()).unwrap();
  assert_eq!(report["config_mismatches"].as_array().unwrap().len(), 1);
  assert_eq!(report["config_mismatches"][0]["field"], "tolerance");
}
