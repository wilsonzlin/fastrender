use std::fs;
use std::process::{Command, Stdio};

use serde_json::Value;
use tempfile::tempdir;

const PAGESET_TIMEOUT_MANIFEST_ENV: &str = "FASTR_PERF_SMOKE_PAGESET_TIMEOUT_MANIFEST";

#[test]
fn perf_smoke_emits_stage_breakdowns() {
  let temp = tempdir().expect("create temp dir");
  let output = temp.path().join("perf-smoke.json");

  let status = Command::new(env!("CARGO_BIN_EXE_perf_smoke"))
    .args([
      "--output",
      output.to_str().unwrap(),
      "--only",
      "flex_dashboard",
    ])
    .stdout(Stdio::null())
    .status()
    .expect("run perf_smoke");

  assert!(status.success(), "perf_smoke should exit successfully");

  let data = fs::read_to_string(&output).expect("read perf_smoke output");
  let summary: Value = serde_json::from_str(&data).expect("parse perf_smoke json");
  assert_eq!(
    summary["schema_version"].as_u64(),
    Some(5),
    "perf_smoke schema_version should be current"
  );
  let fixtures = summary["fixtures"]
    .as_array()
    .expect("fixtures array must exist");
  assert_eq!(
    fixtures.len(),
    1,
    "fixture filter should limit to one entry"
  );
  let fixture = &fixtures[0];
  assert_eq!(
    fixture["status"].as_str(),
    Some("ok"),
    "fixture status should be ok for successful render"
  );
  assert!(
    fixture.get("error").is_some(),
    "fixture should include an error field (null for ok fixtures)"
  );
  for key in ["fetch", "css", "cascade", "layout", "paint"] {
    assert!(
      fixture["stage_ms"][key].as_f64().is_some(),
      "fixture stage_ms should contain numeric {key}"
    );
    assert!(
      summary["stage_ms"][key].as_f64().is_some(),
      "summary stage_ms should contain numeric {key}"
    );
  }

  for key in ["text_fallback_ms", "text_shape_ms", "text_rasterize_ms"] {
    assert!(
      fixture["timings_ms"][key].as_f64().is_some(),
      "fixture timings_ms should contain numeric {key}"
    );
  }

  for key in [
    "shaped_runs",
    "glyphs",
    "fallback_cache_hits",
    "fallback_cache_misses",
    "last_resort_font_fallbacks",
  ] {
    assert!(
      fixture["counts"][key].as_u64().is_some(),
      "fixture counts should contain numeric {key}"
    );
  }
}

#[test]
fn perf_smoke_detects_count_regressions() {
  let temp = tempdir().expect("create temp dir");
  let baseline_path = temp.path().join("baseline.json");
  let baseline_output = temp.path().join("baseline-out.json");

  let result = Command::new(env!("CARGO_BIN_EXE_perf_smoke"))
    .args([
      "--output",
      baseline_output.to_str().unwrap(),
      "--only",
      "flex_dashboard",
    ])
    .stdout(Stdio::null())
    .output()
    .expect("run perf_smoke to generate baseline");

  assert!(
    result.status.success(),
    "baseline perf_smoke run should succeed; stderr: {}",
    String::from_utf8_lossy(&result.stderr)
  );

  let data = fs::read_to_string(&baseline_output).expect("read baseline output");
  let mut baseline: Value = serde_json::from_str(&data).expect("parse baseline json");

  let dom_nodes = baseline["fixtures"][0]["counts"]["dom_nodes"]
    .as_u64()
    .expect("baseline should include dom_nodes");
  // Ensure a deterministic counts regression by making the baseline value much smaller.
  let reduced = (dom_nodes / 10).max(1);
  baseline["fixtures"][0]["counts"]["dom_nodes"] = Value::from(reduced);
  fs::write(&baseline_path, serde_json::to_string(&baseline).unwrap()).unwrap();

  let latest_output = temp.path().join("latest.json");
  let result = Command::new(env!("CARGO_BIN_EXE_perf_smoke"))
    .args([
      "--output",
      latest_output.to_str().unwrap(),
      "--only",
      "flex_dashboard",
      "--baseline",
      baseline_path.to_str().unwrap(),
      "--threshold",
      "10.0",
      "--count-threshold",
      "0.01",
      "--fail-on-regression",
    ])
    .stdout(Stdio::null())
    .output()
    .expect("run perf_smoke with baseline");

  assert!(
    !result.status.success(),
    "perf_smoke should exit non-zero when counts regressions are detected"
  );
  let stderr = String::from_utf8_lossy(&result.stderr);
  assert!(
    stderr.contains("counts.dom_nodes"),
    "stderr should include counts regression label; got: {stderr}"
  );
}

#[test]
fn perf_smoke_fail_on_missing_fixtures_exits_non_zero() {
  let temp = tempdir().expect("create temp dir");
  let output = temp.path().join("perf-smoke.json");
  let manifest_path = temp.path().join("manifest.json");
  let manifest = serde_json::json!({
    "schema_version": 1,
    "fixtures": [
      {
        "name": "flex_dashboard",
        "viewport": [1040, 1240],
        "dpr": 1.0,
        "media": "screen",
        "budget_ms": 100000.0
      },
      {
        "name": "this_fixture_should_not_exist_12345",
        "viewport": [1040, 1240],
        "dpr": 1.0,
        "media": "screen",
        "budget_ms": 100000.0
      }
    ]
  });
  fs::write(&manifest_path, serde_json::to_string(&manifest).unwrap()).unwrap();

  let result = Command::new(env!("CARGO_BIN_EXE_perf_smoke"))
    .env(PAGESET_TIMEOUT_MANIFEST_ENV, manifest_path)
    .args([
      "--suite",
      "pageset-timeouts",
      "--fail-on-missing-fixtures",
      "--no-isolate",
      "--output",
      output.to_str().unwrap(),
    ])
    .output()
    .expect("run perf_smoke");

  assert!(
    !result.status.success(),
    "perf_smoke should fail when pageset-timeouts fixtures are missing"
  );
  let stderr = String::from_utf8_lossy(&result.stderr);
  assert!(
    stderr.contains("this_fixture_should_not_exist_12345"),
    "stderr should mention missing fixture name; got: {stderr}"
  );
}

#[test]
fn perf_smoke_fail_on_budget_exits_non_zero() {
  let temp = tempdir().expect("create temp dir");
  let output = temp.path().join("perf-smoke.json");
  let manifest_path = temp.path().join("manifest.json");
  let manifest = serde_json::json!({
    "schema_version": 1,
    "fixtures": [
      {
        "name": "flex_dashboard",
        "viewport": [1040, 1240],
        "dpr": 1.0,
        "media": "screen",
        "budget_ms": 0.0
      }
    ]
  });
  fs::write(&manifest_path, serde_json::to_string(&manifest).unwrap()).unwrap();

  let result = Command::new(env!("CARGO_BIN_EXE_perf_smoke"))
    .env(PAGESET_TIMEOUT_MANIFEST_ENV, manifest_path)
    .args([
      "--suite",
      "pageset-timeouts",
      "--fail-on-budget",
      "--no-isolate",
      "--output",
      output.to_str().unwrap(),
    ])
    .output()
    .expect("run perf_smoke");

  assert!(
    !result.status.success(),
    "perf_smoke should fail when a fixture exceeds its budget"
  );
  let stderr = String::from_utf8_lossy(&result.stderr);
  assert!(
    stderr.contains("Budget failures"),
    "stderr should include budget failure report; got: {stderr}"
  );
  assert!(
    stderr.contains("flex_dashboard"),
    "stderr should mention the budget-failing fixture; got: {stderr}"
  );
}
