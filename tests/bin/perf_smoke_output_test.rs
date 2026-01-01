use std::fs;
use std::process::Command;

use serde_json::Value;
use tempfile::tempdir;

#[test]
fn perf_smoke_emits_stage_breakdowns() {
  let temp = tempdir().expect("create temp dir");
  let output = temp.path().join("perf-smoke.json");

  let status = Command::new(env!("CARGO_BIN_EXE_perf_smoke"))
    .args([
      "--output",
      output.to_str().unwrap(),
      "--fixtures",
      "flex_dashboard",
    ])
    .status()
    .expect("run perf_smoke");

  assert!(status.success(), "perf_smoke should exit successfully");

  let data = fs::read_to_string(&output).expect("read perf_smoke output");
  let summary: Value = serde_json::from_str(&data).expect("parse perf_smoke json");
  let fixtures = summary["fixtures"]
    .as_array()
    .expect("fixtures array must exist");
  assert_eq!(
    fixtures.len(),
    1,
    "fixture filter should limit to one entry"
  );
  let fixture = &fixtures[0];
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
}
