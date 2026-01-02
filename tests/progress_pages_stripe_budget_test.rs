//! Guardrail: Stripe must stay under the 5s pageset budget.
//!
//! We track pageset performance via committed JSON progress artifacts under `progress/pages/`.
//! This test ensures the `stripe.com` entry remains `status=ok` and comfortably under the hard
//! 5s render budget.

use serde_json::Value;
use std::fs;
use std::path::PathBuf;

const HARD_BUDGET_MS: f64 = 5000.0;

#[test]
fn stripe_progress_is_ok_under_budget() {
  let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let path = repo_root
    .join("progress")
    .join("pages")
    .join("stripe.com.json");
  let raw =
    fs::read_to_string(&path).unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()));
  let json: Value =
    serde_json::from_str(&raw).unwrap_or_else(|err| panic!("failed to parse {}: {err}", path.display()));

  let status = json
    .get("status")
    .and_then(|value| value.as_str())
    .unwrap_or_default();
  assert_eq!(
    status, "ok",
    "expected {} status=ok, got {:?}",
    path.display(),
    json.get("status")
  );

  let total_ms = json.get("total_ms").and_then(|value| value.as_f64());
  let Some(total_ms) = total_ms else {
    panic!(
      "expected {} to have numeric total_ms, got {:?}",
      path.display(),
      json.get("total_ms")
    );
  };
  assert!(
    total_ms.is_finite() && total_ms >= 0.0,
    "expected {} total_ms to be finite and non-negative, got {total_ms:?}",
    path.display()
  );
  assert!(
    total_ms < HARD_BUDGET_MS,
    "expected {} total_ms<{HARD_BUDGET_MS:.0}ms, got {total_ms:.2}ms",
    path.display()
  );

  let Some(stages) = json.get("stages_ms").and_then(|value| value.as_object()) else {
    panic!(
      "expected {} to have stages_ms object, got {:?}",
      path.display(),
      json.get("stages_ms")
    );
  };
  for stage in ["fetch", "css", "cascade", "layout", "paint"] {
    assert!(
      stages.get(stage).and_then(|value| value.as_f64()).is_some(),
      "expected {} stages_ms.{stage} to be numeric, got {:?}",
      path.display(),
      stages.get(stage)
    );
  }
}

