//! Guardrail: committed pageset progress artifacts should use the clarified `*_cpu_ms` key names.
//!
//! Progress files under `progress/pages/*.json` are meant to be stable, human-auditable artifacts.
//! They should not reintroduce legacy CPU-sum key names (e.g. `taffy_flex_compute_ms`) which can be
//! misread as wall-clock timings.

use serde_json::Value;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

const LEGACY_KEYS: [&str; 5] = [
  "text_fallback_ms",
  "text_shape_ms",
  "text_rasterize_ms",
  "taffy_flex_compute_ms",
  "taffy_grid_compute_ms",
];

#[test]
fn progress_pages_do_not_contain_legacy_cpu_sum_keys() {
  let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let progress_dir = repo_root.join("progress").join("pages");
  assert!(
    progress_dir.exists(),
    "expected progress pages directory at {}",
    progress_dir.display()
  );

  let mut offenders = Vec::new();
  for entry in fs::read_dir(&progress_dir).expect("read progress/pages") {
    let entry = entry.expect("read progress entry");
    let path = entry.path();
    if path.extension().and_then(|ext| ext.to_str()) != Some("json") {
      continue;
    }

    let raw = fs::read_to_string(&path)
      .unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()));
    let json: Value = serde_json::from_str(&raw)
      .unwrap_or_else(|err| panic!("failed to parse {} as JSON: {err}", path.display()));
    let mut found = BTreeSet::new();
    collect_legacy_keys(&json, &mut found);
    if !found.is_empty() {
      offenders.push(format!(
        "{}: contains legacy keys: {}",
        path.strip_prefix(&repo_root)
          .unwrap_or(&path)
          .display(),
        found.into_iter().collect::<Vec<_>>().join(", ")
      ));
    }
  }

  if !offenders.is_empty() {
    panic!(
      "found legacy CPU-sum keys in committed progress artifacts:\n{}",
      offenders.join("\n")
    );
  }
}

fn collect_legacy_keys(value: &Value, found: &mut BTreeSet<String>) {
  match value {
    Value::Object(map) => {
      for (key, value) in map {
        if LEGACY_KEYS.contains(&key.as_str()) {
          found.insert(key.clone());
        }
        collect_legacy_keys(value, found);
      }
    }
    Value::Array(items) => {
      for item in items {
        collect_legacy_keys(item, found);
      }
    }
    Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => {}
  }
}

