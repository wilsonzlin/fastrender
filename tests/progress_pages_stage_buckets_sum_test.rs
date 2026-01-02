//! Guardrail: committed pageset progress artifacts should have sane stage buckets.
//!
//! `progress/pages/*.json` is the committed "scoreboard" used for pageset triage. The coarse
//! `stages_ms` buckets are intended to represent wall-time attribution and should therefore:
//! - be non-negative,
//! - sum (within rounding error) to `total_ms`, and
//! - never exceed `total_ms` per bucket (no double-counting).

use serde_json::Value;
use std::fs;
use std::path::{Path, PathBuf};

#[test]
fn progress_pages_stage_buckets_sum_to_total_ms() {
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

    let raw =
      fs::read_to_string(&path).unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()));
    let json: Value = serde_json::from_str(&raw)
      .unwrap_or_else(|err| panic!("failed to parse {} as JSON: {err}", path.display()));

    if json.get("total_ms").is_none() {
      offenders.push(format!(
        "{}: missing total_ms",
        display_path(&repo_root, &path)
      ));
      continue;
    }

    let Some(total_ms) = json.get("total_ms").and_then(|v| v.as_f64()) else {
      // `null` totals are allowed for placeholders / missing cache.
      continue;
    };
    if !total_ms.is_finite() || total_ms < 0.0 {
      offenders.push(format!(
        "{}: invalid total_ms {total_ms:?}",
        display_path(&repo_root, &path)
      ));
      continue;
    }

    let Some(stages) = json.get("stages_ms").and_then(|v| v.as_object()) else {
      offenders.push(format!(
        "{}: missing or invalid stages_ms",
        display_path(&repo_root, &path)
      ));
      continue;
    };

    let mut sum = 0.0f64;
    for stage in ["fetch", "css", "cascade", "layout", "paint"] {
      let Some(ms) = stages.get(stage).and_then(|v| v.as_f64()) else {
        offenders.push(format!(
          "{}: missing stages_ms.{stage}",
          display_path(&repo_root, &path)
        ));
        continue;
      };
      if !ms.is_finite() || ms < 0.0 {
        offenders.push(format!(
          "{}: invalid stages_ms.{stage} {ms:?}",
          display_path(&repo_root, &path)
        ));
        continue;
      }
      if ms > total_ms + 1e-6 {
        offenders.push(format!(
          "{}: stages_ms.{stage}={ms:.3} exceeds total_ms={total_ms:.3}",
          display_path(&repo_root, &path)
        ));
      }
      sum += ms;
    }
    // `box_tree` was introduced later; allow it to be missing for legacy progress artifacts so we
    // don't require a repo-wide migrate in every PR.
    if let Some(ms) = stages.get("box_tree") {
      let Some(ms) = ms.as_f64() else {
        offenders.push(format!(
          "{}: invalid stages_ms.box_tree {:?}",
          display_path(&repo_root, &path),
          ms
        ));
        continue;
      };
      if !ms.is_finite() || ms < 0.0 {
        offenders.push(format!(
          "{}: invalid stages_ms.box_tree {ms:?}",
          display_path(&repo_root, &path)
        ));
        continue;
      }
      if ms > total_ms + 1e-6 {
        offenders.push(format!(
          "{}: stages_ms.box_tree={ms:.3} exceeds total_ms={total_ms:.3}",
          display_path(&repo_root, &path)
        ));
      }
      sum += ms;
    }

    if sum <= 0.0 {
      offenders.push(format!(
        "{}: stages_ms sum is 0 with total_ms={total_ms:.3}",
        display_path(&repo_root, &path)
      ));
      continue;
    }
    if (sum - total_ms).abs() >= 1.0 {
      offenders.push(format!(
        "{}: stages_ms sum ({sum:.3}) does not match total_ms ({total_ms:.3})",
        display_path(&repo_root, &path)
      ));
    }
  }

  if !offenders.is_empty() {
    panic!(
      "found progress artifacts with invalid stage buckets:\n{}",
      offenders.join("\n")
    );
  }
}

fn display_path(repo_root: &Path, path: &Path) -> String {
  path
    .strip_prefix(repo_root)
    .unwrap_or(path)
    .display()
    .to_string()
}
