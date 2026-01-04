use anyhow::{bail, Context, Result};
use clap::Args;
use serde::Deserialize;
use serde_json::Value;
use std::fs;
use std::path::{Component, Path, PathBuf};
use std::process::Command;

const DEFAULT_REPORT_PATH: &str = "target/fixture_chrome_diff/report.json";
const DEFAULT_PROGRESS_DIR: &str = "progress/pages";

#[derive(Args, Debug)]
pub struct SyncProgressAccuracyArgs {
  /// Path to a `diff_renders` JSON report (e.g. `target/fixture_chrome_diff/report.json`).
  #[arg(long, value_name = "PATH", default_value = DEFAULT_REPORT_PATH)]
  pub report: PathBuf,

  /// Directory containing committed `progress/pages/*.json`.
  #[arg(long, value_name = "DIR", default_value = DEFAULT_PROGRESS_DIR)]
  pub progress_dir: PathBuf,

  /// Print planned updates without writing files.
  #[arg(long)]
  pub dry_run: bool,

  /// Fail if a report entry has metrics but no corresponding `<progress-dir>/<name>.json`.
  #[arg(long)]
  pub fail_on_missing_progress: bool,
}

#[derive(Debug, Deserialize)]
struct DiffReport {
  tolerance: u8,
  max_diff_percent: f64,
  results: Vec<DiffReportEntry>,
}

#[derive(Debug, Deserialize)]
struct DiffReportEntry {
  name: String,
  #[serde(default)]
  metrics: Option<MetricsSummary>,
}

#[derive(Debug, Deserialize)]
struct MetricsSummary {
  pixel_diff: u64,
  diff_percentage: f64,
  perceptual_distance: f64,
}

pub fn run_sync_progress_accuracy(mut args: SyncProgressAccuracyArgs) -> Result<()> {
  let repo_root = crate::repo_root();
  if !args.report.is_absolute() {
    args.report = repo_root.join(&args.report);
  }
  if !args.progress_dir.is_absolute() {
    args.progress_dir = repo_root.join(&args.progress_dir);
  }

  let report_raw = fs::read_to_string(&args.report)
    .with_context(|| format!("read diff_renders report {}", args.report.display()))?;
  let report: DiffReport =
    serde_json::from_str(&report_raw).context("parse diff_renders report JSON")?;

  let computed_at_commit = current_git_sha(&repo_root);

  let total_report_entries = report.results.len();
  let mut entries_with_metrics = 0usize;
  let mut updated_progress_files = 0usize;
  let mut skipped_missing_progress = 0usize;
  let mut skipped_no_metrics = 0usize;
  let mut missing_progress: Vec<String> = Vec::new();

  for entry in report.results {
    let Some(metrics) = entry.metrics else {
      skipped_no_metrics += 1;
      continue;
    };
    entries_with_metrics += 1;

    let progress_path = progress_path_for_name(&args.progress_dir, &entry.name)?;
    if !progress_path.is_file() {
      skipped_missing_progress += 1;
      if args.fail_on_missing_progress {
        missing_progress.push(entry.name);
      }
      continue;
    }

    let raw = fs::read_to_string(&progress_path)
      .with_context(|| format!("read {}", progress_path.display()))?;
    let mut progress_json: Value = serde_json::from_str(&raw)
      .with_context(|| format!("parse progress JSON {}", progress_path.display()))?;

    let accuracy = build_accuracy_value(
      &metrics,
      report.tolerance,
      report.max_diff_percent,
      computed_at_commit.as_deref(),
    )?;
    apply_accuracy(&mut progress_json, accuracy)
      .with_context(|| format!("update accuracy for {}", progress_path.display()))?;

    let formatted = format!(
      "{}\n",
      serde_json::to_string_pretty(&progress_json).context("serialize updated progress JSON")?
    );
    if formatted == raw {
      continue;
    }

    updated_progress_files += 1;
    if args.dry_run {
      println!(
        "Would update {} (diff_pixels={}, diff_percent={:.4}%, perceptual={:.4})",
        progress_path.display(),
        metrics.pixel_diff,
        round_accuracy_metric(metrics.diff_percentage, 4),
        round_accuracy_metric(metrics.perceptual_distance, 4)
      );
    } else {
      fs::write(&progress_path, formatted.as_bytes())
        .with_context(|| format!("write {}", progress_path.display()))?;
      println!(
        "Updated {} (diff_pixels={}, diff_percent={:.4}%, perceptual={:.4})",
        progress_path.display(),
        metrics.pixel_diff,
        round_accuracy_metric(metrics.diff_percentage, 4),
        round_accuracy_metric(metrics.perceptual_distance, 4)
      );
    }
  }

  println!();
  println!("Sync progress accuracy summary:");
  println!("  total report entries: {total_report_entries}");
  println!("  entries with metrics: {entries_with_metrics}");
  println!("  progress files updated: {updated_progress_files}");
  println!("  skipped (missing progress): {skipped_missing_progress}");
  println!("  skipped (no metrics): {skipped_no_metrics}");

  missing_progress.sort();
  if args.fail_on_missing_progress && !missing_progress.is_empty() {
    bail!(
      "diff report contains metrics for {} entr{} with no matching progress JSON:\n  {}",
      missing_progress.len(),
      if missing_progress.len() == 1 {
        "y"
      } else {
        "ies"
      },
      missing_progress.join("\n  ")
    );
  }

  Ok(())
}

fn current_git_sha(repo_root: &Path) -> Option<String> {
  if let Ok(output) = Command::new("git")
    .arg("rev-parse")
    .arg("HEAD")
    .current_dir(repo_root)
    .output()
  {
    if output.status.success() {
      let sha = String::from_utf8_lossy(&output.stdout).trim().to_string();
      if !sha.is_empty() {
        return Some(sha);
      }
    }
  }

  for key in ["GITHUB_SHA", "FASTR_GIT_SHA"] {
    if let Ok(val) = std::env::var(key) {
      let trimmed = val.trim();
      if !trimmed.is_empty() {
        return Some(trimmed.to_string());
      }
    }
  }

  None
}

fn round_accuracy_metric(value: f64, places: u32) -> f64 {
  if !value.is_finite() {
    return value;
  }
  let factor = 10f64.powi(places as i32);
  (value * factor).round() / factor
}

fn build_accuracy_value(
  metrics: &MetricsSummary,
  tolerance: u8,
  max_diff_percent: f64,
  computed_at_commit: Option<&str>,
) -> Result<Value> {
  let diff_percent = round_accuracy_metric(metrics.diff_percentage, 4);
  let perceptual = round_accuracy_metric(metrics.perceptual_distance, 4);

  let diff_percent_value =
    serde_json::Number::from_f64(diff_percent).context("diff_percentage is not JSON-compatible")?;
  let perceptual_value = serde_json::Number::from_f64(perceptual)
    .context("perceptual_distance is not JSON-compatible")?;
  let max_diff_percent_value = serde_json::Number::from_f64(max_diff_percent)
    .context("max_diff_percent is not JSON-compatible")?;

  let mut obj = serde_json::Map::new();
  obj.insert("baseline".to_string(), Value::String("chrome".to_string()));
  obj.insert(
    "diff_pixels".to_string(),
    Value::Number(serde_json::Number::from(metrics.pixel_diff)),
  );
  obj.insert(
    "diff_percent".to_string(),
    Value::Number(diff_percent_value),
  );
  obj.insert("perceptual".to_string(), Value::Number(perceptual_value));
  obj.insert(
    "tolerance".to_string(),
    Value::Number(serde_json::Number::from(tolerance as u64)),
  );
  obj.insert(
    "max_diff_percent".to_string(),
    Value::Number(max_diff_percent_value),
  );
  if let Some(sha) = computed_at_commit {
    let trimmed = sha.trim();
    if !trimmed.is_empty() {
      obj.insert(
        "computed_at_commit".to_string(),
        Value::String(trimmed.to_string()),
      );
    }
  }

  Ok(Value::Object(obj))
}

fn apply_accuracy(progress: &mut Value, accuracy: Value) -> Result<()> {
  let root = progress
    .as_object_mut()
    .context("progress JSON should be a top-level object")?;

  if root.contains_key("accuracy") {
    root.insert("accuracy".to_string(), accuracy);
    return Ok(());
  }

  // Preserve existing key order while inserting `accuracy` in the same location
  // `pageset_progress` serializes it: before `hotspot`.
  let current = std::mem::take(root);
  let mut rebuilt = serde_json::Map::new();
  let mut pending = Some(accuracy);

  for (key, value) in current {
    if key == "hotspot" {
      if let Some(accuracy) = pending.take() {
        rebuilt.insert("accuracy".to_string(), accuracy);
      }
    }
    rebuilt.insert(key, value);
  }

  if let Some(accuracy) = pending.take() {
    rebuilt.insert("accuracy".to_string(), accuracy);
  }

  *root = rebuilt;
  Ok(())
}

fn progress_path_for_name(progress_dir: &Path, name: &str) -> Result<PathBuf> {
  let rel = PathBuf::from(format!("{name}.json"));
  for component in rel.components() {
    match component {
      Component::Normal(_) => {}
      // Reject `..`, absolute paths, and other weirdness.
      Component::CurDir | Component::ParentDir | Component::RootDir | Component::Prefix(_) => {
        bail!("invalid report entry name for progress mapping: {name}");
      }
    }
  }
  Ok(progress_dir.join(rel))
}
