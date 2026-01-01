use anyhow::{bail, Context, Result};
use clap::Args;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use tempfile::TempDir;

const FALLBACK_DEFAULT_BUDGET_MS: u64 = 5000;
const DEFAULT_BUDGET_MULTIPLIER: f64 = 1.30;
const DEFAULT_MAX_BUDGET_MS: u64 = 60_000;
const DEFAULT_ROUND_TO_MS: u64 = 100;
const DEFAULT_MAX_BUDGET_INCREASE_PERCENT: f64 = 0.10;
const PAGESET_GUARDRAILS_MANIFEST_PATH: &str = "tests/pages/pageset_guardrails.json";
const LEGACY_TIMEOUT_MANIFEST_PATH: &str = "tests/pages/pageset_timeouts.json";

#[derive(Args, Debug)]
pub struct UpdatePagesetTimeoutBudgetsArgs {
  /// Path to the pageset-timeouts manifest to rewrite
  #[arg(long, default_value = LEGACY_TIMEOUT_MANIFEST_PATH)]
  pub manifest: PathBuf,

  /// Multiply observed `total_ms` by this factor when setting budgets
  #[arg(long, default_value_t = DEFAULT_BUDGET_MULTIPLIER)]
  pub multiplier: f64,

  /// Minimum budget to enforce for all fixtures (defaults to `manifest.default_budget_ms`)
  #[arg(long, value_name = "MS")]
  pub min_budget_ms: Option<u64>,

  /// Hard cap to prevent runaway budgets
  #[arg(long, value_name = "MS", default_value_t = DEFAULT_MAX_BUDGET_MS)]
  pub max_budget_ms: u64,

  /// Round budgets up to this increment (defaults to 100ms)
  #[arg(long, value_name = "MS", default_value_t = DEFAULT_ROUND_TO_MS)]
  pub round_to_ms: u64,

  /// Reject budget increases larger than this fraction (e.g. 0.1 = 10%) unless `--allow-budget-increase` is set
  #[arg(long, default_value_t = DEFAULT_MAX_BUDGET_INCREASE_PERCENT)]
  pub max_budget_increase_percent: f64,

  /// Allow budgets to increase beyond `--max-budget-increase-percent`
  #[arg(long)]
  pub allow_budget_increase: bool,

  /// Run `perf_smoke` in isolation (one process per fixture) instead of `--no-isolate`
  #[arg(long)]
  pub isolate: bool,

  /// Print updated JSON to stdout without writing files
  #[arg(long, conflicts_with = "write")]
  pub dry_run: bool,

  /// Write updated budgets back to the manifest
  #[arg(long, conflicts_with = "dry_run")]
  pub write: bool,
}

#[derive(Debug, Deserialize, Serialize)]
struct PagesetTimeoutManifest {
  schema_version: u32,
  #[serde(default)]
  default_budget_ms: Option<f64>,
  fixtures: Vec<PagesetTimeoutFixture>,
  #[serde(flatten)]
  extra: BTreeMap<String, serde_json::Value>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct PagesetTimeoutFixture {
  name: String,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  url: Option<String>,
  viewport: [u32; 2],
  dpr: f32,
  media: String,
  #[serde(default)]
  budget_ms: Option<f64>,
  #[serde(flatten)]
  extra: BTreeMap<String, serde_json::Value>,
}

#[derive(Debug, Deserialize)]
struct PerfSmokeSummary {
  #[allow(dead_code)]
  schema_version: u32,
  fixtures: Vec<PerfSmokeFixture>,
}

#[derive(Debug, Deserialize)]
struct PerfSmokeFixture {
  name: String,
  total_ms: f64,
}

#[derive(Debug, Clone, Copy)]
struct BudgetPolicy {
  multiplier: f64,
  min_budget_ms: u64,
  max_budget_ms: u64,
  round_to_ms: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BudgetComputation {
  budget_ms: u64,
  capped: bool,
}

#[derive(Debug, Default)]
struct BudgetUpdateSummary {
  tightened: usize,
  loosened: usize,
  unchanged: usize,
  capped: usize,
  missing: Vec<String>,
}

pub fn run_update_pageset_timeout_budgets(args: UpdatePagesetTimeoutBudgetsArgs) -> Result<()> {
  if !args.dry_run && !args.write {
    bail!("pass exactly one of --dry-run or --write");
  }
  if args.multiplier <= 0.0 || !args.multiplier.is_finite() {
    bail!("--multiplier must be a positive, finite number");
  }
  if args.max_budget_increase_percent < 0.0 || !args.max_budget_increase_percent.is_finite() {
    bail!("--max-budget-increase-percent must be a non-negative, finite number");
  }
  if args.max_budget_ms == 0 {
    bail!("--max-budget-ms must be > 0");
  }
  if args.round_to_ms == 0 {
    bail!("--round-to-ms must be > 0");
  }

  let mut manifest = load_manifest(&args.manifest)?;
  let default_min_budget_ms = manifest
    .default_budget_ms
    .unwrap_or(FALLBACK_DEFAULT_BUDGET_MS as f64)
    .ceil() as u64;
  let min_budget_ms = args.min_budget_ms.unwrap_or(default_min_budget_ms);
  if min_budget_ms == 0 {
    bail!("min budget must be > 0");
  }
  if min_budget_ms > args.max_budget_ms {
    bail!(
      "--min-budget-ms ({min_budget_ms}) must be <= --max-budget-ms ({})",
      args.max_budget_ms
    );
  }

  let tempdir = tempfile::tempdir().context("create temp directory for perf_smoke output")?;
  let perf_path = tempdir.path().join("perf_smoke.json");

  run_perf_smoke(&args, &manifest, &perf_path, &tempdir)?;
  let perf = read_perf_smoke_summary(&perf_path)?;
  let timings = timings_map(&perf)?;

  let policy = BudgetPolicy {
    multiplier: args.multiplier,
    min_budget_ms,
    max_budget_ms: args.max_budget_ms,
    round_to_ms: args.round_to_ms,
  };

  let summary = update_manifest_budgets(
    &mut manifest,
    &timings,
    default_min_budget_ms,
    args.max_budget_increase_percent,
    args.allow_budget_increase,
    policy,
  )?;

  let json = serde_json::to_string_pretty(&manifest).context("serialize updated manifest")?;

  if args.dry_run {
    print!("{json}\n");
  } else {
    fs::write(&args.manifest, format!("{json}\n")).with_context(|| {
      format!(
        "failed to write updated pageset timeout manifest {}",
        args.manifest.display()
      )
    })?;
    sync_guardrails_manifests(&args.manifest, &json)?;
  }

  eprintln!(
    "✓ Updated pageset timeout budgets (tightened {}, loosened {}, unchanged {}, capped {})",
    summary.tightened, summary.loosened, summary.unchanged, summary.capped
  );
  if !summary.missing.is_empty() {
    eprintln!(
      "⚠ Missing perf_smoke timings for {} fixture(s): {}",
      summary.missing.len(),
      summary.missing.join(", ")
    );
  }

  Ok(())
}

fn load_manifest(path: &Path) -> Result<PagesetTimeoutManifest> {
  let raw = fs::read_to_string(path)
    .with_context(|| format!("failed to read pageset timeout manifest {}", path.display()))?;
  serde_json::from_str(&raw).with_context(|| format!("invalid JSON in {}", path.display()))
}

fn run_perf_smoke(
  args: &UpdatePagesetTimeoutBudgetsArgs,
  _manifest: &PagesetTimeoutManifest,
  output: &Path,
  _tempdir: &TempDir,
) -> Result<()> {
  // Keep renders deterministic across machines.
  let mut cmd = Command::new("cargo");
  cmd.env("FASTR_USE_BUNDLED_FONTS", "1");
  // Ensure we always exercise the same manifest, even if the caller has env vars set.
  cmd.env("FASTR_PERF_SMOKE_PAGESET_GUARDRAILS_MANIFEST", &args.manifest);
  cmd.env("FASTR_PERF_SMOKE_PAGESET_TIMEOUT_MANIFEST", &args.manifest);
  cmd
    .arg("run")
    .arg("--release")
    .args(["--bin", "perf_smoke", "--"])
    .args(["--suite", "pageset-timeouts"])
    .arg("--output")
    .arg(output);

  if !args.isolate {
    cmd.arg("--no-isolate");
  }

  // Ensure `--dry-run` prints deterministic JSON to stdout by discarding perf_smoke's stdout (it
  // always prints its report).
  cmd.stdout(Stdio::null());
  cmd.stderr(Stdio::inherit());
  cmd.current_dir(crate::repo_root());

  let status = cmd
    .status()
    .context("failed to invoke perf_smoke via cargo run")?;
  if !status.success() {
    bail!("perf_smoke failed with status {status}");
  }
  Ok(())
}

fn sync_guardrails_manifests(manifest_path: &Path, json: &str) -> Result<()> {
  let manifest_path = normalize_manifest_path(manifest_path);
  let guardrails_path = normalize_manifest_path(Path::new(PAGESET_GUARDRAILS_MANIFEST_PATH));
  let legacy_path = normalize_manifest_path(Path::new(LEGACY_TIMEOUT_MANIFEST_PATH));

  if manifest_path == guardrails_path {
    fs::write(&legacy_path, format!("{json}\n")).with_context(|| {
      format!(
        "failed to write legacy pageset_timeouts manifest copy to {}",
        legacy_path.display()
      )
    })?;
  } else if manifest_path == legacy_path {
    fs::write(&guardrails_path, format!("{json}\n")).with_context(|| {
      format!(
        "failed to write pageset_guardrails manifest copy to {}",
        guardrails_path.display()
      )
    })?;
  }

  Ok(())
}

fn normalize_manifest_path(path: &Path) -> PathBuf {
  let resolved = if path.is_absolute() {
    path.to_path_buf()
  } else {
    crate::repo_root().join(path)
  };
  resolved.canonicalize().unwrap_or(resolved)
}

fn read_perf_smoke_summary(path: &Path) -> Result<PerfSmokeSummary> {
  let raw = fs::read_to_string(path)
    .with_context(|| format!("failed to read perf_smoke report {}", path.display()))?;
  serde_json::from_str(&raw)
    .with_context(|| format!("invalid perf_smoke JSON in {}", path.display()))
}

fn timings_map(perf: &PerfSmokeSummary) -> Result<BTreeMap<String, f64>> {
  let mut map = BTreeMap::new();
  for fixture in &perf.fixtures {
    if map.insert(fixture.name.clone(), fixture.total_ms).is_some() {
      bail!(
        "perf_smoke report contains duplicate fixture {}",
        fixture.name
      );
    }
  }
  Ok(map)
}

fn update_manifest_budgets(
  manifest: &mut PagesetTimeoutManifest,
  timings_ms: &BTreeMap<String, f64>,
  default_budget_ms: u64,
  max_increase_percent: f64,
  allow_increase: bool,
  policy: BudgetPolicy,
) -> Result<BudgetUpdateSummary> {
  let default_budget_ms = manifest
    .default_budget_ms
    .map(|v| v.ceil() as u64)
    .unwrap_or(default_budget_ms);

  let mut summary = BudgetUpdateSummary::default();
  let mut violations = Vec::new();

  for fixture in &mut manifest.fixtures {
    let Some(total_ms) = timings_ms.get(&fixture.name).copied() else {
      summary.missing.push(fixture.name.clone());
      continue;
    };

    let old_budget_ms = fixture.budget_ms.unwrap_or(default_budget_ms as f64).ceil();
    if old_budget_ms <= 0.0 || !old_budget_ms.is_finite() {
      bail!(
        "fixture {} has invalid budget_ms {}",
        fixture.name,
        fixture
          .budget_ms
          .map(|v| v.to_string())
          .unwrap_or_else(|| "<unset>".to_string())
      );
    }

    let computation = compute_budget_ms(total_ms, policy)?;
    let new_budget_ms = computation.budget_ms as f64;
    let delta = new_budget_ms - old_budget_ms;
    if delta > 0.0 {
      let max_allowed = old_budget_ms * (1.0 + max_increase_percent);
      if !allow_increase && new_budget_ms > max_allowed + f64::EPSILON {
        violations.push(format!(
          "{}: {:.0}ms -> {:.0}ms (+{:.1}%) exceeds max increase {:.1}%",
          fixture.name,
          old_budget_ms,
          new_budget_ms,
          (delta / old_budget_ms) * 100.0,
          max_increase_percent * 100.0
        ));
      } else {
        summary.loosened += 1;
      }
    } else if delta < 0.0 {
      summary.tightened += 1;
    } else {
      summary.unchanged += 1;
    }

    if computation.capped {
      summary.capped += 1;
    }

    fixture.budget_ms = Some(new_budget_ms);
  }

  if !violations.is_empty() {
    bail!(
      "refusing to increase budgets beyond allowed threshold; re-run with --allow-budget-increase to accept:\n{}",
      violations.join("\n")
    );
  }

  Ok(summary)
}

fn compute_budget_ms(total_ms: f64, policy: BudgetPolicy) -> Result<BudgetComputation> {
  if total_ms < 0.0 || !total_ms.is_finite() {
    bail!("perf_smoke total_ms must be a non-negative, finite number (got {total_ms})");
  }

  let scaled = (total_ms * policy.multiplier).ceil();
  if scaled < 0.0 || !scaled.is_finite() {
    bail!(
      "computed scaled budget is invalid ({scaled}) for total_ms={total_ms} multiplier={}",
      policy.multiplier
    );
  }

  let scaled_ms = scaled as u64;
  let unclamped_ms = scaled_ms.max(policy.min_budget_ms);
  let rounded_ms = round_up_to_increment(unclamped_ms, policy.round_to_ms);
  let capped = rounded_ms > policy.max_budget_ms;
  Ok(BudgetComputation {
    budget_ms: if capped {
      policy.max_budget_ms
    } else {
      rounded_ms
    },
    capped,
  })
}

fn round_up_to_increment(value_ms: u64, increment_ms: u64) -> u64 {
  if increment_ms <= 1 {
    return value_ms;
  }

  let remainder = value_ms % increment_ms;
  if remainder == 0 {
    value_ms
  } else {
    value_ms + (increment_ms - remainder)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json::json;

  fn policy() -> BudgetPolicy {
    BudgetPolicy {
      multiplier: 1.30,
      min_budget_ms: 5000,
      max_budget_ms: 60_000,
      round_to_ms: 100,
    }
  }

  #[test]
  fn compute_budget_respects_min_and_rounding() {
    let computed = compute_budget_ms(100.0, policy()).expect("compute");
    assert_eq!(
      computed,
      BudgetComputation {
        budget_ms: 5000,
        capped: false
      }
    );
  }

  #[test]
  fn compute_budget_scales_and_rounds_up() {
    // 12345.6 * 1.3 = 16049.28 -> ceil 16050 -> round up to 16100.
    let computed = compute_budget_ms(12345.6, policy()).expect("compute");
    assert_eq!(
      computed,
      BudgetComputation {
        budget_ms: 16_100,
        capped: false
      }
    );
  }

  #[test]
  fn compute_budget_caps() {
    let computed = compute_budget_ms(100_000.0, policy()).expect("compute");
    assert_eq!(
      computed,
      BudgetComputation {
        budget_ms: 60_000,
        capped: true
      }
    );
  }

  #[test]
  fn update_manifest_preserves_fixture_settings() {
    let mut manifest: PagesetTimeoutManifest = serde_json::from_value(json!({
      "schema_version": 1,
      "default_budget_ms": 5000.0,
      "fixtures": [
        { "name": "example", "viewport": [800, 600], "dpr": 2.0, "media": "print", "budget_ms": 10000.0 }
      ]
    }))
    .unwrap();

    let mut timings = BTreeMap::new();
    timings.insert("example".to_string(), 5000.0);

    let summary =
      update_manifest_budgets(&mut manifest, &timings, 5000, 1.0, true, policy()).expect("update");
    assert_eq!(summary.tightened, 1);
    assert_eq!(manifest.fixtures.len(), 1);
    let fixture = &manifest.fixtures[0];
    assert_eq!(fixture.viewport, [800, 600]);
    assert_eq!(fixture.dpr, 2.0);
    assert_eq!(fixture.media, "print");
    assert_eq!(fixture.budget_ms, Some(6500.0));
  }

  #[test]
  fn rejects_large_budget_increase_without_override() {
    let mut manifest: PagesetTimeoutManifest = serde_json::from_value(json!({
      "schema_version": 1,
      "default_budget_ms": 5000.0,
      "fixtures": [
        { "name": "example", "viewport": [1200, 800], "dpr": 1.0, "media": "screen", "budget_ms": 10000.0 }
      ]
    }))
    .unwrap();

    let mut timings = BTreeMap::new();
    timings.insert("example".to_string(), 10_000.0);

    let err = update_manifest_budgets(&mut manifest, &timings, 5000, 0.10, false, policy())
      .unwrap_err()
      .to_string();
    assert!(err.contains("refusing to increase budgets"), "got: {err}");
    assert!(err.contains("example:"), "got: {err}");
  }
}
