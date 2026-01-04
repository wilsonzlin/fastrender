mod common;

use clap::Parser;
use common::report::{ensure_parent_dir, escape_html, path_for_report};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

const SCHEMA_VERSION: u32 = 1;
const METRIC_EPS: f64 = 1e-9;
const TOP_N: usize = 20;

#[derive(Parser, Debug)]
#[command(
  name = "compare_diff_reports",
  about = "Compare two diff_renders JSON reports and summarize accuracy deltas"
)]
struct Args {
  /// Baseline diff_report.json
  #[arg(long, value_name = "PATH")]
  baseline: PathBuf,

  /// New diff_report.json
  #[arg(long = "new", value_name = "PATH")]
  new_report: PathBuf,

  /// Path to write diff_report_delta.json
  #[arg(long, default_value = "diff_report_delta.json")]
  json: PathBuf,

  /// Path to write diff_report_delta.html
  #[arg(long, default_value = "diff_report_delta.html")]
  html: PathBuf,

  /// Proceed even when the diff report comparison config differs.
  #[arg(long)]
  allow_config_mismatch: bool,

  /// Exit non-zero when any entry regresses (respecting `--regression-threshold-percent`).
  #[arg(long)]
  fail_on_regression: bool,

  /// Only treat an entry as a failing regression when diff_percentage increases by more than this amount.
  #[arg(long, default_value_t = 0.0, value_name = "PERCENT")]
  regression_threshold_percent: f64,
}

#[derive(Deserialize, Clone)]
struct DiffReport {
  before_dir: String,
  after_dir: String,
  tolerance: u8,
  max_diff_percent: f64,
  max_perceptual_distance: Option<f64>,
  ignore_alpha: bool,
  results: Vec<DiffReportEntry>,
}

#[derive(Deserialize, Clone)]
struct DiffReportEntry {
  name: String,
  status: EntryStatus,
  before: Option<String>,
  after: Option<String>,
  diff: Option<String>,
  metrics: Option<MetricsSummary>,
  error: Option<String>,
}

#[derive(Deserialize, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
enum EntryStatus {
  Match,
  WithinThreshold,
  Diff,
  MissingBefore,
  MissingAfter,
  Error,
}

impl EntryStatus {
  fn kind_weight(&self) -> u8 {
    match self {
      EntryStatus::Match | EntryStatus::WithinThreshold | EntryStatus::Diff => 0,
      EntryStatus::MissingBefore | EntryStatus::MissingAfter => 1,
      EntryStatus::Error => 2,
    }
  }

  fn label(&self) -> &'static str {
    match self {
      EntryStatus::Match => "match",
      EntryStatus::WithinThreshold => "within-threshold",
      EntryStatus::Diff => "diff",
      EntryStatus::MissingBefore => "missing-before",
      EntryStatus::MissingAfter => "missing-after",
      EntryStatus::Error => "error",
    }
  }
}

#[derive(Deserialize, Clone, Copy, Serialize)]
struct MetricsSummary {
  diff_percentage: f64,
  perceptual_distance: f64,
  #[serde(default)]
  pixel_diff: u64,
  #[serde(default)]
  total_pixels: u64,
}

#[derive(Serialize)]
struct DeltaReport {
  schema_version: u32,
  baseline: ReportMeta,
  new: ReportMeta,
  config_mismatches: Vec<ConfigMismatch>,
  totals: DeltaTotals,
  top_improvements: Vec<DeltaRankedEntry>,
  top_regressions: Vec<DeltaRankedEntry>,
  results: Vec<DeltaEntry>,
}

#[derive(Serialize)]
struct ReportMeta {
  before_dir: String,
  after_dir: String,
  tolerance: u8,
  max_diff_percent: f64,
  max_perceptual_distance: Option<f64>,
  ignore_alpha: bool,
}

#[derive(Serialize)]
struct ConfigMismatch {
  field: &'static str,
  baseline: String,
  new: String,
}

#[derive(Serialize, Default)]
struct DeltaTotals {
  entries: usize,
  paired: usize,
  improved: usize,
  regressed: usize,
  unchanged: usize,
  missing_in_baseline: usize,
  missing_in_new: usize,
  baseline_errors: usize,
  new_errors: usize,
  baseline_missing: usize,
  new_missing: usize,
}

#[derive(Serialize)]
struct DeltaEntry {
  name: String,
  baseline: Option<EntrySummary>,
  new: Option<EntrySummary>,
  #[serde(skip_serializing_if = "Option::is_none")]
  diff_percentage_delta: Option<f64>,
  #[serde(skip_serializing_if = "Option::is_none")]
  perceptual_distance_delta: Option<f64>,
  classification: DeltaClassification,
}

#[derive(Serialize, Clone)]
struct EntrySummary {
  status: EntryStatus,
  #[serde(skip_serializing_if = "Option::is_none")]
  before: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  after: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  diff: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  metrics: Option<MetricsSummary>,
  #[serde(skip_serializing_if = "Option::is_none")]
  error: Option<String>,
}

#[derive(Serialize, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
enum DeltaClassification {
  Improved,
  Regressed,
  Unchanged,
  MissingInBaseline,
  MissingInNew,
}

impl DeltaClassification {
  fn label(&self) -> &'static str {
    match self {
      DeltaClassification::Improved => "improved",
      DeltaClassification::Regressed => "regressed",
      DeltaClassification::Unchanged => "unchanged",
      DeltaClassification::MissingInBaseline => "missing-in-baseline",
      DeltaClassification::MissingInNew => "missing-in-new",
    }
  }

  fn row_class(&self) -> &'static str {
    match self {
      DeltaClassification::Improved => "improved",
      DeltaClassification::Regressed => "regressed",
      DeltaClassification::Unchanged => "unchanged",
      DeltaClassification::MissingInBaseline | DeltaClassification::MissingInNew => "missing",
    }
  }
}

#[derive(Serialize)]
struct DeltaRankedEntry {
  name: String,
  diff_percentage_delta: f64,
  perceptual_distance_delta: f64,
}

fn main() {
  match run() {
    Ok(exit_code) => std::process::exit(exit_code),
    Err(err) => {
      eprintln!("error: {err}");
      std::process::exit(1);
    }
  }
}

fn run() -> Result<i32, String> {
  let args = Args::parse();
  validate_args(&args)?;

  let baseline_path =
    fs::canonicalize(&args.baseline).map_err(|e| format!("{}: {e}", args.baseline.display()))?;
  let new_path = fs::canonicalize(&args.new_report)
    .map_err(|e| format!("{}: {e}", args.new_report.display()))?;

  let baseline_report = read_report(&baseline_path)?;
  let new_report = read_report(&new_path)?;

  let baseline_meta = ReportMeta::from_report(&baseline_report);
  let new_meta = ReportMeta::from_report(&new_report);

  let config_mismatches = diff_config(&baseline_report, &new_report);
  if !config_mismatches.is_empty() {
    eprintln!("warning: diff report config mismatch (pass --allow-config-mismatch to proceed):");
    for mismatch in &config_mismatches {
      eprintln!(
        "  - {}: baseline={} new={}",
        mismatch.field, mismatch.baseline, mismatch.new
      );
    }
    if !args.allow_config_mismatch {
      let totals = compute_totals_without_deltas(&baseline_report, &new_report);
      let report = DeltaReport {
        schema_version: SCHEMA_VERSION,
        baseline: baseline_meta,
        new: new_meta,
        config_mismatches,
        totals,
        top_improvements: Vec::new(),
        top_regressions: Vec::new(),
        results: Vec::new(),
      };

      write_json_report(&report, &args.json)?;
      write_html_report(&report, &baseline_path, &new_path, &args.html)?;
      return Ok(1);
    }
  }

  let baseline_by_name = index_entries(baseline_report.results);
  let new_by_name = index_entries(new_report.results);

  let mut names = BTreeSet::new();
  names.extend(baseline_by_name.keys().cloned());
  names.extend(new_by_name.keys().cloned());

  let mut totals = DeltaTotals::default();
  totals.entries = names.len();

  let mut results = Vec::with_capacity(names.len());

  for name in names {
    let baseline_entry = baseline_by_name.get(&name);
    let new_entry = new_by_name.get(&name);

    let baseline_summary = baseline_entry.map(to_entry_summary);
    let new_summary = new_entry.map(to_entry_summary);

    let (diff_percentage_delta, perceptual_distance_delta, classification) =
      classify_delta(baseline_entry, new_entry);

    if baseline_entry.is_some() && new_entry.is_some() {
      totals.paired += 1;
    } else if baseline_entry.is_none() {
      totals.missing_in_baseline += 1;
    } else {
      totals.missing_in_new += 1;
    }

    if let Some(entry) = baseline_entry {
      match entry.status {
        EntryStatus::Error => totals.baseline_errors += 1,
        EntryStatus::MissingBefore | EntryStatus::MissingAfter => totals.baseline_missing += 1,
        _ => {}
      }
    }
    if let Some(entry) = new_entry {
      match entry.status {
        EntryStatus::Error => totals.new_errors += 1,
        EntryStatus::MissingBefore | EntryStatus::MissingAfter => totals.new_missing += 1,
        _ => {}
      }
    }

    match classification {
      DeltaClassification::Improved => totals.improved += 1,
      DeltaClassification::Regressed => totals.regressed += 1,
      DeltaClassification::Unchanged => totals.unchanged += 1,
      DeltaClassification::MissingInBaseline | DeltaClassification::MissingInNew => {}
    }

    results.push(DeltaEntry {
      name,
      baseline: baseline_summary,
      new: new_summary,
      diff_percentage_delta,
      perceptual_distance_delta,
      classification,
    });
  }

  let mut top_improvements = collect_top_metric_deltas(&results, true);
  let mut top_regressions = collect_top_metric_deltas(&results, false);
  top_improvements.truncate(TOP_N);
  top_regressions.truncate(TOP_N);

  let report = DeltaReport {
    schema_version: SCHEMA_VERSION,
    baseline: baseline_meta,
    new: new_meta,
    config_mismatches,
    totals,
    top_improvements,
    top_regressions,
    results,
  };

  write_json_report(&report, &args.json)?;
  write_html_report(&report, &baseline_path, &new_path, &args.html)?;

  if args.fail_on_regression {
    let threshold = args.regression_threshold_percent;
    let failing = report
      .results
      .iter()
      .filter(|entry| is_failing_regression(entry, threshold))
      .count();
    if failing > 0 {
      eprintln!("{failing} regressions over threshold");
      return Ok(1);
    }
  }

  Ok(0)
}

fn validate_args(args: &Args) -> Result<(), String> {
  if !args.regression_threshold_percent.is_finite() || args.regression_threshold_percent < 0.0 {
    return Err("--regression-threshold-percent must be a finite, non-negative number".to_string());
  }
  Ok(())
}

fn compute_totals_without_deltas(baseline: &DiffReport, new_report: &DiffReport) -> DeltaTotals {
  let baseline_names: BTreeSet<String> = baseline.results.iter().map(|e| e.name.clone()).collect();
  let new_names: BTreeSet<String> = new_report.results.iter().map(|e| e.name.clone()).collect();

  let entries = baseline_names.union(&new_names).count();
  let paired = baseline_names.intersection(&new_names).count();
  let missing_in_baseline = new_names.difference(&baseline_names).count();
  let missing_in_new = baseline_names.difference(&new_names).count();

  let baseline_errors = baseline
    .results
    .iter()
    .filter(|entry| matches!(entry.status, EntryStatus::Error))
    .count();
  let new_errors = new_report
    .results
    .iter()
    .filter(|entry| matches!(entry.status, EntryStatus::Error))
    .count();

  let baseline_missing = baseline
    .results
    .iter()
    .filter(|entry| {
      matches!(
        entry.status,
        EntryStatus::MissingBefore | EntryStatus::MissingAfter
      )
    })
    .count();
  let new_missing = new_report
    .results
    .iter()
    .filter(|entry| {
      matches!(
        entry.status,
        EntryStatus::MissingBefore | EntryStatus::MissingAfter
      )
    })
    .count();

  DeltaTotals {
    entries,
    paired,
    missing_in_baseline,
    missing_in_new,
    baseline_errors,
    new_errors,
    baseline_missing,
    new_missing,
    ..DeltaTotals::default()
  }
}

fn read_report(path: &Path) -> Result<DiffReport, String> {
  let raw = fs::read_to_string(path).map_err(|e| format!("{}: {e}", path.display()))?;
  serde_json::from_str(&raw).map_err(|e| format!("failed to parse {}: {e}", path.display()))
}

impl ReportMeta {
  fn from_report(report: &DiffReport) -> Self {
    Self {
      before_dir: report.before_dir.clone(),
      after_dir: report.after_dir.clone(),
      tolerance: report.tolerance,
      max_diff_percent: report.max_diff_percent,
      max_perceptual_distance: report.max_perceptual_distance,
      ignore_alpha: report.ignore_alpha,
    }
  }
}

fn diff_config(baseline: &DiffReport, new_report: &DiffReport) -> Vec<ConfigMismatch> {
  let mut mismatches = Vec::new();
  if baseline.tolerance != new_report.tolerance {
    mismatches.push(ConfigMismatch {
      field: "tolerance",
      baseline: baseline.tolerance.to_string(),
      new: new_report.tolerance.to_string(),
    });
  }
  if !float_eq(baseline.max_diff_percent, new_report.max_diff_percent) {
    mismatches.push(ConfigMismatch {
      field: "max_diff_percent",
      baseline: format!("{:.8}", baseline.max_diff_percent),
      new: format!("{:.8}", new_report.max_diff_percent),
    });
  }
  if baseline.ignore_alpha != new_report.ignore_alpha {
    mismatches.push(ConfigMismatch {
      field: "ignore_alpha",
      baseline: baseline.ignore_alpha.to_string(),
      new: new_report.ignore_alpha.to_string(),
    });
  }
  match (
    baseline.max_perceptual_distance,
    new_report.max_perceptual_distance,
  ) {
    (None, None) => {}
    (Some(a), Some(b)) => {
      if !float_eq(a, b) {
        mismatches.push(ConfigMismatch {
          field: "max_perceptual_distance",
          baseline: format!("{a:.8}"),
          new: format!("{b:.8}"),
        });
      }
    }
    (a, b) => mismatches.push(ConfigMismatch {
      field: "max_perceptual_distance",
      baseline: a
        .map(|v| format!("{v:.8}"))
        .unwrap_or_else(|| "-".to_string()),
      new: b
        .map(|v| format!("{v:.8}"))
        .unwrap_or_else(|| "-".to_string()),
    }),
  }
  mismatches
}

fn index_entries(entries: Vec<DiffReportEntry>) -> BTreeMap<String, DiffReportEntry> {
  let mut map = BTreeMap::new();
  for entry in entries {
    map.insert(entry.name.clone(), entry);
  }
  map
}

fn to_entry_summary(entry: &DiffReportEntry) -> EntrySummary {
  EntrySummary {
    status: entry.status,
    before: entry.before.clone(),
    after: entry.after.clone(),
    diff: entry.diff.clone(),
    metrics: entry.metrics,
    error: entry.error.clone(),
  }
}

fn classify_delta(
  baseline: Option<&DiffReportEntry>,
  new_entry: Option<&DiffReportEntry>,
) -> (Option<f64>, Option<f64>, DeltaClassification) {
  match (baseline, new_entry) {
    (None, None) => (None, None, DeltaClassification::Unchanged),
    (None, Some(_)) => (None, None, DeltaClassification::MissingInBaseline),
    (Some(_), None) => (None, None, DeltaClassification::MissingInNew),
    (Some(b), Some(n)) => {
      let baseline_metrics = b.metrics;
      let new_metrics = n.metrics;
      if let (Some(bm), Some(nm)) = (baseline_metrics, new_metrics) {
        let diff_delta = nm.diff_percentage - bm.diff_percentage;
        let perceptual_delta = nm.perceptual_distance - bm.perceptual_distance;
        (
          Some(diff_delta),
          Some(perceptual_delta),
          classify_metrics(diff_delta, perceptual_delta),
        )
      } else if baseline_metrics.is_some() && new_metrics.is_none() {
        (None, None, DeltaClassification::Regressed)
      } else if baseline_metrics.is_none() && new_metrics.is_some() {
        (None, None, DeltaClassification::Improved)
      } else {
        let base_weight = b.status.kind_weight();
        let new_weight = n.status.kind_weight();
        if base_weight == new_weight {
          (None, None, DeltaClassification::Unchanged)
        } else if new_weight < base_weight {
          (None, None, DeltaClassification::Improved)
        } else {
          (None, None, DeltaClassification::Regressed)
        }
      }
    }
  }
}

fn classify_metrics(diff_delta: f64, perceptual_delta: f64) -> DeltaClassification {
  if diff_delta.abs() <= METRIC_EPS {
    if perceptual_delta.abs() <= METRIC_EPS {
      DeltaClassification::Unchanged
    } else if perceptual_delta < 0.0 {
      DeltaClassification::Improved
    } else {
      DeltaClassification::Regressed
    }
  } else if diff_delta < 0.0 {
    DeltaClassification::Improved
  } else {
    DeltaClassification::Regressed
  }
}

fn collect_top_metric_deltas(entries: &[DeltaEntry], improvements: bool) -> Vec<DeltaRankedEntry> {
  let mut out = entries
    .iter()
    .filter_map(|entry| {
      let diff = entry.diff_percentage_delta?;
      let perceptual = entry.perceptual_distance_delta?;
      Some((entry.name.clone(), diff, perceptual))
    })
    .filter(|(_, diff, perceptual)| {
      if improvements {
        *diff < -METRIC_EPS || (*diff).abs() <= METRIC_EPS && *perceptual < -METRIC_EPS
      } else {
        *diff > METRIC_EPS || (*diff).abs() <= METRIC_EPS && *perceptual > METRIC_EPS
      }
    })
    .map(|(name, diff, perceptual)| DeltaRankedEntry {
      name,
      diff_percentage_delta: diff,
      perceptual_distance_delta: perceptual,
    })
    .collect::<Vec<_>>();

  out.sort_by(|a, b| {
    let primary = if improvements {
      a.diff_percentage_delta
        .partial_cmp(&b.diff_percentage_delta)
        .unwrap_or(std::cmp::Ordering::Equal)
    } else {
      b.diff_percentage_delta
        .partial_cmp(&a.diff_percentage_delta)
        .unwrap_or(std::cmp::Ordering::Equal)
    };
    if primary != std::cmp::Ordering::Equal {
      return primary;
    }

    let secondary = if improvements {
      a.perceptual_distance_delta
        .partial_cmp(&b.perceptual_distance_delta)
        .unwrap_or(std::cmp::Ordering::Equal)
    } else {
      b.perceptual_distance_delta
        .partial_cmp(&a.perceptual_distance_delta)
        .unwrap_or(std::cmp::Ordering::Equal)
    };
    if secondary != std::cmp::Ordering::Equal {
      return secondary;
    }

    a.name.cmp(&b.name)
  });

  out
}

fn is_failing_regression(entry: &DeltaEntry, threshold: f64) -> bool {
  match entry.classification {
    DeltaClassification::Regressed => {
      if let Some(delta) = entry.diff_percentage_delta {
        if delta > threshold + METRIC_EPS {
          return true;
        }
        if delta.abs() <= METRIC_EPS {
          return entry
            .perceptual_distance_delta
            .map(|d| d > METRIC_EPS)
            .unwrap_or(false);
        }
        false
      } else {
        true
      }
    }
    DeltaClassification::MissingInNew => true,
    _ => false,
  }
}

fn float_eq(a: f64, b: f64) -> bool {
  (a - b).abs() <= METRIC_EPS
}

fn write_json_report(report: &DeltaReport, path: &Path) -> Result<(), String> {
  ensure_parent_dir(path)?;
  let json = serde_json::to_string_pretty(report)
    .map_err(|e| format!("failed to serialize JSON report: {e}"))?;
  fs::write(path, json).map_err(|e| format!("failed to write {}: {e}", path.display()))
}

fn write_html_report(
  report: &DeltaReport,
  baseline_report_json: &Path,
  new_report_json: &Path,
  path: &Path,
) -> Result<(), String> {
  ensure_parent_dir(path)?;

  let html_dir = path
    .parent()
    .filter(|p| !p.as_os_str().is_empty())
    .map(PathBuf::from)
    .unwrap_or_else(|| PathBuf::from("."));
  let html_dir = fs::canonicalize(&html_dir).unwrap_or(html_dir);

  let mismatch_block = if report.config_mismatches.is_empty() {
    "".to_string()
  } else {
    let mut rows = String::new();
    for mismatch in &report.config_mismatches {
      rows.push_str(&format!(
        "<tr><td>{}</td><td>{}</td><td>{}</td></tr>",
        escape_html(mismatch.field),
        escape_html(&mismatch.baseline),
        escape_html(&mismatch.new)
      ));
    }
    format!(
      r#"<h2>Config mismatch</h2>
<p class="warning">Baseline/new reports were generated with different diff settings. Deltas may not be comparable.</p>
<table>
  <thead><tr><th>Field</th><th>Baseline</th><th>New</th></tr></thead>
  <tbody>{rows}</tbody>
</table>"#,
    )
  };

  let summary = format!(
    "Paired: {} | Improved: {} | Regressed: {} | Unchanged: {} | Missing entries: {} | Errors (baseline/new): {}/{} | Missing files (baseline/new): {}/{}",
    report.totals.paired,
    report.totals.improved,
    report.totals.regressed,
    report.totals.unchanged,
    report.totals.missing_in_baseline + report.totals.missing_in_new,
    report.totals.baseline_errors,
    report.totals.new_errors,
    report.totals.baseline_missing,
    report.totals.new_missing
  );

  let top_improvements = format_top_list("Top improvements", &report.top_improvements, true);
  let top_regressions = format_top_list("Top regressions", &report.top_regressions, false);

  let baseline_report_dir = baseline_report_json
    .parent()
    .filter(|p| !p.as_os_str().is_empty())
    .unwrap_or_else(|| Path::new("."));
  let new_report_dir = new_report_json
    .parent()
    .filter(|p| !p.as_os_str().is_empty())
    .unwrap_or_else(|| Path::new("."));

  let baseline_html_link = guess_report_html_path(baseline_report_json)
    .map(|path| path_for_report(&html_dir, &path))
    .unwrap_or_else(|| "-".to_string());
  let new_html_link = guess_report_html_path(new_report_json)
    .map(|path| path_for_report(&html_dir, &path))
    .unwrap_or_else(|| "-".to_string());

  let mut rows = String::new();
  for entry in &report.results {
    let baseline_status = entry
      .baseline
      .as_ref()
      .map(|s| s.status.label())
      .unwrap_or("-");
    let new_status = entry.new.as_ref().map(|s| s.status.label()).unwrap_or("-");

    let baseline_diff_image = entry
      .baseline
      .as_ref()
      .and_then(|s| s.diff.as_deref())
      .map(|diff| format_report_image_cell(&html_dir, baseline_report_dir, "Diff", diff))
      .unwrap_or_else(|| "-".to_string());
    let new_diff_image = entry
      .new
      .as_ref()
      .and_then(|s| s.diff.as_deref())
      .map(|diff| format_report_image_cell(&html_dir, new_report_dir, "Diff", diff))
      .unwrap_or_else(|| "-".to_string());

    let baseline_diff = entry
      .baseline
      .as_ref()
      .and_then(|s| s.metrics)
      .map(|m| format!("{:.4}%", m.diff_percentage))
      .unwrap_or_else(|| "-".to_string());
    let new_diff = entry
      .new
      .as_ref()
      .and_then(|s| s.metrics)
      .map(|m| format!("{:.4}%", m.diff_percentage))
      .unwrap_or_else(|| "-".to_string());

    let baseline_perceptual = entry
      .baseline
      .as_ref()
      .and_then(|s| s.metrics)
      .map(|m| format!("{:.4}", m.perceptual_distance))
      .unwrap_or_else(|| "-".to_string());
    let new_perceptual = entry
      .new
      .as_ref()
      .and_then(|s| s.metrics)
      .map(|m| format!("{:.4}", m.perceptual_distance))
      .unwrap_or_else(|| "-".to_string());

    let diff_delta = entry
      .diff_percentage_delta
      .map(|d| format!("{:+.4}%", d))
      .unwrap_or_else(|| "-".to_string());
    let perceptual_delta = entry
      .perceptual_distance_delta
      .map(|d| format!("{:+.4}", d))
      .unwrap_or_else(|| "-".to_string());

    let baseline_error = entry
      .baseline
      .as_ref()
      .and_then(|s| s.error.as_deref())
      .unwrap_or("");
    let new_error = entry
      .new
      .as_ref()
      .and_then(|s| s.error.as_deref())
      .unwrap_or("");
    let error_combined = if baseline_error.is_empty() && new_error.is_empty() {
      "".to_string()
    } else if baseline_error.is_empty() {
      format!("new: {new_error}")
    } else if new_error.is_empty() {
      format!("baseline: {baseline_error}")
    } else {
      format!("baseline: {baseline_error}\nnew: {new_error}")
    };

    rows.push_str(&format!(
      "<tr class=\"{row_class}\"><td>{name}</td><td>{classification}</td><td>{baseline_status}</td><td>{baseline_diff}</td><td>{baseline_perceptual}</td><td>{baseline_diff_image}</td><td>{new_status}</td><td>{new_diff}</td><td>{new_perceptual}</td><td>{new_diff_image}</td><td>{diff_delta}</td><td>{perceptual_delta}</td><td class=\"error\">{error}</td></tr>",
      row_class = entry.classification.row_class(),
      name = escape_html(&entry.name),
      classification = escape_html(entry.classification.label()),
      baseline_status = escape_html(baseline_status),
      baseline_diff = baseline_diff,
      baseline_perceptual = baseline_perceptual,
      baseline_diff_image = baseline_diff_image,
      new_status = escape_html(new_status),
      new_diff = new_diff,
      new_perceptual = new_perceptual,
      new_diff_image = new_diff_image,
      diff_delta = diff_delta,
      perceptual_delta = perceptual_delta,
      error = escape_html(&error_combined),
    ));
  }

  let content = format!(
    r#"<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Diff report delta</title>
    <style>
      body {{ font-family: sans-serif; margin: 20px; }}
      table {{ border-collapse: collapse; width: 100%; }}
      th, td {{ border: 1px solid #ddd; padding: 6px; vertical-align: top; }}
      th {{ background: #f3f3f3; position: sticky; top: 0; }}
      tr.improved {{ background: #f3fff3; }}
      tr.regressed {{ background: #fff3f3; }}
      tr.missing {{ background: #fffbe8; }}
      tr.unchanged {{ background: #ffffff; }}
      .warning {{ color: #b00020; }}
      .error {{ color: #b00020; white-space: pre-wrap; }}
      .top-list table {{ width: auto; }}
      .thumb img {{ max-width: 240px; max-height: 180px; display: block; }}
    </style>
  </head>
  <body>
    <h1>Diff report delta</h1>
    <p><strong>Baseline:</strong> {baseline_before} → {baseline_after}</p>
    <p><strong>Baseline report:</strong> {baseline_report_link}</p>
    <p><strong>New:</strong> {new_before} → {new_after}</p>
    <p><strong>New report:</strong> {new_report_link}</p>
    <p><strong>Config:</strong> tolerance={tolerance}, max_diff_percent={max_diff_percent:.4}, max_perceptual_distance={max_perceptual}, ignore_alpha={ignore_alpha}</p>
    <p><strong>Summary:</strong> {summary}</p>
    {mismatch_block}
    <div class="top-list">
      {top_regressions}
      {top_improvements}
    </div>
    <h2>All entries</h2>
    <table>
      <thead>
        <tr>
          <th>Name</th>
          <th>Delta</th>
          <th>Baseline status</th>
          <th>Baseline diff %</th>
          <th>Baseline perceptual</th>
          <th>Baseline diff</th>
          <th>New status</th>
          <th>New diff %</th>
          <th>New perceptual</th>
          <th>New diff</th>
          <th>Δ diff %</th>
          <th>Δ perceptual</th>
          <th>Error</th>
        </tr>
      </thead>
      <tbody>
        {rows}
      </tbody>
    </table>
  </body>
</html>
"#,
    baseline_before = escape_html(&report.baseline.before_dir),
    baseline_after = escape_html(&report.baseline.after_dir),
    new_before = escape_html(&report.new.before_dir),
    new_after = escape_html(&report.new.after_dir),
    baseline_report_link = format_report_link(&baseline_html_link),
    new_report_link = format_report_link(&new_html_link),
    tolerance = report.new.tolerance,
    max_diff_percent = report.new.max_diff_percent,
    max_perceptual = report
      .new
      .max_perceptual_distance
      .map(|d| format!("{d:.4}"))
      .unwrap_or_else(|| "-".to_string()),
    ignore_alpha = if report.new.ignore_alpha { "yes" } else { "no" },
    summary = escape_html(&summary),
    mismatch_block = mismatch_block,
    top_regressions = top_regressions,
    top_improvements = top_improvements,
    rows = rows,
  );

  fs::write(path, content).map_err(|e| format!("failed to write {}: {e}", path.display()))
}

fn format_report_image_cell(
  delta_html_dir: &Path,
  report_dir: &Path,
  label: &str,
  report_relative_path: &str,
) -> String {
  let target_path = resolve_report_path(report_dir, report_relative_path);
  let rel = path_for_report(delta_html_dir, &target_path);
  format_linked_image(label, &rel)
}

fn resolve_report_path(report_dir: &Path, report_relative_path: &str) -> PathBuf {
  let raw = PathBuf::from(report_relative_path);
  if raw.is_absolute() {
    raw
  } else {
    report_dir.join(raw)
  }
}

fn format_linked_image(label: &str, path: &str) -> String {
  let escaped = escape_html(path);
  format!(
    r#"<div class="thumb"><a href="{p}">{l}</a><br><img src="{p}" loading="lazy"></div>"#,
    p = escaped,
    l = escape_html(label),
  )
}

fn format_report_link(href: &str) -> String {
  if href == "-" {
    return "-".to_string();
  }
  let escaped = escape_html(href);
  format!(r#"<a href="{p}">{p}</a>"#, p = escaped)
}

fn guess_report_html_path(json_path: &Path) -> Option<PathBuf> {
  let dir = json_path.parent()?;
  let stem = json_path.file_stem()?.to_str()?;

  let stem_candidate = dir.join(format!("{stem}.html"));
  if stem_candidate.is_file() {
    return Some(stem_candidate);
  }

  let report_candidate = dir.join("report.html");
  if report_candidate.is_file() {
    return Some(report_candidate);
  }

  let diff_candidate = dir.join("diff_report.html");
  if diff_candidate.is_file() {
    return Some(diff_candidate);
  }

  None
}

fn format_top_list(title: &str, entries: &[DeltaRankedEntry], improvements: bool) -> String {
  if entries.is_empty() {
    return format!("<h2>{}</h2><p>-</p>", escape_html(title));
  }

  let mut rows = String::new();
  for entry in entries {
    rows.push_str(&format!(
      "<tr><td>{}</td><td>{:+.4}%</td><td>{:+.4}</td></tr>",
      escape_html(&entry.name),
      entry.diff_percentage_delta,
      entry.perceptual_distance_delta
    ));
  }

  let note = if improvements {
    "more negative is better"
  } else {
    "more positive is worse"
  };

  format!(
    r#"<h2>{title}</h2>
<p><em>{note}</em></p>
<table>
  <thead><tr><th>Name</th><th>Δ diff %</th><th>Δ perceptual</th></tr></thead>
  <tbody>{rows}</tbody>
</table>"#,
    title = escape_html(title),
    note = escape_html(note),
    rows = rows
  )
}
