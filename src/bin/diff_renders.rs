use clap::Parser;
use fastrender::image_output::{diff_png, DiffMetrics};
use pathdiff::diff_paths;
use serde::Serialize;
use std::collections::{BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Parser, Debug)]
#[command(
  name = "diff_renders",
  about = "Compare two render output directories and produce a report"
)]
struct Args {
  /// Directory containing "before" PNGs
  #[arg(long)]
  before: PathBuf,

  /// Directory containing "after" PNGs
  #[arg(long)]
  after: PathBuf,

  /// Per-channel tolerance (0-255). Defaults to env compat vars or 0.
  #[arg(long)]
  tolerance: Option<u8>,

  /// Maximum percent of pixels allowed to differ (0-100). Defaults to env compat vars or 0.
  #[arg(long)]
  max_diff_percent: Option<f64>,

  /// Only diff a deterministic shard of the inputs (index/total, 0-based)
  #[arg(long, value_parser = parse_shard)]
  shard: Option<(usize, usize)>,

  /// Path to write diff_report.json
  #[arg(long, default_value = "diff_report.json")]
  json: PathBuf,

  /// Path to write diff_report.html
  #[arg(long, default_value = "diff_report.html")]
  html: PathBuf,
}

#[derive(Serialize, Clone)]
struct DiffReport {
  before_dir: String,
  after_dir: String,
  tolerance: u8,
  max_diff_percent: f64,
  shard: Option<ShardInfo>,
  totals: DiffReportTotals,
  results: Vec<DiffReportEntry>,
}

#[derive(Serialize, Clone)]
struct ShardInfo {
  index: usize,
  total: usize,
  discovered: usize,
}

#[derive(Serialize, Default, Clone)]
struct DiffReportTotals {
  discovered: usize,
  processed: usize,
  matches: usize,
  within_threshold: usize,
  differences: usize,
  missing: usize,
  errors: usize,
  shard_skipped: usize,
}

#[derive(Serialize, Clone)]
struct DiffReportEntry {
  name: String,
  status: EntryStatus,
  before: Option<String>,
  after: Option<String>,
  diff: Option<String>,
  metrics: Option<MetricsSummary>,
  error: Option<String>,
}

#[derive(Serialize, Clone, Copy, PartialEq, Eq)]
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
  fn is_failure(&self) -> bool {
    matches!(
      self,
      EntryStatus::Diff
        | EntryStatus::MissingBefore
        | EntryStatus::MissingAfter
        | EntryStatus::Error
    )
  }

  fn sort_weight(&self) -> u8 {
    match self {
      EntryStatus::Error => 4,
      EntryStatus::MissingBefore | EntryStatus::MissingAfter => 3,
      EntryStatus::Diff => 2,
      EntryStatus::WithinThreshold => 1,
      EntryStatus::Match => 0,
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

#[derive(Serialize, Clone, Copy)]
struct MetricsSummary {
  pixel_diff: u64,
  total_pixels: u64,
  diff_percentage: f64,
}

impl From<DiffMetrics> for MetricsSummary {
  fn from(metrics: DiffMetrics) -> Self {
    MetricsSummary {
      pixel_diff: metrics.pixel_diff,
      total_pixels: metrics.total_pixels,
      diff_percentage: metrics.diff_percentage,
    }
  }
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

  let tolerance = resolve_tolerance(args.tolerance)?;
  let max_diff_percent = resolve_max_diff_percent(args.max_diff_percent)?;

  let before_dir = normalize_dir(&args.before)?;
  let after_dir = normalize_dir(&args.after)?;

  let before_pngs = collect_pngs(&before_dir)?;
  let after_pngs = collect_pngs(&after_dir)?;

  let mut stems = BTreeSet::new();
  for key in before_pngs.keys() {
    stems.insert(key.clone());
  }
  for key in after_pngs.keys() {
    stems.insert(key.clone());
  }

  let html_dir = args
    .html
    .parent()
    .filter(|p| !p.as_os_str().is_empty())
    .map(PathBuf::from)
    .unwrap_or_else(|| PathBuf::from("."));
  fs::create_dir_all(&html_dir)
    .map_err(|e| format!("failed to create report directory {html_dir:?}: {e}"))?;

  let html_stem = args
    .html
    .file_stem()
    .and_then(|s| s.to_str())
    .unwrap_or("diff_report");
  let artifact_dir = html_dir.join(format!("{html_stem}_files"));
  if artifact_dir.exists() {
    fs::remove_dir_all(&artifact_dir).map_err(|e| {
      format!(
        "failed to clear existing artifact dir {}: {e}",
        artifact_dir.display()
      )
    })?;
  }
  fs::create_dir_all(&artifact_dir).map_err(|e| {
    format!(
      "failed to create artifact dir {}: {e}",
      artifact_dir.display()
    )
  })?;
  let diff_dir = artifact_dir.join("diffs");
  fs::create_dir_all(&diff_dir)
    .map_err(|e| format!("failed to create diff dir {}: {e}", diff_dir.display()))?;

  let mut totals = DiffReportTotals {
    discovered: stems.len(),
    ..DiffReportTotals::default()
  };
  let mut results = Vec::new();

  for (idx, stem) in stems.iter().enumerate() {
    if let Some((shard_index, shard_total)) = args.shard {
      if idx % shard_total != shard_index {
        totals.shard_skipped += 1;
        continue;
      }
    }

    let before_path = before_pngs.get(stem);
    let after_path = after_pngs.get(stem);

    let before_rel = before_path.map(|p| path_for_report(&html_dir, p));
    let after_rel = after_path.map(|p| path_for_report(&html_dir, p));

    let entry = match (before_path, after_path) {
      (None, Some(_)) => DiffReportEntry {
        name: stem.clone(),
        status: EntryStatus::MissingBefore,
        before: None,
        after: after_rel,
        diff: None,
        metrics: None,
        error: Some("Missing in before dir".to_string()),
      },
      (Some(_), None) => DiffReportEntry {
        name: stem.clone(),
        status: EntryStatus::MissingAfter,
        before: before_rel,
        after: None,
        diff: None,
        metrics: None,
        error: Some("Missing in after dir".to_string()),
      },
      (Some(before), Some(after)) => {
        totals.processed += 1;
        match fs::read(before) {
          Err(e) => DiffReportEntry {
            name: stem.clone(),
            status: EntryStatus::Error,
            before: before_rel,
            after: after_rel,
            diff: None,
            metrics: None,
            error: Some(format!("Failed to read {}: {e}", before.display())),
          },
          Ok(before_png) => match fs::read(after) {
            Err(e) => DiffReportEntry {
              name: stem.clone(),
              status: EntryStatus::Error,
              before: before_rel,
              after: after_rel,
              diff: None,
              metrics: None,
              error: Some(format!("Failed to read {}: {e}", after.display())),
            },
            Ok(after_png) => match diff_png(&after_png, &before_png, tolerance) {
              Err(e) => DiffReportEntry {
                name: stem.clone(),
                status: EntryStatus::Error,
                before: before_rel,
                after: after_rel,
                diff: None,
                metrics: None,
                error: Some(format!("Diff failed: {e}")),
              },
              Ok((metrics, diff_image)) => {
                let status = if metrics.diff_percentage == 0.0 {
                  EntryStatus::Match
                } else if metrics.diff_percentage <= max_diff_percent {
                  EntryStatus::WithinThreshold
                } else {
                  EntryStatus::Diff
                };

                let mut diff_path = None;
                let mut final_status = status;
                let mut error = None;
                if metrics.pixel_diff > 0 {
                  let path = diff_dir.join(format!("{stem}.png"));
                  let write_result = fs::write(&path, diff_image);
                  match write_result {
                    Ok(_) => {
                      diff_path = Some(path_for_report(&html_dir, &path));
                    }
                    Err(e) => {
                      final_status = EntryStatus::Error;
                      error = Some(format!(
                        "Failed to write diff image {}: {e}",
                        path.display()
                      ));
                    }
                  }
                }

                DiffReportEntry {
                  name: stem.clone(),
                  status: final_status,
                  before: before_rel,
                  after: after_rel,
                  diff: diff_path,
                  metrics: Some(metrics.into()),
                  error,
                }
              }
            },
          },
        }
      }
      (None, None) => continue,
    };

    track_status(&mut totals, entry.status);
    results.push(entry);
  }

  let shard = args.shard.map(|(index, total)| ShardInfo {
    index,
    total,
    discovered: stems.len(),
  });

  let report = DiffReport {
    before_dir: display_path(&before_dir),
    after_dir: display_path(&after_dir),
    tolerance,
    max_diff_percent,
    shard,
    totals,
    results,
  };

  write_json_report(&report, &args.json)?;
  write_html_report(&report, &args.html)?;

  let failures = report
    .results
    .iter()
    .filter(|r| r.status.is_failure())
    .count();
  if failures > 0 {
    eprintln!("{failures} differences over threshold");
    return Ok(1);
  }

  Ok(0)
}

fn track_status(totals: &mut DiffReportTotals, status: EntryStatus) {
  match status {
    EntryStatus::Match => totals.matches += 1,
    EntryStatus::WithinThreshold => totals.within_threshold += 1,
    EntryStatus::Diff => totals.differences += 1,
    EntryStatus::MissingBefore | EntryStatus::MissingAfter => totals.missing += 1,
    EntryStatus::Error => totals.errors += 1,
  }
}

fn parse_shard(s: &str) -> Result<(usize, usize), String> {
  let parts: Vec<&str> = s.split('/').collect();
  if parts.len() != 2 {
    return Err("shard must be index/total (e.g., 0/4)".to_string());
  }
  let index = parts[0]
    .parse::<usize>()
    .map_err(|_| "invalid shard index".to_string())?;
  let total = parts[1]
    .parse::<usize>()
    .map_err(|_| "invalid shard total".to_string())?;
  if total == 0 {
    return Err("shard total must be > 0".to_string());
  }
  if index >= total {
    return Err("shard index must be < total".to_string());
  }
  Ok((index, total))
}

fn resolve_tolerance(flag: Option<u8>) -> Result<u8, String> {
  if let Some(value) = flag {
    return Ok(value);
  }
  if let Ok(env) = std::env::var("FIXTURE_TOLERANCE") {
    return env
      .parse::<u8>()
      .map_err(|e| format!("Invalid FIXTURE_TOLERANCE '{env}': {e}"));
  }
  if std::env::var("FIXTURE_FUZZY").is_ok() {
    return Ok(10);
  }
  Ok(0)
}

fn resolve_max_diff_percent(flag: Option<f64>) -> Result<f64, String> {
  if let Some(value) = flag {
    validate_percent(value)?;
    return Ok(value);
  }
  if let Ok(env) = std::env::var("FIXTURE_MAX_DIFFERENT_PERCENT") {
    let parsed = env
      .parse::<f64>()
      .map_err(|e| format!("Invalid FIXTURE_MAX_DIFFERENT_PERCENT '{env}': {e}"))?;
    validate_percent(parsed)?;
    return Ok(parsed);
  }
  if std::env::var("FIXTURE_FUZZY").is_ok() {
    return Ok(1.0);
  }
  Ok(0.0)
}

fn validate_percent(value: f64) -> Result<(), String> {
  if (0.0..=100.0).contains(&value) {
    Ok(())
  } else {
    Err("max_diff_percent must be between 0 and 100".to_string())
  }
}

fn collect_pngs(dir: &Path) -> Result<HashMap<String, PathBuf>, String> {
  if !dir.is_dir() {
    return Err(format!("{} is not a directory", dir.display()));
  }
  let mut map = HashMap::new();
  for entry in fs::read_dir(dir).map_err(|e| format!("failed to read {}: {e}", dir.display()))? {
    let entry = entry.map_err(|e| format!("failed to read entry: {e}"))?;
    let path = entry.path();
    if path.is_dir() {
      continue;
    }
    let ext = path
      .extension()
      .and_then(|e| e.to_str())
      .map(|s| s.to_ascii_lowercase());
    if ext.as_deref() != Some("png") {
      continue;
    }
    let stem = match path.file_stem().map(|s| s.to_string_lossy().to_string()) {
      Some(s) => s,
      None => continue,
    };
    map.entry(stem).or_insert(path);
  }
  Ok(map)
}

fn write_json_report(report: &DiffReport, path: &Path) -> Result<(), String> {
  if let Some(parent) = path.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent).map_err(|e| {
        format!(
          "failed to create parent directory {}: {e}",
          parent.display()
        )
      })?;
    }
  }
  let json = serde_json::to_string_pretty(report)
    .map_err(|e| format!("failed to serialize JSON report: {e}"))?;
  fs::write(path, json).map_err(|e| format!("failed to write {}: {e}", path.display()))
}

fn write_html_report(report: &DiffReport, path: &Path) -> Result<(), String> {
  if let Some(parent) = path.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent).map_err(|e| {
        format!(
          "failed to create parent directory {}: {e}",
          parent.display()
        )
      })?;
    }
  }

  let mut rows = String::new();
  let mut sorted = report.results.clone();
  sorted.sort_by(|a, b| {
    let a_score = (
      a.status.sort_weight(),
      a.metrics.map(|m| m.diff_percentage).unwrap_or(0.0),
    );
    let b_score = (
      b.status.sort_weight(),
      b.metrics.map(|m| m.diff_percentage).unwrap_or(0.0),
    );
    b_score.0.cmp(&a_score.0).then_with(|| {
      b_score
        .1
        .partial_cmp(&a_score.1)
        .unwrap_or(std::cmp::Ordering::Equal)
    })
  });

  for entry in sorted {
    let diff_percent = entry
      .metrics
      .map(|m| format!("{:.4}%", m.diff_percentage))
      .unwrap_or_else(|| "-".to_string());
    let pixel_diff = entry
      .metrics
      .map(|m| m.pixel_diff.to_string())
      .unwrap_or_else(|| "-".to_string());
    let total_pixels = entry
      .metrics
      .map(|m| m.total_pixels.to_string())
      .unwrap_or_else(|| "-".to_string());

    let before_cell = entry
      .before
      .as_ref()
      .map(|p| format_linked_image("Before", p))
      .unwrap_or_else(|| "-".to_string());
    let after_cell = entry
      .after
      .as_ref()
      .map(|p| format_linked_image("After", p))
      .unwrap_or_else(|| "-".to_string());
    let diff_cell = entry
      .diff
      .as_ref()
      .map(|p| format_linked_image("Diff", p))
      .unwrap_or_else(|| "-".to_string());
    let after_and_diff = if entry.diff.is_some() {
      format!("{after_cell}{diff_cell}")
    } else {
      after_cell
    };

    let error = entry.error.unwrap_or_default();
    rows.push_str(&format!(
      "<tr class=\"{}\"><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td class=\"error\">{}</td></tr>",
      entry.status.label(),
      escape_html(&entry.name),
      escape_html(entry.status.label()),
      diff_percent,
      pixel_diff,
      total_pixels,
      before_cell,
      after_and_diff,
      escape_html(&error),
    ));
  }

  let shard_info = report
    .shard
    .as_ref()
    .map(|s| {
      format!(
        "Shard {}/{} ({} discovered)",
        s.index, s.total, s.discovered
      )
    })
    .unwrap_or_default();

  let content = format!(
    r#"<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Render diff report</title>
    <style>
      body {{ font-family: sans-serif; margin: 20px; }}
      table {{ border-collapse: collapse; width: 100%; }}
      th, td {{ border: 1px solid #ddd; padding: 6px; vertical-align: top; }}
      th {{ background: #f3f3f3; position: sticky; top: 0; }}
      tr.match {{ background: #f8fff8; }}
      tr.within-threshold {{ background: #f8f8ff; }}
      tr.diff {{ background: #fff8f8; }}
      tr.missing-before, tr.missing-after, tr.error {{ background: #fff0f0; }}
      .thumb img {{ max-width: 320px; max-height: 240px; display: block; }}
      .error {{ color: #b00020; }}
    </style>
  </head>
  <body>
    <h1>Render diff report</h1>
    <p><strong>Before:</strong> {before}</p>
    <p><strong>After:</strong> {after}</p>
    <p><strong>Tolerance:</strong> {tolerance} | <strong>Max diff %:</strong> {max_diff_percent:.4} {shard}</p>
    <p>Processed {processed} of {discovered} candidates ({matches} exact, {within} within threshold, {diffs} failing, {missing} missing, {errors} errors{skipped}).</p>
    <table>
      <thead>
        <tr>
          <th>Name</th>
          <th>Status</th>
          <th>Diff %</th>
          <th>Pixel diff</th>
          <th>Total pixels</th>
          <th>Before</th>
          <th>After | Diff</th>
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
    before = escape_html(&report.before_dir),
    after = escape_html(&report.after_dir),
    tolerance = report.tolerance,
    max_diff_percent = report.max_diff_percent,
    shard = if shard_info.is_empty() {
      "".to_string()
    } else {
      format!("| {}", escape_html(&shard_info))
    },
    processed = report.totals.processed,
    discovered = report.totals.discovered,
    matches = report.totals.matches,
    within = report.totals.within_threshold,
    diffs = report.totals.differences,
    missing = report.totals.missing,
    errors = report.totals.errors,
    skipped = if report.totals.shard_skipped > 0 {
      format!(", {} skipped by shard", report.totals.shard_skipped)
    } else {
      "".to_string()
    },
    rows = rows,
  );

  fs::write(path, content).map_err(|e| format!("failed to write {}: {e}", path.display()))
}

fn format_linked_image(label: &str, path: &str) -> String {
  let escaped = escape_html(path);
  format!(
    r#"<div class="thumb"><a href="{p}">{l}</a><br><img src="{p}" loading="lazy"></div>"#,
    p = escaped,
    l = escape_html(label)
  )
}

fn escape_html(input: &str) -> String {
  input
    .replace('&', "&amp;")
    .replace('<', "&lt;")
    .replace('>', "&gt;")
    .replace('"', "&quot;")
    .replace('\'', "&#39;")
}

fn path_for_report(base: &Path, target: &Path) -> String {
  diff_paths(target, base)
    .unwrap_or_else(|| target.to_path_buf())
    .display()
    .to_string()
}

fn display_path(path: &Path) -> String {
  fs::canonicalize(path)
    .unwrap_or_else(|_| path.to_path_buf())
    .display()
    .to_string()
}

fn normalize_dir(path: &Path) -> Result<PathBuf, String> {
  let canonical = fs::canonicalize(path).map_err(|e| format!("{}: {e}", path.display()))?;
  if !canonical.is_dir() {
    return Err(format!("{} is not a directory", canonical.display()));
  }
  Ok(canonical)
}
