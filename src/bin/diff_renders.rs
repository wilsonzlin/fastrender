mod common;

use clap::Parser;
use common::report::{display_path, ensure_parent_dir, escape_html, path_for_report};
use fastrender::image_output::{diff_png, DiffMetrics};
use serde::Serialize;
use std::collections::{BTreeSet, HashMap};
use std::fs;
use std::path::{Component, Path, PathBuf};
use walkdir::WalkDir;

#[derive(clap::ValueEnum, Debug, Clone, Copy, Serialize)]
#[serde(rename_all = "snake_case")]
enum SortBy {
  Pixel,
  Percent,
  Perceptual,
}

impl SortBy {
  fn label(&self) -> &'static str {
    match self {
      SortBy::Pixel => "pixel",
      SortBy::Percent => "percent",
      SortBy::Perceptual => "perceptual",
    }
  }
}

#[derive(Parser, Debug)]
#[command(
  name = "diff_renders",
  about = "Compare two render outputs (directories or PNG files) and produce a report"
)]
struct Args {
  /// Directory of PNGs (recursively) or a single PNG file ("before")
  #[arg(long)]
  before: PathBuf,

  /// Directory of PNGs (recursively) or a single PNG file ("after")
  #[arg(long)]
  after: PathBuf,

  /// Per-channel tolerance (0-255). Defaults to env compat vars or 0.
  #[arg(long)]
  tolerance: Option<u8>,

  /// Maximum percent of pixels allowed to differ (0-100). Defaults to env compat vars or 0.
  #[arg(long)]
  max_diff_percent: Option<f64>,

  /// Maximum allowed perceptual distance (0.0 = identical). Defaults to env compat vars when set.
  #[arg(long)]
  max_perceptual_distance: Option<f64>,

  /// Sort report entries by metric within each status group.
  #[arg(long, value_enum, default_value_t = SortBy::Percent)]
  sort_by: SortBy,

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
  #[serde(skip_serializing_if = "Option::is_none")]
  max_perceptual_distance: Option<f64>,
  sort_by: SortBy,
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
  perceptual_distance: f64,
}

impl From<DiffMetrics> for MetricsSummary {
  fn from(metrics: DiffMetrics) -> Self {
    MetricsSummary {
      pixel_diff: metrics.pixel_diff,
      total_pixels: metrics.total_pixels,
      diff_percentage: metrics.diff_percentage,
      perceptual_distance: metrics.perceptual_distance,
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
  let max_perceptual_distance = resolve_max_perceptual_distance(args.max_perceptual_distance)?;

  let before_meta =
    fs::metadata(&args.before).map_err(|e| format!("{}: {e}", args.before.display()))?;
  let after_meta =
    fs::metadata(&args.after).map_err(|e| format!("{}: {e}", args.after.display()))?;

  let html_dir_raw = args
    .html
    .parent()
    .filter(|p| !p.as_os_str().is_empty())
    .map(PathBuf::from)
    .unwrap_or_else(|| PathBuf::from("."));
  fs::create_dir_all(&html_dir_raw)
    .map_err(|e| format!("failed to create report directory {html_dir_raw:?}: {e}"))?;
  let html_dir = fs::canonicalize(&html_dir_raw)
    .map_err(|e| format!("failed to canonicalize report directory {html_dir_raw:?}: {e}"))?;

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

  let (before_root, after_root, mut results, totals) =
    match (before_meta.is_dir(), after_meta.is_dir()) {
      (true, true) => {
        let before_dir = normalize_dir(&args.before)?;
        let after_dir = normalize_dir(&args.after)?;
        let (results, totals) = process_directory(
          &before_dir,
          &after_dir,
          &html_dir,
          &diff_dir,
          tolerance,
          max_diff_percent,
          max_perceptual_distance,
          args.shard,
        )?;
        (before_dir, after_dir, results, totals)
      }
      (false, false) => {
        let before_file = normalize_png_file(&args.before)?;
        let after_file = normalize_png_file(&args.after)?;
        let (results, totals) = process_files(
          &before_file,
          &after_file,
          &html_dir,
          &diff_dir,
          tolerance,
          max_diff_percent,
          max_perceptual_distance,
          args.shard,
        )?;
        (before_file, after_file, results, totals)
      }
      _ => {
        return Err(
          "--before and --after must both be directories or both be PNG files".to_string(),
        );
      }
    };

  sort_entries(&mut results, args.sort_by);

  let shard = args.shard.map(|(index, total)| ShardInfo {
    index,
    total,
    discovered: totals.discovered,
  });

  let report = DiffReport {
    before_dir: display_path(&before_root),
    after_dir: display_path(&after_root),
    tolerance,
    max_diff_percent,
    max_perceptual_distance,
    sort_by: args.sort_by,
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

fn resolve_max_perceptual_distance(flag: Option<f64>) -> Result<Option<f64>, String> {
  if let Some(value) = flag {
    validate_perceptual_distance(value)?;
    return Ok(Some(value));
  }
  if let Ok(env) = std::env::var("FIXTURE_MAX_PERCEPTUAL_DISTANCE") {
    let parsed = env
      .parse::<f64>()
      .map_err(|e| format!("Invalid FIXTURE_MAX_PERCEPTUAL_DISTANCE '{env}': {e}"))?;
    validate_perceptual_distance(parsed)?;
    return Ok(Some(parsed));
  }
  if std::env::var("FIXTURE_FUZZY").is_ok() {
    return Ok(Some(0.05));
  }
  Ok(None)
}

fn validate_perceptual_distance(value: f64) -> Result<(), String> {
  if value.is_finite() && (0.0..=1.0).contains(&value) {
    Ok(())
  } else {
    Err("max_perceptual_distance must be a finite number between 0 and 1".to_string())
  }
}

fn process_directory(
  before_dir: &Path,
  after_dir: &Path,
  html_dir: &Path,
  diff_dir: &Path,
  tolerance: u8,
  max_diff_percent: f64,
  max_perceptual_distance: Option<f64>,
  shard: Option<(usize, usize)>,
) -> Result<(Vec<DiffReportEntry>, DiffReportTotals), String> {
  let before_pngs = collect_pngs(before_dir)?;
  let after_pngs = collect_pngs(after_dir)?;

  let mut keys = BTreeSet::new();
  keys.extend(before_pngs.keys().cloned());
  keys.extend(after_pngs.keys().cloned());

  let mut totals = DiffReportTotals {
    discovered: keys.len(),
    ..DiffReportTotals::default()
  };
  let mut results = Vec::new();

  for (idx, key) in keys.iter().enumerate() {
    if let Some((shard_index, shard_total)) = shard {
      if idx % shard_total != shard_index {
        totals.shard_skipped += 1;
        continue;
      }
    }

    let before_path = before_pngs.get(key).map(PathBuf::as_path);
    let after_path = after_pngs.get(key).map(PathBuf::as_path);
    let entry = process_entry(
      key,
      before_path,
      after_path,
      Some(before_dir),
      Some(after_dir),
      html_dir,
      diff_dir,
      tolerance,
      max_diff_percent,
      max_perceptual_distance,
      &mut totals,
    );
    track_status(&mut totals, entry.status);
    results.push(entry);
  }

  Ok((results, totals))
}

fn derive_file_pair_name(before: &Path, after: &Path) -> String {
  if let Some(common_suffix) = common_suffix_path(before, after) {
    let mut key_path = common_suffix;
    key_path.set_extension("");
    let key = path_to_forward_slashes(&key_path);
    if !key.is_empty() {
      return key;
    }
  }

  before
    .file_stem()
    .or_else(|| after.file_stem())
    .map(|s| s.to_string_lossy().to_string())
    .unwrap_or_else(|| "render".to_string())
}

fn common_suffix_path(before: &Path, after: &Path) -> Option<PathBuf> {
  let before_components: Vec<_> = before.components().collect();
  let after_components: Vec<_> = after.components().collect();

  let mut common_components = Vec::new();
  let mut i = before_components.len();
  let mut j = after_components.len();

  while i > 0 && j > 0 {
    let a = before_components[i - 1];
    let b = after_components[j - 1];
    if a != b {
      break;
    }
    if let Component::Normal(os) = a {
      common_components.push(os.to_os_string());
    } else {
      break;
    }
    i -= 1;
    j -= 1;
  }

  if common_components.is_empty() {
    return None;
  }
  common_components.reverse();

  let mut suffix = PathBuf::new();
  for component in common_components {
    suffix.push(component);
  }
  Some(suffix)
}

fn process_files(
  before: &Path,
  after: &Path,
  html_dir: &Path,
  diff_dir: &Path,
  tolerance: u8,
  max_diff_percent: f64,
  max_perceptual_distance: Option<f64>,
  shard: Option<(usize, usize)>,
) -> Result<(Vec<DiffReportEntry>, DiffReportTotals), String> {
  let name = derive_file_pair_name(before, after);

  let mut totals = DiffReportTotals {
    discovered: 1,
    ..DiffReportTotals::default()
  };

  if let Some((shard_index, shard_total)) = shard {
    if 0 % shard_total != shard_index {
      totals.shard_skipped = 1;
      return Ok((Vec::new(), totals));
    }
  }

  let entry = process_entry(
    &name,
    Some(before),
    Some(after),
    None,
    None,
    html_dir,
    diff_dir,
    tolerance,
    max_diff_percent,
    max_perceptual_distance,
    &mut totals,
  );
  track_status(&mut totals, entry.status);

  Ok((vec![entry], totals))
}

fn process_entry(
  name: &str,
  before_path: Option<&Path>,
  after_path: Option<&Path>,
  before_root: Option<&Path>,
  after_root: Option<&Path>,
  html_dir: &Path,
  diff_dir: &Path,
  tolerance: u8,
  max_diff_percent: f64,
  max_perceptual_distance: Option<f64>,
  totals: &mut DiffReportTotals,
) -> DiffReportEntry {
  let before_rel = before_path.map(|p| path_for_report(html_dir, p));
  let after_rel = after_path.map(|p| path_for_report(html_dir, p));

  match (before_path, after_path) {
    (None, Some(_)) => DiffReportEntry {
      name: name.to_string(),
      status: EntryStatus::MissingBefore,
      before: None,
      after: after_rel,
      diff: None,
      metrics: None,
      error: Some(match before_root {
        Some(root) => format!(
          "Missing in before input: {}",
          diff_path_for_name(root, name).display()
        ),
        None => format!("Missing in before input: {name}.png"),
      }),
    },
    (Some(_), None) => DiffReportEntry {
      name: name.to_string(),
      status: EntryStatus::MissingAfter,
      before: before_rel,
      after: None,
      diff: None,
      metrics: None,
      error: Some(match after_root {
        Some(root) => format!(
          "Missing in after input: {}",
          diff_path_for_name(root, name).display()
        ),
        None => format!("Missing in after input: {name}.png"),
      }),
    },
    (Some(before), Some(after)) => {
      totals.processed += 1;
      match fs::read(before) {
        Err(e) => DiffReportEntry {
          name: name.to_string(),
          status: EntryStatus::Error,
          before: before_rel,
          after: after_rel,
          diff: None,
          metrics: None,
          error: Some(format!("Failed to read {}: {e}", before.display())),
        },
        Ok(before_png) => match fs::read(after) {
          Err(e) => DiffReportEntry {
            name: name.to_string(),
            status: EntryStatus::Error,
            before: before_rel,
            after: after_rel,
            diff: None,
            metrics: None,
            error: Some(format!("Failed to read {}: {e}", after.display())),
          },
          Ok(after_png) => match diff_png(&after_png, &before_png, tolerance) {
            Err(e) => DiffReportEntry {
              name: name.to_string(),
              status: EntryStatus::Error,
              before: before_rel,
              after: after_rel,
              diff: None,
              metrics: None,
              error: Some(format!("Diff failed: {e}")),
            },
            Ok((metrics, diff_image)) => {
              let passes_pixels = metrics.diff_percentage <= max_diff_percent + f64::EPSILON;
              let passes_perceptual = max_perceptual_distance
                .map(|max| metrics.perceptual_distance <= max + f64::EPSILON)
                .unwrap_or(true);

              let status = if metrics.diff_percentage == 0.0 && passes_perceptual {
                EntryStatus::Match
              } else if passes_pixels && passes_perceptual {
                EntryStatus::WithinThreshold
              } else {
                EntryStatus::Diff
              };

              let mut diff_path = None;
              let mut final_status = status;
              let mut error = if metrics.rendered_dimensions != metrics.expected_dimensions {
                Some(format!(
                  "Dimensions differ: after {}x{}, before {}x{}",
                  metrics.rendered_dimensions.0,
                  metrics.rendered_dimensions.1,
                  metrics.expected_dimensions.0,
                  metrics.expected_dimensions.1
                ))
              } else {
                None
              };
              if metrics.pixel_diff > 0 {
                let path = diff_path_for_name(diff_dir, name);
                if let Err(e) = ensure_parent_dir(&path) {
                  final_status = EntryStatus::Error;
                  error = Some(e);
                } else {
                  match fs::write(&path, diff_image) {
                    Ok(_) => {
                      diff_path = Some(path_for_report(html_dir, &path));
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
              }

              DiffReportEntry {
                name: name.to_string(),
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
    (None, None) => DiffReportEntry {
      name: name.to_string(),
      status: EntryStatus::Error,
      before: None,
      after: None,
      diff: None,
      metrics: None,
      error: Some("Missing in both inputs".to_string()),
    },
  }
}

fn collect_pngs(root: &Path) -> Result<HashMap<String, PathBuf>, String> {
  if !root.is_dir() {
    return Err(format!("{} is not a directory", root.display()));
  }
  let mut map = HashMap::new();
  for entry in WalkDir::new(root).sort_by_file_name() {
    let entry = entry.map_err(|e| format!("failed to walk {}: {e}", root.display()))?;
    if !entry.file_type().is_file() {
      continue;
    }
    if entry
      .path()
      .extension()
      .and_then(|ext| ext.to_str())
      .map(|ext| ext.eq_ignore_ascii_case("png"))
      != Some(true)
    {
      continue;
    }
    let rel = entry
      .path()
      .strip_prefix(root)
      .map_err(|e| format!("failed to strip prefix {}: {e}", root.display()))?;
    let mut key_path = rel.to_path_buf();
    key_path.set_extension("");
    let key = path_to_forward_slashes(&key_path);
    if key.is_empty() {
      continue;
    }
    map.entry(key).or_insert_with(|| entry.path().to_path_buf());
  }
  Ok(map)
}

fn diff_path_for_name(diff_dir: &Path, name: &str) -> PathBuf {
  let mut parts = name
    .split('/')
    .filter(|p| !p.is_empty())
    .collect::<Vec<_>>();
  let file_stem = parts.pop().unwrap_or("diff");
  let mut path = diff_dir.to_path_buf();
  for part in parts {
    path.push(part);
  }
  path.push(format!("{file_stem}.png"));
  path
}

fn path_to_forward_slashes(path: &Path) -> String {
  path
    .iter()
    .map(|c| c.to_string_lossy())
    .collect::<Vec<_>>()
    .join("/")
}

fn write_json_report(report: &DiffReport, path: &Path) -> Result<(), String> {
  ensure_parent_dir(path)?;
  let json = serde_json::to_string_pretty(report)
    .map_err(|e| format!("failed to serialize JSON report: {e}"))?;
  fs::write(path, json).map_err(|e| format!("failed to write {}: {e}", path.display()))
}

fn write_html_report(report: &DiffReport, path: &Path) -> Result<(), String> {
  ensure_parent_dir(path)?;

  let mut rows = String::new();

  for entry in &report.results {
    let diff_percent = entry
      .metrics
      .map(|m| format!("{:.4}%", m.diff_percentage))
      .unwrap_or_else(|| "-".to_string());
    let perceptual = entry
      .metrics
      .map(|m| format!("{:.4}", m.perceptual_distance))
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

    let error = entry.error.as_deref().unwrap_or_default();
    rows.push_str(&format!(
      "<tr class=\"{}\"><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td class=\"error\">{}</td></tr>",
      entry.status.label(),
      escape_html(&entry.name),
      escape_html(entry.status.label()),
      diff_percent,
      perceptual,
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
    <p><strong>Tolerance:</strong> {tolerance} | <strong>Max diff %:</strong> {max_diff_percent:.4} | <strong>Max perceptual:</strong> {max_perceptual} | <strong>Sort:</strong> {sort_by} {shard}</p>
    <p>Processed {processed} of {discovered} candidates ({matches} exact, {within} within threshold, {diffs} failing, {missing} missing, {errors} errors{skipped}).</p>
    <table>
      <thead>
        <tr>
          <th>Name</th>
          <th>Status</th>
          <th>Diff %</th>
          <th>Perceptual</th>
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
    max_perceptual = report
      .max_perceptual_distance
      .map(|d| format!("{d:.4}"))
      .unwrap_or_else(|| "-".to_string()),
    sort_by = escape_html(report.sort_by.label()),
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

fn sort_entries(entries: &mut [DiffReportEntry], sort_by: SortBy) {
  entries.sort_by(|a, b| {
    let status = b.status.sort_weight().cmp(&a.status.sort_weight());
    if status != std::cmp::Ordering::Equal {
      return status;
    }

    let metric_cmp = match sort_by {
      SortBy::Pixel => b
        .metrics
        .map(|m| m.pixel_diff)
        .unwrap_or(0)
        .cmp(&a.metrics.map(|m| m.pixel_diff).unwrap_or(0)),
      SortBy::Percent => b
        .metrics
        .map(|m| m.diff_percentage)
        .unwrap_or(0.0)
        .partial_cmp(&a.metrics.map(|m| m.diff_percentage).unwrap_or(0.0))
        .unwrap_or(std::cmp::Ordering::Equal),
      SortBy::Perceptual => b
        .metrics
        .map(|m| m.perceptual_distance)
        .unwrap_or(0.0)
        .partial_cmp(&a.metrics.map(|m| m.perceptual_distance).unwrap_or(0.0))
        .unwrap_or(std::cmp::Ordering::Equal),
    };
    if metric_cmp != std::cmp::Ordering::Equal {
      return metric_cmp;
    }

    a.name.cmp(&b.name)
  });
}

fn format_linked_image(label: &str, path: &str) -> String {
  let escaped = escape_html(path);
  format!(
    r#"<div class="thumb"><a href="{p}">{l}</a><br><img src="{p}" loading="lazy"></div>"#,
    p = escaped,
    l = escape_html(label)
  )
}

fn normalize_dir(path: &Path) -> Result<PathBuf, String> {
  let canonical = fs::canonicalize(path).map_err(|e| format!("{}: {e}", path.display()))?;
  if !canonical.is_dir() {
    return Err(format!("{} is not a directory", canonical.display()));
  }
  Ok(canonical)
}

fn normalize_png_file(path: &Path) -> Result<PathBuf, String> {
  let canonical = fs::canonicalize(path).map_err(|e| format!("{}: {e}", path.display()))?;
  if !canonical.is_file() {
    return Err(format!("{} is not a file", canonical.display()));
  }
  if canonical
    .extension()
    .and_then(|ext| ext.to_str())
    .map(|ext| ext.eq_ignore_ascii_case("png"))
    != Some(true)
  {
    return Err(format!("{} is not a PNG file", canonical.display()));
  }
  Ok(canonical)
}
