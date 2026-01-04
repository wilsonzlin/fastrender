use anyhow::{bail, Context, Result};
use clap::{Args, ValueEnum};
use fastrender::image_compare::{compare_png, CompareConfig};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};
use std::time::Instant;

const DEFAULT_FIXTURES_DIR: &str = "tests/pages/fixtures";
const DEFAULT_OUT_DIR: &str = "target/fixture_determinism";
const DEFAULT_VIEWPORT: &str = "1040x1240";
const DEFAULT_DPR: f32 = 1.0;
const DEFAULT_TIMEOUT: u64 = 10;
const DETERMINISM_DIFFS_DIR: &str = "determinism_diffs";

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
#[clap(rename_all = "lowercase")]
enum MediaMode {
  Screen,
  Print,
}

impl MediaMode {
  fn as_cli_value(self) -> &'static str {
    match self {
      Self::Screen => "screen",
      Self::Print => "print",
    }
  }
}

#[derive(Args, Debug)]
pub struct FixtureDeterminismArgs {
  /// Root directory containing offline fixture directories.
  #[arg(long, value_name = "DIR", default_value = DEFAULT_FIXTURES_DIR)]
  pub fixtures_dir: PathBuf,

  /// Only render fixtures matching these names (comma-separated stems).
  #[arg(long, value_delimiter = ',', value_name = "STEM,...")]
  pub fixtures: Option<Vec<String>>,

  /// Only process a deterministic shard of the fixtures (index/total, 0-based).
  #[arg(long, value_parser = crate::parse_shard)]
  pub shard: Option<(usize, usize)>,

  /// Number of parallel fixture renders (forwarded to `render_fixtures --jobs/-j`).
  #[arg(long, short, value_name = "N")]
  pub jobs: Option<usize>,

  /// Repeat the render N times (default: 2).
  #[arg(long, default_value_t = 2, value_name = "N")]
  pub repeat: usize,

  /// Root directory to write output artifacts into.
  #[arg(long, value_name = "DIR", default_value = DEFAULT_OUT_DIR)]
  pub out_dir: PathBuf,

  /// Viewport size as WxH (e.g. 1040x1240; forwarded to render_fixtures).
  #[arg(long, value_parser = crate::parse_viewport, default_value = DEFAULT_VIEWPORT)]
  pub viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset (forwarded to render_fixtures).
  #[arg(long, default_value_t = DEFAULT_DPR)]
  pub dpr: f32,

  /// Media type for evaluating media queries (forwarded to render_fixtures).
  #[arg(long, value_enum, default_value_t = MediaMode::Screen)]
  pub media: MediaMode,

  /// Hard per-fixture timeout in seconds (forwarded to render_fixtures).
  #[arg(long, default_value_t = DEFAULT_TIMEOUT, value_name = "SECS")]
  pub timeout: u64,

  /// Ignore alpha differences when diffing (forwarded to `diff_renders --ignore-alpha`).
  #[arg(long)]
  pub ignore_alpha: bool,

  /// Allow nondeterministic pixel diffs (exit 0 when fixtures differ).
  ///
  /// Render failures and missing/error outputs still fail the command.
  #[arg(long)]
  pub allow_differences: bool,

  /// Skip building `render_fixtures`/`diff_renders` and reuse existing release binaries.
  #[arg(long)]
  pub no_build: bool,
}

#[derive(Debug)]
struct Layout {
  root: PathBuf,
  report_html: PathBuf,
  report_json: PathBuf,
}

impl Layout {
  fn new(root: PathBuf) -> Self {
    Self {
      report_html: root.join("report.html"),
      report_json: root.join("report.json"),
      root,
    }
  }

  fn run_dir(&self, run: usize) -> PathBuf {
    self.root.join(format!("run{run}"))
  }

  fn pair_dir(&self, before: usize, after: usize) -> PathBuf {
    self.root.join(format!("diff_run{before}_run{after}"))
  }
}

#[derive(Deserialize)]
struct DiffReport {
  totals: DiffReportTotals,
  results: Vec<DiffReportEntry>,
}

#[derive(Deserialize, Serialize, Clone, Default)]
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

#[derive(Deserialize, Serialize, Clone)]
struct DiffReportEntry {
  name: String,
  status: EntryStatus,
  before: Option<String>,
  after: Option<String>,
  diff: Option<String>,
  metrics: Option<MetricsSummary>,
  error: Option<String>,
}

#[derive(Deserialize, Serialize, Clone, Copy, PartialEq, Eq)]
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
  fn is_failure(self) -> bool {
    matches!(
      self,
      EntryStatus::Diff
        | EntryStatus::MissingBefore
        | EntryStatus::MissingAfter
        | EntryStatus::Error
    )
  }

  fn sort_weight(self) -> u8 {
    match self {
      EntryStatus::Error => 4,
      EntryStatus::MissingBefore | EntryStatus::MissingAfter => 3,
      EntryStatus::Diff => 2,
      EntryStatus::WithinThreshold => 1,
      EntryStatus::Match => 0,
    }
  }

  fn label(self) -> &'static str {
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

#[derive(Deserialize, Serialize, Clone, Copy)]
struct MetricsSummary {
  pixel_diff: u64,
  total_pixels: u64,
  diff_percentage: f64,
  perceptual_distance: f64,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  max_channel_diff: Option<u8>,
}

#[derive(Serialize)]
struct FixtureDeterminismReport {
  repeat: usize,
  fixtures_dir: String,
  viewport: (u32, u32),
  dpr: f32,
  media: String,
  timeout_secs: u64,
  ignore_alpha: bool,
  runs: Vec<RunInfo>,
  comparisons: Vec<ComparisonInfo>,
  totals: ReportTotals,
  nondeterministic: Vec<FixtureNondeterminism>,
}

#[derive(Serialize)]
struct RunInfo {
  run: usize,
  dir: String,
  success: bool,
  exit_code: Option<i32>,
}

#[derive(Serialize)]
struct ComparisonInfo {
  before_run: usize,
  after_run: usize,
  report_html: String,
  report_json: String,
  totals: DiffReportTotals,
}

#[derive(Serialize)]
struct ReportTotals {
  fixtures: usize,
  nondeterministic: usize,
  render_failures: usize,
  artifact_failures: usize,
}

#[derive(Serialize)]
struct FixtureNondeterminism {
  name: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  artifacts: Option<DeterminismArtifacts>,
  occurrences: Vec<Occurrence>,
  worst: Occurrence,
}

#[derive(Serialize, Clone)]
struct DeterminismArtifacts {
  root: String,
  expected_png: String,
  actual_png: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  diff_png: Option<String>,
  snapshot_before_dir: String,
  snapshot_after_dir: String,
  diff_snapshots_html: String,
  diff_snapshots_json: String,
}

#[derive(Serialize, Clone)]
struct Occurrence {
  before_run: usize,
  after_run: usize,
  status: EntryStatus,
  before: Option<String>,
  after: Option<String>,
  diff: Option<String>,
  metrics: Option<MetricsSummary>,
  error: Option<String>,
  pair_report: String,
}

pub fn run_fixture_determinism(args: FixtureDeterminismArgs) -> Result<()> {
  validate_args(&args)?;
  let repo_root = crate::repo_root();
  let fixtures_root = resolve_repo_path(&repo_root, &args.fixtures_dir);
  if !fixtures_root.is_dir() {
    bail!(
      "fixtures directory does not exist: {}",
      fixtures_root.display()
    );
  }

  let out_root = resolve_repo_path(&repo_root, &args.out_dir);
  let layout = Layout::new(out_root);
  prepare_out_root(&layout.root).context("prepare output directory")?;

  let render_fixtures_exe = render_fixtures_executable(&repo_root);
  let diff_renders_exe = crate::diff_renders_executable(&repo_root);
  let diff_snapshots_exe = diff_snapshots_executable(&repo_root);
  if args.no_build {
    for exe in [&render_fixtures_exe, &diff_renders_exe, &diff_snapshots_exe] {
      if !exe.is_file() {
        bail!(
          "--no-build was set, but required executable does not exist at {}",
          exe.display()
        );
      }
    }
  } else {
    let mut build_cmd = Command::new("cargo");
    build_cmd
      .arg("build")
      .arg("--release")
      .args(["--bin", "render_fixtures"])
      .args(["--bin", "diff_renders"])
      .args(["--bin", "diff_snapshots"])
      .current_dir(&repo_root);
    println!("Building render_fixtures + diff_renders + diff_snapshots...");
    crate::run_command(build_cmd)?;
  }

  println!(
    "Running fixture determinism audit ({repeat} runs)...",
    repeat = args.repeat
  );
  println!("Input:  {}", fixtures_root.display());
  println!("Output: {}", layout.root.display());
  println!(
    "Viewport: {}x{}  DPR: {}  Media: {}  Timeout: {}s",
    args.viewport.0,
    args.viewport.1,
    args.dpr,
    args.media.as_cli_value(),
    args.timeout
  );
  if let Some(fixtures) = &args.fixtures {
    println!("Fixtures: {}", fixtures.join(","));
  }
  if let Some((idx, total)) = args.shard {
    println!("Shard: {idx}/{total}");
  }
  println!();

  let start = Instant::now();
  let mut runs = Vec::new();
  let mut render_failures = 0usize;

  for run in 1..=args.repeat {
    let out_dir = layout.run_dir(run);
    clear_dir(&out_dir).with_context(|| format!("clear {}", out_dir.display()))?;

    let mut cmd =
      build_render_fixtures_command(&render_fixtures_exe, &fixtures_root, &out_dir, &args)?;
    cmd.current_dir(&repo_root);

    println!("Render run {run}/{total}...", total = args.repeat);
    let status = run_command_allow_failure(cmd).context("render_fixtures failed to run")?;
    let success = status.success();
    if !success {
      render_failures += 1;
    }
    runs.push(RunInfo {
      run,
      dir: path_relative_to_root(&layout.root, &out_dir),
      success,
      exit_code: status.code(),
    });
    println!();
  }

  let mut comparisons = Vec::new();
  let mut per_fixture: BTreeMap<String, Vec<Occurrence>> = BTreeMap::new();
  let mut all_fixtures: BTreeSet<String> = BTreeSet::new();

  for before_run in 1..args.repeat {
    let after_run = before_run + 1;
    let before_dir = layout.run_dir(before_run);
    let after_dir = layout.run_dir(after_run);
    let pair_dir = layout.pair_dir(before_run, after_run);
    clear_dir(&pair_dir).with_context(|| format!("clear {}", pair_dir.display()))?;

    let pair_html = pair_dir.join("report.html");
    let pair_json = pair_dir.join("report.json");
    let cmd = build_diff_renders_command(
      &diff_renders_exe,
      &before_dir,
      &after_dir,
      &pair_json,
      &pair_html,
      args.ignore_alpha,
    )?;
    println!("Diffing run{before_run} → run{after_run}...");
    run_diff_renders_allowing_differences(cmd, &pair_html)?;

    let report =
      parse_diff_report(&pair_json).with_context(|| format!("parse {}", pair_json.display()))?;
    comparisons.push(ComparisonInfo {
      before_run,
      after_run,
      report_html: path_relative_to_root(&layout.root, &pair_html),
      report_json: path_relative_to_root(&layout.root, &pair_json),
      totals: report.totals.clone(),
    });

    for entry in &report.results {
      all_fixtures.insert(entry.name.clone());
      if !entry.status.is_failure() {
        continue;
      }

      let occurrence = Occurrence {
        before_run,
        after_run,
        status: entry.status,
        before: entry
          .before
          .as_deref()
          .map(|p| resolve_path_from_pair(&layout.root, &pair_dir, p)),
        after: entry
          .after
          .as_deref()
          .map(|p| resolve_path_from_pair(&layout.root, &pair_dir, p)),
        diff: entry
          .diff
          .as_deref()
          .map(|p| resolve_path_from_pair(&layout.root, &pair_dir, p)),
        metrics: entry.metrics,
        error: entry.error.clone(),
        pair_report: path_relative_to_root(&layout.root, &pair_html),
      };

      per_fixture
        .entry(entry.name.clone())
        .or_default()
        .push(occurrence);
    }

    println!();
  }

  let mut nondeterministic = per_fixture
    .into_iter()
    .map(|(name, mut occurrences)| {
      occurrences.sort_by(|a, b| {
        let status = b.status.sort_weight().cmp(&a.status.sort_weight());
        if status != std::cmp::Ordering::Equal {
          return status;
        }
        let metric = b
          .metrics
          .map(|m| m.diff_percentage)
          .unwrap_or(f64::INFINITY)
          .partial_cmp(
            &a.metrics
              .map(|m| m.diff_percentage)
              .unwrap_or(f64::INFINITY),
          )
          .unwrap_or(std::cmp::Ordering::Equal);
        if metric != std::cmp::Ordering::Equal {
          return metric;
        }
        a.before_run.cmp(&b.before_run)
      });
      let worst = occurrences[0].clone();
      FixtureNondeterminism {
        name,
        artifacts: None,
        occurrences,
        worst,
      }
    })
    .collect::<Vec<_>>();

  nondeterministic.sort_by(|a, b| {
    let status = b
      .worst
      .status
      .sort_weight()
      .cmp(&a.worst.status.sort_weight());
    if status != std::cmp::Ordering::Equal {
      return status;
    }
    let metric = b
      .worst
      .metrics
      .map(|m| m.diff_percentage)
      .unwrap_or(f64::INFINITY)
      .partial_cmp(
        &a.worst
          .metrics
          .map(|m| m.diff_percentage)
          .unwrap_or(f64::INFINITY),
      )
      .unwrap_or(std::cmp::Ordering::Equal);
    if metric != std::cmp::Ordering::Equal {
      return metric;
    }
    a.name.cmp(&b.name)
  });

  let determinism_root = crate::cargo_target_dir(&repo_root).join(DETERMINISM_DIFFS_DIR);
  let mut artifact_failures = Vec::new();
  for fixture in &mut nondeterministic {
    if let Err(err) = capture_artifacts_for_fixture(
      &repo_root,
      &fixtures_root,
      &render_fixtures_exe,
      &diff_snapshots_exe,
      &determinism_root,
      fixture,
      &layout,
      &args,
    ) {
      artifact_failures.push(format!("{}: {err}", fixture.name));
    }
  }

  let report = FixtureDeterminismReport {
    repeat: args.repeat,
    fixtures_dir: fixtures_root.display().to_string(),
    viewport: args.viewport,
    dpr: args.dpr,
    media: args.media.as_cli_value().to_string(),
    timeout_secs: args.timeout,
    ignore_alpha: args.ignore_alpha,
    runs,
    comparisons,
    totals: ReportTotals {
      fixtures: all_fixtures.len(),
      nondeterministic: nondeterministic.len(),
      render_failures,
      artifact_failures: artifact_failures.len(),
    },
    nondeterministic,
  };

  write_json_pretty(&layout.report_json, &report).context("write report.json")?;
  fs::write(&layout.report_html, render_html(&layout, &report))
    .with_context(|| format!("write {}", layout.report_html.display()))?;

  println!(
    "Done in {:.1}s. {} nondeterministic fixture(s) ({} render run(s) failed, {} artifact capture failure(s)).",
    start.elapsed().as_secs_f64(),
    report.totals.nondeterministic,
    report.totals.render_failures,
    report.totals.artifact_failures
  );
  println!("HTML report: {}", layout.report_html.display());
  println!("JSON report: {}", layout.report_json.display());

  if !artifact_failures.is_empty() {
    println!();
    println!("Artifact capture failures:");
    for err in &artifact_failures {
      println!("- {err}");
    }
  }

  if report.totals.nondeterministic > 0 {
    println!();
    println!("Nondeterministic fixtures:");
    for fixture in &report.nondeterministic {
      let worst = &fixture.worst;
      let diff_percent = worst
        .metrics
        .map(|m| format!("{:.4}%", m.diff_percentage))
        .unwrap_or_else(|| "-".to_string());
      let perceptual = worst
        .metrics
        .map(|m| format!("{:.4}", m.perceptual_distance))
        .unwrap_or_else(|| "-".to_string());
      let max_channel_diff = worst
        .metrics
        .and_then(|m| m.max_channel_diff)
        .map(|v| v.to_string())
        .unwrap_or_else(|| "-".to_string());
      println!(
        "- {} ({before_run}->{after_run}) status={} diff={} max_delta={} perceptual={} before={} after={} diff={}",
        fixture.name,
        worst.status.label(),
        diff_percent,
        max_channel_diff,
        perceptual,
        worst.before.as_deref().unwrap_or("-"),
        worst.after.as_deref().unwrap_or("-"),
        worst.diff.as_deref().unwrap_or("-"),
        before_run = worst.before_run,
        after_run = worst.after_run
      );
      if let Some(artifacts) = fixture.artifacts.as_ref() {
        println!("  Determinism artifacts: {}", artifacts.root);
        println!("    expected: {}", artifacts.expected_png);
        println!("    actual:   {}", artifacts.actual_png);
        if let Some(diff) = artifacts.diff_png.as_deref() {
          println!("    diff:     {diff}");
        }
        println!("    snapshot run1: {}", artifacts.snapshot_before_dir);
        println!("    snapshot run2: {}", artifacts.snapshot_after_dir);
        println!("    diff_snapshots: {}", artifacts.diff_snapshots_html);
      }
    }
  }

  if report.totals.render_failures > 0 {
    bail!(
      "{} render run(s) failed; report: {}",
      report.totals.render_failures,
      layout.report_html.display()
    );
  }

  let has_hard_failures = report.nondeterministic.iter().any(|fixture| {
    fixture.occurrences.iter().any(|occurrence| {
      matches!(
        occurrence.status,
        EntryStatus::MissingBefore | EntryStatus::MissingAfter | EntryStatus::Error
      )
    })
  });

  let has_diffs = report.nondeterministic.iter().any(|fixture| {
    fixture
      .occurrences
      .iter()
      .any(|occurrence| occurrence.status == EntryStatus::Diff)
  });

  // `--allow-differences` only suppresses exit failure for pixel diffs. Missing/error entries are
  // always fatal because they imply the audit couldn't reliably compare runs.
  let should_fail = has_hard_failures
    || (!args.allow_differences && (has_diffs || report.totals.artifact_failures > 0));
  if should_fail {
    let mut message = if has_hard_failures {
      format!(
        "fixture determinism audit encountered missing/error outputs; report: {}",
        layout.report_html.display()
      )
    } else {
      format!(
        "fixture determinism audit found nondeterministic diffs; report: {}",
        layout.report_html.display()
      )
    };

    if report.totals.artifact_failures > 0 {
      message.push_str("\n\nArtifact capture failures:");
      for err in &artifact_failures {
        message.push_str(&format!("\n- {err}"));
      }
    }

    if report.totals.nondeterministic > 0 {
      message.push_str("\n\nPer-fixture determinism artifacts:");
      for fixture in &report.nondeterministic {
        if let Some(artifacts) = fixture.artifacts.as_ref() {
          message.push_str(&format!(
            "\n- {name}\n  expected: {expected}\n  actual:   {actual}\n  diff:     {diff}\n  snapshot run1: {snap1}\n  snapshot run2: {snap2}\n  diff_snapshots: {stage}",
            name = fixture.name,
            expected = artifacts.expected_png,
            actual = artifacts.actual_png,
            diff = artifacts.diff_png.as_deref().unwrap_or("-"),
            snap1 = artifacts.snapshot_before_dir,
            snap2 = artifacts.snapshot_after_dir,
            stage = artifacts.diff_snapshots_html,
          ));
        } else {
          let error = fixture.worst.error.as_deref().unwrap_or("-");
          message.push_str(&format!(
            "\n- {name}\n  status: {status}\n  pair report: {pair}\n  error: {error}",
            name = fixture.name,
            status = fixture.worst.status.label(),
            pair = fixture.worst.pair_report,
            error = error,
          ));
        }
      }
    }
    bail!("{message}");
  }

  Ok(())
}

fn validate_args(args: &FixtureDeterminismArgs) -> Result<()> {
  if args.repeat < 2 {
    bail!("--repeat must be >= 2");
  }
  if args.dpr <= 0.0 || !args.dpr.is_finite() {
    bail!("--dpr must be a positive, finite number");
  }
  if let Some(jobs) = args.jobs {
    if jobs == 0 {
      bail!("--jobs must be > 0");
    }
  }
  if args.timeout == 0 {
    bail!("--timeout must be > 0");
  }
  Ok(())
}

fn resolve_repo_path(repo_root: &Path, path: &Path) -> PathBuf {
  if path.is_absolute() {
    path.to_path_buf()
  } else {
    repo_root.join(path)
  }
}

fn prepare_out_root(root: &Path) -> Result<()> {
  fs::create_dir_all(root).with_context(|| format!("create {}", root.display()))?;

  for entry in fs::read_dir(root).with_context(|| format!("read {}", root.display()))? {
    let entry = entry.context("read output dir entry")?;
    let file_type = entry.file_type().context("read output dir entry type")?;
    let name = entry.file_name().to_string_lossy().to_string();
    let path = entry.path();

    if file_type.is_file() && (name == "report.json" || name == "report.html") {
      let _ = fs::remove_file(&path);
      continue;
    }

    if !file_type.is_dir() {
      continue;
    }

    let is_run_dir = name.starts_with("run") && name[3..].chars().all(|c| c.is_ascii_digit());
    let is_diff_dir = name.starts_with("diff_run");
    if is_run_dir || is_diff_dir {
      let _ = fs::remove_dir_all(&path);
    }
  }

  Ok(())
}

fn clear_dir(path: &Path) -> Result<()> {
  if path.exists() {
    fs::remove_dir_all(path).with_context(|| format!("remove {}", path.display()))?;
  }
  fs::create_dir_all(path).with_context(|| format!("create {}", path.display()))?;
  Ok(())
}

fn render_fixtures_executable(repo_root: &Path) -> PathBuf {
  crate::cargo_target_dir(repo_root)
    .join("release")
    .join(format!("render_fixtures{}", std::env::consts::EXE_SUFFIX))
}

fn diff_snapshots_executable(repo_root: &Path) -> PathBuf {
  crate::cargo_target_dir(repo_root)
    .join("release")
    .join(format!("diff_snapshots{}", std::env::consts::EXE_SUFFIX))
}

fn build_render_fixtures_command(
  render_fixtures_exe: &Path,
  fixtures_root: &Path,
  out_dir: &Path,
  args: &FixtureDeterminismArgs,
) -> Result<Command> {
  let mut cmd = Command::new(render_fixtures_exe);
  // Keep renders deterministic across machines.
  cmd.env("FASTR_USE_BUNDLED_FONTS", "1");
  cmd.arg("--fixtures-dir").arg(fixtures_root);
  cmd.arg("--out-dir").arg(out_dir);
  cmd
    .arg("--viewport")
    .arg(format!("{}x{}", args.viewport.0, args.viewport.1));
  cmd.arg("--dpr").arg(args.dpr.to_string());
  cmd.arg("--media").arg(args.media.as_cli_value());
  cmd.arg("--timeout").arg(args.timeout.to_string());
  if let Some(jobs) = args.jobs {
    cmd.arg("--jobs").arg(jobs.to_string());
  }
  if let Some(fixtures) = &args.fixtures {
    cmd.arg("--fixtures").arg(fixtures.join(","));
  }
  if let Some((idx, total)) = args.shard {
    cmd.arg("--shard").arg(format!("{idx}/{total}"));
  }
  Ok(cmd)
}

fn build_render_fixtures_snapshot_command(
  render_fixtures_exe: &Path,
  fixtures_root: &Path,
  out_dir: &Path,
  args: &FixtureDeterminismArgs,
  fixture: &str,
) -> Result<Command> {
  // Do not forward `--fixtures` / `--shard` from the outer determinism harness: we always want to
  // re-run exactly the single nondeterministic fixture.
  let mut cmd = Command::new(render_fixtures_exe);
  cmd.env("FASTR_USE_BUNDLED_FONTS", "1");
  cmd.arg("--fixtures-dir").arg(fixtures_root);
  cmd.arg("--out-dir").arg(out_dir);
  cmd
    .arg("--viewport")
    .arg(format!("{}x{}", args.viewport.0, args.viewport.1));
  cmd.arg("--dpr").arg(args.dpr.to_string());
  cmd.arg("--media").arg(args.media.as_cli_value());
  cmd.arg("--timeout").arg(args.timeout.to_string());
  if let Some(jobs) = args.jobs {
    cmd.arg("--jobs").arg(jobs.to_string());
  }
  cmd.arg("--fixtures").arg(fixture);
  cmd.arg("--write-snapshot");
  Ok(cmd)
}

fn build_diff_renders_command(
  diff_renders_exe: &Path,
  before_dir: &Path,
  after_dir: &Path,
  json_path: &Path,
  html_path: &Path,
  ignore_alpha: bool,
) -> Result<Command> {
  let mut cmd = Command::new(diff_renders_exe);
  cmd
    .arg("--before")
    .arg(before_dir)
    .arg("--after")
    .arg(after_dir)
    .arg("--tolerance")
    .arg("0")
    .arg("--max-diff-percent")
    .arg("0")
    .arg("--sort-by")
    .arg("percent")
    .arg("--json")
    .arg(json_path)
    .arg("--html")
    .arg(html_path);
  if ignore_alpha {
    cmd.arg("--ignore-alpha");
  }
  Ok(cmd)
}

fn build_diff_snapshots_command(
  diff_snapshots_exe: &Path,
  before_dir: &Path,
  after_dir: &Path,
  json_path: &Path,
  html_path: &Path,
) -> Result<Command> {
  let mut cmd = Command::new(diff_snapshots_exe);
  cmd
    .arg("--before")
    .arg(before_dir)
    .arg("--after")
    .arg(after_dir)
    .arg("--json")
    .arg(json_path)
    .arg("--html")
    .arg(html_path);
  Ok(cmd)
}

fn run_command_allow_failure(mut cmd: Command) -> Result<ExitStatus> {
  crate::print_command(&cmd);
  cmd
    .status()
    .with_context(|| format!("failed to run {:?}", cmd.get_program()))
}

fn run_diff_renders_allowing_differences(mut cmd: Command, report_html: &Path) -> Result<()> {
  crate::print_command(&cmd);
  let output = cmd
    .output()
    .with_context(|| format!("failed to run {:?}", cmd.get_program()))?;

  if !output.stdout.is_empty() {
    print!("{}", String::from_utf8_lossy(&output.stdout));
  }
  if !output.stderr.is_empty() {
    eprint!("{}", String::from_utf8_lossy(&output.stderr));
  }

  if output.status.success() {
    return Ok(());
  }

  if output.status.code() == Some(1) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    if stderr.trim_start().starts_with("error:") {
      bail!("diff_renders failed (see output above)");
    }
    eprintln!(
      "diff_renders reported differences; report: {}",
      report_html.display()
    );
    return Ok(());
  }

  bail!("diff_renders failed with status {}", output.status);
}

fn parse_diff_report(path: &Path) -> Result<DiffReport> {
  let json = fs::read_to_string(path).with_context(|| format!("read {}", path.display()))?;
  serde_json::from_str(&json).context("parse diff_renders report json")
}

fn path_relative_to_root(root: &Path, path: &Path) -> String {
  let rendered = path
    .strip_prefix(root)
    .unwrap_or(path)
    .display()
    .to_string();
  if cfg!(windows) {
    rendered.replace('\\', "/")
  } else {
    rendered
  }
}

fn resolve_path_from_pair(root: &Path, pair_dir: &Path, rel: &str) -> String {
  let rel_path = Path::new(rel);
  let joined = if rel_path.is_absolute() {
    rel_path.to_path_buf()
  } else {
    pair_dir.join(rel_path)
  };

  let resolved = fs::canonicalize(&joined).unwrap_or(joined);
  path_relative_to_root(root, &resolved)
}

fn escape_html(input: &str) -> String {
  input
    .replace('&', "&amp;")
    .replace('<', "&lt;")
    .replace('>', "&gt;")
    .replace('"', "&quot;")
    .replace('\'', "&#39;")
}

fn format_linked_image(label: &str, path: &str) -> String {
  let escaped = escape_html(path);
  format!(
    r#"<div class="thumb"><a href="{p}">{l}</a><br><img src="{p}" loading="lazy"></div>"#,
    p = escaped,
    l = escape_html(label)
  )
}

fn render_html(layout: &Layout, report: &FixtureDeterminismReport) -> String {
  let mut rows = String::new();

  for fixture in &report.nondeterministic {
    let worst = &fixture.worst;

    let occurrences = fixture
      .occurrences
      .iter()
      .map(|o| format!("{}→{}", o.before_run, o.after_run))
      .collect::<Vec<_>>()
      .join(", ");

    let diff_percent = worst
      .metrics
      .map(|m| format!("{:.4}%", m.diff_percentage))
      .unwrap_or_else(|| "-".to_string());
    let perceptual = worst
      .metrics
      .map(|m| format!("{:.4}", m.perceptual_distance))
      .unwrap_or_else(|| "-".to_string());
    let pixel_diff = worst
      .metrics
      .map(|m| m.pixel_diff.to_string())
      .unwrap_or_else(|| "-".to_string());
    let max_channel_diff = worst
      .metrics
      .and_then(|m| m.max_channel_diff)
      .map(|v| v.to_string())
      .unwrap_or_else(|| "-".to_string());
    let total_pixels = worst
      .metrics
      .map(|m| m.total_pixels.to_string())
      .unwrap_or_else(|| "-".to_string());

    let before_cell = worst
      .before
      .as_ref()
      .map(|p| format_linked_image("Before", p))
      .unwrap_or_else(|| "-".to_string());
    let after_cell = worst
      .after
      .as_ref()
      .map(|p| format_linked_image("After", p))
      .unwrap_or_else(|| "-".to_string());
    let diff_cell = worst
      .diff
      .as_ref()
      .map(|p| format_linked_image("Diff", p))
      .unwrap_or_else(|| "-".to_string());
    let after_and_diff = if worst.diff.is_some() {
      format!("{after_cell}{diff_cell}")
    } else {
      after_cell
    };

    let pair_link = format!(
      r#"<a href="{href}">pair report</a>"#,
      href = escape_html(&worst.pair_report)
    );

    let error = worst.error.as_deref().unwrap_or_default();
    rows.push_str(&format!(
      "<tr class=\"{}\"><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{}</td><td class=\"error\">{}</td></tr>",
      worst.status.label(),
      escape_html(&fixture.name),
      escape_html(worst.status.label()),
      escape_html(&occurrences),
      diff_percent,
      perceptual,
      pixel_diff,
      max_channel_diff,
      total_pixels,
      before_cell,
      after_and_diff,
      pair_link,
      escape_html(error),
    ));
  }

  let comparisons = report
    .comparisons
    .iter()
    .map(|c| {
      format!(
        r#"<li>run{before} → run{after}: <a href="{href}">{href}</a> ({diffs} diffs, {missing} missing, {errors} errors)</li>"#,
        before = c.before_run,
        after = c.after_run,
        href = escape_html(&c.report_html),
        diffs = c.totals.differences,
        missing = c.totals.missing,
        errors = c.totals.errors
      )
    })
    .collect::<Vec<_>>()
    .join("\n");

  format!(
    r#"<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Fixture determinism report</title>
    <style>
      body {{ font-family: sans-serif; margin: 20px; }}
      table {{ border-collapse: collapse; width: 100%; }}
      th, td {{ border: 1px solid #ddd; padding: 6px; vertical-align: top; }}
      th {{ background: #f3f3f3; position: sticky; top: 0; }}
      tr.diff {{ background: #fff8f8; }}
      tr.missing-before, tr.missing-after, tr.error {{ background: #fff0f0; }}
      .thumb img {{ max-width: 320px; max-height: 240px; display: block; }}
      .error {{ color: #b00020; }}
    </style>
  </head>
  <body>
    <h1>Fixture determinism report</h1>
    <p><strong>Output:</strong> {out_root}</p>
    <p><strong>Input:</strong> {fixtures_dir}</p>
    <p><strong>Repeat:</strong> {repeat} | <strong>Viewport:</strong> {vw}x{vh} | <strong>DPR:</strong> {dpr} | <strong>Media:</strong> {media} | <strong>Timeout:</strong> {timeout}s | <strong>Ignore alpha:</strong> {ignore_alpha}</p>
    <p>{fixtures} fixture(s) discovered across comparisons; {nondet} nondeterministic, {render_failures} render run(s) failed, {artifact_failures} artifact capture failure(s).</p>
    <h2>Comparison reports</h2>
    <ul>
      {comparisons}
    </ul>
    <h2>Nondeterministic fixtures</h2>
    <table>
      <thead>
        <tr>
          <th>Fixture</th>
          <th>Status</th>
          <th>Occurrences</th>
          <th>Diff %</th>
          <th>Perceptual</th>
          <th>Pixel diff</th>
          <th>Max Δ</th>
          <th>Total pixels</th>
          <th>Before</th>
          <th>After | Diff</th>
          <th>Pair report</th>
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
    out_root = escape_html(&layout.root.display().to_string()),
    fixtures_dir = escape_html(&report.fixtures_dir),
    repeat = report.repeat,
    vw = report.viewport.0,
    vh = report.viewport.1,
    dpr = report.dpr,
    media = escape_html(&report.media),
    timeout = report.timeout_secs,
    ignore_alpha = if report.ignore_alpha { "yes" } else { "no" },
    fixtures = report.totals.fixtures,
    nondet = report.totals.nondeterministic,
    render_failures = report.totals.render_failures,
    artifact_failures = report.totals.artifact_failures,
    comparisons = comparisons,
    rows = rows,
  )
}

fn write_json_pretty(path: &Path, value: &impl Serialize) -> Result<()> {
  let json = serde_json::to_string_pretty(value).context("serialize json")?;
  fs::write(path, json).with_context(|| format!("write {}", path.display()))?;
  Ok(())
}

fn capture_artifacts_for_fixture(
  repo_root: &Path,
  fixtures_root: &Path,
  render_fixtures_exe: &Path,
  diff_snapshots_exe: &Path,
  determinism_root: &Path,
  fixture: &mut FixtureNondeterminism,
  layout: &Layout,
  args: &FixtureDeterminismArgs,
) -> Result<()> {
  let Some(diff_occurrence) = fixture
    .occurrences
    .iter()
    .find(|occ| occ.status == EntryStatus::Diff)
    .cloned()
  else {
    // Snapshot diffs are only actionable when both runs produced PNGs that differ.
    return Ok(());
  };

  let name = fixture.name.as_str();
  let out_root = determinism_root.join(name);
  clear_dir(&out_root).with_context(|| format!("clear {}", out_root.display()))?;

  // --- Pixel artifacts (expected/actual/diff) ---
  let before_png = layout
    .run_dir(diff_occurrence.before_run)
    .join(format!("{name}.png"));
  let after_png = layout
    .run_dir(diff_occurrence.after_run)
    .join(format!("{name}.png"));

  let expected_png_path = out_root.join(format!("{name}_expected.png"));
  let actual_png_path = out_root.join(format!("{name}_actual.png"));
  fs::copy(&before_png, &expected_png_path).with_context(|| {
    format!(
      "copy expected png from {} to {}",
      before_png.display(),
      expected_png_path.display()
    )
  })?;
  fs::copy(&after_png, &actual_png_path).with_context(|| {
    format!(
      "copy actual png from {} to {}",
      after_png.display(),
      actual_png_path.display()
    )
  })?;

  let expected_bytes = fs::read(&expected_png_path)
    .with_context(|| format!("read {}", expected_png_path.display()))?;
  let actual_bytes =
    fs::read(&actual_png_path).with_context(|| format!("read {}", actual_png_path.display()))?;

  let compare_config = if args.ignore_alpha {
    CompareConfig::strict().with_compare_alpha(false)
  } else {
    CompareConfig::strict()
  };
  let diff = compare_png(&actual_bytes, &expected_bytes, &compare_config).context("diff PNGs")?;

  let diff_png_path = out_root.join(format!("{name}_diff.png"));
  let diff_written = match diff.diff_png().context("encode diff png")? {
    Some(bytes) => {
      fs::write(&diff_png_path, bytes).with_context(|| format!("write {}", diff_png_path.display()))?;
      Some(diff_png_path)
    }
    None => None,
  };

  // --- Snapshot capture (re-run with --write-snapshot) ---
  let run1_out = out_root.join("run1");
  let run2_out = out_root.join("run2");
  clear_dir(&run1_out).with_context(|| format!("clear {}", run1_out.display()))?;
  clear_dir(&run2_out).with_context(|| format!("clear {}", run2_out.display()))?;

  for (idx, out_dir) in [(1usize, &run1_out), (2usize, &run2_out)] {
    let mut cmd =
      build_render_fixtures_snapshot_command(render_fixtures_exe, fixtures_root, out_dir, args, name)?;
    cmd.current_dir(repo_root);
    println!("Capturing snapshot for {name} (run {idx}/2)...");
    let status = run_command_allow_failure(cmd).context("render_fixtures failed to run")?;
    if !status.success() {
      bail!("snapshot capture failed for {name} (run {idx}/2) with status {status}");
    }
  }

  let snapshot_before_dir = run1_out.join(name);
  let snapshot_after_dir = run2_out.join(name);

  // For nicer diff_snapshots HTML output, ensure the directories contain a PNG that matches the
  // exact pixel mismatch captured above.
  fs::copy(&expected_png_path, snapshot_before_dir.join("render.png")).with_context(|| {
    format!(
      "copy {} to {}",
      expected_png_path.display(),
      snapshot_before_dir.join("render.png").display()
    )
  })?;
  fs::copy(&actual_png_path, snapshot_after_dir.join("render.png")).with_context(|| {
    format!(
      "copy {} to {}",
      actual_png_path.display(),
      snapshot_after_dir.join("render.png").display()
    )
  })?;

  for required in [
    snapshot_before_dir.join("snapshot.json"),
    snapshot_before_dir.join("diagnostics.json"),
    snapshot_after_dir.join("snapshot.json"),
    snapshot_after_dir.join("diagnostics.json"),
  ] {
    if !required.is_file() {
      bail!("missing snapshot output {}", required.display());
    }
  }

  // --- Stage-level snapshot diff ---
  let diff_snapshots_json = out_root.join("diff_snapshots.json");
  let diff_snapshots_html = out_root.join("diff_snapshots.html");
  let mut cmd = build_diff_snapshots_command(
    diff_snapshots_exe,
    &snapshot_before_dir,
    &snapshot_after_dir,
    &diff_snapshots_json,
    &diff_snapshots_html,
  )?;
  cmd.current_dir(repo_root);
  println!("Generating diff_snapshots report for {name}...");
  crate::run_command(cmd).context("diff_snapshots failed")?;

  fixture.artifacts = Some(DeterminismArtifacts {
    root: path_relative_to_root(repo_root, &out_root),
    expected_png: path_relative_to_root(repo_root, &expected_png_path),
    actual_png: path_relative_to_root(repo_root, &actual_png_path),
    diff_png: diff_written.as_deref().map(|p| path_relative_to_root(repo_root, p)),
    snapshot_before_dir: path_relative_to_root(repo_root, &snapshot_before_dir),
    snapshot_after_dir: path_relative_to_root(repo_root, &snapshot_after_dir),
    diff_snapshots_html: path_relative_to_root(repo_root, &diff_snapshots_html),
    diff_snapshots_json: path_relative_to_root(repo_root, &diff_snapshots_json),
  });

  Ok(())
}
