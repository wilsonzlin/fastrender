//! Pageset progress runner
//!
//! Updates per-page progress artifacts under `progress/pages/<stem>.json` by rendering the
//! cached HTML in `fetches/html/` with:
//! - panic containment (per-page worker process), and
//! - hard timeouts (parent kills runaway workers).
//!
//! This is designed as a "ruthless triage" tool: cheap to run, safe against hangs, and
//! produces committed, comparable metrics.
#![allow(clippy::too_many_lines)]
#![allow(clippy::struct_excessive_bools)]

mod common;

use clap::{ArgAction, Args, Parser, Subcommand, ValueEnum};
use common::args::{
  parse_shard, CompatArgs, CompatProfileArg, DiskCacheArgs, DomCompatArg, LayoutParallelArgs,
  LayoutParallelModeArg, ResourceAccessArgs,
};
use common::render_pipeline::{
  build_render_configs, follow_client_redirects, format_error_with_chain, log_layout_parallelism,
  read_cached_document, render_document, render_document_with_artifacts, PreparedDocument,
  RenderConfigBundle, RenderSurface,
};
use fastrender::api::{
  CascadeDiagnostics, DiagnosticsLevel, LayoutDiagnostics, PaintDiagnostics, RenderArtifactRequest,
  RenderArtifacts, RenderCounts, RenderDiagnostics, RenderStats, ResourceDiagnostics, ResourceKind,
};
use fastrender::debug::snapshot;
use fastrender::error::{RenderError, RenderStage};
use fastrender::pageset::{
  pageset_entries, pageset_stem, PagesetEntry, PagesetFilter, CACHE_HTML_DIR,
};
use fastrender::render_control::{record_stage, set_stage_listener, StageHeartbeat};
use fastrender::resource::normalize_user_agent_for_log;
use fastrender::resource::parse_cached_html_meta;
use fastrender::resource::CacheStalePolicy;
#[cfg(not(feature = "disk_cache"))]
use fastrender::resource::CachingFetcher;
use fastrender::resource::CachingFetcherConfig;
#[cfg(feature = "disk_cache")]
use fastrender::resource::DiskCachingFetcher;
use fastrender::resource::HttpFetcher;
use fastrender::resource::ResourceFetcher;
use fastrender::resource::DEFAULT_ACCEPT_LANGUAGE;
use fastrender::resource::DEFAULT_USER_AGENT;
use fastrender::text::font_db::FontConfig;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fs::{self, File, OpenOptions};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Stdio};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::{Duration, Instant};
use tempfile::NamedTempFile;

#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;

const ASSET_DIR: &str = "fetches/assets";
const DEFAULT_PROGRESS_DIR: &str = "progress/pages";
const DEFAULT_LOG_DIR: &str = "target/pageset/logs";
const DEFAULT_TRACE_DIR: &str = "target/pageset/traces";
const DEFAULT_TRACE_PROGRESS_DIR: &str = "target/pageset/trace-progress";
const DEFAULT_CASCADE_PROGRESS_DIR: &str = "target/pageset/cascade-progress";
const DEFAULT_DUMP_DIR: &str = "target/pageset/dumps";
// Treat traces smaller than this as likely incomplete/partial.
const MIN_TRACE_BYTES: u64 = 4096;
// Keep progress notes compact; the full error chain lives in the per-page log file.
const PROGRESS_NOTE_MAX_CHARS: usize = 240;
// Cap quoted excerpts (e.g., parse/shaping text payloads) before the overall limit.
const PROGRESS_NOTE_QUOTED_EXCERPT_MAX_CHARS: usize = 120;
const PROGRESS_NOTE_ELLIPSIS: char = '…';
// Allow a small buffer past the cooperative render timeout for per-request fetch deadlines.
const FETCH_TIMEOUT_SLACK_MS: u64 = 100;

#[derive(Parser, Debug)]
#[command(
  name = "pageset_progress",
  version,
  about = "Render cached pages and write progress/pages/*.json"
)]
struct Cli {
  #[command(subcommand)]
  command: CommandKind,
}

#[derive(Subcommand, Debug)]
enum CommandKind {
  /// Ensure `progress/pages/*.json` exists for every page in the pageset.
  Sync(SyncArgs),
  /// Apply schema migrations to existing `progress/pages/*.json` artifacts without rendering.
  Migrate(MigrateArgs),
  /// Render cached pages and update `progress/pages/*.json`.
  Run(RunArgs),
  /// Summarize existing `progress/pages/*.json` artifacts.
  Report(ReportArgs),
  #[command(hide = true)]
  Worker(WorkerArgs),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
enum DiagnosticsArg {
  None,
  Basic,
  Verbose,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
enum DumpLevel {
  Summary,
  Full,
}

impl DumpLevel {
  fn as_str(&self) -> &'static str {
    match self {
      DumpLevel::Summary => "summary",
      DumpLevel::Full => "full",
    }
  }

  fn to_request(self) -> RenderArtifactRequest {
    match self {
      DumpLevel::Summary => RenderArtifactRequest::summary(),
      DumpLevel::Full => RenderArtifactRequest::full(),
    }
  }

  fn combine(current: Option<Self>, next: Self) -> Self {
    match (current, next) {
      (Some(DumpLevel::Full), _) | (_, DumpLevel::Full) => DumpLevel::Full,
      (Some(DumpLevel::Summary), _) => DumpLevel::Summary,
      (None, level) => level,
    }
  }
}

impl DiagnosticsArg {
  fn to_level(self) -> DiagnosticsLevel {
    match self {
      DiagnosticsArg::None => DiagnosticsLevel::None,
      DiagnosticsArg::Basic => DiagnosticsLevel::Basic,
      DiagnosticsArg::Verbose => DiagnosticsLevel::Verbose,
    }
  }
}

#[derive(Args, Debug, Clone)]
struct FontSourceArgs {
  /// Use bundled font fixtures instead of scanning system fonts
  #[arg(long)]
  bundled_fonts: bool,

  /// Additional font directories to load (repeatable)
  #[arg(long = "font-dir", value_name = "DIR")]
  font_dir: Vec<PathBuf>,
}

impl FontSourceArgs {
  fn to_font_config(&self) -> Option<FontConfig> {
    if self.bundled_fonts {
      let mut config = FontConfig::bundled_only();
      for dir in &self.font_dir {
        config = config.add_font_dir(dir);
      }
      return Some(config);
    }

    if !self.font_dir.is_empty() {
      let mut config = FontConfig::new()
        .with_system_fonts(false)
        .with_bundled_fonts(false);
      for dir in &self.font_dir {
        config = config.add_font_dir(dir);
      }
      return Some(config);
    }

    None
  }
}

#[derive(Args, Debug)]
struct SyncArgs {
  /// Directory to write committed progress artifacts into
  #[arg(long, default_value = DEFAULT_PROGRESS_DIR)]
  progress_dir: PathBuf,

  /// Directory containing cached HTML (used to flag missing caches)
  #[arg(long, default_value = CACHE_HTML_DIR)]
  html_dir: PathBuf,

  /// Remove progress files that are no longer part of the pageset
  #[arg(long)]
  prune: bool,
}

#[derive(Args, Debug)]
struct MigrateArgs {
  /// Directory containing `progress/pages/*.json`
  #[arg(long, default_value = DEFAULT_PROGRESS_DIR)]
  progress_dir: PathBuf,

  /// Directory containing cached HTML (used only for normalizing missing-cache placeholders)
  #[arg(long, default_value = CACHE_HTML_DIR)]
  html_dir: PathBuf,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, ValueEnum)]
enum ProgressStatusArg {
  Ok,
  Timeout,
  Panic,
  Error,
}

#[derive(Args, Debug, Default, Clone)]
struct ProgressSelectionArgs {
  /// Select pages from an existing `progress/pages` directory instead of the cache listing (filters combine with AND unless `--union` is set)
  #[arg(long, value_name = "DIR")]
  from_progress: Option<PathBuf>,

  /// Filter to failures (timeout|panic|error)
  #[arg(long, requires = "from_progress")]
  only_failures: bool,

  /// Filter to specific statuses (comma-separated)
  #[arg(long, value_enum, value_delimiter = ',', requires = "from_progress")]
  only_status: Vec<ProgressStatusArg>,

  /// Filter to pages slower than this threshold (ms)
  #[arg(long, value_name = "MS", requires = "from_progress")]
  slow_ms: Option<f64>,

  /// Apply the slow filter only to ok pages
  #[arg(long, requires_all = ["from_progress", "slow_ms"])]
  slow_ok_only: bool,

  /// Filter to a hotspot (case-insensitive)
  #[arg(long, value_name = "HOTSPOT", requires = "from_progress")]
  hotspot: Option<String>,

  /// Pick the N slowest pages (after other filters)
  #[arg(long, value_name = "N", requires = "from_progress")]
  top_slowest: Option<usize>,

  /// Combine filters with OR instead of AND
  #[arg(long, requires = "from_progress")]
  union: bool,
}

#[derive(Args, Debug)]
struct RunArgs {
  /// Number of parallel workers
  #[arg(long, short, default_value_t = num_cpus::get())]
  jobs: usize,

  /// Hard per-page timeout in seconds (worker process is killed after this)
  #[arg(long, default_value_t = 5)]
  timeout: u64,

  /// Cooperative timeout handed to the renderer in milliseconds (0 disables)
  ///
  /// When unset, defaults to (timeout - 250ms) to allow a graceful timeout before the hard kill.
  #[arg(long)]
  soft_timeout_ms: Option<u64>,

  /// Viewport size as WxH (e.g., 1200x800)
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset
  #[arg(long, default_value = "1.0")]
  dpr: f32,

  /// Override the User-Agent header
  #[arg(long, default_value = DEFAULT_USER_AGENT)]
  user_agent: String,

  /// Override the Accept-Language header
  #[arg(long, default_value = DEFAULT_ACCEPT_LANGUAGE)]
  accept_language: String,

  /// Disable serving fresh cached HTTP responses without revalidation
  #[arg(long, action = ArgAction::SetTrue)]
  no_http_freshness: bool,

  #[command(flatten)]
  disk_cache: DiskCacheArgs,

  /// Maximum number of external stylesheets to fetch
  #[arg(long)]
  css_limit: Option<usize>,

  /// Use bundled font fixtures instead of scanning system fonts
  #[command(flatten)]
  fonts: FontSourceArgs,

  #[command(flatten)]
  resource_access: ResourceAccessArgs,

  #[command(flatten)]
  layout_parallel: LayoutParallelArgs,

  #[command(flatten)]
  compat: CompatArgs,

  /// Render only listed pages (comma-separated cache stems)
  #[arg(long, value_delimiter = ',')]
  pages: Option<Vec<String>>,

  /// Process only a deterministic shard of the cached pages (index/total, 0-based)
  #[arg(long, value_parser = parse_shard)]
  shard: Option<(usize, usize)>,

  /// Reuse selection from existing progress artifacts (failures/slow pages)
  #[command(flatten)]
  selection: ProgressSelectionArgs,

  /// Directory to write committed progress artifacts into
  #[arg(long, default_value = DEFAULT_PROGRESS_DIR)]
  progress_dir: PathBuf,

  /// Directory to write per-page logs into (not committed)
  #[arg(long, default_value = DEFAULT_LOG_DIR)]
  log_dir: PathBuf,

  /// Re-run failing pages with Chrome tracing enabled
  #[arg(long)]
  trace_failures: bool,

  /// Re-run pages slower than this (ms) with Chrome tracing enabled
  #[arg(long)]
  trace_slow_ms: Option<f64>,

  /// Hard timeout for trace reruns in seconds (defaults to timeout * 2)
  #[arg(long)]
  trace_timeout: Option<u64>,

  /// Cooperative timeout for trace reruns in milliseconds (0 disables; defaults based on trace-timeout)
  #[arg(long)]
  trace_soft_timeout_ms: Option<u64>,

  /// Number of parallel trace workers
  #[arg(long, default_value_t = 1)]
  trace_jobs: usize,

  /// Directory to write Chrome trace JSON into (not committed)
  #[arg(long, default_value = DEFAULT_TRACE_DIR)]
  trace_dir: PathBuf,

  /// Directory to write progress JSON for trace reruns (not committed)
  #[arg(long, default_value = DEFAULT_TRACE_PROGRESS_DIR)]
  trace_progress_dir: PathBuf,

  /// Populate `diagnostics.stats.cascade` by re-running cascade hotspot pages with cascade profiling enabled.
  ///
  /// This keeps the main run free of cascade profiling overhead while still producing actionable
  /// selector/candidate counters for slow cascade pages and cascade timeouts.
  #[arg(long)]
  cascade_diagnostics: bool,

  /// Re-run ok pages whose cascade stage exceeds this threshold (ms) with cascade profiling enabled
  #[arg(long, value_name = "MS", requires = "cascade_diagnostics")]
  cascade_diagnostics_slow_ms: Option<f64>,

  /// Directory to write progress JSON for cascade diagnostic reruns (not committed)
  #[arg(long, default_value = DEFAULT_CASCADE_PROGRESS_DIR)]
  cascade_diagnostics_progress_dir: PathBuf,

  /// Capture pipeline dumps for failing pages (summary|full)
  #[arg(long, value_enum)]
  dump_failures: Option<DumpLevel>,

  /// Capture pipeline dumps for pages slower than this (ms)
  #[arg(long, value_name = "MS", requires = "dump_slow")]
  dump_slow_ms: Option<f64>,

  /// Dump detail level for slow pages (summary|full)
  #[arg(long, value_enum, requires = "dump_slow_ms")]
  dump_slow: Option<DumpLevel>,

  /// Directory to write pipeline dumps into (not committed)
  #[arg(long, default_value = DEFAULT_DUMP_DIR)]
  dump_dir: PathBuf,

  /// Hard timeout in seconds for dump reruns (defaults to timeout * 2)
  #[arg(long)]
  dump_timeout: Option<u64>,

  /// Cooperative timeout in milliseconds for dump reruns (0 disables; defaults based on dump-timeout)
  #[arg(long)]
  dump_soft_timeout_ms: Option<u64>,

  /// Diagnostics capture level for the main run (none|basic|verbose)
  #[arg(long, value_enum, default_value_t = DiagnosticsArg::Basic)]
  diagnostics: DiagnosticsArg,

  /// Print full error chains in logs and progress files
  #[arg(long)]
  verbose: bool,
}

#[derive(Args, Debug)]
struct ReportArgs {
  /// Directory containing `progress/pages/*.json`
  #[arg(long, default_value = DEFAULT_PROGRESS_DIR)]
  progress_dir: PathBuf,

  /// Number of slowest pages to list
  #[arg(long, default_value_t = 10)]
  top: usize,

  /// Compare against another progress directory
  #[arg(long, value_name = "DIR")]
  compare: Option<PathBuf>,

  /// Exit non-zero when pages slow down or regress vs `--compare`
  #[arg(long, requires = "compare")]
  fail_on_regression: bool,

  /// Slowdown threshold (percent) for `--fail-on-regression`
  #[arg(
    long,
    default_value_t = 10.0,
    requires = "compare",
    value_name = "PERCENT"
  )]
  regression_threshold_percent: f64,

  /// Exit non-zero when any page timed out or panicked
  #[arg(long)]
  fail_on_bad: bool,

  /// Include trace reruns when summarizing (if present)
  #[arg(long)]
  include_trace: bool,

  /// Directory containing trace rerun progress JSON (not committed)
  #[arg(long, default_value = DEFAULT_TRACE_PROGRESS_DIR)]
  trace_progress_dir: PathBuf,

  /// Directory containing Chrome trace JSON (not committed)
  #[arg(long, default_value = DEFAULT_TRACE_DIR)]
  trace_dir: PathBuf,

  /// Print structured stats for the listed pages when present
  #[arg(long = "verbose-stats", visible_alias = "show-counts")]
  verbose_stats: bool,

  /// Include machine-generated notes from the most recent run in per-page listings
  #[arg(long)]
  verbose: bool,

  /// Exit non-zero when any timeout/panic/error entry lacks stage attribution
  #[arg(long)]
  fail_on_missing_stages: bool,

  /// Exit non-zero when ok entries have `total_ms` but no per-stage timings (all stage buckets are zero)
  #[arg(long)]
  fail_on_missing_stage_timings: bool,

  /// Exit non-zero when ok entries have stage buckets that materially exceed `total_ms`
  ///
  /// This is a guardrail for catching stage timing accounting regressions (e.g., double counting
  /// or mixing CPU-sum metrics into wall-clock buckets).
  #[arg(long)]
  fail_on_stage_sum_exceeds_total: bool,

  /// Tolerance for `--fail-on-stage-sum-exceeds-total` (percent)
  #[arg(
    long,
    default_value_t = 10.0,
    requires = "fail_on_stage_sum_exceeds_total",
    value_name = "PERCENT"
  )]
  stage_sum_tolerance_percent: f64,

  /// Exit non-zero when ok entries exceed this total render time threshold (ms)
  #[arg(long, value_name = "MS")]
  fail_on_slow_ok_ms: Option<f64>,

  /// Exit non-zero when ok entries have `failure_stage` set (subresource failures despite status=ok)
  #[arg(long)]
  fail_on_ok_with_failures: bool,

  /// Exit non-zero when a page was ok with no failures in the baseline but is now ok with `failure_stage` set
  #[arg(long, requires = "compare")]
  fail_on_new_ok_failures: bool,
}

#[derive(Args, Debug)]
struct WorkerArgs {
  /// Cached HTML path (from fetches/html/*.html)
  #[arg(long)]
  cache_path: PathBuf,

  /// Page stem (used only for logging)
  #[arg(long)]
  stem: String,

  /// Output progress JSON path (committed or temp)
  #[arg(long)]
  progress_path: PathBuf,

  /// Output log path (optional)
  #[arg(long)]
  log_path: Option<PathBuf>,

  /// Path to write heartbeat stage markers (optional)
  #[arg(long)]
  stage_path: Option<PathBuf>,

  /// Viewport size as WxH (e.g., 1200x800)
  #[arg(long, value_parser = parse_viewport)]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset
  #[arg(long)]
  dpr: f32,

  /// Override the User-Agent header
  #[arg(long)]
  user_agent: String,

  /// Override the Accept-Language header
  #[arg(long)]
  accept_language: String,

  /// Disable serving fresh cached HTTP responses without revalidation
  #[arg(long, action = ArgAction::SetTrue)]
  no_http_freshness: bool,

  #[command(flatten)]
  disk_cache: DiskCacheArgs,

  /// Maximum number of external stylesheets to fetch
  #[arg(long)]
  css_limit: Option<usize>,

  #[command(flatten)]
  fonts: FontSourceArgs,

  #[command(flatten)]
  resource_access: ResourceAccessArgs,

  #[command(flatten)]
  layout_parallel: LayoutParallelArgs,

  #[command(flatten)]
  compat: CompatArgs,

  /// Hard per-page timeout in seconds (used for fetch budgeting)
  #[arg(long, default_value_t = 5)]
  timeout: u64,

  /// Cooperative timeout handed to the renderer in milliseconds (0 disables)
  #[arg(long)]
  soft_timeout_ms: Option<u64>,

  /// Write a Chrome trace of the render to this path
  #[arg(long)]
  trace_out: Option<PathBuf>,

  /// Diagnostics capture level (none|basic|verbose)
  #[arg(long, value_enum)]
  diagnostics: DiagnosticsArg,

  /// Print full error chains in logs and progress files
  #[arg(long)]
  verbose: bool,

  /// Capture pipeline dumps for failing pages (summary|full)
  #[arg(long, value_enum)]
  dump_failures: Option<DumpLevel>,

  /// Capture pipeline dumps for pages slower than this (ms)
  #[arg(long, value_name = "MS", requires = "dump_slow")]
  dump_slow_ms: Option<f64>,

  /// Dump detail level for slow pages (summary|full)
  #[arg(long, value_enum, requires = "dump_slow_ms")]
  dump_slow: Option<DumpLevel>,

  /// Directory to write pipeline dumps into (not committed)
  #[arg(long, default_value = DEFAULT_DUMP_DIR)]
  dump_dir: PathBuf,

  /// Hard timeout in seconds for dump reruns (defaults to timeout * 2)
  #[arg(long)]
  dump_timeout: Option<u64>,

  /// Cooperative timeout in milliseconds for dump reruns (0 disables; defaults based on dump-timeout)
  #[arg(long)]
  dump_soft_timeout_ms: Option<u64>,
}

fn parse_viewport(s: &str) -> Result<(u32, u32), String> {
  let parts: Vec<&str> = s.split('x').collect();
  if parts.len() != 2 {
    return Err("viewport must be WxH (e.g., 1200x800)".to_string());
  }
  let w = parts[0].parse::<u32>().map_err(|_| "invalid width")?;
  let h = parts[1].parse::<u32>().map_err(|_| "invalid height")?;
  if w == 0 || h == 0 {
    return Err("width and height must be > 0".to_string());
  }
  Ok((w, h))
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "snake_case")]
enum ProgressStatus {
  Ok,
  Timeout,
  Panic,
  Error,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ProgressStage {
  DomParse,
  Css,
  Cascade,
  Layout,
  Paint,
}

impl ProgressStage {
  fn as_str(&self) -> &'static str {
    match self {
      ProgressStage::DomParse => "dom_parse",
      ProgressStage::Css => "css",
      ProgressStage::Cascade => "cascade",
      ProgressStage::Layout => "layout",
      ProgressStage::Paint => "paint",
    }
  }
}

impl From<RenderStage> for ProgressStage {
  fn from(stage: RenderStage) -> Self {
    match stage {
      RenderStage::DomParse => ProgressStage::DomParse,
      RenderStage::Css => ProgressStage::Css,
      RenderStage::Cascade => ProgressStage::Cascade,
      RenderStage::Layout => ProgressStage::Layout,
      RenderStage::Paint => ProgressStage::Paint,
    }
  }
}

impl ProgressStatus {
  fn as_str(&self) -> &'static str {
    match self {
      ProgressStatus::Ok => "ok",
      ProgressStatus::Timeout => "timeout",
      ProgressStatus::Panic => "panic",
      ProgressStatus::Error => "error",
    }
  }
}

impl Default for ProgressStatus {
  fn default() -> Self {
    ProgressStatus::Error
  }
}

impl From<ProgressStatusArg> for ProgressStatus {
  fn from(value: ProgressStatusArg) -> Self {
    match value {
      ProgressStatusArg::Ok => ProgressStatus::Ok,
      ProgressStatusArg::Timeout => ProgressStatus::Timeout,
      ProgressStatusArg::Panic => ProgressStatus::Panic,
      ProgressStatusArg::Error => ProgressStatus::Error,
    }
  }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
struct StageBuckets {
  fetch: f64,
  css: f64,
  cascade: f64,
  layout: f64,
  paint: f64,
}

impl StageBuckets {
  fn sum(&self) -> f64 {
    self.fetch + self.css + self.cascade + self.layout + self.paint
  }

  fn rescale_to_total(&mut self, total_ms: f64) {
    if !total_ms.is_finite() || total_ms <= 0.0 {
      return;
    }
    let sum = self.sum();
    if !sum.is_finite() || sum <= 0.0 {
      return;
    }
    let scale = total_ms / sum;
    if !scale.is_finite() || scale <= 0.0 {
      return;
    }
    self.fetch *= scale;
    self.css *= scale;
    self.cascade *= scale;
    self.layout *= scale;
    self.paint *= scale;
  }
}

fn stage_buckets_for_progress_stage(stage: ProgressStage, total_ms: f64) -> StageBuckets {
  let mut buckets = StageBuckets::default();
  match stage {
    ProgressStage::DomParse => buckets.fetch = total_ms,
    ProgressStage::Css => buckets.css = total_ms,
    ProgressStage::Cascade => buckets.cascade = total_ms,
    ProgressStage::Layout => buckets.layout = total_ms,
    ProgressStage::Paint => buckets.paint = total_ms,
  }
  buckets
}

pub(crate) fn current_git_sha() -> Option<String> {
  static GIT_SHA: OnceLock<Option<String>> = OnceLock::new();
  GIT_SHA
    .get_or_init(|| {
      if let Ok(output) = Command::new("git").arg("rev-parse").arg("HEAD").output() {
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
    })
    .clone()
}

fn compute_soft_timeout_ms(hard_timeout: Duration, override_ms: Option<u64>) -> Option<u64> {
  if let Some(ms) = override_ms {
    return Some(ms);
  }
  let ms = hard_timeout.as_millis();
  if ms <= 250 {
    None
  } else {
    Some((ms - 250) as u64)
  }
}

fn compute_trace_hard_timeout(
  main_timeout_secs: u64,
  trace_timeout_override: Option<u64>,
) -> Duration {
  let secs = trace_timeout_override.unwrap_or_else(|| main_timeout_secs.saturating_mul(2));
  Duration::from_secs(secs)
}

fn compute_fetch_timeout(hard_timeout: Duration, soft_timeout_ms: Option<u64>) -> Duration {
  if let Some(ms) = soft_timeout_ms {
    if ms > 0 {
      let with_slack = ms.saturating_add(FETCH_TIMEOUT_SLACK_MS);
      return Duration::from_millis(with_slack).min(hard_timeout);
    }
  }
  hard_timeout
}

fn normalize_hotspot(h: &str) -> String {
  let trimmed = h.trim();
  if trimmed.is_empty() {
    "unknown".to_string()
  } else {
    trimmed.to_string()
  }
}

fn is_hotspot_unset(h: &str) -> bool {
  let trimmed = h.trim();
  trimmed.is_empty() || trimmed.eq_ignore_ascii_case("unknown")
}

fn is_manual_field_unset(value: &str) -> bool {
  value.trim().is_empty()
}

fn is_legacy_auto_note_line(line: &str) -> bool {
  let trimmed = line.trim();
  matches!(
    trimmed,
    "not run" | "missing cache" | "timeout" | "panic" | "error"
  ) || trimmed.starts_with("hard timeout after ")
    || trimmed.starts_with("timeout at ")
    || trimmed.starts_with("fetch failed:")
    || trimmed.starts_with("worker exited")
    || trimmed.starts_with("worker try_wait failed")
    || trimmed.starts_with("read:")
    || trimmed.starts_with("renderer init:")
    || trimmed.starts_with("stderr tail")
    || trimmed.starts_with("stage:")
    // Notes are capped to `PROGRESS_NOTE_MAX_CHARS`, which can truncate a line mid-token and leave
    // partial `stage:` prefixes behind.
    || matches!(trimmed, "stage…" | "s…")
    // Legacy progress files often stored the formatted error display in `notes`; treat those as
    // machine-generated so reruns do not preserve stale diagnostics as "manual notes".
    || trimmed.starts_with("[parse]")
    || trimmed.starts_with("[style]")
    || trimmed.starts_with("[layout]")
    || trimmed.starts_with("[paint]")
    || trimmed.starts_with("[resource]")
    || trimmed.starts_with("[other]")
}

fn manual_notes_from_previous(previous: &PageProgress) -> Option<String> {
  let raw = previous.notes.trim();
  if raw.is_empty() {
    return None;
  }
  if previous.auto_notes.trim().is_empty() {
    // Legacy progress files stored machine diagnostics in `notes`. Attempt to strip the known
    // machine-generated lines so only human notes remain durable across runs.
    let manual: String = raw
      .lines()
      .filter(|line| !is_legacy_auto_note_line(line))
      .collect::<Vec<_>>()
      .join("\n");
    let manual = manual.trim();
    if manual.is_empty() {
      None
    } else {
      Some(manual.to_string())
    }
  } else {
    Some(previous.notes.clone())
  }
}

fn legacy_auto_notes_from_previous(previous: &PageProgress) -> Option<String> {
  let raw = previous.notes.trim();
  if raw.is_empty() || !previous.auto_notes.trim().is_empty() {
    return None;
  }
  let auto: String = raw
    .lines()
    .filter(|line| is_legacy_auto_note_line(line))
    .collect::<Vec<_>>()
    .join("\n");
  let auto = auto.trim();
  if auto.is_empty() {
    None
  } else {
    Some(auto.to_string())
  }
}

fn migrate_legacy_notes(progress: &mut PageProgress) {
  if !progress.auto_notes.trim().is_empty() || progress.notes.trim().is_empty() {
    return;
  }
  let manual = manual_notes_from_previous(progress).unwrap_or_default();
  let auto = legacy_auto_notes_from_previous(progress).unwrap_or_default();
  progress.notes = manual;
  progress.auto_notes = auto;
}

fn normalize_missing_cache_placeholder(progress: &mut PageProgress, cache_exists: bool) {
  if cache_exists
    && progress.status == ProgressStatus::Error
    && progress.total_ms.is_none()
    && progress.auto_notes.trim() == "missing cache"
  {
    progress.auto_notes = "not run".to_string();
  }
}

fn merge_alias_manual_fields(progress: &mut PageProgress, alias: &PageProgress) {
  if progress.status == ProgressStatus::Ok {
    return;
  }
  if progress.notes.trim().is_empty() && !alias.notes.trim().is_empty() {
    progress.notes = alias.notes.clone();
  }
  if is_hotspot_unset(&progress.hotspot) && !is_hotspot_unset(&alias.hotspot) {
    progress.hotspot = alias.hotspot.clone();
  }
  if is_manual_field_unset(&progress.last_good_commit)
    && !is_manual_field_unset(&alias.last_good_commit)
  {
    progress.last_good_commit = alias.last_good_commit.clone();
  }
  if is_manual_field_unset(&progress.last_regression_commit)
    && !is_manual_field_unset(&alias.last_regression_commit)
  {
    progress.last_regression_commit = alias.last_regression_commit.clone();
  }
}

fn normalize_hotspot_filter(h: &str) -> String {
  normalize_hotspot(h).to_ascii_lowercase()
}

fn elide_shaping_excerpt(note: &str) -> String {
  if !note.contains("ShapingFailed") {
    return note.to_string();
  }
  let text_marker = "text:";
  let reason_marker = ", reason:";
  let Some(text_start) = note.find(text_marker) else {
    return note.to_string();
  };
  let Some(reason_rel) = note[text_start..].find(reason_marker) else {
    return note.to_string();
  };
  let reason_idx = text_start + reason_rel;
  let mut shortened = String::with_capacity(note.len());
  shortened.push_str(&note[..text_start + text_marker.len()]);
  shortened.push_str(" <omitted>");
  shortened.push_str(&note[reason_idx..]);
  shortened
}

fn truncate_long_quoted_segments(note: &str, max_segment_chars: usize) -> String {
  if !note.contains('"') {
    return note.to_string();
  }
  static QUOTED_RE: OnceLock<Regex> = OnceLock::new();
  let re =
    QUOTED_RE.get_or_init(|| Regex::new(r#""([^"]*)""#).expect("quoted segment regex compiles"));
  re.replace_all(note, |caps: &regex::Captures| {
    let segment = &caps[1];
    if segment.chars().count() <= max_segment_chars {
      caps[0].to_string()
    } else {
      let mut truncated: String = segment.chars().take(max_segment_chars).collect();
      truncated.push(PROGRESS_NOTE_ELLIPSIS);
      format!("\"{truncated}\"")
    }
  })
  .into_owned()
}

fn cap_note_length(note: &str, max_chars: usize) -> String {
  if note.chars().count() <= max_chars || max_chars == 0 {
    return note.to_string();
  }
  let mut truncated: String = note.chars().take(max_chars.saturating_sub(1)).collect();
  truncated.push(PROGRESS_NOTE_ELLIPSIS);
  truncated
}

pub(crate) fn normalize_progress_note(note: &str) -> String {
  let mut normalized = note.to_string();
  normalized = elide_shaping_excerpt(&normalized);
  normalized = truncate_long_quoted_segments(&normalized, PROGRESS_NOTE_QUOTED_EXCERPT_MAX_CHARS);
  cap_note_length(&normalized, PROGRESS_NOTE_MAX_CHARS)
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(crate) struct PageProgress {
  url: String,
  status: ProgressStatus,
  total_ms: Option<f64>,
  stages_ms: StageBuckets,
  notes: String,
  #[serde(default, skip_serializing_if = "String::is_empty")]
  auto_notes: String,
  hotspot: String,
  failure_stage: Option<ProgressStage>,
  timeout_stage: Option<ProgressStage>,
  last_good_commit: String,
  last_regression_commit: String,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  diagnostics: Option<ProgressDiagnostics>,
}

impl PageProgress {
  pub(crate) fn new(url: String) -> Self {
    Self {
      url,
      status: ProgressStatus::Error,
      total_ms: None,
      stages_ms: StageBuckets::default(),
      notes: String::new(),
      auto_notes: String::new(),
      hotspot: String::new(),
      failure_stage: None,
      timeout_stage: None,
      last_good_commit: String::new(),
      last_regression_commit: String::new(),
      diagnostics: None,
    }
  }

  pub(crate) fn merge_preserving_manual(
    mut self,
    previous: Option<PageProgress>,
    current_sha: Option<&str>,
  ) -> Self {
    if let Some(sha) = current_sha {
      self.update_commit_markers(previous.as_ref(), sha);
    }
    if self.status == ProgressStatus::Ok {
      self.notes = String::new();
      self.auto_notes = String::new();
    }
    let Some(prev) = previous else {
      return self;
    };
    if self.status != ProgressStatus::Ok {
      if let Some(notes) = manual_notes_from_previous(&prev) {
        self.notes = notes;
      }
    }
    if !is_hotspot_unset(&prev.hotspot) && is_hotspot_unset(&self.hotspot) {
      self.hotspot = prev.hotspot;
    }
    if !is_manual_field_unset(&prev.last_good_commit)
      && is_manual_field_unset(&self.last_good_commit)
    {
      self.last_good_commit = prev.last_good_commit;
    }
    if !is_manual_field_unset(&prev.last_regression_commit)
      && is_manual_field_unset(&self.last_regression_commit)
    {
      self.last_regression_commit = prev.last_regression_commit;
    }
    self
  }

  fn update_commit_markers(&mut self, previous: Option<&PageProgress>, current_sha: &str) {
    if current_sha.trim().is_empty() {
      return;
    }
    if let Some(prev) = previous {
      match (prev.status, self.status) {
        (ProgressStatus::Ok, new_status) if new_status != ProgressStatus::Ok => {
          if is_manual_field_unset(&self.last_regression_commit)
            && is_manual_field_unset(&prev.last_regression_commit)
          {
            self.last_regression_commit = current_sha.to_string();
          }
        }
        (prev_status, ProgressStatus::Ok) if prev_status != ProgressStatus::Ok => {
          self.last_good_commit = current_sha.to_string();
        }
        (ProgressStatus::Ok, ProgressStatus::Ok) => {
          if is_manual_field_unset(&self.last_good_commit)
            && is_manual_field_unset(&prev.last_good_commit)
          {
            self.last_good_commit = current_sha.to_string();
          }
        }
        _ => {}
      }
    } else if self.status == ProgressStatus::Ok && is_manual_field_unset(&self.last_good_commit) {
      self.last_good_commit = current_sha.to_string();
    }
  }
}

fn read_progress(path: &Path) -> Option<PageProgress> {
  let raw = fs::read_to_string(path).ok()?;
  serde_json::from_str::<PageProgress>(&raw).ok()
}

#[derive(Debug)]
struct LoadedProgress {
  stem: String,
  progress: PageProgress,
  stats: Option<RenderStats>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
struct ProgressDiagnostics {
  #[serde(default)]
  stats: Option<RenderStats>,
}

#[derive(Debug, Default, Clone, Copy)]
struct StatusCounts {
  ok: usize,
  timeout: usize,
  panic: usize,
  error: usize,
}

#[derive(Debug, Clone)]
enum ProgressCriterion {
  Status(HashSet<ProgressStatus>),
  Slow { threshold_ms: f64, ok_only: bool },
  Hotspot(String),
}

#[derive(Debug, Clone, Default)]
struct ProgressSelection {
  criteria: Vec<ProgressCriterion>,
  top_slowest: Option<usize>,
  union: bool,
}

impl ProgressSelectionArgs {
  fn is_active(&self) -> bool {
    self.from_progress.is_some()
  }

  fn to_selection(&self) -> ProgressSelection {
    let mut criteria = Vec::new();
    let mut statuses: HashSet<ProgressStatus> = HashSet::new();
    if self.only_failures {
      statuses.insert(ProgressStatus::Timeout);
      statuses.insert(ProgressStatus::Panic);
      statuses.insert(ProgressStatus::Error);
    }
    for status in &self.only_status {
      statuses.insert((*status).into());
    }
    if !statuses.is_empty() {
      criteria.push(ProgressCriterion::Status(statuses));
    }
    if let Some(threshold_ms) = self.slow_ms {
      criteria.push(ProgressCriterion::Slow {
        threshold_ms,
        ok_only: self.slow_ok_only,
      });
    }
    if let Some(hotspot) = &self.hotspot {
      criteria.push(ProgressCriterion::Hotspot(normalize_hotspot_filter(
        hotspot,
      )));
    }

    ProgressSelection {
      criteria,
      top_slowest: self.top_slowest,
      union: self.union,
    }
  }
}

fn url_hint_from_cache_path(cache_path: &Path) -> String {
  cached_url_from_cache_meta(cache_path)
    .unwrap_or_else(|| format!("file://{}", cache_path.display()))
}

fn cached_url_from_cache_meta(cache_path: &Path) -> Option<String> {
  let mut meta_path = cache_path.to_path_buf();
  if let Some(ext) = meta_path.extension().and_then(|e| e.to_str()) {
    meta_path.set_extension(format!("{ext}.meta"));
  } else {
    meta_path.set_extension("meta");
  }
  let meta = fs::read_to_string(&meta_path).ok()?;
  let parsed_meta = parse_cached_html_meta(&meta);
  parsed_meta.url
}

fn parse_cache_collision_suffix(raw: &str) -> Option<(&str, &str)> {
  raw
    .rsplit_once("--")
    .filter(|(_, suffix)| suffix.len() == 8 && suffix.chars().all(|c| c.is_ascii_hexdigit()))
}

fn cached_html_index_by_pageset_stem(
  cached_paths: &BTreeMap<String, PathBuf>,
) -> HashMap<String, Vec<(String, PathBuf)>> {
  let mut by_stem: HashMap<String, Vec<(String, PathBuf)>> = HashMap::new();
  for (cache_stem, cache_path) in cached_paths {
    if let Some(stem) = pageset_stem(cache_stem) {
      by_stem
        .entry(stem)
        .or_default()
        .push((cache_stem.clone(), cache_path.clone()));
    }
  }
  for entries in by_stem.values_mut() {
    entries.sort_by(|a, b| a.0.cmp(&b.0));
  }
  by_stem
}

fn cached_html_path_for_pageset_entry(
  entry: &PagesetEntry,
  cached_paths: &BTreeMap<String, PathBuf>,
  cached_by_stem: &HashMap<String, Vec<(String, PathBuf)>>,
) -> Option<PathBuf> {
  if let Some(path) = cached_paths.get(&entry.cache_stem) {
    return Some(path.clone());
  }
  let candidates = cached_by_stem.get(&entry.stem)?;
  if candidates.len() == 1 {
    return Some(candidates[0].1.clone());
  }
  if let Some((_, suffix)) = parse_cache_collision_suffix(&entry.cache_stem) {
    let mut matched: Option<&PathBuf> = None;
    let suffix = suffix.to_ascii_lowercase();
    for (cache_stem, cache_path) in candidates {
      if let Some((_, candidate_suffix)) = parse_cache_collision_suffix(cache_stem) {
        if candidate_suffix.to_ascii_lowercase() == suffix {
          if matched.is_some() {
            return None;
          }
          matched = Some(cache_path);
        }
      }
    }
    if let Some(path) = matched {
      return Some(path.clone());
    }
  }
  None
}

fn resolve_pageset_entry_for_cache_stem<'a>(
  cache_stem: &str,
  pageset_by_cache: &HashMap<String, &'a PagesetEntry>,
  pageset_by_stem: &HashMap<String, Vec<&'a PagesetEntry>>,
) -> Option<&'a PagesetEntry> {
  pageset_by_cache
    .get(cache_stem)
    .copied()
    .or_else(|| {
      parse_cache_collision_suffix(cache_stem).and_then(|(base, suffix)| {
        pageset_stem(base).and_then(|stem| {
          let canonical = format!("{stem}--{}", suffix.to_ascii_lowercase());
          pageset_by_cache.get(&canonical).copied()
        })
      })
    })
    .or_else(|| {
      pageset_stem(cache_stem).and_then(|stem| {
        pageset_by_stem
          .get(&stem)
          .and_then(|entries| (entries.len() == 1).then_some(entries[0]))
      })
    })
}

fn atomic_write_with_hook<F>(path: &Path, contents: &[u8], pre_rename_hook: F) -> io::Result<()>
where
  F: FnOnce(&Path) -> io::Result<()>,
{
  if let Some(parent) = path.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent)?;
    }
  }
  let dir = path
    .parent()
    .filter(|p| !p.as_os_str().is_empty())
    .unwrap_or_else(|| Path::new("."));
  let mut temp = NamedTempFile::new_in(dir)?;
  temp.write_all(contents)?;
  temp.flush()?;
  temp.as_file_mut().sync_all()?;
  let temp_path = temp.into_temp_path();
  pre_rename_hook(temp_path.as_ref())?;
  temp_path.persist(path).map_err(|e| e.error)
}

fn atomic_write(path: &Path, contents: &[u8]) -> io::Result<()> {
  atomic_write_with_hook(path, contents, |_| Ok(()))
}

fn serialize_progress(progress: &PageProgress) -> io::Result<String> {
  let mut normalized = progress.clone();
  normalized.notes = normalize_progress_note(&normalized.notes);
  normalized.auto_notes = normalize_progress_note(&normalized.auto_notes);
  let json = serde_json::to_string_pretty(&normalized)
    .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
  Ok(format!("{json}\n"))
}

fn write_progress(path: &Path, progress: &PageProgress) -> io::Result<()> {
  let formatted = serialize_progress(progress)?;
  atomic_write(path, formatted.as_bytes())
}

fn collect_cached_html_paths_in(dir: &Path) -> io::Result<BTreeMap<String, PathBuf>> {
  let mut entries: BTreeMap<String, PathBuf> = BTreeMap::new();
  for entry in
    fs::read_dir(dir).map_err(|e| io::Error::new(e.kind(), format!("{}: {}", dir.display(), e)))?
  {
    let entry = entry?;
    if entry.file_type()?.is_dir() {
      continue;
    }
    let path = entry.path();
    if path.extension().map(|x| x == "html").unwrap_or(false) {
      if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
        entries.insert(stem.to_string(), path);
      }
    }
  }
  Ok(entries)
}

fn collect_alias_progress(
  progress_dir: &Path,
  pageset_by_cache: &HashMap<String, &PagesetEntry>,
  pageset_by_stem: &HashMap<String, Vec<&PagesetEntry>>,
) -> HashMap<String, PageProgress> {
  let mut aliases: HashMap<String, PageProgress> = HashMap::new();
  let entries = match fs::read_dir(progress_dir) {
    Ok(entries) => entries,
    Err(_) => return aliases,
  };

  for entry in entries.flatten() {
    let path = entry.path();
    if path.extension().and_then(|e| e.to_str()) != Some("json") {
      continue;
    }
    let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
      continue;
    };
    let Some(pageset_entry) =
      resolve_pageset_entry_for_cache_stem(stem, pageset_by_cache, pageset_by_stem)
    else {
      continue;
    };
    if pageset_entry.cache_stem == stem {
      continue;
    }
    let Some(mut progress) = read_progress(&path) else {
      continue;
    };
    migrate_legacy_notes(&mut progress);
    let key = pageset_entry.cache_stem.clone();
    aliases
      .entry(key)
      .and_modify(|existing| {
        if existing.notes.trim().is_empty() && !progress.notes.trim().is_empty() {
          *existing = progress.clone();
        }
      })
      .or_insert(progress);
  }

  aliases
}

fn prune_stale_progress(progress_dir: &Path, keep: &BTreeSet<String>) -> io::Result<usize> {
  let mut pruned = 0usize;
  for entry in fs::read_dir(progress_dir)? {
    let entry = entry?;
    if entry.file_type()?.is_dir() {
      continue;
    }
    let path = entry.path();
    if path.extension().and_then(|e| e.to_str()) != Some("json") {
      continue;
    }
    let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
      continue;
    };
    if keep.contains(stem) {
      continue;
    }
    fs::remove_file(path)?;
    pruned += 1;
  }
  Ok(pruned)
}

fn migrate(args: MigrateArgs) -> io::Result<()> {
  let entries = fs::read_dir(&args.progress_dir)
    .map_err(|e| io::Error::new(e.kind(), format!("{}: {}", args.progress_dir.display(), e)))?;

  let mut paths: Vec<PathBuf> = entries
    .filter_map(|entry| {
      let entry = entry.ok()?;
      if entry.file_type().ok()?.is_dir() {
        return None;
      }
      let path = entry.path();
      if path.extension().and_then(|ext| ext.to_str()) != Some("json") {
        return None;
      }
      Some(path)
    })
    .collect();
  paths.sort();

  let mut processed = 0usize;
  let mut rewritten = 0usize;
  for path in paths {
    let stem = path
      .file_stem()
      .and_then(|s| s.to_str())
      .ok_or_else(|| {
        io::Error::new(
          io::ErrorKind::InvalidData,
          format!("{}: invalid filename", path.display()),
        )
      })?
      .to_string();
    let cache_exists = args.html_dir.join(format!("{stem}.html")).exists();

    let raw = fs::read_to_string(&path)?;
    let mut progress: PageProgress = serde_json::from_str(&raw).map_err(|e| {
      io::Error::new(
        io::ErrorKind::InvalidData,
        format!("{}: {}", path.display(), e),
      )
    })?;
    migrate_legacy_notes(&mut progress);
    normalize_missing_cache_placeholder(&mut progress, cache_exists);
    let formatted = serialize_progress(&progress)?;
    if formatted != raw {
      atomic_write(&path, formatted.as_bytes())?;
      rewritten += 1;
    }
    processed += 1;
  }

  println!("Migrated {rewritten} / {processed} progress files.");
  Ok(())
}

fn sync(args: SyncArgs) -> io::Result<()> {
  fs::create_dir_all(&args.progress_dir)?;

  let cached_paths = match collect_cached_html_paths_in(&args.html_dir) {
    Ok(paths) => paths,
    Err(err) if err.kind() == io::ErrorKind::NotFound => BTreeMap::new(),
    Err(err) => return Err(err),
  };
  let cached_by_stem = cached_html_index_by_pageset_stem(&cached_paths);

  let pageset_entries = pageset_entries();
  let pageset_by_cache: HashMap<String, &PagesetEntry> = pageset_entries
    .iter()
    .map(|entry| (entry.cache_stem.clone(), entry))
    .collect();
  let mut pageset_by_stem: HashMap<String, Vec<&PagesetEntry>> = HashMap::new();
  for entry in &pageset_entries {
    pageset_by_stem
      .entry(entry.stem.clone())
      .or_default()
      .push(entry);
  }
  let alias_progress =
    collect_alias_progress(&args.progress_dir, &pageset_by_cache, &pageset_by_stem);

  let mut stems: BTreeSet<String> = BTreeSet::new();
  let mut created = 0usize;
  let mut updated = 0usize;

  for entry in &pageset_entries {
    let progress_path = args.progress_dir.join(format!("{}.json", entry.cache_stem));
    let cache_path = cached_html_path_for_pageset_entry(&entry, &cached_paths, &cached_by_stem);
    let cache_exists = cache_path.is_some();

    let previous = read_progress(&progress_path);
    let mut progress = if let Some(ref prev) = previous {
      prev.clone()
    } else {
      created += 1;
      let mut new_progress = PageProgress::new(entry.url.clone());
      new_progress.auto_notes = "not run".to_string();
      new_progress
    };

    progress.url = cache_path
      .as_ref()
      .and_then(|path| cached_url_from_cache_meta(path))
      .unwrap_or_else(|| entry.url.clone());

    migrate_legacy_notes(&mut progress);
    normalize_missing_cache_placeholder(&mut progress, cache_exists);

    if !cache_exists {
      progress.status = ProgressStatus::Error;
      if is_hotspot_unset(&progress.hotspot) {
        progress.hotspot = "fetch".to_string();
      }
      progress.auto_notes = "missing cache".to_string();
      progress.total_ms = None;
      progress.stages_ms = StageBuckets::default();
      progress.failure_stage = None;
      progress.timeout_stage = None;
      progress.diagnostics = None;
    }

    if let Some(alias) = alias_progress.get(&entry.cache_stem) {
      merge_alias_manual_fields(&mut progress, alias);
    }

    stems.insert(entry.cache_stem.clone());
    write_progress(&progress_path, &progress)?;
    if previous.is_some() {
      updated += 1;
    }
  }

  let pruned = if args.prune {
    prune_stale_progress(&args.progress_dir, &stems)?
  } else {
    0
  };

  println!(
    "Synced {} pages (created {}, updated {}, pruned {})",
    stems.len(),
    created,
    updated,
    pruned
  );

  Ok(())
}

/// Collapse detailed render timing stats into coarse stage buckets used by progress JSON.
///
/// These buckets are **wall-clock stage timers** and are intended to be roughly additive (i.e.
/// `stages_ms.sum()` should approximately match `total_ms` for successful renders).
///
/// Note: `text_*` timings are **subsystem breakdown** counters (and may be CPU-summed), so they are
/// intentionally *not* mixed into stage buckets to avoid double-counting and nonsense totals.
///
/// Keep this consistent with `perf_smoke` (`stage_breakdown_from_stats`).
fn buckets_from_diagnostics(diag: &RenderDiagnostics) -> StageBuckets {
  let Some(stats) = diag.stats.as_ref() else {
    return StageBuckets::default();
  };
  let t = &stats.timings;
  let fetch = t.html_decode_ms.unwrap_or(0.0)
    + t.dom_parse_ms.unwrap_or(0.0)
    + t.dom_meta_viewport_ms.unwrap_or(0.0)
    + t.dom_clone_ms.unwrap_or(0.0)
    + t.dom_top_layer_ms.unwrap_or(0.0);
  let css = t.css_inlining_ms.unwrap_or(0.0) + t.css_parse_ms.unwrap_or(0.0);
  let cascade = t.cascade_ms.unwrap_or(0.0) + t.box_tree_ms.unwrap_or(0.0);
  let layout = t.layout_ms.unwrap_or(0.0);
  let paint = t.paint_build_ms.unwrap_or(0.0)
    + t.paint_optimize_ms.unwrap_or(0.0)
    + t.paint_rasterize_ms.unwrap_or(0.0)
    + t.encode_ms.unwrap_or(0.0);
  StageBuckets {
    fetch,
    css,
    cascade,
    layout,
    paint,
  }
}

fn guess_hotspot(buckets: &StageBuckets) -> &'static str {
  let mut best = ("unknown", 0.0f64);
  for (name, value) in [
    ("fetch", buckets.fetch),
    ("css", buckets.css),
    ("cascade", buckets.cascade),
    ("layout", buckets.layout),
    ("paint", buckets.paint),
  ] {
    if value > best.1 {
      best = (name, value);
    }
  }
  best.0
}

pub(crate) fn hotspot_from_timeout_stage(stage: RenderStage) -> &'static str {
  match stage {
    RenderStage::DomParse => "fetch",
    RenderStage::Css => "css",
    RenderStage::Cascade => "cascade",
    RenderStage::Layout => "layout",
    RenderStage::Paint => "paint",
  }
}

pub(crate) fn progress_stage_from_heartbeat(stage: StageHeartbeat) -> Option<ProgressStage> {
  match stage {
    StageHeartbeat::ReadCache | StageHeartbeat::FollowRedirects | StageHeartbeat::DomParse => {
      Some(ProgressStage::DomParse)
    }
    StageHeartbeat::CssInline | StageHeartbeat::CssParse => Some(ProgressStage::Css),
    StageHeartbeat::Cascade | StageHeartbeat::BoxTree => Some(ProgressStage::Cascade),
    StageHeartbeat::Layout => Some(ProgressStage::Layout),
    StageHeartbeat::PaintBuild | StageHeartbeat::PaintRasterize => Some(ProgressStage::Paint),
    StageHeartbeat::Done => None,
  }
}

pub(crate) fn hotspot_from_progress_stage(stage: ProgressStage) -> &'static str {
  match stage {
    ProgressStage::DomParse => "fetch",
    ProgressStage::Css => "css",
    ProgressStage::Cascade => "cascade",
    ProgressStage::Layout => "layout",
    ProgressStage::Paint => "paint",
  }
}

pub(crate) fn hotspot_from_error(
  err: &fastrender::Error,
  diagnostics: Option<&RenderDiagnostics>,
) -> Option<&'static str> {
  match err {
    fastrender::Error::Resource(_)
    | fastrender::Error::Navigation(_)
    | fastrender::Error::Io(_) => Some("fetch"),
    _ => diagnostics
      .and_then(|d| d.failure_stage)
      .map(hotspot_from_timeout_stage),
  }
}

fn maybe_apply_hotspot(
  progress: &mut PageProgress,
  previous: Option<&PageProgress>,
  hotspot: &str,
  authoritative: bool,
) {
  if authoritative {
    progress.hotspot = hotspot.to_string();
    return;
  }
  if !is_hotspot_unset(&progress.hotspot) {
    return;
  }
  let Some(prev) = previous else {
    progress.hotspot = hotspot.to_string();
    return;
  };
  if is_hotspot_unset(&prev.hotspot) {
    progress.hotspot = hotspot.to_string();
    return;
  }
  // `hotspot` edits are effectively "manual overrides" for failures: the runner historically
  // emitted `unknown` for error statuses, so any non-unknown value on a failing page is assumed to
  // be a deliberate human classification and should not be clobbered.
  //
  // For pages that were previously OK, `hotspot` is generated from timings and should be allowed
  // to change as failures appear/regress.
  if prev.status == ProgressStatus::Ok {
    progress.hotspot = hotspot.to_string();
  }
}

pub(crate) fn apply_diagnostics_to_progress(
  progress: &mut PageProgress,
  diagnostics: &RenderDiagnostics,
) {
  progress.failure_stage = diagnostics.failure_stage.map(ProgressStage::from);
  progress.timeout_stage = diagnostics.timeout_stage.map(ProgressStage::from);
}

pub(crate) fn populate_timeout_progress(
  progress: &mut PageProgress,
  stage: RenderStage,
  elapsed: Duration,
) {
  progress.status = ProgressStatus::Timeout;
  progress.auto_notes = format!("timeout at {stage} after {elapsed:?}");
  progress.hotspot = hotspot_from_timeout_stage(stage).to_string();
  progress.timeout_stage = Some(stage.into());
}

fn populate_timeout_progress_with_heartbeat(
  progress: &mut PageProgress,
  stage: RenderStage,
  elapsed: Duration,
  heartbeat_stage: Option<StageHeartbeat>,
) {
  populate_timeout_progress(progress, stage, elapsed);
  let Some(heartbeat_stage) = heartbeat_stage else {
    return;
  };
  ensure_auto_note_includes(progress, &format!("stage: {}", heartbeat_stage.as_str()));
  if let Some(mapped_stage) = progress_stage_from_heartbeat(heartbeat_stage) {
    progress.timeout_stage = Some(mapped_stage);
    progress.hotspot = hotspot_from_progress_stage(mapped_stage).to_string();
  }
}

fn render_worker(args: WorkerArgs) -> io::Result<()> {
  let _cleanup_delay_guard = WorkerCleanupDelayGuard::from_env();
  let heartbeat = StageHeartbeatWriter::new(args.stage_path.clone());
  let _heartbeat_guard = StageListenerGuard::new(heartbeat.listener());
  record_stage(StageHeartbeat::ReadCache);
  common::render_pipeline::apply_test_render_delay(Some(&args.stem));
  let started = Instant::now();
  let mut log = String::new();
  let current_sha = current_git_sha();

  let progress_before = read_progress(&args.progress_path);

  let cached = match read_cached_document(&args.cache_path) {
    Ok(doc) => doc,
    Err(e) => {
      let log_msg = format_error_with_chain(&e, true);
      let note_msg = format_error_with_chain(&e, args.verbose);
      log.push_str(&format!("Read error: {log_msg}\n"));
      let mut progress = PageProgress::new(url_hint_from_cache_path(&args.cache_path));
      progress.status = ProgressStatus::Error;
      progress.auto_notes = format!("read: {note_msg}");
      progress.failure_stage = heartbeat
        .last_stage()
        .and_then(progress_stage_from_heartbeat);
      if let Some(hotspot) = hotspot_from_error(&e, None) {
        maybe_apply_hotspot(&mut progress, progress_before.as_ref(), hotspot, false);
      }
      if progress.hotspot.trim().is_empty() {
        progress.hotspot = "unknown".to_string();
      }
      let progress = progress.merge_preserving_manual(progress_before, current_sha.as_deref());
      let _ =
        write_progress_with_sentinel(&args.progress_path, args.stage_path.as_deref(), &progress);
      if let Some(path) = &args.log_path {
        let _ = write_text_file(path, &log);
      }
      return Ok(());
    }
  };

  let mut url = cached.document.base_hint.clone();
  if let Ok(parsed) = url::Url::parse(&url) {
    url = parsed.to_string();
  }

  let hard_timeout = Duration::from_secs(args.timeout);
  let fetch_timeout = compute_fetch_timeout(hard_timeout, args.soft_timeout_ms);
  let serve_stale_when_deadline = cfg!(feature = "disk_cache")
    && std::env::var("FASTR_PAGESET_SERVE_STALE")
      .ok()
      .map(|raw| {
        !matches!(
          raw.trim().to_ascii_lowercase().as_str(),
          "0" | "false" | "no" | "off"
        )
      })
      .unwrap_or(true);

  log.push_str(&format!("=== {} ===\n", args.stem));
  log.push_str(&format!("Cache: {}\n", args.cache_path.display()));
  log.push_str(&format!("URL: {}\n", url));
  log.push_str(&format!(
    "User-Agent: {}\n",
    normalize_user_agent_for_log(&args.user_agent)
  ));
  log.push_str(&format!("Accept-Language: {}\n", args.accept_language));
  log.push_str(&format!(
    "Viewport: {}x{}\n",
    args.viewport.0, args.viewport.1
  ));
  log.push_str(&format!("DPR: {}\n", args.dpr));
  let compat_profile = args
    .compat
    .compat_profile_arg()
    .unwrap_or(CompatProfileArg::Standards)
    .as_str();
  let dom_compat = args
    .compat
    .dom_compat_arg()
    .unwrap_or(DomCompatArg::Standard)
    .as_str();
  log.push_str(&format!("Compat profile: {compat_profile}\n"));
  log.push_str(&format!("DOM compat: {dom_compat}\n"));
  if args.fonts.bundled_fonts {
    log.push_str("Fonts: bundled fixtures\n");
  }
  if !args.fonts.font_dir.is_empty() {
    let dirs = args
      .fonts
      .font_dir
      .iter()
      .map(|p| p.display().to_string())
      .collect::<Vec<_>>()
      .join(", ");
    log.push_str(&format!("Font dirs: {dirs}\n"));
  }
  if let Some(ct) = &cached.content_type {
    log.push_str(&format!("Content-Type: {ct}\n"));
  }
  log.push_str(&format!("HTML bytes: {}\n", cached.byte_len));
  let soft_timeout_desc = match args.soft_timeout_ms {
    Some(0) => "disabled".to_string(),
    Some(ms) => format!("{ms}ms"),
    None => "unset".to_string(),
  };
  log.push_str(&format!(
    "Timeouts: hard={}s, soft={}, fetch={}ms\n",
    args.timeout,
    soft_timeout_desc,
    fetch_timeout.as_millis()
  ));
  #[cfg(feature = "disk_cache")]
  {
    let max_age = if args.disk_cache.max_age_secs == 0 {
      "none".to_string()
    } else {
      format!("{}s", args.disk_cache.max_age_secs)
    };
    log.push_str(&format!(
      "Disk cache: max_bytes={} max_age={} writeback_under_deadline={}\n",
      args.disk_cache.max_bytes, max_age, args.disk_cache.writeback_under_deadline
    ));
    let stale_policy = if serve_stale_when_deadline {
      "use_stale_when_deadline"
    } else {
      "revalidate"
    };
    log.push_str(&format!("Disk cache stale policy: {stale_policy}\n"));
  }

  let dump_timeout_secs = args
    .dump_timeout
    .filter(|secs| *secs > 0)
    .unwrap_or_else(|| args.timeout.saturating_mul(2));
  let dump_soft_timeout_ms = compute_soft_timeout_ms(
    Duration::from_secs(dump_timeout_secs),
    args.dump_soft_timeout_ms,
  );
  let dump_soft_desc = match dump_soft_timeout_ms {
    Some(0) => "disabled".to_string(),
    Some(ms) => format!("{ms}ms"),
    None => "unset".to_string(),
  };
  log.push_str(&format!(
    "Dump timeouts: hard={}s, soft={dump_soft_desc}\n",
    dump_timeout_secs
  ));
  flush_log(&log, &args.log_path);

  let http = HttpFetcher::new()
    .with_timeout(fetch_timeout)
    .with_user_agent(args.user_agent.clone())
    .with_accept_language(args.accept_language.clone());
  let honor_http_freshness = cfg!(feature = "disk_cache") && !args.no_http_freshness;
  let memory_config = CachingFetcherConfig {
    honor_http_cache_freshness: honor_http_freshness,
    stale_policy: if serve_stale_when_deadline {
      CacheStalePolicy::UseStaleWhenDeadline
    } else {
      CacheStalePolicy::Revalidate
    },
    ..CachingFetcherConfig::default()
  };
  #[cfg(feature = "disk_cache")]
  let mut disk_config = args.disk_cache.to_config();
  #[cfg(feature = "disk_cache")]
  {
    disk_config.namespace = Some(common::render_pipeline::disk_cache_namespace(
      &args.user_agent,
      &args.accept_language,
    ));
  }
  #[cfg(feature = "disk_cache")]
  let fetcher: std::sync::Arc<dyn ResourceFetcher> = std::sync::Arc::new(
    DiskCachingFetcher::with_configs(http, ASSET_DIR, memory_config, disk_config),
  );
  #[cfg(not(feature = "disk_cache"))]
  let fetcher: std::sync::Arc<dyn ResourceFetcher> =
    std::sync::Arc::new(CachingFetcher::with_config(http, memory_config));
  let renderer_fetcher = std::sync::Arc::clone(&fetcher);

  let render_surface = RenderSurface {
    viewport: args.viewport,
    scroll_x: 0.0,
    scroll_y: 0.0,
    dpr: args.dpr,
    media_type: fastrender::style::media::MediaType::Screen,
    css_limit: args.css_limit,
    allow_partial: false,
    apply_meta_viewport: true,
    base_url: None,
    allow_file_from_http: args.resource_access.allow_file_from_http,
    block_mixed_content: args.resource_access.block_mixed_content,
    same_origin_subresources: args.resource_access.same_origin_subresources,
    allowed_subresource_origins: args.resource_access.allow_subresource_origin.clone(),
    trace_output: None,
    layout_parallelism: args.layout_parallel.parallelism(),
    font_config: args.fonts.to_font_config(),
    compat_profile: args.compat.compat_profile(),
    dom_compat_mode: args.compat.dom_compat_mode(),
  };

  let RenderConfigBundle {
    config,
    mut options,
  } = build_render_configs(&render_surface);

  options.diagnostics_level = args.diagnostics.to_level();
  if let Some(ms) = args.soft_timeout_ms {
    if ms > 0 {
      options.timeout = Some(Duration::from_millis(ms));
    }
  }
  if let Some(path) = &args.trace_out {
    options.trace_output = Some(path.clone());
  }

  let mut renderer =
    match common::render_pipeline::build_renderer_with_fetcher(config, renderer_fetcher) {
      Ok(r) => r,
      Err(e) => {
        let log_msg = format_error_with_chain(&e, true);
        let note_msg = format_error_with_chain(&e, args.verbose);
        log.push_str(&format!("Renderer init error: {log_msg}\n"));
        let mut progress = PageProgress::new(url);
        progress.status = ProgressStatus::Error;
        progress.auto_notes = format!("renderer init: {note_msg}");
        progress.failure_stage = heartbeat
          .last_stage()
          .and_then(progress_stage_from_heartbeat);
        if let Some(hotspot) = hotspot_from_error(&e, None) {
          maybe_apply_hotspot(&mut progress, progress_before.as_ref(), hotspot, false);
        }
        if progress.hotspot.trim().is_empty() {
          progress.hotspot = "unknown".to_string();
        }
        let progress = progress.merge_preserving_manual(progress_before, current_sha.as_deref());
        let _ =
          write_progress_with_sentinel(&args.progress_path, args.stage_path.as_deref(), &progress);
        if let Some(path) = &args.log_path {
          let _ = write_text_file(path, &log);
        }
        return Ok(());
      }
    };

  let mut doc = cached.document;
  log.push_str(&format!("Resource base: {}\n", doc.base_url));
  record_stage(StageHeartbeat::FollowRedirects);
  doc = follow_client_redirects(fetcher.as_ref(), doc, |line| {
    log.push_str(line);
    log.push('\n');
  });
  log.push_str(&format!("Final resource base: {}\n", doc.base_url));
  let dump_doc = doc.clone();

  record_stage(StageHeartbeat::CssInline);
  log.push_str("Stage: render\n");
  flush_log(&log, &args.log_path);
  let render_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
    render_document(&mut renderer, doc, &options)
  }));

  let elapsed_ms = started.elapsed().as_secs_f64() * 1000.0;

  let mut progress = PageProgress::new(url);
  progress.total_ms = Some(elapsed_ms);

  match render_result {
    Ok(Ok(result)) => {
      progress.status = ProgressStatus::Ok;
      progress.stages_ms = buckets_from_diagnostics(&result.diagnostics);
      apply_diagnostics_to_progress(&mut progress, &result.diagnostics);
      if result.diagnostics.stats.is_none() {
        log.push_str("Render stats unavailable; stages_ms left at 0ms.\n");
      }
      progress.hotspot = guess_hotspot(&progress.stages_ms).to_string();
      progress.diagnostics = result
        .diagnostics
        .stats
        .clone()
        .map(|stats| ProgressDiagnostics { stats: Some(stats) });
      log.push_str("Status: OK\n");
      append_fetch_error_summary(&mut log, &result.diagnostics);
      append_stage_summary(&mut log, &result.diagnostics);
      log_layout_parallelism(&result.diagnostics, |line| {
        log.push_str(line);
        log.push('\n');
      });
    }
    Ok(Err(err)) => {
      match &err {
        fastrender::Error::Render(RenderError::Timeout { stage, elapsed }) => {
          populate_timeout_progress_with_heartbeat(
            &mut progress,
            *stage,
            *elapsed,
            heartbeat.last_stage(),
          );
        }
        _ => {
          progress.status = ProgressStatus::Error;
          progress.auto_notes = format_error_with_chain(&err, args.verbose);
          // Avoid clobbering manual `hotspot` edits in committed progress files: only set the
          // inferred hotspot when the previous value is unset/unknown (or when explicitly marked
          // authoritative).
          if let Some(hotspot) = hotspot_from_error(&err, None) {
            maybe_apply_hotspot(&mut progress, progress_before.as_ref(), hotspot, false);
          } else {
            progress.hotspot = "unknown".to_string();
          }
        }
      }

      log.push_str(&format!("Status: {:?}\n", progress.status));
      log.push_str(&format!("Error: {}\n", format_error_with_chain(&err, true)));
    }
    Err(panic) => {
      progress.status = ProgressStatus::Panic;
      progress.auto_notes = panic_to_string(panic);
      progress.hotspot = "unknown".to_string();
      log.push_str("Status: PANIC\n");
      log.push_str(&format!("Panic: {}\n", progress.auto_notes));
    }
  }

  if is_bad_status(progress.status)
    && progress.timeout_stage.is_none()
    && progress.failure_stage.is_none()
  {
    let stage = heartbeat
      .last_stage()
      .and_then(progress_stage_from_heartbeat);
    progress.failure_stage = stage;
  }

  // Attribute cooperative failures using the on-disk stage timeline (also used by the parent for
  // hard-killed workers). This keeps timeout/error artifacts useful for triage even when the
  // renderer exits via a soft timeout and doesn't produce structured timing stats.
  if is_bad_status(progress.status)
    && progress.total_ms.is_some()
    && progress.stages_ms.sum() == 0.0
  {
    let total_ms = progress.total_ms.unwrap_or(0.0);
    let timeline_total_ms = total_ms.round().max(0.0) as u64;
    progress.stages_ms = args
      .stage_path
      .as_deref()
      .and_then(|path| stage_buckets_from_timeline(path, timeline_total_ms))
      .or_else(|| {
        progress
          .timeout_stage
          .or(progress.failure_stage)
          .map(|stage| stage_buckets_for_progress_stage(stage, total_ms))
      })
      .unwrap_or_default();
    progress.stages_ms.rescale_to_total(total_ms);
  }

  if progress.auto_notes.trim().is_empty() {
    progress.auto_notes = match progress.status {
      ProgressStatus::Ok => String::new(),
      ProgressStatus::Timeout => "timeout".to_string(),
      ProgressStatus::Panic => "panic".to_string(),
      ProgressStatus::Error => "error".to_string(),
    };
  }
  if progress.hotspot.trim().is_empty() {
    progress.hotspot = guess_hotspot(&progress.stages_ms).to_string();
  }

  let progress = progress.merge_preserving_manual(progress_before, current_sha.as_deref());
  let wrote_progress =
    write_progress_with_sentinel(&args.progress_path, args.stage_path.as_deref(), &progress);
  if let Err(err) = wrote_progress {
    log.push_str(&format!(
      "Progress write failed (skipping dumps): {}\n",
      format_error_with_chain(&err, args.verbose)
    ));
    flush_log(&log, &args.log_path);
    record_stage(StageHeartbeat::Done);
    std::process::exit(0);
  }

  if let Some(level) = dump_level_for_progress(&args, &progress) {
    log.push_str(&format!(
      "Capturing {} dump for {}...\n",
      level.as_str(),
      progress.status.as_str()
    ));
    // Flush before attempting the (optional) dump capture so a later kill doesn't discard the
    // already-captured progress metadata.
    flush_log(&log, &args.log_path);
    capture_dump_for_page(
      level,
      &args,
      &render_surface,
      &fetcher,
      &dump_doc,
      dump_soft_timeout_ms,
      &mut log,
    );
  }

  flush_log(&log, &args.log_path);

  record_stage(StageHeartbeat::Done);
  if let Some(delay) = std::env::var("FASTR_TEST_POST_DONE_SLEEP_MS")
    .ok()
    .and_then(|v| v.parse::<u64>().ok())
  {
    std::thread::sleep(Duration::from_millis(delay));
  }
  std::process::exit(0);
}

fn append_fetch_error_summary(log: &mut String, diagnostics: &RenderDiagnostics) {
  if diagnostics.fetch_errors.is_empty() {
    return;
  }
  log.push_str(&format!(
    "Fetch errors: {}\n",
    diagnostics.fetch_errors.len()
  ));
  for err in diagnostics.fetch_errors.iter().take(25) {
    let kind = match err.kind {
      ResourceKind::Document => "document",
      ResourceKind::Stylesheet => "stylesheet",
      ResourceKind::Image => "image",
      ResourceKind::Font => "font",
      ResourceKind::Other => "resource",
    };
    let mut meta = Vec::new();
    if let Some(status) = err.status {
      meta.push(format!("status {status}"));
    }
    if let Some(final_url) = &err.final_url {
      if final_url != &err.url {
        meta.push(format!("final {final_url}"));
      }
    }
    let meta = if meta.is_empty() {
      String::new()
    } else {
      format!(" ({})", meta.join(", "))
    };
    log.push_str(&format!("- {kind}: {}{meta}: {}\n", err.url, err.message));
  }
}

fn append_stage_summary(log: &mut String, diagnostics: &RenderDiagnostics) {
  let Some(stats) = diagnostics.stats.as_ref() else {
    return;
  };
  let t = &stats.timings;
  let fmt = |v: Option<f64>| -> String { v.map(|ms| format!("{ms:.2}ms")).unwrap_or("-".into()) };
  log.push_str("Stage timings:\n");
  log.push_str(&format!("  html_decode: {}\n", fmt(t.html_decode_ms)));
  log.push_str(&format!("  dom_parse: {}\n", fmt(t.dom_parse_ms)));
  log.push_str(&format!("  dom_html5ever: {}\n", fmt(t.dom_html5ever_ms)));
  log.push_str(&format!("  dom_convert: {}\n", fmt(t.dom_convert_ms)));
  log.push_str(&format!(
    "  dom_shadow_attach: {}\n",
    fmt(t.dom_shadow_attach_ms)
  ));
  log.push_str(&format!("  dom_compat: {}\n", fmt(t.dom_compat_ms)));
  log.push_str(&format!(
    "  dom_meta_viewport: {}\n",
    fmt(t.dom_meta_viewport_ms)
  ));
  log.push_str(&format!("  dom_clone: {}\n", fmt(t.dom_clone_ms)));
  log.push_str(&format!("  dom_top_layer: {}\n", fmt(t.dom_top_layer_ms)));
  log.push_str(&format!("  css_inlining: {}\n", fmt(t.css_inlining_ms)));
  log.push_str(&format!("  css_parse: {}\n", fmt(t.css_parse_ms)));
  log.push_str(&format!("  cascade: {}\n", fmt(t.cascade_ms)));
  log.push_str(&format!("  box_tree: {}\n", fmt(t.box_tree_ms)));
  log.push_str(&format!("  layout: {}\n", fmt(t.layout_ms)));
  log.push_str(&format!("  text_fallback: {}\n", fmt(t.text_fallback_ms)));
  log.push_str(&format!("  text_shape: {}\n", fmt(t.text_shape_ms)));
  log.push_str(&format!("  paint_build: {}\n", fmt(t.paint_build_ms)));
  log.push_str(&format!("  paint_optimize: {}\n", fmt(t.paint_optimize_ms)));
  log.push_str(&format!(
    "  paint_rasterize: {}\n",
    fmt(t.paint_rasterize_ms)
  ));
  log.push_str(&format!("  text_rasterize: {}\n", fmt(t.text_rasterize_ms)));
  log.push_str(&format!("  encode: {}\n", fmt(t.encode_ms)));
}

fn panic_to_string(panic: Box<dyn std::any::Any + Send + 'static>) -> String {
  panic
    .downcast_ref::<&str>()
    .map(|s| s.to_string())
    .or_else(|| panic.downcast_ref::<String>().cloned())
    .unwrap_or_else(|| "unknown panic".to_string())
}

fn trace_issue_message(trace_path: &Path, reason: Option<&str>) -> Option<String> {
  let base = match fs::metadata(trace_path) {
    Ok(meta) => {
      let size = meta.len();
      if size < MIN_TRACE_BYTES {
        format!(
          "Trace file {} is only {} bytes (likely partial from a crash or kill).",
          trace_path.display(),
          size
        )
      } else {
        return None;
      }
    }
    Err(err) if err.kind() == io::ErrorKind::NotFound => format!(
      "Trace file {} was not written (worker likely crashed or was killed before tracing).",
      trace_path.display()
    ),
    Err(err) => format!(
      "Trace file {} could not be inspected: {}",
      trace_path.display(),
      err
    ),
  };
  let mut message = base;
  if let Some(reason) = reason {
    message.push(' ');
    message.push_str(reason);
  }
  Some(message)
}

fn write_text_file(path: &Path, contents: &str) -> io::Result<()> {
  atomic_write(path, contents.as_bytes())
}

fn write_json_file(path: &Path, value: &impl Serialize) -> io::Result<()> {
  if let Some(parent) = path.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent)?;
    }
  }
  let json = serde_json::to_string_pretty(value)
    .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
  fs::write(path, format!("{json}\n"))
}

fn write_pipeline_dumps(
  level: DumpLevel,
  stem: &str,
  dump_dir: &Path,
  artifacts: &RenderArtifacts,
  log: &mut String,
) -> io::Result<bool> {
  let base_dir = dump_dir.join(stem);
  fs::create_dir_all(&base_dir)?;

  let dom = artifacts.dom.as_ref();
  let styled = artifacts.styled_tree.as_ref();
  let box_tree = artifacts.box_tree.as_ref();
  let fragment_tree = artifacts.fragment_tree.as_ref();
  let display_list = artifacts.display_list.as_ref();

  let have_all = dom.is_some()
    && styled.is_some()
    && box_tree.is_some()
    && fragment_tree.is_some()
    && display_list.is_some();
  let mut wrote_any = false;

  if have_all {
    let snapshot = snapshot::snapshot_pipeline(
      dom.expect("snapshot precondition"),
      styled.expect("snapshot precondition"),
      box_tree.expect("snapshot precondition"),
      fragment_tree.expect("snapshot precondition"),
      display_list.expect("snapshot precondition"),
    );
    write_json_file(&base_dir.join("snapshot.json"), &snapshot)?;
    wrote_any = true;
  } else {
    let mut missing = Vec::new();
    if dom.is_none() {
      missing.push("dom");
    }
    if styled.is_none() {
      missing.push("styled");
    }
    if box_tree.is_none() {
      missing.push("box_tree");
    }
    if fragment_tree.is_none() {
      missing.push("fragment_tree");
    }
    if display_list.is_none() {
      missing.push("display_list");
    }
    log.push_str(&format!(
      "Dump artifacts missing {}; writing partial dumps when available\n",
      missing.join(", ")
    ));
  }

  let should_write_parts = level == DumpLevel::Full || !have_all;
  if should_write_parts {
    if let Some(dom) = dom {
      write_json_file(&base_dir.join("dom.json"), &snapshot::snapshot_dom(dom))?;
      wrote_any = true;
    } else {
      log.push_str("Dump skipped: missing DOM artifact\n");
    }
    if let Some(styled) = styled {
      write_json_file(
        &base_dir.join("styled.json"),
        &snapshot::snapshot_styled(styled),
      )?;
      wrote_any = true;
    } else {
      log.push_str("Dump skipped: missing styled tree\n");
    }
    if let Some(box_tree) = box_tree {
      write_json_file(
        &base_dir.join("box_tree.json"),
        &snapshot::snapshot_box_tree(box_tree),
      )?;
      wrote_any = true;
    } else {
      log.push_str("Dump skipped: missing box tree\n");
    }
    if let Some(fragment_tree) = fragment_tree {
      write_json_file(
        &base_dir.join("fragment_tree.json"),
        &snapshot::snapshot_fragment_tree(fragment_tree),
      )?;
      wrote_any = true;
    } else {
      log.push_str("Dump skipped: missing fragment tree\n");
    }
    if let Some(display_list) = display_list {
      write_json_file(
        &base_dir.join("display_list.json"),
        &snapshot::snapshot_display_list(display_list),
      )?;
      wrote_any = true;
    } else {
      log.push_str("Dump skipped: missing display list\n");
    }
  }

  if !wrote_any {
    log.push_str("Dump skipped: no artifacts were captured\n");
  }
  Ok(wrote_any)
}

fn capture_dump_for_page(
  level: DumpLevel,
  args: &WorkerArgs,
  render_surface: &RenderSurface,
  fetcher: &std::sync::Arc<dyn ResourceFetcher>,
  doc: &PreparedDocument,
  dump_soft_timeout_ms: Option<u64>,
  log: &mut String,
) {
  if std::env::var("FASTR_TEST_DUMP_PANIC").is_ok() {
    let should_panic = match std::env::var("FASTR_TEST_DUMP_PANIC_STEM").ok() {
      Some(filter) => filter
        .split(',')
        .map(|s| s.trim())
        .any(|candidate| candidate == args.stem),
      None => true,
    };
    if should_panic {
      panic!("FASTR_TEST_DUMP_PANIC");
    }
  }
  if let Some(delay) = std::env::var("FASTR_TEST_DUMP_DELAY_MS")
    .ok()
    .and_then(|v| v.parse::<u64>().ok())
  {
    let should_sleep = match std::env::var("FASTR_TEST_DUMP_DELAY_STEM").ok() {
      Some(filter) => filter
        .split(',')
        .map(|s| s.trim())
        .any(|candidate| candidate == args.stem),
      None => true,
    };
    if should_sleep {
      std::thread::sleep(Duration::from_millis(delay));
    }
  }

  let RenderConfigBundle {
    config,
    mut options,
  } = build_render_configs(render_surface);
  options.diagnostics_level = DiagnosticsLevel::Verbose;
  if let Some(ms) = dump_soft_timeout_ms {
    if ms > 0 {
      options.timeout = Some(Duration::from_millis(ms));
    }
  }

  let mut renderer = match common::render_pipeline::build_renderer_with_fetcher(
    config,
    std::sync::Arc::clone(fetcher),
  ) {
    Ok(r) => r,
    Err(e) => {
      let msg = format_error_with_chain(&e, args.verbose);
      log.push_str(&format!("Dump renderer init error: {msg}\n"));
      return;
    }
  };

  let dump_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
    render_document_with_artifacts(&mut renderer, doc.clone(), &options, level.to_request())
  }));

  match dump_result {
    Ok(Ok(report)) => {
      match write_pipeline_dumps(level, &args.stem, &args.dump_dir, &report.artifacts, log) {
        Ok(true) => {
          log.push_str(&format!(
            "Dump written to {}\n",
            args.dump_dir.join(&args.stem).display()
          ));
        }
        Ok(false) => {}
        Err(err) => {
          log.push_str(&format!(
            "Dump write error: {}\n",
            format_error_with_chain(&err, args.verbose)
          ));
        }
      }
    }
    Ok(Err(err)) => {
      log.push_str(&format!(
        "Dump render error: {}\n",
        format_error_with_chain(&err, args.verbose)
      ));
    }
    Err(panic) => {
      log.push_str(&format!("Dump render panic: {}\n", panic_to_string(panic)));
    }
  }
}

#[derive(Clone)]
struct StageHeartbeatWriter {
  path: Option<PathBuf>,
  timeline_path: Option<PathBuf>,
  started: Instant,
  last: Arc<Mutex<Option<StageHeartbeat>>>,
}

impl StageHeartbeatWriter {
  fn new(path: Option<PathBuf>) -> Self {
    let timeline_path = path.as_ref().map(|path| stage_timeline_path(path));
    let started = Instant::now();
    // Best-effort cleanup so a reused stage path doesn't leave stale timelines behind.
    if let Some(timeline_path) = timeline_path.as_ref() {
      let _ = (|| -> io::Result<()> {
        if let Some(parent) = timeline_path.parent() {
          if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent)?;
          }
        }
        let file = OpenOptions::new()
          .write(true)
          .create(true)
          .truncate(true)
          .open(timeline_path)?;
        file.sync_all()?;
        Ok(())
      })();
    }
    Self {
      path,
      timeline_path,
      started,
      last: Arc::new(Mutex::new(None)),
    }
  }

  fn listener(&self) -> Arc<dyn Fn(StageHeartbeat) + Send + Sync> {
    let writer = self.clone();
    Arc::new(move |stage| writer.record(stage))
  }

  fn last_stage(&self) -> Option<StageHeartbeat> {
    self.last.lock().ok().and_then(|guard| *guard)
  }

  fn record(&self, stage: StageHeartbeat) {
    if stage == StageHeartbeat::Done {
      return;
    }
    let Some(path) = &self.path else {
      if let Ok(mut guard) = self.last.lock() {
        if guard.as_ref() == Some(&stage) {
          return;
        }
        *guard = Some(stage);
      }
      return;
    };
    {
      if let Ok(guard) = self.last.lock() {
        if guard.as_ref() == Some(&stage) {
          return;
        }
      }
    }
    let tmp_path = stage_tmp_path(path);
    let contents = format!("{}\n", stage.as_str());
    let write_result = (|| -> io::Result<()> {
      if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
          fs::create_dir_all(parent)?;
        }
      }
      let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&tmp_path)?;
      file.write_all(contents.as_bytes())?;
      file.sync_all()?;
      fs::rename(&tmp_path, path)?;
      Ok(())
    })();
    if write_result.is_ok() {
      if let Some(timeline_path) = &self.timeline_path {
        let _ = (|| -> io::Result<()> {
          let elapsed_ms = u64::try_from(self.started.elapsed().as_millis()).unwrap_or(u64::MAX);
          let line = format!("{elapsed_ms} {}\n", stage.as_str());
          if let Some(parent) = timeline_path.parent() {
            if !parent.as_os_str().is_empty() {
              fs::create_dir_all(parent)?;
            }
          }
          let mut timeline = OpenOptions::new()
            .write(true)
            .create(true)
            .append(true)
            .open(timeline_path)?;
          timeline.write_all(line.as_bytes())?;
          timeline.sync_data()?;
          Ok(())
        })();
      }
      if let Ok(mut guard) = self.last.lock() {
        *guard = Some(stage);
      }
    }
  }
}

struct StageListenerGuard;

impl StageListenerGuard {
  fn new(listener: Arc<dyn Fn(StageHeartbeat) + Send + Sync>) -> Self {
    set_stage_listener(Some(listener));
    Self
  }
}

impl Drop for StageListenerGuard {
  fn drop(&mut self) {
    set_stage_listener(None);
  }
}

fn stage_tmp_path(path: &Path) -> PathBuf {
  let tmp_name = path
    .file_name()
    .map(|name| format!("{}.tmp", name.to_string_lossy()))
    .unwrap_or_else(|| "stage.tmp".to_string());
  path.with_file_name(tmp_name)
}

fn stage_timeline_path(path: &Path) -> PathBuf {
  path.with_extension("stage.timeline")
}

fn progress_sentinel_path(stage_path: &Path) -> PathBuf {
  stage_path.with_extension("progress")
}

fn write_progress_with_sentinel(
  progress_path: &Path,
  stage_path: Option<&Path>,
  progress: &PageProgress,
) -> io::Result<()> {
  write_progress(progress_path, progress)?;
  if let Some(stage_path) = stage_path {
    if std::env::var_os("FASTR_TEST_SKIP_PROGRESS_SENTINEL").is_some() {
      return Ok(());
    }
    let sentinel_path = progress_sentinel_path(stage_path);
    // Best-effort marker so the parent can distinguish "worker wrote progress for this run"
    // from a stale progress artifact left over from a previous invocation.
    atomic_write(&sentinel_path, b"written\n")?;
  }
  Ok(())
}

fn read_stage_file(path: &Path) -> Option<StageHeartbeat> {
  let raw = fs::read_to_string(path).ok()?;
  StageHeartbeat::from_str(raw.trim())
}

fn read_stage_heartbeat(path: &Path) -> Option<StageHeartbeat> {
  read_stage_file(path).or_else(|| read_stage_file(&stage_tmp_path(path)))
}

fn stage_buckets_from_timeline(stage_path: &Path, total_ms: u64) -> Option<StageBuckets> {
  let raw = fs::read_to_string(stage_timeline_path(stage_path)).ok()?;
  let mut entries: Vec<(u64, StageHeartbeat)> = Vec::new();
  for line in raw.lines() {
    let mut parts = line.split_whitespace();
    let Some(ms_raw) = parts.next() else {
      continue;
    };
    let Some(stage_raw) = parts.next() else {
      continue;
    };
    let Ok(ms) = ms_raw.parse::<u64>() else {
      continue;
    };
    let Some(stage) = StageHeartbeat::from_str(stage_raw) else {
      continue;
    };
    if stage == StageHeartbeat::Done {
      continue;
    }
    entries.push((ms, stage));
  }
  let first_stage = entries.first().map(|(_, stage)| *stage)?;

  let mut buckets = StageBuckets::default();
  let mut prev_stage = first_stage;
  let mut prev_ms = 0u64;
  for (at_ms, stage) in entries.iter().skip(1) {
    let at_ms = (*at_ms).min(total_ms);
    if at_ms < prev_ms {
      continue;
    }
    let dur_ms = at_ms - prev_ms;
    match prev_stage.hotspot() {
      "fetch" => buckets.fetch += dur_ms as f64,
      "css" => buckets.css += dur_ms as f64,
      "cascade" => buckets.cascade += dur_ms as f64,
      "layout" => buckets.layout += dur_ms as f64,
      "paint" => buckets.paint += dur_ms as f64,
      _ => {}
    }
    prev_ms = at_ms;
    prev_stage = *stage;
    if prev_ms >= total_ms {
      break;
    }
  }

  if prev_ms < total_ms {
    let dur_ms = total_ms - prev_ms;
    match prev_stage.hotspot() {
      "fetch" => buckets.fetch += dur_ms as f64,
      "css" => buckets.css += dur_ms as f64,
      "cascade" => buckets.cascade += dur_ms as f64,
      "layout" => buckets.layout += dur_ms as f64,
      "paint" => buckets.paint += dur_ms as f64,
      _ => {}
    }
  }

  Some(buckets)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ExitStatusSummary {
  code: Option<i32>,
  signal: Option<i32>,
}

fn summarize_exit_status(status: &ExitStatus) -> ExitStatusSummary {
  ExitStatusSummary {
    code: status.code(),
    signal: {
      #[cfg(unix)]
      {
        status.signal()
      }
      #[cfg(not(unix))]
      {
        None
      }
    },
  }
}

fn format_exit_status(status: ExitStatusSummary) -> String {
  match (status.code, status.signal) {
    (Some(code), Some(signal)) => format!("code {code} (signal {signal})"),
    (Some(code), None) => format!("code {code}"),
    (None, Some(signal)) => format!("signal {signal}"),
    (None, None) => "unknown status".to_string(),
  }
}

fn synthesize_missing_progress_note(status: ExitStatusSummary) -> String {
  format!(
    "worker exited (exit {}) without writing progress",
    format_exit_status(status)
  )
}

fn ensure_auto_note_includes(progress: &mut PageProgress, note: &str) {
  if progress.auto_notes.contains(note) {
    return;
  }
  if progress.auto_notes.trim().is_empty() {
    progress.auto_notes = note.to_string();
  } else {
    progress.auto_notes = format!("{}\n{note}", progress.auto_notes);
  }
}

fn append_timeout_stderr_note(stderr_path: &Path, elapsed: Duration) {
  if let Ok(mut file) = OpenOptions::new()
    .create(true)
    .append(true)
    .open(stderr_path)
  {
    let _ = writeln!(
      file,
      "parent killed worker after {:.2}s hard timeout",
      elapsed.as_secs_f64()
    );
  }
}

fn append_log_line(path: &Path, line: &str) -> io::Result<()> {
  if let Some(parent) = path.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent)?;
    }
  }
  let mut file = OpenOptions::new().append(true).create(true).open(path)?;
  writeln!(file, "{line}")
}

fn flush_log(log: &str, path: &Option<PathBuf>) {
  if let Some(path) = path {
    let _ = write_text_file(path, log);
  }
}

fn stderr_tail(stderr_path: &Path, max_bytes: usize, max_lines: usize) -> Option<String> {
  let data = fs::read(stderr_path).ok()?;
  let slice = if data.len() > max_bytes {
    &data[data.len().saturating_sub(max_bytes)..]
  } else {
    data.as_slice()
  };
  let text = String::from_utf8_lossy(slice);
  let mut lines: Vec<&str> = text.lines().rev().take(max_lines).collect();
  if lines.is_empty() {
    return None;
  }
  lines.reverse();
  Some(lines.join("\n"))
}

fn add_stage_buckets(into: &mut StageBuckets, from: &StageBuckets) {
  into.fetch += from.fetch;
  into.css += from.css;
  into.cascade += from.cascade;
  into.layout += from.layout;
  into.paint += from.paint;
}

fn divide_stage_buckets(total: &StageBuckets, divisor: f64) -> StageBuckets {
  if divisor == 0.0 {
    return StageBuckets::default();
  }
  StageBuckets {
    fetch: total.fetch / divisor,
    css: total.css / divisor,
    cascade: total.cascade / divisor,
    layout: total.layout / divisor,
    paint: total.paint / divisor,
  }
}

fn subtract_stage_buckets(current: &StageBuckets, baseline: &StageBuckets) -> StageBuckets {
  StageBuckets {
    fetch: current.fetch - baseline.fetch,
    css: current.css - baseline.css,
    cascade: current.cascade - baseline.cascade,
    layout: current.layout - baseline.layout,
    paint: current.paint - baseline.paint,
  }
}

fn is_bad_status(status: ProgressStatus) -> bool {
  matches!(
    status,
    ProgressStatus::Timeout | ProgressStatus::Panic | ProgressStatus::Error
  )
}

fn status_label(status: Option<ProgressStatus>) -> &'static str {
  status.map(|s| s.as_str()).unwrap_or("missing")
}

fn format_stage_delta(delta: &StageBuckets) -> String {
  format!(
    "stages_ms=fetch:{:+.2} css:{:+.2} cascade:{:+.2} layout:{:+.2} paint:{:+.2}",
    delta.fetch, delta.css, delta.cascade, delta.layout, delta.paint
  )
}

fn format_percent(delta: Option<f64>) -> String {
  delta
    .map(|p| format!("{:+.2}%", p))
    .unwrap_or_else(|| "n/a".to_string())
}

fn read_progress_dir(dir: &Path) -> io::Result<Vec<LoadedProgress>> {
  let mut files: Vec<PathBuf> = fs::read_dir(dir)
    .map_err(|e| io::Error::new(e.kind(), format!("{}: {}", dir.display(), e)))?
    .filter_map(|entry| entry.ok().map(|e| e.path()))
    .filter(|path| path.extension().map(|ext| ext == "json").unwrap_or(false))
    .collect();

  files.sort();

  if files.is_empty() {
    return Err(io::Error::new(
      io::ErrorKind::NotFound,
      format!("no progress files found in {}", dir.display()),
    ));
  }

  let mut progresses = Vec::new();
  for path in files {
    let contents = fs::read_to_string(&path).map_err(|e| {
      io::Error::new(
        e.kind(),
        format!("failed to read {}: {}", path.display(), e),
      )
    })?;
    let progress: PageProgress = serde_json::from_str(&contents).map_err(|e| {
      io::Error::new(
        io::ErrorKind::InvalidData,
        format!("failed to parse {}: {}", path.display(), e),
      )
    })?;
    let stem = path
      .file_stem()
      .map(|s| s.to_string_lossy().to_string())
      .unwrap_or_else(|| path.display().to_string());
    let stats = progress
      .diagnostics
      .as_ref()
      .and_then(|diag| diag.stats.clone());
    progresses.push(LoadedProgress {
      stem,
      progress,
      stats,
    });
  }
  Ok(progresses)
}

fn read_progress_dir_allow_empty(dir: &Path) -> io::Result<Vec<LoadedProgress>> {
  if !dir.exists() {
    return Ok(Vec::new());
  }
  let mut files: Vec<PathBuf> = fs::read_dir(dir)
    .map_err(|e| io::Error::new(e.kind(), format!("{}: {}", dir.display(), e)))?
    .filter_map(|entry| entry.ok().map(|e| e.path()))
    .filter(|path| path.extension().map(|ext| ext == "json").unwrap_or(false))
    .collect();

  files.sort();

  let mut progresses = Vec::new();
  for path in files {
    let contents = fs::read_to_string(&path).map_err(|e| {
      io::Error::new(
        e.kind(),
        format!("failed to read {}: {}", path.display(), e),
      )
    })?;
    let progress: PageProgress = serde_json::from_str(&contents).map_err(|e| {
      io::Error::new(
        io::ErrorKind::InvalidData,
        format!("failed to parse {}: {}", path.display(), e),
      )
    })?;
    let stem = path
      .file_stem()
      .map(|s| s.to_string_lossy().to_string())
      .unwrap_or_else(|| path.display().to_string());
    let stats = progress
      .diagnostics
      .as_ref()
      .and_then(|diag| diag.stats.clone());
    progresses.push(LoadedProgress {
      stem,
      progress,
      stats,
    });
  }
  Ok(progresses)
}

#[derive(Debug, Default)]
struct ProgressLoadOutcome {
  progresses: Vec<LoadedProgress>,
  warnings: Vec<String>,
}

fn read_progress_dir_tolerant(dir: &Path) -> ProgressLoadOutcome {
  let mut outcome = ProgressLoadOutcome::default();

  let entries = match fs::read_dir(dir) {
    Ok(entries) => entries,
    Err(err) => {
      outcome.warnings.push(format!("{}: {}", dir.display(), err));
      return outcome;
    }
  };

  let mut files: Vec<PathBuf> = entries
    .filter_map(|entry| entry.ok().map(|e| e.path()))
    .filter(|path| path.extension().map(|ext| ext == "json").unwrap_or(false))
    .collect();
  files.sort();

  for path in files {
    let contents = match fs::read_to_string(&path) {
      Ok(contents) => contents,
      Err(err) => {
        outcome
          .warnings
          .push(format!("{}: failed to read: {}", path.display(), err));
        continue;
      }
    };

    match serde_json::from_str::<PageProgress>(&contents) {
      Ok(progress) => {
        let stem = path
          .file_stem()
          .map(|s| s.to_string_lossy().to_string())
          .unwrap_or_else(|| path.display().to_string());
        let stats = progress
          .diagnostics
          .as_ref()
          .and_then(|diag| diag.stats.clone());
        outcome.progresses.push(LoadedProgress {
          stem,
          progress,
          stats,
        });
      }
      Err(err) => {
        outcome.warnings.push(format!(
          "{}: failed to parse progress JSON: {}",
          path.display(),
          err
        ));
      }
    }
  }

  outcome
}

fn entry_matches_criterion(progress: &LoadedProgress, criterion: &ProgressCriterion) -> bool {
  match criterion {
    ProgressCriterion::Status(set) => set.contains(&progress.progress.status),
    ProgressCriterion::Slow {
      threshold_ms,
      ok_only,
    } => {
      if *ok_only && progress.progress.status != ProgressStatus::Ok {
        return false;
      }
      progress
        .progress
        .total_ms
        .map(|ms| ms > *threshold_ms)
        .unwrap_or(false)
    }
    ProgressCriterion::Hotspot(hotspot) => normalize_hotspot(&progress.progress.hotspot)
      .to_ascii_lowercase()
      .eq(hotspot),
  }
}

fn apply_top_slowest<'a>(
  entries: Vec<&'a LoadedProgress>,
  top_slowest: Option<usize>,
) -> Vec<&'a LoadedProgress> {
  let Some(limit) = top_slowest else {
    return entries;
  };
  if limit == 0 {
    return Vec::new();
  }
  let mut with_timings: Vec<&LoadedProgress> = entries
    .into_iter()
    .filter(|entry| entry.progress.total_ms.is_some())
    .collect();
  with_timings.sort_by(|a, b| {
    let a_ms = a.progress.total_ms.unwrap_or(0.0);
    let b_ms = b.progress.total_ms.unwrap_or(0.0);
    b_ms.total_cmp(&a_ms).then_with(|| a.stem.cmp(&b.stem))
  });
  with_timings.truncate(limit);
  with_timings
}

fn select_progress_entries<'a>(
  progresses: &'a [LoadedProgress],
  selection: &ProgressSelection,
) -> Vec<&'a LoadedProgress> {
  let mut filtered: Vec<&LoadedProgress> = if selection.criteria.is_empty() {
    progresses.iter().collect()
  } else if selection.union {
    progresses
      .iter()
      .filter(|entry| {
        selection
          .criteria
          .iter()
          .any(|criterion| entry_matches_criterion(entry, criterion))
      })
      .collect()
  } else {
    progresses
      .iter()
      .filter(|entry| {
        selection
          .criteria
          .iter()
          .all(|criterion| entry_matches_criterion(entry, criterion))
      })
      .collect()
  };

  apply_top_slowest(std::mem::take(&mut filtered), selection.top_slowest)
}

fn apply_shard_filter<T>(items: Vec<T>, shard: Option<(usize, usize)>) -> Vec<T> {
  let Some((index, total)) = shard else {
    return items;
  };
  items
    .into_iter()
    .enumerate()
    .filter(|(idx, _)| idx % total == index)
    .map(|(_, item)| item)
    .collect()
}

fn summarize_status(progresses: &[LoadedProgress]) -> StatusCounts {
  let mut counts = StatusCounts::default();
  for entry in progresses {
    match entry.progress.status {
      ProgressStatus::Ok => counts.ok += 1,
      ProgressStatus::Timeout => counts.timeout += 1,
      ProgressStatus::Panic => counts.panic += 1,
      ProgressStatus::Error => counts.error += 1,
    }
  }
  counts
}

fn push_opt_usize(parts: &mut Vec<String>, label: &str, value: Option<usize>) {
  if let Some(v) = value {
    parts.push(format!("{label}={v}"));
  }
}

fn push_opt_u64(parts: &mut Vec<String>, label: &str, value: Option<u64>) {
  if let Some(v) = value {
    parts.push(format!("{label}={v}"));
  }
}

fn saturating_usize_from_u64(value: u64) -> usize {
  usize::try_from(value).unwrap_or(usize::MAX)
}

fn format_bytes(bytes: u64) -> String {
  const UNITS: [&str; 5] = ["B", "KiB", "MiB", "GiB", "TiB"];
  let mut value = bytes as f64;
  let mut idx = 0usize;
  while value >= 1024.0 && idx + 1 < UNITS.len() {
    value /= 1024.0;
    idx += 1;
  }
  if idx == 0 {
    format!("{bytes}B")
  } else {
    format!("{value:.1}{}", UNITS[idx])
  }
}

fn push_opt_ms(parts: &mut Vec<String>, label: &str, value: Option<f64>) {
  if let Some(v) = value {
    parts.push(format!("{label}={v:.2}ms", v = v));
  }
}

fn nodes_summary(counts: &RenderCounts) -> Option<String> {
  let mut parts = Vec::new();
  push_opt_usize(&mut parts, "dom", counts.dom_nodes);
  push_opt_usize(&mut parts, "styled", counts.styled_nodes);
  push_opt_usize(&mut parts, "boxes", counts.box_nodes);
  push_opt_usize(&mut parts, "fragments", counts.fragments);
  if parts.is_empty() {
    None
  } else {
    Some(parts.join(" "))
  }
}

fn text_summary(counts: &RenderCounts) -> Option<String> {
  let mut parts = Vec::new();
  push_opt_usize(&mut parts, "runs", counts.shaped_runs);
  push_opt_usize(&mut parts, "glyphs", counts.glyphs);
  push_opt_usize(&mut parts, "fallback_hits", counts.fallback_cache_hits);
  push_opt_usize(&mut parts, "fallback_misses", counts.fallback_cache_misses);
  push_opt_usize(
    &mut parts,
    "fallback_glyph_evict",
    counts.fallback_cache_glyph_evictions,
  );
  push_opt_usize(
    &mut parts,
    "fallback_cluster_evict",
    counts.fallback_cache_cluster_evictions,
  );
  push_opt_usize(&mut parts, "fallback_clears", counts.fallback_cache_clears);
  push_opt_usize(
    &mut parts,
    "last_resort_fallbacks",
    counts.last_resort_font_fallbacks,
  );
  if let Some(samples) = &counts.last_resort_font_fallback_samples {
    if !samples.is_empty() {
      parts.push(format!("last_resort_samples=[{}]", samples.join("; ")));
    }
  }
  if parts.is_empty() {
    None
  } else {
    Some(parts.join(" "))
  }
}

fn text_summary_with_timings(stats: &RenderStats) -> Option<String> {
  let mut sections = Vec::new();
  if let Some(summary) = text_summary(&stats.counts) {
    sections.push(summary);
  }
  let mut timing_parts = Vec::new();
  push_opt_ms(
    &mut timing_parts,
    "text_shape_ms",
    stats.timings.text_shape_ms,
  );
  push_opt_ms(
    &mut timing_parts,
    "text_fallback_ms",
    stats.timings.text_fallback_ms,
  );
  if !timing_parts.is_empty() {
    sections.push(format!("timings {}", timing_parts.join(" ")));
  }
  if sections.is_empty() {
    None
  } else {
    Some(sections.join(" | "))
  }
}

fn cascade_summary(cascade: &CascadeDiagnostics) -> Option<String> {
  let mut parts = Vec::new();
  push_opt_u64(&mut parts, "nodes", cascade.nodes);
  push_opt_u64(&mut parts, "candidates", cascade.rule_candidates);
  push_opt_u64(&mut parts, "pruned", cascade.rule_candidates_pruned);
  push_opt_u64(&mut parts, "matches", cascade.rule_matches);
  push_opt_u64(&mut parts, "sel_attempts", cascade.selector_attempts_total);
  push_opt_u64(
    &mut parts,
    "sel_after_bloom",
    cascade.selector_attempts_after_bloom,
  );
  push_opt_u64(
    &mut parts,
    "sel_bloom_rejects",
    cascade.selector_bloom_fast_rejects,
  );
  let mut bucket_parts = Vec::new();
  push_opt_u64(&mut bucket_parts, "id", cascade.rule_candidates_by_id);
  push_opt_u64(&mut bucket_parts, "class", cascade.rule_candidates_by_class);
  push_opt_u64(&mut bucket_parts, "tag", cascade.rule_candidates_by_tag);
  push_opt_u64(&mut bucket_parts, "attr", cascade.rule_candidates_by_attr);
  push_opt_u64(
    &mut bucket_parts,
    "universal",
    cascade.rule_candidates_universal,
  );
  if !bucket_parts.is_empty() {
    parts.push(format!("buckets {}", bucket_parts.join("/")));
  }
  push_opt_ms(&mut parts, "selector", cascade.selector_time_ms);
  push_opt_ms(&mut parts, "declaration", cascade.declaration_time_ms);
  push_opt_ms(&mut parts, "pseudo", cascade.pseudo_time_ms);

  let mut has_parts = Vec::new();
  push_opt_u64(&mut has_parts, "evals", cascade.has_evals);
  push_opt_u64(&mut has_parts, "hits", cascade.has_cache_hits);
  push_opt_u64(&mut has_parts, "prunes", cascade.has_prunes);
  push_opt_u64(&mut has_parts, "evaluated", cascade.has_evaluated);
  if !has_parts.is_empty() {
    parts.push(format!("has {}", has_parts.join(" ")));
  }
  if parts.is_empty() {
    None
  } else {
    Some(parts.join(" "))
  }
}

fn layout_summary(layout: &LayoutDiagnostics) -> Option<String> {
  let mut parts = Vec::new();
  let mut cache_parts = Vec::new();
  push_opt_usize(&mut cache_parts, "lookups", layout.layout_cache_lookups);
  push_opt_usize(&mut cache_parts, "hits", layout.layout_cache_hits);
  push_opt_usize(&mut cache_parts, "stores", layout.layout_cache_stores);
  push_opt_usize(&mut cache_parts, "evictions", layout.layout_cache_evictions);
  if !cache_parts.is_empty() {
    parts.push(format!("layout_cache {}", cache_parts.join(" ")));
  }

  let mut intrinsic_parts = Vec::new();
  push_opt_usize(&mut intrinsic_parts, "lookups", layout.intrinsic_lookups);
  push_opt_usize(&mut intrinsic_parts, "hits", layout.intrinsic_hits);
  if !intrinsic_parts.is_empty() {
    parts.push(format!("intrinsic {}", intrinsic_parts.join(" ")));
  }

  fn push_profile_kind(parts: &mut Vec<String>, label: &str, ms: Option<f64>, calls: Option<u64>) {
    match (ms, calls) {
      (Some(ms), Some(calls)) => {
        if ms > 0.0 || calls > 0 {
          parts.push(format!("{label}={ms:.2}ms/{calls}"));
        }
      }
      _ => {
        if let Some(ms) = ms {
          parts.push(format!("{label}_ms={ms:.2}ms"));
        }
        if let Some(calls) = calls {
          parts.push(format!("{label}_calls={calls}"));
        }
      }
    }
  }

  let mut profile_parts = Vec::new();
  push_profile_kind(
    &mut profile_parts,
    "block",
    layout.layout_block_ms,
    layout.layout_block_calls,
  );
  push_profile_kind(
    &mut profile_parts,
    "inline",
    layout.layout_inline_ms,
    layout.layout_inline_calls,
  );
  push_profile_kind(
    &mut profile_parts,
    "flex",
    layout.layout_flex_ms,
    layout.layout_flex_calls,
  );
  push_profile_kind(
    &mut profile_parts,
    "grid",
    layout.layout_grid_ms,
    layout.layout_grid_calls,
  );
  push_profile_kind(
    &mut profile_parts,
    "table",
    layout.layout_table_ms,
    layout.layout_table_calls,
  );
  push_profile_kind(
    &mut profile_parts,
    "absolute",
    layout.layout_absolute_ms,
    layout.layout_absolute_calls,
  );
  if !profile_parts.is_empty() {
    parts.push(format!("profile {}", profile_parts.join(" ")));
  }

  let mut parallel_parts = Vec::new();
  if let Some(enabled) = layout.layout_parallel_enabled {
    parallel_parts.push(format!("enabled={enabled}"));
  }
  if let Some(auto) = layout.layout_parallel_auto_activated {
    parallel_parts.push(format!("auto={auto}"));
  }
  push_opt_usize(
    &mut parallel_parts,
    "items",
    layout.layout_parallel_work_items,
  );
  push_opt_usize(
    &mut parallel_parts,
    "threads",
    layout.layout_parallel_worker_threads,
  );
  if !parallel_parts.is_empty() {
    parts.push(format!("parallel {}", parallel_parts.join(" ")));
  }

  if parts.is_empty() {
    None
  } else {
    Some(parts.join(" | "))
  }
}

fn paint_summary(paint: &PaintDiagnostics) -> Option<String> {
  let mut parts = Vec::new();
  push_opt_usize(&mut parts, "display_items", paint.display_items);
  push_opt_usize(&mut parts, "optimized_items", paint.optimized_items);
  push_opt_usize(&mut parts, "culled_items", paint.culled_items);
  push_opt_usize(&mut parts, "transparent_removed", paint.transparent_removed);
  push_opt_usize(&mut parts, "noop_removed", paint.noop_removed);
  push_opt_usize(&mut parts, "merged_items", paint.merged_items);
  push_opt_ms(&mut parts, "gradient", paint.gradient_ms);
  push_opt_u64(&mut parts, "gradient_pixels", paint.gradient_pixels);
  push_opt_u64(
    &mut parts,
    "image_pixmap_cache_hits",
    paint.image_pixmap_cache_hits,
  );
  push_opt_u64(
    &mut parts,
    "image_pixmap_cache_misses",
    paint.image_pixmap_cache_misses,
  );
  push_opt_ms(&mut parts, "image_pixmap", paint.image_pixmap_ms);
  push_opt_u64(&mut parts, "background_tiles", paint.background_tiles);
  push_opt_u64(&mut parts, "background_layers", paint.background_layers);
  push_opt_u64(
    &mut parts,
    "background_pattern_fast_paths",
    paint.background_pattern_fast_paths,
  );
  push_opt_ms(&mut parts, "background", paint.background_ms);
  push_opt_u64(&mut parts, "clip_mask_calls", paint.clip_mask_calls);
  push_opt_ms(&mut parts, "clip_mask", paint.clip_mask_ms);
  push_opt_u64(&mut parts, "clip_mask_pixels", paint.clip_mask_pixels);
  push_opt_u64(&mut parts, "layer_allocations", paint.layer_allocations);
  push_opt_u64(&mut parts, "layer_alloc_bytes", paint.layer_alloc_bytes);
  push_opt_usize(&mut parts, "parallel_tasks", paint.parallel_tasks);
  push_opt_usize(&mut parts, "parallel_threads", paint.parallel_threads);
  push_opt_ms(&mut parts, "parallel", paint.parallel_ms);
  push_opt_ms(&mut parts, "serial", paint.serial_ms);
  push_opt_usize(&mut parts, "filter_cache_hits", paint.filter_cache_hits);
  push_opt_usize(&mut parts, "filter_cache_misses", paint.filter_cache_misses);
  push_opt_usize(&mut parts, "blur_cache_hits", paint.blur_cache_hits);
  push_opt_usize(&mut parts, "blur_cache_misses", paint.blur_cache_misses);
  push_opt_usize(&mut parts, "blur_tiles", paint.blur_tiles);
  if parts.is_empty() {
    None
  } else {
    Some(parts.join(" "))
  }
}

fn resources_summary(resources: &ResourceDiagnostics) -> Option<String> {
  let mut parts = Vec::new();
  let doc = *resources
    .fetch_counts
    .get(&ResourceKind::Document)
    .unwrap_or(&0);
  let css = *resources
    .fetch_counts
    .get(&ResourceKind::Stylesheet)
    .unwrap_or(&0);
  let img = *resources
    .fetch_counts
    .get(&ResourceKind::Image)
    .unwrap_or(&0);
  let font = *resources
    .fetch_counts
    .get(&ResourceKind::Font)
    .unwrap_or(&0);
  let other = *resources
    .fetch_counts
    .get(&ResourceKind::Other)
    .unwrap_or(&0);
  if doc + css + img + font + other > 0 {
    parts.push(format!(
      "fetches doc={doc} css={css} img={img} font={font} other={other}"
    ));
  }

  let mut cache_parts = Vec::new();
  if let Some(hits) = resources.image_cache_hits {
    cache_parts.push(format!("hits={hits}"));
  }
  if let Some(misses) = resources.image_cache_misses {
    cache_parts.push(format!("misses={misses}"));
  }
  if !cache_parts.is_empty() {
    parts.push(format!("image_cache {}", cache_parts.join(" ")));
  }

  let mut resource_cache_parts = Vec::new();
  if let Some(hits) = resources.resource_cache_fresh_hits {
    resource_cache_parts.push(format!("fresh_hits={hits}"));
  }
  if let Some(hits) = resources.resource_cache_stale_hits {
    resource_cache_parts.push(format!("stale_hits={hits}"));
  }
  if let Some(hits) = resources.resource_cache_revalidated_hits {
    resource_cache_parts.push(format!("revalidated_hits={hits}"));
  }
  if let Some(misses) = resources.resource_cache_misses {
    resource_cache_parts.push(format!("misses={misses}"));
  }
  if let Some(bytes) = resources.resource_cache_bytes {
    resource_cache_parts.push(format!("bytes={}", format_bytes(bytes as u64)));
  }
  if !resource_cache_parts.is_empty() {
    parts.push(format!("resource_cache {}", resource_cache_parts.join(" ")));
  }

  let mut inflight_parts = Vec::new();
  if let Some(waits) = resources.fetch_inflight_waits {
    inflight_parts.push(format!("waits={waits}"));
  }
  if let Some(ms) = resources.fetch_inflight_wait_ms {
    inflight_parts.push(format!("ms={ms:.2}ms"));
  }
  if !inflight_parts.is_empty() {
    parts.push(format!("inflight {}", inflight_parts.join(" ")));
  }

  let mut disk_cache_parts = Vec::new();
  if let Some(hits) = resources.disk_cache_hits {
    disk_cache_parts.push(format!("hits={hits}"));
  }
  if let Some(misses) = resources.disk_cache_misses {
    disk_cache_parts.push(format!("misses={misses}"));
  }
  if let Some(bytes) = resources.disk_cache_bytes {
    disk_cache_parts.push(format!("bytes={}", format_bytes(bytes as u64)));
  }
  if let Some(waits) = resources.disk_cache_lock_waits {
    disk_cache_parts.push(format!("lock_waits={waits}"));
  }
  if let Some(ms) = resources.disk_cache_lock_wait_ms {
    disk_cache_parts.push(format!("lock_wait={ms:.2}ms"));
  }
  if let Some(ms) = resources.disk_cache_ms {
    disk_cache_parts.push(format!("ms={ms:.2}ms"));
  }
  if !disk_cache_parts.is_empty() {
    parts.push(format!("disk_cache {}", disk_cache_parts.join(" ")));
  }

  let mut network_parts = Vec::new();
  if let Some(fetches) = resources.network_fetches {
    network_parts.push(format!("fetches={fetches}"));
  }
  if let Some(bytes) = resources.network_fetch_bytes {
    network_parts.push(format!("bytes={}", format_bytes(bytes as u64)));
  }
  if let Some(ms) = resources.network_fetch_ms {
    network_parts.push(format!("ms={ms:.2}ms"));
  }
  if !network_parts.is_empty() {
    parts.push(format!("network {}", network_parts.join(" ")));
  }

  if parts.is_empty() {
    None
  } else {
    Some(parts.join(" | "))
  }
}

#[derive(Debug)]
struct PageDelta<'a> {
  stem: String,
  baseline: &'a LoadedProgress,
  current: &'a LoadedProgress,
  delta_ms: f64,
  percent_delta: Option<f64>,
  stage_delta: StageBuckets,
}

#[derive(Debug)]
struct Comparison<'a> {
  transitions: BTreeMap<String, usize>,
  deltas: Vec<PageDelta<'a>>,
  ok_to_bad: Vec<(String, ProgressStatus, ProgressStatus)>,
}

fn build_comparison<'a>(
  current: &'a [LoadedProgress],
  baseline: &'a [LoadedProgress],
) -> Comparison<'a> {
  let current_map: BTreeMap<String, &LoadedProgress> =
    current.iter().map(|p| (p.stem.clone(), p)).collect();
  let baseline_map: BTreeMap<String, &LoadedProgress> =
    baseline.iter().map(|p| (p.stem.clone(), p)).collect();

  let mut stems: BTreeSet<String> = BTreeSet::new();
  stems.extend(current_map.keys().cloned());
  stems.extend(baseline_map.keys().cloned());

  let mut transitions: BTreeMap<String, usize> = BTreeMap::new();
  let mut ok_to_bad: Vec<(String, ProgressStatus, ProgressStatus)> = Vec::new();
  let mut deltas: Vec<PageDelta<'a>> = Vec::new();

  for stem in stems {
    let current_entry = current_map.get(&stem).copied();
    let baseline_entry = baseline_map.get(&stem).copied();
    let from_status = baseline_entry.map(|p| p.progress.status);
    let to_status = current_entry.map(|p| p.progress.status);
    if from_status != to_status {
      let key = format!(
        "{} -> {}",
        status_label(from_status),
        status_label(to_status)
      );
      *transitions.entry(key).or_default() += 1;
    }
    if matches!(from_status, Some(ProgressStatus::Ok)) {
      if let Some(to_status) = to_status {
        if is_bad_status(to_status) {
          ok_to_bad.push((stem.clone(), ProgressStatus::Ok, to_status));
        }
      }
    }
    if let (Some(current), Some(baseline)) = (current_entry, baseline_entry) {
      if let (Some(current_total), Some(baseline_total)) =
        (current.progress.total_ms, baseline.progress.total_ms)
      {
        let percent_delta = if baseline_total.abs() <= f64::EPSILON {
          None
        } else {
          Some((current_total - baseline_total) / baseline_total * 100.0)
        };
        let stage_delta =
          subtract_stage_buckets(&current.progress.stages_ms, &baseline.progress.stages_ms);
        deltas.push(PageDelta {
          stem: stem.clone(),
          baseline,
          current,
          delta_ms: current_total - baseline_total,
          percent_delta,
          stage_delta,
        });
      }
    }
  }

  Comparison {
    transitions,
    deltas,
    ok_to_bad,
  }
}

fn print_render_stats(stats: &RenderStats, indent: &str) -> bool {
  let mut lines: Vec<(&str, String)> = Vec::new();

  if let Some(nodes) = nodes_summary(&stats.counts) {
    lines.push(("nodes", nodes));
  }
  if let Some(text) = text_summary_with_timings(stats) {
    lines.push(("text", text));
  }
  if let Some(cascade) = cascade_summary(&stats.cascade) {
    lines.push(("cascade", cascade));
  }
  if let Some(layout) = layout_summary(&stats.layout) {
    lines.push(("layout", layout));
  }
  if let Some(paint) = paint_summary(&stats.paint) {
    lines.push(("paint", paint));
  }
  if let Some(resources) = resources_summary(&stats.resources) {
    lines.push(("resources", resources));
  }

  if lines.is_empty() {
    return false;
  }

  println!("{indent}stats:");
  for (label, line) in lines {
    println!("{indent}  {label}: {line}");
  }
  true
}

fn print_note_block(label: &str, note: &str, indent: &str) {
  let trimmed = note.trim();
  if trimmed.is_empty() {
    return;
  }
  println!("{indent}{label}:");
  for line in trimmed.lines() {
    println!("{indent}  {line}");
  }
}

const REPORT_OFFENDING_STEMS_LIMIT: usize = 25;

#[derive(Debug, Clone, PartialEq, Eq)]
struct ReportError {
  missing_stages: Vec<String>,
  missing_stage_timings: Vec<String>,
  stage_sum_exceeds_total: Vec<String>,
  slow_ok: Vec<String>,
  ok_with_failures: Vec<String>,
}

impl ReportError {
  fn is_empty(&self) -> bool {
    self.missing_stages.is_empty()
      && self.missing_stage_timings.is_empty()
      && self.stage_sum_exceeds_total.is_empty()
      && self.slow_ok.is_empty()
      && self.ok_with_failures.is_empty()
  }
}

fn evaluate_report_checks(
  args: &ReportArgs,
  progresses: &[LoadedProgress],
) -> Result<(), ReportError> {
  let mut error = ReportError {
    missing_stages: Vec::new(),
    missing_stage_timings: Vec::new(),
    stage_sum_exceeds_total: Vec::new(),
    slow_ok: Vec::new(),
    ok_with_failures: Vec::new(),
  };

  if args.fail_on_missing_stages {
    for entry in progresses {
      if is_bad_status(entry.progress.status)
        && entry.progress.total_ms.is_some()
        && entry.progress.timeout_stage.is_none()
        && entry.progress.failure_stage.is_none()
      {
        error.missing_stages.push(entry.stem.clone());
      }
    }
  }

  if args.fail_on_missing_stage_timings {
    for entry in progresses {
      if entry.progress.status == ProgressStatus::Ok
        && entry.progress.total_ms.is_some()
        && entry.progress.stages_ms.sum() == 0.0
      {
        error.missing_stage_timings.push(entry.stem.clone());
      }
    }
  }

  if args.fail_on_stage_sum_exceeds_total {
    let tolerance = args.stage_sum_tolerance_percent.max(0.0) / 100.0;
    for entry in progresses {
      if entry.progress.status != ProgressStatus::Ok {
        continue;
      }
      let Some(total_ms) = entry.progress.total_ms else {
        continue;
      };
      if !total_ms.is_finite() || total_ms <= 0.0 {
        continue;
      }
      let sum_ms = entry.progress.stages_ms.sum();
      if !sum_ms.is_finite() || sum_ms <= 0.0 {
        continue;
      }
      let allowed_ms = total_ms * (1.0 + tolerance);
      if sum_ms > allowed_ms {
        error.stage_sum_exceeds_total.push(entry.stem.clone());
      }
    }
  }

  if let Some(threshold_ms) = args.fail_on_slow_ok_ms {
    for entry in progresses {
      if entry.progress.status != ProgressStatus::Ok {
        continue;
      }
      let Some(total_ms) = entry.progress.total_ms else {
        continue;
      };
      if total_ms > threshold_ms {
        error.slow_ok.push(entry.stem.clone());
      }
    }
  }

  if args.fail_on_ok_with_failures {
    for entry in progresses {
      if entry.progress.status == ProgressStatus::Ok && entry.progress.failure_stage.is_some() {
        error.ok_with_failures.push(entry.stem.clone());
      }
    }
  }

  if error.is_empty() {
    return Ok(());
  }

  error.missing_stages.sort();
  error.missing_stages.dedup();
  error.missing_stage_timings.sort();
  error.missing_stage_timings.dedup();
  error.stage_sum_exceeds_total.sort();
  error.stage_sum_exceeds_total.dedup();
  error.slow_ok.sort();
  error.slow_ok.dedup();
  error.ok_with_failures.sort();
  error.ok_with_failures.dedup();

  Err(error)
}

fn eprint_offending_stems(stems: &[String]) {
  let shown = stems.len().min(REPORT_OFFENDING_STEMS_LIMIT);
  for stem in stems.iter().take(shown) {
    eprintln!("  {stem}");
  }
  if stems.len() > shown {
    eprintln!(
      "  {} and {} more",
      PROGRESS_NOTE_ELLIPSIS,
      stems.len() - shown
    );
  }
}

fn eprint_offending_slow_ok(progresses: &[LoadedProgress], stems: &[String]) {
  let mut offenders: Vec<(String, f64, String)> = stems
    .iter()
    .filter_map(|stem| {
      let entry = progresses.iter().find(|p| &p.stem == stem)?;
      Some((
        stem.clone(),
        entry.progress.total_ms.unwrap_or(0.0),
        normalize_hotspot(&entry.progress.hotspot),
      ))
    })
    .collect();
  offenders.sort_by(|(a_stem, a_ms, _), (b_stem, b_ms, _)| {
    b_ms.total_cmp(a_ms).then_with(|| a_stem.cmp(b_stem))
  });
  let shown = offenders.len().min(REPORT_OFFENDING_STEMS_LIMIT);
  for (stem, total_ms, hotspot) in offenders.iter().take(shown) {
    eprintln!("  {stem} total={total_ms:.2}ms hotspot={hotspot}");
  }
  if offenders.len() > shown {
    eprintln!(
      "  {} and {} more",
      PROGRESS_NOTE_ELLIPSIS,
      offenders.len() - shown
    );
  }
}

fn eprint_offending_stage_sum_exceeds_total(progresses: &[LoadedProgress], stems: &[String]) {
  let mut offenders: Vec<(String, f64, f64, f64)> = stems
    .iter()
    .filter_map(|stem| {
      let entry = progresses.iter().find(|p| &p.stem == stem)?;
      let total_ms = entry.progress.total_ms.unwrap_or(0.0);
      let sum_ms = entry.progress.stages_ms.sum();
      let ratio = if total_ms > 0.0 { sum_ms / total_ms } else { 0.0 };
      Some((stem.clone(), total_ms, sum_ms, ratio))
    })
    .collect();
  offenders.sort_by(|(a_stem, a_total, a_sum, a_ratio), (b_stem, b_total, b_sum, b_ratio)| {
    b_ratio
      .total_cmp(a_ratio)
      .then_with(|| b_sum.total_cmp(a_sum))
      .then_with(|| b_total.total_cmp(a_total))
      .then_with(|| a_stem.cmp(b_stem))
  });
  let shown = offenders.len().min(REPORT_OFFENDING_STEMS_LIMIT);
  for (stem, total_ms, sum_ms, ratio) in offenders.iter().take(shown) {
    eprintln!(
      "  {stem} total={total_ms:.2}ms stage_sum={sum_ms:.2}ms ratio={ratio:.2}x"
    );
  }
  if offenders.len() > shown {
    eprintln!(
      "  {} and {} more",
      PROGRESS_NOTE_ELLIPSIS,
      offenders.len() - shown
    );
  }
}

fn report(args: ReportArgs) -> io::Result<()> {
  if args.fail_on_regression && args.compare.is_none() {
    eprintln!("--fail-on-regression requires --compare");
    std::process::exit(2);
  }
  if args.fail_on_new_ok_failures && args.compare.is_none() {
    eprintln!("--fail-on-new-ok-failures requires --compare");
    std::process::exit(2);
  }
  if args.regression_threshold_percent < 0.0 {
    eprintln!("regression threshold must be >= 0");
    std::process::exit(2);
  }
  if args.stage_sum_tolerance_percent < 0.0 {
    eprintln!("--stage-sum-tolerance-percent must be >= 0");
    std::process::exit(2);
  }
  if let Some(ms) = args.fail_on_slow_ok_ms {
    if ms < 0.0 {
      eprintln!("--fail-on-slow-ok-ms must be >= 0");
      std::process::exit(2);
    }
  }

  let progresses = read_progress_dir(&args.progress_dir)?;
  let total_pages = progresses.len();
  let status_counts = summarize_status(&progresses);

  println!("Status counts ({total_pages} pages):");
  println!("  ok: {}", status_counts.ok);
  println!("  timeout: {}", status_counts.timeout);
  println!("  panic: {}", status_counts.panic);
  println!("  error: {}", status_counts.error);
  println!();

  let ok_with_failures: Vec<&LoadedProgress> = progresses
    .iter()
    .filter(|p| p.progress.status == ProgressStatus::Ok && p.progress.failure_stage.is_some())
    .collect();
  println!(
    "Ok pages with failures (failure_stage set): {}",
    ok_with_failures.len()
  );
  if ok_with_failures.is_empty() {
    println!("  (none)");
    println!();
  } else {
    let mut stage_counts: BTreeMap<String, usize> = BTreeMap::new();
    for entry in &ok_with_failures {
      let stage = entry
        .progress
        .failure_stage
        .expect("failure_stage should be present for ok-with-failures pages");
      *stage_counts.entry(stage.as_str().to_string()).or_default() += 1;
    }
    for label in ["css", "layout", "paint"] {
      println!(
        "  {label}: {}",
        stage_counts.get(label).copied().unwrap_or(0)
      );
    }
    for (label, count) in stage_counts {
      if matches!(label.as_str(), "css" | "layout" | "paint") {
        continue;
      }
      println!("  {label}: {count}");
    }

    let mut ok_with_failures_timed: Vec<&LoadedProgress> = ok_with_failures
      .iter()
      .copied()
      .filter(|p| p.progress.total_ms.is_some())
      .collect();
    ok_with_failures_timed.sort_by(|a, b| {
      let a_total = a.progress.total_ms.unwrap_or(0.0);
      let b_total = b.progress.total_ms.unwrap_or(0.0);
      b_total
        .total_cmp(&a_total)
        .then_with(|| a.stem.cmp(&b.stem))
    });
    let timed_count = ok_with_failures_timed.len();
    let top_n = args.top.min(timed_count);
    println!("Slowest ok pages with failures (top {top_n} of {timed_count} with timings):");
    if top_n == 0 {
      println!("  (none)");
    } else {
      for (idx, entry) in ok_with_failures_timed.iter().take(top_n).enumerate() {
        let total_ms = entry.progress.total_ms.unwrap_or(0.0);
        let failure_stage = entry
          .progress
          .failure_stage
          .expect("failure_stage should be present for ok-with-failures pages")
          .as_str();
        println!(
          "  {}. {} total={total_ms:.2}ms hotspot={} failure_stage={failure_stage} url={}",
          idx + 1,
          entry.stem,
          normalize_hotspot(&entry.progress.hotspot),
          entry.progress.url
        );
      }
    }
    println!();
  }

  let mut timed: Vec<&LoadedProgress> = progresses
    .iter()
    .filter(|p| p.progress.total_ms.is_some())
    .collect();
  timed.sort_by(|a, b| {
    let a_total = a.progress.total_ms.unwrap_or(0.0);
    let b_total = b.progress.total_ms.unwrap_or(0.0);
    b_total
      .total_cmp(&a_total)
      .then_with(|| a.stem.cmp(&b.stem))
  });

  let timed_count = timed.len();
  let top_n = args.top.min(timed_count);
  println!("Slowest pages (top {top_n} of {timed_count} with timings):");
  if top_n == 0 {
    println!("  (none)");
  } else {
    for (idx, entry) in timed.iter().take(top_n).enumerate() {
      let total_ms = entry.progress.total_ms.unwrap_or(0.0);
      println!(
        "  {}. {} ({}, hotspot={}) total={total_ms:.2}ms url={}",
        idx + 1,
        entry.stem,
        entry.progress.status.as_str(),
        normalize_hotspot(&entry.progress.hotspot),
        entry.progress.url
      );
      if args.verbose_stats {
        if let Some(stats) = &entry.stats {
          if !print_render_stats(stats, "      ") {
            println!("      stats: (present but empty)");
          }
        } else {
          println!("      stats: (not captured in progress file)");
        }
      }
      let manual_notes = manual_notes_from_previous(&entry.progress).unwrap_or_default();
      print_note_block("notes (manual)", &manual_notes, "      ");
      if args.verbose {
        let auto_notes = if entry.progress.auto_notes.trim().is_empty() {
          legacy_auto_notes_from_previous(&entry.progress).unwrap_or_default()
        } else {
          entry.progress.auto_notes.clone()
        };
        print_note_block("last run", &auto_notes, "      ");
      }
    }
  }
  println!();

  let mut failure_hotspots: BTreeMap<String, usize> = BTreeMap::new();
  for entry in progresses.iter().filter(|p| {
    matches!(
      p.progress.status,
      ProgressStatus::Timeout | ProgressStatus::Panic | ProgressStatus::Error
    )
  }) {
    let hotspot = normalize_hotspot(&entry.progress.hotspot);
    *failure_hotspots.entry(hotspot).or_default() += 1;
  }
  println!("Failure hotspots (timeout/panic/error):");
  if failure_hotspots.is_empty() {
    println!("  (none)");
  } else {
    for (hotspot, count) in failure_hotspots {
      println!("  {hotspot}: {count}");
    }
  }
  println!();

  let mut failure_stages: BTreeMap<String, usize> = BTreeMap::new();
  for entry in progresses.iter().filter(|p| {
    matches!(
      p.progress.status,
      ProgressStatus::Timeout | ProgressStatus::Panic | ProgressStatus::Error
    )
  }) {
    let stage = entry
      .progress
      .timeout_stage
      .or(entry.progress.failure_stage);
    let label = stage.map(|s| s.as_str()).unwrap_or("unknown");
    *failure_stages.entry(label.to_string()).or_default() += 1;
  }
  println!("Failure stages (timeout/panic/error):");
  if failure_stages.is_empty() {
    println!("  (none)");
  } else {
    for (stage, count) in failure_stages {
      println!("  {stage}: {count}");
    }
  }
  println!();

  println!("Top-slow hotspots (top {top_n}):");
  if top_n == 0 {
    println!("  (none)");
  } else {
    let mut slow_hotspots: BTreeMap<String, usize> = BTreeMap::new();
    for entry in timed.iter().take(top_n) {
      let hotspot = normalize_hotspot(&entry.progress.hotspot);
      *slow_hotspots.entry(hotspot).or_default() += 1;
    }
    for (hotspot, count) in slow_hotspots {
      println!("  {hotspot}: {count}");
    }
  }
  println!();

  let mut stage_total = StageBuckets::default();
  let mut stage_count = 0usize;
  for entry in &progresses {
    if entry.progress.total_ms.is_none() {
      continue;
    }
    if entry.progress.status != ProgressStatus::Ok {
      continue;
    }
    stage_count += 1;
    add_stage_buckets(&mut stage_total, &entry.progress.stages_ms);
  }
  if stage_count > 0 {
    let stage_mean = divide_stage_buckets(&stage_total, stage_count as f64);
    println!("Stage timings (ok pages with timings: {stage_count}):");
    println!(
      "  totals_ms: fetch={:.2} css={:.2} cascade={:.2} layout={:.2} paint={:.2}",
      stage_total.fetch,
      stage_total.css,
      stage_total.cascade,
      stage_total.layout,
      stage_total.paint
    );
    println!(
      "  means_ms:  fetch={:.2} css={:.2} cascade={:.2} layout={:.2} paint={:.2}",
      stage_mean.fetch, stage_mean.css, stage_mean.cascade, stage_mean.layout, stage_mean.paint
    );
    println!();
  }

  if args.verbose_stats {
    macro_rules! stage_ranking {
      ($label:literal, $field:ident) => {{
        let mut stage_sorted: Vec<(&LoadedProgress, f64)> = progresses
          .iter()
          .filter_map(|entry| {
            if entry.progress.status != ProgressStatus::Ok {
              return None;
            }
            entry.progress.total_ms?;
            let ms = entry.progress.stages_ms.$field;
            if ms <= 0.0 {
              return None;
            }
            Some((entry, ms))
          })
          .collect();
        stage_sorted.sort_by(|(a_entry, a_ms), (b_entry, b_ms)| {
          b_ms
            .total_cmp(a_ms)
            .then_with(|| a_entry.stem.cmp(&b_entry.stem))
        });
        let top = args.top.min(stage_sorted.len());
        if top > 0 {
          println!(
            "Top {label} stage (top {top} of {} ok pages):",
            stage_sorted.len(),
            label = $label
          );
          for (idx, (entry, ms)) in stage_sorted.iter().take(top).enumerate() {
            let total_ms = entry.progress.total_ms.unwrap_or(0.0);
            let share = if total_ms > 0.0 {
              *ms / total_ms * 100.0
            } else {
              0.0
            };
            println!(
              "  {}. {} stage={ms:.2}ms ({share:.1}%) total={total_ms:.2}ms url={}",
              idx + 1,
              entry.stem,
              entry.progress.url
            );
          }
          println!();
        }
      }};
    }

    stage_ranking!("fetch", fetch);
    stage_ranking!("css", css);
    stage_ranking!("cascade", cascade);
    stage_ranking!("layout", layout);
    stage_ranking!("paint", paint);

    macro_rules! text_timing_ranking {
      ($label:literal, $field:ident) => {{
        let mut timing_sorted: Vec<(&LoadedProgress, f64)> = progresses
          .iter()
          .filter_map(|entry| {
            if entry.progress.status != ProgressStatus::Ok {
              return None;
            }
            entry.progress.total_ms?;
            let stats = entry.stats.as_ref()?;
            let ms = stats.timings.$field?;
            if ms <= 0.0 {
              return None;
            }
            Some((entry, ms))
          })
          .collect();
        timing_sorted.sort_by(|(a_entry, a_ms), (b_entry, b_ms)| {
          b_ms
            .total_cmp(a_ms)
            .then_with(|| a_entry.stem.cmp(&b_entry.stem))
        });
        let top = args.top.min(timing_sorted.len());
        if top > 0 {
          println!(
            "Top {label} (top {top} of {} ok pages with stats):",
            timing_sorted.len(),
            label = $label
          );
          for (idx, (entry, ms)) in timing_sorted.iter().take(top).enumerate() {
            let total_ms = entry.progress.total_ms.unwrap_or(0.0);
            let share = if total_ms > 0.0 {
              *ms / total_ms * 100.0
            } else {
              0.0
            };
            println!(
              "  {}. {} {label}={ms:.2}ms ({share:.1}%) total={total_ms:.2}ms url={}",
              idx + 1,
              entry.stem,
              entry.progress.url,
              label = $label
            );
          }
          println!();
        }
      }};
    }

    text_timing_ranking!("text_fallback_ms", text_fallback_ms);
    text_timing_ranking!("text_shape_ms", text_shape_ms);

    macro_rules! stat_ranking_ms {
      ($metric:literal, $getter:expr) => {{
        let mut sorted: Vec<(&LoadedProgress, f64)> = progresses
          .iter()
          .filter_map(|entry| {
            if entry.progress.status != ProgressStatus::Ok {
              return None;
            }
            entry.progress.total_ms?;
            let stats = entry.stats.as_ref()?;
            let ms = ($getter)(stats)?;
            if ms <= 0.0 {
              return None;
            }
            Some((entry, ms))
          })
          .collect();
        sorted.sort_by(|(a_entry, a_ms), (b_entry, b_ms)| {
          b_ms
            .total_cmp(a_ms)
            .then_with(|| a_entry.stem.cmp(&b_entry.stem))
        });
        let top = args.top.min(sorted.len());
        if top > 0 {
          println!(
            "Top {metric} (top {top} of {} with stats):",
            sorted.len(),
            metric = $metric
          );
          for (idx, (entry, ms)) in sorted.iter().take(top).enumerate() {
            let total_ms = entry.progress.total_ms.unwrap_or(0.0);
            println!(
              "  {}. {} {metric}={ms:.2}ms total={total_ms:.2}ms hotspot={} url={}",
              idx + 1,
              entry.stem,
              normalize_hotspot(&entry.progress.hotspot),
              entry.progress.url,
              metric = $metric
            );
          }
          println!();
        }
      }};
    }

    macro_rules! stat_ranking_count {
      ($metric:literal, $getter:expr) => {{
        let mut sorted: Vec<(&LoadedProgress, u64)> = progresses
          .iter()
          .filter_map(|entry| {
            if entry.progress.status != ProgressStatus::Ok {
              return None;
            }
            entry.progress.total_ms?;
            let stats = entry.stats.as_ref()?;
            let value = ($getter)(stats)?;
            if value == 0 {
              return None;
            }
            Some((entry, value))
          })
          .collect();
        sorted.sort_by(|(a_entry, a_value), (b_entry, b_value)| {
          b_value
            .cmp(a_value)
            .then_with(|| a_entry.stem.cmp(&b_entry.stem))
        });
        let top = args.top.min(sorted.len());
        if top > 0 {
          println!(
            "Top {metric} (top {top} of {} with stats):",
            sorted.len(),
            metric = $metric
          );
          for (idx, (entry, value)) in sorted.iter().take(top).enumerate() {
            let total_ms = entry.progress.total_ms.unwrap_or(0.0);
            println!(
              "  {}. {} {metric}={value} total={total_ms:.2}ms hotspot={} url={}",
              idx + 1,
              entry.stem,
              normalize_hotspot(&entry.progress.hotspot),
              entry.progress.url,
              metric = $metric
            );
          }
          println!();
        }
      }};
    }

    stat_ranking_ms!("timings.css_parse_ms", |stats: &RenderStats| stats
      .timings
      .css_parse_ms);
    stat_ranking_ms!("timings.cascade_ms", |stats: &RenderStats| stats
      .timings
      .cascade_ms);
    stat_ranking_ms!("timings.box_tree_ms", |stats: &RenderStats| stats
      .timings
      .box_tree_ms);
    stat_ranking_ms!("timings.layout_ms", |stats: &RenderStats| stats
      .timings
      .layout_ms);
    stat_ranking_ms!("timings.paint_build_ms", |stats: &RenderStats| stats
      .timings
      .paint_build_ms);
    stat_ranking_ms!("timings.paint_rasterize_ms", |stats: &RenderStats| stats
      .timings
      .paint_rasterize_ms);
    stat_ranking_ms!("layout.taffy_grid_compute_ms", |stats: &RenderStats| stats
      .layout
      .taffy_grid_compute_ms);
    stat_ranking_ms!("layout.taffy_flex_compute_ms", |stats: &RenderStats| stats
      .layout
      .taffy_flex_compute_ms);
    stat_ranking_ms!("paint.gradient_ms", |stats: &RenderStats| stats
      .paint
      .gradient_ms);

    stat_ranking_count!("layout.taffy_grid_measure_calls", |stats: &RenderStats| {
      stats.layout.taffy_grid_measure_calls
    });
    stat_ranking_count!("layout.taffy_flex_measure_calls", |stats: &RenderStats| {
      stats.layout.taffy_flex_measure_calls
    });
    stat_ranking_count!("counts.dom_nodes", |stats: &RenderStats| stats
      .counts
      .dom_nodes
      .map(|value| value as u64));
    stat_ranking_count!("counts.box_nodes", |stats: &RenderStats| stats
      .counts
      .box_nodes
      .map(|value| value as u64));
    stat_ranking_count!("counts.fragments", |stats: &RenderStats| stats
      .counts
      .fragments
      .map(|value| value as u64));
    stat_ranking_count!("counts.shaped_runs", |stats: &RenderStats| stats
      .counts
      .shaped_runs
      .map(|value| value as u64));
    stat_ranking_count!("counts.glyphs", |stats: &RenderStats| stats
      .counts
      .glyphs
      .map(|value| value as u64));

    let mut pages_with_stats = 0usize;
    let mut totals = ResourceDiagnostics::default();
    let mut saw = ResourceDiagnostics {
      fetch_counts: BTreeMap::new(),
      ..Default::default()
    };

    for entry in &progresses {
      let Some(stats) = entry.stats.as_ref() else {
        continue;
      };
      pages_with_stats += 1;

      for (kind, count) in &stats.resources.fetch_counts {
        *totals.fetch_counts.entry(*kind).or_default() += *count;
      }

      let res = &stats.resources;
      macro_rules! add_u64 {
        ($field:ident) => {
          if let Some(v) = res.$field {
            saw.$field = Some(0);
            let current = totals.$field.unwrap_or(0) as u64;
            totals.$field = Some(saturating_usize_from_u64(current.saturating_add(v as u64)));
          }
        };
      }
      macro_rules! add_f64 {
        ($field:ident) => {
          if let Some(v) = res.$field {
            saw.$field = Some(0.0);
            let current = totals.$field.unwrap_or(0.0);
            totals.$field = Some(current + v);
          }
        };
      }

      add_u64!(resource_cache_fresh_hits);
      add_u64!(resource_cache_stale_hits);
      add_u64!(resource_cache_revalidated_hits);
      add_u64!(resource_cache_misses);
      add_u64!(resource_cache_bytes);
      add_u64!(disk_cache_hits);
      add_u64!(disk_cache_misses);
      add_u64!(disk_cache_bytes);
      add_f64!(disk_cache_ms);
      add_u64!(disk_cache_lock_waits);
      add_f64!(disk_cache_lock_wait_ms);
      add_u64!(fetch_inflight_waits);
      add_f64!(fetch_inflight_wait_ms);
      add_u64!(network_fetches);
      add_u64!(network_fetch_bytes);
      add_f64!(network_fetch_ms);

      add_u64!(image_cache_hits);
      add_u64!(image_cache_misses);
    }

    // Clear fields that were never present so older progress files don't show misleading zeros.
    macro_rules! maybe_clear_u64 {
      ($field:ident) => {
        if saw.$field.is_none() {
          totals.$field = None;
        }
      };
    }
    macro_rules! maybe_clear_f64 {
      ($field:ident) => {
        if saw.$field.is_none() {
          totals.$field = None;
        }
      };
    }

    maybe_clear_u64!(resource_cache_fresh_hits);
    maybe_clear_u64!(resource_cache_stale_hits);
    maybe_clear_u64!(resource_cache_revalidated_hits);
    maybe_clear_u64!(resource_cache_misses);
    maybe_clear_u64!(resource_cache_bytes);
    maybe_clear_u64!(disk_cache_hits);
    maybe_clear_u64!(disk_cache_misses);
    maybe_clear_u64!(disk_cache_bytes);
    maybe_clear_f64!(disk_cache_ms);
    maybe_clear_u64!(disk_cache_lock_waits);
    maybe_clear_f64!(disk_cache_lock_wait_ms);
    maybe_clear_u64!(fetch_inflight_waits);
    maybe_clear_f64!(fetch_inflight_wait_ms);
    maybe_clear_u64!(network_fetches);
    maybe_clear_u64!(network_fetch_bytes);
    maybe_clear_f64!(network_fetch_ms);
    maybe_clear_u64!(image_cache_hits);
    maybe_clear_u64!(image_cache_misses);

    if pages_with_stats > 0 {
      if let Some(summary) = resources_summary(&totals) {
        println!("Resource totals (pages with stats: {pages_with_stats}):");
        println!("  {summary}");
        let mut derived_parts = Vec::new();
        let resource_cache_present = totals.resource_cache_fresh_hits.is_some()
          || totals.resource_cache_stale_hits.is_some()
          || totals.resource_cache_revalidated_hits.is_some()
          || totals.resource_cache_misses.is_some();
        if resource_cache_present {
          let hits = totals.resource_cache_fresh_hits.unwrap_or(0)
            + totals.resource_cache_stale_hits.unwrap_or(0)
            + totals.resource_cache_revalidated_hits.unwrap_or(0);
          let misses = totals.resource_cache_misses.unwrap_or(0);
          let total = hits + misses;
          if total > 0 {
            derived_parts.push(format!(
              "resource_cache hit_rate={:.1}%",
              hits as f64 / total as f64 * 100.0
            ));
          }
        }

        let disk_cache_present =
          totals.disk_cache_hits.is_some() || totals.disk_cache_misses.is_some();
        if disk_cache_present {
          let hits = totals.disk_cache_hits.unwrap_or(0);
          let misses = totals.disk_cache_misses.unwrap_or(0);
          let total = hits + misses;
          if total > 0 {
            derived_parts.push(format!(
              "disk_cache hit_rate={:.1}%",
              hits as f64 / total as f64 * 100.0
            ));
          }
        }

        let image_cache_present =
          totals.image_cache_hits.is_some() || totals.image_cache_misses.is_some();
        if image_cache_present {
          let hits = totals.image_cache_hits.unwrap_or(0);
          let misses = totals.image_cache_misses.unwrap_or(0);
          let total = hits + misses;
          if total > 0 {
            derived_parts.push(format!(
              "image_cache hit_rate={:.1}%",
              hits as f64 / total as f64 * 100.0
            ));
          }
        }

        if let (Some(lock_ms), Some(total_ms)) =
          (totals.disk_cache_lock_wait_ms, totals.disk_cache_ms)
        {
          if total_ms > 0.0 {
            derived_parts.push(format!(
              "disk_lock_wait_share={:.1}%",
              lock_ms / total_ms * 100.0
            ));
          }
        }

        if let (Some(wait_ms), Some(waits)) =
          (totals.fetch_inflight_wait_ms, totals.fetch_inflight_waits)
        {
          if waits > 0 {
            derived_parts.push(format!("inflight_avg={:.2}ms", wait_ms / waits as f64));
          }
        }

        if let (Some(bytes), Some(fetches)) = (totals.network_fetch_bytes, totals.network_fetches) {
          if fetches > 0 {
            derived_parts.push(format!(
              "network_avg_bytes={}",
              format_bytes((bytes as u64) / fetches as u64)
            ));
          }
        }

        if !derived_parts.is_empty() {
          println!("  derived: {}", derived_parts.join(" "));
        }
        println!();
      }

      let mut network_sorted: Vec<(&LoadedProgress, f64)> = progresses
        .iter()
        .filter_map(|entry| {
          let stats = entry.stats.as_ref()?;
          let ms = stats.resources.network_fetch_ms?;
          if ms <= 0.0 {
            return None;
          }
          Some((entry, ms))
        })
        .collect();
      network_sorted.sort_by(|(a_entry, a_ms), (b_entry, b_ms)| {
        b_ms
          .total_cmp(a_ms)
          .then_with(|| a_entry.stem.cmp(&b_entry.stem))
      });
      let net_top = args.top.min(network_sorted.len());
      if net_top > 0 {
        println!(
          "Top network fetch time (top {net_top} of {} with stats):",
          network_sorted.len()
        );
        for (idx, (entry, ms)) in network_sorted.iter().take(net_top).enumerate() {
          let stats = entry.stats.as_ref().expect("stats should be present");
          let bytes = stats.resources.network_fetch_bytes.unwrap_or(0) as u64;
          let fetches = stats.resources.network_fetches.unwrap_or(0);
          let total_ms = entry.progress.total_ms.unwrap_or(0.0);
          let share = if total_ms > 0.0 {
            *ms / total_ms * 100.0
          } else {
            0.0
          };
          println!(
            "  {}. {} fetches={fetches} bytes={} ms={ms:.2}ms ({share:.1}%) total={total_ms:.2}ms url={}",
            idx + 1,
            entry.stem,
            format_bytes(bytes),
            entry.progress.url
          );
        }
        println!();
      }

      let mut inflight_sorted: Vec<(&LoadedProgress, f64)> = progresses
        .iter()
        .filter_map(|entry| {
          let stats = entry.stats.as_ref()?;
          let ms = stats.resources.fetch_inflight_wait_ms?;
          if ms <= 0.0 {
            return None;
          }
          Some((entry, ms))
        })
        .collect();
      inflight_sorted.sort_by(|(a_entry, a_ms), (b_entry, b_ms)| {
        b_ms
          .total_cmp(a_ms)
          .then_with(|| a_entry.stem.cmp(&b_entry.stem))
      });
      let inflight_top = args.top.min(inflight_sorted.len());
      if inflight_top > 0 {
        println!(
          "Top inflight wait time (top {inflight_top} of {} with stats):",
          inflight_sorted.len()
        );
        for (idx, (entry, ms)) in inflight_sorted.iter().take(inflight_top).enumerate() {
          let stats = entry.stats.as_ref().expect("stats should be present");
          let waits = stats.resources.fetch_inflight_waits.unwrap_or(0);
          let total_ms = entry.progress.total_ms.unwrap_or(0.0);
          let share = if total_ms > 0.0 {
            *ms / total_ms * 100.0
          } else {
            0.0
          };
          println!(
            "  {}. {} waits={waits} ms={ms:.2}ms ({share:.1}%) total={total_ms:.2}ms url={}",
            idx + 1,
            entry.stem,
            entry.progress.url
          );
        }
        println!();
      }

      let mut disk_sorted: Vec<(&LoadedProgress, f64)> = progresses
        .iter()
        .filter_map(|entry| {
          let stats = entry.stats.as_ref()?;
          let ms = stats.resources.disk_cache_ms?;
          if ms <= 0.0 {
            return None;
          }
          Some((entry, ms))
        })
        .collect();
      disk_sorted.sort_by(|(a_entry, a_ms), (b_entry, b_ms)| {
        b_ms
          .total_cmp(a_ms)
          .then_with(|| a_entry.stem.cmp(&b_entry.stem))
      });
      let disk_top = args.top.min(disk_sorted.len());
      if disk_top > 0 {
        println!(
          "Top disk cache time (top {disk_top} of {} with stats):",
          disk_sorted.len()
        );
        for (idx, (entry, ms)) in disk_sorted.iter().take(disk_top).enumerate() {
          let stats = entry.stats.as_ref().expect("stats should be present");
          let hits = stats.resources.disk_cache_hits.unwrap_or(0);
          let misses = stats.resources.disk_cache_misses.unwrap_or(0);
          let bytes = stats.resources.disk_cache_bytes.unwrap_or(0) as u64;
          let lock_wait_ms = stats.resources.disk_cache_lock_wait_ms.unwrap_or(0.0);
          let total_ms = entry.progress.total_ms.unwrap_or(0.0);
          let share = if total_ms > 0.0 {
            *ms / total_ms * 100.0
          } else {
            0.0
          };
          println!(
            "  {}. {} hits={hits} misses={misses} bytes={} lock_wait={lock_wait_ms:.2}ms ms={ms:.2}ms ({share:.1}%) total={total_ms:.2}ms url={}",
            idx + 1,
            entry.stem,
            format_bytes(bytes),
            entry.progress.url
          );
        }
        println!();
      }

      let mut disk_lock_sorted: Vec<(&LoadedProgress, f64)> = progresses
        .iter()
        .filter_map(|entry| {
          let stats = entry.stats.as_ref()?;
          let ms = stats.resources.disk_cache_lock_wait_ms?;
          if ms <= 0.0 {
            return None;
          }
          Some((entry, ms))
        })
        .collect();
      disk_lock_sorted.sort_by(|(a_entry, a_ms), (b_entry, b_ms)| {
        b_ms
          .total_cmp(a_ms)
          .then_with(|| a_entry.stem.cmp(&b_entry.stem))
      });
      let disk_lock_top = args.top.min(disk_lock_sorted.len());
      if disk_lock_top > 0 {
        println!(
          "Top disk cache lock wait time (top {disk_lock_top} of {} with stats):",
          disk_lock_sorted.len()
        );
        for (idx, (entry, ms)) in disk_lock_sorted.iter().take(disk_lock_top).enumerate() {
          let stats = entry.stats.as_ref().expect("stats should be present");
          let waits = stats.resources.disk_cache_lock_waits.unwrap_or(0);
          let disk_total_ms = stats.resources.disk_cache_ms.unwrap_or(0.0);
          let render_total_ms = entry.progress.total_ms.unwrap_or(0.0);
          let share = if render_total_ms > 0.0 {
            *ms / render_total_ms * 100.0
          } else {
            0.0
          };
          println!(
            "  {}. {} waits={waits} lock_wait={ms:.2}ms ({share:.1}%) disk_total={disk_total_ms:.2}ms render_total={render_total_ms:.2}ms url={}",
            idx + 1,
            entry.stem,
            entry.progress.url
          );
        }
        println!();
      }
    }
  }

  if let Some(compare_dir) = args.compare.as_ref() {
    let baseline = read_progress_dir(compare_dir)?;
    let comparison = build_comparison(&progresses, &baseline);

    println!("Status transitions vs {}:", compare_dir.display());
    if comparison.transitions.is_empty() {
      println!("  (none)");
    } else {
      for (transition, count) in comparison.transitions {
        println!("  {transition}: {count}");
      }
    }
    println!();

    let mut regressions: Vec<&PageDelta<'_>> = comparison
      .deltas
      .iter()
      .filter(|d| d.delta_ms > 0.0)
      .collect();
    regressions.sort_by(|a, b| {
      b.delta_ms
        .total_cmp(&a.delta_ms)
        .then_with(|| a.stem.cmp(&b.stem))
    });

    let mut improvements: Vec<&PageDelta<'_>> = comparison
      .deltas
      .iter()
      .filter(|d| d.delta_ms < 0.0)
      .collect();
    improvements.sort_by(|a, b| {
      a.delta_ms
        .total_cmp(&b.delta_ms)
        .then_with(|| a.stem.cmp(&b.stem))
    });

    let regress_top = args.top.min(regressions.len());
    println!(
      "Regressions vs baseline (top {regress_top} of {} with timings):",
      regressions.len()
    );
    if regress_top == 0 {
      println!("  (none)");
    } else {
      for (idx, delta) in regressions.iter().take(regress_top).enumerate() {
        let percent = format_percent(delta.percent_delta);
        println!(
          "  {}. {} ({} -> {}) Δtotal={:+.2}ms ({percent}) {} url={}",
          idx + 1,
          delta.stem,
          status_label(Some(delta.baseline.progress.status)),
          status_label(Some(delta.current.progress.status)),
          delta.delta_ms,
          format_stage_delta(&delta.stage_delta),
          delta.current.progress.url
        );
      }
    }
    println!();

    let improvements_top = args.top.min(improvements.len());
    println!(
      "Improvements vs baseline (top {improvements_top} of {} with timings):",
      improvements.len()
    );
    if improvements_top == 0 {
      println!("  (none)");
    } else {
      for (idx, delta) in improvements.iter().take(improvements_top).enumerate() {
        let percent = format_percent(delta.percent_delta);
        println!(
          "  {}. {} ({} -> {}) Δtotal={:+.2}ms ({percent}) {} url={}",
          idx + 1,
          delta.stem,
          status_label(Some(delta.baseline.progress.status)),
          status_label(Some(delta.current.progress.status)),
          delta.delta_ms,
          format_stage_delta(&delta.stage_delta),
          delta.current.progress.url
        );
      }
    }
    println!();

    let mut new_ok_failures: Vec<String> = Vec::new();
    if args.fail_on_new_ok_failures {
      let current_map: HashMap<&str, &LoadedProgress> = progresses
        .iter()
        .map(|entry| (entry.stem.as_str(), entry))
        .collect();
      let baseline_map: HashMap<&str, &LoadedProgress> = baseline
        .iter()
        .map(|entry| (entry.stem.as_str(), entry))
        .collect();

      for (stem, baseline_entry) in baseline_map {
        let Some(current_entry) = current_map.get(stem) else {
          continue;
        };
        if baseline_entry.progress.status != ProgressStatus::Ok {
          continue;
        }
        if baseline_entry.progress.failure_stage.is_some() {
          continue;
        }
        if current_entry.progress.status != ProgressStatus::Ok {
          continue;
        }
        if current_entry.progress.failure_stage.is_some() {
          new_ok_failures.push(stem.to_string());
        }
      }
      new_ok_failures.sort();
      new_ok_failures.dedup();
    }

    let mut regression_reasons = Vec::new();
    if args.fail_on_regression {
      let mut failed_stems: HashSet<String> = HashSet::new();
      for (stem, from, to) in &comparison.ok_to_bad {
        regression_reasons.push(format!(
          "{stem}: {} -> {}",
          status_label(Some(*from)),
          status_label(Some(*to))
        ));
        failed_stems.insert(stem.clone());
      }
      for delta in &comparison.deltas {
        if failed_stems.contains(&delta.stem) {
          continue;
        }
        if delta.baseline.progress.status != ProgressStatus::Ok {
          continue;
        }
        if delta.current.progress.status != ProgressStatus::Ok {
          continue;
        }
        if let Some(percent) = delta.percent_delta {
          if percent > args.regression_threshold_percent {
            regression_reasons.push(format!(
              "{}: Δtotal={:+.2}ms ({:+.2}%)",
              delta.stem, delta.delta_ms, percent
            ));
            failed_stems.insert(delta.stem.clone());
          }
        }
      }
    }

    if !new_ok_failures.is_empty() || !regression_reasons.is_empty() {
      let mut printed_any = false;
      if !new_ok_failures.is_empty() {
        eprintln!(
          "Failing due to {} ok page(s) gaining failure_stage vs {}:",
          new_ok_failures.len(),
          compare_dir.display()
        );
        eprint_offending_stems(&new_ok_failures);
        printed_any = true;
      }
      if !regression_reasons.is_empty() {
        if printed_any {
          eprintln!();
        }
        eprintln!("Failing due to regressions vs {}:", compare_dir.display());
        for reason in regression_reasons {
          eprintln!("  {reason}");
        }
      }
      std::process::exit(1);
    }
  }

  if args.include_trace {
    println!(
      "Trace artifacts (progress: {}, traces: {}):",
      args.trace_progress_dir.display(),
      args.trace_dir.display()
    );
    let trace_progresses = read_progress_dir_allow_empty(&args.trace_progress_dir)?;
    if trace_progresses.is_empty() {
      println!("  (no trace progress files found)");
    } else {
      for entry in trace_progresses {
        let total_ms = entry
          .progress
          .total_ms
          .map(|ms| format!("{ms:.2}ms"))
          .unwrap_or_else(|| "-".to_string());
        let trace_path = args.trace_dir.join(format!("{}.json", entry.stem));
        let trace_status = match fs::metadata(&trace_path) {
          Ok(meta) => {
            let size = meta.len();
            if size < MIN_TRACE_BYTES {
              format!("{} ({} bytes, likely partial)", trace_path.display(), size)
            } else {
              format!("{} ({} bytes)", trace_path.display(), size)
            }
          }
          Err(err) if err.kind() == io::ErrorKind::NotFound => {
            format!("{} (missing)", trace_path.display())
          }
          Err(err) => format!("{} (unreadable: {})", trace_path.display(), err),
        };
        println!(
          "  {}: status={} total={} -> {}",
          entry.stem,
          entry.progress.status.as_str(),
          total_ms,
          trace_status
        );
      }
    }
    println!();
  }

  if let Err(err) = evaluate_report_checks(&args, &progresses) {
    let mut printed_any = false;
    if !err.missing_stages.is_empty() {
      eprintln!(
        "Failing due to {} timeout/panic/error page(s) missing stage attribution:",
        err.missing_stages.len()
      );
      eprint_offending_stems(&err.missing_stages);
      printed_any = true;
    }
    if !err.missing_stage_timings.is_empty() {
      if printed_any {
        eprintln!();
      }
      eprintln!(
        "Failing due to {} ok page(s) with total_ms but zero per-stage timing buckets:",
        err.missing_stage_timings.len()
      );
      eprint_offending_stems(&err.missing_stage_timings);
      printed_any = true;
    }
    if !err.stage_sum_exceeds_total.is_empty() {
      if printed_any {
        eprintln!();
      }
      let tolerance = args.stage_sum_tolerance_percent;
      eprintln!(
        "Failing due to {} ok page(s) whose stage buckets exceed total_ms by more than {tolerance:.1}%:",
        err.stage_sum_exceeds_total.len()
      );
      eprint_offending_stage_sum_exceeds_total(&progresses, &err.stage_sum_exceeds_total);
      printed_any = true;
    }
    if !err.slow_ok.is_empty() {
      if printed_any {
        eprintln!();
      }
      let threshold_ms = args
        .fail_on_slow_ok_ms
        .expect("slow_ok gate should only run when threshold is set");
      eprintln!(
        "Failing due to {} ok page(s) exceeding {threshold_ms}ms:",
        err.slow_ok.len()
      );
      eprint_offending_slow_ok(&progresses, &err.slow_ok);
      printed_any = true;
    }
    if !err.ok_with_failures.is_empty() {
      if printed_any {
        eprintln!();
      }
      eprintln!(
        "Failing due to {} ok page(s) with failure_stage set:",
        err.ok_with_failures.len()
      );
      eprint_offending_stems(&err.ok_with_failures);
    }
    std::process::exit(1);
  }

  if args.fail_on_bad && (status_counts.timeout > 0 || status_counts.panic > 0) {
    eprintln!(
      "Failing due to {} timeout(s) and {} panic(s).",
      status_counts.timeout, status_counts.panic
    );
    std::process::exit(1);
  }

  Ok(())
}

#[derive(Debug, Clone)]
struct WorkItem {
  stem: String,
  cache_stem: String,
  url: String,
  cache_path: PathBuf,
  progress_path: PathBuf,
  log_path: PathBuf,
  stderr_path: PathBuf,
  stage_path: PathBuf,
  trace_out: Option<PathBuf>,
}

#[derive(Debug, Clone)]
struct DumpSettings {
  failures: Option<DumpLevel>,
  slow: Option<DumpLevel>,
  slow_ms: Option<f64>,
  dir: PathBuf,
  timeout_secs: u64,
  soft_timeout_ms: Option<u64>,
}

fn dump_level_for_progress(args: &WorkerArgs, progress: &PageProgress) -> Option<DumpLevel> {
  let mut level: Option<DumpLevel> = None;
  if let Some(fail_level) = args.dump_failures {
    if progress.status != ProgressStatus::Ok {
      level = Some(DumpLevel::combine(level, fail_level));
    }
  }
  if let Some(threshold_ms) = args.dump_slow_ms {
    if let Some(total_ms) = progress.total_ms {
      if total_ms > threshold_ms {
        if let Some(slow_level) = args.dump_slow {
          level = Some(DumpLevel::combine(level, slow_level));
        }
      }
    }
  }
  level
}

fn collect_cached_html_paths() -> io::Result<BTreeMap<String, PathBuf>> {
  collect_cached_html_paths_in(Path::new(CACHE_HTML_DIR))
}

fn work_item_from_cache(
  cache_stem: &str,
  cache_path: PathBuf,
  entry: Option<&PagesetEntry>,
  args: &RunArgs,
) -> WorkItem {
  // Prefer the final URL recorded by `fetch_pages` so progress metadata matches what we
  // actually render (redirects to `www.`, MDN reference pages, etc).
  let url = cached_url_from_cache_meta(&cache_path)
    .or_else(|| entry.map(|e| e.url.clone()))
    .unwrap_or_else(|| format!("file://{}", cache_path.display()));
  let stem = entry
    .map(|e| e.stem.clone())
    .or_else(|| pageset_stem(cache_stem))
    .unwrap_or_else(|| cache_stem.to_string());
  WorkItem {
    stem,
    cache_stem: cache_stem.to_string(),
    url,
    cache_path,
    progress_path: args.progress_dir.join(format!("{cache_stem}.json")),
    log_path: args.log_dir.join(format!("{cache_stem}.log")),
    stderr_path: args.log_dir.join(format!("{cache_stem}.stderr.log")),
    stage_path: args.log_dir.join(format!("{cache_stem}.stage")),
    trace_out: None,
  }
}

#[derive(Debug)]
struct RunningChild {
  item: WorkItem,
  started: Instant,
  child: Child,
}

#[cfg(test)]
thread_local! {
  /// Test-only environment overrides for worker processes spawned via `spawn_worker`/`run_queue`.
  ///
  /// The integration tests include this file directly as a module, and those tests used to set
  /// `FASTR_TEST_*` variables via `std::env::set_var`. Because the Rust test harness runs tests in
  /// parallel, those process-global env vars leaked across tests and caused flaky worker/dump
  /// behaviour. Keeping overrides thread-local and applying them explicitly to spawned workers
  /// avoids cross-test contamination while preserving the ability to influence the worker process.
  static WORKER_ENV_OVERRIDES: std::cell::RefCell<std::collections::BTreeMap<&'static str, String>> =
    std::cell::RefCell::new(std::collections::BTreeMap::new());
}

#[cfg(test)]
fn set_worker_env_override(key: &'static str, value: &str) -> Option<String> {
  WORKER_ENV_OVERRIDES.with(|overrides| overrides.borrow_mut().insert(key, value.to_string()))
}

#[cfg(test)]
fn restore_worker_env_override(key: &'static str, previous: Option<String>) {
  WORKER_ENV_OVERRIDES.with(|overrides| {
    let mut overrides = overrides.borrow_mut();
    match previous {
      Some(value) => {
        overrides.insert(key, value);
      }
      None => {
        overrides.remove(key);
      }
    }
  });
}

#[cfg(test)]
fn apply_worker_env_overrides(cmd: &mut Command) {
  WORKER_ENV_OVERRIDES.with(|overrides| {
    for (key, value) in overrides.borrow().iter() {
      cmd.env(key, value);
    }
  });
}

fn spawn_worker(
  exe: &Path,
  args: &RunArgs,
  item: &WorkItem,
  diagnostics: DiagnosticsArg,
  cascade_profile: bool,
  soft_timeout_ms: Option<u64>,
  worker_timeout: Duration,
  dump: Option<&DumpSettings>,
) -> io::Result<Child> {
  let worker_timeout_secs = worker_timeout.as_secs().max(1);
  let mut cmd = Command::new(exe);
  cmd
    .arg("worker")
    .arg("--cache-path")
    .arg(&item.cache_path)
    .arg("--stem")
    .arg(&item.cache_stem)
    .arg("--progress-path")
    .arg(&item.progress_path)
    .arg("--log-path")
    .arg(&item.log_path)
    .arg("--stage-path")
    .arg(&item.stage_path)
    .arg("--viewport")
    .arg(format!("{}x{}", args.viewport.0, args.viewport.1))
    .arg("--dpr")
    .arg(args.dpr.to_string())
    .arg("--user-agent")
    .arg(&args.user_agent)
    .arg("--accept-language")
    .arg(&args.accept_language)
    .arg("--disk-cache-max-bytes")
    .arg(args.disk_cache.max_bytes.to_string())
    .arg("--disk-cache-max-age-secs")
    .arg(args.disk_cache.max_age_secs.to_string())
    .arg("--disk-cache-lock-stale-secs")
    .arg(args.disk_cache.lock_stale_secs.to_string())
    .arg("--diagnostics")
    .arg(format!("{:?}", diagnostics).to_ascii_lowercase())
    .arg("--timeout")
    .arg(worker_timeout_secs.to_string());

  if args.no_http_freshness {
    cmd.arg("--no-http-freshness");
  }
  if args.fonts.bundled_fonts {
    cmd.arg("--bundled-fonts");
  }
  for dir in &args.fonts.font_dir {
    cmd.arg("--font-dir").arg(dir);
  }
  if args.verbose {
    cmd.arg("--verbose");
  }
  if let Some(limit) = args.css_limit {
    cmd.arg("--css-limit").arg(limit.to_string());
  }
  if args.resource_access.allow_file_from_http {
    cmd.arg("--allow-file-from-http");
  }
  if args.resource_access.block_mixed_content {
    cmd.arg("--block-mixed-content");
  }
  if args.resource_access.same_origin_subresources {
    cmd.arg("--same-origin-subresources");
  }
  for origin in &args.resource_access.allow_subresource_origin {
    cmd.arg("--allow-subresource-origin").arg(origin);
  }
  match args.layout_parallel.layout_parallel {
    LayoutParallelModeArg::Off => {
      cmd.arg("--layout-parallel").arg("off");
    }
    LayoutParallelModeArg::On => {
      cmd.arg("--layout-parallel").arg("on");
    }
    LayoutParallelModeArg::Auto => {
      cmd.arg("--layout-parallel").arg("auto");
    }
  }
  if args.layout_parallel.layout_parallel != LayoutParallelModeArg::Off {
    cmd
      .arg("--layout-parallel-min-fanout")
      .arg(args.layout_parallel.layout_parallel_min_fanout.to_string());
    if let Some(max_threads) = args.layout_parallel.layout_parallel_max_threads {
      cmd
        .arg("--layout-parallel-max-threads")
        .arg(max_threads.to_string());
    }
    if let Some(min_nodes) = args.layout_parallel.layout_parallel_auto_min_nodes {
      cmd
        .arg("--layout-parallel-auto-min-nodes")
        .arg(min_nodes.to_string());
    }
  }
  if let Some(profile) = args.compat.compat_profile_arg() {
    cmd.arg("--compat-profile").arg(profile.as_str());
  }
  if let Some(mode) = args.compat.dom_compat_arg() {
    cmd.arg("--dom-compat").arg(mode.as_str());
  }
  if let Some(ms) = soft_timeout_ms {
    cmd.arg("--soft-timeout-ms").arg(ms.to_string());
  }
  if let Some(path) = &item.trace_out {
    cmd.arg("--trace-out").arg(path);
  }
  if let Some(dump_settings) = dump {
    if let Some(level) = dump_settings.failures {
      cmd.arg("--dump-failures").arg(level.as_str());
    }
    if let Some(ms) = dump_settings.slow_ms {
      cmd.arg("--dump-slow-ms").arg(format!("{ms}"));
    }
    if let Some(level) = dump_settings.slow {
      cmd.arg("--dump-slow").arg(level.as_str());
    }
    cmd.arg("--dump-dir").arg(&dump_settings.dir);
    cmd
      .arg("--dump-timeout")
      .arg(dump_settings.timeout_secs.to_string());
    if let Some(ms) = dump_settings.soft_timeout_ms {
      cmd.arg("--dump-soft-timeout-ms").arg(ms.to_string());
    }
  }

  if cascade_profile {
    cmd.env("FASTR_CASCADE_PROFILE", "1");
  }

  if let Some(parent) = item.stderr_path.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent)?;
    }
  }
  let stderr_file = File::create(&item.stderr_path)?;
  let stdout_file = stderr_file.try_clone()?;

  // Keep worker I/O deterministic: worker-authored logs stay separate, while stdout/stderr
  // from the process go to a dedicated stderr log for crash/debug output.
  cmd
    .stdin(Stdio::null())
    .stdout(Stdio::from(stdout_file))
    .stderr(Stdio::from(stderr_file));

  #[cfg(test)]
  apply_worker_env_overrides(&mut cmd);

  cmd.spawn()
}

fn run_queue(
  exe: &Path,
  args: &RunArgs,
  mut queue: std::collections::VecDeque<WorkItem>,
  worker_timeout: Duration,
  kill_timeout: Duration,
  soft_timeout_ms: Option<u64>,
  diagnostics: DiagnosticsArg,
  cascade_profile: bool,
  jobs: usize,
  dump: Option<&DumpSettings>,
) -> io::Result<()> {
  let mut running: Vec<RunningChild> = Vec::new();
  let current_sha = current_git_sha();

  while !queue.is_empty() || !running.is_empty() {
    while running.len() < jobs {
      let Some(item) = queue.pop_front() else {
        break;
      };
      let _ = fs::remove_file(&item.stage_path);
      let _ = fs::remove_file(stage_tmp_path(&item.stage_path));
      let _ = fs::remove_file(stage_timeline_path(&item.stage_path));
      let _ = fs::remove_file(progress_sentinel_path(&item.stage_path));
      let child = spawn_worker(
        exe,
        args,
        &item,
        diagnostics,
        cascade_profile,
        soft_timeout_ms,
        worker_timeout,
        dump,
      )?;
      running.push(RunningChild {
        item,
        started: Instant::now(),
        child,
      });
    }

    let mut i = 0usize;
    while i < running.len() {
      let elapsed = running[i].started.elapsed();
      let progress_written = progress_sentinel_path(&running[i].item.stage_path).exists();
      let effective_kill_timeout = if progress_written {
        kill_timeout
      } else {
        worker_timeout
      };
      let timed_out = elapsed >= effective_kill_timeout;

      if timed_out {
        let mut entry = running.swap_remove(i);
        let _ = entry.child.kill();
        let _ = entry.child.wait();
        let progress_written = progress_sentinel_path(&entry.item.stage_path).exists();
        if progress_written {
          // The worker already committed progress and is now likely stuck in optional dump capture.
          // Don't overwrite the committed progress file with a synthetic timeout.
          let _ = append_log_line(
            &entry.item.stderr_path,
            "parent killed worker during dump capture (progress already written)",
          );
          eprintln!("KILLED_AFTER_PROGRESS {}", entry.item.cache_stem);
          continue;
        }

        append_timeout_stderr_note(&entry.item.stderr_path, elapsed);

        let previous = read_progress(&entry.item.progress_path);
        let heartbeat_stage = read_stage_heartbeat(&entry.item.stage_path);
        let timeout_stage = infer_hard_timeout_stage(heartbeat_stage, previous.as_ref());
        let timeout_hotspot = hotspot_from_progress_stage(timeout_stage).to_string();
        let mut progress = PageProgress::new(entry.item.url.clone());
        progress.status = ProgressStatus::Timeout;
        let total_ms = worker_timeout.as_millis() as u64;
        progress.total_ms = Some(total_ms as f64);
        progress.stages_ms =
          stage_buckets_from_timeline(&entry.item.stage_path, total_ms).unwrap_or_default();
        progress.auto_notes = format!("hard timeout after {:.2}s", worker_timeout.as_secs_f64());
        if let Some(stage) = heartbeat_stage {
          progress.auto_notes = format!("{}\nstage: {}", progress.auto_notes, stage.as_str());
        }
        progress.hotspot = timeout_hotspot.clone();
        progress.timeout_stage = Some(timeout_stage);
        let mut progress = progress.merge_preserving_manual(previous, current_sha.as_deref());
        progress.hotspot = timeout_hotspot;
        progress.timeout_stage = Some(timeout_stage);
        let _ = write_progress(&entry.item.progress_path, &progress);

        let _ = write_text_file(
          &entry.item.log_path,
          &format!(
            "=== {} ===\nStatus: TIMEOUT\nKilled after {:.2}s\n",
            entry.item.cache_stem,
            worker_timeout.as_secs_f64()
          ),
        );

        eprintln!("TIMEOUT {}", entry.item.cache_stem);
        if let Some(trace_out) = &entry.item.trace_out {
          if let Some(msg) = trace_issue_message(
            trace_out,
            Some("Trace likely incomplete: worker was killed at the hard timeout."),
          ) {
            let _ = append_log_line(&entry.item.log_path, &msg);
          }
        }
        continue;
      }

      match running[i].child.try_wait() {
        Ok(Some(status)) => {
          let exit_summary = summarize_exit_status(&status);
          let exit_str = format_exit_status(exit_summary);
          let entry = running.swap_remove(i);
          if let Some(p) = read_progress(&entry.item.progress_path) {
            if p.status != ProgressStatus::Ok {
              let note = if p.auto_notes.trim().is_empty() {
                p.notes.as_str()
              } else {
                p.auto_notes.as_str()
              };
              let short: String = note.chars().take(120).collect();
              eprintln!(
                "FAIL {} {:?}: {} (exit: {})",
                entry.item.cache_stem, p.status, short, exit_str
              );
            }
          } else {
            // Fallback: worker exited but didn't write progress.
            let previous = read_progress(&entry.item.progress_path);
            let heartbeat_stage = read_stage_heartbeat(&entry.item.stage_path);
            let mut progress = PageProgress::new(entry.item.url.clone());
            progress.status = ProgressStatus::Panic;
            progress.total_ms = Some(entry.started.elapsed().as_secs_f64() * 1000.0);
            let note = synthesize_missing_progress_note(exit_summary);
            progress.auto_notes = note.clone();
            progress.failure_stage = heartbeat_stage.and_then(progress_stage_from_heartbeat);
            if let Some(stage) = heartbeat_stage {
              maybe_apply_hotspot(&mut progress, previous.as_ref(), stage.hotspot(), false);
            }
            if progress.hotspot.trim().is_empty() {
              progress.hotspot = "unknown".to_string();
            }
            if let Some(tail) = stderr_tail(&entry.item.stderr_path, 2048, 20) {
              ensure_auto_note_includes(
                &mut progress,
                &format!(
                  "stderr tail ({}):\n{}",
                  entry.item.stderr_path.display(),
                  tail
                ),
              );
            }
            let mut progress = progress.merge_preserving_manual(previous, current_sha.as_deref());
            if let Some(stage) = heartbeat_stage {
              ensure_auto_note_includes(&mut progress, &format!("stage: {}", stage.as_str()));
              if is_hotspot_unset(&progress.hotspot) {
                progress.hotspot = stage.hotspot().to_string();
              }
            }
            let _ = write_progress(&entry.item.progress_path, &progress);
            eprintln!(
              "PANIC {} (no progress written, exit: {})",
              entry.item.cache_stem, exit_str
            );
          }
          if let Some(trace_out) = &entry.item.trace_out {
            if let Some(msg) = trace_issue_message(trace_out, None) {
              let _ = append_log_line(&entry.item.log_path, &msg);
            }
          }
          continue;
        }
        Ok(None) => {
          i += 1;
        }
        Err(_) => {
          // Treat as crash and move on.
          let entry = running.swap_remove(i);
          let previous = read_progress(&entry.item.progress_path);
          let heartbeat_stage = read_stage_heartbeat(&entry.item.stage_path);
          let mut progress = PageProgress::new(entry.item.url.clone());
          progress.status = ProgressStatus::Panic;
          progress.total_ms = Some(entry.started.elapsed().as_secs_f64() * 1000.0);
          progress.auto_notes = "worker try_wait failed".to_string();
          progress.failure_stage = heartbeat_stage.and_then(progress_stage_from_heartbeat);
          if let Some(stage) = heartbeat_stage {
            maybe_apply_hotspot(&mut progress, previous.as_ref(), stage.hotspot(), false);
          }
          if progress.hotspot.trim().is_empty() {
            progress.hotspot = "unknown".to_string();
          }
          if let Some(tail) = stderr_tail(&entry.item.stderr_path, 2048, 20) {
            ensure_auto_note_includes(
              &mut progress,
              &format!(
                "stderr tail ({}):\n{}",
                entry.item.stderr_path.display(),
                tail
              ),
            );
          }
          let mut progress = progress.merge_preserving_manual(previous, current_sha.as_deref());
          if let Some(stage) = heartbeat_stage {
            ensure_auto_note_includes(&mut progress, &format!("stage: {}", stage.as_str()));
            if is_hotspot_unset(&progress.hotspot) {
              progress.hotspot = stage.hotspot().to_string();
            }
          }
          let _ = write_progress(&entry.item.progress_path, &progress);
          eprintln!("PANIC {} (try_wait failed)", entry.item.cache_stem);
          if let Some(trace_out) = &entry.item.trace_out {
            if let Some(msg) = trace_issue_message(
              trace_out,
              Some("Trace likely incomplete because the worker crashed before flushing it."),
            ) {
              let _ = append_log_line(&entry.item.log_path, &msg);
            }
          }
          continue;
        }
      }
    }

    std::thread::sleep(Duration::from_millis(20));
  }

  Ok(())
}

struct WorkerCleanupDelayGuard(Option<Duration>);

impl WorkerCleanupDelayGuard {
  fn from_env() -> Self {
    let delay = std::env::var("FASTR_TEST_WORKER_DROP_DELAY_MS")
      .ok()
      .and_then(|v| v.parse::<u64>().ok())
      .map(Duration::from_millis);
    Self(delay)
  }
}

impl Drop for WorkerCleanupDelayGuard {
  fn drop(&mut self) {
    if let Some(delay) = self.0 {
      std::thread::sleep(delay);
    }
  }
}

fn infer_hard_timeout_stage(
  heartbeat_stage: Option<StageHeartbeat>,
  previous: Option<&PageProgress>,
) -> ProgressStage {
  heartbeat_stage
    .and_then(|stage| {
      progress_stage_from_heartbeat(stage)
        .or_else(|| (stage == StageHeartbeat::Done).then_some(ProgressStage::Paint))
    })
    .or_else(|| {
      previous
        .and_then(|p| p.timeout_stage.or(p.failure_stage))
        .or(Some(ProgressStage::DomParse))
    })
    .unwrap_or(ProgressStage::DomParse)
}

fn progress_has_cascade_profile_stats(progress: &PageProgress) -> bool {
  progress
    .diagnostics
    .as_ref()
    .and_then(|d| d.stats.as_ref())
    .is_some_and(|stats| {
      stats.cascade.nodes.is_some()
        || stats.cascade.rule_candidates.is_some()
        || stats.cascade.selector_time_ms.is_some()
        || stats.cascade.has_evals.is_some()
    })
}

fn progress_is_cascade_timeout(progress: &PageProgress) -> bool {
  progress.status == ProgressStatus::Timeout
    && (progress.hotspot.eq_ignore_ascii_case("cascade")
      || matches!(progress.timeout_stage, Some(ProgressStage::Cascade))
      || matches!(progress.failure_stage, Some(ProgressStage::Cascade)))
}

fn merge_cascade_diagnostics(dst: &mut CascadeDiagnostics, src: &CascadeDiagnostics) {
  dst.nodes = dst.nodes.or(src.nodes);
  dst.rule_candidates = dst.rule_candidates.or(src.rule_candidates);
  dst.rule_matches = dst.rule_matches.or(src.rule_matches);
  dst.rule_candidates_pruned = dst.rule_candidates_pruned.or(src.rule_candidates_pruned);
  dst.rule_candidates_by_id = dst.rule_candidates_by_id.or(src.rule_candidates_by_id);
  dst.rule_candidates_by_class = dst
    .rule_candidates_by_class
    .or(src.rule_candidates_by_class);
  dst.rule_candidates_by_tag = dst.rule_candidates_by_tag.or(src.rule_candidates_by_tag);
  dst.rule_candidates_by_attr = dst.rule_candidates_by_attr.or(src.rule_candidates_by_attr);
  dst.rule_candidates_universal = dst
    .rule_candidates_universal
    .or(src.rule_candidates_universal);
  dst.selector_attempts_total = dst.selector_attempts_total.or(src.selector_attempts_total);
  dst.selector_attempts_after_bloom = dst
    .selector_attempts_after_bloom
    .or(src.selector_attempts_after_bloom);
  dst.selector_bloom_fast_rejects = dst
    .selector_bloom_fast_rejects
    .or(src.selector_bloom_fast_rejects);
  dst.selector_time_ms = dst.selector_time_ms.or(src.selector_time_ms);
  dst.declaration_time_ms = dst.declaration_time_ms.or(src.declaration_time_ms);
  dst.pseudo_time_ms = dst.pseudo_time_ms.or(src.pseudo_time_ms);
  dst.has_evals = dst.has_evals.or(src.has_evals);
  dst.has_cache_hits = dst.has_cache_hits.or(src.has_cache_hits);
  dst.has_prunes = dst.has_prunes.or(src.has_prunes);
  dst.has_bloom_prunes = dst.has_bloom_prunes.or(src.has_bloom_prunes);
  dst.has_filter_prunes = dst.has_filter_prunes.or(src.has_filter_prunes);
  dst.has_evaluated = dst.has_evaluated.or(src.has_evaluated);
}

fn merge_cascade_stats_into_progress(progress: &mut PageProgress, rerun_stats: &RenderStats) {
  let diagnostics = progress
    .diagnostics
    .get_or_insert_with(ProgressDiagnostics::default);
  let stats = diagnostics.stats.get_or_insert_with(RenderStats::default);
  merge_cascade_diagnostics(&mut stats.cascade, &rerun_stats.cascade);
}

fn run(args: RunArgs) -> io::Result<()> {
  if args.jobs == 0 {
    eprintln!("jobs must be > 0");
    std::process::exit(2);
  }
  if args.timeout == 0 {
    eprintln!("timeout must be > 0 (hard timeout)");
    std::process::exit(2);
  }
  if args.trace_jobs == 0 {
    eprintln!("trace-jobs must be > 0");
    std::process::exit(2);
  }
  if matches!(args.trace_timeout, Some(0)) {
    eprintln!("trace-timeout must be > 0 when set");
    std::process::exit(2);
  }
  if matches!(args.dump_timeout, Some(0)) {
    eprintln!("dump-timeout must be > 0 when set");
    std::process::exit(2);
  }

  let hard_timeout = Duration::from_secs(args.timeout);
  let soft_timeout_ms = compute_soft_timeout_ms(hard_timeout, args.soft_timeout_ms);
  let trace_hard_timeout = compute_trace_hard_timeout(args.timeout, args.trace_timeout);
  let trace_soft_timeout_ms =
    compute_soft_timeout_ms(trace_hard_timeout, args.trace_soft_timeout_ms);
  let dumps_enabled = args.dump_failures.is_some() || args.dump_slow.is_some();
  let dump_timeout_secs = args
    .dump_timeout
    .unwrap_or_else(|| args.timeout.saturating_mul(2));
  let dump_hard_timeout = Duration::from_secs(dump_timeout_secs);
  let dump_soft_timeout_ms = compute_soft_timeout_ms(dump_hard_timeout, args.dump_soft_timeout_ms);
  let worker_hard_timeout = if dumps_enabled {
    hard_timeout + dump_hard_timeout
  } else {
    hard_timeout
  };

  fs::create_dir_all(&args.progress_dir)?;
  fs::create_dir_all(&args.log_dir)?;
  if args.trace_failures || args.trace_slow_ms.is_some() {
    fs::create_dir_all(&args.trace_dir)?;
    fs::create_dir_all(&args.trace_progress_dir)?;
  }
  #[cfg(feature = "disk_cache")]
  {
    // Ensure disk-backed cache is available before spawning workers.
    fs::create_dir_all(ASSET_DIR)?;
  }
  if dumps_enabled {
    fs::create_dir_all(&args.dump_dir)?;
  }

  let page_filter = args
    .pages
    .as_ref()
    .and_then(|v| PagesetFilter::from_inputs(v));

  let cached_paths = match collect_cached_html_paths() {
    Ok(paths) => paths,
    Err(_) => {
      eprintln!("No cached pages found in {CACHE_HTML_DIR}. Run fetch_pages first.");
      std::process::exit(1);
    }
  };
  let cached_by_stem = cached_html_index_by_pageset_stem(&cached_paths);
  if cached_paths.is_empty() {
    eprintln!("No cached pages in {CACHE_HTML_DIR}. Run fetch_pages first.");
    std::process::exit(1);
  }

  let pageset_entries = pageset_entries();
  let pageset_by_cache: HashMap<String, &PagesetEntry> = pageset_entries
    .iter()
    .map(|entry| (entry.cache_stem.clone(), entry))
    .collect();
  let mut pageset_by_stem: HashMap<String, Vec<&PagesetEntry>> = HashMap::new();
  for entry in &pageset_entries {
    pageset_by_stem
      .entry(entry.stem.clone())
      .or_default()
      .push(entry);
  }

  let filter_matches = |cache_stem: &str, entry: Option<&PagesetEntry>| -> bool {
    match page_filter.as_ref() {
      Some(filter) => {
        if let Some(entry) = entry {
          filter.matches_entry(entry)
        } else {
          filter.matches_cache_stem(cache_stem, pageset_stem(cache_stem).as_deref())
        }
      }
      None => true,
    }
  };

  let mut items: Vec<WorkItem> = Vec::new();

  if args.selection.is_active() {
    let selection_dir = args.selection.from_progress.as_ref().unwrap();
    let selection = args.selection.to_selection();
    let outcome = read_progress_dir_tolerant(selection_dir);
    for warning in &outcome.warnings {
      eprintln!("Progress selection warning: {warning}");
    }

    let selected = select_progress_entries(&outcome.progresses, &selection);
    if selected.is_empty() {
      println!(
        "No pages matched the selection in {}. Nothing to do.",
        selection_dir.display()
      );
      return Ok(());
    }

    let mut stems: Vec<String> = selected.iter().map(|p| p.stem.clone()).collect();
    if let Some(ref filter) = page_filter {
      stems = stems
        .into_iter()
        .filter(|stem| filter.matches_cache_stem(stem, pageset_stem(stem).as_deref()))
        .collect();
    }

    stems = apply_shard_filter(stems, args.shard);

    let mut missing_cache: Vec<String> = Vec::new();
    let mut invalid_stems: Vec<String> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();

    for stem in stems {
      if seen.contains(&stem) {
        continue;
      }
      if let Some(path) = cached_paths.get(&stem) {
        let entry =
          resolve_pageset_entry_for_cache_stem(&stem, &pageset_by_cache, &pageset_by_stem);
        let effective_cache_stem = entry
          .map(|e| e.cache_stem.as_str())
          .unwrap_or(stem.as_str());
        let cache_path = entry
          .and_then(|entry| {
            cached_html_path_for_pageset_entry(entry, &cached_paths, &cached_by_stem)
          })
          .unwrap_or_else(|| path.clone());
        if filter_matches(effective_cache_stem, entry)
          && seen.insert(effective_cache_stem.to_string())
        {
          items.push(work_item_from_cache(
            effective_cache_stem,
            cache_path,
            entry,
            &args,
          ));
        }
        continue;
      }
      let Some(normalized) = pageset_stem(&stem) else {
        invalid_stems.push(stem);
        continue;
      };
      let Some(entries) = pageset_by_stem.get(&normalized) else {
        missing_cache.push(stem);
        continue;
      };
      let mut found = false;
      for entry in entries {
        let path = cached_html_path_for_pageset_entry(entry, &cached_paths, &cached_by_stem);
        if let Some(path) = path {
          if !filter_matches(&entry.cache_stem, Some(entry)) {
            continue;
          }
          found = true;
          if seen.insert(entry.cache_stem.clone()) {
            items.push(work_item_from_cache(
              &entry.cache_stem,
              path.clone(),
              Some(entry),
              &args,
            ));
          }
        }
      }
      if !found {
        missing_cache.push(stem);
      }
    }

    if !invalid_stems.is_empty() {
      eprintln!(
        "Skipping stems that could not be normalized: {}",
        invalid_stems.join(", ")
      );
    }
    if !missing_cache.is_empty() {
      eprintln!(
        "Missing cached HTML for selected pages: {}",
        missing_cache.join(", ")
      );
    }

    if items.is_empty() {
      println!(
        "No pages matched the selection in {} after filtering and cache lookup.",
        selection_dir.display()
      );
      return Ok(());
    }

    println!(
      "Selected {} page(s) from {} ({}):",
      items.len(),
      selection_dir.display(),
      if selection.union {
        "union"
      } else {
        "intersection"
      }
    );
    for item in &items {
      println!("  {}", item.cache_stem);
    }
    println!();
  } else {
    let mut seen_cache_stems: HashSet<String> = HashSet::new();
    let mut filtered_paths: Vec<(String, PathBuf, Option<&PagesetEntry>)> = cached_paths
      .iter()
      .filter_map(|(cache_stem, cache_path)| {
        let entry =
          resolve_pageset_entry_for_cache_stem(cache_stem, &pageset_by_cache, &pageset_by_stem);
        let effective_cache_stem = entry
          .map(|e| e.cache_stem.as_str())
          .unwrap_or(cache_stem.as_str());
        if !filter_matches(effective_cache_stem, entry) {
          return None;
        }
        if !seen_cache_stems.insert(effective_cache_stem.to_string()) {
          return None;
        }
        Some((effective_cache_stem.to_string(), cache_path.clone(), entry))
      })
      .collect();
    filtered_paths.sort_by(|a, b| a.0.cmp(&b.0));
    let matched_count = filtered_paths.len();
    filtered_paths = apply_shard_filter(filtered_paths, args.shard);
    if filtered_paths.is_empty() {
      if matched_count > 0 && args.shard.is_some() {
        let (index, total) = args.shard.expect("shard present");
        println!(
          "Shard {}/{} selected no cached pages ({} matched before sharding). Nothing to do.",
          index, total, matched_count
        );
        return Ok(());
      }
      if let Some(ref filter) = page_filter {
        let requested: Vec<&PagesetEntry> = pageset_entries
          .iter()
          .filter(|entry| filter.matches_entry(entry))
          .collect();
        if !requested.is_empty() {
          let mut missing: Vec<String> = requested
            .iter()
            .filter_map(|entry| {
              cached_html_path_for_pageset_entry(entry, &cached_paths, &cached_by_stem)
                .is_none()
                .then(|| entry.cache_stem.clone())
            })
            .collect();
          if !missing.is_empty() {
            missing.sort();
            missing.dedup();
            eprintln!(
              "Missing cached HTML for requested pages: {}",
              missing.join(", ")
            );
          }
        }
      }
      eprintln!(
        "No cached pages matched the requested filters (--pages/shard) in {CACHE_HTML_DIR}."
      );
      std::process::exit(1);
    }
    items = filtered_paths
      .into_iter()
      .map(|(cache_stem, cache_path, entry)| {
        work_item_from_cache(&cache_stem, cache_path, entry, &args)
      })
      .collect();
  }
  let dump_settings = if dumps_enabled {
    Some(DumpSettings {
      failures: args.dump_failures,
      slow: args.dump_slow,
      slow_ms: args.dump_slow_ms,
      dir: args.dump_dir.clone(),
      timeout_secs: dump_timeout_secs,
      soft_timeout_ms: dump_soft_timeout_ms,
    })
  } else {
    None
  };
  let exe = std::env::current_exe()?;
  println!(
    "Pageset progress: {} pages ({} parallel), hard timeout {}s",
    items.len(),
    args.jobs,
    worker_hard_timeout.as_secs()
  );
  println!(
    "User-Agent: {}",
    normalize_user_agent_for_log(&args.user_agent)
  );
  println!("Accept-Language: {}", args.accept_language);
  #[cfg(feature = "disk_cache")]
  {
    let max_age = if args.disk_cache.max_age_secs == 0 {
      "none".to_string()
    } else {
      format!("{}s", args.disk_cache.max_age_secs)
    };
    println!(
      "Disk cache: max_bytes={} max_age={}",
      args.disk_cache.max_bytes, max_age
    );
    let lock_stale_after = Duration::from_secs(args.disk_cache.lock_stale_secs);
    match common::disk_cache_stats::scan_disk_cache_dir(Path::new(ASSET_DIR), lock_stale_after) {
      Ok(stats) => {
        println!(
          "Disk cache stats: bin_count={} meta_count={} alias_count={} bin_bytes={} locks={} stale_locks={} tmp={} journal={}",
          stats.bin_count,
          stats.meta_count,
          stats.alias_count,
          stats.bin_bytes,
          stats.lock_count,
          stats.stale_lock_count,
          stats.tmp_count,
          stats.journal_bytes
        );
        println!("{}", stats.usage_summary(args.disk_cache.max_bytes));
        if args.disk_cache.max_bytes != 0 && stats.bin_bytes > args.disk_cache.max_bytes {
          println!(
            "Warning: disk cache usage exceeds max_bytes (bin_bytes={} > max_bytes={}). Consider increasing --disk-cache-max-bytes or setting FASTR_DISK_CACHE_MAX_BYTES=0 to disable eviction.",
            stats.bin_bytes, args.disk_cache.max_bytes
          );
        }
        if stats.stale_lock_count > 0 {
          println!(
            "Warning: disk cache contains {} stale .lock file(s). Consider tuning FASTR_DISK_CACHE_LOCK_STALE_SECS (currently {}).",
            stats.stale_lock_count, args.disk_cache.lock_stale_secs
          );
        }
      }
      Err(err) => println!("Disk cache stats: unavailable ({err})"),
    }
  }
  if args.fonts.bundled_fonts {
    println!("Fonts: bundled fixtures (system fonts skipped)");
  } else if !args.fonts.font_dir.is_empty() {
    let dirs = args
      .fonts
      .font_dir
      .iter()
      .map(|p| p.display().to_string())
      .collect::<Vec<_>>()
      .join(", ");
    println!("Fonts: {}", dirs);
  }
  println!("Progress dir: {}", args.progress_dir.display());
  println!();

  let overall_start = Instant::now();
  let queue = std::collections::VecDeque::from(items.clone());
  run_queue(
    &exe,
    &args,
    queue,
    hard_timeout,
    worker_hard_timeout,
    soft_timeout_ms,
    args.diagnostics,
    false,
    args.jobs,
    dump_settings.as_ref(),
  )?;

  // Summary counters based on produced progress files (main run only).
  let mut ok = 0usize;
  let mut timeout = 0usize;
  let mut panic = 0usize;
  let mut error = 0usize;
  for item in &items {
    if let Some(p) = read_progress(&item.progress_path) {
      match p.status {
        ProgressStatus::Ok => ok += 1,
        ProgressStatus::Timeout => timeout += 1,
        ProgressStatus::Panic => panic += 1,
        ProgressStatus::Error => error += 1,
      }
    } else {
      panic += 1;
    }
  }
  let total = ok + timeout + panic + error;

  // Optional cascade diagnostics reruns.
  //
  // We keep the main run (and its timings) free of cascade profiling overhead, but for cascade
  // hotspots it's often useful to have selector candidate/match counters and `:has()` counters.
  // Rerun a small subset of pages with `FASTR_CASCADE_PROFILE=1` and merge the resulting cascade
  // stats into the committed progress JSON.
  if args.cascade_diagnostics {
    fs::create_dir_all(&args.cascade_diagnostics_progress_dir)?;
    let slow_threshold_ms = args.cascade_diagnostics_slow_ms.unwrap_or(500.0);
    let rerun_hard_timeout = Duration::from_secs(args.timeout.saturating_mul(2));
    let rerun_soft_timeout_ms = compute_soft_timeout_ms(rerun_hard_timeout, None);
    let rerun_diagnostics = match args.diagnostics {
      DiagnosticsArg::None => DiagnosticsArg::Basic,
      other => other,
    };

    let mut rerun_items: Vec<WorkItem> = Vec::new();
    for item in &items {
      let Some(p) = read_progress(&item.progress_path) else {
        continue;
      };
      if progress_has_cascade_profile_stats(&p) {
        continue;
      }
      let needs_rerun = match p.status {
        ProgressStatus::Ok => p
          .diagnostics
          .as_ref()
          .and_then(|d| d.stats.as_ref())
          .and_then(|s| s.timings.cascade_ms)
          .is_some_and(|ms| ms > slow_threshold_ms),
        ProgressStatus::Timeout => progress_is_cascade_timeout(&p),
        ProgressStatus::Panic | ProgressStatus::Error => false,
      };
      if !needs_rerun {
        continue;
      }
      rerun_items.push(WorkItem {
        stem: item.stem.clone(),
        cache_stem: item.cache_stem.clone(),
        url: item.url.clone(),
        cache_path: item.cache_path.clone(),
        progress_path: args
          .cascade_diagnostics_progress_dir
          .join(format!("{}.json", item.cache_stem)),
        log_path: args
          .log_dir
          .join(format!("{}.cascade.log", item.cache_stem)),
        stderr_path: args
          .log_dir
          .join(format!("{}.cascade.stderr.log", item.cache_stem)),
        stage_path: args
          .log_dir
          .join(format!("{}.cascade.stage", item.cache_stem)),
        trace_out: None,
      });
    }

    if !rerun_items.is_empty() {
      let soft_desc = rerun_soft_timeout_ms
        .map(|ms| format!("{ms}ms"))
        .unwrap_or_else(|| "disabled".to_string());
      println!(
        "Cascade profiling {} pages (jobs={}, hard timeout {}s, soft timeout {}) writing temp progress to {}...",
        rerun_items.len(),
        args.jobs,
        rerun_hard_timeout.as_secs(),
        soft_desc,
        args.cascade_diagnostics_progress_dir.display()
      );

      let queue = std::collections::VecDeque::from(rerun_items.clone());
      run_queue(
        &exe,
        &args,
        queue,
        rerun_hard_timeout,
        rerun_hard_timeout,
        rerun_soft_timeout_ms,
        rerun_diagnostics,
        true,
        args.jobs,
        None,
      )?;

      // Merge rerun cascade diagnostics into the committed progress artifacts.
      for rerun in &rerun_items {
        let Some(rerun_progress) = read_progress(&rerun.progress_path) else {
          continue;
        };
        let Some(rerun_stats) = rerun_progress
          .diagnostics
          .as_ref()
          .and_then(|d| d.stats.as_ref())
        else {
          continue;
        };
        let committed_path = args.progress_dir.join(format!("{}.json", rerun.cache_stem));
        let Some(mut committed) = read_progress(&committed_path) else {
          continue;
        };
        merge_cascade_stats_into_progress(&mut committed, rerun_stats);
        write_progress(&committed_path, &committed)?;
      }
    }
  }

  // Optional trace reruns (kept out of the committed progress dir).
  if args.trace_failures || args.trace_slow_ms.is_some() {
    let mut trace_items: Vec<WorkItem> = Vec::new();
    for item in &items {
      let Some(p) = read_progress(&item.progress_path) else {
        continue;
      };
      let needs_trace = match p.status {
        ProgressStatus::Ok => args
          .trace_slow_ms
          .and_then(|threshold| p.total_ms.map(|ms| ms > threshold))
          .unwrap_or(false),
        ProgressStatus::Timeout | ProgressStatus::Panic | ProgressStatus::Error => {
          args.trace_failures
        }
      };
      if !needs_trace {
        continue;
      }
      let trace_out = args.trace_dir.join(format!("{}.json", item.cache_stem));
      let trace_progress = args
        .trace_progress_dir
        .join(format!("{}.json", item.cache_stem));
      trace_items.push(WorkItem {
        stem: item.stem.clone(),
        cache_stem: item.cache_stem.clone(),
        url: item.url.clone(),
        cache_path: item.cache_path.clone(),
        progress_path: trace_progress,
        log_path: args.log_dir.join(format!("{}.trace.log", item.cache_stem)),
        stderr_path: args
          .log_dir
          .join(format!("{}.trace.stderr.log", item.cache_stem)),
        stage_path: args
          .log_dir
          .join(format!("{}.trace.stage", item.cache_stem)),
        trace_out: Some(trace_out),
      });
    }
    if !trace_items.is_empty() {
      let soft_trace_desc = trace_soft_timeout_ms
        .map(|ms| format!("{ms}ms"))
        .unwrap_or_else(|| "disabled".to_string());
      println!(
        "Tracing {} pages (jobs={}, hard timeout {}s, soft timeout {}) writing to {}...",
        trace_items.len(),
        args.trace_jobs,
        trace_hard_timeout.as_secs(),
        soft_trace_desc,
        args.trace_dir.display()
      );
      let trace_queue = std::collections::VecDeque::from(trace_items);
      run_queue(
        &exe,
        &args,
        trace_queue,
        trace_hard_timeout,
        trace_hard_timeout,
        trace_soft_timeout_ms,
        DiagnosticsArg::Verbose,
        false,
        args.trace_jobs,
        None,
      )?;
    }
  }

  let elapsed = overall_start.elapsed();
  println!();
  println!(
    "Done: {total} pages in {:.2}s (ok={ok}, timeout={timeout}, panic={panic}, error={error})",
    elapsed.as_secs_f64()
  );
  Ok(())
}

fn worker(args: WorkerArgs) -> io::Result<()> {
  if args.cache_path.as_os_str().is_empty() {
    return Ok(());
  }
  let progress_path = args.progress_path.clone();
  let log_path = args.log_path.clone();
  let stage_path = args.stage_path.clone();
  let stem = args.stem.clone();
  let cache_path = args.cache_path.clone();
  let verbose = args.verbose;
  let started = Instant::now();
  let current_sha = current_git_sha();

  let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| render_worker(args)));
  match result {
    Ok(res) => res,
    Err(panic) => {
      let panic_msg = panic_to_string(panic);
      let progress_written = stage_path
        .as_ref()
        .map(|path| progress_sentinel_path(path).exists())
        .unwrap_or(false);
      if progress_written {
        // Progress for this run was already committed; don't overwrite it just because an optional
        // dump capture panicked.
        if let Some(path) = &log_path {
          let _ = append_log_line(
            path,
            &format!("Panic after progress was written (during dump capture?): {panic_msg}"),
          );
        }
        return Ok(());
      }

      let previous = read_progress(&progress_path);
      let url = url_hint_from_cache_path(&cache_path);
      let heartbeat_stage = stage_path
        .as_ref()
        .and_then(|path| read_stage_heartbeat(path));
      let mut progress = PageProgress::new(url);
      progress.status = ProgressStatus::Panic;
      progress.total_ms = Some(started.elapsed().as_secs_f64() * 1000.0);
      progress.auto_notes = panic_msg;
      progress.failure_stage = heartbeat_stage.and_then(progress_stage_from_heartbeat);
      if let Some(stage) = heartbeat_stage {
        maybe_apply_hotspot(&mut progress, previous.as_ref(), stage.hotspot(), false);
      }
      if progress.hotspot.trim().is_empty() {
        progress.hotspot = "unknown".to_string();
      }
      let mut progress = progress.merge_preserving_manual(previous, current_sha.as_deref());
      if let Some(stage) = heartbeat_stage {
        ensure_auto_note_includes(&mut progress, &format!("stage: {}", stage.as_str()));
        if is_hotspot_unset(&progress.hotspot) {
          progress.hotspot = stage.hotspot().to_string();
        }
      }
      let _ = write_progress_with_sentinel(&progress_path, stage_path.as_deref(), &progress);
      if let Some(path) = &log_path {
        let _ = write_text_file(
          path,
          &format!(
            "=== {stem} ===\nStatus: PANIC\nPanic: {}\nVerbose: {}\n",
            progress.auto_notes, verbose
          ),
        );
      }
      Ok(())
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use fastrender::api::RenderStageTimings;
  use std::collections::{HashSet, VecDeque};
  use std::fs;
  use std::io;
  use std::path::{Path, PathBuf};
  use std::time::Duration;
  use tempfile::tempdir;

  fn make_progress(
    stem: &str,
    status: ProgressStatus,
    total_ms: Option<f64>,
    hotspot: &str,
  ) -> LoadedProgress {
    let mut progress = PageProgress::new(format!("https://{stem}"));
    progress.status = status;
    progress.total_ms = total_ms;
    progress.hotspot = hotspot.to_string();
    LoadedProgress {
      stem: stem.to_string(),
      progress,
      stats: None,
    }
  }

  fn basic_report_args() -> ReportArgs {
    ReportArgs {
      progress_dir: PathBuf::new(),
      top: 10,
      compare: None,
      fail_on_regression: false,
      regression_threshold_percent: 10.0,
      fail_on_bad: false,
      include_trace: false,
      trace_progress_dir: PathBuf::new(),
      trace_dir: PathBuf::new(),
      verbose_stats: false,
      verbose: false,
      fail_on_missing_stages: false,
      fail_on_missing_stage_timings: false,
      fail_on_stage_sum_exceeds_total: false,
      stage_sum_tolerance_percent: 10.0,
      fail_on_slow_ok_ms: None,
      fail_on_ok_with_failures: false,
      fail_on_new_ok_failures: false,
    }
  }

  fn status_set(statuses: &[ProgressStatus]) -> HashSet<ProgressStatus> {
    let mut set = HashSet::new();
    for status in statuses {
      set.insert(*status);
    }
    set
  }

  #[test]
  fn report_gate_fails_on_missing_failure_stage() {
    let mut missing = make_progress("missing", ProgressStatus::Timeout, Some(15.0), "unknown");
    missing.progress.timeout_stage = None;
    missing.progress.failure_stage = None;

    let mut ok = make_progress("ok", ProgressStatus::Timeout, Some(12.0), "unknown");
    ok.progress.timeout_stage = Some(ProgressStage::Layout);

    let progresses = vec![missing, ok];

    let mut args = basic_report_args();
    args.fail_on_missing_stages = true;

    let err = evaluate_report_checks(&args, &progresses).unwrap_err();
    assert_eq!(err.missing_stages, vec!["missing".to_string()]);
    assert!(err.missing_stage_timings.is_empty());
    assert!(err.stage_sum_exceeds_total.is_empty());
  }

  #[test]
  fn report_gate_ignores_placeholder_failures_missing_stages() {
    let mut placeholder_timeout = make_progress(
      "placeholder_timeout",
      ProgressStatus::Timeout,
      None,
      "unknown",
    );
    placeholder_timeout.progress.timeout_stage = None;
    placeholder_timeout.progress.failure_stage = None;

    let mut placeholder_error =
      make_progress("placeholder_error", ProgressStatus::Error, None, "unknown");
    placeholder_error.progress.timeout_stage = None;
    placeholder_error.progress.failure_stage = None;

    let progresses = vec![placeholder_timeout, placeholder_error];

    let mut args = basic_report_args();
    args.fail_on_missing_stages = true;

    assert!(evaluate_report_checks(&args, &progresses).is_ok());
  }

  #[test]
  fn report_gate_fails_on_missing_stage_timings() {
    let missing = make_progress("missing", ProgressStatus::Ok, Some(15.0), "layout");

    let mut ok = make_progress("ok", ProgressStatus::Ok, Some(12.0), "layout");
    ok.progress.stages_ms.fetch = 1.0;

    let progresses = vec![missing, ok];

    let mut args = basic_report_args();
    args.fail_on_missing_stage_timings = true;

    let err = evaluate_report_checks(&args, &progresses).unwrap_err();
    assert_eq!(err.missing_stage_timings, vec!["missing".to_string()]);
    assert!(err.missing_stages.is_empty());
    assert!(err.stage_sum_exceeds_total.is_empty());
  }

  #[test]
  fn report_gate_fails_on_stage_sum_exceeds_total() {
    let mut offender = make_progress("offender", ProgressStatus::Ok, Some(100.0), "layout");
    offender.progress.stages_ms.fetch = 60.0;
    offender.progress.stages_ms.layout = 60.0; // 120ms total

    let mut ok = make_progress("ok", ProgressStatus::Ok, Some(100.0), "layout");
    ok.progress.stages_ms.fetch = 50.0;
    ok.progress.stages_ms.layout = 55.0; // 105ms total

    let progresses = vec![offender, ok];

    let mut args = basic_report_args();
    args.fail_on_stage_sum_exceeds_total = true;
    args.stage_sum_tolerance_percent = 10.0;

    let err = evaluate_report_checks(&args, &progresses).unwrap_err();
    assert_eq!(err.stage_sum_exceeds_total, vec!["offender".to_string()]);
    assert!(err.missing_stages.is_empty());
    assert!(err.missing_stage_timings.is_empty());
  }

  #[test]
  fn report_gate_allows_stage_sum_within_tolerance() {
    let mut page = make_progress("page", ProgressStatus::Ok, Some(100.0), "layout");
    page.progress.stages_ms.fetch = 60.0;
    page.progress.stages_ms.layout = 60.0; // 120ms total

    let progresses = vec![page];

    let mut args = basic_report_args();
    args.fail_on_stage_sum_exceeds_total = true;
    args.stage_sum_tolerance_percent = 25.0;

    assert!(evaluate_report_checks(&args, &progresses).is_ok());
  }

  struct EnvVarGuard {
    key: &'static str,
    previous: Option<String>,
  }

  fn env_lock() -> std::sync::MutexGuard<'static, ()> {
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    LOCK
      .get_or_init(|| std::sync::Mutex::new(()))
      .lock()
      .expect("env lock poisoned")
  }

  impl EnvVarGuard {
    fn set(key: &'static str, value: &str) -> Self {
      let previous = set_worker_env_override(key, value);
      Self { key, previous }
    }
  }

  impl Drop for EnvVarGuard {
    fn drop(&mut self) {
      restore_worker_env_override(self.key, self.previous.take());
    }
  }

  fn pageset_progress_exe() -> Option<PathBuf> {
    std::env::var("CARGO_BIN_EXE_pageset_progress")
      .ok()
      .map(PathBuf::from)
      .or_else(|| {
        let candidate = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
          .join("target")
          .join("debug")
          .join("pageset_progress");
        candidate.exists().then_some(candidate)
      })
  }

  fn basic_run_args(base: &Path) -> RunArgs {
    RunArgs {
      jobs: 1,
      timeout: 1,
      soft_timeout_ms: None,
      viewport: (120, 80),
      dpr: 1.0,
      user_agent: DEFAULT_USER_AGENT.to_string(),
      accept_language: DEFAULT_ACCEPT_LANGUAGE.to_string(),
      no_http_freshness: false,
      disk_cache: DiskCacheArgs {
        max_bytes: common::args::DEFAULT_DISK_CACHE_MAX_BYTES,
        max_age_secs: common::args::DEFAULT_DISK_CACHE_MAX_AGE_SECS,
        lock_stale_secs: common::args::DEFAULT_DISK_CACHE_LOCK_STALE_SECS,
        allow_no_store: false,
        writeback_under_deadline: false,
      },
      css_limit: None,
      fonts: FontSourceArgs {
        bundled_fonts: false,
        font_dir: Vec::new(),
      },
      resource_access: ResourceAccessArgs {
        allow_file_from_http: false,
        block_mixed_content: false,
        same_origin_subresources: false,
        allow_subresource_origin: Vec::new(),
      },
      layout_parallel: LayoutParallelArgs {
        layout_parallel: LayoutParallelModeArg::Off,
        layout_parallel_min_fanout: 8,
        layout_parallel_max_threads: None,
        layout_parallel_auto_min_nodes: None,
      },
      compat: CompatArgs::default(),
      pages: None,
      shard: None,
      selection: ProgressSelectionArgs::default(),
      progress_dir: base.to_path_buf(),
      log_dir: base.to_path_buf(),
      trace_failures: false,
      trace_slow_ms: None,
      trace_timeout: None,
      trace_soft_timeout_ms: None,
      trace_jobs: 1,
      trace_dir: base.join("trace"),
      trace_progress_dir: base.join("trace_progress"),
      cascade_diagnostics: false,
      cascade_diagnostics_slow_ms: None,
      cascade_diagnostics_progress_dir: base.join("cascade_progress"),
      dump_failures: None,
      dump_slow_ms: None,
      dump_slow: None,
      dump_dir: base.join("dump"),
      dump_timeout: None,
      dump_soft_timeout_ms: None,
      diagnostics: DiagnosticsArg::None,
      verbose: false,
    }
  }

  #[test]
  fn write_progress_roundtrips_json_with_newline() {
    let dir = tempdir().unwrap();
    let path = dir.path().join("page.json");

    let mut progress = PageProgress::new("https://example.com/".to_string());
    progress.status = ProgressStatus::Ok;
    progress.total_ms = Some(12.5);

    write_progress(&path, &progress).unwrap();

    let contents = fs::read_to_string(&path).unwrap();
    assert!(
      contents.ends_with('\n'),
      "progress file should end with newline: {contents:?}"
    );
    let parsed: PageProgress = serde_json::from_str(&contents).unwrap();
    assert_eq!(parsed.url, progress.url);
    assert_eq!(parsed.status, progress.status);
    assert_eq!(parsed.total_ms, progress.total_ms);
  }

  #[test]
  fn atomic_write_failure_leaves_original_file() {
    let dir = tempdir().unwrap();
    let path = dir.path().join("page.json");

    let original = "{\"url\":\"https://kept.test/\"}\n";
    fs::write(&path, original).unwrap();

    let err = atomic_write_with_hook(&path, b"{\"url\":\"https://new.test/\"}\n", |temp_path| {
      assert!(temp_path.exists());
      Err(io::Error::new(
        io::ErrorKind::Other,
        "fail before rename for test",
      ))
    });
    assert!(err.is_err());

    let contents = fs::read_to_string(&path).unwrap();
    assert_eq!(contents, original);

    let remaining: Vec<_> = fs::read_dir(dir.path())
      .unwrap()
      .filter_map(|e| e.ok())
      .collect();
    assert_eq!(remaining.len(), 1);
  }

  #[test]
  fn buckets_from_diagnostics_excludes_text_timings() {
    let timings = RenderStageTimings {
      html_decode_ms: Some(1.0),
      dom_parse_ms: Some(2.0),
      dom_meta_viewport_ms: Some(0.5),
      dom_clone_ms: Some(0.25),
      dom_top_layer_ms: Some(0.75),
      css_inlining_ms: Some(3.0),
      css_parse_ms: Some(4.0),
      cascade_ms: Some(5.0),
      box_tree_ms: Some(6.0),
      layout_ms: Some(7.0),
      text_fallback_ms: Some(1.0),
      text_shape_ms: Some(8.0),
      paint_build_ms: Some(9.0),
      paint_optimize_ms: Some(10.0),
      paint_rasterize_ms: Some(11.0),
      text_rasterize_ms: Some(12.0),
      encode_ms: Some(13.0),
      ..RenderStageTimings::default()
    };
    let diag = RenderDiagnostics {
      stats: Some(RenderStats {
        timings,
        ..RenderStats::default()
      }),
      ..RenderDiagnostics::default()
    };
    let buckets = buckets_from_diagnostics(&diag);
    assert_eq!(buckets.fetch, 4.5);
    assert_eq!(buckets.css, 7.0);
    assert_eq!(buckets.cascade, 11.0);
    assert_eq!(buckets.layout, 7.0);
    assert_eq!(buckets.paint, 43.0);
  }

  #[test]
  fn merge_cascade_diagnostics_fills_missing_fields_without_overwriting() {
    let mut dst = CascadeDiagnostics {
      nodes: Some(1),
      selector_time_ms: Some(2.0),
      ..CascadeDiagnostics::default()
    };
    let src = CascadeDiagnostics {
      nodes: Some(99),
      rule_candidates: Some(10),
      selector_time_ms: Some(42.0),
      has_evals: Some(7),
      ..CascadeDiagnostics::default()
    };

    merge_cascade_diagnostics(&mut dst, &src);

    assert_eq!(
      dst.nodes,
      Some(1),
      "existing fields should not be overwritten"
    );
    assert_eq!(
      dst.selector_time_ms,
      Some(2.0),
      "existing fields should not be overwritten"
    );
    assert_eq!(
      dst.rule_candidates,
      Some(10),
      "missing fields should be filled"
    );
    assert_eq!(dst.has_evals, Some(7), "missing fields should be filled");
  }

  #[test]
  fn merge_cascade_stats_into_progress_creates_containers() {
    let mut progress = PageProgress::new("https://example.com".to_string());
    assert!(
      progress.diagnostics.is_none(),
      "sanity: starts without diagnostics"
    );

    let rerun_stats = RenderStats {
      cascade: CascadeDiagnostics {
        nodes: Some(3),
        rule_candidates: Some(4),
        ..CascadeDiagnostics::default()
      },
      ..RenderStats::default()
    };

    merge_cascade_stats_into_progress(&mut progress, &rerun_stats);

    let stats = progress
      .diagnostics
      .as_ref()
      .and_then(|d| d.stats.as_ref())
      .expect("merge should create diagnostics.stats");
    assert_eq!(stats.cascade.nodes, Some(3));
    assert_eq!(stats.cascade.rule_candidates, Some(4));
  }

  #[test]
  fn cascade_summary_includes_pseudo_and_has_counters() {
    let cascade = CascadeDiagnostics {
      nodes: Some(1),
      rule_candidates: Some(2),
      selector_time_ms: Some(3.0),
      pseudo_time_ms: Some(4.0),
      has_evals: Some(5),
      has_cache_hits: Some(6),
      has_prunes: Some(7),
      has_evaluated: Some(8),
      ..CascadeDiagnostics::default()
    };

    let summary = cascade_summary(&cascade).expect("summary");
    assert!(summary.contains("pseudo=4.00ms"), "summary was: {summary}");
    assert!(summary.contains("has"), "summary was: {summary}");
    assert!(summary.contains("evals=5"), "summary was: {summary}");
    assert!(summary.contains("hits=6"), "summary was: {summary}");
    assert!(summary.contains("prunes=7"), "summary was: {summary}");
    assert!(summary.contains("evaluated=8"), "summary was: {summary}");
  }

  #[test]
  fn hotspot_inference_prefers_largest_bucket() {
    let buckets = StageBuckets {
      fetch: 5.0,
      css: 1.0,
      cascade: 2.0,
      layout: 3.0,
      paint: 4.0,
    };
    assert_eq!(guess_hotspot(&buckets), "fetch");
  }

  #[test]
  fn selection_uses_intersection_and_hotspot() {
    let progresses = vec![
      make_progress("a", ProgressStatus::Timeout, Some(100.0), "layout"),
      make_progress("b", ProgressStatus::Error, Some(150.0), "paint"),
      make_progress("c", ProgressStatus::Timeout, Some(200.0), "paint"),
    ];
    let selection = ProgressSelection {
      criteria: vec![
        ProgressCriterion::Status(status_set(&[
          ProgressStatus::Timeout,
          ProgressStatus::Panic,
          ProgressStatus::Error,
        ])),
        ProgressCriterion::Hotspot("layout".into()),
      ],
      top_slowest: None,
      union: false,
    };
    let stems: Vec<String> = select_progress_entries(&progresses, &selection)
      .into_iter()
      .map(|p| p.stem.clone())
      .collect();
    assert_eq!(stems, vec!["a".to_string()]);
  }

  #[test]
  fn selection_union_combines_filters() {
    let progresses = vec![
      make_progress("a", ProgressStatus::Ok, Some(50.0), "layout"),
      make_progress("b", ProgressStatus::Timeout, None, "layout"),
      make_progress("c", ProgressStatus::Ok, Some(500.0), "paint"),
    ];
    let selection = ProgressSelection {
      criteria: vec![
        ProgressCriterion::Status(status_set(&[ProgressStatus::Timeout])),
        ProgressCriterion::Slow {
          threshold_ms: 300.0,
          ok_only: true,
        },
      ],
      top_slowest: None,
      union: true,
    };
    let stems: Vec<String> = select_progress_entries(&progresses, &selection)
      .into_iter()
      .map(|p| p.stem.clone())
      .collect();
    assert_eq!(stems, vec!["b".to_string(), "c".to_string()]);
  }

  #[test]
  fn top_slowest_orders_and_truncates() {
    let progresses = vec![
      make_progress("b", ProgressStatus::Ok, Some(300.0), "layout"),
      make_progress("a", ProgressStatus::Ok, Some(300.0), "layout"),
      make_progress("c", ProgressStatus::Ok, Some(200.0), "layout"),
      make_progress("d", ProgressStatus::Ok, None, "layout"),
    ];
    let selection = ProgressSelection {
      criteria: Vec::new(),
      top_slowest: Some(2),
      union: false,
    };
    let stems: Vec<String> = select_progress_entries(&progresses, &selection)
      .into_iter()
      .map(|p| p.stem.clone())
      .collect();
    assert_eq!(stems, vec!["a".to_string(), "b".to_string()]);
  }

  #[test]
  fn tolerant_loader_skips_invalid_files() {
    let dir = tempdir().unwrap();

    let mut fast = PageProgress::new("https://fast.test/".to_string());
    fast.status = ProgressStatus::Ok;
    fast.total_ms = Some(10.0);
    fs::write(
      dir.path().join("a.json"),
      serde_json::to_string_pretty(&fast).unwrap(),
    )
    .unwrap();

    let mut slow = PageProgress::new("https://slow.test/".to_string());
    slow.status = ProgressStatus::Timeout;
    slow.total_ms = Some(250.0);
    fs::write(
      dir.path().join("b.json"),
      serde_json::to_string_pretty(&slow).unwrap(),
    )
    .unwrap();

    fs::write(dir.path().join("bad.json"), "{ not json").unwrap();

    let outcome = read_progress_dir_tolerant(dir.path());
    let stems: Vec<String> = outcome.progresses.iter().map(|p| p.stem.clone()).collect();
    assert_eq!(stems, vec!["a".to_string(), "b".to_string()]);
    assert_eq!(outcome.warnings.len(), 1);
    assert!(
      outcome.warnings[0].contains("bad.json"),
      "warnings: {:?}",
      outcome.warnings
    );
  }

  #[test]
  fn tolerant_loader_reports_missing_directory() {
    let dir = tempdir().unwrap();
    let missing = dir.path().join("nope");
    let outcome = read_progress_dir_tolerant(&missing);
    assert!(outcome.progresses.is_empty());
    assert_eq!(outcome.warnings.len(), 1);
    assert!(
      outcome.warnings[0].contains("nope"),
      "warnings: {:?}",
      outcome.warnings
    );
  }

  #[test]
  fn cached_html_path_for_entry_prefers_exact_cache_stem() {
    let dir = tempdir().unwrap();
    let canonical = dir.path().join("example.com.html");
    let alias = dir.path().join("www.example.com.html");
    let mut cached_paths: BTreeMap<String, PathBuf> = BTreeMap::new();
    cached_paths.insert("www.example.com".to_string(), alias.clone());
    cached_paths.insert("example.com".to_string(), canonical.clone());
    let cached_by_stem = cached_html_index_by_pageset_stem(&cached_paths);

    let entry = PagesetEntry {
      url: "https://example.com".to_string(),
      stem: "example.com".to_string(),
      cache_stem: "example.com".to_string(),
    };

    assert_eq!(
      cached_html_path_for_pageset_entry(&entry, &cached_paths, &cached_by_stem),
      Some(canonical)
    );
  }

  #[test]
  fn cached_html_path_for_entry_falls_back_to_single_alias() {
    let dir = tempdir().unwrap();
    let alias = dir.path().join("www.example.com.html");
    let mut cached_paths: BTreeMap<String, PathBuf> = BTreeMap::new();
    cached_paths.insert("www.example.com".to_string(), alias.clone());
    let cached_by_stem = cached_html_index_by_pageset_stem(&cached_paths);

    let entry = PagesetEntry {
      url: "https://example.com".to_string(),
      stem: "example.com".to_string(),
      cache_stem: "example.com".to_string(),
    };

    assert_eq!(
      cached_html_path_for_pageset_entry(&entry, &cached_paths, &cached_by_stem),
      Some(alias)
    );
  }

  #[test]
  fn cached_html_path_for_entry_falls_back_to_matching_collision_alias() {
    let dir = tempdir().unwrap();
    let alias_a = dir.path().join("www.example.com--aaaaaaaa.html");
    let alias_b = dir.path().join("www.example.com--bbbbbbbb.html");
    let mut cached_paths: BTreeMap<String, PathBuf> = BTreeMap::new();
    cached_paths.insert("www.example.com--aaaaaaaa".to_string(), alias_a);
    cached_paths.insert("www.example.com--bbbbbbbb".to_string(), alias_b.clone());
    let cached_by_stem = cached_html_index_by_pageset_stem(&cached_paths);

    let entry = PagesetEntry {
      url: "http://example.com".to_string(),
      stem: "example.com".to_string(),
      cache_stem: "example.com--bbbbbbbb".to_string(),
    };

    assert_eq!(
      cached_html_path_for_pageset_entry(&entry, &cached_paths, &cached_by_stem),
      Some(alias_b)
    );
  }

  #[test]
  fn resolve_pageset_entry_maps_www_cache_stem() {
    let entries = vec![PagesetEntry {
      url: "https://example.com".to_string(),
      stem: "example.com".to_string(),
      cache_stem: "example.com".to_string(),
    }];
    let pageset_by_cache: HashMap<String, &PagesetEntry> = entries
      .iter()
      .map(|entry| (entry.cache_stem.clone(), entry))
      .collect();
    let mut pageset_by_stem: HashMap<String, Vec<&PagesetEntry>> = HashMap::new();
    for entry in &entries {
      pageset_by_stem
        .entry(entry.stem.clone())
        .or_default()
        .push(entry);
    }

    let resolved =
      resolve_pageset_entry_for_cache_stem("www.example.com", &pageset_by_cache, &pageset_by_stem)
        .expect("entry");

    assert_eq!(resolved.cache_stem, "example.com");
  }

  #[test]
  fn resolve_pageset_entry_maps_www_cache_stem_with_collision_suffix() {
    let entries = vec![
      PagesetEntry {
        url: "https://example.com".to_string(),
        stem: "example.com".to_string(),
        cache_stem: "example.com--aaaaaaaa".to_string(),
      },
      PagesetEntry {
        url: "http://example.com/".to_string(),
        stem: "example.com".to_string(),
        cache_stem: "example.com--bbbbbbbb".to_string(),
      },
    ];
    let pageset_by_cache: HashMap<String, &PagesetEntry> = entries
      .iter()
      .map(|entry| (entry.cache_stem.clone(), entry))
      .collect();
    let mut pageset_by_stem: HashMap<String, Vec<&PagesetEntry>> = HashMap::new();
    for entry in &entries {
      pageset_by_stem
        .entry(entry.stem.clone())
        .or_default()
        .push(entry);
    }

    let resolved = resolve_pageset_entry_for_cache_stem(
      "www.example.com--bbbbbbbb",
      &pageset_by_cache,
      &pageset_by_stem,
    )
    .expect("entry");

    assert_eq!(resolved.cache_stem, "example.com--bbbbbbbb");
  }

  #[test]
  fn shaping_notes_elide_text_and_keep_reason() {
    let long_text = "garbled".repeat(80);
    let note = format!(
      "[layout] Layout failed: MissingContext(\"Shaping failed (fonts=8): \
       Text(ShapingFailed {{ text: \\\"{long_text}\\\", reason: \\\"No suitable font found for \
       cluster ꧟\\\\u{{333}}\\\" }})\")"
    );

    let normalized = normalize_progress_note(&note);

    assert!(normalized.starts_with("[layout]"));
    assert!(!normalized.contains("[paint]"));
    assert!(!normalized.contains("Invalid paint parameters"));
    assert!(normalized.contains("ShapingFailed"));
    assert!(normalized.contains("text: <omitted>"));
    assert!(normalized.contains("No suitable font found for cluster"));
    assert!(!normalized.contains(&long_text));
    assert!(normalized.chars().count() <= PROGRESS_NOTE_MAX_CHARS);
  }

  #[test]
  fn generic_notes_are_capped_and_preserved() {
    let mut note = "generic failure: ".to_string();
    while note.chars().count() <= PROGRESS_NOTE_MAX_CHARS {
      note.push_str("more detail; ");
    }

    let normalized = normalize_progress_note(&note);
    let expected_prefix: String = note
      .chars()
      .take(PROGRESS_NOTE_MAX_CHARS.saturating_sub(1))
      .collect();

    assert_eq!(normalized.chars().count(), PROGRESS_NOTE_MAX_CHARS);
    assert!(normalized.starts_with(&expected_prefix));
    assert!(normalized.ends_with(PROGRESS_NOTE_ELLIPSIS));
  }

  #[test]
  fn merge_keeps_previous_hotspot_when_new_is_unknown() {
    let previous = PageProgress {
      url: "https://example.com".to_string(),
      hotspot: "layout".to_string(),
      ..PageProgress::default()
    };
    let new_progress = PageProgress {
      url: previous.url.clone(),
      hotspot: "unknown".to_string(),
      ..PageProgress::default()
    };
    let merged = new_progress.merge_preserving_manual(Some(previous), None);
    assert_eq!(merged.hotspot, "layout");
  }

  #[test]
  fn resource_errors_map_to_fetch_hotspot() {
    let err = fastrender::Error::Resource(fastrender::error::ResourceError::new(
      "https://example.com/style.css",
      "fetch failed",
    ));
    assert_eq!(hotspot_from_error(&err, None), Some("fetch"));
  }

  #[test]
  fn inferred_hotspot_does_not_override_manual_hotspot() {
    let err = fastrender::Error::Resource(fastrender::error::ResourceError::new(
      "https://example.com/style.css",
      "fetch failed",
    ));
    let previous = PageProgress {
      url: "https://example.com".to_string(),
      hotspot: "layout".to_string(),
      ..PageProgress::default()
    };
    let mut progress = PageProgress::new(previous.url.clone());
    progress.status = ProgressStatus::Error;
    if let Some(hotspot) = hotspot_from_error(&err, None) {
      maybe_apply_hotspot(&mut progress, Some(&previous), hotspot, false);
    }
    let merged = progress.merge_preserving_manual(Some(previous), None);
    assert_eq!(merged.hotspot, "layout");
  }

  #[test]
  fn inferred_hotspot_overrides_previous_ok_hotspot() {
    let err = fastrender::Error::Resource(fastrender::error::ResourceError::new(
      "https://example.com/style.css",
      "fetch failed",
    ));
    let previous = PageProgress {
      url: "https://example.com".to_string(),
      status: ProgressStatus::Ok,
      hotspot: "layout".to_string(),
      ..PageProgress::default()
    };
    let mut progress = PageProgress::new(previous.url.clone());
    progress.status = ProgressStatus::Error;
    if let Some(hotspot) = hotspot_from_error(&err, None) {
      maybe_apply_hotspot(&mut progress, Some(&previous), hotspot, false);
    }
    let merged = progress.merge_preserving_manual(Some(previous), None);
    assert_eq!(merged.hotspot, "fetch");
  }

  #[test]
  fn timeout_cascade_maps_to_cascade_hotspot() {
    assert_eq!(hotspot_from_timeout_stage(RenderStage::Cascade), "cascade");
  }

  #[test]
  fn merge_preserves_manual_commits_when_new_is_empty() {
    let previous = PageProgress {
      url: "https://example.com".to_string(),
      last_good_commit: "abc1234".to_string(),
      last_regression_commit: "def5678".to_string(),
      ..PageProgress::default()
    };
    let merged = PageProgress::default().merge_preserving_manual(Some(previous), None);
    assert_eq!(merged.last_good_commit, "abc1234");
    assert_eq!(merged.last_regression_commit, "def5678");
  }

  #[test]
  fn rerun_preserves_manual_notes_but_rewrites_auto_notes() {
    let url = "https://example.com".to_string();
    let previous = PageProgress {
      url: url.clone(),
      status: ProgressStatus::Timeout,
      // Simulate a legacy progress file where auto diagnostics accumulated inside `notes`.
      notes: "manual blocker\nhard timeout after 5.00s\nstage: layout".to_string(),
      ..PageProgress::default()
    };

    let new_progress = PageProgress {
      url: url.clone(),
      status: ProgressStatus::Timeout,
      auto_notes: "hard timeout after 5.00s\nstage: paint".to_string(),
      ..PageProgress::default()
    };

    let merged = new_progress.merge_preserving_manual(Some(previous), None);
    assert_eq!(merged.notes, "manual blocker");
    assert_eq!(merged.auto_notes, "hard timeout after 5.00s\nstage: paint");
  }

  #[test]
  fn legacy_error_lines_are_not_treated_as_manual_notes() {
    let url = "https://example.com".to_string();
    let previous = PageProgress {
      url,
      status: ProgressStatus::Timeout,
      notes: "[paint] Invalid paint parameters: Layout failed\nmanual blocker\nhard timeout after 5.00s\nstage: layout".to_string(),
      ..PageProgress::default()
    };

    assert_eq!(
      manual_notes_from_previous(&previous),
      Some("manual blocker".to_string())
    );
    assert_eq!(
      legacy_auto_notes_from_previous(&previous),
      Some(
        "[paint] Invalid paint parameters: Layout failed\nhard timeout after 5.00s\nstage: layout"
          .to_string()
      )
    );
  }

  #[test]
  fn legacy_fetch_failed_lines_are_not_treated_as_manual_notes() {
    let url = "https://example.com".to_string();
    let previous = PageProgress {
      url,
      status: ProgressStatus::Error,
      notes: "fetch failed: [resource] https://example.com: timeout: global\nmanual blocker"
        .to_string(),
      ..PageProgress::default()
    };

    assert_eq!(
      manual_notes_from_previous(&previous),
      Some("manual blocker".to_string())
    );
    assert_eq!(
      legacy_auto_notes_from_previous(&previous),
      Some("fetch failed: [resource] https://example.com: timeout: global".to_string())
    );
  }

  #[test]
  fn truncated_stage_lines_are_ignored_in_manual_notes() {
    let url = "https://example.com".to_string();
    let previous = PageProgress {
      url,
      status: ProgressStatus::Timeout,
      notes: "manual blocker\nstage…\ns…".to_string(),
      ..PageProgress::default()
    };

    assert_eq!(
      manual_notes_from_previous(&previous),
      Some("manual blocker".to_string())
    );
    assert_eq!(
      legacy_auto_notes_from_previous(&previous),
      Some("stage…\ns…".to_string())
    );
  }

  #[test]
  fn migrate_legacy_notes_splits_manual_and_auto() {
    let mut progress = PageProgress {
      url: "https://example.com".to_string(),
      status: ProgressStatus::Timeout,
      notes:
        "[paint] Invalid paint parameters\nmanual blocker\nhard timeout after 5.00s\nstage: layout"
          .to_string(),
      ..PageProgress::default()
    };

    migrate_legacy_notes(&mut progress);

    assert_eq!(progress.notes, "manual blocker");
    assert_eq!(
      progress.auto_notes,
      "[paint] Invalid paint parameters\nhard timeout after 5.00s\nstage: layout"
    );
  }

  #[test]
  fn normalize_missing_cache_placeholder_becomes_not_run_when_cache_exists() {
    let mut progress = PageProgress {
      url: "https://example.com".to_string(),
      status: ProgressStatus::Error,
      notes: String::new(),
      auto_notes: "missing cache".to_string(),
      total_ms: None,
      ..PageProgress::default()
    };

    normalize_missing_cache_placeholder(&mut progress, true);

    assert_eq!(progress.auto_notes, "not run");
  }

  #[test]
  fn merge_alias_manual_fields_copies_unset_fields() {
    let mut progress = PageProgress {
      url: "https://example.com".to_string(),
      status: ProgressStatus::Timeout,
      notes: String::new(),
      hotspot: String::new(),
      last_good_commit: String::new(),
      last_regression_commit: String::new(),
      ..PageProgress::default()
    };

    let alias = PageProgress {
      url: "https://www.example.com".to_string(),
      status: ProgressStatus::Timeout,
      notes: "manual blocker".to_string(),
      hotspot: "layout".to_string(),
      last_good_commit: "abc1234".to_string(),
      last_regression_commit: "def5678".to_string(),
      ..PageProgress::default()
    };

    merge_alias_manual_fields(&mut progress, &alias);
    assert_eq!(progress.notes, "manual blocker");
    assert_eq!(progress.hotspot, "layout");
    assert_eq!(progress.last_good_commit, "abc1234");
    assert_eq!(progress.last_regression_commit, "def5678");
  }

  #[test]
  fn merge_alias_manual_fields_does_not_override_existing() {
    let mut progress = PageProgress {
      url: "https://example.com".to_string(),
      status: ProgressStatus::Timeout,
      notes: "keep".to_string(),
      hotspot: "paint".to_string(),
      last_good_commit: "keepgood".to_string(),
      last_regression_commit: "keepregress".to_string(),
      ..PageProgress::default()
    };

    let alias = PageProgress {
      url: "https://www.example.com".to_string(),
      status: ProgressStatus::Timeout,
      notes: "override".to_string(),
      hotspot: "layout".to_string(),
      last_good_commit: "overridegood".to_string(),
      last_regression_commit: "overrideregress".to_string(),
      ..PageProgress::default()
    };

    merge_alias_manual_fields(&mut progress, &alias);
    assert_eq!(progress.notes, "keep");
    assert_eq!(progress.hotspot, "paint");
    assert_eq!(progress.last_good_commit, "keepgood");
    assert_eq!(progress.last_regression_commit, "keepregress");
  }

  #[test]
  fn collect_alias_progress_resolves_www_and_migrates_legacy_notes() {
    let dir = tempdir().unwrap();
    let alias_path = dir.path().join("www.example.com.json");
    let alias_progress = PageProgress {
      url: "https://www.example.com".to_string(),
      status: ProgressStatus::Timeout,
      notes: "manual blocker\nhard timeout after 5.00s\nstage: layout".to_string(),
      ..PageProgress::default()
    };
    fs::write(
      &alias_path,
      format!(
        "{}\n",
        serde_json::to_string_pretty(&alias_progress).expect("progress json")
      ),
    )
    .unwrap();

    let pageset_entries = vec![PagesetEntry {
      url: "https://example.com".to_string(),
      stem: "example.com".to_string(),
      cache_stem: "example.com".to_string(),
    }];
    let pageset_by_cache: std::collections::HashMap<String, &PagesetEntry> = pageset_entries
      .iter()
      .map(|entry| (entry.cache_stem.clone(), entry))
      .collect();
    let mut pageset_by_stem: std::collections::HashMap<String, Vec<&PagesetEntry>> =
      std::collections::HashMap::new();
    for entry in &pageset_entries {
      pageset_by_stem
        .entry(entry.stem.clone())
        .or_default()
        .push(entry);
    }

    let alias_map = collect_alias_progress(dir.path(), &pageset_by_cache, &pageset_by_stem);
    let migrated = alias_map.get("example.com").expect("alias progress");
    assert_eq!(migrated.notes, "manual blocker");
    assert_eq!(
      migrated.auto_notes,
      "hard timeout after 5.00s\nstage: layout"
    );
  }

  #[test]
  fn reruns_do_not_accumulate_stage_lines_in_auto_notes() {
    let url = "https://example.com".to_string();
    let previous = PageProgress {
      url: url.clone(),
      status: ProgressStatus::Timeout,
      auto_notes: "hard timeout after 5.00s\nstage: layout".to_string(),
      ..PageProgress::default()
    };

    let run2 = PageProgress {
      url,
      status: ProgressStatus::Timeout,
      auto_notes: "hard timeout after 5.00s\nstage: paint".to_string(),
      ..PageProgress::default()
    }
    .merge_preserving_manual(Some(previous), None);

    let stage_lines = run2
      .auto_notes
      .lines()
      .filter(|line| line.trim_start().starts_with("stage:"))
      .count();
    assert_eq!(stage_lines, 1, "auto_notes={}", run2.auto_notes);
    assert!(run2.auto_notes.contains("stage: paint"));
    assert!(!run2.auto_notes.contains("stage: layout"));
  }

  #[test]
  fn ok_pages_clear_manual_notes() {
    let url = "https://example.com".to_string();
    let previous = PageProgress {
      url: url.clone(),
      status: ProgressStatus::Timeout,
      notes: "manual blocker".to_string(),
      auto_notes: "hard timeout after 5.00s\nstage: layout".to_string(),
      ..PageProgress::default()
    };

    let new_progress = PageProgress {
      url,
      status: ProgressStatus::Ok,
      // If the runner forgets to clear this, we'd reintroduce stale diagnostics.
      auto_notes: "stale".to_string(),
      ..PageProgress::default()
    };

    let merged = new_progress.merge_preserving_manual(Some(previous), None);
    assert!(merged.notes.trim().is_empty());
    assert!(merged.auto_notes.trim().is_empty());
  }

  #[test]
  fn synthesized_note_includes_exit_code() {
    let summary = ExitStatusSummary {
      code: Some(101),
      signal: None,
    };
    assert_eq!(
      synthesize_missing_progress_note(summary),
      "worker exited (exit code 101) without writing progress"
    );
  }

  #[cfg(unix)]
  #[test]
  fn synthesized_note_includes_signal() {
    let summary = ExitStatusSummary {
      code: None,
      signal: Some(9),
    };
    assert_eq!(
      synthesize_missing_progress_note(summary),
      "worker exited (exit signal 9) without writing progress"
    );
  }

  #[test]
  fn ensure_auto_note_includes_does_not_clobber_manual_notes() {
    let mut progress = PageProgress::new("https://example.com".to_string());
    progress.notes = "manual note".to_string();
    let note = "worker exited (exit code 1) without writing progress";
    ensure_auto_note_includes(&mut progress, note);
    assert_eq!(progress.notes, "manual note");
    assert!(progress.auto_notes.contains(note));
  }

  #[test]
  fn ensure_auto_note_includes_is_idempotent() {
    let mut progress = PageProgress::new("https://example.com".to_string());
    let note = "worker exited (exit code 1) without writing progress";
    ensure_auto_note_includes(&mut progress, note);
    ensure_auto_note_includes(&mut progress, note);
    assert_eq!(progress.auto_notes, note);
  }

  #[test]
  fn soft_timeout_defaults_leave_buffer_before_hard_kill() {
    let hard = Duration::from_secs(5);
    assert_eq!(compute_soft_timeout_ms(hard, None), Some(4750));
  }

  #[test]
  fn soft_timeout_default_disables_when_hard_timeout_short() {
    let hard = Duration::from_millis(200);
    assert_eq!(compute_soft_timeout_ms(hard, None), None);
  }

  #[test]
  fn soft_timeout_prefers_explicit_override() {
    let hard = Duration::from_secs(5);
    assert_eq!(compute_soft_timeout_ms(hard, Some(1234)), Some(1234));
    assert_eq!(compute_soft_timeout_ms(hard, Some(0)), Some(0));
  }

  #[test]
  fn trace_timeout_defaults_to_double() {
    let hard = compute_trace_hard_timeout(5, None);
    assert_eq!(hard, Duration::from_secs(10));
  }

  #[test]
  fn trace_timeout_respects_override() {
    let hard = compute_trace_hard_timeout(5, Some(7));
    assert_eq!(hard, Duration::from_secs(7));
  }

  #[test]
  fn fetch_timeout_uses_soft_timeout_with_slack_and_cap() {
    let hard = Duration::from_secs(5);
    assert_eq!(
      compute_fetch_timeout(hard, Some(1000)),
      Duration::from_millis(1100)
    );
    assert_eq!(compute_fetch_timeout(hard, Some(4950)), hard);
  }

  #[test]
  fn fetch_timeout_falls_back_when_soft_timeout_disabled_or_missing() {
    let hard = Duration::from_secs(2);
    assert_eq!(compute_fetch_timeout(hard, None), hard);
    assert_eq!(compute_fetch_timeout(hard, Some(0)), hard);
  }

  #[test]
  fn heartbeat_stage_maps_to_hotspot() {
    assert_eq!(StageHeartbeat::ReadCache.hotspot(), "fetch");
    assert_eq!(StageHeartbeat::FollowRedirects.hotspot(), "fetch");
    assert_eq!(StageHeartbeat::DomParse.hotspot(), "fetch");
    assert_eq!(StageHeartbeat::CssInline.hotspot(), "css");
    assert_eq!(StageHeartbeat::CssParse.hotspot(), "css");
    assert_eq!(StageHeartbeat::Cascade.hotspot(), "cascade");
    assert_eq!(StageHeartbeat::BoxTree.hotspot(), "cascade");
    assert_eq!(StageHeartbeat::Layout.hotspot(), "layout");
    assert_eq!(StageHeartbeat::PaintBuild.hotspot(), "paint");
    assert_eq!(StageHeartbeat::PaintRasterize.hotspot(), "paint");
  }

  #[test]
  fn heartbeat_stage_roundtrips_new_markers() {
    for (raw, stage) in [
      ("css_parse", StageHeartbeat::CssParse),
      ("box_tree", StageHeartbeat::BoxTree),
    ] {
      assert_eq!(stage.as_str(), raw);
      assert_eq!(StageHeartbeat::from_str(raw), Some(stage));
      assert_eq!(StageHeartbeat::from_str(stage.as_str()), Some(stage));
    }
  }

  #[test]
  fn heartbeat_stage_maps_to_progress_stage() {
    assert_eq!(
      progress_stage_from_heartbeat(StageHeartbeat::CssParse),
      Some(ProgressStage::Css)
    );
    assert_eq!(
      progress_stage_from_heartbeat(StageHeartbeat::BoxTree),
      Some(ProgressStage::Cascade)
    );
  }

  #[test]
  fn cooperative_timeout_includes_box_tree_stage_note() {
    let mut progress = PageProgress::new("https://timeout.test/".to_string());

    populate_timeout_progress_with_heartbeat(
      &mut progress,
      RenderStage::Cascade,
      Duration::from_millis(1500),
      Some(StageHeartbeat::BoxTree),
    );

    assert_eq!(progress.status, ProgressStatus::Timeout);
    assert_eq!(progress.timeout_stage, Some(ProgressStage::Cascade));
    assert!(
      progress.auto_notes.contains("stage: box_tree"),
      "expected box_tree stage note in auto_notes, got: {}",
      progress.auto_notes
    );
  }

  #[test]
  fn hard_timeout_prefers_heartbeat_stage() {
    let _lock = env_lock();
    let Some(exe) = pageset_progress_exe() else {
      eprintln!("Skipping test: pageset_progress binary not found");
      return;
    };

    let dir = tempdir().unwrap();
    let cache_path = dir.path().join("page.html");
    fs::write(&cache_path, "<html><body>slow</body></html>").unwrap();
    let progress_path = dir.path().join("page.json");
    let log_path = dir.path().join("page.log");
    let stage_path = dir.path().join("page.stage");
    let stderr_path = dir.path().join("page.stderr.log");

    let item = WorkItem {
      stem: "heartbeat".to_string(),
      cache_stem: "heartbeat".to_string(),
      url: url_hint_from_cache_path(&cache_path),
      cache_path: cache_path.clone(),
      progress_path: progress_path.clone(),
      log_path: log_path.clone(),
      stderr_path: stderr_path.clone(),
      stage_path: stage_path.clone(),
      trace_out: None,
    };

    let args = basic_run_args(dir.path());
    let hard_timeout = Duration::from_millis(200);
    let _guard = EnvVarGuard::set("FASTR_TEST_RENDER_DELAY_MS", "500");
    let _stem_guard = EnvVarGuard::set("FASTR_TEST_RENDER_DELAY_STEM", "heartbeat");
    let queue = VecDeque::from(vec![item]);
    run_queue(
      &exe,
      &args,
      queue,
      hard_timeout,
      hard_timeout,
      None,
      args.diagnostics,
      false,
      args.jobs,
      None,
    )
    .unwrap();

    let progress = read_progress(&progress_path).expect("progress");
    assert_eq!(progress.status, ProgressStatus::Timeout);
    let heartbeat_stage = read_stage_heartbeat(&stage_path).expect("heartbeat");
    assert_eq!(progress.hotspot, heartbeat_stage.hotspot().to_string());
    assert!(
      progress.auto_notes.contains(heartbeat_stage.as_str()),
      "auto_notes missing stage: {}",
      progress.auto_notes
    );
  }

  #[test]
  fn kill_after_progress_during_dump_capture_does_not_overwrite_progress() {
    let _lock = env_lock();
    let Some(exe) = pageset_progress_exe() else {
      eprintln!("Skipping test: pageset_progress binary not found");
      return;
    };

    let dir = tempdir().unwrap();
    let cache_path = dir.path().join("page.html");
    fs::write(&cache_path, "<html><body>ok</body></html>").unwrap();
    let progress_path = dir.path().join("page.json");
    let log_path = dir.path().join("page.log");
    let stage_path = dir.path().join("page.stage");
    let stderr_path = dir.path().join("page.stderr.log");

    let item = WorkItem {
      stem: "dumpkill".to_string(),
      cache_stem: "dumpkill".to_string(),
      url: url_hint_from_cache_path(&cache_path),
      cache_path: cache_path.clone(),
      progress_path: progress_path.clone(),
      log_path: log_path.clone(),
      stderr_path: stderr_path.clone(),
      stage_path: stage_path.clone(),
      trace_out: None,
    };

    let mut args = basic_run_args(dir.path());
    // Avoid scanning system fonts in tests.
    args.fonts.bundled_fonts = true;

    let dump_settings = DumpSettings {
      failures: None,
      slow: Some(DumpLevel::Summary),
      slow_ms: Some(0.0),
      dir: dir.path().join("dumps"),
      timeout_secs: 1,
      soft_timeout_ms: None,
    };
    fs::create_dir_all(&dump_settings.dir).unwrap();

    let render_kill_timeout = Duration::from_secs(2);
    let overall_kill_timeout = Duration::from_secs(3);
    let _guard = EnvVarGuard::set("FASTR_TEST_DUMP_DELAY_MS", "10000");
    let _stem_guard = EnvVarGuard::set("FASTR_TEST_DUMP_DELAY_STEM", "dumpkill");
    let queue = VecDeque::from(vec![item]);
    run_queue(
      &exe,
      &args,
      queue,
      render_kill_timeout,
      overall_kill_timeout,
      None,
      args.diagnostics,
      false,
      args.jobs,
      Some(&dump_settings),
    )
    .unwrap();

    let stderr_contents = fs::read_to_string(&stderr_path).expect("stderr log");
    assert!(
      stderr_contents.contains("parent killed worker during dump capture"),
      "expected parent to kill during dump capture, stderr log was:\n{stderr_contents}"
    );

    let progress = read_progress(&progress_path).expect("progress");
    assert_ne!(
      progress.status,
      ProgressStatus::Timeout,
      "progress was overwritten after worker was killed post-progress"
    );
  }

  #[test]
  fn dump_capture_panic_does_not_overwrite_progress() {
    let _lock = env_lock();
    let Some(exe) = pageset_progress_exe() else {
      eprintln!("Skipping test: pageset_progress binary not found");
      return;
    };

    let dir = tempdir().unwrap();
    let cache_path = dir.path().join("page.html");
    fs::write(&cache_path, "<html><body>ok</body></html>").unwrap();
    let progress_path = dir.path().join("page.json");
    let log_path = dir.path().join("page.log");
    let stage_path = dir.path().join("page.stage");
    let stderr_path = dir.path().join("page.stderr.log");

    let item = WorkItem {
      stem: "dumppanic".to_string(),
      cache_stem: "dumppanic".to_string(),
      url: url_hint_from_cache_path(&cache_path),
      cache_path: cache_path.clone(),
      progress_path: progress_path.clone(),
      log_path: log_path.clone(),
      stderr_path: stderr_path.clone(),
      stage_path: stage_path.clone(),
      trace_out: None,
    };

    let mut args = basic_run_args(dir.path());
    // Avoid scanning system fonts in tests.
    args.fonts.bundled_fonts = true;

    let dump_settings = DumpSettings {
      failures: None,
      slow: Some(DumpLevel::Summary),
      slow_ms: Some(0.0),
      dir: dir.path().join("dumps"),
      timeout_secs: 1,
      soft_timeout_ms: None,
    };
    fs::create_dir_all(&dump_settings.dir).unwrap();

    let render_kill_timeout = Duration::from_secs(5);
    let overall_kill_timeout = Duration::from_secs(6);
    let _guard = EnvVarGuard::set("FASTR_TEST_DUMP_PANIC", "1");
    let _stem_guard = EnvVarGuard::set("FASTR_TEST_DUMP_PANIC_STEM", "dumppanic");
    let queue = VecDeque::from(vec![item]);
    run_queue(
      &exe,
      &args,
      queue,
      render_kill_timeout,
      overall_kill_timeout,
      None,
      args.diagnostics,
      false,
      args.jobs,
      Some(&dump_settings),
    )
    .unwrap();

    let progress = read_progress(&progress_path).expect("progress");
    assert_ne!(
      progress.status,
      ProgressStatus::Panic,
      "progress was overwritten after dump capture panic"
    );

    let log_contents = fs::read_to_string(&log_path).expect("page log");
    assert!(
      log_contents.contains("Panic after progress was written"),
      "expected panic to be recorded in the per-page log, got:\n{log_contents}"
    );
  }
}

fn main() -> io::Result<()> {
  let cli = Cli::parse();
  match cli.command {
    CommandKind::Sync(args) => sync(args),
    CommandKind::Migrate(args) => migrate(args),
    CommandKind::Run(args) => run(args),
    CommandKind::Report(args) => report(args),
    CommandKind::Worker(args) => worker(args),
  }
}
