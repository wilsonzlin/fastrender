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
  parse_shard, CompatArgs, CompatProfileArg, DomCompatArg, LayoutParallelArgs, ResourceAccessArgs,
};
use common::render_pipeline::{
  build_render_configs, follow_client_redirects, format_error_with_chain, read_cached_document,
  render_document, RenderConfigBundle, RenderSurface,
};
use fastrender::api::{
  CascadeDiagnostics, DiagnosticsLevel, LayoutDiagnostics, PaintDiagnostics, RenderCounts,
  RenderDiagnostics, RenderStats, ResourceDiagnostics, ResourceKind,
};
use fastrender::error::{RenderError, RenderStage};
use fastrender::pageset::{
  pageset_entries, pageset_stem, PagesetEntry, PagesetFilter, CACHE_HTML_DIR,
};
use fastrender::resource::normalize_user_agent_for_log;
use fastrender::resource::parse_cached_html_meta;
#[cfg(not(feature = "disk_cache"))]
use fastrender::resource::CachingFetcher;
use fastrender::resource::CachingFetcherConfig;
#[cfg(feature = "disk_cache")]
use fastrender::resource::DiskCacheConfig;
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
use std::sync::OnceLock;
use std::time::{Duration, Instant};
use tempfile::NamedTempFile;

#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;

const ASSET_DIR: &str = "fetches/assets";
const DEFAULT_PROGRESS_DIR: &str = "progress/pages";
const DEFAULT_LOG_DIR: &str = "target/pageset/logs";
const DEFAULT_TRACE_DIR: &str = "target/pageset/traces";
const DEFAULT_TRACE_PROGRESS_DIR: &str = "target/pageset/trace-progress";
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
    let Some(prev) = previous else {
      return self;
    };
    // Treat `notes` as the durable human field:
    // - keep previous notes for failures (sticky until fixed),
    // - clear notes when the page is OK.
    if self.status == ProgressStatus::Ok {
      self.notes = String::new();
    } else if !prev.notes.trim().is_empty() {
      self.notes = prev.notes;
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
  let mut meta_path = cache_path.to_path_buf();
  if let Some(ext) = meta_path.extension().and_then(|e| e.to_str()) {
    meta_path.set_extension(format!("{ext}.meta"));
  } else {
    meta_path.set_extension("meta");
  }
  let meta = fs::read_to_string(&meta_path).ok();
  let parsed_meta = meta
    .as_deref()
    .map(parse_cached_html_meta)
    .unwrap_or_default();
  parsed_meta
    .url
    .unwrap_or_else(|| format!("file://{}", cache_path.display()))
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

fn write_progress(path: &Path, progress: &PageProgress) -> io::Result<()> {
  let mut normalized = progress.clone();
  normalized.notes = normalize_progress_note(&normalized.notes);
  let json = serde_json::to_string_pretty(&normalized)
    .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
  let formatted = format!("{json}\n");
  atomic_write(path, formatted.as_bytes())
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

fn sync(args: SyncArgs) -> io::Result<()> {
  fs::create_dir_all(&args.progress_dir)?;

  let mut stems: BTreeSet<String> = BTreeSet::new();
  let mut created = 0usize;
  let mut updated = 0usize;

  for entry in pageset_entries() {
    let progress_path = args.progress_dir.join(format!("{}.json", entry.cache_stem));
    let cache_path = args.html_dir.join(format!("{}.html", entry.cache_stem));
    let cache_exists = cache_path.exists();

    let previous = read_progress(&progress_path);
    let mut progress = if let Some(ref prev) = previous {
      prev.clone()
    } else {
      created += 1;
      let mut new_progress = PageProgress::new(entry.url.clone());
      new_progress.notes = "not run".to_string();
      new_progress
    };

    progress.url = entry.url.clone();

    if !cache_exists {
      progress.status = ProgressStatus::Error;
      if is_hotspot_unset(&progress.hotspot) {
        progress.hotspot = "fetch".to_string();
      }
      if progress.notes.trim().is_empty() || previous.is_none() {
        progress.notes = "missing cache".to_string();
      }
      progress.total_ms = None;
      progress.stages_ms = StageBuckets::default();
      progress.failure_stage = None;
      progress.timeout_stage = None;
      progress.diagnostics = None;
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

/// Collapse detailed render diagnostics timings into coarse stage buckets used by progress JSON.
///
/// - Text shaping is grouped under `layout` because glyph positioning is part of line building.
/// - Text rasterization and final encode time are grouped under `paint` to capture rendering/output costs.
fn buckets_from_diagnostics(diag: &RenderDiagnostics) -> StageBuckets {
  let Some(stats) = diag.stats.as_ref() else {
    return StageBuckets::default();
  };
  let t = &stats.timings;
  let fetch = t.html_decode_ms.unwrap_or(0.0) + t.dom_parse_ms.unwrap_or(0.0);
  let css = t.css_inlining_ms.unwrap_or(0.0) + t.css_parse_ms.unwrap_or(0.0);
  let cascade = t.cascade_ms.unwrap_or(0.0) + t.box_tree_ms.unwrap_or(0.0);
  let layout = t.layout_ms.unwrap_or(0.0) + t.text_shape_ms.unwrap_or(0.0);
  let paint = t.paint_build_ms.unwrap_or(0.0)
    + t.paint_optimize_ms.unwrap_or(0.0)
    + t.paint_rasterize_ms.unwrap_or(0.0)
    + t.text_rasterize_ms.unwrap_or(0.0)
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
  progress.notes = format!("timeout at {stage} after {elapsed:?}");
  progress.hotspot = hotspot_from_timeout_stage(stage).to_string();
  progress.timeout_stage = Some(stage.into());
}

fn render_worker(args: WorkerArgs) -> io::Result<()> {
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
      progress.notes = format!("read: {note_msg}");
      progress.hotspot = "fetch".to_string();
      let progress = progress.merge_preserving_manual(progress_before, current_sha.as_deref());
      let _ = write_progress(&args.progress_path, &progress);
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

  let http = HttpFetcher::new()
    .with_timeout(fetch_timeout)
    .with_user_agent(args.user_agent.clone())
    .with_accept_language(args.accept_language.clone());
  let honor_http_freshness = cfg!(feature = "disk_cache") && !args.no_http_freshness;
  let memory_config = CachingFetcherConfig {
    honor_http_cache_freshness: honor_http_freshness,
    ..CachingFetcherConfig::default()
  };
  #[cfg(feature = "disk_cache")]
  let fetcher: std::sync::Arc<dyn ResourceFetcher> = std::sync::Arc::new(
    DiskCachingFetcher::with_configs(http, ASSET_DIR, memory_config, DiskCacheConfig::default()),
  );
  #[cfg(not(feature = "disk_cache"))]
  let fetcher: std::sync::Arc<dyn ResourceFetcher> =
    std::sync::Arc::new(CachingFetcher::with_config(http, memory_config));
  let renderer_fetcher = std::sync::Arc::clone(&fetcher);

  let RenderConfigBundle {
    config,
    mut options,
  } = build_render_configs(&RenderSurface {
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
  });

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
        progress.notes = format!("renderer init: {note_msg}");
        progress.hotspot = "unknown".to_string();
        let progress = progress.merge_preserving_manual(progress_before, current_sha.as_deref());
        let _ = write_progress(&args.progress_path, &progress);
        if let Some(path) = &args.log_path {
          let _ = write_text_file(path, &log);
        }
        return Ok(());
      }
    };

  let mut doc = cached.document;
  log.push_str(&format!("Resource base: {}\n", doc.base_url));
  doc = follow_client_redirects(fetcher.as_ref(), doc, |line| {
    log.push_str(line);
    log.push('\n');
  });
  log.push_str(&format!("Final resource base: {}\n", doc.base_url));

  // Keep parity with render_pages: fit canvas when @page is detected.
  let has_page_rule = doc.html.to_ascii_lowercase().contains("@page");
  if has_page_rule {
    options.fit_canvas_to_content = Some(true);
    log.push_str("Detected @page rule; fitting canvas to content\n");
  }

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
    }
    Ok(Err(err)) => {
      match &err {
        fastrender::Error::Render(RenderError::Timeout { stage, elapsed }) => {
          populate_timeout_progress(&mut progress, *stage, *elapsed);
        }
        _ => {
          progress.status = ProgressStatus::Error;
          progress.notes = format_error_with_chain(&err, args.verbose);
          progress.hotspot = "unknown".to_string();
        }
      }

      log.push_str(&format!("Status: {:?}\n", progress.status));
      log.push_str(&format!("Error: {}\n", format_error_with_chain(&err, true)));
    }
    Err(panic) => {
      progress.status = ProgressStatus::Panic;
      progress.notes = panic_to_string(panic);
      progress.hotspot = "unknown".to_string();
      log.push_str("Status: PANIC\n");
      log.push_str(&format!("Panic: {}\n", progress.notes));
    }
  }

  if progress.notes.trim().is_empty() {
    // Preserve manual notes if present; otherwise fill something durable.
    progress.notes = match progress.status {
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
  let _ = write_progress(&args.progress_path, &progress);

  if let Some(path) = &args.log_path {
    let _ = write_text_file(path, &log);
  }

  Ok(())
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
  log.push_str(&format!("  css_inlining: {}\n", fmt(t.css_inlining_ms)));
  log.push_str(&format!("  css_parse: {}\n", fmt(t.css_parse_ms)));
  log.push_str(&format!("  cascade: {}\n", fmt(t.cascade_ms)));
  log.push_str(&format!("  box_tree: {}\n", fmt(t.box_tree_ms)));
  log.push_str(&format!("  layout: {}\n", fmt(t.layout_ms)));
  log.push_str(&format!(
    "  text_shape: {}\n",
    fmt(t.text_shape_ms)
  ));
  log.push_str(&format!("  paint_build: {}\n", fmt(t.paint_build_ms)));
  log.push_str(&format!("  paint_optimize: {}\n", fmt(t.paint_optimize_ms)));
  log.push_str(&format!(
    "  paint_rasterize: {}\n",
    fmt(t.paint_rasterize_ms)
  ));
  log.push_str(&format!(
    "  text_rasterize: {}\n",
    fmt(t.text_rasterize_ms)
  ));
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

fn ensure_note_includes(progress: &mut PageProgress, note: &str) {
  if progress.notes.contains(note) {
    return;
  }
  if progress.notes.trim().is_empty() {
    progress.notes = note.to_string();
  } else {
    progress.notes = format!("{}\n{note}", progress.notes);
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

fn cascade_summary(cascade: &CascadeDiagnostics) -> Option<String> {
  let mut parts = Vec::new();
  push_opt_u64(&mut parts, "nodes", cascade.nodes);
  push_opt_u64(&mut parts, "candidates", cascade.rule_candidates);
  push_opt_u64(&mut parts, "pruned", cascade.rule_candidates_pruned);
  push_opt_u64(&mut parts, "matches", cascade.rule_matches);
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

fn report(args: ReportArgs) -> io::Result<()> {
  if args.fail_on_regression && args.compare.is_none() {
    eprintln!("--fail-on-regression requires --compare");
    std::process::exit(2);
  }
  if args.regression_threshold_percent < 0.0 {
    eprintln!("regression threshold must be >= 0");
    std::process::exit(2);
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

    if args.fail_on_regression {
      let mut failed_stems: HashSet<String> = HashSet::new();
      let mut reasons = Vec::new();
      for (stem, from, to) in &comparison.ok_to_bad {
        reasons.push(format!(
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
            reasons.push(format!(
              "{}: Δtotal={:+.2}ms ({:+.2}%)",
              delta.stem, delta.delta_ms, percent
            ));
            failed_stems.insert(delta.stem.clone());
          }
        }
      }
      if !reasons.is_empty() {
        eprintln!("Failing due to regressions vs {}:", compare_dir.display());
        for reason in reasons {
          eprintln!("  {reason}");
        }
        std::process::exit(1);
      }
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
  trace_out: Option<PathBuf>,
}

fn collect_cached_html_paths() -> io::Result<BTreeMap<String, PathBuf>> {
  let mut entries: BTreeMap<String, PathBuf> = BTreeMap::new();
  for entry in fs::read_dir(CACHE_HTML_DIR)
    .map_err(|e| io::Error::new(e.kind(), format!("{}: {}", CACHE_HTML_DIR, e)))?
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

fn work_item_from_cache(
  cache_stem: &str,
  cache_path: PathBuf,
  entry: Option<&PagesetEntry>,
  args: &RunArgs,
) -> WorkItem {
  let url = entry
    .map(|e| e.url.clone())
    .unwrap_or_else(|| url_hint_from_cache_path(&cache_path));
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
    trace_out: None,
  }
}

#[derive(Debug)]
struct RunningChild {
  item: WorkItem,
  started: Instant,
  child: Child,
}

fn spawn_worker(
  exe: &Path,
  args: &RunArgs,
  item: &WorkItem,
  diagnostics: DiagnosticsArg,
  soft_timeout_ms: Option<u64>,
  hard_timeout: Duration,
) -> io::Result<Child> {
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
    .arg("--viewport")
    .arg(format!("{}x{}", args.viewport.0, args.viewport.1))
    .arg("--dpr")
    .arg(args.dpr.to_string())
    .arg("--user-agent")
    .arg(&args.user_agent)
    .arg("--accept-language")
    .arg(&args.accept_language)
    .arg("--diagnostics")
    .arg(format!("{:?}", diagnostics).to_ascii_lowercase())
    .arg("--timeout")
    .arg(hard_timeout.as_secs().to_string());

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
  if args.layout_parallel.layout_parallel {
    cmd.arg("--layout-parallel");
    cmd
      .arg("--layout-parallel-min-fanout")
      .arg(args.layout_parallel.layout_parallel_min_fanout.to_string());
    if let Some(max_threads) = args.layout_parallel.layout_parallel_max_threads {
      cmd
        .arg("--layout-parallel-max-threads")
        .arg(max_threads.to_string());
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

  cmd.spawn()
}

fn run_queue(
  exe: &Path,
  args: &RunArgs,
  mut queue: std::collections::VecDeque<WorkItem>,
  hard_timeout: Duration,
  soft_timeout_ms: Option<u64>,
  diagnostics: DiagnosticsArg,
  jobs: usize,
) -> io::Result<()> {
  let mut running: Vec<RunningChild> = Vec::new();
  let current_sha = current_git_sha();

  while !queue.is_empty() || !running.is_empty() {
    while running.len() < jobs {
      let Some(item) = queue.pop_front() else {
        break;
      };
      let child = spawn_worker(exe, args, &item, diagnostics, soft_timeout_ms, hard_timeout)?;
      running.push(RunningChild {
        item,
        started: Instant::now(),
        child,
      });
    }

    let mut i = 0usize;
    while i < running.len() {
      let elapsed = running[i].started.elapsed();
      let timed_out = elapsed >= hard_timeout;

      if timed_out {
        let mut entry = running.swap_remove(i);
        let _ = entry.child.kill();
        let _ = entry.child.wait();
        append_timeout_stderr_note(&entry.item.stderr_path, elapsed);

        let previous = read_progress(&entry.item.progress_path);
        let mut progress = PageProgress::new(entry.item.url.clone());
        progress.status = ProgressStatus::Timeout;
        progress.total_ms = Some(hard_timeout.as_secs_f64() * 1000.0);
        progress.notes = format!("hard timeout after {:.2}s", hard_timeout.as_secs_f64());
        progress.hotspot = "unknown".to_string();
        let progress = progress.merge_preserving_manual(previous, current_sha.as_deref());
        let _ = write_progress(&entry.item.progress_path, &progress);

        let _ = write_text_file(
          &entry.item.log_path,
          &format!(
            "=== {} ===\nStatus: TIMEOUT\nKilled after {:.2}s\n",
            entry.item.cache_stem,
            hard_timeout.as_secs_f64()
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
              let short: String = p.notes.chars().take(120).collect();
              eprintln!(
                "FAIL {} {:?}: {} (exit: {})",
                entry.item.cache_stem, p.status, short, exit_str
              );
            }
          } else {
            // Fallback: worker exited but didn't write progress.
            let previous = read_progress(&entry.item.progress_path);
            let mut progress = PageProgress::new(entry.item.url.clone());
            progress.status = ProgressStatus::Panic;
            progress.total_ms = Some(entry.started.elapsed().as_secs_f64() * 1000.0);
            let note = synthesize_missing_progress_note(exit_summary);
            progress.notes = note.clone();
            progress.hotspot = "unknown".to_string();
            let mut progress = progress.merge_preserving_manual(previous, current_sha.as_deref());
            ensure_note_includes(&mut progress, &note);
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
          let mut progress = PageProgress::new(entry.item.url.clone());
          progress.status = ProgressStatus::Panic;
          progress.total_ms = Some(entry.started.elapsed().as_secs_f64() * 1000.0);
          progress.notes = "worker try_wait failed".to_string();
          progress.hotspot = "unknown".to_string();
          let progress = progress.merge_preserving_manual(previous, current_sha.as_deref());
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

  let hard_timeout = Duration::from_secs(args.timeout);
  let soft_timeout_ms = compute_soft_timeout_ms(hard_timeout, args.soft_timeout_ms);
  let trace_hard_timeout = compute_trace_hard_timeout(args.timeout, args.trace_timeout);
  let trace_soft_timeout_ms =
    compute_soft_timeout_ms(trace_hard_timeout, args.trace_soft_timeout_ms);

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
        if filter_matches(&stem, pageset_by_cache.get(&stem).copied()) {
          seen.insert(stem.clone());
          items.push(work_item_from_cache(
            &stem,
            path.clone(),
            pageset_by_cache.get(&stem).copied(),
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
        if let Some(path) = cached_paths.get(&entry.cache_stem) {
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
    let mut filtered_paths: Vec<(String, PathBuf)> = cached_paths
      .iter()
      .filter(|(stem, _)| filter_matches(stem, pageset_by_cache.get(stem.as_str()).copied()))
      .map(|(stem, path)| (stem.clone(), path.clone()))
      .collect();
    if let Some(ref filter) = page_filter {
      filtered_paths = filtered_paths
        .into_iter()
        .filter(|(stem, _)| filter.matches_cache_stem(stem, pageset_stem(stem).as_deref()))
        .collect();
    }
    filtered_paths = apply_shard_filter(filtered_paths, args.shard);
    if filtered_paths.is_empty() {
      eprintln!(
        "No cached pages matched the requested filters (--pages/shard) in {CACHE_HTML_DIR}."
      );
      std::process::exit(1);
    }
    items = filtered_paths
      .into_iter()
      .filter_map(|(cache_stem, cache_path)| {
        let entry = pageset_by_cache.get(&cache_stem).copied();
        Some(work_item_from_cache(&cache_stem, cache_path, entry, &args))
      })
      .collect();
  }
  let exe = std::env::current_exe()?;
  println!(
    "Pageset progress: {} pages ({} parallel), hard timeout {}s",
    items.len(),
    args.jobs,
    args.timeout
  );
  println!(
    "User-Agent: {}",
    normalize_user_agent_for_log(&args.user_agent)
  );
  println!("Accept-Language: {}", args.accept_language);
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
    soft_timeout_ms,
    args.diagnostics,
    args.jobs,
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
        trace_soft_timeout_ms,
        DiagnosticsArg::Verbose,
        args.trace_jobs,
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
  let stem = args.stem.clone();
  let cache_path = args.cache_path.clone();
  let verbose = args.verbose;
  let started = Instant::now();
  let current_sha = current_git_sha();

  let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| render_worker(args)));
  match result {
    Ok(res) => res,
    Err(panic) => {
      let previous = read_progress(&progress_path);
      let url = url_hint_from_cache_path(&cache_path);
      let mut progress = PageProgress::new(url);
      progress.status = ProgressStatus::Panic;
      progress.total_ms = Some(started.elapsed().as_secs_f64() * 1000.0);
      progress.notes = panic_to_string(panic);
      progress.hotspot = "unknown".to_string();
      let progress = progress.merge_preserving_manual(previous, current_sha.as_deref());
      let _ = write_progress(&progress_path, &progress);
      if let Some(path) = &log_path {
        let _ = write_text_file(
          path,
          &format!(
            "=== {stem} ===\nStatus: PANIC\nPanic: {}\nVerbose: {}\n",
            progress.notes, verbose
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
  use std::collections::HashSet;
  use std::fs;
  use std::io;
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

  fn status_set(statuses: &[ProgressStatus]) -> HashSet<ProgressStatus> {
    let mut set = HashSet::new();
    for status in statuses {
      set.insert(*status);
    }
    set
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
  fn buckets_from_diagnostics_include_text_and_encode_timings() {
    let timings = RenderStageTimings {
      html_decode_ms: Some(1.0),
      dom_parse_ms: Some(2.0),
      css_inlining_ms: Some(3.0),
      css_parse_ms: Some(4.0),
      cascade_ms: Some(5.0),
      box_tree_ms: Some(6.0),
      layout_ms: Some(7.0),
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
    assert_eq!(buckets.fetch, 3.0);
    assert_eq!(buckets.css, 7.0);
    assert_eq!(buckets.cascade, 11.0);
    assert_eq!(buckets.layout, 15.0);
    assert_eq!(buckets.paint, 55.0);
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
  fn shaping_notes_elide_text_and_keep_reason() {
    let long_text = "garbled".repeat(80);
    let note = format!(
      "[paint] Invalid paint parameters: Layout failed: MissingContext(\"Shaping failed (fonts=8): \
       Text(ShapingFailed {{ text: \\\"{long_text}\\\", reason: \\\"No suitable font found for \
       cluster ꧟\\\\u{{333}}\\\" }})\")"
    );

    let normalized = normalize_progress_note(&note);

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
  fn ensure_note_includes_preserves_manual_notes() {
    let mut progress = PageProgress::new("https://example.com".to_string());
    progress.notes = "manual note".to_string();
    let note = "worker exited (exit code 1) without writing progress";
    ensure_note_includes(&mut progress, note);
    assert!(progress.notes.starts_with("manual note"));
    assert!(progress.notes.contains(note));
  }

  #[test]
  fn ensure_note_includes_is_idempotent() {
    let mut progress = PageProgress::new("https://example.com".to_string());
    let note = "worker exited (exit code 1) without writing progress";
    ensure_note_includes(&mut progress, note);
    ensure_note_includes(&mut progress, note);
    assert_eq!(progress.notes, note);
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
}

fn main() -> io::Result<()> {
  let cli = Cli::parse();
  match cli.command {
    CommandKind::Sync(args) => sync(args),
    CommandKind::Run(args) => run(args),
    CommandKind::Report(args) => report(args),
    CommandKind::Worker(args) => worker(args),
  }
}
