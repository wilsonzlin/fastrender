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
use common::args::{parse_shard, ResourceAccessArgs};
use common::render_pipeline::{
  build_render_configs, follow_client_redirects, format_error_with_chain, read_cached_document,
  render_document, RenderConfigBundle, RenderSurface,
};
use fastrender::api::{DiagnosticsLevel, RenderDiagnostics, ResourceKind};
use fastrender::error::{RenderError, RenderStage};
use fastrender::pageset::{pageset_stem, CACHE_HTML_DIR};
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
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs::{self, File, OpenOptions};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};

#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;

const ASSET_DIR: &str = "fetches/assets";
const DEFAULT_PROGRESS_DIR: &str = "progress/pages";
const DEFAULT_LOG_DIR: &str = "target/pageset/logs";
const DEFAULT_TRACE_DIR: &str = "target/pageset/traces";
const DEFAULT_TRACE_PROGRESS_DIR: &str = "target/pageset/trace-progress";
// Treat traces smaller than this as likely incomplete/partial.
const MIN_TRACE_BYTES: u64 = 4096;

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

  #[command(flatten)]
  resource_access: ResourceAccessArgs,

  /// Render only listed pages (comma-separated cache stems)
  #[arg(long, value_delimiter = ',')]
  pages: Option<Vec<String>>,

  /// Process only a deterministic shard of the cached pages (index/total, 0-based)
  #[arg(long, value_parser = parse_shard)]
  shard: Option<(usize, usize)>,

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
  resource_access: ResourceAccessArgs,

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

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
enum ProgressStatus {
  Ok,
  Timeout,
  Panic,
  Error,
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

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
struct StageBuckets {
  fetch: f64,
  css: f64,
  cascade: f64,
  layout: f64,
  paint: f64,
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

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
struct PageProgress {
  url: String,
  status: ProgressStatus,
  total_ms: Option<f64>,
  stages_ms: StageBuckets,
  notes: String,
  hotspot: String,
  last_good_commit: String,
  last_regression_commit: String,
}

impl PageProgress {
  fn new(url: String) -> Self {
    Self {
      url,
      status: ProgressStatus::Error,
      total_ms: None,
      stages_ms: StageBuckets::default(),
      notes: String::new(),
      hotspot: String::new(),
      last_good_commit: String::new(),
      last_regression_commit: String::new(),
    }
  }

  fn merge_preserving_manual(mut self, previous: Option<PageProgress>) -> Self {
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
}

fn read_progress(path: &Path) -> Option<PageProgress> {
  let raw = fs::read_to_string(path).ok()?;
  serde_json::from_str::<PageProgress>(&raw).ok()
}

#[derive(Debug)]
struct LoadedProgress {
  stem: String,
  progress: PageProgress,
}

#[derive(Debug, Default, Clone, Copy)]
struct StatusCounts {
  ok: usize,
  timeout: usize,
  panic: usize,
  error: usize,
}

fn url_hint_from_cache_path(cache_path: &Path) -> String {
  let mut meta_path = cache_path.to_path_buf();
  if let Some(ext) = meta_path.extension().and_then(|e| e.to_str()) {
    meta_path.set_extension(format!("{ext}.meta"));
  } else {
    meta_path.set_extension("meta");
  }
  let meta = fs::read_to_string(&meta_path).ok();
  let (_, url) = meta
    .as_deref()
    .map(parse_cached_html_meta)
    .unwrap_or((None, None));
  url.unwrap_or_else(|| format!("file://{}", cache_path.display()))
}

fn write_progress(path: &Path, progress: &PageProgress) -> io::Result<()> {
  if let Some(parent) = path.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent)?;
    }
  }
  let json = serde_json::to_string_pretty(progress)
    .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
  fs::write(path, format!("{json}\n"))
}

fn buckets_from_diagnostics(diag: &RenderDiagnostics) -> StageBuckets {
  let Some(stats) = diag.stats.as_ref() else {
    return StageBuckets::default();
  };
  let t = &stats.timings;
  let fetch = t.html_decode_ms.unwrap_or(0.0) + t.dom_parse_ms.unwrap_or(0.0);
  let css = t.css_inlining_ms.unwrap_or(0.0) + t.css_parse_ms.unwrap_or(0.0);
  let cascade = t.cascade_ms.unwrap_or(0.0) + t.box_tree_ms.unwrap_or(0.0);
  let layout = t.layout_ms.unwrap_or(0.0);
  let paint = t.paint_build_ms.unwrap_or(0.0)
    + t.paint_optimize_ms.unwrap_or(0.0)
    + t.paint_rasterize_ms.unwrap_or(0.0);
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

fn hotspot_from_timeout_stage(stage: RenderStage) -> &'static str {
  match stage {
    RenderStage::DomParse => "fetch",
    RenderStage::Css => "css",
    RenderStage::Cascade => "cascade",
    RenderStage::Layout => "layout",
    RenderStage::Paint => "paint",
  }
}

fn render_worker(args: WorkerArgs) -> io::Result<()> {
  let started = Instant::now();
  let mut log = String::new();

  let progress_before = read_progress(&args.progress_path);

  let cached = match read_cached_document(&args.cache_path) {
    Ok(doc) => doc,
    Err(e) => {
      let msg = format_error_with_chain(&e, args.verbose);
      log.push_str(&format!("Read error: {msg}\n"));
      let mut progress = PageProgress::new(url_hint_from_cache_path(&args.cache_path));
      progress.status = ProgressStatus::Error;
      progress.notes = format!("read: {msg}");
      progress.hotspot = "fetch".to_string();
      let progress = progress.merge_preserving_manual(progress_before);
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
  if let Some(ct) = &cached.content_type {
    log.push_str(&format!("Content-Type: {ct}\n"));
  }
  log.push_str(&format!("HTML bytes: {}\n", cached.byte_len));

  let http = HttpFetcher::new()
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
        let msg = format_error_with_chain(&e, args.verbose);
        log.push_str(&format!("Renderer init error: {msg}\n"));
        let mut progress = PageProgress::new(url);
        progress.status = ProgressStatus::Error;
        progress.notes = format!("renderer init: {msg}");
        progress.hotspot = "unknown".to_string();
        let progress = progress.merge_preserving_manual(progress_before);
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
      progress.hotspot = guess_hotspot(&progress.stages_ms).to_string();
      log.push_str("Status: OK\n");
      append_fetch_error_summary(&mut log, &result.diagnostics);
      append_stage_summary(&mut log, &result.diagnostics);
    }
    Ok(Err(err)) => {
      match &err {
        fastrender::Error::Render(RenderError::Timeout { stage, elapsed }) => {
          progress.status = ProgressStatus::Timeout;
          progress.notes = format!("timeout at {stage} after {elapsed:?}");
          progress.hotspot = hotspot_from_timeout_stage(*stage).to_string();
        }
        _ => {
          progress.status = ProgressStatus::Error;
          progress.notes = format_error_with_chain(&err, args.verbose);
          progress.hotspot = "unknown".to_string();
        }
      }

      log.push_str(&format!("Status: {:?}\n", progress.status));
      log.push_str(&format!(
        "Error: {}\n",
        format_error_with_chain(&err, args.verbose)
      ));
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

  let progress = progress.merge_preserving_manual(progress_before);
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
  log.push_str(&format!("  paint_build: {}\n", fmt(t.paint_build_ms)));
  log.push_str(&format!("  paint_optimize: {}\n", fmt(t.paint_optimize_ms)));
  log.push_str(&format!(
    "  paint_rasterize: {}\n",
    fmt(t.paint_rasterize_ms)
  ));
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
  if let Some(parent) = path.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent)?;
    }
  }
  fs::write(path, contents)
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
    progresses.push(LoadedProgress { stem, progress });
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
    progresses.push(LoadedProgress { stem, progress });
  }
  Ok(progresses)
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

fn report(args: ReportArgs) -> io::Result<()> {
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
  url: String,
  cache_path: PathBuf,
  progress_path: PathBuf,
  log_path: PathBuf,
  stderr_path: PathBuf,
  trace_out: Option<PathBuf>,
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
) -> io::Result<Child> {
  let mut cmd = Command::new(exe);
  cmd
    .arg("worker")
    .arg("--cache-path")
    .arg(&item.cache_path)
    .arg("--stem")
    .arg(&item.stem)
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
    .arg(format!("{:?}", diagnostics).to_ascii_lowercase());

  if args.no_http_freshness {
    cmd.arg("--no-http-freshness");
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

  while !queue.is_empty() || !running.is_empty() {
    while running.len() < jobs {
      let Some(item) = queue.pop_front() else {
        break;
      };
      let child = spawn_worker(exe, args, &item, diagnostics, soft_timeout_ms)?;
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
        let progress = progress.merge_preserving_manual(previous);
        let _ = write_progress(&entry.item.progress_path, &progress);

        let _ = write_text_file(
          &entry.item.log_path,
          &format!(
            "=== {} ===\nStatus: TIMEOUT\nKilled after {:.2}s\n",
            entry.item.stem,
            hard_timeout.as_secs_f64()
          ),
        );

        eprintln!("TIMEOUT {}", entry.item.stem);
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
                entry.item.stem, p.status, short, exit_str
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
            let mut progress = progress.merge_preserving_manual(previous);
            ensure_note_includes(&mut progress, &note);
            let _ = write_progress(&entry.item.progress_path, &progress);
            eprintln!(
              "PANIC {} (no progress written, exit: {})",
              entry.item.stem, exit_str
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
          let progress = progress.merge_preserving_manual(previous);
          let _ = write_progress(&entry.item.progress_path, &progress);
          eprintln!("PANIC {} (try_wait failed)", entry.item.stem);
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

  let filter: Option<HashSet<String>> = args.pages.as_ref().map(|v| {
    v.iter()
      .filter_map(|s| pageset_stem(s))
      .collect::<HashSet<_>>()
  });

  let mut entries: Vec<(PathBuf, String)> = match fs::read_dir(CACHE_HTML_DIR) {
    Ok(dir) => dir
      .filter_map(|e| e.ok().map(|e| e.path()))
      .filter(|path| path.extension().map(|x| x == "html").unwrap_or(false))
      .filter_map(|path| {
        let stem_os = path.file_stem()?;
        let stem = pageset_stem(&stem_os.to_string_lossy())?;
        if let Some(ref filter) = filter {
          if !filter.contains(&stem) {
            return None;
          }
        }
        Some((path, stem))
      })
      .collect(),
    Err(_) => {
      eprintln!("No cached pages found in {CACHE_HTML_DIR}.");
      eprintln!("Run fetch_pages first.");
      std::process::exit(1);
    }
  };

  let mut stem_to_paths: HashMap<String, Vec<PathBuf>> = HashMap::new();
  for (path, stem) in &entries {
    stem_to_paths
      .entry(stem.clone())
      .or_default()
      .push(path.clone());
  }

  let duplicates: Vec<_> = stem_to_paths
    .into_iter()
    .filter(|(_, paths)| paths.len() > 1)
    .collect();
  if !duplicates.is_empty() {
    eprintln!("Cached HTML stems collide after normalization:");
    for (stem, paths) in duplicates {
      let joined = paths
        .iter()
        .map(|p| p.display().to_string())
        .collect::<Vec<_>>()
        .join(", ");
      eprintln!("  {stem}: {joined}");
    }
    eprintln!(
      "Clean stale files in {} or re-run fetch_pages --refresh.",
      CACHE_HTML_DIR
    );
    std::process::exit(1);
  }

  entries.sort_by(|a, b| a.1.cmp(&b.1));
  if let Some((index, total)) = args.shard {
    entries = entries
      .into_iter()
      .enumerate()
      .filter(|(idx, _)| idx % total == index)
      .map(|(_, entry)| entry)
      .collect();
  }
  if entries.is_empty() {
    eprintln!("No cached pages in {CACHE_HTML_DIR}. Run fetch_pages first.");
    std::process::exit(1);
  }

  let items: Vec<WorkItem> = entries
    .into_iter()
    .filter_map(|cache_path| {
      let (cache_path, stem) = cache_path;
      let url = url_hint_from_cache_path(&cache_path);
      Some(WorkItem {
        stem: stem.clone(),
        url,
        cache_path,
        progress_path: args.progress_dir.join(format!("{stem}.json")),
        log_path: args.log_dir.join(format!("{stem}.log")),
        stderr_path: args.log_dir.join(format!("{stem}.stderr.log")),
        trace_out: None,
      })
    })
    .collect();

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
      let trace_out = args.trace_dir.join(format!("{}.json", item.stem));
      let trace_progress = args.trace_progress_dir.join(format!("{}.json", item.stem));
      trace_items.push(WorkItem {
        stem: item.stem.clone(),
        url: item.url.clone(),
        cache_path: item.cache_path.clone(),
        progress_path: trace_progress,
        log_path: args.log_dir.join(format!("{}.trace.log", item.stem)),
        stderr_path: args.log_dir.join(format!("{}.trace.stderr.log", item.stem)),
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
      let progress = progress.merge_preserving_manual(previous);
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
  use std::time::Duration;

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
    let merged = new_progress.merge_preserving_manual(Some(previous));
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
    let merged = PageProgress::default().merge_preserving_manual(Some(previous));
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
}

fn main() -> io::Result<()> {
  let cli = Cli::parse();
  match cli.command {
    CommandKind::Run(args) => run(args),
    CommandKind::Report(args) => report(args),
    CommandKind::Worker(args) => worker(args),
  }
}
