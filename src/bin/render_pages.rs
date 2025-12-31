//! Render all cached pages in parallel
//!
//! Renders all HTML files in fetches/html/ to fetches/renders/
//! Logs per-page to fetches/renders/{name}.log
//! Summary to fetches/renders/_summary.log

mod common;

use clap::{ArgAction, Args as ClapArgs, Parser, Subcommand, ValueEnum};
use common::args::{
  parse_bool_preference, parse_color_scheme, parse_contrast, parse_shard, parse_viewport,
  CompatArgs, DiskCacheArgs, LayoutParallelArgs, ResourceAccessArgs,
};
use common::render_pipeline::{
  append_timeout_stderr_note, apply_test_render_delay, apply_worker_common_args,
  build_http_fetcher, build_render_configs, compute_soft_timeout_ms, configure_worker_stdio,
  follow_client_redirects, format_error_with_chain, format_exit_status, log_diagnostics,
  read_cached_document, render_document_with_artifacts, summarize_exit_status, ExitStatusSummary,
  RenderConfigBundle, RenderSurface, WorkerCommonArgs,
};
use fastrender::api::{FastRenderPool, FastRenderPoolConfig};
use fastrender::debug::runtime::RuntimeToggles;
use fastrender::debug::snapshot::QuirksModeSnapshot;
use fastrender::dom::{DomNode, DomNodeType};
use fastrender::image_output::encode_image;
use fastrender::pageset::{pageset_stem, PagesetFilter, CACHE_HTML_DIR};
use fastrender::paint::display_list::DisplayItem;
use fastrender::resource::normalize_user_agent_for_log;
#[cfg(not(feature = "disk_cache"))]
use fastrender::resource::CachingFetcher;
use fastrender::resource::CachingFetcherConfig;
#[cfg(feature = "disk_cache")]
use fastrender::resource::DiskCachingFetcher;
use fastrender::resource::ResourceFetcher;
use fastrender::resource::DEFAULT_ACCEPT_LANGUAGE;
use fastrender::resource::DEFAULT_USER_AGENT;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaType;
use fastrender::tree::box_tree::{BoxNode, BoxType};
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode};
use fastrender::OutputFormat;
use fastrender::{
  snapshot_pipeline, BoxTree, DisplayList, FragmentTree, PipelineSnapshot, RenderArtifactRequest,
  RenderArtifacts,
};
use serde::{Deserialize, Serialize};
use serde_json;
use std::collections::{HashMap, VecDeque};
use std::fmt::Write;
use std::fs;
use std::io;
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};
use std::process::{Child, Command};
use std::sync::mpsc::channel;
use std::sync::mpsc::RecvTimeoutError;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use std::time::Duration;
use std::time::Instant;

const ASSET_DIR: &str = "fetches/assets";
const RENDER_DIR: &str = "fetches/renders";
const RENDER_STACK_SIZE: usize = 64 * 1024 * 1024; // 64MB to avoid stack overflows on large pages

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
enum DumpMode {
  None,
  Summary,
  Full,
}

impl DumpMode {
  fn artifact_request(self) -> RenderArtifactRequest {
    match self {
      DumpMode::None => RenderArtifactRequest::none(),
      DumpMode::Summary => RenderArtifactRequest::summary(),
      DumpMode::Full => RenderArtifactRequest::full(),
    }
  }
}

/// Render all cached pages in parallel
#[derive(Parser, Debug)]
#[command(name = "render_pages", version, about)]
struct Cli {
  #[command(flatten)]
  args: Args,

  #[command(subcommand)]
  command: Option<CliCommand>,
}

#[derive(Subcommand, Debug)]
enum CliCommand {
  #[command(hide = true)]
  Worker(WorkerArgs),
}

#[derive(ClapArgs, Debug, Clone)]
struct Args {
  /// Number of parallel renders/workers
  #[arg(long, short, default_value_t = num_cpus::get())]
  jobs: usize,

  /// Hard per-page timeout in seconds
  #[arg(long, default_value_t = 5)]
  timeout: u64,

  /// Cooperative timeout handed to the renderer in milliseconds (0 disables)
  #[arg(long)]
  soft_timeout_ms: Option<u64>,

  /// Run renders in-process instead of separate worker processes
  #[arg(long)]
  in_process: bool,

  /// Viewport size as WxH (e.g., 1200x800)
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset
  #[arg(long, default_value = "1.0")]
  dpr: f32,

  /// Reduced transparency preference (reduce|no-preference)
  #[arg(long, value_parser = parse_bool_preference)]
  prefers_reduced_transparency: Option<bool>,

  /// Reduced motion preference (reduce|no-preference)
  #[arg(long, value_parser = parse_bool_preference)]
  prefers_reduced_motion: Option<bool>,

  /// Reduced data preference (reduce|no-preference)
  #[arg(long, value_parser = parse_bool_preference)]
  prefers_reduced_data: Option<bool>,

  /// Contrast preference (more|high|less|low|custom|forced|no-preference)
  #[arg(long, value_parser = parse_contrast)]
  prefers_contrast: Option<String>,

  /// Color scheme preference (light|dark|no-preference)
  #[arg(long, value_parser = parse_color_scheme)]
  prefers_color_scheme: Option<String>,

  /// Render only listed pages (comma-separated)
  #[arg(long, value_delimiter = ',')]
  pages: Option<Vec<String>>,

  /// Process only a deterministic shard of the cached pages (index/total, 0-based)
  #[arg(long, value_parser = parse_shard)]
  shard: Option<(usize, usize)>,

  /// Horizontal scroll offset in CSS px
  #[arg(long, default_value = "0.0")]
  scroll_x: f32,

  /// Vertical scroll offset in CSS px
  #[arg(long, default_value = "0.0")]
  scroll_y: f32,

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

  #[command(flatten)]
  resource_access: ResourceAccessArgs,

  #[command(flatten)]
  layout_parallel: LayoutParallelArgs,

  #[command(flatten)]
  compat: CompatArgs,

  /// Enable per-stage timing logs
  #[arg(long)]
  timings: bool,

  /// Write structured diagnostics alongside renders
  #[arg(long)]
  diagnostics_json: bool,

  /// Dump intermediate artifacts (summary|full)
  #[arg(long, value_enum, default_value = "none")]
  dump_intermediate: DumpMode,

  /// Only write intermediate dumps for failures
  #[arg(long)]
  only_failures: bool,

  /// Print full error chains on failure
  #[arg(long)]
  verbose: bool,

  /// Positional page filters (cache stems)
  #[arg(trailing_var_arg = true)]
  filter_pages: Vec<String>,
}

#[derive(ClapArgs, Debug, Clone)]
struct WorkerArgs {
  #[command(flatten)]
  args: Args,

  /// Cached HTML path (from fetches/html/*.html)
  #[arg(long)]
  cache_path: PathBuf,

  /// Page stem (used only for logging)
  #[arg(long)]
  stem: String,

  /// Original cached stem (before normalization)
  #[arg(long)]
  cache_stem: String,
}

struct PageResult {
  name: String,
  status: Status,
  time_ms: u128,
  size: Option<usize>,
}

#[derive(Clone)]
struct CachedEntry {
  path: PathBuf,
  stem: String,
  cache_stem: String,
}

enum Status {
  Ok,
  Crash(String),
  Error(String),
  Timeout(String),
}

#[derive(Clone)]
struct RenderShared {
  render_pool: FastRenderPool,
  fetcher: Arc<dyn ResourceFetcher>,
  base_options: fastrender::api::RenderOptions,
  artifact_request: RenderArtifactRequest,
  dump_mode: DumpMode,
  only_failures: bool,
  diagnostics_json: bool,
  verbose: bool,
  viewport: (u32, u32),
  user_agent: String,
  timeout_secs: Option<u64>,
}

#[derive(Debug, Serialize, Deserialize)]
struct WorkerResult {
  status: String,
  message: Option<String>,
  time_ms: u128,
  png_size: Option<usize>,
}

impl WorkerResult {
  fn from_page_result(result: &PageResult) -> Self {
    Self {
      status: status_label(&result.status).to_string(),
      message: status_error(&result.status).map(str::to_string),
      time_ms: result.time_ms,
      png_size: result.size,
    }
  }

  fn into_page_result(self, name: String) -> PageResult {
    let status = match self.status.as_str() {
      "ok" => Status::Ok,
      "crash" => Status::Crash(self.message.unwrap_or_else(|| "crash".to_string())),
      "timeout" => Status::Timeout(self.message.unwrap_or_else(|| "timeout".to_string())),
      "error" => Status::Error(self.message.unwrap_or_else(|| "error".to_string())),
      other => Status::Error(format!("unknown status {other}")),
    };
    PageResult {
      name,
      status,
      time_ms: self.time_ms,
      size: self.png_size,
    }
  }
}

struct RunningChild {
  entry: CachedEntry,
  started: Instant,
  child: Child,
}

struct RenderOutcome {
  png: Vec<u8>,
  diagnostics: fastrender::RenderDiagnostics,
  artifacts: RenderArtifacts,
}

fn main() {
  if RuntimeToggles::from_env().truthy("FASTR_LAYOUT_PROFILE") {
    eprintln!("FASTR_LAYOUT_PROFILE enabled: layout timings will be logged");
  }

  let cli = Cli::parse();

  if let Some(CliCommand::Worker(worker_args)) = cli.command {
    if let Err(err) = worker_main(worker_args) {
      eprintln!("worker failed: {err}");
      std::process::exit(1);
    }
    return;
  }

  if let Err(err) = run(cli.args) {
    eprintln!("{err}");
    std::process::exit(1);
  }
}

fn run(args: Args) -> io::Result<()> {
  if args.jobs == 0 {
    eprintln!("jobs must be > 0");
    std::process::exit(2);
  }
  if args.timeout == 0 {
    eprintln!("timeout must be > 0");
    std::process::exit(2);
  }

  // Build page filter from --pages and positional args
  let page_filter = {
    let mut all_pages: Vec<String> = args.pages.clone().unwrap_or_default();
    all_pages.extend(args.filter_pages.clone());
    PagesetFilter::from_inputs(&all_pages)
  };

  let hard_timeout = Duration::from_secs(args.timeout);
  let soft_timeout_ms = compute_soft_timeout_ms(hard_timeout, args.soft_timeout_ms);

  if args.timings {
    std::env::set_var("FASTR_RENDER_TIMINGS", "1");
  }

  if let Some(reduce) = args.prefers_reduced_transparency {
    std::env::set_var(
      "FASTR_PREFERS_REDUCED_TRANSPARENCY",
      if reduce { "reduce" } else { "no-preference" },
    );
  }

  if let Some(reduce) = args.prefers_reduced_motion {
    std::env::set_var(
      "FASTR_PREFERS_REDUCED_MOTION",
      if reduce { "reduce" } else { "no-preference" },
    );
  }

  if let Some(reduce) = args.prefers_reduced_data {
    std::env::set_var(
      "FASTR_PREFERS_REDUCED_DATA",
      if reduce { "reduce" } else { "no-preference" },
    );
  }

  if let Some(ref contrast) = args.prefers_contrast {
    std::env::set_var("FASTR_PREFERS_CONTRAST", contrast);
  }

  if let Some(ref color_scheme) = args.prefers_color_scheme {
    std::env::set_var("FASTR_PREFERS_COLOR_SCHEME", color_scheme);
  }

  // Create directories
  fs::create_dir_all(RENDER_DIR)?;
  fs::create_dir_all(ASSET_DIR)?;

  let entries = collect_entries(&args, page_filter);
  if entries.is_empty() {
    eprintln!(
      "No cached pages in {}. Run fetch_pages first.",
      CACHE_HTML_DIR
    );
    std::process::exit(1);
  }

  println!(
    "Rendering {} pages ({} parallel)...",
    entries.len(),
    args.jobs
  );
  println!(
    "User-Agent: {}",
    normalize_user_agent_for_log(&args.user_agent)
  );
  println!("Accept-Language: {}", args.accept_language);
  if let Some((index, total)) = args.shard {
    println!("Shard: {}/{}", index, total);
  }
  if args.in_process {
    println!("Mode: in-process (no worker isolation)");
  } else {
    println!("Mode: per-page worker isolation");
  }
  println!();

  let start = Instant::now();
  let results = if args.in_process {
    run_in_process(&args, &entries, soft_timeout_ms)?
  } else {
    run_workers(&args, entries.clone(), hard_timeout, soft_timeout_ms)?
  };

  let total_elapsed = start.elapsed();
  let mut results = results;
  results.sort_by(|a, b| a.name.cmp(&b.name));

  // Count results
  let pass = results
    .iter()
    .filter(|r| matches!(r.status, Status::Ok))
    .count();
  let timeout = results
    .iter()
    .filter(|r| matches!(r.status, Status::Timeout(_)))
    .count();
  let crash = results
    .iter()
    .filter(|r| matches!(r.status, Status::Crash(_)))
    .count();
  let error = results
    .iter()
    .filter(|r| matches!(r.status, Status::Error(_)))
    .count();

  // Write summary log
  let summary_path = PathBuf::from(RENDER_DIR).join("_summary.log");
  let mut summary = String::new();
  summary.push_str("=== Render Summary ===\n");
  let _ = writeln!(
    summary,
    "Date: {}",
    chrono::Local::now().format("%Y-%m-%d %H:%M:%S")
  );
  let _ = writeln!(summary, "Total time: {:.1}s", total_elapsed.as_secs_f64());
  let _ = writeln!(
    summary,
    "Pages: {} total, {} pass, {} timeout, {} crash, {} error\n",
    results.len(),
    pass,
    timeout,
    crash,
    error
  );

  let _ = writeln!(
    summary,
    "{:<40} {:>8} {:>10} STATUS",
    "PAGE", "TIME", "SIZE"
  );
  let _ = writeln!(summary, "{}", "-".repeat(75));

  for r in &results {
    let status_str = match &r.status {
      Status::Ok => "OK".to_string(),
      Status::Crash(msg) => format!("CRASH: {}", msg.chars().take(30).collect::<String>()),
      Status::Error(msg) => format!("ERROR: {}", msg.chars().take(30).collect::<String>()),
      Status::Timeout(msg) => format!("TIMEOUT: {}", msg.chars().take(30).collect::<String>()),
    };
    let size_str = r
      .size
      .map(|s| format!("{}b", s))
      .unwrap_or_else(|| "-".to_string());
    let _ = writeln!(
      summary,
      "{:<40} {:>6}ms {:>10} {}",
      r.name, r.time_ms, size_str, status_str
    );
  }

  let _ = writeln!(summary, "\n{}", "-".repeat(75));
  let _ = writeln!(summary, "Total: {:.1}s", total_elapsed.as_secs_f64());

  let _ = fs::write(&summary_path, &summary);

  // Print summary
  println!();
  println!(
    "Done in {:.1}s: ✓{} pass, ⏱{} timeout, ✗{} crash, ✗{} error",
    total_elapsed.as_secs_f64(),
    pass,
    timeout,
    crash,
    error
  );
  println!();
  println!("Renders: {}/", RENDER_DIR);
  println!("Summary: {}", summary_path.display());
  println!("Logs:    {}/<page>.log", RENDER_DIR);
  println!();
  println!("Inspect: open {}/*.png", RENDER_DIR);

  if timeout > 0 || crash > 0 || error > 0 {
    std::process::exit(1);
  }

  Ok(())
}

fn log_path_for(stem: &str) -> PathBuf {
  PathBuf::from(RENDER_DIR).join(format!("{stem}.log"))
}

fn diagnostics_path_for(stem: &str) -> PathBuf {
  PathBuf::from(RENDER_DIR).join(format!("{stem}.diagnostics.json"))
}

fn result_path_for(stem: &str) -> PathBuf {
  PathBuf::from(RENDER_DIR).join(format!("{stem}.result.json"))
}

fn stderr_path_for(stem: &str) -> PathBuf {
  PathBuf::from(RENDER_DIR).join(format!("{stem}.stderr.log"))
}

fn output_path_for(stem: &str) -> PathBuf {
  PathBuf::from(RENDER_DIR).join(format!("{stem}.png"))
}

fn panic_to_string(panic: Box<dyn std::any::Any + Send + 'static>) -> String {
  panic
    .downcast_ref::<&str>()
    .map(|s| s.to_string())
    .or_else(|| panic.downcast_ref::<String>().cloned())
    .unwrap_or_else(|| "unknown panic".to_string())
}

fn collect_entries(args: &Args, page_filter: Option<PagesetFilter>) -> Vec<CachedEntry> {
  let mut entries: Vec<CachedEntry> = match fs::read_dir(CACHE_HTML_DIR) {
    Ok(dir) => dir
      .filter_map(|e| e.ok().map(|e| e.path()))
      .filter(|path| path.extension().map(|x| x == "html").unwrap_or(false))
      .filter_map(|path| {
        let stem_os = path.file_stem()?;
        let cache_stem = stem_os.to_string_lossy().to_string();
        let stem = pageset_stem(&cache_stem).unwrap_or_else(|| cache_stem.clone());
        if let Some(ref filter) = page_filter {
          if !filter.matches_cache_stem(&cache_stem, Some(&stem)) {
            return None;
          }
        }
        Some(CachedEntry {
          path,
          stem,
          cache_stem,
        })
      })
      .collect(),
    Err(_) => {
      eprintln!("No cached pages found in {}.", CACHE_HTML_DIR);
      eprintln!("Run fetch_pages first.");
      std::process::exit(1);
    }
  };

  let mut stem_to_paths: HashMap<String, Vec<PathBuf>> = HashMap::new();
  for entry in &entries {
    stem_to_paths
      .entry(entry.stem.clone())
      .or_default()
      .push(entry.path.clone());
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

  entries.sort_by(|a, b| a.cache_stem.cmp(&b.cache_stem));

  if let Some((index, total)) = args.shard {
    entries = entries
      .into_iter()
      .enumerate()
      .filter(|(idx, _)| idx % total == index)
      .map(|(_, entry)| entry)
      .collect();
  }

  entries
}

fn build_render_shared(
  args: &Args,
  pool_size: usize,
  render_timeout_secs: Option<u64>,
  fetch_timeout_secs: Option<u64>,
  soft_timeout_ms: Option<u64>,
) -> RenderShared {
  // Create shared caching fetcher
  let http = build_http_fetcher(&args.user_agent, &args.accept_language, fetch_timeout_secs);
  let honor_http_freshness = cfg!(feature = "disk_cache") && !args.no_http_freshness;
  let memory_config = CachingFetcherConfig {
    honor_http_cache_freshness: honor_http_freshness,
    ..CachingFetcherConfig::default()
  };
  #[cfg(feature = "disk_cache")]
  let fetcher: Arc<dyn ResourceFetcher> = Arc::new(DiskCachingFetcher::with_configs(
    http,
    ASSET_DIR,
    memory_config,
    args.disk_cache.to_config(),
  ));
  #[cfg(not(feature = "disk_cache"))]
  let fetcher: Arc<dyn ResourceFetcher> =
    Arc::new(CachingFetcher::with_config(http, memory_config));

  let RenderConfigBundle {
    config,
    mut options,
  } = build_render_configs(&RenderSurface {
    viewport: args.viewport,
    scroll_x: args.scroll_x,
    scroll_y: args.scroll_y,
    dpr: args.dpr,
    media_type: MediaType::Screen,
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
    font_config: None,
    compat_profile: args.compat.compat_profile(),
    dom_compat_mode: args.compat.dom_compat_mode(),
  });

  if let Some(ms) = soft_timeout_ms {
    if ms > 0 {
      options.timeout = Some(Duration::from_millis(ms));
    }
  }

  let render_pool = FastRenderPool::with_config(
    FastRenderPoolConfig::new()
      .with_renderer_config(config.clone())
      .with_fetcher(Arc::clone(&fetcher))
      .with_pool_size(pool_size),
  )
  .expect("create render pool");

  RenderShared {
    render_pool,
    fetcher,
    base_options: options,
    artifact_request: args.dump_intermediate.artifact_request(),
    dump_mode: args.dump_intermediate,
    only_failures: args.only_failures,
    diagnostics_json: args.diagnostics_json,
    verbose: args.verbose,
    viewport: args.viewport,
    user_agent: args.user_agent.clone(),
    timeout_secs: render_timeout_secs,
  }
}

fn should_panic_for_test(stem: &str) -> bool {
  // Test hook: panic within the render worker when the configured stem matches.
  match std::env::var("FASTR_TEST_RENDER_PANIC_STEM") {
    Ok(value) => value
      .split(',')
      .map(|v| v.trim())
      .any(|candidate| candidate == stem),
    Err(_) => false,
  }
}

fn render_panic_result(
  shared: &RenderShared,
  entry: &CachedEntry,
  started: Instant,
  message: String,
) -> PageResult {
  let time_ms = started.elapsed().as_millis();
  let mut log = format!(
    "=== {} ===\nStatus: CRASH\nPanic: {}\nTime: {}ms\n",
    entry.stem, message, time_ms
  );
  let _ = fs::write(log_path_for(&entry.stem), &log);
  if shared.diagnostics_json {
    let diag = DiagnosticsFile {
      page: entry.stem.clone(),
      status: "crash".to_string(),
      error: Some(message.clone()),
      time_ms,
      png_size: None,
      diagnostics: fastrender::RenderDiagnostics::default(),
      summary: None,
    };
    write_stage_json(diagnostics_path_for(&entry.stem), &diag, &mut log);
    let _ = fs::write(log_path_for(&entry.stem), &log);
  }
  PageResult {
    name: entry.stem.clone(),
    status: Status::Crash(message),
    time_ms,
    size: None,
  }
}

fn render_entry(shared: &RenderShared, entry: &CachedEntry) -> PageResult {
  let started = Instant::now();
  apply_test_render_delay(Some(&entry.stem));
  let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
    if should_panic_for_test(&entry.stem) {
      panic!("FASTR_TEST_RENDER_PANIC_STEM triggered for {}", entry.stem);
    }
    render_entry_inner(shared, entry)
  }));
  match result {
    Ok(res) => res,
    Err(panic) => render_panic_result(shared, entry, started, panic_to_string(panic)),
  }
}

fn render_entry_inner(shared: &RenderShared, entry: &CachedEntry) -> PageResult {
  let name = entry.stem.clone();
  let path = entry.path.clone();
  let output_path = output_path_for(&name);
  let log_path = log_path_for(&name);

  let page_start = Instant::now();
  let mut log = String::new();

  let _ = writeln!(log, "=== {} ===", name);
  let _ = writeln!(log, "Stem: {}", entry.cache_stem);
  let _ = writeln!(log, "Source: {}", path.display());
  let _ = writeln!(log, "Output: {}", output_path.display());
  let _ = writeln!(
    log,
    "User-Agent: {}\n",
    normalize_user_agent_for_log(&shared.user_agent)
  );

  let cached = match read_cached_document(&path) {
    Ok(doc) => doc,
    Err(e) => {
      let msg = format_error_with_chain(&e, shared.verbose);
      let _ = writeln!(log, "Read error: {}", msg);
      let _ = fs::write(&log_path, &log);
      return PageResult {
        name,
        status: Status::Error(format!("read: {}", msg)),
        time_ms: 0,
        size: None,
      };
    }
  };

  let _ = writeln!(log, "HTML bytes: {}", cached.byte_len);
  if let Some(ct) = &cached.content_type {
    let _ = writeln!(log, "Content-Type: {}", ct);
  }
  let _ = writeln!(log, "Viewport: {}x{}", shared.viewport.0, shared.viewport.1);
  let _ = writeln!(log, "Scroll-X: {}px", shared.base_options.scroll_x);
  let _ = writeln!(log, "Scroll-Y: {}px", shared.base_options.scroll_y);

  let mut doc = cached.document;
  let _ = writeln!(log, "Resource base: {}", doc.base_url);

  doc = follow_client_redirects(shared.fetcher.as_ref(), doc, |line| {
    let _ = writeln!(log, "{line}");
  });

  let _ = writeln!(log, "Final resource base: {}", doc.base_url);

  let worker_name = name.clone();
  let mut render_opts = shared.base_options.clone();
  let has_page_rule = doc.html.to_ascii_lowercase().contains("@page");
  if has_page_rule {
    render_opts.fit_canvas_to_content = Some(true);
    let _ = writeln!(log, "Detected @page rule; fitting canvas to content");
  }
  let doc_for_render = doc.clone();
  let render_request = shared.artifact_request;

  let run_render = {
    let render_pool = shared.render_pool.clone();
    let verbose = shared.verbose;
    let timeout_secs = shared.timeout_secs;
    move || -> Result<RenderOutcome, Status> {
      let render_work = move || -> Result<RenderOutcome, fastrender::Error> {
        let report = render_pool.with_renderer(|renderer| {
          render_document_with_artifacts(renderer, doc_for_render, &render_opts, render_request)
        })?;
        let png = encode_image(&report.pixmap, OutputFormat::Png)?;
        Ok(RenderOutcome {
          png,
          diagnostics: report.diagnostics,
          artifacts: report.artifacts,
        })
      };

      let (tx, rx) = channel();
      thread::Builder::new()
        .name(format!("render-pages-worker-{worker_name}"))
        .stack_size(RENDER_STACK_SIZE)
        .spawn(move || {
          let result = std::panic::catch_unwind(AssertUnwindSafe(render_work));
          let _ = tx.send(result);
        })
        .expect("spawn render worker");

      if let Some(secs) = timeout_secs {
        match rx.recv_timeout(Duration::from_secs(secs)) {
          Ok(Ok(outcome)) => {
            outcome.map_err(|e| Status::Error(format_error_with_chain(&e, verbose)))
          }
          Ok(Err(panic)) => Err(Status::Crash(panic_to_string(panic))),
          Err(RecvTimeoutError::Timeout) => {
            Err(Status::Timeout(format!("render timed out after {}s", secs)))
          }
          Err(RecvTimeoutError::Disconnected) => {
            Err(Status::Crash("render worker disconnected".to_string()))
          }
        }
      } else {
        match rx.recv() {
          Ok(Ok(outcome)) => {
            outcome.map_err(|e| Status::Error(format_error_with_chain(&e, verbose)))
          }
          Ok(Err(panic)) => Err(Status::Crash(panic_to_string(panic))),
          Err(_) => Err(Status::Crash("render worker disconnected".to_string())),
        }
      }
    }
  };

  let result = run_render();

  let elapsed = page_start.elapsed();
  let time_ms = elapsed.as_millis();

  let mut captured_artifacts: Option<RenderArtifacts> = None;
  let mut diagnostics = fastrender::RenderDiagnostics::default();

  let (status, size) = match result {
    Ok(outcome) => {
      diagnostics = outcome.diagnostics;
      log_diagnostics(&diagnostics, |line| {
        let _ = writeln!(log, "{line}");
      });

      let size = outcome.png.len();
      let _ = writeln!(log, "PNG size: {} bytes", size);
      let _ = writeln!(log, "Time: {}ms", time_ms);
      log.push_str("Status: OK\n");

      if let Err(e) = fs::write(&output_path, &outcome.png) {
        let _ = writeln!(log, "Write error: {}", e);
        (Status::Error(format!("write: {}", e)), None)
      } else {
        println!("✓ {} ({}b, {}ms)", name, size, time_ms);
        captured_artifacts = Some(outcome.artifacts);
        (Status::Ok, Some(size))
      }
    }
    Err(status) => match status {
      Status::Error(msg) => {
        let _ = writeln!(log, "Time: {}ms", time_ms);
        log.push_str("Status: ERROR\n");
        let _ = writeln!(log, "Error: {}", msg);
        println!("✗ {} ERROR: {} ({}ms)", name, msg, time_ms);
        (Status::Error(msg), None)
      }
      Status::Crash(msg) => {
        let _ = writeln!(log, "Time: {}ms", time_ms);
        log.push_str("Status: CRASH\n");
        let _ = writeln!(log, "Panic: {}", msg);

        let short: String = msg.chars().take(50).collect();
        println!("✗ {} CRASH: {} ({}ms)", name, short, time_ms);
        (Status::Crash(msg), None)
      }
      Status::Timeout(msg) => {
        let _ = writeln!(log, "Time: {}ms", time_ms);
        log.push_str("Status: TIMEOUT\n");
        let _ = writeln!(log, "Timeout: {}", msg);
        println!("✗ {} TIMEOUT: {} ({}ms)", name, msg, time_ms);
        (Status::Timeout(msg), None)
      }
      Status::Ok => (Status::Error("unexpected ok status".to_string()), None),
    },
  };

  let should_dump =
    shared.dump_mode != DumpMode::None && (!shared.only_failures || !matches!(status, Status::Ok));

  let summary = captured_artifacts.as_ref().map(build_intermediate_summary);

  if shared.diagnostics_json {
    let diag_path = diagnostics_path_for(&name);
    let diag_report = DiagnosticsFile {
      page: name.clone(),
      status: status_label(&status).to_string(),
      error: status_error(&status).map(str::to_string),
      time_ms,
      png_size: size,
      diagnostics: diagnostics.clone(),
      summary: summary.clone(),
    };
    write_stage_json(diag_path, &diag_report, &mut log);
  }

  if should_dump {
    if let Some(artifacts) = captured_artifacts.as_ref() {
      match shared.dump_mode {
        DumpMode::None => {}
        DumpMode::Summary => {
          if let Some(summary) = summary.as_ref() {
            let summary_path =
              PathBuf::from(RENDER_DIR).join(format!("{}.intermediate.json", name));
            write_stage_json(summary_path, summary, &mut log);
          } else {
            let _ = writeln!(log, "Summary requested but no artifacts captured");
          }
        }
        DumpMode::Full => {
          if let Some(summary) = summary.as_ref() {
            let summary_path =
              PathBuf::from(RENDER_DIR).join(format!("{}.intermediate.json", name));
            write_stage_json(summary_path, summary, &mut log);
          }
          write_full_artifacts(Path::new(RENDER_DIR), &name, artifacts, &mut log);
          write_pipeline_snapshot(Path::new(RENDER_DIR), &name, artifacts, &mut log);
        }
      }
    } else {
      let _ = writeln!(log, "Artifacts requested but not captured for {}", name);
    }
  }

  // Write per-page log
  let _ = fs::write(&log_path, &log);

  PageResult {
    name,
    status,
    time_ms,
    size,
  }
}

fn run_in_process(
  args: &Args,
  entries: &[CachedEntry],
  soft_timeout_ms: Option<u64>,
) -> io::Result<Vec<PageResult>> {
  let shared = build_render_shared(
    args,
    args.jobs,
    Some(args.timeout),
    Some(args.timeout),
    soft_timeout_ms,
  );
  let results: Mutex<Vec<PageResult>> = Mutex::new(Vec::new());

  // Use a thread pool with limited concurrency
  let thread_pool = rayon::ThreadPoolBuilder::new()
    .num_threads(args.jobs)
    .build()
    .expect("create thread pool");

  thread_pool.scope(|s| {
    for entry in entries {
      let shared = shared.clone();
      let entry = entry.clone();
      let results = &results;
      s.spawn(move |_| {
        let result = render_entry(&shared, &entry);
        results.lock().unwrap().push(result);
      });
    }
  });

  let mut results = results.into_inner().unwrap();
  results.sort_by(|a, b| a.name.cmp(&b.name));
  Ok(results)
}

fn spawn_worker(
  exe: &Path,
  args: &Args,
  entry: &CachedEntry,
  soft_timeout_ms: Option<u64>,
) -> io::Result<Child> {
  let mut cmd = Command::new(exe);
  cmd
    .arg("worker")
    .arg("--cache-path")
    .arg(&entry.path)
    .arg("--stem")
    .arg(&entry.stem)
    .arg("--cache-stem")
    .arg(&entry.cache_stem);

  apply_worker_common_args(
    &mut cmd,
    &WorkerCommonArgs {
      timeout: args.timeout,
      soft_timeout_ms,
      viewport: args.viewport,
      dpr: args.dpr,
      scroll: Some((args.scroll_x, args.scroll_y)),
      user_agent: &args.user_agent,
      accept_language: &args.accept_language,
      no_http_freshness: args.no_http_freshness,
      css_limit: args.css_limit,
      resource_access: &args.resource_access,
      layout_parallel: &args.layout_parallel,
      compat: &args.compat,
    },
  );
  cmd
    .arg("--disk-cache-max-bytes")
    .arg(args.disk_cache.max_bytes.to_string())
    .arg("--disk-cache-max-age-secs")
    .arg(args.disk_cache.max_age_secs.to_string());
  if args.diagnostics_json {
    cmd.arg("--diagnostics-json");
  }
  if args.dump_intermediate != DumpMode::None {
    cmd
      .arg("--dump-intermediate")
      .arg(format!("{:?}", args.dump_intermediate).to_ascii_lowercase());
  }
  if args.only_failures {
    cmd.arg("--only-failures");
  }
  if args.verbose {
    cmd.arg("--verbose");
  }
  if args.timings {
    cmd.arg("--timings");
  }

  configure_worker_stdio(&mut cmd, &stderr_path_for(&entry.stem))?;

  cmd.spawn()
}

fn read_worker_result(stem: &str) -> Option<PageResult> {
  let path = result_path_for(stem);
  let raw = fs::read_to_string(&path).ok()?;
  let parsed: WorkerResult = serde_json::from_str(&raw).ok()?;
  Some(parsed.into_page_result(stem.to_string()))
}

fn write_worker_result(stem: &str, result: &PageResult) -> io::Result<()> {
  let path = result_path_for(stem);
  if let Some(parent) = path.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent)?;
    }
  }
  let json = serde_json::to_string(&WorkerResult::from_page_result(result))
    .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
  fs::write(path, json)
}

fn synthesize_worker_failure_note(summary: ExitStatusSummary) -> String {
  format!(
    "worker exited (exit {}) without writing results",
    format_exit_status(summary)
  )
}

fn write_timeout_artifacts(stem: &str, message: &str, elapsed: Duration, diagnostics_json: bool) {
  let mut log = format!(
    "=== {stem} ===\nStatus: TIMEOUT\nKilled after {:.2}s\n{message}\n",
    elapsed.as_secs_f64()
  );
  let _ = fs::write(log_path_for(stem), &log);
  if diagnostics_json {
    let diag = DiagnosticsFile {
      page: stem.to_string(),
      status: "timeout".to_string(),
      error: Some(message.to_string()),
      time_ms: elapsed.as_millis(),
      png_size: None,
      diagnostics: fastrender::RenderDiagnostics::default(),
      summary: None,
    };
    write_stage_json(diagnostics_path_for(stem), &diag, &mut log);
    let _ = fs::write(log_path_for(stem), &log);
  }
}

fn print_page_status(result: &PageResult) {
  match &result.status {
    Status::Ok => {
      if let Some(size) = result.size {
        println!("✓ {} ({}b, {}ms)", result.name, size, result.time_ms);
      } else {
        println!("✓ {} ({}ms)", result.name, result.time_ms);
      }
    }
    Status::Error(msg) => {
      println!("✗ {} ERROR: {} ({}ms)", result.name, msg, result.time_ms);
    }
    Status::Crash(msg) => {
      let short: String = msg.chars().take(50).collect();
      println!("✗ {} CRASH: {} ({}ms)", result.name, short, result.time_ms);
    }
    Status::Timeout(msg) => {
      println!("✗ {} TIMEOUT: {} ({}ms)", result.name, msg, result.time_ms);
    }
  }
}

fn run_workers(
  args: &Args,
  entries: Vec<CachedEntry>,
  hard_timeout: Duration,
  soft_timeout_ms: Option<u64>,
) -> io::Result<Vec<PageResult>> {
  let exe = std::env::current_exe()?;
  let mut queue: VecDeque<CachedEntry> = VecDeque::from(entries);
  let mut running: Vec<RunningChild> = Vec::new();
  let mut results: Vec<PageResult> = Vec::new();

  while !queue.is_empty() || !running.is_empty() {
    while running.len() < args.jobs {
      let Some(entry) = queue.pop_front() else {
        break;
      };
      let _ = fs::remove_file(result_path_for(&entry.stem));
      let _ = fs::remove_file(stderr_path_for(&entry.stem));
      let child = spawn_worker(&exe, args, &entry, soft_timeout_ms)?;
      running.push(RunningChild {
        entry,
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
        append_timeout_stderr_note(&stderr_path_for(&entry.entry.stem), elapsed);
        let message = format!("hard timeout after {:.2}s", hard_timeout.as_secs_f64());
        write_timeout_artifacts(&entry.entry.stem, &message, elapsed, args.diagnostics_json);
        let result = PageResult {
          name: entry.entry.stem.clone(),
          status: Status::Timeout(message),
          time_ms: elapsed.as_millis(),
          size: None,
        };
        print_page_status(&result);
        results.push(result);
        continue;
      }

      match running[i].child.try_wait() {
        Ok(Some(status)) => {
          let entry = running.swap_remove(i);
          if let Some(result) = read_worker_result(&entry.entry.stem) {
            print_page_status(&result);
            results.push(result);
          } else {
            let summary = summarize_exit_status(&status);
            let message = synthesize_worker_failure_note(summary);
            let time_ms = entry.started.elapsed().as_millis();
            let mut log = format!("=== {} ===\nStatus: CRASH\n{}\n", entry.entry.stem, message);
            let _ = fs::write(log_path_for(&entry.entry.stem), &log);
            if args.diagnostics_json {
              let diag = DiagnosticsFile {
                page: entry.entry.stem.clone(),
                status: "crash".to_string(),
                error: Some(message.clone()),
                time_ms,
                png_size: None,
                diagnostics: fastrender::RenderDiagnostics::default(),
                summary: None,
              };
              write_stage_json(diagnostics_path_for(&entry.entry.stem), &diag, &mut log);
              let _ = fs::write(log_path_for(&entry.entry.stem), &log);
            }
            let result = PageResult {
              name: entry.entry.stem.clone(),
              status: Status::Crash(message),
              time_ms,
              size: None,
            };
            print_page_status(&result);
            results.push(result);
          }
        }
        Ok(None) => {
          i += 1;
        }
        Err(_) => {
          let entry = running.swap_remove(i);
          let message = "worker try_wait failed".to_string();
          let time_ms = entry.started.elapsed().as_millis();
          let mut log = format!("=== {} ===\nStatus: CRASH\n{}\n", entry.entry.stem, message);
          let _ = fs::write(log_path_for(&entry.entry.stem), &log);
          if args.diagnostics_json {
            let diag = DiagnosticsFile {
              page: entry.entry.stem.clone(),
              status: "crash".to_string(),
              error: Some(message.clone()),
              time_ms,
              png_size: None,
              diagnostics: fastrender::RenderDiagnostics::default(),
              summary: None,
            };
            write_stage_json(diagnostics_path_for(&entry.entry.stem), &diag, &mut log);
            let _ = fs::write(log_path_for(&entry.entry.stem), &log);
          }
          let result = PageResult {
            name: entry.entry.stem.clone(),
            status: Status::Crash(message),
            time_ms,
            size: None,
          };
          print_page_status(&result);
          results.push(result);
        }
      }
    }

    std::thread::sleep(Duration::from_millis(20));
  }

  results.sort_by(|a, b| a.name.cmp(&b.name));
  Ok(results)
}

fn worker_main(worker_args: WorkerArgs) -> io::Result<()> {
  let args = worker_args.args;
  let entry = CachedEntry {
    path: worker_args.cache_path,
    stem: worker_args.stem,
    cache_stem: worker_args.cache_stem,
  };

  let hard_timeout = Duration::from_secs(args.timeout);
  let soft_timeout_ms = compute_soft_timeout_ms(hard_timeout, args.soft_timeout_ms);
  let shared = build_render_shared(&args, 1, None, Some(args.timeout), soft_timeout_ms);

  fs::create_dir_all(RENDER_DIR)?;
  let _ = fs::create_dir_all(ASSET_DIR);

  let page_result = render_entry(&shared, &entry);

  write_worker_result(&entry.stem, &page_result)
}

const TEXT_PREVIEW: usize = 160;

#[derive(Debug, Clone, Serialize)]
struct DiagnosticsFile {
  page: String,
  status: String,
  error: Option<String>,
  time_ms: u128,
  png_size: Option<usize>,
  diagnostics: fastrender::RenderDiagnostics,
  summary: Option<IntermediateSummary>,
}

#[derive(Debug, Clone, Serialize, Default)]
struct IntermediateSummary {
  dom_nodes: Option<usize>,
  styled_nodes: Option<usize>,
  boxes: Option<usize>,
  text_boxes: Option<usize>,
  fragments: Option<usize>,
  text_fragments: Option<usize>,
  display_items: Option<usize>,
  warnings: Vec<String>,
}

#[derive(Debug, Serialize)]
struct SerializableDomNode {
  kind: String,
  namespace: Option<String>,
  tag: Option<String>,
  attributes: Option<Vec<(String, String)>>,
  text: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  quirks_mode: Option<String>,
  children: Vec<SerializableDomNode>,
}

#[derive(Debug, Serialize)]
struct SerializableStyledNode {
  node_id: usize,
  tag: Option<String>,
  id: Option<String>,
  classes: Vec<String>,
  display: String,
  children: Vec<SerializableStyledNode>,
}

#[derive(Debug, Serialize)]
struct SerializableBoxNode {
  id: usize,
  kind: String,
  display: String,
  debug: Option<String>,
  text: Option<String>,
  children: Vec<SerializableBoxNode>,
}

#[derive(Debug, Serialize)]
struct SerializableFragmentTree {
  root: SerializableFragmentNode,
  additional: Vec<SerializableFragmentNode>,
}

#[derive(Debug, Serialize)]
struct SerializableFragmentNode {
  kind: String,
  bounds: SerializableRect,
  text: Option<String>,
  style: Option<String>,
  children: Vec<SerializableFragmentNode>,
}

#[derive(Debug, Serialize, Clone, Copy)]
struct SerializableRect {
  x: f32,
  y: f32,
  width: f32,
  height: f32,
}

#[derive(Debug, Serialize)]
struct SerializableDisplayList {
  items: Vec<SerializableDisplayItem>,
}

#[derive(Debug, Serialize)]
struct SerializableDisplayItem {
  kind: String,
  bounds: Option<SerializableRect>,
}

fn status_label(status: &Status) -> &'static str {
  match status {
    Status::Ok => "ok",
    Status::Crash(_) => "crash",
    Status::Error(_) => "error",
    Status::Timeout(_) => "timeout",
  }
}

fn status_error(status: &Status) -> Option<&str> {
  match status {
    Status::Crash(msg) | Status::Error(msg) | Status::Timeout(msg) => Some(msg.as_str()),
    Status::Ok => None,
  }
}

fn build_intermediate_summary(artifacts: &RenderArtifacts) -> IntermediateSummary {
  let mut summary = IntermediateSummary::default();
  summary.dom_nodes = artifacts.dom.as_ref().map(count_dom_nodes);
  summary.styled_nodes = artifacts.styled_tree.as_ref().map(count_styled_nodes);
  summary.boxes = artifacts.box_tree.as_ref().map(BoxTree::count_boxes);
  summary.text_boxes = artifacts.box_tree.as_ref().map(BoxTree::count_text_boxes);
  summary.fragments = artifacts
    .fragment_tree
    .as_ref()
    .map(FragmentTree::fragment_count);
  summary.text_fragments = artifacts.fragment_tree.as_ref().map(|tree| {
    let mut total = count_text_fragments(&tree.root);
    for extra in &tree.additional_fragments {
      total += count_text_fragments(extra);
    }
    total
  });
  summary.display_items = artifacts.display_list.as_ref().map(DisplayList::len);

  if matches!(summary.fragments, Some(0)) {
    summary.warnings.push("fragment_tree_empty".to_string());
  }
  if matches!(summary.display_items, Some(0)) {
    summary.warnings.push("display_list_empty".to_string());
  }

  summary
}

fn count_dom_nodes(node: &DomNode) -> usize {
  1 + node.children.iter().map(count_dom_nodes).sum::<usize>()
}

fn count_styled_nodes(node: &StyledNode) -> usize {
  1 + node.children.iter().map(count_styled_nodes).sum::<usize>()
}

fn count_text_fragments(node: &FragmentNode) -> usize {
  let self_count = match node.content {
    FragmentContent::Text { .. } => 1,
    _ => 0,
  };
  self_count
    + node
      .children
      .iter()
      .map(count_text_fragments)
      .sum::<usize>()
}

fn write_stage_json(path: PathBuf, value: &impl Serialize, log: &mut String) {
  match serde_json::to_string_pretty(value) {
    Ok(s) => {
      if let Err(err) = fs::write(&path, s) {
        let _ = writeln!(log, "Failed to write {}: {}", path.display(), err);
      }
    }
    Err(err) => {
      let _ = writeln!(log, "Failed to serialize {}: {}", path.display(), err);
    }
  }
}

fn write_full_artifacts(base: &Path, name: &str, artifacts: &RenderArtifacts, log: &mut String) {
  if let Some(dom) = &artifacts.dom {
    let serializable = serialize_dom(dom);
    write_stage_json(base.join(format!("{}.dom.json", name)), &serializable, log);
  } else {
    let _ = writeln!(log, "Missing DOM artifact for {}", name);
  }

  if let Some(styled) = &artifacts.styled_tree {
    write_stage_json(
      base.join(format!("{}.styled.json", name)),
      &serialize_styled(styled),
      log,
    );
  } else {
    let _ = writeln!(log, "Missing styled tree for {}", name);
  }

  if let Some(box_tree) = &artifacts.box_tree {
    write_stage_json(
      base.join(format!("{}.boxes.json", name)),
      &serialize_box_node(&box_tree.root),
      log,
    );
  } else {
    let _ = writeln!(log, "Missing box tree for {}", name);
  }

  if let Some(fragment_tree) = &artifacts.fragment_tree {
    let serializable = SerializableFragmentTree {
      root: serialize_fragment_node(&fragment_tree.root),
      additional: fragment_tree
        .additional_fragments
        .iter()
        .map(serialize_fragment_node)
        .collect(),
    };
    write_stage_json(
      base.join(format!("{}.fragments.json", name)),
      &serializable,
      log,
    );
  } else {
    let _ = writeln!(log, "Missing fragment tree for {}", name);
  }

  if let Some(list) = &artifacts.display_list {
    write_stage_json(
      base.join(format!("{}.display_list.json", name)),
      &serialize_display_list(list),
      log,
    );
  } else {
    let _ = writeln!(log, "Missing display list for {}", name);
  }
}

fn write_pipeline_snapshot(base: &Path, name: &str, artifacts: &RenderArtifacts, log: &mut String) {
  let Some(dom) = artifacts.dom.as_ref() else {
    let _ = writeln!(log, "Missing DOM artifact for {}", name);
    return;
  };
  let Some(styled) = artifacts.styled_tree.as_ref() else {
    let _ = writeln!(log, "Missing styled tree for {}", name);
    return;
  };
  let Some(box_tree) = artifacts.box_tree.as_ref() else {
    let _ = writeln!(log, "Missing box tree for {}", name);
    return;
  };
  let Some(fragment_tree) = artifacts.fragment_tree.as_ref() else {
    let _ = writeln!(log, "Missing fragment tree for {}", name);
    return;
  };
  let Some(display_list) = artifacts.display_list.as_ref() else {
    let _ = writeln!(log, "Missing display list for {}", name);
    return;
  };

  let snapshot: PipelineSnapshot =
    snapshot_pipeline(dom, styled, box_tree, fragment_tree, display_list);
  let path = base.join(format!("{}.snapshot.json", name));
  let _ = writeln!(log, "Snapshot: {}", path.display());
  write_stage_json(path, &snapshot, log);
}

fn serialize_dom(node: &DomNode) -> SerializableDomNode {
  let (kind, namespace, tag, attributes, text, quirks_mode) = match &node.node_type {
    DomNodeType::Document { quirks_mode } => (
      "document".to_string(),
      None,
      None,
      None,
      None,
      Some(QuirksModeSnapshot::from(*quirks_mode).as_str().to_string()),
    ),
    DomNodeType::ShadowRoot { .. } => ("shadow-root".to_string(), None, None, None, None, None),
    DomNodeType::Slot {
      namespace,
      attributes,
      ..
    } => (
      "slot".to_string(),
      Some(namespace.clone()),
      None,
      Some(attributes.clone()),
      None,
      None,
    ),
    DomNodeType::Element {
      tag_name,
      namespace,
      attributes,
    } => (
      "element".to_string(),
      Some(namespace.clone()),
      Some(tag_name.clone()),
      Some(attributes.clone()),
      None,
      None,
    ),
    DomNodeType::Text { content } => (
      "text".to_string(),
      None,
      None,
      None,
      Some(truncate_text(content, TEXT_PREVIEW)),
      None,
    ),
  };

  SerializableDomNode {
    kind,
    namespace,
    tag,
    attributes,
    text,
    quirks_mode,
    children: node.children.iter().map(serialize_dom).collect(),
  }
}

fn element_metadata(node: &DomNode) -> (Option<String>, Option<String>, Vec<String>) {
  if let DomNodeType::Element {
    tag_name,
    attributes,
    ..
  } = &node.node_type
  {
    let mut id = None;
    let mut classes = Vec::new();
    for (name, value) in attributes {
      if name.eq_ignore_ascii_case("id") {
        id = Some(value.clone());
      } else if name.eq_ignore_ascii_case("class") {
        classes.extend(value.split_whitespace().map(|c| c.to_string()));
      }
    }
    (Some(tag_name.clone()), id, classes)
  } else {
    (None, None, Vec::new())
  }
}

fn serialize_styled(node: &StyledNode) -> SerializableStyledNode {
  let (tag, id, classes) = element_metadata(&node.node);
  SerializableStyledNode {
    node_id: node.node_id,
    tag,
    id,
    classes,
    display: format!("{:?}", node.styles.display),
    children: node.children.iter().map(serialize_styled).collect(),
  }
}

fn serialize_box_node(node: &BoxNode) -> SerializableBoxNode {
  let (kind, text) = match &node.box_type {
    BoxType::Block(_) => ("block".to_string(), None),
    BoxType::Inline(_) => ("inline".to_string(), None),
    BoxType::Text(t) => (
      "text".to_string(),
      Some(truncate_text(&t.text, TEXT_PREVIEW)),
    ),
    BoxType::Marker(marker) => {
      let payload = match &marker.content {
        fastrender::tree::box_tree::MarkerContent::Text(text) => truncate_text(text, TEXT_PREVIEW),
        fastrender::tree::box_tree::MarkerContent::Image(_) => "[image]".to_string(),
      };
      ("marker".to_string(), Some(payload))
    }
    BoxType::Replaced(_) => ("replaced".to_string(), None),
    BoxType::Anonymous(anon) => (format!("anonymous({:?})", anon.anonymous_type), None),
  };

  SerializableBoxNode {
    id: node.id,
    kind,
    display: format!("{:?}", node.style.display),
    debug: node.debug_info.as_ref().map(|d| d.to_selector()),
    text,
    children: node.children.iter().map(serialize_box_node).collect(),
  }
}

fn serialize_fragment_node(node: &FragmentNode) -> SerializableFragmentNode {
  let (kind, text) = match &node.content {
    FragmentContent::Block { .. } => ("block".to_string(), None),
    FragmentContent::Inline { .. } => ("inline".to_string(), None),
    FragmentContent::Text { text, .. } => {
      ("text".to_string(), Some(truncate_text(text, TEXT_PREVIEW)))
    }
    FragmentContent::Line { .. } => ("line".to_string(), None),
    FragmentContent::Replaced { .. } => ("replaced".to_string(), None),
    FragmentContent::RunningAnchor { .. } => ("running-anchor".to_string(), None),
  };
  let style = node
    .style
    .as_ref()
    .map(|style| format!("{:?}", style.display));

  SerializableFragmentNode {
    kind,
    bounds: SerializableRect {
      x: node.bounds.x(),
      y: node.bounds.y(),
      width: node.bounds.width(),
      height: node.bounds.height(),
    },
    text,
    style,
    children: node.children.iter().map(serialize_fragment_node).collect(),
  }
}

fn serialize_display_list(list: &DisplayList) -> SerializableDisplayList {
  SerializableDisplayList {
    items: list
      .items()
      .iter()
      .map(|item| SerializableDisplayItem {
        kind: display_item_name(item).to_string(),
        bounds: item.bounds().map(|r| SerializableRect {
          x: r.x(),
          y: r.y(),
          width: r.width(),
          height: r.height(),
        }),
      })
      .collect(),
  }
}

fn display_item_name(item: &DisplayItem) -> &'static str {
  match item {
    DisplayItem::FillRect(_) => "fill_rect",
    DisplayItem::StrokeRect(_) => "stroke_rect",
    DisplayItem::Outline(_) => "outline",
    DisplayItem::FillRoundedRect(_) => "fill_rounded_rect",
    DisplayItem::StrokeRoundedRect(_) => "stroke_rounded_rect",
    DisplayItem::Text(_) => "text",
    DisplayItem::Image(_) => "image",
    DisplayItem::BoxShadow(_) => "box_shadow",
    DisplayItem::ListMarker(_) => "list_marker",
    DisplayItem::LinearGradient(_) => "linear_gradient",
    DisplayItem::RadialGradient(_) => "radial_gradient",
    DisplayItem::ConicGradient(_) => "conic_gradient",
    DisplayItem::Border(_) => "border",
    DisplayItem::TableCollapsedBorders(_) => "table_collapsed_borders",
    DisplayItem::TextDecoration(_) => "text_decoration",
    DisplayItem::PushClip(_) => "push_clip",
    DisplayItem::PopClip => "pop_clip",
    DisplayItem::PushOpacity(_) => "push_opacity",
    DisplayItem::PopOpacity => "pop_opacity",
    DisplayItem::PushTransform(_) => "push_transform",
    DisplayItem::PopTransform => "pop_transform",
    DisplayItem::PushBlendMode(_) => "push_blend_mode",
    DisplayItem::PopBlendMode => "pop_blend_mode",
    DisplayItem::PushStackingContext(_) => "push_stacking_context",
    DisplayItem::PopStackingContext => "pop_stacking_context",
  }
}

fn truncate_text(text: &str, limit: usize) -> String {
  let mut out = String::new();
  for (idx, ch) in text.chars().enumerate() {
    if idx >= limit {
      out.push('…');
      break;
    }
    out.push(ch);
  }
  out
}

#[cfg(test)]
mod tests {
  use super::pageset_stem;

  #[test]
  fn normalize_page_name_strips_scheme_and_www() {
    assert_eq!(
      pageset_stem("https://example.com/foo").as_deref(),
      Some("example.com_foo")
    );
    assert_eq!(
      pageset_stem("http://www.example.com").as_deref(),
      Some("example.com")
    );
  }

  #[test]
  fn normalize_page_name_ignores_empty() {
    assert!(pageset_stem("").is_none());
    assert!(pageset_stem("   ").is_none());
  }

  #[test]
  fn normalize_page_name_strips_trailing_punctuation_and_whitespace() {
    assert_eq!(
      pageset_stem(" https://Example.com./path/ ").as_deref(),
      Some("example.com_path")
    );
  }
}
