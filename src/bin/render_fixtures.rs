//! Render offline fixtures under `tests/pages/fixtures/*` to PNGs.
//!
//! This binary is intended for deterministic, offline rendering of imported page fixtures.
//! Network access is denied via `ResourcePolicy` (http/https disabled) and the renderer defaults
//! to bundled fonts.

mod common;

use clap::Parser;
use common::args::{default_jobs, parse_shard, parse_viewport, MediaTypeArg};
use common::render_pipeline::{compute_soft_timeout_ms, format_error_with_chain, CLI_RENDER_STACK_SIZE};
use fastrender::api::{FastRenderPool, FastRenderPoolConfig, RenderArtifactRequest, RenderOptions};
use fastrender::image_output::{encode_image, OutputFormat};
use fastrender::resource::ResourcePolicy;
use fastrender::text::font_db::FontConfig;
use fastrender::{snapshot_pipeline, PipelineSnapshot, RenderArtifacts};
use serde::Serialize;
use std::collections::HashSet;
use std::fmt::Write as _;
use std::fs;
use std::io;
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};
use std::sync::mpsc::{channel, RecvTimeoutError};
use std::sync::Mutex;
use std::thread;
use std::time::{Duration, Instant};
use url::Url;

const DEFAULT_FIXTURES_DIR: &str = "tests/pages/fixtures";
const DEFAULT_OUT_DIR: &str = "target/fixture_renders";

#[derive(Parser, Debug, Clone)]
#[command(name = "render_fixtures", version, about)]
struct Cli {
  /// Directory containing fixture subdirectories.
  ///
  /// Each fixture is a directory containing an `index.html` entrypoint.
  #[arg(long, default_value = DEFAULT_FIXTURES_DIR)]
  fixtures_dir: PathBuf,

  /// Output directory for PNG renders and logs.
  #[arg(long, default_value = DEFAULT_OUT_DIR)]
  out_dir: PathBuf,

  /// Render only a subset of fixtures (comma-separated stems).
  #[arg(long, value_delimiter = ',')]
  fixtures: Option<Vec<String>>,

  /// Process only a deterministic shard of fixtures (index/total, 0-based).
  #[arg(long, value_parser = parse_shard)]
  shard: Option<(usize, usize)>,

  /// Number of parallel fixture renders.
  #[arg(long, short, default_value_t = default_jobs())]
  jobs: usize,

  /// Viewport size as WxH (e.g., 1200x800)
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset.
  #[arg(long, default_value = "1.0")]
  dpr: f32,

  /// Media type for evaluating media queries.
  #[arg(long, value_enum, default_value_t = MediaTypeArg::Screen)]
  media: MediaTypeArg,

  /// Hard per-fixture timeout in seconds.
  #[arg(long, default_value_t = 10)]
  timeout: u64,

  /// Also write `<out-dir>/<stem>/snapshot.json` + diagnostics for later `diff_snapshots`.
  #[arg(long)]
  write_snapshot: bool,

  /// Additional deterministic font directories to load (can be repeated).
  #[arg(long, value_name = "DIR")]
  font_dir: Vec<PathBuf>,
}

#[derive(Clone)]
struct FixtureEntry {
  stem: String,
  index_path: PathBuf,
}

#[derive(Clone)]
struct RenderShared {
  render_pool: FastRenderPool,
  base_options: RenderOptions,
  hard_timeout: Duration,
  write_snapshot: bool,
  out_dir: PathBuf,
}

enum Status {
  Ok,
  Crash(String),
  Error(String),
  Timeout(String),
}

struct FixtureResult {
  stem: String,
  status: Status,
  time_ms: u128,
  size: Option<usize>,
}

struct RenderOutcome {
  png: Vec<u8>,
  diagnostics: fastrender::RenderDiagnostics,
  artifacts: RenderArtifacts,
}

#[derive(Debug, Serialize)]
struct DiagnosticsFile {
  fixture: String,
  status: String,
  error: Option<String>,
  time_ms: u128,
  png_size: Option<usize>,
  diagnostics: fastrender::RenderDiagnostics,
}

fn main() {
  let cli = Cli::parse();
  if let Err(err) = run(cli) {
    eprintln!("{err}");
    std::process::exit(1);
  }
}

fn run(cli: Cli) -> io::Result<()> {
  if cli.jobs == 0 {
    return Err(io::Error::new(io::ErrorKind::InvalidInput, "jobs must be > 0"));
  }
  if cli.timeout == 0 {
    return Err(io::Error::new(
      io::ErrorKind::InvalidInput,
      "timeout must be > 0",
    ));
  }
  if !cli.dpr.is_finite() || cli.dpr <= 0.0 {
    return Err(io::Error::new(
      io::ErrorKind::InvalidInput,
      "dpr must be a finite number > 0",
    ));
  }

  fs::create_dir_all(&cli.out_dir)?;

  let mut fixtures = discover_fixtures(&cli.fixtures_dir)?;
  if fixtures.is_empty() {
    return Err(io::Error::new(
      io::ErrorKind::NotFound,
      format!(
        "No fixtures found under {} (expected directories containing index.html)",
        cli.fixtures_dir.display()
      ),
    ));
  }

  fixtures.sort_by(|a, b| a.stem.cmp(&b.stem));

  if let Some(selected) = &cli.fixtures {
    let wanted: HashSet<String> = selected.iter().map(|s| s.trim().to_string()).collect();
    let mut missing: Vec<String> = wanted
      .iter()
      .filter(|stem| !fixtures.iter().any(|f| &f.stem == *stem))
      .cloned()
      .collect();
    missing.sort();
    if !missing.is_empty() {
      return Err(io::Error::new(
        io::ErrorKind::NotFound,
        format!("Unknown fixtures: {}", missing.join(", ")),
      ));
    }
    fixtures.retain(|f| wanted.contains(&f.stem));
  }

  if let Some((idx, total)) = cli.shard {
    fixtures = fixtures
      .into_iter()
      .enumerate()
      .filter(|(i, _)| i % total == idx)
      .map(|(_, f)| f)
      .collect();
  }

  if fixtures.is_empty() {
    return Err(io::Error::new(
      io::ErrorKind::NotFound,
      "No fixtures selected after filtering/sharding",
    ));
  }

  let hard_timeout = Duration::from_secs(cli.timeout);
  let soft_timeout_ms = compute_soft_timeout_ms(hard_timeout, None);

  let font_config = {
    let mut config = FontConfig::bundled_only();
    if !cli.font_dir.is_empty() {
      config = config.with_font_dirs(cli.font_dir.clone());
    }
    config
  };

  let resource_policy = ResourcePolicy::default().allow_http(false).allow_https(false);
  let render_config = fastrender::api::FastRenderConfig::new()
    .with_default_viewport(cli.viewport.0, cli.viewport.1)
    .with_device_pixel_ratio(cli.dpr)
    .with_meta_viewport(true)
    .with_resource_policy(resource_policy)
    .with_font_sources(font_config);

  let render_pool = FastRenderPool::with_config(
    FastRenderPoolConfig::new()
      .with_renderer_config(render_config)
      .with_pool_size(cli.jobs),
  )
  .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

  let mut base_options = RenderOptions::new()
    .with_viewport(cli.viewport.0, cli.viewport.1)
    .with_device_pixel_ratio(cli.dpr)
    .with_media_type(cli.media.as_media_type());
  if let Some(ms) = soft_timeout_ms {
    if ms > 0 {
      base_options.timeout = Some(Duration::from_millis(ms));
    }
  }

  let shared = RenderShared {
    render_pool,
    base_options,
    hard_timeout,
    write_snapshot: cli.write_snapshot,
    out_dir: cli.out_dir.clone(),
  };

  println!(
    "Rendering {} fixtures ({} parallel) to {}",
    fixtures.len(),
    cli.jobs,
    cli.out_dir.display()
  );
  if let Some((idx, total)) = cli.shard {
    println!("Shard: {idx}/{total}");
  }
  println!(
    "Viewport: {}x{} dpr={} media={:?} timeout={}s",
    cli.viewport.0,
    cli.viewport.1,
    cli.dpr,
    cli.media.as_media_type(),
    cli.timeout
  );
  println!();

  let start = Instant::now();
  let results: Mutex<Vec<FixtureResult>> = Mutex::new(Vec::new());
  let thread_pool = rayon::ThreadPoolBuilder::new()
    .num_threads(cli.jobs)
    .build()
    .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

  thread_pool.scope(|s| {
    for entry in fixtures {
      let shared = shared.clone();
      let results = &results;
      s.spawn(move |_| {
        let result = render_fixture(&shared, &entry);
        results.lock().unwrap().push(result);
      });
    }
  });

  let mut results = results.into_inner().unwrap();
  results.sort_by(|a, b| a.stem.cmp(&b.stem));

  let total_elapsed = start.elapsed();

  let pass = results.iter().filter(|r| matches!(r.status, Status::Ok)).count();
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

  let summary_path = cli.out_dir.join("_summary.log");
  let mut summary = String::new();
  let _ = writeln!(summary, "=== Fixture Render Summary ===");
  let _ = writeln!(summary, "Total time: {:.1}s", total_elapsed.as_secs_f64());
  let _ = writeln!(
    summary,
    "Fixtures: {} total, {} pass, {} timeout, {} crash, {} error\n",
    results.len(),
    pass,
    timeout,
    crash,
    error
  );
  let _ = writeln!(summary, "{:<40} {:>8} {:>10} STATUS", "FIXTURE", "TIME", "SIZE");
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
      .map(|s| format!("{s}b"))
      .unwrap_or_else(|| "-".to_string());
    let _ = writeln!(
      summary,
      "{:<40} {:>6}ms {:>10} {}",
      r.stem, r.time_ms, size_str, status_str
    );
  }
  let _ = writeln!(summary, "\n{}", "-".repeat(75));
  let _ = writeln!(summary, "Total: {:.1}s", total_elapsed.as_secs_f64());
  let _ = fs::write(&summary_path, &summary);

  println!();
  println!(
    "Done in {:.1}s: ✓{} pass, ⏱{} timeout, ✗{} crash, ✗{} error",
    total_elapsed.as_secs_f64(),
    pass,
    timeout,
    crash,
    error
  );
  println!("Summary: {}", summary_path.display());
  println!("Renders:  {}/<fixture>.png", cli.out_dir.display());
  println!("Logs:     {}/<fixture>.log", cli.out_dir.display());
  if cli.write_snapshot {
    println!("Snapshots:{}/<fixture>/snapshot.json", cli.out_dir.display());
  }

  if timeout > 0 || crash > 0 || error > 0 {
    std::process::exit(1);
  }

  Ok(())
}

fn discover_fixtures(fixtures_dir: &Path) -> io::Result<Vec<FixtureEntry>> {
  let mut fixtures = Vec::new();
  for entry in fs::read_dir(fixtures_dir)? {
    let entry = entry?;
    let path = entry.path();
    if !path.is_dir() {
      continue;
    }
    let index_path = path.join("index.html");
    if !index_path.is_file() {
      continue;
    }
    let stem = entry.file_name().to_string_lossy().into_owned();
    fixtures.push(FixtureEntry { stem, index_path });
  }
  Ok(fixtures)
}

fn log_path_for(out_dir: &Path, stem: &str) -> PathBuf {
  out_dir.join(format!("{stem}.log"))
}

fn output_path_for(out_dir: &Path, stem: &str) -> PathBuf {
  out_dir.join(format!("{stem}.png"))
}

fn snapshot_dir_for(out_dir: &Path, stem: &str) -> PathBuf {
  out_dir.join(stem)
}

fn panic_to_string(panic: Box<dyn std::any::Any + Send + 'static>) -> String {
  panic
    .downcast_ref::<&str>()
    .map(|s| s.to_string())
    .or_else(|| panic.downcast_ref::<String>().cloned())
    .unwrap_or_else(|| "unknown panic".to_string())
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

fn render_fixture(shared: &RenderShared, entry: &FixtureEntry) -> FixtureResult {
  let stem = entry.stem.clone();
  let log_path = log_path_for(&shared.out_dir, &stem);
  let output_path = output_path_for(&shared.out_dir, &stem);
  let snapshot_dir = snapshot_dir_for(&shared.out_dir, &stem);

  let mut log = format!("=== {stem} ===\n");
  let _ = writeln!(log, "Entrypoint: {}", entry.index_path.display());
  let _ = writeln!(log, "Viewport: {}x{}", shared.base_options.viewport.unwrap().0, shared.base_options.viewport.unwrap().1);
  let _ = writeln!(log, "DPR: {}", shared.base_options.device_pixel_ratio.unwrap_or(1.0));

  let base_url = match canonical_file_url(&entry.index_path) {
    Ok(url) => url,
    Err(err) => {
      let _ = writeln!(log, "Base URL error: {err}");
      let _ = fs::write(&log_path, log);
      return FixtureResult {
        stem,
        status: Status::Error(format!("base_url: {err}")),
        time_ms: 0,
        size: None,
      };
    }
  };
  let _ = writeln!(log, "Base URL: {base_url}");

  let html = match fs::read_to_string(&entry.index_path) {
    Ok(html) => html,
    Err(err) => {
      let _ = writeln!(log, "Read error: {err}");
      let _ = fs::write(&log_path, log);
      return FixtureResult {
        stem,
        status: Status::Error(format!("read: {err}")),
        time_ms: 0,
        size: None,
      };
    }
  };

  let page_start = Instant::now();

  let render_pool = shared.render_pool.clone();
  let options = shared.base_options.clone();
  let artifact_request = if shared.write_snapshot {
    RenderArtifactRequest::summary()
  } else {
    RenderArtifactRequest::none()
  };

  let render_work = move || -> Result<RenderOutcome, fastrender::Error> {
    let report = render_pool.with_renderer(|renderer| {
      renderer.render_html_with_stylesheets_report(&html, &base_url, options, artifact_request)
    })?;
    let png = encode_image(&report.pixmap, OutputFormat::Png)?;
    Ok(RenderOutcome {
      png,
      diagnostics: report.diagnostics,
      artifacts: report.artifacts,
    })
  };

  let (tx, rx) = channel();
  let worker_name = stem.clone();
  thread::Builder::new()
    .name(format!("render-fixtures-worker-{worker_name}"))
    .stack_size(CLI_RENDER_STACK_SIZE)
    .spawn(move || {
      let result = std::panic::catch_unwind(AssertUnwindSafe(render_work));
      let _ = tx.send(result);
    })
    .expect("spawn render worker");

  let result = match rx.recv_timeout(shared.hard_timeout) {
    Ok(Ok(outcome)) => outcome.map_err(|e| Status::Error(format_error_with_chain(&e, false))),
    Ok(Err(panic)) => Err(Status::Crash(panic_to_string(panic))),
    Err(RecvTimeoutError::Timeout) => Err(Status::Timeout(format!(
      "render timed out after {:.2}s",
      shared.hard_timeout.as_secs_f64()
    ))),
    Err(RecvTimeoutError::Disconnected) => Err(Status::Crash("render worker disconnected".to_string())),
  };

  let elapsed = page_start.elapsed();
  let time_ms = elapsed.as_millis();

  let mut captured_artifacts: Option<RenderArtifacts> = None;
  let mut diagnostics = fastrender::RenderDiagnostics::default();

  let (status, size) = match result {
    Ok(outcome) => {
      diagnostics = outcome.diagnostics;
      common::render_pipeline::log_diagnostics(&diagnostics, |line| {
        let _ = writeln!(log, "{line}");
      });

      let mut status = Status::Ok;

      let blocked_urls = blocked_network_urls(&diagnostics);
      if !blocked_urls.is_empty() {
        status = Status::Error(format!(
          "blocked http/https resources: {}",
          blocked_urls.join(", ")
        ));
      }

      let size = outcome.png.len();
      let _ = writeln!(log, "PNG size: {size} bytes");
      let _ = writeln!(log, "Time: {time_ms}ms");
      match &status {
        Status::Ok => {
          log.push_str("Status: OK\n");
        }
        Status::Error(msg) => {
          log.push_str("Status: ERROR\n");
          let _ = writeln!(log, "Error: {msg}");
        }
        Status::Crash(_) | Status::Timeout(_) => {}
      }

      if let Err(err) = fs::write(&output_path, &outcome.png) {
        let _ = writeln!(log, "Write error: {err}");
        (Status::Error(format!("write: {err}")), None)
      } else {
        captured_artifacts = Some(outcome.artifacts);
        (status, Some(size))
      }
    }
    Err(status) => {
      let _ = writeln!(log, "Time: {time_ms}ms");
      match &status {
        Status::Error(msg) => {
          let _ = writeln!(log, "Status: ERROR");
          let _ = writeln!(log, "Error: {msg}");
        }
        Status::Crash(msg) => {
          let _ = writeln!(log, "Status: CRASH");
          let _ = writeln!(log, "Panic: {msg}");
        }
        Status::Timeout(msg) => {
          let _ = writeln!(log, "Status: TIMEOUT");
          let _ = writeln!(log, "Timeout: {msg}");
        }
        Status::Ok => {}
      }
      (status, None)
    }
  };

  if shared.write_snapshot {
    if let Some(artifacts) = captured_artifacts.as_ref() {
      if let Err(err) = write_snapshot_outputs(&snapshot_dir, artifacts, &mut log) {
        let _ = writeln!(log, "Snapshot write error: {err}");
      }
    } else {
      let _ = writeln!(log, "Snapshot requested but artifacts were not captured");
    }
  }

  let diag_report = DiagnosticsFile {
    fixture: stem.clone(),
    status: status_label(&status).to_string(),
    error: status_error(&status).map(str::to_string),
    time_ms,
    png_size: size,
    diagnostics: diagnostics.clone(),
  };
  let _ = write_diagnostics_file(&snapshot_dir, &diag_report, &mut log, shared.write_snapshot);

  let _ = fs::write(&log_path, &log);

  match &status {
    Status::Ok => {
      if let Some(size) = size {
        println!("✓ {stem} ({size}b, {time_ms}ms)");
      } else {
        println!("✓ {stem} ({time_ms}ms)");
      }
    }
    Status::Error(msg) => println!("✗ {stem} ERROR: {msg} ({time_ms}ms)"),
    Status::Crash(msg) => {
      let short: String = msg.chars().take(50).collect();
      println!("✗ {stem} CRASH: {short} ({time_ms}ms)");
    }
    Status::Timeout(msg) => println!("✗ {stem} TIMEOUT: {msg} ({time_ms}ms)"),
  }

  FixtureResult {
    stem,
    status,
    time_ms,
    size,
  }
}

fn blocked_network_urls(diagnostics: &fastrender::RenderDiagnostics) -> Vec<String> {
  let mut seen = HashSet::<String>::new();
  for entry in diagnostics
    .fetch_errors
    .iter()
    .chain(diagnostics.blocked_fetch_errors.iter())
  {
    if entry.url.starts_with("http://") || entry.url.starts_with("https://") {
      seen.insert(entry.url.clone());
    }
  }
  let mut urls: Vec<String> = seen.into_iter().collect();
  urls.sort();
  urls
}

fn canonical_file_url(path: &Path) -> io::Result<String> {
  let abs = fs::canonicalize(path)?;
  let url = Url::from_file_path(&abs)
    .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "invalid file path for URL"))?;
  Ok(url.to_string())
}

fn write_snapshot_outputs(
  dir: &Path,
  artifacts: &RenderArtifacts,
  log: &mut String,
) -> io::Result<()> {
  fs::create_dir_all(dir)?;

  let snapshot = build_snapshot(artifacts)?;
  let snapshot_path = dir.join("snapshot.json");
  let _ = writeln!(log, "Snapshot: {}", snapshot_path.display());
  write_json_pretty(&snapshot_path, &snapshot, log)?;

  Ok(())
}

fn build_snapshot(artifacts: &RenderArtifacts) -> io::Result<PipelineSnapshot> {
  let dom = artifacts
    .dom
    .as_ref()
    .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "missing DOM artifact"))?;
  let styled = artifacts
    .styled_tree
    .as_ref()
    .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "missing styled tree artifact"))?;
  let box_tree = artifacts
    .box_tree
    .as_ref()
    .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "missing box tree artifact"))?;
  let fragment_tree = artifacts
    .fragment_tree
    .as_ref()
    .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "missing fragment tree artifact"))?;
  let display_list = artifacts
    .display_list
    .as_ref()
    .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "missing display list artifact"))?;
  Ok(snapshot_pipeline(
    dom,
    styled,
    box_tree,
    fragment_tree,
    display_list,
  ))
}

fn write_diagnostics_file(
  snapshot_dir: &Path,
  diag: &DiagnosticsFile,
  log: &mut String,
  enabled: bool,
) -> io::Result<()> {
  if !enabled {
    return Ok(());
  }
  fs::create_dir_all(snapshot_dir)?;
  let path = snapshot_dir.join("diagnostics.json");
  write_json_pretty(&path, diag, log)
}

fn write_json_pretty(path: &Path, value: &impl Serialize, log: &mut String) -> io::Result<()> {
  let json = serde_json::to_string_pretty(value)
    .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
  fs::write(path, json).map_err(|e| {
    let _ = writeln!(log, "Failed to write {}: {e}", path.display());
    e
  })
}
