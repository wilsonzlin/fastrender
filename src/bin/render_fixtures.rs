//! Render offline page fixtures to PNGs (deterministic, no network).
//!
//! This binary renders fixtures under `tests/pages/fixtures/*` (or a custom root)
//! by reading `<fixture>/index.html`, resolving subresources against `file://` URLs,
//! and writing `<out-dir>/<fixture>.png`.
//!
//! The renderer is configured to be deterministic by default:
//! - bundled fonts only (no system font discovery)
//! - offline resource policy (file:/data: only; no http(s))
//!
//! Successful renders must not attempt to fetch blocked resources; any subresource
//! fetch errors are treated as fixture failures.

use clap::{Parser, ValueEnum};
use fastrender::api::{DiagnosticsLevel, FastRenderConfig, FastRenderPool, FastRenderPoolConfig};
use fastrender::api::{RenderOptions, ResourceFetchError};
use fastrender::image_output::encode_image;
use fastrender::resource::ResourcePolicy;
use fastrender::style::media::MediaType;
use fastrender::text::font_db::FontConfig;
use fastrender::OutputFormat;
use rayon::ThreadPoolBuilder;
use std::collections::BTreeSet;
use std::fs;
use std::io;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::{Duration, Instant};
use url::Url;

/// Stack size for worker threads running fixture renders.
///
/// This matches `common::render_pipeline::CLI_RENDER_STACK_SIZE` used by other CLI tools,
/// but we keep it local here to avoid pulling in the full `src/bin/common` module tree.
const CLI_RENDER_STACK_SIZE: usize = 128 * 1024 * 1024; // 128MB

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
enum MediaArg {
  Screen,
  Print,
}

impl MediaArg {
  fn as_media_type(self) -> MediaType {
    match self {
      MediaArg::Screen => MediaType::Screen,
      MediaArg::Print => MediaType::Print,
    }
  }
}

/// Render offline page fixtures to PNGs.
#[derive(Parser, Debug, Clone)]
#[command(name = "render_fixtures", version, about)]
struct Args {
  /// Root directory containing fixture subdirectories.
  #[arg(long, default_value = "tests/pages/fixtures")]
  fixtures_root: PathBuf,

  /// Output directory for rendered PNGs.
  #[arg(long, default_value = "target/fixture_renders")]
  out_dir: PathBuf,

  /// Number of parallel renders.
  #[arg(long, short = 'j', default_value_t = fastrender::system::cpu_budget())]
  jobs: usize,

  /// Viewport size as WxH (e.g., 1200x800).
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset.
  #[arg(long, default_value = "1.0")]
  dpr: f32,

  /// Media type for evaluating media queries.
  #[arg(long, value_enum, default_value_t = MediaArg::Screen)]
  media: MediaArg,

  /// Hard per-fixture render timeout in seconds (cooperative).
  #[arg(long, default_value_t = 5)]
  timeout: u64,

  /// Write structured diagnostics alongside renders.
  #[arg(long)]
  diagnostics_json: bool,

  /// Render only the listed fixtures (comma-separated).
  #[arg(long, value_delimiter = ',')]
  only: Vec<String>,

  /// Positional fixture filters (fixture directory names).
  #[arg(trailing_var_arg = true)]
  filter_fixtures: Vec<String>,
}

#[derive(Clone, Debug)]
struct FixtureTask {
  name: String,
  dir_path: PathBuf,
  index_path: PathBuf,
}

#[derive(Debug)]
enum FixtureStatus {
  Ok { png_bytes: usize },
  Error { message: String },
  Crash { message: String },
}

#[derive(Debug)]
struct FixtureResult {
  name: String,
  elapsed_ms: u128,
  status: FixtureStatus,
}

#[derive(Clone)]
struct Shared {
  render_pool: FastRenderPool,
  options: RenderOptions,
  out_dir: PathBuf,
  diagnostics_json: bool,
}

fn main() {
  let args = Args::parse();
  let code = match run(args) {
    Ok(()) => 0,
    Err(code) => code,
  };
  std::process::exit(code);
}

fn run(args: Args) -> Result<(), i32> {
  if args.jobs == 0 {
    eprintln!("jobs must be > 0");
    return Err(2);
  }
  if args.timeout == 0 {
    eprintln!("timeout must be > 0");
    return Err(2);
  }
  if args.viewport.0 == 0 || args.viewport.1 == 0 {
    eprintln!("viewport width and height must be > 0");
    return Err(2);
  }
  if !args.dpr.is_finite() || args.dpr <= 0.0 {
    eprintln!("dpr must be a positive number");
    return Err(2);
  }

  fs::create_dir_all(&args.out_dir).map_err(|err| {
    eprintln!("failed to create out-dir {}: {err}", args.out_dir.display());
    1
  })?;

  let fixtures = select_fixtures(&args).map_err(|err| {
    eprintln!("{err}");
    1
  })?;
  if fixtures.is_empty() {
    eprintln!(
      "no fixtures found under {} (expected <fixture>/index.html)",
      args.fixtures_root.display()
    );
    return Err(1);
  }

  let render_pool = build_render_pool(&args).map_err(|err| {
    eprintln!("failed to initialize renderer: {err}");
    1
  })?;
  let options = build_render_options(&args);

  let shared = Shared {
    render_pool,
    options,
    out_dir: args.out_dir.clone(),
    diagnostics_json: args.diagnostics_json,
  };

  println!(
    "Rendering {} fixture(s) ({} parallel) to {}",
    fixtures.len(),
    args.jobs,
    args.out_dir.display()
  );
  println!(
    "Viewport: {}x{}  DPR: {}  Media: {:?}  Timeout: {}s",
    args.viewport.0, args.viewport.1, args.dpr, args.media, args.timeout
  );
  println!();

  let results: Mutex<Vec<FixtureResult>> = Mutex::new(Vec::with_capacity(fixtures.len()));
  let pool = ThreadPoolBuilder::new()
    .num_threads(args.jobs)
    .stack_size(CLI_RENDER_STACK_SIZE)
    .build()
    .map_err(|err| {
      eprintln!("failed to create thread pool: {err}");
      1
    })?;

  pool.scope(|scope| {
    for fixture in fixtures {
      let shared = shared.clone();
      let results = &results;
      scope.spawn(move |_| {
        let result = render_fixture(shared, fixture);
        results.lock().unwrap().push(result);
      });
    }
  });

  let mut results = results.into_inner().unwrap_or_default();
  results.sort_by(|a, b| a.name.cmp(&b.name));

  let mut ok = 0usize;
  let mut failed = 0usize;
  for result in &results {
    match &result.status {
      FixtureStatus::Ok { png_bytes } => {
        ok += 1;
        println!(
          "OK  {:<40} {:>6}ms  {}b",
          result.name, result.elapsed_ms, png_bytes
        );
      }
      FixtureStatus::Error { message } => {
        failed += 1;
        eprintln!(
          "ERR {:<40} {:>6}ms  {message}",
          result.name, result.elapsed_ms
        );
      }
      FixtureStatus::Crash { message } => {
        failed += 1;
        eprintln!(
          "CRASH {:<38} {:>6}ms  {message}",
          result.name, result.elapsed_ms
        );
      }
    }
  }

  println!();
  println!("Done: {} ok, {} failed", ok, failed);
  if failed > 0 {
    Err(1)
  } else {
    Ok(())
  }
}

fn select_fixtures(args: &Args) -> io::Result<Vec<FixtureTask>> {
  let mut selected: BTreeSet<String> = BTreeSet::new();
  for name in args.only.iter().chain(args.filter_fixtures.iter()) {
    let trimmed = name.trim();
    if trimmed.is_empty() {
      continue;
    }
    // Keep filtering strict to avoid surprising path traversal.
    if trimmed.contains('/') || trimmed.contains('\\') {
      return Err(io::Error::new(
        io::ErrorKind::InvalidInput,
        format!("invalid fixture name (must be a directory name): {trimmed}"),
      ));
    }
    selected.insert(trimmed.to_string());
  }

  if selected.is_empty() {
    discover_all_fixtures(&args.fixtures_root)
  } else {
    Ok(
      selected
        .into_iter()
        .map(|name| {
          let dir_path = args.fixtures_root.join(&name);
          let index_path = dir_path.join("index.html");
          FixtureTask {
            name,
            dir_path,
            index_path,
          }
        })
        .collect(),
    )
  }
}

fn discover_all_fixtures(fixtures_root: &Path) -> io::Result<Vec<FixtureTask>> {
  let mut fixtures = Vec::new();
  for entry in fs::read_dir(fixtures_root)? {
    let entry = entry?;
    let path = entry.path();
    let file_type = entry.file_type()?;
    if !file_type.is_dir() {
      continue;
    }
    let Some(name) = path.file_name().and_then(|s| s.to_str()) else {
      continue;
    };
    let index_path = path.join("index.html");
    if !index_path.is_file() {
      continue;
    }
    fixtures.push(FixtureTask {
      name: name.to_string(),
      dir_path: path,
      index_path,
    });
  }
  fixtures.sort_by(|a, b| a.name.cmp(&b.name));
  Ok(fixtures)
}

fn build_render_pool(args: &Args) -> fastrender::Result<FastRenderPool> {
  let resource_policy = ResourcePolicy::default()
    .allow_http(false)
    .allow_https(false)
    .allow_file(true)
    .allow_data(true);

  let config = FastRenderConfig::new()
    .with_default_viewport(args.viewport.0, args.viewport.1)
    .with_device_pixel_ratio(args.dpr)
    .with_meta_viewport(true)
    .with_resource_policy(resource_policy)
    .with_font_sources(FontConfig::bundled_only());

  FastRenderPool::with_config(
    FastRenderPoolConfig::new()
      .with_renderer_config(config)
      .with_pool_size(args.jobs),
  )
}

fn build_render_options(args: &Args) -> RenderOptions {
  let mut options = RenderOptions::new()
    .with_viewport(args.viewport.0, args.viewport.1)
    .with_device_pixel_ratio(args.dpr)
    .with_media_type(args.media.as_media_type());
  options.timeout = Some(Duration::from_secs(args.timeout));
  if args.diagnostics_json {
    options = options.with_diagnostics_level(DiagnosticsLevel::Basic);
  }
  options
}

fn render_fixture(shared: Shared, fixture: FixtureTask) -> FixtureResult {
  let started = Instant::now();
  let name = fixture.name.clone();
  let result = catch_unwind(AssertUnwindSafe(|| render_fixture_inner(&shared, &fixture)));
  let status = match result {
    Ok(Ok(png_bytes)) => FixtureStatus::Ok { png_bytes },
    Ok(Err(message)) => FixtureStatus::Error { message },
    Err(panic) => FixtureStatus::Crash {
      message: panic_to_string(panic),
    },
  };

  FixtureResult {
    name,
    elapsed_ms: started.elapsed().as_millis(),
    status,
  }
}

fn render_fixture_inner(shared: &Shared, fixture: &FixtureTask) -> Result<usize, String> {
  let html = fs::read_to_string(&fixture.index_path)
    .map_err(|err| format!("failed to read {}: {err}", fixture.index_path.display()))?;

  let base_hint = file_url_for_dir(&fixture.dir_path)
    .map_err(|err| format!("failed to build file:// base URL for fixture: {err}"))?
    .to_string();

  let options = shared.options.clone();
  let render_result = shared
    .render_pool
    .with_renderer(|renderer| renderer.render_html_with_stylesheets(&html, &base_hint, options))
    .map_err(|err| err.to_string())?;

  if shared.diagnostics_json {
    let diag_path = diagnostics_path_for(&shared.out_dir, &fixture.name);
    let json = serde_json::to_string_pretty(&render_result.diagnostics)
      .map_err(|err| format!("failed to serialize diagnostics: {err}"))?;
    fs::write(&diag_path, json)
      .map_err(|err| format!("failed to write diagnostics {}: {err}", diag_path.display()))?;
  }

  if let Some(message) = disallowed_resource_message(&render_result.diagnostics.fetch_errors) {
    return Err(message);
  }
  if let Some(message) =
    disallowed_resource_message(&render_result.diagnostics.blocked_fetch_errors)
  {
    return Err(message);
  }

  let png_bytes = encode_image(&render_result.pixmap, OutputFormat::Png)
    .map_err(|err| format!("failed to encode PNG: {err}"))?;
  let out_path = output_path_for(&shared.out_dir, &fixture.name);
  fs::write(&out_path, &png_bytes)
    .map_err(|err| format!("failed to write PNG {}: {err}", out_path.display()))?;
  Ok(png_bytes.len())
}

fn disallowed_resource_message(errors: &[ResourceFetchError]) -> Option<String> {
  let first = errors.first()?;
  if first.url.starts_with("http://") || first.url.starts_with("https://") {
    Some(format!(
      "blocked http/https resource: {} ({})",
      first.url, first.message
    ))
  } else {
    Some(format!(
      "subresource fetch failed: {} ({})",
      first.url, first.message
    ))
  }
}

fn output_path_for(out_dir: &Path, fixture: &str) -> PathBuf {
  out_dir.join(format!("{fixture}.png"))
}

fn diagnostics_path_for(out_dir: &Path, fixture: &str) -> PathBuf {
  out_dir.join(format!("{fixture}.diagnostics.json"))
}

fn file_url_for_dir(dir: &Path) -> Result<Url, io::Error> {
  let canonical = fs::canonicalize(dir)?;
  Url::from_directory_path(&canonical).map_err(|()| {
    io::Error::new(
      io::ErrorKind::InvalidInput,
      format!("cannot convert {} to file:// URL", canonical.display()),
    )
  })
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

fn panic_to_string(panic: Box<dyn std::any::Any + Send + 'static>) -> String {
  panic
    .downcast_ref::<&str>()
    .map(|s| s.to_string())
    .or_else(|| panic.downcast_ref::<String>().cloned())
    .unwrap_or_else(|| "unknown panic".to_string())
}

#[cfg(test)]
mod tests {
  use super::parse_viewport;

  #[test]
  fn parse_viewport_values() {
    assert_eq!(parse_viewport("1200x800"), Ok((1200, 800)));
    assert!(parse_viewport("0x800").is_err());
    assert!(parse_viewport("800").is_err());
  }
}
