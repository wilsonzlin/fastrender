//! Fetch a single page and render it to an image.
//!
//! Examples:
//!   fetch_and_render --timeout 120 --viewport 1200x800 --dpr 2.0 https://www.example.com output.png

#![allow(clippy::io_other_error)]
#![allow(clippy::redundant_closure)]
#![allow(clippy::len_zero)]
#![allow(clippy::items_after_test_module)]

mod common;

use clap::Parser;
use common::args::{
  AllowPartialArgs, BaseUrlArgs, MediaArgs, OutputFormatArgs, TimeoutArgs, ViewportArgs,
};
use common::media_prefs::MediaPreferences;
use common::render_pipeline::{
  build_http_fetcher, build_render_configs, build_renderer_with_fetcher, decode_html_resource,
  follow_client_redirects, format_error_with_chain, log_diagnostics, read_cached_document,
  render_document, RenderConfigBundle, RenderSurface,
};
use fastrender::image_output::encode_image;
use fastrender::resource::normalize_user_agent_for_log;
use fastrender::resource::url_to_filename;
#[cfg(not(feature = "disk_cache"))]
use fastrender::resource::CachingFetcher;
#[cfg(feature = "disk_cache")]
use fastrender::resource::DiskCachingFetcher;
use fastrender::resource::ResourceFetcher;
use fastrender::resource::DEFAULT_ACCEPT_LANGUAGE;
use fastrender::resource::DEFAULT_USER_AGENT;
use fastrender::OutputFormat;
use fastrender::Result;
use std::error::Error;
use std::path::{Path, PathBuf};
use std::sync::mpsc::channel;
use std::sync::mpsc::RecvTimeoutError;
use std::sync::Arc;
use std::thread;
use std::time::Duration;

const STACK_SIZE: usize = 64 * 1024 * 1024; // 64MB to avoid stack overflows on large pages
#[cfg(feature = "disk_cache")]
const ASSET_CACHE_DIR: &str = "fetches/assets";

/// Fetch a single page and render it to an image
#[derive(Parser, Debug)]
#[command(name = "fetch_and_render", version, about)]
struct Args {
  /// URL to fetch and render
  url: String,

  /// Output file path (defaults to <url>.<format>); parent directories are created automatically
  output: Option<String>,

  /// Viewport width (deprecated, use --viewport)
  #[arg(hide = true)]
  width: Option<u32>,

  /// Viewport height (deprecated, use --viewport)
  #[arg(hide = true)]
  height: Option<u32>,

  /// Scroll X offset (deprecated, use --scroll-x)
  #[arg(hide = true)]
  scroll_x_pos: Option<u32>,

  /// Scroll Y offset (deprecated, use --scroll-y)
  #[arg(hide = true)]
  scroll_y_pos: Option<u32>,

  #[command(flatten)]
  timeout: TimeoutArgs,

  #[command(flatten)]
  surface: ViewportArgs,

  /// Horizontal scroll offset in CSS px
  #[arg(long, default_value = "0")]
  scroll_x: u32,

  /// Vertical scroll offset in CSS px
  #[arg(long, default_value = "0")]
  scroll_y: u32,

  #[command(flatten)]
  media: MediaArgs,

  #[command(flatten)]
  output_format: OutputFormatArgs,

  #[command(flatten)]
  base_url: BaseUrlArgs,

  #[command(flatten)]
  allow_partial: AllowPartialArgs,

  /// Expand render target to full content size
  #[arg(long)]
  full_page: bool,

  /// Override the User-Agent header
  #[arg(long, default_value = DEFAULT_USER_AGENT)]
  user_agent: String,

  /// Override the Accept-Language header
  #[arg(long, default_value = DEFAULT_ACCEPT_LANGUAGE)]
  accept_language: String,

  /// Maximum number of external stylesheets to fetch
  #[arg(long)]
  css_limit: Option<usize>,

  /// Allow HTTP(S) documents to load file:// subresources
  #[arg(long)]
  allow_file_from_http: bool,

  /// Block mixed HTTP subresources when rendering HTTPS documents
  #[arg(long)]
  block_mixed_content: bool,

  /// Enable per-stage timing logs
  #[arg(long)]
  timings: bool,

  /// Write a Chrome trace to this path
  #[arg(long, value_name = "PATH")]
  trace_out: Option<PathBuf>,

  /// Print full error chains on failure
  #[arg(long)]
  verbose: bool,
}

fn render_page(
  url: &str,
  output: &Path,
  timeout_secs: Option<u64>,
  bundle: RenderConfigBundle,
  user_agent: &str,
  accept_language: &str,
  output_format: OutputFormat,
  base_url_override: Option<String>,
) -> Result<()> {
  let http = build_http_fetcher(user_agent, accept_language, timeout_secs);
  #[cfg(feature = "disk_cache")]
  let fetcher: Arc<dyn ResourceFetcher> = Arc::new(DiskCachingFetcher::new(http, ASSET_CACHE_DIR));
  #[cfg(not(feature = "disk_cache"))]
  let fetcher: Arc<dyn ResourceFetcher> = Arc::new(CachingFetcher::new(http));

  let mut renderer = build_renderer_with_fetcher(bundle.config, fetcher.clone())?;
  let mut log = |line: &str| println!("{line}");

  let prepared = if url.starts_with("file://") {
    let path = url.strip_prefix("file://").unwrap_or(url);
    let cached = read_cached_document(Path::new(path))?;
    follow_client_redirects(fetcher.as_ref(), cached.document, &mut log)
  } else {
    println!("Fetching HTML from: {url}");
    let resource = fetcher.fetch(url)?;
    let base_hint = resource.final_url.as_deref().unwrap_or(url).to_string();
    let doc = decode_html_resource(&resource, &base_hint);
    follow_client_redirects(fetcher.as_ref(), doc, &mut log)
  };
  let prepared = prepared.with_base_override(base_url_override.as_deref());

  let render_result = render_document(&mut renderer, prepared, &bundle.options)?;
  log_diagnostics(&render_result.diagnostics, &mut log);

  let image_data = encode_image(&render_result.pixmap, output_format)?;
  std::fs::write(output, &image_data)?;

  println!("✓ Successfully rendered {url} to {}", output.display());
  println!("  Image size: {} bytes", image_data.len());
  Ok(())
}

fn try_main(args: Args) -> Result<()> {
  let media_prefs = MediaPreferences::from(&args.media.prefs);
  media_prefs.apply_env();
  if args.full_page {
    std::env::set_var("FASTR_FULL_PAGE", "1");
  }
  if args.timings {
    std::env::set_var("FASTR_RENDER_TIMINGS", "1");
  }

  // Resolve dimensions from viewport or deprecated positional args
  let (viewport_w, viewport_h) = args.surface.viewport;
  let width = args.width.unwrap_or(viewport_w);
  let height = args.height.unwrap_or(viewport_h);
  let scroll_x = args.scroll_x_pos.unwrap_or(args.scroll_x) as f32;
  let scroll_y = args.scroll_y_pos.unwrap_or(args.scroll_y) as f32;

  let output_ext = args.output_format.extension();
  let output_format = args.output_format.output_format();
  let output = args
    .output
    .unwrap_or_else(|| format!("{}.{}", url_to_filename(&args.url), output_ext));

  let output_path = Path::new(&output);
  if let Some(parent) = output_path.parent() {
    if !parent.as_os_str().is_empty() {
      std::fs::create_dir_all(parent)?;
    }
  }

  let timeout_secs = args.timeout.seconds(Some(0));

  eprintln!(
    "User-Agent: {}\nAccept-Language: {}\nViewport: {}x{} @{}x, scroll ({}, {})\nOutput: {} ({})",
    normalize_user_agent_for_log(&args.user_agent),
    args.accept_language,
    width,
    height,
    args.surface.dpr,
    scroll_x,
    scroll_y,
    output,
    output_ext
  );

  let RenderConfigBundle { config, options } = build_render_configs(&RenderSurface {
    viewport: (width, height),
    scroll_x,
    scroll_y,
    dpr: args.surface.dpr,
    media_type: args.media.media_type(),
    css_limit: args.css_limit,
    allow_partial: args.allow_partial.allow_partial,
    apply_meta_viewport: true,
    base_url: args.base_url.base_url.clone(),
    allow_file_from_http: args.allow_file_from_http,
    block_mixed_content: args.block_mixed_content,
    trace_output: args.trace_out.clone(),
  });

  let (tx, rx) = channel();
  let url_clone = args.url.clone();
  let output_path = output_path.to_path_buf();
  let user_agent = args.user_agent.clone();
  let accept_language = args.accept_language.clone();
  let base_url_override = args.base_url.base_url.clone();
  let output_format = output_format;

  thread::Builder::new()
    .name("fetch_and_render-worker".to_string())
    .stack_size(STACK_SIZE)
    .spawn(move || {
      let res = render_page(
        &url_clone,
        &output_path,
        timeout_secs,
        RenderConfigBundle { config, options },
        &user_agent,
        &accept_language,
        output_format,
        base_url_override,
      );
      let _ = tx.send(res);
    })
    .expect("spawn render worker");

  if let Some(secs) = timeout_secs {
    match rx.recv_timeout(Duration::from_secs(secs)) {
      Ok(res) => res,
      Err(RecvTimeoutError::Timeout) => {
        eprintln!("Render timed out after {secs} seconds");
        std::process::exit(1);
      }
      Err(RecvTimeoutError::Disconnected) => {
        eprintln!("Render worker exited unexpectedly");
        std::process::exit(1);
      }
    }
  } else {
    match rx.recv() {
      Ok(res) => res,
      Err(_) => {
        eprintln!("Render worker exited unexpectedly");
        std::process::exit(1);
      }
    }
  }
}

fn main() {
  let args = Args::parse();
  let verbose = args.verbose;
  if let Err(err) = try_main(args) {
    eprintln!("{}", format_error_with_chain(&err, verbose));
    if !verbose && err.source().is_some() {
      eprintln!("note: re-run with --verbose to see full error context");
    }
    std::process::exit(1);
  }
}
