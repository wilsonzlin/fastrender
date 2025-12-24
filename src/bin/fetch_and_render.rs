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
use common::render_pipeline::{
  build_http_fetcher, build_render_configs, build_renderer_with_fetcher, decode_html_resource,
  follow_client_redirects, log_diagnostics, read_cached_document, render_document,
  RenderConfigBundle,
};
use fastrender::image_output::encode_image;
use fastrender::resource::normalize_user_agent_for_log;
use fastrender::resource::url_to_filename;
use fastrender::resource::ResourceFetcher;
use fastrender::resource::DEFAULT_ACCEPT_LANGUAGE;
use fastrender::resource::DEFAULT_USER_AGENT;
use fastrender::OutputFormat;
use fastrender::Result;
use std::path::Path;
use std::sync::mpsc::channel;
use std::sync::mpsc::RecvTimeoutError;
use std::sync::Arc;
use std::thread;
use std::time::Duration;

const STACK_SIZE: usize = 64 * 1024 * 1024; // 64MB to avoid stack overflows on large pages

/// Fetch a single page and render it to an image
#[derive(Parser, Debug)]
#[command(name = "fetch_and_render", version, about)]
struct Args {
  /// URL to fetch and render
  url: String,

  /// Output file path (defaults to <url>.png); parent directories are created automatically
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

  /// Per-page timeout in seconds (0 = no timeout)
  #[arg(long, default_value = "0")]
  timeout: u64,

  /// Viewport size as WxH (e.g., 1200x800)
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Horizontal scroll offset in CSS px
  #[arg(long, default_value = "0")]
  scroll_x: u32,

  /// Vertical scroll offset in CSS px
  #[arg(long, default_value = "0")]
  scroll_y: u32,

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

  /// Enable per-stage timing logs
  #[arg(long)]
  timings: bool,
}

fn parse_viewport(s: &str) -> std::result::Result<(u32, u32), String> {
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

fn parse_bool_preference(s: &str) -> std::result::Result<bool, String> {
  let v = s.trim().to_ascii_lowercase();
  if matches!(
    v.as_str(),
    "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
  ) {
    return Ok(true);
  }
  if matches!(
    v.as_str(),
    "0" | "false" | "no" | "off" | "none" | "no-preference"
  ) {
    return Ok(false);
  }
  Err(format!("invalid value: {s}"))
}

fn parse_contrast(s: &str) -> std::result::Result<String, String> {
  let v = s.trim().to_ascii_lowercase();
  match v.as_str() {
    "more" | "high" | "less" | "low" | "custom" | "forced" | "no-preference" => Ok(v),
    _ => Err(format!("invalid contrast value: {s}")),
  }
}

fn parse_color_scheme(s: &str) -> std::result::Result<String, String> {
  let v = s.trim().to_ascii_lowercase();
  match v.as_str() {
    "light" | "dark" | "no-preference" => Ok(v),
    _ => Err(format!("invalid color scheme: {s}")),
  }
}
fn set_media_environment(args: &Args) {
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

  if let Some(contrast) = args.prefers_contrast.clone() {
    std::env::set_var("FASTR_PREFERS_CONTRAST", contrast);
  }

  if let Some(color_scheme) = args.prefers_color_scheme.clone() {
    std::env::set_var("FASTR_PREFERS_COLOR_SCHEME", color_scheme);
  }

  if args.full_page {
    std::env::set_var("FASTR_FULL_PAGE", "1");
  }

  if args.timings {
    std::env::set_var("FASTR_RENDER_TIMINGS", "1");
  }
}

fn render_page(
  url: &str,
  output: &Path,
  timeout_secs: Option<u64>,
  bundle: RenderConfigBundle,
  user_agent: &str,
  accept_language: &str,
) -> Result<()> {
  let fetcher = Arc::new(build_http_fetcher(
    user_agent,
    accept_language,
    timeout_secs,
  ));

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

  let render_result = render_document(&mut renderer, prepared, &bundle.options)?;
  log_diagnostics(&render_result.diagnostics, &mut log);

  let png_data = encode_image(&render_result.pixmap, OutputFormat::Png)?;
  std::fs::write(output, &png_data)?;

  println!("✓ Successfully rendered {url} to {}", output.display());
  println!("  Image size: {} bytes", png_data.len());
  Ok(())
}

fn main() -> Result<()> {
  let args = Args::parse();

  set_media_environment(&args);

  // Resolve dimensions from viewport or deprecated positional args
  let (viewport_w, viewport_h) = args.viewport;
  let width = args.width.unwrap_or(viewport_w);
  let height = args.height.unwrap_or(viewport_h);
  let scroll_x = args.scroll_x_pos.unwrap_or(args.scroll_x) as f32;
  let scroll_y = args.scroll_y_pos.unwrap_or(args.scroll_y) as f32;

  let output = args
    .output
    .unwrap_or_else(|| format!("{}.png", url_to_filename(&args.url)));

  let output_path = Path::new(&output);
  if let Some(parent) = output_path.parent() {
    if !parent.as_os_str().is_empty() {
      std::fs::create_dir_all(parent)?;
    }
  }

  let timeout_secs = if args.timeout == 0 {
    None
  } else {
    Some(args.timeout)
  };

  eprintln!(
    "User-Agent: {}\nAccept-Language: {}\nViewport: {}x{} @{}x, scroll ({}, {})\nOutput: {}",
    normalize_user_agent_for_log(&args.user_agent),
    args.accept_language,
    width,
    height,
    args.dpr,
    scroll_x,
    scroll_y,
    output
  );

  let RenderConfigBundle { config, options } = build_render_configs(
    (width, height),
    scroll_x,
    scroll_y,
    args.dpr,
    args.css_limit,
    true,
  );

  let (tx, rx) = channel();
  let url_clone = args.url.clone();
  let output_path = output_path.to_path_buf();
  let user_agent = args.user_agent.clone();
  let accept_language = args.accept_language.clone();

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
