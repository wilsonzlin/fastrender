mod common;

use clap::Parser;
use common::args::parse_viewport;
use fastrender::api::{FastRender, RenderOptions};
use fastrender::html::encoding::decode_html_bytes;
use fastrender::resource::{
  HttpFetcher, ResourceFetcher, DEFAULT_ACCEPT_LANGUAGE, DEFAULT_USER_AGENT,
};
use fastrender::style::media::MediaType;
use std::error::Error;
use std::fs;
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;
use url::Url;

/// Dump the accessibility tree for an HTML document as JSON.
#[derive(Parser, Debug)]
#[command(name = "dump_a11y", version, about)]
struct Args {
  /// HTML file or URL to inspect
  input: String,

  /// Viewport size as WxH (e.g., 1200x800)
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset
  #[arg(long, default_value = "1.0")]
  dpr: f32,

  /// Output compact JSON instead of pretty-printing.
  #[arg(long)]
  compact: bool,

  /// Override the User-Agent header
  #[arg(long, default_value = DEFAULT_USER_AGENT)]
  user_agent: String,

  /// Override the Accept-Language header
  #[arg(long, default_value = DEFAULT_ACCEPT_LANGUAGE)]
  accept_language: String,

  /// Abort after this many seconds
  #[arg(long)]
  timeout: Option<u64>,
}

fn main() -> Result<(), Box<dyn Error>> {
  // Avoid panicking on SIGPIPE/BrokenPipe when piped through tools like `head`.
  let default_hook = std::panic::take_hook();
  std::panic::set_hook(Box::new(move |info| {
    let mut msg = info.to_string();
    if let Some(s) = info.payload().downcast_ref::<&str>() {
      msg = (*s).to_string();
    } else if let Some(s) = info.payload().downcast_ref::<String>() {
      msg = s.clone();
    }
    if msg.contains("Broken pipe") {
      // Exit silently with success for broken pipe to mirror common CLI behavior.
      std::process::exit(0);
    }
    default_hook(info);
  }));

  let args = Args::parse();

  if let Some(sec) = args.timeout {
    std::thread::spawn(move || {
      std::thread::sleep(Duration::from_secs(sec));
      eprintln!("dump_a11y: timed out after {}s", sec);
      std::process::exit(1);
    });
  }

  let mut http_fetcher = HttpFetcher::new()
    .with_user_agent(args.user_agent.clone())
    .with_accept_language(args.accept_language.clone());
  if let Some(secs) = args.timeout {
    http_fetcher = http_fetcher.with_timeout(Duration::from_secs(secs));
  }
  let fetcher = Arc::new(http_fetcher);

  let (html, base_hint) = load_html(&args.input, fetcher.as_ref())?;

  let mut renderer = FastRender::builder()
    .device_pixel_ratio(args.dpr)
    .fetcher(Arc::clone(&fetcher))
    .build()?;
  renderer.set_base_url(base_hint.clone());

  let options = RenderOptions::new()
    .with_viewport(args.viewport.0, args.viewport.1)
    .with_device_pixel_ratio(args.dpr)
    .with_media_type(MediaType::Screen);
  let tree = renderer.accessibility_tree_html(&html, options)?;
  let json = serde_json::to_value(tree)?;

  if args.compact {
    println!("{}", serde_json::to_string(&json)?);
  } else {
    println!("{}", serde_json::to_string_pretty(&json)?);
  }

  Ok(())
}

fn load_html(
  input: &str,
  fetcher: &dyn ResourceFetcher,
) -> Result<(String, String), Box<dyn Error>> {
  if let Ok(url) = Url::parse(input) {
    if url.scheme() == "file" {
      let path_buf = url.to_file_path().map_err(|_| {
        std::io::Error::new(std::io::ErrorKind::InvalidInput, "invalid file:// path")
      })?;
      let bytes = fs::read(&path_buf)?;
      let html = decode_html_bytes(&bytes, None);
      return Ok((html, url.to_string()));
    }

    let resource = fetcher.fetch(url.as_str())?;
    let base_hint = resource
      .final_url
      .as_deref()
      .unwrap_or_else(|| url.as_str())
      .to_string();
    let html = decode_html_bytes(&resource.bytes, resource.content_type.as_deref());
    return Ok((html, base_hint));
  }

  let path = Path::new(input);
  let bytes = fs::read(path)?;
  let html = decode_html_bytes(&bytes, None);
  let base_hint = Url::from_file_path(path)
    .map(|u| u.to_string())
    .unwrap_or_else(|_| format!("file://{}", input));

  Ok((html, base_hint))
}
