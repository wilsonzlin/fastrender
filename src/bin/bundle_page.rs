//! Capture and replay self-contained page bundles (HTML + assets).
#![allow(clippy::too_many_lines)]

mod common;

use clap::{ArgAction, Args, Parser, Subcommand};
use common::args::CompatArgs;
use common::render_pipeline::{
  build_http_fetcher, build_render_configs, build_renderer_with_fetcher, decode_html_resource,
  render_document, RenderConfigBundle, RenderSurface,
};
use fastrender::css::loader::resolve_href;
use fastrender::html::meta_refresh::{extract_js_location_redirect, extract_meta_refresh_url};
use fastrender::image_output::encode_image;
use fastrender::resource::bundle::{
  Bundle, BundleManifest, BundleRenderConfig, BundledDocument, BundledFetcher, BundledResourceInfo,
  BUNDLE_MANIFEST, BUNDLE_VERSION,
};
use fastrender::resource::{
  url_to_filename, FetchedResource, ResourceFetcher, DEFAULT_ACCEPT_LANGUAGE, DEFAULT_USER_AGENT,
};
use fastrender::style::media::MediaType;
use fastrender::{OutputFormat, Result};
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

#[derive(Parser, Debug)]
#[command(
  name = "bundle_page",
  version,
  about = "Bundle pages for offline replay"
)]
struct Cli {
  #[command(subcommand)]
  command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
  /// Fetch a page and its subresources into a deterministic bundle
  Fetch(FetchArgs),
  /// Render strictly from a bundle with no network access
  Render(RenderArgs),
}

#[derive(Args, Debug)]
struct FetchArgs {
  /// URL (or file:// path) to fetch and bundle
  url: String,

  /// Output bundle path (directory or .tar)
  #[arg(long)]
  out: String,

  /// Viewport size as WxH (e.g., 1200x800)
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries and srcset
  #[arg(long, default_value = "1.0")]
  dpr: f32,

  /// Horizontal scroll offset in CSS px
  #[arg(long, default_value = "0")]
  scroll_x: f32,

  /// Vertical scroll offset in CSS px
  #[arg(long, default_value = "0")]
  scroll_y: f32,

  /// Expand render target to the full document height
  #[arg(long, action = ArgAction::SetTrue)]
  full_page: bool,

  /// Restrict subresource loads to the document origin unless allowlisted.
  #[arg(long)]
  same_origin_subresources: bool,

  /// Allow additional origins when blocking cross-origin subresources (repeatable).
  #[arg(long, value_name = "ORIGIN")]
  allow_subresource_origin: Vec<String>,

  #[command(flatten)]
  compat: CompatArgs,
}

#[derive(Args, Debug)]
struct RenderArgs {
  /// Bundle to render (directory or .tar)
  bundle: String,

  /// Output PNG path
  #[arg(long)]
  out: String,

  /// Override viewport size (WxH)
  #[arg(long, value_parser = parse_viewport)]
  viewport: Option<(u32, u32)>,

  /// Override device pixel ratio
  #[arg(long)]
  dpr: Option<f32>,

  /// Override horizontal scroll offset
  #[arg(long)]
  scroll_x: Option<f32>,

  /// Override vertical scroll offset
  #[arg(long)]
  scroll_y: Option<f32>,

  /// Force full-page rendering (overrides bundle)
  #[arg(long, action = ArgAction::SetTrue)]
  full_page: bool,

  /// Disable full-page rendering even if the bundle captured it
  #[arg(long, action = ArgAction::SetTrue)]
  no_full_page: bool,

  /// Restrict subresource loads to the document origin unless allowlisted.
  #[arg(long)]
  same_origin_subresources: bool,

  /// Allow additional origins when blocking cross-origin subresources (repeatable).
  #[arg(long, value_name = "ORIGIN")]
  allow_subresource_origin: Vec<String>,

  #[command(flatten)]
  compat: CompatArgs,
}

#[derive(Clone)]
struct RecordingFetcher {
  inner: Arc<dyn ResourceFetcher>,
  recorded: Arc<Mutex<HashMap<String, FetchedResource>>>,
}

impl RecordingFetcher {
  fn new(inner: Arc<dyn ResourceFetcher>) -> Self {
    Self {
      inner,
      recorded: Arc::new(Mutex::new(HashMap::new())),
    }
  }

  fn snapshot(&self) -> HashMap<String, FetchedResource> {
    self
      .recorded
      .lock()
      .map(|map| map.clone())
      .unwrap_or_default()
  }
}

impl ResourceFetcher for RecordingFetcher {
  fn fetch(&self, url: &str) -> Result<FetchedResource> {
    if let Ok(map) = self.recorded.lock() {
      if let Some(existing) = map.get(url) {
        return Ok(existing.clone());
      }
    }

    let result = self.inner.fetch(url)?;
    if let Ok(mut map) = self.recorded.lock() {
      map.insert(url.to_string(), result.clone());
    }
    Ok(result)
  }

  fn fetch_with_validation(
    &self,
    url: &str,
    etag: Option<&str>,
    last_modified: Option<&str>,
  ) -> Result<FetchedResource> {
    if let Ok(map) = self.recorded.lock() {
      if let Some(existing) = map.get(url) {
        return Ok(existing.clone());
      }
    }

    let result = self.inner.fetch_with_validation(url, etag, last_modified)?;
    if let Ok(mut map) = self.recorded.lock() {
      map.insert(url.to_string(), result.clone());
    }
    Ok(result)
  }
}

struct ResourceEntry {
  info: BundledResourceInfo,
  bytes: Vec<u8>,
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

fn main() -> Result<()> {
  let cli = Cli::parse();
  match cli.command {
    Command::Fetch(args) => fetch_bundle(args),
    Command::Render(args) => render_bundle(args),
  }
}

fn fetch_bundle(args: FetchArgs) -> Result<()> {
  let out_path = PathBuf::from(&args.out);
  if out_path.exists() {
    return Err(fastrender::Error::Other(format!(
      "Output path already exists: {}",
      out_path.display()
    )));
  }

  let render = BundleRenderConfig {
    viewport: args.viewport,
    device_pixel_ratio: args.dpr,
    scroll_x: args.scroll_x,
    scroll_y: args.scroll_y,
    full_page: args.full_page,
    same_origin_subresources: args.same_origin_subresources,
    allowed_subresource_origins: args.allow_subresource_origin.clone(),
    compat_profile: args.compat.compat_profile(),
    dom_compat_mode: args.compat.dom_compat_mode(),
  };
  apply_full_page_env(render.full_page);

  let http = Arc::new(build_http_fetcher(
    DEFAULT_USER_AGENT,
    DEFAULT_ACCEPT_LANGUAGE,
    None,
  ));
  let recording = RecordingFetcher::new(http);
  let (prepared, document_resource) = fetch_document(&recording, &args.url)?;

  let RenderConfigBundle { config, options } = build_render_configs(&RenderSurface {
    viewport: render.viewport,
    scroll_x: render.scroll_x,
    scroll_y: render.scroll_y,
    dpr: render.device_pixel_ratio,
    media_type: MediaType::Screen,
    css_limit: None,
    allow_partial: false,
    apply_meta_viewport: true,
    base_url: None,
    allow_file_from_http: false,
    block_mixed_content: false,
    same_origin_subresources: render.same_origin_subresources,
    allowed_subresource_origins: render.allowed_subresource_origins.clone(),
    trace_output: None,
    layout_parallelism: None,
    font_config: None,
    compat_profile: render.compat_profile,
    dom_compat_mode: render.dom_compat_mode,
  });
  let fetcher: Arc<dyn ResourceFetcher> = Arc::new(recording.clone());
  let mut renderer = build_renderer_with_fetcher(config, fetcher)?;

  // Render once to ensure all subresources are fetched and cached.
  let _ = render_document(&mut renderer, prepared, &options)?;

  let recorded = recording.snapshot();
  let (manifest, resources, document_bytes) =
    build_manifest(args.url, render, document_resource, recorded);
  write_bundle(&out_path, &manifest, &resources, &document_bytes)?;

  println!(
    "✓ Captured {} resources into {}",
    manifest.resources.len(),
    out_path.display()
  );
  Ok(())
}

fn render_bundle(args: RenderArgs) -> Result<()> {
  let bundle = Bundle::load(&args.bundle)?;
  let manifest = bundle.manifest().clone();
  let (doc_meta_ref, doc_bytes) = bundle.document();
  let doc_meta = doc_meta_ref.clone();

  let mut render = manifest.render.clone();
  if let Some(viewport) = args.viewport {
    render.viewport = viewport;
  }
  if let Some(dpr) = args.dpr {
    render.device_pixel_ratio = dpr;
  }
  if let Some(scroll_x) = args.scroll_x {
    render.scroll_x = scroll_x;
  }
  if let Some(scroll_y) = args.scroll_y {
    render.scroll_y = scroll_y;
  }
  if args.full_page && args.no_full_page {
    return Err(fastrender::Error::Other(
      "Cannot combine --full-page and --no-full-page".to_string(),
    ));
  }
  if args.full_page {
    render.full_page = true;
  } else if args.no_full_page {
    render.full_page = false;
  }
  if args.same_origin_subresources {
    render.same_origin_subresources = true;
  }
  if !args.allow_subresource_origin.is_empty() {
    render.allowed_subresource_origins = args.allow_subresource_origin.clone();
  }
  if let Some(profile) = args.compat.compat_profile_arg() {
    render.compat_profile = profile.as_profile();
  }
  if let Some(mode) = args.compat.dom_compat_arg() {
    render.dom_compat_mode = mode.as_mode();
  }
  apply_full_page_env(render.full_page);

  let RenderConfigBundle { config, options } = build_render_configs(&RenderSurface {
    viewport: render.viewport,
    scroll_x: render.scroll_x,
    scroll_y: render.scroll_y,
    dpr: render.device_pixel_ratio,
    media_type: MediaType::Screen,
    css_limit: None,
    allow_partial: false,
    apply_meta_viewport: true,
    base_url: None,
    allow_file_from_http: false,
    block_mixed_content: false,
    same_origin_subresources: render.same_origin_subresources,
    allowed_subresource_origins: render.allowed_subresource_origins.clone(),
    trace_output: None,
    layout_parallelism: None,
    font_config: None,
    compat_profile: render.compat_profile,
    dom_compat_mode: render.dom_compat_mode,
  });

  let fetcher: Arc<dyn ResourceFetcher> = Arc::new(BundledFetcher::new(bundle));
  let mut renderer = build_renderer_with_fetcher(config, fetcher)?;

  let base_hint = if doc_meta.final_url.is_empty() {
    manifest.original_url.clone()
  } else {
    doc_meta.final_url.clone()
  };
  let mut doc_resource = FetchedResource::with_final_url(
    (*doc_bytes).clone(),
    doc_meta.content_type.clone(),
    Some(base_hint.clone()),
  );
  doc_resource.status = doc_meta.status;
  doc_resource.etag = doc_meta.etag.clone();
  doc_resource.last_modified = doc_meta.last_modified.clone();
  let prepared = decode_html_resource(&doc_resource, &base_hint);
  let result = render_document(&mut renderer, prepared, &options)?;
  let png = encode_image(&result.pixmap, OutputFormat::Png)?;

  let out_path = PathBuf::from(&args.out);
  if let Some(parent) = out_path.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent)?;
    }
  }
  fs::write(&out_path, png)?;
  println!("✓ Rendered bundle to {}", out_path.display());
  Ok(())
}

fn build_manifest(
  original_url: String,
  render: BundleRenderConfig,
  document_resource: FetchedResource,
  mut recorded: HashMap<String, FetchedResource>,
) -> (BundleManifest, Vec<ResourceEntry>, Vec<u8>) {
  let final_url = document_resource
    .final_url
    .clone()
    .unwrap_or_else(|| original_url.clone());
  recorded.remove(&original_url);
  recorded.remove(&final_url);

  let doc_ext = extension_for_resource(&document_resource, &original_url);
  let document_path = format!("document.{}", doc_ext);
  let document = BundledDocument {
    path: document_path,
    content_type: document_resource.content_type.clone(),
    final_url: final_url.clone(),
    status: document_resource.status,
    etag: document_resource.etag.clone(),
    last_modified: document_resource.last_modified.clone(),
  };

  let mut resources: Vec<ResourceEntry> = Vec::new();
  let mut manifest_resources: BTreeMap<String, BundledResourceInfo> = BTreeMap::new();
  let mut urls: Vec<String> = recorded.keys().cloned().collect();
  urls.sort();
  for (idx, url) in urls.iter().enumerate() {
    let res = recorded.get(url).unwrap();
    let path = format!(
      "resources/{:05}_{}.{}",
      idx,
      safe_stem(url),
      extension_for_resource(res, url)
    );
    let info = BundledResourceInfo {
      path: path.clone(),
      content_type: res.content_type.clone(),
      status: res.status,
      final_url: Some(res.final_url.clone().unwrap_or_else(|| url.clone())),
      etag: res.etag.clone(),
      last_modified: res.last_modified.clone(),
    };
    manifest_resources.insert(url.clone(), info.clone());
    resources.push(ResourceEntry {
      info,
      bytes: res.bytes.clone(),
    });
  }

  let manifest = BundleManifest {
    version: BUNDLE_VERSION,
    original_url,
    document,
    render,
    resources: manifest_resources,
  };

  (manifest, resources, document_resource.bytes.clone())
}

fn write_bundle(
  out: &Path,
  manifest: &BundleManifest,
  resources: &[ResourceEntry],
  document_bytes: &[u8],
) -> Result<()> {
  if out
    .extension()
    .and_then(|e| e.to_str())
    .map(|ext| ext.eq_ignore_ascii_case("tar"))
    .unwrap_or(false)
  {
    write_bundle_archive(out, manifest, resources, document_bytes)
  } else {
    write_bundle_directory(out, manifest, resources, document_bytes)
  }
}

fn write_bundle_directory(
  out: &Path,
  manifest: &BundleManifest,
  resources: &[ResourceEntry],
  document_bytes: &[u8],
) -> Result<()> {
  fs::create_dir_all(out)?;
  write_file(out.join(&manifest.document.path), document_bytes)?;
  for entry in resources {
    write_file(out.join(&entry.info.path), &entry.bytes)?;
  }
  let manifest_bytes = serde_json::to_vec_pretty(manifest)
    .map_err(|e| fastrender::Error::Other(format!("Failed to serialize manifest: {e}")))?;
  write_file(out.join(BUNDLE_MANIFEST), &manifest_bytes)?;
  Ok(())
}

fn write_bundle_archive(
  out: &Path,
  manifest: &BundleManifest,
  resources: &[ResourceEntry],
  document_bytes: &[u8],
) -> Result<()> {
  if let Some(parent) = out.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent)?;
    }
  }
  let file = fs::File::create(out).map_err(fastrender::Error::Io)?;
  let mut builder = tar::Builder::new(file);
  builder.mode(tar::HeaderMode::Deterministic);

  append_tar_entry(&mut builder, &manifest.document.path, document_bytes)?;
  for entry in resources {
    append_tar_entry(&mut builder, &entry.info.path, &entry.bytes)?;
  }
  let manifest_bytes = serde_json::to_vec_pretty(manifest)
    .map_err(|e| fastrender::Error::Other(format!("Failed to serialize manifest: {e}")))?;
  append_tar_entry(&mut builder, BUNDLE_MANIFEST, &manifest_bytes)?;
  builder.finish().map_err(fastrender::Error::Io)?;
  Ok(())
}

fn append_tar_entry<W: Write>(
  builder: &mut tar::Builder<W>,
  path: &str,
  bytes: &[u8],
) -> Result<()> {
  let mut header = tar::Header::new_gnu();
  header.set_size(bytes.len() as u64);
  header.set_mode(0o644);
  header.set_mtime(0);
  header.set_uid(0);
  header.set_gid(0);
  header.set_path(path).map_err(fastrender::Error::Io)?;
  header.set_cksum();
  builder
    .append(&header, bytes)
    .map_err(fastrender::Error::Io)
}

fn write_file(path: PathBuf, bytes: &[u8]) -> Result<()> {
  if let Some(parent) = path.parent() {
    fs::create_dir_all(parent)?;
  }
  fs::write(path, bytes)?;
  Ok(())
}

fn extension_for_resource(res: &FetchedResource, url: &str) -> String {
  if let Some(ct_raw) = res.content_type.as_deref() {
    let ct = ct_raw.to_ascii_lowercase();
    if ct.starts_with("text/html") {
      return "html".to_string();
    }
    if ct.contains("text/css") {
      return "css".to_string();
    }
    if ct.contains("image/png") {
      return "png".to_string();
    }
    if ct.contains("image/jpeg") {
      return "jpg".to_string();
    }
    if ct.contains("image/gif") {
      return "gif".to_string();
    }
    if ct.contains("image/webp") {
      return "webp".to_string();
    }
    if ct.contains("image/avif") {
      return "avif".to_string();
    }
    if ct.contains("svg") {
      return "svg".to_string();
    }
    if ct.contains("font/woff2") {
      return "woff2".to_string();
    }
    if ct.contains("font/woff") {
      return "woff".to_string();
    }
    if ct.contains("font/ttf") {
      return "ttf".to_string();
    }
  }

  if let Ok(parsed) = url::Url::parse(url) {
    if let Some(ext) = Path::new(parsed.path())
      .extension()
      .and_then(|e| e.to_str())
    {
      let ext = ext.to_ascii_lowercase();
      if ext.len() <= 8 {
        return ext;
      }
    }
  }

  "bin".to_string()
}

fn safe_stem(url: &str) -> String {
  let mut stem = url_to_filename(url);
  if stem.is_empty() {
    stem = "resource".to_string();
  }
  stem
}

fn fetch_document(
  fetcher: &RecordingFetcher,
  url: &str,
) -> Result<(common::render_pipeline::PreparedDocument, FetchedResource)> {
  let mut resource = fetcher.fetch(url)?;
  let mut base_hint = resource.final_url.as_deref().unwrap_or(url).to_string();
  let mut doc = decode_html_resource(&resource, &base_hint);

  if let Some(refresh) = extract_meta_refresh_url(&doc.html) {
    if let Some(target) = resolve_href(&doc.base_url, &refresh) {
      eprintln!("Following meta refresh to: {target}");
      match fetcher.fetch(&target) {
        Ok(res) => {
          resource = res;
          base_hint = resource.final_url.as_deref().unwrap_or(&target).to_string();
          doc = decode_html_resource(&resource, &base_hint);
        }
        Err(err) => eprintln!("Warning: failed to follow meta refresh: {err}"),
      }
    }
  }

  if let Some(js_redirect) = extract_js_location_redirect(&doc.html) {
    if js_redirect.len() <= 2048 {
      if let Some(target) = resolve_href(&doc.base_url, &js_redirect) {
        eprintln!("Following JS redirect to: {target}");
        match fetcher.fetch(&target) {
          Ok(res) => {
            resource = res;
            base_hint = resource.final_url.as_deref().unwrap_or(&target).to_string();
            doc = decode_html_resource(&resource, &base_hint);
          }
          Err(err) => eprintln!("Warning: failed to follow JS redirect: {err}"),
        }
      }
    } else {
      eprintln!(
        "Warning: skipping JS redirect longer than 2048 bytes (len={})",
        js_redirect.len()
      );
    }
  }

  Ok((doc, resource))
}

fn apply_full_page_env(full_page: bool) {
  if full_page {
    std::env::set_var("FASTR_FULL_PAGE", "1");
  } else {
    std::env::remove_var("FASTR_FULL_PAGE");
  }
}
