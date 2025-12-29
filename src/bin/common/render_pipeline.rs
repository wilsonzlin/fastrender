//! Shared helpers for CLI render binaries.

use fastrender::api::{
  FastRender, FastRenderConfig, RenderArtifactRequest, RenderDiagnostics, RenderOptions,
  RenderReport, RenderResult, ResourceKind,
};
use fastrender::compat::CompatProfile;
use fastrender::css::loader::{infer_base_url, resolve_href};
use fastrender::dom::DomCompatibilityMode;
use fastrender::html::encoding::decode_html_bytes;
use fastrender::html::meta_refresh::{extract_js_location_redirect, extract_meta_refresh_url};
use fastrender::resource::{parse_cached_html_meta, FetchedResource, HttpFetcher, ResourceFetcher};
use fastrender::style::media::MediaType;
use fastrender::text::font_db::FontConfig;
use fastrender::{Error, LayoutParallelism, Result};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

/// Bundle of renderer configuration and per-render options parsed from CLI flags.
#[derive(Debug, Clone)]
pub struct RenderConfigBundle {
  pub config: FastRenderConfig,
  pub options: RenderOptions,
}

/// Shared render surface options parsed from CLI flags.
#[derive(Debug, Clone)]
pub struct RenderSurface {
  pub viewport: (u32, u32),
  pub scroll_x: f32,
  pub scroll_y: f32,
  pub dpr: f32,
  pub media_type: MediaType,
  pub css_limit: Option<usize>,
  pub allow_partial: bool,
  pub apply_meta_viewport: bool,
  pub base_url: Option<String>,
  pub allow_file_from_http: bool,
  pub block_mixed_content: bool,
  pub same_origin_subresources: bool,
  pub allowed_subresource_origins: Vec<String>,
  pub trace_output: Option<PathBuf>,
  pub layout_parallelism: Option<LayoutParallelism>,
  pub font_config: Option<FontConfig>,
  pub compat_profile: CompatProfile,
  pub dom_compat_mode: DomCompatibilityMode,
}

/// Construct render configuration objects from CLI settings.
pub fn build_render_configs(surface: &RenderSurface) -> RenderConfigBundle {
  let mut config = FastRenderConfig::new()
    .with_default_viewport(surface.viewport.0, surface.viewport.1)
    .with_device_pixel_ratio(surface.dpr)
    .with_meta_viewport(surface.apply_meta_viewport)
    .with_allow_file_from_http(surface.allow_file_from_http)
    .with_block_mixed_content(surface.block_mixed_content)
    .compat_profile(surface.compat_profile)
    .with_dom_compat_mode(surface.dom_compat_mode);
  if let Some(base_url) = &surface.base_url {
    config = config.with_base_url(base_url.clone());
  }
  if let Some(font_config) = surface.font_config.as_ref() {
    config = config.with_font_sources(font_config.clone());
  }

  let mut options = RenderOptions::new()
    .with_viewport(surface.viewport.0, surface.viewport.1)
    .with_device_pixel_ratio(surface.dpr)
    .with_media_type(surface.media_type)
    .with_scroll(surface.scroll_x, surface.scroll_y)
    .with_stylesheet_limit(surface.css_limit)
    .allow_partial(surface.allow_partial);
  options.trace_output = surface.trace_output.clone();

  if let Some(parallelism) = surface.layout_parallelism {
    config = config.with_layout_parallelism(parallelism);
    options = options.with_layout_parallelism(parallelism);
  }

  RenderConfigBundle { config, options }
}

/// Configure an HTTP fetcher with headers and timeout suitable for CLI use.
pub fn build_http_fetcher(
  user_agent: &str,
  accept_language: &str,
  timeout_secs: Option<u64>,
) -> HttpFetcher {
  let mut fetcher = HttpFetcher::new()
    .with_user_agent(user_agent.to_string())
    .with_accept_language(accept_language.to_string());
  if let Some(secs) = timeout_secs {
    fetcher = fetcher.with_timeout(Duration::from_secs(secs));
  }
  fetcher
}

/// Rendered HTML along with its resolved base information.
#[derive(Debug, Clone)]
pub struct PreparedDocument {
  pub html: String,
  pub base_hint: String,
  pub base_url: String,
}

impl PreparedDocument {
  pub fn new(html: String, base_hint: String) -> Self {
    let base_url = infer_base_url(&html, &base_hint).into_owned();
    Self {
      html,
      base_hint,
      base_url,
    }
  }

  pub fn with_base_override(mut self, base_url: Option<&str>) -> Self {
    if let Some(base_url) = base_url {
      self.base_hint = base_url.to_string();
      self.base_url = infer_base_url(&self.html, &self.base_hint).into_owned();
    }
    self
  }
}

/// Cached HTML content loaded from disk.
#[derive(Debug, Clone)]
pub struct CachedDocument {
  pub document: PreparedDocument,
  pub content_type: Option<String>,
  pub byte_len: usize,
}

/// Decode a fetched HTML resource using the provided base URL hint.
pub fn decode_html_resource(resource: &FetchedResource, base_hint: &str) -> PreparedDocument {
  let html = decode_html_bytes(&resource.bytes, resource.content_type.as_deref());
  PreparedDocument::new(html, base_hint.to_string())
}

/// Load cached HTML from disk, honoring optional sidecar metadata.
pub fn read_cached_document(path: &Path) -> Result<CachedDocument> {
  let bytes = std::fs::read(path).map_err(Error::Io)?;

  let mut meta_path = path.to_path_buf();
  if let Some(ext) = meta_path.extension().and_then(|e| e.to_str()) {
    meta_path.set_extension(format!("{ext}.meta"));
  } else {
    meta_path.set_extension("meta");
  }
  let meta = std::fs::read_to_string(&meta_path).ok();
  let parsed_meta = meta
    .as_deref()
    .map(parse_cached_html_meta)
    .unwrap_or_default();

  let base_hint = parsed_meta
    .url
    .clone()
    .unwrap_or_else(|| format!("file://{}", path.display()));
  let html = decode_html_bytes(&bytes, parsed_meta.content_type.as_deref());

  Ok(CachedDocument {
    document: PreparedDocument::new(html, base_hint),
    content_type: parsed_meta.content_type,
    byte_len: bytes.len(),
  })
}

/// Follow client-side redirects (meta refresh and JS location) once per page.
pub fn follow_client_redirects(
  fetcher: &dyn ResourceFetcher,
  mut doc: PreparedDocument,
  mut log: impl FnMut(&str),
) -> PreparedDocument {
  if let Some(refresh) = extract_meta_refresh_url(&doc.html) {
    if let Some(target) = resolve_href(&doc.base_url, &refresh) {
      log(&format!("Following meta refresh to: {target}"));
      match fetcher.fetch(&target) {
        Ok(res) => {
          let base_hint = res.final_url.as_deref().unwrap_or(&target).to_string();
          doc = decode_html_resource(&res, &base_hint);
        }
        Err(err) => log(&format!(
          "Warning: failed to follow meta refresh {target}: {err}"
        )),
      }
    }
  }

  if let Some(js_redirect) = extract_js_location_redirect(&doc.html) {
    if js_redirect.len() <= 2048 {
      if let Some(target) = resolve_href(&doc.base_url, &js_redirect) {
        log(&format!("Following JS location redirect to: {target}"));
        match fetcher.fetch(&target) {
          Ok(res) => {
            let base_hint = res.final_url.as_deref().unwrap_or(&target).to_string();
            doc = decode_html_resource(&res, &base_hint);
          }
          Err(err) => log(&format!(
            "Warning: failed to follow JS redirect {target}: {err}"
          )),
        }
      }
    } else {
      log(&format!(
        "Warning: skipping JS redirect of length {}",
        js_redirect.len()
      ));
    }
  }

  doc
}

/// Render prepared HTML using the shared stylesheet inlining path.
pub fn render_document(
  renderer: &mut FastRender,
  doc: PreparedDocument,
  options: &RenderOptions,
) -> Result<RenderResult> {
  renderer.render_html_with_stylesheets(&doc.html, &doc.base_hint, options.clone())
}

/// Render a prepared document while capturing intermediate artifacts.
pub fn render_document_with_artifacts(
  renderer: &mut FastRender,
  doc: PreparedDocument,
  options: &RenderOptions,
  artifacts: RenderArtifactRequest,
) -> Result<RenderReport> {
  renderer.render_html_with_stylesheets_report(
    &doc.html,
    &doc.base_hint,
    options.clone(),
    artifacts,
  )
}

/// Log render diagnostics in a consistent human-readable format.
pub fn log_layout_parallelism(diagnostics: &RenderDiagnostics, mut log: impl FnMut(&str)) {
  if let Some(layout) = &diagnostics.layout_parallelism {
    let mode = format!("{:?}", layout.mode).to_ascii_lowercase();
    let mut line = format!(
      "Layout parallelism: mode={mode} engaged={} nodes={} min_fanout={} workers={} work_items={}",
      layout.engaged,
      layout.node_count,
      layout.min_fanout,
      layout.worker_threads,
      layout.work_items
    );
    if let Some(max_threads) = layout.max_threads {
      line.push_str(&format!(" max_threads={max_threads}"));
    }
    if let Some(min_nodes) = layout.auto_min_nodes {
      line.push_str(&format!(" auto_min_nodes={min_nodes}"));
    }
    log(&line);
  }
}

pub fn log_diagnostics(diagnostics: &RenderDiagnostics, mut log: impl FnMut(&str)) {
  if let Some(err) = &diagnostics.document_error {
    log(&format!("Document error: {err}"));
  }

  if let Some(stage) = diagnostics.timeout_stage {
    log(&format!("Timed out during {stage}"));
  }

  log_layout_parallelism(diagnostics, &mut log);

  for fetch_error in &diagnostics.fetch_errors {
    let kind = match fetch_error.kind {
      ResourceKind::Document => "document",
      ResourceKind::Stylesheet => "stylesheet",
      ResourceKind::Image => "image",
      ResourceKind::Font => "font",
      ResourceKind::Other => "resource",
    };
    let mut meta = Vec::new();
    if let Some(status) = fetch_error.status {
      meta.push(format!("status {}", status));
    }
    if let Some(final_url) = &fetch_error.final_url {
      if final_url != &fetch_error.url {
        meta.push(format!("final {final_url}"));
      }
    }
    if let Some(etag) = &fetch_error.etag {
      meta.push(format!("etag {etag}"));
    }
    if let Some(last_modified) = &fetch_error.last_modified {
      meta.push(format!("last-modified {last_modified}"));
    }
    let meta = if meta.is_empty() {
      String::new()
    } else {
      format!(" ({})", meta.join(", "))
    };
    log(&format!(
      "Warning: failed to fetch {kind} {}{meta}: {}",
      fetch_error.url, fetch_error.message
    ));
  }
}

/// Format an error, optionally including its source chain.
pub fn format_error_with_chain(err: &dyn std::error::Error, verbose: bool) -> String {
  if !verbose {
    return err.to_string();
  }

  let mut lines = vec![err.to_string()];
  let mut current = err.source();
  while let Some(source) = current {
    lines.push(format!("caused by: {}", source));
    current = source.source();
  }

  lines.join("\n")
}

/// Convenience for building a renderer with the provided config and fetcher.
pub fn build_renderer_with_fetcher(
  config: FastRenderConfig,
  fetcher: Arc<dyn ResourceFetcher>,
) -> Result<FastRender> {
  FastRender::with_config_and_fetcher(config, Some(fetcher))
}
