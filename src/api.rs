//! Public API for FastRender
//!
//! This module provides a clean, simple public interface for rendering HTML/CSS
//! to pixels. It wraps the internal rendering pipeline with an ergonomic API.
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::api::FastRender;
//!
//! // Create a new renderer
//! let mut renderer = FastRender::new()?;
//!
//! // Render HTML to pixels
//! let html = r#"
//!     <!DOCTYPE html>
//!     <html>
//!         <head>
//!             <style>
//!                 body { background: #f0f0f0; padding: 20px; }
//!                 h1 { color: navy; }
//!             </style>
//!         </head>
//!         <body>
//!             <h1>Hello, FastRender!</h1>
//!         </body>
//!     </html>
//! "#;
//!
//! let pixmap = renderer.render_html(html, 800, 600)?;
//!
//! // Save to PNG
//! pixmap.save_png("output.png")?;
//! ```
//!
//! # Architecture
//!
//! The FastRender API orchestrates the complete rendering pipeline:
//!
//! ```text
//! HTML/CSS → Parse → Style → Box Tree → Layout → Fragment Tree → Paint → Pixmap
//! ```
//!
//! Each step is handled by specialized components:
//! - **Parsing**: HTML5 parsing via html5ever
//! - **Styling**: CSS cascade and computed styles
//! - **Box Generation**: DOM → CSS box tree conversion
//! - **Layout**: Positioning via formatting contexts
//! - **Painting**: Rasterization via tiny-skia
//!
//! # Thread Safety
//!
//! `FastRender` is `Send` but not `Sync`. Each instance maintains internal
//! caches that are not thread-safe. For multi-threaded rendering, create one
//! instance per thread or use [`FastRenderPool`] to share immutable resources
//! while keeping per-worker caches isolated.

use crate::accessibility::AccessibilityNode;
use crate::animation;
use crate::compat::CompatProfile;
use crate::css::encoding::decode_css_bytes;
use crate::css::loader::{
  absolutize_css_urls, extract_css_links, extract_embedded_css_urls, infer_base_url,
  inject_css_into_html, inline_imports_with_diagnostics, resolve_href_with_base,
};
use crate::css::parser::{
  extract_css_sources, parse_stylesheet, rel_list_contains_stylesheet, StylesheetSource,
};
use crate::css::types::{CssImportLoader, StyleSheet};
use crate::debug;
use crate::debug::inspect::{InspectQuery, InspectionSnapshot};
use crate::debug::runtime::{self, RuntimeToggles};
use crate::debug::trace::TraceHandle;
use crate::dom::DomNode;
use crate::dom::{self, DomCompatibilityMode, DomParseOptions};
use crate::error::Error;
use crate::error::NavigationError;
use crate::error::RenderError;
use crate::error::RenderStage;
use crate::error::ResourceError;
use crate::error::Result;
use crate::geometry::Point;
use crate::geometry::Rect;
use crate::geometry::Size;
use crate::html::encoding::decode_html_bytes;
use crate::html::viewport::ViewportLength;
use crate::image_loader::ImageCache;
use crate::image_output::encode_image;
use crate::image_output::OutputFormat;
use crate::layout::absolute_positioning::resolve_positioned_style;
use crate::layout::contexts::inline::baseline::compute_line_height_with_metrics_viewport;
use crate::layout::contexts::inline::line_builder::TextItem;
use crate::layout::contexts::positioned::ContainingBlock;
use crate::layout::engine::LayoutConfig;
use crate::layout::engine::LayoutEngine;
use crate::layout::flex_profile::flex_profile_enabled;
use crate::layout::flex_profile::log_flex_profile;
use crate::layout::flex_profile::reset_flex_profile;
use crate::layout::formatting_context::intrinsic_cache_clear;
use crate::layout::formatting_context::intrinsic_cache_reset_counters;
use crate::layout::formatting_context::intrinsic_cache_stats;
use crate::layout::formatting_context::LayoutError as FormattingLayoutError;
use crate::layout::pagination::{paginate_fragment_tree_with_options, PaginateOptions};
use crate::layout::profile::layout_profile_enabled;
use crate::layout::profile::log_layout_profile;
use crate::layout::profile::reset_layout_profile;
use crate::paint::display_list_builder::DisplayListBuilder;
use crate::paint::painter::paint_tree_with_resources_scaled;
use crate::paint::painter::paint_tree_with_resources_scaled_offset;
use crate::paint::painter::paint_tree_with_resources_scaled_offset_with_trace;
use crate::render_control::{CancelCallback, DeadlineGuard, RenderDeadline};
use crate::resource::CachingFetcherConfig;
use crate::resource::{
  origin_from_url, CachingFetcher, DocumentOrigin, HttpFetcher, PolicyError, ResourceAccessPolicy,
  ResourceFetcher, ResourcePolicy,
};
use crate::style::cascade::apply_styles_with_media_target_and_imports;
use crate::style::cascade::apply_styles_with_media_target_and_imports_cached;
use crate::style::cascade::apply_styles_with_media_target_and_imports_cached_with_deadline;
use crate::style::cascade::ContainerQueryContext;
use crate::style::cascade::ContainerQueryInfo;
use crate::style::cascade::StyledNode;
use crate::style::color::Rgba;
use crate::style::media::MediaType;
use crate::style::media::{MediaContext, MediaQuery, MediaQueryCache};
use crate::style::page::resolve_page_style;
use crate::style::page::PageSide;
use crate::style::types::ContainerType;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::text::font_db::FontDatabase;
use crate::text::font_db::FontConfig;
use crate::text::font_db::FontStretch;
use crate::text::font_db::FontStyle as DbFontStyle;
use crate::text::font_db::ScaledMetrics;
use crate::text::font_loader::FontContext;
use crate::text::pipeline::ShapingPipeline;
use crate::tree::box_generation::BoxGenerationOptions;
use crate::tree::box_tree::BoxNode;
use crate::tree::box_tree::BoxTree;
use crate::tree::box_tree::BoxType;
use crate::tree::box_tree::FormControlKind;
use crate::tree::box_tree::MarkerContent;
use crate::tree::box_tree::ReplacedBox;
use crate::tree::box_tree::ReplacedType;
#[cfg(test)]
use crate::tree::box_tree::SrcsetCandidate;
#[cfg(test)]
use crate::tree::box_tree::SrcsetDescriptor;
use crate::tree::fragment_tree::FragmentContent;
use crate::tree::fragment_tree::FragmentNode;
use crate::tree::fragment_tree::FragmentTree;
use fontdb::Database as FontDbDatabase;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::io;
use std::mem;
use std::path::PathBuf;
use std::sync::{Arc, Condvar, Mutex, OnceLock};
use url::Url;

use std::time::{Duration, Instant};
pub(crate) const DEFAULT_MAX_IFRAME_DEPTH: usize = 3;
// Re-export Pixmap from tiny-skia for public use
pub use crate::image_loader::ImageCacheConfig;
pub use crate::layout::pagination::PageStacking;
pub use crate::text::font_db::FontCacheConfig;
pub use tiny_skia::Pixmap;

#[derive(Default, Debug, Clone)]
struct ReplacedIntrinsicProfileState {
  depth: usize,
  start: Option<Instant>,
  replaced_nodes: usize,
  image_nodes: usize,
  form_controls: usize,
  videos: usize,
  svgs: usize,
  embeds: usize,
  selection_calls: usize,
  selection_ms: f64,
  probe_calls: usize,
  probe_ms: f64,
  probe_ok: usize,
  probe_err: usize,
  load_calls: usize,
  load_ms: f64,
  render_svg_calls: usize,
  render_svg_ms: f64,
}

thread_local! {
  static REPLACED_INTRINSIC_PROFILE: RefCell<ReplacedIntrinsicProfileState> =
    RefCell::new(ReplacedIntrinsicProfileState::default());
}

/// Main entry point for the FastRender library
///
/// `FastRender` provides a high-level API for rendering HTML/CSS to pixels.
/// It manages internal resources like font contexts and layout engines,
/// providing a simple interface for common rendering tasks.
///
/// # Examples
///
/// ## Basic rendering
///
/// ```rust,ignore
/// use fastrender::api::FastRender;
///
/// let mut renderer = FastRender::new()?;
/// let pixmap = renderer.render_html("<h1>Hello!</h1>", 800, 600)?;
/// ```
///
/// ## With builder pattern
///
/// ```rust,ignore
/// use fastrender::api::{FastRender, FastRenderConfig};
///
/// let config = FastRenderConfig::new()
///     .with_default_background(Rgba::WHITE);
///
/// let mut renderer = FastRender::with_config(config)?;
/// let pixmap = renderer.render_html(html, 1024, 768)?;
/// ```
///
/// ## Access to intermediate structures
///
/// ```rust,ignore
/// use fastrender::api::FastRender;
///
/// let mut renderer = FastRender::new()?;
///
/// // Parse HTML to DOM
/// let dom = renderer.parse_html("<div>Content</div>")?;
///
/// // Layout the document
/// let fragment_tree = renderer.layout_document(&dom, 800, 600)?;
///
/// // Paint to pixmap
/// let pixmap = renderer.paint(&fragment_tree, 800, 600)?;
/// ```
pub struct FastRender {
  /// Font context for text shaping and measurement
  font_context: FontContext,

  /// Layout engine for computing positions
  layout_engine: LayoutEngine,

  /// Image cache for external resources (images, SVG)
  image_cache: ImageCache,

  /// Resource fetcher for loading external resources (shared with ImageCache)
  fetcher: Arc<dyn ResourceFetcher>,

  /// Optional diagnostics sink for recording fetch failures during a render.
  diagnostics: Option<Arc<Mutex<RenderDiagnostics>>>,

  /// Default background color for rendering
  background_color: Rgba,

  /// Default viewport width for render()
  default_width: u32,

  /// Default viewport height for render()
  default_height: u32,

  /// Device pixel ratio used for media queries and resolution-dependent resources
  device_pixel_ratio: f32,

  /// Whether to honor `<meta name="viewport">` directives when computing the
  /// layout viewport for screen media.
  apply_meta_viewport: bool,

  /// Temporary override for device size during media query evaluation.
  pending_device_size: Option<Size>,

  /// Base URL used for resolving links/targets
  base_url: Option<String>,

  /// Compatibility profile for opt-in site-specific behaviors
  compat_profile: CompatProfile,

  /// Optional compatibility mode applied during DOM parsing
  dom_compat_mode: DomCompatibilityMode,

  /// Resource loading policy configuration.
  resource_policy: ResourceAccessPolicy,

  /// Active resource context for the current render.
  resource_context: Option<ResourceContext>,

  /// Maximum iframe nesting depth when painting nested browsing contexts
  max_iframe_depth: usize,

  /// Runtime debug/configuration toggles in effect for this renderer instance.
  runtime_toggles: Arc<RuntimeToggles>,
}

impl std::fmt::Debug for FastRender {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("FastRender")
      .field("background_color", &self.background_color)
      .field("default_width", &self.default_width)
      .field("default_height", &self.default_height)
      .field("device_pixel_ratio", &self.device_pixel_ratio)
      .field("apply_meta_viewport", &self.apply_meta_viewport)
      .field("base_url", &self.base_url)
      .field("compat_profile", &self.compat_profile)
      .field("dom_compat_mode", &self.dom_compat_mode)
      .field("resource_policy", &self.resource_policy)
      .field("max_iframe_depth", &self.max_iframe_depth)
      .finish_non_exhaustive()
  }
}

/// Configuration for FastRender
///
/// Use this to customize rendering behavior when creating a FastRender instance.
///
/// # Examples
///
/// ```rust,ignore
/// use fastrender::api::FastRenderConfig;
/// use fastrender::Rgba;
///
/// let config = FastRenderConfig::new()
///     .with_default_background(Rgba::rgb(240, 240, 240));
/// ```
#[derive(Debug, Clone)]
pub struct FastRenderConfig {
  /// Default background color for rendered images
  pub background_color: Rgba,

  /// Default viewport width (used when not specified)
  pub default_width: u32,

  /// Default viewport height (used when not specified)
  pub default_height: u32,

  /// Device pixel ratio used for media queries and resolution-dependent resources
  pub device_pixel_ratio: f32,

  /// Base URL used to resolve relative resource references (images, CSS)
  pub base_url: Option<String>,

  /// Optional in-memory resource cache configuration. If `None`, caching is disabled.
  pub resource_cache: Option<CachingFetcherConfig>,

  /// Policy controlling which resources may be fetched and how large they may be.
  pub resource_policy: ResourcePolicy,

  /// Maximum iframe nesting depth when painting nested browsing contexts
  pub max_iframe_depth: usize,

  /// Allow http(s) documents to load file:// resources when true (disabled by default)
  pub allow_file_from_http: bool,

  /// Block mixed HTTP subresources when the document is HTTPS.
  pub block_mixed_content: bool,

  /// Compatibility profile controlling opt-in site-specific behaviors.
  pub compat_profile: CompatProfile,

  /// Optional compatibility mode used when parsing HTML.
  pub dom_compat_mode: DomCompatibilityMode,

  /// Whether to honor `<meta name="viewport">` when computing the layout viewport.
  pub apply_meta_viewport: bool,

  /// Font discovery configuration (system vs bundled fonts, extra search paths).
  pub font_config: FontConfig,

  /// Runtime debug/configuration toggles used for this renderer.
  pub runtime_toggles: Arc<RuntimeToggles>,
}

impl Default for FastRenderConfig {
  fn default() -> Self {
    Self {
      background_color: Rgba::WHITE,
      default_width: 800,
      default_height: 600,
      device_pixel_ratio: 1.0,
      base_url: None,
      resource_cache: Some(CachingFetcherConfig::default()),
      resource_policy: ResourcePolicy::default(),
      max_iframe_depth: DEFAULT_MAX_IFRAME_DEPTH,
      allow_file_from_http: false,
      block_mixed_content: false,
      compat_profile: CompatProfile::default(),
      dom_compat_mode: DomCompatibilityMode::Standard,
      apply_meta_viewport: false,
      font_config: FontConfig::default(),
      runtime_toggles: Arc::new(RuntimeToggles::from_env()),
    }
  }
}

impl FastRenderConfig {
  /// Creates a new configuration with default values
  pub fn new() -> Self {
    Self::default()
  }
}

/// Builder for creating FastRender instances
///
/// # Examples
///
/// ```rust,ignore
/// use fastrender::FastRender;
///
/// let mut renderer = FastRender::builder()
///     .viewport_size(1024, 768)
///     .background_color(Rgba::WHITE)
///     .build()?;
/// ```
pub struct FastRenderBuilder {
  config: FastRenderConfig,
  fetcher: Option<Arc<dyn ResourceFetcher>>,
}

impl FastRenderBuilder {
  /// Creates a new builder with default configuration
  pub fn new() -> Self {
    Self {
      config: FastRenderConfig::default(),
      fetcher: None,
    }
  }

  /// Sets the viewport size
  pub fn viewport_size(mut self, width: u32, height: u32) -> Self {
    self.config.default_width = width;
    self.config.default_height = height;
    self
  }

  /// Sets the viewport width
  pub fn viewport_width(mut self, width: u32) -> Self {
    self.config.default_width = width;
    self
  }

  /// Sets the viewport height
  pub fn viewport_height(mut self, height: u32) -> Self {
    self.config.default_height = height;
    self
  }

  /// Sets the background color
  pub fn background_color(mut self, color: Rgba) -> Self {
    self.config.background_color = color;
    self
  }

  /// Sets a base URL used to resolve relative resource references (images, linked CSS)
  pub fn base_url(mut self, url: impl Into<String>) -> Self {
    self.config.base_url = Some(url.into());
    self
  }

  /// Overrides how fonts are discovered and which bundled set to use.
  pub fn font_sources(mut self, font_config: FontConfig) -> Self {
    self.config.font_config = font_config;
    self
  }

  /// Allow loading file:// resources from HTTP(S) documents.
  pub fn allow_file_from_http(mut self, allow: bool) -> Self {
    self.config.allow_file_from_http = allow;
    self
  }

  /// Block mixed HTTP subresources when rendering HTTPS documents.
  pub fn block_mixed_content(mut self, block: bool) -> Self {
    self.config.block_mixed_content = block;
    self
  }

  /// Applies `<meta name="viewport">` directives when computing the layout viewport.
  ///
  /// When enabled, `width`/`height` set the layout viewport used for viewport units
  /// and media queries. `initial-scale` (and optional min/max scale) controls zoom,
  /// clamped to the 0.1–10 range, and scales the effective device pixel ratio as
  /// well as the visual viewport used for device media features.
  pub fn apply_meta_viewport(mut self, enabled: bool) -> Self {
    self.config.apply_meta_viewport = enabled;
    self
  }

  /// Sets the DOM compatibility mode applied during parsing.
  pub fn dom_compatibility_mode(mut self, mode: DomCompatibilityMode) -> Self {
    self.config.dom_compat_mode = mode;
    self
  }

  /// Sets the device pixel ratio used for media queries and image-set selection.
  pub fn device_pixel_ratio(mut self, dpr: f32) -> Self {
    self.config.device_pixel_ratio = dpr;
    self
  }

  /// Enables a specific compatibility profile.
  pub fn compat_mode(mut self, profile: CompatProfile) -> Self {
    self.config.compat_profile = profile;
    self
  }

  /// Turns on site-specific compatibility hacks used for internal captures.
  pub fn with_site_compat_hacks(mut self) -> Self {
    self.config.compat_profile = CompatProfile::SiteCompatibility;
    self
  }

  /// Sets a custom resource policy for fetches performed by this renderer.
  pub fn resource_policy(mut self, policy: ResourcePolicy) -> Self {
    self.config.resource_policy = policy;
    self
  }

  /// Sets a custom resource fetcher for loading external resources (images, CSS)
  ///
  /// This allows injection of custom fetching behavior, such as caching or mocking.
  pub fn fetcher(mut self, fetcher: Arc<dyn ResourceFetcher>) -> Self {
    self.fetcher = Some(fetcher);
    self
  }

  /// Builds the FastRender instance
  pub fn build(self) -> Result<FastRender> {
    FastRender::with_config_and_fetcher(self.config, self.fetcher)
  }

  /// Overrides the runtime debug/configuration toggles used by the renderer.
  pub fn runtime_toggles(mut self, toggles: RuntimeToggles) -> Self {
    self.config.runtime_toggles = Arc::new(toggles);
    self
  }
}

impl Default for FastRenderBuilder {
  fn default() -> Self {
    Self::new()
  }
}

/// Per-request rendering options for both HTML strings and URLs.
#[derive(Clone)]
pub struct RenderOptions {
  /// Optional viewport override. Falls back to the renderer's default size.
  pub viewport: Option<(u32, u32)>,
  /// Optional device pixel ratio override for media queries and image selection.
  pub device_pixel_ratio: Option<f32>,
  /// Controls structured diagnostics capture for this render.
  pub diagnostics_level: DiagnosticsLevel,
  /// Media type used for evaluating media queries.
  pub media_type: MediaType,
  /// Horizontal scroll offset applied before painting.
  pub scroll_x: f32,
  /// Vertical scroll offset applied before painting.
  pub scroll_y: f32,
  /// Maximum number of external stylesheets to inline. `None` means unlimited.
  pub css_limit: Option<usize>,
  /// When true, include an accessibility tree alongside rendering results.
  pub capture_accessibility: bool,
  /// When true, document fetch failures will return a placeholder pixmap with diagnostics instead of an error.
  pub allow_partial: bool,
  /// Optional hard timeout for the render.
  pub timeout: Option<Duration>,
  /// Optional cooperative cancellation callback.
  pub cancel_callback: Option<Arc<CancelCallback>>,
  /// Optional path to write a Chrome trace of this render.
  pub trace_output: Option<PathBuf>,
  /// Optional runtime toggle override for this render.
  pub runtime_toggles: Option<Arc<RuntimeToggles>>,
}

impl Default for RenderOptions {
  fn default() -> Self {
    Self {
      viewport: None,
      device_pixel_ratio: None,
      diagnostics_level: DiagnosticsLevel::None,
      media_type: MediaType::Screen,
      scroll_x: 0.0,
      scroll_y: 0.0,
      css_limit: None,
      capture_accessibility: false,
      allow_partial: false,
      timeout: None,
      cancel_callback: None,
      trace_output: None,
      runtime_toggles: None,
    }
  }
}

impl std::fmt::Debug for RenderOptions {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("RenderOptions")
      .field("viewport", &self.viewport)
      .field("device_pixel_ratio", &self.device_pixel_ratio)
      .field("diagnostics_level", &self.diagnostics_level)
      .field("media_type", &self.media_type)
      .field("scroll_x", &self.scroll_x)
      .field("scroll_y", &self.scroll_y)
      .field("css_limit", &self.css_limit)
      .field("capture_accessibility", &self.capture_accessibility)
      .field("allow_partial", &self.allow_partial)
      .field("timeout", &self.timeout)
      .field(
        "cancel_callback",
        &self.cancel_callback.as_ref().map(|_| "<callback>"),
      )
      .field("trace_output", &self.trace_output)
      .field(
        "runtime_toggles",
        &self.runtime_toggles.as_ref().map(|_| "<toggles>"),
      )
      .finish()
  }
}

impl RenderOptions {
  /// Create a new options struct with defaults.
  pub fn new() -> Self {
    Self::default()
  }

  /// Enable structured diagnostics for this render.
  pub fn with_diagnostics_level(mut self, level: DiagnosticsLevel) -> Self {
    self.diagnostics_level = level;
    self
  }

  /// Override the viewport size for this render.
  pub fn with_viewport(mut self, width: u32, height: u32) -> Self {
    self.viewport = Some((width, height));
    self
  }

  /// Override the device pixel ratio for this render.
  pub fn with_device_pixel_ratio(mut self, dpr: f32) -> Self {
    self.device_pixel_ratio = Some(dpr);
    self
  }

  /// Override the media type used for media queries.
  pub fn with_media_type(mut self, media_type: MediaType) -> Self {
    self.media_type = media_type;
    self
  }

  /// Apply scroll offsets before painting.
  pub fn with_scroll(mut self, scroll_x: f32, scroll_y: f32) -> Self {
    self.scroll_x = scroll_x;
    self.scroll_y = scroll_y;
    self
  }

  /// Limit the number of linked stylesheets to inline.
  pub fn with_stylesheet_limit(mut self, limit: Option<usize>) -> Self {
    self.css_limit = limit;
    self
  }

  /// Request an accessibility tree alongside rendered output.
  pub fn with_accessibility(mut self, enabled: bool) -> Self {
    self.capture_accessibility = enabled;
    self
  }

  /// Enable returning a placeholder pixmap when document fetch fails.
  pub fn allow_partial(mut self, allow: bool) -> Self {
    self.allow_partial = allow;
    self
  }

  /// Set a timeout for the entire render pipeline.
  pub fn with_timeout(mut self, timeout: Option<Duration>) -> Self {
    self.timeout = timeout;
    self
  }

  /// Provide a cooperative cancellation callback that returns true when rendering should stop.
  pub fn with_cancel_callback(mut self, callback: Option<Arc<CancelCallback>>) -> Self {
    self.cancel_callback = callback;
    self
  }

  /// Emit a Chrome trace of the render pipeline to the given path.
  pub fn with_trace_output(mut self, path: impl Into<PathBuf>) -> Self {
    self.trace_output = Some(path.into());
    self
  }

  /// Override the runtime debug/configuration toggles for this render.
  pub fn with_runtime_toggles(mut self, toggles: RuntimeToggles) -> Self {
    self.runtime_toggles = Some(Arc::new(toggles));
    self
  }
}

/// Diagnostics collected during rendering.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct RenderDiagnostics {
  /// Network fetch errors encountered while loading subresources.
  pub fetch_errors: Vec<ResourceFetchError>,
  /// Document-level fetch failure message when a placeholder render is produced.
  pub document_error: Option<String>,
  /// Stage at which rendering timed out, when allow_partial is used.
  pub timeout_stage: Option<RenderStage>,
  /// Optional structured statistics gathered during rendering.
  pub stats: Option<RenderStats>,
}

impl RenderDiagnostics {
  /// Create an empty diagnostics object.
  pub fn new() -> Self {
    Self::default()
  }

  /// Record a failed fetch using structured error information.
  pub fn record_error(&mut self, kind: ResourceKind, url: impl Into<String>, error: &Error) {
    let url = url.into();
    self
      .fetch_errors
      .push(ResourceFetchError::from_error(kind, url, error));
  }

  /// Record a failed fetch with a plain message.
  pub fn record_message(
    &mut self,
    kind: ResourceKind,
    url: impl Into<String>,
    message: impl Into<String>,
  ) {
    self
      .fetch_errors
      .push(ResourceFetchError::new(kind, url, message));
  }

  /// Mark a document-level fetch failure.
  pub fn set_document_error(&mut self, message: impl Into<String>) {
    self.document_error = Some(message.into());
  }
}

/// Shared diagnostics handle for concurrent render stages.
#[derive(Clone, Default)]
pub struct SharedRenderDiagnostics {
  inner: Arc<Mutex<RenderDiagnostics>>,
}

impl SharedRenderDiagnostics {
  /// Construct a new shared diagnostics handle.
  pub fn new() -> Self {
    Self::default()
  }

  /// Record a failed fetch.
  pub fn record(&self, kind: ResourceKind, url: impl Into<String>, message: impl Into<String>) {
    if let Ok(mut guard) = self.inner.lock() {
      guard.record_message(kind, url, message);
    }
  }

  /// Record a failed fetch using structured error information.
  pub fn record_error(&self, kind: ResourceKind, url: impl Into<String>, error: &Error) {
    if let Ok(mut guard) = self.inner.lock() {
      guard.record_error(kind, url, error);
    }
  }

  /// Record a document-level failure.
  pub fn set_document_error(&self, message: impl Into<String>) {
    if let Ok(mut guard) = self.inner.lock() {
      guard.set_document_error(message);
    }
  }

  /// Extract owned diagnostics, cloning when the handle is shared.
  pub fn into_inner(self) -> RenderDiagnostics {
    match Arc::try_unwrap(self.inner) {
      Ok(mutex) => mutex.into_inner().unwrap_or_default(),
      Err(shared) => shared.lock().map(|g| g.clone()).unwrap_or_default(),
    }
  }
}

/// Error captured while fetching a resource needed for rendering.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceFetchError {
  /// Type of resource that failed.
  pub kind: ResourceKind,
  /// URL that failed to load.
  pub url: String,
  /// Human-readable error string.
  pub message: String,
  /// HTTP status code when applicable.
  pub status: Option<u16>,
  /// Final URL after redirects, if known.
  pub final_url: Option<String>,
  /// HTTP ETag header.
  pub etag: Option<String>,
  /// HTTP Last-Modified header.
  pub last_modified: Option<String>,
}

impl ResourceFetchError {
  /// Construct a new fetch error entry.
  pub fn new(kind: ResourceKind, url: impl Into<String>, message: impl Into<String>) -> Self {
    Self {
      kind,
      url: url.into(),
      message: message.into(),
      status: None,
      final_url: None,
      etag: None,
      last_modified: None,
    }
  }

  pub fn from_error(kind: ResourceKind, url: impl Into<String>, error: &Error) -> Self {
    let message = match error {
      Error::Resource(res) => res.message.clone(),
      _ => error.to_string(),
    };
    let mut entry = Self::new(kind, url, message);
    match error {
      Error::Resource(res) => {
        entry.status = res.status;
        entry.final_url = res.final_url.clone().or_else(|| Some(res.url.clone()));
        entry.etag = res.etag.clone();
        entry.last_modified = res.last_modified.clone();
      }
      Error::Navigation(nav) => match nav {
        crate::error::NavigationError::FetchFailed { url, .. } => {
          entry.final_url = Some(url.clone());
        }
      },
      Error::Image(image) => match image {
        crate::error::ImageError::LoadFailed { url, .. }
        | crate::error::ImageError::DecodeFailed { url, .. }
        | crate::error::ImageError::InvalidFormat { url, .. }
        | crate::error::ImageError::NetworkError { url, .. } => entry.final_url = Some(url.clone()),
        _ => {}
      },
      _ => {}
    }

    if entry.final_url.is_none() {
      entry.final_url = Some(entry.url.clone());
    }
    entry
  }
}

/// Shared resource-loading context combining policy and diagnostics.
#[derive(Clone, Default)]
pub struct ResourceContext {
  pub policy: ResourceAccessPolicy,
  pub diagnostics: Option<SharedRenderDiagnostics>,
}

impl ResourceContext {
  /// Record a fetch error when diagnostics collection is enabled.
  pub fn record(&self, kind: ResourceKind, url: impl Into<String>, message: impl Into<String>) {
    if let Some(diag) = &self.diagnostics {
      diag.record(kind, url, message);
    }
  }

  /// Evaluate whether a URL is allowed by policy, recording a diagnostic on failure.
  pub fn check_allowed(
    &self,
    kind: ResourceKind,
    url: &str,
  ) -> std::result::Result<(), PolicyError> {
    match self.policy.allows(url) {
      Ok(()) => Ok(()),
      Err(err) => {
        self.record(kind, url, err.reason.clone());
        Err(err)
      }
    }
  }

  /// Clone this context with a different document origin.
  pub fn for_origin(&self, origin: Option<DocumentOrigin>) -> Self {
    Self {
      policy: self.policy.for_origin(origin),
      diagnostics: self.diagnostics.clone(),
    }
  }
}

/// Classification of resource types for diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ResourceKind {
  /// The root HTML document
  Document,
  /// Stylesheet resource
  Stylesheet,
  /// Image resource
  Image,
  /// Font resource
  Font,
  /// Other resource types
  Other,
}

/// Result of a rendering operation that includes diagnostics.
#[derive(Debug, Clone)]
pub struct RenderResult {
  /// Rendered pixels.
  pub pixmap: Pixmap,
  /// Optional accessibility tree captured during rendering.
  pub accessibility: Option<AccessibilityNode>,
  /// Diagnostics captured while fetching resources.
  pub diagnostics: RenderDiagnostics,
}

/// Intermediate artifacts produced during layout.
#[derive(Debug, Clone)]
pub struct LayoutArtifacts {
  pub dom: DomNode,
  pub stylesheet: StyleSheet,
  pub styled_tree: StyledNode,
  pub box_tree: BoxTree,
  pub fragment_tree: FragmentTree,
}

/// A fully prepared document ready for repeated painting.
///
/// `PreparedDocument` owns the parsed DOM, resolved stylesheet, styled tree, box
/// tree, and laid-out fragment tree. Painting reuses the same font and image
/// caches, allowing callers to apply different scroll offsets or viewport
/// translations without re-running parse/style/layout.
///
/// # Thread safety
///
/// Like `FastRender`, this type is `Send` but not `Sync`; clone or prepare a
/// separate document per thread when painting in parallel.
#[derive(Clone)]
pub struct PreparedDocument {
  dom: DomNode,
  stylesheet: StyleSheet,
  styled_tree: StyledNode,
  box_tree: BoxTree,
  fragment_tree: FragmentTree,
  layout_viewport: Size,
  visual_viewport: Size,
  device_pixel_ratio: f32,
  page_zoom: f32,
  background_color: Rgba,
  default_scroll: Point,
  font_context: FontContext,
  image_cache: ImageCache,
}

impl PreparedDocument {
  /// Paints the prepared document using the provided scroll offsets.
  ///
  /// The painting path mirrors `FastRender::render_html_with_scroll`, applying
  /// scroll snap, scroll-driven animations, sticky positioning, and a root
  /// translation for the scroll offset before rasterization.
  pub fn paint(
    &self,
    scroll_x: f32,
    scroll_y: f32,
    viewport_override: Option<(u32, u32)>,
    background_override: Option<Rgba>,
  ) -> Result<Pixmap> {
    if let Some((w, h)) = viewport_override {
      if w == 0 || h == 0 {
        return Err(Error::Render(RenderError::InvalidParameters {
          message: format!("Invalid viewport override: width={w}, height={h}"),
        }));
      }
    }

    let viewport = viewport_override.map(|(w, h)| Size::new(w as f32, h as f32));
    let paint_scale =
      (self.device_pixel_ratio / self.page_zoom.max(f32::EPSILON)).max(f32::EPSILON);
    paint_fragment_tree_with_scroll(
      self.fragment_tree.clone(),
      scroll_x,
      scroll_y,
      viewport,
      background_override.unwrap_or(self.background_color),
      &self.font_context,
      &self.image_cache,
      paint_scale,
    )
  }

  /// Paints using the viewport captured during preparation and the initial
  /// scroll offsets provided in the original `RenderOptions`.
  pub fn paint_default(&self) -> Result<Pixmap> {
    self.paint(self.default_scroll.x, self.default_scroll.y, None, None)
  }

  /// Paints a specific rectangular region of the prepared document.
  ///
  /// This is a convenience for tiled rendering and uses the rectangle as both
  /// the viewport and scroll offset.
  pub fn paint_region(&self, rect: Rect) -> Result<Pixmap> {
    let viewport = (
      rect.width().max(1.0).round() as u32,
      rect.height().max(1.0).round() as u32,
    );
    self.paint(rect.x(), rect.y(), Some(viewport), None)
  }

  /// Returns the parsed DOM used during preparation.
  pub fn dom(&self) -> &DomNode {
    &self.dom
  }

  /// Returns the fully resolved stylesheet (including imported rules).
  pub fn stylesheet(&self) -> &StyleSheet {
    &self.stylesheet
  }

  /// Returns the styled tree produced during cascade.
  pub fn styled_tree(&self) -> &StyledNode {
    &self.styled_tree
  }

  /// Returns the generated box tree.
  pub fn box_tree(&self) -> &BoxTree {
    &self.box_tree
  }

  /// Returns the laid-out fragment tree prior to any scroll translations.
  pub fn fragment_tree(&self) -> &FragmentTree {
    &self.fragment_tree
  }

  /// Returns the layout viewport size used during preparation.
  pub fn layout_viewport(&self) -> Size {
    self.layout_viewport
  }

  /// Returns the visual viewport after applying `<meta name="viewport">`.
  pub fn visual_viewport(&self) -> Size {
    self.visual_viewport
  }

  /// Returns the device pixel ratio captured during preparation.
  pub fn device_pixel_ratio(&self) -> f32 {
    self.device_pixel_ratio
  }

  /// Returns the scroll offsets supplied when preparing the document.
  pub fn default_scroll(&self) -> Point {
    self.default_scroll
  }
}

/// Intermediate pipeline outputs produced during layout.
#[derive(Debug, Clone)]
pub struct LayoutIntermediates {
  /// DOM after applying top-layer state.
  pub dom: DomNode,
  /// Styled tree produced by cascade.
  pub styled_tree: StyledNode,
  /// Box tree generated from the styled tree.
  pub box_tree: BoxTree,
  /// Positioned fragment tree ready for painting.
  pub fragment_tree: FragmentTree,
}

impl RenderResult {
  /// Extract the rendered pixels, discarding diagnostics.
  pub fn into_pixmap(self) -> Pixmap {
    self.pixmap
  }

  /// Return the captured accessibility tree, if present.
  pub fn accessibility(&self) -> Option<&AccessibilityNode> {
    self.accessibility.as_ref()
  }

  /// Encode the rendered pixmap to an image format, annotating diagnostics when available.
  pub fn encode(self, format: OutputFormat) -> Result<(Vec<u8>, RenderDiagnostics)> {
    let encode_timer = self.diagnostics.stats.as_ref().map(|_| Instant::now());
    let encoded = encode_image(&self.pixmap, format)?;
    let mut diagnostics = self.diagnostics;
    if let (Some(stats), Some(start)) = (diagnostics.stats.as_mut(), encode_timer) {
      stats.timings.encode_ms = Some(start.elapsed().as_secs_f64() * 1000.0);
    }
    Ok((encoded, diagnostics))
  }
}

/// Controls the amount of diagnostics gathered during a render.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DiagnosticsLevel {
  /// Do not collect diagnostics beyond fetch errors.
  None,
  /// Capture basic timing/counter information.
  Basic,
  /// Capture extra, potentially expensive diagnostics.
  Verbose,
}

impl Default for DiagnosticsLevel {
  fn default() -> Self {
    DiagnosticsLevel::None
  }
}

/// Timing information for each pipeline stage (milliseconds).
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct RenderStageTimings {
  pub html_decode_ms: Option<f64>,
  pub dom_parse_ms: Option<f64>,
  pub css_inlining_ms: Option<f64>,
  pub css_parse_ms: Option<f64>,
  pub cascade_ms: Option<f64>,
  pub box_tree_ms: Option<f64>,
  pub layout_ms: Option<f64>,
  pub paint_build_ms: Option<f64>,
  pub paint_optimize_ms: Option<f64>,
  pub paint_rasterize_ms: Option<f64>,
  pub encode_ms: Option<f64>,
}

/// Common counters gathered during rendering.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct RenderCounts {
  pub dom_nodes: Option<usize>,
  pub styled_nodes: Option<usize>,
  pub box_nodes: Option<usize>,
  pub fragments: Option<usize>,
}

/// Cascade selector matching statistics.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct CascadeDiagnostics {
  pub nodes: Option<u64>,
  pub rule_candidates: Option<u64>,
  pub rule_matches: Option<u64>,
  pub selector_time_ms: Option<f64>,
  pub declaration_time_ms: Option<f64>,
  pub pseudo_time_ms: Option<f64>,
}

/// Layout cache and intrinsic sizing statistics.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct LayoutDiagnostics {
  pub intrinsic_lookups: Option<usize>,
  pub intrinsic_hits: Option<usize>,
  pub intrinsic_stores: Option<usize>,
  pub block_intrinsic: Option<usize>,
  pub flex_intrinsic: Option<usize>,
  pub inline_intrinsic: Option<usize>,
  pub layout_cache_lookups: Option<usize>,
  pub layout_cache_hits: Option<usize>,
  pub layout_cache_stores: Option<usize>,
  pub layout_cache_evictions: Option<usize>,
}

/// Paint pipeline statistics.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct PaintDiagnostics {
  pub display_items: Option<usize>,
  pub optimized_items: Option<usize>,
  pub culled_items: Option<usize>,
  pub transparent_removed: Option<usize>,
  pub noop_removed: Option<usize>,
  pub merged_items: Option<usize>,
}

/// Resource loading counters.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct ResourceDiagnostics {
  pub fetch_counts: HashMap<ResourceKind, usize>,
  pub image_cache_hits: Option<usize>,
  pub image_cache_misses: Option<usize>,
}

/// Structured report describing a render.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct RenderStats {
  pub timings: RenderStageTimings,
  pub counts: RenderCounts,
  pub cascade: CascadeDiagnostics,
  pub layout: LayoutDiagnostics,
  pub paint: PaintDiagnostics,
  pub resources: ResourceDiagnostics,
}

fn diagnostics_level_from_env() -> DiagnosticsLevel {
  match std::env::var("FASTR_DIAGNOSTICS_LEVEL")
    .or_else(|_| std::env::var("FASTR_DIAGNOSTICS"))
    .map(|v| v.to_ascii_lowercase())
  {
    Ok(v) if v == "verbose" || v == "2" => DiagnosticsLevel::Verbose,
    Ok(v) if v == "basic" || v == "1" => DiagnosticsLevel::Basic,
    Ok(v) if v == "none" || v == "0" => DiagnosticsLevel::None,
    _ => DiagnosticsLevel::None,
  }
}

fn merge_image_cache_diagnostics(stats: &mut RenderStats) {
  if let Some(image_stats) = crate::image_loader::take_image_cache_diagnostics() {
    stats.resources.image_cache_hits = Some(image_stats.cache_hits);
    stats.resources.image_cache_misses = Some(image_stats.cache_misses);
    if image_stats.cache_misses > 0 {
      *stats
        .resources
        .fetch_counts
        .entry(ResourceKind::Image)
        .or_default() += image_stats.cache_misses;
    }
  }
}

#[derive(Default)]
struct RenderStatsRecorder {
  level: DiagnosticsLevel,
  stats: RenderStats,
}

impl RenderStatsRecorder {
  fn new(level: DiagnosticsLevel) -> Self {
    Self {
      level,
      stats: RenderStats::default(),
    }
  }

  fn enabled(&self) -> bool {
    !matches!(self.level, DiagnosticsLevel::None)
  }

  fn verbose(&self) -> bool {
    matches!(self.level, DiagnosticsLevel::Verbose)
  }

  fn timer(&self) -> Option<Instant> {
    self.enabled().then(Instant::now)
  }

  fn record_ms(target: &mut Option<f64>, start: Option<Instant>) {
    if let Some(s) = start {
      *target = Some(s.elapsed().as_secs_f64() * 1000.0);
    }
  }

  fn record_fetch(&mut self, kind: ResourceKind) {
    *self.stats.resources.fetch_counts.entry(kind).or_default() += 1;
  }

  fn finish(self) -> RenderStats {
    self.stats
  }
}

fn count_dom_nodes_api(node: &DomNode) -> usize {
  1 + node.children.iter().map(count_dom_nodes_api).sum::<usize>()
}

fn count_box_nodes_api(node: &BoxNode) -> usize {
  1 + node.children.iter().map(count_box_nodes_api).sum::<usize>()
}

struct CascadeProfileOverride {
  previous: bool,
}

impl CascadeProfileOverride {
  fn enable() -> Self {
    let previous = crate::style::cascade::cascade_profile_enabled();
    crate::style::cascade::set_cascade_profile_enabled(true);
    crate::style::cascade::reset_cascade_profile();
    Self { previous }
  }
}

impl Drop for CascadeProfileOverride {
  fn drop(&mut self) {
    crate::style::cascade::set_cascade_profile_enabled(self.previous);
  }
}

#[derive(Debug)]
struct RenderOutputs {
  pixmap: Pixmap,
  accessibility: Option<AccessibilityNode>,
}

/// Request for capturing intermediate render artifacts.
///
/// All flags are opt-in to avoid unnecessary cloning and allocations during
/// normal renders.
#[derive(Debug, Clone, Copy, Default)]
pub struct RenderArtifactRequest {
  /// Capture the parsed DOM tree.
  pub dom: bool,
  /// Capture the styled tree after cascade.
  pub styled_tree: bool,
  /// Capture the generated box tree.
  pub box_tree: bool,
  /// Capture the laid-out fragment tree.
  pub fragment_tree: bool,
  /// Capture the built display list prior to rasterization.
  pub display_list: bool,
}

impl RenderArtifactRequest {
  /// No artifacts should be captured.
  pub fn none() -> Self {
    Self::default()
  }

  /// Capture artifacts suitable for summarization.
  pub fn summary() -> Self {
    Self {
      dom: true,
      styled_tree: true,
      box_tree: true,
      fragment_tree: true,
      display_list: true,
    }
  }

  /// Capture all available artifacts for full dumps.
  pub fn full() -> Self {
    Self {
      dom: true,
      styled_tree: true,
      box_tree: true,
      fragment_tree: true,
      display_list: true,
    }
  }

  /// Returns true when no artifacts are requested.
  pub fn is_empty(&self) -> bool {
    !self.dom && !self.styled_tree && !self.box_tree && !self.fragment_tree && !self.display_list
  }
}

/// Intermediate render artifacts captured during a render.
#[derive(Debug, Default, Clone)]
pub struct RenderArtifacts {
  request: RenderArtifactRequest,
  /// Parsed DOM tree.
  pub dom: Option<DomNode>,
  /// Styled tree after cascade.
  pub styled_tree: Option<StyledNode>,
  /// Generated box tree.
  pub box_tree: Option<BoxTree>,
  /// Positioned fragment tree.
  pub fragment_tree: Option<FragmentTree>,
  /// Built display list prior to rasterization.
  pub display_list: Option<crate::paint::display_list::DisplayList>,
}

impl RenderArtifacts {
  /// Create an empty artifacts container honoring the provided request.
  pub fn new(request: RenderArtifactRequest) -> Self {
    Self {
      request,
      ..Self::default()
    }
  }

  /// Returns the requested capture configuration.
  pub fn request(&self) -> RenderArtifactRequest {
    self.request
  }
}

/// Render output that includes intermediate artifacts alongside diagnostics.
#[derive(Debug, Clone)]
pub struct RenderReport {
  /// Rendered pixels.
  pub pixmap: Pixmap,
  /// Optional accessibility tree captured during rendering.
  pub accessibility: Option<AccessibilityNode>,
  /// Diagnostics captured while fetching resources.
  pub diagnostics: RenderDiagnostics,
  /// Optional intermediate artifacts captured during the render.
  pub artifacts: RenderArtifacts,
}

impl RenderReport {
  /// Convert into a `RenderResult`, discarding captured artifacts.
  pub fn into_result(self) -> RenderResult {
    RenderResult {
      pixmap: self.pixmap,
      accessibility: self.accessibility,
      diagnostics: self.diagnostics,
    }
  }
}

impl From<RenderReport> for RenderResult {
  fn from(report: RenderReport) -> Self {
    report.into_result()
  }
}

#[derive(Clone)]
struct TraceSession {
  handle: TraceHandle,
  output: Option<PathBuf>,
}

impl TraceSession {
  fn new(trace_output: Option<PathBuf>) -> Self {
    let output = trace_output.or_else(|| std::env::var_os("FASTR_TRACE_OUT").map(PathBuf::from));
    let handle = if output.is_some() {
      TraceHandle::enabled()
    } else {
      TraceHandle::disabled()
    };
    Self { handle, output }
  }

  fn from_options(options: Option<&RenderOptions>) -> Self {
    let path = options.and_then(|opts| opts.trace_output.clone());
    Self::new(path)
  }

  fn handle(&self) -> &TraceHandle {
    &self.handle
  }

  fn finalize<T>(self, result: Result<T>) -> Result<T> {
    if let Some(path) = self.output {
      let write_result = self.handle.write_chrome_trace(&path).map_err(Error::Io);
      match (result, write_result) {
        (Ok(val), Ok(())) => Ok(val),
        (Ok(_), Err(err)) => Err(err),
        (Err(err), _) => Err(err),
      }
    } else {
      result
    }
  }
}

impl FastRenderConfig {
  /// Sets the default background color
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// use fastrender::api::FastRenderConfig;
  /// use fastrender::Rgba;
  ///
  /// let config = FastRenderConfig::new()
  ///     .with_default_background(Rgba::rgb(255, 255, 255));
  /// ```
  pub fn with_default_background(mut self, color: Rgba) -> Self {
    self.background_color = color;
    self
  }

  /// Sets the runtime debug/configuration toggles to use for this renderer.
  pub fn with_runtime_toggles(mut self, toggles: RuntimeToggles) -> Self {
    self.runtime_toggles = Arc::new(toggles);
    self
  }

  /// Sets the default viewport dimensions
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// let config = FastRenderConfig::new()
  ///     .with_default_viewport(1920, 1080);
  /// ```
  pub fn with_default_viewport(mut self, width: u32, height: u32) -> Self {
    self.default_width = width;
    self.default_height = height;
    self
  }

  /// Sets the device pixel ratio used for media queries and image-set selection.
  pub fn with_device_pixel_ratio(mut self, dpr: f32) -> Self {
    self.device_pixel_ratio = dpr;
    self
  }

  /// Sets the base URL used to resolve relative resource references.
  pub fn with_base_url(mut self, url: impl Into<String>) -> Self {
    self.base_url = Some(url.into());
    self
  }

  /// Sets the resource fetch policy used by the default fetcher.
  pub fn with_resource_policy(mut self, policy: ResourcePolicy) -> Self {
    self.resource_policy = policy;
    self
  }

  /// Configures font discovery and bundled font usage.
  pub fn with_font_sources(mut self, font_config: FontConfig) -> Self {
    self.font_config = font_config;
    self
  }

  /// Allow loading file:// resources from HTTP(S) documents.
  pub fn with_allow_file_from_http(mut self, allow: bool) -> Self {
    self.allow_file_from_http = allow;
    self
  }

  /// Block mixed HTTP subresources when rendering HTTPS documents.
  pub fn with_block_mixed_content(mut self, block: bool) -> Self {
    self.block_mixed_content = block;
    self
  }

  /// Applies `<meta name="viewport">` directives when computing the layout viewport.
  ///
  /// `width`/`height` set the layout viewport. Zoom is driven by `initial-scale`
  /// (or derived from the requested width/height) and clamped by `minimum-scale`/
  /// `maximum-scale` to the 0.1–10 range, scaling the effective device pixel ratio
  /// and the visual viewport used for device media features.
  pub fn with_meta_viewport(mut self, enabled: bool) -> Self {
    self.apply_meta_viewport = enabled;
    self
  }

  /// Sets the DOM compatibility mode applied during parsing.
  pub fn with_dom_compat_mode(mut self, mode: DomCompatibilityMode) -> Self {
    self.dom_compat_mode = mode;
    self
  }

  /// Sets the site compatibility profile applied during box generation.
  pub fn compat_profile(mut self, profile: CompatProfile) -> Self {
    self.compat_profile = profile;
    self
  }

  /// Selects a compatibility profile. The default is spec-first rendering with no
  /// site-specific heuristics.
  pub fn compat_mode(mut self, profile: CompatProfile) -> Self {
    self.compat_profile = profile;
    self
  }

  /// Enables site-specific compatibility hacks used for internal captures.
  pub fn with_site_compat_hacks(mut self) -> Self {
    self.compat_profile = CompatProfile::SiteCompatibility;
    self
  }
}

/// Configuration for [`FastRenderPool`].
#[derive(Clone)]
pub struct FastRenderPoolConfig {
  /// Base renderer configuration applied to each worker.
  pub renderer: FastRenderConfig,
  /// Maximum number of worker renderers kept in the pool.
  pub pool_size: usize,
  /// Cache sizing for font lookups.
  pub font_cache: FontCacheConfig,
  /// Cache sizing for image decoding.
  pub image_cache: ImageCacheConfig,
  /// Optional shared resource fetcher used for documents and subresources.
  pub fetcher: Option<Arc<dyn ResourceFetcher>>,
}

impl Default for FastRenderPoolConfig {
  fn default() -> Self {
    Self {
      renderer: FastRenderConfig::default(),
      pool_size: num_cpus::get().max(1),
      font_cache: FontCacheConfig::default(),
      image_cache: ImageCacheConfig::default(),
      fetcher: None,
    }
  }
}

impl FastRenderPoolConfig {
  /// Create a new pool configuration using defaults.
  pub fn new() -> Self {
    Self::default()
  }

  /// Override the renderer configuration applied to each worker.
  pub fn with_renderer_config(mut self, renderer: FastRenderConfig) -> Self {
    self.renderer = renderer;
    self
  }

  /// Override the maximum number of pooled renderers.
  pub fn with_pool_size(mut self, pool_size: usize) -> Self {
    self.pool_size = pool_size.max(1);
    self
  }

  /// Override the font cache configuration applied to each worker.
  pub fn with_font_cache(mut self, font_cache: FontCacheConfig) -> Self {
    self.font_cache = font_cache;
    self
  }

  /// Override the image cache configuration applied to each worker.
  pub fn with_image_cache(mut self, image_cache: ImageCacheConfig) -> Self {
    self.image_cache = image_cache;
    self
  }

  /// Provide a custom resource fetcher shared by all pooled renderers.
  pub fn with_fetcher(mut self, fetcher: Arc<dyn ResourceFetcher>) -> Self {
    self.fetcher = Some(fetcher);
    self
  }
}

#[derive(Clone)]
pub struct FastRenderPool {
  inner: Arc<PoolInner>,
}

struct PoolInner {
  shared: Arc<SharedPoolResources>,
  state: Mutex<PoolState>,
  available: Condvar,
}

struct PoolState {
  idle: Vec<FastRender>,
  total: usize,
}

struct SharedPoolResources {
  config: FastRenderConfig,
  font_cache: FontCacheConfig,
  image_cache: ImageCacheConfig,
  fetcher: Arc<dyn ResourceFetcher>,
  font_db: Arc<FontDbDatabase>,
  pool_size: usize,
}

impl SharedPoolResources {
  fn build_renderer(&self) -> Result<FastRender> {
    let font_context = FontContext::with_shared_resource_fetcher(
      Arc::clone(&self.font_db),
      Arc::clone(&self.fetcher),
      self.font_cache,
    );
    FastRender::from_parts(
      self.config.clone(),
      Arc::clone(&self.fetcher),
      font_context,
      self.image_cache,
    )
  }
}

impl FastRenderPool {
  /// Create a new pool using the default configuration.
  pub fn new() -> Result<Self> {
    Self::with_config(FastRenderPoolConfig::default())
  }

  /// Create a new pool using the provided configuration.
  pub fn with_config(config: FastRenderPoolConfig) -> Result<Self> {
    let pool_size = config.pool_size.max(1);
    let fetcher = resolve_fetcher(&config.renderer, config.fetcher);
    let inner = PoolInner {
      shared: Arc::new(SharedPoolResources {
        config: config.renderer,
        font_cache: config.font_cache,
        image_cache: config.image_cache,
        fetcher,
        font_db: FontDatabase::shared_system_db(),
        pool_size,
      }),
      state: Mutex::new(PoolState {
        idle: Vec::with_capacity(pool_size),
        total: 0,
      }),
      available: Condvar::new(),
    };

    Ok(Self {
      inner: Arc::new(inner),
    })
  }

  fn acquire_renderer(&self) -> Result<FastRender> {
    let mut guard = self.inner.state.lock().unwrap();
    loop {
      if let Some(renderer) = guard.idle.pop() {
        return Ok(renderer);
      }
      if guard.total < self.inner.shared.pool_size {
        guard.total += 1;
        let shared = Arc::clone(&self.inner.shared);
        drop(guard);
        let built = shared.build_renderer();
        match built {
          Ok(renderer) => return Ok(renderer),
          Err(err) => {
            let mut guard = self.inner.state.lock().unwrap();
            guard.total = guard.total.saturating_sub(1);
            self.inner.available.notify_one();
            return Err(err);
          }
        }
      }
      guard = self.inner.available.wait(guard).unwrap();
    }
  }

  fn release_renderer(&self, renderer: FastRender) {
    let mut guard = self.inner.state.lock().unwrap();
    guard.idle.push(renderer);
    self.inner.available.notify_one();
  }

  fn with_renderer<F, T>(&self, op: F) -> Result<T>
  where
    F: FnOnce(&mut FastRender) -> Result<T>,
  {
    let mut renderer = self.acquire_renderer()?;
    let result = op(&mut renderer);
    self.release_renderer(renderer);
    result
  }

  /// Render HTML to a pixmap using a pooled renderer.
  pub fn render_html(&self, html: &str, width: u32, height: u32) -> Result<Pixmap> {
    self.with_renderer(|renderer| renderer.render_html(html, width, height))
  }

  /// Render HTML with per-request options using a pooled renderer.
  pub fn render_html_with_options(&self, html: &str, options: RenderOptions) -> Result<Pixmap> {
    self.with_renderer(move |renderer| renderer.render_html_with_options(html, options))
  }

  /// Fetch and render a document from a URL using a pooled renderer.
  pub fn render_url(&self, url: &str) -> Result<RenderResult> {
    self.with_renderer(|renderer| renderer.render_url(url))
  }

  /// Fetch and render a document from a URL using explicit options.
  pub fn render_url_with_options(&self, url: &str, options: RenderOptions) -> Result<RenderResult> {
    self.with_renderer(move |renderer| renderer.render_url_with_options(url, options))
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct ResolvedViewport {
  /// The CSS layout viewport (initial containing block). Viewport units (`vw`/`vh`)
  /// and media queries use this size.
  layout_viewport: Size,
  /// The visual viewport after applying the resolved zoom scale. Device media features
  /// (`device-width`/`device-height`) use this value.
  visual_viewport: Size,
  /// Effective device pixel ratio after page zoom. This multiplies the renderer's base DPR.
  device_pixel_ratio: f32,
  /// Effective page zoom derived from meta viewport directives.
  zoom: f32,
}

impl ResolvedViewport {
  fn new(viewport: Size, device_pixel_ratio: f32) -> Self {
    Self {
      layout_viewport: viewport,
      visual_viewport: viewport,
      device_pixel_ratio: sanitize_scale(Some(device_pixel_ratio)).unwrap_or(1.0),
      zoom: 1.0,
    }
  }
}

/// Default zoom clamping range aligned with common browser behavior.
const MIN_EFFECTIVE_SCALE: f32 = 0.1;
const MAX_EFFECTIVE_SCALE: f32 = 10.0;

/// Resolves the effective viewport based on meta viewport directives.
fn resolve_viewport(
  requested: Size,
  base_dpr: f32,
  meta: Option<&crate::html::viewport::MetaViewport>,
) -> ResolvedViewport {
  let mut resolved = ResolvedViewport::new(
    Size::new(requested.width.max(1.0), requested.height.max(1.0)),
    base_dpr,
  );
  let Some(meta) = meta else {
    return resolved;
  };

  // Apply explicit width/height requests first; these drive the layout viewport directly.
  let requested_visual = resolved.visual_viewport;
  if let Some(width) = meta.width {
    if let Some(value) = viewport_length_value(width, requested_visual.width) {
      resolved.layout_viewport.width = value.max(1.0);
    }
  }
  if let Some(height) = meta.height {
    if let Some(value) = viewport_length_value(height, requested_visual.height) {
      resolved.layout_viewport.height = value.max(1.0);
    }
  }

  // Determine the zoom scale. Authors can pin it explicitly with initial-scale; otherwise
  // we derive a default that makes the visual viewport match the requested width/height
  // when a corresponding dimension is provided.
  let mut min_scale = sanitize_scale(meta.minimum_scale);
  let mut max_scale = sanitize_scale(meta.maximum_scale);
  if let (Some(min), Some(max)) = (min_scale, max_scale) {
    if min > max {
      std::mem::swap(&mut min_scale, &mut max_scale);
    }
  }

  let mut scale = meta
    .initial_scale
    .and_then(|value| sanitize_scale(Some(value)))
    .or_else(|| {
      meta.width.and_then(|_| {
        Some((requested_visual.width / resolved.layout_viewport.width).max(f32::EPSILON))
      })
    })
    .or_else(|| {
      meta.height.and_then(|_| {
        Some((requested_visual.height / resolved.layout_viewport.height).max(f32::EPSILON))
      })
    })
    .unwrap_or(1.0);

  if let Some(min) = min_scale {
    scale = scale.max(min);
  }
  if let Some(max) = max_scale {
    scale = scale.min(max);
  }
  scale = sanitize_scale(Some(scale)).unwrap_or(1.0);
  resolved.zoom = scale;

  resolved.visual_viewport = Size::new(
    (requested_visual.width / scale).max(1.0),
    (requested_visual.height / scale).max(1.0),
  );
  resolved.device_pixel_ratio = (resolved.device_pixel_ratio * scale)
    .clamp(MIN_EFFECTIVE_SCALE, MAX_EFFECTIVE_SCALE)
    .max(f32::EPSILON);
  resolved
}

fn viewport_length_value(len: ViewportLength, fallback: f32) -> Option<f32> {
  match len {
    ViewportLength::Device => Some(fallback),
    ViewportLength::Absolute(v) if v.is_finite() && v > 0.0 => Some(v),
    _ => None,
  }
}

fn sanitize_scale(value: Option<f32>) -> Option<f32> {
  value
    .filter(|v| v.is_finite() && *v > 0.0)
    .map(|v| v.clamp(MIN_EFFECTIVE_SCALE, MAX_EFFECTIVE_SCALE))
}

fn check_deadline(deadline: Option<&RenderDeadline>, stage: RenderStage) -> Result<()> {
  if let Some(limit) = deadline {
    limit.check(stage).map_err(Error::Render)?;
  }
  Ok(())
}

#[cfg(test)]
mod viewport_resolution_tests {
  use super::*;
  use crate::html::viewport::parse_meta_viewport_content;

  fn assert_close(actual: f32, expected: f32, label: &str) {
    let delta = (actual - expected).abs();
    assert!(
      delta < 0.01,
      "{label}: expected {expected} got {actual} (delta {delta})"
    );
  }

  #[test]
  fn resolves_meta_viewport_permutations() {
    let requested = Size::new(800.0, 600.0);
    let base_dpr = 2.0;
    let cases: [(&str, Option<&str>, (f32, f32), (f32, f32), f32); 16] = [
      ("no meta", None, (800.0, 600.0), (800.0, 600.0), 2.0),
      (
        "device width baseline",
        Some("width=device-width"),
        (800.0, 600.0),
        (800.0, 600.0),
        2.0,
      ),
      (
        "narrow width default scale",
        Some("width=320"),
        (320.0, 600.0),
        (320.0, 240.0),
        5.0,
      ),
      (
        "width with explicit scale",
        Some("width=320, initial-scale=1.5"),
        (320.0, 600.0),
        (533.33, 400.0),
        3.0,
      ),
      (
        "width with max scale clamp",
        Some("width=320, maximum-scale=1"),
        (320.0, 600.0),
        (800.0, 600.0),
        2.0,
      ),
      (
        "width with min scale clamp",
        Some("width=320, minimum-scale=3"),
        (320.0, 600.0),
        (266.67, 200.0),
        6.0,
      ),
      (
        "initial scale clamped low",
        Some("width=320, initial-scale=0.05"),
        (320.0, 600.0),
        (8000.0, 6000.0),
        0.2,
      ),
      (
        "height only derives scale",
        Some("height=400"),
        (800.0, 400.0),
        (533.33, 400.0),
        3.0,
      ),
      (
        "width and height derive scale from width",
        Some("width=300, height=400"),
        (300.0, 400.0),
        (300.0, 225.0),
        5.33,
      ),
      (
        "initial scale only",
        Some("initial-scale=2"),
        (800.0, 600.0),
        (400.0, 300.0),
        4.0,
      ),
      (
        "initial scale clamped to minimum",
        Some("initial-scale=0.05"),
        (800.0, 600.0),
        (8000.0, 6000.0),
        0.2,
      ),
      (
        "min greater than max swaps",
        Some("minimum-scale=2, maximum-scale=1, initial-scale=5"),
        (800.0, 600.0),
        (400.0, 300.0),
        4.0,
      ),
      (
        "wide layout with zoom",
        Some("width=1200, initial-scale=2"),
        (1200.0, 600.0),
        (400.0, 300.0),
        4.0,
      ),
      (
        "wide layout with clamped zoom",
        Some("width=1200, initial-scale=2, maximum-scale=1.2"),
        (1200.0, 600.0),
        (666.67, 500.0),
        2.4,
      ),
      (
        "narrow layout with tight min/max",
        Some("width=200, height=150, minimum-scale=0.2, maximum-scale=0.25"),
        (200.0, 150.0),
        (3200.0, 2400.0),
        0.5,
      ),
      (
        "device width with scale capped by maximum",
        Some("width=device-width, initial-scale=3, maximum-scale=2"),
        (800.0, 600.0),
        (400.0, 300.0),
        4.0,
      ),
    ];

    for (label, content, expected_layout, expected_visual, expected_dpr) in cases {
      let meta = content.and_then(parse_meta_viewport_content);
      let resolved = resolve_viewport(requested, base_dpr, meta.as_ref());
      assert_close(resolved.layout_viewport.width, expected_layout.0, label);
      assert_close(resolved.layout_viewport.height, expected_layout.1, label);
      assert_close(resolved.visual_viewport.width, expected_visual.0, label);
      assert_close(resolved.visual_viewport.height, expected_visual.1, label);
      assert_close(resolved.device_pixel_ratio, expected_dpr, label);
    }
  }
}

fn apply_sticky_offsets_with_context(
  font_context: &FontContext,
  fragment: &mut FragmentNode,
  parent_rect: Rect,
  scroll: Point,
  viewport: Size,
) {
  let abs_origin = Point::new(
    parent_rect.x() + fragment.bounds.x(),
    parent_rect.y() + fragment.bounds.y(),
  );
  let abs_rect = Rect::from_xywh(
    abs_origin.x,
    abs_origin.y,
    fragment.bounds.width(),
    fragment.bounds.height(),
  );

  for child in fragment.children.iter_mut() {
    apply_sticky_offsets_with_context(font_context, child, abs_rect, scroll, viewport);
  }

  let Some(style) = fragment.style.as_ref() else {
    return;
  };
  if !style.position.is_sticky() {
    return;
  }

  let inline_base = Some(parent_rect.width());
  let block_base = if parent_rect.height() > 0.0 {
    Some(parent_rect.height())
  } else {
    None
  };
  let containing_block =
    ContainingBlock::with_viewport_and_bases(parent_rect, viewport, inline_base, block_base);
  let positioned = resolve_positioned_style(style, &containing_block, viewport, font_context);
  let constraints = crate::layout::contexts::positioned::StickyConstraints::from_style(
    &positioned,
    &containing_block,
    font_context,
  );
  if !constraints.has_constraints() {
    return;
  }

  let mut screen_x = abs_rect.x() - scroll.x;
  let mut screen_y = abs_rect.y() - scroll.y;

  let container_screen_min_x = parent_rect.x() - scroll.x;
  let container_screen_max_x = parent_rect.max_x() - scroll.x;
  let container_screen_min_y = parent_rect.y() - scroll.y;
  let container_screen_max_y = parent_rect.max_y() - scroll.y;

  let left = constraints.left.unwrap_or(0.0);
  let right = constraints.right.unwrap_or(0.0);
  let top = constraints.top.unwrap_or(0.0);
  let bottom = constraints.bottom.unwrap_or(0.0);

  if constraints.left.is_some() || constraints.right.is_some() {
    let min_x = container_screen_min_x + left;
    let max_x = container_screen_max_x - right - abs_rect.width();
    let viewport_min_x = left;
    let viewport_max_x = viewport.width - right - abs_rect.width();
    let clamp_min_x = min_x.max(viewport_min_x);
    let clamp_max_x = max_x.min(viewport_max_x);
    screen_x = screen_x.max(clamp_min_x).min(clamp_max_x);
  }

  if constraints.top.is_some() || constraints.bottom.is_some() {
    let min_y = container_screen_min_y + top;
    let max_y = container_screen_max_y - bottom - abs_rect.height();
    let viewport_min_y = top;
    let viewport_max_y = viewport.height - bottom - abs_rect.height();
    let clamp_min_y = min_y.max(viewport_min_y);
    let clamp_max_y = max_y.min(viewport_max_y);
    screen_y = screen_y.max(clamp_min_y).min(clamp_max_y);
  }

  let mut new_abs_x = screen_x + scroll.x;
  let mut new_abs_y = screen_y + scroll.y;

  let cb_min_x = parent_rect.x();
  let cb_min_y = parent_rect.y();
  let cb_max_x = parent_rect.max_x();
  let cb_max_y = parent_rect.max_y();
  new_abs_x = new_abs_x.min(cb_max_x - abs_rect.width()).max(cb_min_x);
  new_abs_y = new_abs_y.min(cb_max_y - abs_rect.height()).max(cb_min_y);

  let delta = Point::new(new_abs_x - abs_rect.x(), new_abs_y - abs_rect.y());
  if delta.x.abs() > f32::EPSILON || delta.y.abs() > f32::EPSILON {
    *fragment = fragment.translate(delta);
  }
}

fn paint_fragment_tree_with_scroll(
  mut fragment_tree: FragmentTree,
  scroll_x: f32,
  scroll_y: f32,
  viewport_override: Option<Size>,
  background: Rgba,
  font_context: &FontContext,
  image_cache: &ImageCache,
  device_pixel_ratio: f32,
) -> Result<Pixmap> {
  let expand_full_page = std::env::var("FASTR_FULL_PAGE")
    .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
    .unwrap_or(false);

  let viewport_size = viewport_override.unwrap_or_else(|| fragment_tree.viewport_size());
  let scroll_state = crate::scroll::ScrollState::with_viewport(Point::new(scroll_x, scroll_y));
  let scroll_result = crate::scroll::apply_scroll_snap(&mut fragment_tree, &scroll_state);
  let scroll = scroll_result.state.viewport;

  animation::apply_scroll_driven_animations(&mut fragment_tree, scroll);

  apply_sticky_offsets_with_context(
    font_context,
    &mut fragment_tree.root,
    Rect::from_xywh(0.0, 0.0, viewport_size.width, viewport_size.height),
    scroll,
    viewport_size,
  );

  let viewport_width_px = viewport_size.width.max(1.0).ceil() as u32;
  let viewport_height_px = viewport_size.height.max(1.0).ceil() as u32;

  let (target_width, target_height) = if expand_full_page {
    let content_bounds = fragment_tree.content_size();
    let w = viewport_width_px.max(content_bounds.max_x().ceil().max(1.0) as u32);
    let h = viewport_height_px.max(content_bounds.max_y().ceil().max(1.0) as u32);
    (w, h)
  } else {
    (viewport_width_px, viewport_height_px)
  };

  let offset = Point::new(-scroll.x, -scroll.y);
  paint_tree_with_resources_scaled_offset(
    &fragment_tree,
    target_width,
    target_height,
    background,
    font_context.clone(),
    image_cache.clone(),
    device_pixel_ratio,
    offset,
  )
}

/// Options controlling `layout_document` pagination behavior.
#[derive(Debug, Clone, Copy)]
pub struct LayoutDocumentOptions {
  /// Whether paginated pages should be stacked along the block axis or left untranslated.
  pub page_stacking: PageStacking,
}

impl Default for LayoutDocumentOptions {
  fn default() -> Self {
    Self {
      page_stacking: PageStacking::Stacked { gap: 0.0 },
    }
  }
}

impl LayoutDocumentOptions {
  /// Creates a new options struct with defaults.
  pub fn new() -> Self {
    Self::default()
  }

  /// Overrides how paginated pages are positioned in the returned fragment tree.
  pub fn with_page_stacking(mut self, stacking: PageStacking) -> Self {
    self.page_stacking = stacking;
    self
  }
}

fn resolve_fetcher(
  config: &FastRenderConfig,
  fetcher: Option<Arc<dyn ResourceFetcher>>,
) -> Arc<dyn ResourceFetcher> {
  if let Some(fetcher) = fetcher {
    return fetcher;
  }

  if let Some(cache) = config.resource_cache {
    Arc::new(CachingFetcher::with_config(
      HttpFetcher::new().with_policy(config.resource_policy.clone()),
      cache,
    ))
  } else {
    Arc::new(HttpFetcher::new().with_policy(config.resource_policy.clone()))
  }
}

fn build_image_cache(
  base_url: &Option<String>,
  fetcher: Arc<dyn ResourceFetcher>,
  config: ImageCacheConfig,
) -> ImageCache {
  match base_url {
    Some(url) => ImageCache::with_base_url_and_fetcher_and_config(url.clone(), fetcher, config),
    None => ImageCache::with_fetcher_and_config(fetcher, config),
  }
}

impl FastRender {
  fn resolve_scaled_metrics(&self, style: &ComputedStyle) -> Option<ScaledMetrics> {
    let italic = matches!(style.font_style, crate::style::types::FontStyle::Italic);
    let oblique = matches!(style.font_style, crate::style::types::FontStyle::Oblique(_));
    let stretch = FontStretch::from_percentage(style.font_stretch.to_percentage());

    self
      .font_context
      .get_font_full(
        &style.font_family,
        style.font_weight.to_u16(),
        if italic {
          DbFontStyle::Italic
        } else if oblique {
          DbFontStyle::Oblique
        } else {
          DbFontStyle::Normal
        },
        stretch,
      )
      .or_else(|| self.font_context.get_sans_serif())
      .and_then(|font| font.metrics().ok())
      .map(|m| m.scale(style.font_size))
  }

  fn box_generation_options(&self) -> BoxGenerationOptions {
    BoxGenerationOptions::default().with_compat_profile(self.compat_profile)
  }

  fn from_parts(
    config: FastRenderConfig,
    fetcher: Arc<dyn ResourceFetcher>,
    font_context: FontContext,
    image_cache_config: ImageCacheConfig,
  ) -> Result<Self> {
    let layout_config = LayoutConfig::for_viewport(Size::new(
      config.default_width as f32,
      config.default_height as f32,
    ))
    .with_identifier("api");
    let layout_engine = LayoutEngine::with_font_context(layout_config, font_context.clone());
    let image_cache = build_image_cache(&config.base_url, Arc::clone(&fetcher), image_cache_config);

    Ok(Self {
      font_context,
      layout_engine,
      image_cache,
      fetcher,
      diagnostics: None,
      background_color: config.background_color,
      default_width: config.default_width,
      default_height: config.default_height,
      device_pixel_ratio: config.device_pixel_ratio,
      apply_meta_viewport: config.apply_meta_viewport,
      pending_device_size: None,
      base_url: config.base_url.clone(),
      dom_compat_mode: config.dom_compat_mode,
      compat_profile: config.compat_profile,
      resource_policy: ResourceAccessPolicy {
        document_origin: config
          .base_url
          .as_ref()
          .and_then(|url| origin_from_url(url)),
        allow_file_from_http: config.allow_file_from_http,
        block_mixed_content: config.block_mixed_content,
      },
      resource_context: None,
      max_iframe_depth: config.max_iframe_depth,
      runtime_toggles: config.runtime_toggles.clone(),
    })
  }

  /// Creates a new FastRender instance with default configuration
  ///
  /// # Returns
  ///
  /// Returns `Ok(FastRender)` on success, or an error if initialization fails.
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// use fastrender::api::FastRender;
  ///
  /// let mut renderer = FastRender::new()?;
  /// ```
  ///
  /// # Errors
  ///
  /// Returns an error if:
  /// - Font system initialization fails
  /// - No system fonts are available
  pub fn new() -> Result<Self> {
    Self::with_config(FastRenderConfig::default())
  }

  /// Creates a builder for configuring FastRender
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// use fastrender::FastRender;
  ///
  /// let mut renderer = FastRender::builder()
  ///     .viewport_size(1024, 768)
  ///     .background_color(Rgba::WHITE)
  ///     .build()?;
  /// ```
  pub fn builder() -> FastRenderBuilder {
    FastRenderBuilder::new()
  }

  /// Creates a new FastRender instance with custom configuration
  ///
  /// # Arguments
  ///
  /// * `config` - Configuration options for the renderer
  ///
  /// # Returns
  ///
  /// Returns `Ok(FastRender)` on success, or an error if initialization fails.
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// use fastrender::api::{FastRender, FastRenderConfig};
  /// use fastrender::Rgba;
  ///
  /// let config = FastRenderConfig::new()
  ///     .with_default_background(Rgba::rgb(240, 240, 240))
  ///     .with_default_viewport(1024, 768);
  ///
  /// let mut renderer = FastRender::with_config(config)?;
  /// ```
  pub fn with_config(config: FastRenderConfig) -> Result<Self> {
    Self::with_config_and_fetcher(config, None)
  }

  /// Creates a new FastRender instance with custom configuration and fetcher
  ///
  /// # Arguments
  ///
  /// * `config` - Configuration options for the renderer
  /// * `fetcher` - Optional custom resource fetcher (uses HttpFetcher if None)
  pub fn with_config_and_fetcher(
    config: FastRenderConfig,
    fetcher: Option<Arc<dyn ResourceFetcher>>,
  ) -> Result<Self> {
    let font_config = config.font_config.clone();
    let fetcher = resolve_fetcher(&config, fetcher);
    let font_context =
      FontContext::with_resource_fetcher_and_config(font_config, Arc::clone(&fetcher));
    Self::from_parts(config, fetcher, font_context, ImageCacheConfig::default())
  }

  fn resolve_runtime_toggles(&self, options: &RenderOptions) -> Arc<RuntimeToggles> {
    options
      .runtime_toggles
      .clone()
      .unwrap_or_else(|| self.runtime_toggles.clone())
  }

  /// Renders HTML/CSS to a pixmap
  ///
  /// This is the main entry point for rendering. It takes HTML (which may include
  /// embedded CSS via `<style>` tags) and produces a rendered image.
  ///
  /// # Arguments
  ///
  /// * `html` - HTML source code (may include embedded `<style>` tags)
  /// * `width` - Viewport width in pixels
  /// * `height` - Viewport height in pixels
  ///
  /// # Returns
  ///
  /// Returns a `Pixmap` containing the rendered image, or an error if rendering fails.
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// use fastrender::api::FastRender;
  ///
  /// let mut renderer = FastRender::new()?;
  ///
  /// let html = r#"
  ///     <html>
  ///         <head>
  ///             <style>
  ///                 body { background: white; }
  ///                 h1 { color: navy; font-size: 24px; }
  ///             </style>
  ///         </head>
  ///         <body>
  ///             <h1>Hello, World!</h1>
  ///         </body>
  ///     </html>
  /// "#;
  ///
  /// let pixmap = renderer.render_html(html, 800, 600)?;
  /// pixmap.save_png("output.png")?;
  /// ```
  ///
  /// # Errors
  ///
  /// Returns an error if:
  /// - HTML parsing fails
  /// - CSS parsing fails
  /// - Layout computation fails
  /// - Painting fails
  /// - Invalid dimensions (width or height is 0)
  pub fn render_html(&mut self, html: &str, width: u32, height: u32) -> Result<Pixmap> {
    let options = RenderOptions::default().with_viewport(width, height);
    self.render_html_with_options(html, options)
  }

  /// Renders HTML with scroll offsets applied to the viewport
  pub fn render_html_with_scroll(
    &mut self,
    html: &str,
    width: u32,
    height: u32,
    scroll_x: f32,
    scroll_y: f32,
  ) -> Result<Pixmap> {
    let options = RenderOptions::default()
      .with_viewport(width, height)
      .with_scroll(scroll_x, scroll_y);
    self.render_html_with_options(html, options)
  }

  /// Renders HTML with explicit per-request options.
  pub fn render_html_with_options(&mut self, html: &str, options: RenderOptions) -> Result<Pixmap> {
    let outputs = self.render_html_with_options_internal(html, options, None, None)?;
    Ok(outputs.pixmap)
  }

  /// Renders HTML with diagnostics, returning a `RenderResult`.
  pub fn render_html_with_diagnostics(
    &mut self,
    html: &str,
    mut options: RenderOptions,
  ) -> Result<RenderResult> {
    if matches!(options.diagnostics_level, DiagnosticsLevel::None) {
      let env_level = diagnostics_level_from_env();
      if !matches!(env_level, DiagnosticsLevel::None) {
        options.diagnostics_level = env_level;
      }
    }
    let diagnostics_level = options.diagnostics_level;
    let mut stats_recorder = (!matches!(diagnostics_level, DiagnosticsLevel::None))
      .then(|| RenderStatsRecorder::new(diagnostics_level));
    let restore_cascade_profile = if matches!(diagnostics_level, DiagnosticsLevel::Verbose) {
      Some(crate::style::cascade::cascade_profile_enabled())
    } else {
      None
    };
    if stats_recorder.is_some() {
      crate::image_loader::enable_image_cache_diagnostics();
      crate::paint::painter::enable_paint_diagnostics();
      intrinsic_cache_reset_counters();
      crate::layout::formatting_context::layout_cache_reset_counters();
      if matches!(diagnostics_level, DiagnosticsLevel::Verbose) {
        crate::style::cascade::set_cascade_profile_enabled(true);
        crate::style::cascade::reset_cascade_profile();
      }
    }

    let diagnostics = Arc::new(Mutex::new(RenderDiagnostics::default()));
    let previous_sink = self.diagnostics.take();
    self.set_diagnostics_sink(Some(Arc::clone(&diagnostics)));
    let outputs = self.render_html_with_options_internal(
      html,
      options,
      None,
      stats_recorder.as_mut(),
    );
    self.set_diagnostics_sink(previous_sink);
    let outputs = match outputs {
      Ok(outputs) => outputs,
      Err(err) => {
        if stats_recorder.is_some() {
          let _ = crate::image_loader::take_image_cache_diagnostics();
          let _ = crate::paint::painter::take_paint_diagnostics();
        }
        if let Some(previous) = restore_cascade_profile {
          crate::style::cascade::set_cascade_profile_enabled(previous);
        }
        return Err(err);
      }
    };

    if let Some(recorder) = stats_recorder {
      let mut stats = recorder.finish();
      merge_image_cache_diagnostics(&mut stats);
      if let Ok(mut guard) = diagnostics.lock() {
        guard.stats = Some(stats);
      }
    }
    if let Some(previous) = restore_cascade_profile {
      crate::style::cascade::set_cascade_profile_enabled(previous);
    }
    let diagnostics = diagnostics.lock().unwrap().clone();

    Ok(RenderResult {
      pixmap: outputs.pixmap,
      accessibility: outputs.accessibility,
      diagnostics,
    })
  }

  /// Renders HTML with options while capturing requested artifacts.
  pub fn render_html_with_options_and_artifacts(
    &mut self,
    html: &str,
    options: RenderOptions,
    artifacts: &mut RenderArtifacts,
  ) -> Result<Pixmap> {
    let outputs = self.render_html_with_options_internal(html, options, Some(artifacts), None)?;
    Ok(outputs.pixmap)
  }

  /// Renders HTML and returns both the pixmap and accessibility tree.
  pub fn render_html_with_accessibility(
    &mut self,
    html: &str,
    mut options: RenderOptions,
  ) -> Result<(Pixmap, AccessibilityNode)> {
    options.capture_accessibility = true;
    let outputs = self.render_html_with_options_internal(html, options, None, None)?;
    let Some(tree) = outputs.accessibility else {
      return Err(Error::Render(RenderError::InvalidParameters {
        message: "Accessibility capture was requested but not produced".to_string(),
      }));
    };

    Ok((outputs.pixmap, tree))
  }

  /// Convenience wrapper to render HTML with a viewport and capture accessibility output.
  pub fn render_html_with_accessibility_viewport(
    &mut self,
    html: &str,
    width: u32,
    height: u32,
  ) -> Result<(Pixmap, AccessibilityNode)> {
    let options = RenderOptions::new()
      .with_viewport(width, height)
      .with_accessibility(true);
    self.render_html_with_accessibility(html, options)
  }

  fn render_html_with_options_internal(
    &mut self,
    html: &str,
    options: RenderOptions,
    artifacts: Option<&mut RenderArtifacts>,
    stats: Option<&mut RenderStatsRecorder>,
  ) -> Result<RenderOutputs> {
    let toggles = self.resolve_runtime_toggles(&options);
    runtime::with_runtime_toggles(toggles, || {
      let trace = TraceSession::from_options(Some(&options));
      let trace_handle = trace.handle();
      let _root_span = trace_handle.span("render", "pipeline");

      let deadline = RenderDeadline::new(options.timeout, options.cancel_callback.clone());
      let result = self.render_html_with_options_internal_with_deadline(
        html,
        options,
        artifacts,
        Some(&deadline),
        stats,
        trace_handle,
      );
      drop(_root_span);
      trace.finalize(result)
    })
  }

  fn render_html_with_options_internal_with_deadline(
    &mut self,
    html: &str,
    options: RenderOptions,
    artifacts: Option<&mut RenderArtifacts>,
    deadline: Option<&RenderDeadline>,
    stats: Option<&mut RenderStatsRecorder>,
    trace: &TraceHandle,
  ) -> Result<RenderOutputs> {
    let (width, height) = options
      .viewport
      .unwrap_or((self.default_width, self.default_height));
    let original_dpr = self.device_pixel_ratio;
    if let Some(dpr) = options.device_pixel_ratio {
      self.device_pixel_ratio = dpr;
    }

    let shared_diagnostics = self
      .diagnostics
      .as_ref()
      .map(|diag| SharedRenderDiagnostics {
        inner: Arc::clone(diag),
      });
    let context = Some(self.build_resource_context(self.base_url.as_deref(), shared_diagnostics));
    let (prev_self, prev_image, prev_font) = self.push_resource_context(context);
    let result = self.render_html_internal(
      html,
      width,
      height,
      options.scroll_x,
      options.scroll_y,
      options.media_type,
      options.capture_accessibility,
      deadline,
      artifacts,
      stats,
      trace,
    );
    self.pop_resource_context(prev_self, prev_image, prev_font);
    let result = match result {
      Ok(outputs) => Ok(outputs),
      Err(err) => {
        if options.allow_partial {
          let stage = match &err {
            Error::Render(RenderError::Timeout { stage, .. }) => Some(*stage),
            Error::Layout(crate::error::LayoutError::Timeout { .. }) => Some(RenderStage::Layout),
            _ => None,
          };
          if let Some(stage) = stage {
            if let Some(diag) = &self.diagnostics {
              if let Ok(mut guard) = diag.lock() {
                guard.timeout_stage = Some(stage);
              }
            }
            let pixmap = self.render_error_overlay(width, height)?;
            return Ok(RenderOutputs {
              pixmap,
              accessibility: None,
            });
          }
        }
        Err(err)
      }
    };

    if options.device_pixel_ratio.is_some() {
      self.device_pixel_ratio = original_dpr;
    }

    result
  }

  /// Prepares HTML for repeated painting without re-running parse/style/layout.
  pub fn prepare_html(&mut self, html: &str, options: RenderOptions) -> Result<PreparedDocument> {
    self.prepare_html_internal(html, options)
  }

  fn render_html_internal(
    &mut self,
    html: &str,
    width: u32,
    height: u32,
    scroll_x: f32,
    scroll_y: f32,
    media_type: MediaType,
    capture_accessibility: bool,
    deadline: Option<&RenderDeadline>,
    mut artifacts: Option<&mut RenderArtifacts>,
    mut stats: Option<&mut RenderStatsRecorder>,
    trace: &TraceHandle,
  ) -> Result<RenderOutputs> {
    // Validate dimensions
    if width == 0 || height == 0 {
      return Err(Error::Render(RenderError::InvalidParameters {
        message: format!("Invalid dimensions: width={}, height={}", width, height),
      }));
    }

    let _deadline_guard = DeadlineGuard::install(deadline);

    let toggles = runtime::runtime_toggles();
    let timings_enabled = toggles.truthy("FASTR_RENDER_TIMINGS");
    let mut stage_start = timings_enabled.then(Instant::now);
    let overall_start = stage_start.clone();

    // Parse HTML to DOM
    let parse_timer = stats.as_deref().and_then(|rec| rec.timer());
    let dom = {
      let _span = trace.span("dom_parse", "parse");
      self.parse_html(html)?
    };
    if let Some(rec) = stats.as_deref_mut() {
      RenderStatsRecorder::record_ms(&mut rec.stats.timings.dom_parse_ms, parse_timer);
      rec.stats.counts.dom_nodes = Some(count_dom_nodes_api(&dom));
    }
    if let Some(store) = artifacts.as_deref_mut() {
      if store.request().dom {
        store.dom = Some(dom.clone());
      }
    }
    check_deadline(deadline, RenderStage::DomParse)?;

    if let Some(start) = stage_start.as_mut() {
      let now = Instant::now();
      eprintln!("timing:parse {:?}", now - *start);
      *start = now;
    }

    let requested_viewport = Size::new(width as f32, height as f32);
    let base_dpr = self.device_pixel_ratio;
    let meta_viewport = if self.apply_meta_viewport {
      crate::html::viewport::extract_viewport(&dom)
    } else {
      None
    };
    let resolved_viewport = resolve_viewport(requested_viewport, base_dpr, meta_viewport.as_ref());
    let layout_width = resolved_viewport.layout_viewport.width.max(1.0).round() as u32;
    let layout_height = resolved_viewport.layout_viewport.height.max(1.0).round() as u32;

    let previous_dpr = self.device_pixel_ratio;

    let result = (|| -> Result<RenderOutputs> {
      self.device_pixel_ratio = resolved_viewport.device_pixel_ratio;
      self.pending_device_size = Some(resolved_viewport.visual_viewport);
      let layout_timer = stats.as_deref().and_then(|rec| rec.timer());
      let layout_artifacts = self.layout_document_for_media_with_artifacts(
        &dom,
        layout_width,
        layout_height,
        media_type,
        LayoutDocumentOptions::default(),
        deadline,
        trace,
      )?;
      if let Some(rec) = stats.as_deref_mut() {
        RenderStatsRecorder::record_ms(&mut rec.stats.timings.layout_ms, layout_timer);
        rec.stats.counts.styled_nodes = Some(count_styled_nodes_api(&layout_artifacts.styled_tree));
        rec.stats.counts.box_nodes = Some(count_box_nodes_api(&layout_artifacts.box_tree.root));
        rec.stats.counts.fragments = Some(layout_artifacts.fragment_tree.fragment_count());

        let (lookups, hits, stores, block_calls, flex_calls, inline_calls) =
          intrinsic_cache_stats();
        rec.stats.layout.intrinsic_lookups = Some(lookups);
        rec.stats.layout.intrinsic_hits = Some(hits);
        rec.stats.layout.intrinsic_stores = Some(stores);
        rec.stats.layout.block_intrinsic = Some(block_calls);
        rec.stats.layout.flex_intrinsic = Some(flex_calls);
        rec.stats.layout.inline_intrinsic = Some(inline_calls);

        let (cache_lookups, cache_hits, cache_stores, cache_evictions) =
          crate::layout::formatting_context::layout_cache_stats();
        rec.stats.layout.layout_cache_lookups = Some(cache_lookups);
        rec.stats.layout.layout_cache_hits = Some(cache_hits);
        rec.stats.layout.layout_cache_stores = Some(cache_stores);
        rec.stats.layout.layout_cache_evictions = Some(cache_evictions);

        if rec.verbose() {
          let profile = crate::style::cascade::capture_cascade_profile();
          rec.stats.cascade.nodes = Some(profile.nodes);
          rec.stats.cascade.rule_candidates = Some(profile.rule_candidates);
          rec.stats.cascade.rule_matches = Some(profile.rule_matches);
          rec.stats.cascade.selector_time_ms = Some(profile.selector_time_ns as f64 / 1_000_000.0);
          rec.stats.cascade.declaration_time_ms =
            Some(profile.declaration_time_ns as f64 / 1_000_000.0);
          rec.stats.cascade.pseudo_time_ms = Some(profile.pseudo_time_ns as f64 / 1_000_000.0);
        }
      }
      self.pending_device_size = None;
      let LayoutArtifacts {
        styled_tree,
        box_tree,
        mut fragment_tree,
        ..
      } = layout_artifacts;
      if let Some(store) = artifacts.as_deref_mut() {
        if store.request().styled_tree {
          store.styled_tree = Some(styled_tree.clone());
        }
        if store.request().box_tree {
          store.box_tree = Some(box_tree.clone());
        }
      }
      let layout_viewport = fragment_tree.viewport_size();
      if toggles.truthy("FASTR_LOG_FRAG_BOUNDS") {
        let bbox = fragment_tree.content_size();
        eprintln!(
                "[frag-bounds] viewport=({}x{}) bbox=({:.1},{:.1})→({:.1},{:.1}) size=({:.1}x{:.1}) fragments={}",
                layout_viewport.width,
                layout_viewport.height,
                bbox.min_x(),
                bbox.min_y(),
                bbox.max_x(),
                bbox.max_y(),
                bbox.width(),
                bbox.height(),
                fragment_tree.fragment_count()
            );
      }

      if let Some(limit) = toggles.usize("FASTR_DUMP_TEXT_FRAGMENTS") {
        let mut total = 0usize;
        let mut stack = vec![(&fragment_tree.root, crate::geometry::Point::ZERO)];
        while let Some((frag, offset)) = stack.pop() {
          let abs = crate::geometry::Rect::from_xywh(
            frag.bounds.x() + offset.x,
            frag.bounds.y() + offset.y,
            frag.bounds.width(),
            frag.bounds.height(),
          );
          if let crate::tree::fragment_tree::FragmentContent::Text { text, .. } = &frag.content {
            if total < limit {
              eprintln!(
                "text frag {} @ ({:.1},{:.1},{:.1},{:.1}) {:?}",
                total,
                abs.x(),
                abs.y(),
                abs.width(),
                abs.height(),
                text.chars().take(80).collect::<String>()
              );
            }
            total += 1;
          }
          for child in frag.children.iter().rev() {
            stack.push((child, crate::geometry::Point::new(abs.x(), abs.y())));
          }
        }
        eprintln!("total text fragments: {}", total);
      }

      if let Some(query) = toggles.get("FASTR_TRACE_TEXT") {
        fn describe_node(node: &crate::tree::fragment_tree::FragmentNode) -> String {
          let display = node
            .style
            .as_ref()
            .map(|s| format!("{:?}", s.display))
            .unwrap_or_else(|| "None".to_string());
          match &node.content {
            crate::tree::fragment_tree::FragmentContent::Block { box_id } => {
              format!("Block(box_id={:?}, display={})", box_id, display)
            }
            crate::tree::fragment_tree::FragmentContent::Inline {
              box_id,
              fragment_index,
            } => {
              format!(
                "Inline(box_id={:?}, fragment_index={}, display={})",
                box_id, fragment_index, display
              )
            }
            crate::tree::fragment_tree::FragmentContent::Line { baseline } => {
              format!("Line(baseline={:.3}, display={})", baseline, display)
            }
            crate::tree::fragment_tree::FragmentContent::Text {
              text,
              box_id,
              baseline_offset,
              ..
            } => {
              let preview: String = text.chars().take(80).collect();
              format!(
                "Text(box_id={:?}, baseline={:.3}, display={}, text=\"{}\")",
                box_id, baseline_offset, display, preview
              )
            }
            crate::tree::fragment_tree::FragmentContent::Replaced { box_id, .. } => {
              format!("Replaced(box_id={:?}, display={})", box_id, display)
            }
          }
        }

        fn trace_text<'a>(
          node: &'a crate::tree::fragment_tree::FragmentNode,
          offset: crate::geometry::Point,
          query: &str,
          trail: &mut Vec<String>,
        ) -> bool {
          let raw = node.bounds;
          let abs = crate::geometry::Rect::from_xywh(
            node.bounds.x() + offset.x,
            node.bounds.y() + offset.y,
            node.bounds.width(),
            node.bounds.height(),
          );
          let label = format!(
            "{} raw=({:.1},{:.1},{:.1},{:.1}) abs=({:.1},{:.1},{:.1},{:.1})",
            describe_node(node),
            raw.x(),
            raw.y(),
            raw.width(),
            raw.height(),
            abs.x(),
            abs.y(),
            abs.width(),
            abs.height()
          );
          trail.push(label);
          if let crate::tree::fragment_tree::FragmentContent::Text { text, .. } = &node.content {
            if text.contains(query) {
              eprintln!("trace for {:?}:", query);
              for (depth, entry) in trail.iter().enumerate() {
                eprintln!("  {}{}", "  ".repeat(depth), entry);
              }
              trail.pop();
              return true;
            }
          }
          let child_offset = crate::geometry::Point::new(abs.x(), abs.y());
          for child in &node.children {
            if trace_text(child, child_offset, query, trail) {
              trail.pop();
              return true;
            }
          }
          trail.pop();
          false
        }
        let mut trail = Vec::new();
        trace_text(
          &fragment_tree.root,
          crate::geometry::Point::ZERO,
          &query,
          &mut trail,
        );
      }

      if let Some(start) = stage_start.as_mut() {
        let now = Instant::now();
        eprintln!("timing:layout_document {:?}", now - *start);
        *start = now;
      }

      let expand_full_page = toggles.truthy("FASTR_FULL_PAGE");
      let viewport_size = layout_viewport;
      let scroll_state = crate::scroll::ScrollState::with_viewport(Point::new(scroll_x, scroll_y));
      let scroll_result = crate::scroll::apply_scroll_snap(&mut fragment_tree, &scroll_state);
      let scroll = scroll_result.state.viewport;

      animation::apply_scroll_driven_animations(&mut fragment_tree, scroll);

      self.apply_sticky_offsets(
        &mut fragment_tree.root,
        Rect::from_xywh(0.0, 0.0, viewport_size.width, viewport_size.height),
        scroll,
        viewport_size,
      );

      let viewport_width_px = viewport_size.width.max(1.0).ceil() as u32;
      let viewport_height_px = viewport_size.height.max(1.0).ceil() as u32;

      let (target_width, target_height) = if expand_full_page {
        let content_bounds = fragment_tree.content_size();
        let w = viewport_width_px.max(content_bounds.max_x().ceil().max(1.0) as u32);
        let h = viewport_height_px.max(content_bounds.max_y().ceil().max(1.0) as u32);
        (w, h)
      } else {
        (viewport_width_px, viewport_height_px)
      };

      if let Some(store) = artifacts.as_deref_mut() {
        if store.request().fragment_tree {
          store.fragment_tree = Some(fragment_tree.clone());
        }
        if store.request().display_list {
          let viewport = fragment_tree.viewport_size();
          let mut builder = DisplayListBuilder::with_image_cache(self.image_cache.clone())
            .with_font_context(self.font_context.clone())
            .with_device_pixel_ratio(self.device_pixel_ratio)
            .with_viewport_size(viewport.width, viewport.height);
          if let Some(base_url) = &self.base_url {
            builder.set_base_url(base_url.clone());
          }
          let display_list = builder.build_tree(&fragment_tree);
          store.display_list = Some(display_list);
        }
      }

      // Paint to pixmap
      let offset = Point::new(-scroll.x, -scroll.y);
      let pixmap =
        self.paint_with_offset_traced(&fragment_tree, target_width, target_height, offset, trace)?;
      if let Some(rec) = stats.as_deref_mut() {
        if let Some(diag) = crate::paint::painter::take_paint_diagnostics() {
          rec.stats.timings.paint_build_ms = Some(diag.build_ms);
          rec.stats.timings.paint_rasterize_ms = Some(diag.raster_ms);
          rec.stats.paint.display_items = Some(diag.command_count);
        }
      }

      if let Some(start) = stage_start {
        let now = Instant::now();
        eprintln!("timing:paint {:?}", now - start);
        if let Some(overall) = overall_start {
          eprintln!("timing:render_html_total {:?}", now - overall);
        }
      }

      let accessibility = if capture_accessibility {
        Some(crate::accessibility::build_accessibility_tree(&styled_tree))
      } else {
        None
      };

      Ok(RenderOutputs {
        pixmap,
        accessibility,
      })
    })();

    self.device_pixel_ratio = previous_dpr;
    self.pending_device_size = None;

    result
  }

  fn prepare_html_internal(
    &mut self,
    html: &str,
    options: RenderOptions,
  ) -> Result<PreparedDocument> {
    let (width, height) = options
      .viewport
      .unwrap_or((self.default_width, self.default_height));
    if width == 0 || height == 0 {
      return Err(Error::Render(RenderError::InvalidParameters {
        message: format!("Invalid dimensions: width={}, height={}", width, height),
      }));
    }

    let timings_enabled = std::env::var_os("FASTR_RENDER_TIMINGS").is_some();
    let mut stage_start = timings_enabled.then(Instant::now);

    let dom = self.parse_html(html)?;

    if let Some(start) = stage_start.as_mut() {
      let now = Instant::now();
      eprintln!("timing:parse {:?}", now - *start);
      *start = now;
    }

    let requested_viewport = Size::new(width as f32, height as f32);
    let base_dpr = options
      .device_pixel_ratio
      .unwrap_or(self.device_pixel_ratio);
    let meta_viewport = if self.apply_meta_viewport {
      crate::html::viewport::extract_viewport(&dom)
    } else {
      None
    };
    let resolved_viewport = resolve_viewport(requested_viewport, base_dpr, meta_viewport.as_ref());
    let layout_width = resolved_viewport.layout_viewport.width.max(1.0).round() as u32;
    let layout_height = resolved_viewport.layout_viewport.height.max(1.0).round() as u32;

    let previous_dpr = self.device_pixel_ratio;
    let artifacts_result = (|| -> Result<LayoutArtifacts> {
      self.device_pixel_ratio = resolved_viewport.device_pixel_ratio;
      self.pending_device_size = Some(resolved_viewport.visual_viewport);
      let trace = TraceHandle::disabled();
      self.layout_document_for_media_with_artifacts(
        &dom,
        layout_width,
        layout_height,
        options.media_type,
        LayoutDocumentOptions::default(),
        None,
        &trace,
      )
    })();

    self.device_pixel_ratio = previous_dpr;
    self.pending_device_size = None;
    let artifacts = artifacts_result?;

    let layout_viewport = artifacts.fragment_tree.viewport_size();
    if std::env::var("FASTR_LOG_FRAG_BOUNDS")
      .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
      .unwrap_or(false)
    {
      let bbox = artifacts.fragment_tree.content_size();
      eprintln!(
              "[frag-bounds] viewport=({}x{}) bbox=({:.1},{:.1})→({:.1},{:.1}) size=({:.1}x{:.1}) fragments={}",
              layout_viewport.width,
              layout_viewport.height,
              bbox.min_x(),
              bbox.min_y(),
              bbox.max_x(),
              bbox.max_y(),
              bbox.width(),
              bbox.height(),
              artifacts.fragment_tree.fragment_count()
          );
    }

    if let Ok(v) = std::env::var("FASTR_DUMP_TEXT_FRAGMENTS") {
      if v != "0" && !v.eq_ignore_ascii_case("false") {
        let limit: usize = v.parse().unwrap_or(20);
        let mut total = 0usize;
        let mut stack = vec![(&artifacts.fragment_tree.root, crate::geometry::Point::ZERO)];
        while let Some((frag, offset)) = stack.pop() {
          let abs = crate::geometry::Rect::from_xywh(
            frag.bounds.x() + offset.x,
            frag.bounds.y() + offset.y,
            frag.bounds.width(),
            frag.bounds.height(),
          );
          if let crate::tree::fragment_tree::FragmentContent::Text { text, .. } = &frag.content {
            if total < limit {
              eprintln!(
                "text frag {} @ ({:.1},{:.1},{:.1},{:.1}) {:?}",
                total,
                abs.x(),
                abs.y(),
                abs.width(),
                abs.height(),
                text.chars().take(80).collect::<String>()
              );
            }
            total += 1;
          }
          for child in frag.children.iter().rev() {
            stack.push((child, crate::geometry::Point::new(abs.x(), abs.y())));
          }
        }
        eprintln!("total text fragments: {}", total);
      }
    }

    if let Ok(query) = std::env::var("FASTR_TRACE_TEXT") {
      fn describe_node(node: &crate::tree::fragment_tree::FragmentNode) -> String {
        let display = node
          .style
          .as_ref()
          .map(|s| format!("{:?}", s.display))
          .unwrap_or_else(|| "None".to_string());
        match &node.content {
          crate::tree::fragment_tree::FragmentContent::Block { box_id } => {
            format!("Block(box_id={:?}, display={})", box_id, display)
          }
          crate::tree::fragment_tree::FragmentContent::Inline {
            box_id,
            fragment_index,
          } => {
            format!(
              "Inline(box_id={:?}, fragment_index={}, display={})",
              box_id, fragment_index, display
            )
          }
          crate::tree::fragment_tree::FragmentContent::Line { baseline } => {
            format!("Line(baseline={:.3}, display={})", baseline, display)
          }
          crate::tree::fragment_tree::FragmentContent::Text {
            text,
            box_id,
            baseline_offset,
            ..
          } => {
            let preview: String = text.chars().take(80).collect();
            format!(
              "Text(box_id={:?}, baseline={:.3}, display={}, text=\"{}\")",
              box_id, baseline_offset, display, preview
            )
          }
          crate::tree::fragment_tree::FragmentContent::Replaced { box_id, .. } => {
            format!("Replaced(box_id={:?}, display={})", box_id, display)
          }
        }
      }

      fn trace_text<'a>(
        node: &'a crate::tree::fragment_tree::FragmentNode,
        offset: crate::geometry::Point,
        query: &str,
        trail: &mut Vec<String>,
      ) -> bool {
        let raw = node.bounds;
        let abs = crate::geometry::Rect::from_xywh(
          node.bounds.x() + offset.x,
          node.bounds.y() + offset.y,
          node.bounds.width(),
          node.bounds.height(),
        );
        let label = format!(
          "{} raw=({:.1},{:.1},{:.1},{:.1}) abs=({:.1},{:.1},{:.1},{:.1})",
          describe_node(node),
          raw.x(),
          raw.y(),
          raw.width(),
          raw.height(),
          abs.x(),
          abs.y(),
          abs.width(),
          abs.height()
        );
        trail.push(label);
        if let crate::tree::fragment_tree::FragmentContent::Text { text, .. } = &node.content {
          if text.contains(query) {
            eprintln!("trace for {:?}:", query);
            for (depth, entry) in trail.iter().enumerate() {
              eprintln!("  {}{}", "  ".repeat(depth), entry);
            }
            trail.pop();
            return true;
          }
        }
        let child_offset = crate::geometry::Point::new(abs.x(), abs.y());
        for child in &node.children {
          if trace_text(child, child_offset, query, trail) {
            trail.pop();
            return true;
          }
        }
        trail.pop();
        false
      }
      let mut trail = Vec::new();
      trace_text(
        &artifacts.fragment_tree.root,
        crate::geometry::Point::ZERO,
        &query,
        &mut trail,
      );
    }

    if let Some(start) = stage_start {
      let now = Instant::now();
      eprintln!("timing:layout_document {:?}", now - start);
    }

    Ok(PreparedDocument {
      dom: artifacts.dom,
      stylesheet: artifacts.stylesheet,
      styled_tree: artifacts.styled_tree,
      box_tree: artifacts.box_tree,
      fragment_tree: artifacts.fragment_tree,
      layout_viewport,
      visual_viewport: resolved_viewport.visual_viewport,
      device_pixel_ratio: resolved_viewport.device_pixel_ratio,
      page_zoom: resolved_viewport.zoom,
      background_color: self.background_color,
      default_scroll: Point::new(options.scroll_x, options.scroll_y),
      font_context: self.font_context.clone(),
      image_cache: self.image_cache.clone(),
    })
  }

  /// Renders HTML with a custom background color

  /// Renders HTML with a custom background color
  ///
  /// # Arguments
  ///
  /// * `html` - HTML source code
  /// * `width` - Viewport width in pixels
  /// * `height` - Viewport height in pixels
  /// * `background` - Background color for the canvas
  ///
  /// # Returns
  ///
  /// Returns a `Pixmap` containing the rendered image.
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// use fastrender::api::FastRender;
  /// use fastrender::Rgba;
  ///
  /// let mut renderer = FastRender::new()?;
  /// let pixmap = renderer.render_html_with_background(
  ///     "<h1>Hello!</h1>",
  ///     800,
  ///     600,
  ///     Rgba::rgb(240, 240, 240),
  /// )?;
  /// ```
  pub fn render_html_with_background(
    &mut self,
    html: &str,
    width: u32,
    height: u32,
    background: Rgba,
  ) -> Result<Pixmap> {
    let original_background = self.background_color;
    self.background_color = background;
    let result = self.render_html(html, width, height);
    self.background_color = original_background;
    result
  }

  /// Fetches and renders a document from a URL using default options.
  pub fn render_url(&mut self, url: &str) -> Result<RenderResult> {
    self
      .render_url_with_options_report(url, RenderOptions::default(), RenderArtifactRequest::none())
      .map(RenderReport::into_result)
  }

  /// Fetches and renders a document from a URL with explicit options.
  pub fn render_url_with_options(
    &mut self,
    url: &str,
    options: RenderOptions,
  ) -> Result<RenderResult> {
    self
      .render_url_with_options_report(url, options, RenderArtifactRequest::none())
      .map(RenderReport::into_result)
  }

  /// Fetches and renders a document from a URL with explicit options while capturing artifacts.
  pub fn render_url_with_options_report(
    &mut self,
    url: &str,
    mut options: RenderOptions,
    artifacts: RenderArtifactRequest,
  ) -> Result<RenderReport> {
    let trace = TraceSession::from_options(Some(&options));
    let trace_handle = trace.handle();
    let _root_span = trace_handle.span("render", "pipeline");

    if matches!(options.diagnostics_level, DiagnosticsLevel::None) {
      let env_level = diagnostics_level_from_env();
      if !matches!(env_level, DiagnosticsLevel::None) {
        options.diagnostics_level = env_level;
      }
    }
    let diagnostics_level = options.diagnostics_level;
    let mut stats_recorder = (!matches!(diagnostics_level, DiagnosticsLevel::None))
      .then(|| RenderStatsRecorder::new(diagnostics_level));
    let restore_cascade_profile = if matches!(diagnostics_level, DiagnosticsLevel::Verbose) {
      Some(crate::style::cascade::cascade_profile_enabled())
    } else {
      None
    };
    if let Some(stats) = stats_recorder.as_mut() {
      crate::image_loader::enable_image_cache_diagnostics();
      crate::paint::painter::enable_paint_diagnostics();
      intrinsic_cache_reset_counters();
      crate::layout::formatting_context::layout_cache_reset_counters();
      if stats.verbose() {
        crate::style::cascade::set_cascade_profile_enabled(true);
        crate::style::cascade::reset_cascade_profile();
      }
      stats.record_fetch(ResourceKind::Document);
    }

    let (width, height) = options
      .viewport
      .unwrap_or((self.default_width, self.default_height));
    let diagnostics = Arc::new(Mutex::new(RenderDiagnostics::default()));
    self.set_diagnostics_sink(Some(Arc::clone(&diagnostics)));
    let result = (|| -> Result<RenderReport> {
      let resource = {
        let _span = trace_handle.span("html_fetch", "network");
        match self.fetcher.fetch(url) {
        Ok(res) => res,
        Err(e) => {
          if let Ok(mut guard) = diagnostics.lock() {
            guard.record_error(ResourceKind::Document, url, &e);
            guard.document_error = Some(e.to_string());
            if let Some(recorder) = stats_recorder.take() {
              let mut stats = recorder.finish();
              merge_image_cache_diagnostics(&mut stats);
              let _ = crate::paint::painter::take_paint_diagnostics();
              guard.stats = Some(stats);
            }
          }
          if options.allow_partial {
            let pixmap = self.render_error_overlay(width, height)?;
            let diagnostics = diagnostics.lock().unwrap().clone();
            return Ok(RenderReport {
              pixmap,
              accessibility: None,
              diagnostics,
              artifacts: RenderArtifacts::new(artifacts),
            });
          }
          let reason = e.to_string();
          let source = Arc::new(e);
          return Err(Error::Navigation(NavigationError::FetchFailed {
            url: url.to_string(),
            reason,
            source: Some(source),
          }));
        }
        }
      };

      let mut report = self.render_fetched_html_with_options_report_internal(
        &resource,
        Some(url),
        options,
        artifacts,
        stats_recorder.as_mut(),
        trace_handle,
      )?;
      if let Some(recorder) = stats_recorder.take() {
        let mut stats = recorder.finish();
        merge_image_cache_diagnostics(&mut stats);
        if let Ok(mut guard) = diagnostics.lock() {
          guard.stats = Some(stats);
        }
      }
      report.diagnostics = diagnostics.lock().unwrap().clone();
      Ok(report)
    })();

    self.set_diagnostics_sink(None);
    if let Some(previous) = restore_cascade_profile {
      crate::style::cascade::set_cascade_profile_enabled(previous);
    }
    if result.is_err() && stats_recorder.is_some() {
      let _ = crate::image_loader::take_image_cache_diagnostics();
      let _ = crate::paint::painter::take_paint_diagnostics();
    }
    drop(_root_span);
    trace.finalize(result)
  }

  /// Render already-fetched HTML while inlining linked stylesheets using the configured fetcher.
  pub fn render_fetched_html_with_options(
    &mut self,
    resource: &crate::resource::FetchedResource,
    base_hint: Option<&str>,
    options: RenderOptions,
  ) -> Result<RenderResult> {
    self
      .render_fetched_html_with_options_report(
        resource,
        base_hint,
        options,
        RenderArtifactRequest::none(),
      )
      .map(RenderReport::into_result)
  }

  /// Render already-fetched HTML while inlining linked stylesheets using the configured fetcher.
  /// Captures intermediate artifacts.
  pub fn render_fetched_html_with_options_report(
    &mut self,
    resource: &crate::resource::FetchedResource,
    base_hint: Option<&str>,
    options: RenderOptions,
    artifacts: RenderArtifactRequest,
  ) -> Result<RenderReport> {
    let trace = TraceSession::from_options(Some(&options));
    let trace_handle = trace.handle();
    let _root_span = trace_handle.span("render", "pipeline");
    let result = self.render_fetched_html_with_options_report_internal(
      resource,
      base_hint,
      options,
      artifacts,
      None,
      trace_handle,
    );
    drop(_root_span);
    trace.finalize(result)
  }

  fn render_fetched_html_with_options_report_internal(
    &mut self,
    resource: &crate::resource::FetchedResource,
    base_hint: Option<&str>,
    mut options: RenderOptions,
    artifacts: RenderArtifactRequest,
    stats: Option<&mut RenderStatsRecorder>,
    trace: &TraceHandle,
  ) -> Result<RenderReport> {
    let had_sink = self.diagnostics.is_some();
    let diagnostics = if let Some(existing) = &self.diagnostics {
      Arc::clone(existing)
    } else {
      let diag = Arc::new(Mutex::new(RenderDiagnostics::default()));
      self.set_diagnostics_sink(Some(Arc::clone(&diag)));
      diag
    };
    let mut stats = stats.map(|stats| &mut *stats);
    if stats.is_none() && matches!(options.diagnostics_level, DiagnosticsLevel::None) {
      let env_level = diagnostics_level_from_env();
      if !matches!(env_level, DiagnosticsLevel::None) {
        options.diagnostics_level = env_level;
      }
    }
    let mut local_recorder =
      if stats.is_none() && !matches!(options.diagnostics_level, DiagnosticsLevel::None) {
        Some(RenderStatsRecorder::new(options.diagnostics_level))
      } else {
        None
      };
    if stats.is_none() && local_recorder.is_some() {
      crate::image_loader::enable_image_cache_diagnostics();
      crate::paint::painter::enable_paint_diagnostics();
      intrinsic_cache_reset_counters();
      crate::layout::formatting_context::layout_cache_reset_counters();
    }
    let _cascade_profile_override = if stats.is_none()
      && local_recorder.is_some()
      && matches!(options.diagnostics_level, DiagnosticsLevel::Verbose)
    {
      Some(CascadeProfileOverride::enable())
    } else {
      None
    };
    if let Some(recorder) = local_recorder.as_mut() {
      stats = Some(recorder);
    }
    let hint = resource.final_url.as_deref().or(base_hint).unwrap_or("");
    let decode_start = stats.as_deref().and_then(|rec| rec.timer());
    let html = {
      let _span = trace.span("html_decode", "parse");
      decode_html_bytes(&resource.bytes, resource.content_type.as_deref())
    };
    if let Some(rec) = stats.as_deref_mut() {
      RenderStatsRecorder::record_ms(&mut rec.stats.timings.html_decode_ms, decode_start);
    }
    let base_url = infer_base_url(&html, hint).into_owned();
    self.set_base_url(base_url.clone());
    let deadline = RenderDeadline::new(options.timeout, options.cancel_callback.clone());
    let (width, height) = options
      .viewport
      .unwrap_or((self.default_width, self.default_height));

    let html_with_css = {
      let _span = trace.span("css_inline", "style");
      let mut guard = diagnostics.lock().unwrap();
      match self.inline_stylesheets(
        &html,
        &base_url,
        options.media_type,
        options.css_limit,
        &mut guard,
        Some(&deadline),
        stats.as_deref_mut(),
      ) {
        Ok(inlined) => inlined,
        Err(err) => {
          if options.allow_partial {
            if let RenderError::Timeout { stage, .. } = &err {
              guard.timeout_stage = Some(*stage);
              let diagnostics = guard.clone();
              if !had_sink {
                self.set_diagnostics_sink(None);
              }
              if local_recorder.is_some() {
                let _ = crate::image_loader::take_image_cache_diagnostics();
                let _ = crate::paint::painter::take_paint_diagnostics();
              }
              let pixmap = self.render_error_overlay(width, height)?;
              return Ok(RenderReport {
                pixmap,
                accessibility: None,
                diagnostics,
                artifacts: RenderArtifacts::new(artifacts),
              });
            }
          }
          drop(guard);
          if !had_sink {
            self.set_diagnostics_sink(None);
          }
          if local_recorder.is_some() {
            let _ = crate::image_loader::take_image_cache_diagnostics();
            let _ = crate::paint::painter::take_paint_diagnostics();
          }
          return Err(Error::Render(err));
        }
      }
    };
    let mut captured = RenderArtifacts::new(artifacts);
    let outputs = self.render_html_with_options_internal_with_deadline(
      &html_with_css,
      options,
      Some(&mut captured),
      Some(&deadline),
      stats.as_deref_mut(),
      trace,
    );
    if !had_sink {
      self.set_diagnostics_sink(None);
    }
    let outputs = match outputs {
      Ok(outputs) => outputs,
      Err(err) => {
        if local_recorder.is_some() {
          let _ = crate::image_loader::take_image_cache_diagnostics();
          let _ = crate::paint::painter::take_paint_diagnostics();
        }
        return Err(err);
      }
    };
    let diagnostics = diagnostics.lock().unwrap().clone();

    let mut report = RenderReport {
      pixmap: outputs.pixmap,
      accessibility: outputs.accessibility,
      diagnostics,
      artifacts: captured,
    };
    if let Some(recorder) = local_recorder {
      let mut finished = recorder.finish();
      merge_image_cache_diagnostics(&mut finished);
      report.diagnostics.stats = Some(finished);
    }
    Ok(report)
  }

  /// Renders an HTML string after inlining linked stylesheets.
  pub fn render_html_with_stylesheets(
    &mut self,
    html: &str,
    base_hint: &str,
    options: RenderOptions,
  ) -> Result<RenderResult> {
    self
      .render_html_with_stylesheets_report(html, base_hint, options, RenderArtifactRequest::none())
      .map(RenderReport::into_result)
  }

  /// Renders an HTML string after inlining linked stylesheets, capturing artifacts.
  pub fn render_html_with_stylesheets_report(
    &mut self,
    html: &str,
    base_hint: &str,
    options: RenderOptions,
    artifacts: RenderArtifactRequest,
  ) -> Result<RenderReport> {
    let trace = TraceSession::from_options(Some(&options));
    let trace_handle = trace.handle();
    let _root_span = trace_handle.span("render", "pipeline");

    let had_sink = self.diagnostics.is_some();
    let diagnostics = if let Some(existing) = &self.diagnostics {
      Arc::clone(existing)
    } else {
      let diag = Arc::new(Mutex::new(RenderDiagnostics::default()));
      self.set_diagnostics_sink(Some(Arc::clone(&diag)));
      diag
    };
    let base_url = infer_base_url(html, base_hint).into_owned();
    self.set_base_url(base_url.clone());
    let deadline = RenderDeadline::new(options.timeout, options.cancel_callback.clone());
    let (width, height) = options
      .viewport
      .unwrap_or((self.default_width, self.default_height));
    let shared_diagnostics = Some(SharedRenderDiagnostics {
      inner: Arc::clone(&diagnostics),
    });
    let context = Some(self.build_resource_context(Some(&base_url), shared_diagnostics));
    let (prev_self, prev_image, prev_font) = self.push_resource_context(context);
    let result = (|| -> Result<RenderReport> {
      let html_with_css = {
        let _span = trace_handle.span("css_inline", "style");
        let mut guard = diagnostics.lock().unwrap();
        match self.inline_stylesheets(
          html,
          &base_url,
          options.media_type,
          options.css_limit,
          &mut guard,
          Some(&deadline),
          None,
        ) {
          Ok(inlined) => inlined,
          Err(err) => {
            if options.allow_partial {
              if let RenderError::Timeout { stage, .. } = &err {
                guard.timeout_stage = Some(*stage);
                let diagnostics = guard.clone();
                let pixmap = self.render_error_overlay(width, height)?;
                return Ok(RenderReport {
                  pixmap,
                  accessibility: None,
                  diagnostics,
                  artifacts: RenderArtifacts::new(artifacts),
                });
              }
            }
            return Err(Error::Render(err));
          }
        }
      };
      let mut captured = RenderArtifacts::new(artifacts);
      let outputs = self.render_html_with_options_internal_with_deadline(
        &html_with_css,
        options,
        Some(&mut captured),
        Some(&deadline),
        None,
        trace_handle,
      )?;
      let diagnostics = diagnostics.lock().unwrap().clone();

      Ok(RenderReport {
        pixmap: outputs.pixmap,
        accessibility: outputs.accessibility,
        diagnostics,
        artifacts: captured,
      })
    })();
    self.pop_resource_context(prev_self, prev_image, prev_font);

    if !had_sink {
      self.set_diagnostics_sink(None);
    }
    drop(_root_span);
    trace.finalize(result)
  }

  /// Generate a debug snapshot of the full rendering pipeline for an HTML string.
  pub fn snapshot_pipeline(
    &mut self,
    html: &str,
    base_hint: &str,
    options: RenderOptions,
  ) -> Result<debug::snapshot::PipelineSnapshot> {
    let (width, height) = options
      .viewport
      .unwrap_or((self.default_width, self.default_height));

    let mut diagnostics = RenderDiagnostics::default();
    let base_url = infer_base_url(html, base_hint).into_owned();
    self.set_base_url(base_url.clone());

    let html_with_css = self
      .inline_stylesheets(
        html,
        &base_url,
        options.media_type,
        options.css_limit,
        &mut diagnostics,
        None,
        None,
      )
      .map_err(Error::Render)?;

    let requested_viewport = Size::new(width as f32, height as f32);
    let dom = self.parse_html(&html_with_css)?;
    let meta_viewport = if self.apply_meta_viewport {
      crate::html::viewport::extract_viewport(&dom)
    } else {
      None
    };

    let original_dpr = self.device_pixel_ratio;
    if let Some(dpr) = options.device_pixel_ratio {
      self.device_pixel_ratio = dpr;
    }

    let resolved_viewport = resolve_viewport(
      requested_viewport,
      self.device_pixel_ratio,
      meta_viewport.as_ref(),
    );
    let layout_width = resolved_viewport.layout_viewport.width.max(1.0).round() as u32;
    let layout_height = resolved_viewport.layout_viewport.height.max(1.0).round() as u32;

    let snapshot = (|| -> Result<debug::snapshot::PipelineSnapshot> {
      self.device_pixel_ratio = resolved_viewport.device_pixel_ratio;
      self.pending_device_size = Some(resolved_viewport.visual_viewport);

      let mut intermediates = self.layout_document_for_media_intermediates(
        &dom,
        layout_width,
        layout_height,
        options.media_type,
      )?;

      let viewport_size = intermediates.fragment_tree.viewport_size();
      let scroll_state =
        crate::scroll::ScrollState::with_viewport(Point::new(options.scroll_x, options.scroll_y));
      let scroll_result =
        crate::scroll::apply_scroll_snap(&mut intermediates.fragment_tree, &scroll_state);
      let scroll = scroll_result.state.viewport;

      animation::apply_scroll_driven_animations(&mut intermediates.fragment_tree, scroll);

      self.apply_sticky_offsets(
        &mut intermediates.fragment_tree.root,
        Rect::from_xywh(0.0, 0.0, viewport_size.width, viewport_size.height),
        scroll,
        viewport_size,
      );

      let mut display_list =
        DisplayListBuilder::new().build_with_stacking_tree(&intermediates.fragment_tree.root);
      for extra in &intermediates.fragment_tree.additional_fragments {
        let extra_list = DisplayListBuilder::new().build_with_stacking_tree(extra);
        display_list.append(extra_list);
      }

      Ok(debug::snapshot::snapshot_pipeline(
        &intermediates.dom,
        &intermediates.styled_tree,
        &intermediates.box_tree,
        &intermediates.fragment_tree,
        &display_list,
      ))
    })();

    self.device_pixel_ratio = original_dpr;
    self.pending_device_size = None;

    snapshot
  }

  /// Parses HTML into a DOM tree
  ///
  /// Use this when you need direct access to the parsed DOM structure.
  ///
  /// # Arguments
  ///
  /// * `html` - HTML source code
  ///
  /// # Returns
  ///
  /// Returns the root `DomNode` of the parsed document.
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// use fastrender::api::FastRender;
  ///
  /// let renderer = FastRender::new()?;
  /// let dom = renderer.parse_html("<div>Hello</div>")?;
  ///
  /// // Inspect the DOM structure
  /// assert!(dom.children.len() > 0);
  /// ```
  pub fn parse_html(&self, html: &str) -> Result<DomNode> {
    dom::parse_html_with_options(
      html,
      DomParseOptions {
        compatibility_mode: self.dom_compat_mode,
      },
    )
  }

  fn collect_document_stylesheet(
    &self,
    dom: &DomNode,
    media_ctx: &MediaContext,
    media_query_cache: &mut MediaQueryCache,
  ) -> StyleSheet {
    let mut combined_rules = Vec::new();
    let sources = extract_css_sources(dom);
    let fetcher = Arc::clone(&self.fetcher);
    let resource_context = self.resource_context.as_ref();
    let inline_loader = CssImportFetcher::new(
      self.base_url.clone(),
      Arc::clone(&fetcher),
      self.resource_context.clone(),
    );

    for source in sources {
      match source {
        StylesheetSource::Inline(inline) => {
          if inline.disabled || !Self::stylesheet_type_is_css(inline.type_attr.as_deref()) {
            continue;
          }
          if !Self::media_attr_allows(inline.media.as_deref(), media_ctx, media_query_cache) {
            continue;
          }
          if inline.css.trim().is_empty() {
            continue;
          }

          if let Ok(sheet) = parse_stylesheet(&inline.css) {
            let resolved = sheet.resolve_imports_with_cache(
              &inline_loader,
              self.base_url.as_deref(),
              media_ctx,
              Some(media_query_cache),
            );
            combined_rules.extend(resolved.rules);
          }
        }
        StylesheetSource::External(link) => {
          if link.disabled
            || !rel_list_contains_stylesheet(&link.rel)
            || !Self::stylesheet_type_is_css(link.type_attr.as_deref())
          {
            continue;
          }
          if !Self::media_attr_allows(link.media.as_deref(), media_ctx, media_query_cache) {
            continue;
          }
          if link.href.trim().is_empty() {
            continue;
          }

          let Some(stylesheet_url) = resolve_href_with_base(self.base_url.as_deref(), &link.href)
          else {
            continue;
          };

          if let Some(ctx) = resource_context {
            if ctx
              .check_allowed(ResourceKind::Stylesheet, &stylesheet_url)
              .is_err()
            {
              continue;
            }
          }

          match fetcher.fetch(&stylesheet_url) {
            Ok(resource) => {
              let mut css_text =
                decode_css_bytes(&resource.bytes, resource.content_type.as_deref());
              css_text = absolutize_css_urls(&css_text, &stylesheet_url);

              if let Ok(sheet) = parse_stylesheet(&css_text) {
                let loader = CssImportFetcher::new(
                  Some(stylesheet_url.clone()),
                  Arc::clone(&fetcher),
                  resource_context.cloned(),
                );
                let resolved = sheet.resolve_imports_with_cache(
                  &loader,
                  Some(&stylesheet_url),
                  media_ctx,
                  Some(media_query_cache),
                );
                combined_rules.extend(resolved.rules);
              }
            }
            Err(err) => {
              // Per spec, stylesheet loads are best-effort. On failure, continue.
              if let Some(diag) = &self.diagnostics {
                if let Ok(mut guard) = diag.lock() {
                  guard.record_error(ResourceKind::Stylesheet, &stylesheet_url, &err);
                }
              }
              continue;
            }
          }
        }
      }
    }

    StyleSheet {
      rules: combined_rules,
    }
  }

  fn stylesheet_type_is_css(type_attr: Option<&str>) -> bool {
    match type_attr {
      None => true,
      Some(value) => {
        let mime = value.split(';').next().map(str::trim).unwrap_or("");
        mime.is_empty() || mime.eq_ignore_ascii_case("text/css")
      }
    }
  }

  fn media_attr_allows(
    media_attr: Option<&str>,
    media_ctx: &MediaContext,
    cache: &mut MediaQueryCache,
  ) -> bool {
    match media_attr {
      None => true,
      Some(media) => {
        let trimmed = media.trim();
        if trimmed.is_empty() {
          return true;
        }

        match MediaQuery::parse_list(trimmed) {
          Ok(list) => media_ctx.evaluate_list_with_cache(&list, Some(cache)),
          Err(_) => false,
        }
      }
    }
  }

  /// Computes the accessibility tree for the given document.
  ///
  /// This runs HTML and CSS parsing to produce a styled tree, then maps
  /// elements to accessibility roles, names, and states. The resulting
  /// tree is suitable for regression testing and tooling.
  pub fn accessibility_tree(
    &mut self,
    dom: &DomNode,
    width: u32,
    height: u32,
  ) -> Result<AccessibilityNode> {
    let options = RenderOptions::new().with_viewport(width, height);
    self.accessibility_tree_with_options(dom, options)
  }

  /// Computes the accessibility tree for a parsed document using render options.
  pub fn accessibility_tree_with_options(
    &mut self,
    dom: &DomNode,
    options: RenderOptions,
  ) -> Result<AccessibilityNode> {
    let (width, height) = options
      .viewport
      .unwrap_or((self.default_width, self.default_height));
    if width == 0 || height == 0 {
      return Err(Error::Render(RenderError::InvalidParameters {
        message: "Viewport width and height must be positive".to_string(),
      }));
    }

    let original_dpr = self.device_pixel_ratio;
    if let Some(dpr) = options.device_pixel_ratio {
      self.device_pixel_ratio = dpr;
    }

    let requested_viewport = Size::new(width as f32, height as f32);
    let meta_viewport = if self.apply_meta_viewport {
      crate::html::viewport::extract_viewport(dom)
    } else {
      None
    };
    let resolved_viewport = resolve_viewport(
      requested_viewport,
      self.device_pixel_ratio,
      meta_viewport.as_ref(),
    );

    let target_fragment = self.current_target_fragment();
    let mut dom_with_state = dom.clone();
    let modal_open = modal_dialog_present(&dom_with_state);
    apply_top_layer_state(&mut dom_with_state, modal_open, false);
    let viewport_size = resolved_viewport.layout_viewport;
    let device_size = resolved_viewport.visual_viewport;
    let media_ctx = match options.media_type {
      MediaType::Print => MediaContext::print(viewport_size.width, viewport_size.height),
      MediaType::Screen => MediaContext::screen(viewport_size.width, viewport_size.height),
      other => {
        MediaContext::screen(viewport_size.width, viewport_size.height).with_media_type(other)
      }
    }
    .with_device_size(device_size.width, device_size.height)
    .with_device_pixel_ratio(resolved_viewport.device_pixel_ratio)
    .with_env_overrides();
    let mut media_query_cache = MediaQueryCache::default();
    let stylesheet =
      self.collect_document_stylesheet(&dom_with_state, &media_ctx, &mut media_query_cache);
    // Style and accessibility tree construction are deeply recursive. Run them on a larger-stack
    // helper thread to avoid stack overflows on debug builds and on documents with deep nesting.
    let result = std::thread::scope(|scope| {
      let handle = std::thread::Builder::new()
        .name("fastr-accessibility".to_string())
        .stack_size(8 * 1024 * 1024)
        .spawn_scoped(scope, || {
          let mut local_media_query_cache = MediaQueryCache::default();
          let styled_tree = apply_styles_with_media_target_and_imports_cached(
            &dom_with_state,
            &stylesheet,
            &media_ctx,
            target_fragment.as_deref(),
            None,
            None,
            None,
            None,
            None,
            Some(&mut local_media_query_cache),
          );

          crate::accessibility::build_accessibility_tree(&styled_tree)
        })
        .map_err(|e| {
          Error::Render(RenderError::InvalidParameters {
            message: format!("Failed to spawn accessibility worker: {e}"),
          })
        })?;

      handle.join().map_err(|_| {
        Error::Render(RenderError::InvalidParameters {
          message: "Accessibility worker thread panicked".to_string(),
        })
      })
    });

    if options.device_pixel_ratio.is_some() {
      self.device_pixel_ratio = original_dpr;
    }

    result
  }

  /// Computes the accessibility tree for an HTML string using render options.
  pub fn accessibility_tree_html(
    &mut self,
    html: &str,
    options: RenderOptions,
  ) -> Result<AccessibilityNode> {
    let base_hint = self.base_url.clone().unwrap_or_default();
    let base_url = infer_base_url(html, &base_hint).into_owned();
    if !base_url.is_empty() {
      self.set_base_url(base_url);
    }
    let dom = self.parse_html(html)?;
    self.accessibility_tree_with_options(&dom, options)
  }

  /// Convenience helper to serialize the accessibility tree to JSON.
  pub fn accessibility_tree_json(
    &mut self,
    dom: &DomNode,
    width: u32,
    height: u32,
  ) -> Result<String> {
    let tree = self.accessibility_tree(dom, width, height)?;
    serde_json::to_string_pretty(&tree).map_err(|e| {
      Error::Render(RenderError::InvalidParameters {
        message: format!("Failed to serialize accessibility tree: {e}"),
      })
    })
  }

  /// Serialize the accessibility tree for an HTML string.
  pub fn accessibility_tree_html_json(
    &mut self,
    html: &str,
    options: RenderOptions,
  ) -> Result<serde_json::Value> {
    let tree = self.accessibility_tree_html(html, options)?;
    serde_json::to_value(tree).map_err(|e| {
      Error::Render(RenderError::InvalidParameters {
        message: format!("Failed to serialize accessibility tree: {e}"),
      })
    })
  }

  /// Lays out a document and returns the fragment tree
  ///
  /// This performs box generation and layout without painting, returning
  /// the positioned fragment tree. Useful for inspecting layout results
  /// or performing custom painting.
  ///
  /// # Arguments
  ///
  /// * `dom` - The parsed DOM tree
  /// * `width` - Viewport width in pixels
  /// * `height` - Viewport height in pixels
  ///
  /// # Returns
  ///
  /// Returns a `FragmentTree` containing positioned fragments.
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// use fastrender::api::FastRender;
  ///
  /// let mut renderer = FastRender::new()?;
  /// let dom = renderer.parse_html("<div>Content</div>")?;
  /// let fragment_tree = renderer.layout_document(&dom, 800, 600)?;
  ///
  /// // Inspect layout results
  /// println!("Fragment count: {}", fragment_tree.fragment_count());
  /// println!("Viewport: {:?}", fragment_tree.viewport_size());
  /// ```
  ///
  /// When debugging, use [`inspect`](#method.inspect) to retrieve the styled and
  /// box trees alongside the fragments.
  pub fn layout_document(
    &mut self,
    dom: &DomNode,
    width: u32,
    height: u32,
  ) -> Result<FragmentTree> {
    self.layout_document_with_options(dom, width, height, LayoutDocumentOptions::default())
  }

  /// Lays out a document with explicit layout options.
  pub fn layout_document_with_options(
    &mut self,
    dom: &DomNode,
    width: u32,
    height: u32,
    options: LayoutDocumentOptions,
  ) -> Result<FragmentTree> {
    self.layout_document_for_media_with_options(
      dom,
      width,
      height,
      MediaType::Screen,
      options,
      None,
    )
  }

  pub fn layout_document_for_media(
    &mut self,
    dom: &DomNode,
    width: u32,
    height: u32,
    media_type: MediaType,
  ) -> Result<FragmentTree> {
    self.layout_document_for_media_with_options(
      dom,
      width,
      height,
      media_type,
      LayoutDocumentOptions::default(),
      None,
    )
  }

  #[allow(clippy::cognitive_complexity)]
  pub fn layout_document_for_media_intermediates(
    &mut self,
    dom: &DomNode,
    width: u32,
    height: u32,
    media_type: MediaType,
  ) -> Result<LayoutIntermediates> {
    let trace = TraceHandle::disabled();
    let artifacts = self.layout_document_for_media_with_artifacts(
      dom,
      width,
      height,
      media_type,
      LayoutDocumentOptions::default(),
      None,
      &trace,
    )?;
    let LayoutArtifacts {
      dom,
      styled_tree,
      box_tree,
      fragment_tree,
      ..
    } = artifacts;
    Ok(LayoutIntermediates {
      dom,
      styled_tree,
      box_tree,
      fragment_tree,
    })
  }

  #[allow(clippy::cognitive_complexity)]
  pub fn layout_document_for_media_with_options(
    &mut self,
    dom: &DomNode,
    width: u32,
    height: u32,
    media_type: MediaType,
    options: LayoutDocumentOptions,
    deadline: Option<&RenderDeadline>,
  ) -> Result<FragmentTree> {
    let trace = TraceHandle::disabled();
    let artifacts = self.layout_document_for_media_with_artifacts(
      dom,
      width,
      height,
      media_type,
      options,
      deadline,
      &trace,
    )?;
    Ok(artifacts.fragment_tree)
  }

  #[allow(clippy::cognitive_complexity)]
  fn layout_document_for_media_with_artifacts(
    &mut self,
    dom: &DomNode,
    width: u32,
    height: u32,
    media_type: MediaType,
    options: LayoutDocumentOptions,
    deadline: Option<&RenderDeadline>,
    trace: &TraceHandle,
  ) -> Result<LayoutArtifacts> {
    let _deadline_guard = DeadlineGuard::install(deadline);
    let toggles = runtime::runtime_toggles();
    let timings_enabled = toggles.truthy("FASTR_RENDER_TIMINGS");
    let overall_start = timings_enabled.then(Instant::now);

    let mut dom_with_state = dom.clone();
    let modal_open = modal_dialog_present(&dom_with_state);
    apply_top_layer_state(&mut dom_with_state, modal_open, false);

    let viewport_size = Size::new(width as f32, height as f32);
    let device_size = self.pending_device_size.take().unwrap_or(viewport_size);
    let media_ctx = match media_type {
      MediaType::Print => MediaContext::print(viewport_size.width, viewport_size.height),
      MediaType::Screen => MediaContext::screen(viewport_size.width, viewport_size.height),
      other => {
        MediaContext::screen(viewport_size.width, viewport_size.height).with_media_type(other)
      }
    }
    .with_device_size(device_size.width, device_size.height)
    .with_device_pixel_ratio(self.device_pixel_ratio)
    .with_env_overrides();

    let css_parse_start = timings_enabled.then(Instant::now);
    let mut media_query_cache = MediaQueryCache::default();
    let stylesheet = {
      let _span = trace.span("css_parse", "style");
      self.collect_document_stylesheet(&dom_with_state, &media_ctx, &mut media_query_cache)
    };
    if let Some(start) = css_parse_start {
      eprintln!("timing:css_parse {:?}", start.elapsed());
    }

    // Collect codepoints used in text nodes to avoid fetching unused web font subsets.
    let used_codepoints = dom::collect_text_codepoints(&dom_with_state);

    let mut stage_start = overall_start;

    // Apply styles to create styled tree
    let target_fragment = self.current_target_fragment();
    let style_load_start = timings_enabled.then(Instant::now);
    self.font_context.clear_web_fonts();
    let font_faces =
      stylesheet.collect_font_face_rules_with_cache(&media_ctx, Some(&mut media_query_cache));
    // Best-effort loading; rendering should continue even if a web font fails.
    let _ = self.font_context.load_web_fonts(
      &font_faces,
      self.base_url.as_deref(),
      Some(&used_codepoints),
    );
    let keyframes =
      stylesheet.collect_keyframes_with_cache(&media_ctx, Some(&mut media_query_cache));
    let has_container_queries = stylesheet.has_container_rules();
    if let Some(start) = style_load_start {
      eprintln!("timing:style_prepare {:?}", start.elapsed());
    }
    let style_apply_start = timings_enabled.then(Instant::now);
    let mut styled_tree = {
      let _span = trace.span("cascade", "style");
      apply_styles_with_media_target_and_imports_cached_with_deadline(
        &dom_with_state,
        &stylesheet,
        &media_ctx,
        target_fragment.as_deref(),
        None,
        None,
        None,
        None,
        None,
        Some(&mut media_query_cache),
        deadline,
      )?
    };
    check_deadline(deadline, RenderStage::Cascade)?;
    if let Some(start) = style_apply_start {
      eprintln!("timing:style_apply {:?}", start.elapsed());
    }
    let first_style_fingerprints =
      has_container_queries.then(|| styled_fingerprint_map(&styled_tree));

    let fallback_page_size = viewport_size;
    let page_rules =
      stylesheet.collect_page_rules_with_cache(&media_ctx, Some(&mut media_query_cache));
    let page_name_hint = find_first_page_name(&styled_tree);
    let mut layout_viewport = fallback_page_size;
    let mut first_page_style = None;
    if !page_rules.is_empty() {
      let style = resolve_page_style(
        &page_rules,
        0,
        page_name_hint.as_deref(),
        PageSide::Right,
        fallback_page_size,
        styled_tree.styles.root_font_size,
      );
      layout_viewport = style.content_size;
      first_page_style = Some(style);
    }

    if let Some(start) = stage_start.as_mut() {
      let now = Instant::now();
      eprintln!("timing:cascade {:?}", now - *start);
      *start = now;
    }

    // Generate box tree
    let box_gen_options = self.box_generation_options();
    let box_gen_start = timings_enabled.then(Instant::now);
    let mut box_tree = {
      let _span = trace.span("box_gen", "layout");
      crate::tree::box_generation::generate_box_tree_with_anonymous_fixup_with_options(
        &styled_tree,
        &box_gen_options,
      )
    };
    if let Some(start) = box_gen_start {
      eprintln!("timing:box_gen {:?}", start.elapsed());
    }

    // Resolve intrinsic sizes for replaced elements using the image cache
    let intrinsic_start = timings_enabled.then(Instant::now);
    {
      let _span = trace.span("replaced_intrinsic", "layout");
      self.resolve_replaced_intrinsic_sizes_for_media(&mut box_tree.root, layout_viewport, media_type);
    }
    if let Some(start) = intrinsic_start {
      eprintln!("timing:intrinsic_sizes {:?}", start.elapsed());
    }

    if let Some(start) = stage_start.as_mut() {
      let now = Instant::now();
      eprintln!("timing:box_tree {:?}", now - *start);
      *start = now;
    }

    if timings_enabled {
      fn count_boxes(node: &BoxNode, count: &mut usize) {
        *count += 1;
        for child in &node.children {
          count_boxes(child, count);
        }
      }
      let mut box_count = 0;
      count_boxes(&box_tree.root, &mut box_count);
      eprintln!("timing:box_count {}", box_count);
    }

    if toggles.truthy("FASTR_DUMP_COUNTS") {
      fn count_box_kinds(node: &BoxNode, total: &mut usize, text: &mut usize) {
        *total += 1;
        if matches!(node.box_type, BoxType::Text(_)) {
          *text += 1;
        }
        for child in &node.children {
          count_box_kinds(child, total, text);
        }
      }
      let mut total = 0;
      let mut text = 0;
      count_box_kinds(&box_tree.root, &mut total, &mut text);
      eprintln!("box counts total={} text={}", total, text);
    }

    if toggles.truthy("FASTR_DUMP_TEXT") {
      fn truncate(text: &str, limit: usize) -> String {
        let mut out = String::new();
        for (i, ch) in text.chars().enumerate() {
          if i >= limit {
            out.push('…');
            break;
          }
          out.push(ch);
        }
        out
      }
      fn dump_text_nodes(node: &BoxNode) {
        if let BoxType::Text(t) = &node.box_type {
          eprintln!(
            "box text display={:?} \"{}\"",
            node.style.display,
            truncate(&t.text, 80)
          );
        }
        for child in &node.children {
          dump_text_nodes(child);
        }
      }
      dump_text_nodes(&box_tree.root);

      fn dump_box_tree(node: &BoxNode, depth: usize) {
        let indent = "  ".repeat(depth);
        match &node.box_type {
          BoxType::Text(t) => {
            eprintln!(
              "{}text display={:?} \"{}\"",
              indent,
              node.style.display,
              truncate(&t.text, 80)
            );
          }
          BoxType::Inline(_) => {
            eprintln!(
              "{}inline display={:?} children={}",
              indent,
              node.style.display,
              node.children.len()
            );
          }
          BoxType::Block(_) => {
            eprintln!(
              "{}block display={:?} children={}",
              indent,
              node.style.display,
              node.children.len()
            );
          }
          BoxType::Anonymous(anon) => {
            eprintln!(
              "{}anonymous {:?} display={:?} children={}",
              indent,
              anon.anonymous_type,
              node.style.display,
              node.children.len()
            );
          }
          BoxType::Marker(_) => {
            eprintln!(
              "{}marker display={:?} children={}",
              indent,
              node.style.display,
              node.children.len()
            );
          }
          BoxType::Replaced(_) => {
            eprintln!(
              "{}replaced display={:?} children={}",
              indent,
              node.style.display,
              node.children.len()
            );
          }
        }
        for child in &node.children {
          dump_box_tree(child, depth + 1);
        }
      }
      dump_box_tree(&box_tree.root, 0);
    }

    // Update layout engine config for this viewport.
    //
    // Enable the layout result cache by default. This significantly reduces repeated layout
    // work during flex/grid measurement (Taffy) on real pages. The cache is run-scoped and
    // guarded by conservative eligibility rules.
    let mut config = LayoutConfig::for_viewport(layout_viewport);
    config.enable_cache = !toggles.truthy("FASTR_DISABLE_LAYOUT_CACHE");
    let enable_layout_cache = config.enable_cache;
    self.layout_engine = LayoutEngine::with_font_context(config, self.font_context.clone());
    intrinsic_cache_clear();
    let report_intrinsic = toggles.truthy("FASTR_INTRINSIC_STATS");
    let report_layout_cache = toggles.truthy("FASTR_LAYOUT_CACHE_STATS");
    if report_intrinsic {
      intrinsic_cache_reset_counters();
    }
    let layout_profile = layout_profile_enabled();
    if layout_profile {
      reset_layout_profile();
    }
    let flex_profile = flex_profile_enabled();
    if flex_profile {
      reset_flex_profile();
    }

    let mut layout_start = stage_start;

    // Perform initial layout
    let mut fragment_tree = self
      .layout_engine
      .layout_tree_with_trace(&box_tree, trace)
      .map_err(|e| match e {
        FormattingLayoutError::Timeout { elapsed } => Error::Render(RenderError::Timeout {
          stage: RenderStage::Layout,
          elapsed,
        }),
        other => Error::Render(RenderError::InvalidParameters {
          message: format!("Layout failed: {:?}", other),
        }),
      })?;
    let capture_container_fields = toggles.truthy("FASTR_LOG_CONTAINER_FIELDS");
    let first_styled_snapshot = if has_container_queries && capture_container_fields {
      Some(styled_tree.clone())
    } else {
      None
    };

    // Re-run cascade/layout when container queries are present and we have resolved container sizes.
    let log_container_pass = toggles.truthy("FASTR_LOG_CONTAINER_PASS");
    let log_reuse = toggles.truthy("FASTR_LOG_CONTAINER_REUSE");

    if has_container_queries {
      let first_styled_tree = styled_tree.clone();
      let container_ctx = build_container_query_context(&box_tree, &fragment_tree, &media_ctx);
      if log_container_pass {
        let total = container_ctx.containers.len();
        let (size_cnt, inline_cnt) =
          container_ctx
            .containers
            .values()
            .fold((0usize, 0usize), |(s, i), info| match info.container_type {
              ContainerType::Size => (s + 1, i),
              ContainerType::InlineSize => (s, i + 1),
              _ => (s, i),
            });
        let mut sample: Vec<String> = Vec::new();
        for (idx, (id, info)) in container_ctx.containers.iter().take(5).enumerate() {
          sample.push(format!(
            "#{idx}: styled_id={} type={:?} inline={:.1} block={:.1} names={:?}",
            id, info.container_type, info.inline_size, info.block_size, info.names
          ));
        }
        eprintln!(
          "[container-pass] containers={} size={} inline_size={} samples=[{}]",
          total,
          size_cnt,
          inline_cnt,
          sample.join(" | ")
        );
      }

      if !container_ctx.containers.is_empty() {
        let container_scope = build_container_scope(&first_styled_tree, &container_ctx);
        let mut reuse_map: HashMap<usize, *const StyledNode> = HashMap::new();
        build_styled_lookup(&first_styled_tree, &mut reuse_map);
        if log_container_pass && log_reuse {
          let base_total = count_styled_nodes_api(&first_styled_tree);
          eprintln!(
            "[container-reuse] base_total={} scope_size={}",
            base_total,
            container_scope.len()
          );
        }
        let log_container_ids = toggles.usize_list("FASTR_LOG_CONTAINER_IDS");

        let new_styled_tree = apply_styles_with_media_target_and_imports(
          dom,
          &stylesheet,
          &media_ctx,
          target_fragment.as_deref(),
          None,
          None::<&str>,
          Some(&container_ctx),
          Some(&container_scope),
          Some(&reuse_map),
        );

        if let (true, Some(ids)) = (log_container_pass, log_container_ids.as_ref()) {
          let summaries = styled_summary_map(&new_styled_tree);
          for id in ids {
            let summary = summaries
              .get(id)
              .cloned()
              .unwrap_or_else(|| "<unknown>".to_string());
            if let Some(node) = find_styled_by_id(&new_styled_tree, *id) {
              eprintln!(
                "[container-pass] styled_id={} node={} styles={}",
                id,
                summary,
                styled_style_summary(&node.styles)
              );
            } else {
              eprintln!(
                "[container-pass] styled_id={} node={} not found",
                id, summary
              );
            }
          }
        }

        let fingerprints_match = first_style_fingerprints
          .as_ref()
          .map(|first| {
            let second = styled_fingerprint_map(&new_styled_tree);
            *first == second
          })
          .unwrap_or(false);
        styled_tree = new_styled_tree;

        if fingerprints_match {
          if log_container_pass {
            eprintln!(
              "[container-pass] fingerprints match; reuse layout, refresh fragment styles only"
            );
          }
          let style_map = styled_style_map(&styled_tree);
          let mut box_map = HashMap::new();
          collect_box_nodes(&box_tree.root, &mut box_map);
          refresh_fragment_styles(&mut fragment_tree.root, &box_map, &style_map);
        } else {
          if log_container_pass {
            let mut diff = 0usize;
            let mut changed_entries = Vec::new();
            if let Some(first) = &first_style_fingerprints {
              let second = styled_fingerprint_map(&styled_tree);
              for (k, v) in &second {
                if first.get(k) != Some(v) {
                  diff += 1;
                  changed_entries.push(*k);
                }
              }
            }
            eprintln!(
                            "[container-pass] fingerprints differ; rebuild box tree and layout (changed_entries={})",
                            diff
                        );
            if diff > 0 {
              if let Some(n) = toggles.usize("FASTR_LOG_CONTAINER_DIFF") {
                let summaries = styled_summary_map(&styled_tree);
                let second = styled_fingerprint_map(&styled_tree);
                let mut lines = Vec::new();
                for id in changed_entries.iter().take(n) {
                  let kind = match id & 3 {
                    0 => "main",
                    1 => "before",
                    2 => "after",
                    3 => "marker",
                    _ => "unknown",
                  };
                  let base = id >> 2;
                  let summary = summaries
                    .get(&base)
                    .cloned()
                    .unwrap_or_else(|| "<unknown>".to_string());
                  let fp = second.get(id).copied().unwrap_or(0);
                  lines.push(format!(
                    "styled_id={} kind={} {} fp={}",
                    base, kind, summary, fp
                  ));
                  if capture_container_fields {
                    if let (Some(first_tree), Some(new_node)) = (
                      first_styled_snapshot.as_ref(),
                      find_styled_by_id(&styled_tree, base),
                    ) {
                      if let Some(old_node) = find_styled_by_id(first_tree, base) {
                        let diffs = diff_layout_fields(&old_node.styles, &new_node.styles);
                        if !diffs.is_empty() {
                          lines.push(format!("  ↳ layout fields changed: {}", diffs.join(",")));
                        }
                      }
                    }
                  }
                }
                eprintln!(
                  "[container-pass] changed entries sample ({} of {}): {}",
                  lines.len(),
                  diff,
                  lines.join(" | ")
                );
              }
            }
          }
          box_tree =
            crate::tree::box_generation::generate_box_tree_with_anonymous_fixup(&styled_tree);
          self.resolve_replaced_intrinsic_sizes_for_media(
            &mut box_tree.root,
            layout_viewport,
            media_type,
          );
          crate::tree::box_generation::generate_box_tree_with_anonymous_fixup_with_options(
            &styled_tree,
            &box_gen_options,
          );
          self.resolve_replaced_intrinsic_sizes(&mut box_tree.root, layout_viewport);

          intrinsic_cache_clear();
          if report_intrinsic {
            intrinsic_cache_reset_counters();
          }
          if layout_profile {
            reset_layout_profile();
          }
          if flex_profile {
            reset_flex_profile();
          }

          layout_start = timings_enabled.then(Instant::now);
          fragment_tree = self
            .layout_engine
            .layout_tree_reuse_caches_with_trace(&box_tree, trace)
            .map_err(|e| {
              Error::Render(RenderError::InvalidParameters {
                message: format!("Layout failed: {:?}", e),
              })
            })?;
        }
      }
    }

    if let Some(start) = layout_start {
      let now = Instant::now();
      eprintln!("timing:layout {:?}", now - start);
      if let Some(overall) = overall_start {
        eprintln!("timing:layout_document_total {:?}", now - overall);
      }
      if layout_profile {
        log_layout_profile(now - start);
      }
      if flex_profile {
        log_flex_profile(now - start);
      }
    }

    if !page_rules.is_empty() {
      let viewport = first_page_style
        .as_ref()
        .map(|s| s.total_size)
        .unwrap_or(layout_viewport);
      let pages = paginate_fragment_tree_with_options(
        &box_tree,
        first_page_style
          .as_ref()
          .map(|style| (style, &fragment_tree.root)),
        &page_rules,
        fallback_page_size,
        &self.font_context,
        &box_tree.root.style,
        styled_tree.styles.root_font_size,
        page_name_hint.clone(),
        enable_layout_cache,
        PaginateOptions {
          stacking: options.page_stacking,
        },
      )
      .map_err(|e| {
        Error::Render(RenderError::InvalidParameters {
          message: format!("Layout failed: {:?}", e),
        })
      })?;
      fragment_tree = FragmentTree::from_fragments(pages, viewport);
    }

    if report_intrinsic {
      let (lookups, hits, stores, block_calls, flex_calls, inline_calls) = intrinsic_cache_stats();
      eprintln!(
                "intrinsic_cache lookups={} hits={} stores={} hit_rate={:.2}% block_calls={} flex_calls={} inline_calls={}",
                lookups,
                hits,
                stores,
                if lookups > 0 {
                    (hits as f64 / lookups as f64) * 100.0
                } else {
                    0.0
                },
                block_calls,
                flex_calls,
                inline_calls
            );
    }

    if report_layout_cache {
      let stats = self.layout_engine.stats();
      eprintln!(
        "layout_cache hits={} misses={} total_passes={}",
        stats.cache_hits, stats.cache_misses, stats.total_layouts
      );
    }

    if !keyframes.is_empty() {
      fragment_tree.keyframes = keyframes
        .into_iter()
        .map(|rule| (rule.name.clone(), rule))
        .collect();
    }

    Ok(LayoutArtifacts {
      dom: dom_with_state,
      stylesheet,
      styled_tree,
      box_tree,
      fragment_tree,
    })
  }

  /// Inspect nodes matching a query without painting.
  ///
  /// This runs styling, box generation, and layout, then returns snapshots of
  /// the matched DOM node, computed styles, box nodes, and fragments.
  pub fn inspect(
    &mut self,
    dom: &DomNode,
    width: u32,
    height: u32,
    query: InspectQuery,
  ) -> Result<Vec<InspectionSnapshot>> {
    if width == 0 || height == 0 {
      return Err(Error::Render(RenderError::InvalidParameters {
        message: "Viewport width and height must be positive".to_string(),
      }));
    }

    let trace = TraceHandle::disabled();
    let artifacts = self.layout_document_for_media_with_artifacts(
      dom,
      width,
      height,
      MediaType::Screen,
      LayoutDocumentOptions::default(),
      None,
      &trace,
    )?;

    crate::debug::inspect::inspect(
      &artifacts.styled_tree,
      &artifacts.box_tree.root,
      &artifacts.fragment_tree,
      query,
    )
  }

  /// Paints a fragment tree to a pixmap
  ///
  /// Use this when you have a pre-computed fragment tree and want to
  /// paint it to pixels.
  ///
  /// # Arguments
  ///
  /// * `fragment_tree` - The positioned fragment tree
  /// * `width` - Canvas width in pixels
  /// * `height` - Canvas height in pixels
  ///
  /// # Returns
  ///
  /// Returns a `Pixmap` containing the painted image.
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// use fastrender::api::FastRender;
  ///
  /// let mut renderer = FastRender::new()?;
  /// let dom = renderer.parse_html("<div>Content</div>")?;
  /// let fragment_tree = renderer.layout_document(&dom, 800, 600)?;
  /// let pixmap = renderer.paint(&fragment_tree, 800, 600)?;
  /// ```
  pub fn paint(&self, fragment_tree: &FragmentTree, width: u32, height: u32) -> Result<Pixmap> {
    paint_tree_with_resources_scaled(
      fragment_tree,
      width,
      height,
      self.background_color,
      self.font_context.clone(),
      self.image_cache.clone(),
      self.device_pixel_ratio,
    )
  }

  /// Paints a fragment tree with an additional translation applied to all fragments.
  pub fn paint_with_offset(
    &self,
    fragment_tree: &FragmentTree,
    width: u32,
    height: u32,
    offset: Point,
  ) -> Result<Pixmap> {
    let trace = TraceHandle::disabled();
    self.paint_with_offset_traced(fragment_tree, width, height, offset, &trace)
  }

  fn paint_with_offset_traced(
    &self,
    fragment_tree: &FragmentTree,
    width: u32,
    height: u32,
    offset: Point,
    trace: &TraceHandle,
  ) -> Result<Pixmap> {
    paint_tree_with_resources_scaled_offset_with_trace(
      fragment_tree,
      width,
      height,
      self.background_color,
      self.font_context.clone(),
      self.image_cache.clone(),
      self.device_pixel_ratio,
      offset,
      trace.clone(),
    )
  }

  /// Renders HTML using shared font/image resources.
  ///
  /// This is primarily used internally when nested rendering is needed (e.g., SVG foreignObject).
  pub(crate) fn render_html_with_resources(
    &self,
    html: &str,
    width: u32,
    height: u32,
    background: Rgba,
  ) -> Result<Pixmap> {
    render_html_with_shared_resources(
      html,
      width,
      height,
      background,
      &self.font_context,
      &self.image_cache,
      Arc::clone(&self.fetcher),
      self.base_url.clone(),
      self.device_pixel_ratio,
      self.resource_policy.clone(),
      self.resource_context.clone(),
      self.max_iframe_depth,
    )
  }

  /// Returns a reference to the font context
  ///
  /// Useful for querying available fonts or font metrics.
  pub fn font_context(&self) -> &FontContext {
    &self.font_context
  }

  /// Returns a mutable reference to the font context
  ///
  /// Useful for loading additional fonts.
  pub fn font_context_mut(&mut self) -> &mut FontContext {
    &mut self.font_context
  }

  /// Returns a reference to the layout engine
  pub fn layout_engine(&self) -> &LayoutEngine {
    &self.layout_engine
  }

  /// Sets the base URL used to resolve relative resource references.
  pub fn set_base_url(&mut self, base_url: impl Into<String>) {
    let base = base_url.into();
    self.base_url = Some(base.clone());
    self.image_cache.set_base_url(base);
  }

  /// Replaces the font context used for layout and painting.
  ///
  /// This is primarily used by test harnesses to install a deterministic set
  /// of fonts in CI environments.
  pub fn set_font_context(&mut self, font_context: FontContext) {
    self.font_context = font_context.clone();
    let mut config = self.layout_engine.config().clone();
    config.initial_containing_block.width = self.default_width as f32;
    config.initial_containing_block.height = self.default_height as f32;
    self.layout_engine = LayoutEngine::with_font_context(config, font_context);
  }

  /// Overrides the device pixel ratio for subsequent renders.
  ///
  /// This is primarily used by test harnesses that need to vary DPR per test case.
  pub fn set_device_pixel_ratio(&mut self, dpr: f32) {
    if dpr.is_finite() && dpr > 0.0 {
      self.device_pixel_ratio = dpr;
    }
  }

  /// Clears any configured base URL, leaving relative resources unresolved.
  pub fn clear_base_url(&mut self) {
    self.image_cache.clear_base_url();
    self.base_url = None;
  }

  /// Attach or clear the diagnostics sink for downstream fetchers.
  fn set_diagnostics_sink(&mut self, sink: Option<Arc<Mutex<RenderDiagnostics>>>) {
    self.diagnostics = sink.clone();
    self.image_cache.set_diagnostics_sink(sink);
  }

  fn record_fetch_error(&self, kind: ResourceKind, url: &str, message: impl Into<String>) {
    if let Some(diag) = &self.diagnostics {
      if let Ok(mut guard) = diag.lock() {
        guard.record_error(
          kind,
          url,
          &Error::Resource(ResourceError::new(url.to_string(), message.into())),
        );
      }
    }
  }

  fn build_resource_context(
    &self,
    document_url: Option<&str>,
    diagnostics: Option<SharedRenderDiagnostics>,
  ) -> ResourceContext {
    let origin = document_url.and_then(origin_from_url);
    ResourceContext {
      policy: self.resource_policy.for_origin(origin),
      diagnostics,
    }
  }

  fn push_resource_context(
    &mut self,
    context: Option<ResourceContext>,
  ) -> (
    Option<ResourceContext>,
    Option<ResourceContext>,
    Option<ResourceContext>,
  ) {
    let prev_self = self.resource_context.clone();
    let prev_image = self.image_cache.resource_context();
    let prev_font = self.font_context.resource_context();

    self.resource_context = context.clone();
    self.image_cache.set_resource_context(context.clone());
    self.font_context.set_resource_context(context);

    (prev_self, prev_image, prev_font)
  }

  fn pop_resource_context(
    &mut self,
    prev_self: Option<ResourceContext>,
    prev_image: Option<ResourceContext>,
    prev_font: Option<ResourceContext>,
  ) {
    self.resource_context = prev_self;
    self.image_cache.set_resource_context(prev_image);
    self.font_context.set_resource_context(prev_font);
  }
  /// Gets the default background color
  pub fn background_color(&self) -> Rgba {
    self.background_color
  }

  /// Sets the default background color
  pub fn set_background_color(&mut self, color: Rgba) {
    self.background_color = color;
  }

  fn media_context_for_media(
    &self,
    media_type: MediaType,
    width: f32,
    height: f32,
  ) -> MediaContext {
    let base = match media_type {
      MediaType::Print => MediaContext::print(width, height),
      _ => MediaContext::screen(width, height),
    };

    base
      .with_device_pixel_ratio(self.device_pixel_ratio)
      .with_env_overrides()
  }

  /// Create a simple placeholder pixmap indicating a render error.
  fn render_error_overlay(&self, width: u32, height: u32) -> Result<Pixmap> {
    let width = width.max(1);
    let height = height.max(1);
    let mut pixmap =
      Pixmap::new(width, height).ok_or(Error::Render(RenderError::CanvasCreationFailed {
        width,
        height,
      }))?;
    // Light red background
    pixmap.fill(tiny_skia::Color::from_rgba8(255, 235, 238, 255));

    // Draw an "X" mark for visibility.
    let mut paint = tiny_skia::Paint::default();
    paint.set_color(tiny_skia::Color::from_rgba8(198, 40, 40, 255));
    paint.anti_alias = true;
    let mut stroke = tiny_skia::Stroke::default();
    stroke.width = 4.0;
    let inset = 12.0f32;
    let mut builder = tiny_skia::PathBuilder::new();
    builder.move_to(inset, inset);
    builder.line_to(width as f32 - inset, height as f32 - inset);
    builder.move_to(width as f32 - inset, inset);
    builder.line_to(inset, height as f32 - inset);
    if let Some(path) = builder.finish() {
      let _ = pixmap.stroke_path(
        &path,
        &paint,
        &stroke,
        tiny_skia::Transform::identity(),
        None,
      );
    }

    Ok(pixmap)
  }

  /// Extract the fragment identifier (without '#') from the configured base URL, if any.
  fn current_target_fragment(&self) -> Option<String> {
    self.base_url.as_ref().and_then(|url| extract_fragment(url))
  }

  /// Fetch linked stylesheets and inject them into the document so they participate in cascade.
  fn inline_stylesheets(
    &self,
    html: &str,
    base_url: &str,
    media_type: MediaType,
    css_limit: Option<usize>,
    diagnostics: &mut RenderDiagnostics,
    deadline: Option<&RenderDeadline>,
    stats: Option<&mut RenderStatsRecorder>,
  ) -> std::result::Result<String, RenderError> {
    Self::inline_stylesheets_for_html_with_context(
      self.fetcher.as_ref(),
      html,
      base_url,
      media_type,
      css_limit,
      self.resource_context.as_ref(),
      diagnostics,
      deadline,
      stats,
    )
  }

  /// Fetch linked stylesheets using the renderer's fetcher and inject them into the HTML.
  pub fn inline_stylesheets_for_document(
    &self,
    html: &str,
    base_url: &str,
    media_type: MediaType,
    css_limit: Option<usize>,
    diagnostics: &mut RenderDiagnostics,
    deadline: Option<&RenderDeadline>,
  ) -> std::result::Result<String, RenderError> {
    self.inline_stylesheets(
      html,
      base_url,
      media_type,
      css_limit,
      diagnostics,
      deadline,
      None,
    )
  }

  /// Fetch linked stylesheets using the renderer's fetcher with an explicit resource context.
  pub fn inline_stylesheets_for_document_with_context(
    &self,
    html: &str,
    base_url: &str,
    media_type: MediaType,
    css_limit: Option<usize>,
    resource_context: Option<&ResourceContext>,
    diagnostics: &mut RenderDiagnostics,
    deadline: Option<&RenderDeadline>,
  ) -> std::result::Result<String, RenderError> {
    Self::inline_stylesheets_for_html_with_context(
      self.fetcher.as_ref(),
      html,
      base_url,
      media_type,
      css_limit,
      resource_context,
      diagnostics,
      deadline,
      None,
    )
  }

  /// Fetch linked stylesheets using an arbitrary fetcher and inject them into the HTML.
  pub fn inline_stylesheets_for_html(
    fetcher: &dyn ResourceFetcher,
    html: &str,
    base_url: &str,
    media_type: MediaType,
    css_limit: Option<usize>,
    diagnostics: &mut RenderDiagnostics,
    deadline: Option<&RenderDeadline>,
  ) -> std::result::Result<String, RenderError> {
    Self::inline_stylesheets_for_html_with_context(
      fetcher,
      html,
      base_url,
      media_type,
      css_limit,
      None,
      diagnostics,
      deadline,
      None,
    )
  }

  /// Fetch linked stylesheets using an arbitrary fetcher with an explicit resource context.
  pub fn inline_stylesheets_for_html_with_context(
    fetcher: &dyn ResourceFetcher,
    html: &str,
    base_url: &str,
    media_type: MediaType,
    css_limit: Option<usize>,
    resource_context: Option<&ResourceContext>,
    diagnostics: &mut RenderDiagnostics,
    deadline: Option<&RenderDeadline>,
    mut stats: Option<&mut RenderStatsRecorder>,
  ) -> std::result::Result<String, RenderError> {
    let inlining_start = stats.as_deref().and_then(|rec| rec.timer());
    let mut css_links = extract_css_links(html, base_url, media_type);
    if let Some(limit) = css_limit {
      if css_links.len() > limit {
        css_links.truncate(limit);
      }
    }
    let mut seen: HashSet<String> = css_links.iter().cloned().collect();
    for extra in extract_embedded_css_urls(html, base_url) {
      if seen.insert(extra.clone()) {
        css_links.push(extra);
      }
    }

    let mut combined_css = String::new();
    let mut seen_imports = HashSet::new();

    for css_url in css_links {
      seen_imports.insert(css_url.clone());
      if let Some(rec) = stats.as_deref_mut() {
        rec.record_fetch(ResourceKind::Stylesheet);
      }
      if let Some(ctx) = resource_context {
        if let Err(err) = ctx.policy.allows(&css_url) {
          diagnostics.record_message(ResourceKind::Stylesheet, &css_url, err.reason);
          continue;
        }
      }
      match fetcher.fetch(&css_url) {
        Ok(res) => {
          let css_text = decode_css_bytes(&res.bytes, res.content_type.as_deref());
          let rewritten = absolutize_css_urls(&css_text, &css_url);
          let mut import_diags: Vec<(String, String)> = Vec::new();
          let inlined = {
            let mut import_fetch = |u: &str| -> Result<String> {
              if let Some(rec) = stats.as_deref_mut() {
                rec.record_fetch(ResourceKind::Stylesheet);
              }
              if let Some(ctx) = resource_context {
                if let Err(err) = ctx.policy.allows(u) {
                  diagnostics.record_message(ResourceKind::Stylesheet, u, &err.reason);
                  return Err(Error::Resource(ResourceError::new(u.to_string(), err.reason)));
                }
              }
              match fetcher.fetch(u) {
                Ok(res) => Ok(decode_css_bytes(&res.bytes, res.content_type.as_deref())),
                Err(err) => {
                  diagnostics.record_error(ResourceKind::Stylesheet, u, &err);
                  Err(err)
                }
              }
            };

            let mut import_diag = |url: &str, reason: &str| {
              import_diags.push((url.to_string(), reason.to_string()));
            };

            inline_imports_with_diagnostics(
              &rewritten,
              &css_url,
              &mut import_fetch,
              &mut seen_imports,
              &mut import_diag,
              deadline,
            )?
          };
          for (url, reason) in import_diags.drain(..) {
            diagnostics.record_message(ResourceKind::Stylesheet, &url, &reason);
          }
          combined_css.push_str(&inlined);
          combined_css.push('\n');
        }
        Err(err) => diagnostics.record_error(ResourceKind::Stylesheet, &css_url, &err),
      }
    }

    let output = if combined_css.is_empty() {
      html.to_string()
    } else {
      inject_css_into_html(html, &combined_css)
    };
    if let Some(rec) = stats.as_deref_mut() {
      RenderStatsRecorder::record_ms(&mut rec.stats.timings.css_inlining_ms, inlining_start);
    }
    Ok(output)
  }

  /// Populate intrinsic sizes for replaced elements (e.g., images) using the image cache.
  fn resolve_replaced_intrinsic_sizes(&self, node: &mut BoxNode, viewport: Size) {
    self.resolve_replaced_intrinsic_sizes_for_media(node, viewport, MediaType::Screen);
  }

  fn replaced_intrinsic_profile_enabled() -> bool {
    runtime::runtime_toggles().truthy("FASTR_REPLACED_INTRINSIC_PROFILE")
  }

  fn resolve_replaced_intrinsic_sizes_for_media(
    &self,
    node: &mut BoxNode,
    viewport: Size,
    media_type: MediaType,
  ) {
    let profile_enabled = Self::replaced_intrinsic_profile_enabled();
    if profile_enabled {
      REPLACED_INTRINSIC_PROFILE.with(|state| {
        let mut state = state.borrow_mut();
        if state.depth == 0 {
          *state = ReplacedIntrinsicProfileState::default();
          state.start = Some(Instant::now());
        }
        state.depth += 1;
      });
    }

    if let BoxType::Marker(marker_box) = &mut node.box_type {
      if let MarkerContent::Image(replaced) = &mut marker_box.content {
        self.resolve_intrinsic_for_replaced_for_media(
          replaced,
          node.style.as_ref(),
          None,
          viewport,
          media_type,
        );
      }
    }

    if let BoxType::Replaced(replaced_box) = &mut node.box_type {
      let alt = match &replaced_box.replaced_type {
        ReplacedType::Image { alt, .. } => alt.clone(),
        _ => None,
      };
      self.resolve_intrinsic_for_replaced_for_media(
        replaced_box,
        node.style.as_ref(),
        alt.as_deref(),
        viewport,
        media_type,
      );
    }

    for child in &mut node.children {
      self.resolve_replaced_intrinsic_sizes_for_media(child, viewport, media_type);
    }

    if profile_enabled {
      REPLACED_INTRINSIC_PROFILE.with(|state| {
        let mut state = state.borrow_mut();
        state.depth = state.depth.saturating_sub(1);
        if state.depth == 0 {
          let total_ms = state
            .start
            .take()
            .map(|s| s.elapsed().as_secs_f64() * 1000.0)
            .unwrap_or(0.0);
          eprintln!(
            "replaced_intrinsic_profile total_ms={total_ms:.2} replaced={} images={} form_controls={} videos={} svgs={} embeds={} selection_calls={} selection_ms={:.2} probe_calls={} probe_ms={:.2} probe_ok={} probe_err={} load_calls={} load_ms={:.2} render_svg_calls={} render_svg_ms={:.2}",
            state.replaced_nodes,
            state.image_nodes,
            state.form_controls,
            state.videos,
            state.svgs,
            state.embeds,
            state.selection_calls,
            state.selection_ms,
            state.probe_calls,
            state.probe_ms,
            state.probe_ok,
            state.probe_err,
            state.load_calls,
            state.load_ms,
            state.render_svg_calls,
            state.render_svg_ms
          );
        }
      });
    }
  }

  #[allow(clippy::cognitive_complexity)]
  fn resolve_intrinsic_for_replaced(
    &self,
    replaced_box: &mut ReplacedBox,
    style: &ComputedStyle,
    alt: Option<&str>,
    viewport: Size,
  ) {
    self.resolve_intrinsic_for_replaced_for_media(
      replaced_box,
      style,
      alt,
      viewport,
      MediaType::Screen,
    );
  }

  #[allow(clippy::cognitive_complexity)]
  fn resolve_intrinsic_for_replaced_for_media(
    &self,
    replaced_box: &mut ReplacedBox,
    style: &ComputedStyle,
    alt: Option<&str>,
    viewport: Size,
    media_type: MediaType,
  ) {
    if let ReplacedType::Math(math) = &mut replaced_box.replaced_type {
      if math.layout.is_none() {
        let layout = crate::math::layout_mathml(&math.root, style, &self.font_context);
        math.layout = Some(Arc::new(layout));
      }
      if replaced_box.intrinsic_size.is_none() {
        if let Some(layout) = &math.layout {
          replaced_box.intrinsic_size = Some(layout.size());
          if layout.height > 0.0 {
            replaced_box.aspect_ratio = Some(layout.width / layout.height);
          }
        }
      }
      return;
    }

    let profile_enabled = Self::replaced_intrinsic_profile_enabled();
    if profile_enabled {
      REPLACED_INTRINSIC_PROFILE.with(|state| {
        state.borrow_mut().replaced_nodes += 1;
      });
    }

    let mut explicit_no_ratio = false;
    match &replaced_box.replaced_type {
      ReplacedType::FormControl(control) => {
        if profile_enabled {
          REPLACED_INTRINSIC_PROFILE.with(|state| {
            state.borrow_mut().form_controls += 1;
          });
        }
        explicit_no_ratio = true;
        if replaced_box.intrinsic_size.is_none() {
          let metrics_scaled = self.resolve_scaled_metrics(style);
          let char_width = metrics_scaled
            .as_ref()
            .and_then(|m| m.x_height)
            .unwrap_or(style.font_size * 0.6)
            * 0.6;
          let line_height = compute_line_height_with_metrics_viewport(
            style,
            metrics_scaled.as_ref(),
            Some(viewport),
          );

          let size = match &control.control {
            FormControlKind::Text { size_attr, .. } => {
              let cols = size_attr.unwrap_or(20) as f32;
              Size::new(char_width * cols.max(1.0), line_height)
            }
            FormControlKind::TextArea { rows, cols, .. } => {
              let row_count = rows.unwrap_or(2) as f32;
              let col_count = cols.unwrap_or(20) as f32;
              Size::new(
                char_width * col_count.max(1.0),
                line_height * row_count.max(1.0),
              )
            }
            FormControlKind::Button { label } => {
              let text_len = label.chars().count().max(1) as f32;
              Size::new(char_width * text_len + char_width * 2.0, line_height)
            }
            FormControlKind::Select { label, .. } => {
              let text_len = label.chars().count().max(4) as f32;
              Size::new(char_width * text_len + 20.0, line_height)
            }
            FormControlKind::Checkbox { .. } => {
              let edge = (style.font_size * 1.1).clamp(12.0, 20.0);
              Size::new(edge, edge)
            }
            FormControlKind::Range { .. } => Size::new(char_width * 12.0, line_height.max(12.0)),
            FormControlKind::Unknown { .. } => Size::new(char_width * 10.0, line_height),
          };

          replaced_box.intrinsic_size = Some(size);
        }
      }
      ReplacedType::Image {
        src,
        alt: stored_alt,
        srcset,
        picture_sources,
        ..
      } => {
        if profile_enabled {
          REPLACED_INTRINSIC_PROFILE.with(|state| {
            state.borrow_mut().image_nodes += 1;
          });
        }
        // If intrinsic dimensions are already known (e.g., width/height attributes), avoid
        // fetching the image just to rederive them. This keeps box tree construction fast
        // on image-heavy pages while still honoring provided intrinsic sizes/aspect ratios.
        if replaced_box.intrinsic_size.is_some() {
          return;
        }
        // If both width and height are specified (non-auto), intrinsic data is unused per
        // replaced element sizing. Avoid network fetches in that case to speed up heavy pages.
        if style.width.is_some() && style.height.is_some() {
          return;
        }

        let mut have_resource_dimensions = false;

        let has_image_source = !src.is_empty() || !srcset.is_empty() || !picture_sources.is_empty();

        let selected = if has_image_source {
          let select_start = profile_enabled.then(Instant::now);
          let media_ctx = self.media_context_for_media(media_type, viewport.width, viewport.height);
          let selected = replaced_box
            .replaced_type
            .selected_image_source_for_context(crate::tree::box_tree::ImageSelectionContext {
              scale: self.device_pixel_ratio,
              slot_width: None,
              viewport: Some(viewport),
              media_context: Some(&media_ctx),
              font_size: Some(style.font_size),
              base_url: self.base_url.as_deref(),
            });
          if let Some(start) = select_start {
            let ms = start.elapsed().as_secs_f64() * 1000.0;
            REPLACED_INTRINSIC_PROFILE.with(|state| {
              let mut state = state.borrow_mut();
              state.selection_calls += 1;
              state.selection_ms += ms;
            });
          }
          Some(selected)
        } else {
          None
        };

        if let Some(selected) = selected {
          if !selected.url.is_empty() {
            let probe_start = profile_enabled.then(Instant::now);
            let probe_result = self.image_cache.probe(selected.url);
            if let Some(start) = probe_start {
              let ms = start.elapsed().as_secs_f64() * 1000.0;
              REPLACED_INTRINSIC_PROFILE.with(|state| {
                let mut state = state.borrow_mut();
                state.probe_calls += 1;
                state.probe_ms += ms;
                if probe_result.is_ok() {
                  state.probe_ok += 1;
                } else {
                  state.probe_err += 1;
                }
              });
            }
            if let Ok(image) = probe_result {
              let orientation = style.image_orientation.resolve(image.orientation, false);
              explicit_no_ratio = image.aspect_ratio_none;
              if let Some((w, h)) = image.css_dimensions(
                orientation,
                &style.image_resolution,
                self.device_pixel_ratio,
                selected.resolution,
              ) {
                replaced_box.intrinsic_size = Some(Size::new(w, h));
                if !explicit_no_ratio {
                  replaced_box.aspect_ratio = replaced_box
                    .aspect_ratio
                    .or_else(|| image.intrinsic_ratio(orientation))
                    .or_else(|| if h > 0.0 { Some(w / h) } else { None });
                }
                have_resource_dimensions = true;
              }
            }
          }
        }

        if !have_resource_dimensions {
          let candidate_alt = alt
            .filter(|s| !s.is_empty())
            .or_else(|| stored_alt.as_deref().filter(|s| !s.is_empty()));
          if let Some(size) = candidate_alt.and_then(|text| self.alt_intrinsic_size(style, text)) {
            replaced_box.intrinsic_size.get_or_insert(size);
            if size.height > 0.0 && replaced_box.aspect_ratio.is_none() {
              replaced_box.aspect_ratio = Some(size.width / size.height);
            }
          }
        }
      }
      ReplacedType::Video { poster, .. } => {
        if profile_enabled {
          REPLACED_INTRINSIC_PROFILE.with(|state| {
            state.borrow_mut().videos += 1;
          });
        }
        let needs_intrinsic = replaced_box.intrinsic_size.is_none();
        let needs_ratio = replaced_box.aspect_ratio.is_none();
        if needs_intrinsic || needs_ratio {
          if let Some(candidate) = poster.as_deref().filter(|s| !s.is_empty()) {
            let candidate_trimmed = candidate.trim_start();
            let inline_svg =
              candidate_trimmed.starts_with("<svg") || candidate_trimmed.starts_with("<?xml");
            let meta = if inline_svg {
              self
                .image_cache
                .probe_svg_content(candidate, "video-poster")
            } else {
              self
                .image_cache
                .probe(candidate)
                .map(|meta| (*meta).clone())
            };
            if let Ok(meta) = meta {
              let orientation = style.image_orientation.resolve(meta.orientation, false);
              explicit_no_ratio = meta.aspect_ratio_none;
              if let Some((w, h)) = meta.css_dimensions(
                orientation,
                &style.image_resolution,
                self.device_pixel_ratio,
                None,
              ) {
                if needs_intrinsic {
                  replaced_box.intrinsic_size = Some(Size::new(w, h));
                }
                if needs_ratio && !explicit_no_ratio {
                  replaced_box.aspect_ratio = meta.intrinsic_ratio(orientation).or_else(|| {
                    if h > 0.0 {
                      Some(w / h)
                    } else {
                      None
                    }
                  });
                }
              }
            }
          }
        }
      }
      ReplacedType::Svg { content } => {
        if profile_enabled {
          REPLACED_INTRINSIC_PROFILE.with(|state| {
            state.borrow_mut().svgs += 1;
          });
        }
        let needs_intrinsic = replaced_box.intrinsic_size.is_none();
        let needs_ratio = replaced_box.aspect_ratio.is_none();

        if needs_intrinsic || needs_ratio {
          // Inline SVG content can be rendered directly; otherwise try to load via URL/data URI.
          let source = content.svg.trim_start();
          let meta = if source.starts_with('<') {
            self.image_cache.probe_svg_content(&content.svg, "svg")
          } else if !content.svg.is_empty() {
            self
              .image_cache
              .probe(content.svg.as_str())
              .map(|meta| (*meta).clone())
          } else {
            Err(crate::error::Error::Image(
              crate::error::ImageError::LoadFailed {
                url: "svg".to_string(),
                reason: "empty content".to_string(),
              },
            ))
          };

          if let Ok(meta) = meta {
            let orientation = style.image_orientation.resolve(meta.orientation, false);
            explicit_no_ratio = meta.aspect_ratio_none;
            if let Some((w, h)) = meta.css_dimensions(
              orientation,
              &style.image_resolution,
              self.device_pixel_ratio,
              None,
            ) {
              if needs_intrinsic {
                replaced_box.intrinsic_size = Some(Size::new(w, h));
              }
              if needs_ratio && !explicit_no_ratio {
                replaced_box.aspect_ratio = meta.intrinsic_ratio(orientation).or_else(|| {
                  if h > 0.0 {
                    Some(w / h)
                  } else {
                    None
                  }
                });
              }
            }
          }
        }
      }
      ReplacedType::Embed { src }
      | ReplacedType::Object { data: src }
      | ReplacedType::Iframe { src, .. } => {
        if profile_enabled {
          REPLACED_INTRINSIC_PROFILE.with(|state| {
            state.borrow_mut().embeds += 1;
          });
        }
        let needs_intrinsic = replaced_box.intrinsic_size.is_none();
        let needs_ratio = replaced_box.aspect_ratio.is_none();
        if (needs_intrinsic || needs_ratio) && !src.is_empty() {
          let src_trimmed = src.trim_start();
          let inline_svg = src_trimmed.starts_with("<svg") || src_trimmed.starts_with("<?xml");
          let meta = if inline_svg {
            self.image_cache.probe_svg_content(src, "embed")
          } else {
            self.image_cache.probe(src).map(|meta| (*meta).clone())
          };
          if let Ok(meta) = meta {
            let orientation = style.image_orientation.resolve(meta.orientation, false);
            explicit_no_ratio = meta.aspect_ratio_none;
            if let Some((w, h)) = meta.css_dimensions(
              orientation,
              &style.image_resolution,
              self.device_pixel_ratio,
              None,
            ) {
              if needs_intrinsic {
                replaced_box.intrinsic_size = Some(Size::new(w, h));
              }
              if needs_ratio && !explicit_no_ratio {
                replaced_box.aspect_ratio = meta.intrinsic_ratio(orientation).or_else(|| {
                  if h > 0.0 {
                    Some(w / h)
                  } else {
                    None
                  }
                });
              }
            }
          }
        }
      }
      _ => {}
    }

    // If only intrinsic size is present, ensure aspect ratio is recorded
    if replaced_box.aspect_ratio.is_none() && !explicit_no_ratio {
      if let Some(size) = replaced_box.intrinsic_size {
        if size.height > 0.0 {
          replaced_box.aspect_ratio = Some(size.width / size.height);
        }
      }
    }
  }

  /// Computes an intrinsic size for alt text when replaced content cannot be loaded.
  fn alt_intrinsic_size(&self, style: &ComputedStyle, alt: &str) -> Option<Size> {
    let text = alt.trim();
    if text.is_empty() {
      return None;
    }

    let mut runs = ShapingPipeline::new()
      .shape(text, style, &self.font_context)
      .ok()?;
    if runs.is_empty() {
      return None;
    }

    TextItem::apply_spacing_to_runs(&mut runs, text, style.letter_spacing, style.word_spacing);

    let metrics_scaled = self.resolve_scaled_metrics(style);
    let viewport = Size::new(self.default_width as f32, self.default_height as f32);
    let line_height =
      compute_line_height_with_metrics_viewport(style, metrics_scaled.as_ref(), Some(viewport));
    let metrics = TextItem::metrics_from_runs(&runs, line_height, style.font_size);
    let width: f32 = runs.iter().map(|r| r.advance).sum();
    let height = metrics.height;

    if width.is_finite() && height.is_finite() && height > 0.0 {
      Some(Size::new(width, height))
    } else {
      None
    }
  }

  fn apply_sticky_offsets(
    &self,
    fragment: &mut FragmentNode,
    parent_rect: Rect,
    scroll: Point,
    viewport: Size,
  ) {
    apply_sticky_offsets_with_context(&self.font_context, fragment, parent_rect, scroll, viewport);
  }

  // ===  // Convenience methods for encoding to image formats
  // ===
  /// Renders HTML to PNG bytes
  ///
  /// This is a convenience method that renders and encodes in one step.
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// use fastrender::FastRender;
  /// use std::fs;
  ///
  /// let mut renderer = FastRender::new()?;
  /// let png_bytes = renderer.render_to_png("<h1>Hello!</h1>", 800, 600)?;
  /// fs::write("output.png", &png_bytes)?;
  /// ```
  pub fn render_to_png(&mut self, html: &str, width: u32, height: u32) -> Result<Vec<u8>> {
    let pixmap = self.render_html(html, width, height)?;
    encode_image(&pixmap, OutputFormat::Png)
  }

  /// Renders HTML to PNG bytes with scroll offsets
  pub fn render_to_png_with_scroll(
    &mut self,
    html: &str,
    width: u32,
    height: u32,
    scroll_x: f32,
    scroll_y: f32,
  ) -> Result<Vec<u8>> {
    let pixmap = self.render_html_with_scroll(html, width, height, scroll_x, scroll_y)?;
    encode_image(&pixmap, OutputFormat::Png)
  }

  /// Renders HTML to JPEG bytes with specified quality (0-100)
  ///
  /// # Arguments
  ///
  /// * `html` - HTML source code
  /// * `width` - Viewport width in pixels
  /// * `height` - Viewport height in pixels
  /// * `quality` - JPEG quality (0-100, where 100 is highest quality)
  pub fn render_to_jpeg(
    &mut self,
    html: &str,
    width: u32,
    height: u32,
    quality: u8,
  ) -> Result<Vec<u8>> {
    let pixmap = self.render_html(html, width, height)?;
    encode_image(&pixmap, OutputFormat::Jpeg(quality))
  }

  /// Renders HTML to WebP bytes with specified quality (0-100)
  ///
  /// # Arguments
  ///
  /// * `html` - HTML source code
  /// * `width` - Viewport width in pixels
  /// * `height` - Viewport height in pixels
  /// * `quality` - WebP quality (0-100, where 100 is highest quality)
  pub fn render_to_webp(
    &mut self,
    html: &str,
    width: u32,
    height: u32,
    quality: u8,
  ) -> Result<Vec<u8>> {
    let pixmap = self.render_html(html, width, height)?;
    encode_image(&pixmap, OutputFormat::WebP(quality))
  }

  /// Renders HTML using default viewport size
  ///
  /// Uses the viewport size configured in `FastRenderConfig` (default: 800x600).
  pub fn render(&mut self, html: &str) -> Result<Vec<u8>> {
    self.render_to_png(html, self.default_width, self.default_height)
  }
}

/// CSS import loader that uses a ResourceFetcher for byte fetching
struct CssImportFetcher {
  base_url: Option<String>,
  fetcher: Arc<dyn ResourceFetcher>,
  resource_context: Option<ResourceContext>,
}

impl CssImportFetcher {
  fn new(
    base_url: Option<String>,
    fetcher: Arc<dyn ResourceFetcher>,
    resource_context: Option<ResourceContext>,
  ) -> Self {
    Self {
      base_url,
      fetcher,
      resource_context,
    }
  }

  fn resolve_url(&self, href: &str) -> Option<String> {
    resolve_href_with_base(self.base_url.as_deref(), href)
  }
}

impl CssImportLoader for CssImportFetcher {
  fn load(&self, url: &str) -> Result<String> {
    // Resolve the URL first
    let resolved = self.resolve_url(url).ok_or_else(|| {
      Error::Io(io::Error::new(
        io::ErrorKind::InvalidInput,
        format!("Cannot resolve @import URL '{}'", url),
      ))
    })?;

    if let Some(ctx) = &self.resource_context {
      if let Err(err) = ctx.check_allowed(ResourceKind::Stylesheet, &resolved) {
        return Err(Error::Io(io::Error::new(
          io::ErrorKind::PermissionDenied,
          err.reason,
        )));
      }
    }

    // Use the fetcher to get the bytes
    let resource = match self.fetcher.fetch(&resolved) {
      Ok(res) => res,
      Err(err) => {
        if let Some(ctx) = &self.resource_context {
          if let Some(diag) = &ctx.diagnostics {
            diag.record_error(ResourceKind::Stylesheet, resolved.as_str(), &err);
          }
        }
        return Err(err);
      }
    };

    // Decode CSS bytes with charset handling
    let decoded = decode_css_bytes(&resource.bytes, resource.content_type.as_deref());
    Ok(absolutize_css_urls(&decoded, &resolved))
  }
}

fn hash_enum_discriminant<T>(value: &T, hasher: &mut DefaultHasher) {
  mem::discriminant(value).hash(hasher);
}

fn hash_length(len: &Length, hasher: &mut DefaultHasher) {
  hash_enum_discriminant(&len.unit, hasher);
  len.value.to_bits().hash(hasher);
  // Treat calc lengths as distinct from raw by hashing a marker.
  if len.calc.is_some() {
    1u8.hash(hasher);
  } else {
    0u8.hash(hasher);
  }
}

fn hash_option_length(len: &Option<Length>, hasher: &mut DefaultHasher) {
  match len {
    Some(l) => {
      1u8.hash(hasher);
      hash_length(l, hasher);
    }
    None => 0u8.hash(hasher),
  }
}

fn hash_flex_basis(basis: &crate::style::types::FlexBasis, hasher: &mut DefaultHasher) {
  match basis {
    crate::style::types::FlexBasis::Auto => 0u8.hash(hasher),
    crate::style::types::FlexBasis::Length(l) => {
      1u8.hash(hasher);
      hash_length(l, hasher);
    }
  }
}

fn hash_f32(value: f32, hasher: &mut DefaultHasher) {
  value.to_bits().hash(hasher);
}

fn hash_overflow(overflow: &crate::style::types::Overflow, hasher: &mut DefaultHasher) {
  hash_enum_discriminant(overflow, hasher);
}

fn hash_scrollbar_color(color: &crate::style::types::ScrollbarColor, hasher: &mut DefaultHasher) {
  use crate::style::types::ScrollbarColor::Auto;
  use crate::style::types::ScrollbarColor::Colors;
  use crate::style::types::ScrollbarColor::Dark;
  use crate::style::types::ScrollbarColor::Light;
  match color {
    Auto => 0u8.hash(hasher),
    Dark => 1u8.hash(hasher),
    Light => 2u8.hash(hasher),
    Colors { thumb, track } => {
      3u8.hash(hasher);
      thumb.r.hash(hasher);
      thumb.g.hash(hasher);
      thumb.b.hash(hasher);
      thumb.a.to_bits().hash(hasher);
      track.r.hash(hasher);
      track.g.hash(hasher);
      track.b.hash(hasher);
      track.a.to_bits().hash(hasher);
    }
  }
}

fn hash_border_style(style: &crate::style::types::BorderStyle, hasher: &mut DefaultHasher) {
  hash_enum_discriminant(style, hasher);
}

fn hash_position_component(
  component: &crate::style::types::PositionComponent,
  hasher: &mut DefaultHasher,
) {
  use crate::style::types::PositionComponent::Keyword;
  use crate::style::types::PositionComponent::Length;
  use crate::style::types::PositionComponent::Percentage;
  match component {
    Keyword(k) => {
      0u8.hash(hasher);
      hash_enum_discriminant(k, hasher);
    }
    Length(l) => {
      1u8.hash(hasher);
      hash_length(l, hasher);
    }
    Percentage(p) => {
      2u8.hash(hasher);
      hash_f32(*p, hasher);
    }
  }
}

fn hash_object_position(pos: &crate::style::types::ObjectPosition, hasher: &mut DefaultHasher) {
  hash_position_component(&pos.x, hasher);
  hash_position_component(&pos.y, hasher);
}

fn hash_font_weight(weight: &crate::style::types::FontWeight, hasher: &mut DefaultHasher) {
  weight.to_u16().hash(hasher);
}

fn hash_font_style(style: &crate::style::types::FontStyle, hasher: &mut DefaultHasher) {
  match style {
    crate::style::types::FontStyle::Normal => 0u8.hash(hasher),
    crate::style::types::FontStyle::Italic => 1u8.hash(hasher),
    crate::style::types::FontStyle::Oblique(angle) => {
      2u8.hash(hasher);
      match angle {
        Some(a) => {
          1u8.hash(hasher);
          hash_f32(*a, hasher);
        }
        None => 0u8.hash(hasher),
      }
    }
  }
}

fn hash_font_language_override(
  override_lang: &crate::style::types::FontLanguageOverride,
  hasher: &mut DefaultHasher,
) {
  match override_lang {
    crate::style::types::FontLanguageOverride::Normal => 0u8.hash(hasher),
    crate::style::types::FontLanguageOverride::Override(tag) => {
      1u8.hash(hasher);
      tag.hash(hasher);
    }
  }
}

fn hash_font_feature_settings(
  settings: &[crate::style::types::FontFeatureSetting],
  hasher: &mut DefaultHasher,
) {
  settings.len().hash(hasher);
  for setting in settings {
    setting.tag.hash(hasher);
    setting.value.hash(hasher);
  }
}

fn hash_font_variation_settings(
  settings: &[crate::style::types::FontVariationSetting],
  hasher: &mut DefaultHasher,
) {
  settings.len().hash(hasher);
  for setting in settings {
    setting.tag.hash(hasher);
    hash_f32(setting.value, hasher);
  }
}

fn hash_string_vec(list: &[String], hasher: &mut DefaultHasher) {
  list.len().hash(hasher);
  for s in list {
    s.hash(hasher);
  }
}

fn hash_text_transform(transform: &crate::style::types::TextTransform, hasher: &mut DefaultHasher) {
  hash_enum_discriminant(&transform.case, hasher);
  transform.full_width.hash(hasher);
  transform.full_size_kana.hash(hasher);
}

fn hash_tab_size(tab: &crate::style::types::TabSize, hasher: &mut DefaultHasher) {
  match tab {
    crate::style::types::TabSize::Number(n) => {
      0u8.hash(hasher);
      hash_f32(*n, hasher);
    }
    crate::style::types::TabSize::Length(l) => {
      1u8.hash(hasher);
      hash_length(l, hasher);
    }
  }
}

fn hash_vertical_align(align: &crate::style::types::VerticalAlign, hasher: &mut DefaultHasher) {
  use crate::style::types::VerticalAlign::Baseline;
  use crate::style::types::VerticalAlign::Bottom;
  use crate::style::types::VerticalAlign::Length;
  use crate::style::types::VerticalAlign::Middle;
  use crate::style::types::VerticalAlign::Percentage;
  use crate::style::types::VerticalAlign::Sub;
  use crate::style::types::VerticalAlign::Super;
  use crate::style::types::VerticalAlign::TextBottom;
  use crate::style::types::VerticalAlign::TextTop;
  use crate::style::types::VerticalAlign::Top;
  match align {
    Baseline => 0u8.hash(hasher),
    Sub => 1u8.hash(hasher),
    Super => 2u8.hash(hasher),
    TextTop => 3u8.hash(hasher),
    TextBottom => 4u8.hash(hasher),
    Middle => 5u8.hash(hasher),
    Top => 6u8.hash(hasher),
    Bottom => 7u8.hash(hasher),
    Length(l) => {
      8u8.hash(hasher);
      hash_length(l, hasher);
    }
    Percentage(p) => {
      9u8.hash(hasher);
      hash_f32(*p, hasher);
    }
  }
}

fn hash_line_height(height: &crate::style::types::LineHeight, hasher: &mut DefaultHasher) {
  use crate::style::types::LineHeight::Length;
  use crate::style::types::LineHeight::Normal;
  use crate::style::types::LineHeight::Number;
  use crate::style::types::LineHeight::Percentage;
  match height {
    Normal => 0u8.hash(hasher),
    Number(n) => {
      1u8.hash(hasher);
      hash_f32(*n, hasher);
    }
    Length(l) => {
      2u8.hash(hasher);
      hash_length(l, hasher);
    }
    Percentage(p) => {
      3u8.hash(hasher);
      hash_f32(*p, hasher);
    }
  }
}

fn hash_aspect_ratio(ratio: &crate::style::types::AspectRatio, hasher: &mut DefaultHasher) {
  match ratio {
    crate::style::types::AspectRatio::Auto => 0u8.hash(hasher),
    crate::style::types::AspectRatio::Ratio(v) => {
      1u8.hash(hasher);
      hash_f32(*v, hasher);
    }
  }
}

fn hash_text_indent(indent: &crate::style::types::TextIndent, hasher: &mut DefaultHasher) {
  hash_length(&indent.length, hasher);
  indent.hanging.hash(hasher);
  indent.each_line.hash(hasher);
}

fn hash_text_size_adjust(value: &crate::style::types::TextSizeAdjust, hasher: &mut DefaultHasher) {
  use crate::style::types::TextSizeAdjust::Auto;
  use crate::style::types::TextSizeAdjust::None;
  use crate::style::types::TextSizeAdjust::Percentage;
  match value {
    Auto => 0u8.hash(hasher),
    None => 1u8.hash(hasher),
    Percentage(p) => {
      2u8.hash(hasher);
      hash_f32(*p, hasher);
    }
  }
}

fn hash_list_style_type(value: &crate::style::types::ListStyleType, hasher: &mut DefaultHasher) {
  use crate::style::types::ListStyleType::Armenian;
  use crate::style::types::ListStyleType::Circle;
  use crate::style::types::ListStyleType::Custom;
  use crate::style::types::ListStyleType::Decimal;
  use crate::style::types::ListStyleType::DecimalLeadingZero;
  use crate::style::types::ListStyleType::Disc;
  use crate::style::types::ListStyleType::DisclosureClosed;
  use crate::style::types::ListStyleType::DisclosureOpen;
  use crate::style::types::ListStyleType::Georgian;
  use crate::style::types::ListStyleType::LowerAlpha;
  use crate::style::types::ListStyleType::LowerArmenian;
  use crate::style::types::ListStyleType::LowerGreek;
  use crate::style::types::ListStyleType::LowerRoman;
  use crate::style::types::ListStyleType::None;
  use crate::style::types::ListStyleType::Square;
  use crate::style::types::ListStyleType::String;
  use crate::style::types::ListStyleType::UpperAlpha;
  use crate::style::types::ListStyleType::UpperRoman;
  match value {
    Disc => 0u8.hash(hasher),
    Circle => 1u8.hash(hasher),
    Square => 2u8.hash(hasher),
    Decimal => 3u8.hash(hasher),
    DecimalLeadingZero => 4u8.hash(hasher),
    LowerRoman => 5u8.hash(hasher),
    UpperRoman => 6u8.hash(hasher),
    LowerAlpha => 7u8.hash(hasher),
    UpperAlpha => 8u8.hash(hasher),
    Armenian => 9u8.hash(hasher),
    LowerArmenian => 10u8.hash(hasher),
    Georgian => 11u8.hash(hasher),
    LowerGreek => 12u8.hash(hasher),
    DisclosureOpen => 13u8.hash(hasher),
    DisclosureClosed => 14u8.hash(hasher),
    Custom(name) => {
      17u8.hash(hasher);
      name.hash(hasher);
    }
    String(s) => {
      15u8.hash(hasher);
      s.hash(hasher);
    }
    None => 16u8.hash(hasher),
  }
}

fn hash_list_style_image(value: &crate::style::types::ListStyleImage, hasher: &mut DefaultHasher) {
  match value {
    crate::style::types::ListStyleImage::None => 0u8.hash(hasher),
    crate::style::types::ListStyleImage::Url(url) => {
      1u8.hash(hasher);
      url.hash(hasher);
    }
  }
}

fn hash_grid_track(track: &crate::style::types::GridTrack, hasher: &mut DefaultHasher) {
  use crate::style::types::GridTrack::Auto;
  use crate::style::types::GridTrack::FitContent;
  use crate::style::types::GridTrack::Fr;
  use crate::style::types::GridTrack::Length;
  use crate::style::types::GridTrack::MaxContent;
  use crate::style::types::GridTrack::MinContent;
  use crate::style::types::GridTrack::MinMax;
  use crate::style::types::GridTrack::RepeatAutoFill;
  use crate::style::types::GridTrack::RepeatAutoFit;
  match track {
    Length(l) => {
      0u8.hash(hasher);
      hash_length(l, hasher);
    }
    Fr(v) => {
      1u8.hash(hasher);
      hash_f32(*v, hasher);
    }
    Auto => 2u8.hash(hasher),
    MinContent => 3u8.hash(hasher),
    MaxContent => 4u8.hash(hasher),
    FitContent(l) => {
      5u8.hash(hasher);
      hash_length(l, hasher);
    }
    MinMax(a, b) => {
      6u8.hash(hasher);
      hash_grid_track(a, hasher);
      hash_grid_track(b, hasher);
    }
    RepeatAutoFill { tracks, line_names } => {
      7u8.hash(hasher);
      hash_grid_tracks(tracks, hasher);
      hash_grid_line_names(line_names, hasher);
    }
    RepeatAutoFit { tracks, line_names } => {
      8u8.hash(hasher);
      hash_grid_tracks(tracks, hasher);
      hash_grid_line_names(line_names, hasher);
    }
  }
}

fn hash_grid_tracks(tracks: &[crate::style::types::GridTrack], hasher: &mut DefaultHasher) {
  tracks.len().hash(hasher);
  for t in tracks {
    hash_grid_track(t, hasher);
  }
}

fn hash_grid_line_names(names: &[Vec<String>], hasher: &mut DefaultHasher) {
  names.len().hash(hasher);
  for line in names {
    hash_string_vec(line, hasher);
  }
}

fn hash_grid_named_lines(map: &HashMap<String, Vec<usize>>, hasher: &mut DefaultHasher) {
  let mut entries: Vec<_> = map.iter().collect();
  entries.sort_by(|a, b| a.0.cmp(b.0));
  entries.len().hash(hasher);
  for (name, positions) in entries {
    name.hash(hasher);
    positions.hash(hasher);
  }
}

fn hash_grid_template_areas(areas: &[Vec<Option<String>>], hasher: &mut DefaultHasher) {
  areas.len().hash(hasher);
  for row in areas {
    row.len().hash(hasher);
    for cell in row {
      match cell {
        Some(name) => {
          1u8.hash(hasher);
          name.hash(hasher);
        }
        None => 0u8.hash(hasher),
      }
    }
  }
}

fn hash_font_size_adjust(value: &crate::style::types::FontSizeAdjust, hasher: &mut DefaultHasher) {
  use crate::style::types::FontSizeAdjust::FromFont;
  use crate::style::types::FontSizeAdjust::None;
  use crate::style::types::FontSizeAdjust::Number;
  match value {
    None => 0u8.hash(hasher),
    Number(n) => {
      1u8.hash(hasher);
      hash_f32(*n, hasher);
    }
    FromFont => 2u8.hash(hasher),
  }
}

fn hash_font_variant_alternates(
  value: &crate::style::types::FontVariantAlternates,
  hasher: &mut DefaultHasher,
) {
  value.historical_forms.hash(hasher);
  value.stylistic.hash(hasher);
  value.stylesets.hash(hasher);
  value.character_variants.hash(hasher);
  value.swash.hash(hasher);
  value.ornaments.hash(hasher);
  value.annotation.hash(hasher);
}

fn hash_font_variant_numeric(
  value: &crate::style::types::FontVariantNumeric,
  hasher: &mut DefaultHasher,
) {
  hash_enum_discriminant(&value.figure, hasher);
  hash_enum_discriminant(&value.spacing, hasher);
  hash_enum_discriminant(&value.fraction, hasher);
  value.ordinal.hash(hasher);
  value.slashed_zero.hash(hasher);
}

fn hash_font_variant_ligatures(
  value: &crate::style::types::FontVariantLigatures,
  hasher: &mut DefaultHasher,
) {
  value.common.hash(hasher);
  value.discretionary.hash(hasher);
  value.historical.hash(hasher);
  value.contextual.hash(hasher);
}

fn hash_font_variant_east_asian(
  value: &crate::style::types::FontVariantEastAsian,
  hasher: &mut DefaultHasher,
) {
  match value.variant {
    Some(crate::style::types::EastAsianVariant::Jis78) => 1u8.hash(hasher),
    Some(crate::style::types::EastAsianVariant::Jis83) => 2u8.hash(hasher),
    Some(crate::style::types::EastAsianVariant::Jis90) => 3u8.hash(hasher),
    Some(crate::style::types::EastAsianVariant::Jis04) => 4u8.hash(hasher),
    Some(crate::style::types::EastAsianVariant::Simplified) => 5u8.hash(hasher),
    Some(crate::style::types::EastAsianVariant::Traditional) => 6u8.hash(hasher),
    None => 0u8.hash(hasher),
  }
  match value.width {
    Some(crate::style::types::EastAsianWidth::FullWidth) => 1u8.hash(hasher),
    Some(crate::style::types::EastAsianWidth::ProportionalWidth) => 2u8.hash(hasher),
    None => 0u8.hash(hasher),
  }
  value.ruby.hash(hasher);
}

fn hash_font_synthesis(value: &crate::style::types::FontSynthesis, hasher: &mut DefaultHasher) {
  value.weight.hash(hasher);
  value.style.hash(hasher);
  value.small_caps.hash(hasher);
  value.position.hash(hasher);
}

fn hash_transform(transform: &crate::css::types::Transform, hasher: &mut DefaultHasher) {
  use crate::css::types::Transform::Matrix;
  use crate::css::types::Transform::Matrix3d;
  use crate::css::types::Transform::Perspective;
  use crate::css::types::Transform::Rotate;
  use crate::css::types::Transform::Rotate3d;
  use crate::css::types::Transform::RotateX;
  use crate::css::types::Transform::RotateY;
  use crate::css::types::Transform::RotateZ;
  use crate::css::types::Transform::Scale;
  use crate::css::types::Transform::Scale3d;
  use crate::css::types::Transform::ScaleX;
  use crate::css::types::Transform::ScaleY;
  use crate::css::types::Transform::ScaleZ;
  use crate::css::types::Transform::Skew;
  use crate::css::types::Transform::SkewX;
  use crate::css::types::Transform::SkewY;
  use crate::css::types::Transform::Translate;
  use crate::css::types::Transform::Translate3d;
  use crate::css::types::Transform::TranslateX;
  use crate::css::types::Transform::TranslateY;
  use crate::css::types::Transform::TranslateZ;
  match transform {
    Translate(x, y) => {
      0u8.hash(hasher);
      hash_length(x, hasher);
      hash_length(y, hasher);
    }
    TranslateX(x) => {
      1u8.hash(hasher);
      hash_length(x, hasher);
    }
    TranslateY(y) => {
      2u8.hash(hasher);
      hash_length(y, hasher);
    }
    TranslateZ(z) => {
      3u8.hash(hasher);
      hash_length(z, hasher);
    }
    Translate3d(x, y, z) => {
      4u8.hash(hasher);
      hash_length(x, hasher);
      hash_length(y, hasher);
      hash_length(z, hasher);
    }
    Scale(x, y) => {
      5u8.hash(hasher);
      hash_f32(*x, hasher);
      hash_f32(*y, hasher);
    }
    ScaleX(x) => {
      6u8.hash(hasher);
      hash_f32(*x, hasher);
    }
    ScaleY(y) => {
      7u8.hash(hasher);
      hash_f32(*y, hasher);
    }
    ScaleZ(z) => {
      8u8.hash(hasher);
      hash_f32(*z, hasher);
    }
    Scale3d(x, y, z) => {
      9u8.hash(hasher);
      hash_f32(*x, hasher);
      hash_f32(*y, hasher);
      hash_f32(*z, hasher);
    }
    Rotate(r) => {
      10u8.hash(hasher);
      hash_f32(*r, hasher);
    }
    RotateZ(r) => {
      11u8.hash(hasher);
      hash_f32(*r, hasher);
    }
    RotateX(r) => {
      12u8.hash(hasher);
      hash_f32(*r, hasher);
    }
    RotateY(r) => {
      13u8.hash(hasher);
      hash_f32(*r, hasher);
    }
    Rotate3d(x, y, z, r) => {
      14u8.hash(hasher);
      hash_f32(*x, hasher);
      hash_f32(*y, hasher);
      hash_f32(*z, hasher);
      hash_f32(*r, hasher);
    }
    SkewX(x) => {
      15u8.hash(hasher);
      hash_f32(*x, hasher);
    }
    SkewY(y) => {
      16u8.hash(hasher);
      hash_f32(*y, hasher);
    }
    Skew(ax, ay) => {
      17u8.hash(hasher);
      hash_f32(*ax, hasher);
      hash_f32(*ay, hasher);
    }
    Perspective(len) => {
      18u8.hash(hasher);
      hash_length(len, hasher);
    }
    Matrix(a, b, c, d, e, f) => {
      19u8.hash(hasher);
      hash_f32(*a, hasher);
      hash_f32(*b, hasher);
      hash_f32(*c, hasher);
      hash_f32(*d, hasher);
      hash_f32(*e, hasher);
      hash_f32(*f, hasher);
    }
    Matrix3d(values) => {
      20u8.hash(hasher);
      for v in values {
        hash_f32(*v, hasher);
      }
    }
  }
}

fn hash_transforms(transforms: &[crate::css::types::Transform], hasher: &mut DefaultHasher) {
  transforms.len().hash(hasher);
  for t in transforms {
    hash_transform(t, hasher);
  }
}

fn style_layout_fingerprint(style: &ComputedStyle) -> u64 {
  let mut h = DefaultHasher::new();
  hash_enum_discriminant(&style.display, &mut h);
  hash_enum_discriminant(&style.container_type, &mut h);
  hash_string_vec(&style.container_name, &mut h);
  hash_enum_discriminant(&style.containment, &mut h);
  hash_enum_discriminant(&style.position, &mut h);
  hash_enum_discriminant(&style.box_sizing, &mut h);
  hash_option_length(&style.top, &mut h);
  hash_option_length(&style.right, &mut h);
  hash_option_length(&style.bottom, &mut h);
  hash_option_length(&style.left, &mut h);
  hash_enum_discriminant(&style.float, &mut h);
  hash_enum_discriminant(&style.clear, &mut h);
  hash_option_length(&style.width, &mut h);
  hash_option_length(&style.height, &mut h);
  hash_option_length(&style.min_width, &mut h);
  hash_option_length(&style.max_width, &mut h);
  hash_option_length(&style.min_height, &mut h);
  hash_option_length(&style.max_height, &mut h);
  hash_option_length(&style.margin_top, &mut h);
  hash_option_length(&style.margin_right, &mut h);
  hash_option_length(&style.margin_bottom, &mut h);
  hash_option_length(&style.margin_left, &mut h);
  hash_length(&style.padding_top, &mut h);
  hash_length(&style.padding_right, &mut h);
  hash_length(&style.padding_bottom, &mut h);
  hash_length(&style.padding_left, &mut h);
  hash_length(&style.border_top_width, &mut h);
  hash_length(&style.border_right_width, &mut h);
  hash_length(&style.border_bottom_width, &mut h);
  hash_length(&style.border_left_width, &mut h);
  style.order.hash(&mut h);
  hash_border_style(&style.border_top_style, &mut h);
  hash_border_style(&style.border_right_style, &mut h);
  hash_border_style(&style.border_bottom_style, &mut h);
  hash_border_style(&style.border_left_style, &mut h);
  hash_grid_tracks(&style.grid_template_columns, &mut h);
  hash_grid_tracks(&style.grid_template_rows, &mut h);
  hash_grid_template_areas(&style.grid_template_areas, &mut h);
  hash_grid_tracks(&style.grid_auto_rows, &mut h);
  hash_grid_tracks(&style.grid_auto_columns, &mut h);
  hash_enum_discriminant(&style.grid_auto_flow, &mut h);
  hash_grid_named_lines(&style.grid_column_names, &mut h);
  hash_grid_named_lines(&style.grid_row_names, &mut h);
  hash_grid_line_names(&style.grid_column_line_names, &mut h);
  hash_grid_line_names(&style.grid_row_line_names, &mut h);
  hash_length(&style.grid_gap, &mut h);
  hash_length(&style.grid_row_gap, &mut h);
  hash_length(&style.grid_column_gap, &mut h);
  style.grid_column_start.hash(&mut h);
  style.grid_column_end.hash(&mut h);
  style.grid_row_start.hash(&mut h);
  style.grid_row_end.hash(&mut h);
  style.grid_column_raw.hash(&mut h);
  style.grid_row_raw.hash(&mut h);
  match style.column_count {
    Some(c) => {
      1u8.hash(&mut h);
      c.hash(&mut h);
    }
    None => 0u8.hash(&mut h),
  }
  hash_option_length(&style.column_width, &mut h);
  hash_length(&style.column_gap, &mut h);
  match style.column_rule_color {
    Some(c) => {
      1u8.hash(&mut h);
      c.r.hash(&mut h);
      c.g.hash(&mut h);
      c.b.hash(&mut h);
      c.a.to_bits().hash(&mut h);
    }
    None => 0u8.hash(&mut h),
  }
  hash_border_style(&style.column_rule_style, &mut h);
  hash_length(&style.column_rule_width, &mut h);
  hash_enum_discriminant(&style.column_fill, &mut h);
  hash_enum_discriminant(&style.column_span, &mut h);
  hash_enum_discriminant(&style.flex_direction, &mut h);
  hash_enum_discriminant(&style.flex_wrap, &mut h);
  hash_enum_discriminant(&style.justify_content, &mut h);
  hash_enum_discriminant(&style.align_items, &mut h);
  hash_enum_discriminant(&style.align_content, &mut h);
  match style.align_self {
    Some(v) => {
      1u8.hash(&mut h);
      hash_enum_discriminant(&v, &mut h);
    }
    None => 0u8.hash(&mut h),
  }
  hash_enum_discriminant(&style.justify_items, &mut h);
  match style.justify_self {
    Some(v) => {
      1u8.hash(&mut h);
      hash_enum_discriminant(&v, &mut h);
    }
    None => 0u8.hash(&mut h),
  }
  style.flex_grow.to_bits().hash(&mut h);
  style.flex_shrink.to_bits().hash(&mut h);
  hash_flex_basis(&style.flex_basis, &mut h);
  hash_aspect_ratio(&style.aspect_ratio, &mut h);
  // Intrinsic/text sizing influences: include font + text layout knobs.
  hash_string_vec(&style.font_family, &mut h);
  hash_font_weight(&style.font_weight, &mut h);
  hash_font_style(&style.font_style, &mut h);
  hash_enum_discriminant(&style.font_variant, &mut h);
  hash_enum_discriminant(&style.font_variant_caps, &mut h);
  hash_font_variant_alternates(&style.font_variant_alternates, &mut h);
  hash_font_variant_numeric(&style.font_variant_numeric, &mut h);
  hash_font_variant_east_asian(&style.font_variant_east_asian, &mut h);
  hash_font_variant_ligatures(&style.font_variant_ligatures, &mut h);
  hash_enum_discriminant(&style.font_variant_position, &mut h);
  hash_font_size_adjust(&style.font_size_adjust, &mut h);
  hash_font_synthesis(&style.font_synthesis, &mut h);
  hash_font_feature_settings(&style.font_feature_settings, &mut h);
  hash_font_variation_settings(&style.font_variation_settings, &mut h);
  hash_enum_discriminant(&style.font_optical_sizing, &mut h);
  hash_font_language_override(&style.font_language_override, &mut h);
  hash_enum_discriminant(&style.font_variant_emoji, &mut h);
  hash_enum_discriminant(&style.font_stretch, &mut h);
  hash_enum_discriminant(&style.font_kerning, &mut h);
  style.font_size.to_bits().hash(&mut h);
  style.root_font_size.to_bits().hash(&mut h);
  hash_line_height(&style.line_height, &mut h);
  hash_enum_discriminant(&style.direction, &mut h);
  hash_enum_discriminant(&style.unicode_bidi, &mut h);
  hash_enum_discriminant(&style.text_align, &mut h);
  hash_enum_discriminant(&style.text_align_last, &mut h);
  hash_enum_discriminant(&style.text_justify, &mut h);
  hash_enum_discriminant(&style.text_wrap, &mut h);
  hash_text_indent(&style.text_indent, &mut h);
  hash_text_size_adjust(&style.text_size_adjust, &mut h);
  hash_enum_discriminant(&style.text_rendering, &mut h);
  hash_text_transform(&style.text_transform, &mut h);
  hash_enum_discriminant(&style.text_orientation, &mut h);
  hash_enum_discriminant(&style.text_combine_upright, &mut h);
  hash_tab_size(&style.tab_size, &mut h);
  hash_vertical_align(&style.vertical_align, &mut h);
  hash_enum_discriminant(&style.ruby_position, &mut h);
  hash_enum_discriminant(&style.ruby_align, &mut h);
  hash_enum_discriminant(&style.ruby_merge, &mut h);
  style.letter_spacing.to_bits().hash(&mut h);
  style.word_spacing.to_bits().hash(&mut h);
  hash_enum_discriminant(&style.white_space, &mut h);
  hash_enum_discriminant(&style.line_break, &mut h);
  hash_enum_discriminant(&style.hyphens, &mut h);
  hash_enum_discriminant(&style.word_break, &mut h);
  hash_enum_discriminant(&style.overflow_anchor, &mut h);
  hash_enum_discriminant(&style.overflow_wrap, &mut h);
  hash_enum_discriminant(&style.color_scheme, &mut h);
  hash_enum_discriminant(&style.writing_mode, &mut h);
  hash_enum_discriminant(&style.scroll_snap_type.axis, &mut h);
  hash_enum_discriminant(&style.scroll_snap_type.strictness, &mut h);
  hash_enum_discriminant(&style.scroll_snap_align.inline, &mut h);
  hash_enum_discriminant(&style.scroll_snap_align.block, &mut h);
  hash_enum_discriminant(&style.scroll_snap_stop, &mut h);
  hash_length(&style.scroll_padding_top, &mut h);
  hash_length(&style.scroll_padding_right, &mut h);
  hash_length(&style.scroll_padding_bottom, &mut h);
  hash_length(&style.scroll_padding_left, &mut h);
  hash_length(&style.scroll_margin_top, &mut h);
  hash_length(&style.scroll_margin_right, &mut h);
  hash_length(&style.scroll_margin_bottom, &mut h);
  hash_length(&style.scroll_margin_left, &mut h);
  style.scrollbar_gutter.stable.hash(&mut h);
  style.scrollbar_gutter.both_edges.hash(&mut h);
  hash_enum_discriminant(&style.scrollbar_width, &mut h);
  hash_scrollbar_color(&style.scrollbar_color, &mut h);
  hash_list_style_type(&style.list_style_type, &mut h);
  hash_enum_discriminant(&style.list_style_position, &mut h);
  hash_list_style_image(&style.list_style_image, &mut h);
  hash_length(&style.border_spacing_horizontal, &mut h);
  hash_length(&style.border_spacing_vertical, &mut h);
  hash_enum_discriminant(&style.border_collapse, &mut h);
  hash_enum_discriminant(&style.table_layout, &mut h);
  hash_enum_discriminant(&style.caption_side, &mut h);
  hash_enum_discriminant(&style.empty_cells, &mut h);
  hash_overflow(&style.overflow_x, &mut h);
  hash_overflow(&style.overflow_y, &mut h);
  hash_enum_discriminant(&style.object_fit, &mut h);
  hash_object_position(&style.object_position, &mut h);
  hash_transforms(&style.transform, &mut h);
  hash_enum_discriminant(&style.transform_box, &mut h);
  hash_enum_discriminant(&style.transform_style, &mut h);
  hash_option_length(&style.perspective, &mut h);
  hash_length(&style.perspective_origin.x, &mut h);
  hash_length(&style.perspective_origin.y, &mut h);
  hash_enum_discriminant(&style.backface_visibility, &mut h);
  h.finish()
}

fn styled_fingerprint_map(root: &StyledNode) -> HashMap<usize, u64> {
  fn walk(node: &StyledNode, out: &mut HashMap<usize, u64>) {
    let base = node.node_id << 2;
    out.insert(base, style_layout_fingerprint(&node.styles));
    if let Some(before) = &node.before_styles {
      out.insert(base | 1, style_layout_fingerprint(before));
    }
    if let Some(after) = &node.after_styles {
      out.insert(base | 2, style_layout_fingerprint(after));
    }
    if let Some(marker) = &node.marker_styles {
      out.insert(base | 3, style_layout_fingerprint(marker));
    }
    for child in &node.children {
      walk(child, out);
    }
  }

  let mut map = HashMap::new();
  walk(root, &mut map);
  map
}

fn extract_fragment(url: &str) -> Option<String> {
  url.find('#').and_then(|idx| {
    let frag = &url[idx + 1..];
    if frag.is_empty() {
      None
    } else {
      Some(frag.to_string())
    }
  })
}

fn collect_fragment_sizes(fragment: &FragmentNode, sizes: &mut HashMap<usize, (f32, f32)>) {
  let box_id = match &fragment.content {
    FragmentContent::Block { box_id }
    | FragmentContent::Inline { box_id, .. }
    | FragmentContent::Text { box_id, .. }
    | FragmentContent::Replaced { box_id, .. } => *box_id,
    FragmentContent::Line { .. } => None,
  };

  if let Some(id) = box_id {
    let entry = sizes.entry(id).or_insert((0.0, 0.0));
    entry.0 = entry.0.max(fragment.bounds.width());
    entry.1 = entry.1.max(fragment.bounds.height());
  }

  for child in &fragment.children {
    collect_fragment_sizes(child, sizes);
  }
}

fn collect_box_nodes<'a>(node: &'a BoxNode, map: &mut HashMap<usize, &'a BoxNode>) {
  map.insert(node.id, node);
  for child in &node.children {
    collect_box_nodes(child, map);
  }
}

fn styled_style_map(root: &StyledNode) -> HashMap<usize, Arc<ComputedStyle>> {
  fn walk(node: &StyledNode, out: &mut HashMap<usize, Arc<ComputedStyle>>) {
    let base = node.node_id << 2;
    out.insert(base, Arc::new(node.styles.clone()));
    if let Some(before) = &node.before_styles {
      out.insert(base | 1, Arc::new(before.as_ref().clone()));
    }
    if let Some(after) = &node.after_styles {
      out.insert(base | 2, Arc::new(after.as_ref().clone()));
    }
    if let Some(marker) = &node.marker_styles {
      out.insert(base | 3, Arc::new(marker.as_ref().clone()));
    }
    for child in &node.children {
      walk(child, out);
    }
  }

  let mut map = HashMap::new();
  walk(root, &mut map);
  map
}

fn find_first_page_name(node: &StyledNode) -> Option<String> {
  if let Some(name) = &node.styles.page {
    return Some(name.clone());
  }
  for child in &node.children {
    if let Some(name) = find_first_page_name(child) {
      return Some(name);
    }
  }
  None
}

fn styled_summary_map(root: &StyledNode) -> HashMap<usize, String> {
  fn summary(node: &StyledNode) -> String {
    if let crate::dom::DomNodeType::Element {
      tag_name,
      namespace: _,
      attributes,
    } = &node.node.node_type
    {
      let mut out = tag_name.clone();
      if let Some((_, id_val)) = attributes
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case("id"))
      {
        out.push('#');
        out.push_str(id_val);
      }
      if let Some((_, class_val)) = attributes
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case("class"))
      {
        let classes: Vec<&str> = class_val.split_whitespace().collect();
        if !classes.is_empty() {
          out.push('.');
          out.push_str(&classes.join("."));
        }
      }
      out
    } else {
      "<non-element>".to_string()
    }
  }

  fn walk(node: &StyledNode, out: &mut HashMap<usize, String>) {
    out.insert(node.node_id, summary(node));
    for child in &node.children {
      walk(child, out);
    }
  }

  let mut map = HashMap::new();
  walk(root, &mut map);
  map
}

fn find_styled_by_id<'a>(root: &'a StyledNode, id: usize) -> Option<&'a StyledNode> {
  if root.node_id == id {
    return Some(root);
  }
  root
    .children
    .iter()
    .find_map(|child| find_styled_by_id(child, id))
}

fn styled_style_summary(style: &ComputedStyle) -> String {
  use crate::style::display::Display;
  let display = match style.display {
    Display::None => "none",
    Display::Block => "block",
    Display::Inline => "inline",
    Display::InlineBlock => "inline-block",
    Display::Flex => "flex",
    Display::InlineFlex => "inline-flex",
    Display::Grid => "grid",
    Display::InlineGrid => "inline-grid",
    Display::ListItem => "list-item",
    Display::Table => "table",
    Display::TableRow => "table-row",
    Display::TableCell => "table-cell",
    _ => "other",
  };
  let ct = match style.container_type {
    ContainerType::None => "none",
    ContainerType::Normal => "normal",
    ContainerType::Size => "size",
    ContainerType::InlineSize => "inline-size",
  };
  let names = if style.container_name.is_empty() {
    String::new()
  } else {
    format!(" names={:?}", style.container_name)
  };
  format!(
    "display={} width={:?} height={:?} min=({:?},{:?}) max=({:?},{:?}) container_type={}{}",
    display,
    style.width,
    style.height,
    style.min_width,
    style.min_height,
    style.max_width,
    style.max_height,
    ct,
    names
  )
}

#[allow(clippy::cognitive_complexity)]
fn diff_layout_fields(old: &ComputedStyle, new: &ComputedStyle) -> Vec<String> {
  let mut out = Vec::new();
  macro_rules! cmp {
    ($field:ident) => {
      if old.$field != new.$field {
        out.push(stringify!($field).to_string());
      }
    };
  }
  cmp!(display);
  cmp!(position);
  cmp!(float);
  cmp!(clear);
  cmp!(box_sizing);
  cmp!(width);
  cmp!(height);
  cmp!(min_width);
  cmp!(min_height);
  cmp!(max_width);
  cmp!(max_height);
  cmp!(margin_top);
  cmp!(margin_right);
  cmp!(margin_bottom);
  cmp!(margin_left);
  cmp!(padding_top);
  cmp!(padding_right);
  cmp!(padding_bottom);
  cmp!(padding_left);
  cmp!(border_top_width);
  cmp!(border_right_width);
  cmp!(border_bottom_width);
  cmp!(border_left_width);
  cmp!(border_top_style);
  cmp!(border_right_style);
  cmp!(border_bottom_style);
  cmp!(border_left_style);
  cmp!(flex_direction);
  cmp!(flex_wrap);
  cmp!(justify_content);
  cmp!(align_items);
  cmp!(align_content);
  cmp!(align_self);
  cmp!(justify_items);
  cmp!(justify_self);
  if old.flex_grow.to_bits() != new.flex_grow.to_bits() {
    out.push("flex_grow".to_string());
  }
  if old.flex_shrink.to_bits() != new.flex_shrink.to_bits() {
    out.push("flex_shrink".to_string());
  }
  if old.flex_basis != new.flex_basis {
    out.push("flex_basis".to_string());
  }
  cmp!(aspect_ratio);
  cmp!(grid_template_columns);
  cmp!(grid_template_rows);
  cmp!(grid_auto_flow);
  cmp!(grid_auto_rows);
  cmp!(grid_auto_columns);
  cmp!(grid_gap);
  cmp!(grid_row_gap);
  cmp!(grid_column_gap);
  cmp!(grid_column_start);
  cmp!(grid_column_end);
  cmp!(grid_row_start);
  cmp!(grid_row_end);
  cmp!(column_count);
  cmp!(column_width);
  cmp!(column_gap);
  if old.column_rule_color != new.column_rule_color {
    out.push("column_rule_color".to_string());
  }
  cmp!(column_rule_style);
  cmp!(column_rule_width);
  cmp!(column_fill);
  cmp!(column_span);
  cmp!(writing_mode);
  cmp!(direction);
  cmp!(unicode_bidi);
  cmp!(text_align);
  cmp!(text_align_last);
  cmp!(scroll_snap_type);
  cmp!(scroll_snap_align);
  cmp!(scroll_snap_stop);
  cmp!(scroll_padding_top);
  cmp!(scroll_padding_right);
  cmp!(scroll_padding_bottom);
  cmp!(scroll_padding_left);
  cmp!(scroll_margin_top);
  cmp!(scroll_margin_right);
  cmp!(scroll_margin_bottom);
  cmp!(scroll_margin_left);
  cmp!(vertical_align);
  cmp!(line_height);
  cmp!(font_size);
  cmp!(font_family);
  cmp!(font_weight);
  cmp!(font_style);
  cmp!(container_type);
  if old.container_name != new.container_name {
    out.push("container_name".to_string());
  }
  out
}

fn box_style_key(node: &BoxNode) -> Option<usize> {
  let styled_id = node.styled_node_id?;
  let base = styled_id << 2;

  if matches!(node.box_type, BoxType::Marker(_)) {
    return Some(base | 3);
  }

  if let Some(info) = &node.debug_info {
    if info.classes.iter().any(|c| c == "pseudo-element") {
      if info.tag_name.as_deref() == Some("before") {
        return Some(base | 1);
      }
      if info.tag_name.as_deref() == Some("after") {
        return Some(base | 2);
      }
    }
  }

  Some(base)
}

fn fragment_box_id(fragment: &FragmentNode) -> Option<usize> {
  match &fragment.content {
    FragmentContent::Block { box_id }
    | FragmentContent::Inline { box_id, .. }
    | FragmentContent::Text { box_id, .. }
    | FragmentContent::Replaced { box_id, .. } => *box_id,
    FragmentContent::Line { .. } => None,
  }
}

fn refresh_fragment_styles(
  fragment: &mut FragmentNode,
  boxes: &HashMap<usize, &BoxNode>,
  styles: &HashMap<usize, Arc<ComputedStyle>>,
) {
  if let Some(box_id) = fragment_box_id(fragment) {
    if let Some(node) = boxes.get(&box_id) {
      if let Some(key) = box_style_key(node) {
        if let Some(style) = styles.get(&key) {
          fragment.style = Some(style.clone());
        }
      }
    }
  }

  for child in &mut fragment.children {
    refresh_fragment_styles(child, boxes, styles);
  }
}

fn build_container_query_context(
  box_tree: &BoxTree,
  fragments: &FragmentTree,
  media_ctx: &MediaContext,
) -> ContainerQueryContext {
  fn content_box_sizes(node: &BoxNode, inline: f32, block: f32) -> (f32, f32) {
    let mut w = inline;
    let mut h = block;

    let hp = node.style.padding_left.to_px()
      + node.style.padding_right.to_px()
      + node.style.border_left_width.to_px()
      + node.style.border_right_width.to_px();
    let vp = node.style.padding_top.to_px()
      + node.style.padding_bottom.to_px()
      + node.style.border_top_width.to_px()
      + node.style.border_bottom_width.to_px();

    if hp.is_finite() {
      w = (w - hp).max(0.0);
    }
    if vp.is_finite() {
      h = (h - vp).max(0.0);
    }
    (w, h)
  }

  let mut sizes: HashMap<usize, (f32, f32)> = HashMap::new();
  collect_fragment_sizes(&fragments.root, &mut sizes);

  let mut boxes: HashMap<usize, &BoxNode> = HashMap::new();
  collect_box_nodes(&box_tree.root, &mut boxes);

  let mut containers: HashMap<usize, ContainerQueryInfo> = HashMap::new();
  for (box_id, node) in boxes {
    let styled_id = match node.styled_node_id {
      Some(id) => id,
      None => continue,
    };
    match node.style.container_type {
      ContainerType::Size | ContainerType::InlineSize => {
        if let Some((inline, block)) = sizes.get(&box_id) {
          let (content_inline, content_block) = content_box_sizes(node, *inline, *block);
          if runtime::runtime_toggles().truthy("FASTR_LOG_CONTAINER_QUERY") {
            eprintln!(
                            "[cq] box_id={} styled_id={} type={:?} inline={:.2} block={:.2} content_inline={:.2} content_block={:.2}",
                            box_id,
                            styled_id,
                            node.style.container_type,
                            inline,
                            block,
                            content_inline,
                            content_block
                        );
          }
          containers
            .entry(styled_id)
            .and_modify(|entry| {
              entry.inline_size = entry.inline_size.max(content_inline);
              entry.block_size = entry.block_size.max(content_block);
              entry.font_size = entry.font_size.max(node.style.font_size);
            })
            .or_insert_with(|| ContainerQueryInfo {
              inline_size: content_inline,
              block_size: content_block,
              container_type: node.style.container_type,
              names: node.style.container_name.clone(),
              font_size: node.style.font_size,
            });
        }
      }
      ContainerType::None | ContainerType::Normal => {}
    }
  }

  ContainerQueryContext {
    base_media: media_ctx.clone(),
    containers,
  }
}

fn count_styled_nodes_api(node: &StyledNode) -> usize {
  1 + node
    .children
    .iter()
    .map(count_styled_nodes_api)
    .sum::<usize>()
}

fn build_container_scope(styled: &StyledNode, ctx: &ContainerQueryContext) -> HashSet<usize> {
  fn mark(
    node: &StyledNode,
    containers: &HashMap<usize, ContainerQueryInfo>,
    in_container_subtree: bool,
    scope: &mut HashSet<usize>,
  ) -> bool {
    let is_container = containers.contains_key(&node.node_id);
    let mut subtree_has_container = is_container;
    for child in &node.children {
      if mark(
        child,
        containers,
        in_container_subtree || is_container,
        scope,
      ) {
        subtree_has_container = true;
      }
    }

    // Re-run cascade for:
    // - any node that is itself a container,
    // - any descendant of a container (styles may depend on queries),
    // - any ancestor on the path to a container (to reach it).
    if is_container || in_container_subtree || subtree_has_container {
      scope.insert(node.node_id);
    }

    subtree_has_container
  }

  let mut scope = HashSet::new();
  mark(styled, &ctx.containers, false, &mut scope);
  scope
}

fn build_styled_lookup<'a>(styled: &'a StyledNode, out: &mut HashMap<usize, *const StyledNode>) {
  out.insert(styled.node_id, styled as *const _);
  for child in &styled.children {
    build_styled_lookup(child, out);
  }
}

fn boolish(value: &str) -> bool {
  matches!(
    value.to_ascii_lowercase().as_str(),
    "true" | "1" | "yes" | "on" | "open"
  )
}

fn data_fastr_open_state(node: &DomNode) -> Option<(bool, bool)> {
  let value = node.get_attribute_ref("data-fastr-open")?;
  let lower = value.to_ascii_lowercase();
  if lower == "false" {
    return Some((false, false));
  }
  if lower == "modal" {
    return Some((true, true));
  }
  if boolish(&lower) {
    return Some((true, false));
  }
  None
}

fn dialog_state(node: &DomNode) -> Option<(bool, bool)> {
  if !node
    .tag_name()
    .map(|t| t.eq_ignore_ascii_case("dialog"))
    .unwrap_or(false)
  {
    return None;
  }

  let mut open = node.get_attribute_ref("open").is_some();
  let mut modal = node
    .get_attribute_ref("data-fastr-modal")
    .map(boolish)
    .unwrap_or(false);
  if let Some((open_override, modal_override)) = data_fastr_open_state(node) {
    open = open_override;
    modal |= modal_override;
  }

  if !open {
    return None;
  }

  Some((open, modal))
}

fn popover_open(node: &DomNode) -> bool {
  if node.get_attribute_ref("popover").is_none() {
    return false;
  }
  let mut open = node.get_attribute_ref("open").is_some();
  if let Some((open_override, _)) = data_fastr_open_state(node) {
    open = open_override;
  }
  open
}

fn modal_dialog_present(node: &DomNode) -> bool {
  if let Some((_, modal)) = dialog_state(node) {
    if modal {
      return true;
    }
  }

  node.children.iter().any(modal_dialog_present)
}

fn set_attr(attrs: &mut Vec<(String, String)>, name: &str, value: &str) {
  if let Some((_, val)) = attrs.iter_mut().find(|(k, _)| k.eq_ignore_ascii_case(name)) {
    *val = value.to_string();
  } else {
    attrs.push((name.to_string(), value.to_string()));
  }
}

fn remove_attr(attrs: &mut Vec<(String, String)>, name: &str) {
  if let Some(idx) = attrs.iter().position(|(k, _)| k.eq_ignore_ascii_case(name)) {
    attrs.remove(idx);
  }
}

fn apply_top_layer_state(node: &mut DomNode, modal_open: bool, inside_modal: bool) -> bool {
  let mut within_modal = inside_modal;
  let dialog_info = dialog_state(node);
  let has_popover = node.get_attribute_ref("popover").is_some();
  let popover_is_open = if has_popover {
    popover_open(node)
  } else {
    false
  };
  let mut subtree_has_modal = within_modal;

  if let crate::dom::DomNodeType::Element {
    tag_name,
    attributes,
    ..
  } = &mut node.node_type
  {
    let tag_lower = tag_name.to_ascii_lowercase();
    let mut should_open = false;

    if tag_lower == "dialog" {
      if let Some((open, modal)) = dialog_info {
        should_open = open;
        if modal {
          within_modal = true;
          subtree_has_modal = true;
        }
      }
    } else if has_popover {
      should_open = popover_is_open;
    }

    if tag_lower == "dialog" || has_popover {
      if should_open {
        set_attr(attributes, "open", "");
      } else {
        remove_attr(attributes, "open");
      }
    }
  }

  let child_modal = within_modal;
  for child in node.children.iter_mut() {
    let child_contains_modal = apply_top_layer_state(child, modal_open, child_modal);
    subtree_has_modal |= child_contains_modal;
  }

  if modal_open {
    if let crate::dom::DomNodeType::Element { attributes, .. } = &mut node.node_type {
      if !subtree_has_modal {
        set_attr(attributes, "data-fastr-inert", "true");
      }
    }
  }

  subtree_has_modal
}

/// Renders an HTML fragment using shared font/image resources. This is used for nested rendering
/// such as SVG `<foreignObject>` content so we can reuse the outer renderer's caches.
pub(crate) fn render_html_with_shared_resources(
  html: &str,
  width: u32,
  height: u32,
  background: Rgba,
  font_ctx: &FontContext,
  image_cache: &ImageCache,
  fetcher: Arc<dyn ResourceFetcher>,
  base_url: Option<String>,
  device_pixel_ratio: f32,
  resource_policy: ResourceAccessPolicy,
  resource_context: Option<ResourceContext>,
  max_iframe_depth: usize,
) -> Result<Pixmap> {
  let layout_config = LayoutConfig::for_viewport(Size::new(width as f32, height as f32))
    .with_identifier("foreignObject");
  let layout_engine = LayoutEngine::with_font_context(layout_config, font_ctx.clone());
  let mut renderer = FastRender {
    font_context: font_ctx.clone(),
    layout_engine,
    image_cache: image_cache.clone(),
    fetcher,
    diagnostics: None,
    background_color: background,
    default_width: width,
    default_height: height,
    device_pixel_ratio,
    apply_meta_viewport: false,
    pending_device_size: None,
    base_url,
    compat_profile: CompatProfile::default(),
    dom_compat_mode: DomCompatibilityMode::Standard,
    resource_policy,
    resource_context: resource_context.clone(),
    max_iframe_depth,
    runtime_toggles: runtime::runtime_toggles(),
  };

  renderer
    .image_cache
    .set_resource_context(resource_context.clone());
  renderer.font_context.set_resource_context(resource_context);

  let trace = TraceHandle::disabled();
  let toggles = renderer.runtime_toggles.clone();
  runtime::with_runtime_toggles(toggles, || {
    renderer.render_html_internal(
      html,
      width,
      height,
      0.0,
      0.0,
      MediaType::Screen,
      false,
      None,
      None,
      None,
      &trace,
    )
  })
  .map(|out| out.pixmap)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::css::parser::extract_css;
  use crate::dom::DomNodeType;
  use crate::layout::contexts::inline::line_builder::TextItem;
  use crate::layout::engine::LayoutConfig;
  use crate::layout::engine::LayoutEngine;
  use crate::layout::formatting_context::intrinsic_cache_clear;
  use crate::resource::FetchedResource;
  use crate::style::cascade::StyledNode;
  use crate::style::types::{
    BackgroundImage, ImageResolution, ScrollSnapAlign, ScrollSnapAxis, ScrollSnapStop,
    ScrollSnapStrictness, WritingMode,
  };
  use crate::text::pipeline::ShapingPipeline;
  use crate::tree::fragment_tree::FragmentContent;
  use crate::tree::fragment_tree::FragmentTree;
  use crate::ComputedStyle;
  use crate::Rect;
  use base64::Engine;
  use std::collections::HashMap;
  use std::io;
  use std::sync::Arc;

  #[derive(Clone, Default)]
  struct MapFetcher {
    map: HashMap<String, (Vec<u8>, Option<String>)>,
  }

  impl MapFetcher {
    fn with_entry(mut self, url: &str, css: &str) -> Self {
      self.map.insert(
        url.to_string(),
        (css.as_bytes().to_vec(), Some("text/css".to_string())),
      );
      self
    }
  }

  impl ResourceFetcher for MapFetcher {
    fn fetch(&self, url: &str) -> crate::error::Result<FetchedResource> {
      self
        .map
        .get(url)
        .map(|(bytes, content_type)| FetchedResource::new(bytes.clone(), content_type.clone()))
        .ok_or_else(|| {
          Error::Io(io::Error::new(
            io::ErrorKind::NotFound,
            format!("missing resource: {url}"),
          ))
        })
    }
  }

  fn snap_viewport(tree: &mut FragmentTree, scroll: Point) -> Point {
    crate::scroll::apply_scroll_snap(tree, &crate::scroll::ScrollState::with_viewport(scroll))
      .state
      .viewport
  }
  use image::codecs::png::PngEncoder;
  use image::load_from_memory;
  use image::ColorType;
  use image::ImageEncoder;
  use image::RgbaImage;

  fn text_color_for(tree: &FragmentTree, needle: &str) -> Option<Rgba> {
    tree.iter_fragments().find_map(|frag| match &frag.content {
      FragmentContent::Text { text, .. } if text.contains(needle) => {
        frag.style.as_ref().map(|s| s.color)
      }
      _ => None,
    })
  }

  fn styled_color_by_id(styled: &StyledNode, id: &str) -> Option<Rgba> {
    if styled.node.get_attribute("id").as_deref() == Some(id) {
      return Some(styled.styles.color);
    }
    styled
      .children
      .iter()
      .find_map(|child| styled_color_by_id(child, id))
  }

  fn find_styled_by_dom_id<'a>(styled: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
    if styled.node.get_attribute("id").as_deref() == Some(id) {
      return Some(styled);
    }

    styled
      .children
      .iter()
      .find_map(|child| find_styled_by_dom_id(child, id))
  }

  fn styled_node_id_by_id(styled: &StyledNode, id: &str) -> Option<usize> {
    if styled.node.get_attribute("id").as_deref() == Some(id) {
      return Some(styled.node_id);
    }
    styled
      .children
      .iter()
      .find_map(|child| styled_node_id_by_id(child, id))
  }

  fn styled_ancestor_ids(styled: &StyledNode, id: &str) -> Vec<usize> {
    fn helper<'a>(node: &'a StyledNode, id: &str, trail: &mut Vec<usize>) -> bool {
      if node.node.get_attribute("id").as_deref() == Some(id) {
        return true;
      }
      for child in &node.children {
        trail.push(node.node_id);
        if helper(child, id, trail) {
          return true;
        }
        trail.pop();
      }
      false
    }

    let mut out = Vec::new();
    helper(styled, id, &mut out);
    out
  }

  #[test]
  fn sticky_without_offsets_does_not_move() {
    let renderer = FastRender::new().unwrap();

    let mut root = FragmentNode::new(
      Rect::from_xywh(0.0, 0.0, 200.0, 200.0),
      FragmentContent::Block { box_id: None },
      vec![],
    );
    let mut sticky_style = ComputedStyle::default();
    sticky_style.position = crate::style::position::Position::Sticky;
    let sticky = FragmentNode::new_with_style(
      Rect::from_xywh(20.0, 30.0, 50.0, 40.0),
      FragmentContent::Block { box_id: None },
      vec![],
      Arc::new(sticky_style),
    );
    root.children.push(sticky);

    renderer.apply_sticky_offsets(
      &mut root,
      Rect::from_xywh(0.0, 0.0, 200.0, 200.0),
      Point::ZERO,
      Size::new(200.0, 200.0),
    );

    let child = &root.children[0];
    assert!((child.bounds.x() - 20.0).abs() < 0.01);
    assert!((child.bounds.y() - 30.0).abs() < 0.01);
  }

  // ===  // Creation Tests (these should always pass)
  // ===
  #[test]
  fn test_fastrender_new() {
    let result = FastRender::new();
    assert!(result.is_ok());
  }

  #[test]
  fn test_fastrender_with_config() {
    let config = FastRenderConfig::new()
      .with_default_background(Rgba::rgb(128, 128, 128))
      .with_default_viewport(1024, 768);

    let result = FastRender::with_config(config);
    assert!(result.is_ok());

    let renderer = result.unwrap();
    assert_eq!(renderer.background_color().r, 128);
    assert_eq!(renderer.background_color().g, 128);
    assert_eq!(renderer.background_color().b, 128);
  }

  #[test]
  fn test_parse_html() {
    let renderer = FastRender::new().unwrap();
    let result = renderer.parse_html("<html><body><div>Hello</div></body></html>");
    assert!(result.is_ok());
  }

  #[test]
  fn parse_html_uses_standard_mode_by_default() {
    let renderer = FastRender::new().unwrap();
    let dom = renderer
      .parse_html("<html class='no-js'><body></body></html>")
      .expect("parse html");

    let html = dom
      .children
      .iter()
      .find(|c| matches!(c.node_type, DomNodeType::Element { .. }))
      .expect("html element child");
    let classes = match &html.node_type {
      DomNodeType::Element { attributes, .. } => attributes
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case("class"))
        .map(|(_, v)| v.split_whitespace().collect::<Vec<_>>())
        .unwrap_or_default(),
      _ => vec![],
    };

    assert!(classes.contains(&"no-js"));
    assert!(!classes.contains(&"js-enabled"));
  }

  #[test]
  fn parse_html_honors_dom_compat_config() {
    let renderer = FastRender::with_config(
      FastRenderConfig::new().with_dom_compat_mode(DomCompatibilityMode::Compatibility),
    )
    .unwrap();
    let dom = renderer
      .parse_html("<html class='no-js'><body></body></html>")
      .expect("parse html");

    let html = dom
      .children
      .iter()
      .find(|c| matches!(c.node_type, DomNodeType::Element { .. }))
      .expect("html element child");
    let classes = match &html.node_type {
      DomNodeType::Element { attributes, .. } => attributes
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case("class"))
        .map(|(_, v)| v.split_whitespace().collect::<Vec<_>>())
        .unwrap_or_default(),
      _ => vec![],
    };

    assert!(!classes.contains(&"no-js"));
    assert!(classes.contains(&"js-enabled"));
    assert!(classes.contains(&"jsl10n-visible"));
  }

  #[test]
  fn test_render_html_invalid_dimensions() {
    let mut renderer = FastRender::new().unwrap();

    // Zero width
    let result = renderer.render_html("<div>Test</div>", 0, 600);
    assert!(result.is_err());

    // Zero height
    let result = renderer.render_html("<div>Test</div>", 800, 0);
    assert!(result.is_err());
  }

  #[test]
  fn render_html_with_scroll_offsets_viewport() {
    let mut renderer = FastRender::new().unwrap();
    let html = r#"
            <style>
                body { margin: 0; }
                .top { height: 50px; background: rgb(255, 0, 0); }
                .bottom { height: 50px; background: rgb(0, 255, 0); }
            </style>
            <div class="top"></div>
            <div class="bottom"></div>
        "#;

    let top = renderer
      .render_html_with_scroll(html, 10, 50, 0.0, 0.0)
      .expect("render at scroll 0");
    let scrolled = renderer
      .render_html_with_scroll(html, 10, 50, 0.0, 50.0)
      .expect("render with scroll offset");

    let pixel = |pixmap: &Pixmap| {
      let data = pixmap.data();
      let a = data[3];
      if a == 0 {
        return (0u8, 0u8, 0u8, 0u8);
      }
      let r = ((data[0] as u16 * 255) / a as u16) as u8;
      let g = ((data[1] as u16 * 255) / a as u16) as u8;
      let b = ((data[2] as u16 * 255) / a as u16) as u8;
      (r, g, b, a)
    };

    assert_eq!(pixel(&top), (255, 0, 0, 255));
    assert_eq!(pixel(&scrolled), (0, 255, 0, 255));
  }

  #[test]
  fn sticky_element_stays_pinned_when_scrolled() {
    let mut renderer = FastRender::new().unwrap();
    let html = r#"
            <style>
                html, body { margin: 0; padding: 0; }
                .sticky { position: sticky; top: 0; height: 10px; background: rgb(255, 0, 0); }
                .content { height: 100px; background: rgb(0, 255, 0); }
            </style>
            <div class="sticky"></div>
            <div class="content"></div>
        "#;

    let top = renderer
      .render_html_with_scroll(html, 20, 20, 0.0, 0.0)
      .expect("render at scroll 0");
    let scrolled = renderer
      .render_html_with_scroll(html, 20, 20, 0.0, 20.0)
      .expect("render with scroll offset");

    let sample = |pixmap: &Pixmap, x: u32, y: u32| {
      let width = pixmap.width();
      let data = pixmap.data();
      let idx = ((y * width + x) * 4) as usize;
      (data[idx], data[idx + 1], data[idx + 2], data[idx + 3])
    };

    assert_eq!(sample(&top, 0, 0), (255, 0, 0, 255));
    assert_eq!(sample(&scrolled, 0, 0), (255, 0, 0, 255));
    assert_eq!(sample(&scrolled, 10, 15), (0, 255, 0, 255));
  }

  #[test]
  fn render_html_with_horizontal_scroll_offsets_viewport() {
    let mut renderer = FastRender::new().unwrap();
    let html = r"
            <style>
                html, body { margin: 0; padding: 0; }
                body {
                    width: 20px;
                    height: 10px;
                    background: linear-gradient(90deg, rgb(255, 0, 0) 0px 10px, rgb(0, 255, 0) 10px 20px);
                    background-repeat: no-repeat;
                    background-size: 20px 10px;
                }
            </style>
            <div></div>
        ";

    // Reference render to sample both halves of the gradient.
    let reference = renderer
      .render_html_with_scroll(html, 20, 10, 0.0, 0.0)
      .expect("reference render");
    let left = renderer
      .render_html_with_scroll(html, 10, 10, 0.0, 0.0)
      .expect("render at scroll 0");
    let scrolled = renderer
      .render_html_with_scroll(html, 10, 10, 10.0, 0.0)
      .expect("render with horizontal scroll offset");

    let sample = |pixmap: &Pixmap, x: u32, y: u32| -> (u8, u8, u8, u8) {
      let width = pixmap.width();
      let data = pixmap.data();
      let idx = ((y * width + x) * 4) as usize;
      let (r, g, b, a) = (data[idx], data[idx + 1], data[idx + 2], data[idx + 3]);
      if a == 0 {
        (0, 0, 0, 0)
      } else {
        let r = ((r as u16 * 255) / a as u16) as u8;
        let g = ((g as u16 * 255) / a as u16) as u8;
        let b = ((b as u16 * 255) / a as u16) as u8;
        (r, g, b, a)
      }
    };

    let ref_left = sample(&reference, 0, 0);
    let ref_right = sample(&reference, 10, 0);

    assert_eq!(sample(&left, 0, 0), ref_left);
    assert_eq!(sample(&scrolled, 0, 0), ref_right);
  }

  #[test]
  fn test_config_builder() {
    let config = FastRenderConfig::new()
      .with_default_background(Rgba::rgb(100, 100, 100))
      .with_default_viewport(1920, 1080);

    assert_eq!(config.background_color.r, 100);
    assert_eq!(config.default_width, 1920);
    assert_eq!(config.default_height, 1080);
    assert!((config.device_pixel_ratio - 1.0).abs() < f32::EPSILON);
  }

  #[test]
  fn test_set_background_color() {
    let mut renderer = FastRender::new().unwrap();
    renderer.set_background_color(Rgba::rgb(50, 50, 50));
    assert_eq!(renderer.background_color().r, 50);
  }

  #[test]
  fn test_font_context_access() {
    let renderer = FastRender::new().unwrap();
    let _font_context = renderer.font_context();
    // Just verify we can access it
  }

  #[test]
  fn test_layout_engine_access() {
    let renderer = FastRender::new().unwrap();
    let _layout_engine = renderer.layout_engine();
    // Just verify we can access it
  }

  #[test]
  fn builder_sets_device_pixel_ratio() {
    let renderer = FastRender::builder()
      .device_pixel_ratio(2.0)
      .build()
      .expect("renderer with custom dpr");
    assert!((renderer.device_pixel_ratio - 2.0).abs() < f32::EPSILON);
  }

  #[test]
  fn setter_updates_device_pixel_ratio() {
    let mut renderer = FastRender::new().unwrap();
    renderer.set_device_pixel_ratio(2.5);
    assert!((renderer.device_pixel_ratio - 2.5).abs() < f32::EPSILON);
  }

  #[test]
  fn render_scales_output_for_device_pixel_ratio() {
    let mut renderer = FastRender::builder()
      .viewport_size(50, 30)
      .device_pixel_ratio(2.0)
      .build()
      .unwrap();
    let png = renderer.render("<div></div>").unwrap();
    let image = load_from_memory(&png).unwrap();
    assert_eq!(image.width(), 100);
    assert_eq!(image.height(), 60);
  }

  #[test]
  fn render_scales_output_for_fractional_device_pixel_ratio() {
    let mut renderer = FastRender::builder()
      .viewport_size(40, 10)
      .device_pixel_ratio(1.5)
      .build()
      .unwrap();
    let png = renderer.render("<div></div>").unwrap();
    let image = load_from_memory(&png).unwrap();
    assert_eq!(image.width(), 60);
    assert_eq!(image.height(), 15);
  }

  #[test]
  fn layout_document_uses_viewport_for_media_queries() {
    let mut renderer = FastRender::new().unwrap();
    let html = r"
            <style>
                @media (max-width: 600px) { body { color: rgb(255, 0, 0); } }
                @media (min-width: 601px) { body { color: rgb(0, 0, 255); } }
            </style>
            <body>hi</body>
        ";
    let dom = renderer.parse_html(html).unwrap();

    let small = renderer.layout_document(&dom, 500, 800).unwrap();
    let small_color = text_color_for(&small, "hi").expect("color for small viewport");
    assert_eq!(small_color, Rgba::rgb(255, 0, 0));

    let large = renderer.layout_document(&dom, 800, 800).unwrap();
    let large_color = text_color_for(&large, "hi").expect("color for large viewport");
    assert_eq!(large_color, Rgba::rgb(0, 0, 255));
  }

  #[test]
  fn layout_document_resolves_file_imports_with_base_url() {
    let dir = tempfile::tempdir().expect("temp dir");
    let import_path = dir.path().join("import.css");
    std::fs::write(&import_path, "body { color: rgb(9, 8, 7); }").expect("write import");

    let html = r#"
            <style>
                @import "import.css";
            </style>
            <body>hello</body>
        "#;
    let base_url = Url::from_file_path(dir.path().join("page.html"))
      .expect("file base url")
      .to_string();

    let mut renderer = FastRender::builder().base_url(base_url).build().unwrap();
    let dom = renderer.parse_html(html).unwrap();
    let styled = renderer.layout_document(&dom, 400, 200).unwrap();
    let color = text_color_for(&styled, "hello").expect("text color");
    assert_eq!(color, Rgba::rgb(9, 8, 7));
  }

  #[test]
  fn layout_document_resolves_data_url_imports() {
    let mut renderer = FastRender::new().unwrap();
    let data_css = "data:text/css,body%7Bcolor:rgb(11,12,13);%7D";
    let html = format!(
      r#"<style>@import url("{}");</style><body>data import</body>"#,
      data_css
    );
    let dom = renderer.parse_html(&html).unwrap();
    let styled = renderer.layout_document(&dom, 320, 200).unwrap();
    let color = text_color_for(&styled, "data").expect("text color");
    assert_eq!(color, Rgba::rgb(11, 12, 13));
  }

  #[test]
  fn render_uses_configured_default_viewport() {
    let mut renderer = FastRender::builder()
      .viewport_size(320, 240)
      .build()
      .unwrap();
    let png = renderer.render("<div></div>").unwrap();
    let image = load_from_memory(&png).unwrap();
    assert_eq!(image.width(), 320);
    assert_eq!(image.height(), 240);
  }

  // ===  // Rendering Tests (may fail if pipeline not fully integrated)
  // These are marked with ignore to match the integration_test.rs pattern
  // ===
  #[test]
  #[ignore = "Full rendering pipeline integration pending"]
  fn test_render_simple_html() {
    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_html("<div>Hello, World!</div>", 100, 100);
    assert!(result.is_ok());

    let pixmap = result.unwrap();
    assert_eq!(pixmap.width(), 100);
    assert_eq!(pixmap.height(), 100);
  }

  #[test]
  #[ignore = "Full rendering pipeline integration pending"]
  fn test_render_with_style() {
    let mut renderer = FastRender::new().unwrap();
    let html = r#"
            <html>
                <head>
                    <style>
                        body { background: white; }
                        .box { width: 50px; height: 50px; background: red; }
                    </style>
                </head>
                <body>
                    <div class="box"></div>
                </body>
            </html>
        "#;

    let result = renderer.render_html(html, 200, 200);
    assert!(result.is_ok());
  }

  #[test]
  #[ignore = "Full rendering pipeline integration pending"]
  fn test_layout_document() {
    let mut renderer = FastRender::new().unwrap();
    let dom = renderer.parse_html("<div>Content</div>").unwrap();
    let result = renderer.layout_document(&dom, 800, 600);
    assert!(result.is_ok());

    let fragment_tree = result.unwrap();
    assert!(fragment_tree.fragment_count() > 0);
  }

  #[test]
  #[ignore = "Full rendering pipeline integration pending"]
  fn test_paint() {
    let mut renderer = FastRender::new().unwrap();
    let dom = renderer.parse_html("<div>Content</div>").unwrap();
    let fragment_tree = renderer.layout_document(&dom, 800, 600).unwrap();
    let result = renderer.paint(&fragment_tree, 800, 600);
    assert!(result.is_ok());
  }

  #[test]
  #[ignore = "Full rendering pipeline integration pending"]
  fn test_render_html_with_background() {
    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_html_with_background(
      "<div>Test</div>",
      100,
      100,
      Rgba::rgb(255, 0, 0), // Red background
    );
    assert!(result.is_ok());
  }

  #[test]
  fn container_query_applies_with_size_container() {
    let mut renderer = FastRender::new().unwrap();
    let html = r#"
            <style>
            .container { width: 600px; container-type: inline-size; }
            .child { color: rgb(1 2 3); }
            @container (width >= 500px) {
              .child { color: rgb(10 20 30); }
            }
            </style>
            <div id="container" class="container"><div id="target" class="child">Target text</div></div>
        "#;
    let dom = renderer.parse_html(html).unwrap();
    let stylesheet = extract_css(&dom).unwrap();
    assert!(
      stylesheet
        .rules
        .iter()
        .any(|r| matches!(r, crate::css::types::CssRule::Container(_))),
      "stylesheet should contain a container rule"
    );
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let collected = stylesheet.collect_style_rules(&media_ctx);
    eprintln!("collected rules={}", collected.len());
    let styled = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None::<&str>,
      None,
      None,
      None,
    );
    let first_container_id = styled_node_id_by_id(&styled, "container").expect("container id");

    let config = LayoutConfig::for_viewport(Size::new(800.0, 600.0));
    renderer.layout_engine = LayoutEngine::with_font_context(config, renderer.font_context.clone());
    let box_gen_options = renderer.box_generation_options();
    let mut box_tree =
      crate::tree::box_generation::generate_box_tree_with_anonymous_fixup_with_options(
        &styled,
        &box_gen_options,
      );
    renderer.resolve_replaced_intrinsic_sizes(&mut box_tree.root, Size::new(800.0, 600.0));
    intrinsic_cache_clear();
    let fragments = renderer.layout_engine.layout_tree(&box_tree).unwrap();

    let cq_ctx = build_container_query_context(&box_tree, &fragments, &media_ctx);
    assert!(!cq_ctx.containers.is_empty(), "expected container context");
    let info = cq_ctx.containers.values().next().unwrap();
    eprintln!(
      "container inline={} block={} type={:?} ids={:?}",
      info.inline_size,
      info.block_size,
      info.container_type,
      cq_ctx.containers.keys().collect::<Vec<_>>()
    );
    let styled_with_containers = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None::<&str>,
      Some(&cq_ctx),
      None,
      None,
    );
    let second_container_id =
      styled_node_id_by_id(&styled_with_containers, "container").expect("container id");
    eprintln!(
      "container ids first={} second={}",
      first_container_id, second_container_id
    );
    let target_id = styled_node_id_by_id(&styled_with_containers, "target").expect("target id");
    let target_ancestors = styled_ancestor_ids(&styled_with_containers, "target");
    let cond = crate::style::media::MediaQuery::parse("(width >= 500px)").unwrap();
    let matches = cq_ctx.matches(
      target_id,
      &target_ancestors,
      &[crate::css::types::ContainerCondition {
        name: None,
        query: cond.clone(),
      }],
    );
    eprintln!("container match result: {}", matches);
    assert!(matches, "container context should match width query");
    let color = styled_color_by_id(&styled_with_containers, "target").expect("styled color");
    assert_eq!(color, Rgba::rgb(10, 20, 30));
  }

  #[test]
  fn container_query_respects_named_container() {
    let mut renderer = FastRender::new().unwrap();
    let html = r#"
            <style>
            .container { width: 400px; container-type: size; container-name: sidebar; }
            .child { color: rgb(5 5 5); }
            @container sidebar (width >= 300px) {
              .child { color: rgb(2 4 6); }
            }
            @container other (width >= 200px) {
              .child { color: rgb(9 9 9); }
            }
            </style>
            <div class="container"><div id="target" class="child">Named container text</div></div>
        "#;
    let dom = renderer.parse_html(html).unwrap();
    let stylesheet = extract_css(&dom).unwrap();
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let styled = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None::<&str>,
      None,
      None,
      None,
    );

    let config = LayoutConfig::for_viewport(Size::new(800.0, 600.0));
    renderer.layout_engine = LayoutEngine::with_font_context(config, renderer.font_context.clone());
    let box_gen_options = renderer.box_generation_options();
    let mut box_tree =
      crate::tree::box_generation::generate_box_tree_with_anonymous_fixup_with_options(
        &styled,
        &box_gen_options,
      );
    renderer.resolve_replaced_intrinsic_sizes(&mut box_tree.root, Size::new(800.0, 600.0));
    intrinsic_cache_clear();
    let fragments = renderer.layout_engine.layout_tree(&box_tree).unwrap();

    let cq_ctx = build_container_query_context(&box_tree, &fragments, &media_ctx);
    assert!(
      !cq_ctx.containers.is_empty(),
      "expected named container context"
    );
    let styled_with_containers = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None::<&str>,
      Some(&cq_ctx),
      None,
      None,
    );
    let color = styled_color_by_id(&styled_with_containers, "target").expect("styled color");
    assert_eq!(color, Rgba::rgb(2, 4, 6));
  }

  #[test]
  fn container_query_inline_and_block_size_features() {
    let mut renderer = FastRender::new().unwrap();
    let html = r#"
            <style>
            .container { width: 550px; height: 180px; container-type: inline-size; }
            .child { color: rgb(1 1 1); }
            @container (inline-size > 500px) {
              .child { color: rgb(11 22 33); }
            }
            @container (block-size > 150px) {
              .child { color: rgb(200 1 1); }
            }
            </style>
            <div class="container"><div id="target" class="child">Inline sized</div></div>
        "#;

    let dom = renderer.parse_html(html).unwrap();
    let stylesheet = extract_css(&dom).unwrap();
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let styled = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None::<&str>,
      None,
      None,
      None,
    );

    let config = LayoutConfig::for_viewport(Size::new(800.0, 600.0));
    renderer.layout_engine = LayoutEngine::with_font_context(config, renderer.font_context.clone());
    let box_gen_options = renderer.box_generation_options();
    let mut box_tree =
      crate::tree::box_generation::generate_box_tree_with_anonymous_fixup_with_options(
        &styled,
        &box_gen_options,
      );
    renderer.resolve_replaced_intrinsic_sizes(&mut box_tree.root, Size::new(800.0, 600.0));
    intrinsic_cache_clear();
    let fragments = renderer.layout_engine.layout_tree(&box_tree).unwrap();

    let cq_ctx = build_container_query_context(&box_tree, &fragments, &media_ctx);
    let styled_with_containers = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None::<&str>,
      Some(&cq_ctx),
      None,
      None,
    );
    let color = styled_color_by_id(&styled_with_containers, "target").expect("styled color");
    assert_eq!(color, Rgba::rgb(11, 22, 33));
  }

  #[test]
  fn container_query_block_size_matches_size_container() {
    let mut renderer = FastRender::new().unwrap();
    let html = r#"
            <style>
            .container { width: 320px; height: 280px; container-type: size; }
            .child { color: rgb(3 3 3); }
            @container (block-size >= 250px) {
              .child { color: rgb(7 8 9); }
            }
            </style>
            <div class="container"><div id="target" class="child">Block sized</div></div>
        "#;

    let dom = renderer.parse_html(html).unwrap();
    let stylesheet = extract_css(&dom).unwrap();
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let styled = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None::<&str>,
      None,
      None,
      None,
    );

    let config = LayoutConfig::for_viewport(Size::new(800.0, 600.0));
    renderer.layout_engine = LayoutEngine::with_font_context(config, renderer.font_context.clone());
    let box_gen_options = renderer.box_generation_options();
    let mut box_tree =
      crate::tree::box_generation::generate_box_tree_with_anonymous_fixup_with_options(
        &styled,
        &box_gen_options,
      );
    renderer.resolve_replaced_intrinsic_sizes(&mut box_tree.root, Size::new(800.0, 600.0));
    intrinsic_cache_clear();
    let fragments = renderer.layout_engine.layout_tree(&box_tree).unwrap();

    let cq_ctx = build_container_query_context(&box_tree, &fragments, &media_ctx);
    let styled_with_containers = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None::<&str>,
      Some(&cq_ctx),
      None,
      None,
    );
    let color = styled_color_by_id(&styled_with_containers, "target").expect("styled color");
    assert_eq!(color, Rgba::rgb(7, 8, 9));
  }

  #[test]
  fn container_query_em_units_use_container_font() {
    let mut renderer = FastRender::new().unwrap();
    let html = r#"
            <style>
            .container { width: 280px; container-type: inline-size; font-size: 20px; }
            .child { color: rgb(1 1 1); }
            @container (min-width: 15em) {
              .child { color: rgb(9 9 9); }
            }
            </style>
            <div class="container"><div id="target" class="child">EM query</div></div>
        "#;

    let dom = renderer.parse_html(html).unwrap();
    let stylesheet = extract_css(&dom).unwrap();
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let styled = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None::<&str>,
      None,
      None,
      None,
    );

    let config = LayoutConfig::for_viewport(Size::new(800.0, 600.0));
    renderer.layout_engine = LayoutEngine::with_font_context(config, renderer.font_context.clone());
    let box_gen_options = renderer.box_generation_options();
    let mut box_tree =
      crate::tree::box_generation::generate_box_tree_with_anonymous_fixup_with_options(
        &styled,
        &box_gen_options,
      );
    renderer.resolve_replaced_intrinsic_sizes(&mut box_tree.root, Size::new(800.0, 600.0));
    intrinsic_cache_clear();
    let fragments = renderer.layout_engine.layout_tree(&box_tree).unwrap();

    let cq_ctx = build_container_query_context(&box_tree, &fragments, &media_ctx);
    let styled_with_containers = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None::<&str>,
      Some(&cq_ctx),
      None,
      None,
    );
    let color = styled_color_by_id(&styled_with_containers, "target").expect("styled color");
    // 15em resolves to 300px with the container's 20px font size; container is 280px wide, so query should not match.
    assert_eq!(color, Rgba::rgb(1, 1, 1));
  }

  #[test]
  fn container_query_non_size_feature_ignored() {
    let mut renderer = FastRender::new().unwrap();
    let html = r#"
            <style>
            .container { width: 400px; container-type: inline-size; }
            .child { color: rgb(3 3 3); }
            @container (color) {
              .child { color: rgb(9 9 9); }
            }
            </style>
            <div class="container"><div id="target" class="child">Ignore color feature</div></div>
        "#;

    let dom = renderer.parse_html(html).unwrap();
    let stylesheet = extract_css(&dom).unwrap();
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let styled = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None::<&str>,
      None,
      None,
      None,
    );

    let config = LayoutConfig::for_viewport(Size::new(800.0, 600.0));
    renderer.layout_engine = LayoutEngine::with_font_context(config, renderer.font_context.clone());
    let box_gen_options = renderer.box_generation_options();
    let mut box_tree =
      crate::tree::box_generation::generate_box_tree_with_anonymous_fixup_with_options(
        &styled,
        &box_gen_options,
      );
    renderer.resolve_replaced_intrinsic_sizes(&mut box_tree.root, Size::new(800.0, 600.0));
    intrinsic_cache_clear();
    let fragments = renderer.layout_engine.layout_tree(&box_tree).unwrap();

    let cq_ctx = build_container_query_context(&box_tree, &fragments, &media_ctx);
    let styled_with_containers = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None::<&str>,
      Some(&cq_ctx),
      None,
      None,
    );
    let color = styled_color_by_id(&styled_with_containers, "target").expect("styled color");
    assert_eq!(color, Rgba::rgb(3, 3, 3));
  }

  #[test]
  fn resolve_intrinsic_sizes_use_alt_text_when_image_missing() {
    let renderer = FastRender::new().expect("init renderer");
    let style = Arc::new(ComputedStyle::default());
    let alt_text = "fallback alt";

    let mut node = BoxNode::new_replaced(
      style.clone(),
      ReplacedType::Image {
        src: String::new(),
        alt: Some(alt_text.to_string()),
        sizes: None,
        srcset: Vec::new(),
        picture_sources: Vec::new(),
      },
      None,
      None,
    );

    renderer.resolve_replaced_intrinsic_sizes(&mut node, Size::new(800.0, 600.0));
    let replaced = match node.box_type {
      BoxType::Replaced(ref r) => r,
      _ => panic!("not replaced"),
    };

    let mut runs = ShapingPipeline::new()
      .shape(alt_text, &style, renderer.font_context())
      .expect("shape alt text");
    TextItem::apply_spacing_to_runs(
      &mut runs,
      alt_text,
      style.letter_spacing,
      style.word_spacing,
    );
    let scaled = {
      let italic = matches!(style.font_style, crate::style::types::FontStyle::Italic);
      let oblique = matches!(style.font_style, crate::style::types::FontStyle::Oblique(_));
      let stretch = FontStretch::from_percentage(style.font_stretch.to_percentage());
      renderer
        .font_context()
        .get_font_full(
          &style.font_family,
          style.font_weight.to_u16(),
          if italic {
            DbFontStyle::Italic
          } else if oblique {
            DbFontStyle::Oblique
          } else {
            DbFontStyle::Normal
          },
          stretch,
        )
        .or_else(|| renderer.font_context().get_sans_serif())
        .and_then(|font| font.metrics().ok())
        .map(|m| m.scale(style.font_size))
    };
    let viewport = Size::new(
      renderer.default_width as f32,
      renderer.default_height as f32,
    );
    let line_height =
      compute_line_height_with_metrics_viewport(&style, scaled.as_ref(), Some(viewport));
    let metrics = TextItem::metrics_from_runs(&runs, line_height, style.font_size);
    let expected = Size::new(runs.iter().map(|r| r.advance).sum(), metrics.height);

    let intrinsic = replaced.intrinsic_size.expect("alt intrinsic size");
    assert!(
      (intrinsic.width - expected.width).abs() < 0.01,
      "alt text width should drive intrinsic width"
    );
    assert!(
      (intrinsic.height - expected.height).abs() < 0.01,
      "alt text height should match line height"
    );
    assert_eq!(
      replaced.aspect_ratio,
      Some(expected.width / expected.height),
      "alt text should set aspect ratio"
    );
  }

  #[test]
  fn resolve_intrinsic_sizes_handles_inline_svg() {
    let renderer = FastRender::new().expect("init renderer");
    let mut node = BoxNode::new_replaced(
      Arc::new(ComputedStyle::default()),
      ReplacedType::Svg {
        content: crate::tree::box_tree::SvgContent::raw(
          r"<svg xmlns='http://www.w3.org/2000/svg' width='20' height='12'></svg>".to_string(),
        ),
      },
      None,
      None,
    );

    renderer.resolve_replaced_intrinsic_sizes(&mut node, Size::new(800.0, 600.0));
    let replaced = match node.box_type {
      BoxType::Replaced(ref r) => r,
      _ => panic!("not replaced"),
    };
    assert_eq!(
      replaced.intrinsic_size,
      Some(Size::new(20.0, 12.0)),
      "inline svg should populate intrinsic size"
    );
    assert_eq!(
      replaced.aspect_ratio,
      Some(20.0 / 12.0),
      "inline svg should populate aspect ratio"
    );
  }

  #[test]
  fn resolve_intrinsic_sizes_respects_preserve_aspect_ratio_none() {
    let renderer = FastRender::new().expect("init renderer");
    let mut node = BoxNode::new_replaced(
      Arc::new(ComputedStyle::default()),
      ReplacedType::Svg {
                content: crate::tree::box_tree::SvgContent::raw(
                  r"<svg xmlns='http://www.w3.org/2000/svg' width='200' height='100' viewBox='0 0 50 100' preserveAspectRatio='none'></svg>"
                    .to_string(),
                ),
      },
      None,
      None,
        );

    renderer.resolve_replaced_intrinsic_sizes(&mut node, Size::new(800.0, 600.0));
    let replaced = match node.box_type {
      BoxType::Replaced(ref r) => r,
      _ => panic!("not replaced"),
    };

    assert_eq!(
      replaced.intrinsic_size,
      Some(Size::new(200.0, 100.0)),
      "inline svg should still populate intrinsic size"
    );
    assert_eq!(
      replaced.aspect_ratio, None,
      "preserveAspectRatio='none' should drop intrinsic ratio"
    );
  }

  #[test]
  fn resolve_intrinsic_sizes_use_viewbox_ratio() {
    let renderer = FastRender::new().expect("init renderer");
    let mut node = BoxNode::new_replaced(
      Arc::new(ComputedStyle::default()),
      ReplacedType::Svg {
        content: crate::tree::box_tree::SvgContent::raw(
          r"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 50 100'></svg>".to_string(),
        ),
      },
      None,
      None,
    );

    renderer.resolve_replaced_intrinsic_sizes(&mut node, Size::new(800.0, 600.0));
    let replaced = match node.box_type {
      BoxType::Replaced(ref r) => r,
      _ => panic!("not replaced"),
    };

    assert_eq!(replaced.aspect_ratio, Some(0.5));
  }

  #[test]
  fn resolve_intrinsic_sizes_ignore_stroke_bounds() {
    let renderer = FastRender::new().expect("init renderer");
    let mut node = BoxNode::new_replaced(
      Arc::new(ComputedStyle::default()),
      ReplacedType::Svg {
                content: crate::tree::box_tree::SvgContent::raw(r"
                    <svg xmlns='http://www.w3.org/2000/svg' width='20' height='10' viewBox='0 0 20 10'>
                        <rect x='0' y='0' width='20' height='10' stroke='black' stroke-width='50' fill='none'/>
                    </svg>
                "
                .to_string()),
      },
      None,
      None,
        );

    renderer.resolve_replaced_intrinsic_sizes(&mut node, Size::new(800.0, 600.0));
    let replaced = match node.box_type {
      BoxType::Replaced(ref r) => r,
      _ => panic!("not replaced"),
    };

    assert_eq!(
      replaced.intrinsic_size,
      Some(Size::new(20.0, 10.0)),
      "stroke should not inflate SVG intrinsic size"
    );
    assert_eq!(replaced.aspect_ratio, Some(2.0));
  }

  #[test]
  fn image_resolution_scales_intrinsic_size() {
    let renderer = FastRender::new().expect("init renderer");

    let mut pixels = RgbaImage::new(4, 2);
    for p in pixels.pixels_mut() {
      *p = image::Rgba([255, 0, 0, 255]);
    }
    let mut buf = Vec::new();
    PngEncoder::new(&mut buf)
      .write_image(pixels.as_raw(), 4, 2, ColorType::Rgba8.into())
      .expect("encode png");
    let data_url = format!(
      "data:image/png;base64,{}",
      base64::engine::general_purpose::STANDARD.encode(&buf)
    );

    let mut style = ComputedStyle::default();
    style.image_resolution = ImageResolution {
      from_image: false,
      specified: Some(2.0),
      snap: false,
    };

    let mut node = BoxNode::new_replaced(
      Arc::new(style),
      ReplacedType::Image {
        src: data_url,
        alt: None,
        sizes: None,
        srcset: Vec::new(),
        picture_sources: Vec::new(),
      },
      None,
      None,
    );

    renderer.resolve_replaced_intrinsic_sizes(&mut node, Size::new(800.0, 600.0));

    let replaced = match node.box_type {
      BoxType::Replaced(ref r) => r,
      _ => panic!("not replaced"),
    };

    let intrinsic = replaced.intrinsic_size.expect("intrinsic size");
    assert!((intrinsic.width - 2.0).abs() < 1e-6);
    assert!((intrinsic.height - 1.0).abs() < 1e-6);
    assert_eq!(replaced.aspect_ratio, Some(2.0));
  }

  #[test]
  fn srcset_density_scales_intrinsic_size() {
    let renderer = FastRender::new().expect("init renderer");

    let mut pixels = image::RgbaImage::new(4, 2);
    for p in pixels.pixels_mut() {
      *p = image::Rgba([255, 0, 0, 255]);
    }
    let mut buf = Vec::new();
    image::codecs::png::PngEncoder::new(&mut buf)
      .write_image(pixels.as_raw(), 4, 2, image::ColorType::Rgba8.into())
      .expect("encode png");
    let data_url = format!(
      "data:image/png;base64,{}",
      base64::engine::general_purpose::STANDARD.encode(&buf)
    );

    let style = ComputedStyle::default();
    let mut node = BoxNode::new_replaced(
      Arc::new(style),
      ReplacedType::Image {
        src: data_url.clone(),
        alt: None,
        sizes: None,
        srcset: vec![SrcsetCandidate {
          url: data_url.clone(),
          descriptor: SrcsetDescriptor::Density(2.0),
        }],
        picture_sources: Vec::new(),
      },
      None,
      None,
    );

    renderer.resolve_replaced_intrinsic_sizes(&mut node, Size::new(800.0, 600.0));

    let replaced = match node.box_type {
      BoxType::Replaced(ref r) => r,
      _ => panic!("not replaced"),
    };

    let intrinsic = replaced.intrinsic_size.expect("intrinsic size");
    assert!((intrinsic.width - 2.0).abs() < 1e-6);
    assert!((intrinsic.height - 1.0).abs() < 1e-6);
    assert_eq!(replaced.aspect_ratio, Some(2.0));
  }

  #[test]
  fn resolve_intrinsic_sizes_use_video_poster() {
    let renderer = FastRender::new().expect("init renderer");
    let poster = "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='4'></svg>";
    let mut node = BoxNode::new_replaced(
      Arc::new(ComputedStyle::default()),
      ReplacedType::Video {
        src: String::new(),
        poster: Some(poster.to_string()),
      },
      None,
      None,
    );

    renderer.resolve_replaced_intrinsic_sizes(&mut node, Size::new(800.0, 600.0));
    let replaced = match node.box_type {
      BoxType::Replaced(ref r) => r,
      _ => panic!("not replaced"),
    };
    assert_eq!(
      replaced.intrinsic_size,
      Some(Size::new(8.0, 4.0)),
      "poster resource should set intrinsic size"
    );
    assert_eq!(
      replaced.aspect_ratio,
      Some(2.0),
      "poster resource should set aspect ratio"
    );
  }

  #[test]
  fn resolve_intrinsic_sizes_handles_embed_and_object_svg() {
    let renderer = FastRender::new().expect("init renderer");
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='16' height='10'></svg>";

    let mut embed_node = BoxNode::new_replaced(
      Arc::new(ComputedStyle::default()),
      ReplacedType::Embed {
        src: svg.to_string(),
      },
      None,
      None,
    );
    renderer.resolve_replaced_intrinsic_sizes(&mut embed_node, Size::new(800.0, 600.0));
    let embed_replaced = match embed_node.box_type {
      BoxType::Replaced(ref r) => r,
      _ => panic!("not replaced"),
    };
    assert_eq!(
      embed_replaced.intrinsic_size,
      Some(Size::new(16.0, 10.0)),
      "embed should pick intrinsic size from SVG"
    );
    assert_eq!(
      embed_replaced.aspect_ratio,
      Some(16.0 / 10.0),
      "embed should set aspect ratio from SVG"
    );

    let mut object_node = BoxNode::new_replaced(
      Arc::new(ComputedStyle::default()),
      ReplacedType::Object {
        data: svg.to_string(),
      },
      None,
      None,
    );
    renderer.resolve_replaced_intrinsic_sizes(&mut object_node, Size::new(800.0, 600.0));
    let object_replaced = match object_node.box_type {
      BoxType::Replaced(ref r) => r,
      _ => panic!("not replaced"),
    };
    assert_eq!(
      object_replaced.intrinsic_size,
      Some(Size::new(16.0, 10.0)),
      "object should pick intrinsic size from SVG"
    );
    assert_eq!(
      object_replaced.aspect_ratio,
      Some(16.0 / 10.0),
      "object should set aspect ratio from SVG"
    );
  }

  #[test]
  fn scroll_snap_changes_fingerprint() {
    let a = ComputedStyle::default();
    let mut b = a.clone();

    b.scroll_snap_type.axis = crate::style::types::ScrollSnapAxis::X;
    b.scroll_snap_type.strictness = crate::style::types::ScrollSnapStrictness::Mandatory;
    b.scroll_snap_align.inline = crate::style::types::ScrollSnapAlign::Center;
    b.scroll_snap_stop = crate::style::types::ScrollSnapStop::Always;

    let hash_a = style_layout_fingerprint(&a);
    let hash_b = style_layout_fingerprint(&b);

    assert_ne!(
      hash_a, hash_b,
      "scroll snap settings must influence layout fingerprints"
    );
  }

  #[test]
  fn scroll_snap_mandatory_adjusts_offsets() {
    let mut renderer = FastRender::new().unwrap();
    let html = r"
            <style>
                html, body { margin: 0; height: 100%; scroll-snap-type: y mandatory; }
                section { height: 200px; scroll-snap-align: start; }
            </style>
            <section></section>
            <section></section>
        ";

    let dom = renderer.parse_html(html).unwrap();
    let mut fragments = renderer.layout_document(&dom, 100, 100).unwrap();

    let snapped = snap_viewport(&mut fragments, Point::new(0.0, 120.0));
    assert!(
      (snapped.y - 200.0).abs() < 0.1,
      "expected snap to the second section"
    );
    assert!(snapped.x.abs() < 0.1);
  }

  #[test]
  fn scroll_snap_none_leaves_offsets_unchanged() {
    let mut renderer = FastRender::new().unwrap();
    let html = r"
            <style>
                html, body { margin: 0; height: 100%; scroll-snap-type: none; }
                section { height: 200px; scroll-snap-align: start; }
            </style>
            <section></section>
            <section></section>
        ";

    let dom = renderer.parse_html(html).unwrap();
    let mut fragments = renderer.layout_document(&dom, 100, 100).unwrap();

    let snapped = snap_viewport(&mut fragments, Point::new(0.0, 120.0));
    assert!(
      (snapped.y - 120.0).abs() < 0.1,
      "scroll-snap-type:none should not adjust scroll offsets"
    );
    assert!(snapped.x.abs() < 0.1);
  }

  #[test]
  fn scroll_snap_proximity_only_when_close() {
    let mut renderer = FastRender::new().unwrap();
    let html = r"
            <style>
                html, body { margin: 0; height: 100%; scroll-snap-type: y proximity; }
                section { height: 200px; scroll-snap-align: start; }
            </style>
            <section></section>
            <section></section>
        ";

    let dom = renderer.parse_html(html).unwrap();
    let mut fragments = renderer.layout_document(&dom, 100, 100).unwrap();

    let near = snap_viewport(&mut fragments, Point::new(0.0, 30.0));
    assert!(
      near.y.abs() < 0.1,
      "nearby offset should snap to the first section"
    );

    let far = snap_viewport(&mut fragments, Point::new(0.0, 260.0));
    assert!(
      (far.y - 260.0).abs() < 0.1,
      "far offset should remain unchanged for proximity snapping"
    );
  }

  #[test]
  fn scroll_snap_inline_axis_centers_items() {
    // Manually construct a scroll snap container with a single wide item centered along the x-axis.
    let mut container_style = ComputedStyle::default();
    container_style.scroll_snap_type.axis = ScrollSnapAxis::X;
    container_style.scroll_snap_type.strictness = ScrollSnapStrictness::Mandatory;

    let mut item_style = ComputedStyle::default();
    item_style.scroll_snap_align.inline = ScrollSnapAlign::Center;

    let mut container = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 300.0, 100.0),
      vec![],
      Arc::new(container_style),
    );
    let child = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 200.0, 100.0),
      vec![],
      Arc::new(item_style),
    );
    container.children.push(child);

    let mut tree = FragmentTree::with_viewport(container, Size::new(100.0, 100.0));

    // Scrolling partway into the item should snap to its center at x=50 (200/2 - 100/2).
    let snapped = snap_viewport(&mut tree, Point::new(60.0, 0.0));
    assert!(
      (snapped.x - 50.0).abs() < 0.1,
      "expected snap to the centered first item, got {}",
      snapped.x
    );
    assert!(snapped.y.abs() < 0.1);
  }

  #[test]
  fn scroll_padding_insets_snapport_alignment() {
    let mut renderer = FastRender::new().unwrap();
    let html = r"
            <style>
                html, body {
                    margin: 0;
                    height: 100%;
                    scroll-snap-type: y mandatory;
                    scroll-padding-top: 40px;
                }
                section { height: 100px; scroll-snap-align: start; }
            </style>
            <section></section>
            <section></section>
        ";

    let dom = renderer.parse_html(html).unwrap();
    let mut fragments = renderer.layout_document(&dom, 100, 100).unwrap();
    fragments.ensure_scroll_metadata();
    let metadata = fragments
      .scroll_metadata
      .as_ref()
      .and_then(|m| m.containers.first())
      .expect("snap container");
    assert!(
      (metadata.padding_y.0 - 40.0).abs() < 0.1,
      "scroll-padding should parse"
    );

    let snapped = snap_viewport(&mut fragments, Point::new(0.0, 120.0));
    assert!(
      (snapped.y - 60.0).abs() < 0.1,
      "scroll-padding should inset snap positions (snapped={:?})",
      snapped
    );
  }

  #[test]
  fn scroll_margin_shifts_snap_targets() {
    let mut renderer = FastRender::new().unwrap();
    let html = r"
            <style>
                html, body { margin: 0; height: 100%; scroll-snap-type: y mandatory; }
                section { height: 120px; scroll-snap-align: start; }
                section + section { scroll-margin-top: 30px; }
            </style>
            <section></section>
            <section></section>
        ";

    let dom = renderer.parse_html(html).unwrap();
    let mut fragments = renderer.layout_document(&dom, 100, 100).unwrap();

    let snapped = snap_viewport(&mut fragments, Point::new(0.0, 130.0));
    assert!(
      (snapped.y - 90.0).abs() < 0.1,
      "scroll-margin should shift snap targets (snapped={:?})",
      snapped
    );
  }

  #[test]
  fn scroll_snap_horizontal_mandatory_adjusts_offsets() {
    let mut renderer = FastRender::new().unwrap();
    let html = r"
            <style>
                html, body {
                    margin: 0;
                    height: 100%;
                    width: 100%;
                    display: grid;
                    grid-template-columns: 200px 200px;
                    grid-auto-flow: column;
                    scroll-snap-type: x mandatory;
                }
                section {
                    height: 100px;
                    scroll-snap-align: start;
                }
            </style>
            <section></section><section></section>
        ";

    let dom = renderer.parse_html(html).unwrap();
    let mut fragments = renderer.layout_document(&dom, 100, 100).unwrap();

    let snapped = snap_viewport(&mut fragments, Point::new(120.0, 0.0));
    assert!(
            (snapped.x - 200.0).abs() < 0.1,
            "expected snap to the second section on the inline axis (snapped={:?}, xs={:?}, content={:?})",
            snapped,
            fragments.root.children.iter().map(|c| c.bounds.x()).collect::<Vec<_>>(),
            fragments.content_size()
        );
    assert!(snapped.y.abs() < 0.1);
  }

  #[test]
  fn scroll_snap_inline_axis_matches_horizontal_flow() {
    let mut renderer = FastRender::new().unwrap();
    let html = r"
            <style>
                html, body {
                    margin: 0;
                    height: 100%;
                    width: 100%;
                    display: grid;
                    grid-template-columns: 200px 200px;
                    grid-auto-flow: column;
                    scroll-snap-type: inline mandatory;
                }
                section {
                    height: 100px;
                    scroll-snap-align: start;
                }
            </style>
            <section></section><section></section>
        ";

    let dom = renderer.parse_html(html).unwrap();
    let mut fragments = renderer.layout_document(&dom, 100, 100).unwrap();

    let snapped = snap_viewport(&mut fragments, Point::new(120.0, 0.0));
    assert!(
      (snapped.x - 200.0).abs() < 0.1,
      "inline axis snapping should match the horizontal flow (snapped={:?}, content={:?})",
      snapped,
      fragments.content_size()
    );
    assert!(snapped.y.abs() < 0.1);
  }

  #[test]
  fn scroll_snap_block_axis_vertical_writing_mode_snaps_horizontal() {
    let mut container_style = ComputedStyle::default();
    container_style.scroll_snap_type.axis = ScrollSnapAxis::Block;
    container_style.scroll_snap_type.strictness = ScrollSnapStrictness::Mandatory;
    container_style.writing_mode = WritingMode::VerticalRl;

    let mut child_style = ComputedStyle::default();
    child_style.scroll_snap_align.inline = ScrollSnapAlign::Start;
    child_style.scroll_snap_align.block = ScrollSnapAlign::Start;

    let child_a = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![],
      Arc::new(child_style.clone()),
    );
    let child_b = FragmentNode::new_block_styled(
      Rect::from_xywh(200.0, 0.0, 100.0, 100.0),
      vec![],
      Arc::new(child_style),
    );

    let container = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 400.0, 200.0),
      vec![child_a, child_b],
      Arc::new(container_style),
    );
    let mut fragments = FragmentTree::with_viewport(container, Size::new(100.0, 100.0));

    let snapped = snap_viewport(&mut fragments, Point::new(120.0, 0.0));
    assert!(
      (snapped.x - 200.0).abs() < 0.1,
      "block axis should snap horizontally in vertical writing modes (snapped={:?})",
      snapped
    );
    assert!(snapped.y.abs() < 0.1);
  }

  #[test]
  fn scroll_snap_inline_axis_vertical_writing_mode_snaps_vertical() {
    let mut container_style = ComputedStyle::default();
    container_style.scroll_snap_type.axis = ScrollSnapAxis::Inline;
    container_style.scroll_snap_type.strictness = ScrollSnapStrictness::Mandatory;
    container_style.writing_mode = WritingMode::VerticalRl;

    let mut child_style = ComputedStyle::default();
    child_style.scroll_snap_align.inline = ScrollSnapAlign::Start;
    child_style.scroll_snap_align.block = ScrollSnapAlign::Start;

    let child_a = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![],
      Arc::new(child_style.clone()),
    );
    let child_b = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 200.0, 100.0, 100.0),
      vec![],
      Arc::new(child_style),
    );

    let container = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 200.0, 400.0),
      vec![child_a, child_b],
      Arc::new(container_style),
    );
    let mut fragments = FragmentTree::with_viewport(container, Size::new(100.0, 100.0));

    let snapped = snap_viewport(&mut fragments, Point::new(0.0, 120.0));
    assert!(
      (snapped.y - 200.0).abs() < 0.1,
      "inline axis should snap vertically in vertical writing modes (snapped={:?})",
      snapped
    );
    assert!(snapped.x.abs() < 0.1);
  }

  #[test]
  fn external_stylesheets_respect_media_queries() {
    let base_url = "https://example.com/page.html";
    let fetcher = MapFetcher::default()
      .with_entry(
        "https://example.com/screen.css",
        "body { color: rgb(1, 2, 3); }",
      )
      .with_entry(
        "https://example.com/print.css",
        "body { color: rgb(10, 20, 30); }",
      );

    let mut renderer = FastRender::builder()
      .base_url(base_url.to_string())
      .fetcher(Arc::new(fetcher) as Arc<dyn ResourceFetcher>)
      .build()
      .unwrap();

    let html = r#"
      <link rel="stylesheet" href="screen.css" media="screen">
      <link rel="stylesheet" href="print.css" media="print">
      <div>Hello media</div>
    "#;
    let dom = renderer.parse_html(html).unwrap();
    let tree = renderer.layout_document(&dom, 200, 200).unwrap();

    let color = text_color_for(&tree, "Hello media").unwrap();
    assert_eq!(color, Rgba::rgb(1, 2, 3));
  }

  #[test]
  fn external_stylesheets_follow_document_order() {
    let base_url = "https://example.com/page.html";
    let fetcher = MapFetcher::default().with_entry(
      "https://example.com/a.css",
      "body { color: rgb(20, 40, 60); }",
    );

    let mut renderer = FastRender::builder()
      .base_url(base_url.to_string())
      .fetcher(Arc::new(fetcher) as Arc<dyn ResourceFetcher>)
      .build()
      .unwrap();

    let html = r#"
      <style>body { color: rgb(200, 0, 0); }</style>
      <link rel="stylesheet" href="a.css">
      <div>Order Test</div>
    "#;
    let dom = renderer.parse_html(html).unwrap();
    let tree = renderer.layout_document(&dom, 200, 200).unwrap();

    let color = text_color_for(&tree, "Order Test").unwrap();
    assert_eq!(color, Rgba::rgb(20, 40, 60));
  }

  #[test]
  fn stylesheet_urls_resolve_against_stylesheet_base() {
    let base_url = "https://example.com/app/page.html";
    let fetcher = MapFetcher::default().with_entry(
      "https://example.com/app/styles/main.css",
      "#target { background-image: url(\"../images/bg.png\"); }",
    );

    let renderer = FastRender::builder()
      .base_url(base_url.to_string())
      .fetcher(Arc::new(fetcher) as Arc<dyn ResourceFetcher>)
      .build()
      .unwrap();

    let html = r#"
      <link rel="stylesheet" href="styles/main.css">
      <div id="target">bg</div>
    "#;

    let mut dom = renderer.parse_html(html).unwrap();
    let modal_open = modal_dialog_present(&dom);
    apply_top_layer_state(&mut dom, modal_open, false);

    let media_ctx = MediaContext::screen(200.0, 200.0)
      .with_device_pixel_ratio(renderer.device_pixel_ratio)
      .with_env_overrides();
    let mut media_query_cache = MediaQueryCache::default();
    let stylesheet = renderer.collect_document_stylesheet(&dom, &media_ctx, &mut media_query_cache);
    let styled = apply_styles_with_media_target_and_imports_cached(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None,
      None,
      None,
      None,
      Some(&mut media_query_cache),
    );

    let target = find_styled_by_dom_id(&styled, "target").expect("styled node");
    let image = target
      .styles
      .background_layers
      .first()
      .and_then(|layer| layer.image.clone());
    let url = match image {
      Some(BackgroundImage::Url(url)) => url,
      other => panic!("unexpected background image: {:?}", other),
    };

    assert_eq!(url, "https://example.com/app/images/bg.png");
  }

  #[test]
  fn scroll_snap_stop_always_breaks_ties() {
    let mut container_style = ComputedStyle::default();
    container_style.scroll_snap_type.axis = ScrollSnapAxis::X;
    container_style.scroll_snap_type.strictness = ScrollSnapStrictness::Mandatory;

    let mut first = ComputedStyle::default();
    first.scroll_snap_align.inline = ScrollSnapAlign::Start;
    first.scroll_snap_stop = ScrollSnapStop::Normal;

    let mut second = ComputedStyle::default();
    second.scroll_snap_align.inline = ScrollSnapAlign::Start;
    second.scroll_snap_stop = ScrollSnapStop::Always;

    let child_a = FragmentNode::new_block_styled(
      Rect::from_xywh(100.0, 0.0, 50.0, 50.0),
      vec![],
      Arc::new(first),
    );
    let child_b = FragmentNode::new_block_styled(
      Rect::from_xywh(200.0, 0.0, 50.0, 50.0),
      vec![],
      Arc::new(second),
    );

    let container = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 400.0, 60.0),
      vec![child_a, child_b],
      Arc::new(container_style),
    );
    let mut tree = FragmentTree::with_viewport(container, Size::new(100.0, 60.0));
    let snapped = snap_viewport(&mut tree, Point::new(150.0, 0.0));

    assert!(
      (snapped.x - 200.0).abs() < 0.1,
      "stop:always targets should win ties at equal distance"
    );
  }
}
