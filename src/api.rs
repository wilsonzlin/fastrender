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
//! caches that are not thread-safe. For multi-threaded use, create one
//! instance per thread.

use crate::css::encoding::decode_css_bytes;
use crate::css::parser::extract_css;
use crate::css::types::CssImportLoader;
use crate::dom::{self, DomNode};
use crate::error::{Error, RenderError, Result};
use crate::geometry::{Point, Rect, Size};
use crate::image_loader::ImageCache;
use crate::image_output::{encode_image, OutputFormat};
use crate::layout::contexts::inline::baseline::compute_line_height_with_metrics_viewport;
use crate::layout::contexts::inline::line_builder::TextItem;
use crate::layout::engine::{LayoutConfig, LayoutEngine};
use crate::layout::flex_profile::{flex_profile_enabled, log_flex_profile, reset_flex_profile};
use crate::layout::formatting_context::{intrinsic_cache_clear, intrinsic_cache_reset_counters, intrinsic_cache_stats};
use crate::layout::profile::{layout_profile_enabled, log_layout_profile, reset_layout_profile};
use crate::paint::painter::{paint_tree_with_resources_scaled, paint_tree_with_resources_scaled_offset};
use crate::resource::{HttpFetcher, ResourceFetcher};
use crate::style::cascade::{
    apply_styles_with_media_target_and_imports, apply_styles_with_media_target_and_imports_cached,
    ContainerQueryContext, ContainerQueryInfo, StyledNode,
};
use crate::style::color::Rgba;
use crate::style::media::{MediaContext, MediaQueryCache};
use crate::style::types::{
    ContainerType, ScrollSnapAlign, ScrollSnapAxis, ScrollSnapStop, ScrollSnapStrictness, WritingMode,
};
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::text::font_db::{FontStretch, FontStyle as DbFontStyle, ScaledMetrics};
use crate::text::font_loader::FontContext;
use crate::text::pipeline::ShapingPipeline;
use crate::tree::box_tree::{BoxNode, BoxTree, BoxType, MarkerContent, ReplacedBox, ReplacedType};
#[cfg(test)]
use crate::tree::box_tree::{SrcsetCandidate, SrcsetDescriptor};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::io;
use std::mem;
use std::path::Path;
use std::sync::{Arc, OnceLock};
use std::time::Instant;
use url::Url;

// Re-export Pixmap from tiny-skia for public use
pub use tiny_skia::Pixmap;

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

    /// Default background color for rendering
    background_color: Rgba,

    /// Default viewport width for render()
    default_width: u32,

    /// Default viewport height for render()
    default_height: u32,

    /// Device pixel ratio used for media queries and resolution-dependent resources
    device_pixel_ratio: f32,

    /// Base URL used for resolving links/targets
    base_url: Option<String>,
}

impl std::fmt::Debug for FastRender {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FastRender")
            .field("background_color", &self.background_color)
            .field("default_width", &self.default_width)
            .field("default_height", &self.default_height)
            .field("device_pixel_ratio", &self.device_pixel_ratio)
            .field("base_url", &self.base_url)
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
}

impl Default for FastRenderConfig {
    fn default() -> Self {
        Self {
            background_color: Rgba::WHITE,
            default_width: 800,
            default_height: 600,
            device_pixel_ratio: 1.0,
            base_url: None,
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

    /// Sets the device pixel ratio used for media queries and image-set selection.
    pub fn device_pixel_ratio(mut self, dpr: f32) -> Self {
        self.config.device_pixel_ratio = dpr;
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
}

impl Default for FastRenderBuilder {
    fn default() -> Self {
        Self::new()
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
}

impl FastRender {
    fn resolve_scaled_metrics(&self, style: &ComputedStyle) -> Option<ScaledMetrics> {
        let italic = matches!(style.font_style, crate::style::types::FontStyle::Italic);
        let oblique = matches!(style.font_style, crate::style::types::FontStyle::Oblique(_));
        let stretch = FontStretch::from_percentage(style.font_stretch.to_percentage());

        self.font_context
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
        let fetcher = fetcher.unwrap_or_else(|| Arc::new(HttpFetcher::new()));
        let font_context = FontContext::new();
        let layout_config =
            LayoutConfig::for_viewport(Size::new(config.default_width as f32, config.default_height as f32))
                .with_identifier("api");
        let layout_engine = LayoutEngine::with_font_context(layout_config, font_context.clone());
        let image_cache = match &config.base_url {
            Some(url) => ImageCache::with_base_url_and_fetcher(url.clone(), Arc::clone(&fetcher)),
            None => ImageCache::with_fetcher(Arc::clone(&fetcher)),
        };

        Ok(Self {
            font_context,
            layout_engine,
            image_cache,
            fetcher,
            background_color: config.background_color,
            default_width: config.default_width,
            default_height: config.default_height,
            device_pixel_ratio: config.device_pixel_ratio,
            base_url: config.base_url.clone(),
        })
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
        self.render_html_internal(html, width, height, 0.0, 0.0)
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
        self.render_html_internal(html, width, height, scroll_x, scroll_y)
    }

    fn render_html_internal(
        &mut self,
        html: &str,
        width: u32,
        height: u32,
        scroll_x: f32,
        scroll_y: f32,
    ) -> Result<Pixmap> {
        // Validate dimensions
        if width == 0 || height == 0 {
            return Err(Error::Render(RenderError::InvalidParameters {
                message: format!("Invalid dimensions: width={}, height={}", width, height),
            }));
        }

        let timings_enabled = std::env::var_os("FASTR_RENDER_TIMINGS").is_some();
        let mut stage_start = timings_enabled.then(Instant::now);
        let overall_start = stage_start.clone();

        // Parse HTML to DOM
        let dom = self.parse_html(html)?;

        if let Some(start) = stage_start.as_mut() {
            let now = Instant::now();
            eprintln!("timing:parse {:?}", now - *start);
            *start = now;
        }

        // Layout the document
        let fragment_tree = self.layout_document(&dom, width, height)?;

        if std::env::var("FASTR_LOG_FRAG_BOUNDS")
            .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
            .unwrap_or(false)
        {
            let bbox = fragment_tree.content_size();
            eprintln!(
                "[frag-bounds] viewport=({}x{}) bbox=({:.1},{:.1})→({:.1},{:.1}) size=({:.1}x{:.1}) fragments={}",
                width,
                height,
                bbox.min_x(),
                bbox.min_y(),
                bbox.max_x(),
                bbox.max_y(),
                bbox.width(),
                bbox.height(),
                fragment_tree.fragment_count()
            );
        }

        if let Ok(v) = std::env::var("FASTR_DUMP_TEXT_FRAGMENTS") {
            if v != "0" && !v.eq_ignore_ascii_case("false") {
                let limit: usize = v.parse().unwrap_or(20);
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
                    crate::tree::fragment_tree::FragmentContent::Inline { box_id, fragment_index } => {
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
            trace_text(&fragment_tree.root, crate::geometry::Point::ZERO, &query, &mut trail);
        }

        if let Some(start) = stage_start.as_mut() {
            let now = Instant::now();
            eprintln!("timing:layout_document {:?}", now - *start);
            *start = now;
        }

        let expand_full_page = std::env::var("FASTR_FULL_PAGE")
            .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
            .unwrap_or(false);
        let viewport_size = Size::new(width as f32, height as f32);
        let scroll = apply_scroll_snap(&fragment_tree, viewport_size, Point::new(scroll_x, scroll_y));

        let (target_width, target_height) = if expand_full_page {
            let content_bounds = fragment_tree.content_size();
            let w = width.max(content_bounds.max_x().ceil().max(1.0) as u32);
            let h = height.max(content_bounds.max_y().ceil().max(1.0) as u32);
            (w, h)
        } else {
            (width, height)
        };

        // Paint to pixmap
        let offset = Point::new(-scroll.x, -scroll.y);
        let pixmap = self.paint_with_offset(&fragment_tree, target_width, target_height, offset)?;

        if let Some(start) = stage_start {
            let now = Instant::now();
            eprintln!("timing:paint {:?}", now - start);
            if let Some(overall) = overall_start {
                eprintln!("timing:render_html_total {:?}", now - overall);
            }
        }

        Ok(pixmap)
    }

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
        dom::parse_html(html)
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
    pub fn layout_document(&mut self, dom: &DomNode, width: u32, height: u32) -> Result<FragmentTree> {
        // Extract CSS from style tags
        let stylesheet = extract_css(dom)?;

        let timings_enabled = std::env::var_os("FASTR_RENDER_TIMINGS").is_some();
        let mut stage_start = timings_enabled.then(Instant::now);
        let overall_start = stage_start.clone();

        // Apply styles to create styled tree
        let target_fragment = self.current_target_fragment();
        let media_ctx = MediaContext::screen(width as f32, height as f32)
            .with_device_pixel_ratio(self.device_pixel_ratio)
            .with_env_overrides();
        let import_loader = CssImportFetcher::new(self.base_url.clone(), Arc::clone(&self.fetcher));
        let mut media_query_cache = MediaQueryCache::default();
        let resolved_stylesheet = stylesheet.resolve_imports_with_cache(
            &import_loader,
            self.base_url.as_deref(),
            &media_ctx,
            Some(&mut media_query_cache),
        );
        let has_container_queries = resolved_stylesheet.has_container_rules();
        self.font_context.clear_web_fonts();
        let font_faces =
            resolved_stylesheet.collect_font_face_rules_with_cache(&media_ctx, Some(&mut media_query_cache));
        // Best-effort loading; rendering should continue even if a web font fails.
        let _ = self.font_context.load_web_fonts(&font_faces, self.base_url.as_deref());
        let styled_tree = apply_styles_with_media_target_and_imports_cached(
            dom,
            &resolved_stylesheet,
            &media_ctx,
            target_fragment.as_deref(),
            None,
            None,
            None,
            None,
            None,
            Some(&mut media_query_cache),
        );
        let first_style_fingerprints = has_container_queries.then(|| styled_fingerprint_map(&styled_tree));

        if let Some(start) = stage_start.as_mut() {
            let now = Instant::now();
            eprintln!("timing:cascade {:?}", now - *start);
            *start = now;
        }

        // Generate box tree
        let mut box_tree = crate::tree::box_generation::generate_box_tree_with_anonymous_fixup(&styled_tree);

        // Resolve intrinsic sizes for replaced elements using the image cache
        self.resolve_replaced_intrinsic_sizes(&mut box_tree.root, Size::new(width as f32, height as f32));

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

        if std::env::var("FASTR_DUMP_COUNTS")
            .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
            .unwrap_or(false)
        {
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

        if std::env::var("FASTR_DUMP_TEXT")
            .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
            .unwrap_or(false)
        {
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

        // Update layout engine config for this viewport
        let config = LayoutConfig::for_viewport(Size::new(width as f32, height as f32));
        self.layout_engine = LayoutEngine::with_font_context(config, self.font_context.clone());
        intrinsic_cache_clear();
        let report_intrinsic = std::env::var_os("FASTR_INTRINSIC_STATS").is_some();
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
        let mut fragment_tree = self.layout_engine.layout_tree(&box_tree).map_err(|e| {
            Error::Render(RenderError::InvalidParameters {
                message: format!("Layout failed: {:?}", e),
            })
        })?;
        let capture_container_fields = std::env::var("FASTR_LOG_CONTAINER_FIELDS")
            .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
            .unwrap_or(false);
        let first_styled_snapshot = if has_container_queries && capture_container_fields {
            Some(styled_tree.clone())
        } else {
            None
        };

        // Re-run cascade/layout when container queries are present and we have resolved container sizes.
        static LOG_CONTAINER_PASS: OnceLock<bool> = OnceLock::new();
        let log_container_pass = *LOG_CONTAINER_PASS.get_or_init(|| {
            std::env::var("FASTR_LOG_CONTAINER_PASS")
                .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
                .unwrap_or(false)
        });
        static LOG_CONTAINER_REUSE: OnceLock<bool> = OnceLock::new();
        let log_reuse = *LOG_CONTAINER_REUSE.get_or_init(|| {
            std::env::var("FASTR_LOG_CONTAINER_REUSE")
                .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
                .unwrap_or(false)
        });

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
                let log_container_ids: Option<Vec<usize>> = std::env::var("FASTR_LOG_CONTAINER_IDS")
                    .ok()
                    .map(|s| {
                        s.split(',')
                            .filter_map(|tok| tok.trim().parse::<usize>().ok())
                            .collect::<Vec<_>>()
                    })
                    .filter(|v| !v.is_empty());

                let styled_tree = apply_styles_with_media_target_and_imports(
                    dom,
                    &resolved_stylesheet,
                    &media_ctx,
                    target_fragment.as_deref(),
                    None,
                    None::<&str>,
                    Some(&container_ctx),
                    Some(&container_scope),
                    Some(&reuse_map),
                );

                if let (true, Some(ids)) = (log_container_pass, log_container_ids.as_ref()) {
                    let summaries = styled_summary_map(&styled_tree);
                    for id in ids {
                        let summary = summaries.get(id).cloned().unwrap_or_else(|| "<unknown>".to_string());
                        if let Some(node) = find_styled_by_id(&styled_tree, *id) {
                            eprintln!(
                                "[container-pass] styled_id={} node={} styles={}",
                                id,
                                summary,
                                styled_style_summary(&node.styles)
                            );
                        } else {
                            eprintln!("[container-pass] styled_id={} node={} not found", id, summary);
                        }
                    }
                }

                let fingerprints_match = first_style_fingerprints
                    .as_ref()
                    .map(|first| {
                        let second = styled_fingerprint_map(&styled_tree);
                        *first == second
                    })
                    .unwrap_or(false);

                if fingerprints_match {
                    if log_container_pass {
                        eprintln!("[container-pass] fingerprints match; reuse layout, refresh fragment styles only");
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
                            static LOG_CONTAINER_DIFF: OnceLock<Option<usize>> = OnceLock::new();
                            let log_n = *LOG_CONTAINER_DIFF.get_or_init(|| {
                                std::env::var("FASTR_LOG_CONTAINER_DIFF")
                                    .ok()
                                    .and_then(|v| v.parse::<usize>().ok())
                            });
                            if let Some(n) = log_n {
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
                                    let summary =
                                        summaries.get(&base).cloned().unwrap_or_else(|| "<unknown>".to_string());
                                    let fp = second.get(id).cloned().unwrap_or(0);
                                    lines.push(format!("styled_id={} kind={} {} fp={}", base, kind, summary, fp));
                                    if capture_container_fields {
                                        if let (Some(first_tree), Some(new_node)) =
                                            (first_styled_snapshot.as_ref(), find_styled_by_id(&styled_tree, base))
                                        {
                                            if let Some(old_node) = find_styled_by_id(first_tree, base) {
                                                let diffs = diff_layout_fields(&old_node.styles, &new_node.styles);
                                                if !diffs.is_empty() {
                                                    lines.push(format!(
                                                        "  ↳ layout fields changed: {}",
                                                        diffs.join(",")
                                                    ));
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
                    let mut box_tree =
                        crate::tree::box_generation::generate_box_tree_with_anonymous_fixup(&styled_tree);
                    self.resolve_replaced_intrinsic_sizes(&mut box_tree.root, Size::new(width as f32, height as f32));

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
                    fragment_tree = self.layout_engine.layout_tree_reuse_caches(&box_tree).map_err(|e| {
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

        Ok(fragment_tree)
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
        paint_tree_with_resources_scaled_offset(
            fragment_tree,
            width,
            height,
            self.background_color,
            self.font_context.clone(),
            self.image_cache.clone(),
            self.device_pixel_ratio,
            offset,
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

    /// Clears any configured base URL, leaving relative resources unresolved.
    pub fn clear_base_url(&mut self) {
        self.image_cache.clear_base_url();
        self.base_url = None;
    }

    /// Gets the default background color
    pub fn background_color(&self) -> Rgba {
        self.background_color
    }

    /// Sets the default background color
    pub fn set_background_color(&mut self, color: Rgba) {
        self.background_color = color;
    }

    /// Extract the fragment identifier (without '#') from the configured base URL, if any.
    fn current_target_fragment(&self) -> Option<String> {
        self.base_url.as_ref().and_then(|url| extract_fragment(url))
    }

    /// Populate intrinsic sizes for replaced elements (e.g., images) using the image cache.
    fn resolve_replaced_intrinsic_sizes(&self, node: &mut BoxNode, viewport: Size) {
        if let BoxType::Marker(marker_box) = &mut node.box_type {
            if let MarkerContent::Image(replaced) = &mut marker_box.content {
                self.resolve_intrinsic_for_replaced(replaced, node.style.as_ref(), None, viewport);
            }
        }

        if let BoxType::Replaced(replaced_box) = &mut node.box_type {
            let alt = match &replaced_box.replaced_type {
                ReplacedType::Image { alt, .. } => alt.clone(),
                _ => None,
            };
            self.resolve_intrinsic_for_replaced(replaced_box, node.style.as_ref(), alt.as_deref(), viewport);
        }

        for child in &mut node.children {
            self.resolve_replaced_intrinsic_sizes(child, viewport);
        }
    }

    fn resolve_intrinsic_for_replaced(
        &self,
        replaced_box: &mut ReplacedBox,
        style: &ComputedStyle,
        alt: Option<&str>,
        viewport: Size,
    ) {
        let mut explicit_no_ratio = false;
        let replaced_type_snapshot = replaced_box.replaced_type.clone();
        match replaced_type_snapshot {
            ReplacedType::Image {
                src, alt: stored_alt, ..
            } => {
                let mut have_resource_dimensions = false;

                let selected = if !src.is_empty() {
                    let media_ctx = crate::style::media::MediaContext::screen(viewport.width, viewport.height)
                        .with_device_pixel_ratio(self.device_pixel_ratio)
                        .with_env_overrides();
                    Some(replaced_box.replaced_type.selected_image_source_for_context(
                        crate::tree::box_tree::ImageSelectionContext {
                            scale: self.device_pixel_ratio,
                            slot_width: None,
                            viewport: Some(viewport),
                            media_context: Some(&media_ctx),
                            font_size: Some(style.font_size),
                        },
                    ))
                } else {
                    None
                };

                if let Some(selected) = selected {
                    if let Ok(image) = self.image_cache.load(selected.url) {
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
            ReplacedType::Video { src: _, poster } => {
                let needs_intrinsic = replaced_box.intrinsic_size.is_none();
                let needs_ratio = replaced_box.aspect_ratio.is_none();
                if needs_intrinsic || needs_ratio {
                    if let Some(candidate) = poster.as_deref().filter(|s| !s.is_empty()) {
                        let image = if candidate.trim_start().starts_with('<') {
                            self.image_cache.render_svg(candidate)
                        } else {
                            self.image_cache.load(candidate)
                        };
                        if let Ok(image) = image {
                            let orientation = style.image_orientation.resolve(image.orientation, false);
                            explicit_no_ratio = image.aspect_ratio_none;
                            if let Some((w, h)) = image.css_dimensions(
                                orientation,
                                &style.image_resolution,
                                self.device_pixel_ratio,
                                None,
                            ) {
                                if needs_intrinsic {
                                    replaced_box.intrinsic_size = Some(Size::new(w, h));
                                }
                                if needs_ratio && !explicit_no_ratio {
                                    replaced_box.aspect_ratio = image
                                        .intrinsic_ratio(orientation)
                                        .or_else(|| if h > 0.0 { Some(w / h) } else { None });
                                }
                            }
                        }
                    }
                }
            }
            ReplacedType::Svg { content } => {
                let needs_intrinsic = replaced_box.intrinsic_size.is_none();
                let needs_ratio = replaced_box.aspect_ratio.is_none();

                if needs_intrinsic || needs_ratio {
                    // Inline SVG content can be rendered directly; otherwise try to load via URL/data URI.
                    let image = if content.trim_start().starts_with('<') {
                        self.image_cache.render_svg(&content)
                    } else if !content.is_empty() {
                        self.image_cache.load(&content)
                    } else {
                        Err(crate::error::Error::Image(crate::error::ImageError::LoadFailed {
                            url: "svg".to_string(),
                            reason: "empty content".to_string(),
                        }))
                    };

                    if let Ok(image) = image {
                        let orientation = style.image_orientation.resolve(image.orientation, false);
                        explicit_no_ratio = image.aspect_ratio_none;
                        if let Some((w, h)) =
                            image.css_dimensions(orientation, &style.image_resolution, self.device_pixel_ratio, None)
                        {
                            if needs_intrinsic {
                                replaced_box.intrinsic_size = Some(Size::new(w, h));
                            }
                            if needs_ratio && !explicit_no_ratio {
                                replaced_box.aspect_ratio = image
                                    .intrinsic_ratio(orientation)
                                    .or_else(|| if h > 0.0 { Some(w / h) } else { None });
                            }
                        }
                    }
                }
            }
            ReplacedType::Embed { src } | ReplacedType::Object { data: src } | ReplacedType::Iframe { src, .. } => {
                let needs_intrinsic = replaced_box.intrinsic_size.is_none();
                let needs_ratio = replaced_box.aspect_ratio.is_none();
                if (needs_intrinsic || needs_ratio) && !src.is_empty() {
                    let image = if src.trim_start().starts_with('<') {
                        self.image_cache.render_svg(&src)
                    } else {
                        self.image_cache.load(&src)
                    };
                    if let Ok(image) = image {
                        let orientation = style.image_orientation.resolve(image.orientation, false);
                        explicit_no_ratio = image.aspect_ratio_none;
                        if let Some((w, h)) =
                            image.css_dimensions(orientation, &style.image_resolution, self.device_pixel_ratio, None)
                        {
                            if needs_intrinsic {
                                replaced_box.intrinsic_size = Some(Size::new(w, h));
                            }
                            if needs_ratio && !explicit_no_ratio {
                                replaced_box.aspect_ratio = image
                                    .intrinsic_ratio(orientation)
                                    .or_else(|| if h > 0.0 { Some(w / h) } else { None });
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

        let mut runs = ShapingPipeline::new().shape(text, style, &self.font_context).ok()?;
        if runs.is_empty() {
            return None;
        }

        TextItem::apply_spacing_to_runs(&mut runs, text, style.letter_spacing, style.word_spacing);

        let metrics_scaled = self.resolve_scaled_metrics(style);
        let viewport = Size::new(self.default_width as f32, self.default_height as f32);
        let line_height = compute_line_height_with_metrics_viewport(style, metrics_scaled.as_ref(), Some(viewport));
        let metrics = TextItem::metrics_from_runs(&runs, line_height, style.font_size);
        let width: f32 = runs.iter().map(|r| r.advance).sum();
        let height = metrics.height;

        if width.is_finite() && height.is_finite() && height > 0.0 {
            Some(Size::new(width, height))
        } else {
            None
        }
    }

    // =========================================================================
    // Convenience methods for encoding to image formats
    // =========================================================================

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
    pub fn render_to_jpeg(&mut self, html: &str, width: u32, height: u32, quality: u8) -> Result<Vec<u8>> {
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
    pub fn render_to_webp(&mut self, html: &str, width: u32, height: u32, quality: u8) -> Result<Vec<u8>> {
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
}

impl CssImportFetcher {
    fn new(base_url: Option<String>, fetcher: Arc<dyn ResourceFetcher>) -> Self {
        Self { base_url, fetcher }
    }

    fn resolve_url(&self, href: &str) -> Option<String> {
        if href.starts_with("data:") {
            return Some(href.to_string());
        }

        if let Ok(abs) = Url::parse(href) {
            return Some(abs.to_string());
        }

        let base = self.base_url.as_ref()?;
        let mut base_candidate = base.clone();
        if base_candidate.starts_with("file://") {
            let path = &base_candidate["file://".len()..];
            if Path::new(path).is_dir() && !base_candidate.ends_with('/') {
                base_candidate.push('/');
            }
        }

        Url::parse(&base_candidate)
            .or_else(|_| Url::from_file_path(&base_candidate).map_err(|_| url::ParseError::RelativeUrlWithoutBase))
            .ok()
            .and_then(|base_url| base_url.join(href).ok())
            .map(|u| u.to_string())
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

        // Use the fetcher to get the bytes
        let resource = self.fetcher.fetch(&resolved)?;

        // Decode CSS bytes with charset handling
        Ok(decode_css_bytes(&resource.bytes, resource.content_type.as_deref()))
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
    use crate::style::types::ScrollbarColor::*;
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

fn hash_position_component(component: &crate::style::types::PositionComponent, hasher: &mut DefaultHasher) {
    use crate::style::types::PositionComponent::*;
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

fn hash_font_language_override(override_lang: &crate::style::types::FontLanguageOverride, hasher: &mut DefaultHasher) {
    match override_lang {
        crate::style::types::FontLanguageOverride::Normal => 0u8.hash(hasher),
        crate::style::types::FontLanguageOverride::Override(tag) => {
            1u8.hash(hasher);
            tag.hash(hasher);
        }
    }
}

fn hash_font_feature_settings(settings: &[crate::style::types::FontFeatureSetting], hasher: &mut DefaultHasher) {
    settings.len().hash(hasher);
    for setting in settings {
        setting.tag.hash(hasher);
        setting.value.hash(hasher);
    }
}

fn hash_font_variation_settings(settings: &[crate::style::types::FontVariationSetting], hasher: &mut DefaultHasher) {
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
    use crate::style::types::VerticalAlign::*;
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
    use crate::style::types::LineHeight::*;
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
    use crate::style::types::TextSizeAdjust::*;
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
    use crate::style::types::ListStyleType::*;
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
    use crate::style::types::GridTrack::*;
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
    use crate::style::types::FontSizeAdjust::*;
    match value {
        None => 0u8.hash(hasher),
        Number(n) => {
            1u8.hash(hasher);
            hash_f32(*n, hasher);
        }
        FromFont => 2u8.hash(hasher),
    }
}

fn hash_font_variant_alternates(value: &crate::style::types::FontVariantAlternates, hasher: &mut DefaultHasher) {
    value.historical_forms.hash(hasher);
    value.stylistic.hash(hasher);
    value.stylesets.hash(hasher);
    value.character_variants.hash(hasher);
    value.swash.hash(hasher);
    value.ornaments.hash(hasher);
    value.annotation.hash(hasher);
}

fn hash_font_variant_numeric(value: &crate::style::types::FontVariantNumeric, hasher: &mut DefaultHasher) {
    hash_enum_discriminant(&value.figure, hasher);
    hash_enum_discriminant(&value.spacing, hasher);
    hash_enum_discriminant(&value.fraction, hasher);
    value.ordinal.hash(hasher);
    value.slashed_zero.hash(hasher);
}

fn hash_font_variant_ligatures(value: &crate::style::types::FontVariantLigatures, hasher: &mut DefaultHasher) {
    value.common.hash(hasher);
    value.discretionary.hash(hasher);
    value.historical.hash(hasher);
    value.contextual.hash(hasher);
}

fn hash_font_variant_east_asian(value: &crate::style::types::FontVariantEastAsian, hasher: &mut DefaultHasher) {
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
    use crate::css::types::Transform::*;
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
            out.insert(base | 1, Arc::new(before.clone()));
        }
        if let Some(after) = &node.after_styles {
            out.insert(base | 2, Arc::new(after.clone()));
        }
        if let Some(marker) = &node.marker_styles {
            out.insert(base | 3, Arc::new(marker.clone()));
        }
        for child in &node.children {
            walk(child, out);
        }
    }

    let mut map = HashMap::new();
    walk(root, &mut map);
    map
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
            if let Some((_, id_val)) = attributes.iter().find(|(k, _)| k.eq_ignore_ascii_case("id")) {
                out.push('#');
                out.push_str(id_val);
            }
            if let Some((_, class_val)) = attributes.iter().find(|(k, _)| k.eq_ignore_ascii_case("class")) {
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
    root.children.iter().find_map(|child| find_styled_by_id(child, id))
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
        "".to_string()
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
    cmp!(writing_mode);
    cmp!(direction);
    cmp!(unicode_bidi);
    cmp!(text_align);
    cmp!(text_align_last);
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
                    containers
                        .entry(styled_id)
                        .and_modify(|entry| {
                            entry.inline_size = entry.inline_size.max(content_inline);
                            entry.block_size = entry.block_size.max(content_block);
                            entry.font_size = entry.font_size.max(node.style.font_size);
                        })
                        .or_insert(ContainerQueryInfo {
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
    1 + node.children.iter().map(count_styled_nodes_api).sum::<usize>()
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
            if mark(child, containers, in_container_subtree || is_container, scope) {
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

#[derive(Default, Debug)]
struct SnapBounds {
    max_x: f32,
    max_y: f32,
}

impl SnapBounds {
    fn update(&mut self, rect: Rect) {
        self.max_x = self.max_x.max(rect.max_x());
        self.max_y = self.max_y.max(rect.max_y());
    }
}

fn is_vertical_writing_mode(mode: WritingMode) -> bool {
    matches!(
        mode,
        WritingMode::VerticalRl | WritingMode::VerticalLr | WritingMode::SidewaysRl | WritingMode::SidewaysLr
    )
}

fn snap_axis_flags(axis: ScrollSnapAxis, inline_vertical: bool) -> (bool, bool) {
    match axis {
        ScrollSnapAxis::None => (false, false),
        ScrollSnapAxis::Both => (true, true),
        ScrollSnapAxis::X => (true, false),
        ScrollSnapAxis::Y => (false, true),
        ScrollSnapAxis::Inline => {
            if inline_vertical {
                (false, true)
            } else {
                (true, false)
            }
        }
        ScrollSnapAxis::Block => {
            if inline_vertical {
                (true, false)
            } else {
                (false, true)
            }
        }
    }
}

fn snap_position(alignment: ScrollSnapAlign, start: f32, extent: f32, viewport_extent: f32) -> Option<f32> {
    match alignment {
        ScrollSnapAlign::None => None,
        ScrollSnapAlign::Start => Some(start),
        ScrollSnapAlign::End => Some(start + extent - viewport_extent),
        ScrollSnapAlign::Center => Some(start + extent * 0.5 - viewport_extent * 0.5),
    }
}

fn pick_snap_target(
    current: f32,
    max_scroll: f32,
    strictness: ScrollSnapStrictness,
    threshold: f32,
    candidates: &[(f32, ScrollSnapStop)],
) -> f32 {
    if candidates.is_empty() {
        return current.min(max_scroll).max(0.0);
    }

    let mut best = current;
    let mut best_dist = f32::INFINITY;
    let mut best_stop_always = false;

    for &(candidate, stop) in candidates {
        let clamped = candidate.min(max_scroll).max(0.0);
        let dist = (clamped - current).abs();
        let prefer = dist + 1e-3 < best_dist
            || ((dist - best_dist).abs() <= 1e-3 && stop == ScrollSnapStop::Always && !best_stop_always);
        if prefer {
            best = clamped;
            best_dist = dist;
            best_stop_always = stop == ScrollSnapStop::Always;
        }
    }

    match strictness {
        ScrollSnapStrictness::Mandatory => best,
        ScrollSnapStrictness::Proximity => {
            if best_dist <= threshold {
                best
            } else {
                current
            }
        }
    }
}

fn collect_snap_targets(
    node: &FragmentNode,
    offset: Point,
    inline_vertical: bool,
    snap_x: bool,
    snap_y: bool,
    viewport: Size,
    bounds: &mut SnapBounds,
    targets_x: &mut Vec<(f32, ScrollSnapStop)>,
    targets_y: &mut Vec<(f32, ScrollSnapStop)>,
) {
    let abs_bounds = Rect::from_xywh(
        node.bounds.x() + offset.x,
        node.bounds.y() + offset.y,
        node.bounds.width(),
        node.bounds.height(),
    );
    bounds.update(abs_bounds);

    if let Some(style) = node.style.as_ref() {
        if snap_x {
            let align_x = if inline_vertical {
                style.scroll_snap_align.block
            } else {
                style.scroll_snap_align.inline
            };
            if let Some(pos) = snap_position(align_x, abs_bounds.x(), abs_bounds.width(), viewport.width) {
                targets_x.push((pos, style.scroll_snap_stop));
            }
        }
        if snap_y {
            let align_y = if inline_vertical {
                style.scroll_snap_align.inline
            } else {
                style.scroll_snap_align.block
            };
            if let Some(pos) = snap_position(align_y, abs_bounds.y(), abs_bounds.height(), viewport.height) {
                targets_y.push((pos, style.scroll_snap_stop));
            }
        }
    }

    let child_offset = Point::new(abs_bounds.x(), abs_bounds.y());
    for child in &node.children {
        collect_snap_targets(
            child,
            child_offset,
            inline_vertical,
            snap_x,
            snap_y,
            viewport,
            bounds,
            targets_x,
            targets_y,
        );
    }
}

fn find_snap_container<'a>(
    node: &'a FragmentNode,
    origin: Point,
) -> Option<(&'a FragmentNode, &'a ComputedStyle, Point)> {
    if let Some(style) = node.style.as_ref() {
        if style.scroll_snap_type.axis != ScrollSnapAxis::None {
            return Some((node, style, origin));
        }
    }

    for child in &node.children {
        let child_origin = Point::new(origin.x + child.bounds.x(), origin.y + child.bounds.y());
        if let Some(found) = find_snap_container(child, child_origin) {
            return Some(found);
        }
    }

    None
}

fn apply_scroll_snap(fragment_tree: &FragmentTree, viewport: Size, scroll: Point) -> Point {
    let Some((container, style, container_origin)) = find_snap_container(&fragment_tree.root, Point::ZERO) else {
        return scroll;
    };

    if style.scroll_snap_type.axis == ScrollSnapAxis::None {
        return scroll;
    }

    let inline_vertical = is_vertical_writing_mode(style.writing_mode);
    let (snap_x, snap_y) = snap_axis_flags(style.scroll_snap_type.axis, inline_vertical);
    if !snap_x && !snap_y {
        return scroll;
    }

    let mut targets_x = Vec::new();
    let mut targets_y = Vec::new();
    let mut bounds = SnapBounds::default();
    // Normalize coordinates so the snap container origin is at (0,0)
    let container_offset = Point::new(-container_origin.x, -container_origin.y);
    collect_snap_targets(
        container,
        container_offset,
        inline_vertical,
        snap_x,
        snap_y,
        viewport,
        &mut bounds,
        &mut targets_x,
        &mut targets_y,
    );

    // Ensure the container itself contributes to the scrollable area
    let container_rect = Rect::from_xywh(
        container.bounds.x() + container_offset.x,
        container.bounds.y() + container_offset.y,
        container.bounds.width(),
        container.bounds.height(),
    );
    bounds.update(container_rect);

    let max_scroll_x = (bounds.max_x - viewport.width).max(0.0);
    let max_scroll_y = (bounds.max_y - viewport.height).max(0.0);
    let strictness = style.scroll_snap_type.strictness;
    let snapped_x = if snap_x {
        pick_snap_target(scroll.x, max_scroll_x, strictness, viewport.width * 0.5, &targets_x)
    } else {
        scroll.x
    };
    let snapped_y = if snap_y {
        pick_snap_target(scroll.y, max_scroll_y, strictness, viewport.height * 0.5, &targets_y)
    } else {
        scroll.y
    };

    Point::new(snapped_x, snapped_y)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::css::parser::extract_css;
    use crate::layout::contexts::inline::line_builder::TextItem;
    use crate::layout::engine::{LayoutConfig, LayoutEngine};
    use crate::layout::formatting_context::intrinsic_cache_clear;
    use crate::style::cascade::StyledNode;
    use crate::style::types::ImageResolution;
    use crate::text::pipeline::ShapingPipeline;
    use crate::tree::fragment_tree::{FragmentContent, FragmentTree};
    use crate::ComputedStyle;
    use base64::Engine;
    use image::codecs::png::PngEncoder;
    use image::ImageEncoder;
    use image::{load_from_memory, ColorType, RgbaImage};
    use std::sync::Arc;

    fn text_color_for(tree: &FragmentTree, needle: &str) -> Option<Rgba> {
        tree.iter_fragments().find_map(|frag| match &frag.content {
            FragmentContent::Text { text, .. } if text.contains(needle) => frag.style.as_ref().map(|s| s.color),
            _ => None,
        })
    }

    fn styled_color_by_id(styled: &StyledNode, id: &str) -> Option<Rgba> {
        if styled.node.get_attribute("id").as_deref() == Some(id) {
            return Some(styled.styles.color);
        }
        styled.children.iter().find_map(|child| styled_color_by_id(child, id))
    }

    fn styled_node_id_by_id(styled: &StyledNode, id: &str) -> Option<usize> {
        if styled.node.get_attribute("id").as_deref() == Some(id) {
            return Some(styled.node_id);
        }
        styled.children.iter().find_map(|child| styled_node_id_by_id(child, id))
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

    // =========================================================================
    // Creation Tests (these should always pass)
    // =========================================================================

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
    fn render_html_with_horizontal_scroll_offsets_viewport() {
        let mut renderer = FastRender::new().unwrap();
        let html = r#"
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
        "#;

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
        let html = r#"
            <style>
                @media (max-width: 600px) { body { color: rgb(255, 0, 0); } }
                @media (min-width: 601px) { body { color: rgb(0, 0, 255); } }
            </style>
            <body>hi</body>
        "#;
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
        let html = format!(r#"<style>@import url("{}");</style><body>data import</body>"#, data_css);
        let dom = renderer.parse_html(&html).unwrap();
        let styled = renderer.layout_document(&dom, 320, 200).unwrap();
        let color = text_color_for(&styled, "data").expect("text color");
        assert_eq!(color, Rgba::rgb(11, 12, 13));
    }

    #[test]
    fn render_uses_configured_default_viewport() {
        let mut renderer = FastRender::builder().viewport_size(320, 240).build().unwrap();
        let png = renderer.render("<div></div>").unwrap();
        let image = load_from_memory(&png).unwrap();
        assert_eq!(image.width(), 320);
        assert_eq!(image.height(), 240);
    }

    // =========================================================================
    // Rendering Tests (may fail if pipeline not fully integrated)
    // These are marked with ignore to match the integration_test.rs pattern
    // =========================================================================

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
        let mut box_tree = crate::tree::box_generation::generate_box_tree_with_anonymous_fixup(&styled);
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
        let second_container_id = styled_node_id_by_id(&styled_with_containers, "container").expect("container id");
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
        let mut box_tree = crate::tree::box_generation::generate_box_tree_with_anonymous_fixup(&styled);
        renderer.resolve_replaced_intrinsic_sizes(&mut box_tree.root, Size::new(800.0, 600.0));
        intrinsic_cache_clear();
        let fragments = renderer.layout_engine.layout_tree(&box_tree).unwrap();

        let cq_ctx = build_container_query_context(&box_tree, &fragments, &media_ctx);
        assert!(!cq_ctx.containers.is_empty(), "expected named container context");
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
        let mut box_tree = crate::tree::box_generation::generate_box_tree_with_anonymous_fixup(&styled);
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
        let mut box_tree = crate::tree::box_generation::generate_box_tree_with_anonymous_fixup(&styled);
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
        let mut box_tree = crate::tree::box_generation::generate_box_tree_with_anonymous_fixup(&styled);
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
        let mut box_tree = crate::tree::box_generation::generate_box_tree_with_anonymous_fixup(&styled);
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
        TextItem::apply_spacing_to_runs(&mut runs, alt_text, style.letter_spacing, style.word_spacing);
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
        let viewport = Size::new(renderer.default_width as f32, renderer.default_height as f32);
        let line_height = compute_line_height_with_metrics_viewport(&style, scaled.as_ref(), Some(viewport));
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
                content: r#"<svg xmlns='http://www.w3.org/2000/svg' width='20' height='12'></svg>"#.to_string(),
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
                content:
                    r#"<svg xmlns='http://www.w3.org/2000/svg' width='200' height='100' viewBox='0 0 50 100' preserveAspectRatio='none'></svg>"#
                        .to_string(),
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
                content: r#"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 50 100'></svg>"#.to_string(),
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
                content: r#"
                    <svg xmlns='http://www.w3.org/2000/svg' width='20' height='10' viewBox='0 0 20 10'>
                        <rect x='0' y='0' width='20' height='10' stroke='black' stroke-width='50' fill='none'/>
                    </svg>
                "#
                .to_string(),
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
            ReplacedType::Embed { src: svg.to_string() },
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
            ReplacedType::Object { data: svg.to_string() },
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
        let html = r#"
            <style>
                html, body { margin: 0; height: 100%; scroll-snap-type: y mandatory; }
                section { height: 200px; scroll-snap-align: start; }
            </style>
            <section></section>
            <section></section>
        "#;

        let dom = renderer.parse_html(html).unwrap();
        let fragments = renderer.layout_document(&dom, 100, 100).unwrap();

        let snapped = super::apply_scroll_snap(&fragments, Size::new(100.0, 100.0), Point::new(0.0, 120.0));
        assert!((snapped.y - 200.0).abs() < 0.1, "expected snap to the second section");
        assert!(snapped.x.abs() < 0.1);
    }

    #[test]
    fn scroll_snap_none_leaves_offsets_unchanged() {
        let mut renderer = FastRender::new().unwrap();
        let html = r#"
            <style>
                html, body { margin: 0; height: 100%; scroll-snap-type: none; }
                section { height: 200px; scroll-snap-align: start; }
            </style>
            <section></section>
            <section></section>
        "#;

        let dom = renderer.parse_html(html).unwrap();
        let fragments = renderer.layout_document(&dom, 100, 100).unwrap();

        let snapped = super::apply_scroll_snap(&fragments, Size::new(100.0, 100.0), Point::new(0.0, 120.0));
        assert!(
            (snapped.y - 120.0).abs() < 0.1,
            "scroll-snap-type:none should not adjust scroll offsets"
        );
        assert!(snapped.x.abs() < 0.1);
    }

    #[test]
    fn scroll_snap_proximity_only_when_close() {
        let mut renderer = FastRender::new().unwrap();
        let html = r#"
            <style>
                html, body { margin: 0; height: 100%; scroll-snap-type: y proximity; }
                section { height: 200px; scroll-snap-align: start; }
            </style>
            <section></section>
            <section></section>
        "#;

        let dom = renderer.parse_html(html).unwrap();
        let fragments = renderer.layout_document(&dom, 100, 100).unwrap();

        let near = super::apply_scroll_snap(&fragments, Size::new(100.0, 100.0), Point::new(0.0, 30.0));
        assert!(near.y.abs() < 0.1, "nearby offset should snap to the first section");

        let far = super::apply_scroll_snap(&fragments, Size::new(100.0, 100.0), Point::new(0.0, 260.0));
        assert!(
            (far.y - 260.0).abs() < 0.1,
            "far offset should remain unchanged for proximity snapping"
        );
    }

    #[test]
    fn scroll_snap_horizontal_mandatory_adjusts_offsets() {
        let mut renderer = FastRender::new().unwrap();
        let html = r#"
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
        "#;

        let dom = renderer.parse_html(html).unwrap();
        let fragments = renderer.layout_document(&dom, 100, 100).unwrap();

        let snapped = super::apply_scroll_snap(&fragments, Size::new(100.0, 100.0), Point::new(120.0, 0.0));
        assert!(
            (snapped.x - 200.0).abs() < 0.1,
            "expected snap to the second section on the inline axis (snapped={:?}, xs={:?}, content={:?})",
            snapped,
            fragments
                .root
                .children
                .iter()
                .map(|c| c.bounds.x())
                .collect::<Vec<_>>(),
            fragments.content_size()
        );
        assert!(snapped.y.abs() < 0.1);
    }

    #[test]
    fn scroll_snap_inline_axis_matches_horizontal_flow() {
        let mut renderer = FastRender::new().unwrap();
        let html = r#"
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
        "#;

        let dom = renderer.parse_html(html).unwrap();
        let fragments = renderer.layout_document(&dom, 100, 100).unwrap();

        let (container, style, origin) =
            super::find_snap_container(&fragments.root, Point::ZERO).expect("snap container");
        let inline_vertical = super::is_vertical_writing_mode(style.writing_mode);
        let (snap_x, snap_y) = super::snap_axis_flags(style.scroll_snap_type.axis, inline_vertical);
        let mut targets_x = Vec::new();
        let mut targets_y = Vec::new();
        let mut bounds = SnapBounds::default();
        super::collect_snap_targets(
            container,
            Point::new(-origin.x, -origin.y),
            inline_vertical,
            snap_x,
            snap_y,
            Size::new(100.0, 100.0),
            &mut bounds,
            &mut targets_x,
            &mut targets_y,
        );

        let snapped = super::apply_scroll_snap(&fragments, Size::new(100.0, 100.0), Point::new(120.0, 0.0));
        assert!(
            (snapped.x - 200.0).abs() < 0.1,
            "inline axis snapping should match the horizontal flow (snapped={:?}, targets_x={:?}, bounds={:?}, content={:?})",
            snapped,
            targets_x,
            bounds,
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
        let fragments = FragmentTree::with_viewport(container, Size::new(100.0, 100.0));

        let snapped = super::apply_scroll_snap(&fragments, Size::new(100.0, 100.0), Point::new(120.0, 0.0));
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
        let fragments = FragmentTree::with_viewport(container, Size::new(100.0, 100.0));

        let snapped = super::apply_scroll_snap(&fragments, Size::new(100.0, 100.0), Point::new(0.0, 120.0));
        assert!(
            (snapped.y - 200.0).abs() < 0.1,
            "inline axis should snap vertically in vertical writing modes (snapped={:?})",
            snapped
        );
        assert!(snapped.x.abs() < 0.1);
    }

    #[test]
    fn scroll_snap_stop_always_breaks_ties() {
        let candidates = vec![
            (100.0, ScrollSnapStop::Normal),
            (200.0, ScrollSnapStop::Always),
        ];

        let snapped = super::pick_snap_target(
            150.0,
            400.0,
            ScrollSnapStrictness::Mandatory,
            50.0,
            &candidates,
        );

        assert!(
            (snapped - 200.0).abs() < 0.1,
            "stop:always targets should win ties at equal distance"
        );
    }
}
