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
use crate::geometry::Size;
use crate::image_loader::ImageCache;
use crate::image_output::{encode_image, OutputFormat};
use crate::layout::contexts::inline::baseline::compute_line_height_with_metrics;
use crate::layout::contexts::inline::line_builder::TextItem;
use crate::layout::engine::{LayoutConfig, LayoutEngine};
use crate::paint::painter::paint_tree_with_resources_scaled;
use crate::style::cascade::apply_styles_with_media_and_target;
use crate::style::color::Rgba;
use crate::style::media::MediaContext;
use crate::style::ComputedStyle;
use crate::text::font_db::{FontStretch, FontStyle as DbFontStyle, ScaledMetrics};
use crate::text::font_loader::FontContext;
use crate::text::pipeline::ShapingPipeline;
use crate::tree::box_generation::generate_box_tree;
use crate::tree::box_tree::{BoxNode, BoxType, MarkerContent, ReplacedBox, ReplacedType};
use crate::tree::fragment_tree::FragmentTree;
use std::io;
use std::path::Path;
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
#[derive(Debug, Clone)]
pub struct FastRenderBuilder {
    config: FastRenderConfig,
}

impl FastRenderBuilder {
    /// Creates a new builder with default configuration
    pub fn new() -> Self {
        Self {
            config: FastRenderConfig::default(),
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

    /// Builds the FastRender instance
    pub fn build(self) -> Result<FastRender> {
        FastRender::with_config(self.config)
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
        let font_context = FontContext::new();
        let layout_config =
            LayoutConfig::for_viewport(Size::new(config.default_width as f32, config.default_height as f32));
        let layout_engine = LayoutEngine::with_font_context(layout_config, font_context.clone());
        let image_cache = match &config.base_url {
            Some(url) => ImageCache::with_base_url(url.clone()),
            None => ImageCache::new(),
        };

        Ok(Self {
            font_context,
            layout_engine,
            image_cache,
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
        // Validate dimensions
        if width == 0 || height == 0 {
            return Err(Error::Render(RenderError::InvalidParameters {
                message: format!("Invalid dimensions: width={}, height={}", width, height),
            }));
        }

        // Parse HTML to DOM
        let dom = self.parse_html(html)?;

        // Layout the document
        let fragment_tree = self.layout_document(&dom, width, height)?;

        // Paint to pixmap
        self.paint(&fragment_tree, width, height)
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

        // Apply styles to create styled tree
        let target_fragment = self.current_target_fragment();
        let media_ctx =
            MediaContext::screen(width as f32, height as f32).with_device_pixel_ratio(self.device_pixel_ratio);
        let import_loader = CssImportFetcher {
            base_url: self.base_url.clone(),
        };
        let resolved_stylesheet = stylesheet.resolve_imports(&import_loader, self.base_url.as_deref(), &media_ctx);
        self.font_context.clear_web_fonts();
        let font_faces = resolved_stylesheet.collect_font_face_rules(&media_ctx);
        // Best-effort loading; rendering should continue even if a web font fails.
        let _ = self.font_context.load_web_fonts(&font_faces, self.base_url.as_deref());
        let styled_tree =
            apply_styles_with_media_and_target(dom, &resolved_stylesheet, &media_ctx, target_fragment.as_deref());

        // Generate box tree
        let mut box_tree = generate_box_tree(&styled_tree);

        // Resolve intrinsic sizes for replaced elements using the image cache
        self.resolve_replaced_intrinsic_sizes(&mut box_tree.root, Size::new(width as f32, height as f32));

        // Update layout engine config for this viewport
        let config = LayoutConfig::for_viewport(Size::new(width as f32, height as f32));
        self.layout_engine = LayoutEngine::with_font_context(config, self.font_context.clone());

        // Perform layout
        let fragment_tree = self.layout_engine.layout_tree(&box_tree).map_err(|e| {
            Error::Render(RenderError::InvalidParameters {
                message: format!("Layout failed: {:?}", e),
            })
        })?;

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
        let replaced_type_snapshot = replaced_box.replaced_type.clone();
        match replaced_type_snapshot {
            ReplacedType::Image {
                src, alt: stored_alt, ..
            } => {
                let needs_intrinsic = replaced_box.intrinsic_size.is_none();
                let needs_ratio = replaced_box.aspect_ratio.is_none();
                let mut have_resource_dimensions = false;

                let chosen_src = if (needs_intrinsic || needs_ratio) && !src.is_empty() {
                    let media_ctx = crate::style::media::MediaContext::screen(viewport.width, viewport.height)
                        .with_device_pixel_ratio(self.device_pixel_ratio);
                    Some(
                        replaced_box
                            .replaced_type
                            .image_source_for_context(crate::tree::box_tree::ImageSelectionContext {
                                scale: self.device_pixel_ratio,
                                slot_width: None,
                                viewport: Some(viewport),
                                media_context: Some(&media_ctx),
                                font_size: Some(style.font_size),
                            })
                            .to_string(),
                    )
                } else {
                    None
                };

                if let Some(chosen_src) = chosen_src {
                    if let Ok(image) = self.image_cache.load(&chosen_src) {
                        let orientation = style.image_orientation.resolve(image.orientation, false);
                        if let Some((w, h)) =
                            image.css_dimensions(orientation, &style.image_resolution, self.device_pixel_ratio, None)
                        {
                            if needs_intrinsic {
                                replaced_box.intrinsic_size = Some(Size::new(w, h));
                            }
                            if needs_ratio && h > 0.0 {
                                replaced_box.aspect_ratio = Some(w / h);
                            }
                            have_resource_dimensions = true;
                        }
                    }
                }

                if !have_resource_dimensions && (needs_intrinsic || needs_ratio) {
                    let candidate_alt = alt
                        .filter(|s| !s.is_empty())
                        .or_else(|| stored_alt.as_deref().filter(|s| !s.is_empty()));
                    if let Some(size) = candidate_alt.and_then(|text| self.alt_intrinsic_size(style, text)) {
                        if needs_intrinsic {
                            replaced_box.intrinsic_size.get_or_insert(size);
                        }
                        if needs_ratio && size.height > 0.0 && replaced_box.aspect_ratio.is_none() {
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
                            if let Some((w, h)) = image.css_dimensions(
                                orientation,
                                &style.image_resolution,
                                self.device_pixel_ratio,
                                None,
                            ) {
                                if needs_intrinsic {
                                    replaced_box.intrinsic_size = Some(Size::new(w, h));
                                }
                                if needs_ratio && h > 0.0 {
                                    replaced_box.aspect_ratio = Some(w / h);
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
                        if let Some((w, h)) =
                            image.css_dimensions(orientation, &style.image_resolution, self.device_pixel_ratio, None)
                        {
                            if needs_intrinsic {
                                replaced_box.intrinsic_size = Some(Size::new(w, h));
                            }
                            if needs_ratio && h > 0.0 {
                                replaced_box.aspect_ratio = Some(w / h);
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
                        if let Some((w, h)) =
                            image.css_dimensions(orientation, &style.image_resolution, self.device_pixel_ratio, None)
                        {
                            if needs_intrinsic {
                                replaced_box.intrinsic_size = Some(Size::new(w, h));
                            }
                            if needs_ratio && h > 0.0 {
                                replaced_box.aspect_ratio = Some(w / h);
                            }
                        }
                    }
                }
            }
            _ => {}
        }

        // If only intrinsic size is present, ensure aspect ratio is recorded
        if replaced_box.aspect_ratio.is_none() {
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
        let line_height = compute_line_height_with_metrics(style, metrics_scaled.as_ref());
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

#[derive(Clone, Debug)]
struct CssImportFetcher {
    base_url: Option<String>,
}

impl CssImportFetcher {
    fn resolve_url(&self, href: &str) -> Option<Url> {
        if href.starts_with("data:") {
            return None;
        }

        if let Ok(abs) = Url::parse(href) {
            return Some(abs);
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
    }
}

impl CssImportLoader for CssImportFetcher {
    fn load(&self, url: &str) -> Result<String> {
        if url.starts_with("data:") {
            return decode_data_url_to_string(url);
        }

        let resolved = self.resolve_url(url).or_else(|| Url::parse(url).ok()).ok_or_else(|| {
            Error::Io(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Cannot resolve @import URL '{}'", url),
            ))
        })?;

        match resolved.scheme() {
            "file" => {
                let path = resolved.to_file_path().map_err(|_| {
                    Error::Io(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        format!("Invalid file URL for @import: {}", resolved),
                    ))
                })?;
                let bytes = std::fs::read(&path).map_err(Error::Io)?;
                Ok(decode_css_bytes(&bytes, None))
            }
            _ => {
                let config = ureq::Agent::config_builder()
                    .timeout_global(Some(std::time::Duration::from_secs(30)))
                    .build();
                let agent: ureq::Agent = config.into();

                let mut response = agent
                    .get(resolved.as_str())
                    .call()
                    .map_err(|e| Error::Io(io::Error::new(io::ErrorKind::Other, e.to_string())))?;

                let content_type = response
                    .headers()
                    .get("content-type")
                    .and_then(|h| h.to_str().ok())
                    .map(|s| s.to_string());

                let bytes = response
                    .body_mut()
                    .read_to_vec()
                    .map_err(|e| Error::Io(io::Error::new(io::ErrorKind::Other, e.to_string())))?;

                Ok(decode_css_bytes(&bytes, content_type.as_deref()))
            }
        }
    }
}

fn decode_data_url_to_string(data_url: &str) -> Result<String> {
    if !data_url.starts_with("data:") {
        return Err(Error::Io(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Not a data: URL",
        )));
    }

    let mut parts = data_url.splitn(2, ',');
    let header = parts
        .next()
        .ok_or_else(|| Error::Io(io::Error::new(io::ErrorKind::InvalidInput, "Malformed data URL")))?;
    let payload = parts
        .next()
        .ok_or_else(|| Error::Io(io::Error::new(io::ErrorKind::InvalidInput, "Missing data in data URL")))?;

    let header = header.trim_start_matches("data:");
    let mut is_base64 = false;
    let mut charset: Option<String> = None;
    let mut mime: Option<String> = None;
    for (idx, segment) in header.split(';').enumerate() {
        let seg = segment.trim();
        if seg.is_empty() {
            continue;
        }
        if seg.eq_ignore_ascii_case("base64") {
            is_base64 = true;
        } else if seg.to_ascii_lowercase().starts_with("charset=") {
            charset = seg.split_once('=').map(|(_, v)| v.trim().to_string());
        } else if idx == 0 && seg.contains('/') {
            mime = Some(seg.to_string());
        }
    }

    let bytes = if is_base64 {
        use base64::Engine;
        base64::engine::general_purpose::STANDARD.decode(payload).map_err(|e| {
            Error::Io(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Invalid base64 data URL: {}", e),
            ))
        })?
    } else {
        percent_decode_bytes(payload)?
    };

    let content_type = match (mime, charset) {
        (Some(m), Some(cs)) => Some(format!("{};charset={}", m, cs)),
        (Some(m), None) => Some(m),
        (None, Some(cs)) => Some(format!("text/plain;charset={}", cs)),
        (None, None) => None,
    };

    Ok(decode_css_bytes(&bytes, content_type.as_deref()))
}

fn percent_decode_bytes(input: &str) -> Result<Vec<u8>> {
    let mut out = Vec::with_capacity(input.len());
    let bytes = input.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        if bytes[i] == b'%' {
            if i + 2 >= bytes.len() {
                return Err(Error::Io(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Incomplete percent-escape in data URL",
                )));
            }
            let hi = (bytes[i + 1] as char).to_digit(16);
            let lo = (bytes[i + 2] as char).to_digit(16);
            match (hi, lo) {
                (Some(hi), Some(lo)) => {
                    out.push(((hi << 4) | lo) as u8);
                    i += 3;
                }
                _ => {
                    return Err(Error::Io(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Invalid percent-escape in data URL",
                    )))
                }
            }
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    Ok(out)
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::layout::contexts::inline::line_builder::TextItem;
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
        let line_height = compute_line_height_with_metrics(&style, scaled.as_ref());
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
}
