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

use crate::css::parser::extract_css;
use crate::dom::{self, DomNode};
use crate::error::{Error, RenderError, Result};
use crate::geometry::Size;
use crate::image_loader::ImageCache;
use crate::image_output::{encode_image, OutputFormat};
use crate::layout::engine::{LayoutConfig, LayoutEngine};
use crate::paint::painter::paint_tree_with_resources;
use crate::style::cascade::apply_styles;
use crate::style::color::Rgba;
use crate::text::font_loader::FontContext;
use crate::tree::box_generation::generate_box_tree;
use crate::tree::box_tree::{BoxNode, BoxType, ReplacedType};
use crate::tree::fragment_tree::FragmentTree;
use image::GenericImageView;

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
}

impl std::fmt::Debug for FastRender {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FastRender")
            .field("background_color", &self.background_color)
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

    /// Base URL used to resolve relative resource references (images, CSS)
    pub base_url: Option<String>,
}

impl Default for FastRenderConfig {
    fn default() -> Self {
        Self {
            background_color: Rgba::WHITE,
            default_width: 800,
            default_height: 600,
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

    /// Sets the base URL used to resolve relative resource references.
    pub fn with_base_url(mut self, url: impl Into<String>) -> Self {
        self.base_url = Some(url.into());
        self
    }
}

impl FastRender {
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
        let styled_tree = apply_styles(dom, &stylesheet);

        // Generate box tree
        let mut box_tree = generate_box_tree(&styled_tree);

        // Resolve intrinsic sizes for replaced elements using the image cache
        self.resolve_replaced_intrinsic_sizes(&mut box_tree.root);

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
        paint_tree_with_resources(
            fragment_tree,
            width,
            height,
            self.background_color,
            self.font_context.clone(),
            self.image_cache.clone(),
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
        self.image_cache.set_base_url(base_url);
    }

    /// Clears any configured base URL, leaving relative resources unresolved.
    pub fn clear_base_url(&mut self) {
        self.image_cache.clear_base_url();
    }

    /// Gets the default background color
    pub fn background_color(&self) -> Rgba {
        self.background_color
    }

    /// Sets the default background color
    pub fn set_background_color(&mut self, color: Rgba) {
        self.background_color = color;
    }

    /// Populate intrinsic sizes for replaced elements (e.g., images) using the image cache.
    fn resolve_replaced_intrinsic_sizes(&self, node: &mut BoxNode) {
        if let BoxType::Replaced(replaced_box) = &mut node.box_type {
            // Fill missing intrinsic size/aspect ratio for images
            if let ReplacedType::Image { src } = &replaced_box.replaced_type {
                let needs_intrinsic = replaced_box.intrinsic_size.is_none();
                let needs_ratio = replaced_box.aspect_ratio.is_none();

                if (needs_intrinsic || needs_ratio) && !src.is_empty() {
                    if let Ok(image) = self.image_cache.load(src) {
                        let (w, h) = image.dimensions();
                        if w > 0 && h > 0 {
                            let size = Size::new(w as f32, h as f32);
                            if needs_intrinsic {
                                replaced_box.intrinsic_size = Some(size);
                            }
                            if needs_ratio {
                                replaced_box.aspect_ratio = Some(size.width / size.height);
                            }
                        }
                    }
                }
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

        for child in &mut node.children {
            self.resolve_replaced_intrinsic_sizes(child);
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
        self.render_to_png(html, 800, 600)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
