use crate::css::{self, Color};
use crate::dom;
use crate::error::{Error, Result};
use crate::image_output;
use crate::layout;
use crate::paint;
use crate::style;

pub use crate::image_output::OutputFormat as ImageFormat;

#[derive(Debug, Clone)]
pub struct Renderer {
    viewport_width: u32,
    viewport_height: u32,
    background_color: Color,
}

#[derive(Debug, Clone)]
pub struct RenderOptions {
    pub format: ImageFormat,
    pub background_color: Color,
}

impl Default for RenderOptions {
    fn default() -> Self {
        Self {
            format: ImageFormat::Png,
            background_color: Color::WHITE,
        }
    }
}

impl Renderer {
    pub fn new() -> Self {
        Self {
            viewport_width: 1920,
            viewport_height: 1080,
            background_color: Color::WHITE,
        }
    }

    pub fn builder() -> RendererBuilder {
        RendererBuilder::new()
    }

    pub fn render_to_png(&self, html: &str, width: u32, height: u32) -> Result<Vec<u8>> {
        let options = RenderOptions {
            format: ImageFormat::Png,
            background_color: self.background_color,
        };

        self.render_with_size(html, width, height, options)
    }

    /// Renders HTML to PNG with automatic height based on content
    pub fn render_to_png_auto_height(&self, html: &str, width: u32) -> Result<Vec<u8>> {
        // First pass: compute layout with a very large viewport to get actual content height
        let dom = dom::parse_html(html)?;
        let stylesheet = extract_css(&dom)?;
        let styled_tree = style::apply_styles(&dom, &stylesheet);

        // Use a very large height for initial layout computation
        let temp_height = 100000.0;
        let layout_tree = layout::compute_layout(&styled_tree, width as f32, temp_height);

        // Calculate actual content height from layout tree
        let actual_height = calculate_content_height(&layout_tree.root);
        let height = (actual_height.ceil() as u32).max(100); // Ensure minimum height

        // Second pass: render with actual content height
        let options = RenderOptions {
            format: ImageFormat::Png,
            background_color: self.background_color,
        };

        self.render_with_size(html, width, height, options)
    }

    /// Renders HTML to PNG with viewport and optional scroll offset
    pub fn render_to_png_with_scroll(&self, html: &str, width: u32, height: u32, scroll_y: u32) -> Result<Vec<u8>> {
        let options = RenderOptions {
            format: ImageFormat::Png,
            background_color: self.background_color,
        };

        self.render_with_size_and_scroll(html, width, height, scroll_y, options)
    }

    /// Renders HTML to PNG with viewport, scroll offset, and base URL for resolving relative image paths
    pub fn render_to_png_with_scroll_and_base_url(&self, html: &str, width: u32, height: u32, scroll_y: u32, base_url: String) -> Result<Vec<u8>> {
        let options = RenderOptions {
            format: ImageFormat::Png,
            background_color: self.background_color,
        };

        self.render_with_size_scroll_and_base_url(html, width, height, scroll_y, options, Some(base_url))
    }

    fn render_with_size_and_scroll(&self, html: &str, width: u32, height: u32, scroll_y: u32, options: RenderOptions) -> Result<Vec<u8>> {
        self.render_with_size_scroll_and_base_url(html, width, height, scroll_y, options, None)
    }

    fn render_with_size_scroll_and_base_url(&self, html: &str, width: u32, height: u32, scroll_y: u32, options: RenderOptions, base_url: Option<String>) -> Result<Vec<u8>> {
        if width == 0 || height == 0 {
            return Err(Error::InvalidDimensions(width, height));
        }

        // Parse HTML
        let dom = dom::parse_html(html)?;

        // Extract and parse CSS
        let stylesheet = extract_css(&dom)?;

        // Apply styles
        let styled_tree = style::apply_styles(&dom, &stylesheet);

        // Compute layout with large viewport height to get full content
        let full_height = 100000.0;
        let layout_tree = layout::compute_layout(&styled_tree, width as f32, full_height);

        // Paint to pixmap with scroll offset and base URL for image resolution
        let pixmap = paint::paint_with_scroll(&layout_tree.root, width, height, scroll_y, options.background_color, base_url)?;

        // Encode to image format
        let image_data = image_output::encode_image(&pixmap, options.format)?;

        Ok(image_data)
    }

    pub fn render_to_jpeg(&self, html: &str, width: u32, height: u32, quality: u8) -> Result<Vec<u8>> {
        let options = RenderOptions {
            format: ImageFormat::Jpeg(quality),
            background_color: self.background_color,
        };

        self.render_with_size(html, width, height, options)
    }

    pub fn render_to_webp(&self, html: &str, width: u32, height: u32, quality: u8) -> Result<Vec<u8>> {
        let options = RenderOptions {
            format: ImageFormat::WebP(quality),
            background_color: self.background_color,
        };

        self.render_with_size(html, width, height, options)
    }

    pub fn render(&self, html: &str) -> Result<Vec<u8>> {
        self.render_to_png(html, self.viewport_width, self.viewport_height)
    }

    pub fn render_with_options(&self, html: &str, options: RenderOptions) -> Result<Vec<u8>> {
        self.render_with_size(html, self.viewport_width, self.viewport_height, options)
    }

    fn render_with_size(&self, html: &str, width: u32, height: u32, options: RenderOptions) -> Result<Vec<u8>> {
        if width == 0 || height == 0 {
            return Err(Error::InvalidDimensions(width, height));
        }

        // Parse HTML
        let dom = dom::parse_html(html)?;

        // Extract and parse CSS
        let stylesheet = extract_css(&dom)?;

        // Apply styles
        let styled_tree = style::apply_styles(&dom, &stylesheet);

        // Compute layout
        let layout_tree = layout::compute_layout(&styled_tree, width as f32, height as f32);

        // Paint to pixmap
        let pixmap = paint::paint(&layout_tree.root, width, height, options.background_color)?;

        // Encode to image format
        let image_data = image_output::encode_image(&pixmap, options.format)?;

        Ok(image_data)
    }
}

impl Default for Renderer {
    fn default() -> Self {
        Self::new()
    }
}

pub struct RendererBuilder {
    viewport_width: u32,
    viewport_height: u32,
    background_color: Color,
}

impl RendererBuilder {
    pub fn new() -> Self {
        Self {
            viewport_width: 1920,
            viewport_height: 1080,
            background_color: Color::WHITE,
        }
    }

    pub fn viewport_width(mut self, width: u32) -> Self {
        self.viewport_width = width;
        self
    }

    pub fn viewport_height(mut self, height: u32) -> Self {
        self.viewport_height = height;
        self
    }

    pub fn viewport_size(mut self, width: u32, height: u32) -> Self {
        self.viewport_width = width;
        self.viewport_height = height;
        self
    }

    pub fn background_color(mut self, color: Color) -> Self {
        self.background_color = color;
        self
    }

    pub fn build(self) -> Renderer {
        Renderer {
            viewport_width: self.viewport_width,
            viewport_height: self.viewport_height,
            background_color: self.background_color,
        }
    }
}

impl Default for RendererBuilder {
    fn default() -> Self {
        Self::new()
    }
}

fn calculate_content_height(layout_box: &layout::LayoutBox) -> f32 {
    // Calculate the maximum y + height from this box and all its children
    let box_bottom = layout_box.y + layout_box.height;

    let children_bottom = layout_box.children.iter()
        .map(|child| calculate_content_height(child))
        .fold(box_bottom, f32::max);

    children_bottom
}

fn extract_css(dom: &dom::DomNode) -> Result<css::StyleSheet> {
    let mut css_content = String::new();

    // Recursively find <style> tags
    dom.walk_tree(&mut |node| {
        if let Some(tag) = node.tag_name() {
            if tag == "style" {
                // Collect text content from children
                for child in &node.children {
                    if let Some(text) = child.text_content() {
                        css_content.push_str(text);
                        css_content.push('\n');
                    }
                }
            }
        }
    });

    // Parse CSS
    if css_content.is_empty() {
        Ok(css::StyleSheet { rules: Vec::new() })
    } else {
        css::parse_stylesheet(&css_content)
    }
}
