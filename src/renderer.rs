//! HTML to image renderer
//!
//! This module provides the main entry point for rendering HTML/CSS to images.
//!
//! # Pipeline
//!
//! The rendering pipeline consists of:
//! 1. **Parse**: HTML string → DOM tree
//! 2. **Style**: DOM + CSS → Styled tree
//! 3. **Box Generation**: Styled tree → Box tree
//! 4. **Layout**: Box tree → Fragment tree
//! 5. **Paint**: Fragment tree → Pixmap
//! 6. **Encode**: Pixmap → PNG/JPEG/WebP

use crate::css::{self, Color};
use crate::dom::{self, DomNode};
use crate::error::{Error, Result};
use crate::geometry::Size;
use crate::image_output;
use crate::layout::{LayoutConfig, LayoutEngine};
use crate::paint::paint_tree;
use crate::style::{self, ComputedStyles, Display, StyledNode};
use crate::tree::box_tree::{BoxNode, BoxTree, BoxType, ReplacedBox, ReplacedType, TextBox};
use crate::tree::fragment_tree::FragmentTree;
use std::sync::Arc;

pub use crate::image_output::OutputFormat as ImageFormat;

/// Main renderer for converting HTML to images
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

    pub fn render_to_png_auto_height(&self, html: &str, width: u32) -> Result<Vec<u8>> {
        // Parse and style
        let dom = dom::parse_html(html)?;
        let stylesheet = extract_css(&dom)?;
        let styled_tree = style::apply_styles(&dom, &stylesheet);

        // Generate boxes
        let box_tree = generate_box_tree(&styled_tree);

        // Layout with large height to measure content
        let config = LayoutConfig::for_viewport(Size::new(width as f32, 100000.0));
        let engine = LayoutEngine::new(config);
        let fragment_tree = engine.layout_tree(&box_tree).map_err(|e| {
            Error::Render(crate::error::RenderError::InvalidParameters {
                message: format!("Layout failed: {:?}", e),
            })
        })?;

        // Calculate actual content height
        let height = calculate_content_height(&fragment_tree).max(100.0) as u32;

        // Re-layout with actual height and paint
        let config = LayoutConfig::for_viewport(Size::new(width as f32, height as f32));
        let engine = LayoutEngine::new(config);
        let fragment_tree = engine.layout_tree(&box_tree).map_err(|e| {
            Error::Render(crate::error::RenderError::InvalidParameters {
                message: format!("Layout failed: {:?}", e),
            })
        })?;

        let pixmap = paint_tree(&fragment_tree, width, height, self.background_color)?;
        image_output::encode_image(&pixmap, ImageFormat::Png)
    }

    pub fn render_to_png_with_scroll(&self, html: &str, width: u32, height: u32, _scroll_y: u32) -> Result<Vec<u8>> {
        // For now, ignore scroll_y (would need to adjust fragment positions)
        self.render_to_png(html, width, height)
    }

    pub fn render_to_png_with_scroll_and_base_url(
        &self,
        html: &str,
        width: u32,
        height: u32,
        _scroll_y: u32,
        _base_url: String,
    ) -> Result<Vec<u8>> {
        // For now, ignore scroll and base_url
        self.render_to_png(html, width, height)
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
            return Err(Error::Render(crate::error::RenderError::InvalidParameters {
                message: format!("Invalid dimensions: width={}, height={}", width, height),
            }));
        }

        // Parse HTML
        let dom = dom::parse_html(html)?;

        // Extract and parse CSS
        let stylesheet = extract_css(&dom)?;

        // Apply styles
        let styled_tree = style::apply_styles(&dom, &stylesheet);

        // Generate box tree
        let box_tree = generate_box_tree(&styled_tree);

        // Layout
        let config = LayoutConfig::for_viewport(Size::new(width as f32, height as f32));
        let engine = LayoutEngine::new(config);
        let fragment_tree = engine.layout_tree(&box_tree).map_err(|e| {
            Error::Render(crate::error::RenderError::InvalidParameters {
                message: format!("Layout failed: {:?}", e),
            })
        })?;

        // Paint
        let pixmap = paint_tree(&fragment_tree, width, height, options.background_color)?;

        // Encode
        image_output::encode_image(&pixmap, options.format)
    }
}

impl Default for Renderer {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for creating Renderer instances
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

// =============================================================================
// Box Generation
// =============================================================================

/// Generates a BoxTree from a StyledNode tree
fn generate_box_tree(styled: &StyledNode) -> BoxTree {
    let root = generate_box_node(styled);
    BoxTree { root }
}

/// Recursively generates BoxNode from StyledNode
fn generate_box_node(styled: &StyledNode) -> BoxNode {
    let style = Arc::new(styled.styles.clone());

    // Check if this is display: none
    if styled.styles.display == Display::None {
        // Create an empty block (will have no effect on layout)
        return BoxNode::new_block(style, crate::style::display::FormattingContextType::Block, vec![]);
    }

    // Check for text node
    if let Some(text) = styled.node.text_content() {
        if !text.trim().is_empty() {
            return BoxNode::new_text(style, text.to_string());
        }
    }

    // Check for replaced elements (images, etc.)
    if let Some(tag) = styled.node.tag_name() {
        if is_replaced_element(tag) {
            return create_replaced_box(styled, style);
        }
    }

    // Generate children
    let children: Vec<BoxNode> = styled.children.iter().map(generate_box_node).collect();

    // Determine box type based on display
    use crate::style::display::FormattingContextType;
    let fc_type = styled
        .styles
        .display
        .formatting_context_type()
        .unwrap_or(FormattingContextType::Block);

    match styled.styles.display {
        Display::Block | Display::Flex | Display::Grid | Display::Table => BoxNode::new_block(style, fc_type, children),
        Display::Inline => BoxNode::new_inline(style, children),
        Display::InlineBlock => BoxNode::new_inline_block(style, fc_type, children),
        Display::None => BoxNode::new_block(style, FormattingContextType::Block, vec![]),
        _ => BoxNode::new_block(style, fc_type, children),
    }
}

/// Checks if an element is a replaced element
fn is_replaced_element(tag: &str) -> bool {
    matches!(
        tag.to_lowercase().as_str(),
        "img" | "video" | "canvas" | "svg" | "iframe" | "embed" | "object"
    )
}

/// Creates a BoxNode for a replaced element
fn create_replaced_box(styled: &StyledNode, style: Arc<ComputedStyles>) -> BoxNode {
    let tag = styled.node.tag_name().unwrap_or("img");

    // Get src attribute if available
    let src = styled.node.get_attribute("src").unwrap_or_default();

    // Determine replaced type
    let replaced_type = match tag.to_lowercase().as_str() {
        "img" => ReplacedType::Image { src },
        "video" => ReplacedType::Video { src },
        "canvas" => ReplacedType::Canvas,
        "svg" => ReplacedType::Svg { content: String::new() },
        "iframe" => ReplacedType::Iframe { src },
        _ => ReplacedType::Image { src },
    };

    // Get intrinsic size from attributes or use default
    let intrinsic_width = styled
        .node
        .get_attribute("width")
        .and_then(|w| w.parse::<f32>().ok())
        .unwrap_or(300.0);

    let intrinsic_height = styled
        .node
        .get_attribute("height")
        .and_then(|h| h.parse::<f32>().ok())
        .unwrap_or(150.0);

    let replaced_box = ReplacedBox {
        replaced_type,
        intrinsic_size: Some(Size::new(intrinsic_width, intrinsic_height)),
        aspect_ratio: Some(intrinsic_width / intrinsic_height),
    };

    BoxNode {
        box_type: BoxType::Replaced(replaced_box),
        style,
        children: vec![],
        debug_info: None,
    }
}

// =============================================================================
// CSS Extraction
// =============================================================================

/// Extracts CSS from style tags in the DOM
fn extract_css(dom: &DomNode) -> Result<css::StyleSheet> {
    let mut css_content = String::new();

    dom.walk_tree(&mut |node| {
        if let Some(tag) = node.tag_name() {
            if tag == "style" {
                for child in &node.children {
                    if let Some(text) = child.text_content() {
                        css_content.push_str(text);
                        css_content.push('\n');
                    }
                }
            }
        }
    });

    if css_content.is_empty() {
        Ok(css::StyleSheet { rules: Vec::new() })
    } else {
        css::parse_stylesheet(&css_content)
    }
}

// =============================================================================
// Utilities
// =============================================================================

/// Calculates the content height from a fragment tree
fn calculate_content_height(tree: &FragmentTree) -> f32 {
    calculate_fragment_bottom(&tree.root)
}

/// Recursively finds the maximum bottom coordinate
fn calculate_fragment_bottom(fragment: &crate::tree::fragment_tree::FragmentNode) -> f32 {
    let own_bottom = fragment.bounds.y() + fragment.bounds.height();

    let children_bottom = fragment
        .children
        .iter()
        .map(calculate_fragment_bottom)
        .fold(own_bottom, f32::max);

    children_bottom
}
