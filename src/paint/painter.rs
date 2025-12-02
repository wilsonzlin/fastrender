//! Main painter - converts FragmentTree to pixels
//!
//! This module implements the core painting algorithm that transforms
//! the positioned fragment tree into rasterized pixels.
//!
//! # CSS Painting Order
//!
//! Follows CSS 2.1 Appendix E painting order:
//! 1. Background colors and images
//! 2. Borders
//! 3. Child stacking contexts (negative z-index)
//! 4. In-flow non-positioned blocks
//! 5. Floats
//! 6. In-flow inline content
//! 7. Child stacking contexts (z-index: 0 and auto)
//! 8. Positioned descendants (positive z-index)
//!
//! # Architecture
//!
//! The painter walks the fragment tree depth-first, painting each
//! fragment's background, borders, and content. Text is rendered
//! using the system's default font.

use crate::error::{RenderError, Result};
use crate::style::color::Rgba;
use crate::tree::box_tree::ReplacedType;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use tiny_skia::{Paint, PathBuilder, Pixmap, Rect as SkiaRect, Transform};

/// Main painter that rasterizes a FragmentTree to pixels
pub struct Painter {
    /// The pixmap being painted to
    pixmap: Pixmap,
    /// Background color
    background: Rgba,
}

impl Painter {
    /// Creates a new painter with the given dimensions
    pub fn new(width: u32, height: u32, background: Rgba) -> Result<Self> {
        let pixmap = Pixmap::new(width, height).ok_or_else(|| RenderError::InvalidParameters {
            message: format!("Failed to create pixmap {}x{}", width, height),
        })?;

        Ok(Self { pixmap, background })
    }

    /// Paints a fragment tree and returns the resulting pixmap
    pub fn paint(mut self, tree: &FragmentTree) -> Result<Pixmap> {
        // Fill background
        self.fill_background();

        // Paint the root fragment and all children
        self.paint_fragment(&tree.root, 0.0, 0.0);

        Ok(self.pixmap)
    }

    /// Fills the canvas with the background color
    fn fill_background(&mut self) {
        let color = tiny_skia::Color::from_rgba8(
            self.background.r,
            self.background.g,
            self.background.b,
            self.background.alpha_u8(),
        );
        self.pixmap.fill(color);
    }

    /// Paints a fragment and its children recursively
    fn paint_fragment(&mut self, fragment: &FragmentNode, offset_x: f32, offset_y: f32) {
        let x = fragment.bounds.x() + offset_x;
        let y = fragment.bounds.y() + offset_y;
        let width = fragment.bounds.width();
        let height = fragment.bounds.height();

        // Paint based on content type
        match &fragment.content {
            FragmentContent::Block { .. } => {
                // Block fragments may have backgrounds/borders
                // For now, just recurse into children
            }
            FragmentContent::Inline { .. } => {
                // Inline fragments may have backgrounds/borders
            }
            FragmentContent::Text {
                text, baseline_offset, ..
            } => {
                self.paint_text(text, x, y + baseline_offset, height);
            }
            FragmentContent::Line { .. } => {
                // Line boxes are containers, paint children
            }
            FragmentContent::Replaced { replaced_type, .. } => {
                self.paint_replaced(replaced_type, x, y, width, height);
            }
        }

        // Paint children (depth-first)
        for child in &fragment.children {
            self.paint_fragment(child, x, y);
        }
    }

    /// Paints text at the given position
    fn paint_text(&mut self, text: &str, x: f32, y: f32, _height: f32) {
        if text.is_empty() {
            return;
        }

        // Simple text rendering using tiny-skia's path-based approach
        // In production, this would use rustybuzz for shaping and
        // ttf-parser for glyph outlines

        // For now, render text as simple rectangles (placeholder)
        // Each character gets a small box
        let char_width = 8.0f32;
        let char_height = 14.0f32;

        let mut paint = Paint::default();
        paint.set_color_rgba8(0, 0, 0, 255); // Black text
        paint.anti_alias = true;

        for (i, _ch) in text.chars().enumerate() {
            let char_x = x + (i as f32 * char_width);

            // Skip if out of bounds
            if char_x >= self.pixmap.width() as f32 || y >= self.pixmap.height() as f32 {
                continue;
            }

            // Create a small rectangle for each character
            if let Some(rect) = SkiaRect::from_xywh(char_x, y, char_width * 0.8, char_height * 0.7) {
                let path = PathBuilder::from_rect(rect);
                self.pixmap
                    .fill_path(&path, &paint, tiny_skia::FillRule::Winding, Transform::identity(), None);
            }
        }
    }

    /// Paints a replaced element (image, etc.)
    fn paint_replaced(
        &mut self,
        replaced_type: &ReplacedType,
        x: f32,
        y: f32,
        width: f32,
        height: f32,
    ) {
        // For now, draw a placeholder rectangle
        let mut paint = Paint::default();
        paint.set_color_rgba8(200, 200, 200, 255); // Light gray
        paint.anti_alias = true;

        if let Some(rect) = SkiaRect::from_xywh(x, y, width, height) {
            let path = PathBuilder::from_rect(rect);
            self.pixmap
                .fill_path(&path, &paint, tiny_skia::FillRule::Winding, Transform::identity(), None);

            // Draw border
            let mut stroke_paint = Paint::default();
            stroke_paint.set_color_rgba8(150, 150, 150, 255);
            stroke_paint.anti_alias = true;

            let stroke = tiny_skia::Stroke {
                width: 1.0,
                ..Default::default()
            };
            self.pixmap
                .stroke_path(&path, &stroke_paint, &stroke, Transform::identity(), None);
        }

        // Would load and render actual image here for ReplacedType::Image
        if let ReplacedType::Image { src } = replaced_type {
            let _ = src; // Placeholder - would load image from src
        }
    }
}

/// Paints a fragment tree to a pixmap
///
/// This is the main entry point for painting.
pub fn paint_tree(tree: &FragmentTree, width: u32, height: u32, background: Rgba) -> Result<Pixmap> {
    let painter = Painter::new(width, height, background)?;
    painter.paint(tree)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::Rect;

    fn make_empty_tree() -> FragmentTree {
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![]);
        FragmentTree::new(root)
    }

    #[test]
    fn test_painter_creation() {
        let painter = Painter::new(100, 100, Rgba::WHITE);
        assert!(painter.is_ok());
    }

    #[test]
    fn test_paint_empty_tree() {
        let tree = make_empty_tree();
        let result = paint_tree(&tree, 100, 100, Rgba::WHITE);
        assert!(result.is_ok());

        let pixmap = result.unwrap();
        assert_eq!(pixmap.width(), 100);
        assert_eq!(pixmap.height(), 100);
    }

    #[test]
    fn test_paint_with_text() {
        let text_fragment = FragmentNode::new_text(Rect::from_xywh(10.0, 10.0, 50.0, 16.0), "Hello".to_string(), 12.0);
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![text_fragment]);
        let tree = FragmentTree::new(root);

        let result = paint_tree(&tree, 100, 100, Rgba::WHITE);
        assert!(result.is_ok());
    }

    #[test]
    fn test_paint_nested_fragments() {
        let inner = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 30.0, 30.0), vec![]);
        let outer = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 50.0), vec![inner]);
        let tree = FragmentTree::new(outer);

        let result = paint_tree(&tree, 100, 100, Rgba::WHITE);
        assert!(result.is_ok());
    }

    #[test]
    fn test_background_white() {
        let tree = make_empty_tree();
        let result = paint_tree(&tree, 10, 10, Rgba::WHITE);
        assert!(result.is_ok());

        let pixmap = result.unwrap();
        let data = pixmap.data();
        // tiny-skia uses BGRA premultiplied format
        // WHITE in BGRA is (255, 255, 255, 255)
        assert_eq!(data[0], 255); // B
        assert_eq!(data[1], 255); // G
        assert_eq!(data[2], 255); // R
        assert_eq!(data[3], 255); // A
    }
}
