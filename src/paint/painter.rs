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
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::tree::box_tree::ReplacedType;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use tiny_skia::{Paint, PathBuilder, Pixmap, Rect as SkiaRect, Transform};

/// Main painter that rasterizes a FragmentTree to pixels
pub struct Painter {
    /// The pixmap being painted to
    pixmap: Pixmap,
    /// Background color
    background: Rgba,
    /// Font context for text rendering
    font_ctx: FontContext,
}

impl Painter {
    /// Creates a new painter with the given dimensions
    pub fn new(width: u32, height: u32, background: Rgba) -> Result<Self> {
        let pixmap = Pixmap::new(width, height).ok_or_else(|| RenderError::InvalidParameters {
            message: format!("Failed to create pixmap {}x{}", width, height),
        })?;

        Ok(Self { 
            pixmap, 
            background,
            font_ctx: FontContext::new(),
        })
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

        // Get style if available
        let style = fragment.get_style();

        // Paint background first (if present)
        if let Some(style) = style {
            self.paint_background(x, y, width, height, style);
            self.paint_borders(x, y, width, height, style);
        }

        // Paint based on content type
        match &fragment.content {
            FragmentContent::Block { .. } => {
                // Background/borders already painted above
            }
            FragmentContent::Inline { .. } => {
                // Inline fragments may have backgrounds/borders (already painted)
            }
            FragmentContent::Text {
                text, baseline_offset, ..
            } => {
                // Get text color from style
                let color = style.map(|s| s.color).unwrap_or(Rgba::BLACK);
                let font_size = style.map(|s| s.font_size).unwrap_or(16.0);
                self.paint_text(text, x, y + baseline_offset, font_size, color);
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

    /// Paints the background of a fragment
    fn paint_background(&mut self, x: f32, y: f32, width: f32, height: f32, style: &ComputedStyle) {
        // Skip if background is transparent
        if style.background_color.alpha_u8() == 0 {
            return;
        }

        let mut paint = Paint::default();
        paint.set_color_rgba8(
            style.background_color.r,
            style.background_color.g,
            style.background_color.b,
            style.background_color.alpha_u8(),
        );
        paint.anti_alias = true;

        if let Some(rect) = SkiaRect::from_xywh(x, y, width, height) {
            let path = PathBuilder::from_rect(rect);
            self.pixmap
                .fill_path(&path, &paint, tiny_skia::FillRule::Winding, Transform::identity(), None);
        }
    }

    /// Paints the borders of a fragment
    fn paint_borders(&mut self, x: f32, y: f32, width: f32, height: f32, style: &ComputedStyle) {
        // Only paint if there are borders
        let top = style.border_top_width.to_px();
        let right = style.border_right_width.to_px();
        let bottom = style.border_bottom_width.to_px();
        let left = style.border_left_width.to_px();

        if top <= 0.0 && right <= 0.0 && bottom <= 0.0 && left <= 0.0 {
            return;
        }

        // Top border
        if top > 0.0 {
            let mut paint = Paint::default();
            let color = &style.border_top_color;
            paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
            paint.anti_alias = true;
            if let Some(rect) = SkiaRect::from_xywh(x, y, width, top) {
                let path = PathBuilder::from_rect(rect);
                self.pixmap.fill_path(&path, &paint, tiny_skia::FillRule::Winding, Transform::identity(), None);
            }
        }

        // Right border
        if right > 0.0 {
            let mut paint = Paint::default();
            let color = &style.border_right_color;
            paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
            paint.anti_alias = true;
            if let Some(rect) = SkiaRect::from_xywh(x + width - right, y, right, height) {
                let path = PathBuilder::from_rect(rect);
                self.pixmap.fill_path(&path, &paint, tiny_skia::FillRule::Winding, Transform::identity(), None);
            }
        }

        // Bottom border
        if bottom > 0.0 {
            let mut paint = Paint::default();
            let color = &style.border_bottom_color;
            paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
            paint.anti_alias = true;
            if let Some(rect) = SkiaRect::from_xywh(x, y + height - bottom, width, bottom) {
                let path = PathBuilder::from_rect(rect);
                self.pixmap.fill_path(&path, &paint, tiny_skia::FillRule::Winding, Transform::identity(), None);
            }
        }

        // Left border
        if left > 0.0 {
            let mut paint = Paint::default();
            let color = &style.border_left_color;
            paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
            paint.anti_alias = true;
            if let Some(rect) = SkiaRect::from_xywh(x, y, left, height) {
                let path = PathBuilder::from_rect(rect);
                self.pixmap.fill_path(&path, &paint, tiny_skia::FillRule::Winding, Transform::identity(), None);
            }
        }
    }

    /// Paints text at the given position
    fn paint_text(&mut self, text: &str, x: f32, y: f32, font_size: f32, color: Rgba) {
        if text.is_empty() {
            return;
        }

        // Try to get a sans-serif font
        let font = match self.font_ctx.get_sans_serif() {
            Some(f) => f,
            None => {
                // Try DejaVu Sans directly
                let families = vec!["DejaVu Sans".to_string(), "sans-serif".to_string()];
                match self.font_ctx.get_font(&families, 400, false, false) {
                    Some(f) => f,
                    None => {
                        // Fallback to placeholder rectangles if no font available
                        self.paint_text_placeholder(text, x, y, font_size, color);
                        return;
                    }
                }
            }
        };

        // Render text using glyph outlines
        let face = match ttf_parser::Face::parse(&font.data, font.index) {
            Ok(f) => f,
            Err(_) => {
                self.paint_text_placeholder(text, x, y, font_size, color);
                return;
            }
        };

        let units_per_em = face.units_per_em() as f32;
        let scale = font_size / units_per_em;

        let mut paint = Paint::default();
        paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
        paint.anti_alias = true;

        let mut cursor_x = x;
        // Position baseline below the top (ascent is typically ~80% of em)
        let baseline_y = y + font_size * 0.8;

        for ch in text.chars() {
            // Get glyph ID for this character
            let glyph_id = face.glyph_index(ch).map(|id| id.0 as u32).unwrap_or(0);
            
            // Get horizontal advance
            let advance = face
                .glyph_hor_advance(ttf_parser::GlyphId(glyph_id as u16))
                .unwrap_or(0) as f32
                * scale;

            // Build path for the glyph
            if let Some(path) = self.build_glyph_path(&face, glyph_id as u16, cursor_x, baseline_y, scale) {
                self.pixmap
                    .fill_path(&path, &paint, tiny_skia::FillRule::EvenOdd, Transform::identity(), None);
            }

            cursor_x += advance;
            
            // Skip if out of bounds
            if cursor_x >= self.pixmap.width() as f32 {
                break;
            }
        }
    }

    /// Builds a tiny-skia path from a glyph outline
    fn build_glyph_path(&self, face: &ttf_parser::Face, glyph_id: u16, x: f32, baseline_y: f32, scale: f32) -> Option<tiny_skia::Path> {
        use ttf_parser::OutlineBuilder;
        
        struct PathConverter {
            builder: PathBuilder,
            scale: f32,
            x: f32,
            y: f32, // baseline position
        }

        impl OutlineBuilder for PathConverter {
            fn move_to(&mut self, px: f32, py: f32) {
                self.builder.move_to(
                    self.x + px * self.scale,
                    self.y - py * self.scale, // Flip Y axis
                );
            }

            fn line_to(&mut self, px: f32, py: f32) {
                self.builder.line_to(
                    self.x + px * self.scale,
                    self.y - py * self.scale,
                );
            }

            fn quad_to(&mut self, x1: f32, y1: f32, px: f32, py: f32) {
                self.builder.quad_to(
                    self.x + x1 * self.scale,
                    self.y - y1 * self.scale,
                    self.x + px * self.scale,
                    self.y - py * self.scale,
                );
            }

            fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, px: f32, py: f32) {
                self.builder.cubic_to(
                    self.x + x1 * self.scale,
                    self.y - y1 * self.scale,
                    self.x + x2 * self.scale,
                    self.y - y2 * self.scale,
                    self.x + px * self.scale,
                    self.y - py * self.scale,
                );
            }

            fn close(&mut self) {
                self.builder.close();
            }
        }

        let mut converter = PathConverter {
            builder: PathBuilder::new(),
            scale,
            x,
            y: baseline_y,
        };

        face.outline_glyph(ttf_parser::GlyphId(glyph_id), &mut converter)?;
        converter.builder.finish()
    }

    /// Fallback text rendering with placeholder rectangles
    fn paint_text_placeholder(&mut self, text: &str, x: f32, y: f32, font_size: f32, color: Rgba) {
        let char_width = font_size * 0.6;
        let char_height = font_size * 0.8;

        let mut paint = Paint::default();
        paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
        paint.anti_alias = true;

        for (i, _ch) in text.chars().enumerate() {
            let char_x = x + (i as f32 * char_width);

            if char_x >= self.pixmap.width() as f32 || y >= self.pixmap.height() as f32 {
                continue;
            }

            if let Some(rect) = SkiaRect::from_xywh(char_x, y, char_width * 0.8, char_height) {
                let path = PathBuilder::from_rect(rect);
                self.pixmap
                    .fill_path(&path, &paint, tiny_skia::FillRule::Winding, Transform::identity(), None);
            }
        }
    }

    /// Paints a replaced element (image, etc.)
    fn paint_replaced(&mut self, replaced_type: &ReplacedType, x: f32, y: f32, width: f32, height: f32) {
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
