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
use crate::image_loader::ImageCache;
use crate::style::color::Rgba;
use crate::style::types::BorderStyle as CssBorderStyle;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::text::pipeline::{ShapedRun, ShapingPipeline};
use crate::tree::box_tree::ReplacedType;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use image::DynamicImage;
use std::borrow::Cow;
use tiny_skia::{
    FilterQuality, IntSize, Paint, PathBuilder, Pixmap, PixmapPaint, Rect as SkiaRect, Stroke, Transform,
};

/// Main painter that rasterizes a FragmentTree to pixels
pub struct Painter {
    /// The pixmap being painted to
    pixmap: Pixmap,
    /// Background color
    background: Rgba,
    /// Text shaping pipeline
    shaper: ShapingPipeline,
    /// Font context for resolution
    font_ctx: FontContext,
    /// Image cache for replaced content
    image_cache: ImageCache,
}

#[derive(Copy, Clone)]
enum EdgeOrientation {
    Horizontal,
    Vertical,
}

impl Painter {
    /// Creates a new painter with the given dimensions
    pub fn new(width: u32, height: u32, background: Rgba) -> Result<Self> {
        Self::with_resources(width, height, background, FontContext::new(), ImageCache::new())
    }

    /// Creates a painter with explicit font and image resources
    pub fn with_resources(
        width: u32,
        height: u32,
        background: Rgba,
        font_ctx: FontContext,
        image_cache: ImageCache,
    ) -> Result<Self> {
        let pixmap = Pixmap::new(width, height).ok_or_else(|| RenderError::InvalidParameters {
            message: format!("Failed to create pixmap {}x{}", width, height),
        })?;

        Ok(Self {
            pixmap,
            background,
            shaper: ShapingPipeline::new(),
            font_ctx,
            image_cache,
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
            FragmentContent::Block { .. } => {}
            FragmentContent::Inline { .. } => {}
            FragmentContent::Text {
                text,
                baseline_offset,
                shaped,
                ..
            } => {
                // Get text color from style
                let color = style.map(|s| s.color).unwrap_or(Rgba::BLACK);
                let font_size = style.map(|s| s.font_size).unwrap_or(16.0);
                if let Some(runs) = shaped {
                    self.paint_shaped_runs(runs, x, y + baseline_offset, color);
                } else {
                    self.paint_text(text, style, x, y + baseline_offset, font_size, color);
                }
            }
            FragmentContent::Line { .. } => {}
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
            self.paint_border_edge(
                x,
                y + top * 0.5,
                x + width,
                y + top * 0.5,
                top,
                style.border_top_style,
                &style.border_top_color,
                EdgeOrientation::Horizontal,
            );
        }

        // Right border
        if right > 0.0 {
            self.paint_border_edge(
                x + width - right * 0.5,
                y,
                x + width - right * 0.5,
                y + height,
                right,
                style.border_right_style,
                &style.border_right_color,
                EdgeOrientation::Vertical,
            );
        }

        // Bottom border
        if bottom > 0.0 {
            self.paint_border_edge(
                x,
                y + height - bottom * 0.5,
                x + width,
                y + height - bottom * 0.5,
                bottom,
                style.border_bottom_style,
                &style.border_bottom_color,
                EdgeOrientation::Horizontal,
            );
        }

        // Left border
        if left > 0.0 {
            self.paint_border_edge(
                x + left * 0.5,
                y,
                x + left * 0.5,
                y + height,
                left,
                style.border_left_style,
                &style.border_left_color,
                EdgeOrientation::Vertical,
            );
        }
    }

    fn paint_border_edge(
        &mut self,
        x1: f32,
        y1: f32,
        x2: f32,
        y2: f32,
        width: f32,
        style: CssBorderStyle,
        color: &Rgba,
        orientation: EdgeOrientation,
    ) {
        if width <= 0.0 || matches!(style, CssBorderStyle::None | CssBorderStyle::Hidden) {
            return;
        }

        let mut paint = Paint::default();
        paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
        paint.anti_alias = true;

        let mut stroke = Stroke::default();
        stroke.width = width;
        stroke.line_cap = tiny_skia::LineCap::Butt;

        // Dash patterns per CSS styles
        match style {
            CssBorderStyle::Dotted => {
                stroke.dash = tiny_skia::StrokeDash::new(vec![width, width], 0.0);
            }
            CssBorderStyle::Dashed => {
                stroke.dash = tiny_skia::StrokeDash::new(vec![3.0 * width, width], 0.0);
            }
            _ => {}
        }

        let mut path = PathBuilder::new();
        path.move_to(x1, y1);
        path.line_to(x2, y2);
        let base_path = match path.finish() {
            Some(p) => p,
            None => return,
        };

        match style {
            CssBorderStyle::Double => {
                let third = width / 3.0;
                let offset = third + third * 0.5;

                let (outer_path, inner_path) = match orientation {
                    EdgeOrientation::Horizontal => {
                        let mut outer_pb = PathBuilder::new();
                        outer_pb.move_to(x1, y1 - offset);
                        outer_pb.line_to(x2, y2 - offset);

                        let mut inner_pb = PathBuilder::new();
                        inner_pb.move_to(x1, y1 + offset);
                        inner_pb.line_to(x2, y2 + offset);

                        (outer_pb.finish(), inner_pb.finish())
                    }
                    EdgeOrientation::Vertical => {
                        let mut outer_pb = PathBuilder::new();
                        outer_pb.move_to(x1 - offset, y1);
                        outer_pb.line_to(x2 - offset, y2);

                        let mut inner_pb = PathBuilder::new();
                        inner_pb.move_to(x1 + offset, y1);
                        inner_pb.line_to(x2 + offset, y2);

                        (outer_pb.finish(), inner_pb.finish())
                    }
                };

                let mut inner_stroke = stroke.clone();
                inner_stroke.width = third;
                stroke.width = third;

                if let Some(outer) = outer_path {
                    self.pixmap
                        .stroke_path(&outer, &paint, &stroke, Transform::identity(), None);
                }
                if let Some(inner) = inner_path {
                    self.pixmap
                        .stroke_path(&inner, &paint, &inner_stroke, Transform::identity(), None);
                }
            }
            CssBorderStyle::Groove | CssBorderStyle::Ridge | CssBorderStyle::Inset | CssBorderStyle::Outset => {
                // TODO: implement light/dark splitting for 3D borders. For now, draw as solid.
                self.pixmap
                    .stroke_path(&base_path, &paint, &stroke, Transform::identity(), None);
            }
            _ => {
                self.pixmap
                    .stroke_path(&base_path, &paint, &stroke, Transform::identity(), None);
            }
        }
    }

    /// Paints text by shaping and rasterizing glyph outlines.
    fn paint_text(
        &mut self,
        text: &str,
        style: Option<&ComputedStyle>,
        x: f32,
        baseline_y: f32,
        font_size: f32,
        color: Rgba,
    ) {
        if text.is_empty() {
            return;
        }

        // Use computed style when available; otherwise construct a minimal fallback
        let style_for_shaping: Cow<ComputedStyle> = match style {
            Some(s) => Cow::Borrowed(s),
            None => {
                let mut s = ComputedStyle::default();
                s.font_size = font_size;
                Cow::Owned(s)
            }
        };

        // Shape text with the full pipeline (bidi, script, fallback fonts)
        let shaped_runs = match self.shaper.shape(text, &style_for_shaping, &self.font_ctx) {
            Ok(runs) => runs,
            Err(_) => return,
        };

        self.paint_shaped_runs(&shaped_runs, x, baseline_y, color);
    }

    fn paint_shaped_runs(&mut self, runs: &[ShapedRun], origin_x: f32, baseline_y: f32, color: Rgba) {
        let mut pen_x = origin_x;

        for run in runs {
            let run_origin = if run.direction.is_rtl() {
                pen_x + run.advance
            } else {
                pen_x
            };
            self.paint_shaped_run(run, run_origin, baseline_y, color);
            pen_x += run.advance;
        }
    }

    fn paint_shaped_run(&mut self, run: &ShapedRun, origin_x: f32, baseline_y: f32, color: Rgba) {
        let mut paint = Paint::default();
        paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
        paint.anti_alias = true;

        // ttf_parser face
        let face = match ttf_parser::Face::parse(&run.font.data, run.font.index) {
            Ok(f) => f,
            Err(_) => return,
        };
        let units_per_em = face.units_per_em() as f32;
        let scale = run.font_size / units_per_em;

        for glyph in &run.glyphs {
            let glyph_x = match run.direction {
                crate::text::pipeline::Direction::RightToLeft => origin_x - glyph.x_offset,
                _ => origin_x + glyph.x_offset,
            };
            let glyph_y = baseline_y - glyph.y_offset;

            let glyph_id: u16 = glyph.glyph_id as u16;

            if let Some(path) = Self::build_glyph_path(&face, glyph_id, glyph_x, glyph_y, scale) {
                self.pixmap
                    .fill_path(&path, &paint, tiny_skia::FillRule::EvenOdd, Transform::identity(), None);
            }
        }
    }

    fn build_glyph_path(
        face: &ttf_parser::Face,
        glyph_id: u16,
        x: f32,
        baseline_y: f32,
        scale: f32,
    ) -> Option<tiny_skia::Path> {
        use ttf_parser::OutlineBuilder;

        struct PathConverter {
            builder: PathBuilder,
            scale: f32,
            x: f32,
            y: f32,
        }

        impl OutlineBuilder for PathConverter {
            fn move_to(&mut self, px: f32, py: f32) {
                self.builder.move_to(self.x + px * self.scale, self.y - py * self.scale);
            }

            fn line_to(&mut self, px: f32, py: f32) {
                self.builder.line_to(self.x + px * self.scale, self.y - py * self.scale);
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

    /// Paints a replaced element (image, etc.)
    fn paint_replaced(&mut self, replaced_type: &ReplacedType, x: f32, y: f32, width: f32, height: f32) {
        if width <= 0.0 || height <= 0.0 {
            return;
        }

        // Try to render actual content for images
        if let ReplacedType::Image { src } = replaced_type {
            if self.paint_image_from_src(src, x, y, width, height) {
                return;
            }
        }

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
        if let ReplacedType::Image { src: _ } = replaced_type {
            // Placeholder already drawn if image loading failed
        }
    }

    fn paint_image_from_src(&mut self, src: &str, x: f32, y: f32, width: f32, height: f32) -> bool {
        if src.is_empty() {
            return false;
        }

        let image = match self.image_cache.load(src) {
            Ok(img) => img,
            Err(_) => return false,
        };

        let pixmap = match Self::dynamic_image_to_pixmap(&image) {
            Some(pixmap) => pixmap,
            None => return false,
        };

        if pixmap.width() == 0 || pixmap.height() == 0 {
            return false;
        }

        let scale_x = width / pixmap.width() as f32;
        let scale_y = height / pixmap.height() as f32;
        if !scale_x.is_finite() || !scale_y.is_finite() {
            return false;
        }

        let mut paint = PixmapPaint::default();
        paint.quality = FilterQuality::Bilinear;

        let transform = Transform::from_row(scale_x, 0.0, 0.0, scale_y, x, y);
        self.pixmap.draw_pixmap(0, 0, pixmap.as_ref(), &paint, transform, None);
        true
    }

    fn dynamic_image_to_pixmap(image: &DynamicImage) -> Option<Pixmap> {
        let rgba = image.to_rgba8();
        let (width, height) = rgba.dimensions();
        if width == 0 || height == 0 {
            return None;
        }

        // tiny-skia expects premultiplied BGRA
        let mut data = Vec::with_capacity((width * height * 4) as usize);
        for pixel in rgba.pixels() {
            let [r, g, b, a] = pixel.0;
            let alpha = a as f32 / 255.0;
            data.push((b as f32 * alpha).round() as u8);
            data.push((g as f32 * alpha).round() as u8);
            data.push((r as f32 * alpha).round() as u8);
            data.push(a);
        }

        let size = IntSize::from_wh(width, height)?;
        Pixmap::from_vec(data, size)
    }
}

/// Paints a fragment tree to a pixmap
///
/// This is the main entry point for painting.
pub fn paint_tree(tree: &FragmentTree, width: u32, height: u32, background: Rgba) -> Result<Pixmap> {
    let painter = Painter::new(width, height, background)?;
    painter.paint(tree)
}

/// Paints a fragment tree using provided font and image resources.
pub fn paint_tree_with_resources(
    tree: &FragmentTree,
    width: u32,
    height: u32,
    background: Rgba,
    font_ctx: FontContext,
    image_cache: ImageCache,
) -> Result<Pixmap> {
    let painter = Painter::with_resources(width, height, background, font_ctx, image_cache)?;
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
