//! Display List Builder - Converts Fragment Tree to Display List
//!
//! This module builds a display list from the fragment tree by traversing
//! fragments and emitting paint commands in correct CSS paint order.
//!
//! # Pipeline
//!
//! ```text
//! Fragment Tree → Display List Builder → Display List → Rasterizer → Pixels
//! ```
//!
//! # Paint Order (CSS 2.1 Appendix E)
//!
//! For each fragment:
//! 1. Background color
//! 2. Background image
//! 3. Border
//! 4. Children (recursively)
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::paint::{DisplayListBuilder, DisplayList};
//!
//! let builder = DisplayListBuilder::new();
//! let display_list = builder.build(&fragment_tree.root);
//! ```

use crate::geometry::{Point, Rect};
use crate::image_loader::ImageCache;
use crate::layout::contexts::inline::baseline::compute_line_height_with_metrics;
use crate::layout::contexts::inline::line_builder::TextItem as InlineTextItem;
use crate::paint::display_list::{
    ClipItem, DisplayItem, DisplayList, FillRectItem, FontId, GlyphInstance, ImageData, ImageItem, OpacityItem,
    StrokeRectItem, TextItem,
};
use crate::paint::object_fit::{compute_object_fit, default_object_position};
use crate::paint::stacking::StackingContext;
use crate::style::color::Rgba;
use crate::style::types::ObjectFit;
use crate::style::ComputedStyle;
use crate::text::font_db::{FontStretch, FontStyle, ScaledMetrics};
use crate::text::font_loader::FontContext;
use crate::text::pipeline::{ShapedRun, ShapingPipeline};
use crate::tree::box_tree::ReplacedType;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use image::GenericImageView;
use std::collections::HashSet;
use std::sync::Arc;

/// Builder that converts a fragment tree to a display list
///
/// Walks the fragment tree depth-first, emitting display items
/// for backgrounds, borders, and content in correct CSS paint order.
pub struct DisplayListBuilder {
    /// The display list being built
    list: DisplayList,
    image_cache: Option<ImageCache>,
    viewport: Option<(f32, f32)>,
    font_ctx: FontContext,
    shaper: ShapingPipeline,
}

impl DisplayListBuilder {
    fn resolve_scaled_metrics(style: &ComputedStyle, font_ctx: &FontContext) -> Option<ScaledMetrics> {
        let italic = matches!(style.font_style, crate::style::types::FontStyle::Italic);
        let oblique = matches!(style.font_style, crate::style::types::FontStyle::Oblique(_));
        let stretch = FontStretch::from_percentage(style.font_stretch.to_percentage());

        font_ctx
            .get_font_full(
                &style.font_family,
                style.font_weight.to_u16(),
                if italic {
                    FontStyle::Italic
                } else if oblique {
                    FontStyle::Oblique
                } else {
                    FontStyle::Normal
                },
                stretch,
            )
            .or_else(|| font_ctx.get_sans_serif())
            .and_then(|font| font.metrics().ok())
            .map(|m| m.scale(style.font_size))
    }

    /// Creates a new display list builder
    pub fn new() -> Self {
        Self {
            list: DisplayList::new(),
            image_cache: None,
            viewport: None,
            font_ctx: FontContext::new(),
            shaper: ShapingPipeline::new(),
        }
    }

    /// Creates a display list builder backed by an image cache to rasterize replaced images.
    pub fn with_image_cache(image_cache: ImageCache) -> Self {
        Self {
            list: DisplayList::new(),
            image_cache: Some(image_cache),
            viewport: None,
            font_ctx: FontContext::new(),
            shaper: ShapingPipeline::new(),
        }
    }

    /// Sets the font context for shaping text into the display list.
    pub fn with_font_context(mut self, font_ctx: FontContext) -> Self {
        self.font_ctx = font_ctx;
        self
    }

    /// Sets the viewport size for resolving viewport-relative units (vw/vh) in object-position.
    pub fn with_viewport_size(mut self, width: f32, height: f32) -> Self {
        self.viewport = Some((width, height));
        self
    }

    /// Builds a display list from a fragment tree root
    pub fn build(mut self, root: &FragmentNode) -> DisplayList {
        self.build_fragment(root, Point::ZERO);
        self.list
    }

    /// Builds a display list from a FragmentTree
    pub fn build_tree(mut self, tree: &FragmentTree) -> DisplayList {
        self.build_fragment(&tree.root, Point::ZERO);
        self.list
    }

    /// Builds a display list from a stacking context tree (respecting z-order).
    pub fn build_from_stacking(mut self, stacking: &StackingContext) -> DisplayList {
        self.build_stacking_context(stacking, Point::ZERO);
        self.list
    }

    /// Builds a display list by first constructing a stacking context tree from the fragment tree.
    pub fn build_with_stacking_tree(mut self, root: &FragmentNode) -> DisplayList {
        let stacking = crate::paint::stacking::build_stacking_tree_from_fragment_tree(root);
        self.build_stacking_context(&stacking, Point::ZERO);
        self.list
    }

    /// Builds a display list with clipping support
    ///
    /// Fragments with box_ids in the `clips` set will have clipping applied.
    pub fn build_with_clips(mut self, root: &FragmentNode, clips: &HashSet<Option<usize>>) -> DisplayList {
        self.build_fragment_with_clips(root, Point::ZERO, clips);
        self.list
    }

    /// Recursively builds display items for a fragment
    fn build_fragment(&mut self, fragment: &FragmentNode, offset: Point) {
        self.build_fragment_internal(fragment, offset, true);
    }

    /// Builds display items for a fragment without descending into children.
    fn build_fragment_shallow(&mut self, fragment: &FragmentNode, offset: Point) {
        self.build_fragment_internal(fragment, offset, false);
    }

    fn build_fragment_internal(&mut self, fragment: &FragmentNode, offset: Point, recurse_children: bool) {
        let absolute_rect = Rect::new(
            Point::new(fragment.bounds.origin.x + offset.x, fragment.bounds.origin.y + offset.y),
            fragment.bounds.size,
        );

        // CSS Paint Order:
        // 1. Background (handled by caller if style available)
        // 2. Border (handled by caller if style available)
        // 3. Content (text, images)
        // 4. Children

        self.emit_content(fragment, absolute_rect);

        if recurse_children {
            let child_offset = absolute_rect.origin;
            for child in &fragment.children {
                self.build_fragment_internal(child, child_offset, true);
            }
        }
    }

    /// Recursively builds display items with clipping support
    fn build_fragment_with_clips(&mut self, fragment: &FragmentNode, offset: Point, clips: &HashSet<Option<usize>>) {
        let absolute_rect = Rect::new(
            Point::new(fragment.bounds.origin.x + offset.x, fragment.bounds.origin.y + offset.y),
            fragment.bounds.size,
        );

        let box_id = Self::get_box_id(fragment);
        let should_clip = clips.contains(&box_id);

        // Emit content before clipping children
        self.emit_content(fragment, absolute_rect);

        // Push clip if needed
        if should_clip {
            self.list.push(DisplayItem::PushClip(ClipItem {
                rect: absolute_rect,
                radii: None,
            }));
        }

        // Recurse to children
        let child_offset = absolute_rect.origin;
        for child in &fragment.children {
            self.build_fragment_with_clips(child, child_offset, clips);
        }

        // Pop clip
        if should_clip {
            self.list.push(DisplayItem::PopClip);
        }
    }

    fn build_stacking_context(&mut self, context: &StackingContext, offset: Point) {
        let mut children: Vec<&StackingContext> = context.children.iter().collect();
        children.sort_by(|a, b| a.z_index.cmp(&b.z_index).then_with(|| a.tree_order.cmp(&b.tree_order)));

        let (neg, non_neg): (Vec<_>, Vec<_>) = children.into_iter().partition(|c| c.z_index < 0);
        let (zero, pos): (Vec<_>, Vec<_>) = non_neg.into_iter().partition(|c| c.z_index == 0);

        let context_origin = context
            .fragments
            .first()
            .map(|f| f.bounds.origin)
            .unwrap_or(Point::ZERO);
        let descendant_offset = Point::new(offset.x + context_origin.x, offset.y + context_origin.y);

        for child in neg {
            self.build_stacking_context(child, descendant_offset);
        }

        self.emit_fragment_list_shallow(&context.fragments, offset);
        self.emit_fragment_list(&context.layer3_blocks, descendant_offset);
        self.emit_fragment_list(&context.layer4_floats, descendant_offset);
        self.emit_fragment_list(&context.layer5_inlines, descendant_offset);
        self.emit_fragment_list(&context.layer6_positioned, descendant_offset);

        for child in zero {
            self.build_stacking_context(child, descendant_offset);
        }

        for child in pos {
            self.build_stacking_context(child, descendant_offset);
        }
    }

    fn emit_fragment_list(&mut self, fragments: &[FragmentNode], offset: Point) {
        for fragment in fragments {
            self.build_fragment(fragment, offset);
        }
    }

    fn emit_fragment_list_shallow(&mut self, fragments: &[FragmentNode], offset: Point) {
        for fragment in fragments {
            self.build_fragment_shallow(fragment, offset);
        }
    }

    /// Emits display items for fragment content
    fn emit_content(&mut self, fragment: &FragmentNode, rect: Rect) {
        match &fragment.content {
            FragmentContent::Text {
                text,
                baseline_offset,
                shaped,
                ..
            } => {
                if text.is_empty() {
                    return;
                }

                let color = fragment.style.as_deref().map(|s| s.color).unwrap_or(Rgba::BLACK);
                let baseline = rect.origin.y + baseline_offset;
                let start_x = rect.origin.x;

                if let Some(runs) = shaped {
                    self.emit_shaped_runs(runs, color, baseline, start_x);
                    return;
                }

                if let Some(style) = fragment.style.as_deref() {
                    if let Ok(mut runs) = self.shaper.shape(text, style, &self.font_ctx) {
                        InlineTextItem::apply_spacing_to_runs(
                            &mut runs,
                            text,
                            style.letter_spacing,
                            style.word_spacing,
                        );
                        self.emit_shaped_runs(&runs, color, baseline, start_x);
                        return;
                    }
                }

                // Fallback: naive glyphs when shaping fails or no style is present
                let font_size = fragment.style.as_deref().map(|s| s.font_size).unwrap_or(16.0);
                let char_width = font_size * 0.6;
                let glyphs: Vec<GlyphInstance> = text
                    .chars()
                    .enumerate()
                    .map(|(i, _c)| GlyphInstance {
                        glyph_id: i as u32,
                        offset: Point::new(i as f32 * char_width, 0.0),
                        advance: char_width,
                    })
                    .collect();
                let advance_width = text.len() as f32 * char_width;

                self.list.push(DisplayItem::Text(TextItem {
                    origin: Point::new(start_x, baseline),
                    glyphs,
                    color,
                    font_size,
                    advance_width,
                    font_id: None,
                }));
            }

            FragmentContent::Replaced { replaced_type, .. } => {
                let source = match replaced_type {
                    ReplacedType::Image { src, .. } => src.as_str(),
                    ReplacedType::Svg { content } => content.as_str(),
                    ReplacedType::Embed { src } => src.as_str(),
                    ReplacedType::Object { data } => data.as_str(),
                    ReplacedType::Iframe { src } | ReplacedType::Video { src } => src.as_str(),
                    _ => "",
                };
                let image = self.decode_image(source);
                if image.is_none() {
                    if let ReplacedType::Image { alt: Some(alt), .. } = replaced_type {
                        if self.emit_alt_text(alt, fragment, rect) {
                            return;
                        }
                    }
                }

                let image = image.unwrap_or_else(|| ImageData::new(1, 1, vec![128, 128, 128, 255]));
                let (dest_x, dest_y, dest_w, dest_h) = {
                    let (fit, position, font_size) = if let Some(style) = fragment.style.as_deref() {
                        (style.object_fit, style.object_position, style.font_size)
                    } else {
                        (ObjectFit::Fill, default_object_position(), 16.0)
                    };

                    compute_object_fit(
                        fit,
                        position,
                        rect.width(),
                        rect.height(),
                        image.width as f32,
                        image.height as f32,
                        font_size,
                        self.viewport,
                    )
                    .unwrap_or((0.0, 0.0, rect.width(), rect.height()))
                };

                let dest_rect = Rect::from_xywh(rect.x() + dest_x, rect.y() + dest_y, dest_w, dest_h);
                self.list.push(DisplayItem::Image(ImageItem {
                    dest_rect,
                    image: Arc::new(image),
                    src_rect: None,
                }));
            }

            // Block, Inline, Line, other replaced types - no direct content
            _ => {}
        }
    }

    /// Gets the box_id from a fragment
    fn get_box_id(fragment: &FragmentNode) -> Option<usize> {
        match &fragment.content {
            FragmentContent::Block { box_id } => *box_id,
            FragmentContent::Inline { box_id, .. } => *box_id,
            FragmentContent::Text { box_id, .. } => *box_id,
            FragmentContent::Replaced { box_id, .. } => *box_id,
            FragmentContent::Line { .. } => None,
        }
    }

    /// Emits a background fill for a fragment
    pub fn emit_background(&mut self, rect: Rect, color: Rgba) {
        if !color.is_transparent() {
            self.list.push(DisplayItem::FillRect(FillRectItem { rect, color }));
        }
    }

    /// Emits border strokes for a fragment
    pub fn emit_border(&mut self, rect: Rect, width: f32, color: Rgba) {
        if width > 0.0 && !color.is_transparent() {
            self.list
                .push(DisplayItem::StrokeRect(StrokeRectItem { rect, color, width }));
        }
    }

    /// Begins an opacity layer
    pub fn push_opacity(&mut self, opacity: f32) {
        self.list.push(DisplayItem::PushOpacity(OpacityItem { opacity }));
    }

    /// Ends an opacity layer
    pub fn pop_opacity(&mut self) {
        self.list.push(DisplayItem::PopOpacity);
    }

    /// Begins a clip region
    pub fn push_clip(&mut self, rect: Rect) {
        self.list.push(DisplayItem::PushClip(ClipItem { rect, radii: None }));
    }

    /// Ends a clip region
    pub fn pop_clip(&mut self) {
        self.list.push(DisplayItem::PopClip);
    }

    fn emit_shaped_runs(&mut self, runs: &[ShapedRun], color: Rgba, baseline_y: f32, start_x: f32) {
        let mut pen_x = start_x;
        for run in runs {
            let origin_x = if run.direction.is_rtl() {
                pen_x + run.advance
            } else {
                pen_x
            };
            let glyphs = self.glyphs_from_run(run, origin_x, baseline_y);
            let font_id = self.font_id_from_run(run);

            self.list.push(DisplayItem::Text(TextItem {
                origin: Point::new(origin_x, baseline_y),
                glyphs,
                color,
                font_size: run.font_size,
                advance_width: run.advance,
                font_id: Some(font_id),
            }));

            pen_x += run.advance;
        }
    }

    fn glyphs_from_run(&self, run: &ShapedRun, origin_x: f32, baseline_y: f32) -> Vec<GlyphInstance> {
        let mut glyphs = Vec::with_capacity(run.glyphs.len());

        for glyph in &run.glyphs {
            let x = match run.direction {
                crate::text::pipeline::Direction::RightToLeft => origin_x - glyph.x_offset,
                _ => origin_x + glyph.x_offset,
            };
            let y = baseline_y - glyph.y_offset;
            glyphs.push(GlyphInstance {
                glyph_id: glyph.glyph_id,
                offset: Point::new(x - origin_x, y - baseline_y),
                advance: glyph.x_advance,
            });
        }

        glyphs
    }

    fn font_id_from_run(&self, run: &ShapedRun) -> FontId {
        FontId {
            family: run.font.family.clone(),
            weight: run.font.weight.value(),
            style: run.font.style,
            stretch: run.font.stretch,
        }
    }

    fn emit_alt_text(&mut self, alt: &str, fragment: &FragmentNode, rect: Rect) -> bool {
        let text = alt.trim();
        if text.is_empty() {
            return false;
        }

        let Some(style) = fragment.style.as_deref() else {
            return self.emit_naive_alt(text, rect, None);
        };

        let mut runs = match self.shaper.shape(text, style, &self.font_ctx) {
            Ok(r) => r,
            Err(_) => return self.emit_naive_alt(text, rect, Some(style)),
        };
        InlineTextItem::apply_spacing_to_runs(&mut runs, text, style.letter_spacing, style.word_spacing);

        let metrics_scaled = Self::resolve_scaled_metrics(style, &self.font_ctx);
        let line_height = compute_line_height_with_metrics(style, metrics_scaled.as_ref());
        let metrics = InlineTextItem::metrics_from_runs(&runs, line_height, style.font_size);
        let half_leading = ((metrics.line_height - (metrics.ascent + metrics.descent)) / 2.0).max(0.0);
        let baseline = rect.y() + half_leading + metrics.baseline_offset;

        self.emit_shaped_runs(&runs, style.color, baseline, rect.x());
        true
    }

    fn emit_naive_alt(&mut self, text: &str, rect: Rect, style: Option<&ComputedStyle>) -> bool {
        let font_size = style.map(|s| s.font_size).unwrap_or(16.0);
        let color = style.map(|s| s.color).unwrap_or(Rgba::BLACK);
        let char_width = font_size * 0.6;
        let origin = Point::new(rect.x(), rect.y() + font_size * 0.8);
        let glyphs: Vec<GlyphInstance> = text
            .chars()
            .enumerate()
            .map(|(i, _)| GlyphInstance {
                glyph_id: 0,
                offset: Point::new(i as f32 * char_width, 0.0),
                advance: char_width,
            })
            .collect();
        let advance_width = text.len() as f32 * char_width;

        self.list.push(DisplayItem::Text(TextItem {
            origin,
            glyphs,
            color,
            font_size,
            advance_width,
            font_id: None,
        }));
        true
    }

    fn decode_image(&self, src: &str) -> Option<ImageData> {
        let cache = self.image_cache.as_ref()?;
        let image = match cache.load(src) {
            Ok(img) => img,
            Err(_) if src.trim_start().starts_with('<') => cache.render_svg(src).ok()?,
            Err(_) => return None,
        };
        let (w, h) = image.dimensions();
        let rgba = image.to_rgba8();
        Some(ImageData::new(w, h, rgba.into_raw()))
    }
}

impl Default for DisplayListBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::image_loader::ImageCache;
    use crate::paint::stacking::{StackingContext, StackingContextReason};
    use crate::style::display::Display;
    use crate::style::position::Position;
    use crate::style::ComputedStyle;
    use crate::tree::box_tree::ReplacedType;

    fn create_block_fragment(x: f32, y: f32, width: f32, height: f32) -> FragmentNode {
        FragmentNode::new_block(Rect::from_xywh(x, y, width, height), vec![])
    }

    fn create_text_fragment(x: f32, y: f32, width: f32, height: f32, text: &str) -> FragmentNode {
        FragmentNode::new_text(Rect::from_xywh(x, y, width, height), text.to_string(), 12.0)
    }

    fn create_image_fragment(x: f32, y: f32, width: f32, height: f32, src: &str) -> FragmentNode {
        FragmentNode::new_replaced(
            Rect::from_xywh(x, y, width, height),
            ReplacedType::Image {
                src: src.to_string(),
                alt: None,
            },
        )
    }

    fn text_fragment_at(x: f32, label: &str) -> FragmentNode {
        FragmentNode::new_text(Rect::from_xywh(x, 0.0, 10.0, 10.0), label.to_string(), 12.0)
    }

    #[test]
    fn test_builder_empty_fragment() {
        let fragment = create_block_fragment(0.0, 0.0, 100.0, 100.0);
        let builder = DisplayListBuilder::new();
        let list = builder.build(&fragment);

        assert!(list.is_empty());
    }

    #[test]
    fn test_builder_text_fragment() {
        let fragment = create_text_fragment(10.0, 20.0, 100.0, 20.0, "Hello");
        let builder = DisplayListBuilder::new();
        let list = builder.build(&fragment);

        assert_eq!(list.len(), 1);
        assert!(matches!(list.items()[0], DisplayItem::Text(_)));
    }

    #[test]
    fn test_builder_text_position() {
        let fragment = create_text_fragment(10.0, 20.0, 100.0, 20.0, "Hello");
        let builder = DisplayListBuilder::new();
        let list = builder.build(&fragment);

        if let DisplayItem::Text(text) = &list.items()[0] {
            assert_eq!(text.origin.x, 10.0);
            assert_eq!(text.glyphs.len(), 5);
        } else {
            panic!("Expected Text item");
        }
    }

    #[test]
    fn test_builder_image_fragment() {
        let fragment = create_image_fragment(0.0, 0.0, 100.0, 100.0, "test.png");
        let builder = DisplayListBuilder::new();
        let list = builder.build(&fragment);

        assert_eq!(list.len(), 1);
        assert!(matches!(list.items()[0], DisplayItem::Image(_)));
    }

    #[test]
    fn stacking_context_order_respected() {
        let mut root = StackingContext::root();
        root.layer5_inlines.push(text_fragment_at(20.0, "root"));

        let mut neg = StackingContext::with_reason(-1, StackingContextReason::PositionedWithZIndex, 1);
        neg.layer5_inlines.push(text_fragment_at(0.0, "neg"));

        let mut pos = StackingContext::with_reason(1, StackingContextReason::PositionedWithZIndex, 2);
        pos.layer5_inlines.push(text_fragment_at(40.0, "pos"));

        root.add_child(neg);
        root.add_child(pos);

        let list = DisplayListBuilder::new().build_from_stacking(&root);
        let origins: Vec<f32> = list
            .items()
            .iter()
            .filter_map(|item| match item {
                DisplayItem::Text(t) => Some(t.origin.x),
                _ => None,
            })
            .collect();

        assert_eq!(origins, vec![0.0, 20.0, 40.0]);
    }

    #[test]
    fn build_with_stacking_tree_respects_z_order_from_styles() {
        fn styled_fragment(x: f32, label: &str, z: i32) -> FragmentNode {
            let mut style = ComputedStyle::default();
            style.position = Position::Relative;
            style.z_index = z;
            FragmentNode::new_inline_styled(
                Rect::from_xywh(x, 0.0, 10.0, 10.0),
                0,
                vec![FragmentNode::new_text(
                    Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
                    label.to_string(),
                    12.0,
                )],
                Arc::new(style),
            )
        }

        let child_neg = styled_fragment(0.0, "neg", -1);
        let child_zero = styled_fragment(20.0, "zero", 0);
        let child_pos = styled_fragment(40.0, "pos", 1);

        let root = FragmentNode::new_block(
            Rect::from_xywh(0.0, 0.0, 100.0, 20.0),
            vec![child_neg, child_zero, child_pos],
        );

        let list = DisplayListBuilder::new().build_with_stacking_tree(&root);
        let origins: Vec<f32> = list
            .items()
            .iter()
            .filter_map(|item| match item {
                DisplayItem::Text(t) => Some(t.origin.x),
                _ => None,
            })
            .collect();

        assert_eq!(origins, vec![0.0, 20.0, 40.0]);
    }

    #[test]
    fn test_builder_nested_fragments() {
        let child1 = create_text_fragment(0.0, 0.0, 50.0, 20.0, "One");
        let child2 = create_text_fragment(0.0, 20.0, 50.0, 20.0, "Two");
        let parent = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 100.0, 50.0), vec![child1, child2]);

        let builder = DisplayListBuilder::new();
        let list = builder.build(&parent);

        assert_eq!(list.len(), 2);
    }

    #[test]
    fn test_builder_position_offset() {
        let child = create_text_fragment(10.0, 10.0, 50.0, 20.0, "Hi");
        let parent = FragmentNode::new_block(Rect::from_xywh(20.0, 20.0, 100.0, 50.0), vec![child]);

        let builder = DisplayListBuilder::new();
        let list = builder.build(&parent);

        if let DisplayItem::Text(text) = &list.items()[0] {
            assert_eq!(text.origin.x, 30.0);
        } else {
            panic!("Expected Text item");
        }
    }

    #[test]
    fn test_builder_with_clips() {
        let child = create_text_fragment(0.0, 0.0, 50.0, 20.0, "Clipped");
        let parent = FragmentNode::new_block_with_id(Rect::from_xywh(0.0, 0.0, 100.0, 50.0), 42, vec![child]);

        let mut clips = HashSet::new();
        clips.insert(Some(42));

        let builder = DisplayListBuilder::new();
        let list = builder.build_with_clips(&parent, &clips);

        assert_eq!(list.len(), 3);
        assert!(matches!(list.items()[0], DisplayItem::PushClip(_)));
        assert!(matches!(list.items()[1], DisplayItem::Text(_)));
        assert!(matches!(list.items()[2], DisplayItem::PopClip));
    }

    #[test]
    fn test_builder_no_clips() {
        let child = create_text_fragment(0.0, 0.0, 50.0, 20.0, "NotClipped");
        let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 50.0), vec![child]);

        let clips = HashSet::new();

        let builder = DisplayListBuilder::new();
        let list = builder.build_with_clips(&parent, &clips);

        assert_eq!(list.len(), 1);
        assert!(matches!(list.items()[0], DisplayItem::Text(_)));
    }

    #[test]
    fn test_emit_background() {
        let mut builder = DisplayListBuilder::new();
        builder.emit_background(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), Rgba::RED);

        let list = builder.list;
        assert_eq!(list.len(), 1);
        assert!(matches!(list.items()[0], DisplayItem::FillRect(_)));
    }

    #[test]
    fn test_emit_background_transparent_skipped() {
        let mut builder = DisplayListBuilder::new();
        builder.emit_background(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), Rgba::TRANSPARENT);

        let list = builder.list;
        assert!(list.is_empty());
    }

    #[test]
    fn test_emit_border() {
        let mut builder = DisplayListBuilder::new();
        builder.emit_border(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), 2.0, Rgba::BLACK);

        let list = builder.list;
        assert_eq!(list.len(), 1);
        assert!(matches!(list.items()[0], DisplayItem::StrokeRect(_)));
    }

    #[test]
    fn test_push_pop_opacity() {
        let mut builder = DisplayListBuilder::new();
        builder.push_opacity(0.5);
        builder.emit_background(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), Rgba::RED);
        builder.pop_opacity();

        let list = builder.list;
        assert_eq!(list.len(), 3);
        assert!(matches!(list.items()[0], DisplayItem::PushOpacity(_)));
        assert!(matches!(list.items()[1], DisplayItem::FillRect(_)));
        assert!(matches!(list.items()[2], DisplayItem::PopOpacity));
    }

    #[test]
    fn test_push_pop_clip() {
        let mut builder = DisplayListBuilder::new();
        builder.push_clip(Rect::from_xywh(0.0, 0.0, 50.0, 50.0));
        builder.emit_background(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), Rgba::RED);
        builder.pop_clip();

        let list = builder.list;
        assert_eq!(list.len(), 3);
        assert!(matches!(list.items()[0], DisplayItem::PushClip(_)));
        assert!(matches!(list.items()[1], DisplayItem::FillRect(_)));
        assert!(matches!(list.items()[2], DisplayItem::PopClip));
    }

    #[test]
    fn test_fragment_tree_wrapper() {
        let child = create_text_fragment(10.0, 10.0, 50.0, 20.0, "Tree");
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 50.0), vec![child]);
        let tree = FragmentTree::new(root);

        let builder = DisplayListBuilder::new();
        let list = builder.build_tree(&tree);

        assert_eq!(list.len(), 1);
    }

    #[test]
    fn test_empty_text_skipped() {
        let fragment = create_text_fragment(0.0, 0.0, 100.0, 20.0, "");
        let builder = DisplayListBuilder::new();
        let list = builder.build(&fragment);

        assert!(list.is_empty());
    }

    #[test]
    fn test_deeply_nested() {
        let text = create_text_fragment(5.0, 5.0, 20.0, 10.0, "X");
        let level3 = FragmentNode::new_block(Rect::from_xywh(5.0, 5.0, 30.0, 20.0), vec![text]);
        let level2 = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 50.0, 40.0), vec![level3]);
        let level1 = FragmentNode::new_block(Rect::from_xywh(20.0, 20.0, 70.0, 60.0), vec![level2]);

        let builder = DisplayListBuilder::new();
        let list = builder.build(&level1);

        assert_eq!(list.len(), 1);
        if let DisplayItem::Text(text) = &list.items()[0] {
            assert_eq!(text.origin.x, 40.0);
        }
    }

    #[test]
    fn test_complex_tree() {
        let text1 = create_text_fragment(0.0, 0.0, 100.0, 20.0, "Line1");
        let text2 = create_text_fragment(0.0, 20.0, 100.0, 20.0, "Line2");
        let image = create_image_fragment(0.0, 40.0, 50.0, 50.0, "icon.png");

        let inner = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 120.0, 100.0), vec![text1, text2, image]);
        let outer = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 200.0, 200.0), vec![inner]);

        let builder = DisplayListBuilder::new();
        let list = builder.build(&outer);

        assert_eq!(list.len(), 3);

        let text_count = list
            .items()
            .iter()
            .filter(|i| matches!(i, DisplayItem::Text(_)))
            .count();
        let image_count = list
            .items()
            .iter()
            .filter(|i| matches!(i, DisplayItem::Image(_)))
            .count();

        assert_eq!(text_count, 2);
        assert_eq!(image_count, 1);
    }

    #[test]
    fn test_image_decoding_uses_cache() {
        // 1x1 red inline SVG
        let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1"><rect width="1" height="1" fill="red"/></svg>"#;
        let fragment = create_image_fragment(0.0, 0.0, 10.0, 10.0, svg);
        let builder = DisplayListBuilder::with_image_cache(ImageCache::new());
        let list = builder.build(&fragment);

        assert_eq!(list.len(), 1);
        let DisplayItem::Image(img) = &list.items()[0] else {
            panic!("Expected image item");
        };
        assert_eq!(img.image.width, 1);
        assert_eq!(img.image.height, 1);
        let pixels = img.image.pixels.as_ref();
        assert_eq!(pixels.len(), 4);
        assert_eq!(pixels, &[255, 0, 0, 255]);
    }

    #[test]
    fn embed_and_object_decode_images() {
        let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" width="2" height="2"><rect width="2" height="2" fill="blue"/></svg>"#;

        let embed_fragment = FragmentNode::new_replaced(
            Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
            ReplacedType::Embed { src: svg.to_string() },
        );
        let embed_list = DisplayListBuilder::with_image_cache(ImageCache::new()).build(&embed_fragment);
        let DisplayItem::Image(embed_img) = &embed_list.items()[0] else {
            panic!("expected image item for embed");
        };
        assert_eq!(embed_img.image.width, 2);
        assert_eq!(embed_img.image.height, 2);

        let object_fragment = FragmentNode::new_replaced(
            Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
            ReplacedType::Object { data: svg.to_string() },
        );
        let object_list = DisplayListBuilder::with_image_cache(ImageCache::new()).build(&object_fragment);
        let DisplayItem::Image(object_img) = &object_list.items()[0] else {
            panic!("expected image item for object");
        };
        assert_eq!(object_img.image.width, 2);
        assert_eq!(object_img.image.height, 2);
    }

    #[test]
    fn test_object_fit_contain_applied_in_display_list() {
        let mut style = ComputedStyle::default();
        style.display = Display::Inline;
        style.object_fit = crate::style::types::ObjectFit::Contain;
        style.object_position = crate::style::types::ObjectPosition {
            x: crate::style::types::PositionComponent::Keyword(crate::style::types::PositionKeyword::Center),
            y: crate::style::types::PositionComponent::Keyword(crate::style::types::PositionKeyword::Center),
        };

        let fragment = FragmentNode {
            bounds: Rect::from_xywh(0.0, 0.0, 200.0, 100.0),
            content: FragmentContent::Replaced {
                box_id: None,
                replaced_type: ReplacedType::Image {
                    src: "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMB/6X1ru4AAAAASUVORK5CYII=".to_string(),
                    alt: None,
                },
            },
            children: vec![],
            style: Some(Arc::new(style)),
        };

        let builder = DisplayListBuilder::with_image_cache(ImageCache::new());
        let list = builder.build(&fragment);

        assert_eq!(list.len(), 1);
        let DisplayItem::Image(img) = &list.items()[0] else {
            panic!("Expected image item");
        };
        // Image is 1x1, box is 200x100, contain => scale to min(200,100) => 100x100, centered horizontally.
        assert!((img.dest_rect.width() - 100.0).abs() < 0.1);
        assert!((img.dest_rect.height() - 100.0).abs() < 0.1);
        assert!((img.dest_rect.x() - 50.0).abs() < 0.1);
        assert!((img.dest_rect.y() - 0.0).abs() < 0.1);
    }

    #[test]
    fn object_position_viewport_units_resolve_in_display_list() {
        let mut style = ComputedStyle::default();
        style.object_fit = crate::style::types::ObjectFit::None;
        // Position 10vw from the left of the box. With 200px viewport width, free space is 50px (100-50).
        style.object_position = crate::style::types::ObjectPosition {
            x: crate::style::types::PositionComponent::Length(crate::style::values::Length::new(
                10.0,
                crate::style::values::LengthUnit::Vw,
            )),
            y: crate::style::types::PositionComponent::Keyword(crate::style::types::PositionKeyword::Start),
        };

        let fragment = FragmentNode {
            bounds: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
            content: FragmentContent::Replaced {
                box_id: None,
                replaced_type: ReplacedType::Image {
                    src: "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMB/6X1ru4AAAAASUVORK5CYII=".to_string(),
                    alt: None,
                },
            },
            children: vec![],
            style: Some(Arc::new(style)),
        };

        let builder = DisplayListBuilder::with_image_cache(ImageCache::new()).with_viewport_size(200.0, 100.0);
        let list = builder.build(&fragment);

        assert_eq!(list.len(), 1);
        let DisplayItem::Image(img) = &list.items()[0] else {
            panic!("Expected image item");
        };
        // free_x = 100 - 1 = 99; but we align with 10vw (20px), so dest_rect.x should be ~20.
        assert!((img.dest_rect.x() - 20.0).abs() < 0.5);
    }

    #[test]
    fn alt_text_emitted_when_image_missing() {
        let mut style = ComputedStyle::default();
        style.color = Rgba::BLACK;
        style.font_size = 12.0;

        let fragment = FragmentNode {
            bounds: Rect::from_xywh(0.0, 0.0, 50.0, 20.0),
            content: FragmentContent::Replaced {
                box_id: None,
                replaced_type: ReplacedType::Image {
                    src: String::new(),
                    alt: Some("alt text".to_string()),
                },
            },
            children: vec![],
            style: Some(Arc::new(style)),
        };

        let builder = DisplayListBuilder::new();
        let list = builder.build(&fragment);

        assert!(!list.is_empty());
        let DisplayItem::Text(text) = &list.items()[0] else {
            panic!("Expected text item for alt fallback");
        };
        assert!(text.advance_width > 0.0);
    }
}
