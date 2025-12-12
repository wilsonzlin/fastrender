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

use crate::css::types::ColorStop;
use crate::css::types::{RadialGradientShape, RadialGradientSize};
use crate::geometry::{Point, Rect};
use crate::image_loader::ImageCache;
use crate::layout::contexts::inline::baseline::compute_line_height_with_metrics;
use crate::layout::contexts::inline::line_builder::TextItem as InlineTextItem;
use crate::paint::display_list::{
    BlendMode, BlendModeItem, BorderImageItem, BorderImageSourceItem, BorderItem, BorderSide, BoxShadowItem, ClipItem,
    ConicGradientItem, DecorationPaint, DecorationStroke, DisplayItem, DisplayList, EmphasisMark, EmphasisText,
    FillRectItem, FontId, GlyphInstance, GradientSpread, GradientStop, ImageData, ImageFilterQuality, ImageItem,
    LinearGradientItem, ListMarkerItem, OpacityItem, OutlineItem, RadialGradientItem, ResolvedFilter,
    StackingContextItem, StrokeRectItem, TextDecorationItem, TextEmphasis, TextItem, TextShadowItem, Transform2D,
};
use crate::paint::object_fit::{compute_object_fit, default_object_position};
use crate::paint::stacking::StackingContext;
use crate::paint::text_shadow::resolve_text_shadows;
use crate::style::color::Rgba;
use crate::style::types::{
    BackgroundAttachment, BackgroundBox, BackgroundImage, BackgroundLayer, BackgroundPosition, BackgroundRepeatKeyword,
    BackgroundSize, BackgroundSizeComponent, BackgroundSizeKeyword, BorderImageSource, ImageOrientation,
    ImageRendering, Isolation, MixBlendMode, ObjectFit, ResolvedTextDecoration, TextDecorationLine,
    TextDecorationSkipInk, TextDecorationStyle, TextDecorationThickness, TextEmphasisPosition, TextEmphasisStyle,
    TextUnderlineOffset, TextUnderlinePosition,
};
use crate::style::values::{Length, LengthUnit};
use crate::style::ComputedStyle;
use crate::text::font_db::{FontStretch, FontStyle, ScaledMetrics};
use crate::text::font_loader::FontContext;
use crate::text::pipeline::{ShapedRun, ShapingPipeline};
use crate::tree::box_tree::ReplacedType;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
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
    device_pixel_ratio: f32,
}

#[derive(Clone, Copy)]
struct BackgroundRects {
    border: Rect,
    padding: Rect,
    content: Rect,
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
            image_cache: Some(ImageCache::new()),
            viewport: None,
            font_ctx: FontContext::new(),
            shaper: ShapingPipeline::new(),
            device_pixel_ratio: 1.0,
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
            device_pixel_ratio: 1.0,
        }
    }

    /// Sets the base URL used for resolving relative image URLs when decoding backgrounds/replaced elements.
    pub fn with_base_url(mut self, base_url: impl Into<String>) -> Self {
        let url = base_url.into();
        self.image_cache = self.image_cache.take().map(|mut cache| {
            cache.set_base_url(url);
            cache
        });
        self
    }

    /// Updates the base URL on the underlying image cache.
    pub fn set_base_url(&mut self, base_url: impl Into<String>) {
        if let Some(cache) = self.image_cache.as_mut() {
            cache.set_base_url(base_url);
        }
    }

    /// Sets the font context for shaping text into the display list.
    pub fn with_font_context(mut self, font_ctx: FontContext) -> Self {
        self.font_ctx = font_ctx;
        self
    }

    /// Sets the device pixel ratio for density selection (e.g., srcset/image-set).
    pub fn with_device_pixel_ratio(mut self, dpr: f32) -> Self {
        self.device_pixel_ratio = if dpr.is_finite() && dpr > 0.0 { dpr } else { 1.0 };
        self
    }

    /// Updates the device pixel ratio in place.
    pub fn set_device_pixel_ratio(&mut self, dpr: f32) {
        self.device_pixel_ratio = if dpr.is_finite() && dpr > 0.0 { dpr } else { 1.0 };
    }

    /// Sets the viewport size for resolving viewport-relative units (vw/vh) in object-position.
    pub fn with_viewport_size(mut self, width: f32, height: f32) -> Self {
        self.viewport = Some((width, height));
        self
    }

    /// Builds a display list from a fragment tree root
    pub fn build(mut self, root: &FragmentNode) -> DisplayList {
        if self.viewport.is_none() {
            self.viewport = Some((root.bounds.width(), root.bounds.height()));
        }
        self.build_fragment(root, Point::ZERO);
        self.list
    }

    /// Builds a display list from a FragmentTree
    pub fn build_tree(mut self, tree: &FragmentTree) -> DisplayList {
        if self.viewport.is_none() {
            let viewport = tree.viewport_size();
            self.viewport = Some((viewport.width, viewport.height));
        }
        self.build_fragment(&tree.root, Point::ZERO);
        self.list
    }

    /// Builds a display list from a stacking context tree (respecting z-order).
    pub fn build_from_stacking(mut self, stacking: &StackingContext) -> DisplayList {
        if self.viewport.is_none() {
            self.viewport = Some((stacking.bounds.width(), stacking.bounds.height()));
        }
        self.build_stacking_context(stacking, Point::ZERO);
        self.list
    }

    /// Builds a display list by first constructing a stacking context tree from the fragment tree.
    pub fn build_with_stacking_tree(mut self, root: &FragmentNode) -> DisplayList {
        if self.viewport.is_none() {
            self.viewport = Some((root.bounds.width(), root.bounds.height()));
        }
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
        if let Some(style) = fragment.style.as_deref() {
            if !matches!(style.visibility, crate::style::computed::Visibility::Visible) {
                return;
            }
        }

        let opacity = fragment.style.as_deref().map(|s| s.opacity).unwrap_or(1.0);
        let push_opacity = opacity < 1.0 - f32::EPSILON;
        if push_opacity {
            self.push_opacity(opacity);
        }

        let absolute_rect = Rect::new(
            Point::new(fragment.bounds.origin.x + offset.x, fragment.bounds.origin.y + offset.y),
            fragment.bounds.size,
        );

        if let Some(style) = fragment.style.as_deref() {
            self.emit_box_shadows_from_style(absolute_rect, style, false);
            self.emit_background_from_style(absolute_rect, style);
            self.emit_box_shadows_from_style(absolute_rect, style, true);
            self.emit_border_from_style(absolute_rect, style);
        }

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

        if let Some(style) = fragment.style.as_deref() {
            self.emit_outline(absolute_rect, style);
        }

        if push_opacity {
            self.pop_opacity();
        }
    }

    /// Recursively builds display items with clipping support
    fn build_fragment_with_clips(&mut self, fragment: &FragmentNode, offset: Point, clips: &HashSet<Option<usize>>) {
        if let Some(style) = fragment.style.as_deref() {
            if !matches!(style.visibility, crate::style::computed::Visibility::Visible) {
                return;
            }
        }

        let opacity = fragment.style.as_deref().map(|s| s.opacity).unwrap_or(1.0);
        let push_opacity = opacity < 1.0 - f32::EPSILON;
        if push_opacity {
            self.push_opacity(opacity);
        }

        let absolute_rect = Rect::new(
            Point::new(fragment.bounds.origin.x + offset.x, fragment.bounds.origin.y + offset.y),
            fragment.bounds.size,
        );

        if let Some(style) = fragment.style.as_deref() {
            self.emit_background_from_style(absolute_rect, style);
            self.emit_border_from_style(absolute_rect, style);
        }

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

        if let Some(style) = fragment.style.as_deref() {
            self.emit_outline(absolute_rect, style);
        }

        if push_opacity {
            self.pop_opacity();
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

        let root_fragment = context.fragments.first();
        let root_style = root_fragment.and_then(|f| f.style.as_deref());
        let paint_contained = root_style.map(|s| s.containment.paint).unwrap_or(false);
        let context_bounds = Rect::from_xywh(
            context.bounds.x() + offset.x,
            context.bounds.y() + offset.y,
            context.bounds.width(),
            context.bounds.height(),
        );

        let mix_blend_mode = root_style
            .map(|s| Self::convert_blend_mode(s.mix_blend_mode))
            .unwrap_or(BlendMode::Normal);
        let is_isolated = root_style
            .map(|s| matches!(s.isolation, Isolation::Isolate) || !s.backdrop_filter.is_empty())
            .unwrap_or(false);
        let (filters, backdrop_filters, radii) = root_style
            .map(|style| {
                (
                    Self::resolve_filters(&style.filter, style),
                    Self::resolve_filters(&style.backdrop_filter, style),
                    Self::resolve_border_radii(Some(style), context.bounds),
                )
            })
            .unwrap_or((Vec::new(), Vec::new(), crate::paint::display_list::BorderRadii::ZERO));
        let transform = root_style.and_then(|style| Self::build_transform(style, context.bounds));

        let mut pushed_clip = false;
        if paint_contained {
            let clip = root_fragment
                .and_then(|fragment| {
                    let style = root_style?;
                    let rect = Rect::new(
                        Point::new(fragment.bounds.origin.x + offset.x, fragment.bounds.origin.y + offset.y),
                        fragment.bounds.size,
                    );
                    let rects = Self::background_rects(rect, style);
                    let radii = Self::resolve_clip_radii(style, &rects, BackgroundBox::PaddingBox);
                    Some(ClipItem {
                        rect: rects.padding,
                        radii: if radii.is_zero() { None } else { Some(radii) },
                    })
                })
                .unwrap_or(ClipItem {
                    rect: context_bounds,
                    radii: None,
                });

            self.list.push(DisplayItem::PushClip(clip));
            pushed_clip = true;
        }

        self.list.push(DisplayItem::PushStackingContext(StackingContextItem {
            z_index: context.z_index,
            creates_stacking_context: true,
            bounds: context_bounds,
            mix_blend_mode,
            is_isolated,
            transform,
            filters,
            backdrop_filters,
            radii,
        }));

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

        self.list.push(DisplayItem::PopStackingContext);
        if pushed_clip {
            self.list.push(DisplayItem::PopClip);
        }
    }

    fn convert_blend_mode(mode: MixBlendMode) -> BlendMode {
        match mode {
            MixBlendMode::Normal => BlendMode::Normal,
            MixBlendMode::Multiply => BlendMode::Multiply,
            MixBlendMode::Screen => BlendMode::Screen,
            MixBlendMode::Overlay => BlendMode::Overlay,
            MixBlendMode::Darken => BlendMode::Darken,
            MixBlendMode::Lighten => BlendMode::Lighten,
            MixBlendMode::ColorDodge => BlendMode::ColorDodge,
            MixBlendMode::ColorBurn => BlendMode::ColorBurn,
            MixBlendMode::HardLight => BlendMode::HardLight,
            MixBlendMode::SoftLight => BlendMode::SoftLight,
            MixBlendMode::Difference => BlendMode::Difference,
            MixBlendMode::Exclusion => BlendMode::Exclusion,
            MixBlendMode::Hue => BlendMode::Hue,
            MixBlendMode::Saturation => BlendMode::Saturation,
            MixBlendMode::Color => BlendMode::Color,
            MixBlendMode::Luminosity => BlendMode::Luminosity,
        }
    }

    fn background_rects(rect: Rect, style: &ComputedStyle) -> BackgroundRects {
        let font_size = style.font_size;
        let base = rect.width().max(0.0);

        let border_left = Self::resolve_length_for_paint(&style.border_left_width, font_size, base);
        let border_right = Self::resolve_length_for_paint(&style.border_right_width, font_size, base);
        let border_top = Self::resolve_length_for_paint(&style.border_top_width, font_size, base);
        let border_bottom = Self::resolve_length_for_paint(&style.border_bottom_width, font_size, base);

        let padding_left = Self::resolve_length_for_paint(&style.padding_left, font_size, base);
        let padding_right = Self::resolve_length_for_paint(&style.padding_right, font_size, base);
        let padding_top = Self::resolve_length_for_paint(&style.padding_top, font_size, base);
        let padding_bottom = Self::resolve_length_for_paint(&style.padding_bottom, font_size, base);

        let border_rect = rect;
        let padding_rect = Self::inset_rect(border_rect, border_left, border_top, border_right, border_bottom);
        let content_rect = Self::inset_rect(padding_rect, padding_left, padding_top, padding_right, padding_bottom);

        BackgroundRects {
            border: border_rect,
            padding: padding_rect,
            content: content_rect,
        }
    }

    fn resolve_border_radii(style: Option<&ComputedStyle>, bounds: Rect) -> crate::paint::display_list::BorderRadii {
        let Some(style) = style else {
            return crate::paint::display_list::BorderRadii::ZERO;
        };
        let w = bounds.width().max(0.0);
        let h = bounds.height().max(0.0);
        if w <= 0.0 || h <= 0.0 {
            return crate::paint::display_list::BorderRadii::ZERO;
        }

        fn resolve_radius(len: &Length, reference: f32, font_size: f32) -> f32 {
            match len.unit {
                LengthUnit::Percent => len.resolve_against(reference),
                LengthUnit::Em | LengthUnit::Rem => len.resolve_with_font_size(font_size),
                _ if len.unit.is_absolute() => len.to_px(),
                _ => len.value * font_size,
            }
        }

        crate::paint::display_list::BorderRadii {
            top_left: resolve_radius(&style.border_top_left_radius, w, style.font_size),
            top_right: resolve_radius(&style.border_top_right_radius, w, style.font_size),
            bottom_right: resolve_radius(&style.border_bottom_right_radius, w, style.font_size),
            bottom_left: resolve_radius(&style.border_bottom_left_radius, w, style.font_size),
        }
        .clamped(w, h)
    }

    fn resolve_clip_radii(
        style: &ComputedStyle,
        rects: &BackgroundRects,
        clip: BackgroundBox,
    ) -> crate::paint::display_list::BorderRadii {
        let base = Self::resolve_border_radii(Some(style), rects.border);
        if base.is_zero() {
            return base;
        }

        let percentage_base = rects.border.width().max(0.0);
        let font_size = style.font_size;
        let border_left = Self::resolve_length_for_paint(&style.border_left_width, font_size, percentage_base);
        let border_right = Self::resolve_length_for_paint(&style.border_right_width, font_size, percentage_base);
        let border_top = Self::resolve_length_for_paint(&style.border_top_width, font_size, percentage_base);
        let border_bottom = Self::resolve_length_for_paint(&style.border_bottom_width, font_size, percentage_base);

        let padding_left = Self::resolve_length_for_paint(&style.padding_left, font_size, percentage_base);
        let padding_right = Self::resolve_length_for_paint(&style.padding_right, font_size, percentage_base);
        let padding_top = Self::resolve_length_for_paint(&style.padding_top, font_size, percentage_base);
        let padding_bottom = Self::resolve_length_for_paint(&style.padding_bottom, font_size, percentage_base);

        match clip {
            BackgroundBox::BorderBox => base,
            BackgroundBox::PaddingBox => {
                let shrunk = crate::paint::display_list::BorderRadii {
                    top_left: (base.top_left - border_left.max(border_top)).max(0.0),
                    top_right: (base.top_right - border_right.max(border_top)).max(0.0),
                    bottom_right: (base.bottom_right - border_right.max(border_bottom)).max(0.0),
                    bottom_left: (base.bottom_left - border_left.max(border_bottom)).max(0.0),
                };
                shrunk.clamped(rects.padding.width(), rects.padding.height())
            }
            BackgroundBox::ContentBox => {
                let shrink_left = border_left + padding_left;
                let shrink_right = border_right + padding_right;
                let shrink_top = border_top + padding_top;
                let shrink_bottom = border_bottom + padding_bottom;
                let shrunk = crate::paint::display_list::BorderRadii {
                    top_left: (base.top_left - shrink_left.max(shrink_top)).max(0.0),
                    top_right: (base.top_right - shrink_right.max(shrink_top)).max(0.0),
                    bottom_right: (base.bottom_right - shrink_right.max(shrink_bottom)).max(0.0),
                    bottom_left: (base.bottom_left - shrink_left.max(shrink_bottom)).max(0.0),
                };
                shrunk.clamped(rects.content.width(), rects.content.height())
            }
        }
    }

    fn normalize_color_stops(stops: &[ColorStop], current_color: Rgba) -> Vec<(f32, Rgba)> {
        if stops.is_empty() {
            return Vec::new();
        }

        let mut positions: Vec<Option<f32>> = stops.iter().map(|s| s.position).collect();
        if positions.iter().all(|p| p.is_none()) {
            if stops.len() == 1 {
                return vec![(0.0, stops[0].color.to_rgba(current_color))];
            }
            let denom = (stops.len() - 1) as f32;
            return stops
                .iter()
                .enumerate()
                .map(|(i, s)| (i as f32 / denom, s.color.to_rgba(current_color)))
                .collect();
        }

        if positions.first().and_then(|p| *p).is_none() {
            positions[0] = Some(0.0);
        }
        if positions.last().and_then(|p| *p).is_none() {
            if let Some(last) = positions.last_mut() {
                *last = Some(1.0);
            }
        }

        let mut last_known: Option<(usize, f32)> = None;
        for i in 0..positions.len() {
            if let Some(pos) = positions[i] {
                if let Some((start_idx, start_pos)) = last_known {
                    let gap = i.saturating_sub(start_idx + 1);
                    if gap > 0 {
                        let step = (pos - start_pos) / (gap as f32 + 1.0);
                        for j in 1..=gap {
                            positions[start_idx + j] = Some((start_pos + step * j as f32).max(start_pos));
                        }
                    }
                } else if i > 0 {
                    let gap = i;
                    let step = pos / gap as f32;
                    for j in 0..i {
                        positions[j] = Some(step * j as f32);
                    }
                }
                last_known = Some((i, pos));
            }
        }

        let mut output = Vec::with_capacity(stops.len());
        let mut prev = 0.0;
        for (idx, pos_opt) in positions.into_iter().enumerate() {
            let pos = pos_opt.unwrap_or(prev);
            let clamped = pos.max(prev).clamp(0.0, 1.0);
            prev = clamped;
            output.push((clamped, stops[idx].color.to_rgba(current_color)));
        }

        output
    }

    fn normalize_color_stops_unclamped(stops: &[ColorStop], current_color: Rgba) -> Vec<(f32, Rgba)> {
        if stops.is_empty() {
            return Vec::new();
        }

        let mut positions: Vec<Option<f32>> = stops.iter().map(|s| s.position).collect();
        if positions.iter().all(|p| p.is_none()) {
            if stops.len() == 1 {
                return vec![(0.0, stops[0].color.to_rgba(current_color))];
            }
            let denom = (stops.len() - 1) as f32;
            return stops
                .iter()
                .enumerate()
                .map(|(i, s)| (i as f32 / denom, s.color.to_rgba(current_color)))
                .collect();
        }

        if positions.first().and_then(|p| *p).is_none() {
            positions[0] = Some(0.0);
        }
        if positions.last().and_then(|p| *p).is_none() {
            if let Some(last) = positions.last_mut() {
                *last = Some(1.0);
            }
        }

        let mut last_known: Option<(usize, f32)> = None;
        for i in 0..positions.len() {
            if let Some(pos) = positions[i] {
                if let Some((start_idx, start_pos)) = last_known {
                    let gap = i.saturating_sub(start_idx + 1);
                    if gap > 0 {
                        let step = (pos - start_pos) / (gap as f32 + 1.0);
                        for j in 1..=gap {
                            positions[start_idx + j] = Some(start_pos + step * j as f32);
                        }
                    }
                } else if i > 0 {
                    let gap = i;
                    let step = pos / gap as f32;
                    for j in 0..i {
                        positions[j] = Some(step * j as f32);
                    }
                }
                last_known = Some((i, pos));
            }
        }

        let mut output = Vec::with_capacity(stops.len());
        let mut prev = 0.0;
        for (idx, pos_opt) in positions.into_iter().enumerate() {
            let pos = pos_opt.unwrap_or(prev);
            let monotonic = pos.max(prev);
            prev = monotonic;
            output.push((monotonic, stops[idx].color.to_rgba(current_color)));
        }
        output
    }

    fn gradient_stops(stops: &[(f32, Rgba)]) -> Vec<GradientStop> {
        stops
            .iter()
            .map(|(pos, color)| GradientStop {
                position: pos.clamp(0.0, 1.0),
                color: *color,
            })
            .collect()
    }

    fn gradient_stops_unclamped(stops: &[(f32, Rgba)]) -> Vec<GradientStop> {
        stops
            .iter()
            .map(|(pos, color)| GradientStop {
                position: *pos,
                color: *color,
            })
            .collect()
    }

    fn radial_geometry(
        rect: Rect,
        position: &BackgroundPosition,
        size: &RadialGradientSize,
        shape: RadialGradientShape,
        font_size: f32,
    ) -> (f32, f32, f32, f32) {
        let (align_x, off_x, align_y, off_y) = match position {
            BackgroundPosition::Position { x, y } => {
                let ox = Self::resolve_length_for_paint(&x.offset, font_size, rect.width());
                let oy = Self::resolve_length_for_paint(&y.offset, font_size, rect.height());
                (x.alignment, ox, y.alignment, oy)
            }
        };
        let cx = rect.x() + align_x * rect.width() + off_x;
        let cy = rect.y() + align_y * rect.height() + off_y;

        let dx_left = (cx - rect.x()).max(0.0);
        let dx_right = (rect.x() + rect.width() - cx).max(0.0);
        let dy_top = (cy - rect.y()).max(0.0);
        let dy_bottom = (rect.y() + rect.height() - cy).max(0.0);

        let (mut rx, mut ry) = match size {
            RadialGradientSize::ClosestSide => (dx_left.min(dx_right), dy_top.min(dy_bottom)),
            RadialGradientSize::FarthestSide => (dx_left.max(dx_right), dy_top.max(dy_bottom)),
            RadialGradientSize::ClosestCorner => {
                let corners = [
                    (dx_left, dy_top),
                    (dx_left, dy_bottom),
                    (dx_right, dy_top),
                    (dx_right, dy_bottom),
                ];
                let mut best = std::f32::INFINITY;
                let mut best_pair = (0.0, 0.0);
                for (dx, dy) in corners {
                    let dist = (dx * dx + dy * dy).sqrt();
                    if dist < best {
                        best = dist;
                        best_pair = (dx, dy);
                    }
                }
                (
                    best_pair.0 * std::f32::consts::SQRT_2,
                    best_pair.1 * std::f32::consts::SQRT_2,
                )
            }
            RadialGradientSize::FarthestCorner => {
                let corners = [
                    (dx_left, dy_top),
                    (dx_left, dy_bottom),
                    (dx_right, dy_top),
                    (dx_right, dy_bottom),
                ];
                let mut best = -std::f32::INFINITY;
                let mut best_pair = (0.0, 0.0);
                for (dx, dy) in corners {
                    let dist = (dx * dx + dy * dy).sqrt();
                    if dist > best {
                        best = dist;
                        best_pair = (dx, dy);
                    }
                }
                (
                    best_pair.0 * std::f32::consts::SQRT_2,
                    best_pair.1 * std::f32::consts::SQRT_2,
                )
            }
            RadialGradientSize::Explicit { x, y } => {
                let rx = Self::resolve_length_for_paint(x, font_size, rect.width()).max(0.0);
                let ry = y
                    .as_ref()
                    .map(|yy| Self::resolve_length_for_paint(yy, font_size, rect.height()).max(0.0))
                    .unwrap_or(rx);
                (rx, ry)
            }
        };

        if matches!(shape, RadialGradientShape::Circle) {
            let r = if matches!(
                size,
                RadialGradientSize::ClosestCorner | RadialGradientSize::FarthestCorner
            ) {
                let avg = (rx * rx + ry * ry) / 2.0;
                avg.sqrt()
            } else {
                rx.min(ry)
            };
            rx = r;
            ry = r;
        }

        (cx, cy, rx, ry)
    }

    fn resolve_gradient_center(rect: Rect, position: &BackgroundPosition, font_size: f32, clip_rect: Rect) -> Point {
        let (align_x, off_x, align_y, off_y) = match position {
            BackgroundPosition::Position { x, y } => {
                let ox = Self::resolve_length_for_paint(&x.offset, font_size, rect.width());
                let oy = Self::resolve_length_for_paint(&y.offset, font_size, rect.height());
                (x.alignment, ox, y.alignment, oy)
            }
        };
        let cx = rect.x() + align_x * rect.width() + off_x - clip_rect.x();
        let cy = rect.y() + align_y * rect.height() + off_y - clip_rect.y();
        Point::new(cx, cy)
    }

    fn resolve_filters(filters: &[crate::style::types::FilterFunction], style: &ComputedStyle) -> Vec<ResolvedFilter> {
        filters
            .iter()
            .filter_map(|f| match f {
                crate::style::types::FilterFunction::Blur(len) => {
                    Some(ResolvedFilter::Blur(Self::resolve_filter_length(len, style)))
                }
                crate::style::types::FilterFunction::Brightness(v) => Some(ResolvedFilter::Brightness(*v)),
                crate::style::types::FilterFunction::Contrast(v) => Some(ResolvedFilter::Contrast(*v)),
                crate::style::types::FilterFunction::Grayscale(v) => Some(ResolvedFilter::Grayscale(*v)),
                crate::style::types::FilterFunction::Sepia(v) => Some(ResolvedFilter::Sepia(*v)),
                crate::style::types::FilterFunction::Saturate(v) => Some(ResolvedFilter::Saturate(*v)),
                crate::style::types::FilterFunction::HueRotate(deg) => Some(ResolvedFilter::HueRotate(*deg)),
                crate::style::types::FilterFunction::Invert(v) => Some(ResolvedFilter::Invert(*v)),
                crate::style::types::FilterFunction::Opacity(v) => Some(ResolvedFilter::Opacity(*v)),
                crate::style::types::FilterFunction::DropShadow(shadow) => {
                    let color = match shadow.color {
                        crate::style::types::FilterColor::CurrentColor => style.color,
                        crate::style::types::FilterColor::Color(c) => c,
                    };
                    Some(ResolvedFilter::DropShadow {
                        offset_x: Self::resolve_filter_length(&shadow.offset_x, style),
                        offset_y: Self::resolve_filter_length(&shadow.offset_y, style),
                        blur_radius: Self::resolve_filter_length(&shadow.blur_radius, style),
                        spread: Self::resolve_filter_length(&shadow.spread, style),
                        color,
                    })
                }
            })
            .collect()
    }

    fn resolve_filter_length(len: &Length, style: &ComputedStyle) -> f32 {
        match len.unit {
            LengthUnit::Em | LengthUnit::Rem => len.resolve_with_font_size(style.font_size),
            LengthUnit::Percent => len.resolve_against(style.font_size),
            unit if unit.is_absolute() => len.to_px(),
            _ => 0.0,
        }
    }

    fn compute_background_size(
        layer: &BackgroundLayer,
        font_size: f32,
        area_w: f32,
        area_h: f32,
        img_w: f32,
        img_h: f32,
    ) -> (f32, f32) {
        let natural_w = if img_w > 0.0 { Some(img_w) } else { None };
        let natural_h = if img_h > 0.0 { Some(img_h) } else { None };
        let ratio = if img_w > 0.0 && img_h > 0.0 {
            Some(img_w / img_h)
        } else {
            None
        };

        match layer.size {
            BackgroundSize::Keyword(BackgroundSizeKeyword::Cover) => {
                if let (Some(w), Some(h)) = (natural_w, natural_h) {
                    let scale = (area_w / w).max(area_h / h);
                    (w * scale, h * scale)
                } else {
                    (area_w.max(0.0), area_h.max(0.0))
                }
            }
            BackgroundSize::Keyword(BackgroundSizeKeyword::Contain) => {
                if let (Some(w), Some(h)) = (natural_w, natural_h) {
                    let scale = (area_w / w).min(area_h / h);
                    (w * scale, h * scale)
                } else {
                    (area_w.max(0.0), area_h.max(0.0))
                }
            }
            BackgroundSize::Explicit(x, y) => {
                let resolve = |component: BackgroundSizeComponent, area: f32| -> Option<f32> {
                    match component {
                        BackgroundSizeComponent::Auto => None,
                        BackgroundSizeComponent::Length(len) => {
                            Some(Self::resolve_length_for_paint(&len, font_size, area).max(0.0))
                        }
                    }
                };

                let resolved_x = resolve(x, area_w);
                let resolved_y = resolve(y, area_h);

                match (resolved_x, resolved_y) {
                    (Some(w), Some(h)) => (w, h),
                    (Some(w), None) => {
                        if let Some(r) = ratio {
                            (w, (w / r).max(0.0))
                        } else if let Some(h) = natural_h {
                            (w, h)
                        } else {
                            (w, area_h.max(0.0))
                        }
                    }
                    (None, Some(h)) => {
                        if let Some(r) = ratio {
                            ((h * r).max(0.0), h)
                        } else if let Some(w) = natural_w {
                            (w, h)
                        } else {
                            (area_w.max(0.0), h)
                        }
                    }
                    (None, None) => {
                        if let (Some(w), Some(h)) = (natural_w, natural_h) {
                            (w, h)
                        } else {
                            (area_w.max(0.0), area_h.max(0.0))
                        }
                    }
                }
            }
        }
    }

    fn resolve_background_offset(
        pos: BackgroundPosition,
        area_w: f32,
        area_h: f32,
        tile_w: f32,
        tile_h: f32,
        font_size: f32,
    ) -> (f32, f32) {
        let resolve_axis = |comp: crate::style::types::BackgroundPositionComponent, area: f32, tile: f32| -> f32 {
            let available = area - tile;
            let offset = Self::resolve_length_for_paint(&comp.offset, font_size, available);
            comp.alignment * available + offset
        };

        match pos {
            BackgroundPosition::Position { x, y } => {
                let x = resolve_axis(x, area_w, tile_w);
                let y = resolve_axis(y, area_h, tile_h);
                (x, y)
            }
        }
    }

    fn aligned_start(origin: f32, tile: f32, clip_min: f32) -> f32 {
        if tile == 0.0 {
            return origin;
        }
        let steps = ((clip_min - origin) / tile).floor();
        origin + steps * tile
    }

    fn round_tile_length(area_len: f32, tile_len: f32) -> f32 {
        if tile_len == 0.0 {
            return 0.0;
        }
        let count = (area_len / tile_len).round().max(1.0);
        area_len / count
    }

    fn tile_positions(
        repeat: BackgroundRepeatKeyword,
        area_start: f32,
        area_len: f32,
        tile_len: f32,
        offset: f32,
        clip_min: f32,
        clip_max: f32,
    ) -> Vec<f32> {
        if tile_len <= 0.0 {
            return Vec::new();
        }

        match repeat {
            BackgroundRepeatKeyword::NoRepeat => vec![area_start + offset],
            BackgroundRepeatKeyword::Repeat | BackgroundRepeatKeyword::Round => {
                let start = Self::aligned_start(area_start + offset, tile_len, clip_min);
                let mut positions = Vec::new();
                let mut pos = start;
                while pos < clip_max {
                    positions.push(pos);
                    pos += tile_len;
                }
                positions
            }
            BackgroundRepeatKeyword::Space => {
                let count = (area_len / tile_len).floor() as i32;
                if count >= 2 {
                    let spacing = (area_len - tile_len * count as f32) / (count as f32 - 1.0);
                    let step = tile_len + spacing;
                    let anchor = area_start;
                    let mut positions = Vec::new();
                    let k = ((clip_min - anchor) / step).floor();
                    let mut pos = anchor + k * step;
                    while pos < clip_max {
                        positions.push(pos);
                        pos += step;
                    }
                    positions
                } else {
                    let centered = area_start + offset + (area_len - tile_len) * 0.5;
                    vec![centered]
                }
            }
        }
    }

    fn resolve_length_for_paint(len: &Length, font_size: f32, percentage_base: f32) -> f32 {
        match len.unit {
            LengthUnit::Percent => len.resolve_against(percentage_base),
            LengthUnit::Em | LengthUnit::Rem => len.resolve_with_font_size(font_size),
            _ if len.unit.is_absolute() => len.to_px(),
            _ => len.value,
        }
    }

    fn inset_rect(rect: Rect, left: f32, top: f32, right: f32, bottom: f32) -> Rect {
        let new_x = rect.x() + left;
        let new_y = rect.y() + top;
        let new_w = (rect.width() - left - right).max(0.0);
        let new_h = (rect.height() - top - bottom).max(0.0);
        Rect::from_xywh(new_x, new_y, new_w, new_h)
    }

    fn border_radii(rect: Rect, style: &ComputedStyle) -> crate::paint::display_list::BorderRadii {
        Self::resolve_border_radii(Some(style), rect)
    }

    fn border_style_visible(style: crate::style::types::BorderStyle) -> bool {
        !matches!(
            style,
            crate::style::types::BorderStyle::None | crate::style::types::BorderStyle::Hidden
        )
    }

    fn border_side_visible(side: &BorderSide) -> bool {
        side.width > 0.0 && Self::border_style_visible(side.style) && !side.color.is_transparent()
    }

    fn build_transform(style: &ComputedStyle, bounds: Rect) -> Option<Transform2D> {
        if style.transform.is_empty() {
            return None;
        }

        let mut ts = Transform2D::identity();
        for component in &style.transform {
            let next = match component {
                crate::css::types::Transform::Translate(x, y) => {
                    let tx = Self::resolve_transform_length(x, style.font_size, bounds.width());
                    let ty = Self::resolve_transform_length(y, style.font_size, bounds.height());
                    Transform2D::translate(tx, ty)
                }
                crate::css::types::Transform::TranslateX(x) => {
                    let tx = Self::resolve_transform_length(x, style.font_size, bounds.width());
                    Transform2D::translate(tx, 0.0)
                }
                crate::css::types::Transform::TranslateY(y) => {
                    let ty = Self::resolve_transform_length(y, style.font_size, bounds.height());
                    Transform2D::translate(0.0, ty)
                }
                crate::css::types::Transform::Scale(sx, sy) => Transform2D::scale(*sx, *sy),
                crate::css::types::Transform::ScaleX(sx) => Transform2D::scale(*sx, 1.0),
                crate::css::types::Transform::ScaleY(sy) => Transform2D::scale(1.0, *sy),
                crate::css::types::Transform::Rotate(deg) => Transform2D::rotate(*deg),
                crate::css::types::Transform::SkewX(deg) => Transform2D::skew(deg.to_radians().tan(), 0.0),
                crate::css::types::Transform::SkewY(deg) => Transform2D::skew(0.0, deg.to_radians().tan()),
                crate::css::types::Transform::Matrix(a, b, c, d, e, f) => Transform2D {
                    a: *a,
                    b: *b,
                    c: *c,
                    d: *d,
                    e: *e,
                    f: *f,
                },
            };
            ts = ts.multiply(&next);
        }

        let origin_x = Self::resolve_transform_length(&style.transform_origin.x, style.font_size, bounds.width());
        let origin_y = Self::resolve_transform_length(&style.transform_origin.y, style.font_size, bounds.height());
        let origin = Point::new(bounds.x() + origin_x, bounds.y() + origin_y);

        let translate_to_origin = Transform2D::translate(origin.x, origin.y);
        let translate_back = Transform2D::translate(-origin.x, -origin.y);
        Some(translate_to_origin.multiply(&ts).multiply(&translate_back))
    }

    fn resolve_transform_length(len: &Length, font_size: f32, percentage_base: f32) -> f32 {
        match len.unit {
            LengthUnit::Percent => len.resolve_against(percentage_base),
            LengthUnit::Em | LengthUnit::Rem => len.resolve_with_font_size(font_size),
            _ if len.unit.is_absolute() => len.to_px(),
            _ => len.value,
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
                is_marker,
                ..
            } => {
                if text.is_empty() {
                    return;
                }

                let style_opt = fragment.style.as_deref();
                let color = style_opt.map(|s| s.color).unwrap_or(Rgba::BLACK);
                let shadows = Self::text_shadows_from_style(style_opt);
                let baseline = rect.origin.y + baseline_offset;
                let start_x = rect.origin.x;

                let mut shaped_storage: Option<Vec<ShapedRun>> = None;
                let runs_ref: Option<&[ShapedRun]> = if let Some(runs) = shaped {
                    Some(runs.as_slice())
                } else if let Some(style) = style_opt {
                    if let Ok(mut runs) = self.shaper.shape(text, style, &self.font_ctx) {
                        InlineTextItem::apply_spacing_to_runs(
                            &mut runs,
                            text,
                            style.letter_spacing,
                            style.word_spacing,
                        );
                        shaped_storage = Some(runs);
                    }
                    shaped_storage.as_deref()
                } else {
                    None
                };

                if let Some(runs) = runs_ref {
                    if *is_marker {
                        self.emit_list_marker_runs(runs, color, baseline, start_x, &shadows, style_opt);
                    } else {
                        self.emit_shaped_runs(runs, color, baseline, start_x, &shadows, style_opt);
                    }
                } else {
                    // Fallback: naive glyphs when shaping fails or no style is present
                    let font_size = style_opt.map(|s| s.font_size).unwrap_or(16.0);
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

                    if *is_marker {
                        self.list.push(DisplayItem::ListMarker(ListMarkerItem {
                            origin: Point::new(start_x, baseline),
                            glyphs,
                            color,
                            shadows: shadows.clone(),
                            font_size,
                            advance_width,
                            font_id: None,
                            synthetic_bold: 0.0,
                            synthetic_oblique: 0.0,
                            emphasis: None,
                            background: None,
                        }));
                    } else {
                        self.list.push(DisplayItem::Text(TextItem {
                            origin: Point::new(start_x, baseline),
                            glyphs,
                            color,
                            shadows: shadows.clone(),
                            font_size,
                            advance_width,
                            font_id: None,
                            synthetic_bold: 0.0,
                            synthetic_oblique: 0.0,
                            emphasis: None,
                            decorations: Vec::new(),
                        }));
                    }
                }

                if let Some(style) = style_opt {
                    self.emit_text_decorations(style, runs_ref, rect.x(), rect.width(), baseline);
                }
            }

            FragmentContent::Replaced { replaced_type, .. } => {
                let media_ctx = self.viewport.map(|(w, h)| {
                    crate::style::media::MediaContext::screen(w, h).with_device_pixel_ratio(self.device_pixel_ratio)
                });
                let style_for_image = fragment.style.as_deref();
                let sources = replaced_type.image_sources_with_fallback(crate::tree::box_tree::ImageSelectionContext {
                    scale: self.device_pixel_ratio,
                    slot_width: Some(rect.width()),
                    viewport: self.viewport.map(|(w, h)| crate::geometry::Size::new(w, h)),
                    media_context: media_ctx.as_ref(),
                    font_size: fragment.style.as_deref().map(|s| s.font_size),
                });

                if let Some(image) = sources
                    .iter()
                    .filter_map(|s| self.decode_image(s, style_for_image, false))
                    .next()
                {
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
                            image.css_width,
                            image.css_height,
                            font_size,
                            self.viewport,
                        )
                        .unwrap_or((0.0, 0.0, rect.width(), rect.height()))
                    };

                    let dest_rect = Rect::from_xywh(rect.x() + dest_x, rect.y() + dest_y, dest_w, dest_h);
                    self.list.push(DisplayItem::Image(ImageItem {
                        dest_rect,
                        image: Arc::new(image),
                        filter_quality: Self::image_filter_quality(fragment.style.as_deref()),
                        src_rect: None,
                    }));
                    return;
                }

                if let ReplacedType::Image { alt: Some(alt), .. } = replaced_type {
                    if self.emit_alt_text(alt, fragment, rect) {
                        return;
                    }
                }

                self.emit_replaced_placeholder(replaced_type, fragment, rect);
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
            self.list.push(DisplayItem::StrokeRect(StrokeRectItem {
                rect,
                color,
                width,
                blend_mode: BlendMode::Normal,
            }));
        }
    }

    fn emit_background_from_style(&mut self, rect: Rect, style: &ComputedStyle) {
        let has_images = style.background_layers.iter().any(|l| l.image.is_some());
        if style.background_color.is_transparent() && !has_images {
            return;
        }

        let rects = Self::background_rects(rect, style);
        let fallback = BackgroundLayer::default();
        let color_layer = style.background_layers.first().unwrap_or(&fallback);
        let color_clip_rect = match color_layer.clip {
            BackgroundBox::BorderBox => rects.border,
            BackgroundBox::PaddingBox => rects.padding,
            BackgroundBox::ContentBox => rects.content,
        };
        if style.background_color.alpha_u8() > 0 && color_clip_rect.width() > 0.0 && color_clip_rect.height() > 0.0 {
            let radii = Self::resolve_clip_radii(style, &rects, color_layer.clip);
            if radii.is_zero() {
                self.emit_background(color_clip_rect, style.background_color);
            } else {
                self.list.push(DisplayItem::FillRoundedRect(
                    crate::paint::display_list::FillRoundedRectItem {
                        rect: color_clip_rect,
                        color: style.background_color,
                        radii,
                    },
                ));
            }
        }

        for layer in style.background_layers.iter().rev() {
            if let Some(image) = &layer.image {
                self.emit_background_layer(&rects, style, layer, image);
            }
        }
    }

    fn emit_background_layer(
        &mut self,
        rects: &BackgroundRects,
        style: &ComputedStyle,
        layer: &BackgroundLayer,
        bg: &BackgroundImage,
    ) {
        let is_local = layer.attachment == BackgroundAttachment::Local;
        let clip_box = if is_local {
            match layer.clip {
                BackgroundBox::ContentBox => BackgroundBox::ContentBox,
                _ => BackgroundBox::PaddingBox,
            }
        } else {
            layer.clip
        };
        let clip_rect = match clip_box {
            BackgroundBox::BorderBox => rects.border,
            BackgroundBox::PaddingBox => rects.padding,
            BackgroundBox::ContentBox => rects.content,
        };
        let origin_rect = if layer.attachment == BackgroundAttachment::Fixed {
            if let Some((w, h)) = self.viewport {
                Rect::from_xywh(0.0, 0.0, w, h)
            } else {
                rects.border
            }
        } else if is_local {
            match layer.origin {
                BackgroundBox::ContentBox => rects.content,
                _ => rects.padding,
            }
        } else {
            match layer.origin {
                BackgroundBox::BorderBox => rects.border,
                BackgroundBox::PaddingBox => rects.padding,
                BackgroundBox::ContentBox => rects.content,
            }
        };

        if clip_rect.width() <= 0.0
            || clip_rect.height() <= 0.0
            || origin_rect.width() <= 0.0
            || origin_rect.height() <= 0.0
        {
            return;
        }

        let clip_radii = Self::resolve_clip_radii(style, rects, clip_box);
        let blend_mode = Self::convert_blend_mode(layer.blend_mode);
        let use_blend = blend_mode != BlendMode::Normal;
        let mut pushed_clip = false;
        if !clip_radii.is_zero() {
            self.list.push(DisplayItem::PushClip(ClipItem {
                rect: clip_rect,
                radii: Some(clip_radii),
            }));
            pushed_clip = true;
        }
        if use_blend {
            self.list
                .push(DisplayItem::PushBlendMode(BlendModeItem { mode: blend_mode }));
        }

        match bg {
            BackgroundImage::LinearGradient { angle, stops } => {
                let resolved = Self::normalize_color_stops(stops, style.color);
                if !resolved.is_empty() {
                    let rad = angle.to_radians();
                    let dx = rad.sin();
                    let dy = -rad.cos();
                    let len = 0.5 * (origin_rect.width() * dx.abs() + origin_rect.height() * dy.abs());
                    let cx = origin_rect.x() + origin_rect.width() / 2.0;
                    let cy = origin_rect.y() + origin_rect.height() / 2.0;
                    let start = Point::new(cx - dx * len - clip_rect.x(), cy - dy * len - clip_rect.y());
                    let end = Point::new(cx + dx * len - clip_rect.x(), cy + dy * len - clip_rect.y());
                    self.list.push(DisplayItem::LinearGradient(LinearGradientItem {
                        rect: clip_rect,
                        start,
                        end,
                        stops: Self::gradient_stops(&resolved),
                        spread: GradientSpread::Pad,
                    }));
                }
            }
            BackgroundImage::RepeatingLinearGradient { angle, stops } => {
                let resolved = Self::normalize_color_stops(stops, style.color);
                if !resolved.is_empty() {
                    let rad = angle.to_radians();
                    let dx = rad.sin();
                    let dy = -rad.cos();
                    let len = 0.5 * (origin_rect.width() * dx.abs() + origin_rect.height() * dy.abs());
                    let cx = origin_rect.x() + origin_rect.width() / 2.0;
                    let cy = origin_rect.y() + origin_rect.height() / 2.0;
                    let start = Point::new(cx - dx * len - clip_rect.x(), cy - dy * len - clip_rect.y());
                    let end = Point::new(cx + dx * len - clip_rect.x(), cy + dy * len - clip_rect.y());
                    self.list.push(DisplayItem::LinearGradient(LinearGradientItem {
                        rect: clip_rect,
                        start,
                        end,
                        stops: Self::gradient_stops(&resolved),
                        spread: GradientSpread::Repeat,
                    }));
                }
            }
            BackgroundImage::ConicGradient {
                from_angle,
                position,
                stops,
            } => {
                let resolved = Self::normalize_color_stops_unclamped(stops, style.color);
                if !resolved.is_empty() {
                    let center = Self::resolve_gradient_center(origin_rect, position, style.font_size, clip_rect);
                    self.list.push(DisplayItem::ConicGradient(ConicGradientItem {
                        rect: clip_rect,
                        center,
                        from_angle: *from_angle,
                        stops: Self::gradient_stops_unclamped(&resolved),
                        repeating: false,
                    }));
                }
            }
            BackgroundImage::RepeatingConicGradient {
                from_angle,
                position,
                stops,
            } => {
                let resolved = Self::normalize_color_stops_unclamped(stops, style.color);
                if !resolved.is_empty() {
                    let center = Self::resolve_gradient_center(origin_rect, position, style.font_size, clip_rect);
                    self.list.push(DisplayItem::ConicGradient(ConicGradientItem {
                        rect: clip_rect,
                        center,
                        from_angle: *from_angle,
                        stops: Self::gradient_stops_unclamped(&resolved),
                        repeating: true,
                    }));
                }
            }
            BackgroundImage::RadialGradient {
                shape,
                size,
                position,
                stops,
            } => {
                let resolved = Self::normalize_color_stops(stops, style.color);
                if !resolved.is_empty() {
                    let (cx, cy, radius_x, radius_y) =
                        Self::radial_geometry(origin_rect, position, size, *shape, style.font_size);
                    let center = Point::new(cx - clip_rect.x(), cy - clip_rect.y());
                    self.list.push(DisplayItem::RadialGradient(RadialGradientItem {
                        rect: clip_rect,
                        center,
                        radii: Point::new(radius_x, radius_y),
                        stops: Self::gradient_stops(&resolved),
                        spread: GradientSpread::Pad,
                    }));
                }
            }
            BackgroundImage::RepeatingRadialGradient {
                shape,
                size,
                position,
                stops,
            } => {
                let resolved = Self::normalize_color_stops(stops, style.color);
                if !resolved.is_empty() {
                    let (cx, cy, radius_x, radius_y) =
                        Self::radial_geometry(origin_rect, position, size, *shape, style.font_size);
                    let center = Point::new(cx - clip_rect.x(), cy - clip_rect.y());
                    self.list.push(DisplayItem::RadialGradient(RadialGradientItem {
                        rect: clip_rect,
                        center,
                        radii: Point::new(radius_x, radius_y),
                        stops: Self::gradient_stops(&resolved),
                        spread: GradientSpread::Repeat,
                    }));
                }
            }
            BackgroundImage::Url(src) => {
                if let Some(image) = self.decode_image(src, Some(style), true) {
                    let img_w = image.css_width;
                    let img_h = image.css_height;
                    if img_w > 0.0 && img_h > 0.0 {
                        let (mut tile_w, mut tile_h) = Self::compute_background_size(
                            layer,
                            style.font_size,
                            origin_rect.width(),
                            origin_rect.height(),
                            img_w,
                            img_h,
                        );

                        let mut rounded_x = false;
                        let mut rounded_y = false;
                        if layer.repeat.x == BackgroundRepeatKeyword::Round {
                            tile_w = Self::round_tile_length(origin_rect.width(), tile_w);
                            rounded_x = true;
                        }
                        if layer.repeat.y == BackgroundRepeatKeyword::Round {
                            tile_h = Self::round_tile_length(origin_rect.height(), tile_h);
                            rounded_y = true;
                        }
                        if rounded_x ^ rounded_y
                            && matches!(
                                layer.size,
                                BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto)
                            )
                        {
                            let aspect = if img_h != 0.0 { img_w / img_h } else { 1.0 };
                            if rounded_x {
                                tile_h = tile_w / aspect;
                            } else {
                                tile_w = tile_h * aspect;
                            }
                        }

                        if tile_w > 0.0 && tile_h > 0.0 {
                            let (offset_x, offset_y) = Self::resolve_background_offset(
                                layer.position,
                                origin_rect.width(),
                                origin_rect.height(),
                                tile_w,
                                tile_h,
                                style.font_size,
                            );

                            let positions_x = Self::tile_positions(
                                layer.repeat.x,
                                origin_rect.x(),
                                origin_rect.width(),
                                tile_w,
                                offset_x,
                                clip_rect.min_x(),
                                clip_rect.max_x(),
                            );
                            let positions_y = Self::tile_positions(
                                layer.repeat.y,
                                origin_rect.y(),
                                origin_rect.height(),
                                tile_h,
                                offset_y,
                                clip_rect.min_y(),
                                clip_rect.max_y(),
                            );

                            let max_x = clip_rect.max_x();
                            let max_y = clip_rect.max_y();
                            let quality = Self::image_filter_quality(Some(style));
                            let image = Arc::new(image);

                            for ty in positions_y.iter().copied() {
                                for tx in positions_x.iter().copied() {
                                    if tx >= max_x || ty >= max_y {
                                        continue;
                                    }

                                    let tile_rect = Rect::from_xywh(tx, ty, tile_w, tile_h);
                                    let Some(intersection) = tile_rect.intersection(clip_rect) else {
                                        continue;
                                    };
                                    if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
                                        continue;
                                    }

                                    let scale_x = image.width as f32 / tile_w;
                                    let scale_y = image.height as f32 / tile_h;
                                    if !scale_x.is_finite() || !scale_y.is_finite() {
                                        continue;
                                    }

                                    let src_rect = Rect::from_xywh(
                                        (intersection.x() - tile_rect.x()) * scale_x,
                                        (intersection.y() - tile_rect.y()) * scale_y,
                                        intersection.width() * scale_x,
                                        intersection.height() * scale_y,
                                    );

                                    self.list.push(DisplayItem::Image(ImageItem {
                                        dest_rect: intersection,
                                        image: image.clone(),
                                        filter_quality: quality,
                                        src_rect: Some(src_rect),
                                    }));
                                }
                            }
                        }
                    }
                }
            }
            BackgroundImage::None => {}
        }

        if use_blend {
            self.list.push(DisplayItem::PopBlendMode);
        }
        if pushed_clip {
            self.list.push(DisplayItem::PopClip);
        }
    }

    fn emit_box_shadows_from_style(&mut self, rect: Rect, style: &ComputedStyle, inset: bool) {
        if style.box_shadow.is_empty() {
            return;
        }
        let rects = Self::background_rects(rect, style);
        let outer_radii = Self::border_radii(rect, style).clamped(rect.width(), rect.height());
        let inner_radii = Self::resolve_clip_radii(style, &rects, BackgroundBox::PaddingBox);
        let base_rect = if inset { rects.padding } else { rects.border };

        for shadow in &style.box_shadow {
            if shadow.inset != inset {
                continue;
            }
            let offset_x = Self::resolve_length_for_paint(&shadow.offset_x, style.font_size, rect.width());
            let offset_y = Self::resolve_length_for_paint(&shadow.offset_y, style.font_size, rect.width());
            let blur = Self::resolve_length_for_paint(&shadow.blur_radius, style.font_size, rect.width()).max(0.0);
            let spread = Self::resolve_length_for_paint(&shadow.spread_radius, style.font_size, rect.width()).max(-1e6);

            self.list.push(DisplayItem::BoxShadow(BoxShadowItem {
                rect: base_rect,
                radii: if inset { inner_radii } else { outer_radii },
                offset: Point::new(offset_x, offset_y),
                blur_radius: blur,
                spread_radius: spread,
                color: shadow.color,
                inset,
            }));
        }
    }

    fn emit_border_from_style(&mut self, rect: Rect, style: &ComputedStyle) {
        let widths = (
            Self::resolve_length_for_paint(&style.border_top_width, style.font_size, rect.width()),
            Self::resolve_length_for_paint(&style.border_right_width, style.font_size, rect.width()),
            Self::resolve_length_for_paint(&style.border_bottom_width, style.font_size, rect.width()),
            Self::resolve_length_for_paint(&style.border_left_width, style.font_size, rect.width()),
        );

        let sides = (
            BorderSide {
                width: widths.0,
                style: style.border_top_style,
                color: style.border_top_color,
            },
            BorderSide {
                width: widths.1,
                style: style.border_right_style,
                color: style.border_right_color,
            },
            BorderSide {
                width: widths.2,
                style: style.border_bottom_style,
                color: style.border_bottom_color,
            },
            BorderSide {
                width: widths.3,
                style: style.border_left_style,
                color: style.border_left_color,
            },
        );

        let any_visible = Self::border_side_visible(&sides.0)
            || Self::border_side_visible(&sides.1)
            || Self::border_side_visible(&sides.2)
            || Self::border_side_visible(&sides.3);
        if !any_visible {
            return;
        }

        let border_image = match &style.border_image.source {
            BorderImageSource::Image(bg) => {
                let source = match bg {
                    BackgroundImage::Url(src) => self.decode_image(src, Some(style), true).map(BorderImageSourceItem::Raster),
                    BackgroundImage::LinearGradient { .. }
                    | BackgroundImage::RepeatingLinearGradient { .. }
                    | BackgroundImage::RadialGradient { .. }
                    | BackgroundImage::RepeatingRadialGradient { .. }
                    | BackgroundImage::ConicGradient { .. }
                    | BackgroundImage::RepeatingConicGradient { .. } => {
                        Some(BorderImageSourceItem::Generated(bg.clone()))
                    }
                    BackgroundImage::None => None,
                };
                source.map(|source| BorderImageItem {
                    source,
                    slice: style.border_image.slice.clone(),
                    width: style.border_image.width.clone(),
                    outset: style.border_image.outset.clone(),
                    repeat: style.border_image.repeat,
                    current_color: style.color,
                })
            }
            BorderImageSource::None => None,
        };

        let radii = Self::border_radii(rect, style).clamped(rect.width(), rect.height());
        self.list.push(DisplayItem::Border(BorderItem {
            rect,
            top: sides.0,
            right: sides.1,
            bottom: sides.2,
            left: sides.3,
            image: border_image,
            radii,
        }));
    }

    fn emit_outline(&mut self, rect: Rect, style: &ComputedStyle) {
        let ow = style.outline_width.to_px();
        let outline_style = style.outline_style.to_border_style();
        if ow <= 0.0
            || matches!(
                outline_style,
                crate::style::types::BorderStyle::None | crate::style::types::BorderStyle::Hidden
            )
        {
            return;
        }
        let offset = style.outline_offset.to_px();
        let (color, invert) = style.outline_color.resolve(style.color);
        if ow > 0.0 && !color.is_transparent() {
            self.list.push(DisplayItem::Outline(OutlineItem {
                rect,
                width: ow,
                style: outline_style,
                color,
                offset,
                invert,
            }));
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

    fn emit_shaped_runs(
        &mut self,
        runs: &[ShapedRun],
        color: Rgba,
        baseline_y: f32,
        start_x: f32,
        shadows: &[TextShadowItem],
        style: Option<&ComputedStyle>,
    ) {
        let mut pen_x = start_x;
        for run in runs {
            let origin_x = if run.direction.is_rtl() {
                pen_x + run.advance
            } else {
                pen_x
            };
            let glyphs = self.glyphs_from_run(run, origin_x, baseline_y);
            let font_id = self.font_id_from_run(run);
            let emphasis = style.and_then(|s| self.build_emphasis(run, s, origin_x, baseline_y));

            self.list.push(DisplayItem::Text(TextItem {
                origin: Point::new(origin_x, baseline_y),
                glyphs,
                color,
                shadows: shadows.to_vec(),
                font_size: run.font_size,
                advance_width: run.advance,
                font_id: Some(font_id),
                synthetic_bold: run.synthetic_bold,
                synthetic_oblique: run.synthetic_oblique,
                emphasis,
                decorations: Vec::new(),
            }));

            pen_x += run.advance;
        }
    }

    fn emit_list_marker_runs(
        &mut self,
        runs: &[ShapedRun],
        color: Rgba,
        baseline_y: f32,
        start_x: f32,
        shadows: &[TextShadowItem],
        style: Option<&ComputedStyle>,
    ) {
        let mut pen_x = start_x;
        for run in runs {
            let origin_x = if run.direction.is_rtl() {
                pen_x + run.advance
            } else {
                pen_x
            };
            let glyphs = self.glyphs_from_run(run, origin_x, baseline_y);
            let font_id = self.font_id_from_run(run);
            let emphasis = style.and_then(|s| self.build_emphasis(run, s, origin_x, baseline_y));
            self.list.push(DisplayItem::ListMarker(ListMarkerItem {
                origin: Point::new(origin_x, baseline_y),
                glyphs,
                font_size: run.font_size,
                color,
                shadows: shadows.to_vec(),
                advance_width: run.advance,
                font_id: Some(font_id),
                synthetic_bold: run.synthetic_bold,
                synthetic_oblique: run.synthetic_oblique,
                emphasis,
                background: None,
            }));

            pen_x += run.advance;
        }
    }

    fn emit_text_decorations(
        &mut self,
        style: &ComputedStyle,
        runs: Option<&[ShapedRun]>,
        line_start: f32,
        line_width: f32,
        baseline_y: f32,
    ) {
        if line_width <= 0.0 {
            return;
        }

        let decorations = if !style.applied_text_decorations.is_empty() {
            style.applied_text_decorations.clone()
        } else if !style.text_decoration.lines.is_empty() {
            vec![ResolvedTextDecoration {
                decoration: style.text_decoration.clone(),
                skip_ink: style.text_decoration_skip_ink,
                underline_offset: style.text_underline_offset,
                underline_position: style.text_underline_position,
            }]
        } else {
            Vec::new()
        };
        if decorations.is_empty() {
            return;
        }

        let Some(metrics) = self.decoration_metrics(runs, style) else {
            return;
        };

        let mut paints = Vec::new();
        let mut min_y = f32::INFINITY;
        let mut max_y = f32::NEG_INFINITY;

        for deco in decorations {
            let decoration_color = deco.decoration.color.unwrap_or(style.color);
            if decoration_color.alpha_u8() == 0 {
                continue;
            }

            let used_thickness = match deco.decoration.thickness {
                TextDecorationThickness::Auto => None,
                TextDecorationThickness::FromFont => None,
                TextDecorationThickness::Length(l) => {
                    let unit = l.unit;
                    let resolved = if unit == LengthUnit::Percent {
                        l.resolve_against(style.font_size)
                    } else if unit.is_font_relative() {
                        l.resolve_with_font_size(style.font_size)
                    } else if unit.is_viewport_relative() {
                        match self.viewport {
                            Some((vw, vh)) => l.resolve_with_viewport(vw, vh),
                            None => l.to_px(),
                        }
                    } else {
                        l.to_px()
                    };
                    Some(resolved)
                }
            };

            let underline_offset = self.resolve_underline_offset_value(deco.underline_offset, style);
            let mut paint = DecorationPaint {
                style: deco.decoration.style,
                color: decoration_color,
                underline: None,
                overline: None,
                line_through: None,
            };

            if deco.decoration.lines.contains(TextDecorationLine::UNDERLINE) {
                let thickness = used_thickness.unwrap_or(metrics.underline_thickness);
                let center = baseline_y
                    - self.underline_position(&metrics, deco.underline_position, underline_offset, thickness);
                let segments = if matches!(deco.skip_ink, TextDecorationSkipInk::Auto | TextDecorationSkipInk::All) {
                    runs.map(|r| {
                        self.build_underline_segments(
                            r,
                            line_start,
                            line_width,
                            center,
                            thickness,
                            baseline_y,
                            deco.skip_ink,
                        )
                    })
                } else {
                    None
                };
                paint.underline = Some(DecorationStroke {
                    center,
                    thickness,
                    segments,
                });
                let half_extent = Self::stroke_half_extent(deco.decoration.style, thickness);
                min_y = min_y.min(center - half_extent);
                max_y = max_y.max(center + half_extent);
            }
            if deco.decoration.lines.contains(TextDecorationLine::OVERLINE) {
                let thickness = used_thickness.unwrap_or(metrics.underline_thickness);
                let center = baseline_y - metrics.ascent;
                paint.overline = Some(DecorationStroke {
                    center,
                    thickness,
                    segments: None,
                });
                let half_extent = Self::stroke_half_extent(deco.decoration.style, thickness);
                min_y = min_y.min(center - half_extent);
                max_y = max_y.max(center + half_extent);
            }
            if deco.decoration.lines.contains(TextDecorationLine::LINE_THROUGH) {
                let thickness = used_thickness.unwrap_or(metrics.strike_thickness);
                let center = baseline_y - metrics.strike_pos;
                paint.line_through = Some(DecorationStroke {
                    center,
                    thickness,
                    segments: None,
                });
                let half_extent = Self::stroke_half_extent(deco.decoration.style, thickness);
                min_y = min_y.min(center - half_extent);
                max_y = max_y.max(center + half_extent);
            }

            if paint.underline.is_some() || paint.overline.is_some() || paint.line_through.is_some() {
                paints.push(paint);
            }
        }

        if paints.is_empty() {
            return;
        }

        let bounds = Rect::from_xywh(line_start, min_y, line_width, (max_y - min_y).max(0.0));
        self.list.push(DisplayItem::TextDecoration(TextDecorationItem {
            bounds,
            line_start,
            line_width,
            decorations: paints,
        }));
    }

    fn stroke_half_extent(style: TextDecorationStyle, thickness: f32) -> f32 {
        match style {
            TextDecorationStyle::Double => thickness * 2.5,
            TextDecorationStyle::Wavy => thickness * 2.0,
            _ => thickness * 0.5,
        }
    }

    fn resolve_underline_offset_value(&self, offset: TextUnderlineOffset, style: &ComputedStyle) -> f32 {
        match offset {
            TextUnderlineOffset::Auto => 0.0,
            TextUnderlineOffset::Length(l) => {
                if l.unit == LengthUnit::Percent {
                    l.resolve_against(style.font_size)
                } else if l.unit.is_font_relative() {
                    l.resolve_with_font_size(style.font_size)
                } else if l.unit.is_viewport_relative() {
                    match self.viewport {
                        Some((vw, vh)) => l.resolve_with_viewport(vw, vh),
                        None => l.to_px(),
                    }
                } else if l.unit.is_absolute() {
                    l.to_px()
                } else {
                    l.value * style.font_size
                }
            }
        }
    }

    fn underline_position(
        &self,
        metrics: &DecorationMetrics,
        position: TextUnderlinePosition,
        offset: f32,
        thickness: f32,
    ) -> f32 {
        let under_base = -metrics.descent - thickness * 0.5;
        let base = match position {
            TextUnderlinePosition::Auto | TextUnderlinePosition::FromFont => metrics.underline_pos,
            TextUnderlinePosition::Under | TextUnderlinePosition::UnderLeft | TextUnderlinePosition::UnderRight => {
                metrics.underline_pos.min(under_base)
            }
            TextUnderlinePosition::Left | TextUnderlinePosition::Right => metrics.underline_pos,
        };

        metrics.underline_position_with_offset(base, offset)
    }

    fn decoration_metrics(&self, runs: Option<&[ShapedRun]>, style: &ComputedStyle) -> Option<DecorationMetrics> {
        let mut metrics_source = runs.and_then(|rs| {
            rs.iter()
                .find_map(|run| run.font.metrics().ok().map(|m| (m, run.font_size)))
        });

        if metrics_source.is_none() {
            let italic = matches!(style.font_style, crate::style::types::FontStyle::Italic);
            let oblique = matches!(style.font_style, crate::style::types::FontStyle::Oblique(_));
            let stretch = crate::text::font_db::FontStretch::from_percentage(style.font_stretch.to_percentage());
            metrics_source = self
                .font_ctx
                .get_font_full(
                    &style.font_family,
                    style.font_weight.to_u16(),
                    if italic {
                        crate::text::font_db::FontStyle::Italic
                    } else if oblique {
                        crate::text::font_db::FontStyle::Oblique
                    } else {
                        crate::text::font_db::FontStyle::Normal
                    },
                    stretch,
                )
                .or_else(|| self.font_ctx.get_sans_serif())
                .and_then(|font| font.metrics().ok().map(|m| (m, style.font_size)));
        }

        if let Some((metrics, size)) = metrics_source {
            let scale = size / (metrics.units_per_em as f32);

            let underline_pos = metrics.underline_position as f32 * scale;
            let underline_thickness = (metrics.underline_thickness as f32 * scale).max(1.0);
            let descent = (metrics.descent as f32 * scale).abs();
            let strike_pos = metrics
                .strikeout_position
                .map(|p| p as f32 * scale)
                .unwrap_or_else(|| metrics.ascent as f32 * scale * 0.3);
            let strike_thickness = metrics
                .strikeout_thickness
                .map(|t| t as f32 * scale)
                .unwrap_or(underline_thickness);
            let ascent = metrics.ascent as f32 * scale;

            Some(DecorationMetrics {
                underline_pos,
                underline_thickness,
                strike_pos,
                strike_thickness,
                ascent,
                descent,
            })
        } else {
            // Fallback heuristic metrics when we cannot obtain font metrics.
            let size = style.font_size.max(1.0);
            let ascent = size * 0.8;
            let descent = size - ascent;
            let underline_thickness = (size * 0.05).max(1.0);
            let underline_pos = descent * 0.5;
            let strike_pos = ascent * 0.4;

            Some(DecorationMetrics {
                underline_pos,
                underline_thickness,
                strike_pos,
                strike_thickness: underline_thickness,
                ascent,
                descent,
            })
        }
    }

    fn build_underline_segments(
        &self,
        runs: &[ShapedRun],
        line_start: f32,
        line_width: f32,
        center: f32,
        thickness: f32,
        baseline_y: f32,
        skip_ink: TextDecorationSkipInk,
    ) -> Vec<(f32, f32)> {
        if line_width <= 0.0 {
            return Vec::new();
        }

        let band_half = (thickness * 0.5).abs();
        let band_top = center - band_half;
        let band_bottom = center + band_half;
        let mut exclusions = collect_underline_exclusions(
            runs,
            line_start,
            baseline_y,
            band_top,
            band_bottom,
            skip_ink == TextDecorationSkipInk::All,
        );

        let mut segments = subtract_intervals((line_start, line_start + line_width), &mut exclusions);
        if segments.is_empty() && skip_ink != TextDecorationSkipInk::All {
            // Never drop the underline entirely when skipping ink; fall back to a full span.
            segments.push((line_start, line_start + line_width));
        }

        segments
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

    fn build_emphasis(
        &self,
        run: &ShapedRun,
        style: &ComputedStyle,
        origin_x: f32,
        baseline_y: f32,
    ) -> Option<TextEmphasis> {
        if style.text_emphasis_style.is_none() {
            return None;
        }
        let mark_color = style.text_emphasis_color.unwrap_or(style.color);
        let mut ascent = run.font_size * 0.8;
        let mut descent = run.font_size * 0.2;
        if let Ok(metrics) = run.font.metrics() {
            let scaled = metrics.scale(run.font_size);
            ascent = scaled.ascent;
            descent = scaled.descent;
        }
        let mark_size = (style.font_size * 0.5).max(1.0);
        let gap = mark_size * 0.3;
        let resolved_position = match style.text_emphasis_position {
            TextEmphasisPosition::Auto => TextEmphasisPosition::Over,
            other => other,
        };
        let center_y = match resolved_position {
            TextEmphasisPosition::Over | TextEmphasisPosition::OverLeft | TextEmphasisPosition::OverRight => {
                baseline_y - ascent - gap - mark_size * 0.5
            }
            TextEmphasisPosition::Under | TextEmphasisPosition::UnderLeft | TextEmphasisPosition::UnderRight => {
                baseline_y + descent + gap + mark_size * 0.5
            }
            TextEmphasisPosition::Auto => baseline_y - ascent - gap - mark_size * 0.5,
        };

        let mut marks = Vec::new();
        let mut seen_clusters = HashSet::new();
        let run_origin = if run.direction.is_rtl() {
            origin_x + run.advance
        } else {
            origin_x
        };
        for glyph in &run.glyphs {
            if !seen_clusters.insert(glyph.cluster) {
                continue;
            }
            let text_byte = glyph.cluster as usize;
            if text_byte < run.text.len() {
                if let Some(ch) = run.text[text_byte..].chars().next() {
                    if ch.is_whitespace() || ch.is_control() {
                        continue;
                    }
                }
            }
            let center_x = match run.direction {
                crate::text::pipeline::Direction::RightToLeft => run_origin - (glyph.x_offset + glyph.x_advance * 0.5),
                _ => run_origin + glyph.x_offset + glyph.x_advance * 0.5,
            };
            marks.push(EmphasisMark {
                center: Point::new(center_x, center_y),
            });
        }

        let text = if let TextEmphasisStyle::String(ref s) = style.text_emphasis_style {
            if s.is_empty() {
                None
            } else {
                let mut mark_style = style.clone();
                mark_style.font_size = style.font_size * 0.5;
                match self.shaper.shape(s, &mark_style, &self.font_ctx) {
                    Ok(mark_runs) if !mark_runs.is_empty() => {
                        let mark_font_id = self.font_id_from_run(&mark_runs[0]);
                        let mut glyphs = Vec::new();
                        let mut width = 0.0;
                        let mut ascent: f32 = 0.0;
                        let mut descent: f32 = 0.0;
                        for r in &mark_runs {
                            if let Ok(m) = r.font.metrics() {
                                let scaled = m.scale(r.font_size);
                                ascent = ascent.max(scaled.ascent);
                                descent = descent.max(scaled.descent);
                            }
                        }
                        if ascent == 0.0 && descent == 0.0 {
                            ascent = mark_style.font_size * 0.8;
                            descent = mark_style.font_size * 0.2;
                        }
                        for r in mark_runs {
                            let mark_origin = if r.direction.is_rtl() { width + r.advance } else { width };
                            for g in r.glyphs {
                                let x = match r.direction {
                                    crate::text::pipeline::Direction::RightToLeft => mark_origin - g.x_offset,
                                    _ => mark_origin + g.x_offset,
                                };
                                glyphs.push(GlyphInstance {
                                    glyph_id: g.glyph_id,
                                    offset: Point::new(x, -g.y_offset),
                                    advance: g.x_advance,
                                });
                            }
                            width += r.advance;
                        }
                        Some(EmphasisText {
                            glyphs,
                            font_id: Some(mark_font_id),
                            font_size: mark_style.font_size,
                            width,
                            height: ascent + descent,
                            baseline_offset: ascent,
                        })
                    }
                    _ => None,
                }
            }
        } else {
            None
        };

        Some(TextEmphasis {
            style: style.text_emphasis_style.clone(),
            color: mark_color,
            position: resolved_position,
            size: mark_size,
            marks,
            text,
        })
    }

    fn font_id_from_run(&self, run: &ShapedRun) -> FontId {
        FontId {
            family: run.font.family.clone(),
            weight: run.font.weight.value(),
            style: run.font.style,
            stretch: run.font.stretch,
        }
    }

    fn text_shadows_from_style(style: Option<&ComputedStyle>) -> Vec<TextShadowItem> {
        style
            .map(|s| {
                resolve_text_shadows(s)
                    .into_iter()
                    .map(|shadow| TextShadowItem {
                        offset: Point::new(shadow.offset_x, shadow.offset_y),
                        blur_radius: shadow.blur_radius,
                        color: shadow.color,
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    fn emit_replaced_placeholder(&mut self, replaced_type: &ReplacedType, fragment: &FragmentNode, rect: Rect) {
        let placeholder_color = Rgba::rgb(200, 200, 200);
        self.list.push(DisplayItem::FillRect(FillRectItem {
            rect,
            color: placeholder_color,
        }));

        let stroke_color = Rgba::rgb(150, 150, 150);
        self.list.push(DisplayItem::StrokeRect(StrokeRectItem {
            rect,
            color: stroke_color,
            width: 1.0,
            blend_mode: BlendMode::Normal,
        }));

        let label = match replaced_type {
            ReplacedType::Video { .. } => Some("video"),
            ReplacedType::Iframe { .. } => Some("iframe"),
            ReplacedType::Canvas => Some("canvas"),
            ReplacedType::Embed { .. } => Some("embed"),
            ReplacedType::Object { .. } => Some("object"),
            _ => None,
        };

        if let Some(label_text) = label {
            let label_style = fragment.style.as_deref().map(|style| {
                let mut clone = style.clone();
                clone.color = Rgba::rgb(120, 120, 120);
                clone
            });
            let inset = 2.0;
            let label_rect = Rect::from_xywh(
                rect.x() + inset,
                rect.y() + inset,
                (rect.width() - inset * 2.0).max(0.0),
                (rect.height() - inset * 2.0).max(0.0),
            );
            let label_style_ref = label_style
                .as_ref()
                .map(|s| s as &ComputedStyle)
                .or_else(|| fragment.style.as_deref());
            let _ = self.emit_text_with_style(label_text, label_style_ref, label_rect);
        }
    }

    fn emit_alt_text(&mut self, alt: &str, fragment: &FragmentNode, rect: Rect) -> bool {
        self.emit_text_with_style(alt, fragment.style.as_deref(), rect)
    }

    fn emit_text_with_style(&mut self, text: &str, style: Option<&ComputedStyle>, rect: Rect) -> bool {
        let text = text.trim();
        if text.is_empty() {
            return false;
        }

        let Some(style) = style else {
            return self.emit_naive_text(text, rect, None);
        };

        let mut runs = match self.shaper.shape(text, style, &self.font_ctx) {
            Ok(r) => r,
            Err(_) => return self.emit_naive_text(text, rect, Some(style)),
        };
        InlineTextItem::apply_spacing_to_runs(&mut runs, text, style.letter_spacing, style.word_spacing);

        let metrics_scaled = Self::resolve_scaled_metrics(style, &self.font_ctx);
        let line_height = compute_line_height_with_metrics(style, metrics_scaled.as_ref());
        let metrics = InlineTextItem::metrics_from_runs(&runs, line_height, style.font_size);
        let half_leading = ((metrics.line_height - (metrics.ascent + metrics.descent)) / 2.0).max(0.0);
        let baseline = rect.y() + half_leading + metrics.baseline_offset;

        let shadows = Self::text_shadows_from_style(Some(style));
        self.emit_shaped_runs(&runs, style.color, baseline, rect.x(), &shadows, Some(style));
        true
    }

    fn emit_naive_text(&mut self, text: &str, rect: Rect, style: Option<&ComputedStyle>) -> bool {
        let font_size = style.map(|s| s.font_size).unwrap_or(16.0);
        let color = style.map(|s| s.color).unwrap_or(Rgba::BLACK);
        let shadows = Self::text_shadows_from_style(style);
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
            shadows,
            font_size,
            advance_width,
            font_id: None,
            synthetic_bold: 0.0,
            synthetic_oblique: 0.0,
            emphasis: None,
            decorations: Vec::new(),
        }));
        true
    }

    fn decode_image(&self, src: &str, style: Option<&ComputedStyle>, decorative: bool) -> Option<ImageData> {
        let cache = self.image_cache.as_ref()?;
        let image = match cache.load(src) {
            Ok(img) => img,
            Err(_) if src.trim_start().starts_with('<') => cache.render_svg(src).ok()?,
            Err(_) => return None,
        };
        let image_resolution = style.map(|s| s.image_resolution).unwrap_or_default();
        let orientation = style
            .map(|s| s.image_orientation.resolve(image.orientation, decorative))
            .unwrap_or_else(|| ImageOrientation::default().resolve(image.orientation, decorative));
        let Some((css_w, css_h)) = image.css_dimensions(orientation, &image_resolution, self.device_pixel_ratio, None)
        else {
            return None;
        };
        let rgba = image.to_oriented_rgba(orientation);
        let (w, h) = rgba.dimensions();
        if w == 0 || h == 0 {
            return None;
        }
        Some(ImageData::new(w, h, css_w, css_h, rgba.into_raw()))
    }

    fn image_filter_quality(style: Option<&ComputedStyle>) -> ImageFilterQuality {
        match style.map(|s| s.image_rendering) {
            Some(ImageRendering::CrispEdges) | Some(ImageRendering::Pixelated) => ImageFilterQuality::Nearest,
            _ => ImageFilterQuality::Linear,
        }
    }
}

#[derive(Debug, Clone)]
struct DecorationMetrics {
    underline_pos: f32,
    underline_thickness: f32,
    strike_pos: f32,
    strike_thickness: f32,
    ascent: f32,
    descent: f32,
}

impl DecorationMetrics {
    fn underline_position_with_offset(&self, base: f32, offset: f32) -> f32 {
        let direction = if base >= 0.0 { 1.0 } else { -1.0 };
        base + offset * direction
    }
}

fn collect_underline_exclusions(
    runs: &[ShapedRun],
    line_start: f32,
    baseline_y: f32,
    band_top: f32,
    band_bottom: f32,
    skip_all: bool,
) -> Vec<(f32, f32)> {
    let mut intervals = Vec::new();
    let tolerance = 0.5;

    let mut pen_x = line_start;
    for run in runs {
        let face = match ttf_parser::Face::parse(&run.font.data, run.font.index) {
            Ok(f) => f,
            Err(_) => continue,
        };
        let units_per_em = face.units_per_em() as f32;
        if units_per_em == 0.0 {
            continue;
        }
        let scale = run.font_size / units_per_em;
        let run_origin = if run.direction.is_rtl() {
            pen_x + run.advance
        } else {
            pen_x
        };

        for glyph in &run.glyphs {
            let glyph_x = match run.direction {
                crate::text::pipeline::Direction::RightToLeft => run_origin - glyph.x_offset,
                _ => run_origin + glyph.x_offset,
            };
            let glyph_y = baseline_y - glyph.y_offset;
            if let Some(bbox) = face.glyph_bounding_box(ttf_parser::GlyphId(glyph.glyph_id as u16)) {
                let left = glyph_x + bbox.x_min as f32 * scale - tolerance;
                let right = glyph_x + bbox.x_max as f32 * scale + tolerance;
                let top = glyph_y - bbox.y_max as f32 * scale - tolerance;
                let bottom = glyph_y - bbox.y_min as f32 * scale + tolerance;

                if skip_all || (bottom >= band_top && top <= band_bottom) {
                    intervals.push((left, right));
                }
            }
        }

        pen_x += run.advance;
    }

    intervals
}

fn subtract_intervals(total: (f32, f32), exclusions: &mut Vec<(f32, f32)>) -> Vec<(f32, f32)> {
    exclusions.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
    let mut start = total.0;
    let mut allowed = Vec::new();

    for &(ex_start, ex_end) in exclusions.iter() {
        if ex_end <= start {
            continue;
        }
        if ex_start > total.1 {
            break;
        }
        let seg_end = ex_start.min(total.1);
        if seg_end > start {
            allowed.push((start, seg_end));
        }
        start = ex_end.max(start);
        if start >= total.1 {
            break;
        }
    }

    if start < total.1 {
        allowed.push((start, total.1));
    }

    allowed
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
    use crate::style::color::{Color, Rgba};
    use crate::style::display::Display;
    use crate::style::position::Position;
    use crate::style::types::{
        BackgroundImage, BackgroundLayer, BackgroundRepeat, ImageRendering, MixBlendMode, TextDecorationLine,
    };
    use crate::style::ComputedStyle;
    use crate::tree::box_tree::ReplacedType;
    use std::path::PathBuf;

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
                sizes: None,
                srcset: Vec::new(),
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
    fn builder_emits_text_decorations() {
        let mut style = ComputedStyle::default();
        style.text_decoration.lines = TextDecorationLine::UNDERLINE;
        style.text_decoration.color = Some(Rgba::BLACK);
        let fragment = FragmentNode::new_text_styled(
            Rect::from_xywh(0.0, 0.0, 50.0, 16.0),
            "Hi".to_string(),
            12.0,
            Arc::new(style),
        );

        let builder = DisplayListBuilder::new();
        let list = builder.build(&fragment);
        assert!(
            list.items().iter().any(|i| matches!(i, DisplayItem::TextDecoration(_))),
            "display list should include text decoration items"
        );
    }

    #[test]
    fn test_builder_image_fragment() {
        let fragment = create_image_fragment(
            0.0,
            0.0,
            100.0,
            100.0,
            "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E",
        );
        let builder = DisplayListBuilder::with_image_cache(ImageCache::new());
        let list = builder.build(&fragment);

        assert_eq!(list.len(), 1);
        assert!(matches!(list.items()[0], DisplayItem::Image(_)));
    }

    #[test]
    fn default_builder_decodes_images_without_explicit_cache() {
        // 1x1 blue inline SVG
        let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1"><rect width="1" height="1" fill="blue"/></svg>"#;
        let fragment = create_image_fragment(0.0, 0.0, 10.0, 10.0, svg);

        let list = DisplayListBuilder::new().build(&fragment);

        assert_eq!(list.len(), 1, "image content should decode instead of placeholder");
        let DisplayItem::Image(img) = &list.items()[0] else {
            panic!("expected image display item");
        };
        assert_eq!(img.image.width, 1);
        assert_eq!(img.image.height, 1);
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
            style.z_index = Some(z);
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
    fn background_blend_mode_emits_push_and_pop() {
        let mut style = ComputedStyle::default();
        style.background_color = Rgba::BLUE;
        style.set_background_layers(vec![BackgroundLayer {
            image: Some(BackgroundImage::LinearGradient {
                angle: 0.0,
                stops: vec![
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::RED),
                        position: Some(0.0),
                    },
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::RED),
                        position: Some(1.0),
                    },
                ],
            }),
            repeat: BackgroundRepeat::no_repeat(),
            blend_mode: MixBlendMode::Multiply,
            ..BackgroundLayer::default()
        }]);
        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 20.0, 10.0), vec![], Arc::new(style));

        let list = DisplayListBuilder::new().build(&fragment);
        let mut push_idx = None;
        let mut gradient_idx = None;
        let mut pop_idx = None;
        for (idx, item) in list.items().iter().enumerate() {
            match item {
                DisplayItem::PushBlendMode(mode) => {
                    push_idx = Some(idx);
                    assert_eq!(mode.mode, BlendMode::Multiply);
                }
                DisplayItem::LinearGradient(_) => gradient_idx = Some(idx),
                DisplayItem::PopBlendMode => pop_idx = Some(idx),
                _ => {}
            }
        }

        assert!(push_idx.is_some(), "blend push missing");
        assert!(pop_idx.is_some(), "blend pop missing");
        assert!(gradient_idx.is_some(), "background gradient missing");
        assert!(
            push_idx.unwrap() < gradient_idx.unwrap() && gradient_idx.unwrap() < pop_idx.unwrap(),
            "blend mode should wrap background layer"
        );
    }

    #[test]
    fn background_url_resolves_relative_to_base() {
        // Create a 1x1 PNG on disk.
        let mut path: PathBuf = std::env::temp_dir();
        path.push(format!(
            "fastrender_dl_base_url_{}_{}.png",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let img = image::RgbaImage::from_raw(1, 1, vec![0, 0, 0, 255]).expect("raw rgba");
        img.save(&path).expect("write png");

        let dir = path.parent().unwrap().to_path_buf();
        let base_url = format!("file://{}", dir.display());

        let mut style = ComputedStyle::default();
        style.set_background_layers(vec![BackgroundLayer {
            image: Some(BackgroundImage::Url(
                path.file_name().unwrap().to_str().unwrap().to_string(),
            )),
            repeat: BackgroundRepeat::no_repeat(),
            ..BackgroundLayer::default()
        }]);

        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], Arc::new(style));
        let list = DisplayListBuilder::new().with_base_url(base_url).build(&fragment);

        // Expect one image item in the list (background image decoded).
        assert!(
            list.items().iter().any(|item| matches!(item, DisplayItem::Image(_))),
            "background image should decode via base URL"
        );
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
    fn visibility_hidden_skips_display_items() {
        let mut style = ComputedStyle::default();
        style.visibility = crate::style::computed::Visibility::Hidden;
        let fragment = FragmentNode::new_text_styled(
            Rect::from_xywh(0.0, 0.0, 100.0, 20.0),
            "hidden".to_string(),
            16.0,
            Arc::new(style),
        );
        let builder = DisplayListBuilder::new();
        let list = builder.build(&fragment);

        assert!(list.is_empty());
    }

    #[test]
    fn outline_emits_stroke_rect() {
        let mut style = ComputedStyle::default();
        style.outline_style = crate::style::types::OutlineStyle::Solid;
        style.outline_width = Length::px(2.0);
        style.outline_color = crate::style::types::OutlineColor::Color(Rgba::RED);
        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], Arc::new(style));

        let builder = DisplayListBuilder::new();
        let list = builder.build(&fragment);
        assert!(list.items().iter().any(|item| matches!(item, DisplayItem::Outline(_))), "outline should emit outline item");
    }

    #[test]
    fn outline_emits_even_when_clipped() {
        let mut style = ComputedStyle::default();
        style.outline_style = crate::style::types::OutlineStyle::Solid;
        style.outline_width = Length::px(2.0);
        style.outline_color = crate::style::types::OutlineColor::Color(Rgba::RED);
        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], Arc::new(style));
        let clips = vec![None].into_iter().collect();
        let builder = DisplayListBuilder::new();
        let list = builder.build_with_clips(&fragment, &clips);
        assert!(list.items().iter().any(|item| matches!(item, DisplayItem::Outline(_))), "outline should be emitted even when fragment is clipped");
    }

    #[test]
    fn hidden_fragment_skipped_with_clips() {
        let mut style = ComputedStyle::default();
        style.visibility = crate::style::computed::Visibility::Hidden;
        let fragment = FragmentNode::new_block_styled(
            Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
            vec![FragmentNode::new_text(
                Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
                "hidden".to_string(),
                12.0,
            )],
            Arc::new(style),
        );
        let clips = HashSet::from([None]);
        let builder = DisplayListBuilder::new();
        let list = builder.build_with_clips(&fragment, &clips);
        assert!(list.is_empty(), "hidden fragments should not emit display items");
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
        let image = create_image_fragment(
            0.0,
            40.0,
            50.0,
            50.0,
            "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E",
        );

        let inner = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 120.0, 100.0), vec![text1, text2, image]);
        let outer = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 200.0, 200.0), vec![inner]);

        let builder = DisplayListBuilder::with_image_cache(ImageCache::new());
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
                    src: "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E".to_string(),
                    alt: None,
                    sizes: None,
                    srcset: Vec::new(),
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
                    src: "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E".to_string(),
                    alt: None,
                    sizes: None,
                    srcset: Vec::new(),
                },
            },
            children: vec![],
            style: Some(Arc::new(style)),
        };

        let tree = FragmentTree::with_viewport(fragment, crate::geometry::Size::new(200.0, 200.0));
        let builder = DisplayListBuilder::with_image_cache(ImageCache::new());
        let list = builder.build_tree(&tree);

        assert_eq!(list.len(), 1);
        let DisplayItem::Image(img) = &list.items()[0] else {
            panic!("Expected image item");
        };
        // free_x = 100 - 1 = 99; but we align with 10vw (20px), so dest_rect.x should be ~20.
        assert!((img.dest_rect.x() - 20.0).abs() < 0.5);
    }

    #[test]
    fn image_rendering_pixelated_sets_nearest_filter_quality() {
        let mut style = ComputedStyle::default();
        style.image_rendering = ImageRendering::Pixelated;
        let fragment = FragmentNode {
            bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
            content: FragmentContent::Replaced {
                box_id: None,
                replaced_type: ReplacedType::Image {
                    src: "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E".to_string(),
                    alt: None,
                    sizes: None,
                    srcset: Vec::new(),
                },
            },
            children: vec![],
            style: Some(Arc::new(style)),
        };

        let list = DisplayListBuilder::new().build(&fragment);
        let DisplayItem::Image(img) = &list.items()[0] else {
            panic!("Expected image item");
        };
        assert_eq!(img.filter_quality, ImageFilterQuality::Nearest);
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
                    sizes: None,
                    srcset: Vec::new(),
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

    #[test]
    fn missing_image_without_alt_emits_placeholder() {
        let fragment = FragmentNode::new_replaced(
            Rect::from_xywh(0.0, 0.0, 40.0, 20.0),
            ReplacedType::Image {
                src: String::new(),
                alt: None,
                sizes: None,
                srcset: Vec::new(),
            },
        );
        let builder = DisplayListBuilder::new();
        let list = builder.build(&fragment);

        assert_eq!(list.len(), 2, "expected fill + stroke placeholder items");
        assert!(matches!(list.items()[0], DisplayItem::FillRect(_)));
        assert!(matches!(list.items()[1], DisplayItem::StrokeRect(_)));
    }

    #[test]
    fn non_image_replaced_uses_labeled_placeholder() {
        let fragment = FragmentNode::new_replaced(
            Rect::from_xywh(0.0, 0.0, 40.0, 20.0),
            ReplacedType::Video {
                src: String::new(),
                poster: None,
            },
        );
        let builder = DisplayListBuilder::new();
        let list = builder.build(&fragment);

        assert_eq!(list.len(), 3, "fill, stroke, and label text expected");
        assert!(matches!(list.items()[0], DisplayItem::FillRect(_)));
        assert!(matches!(list.items()[1], DisplayItem::StrokeRect(_)));
        assert!(matches!(list.items()[2], DisplayItem::Text(_)));
    }

    #[test]
    fn video_poster_decodes_before_placeholder() {
        let poster = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"4\" height=\"2\"><rect width=\"4\" height=\"2\" fill=\"red\"/></svg>";
        let fragment = FragmentNode::new_replaced(
            Rect::from_xywh(0.0, 0.0, 40.0, 20.0),
            ReplacedType::Video {
                src: String::new(),
                poster: Some(poster.to_string()),
            },
        );
        let builder = DisplayListBuilder::with_image_cache(ImageCache::new());
        let list = builder.build(&fragment);

        assert_eq!(list.len(), 1, "poster image should render as content");
        assert!(matches!(list.items()[0], DisplayItem::Image(_)));
    }
}
