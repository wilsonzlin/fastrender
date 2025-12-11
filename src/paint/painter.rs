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

use crate::css::types::ColorStop;
use crate::error::{RenderError, Result};
use crate::geometry::{Point, Rect};
use crate::image_loader::ImageCache;
use crate::layout::contexts::inline::baseline::compute_line_height_with_metrics;
use crate::layout::contexts::inline::line_builder::TextItem;
use crate::paint::blur::apply_gaussian_blur;
use crate::paint::display_list::BorderRadii;
use crate::paint::object_fit::{compute_object_fit, default_object_position};
use crate::paint::rasterize::fill_rounded_rect;
use crate::paint::stacking::creates_stacking_context;
use crate::paint::text_shadow::{resolve_text_shadows, PathBounds, ResolvedTextShadow};
#[cfg(test)]
use crate::style::color::Color;
use crate::style::color::Rgba;
use crate::style::display::Display;
use crate::style::position::Position;
use crate::style::types::{
    BackgroundAttachment, BackgroundImage, BackgroundLayer, BackgroundPosition, BackgroundRepeatKeyword,
    BackgroundSize, BackgroundSizeComponent, BackgroundSizeKeyword, BorderStyle as CssBorderStyle, ImageRendering,
    ObjectFit, TextDecorationLine, TextDecorationSkipInk, TextDecorationStyle, TextDecorationThickness,
};
use crate::style::types::{FilterColor, FilterFunction, MixBlendMode, Overflow};
use crate::style::values::{Length, LengthUnit};
use crate::style::ComputedStyle;
use crate::text::font_db::{FontStretch, FontStyle, ScaledMetrics};
use crate::text::font_loader::FontContext;
use crate::text::pipeline::{ShapedRun, ShapingPipeline};
use crate::tree::box_tree::ReplacedType;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use image::DynamicImage;
use std::borrow::Cow;
use std::sync::Arc;
use tiny_skia::{
    BlendMode as SkiaBlendMode, FilterQuality, IntSize, LinearGradient, Mask, MaskType, Paint, PathBuilder, Pattern,
    Pixmap, PixmapPaint, PremultipliedColorU8, RadialGradient, Rect as SkiaRect, SpreadMode, Stroke, Transform,
};

/// Main painter that rasterizes a FragmentTree to pixels
pub struct Painter {
    /// The pixmap being painted to
    pixmap: Pixmap,
    /// CSS-to-device scale factor (device pixel ratio)
    scale: f32,
    /// Logical viewport width in CSS px
    css_width: f32,
    /// Logical viewport height in CSS px
    css_height: f32,
    /// Background color
    background: Rgba,
    /// Text shaping pipeline
    shaper: ShapingPipeline,
    /// Font context for resolution
    font_ctx: FontContext,
    /// Image cache for replaced content
    image_cache: ImageCache,
}

#[derive(Debug, Clone)]
enum ResolvedFilter {
    Blur(f32),
    Brightness(f32),
    Contrast(f32),
    Grayscale(f32),
    Sepia(f32),
    Saturate(f32),
    HueRotate(f32),
    Invert(f32),
    Opacity(f32),
    DropShadow {
        offset_x: f32,
        offset_y: f32,
        blur_radius: f32,
        spread: f32,
        color: Rgba,
    },
}

#[derive(Debug)]
enum DisplayCommand {
    Background {
        rect: Rect,
        style: Arc<ComputedStyle>,
    },
    Border {
        rect: Rect,
        style: Arc<ComputedStyle>,
    },
    Text {
        rect: Rect,
        baseline_offset: f32,
        text: String,
        runs: Option<Vec<ShapedRun>>,
        style: Arc<ComputedStyle>,
    },
    Replaced {
        rect: Rect,
        replaced_type: ReplacedType,
        style: Arc<ComputedStyle>,
    },
    Outline {
        rect: Rect,
        style: Arc<ComputedStyle>,
    },
    StackingContext {
        rect: Rect,
        opacity: f32,
        transform: Option<Transform>,
        blend_mode: MixBlendMode,
        isolated: bool,
        filters: Vec<ResolvedFilter>,
        backdrop_filters: Vec<ResolvedFilter>,
        radii: BorderRadii,
        clip: Option<(Rect, BorderRadii, bool, bool)>,
        commands: Vec<DisplayCommand>,
    },
}

fn is_positioned(style: &ComputedStyle) -> bool {
    !matches!(style.position, Position::Static)
}

fn is_float_for_stacking(style: &ComputedStyle) -> bool {
    if is_positioned(style) {
        return false;
    }
    style.float.is_floating()
}

fn is_inline_level(style: &ComputedStyle, fragment: &FragmentNode) -> bool {
    let is_inline_display = matches!(
        style.display,
        Display::Inline | Display::InlineBlock | Display::InlineFlex | Display::InlineGrid | Display::InlineTable
    );

    let is_inline_content = matches!(
        fragment.content,
        FragmentContent::Inline { .. } | FragmentContent::Text { .. } | FragmentContent::Line { .. }
    );

    is_inline_display || is_inline_content
}

#[derive(Copy, Clone)]
enum EdgeOrientation {
    Horizontal,
    Vertical,
}

#[derive(Copy, Clone)]
enum BorderEdge {
    Top,
    Right,
    Bottom,
    Left,
}

impl BorderEdge {
    fn orientation(self) -> EdgeOrientation {
        match self {
            BorderEdge::Top | BorderEdge::Bottom => EdgeOrientation::Horizontal,
            BorderEdge::Left | BorderEdge::Right => EdgeOrientation::Vertical,
        }
    }

    /// Returns a pair of parallel paths offset by `offset` from the center line.
    fn parallel_lines(
        &self,
        x1: f32,
        y1: f32,
        x2: f32,
        y2: f32,
        offset: f32,
    ) -> (Option<tiny_skia::Path>, Option<tiny_skia::Path>) {
        match self.orientation() {
            EdgeOrientation::Horizontal => {
                let mut first = PathBuilder::new();
                first.move_to(x1, y1 - offset);
                first.line_to(x2, y2 - offset);

                let mut second = PathBuilder::new();
                second.move_to(x1, y1 + offset);
                second.line_to(x2, y2 + offset);

                (first.finish(), second.finish())
            }
            EdgeOrientation::Vertical => {
                let mut first = PathBuilder::new();
                first.move_to(x1 - offset, y1);
                first.line_to(x2 - offset, y2);

                let mut second = PathBuilder::new();
                second.move_to(x1 + offset, y1);
                second.line_to(x2 + offset, y2);

                (first.finish(), second.finish())
            }
        }
    }

    fn groove_ridge_colors(self, base: &Rgba, style: CssBorderStyle) -> (Rgba, Rgba) {
        let lighten = |c: &Rgba| shade_color(c, 1.25);
        let darken = |c: &Rgba| shade_color(c, 0.75);

        let (first_light, second_light) = match style {
            CssBorderStyle::Groove => (false, true),
            CssBorderStyle::Ridge => (true, false),
            _ => (false, false),
        };

        let (first, second) = match self {
            BorderEdge::Top | BorderEdge::Left => (first_light, second_light),
            BorderEdge::Right | BorderEdge::Bottom => (!first_light, !second_light),
        };

        (
            if first { lighten(base) } else { darken(base) },
            if second { lighten(base) } else { darken(base) },
        )
    }

    fn inset_outset_color(self, base: &Rgba, style: CssBorderStyle) -> Rgba {
        match style {
            CssBorderStyle::Inset => match self {
                BorderEdge::Top | BorderEdge::Left => shade_color(base, 0.75),
                BorderEdge::Right | BorderEdge::Bottom => shade_color(base, 1.25),
            },
            CssBorderStyle::Outset => match self {
                BorderEdge::Top | BorderEdge::Left => shade_color(base, 1.25),
                BorderEdge::Right | BorderEdge::Bottom => shade_color(base, 0.75),
            },
            _ => *base,
        }
    }
}

fn shade_color(color: &Rgba, factor: f32) -> Rgba {
    let clamp = |v: f32| v.max(0.0).min(255.0) as u8;
    let r = clamp(color.r as f32 * factor);
    let g = clamp(color.g as f32 * factor);
    let b = clamp(color.b as f32 * factor);
    Rgba::new(r, g, b, color.a)
}

impl Painter {
    fn resolve_scaled_metrics(&self, style: &ComputedStyle) -> Option<ScaledMetrics> {
        let italic = matches!(style.font_style, crate::style::types::FontStyle::Italic);
        let oblique = matches!(style.font_style, crate::style::types::FontStyle::Oblique(_));
        let stretch = FontStretch::from_percentage(style.font_stretch.to_percentage());

        self.font_ctx
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
            .or_else(|| self.font_ctx.get_sans_serif())
            .and_then(|font| font.metrics().ok())
            .map(|m| m.scale(style.font_size))
    }

    fn filter_quality_for_image(style: Option<&ComputedStyle>) -> FilterQuality {
        match style.map(|s| s.image_rendering) {
            Some(ImageRendering::CrispEdges) | Some(ImageRendering::Pixelated) => FilterQuality::Nearest,
            _ => FilterQuality::Bilinear,
        }
    }

    /// Creates a new painter with the given dimensions
    pub fn new(width: u32, height: u32, background: Rgba) -> Result<Self> {
        Self::with_resources_scaled(width, height, background, FontContext::new(), ImageCache::new(), 1.0)
    }

    /// Creates a painter with explicit font and image resources
    pub fn with_resources(
        width: u32,
        height: u32,
        background: Rgba,
        font_ctx: FontContext,
        image_cache: ImageCache,
    ) -> Result<Self> {
        Self::with_resources_scaled(width, height, background, font_ctx, image_cache, 1.0)
    }

    /// Creates a painter with explicit font/image resources and a device scale.
    pub fn with_resources_scaled(
        width: u32,
        height: u32,
        background: Rgba,
        font_ctx: FontContext,
        image_cache: ImageCache,
        scale: f32,
    ) -> Result<Self> {
        let scale = if scale.is_finite() && scale > 0.0 { scale } else { 1.0 };
        let device_w = ((width as f32) * scale).round().max(1.0) as u32;
        let device_h = ((height as f32) * scale).round().max(1.0) as u32;
        let pixmap = Pixmap::new(device_w, device_h).ok_or_else(|| RenderError::InvalidParameters {
            message: format!("Failed to create pixmap {}x{}", device_w, device_h),
        })?;

        Ok(Self {
            pixmap,
            scale,
            css_width: width as f32,
            css_height: height as f32,
            background,
            shaper: ShapingPipeline::new(),
            font_ctx,
            image_cache,
        })
    }

    #[inline]
    fn device_length(&self, value: f32) -> f32 {
        value * self.scale
    }

    #[inline]
    fn device_rect(&self, rect: Rect) -> Rect {
        Rect::from_xywh(
            rect.x() * self.scale,
            rect.y() * self.scale,
            rect.width() * self.scale,
            rect.height() * self.scale,
        )
    }

    #[inline]
    fn device_radii(&self, radii: BorderRadii) -> BorderRadii {
        BorderRadii {
            top_left: radii.top_left * self.scale,
            top_right: radii.top_right * self.scale,
            bottom_right: radii.bottom_right * self.scale,
            bottom_left: radii.bottom_left * self.scale,
        }
    }

    #[inline]
    fn device_transform(&self, transform: Option<Transform>) -> Option<Transform> {
        transform.map(|mut t| {
            t.tx *= self.scale;
            t.ty *= self.scale;
            t
        })
    }

    #[allow(dead_code)]
    #[inline]
    fn device_dimensions(&self, width: f32, height: f32) -> Option<(u32, u32)> {
        if width <= 0.0 || height <= 0.0 {
            return None;
        }
        let w = ((width * self.scale).round()).max(0.0) as u32;
        let h = ((height * self.scale).round()).max(0.0) as u32;
        Some((w, h))
    }

    /// Paints a fragment tree and returns the resulting pixmap
    pub fn paint(mut self, tree: &FragmentTree) -> Result<Pixmap> {
        // Fill background
        self.fill_background();

        // Build display list in stacking-context order then paint
        let mut items = Vec::new();
        self.collect_stacking_context(&tree.root, Point::ZERO, None, true, &mut items);
        for item in items {
            self.execute_command(item)?;
        }

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

    /// Collect display commands respecting stacking-context ordering.
    ///
    /// This follows the simplified CSS painting order for a stacking context:
    /// element background/border → negative z-index stacking contexts →
    /// in-flow/non-positioned content → z-index:auto/0 stacking contexts →
    /// positive z-index stacking contexts.
    fn collect_stacking_context(
        &self,
        fragment: &FragmentNode,
        offset: Point,
        parent_style: Option<&ComputedStyle>,
        is_root_context: bool,
        items: &mut Vec<DisplayCommand>,
    ) {
        if let Some(style) = fragment.style.as_deref() {
            if !matches!(style.visibility, crate::style::computed::Visibility::Visible) {
                return;
            }
        }

        let abs_bounds = Rect::from_xywh(
            fragment.bounds.x() + offset.x,
            fragment.bounds.y() + offset.y,
            fragment.bounds.width(),
            fragment.bounds.height(),
        );

        let style_ref = fragment.style.as_deref();
        let establishes_context = style_ref
            .map(|s| creates_stacking_context(s, parent_style, is_root_context))
            .unwrap_or(is_root_context);

        // Collect commands for this subtree locally so we can wrap the context (opacity, etc.)
        let mut local_commands = Vec::new();

        if !establishes_context {
            self.enqueue_background_and_borders(fragment, abs_bounds, &mut local_commands);
            self.enqueue_content(fragment, abs_bounds, &mut local_commands);

            let next_offset = Point::new(abs_bounds.x(), abs_bounds.y());
            let mut negative_contexts = Vec::new();
            let mut zero_contexts = Vec::new();
            let mut positive_contexts = Vec::new();
            let mut blocks = Vec::new();
            let mut floats = Vec::new();
            let mut inlines = Vec::new();
            let mut positioned = Vec::new();

            for (idx, child) in fragment.children.iter().enumerate() {
                if let Some(style) = child.style.as_deref() {
                    if creates_stacking_context(style, style_ref, false) {
                        let z = style.z_index.unwrap_or(0);
                        if z < 0 {
                            negative_contexts.push((z, idx));
                        } else if z == 0 {
                            zero_contexts.push((z, idx));
                        } else {
                            positive_contexts.push((z, idx));
                        }
                        continue;
                    }
                    if is_positioned(style) {
                        positioned.push(idx);
                    } else if is_float_for_stacking(style) {
                        floats.push(idx);
                    } else if is_inline_level(style, child) {
                        inlines.push(idx);
                    } else {
                        blocks.push(idx);
                    }
                } else {
                    if matches!(
                        child.content,
                        FragmentContent::Inline { .. } | FragmentContent::Text { .. } | FragmentContent::Line { .. }
                    ) {
                        inlines.push(idx);
                    } else {
                        blocks.push(idx);
                    }
                }
            }

            negative_contexts.sort_by(|(z1, i1), (z2, i2)| z1.cmp(z2).then_with(|| i1.cmp(i2)));
            for (_, idx) in negative_contexts {
                self.collect_stacking_context(
                    &fragment.children[idx],
                    next_offset,
                    style_ref,
                    false,
                    &mut local_commands,
                );
            }

            for idx in blocks {
                self.collect_stacking_context(
                    &fragment.children[idx],
                    next_offset,
                    style_ref,
                    false,
                    &mut local_commands,
                );
            }
            for idx in floats {
                self.collect_stacking_context(
                    &fragment.children[idx],
                    next_offset,
                    style_ref,
                    false,
                    &mut local_commands,
                );
            }
            for idx in inlines {
                self.collect_stacking_context(
                    &fragment.children[idx],
                    next_offset,
                    style_ref,
                    false,
                    &mut local_commands,
                );
            }
            for idx in positioned {
                self.collect_stacking_context(
                    &fragment.children[idx],
                    next_offset,
                    style_ref,
                    false,
                    &mut local_commands,
                );
            }

            zero_contexts.sort_by(|(z1, i1), (z2, i2)| z1.cmp(z2).then_with(|| i1.cmp(i2)));
            for (_, idx) in zero_contexts {
                self.collect_stacking_context(
                    &fragment.children[idx],
                    next_offset,
                    style_ref,
                    false,
                    &mut local_commands,
                );
            }

            positive_contexts.sort_by(|(z1, i1), (z2, i2)| z1.cmp(z2).then_with(|| i1.cmp(i2)));
            for (_, idx) in positive_contexts {
                self.collect_stacking_context(
                    &fragment.children[idx],
                    next_offset,
                    style_ref,
                    false,
                    &mut local_commands,
                );
            }

            items.extend(local_commands);
            return;
        }

        // Stacking context: paint own background/border first
        self.enqueue_background_and_borders(fragment, abs_bounds, &mut local_commands);

        // Partition children into stacking-context buckets and normal content
        let mut negative_contexts = Vec::new();
        let mut zero_contexts = Vec::new();
        let mut positive_contexts = Vec::new();
        let mut blocks = Vec::new();
        let mut floats = Vec::new();
        let mut inlines = Vec::new();
        let mut positioned = Vec::new();

        for (idx, child) in fragment.children.iter().enumerate() {
            if let Some(style) = child.style.as_deref() {
                if creates_stacking_context(style, style_ref, false) {
                    let z = style.z_index.unwrap_or(0);
                    if z < 0 {
                        negative_contexts.push((z, idx));
                    } else if z == 0 {
                        zero_contexts.push((z, idx));
                    } else {
                        positive_contexts.push((z, idx));
                    }
                    continue;
                }

                if is_positioned(style) {
                    positioned.push(idx);
                } else if is_float_for_stacking(style) {
                    floats.push(idx);
                } else if is_inline_level(style, child) {
                    inlines.push(idx);
                } else {
                    blocks.push(idx);
                }
            } else if matches!(
                child.content,
                FragmentContent::Inline { .. } | FragmentContent::Text { .. } | FragmentContent::Line { .. }
            ) {
                inlines.push(idx);
            }
            // No style: treat as block-level unless inline-like
            else {
                blocks.push(idx);
            }
        }

        let child_offset = Point::new(abs_bounds.x(), abs_bounds.y());

        negative_contexts.sort_by(|(z1, i1), (z2, i2)| z1.cmp(z2).then_with(|| i1.cmp(i2)));
        for (_, idx) in negative_contexts {
            self.collect_stacking_context(
                &fragment.children[idx],
                child_offset,
                style_ref,
                false,
                &mut local_commands,
            );
        }

        // In-flow/non-positioned content for this context
        self.enqueue_content(fragment, abs_bounds, &mut local_commands);
        for idx in blocks {
            self.collect_stacking_context(
                &fragment.children[idx],
                child_offset,
                style_ref,
                false,
                &mut local_commands,
            );
        }
        for idx in floats {
            self.collect_stacking_context(
                &fragment.children[idx],
                child_offset,
                style_ref,
                false,
                &mut local_commands,
            );
        }
        for idx in inlines {
            self.collect_stacking_context(
                &fragment.children[idx],
                child_offset,
                style_ref,
                false,
                &mut local_commands,
            );
        }
        for idx in positioned {
            self.collect_stacking_context(
                &fragment.children[idx],
                child_offset,
                style_ref,
                false,
                &mut local_commands,
            );
        }

        zero_contexts.sort_by(|(z1, i1), (z2, i2)| z1.cmp(z2).then_with(|| i1.cmp(i2)));
        for (_, idx) in zero_contexts {
            self.collect_stacking_context(
                &fragment.children[idx],
                child_offset,
                style_ref,
                false,
                &mut local_commands,
            );
        }

        positive_contexts.sort_by(|(z1, i1), (z2, i2)| z1.cmp(z2).then_with(|| i1.cmp(i2)));
        for (_, idx) in positive_contexts {
            self.collect_stacking_context(
                &fragment.children[idx],
                child_offset,
                style_ref,
                false,
                &mut local_commands,
            );
        }

        // Wrap the stacking context if it applies an effect (opacity/transform); otherwise flatten
        let opacity = style_ref.map(|s| s.opacity).unwrap_or(1.0).clamp(0.0, 1.0);
        let transform = build_transform(style_ref, abs_bounds);
        let blend_mode = style_ref.map(|s| s.mix_blend_mode).unwrap_or(MixBlendMode::Normal);
        let isolated = style_ref
            .map(|s| matches!(s.isolation, crate::style::types::Isolation::Isolate))
            .unwrap_or(false);
        let filters = style_ref
            .as_deref()
            .map(|s| resolve_filters(&s.filter, s, self.scale))
            .unwrap_or_default();
        let has_filters = !filters.is_empty();
        let backdrop_filters = style_ref
            .as_deref()
            .map(|s| resolve_filters(&s.backdrop_filter, s, self.scale))
            .unwrap_or_default();
        let has_backdrop = !backdrop_filters.is_empty();
        let clip = style_ref.and_then(|style| {
            let clip_x = matches!(
                style.overflow_x,
                Overflow::Hidden | Overflow::Scroll | Overflow::Auto | Overflow::Clip
            ) || style.containment.paint;
            let clip_y = matches!(
                style.overflow_y,
                Overflow::Hidden | Overflow::Scroll | Overflow::Auto | Overflow::Clip
            ) || style.containment.paint;
            if !clip_x && !clip_y {
                return None;
            }
            let rects = background_rects(
                abs_bounds.x(),
                abs_bounds.y(),
                abs_bounds.width(),
                abs_bounds.height(),
                style,
            );
            let clip_rect = rects.padding;
            if clip_rect.width() <= 0.0 || clip_rect.height() <= 0.0 {
                return None;
            }
            let clip_radii = resolve_clip_radii(style, &rects, crate::style::types::BackgroundBox::PaddingBox);
            Some((clip_rect, clip_radii, clip_x, clip_y))
        });
        if opacity < 1.0
            || transform.is_some()
            || !matches!(blend_mode, MixBlendMode::Normal)
            || isolated
            || has_filters
            || has_backdrop
            || clip.is_some()
        {
            let radii = resolve_border_radii(style_ref, abs_bounds);
            items.push(DisplayCommand::StackingContext {
                rect: abs_bounds,
                opacity,
                transform,
                blend_mode,
                isolated,
                filters,
                backdrop_filters,
                radii,
                clip,
                commands: local_commands,
            });
        } else {
            items.extend(local_commands);
        }
    }

    /// Enqueue background and border commands for a fragment (no children).
    fn enqueue_background_and_borders(
        &self,
        fragment: &FragmentNode,
        abs_bounds: Rect,
        items: &mut Vec<DisplayCommand>,
    ) {
        let Some(style) = fragment.style.clone() else { return };

        let has_background =
            style.background_color.alpha_u8() > 0 || style.background_layers.iter().any(|l| l.image.is_some());
        if has_background {
            items.push(DisplayCommand::Background {
                rect: abs_bounds,
                style: style.clone(),
            });
        }

        let has_border = style.border_top_width.to_px() > 0.0
            || style.border_right_width.to_px() > 0.0
            || style.border_bottom_width.to_px() > 0.0
            || style.border_left_width.to_px() > 0.0;
        if has_border {
            items.push(DisplayCommand::Border {
                rect: abs_bounds,
                style: style.clone(),
            });
        }

        let outline_style = style.outline_style.to_border_style();
        if outline_style != CssBorderStyle::None
            && outline_style != CssBorderStyle::Hidden
            && style.outline_width.to_px() > 0.0
        {
            let ow = style.outline_width.to_px();
            let expand = style.outline_offset.to_px() + ow * 0.5;
            let outline_rect = Rect::from_xywh(
                abs_bounds.x() - expand,
                abs_bounds.y() - expand,
                abs_bounds.width() + 2.0 * expand,
                abs_bounds.height() + 2.0 * expand,
            );
            items.push(DisplayCommand::Outline {
                rect: outline_rect,
                style,
            });
        }
    }

    /// Enqueue paint commands for the fragment's own content (text/replaced).
    fn enqueue_content(&self, fragment: &FragmentNode, abs_bounds: Rect, items: &mut Vec<DisplayCommand>) {
        match &fragment.content {
            FragmentContent::Text {
                text,
                baseline_offset,
                shaped,
                ..
            } => {
                if let Some(style) = fragment.style.clone() {
                    items.push(DisplayCommand::Text {
                        rect: abs_bounds,
                        baseline_offset: *baseline_offset,
                        text: text.clone(),
                        runs: shaped.clone(),
                        style,
                    });
                }
            }
            FragmentContent::Replaced { replaced_type, .. } => {
                if let Some(style) = fragment.style.clone() {
                    items.push(DisplayCommand::Replaced {
                        rect: abs_bounds,
                        replaced_type: replaced_type.clone(),
                        style,
                    });
                }
            }
            _ => {}
        }
    }

    fn execute_command(&mut self, command: DisplayCommand) -> Result<()> {
        match command {
            DisplayCommand::Background { rect, style } => {
                self.paint_background(rect.x(), rect.y(), rect.width(), rect.height(), &style);
            }
            DisplayCommand::Border { rect, style } => {
                self.paint_borders(rect.x(), rect.y(), rect.width(), rect.height(), &style);
            }
            DisplayCommand::Outline { rect, style } => {
                self.paint_outline(rect.x(), rect.y(), rect.width(), rect.height(), &style);
            }
            DisplayCommand::Text {
                rect,
                baseline_offset,
                text,
                runs,
                style,
            } => {
                let color = style.color;
                let shaped_runs = runs
                    .clone()
                    .or_else(|| self.shaper.shape(&text, &style, &self.font_ctx).ok());
                let baseline_y = rect.y() + baseline_offset;
                if let Some(ref shaped) = shaped_runs {
                    self.paint_shaped_runs(shaped, rect.x(), baseline_y, color, Some(&style));
                } else {
                    self.paint_text(&text, Some(&style), rect.x(), baseline_y, style.font_size, color);
                }
                self.paint_text_decoration(&style, shaped_runs.as_deref(), rect.x(), baseline_y, rect.width());
                self.paint_text_emphasis(&style, shaped_runs.as_deref(), rect.x(), baseline_y);
            }
            DisplayCommand::Replaced {
                rect,
                replaced_type,
                style,
            } => self.paint_replaced(
                &replaced_type,
                Some(&style),
                rect.x(),
                rect.y(),
                rect.width(),
                rect.height(),
            ),
            DisplayCommand::StackingContext {
                rect: context_rect,
                opacity,
                transform,
                blend_mode,
                isolated,
                filters,
                backdrop_filters,
                radii,
                clip,
                commands,
            } => {
                if opacity <= 0.0 {
                    return Ok(());
                }

                let Some(bounds) = stacking_context_bounds(
                    &commands,
                    &filters,
                    &backdrop_filters,
                    context_rect,
                    transform.as_ref(),
                    clip.as_ref(),
                ) else {
                    return Ok(());
                };

                // Reduce layer size by translating to the top-left of the local bounds.
                let offset = Point::new(bounds.min_x(), bounds.min_y());
                let translated = translate_commands(commands, -offset.x, -offset.y);

                let device_bounds = self.device_rect(bounds);
                let width = device_bounds.width().ceil().max(0.0);
                let height = device_bounds.height().ceil().max(0.0);
                if width <= 0.0 || height <= 0.0 {
                    return Ok(());
                }

                let root_rect = Rect::from_xywh(
                    context_rect.x() - offset.x,
                    context_rect.y() - offset.y,
                    context_rect.width(),
                    context_rect.height(),
                );
                let device_root_rect = self.device_rect(root_rect);
                let (mut unclipped, clipped): (Vec<DisplayCommand>, Vec<DisplayCommand>) =
                    translated.into_iter().partition(|cmd| match cmd {
                        DisplayCommand::Outline { .. } => true,
                        DisplayCommand::Background { rect, .. } | DisplayCommand::Border { rect, .. } => {
                            (rect.x() - root_rect.x()).abs() < f32::EPSILON
                                && (rect.y() - root_rect.y()).abs() < f32::EPSILON
                                && (rect.width() - root_rect.width()).abs() < f32::EPSILON
                                && (rect.height() - root_rect.height()).abs() < f32::EPSILON
                        }
                        _ => false,
                    });

                let layer = match Pixmap::new(width as u32, height as u32) {
                    Some(p) => p,
                    None => return Ok(()),
                };

                let mut base_painter = Painter {
                    pixmap: layer,
                    scale: self.scale,
                    css_width: self.css_width,
                    css_height: self.css_height,
                    background: Rgba::new(0, 0, 0, 0.0),
                    shaper: ShapingPipeline::new(),
                    font_ctx: self.font_ctx.clone(),
                    image_cache: self.image_cache.clone(),
                };
                for cmd in unclipped.drain(..) {
                    base_painter.execute_command(cmd)?;
                }

                if !clipped.is_empty() {
                    let clip_layer = match Pixmap::new(width as u32, height as u32) {
                        Some(p) => p,
                        None => return Ok(()),
                    };
                    let mut clip_painter = Painter {
                        pixmap: clip_layer,
                        scale: self.scale,
                        css_width: self.css_width,
                        css_height: self.css_height,
                        background: Rgba::new(0, 0, 0, 0.0),
                        shaper: ShapingPipeline::new(),
                        font_ctx: self.font_ctx.clone(),
                        image_cache: self.image_cache.clone(),
                    };
                    for cmd in clipped {
                        clip_painter.execute_command(cmd)?;
                    }
                    let mut clip_pixmap = clip_painter.pixmap;
                    if let Some((mut clip_rect, mut clip_radii, clip_x, clip_y)) = clip {
                        if !clip_x {
                            clip_rect = Rect::from_xywh(offset.x, clip_rect.y(), bounds.width(), clip_rect.height());
                            clip_radii = BorderRadii::ZERO;
                        }
                        if !clip_y {
                            clip_rect = Rect::from_xywh(clip_rect.x(), offset.y, clip_rect.width(), bounds.height());
                            clip_radii = BorderRadii::ZERO;
                        }
                        let local_clip = Rect::from_xywh(
                            clip_rect.x() - offset.x,
                            clip_rect.y() - offset.y,
                            clip_rect.width(),
                            clip_rect.height(),
                        );
                        apply_clip_mask_rect(&mut clip_pixmap, self.device_rect(local_clip), self.device_radii(clip_radii));
                    }
                    base_painter.pixmap.draw_pixmap(
                        0,
                        0,
                        clip_pixmap.as_ref(),
                        &PixmapPaint::default(),
                        Transform::identity(),
                        None,
                    );
                }

                let mut layer_pixmap = base_painter.pixmap;
                if !filters.is_empty() {
                    apply_filters(&mut layer_pixmap, &filters, self.scale);
                }

                let device_radii = self.device_radii(radii);
                if !radii.is_zero() || !filters.is_empty() {
                    let (out_l, out_t, out_r, out_b) = filter_outset(&filters);
                    let device_out_l = self.device_length(out_l);
                    let device_out_t = self.device_length(out_t);
                    let device_out_r = self.device_length(out_r);
                    let device_out_b = self.device_length(out_b);
                    let clip_rect = Rect::from_xywh(
                        device_root_rect.x() - device_out_l,
                        device_root_rect.y() - device_out_t,
                        device_root_rect.width() + device_out_l + device_out_r,
                        device_root_rect.height() + device_out_t + device_out_b,
                    );
                    apply_clip_mask_rect(&mut layer_pixmap, clip_rect, device_radii);
                }

                if !backdrop_filters.is_empty() {
                    apply_backdrop_filters(
                        &mut self.pixmap,
                        &device_root_rect,
                        &backdrop_filters,
                        device_radii,
                        self.scale,
                    );
                }

                let mut final_transform =
                    self.device_transform(transform).unwrap_or_else(Transform::identity);
                final_transform = final_transform.pre_concat(Transform::from_translate(
                    self.device_length(offset.x),
                    self.device_length(offset.y),
                ));
                let fallback_blend = if isolated {
                    SkiaBlendMode::SourceOver
                } else {
                    map_blend_mode(blend_mode)
                };
                if is_hsl_blend(blend_mode) && !isolated {
                    if let Some(mut transformed) = Pixmap::new(self.pixmap.width(), self.pixmap.height()) {
                        let mut paint = PixmapPaint::default();
                        paint.opacity = 1.0;
                        paint.blend_mode = SkiaBlendMode::SourceOver;
                        transformed.draw_pixmap(0, 0, layer_pixmap.as_ref(), &paint, final_transform, None);
                        composite_hsl_layer(
                            &mut self.pixmap,
                            &transformed,
                            opacity.min(1.0),
                            blend_mode,
                            Some(device_bounds),
                        );
                    } else {
                        let mut paint = PixmapPaint::default();
                        paint.opacity = opacity.min(1.0);
                        paint.blend_mode = fallback_blend;
                        self.pixmap
                            .draw_pixmap(0, 0, layer_pixmap.as_ref(), &paint, final_transform, None);
                    }
                } else {
                    let mut paint = PixmapPaint::default();
                    paint.opacity = opacity.min(1.0);
                    paint.blend_mode = fallback_blend;
                    self.pixmap
                        .draw_pixmap(0, 0, layer_pixmap.as_ref(), &paint, final_transform, None);
                }
            }
        }
        Ok(())
    }

    /// Paints the background of a fragment
    fn paint_background(&mut self, x: f32, y: f32, width: f32, height: f32, style: &ComputedStyle) {
        let rects = background_rects(x, y, width, height, style);
        let scaled_rects = self.scale_background_rects(&rects);
        let fallback_layer = BackgroundLayer::default();
        let color_clip_layer = style.background_layers.first().unwrap_or(&fallback_layer);
        let color_clip_rect = match color_clip_layer.clip {
            crate::style::types::BackgroundBox::BorderBox => scaled_rects.border,
            crate::style::types::BackgroundBox::PaddingBox => scaled_rects.padding,
            crate::style::types::BackgroundBox::ContentBox => scaled_rects.content,
        };

        if color_clip_rect.width() <= 0.0 || color_clip_rect.height() <= 0.0 {
            return;
        }

        let color_clip_radii =
            self.device_radii(resolve_clip_radii(style, &rects, color_clip_layer.clip));

        if style.background_color.alpha_u8() > 0 {
            let _ = fill_rounded_rect(
                &mut self.pixmap,
                color_clip_rect.x(),
                color_clip_rect.y(),
                color_clip_rect.width(),
                color_clip_rect.height(),
                &color_clip_radii,
                style.background_color,
            );
        }

        for layer in style.background_layers.iter().rev() {
            if let Some(image) = &layer.image {
                self.paint_background_image_layer(&rects, style, layer, image);
            }
        }
    }

    fn scale_background_rects(&self, rects: &BackgroundRects) -> BackgroundRects {
        BackgroundRects {
            border: self.device_rect(rects.border),
            padding: self.device_rect(rects.padding),
            content: self.device_rect(rects.content),
        }
    }

    fn paint_background_image_layer(
        &mut self,
        rects: &BackgroundRects,
        style: &ComputedStyle,
        layer: &BackgroundLayer,
        bg: &BackgroundImage,
    ) {
        let is_local = layer.attachment == BackgroundAttachment::Local;
        let clip_box = if is_local {
            match layer.clip {
                crate::style::types::BackgroundBox::ContentBox => crate::style::types::BackgroundBox::ContentBox,
                _ => crate::style::types::BackgroundBox::PaddingBox,
            }
        } else {
            layer.clip
        };
        let clip_rect_css = match clip_box {
            crate::style::types::BackgroundBox::BorderBox => rects.border,
            crate::style::types::BackgroundBox::PaddingBox => rects.padding,
            crate::style::types::BackgroundBox::ContentBox => rects.content,
        };
        let clip_radii = resolve_clip_radii(style, rects, clip_box);
        let origin_rect_css = if layer.attachment == BackgroundAttachment::Fixed {
            Rect::from_xywh(0.0, 0.0, self.css_width, self.css_height)
        } else if is_local {
            match layer.origin {
                crate::style::types::BackgroundBox::ContentBox => rects.content,
                _ => rects.padding,
            }
        } else {
            match layer.origin {
                crate::style::types::BackgroundBox::BorderBox => rects.border,
                crate::style::types::BackgroundBox::PaddingBox => rects.padding,
                crate::style::types::BackgroundBox::ContentBox => rects.content,
            }
        };

        if clip_rect_css.width() <= 0.0 || clip_rect_css.height() <= 0.0 {
            return;
        }
        if origin_rect_css.width() <= 0.0 || origin_rect_css.height() <= 0.0 {
            return;
        }

        let clip_rect = self.device_rect(clip_rect_css);
        let clip_radii = self.device_radii(clip_radii);

        let clip_mask = if clip_radii.is_zero() {
            None
        } else {
            build_rounded_rect_mask(clip_rect, clip_radii, self.pixmap.width(), self.pixmap.height())
        };

        match bg {
            BackgroundImage::LinearGradient { angle, stops } => {
                let resolved = normalize_color_stops(stops, style.color);
                self.paint_linear_gradient(
                    origin_rect_css,
                    clip_rect_css,
                    clip_mask.as_ref(),
                    *angle,
                    &resolved,
                    SpreadMode::Pad,
                    layer.blend_mode,
                );
            }
            BackgroundImage::RadialGradient { stops } => {
                let resolved = normalize_color_stops(stops, style.color);
                self.paint_radial_gradient(
                    origin_rect_css,
                    clip_rect_css,
                    clip_mask.as_ref(),
                    &resolved,
                    SpreadMode::Pad,
                    layer.blend_mode,
                );
            }
            BackgroundImage::RepeatingLinearGradient { angle, stops } => {
                let resolved = normalize_color_stops(stops, style.color);
                self.paint_linear_gradient(
                    origin_rect_css,
                    clip_rect_css,
                    clip_mask.as_ref(),
                    *angle,
                    &resolved,
                    SpreadMode::Repeat,
                    layer.blend_mode,
                );
            }
            BackgroundImage::RepeatingRadialGradient { stops } => {
                let resolved = normalize_color_stops(stops, style.color);
                self.paint_radial_gradient(
                    origin_rect_css,
                    clip_rect_css,
                    clip_mask.as_ref(),
                    &resolved,
                    SpreadMode::Repeat,
                    layer.blend_mode,
                );
            }
            BackgroundImage::None => {
                return;
            }
            BackgroundImage::Url(src) => {
                let image = match self.image_cache.load(src) {
                    Ok(img) => img,
                    Err(_) => return,
                };

                let pixmap = match Self::dynamic_image_to_pixmap(&image) {
                    Some(p) => p,
                    None => return,
                };
                let img_w = pixmap.width() as f32;
                let img_h = pixmap.height() as f32;
                if img_w <= 0.0 || img_h <= 0.0 {
                    return;
                }

                let (mut tile_w, mut tile_h) = compute_background_size(
                    layer,
                    style.font_size,
                    origin_rect_css.width(),
                    origin_rect_css.height(),
                    img_w,
                    img_h,
                );
                if tile_w <= 0.0 || tile_h <= 0.0 {
                    return;
                }

                let mut rounded_x = false;
                let mut rounded_y = false;
                if layer.repeat.x == BackgroundRepeatKeyword::Round {
                    tile_w = round_tile_length(origin_rect_css.width(), tile_w);
                    rounded_x = true;
                }
                if layer.repeat.y == BackgroundRepeatKeyword::Round {
                    tile_h = round_tile_length(origin_rect_css.height(), tile_h);
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

                let (offset_x, offset_y) = resolve_background_offset(
                    layer.position,
                    origin_rect_css.width(),
                    origin_rect_css.height(),
                    tile_w,
                    tile_h,
                    style.font_size,
                );

                let positions_x = tile_positions(
                    layer.repeat.x,
                    origin_rect_css.x(),
                    origin_rect_css.width(),
                    tile_w,
                    offset_x,
                    clip_rect_css.min_x(),
                    clip_rect_css.max_x(),
                );
                let positions_y = tile_positions(
                    layer.repeat.y,
                    origin_rect_css.y(),
                    origin_rect_css.height(),
                    tile_h,
                    offset_y,
                    clip_rect_css.min_y(),
                    clip_rect_css.max_y(),
                );

                let max_x = clip_rect_css.max_x();
                let max_y = clip_rect_css.max_y();
                let quality = Self::filter_quality_for_image(Some(style));

                for ty in positions_y.iter().copied() {
                    for tx in positions_x.iter().copied() {
                        if tx >= max_x || ty >= max_y {
                            continue;
                        }
                        self.paint_background_tile(
                            &pixmap,
                            tx,
                            ty,
                            tile_w,
                            tile_h,
                            clip_rect_css,
                            clip_mask.as_ref(),
                            layer.blend_mode,
                            quality,
                        );
                    }
                }
            }
        }
    }

    fn paint_linear_gradient(
        &mut self,
        gradient_rect: Rect,
        paint_rect: Rect,
        clip_mask: Option<&Mask>,
        angle: f32,
        stops: &[(f32, Rgba)],
        spread: SpreadMode,
        blend_mode: MixBlendMode,
    ) {
        if stops.is_empty() {
            return;
        }

        let gradient_rect = self.device_rect(gradient_rect);
        let paint_rect = self.device_rect(paint_rect);

        let skia_stops = gradient_stops(stops);
        let rad = angle.to_radians();
        let dx = rad.sin();
        let dy = -rad.cos(); // CSS 0deg points up
        let len = 0.5 * (gradient_rect.width() * dx.abs() + gradient_rect.height() * dy.abs());
        let cx = gradient_rect.x() + gradient_rect.width() / 2.0;
        let cy = gradient_rect.y() + gradient_rect.height() / 2.0;

        let start = tiny_skia::Point::from_xy(cx - dx * len, cy - dy * len);
        let end = tiny_skia::Point::from_xy(cx + dx * len, cy + dy * len);
        let Some(shader) = LinearGradient::new(start, end, skia_stops, spread, Transform::identity()) else {
            return;
        };

        let Some(skia_rect) =
            SkiaRect::from_xywh(paint_rect.x(), paint_rect.y(), paint_rect.width(), paint_rect.height())
        else {
            return;
        };
        let path = PathBuilder::from_rect(skia_rect);

        let mut paint = Paint::default();
        paint.shader = shader;
        paint.anti_alias = true;

        if is_hsl_blend(blend_mode) {
            if let Some(mut layer) = Pixmap::new(self.pixmap.width(), self.pixmap.height()) {
                paint.blend_mode = SkiaBlendMode::SourceOver;
                layer.fill_path(
                    &path,
                    &paint,
                    tiny_skia::FillRule::Winding,
                    Transform::identity(),
                    clip_mask,
                );
                composite_hsl_layer(&mut self.pixmap, &layer, 1.0, blend_mode, Some(paint_rect));
            } else {
                paint.blend_mode = map_blend_mode(blend_mode);
                self.pixmap.fill_path(
                    &path,
                    &paint,
                    tiny_skia::FillRule::Winding,
                    Transform::identity(),
                    clip_mask,
                );
            }
        } else {
            paint.blend_mode = map_blend_mode(blend_mode);
            self.pixmap.fill_path(
                &path,
                &paint,
                tiny_skia::FillRule::Winding,
                Transform::identity(),
                clip_mask,
            );
        }
    }

    fn paint_radial_gradient(
        &mut self,
        gradient_rect: Rect,
        paint_rect: Rect,
        clip_mask: Option<&Mask>,
        stops: &[(f32, Rgba)],
        spread: SpreadMode,
        blend_mode: MixBlendMode,
    ) {
        if stops.is_empty() {
            return;
        }

        let gradient_rect = self.device_rect(gradient_rect);
        let paint_rect = self.device_rect(paint_rect);

        let skia_stops = gradient_stops(stops);
        let cx = gradient_rect.x() + gradient_rect.width() / 2.0;
        let cy = gradient_rect.y() + gradient_rect.height() / 2.0;
        let radius = ((gradient_rect.width() * gradient_rect.width() + gradient_rect.height() * gradient_rect.height())
            as f32)
            .sqrt()
            / 2.0;

        let center = tiny_skia::Point::from_xy(cx, cy);
        let Some(shader) = RadialGradient::new(center, center, radius, skia_stops, spread, Transform::identity())
        else {
            return;
        };

        let Some(skia_rect) =
            SkiaRect::from_xywh(paint_rect.x(), paint_rect.y(), paint_rect.width(), paint_rect.height())
        else {
            return;
        };
        let path = PathBuilder::from_rect(skia_rect);

        let mut paint = Paint::default();
        paint.shader = shader;
        paint.anti_alias = true;

        if is_hsl_blend(blend_mode) {
            if let Some(mut layer) = Pixmap::new(self.pixmap.width(), self.pixmap.height()) {
                paint.blend_mode = SkiaBlendMode::SourceOver;
                layer.fill_path(
                    &path,
                    &paint,
                    tiny_skia::FillRule::Winding,
                    Transform::identity(),
                    clip_mask,
                );
                composite_hsl_layer(&mut self.pixmap, &layer, 1.0, blend_mode, Some(paint_rect));
            } else {
                paint.blend_mode = map_blend_mode(blend_mode);
                self.pixmap.fill_path(
                    &path,
                    &paint,
                    tiny_skia::FillRule::Winding,
                    Transform::identity(),
                    clip_mask,
                );
            }
        } else {
            paint.blend_mode = map_blend_mode(blend_mode);
            self.pixmap.fill_path(
                &path,
                &paint,
                tiny_skia::FillRule::Winding,
                Transform::identity(),
                clip_mask,
            );
        }
    }

    fn paint_background_tile(
        &mut self,
        pixmap: &Pixmap,
        tile_x: f32,
        tile_y: f32,
        tile_w: f32,
        tile_h: f32,
        clip: Rect,
        mask: Option<&Mask>,
        blend_mode: MixBlendMode,
        quality: FilterQuality,
    ) {
        if tile_w <= 0.0 || tile_h <= 0.0 {
            return;
        }

        let tile_rect = self.device_rect(Rect::from_xywh(tile_x, tile_y, tile_w, tile_h));
        let clip_rect = self.device_rect(clip);
        let Some(intersection) = tile_rect.intersection(clip_rect) else {
            return;
        };
        if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
            return;
        }

        let scale_x = tile_rect.width() / pixmap.width() as f32;
        let scale_y = tile_rect.height() / pixmap.height() as f32;
        if !scale_x.is_finite() || !scale_y.is_finite() {
            return;
        }

        let mut paint = Paint::default();
        paint.shader = Pattern::new(
            pixmap.as_ref(),
            SpreadMode::Pad,
            quality,
            1.0,
            Transform::from_row(scale_x, 0.0, 0.0, scale_y, tile_rect.x(), tile_rect.y()),
        );
        paint.anti_alias = false;

        if let Some(rect) = SkiaRect::from_xywh(
            intersection.x(),
            intersection.y(),
            intersection.width(),
            intersection.height(),
        ) {
            if is_hsl_blend(blend_mode) {
                if let Some(mut layer) = Pixmap::new(self.pixmap.width(), self.pixmap.height()) {
                    paint.blend_mode = SkiaBlendMode::SourceOver;
                    layer.fill_rect(rect, &paint, Transform::identity(), mask);
                    composite_hsl_layer(&mut self.pixmap, &layer, 1.0, blend_mode, Some(intersection));
                } else {
                    paint.blend_mode = map_blend_mode(blend_mode);
                    self.pixmap.fill_rect(rect, &paint, Transform::identity(), mask);
                }
            } else {
                paint.blend_mode = map_blend_mode(blend_mode);
                self.pixmap.fill_rect(rect, &paint, Transform::identity(), mask);
            }
        }
    }

    /// Paints the borders of a fragment
    fn paint_borders(&mut self, x: f32, y: f32, width: f32, height: f32, style: &ComputedStyle) {
        // Only paint if there are borders
        let top = style.border_top_width.to_px() * self.scale;
        let right = style.border_right_width.to_px() * self.scale;
        let bottom = style.border_bottom_width.to_px() * self.scale;
        let left = style.border_left_width.to_px() * self.scale;

        let x = self.device_length(x);
        let y = self.device_length(y);
        let width = self.device_length(width);
        let height = self.device_length(height);

        if top <= 0.0 && right <= 0.0 && bottom <= 0.0 && left <= 0.0 {
            return;
        }

        // Top border
        if top > 0.0 {
            self.paint_border_edge(
                BorderEdge::Top,
                x,
                y + top * 0.5,
                x + width,
                y + top * 0.5,
                top,
                style.border_top_style,
                &style.border_top_color,
            );
        }

        // Right border
        if right > 0.0 {
            self.paint_border_edge(
                BorderEdge::Right,
                x + width - right * 0.5,
                y,
                x + width - right * 0.5,
                y + height,
                right,
                style.border_right_style,
                &style.border_right_color,
            );
        }

        // Bottom border
        if bottom > 0.0 {
            self.paint_border_edge(
                BorderEdge::Bottom,
                x,
                y + height - bottom * 0.5,
                x + width,
                y + height - bottom * 0.5,
                bottom,
                style.border_bottom_style,
                &style.border_bottom_color,
            );
        }

        // Left border
        if left > 0.0 {
            self.paint_border_edge(
                BorderEdge::Left,
                x + left * 0.5,
                y,
                x + left * 0.5,
                y + height,
                left,
                style.border_left_style,
                &style.border_left_color,
            );
        }
    }

    fn paint_border_edge(
        &mut self,
        edge: BorderEdge,
        x1: f32,
        y1: f32,
        x2: f32,
        y2: f32,
        width: f32,
        style: CssBorderStyle,
        color: &Rgba,
    ) {
        self.paint_border_edge_with_mode(edge, x1, y1, x2, y2, width, style, color, SkiaBlendMode::SourceOver);
    }

    fn paint_border_edge_with_mode(
        &mut self,
        edge: BorderEdge,
        x1: f32,
        y1: f32,
        x2: f32,
        y2: f32,
        width: f32,
        style: CssBorderStyle,
        color: &Rgba,
        blend_mode: SkiaBlendMode,
    ) {
        if width <= 0.0 || matches!(style, CssBorderStyle::None | CssBorderStyle::Hidden) {
            return;
        }

        let mut paint = Paint::default();
        paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
        paint.blend_mode = blend_mode;
        paint.anti_alias = true;

        let mut stroke = Stroke::default();
        stroke.width = width;
        stroke.line_cap = match style {
            CssBorderStyle::Dotted => tiny_skia::LineCap::Round,
            _ => tiny_skia::LineCap::Butt,
        };

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

                let (outer_path, inner_path) = edge.parallel_lines(x1, y1, x2, y2, offset);

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
            CssBorderStyle::Groove | CssBorderStyle::Ridge => {
                let half = width / 2.0;
                let offset = half * 0.5;
                let (first_path, second_path) = edge.parallel_lines(x1, y1, x2, y2, offset);

                let mut first_paint = paint.clone();
                let mut second_paint = paint.clone();
                let mut first_stroke = stroke.clone();
                let mut second_stroke = stroke.clone();
                first_stroke.width = half;
                second_stroke.width = half;

                let (first_color, second_color) = edge.groove_ridge_colors(color, style);
                first_paint.set_color_rgba8(first_color.r, first_color.g, first_color.b, first_color.alpha_u8());
                second_paint.set_color_rgba8(second_color.r, second_color.g, second_color.b, second_color.alpha_u8());

                if let Some(first) = first_path {
                    self.pixmap
                        .stroke_path(&first, &first_paint, &first_stroke, Transform::identity(), None);
                }
                if let Some(second) = second_path {
                    self.pixmap
                        .stroke_path(&second, &second_paint, &second_stroke, Transform::identity(), None);
                }
            }
            CssBorderStyle::Inset | CssBorderStyle::Outset => {
                let shaded = edge.inset_outset_color(color, style);
                paint.set_color_rgba8(shaded.r, shaded.g, shaded.b, shaded.alpha_u8());
                self.pixmap
                    .stroke_path(&base_path, &paint, &stroke, Transform::identity(), None);
            }
            _ => {
                self.pixmap
                    .stroke_path(&base_path, &paint, &stroke, Transform::identity(), None);
            }
        }
    }

    fn paint_outline(&mut self, x: f32, y: f32, width: f32, height: f32, style: &ComputedStyle) {
        let ow = style.outline_width.to_px() * self.scale;
        let outline_style = style.outline_style.to_border_style();
        if ow <= 0.0 || matches!(outline_style, CssBorderStyle::None | CssBorderStyle::Hidden) {
            return;
        }
        let offset = style.outline_offset.to_px() * self.scale;
        let expand = offset + ow * 0.5;
        let outer_x = self.device_length(x) - expand;
        let outer_y = self.device_length(y) - expand;
        let outer_w = self.device_length(width) + 2.0 * expand;
        let outer_h = self.device_length(height) + 2.0 * expand;
        let (color, invert) = style.outline_color.resolve(style.color);
        let blend_mode = if invert {
            SkiaBlendMode::Difference
        } else {
            SkiaBlendMode::SourceOver
        };

        self.paint_border_edge_with_mode(
            BorderEdge::Top,
            outer_x,
            outer_y + ow * 0.5,
            outer_x + outer_w,
            outer_y + ow * 0.5,
            ow,
            outline_style,
            &color,
            blend_mode,
        );
        self.paint_border_edge_with_mode(
            BorderEdge::Bottom,
            outer_x,
            outer_y + outer_h - ow * 0.5,
            outer_x + outer_w,
            outer_y + outer_h - ow * 0.5,
            ow,
            outline_style,
            &color,
            blend_mode,
        );
        self.paint_border_edge_with_mode(
            BorderEdge::Left,
            outer_x + ow * 0.5,
            outer_y,
            outer_x + ow * 0.5,
            outer_y + outer_h,
            ow,
            outline_style,
            &color,
            blend_mode,
        );
        self.paint_border_edge_with_mode(
            BorderEdge::Right,
            outer_x + outer_w - ow * 0.5,
            outer_y,
            outer_x + outer_w - ow * 0.5,
            outer_y + outer_h,
            ow,
            outline_style,
            &color,
            blend_mode,
        );
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

        self.paint_shaped_runs(&shaped_runs, x, baseline_y, color, style);
    }

    fn paint_shaped_runs(
        &mut self,
        runs: &[ShapedRun],
        origin_x: f32,
        baseline_y: f32,
        color: Rgba,
        style: Option<&ComputedStyle>,
    ) {
        let mut pen_x = origin_x;

        for run in runs {
            let run_origin = if run.direction.is_rtl() {
                pen_x + run.advance
            } else {
                pen_x
            };
            self.paint_shaped_run(run, run_origin, baseline_y, color, style);
            pen_x += run.advance;
        }
    }

    fn paint_shaped_run(
        &mut self,
        run: &ShapedRun,
        origin_x: f32,
        baseline_y: f32,
        color: Rgba,
        style: Option<&ComputedStyle>,
    ) {
        let origin_x = origin_x * self.scale;
        let baseline_y = baseline_y * self.scale;
        // ttf_parser face
        let face = match ttf_parser::Face::parse(&run.font.data, run.font.index) {
            Ok(f) => f,
            Err(_) => return,
        };
        let units_per_em = face.units_per_em() as f32;
        let mut scale = run.font_size / units_per_em;
        scale *= run.scale * self.scale;

        let mut glyph_paths = Vec::with_capacity(run.glyphs.len());
        let mut bounds = PathBounds::new();

        for glyph in &run.glyphs {
            let glyph_x = match run.direction {
                crate::text::pipeline::Direction::RightToLeft => origin_x - glyph.x_offset * self.scale,
                _ => origin_x + glyph.x_offset * self.scale,
            };
            let glyph_y = baseline_y - glyph.y_offset * self.scale;

            let glyph_id: u16 = glyph.glyph_id as u16;

            if let Some(path) = Self::build_glyph_path(&face, glyph_id, glyph_x, glyph_y, scale) {
                bounds.include(&path.bounds());
                glyph_paths.push(path);
            }
        }

        if glyph_paths.is_empty() || !bounds.is_valid() {
            return;
        }

        // Rotate sideways runs for vertical text-orientation.
        if !matches!(run.rotation, crate::text::pipeline::RunRotation::None) {
            let angle = match run.rotation {
                crate::text::pipeline::RunRotation::Ccw90 => -90.0_f32.to_radians(),
                crate::text::pipeline::RunRotation::Cw90 => 90.0_f32.to_radians(),
                crate::text::pipeline::RunRotation::None => 0.0,
            };
            let (sin, cos) = angle.sin_cos();
            let tx = origin_x - origin_x * cos + baseline_y * sin;
            let ty = baseline_y - origin_x * sin - baseline_y * cos;
            let rotate = tiny_skia::Transform::from_row(cos, sin, -sin, cos, tx, ty);

            let mut rotated_paths = Vec::with_capacity(glyph_paths.len());
            let mut rotated_bounds = PathBounds::new();
            for path in glyph_paths.drain(..) {
                if let Some(rotated) = path.clone().transform(rotate) {
                    let rect = path.bounds();
                    let corners = [
                        (rect.left(), rect.top()),
                        (rect.right(), rect.top()),
                        (rect.right(), rect.bottom()),
                        (rect.left(), rect.bottom()),
                    ];
                    let mut min_x = f32::INFINITY;
                    let mut min_y = f32::INFINITY;
                    let mut max_x = f32::NEG_INFINITY;
                    let mut max_y = f32::NEG_INFINITY;
                    for (x, y) in corners {
                        let mapped_x = x * cos + y * -sin + tx;
                        let mapped_y = x * sin + y * cos + ty;
                        min_x = min_x.min(mapped_x);
                        min_y = min_y.min(mapped_y);
                        max_x = max_x.max(mapped_x);
                        max_y = max_y.max(mapped_y);
                    }
                    if let Some(mapped) = tiny_skia::Rect::from_ltrb(min_x, min_y, max_x, max_y) {
                        rotated_bounds.include(&mapped);
                    }
                    rotated_paths.push(rotated);
                }
            }
            if rotated_bounds.is_valid() && !rotated_paths.is_empty() {
                glyph_paths = rotated_paths;
                bounds = rotated_bounds;
            } else {
                glyph_paths = rotated_paths;
            }
        }

        if let Some(style) = style {
            if !style.text_shadow.is_empty() {
                let shadows = resolve_text_shadows(style);
                if !shadows.is_empty() {
                    self.paint_text_shadows(&glyph_paths, &bounds, &shadows);
                }
            }
        }

        let mut paint = Paint::default();
        paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
        paint.anti_alias = true;

        for path in &glyph_paths {
            self.pixmap
                .fill_path(path, &paint, tiny_skia::FillRule::EvenOdd, Transform::identity(), None);
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

    fn paint_text_shadows(&mut self, paths: &[tiny_skia::Path], bounds: &PathBounds, shadows: &[ResolvedTextShadow]) {
        for shadow in shadows {
            let offset_x = shadow.offset_x * self.scale;
            let offset_y = shadow.offset_y * self.scale;
            let blur = shadow.blur_radius * self.scale;

            let blur_margin = (blur.abs() * 3.0).ceil();
            let shadow_min_x = bounds.min_x + offset_x - blur_margin;
            let shadow_max_x = bounds.max_x + offset_x + blur_margin;
            let shadow_min_y = bounds.min_y + offset_y - blur_margin;
            let shadow_max_y = bounds.max_y + offset_y + blur_margin;

            let shadow_width = (shadow_max_x - shadow_min_x).ceil().max(0.0) as u32;
            let shadow_height = (shadow_max_y - shadow_min_y).ceil().max(0.0) as u32;
            if shadow_width == 0 || shadow_height == 0 {
                continue;
            }

            let Some(mut shadow_pixmap) = Pixmap::new(shadow_width, shadow_height) else {
                continue;
            };

            let mut paint = Paint::default();
            paint.set_color_rgba8(shadow.color.r, shadow.color.g, shadow.color.b, shadow.color.alpha_u8());
            paint.anti_alias = true;

            let translate_x = -bounds.min_x + blur_margin;
            let translate_y = -bounds.min_y + blur_margin;
            let transform = Transform::from_translate(translate_x, translate_y);
            for path in paths {
                shadow_pixmap.fill_path(path, &paint, tiny_skia::FillRule::EvenOdd, transform, None);
            }

            if blur > 0.0 {
                apply_gaussian_blur(&mut shadow_pixmap, blur);
            }

            let dest_x = shadow_min_x.floor() as i32;
            let dest_y = shadow_min_y.floor() as i32;
            let frac_x = shadow_min_x - dest_x as f32;
            let frac_y = shadow_min_y - dest_y as f32;
            let pixmap_paint = PixmapPaint {
                opacity: 1.0,
                blend_mode: SkiaBlendMode::SourceOver,
                ..Default::default()
            };
            self.pixmap.draw_pixmap(
                dest_x,
                dest_y,
                shadow_pixmap.as_ref(),
                &pixmap_paint,
                Transform::from_translate(frac_x, frac_y),
                None,
            );
        }
    }

    /// Paints a replaced element (image, etc.)
    fn paint_replaced(
        &mut self,
        replaced_type: &ReplacedType,
        style: Option<&ComputedStyle>,
        x: f32,
        y: f32,
        width: f32,
        height: f32,
    ) {
        if width <= 0.0 || height <= 0.0 {
            return;
        }

        let content_rect = if let Some(style) = style {
            background_rects(x, y, width, height, style).content
        } else {
            Rect::from_xywh(x, y, width, height)
        };
        if content_rect.width() <= 0.0 || content_rect.height() <= 0.0 {
            return;
        }

        // Try to render actual content for images and SVG
        match replaced_type {
            ReplacedType::Image { alt, .. } => {
                let chosen_src = replaced_type.image_source_for_scale(self.scale, Some(content_rect.width()));
                if self.paint_image_from_src(
                    chosen_src,
                    style,
                    content_rect.x(),
                    content_rect.y(),
                    content_rect.width(),
                    content_rect.height(),
                ) {
                    return;
                }
                if let (Some(style), Some(alt_text)) = (style, alt.as_deref()) {
                    if self.paint_alt_text(alt_text, style, content_rect) {
                        return;
                    }
                }
            }
            ReplacedType::Svg { content } => {
                if self.paint_svg(
                    content,
                    style,
                    content_rect.x(),
                    content_rect.y(),
                    content_rect.width(),
                    content_rect.height(),
                ) {
                    return;
                }
            }
            ReplacedType::Embed { src } => {
                if self.paint_image_from_src(
                    src,
                    style,
                    content_rect.x(),
                    content_rect.y(),
                    content_rect.width(),
                    content_rect.height(),
                ) {
                    return;
                }
            }
            ReplacedType::Object { data } => {
                if self.paint_image_from_src(
                    data,
                    style,
                    content_rect.x(),
                    content_rect.y(),
                    content_rect.width(),
                    content_rect.height(),
                ) {
                    return;
                }
            }
            ReplacedType::Iframe { src } => {
                if self.paint_image_from_src(
                    src,
                    style,
                    content_rect.x(),
                    content_rect.y(),
                    content_rect.width(),
                    content_rect.height(),
                ) {
                    return;
                }
            }
            ReplacedType::Video { src, poster } => {
                if let Some(poster_src) = poster.as_deref() {
                    if self.paint_image_from_src(
                        poster_src,
                        style,
                        content_rect.x(),
                        content_rect.y(),
                        content_rect.width(),
                        content_rect.height(),
                    ) {
                        return;
                    }
                }
                if self.paint_image_from_src(
                    src,
                    style,
                    content_rect.x(),
                    content_rect.y(),
                    content_rect.width(),
                    content_rect.height(),
                ) {
                    return;
                }
            }
            _ => {}
        }

        self.paint_replaced_placeholder(replaced_type, style, content_rect);
    }

    fn paint_image_from_src(
        &mut self,
        src: &str,
        style: Option<&ComputedStyle>,
        x: f32,
        y: f32,
        width: f32,
        height: f32,
    ) -> bool {
        if src.is_empty() {
            return false;
        }

        let image = match self.image_cache.load(src) {
            Ok(img) => img,
            Err(_) => {
                if src.trim_start().starts_with('<') {
                    match self.image_cache.render_svg(src) {
                        Ok(img) => img,
                        Err(_) => return false,
                    }
                } else {
                    return false;
                }
            }
        };

        let pixmap = match Self::dynamic_image_to_pixmap(&image) {
            Some(pixmap) => pixmap,
            None => return false,
        };

        if pixmap.width() == 0 || pixmap.height() == 0 {
            return false;
        }

        let img_w = pixmap.width() as f32;
        let img_h = pixmap.height() as f32;
        let fit = style.map(|s| s.object_fit).unwrap_or(ObjectFit::Fill);
        let pos = style.map(|s| s.object_position).unwrap_or(default_object_position());

        let (dest_x, dest_y, dest_w, dest_h) = match compute_object_fit(
            fit,
            pos,
            width,
            height,
            img_w,
            img_h,
            style.map(|s| s.font_size).unwrap_or(16.0),
            Some((self.css_width, self.css_height)),
        ) {
            Some(v) => v,
            None => return false,
        };

        let dest_x = self.device_length(dest_x);
        let dest_y = self.device_length(dest_y);
        let dest_w = self.device_length(dest_w);
        let dest_h = self.device_length(dest_h);

        let scale_x = dest_w / img_w;
        let scale_y = dest_h / img_h;
        if !scale_x.is_finite() || !scale_y.is_finite() {
            return false;
        }

        let mut paint = PixmapPaint::default();
        paint.quality = Self::filter_quality_for_image(style);

        let transform = Transform::from_row(
            scale_x,
            0.0,
            0.0,
            scale_y,
            self.device_length(x) + dest_x,
            self.device_length(y) + dest_y,
        );
        self.pixmap.draw_pixmap(0, 0, pixmap.as_ref(), &paint, transform, None);
        true
    }

    fn paint_svg(
        &mut self,
        content: &str,
        style: Option<&ComputedStyle>,
        x: f32,
        y: f32,
        width: f32,
        height: f32,
    ) -> bool {
        if content.is_empty() {
            return false;
        }

        let image = if content.trim_start().starts_with('<') {
            match self.image_cache.render_svg(content) {
                Ok(img) => img,
                Err(_) => return false,
            }
        } else {
            match self.image_cache.load(content) {
                Ok(img) => img,
                Err(_) => return false,
            }
        };

        let pixmap = match Self::dynamic_image_to_pixmap(&image) {
            Some(pixmap) => pixmap,
            None => return false,
        };

        if pixmap.width() == 0 || pixmap.height() == 0 {
            return false;
        }

        let img_w = pixmap.width() as f32;
        let img_h = pixmap.height() as f32;
        let fit = style.map(|s| s.object_fit).unwrap_or(ObjectFit::Fill);
        let pos = style.map(|s| s.object_position).unwrap_or(default_object_position());

        let (dest_x, dest_y, dest_w, dest_h) = match compute_object_fit(
            fit,
            pos,
            width,
            height,
            img_w,
            img_h,
            style.map(|s| s.font_size).unwrap_or(16.0),
            Some((self.css_width, self.css_height)),
        ) {
            Some(v) => v,
            None => return false,
        };

        let dest_x = self.device_length(dest_x);
        let dest_y = self.device_length(dest_y);
        let dest_w = self.device_length(dest_w);
        let dest_h = self.device_length(dest_h);

        let scale_x = dest_w / img_w;
        let scale_y = dest_h / img_h;
        if !scale_x.is_finite() || !scale_y.is_finite() {
            return false;
        }

        let mut paint = PixmapPaint::default();
        paint.quality = Self::filter_quality_for_image(style);

        let transform = Transform::from_row(
            scale_x,
            0.0,
            0.0,
            scale_y,
            self.device_length(x) + dest_x,
            self.device_length(y) + dest_y,
        );
        self.pixmap.draw_pixmap(0, 0, pixmap.as_ref(), &paint, transform, None);
        true
    }

    fn paint_replaced_placeholder(&mut self, replaced_type: &ReplacedType, style: Option<&ComputedStyle>, rect: Rect) {
        let mut paint = Paint::default();
        paint.set_color_rgba8(200, 200, 200, 255); // Light gray
        paint.anti_alias = true;

        let device_rect = self.device_rect(rect);

        if let Some(sk_rect) = SkiaRect::from_xywh(
            device_rect.x(),
            device_rect.y(),
            device_rect.width(),
            device_rect.height(),
        ) {
            let path = PathBuilder::from_rect(sk_rect);
            self.pixmap
                .fill_path(&path, &paint, tiny_skia::FillRule::Winding, Transform::identity(), None);

            // Border around the placeholder
            let mut stroke_paint = Paint::default();
            stroke_paint.set_color_rgba8(150, 150, 150, 255);
            stroke_paint.anti_alias = true;

            let stroke = tiny_skia::Stroke {
                width: 1.0 * self.scale,
                ..Default::default()
            };
            self.pixmap
                .stroke_path(&path, &stroke_paint, &stroke, Transform::identity(), None);
        }

        // Optional label to hint the missing resource type
        let label = match replaced_type {
            ReplacedType::Video { .. } => Some("video"),
            ReplacedType::Iframe { .. } => Some("iframe"),
            ReplacedType::Canvas => Some("canvas"),
            ReplacedType::Embed { .. } => Some("embed"),
            ReplacedType::Object { .. } => Some("object"),
            _ => None,
        };

        if let (Some(style), Some(label_text)) = (style, label) {
            let mut label_style = style.clone();
            label_style.color = Rgba::rgb(120, 120, 120);
            // Use a small inset to avoid clipping against the placeholder edges
            let inset = 2.0;
            let label_rect = Rect::from_xywh(
                rect.x() + inset,
                rect.y() + inset,
                (rect.width() - 2.0 * inset).max(0.0),
                (rect.height() - 2.0 * inset).max(0.0),
            );
            let _ = self.paint_alt_text(label_text, &label_style, label_rect);
        }
    }

    fn paint_alt_text(&mut self, alt: &str, style: &ComputedStyle, rect: Rect) -> bool {
        let text = alt.trim();
        if text.is_empty() {
            return false;
        }

        let mut runs = match self.shaper.shape(text, style, &self.font_ctx) {
            Ok(runs) => runs,
            Err(_) => return false,
        };
        if runs.is_empty() {
            return false;
        }

        TextItem::apply_spacing_to_runs(&mut runs, text, style.letter_spacing, style.word_spacing);

        let metrics_scaled = self.resolve_scaled_metrics(style);
        let line_height = compute_line_height_with_metrics(style, metrics_scaled.as_ref());
        let metrics = TextItem::metrics_from_runs(&runs, line_height, style.font_size);
        let half_leading = ((metrics.line_height - (metrics.ascent + metrics.descent)) / 2.0).max(0.0);
        let baseline_y = rect.y() + half_leading + metrics.baseline_offset;

        self.paint_shaped_runs(&runs, rect.x(), baseline_y, style.color, Some(style));
        true
    }

    fn decoration_metrics<'a>(
        &self,
        runs: Option<&'a [ShapedRun]>,
        style: &ComputedStyle,
    ) -> Option<DecorationMetrics> {
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

    #[allow(dead_code)]
    fn resolve_underline_offset(&self, style: &ComputedStyle) -> f32 {
        self.resolve_underline_offset_value(style.text_underline_offset, style)
    }

    fn resolve_underline_offset_value(
        &self,
        offset: crate::style::types::TextUnderlineOffset,
        style: &ComputedStyle,
    ) -> f32 {
        let resolved = match offset {
            crate::style::types::TextUnderlineOffset::Auto => 0.0,
            crate::style::types::TextUnderlineOffset::Length(l) => {
                if l.unit == LengthUnit::Percent {
                    l.resolve_against(style.font_size)
                } else if l.unit.is_font_relative() {
                    l.resolve_with_font_size(style.font_size)
                } else if l.unit.is_viewport_relative() {
                    l.resolve_with_viewport(self.css_width, self.css_height)
                } else if l.unit.is_absolute() {
                    l.to_px()
                } else {
                    l.value * style.font_size
                }
            }
        };
        resolved * self.scale
    }

    fn underline_position(
        &self,
        metrics: &DecorationMetrics,
        position: crate::style::types::TextUnderlinePosition,
        offset: f32,
        thickness: f32,
    ) -> f32 {
        let under_base = -metrics.descent - thickness * 0.5;
        let base =
            match position {
                crate::style::types::TextUnderlinePosition::Auto
                | crate::style::types::TextUnderlinePosition::FromFont => metrics.underline_pos,
                crate::style::types::TextUnderlinePosition::Under
                | crate::style::types::TextUnderlinePosition::UnderLeft
                | crate::style::types::TextUnderlinePosition::UnderRight => metrics.underline_pos.min(under_base),
                crate::style::types::TextUnderlinePosition::Left
                | crate::style::types::TextUnderlinePosition::Right => metrics.underline_pos,
            };

        metrics.underline_position_with_offset(base, offset)
    }

    fn dynamic_image_to_pixmap(image: &DynamicImage) -> Option<Pixmap> {
        let rgba = image.to_rgba8();
        let (width, height) = rgba.dimensions();
        if width == 0 || height == 0 {
            return None;
        }

        // tiny-skia expects premultiplied RGBA
        let mut data = Vec::with_capacity((width * height * 4) as usize);
        for pixel in rgba.pixels() {
            let [r, g, b, a] = pixel.0;
            let alpha = a as f32 / 255.0;
            data.push((r as f32 * alpha).round() as u8);
            data.push((g as f32 * alpha).round() as u8);
            data.push((b as f32 * alpha).round() as u8);
            data.push(a);
        }

        let size = IntSize::from_wh(width, height)?;
        Pixmap::from_vec(data, size)
    }

    fn paint_text_decoration(
        &mut self,
        style: &ComputedStyle,
        runs: Option<&[ShapedRun]>,
        x: f32,
        baseline_y: f32,
        width: f32,
    ) {
        let has_any_decoration = !style.applied_text_decorations.is_empty() || !style.text_decoration.lines.is_empty();
        if !has_any_decoration || width <= 0.0 {
            return;
        }

        let x = self.device_length(x);
        let baseline_y = self.device_length(baseline_y);
        let width = self.device_length(width);

        let Some(metrics) = self.decoration_metrics(runs, style) else {
            return;
        };
        let metrics = metrics.scaled(self.scale);

        let draw_solid_line =
            |pixmap: &mut Pixmap, paint: &Paint, start: f32, len: f32, center: f32, thickness: f32| {
                if thickness <= 0.0 {
                    return;
                }
                if let Some(rect) = SkiaRect::from_xywh(start, center - thickness * 0.5, len, thickness) {
                    let path = PathBuilder::from_rect(rect);
                    pixmap.fill_path(&path, &paint, tiny_skia::FillRule::Winding, Transform::identity(), None);
                }
            };

        let draw_stroked_line = |pixmap: &mut Pixmap,
                                 paint: &Paint,
                                 start: f32,
                                 len: f32,
                                 center: f32,
                                 thickness: f32,
                                 dash: Option<Vec<f32>>,
                                 round: bool| {
            let mut path = PathBuilder::new();
            path.move_to(start, center);
            path.line_to(start + len, center);
            let Some(path) = path.finish() else { return };

            let mut stroke = Stroke::default();
            stroke.width = thickness;
            stroke.line_cap = if round {
                tiny_skia::LineCap::Round
            } else {
                tiny_skia::LineCap::Butt
            };
            if let Some(arr) = dash {
                stroke.dash = tiny_skia::StrokeDash::new(arr, 0.0);
            }

            pixmap.stroke_path(&path, &paint, &stroke, Transform::identity(), None);
        };

        let draw_wavy_line = |pixmap: &mut Pixmap, paint: &Paint, start: f32, len: f32, center: f32, thickness: f32| {
            if thickness <= 0.0 || len <= 0.0 {
                return;
            }
            let wavelength = (thickness * 4.0).max(6.0);
            let amplitude = (thickness * 0.75).max(thickness * 0.5);

            let mut path = PathBuilder::new();
            path.move_to(start, center);
            let mut cursor = start;
            let mut up = true;
            while cursor < start + len {
                let end = (cursor + wavelength).min(start + len);
                let mid = cursor + (end - cursor) * 0.5;
                let control_y = if up { center - amplitude } else { center + amplitude };
                path.quad_to(mid, control_y, end, center);
                cursor = end;
                up = !up;
            }

            if let Some(path) = path.finish() {
                let mut stroke = Stroke::default();
                stroke.width = thickness.max(0.5);
                stroke.line_cap = tiny_skia::LineCap::Round;
                pixmap.stroke_path(&path, &paint, &stroke, Transform::identity(), None);
            }
        };

        let decorations = if !style.applied_text_decorations.is_empty() {
            Cow::Borrowed(style.applied_text_decorations.as_slice())
        } else {
            Cow::Owned(vec![crate::style::types::ResolvedTextDecoration {
                decoration: style.text_decoration.clone(),
                skip_ink: style.text_decoration_skip_ink,
                underline_offset: style.text_underline_offset,
                underline_position: style.text_underline_position,
            }])
        };

        for deco in decorations.iter() {
            let mut paint = Paint::default();
            paint.anti_alias = true;
            let decoration_color = deco.decoration.color.unwrap_or(style.color);
            paint.set_color(color_to_skia(decoration_color));

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
                        l.resolve_with_viewport(self.css_width, self.css_height)
                    } else {
                        l.to_px()
                    };
                    Some(resolved * self.scale)
                }
            };

            let painter_style = deco.decoration.style;
            let render_line = |pixmap: &mut Pixmap, center: f32, thickness: f32| match painter_style {
                TextDecorationStyle::Solid => draw_solid_line(pixmap, &paint, x, width, center, thickness),
                TextDecorationStyle::Double => {
                    let line_thickness = (thickness * 0.7).max(0.5);
                    let gap = line_thickness.max(thickness * 0.6);
                    draw_solid_line(pixmap, &paint, x, width, center - (gap * 0.5), line_thickness);
                    draw_solid_line(pixmap, &paint, x, width, center + (gap * 0.5), line_thickness);
                }
                TextDecorationStyle::Dotted => {
                    draw_stroked_line(
                        pixmap,
                        &paint,
                        x,
                        width,
                        center,
                        thickness,
                        Some(vec![thickness, thickness]),
                        true,
                    );
                }
                TextDecorationStyle::Dashed => {
                    draw_stroked_line(
                        pixmap,
                        &paint,
                        x,
                        width,
                        center,
                        thickness,
                        Some(vec![3.0 * thickness, thickness]),
                        false,
                    );
                }
                TextDecorationStyle::Wavy => draw_wavy_line(pixmap, &paint, x, width, center, thickness),
            };

            let render_line_segment =
                |pixmap: &mut Pixmap, start: f32, len: f32, center: f32, thickness: f32| match painter_style {
                    TextDecorationStyle::Solid => draw_solid_line(pixmap, &paint, start, len, center, thickness),
                    TextDecorationStyle::Double => {
                        let line_thickness = (thickness * 0.7).max(0.5);
                        let gap = line_thickness.max(thickness * 0.6);
                        draw_solid_line(pixmap, &paint, start, len, center - (gap * 0.5), line_thickness);
                        draw_solid_line(pixmap, &paint, start, len, center + (gap * 0.5), line_thickness);
                    }
                    TextDecorationStyle::Dotted => {
                        draw_stroked_line(
                            pixmap,
                            &paint,
                            start,
                            len,
                            center,
                            thickness,
                            Some(vec![thickness, thickness]),
                            true,
                        );
                    }
                    TextDecorationStyle::Dashed => {
                        draw_stroked_line(
                            pixmap,
                            &paint,
                            start,
                            len,
                            center,
                            thickness,
                            Some(vec![3.0 * thickness, thickness]),
                            false,
                        );
                    }
                    TextDecorationStyle::Wavy => draw_wavy_line(pixmap, &paint, start, len, center, thickness),
                };

            let underline_offset = self.resolve_underline_offset_value(deco.underline_offset, style);
            if deco.decoration.lines.contains(TextDecorationLine::UNDERLINE) {
                let thickness = used_thickness.unwrap_or(metrics.underline_thickness);
                let adjusted_pos =
                    self.underline_position(&metrics, deco.underline_position, underline_offset, thickness);
                let center = baseline_y - adjusted_pos;
                if matches!(
                    deco.skip_ink,
                    crate::style::types::TextDecorationSkipInk::Auto | crate::style::types::TextDecorationSkipInk::All
                ) {
                    if let Some(runs) = runs {
                        let segments =
                            self.build_underline_segments(runs, x, width, center, thickness, baseline_y, deco.skip_ink);
                        for (seg_start, seg_end) in segments {
                            let seg_width = seg_end - seg_start;
                            if seg_width <= 0.0 {
                                continue;
                            }
                            render_line_segment(&mut self.pixmap, seg_start, seg_width, center, thickness);
                        }
                    } else {
                        render_line(&mut self.pixmap, center, thickness);
                    }
                } else {
                    render_line(&mut self.pixmap, center, thickness);
                }
            }
            if deco.decoration.lines.contains(TextDecorationLine::OVERLINE) {
                render_line(
                    &mut self.pixmap,
                    baseline_y - metrics.ascent,
                    used_thickness.unwrap_or(metrics.underline_thickness),
                );
            }
            if deco.decoration.lines.contains(TextDecorationLine::LINE_THROUGH) {
                render_line(
                    &mut self.pixmap,
                    baseline_y - metrics.strike_pos,
                    used_thickness.unwrap_or(metrics.strike_thickness),
                );
            }
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
            self.scale,
        );

        let mut segments = subtract_intervals((line_start, line_start + line_width), &mut exclusions);
        if segments.is_empty() && skip_ink != TextDecorationSkipInk::All {
            // Never drop the underline entirely when skipping ink; fall back to a full span.
            segments.push((line_start, line_start + line_width));
        }
        segments
    }

    fn paint_text_emphasis(
        &mut self,
        style: &ComputedStyle,
        runs: Option<&[ShapedRun]>,
        origin_x: f32,
        baseline_y: f32,
    ) {
        let runs = match runs {
            Some(r) => r,
            None => return,
        };
        if style.text_emphasis_style.is_none() {
            return;
        }

        let origin_x = self.device_length(origin_x);
        let baseline_y = self.device_length(baseline_y);

        let Some(metrics) = self.decoration_metrics(Some(runs), style) else {
            return;
        };
        let metrics = metrics.scaled(self.scale);

        let resolved_position = match style.text_emphasis_position {
            crate::style::types::TextEmphasisPosition::Auto => crate::style::types::TextEmphasisPosition::Over,
            other => other,
        };
        let emphasis_color = style.text_emphasis_color.unwrap_or(style.color);
        let mark_size = (style.font_size * 0.5 * self.scale).max(1.0);
        let gap = mark_size * 0.3;

        let center_y = match resolved_position {
            crate::style::types::TextEmphasisPosition::Over
            | crate::style::types::TextEmphasisPosition::OverLeft
            | crate::style::types::TextEmphasisPosition::OverRight => {
                baseline_y - metrics.ascent - gap - mark_size * 0.5
            }
            crate::style::types::TextEmphasisPosition::Under
            | crate::style::types::TextEmphasisPosition::UnderLeft
            | crate::style::types::TextEmphasisPosition::UnderRight => {
                baseline_y + metrics.descent + gap + mark_size * 0.5
            }
            crate::style::types::TextEmphasisPosition::Auto => baseline_y - metrics.ascent - gap - mark_size * 0.5,
        };

        // Precompute mark painter for string emphasis.
        let mut string_mark: Option<(Vec<tiny_skia::Path>, f32, f32)> = None;
        if let crate::style::types::TextEmphasisStyle::String(ref s) = style.text_emphasis_style {
            if !s.is_empty() {
                let mut mark_style = style.clone();
                mark_style.font_size = style.font_size * 0.5;
                let Ok(mark_runs) = self.shaper.shape(s, &mark_style, &self.font_ctx) else {
                    return;
                };
                let mark_width: f32 = mark_runs.iter().map(|r| r.advance * self.scale).sum();
                let mark_metrics = crate::layout::contexts::inline::line_builder::TextItem::metrics_from_runs(
                    &mark_runs,
                    mark_style.font_size,
                    mark_style.font_size,
                );
                let mut paths = Vec::new();
                let mut pen_x = 0.0;
                for run in &mark_runs {
                    let advance = run.advance * self.scale;
                    let run_origin = if run.direction.is_rtl() { pen_x + advance } else { pen_x };
                    let face = match ttf_parser::Face::parse(&run.font.data, run.font.index) {
                        Ok(f) => f,
                        Err(_) => continue,
                    };
                    let units_per_em = face.units_per_em() as f32;
                    let scale = (run.font_size / units_per_em) * self.scale;
                    for glyph in &run.glyphs {
                        let glyph_x = match run.direction {
                            crate::text::pipeline::Direction::RightToLeft => run_origin - glyph.x_offset * self.scale,
                            _ => run_origin + glyph.x_offset * self.scale,
                        };
                        let glyph_y = mark_metrics.baseline_offset * self.scale;
                        if let Some(path) =
                            Self::build_glyph_path(&face, glyph.glyph_id as u16, glyph_x, glyph_y, scale)
                        {
                            paths.push(path);
                        }
                    }
                    pen_x += advance;
                }
                if !paths.is_empty() {
                    string_mark = Some((
                        paths,
                        mark_width,
                        (mark_metrics.ascent + mark_metrics.descent) * self.scale,
                    ));
                }
            }
        }

        let mut pen_x = origin_x;
        for run in runs {
            let advance = run.advance * self.scale;
            let run_origin = if run.direction.is_rtl() { pen_x + advance } else { pen_x };

            let mut seen_clusters = std::collections::HashSet::new();
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
                let mark_center_x = match run.direction {
                    crate::text::pipeline::Direction::RightToLeft => {
                        run_origin - (glyph.x_offset + glyph.x_advance * 0.5) * self.scale
                    }
                    _ => run_origin + (glyph.x_offset + glyph.x_advance * 0.5) * self.scale,
                };

                match style.text_emphasis_style {
                    crate::style::types::TextEmphasisStyle::Mark { fill, shape } => {
                        self.draw_emphasis_mark(
                            mark_center_x,
                            center_y,
                            mark_size,
                            fill,
                            shape,
                            emphasis_color,
                            resolved_position,
                        );
                    }
                    crate::style::types::TextEmphasisStyle::String(_) => {
                        if let Some((ref paths, width, height)) = string_mark {
                            let mut paint = Paint::default();
                            paint.anti_alias = true;
                            paint.set_color(color_to_skia(emphasis_color));

                            let offset_x = mark_center_x - width * 0.5;
                            let offset_y = center_y - height * 0.5;
                            for path in paths {
                                let translated = path.clone().transform(Transform::from_translate(offset_x, offset_y));
                                if let Some(p) = translated {
                                    self.pixmap.fill_path(
                                        &p,
                                        &paint,
                                        tiny_skia::FillRule::EvenOdd,
                                        Transform::identity(),
                                        None,
                                    );
                                }
                            }
                        }
                    }
                    crate::style::types::TextEmphasisStyle::None => {}
                }
            }

            pen_x += advance;
        }
    }

    fn draw_emphasis_mark(
        &mut self,
        center_x: f32,
        center_y: f32,
        size: f32,
        fill: crate::style::types::TextEmphasisFill,
        shape: crate::style::types::TextEmphasisShape,
        color: Rgba,
        position: crate::style::types::TextEmphasisPosition,
    ) {
        let mut paint = Paint::default();
        paint.anti_alias = true;
        paint.set_color(color_to_skia(color));

        match shape {
            crate::style::types::TextEmphasisShape::Dot => {
                let radius = size * 0.5;
                if let Some(path) = PathBuilder::from_circle(center_x, center_y, radius) {
                    match fill {
                        crate::style::types::TextEmphasisFill::Filled => self.pixmap.fill_path(
                            &path,
                            &paint,
                            tiny_skia::FillRule::EvenOdd,
                            Transform::identity(),
                            None,
                        ),
                        crate::style::types::TextEmphasisFill::Open => {
                            let mut stroke = Stroke::default();
                            stroke.width = (size * 0.18).max(0.5);
                            self.pixmap
                                .stroke_path(&path, &paint, &stroke, Transform::identity(), None);
                        }
                    }
                }
            }
            crate::style::types::TextEmphasisShape::Circle => {
                let radius = size * 0.5;
                if let Some(path) = PathBuilder::from_circle(center_x, center_y, radius) {
                    match fill {
                        crate::style::types::TextEmphasisFill::Filled => self.pixmap.fill_path(
                            &path,
                            &paint,
                            tiny_skia::FillRule::EvenOdd,
                            Transform::identity(),
                            None,
                        ),
                        crate::style::types::TextEmphasisFill::Open => {
                            let mut stroke = Stroke::default();
                            stroke.width = (size * 0.18).max(0.5);
                            self.pixmap
                                .stroke_path(&path, &paint, &stroke, Transform::identity(), None);
                        }
                    }
                }
            }
            crate::style::types::TextEmphasisShape::DoubleCircle => {
                let mut stroke = Stroke::default();
                stroke.width = (size * 0.14).max(0.5);
                let radii = [size * 0.5, size * 0.33];
                for radius in radii {
                    if let Some(path) = PathBuilder::from_circle(center_x, center_y, radius) {
                        self.pixmap
                            .stroke_path(&path, &paint, &stroke, Transform::identity(), None);
                    }
                }
            }
            crate::style::types::TextEmphasisShape::Triangle => {
                let half = size * 0.5;
                let height = size * 0.9;
                let direction = matches!(
                    position,
                    crate::style::types::TextEmphasisPosition::Over
                        | crate::style::types::TextEmphasisPosition::OverLeft
                        | crate::style::types::TextEmphasisPosition::OverRight
                );
                let apex_y = if direction {
                    center_y - height * 0.5
                } else {
                    center_y + height * 0.5
                };
                let base_y = if direction {
                    center_y + height * 0.5
                } else {
                    center_y - height * 0.5
                };
                let mut builder = PathBuilder::new();
                builder.move_to(center_x, apex_y);
                builder.line_to(center_x - half, base_y);
                builder.line_to(center_x + half, base_y);
                builder.close();
                if let Some(path) = builder.finish() {
                    match fill {
                        crate::style::types::TextEmphasisFill::Filled => {
                            self.pixmap.fill_path(
                                &path,
                                &paint,
                                tiny_skia::FillRule::EvenOdd,
                                Transform::identity(),
                                None,
                            );
                        }
                        crate::style::types::TextEmphasisFill::Open => {
                            let mut stroke = Stroke::default();
                            stroke.width = (size * 0.18).max(0.5);
                            self.pixmap
                                .stroke_path(&path, &paint, &stroke, Transform::identity(), None);
                        }
                    }
                }
            }
            crate::style::types::TextEmphasisShape::Sesame => {
                let len = size * 0.75;
                let angle = 20.0_f32.to_radians();
                let dx = (angle.cos() * len * 0.5, angle.sin() * len * 0.5);
                let mut builder = PathBuilder::new();
                builder.move_to(center_x - dx.0, center_y - dx.1);
                builder.line_to(center_x + dx.0, center_y + dx.1);
                if let Some(path) = builder.finish() {
                    let mut stroke = Stroke::default();
                    stroke.width = (size * 0.2).max(0.6);
                    stroke.line_cap = tiny_skia::LineCap::Round;
                    self.pixmap
                        .stroke_path(&path, &paint, &stroke, Transform::identity(), None);
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
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

    fn scaled(self, scale: f32) -> Self {
        Self {
            underline_pos: self.underline_pos * scale,
            underline_thickness: self.underline_thickness * scale,
            strike_pos: self.strike_pos * scale,
            strike_thickness: self.strike_thickness * scale,
            ascent: self.ascent * scale,
            descent: self.descent * scale,
        }
    }
}

fn collect_underline_exclusions(
    runs: &[ShapedRun],
    line_start: f32,
    baseline_y: f32,
    band_top: f32,
    band_bottom: f32,
    skip_all: bool,
    device_scale: f32,
) -> Vec<(f32, f32)> {
    let mut intervals = Vec::new();
    // Small inflation to account for antialiasing without swallowing the entire line.
    let tolerance = 0.5 * device_scale;

    let mut pen_x = line_start * device_scale;
    for run in runs {
        let face = match ttf_parser::Face::parse(&run.font.data, run.font.index) {
            Ok(f) => f,
            Err(_) => continue,
        };
        let units_per_em = face.units_per_em() as f32;
        if units_per_em == 0.0 {
            continue;
        }
        let mut scale = run.font_size / units_per_em;
        scale *= run.scale * device_scale;
        let advance = run.advance * device_scale;
        let run_origin = if run.direction.is_rtl() { pen_x + advance } else { pen_x };

        for glyph in &run.glyphs {
            let glyph_x = match run.direction {
                crate::text::pipeline::Direction::RightToLeft => run_origin - glyph.x_offset * device_scale,
                _ => run_origin + glyph.x_offset * device_scale,
            };
            let glyph_y = baseline_y - glyph.y_offset * device_scale;
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

        pen_x += advance;
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

fn color_to_skia(color: Rgba) -> tiny_skia::Color {
    let alpha = (color.a * 255.0).clamp(0.0, 255.0).round() as u8;
    tiny_skia::Color::from_rgba8(color.r, color.g, color.b, alpha)
}

fn build_transform(style: Option<&ComputedStyle>, bounds: Rect) -> Option<Transform> {
    let style = style?;
    if style.transform.is_empty() {
        return None;
    }

    let mut ts = Transform::identity();
    for component in &style.transform {
        let next = match component {
            crate::css::types::Transform::Translate(x, y) => {
                let tx = resolve_transform_length(x, style.font_size, bounds.width());
                let ty = resolve_transform_length(y, style.font_size, bounds.height());
                Transform::from_translate(tx, ty)
            }
            crate::css::types::Transform::TranslateX(x) => {
                let tx = resolve_transform_length(x, style.font_size, bounds.width());
                Transform::from_translate(tx, 0.0)
            }
            crate::css::types::Transform::TranslateY(y) => {
                let ty = resolve_transform_length(y, style.font_size, bounds.height());
                Transform::from_translate(0.0, ty)
            }
            crate::css::types::Transform::Scale(sx, sy) => Transform::from_scale(*sx, *sy),
            crate::css::types::Transform::ScaleX(sx) => Transform::from_scale(*sx, 1.0),
            crate::css::types::Transform::ScaleY(sy) => Transform::from_scale(1.0, *sy),
            crate::css::types::Transform::Rotate(deg) => Transform::from_rotate(*deg),
            crate::css::types::Transform::SkewX(deg) => Transform::from_skew(deg.to_radians().tan(), 0.0),
            crate::css::types::Transform::SkewY(deg) => Transform::from_skew(0.0, deg.to_radians().tan()),
            crate::css::types::Transform::Matrix(a, b, c, d, e, f) => Transform::from_row(*a, *b, *c, *d, *e, *f),
        };
        ts = ts.pre_concat(next);
    }

    let origin_x = resolve_transform_length(&style.transform_origin.x, style.font_size, bounds.width());
    let origin_y = resolve_transform_length(&style.transform_origin.y, style.font_size, bounds.height());
    let origin = Point::new(bounds.x() + origin_x, bounds.y() + origin_y);

    // Apply transform around the resolved origin.
    ts = Transform::from_translate(origin.x, origin.y)
        .pre_concat(ts)
        .pre_concat(Transform::from_translate(-origin.x, -origin.y));

    Some(ts)
}

fn resolve_transform_length(len: &Length, font_size: f32, percentage_base: f32) -> f32 {
    match len.unit {
        LengthUnit::Percent => len.resolve_against(percentage_base),
        LengthUnit::Em | LengthUnit::Rem => len.resolve_with_font_size(font_size),
        _ if len.unit.is_absolute() => len.to_px(),
        _ => len.value,
    }
}

fn transform_rect(rect: Rect, ts: &Transform) -> Rect {
    let corners = [
        (rect.min_x(), rect.min_y()),
        (rect.max_x(), rect.min_y()),
        (rect.max_x(), rect.max_y()),
        (rect.min_x(), rect.max_y()),
    ];
    let mut min_x = f32::INFINITY;
    let mut min_y = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    let mut max_y = f32::NEG_INFINITY;

    for (x, y) in corners {
        let tx = x * ts.sx + y * ts.kx + ts.tx;
        let ty = x * ts.ky + y * ts.sy + ts.ty;
        min_x = min_x.min(tx);
        min_y = min_y.min(ty);
        max_x = max_x.max(tx);
        max_y = max_y.max(ty);
    }

    Rect::from_xywh(min_x, min_y, max_x - min_x, max_y - min_y)
}

fn approx_same_rect(a: Rect, b: Rect) -> bool {
    let eps = 0.001;
    (a.min_x() - b.min_x()).abs() <= eps
        && (a.min_y() - b.min_y()).abs() <= eps
        && (a.width() - b.width()).abs() <= eps
        && (a.height() - b.height()).abs() <= eps
}

fn clip_rect_axes(mut bounds: Rect, clip: Rect, clip_x: bool, clip_y: bool) -> Rect {
    if clip_x {
        let min_x = bounds.min_x().max(clip.min_x());
        let max_x = bounds.max_x().min(clip.max_x());
        bounds.origin.x = min_x;
        bounds.size.width = (max_x - min_x).max(0.0);
    }
    if clip_y {
        let min_y = bounds.min_y().max(clip.min_y());
        let max_y = bounds.max_y().min(clip.max_y());
        bounds.origin.y = min_y;
        bounds.size.height = (max_y - min_y).max(0.0);
    }
    bounds
}

fn compute_descendant_bounds(commands: &[DisplayCommand], root_rect: Rect) -> Option<Rect> {
    let mut current: Option<Rect> = None;
    for cmd in commands {
        if matches!(cmd, DisplayCommand::Background { rect, .. } | DisplayCommand::Border { rect, .. } if approx_same_rect(*rect, root_rect))
        {
            continue;
        }
        if let Some(r) = command_bounds(cmd) {
            current = Some(match current {
                Some(acc) => acc.union(r),
                None => r,
            });
        }
    }
    current
}

fn stacking_context_bounds(
    commands: &[DisplayCommand],
    filters: &[ResolvedFilter],
    backdrop_filters: &[ResolvedFilter],
    rect: Rect,
    transform: Option<&Transform>,
    clip: Option<&(Rect, BorderRadii, bool, bool)>,
) -> Option<Rect> {
    let mut base = rect;
    if let Some(desc) = compute_descendant_bounds(commands, rect) {
        let clipped = if let Some((clip_rect, _, clip_x, clip_y)) = clip {
            clip_non_outline(commands, desc, *clip_rect, *clip_x, *clip_y)
        } else {
            desc
        };
        base = base.union(clipped);
    }
    let (l, t, r, b) = filter_outset(filters);
    let (bl, bt, br, bb) = filter_outset(backdrop_filters);
    let total_l = l.max(bl);
    let total_t = t.max(bt);
    let total_r = r.max(br);
    let total_b = b.max(bb);
    if total_l > 0.0 || total_t > 0.0 || total_r > 0.0 || total_b > 0.0 {
        base = Rect::from_xywh(
            base.min_x() - total_l,
            base.min_y() - total_t,
            base.width() + total_l + total_r,
            base.height() + total_t + total_b,
        );
    }
    if let Some(ts) = transform {
        let transformed = transform_rect(base, ts);
        base = base.union(transformed);
    }
    Some(base)
}

fn command_bounds(cmd: &DisplayCommand) -> Option<Rect> {
    match cmd {
        DisplayCommand::Background { rect, .. }
        | DisplayCommand::Border { rect, .. }
        | DisplayCommand::Outline { rect, .. }
        | DisplayCommand::Text { rect, .. }
        | DisplayCommand::Replaced { rect, .. } => Some(*rect),
        DisplayCommand::StackingContext {
            commands,
            filters,
            backdrop_filters,
            clip,
            rect,
            transform,
            ..
        } => stacking_context_bounds(
            commands,
            filters,
            backdrop_filters,
            *rect,
            transform.as_ref(),
            clip.as_ref(),
        ),
    }
}

#[allow(dead_code)]
fn compute_commands_bounds(commands: &[DisplayCommand]) -> Option<Rect> {
    let mut current: Option<Rect> = None;
    for cmd in commands {
        if let Some(r) = command_bounds(cmd) {
            current = Some(match current {
                Some(acc) => acc.union(r),
                None => r,
            });
        }
    }
    current
}

fn clip_non_outline(
    commands: &[DisplayCommand],
    mut bounds: Rect,
    clip_rect: Rect,
    clip_x: bool,
    clip_y: bool,
) -> Rect {
    let mut current: Option<Rect> = None;
    for cmd in commands {
        if matches!(cmd, DisplayCommand::Outline { .. }) {
            continue;
        }
        if let Some(r) = command_bounds(cmd) {
            let clipped = clip_rect_axes(r, clip_rect, clip_x, clip_y);
            current = Some(current.map(|c| c.union(clipped)).unwrap_or(clipped));
        }
    }
    if let Some(c) = current {
        bounds = bounds.union(c);
    }
    bounds
}

fn translate_commands(commands: Vec<DisplayCommand>, dx: f32, dy: f32) -> Vec<DisplayCommand> {
    let offset = Point::new(dx, dy);
    commands
        .into_iter()
        .map(|cmd| match cmd {
            DisplayCommand::Background { rect, style } => DisplayCommand::Background {
                rect: rect.translate(offset),
                style,
            },
            DisplayCommand::Border { rect, style } => DisplayCommand::Border {
                rect: rect.translate(offset),
                style,
            },
            DisplayCommand::Outline { rect, style } => DisplayCommand::Outline {
                rect: rect.translate(offset),
                style,
            },
            DisplayCommand::Text {
                rect,
                baseline_offset,
                text,
                runs,
                style,
            } => DisplayCommand::Text {
                rect: rect.translate(offset),
                baseline_offset,
                text,
                runs,
                style,
            },
            DisplayCommand::Replaced {
                rect,
                replaced_type,
                style,
            } => DisplayCommand::Replaced {
                rect: rect.translate(offset),
                replaced_type,
                style,
            },
            DisplayCommand::StackingContext {
                rect,
                opacity,
                transform,
                blend_mode,
                isolated,
                filters,
                backdrop_filters,
                radii,
                clip,
                commands,
            } => DisplayCommand::StackingContext {
                rect: rect.translate(offset),
                opacity,
                transform,
                blend_mode,
                isolated,
                filters,
                backdrop_filters,
                radii,
                clip: clip.map(|(rect, r, cx, cy)| (rect.translate(offset), r, cx, cy)),
                commands: translate_commands(commands, dx, dy),
            },
        })
        .collect()
}

fn resolve_filters(filters: &[FilterFunction], style: &ComputedStyle, _scale: f32) -> Vec<ResolvedFilter> {
    filters
        .iter()
        .filter_map(|f| match f {
            FilterFunction::Blur(len) => Some(ResolvedFilter::Blur(resolve_filter_length(len, style))),
            FilterFunction::Brightness(v) => Some(ResolvedFilter::Brightness(*v)),
            FilterFunction::Contrast(v) => Some(ResolvedFilter::Contrast(*v)),
            FilterFunction::Grayscale(v) => Some(ResolvedFilter::Grayscale(*v)),
            FilterFunction::Sepia(v) => Some(ResolvedFilter::Sepia(*v)),
            FilterFunction::Saturate(v) => Some(ResolvedFilter::Saturate(*v)),
            FilterFunction::HueRotate(deg) => Some(ResolvedFilter::HueRotate(*deg)),
            FilterFunction::Invert(v) => Some(ResolvedFilter::Invert(*v)),
            FilterFunction::Opacity(v) => Some(ResolvedFilter::Opacity(*v)),
            FilterFunction::DropShadow(shadow) => {
                let color = match shadow.color {
                    FilterColor::CurrentColor => style.color,
                    FilterColor::Color(c) => c,
                };
                Some(ResolvedFilter::DropShadow {
                    offset_x: resolve_filter_length(&shadow.offset_x, style),
                    offset_y: resolve_filter_length(&shadow.offset_y, style),
                    blur_radius: resolve_filter_length(&shadow.blur_radius, style),
                    spread: resolve_filter_length(&shadow.spread, style),
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

fn filter_outset(filters: &[ResolvedFilter]) -> (f32, f32, f32, f32) {
    let mut left: f32 = 0.0;
    let mut top: f32 = 0.0;
    let mut right: f32 = 0.0;
    let mut bottom: f32 = 0.0;

    for filter in filters {
        match *filter {
            ResolvedFilter::Blur(radius) => {
                let delta = radius.abs() * 3.0;
                left = left.max(delta);
                right = right.max(delta);
                top = top.max(delta);
                bottom = bottom.max(delta);
            }
            ResolvedFilter::DropShadow {
                offset_x,
                offset_y,
                blur_radius,
                spread,
                ..
            } => {
                let delta = blur_radius.abs() * 3.0 + spread.max(0.0);
                left = left.max(delta - offset_x.min(0.0));
                right = right.max(delta + offset_x.max(0.0));
                top = top.max(delta - offset_y.min(0.0));
                bottom = bottom.max(delta + offset_y.max(0.0));
            }
            _ => {}
        }
    }

    (left.max(0.0), top.max(0.0), right.max(0.0), bottom.max(0.0))
}

fn apply_filters(pixmap: &mut Pixmap, filters: &[ResolvedFilter], scale: f32) {
    for filter in filters {
        match *filter {
            ResolvedFilter::Blur(radius) => apply_gaussian_blur(pixmap, radius * scale),
            ResolvedFilter::Brightness(amount) => apply_color_filter(pixmap, |c, a| (scale_color(c, amount), a)),
            ResolvedFilter::Contrast(amount) => apply_color_filter(pixmap, |c, a| (apply_contrast(c, amount), a)),
            ResolvedFilter::Grayscale(amount) => apply_color_filter(pixmap, |c, a| (grayscale(c, amount), a)),
            ResolvedFilter::Sepia(amount) => apply_color_filter(pixmap, |c, a| (sepia(c, amount), a)),
            ResolvedFilter::Saturate(amount) => apply_color_filter(pixmap, |c, a| (saturate(c, amount), a)),
            ResolvedFilter::HueRotate(deg) => apply_color_filter(pixmap, |c, a| (hue_rotate(c, deg), a)),
            ResolvedFilter::Invert(amount) => apply_color_filter(pixmap, |c, a| (invert(c, amount), a)),
            ResolvedFilter::Opacity(amount) => apply_color_filter(pixmap, |c, a| (c, a * amount)),
            ResolvedFilter::DropShadow {
                offset_x,
                offset_y,
                blur_radius,
                spread,
                color,
            } => apply_drop_shadow(
                pixmap,
                offset_x * scale,
                offset_y * scale,
                blur_radius * scale,
                spread * scale,
                color,
            ),
        }
    }
}

fn apply_backdrop_filters(
    pixmap: &mut Pixmap,
    bounds: &Rect,
    filters: &[ResolvedFilter],
    radii: BorderRadii,
    scale: f32,
) {
    if filters.is_empty() {
        return;
    }
    let (out_l, out_t, out_r, out_b) = filter_outset(filters);
    let out_l = out_l * scale;
    let out_t = out_t * scale;
    let out_r = out_r * scale;
    let out_b = out_b * scale;

    let x = (bounds.min_x() - out_l).floor() as i32;
    let y = (bounds.min_y() - out_t).floor() as i32;
    let width = (bounds.width() + out_l + out_r).ceil() as u32;
    let height = (bounds.height() + out_t + out_b).ceil() as u32;
    if width == 0 || height == 0 {
        return;
    }

    let pix_w = pixmap.width() as i32;
    let pix_h = pixmap.height() as i32;
    if x >= pix_w || y >= pix_h {
        return;
    }

    let clamped_x = x.max(0) as u32;
    let clamped_y = y.max(0) as u32;
    let max_w = pix_w.saturating_sub(clamped_x as i32).max(0) as u32;
    let max_h = pix_h.saturating_sub(clamped_y as i32).max(0) as u32;
    let region_w = width.min(max_w);
    let region_h = height.min(max_h);
    if region_w == 0 || region_h == 0 {
        return;
    }

    let mut region = match Pixmap::new(region_w, region_h) {
        Some(p) => p,
        None => return,
    };

    // Copy region
    let bytes_per_row = pixmap.width() as usize * 4;
    let region_row_bytes = region_w as usize * 4;
    let start = (clamped_y as usize * bytes_per_row) + clamped_x as usize * 4;
    let data = pixmap.data();
    let dest = region.data_mut();
    for row in 0..region_h as usize {
        let src_offset = start + row * bytes_per_row;
        let dst_offset = row * (region_w as usize);
        let src_slice = &data[src_offset..src_offset + region_row_bytes];
        let dst_slice = &mut dest[dst_offset..dst_offset + region_row_bytes];
        dst_slice.copy_from_slice(src_slice);
    }

    apply_filters(&mut region, filters, scale);
    if !radii.is_zero() {
        let local_rect = Rect::from_xywh(
            bounds.x() - clamped_x as f32,
            bounds.y() - clamped_y as f32,
            bounds.width(),
            bounds.height(),
        );
        apply_clip_mask_rect(&mut region, local_rect, radii);
    }

    let mut paint = PixmapPaint::default();
    paint.blend_mode = SkiaBlendMode::SourceOver;
    pixmap.draw_pixmap(
        clamped_x as i32,
        clamped_y as i32,
        region.as_ref(),
        &paint,
        Transform::identity(),
        None,
    );
}

fn apply_color_filter<F>(pixmap: &mut Pixmap, mut f: F)
where
    F: FnMut([f32; 3], f32) -> ([f32; 3], f32),
{
    for px in pixmap.pixels_mut() {
        let alpha = px.alpha() as f32 / 255.0;
        let base = if alpha > 0.0 {
            [
                (px.red() as f32 / 255.0) / alpha,
                (px.green() as f32 / 255.0) / alpha,
                (px.blue() as f32 / 255.0) / alpha,
            ]
        } else {
            [0.0, 0.0, 0.0]
        };
        let (mut color, mut new_alpha) = f(base, alpha);
        new_alpha = new_alpha.clamp(0.0, 1.0);
        color[0] = color[0].clamp(0.0, 1.0);
        color[1] = color[1].clamp(0.0, 1.0);
        color[2] = color[2].clamp(0.0, 1.0);

        let r = (color[0] * new_alpha * 255.0).round().clamp(0.0, 255.0) as u8;
        let g = (color[1] * new_alpha * 255.0).round().clamp(0.0, 255.0) as u8;
        let b = (color[2] * new_alpha * 255.0).round().clamp(0.0, 255.0) as u8;
        let a = (new_alpha * 255.0).round().clamp(0.0, 255.0) as u8;

        *px = PremultipliedColorU8::from_rgba(r, g, b, a).unwrap_or(PremultipliedColorU8::TRANSPARENT);
    }
}

fn scale_color(color: [f32; 3], factor: f32) -> [f32; 3] {
    [color[0] * factor, color[1] * factor, color[2] * factor]
}

fn apply_contrast(color: [f32; 3], factor: f32) -> [f32; 3] {
    [
        ((color[0] - 0.5) * factor + 0.5),
        ((color[1] - 0.5) * factor + 0.5),
        ((color[2] - 0.5) * factor + 0.5),
    ]
}

fn grayscale(color: [f32; 3], amount: f32) -> [f32; 3] {
    let gray = color[0] * 0.2126 + color[1] * 0.7152 + color[2] * 0.0722;
    [
        color[0] + (gray - color[0]) * amount,
        color[1] + (gray - color[1]) * amount,
        color[2] + (gray - color[2]) * amount,
    ]
}

fn sepia(color: [f32; 3], amount: f32) -> [f32; 3] {
    let sepia_r = color[0] * 0.393 + color[1] * 0.769 + color[2] * 0.189;
    let sepia_g = color[0] * 0.349 + color[1] * 0.686 + color[2] * 0.168;
    let sepia_b = color[0] * 0.272 + color[1] * 0.534 + color[2] * 0.131;
    [
        color[0] + (sepia_r - color[0]) * amount,
        color[1] + (sepia_g - color[1]) * amount,
        color[2] + (sepia_b - color[2]) * amount,
    ]
}

fn saturate(color: [f32; 3], factor: f32) -> [f32; 3] {
    let rw = 0.213;
    let gw = 0.715;
    let bw = 0.072;
    [
        (rw + (1.0 - rw) * factor) * color[0] + (gw - gw * factor) * color[1] + (bw - bw * factor) * color[2],
        (rw - rw * factor) * color[0] + (gw + (1.0 - gw) * factor) * color[1] + (bw - bw * factor) * color[2],
        (rw - rw * factor) * color[0] + (gw - gw * factor) * color[1] + (bw + (1.0 - bw) * factor) * color[2],
    ]
}

fn hue_rotate(color: [f32; 3], degrees: f32) -> [f32; 3] {
    let angle = degrees.to_radians();
    let cos = angle.cos();
    let sin = angle.sin();

    let r = color[0];
    let g = color[1];
    let b = color[2];

    [
        r * (0.213 + cos * 0.787 - sin * 0.213)
            + g * (0.715 - 0.715 * cos - 0.715 * sin)
            + b * (0.072 - 0.072 * cos + 0.928 * sin),
        r * (0.213 - 0.213 * cos + 0.143 * sin)
            + g * (0.715 + 0.285 * cos + 0.140 * sin)
            + b * (0.072 - 0.072 * cos - 0.283 * sin),
        r * (0.213 - 0.213 * cos - 0.787 * sin)
            + g * (0.715 - 0.715 * cos + 0.715 * sin)
            + b * (0.072 + 0.928 * cos + 0.072 * sin),
    ]
}

fn invert(color: [f32; 3], amount: f32) -> [f32; 3] {
    [
        color[0] + (1.0 - color[0] - color[0]) * amount,
        color[1] + (1.0 - color[1] - color[1]) * amount,
        color[2] + (1.0 - color[2] - color[2]) * amount,
    ]
}

fn apply_drop_shadow(pixmap: &mut Pixmap, offset_x: f32, offset_y: f32, blur_radius: f32, spread: f32, color: Rgba) {
    if pixmap.width() == 0 || pixmap.height() == 0 {
        return;
    }

    let source = pixmap.clone();
    let mut shadow = match Pixmap::new(source.width(), source.height()) {
        Some(p) => p,
        None => return,
    };

    {
        let src = source.pixels();
        let dst = shadow.pixels_mut();
        for (src_px, dst_px) in src.iter().zip(dst.iter_mut()) {
            let alpha = src_px.alpha() as f32 / 255.0;
            if alpha == 0.0 {
                *dst_px = PremultipliedColorU8::TRANSPARENT;
                continue;
            }
            let total_alpha = (color.a * alpha).clamp(0.0, 1.0);
            let r = (color.r as f32 / 255.0) * total_alpha;
            let g = (color.g as f32 / 255.0) * total_alpha;
            let b = (color.b as f32 / 255.0) * total_alpha;
            let a = total_alpha * 255.0;
            *dst_px = PremultipliedColorU8::from_rgba(
                (r * 255.0).round() as u8,
                (g * 255.0).round() as u8,
                (b * 255.0).round() as u8,
                a.round().clamp(0.0, 255.0) as u8,
            )
            .unwrap_or(PremultipliedColorU8::TRANSPARENT);
        }
    }

    if spread > 0.0 {
        apply_spread(&mut shadow, spread);
    }

    if blur_radius > 0.0 {
        apply_gaussian_blur(&mut shadow, blur_radius);
    }

    let mut result = match Pixmap::new(source.width(), source.height()) {
        Some(p) => p,
        None => return,
    };

    let mut paint = PixmapPaint::default();
    paint.blend_mode = SkiaBlendMode::SourceOver;
    result.draw_pixmap(
        0,
        0,
        shadow.as_ref(),
        &paint,
        Transform::from_translate(offset_x, offset_y),
        None,
    );
    result.draw_pixmap(0, 0, source.as_ref(), &paint, Transform::identity(), None);

    *pixmap = result;
}

fn apply_spread(pixmap: &mut Pixmap, spread: f32) {
    let radius = spread.ceil() as i32;
    if radius <= 0 {
        return;
    }
    let width = pixmap.width() as i32;
    let height = pixmap.height() as i32;
    let original = pixmap.clone();
    let src = original.pixels();
    let dst = pixmap.pixels_mut();

    for y in 0..height {
        for x in 0..width {
            let mut max = [0u8; 4];
            for dy in -radius..=radius {
                for dx in -radius..=radius {
                    let ny = (y + dy).clamp(0, height - 1);
                    let nx = (x + dx).clamp(0, width - 1);
                    let idx = (ny as usize) * (width as usize) + nx as usize;
                    let px = src[idx];
                    max[0] = max[0].max(px.red());
                    max[1] = max[1].max(px.green());
                    max[2] = max[2].max(px.blue());
                    max[3] = max[3].max(px.alpha());
                }
            }
            let idx = (y as usize) * (width as usize) + x as usize;
            dst[idx] = PremultipliedColorU8::from_rgba(max[0], max[1], max[2], max[3])
                .unwrap_or(PremultipliedColorU8::TRANSPARENT);
        }
    }
}

fn is_hsl_blend(mode: MixBlendMode) -> bool {
    matches!(
        mode,
        MixBlendMode::Hue | MixBlendMode::Saturation | MixBlendMode::Color | MixBlendMode::Luminosity
    )
}

fn rgb_to_hsl(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    let l = (max + min) / 2.0;
    if (max - min).abs() < f32::EPSILON {
        return (0.0, 0.0, l);
    }

    let d = max - min;
    let s = if l > 0.5 {
        d / (2.0 - max - min)
    } else {
        d / (max + min)
    };
    let h = if (max - r).abs() < f32::EPSILON {
        (g - b) / d + if g < b { 6.0 } else { 0.0 }
    } else if (max - g).abs() < f32::EPSILON {
        (b - r) / d + 2.0
    } else {
        (r - g) / d + 4.0
    } / 6.0;
    (h, s, l)
}

fn hue_to_rgb(p: f32, q: f32, t: f32) -> f32 {
    let mut t = t;
    if t < 0.0 {
        t += 1.0;
    }
    if t > 1.0 {
        t -= 1.0;
    }
    if t < 1.0 / 6.0 {
        p + (q - p) * 6.0 * t
    } else if t < 0.5 {
        q
    } else if t < 2.0 / 3.0 {
        p + (q - p) * (2.0 / 3.0 - t) * 6.0
    } else {
        p
    }
}

fn hsl_to_rgb(h: f32, s: f32, l: f32) -> (f32, f32, f32) {
    if s <= 0.0 {
        return (l, l, l);
    }
    let q = if l < 0.5 { l * (1.0 + s) } else { l + s - l * s };
    let p = 2.0 * l - q;
    let r = hue_to_rgb(p, q, h + 1.0 / 3.0);
    let g = hue_to_rgb(p, q, h);
    let b = hue_to_rgb(p, q, h - 1.0 / 3.0);
    (r, g, b)
}

fn apply_hsl_blend(mode: MixBlendMode, src: (f32, f32, f32), dst: (f32, f32, f32)) -> (f32, f32, f32) {
    let (sh, ss, sl) = rgb_to_hsl(src.0, src.1, src.2);
    let (dh, ds, dl) = rgb_to_hsl(dst.0, dst.1, dst.2);
    match mode {
        MixBlendMode::Hue => hsl_to_rgb(sh, ds, dl),
        MixBlendMode::Saturation => hsl_to_rgb(dh, ss, dl),
        MixBlendMode::Color => hsl_to_rgb(sh, ss, dl),
        MixBlendMode::Luminosity => hsl_to_rgb(dh, ds, sl),
        _ => dst,
    }
}

fn composite_hsl_layer(dest: &mut Pixmap, layer: &Pixmap, opacity: f32, mode: MixBlendMode, area: Option<Rect>) {
    if !is_hsl_blend(mode) {
        return;
    }
    let dest_width = dest.width() as usize;
    let dest_height = dest.height() as usize;
    let layer_width = layer.width() as usize;
    let layer_height = layer.height() as usize;
    if dest_width == 0 || dest_height == 0 || layer_width == 0 || layer_height == 0 {
        return;
    }

    let max_width = dest_width.min(layer_width);
    let max_height = dest_height.min(layer_height);
    let (mut x0, mut y0, mut x1, mut y1) = if let Some(rect) = area {
        let x0 = rect.min_x().floor() as i32;
        let y0 = rect.min_y().floor() as i32;
        let x1 = rect.max_x().ceil() as i32;
        let y1 = rect.max_y().ceil() as i32;
        (x0, y0, x1, y1)
    } else {
        (0, 0, max_width as i32, max_height as i32)
    };
    x0 = x0.clamp(0, max_width as i32);
    y0 = y0.clamp(0, max_height as i32);
    x1 = x1.clamp(0, max_width as i32);
    y1 = y1.clamp(0, max_height as i32);
    if x0 >= x1 || y0 >= y1 {
        return;
    }

    let src_pixels = layer.pixels();
    let dst_pixels = dest.pixels_mut();
    let src_stride = layer_width;
    let dst_stride = dest_width;
    let opacity = opacity.clamp(0.0, 1.0);

    for y in y0..y1 {
        let yi = y as usize;
        for x in x0..x1 {
            let xi = x as usize;
            let src_px = src_pixels[yi * src_stride + xi];
            let raw_sa = src_px.alpha() as f32 / 255.0;
            if raw_sa == 0.0 || opacity == 0.0 {
                continue;
            }
            let dst_px = &mut dst_pixels[yi * dst_stride + xi];
            let sa = (raw_sa * opacity).clamp(0.0, 1.0);
            let da = dst_px.alpha() as f32 / 255.0;

            let src_rgb = if raw_sa > 0.0 {
                (
                    (src_px.red() as f32 / 255.0) / raw_sa,
                    (src_px.green() as f32 / 255.0) / raw_sa,
                    (src_px.blue() as f32 / 255.0) / raw_sa,
                )
            } else {
                (0.0, 0.0, 0.0)
            };
            let dst_rgb = if da > 0.0 {
                (
                    (dst_px.red() as f32 / 255.0) / da,
                    (dst_px.green() as f32 / 255.0) / da,
                    (dst_px.blue() as f32 / 255.0) / da,
                )
            } else {
                (0.0, 0.0, 0.0)
            };

            let blended_rgb = apply_hsl_blend(mode, src_rgb, dst_rgb);

            let out_a = sa + da * (1.0 - sa);
            let out_rgb = if out_a > 0.0 {
                (
                    (blended_rgb.0 * sa + dst_rgb.0 * da * (1.0 - sa)) / out_a,
                    (blended_rgb.1 * sa + dst_rgb.1 * da * (1.0 - sa)) / out_a,
                    (blended_rgb.2 * sa + dst_rgb.2 * da * (1.0 - sa)) / out_a,
                )
            } else {
                (0.0, 0.0, 0.0)
            };

            let out_a_u8 = (out_a * 255.0 + 0.5).clamp(0.0, 255.0) as u8;
            let scale = out_a;
            let r = ((out_rgb.0 * scale) * 255.0 + 0.5).clamp(0.0, out_a_u8 as f32) as u8;
            let g = ((out_rgb.1 * scale) * 255.0 + 0.5).clamp(0.0, out_a_u8 as f32) as u8;
            let b = ((out_rgb.2 * scale) * 255.0 + 0.5).clamp(0.0, out_a_u8 as f32) as u8;
            *dst_px = PremultipliedColorU8::from_rgba(r, g, b, out_a_u8).unwrap_or(PremultipliedColorU8::TRANSPARENT);
        }
    }
}

fn map_blend_mode(mode: MixBlendMode) -> SkiaBlendMode {
    match mode {
        MixBlendMode::Normal => SkiaBlendMode::SourceOver,
        MixBlendMode::Multiply => SkiaBlendMode::Multiply,
        MixBlendMode::Screen => SkiaBlendMode::Screen,
        MixBlendMode::Overlay => SkiaBlendMode::Overlay,
        MixBlendMode::Darken => SkiaBlendMode::Darken,
        MixBlendMode::Lighten => SkiaBlendMode::Lighten,
        MixBlendMode::ColorDodge => SkiaBlendMode::ColorDodge,
        MixBlendMode::ColorBurn => SkiaBlendMode::ColorBurn,
        MixBlendMode::HardLight => SkiaBlendMode::HardLight,
        MixBlendMode::SoftLight => SkiaBlendMode::SoftLight,
        MixBlendMode::Difference => SkiaBlendMode::Difference,
        MixBlendMode::Exclusion => SkiaBlendMode::Exclusion,
        MixBlendMode::Hue => SkiaBlendMode::Hue,
        MixBlendMode::Saturation => SkiaBlendMode::Saturation,
        MixBlendMode::Color => SkiaBlendMode::Color,
        MixBlendMode::Luminosity => SkiaBlendMode::Luminosity,
    }
}

fn resolve_border_radii(style: Option<&ComputedStyle>, bounds: Rect) -> BorderRadii {
    let Some(style) = style else { return BorderRadii::ZERO };
    let w = bounds.width().max(0.0);
    let h = bounds.height().max(0.0);
    if w <= 0.0 || h <= 0.0 {
        return BorderRadii::ZERO;
    }

    fn resolve_radius(len: &Length, reference: f32, font_size: f32) -> f32 {
        match len.unit {
            LengthUnit::Percent => len.resolve_against(reference),
            LengthUnit::Em | LengthUnit::Rem => len.resolve_with_font_size(font_size),
            _ if len.unit.is_absolute() => len.to_px(),
            // Approximate unknown relative units using font size as a base.
            _ => len.value * font_size,
        }
    }

    let radii = BorderRadii {
        top_left: resolve_radius(&style.border_top_left_radius, w, style.font_size),
        top_right: resolve_radius(&style.border_top_right_radius, w, style.font_size),
        bottom_right: resolve_radius(&style.border_bottom_right_radius, w, style.font_size),
        bottom_left: resolve_radius(&style.border_bottom_left_radius, w, style.font_size),
    };
    radii.clamped(w, h)
}

fn resolve_clip_radii(
    style: &ComputedStyle,
    rects: &BackgroundRects,
    clip: crate::style::types::BackgroundBox,
) -> BorderRadii {
    let base = resolve_border_radii(Some(style), rects.border);
    if base.is_zero() {
        return base;
    }

    let percentage_base = rects.border.width().max(0.0);
    let font_size = style.font_size;
    let border_left = resolve_length_for_paint(&style.border_left_width, font_size, percentage_base);
    let border_right = resolve_length_for_paint(&style.border_right_width, font_size, percentage_base);
    let border_top = resolve_length_for_paint(&style.border_top_width, font_size, percentage_base);
    let border_bottom = resolve_length_for_paint(&style.border_bottom_width, font_size, percentage_base);

    let padding_left = resolve_length_for_paint(&style.padding_left, font_size, percentage_base);
    let padding_right = resolve_length_for_paint(&style.padding_right, font_size, percentage_base);
    let padding_top = resolve_length_for_paint(&style.padding_top, font_size, percentage_base);
    let padding_bottom = resolve_length_for_paint(&style.padding_bottom, font_size, percentage_base);

    match clip {
        crate::style::types::BackgroundBox::BorderBox => base,
        crate::style::types::BackgroundBox::PaddingBox => {
            let shrunk = BorderRadii {
                top_left: (base.top_left - border_left.max(border_top)).max(0.0),
                top_right: (base.top_right - border_right.max(border_top)).max(0.0),
                bottom_right: (base.bottom_right - border_right.max(border_bottom)).max(0.0),
                bottom_left: (base.bottom_left - border_left.max(border_bottom)).max(0.0),
            };
            shrunk.clamped(rects.padding.width(), rects.padding.height())
        }
        crate::style::types::BackgroundBox::ContentBox => {
            let shrink_left = border_left + padding_left;
            let shrink_right = border_right + padding_right;
            let shrink_top = border_top + padding_top;
            let shrink_bottom = border_bottom + padding_bottom;
            let shrunk = BorderRadii {
                top_left: (base.top_left - shrink_left.max(shrink_top)).max(0.0),
                top_right: (base.top_right - shrink_right.max(shrink_top)).max(0.0),
                bottom_right: (base.bottom_right - shrink_right.max(shrink_bottom)).max(0.0),
                bottom_left: (base.bottom_left - shrink_left.max(shrink_bottom)).max(0.0),
            };
            shrunk.clamped(rects.content.width(), rects.content.height())
        }
    }
}

fn build_rounded_rect_mask(rect: Rect, radii: BorderRadii, canvas_w: u32, canvas_h: u32) -> Option<Mask> {
    if canvas_w == 0 || canvas_h == 0 || rect.width() <= 0.0 || rect.height() <= 0.0 {
        return None;
    }

    let mut mask_pixmap = Pixmap::new(canvas_w, canvas_h)?;
    let _ = fill_rounded_rect(
        &mut mask_pixmap,
        rect.x(),
        rect.y(),
        rect.width(),
        rect.height(),
        &radii,
        Rgba::new(255, 255, 255, 1.0),
    );
    Some(Mask::from_pixmap(mask_pixmap.as_ref(), MaskType::Alpha))
}

fn apply_clip_mask_rect(pixmap: &mut Pixmap, rect: Rect, radii: BorderRadii) {
    if rect.width() <= 0.0 || rect.height() <= 0.0 {
        return;
    }
    let width = pixmap.width();
    let height = pixmap.height();
    if width == 0 || height == 0 {
        return;
    }

    let mut mask_pixmap = match Pixmap::new(width, height) {
        Some(p) => p,
        None => return,
    };
    let clamped = radii.clamped(rect.width(), rect.height());
    let _ = fill_rounded_rect(
        &mut mask_pixmap,
        rect.x(),
        rect.y(),
        rect.width(),
        rect.height(),
        &clamped,
        Rgba::new(255, 255, 255, 1.0),
    );

    let mask = Mask::from_pixmap(mask_pixmap.as_ref(), MaskType::Alpha);
    pixmap.apply_mask(&mask);

    // Hard-clip pixels outside the rectangle to avoid filter bleed.
    let x0 = rect.x().floor().max(0.0) as u32;
    let y0 = rect.y().floor().max(0.0) as u32;
    let x1 = (rect.x() + rect.width()).ceil().min(width as f32) as u32;
    let y1 = (rect.y() + rect.height()).ceil().min(height as f32) as u32;
    let data = pixmap.data_mut();
    let stride = (width as usize) * 4;
    for y in 0..height {
        for x in 0..width {
            if x < x0 || x >= x1 || y < y0 || y >= y1 {
                let idx = (y as usize * stride) + (x as usize * 4);
                data[idx + 0] = 0;
                data[idx + 1] = 0;
                data[idx + 2] = 0;
                data[idx + 3] = 0;
            }
        }
    }
}

#[derive(Clone, Copy)]
struct BackgroundRects {
    border: Rect,
    padding: Rect,
    content: Rect,
}

fn background_rects(x: f32, y: f32, width: f32, height: f32, style: &ComputedStyle) -> BackgroundRects {
    let base = width.max(0.0);
    let font_size = style.font_size;

    let border_left = resolve_length_for_paint(&style.border_left_width, font_size, base);
    let border_right = resolve_length_for_paint(&style.border_right_width, font_size, base);
    let border_top = resolve_length_for_paint(&style.border_top_width, font_size, base);
    let border_bottom = resolve_length_for_paint(&style.border_bottom_width, font_size, base);

    let padding_left = resolve_length_for_paint(&style.padding_left, font_size, base);
    let padding_right = resolve_length_for_paint(&style.padding_right, font_size, base);
    let padding_top = resolve_length_for_paint(&style.padding_top, font_size, base);
    let padding_bottom = resolve_length_for_paint(&style.padding_bottom, font_size, base);

    let border_rect = Rect::from_xywh(x, y, width, height);
    let padding_rect = inset_rect(border_rect, border_left, border_top, border_right, border_bottom);
    let content_rect = inset_rect(padding_rect, padding_left, padding_top, padding_right, padding_bottom);

    BackgroundRects {
        border: border_rect,
        padding: padding_rect,
        content: content_rect,
    }
}

fn inset_rect(rect: Rect, left: f32, top: f32, right: f32, bottom: f32) -> Rect {
    let new_x = rect.x() + left;
    let new_y = rect.y() + top;
    let new_w = (rect.width() - left - right).max(0.0);
    let new_h = (rect.height() - top - bottom).max(0.0);
    Rect::from_xywh(new_x, new_y, new_w, new_h)
}

fn resolve_length_for_paint(len: &Length, font_size: f32, percentage_base: f32) -> f32 {
    match len.unit {
        LengthUnit::Percent => len.resolve_against(percentage_base),
        LengthUnit::Em | LengthUnit::Rem => len.resolve_with_font_size(font_size),
        _ if len.unit.is_absolute() => len.to_px(),
        _ => len.value,
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
                        Some(resolve_length_for_paint(&len, font_size, area).max(0.0))
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
        let offset = resolve_length_for_paint(&comp.offset, font_size, available);
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

fn gradient_stops(stops: &[(f32, Rgba)]) -> Vec<tiny_skia::GradientStop> {
    stops
        .iter()
        .map(|(pos, color)| {
            let alpha = (color.a * 255.0).round().clamp(0.0, 255.0) as u8;
            tiny_skia::GradientStop::new(
                pos.clamp(0.0, 1.0),
                tiny_skia::Color::from_rgba8(color.r, color.g, color.b, alpha),
            )
        })
        .collect()
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
            let start = aligned_start(area_start + offset, tile_len, clip_min);
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
                // When fewer than two tiles fit, center the single tile per CSS Backgrounds 3.
                let centered = area_start + offset + (area_len - tile_len) * 0.5;
                vec![centered]
            }
        }
    }
}

/// Paints a fragment tree to a pixmap
///
/// This is the main entry point for painting.
pub fn paint_tree(tree: &FragmentTree, width: u32, height: u32, background: Rgba) -> Result<Pixmap> {
    paint_tree_scaled(tree, width, height, background, 1.0)
}

/// Paints a fragment tree at the given device scale.
pub fn paint_tree_scaled(
    tree: &FragmentTree,
    width: u32,
    height: u32,
    background: Rgba,
    scale: f32,
) -> Result<Pixmap> {
    let painter = Painter::with_resources_scaled(width, height, background, FontContext::new(), ImageCache::new(), scale)?;
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
    paint_tree_with_resources_scaled(tree, width, height, background, font_ctx, image_cache, 1.0)
}

/// Paints a fragment tree using explicit resources at the provided device scale.
pub fn paint_tree_with_resources_scaled(
    tree: &FragmentTree,
    width: u32,
    height: u32,
    background: Rgba,
    font_ctx: FontContext,
    image_cache: ImageCache,
    scale: f32,
) -> Result<Pixmap> {
    let painter = Painter::with_resources_scaled(width, height, background, font_ctx, image_cache, scale)?;
    painter.paint(tree)
}

/// Scales a pixmap by the given device pixel ratio, returning a new pixmap.
/// This is a coarse fallback for high-DPI outputs; painting should ideally
/// happen directly at device resolution.
pub fn scale_pixmap_for_dpr(pixmap: Pixmap, dpr: f32) -> Result<Pixmap> {
    let dpr = if dpr.is_finite() && dpr > 0.0 { dpr } else { 1.0 };
    if (dpr - 1.0).abs() < f32::EPSILON {
        return Ok(pixmap);
    }

    let new_w = (((pixmap.width() as f32) * dpr).round()).max(1.0) as u32;
    let new_h = (((pixmap.height() as f32) * dpr).round()).max(1.0) as u32;
    let mut target = Pixmap::new(new_w, new_h).ok_or_else(|| RenderError::InvalidParameters {
        message: "Failed to allocate scaled pixmap".to_string(),
    })?;

    let mut paint = PixmapPaint::default();
    paint.quality = FilterQuality::Bilinear;
    let transform = Transform::from_scale(dpr, dpr);
    target.draw_pixmap(0, 0, pixmap.as_ref(), &paint, transform, None);
    Ok(target)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::css::types::TextShadow;
    use crate::geometry::Rect;
    use crate::image_loader::ImageCache;
    use crate::paint::display_list::BorderRadii;
    use crate::style::types::{
        BackgroundAttachment, BackgroundBox, BackgroundRepeat, Isolation, MixBlendMode, OutlineColor, OutlineStyle,
        Overflow,
    };
    use crate::style::values::Length;
    use crate::style::ComputedStyle;
    use crate::text::font_loader::FontContext;
    use crate::Position;
    use crate::tree::box_tree::{SrcsetCandidate, SrcsetDescriptor};
    use std::sync::Arc;

    fn make_empty_tree() -> FragmentTree {
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![]);
        FragmentTree::new(root)
    }

    fn red_svg() -> String {
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\"><rect width=\"1\" height=\"1\" fill=\"red\"/></svg>"
            .to_string()
    }

    fn color_at(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
        let px = pixmap.pixel(x, y).expect("pixel in bounds");
        let a = px.alpha();
        if a == 0 {
            return (0, 0, 0, 0);
        }
        let r = ((px.red() as u16 * 255) / a as u16) as u8;
        let g = ((px.green() as u16 * 255) / a as u16) as u8;
        let b = ((px.blue() as u16 * 255) / a as u16) as u8;
        (r, g, b, a)
    }

    fn hue_delta(a: f32, b: f32) -> f32 {
        let d = (a - b).abs();
        d.min(1.0 - d)
    }

    fn assert_hsl_components(
        actual: (u8, u8, u8),
        expected_hsl: (f32, f32, f32),
        tol_h: f32,
        tol_s: f32,
        tol_l: f32,
        context: &str,
    ) {
        let (h, s, l) = rgb_to_hsl(
            actual.0 as f32 / 255.0,
            actual.1 as f32 / 255.0,
            actual.2 as f32 / 255.0,
        );
        assert!(
            hue_delta(h, expected_hsl.0) <= tol_h
                && (s - expected_hsl.1).abs() <= tol_s
                && (l - expected_hsl.2).abs() <= tol_l,
            "{context}: expected hsl {:?}, got hsl ({h:.3},{s:.3},{l:.3})",
            expected_hsl
        );
    }

    fn bounding_box_for_color(
        pixmap: &Pixmap,
        predicate: impl Fn((u8, u8, u8, u8)) -> bool,
    ) -> Option<(u32, u32, u32, u32)> {
        let mut min_x = u32::MAX;
        let mut min_y = u32::MAX;
        let mut max_x = 0u32;
        let mut max_y = 0u32;

        for y in 0..pixmap.height() {
            for x in 0..pixmap.width() {
                let color = color_at(pixmap, x, y);
                if predicate(color) {
                    min_x = min_x.min(x);
                    min_y = min_y.min(y);
                    max_x = max_x.max(x);
                    max_y = max_y.max(y);
                }
            }
        }

        if min_x == u32::MAX {
            None
        } else {
            Some((min_x, min_y, max_x, max_y))
        }
    }

    #[test]
    fn stacking_order_places_floats_between_blocks_and_inlines() {
        // Build four children that should paint in block → float → inline → positioned order.
        let mut block_style = ComputedStyle::default();
        block_style.display = Display::Block;
        block_style.background_color = Rgba::RED;
        let mut block = FragmentNode::new_block(Rect::from_xywh(10.0, 0.0, 5.0, 5.0), vec![]);
        block.style = Some(Arc::new(block_style));

        let mut float_style = ComputedStyle::default();
        float_style.display = Display::Block;
        float_style.float = crate::style::float::Float::Left;
        float_style.background_color = Rgba::GREEN;
        let mut float_frag = FragmentNode::new_block(Rect::from_xywh(20.0, 0.0, 5.0, 5.0), vec![]);
        float_frag.style = Some(Arc::new(float_style));

        let mut inline_style = ComputedStyle::default();
        inline_style.display = Display::Inline;
        inline_style.background_color = Rgba::BLUE;
        let mut inline = FragmentNode::new_text(Rect::from_xywh(30.0, 0.0, 5.0, 5.0), "x".to_string(), 12.0);
        inline.style = Some(Arc::new(inline_style));

        let mut positioned_style = ComputedStyle::default();
        positioned_style.display = Display::Block;
        positioned_style.position = Position::Relative;
        positioned_style.background_color = Rgba::WHITE;
        let mut positioned = FragmentNode::new_block(Rect::from_xywh(40.0, 0.0, 5.0, 5.0), vec![]);
        positioned.style = Some(Arc::new(positioned_style));

        let root = FragmentNode::new_block(
            Rect::from_xywh(0.0, 0.0, 100.0, 10.0),
            vec![block, float_frag, inline, positioned],
        );

        let painter = Painter::new(100, 10, Rgba::WHITE).expect("painter");
        let mut commands = Vec::new();
        painter.collect_stacking_context(&root, Point::ZERO, None, true, &mut commands);

        // Background commands occur in paint order; filter child backgrounds to check ordering.
        let xs: Vec<f32> = commands
            .iter()
            .filter_map(|cmd| match cmd {
                DisplayCommand::Background { rect, .. } if rect.width() == 5.0 => Some(rect.x()),
                _ => None,
            })
            .collect();

        assert_eq!(xs, vec![10.0, 20.0, 30.0, 40.0]);
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
        // tiny-skia stores premultiplied RGBA bytes
        // WHITE in RGBA is (255, 255, 255, 255)
        assert_eq!(data[0], 255); // R
        assert_eq!(data[1], 255); // G
        assert_eq!(data[2], 255); // B
        assert_eq!(data[3], 255); // A
    }

    #[test]
    fn text_decoration_metrics_available() {
        let painter = Painter::new(10, 10, Rgba::WHITE).expect("painter");
        let style = ComputedStyle::default();
        let metrics = painter.decoration_metrics(None, &style);
        assert!(metrics.is_some());
    }

    #[test]
    fn text_shadow_offsets_are_painted() {
        let mut style = ComputedStyle::default();
        style.color = Rgba::BLACK;
        style.font_size = 16.0;
        style.text_shadow = vec![TextShadow {
            offset_x: Length::px(4.0),
            offset_y: Length::px(0.0),
            blur_radius: Length::px(0.0),
            color: Rgba::from_rgba8(255, 0, 0, 255),
        }];
        let style = Arc::new(style);

        let fragment =
            FragmentNode::new_text_styled(Rect::from_xywh(10.0, 10.0, 80.0, 30.0), "Hi".to_string(), 16.0, style);
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 60.0), vec![fragment]);
        let tree = FragmentTree::new(root);

        let pixmap = paint_tree(&tree, 120, 60, Rgba::WHITE).expect("paint");

        let black_bbox =
            bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32).expect("black text");
        let red_bbox =
            bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80).expect("shadow");

        assert!(
            red_bbox.0 > black_bbox.0 + 2,
            "shadow should be offset to the right of the glyphs"
        );
        assert!(
            red_bbox.1.abs_diff(black_bbox.1) <= 2,
            "shadow should align vertically when no y offset is set"
        );
    }

    #[test]
    fn underline_offset_moves_line() {
        let mut style = ComputedStyle::default();
        style.text_decoration.lines = crate::style::types::TextDecorationLine::UNDERLINE;
        style.text_decoration.color = Some(Rgba::BLACK);
        style.font_size = 20.0;

        let painter = Painter::new(10, 10, Rgba::WHITE).expect("painter");
        let runs = painter.shaper.shape("Hi", &style, &painter.font_ctx).expect("shape");
        let metrics = painter.decoration_metrics(Some(&runs), &style).expect("metrics");
        let baseline = 20.0;
        let thickness = match style.text_decoration.thickness {
            crate::style::types::TextDecorationThickness::Length(l) => l.to_px(),
            _ => metrics.underline_thickness,
        };
        let base_center =
            baseline - painter.underline_position(&metrics, style.text_underline_position, 0.0, thickness);

        style.text_underline_offset = crate::style::types::TextUnderlineOffset::Length(Length::px(4.0));
        let shifted_center = baseline
            - painter.underline_position(
                &metrics,
                style.text_underline_position,
                painter.resolve_underline_offset(&style),
                thickness,
            );

        assert!(
            shifted_center > base_center,
            "positive offset should move underline further from the baseline"
        );
        assert!(
            (shifted_center - base_center - 4.0).abs() < 0.5,
            "underline offset should roughly follow the authored length"
        );
    }

    #[test]
    fn underline_position_under_moves_line_downward() {
        let mut style = ComputedStyle::default();
        style.text_decoration.lines = crate::style::types::TextDecorationLine::UNDERLINE;
        style.text_decoration.color = Some(Rgba::BLACK);
        style.font_size = 20.0;

        let painter = Painter::new(20, 20, Rgba::WHITE).expect("painter");
        let runs = painter.shaper.shape("Hg", &style, &painter.font_ctx).expect("shape");
        let metrics = painter.decoration_metrics(Some(&runs), &style).expect("metrics");
        let thickness = match style.text_decoration.thickness {
            crate::style::types::TextDecorationThickness::Length(l) => l.to_px(),
            _ => metrics.underline_thickness,
        };
        let baseline = 20.0;

        let auto_center =
            baseline - painter.underline_position(&metrics, style.text_underline_position, 0.0, thickness);
        style.text_underline_position = crate::style::types::TextUnderlinePosition::Under;
        let under_center =
            baseline - painter.underline_position(&metrics, style.text_underline_position, 0.0, thickness);

        assert!(
            under_center > auto_center,
            "requesting under position should place the underline below the auto position"
        );
        assert!(
            under_center - auto_center > 0.5,
            "under position should move the line a noticeable distance below the baseline"
        );
    }

    #[test]
    fn skip_ink_all_forces_exclusions_even_without_overlap() {
        let mut style = ComputedStyle::default();
        style.color = Rgba::BLACK;
        style.font_size = 24.0;
        style.text_decoration.lines = crate::style::types::TextDecorationLine::UNDERLINE;
        style.text_decoration.color = Some(Rgba::from_rgba8(255, 0, 0, 255));
        style.text_decoration_skip_ink = crate::style::types::TextDecorationSkipInk::All;
        style.text_decoration.thickness = crate::style::types::TextDecorationThickness::Length(Length::px(6.0));
        style.text_underline_offset = crate::style::types::TextUnderlineOffset::Length(Length::px(0.0));

        let painter = Painter::new(240, 140, Rgba::WHITE).expect("painter");
        let runs = painter.shaper.shape("HI HI", &style, &painter.font_ctx).expect("shape");
        let metrics = painter.decoration_metrics(Some(&runs), &style).expect("metrics");
        let width: f32 = runs.iter().map(|r| r.advance).sum();
        let baseline = 50.0;
        // Place the underline well below the glyph ink so auto skip-ink would keep it continuous.
        let center = baseline + 30.0;
        let thickness = match style.text_decoration.thickness {
            crate::style::types::TextDecorationThickness::Length(l) => l.to_px(),
            _ => metrics.underline_thickness,
        };
        let line_start = 10.0;
        let segments_all = painter.build_underline_segments(
            &runs,
            line_start,
            width,
            center,
            thickness,
            baseline,
            crate::style::types::TextDecorationSkipInk::All,
        );

        let segments_auto = painter.build_underline_segments(
            &runs,
            line_start,
            width,
            center,
            thickness,
            baseline,
            crate::style::types::TextDecorationSkipInk::Auto,
        );

        assert!(
            !segments_all.is_empty(),
            "skip-ink all should still paint around glyphs rather than dropping the line entirely"
        );
        assert_eq!(
            segments_auto.len(),
            1,
            "auto skip-ink should keep a continuous line when nothing overlaps the band"
        );
        let full_span = (line_start, line_start + width);
        let auto_span = segments_auto[0];
        assert!(
            (auto_span.0 - full_span.0).abs() < 0.01 && (auto_span.1 - full_span.1).abs() < 0.01,
            "auto span should cover the entire underline"
        );
        let carved_length: f32 = segments_all.iter().map(|(s, e)| e - s).sum();
        assert!(
            carved_length < width - 0.5,
            "skip-ink: all should carve out glyph intervals even when the underline is far from ink"
        );
    }

    #[test]
    fn underline_skip_ink_carves_descenders() {
        let mut style = ComputedStyle::default();
        style.color = Rgba::BLACK;
        style.font_size = 28.0;
        style.text_decoration.lines = crate::style::types::TextDecorationLine::UNDERLINE;
        style.text_decoration.color = Some(Rgba::from_rgba8(255, 0, 0, 255));
        style.text_decoration.thickness = crate::style::types::TextDecorationThickness::Length(Length::px(3.0));
        style.text_underline_offset = crate::style::types::TextUnderlineOffset::Length(Length::px(-1.0));

        let painter = Painter::new(160, 100, Rgba::WHITE).expect("painter");
        let runs = painter.shaper.shape("gy", &style, &painter.font_ctx).expect("shape");
        assert!(!runs.is_empty(), "shaping should yield glyphs for skip-ink evaluation");
        let metrics = painter.decoration_metrics(Some(&runs), &style).expect("metrics");
        let line_start = 10.0;
        let line_width: f32 = runs.iter().map(|r| r.advance).sum();
        let baseline = 50.0;
        let thickness = match style.text_decoration.thickness {
            crate::style::types::TextDecorationThickness::Length(l) => l.to_px(),
            _ => metrics.underline_thickness,
        };
        let center = baseline
            - painter.underline_position(
                &metrics,
                style.text_underline_position,
                painter.resolve_underline_offset(&style),
                thickness,
            );
        let exclusions = collect_underline_exclusions(
            &runs,
            line_start,
            baseline,
            center - thickness * 0.5,
            center + thickness * 0.5,
            false,
            painter.scale,
        );
        let target = exclusions
            .iter()
            .max_by(|a, b| {
                (a.1 - a.0)
                    .partial_cmp(&(b.1 - b.0))
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
            .copied()
            .expect("descenders should intersect the underline band when skip-ink is evaluated");
        let sample_x_f = (target.0 + target.1) * 0.5;
        let sample_x = sample_x_f.round().clamp(0.0, 159.0) as u32;
        let segments = painter.build_underline_segments(
            &runs,
            line_start,
            line_width,
            center,
            thickness,
            baseline,
            crate::style::types::TextDecorationSkipInk::Auto,
        );
        assert!(
            !segments.iter().any(|(s, e)| sample_x_f >= *s && sample_x_f <= *e),
            "segments should omit the exclusion area chosen for sampling"
        );
        let sample_y = center.round().clamp(0.0, 99.0) as u32;

        let baseline_offset = baseline - 10.0;
        let rect = Rect::from_xywh(line_start, 10.0, line_width, 60.0);
        let fragment_auto = FragmentNode::new_text_shaped(
            rect,
            "gy".to_string(),
            baseline_offset,
            runs.clone(),
            Arc::new(style.clone()),
        );
        let root_auto = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 160.0, 100.0), vec![fragment_auto]);
        let pix_auto = paint_tree(&FragmentTree::new(root_auto), 160, 100, Rgba::WHITE).expect("auto paint");

        let mut no_skip_style = style;
        no_skip_style.text_decoration_skip_ink = crate::style::types::TextDecorationSkipInk::None;
        let fragment_none =
            FragmentNode::new_text_shaped(rect, "gy".to_string(), baseline_offset, runs, Arc::new(no_skip_style));
        let root_none = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 160.0, 100.0), vec![fragment_none]);
        let pix_none = paint_tree(&FragmentTree::new(root_none), 160, 100, Rgba::WHITE).expect("no-skip paint");

        let auto_px = color_at(&pix_auto, sample_x, sample_y);
        let no_skip_px = color_at(&pix_none, sample_x, sample_y);

        assert!(
            no_skip_px.0 > 200 && no_skip_px.1 < 80 && no_skip_px.2 < 80,
            "a continuous underline should paint the decoration color through descenders when skip-ink is none"
        );
        assert!(
            auto_px.0 < 80 && auto_px.1 < 80 && auto_px.2 < 80,
            "skip-ink should leave the descender covered by the text color instead of the decoration color"
        );
    }

    #[test]
    fn paints_text_emphasis_marks_above_text() {
        let mut style = ComputedStyle::default();
        style.color = Rgba::BLACK;
        style.font_size = 24.0;
        style.text_emphasis_style = crate::style::types::TextEmphasisStyle::Mark {
            fill: crate::style::types::TextEmphasisFill::Filled,
            shape: crate::style::types::TextEmphasisShape::Circle,
        };
        style.text_emphasis_color = Some(Rgba::from_rgba8(255, 0, 0, 255));
        let style = Arc::new(style);

        let painter = Painter::new(80, 60, Rgba::WHITE).expect("painter");
        let runs = painter.shaper.shape("A", &style, &painter.font_ctx).expect("shape");
        let width: f32 = runs.iter().map(|r| r.advance).sum();
        let baseline = 32.0;
        let rect = Rect::from_xywh(10.0, 8.0, width + 2.0, 40.0);
        let fragment = FragmentNode::new_text_shaped(rect, "A".to_string(), baseline - rect.y(), runs, style);
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 80.0, 60.0), vec![fragment]);
        let tree = FragmentTree::new(root);

        let pixmap = paint_tree(&tree, 80, 60, Rgba::WHITE).expect("paint");

        let black_bbox =
            bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32).expect("text");
        let red_bbox = bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80)
            .expect("emphasis mark");

        assert!(
            red_bbox.1 < black_bbox.1,
            "emphasis mark should appear above the glyphs when positioned over the text"
        );
    }

    #[test]
    fn replaced_content_respects_padding_box() {
        let mut style = ComputedStyle::default();
        style.padding_left = Length::px(4.0);
        style.padding_right = Length::px(4.0);
        style.padding_top = Length::px(4.0);
        style.padding_bottom = Length::px(4.0);
        style.background_color = Rgba::BLUE;

        let mut painter =
            Painter::with_resources(40, 40, Rgba::WHITE, FontContext::new(), ImageCache::new()).expect("painter");
        painter.fill_background();

        let box_x = 10.0;
        let box_y = 10.0;
        let box_size = 20.0;
        let rects = background_rects(box_x, box_y, box_size, box_size, &style);
        assert!((rects.content.x() - 14.0).abs() < 0.01);
        assert!((rects.content.y() - 14.0).abs() < 0.01);
        assert!((rects.content.width() - 12.0).abs() < 0.01);
        assert!((rects.content.height() - 12.0).abs() < 0.01);
        painter.paint_background(box_x, box_y, box_size, box_size, &style);
        painter.paint_replaced(
            &ReplacedType::Svg { content: red_svg() },
            Some(&style),
            box_x,
            box_y,
            box_size,
            box_size,
        );

        let pixmap = painter.pixmap;
        assert_eq!(color_at(&pixmap, 11, 11), (0, 0, 255, 255));
        assert_eq!(color_at(&pixmap, 15, 15), (255, 0, 0, 255));
        assert_eq!(color_at(&pixmap, 20, 20), (255, 0, 0, 255));
        assert_eq!(color_at(&pixmap, 27, 27), (0, 0, 255, 255));
    }

    #[test]
    fn paints_alt_text_when_image_missing() {
        let mut style = ComputedStyle::default();
        style.color = Rgba::BLACK;

        let fragment = FragmentNode::new_with_style(
            Rect::from_xywh(0.0, 0.0, 50.0, 20.0),
            FragmentContent::Replaced {
                replaced_type: ReplacedType::Image {
                    src: String::new(),
                    alt: Some("alt".to_string()),
                    srcset: Vec::new(),
                },
                box_id: None,
            },
            vec![],
            Arc::new(style),
        );

        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 60.0, 30.0), vec![fragment]);
        let tree = FragmentTree::new(root);
        let pixmap = paint_tree(&tree, 60, 30, Rgba::WHITE).expect("paint alt");

        let center = color_at(&pixmap, 25, 10);
        assert_ne!(
            center,
            (200, 200, 200, 255),
            "alt text should prevent placeholder rectangles"
        );

        let mut has_ink = false;
        for y in 0..pixmap.height() {
            for x in 0..pixmap.width() {
                if color_at(&pixmap, x, y) != (255, 255, 255, 255) {
                    has_ink = true;
                    break;
                }
            }
            if has_ink {
                break;
            }
        }
        assert!(has_ink, "alt text should paint glyphs");
    }

    #[test]
    fn paints_embed_svg_content() {
        let svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"2\" height=\"2\"><rect width=\"2\" height=\"2\" fill=\"red\"/></svg>";
        let style = Arc::new(ComputedStyle::default());
        let fragment = FragmentNode::new_with_style(
            Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
            FragmentContent::Replaced {
                replaced_type: ReplacedType::Embed { src: svg.to_string() },
                box_id: None,
            },
            vec![],
            style,
        );
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![fragment]);
        let tree = FragmentTree::new(root);

        let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint embed");
        assert_eq!(
            color_at(&pixmap, 10, 10),
            (255, 0, 0, 255),
            "embed should render svg content"
        );
    }

    #[test]
    fn paints_video_poster_image_content() {
        let poster =
            "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"8\" height=\"8\"><rect width=\"8\" height=\"8\" fill=\"lime\"/></svg>";
        let style = Arc::new(ComputedStyle::default());
        let fragment = FragmentNode::new_with_style(
            Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
            FragmentContent::Replaced {
                replaced_type: ReplacedType::Video {
                    src: String::new(),
                    poster: Some(poster.to_string()),
                },
                box_id: None,
            },
            vec![],
            style,
        );
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 12.0, 12.0), vec![fragment]);
        let tree = FragmentTree::new(root);

        let pixmap = paint_tree(&tree, 12, 12, Rgba::WHITE).expect("paint video poster");
        assert_eq!(
            color_at(&pixmap, 5, 5),
            (0, 255, 0, 255),
            "poster content should paint instead of placeholder"
        );
    }

    fn svg_data_url(color: &str) -> String {
        format!(
            "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' width='1' height='1'><rect width='1' height='1' fill='{color}'/></svg>"
        )
    }

    #[test]
    fn srcset_chooses_best_density_for_device_scale() {
        let red = svg_data_url("red");
        let blue = svg_data_url("blue");

        let replaced = ReplacedType::Image {
            src: red.clone(),
            alt: None,
            srcset: vec![
                SrcsetCandidate {
                    url: red.clone(),
                    descriptor: SrcsetDescriptor::Density(1.0),
                },
                SrcsetCandidate {
                    url: blue.clone(),
                    descriptor: SrcsetDescriptor::Density(2.0),
                },
            ],
        };

        let style = ComputedStyle::default();
        let mut painter = Painter::with_resources_scaled(
            2,
            2,
            Rgba::WHITE,
            FontContext::new(),
            ImageCache::new(),
            2.0,
        )
        .expect("painter");
        painter.paint_replaced(&replaced, Some(&style), 0.0, 0.0, 1.0, 1.0);

        let px = painter.pixmap.pixel(0, 0).unwrap();
        assert_eq!(
            (px.red(), px.green(), px.blue()),
            (0, 0, 255),
            "2x density should pick the blue candidate at scale 2.0"
        );
    }

    #[test]
    fn srcset_width_descriptor_uses_slot_width() {
        let red = svg_data_url("red");
        let blue = svg_data_url("blue");

        let replaced = ReplacedType::Image {
            src: red.clone(),
            alt: None,
            srcset: vec![
                SrcsetCandidate {
                    url: red.clone(),
                    descriptor: SrcsetDescriptor::Width(100),
                },
                SrcsetCandidate {
                    url: blue.clone(),
                    descriptor: SrcsetDescriptor::Width(300),
                },
            ],
        };

        let style = ComputedStyle::default();
        let mut painter = Painter::with_resources_scaled(
            120,
            20,
            Rgba::WHITE,
            FontContext::new(),
            ImageCache::new(),
            2.0,
        )
        .expect("painter");

        painter.paint_replaced(&replaced, Some(&style), 0.0, 0.0, 100.0, 10.0);

        let px = painter.pixmap.pixel(100, 10).unwrap();
        assert_eq!(
            (px.red(), px.green(), px.blue()),
            (0, 0, 255),
            "with 100px slot at DPR=2, the 300w candidate (density 3) should be chosen"
        );
    }

    #[test]
    fn paints_linear_gradient_background() {
        let mut style = ComputedStyle::default();
        style.set_background_layers(vec![BackgroundLayer {
            image: Some(BackgroundImage::LinearGradient {
                angle: 90.0,
                stops: vec![
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::RED),
                        position: Some(0.0),
                    },
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::BLUE),
                        position: Some(1.0),
                    },
                ],
            }),
            ..BackgroundLayer::default()
        }]);

        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![], Arc::new(style));
        let tree = FragmentTree::new(fragment);

        let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");
        let left = color_at(&pixmap, 2, 10);
        let right = color_at(&pixmap, 18, 10);
        assert!(left.0 > right.0, "left should be redder than right");
        assert!(right.2 > left.2, "right should be bluer than left");
    }

    #[test]
    fn normalize_color_stops_resolves_current_color() {
        let stops = vec![
            crate::css::types::ColorStop {
                color: Color::CurrentColor,
                position: Some(0.0),
            },
            crate::css::types::ColorStop {
                color: Color::Rgba(Rgba::BLUE),
                position: Some(1.0),
            },
        ];
        let resolved = normalize_color_stops(&stops, Rgba::new(10, 20, 30, 1.0));
        assert_eq!(resolved.len(), 2);
        assert_eq!(resolved[0].1, Rgba::new(10, 20, 30, 1.0));
        assert_eq!(resolved[1].1, Rgba::BLUE);
    }

    #[test]
    fn paints_repeating_linear_gradient_background() {
        let mut style = ComputedStyle::default();
        style.set_background_layers(vec![BackgroundLayer {
            image: Some(BackgroundImage::RepeatingLinearGradient {
                angle: 0.0,
                stops: vec![
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::RED),
                        position: Some(0.0),
                    },
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::BLUE),
                        position: Some(0.5),
                    },
                ],
            }),
            ..BackgroundLayer::default()
        }]);

        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![], Arc::new(style));
        let tree = FragmentTree::new(fragment);
        let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");

        let top = color_at(&pixmap, 10, 2);
        let middle = color_at(&pixmap, 10, 10);
        let bottom = color_at(&pixmap, 10, 18);

        // Repeating stripes: samples at different rows should not all match; require at least two distinct colors.
        assert!(top != middle || middle != bottom);
        let mut distinct = std::collections::HashSet::new();
        distinct.insert(top);
        distinct.insert(middle);
        distinct.insert(bottom);
        assert!(
            distinct.len() >= 2,
            "expected at least two colors in repeating gradient"
        );
    }

    #[test]
    fn background_attachment_fixed_anchors_to_viewport() {
        let mut style = ComputedStyle::default();
        style.set_background_layers(vec![BackgroundLayer {
            image: Some(BackgroundImage::LinearGradient {
                angle: 90.0,
                stops: vec![
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::RED),
                        position: Some(0.0),
                    },
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::BLUE),
                        position: Some(1.0),
                    },
                ],
            }),
            attachment: BackgroundAttachment::Fixed,
            ..BackgroundLayer::default()
        }]);

        // Gradient anchors to viewport: samples at successive x positions diverge even though elements have their own origins.
        let style_arc = Arc::new(style);
        let first = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 1.0, 1.0), vec![], style_arc.clone());
        let second = FragmentNode::new_block_styled(Rect::from_xywh(1.0, 0.0, 1.0, 1.0), vec![], style_arc.clone());
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 4.0, 2.0), vec![first, second]);
        let tree = FragmentTree::new(root);
        let pixmap = paint_tree(&tree, 4, 2, Rgba::WHITE).expect("paint");

        let left = color_at(&pixmap, 0, 0);
        let right = color_at(&pixmap, 1, 0);
        assert!(
            left.0 > right.0,
            "fixed attachment should keep gradient anchored to viewport"
        );
        assert!(right.2 > left.2);
    }

    #[test]
    fn background_attachment_local_uses_scrollable_overflow_area() {
        let mut style = ComputedStyle::default();
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
            attachment: BackgroundAttachment::Local,
            ..BackgroundLayer::default()
        }]);
        style.overflow_x = Overflow::Scroll;
        style.overflow_y = Overflow::Scroll;
        style.border_top_width = Length::px(2.0);
        style.border_right_width = Length::px(2.0);
        style.border_bottom_width = Length::px(2.0);
        style.border_left_width = Length::px(2.0);
        style.border_top_style = CssBorderStyle::Solid;
        style.border_right_style = CssBorderStyle::Solid;
        style.border_bottom_style = CssBorderStyle::Solid;
        style.border_left_style = CssBorderStyle::Solid;
        style.border_top_color = Rgba::TRANSPARENT;
        style.border_right_color = Rgba::TRANSPARENT;
        style.border_bottom_color = Rgba::TRANSPARENT;
        style.border_left_color = Rgba::TRANSPARENT;

        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 12.0, 12.0), vec![], Arc::new(style));
        let tree = FragmentTree::new(fragment);
        let pixmap = paint_tree(&tree, 12, 12, Rgba::WHITE).expect("paint");

        // Border-box samples should stay transparent because local attachment anchors to the scrollable overflow area
        // (padding box) and border-box clipping collapses to padding-box per CSS Backgrounds 3.
        assert_eq!(color_at(&pixmap, 1, 1), (255, 255, 255, 255));
        // Padding box should still paint the background image.
        assert_eq!(color_at(&pixmap, 6, 6), (255, 0, 0, 255));
    }

    #[test]
    fn background_color_uses_first_layer_clip_value() {
        let mut style = ComputedStyle::default();
        style.background_color = Rgba::RED;
        style.background_images = vec![None, None];
        style.background_clips = vec![BackgroundBox::PaddingBox, BackgroundBox::BorderBox];
        style.rebuild_background_layers();
        style.border_top_width = Length::px(2.0);
        style.border_right_width = Length::px(2.0);
        style.border_bottom_width = Length::px(2.0);
        style.border_left_width = Length::px(2.0);
        style.border_top_style = CssBorderStyle::Solid;
        style.border_right_style = CssBorderStyle::Solid;
        style.border_bottom_style = CssBorderStyle::Solid;
        style.border_left_style = CssBorderStyle::Solid;
        style.border_top_color = Rgba::WHITE;
        style.border_right_color = Rgba::WHITE;
        style.border_bottom_color = Rgba::WHITE;
        style.border_left_color = Rgba::WHITE;

        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 12.0, 12.0), vec![], Arc::new(style));
        let tree = FragmentTree::new(fragment);
        let pixmap = paint_tree(&tree, 12, 12, Rgba::WHITE).expect("paint");

        // Color should be clipped to the first layer's clip (padding-box), leaving border white.
        assert_eq!(color_at(&pixmap, 1, 1), (255, 255, 255, 255));
        assert_eq!(color_at(&pixmap, 3, 3), (255, 0, 0, 255));
    }

    #[test]
    fn overflow_hidden_clips_children() {
        let mut parent_style = ComputedStyle::default();
        parent_style.overflow_x = Overflow::Hidden;
        parent_style.overflow_y = Overflow::Visible;
        parent_style.position = Position::Relative;
        parent_style.background_color = Rgba::BLUE;
        let parent_style = Arc::new(parent_style);

        let mut child_style = ComputedStyle::default();
        child_style.background_color = Rgba::RED;
        let child =
            FragmentNode::new_block_styled(Rect::from_xywh(-5.0, -5.0, 30.0, 40.0), vec![], Arc::new(child_style));
        let parent = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![child], parent_style);
        let tree = FragmentTree::new(parent);

        let pixmap = paint_tree(&tree, 40, 40, Rgba::WHITE).expect("paint");
        assert_eq!(color_at(&pixmap, 10, 10), (255, 0, 0, 255));
        // Horizontal overflow should clip; vertical overflow should remain visible.
        assert_eq!(color_at(&pixmap, 22, 2), (255, 255, 255, 255));
        assert_eq!(color_at(&pixmap, 10, 25), (255, 0, 0, 255));
    }

    #[test]
    fn overflow_y_hidden_clips_vertical_only() {
        let mut parent_style = ComputedStyle::default();
        parent_style.overflow_x = Overflow::Visible;
        parent_style.overflow_y = Overflow::Hidden;
        parent_style.position = Position::Relative;
        parent_style.background_color = Rgba::BLUE;
        let parent_style = Arc::new(parent_style);

        let mut child_style = ComputedStyle::default();
        child_style.background_color = Rgba::RED;
        let child =
            FragmentNode::new_block_styled(Rect::from_xywh(-5.0, -5.0, 30.0, 40.0), vec![], Arc::new(child_style));
        let parent = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![child], parent_style);
        let tree = FragmentTree::new(parent);

        let pixmap = paint_tree(&tree, 40, 40, Rgba::WHITE).expect("paint");
        // Vertical overflow clipped; horizontal overflow visible.
        assert_eq!(color_at(&pixmap, 10, 10), (255, 0, 0, 255));
        assert_eq!(color_at(&pixmap, 10, 25), (255, 255, 255, 255));
        assert_eq!(color_at(&pixmap, 22, 10), (255, 0, 0, 255));
    }

    #[test]
    fn overflow_clip_limits_layer_bounds() {
        let style = Arc::new(ComputedStyle::default());
        let root_rect = Rect::from_xywh(0.0, 0.0, 20.0, 20.0);
        let commands = vec![
            DisplayCommand::Background {
                rect: root_rect,
                style: style.clone(),
            },
            DisplayCommand::Background {
                rect: Rect::from_xywh(1000.0, 0.0, 10.0, 10.0),
                style,
            },
        ];
        let clip = Some((root_rect, BorderRadii::ZERO, true, true));
        let bounds = stacking_context_bounds(&commands, &[], &[], root_rect, None, clip.as_ref()).expect("bounds");
        assert!((bounds.width() - 1010.0).abs() < 0.01);
        assert!(bounds.min_x().abs() < 0.01);
        assert!((bounds.max_x() - 1010.0).abs() < 0.01);
    }

    #[test]
    fn transformed_child_extends_parent_bounds() {
        let mut parent_style = ComputedStyle::default();
        parent_style.isolation = Isolation::Isolate;
        let parent_style = Arc::new(parent_style);

        let mut child_style = ComputedStyle::default();
        child_style.background_color = Rgba::RED;
        child_style.transform = vec![crate::css::types::Transform::Translate(
            Length::px(60.0),
            Length::px(0.0),
        )];
        let child_style = Arc::new(child_style);

        let child = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![], child_style);
        let parent = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 1.0, 1.0), vec![child], parent_style);
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 40.0), vec![parent]);
        let tree = FragmentTree::new(root);

        let pixmap = paint_tree(&tree, 120, 40, Rgba::WHITE).expect("paint");
        // The translated child should remain visible even though the parent stacking context bounds
        // are derived from its untransformed content.
        assert_eq!(color_at(&pixmap, 70, 10), (255, 0, 0, 255));
        assert_eq!(color_at(&pixmap, 5, 5), (255, 255, 255, 255));
    }

    #[test]
    fn object_fit_contain_centers_image() {
        let fit = ObjectFit::Contain;
        let position = crate::style::types::ObjectPosition {
            x: crate::style::types::PositionComponent::Keyword(crate::style::types::PositionKeyword::Center),
            y: crate::style::types::PositionComponent::Keyword(crate::style::types::PositionKeyword::Center),
        };

        let (offset_x, offset_y, dest_w, dest_h) =
            compute_object_fit(fit, position, 200.0, 100.0, 100.0, 100.0, 16.0, Some((200.0, 100.0)))
                .expect("fit computed");
        assert_eq!(dest_h, 100.0);
        assert_eq!(dest_w, 100.0);
        assert!((offset_x - 50.0).abs() < 0.01);
        assert!((offset_y - 0.0).abs() < 0.01);
    }

    #[test]
    fn background_cover_scales_to_fill() {
        let mut layer = BackgroundLayer::default();
        layer.size = BackgroundSize::Keyword(BackgroundSizeKeyword::Cover);
        let (tw, th) = compute_background_size(&layer, 16.0, 200.0, 100.0, 50.0, 50.0);
        let (ox, oy) = resolve_background_offset(layer.position, 200.0, 100.0, tw, th, 16.0);
        assert!((tw - 200.0).abs() < 0.01);
        assert!((th - 200.0).abs() < 0.01);
        assert!((ox - 0.0).abs() < 0.01);
        assert!((oy - 0.0).abs() < 0.01);
    }

    #[test]
    fn background_position_alignment_and_offsets_resolve_against_available_space() {
        let layer = BackgroundLayer {
            position: BackgroundPosition::Position {
                x: crate::style::types::BackgroundPositionComponent {
                    alignment: 1.0,
                    offset: Length::px(-10.0),
                },
                y: crate::style::types::BackgroundPositionComponent {
                    alignment: 1.0,
                    offset: Length::percent(-20.0),
                },
            },
            ..BackgroundLayer::default()
        };

        let (ox, oy) = resolve_background_offset(layer.position, 100.0, 60.0, 20.0, 10.0, 16.0);
        // available_x = 80; 1 * 80 - 10 = 70
        // available_y = 50; 1 * 50 - 20%*50 = 40
        assert!((ox - 70.0).abs() < 0.01);
        assert!((oy - 40.0).abs() < 0.01);
    }

    #[test]
    fn background_size_single_dimension_auto_uses_intrinsic_ratio() {
        let mut layer = BackgroundLayer::default();
        layer.size = BackgroundSize::Explicit(
            BackgroundSizeComponent::Auto,
            BackgroundSizeComponent::Length(Length::px(25.0)),
        );
        let (tw, th) = compute_background_size(&layer, 16.0, 200.0, 100.0, 100.0, 50.0);
        assert!((tw - 50.0).abs() < 0.01);
        assert!((th - 25.0).abs() < 0.01);
    }

    #[test]
    fn background_size_auto_auto_uses_intrinsic_size_or_falls_back() {
        let layer = BackgroundLayer::default();
        let (tw, th) = compute_background_size(&layer, 16.0, 120.0, 80.0, 30.0, 10.0);
        assert!((tw - 30.0).abs() < 0.01);
        assert!((th - 10.0).abs() < 0.01);

        let (tw, th) = compute_background_size(&layer, 16.0, 50.0, 60.0, 0.0, 0.0);
        assert!((tw - 50.0).abs() < 0.01);
        assert!((th - 60.0).abs() < 0.01);
    }

    #[test]
    fn background_repeat_space_distributes_evenly() {
        let positions = tile_positions(BackgroundRepeatKeyword::Space, 0.0, 100.0, 30.0, 0.0, 0.0, 100.0);
        assert_eq!(positions.len(), 3);
        assert!((positions[0] - 0.0).abs() < 1e-4);
        assert!((positions[1] - 35.0).abs() < 1e-3);
        assert!((positions[2] - 70.0).abs() < 1e-3);
    }

    #[test]
    fn background_repeat_space_centers_single_tile() {
        let positions = tile_positions(BackgroundRepeatKeyword::Space, 10.0, 40.0, 30.0, 0.0, 0.0, 100.0);
        assert_eq!(positions, vec![15.0]); // 10 + (40-30)/2

        let with_offset = tile_positions(BackgroundRepeatKeyword::Space, 0.0, 40.0, 30.0, 5.0, 0.0, 100.0);
        assert_eq!(with_offset, vec![10.0]); // centered (5) plus offset (5)
    }

    #[test]
    fn background_repeat_space_centers_oversized_tile() {
        // Tile larger than area => still center it.
        let positions = tile_positions(BackgroundRepeatKeyword::Space, 0.0, 20.0, 30.0, 0.0, 0.0, 100.0);
        assert_eq!(positions, vec![-5.0]); // (20-30)/2
    }

    #[test]
    fn background_repeat_round_resizes_to_integer_tiles() {
        let rounded = round_tile_length(1099.0, 100.0);
        assert!((rounded - (1099.0 / 11.0)).abs() < 1e-3);
    }

    #[test]
    fn background_blend_mode_multiplies_layers() {
        let make_style = |blend_mode| {
            let mut style = ComputedStyle::default();
            style.background_color = Rgba::BLUE;
            style.set_background_layers(vec![BackgroundLayer {
                image: Some(BackgroundImage::LinearGradient {
                    angle: 0.0,
                    stops: vec![
                        crate::css::types::ColorStop {
                            color: Color::Rgba(Rgba::new(255, 255, 0, 1.0)),
                            position: Some(0.0),
                        },
                        crate::css::types::ColorStop {
                            color: Color::Rgba(Rgba::new(255, 255, 0, 1.0)),
                            position: Some(1.0),
                        },
                    ],
                }),
                repeat: BackgroundRepeat::no_repeat(),
                blend_mode,
                ..BackgroundLayer::default()
            }]);
            style
        };

        let normal_fragment = FragmentNode::new_block_styled(
            Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
            vec![],
            Arc::new(make_style(MixBlendMode::Normal)),
        );
        let multiply_fragment = FragmentNode::new_block_styled(
            Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
            vec![],
            Arc::new(make_style(MixBlendMode::Multiply)),
        );

        let normal = paint_tree(&FragmentTree::new(normal_fragment), 10, 10, Rgba::WHITE).expect("paint");
        let multiplied =
            paint_tree(&FragmentTree::new(multiply_fragment), 10, 10, Rgba::WHITE).expect("paint multiply");

        assert_eq!(color_at(&normal, 5, 5), (255, 255, 0, 255));
        let blended = color_at(&multiplied, 5, 5);
        assert!(
            blended.0 < 5 && blended.1 < 5 && blended.2 < 5,
            "multiply blend should blacken blue + yellow, got {:?}",
            blended
        );
    }

    #[test]
    fn mix_blend_mode_hue_preserves_hsl_components() {
        let dst = (30u8, 120u8, 220u8);
        let src = (200u8, 30u8, 30u8);

        let mut root_style = ComputedStyle::default();
        root_style.background_color = Rgba::from_rgba8(dst.0, dst.1, dst.2, 255);

        let mut child_style = ComputedStyle::default();
        child_style.background_color = Rgba::from_rgba8(src.0, src.1, src.2, 255);
        child_style.mix_blend_mode = MixBlendMode::Hue;

        let child = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 2.0, 2.0), vec![], Arc::new(child_style));
        let mut root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 2.0, 2.0), vec![child]);
        root.style = Some(Arc::new(root_style));

        let pixmap = paint_tree(&FragmentTree::new(root), 2, 2, Rgba::WHITE).expect("paint");
        let (r, g, b, _) = color_at(&pixmap, 0, 0);

        let expected = apply_hsl_blend(
            MixBlendMode::Hue,
            (src.0 as f32 / 255.0, src.1 as f32 / 255.0, src.2 as f32 / 255.0),
            (dst.0 as f32 / 255.0, dst.1 as f32 / 255.0, dst.2 as f32 / 255.0),
        );
        let expected_hsl = rgb_to_hsl(expected.0, expected.1, expected.2);
        assert_hsl_components((r, g, b), expected_hsl, 0.02, 0.05, 0.05, "hue mix-blend-mode");
    }

    #[test]
    fn background_blend_mode_color_preserves_luminance() {
        let dst = (30u8, 120u8, 220u8);
        let src = (200u8, 30u8, 30u8);

        let mut style = ComputedStyle::default();
        style.background_color = Rgba::from_rgba8(dst.0, dst.1, dst.2, 255);
        style.set_background_layers(vec![BackgroundLayer {
            image: Some(BackgroundImage::LinearGradient {
                angle: 0.0,
                stops: vec![
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::from_rgba8(src.0, src.1, src.2, 255)),
                        position: Some(0.0),
                    },
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::from_rgba8(src.0, src.1, src.2, 255)),
                        position: Some(1.0),
                    },
                ],
            }),
            repeat: BackgroundRepeat::no_repeat(),
            blend_mode: MixBlendMode::Color,
            ..BackgroundLayer::default()
        }]);

        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 2.0, 2.0), vec![], Arc::new(style));
        let pixmap = paint_tree(&FragmentTree::new(fragment), 2, 2, Rgba::WHITE).expect("paint");
        let (r, g, b, _) = color_at(&pixmap, 0, 0);

        let expected = apply_hsl_blend(
            MixBlendMode::Color,
            (src.0 as f32 / 255.0, src.1 as f32 / 255.0, src.2 as f32 / 255.0),
            (dst.0 as f32 / 255.0, dst.1 as f32 / 255.0, dst.2 as f32 / 255.0),
        );
        let expected_hsl = rgb_to_hsl(expected.0, expected.1, expected.2);
        assert_hsl_components((r, g, b), expected_hsl, 0.02, 0.05, 0.05, "background color blend");
    }

    #[test]
    fn background_layers_paint_top_to_bottom() {
        let mut style = ComputedStyle::default();

        let top = BackgroundLayer {
            image: Some(BackgroundImage::LinearGradient {
                angle: 0.0,
                stops: vec![
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::from_rgba8(0, 255, 0, 128)),
                        position: Some(0.0),
                    },
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::from_rgba8(0, 255, 0, 128)),
                        position: Some(1.0),
                    },
                ],
            }),
            ..BackgroundLayer::default()
        };
        let bottom = BackgroundLayer {
            image: Some(BackgroundImage::LinearGradient {
                angle: 0.0,
                stops: vec![
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::BLUE),
                        position: Some(0.0),
                    },
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::BLUE),
                        position: Some(1.0),
                    },
                ],
            }),
            ..BackgroundLayer::default()
        };
        style.set_background_layers(vec![top, bottom]);

        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], Arc::new(style));
        let tree = FragmentTree::new(fragment);
        let pixmap = paint_tree(&tree, 10, 10, Rgba::WHITE).expect("paint");

        let center = color_at(&pixmap, 5, 5);
        // Top (semi-transparent green) over opaque blue yields roughly 50/50 mix.
        assert!(
            center.1 > 110 && center.1 < 150 && center.2 > 110 && center.2 < 150 && center.0 == 0,
            "expected blended green-over-blue, got {:?}",
            center
        );
    }

    #[test]
    fn background_layers_use_per_layer_clips() {
        let mut style = ComputedStyle::default();
        style.padding_left = Length::px(4.0);
        style.padding_right = Length::px(4.0);
        style.padding_top = Length::px(4.0);
        style.padding_bottom = Length::px(4.0);

        let top = BackgroundLayer {
            image: Some(BackgroundImage::LinearGradient {
                angle: 0.0,
                stops: vec![
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::GREEN),
                        position: Some(0.0),
                    },
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::GREEN),
                        position: Some(1.0),
                    },
                ],
            }),
            clip: crate::style::types::BackgroundBox::ContentBox,
            ..BackgroundLayer::default()
        };
        let bottom = BackgroundLayer {
            image: Some(BackgroundImage::LinearGradient {
                angle: 0.0,
                stops: vec![
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::BLUE),
                        position: Some(0.0),
                    },
                    crate::css::types::ColorStop {
                        color: Color::Rgba(Rgba::BLUE),
                        position: Some(1.0),
                    },
                ],
            }),
            clip: crate::style::types::BackgroundBox::BorderBox,
            ..BackgroundLayer::default()
        };
        style.set_background_layers(vec![top, bottom]);

        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![], Arc::new(style));
        let tree = FragmentTree::new(fragment);
        let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");

        // Padding area should see only the border-box layer (blue).
        let padding_px = color_at(&pixmap, 1, 1);
        assert!(
            padding_px.2 > 200 && padding_px.1 < 50,
            "expected blue in padding area, got {:?}",
            padding_px
        );

        // Content area should be covered by the content-clipped top layer (green).
        let content_px = color_at(&pixmap, 10, 10);
        assert!(
            content_px.1 > 200 && content_px.2 < 50,
            "expected green in content area, got {:?}",
            content_px
        );
    }

    #[test]
    fn outline_draws_outside_box() {
        let mut style = ComputedStyle::default();
        style.background_color = Rgba::BLUE;
        style.outline_style = OutlineStyle::Solid;
        style.outline_width = Length::px(4.0);
        style.outline_color = OutlineColor::Color(Rgba::RED);
        style.outline_offset = Length::px(2.0);
        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(4.0, 4.0, 10.0, 10.0), vec![], Arc::new(style));
        let tree = FragmentTree::new(fragment);

        let pixmap = paint_tree(&tree, 30, 30, Rgba::WHITE).expect("paint");
        assert_eq!(color_at(&pixmap, 5, 5), (0, 0, 255, 255));
        let mut found_outline = false;
        for y in 0..5 {
            for x in 0..10 {
                let px = color_at(&pixmap, x, y);
                if px != (255, 255, 255, 255) {
                    found_outline = true;
                }
            }
        }
        assert!(found_outline, "outline stroke should paint outside the box");
        assert_eq!(color_at(&pixmap, 0, 0), (255, 255, 255, 255));
    }

    #[test]
    fn filters_expand_clip_under_radii() {
        let mut painter = Painter::with_resources_scaled(
            4,
            4,
            Rgba::TRANSPARENT,
            FontContext::new(),
            ImageCache::new(),
            1.0,
        )
        .unwrap();
        painter.fill_background();

        let mut style = ComputedStyle::default();
        style.background_color = Rgba::RED;
        let style = Arc::new(style);

        let cmd = DisplayCommand::StackingContext {
            rect: Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
            opacity: 1.0,
            transform: None,
            blend_mode: MixBlendMode::Normal,
            isolated: true,
            filters: vec![ResolvedFilter::Blur(1.0)],
            backdrop_filters: Vec::new(),
            radii: BorderRadii::uniform(0.5),
            clip: None,
            commands: vec![DisplayCommand::Background {
                rect: Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
                style,
            }],
        };

        painter.execute_command(cmd).unwrap();
        let alpha_near = painter.pixmap.pixel(0, 1).unwrap().alpha();
        assert!(
            alpha_near > 0,
            "expected blur spill outside rounded rect; alpha at (0,1) was {alpha_near}"
        );
        assert!(
            alpha_near < 255,
            "blurred spill should be softer than solid fill; alpha at (0,1) was {alpha_near}"
        );
    }

    #[test]
    fn outline_not_clipped_by_overflow_hidden() {
        let mut style = ComputedStyle::default();
        style.background_color = Rgba::WHITE;
        style.outline_style = OutlineStyle::Solid;
        style.outline_width = Length::px(4.0);
        style.outline_color = OutlineColor::Color(Rgba::RED);
        style.overflow_x = Overflow::Hidden;
        style.overflow_y = Overflow::Hidden;
        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(10.0, 10.0, 10.0, 10.0), vec![], Arc::new(style));
        let tree = FragmentTree::new(fragment);

        let pixmap = paint_tree(&tree, 40, 40, Rgba::WHITE).expect("paint");
        // Outline should extend beyond the 10..20 box even though overflow is hidden.
        assert_eq!(color_at(&pixmap, 8, 15), (255, 0, 0, 255));
    }

    #[test]
    fn visibility_hidden_prevents_painting() {
        let mut style = ComputedStyle::default();
        style.visibility = crate::style::computed::Visibility::Hidden;
        style.background_color = Rgba::RED;
        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], Arc::new(style));
        let tree = FragmentTree::new(fragment);

        let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");
        assert_eq!(color_at(&pixmap, 5, 5), (255, 255, 255, 255));
    }

    #[test]
    fn visibility_collapse_prevents_painting() {
        let mut style = ComputedStyle::default();
        style.visibility = crate::style::computed::Visibility::Collapse;
        style.background_color = Rgba::BLUE;
        let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], Arc::new(style));
        let tree = FragmentTree::new(fragment);

        let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");
        assert_eq!(color_at(&pixmap, 5, 5), (255, 255, 255, 255));
    }
}
