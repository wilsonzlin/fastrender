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
use crate::geometry::{Point, Rect};
use crate::image_loader::ImageCache;
use crate::paint::stacking::creates_stacking_context;
use crate::style::color::Rgba;
use crate::style::types::MixBlendMode;
use crate::style::types::{
    BackgroundImage, BackgroundPosition, BackgroundRepeat, BackgroundSize, BorderStyle as CssBorderStyle, ObjectFit,
    ObjectPosition, PositionComponent,
};
use crate::style::values::{Length, LengthUnit};
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::text::pipeline::{ShapedRun, ShapingPipeline};
use crate::tree::box_tree::ReplacedType;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use image::DynamicImage;
use std::borrow::Cow;
use std::sync::Arc;
use tiny_skia::{
    BlendMode as SkiaBlendMode, FilterQuality, IntSize, Paint, PathBuilder, Pattern, Pixmap, PixmapPaint,
    Rect as SkiaRect, SpreadMode, Stroke, Transform,
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
    StackingContext {
        opacity: f32,
        transform: Option<Transform>,
        blend_mode: MixBlendMode,
        isolated: bool,
        commands: Vec<DisplayCommand>,
    },
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
            for child in &fragment.children {
                self.collect_stacking_context(child, next_offset, style_ref, false, &mut local_commands);
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
        let mut normal_children = Vec::new();

        for (idx, child) in fragment.children.iter().enumerate() {
            if let Some(style) = child.style.as_deref() {
                if creates_stacking_context(style, style_ref, false) {
                    let z = style.z_index;
                    if z < 0 {
                        negative_contexts.push((z, idx));
                    } else if z == 0 {
                        zero_contexts.push((z, idx));
                    } else {
                        positive_contexts.push((z, idx));
                    }
                    continue;
                }
            }
            normal_children.push(idx);
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
        for idx in normal_children {
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
        if opacity < 1.0 || transform.is_some() || !matches!(blend_mode, MixBlendMode::Normal) || isolated {
            items.push(DisplayCommand::StackingContext {
                opacity,
                transform,
                blend_mode,
                isolated,
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

        let has_background = style.background_color.alpha_u8() > 0 || style.background_image.is_some();
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
                self.paint_background_image(rect.x(), rect.y(), rect.width(), rect.height(), &style);
            }
            DisplayCommand::Border { rect, style } => {
                self.paint_borders(rect.x(), rect.y(), rect.width(), rect.height(), &style);
            }
            DisplayCommand::Text {
                rect,
                baseline_offset,
                text,
                runs,
                style,
            } => {
                let color = style.color;
                if let Some(runs) = runs {
                    self.paint_shaped_runs(&runs, rect.x(), rect.y() + baseline_offset, color);
                } else {
                    self.paint_text(
                        &text,
                        Some(&style),
                        rect.x(),
                        rect.y() + baseline_offset,
                        style.font_size,
                        color,
                    );
                }
            }
            DisplayCommand::Replaced {
                rect,
                replaced_type,
                style,
            } => {
                self.paint_replaced(
                    &replaced_type,
                    Some(&style),
                    rect.x(),
                    rect.y(),
                    rect.width(),
                    rect.height(),
                );
            }
            DisplayCommand::StackingContext {
                opacity,
                transform,
                blend_mode,
                isolated,
                commands,
            } => {
                if opacity <= 0.0 {
                    return Ok(());
                }

                let Some(bounds) = compute_commands_bounds(&commands) else {
                    return Ok(());
                };

                // Reduce layer size by translating to the top-left of the local bounds.
                let offset = Point::new(bounds.min_x(), bounds.min_y());
                let translated = translate_commands(commands, -offset.x, -offset.y);

                let width = bounds.width().ceil().max(0.0);
                let height = bounds.height().ceil().max(0.0);
                if width <= 0.0 || height <= 0.0 {
                    return Ok(());
                }

                let layer = match Pixmap::new(width as u32, height as u32) {
                    Some(p) => p,
                    None => return Ok(()),
                };

                let mut painter = Painter {
                    pixmap: layer,
                    background: Rgba::new(0, 0, 0, 0.0),
                    shaper: ShapingPipeline::new(),
                    font_ctx: self.font_ctx.clone(),
                    image_cache: self.image_cache.clone(),
                };
                for cmd in translated {
                    painter.execute_command(cmd)?;
                }

                let mut paint = PixmapPaint::default();
                paint.opacity = opacity.min(1.0);
                let mut final_transform = transform.unwrap_or_else(Transform::identity);
                final_transform = final_transform.pre_concat(Transform::from_translate(offset.x, offset.y));
                paint.blend_mode = if isolated {
                    SkiaBlendMode::SourceOver
                } else {
                    map_blend_mode(blend_mode)
                };
                self.pixmap
                    .draw_pixmap(0, 0, painter.pixmap.as_ref(), &paint, final_transform, None);
            }
        }
        Ok(())
    }

    /// Paints the background of a fragment
    fn paint_background(&mut self, x: f32, y: f32, width: f32, height: f32, style: &ComputedStyle) {
        // Skip if background is transparent
        if style.background_color.alpha_u8() == 0 {
            return;
        }

        let rects = background_rects(x, y, width, height, style);
        let clip = match style.background_clip {
            crate::style::types::BackgroundBox::BorderBox => rects.border,
            crate::style::types::BackgroundBox::PaddingBox => rects.padding,
            crate::style::types::BackgroundBox::ContentBox => rects.content,
        };

        if clip.width() <= 0.0 || clip.height() <= 0.0 {
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

        if let Some(rect) = SkiaRect::from_xywh(clip.x(), clip.y(), clip.width(), clip.height()) {
            let path = PathBuilder::from_rect(rect);
            self.pixmap
                .fill_path(&path, &paint, tiny_skia::FillRule::Winding, Transform::identity(), None);
        }
    }

    fn paint_background_image(&mut self, x: f32, y: f32, width: f32, height: f32, style: &ComputedStyle) {
        let Some(bg) = &style.background_image else { return };
        let src = match bg {
            BackgroundImage::Url(u) => u,
            _ => return, // Gradients not supported yet
        };

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

        if width <= 0.0 || height <= 0.0 {
            return;
        }

        let rects = background_rects(x, y, width, height, style);
        let clip_rect = match style.background_clip {
            crate::style::types::BackgroundBox::BorderBox => rects.border,
            crate::style::types::BackgroundBox::PaddingBox => rects.padding,
            crate::style::types::BackgroundBox::ContentBox => rects.content,
        };
        let origin_rect = match style.background_origin {
            crate::style::types::BackgroundBox::BorderBox => rects.border,
            crate::style::types::BackgroundBox::PaddingBox => rects.padding,
            crate::style::types::BackgroundBox::ContentBox => rects.content,
        };

        if clip_rect.width() <= 0.0 || clip_rect.height() <= 0.0 {
            return;
        }
        if origin_rect.width() <= 0.0 || origin_rect.height() <= 0.0 {
            return;
        }

        let (tile_w, tile_h) = compute_background_size(style, origin_rect.width(), origin_rect.height(), img_w, img_h);
        if tile_w <= 0.0 || tile_h <= 0.0 {
            return;
        }

        let (offset_x, offset_y) = resolve_background_offset(
            style.background_position,
            origin_rect.width(),
            origin_rect.height(),
            tile_w,
            tile_h,
            style.font_size,
        );

        let tile_origin_x = origin_rect.x() + offset_x;
        let tile_origin_y = origin_rect.y() + offset_y;
        let repeat = style.background_repeat;
        let repeat_x = matches!(repeat, BackgroundRepeat::Repeat | BackgroundRepeat::RepeatX);
        let repeat_y = matches!(repeat, BackgroundRepeat::Repeat | BackgroundRepeat::RepeatY);

        let start_x = if repeat_x {
            aligned_start(tile_origin_x, tile_w, clip_rect.min_x())
        } else {
            tile_origin_x
        };
        let start_y = if repeat_y {
            aligned_start(tile_origin_y, tile_h, clip_rect.min_y())
        } else {
            tile_origin_y
        };

        let max_x = clip_rect.max_x();
        let max_y = clip_rect.max_y();

        match repeat {
            BackgroundRepeat::NoRepeat => {
                self.paint_background_tile(&pixmap, tile_origin_x, tile_origin_y, tile_w, tile_h, clip_rect);
            }
            BackgroundRepeat::RepeatX => {
                let mut tx = start_x;
                while tx < max_x {
                    self.paint_background_tile(&pixmap, tx, tile_origin_y, tile_w, tile_h, clip_rect);
                    tx += tile_w;
                }
            }
            BackgroundRepeat::RepeatY => {
                let mut ty = start_y;
                while ty < max_y {
                    self.paint_background_tile(&pixmap, tile_origin_x, ty, tile_w, tile_h, clip_rect);
                    ty += tile_h;
                }
            }
            BackgroundRepeat::Repeat => {
                let mut ty = start_y;
                while ty < max_y {
                    let mut tx = start_x;
                    while tx < max_x {
                        self.paint_background_tile(&pixmap, tx, ty, tile_w, tile_h, clip_rect);
                        tx += tile_w;
                    }
                    ty += tile_h;
                }
            }
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
    ) {
        if tile_w <= 0.0 || tile_h <= 0.0 {
            return;
        }

        let tile_rect = Rect::from_xywh(tile_x, tile_y, tile_w, tile_h);
        let Some(intersection) = tile_rect.intersection(clip) else {
            return;
        };
        if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
            return;
        }

        let scale_x = tile_w / pixmap.width() as f32;
        let scale_y = tile_h / pixmap.height() as f32;
        if !scale_x.is_finite() || !scale_y.is_finite() {
            return;
        }

        let mut paint = Paint::default();
        paint.shader = Pattern::new(
            pixmap.as_ref(),
            SpreadMode::Pad,
            FilterQuality::Bilinear,
            1.0,
            Transform::from_row(scale_x, 0.0, 0.0, scale_y, tile_x, tile_y),
        );
        paint.anti_alias = false;

        if let Some(rect) = SkiaRect::from_xywh(
            intersection.x(),
            intersection.y(),
            intersection.width(),
            intersection.height(),
        ) {
            self.pixmap.fill_rect(rect, &paint, Transform::identity(), None);
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
        if width <= 0.0 || matches!(style, CssBorderStyle::None | CssBorderStyle::Hidden) {
            return;
        }

        let mut paint = Paint::default();
        paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
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

        // Try to render actual content for images
        if let ReplacedType::Image { src } = replaced_type {
            if self.paint_image_from_src(src, style, x, y, width, height) {
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
            Err(_) => return false,
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

        let (dest_x, dest_y, dest_w, dest_h) = match compute_object_fit(fit, pos, width, height, img_w, img_h) {
            Some(v) => v,
            None => return false,
        };

        let scale_x = dest_w / img_w;
        let scale_y = dest_h / img_h;
        if !scale_x.is_finite() || !scale_y.is_finite() {
            return false;
        }

        let mut paint = PixmapPaint::default();
        paint.quality = FilterQuality::Bilinear;

        let transform = Transform::from_row(scale_x, 0.0, 0.0, scale_y, x + dest_x, y + dest_y);
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

fn default_object_position() -> ObjectPosition {
    use crate::style::types::PositionKeyword::Center;
    ObjectPosition {
        x: PositionComponent::Keyword(Center),
        y: PositionComponent::Keyword(Center),
    }
}

fn resolve_object_position(comp: PositionComponent, free: f32) -> f32 {
    use crate::style::types::PositionKeyword::{Center, End, Start};

    match comp {
        PositionComponent::Keyword(Start) => 0.0,
        PositionComponent::Keyword(Center) => free * 0.5,
        PositionComponent::Keyword(End) => free,
        PositionComponent::Length(len) => {
            if len.unit.is_percentage() {
                len.resolve_against(free)
            } else if len.unit.is_absolute() {
                len.to_px()
            } else {
                len.value
            }
        }
        PositionComponent::Percentage(pct) => free * pct,
    }
}

fn compute_object_fit(
    fit: ObjectFit,
    position: ObjectPosition,
    box_width: f32,
    box_height: f32,
    image_width: f32,
    image_height: f32,
) -> Option<(f32, f32, f32, f32)> {
    if box_width <= 0.0 || box_height <= 0.0 || image_width <= 0.0 || image_height <= 0.0 {
        return None;
    }

    let scale_x = box_width / image_width;
    let scale_y = box_height / image_height;

    let scale = match fit {
        ObjectFit::Fill => (scale_x, scale_y),
        ObjectFit::Contain => {
            let s = scale_x.min(scale_y);
            (s, s)
        }
        ObjectFit::Cover => {
            let s = scale_x.max(scale_y);
            (s, s)
        }
        ObjectFit::None => (1.0, 1.0),
        ObjectFit::ScaleDown => {
            if image_width <= box_width && image_height <= box_height {
                (1.0, 1.0)
            } else {
                let s = scale_x.min(scale_y);
                (s, s)
            }
        }
    };

    let dest_w = image_width * scale.0;
    let dest_h = image_height * scale.1;
    let free_x = box_width - dest_w;
    let free_y = box_height - dest_h;

    let offset_x = resolve_object_position(position.x, free_x);
    let offset_y = resolve_object_position(position.y, free_y);

    Some((offset_x, offset_y, dest_w, dest_h))
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

fn command_bounds(cmd: &DisplayCommand) -> Option<Rect> {
    match cmd {
        DisplayCommand::Background { rect, .. }
        | DisplayCommand::Border { rect, .. }
        | DisplayCommand::Text { rect, .. }
        | DisplayCommand::Replaced { rect, .. } => Some(*rect),
        DisplayCommand::StackingContext { commands, .. } => compute_commands_bounds(commands),
    }
}

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
                opacity,
                transform,
                blend_mode,
                isolated,
                commands,
            } => DisplayCommand::StackingContext {
                opacity,
                transform,
                blend_mode,
                isolated,
                commands: translate_commands(commands, dx, dy),
            },
        })
        .collect()
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

fn compute_background_size(style: &ComputedStyle, area_w: f32, area_h: f32, img_w: f32, img_h: f32) -> (f32, f32) {
    match style.background_size {
        BackgroundSize::Auto => (img_w, img_h),
        BackgroundSize::Cover => {
            let scale = (area_w / img_w).max(area_h / img_h);
            (img_w * scale, img_h * scale)
        }
        BackgroundSize::Contain => {
            let scale = (area_w / img_w).min(area_h / img_h);
            (img_w * scale, img_h * scale)
        }
        BackgroundSize::Length(w, h) => {
            let resolved_w = resolve_length_for_paint(&w, style.font_size, area_w);
            let resolved_h = resolve_length_for_paint(&h, style.font_size, area_h);
            (resolved_w.max(0.0), resolved_h.max(0.0))
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
    let resolve_axis = |len: Length, area: f32, tile: f32| -> f32 {
        match len.unit {
            LengthUnit::Percent => (len.value / 100.0) * (area - tile),
            LengthUnit::Em | LengthUnit::Rem => len.resolve_with_font_size(font_size),
            _ if len.unit.is_absolute() => len.to_px(),
            _ => len.value,
        }
    };

    match pos {
        BackgroundPosition::Center => ((area_w - tile_w) * 0.5, (area_h - tile_h) * 0.5),
        BackgroundPosition::Position(x_len, y_len) => {
            let x = resolve_axis(x_len, area_w, tile_w);
            let y = resolve_axis(y_len, area_h, tile_h);
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

    #[test]
    fn object_fit_contain_centers_image() {
        let fit = ObjectFit::Contain;
        let position = ObjectPosition {
            x: PositionComponent::Keyword(crate::style::types::PositionKeyword::Center),
            y: PositionComponent::Keyword(crate::style::types::PositionKeyword::Center),
        };

        let (offset_x, offset_y, dest_w, dest_h) =
            compute_object_fit(fit, position, 200.0, 100.0, 100.0, 100.0).expect("fit computed");
        assert_eq!(dest_h, 100.0);
        assert_eq!(dest_w, 100.0);
        assert!((offset_x - 50.0).abs() < 0.01);
        assert!((offset_y - 0.0).abs() < 0.01);
    }

    #[test]
    fn background_cover_scales_to_fill() {
        let mut style = ComputedStyle::default();
        style.background_size = BackgroundSize::Cover;
        let (tw, th) = compute_background_size(&style, 200.0, 100.0, 50.0, 50.0);
        let (ox, oy) = resolve_background_offset(style.background_position, 200.0, 100.0, tw, th, style.font_size);
        assert!((tw - 200.0).abs() < 0.01);
        assert!((th - 200.0).abs() < 0.01);
        assert!((ox - 0.0).abs() < 0.01);
        assert!((oy - -50.0).abs() < 0.01);
    }
}
