use crate::geometry::{Point, Rect};
use crate::paint::display_list::BorderRadii;
use crate::paint::rasterize::fill_rounded_rect;
use crate::style::types::{BackgroundPosition, BasicShape, ClipPath, ClipRadii, FillRule, ReferenceBox, ShapeRadius};
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use tiny_skia::{FillRule as SkFillRule, IntSize, Mask, MaskType, PathBuilder, Pixmap, Rect as SkRect};

#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedClipPath {
    Inset {
        rect: Rect,
        radii: BorderRadii,
    },
    Circle {
        center: Point,
        radius: f32,
    },
    Ellipse {
        center: Point,
        radius_x: f32,
        radius_y: f32,
    },
    Polygon {
        points: Vec<Point>,
        fill_rule: SkFillRule,
    },
}

impl ResolvedClipPath {
    pub(crate) fn translate(&self, dx: f32, dy: f32) -> Self {
        let delta = Point::new(dx, dy);
        match self {
            ResolvedClipPath::Inset { rect, radii } => ResolvedClipPath::Inset {
                rect: rect.translate(delta),
                radii: *radii,
            },
            ResolvedClipPath::Circle { center, radius } => ResolvedClipPath::Circle {
                center: Point::new(center.x + dx, center.y + dy),
                radius: *radius,
            },
            ResolvedClipPath::Ellipse {
                center,
                radius_x,
                radius_y,
            } => ResolvedClipPath::Ellipse {
                center: Point::new(center.x + dx, center.y + dy),
                radius_x: *radius_x,
                radius_y: *radius_y,
            },
            ResolvedClipPath::Polygon { points, fill_rule } => ResolvedClipPath::Polygon {
                points: points.iter().map(|p| Point::new(p.x + dx, p.y + dy)).collect(),
                fill_rule: *fill_rule,
            },
        }
    }

    pub(crate) fn bounds(&self) -> Rect {
        match self {
            ResolvedClipPath::Inset { rect, .. } => *rect,
            ResolvedClipPath::Circle { center, radius } => {
                Rect::from_xywh(center.x - radius, center.y - radius, radius * 2.0, radius * 2.0)
            }
            ResolvedClipPath::Ellipse {
                center,
                radius_x,
                radius_y,
            } => Rect::from_xywh(center.x - radius_x, center.y - radius_y, radius_x * 2.0, radius_y * 2.0),
            ResolvedClipPath::Polygon { points, .. } => {
                let mut min_x = f32::INFINITY;
                let mut min_y = f32::INFINITY;
                let mut max_x = f32::NEG_INFINITY;
                let mut max_y = f32::NEG_INFINITY;
                for p in points {
                    min_x = min_x.min(p.x);
                    min_y = min_y.min(p.y);
                    max_x = max_x.max(p.x);
                    max_y = max_y.max(p.y);
                }
                Rect::from_xywh(min_x, min_y, (max_x - min_x).max(0.0), (max_y - min_y).max(0.0))
            }
        }
    }

    pub(crate) fn mask(&self, scale: f32, size: IntSize) -> Option<Mask> {
        if size.width() == 0 || size.height() == 0 {
            return None;
        }
        let mut mask_pixmap = Pixmap::new(size.width(), size.height())?;
        match self {
            ResolvedClipPath::Inset { rect, radii } => {
                let scaled = Rect::from_xywh(
                    rect.x() * scale,
                    rect.y() * scale,
                    rect.width() * scale,
                    rect.height() * scale,
                );
                let scaled_radii = BorderRadii {
                    top_left: radii.top_left * scale,
                    top_right: radii.top_right * scale,
                    bottom_right: radii.bottom_right * scale,
                    bottom_left: radii.bottom_left * scale,
                }
                .clamped(scaled.width(), scaled.height());
                let _ = fill_rounded_rect(
                    &mut mask_pixmap,
                    scaled.x(),
                    scaled.y(),
                    scaled.width(),
                    scaled.height(),
                    &scaled_radii,
                    crate::style::color::Rgba::new(255, 255, 255, 1.0),
                );
            }
            ResolvedClipPath::Circle { center, radius } => {
                if *radius <= 0.0 {
                    return None;
                }
                let r = *radius * scale;
                let rect = SkRect::from_xywh(center.x * scale - r, center.y * scale - r, r * 2.0, r * 2.0)?;
                if let Some(path) = PathBuilder::from_oval(rect) {
                    let mut paint = tiny_skia::Paint::default();
                    paint.anti_alias = true;
                    paint.set_color(tiny_skia::Color::from_rgba8(255, 255, 255, 255));
                    mask_pixmap.fill_path(&path, &paint, SkFillRule::Winding, Default::default(), None);
                }
            }
            ResolvedClipPath::Ellipse {
                center,
                radius_x,
                radius_y,
            } => {
                if *radius_x <= 0.0 || *radius_y <= 0.0 {
                    return None;
                }
                let rect = SkRect::from_xywh(
                    (center.x - radius_x) * scale,
                    (center.y - radius_y) * scale,
                    radius_x * 2.0 * scale,
                    radius_y * 2.0 * scale,
                )?;
                if let Some(path) = PathBuilder::from_oval(rect) {
                    let mut paint = tiny_skia::Paint::default();
                    paint.anti_alias = true;
                    paint.set_color(tiny_skia::Color::from_rgba8(255, 255, 255, 255));
                    mask_pixmap.fill_path(&path, &paint, SkFillRule::Winding, Default::default(), None);
                }
            }
            ResolvedClipPath::Polygon { points, fill_rule } => {
                if points.len() < 3 {
                    return None;
                }
                let mut builder = PathBuilder::new();
                builder.move_to(points[0].x * scale, points[0].y * scale);
                for p in &points[1..] {
                    builder.line_to(p.x * scale, p.y * scale);
                }
                builder.close();
                if let Some(path) = builder.finish() {
                    let mut paint = tiny_skia::Paint::default();
                    paint.anti_alias = true;
                    paint.set_color(tiny_skia::Color::from_rgba8(255, 255, 255, 255));
                    mask_pixmap.fill_path(&path, &paint, *fill_rule, Default::default(), None);
                }
            }
        }

        Some(Mask::from_pixmap(mask_pixmap.as_ref(), MaskType::Alpha))
    }
}

pub(crate) fn resolve_clip_path(
    style: &ComputedStyle,
    bounds: Rect,
    viewport: (f32, f32),
    font_ctx: &FontContext,
) -> Option<ResolvedClipPath> {
    match &style.clip_path {
        ClipPath::None => None,
        ClipPath::Box(reference) => {
            let rect = select_reference_box(reference_boxes(style, bounds, viewport, font_ctx), *reference);
            Some(ResolvedClipPath::Inset {
                rect,
                radii: BorderRadii::ZERO,
            })
        }
        ClipPath::BasicShape(shape, reference_override) => {
            let boxes = reference_boxes(style, bounds, viewport, font_ctx);
            let reference = reference_override.unwrap_or(ReferenceBox::BorderBox);
            let reference_rect = select_reference_box(boxes, reference);
            resolve_basic_shape(shape, reference_rect, style, viewport, font_ctx)
        }
    }
}

fn resolve_basic_shape(
    shape: &BasicShape,
    reference: Rect,
    style: &ComputedStyle,
    viewport: (f32, f32),
    font_ctx: &FontContext,
) -> Option<ResolvedClipPath> {
    match shape {
        BasicShape::Inset {
            top,
            right,
            bottom,
            left,
            border_radius,
        } => {
            let top_px = resolve_clip_length(*top, style, reference.height(), viewport, font_ctx);
            let right_px = resolve_clip_length(*right, style, reference.width(), viewport, font_ctx);
            let bottom_px = resolve_clip_length(*bottom, style, reference.height(), viewport, font_ctx);
            let left_px = resolve_clip_length(*left, style, reference.width(), viewport, font_ctx);
            let rect = Rect::from_xywh(
                reference.x() + left_px,
                reference.y() + top_px,
                (reference.width() - left_px - right_px).max(0.0),
                (reference.height() - top_px - bottom_px).max(0.0),
            );
            let radii = border_radius
                .map(|r| resolve_clip_radii(r, style, rect, viewport, font_ctx))
                .unwrap_or(BorderRadii::ZERO)
                .clamped(rect.width(), rect.height());
            Some(ResolvedClipPath::Inset { rect, radii })
        }
        BasicShape::Circle { radius, position } => {
            let center = resolve_position(position, reference, style, viewport, font_ctx);
            let resolved_radius = resolve_circle_radius(radius, center, reference, style, viewport, font_ctx);
            if resolved_radius <= 0.0 {
                return None;
            }
            Some(ResolvedClipPath::Circle {
                center,
                radius: resolved_radius,
            })
        }
        BasicShape::Ellipse {
            radius_x,
            radius_y,
            position,
        } => {
            let center = resolve_position(position, reference, style, viewport, font_ctx);
            let rx = resolve_ellipse_radius(radius_x, true, center, reference, style, viewport, font_ctx);
            let ry = resolve_ellipse_radius(radius_y, false, center, reference, style, viewport, font_ctx);
            if rx <= 0.0 || ry <= 0.0 {
                return None;
            }
            Some(ResolvedClipPath::Ellipse {
                center,
                radius_x: rx,
                radius_y: ry,
            })
        }
        BasicShape::Polygon { fill, points } => {
            if points.len() < 3 {
                return None;
            }
            let resolved: Vec<Point> = points
                .iter()
                .map(|(x, y)| {
                    Point::new(
                        reference.x() + resolve_clip_length(*x, style, reference.width(), viewport, font_ctx),
                        reference.y() + resolve_clip_length(*y, style, reference.height(), viewport, font_ctx),
                    )
                })
                .collect();
            let fill_rule = match fill {
                FillRule::EvenOdd => SkFillRule::EvenOdd,
                FillRule::NonZero => SkFillRule::Winding,
            };
            Some(ResolvedClipPath::Polygon {
                points: resolved,
                fill_rule,
            })
        }
    }
}

fn resolve_circle_radius(
    radius: &ShapeRadius,
    center: Point,
    reference: Rect,
    style: &ComputedStyle,
    viewport: (f32, f32),
    font_ctx: &FontContext,
) -> f32 {
    match radius {
        ShapeRadius::Length(len) => resolve_clip_length(
            *len,
            style,
            reference.width().min(reference.height()),
            viewport,
            font_ctx,
        ),
        ShapeRadius::ClosestSide => {
            let dx = (center.x - reference.x())
                .abs()
                .min((reference.max_x() - center.x).abs());
            let dy = (center.y - reference.y())
                .abs()
                .min((reference.max_y() - center.y).abs());
            dx.min(dy)
        }
        ShapeRadius::FarthestSide => {
            let dx = (center.x - reference.x())
                .abs()
                .max((reference.max_x() - center.x).abs());
            let dy = (center.y - reference.y())
                .abs()
                .max((reference.max_y() - center.y).abs());
            dx.max(dy)
        }
    }
}

fn resolve_ellipse_radius(
    radius: &ShapeRadius,
    horizontal: bool,
    center: Point,
    reference: Rect,
    style: &ComputedStyle,
    viewport: (f32, f32),
    font_ctx: &FontContext,
) -> f32 {
    let base = if horizontal {
        reference.width()
    } else {
        reference.height()
    };
    match radius {
        ShapeRadius::Length(len) => resolve_clip_length(*len, style, base, viewport, font_ctx),
        ShapeRadius::ClosestSide => {
            if horizontal {
                (center.x - reference.x())
                    .abs()
                    .min((reference.max_x() - center.x).abs())
            } else {
                (center.y - reference.y())
                    .abs()
                    .min((reference.max_y() - center.y).abs())
            }
        }
        ShapeRadius::FarthestSide => {
            if horizontal {
                (center.x - reference.x())
                    .abs()
                    .max((reference.max_x() - center.x).abs())
            } else {
                (center.y - reference.y())
                    .abs()
                    .max((reference.max_y() - center.y).abs())
            }
        }
    }
}

fn resolve_position(
    position: &BackgroundPosition,
    reference: Rect,
    style: &ComputedStyle,
    viewport: (f32, f32),
    font_ctx: &FontContext,
) -> Point {
    let (x, y) = match position {
        BackgroundPosition::Position { x, y } => {
            let px = resolve_position_component(x, reference.x(), reference.width(), style, viewport, font_ctx);
            let py = resolve_position_component(y, reference.y(), reference.height(), style, viewport, font_ctx);
            (px, py)
        }
    };
    Point::new(x, y)
}

fn resolve_position_component(
    component: &crate::style::types::BackgroundPositionComponent,
    origin: f32,
    length: f32,
    style: &ComputedStyle,
    viewport: (f32, f32),
    font_ctx: &FontContext,
) -> f32 {
    let offset = resolve_clip_length(component.offset, style, length, viewport, font_ctx);
    origin + component.alignment * length + offset
}

fn resolve_clip_length(
    len: Length,
    style: &ComputedStyle,
    percentage_base: f32,
    viewport: (f32, f32),
    _font_ctx: &FontContext,
) -> f32 {
    len.resolve_with_context(
        Some(percentage_base),
        viewport.0,
        viewport.1,
        style.font_size,
        style.root_font_size,
    )
    .unwrap_or(len.value)
}

fn resolve_clip_radii(
    radii: ClipRadii,
    style: &ComputedStyle,
    rect: Rect,
    viewport: (f32, f32),
    font_ctx: &FontContext,
) -> BorderRadii {
    BorderRadii {
        top_left: resolve_clip_length(radii.top_left, style, rect.width(), viewport, font_ctx),
        top_right: resolve_clip_length(radii.top_right, style, rect.width(), viewport, font_ctx),
        bottom_right: resolve_clip_length(radii.bottom_right, style, rect.width(), viewport, font_ctx),
        bottom_left: resolve_clip_length(radii.bottom_left, style, rect.width(), viewport, font_ctx),
    }
}

#[derive(Clone, Copy)]
struct ReferenceBoxes {
    border: Rect,
    padding: Rect,
    content: Rect,
    margin: Rect,
}

fn reference_boxes(
    style: &ComputedStyle,
    bounds: Rect,
    viewport: (f32, f32),
    font_ctx: &FontContext,
) -> ReferenceBoxes {
    let base = bounds.width().max(0.0);
    let border_left = resolve_clip_length(style.border_left_width, style, base, viewport, font_ctx);
    let border_right = resolve_clip_length(style.border_right_width, style, base, viewport, font_ctx);
    let border_top = resolve_clip_length(style.border_top_width, style, base, viewport, font_ctx);
    let border_bottom = resolve_clip_length(style.border_bottom_width, style, base, viewport, font_ctx);

    let padding_left = resolve_clip_length(style.padding_left, style, base, viewport, font_ctx);
    let padding_right = resolve_clip_length(style.padding_right, style, base, viewport, font_ctx);
    let padding_top = resolve_clip_length(style.padding_top, style, base, viewport, font_ctx);
    let padding_bottom = resolve_clip_length(style.padding_bottom, style, base, viewport, font_ctx);

    let border_rect = bounds;
    let padding_rect = inset_rect(border_rect, border_left, border_top, border_right, border_bottom);
    let content_rect = inset_rect(padding_rect, padding_left, padding_top, padding_right, padding_bottom);

    let margin_left = style
        .margin_left
        .map(|m| resolve_clip_length(m, style, base, viewport, font_ctx))
        .unwrap_or(0.0);
    let margin_right = style
        .margin_right
        .map(|m| resolve_clip_length(m, style, base, viewport, font_ctx))
        .unwrap_or(0.0);
    let margin_top = style
        .margin_top
        .map(|m| resolve_clip_length(m, style, base, viewport, font_ctx))
        .unwrap_or(0.0);
    let margin_bottom = style
        .margin_bottom
        .map(|m| resolve_clip_length(m, style, base, viewport, font_ctx))
        .unwrap_or(0.0);

    let margin_rect = Rect::from_xywh(
        border_rect.x() - margin_left,
        border_rect.y() - margin_top,
        border_rect.width() + margin_left + margin_right,
        border_rect.height() + margin_top + margin_bottom,
    );

    ReferenceBoxes {
        border: border_rect,
        padding: padding_rect,
        content: content_rect,
        margin: margin_rect,
    }
}

fn select_reference_box(boxes: ReferenceBoxes, reference: ReferenceBox) -> Rect {
    match reference {
        ReferenceBox::BorderBox | ReferenceBox::FillBox | ReferenceBox::StrokeBox | ReferenceBox::ViewBox => {
            boxes.border
        }
        ReferenceBox::PaddingBox => boxes.padding,
        ReferenceBox::ContentBox => boxes.content,
        ReferenceBox::MarginBox => boxes.margin,
    }
}

fn inset_rect(rect: Rect, left: f32, top: f32, right: f32, bottom: f32) -> Rect {
    Rect::from_xywh(
        rect.x() + left,
        rect.y() + top,
        (rect.width() - left - right).max(0.0),
        (rect.height() - top - bottom).max(0.0),
    )
}
