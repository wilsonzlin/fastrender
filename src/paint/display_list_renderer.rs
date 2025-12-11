//! Display list renderer
//!
//! Executes a `DisplayList` into a pixel buffer using the existing canvas/text
//! rasterization utilities. This path keeps the display list as the
//! paint-time contract while still reusing the shared `FontContext`.

use crate::error::{RenderError, Result};
use crate::geometry::{Point, Rect};
use crate::paint::blur::apply_gaussian_blur;
use crate::paint::canvas::Canvas;
use crate::paint::display_list::{
    BlendMode, BorderItem, BorderRadii, BorderSide, BoxShadowItem, ClipItem, DecorationPaint, DecorationStroke,
    DisplayItem, DisplayList, EmphasisMark, FillRectItem, FontId, GlyphInstance, ImageItem, LinearGradientItem,
    OpacityItem, RadialGradientItem, ResolvedFilter, StrokeRectItem, TextEmphasis, TextItem, TextShadowItem,
    TransformItem,
};
use crate::paint::rasterize::{fill_rounded_rect, render_box_shadow, BoxShadow};
use crate::paint::text_shadow::PathBounds;
use crate::style::color::Rgba;
use crate::style::types::{
    BorderStyle as CssBorderStyle, TextDecorationStyle, TextEmphasisFill, TextEmphasisPosition, TextEmphasisShape,
    TextEmphasisStyle,
};
use crate::text::font_db::{FontStretch, FontStyle as DbFontStyle, LoadedFont};
use crate::text::font_loader::FontContext;
use crate::text::shaper::GlyphPosition;
use tiny_skia::{
    BlendMode as SkiaBlendMode, GradientStop as SkiaGradientStop, LinearGradient, Mask, PathBuilder, Pixmap,
    PixmapPaint, Point as SkiaPoint, PremultipliedColorU8, RadialGradient, SpreadMode, Stroke, StrokeDash, Transform,
};

fn map_blend_mode(mode: BlendMode) -> tiny_skia::BlendMode {
    match mode {
        BlendMode::Normal => tiny_skia::BlendMode::SourceOver,
        BlendMode::Multiply => tiny_skia::BlendMode::Multiply,
        BlendMode::Screen => tiny_skia::BlendMode::Screen,
        BlendMode::Overlay => tiny_skia::BlendMode::Overlay,
        BlendMode::Darken => tiny_skia::BlendMode::Darken,
        BlendMode::Lighten => tiny_skia::BlendMode::Lighten,
        BlendMode::ColorDodge => tiny_skia::BlendMode::ColorDodge,
        BlendMode::ColorBurn => tiny_skia::BlendMode::ColorBurn,
        BlendMode::HardLight => tiny_skia::BlendMode::HardLight,
        BlendMode::SoftLight => tiny_skia::BlendMode::SoftLight,
        BlendMode::Difference => tiny_skia::BlendMode::Difference,
        BlendMode::Exclusion => tiny_skia::BlendMode::Exclusion,
        BlendMode::Hue => tiny_skia::BlendMode::Hue,
        BlendMode::Saturation => tiny_skia::BlendMode::Saturation,
        BlendMode::Color => tiny_skia::BlendMode::Color,
        BlendMode::Luminosity => tiny_skia::BlendMode::Luminosity,
    }
}

fn shade_color(color: &Rgba, factor: f32) -> Rgba {
    let clamp = |v: f32| v.max(0.0).min(255.0) as u8;
    let r = clamp(color.r as f32 * factor);
    let g = clamp(color.g as f32 * factor);
    let b = clamp(color.b as f32 * factor);
    Rgba::new(r, g, b, color.a)
}

fn rgb_to_hsl(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    let l = (max + min) * 0.5;

    if (max - min).abs() < f32::EPSILON {
        return (0.0, 0.0, l);
    }

    let d = max - min;
    let s = if l > 0.5 {
        d / (2.0 - max - min)
    } else {
        d / (max + min)
    };
    let h = if max == r {
        ((g - b) / d + if g < b { 6.0 } else { 0.0 }) / 6.0
    } else if max == g {
        ((b - r) / d + 2.0) / 6.0
    } else {
        ((r - g) / d + 4.0) / 6.0
    };
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

fn apply_hsl_blend(mode: BlendMode, src: (f32, f32, f32), dst: (f32, f32, f32)) -> (f32, f32, f32) {
    let (sh, ss, sl) = rgb_to_hsl(src.0, src.1, src.2);
    let (dh, ds, dl) = rgb_to_hsl(dst.0, dst.1, dst.2);
    match mode {
        BlendMode::Hue => hsl_to_rgb(sh, ds, dl),
        BlendMode::Saturation => hsl_to_rgb(dh, ss, dl),
        BlendMode::Color => hsl_to_rgb(sh, ss, dl),
        BlendMode::Luminosity => hsl_to_rgb(dh, ds, sl),
        _ => dst,
    }
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

fn set_paint_color(paint: &mut tiny_skia::Paint, color: &Rgba, opacity: f32) {
    let alpha = (color.a * opacity * 255.0).round().clamp(0.0, 255.0) as u8;
    paint.set_color_rgba8(color.r, color.g, color.b, alpha);
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

fn filter_outset(filters: &[ResolvedFilter], scale: f32) -> (f32, f32, f32, f32) {
    let mut left: f32 = 0.0;
    let mut top: f32 = 0.0;
    let mut right: f32 = 0.0;
    let mut bottom: f32 = 0.0;

    for filter in filters {
        match *filter {
            ResolvedFilter::Blur(radius) => {
                let delta = (radius * scale).abs() * 3.0;
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
                let dx = offset_x * scale;
                let dy = offset_y * scale;
                let blur = blur_radius * scale;
                let spread = spread * scale;
                let delta = blur.abs() * 3.0 + spread.max(0.0);
                left = left.max(delta - dx.min(0.0));
                right = right.max(delta + dx.max(0.0));
                top = top.max(delta - dy.min(0.0));
                bottom = bottom.max(delta + dy.max(0.0));
            }
            _ => {}
        }
    }

    (left.max(0.0), top.max(0.0), right.max(0.0), bottom.max(0.0))
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
    let (out_l, out_t, out_r, out_b) = filter_outset(filters, scale);
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

    let bytes_per_row = pixmap.width() as usize * 4;
    let region_row_bytes = region_w as usize * 4;
    let start = (clamped_y as usize * bytes_per_row) + clamped_x as usize * 4;
    let data = pixmap.data();
    let dest = region.data_mut();

    for row in 0..region_h as usize {
        let src_idx = start + row * bytes_per_row;
        let dst_idx = row * region_row_bytes;
        dest[dst_idx..dst_idx + region_row_bytes].copy_from_slice(&data[src_idx..src_idx + region_row_bytes]);
    }

    apply_filters(&mut region, filters, scale);

    if !radii.is_zero() {
        let mut mask = match Pixmap::new(region_w, region_h) {
            Some(p) => p,
            None => return,
        };
        let local_x = bounds.x() - clamped_x as f32;
        let local_y = bounds.y() - clamped_y as f32;
        let _ = fill_rounded_rect(
            &mut mask,
            local_x,
            local_y,
            bounds.width(),
            bounds.height(),
            &radii.clamped(bounds.width(), bounds.height()),
            Rgba::new(255, 255, 255, 1.0),
        );
        for (dst, m) in region.pixels_mut().iter_mut().zip(mask.pixels()) {
            let alpha = m.alpha();
            if alpha == 255 {
                continue;
            }
            if alpha == 0 {
                *dst = tiny_skia::PremultipliedColorU8::from_rgba(0, 0, 0, 0).unwrap();
                continue;
            }
            let scale = alpha as f32 / 255.0;
            let premultiply = |v: u8| ((v as f32 * scale).round().clamp(0.0, 255.0)) as u8;
            *dst = tiny_skia::PremultipliedColorU8::from_rgba(
                premultiply(dst.red()),
                premultiply(dst.green()),
                premultiply(dst.blue()),
                premultiply(dst.alpha()),
            )
            .unwrap_or(*dst);
        }
    }

    let write_x = bounds.min_x().floor().max(clamped_x as f32) as u32;
    let write_y = bounds.min_y().floor().max(clamped_y as f32) as u32;
    let write_w = (bounds.width().ceil() as i32).min(pix_w - write_x as i32).max(0) as u32;
    let write_h = (bounds.height().ceil() as i32).min(pix_h - write_y as i32).max(0) as u32;
    if write_w == 0 || write_h == 0 {
        return;
    }

    let src_start_x = (write_x as i32 - clamped_x as i32) as u32;
    let src_start_y = (write_y as i32 - clamped_y as i32) as u32;
    for row in 0..write_h as usize {
        let dst_idx = (write_y as usize + row) * bytes_per_row + write_x as usize * 4;
        let src_idx = ((src_start_y as usize + row) * region_w as usize + src_start_x as usize) * 4;
        let src = &region.data()[src_idx..src_idx + write_w as usize * 4];
        let dst = &mut pixmap.data_mut()[dst_idx..dst_idx + write_w as usize * 4];
        dst.copy_from_slice(src);
    }
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

fn apply_color_filter(pixmap: &mut Pixmap, f: impl Fn((u8, u8, u8), f32) -> ((u8, u8, u8), f32)) {
    for pixel in pixmap.pixels_mut() {
        let a = pixel.alpha() as f32 / 255.0;
        let (c, a2) = f((pixel.red(), pixel.green(), pixel.blue()), a);
        let final_a = (a2 * 255.0).round().clamp(0.0, 255.0) as u8;
        let premultiply = |v: u8| ((v as f32 * a2).round().clamp(0.0, 255.0)) as u8;
        *pixel =
            tiny_skia::PremultipliedColorU8::from_rgba(premultiply(c.0), premultiply(c.1), premultiply(c.2), final_a)
                .unwrap_or(tiny_skia::PremultipliedColorU8::from_rgba(0, 0, 0, 0).unwrap());
    }
}

fn scale_color((r, g, b): (u8, u8, u8), amount: f32) -> (u8, u8, u8) {
    let scale = |v: u8| ((v as f32) * amount).round().clamp(0.0, 255.0) as u8;
    (scale(r), scale(g), scale(b))
}

fn apply_contrast((r, g, b): (u8, u8, u8), amount: f32) -> (u8, u8, u8) {
    let factor = (259.0 * (amount + 255.0)) / (255.0 * (259.0 - amount));
    let adjust = |v: u8| ((factor * (v as f32 - 128.0) + 128.0).round().clamp(0.0, 255.0)) as u8;
    (adjust(r), adjust(g), adjust(b))
}

fn grayscale((r, g, b): (u8, u8, u8), amount: f32) -> (u8, u8, u8) {
    let gray = (0.2126 * r as f32 + 0.7152 * g as f32 + 0.0722 * b as f32)
        .round()
        .clamp(0.0, 255.0) as u8;
    let mix = |v: u8| {
        ((v as f32 * (1.0 - amount) + gray as f32 * amount)
            .round()
            .clamp(0.0, 255.0)) as u8
    };
    (mix(r), mix(g), mix(b))
}

fn sepia((r, g, b): (u8, u8, u8), amount: f32) -> (u8, u8, u8) {
    let tr = (0.393 * r as f32 + 0.769 * g as f32 + 0.189 * b as f32)
        .round()
        .clamp(0.0, 255.0);
    let tg = (0.349 * r as f32 + 0.686 * g as f32 + 0.168 * b as f32)
        .round()
        .clamp(0.0, 255.0);
    let tb = (0.272 * r as f32 + 0.534 * g as f32 + 0.131 * b as f32)
        .round()
        .clamp(0.0, 255.0);
    let mix = |orig: u8, target: f32| {
        ((orig as f32 * (1.0 - amount) + target * amount)
            .round()
            .clamp(0.0, 255.0)) as u8
    };
    (mix(r, tr), mix(g, tg), mix(b, tb))
}

fn saturate((r, g, b): (u8, u8, u8), amount: f32) -> (u8, u8, u8) {
    let gray = 0.299 * r as f32 + 0.587 * g as f32 + 0.114 * b as f32;
    let mix = |v: u8| ((gray + (v as f32 - gray) * amount).round().clamp(0.0, 255.0)) as u8;
    (mix(r), mix(g), mix(b))
}

fn hue_rotate((r, g, b): (u8, u8, u8), degrees: f32) -> (u8, u8, u8) {
    let rad = degrees.to_radians();
    let cos = rad.cos();
    let sin = rad.sin();
    let nr = (0.213 + cos * 0.787 - sin * 0.213) * r as f32
        + (0.715 - cos * 0.715 - sin * 0.715) * g as f32
        + (0.072 - cos * 0.072 + sin * 0.928) * b as f32;
    let ng = (0.213 - cos * 0.213 + sin * 0.143) * r as f32
        + (0.715 + cos * 0.285 + sin * 0.140) * g as f32
        + (0.072 - cos * 0.072 - sin * 0.283) * b as f32;
    let nb = (0.213 - cos * 0.213 - sin * 0.787) * r as f32
        + (0.715 - cos * 0.715 + sin * 0.715) * g as f32
        + (0.072 + cos * 0.928 + sin * 0.072) * b as f32;
    (
        nr.round().clamp(0.0, 255.0) as u8,
        ng.round().clamp(0.0, 255.0) as u8,
        nb.round().clamp(0.0, 255.0) as u8,
    )
}

fn invert((r, g, b): (u8, u8, u8), amount: f32) -> (u8, u8, u8) {
    let inv = |v: u8| {
        ((255.0 - v as f32) * amount + v as f32 * (1.0 - amount))
            .round()
            .clamp(0.0, 255.0) as u8
    };
    (inv(r), inv(g), inv(b))
}

/// Renders a display list into a pixmap using the provided font context.
pub struct DisplayListRenderer {
    canvas: Canvas,
    font_ctx: FontContext,
    stacking_layers: Vec<StackingRecord>,
    blend_stack: Vec<Option<BlendMode>>,
    scale: f32,
}

#[derive(Debug)]
struct StackingRecord {
    needs_layer: bool,
    filters: Vec<ResolvedFilter>,
    radii: BorderRadii,
    mask_bounds: Rect,
}

impl DisplayListRenderer {
    #[inline]
    fn ds_len(&self, v: f32) -> f32 {
        v * self.scale
    }

    #[inline]
    fn ds_point(&self, p: Point) -> Point {
        Point::new(p.x * self.scale, p.y * self.scale)
    }

    #[inline]
    fn ds_rect(&self, rect: Rect) -> Rect {
        Rect::from_xywh(
            rect.x() * self.scale,
            rect.y() * self.scale,
            rect.width() * self.scale,
            rect.height() * self.scale,
        )
    }

    #[inline]
    fn ds_radii(&self, radii: BorderRadii) -> BorderRadii {
        BorderRadii {
            top_left: radii.top_left * self.scale,
            top_right: radii.top_right * self.scale,
            bottom_right: radii.bottom_right * self.scale,
            bottom_left: radii.bottom_left * self.scale,
        }
    }

    fn ds_filters(&self, filters: &[ResolvedFilter]) -> Vec<ResolvedFilter> {
        filters.to_vec()
    }

    fn scale_text_item(&self, item: &TextItem) -> TextItem {
        let mut scaled = item.clone();
        scaled.origin = self.ds_point(item.origin);
        scaled.font_size = self.ds_len(item.font_size);
        scaled.advance_width = self.ds_len(item.advance_width);
        scaled.synthetic_bold = self.ds_len(item.synthetic_bold);
        scaled.glyphs = item
            .glyphs
            .iter()
            .map(|g| GlyphInstance {
                glyph_id: g.glyph_id,
                offset: self.ds_point(g.offset),
                advance: self.ds_len(g.advance),
            })
            .collect();
        scaled.shadows = item
            .shadows
            .iter()
            .map(|s| TextShadowItem {
                offset: self.ds_point(s.offset),
                blur_radius: self.ds_len(s.blur_radius),
                color: s.color,
            })
            .collect();
        if let Some(emphasis) = &item.emphasis {
            let mut e = emphasis.clone();
            e.size = self.ds_len(e.size);
            e.marks = e.marks.iter().map(|m| EmphasisMark { center: self.ds_point(m.center) }).collect();
            if let Some(text) = &e.text {
                let mut t = text.clone();
                t.font_size = self.ds_len(t.font_size);
                t.glyphs = t
                    .glyphs
                    .into_iter()
                    .map(|g| GlyphInstance {
                        glyph_id: g.glyph_id,
                        offset: self.ds_point(g.offset),
                        advance: self.ds_len(g.advance),
                    })
                    .collect();
                e.text = Some(t);
            }
            scaled.emphasis = Some(e);
        }
        scaled
    }

    fn scale_decoration_item(
        &self,
        item: &crate::paint::display_list::TextDecorationItem,
    ) -> crate::paint::display_list::TextDecorationItem {
        let mut scaled = item.clone();
        scaled.bounds = self.ds_rect(item.bounds);
        scaled.line_start = self.ds_len(item.line_start);
        scaled.line_width = self.ds_len(item.line_width);
        scaled.decorations = item
            .decorations
            .iter()
            .map(|d| {
                let scale_stroke = |s: &Option<DecorationStroke>| -> Option<DecorationStroke> {
                    s.as_ref().map(|stroke| DecorationStroke {
                        center: self.ds_len(stroke.center),
                        thickness: self.ds_len(stroke.thickness),
                        segments: stroke.segments.as_ref().map(|segs| {
                            segs.iter()
                                .map(|(a, b)| (self.ds_len(*a), self.ds_len(*b)))
                                .collect()
                        }),
                    })
                };
                DecorationPaint {
                    style: d.style,
                    color: d.color,
                    underline: scale_stroke(&d.underline),
                    overline: scale_stroke(&d.overline),
                    line_through: scale_stroke(&d.line_through),
                }
            })
            .collect();
        scaled
    }

    /// Creates a renderer with the given dimensions and background color.
    pub fn new(width: u32, height: u32, background: Rgba, font_ctx: FontContext) -> Result<Self> {
        Self::new_scaled(width, height, background, font_ctx, 1.0)
    }

    /// Creates a renderer with an explicit device scale (DPR). Coordinates are in CSS px.
    pub fn new_scaled(
        width: u32,
        height: u32,
        background: Rgba,
        font_ctx: FontContext,
        scale: f32,
    ) -> Result<Self> {
        let scale = if scale.is_finite() && scale > 0.0 { scale } else { 1.0 };
        let device_w = ((width as f32) * scale).round().max(1.0) as u32;
        let device_h = ((height as f32) * scale).round().max(1.0) as u32;
        Ok(Self {
            canvas: Canvas::new(device_w, device_h, background)?,
            font_ctx,
            stacking_layers: Vec::new(),
            blend_stack: Vec::new(),
            scale,
        })
    }

    fn render_linear_gradient(&mut self, item: &LinearGradientItem) {
        let Some(stops) = self.convert_stops(&item.stops) else {
            return;
        };
        let rect = self.ds_rect(item.rect);
        let start = SkiaPoint::from_xy(rect.x() + self.ds_len(item.start.x), rect.y() + self.ds_len(item.start.y));
        let end = SkiaPoint::from_xy(rect.x() + self.ds_len(item.end.x), rect.y() + self.ds_len(item.end.y));
        let spread = match item.spread {
            crate::paint::display_list::GradientSpread::Pad => SpreadMode::Pad,
            crate::paint::display_list::GradientSpread::Repeat => SpreadMode::Repeat,
            crate::paint::display_list::GradientSpread::Reflect => SpreadMode::Reflect,
        };
        let Some(shader) = LinearGradient::new(start, end, stops, spread, Transform::identity()) else {
            return;
        };

        let Some(skia_rect) = tiny_skia::Rect::from_xywh(rect.x(), rect.y(), rect.width(), rect.height()) else {
            return;
        };

        let path = tiny_skia::PathBuilder::from_rect(skia_rect);

        let mut paint = tiny_skia::Paint::default();
        paint.shader = shader;
        paint.anti_alias = true;
        paint.blend_mode = self.canvas.blend_mode();
        let transform = self.canvas.transform();
        let clip = self.canvas.clip_mask().cloned();
        self.canvas
            .pixmap_mut()
            .fill_path(&path, &paint, tiny_skia::FillRule::Winding, transform, clip.as_ref());
    }

    fn render_radial_gradient(&mut self, item: &RadialGradientItem) {
        let Some(stops) = self.convert_stops(&item.stops) else {
            return;
        };
        let rect = self.ds_rect(item.rect);
        let center = SkiaPoint::from_xy(rect.x() + self.ds_len(item.center.x), rect.y() + self.ds_len(item.center.y));
        let spread = match item.spread {
            crate::paint::display_list::GradientSpread::Pad => SpreadMode::Pad,
            crate::paint::display_list::GradientSpread::Repeat => SpreadMode::Repeat,
            crate::paint::display_list::GradientSpread::Reflect => SpreadMode::Reflect,
        };
        let radius = self.ds_len(item.radius);
        let Some(shader) = RadialGradient::new(center, center, radius, stops, spread, Transform::identity())
        else {
            return;
        };

        let Some(skia_rect) = tiny_skia::Rect::from_xywh(rect.x(), rect.y(), rect.width(), rect.height()) else {
            return;
        };

        let path = tiny_skia::PathBuilder::from_rect(skia_rect);

        let mut paint = tiny_skia::Paint::default();
        paint.shader = shader;
        paint.anti_alias = true;
        paint.blend_mode = self.canvas.blend_mode();
        let transform = self.canvas.transform();
        let clip = self.canvas.clip_mask().cloned();
        self.canvas
            .pixmap_mut()
            .fill_path(&path, &paint, tiny_skia::FillRule::Winding, transform, clip.as_ref());
    }

    fn render_border(&mut self, item: &BorderItem) {
        let opacity = self.canvas.opacity().clamp(0.0, 1.0);
        if opacity <= 0.0 {
            return;
        }

        let transform = self.canvas.transform();
        let clip = self.canvas.clip_mask().cloned();
        let blend_mode = self.canvas.blend_mode();
        let rect = self.ds_rect(item.rect);
        let radii = self.ds_radii(item.radii);
        let top = BorderSide { width: self.ds_len(item.top.width), ..item.top.clone() };
        let right = BorderSide { width: self.ds_len(item.right.width), ..item.right.clone() };
        let bottom = BorderSide { width: self.ds_len(item.bottom.width), ..item.bottom.clone() };
        let left = BorderSide { width: self.ds_len(item.left.width), ..item.left.clone() };

        let mut pushed_clip = false;
        if radii.has_radius() {
            self.canvas.save();
            self.canvas.set_clip_with_radii(rect, Some(radii));
            pushed_clip = true;
        }

        let edges: [(_, _, _, _); 4] = [
            (
                BorderEdge::Top,
                &top,
                (rect.x(), rect.y() + top.width * 0.5),
                (rect.x() + rect.width(), rect.y() + top.width * 0.5),
            ),
            (
                BorderEdge::Right,
                &right,
                (rect.x() + rect.width() - right.width * 0.5, rect.y()),
                (rect.x() + rect.width() - right.width * 0.5, rect.y() + rect.height()),
            ),
            (
                BorderEdge::Bottom,
                &bottom,
                (rect.x(), rect.y() + rect.height() - bottom.width * 0.5),
                (rect.x() + rect.width(), rect.y() + rect.height() - bottom.width * 0.5),
            ),
            (
                BorderEdge::Left,
                &left,
                (rect.x() + left.width * 0.5, rect.y()),
                (rect.x() + left.width * 0.5, rect.y() + rect.height()),
            ),
        ];

        for (edge, side, (x1, y1), (x2, y2)) in edges {
            self.render_border_edge(
                edge,
                x1,
                y1,
                x2,
                y2,
                side,
                blend_mode,
                opacity,
                clip.as_ref(),
                transform,
            );
        }

        if pushed_clip {
            self.canvas.restore();
        }
    }

    fn render_border_edge(
        &mut self,
        edge: BorderEdge,
        x1: f32,
        y1: f32,
        x2: f32,
        y2: f32,
        side: &BorderSide,
        blend_mode: tiny_skia::BlendMode,
        opacity: f32,
        clip: Option<&Mask>,
        transform: Transform,
    ) {
        if side.width <= 0.0
            || matches!(side.style, CssBorderStyle::None | CssBorderStyle::Hidden)
            || side.color.is_transparent()
        {
            return;
        }

        let mut paint = tiny_skia::Paint::default();
        set_paint_color(&mut paint, &side.color, opacity);
        paint.blend_mode = blend_mode;
        paint.anti_alias = true;

        let mut stroke = Stroke::default();
        stroke.width = side.width;
        stroke.line_cap = match side.style {
            CssBorderStyle::Dotted => tiny_skia::LineCap::Round,
            _ => tiny_skia::LineCap::Butt,
        };

        match side.style {
            CssBorderStyle::Dotted => {
                stroke.dash = StrokeDash::new(vec![side.width, side.width], 0.0);
            }
            CssBorderStyle::Dashed => {
                stroke.dash = StrokeDash::new(vec![3.0 * side.width, side.width], 0.0);
            }
            _ => {}
        }

        let mut path = PathBuilder::new();
        path.move_to(x1, y1);
        path.line_to(x2, y2);
        let Some(base_path) = path.finish() else {
            return;
        };

        let pixmap = self.canvas.pixmap_mut();

        match side.style {
            CssBorderStyle::Double => {
                let third = side.width / 3.0;
                let offset = third + third * 0.5;

                let (outer_path, inner_path) = edge.parallel_lines(x1, y1, x2, y2, offset);

                let mut inner_stroke = stroke.clone();
                inner_stroke.width = third;
                stroke.width = third;

                if let Some(outer) = outer_path {
                    pixmap.stroke_path(&outer, &paint, &stroke, transform, clip);
                }
                if let Some(inner) = inner_path {
                    pixmap.stroke_path(&inner, &paint, &inner_stroke, transform, clip);
                }
            }
            CssBorderStyle::Groove | CssBorderStyle::Ridge => {
                let half = side.width / 2.0;
                let offset = half * 0.5;
                let (first_path, second_path) = edge.parallel_lines(x1, y1, x2, y2, offset);

                let mut first_paint = paint.clone();
                let mut second_paint = paint.clone();
                let mut first_stroke = stroke.clone();
                let mut second_stroke = stroke.clone();
                first_stroke.width = half;
                second_stroke.width = half;

                let (first_color, second_color) = edge.groove_ridge_colors(&side.color, side.style);
                set_paint_color(&mut first_paint, &first_color, opacity);
                set_paint_color(&mut second_paint, &second_color, opacity);

                if let Some(first) = first_path {
                    pixmap.stroke_path(&first, &first_paint, &first_stroke, transform, clip);
                }
                if let Some(second) = second_path {
                    pixmap.stroke_path(&second, &second_paint, &second_stroke, transform, clip);
                }
            }
            CssBorderStyle::Inset | CssBorderStyle::Outset => {
                let shaded = edge.inset_outset_color(&side.color, side.style);
                set_paint_color(&mut paint, &shaded, opacity);
                pixmap.stroke_path(&base_path, &paint, &stroke, transform, clip);
            }
            _ => {
                pixmap.stroke_path(&base_path, &paint, &stroke, transform, clip);
            }
        }
    }

    fn convert_stops(
        &self,
        stops: &[crate::paint::display_list::GradientStop],
    ) -> Option<Vec<tiny_skia::GradientStop>> {
        if stops.is_empty() {
            return None;
        }
        let opacity = self.canvas.opacity().clamp(0.0, 1.0);
        Some(
            stops
                .iter()
                .map(|s| {
                    let alpha = (s.color.a * opacity * 255.0).round().clamp(0.0, 255.0) as u8;
                    SkiaGradientStop::new(
                        s.position,
                        tiny_skia::Color::from_rgba8(s.color.r, s.color.g, s.color.b, alpha),
                    )
                })
                .collect(),
        )
    }

    fn render_box_shadow(&mut self, item: &BoxShadowItem) {
        let mut shadow = BoxShadow {
            offset_x: self.ds_len(item.offset.x),
            offset_y: self.ds_len(item.offset.y),
            blur_radius: self.ds_len(item.blur_radius),
            spread_radius: self.ds_len(item.spread_radius),
            color: item.color,
            inset: item.inset,
        };

        let rect = self.ds_rect(item.rect);
        let radii = self.ds_radii(item.radii);

        let mut temp = match Pixmap::new(self.canvas.width(), self.canvas.height()) {
            Some(p) => p,
            None => return,
        };

        // Apply current opacity to the shadow when compositing.
        let opacity = self.canvas.opacity().clamp(0.0, 1.0);
        shadow.color.a *= opacity;

        let _ = render_box_shadow(
            &mut temp,
            rect.x(),
            rect.y(),
            rect.width(),
            rect.height(),
            &radii,
            &shadow,
        );

        let paint = tiny_skia::PixmapPaint {
            opacity: 1.0,
            blend_mode: self.canvas.blend_mode(),
            ..Default::default()
        };
        let clip = self.canvas.clip_mask().cloned();
        let transform = self.canvas.transform();
        self.canvas
            .pixmap_mut()
            .draw_pixmap(0, 0, temp.as_ref(), &paint, transform, clip.as_ref());
    }

    fn composite_hsl_layer(&mut self, layer: &Pixmap, opacity: f32, mode: BlendMode) -> Result<()> {
        let target = self.canvas.pixmap_mut();
        let width = target.width().min(layer.width()) as usize;
        let height = target.height().min(layer.height()) as usize;
        if width == 0 || height == 0 {
            return Ok(());
        }

        let src_pixels = layer.pixels();
        let src_stride = layer.width() as usize;
        let dst_stride = target.width() as usize;
        let dst_pixels = target.pixels_mut();
        let opacity = opacity.clamp(0.0, 1.0);

        for y in 0..height {
            let src_row = &src_pixels[y * src_stride..y * src_stride + width];
            let dst_row = &mut dst_pixels[y * dst_stride..y * dst_stride + width];
            for (src_px, dst_px) in src_row.iter().zip(dst_row.iter_mut()) {
                let raw_sa = src_px.alpha() as f32 / 255.0;
                if raw_sa == 0.0 || opacity == 0.0 {
                    continue;
                }
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
                *dst_px =
                    PremultipliedColorU8::from_rgba(r, g, b, out_a_u8).unwrap_or(PremultipliedColorU8::TRANSPARENT);
            }
        }

        Ok(())
    }

    /// Consumes the renderer and returns the painted pixmap.
    pub fn render(mut self, list: &DisplayList) -> Result<Pixmap> {
        for item in list.items() {
            self.render_item(item)?;
        }
        Ok(self.canvas.into_pixmap())
    }

    fn render_item(&mut self, item: &DisplayItem) -> Result<()> {
        match item {
            DisplayItem::FillRect(FillRectItem { rect, color }) => self.canvas.draw_rect(self.ds_rect(*rect), *color),
            DisplayItem::StrokeRect(StrokeRectItem {
                rect,
                color,
                width,
                blend_mode,
            }) => self
                .canvas
                .stroke_rect_with_blend(self.ds_rect(*rect), *color, self.ds_len(*width), *blend_mode),
            DisplayItem::FillRoundedRect(item) => {
                self.canvas
                    .draw_rounded_rect(self.ds_rect(item.rect), self.ds_radii(item.radii), item.color)
            }
            DisplayItem::StrokeRoundedRect(item) => self.canvas.stroke_rounded_rect(
                self.ds_rect(item.rect),
                self.ds_radii(item.radii),
                item.color,
                self.ds_len(item.width),
            ),
            DisplayItem::LinearGradient(item) => self.render_linear_gradient(item),
            DisplayItem::RadialGradient(item) => self.render_radial_gradient(item),
            DisplayItem::Border(item) => self.render_border(item),
            DisplayItem::TextDecoration(item) => {
                let scaled = self.scale_decoration_item(item);
                self.render_text_decoration(&scaled)?
            }
            DisplayItem::Text(item) => {
                let scaled = self.scale_text_item(item);
                self.render_text(&scaled)?
            }
            DisplayItem::Image(item) => self.render_image(item)?,
            DisplayItem::BoxShadow(item) => self.render_box_shadow(item),
            DisplayItem::PushStackingContext(item) => {
                let scaled_filters = self.ds_filters(&item.filters);
                let scaled_backdrop = self.ds_filters(&item.backdrop_filters);
                let bounds = self.ds_rect(item.bounds);
                let radii = self.ds_radii(item.radii);

                if !scaled_backdrop.is_empty() {
                    apply_backdrop_filters(self.canvas.pixmap_mut(), &bounds, &scaled_backdrop, radii, self.scale);
                }

                let needs_layer = item.is_isolated
                    || !matches!(item.mix_blend_mode, crate::paint::display_list::BlendMode::Normal)
                    || !scaled_filters.is_empty()
                    || !scaled_backdrop.is_empty();
                if needs_layer {
                    let blend = if item.is_isolated {
                        tiny_skia::BlendMode::SourceOver
                    } else {
                        map_blend_mode(item.mix_blend_mode)
                    };
                    self.canvas.push_layer_with_blend(1.0, Some(blend))?;
                } else {
                    self.canvas.save();
                }
                self.stacking_layers.push(StackingRecord {
                    needs_layer,
                    filters: scaled_filters,
                    radii,
                    mask_bounds: bounds,
                });

                if let Some(matrix) = item.transform {
                    let t = Transform::from_row(
                        matrix.a,
                        matrix.b,
                        matrix.c,
                        matrix.d,
                        self.ds_len(matrix.e),
                        self.ds_len(matrix.f),
                    );
                    let combined = self.canvas.transform().post_concat(t);
                    self.canvas.set_transform(combined);
                }
            }
            DisplayItem::PopStackingContext => {
                let record = self.stacking_layers.pop().unwrap_or(StackingRecord {
                    needs_layer: false,
                    filters: Vec::new(),
                    radii: BorderRadii::ZERO,
                    mask_bounds: Rect::ZERO,
                });
                if record.needs_layer {
                    if !record.filters.is_empty() {
                        apply_filters(self.canvas.pixmap_mut(), &record.filters, self.scale);
                    }
                    if !record.radii.is_zero() {
                        self.canvas.save();
                        self.canvas.set_clip_with_radii(record.mask_bounds, Some(record.radii));
                        self.canvas.pop_layer()?;
                        self.canvas.restore();
                    } else {
                        self.canvas.pop_layer()?;
                    }
                } else {
                    self.canvas.restore();
                }
            }
            DisplayItem::PushClip(clip) => self.push_clip(clip),
            DisplayItem::PopClip => self.pop_clip(),
            DisplayItem::PushOpacity(OpacityItem { opacity }) => {
                self.canvas.push_layer(*opacity)?;
            }
            DisplayItem::PopOpacity => {
                self.canvas.pop_layer()?;
            }
            DisplayItem::PushTransform(transform) => self.push_transform(transform),
            DisplayItem::PopTransform => {
                self.canvas.restore();
            }
            DisplayItem::PushBlendMode(mode) => {
                if matches!(
                    mode.mode,
                    BlendMode::Hue | BlendMode::Saturation | BlendMode::Color | BlendMode::Luminosity
                ) {
                    self.canvas.push_layer(1.0)?;
                    self.blend_stack.push(Some(mode.mode));
                } else {
                    self.canvas
                        .push_layer_with_blend(1.0, Some(map_blend_mode(mode.mode)))?;
                    self.blend_stack.push(None);
                }
            }
            DisplayItem::PopBlendMode => {
                let blend_mode = self.blend_stack.pop().flatten();
                if let Some(mode) = blend_mode {
                    let (layer, opacity, _) = self.canvas.pop_layer_raw()?;
                    self.composite_hsl_layer(&layer, opacity, mode)?;
                } else {
                    self.canvas.pop_layer()?;
                }
            }
        }

        Ok(())
    }

    fn render_text(&mut self, item: &TextItem) -> Result<()> {
        let Some(font) = self.resolve_font(item.font_id.as_ref()) else {
            return Err(RenderError::RasterizationFailed {
                reason: "Unable to resolve font for display list text".into(),
            }
            .into());
        };

        let face = match font.as_ttf_face() {
            Ok(f) => f,
            Err(_) => {
                return Err(RenderError::RasterizationFailed {
                    reason: "Unable to parse font face for text shadows".into(),
                }
                .into())
            }
        };

        let (paths, bounds) = self.glyph_paths(&face, item);
        if !item.shadows.is_empty() && !paths.is_empty() && bounds.is_valid() {
            self.render_text_shadows(&paths, &bounds, item);
        }

        let glyphs: Vec<GlyphPosition> = item
            .glyphs
            .iter()
            .map(|g| GlyphPosition {
                glyph_id: g.glyph_id,
                cluster: 0,
                advance: g.advance,
                advance_y: 0.0,
                offset_x: g.offset.x,
                offset_y: g.offset.y,
            })
            .collect();

        self.canvas.draw_text(
            item.origin,
            &glyphs,
            &font,
            item.font_size,
            item.color,
            item.synthetic_bold,
            item.synthetic_oblique,
        );
        if let Some(emphasis) = &item.emphasis {
            self.render_emphasis(emphasis)?;
        }
        Ok(())
    }

    fn render_text_decoration(&mut self, item: &crate::paint::display_list::TextDecorationItem) -> Result<()> {
        if item.decorations.is_empty() || item.line_width <= 0.0 {
            return Ok(());
        }

        let clip = self.canvas.clip_mask().cloned();
        let transform = self.canvas.transform();
        let blend_mode = self.canvas.blend_mode();
        let pixmap = self.canvas.pixmap_mut();

        let draw_solid_line =
            |pixmap: &mut Pixmap, paint: &tiny_skia::Paint, start: f32, len: f32, center: f32, thickness: f32| {
                if thickness <= 0.0 || len <= 0.0 {
                    return;
                }
                if let Some(rect) = tiny_skia::Rect::from_xywh(start, center - thickness * 0.5, len, thickness) {
                    let path = PathBuilder::from_rect(rect);
                    pixmap.fill_path(&path, paint, tiny_skia::FillRule::Winding, transform, clip.as_ref());
                }
            };

        let draw_stroked_line = |pixmap: &mut Pixmap,
                                 paint: &tiny_skia::Paint,
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

            pixmap.stroke_path(&path, paint, &stroke, transform, clip.as_ref());
        };

        let draw_wavy_line =
            |pixmap: &mut Pixmap, paint: &tiny_skia::Paint, start: f32, len: f32, center: f32, thickness: f32| {
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
                    pixmap.stroke_path(&path, paint, &stroke, transform, clip.as_ref());
                }
            };

        let render_line = |pixmap: &mut Pixmap,
                           paint: &tiny_skia::Paint,
                           style: TextDecorationStyle,
                           start: f32,
                           len: f32,
                           center: f32,
                           thickness: f32| match style {
            TextDecorationStyle::Solid => draw_solid_line(pixmap, paint, start, len, center, thickness),
            TextDecorationStyle::Double => {
                let line_thickness = (thickness * 0.7).max(0.5);
                let gap = line_thickness.max(thickness * 0.6);
                draw_solid_line(pixmap, paint, start, len, center - (gap * 0.5), line_thickness);
                draw_solid_line(pixmap, paint, start, len, center + (gap * 0.5), line_thickness);
            }
            TextDecorationStyle::Dotted => {
                draw_stroked_line(
                    pixmap,
                    paint,
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
                    paint,
                    start,
                    len,
                    center,
                    thickness,
                    Some(vec![3.0 * thickness, thickness]),
                    false,
                );
            }
            TextDecorationStyle::Wavy => draw_wavy_line(pixmap, paint, start, len, center, thickness),
        };

        for deco in &item.decorations {
            let mut paint = tiny_skia::Paint::default();
            paint.anti_alias = true;
            let alpha = (deco.color.a * 255.0).round().clamp(0.0, 255.0) as u8;
            paint.set_color_rgba8(deco.color.r, deco.color.g, deco.color.b, alpha);
            paint.blend_mode = blend_mode;

            if let Some(underline) = &deco.underline {
                if let Some(segments) = &underline.segments {
                    for (start, end) in segments {
                        let len = end - start;
                        if len <= 0.0 {
                            continue;
                        }
                        render_line(
                            pixmap,
                            &paint,
                            deco.style,
                            *start,
                            len,
                            underline.center,
                            underline.thickness,
                        );
                    }
                } else {
                    render_line(
                        pixmap,
                        &paint,
                        deco.style,
                        item.line_start,
                        item.line_width,
                        underline.center,
                        underline.thickness,
                    );
                }
            }
            if let Some(overline) = &deco.overline {
                render_line(
                    pixmap,
                    &paint,
                    deco.style,
                    item.line_start,
                    item.line_width,
                    overline.center,
                    overline.thickness,
                );
            }
            if let Some(strike) = &deco.line_through {
                render_line(
                    pixmap,
                    &paint,
                    deco.style,
                    item.line_start,
                    item.line_width,
                    strike.center,
                    strike.thickness,
                );
            }
        }

        Ok(())
    }

    fn glyph_paths(&self, face: &ttf_parser::Face<'_>, item: &TextItem) -> (Vec<tiny_skia::Path>, PathBounds) {
        let units_per_em = face.units_per_em() as f32;
        let scale = item.font_size / units_per_em;
        let mut paths = Vec::with_capacity(item.glyphs.len());
        let mut bounds = PathBounds::new();

        for glyph in &item.glyphs {
            let x = item.origin.x + glyph.offset.x;
            let y = item.origin.y + glyph.offset.y;
            if let Some(path) = Self::build_glyph_path(face, glyph.glyph_id as u16, x, y, scale, item.synthetic_oblique)
            {
                bounds.include(&path.bounds());
                paths.push(path);
            }
        }

        (paths, bounds)
    }

    fn build_glyph_path(
        face: &ttf_parser::Face<'_>,
        glyph_id: u16,
        x: f32,
        baseline_y: f32,
        scale: f32,
        synthetic_oblique: f32,
    ) -> Option<tiny_skia::Path> {
        use ttf_parser::OutlineBuilder;

        struct PathConverter {
            builder: PathBuilder,
            scale: f32,
            x: f32,
            y: f32,
            skew: f32,
        }

        impl OutlineBuilder for PathConverter {
            fn move_to(&mut self, px: f32, py: f32) {
                self.builder
                    .move_to(self.x + (px + self.skew * py) * self.scale, self.y - py * self.scale);
            }

            fn line_to(&mut self, px: f32, py: f32) {
                self.builder
                    .line_to(self.x + (px + self.skew * py) * self.scale, self.y - py * self.scale);
            }

            fn quad_to(&mut self, x1: f32, y1: f32, px: f32, py: f32) {
                self.builder.quad_to(
                    self.x + (x1 + self.skew * y1) * self.scale,
                    self.y - y1 * self.scale,
                    self.x + (px + self.skew * py) * self.scale,
                    self.y - py * self.scale,
                );
            }

            fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, px: f32, py: f32) {
                self.builder.cubic_to(
                    self.x + (x1 + self.skew * y1) * self.scale,
                    self.y - y1 * self.scale,
                    self.x + (x2 + self.skew * y2) * self.scale,
                    self.y - y2 * self.scale,
                    self.x + (px + self.skew * py) * self.scale,
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
            skew: synthetic_oblique,
        };

        face.outline_glyph(ttf_parser::GlyphId(glyph_id), &mut converter)?;
        converter.builder.finish()
    }

    fn render_text_shadows(&mut self, paths: &[tiny_skia::Path], bounds: &PathBounds, item: &TextItem) {
        let opacity = self.canvas.opacity().clamp(0.0, 1.0);
        for shadow in &item.shadows {
            let blur_margin = (shadow.blur_radius.abs() * 3.0).ceil();
            let shadow_min_x = bounds.min_x + shadow.offset.x - blur_margin;
            let shadow_max_x = bounds.max_x + shadow.offset.x + blur_margin;
            let shadow_min_y = bounds.min_y + shadow.offset.y - blur_margin;
            let shadow_max_y = bounds.max_y + shadow.offset.y + blur_margin;

            let shadow_width = (shadow_max_x - shadow_min_x).ceil().max(0.0) as u32;
            let shadow_height = (shadow_max_y - shadow_min_y).ceil().max(0.0) as u32;
            if shadow_width == 0 || shadow_height == 0 {
                continue;
            }

            let Some(mut shadow_pixmap) = Pixmap::new(shadow_width, shadow_height) else {
                continue;
            };

            let mut paint = tiny_skia::Paint::default();
            let alpha = (shadow.color.a * opacity).clamp(0.0, 1.0);
            paint.set_color_rgba8(
                shadow.color.r,
                shadow.color.g,
                shadow.color.b,
                (alpha * 255.0).round().clamp(0.0, 255.0) as u8,
            );
            paint.anti_alias = true;

            let translate_x = -bounds.min_x + blur_margin;
            let translate_y = -bounds.min_y + blur_margin;
            let transform = Transform::from_translate(translate_x, translate_y);
            for path in paths {
                shadow_pixmap.fill_path(path, &paint, tiny_skia::FillRule::EvenOdd, transform, None);
            }

            if shadow.blur_radius > 0.0 {
                apply_gaussian_blur(&mut shadow_pixmap, shadow.blur_radius);
            }

            let dest_x = shadow_min_x.floor() as i32;
            let dest_y = shadow_min_y.floor() as i32;
            let frac_x = shadow_min_x - dest_x as f32;
            let frac_y = shadow_min_y - dest_y as f32;
            let mut pixmap_paint = tiny_skia::PixmapPaint {
                opacity: 1.0,
                blend_mode: self.canvas.blend_mode(),
                ..Default::default()
            };
            pixmap_paint.opacity = opacity;
            let clip = self.canvas.clip_mask().cloned();
            let transform = Transform::from_translate(frac_x, frac_y).post_concat(self.canvas.transform());
            self.canvas.pixmap_mut().draw_pixmap(
                dest_x,
                dest_y,
                shadow_pixmap.as_ref(),
                &pixmap_paint,
                transform,
                clip.as_ref(),
            );
        }
    }

    fn render_emphasis(&mut self, emphasis: &TextEmphasis) -> Result<()> {
        if emphasis.marks.is_empty() {
            return Ok(());
        }

        if let TextEmphasisStyle::String(_) = emphasis.style {
            if let Some(text) = &emphasis.text {
                let font = self
                    .resolve_font(text.font_id.as_ref())
                    .ok_or(RenderError::RasterizationFailed {
                        reason: "Unable to resolve font for emphasis string".into(),
                    })?;
                let glyphs: Vec<GlyphPosition> = text
                    .glyphs
                    .iter()
                    .map(|g| GlyphPosition {
                        glyph_id: g.glyph_id,
                        cluster: 0,
                        advance: g.advance,
                        advance_y: 0.0,
                        offset_x: g.offset.x,
                        offset_y: g.offset.y,
                    })
                    .collect();
                for mark in &emphasis.marks {
                    let mark_origin = Point::new(
                        mark.center.x - text.width * 0.5,
                        mark.center.y - text.height * 0.5 + text.baseline_offset,
                    );
                    self.canvas
                        .draw_text(mark_origin, &glyphs, &font, text.font_size, emphasis.color, 0.0, 0.0);
                }
                return Ok(());
            }
        }

        let mut paint = tiny_skia::Paint::default();
        paint.anti_alias = true;
        let alpha = (emphasis.color.a * self.canvas.opacity() * 255.0)
            .round()
            .clamp(0.0, 255.0) as u8;
        paint.set_color_rgba8(emphasis.color.r, emphasis.color.g, emphasis.color.b, alpha);
        paint.blend_mode = self.canvas.blend_mode();
        let transform = self.canvas.transform();
        let clip = self.canvas.clip_mask().cloned();

        for mark in &emphasis.marks {
            match emphasis.style {
                TextEmphasisStyle::Mark {
                    fill,
                    shape: TextEmphasisShape::Dot,
                } => {
                    let radius = emphasis.size * 0.5;
                    if let Some(path) = tiny_skia::PathBuilder::from_circle(mark.center.x, mark.center.y, radius) {
                        match fill {
                            TextEmphasisFill::Filled => {
                                self.canvas.pixmap_mut().fill_path(
                                    &path,
                                    &paint,
                                    tiny_skia::FillRule::EvenOdd,
                                    transform,
                                    clip.as_ref(),
                                );
                            }
                            TextEmphasisFill::Open => {
                                let mut stroke = tiny_skia::Stroke::default();
                                stroke.width = (emphasis.size * 0.18).max(0.5);
                                self.canvas
                                    .pixmap_mut()
                                    .stroke_path(&path, &paint, &stroke, transform, clip.as_ref());
                            }
                        }
                    }
                }
                TextEmphasisStyle::Mark {
                    fill,
                    shape: TextEmphasisShape::Circle,
                } => {
                    let radius = emphasis.size * 0.5;
                    if let Some(path) = tiny_skia::PathBuilder::from_circle(mark.center.x, mark.center.y, radius) {
                        match fill {
                            TextEmphasisFill::Filled => {
                                self.canvas.pixmap_mut().fill_path(
                                    &path,
                                    &paint,
                                    tiny_skia::FillRule::EvenOdd,
                                    transform,
                                    clip.as_ref(),
                                );
                            }
                            TextEmphasisFill::Open => {
                                let mut stroke = tiny_skia::Stroke::default();
                                stroke.width = (emphasis.size * 0.18).max(0.5);
                                self.canvas
                                    .pixmap_mut()
                                    .stroke_path(&path, &paint, &stroke, transform, clip.as_ref());
                            }
                        }
                    }
                }
                TextEmphasisStyle::Mark {
                    fill: _,
                    shape: TextEmphasisShape::DoubleCircle,
                } => {
                    let mut stroke = tiny_skia::Stroke::default();
                    stroke.width = (emphasis.size * 0.14).max(0.5);
                    let radii = [emphasis.size * 0.5, emphasis.size * 0.33];
                    for radius in radii {
                        if let Some(path) = tiny_skia::PathBuilder::from_circle(mark.center.x, mark.center.y, radius) {
                            self.canvas
                                .pixmap_mut()
                                .stroke_path(&path, &paint, &stroke, transform, clip.as_ref());
                        }
                    }
                }
                TextEmphasisStyle::Mark {
                    fill,
                    shape: TextEmphasisShape::Triangle,
                } => {
                    let half = emphasis.size * 0.5;
                    let height = emphasis.size * 0.9;
                    let direction = matches!(
                        emphasis.position,
                        TextEmphasisPosition::Over | TextEmphasisPosition::OverLeft | TextEmphasisPosition::OverRight
                    );
                    let apex_y = if direction {
                        mark.center.y - height * 0.5
                    } else {
                        mark.center.y + height * 0.5
                    };
                    let base_y = if direction {
                        mark.center.y + height * 0.5
                    } else {
                        mark.center.y - height * 0.5
                    };
                    let mut builder = tiny_skia::PathBuilder::new();
                    builder.move_to(mark.center.x, apex_y);
                    builder.line_to(mark.center.x - half, base_y);
                    builder.line_to(mark.center.x + half, base_y);
                    builder.close();
                    if let Some(path) = builder.finish() {
                        match fill {
                            TextEmphasisFill::Filled => {
                                self.canvas.pixmap_mut().fill_path(
                                    &path,
                                    &paint,
                                    tiny_skia::FillRule::EvenOdd,
                                    transform,
                                    clip.as_ref(),
                                );
                            }
                            TextEmphasisFill::Open => {
                                let mut stroke = tiny_skia::Stroke::default();
                                stroke.width = (emphasis.size * 0.18).max(0.5);
                                self.canvas
                                    .pixmap_mut()
                                    .stroke_path(&path, &paint, &stroke, transform, clip.as_ref());
                            }
                        }
                    }
                }
                TextEmphasisStyle::Mark {
                    fill: _,
                    shape: TextEmphasisShape::Sesame,
                } => {
                    let len = emphasis.size * 0.75;
                    let angle = 20.0_f32.to_radians();
                    let dx = (angle.cos() * len * 0.5, angle.sin() * len * 0.5);
                    let mut builder = tiny_skia::PathBuilder::new();
                    builder.move_to(mark.center.x - dx.0, mark.center.y - dx.1);
                    builder.line_to(mark.center.x + dx.0, mark.center.y + dx.1);
                    if let Some(path) = builder.finish() {
                        let mut stroke = tiny_skia::Stroke::default();
                        stroke.width = (emphasis.size * 0.2).max(0.6);
                        stroke.line_cap = tiny_skia::LineCap::Round;
                        self.canvas
                            .pixmap_mut()
                            .stroke_path(&path, &paint, &stroke, transform, clip.as_ref());
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn render_image(&mut self, item: &ImageItem) -> Result<()> {
        let dest_rect = self.ds_rect(item.dest_rect);
        if let Some(clip) = self.canvas.clip_bounds() {
            if clip.width() <= 0.0 || clip.height() <= 0.0 || clip.intersection(dest_rect).is_none() {
                return Ok(());
            }
        }

        let Some(pixmap) = self.image_to_pixmap(item) else {
            return Ok(());
        };

        if dest_rect.width() <= 0.0 || dest_rect.height() <= 0.0 {
            return Ok(());
        }

        let paint = tiny_skia::PixmapPaint {
            opacity: self.canvas.opacity(),
            blend_mode: self.canvas.blend_mode(),
            quality: item.filter_quality.into(),
            ..Default::default()
        };

        let scale_x = dest_rect.width() / pixmap.width() as f32;
        let scale_y = dest_rect.height() / pixmap.height() as f32;

        let transform = Transform::from_row(scale_x, 0.0, 0.0, scale_y, dest_rect.x(), dest_rect.y())
            .post_concat(self.canvas.transform());
        let clip_mask = self.canvas.clip_mask().cloned();
        self.canvas
            .pixmap_mut()
            .draw_pixmap(0, 0, pixmap.as_ref(), &paint, transform, clip_mask.as_ref());

        Ok(())
    }

    fn push_clip(&mut self, clip: &ClipItem) {
        self.canvas.save();
        let radii = clip.radii.map(|r| self.ds_radii(r));
        self.canvas.set_clip_with_radii(self.ds_rect(clip.rect), radii);
    }

    fn pop_clip(&mut self) {
        self.canvas.restore();
    }

    fn push_transform(&mut self, transform: &TransformItem) {
        self.canvas.save();
        let matrix = Transform::from_row(
            transform.transform.a,
            transform.transform.b,
            transform.transform.c,
            transform.transform.d,
            transform.transform.e * self.scale,
            transform.transform.f * self.scale,
        );
        let combined = self.canvas.transform().post_concat(matrix);
        self.canvas.set_transform(combined);
    }

    fn resolve_font(&self, font_id: Option<&FontId>) -> Option<LoadedFont> {
        let mut families = Vec::new();
        let mut weight = 400;
        let mut italic = false;
        let mut oblique = false;
        let mut stretch = FontStretch::Normal;

        if let Some(id) = font_id {
            families.push(id.family.clone());
            weight = id.weight;
            italic = matches!(id.style, DbFontStyle::Italic);
            oblique = matches!(id.style, DbFontStyle::Oblique);
            stretch = id.stretch;
        }

        if families.is_empty() {
            families.push("sans-serif".to_string());
        }

        self.font_ctx
            .get_font_full(
                &families,
                weight,
                if italic {
                    DbFontStyle::Italic
                } else if oblique {
                    DbFontStyle::Oblique
                } else {
                    DbFontStyle::Normal
                },
                stretch,
            )
            .or_else(|| self.font_ctx.get_sans_serif())
    }

    fn image_to_pixmap(&self, item: &ImageItem) -> Option<Pixmap> {
        let mut data = Vec::with_capacity((item.image.width * item.image.height * 4) as usize);
        let pixels = item.image.pixels.as_ref();
        for chunk in pixels.chunks_exact(4) {
            let r = chunk[0] as f32;
            let g = chunk[1] as f32;
            let b = chunk[2] as f32;
            let a = chunk[3] as f32 / 255.0;
            data.push((r * a).round() as u8);
            data.push((g * a).round() as u8);
            data.push((b * a).round() as u8);
            data.push(chunk[3]);
        }

        let size = tiny_skia::IntSize::from_wh(item.image.width, item.image.height)?;
        let full = Pixmap::from_vec(data, size)?;

        if let Some(src) = item.src_rect {
            let src_x = src.x().max(0.0).floor() as u32;
            let src_y = src.y().max(0.0).floor() as u32;
            let src_w = src.width().ceil() as u32;
            let src_h = src.height().ceil() as u32;
            if src_w == 0 || src_h == 0 {
                return None;
            }
            let max_x = item.image.width.saturating_sub(src_x);
            let max_y = item.image.height.saturating_sub(src_y);
            let crop_w = src_w.min(max_x);
            let crop_h = src_h.min(max_y);
            if crop_w == 0 || crop_h == 0 {
                return None;
            }
            let mut cropped = Pixmap::new(crop_w, crop_h)?;
            for row in 0..crop_h {
                let src_index = ((src_y + row) * item.image.width + src_x) as usize * 4;
                let dst_index = (row * crop_w) as usize * 4;
                cropped.data_mut()[dst_index..dst_index + (crop_w as usize * 4)]
                    .copy_from_slice(&full.data()[src_index..src_index + (crop_w as usize * 4)]);
            }
            Some(cropped)
        } else {
            Some(full)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::{Point, Rect};
    use crate::paint::display_list::{
        BoxShadowItem, DecorationPaint, DecorationStroke, DisplayItem, DisplayList, FillRectItem, GlyphInstance,
        GradientSpread, GradientStop, ImageData, ImageFilterQuality, ImageItem, LinearGradientItem, OpacityItem,
        RadialGradientItem, TextDecorationItem, TextEmphasis, TextItem, TextShadowItem, Transform2D,
    };
    use crate::style::color::Rgba;
    use crate::style::types::{TextEmphasisFill, TextEmphasisPosition, TextEmphasisShape, TextEmphasisStyle};
    use std::sync::Arc;

    fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
        let idx = ((y * pixmap.width() + x) * 4) as usize;
        let data = pixmap.data();
        (data[idx], data[idx + 1], data[idx + 2], data[idx + 3])
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
                if predicate(pixel(pixmap, x, y)) {
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
    fn transform_push_is_scoped() {
        let renderer = DisplayListRenderer::new(20, 10, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        list.push(DisplayItem::PushTransform(TransformItem {
            transform: Transform2D {
                a: 1.0,
                b: 0.0,
                c: 0.0,
                d: 1.0,
                e: 10.0,
                f: 0.0,
            },
        }));
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
            color: Rgba::rgb(255, 0, 0),
        }));
        list.push(DisplayItem::PopTransform);
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
            color: Rgba::rgb(0, 255, 0),
        }));

        let pixmap = renderer.render(&list).unwrap();
        assert_eq!(pixel(&pixmap, 10, 0), (255, 0, 0, 255));
        assert_eq!(pixel(&pixmap, 0, 0), (0, 255, 0, 255));
    }

    #[test]
    fn image_filter_quality_respects_sampling_mode() {
        let pixels = vec![
            255, 0, 0, 255, // red
            0, 0, 255, 255, // blue
        ];
        let image = Arc::new(ImageData::new(2, 1, pixels));

        let render_with_quality = |quality: ImageFilterQuality| {
            let mut list = DisplayList::new();
            list.push(DisplayItem::Image(ImageItem {
                dest_rect: Rect::from_xywh(0.0, 0.0, 4.0, 1.0),
                image: image.clone(),
                filter_quality: quality,
                src_rect: None,
            }));
            DisplayListRenderer::new(4, 1, Rgba::WHITE, FontContext::new())
                .unwrap()
                .render(&list)
                .unwrap()
        };

        let nearest = render_with_quality(ImageFilterQuality::Nearest);
        let linear = render_with_quality(ImageFilterQuality::Linear);

        assert_eq!(pixel(&nearest, 0, 0), (255, 0, 0, 255));
        assert_eq!(pixel(&nearest, 1, 0), (255, 0, 0, 255));

        let blended = pixel(&linear, 1, 0);
        assert!(
            blended.0 < 255 && blended.2 < 255,
            "expected interpolation with linear sampling"
        );
    }

    #[test]
    fn image_src_rect_is_cropped_before_scaling() {
        let pixels = vec![
            255, 0, 0, 255, // red
            0, 255, 0, 255, // green
            0, 0, 255, 255, // blue
            255, 255, 0, 255, // yellow
        ];
        let image = Arc::new(ImageData::new(2, 2, pixels));

        let mut list = DisplayList::new();
        list.push(DisplayItem::Image(ImageItem {
            dest_rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
            image: image.clone(),
            filter_quality: ImageFilterQuality::Nearest,
            src_rect: Some(Rect::from_xywh(1.0, 0.0, 1.0, 2.0)), // right column (green/yellow)
        }));

        let pixmap = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new())
            .unwrap()
            .render(&list)
            .unwrap();

        // Top-left pixel should come from the cropped source's first row: green.
        assert_eq!(pixel(&pixmap, 0, 0), (0, 255, 0, 255));
        // Bottom-left pixel should come from yellow.
        assert_eq!(pixel(&pixmap, 0, 3), (255, 255, 0, 255));
    }

    #[test]
    fn renders_text_decoration_items() {
        let mut list = DisplayList::new();
        list.push(DisplayItem::TextDecoration(TextDecorationItem {
            bounds: Rect::from_xywh(0.0, 0.0, 20.0, 10.0),
            line_start: 2.0,
            line_width: 16.0,
            decorations: vec![DecorationPaint {
                style: TextDecorationStyle::Solid,
                color: Rgba::from_rgba8(255, 0, 0, 255),
                underline: Some(DecorationStroke {
                    center: 5.0,
                    thickness: 2.0,
                    segments: None,
                }),
                overline: None,
                line_through: None,
            }],
        }));

        let pixmap = DisplayListRenderer::new(20, 10, Rgba::WHITE, FontContext::new())
            .expect("renderer")
            .render(&list)
            .expect("rendered");
        assert_eq!(pixel(&pixmap, 10, 5), (255, 0, 0, 255));
    }

    #[test]
    fn text_decoration_segments_respect_gaps() {
        let mut list = DisplayList::new();
        list.push(DisplayItem::TextDecoration(TextDecorationItem {
            bounds: Rect::from_xywh(0.0, 0.0, 20.0, 10.0),
            line_start: 0.0,
            line_width: 20.0,
            decorations: vec![DecorationPaint {
                style: TextDecorationStyle::Solid,
                color: Rgba::from_rgba8(0, 0, 255, 255),
                underline: Some(DecorationStroke {
                    center: 5.0,
                    thickness: 2.0,
                    segments: Some(vec![(0.0, 6.0), (14.0, 20.0)]),
                }),
                overline: None,
                line_through: None,
            }],
        }));

        let pixmap = DisplayListRenderer::new(20, 10, Rgba::WHITE, FontContext::new())
            .expect("renderer")
            .render(&list)
            .expect("rendered");
        // Painted segments should hit inside and right span.
        assert_eq!(pixel(&pixmap, 2, 5), (0, 0, 255, 255));
        assert_eq!(pixel(&pixmap, 16, 5), (0, 0, 255, 255));
        // Gap should remain white.
        assert_eq!(pixel(&pixmap, 10, 5), (255, 255, 255, 255));
    }

    #[test]
    fn renders_linear_gradient() {
        let renderer = DisplayListRenderer::new(2, 1, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        list.push(DisplayItem::LinearGradient(LinearGradientItem {
            rect: Rect::from_xywh(0.0, 0.0, 2.0, 1.0),
            start: Point::new(0.0, 0.0),
            end: Point::new(2.0, 0.0),
            spread: GradientSpread::Pad,
            stops: vec![
                GradientStop {
                    position: 0.0,
                    color: Rgba::rgb(255, 0, 0),
                },
                GradientStop {
                    position: 1.0,
                    color: Rgba::rgb(0, 0, 255),
                },
            ],
        }));

        let pixmap = renderer.render(&list).unwrap();
        let first = pixel(&pixmap, 0, 0);
        let second = pixel(&pixmap, 1, 0);
        assert!(first.0 > first.2);
        assert!(second.2 > second.0);
        assert_eq!(first.3, 255);
        assert_eq!(second.3, 255);
    }

    #[test]
    fn renders_repeating_linear_gradient() {
        let renderer = DisplayListRenderer::new(4, 1, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        list.push(DisplayItem::LinearGradient(LinearGradientItem {
            rect: Rect::from_xywh(0.0, 0.0, 4.0, 1.0),
            start: Point::new(0.0, 0.0),
            end: Point::new(1.0, 0.0),
            spread: GradientSpread::Repeat,
            stops: vec![
                GradientStop {
                    position: 0.0,
                    color: Rgba::rgb(255, 0, 0),
                },
                GradientStop {
                    position: 1.0,
                    color: Rgba::rgb(0, 0, 255),
                },
            ],
        }));

        let pixmap = renderer.render(&list).unwrap();
        let right = pixel(&pixmap, 3, 0);
        assert!(right.0 > 0, "repeat spread should reintroduce the start color");
        assert!(right.2 > 0);
        assert_eq!(right.3, 255);
    }

    #[test]
    fn renders_radial_gradient_center_color() {
        let renderer = DisplayListRenderer::new(3, 3, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        list.push(DisplayItem::RadialGradient(RadialGradientItem {
            rect: Rect::from_xywh(0.0, 0.0, 3.0, 3.0),
            center: Point::new(1.5, 1.5),
            radius: 2.0,
            spread: GradientSpread::Pad,
            stops: vec![
                GradientStop {
                    position: 0.0,
                    color: Rgba::rgb(0, 255, 0),
                },
                GradientStop {
                    position: 1.0,
                    color: Rgba::rgb(0, 0, 0),
                },
            ],
        }));

        let pixmap = renderer.render(&list).unwrap();
        assert_eq!(pixel(&pixmap, 1, 1).1, 255);
    }

    #[test]
    fn renders_box_shadow() {
        let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        list.push(DisplayItem::BoxShadow(BoxShadowItem {
            rect: Rect::from_xywh(2.0, 2.0, 4.0, 4.0),
            radii: crate::paint::display_list::BorderRadii::uniform(0.0),
            offset: Point::new(0.0, 0.0),
            blur_radius: 0.0,
            spread_radius: 0.0,
            color: Rgba::new(0, 0, 0, 0.5),
            inset: false,
        }));

        let pixmap = renderer.render(&list).unwrap();
        // Center of the box should have darkened pixels due to shadow.
        assert!(pixel(&pixmap, 3, 3).3 > 200);
    }

    #[test]
    fn renders_text_shadows() {
        let font_ctx = FontContext::new();
        let Some(font) = font_ctx.get_sans_serif() else {
            return;
        };
        let Ok(face) = font.as_ttf_face() else {
            return;
        };
        let Some(glyph_id) = face.glyph_index('H') else {
            return;
        };

        let mut list = DisplayList::new();
        list.push(DisplayItem::Text(TextItem {
            origin: Point::new(10.0, 24.0),
            glyphs: vec![GlyphInstance {
                glyph_id: glyph_id.0 as u32,
                offset: Point::new(0.0, 0.0),
                advance: 14.0,
            }],
            color: Rgba::BLACK,
            shadows: vec![TextShadowItem {
                offset: Point::new(4.0, 0.0),
                blur_radius: 0.0,
                color: Rgba::from_rgba8(255, 0, 0, 255),
            }],
            font_size: 20.0,
            advance_width: 14.0,
            font_id: Some(FontId {
                family: font.family.clone(),
                weight: font.weight.value(),
                style: font.style,
                stretch: font.stretch,
            }),
            synthetic_bold: 0.0,
            synthetic_oblique: 0.0,
            emphasis: None,
            decorations: Vec::new(),
        }));

        let renderer = DisplayListRenderer::new(80, 40, Rgba::WHITE, font_ctx).expect("renderer");
        let pixmap = renderer.render(&list).expect("rendered");

        let black_bbox =
            bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32).expect("text pixels");
        let red_bbox =
            bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80).expect("shadow");

        assert!(red_bbox.0 > black_bbox.0 + 1, "shadow should be offset to the right");
        assert!(
            red_bbox.1.abs_diff(black_bbox.1) <= 2,
            "shadow should align vertically when y-offset is zero"
        );
    }

    #[test]
    fn renders_text_emphasis_marks() {
        let mut list = DisplayList::new();
        list.push(DisplayItem::Text(TextItem {
            origin: Point::new(20.0, 30.0),
            glyphs: Vec::new(),
            color: Rgba::BLACK,
            shadows: vec![],
            font_size: 16.0,
            advance_width: 0.0,
            font_id: None,
            synthetic_bold: 0.0,
            synthetic_oblique: 0.0,
            emphasis: Some(TextEmphasis {
                style: TextEmphasisStyle::Mark {
                    fill: TextEmphasisFill::Filled,
                    shape: TextEmphasisShape::Circle,
                },
                color: Rgba::from_rgba8(255, 0, 0, 255),
                position: TextEmphasisPosition::Over,
                size: 6.0,
                marks: vec![crate::paint::display_list::EmphasisMark {
                    center: Point::new(20.0, 12.0),
                }],
                text: None,
            }),
            decorations: Vec::new(),
        }));

        let renderer = DisplayListRenderer::new(40, 40, Rgba::WHITE, FontContext::new()).expect("renderer");
        let pixmap = renderer.render(&list).expect("rendered");

        let red_pixel = pixel(&pixmap, 20, 12);
        assert!(
            red_pixel.0 > 200 && red_pixel.1 < 80 && red_pixel.2 < 80,
            "emphasis mark should paint at the provided center"
        );
        let below_pixel = pixel(&pixmap, 20, 30);
        assert_eq!(
            below_pixel,
            (255, 255, 255, 255),
            "baseline area should remain untouched when no text glyphs are present"
        );
    }

    #[test]
    fn stacking_context_restores_clip() {
        let renderer = DisplayListRenderer::new(6, 6, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        // Outer clip to a 4x4 square.
        list.push(DisplayItem::PushClip(ClipItem {
            rect: Rect::from_xywh(1.0, 1.0, 4.0, 4.0),
            radii: None,
        }));
        // Start stacking context and narrow the clip further.
        list.push(DisplayItem::PushStackingContext(
            crate::paint::display_list::StackingContextItem {
                z_index: 0,
                creates_stacking_context: true,
                bounds: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
                mix_blend_mode: crate::paint::display_list::BlendMode::Normal,
                is_isolated: false,
                transform: None,
                filters: Vec::new(),
                backdrop_filters: Vec::new(),
                radii: BorderRadii::ZERO,
            },
        ));
        list.push(DisplayItem::PushClip(ClipItem {
            rect: Rect::from_xywh(2.0, 2.0, 2.0, 2.0),
            radii: None,
        }));
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
            color: Rgba::rgb(255, 0, 0),
        }));
        list.push(DisplayItem::PopClip);
        list.push(DisplayItem::PopStackingContext);
        // After popping stacking context, outer clip should remain.
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
            color: Rgba::rgb(0, 0, 255),
        }));
        list.push(DisplayItem::PopClip);

        let pixmap = renderer.render(&list).unwrap();
        // Pixel inside outer clip but outside inner clip should be blue (second fill) not red.
        assert_eq!(pixel(&pixmap, 1, 1), (0, 0, 255, 255));
    }

    #[test]
    fn stacking_context_blends_as_group() {
        let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        list.push(DisplayItem::PushStackingContext(
            crate::paint::display_list::StackingContextItem {
                z_index: 0,
                creates_stacking_context: true,
                bounds: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
                mix_blend_mode: crate::paint::display_list::BlendMode::Multiply,
                is_isolated: false,
                transform: None,
                filters: Vec::new(),
                backdrop_filters: Vec::new(),
                radii: BorderRadii::ZERO,
            },
        ));
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
            color: Rgba::rgb(255, 0, 0),
        }));
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
            color: Rgba::rgb(0, 255, 0),
        }));
        list.push(DisplayItem::PopStackingContext);

        let pixmap = renderer.render(&list).unwrap();
        // Without grouping, the second rect would multiply against the first and go black.
        assert_eq!(pixel(&pixmap, 0, 0), (0, 255, 0, 255));
    }

    #[test]
    fn opacity_groups_composite_as_layers() {
        let renderer = DisplayListRenderer::new(8, 8, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 0.5 }));
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
            color: Rgba::rgb(255, 0, 0),
        }));
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(3.0, 3.0, 6.0, 6.0),
            color: Rgba::rgb(255, 0, 0),
        }));
        list.push(DisplayItem::PopOpacity);

        let pixmap = renderer.render(&list).unwrap();
        let single = pixel(&pixmap, 1, 1);
        let overlap = pixel(&pixmap, 4, 4);

        assert_eq!(single, overlap, "opacity should apply once across the group");
        assert_eq!(single, (255, 128, 128, 255));
    }
}
