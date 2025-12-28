//! Display list renderer
//!
//! Executes a `DisplayList` into a pixel buffer using the existing canvas/text
//! rasterization utilities. This path keeps the display list as the
//! paint-time contract while still reusing the shared `FontContext`.

use crate::css::types::ColorStop;
use crate::css::types::RadialGradientShape;
use crate::css::types::RadialGradientSize;
use crate::error::RenderError;
use crate::error::Result;
use crate::geometry::Point;
use crate::geometry::Rect;
use crate::paint::blur::apply_gaussian_blur;
use crate::paint::canvas::crop_mask;
use crate::paint::canvas::Canvas;
use crate::paint::display_list::BlendMode;
use crate::paint::display_list::BorderImageItem;
use crate::paint::display_list::BorderImageSourceItem;
use crate::paint::display_list::BorderItem;
use crate::paint::display_list::BorderRadii;
use crate::paint::display_list::BorderSide;
use crate::paint::display_list::BoxShadowItem;
use crate::paint::display_list::ClipItem;
use crate::paint::display_list::ClipShape;
use crate::paint::display_list::ConicGradientItem;
use crate::paint::display_list::DecorationPaint;
use crate::paint::display_list::DecorationStroke;
use crate::paint::display_list::DisplayItem;
use crate::paint::display_list::DisplayList;
use crate::paint::display_list::EmphasisMark;
use crate::paint::display_list::FillRectItem;
use crate::paint::display_list::FontId;
use crate::paint::display_list::GlyphInstance;
use crate::paint::display_list::ImageData;
use crate::paint::display_list::ImageFilterQuality;
use crate::paint::display_list::ImageItem;
use crate::paint::display_list::LinearGradientItem;
use crate::paint::display_list::ListMarkerItem;
#[cfg(test)]
use crate::paint::display_list::MaskReferenceRects;
use crate::paint::display_list::OpacityItem;
use crate::paint::display_list::OutlineItem;
use crate::paint::display_list::RadialGradientItem;
use crate::paint::display_list::ResolvedFilter;
use crate::paint::display_list::ResolvedMask;
use crate::paint::display_list::ResolvedMaskImage;
use crate::paint::display_list::StackingContextItem;
use crate::paint::display_list::StrokeRectItem;
use crate::paint::display_list::TextEmphasis;
use crate::paint::display_list::TextItem;
use crate::paint::display_list::TextShadowItem;
use crate::paint::display_list::Transform3D;
use crate::paint::display_list::TransformItem;
use crate::paint::filter_outset::filter_outset;
use crate::paint::filter_outset::filter_outset_with_bounds;
use crate::paint::homography::Homography;
use crate::paint::projective_warp::warp_pixmap;
use crate::paint::rasterize::fill_rounded_rect;
use crate::paint::rasterize::render_box_shadow;
use crate::paint::rasterize::BoxShadow;
use crate::paint::text_rasterize::TextRasterizer;
use crate::paint::text_shadow::PathBounds;
use crate::paint::transform3d::backface_is_hidden;
use crate::style::color::Rgba;
use crate::style::types::BackfaceVisibility;
use crate::style::types::BackgroundImage;
use crate::style::types::BackgroundPosition;
use crate::style::types::BackgroundRepeatKeyword;
use crate::style::types::BackgroundSize;
use crate::style::types::BackgroundSizeComponent;
use crate::style::types::BackgroundSizeKeyword;
use crate::style::types::BorderImageOutsetValue;
use crate::style::types::BorderImageRepeat;
use crate::style::types::BorderImageSliceValue;
use crate::style::types::BorderImageWidth;
use crate::style::types::BorderImageWidthValue;
use crate::style::types::BorderStyle as CssBorderStyle;
use crate::style::types::MaskClip;
use crate::style::types::MaskComposite;
use crate::style::types::MaskMode;
use crate::style::types::MaskOrigin;
use crate::style::types::TextDecorationStyle;
use crate::style::types::TextEmphasisFill;
use crate::style::types::TextEmphasisPosition;
use crate::style::types::TextEmphasisShape;
use crate::style::types::TextEmphasisStyle;
use crate::style::types::TransformStyle;
use crate::style::values::Length;
#[cfg(test)]
use crate::style::ComputedStyle;
use crate::text::color_fonts::ColorGlyphRaster;
use crate::text::font_db::FontStretch;
use crate::text::font_db::FontStyle as DbFontStyle;
use crate::text::font_db::LoadedFont;
use crate::text::font_loader::FontContext;
use crate::text::pipeline::GlyphPosition;
use rayon::prelude::*;
use tiny_skia::BlendMode as SkiaBlendMode;
use tiny_skia::GradientStop as SkiaGradientStop;
use tiny_skia::IntSize;
use tiny_skia::LinearGradient;
use tiny_skia::Mask;
use tiny_skia::MaskType;
use tiny_skia::PathBuilder;
use tiny_skia::Pattern;
use tiny_skia::Pixmap;
use tiny_skia::PixmapPaint;
use tiny_skia::Point as SkiaPoint;
use tiny_skia::PremultipliedColorU8;
use tiny_skia::RadialGradient;
use tiny_skia::SpreadMode;
use tiny_skia::Stroke;
use tiny_skia::StrokeDash;
use tiny_skia::Transform;

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
    BlendMode::PlusLighter => tiny_skia::BlendMode::Plus,
    BlendMode::PlusDarker => tiny_skia::BlendMode::Darken,
    BlendMode::HueHsv => tiny_skia::BlendMode::Hue,
    BlendMode::SaturationHsv => tiny_skia::BlendMode::Saturation,
    BlendMode::ColorHsv => tiny_skia::BlendMode::Color,
    BlendMode::LuminosityHsv => tiny_skia::BlendMode::Luminosity,
    BlendMode::HueOklch => tiny_skia::BlendMode::Hue,
    BlendMode::ChromaOklch => tiny_skia::BlendMode::Saturation,
    BlendMode::ColorOklch => tiny_skia::BlendMode::Color,
    BlendMode::LuminosityOklch => tiny_skia::BlendMode::Luminosity,
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

fn shade_color(color: &Rgba, factor: f32) -> Rgba {
  let clamp = |v: f32| v.clamp(0.0, 255.0) as u8;
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
  let q = if l < 0.5 {
    l * (1.0 + s)
  } else {
    l + s - l * s
  };
  let p = 2.0 * l - q;
  let r = hue_to_rgb(p, q, h + 1.0 / 3.0);
  let g = hue_to_rgb(p, q, h);
  let b = hue_to_rgb(p, q, h - 1.0 / 3.0);
  (r, g, b)
}

fn rgb_to_hsv(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  let max = r.max(g).max(b);
  let min = r.min(g).min(b);
  let v = max;
  let d = max - min;
  if d.abs() < f32::EPSILON {
    return (0.0, 0.0, v);
  }
  let s = if max > 0.0 { d / max } else { 0.0 };
  let h = if (max - r).abs() < f32::EPSILON {
    (g - b) / d + if g < b { 6.0 } else { 0.0 }
  } else if (max - g).abs() < f32::EPSILON {
    (b - r) / d + 2.0
  } else {
    (r - g) / d + 4.0
  } / 6.0;
  (h, s, v)
}

fn hsv_to_rgb(h: f32, s: f32, v: f32) -> (f32, f32, f32) {
  if s <= 0.0 {
    return (v, v, v);
  }
  let hh = (h * 6.0) % 6.0;
  let i = hh.floor();
  let f = hh - i;
  let p = v * (1.0 - s);
  let q = v * (1.0 - s * f);
  let t = v * (1.0 - s * (1.0 - f));
  match i as i32 {
    0 => (v, t, p),
    1 => (q, v, p),
    2 => (p, v, t),
    3 => (p, q, v),
    4 => (t, p, v),
    _ => (v, p, q),
  }
}

fn srgb_to_linear(v: f32) -> f32 {
  if v <= 0.04045 {
    v / 12.92
  } else {
    ((v + 0.055) / 1.055).powf(2.4)
  }
}

fn linear_to_srgb(v: f32) -> f32 {
  if v <= 0.0031308 {
    v * 12.92
  } else {
    1.055 * v.powf(1.0 / 2.4) - 0.055
  }
}

fn linear_srgb_to_oklab(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  let l = 0.412_221_46 * r + 0.536_332_54 * g + 0.051_445_996 * b;
  let m = 0.211_903_5 * r + 0.680_699_5 * g + 0.107_396_96 * b;
  let s = 0.088_302_46 * r + 0.281_718_85 * g + 0.629_978_7 * b;

  let l_ = l.cbrt();
  let m_ = m.cbrt();
  let s_ = s.cbrt();

  let l_ok = 0.210_454_26 * l_ + 0.793_617_8 * m_ - 0.004_072_047 * s_;
  let a_ok = 1.977_998_5 * l_ - 2.428_592_2 * m_ + 0.450_593_7 * s_;
  let b_ok = 0.025_904_037 * l_ + 0.782_771_77 * m_ - 0.808_675_77 * s_;
  (l_ok, a_ok, b_ok)
}

fn oklab_to_linear_srgb(l: f32, a: f32, b: f32) -> (f32, f32, f32) {
  let l_ = l + 0.396_337_78 * a + 0.215_803_76 * b;
  let m_ = l - 0.105_561_35 * a - 0.063_854_17 * b;
  let s_ = l - 0.089_484_18 * a - 1.291_485_5 * b;

  let l3 = l_.powf(3.0);
  let m3 = m_.powf(3.0);
  let s3 = s_.powf(3.0);

  let r = 4.076_741_7 * l3 - 3.307_711_6 * m3 + 0.230_969_94 * s3;
  let g = -1.268_438_ * l3 + 2.609_757_4 * m3 - 0.341_319_38 * s3;
  let b = 0.004_421_97 * l3 - 0.703_418_6 * m3 + 1.698_594_8 * s3;
  (r, g, b)
}

fn rgb_to_oklch(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  let (l, a, bb) = linear_srgb_to_oklab(srgb_to_linear(r), srgb_to_linear(g), srgb_to_linear(b));
  let c = (a * a + bb * bb).sqrt();
  let h = bb.atan2(a).to_degrees().rem_euclid(360.0);
  (l, c, h)
}

fn oklch_to_rgb(l: f32, c: f32, h: f32) -> (f32, f32, f32) {
  let hr = h.to_radians();
  let a = c * hr.cos();
  let b = c * hr.sin();
  let (r_lin, g_lin, b_lin) = oklab_to_linear_srgb(l, a, b);
  (
    linear_to_srgb(r_lin).clamp(0.0, 1.0),
    linear_to_srgb(g_lin).clamp(0.0, 1.0),
    linear_to_srgb(b_lin).clamp(0.0, 1.0),
  )
}

fn is_manual_blend(mode: BlendMode) -> bool {
  matches!(
    mode,
    BlendMode::Hue
      | BlendMode::Saturation
      | BlendMode::Color
      | BlendMode::Luminosity
      | BlendMode::HueHsv
      | BlendMode::SaturationHsv
      | BlendMode::ColorHsv
      | BlendMode::LuminosityHsv
      | BlendMode::HueOklch
      | BlendMode::ChromaOklch
      | BlendMode::ColorOklch
      | BlendMode::LuminosityOklch
      | BlendMode::PlusDarker
  )
}

fn apply_manual_blend(
  mode: BlendMode,
  src: (f32, f32, f32),
  dst: (f32, f32, f32),
) -> (f32, f32, f32) {
  match mode {
    BlendMode::Hue | BlendMode::Saturation | BlendMode::Color | BlendMode::Luminosity => {
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
    BlendMode::HueHsv
    | BlendMode::SaturationHsv
    | BlendMode::ColorHsv
    | BlendMode::LuminosityHsv => {
      let (sh, ss, sv) = rgb_to_hsv(src.0, src.1, src.2);
      let (dh, ds, dv) = rgb_to_hsv(dst.0, dst.1, dst.2);
      match mode {
        BlendMode::HueHsv => hsv_to_rgb(sh, ds, dv),
        BlendMode::SaturationHsv => hsv_to_rgb(dh, ss, dv),
        BlendMode::ColorHsv => hsv_to_rgb(sh, ss, dv),
        BlendMode::LuminosityHsv => hsv_to_rgb(dh, ds, sv),
        _ => dst,
      }
    }
    BlendMode::HueOklch
    | BlendMode::ChromaOklch
    | BlendMode::ColorOklch
    | BlendMode::LuminosityOklch => {
      let (sl, sc, sh) = rgb_to_oklch(src.0, src.1, src.2);
      let (dl, dc, dh) = rgb_to_oklch(dst.0, dst.1, dst.2);
      match mode {
        BlendMode::HueOklch => oklch_to_rgb(dl, dc, sh),
        BlendMode::ChromaOklch => oklch_to_rgb(dl, sc, dh),
        BlendMode::ColorOklch => oklch_to_rgb(dl, sc, sh),
        BlendMode::LuminosityOklch => oklch_to_rgb(sl, dc, dh),
        _ => dst,
      }
    }
    BlendMode::PlusDarker => (
      (src.0 + dst.0 - 1.0).max(0.0).min(1.0),
      (src.1 + dst.1 - 1.0).max(0.0).min(1.0),
      (src.2 + dst.2 - 1.0).max(0.0).min(1.0),
    ),
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

fn apply_filters(pixmap: &mut Pixmap, filters: &[ResolvedFilter], scale: f32, bbox: Rect) {
  for filter in filters {
    match filter {
      ResolvedFilter::Blur(radius) => apply_gaussian_blur(pixmap, *radius * scale),
      ResolvedFilter::Brightness(amount) => {
        apply_color_filter(pixmap, |c, a| (scale_color(c, *amount), a))
      }
      ResolvedFilter::Contrast(amount) => {
        apply_color_filter(pixmap, |c, a| (apply_contrast(c, *amount), a))
      }
      ResolvedFilter::Grayscale(amount) => {
        apply_color_filter(pixmap, |c, a| (grayscale(c, *amount), a))
      }
      ResolvedFilter::Sepia(amount) => apply_color_filter(pixmap, |c, a| (sepia(c, *amount), a)),
      ResolvedFilter::Saturate(amount) => {
        apply_color_filter(pixmap, |c, a| (saturate(c, *amount), a))
      }
      ResolvedFilter::HueRotate(deg) => apply_color_filter(pixmap, |c, a| (hue_rotate(c, *deg), a)),
      ResolvedFilter::Invert(amount) => apply_color_filter(pixmap, |c, a| (invert(c, *amount), a)),
      ResolvedFilter::Opacity(amount) => apply_color_filter(pixmap, |c, a| (c, a * *amount)),
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
        *color,
      ),
      ResolvedFilter::SvgFilter(ref filter) => {
        crate::paint::svg_filter::apply_svg_filter(filter.as_ref(), pixmap, scale, bbox);
      }
    }
  }
}

fn apply_filters_scoped(
  pixmap: &mut Pixmap,
  filters: &[ResolvedFilter],
  scale: f32,
  bbox: Rect,
  bounds: Option<&Rect>,
) {
  if filters.is_empty() {
    return;
  }

  let Some(bounds) = bounds else {
    apply_filters(pixmap, filters, scale, bbox);
    return;
  };

  let x = bounds.min_x().floor() as i32;
  let y = bounds.min_y().floor() as i32;
  let width = bounds.width().ceil() as u32;
  let height = bounds.height().ceil() as u32;
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

  if region_w == pixmap.width() && region_h == pixmap.height() && clamped_x == 0 && clamped_y == 0 {
    apply_filters(pixmap, filters, scale, bbox);
    return;
  }

  let Some(mut region) = Pixmap::new(region_w, region_h) else {
    apply_filters(pixmap, filters, scale, bbox);
    return;
  };

  let bytes_per_row = pixmap.width() as usize * 4;
  let region_row_bytes = region_w as usize * 4;
  let start = (clamped_y as usize * bytes_per_row) + clamped_x as usize * 4;
  let data = pixmap.data();
  let dest = region.data_mut();

  for row in 0..region_h as usize {
    let src_idx = start + row * bytes_per_row;
    let dst_idx = row * region_row_bytes;
    dest[dst_idx..dst_idx + region_row_bytes]
      .copy_from_slice(&data[src_idx..src_idx + region_row_bytes]);
  }

  let local_bbox = Rect::from_xywh(
    bbox.x() - clamped_x as f32,
    bbox.y() - clamped_y as f32,
    bbox.width(),
    bbox.height(),
  );
  apply_filters(&mut region, filters, scale, local_bbox);

  for row in 0..region_h as usize {
    let dst_idx = (clamped_y as usize + row) * bytes_per_row + clamped_x as usize * 4;
    let src_idx = row * region_row_bytes;
    let src = &region.data()[src_idx..src_idx + region_row_bytes];
    let dst = &mut pixmap.data_mut()[dst_idx..dst_idx + region_row_bytes];
    dst.copy_from_slice(src);
  }
}
fn apply_backdrop_filters(
  pixmap: &mut Pixmap,
  bounds: &Rect,
  filters: &[ResolvedFilter],
  radii: BorderRadii,
  scale: f32,
  clip_mask: Option<&Mask>,
  clip_bounds: Option<Rect>,
  filter_bounds: Rect,
) {
  if filters.is_empty() {
    return;
  }
  let (out_l, out_t, out_r, out_b) =
    filter_outset_with_bounds(filters, scale, Some(filter_bounds)).as_tuple();
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
    dest[dst_idx..dst_idx + region_row_bytes]
      .copy_from_slice(&data[src_idx..src_idx + region_row_bytes]);
  }

  let local_bbox = Rect::from_xywh(
    bounds.x() - clamped_x as f32,
    bounds.y() - clamped_y as f32,
    bounds.width(),
    bounds.height(),
  );
  apply_filters(&mut region, filters, scale, local_bbox);

  let radii_mask = if !radii.is_zero() {
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
    Some(mask)
  } else {
    None
  };

  let mut write_rect = *bounds;
  if let Some(clip) = clip_bounds {
    let Some(intersection) = write_rect.intersection(clip) else {
      return;
    };
    write_rect = intersection;
  }
  if write_rect.width() <= 0.0 || write_rect.height() <= 0.0 {
    return;
  }

  let write_x = write_rect.min_x().floor().max(clamped_x as f32).max(0.0) as u32;
  let write_y = write_rect.min_y().floor().max(clamped_y as f32).max(0.0) as u32;
  let write_w = (write_rect.width().ceil() as i32)
    .min(pix_w - write_x as i32)
    .max(0) as u32;
  let write_h = (write_rect.height().ceil() as i32)
    .min(pix_h - write_y as i32)
    .max(0) as u32;
  if write_w == 0 || write_h == 0 {
    return;
  }

  let src_start_x = (write_x as i32 - clamped_x as i32) as u32;
  let src_start_y = (write_y as i32 - clamped_y as i32) as u32;
  let dest_data = pixmap.data_mut();
  let src_data = region.data();
  let region_width = region.width() as usize;
  let clip_mask_data =
    clip_mask.map(|mask| (mask.data(), mask.width() as usize, mask.height() as usize));
  let radii_pixels = radii_mask.as_ref().map(|mask| mask.pixels());

  for row in 0..write_h as usize {
    let dst_idx = (write_y as usize + row) * bytes_per_row + write_x as usize * 4;
    let src_idx = ((src_start_y as usize + row) * region_width + src_start_x as usize) * 4;
    let mask_y = write_y as usize + row;

    for col in 0..write_w as usize {
      let dest_offset = dst_idx + col * 4;
      let src_offset = src_idx + col * 4;
      let mut coverage = 255u16;

      if let Some((mask_data, mask_w, mask_h)) = &clip_mask_data {
        let mask_x = write_x as usize + col;
        if mask_x >= *mask_w || mask_y >= *mask_h {
          coverage = 0;
        } else {
          let mask_idx = mask_y * mask_w + mask_x;
          coverage = coverage.saturating_mul(mask_data[mask_idx] as u16) / 255;
        }
      }

      if let Some(mask_pixels) = radii_pixels {
        let mask_idx = (src_start_y as usize + row) * region_width + (src_start_x as usize + col);
        coverage = coverage.saturating_mul(mask_pixels[mask_idx].alpha() as u16) / 255;
      }

      if coverage >= 255 {
        dest_data[dest_offset..dest_offset + 4]
          .copy_from_slice(&src_data[src_offset..src_offset + 4]);
        continue;
      }

      if coverage == 0 {
        continue;
      }

      let cov = coverage as f32 / 255.0;
      let inv = 1.0 - cov;
      let src_px = &src_data[src_offset..src_offset + 4];
      let dst_slice = &mut dest_data[dest_offset..dest_offset + 4];
      for i in 0..4 {
        let val = (src_px[i] as f32 * cov + dst_slice[i] as f32 * inv)
          .round()
          .clamp(0.0, 255.0) as u8;
        dst_slice[i] = val;
      }
    }
  }
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
        data[idx] = 0;
        data[idx + 1] = 0;
        data[idx + 2] = 0;
        data[idx + 3] = 0;
      }
    }
  }
}

fn rect_int_bounds(rect: &Rect) -> (i32, i32, i32, i32) {
  let x0 = rect.min_x().floor() as i32;
  let y0 = rect.min_y().floor() as i32;
  let x1 = rect.max_x().ceil() as i32;
  let y1 = rect.max_y().ceil() as i32;
  (x0, y0, x1, y1)
}

fn apply_drop_shadow(
  pixmap: &mut Pixmap,
  offset_x: f32,
  offset_y: f32,
  blur_radius: f32,
  spread: f32,
  color: Rgba,
) {
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

  if spread != 0.0 {
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
  let radius = spread.abs().ceil() as i32;
  if radius <= 0 || spread == 0.0 {
    return;
  }
  let expand = spread > 0.0;
  let width = pixmap.width() as i32;
  let height = pixmap.height() as i32;
  let original = pixmap.clone();
  let src = original.pixels();
  let dst = pixmap.pixels_mut();
  let row_len = width as usize;

  dst.par_iter_mut().enumerate().for_each(|(idx, dst_px)| {
    let y = (idx / row_len) as i32;
    let x = (idx % row_len) as i32;
    let mut agg = if expand { [0u8; 4] } else { [255u8; 4] };
    for dy in -radius..=radius {
      for dx in -radius..=radius {
        let ny = (y + dy).clamp(0, height - 1);
        let nx = (x + dx).clamp(0, width - 1);
        let sample_idx = (ny as usize) * row_len + nx as usize;
        let px = src[sample_idx];
        if expand {
          agg[0] = agg[0].max(px.red());
          agg[1] = agg[1].max(px.green());
          agg[2] = agg[2].max(px.blue());
          agg[3] = agg[3].max(px.alpha());
        } else {
          agg[0] = agg[0].min(px.red());
          agg[1] = agg[1].min(px.green());
          agg[2] = agg[2].min(px.blue());
          agg[3] = agg[3].min(px.alpha());
        }
      }
    }
    *dst_px = PremultipliedColorU8::from_rgba(agg[0], agg[1], agg[2], agg[3])
      .unwrap_or(PremultipliedColorU8::TRANSPARENT);
  });
}

fn apply_color_filter(
  pixmap: &mut Pixmap,
  f: impl Fn((u8, u8, u8), f32) -> ((u8, u8, u8), f32) + Send + Sync,
) {
  let pixels = pixmap.pixels_mut();
  let threshold = 2048;
  if pixels.len() > threshold {
    pixels.par_iter_mut().for_each(|pixel| {
      let a = pixel.alpha() as f32 / 255.0;
      let (c, a2) = f((pixel.red(), pixel.green(), pixel.blue()), a);
      let final_a = (a2 * 255.0).round().clamp(0.0, 255.0) as u8;
      let premultiply = |v: u8| ((v as f32 * a2).round().clamp(0.0, 255.0)) as u8;
      *pixel = tiny_skia::PremultipliedColorU8::from_rgba(
        premultiply(c.0),
        premultiply(c.1),
        premultiply(c.2),
        final_a,
      )
      .unwrap_or_else(|| tiny_skia::PremultipliedColorU8::from_rgba(0, 0, 0, 0).unwrap());
    });
  } else {
    for pixel in pixels.iter_mut() {
      let a = pixel.alpha() as f32 / 255.0;
      let (c, a2) = f((pixel.red(), pixel.green(), pixel.blue()), a);
      let final_a = (a2 * 255.0).round().clamp(0.0, 255.0) as u8;
      let premultiply = |v: u8| ((v as f32 * a2).round().clamp(0.0, 255.0)) as u8;
      *pixel = tiny_skia::PremultipliedColorU8::from_rgba(
        premultiply(c.0),
        premultiply(c.1),
        premultiply(c.2),
        final_a,
      )
      .unwrap_or_else(|| tiny_skia::PremultipliedColorU8::from_rgba(0, 0, 0, 0).unwrap());
    }
  }
}

fn scale_color((r, g, b): (u8, u8, u8), amount: f32) -> (u8, u8, u8) {
  let scale = |v: u8| ((v as f32) * amount).round().clamp(0.0, 255.0) as u8;
  (scale(r), scale(g), scale(b))
}

fn apply_contrast((r, g, b): (u8, u8, u8), amount: f32) -> (u8, u8, u8) {
  let factor = (259.0 * (amount + 255.0)) / (255.0 * (259.0 - amount));
  let adjust = |v: u8| {
    ((factor * (v as f32 - 128.0) + 128.0)
      .round()
      .clamp(0.0, 255.0)) as u8
  };
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
  let mix = |v: u8| {
    ((gray + (v as f32 - gray) * amount)
      .round()
      .clamp(0.0, 255.0)) as u8
  };
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
  text_rasterizer: TextRasterizer,
  stacking_layers: Vec<StackingRecord>,
  blend_stack: Vec<Option<BlendMode>>,
  scale: f32,
  transform_stack: Vec<Transform3D>,
  perspective_stack: Vec<Transform3D>,
  culled_depth: usize,
  preserve_3d_disabled: bool,
}

#[derive(Debug)]
struct StackingRecord {
  needs_layer: bool,
  filters: Vec<ResolvedFilter>,
  radii: BorderRadii,
  mask_bounds: Rect,
  mask: Option<ResolvedMask>,
  css_bounds: Rect,
  manual_blend: Option<BlendMode>,
  layer_bounds: Option<Rect>,
  projective_transform: Option<Transform3D>,
  parent_transform: Transform,
  culled: bool,
  pending_backdrop: Option<PendingBackdrop>,
}

#[derive(Debug, Clone)]
struct PendingBackdrop {
  bounds: Rect,
  filters: Vec<ResolvedFilter>,
  radii: BorderRadii,
  filter_bounds: Rect,
}

#[derive(Clone)]
struct StackingNode {
  context: StackingContextItem,
  start_index: usize,
  end_index: usize,
  segments: Vec<StackingSegment>,
  subtree_items: Vec<DisplayItem>,
}

#[derive(Clone)]
enum StackingSegment {
  Items(Vec<DisplayItem>),
  Child(StackingNode),
}

#[derive(Clone)]
struct SceneItem {
  source: SceneItemSource,
  transform: Transform3D,
  bounds: Rect,
  backface_visibility: BackfaceVisibility,
  transform_style: TransformStyle,
  order: usize,
}

#[derive(Clone)]
enum SceneItemSource {
  Segment(Vec<DisplayItem>),
  FlattenedSubtree(StackingNode),
}

struct GlyphShadowSource {
  path: Option<tiny_skia::Path>,
  color_glyph: Option<ColorGlyphRaster>,
  origin: Point,
}

fn parse_stacking_node(items: &[DisplayItem], start: usize) -> Option<(StackingNode, usize)> {
  if start >= items.len() {
    return None;
  }
  let DisplayItem::PushStackingContext(context) = &items[start] else {
    return None;
  };

  let mut idx = start + 1;
  let mut segments = Vec::new();
  let mut current_items = Vec::new();
  while idx < items.len() {
    match &items[idx] {
      DisplayItem::PushStackingContext(_) => {
        if !current_items.is_empty() {
          segments.push(StackingSegment::Items(current_items));
          current_items = Vec::new();
        }
        let Some((child, end)) = parse_stacking_node(items, idx) else {
          return None;
        };
        segments.push(StackingSegment::Child(child));
        idx = end + 1;
        continue;
      }
      DisplayItem::PopStackingContext => {
        break;
      }
      other => current_items.push(other.clone()),
    }
    idx += 1;
  }

  if idx >= items.len() {
    return None;
  }

  if !current_items.is_empty() {
    segments.push(StackingSegment::Items(current_items));
  }

  let end_index = idx;
  let subtree_items = items[start..=end_index].iter().cloned().collect();
  Some((
    StackingNode {
      context: context.clone(),
      start_index: start,
      end_index,
      segments,
      subtree_items,
    },
    end_index,
  ))
}

fn compute_items_bounds(items: &[DisplayItem]) -> Option<Rect> {
  let mut bounds: Option<Rect> = None;
  for item in items {
    if let Some(b) = item.bounds() {
      bounds = Some(match bounds {
        Some(existing) => existing.union(b),
        None => b,
      });
    }
  }
  bounds
}

fn collect_scene_items(
  node: &StackingNode,
  parent_transform: &Transform3D,
  in_preserve_context: bool,
  order_counter: &mut usize,
) -> Vec<SceneItem> {
  let local_transform = node.context.transform.unwrap_or_else(Transform3D::identity);
  let combined_transform = parent_transform.multiply(&local_transform);
  let child_transform = node
    .context
    .child_perspective
    .as_ref()
    .map(|perspective| combined_transform.multiply(perspective))
    .unwrap_or(combined_transform);
  let mut items = Vec::new();
  let transform_style = node.context.transform_style;

  if !in_preserve_context || !matches!(transform_style, TransformStyle::Preserve3d) {
    let order = *order_counter;
    *order_counter += 1;
    items.push(SceneItem {
      source: SceneItemSource::FlattenedSubtree(node.clone()),
      transform: combined_transform,
      bounds: node.context.bounds,
      backface_visibility: node.context.backface_visibility,
      transform_style,
      order,
    });
    return items;
  }

  for segment in &node.segments {
    match segment {
      StackingSegment::Items(list) => {
        if list.is_empty() {
          continue;
        }
        let bounds = compute_items_bounds(list).unwrap_or(node.context.bounds);
        let order = *order_counter;
        *order_counter += 1;
        items.push(SceneItem {
          source: SceneItemSource::Segment(list.clone()),
          transform: combined_transform,
          bounds,
          backface_visibility: node.context.backface_visibility,
          transform_style,
          order,
        });
      }
      StackingSegment::Child(child) => {
        items.extend(collect_scene_items(
          child,
          &child_transform,
          true,
          order_counter,
        ));
      }
    }
  }

  items
}

fn scene_depth(transform: &Transform3D, bounds: Rect) -> f32 {
  let corners = [
    (bounds.min_x(), bounds.min_y()),
    (bounds.max_x(), bounds.min_y()),
    (bounds.max_x(), bounds.max_y()),
    (bounds.min_x(), bounds.max_y()),
  ];
  let mut total = 0.0;
  for (x, y) in corners {
    let (_, _, z, w) = transform.transform_point(x, y, 0.0);
    total += if w.abs() > 1e-6 { z / w } else { z };
  }
  total / 4.0
}

impl DisplayListRenderer {
  #[inline]
  fn ds_len(&self, v: f32) -> f32 {
    if !v.is_finite() {
      return 0.0;
    }
    v * self.scale
  }

  fn to_skia_transform_checked(&self, transform: &Transform3D) -> (Transform, bool) {
    if let Some(affine) = transform.to_2d() {
      return (
        Transform::from_row(
          affine.a,
          affine.b,
          affine.c,
          affine.d,
          self.ds_len(affine.e),
          self.ds_len(affine.f),
        ),
        true,
      );
    }

    let (approx, valid) = transform.approximate_2d_with_validity();
    (
      Transform::from_row(
        approx.a,
        approx.b,
        approx.c,
        approx.d,
        self.ds_len(approx.e),
        self.ds_len(approx.f),
      ),
      valid,
    )
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

  fn stacking_layer_bounds(
    &self,
    bounds: Rect,
    transform: Transform,
    filters: &[ResolvedFilter],
    backdrop_filters: &[ResolvedFilter],
  ) -> Option<Rect> {
    let mut layer_bounds = transform_rect(bounds, &transform);
    let (f_l, f_t, f_r, f_b) = filter_outset(filters, self.scale).as_tuple();
    let (b_l, b_t, b_r, b_b) = filter_outset(backdrop_filters, self.scale).as_tuple();
    let expand_left = f_l.max(b_l);
    let expand_top = f_t.max(b_t);
    let expand_right = f_r.max(b_r);
    let expand_bottom = f_b.max(b_b);
    layer_bounds = Rect::from_xywh(
      layer_bounds.x() - expand_left,
      layer_bounds.y() - expand_top,
      layer_bounds.width() + expand_left + expand_right,
      layer_bounds.height() + expand_top + expand_bottom,
    );

    if let Some(clip) = self.canvas.clip_bounds() {
      layer_bounds = match layer_bounds.intersection(clip) {
        Some(r) => r,
        None => return None,
      };
    }

    layer_bounds = match layer_bounds.intersection(self.canvas.bounds()) {
      Some(r) => r,
      None => return None,
    };

    if layer_bounds.width() <= 0.0
      || layer_bounds.height() <= 0.0
      || !layer_bounds.x().is_finite()
      || !layer_bounds.y().is_finite()
    {
      return None;
    }

    Some(layer_bounds)
  }

  fn layer_space_bounds(&self, bounds: Rect, origin: (i32, i32), layer: &Pixmap) -> Option<Rect> {
    let translated = Rect::from_xywh(
      bounds.x() - origin.0 as f32,
      bounds.y() - origin.1 as f32,
      bounds.width(),
      bounds.height(),
    );
    let layer_rect = Rect::from_xywh(0.0, 0.0, layer.width() as f32, layer.height() as f32);
    translated.intersection(layer_rect)
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
      e.inline_vertical = emphasis.inline_vertical;
      e.marks = e
        .marks
        .iter()
        .map(|m| EmphasisMark {
          center: self.ds_point(m.center),
        })
        .collect();
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

  fn scale_list_marker_item(&self, item: &ListMarkerItem) -> ListMarkerItem {
    let text_equiv = TextItem {
      origin: item.origin,
      glyphs: item.glyphs.clone(),
      color: item.color,
      shadows: item.shadows.clone(),
      font_size: item.font_size,
      advance_width: item.advance_width,
      font_id: item.font_id.clone(),
      synthetic_bold: item.synthetic_bold,
      synthetic_oblique: item.synthetic_oblique,
      emphasis: item.emphasis.clone(),
      decorations: Vec::new(),
    };
    let scaled = self.scale_text_item(&text_equiv);
    ListMarkerItem {
      origin: scaled.origin,
      glyphs: scaled.glyphs,
      color: scaled.color,
      shadows: scaled.shadows,
      font_size: scaled.font_size,
      advance_width: scaled.advance_width,
      font_id: scaled.font_id,
      synthetic_bold: scaled.synthetic_bold,
      synthetic_oblique: scaled.synthetic_oblique,
      emphasis: scaled.emphasis,
      background: item.background,
    }
  }

  fn scale_decoration_item(
    &self,
    item: &crate::paint::display_list::TextDecorationItem,
  ) -> crate::paint::display_list::TextDecorationItem {
    let mut scaled = item.clone();
    scaled.bounds = self.ds_rect(item.bounds);
    scaled.line_start = self.ds_len(item.line_start);
    scaled.line_width = self.ds_len(item.line_width);
    scaled.inline_vertical = item.inline_vertical;
    scaled.decorations = item
      .decorations
      .iter()
      .map(|d| {
        let scale_stroke = |s: &Option<DecorationStroke>| -> Option<DecorationStroke> {
          s.as_ref().map(|stroke| DecorationStroke {
            center: self.ds_len(stroke.center),
            thickness: self.ds_len(stroke.thickness),
            segments: stroke.segments.as_ref().map(|segs| {
              segs
                .iter()
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
    let scale = if scale.is_finite() && scale > 0.0 {
      scale
    } else {
      1.0
    };
    let device_w = ((width as f32) * scale).round().max(1.0) as u32;
    let device_h = ((height as f32) * scale).round().max(1.0) as u32;
    Ok(Self {
      canvas: Canvas::new(device_w, device_h, background)?,
      font_ctx,
      text_rasterizer: TextRasterizer::new(),
      stacking_layers: Vec::new(),
      blend_stack: Vec::new(),
      scale,
      transform_stack: vec![Transform3D::identity()],
      perspective_stack: vec![Transform3D::identity()],
      culled_depth: 0,
      preserve_3d_disabled: false,
    })
  }

  fn render_linear_gradient(&mut self, item: &LinearGradientItem) {
    let Some(stops) = self.convert_stops(&item.stops) else {
      return;
    };
    let rect = self.ds_rect(item.rect);
    let start = SkiaPoint::from_xy(
      rect.x() + self.ds_len(item.start.x),
      rect.y() + self.ds_len(item.start.y),
    );
    let end = SkiaPoint::from_xy(
      rect.x() + self.ds_len(item.end.x),
      rect.y() + self.ds_len(item.end.y),
    );
    let spread = match item.spread {
      crate::paint::display_list::GradientSpread::Pad => SpreadMode::Pad,
      crate::paint::display_list::GradientSpread::Repeat => SpreadMode::Repeat,
      crate::paint::display_list::GradientSpread::Reflect => SpreadMode::Reflect,
    };
    let Some(shader) = LinearGradient::new(start, end, stops, spread, Transform::identity()) else {
      return;
    };

    let Some(skia_rect) =
      tiny_skia::Rect::from_xywh(rect.x(), rect.y(), rect.width(), rect.height())
    else {
      return;
    };

    let path = tiny_skia::PathBuilder::from_rect(skia_rect);

    let mut paint = tiny_skia::Paint::default();
    paint.shader = shader;
    paint.anti_alias = true;
    paint.blend_mode = self.canvas.blend_mode();
    let transform = self.canvas.transform();
    let clip = self.canvas.clip_mask().cloned();
    self.canvas.pixmap_mut().fill_path(
      &path,
      &paint,
      tiny_skia::FillRule::Winding,
      transform,
      clip.as_ref(),
    );
  }

  fn render_radial_gradient(&mut self, item: &RadialGradientItem) {
    let Some(stops) = self.convert_stops(&item.stops) else {
      return;
    };
    let rect = self.ds_rect(item.rect);
    let center = SkiaPoint::from_xy(
      rect.x() + self.ds_len(item.center.x),
      rect.y() + self.ds_len(item.center.y),
    );
    let radii = SkiaPoint::from_xy(self.ds_len(item.radii.x), self.ds_len(item.radii.y));
    let spread = match item.spread {
      crate::paint::display_list::GradientSpread::Pad => SpreadMode::Pad,
      crate::paint::display_list::GradientSpread::Repeat => SpreadMode::Repeat,
      crate::paint::display_list::GradientSpread::Reflect => SpreadMode::Reflect,
    };
    let transform = Transform::from_translate(center.x, center.y).pre_scale(radii.x, radii.y);
    let Some(shader) = RadialGradient::new(
      SkiaPoint::from_xy(0.0, 0.0),
      SkiaPoint::from_xy(0.0, 0.0),
      1.0,
      stops,
      spread,
      transform,
    ) else {
      return;
    };

    let Some(skia_rect) =
      tiny_skia::Rect::from_xywh(rect.x(), rect.y(), rect.width(), rect.height())
    else {
      return;
    };

    let path = tiny_skia::PathBuilder::from_rect(skia_rect);

    let mut paint = tiny_skia::Paint::default();
    paint.shader = shader;
    paint.anti_alias = true;
    paint.blend_mode = self.canvas.blend_mode();
    let transform = self.canvas.transform();
    let clip = self.canvas.clip_mask().cloned();
    self.canvas.pixmap_mut().fill_path(
      &path,
      &paint,
      tiny_skia::FillRule::Winding,
      transform,
      clip.as_ref(),
    );
  }

  fn render_conic_gradient(&mut self, item: &ConicGradientItem) {
    let rect = self.ds_rect(item.rect);
    let width = rect.width().ceil() as u32;
    let height = rect.height().ceil() as u32;
    if width == 0 || height == 0 {
      return;
    }

    let opacity = self.canvas.opacity().clamp(0.0, 1.0);
    let stops: Vec<(f32, crate::style::color::Rgba)> = item
      .stops
      .iter()
      .map(|s| {
        (
          s.position,
          crate::style::color::Rgba {
            a: s.color.a * opacity,
            ..s.color
          },
        )
      })
      .collect();
    if stops.is_empty() {
      return;
    }

    let center = SkiaPoint::from_xy(self.ds_len(item.center.x), self.ds_len(item.center.y));
    let start_angle = item.from_angle.to_radians();
    let period = if item.repeating {
      stops.last().map(|s| s.0).unwrap_or(1.0).max(1e-6)
    } else {
      1.0
    };

    let mut pix = match Pixmap::new(width, height) {
      Some(p) => p,
      None => return,
    };
    let data = pix.pixels_mut();
    let transparent = PremultipliedColorU8::from_rgba(0, 0, 0, 0).unwrap();
    for y in 0..height {
      for x in 0..width {
        let dx = x as f32 + 0.5 - center.x;
        let dy = y as f32 + 0.5 - center.y;
        let angle = dx.atan2(-dy) + start_angle;
        let mut t = (angle / (2.0 * std::f32::consts::PI)).rem_euclid(1.0);
        t *= period;
        let color = sample_conic_stops(&stops, t, item.repeating, period);
        let idx = (y * width + x) as usize;
        data[idx] = PremultipliedColorU8::from_rgba(
          color.r,
          color.g,
          color.b,
          (color.a * 255.0).round().clamp(0.0, 255.0) as u8,
        )
        .unwrap_or(transparent);
      }
    }

    let paint = tiny_skia::PixmapPaint {
      opacity: 1.0,
      blend_mode: self.canvas.blend_mode(),
      ..Default::default()
    };
    let dest_x = rect.x().floor() as i32;
    let dest_y = rect.y().floor() as i32;
    let frac_x = rect.x() - dest_x as f32;
    let frac_y = rect.y() - dest_y as f32;
    let transform = Transform::from_translate(frac_x, frac_y).post_concat(self.canvas.transform());
    let clip = self.canvas.clip_mask().cloned();
    self.canvas.pixmap_mut().draw_pixmap(
      dest_x,
      dest_y,
      pix.as_ref(),
      &paint,
      transform,
      clip.as_ref(),
    );
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
    let top = BorderSide {
      width: self.ds_len(item.top.width),
      ..item.top.clone()
    };
    let right = BorderSide {
      width: self.ds_len(item.right.width),
      ..item.right.clone()
    };
    let bottom = BorderSide {
      width: self.ds_len(item.bottom.width),
      ..item.bottom.clone()
    };
    let left = BorderSide {
      width: self.ds_len(item.left.width),
      ..item.left.clone()
    };

    let pushed_clip = if radii.has_radius() {
      self.canvas.save();
      self.canvas.set_clip_with_radii(rect, Some(radii));
      true
    } else {
      false
    };

    if let Some(border_image) = item.image.as_ref() {
      if self.render_border_image(
        border_image,
        rect,
        top.width,
        right.width,
        bottom.width,
        left.width,
        opacity,
        clip.as_ref(),
        transform,
      ) {
        if pushed_clip {
          self.canvas.restore();
        }
        return;
      }
    }

    let top_center = rect.y() + top.width * 0.5;
    let bottom_center = rect.y() + rect.height() - bottom.width * 0.5;
    let left_center = rect.x() + left.width * 0.5;
    let right_center = rect.x() + rect.width() - right.width * 0.5;
    let edges: [(_, _, _, _); 4] = [
      (
        BorderEdge::Top,
        &top,
        (left_center, top_center),
        (right_center, top_center),
      ),
      (
        BorderEdge::Right,
        &right,
        (right_center, top_center),
        (right_center, bottom_center),
      ),
      (
        BorderEdge::Bottom,
        &bottom,
        (left_center, bottom_center),
        (right_center, bottom_center),
      ),
      (
        BorderEdge::Left,
        &left,
        (left_center, top_center),
        (left_center, bottom_center),
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

  fn render_outline(&mut self, item: &OutlineItem) {
    let width = self.ds_len(item.width);
    if width <= 0.0 || matches!(item.style, CssBorderStyle::None | CssBorderStyle::Hidden) {
      return;
    }
    let offset = self.ds_len(item.offset);
    let expand = offset + width * 0.5;
    let rect = self.ds_rect(item.rect);
    let outer_x = rect.x() - expand;
    let outer_y = rect.y() - expand;
    let outer_w = rect.width() + 2.0 * expand;
    let outer_h = rect.height() + 2.0 * expand;
    let blend_mode = if item.invert {
      tiny_skia::BlendMode::Difference
    } else {
      self.canvas.blend_mode()
    };
    let opacity = self.canvas.opacity().clamp(0.0, 1.0);
    // CSS outlines should not be clipped by overflow/clip regions.
    let clip = None;
    self.canvas.save();
    self.canvas.clear_clip();
    let transform = self.canvas.transform();
    let color = item.color;

    // Top
    self.render_border_edge(
      BorderEdge::Top,
      outer_x,
      outer_y + width * 0.5,
      outer_x + outer_w,
      outer_y + width * 0.5,
      &BorderSide {
        width,
        style: item.style,
        color,
      },
      blend_mode,
      opacity,
      clip.as_ref(),
      transform,
    );
    // Bottom
    self.render_border_edge(
      BorderEdge::Bottom,
      outer_x,
      outer_y + outer_h - width * 0.5,
      outer_x + outer_w,
      outer_y + outer_h - width * 0.5,
      &BorderSide {
        width,
        style: item.style,
        color,
      },
      blend_mode,
      opacity,
      clip.as_ref(),
      transform,
    );
    // Left
    self.render_border_edge(
      BorderEdge::Left,
      outer_x + width * 0.5,
      outer_y,
      outer_x + width * 0.5,
      outer_y + outer_h,
      &BorderSide {
        width,
        style: item.style,
        color,
      },
      blend_mode,
      opacity,
      clip.as_ref(),
      transform,
    );
    // Right
    self.render_border_edge(
      BorderEdge::Right,
      outer_x + outer_w - width * 0.5,
      outer_y,
      outer_x + outer_w - width * 0.5,
      outer_y + outer_h,
      &BorderSide {
        width,
        style: item.style,
        color,
      },
      blend_mode,
      opacity,
      clip.as_ref(),
      transform,
    );
    self.canvas.restore();
  }

  fn render_border_image(
    &mut self,
    border_image: &BorderImageItem,
    rect: Rect,
    top: f32,
    right: f32,
    bottom: f32,
    left: f32,
    opacity: f32,
    clip: Option<&Mask>,
    transform: Transform,
  ) -> bool {
    let border_widths = BorderImageWidths {
      top,
      right,
      bottom,
      left,
    };
    let target_widths = resolve_border_image_widths(
      &border_image.width,
      border_widths,
      rect.width(),
      rect.height(),
      border_image.font_size,
      border_image.root_font_size,
      border_image.viewport,
    );
    let outsets = resolve_border_image_outset(
      &border_image.outset,
      target_widths,
      border_image.font_size,
      border_image.root_font_size,
      border_image.viewport,
    );

    let outer_rect = Rect::from_xywh(
      rect.x() - outsets.left,
      rect.y() - outsets.top,
      rect.width() + outsets.left + outsets.right,
      rect.height() + outsets.top + outsets.bottom,
    );
    let inner_rect = Rect::from_xywh(
      outer_rect.x() + target_widths.left,
      outer_rect.y() + target_widths.top,
      outer_rect.width() - target_widths.left - target_widths.right,
      outer_rect.height() - target_widths.top - target_widths.bottom,
    );
    if inner_rect.width() <= 0.0 || inner_rect.height() <= 0.0 {
      return true;
    }

    let (pixmap, img_w, img_h) = match &border_image.source {
      BorderImageSourceItem::Raster(image) => {
        let Some(pixmap) = image_data_to_pixmap(image) else {
          return false;
        };
        let (w, h) = (pixmap.width(), pixmap.height());
        if w == 0 || h == 0 {
          return false;
        }
        (pixmap, w, h)
      }
      BorderImageSourceItem::Generated(bg) => {
        let img_w = outer_rect.width().max(1.0).round() as u32;
        let img_h = outer_rect.height().max(1.0).round() as u32;
        let Some(pixmap) = render_generated_border_image(
          bg,
          border_image.current_color,
          img_w,
          img_h,
          border_image.font_size,
          border_image.root_font_size,
          border_image.viewport,
        ) else {
          return false;
        };
        let (w, h) = (pixmap.width(), pixmap.height());
        if w == 0 || h == 0 {
          return false;
        }
        (pixmap, w, h)
      }
    };

    let slice_top = resolve_slice_value(border_image.slice.top, img_h);
    let slice_right = resolve_slice_value(border_image.slice.right, img_w);
    let slice_bottom = resolve_slice_value(border_image.slice.bottom, img_h);
    let slice_left = resolve_slice_value(border_image.slice.left, img_w);

    let sx0 = 0.0;
    let sx1 = slice_left.min(img_w as f32);
    let sx2 = (img_w as f32 - slice_right).max(sx1);
    let sx3 = img_w as f32;

    let sy0 = 0.0;
    let sy1 = slice_top.min(img_h as f32);
    let sy2 = (img_h as f32 - slice_bottom).max(sy1);
    let sy3 = img_h as f32;

    let (repeat_x, repeat_y) = border_image.repeat;

    // Corners
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx0, sy0, sx1 - sx0, sy1 - sy0),
      Rect::from_xywh(
        outer_rect.x(),
        outer_rect.y(),
        target_widths.left,
        target_widths.top,
      ),
      BorderImageRepeat::Stretch,
      BorderImageRepeat::Stretch,
      opacity,
      clip,
      transform,
    );
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx2, sy0, sx3 - sx2, sy1 - sy0),
      Rect::from_xywh(
        outer_rect.x() + outer_rect.width() - target_widths.right,
        outer_rect.y(),
        target_widths.right,
        target_widths.top,
      ),
      BorderImageRepeat::Stretch,
      BorderImageRepeat::Stretch,
      opacity,
      clip,
      transform,
    );
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx0, sy2, sx1 - sx0, sy3 - sy2),
      Rect::from_xywh(
        outer_rect.x(),
        outer_rect.y() + outer_rect.height() - target_widths.bottom,
        target_widths.left,
        target_widths.bottom,
      ),
      BorderImageRepeat::Stretch,
      BorderImageRepeat::Stretch,
      opacity,
      clip,
      transform,
    );
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx2, sy2, sx3 - sx2, sy3 - sy2),
      Rect::from_xywh(
        outer_rect.x() + outer_rect.width() - target_widths.right,
        outer_rect.y() + outer_rect.height() - target_widths.bottom,
        target_widths.right,
        target_widths.bottom,
      ),
      BorderImageRepeat::Stretch,
      BorderImageRepeat::Stretch,
      opacity,
      clip,
      transform,
    );

    // Edges
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx1, sy0, sx2 - sx1, sy1 - sy0),
      Rect::from_xywh(
        inner_rect.x(),
        outer_rect.y(),
        inner_rect.width(),
        target_widths.top,
      ),
      repeat_x,
      BorderImageRepeat::Stretch,
      opacity,
      clip,
      transform,
    );
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx1, sy2, sx2 - sx1, sy3 - sy2),
      Rect::from_xywh(
        inner_rect.x(),
        outer_rect.y() + outer_rect.height() - target_widths.bottom,
        inner_rect.width(),
        target_widths.bottom,
      ),
      repeat_x,
      BorderImageRepeat::Stretch,
      opacity,
      clip,
      transform,
    );
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx0, sy1, sx1 - sx0, sy2 - sy1),
      Rect::from_xywh(
        outer_rect.x(),
        inner_rect.y(),
        target_widths.left,
        inner_rect.height(),
      ),
      BorderImageRepeat::Stretch,
      repeat_y,
      opacity,
      clip,
      transform,
    );
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx2, sy1, sx3 - sx2, sy2 - sy1),
      Rect::from_xywh(
        outer_rect.x() + outer_rect.width() - target_widths.right,
        inner_rect.y(),
        target_widths.right,
        inner_rect.height(),
      ),
      BorderImageRepeat::Stretch,
      repeat_y,
      opacity,
      clip,
      transform,
    );

    if border_image.slice.fill {
      self.paint_border_patch(
        &pixmap,
        Rect::from_xywh(sx1, sy1, sx2 - sx1, sy2 - sy1),
        inner_rect,
        repeat_x,
        repeat_y,
        opacity,
        clip,
        transform,
      );
    }

    true
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
        // When the stroke is too thin to draw two lines and a gap, fall back to a solid stroke.
        if side.width < 3.0 {
          pixmap.stroke_path(&base_path, &paint, &stroke, transform, clip);
          return;
        }

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

  fn paint_border_patch(
    &mut self,
    source: &Pixmap,
    src_rect: Rect,
    dest_rect: Rect,
    repeat_x: BorderImageRepeat,
    repeat_y: BorderImageRepeat,
    opacity: f32,
    clip: Option<&Mask>,
    transform: Transform,
  ) {
    if src_rect.width() <= 0.0
      || src_rect.height() <= 0.0
      || dest_rect.width() <= 0.0
      || dest_rect.height() <= 0.0
    {
      return;
    }

    let sx0 = src_rect.x().max(0.0).floor() as u32;
    let sy0 = src_rect.y().max(0.0).floor() as u32;
    let sx1 = (src_rect.x() + src_rect.width())
      .ceil()
      .min(source.width() as f32)
      .max(0.0) as u32;
    let sy1 = (src_rect.y() + src_rect.height())
      .ceil()
      .min(source.height() as f32)
      .max(0.0) as u32;
    if sx1 <= sx0 || sy1 <= sy0 {
      return;
    }
    let width = sx1 - sx0;
    let height = sy1 - sy0;

    let data = source.data();
    let mut patch = Vec::with_capacity((width * height * 4) as usize);
    for row in sy0..sy1 {
      let start = ((row * source.width() + sx0) * 4) as usize;
      let end = start + (width * 4) as usize;
      patch.extend_from_slice(&data[start..end]);
    }
    let Some(patch_pixmap) = Pixmap::from_vec(patch, IntSize::from_wh(width, height).unwrap())
    else {
      return;
    };

    let mut tile_w = dest_rect.width();
    let mut tile_h = dest_rect.height();

    let mut scale_x = tile_w / width as f32;
    let mut scale_y = tile_h / height as f32;

    if repeat_x != BorderImageRepeat::Stretch {
      scale_x = scale_y;
      tile_w = width as f32 * scale_x;
    }
    if repeat_y != BorderImageRepeat::Stretch {
      scale_y = scale_x;
      tile_h = height as f32 * scale_y;
    }

    if repeat_x == BorderImageRepeat::Round && tile_w > 0.0 {
      let count = (dest_rect.width() / tile_w).round().max(1.0);
      tile_w = dest_rect.width() / count;
      scale_x = tile_w / width as f32;
    }
    if repeat_y == BorderImageRepeat::Round && tile_h > 0.0 {
      let count = (dest_rect.height() / tile_h).round().max(1.0);
      tile_h = dest_rect.height() / count;
      scale_y = tile_h / height as f32;
    }

    let positions_x = match repeat_x {
      BorderImageRepeat::Stretch => vec![dest_rect.x()],
      BorderImageRepeat::Round => {
        let mut pos = Vec::new();
        let mut cursor = dest_rect.x();
        let end = dest_rect.x() + dest_rect.width();
        if tile_w <= 0.0 {
          return;
        }
        while cursor < end - 1e-3 {
          pos.push(cursor);
          cursor += tile_w;
        }
        pos
      }
      BorderImageRepeat::Space => {
        if tile_w <= 0.0 {
          return;
        }
        let count = (dest_rect.width() / tile_w).floor();
        if count < 1.0 {
          vec![dest_rect.x() + (dest_rect.width() - tile_w) * 0.5]
        } else if count < 2.0 {
          vec![dest_rect.x() + (dest_rect.width() - tile_w) * 0.5]
        } else {
          let spacing = (dest_rect.width() - tile_w * count) / (count - 1.0);
          let mut pos = Vec::with_capacity(count as usize);
          let mut cursor = dest_rect.x();
          for _ in 0..(count as usize) {
            pos.push(cursor);
            cursor += tile_w + spacing;
          }
          pos
        }
      }
      BorderImageRepeat::Repeat => {
        let mut pos = Vec::new();
        let mut cursor = dest_rect.x();
        let end = dest_rect.x() + dest_rect.width();
        if tile_w <= 0.0 {
          return;
        }
        while cursor < end - 1e-3 {
          pos.push(cursor);
          cursor += tile_w;
        }
        pos
      }
    };

    let positions_y = match repeat_y {
      BorderImageRepeat::Stretch => vec![dest_rect.y()],
      BorderImageRepeat::Round => {
        let mut pos = Vec::new();
        let mut cursor = dest_rect.y();
        let end = dest_rect.y() + dest_rect.height();
        if tile_h <= 0.0 {
          return;
        }
        while cursor < end - 1e-3 {
          pos.push(cursor);
          cursor += tile_h;
        }
        pos
      }
      BorderImageRepeat::Space => {
        if tile_h <= 0.0 {
          return;
        }
        let count = (dest_rect.height() / tile_h).floor();
        if count < 1.0 {
          vec![dest_rect.y() + (dest_rect.height() - tile_h) * 0.5]
        } else if count < 2.0 {
          vec![dest_rect.y() + (dest_rect.height() - tile_h) * 0.5]
        } else {
          let spacing = (dest_rect.height() - tile_h * count) / (count - 1.0);
          let mut pos = Vec::with_capacity(count as usize);
          let mut cursor = dest_rect.y();
          for _ in 0..(count as usize) {
            pos.push(cursor);
            cursor += tile_h + spacing;
          }
          pos
        }
      }
      BorderImageRepeat::Repeat => {
        let mut pos = Vec::new();
        let mut cursor = dest_rect.y();
        let end = dest_rect.y() + dest_rect.height();
        if tile_h <= 0.0 {
          return;
        }
        while cursor < end - 1e-3 {
          pos.push(cursor);
          cursor += tile_h;
        }
        pos
      }
    };

    let clip_rect = dest_rect;
    for ty in positions_y.iter().copied() {
      for tx in positions_x.iter().copied() {
        let tile_rect = Rect::from_xywh(tx, ty, tile_w, tile_h);
        let Some(intersection) = tile_rect.intersection(clip_rect) else {
          continue;
        };
        if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
          continue;
        }
        let Some(src_rect) = tiny_skia::Rect::from_xywh(
          intersection.x(),
          intersection.y(),
          intersection.width(),
          intersection.height(),
        ) else {
          continue;
        };
        let mut paint = tiny_skia::Paint::default();
        paint.shader = Pattern::new(
          patch_pixmap.as_ref(),
          SpreadMode::Pad,
          tiny_skia::FilterQuality::Bilinear,
          opacity,
          Transform::from_row(scale_x, 0.0, 0.0, scale_y, tx, ty),
        );
        paint.anti_alias = false;
        paint.blend_mode = self.canvas.blend_mode();
        self
          .canvas
          .pixmap_mut()
          .fill_rect(src_rect, &paint, transform, clip);
      }
    }
  }

  fn render_mask(&self, mask: &ResolvedMask) -> Option<Mask> {
    let viewport = (
      self.canvas.width() as f32 / self.scale,
      self.canvas.height() as f32 / self.scale,
    );
    let rects = mask.rects;
    let mut combined: Option<Mask> = None;
    let canvas_clip = Rect::from_xywh(0.0, 0.0, viewport.0, viewport.1);

    for layer in mask.layers.iter().rev() {
      let origin_rect_css = match layer.origin {
        MaskOrigin::BorderBox => rects.border,
        MaskOrigin::PaddingBox => rects.padding,
        MaskOrigin::ContentBox => rects.content,
      };
      let clip_rect_css = match layer.clip {
        MaskClip::BorderBox => rects.border,
        MaskClip::PaddingBox => rects.padding,
        MaskClip::ContentBox | MaskClip::Text => rects.content,
        MaskClip::NoClip => canvas_clip,
      };
      if origin_rect_css.width() <= 0.0
        || origin_rect_css.height() <= 0.0
        || clip_rect_css.width() <= 0.0
        || clip_rect_css.height() <= 0.0
      {
        continue;
      }

      let (img_w, img_h) = match &layer.image {
        ResolvedMaskImage::Raster(image) => (image.css_width, image.css_height),
        ResolvedMaskImage::Generated(_) => (0.0, 0.0),
      };
      let (mut tile_w, mut tile_h) = compute_background_size_from_value(
        &layer.size,
        mask.font_size,
        mask.root_font_size,
        viewport,
        origin_rect_css.width(),
        origin_rect_css.height(),
        img_w,
        img_h,
      );
      if tile_w <= 0.0 || tile_h <= 0.0 {
        continue;
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
        let aspect = if img_w > 0.0 && img_h > 0.0 {
          img_w / img_h
        } else {
          1.0
        };
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
        mask.font_size,
        mask.root_font_size,
        viewport,
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

      let tile = match &layer.image {
        ResolvedMaskImage::Generated(image) => {
          let pixmap_w = tile_w.ceil().max(1.0) as u32;
          let pixmap_h = tile_h.ceil().max(1.0) as u32;
          render_generated_border_image(
            image,
            mask.color,
            pixmap_w,
            pixmap_h,
            mask.font_size,
            mask.root_font_size,
            Some(viewport),
          )
        }
        ResolvedMaskImage::Raster(image) => image_data_to_pixmap(image),
      };
      let Some(tile) = tile else { continue };
      let Some(mask_tile) = mask_tile_from_image(&tile, layer.mode) else {
        continue;
      };

      let mut mask_pixmap = Pixmap::new(self.canvas.width(), self.canvas.height())?;
      for ty in positions_y.iter().copied() {
        for tx in positions_x.iter().copied() {
          paint_mask_tile(
            &mut mask_pixmap,
            &mask_tile,
            tx,
            ty,
            tile_w,
            tile_h,
            clip_rect_css,
            self.scale,
          );
        }
      }

      let layer_mask = Mask::from_pixmap(mask_pixmap.as_ref(), MaskType::Alpha);

      if let Some(dest) = combined.as_mut() {
        apply_mask_composite(dest, &layer_mask, layer.composite);
      } else {
        combined = Some(layer_mask);
      }
    }

    combined
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
          let pos = if s.position.is_finite() {
            s.position.clamp(0.0, 1.0)
          } else {
            0.0
          };
          let alpha = (s.color.a * opacity * 255.0).round().clamp(0.0, 255.0) as u8;
          SkiaGradientStop::new(
            pos,
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
    self
      .canvas
      .pixmap_mut()
      .draw_pixmap(0, 0, temp.as_ref(), &paint, transform, clip.as_ref());
  }

  fn composite_manual_layer(
    &mut self,
    layer: &Pixmap,
    opacity: f32,
    mode: BlendMode,
    origin: (i32, i32),
    region: Option<&Rect>,
  ) -> Result<()> {
    let target = self.canvas.pixmap_mut();
    let opacity = opacity.clamp(0.0, 1.0);
    if opacity == 0.0 {
      return Ok(());
    }

    let (mut x0, mut y0, mut x1, mut y1) = (
      origin.0,
      origin.1,
      origin.0 + layer.width() as i32,
      origin.1 + layer.height() as i32,
    );
    if let Some(region) = region {
      let (rx0, ry0, rx1, ry1) = rect_int_bounds(region);
      x0 = x0.max(rx0);
      y0 = y0.max(ry0);
      x1 = x1.min(rx1);
      y1 = y1.min(ry1);
    }
    x0 = x0.max(0);
    y0 = y0.max(0);
    x1 = x1
      .min(target.width() as i32)
      .min(origin.0 + layer.width() as i32);
    y1 = y1
      .min(target.height() as i32)
      .min(origin.1 + layer.height() as i32);
    if x0 >= x1 || y0 >= y1 {
      return Ok(());
    }

    let src_pixels = layer.pixels();
    let src_stride = layer.width() as usize;
    let dst_stride = target.width() as usize;
    let dst_pixels = target.pixels_mut();

    for y in y0..y1 {
      let src_row = (y - origin.1) as usize;
      let dst_row = y as usize;
      let src_start = src_row * src_stride + (x0 - origin.0) as usize;
      let dst_start = dst_row * dst_stride + x0 as usize;
      let width = (x1 - x0) as usize;
      let src_slice = &src_pixels[src_start..src_start + width];
      let dst_slice = &mut dst_pixels[dst_start..dst_start + width];
      for (src_px, dst_px) in src_slice.iter().zip(dst_slice.iter_mut()) {
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

        let blended_rgb = apply_manual_blend(mode, src_rgb, dst_rgb);

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
        *dst_px = PremultipliedColorU8::from_rgba(r, g, b, out_a_u8)
          .unwrap_or(PremultipliedColorU8::TRANSPARENT);
      }
    }

    Ok(())
  }

  /// Consumes the renderer and returns the painted pixmap.
  pub fn render(mut self, list: &DisplayList) -> Result<Pixmap> {
    let list = crate::paint::preserve_3d::composite_preserve_3d(list);
    self.render_slice(list.items())?;
    Ok(self.canvas.into_pixmap())
  }

  fn apply_pending_backdrop(&mut self, next_item: &DisplayItem) {
    let Some(record) = self.stacking_layers.last_mut() else {
      return;
    };
    if record.pending_backdrop.is_none() {
      return;
    }
    if matches!(next_item, DisplayItem::PushClip(_)) {
      return;
    }

    let pending = record.pending_backdrop.take().unwrap();
    let clip_mask = self.canvas.clip_mask().cloned();
    let clip_bounds = self.canvas.clip_bounds();
    apply_backdrop_filters(
      self.canvas.backdrop_pixmap_mut(),
      &pending.bounds,
      &pending.filters,
      pending.radii,
      self.scale,
      clip_mask.as_ref(),
      clip_bounds,
      pending.filter_bounds,
    );
  }
  fn render_slice(&mut self, items: &[DisplayItem]) -> Result<()> {
    let mut idx = 0;
    while idx < items.len() {
      if !self.preserve_3d_disabled {
        if let DisplayItem::PushStackingContext(sc) = &items[idx] {
          if matches!(sc.transform_style, TransformStyle::Preserve3d) {
            // `render_item` normally applies pending backdrop filters before processing the next
            // item. We skip `render_item` for preserve-3d roots to render the subtree as a
            // depth-sorted scene, so apply pending backdrops here to keep behavior consistent.
            self.apply_pending_backdrop(&items[idx]);
            idx = self.render_preserve_3d_context(items, idx)? + 1;
            continue;
          }
        }
      }
      self.render_item(&items[idx])?;
      idx += 1;
    }
    Ok(())
  }

  fn render_preserve_3d_context(
    &mut self,
    items: &[DisplayItem],
    start_index: usize,
  ) -> Result<usize> {
    let Some((node, end_idx)) = parse_stacking_node(items, start_index) else {
      // Fallback to normal rendering if the tree is malformed.
      self.render_item(&items[start_index])?;
      return Ok(start_index);
    };

    let parent_transform = *self
      .transform_stack
      .last()
      .unwrap_or(&Transform3D::identity());
    let local_transform = node.context.transform.unwrap_or_else(Transform3D::identity);
    let combined_root = parent_transform.multiply(&local_transform);

    if matches!(node.context.backface_visibility, BackfaceVisibility::Hidden)
      && backface_is_hidden(&combined_root)
    {
      return Ok(end_idx);
    }

    let mut order = 0;
    let mut scene_items = collect_scene_items(&node, &parent_transform, true, &mut order);
    scene_items.retain(|item| {
      !matches!(item.backface_visibility, BackfaceVisibility::Hidden)
        || !backface_is_hidden(&item.transform)
    });
    scene_items.sort_by(|a, b| {
      let da = scene_depth(&a.transform, a.bounds);
      let db = scene_depth(&b.transform, b.bounds);
      da.partial_cmp(&db)
        .unwrap_or(std::cmp::Ordering::Equal)
        .then_with(|| a.order.cmp(&b.order))
    });

    for scene_item in scene_items {
      if let Some(pixmap) = self.render_scene_item(&scene_item)? {
        let adjusted_transform = scene_item.transform.multiply(&Transform3D::translate(
          scene_item.bounds.x(),
          scene_item.bounds.y(),
          0.0,
        ));
        self.warp_pixmap(&pixmap, &adjusted_transform);
      }
    }

    Ok(end_idx)
  }

  fn render_scene_item(&self, item: &SceneItem) -> Result<Option<Pixmap>> {
    let bounds = item.bounds;
    if bounds.width() <= 0.0 || bounds.height() <= 0.0 {
      return Ok(None);
    }

    let width = (bounds.width() * self.scale).ceil().max(1.0) as u32;
    let height = (bounds.height() * self.scale).ceil().max(1.0) as u32;

    let mut list = DisplayList::new();
    let translation = Transform3D::translate(-bounds.x(), -bounds.y(), 0.0);
    list.push(DisplayItem::PushTransform(TransformItem {
      transform: translation,
    }));
    match &item.source {
      SceneItemSource::Segment(items) => {
        for it in items {
          list.push(it.clone());
        }
      }
      SceneItemSource::FlattenedSubtree(node) => {
        for (i, it) in node.subtree_items.iter().enumerate() {
          if i == 0 {
            if let DisplayItem::PushStackingContext(sc) = it {
              let mut adjusted = sc.clone();
              adjusted.transform = None;
              list.push(DisplayItem::PushStackingContext(adjusted));
              continue;
            }
          }
          list.push(it.clone());
        }
      }
    }
    list.push(DisplayItem::PopTransform);

    let mut renderer = DisplayListRenderer::new_scaled(
      width,
      height,
      Rgba::TRANSPARENT,
      self.font_ctx.clone(),
      self.scale,
    )?;
    renderer.preserve_3d_disabled = true;
    let pixmap = renderer.render(&list)?;
    Ok(Some(pixmap))
  }

  fn warp_pixmap(&mut self, pixmap: &Pixmap, transform: &Transform3D) {
    let mut paint = PixmapPaint::default();
    paint.blend_mode = self.canvas.blend_mode();

    let parent_transform = self.canvas.transform();

    let homography = Homography::from_transform3d_z0(transform);
    if homography.is_affine() {
      let clip = self.canvas.clip_mask().cloned();
      let (t, _valid) = self.to_skia_transform_checked(transform);
      let skia_transform = parent_transform.post_concat(t);
      self.canvas.pixmap_mut().draw_pixmap(
        0,
        0,
        pixmap.as_ref(),
        &paint,
        skia_transform,
        clip.as_ref(),
      );
      return;
    }

    let src_w = pixmap.width() as f32;
    let src_h = pixmap.height() as f32;
    if src_w <= 0.0 || src_h <= 0.0 {
      return;
    }
    let css_w = src_w / self.scale;
    let css_h = src_h / self.scale;

    let corners = [(0.0, 0.0), (css_w, 0.0), (css_w, css_h), (0.0, css_h)];
    let mut dst_quad_points = [Point::ZERO; 4];
    let mut dst_quad = [(0.0f32, 0.0f32); 4];
    let mut valid = true;

    for (i, (x, y)) in corners.iter().enumerate() {
      let (tx, ty, _tz, tw) = transform.transform_point(*x, *y, 0.0);
      if tw.abs() < Transform3D::MIN_PROJECTIVE_W {
        valid = false;
        break;
      }
      let px = (tx / tw) * self.scale;
      let py = (ty / tw) * self.scale;
      let dst_x = px * parent_transform.sx + py * parent_transform.kx + parent_transform.tx;
      let dst_y = px * parent_transform.ky + py * parent_transform.sy + parent_transform.ty;
      dst_quad[i] = (dst_x, dst_y);
      dst_quad_points[i] = Point::new(dst_x, dst_y);
    }

    let src_quad = [
      Point::new(0.0, 0.0),
      Point::new(src_w, 0.0),
      Point::new(src_w, src_h),
      Point::new(0.0, src_h),
    ];
    let warped = valid
      .then(|| Homography::from_quad_to_quad(src_quad, dst_quad_points))
      .flatten()
      .and_then(|h| {
        warp_pixmap(
          pixmap,
          &h,
          &dst_quad,
          (self.canvas.width(), self.canvas.height()),
          self.canvas.clip_mask(),
        )
      });

    if let Some(warped) = warped {
      self.canvas.pixmap_mut().draw_pixmap(
        warped.offset.0,
        warped.offset.1,
        warped.pixmap.as_ref(),
        &paint,
        Transform::identity(),
        None,
      );
      return;
    }

    // Fall back to the affine approximation when a stable projective warp can't be computed.
    let clip = self.canvas.clip_mask().cloned();
    let (t, _valid) = self.to_skia_transform_checked(transform);
    let skia_transform = parent_transform.post_concat(t);
    self.canvas.pixmap_mut().draw_pixmap(
      0,
      0,
      pixmap.as_ref(),
      &paint,
      skia_transform,
      clip.as_ref(),
    );
  }

  fn render_item(&mut self, item: &DisplayItem) -> Result<()> {
    if self.culled_depth > 0 {
      match item {
        DisplayItem::PushStackingContext(_) => {
          self.culled_depth += 1;
        }
        DisplayItem::PopStackingContext => {
          if self.culled_depth > 0 {
            self.culled_depth -= 1;
          }
        }
        _ => {}
      }
      return Ok(());
    }

    self.apply_pending_backdrop(item);

    match item {
      DisplayItem::FillRect(FillRectItem { rect, color }) => {
        self.canvas.draw_rect(self.ds_rect(*rect), *color)
      }
      DisplayItem::StrokeRect(StrokeRectItem {
        rect,
        color,
        width,
        blend_mode,
      }) => self.canvas.stroke_rect_with_blend(
        self.ds_rect(*rect),
        *color,
        self.ds_len(*width),
        *blend_mode,
      ),
      DisplayItem::FillRoundedRect(item) => {
        self.canvas.draw_rounded_rect(
          self.ds_rect(item.rect),
          self.ds_radii(item.radii),
          item.color,
        );
      }
      DisplayItem::StrokeRoundedRect(item) => self.canvas.stroke_rounded_rect(
        self.ds_rect(item.rect),
        self.ds_radii(item.radii),
        item.color,
        self.ds_len(item.width),
      ),
      DisplayItem::Outline(item) => self.render_outline(item),
      DisplayItem::LinearGradient(item) => self.render_linear_gradient(item),
      DisplayItem::RadialGradient(item) => self.render_radial_gradient(item),
      DisplayItem::ConicGradient(item) => self.render_conic_gradient(item),
      DisplayItem::Border(item) => self.render_border(item),
      DisplayItem::TextDecoration(item) => {
        let scaled = self.scale_decoration_item(item);
        self.render_text_decoration(&scaled)?;
      }
      DisplayItem::Text(item) => {
        let scaled = self.scale_text_item(item);
        self.render_text(&scaled)?;
      }
      DisplayItem::Image(item) => self.render_image(item)?,
      DisplayItem::BoxShadow(item) => self.render_box_shadow(item),
      DisplayItem::ListMarker(item) => {
        let scaled = self.scale_list_marker_item(item);
        self.render_list_marker(&scaled)?;
      }
      DisplayItem::PushStackingContext(item) => {
        let parent_child_transform = *self
          .transform_stack
          .last()
          .unwrap_or(&Transform3D::identity());
        let parent_perspective = *self
          .perspective_stack
          .last()
          .unwrap_or(&Transform3D::identity());
        let local_transform = item.transform.unwrap_or(Transform3D::identity());
        let painting_transform_3d = parent_child_transform.multiply(&local_transform);

        if matches!(item.backface_visibility, BackfaceVisibility::Hidden)
          && backface_is_hidden(&painting_transform_3d)
        {
          self.culled_depth = 1;
          return Ok(());
        }

        let warp_candidate = parent_perspective.multiply(&local_transform);
        let projective_transform = {
          let h = Homography::from_transform3d_z0(&warp_candidate);
          (!h.is_affine()).then_some(warp_candidate)
        };

        let child_base_transform = if let Some(perspective) = item.child_perspective.as_ref() {
          painting_transform_3d.multiply(perspective)
        } else {
          painting_transform_3d
        };

        let scaled_filters = self.ds_filters(&item.filters);
        let scaled_backdrop = self.ds_filters(&item.backdrop_filters);
        let has_backdrop = !scaled_backdrop.is_empty();
        let css_bounds = item.bounds;
        let bounds = self.ds_rect(css_bounds);
        let radii = self.ds_radii(item.radii);
        let mask = item.mask.clone();

        let is_isolated = item.is_isolated || has_backdrop;
        let manual_blend = if is_manual_blend(item.mix_blend_mode) {
          Some(item.mix_blend_mode)
        } else {
          None
        };

        let parent_transform = self.canvas.transform();
        let mut local_skia_transform: Option<Transform> = None;
        let mut combined_transform = parent_transform;
        if projective_transform.is_none() && item.transform.is_some() {
          let (t, valid) = self.to_skia_transform_checked(&local_transform);
          if valid {
            local_skia_transform = Some(t);
            combined_transform = combined_transform.post_concat(t);
          }
        }

        self.transform_stack.push(child_base_transform);
        let perspective_base = if projective_transform.is_some() {
          Transform3D::identity()
        } else {
          parent_perspective
        };
        let next_perspective = item
          .child_perspective
          .as_ref()
          .map(|p| perspective_base.multiply(p))
          .unwrap_or(perspective_base);
        self.perspective_stack.push(next_perspective);

        let pending_backdrop = if has_backdrop {
          let backdrop_bounds = if let Some(projective_transform) = projective_transform.as_ref() {
            let projected_css = projective_transform.transform_rect(css_bounds);
            let projected_device = self.ds_rect(projected_css);
            transform_rect(projected_device, &parent_transform)
          } else {
            transform_rect(bounds, &combined_transform)
          };
          Some(PendingBackdrop {
            bounds: backdrop_bounds,
            filters: scaled_backdrop.clone(),
            radii,
            filter_bounds: css_bounds,
          })
        } else {
          None
        };

        let needs_layer = is_isolated
          || !matches!(
            item.mix_blend_mode,
            crate::paint::display_list::BlendMode::Normal
          )
          || !scaled_filters.is_empty()
          || has_backdrop
          || mask.is_some()
          || projective_transform.is_some();
        let layer_bounds = needs_layer
          .then(|| {
            let bounds_transform = if projective_transform.is_some() {
              parent_transform
            } else {
              combined_transform
            };
            self.stacking_layer_bounds(bounds, bounds_transform, &scaled_filters, &scaled_backdrop)
          })
          .flatten();
        let layer_origin = if needs_layer && !has_backdrop {
          layer_bounds.map(|rect| (rect.min_x().floor(), rect.min_y().floor()))
        } else {
          None
        };
        if needs_layer {
          let bounded_rect = if has_backdrop { None } else { layer_bounds };
          if manual_blend.is_some() {
            if let Some(layer_rect) = bounded_rect {
              self.canvas.push_layer_bounded(1.0, None, layer_rect)?;
            } else {
              self.canvas.push_layer(1.0)?;
            }
          } else {
            let blend = if is_isolated {
              tiny_skia::BlendMode::SourceOver
            } else {
              map_blend_mode(item.mix_blend_mode)
            };
            if let Some(layer_rect) = bounded_rect {
              self
                .canvas
                .push_layer_bounded(1.0, Some(blend), layer_rect)?;
            } else {
              self.canvas.push_layer_with_blend(1.0, Some(blend))?;
            }
          }
          if projective_transform.is_some() {
            self.canvas.clear_clip();
          }
        } else {
          self.canvas.save();
        }
        self.stacking_layers.push(StackingRecord {
          needs_layer,
          filters: scaled_filters,
          radii,
          mask_bounds: bounds,
          mask,
          css_bounds,
          manual_blend,
          layer_bounds,
          projective_transform,
          parent_transform,
          culled: false,
          pending_backdrop,
        });

        if let Some(t) = local_skia_transform {
          let mut layer_transform = parent_transform.post_concat(t);
          if let Some((origin_x, origin_y)) = layer_origin {
            layer_transform = layer_transform.post_translate(-origin_x, -origin_y);
          }
          self.canvas.set_transform(layer_transform);
        }
      }
      DisplayItem::PopStackingContext => {
        let record = self
          .stacking_layers
          .pop()
          .unwrap_or_else(|| StackingRecord {
            needs_layer: false,
            filters: Vec::new(),
            radii: BorderRadii::ZERO,
            mask_bounds: Rect::ZERO,
            mask: None,
            css_bounds: Rect::ZERO,
            manual_blend: None,
            layer_bounds: None,
            projective_transform: None,
            parent_transform: Transform::identity(),
            culled: false,
            pending_backdrop: None,
          });

        let _ = self.transform_stack.pop();
        let _ = self.perspective_stack.pop();
        if record.culled {
          if self.culled_depth > 0 {
            self.culled_depth -= 1;
          }
          return Ok(());
        }
        if record.needs_layer {
          let (mut layer, origin, opacity, composite_blend) = self.canvas.pop_layer_raw()?;

          let (out_l, out_t, out_r, out_b) =
            filter_outset_with_bounds(&record.filters, self.scale, Some(record.css_bounds))
              .as_tuple();

          let mut effect_bounds = record.layer_bounds;
          if effect_bounds.is_none() {
            let expanded_bounds = Rect::from_xywh(
              record.mask_bounds.x() - out_l,
              record.mask_bounds.y() - out_t,
              record.mask_bounds.width() + out_l + out_r,
              record.mask_bounds.height() + out_t + out_b,
            );
            if expanded_bounds.width() > 0.0 && expanded_bounds.height() > 0.0 {
              effect_bounds = Some(expanded_bounds);
            }
          }

          let layer_region =
            effect_bounds.and_then(|rect| self.layer_space_bounds(rect, origin, &layer));

          if let Some(mask_style) = record.mask.as_ref() {
            if let Some(mask) = self.render_mask(mask_style) {
              if let Some(cropped) = crop_mask(
                &mask,
                origin.0 as u32,
                origin.1 as u32,
                layer.width(),
                layer.height(),
              ) {
                layer.apply_mask(&cropped);
              } else if mask.width() == layer.width() && mask.height() == layer.height() {
                layer.apply_mask(&mask);
              } else {
                return Ok(());
              }
            }
          }

          if !record.filters.is_empty() {
            let bbox = Rect::from_xywh(
              record.mask_bounds.x() - origin.0 as f32,
              record.mask_bounds.y() - origin.1 as f32,
              record.mask_bounds.width(),
              record.mask_bounds.height(),
            );
            apply_filters_scoped(
              &mut layer,
              &record.filters,
              self.scale,
              bbox,
              layer_region.as_ref(),
            );
          }

          if !record.radii.is_zero() || !record.filters.is_empty() {
            let clip_rect = Rect::from_xywh(
              record.mask_bounds.x() - out_l,
              record.mask_bounds.y() - out_t,
              record.mask_bounds.width() + out_l + out_r,
              record.mask_bounds.height() + out_t + out_b,
            );
            if let Some(local_clip) = self.layer_space_bounds(clip_rect, origin, &layer) {
              apply_clip_mask_rect(&mut layer, local_clip, record.radii);
            }
          }

          if let Some(projective_transform) = record.projective_transform {
            let warp_bounds = if record.filters.is_empty() {
              record.css_bounds
            } else {
              let (out_l, out_t, out_r, out_b) =
                filter_outset_with_bounds(&record.filters, 1.0, Some(record.css_bounds)).as_tuple();
              let expanded = Rect::from_xywh(
                record.css_bounds.x() - out_l,
                record.css_bounds.y() - out_t,
                record.css_bounds.width() + out_l + out_r,
                record.css_bounds.height() + out_t + out_b,
              );
              if expanded.width() > 0.0 && expanded.height() > 0.0 {
                expanded
              } else {
                record.css_bounds
              }
            };

            let corners = [
              (warp_bounds.min_x(), warp_bounds.min_y()),
              (warp_bounds.max_x(), warp_bounds.min_y()),
              (warp_bounds.max_x(), warp_bounds.max_y()),
              (warp_bounds.min_x(), warp_bounds.max_y()),
            ];
            let mut src_quad = [Point::ZERO; 4];
            let mut dst_quad_points = [Point::ZERO; 4];
            let mut dst_quad = [(0.0f32, 0.0f32); 4];
            let mut valid = true;

            for (i, (x, y)) in corners.iter().enumerate() {
              let sx = *x * self.scale;
              let sy = *y * self.scale;
              let src_x = sx * record.parent_transform.sx
                + sy * record.parent_transform.kx
                + record.parent_transform.tx;
              let src_y = sx * record.parent_transform.ky
                + sy * record.parent_transform.sy
                + record.parent_transform.ty;
              src_quad[i] = Point::new(src_x - origin.0 as f32, src_y - origin.1 as f32);

              let (tx, ty, _tz, tw) = projective_transform.transform_point(*x, *y, 0.0);
              if tw.abs() < 1e-6 {
                valid = false;
                break;
              }
              let px = (tx / tw) * self.scale;
              let py = (ty / tw) * self.scale;
              let dst_x = px * record.parent_transform.sx
                + py * record.parent_transform.kx
                + record.parent_transform.tx;
              let dst_y = px * record.parent_transform.ky
                + py * record.parent_transform.sy
                + record.parent_transform.ty;
              dst_quad[i] = (dst_x, dst_y);
              dst_quad_points[i] = Point::new(dst_x, dst_y);
            }

            let warped = valid
              .then(|| Homography::from_quad_to_quad(src_quad, dst_quad_points))
              .flatten()
              .and_then(|homography| {
                warp_pixmap(
                  &layer,
                  &homography,
                  &dst_quad,
                  (self.canvas.width(), self.canvas.height()),
                  self.canvas.clip_mask(),
                )
              });

            if let Some(warped) = warped {
              if let Some(mode) = record.manual_blend {
                self.composite_manual_layer(&warped.pixmap, opacity, mode, warped.offset, None)?;
              } else {
                let mut paint = PixmapPaint::default();
                paint.opacity = opacity;
                paint.blend_mode = composite_blend.unwrap_or(self.canvas.blend_mode());
                self.canvas.pixmap_mut().draw_pixmap(
                  warped.offset.0,
                  warped.offset.1,
                  warped.pixmap.as_ref(),
                  &paint,
                  Transform::identity(),
                  None,
                );
              }
            } else if let Some(mode) = record.manual_blend {
              if let Some(mask) = self.canvas.clip_mask().cloned() {
                let Some(cropped) = crop_mask(
                  &mask,
                  origin.0 as u32,
                  origin.1 as u32,
                  layer.width(),
                  layer.height(),
                ) else {
                  return Ok(());
                };
                layer.apply_mask(&cropped);
              }
              self.composite_manual_layer(&layer, opacity, mode, origin, effect_bounds.as_ref())?;
            } else {
              self
                .canvas
                .composite_layer(&layer, opacity, composite_blend, origin);
            }
          } else if let Some(mode) = record.manual_blend {
            if let Some(mask) = self.canvas.clip_mask().cloned() {
              let Some(cropped) = crop_mask(
                &mask,
                origin.0 as u32,
                origin.1 as u32,
                layer.width(),
                layer.height(),
              ) else {
                return Ok(());
              };
              layer.apply_mask(&cropped);
            }
            self.composite_manual_layer(&layer, opacity, mode, origin, effect_bounds.as_ref())?;
          } else {
            self
              .canvas
              .composite_layer(&layer, opacity, composite_blend, origin);
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
      DisplayItem::PushTransform(transform) => {
        let parent = *self
          .transform_stack
          .last()
          .unwrap_or(&Transform3D::identity());
        let combined = parent.multiply(&transform.transform);
        self.transform_stack.push(combined);
        self.push_transform(transform);
      }
      DisplayItem::PopTransform => {
        let _ = self.transform_stack.pop();
        self.canvas.restore();
      }
      DisplayItem::PushBlendMode(mode) => {
        if is_manual_blend(mode.mode) {
          self.canvas.push_layer(1.0)?;
          self.blend_stack.push(Some(mode.mode));
        } else {
          self
            .canvas
            .push_layer_with_blend(1.0, Some(map_blend_mode(mode.mode)))?;
          self.blend_stack.push(None);
        }
      }
      DisplayItem::PopBlendMode => {
        let blend_mode = self.blend_stack.pop().flatten();
        if let Some(mode) = blend_mode {
          let (layer, origin, opacity, _) = self.canvas.pop_layer_raw()?;
          let region = self.canvas.clip_bounds();
          self.composite_manual_layer(&layer, opacity, mode, origin, region.as_ref())?;
        } else {
          self.canvas.pop_layer()?;
        }
      }
    }

    Ok(())
  }

  fn render_text(&mut self, item: &TextItem) -> Result<()> {
    let Some(font) = self.resolve_font(item.font_id.as_ref()) else {
      return Err(
        RenderError::RasterizationFailed {
          reason: "Unable to resolve font for display list text".into(),
        }
        .into(),
      );
    };

    let face = match font.as_ttf_face() {
      Ok(f) => f,
      Err(_) => {
        return Err(
          RenderError::RasterizationFailed {
            reason: "Unable to parse font face for text shadows".into(),
          }
          .into(),
        )
      }
    };

    let (glyphs, bounds) = if item.shadows.is_empty() {
      (Vec::new(), PathBounds::new())
    } else {
      self.glyph_shadow_sources(&face, &font, item)
    };
    if !glyphs.is_empty() && bounds.is_valid() {
      self.render_text_shadows(&glyphs, &bounds, item);
    }

    let glyphs: Vec<GlyphPosition> = item
      .glyphs
      .iter()
      .map(|g| GlyphPosition {
        glyph_id: g.glyph_id,
        cluster: 0,
        x_offset: g.offset.x,
        y_offset: g.offset.y,
        x_advance: g.advance,
        y_advance: 0.0,
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

  fn render_list_marker(&mut self, item: &ListMarkerItem) -> Result<()> {
    if let Some(bg) = item.background {
      let bounds = crate::paint::display_list::list_marker_bounds(item);
      self.canvas.draw_rect(bounds, bg);
    }

    let text = TextItem {
      origin: item.origin,
      glyphs: item.glyphs.clone(),
      color: item.color,
      shadows: item.shadows.clone(),
      font_size: item.font_size,
      advance_width: item.advance_width,
      font_id: item.font_id.clone(),
      synthetic_bold: item.synthetic_bold,
      synthetic_oblique: item.synthetic_oblique,
      emphasis: item.emphasis.clone(),
      decorations: Vec::new(),
    };
    self.render_text(&text)
  }

  fn render_text_decoration(
    &mut self,
    item: &crate::paint::display_list::TextDecorationItem,
  ) -> Result<()> {
    if item.decorations.is_empty() || item.line_width <= 0.0 {
      return Ok(());
    }

    let inline_vertical = item.inline_vertical;
    let inline_start = item.line_start;
    let inline_len = item.line_width;
    let clip = self.canvas.clip_mask().cloned();
    let transform = self.canvas.transform();
    let blend_mode = self.canvas.blend_mode();
    let pixmap = self.canvas.pixmap_mut();

    let draw_solid_line = |pixmap: &mut Pixmap,
                           paint: &tiny_skia::Paint,
                           start: f32,
                           len: f32,
                           center: f32,
                           thickness: f32| {
      if thickness <= 0.0 || len <= 0.0 {
        return;
      }
      let rect = if inline_vertical {
        tiny_skia::Rect::from_xywh(center - thickness * 0.5, start, thickness, len)
      } else {
        tiny_skia::Rect::from_xywh(start, center - thickness * 0.5, len, thickness)
      };
      if let Some(rect) = rect {
        let path = PathBuilder::from_rect(rect);
        pixmap.fill_path(
          &path,
          paint,
          tiny_skia::FillRule::Winding,
          transform,
          clip.as_ref(),
        );
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
      if inline_vertical {
        path.move_to(center, start);
        path.line_to(center, start + len);
      } else {
        path.move_to(start, center);
        path.line_to(start + len, center);
      }
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

    let draw_wavy_line = |pixmap: &mut Pixmap,
                          paint: &tiny_skia::Paint,
                          start: f32,
                          len: f32,
                          center: f32,
                          thickness: f32| {
      if thickness <= 0.0 || len <= 0.0 {
        return;
      }
      let wavelength = (thickness * 4.0).max(6.0);
      let amplitude = (thickness * 0.75).max(thickness * 0.5);

      let mut path = PathBuilder::new();
      if inline_vertical {
        path.move_to(center, start);
      } else {
        path.move_to(start, center);
      }
      let mut cursor = start;
      let mut up = true;
      while cursor < start + len {
        let end = (cursor + wavelength).min(start + len);
        let mid = cursor + (end - cursor) * 0.5;
        if inline_vertical {
          let control_x = if up {
            center - amplitude
          } else {
            center + amplitude
          };
          path.quad_to(control_x, mid, center, end);
        } else {
          let control_y = if up {
            center - amplitude
          } else {
            center + amplitude
          };
          path.quad_to(mid, control_y, end, center);
        }
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
        draw_solid_line(
          pixmap,
          paint,
          start,
          len,
          center - (gap * 0.5),
          line_thickness,
        );
        draw_solid_line(
          pixmap,
          paint,
          start,
          len,
          center + (gap * 0.5),
          line_thickness,
        );
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
              item.line_start + *start,
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
            inline_start,
            inline_len,
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
          inline_start,
          inline_len,
          overline.center,
          overline.thickness,
        );
      }
      if let Some(strike) = &deco.line_through {
        render_line(
          pixmap,
          &paint,
          deco.style,
          inline_start,
          inline_len,
          strike.center,
          strike.thickness,
        );
      }
    }

    Ok(())
  }

  fn glyph_shadow_sources(
    &mut self,
    face: &ttf_parser::Face<'_>,
    font: &LoadedFont,
    item: &TextItem,
  ) -> (Vec<GlyphShadowSource>, PathBounds) {
    let units_per_em = face.units_per_em() as f32;
    let scale = item.font_size / units_per_em;
    let mut glyphs = Vec::with_capacity(item.glyphs.len());
    let mut bounds = PathBounds::new();

    for glyph in &item.glyphs {
      let origin = Point::new(
        item.origin.x + glyph.offset.x,
        item.origin.y + glyph.offset.y,
      );
      let color_glyph = self.text_rasterizer.get_color_glyph(
        font,
        glyph.glyph_id,
        item.font_size,
        0,
        &[],
        item.color,
        item.synthetic_oblique,
      );
      let mut path = None;
      if color_glyph.is_none() {
        if let Some(p) = Self::build_glyph_path(
          face,
          glyph.glyph_id as u16,
          origin.x,
          origin.y,
          scale,
          item.synthetic_oblique,
        ) {
          bounds.include(&p.bounds());
          path = Some(p);
        }
      }

      if let Some(ref color) = color_glyph {
        if let Some(rect) = tiny_skia::Rect::from_xywh(
          origin.x + color.left,
          origin.y + color.top,
          color.image.width() as f32,
          color.image.height() as f32,
        ) {
          bounds.include(&rect);
        }
      }

      if path.is_none() && color_glyph.is_none() {
        continue;
      }

      glyphs.push(GlyphShadowSource {
        path,
        color_glyph,
        origin,
      });
    }

    (glyphs, bounds)
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
        self.builder.move_to(
          self.x + (px + self.skew * py) * self.scale,
          self.y - py * self.scale,
        );
      }

      fn line_to(&mut self, px: f32, py: f32) {
        self.builder.line_to(
          self.x + (px + self.skew * py) * self.scale,
          self.y - py * self.scale,
        );
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

  fn render_text_shadows(
    &mut self,
    glyphs: &[GlyphShadowSource],
    bounds: &PathBounds,
    item: &TextItem,
  ) {
    if glyphs.is_empty() {
      return;
    }
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
      for glyph in glyphs {
        if let Some(color) = &glyph.color_glyph {
          self.draw_color_shadow(
            &mut shadow_pixmap,
            color,
            glyph.origin,
            translate_x,
            translate_y,
            shadow.color,
            opacity,
          );
          continue;
        }
        if let Some(path) = glyph.path.as_ref() {
          shadow_pixmap.fill_path(path, &paint, tiny_skia::FillRule::EvenOdd, transform, None);
        }
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
      let transform =
        Transform::from_translate(frac_x, frac_y).post_concat(self.canvas.transform());
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

  fn draw_color_shadow(
    &self,
    target: &mut Pixmap,
    glyph: &ColorGlyphRaster,
    origin: Point,
    translate_x: f32,
    translate_y: f32,
    shadow_color: Rgba,
    opacity: f32,
  ) {
    let width = glyph.image.width();
    let height = glyph.image.height();
    if width == 0 || height == 0 {
      return;
    }

    let alpha_scale = (shadow_color.a * opacity).clamp(0.0, 1.0);
    if alpha_scale <= 0.0 {
      return;
    }

    let mut data = Vec::with_capacity((width * height * 4) as usize);
    for pixel in glyph.image.data().chunks_exact(4) {
      let glyph_alpha = pixel[3] as f32 / 255.0;
      let final_alpha = glyph_alpha * alpha_scale;
      let final_a_u8 = (final_alpha * 255.0).round().clamp(0.0, 255.0) as u8;
      let premul = |c: u8| (c as f32 * final_alpha).round().clamp(0.0, 255.0) as u8;
      data.push(premul(shadow_color.b));
      data.push(premul(shadow_color.g));
      data.push(premul(shadow_color.r));
      data.push(final_a_u8);
    }

    let Some(size) = IntSize::from_wh(width, height) else {
      return;
    };
    let Some(tinted) = Pixmap::from_vec(data, size) else {
      return;
    };

    let draw_x = origin.x + glyph.left + translate_x;
    let draw_y = origin.y + glyph.top + translate_y;
    let dest_x = draw_x.floor() as i32;
    let dest_y = draw_y.floor() as i32;
    let frac_x = draw_x - dest_x as f32;
    let frac_y = draw_y - dest_y as f32;

    let mut paint = tiny_skia::PixmapPaint::default();
    paint.opacity = 1.0;
    paint.blend_mode = tiny_skia::BlendMode::SourceOver;

    target.draw_pixmap(
      dest_x,
      dest_y,
      tinted.as_ref(),
      &paint,
      Transform::from_translate(frac_x, frac_y),
      None,
    );
  }

  fn render_emphasis(&mut self, emphasis: &TextEmphasis) -> Result<()> {
    if emphasis.marks.is_empty() {
      return Ok(());
    }

    let inline_vertical = emphasis.inline_vertical;
    if let TextEmphasisStyle::String(_) = emphasis.style {
      if let Some(text) = &emphasis.text {
        let font = self.resolve_font(text.font_id.as_ref()).ok_or_else(|| {
          RenderError::RasterizationFailed {
            reason: "Unable to resolve font for emphasis string".into(),
          }
        })?;
        let glyphs: Vec<GlyphPosition> = text
          .glyphs
          .iter()
          .map(|g| GlyphPosition {
            glyph_id: g.glyph_id,
            cluster: 0,
            x_offset: g.offset.x,
            y_offset: g.offset.y,
            x_advance: g.advance,
            y_advance: 0.0,
          })
          .collect();
        for mark in &emphasis.marks {
          let mark_origin = if inline_vertical {
            Point::new(
              mark.center.x - text.height * 0.5 + text.baseline_offset,
              mark.center.y - text.width * 0.5,
            )
          } else {
            Point::new(
              mark.center.x - text.width * 0.5,
              mark.center.y - text.height * 0.5 + text.baseline_offset,
            )
          };
          self.canvas.draw_text(
            mark_origin,
            &glyphs,
            &font,
            text.font_size,
            emphasis.color,
            0.0,
            0.0,
          );
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
          if let Some(path) =
            tiny_skia::PathBuilder::from_circle(mark.center.x, mark.center.y, radius)
          {
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
                self.canvas.pixmap_mut().stroke_path(
                  &path,
                  &paint,
                  &stroke,
                  transform,
                  clip.as_ref(),
                );
              }
            }
          }
        }
        TextEmphasisStyle::Mark {
          fill,
          shape: TextEmphasisShape::Circle,
        } => {
          let radius = emphasis.size * 0.5;
          if let Some(path) =
            tiny_skia::PathBuilder::from_circle(mark.center.x, mark.center.y, radius)
          {
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
                self.canvas.pixmap_mut().stroke_path(
                  &path,
                  &paint,
                  &stroke,
                  transform,
                  clip.as_ref(),
                );
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
            if let Some(path) =
              tiny_skia::PathBuilder::from_circle(mark.center.x, mark.center.y, radius)
            {
              self.canvas.pixmap_mut().stroke_path(
                &path,
                &paint,
                &stroke,
                transform,
                clip.as_ref(),
              );
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
            TextEmphasisPosition::Over
              | TextEmphasisPosition::OverLeft
              | TextEmphasisPosition::OverRight
          );
          let mut builder = tiny_skia::PathBuilder::new();
          if inline_vertical {
            let apex_x = if direction {
              mark.center.x - height * 0.5
            } else {
              mark.center.x + height * 0.5
            };
            let base_x = if direction {
              mark.center.x + height * 0.5
            } else {
              mark.center.x - height * 0.5
            };
            builder.move_to(apex_x, mark.center.y);
            builder.line_to(base_x, mark.center.y - half);
            builder.line_to(base_x, mark.center.y + half);
          } else {
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
            builder.move_to(mark.center.x, apex_y);
            builder.line_to(mark.center.x - half, base_y);
            builder.line_to(mark.center.x + half, base_y);
          }
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
                self.canvas.pixmap_mut().stroke_path(
                  &path,
                  &paint,
                  &stroke,
                  transform,
                  clip.as_ref(),
                );
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
          if inline_vertical {
            builder.move_to(mark.center.x - dx.1, mark.center.y - dx.0);
            builder.line_to(mark.center.x + dx.1, mark.center.y + dx.0);
          } else {
            builder.move_to(mark.center.x - dx.0, mark.center.y - dx.1);
            builder.line_to(mark.center.x + dx.0, mark.center.y + dx.1);
          }
          if let Some(path) = builder.finish() {
            let mut stroke = tiny_skia::Stroke::default();
            stroke.width = (emphasis.size * 0.2).max(0.6);
            stroke.line_cap = tiny_skia::LineCap::Round;
            self
              .canvas
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
    let mut dest_rect = self.ds_rect(item.dest_rect);
    if self.canvas.apply_clip(dest_rect).is_none() {
      return Ok(());
    }

    let Some(pixmap) = self.image_to_pixmap(item) else {
      return Ok(());
    };

    if dest_rect.width() <= 0.0 || dest_rect.height() <= 0.0 {
      return Ok(());
    }

    if matches!(item.filter_quality, ImageFilterQuality::Nearest)
      && (dest_rect.width() > pixmap.width() as f32 || dest_rect.height() > pixmap.height() as f32)
    {
      let (snapped_w, offset_x) =
        crate::paint::painter::snap_upscale(dest_rect.width(), pixmap.width() as f32)
          .unwrap_or_else(|| (dest_rect.width(), 0.0));
      let (snapped_h, offset_y) =
        crate::paint::painter::snap_upscale(dest_rect.height(), pixmap.height() as f32)
          .unwrap_or_else(|| (dest_rect.height(), 0.0));
      dest_rect = Rect::from_xywh(
        dest_rect.x() + offset_x,
        dest_rect.y() + offset_y,
        snapped_w,
        snapped_h,
      );
    }

    let paint = tiny_skia::PixmapPaint {
      opacity: self.canvas.opacity(),
      blend_mode: self.canvas.blend_mode(),
      quality: item.filter_quality.into(),
    };

    let scale_x = dest_rect.width() / pixmap.width() as f32;
    let scale_y = dest_rect.height() / pixmap.height() as f32;

    let transform = Transform::from_row(scale_x, 0.0, 0.0, scale_y, dest_rect.x(), dest_rect.y())
      .post_concat(self.canvas.transform());
    let clip_mask = self.canvas.clip_mask().cloned();
    self.canvas.pixmap_mut().draw_pixmap(
      0,
      0,
      pixmap.as_ref(),
      &paint,
      transform,
      clip_mask.as_ref(),
    );

    Ok(())
  }

  fn push_clip(&mut self, clip: &ClipItem) {
    self.canvas.save();
    match &clip.shape {
      ClipShape::Rect { rect, radii } => {
        let radii = radii.map(|r| self.ds_radii(r));
        self.canvas.set_clip_with_radii(self.ds_rect(*rect), radii);
      }
      ClipShape::Path { path } => {
        self.canvas.set_clip_path(path, self.scale);
      }
    }
  }

  fn pop_clip(&mut self) {
    self.canvas.restore();
  }

  fn push_transform(&mut self, transform: &TransformItem) {
    self.canvas.save();
    let (matrix, valid) = self.to_skia_transform_checked(&transform.transform);
    if valid {
      let combined = self.canvas.transform().post_concat(matrix);
      self.canvas.set_transform(combined);
    }
  }

  fn resolve_font(&self, font_id: Option<&FontId>) -> Option<LoadedFont> {
    let mut families = Vec::new();
    let (weight, italic, oblique, stretch) = match font_id {
      Some(id) => {
        families.push(id.family.clone());
        (
          id.weight,
          matches!(id.style, DbFontStyle::Italic),
          matches!(id.style, DbFontStyle::Oblique),
          id.stretch,
        )
      }
      None => (400, false, false, FontStretch::Normal),
    };

    if families.is_empty() {
      families.push("sans-serif".to_string());
    }

    self
      .font_ctx
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
    let Some(full) = image_data_to_pixmap(&item.image) else {
      return None;
    };

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

fn image_data_to_pixmap(image: &ImageData) -> Option<Pixmap> {
  let mut data = Vec::with_capacity((image.width * image.height * 4) as usize);
  for chunk in image.pixels.chunks_exact(4) {
    let r = chunk[0] as f32;
    let g = chunk[1] as f32;
    let b = chunk[2] as f32;
    let a = chunk[3] as f32 / 255.0;
    data.push((r * a).round() as u8);
    data.push((g * a).round() as u8);
    data.push((b * a).round() as u8);
    data.push(chunk[3]);
  }

  let size = tiny_skia::IntSize::from_wh(image.width, image.height)?;
  Pixmap::from_vec(data, size)
}

fn sample_conic_stops(
  stops: &[(f32, crate::style::color::Rgba)],
  t: f32,
  repeating: bool,
  period: f32,
) -> crate::style::color::Rgba {
  if stops.is_empty() {
    return crate::style::color::Rgba::TRANSPARENT;
  }
  if stops.len() == 1 {
    return stops[0].1;
  }
  let total = if repeating {
    period
  } else {
    stops.last().map(|s| s.0).unwrap_or(1.0)
  };
  let mut pos = t;
  if repeating && total > 0.0 {
    pos = pos.rem_euclid(total);
  }
  if pos <= stops[0].0 {
    return stops[0].1;
  }
  if pos >= stops.last().unwrap().0 && !repeating {
    return stops.last().unwrap().1;
  }
  for window in stops.windows(2) {
    let (p0, c0) = window[0];
    let (p1, c1) = window[1];
    if pos < p0 {
      return c0;
    }
    if pos <= p1 || (repeating && (p1 - p0).abs() < f32::EPSILON) {
      let span = (p1 - p0).max(1e-6);
      let frac = ((pos - p0) / span).clamp(0.0, 1.0);
      return crate::style::color::Rgba {
        r: ((1.0 - frac) * c0.r as f32 + frac * c1.r as f32)
          .round()
          .clamp(0.0, 255.0) as u8,
        g: ((1.0 - frac) * c0.g as f32 + frac * c1.g as f32)
          .round()
          .clamp(0.0, 255.0) as u8,
        b: ((1.0 - frac) * c0.b as f32 + frac * c1.b as f32)
          .round()
          .clamp(0.0, 255.0) as u8,
        a: (1.0 - frac) * c0.a + frac * c1.a,
      };
    }
  }
  stops.last().unwrap().1
}

#[derive(Copy, Clone)]
struct BorderImageWidths {
  top: f32,
  right: f32,
  bottom: f32,
  left: f32,
}

fn resolve_slice_value(value: BorderImageSliceValue, axis_len: u32) -> f32 {
  match value {
    BorderImageSliceValue::Number(n) => n.max(0.0),
    BorderImageSliceValue::Percentage(p) => (p / 100.0) * axis_len as f32,
  }
}

fn resolve_length_for_border_image(
  len: &Length,
  percentage_base: f32,
  font_size: f32,
  root_font_size: f32,
  viewport: Option<(f32, f32)>,
) -> f32 {
  let needs_viewport = len.unit.is_viewport_relative()
    || len
      .calc
      .as_ref()
      .map(|c| c.has_viewport_relative())
      .unwrap_or(false);
  let (vw, vh) = match viewport {
    Some(vp) => vp,
    None if needs_viewport => (f32::NAN, f32::NAN),
    None => (0.0, 0.0),
  };

  len
    .resolve_with_context(Some(percentage_base), vw, vh, font_size, root_font_size)
    .unwrap_or_else(|| {
      if len.unit.is_absolute() {
        len.to_px()
      } else {
        len.value * font_size
      }
    })
}

fn resolve_border_image_widths(
  widths: &BorderImageWidth,
  border: BorderImageWidths,
  box_width: f32,
  box_height: f32,
  font_size: f32,
  root_font_size: f32,
  viewport: Option<(f32, f32)>,
) -> BorderImageWidths {
  let resolve_single = |value: BorderImageWidthValue, border: f32, axis: f32| -> f32 {
    match value {
      BorderImageWidthValue::Auto => border,
      BorderImageWidthValue::Number(n) => (n * border).max(0.0),
      BorderImageWidthValue::Length(len) => {
        resolve_length_for_border_image(&len, axis, font_size, root_font_size, viewport).max(0.0)
      }
      BorderImageWidthValue::Percentage(p) => ((p / 100.0) * axis).max(0.0),
    }
  };

  BorderImageWidths {
    top: resolve_single(widths.top, border.top, box_height),
    right: resolve_single(widths.right, border.right, box_width),
    bottom: resolve_single(widths.bottom, border.bottom, box_height),
    left: resolve_single(widths.left, border.left, box_width),
  }
}

fn resolve_border_image_outset(
  outset: &crate::style::types::BorderImageOutset,
  border: BorderImageWidths,
  font_size: f32,
  root_font_size: f32,
  viewport: Option<(f32, f32)>,
) -> BorderImageWidths {
  let resolve_single = |value: BorderImageOutsetValue, border: f32| -> f32 {
    match value {
      BorderImageOutsetValue::Number(n) => (n * border).max(0.0),
      BorderImageOutsetValue::Length(len) => {
        resolve_length_for_border_image(&len, border.max(1.0), font_size, root_font_size, viewport)
          .max(0.0)
      }
    }
  };

  BorderImageWidths {
    top: resolve_single(outset.top, border.top),
    right: resolve_single(outset.right, border.right),
    bottom: resolve_single(outset.bottom, border.bottom),
    left: resolve_single(outset.left, border.left),
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
          for (j, slot) in positions
            .iter_mut()
            .enumerate()
            .take(start_idx + gap + 1)
            .skip(start_idx + 1)
          {
            *slot = Some((start_pos + step * j as f32).max(start_pos));
          }
        }
      } else if i > 0 {
        let gap = i;
        let step = pos / gap as f32;
        for (j, slot) in positions.iter_mut().take(i).enumerate() {
          *slot = Some(step * j as f32);
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
          for (j, slot) in positions
            .iter_mut()
            .enumerate()
            .take(start_idx + gap + 1)
            .skip(start_idx + 1)
          {
            *slot = Some(start_pos + step * j as f32);
          }
        }
      } else if i > 0 {
        let gap = i;
        let step = pos / gap as f32;
        for (j, slot) in positions.iter_mut().take(i).enumerate() {
          *slot = Some(step * j as f32);
        }
      }
      last_known = Some((i, pos));
    }
  }

  let mut output = Vec::with_capacity(stops.len());
  let mut prev = 0.0;
  for (idx, pos_opt) in positions.into_iter().enumerate() {
    let pos = pos_opt.unwrap_or(prev);
    let unclamped = pos.max(prev);
    prev = unclamped;
    output.push((unclamped, stops[idx].color.to_rgba(current_color)));
  }

  output
}

fn gradient_stops(stops: &[(f32, Rgba)]) -> Vec<tiny_skia::GradientStop> {
  stops
    .iter()
    .map(|(p, c)| {
      tiny_skia::GradientStop::new(
        *p,
        tiny_skia::Color::from_rgba8(c.r, c.g, c.b, (c.a * 255.0).round().clamp(0.0, 255.0) as u8),
      )
    })
    .collect()
}

fn radial_geometry(
  rect: Rect,
  position: &BackgroundPosition,
  size: &RadialGradientSize,
  shape: RadialGradientShape,
  font_size: f32,
  root_font_size: f32,
  viewport: Option<(f32, f32)>,
) -> (f32, f32, f32, f32) {
  let (align_x, off_x, align_y, off_y) = match position {
    BackgroundPosition::Position { x, y } => {
      let ox = resolve_length_for_border_image(
        &x.offset,
        rect.width(),
        font_size,
        root_font_size,
        viewport,
      );
      let oy = resolve_length_for_border_image(
        &y.offset,
        rect.height(),
        font_size,
        root_font_size,
        viewport,
      );
      (x.alignment, ox, y.alignment, oy)
    }
  };
  let cx = rect.x() + align_x * rect.width() + off_x;
  let cy = rect.y() + align_y * rect.height() + off_y;

  let dx_left = (cx - rect.x()).max(0.0);
  let dx_right = (rect.x() + rect.width() - cx).max(0.0);
  let dy_top = (cy - rect.y()).max(0.0);
  let dy_bottom = (rect.y() + rect.height() - cy).max(0.0);

  let (mut radius_x, mut radius_y) = match size {
    RadialGradientSize::ClosestSide => (dx_left.min(dx_right), dy_top.min(dy_bottom)),
    RadialGradientSize::FarthestSide => (dx_left.max(dx_right), dy_top.max(dy_bottom)),
    RadialGradientSize::ClosestCorner => {
      let corners = [
        (dx_left, dy_top),
        (dx_left, dy_bottom),
        (dx_right, dy_top),
        (dx_right, dy_bottom),
      ];
      let mut best = f32::INFINITY;
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
      let mut best = -f32::INFINITY;
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
      let rx =
        resolve_length_for_border_image(x, rect.width(), font_size, root_font_size, viewport)
          .max(0.0);
      let ry = y
        .as_ref()
        .map(|yy| {
          resolve_length_for_border_image(yy, rect.height(), font_size, root_font_size, viewport)
            .max(0.0)
        })
        .unwrap_or(rx);
      (rx, ry)
    }
  };

  if matches!(shape, RadialGradientShape::Circle) {
    let r = if matches!(
      size,
      RadialGradientSize::ClosestCorner | RadialGradientSize::FarthestCorner
    ) {
      let r_corner = ((radius_x * radius_x + radius_y * radius_y) / 2.0).sqrt();
      r_corner
    } else {
      radius_x.max(radius_y)
    };
    radius_x = r;
    radius_y = r;
  }

  (cx, cy, radius_x.max(0.0), radius_y.max(0.0))
}

fn resolve_gradient_center(
  rect: Rect,
  position: &BackgroundPosition,
  font_size: f32,
  root_font_size: f32,
  viewport: Option<(f32, f32)>,
) -> Point {
  let (align_x, off_x, align_y, off_y) = match position {
    BackgroundPosition::Position { x, y } => {
      let ox = resolve_length_for_border_image(
        &x.offset,
        rect.width(),
        font_size,
        root_font_size,
        viewport,
      );
      let oy = resolve_length_for_border_image(
        &y.offset,
        rect.height(),
        font_size,
        root_font_size,
        viewport,
      );
      (x.alignment, ox, y.alignment, oy)
    }
  };
  let cx = rect.x() + align_x * rect.width() + off_x;
  let cy = rect.y() + align_y * rect.height() + off_y;
  Point::new(cx, cy)
}

fn sample_stops(stops: &[(f32, Rgba)], t: f32, repeating: bool, period: f32) -> Rgba {
  if stops.is_empty() {
    return Rgba::TRANSPARENT;
  }
  if stops.len() == 1 {
    return stops[0].1;
  }
  let total = if repeating {
    period
  } else {
    stops.last().map(|(p, _)| *p).unwrap_or(1.0)
  };
  let mut pos = t;
  if repeating && total > 0.0 {
    pos = pos.rem_euclid(total);
  }
  if pos <= stops[0].0 {
    return stops[0].1;
  }
  if pos >= stops.last().unwrap().0 && !repeating {
    return stops.last().unwrap().1;
  }
  for window in stops.windows(2) {
    let (p0, c0) = window[0];
    let (p1, c1) = window[1];
    if pos >= p0 && pos <= p1 {
      let t = if (p1 - p0).abs() < f32::EPSILON {
        0.0
      } else {
        ((pos - p0) / (p1 - p0)).clamp(0.0, 1.0)
      };
      return Rgba {
        r: (c0.r as f32 + (c1.r as f32 - c0.r as f32) * t).round() as u8,
        g: (c0.g as f32 + (c1.g as f32 - c0.g as f32) * t).round() as u8,
        b: (c0.b as f32 + (c1.b as f32 - c0.b as f32) * t).round() as u8,
        a: c0.a + (c1.a - c0.a) * t,
      };
    }
  }
  Rgba::TRANSPARENT
}

fn render_generated_border_image(
  bg: &BackgroundImage,
  current_color: Rgba,
  width: u32,
  height: u32,
  font_size: f32,
  root_font_size: f32,
  viewport: Option<(f32, f32)>,
) -> Option<Pixmap> {
  if width == 0 || height == 0 {
    return None;
  }

  let rect = Rect::from_xywh(0.0, 0.0, width as f32, height as f32);
  match bg {
    BackgroundImage::LinearGradient { angle, stops } => {
      let resolved = normalize_color_stops(stops, current_color);
      if resolved.is_empty() {
        return None;
      }
      let skia_stops = gradient_stops(&resolved);
      let rad = angle.to_radians();
      let dx = rad.sin();
      let dy = -rad.cos();
      let len = 0.5 * (rect.width() * dx.abs() + rect.height() * dy.abs());
      let cx = rect.x() + rect.width() / 2.0;
      let cy = rect.y() + rect.height() / 2.0;

      let start = tiny_skia::Point::from_xy(cx - dx * len, cy - dy * len);
      let end = tiny_skia::Point::from_xy(cx + dx * len, cy + dy * len);
      let shader = LinearGradient::new(
        start,
        end,
        skia_stops,
        SpreadMode::Pad,
        Transform::identity(),
      )?;

      let mut pixmap = Pixmap::new(width, height)?;
      let skia_rect = tiny_skia::Rect::from_xywh(0.0, 0.0, width as f32, height as f32)?;
      let path = PathBuilder::from_rect(skia_rect);

      let mut paint = tiny_skia::Paint::default();
      paint.shader = shader;
      paint.anti_alias = true;
      pixmap.fill_path(
        &path,
        &paint,
        tiny_skia::FillRule::Winding,
        Transform::identity(),
        None,
      );
      Some(pixmap)
    }
    BackgroundImage::RepeatingLinearGradient { angle, stops } => {
      let resolved = normalize_color_stops(stops, current_color);
      if resolved.is_empty() {
        return None;
      }
      let skia_stops = gradient_stops(&resolved);
      let rad = angle.to_radians();
      let dx = rad.sin();
      let dy = -rad.cos();
      let len = 0.5 * (rect.width() * dx.abs() + rect.height() * dy.abs());
      let cx = rect.x() + rect.width() / 2.0;
      let cy = rect.y() + rect.height() / 2.0;

      let start = tiny_skia::Point::from_xy(cx - dx * len, cy - dy * len);
      let end = tiny_skia::Point::from_xy(cx + dx * len, cy + dy * len);
      let shader = LinearGradient::new(
        start,
        end,
        skia_stops,
        SpreadMode::Repeat,
        Transform::identity(),
      )?;

      let mut pixmap = Pixmap::new(width, height)?;
      let skia_rect = tiny_skia::Rect::from_xywh(0.0, 0.0, width as f32, height as f32)?;
      let path = PathBuilder::from_rect(skia_rect);

      let mut paint = tiny_skia::Paint::default();
      paint.shader = shader;
      paint.anti_alias = true;
      pixmap.fill_path(
        &path,
        &paint,
        tiny_skia::FillRule::Winding,
        Transform::identity(),
        None,
      );
      Some(pixmap)
    }
    BackgroundImage::RadialGradient {
      shape,
      size,
      position,
      stops,
    } => {
      let resolved = normalize_color_stops(stops, current_color);
      if resolved.is_empty() {
        return None;
      }
      let skia_stops = gradient_stops(&resolved);
      let (cx, cy, radius_x, radius_y) = radial_geometry(
        rect,
        position,
        size,
        *shape,
        font_size,
        root_font_size,
        viewport,
      );
      let transform = Transform::from_translate(cx, cy).pre_scale(radius_x, radius_y);
      let shader = RadialGradient::new(
        tiny_skia::Point::from_xy(0.0, 0.0),
        tiny_skia::Point::from_xy(0.0, 0.0),
        1.0,
        skia_stops,
        SpreadMode::Pad,
        transform,
      )?;

      let mut pixmap = Pixmap::new(width, height)?;
      let skia_rect = tiny_skia::Rect::from_xywh(0.0, 0.0, width as f32, height as f32)?;
      let path = PathBuilder::from_rect(skia_rect);
      let mut paint = tiny_skia::Paint::default();
      paint.shader = shader;
      paint.anti_alias = true;
      pixmap.fill_path(
        &path,
        &paint,
        tiny_skia::FillRule::Winding,
        Transform::identity(),
        None,
      );
      Some(pixmap)
    }
    BackgroundImage::RepeatingRadialGradient {
      shape,
      size,
      position,
      stops,
    } => {
      let resolved = normalize_color_stops(stops, current_color);
      if resolved.is_empty() {
        return None;
      }
      let skia_stops = gradient_stops(&resolved);
      let (cx, cy, radius_x, radius_y) = radial_geometry(
        rect,
        position,
        size,
        *shape,
        font_size,
        root_font_size,
        viewport,
      );
      let transform = Transform::from_translate(cx, cy).pre_scale(radius_x, radius_y);
      let shader = RadialGradient::new(
        tiny_skia::Point::from_xy(0.0, 0.0),
        tiny_skia::Point::from_xy(0.0, 0.0),
        1.0,
        skia_stops,
        SpreadMode::Repeat,
        transform,
      )?;

      let mut pixmap = Pixmap::new(width, height)?;
      let skia_rect = tiny_skia::Rect::from_xywh(0.0, 0.0, width as f32, height as f32)?;
      let path = PathBuilder::from_rect(skia_rect);
      let mut paint = tiny_skia::Paint::default();
      paint.shader = shader;
      paint.anti_alias = true;
      pixmap.fill_path(
        &path,
        &paint,
        tiny_skia::FillRule::Winding,
        Transform::identity(),
        None,
      );
      Some(pixmap)
    }
    BackgroundImage::ConicGradient {
      from_angle,
      position,
      stops,
    } => {
      let resolved = normalize_color_stops_unclamped(stops, current_color);
      if resolved.is_empty() {
        return None;
      }
      let mut pixmap = Pixmap::new(width, height)?;
      let center = resolve_gradient_center(rect, position, font_size, root_font_size, viewport);
      let start_angle = from_angle.to_radians();
      let period = 1.0;
      let data = pixmap.pixels_mut();
      let transparent = PremultipliedColorU8::from_rgba(0, 0, 0, 0).unwrap();
      for y in 0..height {
        for x in 0..width {
          let dx = x as f32 + 0.5 - center.x;
          let dy = y as f32 + 0.5 - center.y;
          let angle = dx.atan2(-dy) + start_angle;
          let mut t = (angle / (2.0 * std::f32::consts::PI)).rem_euclid(1.0);
          t *= period;
          let color = sample_stops(&resolved, t, false, period);
          let idx = (y * width + x) as usize;
          data[idx] = PremultipliedColorU8::from_rgba(
            color.r,
            color.g,
            color.b,
            (color.a * 255.0).round().clamp(0.0, 255.0) as u8,
          )
          .unwrap_or(transparent);
        }
      }
      Some(pixmap)
    }
    BackgroundImage::RepeatingConicGradient {
      from_angle,
      position,
      stops,
    } => {
      let resolved = normalize_color_stops_unclamped(stops, current_color);
      if resolved.is_empty() {
        return None;
      }
      let mut pixmap = Pixmap::new(width, height)?;
      let center = resolve_gradient_center(rect, position, font_size, root_font_size, viewport);
      let start_angle = from_angle.to_radians();
      let period = resolved.last().map(|(p, _)| *p).unwrap_or(1.0).max(1e-6);
      let data = pixmap.pixels_mut();
      let transparent = PremultipliedColorU8::from_rgba(0, 0, 0, 0).unwrap();
      for y in 0..height {
        for x in 0..width {
          let dx = x as f32 + 0.5 - center.x;
          let dy = y as f32 + 0.5 - center.y;
          let angle = dx.atan2(-dy) + start_angle;
          let mut t = (angle / (2.0 * std::f32::consts::PI)).rem_euclid(1.0);
          t *= period;
          let color = sample_stops(&resolved, t, true, period);
          let idx = (y * width + x) as usize;
          data[idx] = PremultipliedColorU8::from_rgba(
            color.r,
            color.g,
            color.b,
            (color.a * 255.0).round().clamp(0.0, 255.0) as u8,
          )
          .unwrap_or(transparent);
        }
      }
      Some(pixmap)
    }
    BackgroundImage::None | BackgroundImage::Url(_) => None,
  }
}

fn resolve_length_for_paint(
  len: &Length,
  font_size: f32,
  root_font_size: f32,
  percentage_base: f32,
  viewport: (f32, f32),
) -> f32 {
  len
    .resolve_with_context(
      Some(percentage_base),
      viewport.0,
      viewport.1,
      font_size,
      root_font_size,
    )
    .unwrap_or_else(|| {
      if len.unit.is_absolute() {
        len.to_px()
      } else {
        len.value * font_size
      }
    })
}

fn compute_background_size_from_value(
  size: &BackgroundSize,
  font_size: f32,
  root_font_size: f32,
  viewport: (f32, f32),
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

  match size {
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
            Some(resolve_length_for_paint(&len, font_size, root_font_size, area, viewport).max(0.0))
          }
        }
      };

      let resolved_x = resolve(*x, area_w);
      let resolved_y = resolve(*y, area_h);

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
  root_font_size: f32,
  viewport: (f32, f32),
) -> (f32, f32) {
  let resolve_axis =
    |comp: crate::style::types::BackgroundPositionComponent, area: f32, tile: f32| -> f32 {
      let available = area - tile;
      let offset =
        resolve_length_for_paint(&comp.offset, font_size, root_font_size, available, viewport);
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

fn round_tile_length(area: f32, tile: f32) -> f32 {
  if tile <= 0.0 || area <= 0.0 {
    return tile;
  }
  let count = (area / tile).round().max(1.0);
  area / count
}

fn tile_positions(
  repeat: BackgroundRepeatKeyword,
  area_start: f32,
  area_len: f32,
  tile_len: f32,
  offset: f32,
  clip_start: f32,
  clip_end: f32,
) -> Vec<f32> {
  if tile_len <= 0.0 || area_len <= 0.0 {
    return Vec::new();
  }
  match repeat {
    BackgroundRepeatKeyword::NoRepeat => {
      let start = area_start + offset;
      if start + tile_len < clip_start || start > clip_end {
        Vec::new()
      } else {
        vec![start]
      }
    }
    BackgroundRepeatKeyword::Repeat => {
      let mut positions = Vec::new();
      let first = area_start + offset;
      let mut cursor = first;
      while cursor < clip_end {
        if cursor + tile_len >= clip_start - tile_len {
          positions.push(cursor);
        }
        cursor += tile_len;
      }
      positions
    }
    BackgroundRepeatKeyword::Space => {
      let count = (area_len / tile_len).floor();
      if count < 1.0 {
        vec![area_start + (area_len - tile_len) * 0.5]
      } else if count < 2.0 {
        vec![area_start + (area_len - tile_len) * 0.5]
      } else {
        let spacing = (area_len - tile_len * count) / (count - 1.0);
        let mut positions = Vec::with_capacity(count as usize);
        let mut cursor = area_start;
        for _ in 0..(count as usize) {
          positions.push(cursor);
          cursor += tile_len + spacing;
        }
        positions
      }
    }
    BackgroundRepeatKeyword::Round => {
      let count = (area_len / tile_len).round().max(1.0);
      let len = area_len / count;
      let mut positions = Vec::new();
      let mut cursor = area_start;
      while cursor < area_start + area_len - 1e-3 {
        positions.push(cursor);
        cursor += len;
      }
      positions
    }
  }
}

fn mask_value_from_pixel(pixel: &[u8], mode: MaskMode) -> u8 {
  let a = pixel.get(3).copied().unwrap_or(0) as f32 / 255.0;
  let value = match mode {
    MaskMode::Alpha => a,
    MaskMode::Luminance => {
      if a <= 0.0 {
        0.0
      } else {
        let r = pixel.get(0).copied().unwrap_or(0) as f32 / 255.0 / a;
        let g = pixel.get(1).copied().unwrap_or(0) as f32 / 255.0 / a;
        let b = pixel.get(2).copied().unwrap_or(0) as f32 / 255.0 / a;
        (0.2126 * r + 0.7152 * g + 0.0722 * b) * a
      }
    }
  };
  (value * 255.0).round().clamp(0.0, 255.0) as u8
}

fn mask_tile_from_image(tile: &Pixmap, mode: MaskMode) -> Option<Pixmap> {
  let size = IntSize::from_wh(tile.width(), tile.height())?;
  let mut data = Vec::with_capacity(tile.data().len());
  for chunk in tile.data().chunks(4) {
    let v = mask_value_from_pixel(chunk, mode);
    data.extend_from_slice(&[v, v, v, v]);
  }
  Pixmap::from_vec(data, size)
}

fn paint_mask_tile(
  dest: &mut Pixmap,
  tile: &Pixmap,
  tx: f32,
  ty: f32,
  tile_w: f32,
  tile_h: f32,
  clip_rect: Rect,
  scale: f32,
) {
  if tile_w <= 0.0 || tile_h <= 0.0 {
    return;
  }
  let tile_rect = Rect::from_xywh(tx, ty, tile_w, tile_h);
  let Some(intersection) = tile_rect.intersection(clip_rect) else {
    return;
  };
  if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
    return;
  }

  let device_clip = Rect::from_xywh(
    intersection.x() * scale,
    intersection.y() * scale,
    intersection.width() * scale,
    intersection.height() * scale,
  );
  if device_clip.width() <= 0.0 || device_clip.height() <= 0.0 {
    return;
  }
  let Some(src_rect) = tiny_skia::Rect::from_xywh(
    device_clip.x(),
    device_clip.y(),
    device_clip.width(),
    device_clip.height(),
  ) else {
    return;
  };

  let scale_x = tile_w / tile.width() as f32;
  let scale_y = tile_h / tile.height() as f32;

  let mut paint = tiny_skia::Paint::default();
  paint.shader = Pattern::new(
    tile.as_ref(),
    SpreadMode::Pad,
    tiny_skia::FilterQuality::Bilinear,
    1.0,
    Transform::from_row(
      scale_x * scale,
      0.0,
      0.0,
      scale_y * scale,
      tx * scale,
      ty * scale,
    ),
  );
  paint.anti_alias = false;
  dest.fill_rect(src_rect, &paint, Transform::identity(), None);
}

fn apply_mask_composite(dest: &mut Mask, src: &Mask, op: MaskComposite) {
  if dest.width() != src.width() || dest.height() != src.height() {
    return;
  }

  let dest_data = dest.data_mut();
  let src_data = src.data();
  for (d, s) in dest_data.iter_mut().zip(src_data.iter()) {
    let src = *s as u16;
    let dst = *d as u16;
    let out = match op {
      MaskComposite::Add => src + dst.saturating_mul(255 - src) / 255,
      MaskComposite::Subtract => src.saturating_mul(255 - dst) / 255,
      MaskComposite::Intersect => src.saturating_mul(dst) / 255,
      MaskComposite::Exclude => {
        let src_out = src.saturating_mul(255 - dst) / 255;
        let dst_out = dst.saturating_mul(255 - src) / 255;
        src_out + dst_out
      }
    };
    *d = out.min(255) as u8;
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::Point;
  use crate::geometry::Rect;
  use crate::paint::clip_path::ResolvedClipPath;
  use crate::paint::display_list::BlendMode;
  use crate::paint::display_list::BorderImageItem;
  use crate::paint::display_list::BorderImageSourceItem;
  use crate::paint::display_list::BorderItem;
  use crate::paint::display_list::BorderRadii;
  use crate::paint::display_list::BorderSide;
  use crate::paint::display_list::BoxShadowItem;
  use crate::paint::display_list::ClipItem;
  use crate::paint::display_list::ClipShape;
  use crate::paint::display_list::DecorationPaint;
  use crate::paint::display_list::DecorationStroke;
  use crate::paint::display_list::DisplayItem;
  use crate::paint::display_list::DisplayList;
  use crate::paint::display_list::FillRectItem;
  use crate::paint::display_list::GlyphInstance;
  use crate::paint::display_list::GradientSpread;
  use crate::paint::display_list::GradientStop;
  use crate::paint::display_list::ImageData;
  use crate::paint::display_list::ImageFilterQuality;
  use crate::paint::display_list::ImageItem;
  use crate::paint::display_list::LinearGradientItem;
  use crate::paint::display_list::OpacityItem;
  use crate::paint::display_list::RadialGradientItem;
  use crate::paint::display_list::ResolvedMaskLayer;
  use crate::paint::display_list::StackingContextItem;
  use crate::paint::display_list::TextDecorationItem;
  use crate::paint::display_list::TextEmphasis;
  use crate::paint::display_list::TextItem;
  use crate::paint::display_list::TextShadowItem;
  use crate::paint::display_list::Transform3D;
  use crate::paint::display_list_builder::DisplayListBuilder;
  use crate::style::color::{Color, Rgba};
  use crate::style::types::BackfaceVisibility;
  use crate::style::types::BackgroundImage;
  use crate::style::types::BackgroundRepeat;
  use crate::style::types::BackgroundSize;
  use crate::style::types::BackgroundSizeComponent;
  use crate::style::types::BorderImageSlice;
  use crate::style::types::BorderImageSliceValue;
  use crate::style::types::BorderImageWidth;
  use crate::style::types::BorderImageWidthValue;
  use crate::style::types::BorderStyle as CssBorderStyle;
  use crate::style::types::MaskClip;
  use crate::style::types::MaskComposite;
  use crate::style::types::MaskLayer;
  use crate::style::types::TextDecorationLine;
  use crate::style::types::TextDecorationThickness;
  use crate::style::types::TextEmphasisFill;
  use crate::style::types::TextEmphasisPosition;
  use crate::style::types::TextEmphasisShape;
  use crate::style::types::TextEmphasisStyle;
  use crate::style::values::CalcLength;
  use crate::style::values::Length;
  use crate::style::values::LengthUnit;
  use crate::style::ComputedStyle;
  use crate::tree::fragment_tree::FragmentNode;
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

  fn mask_rects(bounds: Rect, padding: (f32, f32, f32, f32)) -> MaskReferenceRects {
    let (left, top, right, bottom) = padding;
    let content = Rect::from_xywh(
      bounds.x() + left,
      bounds.y() + top,
      (bounds.width() - left - right).max(0.0),
      (bounds.height() - top - bottom).max(0.0),
    );
    MaskReferenceRects {
      border: bounds,
      padding: bounds,
      content,
    }
  }

  #[test]
  fn border_strokes_stay_within_border_box() {
    let renderer = DisplayListRenderer::new(15, 8, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    let side = BorderSide {
      width: 2.0,
      style: CssBorderStyle::Dotted,
      color: Rgba::BLACK,
    };
    list.push(DisplayItem::Border(Box::new(BorderItem {
      rect: Rect::from_xywh(2.0, 1.0, 10.0, 4.0),
      top: side.clone(),
      right: side.clone(),
      bottom: side.clone(),
      left: side,
      image: None,
      radii: BorderRadii::ZERO,
    })));

    let pixmap = renderer.render(&list).unwrap();
    // Outside the border box should stay untouched when strokes are centered on the edges.
    assert_eq!(pixel(&pixmap, 1, 2), (255, 255, 255, 255));
    assert_eq!(pixel(&pixmap, 13, 2), (255, 255, 255, 255));
    assert_eq!(pixel(&pixmap, 2, 0), (255, 255, 255, 255));
    // Border still paints along the edges of the box.
    assert_eq!(pixel(&pixmap, 3, 2), (0, 0, 0, 255));
  }

  #[test]
  fn border_image_paints_raster_slices() {
    let renderer = DisplayListRenderer::new(8, 8, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();

    let pixels = vec![
      255, 0, 0, 255, // red
      0, 255, 0, 255, // green
      0, 0, 255, 255, // blue
      0, 0, 0, 255, // black
    ];
    let image = ImageData::new_pixels(2, 2, pixels);
    let border_image = BorderImageItem {
      source: BorderImageSourceItem::Raster(image),
      slice: BorderImageSlice {
        top: BorderImageSliceValue::Number(1.0),
        right: BorderImageSliceValue::Number(1.0),
        bottom: BorderImageSliceValue::Number(1.0),
        left: BorderImageSliceValue::Number(1.0),
        fill: false,
      },
      width: BorderImageWidth {
        top: BorderImageWidthValue::Number(1.0),
        right: BorderImageWidthValue::Number(1.0),
        bottom: BorderImageWidthValue::Number(1.0),
        left: BorderImageWidthValue::Number(1.0),
      },
      outset: crate::style::types::BorderImageOutset::default(),
      repeat: (
        crate::style::types::BorderImageRepeat::Stretch,
        crate::style::types::BorderImageRepeat::Stretch,
      ),
      current_color: Rgba::BLACK,
      font_size: 16.0,
      root_font_size: 16.0,
      viewport: Some((8.0, 8.0)),
    };

    let side = BorderSide {
      width: 2.0,
      style: CssBorderStyle::Solid,
      color: Rgba::BLACK,
    };
    list.push(DisplayItem::Border(Box::new(BorderItem {
      rect: Rect::from_xywh(1.0, 1.0, 6.0, 6.0),
      top: side.clone(),
      right: side.clone(),
      bottom: side.clone(),
      left: side,
      image: Some(border_image),
      radii: BorderRadii::ZERO,
    })));

    let pixmap = renderer.render(&list).unwrap();
    // Top-left border area should come from the red slice.
    assert_eq!(pixel(&pixmap, 1, 1), (255, 0, 0, 255));
    // Top-right border area should come from the green slice.
    assert_eq!(pixel(&pixmap, 6, 1), (0, 255, 0, 255));
    // Bottom-left border area should come from the blue slice.
    assert_eq!(pixel(&pixmap, 1, 6), (0, 0, 255, 255));
    // Center remains white because fill is false.
    assert_eq!(pixel(&pixmap, 4, 4), (255, 255, 255, 255));
  }

  #[test]
  fn outline_respects_offset_and_style() {
    let renderer = DisplayListRenderer::new(8, 8, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    list.push(DisplayItem::Outline(OutlineItem {
      rect: Rect::from_xywh(2.0, 2.0, 2.0, 2.0),
      width: 2.0,
      style: CssBorderStyle::Solid,
      color: Rgba::BLACK,
      offset: 1.0,
      invert: false,
    }));

    let pixmap = renderer.render(&list).unwrap();
    // Outline expands outward; top-left outer pixel should be stroked.
    assert_eq!(pixel(&pixmap, 0, 0), (0, 0, 0, 255));
    // Inside the original rect stays untouched.
    assert_eq!(pixel(&pixmap, 3, 3), (255, 255, 255, 255));
  }

  #[test]
  fn viewport_relative_border_image_length_requires_viewport() {
    // 10vw should resolve to 20px with a 200px viewport, and remain unresolved without one.
    let len = Length::calc(CalcLength::single(LengthUnit::Vw, 10.0));

    let resolved = resolve_length_for_border_image(&len, 100.0, 16.0, 16.0, Some((200.0, 100.0)));
    assert!((resolved - 20.0).abs() < 0.001);

    let unresolved = resolve_length_for_border_image(&len, 100.0, 16.0, 16.0, None);
    assert_eq!(unresolved, 0.0);
  }

  #[test]
  fn outline_paints_outside_overflow_hidden_clip() {
    let renderer = DisplayListRenderer::new(20, 20, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    // Simulate overflow:hidden clipping the content box but paint outline after clip.
    list.push(DisplayItem::PushClip(ClipItem {
      shape: ClipShape::Rect {
        rect: Rect::from_xywh(8.0, 8.0, 4.0, 4.0),
        radii: Some(BorderRadii::ZERO),
      },
    }));
    list.push(DisplayItem::PopClip);
    list.push(DisplayItem::Outline(OutlineItem {
      rect: Rect::from_xywh(8.0, 8.0, 4.0, 4.0),
      width: 2.0,
      style: CssBorderStyle::Solid,
      color: Rgba::RED,
      offset: 0.0,
      invert: false,
    }));

    let pixmap = renderer.render(&list).unwrap();
    // Outline should render outside the clipped area; check just left of the content box.
    assert_eq!(pixel(&pixmap, 7, 10), (255, 0, 0, 255));
    // Center remains unclipped by outline.
    assert_eq!(pixel(&pixmap, 10, 10), (255, 255, 255, 255));
  }

  #[test]
  fn double_border_thinner_than_three_falls_back_to_solid() {
    let make_list = |style| {
      let mut list = DisplayList::new();
      let side = BorderSide {
        width: 1.0,
        style,
        color: Rgba::BLACK,
      };
      list.push(DisplayItem::Border(Box::new(BorderItem {
        rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
        top: side.clone(),
        right: side.clone(),
        bottom: side.clone(),
        left: side,
        image: None,
        radii: BorderRadii::ZERO,
      })));
      list
    };

    let pix_double = DisplayListRenderer::new(6, 6, Rgba::WHITE, FontContext::new())
      .unwrap()
      .render(&make_list(CssBorderStyle::Double))
      .unwrap();
    let pix_solid = DisplayListRenderer::new(6, 6, Rgba::WHITE, FontContext::new())
      .unwrap()
      .render(&make_list(CssBorderStyle::Solid))
      .unwrap();

    assert_eq!(pix_double.data(), pix_solid.data());
  }

  #[test]
  fn transform_push_is_scoped() {
    let renderer = DisplayListRenderer::new(20, 10, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushTransform(TransformItem {
      transform: Transform3D::translate(10.0, 0.0, 0.0),
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
    let image = Arc::new(ImageData::new_pixels(2, 1, pixels));

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
    let image = Arc::new(ImageData::new_pixels(2, 2, pixels));

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
  fn pixelated_nearest_snaps_upscale_to_integer_factor() {
    let pixels = vec![
      0, 0, 0, 255, 0, 0, 0, 255, // 2x1 black strip
    ];
    let image = Arc::new(ImageData::new_pixels(2, 1, pixels));

    let mut list = DisplayList::new();
    list.push(DisplayItem::Image(ImageItem {
      dest_rect: Rect::from_xywh(0.0, 0.0, 5.0, 1.0),
      image: image.clone(),
      filter_quality: ImageFilterQuality::Nearest,
      src_rect: None,
    }));

    let pixmap = DisplayListRenderer::new(5, 1, Rgba::WHITE, FontContext::new())
      .unwrap()
      .render(&list)
      .unwrap();

    // Snapped scaling shrinks to a 2x integer factor (4px) centered in the 5px box.
    assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 255, 255));
    assert_eq!(pixel(&pixmap, 1, 0), (0, 0, 0, 255));
    assert_eq!(pixel(&pixmap, 4, 0), (0, 0, 0, 255));
  }

  #[test]
  fn renders_text_decoration_items() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::TextDecoration(TextDecorationItem {
      bounds: Rect::from_xywh(0.0, 0.0, 20.0, 10.0),
      line_start: 2.0,
      line_width: 16.0,
      inline_vertical: false,
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
      inline_vertical: false,
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
  fn text_decoration_percentage_thickness_paints_font_relative_size() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.font_size = 20.0;
    style.text_decoration.lines = TextDecorationLine::UNDERLINE;
    style.text_decoration.color = Some(Rgba::from_rgba8(255, 0, 0, 255));
    style.text_decoration.thickness = TextDecorationThickness::Length(Length::percent(50.0));
    let style = Arc::new(style);

    let text_rect = Rect::from_xywh(10.0, 10.0, 80.0, 30.0);
    let fragment = FragmentNode::new_text_styled(text_rect, "Hi".to_string(), 22.0, style);
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 60.0), vec![fragment]);

    let font_ctx = FontContext::new();
    let list = DisplayListBuilder::new()
      .with_font_context(font_ctx.clone())
      .build(&root);
    let pixmap = DisplayListRenderer::new(120, 60, Rgba::WHITE, font_ctx)
      .expect("renderer")
      .render(&list)
      .expect("rendered");

    let red_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80)
        .expect("underline");
    let height = red_bbox.3 - red_bbox.1 + 1;
    assert!(
      (9..=11).contains(&height),
      "expected underline thickness around 10px (50% of 20px font), got {height}"
    );
  }

  #[test]
  fn text_decoration_scales_with_device_pixel_ratio() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.font_size = 20.0;
    style.text_decoration.lines = TextDecorationLine::UNDERLINE;
    style.text_decoration.color = Some(Rgba::from_rgba8(255, 0, 0, 255));
    style.text_decoration.thickness = TextDecorationThickness::Length(Length::px(4.0));
    let style = Arc::new(style);

    let text_rect = Rect::from_xywh(10.0, 10.0, 80.0, 30.0);
    let fragment = FragmentNode::new_text_styled(text_rect, "Hi".to_string(), 22.0, style);
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 60.0), vec![fragment]);

    let font_ctx = FontContext::new();
    let list = DisplayListBuilder::new()
      .with_font_context(font_ctx.clone())
      .build(&root);
    let pixmap = DisplayListRenderer::new_scaled(120, 60, Rgba::WHITE, font_ctx, 2.0)
      .expect("renderer")
      .render(&list)
      .expect("rendered");

    let red_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80)
        .expect("underline");
    let height = red_bbox.3 - red_bbox.1 + 1;
    assert!(
      (7..=9).contains(&height),
      "expected underline thickness around 8 device px (4 CSS px at 2x), got {height}"
    );
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
    assert!(
      right.0 > 0,
      "repeat spread should reintroduce the start color"
    );
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
      radii: Point::new(
        1.5 * std::f32::consts::SQRT_2,
        1.5 * std::f32::consts::SQRT_2,
      ),
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
  fn renders_conic_gradient_angles() {
    let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    list.push(DisplayItem::ConicGradient(ConicGradientItem {
      rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      center: Point::new(2.0, 2.0),
      from_angle: 0.0,
      repeating: false,
      stops: vec![
        GradientStop {
          position: 0.0,
          color: Rgba::rgb(255, 0, 0),
        },
        GradientStop {
          position: 0.5,
          color: Rgba::rgb(0, 0, 255),
        },
      ],
    }));

    let pixmap = renderer.render(&list).unwrap();
    assert!(pixel(&pixmap, 2, 0).0 > pixel(&pixmap, 2, 0).2);
    assert!(pixel(&pixmap, 2, 3).2 > pixel(&pixmap, 2, 3).0);
  }

  #[test]
  fn renders_radial_gradient_as_ellipse() {
    let renderer = DisplayListRenderer::new(20, 10, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    list.push(DisplayItem::RadialGradient(RadialGradientItem {
      rect: Rect::from_xywh(0.0, 0.0, 20.0, 10.0),
      center: Point::new(10.0, 5.0),
      radii: Point::new(
        10.0 * std::f32::consts::SQRT_2,
        5.0 * std::f32::consts::SQRT_2,
      ),
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
    let top_center = pixel(&pixmap, 10, 0);
    let right_center = pixel(&pixmap, 19, 5);
    let diff_r = (top_center.0 as i32 - right_center.0 as i32).abs();
    let diff_b = (top_center.2 as i32 - right_center.2 as i32).abs();
    assert!(
      diff_r < 32 && diff_b < 32,
      "elliptical gradient should put horizontal/vertical edges at similar stops"
    );
    let corner = pixel(&pixmap, 19, 9);
    assert!(corner.2 > corner.0, "corner should reach the final stop");
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
  fn drop_shadow_negative_spread_erodes_shadow() {
    let renderer = DisplayListRenderer::new(60, 40, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(10.0, 10.0, 20.0, 10.0),
      plane_rect: Rect::from_xywh(10.0, 10.0, 20.0, 10.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: None,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: vec![ResolvedFilter::DropShadow {
        offset_x: 6.0,
        offset_y: 6.0,
        blur_radius: 0.0,
        spread: -2.0,
        color: Rgba::from_rgba8(255, 0, 0, 255),
      }],
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(10.0, 10.0, 20.0, 10.0),
      color: Rgba::BLACK,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    let shadow_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > g && r > b).expect("shadow");
    let width = shadow_bbox.2 - shadow_bbox.0 + 1;
    assert!(
      width < 20,
      "negative spread should shrink shadow width (got width {width})"
    );
  }

  #[test]
  fn drop_shadow_negative_spread_reduces_outset() {
    let filters = vec![ResolvedFilter::DropShadow {
      offset_x: 0.0,
      offset_y: 0.0,
      blur_radius: 4.0,
      spread: -2.0,
      color: Rgba::BLACK,
    }];
    let (l, t, r, b) = filter_outset(&filters, 1.0).as_tuple();
    let with_zero_spread = vec![ResolvedFilter::DropShadow {
      offset_x: 0.0,
      offset_y: 0.0,
      blur_radius: 4.0,
      spread: 0.0,
      color: Rgba::BLACK,
    }];
    let (l0, t0, r0, b0) = filter_outset(&with_zero_spread, 1.0).as_tuple();
    assert!(
      (l - 10.0).abs() < 0.01
        && (t - 10.0).abs() < 0.01
        && (r - 10.0).abs() < 0.01
        && (b - 10.0).abs() < 0.01,
      "negative spread should reduce blur outset (got {l},{t},{r},{b})"
    );
    assert!(
      l < l0 && t < t0 && r < r0 && b < b0,
      "reduced spread should shrink outsets"
    );
  }

  #[test]
  fn svg_filter_data_url_applies() {
    use base64::{engine::general_purpose, Engine as _};

    let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='2' height='2'><filter id='f'><feFlood flood-color='rgb(255,0,0)' flood-opacity='1' result='f'/><feComposite in='f' in2='SourceAlpha' operator='in'/></filter></svg>";
    let data_url = format!(
      "data:image/svg+xml;base64,{}",
      general_purpose::STANDARD.encode(svg)
    );

    let cache = crate::image_loader::ImageCache::new();
    let filter =
      crate::paint::svg_filter::load_svg_filter(&data_url, &cache).expect("parsed svg filter");

    let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(1.0, 1.0, 8.0, 8.0),
      plane_rect: Rect::from_xywh(1.0, 1.0, 8.0, 8.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: None,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: vec![
        ResolvedFilter::Brightness(0.0),
        ResolvedFilter::SvgFilter(filter),
      ],
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(1.0, 1.0, 8.0, 8.0),
      color: Rgba::from_rgba8(0, 0, 255, 255),
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    let center = pixel(&pixmap, 5, 5);
    assert!(
      center.0 > center.2,
      "svg filter should tint output toward red"
    );
    assert!(
      center.1 < center.0,
      "red flood should dominate other channels"
    );
  }

  #[test]
  fn svg_filter_default_region_extends_bounds() {
    use base64::{engine::general_purpose, Engine as _};

    let svg =
      "<svg xmlns='http://www.w3.org/2000/svg'><filter id='f'><feGaussianBlur stdDeviation='2'/></filter></svg>";
    let data_url = format!(
      "data:image/svg+xml;base64,{}",
      general_purpose::STANDARD.encode(svg)
    );
    let cache = crate::image_loader::ImageCache::new();
    let filter =
      crate::paint::svg_filter::load_svg_filter(&data_url, &cache).expect("parsed svg filter");

    let renderer = DisplayListRenderer::new(40, 40, Rgba::TRANSPARENT, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    let bounds = Rect::from_xywh(10.0, 10.0, 20.0, 20.0);
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds,
      plane_rect: bounds,
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: None,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: vec![ResolvedFilter::SvgFilter(filter)],
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: bounds,
      color: Rgba::RED,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    let outside = pixel(&pixmap, 31, 20);
    assert!(
      outside.3 > 0,
      "default filter region should allow blur to extend outside bounds (alpha={})",
      outside.3
    );
  }

  #[test]
  fn svg_filter_explicit_region_clips_to_bounds() {
    use base64::{engine::general_purpose, Engine as _};

    let svg = "<svg xmlns='http://www.w3.org/2000/svg'><filter id='f' x='0' y='0' width='100%' height='100%'><feGaussianBlur stdDeviation='2'/></filter></svg>";
    let data_url = format!(
      "data:image/svg+xml;base64,{}",
      general_purpose::STANDARD.encode(svg)
    );
    let cache = crate::image_loader::ImageCache::new();
    let filter =
      crate::paint::svg_filter::load_svg_filter(&data_url, &cache).expect("parsed svg filter");

    let renderer = DisplayListRenderer::new(40, 40, Rgba::TRANSPARENT, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    let bounds = Rect::from_xywh(10.0, 10.0, 20.0, 20.0);
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds,
      plane_rect: bounds,
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: None,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: vec![ResolvedFilter::SvgFilter(filter)],
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: bounds,
      color: Rgba::RED,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    let outside = pixel(&pixmap, 31, 20);
    assert_eq!(
      outside.3, 0,
      "explicit region matching bounds should clip blur outside"
    );
  }

  #[test]
  fn filter_outset_accumulates_blurs() {
    let filters = vec![ResolvedFilter::Blur(2.0), ResolvedFilter::Blur(3.0)];
    let (l, t, r, b) = filter_outset(&filters, 1.0).as_tuple();
    assert!(
      (l - 15.0).abs() < 0.01
        && (t - 15.0).abs() < 0.01
        && (r - 15.0).abs() < 0.01
        && (b - 15.0).abs() < 0.01,
      "blur outsets should add up across filter chain"
    );
  }

  #[test]
  fn filter_outset_accumulates_drop_shadow_offsets() {
    let filters = vec![
      ResolvedFilter::Blur(2.0),
      ResolvedFilter::DropShadow {
        offset_x: -4.0,
        offset_y: 3.0,
        blur_radius: 1.0,
        spread: 0.0,
        color: Rgba::BLACK,
      },
    ];
    let (l, t, r, b) = filter_outset(&filters, 1.0).as_tuple();
    assert!(
      (l - 13.0).abs() < 0.01
        && (t - 6.0).abs() < 0.01
        && (r - 6.0).abs() < 0.01
        && (b - 12.0).abs() < 0.01,
      "expected accumulated outsets to be l=13,t=6,r=6,b=12 but got {l},{t},{r},{b}"
    );
  }

  #[test]
  fn blur_filter_outset_scales_with_device_pixel_ratio() {
    let filters = vec![ResolvedFilter::Blur(4.0)];
    let (l, t, r, b) = filter_outset(&filters, 1.0).as_tuple();
    // Blur outset is radius * 3 per side.
    assert!(
      (l - 12.0).abs() < 0.01
        && (t - 12.0).abs() < 0.01
        && (r - 12.0).abs() < 0.01
        && (b - 12.0).abs() < 0.01
    );

    let filters = vec![ResolvedFilter::Blur(2.0)];
    let (l, t, r, b) = filter_outset(&filters, 2.0).as_tuple();
    // Device pixel ratio doubles the blur radius before computing outsets.
    assert!(
      (l - 12.0).abs() < 0.01
        && (t - 12.0).abs() < 0.01
        && (r - 12.0).abs() < 0.01
        && (b - 12.0).abs() < 0.01
    );
  }

  #[test]
  fn stacking_context_filter_stays_bounded() {
    let filters = vec![ResolvedFilter::Blur(1.0)];
    let renderer = DisplayListRenderer::new(20, 20, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    let bounds = Rect::from_xywh(2.0, 2.0, 4.0, 4.0);
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds,
      plane_rect: bounds,
      mix_blend_mode: BlendMode::Normal,
      is_isolated: true,
      transform: None,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: filters.clone(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: bounds,
      color: Rgba::RED,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    let bbox = bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > g && r > b)
      .expect("blurred content");
    let (out_l, out_t, out_r, out_b) = filter_outset(&filters, 1.0).as_tuple();
    let expected = Rect::from_xywh(
      bounds.x() - out_l,
      bounds.y() - out_t,
      bounds.width() + out_l + out_r,
      bounds.height() + out_t + out_b,
    );
    assert!(
      bbox.0 >= expected.min_x().floor() as u32 && bbox.1 >= expected.min_y().floor() as u32,
      "expected bbox {:?} to start within {:?}",
      bbox,
      expected
    );
    assert!(
      bbox.2 <= expected.max_x().ceil() as u32 && bbox.3 <= expected.max_y().ceil() as u32,
      "expected bbox {:?} to end within {:?}",
      bbox,
      expected
    );
  }

  #[test]
  fn backdrop_filter_applies_to_background() {
    let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();

    // Background layer: solid red.
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      color: Rgba::rgb(255, 0, 0),
    }));

    // Foreground stacking context with backdrop-filter:invert on a transparent box.
    list.push(DisplayItem::PushStackingContext(
      crate::paint::display_list::StackingContextItem {
        z_index: 0,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
        plane_rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
        mix_blend_mode: crate::paint::display_list::BlendMode::Normal,
        is_isolated: false,
        transform: None,
        child_perspective: None,
        transform_style: TransformStyle::Flat,
        backface_visibility: BackfaceVisibility::Visible,
        filters: Vec::new(),
        backdrop_filters: vec![ResolvedFilter::Invert(1.0)],
        radii: BorderRadii::ZERO,
        mask: None,
      },
    ));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      color: Rgba::from_rgba8(0, 0, 0, 0),
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    // Red background inverted through backdrop-filter yields cyan.
    assert_eq!(pixel(&pixmap, 0, 0), (0, 255, 255, 255));
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
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32)
        .expect("text pixels");
    let red_bbox = bounding_box_for_color(&pixmap, |(r, g, b, _)| {
      // Detect reddish halo against white background without catching black text.
      let (r, g, b) = (r as u16, g as u16, b as u16);
      r > g + 20 && r > b + 20 && !(r < 40 && g < 40 && b < 40) && (g < 250 || b < 250)
    })
    .expect("shadow");

    assert!(
      red_bbox.0 > black_bbox.0 + 1,
      "shadow should be offset to the right"
    );
    assert!(
      red_bbox.1.abs_diff(black_bbox.1) <= 2,
      "shadow should align vertically when y-offset is zero"
    );
  }

  #[test]
  fn text_shadows_scale_with_device_pixel_ratio() {
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
        offset: Point::new(2.0, 0.0),
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

    let mut renderer = DisplayListRenderer::new_scaled(80, 40, Rgba::WHITE, font_ctx, 2.0)
      .expect("renderer with scale");
    let text_item = match &list.items()[0] {
      DisplayItem::Text(t) => t,
      _ => unreachable!(),
    };
    let scaled_item = renderer.scale_text_item(text_item);
    assert!(
      (scaled_item.shadows[0].offset.x - 4.0).abs() < 0.5,
      "shadow offset should scale with DPR (expected ~4, got {})",
      scaled_item.shadows[0].offset.x
    );
    let (_glyphs, bounds) = renderer.glyph_shadow_sources(&face, &font, &scaled_item);
    let shadow = &scaled_item.shadows[0];
    let shadow_min_x = bounds.min_x + shadow.offset.x;
    let shadow_max_x = bounds.max_x + shadow.offset.x;
    assert!(
      shadow_min_x > bounds.min_x,
      "shadow min x should move to the right with positive offset"
    );
    assert!(
      shadow_max_x - shadow_min_x >= bounds.max_x - bounds.min_x,
      "shadow bounds should not shrink relative to glyph bounds"
    );

    let pixmap = renderer.render(&list).expect("rendered");

    let black_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32)
        .expect("text pixels");
    let shadow_pixels = pixmap
      .data()
      .chunks_exact(4)
      .filter(|px| px[3] > 0 && px[0] > px[1] && px[0] > px[2])
      .count();
    assert!(shadow_pixels > 0, "expected shadow pixels to be present");

    let red_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > g && r > b).expect("shadow");

    let dx = red_bbox.0.saturating_sub(black_bbox.0);
    assert!(
      (3..=5).contains(&dx),
      "shadow should move ~4 device px at 2x scale (got {dx})"
    );
    assert!(
      red_bbox.1.abs_diff(black_bbox.1) <= 2,
      "shadow should stay aligned vertically"
    );
  }

  #[test]
  fn text_shadow_blur_scales_with_device_pixel_ratio() {
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
        offset: Point::new(0.0, 0.0),
        blur_radius: 4.0,
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

    let mut renderer = DisplayListRenderer::new_scaled(80, 40, Rgba::WHITE, font_ctx.clone(), 2.0)
      .expect("renderer with scale");
    let text_item = match &list.items()[0] {
      DisplayItem::Text(t) => t,
      _ => unreachable!(),
    };
    let scaled_item = renderer.scale_text_item(text_item);
    assert!(
      (scaled_item.shadows[0].blur_radius - 8.0).abs() < 0.5,
      "blur radius should scale with DPR (expected ~8, got {})",
      scaled_item.shadows[0].blur_radius
    );
    let (_glyphs, bounds) = renderer.glyph_shadow_sources(&face, &font, &scaled_item);
    let shadow = &scaled_item.shadows[0];
    let blur_margin = (shadow.blur_radius.abs() * 3.0).ceil();
    let shadow_min_x = bounds.min_x + shadow.offset.x - blur_margin;
    let shadow_max_x = bounds.max_x + shadow.offset.x + blur_margin;
    assert!(
      shadow_max_x - shadow_min_x > bounds.max_x - bounds.min_x,
      "shadow bounds should expand beyond glyph bounds"
    );

    let pixmap = renderer.render(&list).expect("rendered");
    let shadow_pixels = pixmap
      .data()
      .chunks_exact(4)
      .filter(|px| px[3] > 0 && px[0] > px[1] && px[0] > px[2])
      .count();
    assert!(shadow_pixels > 0, "expected shadow pixels to be present");
  }

  #[test]
  fn text_shadow_resolves_percent_and_em_units() {
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
      origin: Point::new(0.0, 20.0),
      glyphs: vec![GlyphInstance {
        glyph_id: glyph_id.0 as u32,
        offset: Point::new(0.0, 0.0),
        advance: 14.0,
      }],
      color: Rgba::BLACK,
      shadows: vec![TextShadowItem {
        offset: Point::new(10.0, 20.0), // percent(50%) + em(1.0) resolved against font size 20px
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

    let renderer = DisplayListRenderer::new(120, 100, Rgba::WHITE, font_ctx).expect("renderer");
    let pixmap = renderer.render(&list).expect("rendered");

    let black_bbox = bounding_box_for_color(&pixmap, |(r, g, b, a)| {
      a > 0 && r.abs_diff(g) <= 10 && r.abs_diff(b) <= 10 && r < 96 && g < 96 && b < 96
    })
    .expect("text pixels");
    let red_bbox = bounding_box_for_color(&pixmap, |(r, g, b, a)| {
      a > 0 && r > 32 && r > g.saturating_add(20) && r > b.saturating_add(20)
    })
    .expect("shadow");

    let dx = red_bbox.0 as i32 - black_bbox.0 as i32;
    let dy = red_bbox.1 as i32 - black_bbox.1 as i32;
    assert!(
      (9..=11).contains(&dx),
      "percent offset_x should resolve to ~10px (got {dx})"
    );
    assert!(
      (19..=21).contains(&dy),
      "1em offset_y should resolve to ~20px (got {dy})"
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
        inline_vertical: false,
        text: None,
      }),
      decorations: Vec::new(),
    }));

    let renderer =
      DisplayListRenderer::new(40, 40, Rgba::WHITE, FontContext::new()).expect("renderer");
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
      shape: ClipShape::Rect {
        rect: Rect::from_xywh(1.0, 1.0, 4.0, 4.0),
        radii: None,
      },
    }));
    // Start stacking context and narrow the clip further.
    list.push(DisplayItem::PushStackingContext(
      crate::paint::display_list::StackingContextItem {
        z_index: 0,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
        plane_rect: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
        mix_blend_mode: crate::paint::display_list::BlendMode::Normal,
        is_isolated: false,
        transform: None,
        child_perspective: None,
        transform_style: TransformStyle::Flat,
        backface_visibility: BackfaceVisibility::Visible,
        filters: Vec::new(),
        backdrop_filters: Vec::new(),
        radii: BorderRadii::ZERO,
        mask: None,
      },
    ));
    list.push(DisplayItem::PushClip(ClipItem {
      shape: ClipShape::Rect {
        rect: Rect::from_xywh(2.0, 2.0, 2.0, 2.0),
        radii: None,
      },
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
  fn backdrop_filters_respect_stacking_transforms() {
    let renderer = DisplayListRenderer::new(8, 6, Rgba::BLUE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();

    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
      plane_rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
      mix_blend_mode: crate::paint::display_list::BlendMode::Normal,
      is_isolated: false,
      transform: Some(Transform3D::translate(2.0, 1.0, 0.0)),
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: vec![ResolvedFilter::Invert(1.0)],
      radii: BorderRadii::ZERO,
      mask: None,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).expect("render");

    let origin_px = pixel(&pixmap, 0, 0);
    assert_eq!(
      origin_px,
      (0, 0, 255, 255),
      "backdrop filter should not run at the origin"
    );

    let translated_px = pixel(&pixmap, 2, 1);
    assert!(
      translated_px.0 > 200 && translated_px.1 > 200 && translated_px.2 < 80,
      "translated backdrop region should invert the backdrop, got {:?}",
      translated_px
    );
  }

  #[test]
  fn clip_path_pushes_path_mask() {
    let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    let triangle = crate::paint::clip_path::ResolvedClipPath::Polygon {
      points: vec![
        Point::new(0.0, 0.0),
        Point::new(4.0, 0.0),
        Point::new(0.0, 4.0),
      ],
      fill_rule: tiny_skia::FillRule::Winding,
    };
    list.push(DisplayItem::PushClip(ClipItem {
      shape: ClipShape::Path { path: triangle },
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      color: Rgba::rgb(0, 0, 255),
    }));
    list.push(DisplayItem::PopClip);

    let pixmap = renderer.render(&list).unwrap();
    assert_eq!(pixel(&pixmap, 0, 0), (0, 0, 255, 255));
    assert_eq!(pixel(&pixmap, 3, 3), (255, 255, 255, 255));
  }

  #[test]
  fn clip_path_with_bounded_layer_clips_content() {
    let renderer = DisplayListRenderer::new(12, 12, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    let triangle = ResolvedClipPath::Polygon {
      points: vec![
        Point::new(0.0, 0.0),
        Point::new(10.0, 0.0),
        Point::new(0.0, 10.0),
      ],
      fill_rule: tiny_skia::FillRule::Winding,
    };
    list.push(DisplayItem::PushClip(ClipItem {
      shape: ClipShape::Path { path: triangle },
    }));
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      plane_rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: None,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: vec![ResolvedFilter::Brightness(1.0)],
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      color: Rgba::rgb(0, 0, 255),
    }));
    list.push(DisplayItem::PopStackingContext);
    list.push(DisplayItem::PopClip);

    let pixmap = renderer.render(&list).unwrap();
    assert_eq!(pixel(&pixmap, 2, 2), (0, 0, 255, 255));
    assert_eq!(pixel(&pixmap, 9, 1), (255, 255, 255, 255));
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
        plane_rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
        mix_blend_mode: crate::paint::display_list::BlendMode::Multiply,
        is_isolated: false,
        transform: None,
        child_perspective: None,
        transform_style: TransformStyle::Flat,
        backface_visibility: BackfaceVisibility::Visible,
        filters: Vec::new(),
        backdrop_filters: Vec::new(),
        radii: BorderRadii::ZERO,
        mask: None,
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
  fn stacking_context_multiply_blend_darkens_background() {
    let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    // Gray backdrop (50% luminance)
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      color: Rgba::from_rgba8(128, 128, 128, 255),
    }));

    // Child stacking context with multiply blend paints solid red.
    list.push(DisplayItem::PushStackingContext(
      crate::paint::display_list::StackingContextItem {
        z_index: 0,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
        plane_rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
        mix_blend_mode: crate::paint::display_list::BlendMode::Multiply,
        is_isolated: false,
        transform: None,
        child_perspective: None,
        transform_style: TransformStyle::Flat,
        backface_visibility: BackfaceVisibility::Visible,
        filters: Vec::new(),
        backdrop_filters: Vec::new(),
        radii: BorderRadii::ZERO,
        mask: None,
      },
    ));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      color: Rgba::rgb(255, 0, 0),
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    let px = pixel(&pixmap, 0, 0);
    // Multiply of red (1,0,0) on 50% gray (0.5,0.5,0.5) yields ~128 red, zero green/blue.
    assert!(
      (px.0 as i32 - 128).abs() <= 2,
      "red channel should be ~128 (got {:?})",
      px
    );
    assert!(
      px.1 < 5 && px.2 < 5,
      "green/blue should be near zero (got {:?})",
      px
    );
    assert_eq!(px.3, 255, "alpha should remain opaque (got {:?})", px);
  }

  #[test]
  fn isolated_stacking_context_respects_blend_mode() {
    let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    // Gray backdrop (50% luminance)
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      color: Rgba::from_rgba8(128, 128, 128, 255),
    }));

    list.push(DisplayItem::PushStackingContext(
      crate::paint::display_list::StackingContextItem {
        z_index: 0,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
        plane_rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
        mix_blend_mode: crate::paint::display_list::BlendMode::Multiply,
        is_isolated: true,
        transform: None,
        child_perspective: None,
        transform_style: TransformStyle::Flat,
        backface_visibility: BackfaceVisibility::Visible,
        filters: Vec::new(),
        backdrop_filters: Vec::new(),
        radii: BorderRadii::ZERO,
        mask: None,
      },
    ));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      color: Rgba::rgb(255, 0, 0),
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    let px = pixel(&pixmap, 0, 0);
    // Multiply should still apply even when the stacking context is isolated.
    assert!(
      (px.0 as i32 - 128).abs() <= 2,
      "red channel should be ~128 (got {:?})",
      px
    );
    assert!(
      px.1 < 5 && px.2 < 5,
      "green/blue should be near zero (got {:?})",
      px
    );
    assert_eq!(px.3, 255, "alpha should remain opaque (got {:?})", px);
  }

  #[test]
  fn isolated_manual_blend_mode_applies() {
    let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    // Start with a gray backdrop to make hue blending distinguishable from source-over.
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      color: Rgba::from_rgba8(128, 128, 128, 255),
    }));

    list.push(DisplayItem::PushStackingContext(
      crate::paint::display_list::StackingContextItem {
        z_index: 0,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
        plane_rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
        mix_blend_mode: crate::paint::display_list::BlendMode::Hue,
        is_isolated: true,
        transform: None,
        child_perspective: None,
        transform_style: TransformStyle::Flat,
        backface_visibility: BackfaceVisibility::Visible,
        filters: Vec::new(),
        backdrop_filters: Vec::new(),
        radii: BorderRadii::ZERO,
        mask: None,
      },
    ));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      color: Rgba::rgb(255, 0, 0),
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    let px = pixel(&pixmap, 0, 0);
    // Hue takes destination saturation/lightness; with a gray backdrop the result stays gray.
    assert!(
      (px.0 as i32 - 128).abs() <= 2
        && (px.1 as i32 - 128).abs() <= 2
        && (px.2 as i32 - 128).abs() <= 2,
      "manual hue blend should preserve the gray backdrop (got {:?})",
      px
    );
    assert_eq!(px.3, 255, "alpha should remain opaque (got {:?})", px);
  }

  #[test]
  fn mask_clip_respects_content_box() {
    let renderer = DisplayListRenderer::new(6, 6, Rgba::WHITE, FontContext::new()).unwrap();

    let bounds = Rect::from_xywh(0.0, 0.0, 6.0, 6.0);
    let mut layer = MaskLayer::default();
    layer.image = Some(BackgroundImage::LinearGradient {
      angle: 0.0,
      stops: vec![
        ColorStop {
          color: Color::Rgba(Rgba::BLACK),
          position: Some(0.0),
        },
        ColorStop {
          color: Color::Rgba(Rgba::BLACK),
          position: Some(1.0),
        },
      ],
    });
    layer.repeat = BackgroundRepeat::no_repeat();
    layer.clip = MaskClip::ContentBox;
    let resolved_layer = ResolvedMaskLayer {
      image: ResolvedMaskImage::Generated(Box::new(layer.image.clone().unwrap())),
      repeat: layer.repeat,
      position: layer.position.clone(),
      size: layer.size.clone(),
      origin: layer.origin,
      clip: layer.clip,
      mode: layer.mode,
      composite: layer.composite,
    };
    let mask = ResolvedMask {
      layers: vec![resolved_layer],
      color: Rgba::BLACK,
      font_size: 16.0,
      root_font_size: 16.0,
      rects: mask_rects(bounds, (1.0, 1.0, 1.0, 1.0)),
    };

    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds,
      plane_rect: bounds,
      mix_blend_mode: BlendMode::Normal,
      is_isolated: true,
      transform: None,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: Some(mask),
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
      color: Rgba::GREEN,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 255, 255));
    assert_eq!(pixel(&pixmap, 1, 1), (0, 255, 0, 255));
  }

  #[test]
  fn mask_composite_intersects_layers() {
    let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();

    let vertical = BackgroundImage::LinearGradient {
      angle: 90.0,
      stops: vec![
        ColorStop {
          color: Color::Rgba(Rgba::BLACK),
          position: Some(0.0),
        },
        ColorStop {
          color: Color::Rgba(Rgba::BLACK),
          position: Some(0.5),
        },
        ColorStop {
          color: Color::Rgba(Rgba::TRANSPARENT),
          position: Some(0.5),
        },
        ColorStop {
          color: Color::Rgba(Rgba::TRANSPARENT),
          position: Some(1.0),
        },
      ],
    };
    let horizontal = BackgroundImage::LinearGradient {
      angle: 180.0,
      stops: vec![
        ColorStop {
          color: Color::Rgba(Rgba::BLACK),
          position: Some(0.0),
        },
        ColorStop {
          color: Color::Rgba(Rgba::BLACK),
          position: Some(0.5),
        },
        ColorStop {
          color: Color::Rgba(Rgba::TRANSPARENT),
          position: Some(0.5),
        },
        ColorStop {
          color: Color::Rgba(Rgba::TRANSPARENT),
          position: Some(1.0),
        },
      ],
    };

    let mut first_layer = MaskLayer::default();
    first_layer.image = Some(vertical.clone());
    first_layer.repeat = BackgroundRepeat::no_repeat();
    first_layer.size = BackgroundSize::Explicit(
      BackgroundSizeComponent::Length(Length::percent(100.0)),
      BackgroundSizeComponent::Length(Length::percent(100.0)),
    );
    first_layer.composite = MaskComposite::Intersect;

    let mut second_layer = MaskLayer::default();
    second_layer.image = Some(horizontal.clone());
    second_layer.repeat = BackgroundRepeat::no_repeat();
    second_layer.size = BackgroundSize::Explicit(
      BackgroundSizeComponent::Length(Length::percent(100.0)),
      BackgroundSizeComponent::Length(Length::percent(100.0)),
    );

    let resolved_layers = vec![
      ResolvedMaskLayer {
        image: ResolvedMaskImage::Generated(Box::new(vertical)),
        repeat: first_layer.repeat,
        position: first_layer.position.clone(),
        size: first_layer.size.clone(),
        origin: first_layer.origin,
        clip: first_layer.clip,
        mode: first_layer.mode,
        composite: first_layer.composite,
      },
      ResolvedMaskLayer {
        image: ResolvedMaskImage::Generated(Box::new(horizontal)),
        repeat: second_layer.repeat,
        position: second_layer.position.clone(),
        size: second_layer.size.clone(),
        origin: second_layer.origin,
        clip: second_layer.clip,
        mode: second_layer.mode,
        composite: second_layer.composite,
      },
    ];
    let mask = ResolvedMask {
      layers: resolved_layers,
      color: Rgba::BLACK,
      font_size: 16.0,
      root_font_size: 16.0,
      rects: mask_rects(Rect::from_xywh(0.0, 0.0, 4.0, 4.0), (0.0, 0.0, 0.0, 0.0)),
    };

    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      plane_rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: true,
      transform: None,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: Some(mask),
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      color: Rgba::BLUE,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    assert_eq!(pixel(&pixmap, 0, 0), (0, 0, 255, 255));
    assert_eq!(pixel(&pixmap, 3, 0), (255, 255, 255, 255));
    assert_eq!(pixel(&pixmap, 0, 3), (255, 255, 255, 255));
  }

  #[test]
  fn mask_gradient_without_terminal_stop_masks() {
    let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();

    let horizontal = BackgroundImage::LinearGradient {
      angle: 90.0,
      stops: vec![
        ColorStop {
          color: Color::Rgba(Rgba::BLACK),
          position: Some(0.0),
        },
        ColorStop {
          color: Color::Rgba(Rgba::BLACK),
          position: Some(0.5),
        },
        ColorStop {
          color: Color::Rgba(Rgba::TRANSPARENT),
          position: Some(0.5),
        },
      ],
    };

    let mut layer = MaskLayer::default();
    layer.image = Some(horizontal.clone());
    layer.repeat = BackgroundRepeat::no_repeat();
    layer.size = BackgroundSize::Explicit(
      BackgroundSizeComponent::Length(Length::percent(100.0)),
      BackgroundSizeComponent::Length(Length::percent(100.0)),
    );
    let mask = ResolvedMask {
      layers: vec![ResolvedMaskLayer {
        image: ResolvedMaskImage::Generated(Box::new(horizontal)),
        repeat: layer.repeat,
        position: layer.position.clone(),
        size: layer.size.clone(),
        origin: layer.origin,
        clip: layer.clip,
        mode: layer.mode,
        composite: layer.composite,
      }],
      color: Rgba::BLACK,
      font_size: 16.0,
      root_font_size: 16.0,
      rects: mask_rects(Rect::from_xywh(0.0, 0.0, 4.0, 4.0), (0.0, 0.0, 0.0, 0.0)),
    };

    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      plane_rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: true,
      transform: None,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: Some(mask),
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      color: Rgba::BLUE,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    assert_eq!(pixel(&pixmap, 0, 0), (0, 0, 255, 255));
    assert_eq!(pixel(&pixmap, 3, 0), (255, 255, 255, 255));
  }

  #[test]
  fn stacking_context_opacity_affects_descendants() {
    let mut root_style = ComputedStyle::default();
    root_style.background_color = Rgba::RED;
    root_style.opacity = 0.5;

    let mut child_style = ComputedStyle::default();
    child_style.background_color = Rgba::BLUE;

    let child = FragmentNode::new_block_styled(
      Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
      vec![],
      Arc::new(child_style),
    );
    let root = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
      vec![child],
      Arc::new(root_style),
    );

    let list = DisplayListBuilder::new().build_with_stacking_tree(&root);
    let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
    let pixmap = renderer.render(&list).unwrap();

    // Root background tinted by opacity.
    assert_eq!(pixel(&pixmap, 0, 0), (255, 128, 128, 255));
    // Child content is affected by the same stacking context opacity.
    assert_eq!(pixel(&pixmap, 1, 1), (128, 128, 255, 255));
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

    assert_eq!(
      single, overlap,
      "opacity should apply once across the group"
    );
    assert_eq!(single, (255, 128, 128, 255));
  }
}
