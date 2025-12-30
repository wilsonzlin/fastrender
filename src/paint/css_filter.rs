//! Shared CSS filter utilities.
//!
//! These helpers mirror the legacy painter backend implementations and operate
//! on unpremultiplied channel values to avoid precision loss on semi-transparent
//! pixels. They are used by both the legacy painter and the display list
//! renderer to keep filter behavior in sync.

use crate::error::RenderError;
use crate::paint::blur::{alpha_bounds, apply_gaussian_blur};
use crate::paint::pixmap::new_pixmap;
use crate::render_control::{active_deadline, with_deadline};
use crate::style::color::Rgba;
use rayon::prelude::*;
use tiny_skia::BlendMode as SkiaBlendMode;
use tiny_skia::{Pixmap, PixmapPaint, PremultipliedColorU8, Transform};

const COLOR_FILTER_PARALLEL_THRESHOLD: usize = 2048;

pub(crate) fn apply_color_filter<F>(pixmap: &mut Pixmap, f: F)
where
  F: Fn([f32; 3], f32) -> ([f32; 3], f32) + Send + Sync,
{
  let pixels = pixmap.pixels_mut();
  if pixels.len() > COLOR_FILTER_PARALLEL_THRESHOLD {
    let deadline = active_deadline();
    pixels
      .par_iter_mut()
      .for_each(|px| with_deadline(deadline.as_ref(), || apply_color_filter_to_pixel(px, &f)));
  } else {
    for px in pixels.iter_mut() {
      if std::env::var("DEBUG_FILTER_PIXEL").is_ok() {
        eprintln!(
          "filter in: r={} g={} b={} a={}",
          px.red(),
          px.green(),
          px.blue(),
          px.alpha()
        );
      }
      apply_color_filter_to_pixel(px, &f);
      if std::env::var("DEBUG_FILTER_PIXEL").is_ok() {
        eprintln!(
          "filter out: r={} g={} b={} a={}",
          px.red(),
          px.green(),
          px.blue(),
          px.alpha()
        );
      }
    }
  }
}

fn apply_color_filter_to_pixel<F>(px: &mut PremultipliedColorU8, f: &F)
where
  F: Fn([f32; 3], f32) -> ([f32; 3], f32),
{
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

pub(crate) fn scale_color(color: [f32; 3], factor: f32) -> [f32; 3] {
  [color[0] * factor, color[1] * factor, color[2] * factor]
}

pub(crate) fn apply_contrast(color: [f32; 3], factor: f32) -> [f32; 3] {
  [
    (color[0] - 0.5) * factor + 0.5,
    (color[1] - 0.5) * factor + 0.5,
    (color[2] - 0.5) * factor + 0.5,
  ]
}

pub(crate) fn grayscale(color: [f32; 3], amount: f32) -> [f32; 3] {
  let gray = color[0] * 0.2126 + color[1] * 0.7152 + color[2] * 0.0722;
  [
    color[0] + (gray - color[0]) * amount,
    color[1] + (gray - color[1]) * amount,
    color[2] + (gray - color[2]) * amount,
  ]
}

pub(crate) fn sepia(color: [f32; 3], amount: f32) -> [f32; 3] {
  let sepia_r = color[0] * 0.393 + color[1] * 0.769 + color[2] * 0.189;
  let sepia_g = color[0] * 0.349 + color[1] * 0.686 + color[2] * 0.168;
  let sepia_b = color[0] * 0.272 + color[1] * 0.534 + color[2] * 0.131;
  [
    color[0] + (sepia_r - color[0]) * amount,
    color[1] + (sepia_g - color[1]) * amount,
    color[2] + (sepia_b - color[2]) * amount,
  ]
}

pub(crate) fn saturate(color: [f32; 3], factor: f32) -> [f32; 3] {
  let rw = 0.213;
  let gw = 0.715;
  let bw = 0.072;
  [
    (rw + (1.0 - rw) * factor) * color[0]
      + (gw - gw * factor) * color[1]
      + (bw - bw * factor) * color[2],
    (rw - rw * factor) * color[0]
      + (gw + (1.0 - gw) * factor) * color[1]
      + (bw - bw * factor) * color[2],
    (rw - rw * factor) * color[0]
      + (gw - gw * factor) * color[1]
      + (bw + (1.0 - bw) * factor) * color[2],
  ]
}

pub(crate) fn hue_rotate(color: [f32; 3], degrees: f32) -> [f32; 3] {
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

pub(crate) fn invert(color: [f32; 3], amount: f32) -> [f32; 3] {
  [
    color[0] + (1.0 - color[0] - color[0]) * amount,
    color[1] + (1.0 - color[1] - color[1]) * amount,
    color[2] + (1.0 - color[2] - color[2]) * amount,
  ]
}

pub(crate) fn apply_spread(pixmap: &mut Pixmap, spread: f32) {
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

  let mut base_ratio = (0.0, 0.0, 0.0);
  for px in src.iter() {
    let alpha = px.alpha();
    if alpha > 0 {
      let a = alpha as f32;
      base_ratio = (
        px.red() as f32 / a,
        px.green() as f32 / a,
        px.blue() as f32 / a,
      );
      break;
    }
  }

  for y in 0..height {
    for x in 0..width {
      let mut agg_alpha = if expand { 0u8 } else { 255u8 };
      for dy in -radius..=radius {
        for dx in -radius..=radius {
          let ny = (y + dy).clamp(0, height - 1);
          let nx = (x + dx).clamp(0, width - 1);
          let idx = (ny as usize) * (width as usize) + nx as usize;
          let px = src[idx];
          if expand {
            agg_alpha = agg_alpha.max(px.alpha());
          } else {
            agg_alpha = agg_alpha.min(px.alpha());
          }
        }
      }
      let idx = (y as usize) * (width as usize) + x as usize;
      if agg_alpha == 0 {
        dst[idx] = PremultipliedColorU8::TRANSPARENT;
        continue;
      }

      let orig = src[idx];
      let orig_alpha = orig.alpha();
      if orig_alpha > 0 {
        let factor = (agg_alpha as f32) / (orig_alpha as f32);
        let r = (orig.red() as f32 * factor).round().clamp(0.0, 255.0) as u8;
        let g = (orig.green() as f32 * factor).round().clamp(0.0, 255.0) as u8;
        let b = (orig.blue() as f32 * factor).round().clamp(0.0, 255.0) as u8;
        dst[idx] = PremultipliedColorU8::from_rgba(r, g, b, agg_alpha)
          .unwrap_or(PremultipliedColorU8::TRANSPARENT);
      } else {
        let r = (base_ratio.0 * agg_alpha as f32).round().clamp(0.0, 255.0) as u8;
        let g = (base_ratio.1 * agg_alpha as f32).round().clamp(0.0, 255.0) as u8;
        let b = (base_ratio.2 * agg_alpha as f32).round().clamp(0.0, 255.0) as u8;
        dst[idx] = PremultipliedColorU8::from_rgba(r, g, b, agg_alpha)
          .unwrap_or(PremultipliedColorU8::TRANSPARENT);
      }
    }
  }
}

pub(crate) fn apply_drop_shadow(
  pixmap: &mut Pixmap,
  offset_x: f32,
  offset_y: f32,
  blur_radius: f32,
  spread: f32,
  color: Rgba,
) -> Result<(), RenderError> {
  if pixmap.width() == 0 || pixmap.height() == 0 {
    return Ok(());
  }

  let source = pixmap.clone();
  let Some((min_x, min_y, bounds_w, bounds_h)) = alpha_bounds(&source) else {
    return Ok(());
  };
  let blur_pad = (blur_radius.abs() * 3.0).ceil() as u32;
  let spread_pad = spread.max(0.0).ceil() as u32;
  let pad = blur_pad + spread_pad;

  let mut shadow = match new_pixmap(bounds_w + pad * 2, bounds_h + pad * 2) {
    Some(p) => p,
    None => return Ok(()),
  };

  {
    let src = source.pixels();
    let src_stride = source.width() as usize;
    let dst_stride = shadow.width() as usize;
    let dst = shadow.pixels_mut();
    for y in 0..bounds_h as usize {
      let src_row = (min_y as usize + y) * src_stride;
      let dst_row = (pad as usize + y) * dst_stride;
      for x in 0..bounds_w as usize {
        let src_px = src[src_row + min_x as usize + x];
        let alpha = src_px.alpha() as f32 / 255.0;
        let dst_idx = dst_row + pad as usize + x;
        if alpha == 0.0 {
          dst[dst_idx] = PremultipliedColorU8::TRANSPARENT;
          continue;
        }
        let total_alpha = (color.a * alpha).clamp(0.0, 1.0);
        let r = (color.r as f32 / 255.0) * total_alpha;
        let g = (color.g as f32 / 255.0) * total_alpha;
        let b = (color.b as f32 / 255.0) * total_alpha;
        let a = total_alpha * 255.0;
        dst[dst_idx] = PremultipliedColorU8::from_rgba(
          (r * 255.0).round() as u8,
          (g * 255.0).round() as u8,
          (b * 255.0).round() as u8,
          a.round().clamp(0.0, 255.0) as u8,
        )
        .unwrap_or(PremultipliedColorU8::TRANSPARENT);
      }
    }
  }

  if spread != 0.0 {
    apply_spread(&mut shadow, spread);
  }

  if blur_radius > 0.0 {
    apply_gaussian_blur(&mut shadow, blur_radius)?;
  }

  let mut result = match new_pixmap(source.width(), source.height()) {
    Some(p) => p,
    None => return Ok(()),
  };

  let dest_x = min_x as i32 - pad as i32;
  let dest_y = min_y as i32 - pad as i32;
  let mut paint = PixmapPaint::default();
  paint.blend_mode = SkiaBlendMode::SourceOver;
  result.draw_pixmap(
    dest_x,
    dest_y,
    shadow.as_ref(),
    &paint,
    Transform::from_translate(offset_x, offset_y),
    None,
  );
  result.draw_pixmap(0, 0, source.as_ref(), &paint, Transform::identity(), None);

  *pixmap = result;
  Ok(())
}
