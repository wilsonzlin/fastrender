use crate::geometry::Point;
use crate::paint::homography::{quad_bounds, Homography};
use crate::render_control::{active_deadline, with_deadline};
use lru::LruCache;
use rayon::prelude::*;
use std::num::NonZeroUsize;
use std::sync::Arc;
use tiny_skia::Mask;
use tiny_skia::Pixmap;
use tiny_skia::PremultipliedColorU8;

pub struct WarpedPixmap {
  pub pixmap: Pixmap,
  pub offset: (i32, i32),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WarpCacheKey {
  src_id: usize,
  src_dims: (u32, u32),
  homography: [u32; 9],
  dst_quad: [u32; 8],
  target: (u32, u32),
  clip: Option<(usize, u32, u32)>,
}

pub struct WarpCache {
  lru: LruCache<WarpCacheKey, Arc<WarpedPixmap>>,
}

impl WarpCache {
  pub fn new(capacity: usize) -> Self {
    let cap = NonZeroUsize::new(capacity).unwrap_or_else(|| NonZeroUsize::new(1).unwrap());
    Self {
      lru: LruCache::new(cap),
    }
  }

  pub fn get_or_insert(
    &mut self,
    key: WarpCacheKey,
    build: impl FnOnce() -> Option<WarpedPixmap>,
  ) -> Option<Arc<WarpedPixmap>> {
    if let Some(existing) = self.lru.get(&key) {
      return Some(existing.clone());
    }
    let Some(new) = build() else {
      return None;
    };
    let arc = Arc::new(new);
    self.lru.put(key, arc.clone());
    Some(arc)
  }
}

const PARALLEL_THRESHOLD: u32 = 2048;

pub fn warp_cache_key(
  src: &Pixmap,
  homography: &Homography,
  dst_quad: &[(f32, f32); 4],
  target_size: (u32, u32),
  clip: Option<&Mask>,
) -> Option<WarpCacheKey> {
  if !homography.is_finite() {
    return None;
  }
  let mut homography_bits = [0u32; 9];
  for (out, v) in homography_bits.iter_mut().zip(homography.m.iter()) {
    if !v.is_finite() {
      return None;
    }
    *out = v.to_bits();
  }

  if dst_quad
    .iter()
    .any(|(x, y)| !x.is_finite() || !y.is_finite())
  {
    return None;
  }
  let mut quad_bits = [0u32; 8];
  for (i, (x, y)) in dst_quad.iter().enumerate() {
    quad_bits[i * 2] = x.to_bits();
    quad_bits[i * 2 + 1] = y.to_bits();
  }

  Some(WarpCacheKey {
    src_id: src.pixels().as_ptr() as usize,
    src_dims: (src.width(), src.height()),
    homography: homography_bits,
    dst_quad: quad_bits,
    target: target_size,
    clip: clip.map(|mask| (mask.data().as_ptr() as usize, mask.width(), mask.height())),
  })
}

pub fn warp_pixmap(
  src: &Pixmap,
  homography: &Homography,
  dst_quad: &[(f32, f32); 4],
  target_size: (u32, u32),
  clip: Option<&Mask>,
) -> Option<WarpedPixmap> {
  if target_size.0 == 0 || target_size.1 == 0 {
    return None;
  }
  if src.width() == 0 || src.height() == 0 {
    return None;
  }
  if !homography.is_finite() {
    return None;
  }
  if dst_quad
    .iter()
    .any(|(x, y)| !x.is_finite() || !y.is_finite())
  {
    return None;
  }
  if quad_area(dst_quad).abs() < 1e-3 {
    return None;
  }
  let inv = homography.inverse()?;
  let dst_points: [Point; 4] = dst_quad.map(|(x, y)| Point::new(x, y));
  let bounds = quad_bounds(&dst_points);
  if bounds.width() <= 0.0
    || bounds.height() <= 0.0
    || !bounds.x().is_finite()
    || !bounds.y().is_finite()
  {
    return None;
  }

  let (target_w, target_h) = target_size;
  let mask_w = clip.map(|m| m.width()).unwrap_or(target_w);
  let mask_h = clip.map(|m| m.height()).unwrap_or(target_h);
  let bound_w = target_w.min(mask_w);
  let bound_h = target_h.min(mask_h);

  let mut min_x_i = bounds.min_x().floor() as i32;
  let mut max_x_i = bounds.max_x().ceil() as i32;
  let mut min_y_i = bounds.min_y().floor() as i32;
  let mut max_y_i = bounds.max_y().ceil() as i32;
  min_x_i = min_x_i.clamp(0, bound_w as i32);
  max_x_i = max_x_i.clamp(0, bound_w as i32);
  min_y_i = min_y_i.clamp(0, bound_h as i32);
  max_y_i = max_y_i.clamp(0, bound_h as i32);

  let width = max_x_i.saturating_sub(min_x_i) as u32;
  let height = max_y_i.saturating_sub(min_y_i) as u32;
  if width == 0 || height == 0 {
    return None;
  }

  let src_w = src.width() as i32;
  let src_h = src.height() as i32;

  let mut output = Pixmap::new(width, height)?;
  let clip_data = clip.map(|m| (m.data(), m.width() as usize, m.height() as usize));
  let area = width.saturating_mul(height);

  let process_row = |row_idx: usize, row: &mut [PremultipliedColorU8]| {
    let global_y = min_y_i + row_idx as i32;
    for (col_idx, dst_px) in row.iter_mut().enumerate() {
      let global_x = min_x_i + col_idx as i32;
      let clip_alpha = clip_data
        .as_ref()
        .and_then(|(data, stride, height)| {
          if global_x < 0 || global_y < 0 || global_y as usize >= *height {
            None
          } else {
            data.get(global_y as usize * *stride + global_x as usize)
          }
        })
        .copied()
        .unwrap_or(255);
      if clip_alpha == 0 {
        continue;
      }
      let dx = global_x as f32 + 0.5;
      let dy = global_y as f32 + 0.5;
      if !point_in_quad((dx, dy), dst_quad) {
        continue;
      }
      let Some((sx, sy)) = inv.transform_point(dx, dy) else {
        continue;
      };
      if sx < 0.0 || sy < 0.0 || sx >= src_w as f32 || sy >= src_h as f32 {
        continue;
      }
      let mut sample = sample_bilinear(src, sx, sy);
      if clip_alpha < 255 {
        let scale = clip_alpha as f32 / 255.0;
        let premultiply = |v: u8| ((v as f32 * scale).round().clamp(0.0, 255.0)) as u8;
        sample = PremultipliedColorU8::from_rgba(
          premultiply(sample.red()),
          premultiply(sample.green()),
          premultiply(sample.blue()),
          premultiply(sample.alpha()),
        )
        .unwrap_or(sample);
      }
      *dst_px = sample;
    }
  };

  if area > PARALLEL_THRESHOLD {
    let deadline = active_deadline();
    output
      .pixels_mut()
      .par_chunks_mut(width as usize)
      .enumerate()
      .for_each(|(row_idx, row)| with_deadline(deadline.as_ref(), || process_row(row_idx, row)));
  } else {
    for (row_idx, row) in output.pixels_mut().chunks_mut(width as usize).enumerate() {
      process_row(row_idx, row);
    }
  }

  Some(WarpedPixmap {
    pixmap: output,
    offset: (min_x_i, min_y_i),
  })
}

pub fn warp_pixmap_cached(
  cache: Option<&mut WarpCache>,
  src: &Pixmap,
  homography: &Homography,
  dst_quad: &[(f32, f32); 4],
  target_size: (u32, u32),
  clip: Option<&Mask>,
) -> Option<Arc<WarpedPixmap>> {
  if let Some(cache) = cache {
    if let Some(key) = warp_cache_key(src, homography, dst_quad, target_size, clip) {
      return cache.get_or_insert(key, || {
        warp_pixmap(src, homography, dst_quad, target_size, clip)
      });
    }
  }
  warp_pixmap(src, homography, dst_quad, target_size, clip).map(Arc::new)
}

fn quad_area(quad: &[(f32, f32); 4]) -> f32 {
  let mut area = 0.0;
  for i in 0..4 {
    let (x0, y0) = quad[i];
    let (x1, y1) = quad[(i + 1) % 4];
    area += x0 * y1 - x1 * y0;
  }
  0.5 * area
}

fn point_in_quad(p: (f32, f32), quad: &[(f32, f32); 4]) -> bool {
  let mut sign = 0.0f32;
  for i in 0..4 {
    let (x1, y1) = quad[i];
    let (x2, y2) = quad[(i + 1) % 4];
    let cross = (x2 - x1) * (p.1 - y1) - (y2 - y1) * (p.0 - x1);
    if !cross.is_finite() {
      return false;
    }
    if cross.abs() < 1e-6 {
      continue;
    }
    if sign.abs() < 1e-6 {
      sign = cross;
      continue;
    }
    if cross * sign < 0.0 {
      return false;
    }
  }
  true
}

fn sample_bilinear(src: &Pixmap, x: f32, y: f32) -> PremultipliedColorU8 {
  let sx0 = x.floor() as i32;
  let sy0 = y.floor() as i32;
  let sx1 = (sx0 + 1).min(src.width() as i32 - 1);
  let sy1 = (sy0 + 1).min(src.height() as i32 - 1);

  let fx = x - sx0 as f32;
  let fy = y - sy0 as f32;

  let c00 = src
    .pixel(sx0 as u32, sy0 as u32)
    .unwrap_or(PremultipliedColorU8::TRANSPARENT);
  let c10 = src
    .pixel(sx1 as u32, sy0 as u32)
    .unwrap_or(PremultipliedColorU8::TRANSPARENT);
  let c01 = src
    .pixel(sx0 as u32, sy1 as u32)
    .unwrap_or(PremultipliedColorU8::TRANSPARENT);
  let c11 = src
    .pixel(sx1 as u32, sy1 as u32)
    .unwrap_or(PremultipliedColorU8::TRANSPARENT);

  let lerp = |a: f32, b: f32, t: f32| a + (b - a) * t;
  let top_r = lerp(c00.red() as f32, c10.red() as f32, fx);
  let top_g = lerp(c00.green() as f32, c10.green() as f32, fx);
  let top_b = lerp(c00.blue() as f32, c10.blue() as f32, fx);
  let top_a = lerp(c00.alpha() as f32, c10.alpha() as f32, fx);

  let bot_r = lerp(c01.red() as f32, c11.red() as f32, fx);
  let bot_g = lerp(c01.green() as f32, c11.green() as f32, fx);
  let bot_b = lerp(c01.blue() as f32, c11.blue() as f32, fx);
  let bot_a = lerp(c01.alpha() as f32, c11.alpha() as f32, fx);

  let r = lerp(top_r, bot_r, fy).round().clamp(0.0, 255.0) as u8;
  let g = lerp(top_g, bot_g, fy).round().clamp(0.0, 255.0) as u8;
  let b = lerp(top_b, bot_b, fy).round().clamp(0.0, 255.0) as u8;
  let a = lerp(top_a, bot_a, fy).round().clamp(0.0, 255.0) as u8;

  PremultipliedColorU8::from_rgba(r, g, b, a).unwrap_or(PremultipliedColorU8::TRANSPARENT)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::Point;
  use tiny_skia::MaskType;

  fn color(r: u8, g: u8, b: u8, a: u8) -> PremultipliedColorU8 {
    PremultipliedColorU8::from_rgba(r, g, b, a).unwrap()
  }

  fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> PremultipliedColorU8 {
    pixmap.pixels()[(y * pixmap.width() + x) as usize]
  }

  #[test]
  fn warp_identity_matches_source() {
    let mut src = Pixmap::new(2, 2).unwrap();
    let src_pixels = src.pixels_mut();
    src_pixels[0] = color(255, 0, 0, 255);
    src_pixels[1] = color(0, 255, 0, 255);
    src_pixels[2] = color(0, 0, 255, 255);
    src_pixels[3] = color(255, 255, 255, 255);

    let dst_quad = [(0.0, 0.0), (2.0, 0.0), (2.0, 2.0), (0.0, 2.0)];
    let warped = warp_pixmap(&src, &Homography::identity(), &dst_quad, (2, 2), None).expect("warp");

    assert_eq!(warped.offset, (0, 0));
    assert_eq!(warped.pixmap.width(), 2);
    assert_eq!(warped.pixmap.height(), 2);

    for y in 0..2 {
      for x in 0..2 {
        assert_eq!(pixel(&warped.pixmap, x, y), pixel(&src, x, y));
      }
    }
  }

  #[test]
  fn warp_to_trapezoid_matches_inverse_sampling() {
    let mut src = Pixmap::new(2, 2).unwrap();
    let src_pixels = src.pixels_mut();
    src_pixels[0] = color(255, 0, 0, 255);
    src_pixels[1] = color(0, 255, 0, 255);
    src_pixels[2] = color(0, 0, 255, 255);
    src_pixels[3] = color(255, 255, 255, 255);

    let dst_quad = [(1.0, 1.0), (4.0, 1.0), (5.0, 4.5), (0.5, 4.5)];
    let src_quad = [(0.0, 0.0), (2.0, 0.0), (2.0, 2.0), (0.0, 2.0)];
    let src_points: [Point; 4] = src_quad.map(|(x, y)| Point::new(x, y));
    let dst_points: [Point; 4] = dst_quad.map(|(x, y)| Point::new(x, y));

    let homography = Homography::from_quad_to_quad(src_points, dst_points).expect("homography");

    let warped = warp_pixmap(&src, &homography, &dst_quad, (6, 6), None).expect("warp");
    let inv = homography.invert().expect("invert");

    let expected_at = |x: i32, y: i32| {
      let dx = x as f32 + 0.5;
      let dy = y as f32 + 0.5;
      let Some((sx, sy)) = inv.transform_point(dx, dy) else {
        return PremultipliedColorU8::TRANSPARENT;
      };
      if sx < 0.0 || sy < 0.0 || sx >= src.width() as f32 || sy >= src.height() as f32 {
        return PremultipliedColorU8::TRANSPARENT;
      }
      sample_bilinear(&src, sx, sy)
    };

    let samples = [(1, 1), (4, 1), (0, 4), (4, 4)];
    for (x, y) in samples {
      let local_x = x - warped.offset.0;
      let local_y = y - warped.offset.1;
      if local_x < 0
        || local_y < 0
        || local_x >= warped.pixmap.width() as i32
        || local_y >= warped.pixmap.height() as i32
      {
        continue;
      }
      assert_eq!(
        pixel(&warped.pixmap, local_x as u32, local_y as u32),
        expected_at(x, y)
      );
    }
  }

  #[test]
  fn clip_mask_limits_warp_region() {
    let mut src = Pixmap::new(3, 3).unwrap();
    src.pixels_mut().fill(color(200, 0, 0, 255));

    let dst_quad = [(0.0, 0.0), (3.0, 0.0), (3.0, 3.0), (0.0, 3.0)];
    let mut mask_pixmap = Pixmap::new(3, 3).unwrap();
    mask_pixmap.pixels_mut().fill(color(0, 0, 0, 0));
    mask_pixmap.pixels_mut()[4] = color(0, 0, 0, 255);
    let mask = Mask::from_pixmap(mask_pixmap.as_ref(), MaskType::Alpha);

    let warped = warp_pixmap(
      &src,
      &Homography::identity(),
      &dst_quad,
      (3, 3),
      Some(&mask),
    )
    .expect("warp");

    for y in 0..3 {
      for x in 0..3 {
        let px = pixel(&warped.pixmap, x, y);
        if x == 1 && y == 1 {
          assert!(px.red() > 0);
          assert_eq!(px.alpha(), 255);
        } else {
          assert_eq!(px, PremultipliedColorU8::TRANSPARENT);
        }
      }
    }
  }
}
