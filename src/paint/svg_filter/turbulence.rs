use crate::geometry::Rect;
use crate::paint::pixmap::new_pixmap;
use crate::error::RenderStage;
use crate::render_control::{active_deadline, check_active, with_deadline};
use rayon::prelude::*;
use tiny_skia::{Pixmap, PremultipliedColorU8};

use super::{RenderResult, TurbulenceType, FILTER_DEADLINE_STRIDE};

#[derive(Clone, Copy, Debug)]
struct ResolvedRegion {
  x: u32,
  y: u32,
  width: u32,
  height: u32,
}

fn resolve_region(
  filter_region: Rect,
  output_width: u32,
  output_height: u32,
) -> Option<ResolvedRegion> {
  if output_width == 0 || output_height == 0 {
    return None;
  }

  let width = output_width as i32;
  let height = output_height as i32;

  let min_x = filter_region.min_x().floor() as i32;
  let min_y = filter_region.min_y().floor() as i32;
  let max_x = filter_region.max_x().ceil() as i32;
  let max_y = filter_region.max_y().ceil() as i32;

  let start_x = min_x.clamp(0, width);
  let start_y = min_y.clamp(0, height);
  let end_x = max_x.clamp(0, width);
  let end_y = max_y.clamp(0, height);

  if start_x >= end_x || start_y >= end_y {
    return None;
  }

  Some(ResolvedRegion {
    x: start_x as u32,
    y: start_y as u32,
    width: (end_x - start_x) as u32,
    height: (end_y - start_y) as u32,
  })
}

pub(super) fn render_turbulence(
  output_width: u32,
  output_height: u32,
  filter_region: Rect,
  base_frequency: (f32, f32),
  seed: u32,
  octaves: u32,
  stitch_tiles: bool,
  kind: TurbulenceType,
) -> RenderResult<Option<Pixmap>> {
  check_active(RenderStage::Paint)?;
  let region = match resolve_region(filter_region, output_width, output_height) {
    Some(region) => region,
    None => return Ok(new_pixmap(output_width, output_height)),
  };

  let mut pixmap = match new_pixmap(output_width, output_height) {
    Some(pixmap) => pixmap,
    None => return Ok(None),
  };

  let perm_table = build_permutation(seed);
  let octaves = octaves.max(1);
  let normalization: f32 = (0..octaves).map(|i| 0.5_f32.powi(i as i32)).sum();

  let row_len = output_width as usize;
  let start_x = region.x as usize;
  let end_x = start_x + region.width as usize;
  let base_freq_x = base_frequency.0.abs();
  let base_freq_y = base_frequency.1.abs();
  let stitch_width = region.width.saturating_sub(1).max(1);
  let stitch_height = region.height.saturating_sub(1).max(1);

  let deadline = active_deadline();
  pixmap
    .pixels_mut()
    .par_chunks_mut(row_len)
    .enumerate()
    .skip(region.y as usize)
    .take(region.height as usize)
    .try_for_each(|(y, row)| {
      with_deadline(deadline.as_ref(), || -> RenderResult<()> {
        let y_coord = (y - region.y as usize) as f32;
        let row_slice = &mut row[start_x..end_x];
        for (x_offset, px) in row_slice.iter_mut().enumerate() {
          if x_offset % FILTER_DEADLINE_STRIDE == 0 {
            check_active(RenderStage::Paint)?;
          }
          let x_coord = x_offset as f32;
          let mut freq_x = base_freq_x;
          let mut freq_y = base_freq_y;
          let mut amplitude = 1.0;
          let mut value = 0.0;

          for _ in 0..octaves {
            let (freq_x_adj, wrap_x) = adjust_frequency(freq_x, stitch_width, stitch_tiles);
            let (freq_y_adj, wrap_y) = adjust_frequency(freq_y, stitch_height, stitch_tiles);
            let noise = if freq_x_adj == 0.0 && freq_y_adj == 0.0 {
              0.0
            } else {
              perlin(
                x_coord * freq_x_adj,
                y_coord * freq_y_adj,
                &perm_table,
                wrap_x,
                wrap_y,
              )
            };
            let noise = match kind {
              TurbulenceType::FractalNoise => noise,
              TurbulenceType::Turbulence => noise.abs(),
            };
            value += noise * amplitude;
            freq_x *= 2.0;
            freq_y *= 2.0;
            amplitude *= 0.5;
          }

          let normalized = if normalization > 0.0 {
            value / normalization
          } else {
            0.0
          };
          let mapped = (normalized * 0.5 + 0.5).clamp(0.0, 1.0);
          let byte = (mapped * 255.0).round().clamp(0.0, 255.0) as u8;
          *px = PremultipliedColorU8::from_rgba(byte, byte, byte, 255)
            .unwrap_or(PremultipliedColorU8::TRANSPARENT);
        }
        Ok(())
      })
    })?;

  Ok(Some(pixmap))
}

fn adjust_frequency(freq: f32, extent: u32, stitch: bool) -> (f32, Option<i32>) {
  if !stitch || extent == 0 {
    return (freq, None);
  }
  let mut wrap = (freq * extent as f32).round() as i32;
  if wrap == 0 {
    if freq == 0.0 {
      return (0.0, None);
    }
    wrap = 1;
  }
  if wrap < 0 {
    wrap = -wrap;
  }
  let adjusted = wrap as f32 / extent as f32;
  (adjusted, Some(wrap))
}

fn perlin(x: f32, y: f32, perm: &[u8; 512], wrap_x: Option<i32>, wrap_y: Option<i32>) -> f32 {
  let xi0 = x.floor() as i32;
  let yi0 = y.floor() as i32;
  let xf = x - xi0 as f32;
  let yf = y - yi0 as f32;

  let xi1 = xi0 + 1;
  let yi1 = yi0 + 1;

  let u = fade(xf);
  let v = fade(yf);

  let n00 = grad(hash(xi0, yi0, perm, wrap_x, wrap_y), xf, yf);
  let n10 = grad(hash(xi1, yi0, perm, wrap_x, wrap_y), xf - 1.0, yf);
  let n01 = grad(hash(xi0, yi1, perm, wrap_x, wrap_y), xf, yf - 1.0);
  let n11 = grad(hash(xi1, yi1, perm, wrap_x, wrap_y), xf - 1.0, yf - 1.0);

  let x1 = lerp(n00, n10, u);
  let x2 = lerp(n01, n11, u);
  lerp(x1, x2, v)
}

fn hash(xi: i32, yi: i32, perm: &[u8; 512], wrap_x: Option<i32>, wrap_y: Option<i32>) -> u8 {
  let xi = wrap_index(xi, wrap_x);
  let yi = wrap_index(yi, wrap_y);
  perm[(perm[xi] as usize + yi) & 255]
}

fn wrap_index(idx: i32, wrap: Option<i32>) -> usize {
  let value = match wrap {
    Some(period) if period > 0 => {
      let mut v = idx % period;
      if v < 0 {
        v += period;
      }
      v as usize
    }
    _ => (idx & 255) as usize,
  };
  value & 255
}

fn fade(t: f32) -> f32 {
  t * t * t * (t * (t * 6.0 - 15.0) + 10.0)
}

fn lerp(a: f32, b: f32, t: f32) -> f32 {
  a + t * (b - a)
}

fn grad(hash: u8, x: f32, y: f32) -> f32 {
  let h = hash & 7;
  let u = if h < 4 { x } else { y };
  let v = if h < 4 { y } else { x };
  let a = if (h & 1) == 0 { u } else { -u };
  let b = if (h & 2) == 0 { v } else { -v };
  a + b
}

fn build_permutation(seed: u32) -> [u8; 512] {
  let mut source = [0u8; 256];
  for (i, v) in source.iter_mut().enumerate() {
    *v = i as u8;
  }
  let mut rng = XorShift32::new(seed);
  for i in (1..256).rev() {
    let j = (rng.next_u32() % ((i + 1) as u32)) as usize;
    source.swap(i, j);
  }
  let mut perm = [0u8; 512];
  for i in 0..512 {
    perm[i] = source[i & 255];
  }
  perm
}

#[derive(Clone)]
struct XorShift32 {
  state: u32,
}

impl XorShift32 {
  fn new(seed: u32) -> Self {
    let state = seed.wrapping_add(1).wrapping_mul(0x9e37_79b9);
    Self { state }
  }

  fn next_u32(&mut self) -> u32 {
    let mut x = self.state;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    self.state = x;
    x
  }
}
