use rayon::prelude::*;
use tiny_skia::{Mask, Pixmap, PixmapPaint, PremultipliedColorU8, Transform};

const PARALLEL_THRESHOLD: usize = 2048;

/// 3x3 projective transform matrix.
///
/// Stored in row-major order:
/// ```text
/// [m0 m1 m2]
/// [m3 m4 m5]
/// [m6 m7 m8]
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Homography {
  pub m: [f32; 9],
}

impl Homography {
  pub const IDENTITY: Self = Self {
    m: [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0],
  };

  pub fn identity() -> Self {
    Self::IDENTITY
  }

  pub fn new(m: [f32; 9]) -> Self {
    Self { m }
  }

  /// Transform a point. Returns None for degenerate results (NaN/Inf or w=0).
  pub fn transform_point(&self, x: f32, y: f32) -> Option<(f32, f32)> {
    let hx = self.m[0] * x + self.m[1] * y + self.m[2];
    let hy = self.m[3] * x + self.m[4] * y + self.m[5];
    let hw = self.m[6] * x + self.m[7] * y + self.m[8];
    if !hx.is_finite() || !hy.is_finite() || !hw.is_finite() || hw.abs() < f32::EPSILON {
      return None;
    }
    Some((hx / hw, hy / hw))
  }

  /// Matrix multiplication (self * other).
  pub fn multiply(&self, other: &Homography) -> Homography {
    let mut out = [0.0_f32; 9];
    for row in 0..3 {
      for col in 0..3 {
        out[row * 3 + col] = self.m[row * 3 + 0] * other.m[col + 0]
          + self.m[row * 3 + 1] * other.m[col + 3]
          + self.m[row * 3 + 2] * other.m[col + 6];
      }
    }
    Homography { m: out }
  }

  /// Invert the homography. Returns None if not invertible.
  pub fn invert(&self) -> Option<Homography> {
    let m = &self.m;
    let det = m[0] * (m[4] * m[8] - m[5] * m[7])
      - m[1] * (m[3] * m[8] - m[5] * m[6])
      + m[2] * (m[3] * m[7] - m[4] * m[6]);
    if det.abs() < f32::EPSILON {
      return None;
    }
    let inv_det = 1.0 / det;
    let inv = [
      (m[4] * m[8] - m[5] * m[7]) * inv_det,
      (m[2] * m[7] - m[1] * m[8]) * inv_det,
      (m[1] * m[5] - m[2] * m[4]) * inv_det,
      (m[5] * m[6] - m[3] * m[8]) * inv_det,
      (m[0] * m[8] - m[2] * m[6]) * inv_det,
      (m[2] * m[3] - m[0] * m[5]) * inv_det,
      (m[3] * m[7] - m[4] * m[6]) * inv_det,
      (m[1] * m[6] - m[0] * m[7]) * inv_det,
      (m[0] * m[4] - m[1] * m[3]) * inv_det,
    ];
    Some(Homography { m: inv })
  }

  /// Build a homography that maps the unit square (0,0)-(1,1) to the given quadrilateral.
  pub fn from_unit_square_to_quad(dst: [(f32, f32); 4]) -> Option<Self> {
    let (x0, y0) = dst[0];
    let (x1, y1) = dst[1];
    let (x2, y2) = dst[2];
    let (x3, y3) = dst[3];

    let dx1 = x1 - x2;
    let dy1 = y1 - y2;
    let dx2 = x3 - x2;
    let dy2 = y3 - y2;
    let dx3 = x0 - x1 + x2 - x3;
    let dy3 = y0 - y1 + y2 - y3;

    if dx3.abs() < f32::EPSILON && dy3.abs() < f32::EPSILON {
      return Some(Homography {
        m: [
          x1 - x0,
          x3 - x0,
          x0,
          y1 - y0,
          y3 - y0,
          y0,
          0.0,
          0.0,
          1.0,
        ],
      });
    }

    let det = dx1 * dy2 - dx2 * dy1;
    if det.abs() < f32::EPSILON {
      return None;
    }
    let g = (dx3 * dy2 - dx2 * dy3) / det;
    let h = (dx1 * dy3 - dx3 * dy1) / det;

    Some(Homography {
      m: [
        x1 - x0 + g * x1,
        x3 - x0 + h * x3,
        x0,
        y1 - y0 + g * y1,
        y3 - y0 + h * y3,
        y0,
        g,
        h,
        1.0,
      ],
    })
  }

  /// Build a homography mapping a rectangle (0..src_width, 0..src_height) to a quadrilateral.
  pub fn from_rect_to_quad(src_width: f32, src_height: f32, dst: [(f32, f32); 4]) -> Option<Self> {
    if src_width <= 0.0 || src_height <= 0.0 {
      return None;
    }
    let scale = Homography {
      m: [
        1.0 / src_width,
        0.0,
        0.0,
        0.0,
        1.0 / src_height,
        0.0,
        0.0,
        0.0,
        1.0,
      ],
    };
    let to_quad = Homography::from_unit_square_to_quad(dst)?;
    Some(to_quad.multiply(&scale))
  }
}

/// Warp pixels from `src` into `dst` using a projective transform.
///
/// The homography maps source coordinates into destination coordinates; the inverse is used for
/// sampling. Bilinear filtering and premultiplied-alpha blending are applied.
pub fn warp_pixmap(
  dst: &mut Pixmap,
  src: &Pixmap,
  h_src_to_dst: &Homography,
  opacity: f32,
  blend_mode: tiny_skia::BlendMode,
  clip_mask: Option<&Mask>,
) {
  if src.width() == 0 || src.height() == 0 || dst.width() == 0 || dst.height() == 0 {
    return;
  }
  let Some(h_dst_to_src) = h_src_to_dst.invert() else {
    return;
  };

  let corners = [
    (0.0_f32, 0.0_f32),
    (src.width() as f32, 0.0_f32),
    (src.width() as f32, src.height() as f32),
    (0.0_f32, src.height() as f32),
  ];
  let mut min_x = f32::INFINITY;
  let mut min_y = f32::INFINITY;
  let mut max_x = f32::NEG_INFINITY;
  let mut max_y = f32::NEG_INFINITY;
  let mut has_point = false;
  for (x, y) in corners {
    if let Some((tx, ty)) = h_src_to_dst.transform_point(x, y) {
      if tx.is_finite() && ty.is_finite() {
        has_point = true;
        min_x = min_x.min(tx);
        min_y = min_y.min(ty);
        max_x = max_x.max(tx);
        max_y = max_y.max(ty);
      }
    }
  }
  if !has_point {
    return;
  }

  let dst_w = dst.width() as f32;
  let dst_h = dst.height() as f32;
  let x0 = min_x.floor().max(0.0).min(dst_w) as i32;
  let y0 = min_y.floor().max(0.0).min(dst_h) as i32;
  let x1 = max_x.ceil().max(0.0).min(dst_w) as i32;
  let y1 = max_y.ceil().max(0.0).min(dst_h) as i32;
  if x0 >= x1 || y0 >= y1 {
    return;
  }

  let region_w = (x1 - x0) as u32;
  let region_h = (y1 - y0) as u32;
  let Some(mut warped) = Pixmap::new(region_w, region_h) else {
    return;
  };

  let src_width = src.width() as usize;
  let src_height = src.height() as usize;
  let src_pixels = src.pixels();
  let opacity = opacity.clamp(0.0, 1.0);
  let pixel_count = region_w as usize * region_h as usize;
  if pixel_count > PARALLEL_THRESHOLD {
    warped
      .pixels_mut()
      .par_chunks_mut(region_w as usize)
      .enumerate()
      .for_each(|(row_idx, row)| {
        let y = y0 + row_idx as i32;
        let cy = y as f32 + 0.5;
        for (col_idx, px) in row.iter_mut().enumerate() {
          let x = x0 + col_idx as i32;
          let cx = x as f32 + 0.5;
          let Some((sx, sy)) = h_dst_to_src.transform_point(cx as f32, cy) else {
            continue;
          };
          let sample = sample_bilinear(src_pixels, src_width, src_height, sx, sy);
          if sample[3] <= 0.0 {
            continue;
          }
          *px = pack_color([
            sample[0] * opacity,
            sample[1] * opacity,
            sample[2] * opacity,
            sample[3] * opacity,
          ]);
        }
      });
  } else {
    for row_idx in 0..region_h as i32 {
      let y = y0 + row_idx;
      let cy = y as f32 + 0.5;
      let row_start = row_idx as usize * region_w as usize;
      let row_slice = &mut warped.pixels_mut()[row_start..row_start + region_w as usize];
      for col_idx in 0..region_w as i32 {
        let x = x0 + col_idx;
        let cx = x as f32 + 0.5;
        if let Some((sx, sy)) = h_dst_to_src.transform_point(cx as f32, cy) {
          let sample = sample_bilinear(src_pixels, src_width, src_height, sx, sy);
          if sample[3] <= 0.0 {
            continue;
          }
          row_slice[col_idx as usize] = pack_color([
            sample[0] * opacity,
            sample[1] * opacity,
            sample[2] * opacity,
            sample[3] * opacity,
          ]);
        }
      }
    }
  }

  let paint = PixmapPaint {
    opacity: 1.0,
    blend_mode,
    ..Default::default()
  };
  let _ = dst.draw_pixmap(
    x0,
    y0,
    warped.as_ref(),
    &paint,
    Transform::identity(),
    clip_mask,
  );
}

fn sample_bilinear(
  pixels: &[PremultipliedColorU8],
  width: usize,
  height: usize,
  x: f32,
  y: f32,
) -> [f32; 4] {
  if !x.is_finite() || !y.is_finite() || width == 0 || height == 0 {
    return [0.0; 4];
  }

  let sx = x - 0.5;
  let sy = y - 0.5;
  let x0 = sx.floor() as i32;
  let y0 = sy.floor() as i32;
  let tx = sx - x0 as f32;
  let ty = sy - y0 as f32;
  let x1 = x0 + 1;
  let y1 = y0 + 1;

  let weights = [
    ((1.0 - tx) * (1.0 - ty), x0, y0),
    (tx * (1.0 - ty), x1, y0),
    ((1.0 - tx) * ty, x0, y1),
    (tx * ty, x1, y1),
  ];

  let mut out = [0.0_f32; 4];
  for &(w, px, py) in &weights {
    if w <= 0.0 || px < 0 || py < 0 || px >= width as i32 || py >= height as i32 {
      continue;
    }
    let idx = py as usize * width + px as usize;
    let c = pixels[idx];
    out[0] += c.red() as f32 / 255.0 * w;
    out[1] += c.green() as f32 / 255.0 * w;
    out[2] += c.blue() as f32 / 255.0 * w;
    out[3] += c.alpha() as f32 / 255.0 * w;
  }
  out
}

fn pack_color(channels: [f32; 4]) -> PremultipliedColorU8 {
  let clamp = |v: f32, max: u8| -> u8 {
    let scaled = (v * 255.0 + 0.5).clamp(0.0, 255.0) as u8;
    scaled.min(max)
  };
  let a = clamp(channels[3], u8::MAX);
  let r = clamp(channels[0], a);
  let g = clamp(channels[1], a);
  let b = clamp(channels[2], a);
  PremultipliedColorU8::from_rgba(r, g, b, a).unwrap_or(PremultipliedColorU8::TRANSPARENT)
}

#[cfg(test)]
mod tests {
  use super::*;

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

    let mut dst = Pixmap::new(2, 2).unwrap();
    warp_pixmap(
      &mut dst,
      &src,
      &Homography::IDENTITY,
      1.0,
      tiny_skia::BlendMode::SourceOver,
      None,
    );

    for y in 0..2 {
      for x in 0..2 {
        assert_eq!(pixel(&dst, x, y), pixel(&src, x, y));
      }
    }
  }

  #[test]
  fn warp_to_trapezoid_places_corners() {
    let mut src = Pixmap::new(2, 2).unwrap();
    let src_pixels = src.pixels_mut();
    src_pixels[0] = color(255, 0, 0, 255);
    src_pixels[1] = color(0, 255, 0, 255);
    src_pixels[2] = color(0, 0, 255, 255);
    src_pixels[3] = color(255, 255, 255, 255);

    let mut dst = Pixmap::new(6, 6).unwrap();
    let homography = Homography::from_rect_to_quad(
      2.0,
      2.0,
      [(1.0, 1.0), (4.0, 1.0), (5.0, 4.5), (0.5, 4.5)],
    )
    .unwrap();
    warp_pixmap(
      &mut dst,
      &src,
      &homography,
      1.0,
      tiny_skia::BlendMode::SourceOver,
      None,
    );

    let inv = homography.invert().unwrap();
    let sample_at = |x: u32, y: u32| {
      inv
        .transform_point(x as f32 + 0.5, y as f32 + 0.5)
        .map(|(sx, sy)| {
          pack_color(sample_bilinear(
            src.pixels(),
            src.width() as usize,
            src.height() as usize,
            sx,
            sy,
          ))
        })
        .unwrap_or(PremultipliedColorU8::TRANSPARENT)
    };

    assert_eq!(pixel(&dst, 1, 1), sample_at(1, 1));
    assert_eq!(pixel(&dst, 4, 1), sample_at(4, 1));
    assert_eq!(pixel(&dst, 0, 4), sample_at(0, 4));
    assert_eq!(pixel(&dst, 4, 4), sample_at(4, 4));
  }

  #[test]
  fn clip_mask_limits_warp_region() {
    let mut src = Pixmap::new(3, 3).unwrap();
    src.pixels_mut().fill(color(200, 0, 0, 255));

    let mut dst = Pixmap::new(3, 3).unwrap();
    dst
      .pixels_mut()
      .fill(color(0, 200, 0, 255));

    let mut mask_pixmap = Pixmap::new(3, 3).unwrap();
    mask_pixmap.pixels_mut().fill(color(0, 0, 0, 0));
    mask_pixmap.pixels_mut()[4] = color(255, 255, 255, 255);
    let mask = Mask::from_pixmap(mask_pixmap.as_ref(), tiny_skia::MaskType::Alpha);

    warp_pixmap(
      &mut dst,
      &src,
      &Homography::IDENTITY,
      1.0,
      tiny_skia::BlendMode::SourceOver,
      Some(&mask),
    );

    for y in 0..3 {
      for x in 0..3 {
        let px = pixel(&dst, x, y);
        if x == 1 && y == 1 {
          assert!(px.red() > 0);
        } else {
          assert_eq!(px.red(), 0);
          assert_eq!(px.green(), 200);
        }
      }
    }
  }
}
