use tiny_skia::Pixmap;

const FAST_GAUSS_THRESHOLD_SIGMA: f32 = 4.0;

pub(crate) fn gaussian_kernel(sigma: f32) -> (Vec<f32>, usize) {
  let sigma = sigma.abs();
  let radius = (sigma * 3.0).ceil() as usize;
  if radius == 0 {
    return (Vec::new(), 0);
  }

  let mut kernel = Vec::with_capacity(radius * 2 + 1);
  let sigma_sq = sigma * sigma;
  let factor = 1.0 / (2.0 * std::f32::consts::PI * sigma_sq);
  let mut sum = 0.0;

  for i in 0..=radius * 2 {
    let x = i as f32 - radius as f32;
    let value = factor * (-x * x / (2.0 * sigma_sq)).exp();
    kernel.push(value);
    sum += value;
  }

  // Normalize kernel
  if sum != 0.0 {
    for k in &mut kernel {
      *k /= sum;
    }
  }
  (kernel, radius)
}

fn gaussian_kernel_fixed(sigma: f32) -> (Vec<i32>, usize, i32) {
  let sigma = sigma.abs();
  let radius = (sigma * 3.0).ceil() as usize;
  if radius == 0 {
    return (Vec::new(), 0, 0);
  }

  // Fixed-point scale chosen to keep accumulators in i32.
  const SCALE: i32 = 1 << 16;
  let mut weights = Vec::with_capacity(radius * 2 + 1);
  let sigma_sq = sigma as f64 * sigma as f64;
  let denom = 2.0 * sigma_sq;
  let mut sum = 0.0f64;
  for i in 0..=radius * 2 {
    let x = i as i32 - radius as i32;
    let w = (-(x as f64 * x as f64) / denom).exp();
    weights.push(w);
    sum += w;
  }
  if sum == 0.0 {
    return (Vec::new(), 0, 0);
  }

  let mut fixed = Vec::with_capacity(weights.len());
  let mut fixed_sum: i32 = 0;
  for (idx, w) in weights.iter().enumerate() {
    let mut v = ((*w / sum) * SCALE as f64).round() as i32;
    if idx == weights.len().saturating_sub(1) {
      v = SCALE - fixed_sum;
    }
    fixed_sum += v;
    fixed.push(v);
  }

  (fixed, radius, SCALE)
}

#[inline]
fn clamp_channel_to_alpha(channel: i32, alpha: i32) -> u8 {
  channel.clamp(0, alpha).clamp(0, 255) as u8
}

fn gaussian_convolve_premultiplied(pixmap: &mut Pixmap, sigma_x: f32, sigma_y: f32) {
  let (kernel_x, radius_x, scale_x) = gaussian_kernel_fixed(sigma_x);
  let (kernel_y, radius_y, scale_y) = gaussian_kernel_fixed(sigma_y);
  if (radius_x == 0 || kernel_x.is_empty() || scale_x <= 0)
    && (radius_y == 0 || kernel_y.is_empty() || scale_y <= 0)
  {
    return;
  }

  let width = pixmap.width() as usize;
  let height = pixmap.height() as usize;
  if width == 0 || height == 0 {
    return;
  }

  let len = width * height * 4;
  let mut src = pixmap.data().to_vec();
  let mut buf = vec![0u8; len];

  if radius_x == 0 || kernel_x.is_empty() || scale_x <= 0 {
    buf.copy_from_slice(&src);
  } else {
    // Horizontal pass: src -> buf
    for y in 0..height {
      let row_start = y * width * 4;
      for x in 0..width {
        let mut acc_r: i32 = 0;
        let mut acc_g: i32 = 0;
        let mut acc_b: i32 = 0;
        let mut acc_a: i32 = 0;
        for (i, &w) in kernel_x.iter().enumerate() {
          let offset = i as isize - radius_x as isize;
          let cx = (x as isize + offset).clamp(0, width as isize - 1) as usize;
          let idx = row_start + cx * 4;
          acc_r += w * src[idx] as i32;
          acc_g += w * src[idx + 1] as i32;
          acc_b += w * src[idx + 2] as i32;
          acc_a += w * src[idx + 3] as i32;
        }
        let out_idx = row_start + x * 4;
        let a = ((acc_a + scale_x / 2) / scale_x).clamp(0, 255);
        let r = (acc_r + scale_x / 2) / scale_x;
        let g = (acc_g + scale_x / 2) / scale_x;
        let b = (acc_b + scale_x / 2) / scale_x;
        buf[out_idx] = clamp_channel_to_alpha(r, a);
        buf[out_idx + 1] = clamp_channel_to_alpha(g, a);
        buf[out_idx + 2] = clamp_channel_to_alpha(b, a);
        buf[out_idx + 3] = a as u8;
      }
    }
  }

  if radius_y == 0 || kernel_y.is_empty() || scale_y <= 0 {
    pixmap.data_mut().copy_from_slice(&buf);
    return;
  }

  // Vertical pass: buf -> src (reuse src storage)
  for y in 0..height {
    for x in 0..width {
      let mut acc_r: i32 = 0;
      let mut acc_g: i32 = 0;
      let mut acc_b: i32 = 0;
      let mut acc_a: i32 = 0;
      for (i, &w) in kernel_y.iter().enumerate() {
        let offset = i as isize - radius_y as isize;
        let cy = (y as isize + offset).clamp(0, height as isize - 1) as usize;
        let idx = (cy * width + x) * 4;
        acc_r += w * buf[idx] as i32;
        acc_g += w * buf[idx + 1] as i32;
        acc_b += w * buf[idx + 2] as i32;
        acc_a += w * buf[idx + 3] as i32;
      }
      let out_idx = (y * width + x) * 4;
      let a = ((acc_a + scale_y / 2) / scale_y).clamp(0, 255);
      let r = (acc_r + scale_y / 2) / scale_y;
      let g = (acc_g + scale_y / 2) / scale_y;
      let b = (acc_b + scale_y / 2) / scale_y;
      src[out_idx] = clamp_channel_to_alpha(r, a);
      src[out_idx + 1] = clamp_channel_to_alpha(g, a);
      src[out_idx + 2] = clamp_channel_to_alpha(b, a);
      src[out_idx + 3] = a as u8;
    }
  }

  pixmap.data_mut().copy_from_slice(&src);
}

fn box_sizes_for_gauss(sigma: f32, n: usize) -> [usize; 3] {
  debug_assert!(n == 3);
  let sigma = sigma.abs().max(0.0) as f64;
  if sigma <= 0.0 {
    return [1, 1, 1];
  }
  // From: https://www.peterkovesi.com/matlabfns/Spatial/gaussfilt.m
  let n = n as f64;
  let w_ideal = ((12.0 * sigma * sigma / n) + 1.0).sqrt();
  let mut wl = w_ideal.floor() as i32;
  if wl % 2 == 0 {
    wl -= 1;
  }
  let wu = wl + 2;

  let m_ideal = (12.0 * sigma * sigma - n * (wl * wl) as f64 - 4.0 * n * wl as f64 - 3.0 * n)
    / (-4.0 * wl as f64 - 4.0);
  let m = m_ideal.round().clamp(0.0, n) as usize;

  let mut sizes = [wu as usize; 3];
  for s in sizes.iter_mut().take(m) {
    *s = wl as usize;
  }
  sizes
}

fn box_blur_h(src: &[u8], dst: &mut [u8], width: usize, height: usize, radius: usize) {
  if radius == 0 {
    dst.copy_from_slice(src);
    return;
  }
  let window = (radius * 2 + 1) as i32;
  let half = window / 2;
  for y in 0..height {
    let row_start = y * width * 4;
    let mut sum_r: i32 = 0;
    let mut sum_g: i32 = 0;
    let mut sum_b: i32 = 0;
    let mut sum_a: i32 = 0;
    for dx in -(radius as isize)..=(radius as isize) {
      let cx = dx.clamp(0, width as isize - 1) as usize;
      let idx = row_start + cx * 4;
      sum_r += src[idx] as i32;
      sum_g += src[idx + 1] as i32;
      sum_b += src[idx + 2] as i32;
      sum_a += src[idx + 3] as i32;
    }

    for x in 0..width {
      let out_idx = row_start + x * 4;
      let a = ((sum_a + half) / window).clamp(0, 255);
      let r = (sum_r + half) / window;
      let g = (sum_g + half) / window;
      let b = (sum_b + half) / window;
      dst[out_idx] = clamp_channel_to_alpha(r, a);
      dst[out_idx + 1] = clamp_channel_to_alpha(g, a);
      dst[out_idx + 2] = clamp_channel_to_alpha(b, a);
      dst[out_idx + 3] = a as u8;

      let remove_x = (x as isize - radius as isize).clamp(0, width as isize - 1) as usize;
      let add_x = (x as isize + radius as isize + 1).clamp(0, width as isize - 1) as usize;
      let rem_idx = row_start + remove_x * 4;
      let add_idx = row_start + add_x * 4;
      sum_r += src[add_idx] as i32 - src[rem_idx] as i32;
      sum_g += src[add_idx + 1] as i32 - src[rem_idx + 1] as i32;
      sum_b += src[add_idx + 2] as i32 - src[rem_idx + 2] as i32;
      sum_a += src[add_idx + 3] as i32 - src[rem_idx + 3] as i32;
    }
  }
}

fn box_blur_v(src: &[u8], dst: &mut [u8], width: usize, height: usize, radius: usize) {
  if radius == 0 {
    dst.copy_from_slice(src);
    return;
  }
  let window = (radius * 2 + 1) as i32;
  let half = window / 2;
  for x in 0..width {
    let mut sum_r: i32 = 0;
    let mut sum_g: i32 = 0;
    let mut sum_b: i32 = 0;
    let mut sum_a: i32 = 0;
    for dy in -(radius as isize)..=(radius as isize) {
      let cy = dy.clamp(0, height as isize - 1) as usize;
      let idx = (cy * width + x) * 4;
      sum_r += src[idx] as i32;
      sum_g += src[idx + 1] as i32;
      sum_b += src[idx + 2] as i32;
      sum_a += src[idx + 3] as i32;
    }

    for y in 0..height {
      let out_idx = (y * width + x) * 4;
      let a = ((sum_a + half) / window).clamp(0, 255);
      let r = (sum_r + half) / window;
      let g = (sum_g + half) / window;
      let b = (sum_b + half) / window;
      dst[out_idx] = clamp_channel_to_alpha(r, a);
      dst[out_idx + 1] = clamp_channel_to_alpha(g, a);
      dst[out_idx + 2] = clamp_channel_to_alpha(b, a);
      dst[out_idx + 3] = a as u8;

      let remove_y = (y as isize - radius as isize).clamp(0, height as isize - 1) as usize;
      let add_y = (y as isize + radius as isize + 1).clamp(0, height as isize - 1) as usize;
      let rem_idx = (remove_y * width + x) * 4;
      let add_idx = (add_y * width + x) * 4;
      sum_r += src[add_idx] as i32 - src[rem_idx] as i32;
      sum_g += src[add_idx + 1] as i32 - src[rem_idx + 1] as i32;
      sum_b += src[add_idx + 2] as i32 - src[rem_idx + 2] as i32;
      sum_a += src[add_idx + 3] as i32 - src[rem_idx + 3] as i32;
    }
  }
}

fn gaussian_blur_box_approx(pixmap: &mut Pixmap, sigma_x: f32, sigma_y: f32) {
  let sigma_x = sigma_x.abs();
  let sigma_y = sigma_y.abs();
  if sigma_x == 0.0 && sigma_y == 0.0 {
    return;
  }
  let width = pixmap.width() as usize;
  let height = pixmap.height() as usize;
  if width == 0 || height == 0 {
    return;
  }
  let len = width * height * 4;
  let mut a = pixmap.data().to_vec();
  let mut b = vec![0u8; len];

  let sizes_x = box_sizes_for_gauss(sigma_x, 3);
  let sizes_y = box_sizes_for_gauss(sigma_y, 3);
  for i in 0..3 {
    let radius_x = sizes_x[i].saturating_sub(1) / 2;
    let radius_y = sizes_y[i].saturating_sub(1) / 2;
    if radius_x == 0 && radius_y == 0 {
      continue;
    }
    if radius_x > 0 {
      box_blur_h(&a, &mut b, width, height, radius_x);
    } else {
      b.copy_from_slice(&a);
    }
    if radius_y > 0 {
      box_blur_v(&b, &mut a, width, height, radius_y);
    } else {
      a.copy_from_slice(&b);
    }
  }

  pixmap.data_mut().copy_from_slice(&a);
}

pub(crate) fn apply_gaussian_blur_anisotropic(
  pixmap: &mut Pixmap,
  sigma_x: f32,
  sigma_y: f32,
) {
  let sigma_x = sigma_x.abs();
  let sigma_y = sigma_y.abs();
  if sigma_x == 0.0 && sigma_y == 0.0 {
    return;
  }

  let fast_enabled = std::env::var("FASTR_FAST_BLUR")
    .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
    .unwrap_or(false);
  if fast_enabled {
    if sigma_x <= FAST_GAUSS_THRESHOLD_SIGMA && sigma_y <= FAST_GAUSS_THRESHOLD_SIGMA {
      gaussian_convolve_premultiplied(pixmap, sigma_x, sigma_y);
    } else {
      gaussian_blur_box_approx(pixmap, sigma_x, sigma_y);
    }
    return;
  }

  gaussian_blur_precise(pixmap, sigma_x, sigma_y);
}

pub(crate) fn apply_gaussian_blur(pixmap: &mut Pixmap, sigma: f32) {
  apply_gaussian_blur_anisotropic(pixmap, sigma, sigma);
}

fn gaussian_blur_precise(pixmap: &mut Pixmap, sigma_x: f32, sigma_y: f32) {
  let (kernel_x, radius_x) = gaussian_kernel(sigma_x);
  let (kernel_y, radius_y) = gaussian_kernel(sigma_y);
  if (radius_x == 0 || kernel_x.is_empty()) && (radius_y == 0 || kernel_y.is_empty()) {
    return;
  }

  let width = pixmap.width() as usize;
  let height = pixmap.height() as usize;
  if width == 0 || height == 0 {
    return;
  }

  let src: Vec<[f32; 4]> = pixmap
    .pixels()
    .iter()
    .map(|p| {
      [
        p.red() as f32 / 255.0,
        p.green() as f32 / 255.0,
        p.blue() as f32 / 255.0,
        p.alpha() as f32 / 255.0,
      ]
    })
    .collect();

  let mut temp = vec![[0.0; 4]; src.len()];
  let mut dst = vec![[0.0; 4]; src.len()];

  if radius_x == 0 || kernel_x.is_empty() {
    temp.copy_from_slice(&src);
  } else {
    for y in 0..height {
      for x in 0..width {
        let mut accum = [0.0; 4];
        let mut weight_sum = 0.0;
        for (i, weight) in kernel_x.iter().enumerate() {
          let offset = i as isize - radius_x as isize;
          let cx = (x as isize + offset).clamp(0, width as isize - 1) as usize;
          let sample = src[y * width + cx];
          accum[0] += sample[0] * weight;
          accum[1] += sample[1] * weight;
          accum[2] += sample[2] * weight;
          accum[3] += sample[3] * weight;
          weight_sum += weight;
        }
        let idx = y * width + x;
        temp[idx] = [
          accum[0] / weight_sum,
          accum[1] / weight_sum,
          accum[2] / weight_sum,
          accum[3] / weight_sum,
        ];
      }
    }
  }

  if radius_y == 0 || kernel_y.is_empty() {
    dst.copy_from_slice(&temp);
  } else {
    for y in 0..height {
      for x in 0..width {
        let mut accum = [0.0; 4];
        let mut weight_sum = 0.0;
        for (i, weight) in kernel_y.iter().enumerate() {
          let offset = i as isize - radius_y as isize;
          let cy = (y as isize + offset).clamp(0, height as isize - 1) as usize;
          let sample = temp[cy * width + x];
          accum[0] += sample[0] * weight;
          accum[1] += sample[1] * weight;
          accum[2] += sample[2] * weight;
          accum[3] += sample[3] * weight;
          weight_sum += weight;
        }
        let idx = y * width + x;
        dst[idx] = [
          accum[0] / weight_sum,
          accum[1] / weight_sum,
          accum[2] / weight_sum,
          accum[3] / weight_sum,
        ];
      }
    }
  }

  for (src_px, vals) in pixmap.pixels_mut().iter_mut().zip(dst.iter()) {
    let r = (vals[0] * 255.0).round().clamp(0.0, 255.0) as u8;
    let g = (vals[1] * 255.0).round().clamp(0.0, 255.0) as u8;
    let b = (vals[2] * 255.0).round().clamp(0.0, 255.0) as u8;
    let a = (vals[3] * 255.0).round().clamp(0.0, 255.0) as u8;
    *src_px = tiny_skia::PremultipliedColorU8::from_rgba(r, g, b, a)
      .unwrap_or(tiny_skia::PremultipliedColorU8::TRANSPARENT);
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use tiny_skia::PremultipliedColorU8;

  #[test]
  fn blur_with_zero_sigma_x_only_blurs_vertically() {
    let mut pixmap = Pixmap::new(3, 5).unwrap();
    let idx = 2 * 3 + 1;
    pixmap.pixels_mut()[idx] = PremultipliedColorU8::from_rgba(255, 255, 255, 255).unwrap();

    apply_gaussian_blur_anisotropic(&mut pixmap, 0.0, 2.0);

    let pixels = pixmap.pixels();
    let mut center_non_zero = 0;
    for (i, px) in pixels.iter().enumerate() {
      let x = i % 3;
      if x == 1 {
        if px.alpha() > 0 {
          center_non_zero += 1;
        }
      } else {
        assert_eq!(
          px.alpha(),
          0,
          "blur spread horizontally to column {}",
          x
        );
      }
    }
    assert!(center_non_zero > 1, "expected vertical spread in center column");
  }
}
