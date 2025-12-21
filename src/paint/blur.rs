use tiny_skia::Pixmap;

pub(crate) fn gaussian_kernel(sigma: f32) -> (Vec<f32>, usize) {
  let radius = (sigma.abs() * 3.0).ceil() as usize;
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

pub(crate) fn apply_gaussian_blur(pixmap: &mut Pixmap, sigma: f32) {
  let radius = (sigma.abs() * 3.0).ceil() as usize;
  if radius == 0 {
    return;
  }

  let (kernel, radius) = gaussian_kernel(sigma);
  if kernel.is_empty() {
    return;
  }

  let width = pixmap.width() as usize;
  let height = pixmap.height() as usize;
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

  // Horizontal pass
  for y in 0..height {
    for x in 0..width {
      let mut accum = [0.0; 4];
      let mut weight_sum = 0.0;
      for (i, weight) in kernel.iter().enumerate() {
        let offset = i as isize - radius as isize;
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

  // Vertical pass
  for y in 0..height {
    for x in 0..width {
      let mut accum = [0.0; 4];
      let mut weight_sum = 0.0;
      for (i, weight) in kernel.iter().enumerate() {
        let offset = i as isize - radius as isize;
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

  for (src_px, vals) in pixmap.pixels_mut().iter_mut().zip(dst.iter()) {
    let r = (vals[0] * 255.0).round().clamp(0.0, 255.0) as u8;
    let g = (vals[1] * 255.0).round().clamp(0.0, 255.0) as u8;
    let b = (vals[2] * 255.0).round().clamp(0.0, 255.0) as u8;
    let a = (vals[3] * 255.0).round().clamp(0.0, 255.0) as u8;
    *src_px = tiny_skia::PremultipliedColorU8::from_rgba(r, g, b, a)
      .unwrap_or(tiny_skia::PremultipliedColorU8::TRANSPARENT);
  }
}
