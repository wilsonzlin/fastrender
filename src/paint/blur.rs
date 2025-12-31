use crate::debug::runtime;
use crate::error::{RenderError, RenderStage};
use crate::paint::painter::with_paint_diagnostics;
use crate::paint::pixmap::{guard_allocation_bytes, new_pixmap_with_context};
use crate::paint::svg_filter::FilterCacheConfig;
use crate::render_control::{active_deadline, check_active, check_active_periodic, DeadlineGuard};
use lru::LruCache;
use rayon::prelude::*;
use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::hash::BuildHasherDefault;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Mutex;
use tiny_skia::Pixmap;

const FAST_GAUSS_THRESHOLD_SIGMA: f32 = 4.0;
const BLUR_DEADLINE_STRIDE: usize = 256;
const PARALLEL_BLUR_MIN_PIXELS: usize = 512 * 512;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BlurParallelism {
  Auto,
  Serial,
}

type BlurHasher = BuildHasherDefault<DefaultHasher>;

#[derive(Default)]
struct BlurScratch {
  ping: Vec<u8>,
  pong: Vec<u8>,
}

impl BlurScratch {
  fn ensure_len(&mut self, len: usize, context: &str) -> Result<(), RenderError> {
    if self.ping.len() < len {
      self
        .ping
        .try_reserve_exact(len.saturating_sub(self.ping.len()))
        .map_err(|err| RenderError::InvalidParameters {
          message: format!("{context}: blur scratch allocation failed: {err}"),
        })?;
      self.ping.resize(len, 0);
    }
    if self.pong.len() < len {
      self
        .pong
        .try_reserve_exact(len.saturating_sub(self.pong.len()))
        .map_err(|err| RenderError::InvalidParameters {
          message: format!("{context}: blur scratch allocation failed: {err}"),
        })?;
      self.pong.resize(len, 0);
    }
    Ok(())
  }

  fn split(&mut self, len: usize, context: &str) -> Result<(&mut [u8], &mut [u8]), RenderError> {
    self.ensure_len(len, context)?;
    Ok((&mut self.ping[..len], &mut self.pong[..len]))
  }
}

thread_local! {
  static BLUR_SCRATCH: RefCell<BlurScratch> = RefCell::new(BlurScratch::default());
}

fn blur_buffer_len(width: usize, height: usize, context: &str) -> Result<usize, RenderError> {
  let pixels = (width as u64)
    .checked_mul(height as u64)
    .ok_or(RenderError::InvalidParameters {
      message: format!("{context}: blur dimensions overflow ({width}x{height})"),
    })?;
  let bytes = pixels
    .checked_mul(4)
    .ok_or(RenderError::InvalidParameters {
      message: format!("{context}: blur buffer byte size overflow ({width}x{height})"),
    })?;
  guard_allocation_bytes(bytes, context)
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct BlurCacheKey {
  width: u32,
  height: u32,
  sigma_x_bits: u32,
  sigma_y_bits: u32,
  scale_bits: u32,
  fingerprint: u64,
}

impl BlurCacheKey {
  pub(crate) fn new(radius_x: f32, radius_y: f32, scale: f32, pixmap: &Pixmap) -> Option<Self> {
    let radius_x = radius_x.abs();
    let radius_y = radius_y.abs();
    if (radius_x == 0.0 && radius_y == 0.0) || pixmap.width() == 0 || pixmap.height() == 0 {
      return None;
    }
    Some(Self {
      width: pixmap.width(),
      height: pixmap.height(),
      sigma_x_bits: radius_x.to_bits(),
      sigma_y_bits: radius_y.to_bits(),
      scale_bits: scale.to_bits(),
      fingerprint: pixel_fingerprint(pixmap.data()),
    })
  }
}

pub struct BlurCache {
  lru: LruCache<BlurCacheKey, Pixmap, BlurHasher>,
  current_bytes: usize,
  config: FilterCacheConfig,
}

impl BlurCache {
  pub(crate) fn new(config: FilterCacheConfig) -> Self {
    Self {
      lru: LruCache::unbounded_with_hasher(BlurHasher::default()),
      current_bytes: 0,
      config,
    }
  }

  pub(crate) fn get(&mut self, key: &BlurCacheKey) -> Option<Pixmap> {
    let hit = self.lru.get(key).map(Clone::clone);
    if hit.is_some() {
      record_blur_cache_hit();
    } else {
      record_blur_cache_miss();
    }
    hit
  }

  pub(crate) fn put(&mut self, key: BlurCacheKey, pixmap: &Pixmap) {
    if self.config.max_items == 0 {
      return;
    }
    let weight = pixmap.data().len();
    if self.config.max_bytes > 0 && weight > self.config.max_bytes {
      return;
    }

    if let Some(existing) = self.lru.peek(&key) {
      let existing_weight: usize = existing.data().len();
      self.current_bytes = self.current_bytes.saturating_sub(existing_weight);
    }

    self.current_bytes = self.current_bytes.saturating_add(weight);
    self.lru.put(key, pixmap.clone());
    self.evict();
  }

  fn evict(&mut self) {
    while (self.config.max_items > 0 && self.lru.len() > self.config.max_items)
      || (self.config.max_bytes > 0 && self.current_bytes > self.config.max_bytes)
    {
      if let Some((_key, value)) = self.lru.pop_lru() {
        let removed_weight: usize = value.data().len();
        self.current_bytes = self.current_bytes.saturating_sub(removed_weight);
      } else {
        break;
      }
    }
  }
}

impl Default for BlurCache {
  fn default() -> Self {
    Self::new(FilterCacheConfig::from_env())
  }
}

#[inline]
fn blur_deadline_exceeded(counter: &mut usize) -> bool {
  check_active_periodic(counter, BLUR_DEADLINE_STRIDE, RenderStage::Paint).is_err()
}

#[inline]
fn blur_should_parallelize(width: usize, height: usize) -> bool {
  if rayon::current_num_threads() <= 1 {
    return false;
  }
  width.checked_mul(height).unwrap_or(usize::MAX) >= PARALLEL_BLUR_MIN_PIXELS
}

#[inline]
fn blur_deadline_exceeded_parallel(deadline_enabled: bool, counter: &AtomicUsize) -> bool {
  if !deadline_enabled {
    return false;
  }
  let next = counter.fetch_add(1, Ordering::Relaxed).wrapping_add(1);
  if next % BLUR_DEADLINE_STRIDE == 0 {
    check_active(RenderStage::Paint).is_err()
  } else {
    false
  }
}

fn record_blur_cache_hit() {
  with_paint_diagnostics(|diag| diag.blur_cache_hits += 1);
}

fn record_blur_cache_miss() {
  with_paint_diagnostics(|diag| diag.blur_cache_misses += 1);
}

fn record_blur_tiles(count: usize) {
  if count > 0 {
    with_paint_diagnostics(|diag| diag.blur_tiles += count);
  }
}

pub(crate) fn pixel_fingerprint(data: &[u8]) -> u64 {
  const PRIME: u64 = 0x9E37_79B1_85EB_CA87;
  let mut hash = (data.len() as u64).wrapping_mul(PRIME);
  let mut chunks = data.chunks_exact(8);
  for chunk in &mut chunks {
    let mut buf = [0u8; 8];
    buf.copy_from_slice(chunk);
    hash ^= u64::from_le_bytes(buf).wrapping_mul(PRIME).rotate_left(7);
  }
  let rem = chunks.remainder();
  if !rem.is_empty() {
    let mut buf = [0u8; 8];
    buf[..rem.len()].copy_from_slice(rem);
    hash ^= u64::from_le_bytes(buf).wrapping_mul(PRIME).rotate_left(11);
  }
  hash
}

pub(crate) fn alpha_bounds(pixmap: &Pixmap) -> Option<(u32, u32, u32, u32)> {
  let width = pixmap.width() as usize;
  let height = pixmap.height() as usize;
  if width == 0 || height == 0 {
    return None;
  }
  let mut min_x = width;
  let mut min_y = height;
  let mut max_x = 0usize;
  let mut max_y = 0usize;
  let data = pixmap.data();
  for y in 0..height {
    let row = &data[y * width * 4..(y + 1) * width * 4];
    for x in 0..width {
      if row[x * 4 + 3] != 0 {
        min_x = min_x.min(x);
        min_y = min_y.min(y);
        max_x = max_x.max(x);
        max_y = max_y.max(y);
      }
    }
  }
  if min_x > max_x || min_y > max_y {
    return None;
  }
  Some((
    min_x as u32,
    min_y as u32,
    (max_x - min_x + 1) as u32,
    (max_y - min_y + 1) as u32,
  ))
}

#[allow(dead_code)]
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

fn gaussian_convolve_premultiplied(pixmap: &mut Pixmap, sigma: f32) -> Result<bool, RenderError> {
  gaussian_convolve_premultiplied_with_parallelism(pixmap, sigma, BlurParallelism::Auto)
}

fn gaussian_convolve_premultiplied_with_parallelism(
  pixmap: &mut Pixmap,
  sigma: f32,
  parallelism: BlurParallelism,
) -> Result<bool, RenderError> {
  let (kernel, radius, scale) = gaussian_kernel_fixed(sigma);
  if radius == 0 || kernel.is_empty() || scale <= 0 {
    return Ok(false);
  }

  let width = pixmap.width() as usize;
  let height = pixmap.height() as usize;
  if width == 0 || height == 0 {
    return Ok(false);
  }

  let use_parallel = match parallelism {
    BlurParallelism::Serial => false,
    BlurParallelism::Auto => blur_should_parallelize(width, height),
  };

  let len = blur_buffer_len(width, height, "gaussian blur")?;
  let mut error: Option<RenderError> = None;
  let mut skipped = false;
  BLUR_SCRATCH.with(|scratch| {
    let mut scratch = scratch.borrow_mut();
    let (src, buf) = match scratch.split(len, "gaussian blur") {
      Ok(parts) => parts,
      Err(err) => {
        error = Some(err);
        return;
      }
    };
    src[..len].copy_from_slice(pixmap.data());

    let row_stride = width * 4;
    let src = &src[..len];
    let buf = &mut buf[..len];

    if !use_parallel {
      let mut deadline_counter = 0usize;

      // Horizontal pass: src -> buf
      for y in 0..height {
        if blur_deadline_exceeded(&mut deadline_counter) {
          skipped = true;
          pixmap.data_mut().copy_from_slice(src);
          return;
        }
        let row_start = y * width * 4;
        for x in 0..width {
          let mut acc_r: i32 = 0;
          let mut acc_g: i32 = 0;
          let mut acc_b: i32 = 0;
          let mut acc_a: i32 = 0;
          for (i, &w) in kernel.iter().enumerate() {
            let offset = i as isize - radius as isize;
            let cx = (x as isize + offset).clamp(0, width as isize - 1) as usize;
            let idx = row_start + cx * 4;
            acc_r += w * src[idx] as i32;
            acc_g += w * src[idx + 1] as i32;
            acc_b += w * src[idx + 2] as i32;
            acc_a += w * src[idx + 3] as i32;
          }
          let out_idx = row_start + x * 4;
          let a = ((acc_a + scale / 2) / scale).clamp(0, 255);
          let r = (acc_r + scale / 2) / scale;
          let g = (acc_g + scale / 2) / scale;
          let b = (acc_b + scale / 2) / scale;
          buf[out_idx] = clamp_channel_to_alpha(r, a);
          buf[out_idx + 1] = clamp_channel_to_alpha(g, a);
          buf[out_idx + 2] = clamp_channel_to_alpha(b, a);
          buf[out_idx + 3] = a as u8;
        }
      }

      // Vertical pass: buf -> pixmap data
      let dst = pixmap.data_mut();
      for y in 0..height {
        if blur_deadline_exceeded(&mut deadline_counter) {
          skipped = true;
          dst.copy_from_slice(src);
          return;
        }
        for x in 0..width {
          let mut acc_r: i32 = 0;
          let mut acc_g: i32 = 0;
          let mut acc_b: i32 = 0;
          let mut acc_a: i32 = 0;
          for (i, &w) in kernel.iter().enumerate() {
            let offset = i as isize - radius as isize;
            let cy = (y as isize + offset).clamp(0, height as isize - 1) as usize;
            let idx = (cy * width + x) * 4;
            acc_r += w * buf[idx] as i32;
            acc_g += w * buf[idx + 1] as i32;
            acc_b += w * buf[idx + 2] as i32;
            acc_a += w * buf[idx + 3] as i32;
          }
          let out_idx = (y * width + x) * 4;
          let a = ((acc_a + scale / 2) / scale).clamp(0, 255);
          let r = (acc_r + scale / 2) / scale;
          let g = (acc_g + scale / 2) / scale;
          let b = (acc_b + scale / 2) / scale;
          dst[out_idx] = clamp_channel_to_alpha(r, a);
          dst[out_idx + 1] = clamp_channel_to_alpha(g, a);
          dst[out_idx + 2] = clamp_channel_to_alpha(b, a);
          dst[out_idx + 3] = a as u8;
        }
      }
    } else {
      let deadline = active_deadline();
      let deadline_enabled = deadline.as_ref().map_or(false, |d| d.is_enabled());
      let cancelled = AtomicBool::new(false);
      let deadline_counter = AtomicUsize::new(0);

      let convolve_row_x = |y: usize, out_row: &mut [u8]| {
        if cancelled.load(Ordering::Relaxed) {
          return;
        }
        if blur_deadline_exceeded_parallel(deadline_enabled, &deadline_counter) {
          cancelled.store(true, Ordering::Relaxed);
          return;
        }
        let src_row = &src[y * row_stride..(y + 1) * row_stride];
        for x in 0..width {
          let mut acc_r: i32 = 0;
          let mut acc_g: i32 = 0;
          let mut acc_b: i32 = 0;
          let mut acc_a: i32 = 0;
          for (i, &w) in kernel.iter().enumerate() {
            let offset = i as isize - radius as isize;
            let cx = (x as isize + offset).clamp(0, width as isize - 1) as usize;
            let idx = cx * 4;
            acc_r += w * src_row[idx] as i32;
            acc_g += w * src_row[idx + 1] as i32;
            acc_b += w * src_row[idx + 2] as i32;
            acc_a += w * src_row[idx + 3] as i32;
          }
          let out_idx = x * 4;
          let a = ((acc_a + scale / 2) / scale).clamp(0, 255);
          let r = (acc_r + scale / 2) / scale;
          let g = (acc_g + scale / 2) / scale;
          let b = (acc_b + scale / 2) / scale;
          out_row[out_idx] = clamp_channel_to_alpha(r, a);
          out_row[out_idx + 1] = clamp_channel_to_alpha(g, a);
          out_row[out_idx + 2] = clamp_channel_to_alpha(b, a);
          out_row[out_idx + 3] = a as u8;
        }
      };

      if deadline_enabled {
        buf
          .par_chunks_mut(row_stride)
          .enumerate()
          .for_each_init(
            || DeadlineGuard::install(deadline.as_ref()),
            |_, (y, out_row)| convolve_row_x(y, out_row),
          );
      } else {
        buf
          .par_chunks_mut(row_stride)
          .enumerate()
          .for_each(|(y, out_row)| convolve_row_x(y, out_row));
      }

      if cancelled.load(Ordering::Relaxed) {
        skipped = true;
        pixmap.data_mut().copy_from_slice(src);
        return;
      }

      let buf = &*buf;
      let dst = pixmap.data_mut();
      let convolve_row_y = |y: usize, out_row: &mut [u8]| {
        if cancelled.load(Ordering::Relaxed) {
          return;
        }
        if blur_deadline_exceeded_parallel(deadline_enabled, &deadline_counter) {
          cancelled.store(true, Ordering::Relaxed);
          return;
        }
        for x in 0..width {
          let mut acc_r: i32 = 0;
          let mut acc_g: i32 = 0;
          let mut acc_b: i32 = 0;
          let mut acc_a: i32 = 0;
          for (i, &w) in kernel.iter().enumerate() {
            let offset = i as isize - radius as isize;
            let cy = (y as isize + offset).clamp(0, height as isize - 1) as usize;
            let idx = cy * row_stride + x * 4;
            acc_r += w * buf[idx] as i32;
            acc_g += w * buf[idx + 1] as i32;
            acc_b += w * buf[idx + 2] as i32;
            acc_a += w * buf[idx + 3] as i32;
          }
          let out_idx = x * 4;
          let a = ((acc_a + scale / 2) / scale).clamp(0, 255);
          let r = (acc_r + scale / 2) / scale;
          let g = (acc_g + scale / 2) / scale;
          let b = (acc_b + scale / 2) / scale;
          out_row[out_idx] = clamp_channel_to_alpha(r, a);
          out_row[out_idx + 1] = clamp_channel_to_alpha(g, a);
          out_row[out_idx + 2] = clamp_channel_to_alpha(b, a);
          out_row[out_idx + 3] = a as u8;
        }
      };

      if deadline_enabled {
        dst
          .par_chunks_mut(row_stride)
          .enumerate()
          .for_each_init(
            || DeadlineGuard::install(deadline.as_ref()),
            |_, (y, out_row)| convolve_row_y(y, out_row),
          );
      } else {
        dst
          .par_chunks_mut(row_stride)
          .enumerate()
          .for_each(|(y, out_row)| convolve_row_y(y, out_row));
      }

      if cancelled.load(Ordering::Relaxed) {
        skipped = true;
        dst.copy_from_slice(src);
      }
    }
  });
  if let Some(err) = error {
    Err(err)
  } else {
    Ok(!skipped)
  }
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

fn box_blur_h(
  src: &[u8],
  dst: &mut [u8],
  width: usize,
  height: usize,
  radius: usize,
  deadline_counter: &mut usize,
) -> bool {
  if radius == 0 {
    dst.copy_from_slice(src);
    return false;
  }
  let window = (radius * 2 + 1) as i32;
  let half = window / 2;
  for y in 0..height {
    if blur_deadline_exceeded(deadline_counter) {
      return true;
    }
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
  false
}

fn box_blur_h_parallel(
  src: &[u8],
  dst: &mut [u8],
  width: usize,
  _height: usize,
  radius: usize,
  deadline: &Option<crate::render_control::RenderDeadline>,
  deadline_enabled: bool,
  cancelled: &AtomicBool,
  deadline_counter: &AtomicUsize,
) -> bool {
  if radius == 0 {
    dst.copy_from_slice(src);
    return false;
  }
  let window = (radius * 2 + 1) as i32;
  let half = window / 2;
  let row_stride = width * 4;

  let blur_row = |y: usize, out_row: &mut [u8]| {
    if cancelled.load(Ordering::Relaxed) {
      return;
    }
    if blur_deadline_exceeded_parallel(deadline_enabled, deadline_counter) {
      cancelled.store(true, Ordering::Relaxed);
      return;
    }

    let src_row = &src[y * row_stride..(y + 1) * row_stride];
    let mut sum_r: i32 = 0;
    let mut sum_g: i32 = 0;
    let mut sum_b: i32 = 0;
    let mut sum_a: i32 = 0;
    for dx in -(radius as isize)..=(radius as isize) {
      let cx = dx.clamp(0, width as isize - 1) as usize;
      let idx = cx * 4;
      sum_r += src_row[idx] as i32;
      sum_g += src_row[idx + 1] as i32;
      sum_b += src_row[idx + 2] as i32;
      sum_a += src_row[idx + 3] as i32;
    }

    for x in 0..width {
      let out_idx = x * 4;
      let a = ((sum_a + half) / window).clamp(0, 255);
      let r = (sum_r + half) / window;
      let g = (sum_g + half) / window;
      let b = (sum_b + half) / window;
      out_row[out_idx] = clamp_channel_to_alpha(r, a);
      out_row[out_idx + 1] = clamp_channel_to_alpha(g, a);
      out_row[out_idx + 2] = clamp_channel_to_alpha(b, a);
      out_row[out_idx + 3] = a as u8;

      let remove_x = (x as isize - radius as isize).clamp(0, width as isize - 1) as usize;
      let add_x = (x as isize + radius as isize + 1).clamp(0, width as isize - 1) as usize;
      let rem_idx = remove_x * 4;
      let add_idx = add_x * 4;
      sum_r += src_row[add_idx] as i32 - src_row[rem_idx] as i32;
      sum_g += src_row[add_idx + 1] as i32 - src_row[rem_idx + 1] as i32;
      sum_b += src_row[add_idx + 2] as i32 - src_row[rem_idx + 2] as i32;
      sum_a += src_row[add_idx + 3] as i32 - src_row[rem_idx + 3] as i32;
    }
  };

  if deadline_enabled {
    dst
      .par_chunks_mut(row_stride)
      .enumerate()
      .for_each_init(
        || DeadlineGuard::install(deadline.as_ref()),
        |_, (y, out_row)| blur_row(y, out_row),
      );
  } else {
    dst
      .par_chunks_mut(row_stride)
      .enumerate()
      .for_each(|(y, out_row)| blur_row(y, out_row));
  }

  cancelled.load(Ordering::Relaxed)
}

fn box_blur_v(
  src: &[u8],
  dst: &mut [u8],
  width: usize,
  height: usize,
  radius: usize,
  deadline_counter: &mut usize,
) -> bool {
  if radius == 0 {
    dst.copy_from_slice(src);
    return false;
  }
  let window = (radius * 2 + 1) as i32;
  let half = window / 2;
  for x in 0..width {
    if blur_deadline_exceeded(deadline_counter) {
      return true;
    }
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
  false
}

fn box_blur_v_parallel(
  src: &[u8],
  dst: &mut [u8],
  width: usize,
  height: usize,
  radius: usize,
  deadline: &Option<crate::render_control::RenderDeadline>,
  deadline_enabled: bool,
  cancelled: &AtomicBool,
  deadline_counter: &AtomicUsize,
) -> bool {
  if radius == 0 {
    dst.copy_from_slice(src);
    return false;
  }
  const COLUMN_BLOCK: usize = 32;
  let window = (radius * 2 + 1) as i32;
  let half = window / 2;
  let blocks = (width + COLUMN_BLOCK - 1) / COLUMN_BLOCK;
  let dst_base = dst.as_mut_ptr() as usize;

  let blur_block = |block: usize| {
    let x_start = block * COLUMN_BLOCK;
    let x_end = (x_start + COLUMN_BLOCK).min(width);
    let dst_ptr = dst_base as *mut u8;

    for x in x_start..x_end {
      if cancelled.load(Ordering::Relaxed) {
        return;
      }
      if blur_deadline_exceeded_parallel(deadline_enabled, deadline_counter) {
        cancelled.store(true, Ordering::Relaxed);
        return;
      }

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
        unsafe {
          let out = dst_ptr.add(out_idx);
          *out = clamp_channel_to_alpha(r, a);
          *out.add(1) = clamp_channel_to_alpha(g, a);
          *out.add(2) = clamp_channel_to_alpha(b, a);
          *out.add(3) = a as u8;
        }

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
  };

  if deadline_enabled {
    (0..blocks).into_par_iter().for_each_init(
      || DeadlineGuard::install(deadline.as_ref()),
      |_, block| blur_block(block),
    );
  } else {
    (0..blocks).into_par_iter().for_each(|block| blur_block(block));
  }

  cancelled.load(Ordering::Relaxed)
}

fn gaussian_blur_box_approx(pixmap: &mut Pixmap, sigma: f32) -> Result<bool, RenderError> {
  gaussian_blur_box_approx_with_parallelism(pixmap, sigma, BlurParallelism::Auto)
}

fn gaussian_blur_box_approx_with_parallelism(
  pixmap: &mut Pixmap,
  sigma: f32,
  parallelism: BlurParallelism,
) -> Result<bool, RenderError> {
  let sigma = sigma.abs();
  if sigma <= 0.0 {
    return Ok(false);
  }
  let width = pixmap.width() as usize;
  let height = pixmap.height() as usize;
  if width == 0 || height == 0 {
    return Ok(false);
  }

  let use_parallel = match parallelism {
    BlurParallelism::Serial => false,
    BlurParallelism::Auto => blur_should_parallelize(width, height),
  };

  let len = blur_buffer_len(width, height, "box blur")?;
  let mut error: Option<RenderError> = None;
  let mut skipped = false;
  BLUR_SCRATCH.with(|scratch| {
    let mut scratch = scratch.borrow_mut();
    let (a, b) = match scratch.split(len, "box blur") {
      Ok(parts) => parts,
      Err(err) => {
        error = Some(err);
        return;
      }
    };
    a[..len].copy_from_slice(pixmap.data());
    let sizes = box_sizes_for_gauss(sigma, 3);
    if !use_parallel {
      let mut deadline_counter = 0usize;
      for size in sizes {
        let radius = size.saturating_sub(1) / 2;
        if radius == 0 {
          continue;
        }
        if box_blur_h(
          &a[..len],
          &mut b[..len],
          width,
          height,
          radius,
          &mut deadline_counter,
        ) {
          skipped = true;
          return;
        }
        if box_blur_v(
          &b[..len],
          &mut a[..len],
          width,
          height,
          radius,
          &mut deadline_counter,
        ) {
          skipped = true;
          return;
        }
      }
      pixmap.data_mut().copy_from_slice(&a[..len]);
      return;
    }

    let deadline = active_deadline();
    let deadline_enabled = deadline.as_ref().map_or(false, |d| d.is_enabled());
    let cancelled = AtomicBool::new(false);
    let deadline_counter = AtomicUsize::new(0);
    for size in sizes {
      let radius = size.saturating_sub(1) / 2;
      if radius == 0 {
        continue;
      }
      if box_blur_h_parallel(
        &a[..len],
        &mut b[..len],
        width,
        height,
        radius,
        &deadline,
        deadline_enabled,
        &cancelled,
        &deadline_counter,
      ) {
        skipped = true;
        return;
      }
      if box_blur_v_parallel(
        &b[..len],
        &mut a[..len],
        width,
        height,
        radius,
        &deadline,
        deadline_enabled,
        &cancelled,
        &deadline_counter,
      ) {
        skipped = true;
        return;
      }
    }

    if cancelled.load(Ordering::Relaxed) {
      skipped = true;
      return;
    }

    pixmap.data_mut().copy_from_slice(&a[..len]);
  });
  if let Some(err) = error {
    Err(err)
  } else {
    Ok(!skipped)
  }
}

fn blur_isotropic_body(pixmap: &mut Pixmap, sigma: f32) -> Result<bool, RenderError> {
  let sigma = sigma.abs();
  let radius = (sigma * 3.0).ceil() as usize;
  if radius == 0 {
    return Ok(false);
  }

  let fast_enabled = runtime::runtime_toggles().truthy("FASTR_FAST_BLUR");
  if fast_enabled || sigma <= FAST_GAUSS_THRESHOLD_SIGMA {
    gaussian_convolve_premultiplied(pixmap, sigma)
  } else {
    gaussian_blur_box_approx(pixmap, sigma)
  }
}

fn blur_anisotropic_body(
  pixmap: &mut Pixmap,
  sigma_x: f32,
  sigma_y: f32,
) -> Result<bool, RenderError> {
  blur_anisotropic_body_with_parallelism(pixmap, sigma_x, sigma_y, BlurParallelism::Auto)
}

fn blur_anisotropic_body_with_parallelism(
  pixmap: &mut Pixmap,
  sigma_x: f32,
  sigma_y: f32,
  parallelism: BlurParallelism,
) -> Result<bool, RenderError> {
  let sigma_x = sigma_x.abs();
  let sigma_y = sigma_y.abs();
  if sigma_x == sigma_y {
    return blur_isotropic_body(pixmap, sigma_x);
  }

  if sigma_x == 0.0 && sigma_y == 0.0 {
    return Ok(false);
  }

  let width = pixmap.width() as usize;
  let height = pixmap.height() as usize;
  if width == 0 || height == 0 {
    return Ok(false);
  }

  let (kernel_x, radius_x, scale_x) = gaussian_kernel_fixed(sigma_x);
  let (kernel_y, radius_y, scale_y) = gaussian_kernel_fixed(sigma_y);
  if (radius_x == 0 || kernel_x.is_empty()) && (radius_y == 0 || kernel_y.is_empty()) {
    return Ok(false);
  }

  let use_parallel = match parallelism {
    BlurParallelism::Serial => false,
    BlurParallelism::Auto => blur_should_parallelize(width, height),
  };

  let len = blur_buffer_len(width, height, "anisotropic blur")?;
  let mut error: Option<RenderError> = None;
  let mut skipped = false;
  BLUR_SCRATCH.with(|scratch| {
    let mut scratch = scratch.borrow_mut();
    let (src, tmp) = match scratch.split(len, "anisotropic blur") {
      Ok(parts) => parts,
      Err(err) => {
        error = Some(err);
        return;
      }
    };
    src[..len].copy_from_slice(pixmap.data());

    let row_stride = width * 4;
    let src = &src[..len];
    let tmp = &mut tmp[..len];

    if !use_parallel {
      let mut deadline_counter = 0usize;

      // Horizontal pass (if needed): src -> tmp, otherwise copy
      if radius_x == 0 || kernel_x.is_empty() {
        tmp[..len].copy_from_slice(src);
      } else {
        for y in 0..height {
          if blur_deadline_exceeded(&mut deadline_counter) {
            skipped = true;
            pixmap.data_mut().copy_from_slice(src);
            return;
          }
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
            tmp[out_idx] = clamp_channel_to_alpha(r, a);
            tmp[out_idx + 1] = clamp_channel_to_alpha(g, a);
            tmp[out_idx + 2] = clamp_channel_to_alpha(b, a);
            tmp[out_idx + 3] = a as u8;
          }
        }
      }

      // Vertical pass (if needed): tmp/src -> pixmap
      let src_for_y = if radius_x == 0 || kernel_x.is_empty() {
        src
      } else {
        &tmp[..len]
      };
      let dst = pixmap.data_mut();
      if radius_y == 0 || kernel_y.is_empty() {
        dst.copy_from_slice(src_for_y);
        return;
      }
      for y in 0..height {
        if blur_deadline_exceeded(&mut deadline_counter) {
          skipped = true;
          dst.copy_from_slice(src);
          return;
        }
        for x in 0..width {
          let mut acc_r: i32 = 0;
          let mut acc_g: i32 = 0;
          let mut acc_b: i32 = 0;
          let mut acc_a: i32 = 0;
          for (i, &w) in kernel_y.iter().enumerate() {
            let offset = i as isize - radius_y as isize;
            let cy = (y as isize + offset).clamp(0, height as isize - 1) as usize;
            let idx = (cy * width + x) * 4;
            acc_r += w * src_for_y[idx] as i32;
            acc_g += w * src_for_y[idx + 1] as i32;
            acc_b += w * src_for_y[idx + 2] as i32;
            acc_a += w * src_for_y[idx + 3] as i32;
          }
          let out_idx = (y * width + x) * 4;
          let a = ((acc_a + scale_y / 2) / scale_y).clamp(0, 255);
          let r = (acc_r + scale_y / 2) / scale_y;
          let g = (acc_g + scale_y / 2) / scale_y;
          let b = (acc_b + scale_y / 2) / scale_y;
          dst[out_idx] = clamp_channel_to_alpha(r, a);
          dst[out_idx + 1] = clamp_channel_to_alpha(g, a);
          dst[out_idx + 2] = clamp_channel_to_alpha(b, a);
          dst[out_idx + 3] = a as u8;
        }
      }
    } else {
      let deadline = active_deadline();
      let deadline_enabled = deadline.as_ref().map_or(false, |d| d.is_enabled());
      let cancelled = AtomicBool::new(false);
      let deadline_counter = AtomicUsize::new(0);

      // Horizontal pass (if needed): src -> tmp, otherwise copy
      if radius_x == 0 || kernel_x.is_empty() {
        tmp.copy_from_slice(src);
      } else {
        let convolve_row_x = |y: usize, out_row: &mut [u8]| {
          if cancelled.load(Ordering::Relaxed) {
            return;
          }
          if blur_deadline_exceeded_parallel(deadline_enabled, &deadline_counter) {
            cancelled.store(true, Ordering::Relaxed);
            return;
          }
          let src_row = &src[y * row_stride..(y + 1) * row_stride];
          for x in 0..width {
            let mut acc_r: i32 = 0;
            let mut acc_g: i32 = 0;
            let mut acc_b: i32 = 0;
            let mut acc_a: i32 = 0;
            for (i, &w) in kernel_x.iter().enumerate() {
              let offset = i as isize - radius_x as isize;
              let cx = (x as isize + offset).clamp(0, width as isize - 1) as usize;
              let idx = cx * 4;
              acc_r += w * src_row[idx] as i32;
              acc_g += w * src_row[idx + 1] as i32;
              acc_b += w * src_row[idx + 2] as i32;
              acc_a += w * src_row[idx + 3] as i32;
            }
            let out_idx = x * 4;
            let a = ((acc_a + scale_x / 2) / scale_x).clamp(0, 255);
            let r = (acc_r + scale_x / 2) / scale_x;
            let g = (acc_g + scale_x / 2) / scale_x;
            let b = (acc_b + scale_x / 2) / scale_x;
            out_row[out_idx] = clamp_channel_to_alpha(r, a);
            out_row[out_idx + 1] = clamp_channel_to_alpha(g, a);
            out_row[out_idx + 2] = clamp_channel_to_alpha(b, a);
            out_row[out_idx + 3] = a as u8;
          }
        };

        if deadline_enabled {
          tmp
            .par_chunks_mut(row_stride)
            .enumerate()
            .for_each_init(
              || DeadlineGuard::install(deadline.as_ref()),
              |_, (y, out_row)| convolve_row_x(y, out_row),
            );
        } else {
          tmp
            .par_chunks_mut(row_stride)
            .enumerate()
            .for_each(|(y, out_row)| convolve_row_x(y, out_row));
        }

        if cancelled.load(Ordering::Relaxed) {
          skipped = true;
          pixmap.data_mut().copy_from_slice(src);
          return;
        }
      }

      // Vertical pass (if needed): tmp/src -> pixmap
      let src_for_y: &[u8] = if radius_x == 0 || kernel_x.is_empty() {
        src
      } else {
        &*tmp
      };
      let dst = pixmap.data_mut();
      if radius_y == 0 || kernel_y.is_empty() {
        dst.copy_from_slice(src_for_y);
        return;
      }

      let convolve_row_y = |y: usize, out_row: &mut [u8]| {
        if cancelled.load(Ordering::Relaxed) {
          return;
        }
        if blur_deadline_exceeded_parallel(deadline_enabled, &deadline_counter) {
          cancelled.store(true, Ordering::Relaxed);
          return;
        }
        for x in 0..width {
          let mut acc_r: i32 = 0;
          let mut acc_g: i32 = 0;
          let mut acc_b: i32 = 0;
          let mut acc_a: i32 = 0;
          for (i, &w) in kernel_y.iter().enumerate() {
            let offset = i as isize - radius_y as isize;
            let cy = (y as isize + offset).clamp(0, height as isize - 1) as usize;
            let idx = cy * row_stride + x * 4;
            acc_r += w * src_for_y[idx] as i32;
            acc_g += w * src_for_y[idx + 1] as i32;
            acc_b += w * src_for_y[idx + 2] as i32;
            acc_a += w * src_for_y[idx + 3] as i32;
          }
          let out_idx = x * 4;
          let a = ((acc_a + scale_y / 2) / scale_y).clamp(0, 255);
          let r = (acc_r + scale_y / 2) / scale_y;
          let g = (acc_g + scale_y / 2) / scale_y;
          let b = (acc_b + scale_y / 2) / scale_y;
          out_row[out_idx] = clamp_channel_to_alpha(r, a);
          out_row[out_idx + 1] = clamp_channel_to_alpha(g, a);
          out_row[out_idx + 2] = clamp_channel_to_alpha(b, a);
          out_row[out_idx + 3] = a as u8;
        }
      };

      if deadline_enabled {
        dst
          .par_chunks_mut(row_stride)
          .enumerate()
          .for_each_init(
            || DeadlineGuard::install(deadline.as_ref()),
            |_, (y, out_row)| convolve_row_y(y, out_row),
          );
      } else {
        dst
          .par_chunks_mut(row_stride)
          .enumerate()
          .for_each(|(y, out_row)| convolve_row_y(y, out_row));
      }

      if cancelled.load(Ordering::Relaxed) {
        skipped = true;
        dst.copy_from_slice(src);
      }
    }
  });
  if let Some(err) = error {
    Err(err)
  } else {
    Ok(!skipped)
  }
}

fn tile_blur(
  pixmap: &Pixmap,
  sigma_x: f32,
  sigma_y: f32,
  config: &FilterCacheConfig,
) -> Result<Option<(Pixmap, usize)>, RenderError> {
  if config.max_bytes == 0 {
    return Ok(None);
  }
  let data_len = pixmap.data().len();
  if data_len <= config.max_bytes {
    return Ok(None);
  }
  let width = pixmap.width();
  let height = pixmap.height();
  if width == 0 || height == 0 {
    return Ok(None);
  }

  let deadline = active_deadline();
  let deadline_enabled = deadline.as_ref().map_or(false, |d| d.is_enabled());
  if deadline_enabled && check_active(RenderStage::Paint).is_err() {
    let mut out = new_pixmap_with_context(width, height, "tile blur output")?;
    out.data_mut().copy_from_slice(pixmap.data());
    return Ok(Some((out, 0)));
  }

  let pad_x = (sigma_x.abs() * 3.0).ceil() as u32;
  let pad_y = (sigma_y.abs() * 3.0).ceil() as u32;
  let budget_pixels = config.max_bytes / 4;
  if budget_pixels == 0 {
    return Ok(None);
  }
  let base_span = (budget_pixels as f64).sqrt().floor() as u32;
  let tile_w = base_span.saturating_sub(pad_x.saturating_mul(2)).max(1);
  let tile_h = base_span.saturating_sub(pad_y.saturating_mul(2)).max(1);
  if tile_w >= width && tile_h >= height {
    return Ok(None);
  }

  #[derive(Clone, Copy, Debug)]
  struct TileJob {
    x: u32,
    y: u32,
    copy_w: u32,
    copy_h: u32,
    src_min_x: u32,
    src_min_y: u32,
    src_w: u32,
    src_h: u32,
    offset_x: u32,
    offset_y: u32,
  }

  let tiles_x = ((width as u64 + tile_w as u64 - 1) / tile_w as u64) as u32;
  let tiles_y = ((height as u64 + tile_h as u64 - 1) / tile_h as u64) as u32;
  let tiles = usize::try_from(tiles_x as u64 * tiles_y as u64).unwrap_or(usize::MAX);
  if tiles == 0 {
    return Ok(None);
  }

  let mut out = new_pixmap_with_context(width, height, "tile blur output")?;
  let source = pixmap.data();
  let row_stride = width as usize * 4;

  // When tiles are small enough that per-tile blur won't parallelize, distribute work across tiles
  // instead of paying nested rayon overhead inside each tile blur pass.
  let max_src_w = width.min(tile_w.saturating_add(pad_x.saturating_mul(2)));
  let max_src_h = height.min(tile_h.saturating_add(pad_y.saturating_mul(2)));
  let max_tile_pixels = (max_src_w as usize).saturating_mul(max_src_h as usize);
  let parallel_tiles = max_tile_pixels < PARALLEL_BLUR_MIN_PIXELS
    && tiles > 1
    && rayon::current_num_threads() > 1;

  if parallel_tiles {
    let out_stride = row_stride;
    let out_base = out.data_mut().as_mut_ptr() as usize;
    let cancelled = AtomicBool::new(false);
    let deadline_counter = AtomicUsize::new(0);
    let error: Mutex<Option<RenderError>> = Mutex::new(None);

    let blur_job = |job: TileJob| {
      if cancelled.load(Ordering::Relaxed) {
        return;
      }
      let out_ptr = out_base as *mut u8;

      if blur_deadline_exceeded_parallel(deadline_enabled, &deadline_counter) {
        cancelled.store(true, Ordering::Relaxed);
        return;
      }
      if deadline_enabled && check_active(RenderStage::Paint).is_err() {
        cancelled.store(true, Ordering::Relaxed);
        return;
      }

      let mut tile = match new_pixmap_with_context(job.src_w, job.src_h, "tile blur tile") {
        Ok(p) => p,
        Err(err) => {
          cancelled.store(true, Ordering::Relaxed);
          if let Ok(mut slot) = error.lock() {
            if slot.is_none() {
              *slot = Some(err);
            }
          }
          return;
        }
      };

      let tile_stride = job.src_w as usize * 4;
      for row in 0..job.src_h as usize {
        if cancelled.load(Ordering::Relaxed) {
          return;
        }
        if blur_deadline_exceeded_parallel(deadline_enabled, &deadline_counter) {
          cancelled.store(true, Ordering::Relaxed);
          return;
        }
        let src_start = (job.src_min_y as usize + row) * row_stride + job.src_min_x as usize * 4;
        let dst_start = row * tile_stride;
        tile.data_mut()[dst_start..dst_start + tile_stride]
          .copy_from_slice(&source[src_start..src_start + tile_stride]);
      }

      match blur_anisotropic_body_with_parallelism(&mut tile, sigma_x, sigma_y, BlurParallelism::Serial)
      {
        Ok(true) => {}
        Ok(false) => {
          cancelled.store(true, Ordering::Relaxed);
          return;
        }
        Err(err) => {
          cancelled.store(true, Ordering::Relaxed);
          if let Ok(mut slot) = error.lock() {
            if slot.is_none() {
              *slot = Some(err);
            }
          }
          return;
        }
      }

      if deadline_enabled && check_active(RenderStage::Paint).is_err() {
        cancelled.store(true, Ordering::Relaxed);
        return;
      }

      let offset_x = job.offset_x as usize;
      let offset_y = job.offset_y as usize;
      let copy_w_bytes = job.copy_w as usize * 4;
      let tile_data = tile.data();
      for row in 0..job.copy_h as usize {
        if cancelled.load(Ordering::Relaxed) {
          return;
        }
        if blur_deadline_exceeded_parallel(deadline_enabled, &deadline_counter) {
          cancelled.store(true, Ordering::Relaxed);
          return;
        }
        let src_start = (offset_y + row) * tile_stride + offset_x * 4;
        let dst_start = (job.y as usize + row) * out_stride + job.x as usize * 4;
        unsafe {
          std::ptr::copy_nonoverlapping(
            tile_data.as_ptr().add(src_start),
            out_ptr.add(dst_start),
            copy_w_bytes,
          );
        }
      }
    };

    let tiles_x_usize = tiles_x as usize;
    let total_tiles_u64 = tiles_x as u64 * tiles_y as u64;
    if total_tiles_u64 <= usize::MAX as u64 {
      let total_tiles = total_tiles_u64 as usize;
      let blur_tile = |idx: usize| {
        let ty = idx / tiles_x_usize;
        let tx = idx - ty * tiles_x_usize;
        let x = (tx as u32).saturating_mul(tile_w);
        let y = (ty as u32).saturating_mul(tile_h);
        if x >= width || y >= height {
          return;
        }
        let copy_w = tile_w.min(width - x);
        let copy_h = tile_h.min(height - y);
        let src_min_x = x.saturating_sub(pad_x);
        let src_min_y = y.saturating_sub(pad_y);
        let src_max_x = (x + copy_w + pad_x).min(width);
        let src_max_y = (y + copy_h + pad_y).min(height);
        let src_w = src_max_x.saturating_sub(src_min_x);
        let src_h = src_max_y.saturating_sub(src_min_y);
        blur_job(TileJob {
          x,
          y,
          copy_w,
          copy_h,
          src_min_x,
          src_min_y,
          src_w,
          src_h,
          offset_x: x - src_min_x,
          offset_y: y - src_min_y,
        });
      };

      if deadline_enabled {
        (0..total_tiles).into_par_iter().for_each_init(
          || DeadlineGuard::install(deadline.as_ref()),
          |_, idx| blur_tile(idx),
        );
      } else {
        (0..total_tiles).into_par_iter().for_each(|idx| blur_tile(idx));
      }
    } else if deadline_enabled {
      // Extremely high tile counts can overflow usize on 32-bit platforms; fall back to row-parallel
      // iteration rather than allocating a tile job list.
      (0..tiles_y as usize).into_par_iter().for_each_init(
        || DeadlineGuard::install(deadline.as_ref()),
        |_, ty| {
          let y = (ty as u32).saturating_mul(tile_h);
          if y >= height {
            return;
          }
          let copy_h = tile_h.min(height - y);
          for tx in 0..tiles_x as usize {
            let x = (tx as u32).saturating_mul(tile_w);
            if x >= width {
              break;
            }
            let copy_w = tile_w.min(width - x);
            let src_min_x = x.saturating_sub(pad_x);
            let src_min_y = y.saturating_sub(pad_y);
            let src_max_x = (x + copy_w + pad_x).min(width);
            let src_max_y = (y + copy_h + pad_y).min(height);
            let src_w = src_max_x.saturating_sub(src_min_x);
            let src_h = src_max_y.saturating_sub(src_min_y);
            blur_job(TileJob {
              x,
              y,
              copy_w,
              copy_h,
              src_min_x,
              src_min_y,
              src_w,
              src_h,
              offset_x: x - src_min_x,
              offset_y: y - src_min_y,
            });
          }
        },
      );
    } else {
      (0..tiles_y as usize).into_par_iter().for_each(|ty| {
        let y = (ty as u32).saturating_mul(tile_h);
        if y >= height {
          return;
        }
        let copy_h = tile_h.min(height - y);
        for tx in 0..tiles_x as usize {
          let x = (tx as u32).saturating_mul(tile_w);
          if x >= width {
            break;
          }
          let copy_w = tile_w.min(width - x);
          let src_min_x = x.saturating_sub(pad_x);
          let src_min_y = y.saturating_sub(pad_y);
          let src_max_x = (x + copy_w + pad_x).min(width);
          let src_max_y = (y + copy_h + pad_y).min(height);
          let src_w = src_max_x.saturating_sub(src_min_x);
          let src_h = src_max_y.saturating_sub(src_min_y);
          blur_job(TileJob {
            x,
            y,
            copy_w,
            copy_h,
            src_min_x,
            src_min_y,
            src_w,
            src_h,
            offset_x: x - src_min_x,
            offset_y: y - src_min_y,
          });
        }
      });
    }

    if let Ok(mut slot) = error.lock() {
      if let Some(err) = slot.take() {
        return Err(err);
      }
    }

    if cancelled.load(Ordering::Relaxed) {
      out.data_mut().copy_from_slice(source);
      return Ok(Some((out, 0)));
    }

    return Ok(Some((out, tiles)));
  }

  let mut deadline_counter = 0usize;
  let mut cancelled = false;
  for ty in 0..tiles_y {
    let y = ty.saturating_mul(tile_h);
    let copy_h = tile_h.min(height - y);
    for tx in 0..tiles_x {
      let x = tx.saturating_mul(tile_w);
      let copy_w = tile_w.min(width - x);
      let src_min_x = x.saturating_sub(pad_x);
      let src_min_y = y.saturating_sub(pad_y);
      let src_max_x = (x + copy_w + pad_x).min(width);
      let src_max_y = (y + copy_h + pad_y).min(height);
      let src_w = src_max_x.saturating_sub(src_min_x);
      let src_h = src_max_y.saturating_sub(src_min_y);
      let offset_x = (x - src_min_x) as usize;
      let offset_y = (y - src_min_y) as usize;

      if blur_deadline_exceeded(&mut deadline_counter) {
        cancelled = true;
        break;
      }

      let mut tile = new_pixmap_with_context(src_w, src_h, "tile blur tile")?;
      let tile_stride = src_w as usize * 4;
      for row in 0..src_h as usize {
        if blur_deadline_exceeded(&mut deadline_counter) {
          cancelled = true;
          break;
        }
        let src_start = (src_min_y as usize + row) * row_stride + src_min_x as usize * 4;
        let dst_start = row * tile_stride;
        tile.data_mut()[dst_start..dst_start + tile_stride]
          .copy_from_slice(&source[src_start..src_start + tile_stride]);
      }
      if cancelled {
        break;
      }

      let blurred = blur_anisotropic_body(&mut tile, sigma_x, sigma_y)?;
      if !blurred {
        cancelled = true;
        break;
      }

      // If the blur was cancelled mid-tile, skip the entire tiled blur and leave the pixmap unchanged.
      if deadline_enabled && check_active(RenderStage::Paint).is_err() {
        cancelled = true;
        break;
      }

      let copy_w_bytes = copy_w as usize * 4;
      let tile_data = tile.data();
      {
        let out_data = out.data_mut();
        for row in 0..copy_h as usize {
          if blur_deadline_exceeded(&mut deadline_counter) {
            cancelled = true;
            break;
          }
          let src_start = (offset_y + row) * tile_stride + offset_x * 4;
          let dst_start = (y as usize + row) * row_stride + x as usize * 4;
          out_data[dst_start..dst_start + copy_w_bytes]
            .copy_from_slice(&tile_data[src_start..src_start + copy_w_bytes]);
        }
      }
      if cancelled {
        break;
      }
    }
    if cancelled {
      break;
    }
  }

  if cancelled {
    out.data_mut().copy_from_slice(source);
    return Ok(Some((out, 0)));
  }

  Ok(Some((out, tiles)))
}

fn apply_blur_internal(
  pixmap: &mut Pixmap,
  sigma_x: f32,
  sigma_y: f32,
  mut cache: Option<&mut BlurCache>,
  scale: f32,
) -> Result<(), RenderError> {
  let sigma_x = sigma_x.abs();
  let sigma_y = sigma_y.abs();
  if (sigma_x == 0.0 && sigma_y == 0.0) || pixmap.width() == 0 || pixmap.height() == 0 {
    return Ok(());
  }

  let cache_config = cache.as_ref().map(|cache| cache.config);
  let cache_key = match cache_config {
    Some(config) if config.max_items != 0 => {
      let weight = pixmap.data().len();
      if config.max_bytes > 0 && weight > config.max_bytes {
        None
      } else {
        BlurCacheKey::new(sigma_x, sigma_y, scale, pixmap)
      }
    }
    _ => None,
  };
  if let (Some(key), Some(cache)) = (cache_key.as_ref(), cache.as_deref_mut()) {
    if let Some(cached) = cache.get(key) {
      pixmap.data_mut().copy_from_slice(cached.data());
      return Ok(());
    }
  }

  let config = cache_config.unwrap_or_else(FilterCacheConfig::from_env);

  let (tile_count, blur_applied) =
    if let Some((tiled, tiles)) = tile_blur(pixmap, sigma_x, sigma_y, &config)? {
      *pixmap = tiled;
      (tiles, tiles > 0)
    } else if sigma_x == sigma_y {
      (0, blur_isotropic_body(pixmap, sigma_x)?)
    } else {
      (0, blur_anisotropic_body(pixmap, sigma_x, sigma_y)?)
    };

  record_blur_tiles(tile_count);

  if blur_applied {
    if let (Some(key), Some(cache)) = (cache_key, cache) {
      cache.put(key, pixmap);
    }
  }

  Ok(())
}

pub fn apply_gaussian_blur(pixmap: &mut Pixmap, sigma: f32) -> Result<(), RenderError> {
  apply_blur_internal(pixmap, sigma, sigma, None, 1.0)
}

pub fn apply_gaussian_blur_anisotropic(
  pixmap: &mut Pixmap,
  sigma_x: f32,
  sigma_y: f32,
) -> Result<(), RenderError> {
  apply_blur_internal(pixmap, sigma_x, sigma_y, None, 1.0)
}

pub(crate) fn apply_gaussian_blur_cached(
  pixmap: &mut Pixmap,
  sigma_x: f32,
  sigma_y: f32,
  cache: Option<&mut BlurCache>,
  scale: f32,
) -> Result<(), RenderError> {
  apply_blur_internal(pixmap, sigma_x, sigma_y, cache, scale)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::paint::painter::{enable_paint_diagnostics, take_paint_diagnostics};
  use crate::paint::pixmap::new_pixmap;
  use crate::render_control::{with_deadline, RenderDeadline};
  use rayon::ThreadPoolBuilder;
  use std::sync::Arc;
  use tiny_skia::PremultipliedColorU8;

  #[test]
  fn blur_with_zero_sigma_x_only_blurs_vertically() {
    let mut pixmap = new_pixmap(3, 5).unwrap();
    let idx = 2 * 3 + 1;
    pixmap.pixels_mut()[idx] = PremultipliedColorU8::from_rgba(255, 255, 255, 255).unwrap();

    apply_gaussian_blur_anisotropic(&mut pixmap, 0.0, 2.0).unwrap();

    let pixels = pixmap.pixels();
    let mut center_non_zero = 0;
    for (i, px) in pixels.iter().enumerate() {
      let x = i % 3;
      if x == 1 {
        if px.alpha() > 0 {
          center_non_zero += 1;
        }
      } else {
        assert_eq!(px.alpha(), 0, "blur spread horizontally to column {}", x);
      }
    }
    assert!(
      center_non_zero > 1,
      "expected vertical spread in center column"
    );
  }

  #[test]
  fn blur_cache_skips_lookup_when_pixmap_exceeds_max_bytes() {
    enable_paint_diagnostics();
    let mut cache = BlurCache::new(FilterCacheConfig {
      max_items: 8,
      max_bytes: 8 * 1024,
    });
    let mut pixmap = new_pixmap(64, 64).unwrap();
    let idx = 32 * 64 + 32;
    pixmap.pixels_mut()[idx] = PremultipliedColorU8::from_rgba(255, 255, 255, 255).unwrap();

    // The pixmap is larger than max_bytes, so caching is skipped entirely.
    apply_gaussian_blur_cached(&mut pixmap, 1.0, 1.0, Some(&mut cache), 1.0).unwrap();
    apply_gaussian_blur_cached(&mut pixmap, 1.0, 1.0, Some(&mut cache), 1.0).unwrap();

    let diag = take_paint_diagnostics().unwrap();
    assert_eq!(diag.blur_cache_hits, 0);
    assert_eq!(diag.blur_cache_misses, 0);
    assert!(
      pixmap.pixels().iter().any(|p| p.alpha() > 0),
      "expected blurred output to retain some alpha"
    );
  }

  #[test]
  fn blur_cache_skips_lookup_when_max_items_is_zero() {
    enable_paint_diagnostics();
    let mut cache = BlurCache::new(FilterCacheConfig {
      max_items: 0,
      max_bytes: 32 * 1024,
    });
    let mut pixmap = new_pixmap(32, 32).unwrap();
    let idx = 16 * 32 + 16;
    pixmap.pixels_mut()[idx] = PremultipliedColorU8::from_rgba(255, 255, 255, 255).unwrap();

    apply_gaussian_blur_cached(&mut pixmap, 1.0, 1.0, Some(&mut cache), 1.0).unwrap();
    apply_gaussian_blur_cached(&mut pixmap, 1.0, 1.0, Some(&mut cache), 1.0).unwrap();

    let diag = take_paint_diagnostics().unwrap();
    assert_eq!(diag.blur_cache_hits, 0);
    assert_eq!(diag.blur_cache_misses, 0);
    assert!(
      pixmap.pixels().iter().any(|p| p.alpha() > 0),
      "expected blurred output to retain some alpha"
    );
  }

  #[test]
  fn blur_cache_does_not_store_when_deadline_exceeded() {
    let mut cache = BlurCache::new(FilterCacheConfig {
      max_items: 8,
      max_bytes: 1024 * 1024,
    });
    let mut pixmap = new_pixmap(64, 2048).unwrap();
    for (i, px) in pixmap.pixels_mut().iter_mut().enumerate() {
      let a = (i % 256) as u8;
      *px = PremultipliedColorU8::from_rgba(a, a, a, a).unwrap();
    }
    let original = pixmap.data().to_vec();

    let cancel: Arc<crate::render_control::CancelCallback> = Arc::new(|| true);
    let deadline = RenderDeadline::new(None, Some(cancel));
    with_deadline(Some(&deadline), || {
      apply_gaussian_blur_cached(&mut pixmap, 3.0, 3.0, Some(&mut cache), 1.0).unwrap();
    });

    assert_eq!(pixmap.data(), original.as_slice());
    assert_eq!(
      cache.lru.len(),
      0,
      "expected blur cache to remain empty when blur is skipped due to deadline"
    );
  }

  #[test]
  fn tile_blur_respects_deadline_cancellation() {
    let mut cache = BlurCache::new(FilterCacheConfig {
      max_items: 0,
      max_bytes: 4 * 1024,
    });
    let mut pixmap = new_pixmap(64, 64).unwrap();
    for (i, px) in pixmap.pixels_mut().iter_mut().enumerate() {
      let a = (i % 256) as u8;
      *px = PremultipliedColorU8::from_rgba(a, a, a, a).unwrap();
    }
    let original = pixmap.data().to_vec();

    let cancel: Arc<crate::render_control::CancelCallback> = Arc::new(|| true);
    let deadline = RenderDeadline::new(None, Some(cancel));
    with_deadline(Some(&deadline), || {
      apply_gaussian_blur_cached(&mut pixmap, 3.0, 3.0, Some(&mut cache), 1.0).unwrap();
    });

    assert_eq!(
      pixmap.data(),
      original.as_slice(),
      "expected tiled blur to skip when deadline is cancelled"
    );
  }

  #[test]
  fn tile_blur_matches_direct_output() {
    let mut base = new_pixmap(32, 24).unwrap();
    let w = base.width() as usize;
    for y in 0..base.height() as usize {
      for x in 0..w {
        let a = ((x * 29 + y * 31) % 256) as u8;
        let r0 = ((x * 11 + y * 7) % 256) as u8;
        let g0 = ((x * 5 + y * 13) % 256) as u8;
        let b0 = ((x * 17 + y * 19) % 256) as u8;
        let premul = |c: u8| ((c as u16 * a as u16) / 255) as u8;
        base.pixels_mut()[y * w + x] =
          PremultipliedColorU8::from_rgba(premul(r0), premul(g0), premul(b0), a).unwrap();
      }
    }

    let mut direct = base.clone();
    apply_gaussian_blur(&mut direct, 3.0).unwrap();

    let mut tiled = base.clone();
    let mut cache = BlurCache::new(FilterCacheConfig {
      max_items: 0,
      max_bytes: 2048,
    });

    // Ensure the tile-parallel branch is exercised even when the global rayon pool is single-threaded.
    ThreadPoolBuilder::new()
      .num_threads(2)
      .build()
      .unwrap()
      .install(|| apply_gaussian_blur_cached(&mut tiled, 3.0, 3.0, Some(&mut cache), 1.0).unwrap());

    assert_eq!(
      direct.data(),
      tiled.data(),
      "tiled blur output mismatch"
    );
  }

  #[test]
  fn parallel_blur_matches_serial_output() {
    let mut base = new_pixmap(512, 512).unwrap();
    let w = base.width() as usize;
    for y in 0..base.height() as usize {
      for x in 0..w {
        let a = ((x * 29 + y * 31) % 256) as u8;
        let r0 = ((x * 11 + y * 7) % 256) as u8;
        let g0 = ((x * 5 + y * 13) % 256) as u8;
        let b0 = ((x * 17 + y * 19) % 256) as u8;
        let premul = |c: u8| ((c as u16 * a as u16) / 255) as u8;
        base.pixels_mut()[y * w + x] =
          PremultipliedColorU8::from_rgba(premul(r0), premul(g0), premul(b0), a).unwrap();
      }
    }

    let pool = ThreadPoolBuilder::new().num_threads(2).build().unwrap();

    let mut serial = base.clone();
    gaussian_convolve_premultiplied_with_parallelism(&mut serial, 3.0, BlurParallelism::Serial)
      .unwrap();
    let mut parallel = base.clone();
    pool.install(|| {
      gaussian_convolve_premultiplied_with_parallelism(&mut parallel, 3.0, BlurParallelism::Auto)
        .unwrap();
    });
    assert_eq!(
      serial.data(),
      parallel.data(),
      "isotropic gaussian output mismatch"
    );

    let mut serial = base.clone();
    gaussian_blur_box_approx_with_parallelism(&mut serial, 8.0, BlurParallelism::Serial).unwrap();
    let mut parallel = base.clone();
    pool.install(|| {
      gaussian_blur_box_approx_with_parallelism(&mut parallel, 8.0, BlurParallelism::Auto).unwrap();
    });
    assert_eq!(
      serial.data(),
      parallel.data(),
      "isotropic box blur output mismatch"
    );

    let mut serial = base.clone();
    blur_anisotropic_body_with_parallelism(&mut serial, 2.0, 5.0, BlurParallelism::Serial).unwrap();
    let mut parallel = base.clone();
    pool.install(|| {
      blur_anisotropic_body_with_parallelism(&mut parallel, 2.0, 5.0, BlurParallelism::Auto)
        .unwrap();
    });
    assert_eq!(
      serial.data(),
      parallel.data(),
      "anisotropic gaussian output mismatch"
    );
  }
}
