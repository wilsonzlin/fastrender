use crate::error::{RenderError, RenderStage};
use crate::geometry::Point;
use crate::paint::pixmap::new_pixmap;
use crate::render_control::{active_deadline, check_active, check_active_periodic, with_deadline};
use crate::style::color::Rgba;
use lru::LruCache;
use rayon::prelude::*;
use rustc_hash::FxHashMap;
use rustc_hash::FxHasher;
use std::hash::BuildHasherDefault;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tiny_skia::{ColorU8, Pixmap, PremultipliedColorU8, SpreadMode};

const DEADLINE_PIXELS_STRIDE: usize = 16 * 1024;
const GRADIENT_PARALLEL_THRESHOLD_PIXELS: usize = 1_000_000;

const DEFAULT_GRADIENT_PIXMAP_CACHE_ITEMS: usize = 64;
// Rasterized gradients can be extremely large (e.g. full-size mask/border-image gradients on
// long-scrolling pages). Keep the cache modest and bounded via LRU eviction to avoid runaway
// memory usage when a page triggers unique gradients.
const DEFAULT_GRADIENT_PIXMAP_CACHE_BYTES: usize = 64 * 1024 * 1024;

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum SpreadModeKey {
  Pad,
  Repeat,
  Reflect,
}

impl From<SpreadMode> for SpreadModeKey {
  fn from(value: SpreadMode) -> Self {
    match value {
      SpreadMode::Pad => SpreadModeKey::Pad,
      SpreadMode::Repeat => SpreadModeKey::Repeat,
      SpreadMode::Reflect => SpreadModeKey::Reflect,
    }
  }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct GradientStats {
  pub pixels: u64,
  pub duration: Duration,
}

impl GradientStats {
  pub fn record(&mut self, pixels: u64, duration: Duration) {
    self.pixels = self.pixels.saturating_add(pixels);
    self.duration += duration;
  }

  pub fn merge(&mut self, other: &GradientStats) {
    self.pixels = self.pixels.saturating_add(other.pixels);
    self.duration += other.duration;
  }

  pub fn millis(&self) -> f64 {
    self.duration.as_secs_f64() * 1000.0
  }
}

type GradientPixmapHasher = BuildHasherDefault<FxHasher>;

#[derive(Clone, Copy, Debug)]
pub struct GradientPixmapCacheConfig {
  pub max_items: usize,
  pub max_bytes: usize,
}

impl Default for GradientPixmapCacheConfig {
  fn default() -> Self {
    Self {
      max_items: DEFAULT_GRADIENT_PIXMAP_CACHE_ITEMS,
      max_bytes: DEFAULT_GRADIENT_PIXMAP_CACHE_BYTES,
    }
  }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct GradientPixmapCacheKey {
  kind: GradientPixmapCacheKeyKind,
  width: u32,
  height: u32,
  params: Vec<u32>,
  lut_key: Option<GradientCacheKey>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
enum GradientPixmapCacheKeyKind {
  Linear,
  Conic,
  ConicScaled,
  Radial,
}

impl GradientPixmapCacheKey {
  pub fn linear(
    width: u32,
    height: u32,
    start: Point,
    end: Point,
    spread: SpreadMode,
    stops: &[(f32, Rgba)],
    bucket: u16,
  ) -> Option<Self> {
    if width == 0 || height == 0 || stops.is_empty() {
      return None;
    }
    if !start.x.is_finite() || !start.y.is_finite() || !end.x.is_finite() || !end.y.is_finite() {
      return None;
    }
    let period = gradient_period(stops);
    Some(Self {
      kind: GradientPixmapCacheKeyKind::Linear,
      width,
      height,
      params: vec![
        start.x.to_bits(),
        start.y.to_bits(),
        end.x.to_bits(),
        end.y.to_bits(),
      ],
      lut_key: Some(GradientCacheKey::new(stops, spread, period, bucket)),
    })
  }

  pub fn conic(
    width: u32,
    height: u32,
    center: Point,
    start_angle: f32,
    spread: SpreadMode,
    stops: &[(f32, Rgba)],
    bucket: u16,
  ) -> Option<Self> {
    if width == 0 || height == 0 || stops.is_empty() {
      return None;
    }
    if !center.x.is_finite() || !center.y.is_finite() || !start_angle.is_finite() {
      return None;
    }
    let period = gradient_period(stops);
    // Canonicalize angle so angles that differ by 2π share cache entries.
    let canonical_angle = start_angle.rem_euclid(std::f32::consts::PI * 2.0);
    Some(Self {
      kind: GradientPixmapCacheKeyKind::Conic,
      width,
      height,
      params: vec![
        center.x.to_bits(),
        center.y.to_bits(),
        canonical_angle.to_bits(),
      ],
      lut_key: Some(GradientCacheKey::new(stops, spread, period, bucket)),
    })
  }

  pub fn conic_scaled(
    width: u32,
    height: u32,
    center: Point,
    start_angle: f32,
    spread: SpreadMode,
    stops: &[(f32, Rgba)],
    bucket: u16,
    scale_x: f32,
    scale_y: f32,
  ) -> Option<Self> {
    if width == 0 || height == 0 || stops.is_empty() {
      return None;
    }
    if !center.x.is_finite()
      || !center.y.is_finite()
      || !start_angle.is_finite()
      || !scale_x.is_finite()
      || !scale_y.is_finite()
    {
      return None;
    }
    let period = gradient_period(stops);
    let canonical_angle = start_angle.rem_euclid(std::f32::consts::PI * 2.0);
    Some(Self {
      kind: GradientPixmapCacheKeyKind::ConicScaled,
      width,
      height,
      params: vec![
        center.x.to_bits(),
        center.y.to_bits(),
        canonical_angle.to_bits(),
        scale_x.to_bits(),
        scale_y.to_bits(),
      ],
      lut_key: Some(GradientCacheKey::new(stops, spread, period, bucket)),
    })
  }

  pub fn radial(
    width: u32,
    height: u32,
    center: Point,
    radii: Point,
    spread: SpreadMode,
    stops: &[(f32, Rgba)],
  ) -> Option<Self> {
    if width == 0 || height == 0 || stops.is_empty() {
      return None;
    }
    if !center.x.is_finite()
      || !center.y.is_finite()
      || !radii.x.is_finite()
      || !radii.y.is_finite()
      || radii.x <= 0.0
      || radii.y <= 0.0
    {
      return None;
    }
    Some(Self {
      kind: GradientPixmapCacheKeyKind::Radial,
      width,
      height,
      params: vec![
        center.x.to_bits(),
        center.y.to_bits(),
        radii.x.to_bits(),
        radii.y.to_bits(),
      ],
      lut_key: Some(GradientCacheKey::new(
        stops,
        spread,
        gradient_period(stops),
        0,
      )),
    })
  }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct GradientPixmapCacheStats {
  pub hits: u64,
  pub misses: u64,
  pub bytes: u64,
  pub items: usize,
}

struct GradientPixmapCacheInner {
  lru: LruCache<GradientPixmapCacheKey, Arc<Pixmap>, GradientPixmapHasher>,
  hits: u64,
  misses: u64,
  bytes: usize,
  config: GradientPixmapCacheConfig,
}

impl GradientPixmapCacheInner {
  fn new(config: GradientPixmapCacheConfig) -> Self {
    Self {
      lru: LruCache::unbounded_with_hasher(GradientPixmapHasher::default()),
      hits: 0,
      misses: 0,
      bytes: 0,
      config,
    }
  }

  fn evict(&mut self) {
    while (self.config.max_items > 0 && self.lru.len() > self.config.max_items)
      || (self.config.max_bytes > 0 && self.bytes > self.config.max_bytes)
    {
      if let Some((_key, value)) = self.lru.pop_lru() {
        self.bytes = self.bytes.saturating_sub(value.data().len());
      } else {
        break;
      }
    }
  }

  fn stats(&self) -> GradientPixmapCacheStats {
    GradientPixmapCacheStats {
      hits: self.hits,
      misses: self.misses,
      bytes: self.bytes as u64,
      items: self.lru.len(),
    }
  }
}

#[derive(Clone)]
pub struct GradientPixmapCache {
  inner: Arc<Mutex<GradientPixmapCacheInner>>,
}

impl Default for GradientPixmapCache {
  fn default() -> Self {
    Self::new(GradientPixmapCacheConfig::default())
  }
}

impl GradientPixmapCache {
  pub fn new(config: GradientPixmapCacheConfig) -> Self {
    Self {
      inner: Arc::new(Mutex::new(GradientPixmapCacheInner::new(config))),
    }
  }

  pub fn snapshot(&self) -> GradientPixmapCacheStats {
    let guard = self
      .inner
      .lock()
      .unwrap_or_else(|poisoned| poisoned.into_inner());
    guard.stats()
  }

  pub fn get_or_insert<F>(
    &self,
    key: GradientPixmapCacheKey,
    build: F,
  ) -> Result<Option<Arc<Pixmap>>, RenderError>
  where
    F: FnOnce() -> Result<Option<Pixmap>, RenderError>,
  {
    // Fast path: caching disabled.
    {
      let guard = self
        .inner
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
      if guard.config.max_items == 0 {
        drop(guard);
        return Ok(build()?.map(Arc::new));
      }
    }

    {
      let mut guard = match self.inner.lock() {
        Ok(guard) => guard,
        Err(poisoned) => {
          let mut guard = poisoned.into_inner();
          // Cache is a performance optimization. If we panic while holding the lock, clear the
          // cache so we don't keep partially inserted entries around.
          guard.lru.clear();
          guard.hits = 0;
          guard.misses = 0;
          guard.bytes = 0;
          guard
        }
      };
      if let Some(found) = guard.lru.get(&key).cloned() {
        guard.hits = guard.hits.saturating_add(1);
        return Ok(Some(found));
      }
      guard.misses = guard.misses.saturating_add(1);
    }

    let Some(pixmap) = build()? else {
      return Ok(None);
    };
    let weight = pixmap.data().len();

    let arc = Arc::new(pixmap);

    let mut guard = match self.inner.lock() {
      Ok(guard) => guard,
      Err(poisoned) => {
        let mut guard = poisoned.into_inner();
        guard.lru.clear();
        guard.hits = 0;
        guard.misses = 0;
        guard.bytes = 0;
        guard
      }
    };

    // Another thread may have inserted while we were rasterizing.
    if let Some(found) = guard.lru.get(&key).cloned() {
      guard.hits = guard.hits.saturating_add(1);
      return Ok(Some(found));
    }

    if guard.config.max_bytes > 0 && weight > guard.config.max_bytes {
      return Ok(Some(arc));
    }

    if let Some(existing) = guard.lru.peek(&key) {
      guard.bytes = guard.bytes.saturating_sub(existing.data().len());
    }
    guard.bytes = guard.bytes.saturating_add(weight);
    guard.lru.put(key, arc.clone());
    guard.evict();
    Ok(Some(arc))
  }
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct GradientStopKey {
  pos_bits: u32,
  r: u8,
  g: u8,
  b: u8,
  a_bits: u32,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct GradientCacheKey {
  stops: Vec<GradientStopKey>,
  spread: SpreadModeKey,
  period_bits: u32,
  bucket: u16,
}

impl GradientCacheKey {
  pub fn new(stops: &[(f32, Rgba)], spread: SpreadMode, period: f32, bucket: u16) -> Self {
    Self {
      stops: stops
        .iter()
        .map(|(pos, color)| GradientStopKey {
          pos_bits: pos.to_bits(),
          r: color.r,
          g: color.g,
          b: color.b,
          a_bits: color.a.to_bits(),
        })
        .collect(),
      spread: spread.into(),
      period_bits: period.to_bits(),
      bucket,
    }
  }
}

#[derive(Clone)]
pub struct GradientLut {
  colors: Arc<Vec<PremultipliedColorU8>>,
  spread: SpreadModeKey,
  period: f32,
  scale: f32,
  last_idx: usize,
  first: PremultipliedColorU8,
  last: PremultipliedColorU8,
}

impl GradientLut {
  #[inline(always)]
  fn sample_mapped(&self, t: f32) -> PremultipliedColorU8 {
    debug_assert!(t.is_finite());
    debug_assert!(t >= 0.0);
    if self.last_idx == 0 {
      return self.first;
    }

    let scaled = t * self.scale;
    let idx = scaled as usize;
    if idx >= self.last_idx {
      return self.last;
    }
    // Fast path: exact hit on a LUT entry.
    let frac = scaled - idx as f32;
    if frac <= 0.0 {
      // SAFETY: idx < last_idx implies idx is within the LUT.
      return unsafe { *self.colors.get_unchecked(idx) };
    }
    // SAFETY: idx < last_idx implies idx+1 is within the LUT.
    let c0 = unsafe { *self.colors.get_unchecked(idx) };
    let c1 = unsafe { *self.colors.get_unchecked(idx + 1) };
    blend_premul(c0, c1, frac)
  }

  #[inline(always)]
  fn sample_pad(&self, t: f32) -> PremultipliedColorU8 {
    if self.last_idx == 0 || !t.is_finite() {
      return self.first;
    }
    if t <= 0.0 {
      return self.first;
    }
    if t >= self.period {
      return self.last;
    }
    self.sample_mapped(t)
  }

  #[inline(always)]
  fn sample_repeat(&self, mut t: f32) -> PremultipliedColorU8 {
    if self.last_idx == 0 || !t.is_finite() {
      return self.first;
    }
    let p = self.period;
    if p <= 0.0 {
      return self.first;
    }
    t = t % p;
    if t < 0.0 {
      t += p;
    }
    self.sample_mapped(t)
  }

  #[inline(always)]
  fn sample_reflect(&self, mut t: f32) -> PremultipliedColorU8 {
    if self.last_idx == 0 || !t.is_finite() {
      return self.first;
    }
    let p = self.period;
    if p <= 0.0 {
      return self.first;
    }
    let two_p = p * 2.0;
    t = t % two_p;
    if t < 0.0 {
      t += two_p;
    }
    if t > p {
      t = two_p - t;
    }
    self.sample_mapped(t)
  }

  #[inline(always)]
  fn sample(&self, t: f32) -> PremultipliedColorU8 {
    match self.spread {
      SpreadModeKey::Pad => self.sample_pad(t),
      SpreadModeKey::Repeat => self.sample_repeat(t),
      SpreadModeKey::Reflect => self.sample_reflect(t),
    }
  }
}

#[derive(Clone, Default)]
pub struct GradientLutCache {
  inner: Arc<Mutex<FxHashMap<GradientCacheKey, Arc<GradientLut>>>>,
}

impl GradientLutCache {
  pub fn get_or_build<F>(&self, key: GradientCacheKey, build: F) -> Arc<GradientLut>
  where
    F: FnOnce() -> GradientLut,
  {
    let mut guard = match self.inner.lock() {
      Ok(guard) => guard,
      Err(poisoned) => {
        let mut guard = poisoned.into_inner();
        // This cache is a performance optimization. If a panic happened while holding the lock we
        // may have partially inserted state, so clear everything and rebuild entries on demand.
        guard.clear();
        guard
      }
    };
    if let Some(found) = guard.get(&key) {
      return found.clone();
    }
    let lut = Arc::new(build());
    guard.entry(key).or_insert_with(|| lut.clone()).clone()
  }
}

#[inline(always)]
fn blend_premul(a: PremultipliedColorU8, b: PremultipliedColorU8, t: f32) -> PremultipliedColorU8 {
  debug_assert!(t.is_finite());
  debug_assert!((0.0..=1.0).contains(&t));

  let inv = 1.0 - t;
  let r = (a.red() as f32 * inv + b.red() as f32 * t + 0.5) as u8;
  let g = (a.green() as f32 * inv + b.green() as f32 * t + 0.5) as u8;
  let blue = (a.blue() as f32 * inv + b.blue() as f32 * t + 0.5) as u8;
  let alpha = (a.alpha() as f32 * inv + b.alpha() as f32 * t + 0.5) as u8;

  PremultipliedColorU8::from_rgba(r, g, blue, alpha).unwrap_or(PremultipliedColorU8::TRANSPARENT)
}

fn build_gradient_lut(
  stops: &[(f32, Rgba)],
  spread: SpreadMode,
  period: f32,
  bucket: u16,
) -> GradientLut {
  let premultiply_rgba = |color: Rgba| -> PremultipliedColorU8 {
    let alpha_u8 = (color.a * 255.0).round().clamp(0.0, 255.0) as u8;
    ColorU8::from_rgba(color.r, color.g, color.b, alpha_u8).premultiply()
  };

  let max_idx = bucket.max(1) as usize;
  let step_count = max_idx + 1;
  let max_idx = max_idx as f32;
  let mut colors = Vec::with_capacity(step_count);
  let mut window = stops.windows(2).peekable();
  for i in 0..step_count {
    let pos = (i as f32 / max_idx) * period;
    while let Some(segment) = window.peek() {
      if pos > segment[1].0 {
        window.next();
      } else {
        break;
      }
    }
    let color = if let Some(segment) = window.peek() {
      let (p0, c0) = segment[0];
      let (p1, c1) = segment[1];
      if (p1 - p0).abs() < f32::EPSILON {
        c0
      } else {
        let frac = ((pos - p0) / (p1 - p0)).clamp(0.0, 1.0);
        Rgba {
          r: (c0.r as f32 + (c1.r as f32 - c0.r as f32) * frac)
            .round()
            .clamp(0.0, 255.0) as u8,
          g: (c0.g as f32 + (c1.g as f32 - c0.g as f32) * frac)
            .round()
            .clamp(0.0, 255.0) as u8,
          b: (c0.b as f32 + (c1.b as f32 - c0.b as f32) * frac)
            .round()
            .clamp(0.0, 255.0) as u8,
          a: c0.a + (c1.a - c0.a) * frac,
        }
      }
    } else {
      stops.last().map(|(_, c)| *c).unwrap_or(Rgba::TRANSPARENT)
    };
    colors.push(premultiply_rgba(color));
  }

  let colors = Arc::new(colors);
  let last_idx = colors.len().saturating_sub(1);
  // Pad gradients use `first/last` directly when clamping outside the stop range, so make sure
  // these match the actual terminal stop colors rather than the sampled LUT values. This matters
  // when there are duplicate stops at the terminal positions (e.g. a sharp edge at `t==period`),
  // where the LUT sample at the exact position can come from the preceding segment.
  let first = stops
    .first()
    .map(|(_, c)| premultiply_rgba(*c))
    .unwrap_or(PremultipliedColorU8::TRANSPARENT);
  let last = stops.last().map(|(_, c)| premultiply_rgba(*c)).unwrap_or(first);
  let scale = max_idx / period.max(1e-6);

  GradientLut {
    spread: spread.into(),
    period,
    scale,
    last_idx,
    first,
    last,
    colors,
  }
}

pub fn gradient_period(stops: &[(f32, Rgba)]) -> f32 {
  stops.last().map(|(pos, _)| *pos).unwrap_or(1.0).max(1e-6)
}

pub fn gradient_bucket(max_dim: u32) -> u16 {
  let mut bucket = 64u32;
  let target = max_dim.max(64);
  while bucket < target {
    bucket *= 2;
    if bucket >= 4096 {
      bucket = 4096;
      break;
    }
  }
  bucket as u16
}

pub fn rasterize_linear_gradient(
  width: u32,
  height: u32,
  start: Point,
  end: Point,
  spread: SpreadMode,
  stops: &[(f32, Rgba)],
  cache: &GradientLutCache,
  bucket: u16,
) -> Result<Option<Pixmap>, RenderError> {
  check_active(RenderStage::Paint)?;
  if width == 0 || height == 0 || stops.is_empty() {
    return Ok(None);
  }
  let period = gradient_period(stops);
  let key = GradientCacheKey::new(stops, spread, period, bucket);
  let lut = cache.get_or_build(key, || build_gradient_lut(stops, spread, period, bucket));

  let dx = end.x - start.x;
  let dy = end.y - start.y;
  let denom = dx * dx + dy * dy;
  let Some(mut pixmap) = new_pixmap(width, height) else {
    return Ok(None);
  };
  let pixels_len = width as usize * height as usize;
  if denom.abs() <= f32::EPSILON {
    let color = lut.sample(0.0);
    let pixels = pixmap.pixels_mut();
    if pixels_len >= GRADIENT_PARALLEL_THRESHOLD_PIXELS {
      let deadline = active_deadline();
      pixels
        .par_chunks_mut(DEADLINE_PIXELS_STRIDE)
        .try_for_each(|chunk| {
          with_deadline(deadline.as_ref(), || {
            let mut counter = 0usize;
            check_active_periodic(&mut counter, 1, RenderStage::Paint)?;
            chunk.fill(color);
            Ok(())
          })
        })?;
    } else {
      let mut deadline_counter = 0usize;
      for chunk in pixels.chunks_mut(DEADLINE_PIXELS_STRIDE) {
        check_active_periodic(&mut deadline_counter, 1, RenderStage::Paint)?;
        chunk.fill(color);
      }
    }
    return Ok(Some(pixmap));
  }

  let inv_len = 1.0 / denom;
  let step_x = dx * inv_len;
  let step_y = dy * inv_len;
  let start_dot = (0.5 - start.x) * dx + (0.5 - start.y) * dy;
  let row_start0 = start_dot * inv_len;
  let stride = width as usize;
  let pixels = pixmap.pixels_mut();
  let spread_key: SpreadModeKey = spread.into();
  let sample_row = |y: usize, row: &mut [PremultipliedColorU8]| -> Result<(), RenderError> {
    let mut t = row_start0 + y as f32 * step_y;
    let mut deadline_counter = 0usize;
    match spread_key {
      SpreadModeKey::Pad => {
        for chunk in row.chunks_mut(DEADLINE_PIXELS_STRIDE) {
          check_active_periodic(&mut deadline_counter, 1, RenderStage::Paint)?;
          for pixel in chunk {
            *pixel = lut.sample_pad(t);
            t += step_x;
          }
        }
      }
      SpreadModeKey::Repeat => {
        for chunk in row.chunks_mut(DEADLINE_PIXELS_STRIDE) {
          check_active_periodic(&mut deadline_counter, 1, RenderStage::Paint)?;
          for pixel in chunk {
            *pixel = lut.sample_repeat(t);
            t += step_x;
          }
        }
      }
      SpreadModeKey::Reflect => {
        for chunk in row.chunks_mut(DEADLINE_PIXELS_STRIDE) {
          check_active_periodic(&mut deadline_counter, 1, RenderStage::Paint)?;
          for pixel in chunk {
            *pixel = lut.sample_reflect(t);
            t += step_x;
          }
        }
      }
    };
    Ok(())
  };

  if pixels_len >= GRADIENT_PARALLEL_THRESHOLD_PIXELS {
    let deadline = active_deadline();
    pixels
      .par_chunks_mut(stride)
      .enumerate()
      .try_for_each(|(y, row)| {
        with_deadline(deadline.as_ref(), || -> Result<(), RenderError> {
          sample_row(y, row)
        })
      })?;
  } else {
    for (y, row) in pixels.chunks_mut(stride).enumerate() {
      sample_row(y, row)?;
    }
  }

  Ok(Some(pixmap))
}

pub fn rasterize_linear_gradient_cached(
  pixmap_cache: &GradientPixmapCache,
  width: u32,
  height: u32,
  start: Point,
  end: Point,
  spread: SpreadMode,
  stops: &[(f32, Rgba)],
  cache: &GradientLutCache,
  bucket: u16,
) -> Result<Option<Arc<Pixmap>>, RenderError> {
  let Some(key) = GradientPixmapCacheKey::linear(width, height, start, end, spread, stops, bucket)
  else {
    return Ok(None);
  };
  pixmap_cache.get_or_insert(key, || {
    rasterize_linear_gradient(width, height, start, end, spread, stops, cache, bucket)
  })
}

pub fn rasterize_conic_gradient(
  width: u32,
  height: u32,
  center: Point,
  start_angle: f32,
  spread: SpreadMode,
  stops: &[(f32, Rgba)],
  cache: &GradientLutCache,
  bucket: u16,
) -> Result<Option<Pixmap>, RenderError> {
  check_active(RenderStage::Paint)?;
  if width == 0 || height == 0 || stops.is_empty() {
    return Ok(None);
  }

  let period = gradient_period(stops);
  let key = GradientCacheKey::new(stops, spread, period, bucket);
  let lut = cache.get_or_build(key, || build_gradient_lut(stops, spread, period, bucket));
  let Some(mut pixmap) = new_pixmap(width, height) else {
    return Ok(None);
  };

  let start_angle = start_angle.rem_euclid(std::f32::consts::PI * 2.0);
  // `dx.atan2(-dy)` yields an angle in [-π, π] where 0 points "up" and angles increase clockwise
  // (matching CSS conic gradients). Map it into a [0, 1) turn fraction. Importantly, we map the
  // full circle into [0, 1) regardless of the last stop position; for non-repeating conic
  // gradients, the terminal stop color should simply extend through the remainder of the circle
  // (e.g. stops ending at 0.5 should dominate angles in [0.5, 1.0)).
  let angle_scale = 0.5 / std::f32::consts::PI;
  let stride = width as usize;
  let pixels = pixmap.pixels_mut();
  let dx0 = 0.5 - center.x;
  let pixels_len = width as usize * height as usize;
  let spread_key: SpreadModeKey = spread.into();
  let sample_row = |y: usize, row: &mut [PremultipliedColorU8]| -> Result<(), RenderError> {
    let dy = y as f32 + 0.5 - center.y;
    let mut dx = dx0;
    let mut deadline_counter = 0usize;
    match spread_key {
      SpreadModeKey::Pad => {
        for chunk in row.chunks_mut(DEADLINE_PIXELS_STRIDE) {
          check_active_periodic(&mut deadline_counter, 1, RenderStage::Paint)?;
          for pixel in chunk {
            let mut t = (dx.atan2(-dy) + start_angle) * angle_scale;
            if t < 0.0 {
              t += 1.0;
            } else if t >= 1.0 {
              t -= 1.0;
            }
            *pixel = lut.sample_pad(t);
            dx += 1.0;
          }
        }
      }
      SpreadModeKey::Repeat => {
        for chunk in row.chunks_mut(DEADLINE_PIXELS_STRIDE) {
          check_active_periodic(&mut deadline_counter, 1, RenderStage::Paint)?;
          for pixel in chunk {
            let mut t = (dx.atan2(-dy) + start_angle) * angle_scale;
            if t < 0.0 {
              t += 1.0;
            } else if t >= 1.0 {
              t -= 1.0;
            }
            *pixel = lut.sample_repeat(t);
            dx += 1.0;
          }
        }
      }
      SpreadModeKey::Reflect => {
        for chunk in row.chunks_mut(DEADLINE_PIXELS_STRIDE) {
          check_active_periodic(&mut deadline_counter, 1, RenderStage::Paint)?;
          for pixel in chunk {
            let mut t = (dx.atan2(-dy) + start_angle) * angle_scale;
            if t < 0.0 {
              t += 1.0;
            } else if t >= 1.0 {
              t -= 1.0;
            }
            *pixel = lut.sample_reflect(t);
            dx += 1.0;
          }
        }
      }
    };
    Ok(())
  };

  if pixels_len >= GRADIENT_PARALLEL_THRESHOLD_PIXELS {
    let deadline = active_deadline();
    pixels
      .par_chunks_mut(stride)
      .enumerate()
      .try_for_each(|(y, row)| {
        with_deadline(deadline.as_ref(), || -> Result<(), RenderError> {
          sample_row(y, row)
        })
      })?;
  } else {
    for (y, row) in pixels.chunks_mut(stride).enumerate() {
      sample_row(y, row)?;
    }
  }

  Ok(Some(pixmap))
}

pub fn rasterize_conic_gradient_cached(
  pixmap_cache: &GradientPixmapCache,
  width: u32,
  height: u32,
  center: Point,
  start_angle: f32,
  spread: SpreadMode,
  stops: &[(f32, Rgba)],
  cache: &GradientLutCache,
  bucket: u16,
) -> Result<Option<Arc<Pixmap>>, RenderError> {
  let Some(key) =
    GradientPixmapCacheKey::conic(width, height, center, start_angle, spread, stops, bucket)
  else {
    return Ok(None);
  };
  pixmap_cache.get_or_insert(key, || {
    rasterize_conic_gradient(
      width,
      height,
      center,
      start_angle,
      spread,
      stops,
      cache,
      bucket,
    )
  })
}

/// Rasterize a conic gradient into a pixmap where the sampling coordinate space is scaled.
///
/// This is useful when the resulting pixmap will be drawn with a non-uniform scale (e.g. as a
/// repeated pattern where the tile size is fractional in device pixels). The `scale_x/scale_y`
/// parameters describe how many destination (device) pixels correspond to a 1px step in the
/// rasterized pixmap.
pub fn rasterize_conic_gradient_scaled(
  width: u32,
  height: u32,
  center: Point,
  start_angle: f32,
  spread: SpreadMode,
  stops: &[(f32, Rgba)],
  cache: &GradientLutCache,
  bucket: u16,
  scale_x: f32,
  scale_y: f32,
) -> Result<Option<Pixmap>, RenderError> {
  check_active(RenderStage::Paint)?;
  if width == 0
    || height == 0
    || stops.is_empty()
    || !scale_x.is_finite()
    || !scale_y.is_finite()
    || scale_x <= 0.0
    || scale_y <= 0.0
  {
    return Ok(None);
  }

  let period = gradient_period(stops);
  let key = GradientCacheKey::new(stops, spread, period, bucket);
  let lut = cache.get_or_build(key, || build_gradient_lut(stops, spread, period, bucket));
  let Some(mut pixmap) = new_pixmap(width, height) else {
    return Ok(None);
  };

  let start_angle = start_angle.rem_euclid(std::f32::consts::PI * 2.0);
  let angle_scale = 0.5 / std::f32::consts::PI;
  let stride = width as usize;
  let pixels = pixmap.pixels_mut();
  let dx0 = (0.5 - center.x) * scale_x;
  let mut deadline_counter = 0usize;
  const DEADLINE_PIXELS_STRIDE: usize = 16 * 1024;
  for y in 0..height as usize {
    let dy = (y as f32 + 0.5 - center.y) * scale_y;
    let mut dx = dx0;
    let row_base = y * stride;
    for chunk in pixels[row_base..row_base + stride].chunks_mut(DEADLINE_PIXELS_STRIDE) {
      check_active_periodic(&mut deadline_counter, 1, RenderStage::Paint)?;
      for pixel in chunk {
        let mut t = (dx.atan2(-dy) + start_angle) * angle_scale;
        if t < 0.0 {
          t += 1.0;
        } else if t >= 1.0 {
          t -= 1.0;
        }
        *pixel = lut.sample(t);
        dx += scale_x;
      }
    }
  }

  Ok(Some(pixmap))
}

pub fn rasterize_conic_gradient_scaled_cached(
  pixmap_cache: &GradientPixmapCache,
  width: u32,
  height: u32,
  center: Point,
  start_angle: f32,
  spread: SpreadMode,
  stops: &[(f32, Rgba)],
  cache: &GradientLutCache,
  bucket: u16,
  scale_x: f32,
  scale_y: f32,
) -> Result<Option<Arc<Pixmap>>, RenderError> {
  let Some(key) = GradientPixmapCacheKey::conic_scaled(
    width,
    height,
    center,
    start_angle,
    spread,
    stops,
    bucket,
    scale_x,
    scale_y,
  ) else {
    return Ok(None);
  };
  pixmap_cache.get_or_insert(key, || {
    rasterize_conic_gradient_scaled(
      width,
      height,
      center,
      start_angle,
      spread,
      stops,
      cache,
      bucket,
      scale_x,
      scale_y,
    )
  })
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::render_control::{with_deadline, RenderDeadline};
  use std::time::Instant;

  fn naive_conic(
    width: u32,
    height: u32,
    center: Point,
    start_angle: f32,
    stops: &[(f32, Rgba)],
    spread: SpreadMode,
  ) -> Pixmap {
    let period = gradient_period(stops);
    let Some(mut pixmap) = new_pixmap(width, height) else {
      panic!("pixmap allocation failed");
    };
    let stride = width as usize;
    let pixels = pixmap.pixels_mut();
    let inv_two_pi = 0.5 / std::f32::consts::PI;
    for y in 0..height as usize {
      let dy = y as f32 + 0.5 - center.y;
      for x in 0..width as usize {
        let dx = x as f32 + 0.5 - center.x;
        let angle = dx.atan2(-dy) + start_angle;
        let mut pos = (angle * inv_two_pi).rem_euclid(1.0) * period;
        match spread {
          SpreadMode::Repeat => {
            pos = pos.rem_euclid(period);
          }
          SpreadMode::Pad => pos = pos.clamp(0.0, period),
          SpreadMode::Reflect => {
            let two_p = period * 2.0;
            let mut v = pos.rem_euclid(two_p);
            if v > period {
              v = two_p - v;
            }
            pos = v;
          }
        }
        let color = sample_stop_color(stops, pos, period, spread);
        pixels[y * stride + x] = PremultipliedColorU8::from_rgba(
          color.r,
          color.g,
          color.b,
          (color.a * 255.0).round().clamp(0.0, 255.0) as u8,
        )
        .unwrap();
      }
    }
    pixmap
  }

  fn sample_stop_color(stops: &[(f32, Rgba)], t: f32, period: f32, spread: SpreadMode) -> Rgba {
    if stops.is_empty() {
      return Rgba::TRANSPARENT;
    }
    let mut pos = match spread {
      SpreadMode::Pad => t.clamp(0.0, period),
      SpreadMode::Repeat => t.rem_euclid(period),
      SpreadMode::Reflect => {
        let two_p = period * 2.0;
        let mut v = t.rem_euclid(two_p);
        if v > period {
          v = two_p - v;
        }
        v
      }
    };
    if pos <= stops[0].0 {
      return stops[0].1;
    }
    if pos >= stops.last().unwrap().0 {
      return stops.last().unwrap().1;
    }
    for window in stops.windows(2) {
      let (p0, c0) = window[0];
      let (p1, c1) = window[1];
      if pos >= p0 && pos <= p1 {
        let frac = ((pos - p0) / (p1 - p0)).clamp(0.0, 1.0);
        return Rgba {
          r: (c0.r as f32 + (c1.r as f32 - c0.r as f32) * frac)
            .round()
            .clamp(0.0, 255.0) as u8,
          g: (c0.g as f32 + (c1.g as f32 - c0.g as f32) * frac)
            .round()
            .clamp(0.0, 255.0) as u8,
          b: (c0.b as f32 + (c1.b as f32 - c0.b as f32) * frac)
            .round()
            .clamp(0.0, 255.0) as u8,
          a: c0.a + (c1.a - c0.a) * frac,
        };
      }
    }
    stops.last().unwrap().1
  }

  #[test]
  fn gradient_lut_cache_recovers_from_poisoned_lock() {
    let cache = GradientLutCache::default();

    let result = std::panic::catch_unwind(|| {
      let _guard = cache.inner.lock().unwrap();
      panic!("poison gradient LUT cache lock");
    });
    assert!(result.is_err(), "expected panic to be caught");
    assert!(
      cache.inner.is_poisoned(),
      "expected LUT cache mutex to be poisoned"
    );

    let stops = [(0.0, Rgba::BLACK), (1.0, Rgba::WHITE)];
    let key = GradientCacheKey::new(&stops, SpreadMode::Pad, 1.0, 16);
    let lut = cache.get_or_build(key, || build_gradient_lut(&stops, SpreadMode::Pad, 1.0, 16));
    assert_eq!(lut.period, 1.0);
    assert!(!lut.colors.is_empty());
  }

  #[test]
  fn gradient_pixmap_cache_hits_on_second_render() {
    let lut_cache = GradientLutCache::default();
    let pixmap_cache = GradientPixmapCache::default();
    let stops = vec![(0.0, Rgba::RED), (1.0, Rgba::BLUE)];
    let width = 64;
    let height = 32;
    let bucket = gradient_bucket(width.max(height));
    let start = Point::new(0.0, 0.0);
    let end = Point::new(width as f32, 0.0);

    let first = rasterize_linear_gradient_cached(
      &pixmap_cache,
      width,
      height,
      start,
      end,
      SpreadMode::Pad,
      &stops,
      &lut_cache,
      bucket,
    )
    .expect("first rasterize")
    .expect("first pixmap");
    let after_first = pixmap_cache.snapshot();
    assert_eq!(after_first.misses, 1);
    assert_eq!(after_first.hits, 0);

    let second = rasterize_linear_gradient_cached(
      &pixmap_cache,
      width,
      height,
      start,
      end,
      SpreadMode::Pad,
      &stops,
      &lut_cache,
      bucket,
    )
    .expect("second rasterize")
    .expect("second pixmap");
    let after_second = pixmap_cache.snapshot();
    assert_eq!(after_second.misses, 1);
    assert_eq!(after_second.hits, 1);
    assert!(Arc::ptr_eq(&first, &second));
    assert_eq!(first.data(), second.data());
  }

  fn max_diff(a: &Pixmap, b: &Pixmap) -> u8 {
    a.data()
      .iter()
      .zip(b.data())
      .map(|(x, y)| x.abs_diff(*y))
      .max()
      .unwrap_or(0)
  }

  #[test]
  fn conic_lut_matches_naive_with_low_error() {
    let stops = vec![(0.0, Rgba::RED), (0.5, Rgba::GREEN), (1.0, Rgba::BLUE)];
    let cache = GradientLutCache::default();
    let width = 64;
    let height = 64;
    let center = Point::new(width as f32 / 2.0, height as f32 / 2.0);
    let lut_pixmap = rasterize_conic_gradient(
      width,
      height,
      center,
      0.0,
      SpreadMode::Repeat,
      &stops,
      &cache,
      gradient_bucket(width.max(height).saturating_mul(2)),
    )
    .expect("lut rasterize")
    .expect("lut pixmap");
    let naive = naive_conic(width, height, center, 0.0, &stops, SpreadMode::Repeat);
    assert!(max_diff(&lut_pixmap, &naive) <= 1);
  }

  fn naive_conic_scaled(
    width: u32,
    height: u32,
    center: Point,
    start_angle: f32,
    stops: &[(f32, Rgba)],
    spread: SpreadMode,
    scale_x: f32,
    scale_y: f32,
  ) -> Pixmap {
    let period = gradient_period(stops);
    let Some(mut pixmap) = new_pixmap(width, height) else {
      panic!("pixmap allocation failed");
    };
    let stride = width as usize;
    let pixels = pixmap.pixels_mut();
    let inv_two_pi = 0.5 / std::f32::consts::PI;
    for y in 0..height as usize {
      let dy = (y as f32 + 0.5 - center.y) * scale_y;
      for x in 0..width as usize {
        let dx = (x as f32 + 0.5 - center.x) * scale_x;
        let angle = dx.atan2(-dy) + start_angle;
        let mut pos = (angle * inv_two_pi).rem_euclid(1.0) * period;
        match spread {
          SpreadMode::Repeat => {
            pos = pos.rem_euclid(period);
          }
          SpreadMode::Pad => pos = pos.clamp(0.0, period),
          SpreadMode::Reflect => {
            let two_p = period * 2.0;
            let mut v = pos.rem_euclid(two_p);
            if v > period {
              v = two_p - v;
            }
            pos = v;
          }
        }
        let color = sample_stop_color(stops, pos, period, spread);
        pixels[y * stride + x] = PremultipliedColorU8::from_rgba(
          color.r,
          color.g,
          color.b,
          (color.a * 255.0).round().clamp(0.0, 255.0) as u8,
        )
        .unwrap();
      }
    }
    pixmap
  }

  #[test]
  fn linear_lut_matches_naive_with_low_error() {
    let stops = vec![(0.0, Rgba::RED), (1.0, Rgba::BLUE)];
    let cache = GradientLutCache::default();
    let width = 32;
    let height = 8;
    let start = Point::new(0.0, 0.0);
    let end = Point::new(width as f32, 0.0);
    let lut_pixmap = rasterize_linear_gradient(
      width,
      height,
      start,
      end,
      SpreadMode::Pad,
      &stops,
      &cache,
      gradient_bucket(width.max(height)),
    )
    .expect("lut rasterize")
    .expect("lut pixmap");

    let mut naive = new_pixmap(width, height).expect("pixmap");
    let denom = (end.x - start.x) * (end.x - start.x) + (end.y - start.y) * (end.y - start.y);
    let inv = 1.0 / denom;
    let stride = width as usize;
    let pixels = naive.pixels_mut();
    for y in 0..height as usize {
      for x in 0..width as usize {
        let px = x as f32 + 0.5;
        let py = y as f32 + 0.5;
        let t = ((px - start.x) * (end.x - start.x) + (py - start.y) * (end.y - start.y)) * inv;
        let pos = t.clamp(0.0, 1.0);
        let color = sample_stop_color(&stops, pos, 1.0, SpreadMode::Pad);
        pixels[y * stride + x] = PremultipliedColorU8::from_rgba(
          color.r,
          color.g,
          color.b,
          (color.a * 255.0).round().clamp(0.0, 255.0) as u8,
        )
        .unwrap();
      }
    }

    assert!(max_diff(&lut_pixmap, &naive) <= 1);
  }

  #[test]
  fn linear_gradient_premultiplies_semi_transparent_stops() {
    let color = Rgba {
      r: 0,
      g: 255,
      b: 0,
      a: 0.5,
    };
    let stops = vec![(0.0, color), (1.0, color)];
    let cache = GradientLutCache::default();
    let pixmap = rasterize_linear_gradient(
      1,
      1,
      Point::new(0.0, 0.0),
      Point::new(0.0, 0.0),
      SpreadMode::Pad,
      &stops,
      &cache,
      1,
    )
    .expect("gradient rasterize")
    .expect("gradient pixmap");
    let px = pixmap.pixel(0, 0).expect("pixel");
    assert_eq!(px.red(), 0);
    assert_eq!(px.green(), 128);
    assert_eq!(px.blue(), 0);
    assert_eq!(px.alpha(), 128);
  }

  #[test]
  fn conic_lut_scaled_matches_naive_with_low_error() {
    let stops = vec![(0.0, Rgba::RED), (0.5, Rgba::GREEN), (1.0, Rgba::BLUE)];
    let cache = GradientLutCache::default();
    let width = 64;
    let height = 32;
    let center = Point::new(width as f32 / 2.0, height as f32 / 2.0);
    let scale_x = 0.75;
    let scale_y = 1.25;
    let lut_pixmap = rasterize_conic_gradient_scaled(
      width,
      height,
      center,
      0.0,
      SpreadMode::Repeat,
      &stops,
      &cache,
      gradient_bucket(width.max(height)),
      scale_x,
      scale_y,
    )
    .expect("lut rasterize")
    .expect("lut pixmap");
    let naive = naive_conic_scaled(
      width,
      height,
      center,
      0.0,
      &stops,
      SpreadMode::Repeat,
      scale_x,
      scale_y,
    );
    let diff = max_diff(&lut_pixmap, &naive);
    assert!(
      diff <= 2,
      "expected scaled conic LUT raster to be close to naive; max_diff={diff}"
    );
  }

  #[test]
  fn gradient_rasterizers_timeout_under_tiny_deadline() {
    let stops = vec![(0.0, Rgba::RED), (1.0, Rgba::BLUE)];
    let cache = GradientLutCache::default();
    let width = 2048;
    let height = 2048;
    let center = Point::new(width as f32 / 2.0, height as f32 / 2.0);
    let deadline = RenderDeadline::new(Some(Duration::from_millis(1)), None);
    let start = Instant::now();
    let result = with_deadline(Some(&deadline), || {
      rasterize_conic_gradient(
        width,
        height,
        center,
        0.0,
        SpreadMode::Repeat,
        &stops,
        &cache,
        gradient_bucket(width.max(height).saturating_mul(2)),
      )
    });
    let elapsed = start.elapsed();
    assert!(
      matches!(
        result,
        Err(RenderError::Timeout {
          stage: RenderStage::Paint,
          ..
        })
      ),
      "expected timeout, got {result:?}"
    );
    assert!(
      elapsed < Duration::from_millis(250),
      "timeout should be cooperative (elapsed {elapsed:?})"
    );
  }

  #[test]
  fn gradient_output_unchanged_with_deadline_enabled() {
    let cache = GradientLutCache::default();
    let deadline = RenderDeadline::new(Some(Duration::from_secs(60)), None);

    let linear_stops = vec![(0.0, Rgba::RED), (0.35, Rgba::GREEN), (1.0, Rgba::BLUE)];
    let width = 256;
    let height = 128;
    let start = Point::new(0.0, 0.0);
    let end = Point::new(width as f32, height as f32);
    let bucket = gradient_bucket(width.max(height));
    let base = rasterize_linear_gradient(
      width,
      height,
      start,
      end,
      SpreadMode::Pad,
      &linear_stops,
      &cache,
      bucket,
    )
    .expect("linear rasterize")
    .expect("linear pixmap");
    let with_deadline_pixmap = with_deadline(Some(&deadline), || {
      rasterize_linear_gradient(
        width,
        height,
        start,
        end,
        SpreadMode::Pad,
        &linear_stops,
        &cache,
        bucket,
      )
    })
    .expect("linear rasterize with deadline")
    .expect("linear pixmap with deadline");
    assert_eq!(base.data(), with_deadline_pixmap.data());

    let conic_stops = vec![(0.0, Rgba::BLACK), (0.5, Rgba::WHITE), (1.0, Rgba::BLACK)];
    let size = 192u32;
    let center = Point::new(size as f32 / 2.0, size as f32 / 2.0);
    let bucket = gradient_bucket(size.saturating_mul(2));
    let base = rasterize_conic_gradient(
      size,
      size,
      center,
      0.4,
      SpreadMode::Repeat,
      &conic_stops,
      &cache,
      bucket,
    )
    .expect("conic rasterize")
    .expect("conic pixmap");
    let with_deadline_pixmap = with_deadline(Some(&deadline), || {
      rasterize_conic_gradient(
        size,
        size,
        center,
        0.4,
        SpreadMode::Repeat,
        &conic_stops,
        &cache,
        bucket,
      )
    })
    .expect("conic rasterize with deadline")
    .expect("conic pixmap with deadline");
    assert_eq!(base.data(), with_deadline_pixmap.data());
  }
}
