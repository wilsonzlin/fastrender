//! Image loading and caching
//!
//! This module provides image loading from various sources (HTTP, file, data URLs)
//! with in-memory caching and support for various image formats including SVG.

use crate::api::{RenderDiagnostics, ResourceContext, ResourceKind};
use crate::debug::runtime;
use crate::error::{Error, ImageError, RenderError, RenderStage, Result};
use crate::paint::painter::with_paint_diagnostics;
use crate::paint::pixmap::{new_pixmap, MAX_PIXMAP_BYTES};
use crate::render_control::{self, check_active, check_active_periodic};
use crate::resource::{ensure_http_success, ensure_image_mime_sane};
use crate::resource::CacheArtifactKind;
use crate::resource::CachingFetcher;
use crate::resource::CachingFetcherConfig;
use crate::resource::FetchContextKind;
use crate::resource::FetchDestination;
use crate::resource::FetchRequest;
use crate::resource::FetchedResource;
use crate::resource::HttpFetcher;
use crate::resource::ResourceFetcher;
use crate::style::color::Rgba;
use crate::style::types::ImageResolution;
use crate::style::types::OrientationTransform;
use crate::svg::{
  map_svg_aspect_ratio, parse_svg_length_px, parse_svg_view_box,
  svg_intrinsic_dimensions_from_attributes, svg_view_box_root_transform, SvgPreserveAspectRatio,
  SvgViewBox,
};
use avif_decode::Decoder as AvifDecoder;
use avif_decode::Image as AvifImage;
use avif_parse::AvifData;
use exif;
use image::imageops;
use image::DynamicImage;
use image::GenericImageView;
use image::ImageDecoder;
use image::ImageFormat;
use image::ImageReader;
use image::RgbaImage;
use lru::LruCache;
use percent_encoding::percent_decode_str;
use roxmltree::Document;
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::borrow::Cow;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::{self, BufRead, Cursor, Read, Seek, SeekFrom};
use std::path::Path;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::time::{Duration, Instant};
use tiny_skia::{FilterQuality, IntSize, Pixmap};
use url::Url;

fn unescape_js_escapes(input: &str) -> Cow<'_, str> {
  if !input.contains('\\') {
    return Cow::Borrowed(input);
  }

  let mut out = String::with_capacity(input.len());
  let bytes = input.as_bytes();
  let mut i = 0;
  while i < bytes.len() {
    if bytes[i] == b'\\' {
      if i + 1 < bytes.len()
        && (bytes[i + 1] == b'"' || bytes[i + 1] == b'\'' || bytes[i + 1] == b'/')
      {
        out.push(bytes[i + 1] as char);
        i += 2;
        continue;
      }

      if i + 5 < bytes.len() && (bytes[i + 1] == b'u' || bytes[i + 1] == b'U') {
        if let Ok(code) = u16::from_str_radix(&input[i + 2..i + 6], 16) {
          if let Some(ch) = char::from_u32(code as u32) {
            out.push(ch);
            i += 6;
            continue;
          }
        }
      }
    }

    out.push(bytes[i] as char);
    i += 1;
  }

  Cow::Owned(out)
}

fn decode_inline_svg_url(url: &str) -> Option<String> {
  let trimmed = url.trim_start();
  if trimmed.is_empty() {
    return None;
  }

  if trimmed.starts_with('<') {
    return Some(unescape_js_escapes(trimmed).into_owned());
  }

  // Inline SVG markup sometimes appears percent-encoded (e.g. `%3Csvg ...`). Treat it the same as
  // raw `<svg...>` strings so the renderer doesn't accidentally resolve it as a network URL.
  if trimmed
    .get(.."%3csvg".len())
    .is_some_and(|prefix| prefix.eq_ignore_ascii_case("%3csvg"))
  {
    if let Ok(decoded) = percent_decode_str(trimmed).decode_utf8() {
      let decoded = decoded.into_owned();
      let decoded_trimmed = decoded.trim_start();
      if decoded_trimmed.starts_with('<') {
        return Some(unescape_js_escapes(decoded_trimmed).into_owned());
      }
    }
  }

  None
}

fn image_profile_threshold_ms() -> Option<f64> {
  runtime::runtime_toggles().f64("FASTR_IMAGE_PROFILE_MS")
}

fn image_probe_max_bytes() -> usize {
  runtime::runtime_toggles()
    .usize("FASTR_IMAGE_PROBE_MAX_BYTES")
    .unwrap_or(64 * 1024)
    .clamp(1, 16 * 1024 * 1024)
}

/// Image cache diagnostics collection.
#[derive(Debug, Default, Clone)]
pub struct ImageCacheDiagnostics {
  pub requests: usize,
  pub cache_hits: usize,
  pub cache_misses: usize,
  pub decode_ms: f64,
  /// Number of `ResourceFetcher::fetch_partial` calls made by image probes.
  pub probe_partial_requests: usize,
  /// Total bytes returned by partial probe fetches (sum of response body prefixes).
  pub probe_partial_bytes_total: usize,
  /// Number of times an image probe attempted partial fetches but ultimately fell back to a full
  /// `fetch()` due to insufficient bytes / unsupported partial responses.
  pub probe_partial_fallback_full: usize,
  pub raster_pixmap_cache_hits: usize,
  pub raster_pixmap_cache_misses: usize,
  /// Maximum cached bytes observed for the raster pixmap cache during the diagnostic window.
  pub raster_pixmap_cache_bytes: usize,
}

static IMAGE_CACHE_DIAGNOSTICS_ACTIVE: AtomicBool = AtomicBool::new(false);
static IMAGE_CACHE_DIAGNOSTICS: Mutex<Option<ImageCacheDiagnostics>> = Mutex::new(None);

pub(crate) fn enable_image_cache_diagnostics() {
  IMAGE_CACHE_DIAGNOSTICS_ACTIVE.store(true, Ordering::Relaxed);
  let mut guard = IMAGE_CACHE_DIAGNOSTICS
    .lock()
    .unwrap_or_else(|poisoned| poisoned.into_inner());
  *guard = Some(ImageCacheDiagnostics::default());
}

pub(crate) fn take_image_cache_diagnostics() -> Option<ImageCacheDiagnostics> {
  IMAGE_CACHE_DIAGNOSTICS_ACTIVE.store(false, Ordering::Relaxed);
  IMAGE_CACHE_DIAGNOSTICS
    .lock()
    .unwrap_or_else(|poisoned| poisoned.into_inner())
    .take()
}

#[inline]
fn with_image_cache_diagnostics<F: FnOnce(&mut ImageCacheDiagnostics)>(f: F) {
  if !IMAGE_CACHE_DIAGNOSTICS_ACTIVE.load(Ordering::Relaxed) {
    return;
  }
  let mut guard = IMAGE_CACHE_DIAGNOSTICS
    .lock()
    .unwrap_or_else(|poisoned| poisoned.into_inner());
  if let Some(stats) = guard.as_mut() {
    f(stats);
  }
}

fn record_image_cache_request() {
  with_image_cache_diagnostics(|stats| stats.requests += 1);
}

fn record_image_cache_hit() {
  with_image_cache_diagnostics(|stats| stats.cache_hits += 1);
}

fn record_image_cache_miss() {
  with_image_cache_diagnostics(|stats| stats.cache_misses += 1);
}

fn record_image_decode_ms(duration_ms: f64) {
  if !duration_ms.is_finite() || duration_ms <= 0.0 {
    return;
  }
  with_image_cache_diagnostics(|stats| stats.decode_ms += duration_ms);
}

fn record_probe_partial_fetch(bytes: usize) {
  with_image_cache_diagnostics(|stats| {
    stats.probe_partial_requests += 1;
    stats.probe_partial_bytes_total = stats.probe_partial_bytes_total.saturating_add(bytes);
  });
}

fn record_probe_partial_fallback_full() {
  with_image_cache_diagnostics(|stats| stats.probe_partial_fallback_full += 1);
}

fn record_raster_pixmap_cache_hit() {
  with_image_cache_diagnostics(|stats| stats.raster_pixmap_cache_hits += 1);
}

fn record_raster_pixmap_cache_miss() {
  with_image_cache_diagnostics(|stats| stats.raster_pixmap_cache_misses += 1);
}

fn record_raster_pixmap_cache_bytes(bytes: usize) {
  with_image_cache_diagnostics(|stats| {
    stats.raster_pixmap_cache_bytes = stats.raster_pixmap_cache_bytes.max(bytes);
  });
}

const IMAGE_DECODE_DEADLINE_STRIDE: usize = 8192;

struct DeadlineCursor<'a> {
  inner: Cursor<&'a [u8]>,
  deadline_counter: usize,
}

impl<'a> DeadlineCursor<'a> {
  fn new(bytes: &'a [u8]) -> Self {
    Self {
      inner: Cursor::new(bytes),
      deadline_counter: 0,
    }
  }

  fn check_deadline(&mut self) -> io::Result<()> {
    check_active_periodic(
      &mut self.deadline_counter,
      IMAGE_DECODE_DEADLINE_STRIDE,
      RenderStage::Paint,
    )
    .map_err(|err| io::Error::new(io::ErrorKind::Other, err))
  }
}

impl<'a> Read for DeadlineCursor<'a> {
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    self.check_deadline()?;
    self.inner.read(buf)
  }
}

impl<'a> BufRead for DeadlineCursor<'a> {
  fn fill_buf(&mut self) -> io::Result<&[u8]> {
    self.check_deadline()?;
    self.inner.fill_buf()
  }

  fn consume(&mut self, amt: usize) {
    self.inner.consume(amt);
  }
}

impl<'a> Seek for DeadlineCursor<'a> {
  fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
    self.check_deadline()?;
    self.inner.seek(pos)
  }
}

enum AvifDecodeError {
  Timeout(RenderError),
  Image(image::ImageError),
}

impl From<RenderError> for AvifDecodeError {
  fn from(err: RenderError) -> Self {
    Self::Timeout(err)
  }
}

impl From<image::ImageError> for AvifDecodeError {
  fn from(err: image::ImageError) -> Self {
    Self::Image(err)
  }
}

#[derive(Clone)]
struct CacheEntry<V> {
  value: V,
  bytes: usize,
}

struct SizedLruCache<K, V> {
  inner: LruCache<K, CacheEntry<V>>,
  max_entries: Option<usize>,
  max_bytes: Option<usize>,
  current_bytes: usize,
}

impl<K: Eq + Hash, V> SizedLruCache<K, V> {
  fn new(max_entries: usize, max_bytes: usize) -> Self {
    Self {
      inner: LruCache::unbounded(),
      max_entries: (max_entries > 0).then_some(max_entries),
      max_bytes: (max_bytes > 0).then_some(max_bytes),
      current_bytes: 0,
    }
  }

  fn get_cloned<Q>(&mut self, key: &Q) -> Option<V>
  where
    V: Clone,
    K: Borrow<Q>,
    Q: Hash + Eq + ?Sized,
  {
    self.inner.get(key).map(|entry| entry.value.clone())
  }

  fn insert(&mut self, key: K, value: V, bytes: usize) {
    if let Some(entry) = self.inner.pop(&key) {
      self.current_bytes = self.current_bytes.saturating_sub(entry.bytes);
    }
    self.inner.put(key, CacheEntry { value, bytes });
    self.current_bytes = self.current_bytes.saturating_add(bytes);
    self.evict_if_needed();
  }

  fn evict_if_needed(&mut self) {
    while self
      .max_entries
      .is_some_and(|limit| self.inner.len() > limit)
      || self
        .max_bytes
        .is_some_and(|limit| self.current_bytes > limit)
    {
      if let Some((_key, entry)) = self.inner.pop_lru() {
        self.current_bytes = self.current_bytes.saturating_sub(entry.bytes);
      } else {
        break;
      }
    }
  }

  fn current_bytes(&self) -> usize {
    self.current_bytes
  }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct SvgPixmapKey {
  hash: u64,
  url_hash: u64,
  len: usize,
  width: u32,
  height: u32,
  device_pixel_ratio_bits: u32,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct RasterPixmapKey {
  url_hash: u64,
  len: usize,
  orientation: OrientationTransform,
  decorative: bool,
  target_width: u32,
  target_height: u32,
  quality_bits: u8,
}

fn raster_pixmap_key(
  url: &str,
  orientation: OrientationTransform,
  decorative: bool,
  target_width: u32,
  target_height: u32,
  quality_bits: u8,
) -> RasterPixmapKey {
  let mut url_hasher = DefaultHasher::new();
  url.hash(&mut url_hasher);
  RasterPixmapKey {
    url_hash: url_hasher.finish(),
    len: url.len(),
    orientation,
    decorative,
    target_width,
    target_height,
    quality_bits,
  }
}

fn raster_pixmap_quality_bits(quality: FilterQuality) -> u8 {
  match quality {
    FilterQuality::Nearest => 0,
    FilterQuality::Bilinear => 1,
    // tiny-skia currently exposes a small fixed set of qualities. Store a conservative default so
    // future additions don't break hashing behaviour.
    _ => 255,
  }
}

fn raster_pixmap_full_key(
  url: &str,
  orientation: OrientationTransform,
  decorative: bool,
) -> RasterPixmapKey {
  raster_pixmap_key(url, orientation, decorative, 0, 0, 0)
}

fn svg_pixmap_key(
  svg_content: &str,
  url: &str,
  device_pixel_ratio: f32,
  width: u32,
  height: u32,
) -> SvgPixmapKey {
  let mut content_hasher = DefaultHasher::new();
  svg_content.hash(&mut content_hasher);
  let mut url_hasher = DefaultHasher::new();
  url.hash(&mut url_hasher);
  SvgPixmapKey {
    hash: content_hasher.finish(),
    url_hash: url_hasher.finish(),
    len: svg_content.len(),
    width,
    height,
    device_pixel_ratio_bits: device_pixel_ratio.to_bits(),
  }
}

fn inline_svg_cache_key(svg_content: &str) -> String {
  let mut hasher = DefaultHasher::new();
  svg_content.hash(&mut hasher);
  format!("inline-svg:{:016x}:{}", hasher.finish(), svg_content.len())
}

fn svg_parse_fill_color(value: &str) -> Option<Rgba> {
  let trimmed = value.trim();
  if trimmed.is_empty() {
    return None;
  }
  if trimmed.eq_ignore_ascii_case("none") {
    return Some(Rgba::new(0, 0, 0, 0.0));
  }
  if trimmed.eq_ignore_ascii_case("currentColor") {
    return None;
  }
  if let Some(hex) = crate::style::defaults::parse_color_attribute(trimmed) {
    return Some(hex);
  }
  trimmed.parse::<csscolorparser::Color>().ok().map(|c| {
    Rgba::new(
      (c.r * 255.0).round() as u8,
      (c.g * 255.0).round() as u8,
      (c.b * 255.0).round() as u8,
      c.a as f32,
    )
  })
}

fn multiply_alpha(mut color: Rgba, alpha: f32) -> Rgba {
  if !alpha.is_finite() {
    return color;
  }
  color.a = (color.a * alpha).clamp(0.0, 1.0);
  color
}

fn build_tiny_skia_path_from_svg_path_data(
  data: &str,
  deadline_counter: &mut usize,
) -> std::result::Result<Option<tiny_skia::Path>, RenderError> {
  use svgtypes::PathParser;
  use svgtypes::PathSegment;
  use tiny_skia::PathBuilder;

  let mut pb = PathBuilder::new();
  let mut current = (0.0f32, 0.0f32);
  let mut subpath_start = (0.0f32, 0.0f32);
  let mut last_cubic_ctrl: Option<(f32, f32)> = None;
  let mut last_quad_ctrl: Option<(f32, f32)> = None;

  for segment in PathParser::from(data) {
    check_active_periodic(
      deadline_counter,
      IMAGE_DECODE_DEADLINE_STRIDE,
      RenderStage::Paint,
    )?;
    let seg = match segment {
      Ok(seg) => seg,
      Err(_) => return Ok(None),
    };
    match seg {
      PathSegment::MoveTo { abs, x, y } => {
        let (nx, ny) = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        pb.move_to(nx, ny);
        current = (nx, ny);
        subpath_start = current;
        last_cubic_ctrl = None;
        last_quad_ctrl = None;
      }
      PathSegment::LineTo { abs, x, y } => {
        let (nx, ny) = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        pb.line_to(nx, ny);
        current = (nx, ny);
        last_cubic_ctrl = None;
        last_quad_ctrl = None;
      }
      PathSegment::HorizontalLineTo { abs, x } => {
        let nx = if abs { x as f32 } else { current.0 + x as f32 };
        pb.line_to(nx, current.1);
        current.0 = nx;
        last_cubic_ctrl = None;
        last_quad_ctrl = None;
      }
      PathSegment::VerticalLineTo { abs, y } => {
        let ny = if abs { y as f32 } else { current.1 + y as f32 };
        pb.line_to(current.0, ny);
        current.1 = ny;
        last_cubic_ctrl = None;
        last_quad_ctrl = None;
      }
      PathSegment::CurveTo {
        abs,
        x1,
        y1,
        x2,
        y2,
        x,
        y,
      } => {
        let (cx1, cy1) = if abs {
          (x1 as f32, y1 as f32)
        } else {
          (current.0 + x1 as f32, current.1 + y1 as f32)
        };
        let (cx2, cy2) = if abs {
          (x2 as f32, y2 as f32)
        } else {
          (current.0 + x2 as f32, current.1 + y2 as f32)
        };
        let (nx, ny) = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        pb.cubic_to(cx1, cy1, cx2, cy2, nx, ny);
        current = (nx, ny);
        last_cubic_ctrl = Some((cx2, cy2));
        last_quad_ctrl = None;
      }
      PathSegment::SmoothCurveTo { abs, x2, y2, x, y } => {
        let (cx1, cy1) = match last_cubic_ctrl {
          Some((px, py)) => (2.0 * current.0 - px, 2.0 * current.1 - py),
          None => current,
        };
        let (cx2, cy2) = if abs {
          (x2 as f32, y2 as f32)
        } else {
          (current.0 + x2 as f32, current.1 + y2 as f32)
        };
        let (nx, ny) = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        pb.cubic_to(cx1, cy1, cx2, cy2, nx, ny);
        current = (nx, ny);
        last_cubic_ctrl = Some((cx2, cy2));
        last_quad_ctrl = None;
      }
      PathSegment::Quadratic { abs, x1, y1, x, y } => {
        let (cx1, cy1) = if abs {
          (x1 as f32, y1 as f32)
        } else {
          (current.0 + x1 as f32, current.1 + y1 as f32)
        };
        let (nx, ny) = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        pb.quad_to(cx1, cy1, nx, ny);
        current = (nx, ny);
        last_quad_ctrl = Some((cx1, cy1));
        last_cubic_ctrl = None;
      }
      PathSegment::SmoothQuadratic { abs, x, y } => {
        let (cx1, cy1) = match last_quad_ctrl {
          Some((px, py)) => (2.0 * current.0 - px, 2.0 * current.1 - py),
          None => current,
        };
        let (nx, ny) = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        pb.quad_to(cx1, cy1, nx, ny);
        current = (nx, ny);
        last_quad_ctrl = Some((cx1, cy1));
        last_cubic_ctrl = None;
      }
      PathSegment::EllipticalArc {
        abs,
        rx,
        ry,
        x_axis_rotation,
        large_arc,
        sweep,
        x,
        y,
      } => {
        let (nx, ny) = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };

        if !arc_to_cubic_beziers(
          &mut pb,
          current,
          (rx as f32).abs(),
          (ry as f32).abs(),
          x_axis_rotation as f32,
          large_arc,
          sweep,
          (nx, ny),
        ) {
          pb.line_to(nx, ny);
        }

        current = (nx, ny);
        last_cubic_ctrl = None;
        last_quad_ctrl = None;
      }
      PathSegment::ClosePath { .. } => {
        pb.close();
        current = subpath_start;
        last_cubic_ctrl = None;
        last_quad_ctrl = None;
      }
    }
  }

  Ok(pb.finish())
}

fn arc_to_cubic_beziers(
  pb: &mut tiny_skia::PathBuilder,
  start: (f32, f32),
  rx: f32,
  ry: f32,
  x_axis_rotation_deg: f32,
  large_arc: bool,
  sweep: bool,
  end: (f32, f32),
) -> bool {
  if !start.0.is_finite()
    || !start.1.is_finite()
    || !end.0.is_finite()
    || !end.1.is_finite()
    || !rx.is_finite()
    || !ry.is_finite()
    || !x_axis_rotation_deg.is_finite()
  {
    return false;
  }
  if (start.0 - end.0).abs() < f32::EPSILON && (start.1 - end.1).abs() < f32::EPSILON {
    return true;
  }
  if rx <= 0.0 || ry <= 0.0 {
    return false;
  }

  let x0 = start.0 as f64;
  let y0 = start.1 as f64;
  let x1 = end.0 as f64;
  let y1 = end.1 as f64;
  let mut rx = rx as f64;
  let mut ry = ry as f64;

  let phi = (x_axis_rotation_deg as f64).to_radians();
  let (sin_phi, cos_phi) = phi.sin_cos();

  let dx2 = (x0 - x1) * 0.5;
  let dy2 = (y0 - y1) * 0.5;
  let x1p = cos_phi * dx2 + sin_phi * dy2;
  let y1p = -sin_phi * dx2 + cos_phi * dy2;

  let rx_sq = rx * rx;
  let ry_sq = ry * ry;
  let x1p_sq = x1p * x1p;
  let y1p_sq = y1p * y1p;

  let lambda = (x1p_sq / rx_sq) + (y1p_sq / ry_sq);
  if lambda > 1.0 {
    let scale = lambda.sqrt();
    rx *= scale;
    ry *= scale;
  }

  let rx_sq = rx * rx;
  let ry_sq = ry * ry;
  let denom = rx_sq * y1p_sq + ry_sq * x1p_sq;
  if denom.abs() < f64::EPSILON {
    return false;
  }
  let mut num = rx_sq * ry_sq - rx_sq * y1p_sq - ry_sq * x1p_sq;
  if num < 0.0 {
    num = 0.0;
  }
  let sign = if large_arc == sweep { -1.0 } else { 1.0 };
  let coef = sign * (num / denom).sqrt();
  let cxp = coef * (rx * y1p) / ry;
  let cyp = coef * (-ry * x1p) / rx;

  let cx = cos_phi * cxp - sin_phi * cyp + (x0 + x1) * 0.5;
  let cy = sin_phi * cxp + cos_phi * cyp + (y0 + y1) * 0.5;

  let v1x = (x1p - cxp) / rx;
  let v1y = (y1p - cyp) / ry;
  let v2x = (-x1p - cxp) / rx;
  let v2y = (-y1p - cyp) / ry;

  fn vector_angle(ux: f64, uy: f64, vx: f64, vy: f64) -> f64 {
    let dot = ux * vx + uy * vy;
    let det = ux * vy - uy * vx;
    det.atan2(dot)
  }

  let mut start_angle = vector_angle(1.0, 0.0, v1x, v1y);
  let mut delta_angle = vector_angle(v1x, v1y, v2x, v2y);

  if !sweep && delta_angle > 0.0 {
    delta_angle -= std::f64::consts::TAU;
  } else if sweep && delta_angle < 0.0 {
    delta_angle += std::f64::consts::TAU;
  }

  // Split the arc into segments no larger than 90 degrees.
  let segments = (delta_angle.abs() / (std::f64::consts::FRAC_PI_2))
    .ceil()
    .max(1.0) as usize;
  let seg_angle = delta_angle / segments as f64;

  for _ in 0..segments {
    let theta1 = start_angle;
    let theta2 = theta1 + seg_angle;
    start_angle = theta2;

    let (sin_t1, cos_t1) = theta1.sin_cos();
    let (sin_t2, cos_t2) = theta2.sin_cos();

    let alpha = (4.0 / 3.0) * ((theta2 - theta1) * 0.25).tan();

    let x1 = cos_t1;
    let y1 = sin_t1;
    let x2 = cos_t2;
    let y2 = sin_t2;

    let cp1x = x1 - alpha * y1;
    let cp1y = y1 + alpha * x1;
    let cp2x = x2 + alpha * y2;
    let cp2y = y2 - alpha * x2;

    let map = |x: f64, y: f64| -> (f64, f64) {
      (
        cx + cos_phi * rx * x - sin_phi * ry * y,
        cy + sin_phi * rx * x + cos_phi * ry * y,
      )
    };

    let (c1x, c1y) = map(cp1x, cp1y);
    let (c2x, c2y) = map(cp2x, cp2y);
    let (ex, ey) = map(x2, y2);
    pb.cubic_to(
      c1x as f32, c1y as f32, c2x as f32, c2y as f32, ex as f32, ey as f32,
    );
  }

  true
}

fn try_render_simple_svg_pixmap(
  svg_content: &str,
  render_width: u32,
  render_height: u32,
) -> std::result::Result<Option<Pixmap>, RenderError> {
  use tiny_skia::FillRule;
  use tiny_skia::Paint;

  if render_width == 0 || render_height == 0 {
    return Ok(None);
  }

  let mut deadline_counter = 0usize;
  check_active_periodic(
    &mut deadline_counter,
    IMAGE_DECODE_DEADLINE_STRIDE,
    RenderStage::Paint,
  )?;

  let doc = match Document::parse(svg_content) {
    Ok(doc) => doc,
    Err(_) => return Ok(None),
  };
  let root = doc.root_element();
  let has_view_box_attr = root.attribute("viewBox").is_some();
  if !root.tag_name().name().eq_ignore_ascii_case("svg") {
    return Ok(None);
  }

  for node in root.descendants().filter(|n| n.is_element()) {
    check_active_periodic(
      &mut deadline_counter,
      IMAGE_DECODE_DEADLINE_STRIDE,
      RenderStage::Paint,
    )?;
    let name = node.tag_name().name();
    let allowed = matches!(name, "svg" | "g" | "path" | "title" | "desc" | "metadata");
    if !allowed {
      return Ok(None);
    }
    if node.attribute("transform").is_some()
      || node.attribute("filter").is_some()
      || node.attribute("mask").is_some()
      || node.attribute("clip-path").is_some()
      || node.attribute("style").is_some()
    {
      return Ok(None);
    }
    if name.eq_ignore_ascii_case("path") {
      if node.attribute("d").is_none() {
        return Ok(None);
      }
      if node
        .attribute("stroke")
        .is_some_and(|v| !v.trim().eq_ignore_ascii_case("none"))
      {
        return Ok(None);
      }
      if node.attribute("stroke-width").is_some()
        || node.attribute("stroke-linecap").is_some()
        || node.attribute("stroke-linejoin").is_some()
        || node.attribute("stroke-miterlimit").is_some()
        || node.attribute("stroke-dasharray").is_some()
        || node.attribute("stroke-dashoffset").is_some()
      {
        return Ok(None);
      }
    }
  }

  let view_box = root
    .attribute("viewBox")
    .and_then(parse_svg_view_box)
    .or_else(|| {
      let w = root.attribute("width").and_then(parse_svg_length_px)?;
      let h = root.attribute("height").and_then(parse_svg_length_px)?;
      Some(SvgViewBox {
        min_x: 0.0,
        min_y: 0.0,
        width: w,
        height: h,
      })
    })
    .unwrap_or(SvgViewBox {
      min_x: 0.0,
      min_y: 0.0,
      width: render_width as f32,
      height: render_height as f32,
    });
  if !(view_box.width.is_finite()
    && view_box.height.is_finite()
    && view_box.width > 0.0
    && view_box.height > 0.0)
  {
    return Ok(None);
  }

  let mut preserve = SvgPreserveAspectRatio::parse(root.attribute("preserveAspectRatio"));
  // Without a viewBox, the viewport and user coordinate systems are the same, so the
  // viewBox-to-viewport preserveAspectRatio mapping must be ignored (equivalent to `none`).
  if !has_view_box_attr {
    preserve.none = true;
  }
  let transform = map_svg_aspect_ratio(
    view_box,
    preserve,
    render_width as f32,
    render_height as f32,
  );

  let Some(mut pixmap) = new_pixmap(render_width, render_height) else {
    return Ok(None);
  };
  for node in root.descendants().filter(|n| n.is_element()) {
    check_active_periodic(
      &mut deadline_counter,
      IMAGE_DECODE_DEADLINE_STRIDE,
      RenderStage::Paint,
    )?;
    if !node.tag_name().name().eq_ignore_ascii_case("path") {
      continue;
    }
    let Some(d) = node.attribute("d") else {
      return Ok(None);
    };
    let path = match build_tiny_skia_path_from_svg_path_data(d, &mut deadline_counter)? {
      Some(path) => path,
      None => return Ok(None),
    };

    let fill = node.attribute("fill");
    let mut color = match fill {
      Some(v) => match svg_parse_fill_color(v) {
        Some(color) => color,
        None => return Ok(None),
      },
      None => Rgba::new(0, 0, 0, 1.0),
    };
    if color.a <= 0.0 {
      continue;
    }

    if let Some(opacity_raw) = node.attribute("opacity") {
      if let Ok(alpha) = opacity_raw.trim().parse::<f32>() {
        color = multiply_alpha(color, alpha);
      }
    }
    if let Some(opacity_raw) = node.attribute("fill-opacity") {
      if let Ok(alpha) = opacity_raw.trim().parse::<f32>() {
        color = multiply_alpha(color, alpha);
      }
    }

    let fill_rule = match node.attribute("fill-rule").map(|v| v.trim()) {
      None => FillRule::Winding,
      Some(v) if v.eq_ignore_ascii_case("nonzero") => FillRule::Winding,
      Some(v) if v.eq_ignore_ascii_case("evenodd") => FillRule::EvenOdd,
      Some(_) => return Ok(None),
    };

    let mut paint = Paint::default();
    paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
    paint.anti_alias = true;
    pixmap.fill_path(&path, &paint, fill_rule, transform, None);
  }

  Ok(Some(pixmap))
}

// ============================================================================
// CachedImage
// ============================================================================

/// Decoded image plus orientation metadata.
pub struct CachedImage {
  pub image: Arc<DynamicImage>,
  pub orientation: Option<OrientationTransform>,
  /// Resolution in image pixels per CSS px (dppx) when provided by metadata.
  pub resolution: Option<f32>,
  /// Whether this image originated from a vector source (SVG).
  pub is_vector: bool,
  /// Intrinsic aspect ratio when known. SVGs that opt out of aspect-ratio preservation keep this
  /// as `None` and set `aspect_ratio_none` to true.
  pub intrinsic_ratio: Option<f32>,
  /// True when the resource explicitly disables aspect-ratio preservation (e.g., SVG
  /// `preserveAspectRatio="none"`).
  pub aspect_ratio_none: bool,
  /// Raw SVG markup when the image originated from a vector source.
  pub svg_content: Option<Arc<str>>,
}

impl CachedImage {
  pub fn dimensions(&self) -> (u32, u32) {
    self.image.dimensions()
  }

  pub fn width(&self) -> u32 {
    self.image.width()
  }

  pub fn height(&self) -> u32 {
    self.image.height()
  }

  pub fn oriented_dimensions(&self, transform: OrientationTransform) -> (u32, u32) {
    let (w, h) = self.dimensions();
    transform.oriented_dimensions(w, h)
  }

  /// Computes CSS pixel dimensions after applying orientation and the provided image-resolution.
  pub fn css_dimensions(
    &self,
    transform: OrientationTransform,
    resolution: &ImageResolution,
    device_pixel_ratio: f32,
    override_resolution: Option<f32>,
  ) -> Option<(f32, f32)> {
    let (w, h) = self.oriented_dimensions(transform);
    if w == 0 || h == 0 {
      return None;
    }
    if self.is_vector {
      return Some((w as f32, h as f32));
    }
    let used = resolution.used_resolution(override_resolution, self.resolution, device_pixel_ratio);
    if used <= 0.0 || !used.is_finite() {
      return None;
    }
    Some((w as f32 / used, h as f32 / used))
  }

  /// Intrinsic aspect ratio, adjusted for EXIF orientation when present.
  pub fn intrinsic_ratio(&self, transform: OrientationTransform) -> Option<f32> {
    if self.aspect_ratio_none {
      return None;
    }

    let mut ratio = self.intrinsic_ratio;
    if ratio.is_none() {
      let (w, h) = self.oriented_dimensions(transform);
      if h > 0 {
        ratio = Some(w as f32 / h as f32);
      }
    }

    if let Some(r) = ratio {
      if transform.quarter_turns % 2 == 1 {
        return Some(1.0 / r);
      }
    }

    ratio
  }

  pub fn to_oriented_rgba(&self, transform: OrientationTransform) -> RgbaImage {
    let mut rgba = self.image.to_rgba8();

    match transform.quarter_turns % 4 {
      0 => {}
      1 => rgba = imageops::rotate90(&rgba),
      2 => rgba = imageops::rotate180(&rgba),
      3 => rgba = imageops::rotate270(&rgba),
      _ => {}
    }

    if transform.flip_x {
      rgba = imageops::flip_horizontal(&rgba);
    }

    rgba
  }
}

#[derive(Debug, Clone)]
pub struct CachedImageMetadata {
  pub width: u32,
  pub height: u32,
  pub orientation: Option<OrientationTransform>,
  pub resolution: Option<f32>,
  pub is_vector: bool,
  pub intrinsic_ratio: Option<f32>,
  pub aspect_ratio_none: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
struct DiskCachedOrientationTransformV1 {
  quarter_turns: u8,
  flip_x: bool,
}

impl From<OrientationTransform> for DiskCachedOrientationTransformV1 {
  fn from(value: OrientationTransform) -> Self {
    Self {
      quarter_turns: value.quarter_turns,
      flip_x: value.flip_x,
    }
  }
}

impl From<DiskCachedOrientationTransformV1> for OrientationTransform {
  fn from(value: DiskCachedOrientationTransformV1) -> Self {
    Self {
      quarter_turns: value.quarter_turns,
      flip_x: value.flip_x,
    }
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct DiskCachedImageProbeMetadataV1 {
  width: u32,
  height: u32,
  orientation: Option<DiskCachedOrientationTransformV1>,
  resolution: Option<f32>,
  is_vector: bool,
  intrinsic_ratio: Option<f32>,
  aspect_ratio_none: bool,
}

impl From<&CachedImageMetadata> for DiskCachedImageProbeMetadataV1 {
  fn from(meta: &CachedImageMetadata) -> Self {
    Self {
      width: meta.width,
      height: meta.height,
      orientation: meta.orientation.map(Into::into),
      resolution: meta.resolution,
      is_vector: meta.is_vector,
      intrinsic_ratio: meta.intrinsic_ratio,
      aspect_ratio_none: meta.aspect_ratio_none,
    }
  }
}

impl From<DiskCachedImageProbeMetadataV1> for CachedImageMetadata {
  fn from(meta: DiskCachedImageProbeMetadataV1) -> Self {
    Self {
      width: meta.width,
      height: meta.height,
      orientation: meta.orientation.map(Into::into),
      resolution: meta.resolution,
      is_vector: meta.is_vector,
      intrinsic_ratio: meta.intrinsic_ratio,
      aspect_ratio_none: meta.aspect_ratio_none,
    }
  }
}

fn encode_probe_metadata_for_disk(meta: &CachedImageMetadata) -> Option<Vec<u8>> {
  serde_json::to_vec(&DiskCachedImageProbeMetadataV1::from(meta)).ok()
}

fn decode_probe_metadata_from_disk(bytes: &[u8]) -> Option<CachedImageMetadata> {
  serde_json::from_slice::<DiskCachedImageProbeMetadataV1>(bytes)
    .ok()
    .map(Into::into)
}

impl CachedImageMetadata {
  pub fn dimensions(&self) -> (u32, u32) {
    (self.width, self.height)
  }

  pub fn oriented_dimensions(&self, transform: OrientationTransform) -> (u32, u32) {
    transform.oriented_dimensions(self.width, self.height)
  }

  pub fn css_dimensions(
    &self,
    transform: OrientationTransform,
    resolution: &ImageResolution,
    device_pixel_ratio: f32,
    override_resolution: Option<f32>,
  ) -> Option<(f32, f32)> {
    let (w, h) = self.oriented_dimensions(transform);
    if w == 0 || h == 0 {
      return None;
    }
    if self.is_vector {
      return Some((w as f32, h as f32));
    }
    let used = resolution.used_resolution(override_resolution, self.resolution, device_pixel_ratio);
    if used <= 0.0 || !used.is_finite() {
      return None;
    }
    Some((w as f32 / used, h as f32 / used))
  }

  pub fn intrinsic_ratio(&self, transform: OrientationTransform) -> Option<f32> {
    if self.aspect_ratio_none {
      return None;
    }

    let mut ratio = self.intrinsic_ratio;
    if ratio.is_none() {
      let (w, h) = self.oriented_dimensions(transform);
      if h > 0 {
        ratio = Some(w as f32 / h as f32);
      }
    }

    if let Some(r) = ratio {
      if transform.quarter_turns % 2 == 1 {
        return Some(1.0 / r);
      }
    }

    ratio
  }
}

impl From<&CachedImage> for CachedImageMetadata {
  fn from(image: &CachedImage) -> Self {
    let (width, height) = image.dimensions();
    Self {
      width,
      height,
      orientation: image.orientation,
      resolution: image.resolution,
      is_vector: image.is_vector,
      intrinsic_ratio: image.intrinsic_ratio,
      aspect_ratio_none: image.aspect_ratio_none,
    }
  }
}

fn is_about_url(url: &str) -> bool {
  let trimmed = url.trim_start();
  trimmed
    .get(..6)
    .is_some_and(|prefix| prefix.eq_ignore_ascii_case("about:"))
}

fn about_url_placeholder_image() -> Arc<CachedImage> {
  static PLACEHOLDER: OnceLock<Arc<CachedImage>> = OnceLock::new();
  Arc::clone(PLACEHOLDER.get_or_init(|| {
    // A 1×1 fully transparent RGBA buffer so layout/paint can proceed deterministically for
    // non-fetchable `about:` URLs (e.g. `about:blank`).
    let img = RgbaImage::new(1, 1);
    Arc::new(CachedImage {
      image: Arc::new(DynamicImage::ImageRgba8(img)),
      orientation: None,
      resolution: None,
      is_vector: false,
      intrinsic_ratio: None,
      aspect_ratio_none: false,
      svg_content: None,
    })
  }))
}

fn about_url_placeholder_metadata() -> Arc<CachedImageMetadata> {
  static PLACEHOLDER_META: OnceLock<Arc<CachedImageMetadata>> = OnceLock::new();
  Arc::clone(
    PLACEHOLDER_META
      .get_or_init(|| Arc::new(CachedImageMetadata::from(&*about_url_placeholder_image()))),
  )
}

fn about_url_placeholder_pixmap() -> Arc<tiny_skia::Pixmap> {
  static PLACEHOLDER_PIXMAP: OnceLock<Arc<tiny_skia::Pixmap>> = OnceLock::new();
  Arc::clone(PLACEHOLDER_PIXMAP.get_or_init(|| {
    // new_pixmap returns a zeroed RGBA buffer, which is already a premultiplied fully-transparent
    // pixel for tiny-skia.
    Arc::new(new_pixmap(1, 1).expect("1x1 pixmap allocation must succeed"))
  }))
}

fn status_is_http_success(status: Option<u16>) -> bool {
  matches!(status, Some(200..=299))
}

fn is_empty_body_error_for_image(error: &Error) -> bool {
  matches!(
    error,
    Error::Resource(res)
      if status_is_http_success(res.status) && res.message.contains("empty HTTP response body")
  )
}

fn payload_looks_like_markup_but_not_svg(bytes: &[u8]) -> bool {
  let sample = &bytes[..bytes.len().min(256)];
  let mut i = 0;
  if sample.starts_with(b"\xef\xbb\xbf") {
    i = 3;
  }
  while i < sample.len() && sample[i].is_ascii_whitespace() {
    i += 1;
  }
  let rest = &sample[i..];
  if rest.is_empty() || rest[0] != b'<' {
    return false;
  }

  // Accept common SVG/XML prologs so that SVG documents aren't mistaken for HTML.
  if rest.len() >= 4
    && rest[0] == b'<'
    && rest[1].to_ascii_lowercase() == b's'
    && rest[2].to_ascii_lowercase() == b'v'
    && rest[3].to_ascii_lowercase() == b'g'
  {
    return false;
  }
  if rest.len() >= 5
    && rest[0] == b'<'
    && rest[1] == b'?'
    && rest[2].to_ascii_lowercase() == b'x'
    && rest[3].to_ascii_lowercase() == b'm'
    && rest[4].to_ascii_lowercase() == b'l'
  {
    return false;
  }
  if rest.len() >= 10
    && rest[0] == b'<'
    && rest[1] == b'!'
    && rest[2].to_ascii_lowercase() == b'd'
    && rest[3].to_ascii_lowercase() == b'o'
    && rest[4].to_ascii_lowercase() == b'c'
    && rest[5].to_ascii_lowercase() == b't'
    && rest[6].to_ascii_lowercase() == b'y'
    && rest[7].to_ascii_lowercase() == b'p'
    && rest[8].to_ascii_lowercase() == b'e'
  {
    let mut j = 9;
    while j < rest.len() && rest[j].is_ascii_whitespace() {
      j += 1;
    }
    if rest.len().saturating_sub(j) >= 3
      && rest[j].to_ascii_lowercase() == b's'
      && rest[j + 1].to_ascii_lowercase() == b'v'
      && rest[j + 2].to_ascii_lowercase() == b'g'
    {
      return false;
    }
  }

  true
}

// ============================================================================
// ImageCache
// ============================================================================

/// Configuration for [`ImageCache`].
#[derive(Debug, Clone, Copy)]
pub struct ImageCacheConfig {
  /// Maximum number of decoded pixels (width * height). `0` disables the limit.
  pub max_decoded_pixels: u64,
  /// Maximum allowed width or height for a decoded image. `0` disables the limit.
  pub max_decoded_dimension: u32,
  /// Maximum number of decoded images kept in memory (`0` disables eviction by count).
  pub max_cached_images: usize,
  /// Maximum estimated bytes of decoded images kept in memory (`0` disables eviction by size).
  pub max_cached_image_bytes: usize,
  /// Maximum number of rasterized SVG pixmaps kept in memory (`0` disables eviction by count).
  pub max_cached_svg_pixmaps: usize,
  /// Maximum estimated bytes of cached SVG pixmaps (`0` disables eviction by size).
  pub max_cached_svg_bytes: usize,
  /// Maximum number of cached premultiplied raster pixmaps (`0` disables eviction by count).
  pub max_cached_raster_pixmaps: usize,
  /// Maximum estimated bytes of cached raster pixmaps (`0` disables eviction by size).
  pub max_cached_raster_bytes: usize,
}

impl Default for ImageCacheConfig {
  fn default() -> Self {
    const DEFAULT_MAX_RASTER_PIXMAP_CACHE_ITEMS: usize = 256;
    const DEFAULT_MAX_RASTER_PIXMAP_CACHE_BYTES: usize = 128 * 1024 * 1024;

    let max_cached_raster_pixmaps = std::env::var("FASTR_IMAGE_RASTER_PIXMAP_CACHE_ITEMS")
      .ok()
      .and_then(|v| v.trim().parse::<usize>().ok())
      .unwrap_or(DEFAULT_MAX_RASTER_PIXMAP_CACHE_ITEMS);
    let max_cached_raster_bytes = std::env::var("FASTR_IMAGE_RASTER_PIXMAP_CACHE_BYTES")
      .ok()
      .and_then(|v| v.trim().parse::<usize>().ok())
      .unwrap_or(DEFAULT_MAX_RASTER_PIXMAP_CACHE_BYTES);

    Self {
      max_decoded_pixels: 100_000_000,
      max_decoded_dimension: 32768,
      max_cached_images: 256,
      max_cached_image_bytes: 256 * 1024 * 1024,
      max_cached_svg_pixmaps: 128,
      max_cached_svg_bytes: 128 * 1024 * 1024,
      max_cached_raster_pixmaps,
      max_cached_raster_bytes,
    }
  }
}

impl ImageCacheConfig {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn with_max_decoded_pixels(mut self, max: u64) -> Self {
    self.max_decoded_pixels = max;
    self
  }

  pub fn with_max_decoded_dimension(mut self, max: u32) -> Self {
    self.max_decoded_dimension = max;
    self
  }

  pub fn with_max_cached_images(mut self, max: usize) -> Self {
    self.max_cached_images = max;
    self
  }

  pub fn with_max_cached_image_bytes(mut self, max: usize) -> Self {
    self.max_cached_image_bytes = max;
    self
  }

  pub fn with_max_cached_svg_pixmaps(mut self, max: usize) -> Self {
    self.max_cached_svg_pixmaps = max;
    self
  }

  pub fn with_max_cached_svg_bytes(mut self, max: usize) -> Self {
    self.max_cached_svg_bytes = max;
    self
  }

  pub fn with_max_cached_raster_pixmaps(mut self, max: usize) -> Self {
    self.max_cached_raster_pixmaps = max;
    self
  }

  pub fn with_max_cached_raster_bytes(mut self, max: usize) -> Self {
    self.max_cached_raster_bytes = max;
    self
  }
}

#[derive(Clone)]
enum SharedImageResult {
  Success(Arc<CachedImage>),
  Error(Error),
}

impl SharedImageResult {
  fn as_result(&self) -> Result<Arc<CachedImage>> {
    match self {
      Self::Success(img) => Ok(Arc::clone(img)),
      Self::Error(err) => Err(err.clone()),
    }
  }
}

struct DecodeInFlight {
  result: Mutex<Option<SharedImageResult>>,
  cv: Condvar,
}

impl DecodeInFlight {
  fn new() -> Self {
    Self {
      result: Mutex::new(None),
      cv: Condvar::new(),
    }
  }

  fn set(&self, result: SharedImageResult) {
    let mut slot = self
      .result
      .lock()
      .unwrap_or_else(|poisoned| poisoned.into_inner());
    *slot = Some(result);
    self.cv.notify_all();
  }

  fn wait(&self, _url: &str) -> Result<Arc<CachedImage>> {
    let mut guard = self
      .result
      .lock()
      .unwrap_or_else(|poisoned| poisoned.into_inner());
    let deadline = render_control::active_deadline().filter(|d| d.is_enabled());
    while guard.is_none() {
      if let Some(deadline) = deadline.as_ref() {
        deadline.check(RenderStage::Paint).map_err(Error::Render)?;
        let wait_for = if deadline.timeout_limit().is_some() {
          match deadline.remaining_timeout() {
            Some(remaining) if !remaining.is_zero() => remaining.min(Duration::from_millis(10)),
            _ => {
              return Err(Error::Render(RenderError::Timeout {
                stage: RenderStage::Paint,
                elapsed: deadline.elapsed(),
              }));
            }
          }
        } else {
          Duration::from_millis(10)
        };
        guard = self
          .cv
          .wait_timeout(guard, wait_for)
          .unwrap_or_else(|poisoned| poisoned.into_inner())
          .0;
      } else {
        guard = self
          .cv
          .wait(guard)
          .unwrap_or_else(|poisoned| poisoned.into_inner());
      }
    }
    guard.as_ref().unwrap().as_result()
  }
}

#[derive(Clone)]
enum SharedMetaResult {
  Success(Arc<CachedImageMetadata>),
  Error(Error),
}

impl SharedMetaResult {
  fn as_result(&self) -> Result<Arc<CachedImageMetadata>> {
    match self {
      Self::Success(meta) => Ok(Arc::clone(meta)),
      Self::Error(err) => Err(err.clone()),
    }
  }
}

struct ProbeInFlight {
  result: Mutex<Option<SharedMetaResult>>,
  cv: Condvar,
}

impl ProbeInFlight {
  fn new() -> Self {
    Self {
      result: Mutex::new(None),
      cv: Condvar::new(),
    }
  }

  fn set(&self, result: SharedMetaResult) {
    let mut slot = self
      .result
      .lock()
      .unwrap_or_else(|poisoned| poisoned.into_inner());
    *slot = Some(result);
    self.cv.notify_all();
  }

  fn wait(&self, _url: &str) -> Result<Arc<CachedImageMetadata>> {
    let mut guard = self
      .result
      .lock()
      .unwrap_or_else(|poisoned| poisoned.into_inner());
    let deadline = render_control::active_deadline().filter(|d| d.is_enabled());
    while guard.is_none() {
      if let Some(deadline) = deadline.as_ref() {
        deadline.check(RenderStage::Paint).map_err(Error::Render)?;
        let wait_for = if deadline.timeout_limit().is_some() {
          match deadline.remaining_timeout() {
            Some(remaining) if !remaining.is_zero() => remaining.min(Duration::from_millis(10)),
            _ => {
              return Err(Error::Render(RenderError::Timeout {
                stage: RenderStage::Paint,
                elapsed: deadline.elapsed(),
              }));
            }
          }
        } else {
          Duration::from_millis(10)
        };
        guard = self
          .cv
          .wait_timeout(guard, wait_for)
          .unwrap_or_else(|poisoned| poisoned.into_inner())
          .0;
      } else {
        guard = self
          .cv
          .wait(guard)
          .unwrap_or_else(|poisoned| poisoned.into_inner());
      }
    }
    guard.as_ref().unwrap().as_result()
  }
}

/// Cache for loaded images
///
/// `ImageCache` provides in-memory caching of decoded images, with support for
/// loading from URLs, files, and data URLs. It uses a [`ResourceFetcher`] for
/// the actual byte fetching, allowing custom fetching strategies (caching,
/// mocking, etc.) to be injected.
///
/// # Example
///
/// ```rust,no_run
/// # use fastrender::image_loader::ImageCache;
/// # use fastrender::resource::HttpFetcher;
/// # use std::sync::Arc;
/// # fn main() -> fastrender::Result<()> {
///
/// let fetcher = Arc::new(HttpFetcher::new());
/// let cache = ImageCache::with_fetcher(fetcher);
/// let image = cache.load("https://example.com/image.png")?;
/// # Ok(())
/// # }
/// ```
pub struct ImageCache {
  /// In-memory cache of decoded images (keyed by resolved URL)
  cache: Arc<Mutex<SizedLruCache<String, Arc<CachedImage>>>>,
  /// In-flight decodes keyed by resolved URL to de-duplicate concurrent loads.
  in_flight: Arc<Mutex<HashMap<String, Arc<DecodeInFlight>>>>,
  /// In-memory cache of probed metadata (keyed by resolved URL).
  meta_cache: Arc<Mutex<HashMap<String, Arc<CachedImageMetadata>>>>,
  /// Raw resources captured during metadata probes to avoid duplicate fetches between layout and paint.
  raw_cache: Arc<Mutex<HashMap<String, Arc<FetchedResource>>>>,
  /// In-flight probes keyed by resolved URL to de-duplicate concurrent metadata loads.
  meta_in_flight: Arc<Mutex<HashMap<String, Arc<ProbeInFlight>>>>,
  /// In-memory cache of rendered inline SVG pixmaps keyed by (hash, size).
  svg_pixmap_cache: Arc<Mutex<SizedLruCache<SvgPixmapKey, Arc<tiny_skia::Pixmap>>>>,
  /// In-memory cache of premultiplied raster pixmaps keyed by URL + orientation.
  raster_pixmap_cache: Arc<Mutex<SizedLruCache<RasterPixmapKey, Arc<tiny_skia::Pixmap>>>>,
  /// Base URL for resolving relative image sources
  base_url: Option<String>,
  /// Resource fetcher for loading bytes from URLs
  fetcher: Arc<dyn ResourceFetcher>,
  /// Decode limits.
  config: ImageCacheConfig,
  /// Optional diagnostics sink for recording fetch failures.
  diagnostics: Option<Arc<Mutex<RenderDiagnostics>>>,
  /// Optional resource context (policy + diagnostics).
  resource_context: Option<ResourceContext>,
}

struct DecodeInFlightOwnerGuard<'a> {
  cache: &'a ImageCache,
  url: &'a str,
  flight: Arc<DecodeInFlight>,
  finished: bool,
}

impl<'a> DecodeInFlightOwnerGuard<'a> {
  fn new(cache: &'a ImageCache, url: &'a str, flight: Arc<DecodeInFlight>) -> Self {
    Self {
      cache,
      url,
      flight,
      finished: false,
    }
  }

  fn finish(&mut self, result: SharedImageResult) {
    if self.finished {
      return;
    }
    self.finished = true;
    self.cache.finish_inflight(self.url, &self.flight, result);
  }
}

impl Drop for DecodeInFlightOwnerGuard<'_> {
  fn drop(&mut self) {
    if self.finished {
      return;
    }

    self.finished = true;
    let err = Error::Image(ImageError::LoadFailed {
      url: self.url.to_string(),
      reason: "in-flight image decode owner dropped without resolving".to_string(),
    });
    self.cache.record_image_error(self.url, &err);
    self
      .cache
      .finish_inflight(self.url, &self.flight, SharedImageResult::Error(err));
  }
}

struct ProbeInFlightOwnerGuard<'a> {
  cache: &'a ImageCache,
  url: &'a str,
  flight: Arc<ProbeInFlight>,
  finished: bool,
}

impl<'a> ProbeInFlightOwnerGuard<'a> {
  fn new(cache: &'a ImageCache, url: &'a str, flight: Arc<ProbeInFlight>) -> Self {
    Self {
      cache,
      url,
      flight,
      finished: false,
    }
  }

  fn finish(&mut self, result: SharedMetaResult) {
    if self.finished {
      return;
    }
    self.finished = true;
    self
      .cache
      .finish_meta_inflight(self.url, &self.flight, result);
  }
}

impl Drop for ProbeInFlightOwnerGuard<'_> {
  fn drop(&mut self) {
    if self.finished {
      return;
    }

    self.finished = true;
    let err = Error::Image(ImageError::LoadFailed {
      url: self.url.to_string(),
      reason: "in-flight image probe owner dropped without resolving".to_string(),
    });
    self.cache.record_image_error(self.url, &err);
    self
      .cache
      .finish_meta_inflight(self.url, &self.flight, SharedMetaResult::Error(err));
  }
}

impl ImageCache {
  /// Create a new ImageCache with the default HTTP fetcher
  pub fn new() -> Self {
    Self::with_config(ImageCacheConfig::default())
  }

  /// Create a new ImageCache with a custom fetcher
  pub fn with_fetcher(fetcher: Arc<dyn ResourceFetcher>) -> Self {
    Self::with_fetcher_and_config(fetcher, ImageCacheConfig::default())
  }

  /// Create a new ImageCache with the default HTTP fetcher and custom limits.
  pub fn with_config(config: ImageCacheConfig) -> Self {
    Self::with_base_url_fetcher_and_config(
      None,
      Arc::new(CachingFetcher::with_config(
        HttpFetcher::new(),
        CachingFetcherConfig::default(),
      )),
      config,
    )
  }

  /// Create a new ImageCache with a custom fetcher and limits.
  pub fn with_fetcher_and_config(
    fetcher: Arc<dyn ResourceFetcher>,
    config: ImageCacheConfig,
  ) -> Self {
    Self::with_base_url_fetcher_and_config(None, fetcher, config)
  }

  /// Create a new ImageCache with a base URL and the default HTTP fetcher
  pub fn with_base_url(base_url: String) -> Self {
    Self::with_base_url_and_config(base_url, ImageCacheConfig::default())
  }

  /// Create a new ImageCache with a base URL, default fetcher, and custom limits.
  pub fn with_base_url_and_config(base_url: String, config: ImageCacheConfig) -> Self {
    Self::with_base_url_fetcher_and_config(
      Some(base_url),
      Arc::new(CachingFetcher::with_config(
        HttpFetcher::new(),
        CachingFetcherConfig::default(),
      )),
      config,
    )
  }

  /// Create a new ImageCache with both a base URL and a custom fetcher
  pub fn with_base_url_and_fetcher(base_url: String, fetcher: Arc<dyn ResourceFetcher>) -> Self {
    Self::with_base_url_fetcher_and_config(Some(base_url), fetcher, ImageCacheConfig::default())
  }

  /// Create a new ImageCache with a base URL, custom fetcher, and limits.
  pub fn with_base_url_and_fetcher_and_config(
    base_url: String,
    fetcher: Arc<dyn ResourceFetcher>,
    config: ImageCacheConfig,
  ) -> Self {
    Self::with_base_url_fetcher_and_config(Some(base_url), fetcher, config)
  }

  fn with_base_url_fetcher_and_config(
    base_url: Option<String>,
    fetcher: Arc<dyn ResourceFetcher>,
    config: ImageCacheConfig,
  ) -> Self {
    Self {
      cache: Arc::new(Mutex::new(SizedLruCache::new(
        config.max_cached_images,
        config.max_cached_image_bytes,
      ))),
      in_flight: Arc::new(Mutex::new(HashMap::new())),
      meta_cache: Arc::new(Mutex::new(HashMap::new())),
      raw_cache: Arc::new(Mutex::new(HashMap::new())),
      meta_in_flight: Arc::new(Mutex::new(HashMap::new())),
      svg_pixmap_cache: Arc::new(Mutex::new(SizedLruCache::new(
        config.max_cached_svg_pixmaps,
        config.max_cached_svg_bytes,
      ))),
      raster_pixmap_cache: Arc::new(Mutex::new(SizedLruCache::new(
        config.max_cached_raster_pixmaps,
        config.max_cached_raster_bytes,
      ))),
      base_url,
      fetcher,
      config,
      diagnostics: None,
      resource_context: None,
    }
  }

  /// Sets or replaces the base URL used to resolve relative image sources.
  pub fn set_base_url(&mut self, base_url: impl Into<String>) {
    self.base_url = Some(base_url.into());
  }

  /// Clears any previously configured base URL.
  pub fn clear_base_url(&mut self) {
    self.base_url = None;
  }

  /// Returns the configured base URL for resolving relative paths.
  pub fn base_url(&self) -> Option<String> {
    self.base_url.clone()
  }

  /// Set the active resource context for policy and diagnostics.
  pub fn set_resource_context(&mut self, context: Option<ResourceContext>) {
    self.resource_context = context;
  }

  /// Retrieve the current resource context.
  pub fn resource_context(&self) -> Option<ResourceContext> {
    self.resource_context.clone()
  }

  /// Sets the resource fetcher
  pub fn set_fetcher(&mut self, fetcher: Arc<dyn ResourceFetcher>) {
    self.fetcher = fetcher;
  }

  /// Returns a reference to the current fetcher
  pub fn fetcher(&self) -> &Arc<dyn ResourceFetcher> {
    &self.fetcher
  }

  /// Attach a diagnostics sink for recording fetch failures.
  pub fn set_diagnostics_sink(&mut self, diagnostics: Option<Arc<Mutex<RenderDiagnostics>>>) {
    self.diagnostics = diagnostics;
  }

  /// Resolve a potentially relative URL to an absolute URL
  pub fn resolve_url(&self, url: &str) -> String {
    if url.trim().is_empty() {
      return String::new();
    }

    // Absolute or data URLs can be returned directly.
    if crate::resource::is_data_url(url) {
      // Data URLs often appear inside CSS/JS strings with backslash-escaped quotes
      // (e.g. `data:image/svg+xml,<svg xmlns=\\\"...\\\">`). Unescape those so the SVG/XML parser
      // sees valid markup.
      if url.contains('\\') {
        return unescape_js_escapes(url).into_owned();
      }
      return url.to_string();
    }
    if let Ok(parsed) = url::Url::parse(url) {
      return parsed.to_string();
    }

    // Resolve against the configured base URL when present.
    if let Some(base) = &self.base_url {
      if let Some(resolved) = resolve_against_base(base, url) {
        return resolved;
      }
    }

    // No usable base; return the reference unchanged.
    url.to_string()
  }

  /// Load an image from a URL or file path
  ///
  /// The URL is first resolved against the base URL if one is configured.
  /// Results are cached in memory, so subsequent loads of the same URL
  /// return the cached image.
  pub fn load(&self, url: &str) -> Result<Arc<CachedImage>> {
    let trimmed = url.trim();
    if trimmed.is_empty() {
      return Ok(about_url_placeholder_image());
    }
    if let Some(svg) = decode_inline_svg_url(trimmed) {
      return self.render_svg(svg.as_str());
    }
    if is_about_url(trimmed) {
      return Ok(about_url_placeholder_image());
    }

    // Resolve the URL first
    let resolved_url = self.resolve_url(trimmed);
    self.enforce_image_policy(&resolved_url)?;

    // Check cache first (using resolved URL as key)
    record_image_cache_request();
    if let Some(img) = self.get_cached(&resolved_url) {
      record_image_cache_hit();
      return Ok(img);
    }
    let (flight, is_owner) = self.join_inflight(&resolved_url);
    if !is_owner {
      record_image_cache_hit();
      return flight.wait(&resolved_url);
    }

    let mut inflight_guard = DecodeInFlightOwnerGuard::new(self, &resolved_url, flight);

    if let Some(resource) = self.take_raw_cached_resource(&resolved_url) {
      record_image_cache_hit();
      let result = self.decode_resource_into_cache(&resolved_url, &resource);
      let shared = match &result {
        Ok(img) => SharedImageResult::Success(Arc::clone(img)),
        Err(err) => {
          self.record_image_error(&resolved_url, err);
          SharedImageResult::Error(err.clone())
        }
      };
      inflight_guard.finish(shared);
      return result;
    }

    record_image_cache_miss();
    let result = self.fetch_and_decode(&resolved_url);
    let shared = match &result {
      Ok(img) => SharedImageResult::Success(Arc::clone(img)),
      Err(err) => SharedImageResult::Error(err.clone()),
    };
    inflight_guard.finish(shared);

    result
  }

  /// Load a decoded raster image and convert it to a premultiplied [`tiny_skia::Pixmap`],
  /// caching the result for reuse in subsequent paint calls.
  ///
  /// Returns `Ok(None)` when the resource is a vector image (SVG) or the conversion fails.
  pub fn load_raster_pixmap(
    &self,
    url: &str,
    orientation: OrientationTransform,
    decorative: bool,
  ) -> Result<Option<Arc<tiny_skia::Pixmap>>> {
    let resolved_url = self.resolve_url(url);
    if is_about_url(&resolved_url) {
      return Ok(Some(about_url_placeholder_pixmap()));
    }
    self.enforce_image_policy(&resolved_url)?;

    let key = raster_pixmap_full_key(&resolved_url, orientation, decorative);
    if let Ok(mut cache) = self.raster_pixmap_cache.lock() {
      if let Some(cached) = cache.get_cloned(&key) {
        record_raster_pixmap_cache_hit();
        record_raster_pixmap_cache_bytes(cache.current_bytes());
        with_paint_diagnostics(|diag| {
          diag.image_pixmap_cache_hits = diag.image_pixmap_cache_hits.saturating_add(1);
        });
        return Ok(Some(cached));
      }
    }
    record_raster_pixmap_cache_miss();
    with_paint_diagnostics(|diag| {
      diag.image_pixmap_cache_misses = diag.image_pixmap_cache_misses.saturating_add(1);
    });

    let image = self.load(&resolved_url)?;
    if image.is_vector {
      return Ok(None);
    }

    let (width, height) = image.oriented_dimensions(orientation);
    if width == 0 || height == 0 {
      return Ok(None);
    }

    let Some(bytes) = u64::from(width)
      .checked_mul(u64::from(height))
      .and_then(|px| px.checked_mul(4))
    else {
      return Ok(None);
    };
    if bytes > MAX_PIXMAP_BYTES {
      return Ok(None);
    }
    let bytes = match usize::try_from(bytes) {
      Ok(bytes) => bytes,
      Err(_) => return Ok(None),
    };

    let rgba = image.to_oriented_rgba(orientation);
    let (rgba_w, rgba_h) = rgba.dimensions();
    if rgba_w != width || rgba_h != height {
      return Ok(None);
    }
    let mut data = rgba.into_raw();

    // tiny-skia expects premultiplied RGBA.
    for pixel in data.chunks_exact_mut(4) {
      let alpha = pixel[3] as f32 / 255.0;
      pixel[0] = (pixel[0] as f32 * alpha).round() as u8;
      pixel[1] = (pixel[1] as f32 * alpha).round() as u8;
      pixel[2] = (pixel[2] as f32 * alpha).round() as u8;
    }

    let Some(size) = IntSize::from_wh(width, height) else {
      return Ok(None);
    };
    let Some(pixmap) = Pixmap::from_vec(data, size) else {
      return Ok(None);
    };
    let pixmap = Arc::new(pixmap);

    if self.config.max_cached_raster_bytes > 0 && bytes > self.config.max_cached_raster_bytes {
      return Ok(Some(pixmap));
    }

    if let Ok(mut cache) = self.raster_pixmap_cache.lock() {
      cache.insert(key, Arc::clone(&pixmap), bytes);
      record_raster_pixmap_cache_bytes(cache.current_bytes());
    }

    Ok(Some(pixmap))
  }

  /// Load a decoded raster image and convert it to a premultiplied [`tiny_skia::Pixmap`], but
  /// first resample it toward the requested target size.
  ///
  /// This is intended for paint call sites that render the image substantially smaller than its
  /// intrinsic dimensions: resampling before premultiplication keeps work proportional to the
  /// destination pixel count.
  ///
  /// Callers should pass target sizes in *device pixels* (after applying device pixel ratio) and
  /// only use this path when downscaling is desired (i.e., target <= intrinsic). If the requested
  /// target would require upscaling, this function falls back to [`Self::load_raster_pixmap`].
  pub fn load_raster_pixmap_at_size(
    &self,
    url: &str,
    orientation: OrientationTransform,
    decorative: bool,
    target_width: u32,
    target_height: u32,
    quality: FilterQuality,
  ) -> Result<Option<Arc<tiny_skia::Pixmap>>> {
    if target_width == 0 || target_height == 0 {
      return Ok(None);
    }
    let resolved_url = self.resolve_url(url);
    if is_about_url(&resolved_url) {
      return Ok(Some(about_url_placeholder_pixmap()));
    }
    self.enforce_image_policy(&resolved_url)?;

    let key = raster_pixmap_key(
      &resolved_url,
      orientation,
      decorative,
      target_width,
      target_height,
      raster_pixmap_quality_bits(quality),
    );
    if let Ok(mut cache) = self.raster_pixmap_cache.lock() {
      if let Some(cached) = cache.get_cloned(&key) {
        record_raster_pixmap_cache_hit();
        record_raster_pixmap_cache_bytes(cache.current_bytes());
        with_paint_diagnostics(|diag| {
          diag.image_pixmap_cache_hits = diag.image_pixmap_cache_hits.saturating_add(1);
        });
        return Ok(Some(cached));
      }
    }
    record_raster_pixmap_cache_miss();
    with_paint_diagnostics(|diag| {
      diag.image_pixmap_cache_misses = diag.image_pixmap_cache_misses.saturating_add(1);
    });

    let image = self.load(&resolved_url)?;
    if image.is_vector {
      return Ok(None);
    }

    let (src_w, src_h) = image.oriented_dimensions(orientation);
    if src_w == 0 || src_h == 0 {
      return Ok(None);
    }
    if target_width >= src_w || target_height >= src_h {
      // Callers should only use this path when downscaling. If any axis would require upscaling,
      // fall back to the full-resolution pixmap cache so results stay stable (important for
      // pixelated/crisp-edges semantics and to avoid needless resampling work).
      return self.load_raster_pixmap(&resolved_url, orientation, decorative);
    }

    let Some(bytes) = u64::from(target_width)
      .checked_mul(u64::from(target_height))
      .and_then(|px| px.checked_mul(4))
    else {
      return Ok(None);
    };
    if bytes > MAX_PIXMAP_BYTES {
      return Ok(None);
    }
    let bytes = match usize::try_from(bytes) {
      Ok(bytes) => bytes,
      Err(_) => return Ok(None),
    };

    let filter = match quality {
      FilterQuality::Nearest => image::imageops::FilterType::Nearest,
      FilterQuality::Bilinear => image::imageops::FilterType::Triangle,
      _ => image::imageops::FilterType::Triangle,
    };

    // We resize in the decoded image's native orientation and then apply the requested
    // orientation transform on the smaller output buffer.
    let (pre_w, pre_h) = if orientation.swaps_axes() {
      (target_height, target_width)
    } else {
      (target_width, target_height)
    };
    let resized = image.image.resize_exact(pre_w, pre_h, filter);
    let mut rgba = resized.to_rgba8();

    match orientation.quarter_turns % 4 {
      0 => {}
      1 => rgba = imageops::rotate90(&rgba),
      2 => rgba = imageops::rotate180(&rgba),
      3 => rgba = imageops::rotate270(&rgba),
      _ => {}
    }
    if orientation.flip_x {
      rgba = imageops::flip_horizontal(&rgba);
    }

    let (rgba_w, rgba_h) = rgba.dimensions();
    if rgba_w != target_width || rgba_h != target_height {
      return Ok(None);
    }
    let mut data = rgba.into_raw();

    // tiny-skia expects premultiplied RGBA.
    for pixel in data.chunks_exact_mut(4) {
      let alpha = pixel[3] as f32 / 255.0;
      pixel[0] = (pixel[0] as f32 * alpha).round() as u8;
      pixel[1] = (pixel[1] as f32 * alpha).round() as u8;
      pixel[2] = (pixel[2] as f32 * alpha).round() as u8;
    }

    let Some(size) = IntSize::from_wh(target_width, target_height) else {
      return Ok(None);
    };
    let Some(pixmap) = Pixmap::from_vec(data, size) else {
      return Ok(None);
    };
    let pixmap = Arc::new(pixmap);

    if self.config.max_cached_raster_bytes > 0 && bytes > self.config.max_cached_raster_bytes {
      return Ok(Some(pixmap));
    }

    if let Ok(mut cache) = self.raster_pixmap_cache.lock() {
      cache.insert(key, Arc::clone(&pixmap), bytes);
      record_raster_pixmap_cache_bytes(cache.current_bytes());
    }

    Ok(Some(pixmap))
  }

  /// Probe image metadata (dimensions, EXIF orientation/resolution, SVG intrinsic ratio)
  /// without fully decoding the image.
  pub fn probe(&self, url: &str) -> Result<Arc<CachedImageMetadata>> {
    let trimmed = url.trim();
    if trimmed.is_empty() {
      return Ok(about_url_placeholder_metadata());
    }
    if let Some(svg) = decode_inline_svg_url(trimmed) {
      let cache_key = inline_svg_cache_key(svg.trim_start());
      record_image_cache_request();

      if let Some(img) = self.get_cached(&cache_key) {
        record_image_cache_hit();
        return Ok(Arc::new(CachedImageMetadata::from(&*img)));
      }

      if let Some(meta) = self.get_cached_meta(&cache_key) {
        record_image_cache_hit();
        return Ok(meta);
      }

      let (flight, is_owner) = self.join_meta_inflight(&cache_key);
      if !is_owner {
        record_image_cache_hit();
        return flight.wait(&cache_key);
      }

      let mut inflight_guard = ProbeInFlightOwnerGuard::new(self, &cache_key, flight);
      record_image_cache_miss();
      let result = self
        .probe_svg_content(svg.as_str(), "inline-svg")
        .map(Arc::new);
      let shared = match &result {
        Ok(meta) => {
          if let Ok(mut cache) = self.meta_cache.lock() {
            cache.insert(cache_key.clone(), Arc::clone(meta));
          }
          SharedMetaResult::Success(Arc::clone(meta))
        }
        Err(err) => SharedMetaResult::Error(err.clone()),
      };
      inflight_guard.finish(shared);
      return result;
    }
    if is_about_url(trimmed) {
      return Ok(about_url_placeholder_metadata());
    }

    let resolved_url = self.resolve_url(trimmed);
    self.probe_resolved_url(&resolved_url)
  }

  pub fn probe_resolved(&self, resolved_url: &str) -> Result<Arc<CachedImageMetadata>> {
    let resolved_url = resolved_url.trim();
    let trimmed = resolved_url.trim_start();
    if trimmed.starts_with('<') {
      return self.probe(trimmed);
    }
    self.probe_resolved_url(resolved_url)
  }

  fn probe_resolved_url(&self, resolved_url: &str) -> Result<Arc<CachedImageMetadata>> {
    if resolved_url.is_empty() {
      return Err(Error::Image(ImageError::LoadFailed {
        url: resolved_url.to_string(),
        reason: "image probe URL is empty".to_string(),
      }));
    }
    if is_about_url(resolved_url) {
      return Ok(about_url_placeholder_metadata());
    }

    self.enforce_image_policy(resolved_url)?;
    record_image_cache_request();
    if let Some(img) = self.get_cached(resolved_url) {
      record_image_cache_hit();
      return Ok(Arc::new(CachedImageMetadata::from(&*img)));
    }

    if let Some(meta) = self.get_cached_meta(resolved_url) {
      record_image_cache_hit();
      return Ok(meta);
    }

    let (flight, is_owner) = self.join_meta_inflight(resolved_url);
    if !is_owner {
      record_image_cache_hit();
      return flight.wait(resolved_url);
    }

    let mut inflight_guard = ProbeInFlightOwnerGuard::new(self, resolved_url, flight);

    if let Some(cached) = self.fetcher.read_cache_artifact(
      FetchContextKind::Image,
      resolved_url,
      CacheArtifactKind::ImageProbeMetadata,
    ) {
      if let Some(ctx) = &self.resource_context {
        let policy_url = cached.final_url.as_deref().unwrap_or(resolved_url);
        if let Err(err) = ctx.check_allowed(ResourceKind::Image, policy_url) {
          let blocked = Error::Image(ImageError::LoadFailed {
            url: resolved_url.to_string(),
            reason: err.reason,
          });
          self.record_image_error(resolved_url, &blocked);
          inflight_guard.finish(SharedMetaResult::Error(blocked.clone()));
          return Err(blocked);
        }
      }

      if let Some(decoded) = decode_probe_metadata_from_disk(&cached.bytes) {
        let meta = Arc::new(decoded);
        if let Ok(mut cache) = self.meta_cache.lock() {
          cache.insert(resolved_url.to_string(), Arc::clone(&meta));
        }
        record_image_cache_hit();
        inflight_guard.finish(SharedMetaResult::Success(Arc::clone(&meta)));
        return Ok(meta);
      }

      // Corrupt or incompatible cache entry; evict so we don't repeatedly reparse it.
      self.fetcher.remove_cache_artifact(
        FetchContextKind::Image,
        cached.final_url.as_deref().unwrap_or(resolved_url),
        CacheArtifactKind::ImageProbeMetadata,
      );
    }

    record_image_cache_miss();
    let result = self.fetch_and_probe(resolved_url);
    let shared = match &result {
      Ok(meta) => SharedMetaResult::Success(Arc::clone(meta)),
      Err(err) => SharedMetaResult::Error(err.clone()),
    };
    inflight_guard.finish(shared);

    result
  }

  fn get_cached(&self, resolved_url: &str) -> Option<Arc<CachedImage>> {
    self
      .cache
      .lock()
      .ok()
      .and_then(|mut cache| cache.get_cloned(resolved_url))
  }

  fn get_cached_meta(&self, resolved_url: &str) -> Option<Arc<CachedImageMetadata>> {
    self
      .meta_cache
      .lock()
      .ok()
      .and_then(|cache| cache.get(resolved_url).cloned())
  }

  fn take_raw_cached_resource(&self, resolved_url: &str) -> Option<Arc<FetchedResource>> {
    self
      .raw_cache
      .lock()
      .ok()
      .and_then(|mut cache| cache.remove(resolved_url))
  }

  fn cache_placeholder_image(&self, resolved_url: &str) -> Arc<CachedImage> {
    let image = about_url_placeholder_image();
    self.insert_cached_image(resolved_url, Arc::clone(&image));
    let meta = about_url_placeholder_metadata();
    if let Ok(mut cache) = self.meta_cache.lock() {
      cache.insert(resolved_url.to_string(), Arc::clone(&meta));
    }
    image
  }

  fn cache_placeholder_metadata(&self, resolved_url: &str) -> Arc<CachedImageMetadata> {
    let meta = about_url_placeholder_metadata();
    if let Ok(mut cache) = self.meta_cache.lock() {
      cache.insert(resolved_url.to_string(), Arc::clone(&meta));
    }
    meta
  }

  fn insert_cached_image(&self, resolved_url: &str, image: Arc<CachedImage>) {
    let mut bytes = Self::estimate_image_bytes(&image.image);
    if let Some(svg) = &image.svg_content {
      bytes = bytes.saturating_add(svg.len());
    }
    if let Ok(mut cache) = self.cache.lock() {
      cache.insert(resolved_url.to_string(), image, bytes);
    }
  }

  fn insert_svg_pixmap(&self, key: SvgPixmapKey, pixmap: Arc<tiny_skia::Pixmap>) {
    let bytes = pixmap.data().len();
    if let Ok(mut cache) = self.svg_pixmap_cache.lock() {
      cache.insert(key, pixmap, bytes);
    }
  }

  fn estimate_image_bytes(image: &DynamicImage) -> usize {
    let (width, height) = image.dimensions();
    let pixels = usize::try_from(width)
      .unwrap_or(0)
      .saturating_mul(usize::try_from(height).unwrap_or(0));
    let bpp = usize::from(image.color().bytes_per_pixel()).max(1);
    pixels.saturating_mul(bpp)
  }

  fn record_image_error(&self, url: &str, error: &Error) {
    if let Some(diag) = &self.diagnostics {
      if let Ok(mut guard) = diag.lock() {
        guard.record_error(ResourceKind::Image, url, error);
      }
    }
  }

  fn record_invalid_image(&self, url: &str) {
    const INVALID_IMAGE_LIMIT: usize = 64;
    let Some(diag) = &self.diagnostics else {
      return;
    };
    let Ok(mut guard) = diag.lock() else {
      return;
    };
    if guard.invalid_images.len() >= INVALID_IMAGE_LIMIT {
      return;
    }
    if guard.invalid_images.iter().any(|u| u == url) {
      return;
    }
    guard.invalid_images.push(url.to_string());
  }

  fn enforce_image_policy(&self, url: &str) -> Result<()> {
    if let Some(ctx) = &self.resource_context {
      if let Err(err) = ctx.check_allowed(ResourceKind::Image, url) {
        let blocked = Error::Image(ImageError::LoadFailed {
          url: url.to_string(),
          reason: err.reason,
        });
        if ctx.diagnostics.is_none() {
          self.record_image_error(url, &blocked);
        }
        return Err(blocked);
      }
    }

    Ok(())
  }

  /// Enforce the active resource policy for subresources referenced within an SVG document.
  fn enforce_svg_resource_policy(&self, svg_content: &str, svg_url: &str) -> Result<()> {
    let Some(ctx) = &self.resource_context else {
      return Ok(());
    };

    let doc = match roxmltree::Document::parse(svg_content) {
      Ok(doc) => doc,
      Err(_) => return Ok(()),
    };

    for node in doc.descendants() {
      for attr in node.attributes() {
        if attr.name() != "href" {
          continue;
        }

        let href = attr.value().trim();
        if href.is_empty()
          || href.starts_with('#')
          || crate::resource::is_data_url(href)
          || is_about_url(href)
        {
          continue;
        }

        let resolved = resolve_against_base(svg_url, href).unwrap_or_else(|| href.to_string());
        if let Err(err) = ctx.check_allowed(ResourceKind::Image, &resolved) {
          return Err(Error::Image(ImageError::LoadFailed {
            url: resolved,
            reason: err.reason,
          }));
        }
      }
    }

    Ok(())
  }

  fn fetch_and_decode(&self, resolved_url: &str) -> Result<Arc<CachedImage>> {
    let threshold_ms = image_profile_threshold_ms();
    let profile_enabled = threshold_ms.is_some();
    let total_start = profile_enabled.then(Instant::now);
    let fetch_start = profile_enabled.then(Instant::now);

    let referrer = self
      .resource_context
      .as_ref()
      .and_then(|ctx| ctx.document_url.as_deref());
    let mut request = FetchRequest::new(resolved_url, FetchDestination::Image);
    if let Some(referrer) = referrer {
      request = request.with_referrer(referrer);
    }
    let resource = match self.fetcher.fetch_with_request(request) {
      Ok(res) => res,
      Err(err) => {
        if is_empty_body_error_for_image(&err) {
          return Ok(self.cache_placeholder_image(resolved_url));
        }
        self.record_image_error(resolved_url, &err);
        return Err(err);
      }
    };
    if let Some(ctx) = &self.resource_context {
      let policy_url = resource.final_url.as_deref().unwrap_or(resolved_url);
      if let Err(err) = ctx.check_allowed(ResourceKind::Image, policy_url) {
        let blocked = Error::Image(ImageError::LoadFailed {
          url: resolved_url.to_string(),
          reason: err.reason,
        });
        self.record_image_error(resolved_url, &blocked);
        return Err(blocked);
      }
    }
    if status_is_http_success(resource.status)
      && payload_looks_like_markup_but_not_svg(&resource.bytes)
    {
      self.record_invalid_image(resolved_url);
      return Ok(self.cache_placeholder_image(resolved_url));
    }
    if let Err(err) =
      ensure_http_success(&resource, resolved_url).and_then(|()| ensure_image_mime_sane(&resource, resolved_url))
    {
      self.record_image_error(resolved_url, &err);
      return Err(err);
    }
    let fetch_ms = fetch_start.map(|s| s.elapsed().as_secs_f64() * 1000.0);
    let decode_timer = Instant::now();
    let decode_start = profile_enabled.then_some(decode_timer);
    let (img, orientation, resolution, is_vector, intrinsic_ratio, aspect_ratio_none, svg_content) =
      match self.decode_resource(&resource, resolved_url) {
        Ok(decoded) => decoded,
        Err(err) => {
          self.record_image_error(resolved_url, &err);
          return Err(err);
        }
      };
    let decode_ms_value = decode_timer.elapsed().as_secs_f64() * 1000.0;
    let decode_ms = decode_start.map(|_| decode_ms_value);
    record_image_decode_ms(decode_ms_value);

    let img_arc = Arc::new(CachedImage {
      image: Arc::new(img),
      orientation,
      resolution,
      is_vector,
      intrinsic_ratio,
      aspect_ratio_none,
      svg_content,
    });

    self.insert_cached_image(resolved_url, Arc::clone(&img_arc));

    if let (Some(threshold_ms), Some(total_start)) = (threshold_ms, total_start) {
      let total_ms = total_start.elapsed().as_secs_f64() * 1000.0;
      if total_ms >= threshold_ms {
        let content_type = resource
          .content_type
          .as_deref()
          .unwrap_or("<unknown>")
          .split(';')
          .next()
          .unwrap_or("<unknown>");
        eprintln!(
          "image_profile kind=decode total_ms={total_ms:.2} fetch_ms={:.2} decode_ms={:.2} bytes={} dims={}x{} vector={} url={}",
          fetch_ms.unwrap_or(0.0),
          decode_ms.unwrap_or(0.0),
          resource.bytes.len(),
          img_arc.image.width(),
          img_arc.image.height(),
          img_arc.is_vector,
          resolved_url
        );
        eprintln!(" image_profile content_type={content_type}");
      }
    }

    Ok(img_arc)
  }

  fn decode_resource_into_cache(
    &self,
    resolved_url: &str,
    resource: &FetchedResource,
  ) -> Result<Arc<CachedImage>> {
    if status_is_http_success(resource.status)
      && payload_looks_like_markup_but_not_svg(&resource.bytes)
    {
      self.record_invalid_image(resolved_url);
      return Ok(self.cache_placeholder_image(resolved_url));
    }
    let threshold_ms = image_profile_threshold_ms();
    let profile_enabled = threshold_ms.is_some();
    let total_start = profile_enabled.then(Instant::now);
    let decode_timer = Instant::now();
    let decode_start = profile_enabled.then_some(decode_timer);
    let (img, orientation, resolution, is_vector, intrinsic_ratio, aspect_ratio_none, svg_content) =
      self.decode_resource(resource, resolved_url)?;
    let decode_ms_value = decode_timer.elapsed().as_secs_f64() * 1000.0;
    let decode_ms = decode_start.map(|_| decode_ms_value);
    record_image_decode_ms(decode_ms_value);

    let img_arc = Arc::new(CachedImage {
      image: Arc::new(img),
      orientation,
      resolution,
      is_vector,
      intrinsic_ratio,
      aspect_ratio_none,
      svg_content,
    });

    self.insert_cached_image(resolved_url, Arc::clone(&img_arc));

    if let (Some(threshold_ms), Some(total_start)) = (threshold_ms, total_start) {
      let total_ms = total_start.elapsed().as_secs_f64() * 1000.0;
      if total_ms >= threshold_ms {
        let content_type = resource
          .content_type
          .as_deref()
          .unwrap_or("<unknown>")
          .split(';')
          .next()
          .unwrap_or("<unknown>");
        eprintln!(
          "image_profile kind=decode total_ms={total_ms:.2} fetch_ms=0.00 decode_ms={:.2} bytes={} dims={}x{} vector={} url={}",
          decode_ms.unwrap_or(0.0),
          resource.bytes.len(),
          img_arc.image.width(),
          img_arc.image.height(),
          img_arc.is_vector,
          resolved_url
        );
        eprintln!(" image_profile content_type={content_type}");
      }
    }

    Ok(img_arc)
  }

  fn fetch_and_probe(&self, resolved_url: &str) -> Result<Arc<CachedImageMetadata>> {
    let threshold_ms = image_profile_threshold_ms();
    let profile_enabled = threshold_ms.is_some();
    let total_start = profile_enabled.then(Instant::now);
    let fetch_start = profile_enabled.then(Instant::now);

    let check_resource_allowed = |resource: &FetchedResource| -> Result<()> {
      if let Some(ctx) = &self.resource_context {
        let policy_url = resource.final_url.as_deref().unwrap_or(resolved_url);
        if let Err(err) = ctx.check_allowed(ResourceKind::Image, policy_url) {
          return Err(Error::Image(ImageError::LoadFailed {
            url: resolved_url.to_string(),
            reason: err.reason,
          }));
        }
      }
      ensure_http_success(resource, resolved_url)
    };

    let probe_limit = image_probe_max_bytes();
    let retry_limit = probe_limit
      .saturating_mul(8)
      .max(512 * 1024)
      .clamp(1, 64 * 1024 * 1024);

    const RAW_RESOURCE_CACHE_LIMIT_BYTES: usize = 5 * 1024 * 1024;

    for (idx, limit) in [probe_limit, retry_limit].into_iter().enumerate() {
      let resource =
        match self
          .fetcher
          .fetch_partial_with_context(FetchContextKind::Image, resolved_url, limit)
        {
          Ok(res) => res,
          Err(err) => {
            if is_empty_body_error_for_image(&err) {
              return Ok(self.cache_placeholder_metadata(resolved_url));
            }
            let _ = err;
            break;
          }
        };
      // Some servers reject `Range` requests for images (or bot-mitigation paths) with status codes
      // like 405/416 even though a full GET without a `Range` header would succeed. In that case,
      // skip reporting a fetch error from the probe and fall back to a full fetch.
      if matches!(resource.status, Some(405 | 416)) {
        record_probe_partial_fetch(resource.bytes.len());
        break;
      }
      record_probe_partial_fetch(resource.bytes.len());
      let resource = Arc::new(resource);

      if let Err(err) = check_resource_allowed(resource.as_ref()) {
        self.record_image_error(resolved_url, &err);
        return Err(err);
      }
      if status_is_http_success(resource.status)
        && payload_looks_like_markup_but_not_svg(&resource.bytes)
      {
        self.record_invalid_image(resolved_url);
        return Ok(self.cache_placeholder_metadata(resolved_url));
      }

      let fetch_ms = fetch_start.map(|s| s.elapsed().as_secs_f64() * 1000.0);
      let attempt_probe_start = profile_enabled.then(Instant::now);
      match self.probe_resource(&resource, resolved_url) {
        Ok(meta) => {
          let probe_ms = attempt_probe_start.map(|s| s.elapsed().as_secs_f64() * 1000.0);
          let meta = Arc::new(meta);

          if let Ok(mut cache) = self.meta_cache.lock() {
            cache.insert(resolved_url.to_string(), Arc::clone(&meta));
          }
          if let Some(serialized) = encode_probe_metadata_for_disk(&meta) {
            self.fetcher.write_cache_artifact(
              FetchContextKind::Image,
              resolved_url,
              CacheArtifactKind::ImageProbeMetadata,
              &serialized,
              Some(resource.as_ref()),
            );
          }

          // When the image is small enough to fit in the probe prefix, keep the bytes so a later
          // decode can reuse them without issuing another HTTP request.
          if resource.bytes.len() < limit && resource.bytes.len() <= RAW_RESOURCE_CACHE_LIMIT_BYTES
          {
            if let Ok(mut cache) = self.raw_cache.lock() {
              cache.insert(resolved_url.to_string(), Arc::clone(&resource));
            }
          }

          if let (Some(threshold_ms), Some(total_start)) = (threshold_ms, total_start) {
            let total_ms = total_start.elapsed().as_secs_f64() * 1000.0;
            if total_ms >= threshold_ms {
              let content_type = resource
                .content_type
                .as_deref()
                .unwrap_or("<unknown>")
                .split(';')
                .next()
                .unwrap_or("<unknown>");
              eprintln!(
                "image_profile kind=probe total_ms={total_ms:.2} fetch_ms={:.2} probe_ms={:.2} bytes={} dims={}x{} vector={} url={}",
                fetch_ms.unwrap_or(0.0),
                probe_ms.unwrap_or(0.0),
                resource.bytes.len(),
                meta.width,
                meta.height,
                meta.is_vector,
                resolved_url
              );
              eprintln!(" image_profile content_type={content_type}");
            }
          }

          return Ok(meta);
        }
        Err(err) => {
          // Only retry/fallback when it looks like the prefix may have been truncated.
          if resource.bytes.len() < limit {
            self.record_image_error(resolved_url, &err);
            return Err(err);
          }
          let _ = err;
          // Retry once with a larger prefix, then fall back to a full fetch.
          if idx == 0 {
            continue;
          }
          break;
        }
      }
    }

    if probe_limit > 0 {
      record_probe_partial_fallback_full();
    }

    let referrer = self
      .resource_context
      .as_ref()
      .and_then(|ctx| ctx.document_url.as_deref());
    let mut request = FetchRequest::new(resolved_url, FetchDestination::Image);
    if let Some(referrer) = referrer {
      request = request.with_referrer(referrer);
    }
    let resource = match self.fetcher.fetch_with_request(request) {
      Ok(res) => res,
      Err(err) => {
        if is_empty_body_error_for_image(&err) {
          return Ok(self.cache_placeholder_metadata(resolved_url));
        }
        self.record_image_error(resolved_url, &err);
        return Err(err);
      }
    };
    let resource = Arc::new(resource);
    if let Err(err) = check_resource_allowed(resource.as_ref()) {
      self.record_image_error(resolved_url, &err);
      return Err(err);
    }
    if status_is_http_success(resource.status) && payload_looks_like_markup_but_not_svg(&resource.bytes) {
      self.record_invalid_image(resolved_url);
      return Ok(self.cache_placeholder_metadata(resolved_url));
    }
    let fetch_ms = fetch_start.map(|s| s.elapsed().as_secs_f64() * 1000.0);
    let probe_start = profile_enabled.then(Instant::now);
    let meta = match self.probe_resource(&resource, resolved_url) {
      Ok(meta) => meta,
      Err(err) => {
        self.record_image_error(resolved_url, &err);
        return Err(err);
      }
    };
    let probe_ms = probe_start.map(|s| s.elapsed().as_secs_f64() * 1000.0);
    let meta = Arc::new(meta);

    if let Ok(mut cache) = self.meta_cache.lock() {
      cache.insert(resolved_url.to_string(), Arc::clone(&meta));
    }
    if let Some(serialized) = encode_probe_metadata_for_disk(&meta) {
      self.fetcher.write_cache_artifact(
        FetchContextKind::Image,
        resolved_url,
        CacheArtifactKind::ImageProbeMetadata,
        &serialized,
        Some(resource.as_ref()),
      );
    }

    if resource.bytes.len() <= RAW_RESOURCE_CACHE_LIMIT_BYTES {
      if let Ok(mut cache) = self.raw_cache.lock() {
        cache.insert(resolved_url.to_string(), Arc::clone(&resource));
      }
    }

    if let (Some(threshold_ms), Some(total_start)) = (threshold_ms, total_start) {
      let total_ms = total_start.elapsed().as_secs_f64() * 1000.0;
      if total_ms >= threshold_ms {
        let content_type = resource
          .content_type
          .as_deref()
          .unwrap_or("<unknown>")
          .split(';')
          .next()
          .unwrap_or("<unknown>");
        eprintln!(
          "image_profile kind=probe total_ms={total_ms:.2} fetch_ms={:.2} probe_ms={:.2} bytes={} dims={}x{} vector={} url={}",
          fetch_ms.unwrap_or(0.0),
          probe_ms.unwrap_or(0.0),
          resource.bytes.len(),
          meta.width,
          meta.height,
          meta.is_vector,
          resolved_url
        );
        eprintln!(" image_profile content_type={content_type}");
      }
    }

    Ok(meta)
  }

  fn join_inflight(&self, resolved_url: &str) -> (Arc<DecodeInFlight>, bool) {
    let mut map = match self.in_flight.lock() {
      Ok(map) => map,
      Err(poisoned) => {
        let mut map = poisoned.into_inner();
        map.clear();
        map
      }
    };
    if let Some(existing) = map.get(resolved_url) {
      return (Arc::clone(existing), false);
    }

    let flight = Arc::new(DecodeInFlight::new());
    map.insert(resolved_url.to_string(), Arc::clone(&flight));
    (flight, true)
  }

  fn join_meta_inflight(&self, resolved_url: &str) -> (Arc<ProbeInFlight>, bool) {
    let mut map = match self.meta_in_flight.lock() {
      Ok(map) => map,
      Err(poisoned) => {
        let mut map = poisoned.into_inner();
        map.clear();
        map
      }
    };
    if let Some(existing) = map.get(resolved_url) {
      return (Arc::clone(existing), false);
    }

    let flight = Arc::new(ProbeInFlight::new());
    map.insert(resolved_url.to_string(), Arc::clone(&flight));
    (flight, true)
  }

  fn finish_inflight(
    &self,
    resolved_url: &str,
    flight: &Arc<DecodeInFlight>,
    result: SharedImageResult,
  ) {
    flight.set(result);
    let mut map = self
      .in_flight
      .lock()
      .unwrap_or_else(|poisoned| poisoned.into_inner());
    map.remove(resolved_url);
  }

  fn finish_meta_inflight(
    &self,
    resolved_url: &str,
    flight: &Arc<ProbeInFlight>,
    result: SharedMetaResult,
  ) {
    flight.set(result);
    let mut map = self
      .meta_in_flight
      .lock()
      .unwrap_or_else(|poisoned| poisoned.into_inner());
    map.remove(resolved_url);
  }

  /// Render raw SVG content to an image, caching by content hash.
  pub fn render_svg(&self, svg_content: &str) -> Result<Arc<CachedImage>> {
    record_image_cache_request();
    let cache_key = inline_svg_cache_key(svg_content);
    if let Some(image) = self.get_cached(&cache_key) {
      self.enforce_svg_resource_policy(svg_content, "inline-svg")?;
      record_image_cache_hit();
      return Ok(image);
    }
    record_image_cache_miss();
    let decode_timer = Instant::now();
    let (img, intrinsic_ratio, aspect_ratio_none) = self.render_svg_to_image(svg_content)?;
    let svg_content = Arc::<str>::from(svg_content);
    record_image_decode_ms(decode_timer.elapsed().as_secs_f64() * 1000.0);
    let cached = Arc::new(CachedImage {
      image: Arc::new(img),
      orientation: None,
      resolution: None,
      is_vector: true,
      intrinsic_ratio,
      aspect_ratio_none,
      svg_content: Some(svg_content),
    });
    self.insert_cached_image(&cache_key, Arc::clone(&cached));
    Ok(cached)
  }

  pub(crate) fn render_svg_pixmap_at_size(
    &self,
    svg_content: &str,
    render_width: u32,
    render_height: u32,
    url: &str,
    device_pixel_ratio: f32,
  ) -> Result<Arc<tiny_skia::Pixmap>> {
    self.enforce_svg_resource_policy(svg_content, url)?;
    self.enforce_decode_limits(render_width, render_height, url)?;
    check_active(RenderStage::Paint).map_err(Error::Render)?;

    let key = svg_pixmap_key(
      svg_content,
      url,
      device_pixel_ratio,
      render_width,
      render_height,
    );
    record_image_cache_request();
    if let Ok(mut cache) = self.svg_pixmap_cache.lock() {
      if let Some(cached) = cache.get_cloned(&key) {
        record_image_cache_hit();
        return Ok(cached);
      }
    }

    record_image_cache_miss();
    self.render_svg_pixmap_at_size_uncached(svg_content, key, render_width, render_height, url)
  }

  pub(crate) fn render_svg_pixmap_at_size_with_injected_style(
    &self,
    svg_content: &str,
    insert_pos: usize,
    style_element: &str,
    render_width: u32,
    render_height: u32,
    url: &str,
    device_pixel_ratio: f32,
  ) -> Result<Arc<tiny_skia::Pixmap>> {
    // Policy enforcement is based on the SVG markup itself; injected document CSS does not add
    // new `<image href>` style subresources that we currently police.
    self.enforce_svg_resource_policy(svg_content, url)?;
    self.enforce_decode_limits(render_width, render_height, url)?;
    check_active(RenderStage::Paint).map_err(Error::Render)?;

    let Some(prefix) = svg_content.get(..insert_pos) else {
      return self.render_svg_pixmap_at_size(
        svg_content,
        render_width,
        render_height,
        url,
        device_pixel_ratio,
      );
    };
    let Some(suffix) = svg_content.get(insert_pos..) else {
      return self.render_svg_pixmap_at_size(
        svg_content,
        render_width,
        render_height,
        url,
        device_pixel_ratio,
      );
    };

    // Match `svg_pixmap_key` hashing semantics for a single combined string without allocating it.
    // `Hash` for `str` appends a 0xFF terminator byte, so hashing chunks separately would
    // introduce extra terminators and change the key. We instead hash the raw bytes and append
    // the terminator once.
    let mut content_hasher = DefaultHasher::new();
    content_hasher.write(prefix.as_bytes());
    content_hasher.write(style_element.as_bytes());
    content_hasher.write(suffix.as_bytes());
    content_hasher.write_u8(0xff);
    let mut url_hasher = DefaultHasher::new();
    url.hash(&mut url_hasher);
    let key = SvgPixmapKey {
      hash: content_hasher.finish(),
      url_hash: url_hasher.finish(),
      len: prefix.len() + style_element.len() + suffix.len(),
      width: render_width,
      height: render_height,
      device_pixel_ratio_bits: device_pixel_ratio.to_bits(),
    };

    record_image_cache_request();
    if let Ok(mut cache) = self.svg_pixmap_cache.lock() {
      if let Some(cached) = cache.get_cloned(&key) {
        record_image_cache_hit();
        return Ok(cached);
      }
    }

    record_image_cache_miss();

    let mut combined = String::with_capacity(key.len);
    combined.push_str(prefix);
    combined.push_str(style_element);
    combined.push_str(suffix);

    self.render_svg_pixmap_at_size_uncached(&combined, key, render_width, render_height, url)
  }

  fn render_svg_pixmap_at_size_uncached(
    &self,
    svg_content: &str,
    key: SvgPixmapKey,
    render_width: u32,
    render_height: u32,
    url: &str,
  ) -> Result<Arc<tiny_skia::Pixmap>> {
    use resvg::usvg;

    let render_timer = Instant::now();

    if let Some(pixmap) = try_render_simple_svg_pixmap(svg_content, render_width, render_height)? {
      let pixmap = Arc::new(pixmap);
      record_image_decode_ms(render_timer.elapsed().as_secs_f64() * 1000.0);
      self.insert_svg_pixmap(key, Arc::clone(&pixmap));
      return Ok(pixmap);
    }

    let mut options = usvg::Options::default();
    if let Ok(parsed) = Url::parse(url) {
      if parsed.scheme() == "file" {
        if let Ok(path) = parsed.to_file_path() {
          if let Some(dir) = path.parent() {
            options.resources_dir = std::fs::canonicalize(dir)
              .ok()
              .or_else(|| Some(dir.to_path_buf()));
          }
        }
      }
    }
    let tree = usvg::Tree::from_str(svg_content, &options).map_err(|e| {
      Error::Image(ImageError::DecodeFailed {
        url: url.to_string(),
        reason: format!("Failed to parse SVG: {}", e),
      })
    })?;

    let size = tree.size();
    let source_width = size.width();
    let source_height = size.height();
    if source_width <= 0.0 || source_height <= 0.0 {
      return Err(Error::Render(RenderError::CanvasCreationFailed {
        width: source_width as u32,
        height: source_height as u32,
      }));
    }

    let Some(mut pixmap) = new_pixmap(render_width, render_height) else {
      return Err(Error::Render(RenderError::CanvasCreationFailed {
        width: render_width,
        height: render_height,
      }));
    };

    let transform = match svg_view_box_root_transform(
      svg_content,
      source_width,
      source_height,
      render_width as f32,
      render_height as f32,
    ) {
      Some(transform) => transform,
      None => {
        let scale_x = render_width as f32 / source_width;
        let scale_y = render_height as f32 / source_height;
        tiny_skia::Transform::from_scale(scale_x, scale_y)
      }
    };
    check_active(RenderStage::Paint).map_err(Error::Render)?;
    resvg::render(&tree, transform, &mut pixmap.as_mut());
    check_active(RenderStage::Paint).map_err(Error::Render)?;

    let pixmap = Arc::new(pixmap);
    record_image_decode_ms(render_timer.elapsed().as_secs_f64() * 1000.0);
    self.insert_svg_pixmap(key, Arc::clone(&pixmap));

    Ok(pixmap)
  }

  /// Probe intrinsic SVG metadata (dimensions/aspect ratio) from raw markup without rasterizing.
  pub fn probe_svg_content(
    &self,
    svg_content: &str,
    url_hint: &str,
  ) -> Result<CachedImageMetadata> {
    const DEFAULT_WIDTH: f32 = 300.0;
    const DEFAULT_HEIGHT: f32 = 150.0;

    self.enforce_svg_resource_policy(svg_content, url_hint)?;

    let (meta_width, meta_height, meta_ratio, aspect_ratio_none) =
      svg_intrinsic_metadata(svg_content).unwrap_or((None, None, None, false));

    let ratio = meta_ratio.filter(|r| *r > 0.0);
    let (target_width, target_height) = match (
      meta_width.filter(|w| *w > 0.0),
      meta_height.filter(|h| *h > 0.0),
      ratio,
    ) {
      (Some(w), Some(h), _) => (w, h),
      (Some(w), None, Some(r)) => (w, (w / r).max(1.0)),
      (None, Some(h), Some(r)) => ((h * r).max(1.0), h),
      (Some(w), None, None) => (w, DEFAULT_HEIGHT),
      (None, Some(h), None) => (DEFAULT_WIDTH, h),
      (None, None, Some(r)) => (DEFAULT_WIDTH, (DEFAULT_WIDTH / r).max(1.0)),
      (None, None, None) => (DEFAULT_WIDTH, DEFAULT_HEIGHT),
    };

    let width = target_width.max(1.0).round() as u32;
    let height = target_height.max(1.0).round() as u32;
    self.enforce_decode_limits(width, height, url_hint)?;

    let intrinsic_ratio = if aspect_ratio_none {
      None
    } else {
      ratio.or_else(|| {
        if height > 0 {
          Some(width as f32 / height as f32)
        } else {
          None
        }
      })
    };

    Ok(CachedImageMetadata {
      width,
      height,
      orientation: None,
      resolution: None,
      is_vector: true,
      intrinsic_ratio,
      aspect_ratio_none,
    })
  }

  /// Decode a fetched resource into an image
  fn decode_resource(
    &self,
    resource: &FetchedResource,
    url: &str,
  ) -> Result<(
    DynamicImage,
    Option<OrientationTransform>,
    Option<f32>,
    bool,
    Option<f32>,
    bool,
    Option<Arc<str>>,
  )> {
    let bytes = &resource.bytes;
    let content_type = resource.content_type.as_deref();
    check_active(RenderStage::Paint).map_err(Error::Render)?;
    if bytes.is_empty() {
      let img = RgbaImage::new(1, 1);
      return Ok((DynamicImage::ImageRgba8(img), None, None, false, None, false, None));
    }

    // Check if this is SVG
    let mime_is_svg = content_type
      .map(|m| m.contains("image/svg"))
      .unwrap_or(false);
    let is_svg = mime_is_svg
      || std::str::from_utf8(bytes)
        .ok()
        .map(|s| s.trim_start().starts_with("<svg") || s.trim_start().starts_with("<?xml"))
        .unwrap_or(false);

    if is_svg {
      let content = std::str::from_utf8(bytes).map_err(|e| {
        Error::Image(ImageError::DecodeFailed {
          url: url.to_string(),
          reason: format!("SVG not valid UTF-8: {}", e),
        })
      })?;
      let svg_content: Arc<str> = Arc::from(content);
      let (img, ratio, aspect_none) = self.render_svg_to_image_with_url(&svg_content, url)?;
      return Ok((img, None, None, true, ratio, aspect_none, Some(svg_content)));
    }

    // Regular image - extract EXIF metadata and decode
    let (orientation, resolution) = Self::exif_metadata(bytes);
    self
      .decode_bitmap(bytes, content_type, url)
      .map(|img| (img, orientation, resolution, false, None, false, None))
  }

  fn probe_resource(&self, resource: &FetchedResource, url: &str) -> Result<CachedImageMetadata> {
    let bytes = &resource.bytes;
    let content_type = resource.content_type.as_deref();
    if bytes.is_empty() {
      return Ok((*about_url_placeholder_metadata()).clone());
    }

    // SVG: parse intrinsic metadata without rasterizing.
    let mime_is_svg = content_type
      .map(|m| m.contains("image/svg"))
      .unwrap_or(false);
    let is_svg = mime_is_svg
      || std::str::from_utf8(bytes)
        .ok()
        .map(|s| s.trim_start().starts_with("<svg") || s.trim_start().starts_with("<?xml"))
        .unwrap_or(false);
    if is_svg {
      let content = std::str::from_utf8(bytes).map_err(|e| {
        Error::Image(ImageError::DecodeFailed {
          url: url.to_string(),
          reason: format!("SVG not valid UTF-8: {}", e),
        })
      })?;

      const DEFAULT_WIDTH: f32 = 300.0;
      const DEFAULT_HEIGHT: f32 = 150.0;
      let (meta_width, meta_height, meta_ratio, aspect_ratio_none) =
        svg_intrinsic_metadata(content).unwrap_or((None, None, None, false));
      let ratio = meta_ratio.filter(|r| *r > 0.0);
      let (target_width, target_height) = match (
        meta_width.filter(|w| *w > 0.0),
        meta_height.filter(|h| *h > 0.0),
        ratio,
      ) {
        (Some(w), Some(h), _) => (w, h),
        (Some(w), None, Some(r)) => (w, (w / r).max(1.0)),
        (None, Some(h), Some(r)) => ((h * r).max(1.0), h),
        (Some(w), None, None) => (w, DEFAULT_HEIGHT),
        (None, Some(h), None) => (DEFAULT_WIDTH, h),
        (None, None, Some(r)) => (DEFAULT_WIDTH, (DEFAULT_WIDTH / r).max(1.0)),
        (None, None, None) => (DEFAULT_WIDTH, DEFAULT_HEIGHT),
      };

      let width = target_width.max(1.0).round() as u32;
      let height = target_height.max(1.0).round() as u32;
      self.enforce_decode_limits(width, height, url)?;

      let ratio = if aspect_ratio_none {
        None
      } else {
        ratio.or_else(|| {
          if height > 0 {
            Some(width as f32 / height as f32)
          } else {
            None
          }
        })
      };

      return Ok(CachedImageMetadata {
        width,
        height,
        orientation: None,
        resolution: None,
        is_vector: true,
        intrinsic_ratio: ratio,
        aspect_ratio_none,
      });
    }

    let (orientation, resolution) = Self::exif_metadata(bytes);
    let format_from_content_type = Self::format_from_content_type(content_type);
    let sniffed_format = Self::sniff_image_format(bytes);
    let (width, height) = self
      .predecoded_dimensions(bytes, format_from_content_type, sniffed_format)
      .ok_or_else(|| {
        Error::Image(ImageError::DecodeFailed {
          url: url.to_string(),
          reason: "Unable to determine image dimensions".to_string(),
        })
      })?;
    self.enforce_decode_limits(width, height, url)?;

    Ok(CachedImageMetadata {
      width,
      height,
      orientation,
      resolution,
      is_vector: false,
      intrinsic_ratio: None,
      aspect_ratio_none: false,
    })
  }

  fn decode_bitmap(
    &self,
    bytes: &[u8],
    content_type: Option<&str>,
    url: &str,
  ) -> Result<DynamicImage> {
    check_active(RenderStage::Paint).map_err(Error::Render)?;
    let format_from_content_type = Self::format_from_content_type(content_type);
    let sniffed_format = Self::sniff_image_format(bytes);
    if let Some((width, height)) =
      self.predecoded_dimensions(bytes, format_from_content_type, sniffed_format)
    {
      self.enforce_decode_limits(width, height, url)?;
    }
    let mut last_error: Option<Error> = None;

    if matches!(format_from_content_type, Some(ImageFormat::Avif))
      || matches!(sniffed_format, Some(ImageFormat::Avif))
    {
      match Self::decode_avif(bytes) {
        Ok(img) => return self.finish_bitmap_decode(img, url),
        Err(AvifDecodeError::Timeout(err)) => return Err(Error::Render(err)),
        Err(AvifDecodeError::Image(err)) => last_error = Some(self.decode_error(url, err)),
      }
    }

    if let Some(format) = format_from_content_type {
      if format != ImageFormat::Avif {
        check_active(RenderStage::Paint).map_err(Error::Render)?;
        match Self::decode_with_format(bytes, format) {
          Ok(img) => return self.finish_bitmap_decode(img, url),
          Err(err) => {
            let mapped = self.decode_error(url, err);
            if let Error::Render(_) = mapped {
              return Err(mapped);
            }
            last_error = Some(mapped);
          }
        }
      }
    }

    if let Some(format) = sniffed_format {
      if Some(format) != format_from_content_type && format != ImageFormat::Avif {
        check_active(RenderStage::Paint).map_err(Error::Render)?;
        match Self::decode_with_format(bytes, format) {
          Ok(img) => return self.finish_bitmap_decode(img, url),
          Err(err) => {
            let mapped = self.decode_error(url, err);
            if let Error::Render(_) = mapped {
              return Err(mapped);
            }
            last_error = Some(mapped);
          }
        }
      }
    }

    check_active(RenderStage::Paint).map_err(Error::Render)?;
    match Self::decode_with_guess(bytes) {
      Ok(img) => self.finish_bitmap_decode(img, url),
      Err(err) => {
        let mapped = self.decode_error(url, err);
        if let Error::Render(_) = mapped {
          Err(mapped)
        } else {
          Err(last_error.unwrap_or(mapped))
        }
      }
    }
  }

  fn finish_bitmap_decode(&self, img: DynamicImage, url: &str) -> Result<DynamicImage> {
    self.enforce_decode_limits(img.width(), img.height(), url)?;
    Ok(img)
  }

  fn format_from_content_type(content_type: Option<&str>) -> Option<ImageFormat> {
    let mime = content_type?
      .split(';')
      .next()
      .map(|ct| ct.trim().to_ascii_lowercase())?;
    ImageFormat::from_mime_type(mime)
  }

  fn sniff_image_format(bytes: &[u8]) -> Option<ImageFormat> {
    image::guess_format(bytes).ok()
  }

  fn decode_with_format(bytes: &[u8], format: ImageFormat) -> image::ImageResult<DynamicImage> {
    ImageReader::with_format(DeadlineCursor::new(bytes), format).decode()
  }

  fn decode_with_guess(bytes: &[u8]) -> image::ImageResult<DynamicImage> {
    ImageReader::new(DeadlineCursor::new(bytes))
      .with_guessed_format()?
      .decode()
  }

  fn decode_error(&self, url: &str, err: image::ImageError) -> Error {
    if let image::ImageError::IoError(io_err) = &err {
      if let Some(render_err) = io_err
        .get_ref()
        .and_then(|source| source.downcast_ref::<RenderError>())
      {
        return Error::Render(render_err.clone());
      }
    }
    Error::Image(ImageError::DecodeFailed {
      url: url.to_string(),
      reason: err.to_string(),
    })
  }

  fn predecoded_dimensions(
    &self,
    bytes: &[u8],
    format_from_content_type: Option<ImageFormat>,
    sniffed_format: Option<ImageFormat>,
  ) -> Option<(u32, u32)> {
    format_from_content_type
      .and_then(|format| Self::dimensions_for_format(bytes, format))
      .or_else(|| sniffed_format.and_then(|format| Self::dimensions_for_format(bytes, format)))
  }

  /// Returns a prefix of `bytes` that excludes any trailing data that does not form a valid top
  /// level ISO-BMFF box.
  ///
  /// Some AVIF payloads (including pageset content) include non-box trailer bytes that trip
  /// debug-only assertions inside `avif_parse`. Trimming to the last complete box keeps intrinsic
  /// probing robust without impacting real decoders (which generally ignore such trailers).
  fn trim_isobmff_trailing_bytes(bytes: &[u8]) -> &[u8] {
    let len = bytes.len();
    let mut offset = 0usize;

    while offset + 8 <= len {
      let size = u32::from_be_bytes(bytes[offset..offset + 4].try_into().unwrap());

      let box_size = match size {
        // Box extends to end of file.
        0 => len - offset,
        // Extended size stored in the next 8 bytes.
        1 => {
          if offset + 16 > len {
            break;
          }
          let ext = u64::from_be_bytes(bytes[offset + 8..offset + 16].try_into().unwrap());
          if ext < 16 {
            break;
          }
          let Ok(ext) = usize::try_from(ext) else {
            break;
          };
          ext
        }
        // Regular 32-bit size.
        n => n as usize,
      };

      // Invalid box size.
      if box_size < 8 {
        break;
      }
      let Some(next) = offset.checked_add(box_size) else {
        break;
      };
      if next > len {
        break;
      }

      offset = next;
      if offset == len {
        return bytes;
      }
    }

    if offset > 0 {
      &bytes[..offset]
    } else {
      bytes
    }
  }

  fn dimensions_for_format(bytes: &[u8], format: ImageFormat) -> Option<(u32, u32)> {
    match format {
      ImageFormat::Png => image::codecs::png::PngDecoder::new(Cursor::new(bytes))
        .ok()
        .map(|d| d.dimensions()),
      ImageFormat::Jpeg => image::codecs::jpeg::JpegDecoder::new(Cursor::new(bytes))
        .ok()
        .map(|d| d.dimensions()),
      ImageFormat::Gif => image::codecs::gif::GifDecoder::new(Cursor::new(bytes))
        .ok()
        .map(|d| d.dimensions()),
      ImageFormat::WebP => image::codecs::webp::WebPDecoder::new(Cursor::new(bytes))
        .ok()
        .map(|d| d.dimensions()),
      ImageFormat::Avif => {
        // `avif_parse` includes debug assertions that can panic when the payload includes trailing
        // bytes (which is tolerated by other decoders and observed on pageset content). Guard
        // against that so image probing doesn't crash the renderer in debug builds.
        let trimmed = Self::trim_isobmff_trailing_bytes(bytes);
        std::panic::catch_unwind(|| {
          let mut cursor = Cursor::new(trimmed);
          let data = AvifData::from_reader(&mut cursor).ok()?;
          let meta = data.primary_item_metadata().ok()?;
          Some((meta.max_frame_width.get(), meta.max_frame_height.get()))
        })
        .ok()
        .flatten()
      }
      _ => None,
    }
  }

  fn enforce_decode_limits(&self, width: u32, height: u32, url: &str) -> Result<()> {
    if self.config.max_decoded_dimension > 0
      && (width > self.config.max_decoded_dimension || height > self.config.max_decoded_dimension)
    {
      return Err(Error::Image(ImageError::DecodeFailed {
        url: url.to_string(),
        reason: format!(
          "Image dimensions {}x{} exceed maximum dimension {}",
          width, height, self.config.max_decoded_dimension
        ),
      }));
    }

    if self.config.max_decoded_pixels > 0 {
      let pixels = u64::from(width) * u64::from(height);
      if pixels > self.config.max_decoded_pixels {
        return Err(Error::Image(ImageError::DecodeFailed {
          url: url.to_string(),
          reason: format!(
            "Image dimensions {}x{} exceed pixel budget of {}",
            width, height, self.config.max_decoded_pixels
          ),
        }));
      }
    }

    Ok(())
  }

  fn decode_avif(bytes: &[u8]) -> std::result::Result<DynamicImage, AvifDecodeError> {
    check_active(RenderStage::Paint).map_err(AvifDecodeError::from)?;
    let decoder =
      AvifDecoder::from_avif(bytes).map_err(|err| AvifDecodeError::Image(Self::avif_error(err)))?;
    check_active(RenderStage::Paint).map_err(AvifDecodeError::from)?;
    let image = decoder
      .to_image()
      .map_err(|err| AvifDecodeError::Image(Self::avif_error(err)))?;
    let mut deadline_counter = 0usize;
    check_active(RenderStage::Paint).map_err(AvifDecodeError::from)?;
    Self::avif_image_to_dynamic(image, &mut deadline_counter)
  }

  fn reserve_for_bytes(bytes: u64, context: &str) -> std::result::Result<usize, image::ImageError> {
    if bytes > MAX_PIXMAP_BYTES {
      return Err(image::ImageError::IoError(io::Error::new(
        io::ErrorKind::Other,
        format!("{context}: buffer would require {bytes} bytes (limit {MAX_PIXMAP_BYTES})"),
      )));
    }
    usize::try_from(bytes).map_err(|_| {
      image::ImageError::IoError(io::Error::new(
        io::ErrorKind::Other,
        format!("{context}: buffer size {bytes} does not fit in usize"),
      ))
    })
  }

  fn reserve_image_buffer(
    bytes: u64,
    context: &str,
  ) -> std::result::Result<Vec<u8>, image::ImageError> {
    let len = Self::reserve_for_bytes(bytes, context)?;
    let mut buf = Vec::new();
    buf.try_reserve_exact(len).map_err(|err| {
      image::ImageError::IoError(io::Error::new(
        io::ErrorKind::Other,
        format!("{context}: buffer allocation failed for {bytes} bytes: {err}"),
      ))
    })?;
    Ok(buf)
  }

  fn reserve_image_buffer_u16(
    bytes: u64,
    context: &str,
  ) -> std::result::Result<Vec<u16>, image::ImageError> {
    if bytes % 2 != 0 {
      return Err(image::ImageError::IoError(io::Error::new(
        io::ErrorKind::Other,
        format!("{context}: buffer size {bytes} is not aligned to u16"),
      )));
    }
    let len = Self::reserve_for_bytes(bytes, context)? / 2;
    let mut buf: Vec<u16> = Vec::new();
    buf.try_reserve_exact(len).map_err(|err| {
      image::ImageError::IoError(io::Error::new(
        io::ErrorKind::Other,
        format!("{context}: buffer allocation failed for {bytes} bytes: {err}"),
      ))
    })?;
    Ok(buf)
  }

  fn avif_image_to_dynamic(
    image: AvifImage,
    deadline_counter: &mut usize,
  ) -> std::result::Result<DynamicImage, AvifDecodeError> {
    let dimension_error = || {
      AvifDecodeError::Image(image::ImageError::Parameter(
        image::error::ParameterError::from_kind(
          image::error::ParameterErrorKind::DimensionMismatch,
        ),
      ))
    };

    match image {
      AvifImage::Rgb8(img) => {
        let (width, height) =
          Self::avif_dimensions(img.width(), img.height()).map_err(AvifDecodeError::Image)?;
        let Some(bytes) = u64::from(width)
          .checked_mul(u64::from(height))
          .and_then(|px| px.checked_mul(3))
        else {
          return Err(AvifDecodeError::Image(Self::avif_error(
            "RGB8 dimensions overflow",
          )));
        };
        let mut buf = Self::reserve_image_buffer(bytes, "avif rgb8 data")?;
        for px in img.buf() {
          check_active_periodic(
            deadline_counter,
            IMAGE_DECODE_DEADLINE_STRIDE,
            RenderStage::Paint,
          )
          .map_err(AvifDecodeError::from)?;
          buf.extend_from_slice(&[px.r, px.g, px.b]);
        }
        image::RgbImage::from_vec(width, height, buf)
          .map(DynamicImage::ImageRgb8)
          .ok_or_else(dimension_error)
      }
      AvifImage::Rgb16(img) => {
        let (width, height) =
          Self::avif_dimensions(img.width(), img.height()).map_err(AvifDecodeError::Image)?;
        let Some(bytes) = u64::from(width)
          .checked_mul(u64::from(height))
          .and_then(|px| px.checked_mul(3))
          .and_then(|px| px.checked_mul(2))
        else {
          return Err(AvifDecodeError::Image(Self::avif_error(
            "RGB16 dimensions overflow",
          )));
        };
        let mut buf = Self::reserve_image_buffer_u16(bytes, "avif rgb16 data")?;
        for px in img.buf() {
          check_active_periodic(
            deadline_counter,
            IMAGE_DECODE_DEADLINE_STRIDE,
            RenderStage::Paint,
          )
          .map_err(AvifDecodeError::from)?;
          buf.extend_from_slice(&[px.r, px.g, px.b]);
        }
        image::ImageBuffer::from_vec(width, height, buf)
          .map(DynamicImage::ImageRgb16)
          .ok_or_else(dimension_error)
      }
      AvifImage::Rgba8(img) => {
        let (width, height) =
          Self::avif_dimensions(img.width(), img.height()).map_err(AvifDecodeError::Image)?;
        let Some(bytes) = u64::from(width)
          .checked_mul(u64::from(height))
          .and_then(|px| px.checked_mul(4))
        else {
          return Err(AvifDecodeError::Image(Self::avif_error(
            "RGBA8 dimensions overflow",
          )));
        };
        let mut buf = Self::reserve_image_buffer(bytes, "avif rgba8 data")?;
        for px in img.buf() {
          check_active_periodic(
            deadline_counter,
            IMAGE_DECODE_DEADLINE_STRIDE,
            RenderStage::Paint,
          )
          .map_err(AvifDecodeError::from)?;
          buf.extend_from_slice(&[px.r, px.g, px.b, px.a]);
        }
        image::RgbaImage::from_vec(width, height, buf)
          .map(DynamicImage::ImageRgba8)
          .ok_or_else(dimension_error)
      }
      AvifImage::Rgba16(img) => {
        let (width, height) =
          Self::avif_dimensions(img.width(), img.height()).map_err(AvifDecodeError::Image)?;
        let Some(bytes) = u64::from(width)
          .checked_mul(u64::from(height))
          .and_then(|px| px.checked_mul(4))
          .and_then(|px| px.checked_mul(2))
        else {
          return Err(AvifDecodeError::Image(Self::avif_error(
            "RGBA16 dimensions overflow",
          )));
        };
        let mut buf = Self::reserve_image_buffer_u16(bytes, "avif rgba16 data")?;
        for px in img.buf() {
          check_active_periodic(
            deadline_counter,
            IMAGE_DECODE_DEADLINE_STRIDE,
            RenderStage::Paint,
          )
          .map_err(AvifDecodeError::from)?;
          buf.extend_from_slice(&[px.r, px.g, px.b, px.a]);
        }
        image::ImageBuffer::from_vec(width, height, buf)
          .map(DynamicImage::ImageRgba16)
          .ok_or_else(dimension_error)
      }
      AvifImage::Gray8(img) => {
        let (width, height) =
          Self::avif_dimensions(img.width(), img.height()).map_err(AvifDecodeError::Image)?;
        let Some(bytes) = u64::from(width).checked_mul(u64::from(height)) else {
          return Err(AvifDecodeError::Image(Self::avif_error(
            "Gray8 dimensions overflow",
          )));
        };
        let mut buf = Self::reserve_image_buffer(bytes, "avif gray8 data")?;
        for px in img.buf() {
          check_active_periodic(
            deadline_counter,
            IMAGE_DECODE_DEADLINE_STRIDE,
            RenderStage::Paint,
          )
          .map_err(AvifDecodeError::from)?;
          buf.push(px.value());
        }
        image::ImageBuffer::from_vec(width, height, buf)
          .map(DynamicImage::ImageLuma8)
          .ok_or_else(dimension_error)
      }
      AvifImage::Gray16(img) => {
        let (width, height) =
          Self::avif_dimensions(img.width(), img.height()).map_err(AvifDecodeError::Image)?;
        let Some(bytes) = u64::from(width)
          .checked_mul(u64::from(height))
          .and_then(|px| px.checked_mul(2))
        else {
          return Err(AvifDecodeError::Image(Self::avif_error(
            "Gray16 dimensions overflow",
          )));
        };
        let mut buf = Self::reserve_image_buffer_u16(bytes, "avif gray16 data")?;
        for px in img.buf() {
          check_active_periodic(
            deadline_counter,
            IMAGE_DECODE_DEADLINE_STRIDE,
            RenderStage::Paint,
          )
          .map_err(AvifDecodeError::from)?;
          buf.push(px.value());
        }
        image::ImageBuffer::from_vec(width, height, buf)
          .map(DynamicImage::ImageLuma16)
          .ok_or_else(dimension_error)
      }
    }
  }

  fn avif_dimensions(
    width: usize,
    height: usize,
  ) -> std::result::Result<(u32, u32), image::ImageError> {
    let to_u32 = |v: usize| {
      u32::try_from(v).map_err(|_| {
        image::ImageError::Parameter(image::error::ParameterError::from_kind(
          image::error::ParameterErrorKind::DimensionMismatch,
        ))
      })
    };
    Ok((to_u32(width)?, to_u32(height)?))
  }

  fn avif_error(err: impl std::fmt::Display) -> image::ImageError {
    image::ImageError::Decoding(image::error::DecodingError::new(
      ImageFormat::Avif.into(),
      err.to_string(),
    ))
  }

  fn orientation_from_exif(value: u16) -> Option<OrientationTransform> {
    match value {
      1 => Some(OrientationTransform::IDENTITY),
      2 => Some(OrientationTransform {
        quarter_turns: 0,
        flip_x: true,
      }),
      3 => Some(OrientationTransform {
        quarter_turns: 2,
        flip_x: false,
      }),
      4 => Some(OrientationTransform {
        quarter_turns: 2,
        flip_x: true,
      }),
      5 => Some(OrientationTransform {
        quarter_turns: 1,
        flip_x: true,
      }),
      6 => Some(OrientationTransform {
        quarter_turns: 1,
        flip_x: false,
      }),
      7 => Some(OrientationTransform {
        quarter_turns: 3,
        flip_x: true,
      }),
      8 => Some(OrientationTransform {
        quarter_turns: 3,
        flip_x: false,
      }),
      _ => None,
    }
  }

  fn exif_metadata(bytes: &[u8]) -> (Option<OrientationTransform>, Option<f32>) {
    let mut cursor = std::io::Cursor::new(bytes);
    let Ok(exif) = exif::Reader::new().read_from_container(&mut cursor) else {
      return (None, None);
    };

    let orientation = exif
      .get_field(exif::Tag::Orientation, exif::In::PRIMARY)
      .and_then(|f| f.value.get_uint(0))
      .and_then(|v| Self::orientation_from_exif(v as u16));

    let resolution_unit = exif
      .get_field(exif::Tag::ResolutionUnit, exif::In::PRIMARY)
      .and_then(|f| f.value.get_uint(0))
      .unwrap_or(0);

    let rational_to_f32 = |r: exif::Rational| -> Option<f32> {
      if r.denom == 0 {
        None
      } else {
        Some(r.num as f32 / r.denom as f32)
      }
    };

    let x_res = exif
      .get_field(exif::Tag::XResolution, exif::In::PRIMARY)
      .and_then(|f| {
        if let exif::Value::Rational(ref vals) = f.value {
          vals.first().copied()
        } else {
          None
        }
      })
      .and_then(rational_to_f32);
    let y_res = exif
      .get_field(exif::Tag::YResolution, exif::In::PRIMARY)
      .and_then(|f| {
        if let exif::Value::Rational(ref vals) = f.value {
          vals.first().copied()
        } else {
          None
        }
      })
      .and_then(rational_to_f32);
    let avg_res = match (x_res, y_res) {
      (Some(x), Some(y)) if x.is_finite() && y.is_finite() && x > 0.0 && y > 0.0 => {
        Some((x + y) / 2.0)
      }
      (Some(v), None) | (None, Some(v)) if v.is_finite() && v > 0.0 => Some(v),
      _ => None,
    };

    let resolution = avg_res.and_then(|res| match resolution_unit {
      2 => Some(res / 96.0),          // inch -> dppx
      3 => Some((res * 2.54) / 96.0), // cm -> dppx
      _ => None,
    });

    (orientation, resolution)
  }

  #[allow(dead_code)]
  fn exif_orientation(bytes: &[u8]) -> Option<OrientationTransform> {
    Self::exif_metadata(bytes).0
  }

  /// Renders raw SVG content to a raster image, returning any parsed intrinsic aspect ratio
  /// information.
  pub fn render_svg_to_image(
    &self,
    svg_content: &str,
  ) -> Result<(DynamicImage, Option<f32>, bool)> {
    self.render_svg_to_image_with_url(svg_content, "SVG content")
  }

  fn render_svg_to_image_with_url(
    &self,
    svg_content: &str,
    url: &str,
  ) -> Result<(DynamicImage, Option<f32>, bool)> {
    use resvg::usvg;

    const DEFAULT_WIDTH: f32 = 300.0;
    const DEFAULT_HEIGHT: f32 = 150.0;

    check_active(RenderStage::Paint).map_err(Error::Render)?;
    let (meta_width, meta_height, meta_ratio, aspect_ratio_none) =
      svg_intrinsic_metadata(svg_content).unwrap_or((None, None, None, false));

    // Parse SVG
    let mut options = usvg::Options::default();
    if let Ok(parsed) = Url::parse(url) {
      if parsed.scheme() == "file" {
        if let Ok(path) = parsed.to_file_path() {
          if let Some(dir) = path.parent() {
            options.resources_dir = std::fs::canonicalize(dir)
              .ok()
              .or_else(|| Some(dir.to_path_buf()));
          }
        }
      }
    }
    self.enforce_svg_resource_policy(svg_content, url)?;
    let tree = usvg::Tree::from_str(svg_content, &options).map_err(|e| {
      Error::Image(ImageError::DecodeFailed {
        url: url.to_string(),
        reason: format!("Failed to parse SVG: {}", e),
      })
    })?;

    let size = tree.size();
    let source_width = size.width();
    let source_height = size.height();
    if source_width <= 0.0 || source_height <= 0.0 {
      return Err(Error::Render(RenderError::CanvasCreationFailed {
        width: source_width as u32,
        height: source_height as u32,
      }));
    }

    let ratio = meta_ratio.filter(|r| *r > 0.0);
    let (target_width, target_height) = match (
      meta_width.filter(|w| *w > 0.0),
      meta_height.filter(|h| *h > 0.0),
      ratio,
    ) {
      (Some(w), Some(h), _) => (w, h),
      (Some(w), None, Some(r)) => (w, (w / r).max(1.0)),
      (None, Some(h), Some(r)) => ((h * r).max(1.0), h),
      (Some(w), None, None) => (w, DEFAULT_HEIGHT),
      (None, Some(h), None) => (DEFAULT_WIDTH, h),
      (None, None, Some(r)) => (DEFAULT_WIDTH, (DEFAULT_WIDTH / r).max(1.0)),
      (None, None, None) => (DEFAULT_WIDTH, DEFAULT_HEIGHT),
    };

    let render_width = target_width.max(1.0).round() as u32;
    let render_height = target_height.max(1.0).round() as u32;

    self.enforce_decode_limits(render_width, render_height, url)?;
    check_active(RenderStage::Paint).map_err(Error::Render)?;

    // Render SVG to pixmap, scaling to the target intrinsic dimensions when needed
    let mut pixmap = new_pixmap(render_width, render_height).ok_or(Error::Render(
      RenderError::CanvasCreationFailed {
        width: render_width,
        height: render_height,
      },
    ))?;

    let render_width_f = render_width as f32;
    let render_height_f = render_height as f32;

    let scale_x = render_width_f / source_width;
    let scale_y = render_height_f / source_height;
    let transform = if aspect_ratio_none {
      tiny_skia::Transform::from_scale(scale_x, scale_y)
    } else {
      let scale = scale_x.min(scale_y);
      let translate_x = (render_width_f - source_width * scale) * 0.5;
      let translate_y = (render_height_f - source_height * scale) * 0.5;
      tiny_skia::Transform::from_row(scale, 0.0, 0.0, scale, translate_x, translate_y)
    };
    check_active(RenderStage::Paint).map_err(Error::Render)?;
    resvg::render(&tree, transform, &mut pixmap.as_mut());
    check_active(RenderStage::Paint).map_err(Error::Render)?;

    // Convert pixmap to image
    let rgba_data = pixmap.take();
    let img =
      image::RgbaImage::from_raw(render_width, render_height, rgba_data).ok_or_else(|| {
        Error::Image(ImageError::DecodeFailed {
          url: url.to_string(),
          reason: "Failed to create image from SVG pixmap".to_string(),
        })
      })?;

    let ratio = if aspect_ratio_none {
      None
    } else {
      ratio.or_else(|| {
        if render_height > 0 {
          Some(render_width as f32 / render_height as f32)
        } else {
          None
        }
      })
    };

    Ok((
      image::DynamicImage::ImageRgba8(img),
      ratio,
      aspect_ratio_none,
    ))
  }
}

/// Returns intrinsic metadata extracted from the SVG root element: explicit width/height when
/// present (absolute units only), an intrinsic aspect ratio (if not disabled), and whether
/// preserveAspectRatio="none" was specified.
fn svg_intrinsic_metadata(
  svg_content: &str,
) -> Option<(Option<f32>, Option<f32>, Option<f32>, bool)> {
  let doc = Document::parse(svg_content).ok()?;
  let root = doc.root_element();
  if !root.tag_name().name().eq_ignore_ascii_case("svg") {
    return None;
  }

  let intrinsic = svg_intrinsic_dimensions_from_attributes(
    root.attribute("width"),
    root.attribute("height"),
    root.attribute("viewBox"),
    root.attribute("preserveAspectRatio"),
  );

  Some((
    intrinsic.width,
    intrinsic.height,
    intrinsic.aspect_ratio,
    intrinsic.aspect_ratio_none,
  ))
}

// ============================================================================
// URL Resolution
// ============================================================================

pub(crate) fn resolve_against_base(base: &str, reference: &str) -> Option<String> {
  // Normalize file:// bases that point to directories so Url::join keeps the directory segment.
  let mut base_candidate = base.to_string();
  if base_candidate.starts_with("file://") {
    let path = &base_candidate["file://".len()..];
    if Path::new(path).is_dir() && !base_candidate.ends_with('/') {
      base_candidate.push('/');
    }
  }

  let mut base_url = Url::parse(&base_candidate)
    .or_else(|_| {
      Url::from_file_path(&base_candidate).map_err(|()| url::ParseError::RelativeUrlWithoutBase)
    })
    .ok()?;

  if base_url.scheme() == "file" {
    if let Ok(path) = base_url.to_file_path() {
      if path.is_dir() && !base_url.path().ends_with('/') {
        let mut path_str = base_url.path().to_string();
        path_str.push('/');
        base_url.set_path(&path_str);
      }
    }
  }

  base_url.join(reference).ok().map(|u| u.to_string())
}

// ============================================================================
// Trait Implementations
// ============================================================================

impl Default for ImageCache {
  fn default() -> Self {
    Self::new()
  }
}

impl Clone for ImageCache {
  fn clone(&self) -> Self {
    Self {
      cache: Arc::clone(&self.cache),
      in_flight: Arc::clone(&self.in_flight),
      meta_cache: Arc::clone(&self.meta_cache),
      raw_cache: Arc::clone(&self.raw_cache),
      meta_in_flight: Arc::clone(&self.meta_in_flight),
      svg_pixmap_cache: Arc::clone(&self.svg_pixmap_cache),
      raster_pixmap_cache: Arc::clone(&self.raster_pixmap_cache),
      base_url: self.base_url.clone(),
      fetcher: Arc::clone(&self.fetcher),
      config: self.config,
      diagnostics: self.diagnostics.clone(),
      resource_context: self.resource_context.clone(),
    }
  }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
  use super::*;
  use crate::render_control::RenderDeadline;
  use crate::style::types::OrientationTransform;
  use base64::Engine;
  use image::codecs::png::PngEncoder;
  use image::ColorType;
  use image::ImageEncoder;
  use image::RgbaImage;
  use std::path::PathBuf;
  use std::time::Duration;
  use std::time::SystemTime;

  #[test]
  fn image_cache_diagnostics_survives_poisoned_lock() {
    let result = std::panic::catch_unwind(|| {
      let _guard = IMAGE_CACHE_DIAGNOSTICS.lock().unwrap();
      panic!("poison image cache diagnostics lock");
    });
    assert!(result.is_err(), "expected panic to be caught");

    assert!(
      IMAGE_CACHE_DIAGNOSTICS.is_poisoned(),
      "expected image cache diagnostics mutex to be poisoned"
    );

    enable_image_cache_diagnostics();
    record_image_cache_request();
    let stats = take_image_cache_diagnostics().expect("diagnostics enabled");
    assert_eq!(stats.requests, 1);
  }

  #[test]
  fn decode_inflight_recovers_from_poisoned_lock() {
    let inflight = DecodeInFlight::new();
    let result = std::panic::catch_unwind(|| {
      let _guard = inflight.result.lock().unwrap();
      panic!("poison decode inflight lock");
    });
    assert!(result.is_err(), "expected panic to be caught");

    let deadline = RenderDeadline::new(Some(Duration::from_millis(50)), None);
    render_control::with_deadline(Some(&deadline), || {
      inflight.set(SharedImageResult::Error(Error::Render(
        RenderError::Timeout {
          stage: RenderStage::Paint,
          elapsed: Duration::from_millis(0),
        },
      )));
      let err = match inflight.wait("https://example.com/image.png") {
        Ok(_) => panic!("expected error result"),
        Err(err) => err,
      };
      assert!(matches!(err, Error::Render(RenderError::Timeout { .. })));
    });
  }

  #[test]
  fn probe_inflight_recovers_from_poisoned_lock() {
    let inflight = ProbeInFlight::new();
    let result = std::panic::catch_unwind(|| {
      let _guard = inflight.result.lock().unwrap();
      panic!("poison probe inflight lock");
    });
    assert!(result.is_err(), "expected panic to be caught");

    let deadline = RenderDeadline::new(Some(Duration::from_millis(50)), None);
    render_control::with_deadline(Some(&deadline), || {
      inflight.set(SharedMetaResult::Error(Error::Render(
        RenderError::Timeout {
          stage: RenderStage::Paint,
          elapsed: Duration::from_millis(0),
        },
      )));
      let err = match inflight.wait("https://example.com/image.png") {
        Ok(_) => panic!("expected error result"),
        Err(err) => err,
      };
      assert!(matches!(err, Error::Render(RenderError::Timeout { .. })));
    });
  }

  #[test]
  fn http_403_image_reports_resource_error_with_status() {
    use std::io::{Read, Write};
    use std::net::TcpListener;
    use std::time::Duration;

    let listener = match TcpListener::bind("127.0.0.1:0") {
      Ok(listener) => listener,
      Err(err)
        if matches!(
          err.kind(),
          std::io::ErrorKind::PermissionDenied | std::io::ErrorKind::AddrNotAvailable
        ) =>
      {
        eprintln!("skipping http_403_image_reports_resource_error_with_status: cannot bind localhost: {err}");
        return;
      }
      Err(err) => panic!("bind localhost: {err}"),
    };
    let addr = listener.local_addr().expect("listener addr");
    let url = format!("http://{addr}/blocked.png");

    let server = std::thread::spawn(move || {
      let (mut stream, _) = listener.accept().expect("accept");
      let _ = stream.set_read_timeout(Some(Duration::from_secs(1)));
      let mut buf = [0u8; 1024];
      let _ = stream.read(&mut buf);

      let body = "<html>Forbidden</html>";
      let response = format!(
        "HTTP/1.1 403 Forbidden\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
        body.len(),
        body
      );
      stream
        .write_all(response.as_bytes())
        .expect("write response");
      let _ = stream.flush();
    });

    let diagnostics = Arc::new(Mutex::new(RenderDiagnostics::default()));
    let mut cache = ImageCache::with_fetcher(Arc::new(HttpFetcher::new()));
    cache.set_diagnostics_sink(Some(Arc::clone(&diagnostics)));

    let err = match cache.load(&url) {
      Ok(_) => panic!("image load should fail"),
      Err(err) => err,
    };
    match err {
      Error::Resource(ref res) => {
        assert_eq!(res.status, Some(403));
        assert_eq!(res.final_url.as_deref(), Some(url.as_str()));
      }
      other => panic!("expected resource error, got {other:?}"),
    }

    let diag = diagnostics.lock().unwrap().clone();
    let entry = diag
      .fetch_errors
      .iter()
      .find(|e| e.kind == ResourceKind::Image && e.url == url)
      .expect("diagnostics entry");
    assert_eq!(entry.status, Some(403));
    assert_eq!(entry.final_url.as_deref(), Some(url.as_str()));

    server.join().unwrap();
  }

  #[test]
  fn image_cache_load_about_blank_returns_transparent_placeholder() {
    #[derive(Clone)]
    struct PanicFetcher;

    impl ResourceFetcher for PanicFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        panic!("fetch should not be called for about: URLs");
      }

      fn fetch_partial_with_context(
        &self,
        _kind: FetchContextKind,
        _url: &str,
        _max_bytes: usize,
      ) -> Result<FetchedResource> {
        panic!("partial fetch should not be called for about: URLs");
      }
    }

    let cache = ImageCache::with_fetcher(Arc::new(PanicFetcher));
    let image = cache
      .load("about:blank")
      .expect("about:blank placeholder loads");
    assert!(!image.is_vector);
    assert_eq!(image.dimensions(), (1, 1));

    let rgba = image.image.to_rgba8();
    assert_eq!(rgba.dimensions(), (1, 1));
    assert_eq!(rgba.get_pixel(0, 0).0, [0, 0, 0, 0]);
  }

  #[test]
  fn image_cache_load_empty_url_returns_transparent_placeholder() {
    #[derive(Clone)]
    struct PanicFetcher;

    impl ResourceFetcher for PanicFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        panic!("fetch should not be called for empty URLs");
      }

      fn fetch_partial_with_context(
        &self,
        _kind: FetchContextKind,
        _url: &str,
        _max_bytes: usize,
      ) -> Result<FetchedResource> {
        panic!("partial fetch should not be called for empty URLs");
      }
    }

    let cache = ImageCache::with_fetcher(Arc::new(PanicFetcher));
    let image = cache.load(" \t\r\n").expect("empty url placeholder loads");
    assert!(!image.is_vector);
    assert_eq!(image.dimensions(), (1, 1));

    let rgba = image.image.to_rgba8();
    assert_eq!(rgba.dimensions(), (1, 1));
    assert_eq!(rgba.get_pixel(0, 0).0, [0, 0, 0, 0]);
  }

  #[test]
  fn image_cache_probe_about_blank_returns_placeholder_metadata() {
    #[derive(Clone)]
    struct PanicFetcher;

    impl ResourceFetcher for PanicFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        panic!("fetch should not be called for about: URLs");
      }

      fn fetch_partial_with_context(
        &self,
        _kind: FetchContextKind,
        _url: &str,
        _max_bytes: usize,
      ) -> Result<FetchedResource> {
        panic!("partial fetch should not be called for about: URLs");
      }
    }

    let cache = ImageCache::with_fetcher(Arc::new(PanicFetcher));
    let meta = cache
      .probe("about:blank")
      .expect("about:blank probe succeeds");
    assert!(!meta.is_vector);
    assert_eq!(meta.dimensions(), (1, 1));
    assert_eq!(
      meta.intrinsic_ratio(OrientationTransform::IDENTITY),
      Some(1.0)
    );
  }

  #[test]
  fn image_cache_load_empty_http_body_returns_placeholder_without_diagnostics() {
    #[derive(Clone)]
    struct EmptyBodyFetcher;

    impl ResourceFetcher for EmptyBodyFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        Err(Error::Resource(
          crate::error::ResourceError::new(url, "empty HTTP response body").with_status(200),
        ))
      }
    }

    let diagnostics = Arc::new(Mutex::new(RenderDiagnostics::default()));
    let mut cache = ImageCache::with_fetcher(Arc::new(EmptyBodyFetcher));
    cache.set_diagnostics_sink(Some(Arc::clone(&diagnostics)));

    let image = cache
      .load("https://example.com/pixel.png")
      .expect("empty-body image loads as placeholder");
    assert_eq!(image.dimensions(), (1, 1));

    let diag = diagnostics.lock().unwrap().clone();
    assert!(
      diag.fetch_errors.is_empty(),
      "placeholder images should not be recorded as fetch errors"
    );
  }

  #[test]
  fn image_cache_probe_empty_http_body_returns_placeholder_metadata_without_diagnostics() {
    #[derive(Clone)]
    struct EmptyBodyFetcher;

    impl ResourceFetcher for EmptyBodyFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        Err(Error::Resource(
          crate::error::ResourceError::new(url, "empty HTTP response body").with_status(200),
        ))
      }
    }

    let diagnostics = Arc::new(Mutex::new(RenderDiagnostics::default()));
    let mut cache = ImageCache::with_fetcher(Arc::new(EmptyBodyFetcher));
    cache.set_diagnostics_sink(Some(Arc::clone(&diagnostics)));

    let meta = cache
      .probe("https://example.com/pixel.png")
      .expect("empty-body image probe returns placeholder");
    assert_eq!(meta.dimensions(), (1, 1));

    let diag = diagnostics.lock().unwrap().clone();
    assert!(diag.fetch_errors.is_empty());
    assert!(
      diag.invalid_images.is_empty(),
      "empty-body placeholder images should not be treated as invalid images"
    );
  }

  #[test]
  fn image_cache_html_payload_returns_placeholder_without_diagnostics() {
    #[derive(Clone)]
    struct HtmlFetcher;

    impl ResourceFetcher for HtmlFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        let mut res = FetchedResource::new(
          b"<!doctype html><html><head></head><body>not an image</body></html>".to_vec(),
          Some("image/png".to_string()),
        );
        res.status = Some(200);
        Ok(res)
      }
    }

    let diagnostics = Arc::new(Mutex::new(RenderDiagnostics::default()));
    let mut cache = ImageCache::with_fetcher(Arc::new(HtmlFetcher));
    cache.set_diagnostics_sink(Some(Arc::clone(&diagnostics)));

    let image = cache
      .load("https://example.com/not-really.png")
      .expect("HTML body treated as placeholder image");
    assert_eq!(image.dimensions(), (1, 1));

    let meta = cache
      .probe("https://example.com/not-really.png")
      .expect("HTML body treated as placeholder metadata");
    assert_eq!(meta.dimensions(), (1, 1));

    let diag = diagnostics.lock().unwrap().clone();
    assert!(diag.fetch_errors.is_empty());
    assert!(
      diag.invalid_images
        .iter()
        .any(|u| u == "https://example.com/not-really.png"),
      "invalid image URLs should be tracked in diagnostics.invalid_images"
    );
  }

  #[test]
  fn image_cache_html_payload_with_206_status_returns_placeholder_without_diagnostics() {
    #[derive(Clone)]
    struct PartialHtmlFetcher;

    impl ResourceFetcher for PartialHtmlFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        panic!("full fetch should not be required for HTML payload probe");
      }

      fn fetch_partial_with_context(
        &self,
        _kind: FetchContextKind,
        _url: &str,
        _max_bytes: usize,
      ) -> Result<FetchedResource> {
        let mut res = FetchedResource::new(
          b"<!doctype html><html><body>partial content</body></html>".to_vec(),
          Some("image/png".to_string()),
        );
        res.status = Some(206);
        Ok(res)
      }
    }

    let diagnostics = Arc::new(Mutex::new(RenderDiagnostics::default()));
    let mut cache = ImageCache::with_fetcher(Arc::new(PartialHtmlFetcher));
    cache.set_diagnostics_sink(Some(Arc::clone(&diagnostics)));

    let meta = cache
      .probe("https://example.com/not-an-image.png")
      .expect("HTML probe returns placeholder metadata");
    assert_eq!(meta.dimensions(), (1, 1));

    let diag = diagnostics.lock().unwrap().clone();
    assert!(diag.fetch_errors.is_empty());
    assert!(
      diag.invalid_images
        .iter()
        .any(|u| u == "https://example.com/not-an-image.png"),
      "invalid image URLs should be tracked in diagnostics.invalid_images"
    );
  }

  fn padded_png() -> Vec<u8> {
    // 1x1 RGBA PNG, padded with trailing bytes so that the probe must use a prefix fetch.
    const PNG_1X1: &[u8] = &[
      0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0x00, 0x00, 0x00, 0x0D, 0x49, 0x48, 0x44,
      0x52, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x08, 0x04, 0x00, 0x00, 0x00, 0xB5,
      0x1C, 0x0C, 0x02, 0x00, 0x00, 0x00, 0x0B, 0x49, 0x44, 0x41, 0x54, 0x78, 0xDA, 0x63, 0xFC,
      0xFF, 0x1F, 0x00, 0x03, 0x03, 0x01, 0x02, 0x94, 0x60, 0xC4, 0x1B, 0x00, 0x00, 0x00, 0x00,
      0x49, 0x45, 0x4E, 0x44, 0xAE, 0x42, 0x60, 0x82,
    ];

    let mut bytes = PNG_1X1.to_vec();
    bytes.resize(128 * 1024, 0);
    bytes
  }

  #[cfg(feature = "disk_cache")]
  #[test]
  fn image_probe_persists_metadata_to_disk_cache_and_reuses_it() {
    use crate::resource::{CachingFetcherConfig, DiskCacheConfig, DiskCachingFetcher};
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    #[derive(Clone)]
    struct CountingPartialFetcher {
      calls: Arc<AtomicUsize>,
      body: Arc<Vec<u8>>,
    }

    impl ResourceFetcher for CountingPartialFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        panic!("expected partial fetch only");
      }

      fn fetch_partial_with_context(
        &self,
        _kind: FetchContextKind,
        url: &str,
        max_bytes: usize,
      ) -> Result<FetchedResource> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        let mut res =
          FetchedResource::new(self.body.as_ref().clone(), Some("image/png".to_string()));
        res.status = Some(200);
        res.final_url = Some(url.to_string());
        if res.bytes.len() > max_bytes {
          res.bytes.truncate(max_bytes);
        }
        Ok(res)
      }
    }

    let tmp = tempfile::tempdir().expect("tempdir");
    let cache_dir = tmp.path().join("assets");
    let url = "https://example.com/probe.png";
    let body = Arc::new(padded_png());

    let calls = Arc::new(AtomicUsize::new(0));
    let disk = DiskCachingFetcher::with_configs(
      CountingPartialFetcher {
        calls: Arc::clone(&calls),
        body: Arc::clone(&body),
      },
      &cache_dir,
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        ..DiskCacheConfig::default()
      },
    );
    let cache = ImageCache::with_fetcher(Arc::new(disk));
    let meta = cache.probe(url).expect("probe succeeds");
    assert_eq!(meta.dimensions(), (1, 1));
    assert_eq!(
      calls.load(Ordering::SeqCst),
      1,
      "first probe should hit the network via fetch_partial"
    );

    let calls2 = Arc::new(AtomicUsize::new(0));
    let disk2 = DiskCachingFetcher::with_configs(
      CountingPartialFetcher {
        calls: Arc::clone(&calls2),
        body: Arc::clone(&body),
      },
      &cache_dir,
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        ..DiskCacheConfig::default()
      },
    );
    let cache2 = ImageCache::with_fetcher(Arc::new(disk2));
    let meta2 = cache2.probe(url).expect("probe succeeds from disk");
    assert_eq!(meta2.dimensions(), (1, 1));
    assert_eq!(
      calls2.load(Ordering::SeqCst),
      0,
      "second probe should reuse persisted probe metadata without network calls"
    );
  }

  #[cfg(feature = "disk_cache")]
  #[test]
  fn image_probe_resolved_reuses_persisted_metadata_to_disk_cache() {
    use crate::resource::{CachingFetcherConfig, DiskCacheConfig, DiskCachingFetcher};
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    #[derive(Clone)]
    struct CountingPartialFetcher {
      calls: Arc<AtomicUsize>,
      body: Arc<Vec<u8>>,
    }

    impl ResourceFetcher for CountingPartialFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        panic!("expected partial fetch only");
      }

      fn fetch_partial_with_context(
        &self,
        _kind: FetchContextKind,
        url: &str,
        max_bytes: usize,
      ) -> Result<FetchedResource> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        let mut res =
          FetchedResource::new(self.body.as_ref().clone(), Some("image/png".to_string()));
        res.status = Some(200);
        res.final_url = Some(url.to_string());
        if res.bytes.len() > max_bytes {
          res.bytes.truncate(max_bytes);
        }
        Ok(res)
      }
    }

    let tmp = tempfile::tempdir().expect("tempdir");
    let cache_dir = tmp.path().join("assets");
    let url = "https://example.com/probe_resolved.png";
    let body = Arc::new(padded_png());

    let calls = Arc::new(AtomicUsize::new(0));
    let disk = DiskCachingFetcher::with_configs(
      CountingPartialFetcher {
        calls: Arc::clone(&calls),
        body: Arc::clone(&body),
      },
      &cache_dir,
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        ..DiskCacheConfig::default()
      },
    );
    let cache = ImageCache::with_fetcher(Arc::new(disk));
    let meta = cache.probe_resolved(url).expect("probe succeeds");
    assert_eq!(meta.dimensions(), (1, 1));
    assert_eq!(
      calls.load(Ordering::SeqCst),
      1,
      "first resolved probe should hit the network via fetch_partial"
    );

    let calls2 = Arc::new(AtomicUsize::new(0));
    let disk2 = DiskCachingFetcher::with_configs(
      CountingPartialFetcher {
        calls: Arc::clone(&calls2),
        body: Arc::clone(&body),
      },
      &cache_dir,
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        ..DiskCacheConfig::default()
      },
    );
    let cache2 = ImageCache::with_fetcher(Arc::new(disk2));
    let meta2 = cache2
      .probe_resolved(url)
      .expect("probe succeeds from disk");
    assert_eq!(meta2.dimensions(), (1, 1));
    assert_eq!(
      calls2.load(Ordering::SeqCst),
      0,
      "second resolved probe should reuse persisted probe metadata without network calls"
    );
  }

  #[cfg(feature = "disk_cache")]
  #[test]
  fn image_probe_artifact_inherits_stored_at_from_cached_resource() {
    use crate::resource::{
      CachingFetcherConfig, DiskCacheConfig, DiskCachingFetcher, FetchDestination, FetchRequest,
    };
    use std::fs;
    use std::sync::Arc;

    #[derive(Clone)]
    struct FullFetcher {
      body: Arc<Vec<u8>>,
    }

    impl ResourceFetcher for FullFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        let mut res =
          FetchedResource::new(self.body.as_ref().clone(), Some("image/png".to_string()));
        res.status = Some(200);
        res.final_url = Some(url.to_string());
        Ok(res)
      }
    }

    #[derive(Clone)]
    struct PanicFetcher;

    impl ResourceFetcher for PanicFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        panic!("network fetch should not be called");
      }

      fn fetch_partial_with_context(
        &self,
        _kind: FetchContextKind,
        _url: &str,
        _max_bytes: usize,
      ) -> Result<FetchedResource> {
        panic!("network partial fetch should not be called");
      }
    }

    let tmp = tempfile::tempdir().expect("tempdir");
    let cache_dir = tmp.path().join("assets");
    let url = "https://example.com/aged.png";
    let body = Arc::new(padded_png());

    // Persist the full image bytes into the disk cache so the probe can later derive metadata from
    // disk without touching the network.
    let disk = DiskCachingFetcher::with_configs(
      FullFetcher {
        body: Arc::clone(&body),
      },
      &cache_dir,
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        ..DiskCacheConfig::default()
      },
    );
    disk
      .fetch_with_request(FetchRequest::new(url, FetchDestination::Image))
      .expect("seed fetch");

    // Locate the primary cached image entry and force its `stored_at` to be very old so we can
    // assert the derived probe artifact inherits that age rather than refreshing it.
    let mut resource_meta_path = None;
    for entry in fs::read_dir(&cache_dir).expect("read cache dir") {
      let path = entry.expect("dir entry").path();
      if !path.to_string_lossy().ends_with(".bin.meta") {
        continue;
      }
      let bytes = fs::read(&path).expect("read meta");
      let value: serde_json::Value = serde_json::from_slice(&bytes).expect("parse meta json");
      let ct = value
        .get("content_type")
        .and_then(|v| v.as_str())
        .unwrap_or("");
      if ct == "image/png" {
        resource_meta_path = Some(path);
        break;
      }
    }
    let resource_meta_path = resource_meta_path.expect("cached image meta file");
    let meta_bytes = fs::read(&resource_meta_path).expect("read meta bytes");
    let mut value: serde_json::Value =
      serde_json::from_slice(&meta_bytes).expect("parse meta json");
    value["stored_at"] = serde_json::Value::from(0u64);
    fs::write(
      &resource_meta_path,
      serde_json::to_vec(&value).expect("serialize meta"),
    )
    .expect("write meta");

    // New fetcher instance (empty memory cache). The probe should be satisfied entirely from disk.
    let disk2 = DiskCachingFetcher::with_configs(
      PanicFetcher,
      &cache_dir,
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        ..DiskCacheConfig::default()
      },
    );
    let cache = ImageCache::with_fetcher(Arc::new(disk2));
    let meta = cache.probe(url).expect("probe succeeds");
    assert_eq!(meta.dimensions(), (1, 1));

    // The persisted probe metadata should inherit the primary resource `stored_at` timestamp so it
    // becomes stale alongside the cached image bytes.
    let mut probe_meta_path = None;
    for entry in fs::read_dir(&cache_dir).expect("read cache dir") {
      let path = entry.expect("dir entry").path();
      if !path.to_string_lossy().ends_with(".bin.meta") {
        continue;
      }
      let bytes = fs::read(&path).expect("read meta");
      let value: serde_json::Value = serde_json::from_slice(&bytes).expect("parse meta json");
      let ct = value
        .get("content_type")
        .and_then(|v| v.as_str())
        .unwrap_or("");
      if ct == "application/x-fastrender-image-probe+json" {
        probe_meta_path = Some(path);
        break;
      }
    }
    let probe_meta_path = probe_meta_path.expect("probe meta file");
    let probe_bytes = fs::read(&probe_meta_path).expect("read probe meta");
    let probe_value: serde_json::Value =
      serde_json::from_slice(&probe_bytes).expect("parse probe meta json");
    assert_eq!(
      probe_value.get("stored_at").and_then(|v| v.as_u64()),
      Some(0),
      "probe metadata should inherit stored_at from the cached resource"
    );
  }

  #[cfg(feature = "disk_cache")]
  #[test]
  fn image_probe_disk_cache_respects_staleness_and_recovers_from_corruption() {
    use crate::resource::{CachingFetcherConfig, DiskCacheConfig, DiskCachingFetcher};
    use std::fs;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;
    use std::time::Duration;

    #[derive(Clone)]
    struct CountingPartialFetcher {
      calls: Arc<AtomicUsize>,
      body: Arc<Vec<u8>>,
    }

    impl ResourceFetcher for CountingPartialFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        panic!("expected partial fetch only");
      }

      fn fetch_partial_with_context(
        &self,
        _kind: FetchContextKind,
        url: &str,
        max_bytes: usize,
      ) -> Result<FetchedResource> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        let mut res =
          FetchedResource::new(self.body.as_ref().clone(), Some("image/png".to_string()));
        res.status = Some(200);
        res.final_url = Some(url.to_string());
        if res.bytes.len() > max_bytes {
          res.bytes.truncate(max_bytes);
        }
        Ok(res)
      }
    }

    let tmp = tempfile::tempdir().expect("tempdir");
    let cache_dir = tmp.path().join("assets");
    let url = "https://example.com/stale.png";
    let body = Arc::new(padded_png());

    let calls = Arc::new(AtomicUsize::new(0));
    let disk = DiskCachingFetcher::with_configs(
      CountingPartialFetcher {
        calls: Arc::clone(&calls),
        body: Arc::clone(&body),
      },
      &cache_dir,
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        max_age: Some(Duration::from_secs(1)),
        ..DiskCacheConfig::default()
      },
    );
    let cache = ImageCache::with_fetcher(Arc::new(disk));
    let meta = cache.probe(url).expect("probe succeeds");
    assert_eq!(meta.dimensions(), (1, 1));
    assert_eq!(calls.load(Ordering::SeqCst), 1);

    // Locate the probe-metadata cache entry and force it to be stale by setting stored_at=0.
    let mut meta_path = None;
    for entry in fs::read_dir(&cache_dir).expect("read cache dir") {
      let path = entry.expect("dir entry").path();
      if !path.to_string_lossy().ends_with(".bin.meta") {
        continue;
      }
      let bytes = fs::read(&path).expect("read meta");
      let value: serde_json::Value = serde_json::from_slice(&bytes).expect("parse meta json");
      let ct = value
        .get("content_type")
        .and_then(|v| v.as_str())
        .unwrap_or("");
      if ct == "application/x-fastrender-image-probe+json" {
        meta_path = Some(path);
        break;
      }
    }
    let meta_path = meta_path.expect("probe metadata entry meta file");
    let meta_bytes = fs::read(&meta_path).expect("read meta bytes");
    let mut value: serde_json::Value =
      serde_json::from_slice(&meta_bytes).expect("parse meta json");
    value["stored_at"] = serde_json::Value::from(0u64);
    fs::write(
      &meta_path,
      serde_json::to_vec(&value).expect("serialize meta"),
    )
    .expect("write meta");

    let calls2 = Arc::new(AtomicUsize::new(0));
    let disk2 = DiskCachingFetcher::with_configs(
      CountingPartialFetcher {
        calls: Arc::clone(&calls2),
        body: Arc::clone(&body),
      },
      &cache_dir,
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        max_age: Some(Duration::from_secs(1)),
        ..DiskCacheConfig::default()
      },
    );
    let cache2 = ImageCache::with_fetcher(Arc::new(disk2));
    let meta2 = cache2.probe(url).expect("probe succeeds after staleness");
    assert_eq!(meta2.dimensions(), (1, 1));
    assert_eq!(
      calls2.load(Ordering::SeqCst),
      1,
      "stale probe metadata should trigger a network refresh"
    );

    // Corrupt the on-disk probe data while keeping the length intact, then ensure we can recover.
    let len = value.get("len").and_then(|v| v.as_u64()).unwrap_or(0) as usize;
    let meta_string = meta_path.to_string_lossy();
    let data_path =
      std::path::PathBuf::from(meta_string.strip_suffix(".meta").expect("meta path suffix"));
    fs::write(&data_path, vec![0u8; len.max(1)]).expect("write corrupt data");

    let calls3 = Arc::new(AtomicUsize::new(0));
    let disk3 = DiskCachingFetcher::with_configs(
      CountingPartialFetcher {
        calls: Arc::clone(&calls3),
        body: Arc::clone(&body),
      },
      &cache_dir,
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        max_age: Some(Duration::from_secs(1)),
        ..DiskCacheConfig::default()
      },
    );
    let cache3 = ImageCache::with_fetcher(Arc::new(disk3));
    let meta3 = cache3.probe(url).expect("probe succeeds after corruption");
    assert_eq!(meta3.dimensions(), (1, 1));
    assert_eq!(
      calls3.load(Ordering::SeqCst),
      1,
      "corrupt probe metadata should be treated as a miss and refreshed"
    );
  }

  fn read_http_headers(stream: &mut std::net::TcpStream) -> String {
    use std::io::Read;

    let mut buf = Vec::new();
    let mut scratch = [0u8; 1024];
    loop {
      match stream.read(&mut scratch) {
        Ok(0) => break,
        Ok(n) => {
          buf.extend_from_slice(&scratch[..n]);
          if buf.windows(4).any(|w| w == b"\r\n\r\n") || buf.len() > 32 * 1024 {
            break;
          }
        }
        Err(_) => break,
      }
    }
    String::from_utf8_lossy(&buf).to_string()
  }

  fn extract_range_header(req: &str) -> Option<String> {
    req.lines().find_map(|line| {
      let (name, value) = line.split_once(':')?;
      if name.trim().eq_ignore_ascii_case("range") {
        Some(value.trim().to_string())
      } else {
        None
      }
    })
  }

  fn parse_range_end(range: &str) -> Option<usize> {
    let range = range.trim();
    let range = range.strip_prefix("bytes=")?;
    let (_start, end) = range.split_once('-')?;
    end.trim().parse::<usize>().ok()
  }

  #[test]
  fn image_probe_uses_http_range_requests() {
    use std::io::Write;
    use std::net::TcpListener;
    use std::time::Duration;

    let listener = match TcpListener::bind("127.0.0.1:0") {
      Ok(listener) => listener,
      Err(err)
        if matches!(
          err.kind(),
          std::io::ErrorKind::PermissionDenied | std::io::ErrorKind::AddrNotAvailable
        ) =>
      {
        eprintln!("skipping image_probe_uses_http_range_requests: cannot bind localhost: {err}");
        return;
      }
      Err(err) => panic!("bind localhost: {err}"),
    };
    let addr = listener.local_addr().expect("listener addr");
    let url = format!("http://{addr}/probe.png");
    let body = padded_png();

    let server = std::thread::spawn(move || {
      let (mut stream, _) = listener.accept().expect("accept");
      let _ = stream.set_read_timeout(Some(Duration::from_secs(1)));
      let req = read_http_headers(&mut stream);

      let range = extract_range_header(&req).expect("missing Range header");
      let end = parse_range_end(&range).expect("invalid Range header");
      let prefix_len = (end + 1).min(body.len());
      let prefix = &body[..prefix_len];

      let header = format!(
        "HTTP/1.1 206 Partial Content\r\nContent-Type: image/png\r\nContent-Length: {}\r\nContent-Range: bytes 0-{}/{}\r\nConnection: close\r\n\r\n",
        prefix.len(),
        prefix.len().saturating_sub(1),
        body.len()
      );
      stream.write_all(header.as_bytes()).expect("write header");
      stream.write_all(prefix).expect("write body");
      let _ = stream.flush();
    });

    let cache = ImageCache::with_fetcher(Arc::new(HttpFetcher::new()));
    let meta = cache.probe(&url).expect("probe succeeds");
    assert_eq!(meta.dimensions(), (1, 1));

    server.join().unwrap();
  }

  #[test]
  fn image_probe_partial_fetch_handles_range_ignored() {
    use std::io::Write;
    use std::net::TcpListener;
    use std::time::Duration;

    let listener = match TcpListener::bind("127.0.0.1:0") {
      Ok(listener) => listener,
      Err(err)
        if matches!(
          err.kind(),
          std::io::ErrorKind::PermissionDenied | std::io::ErrorKind::AddrNotAvailable
        ) =>
      {
        eprintln!(
          "skipping image_probe_partial_fetch_handles_range_ignored: cannot bind localhost: {err}"
        );
        return;
      }
      Err(err) => panic!("bind localhost: {err}"),
    };
    let addr = listener.local_addr().expect("listener addr");
    let url = format!("http://{addr}/probe.png");
    let body = padded_png();

    let server = std::thread::spawn(move || {
      let (mut stream, _) = listener.accept().expect("accept");
      let _ = stream.set_read_timeout(Some(Duration::from_secs(1)));
      let req = read_http_headers(&mut stream);

      let range = extract_range_header(&req).expect("missing Range header");
      let end = parse_range_end(&range).expect("invalid Range header");
      let prefix_len = (end + 1).min(body.len());
      let prefix = &body[..prefix_len];

      // Ignore Range and respond with 200 + the full content-length. Send only the prefix
      // immediately; if the client tried to read the entire body it would stall and hit the
      // request timeout below.
      let header = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: image/png\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
      );
      stream.write_all(header.as_bytes()).expect("write header");
      stream.write_all(prefix).expect("write prefix");
      let _ = stream.flush();
      std::thread::sleep(Duration::from_millis(500));
      let _ = stream.write_all(&body[prefix_len..]);
      let _ = stream.flush();
    });

    let cache = ImageCache::with_fetcher(Arc::new(
      HttpFetcher::new().with_timeout(Duration::from_millis(150)),
    ));
    let meta = cache.probe(&url).expect("probe succeeds");
    assert_eq!(meta.dimensions(), (1, 1));

    server.join().unwrap();
  }

  #[test]
  fn image_probe_partial_fetch_falls_back_on_http_405() {
    use std::io::Write;
    use std::net::TcpListener;
    use std::time::Duration;

    let listener = match TcpListener::bind("127.0.0.1:0") {
      Ok(listener) => listener,
      Err(err)
        if matches!(
          err.kind(),
          std::io::ErrorKind::PermissionDenied | std::io::ErrorKind::AddrNotAvailable
        ) =>
      {
        eprintln!(
          "skipping image_probe_partial_fetch_falls_back_on_http_405: cannot bind localhost: {err}"
        );
        return;
      }
      Err(err) => panic!("bind localhost: {err}"),
    };
    let addr = listener.local_addr().expect("listener addr");
    let url = format!("http://{addr}/probe.png");
    let body = padded_png();

    let server = std::thread::spawn(move || {
      // First request should be the partial probe with a Range header.
      {
        let (mut stream, _) = listener.accept().expect("accept range request");
        let _ = stream.set_read_timeout(Some(Duration::from_secs(1)));
        let req = read_http_headers(&mut stream);
        assert!(
          extract_range_header(&req).is_some(),
          "expected Range header on probe request"
        );

        let header = "HTTP/1.1 405 Method Not Allowed\r\nContent-Type: text/html\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
        stream.write_all(header.as_bytes()).expect("write header");
        let _ = stream.flush();
      }

      // Second request should be the fallback full fetch (no Range header).
      {
        let (mut stream, _) = listener.accept().expect("accept full request");
        let _ = stream.set_read_timeout(Some(Duration::from_secs(1)));
        let req = read_http_headers(&mut stream);
        assert!(
          extract_range_header(&req).is_none(),
          "unexpected Range header on fallback full fetch"
        );

        let header = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: image/png\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
          body.len()
        );
        stream.write_all(header.as_bytes()).expect("write header");
        stream.write_all(&body).expect("write body");
        let _ = stream.flush();
      }
    });

    enable_image_cache_diagnostics();
    let cache = ImageCache::with_fetcher(Arc::new(HttpFetcher::new()));
    let meta = cache.probe(&url).expect("probe succeeds");
    assert_eq!(meta.dimensions(), (1, 1));

    let stats = take_image_cache_diagnostics().expect("diagnostics enabled");
    assert_eq!(stats.probe_partial_requests, 1);
    assert_eq!(stats.probe_partial_fallback_full, 1);

    server.join().unwrap();
  }

  #[test]
  fn svg_viewbox_renders_with_default_letterboxing() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><rect width='100' height='100' fill='red'/></svg>";

    let pixmap = cache
      .render_svg_pixmap_at_size(svg, 200, 100, "test://svg", 1.0)
      .expect("render svg");

    let left = pixmap.pixel(10, 50).expect("left padding");
    assert_eq!(left.alpha(), 0, "letterboxed area should be transparent");

    let center = pixmap.pixel(100, 50).expect("center pixel");
    assert_eq!(
      (center.red(), center.green(), center.blue(), center.alpha()),
      (255, 0, 0, 255)
    );
  }

  #[test]
  fn svg_viewbox_none_stretches_to_viewport() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100' preserveAspectRatio='none'><rect width='100' height='100' fill='red'/></svg>";

    let pixmap = cache
      .render_svg_pixmap_at_size(svg, 200, 100, "test://svg", 1.0)
      .expect("render svg");

    let left = pixmap.pixel(10, 50).expect("left pixel");
    assert_eq!(
      (left.red(), left.green(), left.blue(), left.alpha()),
      (255, 0, 0, 255)
    );
  }

  #[test]
  fn svg_viewbox_aligns_min_min_meet() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100' preserveAspectRatio='xMinYMin meet'><rect width='100' height='100' fill='red'/></svg>";

    let pixmap = cache
      .render_svg_pixmap_at_size(svg, 200, 100, "test://svg", 1.0)
      .expect("render svg");

    let left = pixmap.pixel(10, 50).expect("left pixel");
    assert_eq!(left.alpha(), 255);

    let right = pixmap.pixel(190, 50).expect("right padding");
    assert_eq!(right.alpha(), 0);
  }

  #[test]
  fn svg_viewbox_slice_fills_viewport() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100' preserveAspectRatio='xMidYMid slice'><circle cx='50' cy='50' r='50' fill='red'/></svg>";

    let pixmap = cache
      .render_svg_pixmap_at_size(svg, 200, 100, "test://svg", 1.0)
      .expect("render svg");

    let left = pixmap.pixel(10, 50).expect("left pixel");
    assert_eq!(left.alpha(), 255);
    assert_eq!(left.red(), 255);
  }

  #[test]
  fn svg_width_height_set_intrinsic_size_and_ratio() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='100'></svg>";
    let img = cache.render_svg(svg).expect("rendered");

    assert_eq!(img.width(), 200);
    assert_eq!(img.height(), 100);
    assert_eq!(
      img.intrinsic_ratio(OrientationTransform::IDENTITY),
      Some(2.0)
    );
    assert!(!img.aspect_ratio_none);
  }

  #[test]
  fn svg_viewbox_defaults_to_300x150_with_ratio() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 40 20'></svg>";
    let img = cache.render_svg(svg).expect("rendered");

    assert_eq!(img.width(), 300);
    assert_eq!(img.height(), 150);
    assert_eq!(
      img.intrinsic_ratio(OrientationTransform::IDENTITY),
      Some(2.0)
    );
    assert!(!img.aspect_ratio_none);
  }

  #[test]
  fn svg_preserve_aspect_ratio_none_disables_ratio() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 50' preserveAspectRatio='none'></svg>";
    let img = cache.render_svg(svg).expect("rendered");

    assert_eq!(img.width(), 300);
    assert_eq!(img.height(), 150);
    assert!(img.aspect_ratio_none);
    assert_eq!(img.intrinsic_ratio(OrientationTransform::IDENTITY), None);
  }

  #[test]
  fn svg_fast_path_without_viewbox_scales_non_uniformly() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='100' height='100'>\
      <path d='M0 0 H100 V100 H0 Z' fill='red'/>\
    </svg>";

    let pixmap = cache
      .render_svg_pixmap_at_size(svg, 200, 100, "test://fast-path", 1.0)
      .expect("rendered pixmap");
    let pixel = pixmap.pixel(10, 50).expect("pixel");

    assert_eq!(
      (pixel.red(), pixel.green(), pixel.blue(), pixel.alpha()),
      (255, 0, 0, 255)
    );
  }

  #[test]
  fn svg_percent_width_height_ignored_defaults_with_viewbox_ratio() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='50%' height='25%' viewBox='0 0 200 100'></svg>";
    let img = cache.render_svg(svg).expect("rendered");

    // Percent lengths are ignored; fall back to 300x150 but keep the viewBox ratio (2:1).
    assert_eq!(img.width(), 300);
    assert_eq!(img.height(), 150);
    assert_eq!(
      img.intrinsic_ratio(OrientationTransform::IDENTITY),
      Some(2.0)
    );
  }

  #[test]
  fn svg_absolute_units_convert_to_px() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='1in' height='0.5in'></svg>";

    let meta = cache
      .probe_svg_content(svg, "inline inches")
      .expect("probe inches svg");
    assert_eq!(meta.width, 96);
    assert_eq!(meta.height, 48);

    let img = cache.render_svg(svg).expect("rendered");
    assert_eq!(img.width(), 96);
    assert_eq!(img.height(), 48);
    assert_eq!(
      img.intrinsic_ratio(OrientationTransform::IDENTITY),
      Some(2.0)
    );
  }

  #[test]
  fn svg_metric_units_convert_to_px() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='2.54cm' height='25.4mm'></svg>";

    let meta = cache
      .probe_svg_content(svg, "inline metric")
      .expect("probe metric svg");
    assert_eq!(meta.width, 96);
    assert_eq!(meta.height, 96);

    let img = cache.render_svg(svg).expect("rendered");
    assert_eq!(img.width(), 96);
    assert_eq!(img.height(), 96);
  }

  #[test]
  fn render_inline_svg_returns_image() {
    let cache = ImageCache::new();
    let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" width="10" height="5"></svg>"#;
    let (image, ratio, aspect_none) = cache.render_svg_to_image(svg).expect("svg render");
    assert_eq!(image.width(), 10);
    assert_eq!(image.height(), 5);
    assert_eq!(ratio, Some(2.0));
    assert!(!aspect_none);
  }

  #[test]
  fn render_svg_to_image_width_only_viewbox_preserves_aspect_ratio() {
    let cache = ImageCache::new();
    let svg = r#"
      <svg xmlns='http://www.w3.org/2000/svg' width='200' viewBox='0 0 100 100'>
        <rect x='0' y='0' width='100' height='100' fill='red'/>
      </svg>
    "#;
    let (image, _, _) = cache.render_svg_to_image(svg).expect("render svg");

    assert_eq!((image.width(), image.height()), (200, 200));
    let rgba = image.to_rgba8();
    let padding_pixel = rgba.get_pixel(100, 10).0;
    assert_eq!(
      padding_pixel[3], 0,
      "preserveAspectRatio='xMidYMid meet' should leave top padding transparent"
    );
    assert_eq!(rgba.get_pixel(100, 100).0, [255, 0, 0, 255]);
  }

  #[test]
  fn render_svg_to_image_respects_preserve_aspect_ratio_none_when_scaling() {
    let cache = ImageCache::new();
    let svg = r#"
      <svg xmlns='http://www.w3.org/2000/svg' width='200' viewBox='0 0 100 100' preserveAspectRatio='none'>
        <rect x='0' y='0' width='100' height='100' fill='red'/>
      </svg>
    "#;
    let (image, _, aspect_none) = cache.render_svg_to_image(svg).expect("render svg");

    assert!(aspect_none);
    assert_eq!((image.width(), image.height()), (200, 150));
    let rgba = image.to_rgba8();
    assert_eq!(
      rgba.get_pixel(100, 10).0,
      [255, 0, 0, 255],
      "preserveAspectRatio='none' should stretch to the top edge"
    );
  }

  #[test]
  fn render_svg_to_image_height_only_viewbox_preserves_aspect_ratio() {
    let cache = ImageCache::new();
    let svg = r#"
      <svg xmlns='http://www.w3.org/2000/svg' height='200' viewBox='0 0 100 100'>
        <rect x='0' y='0' width='100' height='100' fill='red'/>
      </svg>
    "#;
    let (image, _, _) = cache.render_svg_to_image(svg).expect("render svg");

    assert_eq!((image.width(), image.height()), (200, 200));
    let rgba = image.to_rgba8();
    let padding_pixel = rgba.get_pixel(10, 100).0;
    assert_eq!(
      padding_pixel[3], 0,
      "preserveAspectRatio='xMidYMid meet' should leave left padding transparent"
    );
    assert_eq!(rgba.get_pixel(100, 100).0, [255, 0, 0, 255]);
  }

  #[test]
  fn load_svg_data_url() {
    let cache = ImageCache::new();
    let data_url =
            "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E";
    let image = cache.load(data_url).expect("decode data URL");
    assert_eq!(image.width(), 1);
    assert_eq!(image.height(), 1);
  }

  #[test]
  fn svg_mask_application() {
    let cache = ImageCache::new();
    let svg = r#"
      <svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'>
        <defs>
          <mask id='m'>
            <rect width='100%' height='100%' fill='white'/>
            <rect width='10' height='20' fill='black'/>
          </mask>
        </defs>
        <rect width='20' height='20' fill='red' mask='url(#m)' />
      </svg>
    "#;

    let image = cache.render_svg(svg).expect("render svg mask");
    let rgba = image.image.to_rgba8();
    let left_alpha = rgba.get_pixel(5, 10)[3];
    let right_alpha = rgba.get_pixel(15, 10)[3];
    assert!(
      left_alpha < right_alpha,
      "mask should reduce left side opacity"
    );
  }

  #[test]
  fn exposes_exif_orientation() {
    let cache = ImageCache::new();
    let image = cache
      .load("tests/fixtures/image_orientation/orientation-6.jpg")
      .expect("load oriented image");
    assert_eq!(
      image.orientation,
      Some(OrientationTransform {
        quarter_turns: 1,
        flip_x: false
      })
    );
  }

  #[test]
  fn resolves_relative_urls_against_base() {
    let mut cache = ImageCache::new();
    let mut path: PathBuf = std::env::temp_dir();
    path.push(format!(
      "fastrender_base_url_test_{}_{}.png",
      std::process::id(),
      std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos()
    ));
    let dir = path.parent().unwrap().to_path_buf();
    let image = RgbaImage::from_raw(1, 1, vec![255, 0, 0, 255]).expect("build 1x1");
    image.save(&path).expect("encode png");
    let base_url = format!("file://{}", dir.display());
    cache.set_base_url(base_url);

    let image = cache
      .load(path.file_name().unwrap().to_str().unwrap())
      .expect("load via base");
    assert_eq!(image.width(), 1);
    assert_eq!(image.height(), 1);
  }

  #[test]
  fn resolves_relative_paths_against_http_base() {
    let cache = ImageCache::with_base_url("https://example.com/a/b/".to_string());
    assert_eq!(
      cache.resolve_url("../img.png"),
      "https://example.com/a/img.png".to_string()
    );
    assert_eq!(
      cache.resolve_url("./nested/icon.png"),
      "https://example.com/a/b/nested/icon.png".to_string()
    );
  }

  #[test]
  fn resolves_protocol_relative_urls_using_base_scheme() {
    let cache = ImageCache::with_base_url("https://example.com/base/".to_string());
    assert_eq!(
      cache.resolve_url("//cdn.example.com/asset.png"),
      "https://cdn.example.com/asset.png".to_string()
    );
  }

  #[test]
  fn resolves_file_base_without_trailing_slash_as_directory() {
    let mut dir: PathBuf = std::env::temp_dir();
    dir.push(format!(
      "fastrender_url_base_{}_{}",
      std::process::id(),
      SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos()
    ));
    std::fs::create_dir_all(dir.join("assets")).expect("create temp dir");
    let base = format!("file://{}", dir.display());
    let cache = ImageCache::with_base_url(base);

    let resolved = cache.resolve_url("assets/image.png");
    assert!(
      resolved.ends_with("/assets/image.png"),
      "resolved path should keep directory: {}",
      resolved
    );

    std::fs::remove_dir_all(&dir).ok();
  }

  #[test]
  fn with_fetcher_uses_custom_fetcher() {
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;

    struct CountingFetcher {
      count: AtomicUsize,
    }

    impl ResourceFetcher for CountingFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        self.count.fetch_add(1, Ordering::SeqCst);
        // Return a minimal valid PNG
        let png_data = vec![
          0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, // PNG signature
          0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52, // IHDR chunk
          0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, // 1x1
          0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53, 0xde, 0x00, 0x00, 0x00, 0x0c, 0x49, 0x44,
          0x41, 0x54, 0x08, 0xd7, 0x63, 0xf8, 0xff, 0xff, 0x3f, 0x00, 0x05, 0xfe, 0x02, 0xfe, 0xdc,
          0xcc, 0x59, 0xe7, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82,
        ];
        Ok(FetchedResource::new(
          png_data,
          Some("image/png".to_string()),
        ))
      }
    }

    let fetcher = Arc::new(CountingFetcher {
      count: AtomicUsize::new(0),
    });
    let cache = ImageCache::with_fetcher(Arc::clone(&fetcher) as Arc<dyn ResourceFetcher>);

    let _ = cache.load("test://image.png");
    assert_eq!(fetcher.count.load(Ordering::SeqCst), 1);

    // Second load should use cache
    let _ = cache.load("test://image.png");
    assert_eq!(fetcher.count.load(Ordering::SeqCst), 1);

    // Different URL should fetch again
    let _ = cache.load("test://other.png");
    assert_eq!(fetcher.count.load(Ordering::SeqCst), 2);
  }

  struct StaticFetcher {
    bytes: Vec<u8>,
    content_type: Option<String>,
  }

  impl ResourceFetcher for StaticFetcher {
    fn fetch(&self, _url: &str) -> Result<FetchedResource> {
      Ok(FetchedResource::new(
        self.bytes.clone(),
        self.content_type.clone(),
      ))
    }
  }

  fn avif_fixture_bytes() -> Vec<u8> {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/avif/solid.avif");
    std::fs::read(&path).expect("read avif fixture")
  }

  fn assert_green_pixel(pixel: [u8; 4]) {
    assert!(pixel[1] >= 180, "expected green channel, got {pixel:?}");
    assert!(
      pixel[0] < 50 && pixel[2] < 50,
      "expected low red/blue, got {pixel:?}"
    );
  }

  #[test]
  fn decodes_avif_with_declared_content_type() {
    let bytes = avif_fixture_bytes();
    let fetcher = Arc::new(StaticFetcher {
      bytes: bytes.clone(),
      content_type: Some("image/avif".to_string()),
    });
    let cache = ImageCache::with_fetcher(fetcher);

    let image = cache.load("test://avif.declared").expect("decode avif");
    assert_eq!(image.width(), 4);
    assert_eq!(image.height(), 4);

    let pixel = image.image.to_rgba8().get_pixel(0, 0).0;
    assert_green_pixel(pixel);
  }

  #[test]
  fn decodes_avif_when_content_type_is_incorrect() {
    let bytes = avif_fixture_bytes();
    let fetcher = Arc::new(StaticFetcher {
      bytes: bytes.clone(),
      content_type: Some("image/png".to_string()),
    });
    let cache = ImageCache::with_fetcher(fetcher);

    let image = cache
      .load("test://avif.sniff")
      .expect("decode avif via sniffing");
    assert_eq!(image.width(), 4);
    assert_eq!(image.height(), 4);

    let pixel = image.image.to_rgba8().get_pixel(2, 2).0;
    assert_green_pixel(pixel);
  }

  #[test]
  fn decode_inflight_wait_respects_render_deadline() {
    use std::sync::mpsc;
    use std::sync::Barrier;
    use std::thread;

    struct BlockingFetcher {
      started: Arc<Barrier>,
      release: Arc<Barrier>,
      url: String,
    }

    impl ResourceFetcher for BlockingFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        if url == self.url {
          self.started.wait();
          self.release.wait();
          return Ok(FetchedResource::new(
            b"not an image".to_vec(),
            Some("image/png".to_string()),
          ));
        }

        Err(Error::Resource(crate::error::ResourceError::new(
          url.to_string(),
          "unexpected url",
        )))
      }
    }

    let started = Arc::new(Barrier::new(2));
    let release = Arc::new(Barrier::new(2));
    let url = "https://example.com/blocked.png".to_string();
    let cache = ImageCache::with_fetcher(Arc::new(BlockingFetcher {
      started: Arc::clone(&started),
      release: Arc::clone(&release),
      url: url.clone(),
    }));

    let owner_cache = cache.clone();
    let owner_url = url.clone();
    let owner_handle = thread::spawn(move || owner_cache.load(&owner_url));

    // Wait until the owner has entered the fetcher (and therefore registered the in-flight entry).
    started.wait();

    let waiter_cache = cache.clone();
    let waiter_url = url.clone();
    let (tx, rx) = mpsc::channel();
    let waiter_handle = thread::spawn(move || {
      let deadline = render_control::RenderDeadline::new(Some(Duration::from_millis(50)), None);
      let start = Instant::now();
      let result =
        render_control::with_deadline(Some(&deadline), || waiter_cache.load(&waiter_url));
      tx.send((result, start.elapsed())).unwrap();
    });

    let (result, elapsed) = match rx.recv_timeout(Duration::from_secs(1)) {
      Ok(value) => value,
      Err(err) => {
        // Make sure we don't leave the owner thread blocked on the barrier.
        release.wait();
        let _ = owner_handle.join();
        drop(waiter_handle);
        panic!("waiter decode did not complete under deadline: {err}");
      }
    };

    let err = match result {
      Ok(_) => panic!("waiter decode should fail under deadline"),
      Err(err) => err,
    };
    match err {
      Error::Render(RenderError::Timeout { stage, .. }) => {
        assert_eq!(stage, RenderStage::Paint);
      }
      other => panic!("unexpected error after {elapsed:?}: {other:?}"),
    }

    // Let the owner thread exit so it can resolve the in-flight entry.
    release.wait();
    let _ = owner_handle.join();
    let _ = waiter_handle.join();
  }

  fn mean_abs_diff(a: &[u8], b: &[u8]) -> f64 {
    assert_eq!(a.len(), b.len());
    let sum: u64 = a
      .iter()
      .zip(b.iter())
      .map(|(lhs, rhs)| i16::from(*lhs).abs_diff(i16::from(*rhs)) as u64)
      .sum();
    sum as f64 / a.len() as f64
  }

  #[test]
  fn raster_pixmap_at_size_matches_draw_scaled_output_with_orientation() {
    let mut image = RgbaImage::new(6, 4);
    for y in 0..image.height() {
      for x in 0..image.width() {
        let r = (x * 40).min(255) as u8;
        let g = (y * 60).min(255) as u8;
        let b = 128u8;
        image.put_pixel(x, y, image::Rgba([r, g, b, 255]));
      }
    }
    // Include one translucent pixel to exercise premultiplication.
    image.put_pixel(2, 1, image::Rgba([255, 0, 0, 128]));

    let mut png = Vec::new();
    PngEncoder::new(&mut png)
      .write_image(
        &image,
        image.width(),
        image.height(),
        ColorType::Rgba8.into(),
      )
      .expect("encode png");
    let src = format!(
      "data:image/png;base64,{}",
      base64::engine::general_purpose::STANDARD.encode(&png)
    );

    let cache = ImageCache::new();
    let orientation = OrientationTransform {
      quarter_turns: 1,
      flip_x: true,
    };

    let full = cache
      .load_raster_pixmap(&src, orientation, false)
      .expect("load full pixmap")
      .expect("raster pixmap");

    for (target_w, target_h) in [(3u32, 4u32), (2u32, 3u32)] {
      let mut expected = Pixmap::new(target_w, target_h).expect("dst pixmap");
      let scale_x = target_w as f32 / full.width() as f32;
      let scale_y = target_h as f32 / full.height() as f32;
      let mut paint = tiny_skia::PixmapPaint::default();
      paint.quality = FilterQuality::Bilinear;
      expected.draw_pixmap(
        0,
        0,
        full.as_ref().as_ref(),
        &paint,
        tiny_skia::Transform::from_row(scale_x, 0.0, 0.0, scale_y, 0.0, 0.0),
        None,
      );

      let scaled = cache
        .load_raster_pixmap_at_size(
          &src,
          orientation,
          false,
          target_w,
          target_h,
          FilterQuality::Bilinear,
        )
        .expect("scaled pixmap")
        .expect("raster pixmap");

      assert_eq!((scaled.width(), scaled.height()), (target_w, target_h));
      let diff = mean_abs_diff(expected.data(), scaled.data());
      assert!(
        diff <= 10.0,
        "expected scaled output to match within tolerance (diff={diff})"
      );
    }

    let scaled_a = cache
      .load_raster_pixmap_at_size(&src, orientation, false, 3, 4, FilterQuality::Bilinear)
      .expect("scaled pixmap")
      .expect("raster pixmap");
    let scaled_a_again = cache
      .load_raster_pixmap_at_size(&src, orientation, false, 3, 4, FilterQuality::Bilinear)
      .expect("scaled pixmap")
      .expect("raster pixmap");
    assert!(Arc::ptr_eq(&scaled_a, &scaled_a_again));

    let scaled_b = cache
      .load_raster_pixmap_at_size(&src, orientation, false, 2, 3, FilterQuality::Bilinear)
      .expect("scaled pixmap")
      .expect("raster pixmap");
    assert!(!Arc::ptr_eq(&scaled_a, &scaled_b));
  }

  #[test]
  fn decode_inflight_wait_respects_cancel_callback() {
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::mpsc;
    use std::sync::Barrier;
    use std::thread;

    struct BlockingFetcher {
      started: Arc<Barrier>,
      release: Arc<Barrier>,
      url: String,
    }

    impl ResourceFetcher for BlockingFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        if url == self.url {
          self.started.wait();
          self.release.wait();
          return Ok(FetchedResource::new(
            b"not an image".to_vec(),
            Some("image/png".to_string()),
          ));
        }

        Err(Error::Resource(crate::error::ResourceError::new(
          url.to_string(),
          "unexpected url",
        )))
      }
    }

    let started = Arc::new(Barrier::new(2));
    let release = Arc::new(Barrier::new(2));
    let url = "https://example.com/blocked-image.png".to_string();
    let cache = ImageCache::with_fetcher(Arc::new(BlockingFetcher {
      started: Arc::clone(&started),
      release: Arc::clone(&release),
      url: url.clone(),
    }));

    let owner_cache = cache.clone();
    let owner_url = url.clone();
    let owner_handle = thread::spawn(move || owner_cache.load(&owner_url));

    // Wait until the owner has entered the fetcher (and therefore registered the in-flight entry).
    started.wait();

    let waiter_cache = cache.clone();
    let waiter_url = url.clone();
    let cancel_flag = Arc::new(AtomicBool::new(false));
    let cancel_flag_worker = Arc::clone(&cancel_flag);
    let cancel: Arc<crate::render_control::CancelCallback> =
      Arc::new(move || cancel_flag_worker.load(Ordering::Relaxed));
    let (tx, rx) = mpsc::channel();
    let waiter_handle = thread::spawn(move || {
      let deadline = render_control::RenderDeadline::new(None, Some(cancel));
      let start = Instant::now();
      let result =
        render_control::with_deadline(Some(&deadline), || waiter_cache.load(&waiter_url));
      tx.send((result, start.elapsed())).unwrap();
    });

    thread::sleep(Duration::from_millis(50));
    cancel_flag.store(true, Ordering::Relaxed);

    let (result, elapsed) = match rx.recv_timeout(Duration::from_secs(1)) {
      Ok(value) => value,
      Err(err) => {
        release.wait();
        let _ = owner_handle.join();
        drop(waiter_handle);
        panic!("waiter decode did not complete under cancel: {err}");
      }
    };

    let err = match result {
      Ok(_) => panic!("waiter decode should fail under cancel callback"),
      Err(err) => err,
    };
    match err {
      Error::Render(RenderError::Timeout { stage, .. }) => {
        assert_eq!(stage, RenderStage::Paint);
      }
      other => panic!("unexpected error after {elapsed:?}: {other:?}"),
    }

    release.wait();
    let _ = owner_handle.join();
    let _ = waiter_handle.join();
  }

  #[test]
  fn probe_inflight_deduplicates_cache_artifact_reads() {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Barrier;
    use std::thread;

    struct ArtifactFetcher {
      url: String,
      bytes: Arc<Vec<u8>>,
      calls: Arc<AtomicUsize>,
    }

    impl ResourceFetcher for ArtifactFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        panic!("network fetch should not be called");
      }

      fn fetch_partial_with_context(
        &self,
        _kind: FetchContextKind,
        _url: &str,
        _max_bytes: usize,
      ) -> Result<FetchedResource> {
        panic!("network partial fetch should not be called");
      }

      fn read_cache_artifact(
        &self,
        kind: FetchContextKind,
        url: &str,
        artifact: CacheArtifactKind,
      ) -> Option<FetchedResource> {
        assert_eq!(kind, FetchContextKind::Image);
        assert_eq!(artifact, CacheArtifactKind::ImageProbeMetadata);
        if url != self.url {
          return None;
        }

        let idx = self.calls.fetch_add(1, Ordering::SeqCst);
        if idx == 0 {
          // Simulate a slow disk read so concurrent probes overlap.
          thread::sleep(Duration::from_millis(50));
        }

        let mut res = FetchedResource::new(
          self.bytes.as_ref().clone(),
          Some("application/x-fastrender-image-probe+json".to_string()),
        );
        res.status = Some(200);
        res.final_url = Some(url.to_string());
        Some(res)
      }
    }

    let meta = CachedImageMetadata {
      width: 42,
      height: 24,
      orientation: None,
      resolution: None,
      is_vector: false,
      intrinsic_ratio: None,
      aspect_ratio_none: false,
    };
    let bytes = Arc::new(encode_probe_metadata_for_disk(&meta).expect("encode metadata"));
    let url = "https://example.com/probe-inflight-artifact.png".to_string();
    let calls = Arc::new(AtomicUsize::new(0));
    let cache = ImageCache::with_fetcher(Arc::new(ArtifactFetcher {
      url: url.clone(),
      bytes: Arc::clone(&bytes),
      calls: Arc::clone(&calls),
    }));

    let threads = 8usize;
    let barrier = Arc::new(Barrier::new(threads));
    let mut handles = Vec::new();
    for _ in 0..threads {
      let cache = cache.clone();
      let url = url.clone();
      let barrier = Arc::clone(&barrier);
      handles.push(thread::spawn(move || {
        barrier.wait();
        cache.probe_resolved(&url)
      }));
    }

    for handle in handles {
      let probed = handle
        .join()
        .expect("probe thread panicked")
        .expect("probe ok");
      assert_eq!(probed.dimensions(), (42, 24));
    }

    assert_eq!(
      calls.load(Ordering::SeqCst),
      1,
      "cache artifact reads should be single-flight under probe in-flight"
    );
  }

  #[test]
  fn probe_inflight_wait_respects_render_deadline() {
    use std::sync::mpsc;
    use std::sync::Barrier;
    use std::thread;

    struct BlockingFetcher {
      started: Arc<Barrier>,
      release: Arc<Barrier>,
      url: String,
    }

    impl ResourceFetcher for BlockingFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        if url == self.url {
          self.started.wait();
          self.release.wait();
          return Ok(FetchedResource::new(
            b"not an image".to_vec(),
            Some("image/png".to_string()),
          ));
        }

        Err(Error::Resource(crate::error::ResourceError::new(
          url.to_string(),
          "unexpected url",
        )))
      }
    }

    let started = Arc::new(Barrier::new(2));
    let release = Arc::new(Barrier::new(2));
    let url = "https://example.com/blocked-probe.png".to_string();
    let cache = ImageCache::with_fetcher(Arc::new(BlockingFetcher {
      started: Arc::clone(&started),
      release: Arc::clone(&release),
      url: url.clone(),
    }));

    let owner_cache = cache.clone();
    let owner_url = url.clone();
    let owner_handle = thread::spawn(move || owner_cache.probe(&owner_url));

    started.wait();

    let waiter_cache = cache.clone();
    let waiter_url = url.clone();
    let (tx, rx) = mpsc::channel();
    let waiter_handle = thread::spawn(move || {
      let deadline = render_control::RenderDeadline::new(Some(Duration::from_millis(50)), None);
      let start = Instant::now();
      let result =
        render_control::with_deadline(Some(&deadline), || waiter_cache.probe(&waiter_url));
      tx.send((result, start.elapsed())).unwrap();
    });

    let (result, elapsed) = match rx.recv_timeout(Duration::from_secs(1)) {
      Ok(value) => value,
      Err(err) => {
        release.wait();
        let _ = owner_handle.join();
        drop(waiter_handle);
        panic!("waiter probe did not complete under deadline: {err}");
      }
    };

    let err = match result {
      Ok(_) => panic!("waiter probe should fail under deadline"),
      Err(err) => err,
    };
    match err {
      Error::Render(RenderError::Timeout { stage, .. }) => {
        assert_eq!(stage, RenderStage::Paint);
      }
      other => panic!("unexpected error after {elapsed:?}: {other:?}"),
    }

    release.wait();
    let _ = owner_handle.join();
    let _ = waiter_handle.join();
  }

  #[test]
  fn probe_inflight_wait_respects_cancel_callback() {
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::mpsc;
    use std::sync::Barrier;
    use std::thread;

    struct BlockingFetcher {
      started: Arc<Barrier>,
      release: Arc<Barrier>,
      url: String,
    }

    impl ResourceFetcher for BlockingFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        if url == self.url {
          self.started.wait();
          self.release.wait();
          return Ok(FetchedResource::new(
            b"not an image".to_vec(),
            Some("image/png".to_string()),
          ));
        }

        Err(Error::Resource(crate::error::ResourceError::new(
          url.to_string(),
          "unexpected url",
        )))
      }
    }

    let started = Arc::new(Barrier::new(2));
    let release = Arc::new(Barrier::new(2));
    let url = "https://example.com/blocked-probe.png".to_string();
    let cache = ImageCache::with_fetcher(Arc::new(BlockingFetcher {
      started: Arc::clone(&started),
      release: Arc::clone(&release),
      url: url.clone(),
    }));

    let owner_cache = cache.clone();
    let owner_url = url.clone();
    let owner_handle = thread::spawn(move || owner_cache.probe(&owner_url));

    started.wait();

    let waiter_cache = cache.clone();
    let waiter_url = url.clone();
    let cancel_flag = Arc::new(AtomicBool::new(false));
    let cancel_flag_worker = Arc::clone(&cancel_flag);
    let cancel: Arc<crate::render_control::CancelCallback> =
      Arc::new(move || cancel_flag_worker.load(Ordering::Relaxed));
    let (tx, rx) = mpsc::channel();
    let waiter_handle = thread::spawn(move || {
      let deadline = render_control::RenderDeadline::new(None, Some(cancel));
      let start = Instant::now();
      let result =
        render_control::with_deadline(Some(&deadline), || waiter_cache.probe(&waiter_url));
      tx.send((result, start.elapsed())).unwrap();
    });

    thread::sleep(Duration::from_millis(50));
    cancel_flag.store(true, Ordering::Relaxed);

    let (result, elapsed) = match rx.recv_timeout(Duration::from_secs(1)) {
      Ok(value) => value,
      Err(err) => {
        release.wait();
        let _ = owner_handle.join();
        drop(waiter_handle);
        panic!("waiter probe did not complete under cancel: {err}");
      }
    };

    let err = match result {
      Ok(_) => panic!("waiter probe should fail under cancel callback"),
      Err(err) => err,
    };
    match err {
      Error::Render(RenderError::Timeout { stage, .. }) => {
        assert_eq!(stage, RenderStage::Paint);
      }
      other => panic!("unexpected error after {elapsed:?}: {other:?}"),
    }

    release.wait();
    let _ = owner_handle.join();
    let _ = waiter_handle.join();
  }
}
