//! Image loading and caching
//!
//! This module provides image loading from various sources (HTTP, file, data URLs)
//! with in-memory caching and support for various image formats including SVG.

use crate::api::{RenderDiagnostics, ResourceContext, ResourceKind};
use crate::debug::runtime;
use crate::error::Error;
use crate::error::ImageError;
use crate::error::RenderError;
use crate::error::Result;
use crate::resource::CachingFetcher;
use crate::resource::CachingFetcherConfig;
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
use image::RgbaImage;
use roxmltree::Document;
use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::Cursor;
use std::path::Path;
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::time::Instant;
use url::Url;

fn image_profile_threshold_ms() -> Option<f64> {
  runtime::runtime_toggles().f64("FASTR_IMAGE_PROFILE_MS")
}

/// Per-thread image cache diagnostics collection.
#[derive(Debug, Default, Clone)]
pub struct ImageCacheDiagnostics {
  pub requests: usize,
  pub cache_hits: usize,
  pub cache_misses: usize,
}

thread_local! {
  static IMAGE_CACHE_DIAGNOSTICS: RefCell<Option<ImageCacheDiagnostics>> = RefCell::new(None);
}

pub(crate) fn enable_image_cache_diagnostics() {
  IMAGE_CACHE_DIAGNOSTICS.with(|cell| {
    *cell.borrow_mut() = Some(ImageCacheDiagnostics::default());
  });
}

pub(crate) fn take_image_cache_diagnostics() -> Option<ImageCacheDiagnostics> {
  IMAGE_CACHE_DIAGNOSTICS.with(|cell| cell.borrow_mut().take())
}

fn record_image_cache_request() {
  IMAGE_CACHE_DIAGNOSTICS.with(|cell| {
    if let Some(stats) = cell.borrow_mut().as_mut() {
      stats.requests += 1;
    }
  });
}

fn record_image_cache_hit() {
  IMAGE_CACHE_DIAGNOSTICS.with(|cell| {
    if let Some(stats) = cell.borrow_mut().as_mut() {
      stats.cache_hits += 1;
    }
  });
}

fn record_image_cache_miss() {
  IMAGE_CACHE_DIAGNOSTICS.with(|cell| {
    if let Some(stats) = cell.borrow_mut().as_mut() {
      stats.cache_misses += 1;
    }
  });
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct SvgPixmapKey {
  hash: u64,
  len: usize,
  width: u32,
  height: u32,
}

fn svg_pixmap_key(svg_content: &str, width: u32, height: u32) -> SvgPixmapKey {
  let mut hasher = DefaultHasher::new();
  svg_content.hash(&mut hasher);
  SvgPixmapKey {
    hash: hasher.finish(),
    len: svg_content.len(),
    width,
    height,
  }
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

fn build_tiny_skia_path_from_svg_path_data(data: &str) -> Option<tiny_skia::Path> {
  use svgtypes::PathParser;
  use svgtypes::PathSegment;
  use tiny_skia::PathBuilder;

  let mut pb = PathBuilder::new();
  let mut current = (0.0f32, 0.0f32);
  let mut subpath_start = (0.0f32, 0.0f32);
  let mut last_cubic_ctrl: Option<(f32, f32)> = None;
  let mut last_quad_ctrl: Option<(f32, f32)> = None;

  for segment in PathParser::from(data) {
    let seg = segment.ok()?;
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

  pb.finish()
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
) -> Option<tiny_skia::Pixmap> {
  use tiny_skia::FillRule;
  use tiny_skia::Paint;
  use tiny_skia::Pixmap;

  if render_width == 0 || render_height == 0 {
    return None;
  }

  let doc = Document::parse(svg_content).ok()?;
  let root = doc.root_element();
  let has_view_box_attr = root.attribute("viewBox").is_some();
  if !root.tag_name().name().eq_ignore_ascii_case("svg") {
    return None;
  }

  for node in root.descendants().filter(|n| n.is_element()) {
    let name = node.tag_name().name();
    let allowed = matches!(name, "svg" | "g" | "path" | "title" | "desc" | "metadata");
    if !allowed {
      return None;
    }
    if node.attribute("transform").is_some()
      || node.attribute("filter").is_some()
      || node.attribute("mask").is_some()
      || node.attribute("clip-path").is_some()
      || node.attribute("style").is_some()
    {
      return None;
    }
    if name.eq_ignore_ascii_case("path") {
      if node.attribute("d").is_none() {
        return None;
      }
      if node
        .attribute("stroke")
        .is_some_and(|v| !v.trim().eq_ignore_ascii_case("none"))
      {
        return None;
      }
      if node.attribute("stroke-width").is_some()
        || node.attribute("stroke-linecap").is_some()
        || node.attribute("stroke-linejoin").is_some()
        || node.attribute("stroke-miterlimit").is_some()
        || node.attribute("stroke-dasharray").is_some()
        || node.attribute("stroke-dashoffset").is_some()
      {
        return None;
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
    return None;
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

  let mut pixmap = Pixmap::new(render_width, render_height)?;
  for node in root.descendants().filter(|n| n.is_element()) {
    if !node.tag_name().name().eq_ignore_ascii_case("path") {
      continue;
    }
    let d = node.attribute("d")?;
    let path = build_tiny_skia_path_from_svg_path_data(d)?;

    let fill = node.attribute("fill");
    let mut color = match fill {
      Some(v) => svg_parse_fill_color(v)?,
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
      Some(_) => return None,
    };

    let mut paint = Paint::default();
    paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
    paint.anti_alias = true;
    pixmap.fill_path(&path, &paint, fill_rule, transform, None);
  }

  Some(pixmap)
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
}

impl Default for ImageCacheConfig {
  fn default() -> Self {
    Self {
      max_decoded_pixels: 100_000_000,
      max_decoded_dimension: 32768,
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
    if let Ok(mut slot) = self.result.lock() {
      *slot = Some(result);
      self.cv.notify_all();
    }
  }

  fn wait(&self) -> Result<Arc<CachedImage>> {
    let mut guard = self.result.lock().unwrap();
    while guard.is_none() {
      guard = self.cv.wait(guard).unwrap();
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
    if let Ok(mut slot) = self.result.lock() {
      *slot = Some(result);
      self.cv.notify_all();
    }
  }

  fn wait(&self) -> Result<Arc<CachedImageMetadata>> {
    let mut guard = self.result.lock().unwrap();
    while guard.is_none() {
      guard = self.cv.wait(guard).unwrap();
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
  cache: Arc<Mutex<HashMap<String, Arc<CachedImage>>>>,
  /// In-flight decodes keyed by resolved URL to de-duplicate concurrent loads.
  in_flight: Arc<Mutex<HashMap<String, Arc<DecodeInFlight>>>>,
  /// In-memory cache of probed metadata (keyed by resolved URL).
  meta_cache: Arc<Mutex<HashMap<String, Arc<CachedImageMetadata>>>>,
  /// Raw resources captured during metadata probes to avoid duplicate fetches between layout and paint.
  raw_cache: Arc<Mutex<HashMap<String, Arc<FetchedResource>>>>,
  /// In-flight probes keyed by resolved URL to de-duplicate concurrent metadata loads.
  meta_in_flight: Arc<Mutex<HashMap<String, Arc<ProbeInFlight>>>>,
  /// In-memory cache of rendered inline SVG pixmaps keyed by (hash, size).
  svg_pixmap_cache: Arc<Mutex<HashMap<SvgPixmapKey, Arc<tiny_skia::Pixmap>>>>,
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
      cache: Arc::new(Mutex::new(HashMap::new())),
      in_flight: Arc::new(Mutex::new(HashMap::new())),
      meta_cache: Arc::new(Mutex::new(HashMap::new())),
      raw_cache: Arc::new(Mutex::new(HashMap::new())),
      meta_in_flight: Arc::new(Mutex::new(HashMap::new())),
      svg_pixmap_cache: Arc::new(Mutex::new(HashMap::new())),
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
    if url.is_empty() {
      return String::new();
    }

    // Absolute or data URLs can be returned directly.
    if url.starts_with("data:") {
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
    // Resolve the URL first
    let resolved_url = self.resolve_url(url);
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
      return flight.wait();
    }

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
      self.finish_inflight(&resolved_url, &flight, shared);
      return result;
    }

    record_image_cache_miss();
    let result = self.fetch_and_decode(&resolved_url);
    let shared = match &result {
      Ok(img) => SharedImageResult::Success(Arc::clone(img)),
      Err(err) => SharedImageResult::Error(err.clone()),
    };
    self.finish_inflight(&resolved_url, &flight, shared);

    result
  }

  /// Probe image metadata (dimensions, EXIF orientation/resolution, SVG intrinsic ratio)
  /// without fully decoding the image.
  pub fn probe(&self, url: &str) -> Result<Arc<CachedImageMetadata>> {
    let resolved_url = self.resolve_url(url);
    self.enforce_image_policy(&resolved_url)?;

    record_image_cache_request();
    if let Some(img) = self.get_cached(&resolved_url) {
      record_image_cache_hit();
      return Ok(Arc::new(CachedImageMetadata::from(&*img)));
    }

    if let Some(meta) = self.get_cached_meta(&resolved_url) {
      record_image_cache_hit();
      return Ok(meta);
    }
    let (flight, is_owner) = self.join_meta_inflight(&resolved_url);
    if !is_owner {
      record_image_cache_hit();
      return flight.wait();
    }

    record_image_cache_miss();
    let result = self.fetch_and_probe(&resolved_url);
    let shared = match &result {
      Ok(meta) => SharedMetaResult::Success(Arc::clone(meta)),
      Err(err) => SharedMetaResult::Error(err.clone()),
    };
    self.finish_meta_inflight(&resolved_url, &flight, shared);

    result
  }

  fn get_cached(&self, resolved_url: &str) -> Option<Arc<CachedImage>> {
    self
      .cache
      .lock()
      .ok()
      .and_then(|cache| cache.get(resolved_url).cloned())
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

  fn record_image_error(&self, url: &str, error: &Error) {
    if let Some(diag) = &self.diagnostics {
      if let Ok(mut guard) = diag.lock() {
        guard.record_error(ResourceKind::Image, url, error);
      }
    }
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
        if href.is_empty() || href.starts_with('#') || href.starts_with("data:") {
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

    let resource = match self.fetcher.fetch(resolved_url) {
      Ok(res) => res,
      Err(err) => {
        self.record_image_error(resolved_url, &err);
        return Err(err);
      }
    };
    if let Some(ctx) = &self.resource_context {
      if let Err(err) = ctx.check_allowed_with_final(
        ResourceKind::Image,
        resolved_url,
        resource.final_url.as_deref(),
      ) {
        let blocked = Error::Image(ImageError::LoadFailed {
          url: resolved_url.to_string(),
          reason: err.reason,
        });
        self.record_image_error(resolved_url, &blocked);
        return Err(blocked);
      }
    }
    let fetch_ms = fetch_start.map(|s| s.elapsed().as_secs_f64() * 1000.0);
    let decode_start = profile_enabled.then(Instant::now);
    let (img, orientation, resolution, is_vector, intrinsic_ratio, aspect_ratio_none) =
      match self.decode_resource(&resource, resolved_url) {
        Ok(decoded) => decoded,
        Err(err) => {
          self.record_image_error(resolved_url, &err);
          return Err(err);
        }
      };
    let decode_ms = decode_start.map(|s| s.elapsed().as_secs_f64() * 1000.0);

    let img_arc = Arc::new(CachedImage {
      image: Arc::new(img),
      orientation,
      resolution,
      is_vector,
      intrinsic_ratio,
      aspect_ratio_none,
    });

    if let Ok(mut cache) = self.cache.lock() {
      cache.insert(resolved_url.to_string(), Arc::clone(&img_arc));
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
    let threshold_ms = image_profile_threshold_ms();
    let profile_enabled = threshold_ms.is_some();
    let total_start = profile_enabled.then(Instant::now);
    let decode_start = profile_enabled.then(Instant::now);
    let (img, orientation, resolution, is_vector, intrinsic_ratio, aspect_ratio_none) =
      self.decode_resource(resource, resolved_url)?;
    let decode_ms = decode_start.map(|s| s.elapsed().as_secs_f64() * 1000.0);

    let img_arc = Arc::new(CachedImage {
      image: Arc::new(img),
      orientation,
      resolution,
      is_vector,
      intrinsic_ratio,
      aspect_ratio_none,
    });

    if let Ok(mut cache) = self.cache.lock() {
      cache.insert(resolved_url.to_string(), Arc::clone(&img_arc));
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

    let resource = match self.fetcher.fetch(resolved_url) {
      Ok(res) => Arc::new(res),
      Err(err) => {
        self.record_image_error(resolved_url, &err);
        return Err(err);
      }
    };
    if let Some(ctx) = &self.resource_context {
      if let Err(err) = ctx.check_allowed_with_final(
        ResourceKind::Image,
        resolved_url,
        resource.final_url.as_deref(),
      ) {
        let blocked = Error::Image(ImageError::LoadFailed {
          url: resolved_url.to_string(),
          reason: err.reason,
        });
        self.record_image_error(resolved_url, &blocked);
        return Err(blocked);
      }
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

    const RAW_RESOURCE_CACHE_LIMIT_BYTES: usize = 5 * 1024 * 1024;
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
    let mut map = self.in_flight.lock().unwrap();
    if let Some(existing) = map.get(resolved_url) {
      return (Arc::clone(existing), false);
    }

    let flight = Arc::new(DecodeInFlight::new());
    map.insert(resolved_url.to_string(), Arc::clone(&flight));
    (flight, true)
  }

  fn join_meta_inflight(&self, resolved_url: &str) -> (Arc<ProbeInFlight>, bool) {
    let mut map = self.meta_in_flight.lock().unwrap();
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
    if let Ok(mut map) = self.in_flight.lock() {
      map.remove(resolved_url);
    }
  }

  fn finish_meta_inflight(
    &self,
    resolved_url: &str,
    flight: &Arc<ProbeInFlight>,
    result: SharedMetaResult,
  ) {
    flight.set(result);
    if let Ok(mut map) = self.meta_in_flight.lock() {
      map.remove(resolved_url);
    }
  }

  /// Render raw SVG content to an image (uncached).
  pub fn render_svg(&self, svg_content: &str) -> Result<Arc<CachedImage>> {
    let (img, intrinsic_ratio, aspect_ratio_none) = self.render_svg_to_image(svg_content)?;
    Ok(Arc::new(CachedImage {
      image: Arc::new(img),
      orientation: None,
      resolution: None,
      is_vector: true,
      intrinsic_ratio,
      aspect_ratio_none,
    }))
  }

  pub(crate) fn render_svg_pixmap_at_size(
    &self,
    svg_content: &str,
    render_width: u32,
    render_height: u32,
    url: &str,
  ) -> Result<Arc<tiny_skia::Pixmap>> {
    use resvg::usvg;

    self.enforce_svg_resource_policy(svg_content, url)?;
    self.enforce_decode_limits(render_width, render_height, url)?;

    let key = svg_pixmap_key(svg_content, render_width, render_height);
    if let Ok(cache) = self.svg_pixmap_cache.lock() {
      if let Some(cached) = cache.get(&key) {
        return Ok(Arc::clone(cached));
      }
    }

    if let Some(pixmap) = try_render_simple_svg_pixmap(svg_content, render_width, render_height) {
      let pixmap = Arc::new(pixmap);
      if let Ok(mut cache) = self.svg_pixmap_cache.lock() {
        cache.insert(key, Arc::clone(&pixmap));
      }
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

    let Some(mut pixmap) = tiny_skia::Pixmap::new(render_width, render_height) else {
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
    resvg::render(&tree, transform, &mut pixmap.as_mut());

    let pixmap = Arc::new(pixmap);
    if let Ok(mut cache) = self.svg_pixmap_cache.lock() {
      cache.insert(key, Arc::clone(&pixmap));
    }

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
  )> {
    let bytes = &resource.bytes;
    let content_type = resource.content_type.as_deref();

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
      return self
        .render_svg_to_image_with_url(content, url)
        .map(|(img, ratio, aspect_none)| (img, None, None, true, ratio, aspect_none));
    }

    // Regular image - extract EXIF metadata and decode
    let (orientation, resolution) = Self::exif_metadata(bytes);
    self
      .decode_bitmap(bytes, content_type, url)
      .map(|img| (img, orientation, resolution, false, None, false))
  }

  fn probe_resource(&self, resource: &FetchedResource, url: &str) -> Result<CachedImageMetadata> {
    let bytes = &resource.bytes;
    let content_type = resource.content_type.as_deref();

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
    let format_from_content_type = Self::format_from_content_type(content_type);
    let sniffed_format = Self::sniff_image_format(bytes);
    if let Some((width, height)) =
      self.predecoded_dimensions(bytes, format_from_content_type, sniffed_format)
    {
      self.enforce_decode_limits(width, height, url)?;
    }
    let mut last_error: Option<image::ImageError> = None;

    if matches!(format_from_content_type, Some(ImageFormat::Avif))
      || matches!(sniffed_format, Some(ImageFormat::Avif))
    {
      match Self::decode_avif(bytes) {
        Ok(img) => return self.finish_bitmap_decode(img, url),
        Err(err) => last_error = Some(err),
      }
    }

    if let Some(format) = format_from_content_type {
      if format != ImageFormat::Avif {
        match image::load_from_memory_with_format(bytes, format) {
          Ok(img) => return self.finish_bitmap_decode(img, url),
          Err(err) => last_error = Some(err),
        }
      }
    }

    if let Some(format) = sniffed_format {
      if Some(format) != format_from_content_type && format != ImageFormat::Avif {
        match image::load_from_memory_with_format(bytes, format) {
          Ok(img) => return self.finish_bitmap_decode(img, url),
          Err(err) => last_error = Some(err),
        }
      }
    }

    match image::load_from_memory(bytes) {
      Ok(img) => self.finish_bitmap_decode(img, url),
      Err(err) => Err(self.decode_error(url, last_error.unwrap_or(err))),
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

  fn decode_error(&self, url: &str, err: image::ImageError) -> Error {
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
        let mut cursor = Cursor::new(bytes);
        let data = AvifData::from_reader(&mut cursor).ok()?;
        let meta = data.primary_item_metadata().ok()?;
        Some((meta.max_frame_width.get(), meta.max_frame_height.get()))
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

  fn decode_avif(bytes: &[u8]) -> std::result::Result<DynamicImage, image::ImageError> {
    let decoder = AvifDecoder::from_avif(bytes).map_err(|err| Self::avif_error(err))?;
    let image = decoder.to_image().map_err(|err| Self::avif_error(err))?;
    Self::avif_image_to_dynamic(image)
  }

  fn avif_image_to_dynamic(
    image: AvifImage,
  ) -> std::result::Result<DynamicImage, image::ImageError> {
    let dimension_error = || {
      image::ImageError::Parameter(image::error::ParameterError::from_kind(
        image::error::ParameterErrorKind::DimensionMismatch,
      ))
    };

    match image {
      AvifImage::Rgb8(img) => {
        let (width, height) = Self::avif_dimensions(img.width(), img.height())?;
        let mut buf = Vec::with_capacity(width as usize * height as usize * 3);
        for px in img.buf() {
          buf.extend_from_slice(&[px.r, px.g, px.b]);
        }
        image::RgbImage::from_vec(width, height, buf)
          .map(DynamicImage::ImageRgb8)
          .ok_or_else(dimension_error)
      }
      AvifImage::Rgb16(img) => {
        let (width, height) = Self::avif_dimensions(img.width(), img.height())?;
        let mut buf = Vec::with_capacity(width as usize * height as usize * 3);
        for px in img.buf() {
          buf.extend_from_slice(&[px.r, px.g, px.b]);
        }
        image::ImageBuffer::from_vec(width, height, buf)
          .map(DynamicImage::ImageRgb16)
          .ok_or_else(dimension_error)
      }
      AvifImage::Rgba8(img) => {
        let (width, height) = Self::avif_dimensions(img.width(), img.height())?;
        let mut buf = Vec::with_capacity(width as usize * height as usize * 4);
        for px in img.buf() {
          buf.extend_from_slice(&[px.r, px.g, px.b, px.a]);
        }
        image::RgbaImage::from_vec(width, height, buf)
          .map(DynamicImage::ImageRgba8)
          .ok_or_else(dimension_error)
      }
      AvifImage::Rgba16(img) => {
        let (width, height) = Self::avif_dimensions(img.width(), img.height())?;
        let mut buf = Vec::with_capacity(width as usize * height as usize * 4);
        for px in img.buf() {
          buf.extend_from_slice(&[px.r, px.g, px.b, px.a]);
        }
        image::ImageBuffer::from_vec(width, height, buf)
          .map(DynamicImage::ImageRgba16)
          .ok_or_else(dimension_error)
      }
      AvifImage::Gray8(img) => {
        let (width, height) = Self::avif_dimensions(img.width(), img.height())?;
        let mut buf = Vec::with_capacity(width as usize * height as usize);
        for px in img.buf() {
          buf.push(px.value());
        }
        image::ImageBuffer::from_vec(width, height, buf)
          .map(DynamicImage::ImageLuma8)
          .ok_or_else(dimension_error)
      }
      AvifImage::Gray16(img) => {
        let (width, height) = Self::avif_dimensions(img.width(), img.height())?;
        let mut buf = Vec::with_capacity(width as usize * height as usize);
        for px in img.buf() {
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

    // Render SVG to pixmap, scaling to the target intrinsic dimensions when needed
    let mut pixmap = tiny_skia::Pixmap::new(render_width, render_height).ok_or(Error::Render(
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
    resvg::render(&tree, transform, &mut pixmap.as_mut());

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
  use crate::style::types::OrientationTransform;
  use image::RgbaImage;
  use std::path::PathBuf;
  use std::time::SystemTime;

  #[test]
  fn svg_viewbox_renders_with_default_letterboxing() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><rect width='100' height='100' fill='red'/></svg>";

    let pixmap = cache
      .render_svg_pixmap_at_size(svg, 200, 100, "test://svg")
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
      .render_svg_pixmap_at_size(svg, 200, 100, "test://svg")
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
      .render_svg_pixmap_at_size(svg, 200, 100, "test://svg")
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
      .render_svg_pixmap_at_size(svg, 200, 100, "test://svg")
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
      .render_svg_pixmap_at_size(svg, 200, 100, "test://fast-path")
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
}
