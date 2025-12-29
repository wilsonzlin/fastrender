//! Shape resolution for `shape-outside`.
//!
//! Converts CSS `shape-outside` values into per-row horizontal spans that the
//! float context can use to shorten line boxes. Shapes are expanded by
//! `shape-margin` and support basic shapes as well as simple image/gradient
//! masks.

use crate::css::types::{ColorStop, RadialGradientShape, RadialGradientSize};
use crate::geometry::{Point, Rect, Size};
use crate::image_loader::ImageCache;
use crate::paint::clip_path::{resolve_basic_shape, ResolvedClipPath};
use crate::style::color::Rgba;
use crate::style::types::{BackgroundImage, BackgroundPosition, ReferenceBox, ShapeOutside};
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use std::f32::consts::PI;
use tiny_skia::{Mask, PathBuilder, Pixmap, SpreadMode, Transform};

/// Horizontal coverage for a float shape sampled at 1 CSS px increments.
#[derive(Debug, Clone, PartialEq)]
pub struct FloatShape {
  start_y: f32,
  spans: Vec<Option<(f32, f32)>>,
}

impl FloatShape {
  /// Horizontal span at a particular y coordinate.
  pub fn span_at(&self, y: f32) -> Option<(f32, f32)> {
    let idx = ((y - self.start_y).floor()) as isize;
    if idx < 0 || idx as usize >= self.spans.len() {
      return None;
    }
    self.spans[idx as usize]
  }

  /// Combined span covering any shape pixels in the given range.
  pub fn span_in_range(&self, y_start: f32, y_end: f32) -> Option<(f32, f32)> {
    if y_end <= self.start_y || y_start >= self.bottom() {
      return None;
    }
    let start_idx = ((y_start - self.start_y).floor()).max(0.0) as usize;
    let end_idx = ((y_end - self.start_y).ceil()).min(self.spans.len() as f32) as usize;
    let mut min_x = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    for span in &self.spans[start_idx..end_idx] {
      if let Some((l, r)) = span {
        min_x = min_x.min(*l);
        max_x = max_x.max(*r);
      }
    }
    if max_x > min_x {
      Some((min_x, max_x))
    } else {
      None
    }
  }

  pub fn top(&self) -> f32 {
    self.start_y
  }

  pub fn bottom(&self) -> f32 {
    self.start_y + self.spans.len() as f32
  }

  /// Returns the next Y coordinate after `y` where the sampled span changes.
  ///
  /// This is used by float layout to find the next vertical boundary that could
  /// alter available inline space when a `shape-outside` float is active.
  pub fn next_change_after(&self, y: f32) -> Option<f32> {
    if self.spans.is_empty() {
      return None;
    }
    let mut idx = ((y - self.start_y).floor() as isize + 1).max(0) as usize;
    let current = self.span_at(y);
    while idx < self.spans.len() {
      if self.spans[idx] != current {
        return Some(self.start_y + idx as f32);
      }
      idx += 1;
    }
    None
  }
}

/// Resolve the float shape for `shape-outside`.
///
/// Returns `None` when no special wrapping shape applies (fallback to the float's
/// margin box rectangle).
pub fn build_float_shape(
  style: &ComputedStyle,
  margin_box: Rect,
  border_box: Rect,
  containing_block: Size,
  viewport: Size,
  font_ctx: &FontContext,
  image_cache: &ImageCache,
) -> Option<FloatShape> {
  let shape_margin = resolve_shape_margin_px(style.shape_margin, style, viewport);
  let reference_boxes = compute_reference_boxes(
    style,
    margin_box,
    border_box,
    containing_block,
    viewport,
    font_ctx,
  );

  match &style.shape_outside {
    ShapeOutside::None => None,
    ShapeOutside::Box(reference) => {
      let rect = select_reference_box(reference_boxes, *reference);
      Some(expand_spans(rect_span(rect), shape_margin))
    }
    ShapeOutside::BasicShape(basic, reference_override) => {
      let reference = reference_override.unwrap_or(ReferenceBox::MarginBox);
      let reference_rect = select_reference_box(reference_boxes, reference);
      let resolved = resolve_basic_shape(
        basic,
        reference_rect,
        style,
        (viewport.width, viewport.height),
        font_ctx,
      )?;
      let (mask, origin) = rasterize_clip_shape(&resolved)?;
      let base = spans_from_mask(&mask, origin, 0.0);
      Some(expand_spans(base, shape_margin))
    }
    ShapeOutside::Image(image) => {
      let reference_rect = reference_boxes.margin;
      let bitmap = image_mask(image, reference_rect, style, viewport, image_cache)?;
      let base = spans_from_alpha_pixels(
        bitmap.width,
        bitmap.height,
        &bitmap.data,
        Point::new(reference_rect.x(), reference_rect.y()),
        style.shape_image_threshold,
      );
      Some(expand_spans(base, shape_margin))
    }
  }
}

fn resolve_shape_margin_px(margin: Length, style: &ComputedStyle, viewport: Size) -> f32 {
  margin
    .resolve_with_context(
      None,
      viewport.width,
      viewport.height,
      style.font_size,
      style.root_font_size,
    )
    .unwrap_or(margin.value)
    .max(0.0)
}

#[derive(Clone, Copy)]
struct ReferenceRects {
  border: Rect,
  padding: Rect,
  content: Rect,
  margin: Rect,
}

fn compute_reference_boxes(
  style: &ComputedStyle,
  margin_box: Rect,
  border_box: Rect,
  containing_block: Size,
  viewport: Size,
  font_ctx: &FontContext,
) -> ReferenceRects {
  let resolve = |len: Length| -> f32 {
    crate::layout::utils::resolve_length_with_percentage_metrics(
      len,
      Some(containing_block.width),
      viewport,
      style.font_size,
      style.root_font_size,
      Some(style),
      Some(font_ctx),
    )
    .unwrap_or(len.value)
    .max(0.0)
  };

  let border_left = resolve(style.border_left_width);
  let border_right = resolve(style.border_right_width);
  let border_top = resolve(style.border_top_width);
  let border_bottom = resolve(style.border_bottom_width);

  let padding_left = resolve(style.padding_left);
  let padding_right = resolve(style.padding_right);
  let padding_top = resolve(style.padding_top);
  let padding_bottom = resolve(style.padding_bottom);

  let padding_rect = inset_rect(
    border_box,
    border_left,
    border_top,
    border_right,
    border_bottom,
  );
  let content_rect = inset_rect(
    padding_rect,
    padding_left,
    padding_top,
    padding_right,
    padding_bottom,
  );

  ReferenceRects {
    border: border_box,
    padding: padding_rect,
    content: content_rect,
    margin: margin_box,
  }
}

fn select_reference_box(boxes: ReferenceRects, reference: ReferenceBox) -> Rect {
  match reference {
    ReferenceBox::BorderBox
    | ReferenceBox::FillBox
    | ReferenceBox::StrokeBox
    | ReferenceBox::ViewBox => boxes.border,
    ReferenceBox::PaddingBox => boxes.padding,
    ReferenceBox::ContentBox => boxes.content,
    ReferenceBox::MarginBox => boxes.margin,
  }
}

fn inset_rect(rect: Rect, left: f32, top: f32, right: f32, bottom: f32) -> Rect {
  Rect::from_xywh(
    rect.x() + left,
    rect.y() + top,
    (rect.width() - left - right).max(0.0),
    (rect.height() - top - bottom).max(0.0),
  )
}

fn rect_span(rect: Rect) -> SpanBuffer {
  let start_y = rect.y();
  let height = rect.height().ceil().max(0.0) as usize;
  let mut spans = vec![None; height];
  for (idx, span) in spans.iter_mut().enumerate() {
    let y = start_y + idx as f32;
    if y >= rect.y() && y < rect.max_y() {
      *span = Some((rect.x(), rect.max_x()));
    }
  }
  SpanBuffer { start_y, spans }
}

fn expand_spans(base: SpanBuffer, margin: f32) -> FloatShape {
  if margin <= 0.0 {
    return FloatShape {
      start_y: base.start_y,
      spans: base.spans,
    };
  }

  let start_y = base.start_y - margin;
  let end_y = base.start_y + base.spans.len() as f32 + margin;
  let out_len = (end_y - start_y).ceil().max(0.0) as usize;
  let mut spans = vec![None; out_len];

  for (row_idx, span) in base.spans.iter().enumerate() {
    let Some((min_x, max_x)) = span else { continue };
    let base_center_y = base.start_y + row_idx as f32 + 0.5;
    let out_start = ((base_center_y - margin - start_y - 0.5).floor()).max(0.0) as usize;
    let out_end = ((base_center_y + margin - start_y - 0.5).ceil()).min(out_len as f32) as usize;

    for out_idx in out_start..out_end {
      let center_y = start_y + out_idx as f32 + 0.5;
      let dy = (center_y - base_center_y).abs();
      if dy > margin {
        continue;
      }
      let dx = (margin * margin - dy * dy).max(0.0).sqrt();
      let entry: &mut Option<(f32, f32)> = &mut spans[out_idx];
      match entry {
        Some((l, r)) => {
          *l = l.min(min_x - dx);
          *r = r.max(max_x + dx);
        }
        None => *entry = Some((min_x - dx, max_x + dx)),
      }
    }
  }

  FloatShape { start_y, spans }
}

fn rasterize_clip_shape(shape: &ResolvedClipPath) -> Option<(Mask, Point)> {
  let bounds = shape.bounds();
  if bounds.width() <= 0.0 || bounds.height() <= 0.0 {
    return None;
  }
  let origin = Point::new(bounds.x().floor(), bounds.y().floor());
  let width = (bounds.max_x() - origin.x).ceil().max(0.0) as u32;
  let height = (bounds.max_y() - origin.y).ceil().max(0.0) as u32;
  let translated = shape.translate(-origin.x, -origin.y);
  translated
    .mask(
      1.0,
      tiny_skia::IntSize::from_wh(width, height)?,
      Transform::identity(),
    )
    .map(|m| (m, origin))
}

fn spans_from_mask(mask: &Mask, origin: Point, threshold: f32) -> SpanBuffer {
  let width = mask.width();
  let height = mask.height();
  let data = mask.data();
  let mut spans = Vec::with_capacity(height as usize);
  let start_y = origin.y;
  let row_stride = if height > 0 {
    data.len() / height as usize
  } else {
    0
  };
  let threshold_u8 = (threshold.clamp(0.0, 1.0) * 255.0) as u8;

  let mut row_start = 0usize;
  for _ in 0..height {
    let mut min_x = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    for x in 0..width {
      let alpha = data[row_start + x as usize];
      if alpha > threshold_u8 {
        min_x = min_x.min(origin.x + x as f32);
        max_x = max_x.max(origin.x + x as f32 + 1.0);
      }
    }
    if max_x > min_x {
      spans.push(Some((min_x, max_x)));
    } else {
      spans.push(None);
    }
    row_start += row_stride;
  }

  SpanBuffer { start_y, spans }
}

struct SpanBuffer {
  start_y: f32,
  spans: Vec<Option<(f32, f32)>>,
}

struct AlphaBitmap {
  width: u32,
  height: u32,
  data: Vec<u8>,
}

fn spans_from_alpha_pixels(
  width: u32,
  height: u32,
  data: &[u8],
  origin: Point,
  threshold: f32,
) -> SpanBuffer {
  let mut spans = Vec::with_capacity(height as usize);
  let threshold_u8 = (threshold.clamp(0.0, 1.0) * 255.0) as u8;
  for row in 0..height {
    let row_start = (row * width) as usize;
    let mut min_x = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    for col in 0..width {
      if data[row_start + col as usize] > threshold_u8 {
        min_x = min_x.min(origin.x + col as f32);
        max_x = max_x.max(origin.x + col as f32 + 1.0);
      }
    }
    if max_x > min_x {
      spans.push(Some((min_x, max_x)));
    } else {
      spans.push(None);
    }
  }

  SpanBuffer {
    start_y: origin.y,
    spans,
  }
}

fn image_mask(
  image: &BackgroundImage,
  reference_rect: Rect,
  style: &ComputedStyle,
  viewport: Size,
  image_cache: &ImageCache,
) -> Option<AlphaBitmap> {
  match image {
    BackgroundImage::Url(url) => {
      let cached = image_cache.load(url).ok()?;
      let transform = style.image_orientation.resolve(cached.orientation, true);
      let rgba = cached.to_oriented_rgba(transform);
      let (width, height) = rgba.dimensions();
      let mut alpha = Vec::with_capacity((width * height) as usize);
      for chunk in rgba.as_raw().chunks_exact(4) {
        alpha.push(chunk[3]);
      }
      Some(AlphaBitmap {
        width,
        height,
        data: alpha,
      })
    }
    BackgroundImage::LinearGradient { angle, stops } => {
      let (width, height, origin) = image_size(reference_rect);
      let pixmap = render_linear_gradient(*angle, stops, style.color, width, height)?;
      Some(alpha_from_pixmap(pixmap, origin))
    }
    BackgroundImage::RepeatingLinearGradient { angle, stops } => {
      let (width, height, origin) = image_size(reference_rect);
      let pixmap = render_linear_gradient_repeat(*angle, stops, style.color, width, height)?;
      Some(alpha_from_pixmap(pixmap, origin))
    }
    BackgroundImage::RadialGradient {
      shape,
      size,
      position,
      stops,
    } => {
      let (width, height, origin) = image_size(reference_rect);
      let pixmap = render_radial_gradient_image(
        *shape,
        size,
        position,
        stops,
        style,
        viewport,
        reference_rect,
        width,
        height,
        false,
      )?;
      Some(alpha_from_pixmap(pixmap, origin))
    }
    BackgroundImage::RepeatingRadialGradient {
      shape,
      size,
      position,
      stops,
    } => {
      let (width, height, origin) = image_size(reference_rect);
      let pixmap = render_radial_gradient_image(
        *shape,
        size,
        position,
        stops,
        style,
        viewport,
        reference_rect,
        width,
        height,
        true,
      )?;
      Some(alpha_from_pixmap(pixmap, origin))
    }
    BackgroundImage::ConicGradient {
      from_angle,
      position,
      stops,
    } => render_conic_gradient_alpha(
      *from_angle,
      position,
      stops,
      style,
      viewport,
      reference_rect,
      false,
    ),
    BackgroundImage::RepeatingConicGradient {
      from_angle,
      position,
      stops,
    } => render_conic_gradient_alpha(
      *from_angle,
      position,
      stops,
      style,
      viewport,
      reference_rect,
      true,
    ),
    _ => None,
  }
}

fn image_size(rect: Rect) -> (u32, u32, Point) {
  let origin = Point::new(rect.x(), rect.y());
  let width = rect.width().ceil().max(1.0) as u32;
  let height = rect.height().ceil().max(1.0) as u32;
  (width, height, origin)
}

fn alpha_from_pixmap(pixmap: Pixmap, _origin: Point) -> AlphaBitmap {
  let mut alpha = Vec::with_capacity((pixmap.width() * pixmap.height()) as usize);
  for chunk in pixmap.data().chunks_exact(4) {
    alpha.push(chunk[3]);
  }
  AlphaBitmap {
    width: pixmap.width(),
    height: pixmap.height(),
    data: alpha,
  }
}

fn render_linear_gradient(
  angle: f32,
  stops: &[ColorStop],
  current_color: Rgba,
  width: u32,
  height: u32,
) -> Option<Pixmap> {
  let resolved = normalize_color_stops(stops, current_color);
  if resolved.is_empty() {
    return None;
  }
  let sk_stops = gradient_stops(&resolved);
  let rect = Rect::from_xywh(0.0, 0.0, width as f32, height as f32);
  let rad = angle.to_radians();
  let dx = rad.sin();
  let dy = -rad.cos();
  let len = 0.5 * (rect.width() * dx.abs() + rect.height() * dy.abs());
  let cx = rect.x() + rect.width() / 2.0;
  let cy = rect.y() + rect.height() / 2.0;

  let start = tiny_skia::Point::from_xy(cx - dx * len, cy - dy * len);
  let end = tiny_skia::Point::from_xy(cx + dx * len, cy + dy * len);
  let shader =
    tiny_skia::LinearGradient::new(start, end, sk_stops, SpreadMode::Pad, Transform::identity())?;

  let mut pixmap = Pixmap::new(width, height)?;
  let sk_rect = tiny_skia::Rect::from_xywh(0.0, 0.0, width as f32, height as f32)?;
  let path = PathBuilder::from_rect(sk_rect);
  let mut paint = tiny_skia::Paint::default();
  paint.shader = shader;
  paint.anti_alias = true;
  pixmap.fill_path(
    &path,
    &paint,
    tiny_skia::FillRule::Winding,
    Transform::identity(),
    None,
  );
  Some(pixmap)
}

fn render_linear_gradient_repeat(
  angle: f32,
  stops: &[ColorStop],
  current_color: Rgba,
  width: u32,
  height: u32,
) -> Option<Pixmap> {
  let resolved = normalize_color_stops(stops, current_color);
  if resolved.is_empty() {
    return None;
  }
  let sk_stops = gradient_stops(&resolved);
  let rect = Rect::from_xywh(0.0, 0.0, width as f32, height as f32);
  let rad = angle.to_radians();
  let dx = rad.sin();
  let dy = -rad.cos();
  let len = 0.5 * (rect.width() * dx.abs() + rect.height() * dy.abs());
  let cx = rect.x() + rect.width() / 2.0;
  let cy = rect.y() + rect.height() / 2.0;

  let start = tiny_skia::Point::from_xy(cx - dx * len, cy - dy * len);
  let end = tiny_skia::Point::from_xy(cx + dx * len, cy + dy * len);
  let shader = tiny_skia::LinearGradient::new(
    start,
    end,
    sk_stops,
    SpreadMode::Repeat,
    Transform::identity(),
  )?;

  let mut pixmap = Pixmap::new(width, height)?;
  let sk_rect = tiny_skia::Rect::from_xywh(0.0, 0.0, width as f32, height as f32)?;
  let path = PathBuilder::from_rect(sk_rect);
  let mut paint = tiny_skia::Paint::default();
  paint.shader = shader;
  paint.anti_alias = true;
  pixmap.fill_path(
    &path,
    &paint,
    tiny_skia::FillRule::Winding,
    Transform::identity(),
    None,
  );
  Some(pixmap)
}

fn render_radial_gradient_image(
  shape: RadialGradientShape,
  size: &RadialGradientSize,
  position: &BackgroundPosition,
  stops: &[ColorStop],
  style: &ComputedStyle,
  viewport: Size,
  reference_rect: Rect,
  width: u32,
  height: u32,
  repeat: bool,
) -> Option<Pixmap> {
  let resolved = normalize_color_stops(stops, style.color);
  if resolved.is_empty() {
    return None;
  }
  let sk_stops = gradient_stops(&resolved);
  let spread = if repeat {
    SpreadMode::Repeat
  } else {
    SpreadMode::Pad
  };

  let (cx, cy, radius_x, radius_y) = radial_geometry(
    Rect::from_xywh(0.0, 0.0, reference_rect.width(), reference_rect.height()),
    position,
    size,
    shape,
    style.font_size,
    style.root_font_size,
    Some((viewport.width, viewport.height)),
  );
  if radius_x <= 0.0 || radius_y <= 0.0 {
    return None;
  }

  let Some(path_rect) = tiny_skia::Rect::from_xywh(0.0, 0.0, width as f32, height as f32) else {
    return None;
  };

  let Some(shader) = tiny_skia::RadialGradient::new(
    tiny_skia::Point::from_xy(0.0, 0.0),
    tiny_skia::Point::from_xy(0.0, 0.0),
    1.0,
    sk_stops,
    spread,
    Transform::from_translate(cx, cy).pre_scale(radius_x, radius_y),
  ) else {
    return None;
  };

  let mut pixmap = Pixmap::new(width, height)?;
  let path = PathBuilder::from_rect(path_rect);
  let mut paint = tiny_skia::Paint::default();
  paint.shader = shader;
  paint.anti_alias = true;
  pixmap.fill_path(
    &path,
    &paint,
    tiny_skia::FillRule::Winding,
    Transform::identity(),
    None,
  );
  Some(pixmap)
}

fn render_conic_gradient_alpha(
  from_angle: f32,
  position: &BackgroundPosition,
  stops: &[ColorStop],
  style: &ComputedStyle,
  viewport: Size,
  reference_rect: Rect,
  repeating: bool,
) -> Option<AlphaBitmap> {
  let resolved = normalize_color_stops_unclamped(stops, style.color);
  if resolved.is_empty() {
    return None;
  }

  let width = reference_rect.width().ceil().max(1.0) as u32;
  let height = reference_rect.height().ceil().max(1.0) as u32;
  if width == 0 || height == 0 {
    return None;
  }

  let center = resolve_gradient_center(
    Rect::from_xywh(0.0, 0.0, reference_rect.width(), reference_rect.height()),
    position,
    style.font_size,
    style.root_font_size,
    Some((viewport.width, viewport.height)),
  );

  let start_angle = from_angle.to_radians();
  let period = if repeating {
    resolved.last().map(|s| s.0).unwrap_or(1.0).max(1e-6)
  } else {
    1.0
  };

  let mut data = Vec::with_capacity((width * height) as usize);
  for y in 0..height {
    for x in 0..width {
      let dx = x as f32 + 0.5 - center.x;
      let dy = y as f32 + 0.5 - center.y;
      let angle = dx.atan2(-dy) + start_angle;
      let mut t = (angle / (2.0 * PI)).rem_euclid(1.0);
      t *= period;
      let color = sample_conic_stops(&resolved, t, repeating, period);
      data.push((color.a * 255.0).round().clamp(0.0, 255.0) as u8);
    }
  }

  Some(AlphaBitmap {
    width,
    height,
    data,
  })
}

fn normalize_color_stops(stops: &[ColorStop], current_color: Rgba) -> Vec<(f32, Rgba)> {
  if stops.is_empty() {
    return Vec::new();
  }
  let mut resolved: Vec<(Option<f32>, Rgba)> = stops
    .iter()
    .map(|s| (s.position, s.color.to_rgba(current_color)))
    .collect();
  if resolved.first().map(|(p, _)| p.is_none()).unwrap_or(false) {
    resolved[0].0 = Some(0.0);
  }
  if let Some(last) = resolved.last_mut() {
    if last.0.is_none() {
      last.0 = Some(1.0);
    }
  }

  let len = resolved.len();
  let mut i = 0;
  while i < len {
    if resolved[i].0.is_some() {
      i += 1;
      continue;
    }
    let start = i - 1;
    let mut end = i;
    while end < len && resolved[end].0.is_none() {
      end += 1;
    }
    let start_pos = resolved[start].0.unwrap_or(0.0);
    let end_pos = resolved.get(end).and_then(|(p, _)| *p).unwrap_or(1.0);
    let count = end - start;
    for j in 0..(count - 1) {
      let pos = start_pos + (end_pos - start_pos) * ((j + 1) as f32 / count as f32);
      resolved[start + j + 1].0 = Some(pos);
    }
    i = end;
  }

  resolved
    .into_iter()
    .map(|(p, c)| (p.unwrap_or(0.0).clamp(0.0, 1.0), c))
    .collect()
}

fn gradient_stops(stops: &[(f32, Rgba)]) -> Vec<tiny_skia::GradientStop> {
  stops
    .iter()
    .map(|(pos, color)| {
      tiny_skia::GradientStop::new(
        *pos,
        tiny_skia::Color::from_rgba8(color.r, color.g, color.b, (color.a * 255.0).round() as u8),
      )
    })
    .collect()
}

fn normalize_color_stops_unclamped(stops: &[ColorStop], current_color: Rgba) -> Vec<(f32, Rgba)> {
  if stops.is_empty() {
    return Vec::new();
  }

  let mut positions: Vec<Option<f32>> = stops.iter().map(|s| s.position).collect();
  if positions.iter().all(|p| p.is_none()) {
    if stops.len() == 1 {
      return vec![(0.0, stops[0].color.to_rgba(current_color))];
    }
    let denom = (stops.len() - 1) as f32;
    return stops
      .iter()
      .enumerate()
      .map(|(i, s)| (i as f32 / denom, s.color.to_rgba(current_color)))
      .collect();
  }

  if positions.first().and_then(|p| *p).is_none() {
    positions[0] = Some(0.0);
  }
  if positions.last().and_then(|p| *p).is_none() {
    if let Some(last) = positions.last_mut() {
      *last = Some(1.0);
    }
  }

  let mut last_known: Option<(usize, f32)> = None;
  for i in 0..positions.len() {
    if let Some(pos) = positions[i] {
      if let Some((start_idx, start_pos)) = last_known {
        let gap = i.saturating_sub(start_idx + 1);
        if gap > 0 {
          let step = (pos - start_pos) / (gap as f32 + 1.0);
          for (offset, slot) in positions[start_idx + 1..i].iter_mut().enumerate() {
            *slot = Some(start_pos + step * (offset + 1) as f32);
          }
        }
      } else if i > 0 {
        let gap = i;
        let step = pos / gap as f32;
        for (j, slot) in positions.iter_mut().take(i).enumerate() {
          *slot = Some(step * j as f32);
        }
      }
      last_known = Some((i, pos));
    }
  }

  let mut output = Vec::with_capacity(stops.len());
  let mut prev = 0.0;
  for (idx, pos_opt) in positions.into_iter().enumerate() {
    let pos = pos_opt.unwrap_or(prev);
    let monotonic = pos.max(prev);
    prev = monotonic;
    output.push((monotonic, stops[idx].color.to_rgba(current_color)));
  }
  output
}

fn resolve_length_for_paint(
  len: &Length,
  font_size: f32,
  root_font_size: f32,
  percentage_base: f32,
  viewport: Option<(f32, f32)>,
) -> f32 {
  len
    .resolve_with_context(
      Some(percentage_base),
      viewport.map(|v| v.0).unwrap_or(0.0),
      viewport.map(|v| v.1).unwrap_or(0.0),
      font_size,
      root_font_size,
    )
    .unwrap_or(len.value)
}

fn radial_geometry(
  rect: Rect,
  position: &BackgroundPosition,
  size: &RadialGradientSize,
  shape: RadialGradientShape,
  font_size: f32,
  root_font_size: f32,
  viewport: Option<(f32, f32)>,
) -> (f32, f32, f32, f32) {
  let (align_x, off_x, align_y, off_y) = match position {
    BackgroundPosition::Position { x, y } => {
      let ox =
        resolve_length_for_paint(&x.offset, font_size, root_font_size, rect.width(), viewport);
      let oy = resolve_length_for_paint(
        &y.offset,
        font_size,
        root_font_size,
        rect.height(),
        viewport,
      );
      (x.alignment, ox, y.alignment, oy)
    }
  };
  let cx = rect.x() + align_x * rect.width() + off_x;
  let cy = rect.y() + align_y * rect.height() + off_y;

  let dx_left = (cx - rect.x()).max(0.0);
  let dx_right = (rect.x() + rect.width() - cx).max(0.0);
  let dy_top = (cy - rect.y()).max(0.0);
  let dy_bottom = (rect.y() + rect.height() - cy).max(0.0);

  let (mut rx, mut ry) = match size {
    RadialGradientSize::ClosestSide => (dx_left.min(dx_right), dy_top.min(dy_bottom)),
    RadialGradientSize::FarthestSide => (dx_left.max(dx_right), dy_top.max(dy_bottom)),
    RadialGradientSize::ClosestCorner => {
      let corners = [
        (dx_left, dy_top),
        (dx_left, dy_bottom),
        (dx_right, dy_top),
        (dx_right, dy_bottom),
      ];
      let mut best = f32::INFINITY;
      let mut best_pair = (0.0, 0.0);
      for (dx, dy) in corners {
        let dist = (dx * dx + dy * dy).sqrt();
        if dist < best {
          best = dist;
          best_pair = (dx, dy);
        }
      }
      (
        best_pair.0 * std::f32::consts::SQRT_2,
        best_pair.1 * std::f32::consts::SQRT_2,
      )
    }
    RadialGradientSize::FarthestCorner => {
      let corners = [
        (dx_left, dy_top),
        (dx_left, dy_bottom),
        (dx_right, dy_top),
        (dx_right, dy_bottom),
      ];
      let mut best = -f32::INFINITY;
      let mut best_pair = (0.0, 0.0);
      for (dx, dy) in corners {
        let dist = (dx * dx + dy * dy).sqrt();
        if dist > best {
          best = dist;
          best_pair = (dx, dy);
        }
      }
      (
        best_pair.0 * std::f32::consts::SQRT_2,
        best_pair.1 * std::f32::consts::SQRT_2,
      )
    }
    RadialGradientSize::Explicit { x, y } => {
      let rx =
        resolve_length_for_paint(x, font_size, root_font_size, rect.width(), viewport).max(0.0);
      let ry = y
        .as_ref()
        .map(|yy| {
          resolve_length_for_paint(yy, font_size, root_font_size, rect.height(), viewport).max(0.0)
        })
        .unwrap_or(rx);
      (rx, ry)
    }
  };

  if matches!(shape, RadialGradientShape::Circle) {
    let r = if matches!(
      size,
      RadialGradientSize::ClosestCorner | RadialGradientSize::FarthestCorner
    ) {
      let avg = (rx * rx + ry * ry) / 2.0;
      avg.sqrt()
    } else {
      rx.min(ry)
    };
    rx = r;
    ry = r;
  }

  (cx, cy, rx, ry)
}

fn resolve_gradient_center(
  rect: Rect,
  position: &BackgroundPosition,
  font_size: f32,
  root_font_size: f32,
  viewport: Option<(f32, f32)>,
) -> Point {
  let (align_x, off_x, align_y, off_y) = match position {
    BackgroundPosition::Position { x, y } => {
      let ox =
        resolve_length_for_paint(&x.offset, font_size, root_font_size, rect.width(), viewport);
      let oy = resolve_length_for_paint(
        &y.offset,
        font_size,
        root_font_size,
        rect.height(),
        viewport,
      );
      (x.alignment, ox, y.alignment, oy)
    }
  };
  Point::new(
    rect.x() + align_x * rect.width() + off_x,
    rect.y() + align_y * rect.height() + off_y,
  )
}

fn sample_conic_stops(stops: &[(f32, Rgba)], t: f32, repeating: bool, period: f32) -> Rgba {
  if stops.is_empty() {
    return Rgba::TRANSPARENT;
  }
  if stops.len() == 1 {
    return stops[0].1;
  }
  let total = if repeating {
    period
  } else {
    stops.last().map(|s| s.0).unwrap_or(1.0)
  };
  let mut pos = t;
  if repeating && total > 0.0 {
    pos = pos.rem_euclid(total);
  }
  if pos <= stops[0].0 {
    return stops[0].1;
  }
  if pos >= stops.last().unwrap().0 && !repeating {
    return stops.last().unwrap().1;
  }
  for window in stops.windows(2) {
    let (p0, c0) = window[0];
    let (p1, c1) = window[1];
    if pos < p0 {
      return c0;
    }
    if pos <= p1 || (repeating && (p1 - p0).abs() < f32::EPSILON) {
      let span = (p1 - p0).max(1e-6);
      let frac = ((pos - p0) / span).clamp(0.0, 1.0);
      return Rgba {
        r: ((1.0 - frac) * c0.r as f32 + frac * c1.r as f32)
          .round()
          .clamp(0.0, 255.0) as u8,
        g: ((1.0 - frac) * c0.g as f32 + frac * c1.g as f32)
          .round()
          .clamp(0.0, 255.0) as u8,
        b: ((1.0 - frac) * c0.b as f32 + frac * c1.b as f32)
          .round()
          .clamp(0.0, 255.0) as u8,
        a: (1.0 - frac) * c0.a + frac * c1.a,
      };
    }
  }

  stops.last().map(|(_, c)| *c).unwrap_or(Rgba::TRANSPARENT)
}
