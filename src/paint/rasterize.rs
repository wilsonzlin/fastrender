//! Path rendering primitives using tiny-skia
//!
//! This module provides low-level rendering functions for CSS paint primitives:
//! - Rectangles (fill and stroke)
//! - Rounded rectangles (border-radius)
//! - Borders (all four sides with different colors/widths)
//! - Box shadows (inset and outset)
//!
//! All rendering is done using tiny-skia's path-based approach for proper
//! anti-aliasing and high-quality output.
//!
//! # CSS Specification References
//!
//! - CSS Backgrounds and Borders Module Level 3
//! - CSS 2.1 Section 8.5 (Borders)
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::paint::rasterize::{fill_rect, fill_rounded_rect, BorderRadii};
//! use fastrender::Rgba;
//! use tiny_skia::Pixmap;
//!
//! let mut pixmap = new_pixmap(100, 100).unwrap();
//!
//! // Fill a simple rectangle
//! fill_rect(&mut pixmap, 10.0, 10.0, 80.0, 80.0, Rgba::rgb(255, 0, 0));
//!
//! // Fill a rounded rectangle
//! let radii = BorderRadii::uniform(8.0);
//! fill_rounded_rect(&mut pixmap, 20.0, 20.0, 60.0, 60.0, &radii, Rgba::rgb(0, 255, 0));
//! ```

use crate::error::{RenderError, RenderStage};
use crate::geometry::Rect;
use crate::paint::blur::{apply_gaussian_blur_cached, BlurCache};
use crate::paint::display_list::BorderRadii;
use crate::paint::display_list::BorderRadius;
use crate::paint::pixmap::new_pixmap;
use crate::render_control::check_active_periodic;
use crate::style::color::Rgba;
use tiny_skia::FillRule;
use tiny_skia::LineCap;
use tiny_skia::LineJoin;
use tiny_skia::Paint;
use tiny_skia::Path;
use tiny_skia::PathBuilder;
use tiny_skia::Pixmap;
use tiny_skia::PixmapPaint;
use tiny_skia::PremultipliedColorU8;
use tiny_skia::Stroke;
use tiny_skia::Transform;

const DEADLINE_STRIDE: usize = 16 * 1024;

/// Border widths for all four sides
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct BorderWidths {
  /// Top border width
  pub top: f32,
  /// Right border width
  pub right: f32,
  /// Bottom border width
  pub bottom: f32,
  /// Left border width
  pub left: f32,
}

impl BorderWidths {
  /// Creates uniform border widths
  pub const fn uniform(width: f32) -> Self {
    Self {
      top: width,
      right: width,
      bottom: width,
      left: width,
    }
  }

  /// Creates border widths from individual values
  pub const fn new(top: f32, right: f32, bottom: f32, left: f32) -> Self {
    Self {
      top,
      right,
      bottom,
      left,
    }
  }

  /// Returns true if any border has non-zero width
  pub fn has_border(&self) -> bool {
    self.top > 0.0 || self.right > 0.0 || self.bottom > 0.0 || self.left > 0.0
  }
}

/// Border colors for all four sides
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BorderColors {
  /// Top border color
  pub top: Rgba,
  /// Right border color
  pub right: Rgba,
  /// Bottom border color
  pub bottom: Rgba,
  /// Left border color
  pub left: Rgba,
}

impl BorderColors {
  /// Creates uniform border colors
  pub const fn uniform(color: Rgba) -> Self {
    Self {
      top: color,
      right: color,
      bottom: color,
      left: color,
    }
  }

  /// Creates border colors from individual values
  pub const fn new(top: Rgba, right: Rgba, bottom: Rgba, left: Rgba) -> Self {
    Self {
      top,
      right,
      bottom,
      left,
    }
  }
}

impl Default for BorderColors {
  fn default() -> Self {
    Self::uniform(Rgba::BLACK)
  }
}

/// Box shadow parameters
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BoxShadow {
  /// Horizontal offset (positive = right)
  pub offset_x: f32,
  /// Vertical offset (positive = down)
  pub offset_y: f32,
  /// Blur radius (gaussian blur standard deviation)
  pub blur_radius: f32,
  /// Spread radius (expands/contracts shadow)
  pub spread_radius: f32,
  /// Shadow color (typically with alpha)
  pub color: Rgba,
  /// Whether this is an inset shadow
  pub inset: bool,
}

impl BoxShadow {
  /// Creates a new outset box shadow
  pub const fn new(
    offset_x: f32,
    offset_y: f32,
    blur_radius: f32,
    spread_radius: f32,
    color: Rgba,
  ) -> Self {
    Self {
      offset_x,
      offset_y,
      blur_radius,
      spread_radius,
      color,
      inset: false,
    }
  }

  /// Creates a new inset box shadow
  pub const fn inset(
    offset_x: f32,
    offset_y: f32,
    blur_radius: f32,
    spread_radius: f32,
    color: Rgba,
  ) -> Self {
    Self {
      offset_x,
      offset_y,
      blur_radius,
      spread_radius,
      color,
      inset: true,
    }
  }
}

/// Estimates the pixel work required to rasterize a box shadow into its temporary surface.
///
/// This mirrors the temporary pixmap sizing logic used by `render_box_shadow_cached` so paint
/// parallelism heuristics can account for expensive blur-heavy shadows without allocating.
pub(crate) fn estimate_box_shadow_work_pixels(width: f32, height: f32, shadow: &BoxShadow) -> u64 {
  if shadow.color.a == 0.0 {
    return 0;
  }

  let sigma = shadow.blur_radius.max(0.0);
  let blur_pad = (sigma * 3.0).ceil().max(0.0);
  let spread = shadow.spread_radius;

  if shadow.inset {
    let pad_x = (blur_pad + shadow.offset_x.abs() + spread.abs()).ceil().max(0.0);
    let pad_y = (blur_pad + shadow.offset_y.abs() + spread.abs()).ceil().max(0.0);
    let w = (width + pad_x * 2.0).max(1.0).ceil() as u64;
    let h = (height + pad_y * 2.0).max(1.0).ceil() as u64;
    return w.saturating_mul(h);
  }

  // Outset shadows expand by spread radius and include 3σ blur padding.
  let shadow_w = (width + spread * 2.0).max(0.0);
  let shadow_h = (height + spread * 2.0).max(0.0);
  let w = (shadow_w + blur_pad * 2.0).max(1.0).ceil() as u64;
  let h = (shadow_h + blur_pad * 2.0).max(1.0).ceil() as u64;
  w.saturating_mul(h)
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Converts our Rgba type to tiny-skia's Color
fn to_skia_color(color: Rgba) -> tiny_skia::Color {
  tiny_skia::Color::from_rgba8(color.r, color.g, color.b, color.alpha_u8())
}

/// Creates a Paint object with the given color
fn make_paint(color: Rgba) -> Paint<'static> {
  let mut paint = Paint::default();
  paint.set_color(to_skia_color(color));
  paint.anti_alias = true;
  paint
}

/// Creates a Stroke object with the given width
fn make_stroke(width: f32) -> Stroke {
  Stroke {
    width,
    line_cap: LineCap::Butt,
    line_join: LineJoin::Miter,
    miter_limit: 4.0,
    dash: None,
  }
}

// ============================================================================
// Rectangle Rendering
// ============================================================================

/// Fills a rectangle with a solid color
///
/// # Arguments
///
/// * `pixmap` - The pixmap to draw to
/// * `x` - X coordinate of top-left corner
/// * `y` - Y coordinate of top-left corner
/// * `width` - Width of rectangle
/// * `height` - Height of rectangle
/// * `color` - Fill color
///
/// # Returns
///
/// Returns `true` if the rectangle was drawn, `false` if it couldn't be rendered
/// (e.g., zero dimensions or completely transparent color).
pub fn fill_rect(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  color: Rgba,
) -> bool {
  // Skip if completely transparent or zero-sized
  if color.a == 0.0 || width <= 0.0 || height <= 0.0 {
    return false;
  }

  // Create rectangle path
  let rect = match tiny_skia::Rect::from_xywh(x, y, width, height) {
    Some(r) => r,
    None => return false,
  };

  let path = PathBuilder::from_rect(rect);
  let paint = make_paint(color);

  pixmap.fill_path(
    &path,
    &paint,
    FillRule::Winding,
    Transform::identity(),
    None,
  );
  true
}

/// Fills a rectangle from a Rect struct
pub fn fill_rect_from_rect(pixmap: &mut Pixmap, rect: &Rect, color: Rgba) -> bool {
  fill_rect(
    pixmap,
    rect.x(),
    rect.y(),
    rect.width(),
    rect.height(),
    color,
  )
}

/// Strokes a rectangle outline
///
/// # Arguments
///
/// * `pixmap` - The pixmap to draw to
/// * `x` - X coordinate of top-left corner
/// * `y` - Y coordinate of top-left corner
/// * `width` - Width of rectangle
/// * `height` - Height of rectangle
/// * `color` - Stroke color
/// * `stroke_width` - Width of the stroke
///
/// # Returns
///
/// Returns `true` if the rectangle was drawn.
pub fn stroke_rect(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  color: Rgba,
  stroke_width: f32,
) -> bool {
  // Skip if completely transparent or zero-sized
  if color.a == 0.0 || width <= 0.0 || height <= 0.0 || stroke_width <= 0.0 {
    return false;
  }

  // Adjust rectangle to account for stroke centering
  let half_stroke = stroke_width / 2.0;
  let rect = match tiny_skia::Rect::from_xywh(
    x + half_stroke,
    y + half_stroke,
    width - stroke_width,
    height - stroke_width,
  ) {
    Some(r) => r,
    None => return false,
  };

  let path = PathBuilder::from_rect(rect);
  let paint = make_paint(color);
  let stroke = make_stroke(stroke_width);

  pixmap.stroke_path(&path, &paint, &stroke, Transform::identity(), None);
  true
}

// ============================================================================
// Rounded Rectangle Rendering
// ============================================================================

/// Builds a path for a rounded rectangle
///
/// Creates a path with circular arcs at each corner. The radii are
/// automatically clamped to prevent overlap.
pub(crate) fn build_rounded_rect_path(
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  radii: &BorderRadii,
) -> Option<Path> {
  if width <= 0.0 || height <= 0.0 {
    return None;
  }

  // Clamp radii to prevent overlap
  let radii = radii.clamped(width, height);

  // If no radii, return a simple rectangle
  if radii.is_zero() {
    let rect = tiny_skia::Rect::from_xywh(x, y, width, height)?;
    return Some(PathBuilder::from_rect(rect));
  }

  let mut pb = PathBuilder::new();
  let right = x + width;
  let bottom = y + height;
  let tl = radii.top_left;
  let tr = radii.top_right;
  let br = radii.bottom_right;
  let bl = radii.bottom_left;

  const KAPPA: f32 = 0.552_284_8;

  pb.move_to(x + tl.x, y);
  pb.line_to(right - tr.x, y);
  if tr.x > 0.0 || tr.y > 0.0 {
    pb.cubic_to(
      right - tr.x + tr.x * KAPPA,
      y,
      right,
      y + tr.y - tr.y * KAPPA,
      right,
      y + tr.y,
    );
  } else {
    pb.line_to(right, y);
  }

  pb.line_to(right, bottom - br.y);
  if br.x > 0.0 || br.y > 0.0 {
    pb.cubic_to(
      right,
      bottom - br.y + br.y * KAPPA,
      right - br.x * KAPPA,
      bottom,
      right - br.x,
      bottom,
    );
  } else {
    pb.line_to(right, bottom);
  }

  pb.line_to(x + bl.x, bottom);
  if bl.x > 0.0 || bl.y > 0.0 {
    pb.cubic_to(
      x + bl.x * (1.0 - KAPPA),
      bottom,
      x,
      bottom - bl.y + bl.y * KAPPA,
      x,
      bottom - bl.y,
    );
  } else {
    pb.line_to(x, bottom);
  }

  pb.line_to(x, y + tl.y);
  if tl.x > 0.0 || tl.y > 0.0 {
    pb.cubic_to(
      x,
      y + tl.y * (1.0 - KAPPA),
      x + tl.x * (1.0 - KAPPA),
      y,
      x + tl.x,
      y,
    );
  } else {
    pb.line_to(x, y);
  }

  pb.close();
  pb.finish()
}

/// Fills a rounded rectangle with a solid color
///
/// # Arguments
///
/// * `pixmap` - The pixmap to draw to
/// * `x` - X coordinate of top-left corner
/// * `y` - Y coordinate of top-left corner
/// * `width` - Width of rectangle
/// * `height` - Height of rectangle
/// * `radii` - Corner radii
/// * `color` - Fill color
///
/// # Returns
///
/// Returns `true` if the rectangle was drawn.
pub fn fill_rounded_rect(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  radii: &BorderRadii,
  color: Rgba,
) -> bool {
  // Skip if completely transparent
  if color.a == 0.0 {
    return false;
  }

  // If no radii, use simple rectangle
  if radii.is_zero() {
    return fill_rect(pixmap, x, y, width, height, color);
  }

  let path = match build_rounded_rect_path(x, y, width, height, radii) {
    Some(p) => p,
    None => return false,
  };

  let paint = make_paint(color);
  pixmap.fill_path(
    &path,
    &paint,
    FillRule::Winding,
    Transform::identity(),
    None,
  );
  true
}

/// Strokes a rounded rectangle outline
pub fn stroke_rounded_rect(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  radii: &BorderRadii,
  color: Rgba,
  stroke_width: f32,
) -> bool {
  // Skip if completely transparent
  if color.a == 0.0 || stroke_width <= 0.0 {
    return false;
  }

  // If no radii, use simple rectangle
  if radii.is_zero() {
    return stroke_rect(pixmap, x, y, width, height, color, stroke_width);
  }

  // Adjust for stroke centering
  let half_stroke = stroke_width / 2.0;
  let inner_radii = radii.shrink(half_stroke);

  let path = match build_rounded_rect_path(
    x + half_stroke,
    y + half_stroke,
    width - stroke_width,
    height - stroke_width,
    &inner_radii,
  ) {
    Some(p) => p,
    None => return false,
  };

  let paint = make_paint(color);
  let stroke = make_stroke(stroke_width);

  pixmap.stroke_path(&path, &paint, &stroke, Transform::identity(), None);
  true
}

// ============================================================================
// Border Rendering
// ============================================================================

/// Renders CSS borders with potentially different colors and widths per side
///
/// This function renders borders as filled trapezoids/triangles meeting at
/// 45-degree miters, which is the standard CSS border rendering approach.
///
/// # Arguments
///
/// * `pixmap` - The pixmap to draw to
/// * `x` - X coordinate of the box
/// * `y` - Y coordinate of the box
/// * `width` - Width of the box
/// * `height` - Height of the box
/// * `widths` - Border widths for each side
/// * `colors` - Border colors for each side
/// * `radii` - Optional corner radii
///
/// # Returns
///
/// Returns `true` if borders were drawn.
pub fn render_borders(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  widths: &BorderWidths,
  colors: &BorderColors,
  radii: &BorderRadii,
) -> bool {
  if !widths.has_border() {
    return false;
  }

  let mut rendered = false;

  // For rounded borders, use stroke rendering
  if radii.has_radius() {
    rendered |= render_rounded_borders(pixmap, x, y, width, height, widths, colors, radii);
  } else {
    // For straight borders, render each side as a trapezoid
    rendered |= render_top_border(pixmap, x, y, width, widths, colors.top);
    rendered |= render_right_border(pixmap, x, y, width, height, widths, colors.right);
    rendered |= render_bottom_border(pixmap, x, y, width, height, widths, colors.bottom);
    rendered |= render_left_border(pixmap, x, y, height, widths, colors.left);
  }

  rendered
}

/// Renders the top border as a trapezoid
fn render_top_border(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  widths: &BorderWidths,
  color: Rgba,
) -> bool {
  if widths.top <= 0.0 || color.a == 0.0 {
    return false;
  }

  let mut pb = PathBuilder::new();

  // Outer top-left
  pb.move_to(x, y);
  // Outer top-right
  pb.line_to(x + width, y);
  // Inner top-right (diagonal cut for miter)
  pb.line_to(x + width - widths.right, y + widths.top);
  // Inner top-left (diagonal cut for miter)
  pb.line_to(x + widths.left, y + widths.top);
  pb.close();

  if let Some(path) = pb.finish() {
    let paint = make_paint(color);
    pixmap.fill_path(
      &path,
      &paint,
      FillRule::Winding,
      Transform::identity(),
      None,
    );
    return true;
  }

  false
}

/// Renders the right border as a trapezoid
fn render_right_border(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  widths: &BorderWidths,
  color: Rgba,
) -> bool {
  if widths.right <= 0.0 || color.a == 0.0 {
    return false;
  }

  let right = x + width;
  let bottom = y + height;

  let mut pb = PathBuilder::new();

  // Outer top-right
  pb.move_to(right, y);
  // Outer bottom-right
  pb.line_to(right, bottom);
  // Inner bottom-right
  pb.line_to(right - widths.right, bottom - widths.bottom);
  // Inner top-right
  pb.line_to(right - widths.right, y + widths.top);
  pb.close();

  if let Some(path) = pb.finish() {
    let paint = make_paint(color);
    pixmap.fill_path(
      &path,
      &paint,
      FillRule::Winding,
      Transform::identity(),
      None,
    );
    return true;
  }

  false
}

/// Renders the bottom border as a trapezoid
fn render_bottom_border(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  widths: &BorderWidths,
  color: Rgba,
) -> bool {
  if widths.bottom <= 0.0 || color.a == 0.0 {
    return false;
  }

  let right = x + width;
  let bottom = y + height;

  let mut pb = PathBuilder::new();

  // Outer bottom-left
  pb.move_to(x, bottom);
  // Outer bottom-right
  pb.line_to(right, bottom);
  // Inner bottom-right
  pb.line_to(right - widths.right, bottom - widths.bottom);
  // Inner bottom-left
  pb.line_to(x + widths.left, bottom - widths.bottom);
  pb.close();

  if let Some(path) = pb.finish() {
    let paint = make_paint(color);
    pixmap.fill_path(
      &path,
      &paint,
      FillRule::Winding,
      Transform::identity(),
      None,
    );
    return true;
  }

  false
}

/// Renders the left border as a trapezoid
fn render_left_border(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  height: f32,
  widths: &BorderWidths,
  color: Rgba,
) -> bool {
  if widths.left <= 0.0 || color.a == 0.0 {
    return false;
  }

  let bottom = y + height;

  let mut pb = PathBuilder::new();

  // Outer top-left
  pb.move_to(x, y);
  // Outer bottom-left
  pb.line_to(x, bottom);
  // Inner bottom-left
  pb.line_to(x + widths.left, bottom - widths.bottom);
  // Inner top-left
  pb.line_to(x + widths.left, y + widths.top);
  pb.close();

  if let Some(path) = pb.finish() {
    let paint = make_paint(color);
    pixmap.fill_path(
      &path,
      &paint,
      FillRule::Winding,
      Transform::identity(),
      None,
    );
    return true;
  }

  false
}

/// Renders rounded borders using stroke rendering
///
/// For rounded borders, we approximate by stroking rounded rectangles.
/// This is a simplification - full CSS rounded border rendering is complex
/// when colors differ per side.
fn render_rounded_borders(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  widths: &BorderWidths,
  colors: &BorderColors,
  radii: &BorderRadii,
) -> bool {
  // If all colors are the same, we can use simple stroke
  if colors.top == colors.right && colors.right == colors.bottom && colors.bottom == colors.left {
    // Average the widths for the stroke
    let avg_width = (widths.top + widths.right + widths.bottom + widths.left) / 4.0;
    if avg_width > 0.0 {
      return stroke_rounded_rect(pixmap, x, y, width, height, radii, colors.top, avg_width);
    }
    return false;
  }

  // For different colors per side, render each side separately
  // This is an approximation that doesn't perfectly handle corner transitions
  let mut rendered = false;

  // Top border segment
  if widths.top > 0.0 && colors.top.a > 0.0 {
    let half = widths.top / 2.0;
    let inner_radii = radii.shrink(half);
    if let Some(path) = build_top_border_path(x, y, width, widths, &inner_radii) {
      let paint = make_paint(colors.top);
      let stroke = make_stroke(widths.top);
      pixmap.stroke_path(&path, &paint, &stroke, Transform::identity(), None);
      rendered = true;
    }
  }

  // Right border segment
  if widths.right > 0.0 && colors.right.a > 0.0 {
    let half = widths.right / 2.0;
    let inner_radii = radii.shrink(half);
    if let Some(path) = build_right_border_path(x, y, width, height, widths, &inner_radii) {
      let paint = make_paint(colors.right);
      let stroke = make_stroke(widths.right);
      pixmap.stroke_path(&path, &paint, &stroke, Transform::identity(), None);
      rendered = true;
    }
  }

  // Bottom border segment
  if widths.bottom > 0.0 && colors.bottom.a > 0.0 {
    let half = widths.bottom / 2.0;
    let inner_radii = radii.shrink(half);
    if let Some(path) = build_bottom_border_path(x, y, width, height, widths, &inner_radii) {
      let paint = make_paint(colors.bottom);
      let stroke = make_stroke(widths.bottom);
      pixmap.stroke_path(&path, &paint, &stroke, Transform::identity(), None);
      rendered = true;
    }
  }

  // Left border segment
  if widths.left > 0.0 && colors.left.a > 0.0 {
    let half = widths.left / 2.0;
    let inner_radii = radii.shrink(half);
    if let Some(path) = build_left_border_path(x, y, height, widths, &inner_radii) {
      let paint = make_paint(colors.left);
      let stroke = make_stroke(widths.left);
      pixmap.stroke_path(&path, &paint, &stroke, Transform::identity(), None);
      rendered = true;
    }
  }

  rendered
}

/// Builds path for top border segment (with rounded corners)
fn build_top_border_path(
  x: f32,
  y: f32,
  width: f32,
  widths: &BorderWidths,
  radii: &BorderRadii,
) -> Option<Path> {
  let half_top = widths.top / 2.0;
  let half_left = widths.left / 2.0;
  let half_right = widths.right / 2.0;

  let mut pb = PathBuilder::new();

  // Start after top-left corner
  let start_x = x + half_left + radii.top_left.x;
  pb.move_to(start_x, y + half_top);

  // Line to before top-right corner
  let end_x = x + width - half_right - radii.top_right.x;
  pb.line_to(end_x, y + half_top);

  pb.finish()
}

/// Builds path for right border segment (with rounded corners)
fn build_right_border_path(
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  widths: &BorderWidths,
  radii: &BorderRadii,
) -> Option<Path> {
  let half_top = widths.top / 2.0;
  let half_right = widths.right / 2.0;
  let half_bottom = widths.bottom / 2.0;

  let right = x + width - half_right;

  let mut pb = PathBuilder::new();

  // Start after top-right corner
  let start_y = y + half_top + radii.top_right.y;
  pb.move_to(right, start_y);

  // Line to before bottom-right corner
  let end_y = y + height - half_bottom - radii.bottom_right.y;
  pb.line_to(right, end_y);

  pb.finish()
}

/// Builds path for bottom border segment (with rounded corners)
fn build_bottom_border_path(
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  widths: &BorderWidths,
  radii: &BorderRadii,
) -> Option<Path> {
  let half_bottom = widths.bottom / 2.0;
  let half_left = widths.left / 2.0;
  let half_right = widths.right / 2.0;

  let bottom = y + height - half_bottom;

  let mut pb = PathBuilder::new();

  // Start after bottom-right corner
  let start_x = x + width - half_right - radii.bottom_right.x;
  pb.move_to(start_x, bottom);

  // Line to before bottom-left corner
  let end_x = x + half_left + radii.bottom_left.x;
  pb.line_to(end_x, bottom);

  pb.finish()
}

/// Builds path for left border segment (with rounded corners)
fn build_left_border_path(
  x: f32,
  y: f32,
  height: f32,
  widths: &BorderWidths,
  radii: &BorderRadii,
) -> Option<Path> {
  let half_top = widths.top / 2.0;
  let half_left = widths.left / 2.0;
  let half_bottom = widths.bottom / 2.0;

  let left = x + half_left;

  let mut pb = PathBuilder::new();

  // Start after bottom-left corner
  let start_y = y + height - half_bottom - radii.bottom_left.y;
  pb.move_to(left, start_y);

  // Line to before top-left corner
  let end_y = y + half_top + radii.top_left.y;
  pb.line_to(left, end_y);

  pb.finish()
}

// ============================================================================
// Box Shadow Rendering
// ============================================================================

/// Renders a box shadow
///
/// Implements CSS box-shadow rendering with support for:
/// - Offset (x, y)
/// - Blur radius (gaussian blur approximation)
/// - Spread radius (expands/contracts shadow shape)
/// - Inset shadows
///
/// # Arguments
///
/// * `pixmap` - The pixmap to draw to
/// * `x` - X coordinate of the box
/// * `y` - Y coordinate of the box
/// * `width` - Width of the box
/// * `height` - Height of the box
/// * `radii` - Corner radii of the box
/// * `shadow` - Shadow parameters
///
/// # Returns
///
/// Returns `true` if shadow was rendered.
///
/// # Implementation Notes
///
/// For performance, this uses a simplified shadow rendering approach:
/// - Blur is approximated by rendering multiple semi-transparent layers
/// - True gaussian blur would require a separate render target and convolution
pub fn render_box_shadow(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  radii: &BorderRadii,
  shadow: &BoxShadow,
) -> Result<bool, RenderError> {
  render_box_shadow_cached(pixmap, x, y, width, height, radii, shadow, None)
}

/// Renders a box shadow using an optional blur cache.
///
/// When `cache` is provided, the gaussian blur step can be reused across identical
/// box shadows, avoiding repeated convolution work.
pub fn render_box_shadow_cached(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  radii: &BorderRadii,
  shadow: &BoxShadow,
  cache: Option<&mut BlurCache>,
) -> Result<bool, RenderError> {
  crate::render_control::check_active(RenderStage::Paint)?;
  if shadow.color.a == 0.0 {
    return Ok(false);
  }

  if shadow.inset {
    render_inset_shadow(pixmap, x, y, width, height, radii, shadow, cache)
  } else {
    render_outset_shadow(pixmap, x, y, width, height, radii, shadow, cache)
  }
}

/// Renders an outset (regular) box shadow
fn render_outset_shadow(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  radii: &BorderRadii,
  shadow: &BoxShadow,
  cache: Option<&mut BlurCache>,
) -> Result<bool, RenderError> {
  crate::render_control::check_active(RenderStage::Paint)?;
  let sigma = shadow.blur_radius.max(0.0);
  let spread = shadow.spread_radius;
  let shadow_rect = Rect::from_xywh(
    x + shadow.offset_x - spread,
    y + shadow.offset_y - spread,
    width + spread * 2.0,
    height + spread * 2.0,
  );
  let radii = BorderRadii {
    top_left: BorderRadius {
      x: (radii.top_left.x + spread).max(0.0),
      y: (radii.top_left.y + spread).max(0.0),
    },
    top_right: BorderRadius {
      x: (radii.top_right.x + spread).max(0.0),
      y: (radii.top_right.y + spread).max(0.0),
    },
    bottom_right: BorderRadius {
      x: (radii.bottom_right.x + spread).max(0.0),
      y: (radii.bottom_right.y + spread).max(0.0),
    },
    bottom_left: BorderRadius {
      x: (radii.bottom_left.x + spread).max(0.0),
      y: (radii.bottom_left.y + spread).max(0.0),
    },
  };

  let blur_pad = (sigma * 3.0).ceil();
  let min_x = (shadow_rect.x() - blur_pad).floor();
  let min_y = (shadow_rect.y() - blur_pad).floor();
  let max_x = (shadow_rect.x() + shadow_rect.width() + blur_pad).ceil();
  let max_y = (shadow_rect.y() + shadow_rect.height() + blur_pad).ceil();

  if max_x <= min_x || max_y <= min_y {
    return Ok(false);
  }

  let canvas_w = (max_x - min_x).max(1.0) as u32;
  let canvas_h = (max_y - min_y).max(1.0) as u32;
  let mut tmp = match new_pixmap(canvas_w, canvas_h) {
    Some(p) => p,
    None => return Ok(false),
  };

  let draw_x = shadow_rect.x() - min_x;
  let draw_y = shadow_rect.y() - min_y;

  let _ = fill_rounded_rect(
    &mut tmp,
    draw_x,
    draw_y,
    shadow_rect.width(),
    shadow_rect.height(),
    &radii,
    shadow.color,
  );
  crate::render_control::check_active(RenderStage::Paint)?;

  if sigma > 0.0 {
    apply_gaussian_blur_cached(&mut tmp, sigma, sigma, cache, 1.0)?;
  }
  crate::render_control::check_active(RenderStage::Paint)?;

  let mut paint = PixmapPaint::default();
  paint.blend_mode = tiny_skia::BlendMode::SourceOver;
  pixmap.draw_pixmap(
    min_x as i32,
    min_y as i32,
    tmp.as_ref(),
    &paint,
    Transform::identity(),
    None,
  );
  Ok(true)
}

/// Renders an inset box shadow
fn render_inset_shadow(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  radii: &BorderRadii,
  shadow: &BoxShadow,
  cache: Option<&mut BlurCache>,
) -> Result<bool, RenderError> {
  crate::render_control::check_active(RenderStage::Paint)?;
  let sigma = shadow.blur_radius.max(0.0);
  let blur_pad = (sigma * 3.0).ceil();
  let spread = shadow.spread_radius;

  let pad_x = (blur_pad + shadow.offset_x.abs() + spread.abs()).ceil();
  let pad_y = (blur_pad + shadow.offset_y.abs() + spread.abs()).ceil();

  let canvas_w = (width + pad_x * 2.0).max(1.0) as u32;
  let canvas_h = (height + pad_y * 2.0).max(1.0) as u32;
  let mut tmp = match new_pixmap(canvas_w, canvas_h) {
    Some(p) => p,
    None => return Ok(false),
  };

  let outer_x = pad_x + shadow.offset_x + spread;
  let outer_y = pad_y + shadow.offset_y + spread;
  let outer_w = (width - 2.0 * spread).max(0.0);
  let outer_h = (height - 2.0 * spread).max(0.0);

  let adjusted_radii = radii.shrink(spread).clamped(outer_w, outer_h);
  let _ = fill_rounded_rect(
    &mut tmp,
    outer_x,
    outer_y,
    outer_w,
    outer_h,
    &adjusted_radii,
    shadow.color,
  );
  crate::render_control::check_active(RenderStage::Paint)?;

  if sigma > 0.0 {
    apply_gaussian_blur_cached(&mut tmp, sigma, sigma, cache, 1.0)?;
  }
  crate::render_control::check_active(RenderStage::Paint)?;

  // Clip to the outer box to keep inset shadows inside
  let width_px = tmp.width() as usize;
  let height_px = tmp.height() as usize;
  let x_start = outer_x.ceil() as i32;
  let x_end = (outer_x + outer_w).floor() as i32;
  let y_start = outer_y.ceil() as i32;
  let y_end = (outer_y + outer_h).floor() as i32;

  let x_start = x_start.clamp(0, width_px as i32);
  let x_end = x_end.clamp(-1, width_px as i32 - 1);
  let y_start = y_start.clamp(0, height_px as i32);
  let y_end = y_end.clamp(-1, height_px as i32 - 1);

  let pixels = tmp.pixels_mut();
  let transparent = PremultipliedColorU8::TRANSPARENT;
  let deadline_row_stride = (DEADLINE_STRIDE / width_px).max(1);
  let mut deadline_counter = 0usize;
  for y_idx in 0..height_px {
    check_active_periodic(&mut deadline_counter, deadline_row_stride, RenderStage::Paint)?;
    let row_base = y_idx * width_px;
    let row = &mut pixels[row_base..row_base + width_px];

    let y_coord = y_idx as i32;
    if y_coord < y_start || y_coord > y_end {
      row.fill(transparent);
      continue;
    }

    if x_start > x_end {
      row.fill(transparent);
      continue;
    }

    if x_start > 0 {
      row[..x_start as usize].fill(transparent);
    }

    let right_start = (x_end + 1) as usize;
    if right_start < width_px {
      row[right_start..].fill(transparent);
    }
  }
  crate::render_control::check_active(RenderStage::Paint)?;

  let mut final_paint = PixmapPaint::default();
  final_paint.blend_mode = tiny_skia::BlendMode::SourceOver;
  pixmap.draw_pixmap(
    (x - pad_x) as i32,
    (y - pad_y) as i32,
    tmp.as_ref(),
    &final_paint,
    Transform::identity(),
    None,
  );
  Ok(true)
}

/// Renders a blurred shadow by drawing multiple layers
#[allow(dead_code)]
fn render_blurred_shadow(
  pixmap: &mut Pixmap,
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  radii: &BorderRadii,
  blur_radius: f32,
  color: Rgba,
) -> bool {
  // Number of layers for blur approximation
  let layers = (blur_radius * 2.0).ceil() as i32;
  let layers = layers.clamp(1, 20);

  let mut rendered = false;

  for i in 0..layers {
    let t = (i as f32) / (layers as f32);
    let expand = blur_radius * (1.0 - t);

    // Calculate layer opacity (gaussian-like falloff)
    let alpha_factor = 1.0 - t * t; // Quadratic falloff
    let layer_alpha = ((color.alpha_u8() as f32) / (layers as f32) * alpha_factor) as u8;

    if layer_alpha == 0 {
      continue;
    }

    let layer_color = Rgba::from_rgba8(color.r, color.g, color.b, layer_alpha);

    // Expand the rectangle for this layer
    let layer_x = x - expand;
    let layer_y = y - expand;
    let layer_width = width + expand * 2.0;
    let layer_height = height + expand * 2.0;

    // Expand radii proportionally
    let layer_radii = BorderRadii {
      top_left: radii.top_left + expand,
      top_right: radii.top_right + expand,
      bottom_right: radii.bottom_right + expand,
      bottom_left: radii.bottom_left + expand,
    };

    rendered |= fill_rounded_rect(
      pixmap,
      layer_x,
      layer_y,
      layer_width,
      layer_height,
      &layer_radii,
      layer_color,
    );
  }

  rendered
}

/// Renders a blurred inset shadow
#[allow(dead_code)]
fn render_blurred_inset_shadow(
  pixmap: &mut Pixmap,
  outer_x: f32,
  outer_y: f32,
  outer_width: f32,
  outer_height: f32,
  outer_radii: &BorderRadii,
  inner_x: f32,
  inner_y: f32,
  inner_width: f32,
  inner_height: f32,
  inner_radii: &BorderRadii,
  blur_radius: f32,
  color: Rgba,
) -> bool {
  // For inset shadow, we draw from inner to outer with decreasing opacity
  let layers = (blur_radius * 2.0).ceil() as i32;
  let layers = layers.clamp(1, 20);

  let mut rendered = false;

  for i in 0..layers {
    let t = (i as f32) / (layers as f32);

    // Interpolate from inner to outer
    let layer_x = inner_x + (outer_x - inner_x) * t;
    let layer_y = inner_y + (outer_y - inner_y) * t;
    let layer_width = inner_width + (outer_width - inner_width) * t;
    let layer_height = inner_height + (outer_height - inner_height) * t;

    // Interpolate radii
    let layer_radii = BorderRadii {
      top_left: inner_radii.top_left + (outer_radii.top_left - inner_radii.top_left) * t,
      top_right: inner_radii.top_right + (outer_radii.top_right - inner_radii.top_right) * t,
      bottom_right: inner_radii.bottom_right
        + (outer_radii.bottom_right - inner_radii.bottom_right) * t,
      bottom_left: inner_radii.bottom_left
        + (outer_radii.bottom_left - inner_radii.bottom_left) * t,
    };

    // Calculate opacity
    let alpha_factor = t * t; // Quadratic increase toward edges
    let layer_alpha = ((color.alpha_u8() as f32) / (layers as f32) * alpha_factor) as u8;

    if layer_alpha == 0 {
      continue;
    }

    let layer_color = Rgba::from_rgba8(color.r, color.g, color.b, layer_alpha);

    // Draw as a frame (outer - inner)
    // Simplified: just draw the layer rectangle
    // A proper implementation would use clipping
    rendered |= fill_rounded_rect(
      pixmap,
      layer_x,
      layer_y,
      layer_width,
      layer_height,
      &layer_radii,
      layer_color,
    );
  }

  rendered
}

// ============================================================================
// Utility Functions
// ============================================================================

/// Renders a simple line
pub fn draw_line(
  pixmap: &mut Pixmap,
  x1: f32,
  y1: f32,
  x2: f32,
  y2: f32,
  color: Rgba,
  width: f32,
) -> bool {
  if color.a == 0.0 || width <= 0.0 {
    return false;
  }

  let mut pb = PathBuilder::new();
  pb.move_to(x1, y1);
  pb.line_to(x2, y2);

  let path = match pb.finish() {
    Some(p) => p,
    None => return false,
  };

  let paint = make_paint(color);
  let stroke = make_stroke(width);

  pixmap.stroke_path(&path, &paint, &stroke, Transform::identity(), None);
  true
}

/// Fills an ellipse
pub fn fill_ellipse(pixmap: &mut Pixmap, cx: f32, cy: f32, rx: f32, ry: f32, color: Rgba) -> bool {
  if color.a == 0.0 || rx <= 0.0 || ry <= 0.0 {
    return false;
  }

  // Approximate ellipse with bezier curves
  let k = 0.552_284_8;
  let kx = k * rx;
  let ky = k * ry;

  let mut pb = PathBuilder::new();

  // Start at top
  pb.move_to(cx, cy - ry);

  // Top-right quadrant
  pb.cubic_to(cx + kx, cy - ry, cx + rx, cy - ky, cx + rx, cy);

  // Bottom-right quadrant
  pb.cubic_to(cx + rx, cy + ky, cx + kx, cy + ry, cx, cy + ry);

  // Bottom-left quadrant
  pb.cubic_to(cx - kx, cy + ry, cx - rx, cy + ky, cx - rx, cy);

  // Top-left quadrant
  pb.cubic_to(cx - rx, cy - ky, cx - kx, cy - ry, cx, cy - ry);

  pb.close();

  let path = match pb.finish() {
    Some(p) => p,
    None => return false,
  };

  let paint = make_paint(color);
  pixmap.fill_path(
    &path,
    &paint,
    FillRule::Winding,
    Transform::identity(),
    None,
  );
  true
}

/// Fills a circle (convenience function)
pub fn fill_circle(pixmap: &mut Pixmap, cx: f32, cy: f32, radius: f32, color: Rgba) -> bool {
  fill_ellipse(pixmap, cx, cy, radius, radius, color)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::paint::painter::{enable_paint_diagnostics, take_paint_diagnostics};
  use crate::render_control::{with_deadline, RenderDeadline};
  use std::time::Duration;

  #[test]
  fn test_border_radii_uniform() {
    let radii = BorderRadii::uniform(10.0);
    assert_eq!(radii.top_left, BorderRadius::uniform(10.0));
    assert_eq!(radii.top_right, BorderRadius::uniform(10.0));
    assert_eq!(radii.bottom_right, BorderRadius::uniform(10.0));
    assert_eq!(radii.bottom_left, BorderRadius::uniform(10.0));
    assert!(radii.has_radius());
  }

  #[test]
  fn test_border_radii_zero() {
    let radii = BorderRadii::zero();
    assert!(!radii.has_radius());
    assert!(radii.is_zero());
  }

  #[test]
  fn test_border_radii_clamped() {
    let radii = BorderRadii::new(
      BorderRadius::uniform(100.0),
      BorderRadius::uniform(100.0),
      BorderRadius::uniform(100.0),
      BorderRadius::uniform(100.0),
    );
    let clamped = radii.clamped(50.0, 50.0);
    // Should be scaled down so sum of adjacent radii doesn't exceed dimension
    assert!(clamped.top_left.x + clamped.top_right.x <= 50.0);
    assert!(clamped.bottom_left.x + clamped.bottom_right.x <= 50.0);
    assert!(clamped.top_left.y + clamped.bottom_left.y <= 50.0);
    assert!(clamped.top_right.y + clamped.bottom_right.y <= 50.0);
  }

  #[test]
  fn test_border_radii_shrink() {
    let radii = BorderRadii::uniform(10.0);
    let shrunk = radii.shrink(3.0);
    assert_eq!(shrunk.top_left, BorderRadius::uniform(7.0));
    assert_eq!(shrunk.top_right, BorderRadius::uniform(7.0));

    let over_shrunk = radii.shrink(15.0);
    assert_eq!(over_shrunk.top_left, BorderRadius::ZERO);
    assert!(over_shrunk.is_zero());
  }

  #[test]
  fn test_border_widths() {
    let widths = BorderWidths::uniform(5.0);
    assert!(widths.has_border());

    let no_border = BorderWidths::new(0.0, 0.0, 0.0, 0.0);
    assert!(!no_border.has_border());
  }

  #[test]
  fn test_fill_rect() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let result = fill_rect(&mut pixmap, 10.0, 10.0, 50.0, 50.0, Rgba::rgb(255, 0, 0));
    assert!(result);
  }

  #[test]
  fn test_fill_rect_transparent() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let result = fill_rect(&mut pixmap, 10.0, 10.0, 50.0, 50.0, Rgba::TRANSPARENT);
    assert!(!result);
  }

  #[test]
  fn test_fill_rect_zero_size() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let result = fill_rect(&mut pixmap, 10.0, 10.0, 0.0, 50.0, Rgba::rgb(255, 0, 0));
    assert!(!result);
  }

  #[test]
  fn test_stroke_rect() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let result = stroke_rect(
      &mut pixmap,
      10.0,
      10.0,
      50.0,
      50.0,
      Rgba::rgb(0, 0, 255),
      2.0,
    );
    assert!(result);
  }

  #[test]
  fn test_fill_rounded_rect() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let radii = BorderRadii::uniform(10.0);
    let result = fill_rounded_rect(
      &mut pixmap,
      10.0,
      10.0,
      50.0,
      50.0,
      &radii,
      Rgba::rgb(0, 255, 0),
    );
    assert!(result);
  }

  #[test]
  fn test_fill_rounded_rect_no_radii() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let radii = BorderRadii::zero();
    let result = fill_rounded_rect(
      &mut pixmap,
      10.0,
      10.0,
      50.0,
      50.0,
      &radii,
      Rgba::rgb(0, 255, 0),
    );
    assert!(result);
  }

  #[test]
  fn test_render_borders() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let widths = BorderWidths::uniform(5.0);
    let colors = BorderColors::uniform(Rgba::rgb(0, 0, 0));
    let radii = BorderRadii::zero();
    let result = render_borders(
      &mut pixmap,
      10.0,
      10.0,
      80.0,
      80.0,
      &widths,
      &colors,
      &radii,
    );
    assert!(result);
  }

  #[test]
  fn test_render_borders_different_colors() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let widths = BorderWidths::uniform(5.0);
    let colors = BorderColors::new(
      Rgba::rgb(255, 0, 0),
      Rgba::rgb(0, 255, 0),
      Rgba::rgb(0, 0, 255),
      Rgba::rgb(255, 255, 0),
    );
    let radii = BorderRadii::zero();
    let result = render_borders(
      &mut pixmap,
      10.0,
      10.0,
      80.0,
      80.0,
      &widths,
      &colors,
      &radii,
    );
    assert!(result);
  }

  #[test]
  fn test_render_rounded_borders() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let widths = BorderWidths::uniform(5.0);
    let colors = BorderColors::uniform(Rgba::rgb(0, 0, 0));
    let radii = BorderRadii::uniform(10.0);
    let result = render_borders(
      &mut pixmap,
      10.0,
      10.0,
      80.0,
      80.0,
      &widths,
      &colors,
      &radii,
    );
    assert!(result);
  }

  #[test]
  fn test_render_box_shadow() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let radii = BorderRadii::zero();
    let shadow = BoxShadow::new(5.0, 5.0, 10.0, 0.0, Rgba::from_rgba8(0, 0, 0, 128));
    let result = render_box_shadow(&mut pixmap, 20.0, 20.0, 50.0, 50.0, &radii, &shadow).unwrap();
    assert!(result);
  }

  #[test]
  fn test_render_box_shadow_cached_uses_blur_cache() {
    enable_paint_diagnostics();
    let mut cache = BlurCache::default();
    let radii = BorderRadii::zero();
    let shadow = BoxShadow::new(0.0, 0.0, 8.0, 0.0, Rgba::from_rgba8(0, 0, 0, 128));
    let mut pixmap = new_pixmap(120, 120).unwrap();
    let result = render_box_shadow_cached(
      &mut pixmap,
      20.0,
      20.0,
      50.0,
      50.0,
      &radii,
      &shadow,
      Some(&mut cache),
    )
    .unwrap();
    assert!(result);

    let mut pixmap2 = new_pixmap(120, 120).unwrap();
    let result2 = render_box_shadow_cached(
      &mut pixmap2,
      20.0,
      20.0,
      50.0,
      50.0,
      &radii,
      &shadow,
      Some(&mut cache),
    )
    .unwrap();
    assert!(result2);

    let stats = take_paint_diagnostics().expect("diagnostics enabled");
    assert!(
      stats.blur_cache_misses > 0,
      "expected at least one miss, got {}",
      stats.blur_cache_misses
    );
    assert!(
      stats.blur_cache_hits > 0,
      "expected hits from second shadow, got {}",
      stats.blur_cache_hits
    );
  }

  #[test]
  fn test_render_inset_shadow() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let radii = BorderRadii::zero();
    let shadow = BoxShadow::inset(5.0, 5.0, 10.0, 5.0, Rgba::from_rgba8(0, 0, 0, 128));
    let result = render_box_shadow(&mut pixmap, 10.0, 10.0, 80.0, 80.0, &radii, &shadow).unwrap();
    assert!(result);
  }

  #[test]
  fn test_render_box_shadow_times_out_with_expired_deadline() {
    let mut pixmap = new_pixmap(2048, 2048).unwrap();
    let radii = BorderRadii::uniform(32.0);
    let shadow = BoxShadow::inset(10.0, 10.0, 64.0, 16.0, Rgba::from_rgba8(0, 0, 0, 200));
    let deadline = RenderDeadline::new(Some(Duration::from_millis(0)), None);
    let width = pixmap.width() as f32;
    let height = pixmap.height() as f32;
    let result = with_deadline(Some(&deadline), || {
      render_box_shadow_cached(
        &mut pixmap,
        0.0,
        0.0,
        width,
        height,
        &radii,
        &shadow,
        None,
      )
    });
    assert!(matches!(
      result,
      Err(RenderError::Timeout {
        stage: RenderStage::Paint,
        ..
      })
    ));
  }

  #[test]
  fn test_draw_line() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let result = draw_line(
      &mut pixmap,
      0.0,
      0.0,
      100.0,
      100.0,
      Rgba::rgb(255, 0, 0),
      2.0,
    );
    assert!(result);
  }

  #[test]
  fn test_fill_circle() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let result = fill_circle(&mut pixmap, 50.0, 50.0, 30.0, Rgba::rgb(0, 128, 255));
    assert!(result);
  }

  #[test]
  fn test_fill_ellipse() {
    let mut pixmap = new_pixmap(100, 100).unwrap();
    let result = fill_ellipse(&mut pixmap, 50.0, 50.0, 40.0, 20.0, Rgba::rgb(128, 0, 255));
    assert!(result);
  }
}
