//! Canvas wrapper for tiny-skia 2D graphics library
//!
//! This module provides a high-level abstraction over tiny-skia for painting
//! display items to pixels. It handles:
//!
//! - Rectangle filling and stroking (with optional rounded corners)
//! - Text/glyph rendering
//! - State management (transforms, clips, opacity)
//! - Color conversion between CSS and tiny-skia formats
//!
//! # Architecture
//!
//! The Canvas wraps a tiny-skia `Pixmap` and maintains a stack of graphics states.
//! Each state includes the current transform, clip region, and opacity. States
//! can be pushed/popped to implement CSS effects like opacity layers.
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::paint::canvas::Canvas;
//! use fastrender::geometry::{Point, Rect, Size};
//! use fastrender::Rgba;
//!
//! // Create a canvas
//! let mut canvas = Canvas::new(800, 600, Rgba::WHITE)?;
//!
//! // Draw a red rectangle
//! let rect = Rect::from_xywh(100.0, 100.0, 200.0, 150.0);
//! canvas.draw_rect(rect, Rgba::rgb(255, 0, 0));
//!
//! // Draw a rounded rectangle
//! canvas.draw_rounded_rect(rect, 10.0, Rgba::rgb(0, 255, 0));
//!
//! // Get the resulting pixels
//! let pixmap = canvas.into_pixmap();
//! ```
//!
//! # CSS Specification References
//!
//! - CSS Backgrounds and Borders Level 3: Background/border painting
//! - CSS Color Level 4: Color handling
//! - CSS 2.1 Appendix E: Paint order

use super::display_list::BlendMode;
use super::display_list::BorderRadii;
#[cfg(test)]
use super::display_list::BorderRadius;
use crate::error::RenderError;
use crate::error::Result;
use crate::geometry::Point;
use crate::geometry::Rect;
use crate::geometry::Size;
use crate::paint::clip_path::ResolvedClipPath;
use crate::paint::text_rasterize::{GlyphCacheStats, TextRasterizer, TextRenderState};
use crate::paint::text_shadow::PathBounds;
use crate::style::color::Rgba;
use crate::text::font_db::LoadedFont;
use crate::text::pipeline::GlyphPosition;
use crate::text::variations::FontVariation;
use tiny_skia::BlendMode as SkiaBlendMode;
use tiny_skia::FillRule;
use tiny_skia::IntSize;
use tiny_skia::Mask;
use tiny_skia::MaskType;
use tiny_skia::Paint;
use tiny_skia::PathBuilder;
use tiny_skia::Pixmap;
use tiny_skia::PixmapPaint;
use tiny_skia::Rect as SkiaRect;
use tiny_skia::Stroke;
use tiny_skia::Transform;

// ============================================================================
// Canvas State
// ============================================================================

/// Graphics state for the canvas
///
/// Represents the current rendering state including transform, opacity, and clip.
/// States can be stacked to implement CSS effects like opacity layers.
#[derive(Debug, Clone)]
struct CanvasState {
  /// Current transform matrix
  transform: Transform,
  /// Current opacity (0.0 to 1.0)
  opacity: f32,
  /// Clip rectangle (if any)
  clip_rect: Option<Rect>,
  /// Clip mask (respects radii/intersections)
  clip_mask: Option<Mask>,
  /// Blend mode
  blend_mode: SkiaBlendMode,
}

impl CanvasState {
  /// Creates a new default state
  fn new() -> Self {
    Self {
      transform: Transform::identity(),
      opacity: 1.0,
      clip_rect: None,
      clip_mask: None,
      blend_mode: SkiaBlendMode::SourceOver,
    }
  }

  /// Creates a paint with the current state applied
  fn create_paint(&self, color: Rgba) -> Paint<'static> {
    self.create_paint_with_blend(color, self.blend_mode)
  }

  /// Creates a paint with an explicit blend mode override
  fn create_paint_with_blend(&self, color: Rgba, blend_mode: SkiaBlendMode) -> Paint<'static> {
    let mut paint = Paint::default();
    // Apply opacity to alpha (color.a is already 0.0-1.0)
    let alpha = color.a * self.opacity;
    paint.set_color_rgba8(color.r, color.g, color.b, (alpha * 255.0) as u8);
    paint.anti_alias = true;
    paint.blend_mode = blend_mode;
    paint
  }
}

impl Default for CanvasState {
  fn default() -> Self {
    Self::new()
  }
}

#[derive(Debug)]
struct LayerRecord {
  pixmap: Pixmap,
  state_stack: Vec<CanvasState>,
  current_state: CanvasState,
  opacity: f32,
  composite_blend: Option<SkiaBlendMode>,
  origin: (i32, i32),
}

// ============================================================================
// Canvas
// ============================================================================

/// Canvas for 2D graphics rendering using tiny-skia
///
/// Provides a high-level API for drawing primitives (rectangles, text, etc.)
/// to a pixel buffer. Maintains a stack of graphics states for implementing
/// CSS effects like opacity layers and transforms.
///
/// # Thread Safety
///
/// Canvas is not thread-safe. Create separate Canvas instances for each thread
/// if parallel rendering is needed.
///
/// # Memory Usage
///
/// The canvas allocates memory for the pixel buffer (width × height × 4 bytes)
/// plus state stack overhead.
pub struct Canvas {
  /// The underlying pixel buffer
  pixmap: Pixmap,
  /// Stack of graphics states
  state_stack: Vec<CanvasState>,
  /// Stack of offscreen layers for grouped effects
  layer_stack: Vec<LayerRecord>,
  /// Current graphics state
  current_state: CanvasState,
  /// Cached text rasterizer
  text_rasterizer: TextRasterizer,
}

impl Canvas {
  /// Creates a new canvas with the given dimensions and background color
  ///
  /// # Arguments
  ///
  /// * `width` - Canvas width in pixels
  /// * `height` - Canvas height in pixels
  /// * `background` - Background fill color
  ///
  /// # Returns
  ///
  /// Returns a new Canvas or an error if the dimensions are invalid.
  ///
  /// # Errors
  ///
  /// Returns `RenderError::InvalidParameters` if:
  /// - Width or height is zero
  /// - Width × height would overflow
  /// - Allocation fails
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// use fastrender::paint::canvas::Canvas;
  /// use fastrender::Rgba;
  ///
  /// let canvas = Canvas::new(800, 600, Rgba::WHITE)?;
  /// assert_eq!(canvas.width(), 800);
  /// assert_eq!(canvas.height(), 600);
  /// ```
  pub fn new(width: u32, height: u32, background: Rgba) -> Result<Self> {
    let pixmap = Pixmap::new(width, height).ok_or_else(|| RenderError::InvalidParameters {
      message: format!("Failed to create canvas {}x{}", width, height),
    })?;

    let mut canvas = Self {
      pixmap,
      state_stack: Vec::new(),
      layer_stack: Vec::new(),
      current_state: CanvasState::new(),
      text_rasterizer: TextRasterizer::new(),
    };

    // Fill with background color
    canvas.clear(background);

    Ok(canvas)
  }

  /// Creates a new canvas with transparent background
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// let canvas = Canvas::new_transparent(400, 300)?;
  /// ```
  pub fn new_transparent(width: u32, height: u32) -> Result<Self> {
    Self::new(width, height, Rgba::TRANSPARENT)
  }

  /// Wraps an existing pixmap in a Canvas without clearing it.
  pub fn from_pixmap(pixmap: Pixmap) -> Self {
    Self {
      pixmap,
      state_stack: Vec::new(),
      layer_stack: Vec::new(),
      current_state: CanvasState::new(),
      text_rasterizer: TextRasterizer::new(),
    }
  }

  /// Returns the canvas width in pixels
  #[inline]
  pub fn width(&self) -> u32 {
    self.pixmap.width()
  }

  /// Returns the canvas height in pixels
  #[inline]
  pub fn height(&self) -> u32 {
    self.pixmap.height()
  }

  /// Returns the canvas size
  #[inline]
  pub fn size(&self) -> Size {
    Size::new(self.width() as f32, self.height() as f32)
  }

  /// Returns the canvas bounds as a rectangle
  #[inline]
  pub fn bounds(&self) -> Rect {
    Rect::from_xywh(0.0, 0.0, self.width() as f32, self.height() as f32)
  }

  /// Clears the canvas with the given color
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// canvas.clear(Rgba::WHITE);
  /// ```
  pub fn clear(&mut self, color: Rgba) {
    let skia_color = tiny_skia::Color::from_rgba8(color.r, color.g, color.b, color.alpha_u8());
    self.pixmap.fill(skia_color);
  }

  /// Consumes the canvas and returns the underlying pixmap
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// let pixmap = canvas.into_pixmap();
  /// pixmap.save_png("output.png")?;
  /// ```
  pub fn into_pixmap(self) -> Pixmap {
    self.pixmap
  }

  /// Returns a reference to the underlying pixmap
  #[inline]
  pub fn pixmap(&self) -> &Pixmap {
    &self.pixmap
  }

  /// Returns a mutable reference to the underlying pixmap
  #[inline]
  pub fn pixmap_mut(&mut self) -> &mut Pixmap {
    &mut self.pixmap
  }

  /// Returns glyph cache statistics for text rendering.
  pub fn text_cache_stats(&self) -> GlyphCacheStats {
    self.text_rasterizer.cache_stats()
  }

  /// Resets glyph cache stats without clearing cached outlines.
  pub fn reset_text_cache_stats(&mut self) {
    self.text_rasterizer.reset_cache_stats();
  }

  /// Returns a mutable reference to the pixmap that will receive composited output.
  ///
  /// When painting inside an offscreen layer, this refers to the parent layer's pixmap
  /// that already contains the backdrop content.
  pub(crate) fn backdrop_pixmap_mut(&mut self) -> &mut Pixmap {
    self
      .layer_stack
      .last_mut()
      .map(|layer| &mut layer.pixmap)
      .unwrap_or(&mut self.pixmap)
  }

  // ========================================================================
  // State Management
  // ========================================================================

  /// Saves the current graphics state to the stack
  ///
  /// The saved state can be restored later with `restore()`.
  /// Use this to implement CSS effects like opacity layers.
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// canvas.save();
  /// canvas.set_opacity(0.5);
  /// canvas.draw_rect(rect, Rgba::RED);
  /// canvas.restore(); // Opacity is back to 1.0
  /// ```
  pub fn save(&mut self) {
    self.state_stack.push(self.current_state.clone());
  }

  /// Restores the previously saved graphics state
  ///
  /// Pops the most recently saved state from the stack.
  /// Does nothing if the stack is empty.
  pub fn restore(&mut self) {
    if let Some(state) = self.state_stack.pop() {
      self.current_state = state;
    }
  }

  /// Returns the current state stack depth
  #[inline]
  pub fn state_depth(&self) -> usize {
    self.state_stack.len()
  }

  /// Sets the current opacity (0.0 to 1.0)
  ///
  /// Opacity is multiplied with color alpha when drawing.
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// canvas.set_opacity(0.5); // 50% opacity
  /// canvas.draw_rect(rect, Rgba::RED); // Draws at 50% opacity
  /// ```
  pub fn set_opacity(&mut self, opacity: f32) {
    self.current_state.opacity = opacity.clamp(0.0, 1.0);
  }

  /// Returns the current opacity
  #[inline]
  pub fn opacity(&self) -> f32 {
    self.current_state.opacity
  }

  /// Pushes a new offscreen layer for grouped compositing (e.g., opacity).
  pub fn push_layer(&mut self, opacity: f32) -> Result<()> {
    self.push_layer_with_blend(opacity, None)
  }

  /// Pushes a new offscreen layer with explicit bounds.
  ///
  /// The layer pixmap will be clipped to the provided bounds (after clamping to the canvas)
  /// and drawing inside the layer will be translated so global coordinates continue to work.
  pub fn push_layer_bounded(
    &mut self,
    opacity: f32,
    blend: Option<SkiaBlendMode>,
    bounds: Rect,
  ) -> Result<()> {
    let (origin_x, origin_y, width, height) = match self.layer_bounds(bounds) {
      Some(b) => b,
      None => (0, 0, self.pixmap.width(), self.pixmap.height()),
    };
    self.push_layer_internal(opacity, blend, origin_x, origin_y, width, height)
  }

  /// Pushes a new offscreen layer with an explicit composite blend mode.
  pub fn push_layer_with_blend(
    &mut self,
    opacity: f32,
    blend: Option<SkiaBlendMode>,
  ) -> Result<()> {
    self.push_layer_internal(
      opacity,
      blend,
      0,
      0,
      self.pixmap.width(),
      self.pixmap.height(),
    )
  }

  fn push_layer_internal(
    &mut self,
    opacity: f32,
    blend: Option<SkiaBlendMode>,
    origin_x: i32,
    origin_y: i32,
    width: u32,
    height: u32,
  ) -> Result<()> {
    let parent_width = self.pixmap.width();
    let parent_height = self.pixmap.height();
    let width = width.max(1);
    let height = height.max(1);

    let new_pixmap = Pixmap::new(width, height).ok_or_else(|| RenderError::InvalidParameters {
      message: "Failed to create layer pixmap".into(),
    })?;

    let record = LayerRecord {
      pixmap: std::mem::replace(&mut self.pixmap, new_pixmap),
      state_stack: self.state_stack.clone(),
      current_state: self.current_state.clone(),
      opacity: opacity.clamp(0.0, 1.0),
      composite_blend: blend,
      origin: (origin_x, origin_y),
    };
    self.layer_stack.push(record);
    // Painting inside the layer should start from a neutral state.
    self.current_state.opacity = 1.0;
    self.current_state.blend_mode = SkiaBlendMode::SourceOver;

    if origin_x != 0 || origin_y != 0 || width != parent_width || height != parent_height {
      let layer_rect = Rect::from_xywh(
        origin_x as f32,
        origin_y as f32,
        width as f32,
        height as f32,
      );
      self.current_state.transform = self
        .current_state
        .transform
        .pre_translate(-(origin_x as f32), -(origin_y as f32));
      if let Some(clip_rect) = self.current_state.clip_rect.take() {
        let intersected = clip_rect.intersection(layer_rect).unwrap_or(Rect::ZERO);
        self.current_state.clip_rect = if intersected.width() <= 0.0 || intersected.height() <= 0.0
        {
          Some(Rect::ZERO)
        } else {
          Some(Rect::from_xywh(
            intersected.x() - origin_x as f32,
            intersected.y() - origin_y as f32,
            intersected.width(),
            intersected.height(),
          ))
        };
      }
      if let Some(mask) = self.current_state.clip_mask.take() {
        self.current_state.clip_mask =
          crop_mask(&mask, origin_x as u32, origin_y as u32, width, height);
      }
    }

    Ok(())
  }

  fn layer_bounds(&self, bounds: Rect) -> Option<(i32, i32, u32, u32)> {
    if !bounds.x().is_finite()
      || !bounds.y().is_finite()
      || !bounds.width().is_finite()
      || !bounds.height().is_finite()
    {
      return None;
    }

    let x0 = bounds.min_x().floor() as i32;
    let y0 = bounds.min_y().floor() as i32;
    let x1 = bounds.max_x().ceil() as i32;
    let y1 = bounds.max_y().ceil() as i32;

    let canvas_w = self.pixmap.width() as i32;
    let canvas_h = self.pixmap.height() as i32;
    let clamped_x0 = x0.clamp(0, canvas_w);
    let clamped_y0 = y0.clamp(0, canvas_h);
    let clamped_x1 = x1.clamp(0, canvas_w);
    let clamped_y1 = y1.clamp(0, canvas_h);
    let width = clamped_x1.saturating_sub(clamped_x0) as u32;
    let height = clamped_y1.saturating_sub(clamped_y0) as u32;
    if width == 0 || height == 0 {
      return None;
    }
    Some((clamped_x0, clamped_y0, width, height))
  }

  /// Pops the most recent offscreen layer without compositing it.
  ///
  /// Returns the layer pixmap, the effective opacity (including parent opacity),
  /// and any explicit composite blend mode that was requested.
  pub fn pop_layer_raw(&mut self) -> Result<(Pixmap, (i32, i32), f32, Option<SkiaBlendMode>)> {
    let Some(record) = self.layer_stack.pop() else {
      return Err(
        RenderError::InvalidParameters {
          message: "pop_layer without matching push".into(),
        }
        .into(),
      );
    };

    let layer_pixmap = std::mem::replace(&mut self.pixmap, record.pixmap);
    self.state_stack = record.state_stack;
    self.current_state = record.current_state;
    let opacity = (record.opacity * self.current_state.opacity).clamp(0.0, 1.0);
    Ok((layer_pixmap, record.origin, opacity, record.composite_blend))
  }

  /// Pops the most recent offscreen layer and composites it into the parent.
  pub fn pop_layer(&mut self) -> Result<()> {
    let (layer_pixmap, origin, opacity, composite_blend) = self.pop_layer_raw()?;

    self.composite_layer(&layer_pixmap, opacity, composite_blend, origin);
    Ok(())
  }

  pub(crate) fn composite_layer(
    &mut self,
    layer: &Pixmap,
    opacity: f32,
    composite_blend: Option<SkiaBlendMode>,
    origin: (i32, i32),
  ) {
    let mut paint = PixmapPaint::default();
    paint.opacity = opacity;
    paint.blend_mode = composite_blend.unwrap_or(self.current_state.blend_mode);
    let clip = self.current_state.clip_mask.clone();
    let transform = self.current_state.transform;

    self.pixmap.draw_pixmap(
      origin.0,
      origin.1,
      layer.as_ref(),
      &paint,
      transform,
      clip.as_ref(),
    );
  }

  /// Returns the current blend mode.
  #[inline]
  pub(crate) fn blend_mode(&self) -> SkiaBlendMode {
    self.current_state.blend_mode
  }

  /// Sets the current transform
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// canvas.set_transform(Transform::from_translate(100.0, 50.0));
  /// ```
  pub fn set_transform(&mut self, transform: Transform) {
    self.current_state.transform = transform;
  }

  /// Returns the current transform
  #[inline]
  pub fn transform(&self) -> Transform {
    self.current_state.transform
  }

  /// Applies a translation to the current transform
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// canvas.translate(100.0, 50.0);
  /// canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 50.0, 50.0), Rgba::RED);
  /// // Rectangle is drawn at (100, 50)
  /// ```
  pub fn translate(&mut self, dx: f32, dy: f32) {
    self.current_state.transform = self.current_state.transform.pre_translate(dx, dy);
  }

  /// Applies a scale to the current transform
  pub fn scale(&mut self, sx: f32, sy: f32) {
    self.current_state.transform = self.current_state.transform.pre_scale(sx, sy);
  }

  /// Sets the blend mode for subsequent drawing operations
  pub fn set_blend_mode(&mut self, mode: BlendMode) {
    self.current_state.blend_mode = mode.to_skia();
  }

  /// Sets a clip rectangle
  ///
  /// Subsequent drawing operations will be clipped to this rectangle.
  pub fn set_clip(&mut self, rect: Rect) {
    self.set_clip_with_radii(rect, None);
  }

  /// Sets a clip rectangle with optional corner radii.
  pub fn set_clip_with_radii(&mut self, rect: Rect, radii: Option<BorderRadii>) {
    let transform = self.current_state.transform;
    let clip_bounds = if transform == Transform::identity() {
      rect
    } else {
      Self::transform_rect_aabb(rect, transform)
    };

    let base_clip = match self.current_state.clip_rect {
      Some(existing) => existing.intersection(clip_bounds).unwrap_or(Rect::ZERO),
      None => clip_bounds,
    };
    self.current_state.clip_rect = Some(base_clip);

    let new_mask = self.build_clip_mask(rect, radii.unwrap_or(BorderRadii::ZERO));
    self.current_state.clip_mask = match (new_mask, self.current_state.clip_mask.take()) {
      (Some(mut next), Some(existing)) => {
        combine_masks(&mut next, &existing);
        Some(next)
      }
      (Some(mask), None) => Some(mask),
      (None, existing) => existing,
    };
  }

  /// Sets an arbitrary clip path (basic shapes)
  pub fn set_clip_path(&mut self, path: &ResolvedClipPath, scale: f32) {
    let bounds = path.bounds();
    let scaled_bounds = Rect::from_xywh(
      bounds.x() * scale,
      bounds.y() * scale,
      bounds.width() * scale,
      bounds.height() * scale,
    );
    let base_clip = match self.current_state.clip_rect {
      Some(existing) => existing.intersection(scaled_bounds).unwrap_or(Rect::ZERO),
      None => scaled_bounds,
    };
    self.current_state.clip_rect = Some(base_clip);

    let new_mask = IntSize::from_wh(self.pixmap.width(), self.pixmap.height())
      .and_then(|size| path.mask(scale, size, self.current_state.transform));
    self.current_state.clip_mask = match (new_mask, self.current_state.clip_mask.take()) {
      (Some(mut next), Some(existing)) => {
        combine_masks(&mut next, &existing);
        Some(next)
      }
      (Some(mask), None) => Some(mask),
      (None, existing) => existing,
    };
  }

  /// Clears the clip rectangle
  pub fn clear_clip(&mut self) {
    self.current_state.clip_rect = None;
    self.current_state.clip_mask = None;
  }

  /// Returns the current clip bounds if any.
  pub(crate) fn clip_bounds(&self) -> Option<Rect> {
    self.current_state.clip_rect
  }

  /// Returns the current clip mask, including any rounded radii.
  pub(crate) fn clip_mask(&self) -> Option<&Mask> {
    self.current_state.clip_mask.as_ref()
  }

  // ========================================================================
  // Drawing Operations
  // ========================================================================

  /// Draws a filled rectangle
  ///
  /// # Arguments
  ///
  /// * `rect` - Rectangle to fill
  /// * `color` - Fill color
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// let rect = Rect::from_xywh(10.0, 10.0, 100.0, 50.0);
  /// canvas.draw_rect(rect, Rgba::rgb(255, 0, 0));
  /// ```
  pub fn draw_rect(&mut self, rect: Rect, color: Rgba) {
    // Skip fully transparent colors
    if color.a == 0.0 && self.current_state.opacity == 0.0 {
      return;
    }

    // Apply clip
    let rect = match self.apply_clip(rect) {
      Some(r) => r,
      None => return, // Fully clipped
    };

    if let Some(skia_rect) = self.to_skia_rect(rect) {
      let path = PathBuilder::from_rect(skia_rect);
      let paint = self.current_state.create_paint(color);
      self.pixmap.fill_path(
        &path,
        &paint,
        FillRule::Winding,
        self.current_state.transform,
        self.current_state.clip_mask.as_ref(),
      );
    }
  }

  /// Draws a stroked rectangle outline
  ///
  /// # Arguments
  ///
  /// * `rect` - Rectangle to stroke
  /// * `color` - Stroke color
  /// * `width` - Stroke width in pixels
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// canvas.stroke_rect(rect, Rgba::BLACK, 2.0);
  /// ```
  pub fn stroke_rect(&mut self, rect: Rect, color: Rgba, width: f32) {
    if color.a == 0.0 && self.current_state.opacity == 0.0 {
      return;
    }

    if let Some(skia_rect) = self.to_skia_rect(rect) {
      let path = PathBuilder::from_rect(skia_rect);
      let paint = self.current_state.create_paint(color);
      let stroke = Stroke {
        width,
        ..Default::default()
      };
      self.pixmap.stroke_path(
        &path,
        &paint,
        &stroke,
        self.current_state.transform,
        self.current_state.clip_mask.as_ref(),
      );
    }
  }

  /// Draws a stroked rectangle outline using an explicit blend mode override.
  pub fn stroke_rect_with_blend(
    &mut self,
    rect: Rect,
    color: Rgba,
    width: f32,
    blend_mode: BlendMode,
  ) {
    if color.a == 0.0 && self.current_state.opacity == 0.0 {
      return;
    }

    if let Some(skia_rect) = self.to_skia_rect(rect) {
      let path = PathBuilder::from_rect(skia_rect);
      let paint = self
        .current_state
        .create_paint_with_blend(color, blend_mode.to_skia());
      let stroke = Stroke {
        width,
        ..Default::default()
      };
      self.pixmap.stroke_path(
        &path,
        &paint,
        &stroke,
        self.current_state.transform,
        self.current_state.clip_mask.as_ref(),
      );
    }
  }

  /// Draws a filled rounded rectangle
  ///
  /// # Arguments
  ///
  /// * `rect` - Rectangle bounds
  /// * `radii` - Corner radii
  /// * `color` - Fill color
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// let radii = BorderRadii::uniform(10.0);
  /// canvas.draw_rounded_rect(rect, radii, Rgba::BLUE);
  /// ```
  pub fn draw_rounded_rect(&mut self, rect: Rect, radii: BorderRadii, color: Rgba) {
    if color.a == 0.0 && self.current_state.opacity == 0.0 {
      return;
    }

    // If no radius, use simple rect
    if !radii.has_radius() {
      return self.draw_rect(rect, color);
    }

    if let Some(clip) = self.current_state.clip_rect {
      if clip.width() <= 0.0 || clip.height() <= 0.0 {
        return;
      }

      let intersects = if self.current_state.transform == Transform::identity() {
        rect.intersection(clip).is_some()
      } else {
        Self::transform_rect_aabb(rect, self.current_state.transform)
          .intersection(clip)
          .is_some()
      };

      if !intersects {
        return;
      }
    }

    if let Some(path) = self.build_rounded_rect_path(rect, radii) {
      let paint = self.current_state.create_paint(color);
      self.pixmap.fill_path(
        &path,
        &paint,
        FillRule::Winding,
        self.current_state.transform,
        self.current_state.clip_mask.as_ref(),
      );
    }
  }

  /// Draws a stroked rounded rectangle outline
  pub fn stroke_rounded_rect(&mut self, rect: Rect, radii: BorderRadii, color: Rgba, width: f32) {
    if color.a == 0.0 && self.current_state.opacity == 0.0 {
      return;
    }

    if !radii.has_radius() {
      return self.stroke_rect(rect, color, width);
    }

    if let Some(clip) = self.current_state.clip_rect {
      if clip.width() <= 0.0 || clip.height() <= 0.0 {
        return;
      }

      let intersects = if self.current_state.transform == Transform::identity() {
        rect.intersection(clip).is_some()
      } else {
        Self::transform_rect_aabb(rect, self.current_state.transform)
          .intersection(clip)
          .is_some()
      };

      if !intersects {
        return;
      }
    }

    if let Some(path) = self.build_rounded_rect_path(rect, radii) {
      let paint = self.current_state.create_paint(color);
      let stroke = Stroke {
        width,
        ..Default::default()
      };
      self.pixmap.stroke_path(
        &path,
        &paint,
        &stroke,
        self.current_state.transform,
        self.current_state.clip_mask.as_ref(),
      );
    }
  }

  pub(crate) fn glyph_paths(
    &mut self,
    position: Point,
    glyphs: &[GlyphPosition],
    font: &LoadedFont,
    font_size: f32,
    synthetic_oblique: f32,
    rotation: Option<Transform>,
  ) -> Result<(Vec<tiny_skia::Path>, PathBounds)> {
    let paths = self.text_rasterizer.positioned_glyph_paths(
      glyphs,
      font,
      font_size,
      position.x,
      position.y,
      synthetic_oblique,
      rotation,
    )?;
    let mut bounds = PathBounds::new();
    for path in &paths {
      bounds.include(&path.bounds());
    }
    Ok((paths, bounds))
  }

  /// Draws text glyphs at the specified position
  ///
  /// Renders shaped glyphs from the text shaping pipeline. Each glyph is
  /// drawn using its outline from the font.
  ///
  /// # Arguments
  ///
  /// * `position` - Baseline origin for the text
  /// * `glyphs` - Positioned glyphs from text shaping
  /// * `font` - Font containing glyph outlines
  /// * `font_size` - Font size in pixels
  /// * `color` - Text color
  /// * `palette_index` - CPAL palette selection for color fonts
  /// * `variations` - Font variation coordinates applied when shaping
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// let pipeline = ShapingPipeline::new();
  /// let style = ComputedStyle::default();
  /// let runs = pipeline.shape("Hello", &style, &font_context)?;
  /// let run = &runs[0];
  /// canvas.draw_text(
  ///   Point::new(10.0, 50.0),
  ///   &run.glyphs,
  ///   &run.font,
  ///   run.font_size,
  ///   Rgba::BLACK,
  ///   run.synthetic_bold,
  ///   run.synthetic_oblique,
  ///   run.palette_index,
  ///   &run.variations,
  /// );
  /// ```
  pub fn draw_text(
    &mut self,
    position: Point,
    glyphs: &[GlyphPosition],
    font: &LoadedFont,
    font_size: f32,
    color: Rgba,
    synthetic_bold: f32,
    synthetic_oblique: f32,
    palette_index: u16,
    variations: &[FontVariation],
  ) {
    if glyphs.is_empty() || (color.a == 0.0 && self.current_state.opacity == 0.0) {
      return;
    }

    let state = TextRenderState {
      transform: self.current_state.transform,
      clip_mask: self.current_state.clip_mask.as_ref(),
      opacity: self.current_state.opacity,
      blend_mode: self.current_state.blend_mode,
    };

    let _ = self.text_rasterizer.render_glyphs_with_state(
      glyphs,
      font,
      font_size,
      position.x,
      position.y,
      color,
      synthetic_bold,
      synthetic_oblique,
      palette_index,
      variations,
      None,
      state,
      &mut self.pixmap,
    );
  }

  /// Draws a line between two points
  ///
  /// # Arguments
  ///
  /// * `start` - Starting point
  /// * `end` - Ending point
  /// * `color` - Line color
  /// * `width` - Line width in pixels
  pub fn draw_line(&mut self, start: Point, end: Point, color: Rgba, width: f32) {
    if color.a == 0.0 && self.current_state.opacity == 0.0 {
      return;
    }

    let mut pb = PathBuilder::new();
    pb.move_to(start.x, start.y);
    pb.line_to(end.x, end.y);

    if let Some(path) = pb.finish() {
      let paint = self.current_state.create_paint(color);
      let stroke = Stroke {
        width,
        ..Default::default()
      };
      self.pixmap.stroke_path(
        &path,
        &paint,
        &stroke,
        self.current_state.transform,
        self.current_state.clip_mask.as_ref(),
      );
    }
  }

  /// Draws a filled circle
  ///
  /// # Arguments
  ///
  /// * `center` - Center point of the circle
  /// * `radius` - Circle radius in pixels
  /// * `color` - Fill color
  pub fn draw_circle(&mut self, center: Point, radius: f32, color: Rgba) {
    if color.a == 0.0 || radius <= 0.0 {
      return;
    }

    if let Some(path) = self.build_circle_path(center, radius) {
      let paint = self.current_state.create_paint(color);
      self.pixmap.fill_path(
        &path,
        &paint,
        FillRule::Winding,
        self.current_state.transform,
        self.current_state.clip_mask.as_ref(),
      );
    }
  }

  /// Strokes a circle outline
  pub fn stroke_circle(&mut self, center: Point, radius: f32, color: Rgba, width: f32) {
    if color.a == 0.0 || radius <= 0.0 {
      return;
    }

    if let Some(path) = self.build_circle_path(center, radius) {
      let paint = self.current_state.create_paint(color);
      let stroke = Stroke {
        width,
        ..Default::default()
      };
      self.pixmap.stroke_path(
        &path,
        &paint,
        &stroke,
        self.current_state.transform,
        self.current_state.clip_mask.as_ref(),
      );
    }
  }

  // ========================================================================
  // Path Building Helpers
  // ========================================================================

  /// Converts a geometry Rect to tiny-skia Rect
  fn to_skia_rect(&self, rect: Rect) -> Option<SkiaRect> {
    SkiaRect::from_xywh(rect.x(), rect.y(), rect.width(), rect.height())
  }

  /// Applies the current clip to a rectangle
  /// Applies the current clip to a rectangle.
  pub(crate) fn apply_clip(&self, rect: Rect) -> Option<Rect> {
    if self.current_state.clip_mask.is_some() && !self.current_state.transform.is_identity() {
      return Some(rect);
    }
    if let Some(clip) = self.current_state.clip_rect {
      if clip.width() <= 0.0 || clip.height() <= 0.0 {
        return None;
      }

      if self.current_state.transform == Transform::identity() {
        rect.intersection(clip)
      } else {
        let transformed_rect = Self::transform_rect_aabb(rect, self.current_state.transform);
        if transformed_rect.intersection(clip).is_some() {
          Some(rect)
        } else {
          None
        }
      }
    } else {
      Some(rect)
    }
  }

  #[inline]
  fn transform_point(transform: Transform, point: Point) -> Point {
    Point::new(
      point.x * transform.sx + point.y * transform.kx + transform.tx,
      point.x * transform.ky + point.y * transform.sy + transform.ty,
    )
  }

  #[inline]
  fn transform_rect_aabb(rect: Rect, transform: Transform) -> Rect {
    let p1 = Self::transform_point(transform, rect.origin);
    let p2 = Self::transform_point(transform, Point::new(rect.max_x(), rect.min_y()));
    let p3 = Self::transform_point(transform, Point::new(rect.min_x(), rect.max_y()));
    let p4 = Self::transform_point(transform, Point::new(rect.max_x(), rect.max_y()));

    let min_x = p1.x.min(p2.x).min(p3.x).min(p4.x);
    let max_x = p1.x.max(p2.x).max(p3.x).max(p4.x);
    let min_y = p1.y.min(p2.y).min(p3.y).min(p4.y);
    let max_y = p1.y.max(p2.y).max(p3.y).max(p4.y);

    Rect::from_xywh(min_x, min_y, max_x - min_x, max_y - min_y)
  }

  fn build_clip_mask(&self, rect: Rect, radii: BorderRadii) -> Option<Mask> {
    if rect.width() <= 0.0 || rect.height() <= 0.0 || self.width() == 0 || self.height() == 0 {
      return None;
    }

    let mut mask_pixmap = Pixmap::new(self.width(), self.height())?;
    let paint = {
      let mut p = Paint::default();
      p.set_color_rgba8(255, 255, 255, 255);
      p
    };

    let path = self.build_rounded_rect_path(rect, radii)?;
    mask_pixmap.fill_path(
      &path,
      &paint,
      FillRule::Winding,
      self.current_state.transform,
      None,
    );
    Some(Mask::from_pixmap(mask_pixmap.as_ref(), MaskType::Alpha))
  }

  /// Builds a path for a rounded rectangle
  fn build_rounded_rect_path(&self, rect: Rect, radii: BorderRadii) -> Option<tiny_skia::Path> {
    crate::paint::rasterize::build_rounded_rect_path(
      rect.x(),
      rect.y(),
      rect.width(),
      rect.height(),
      &radii,
    )
  }

  /// Builds a path for a circle using cubic bezier approximation
  fn build_circle_path(&self, center: Point, radius: f32) -> Option<tiny_skia::Path> {
    // Use the cubic bezier approximation for a circle
    // Magic number for circle approximation: 4/3 * tan(π/8) ≈ 0.5522847498
    const KAPPA: f32 = 0.552_284_8;
    let k = radius * KAPPA;

    let mut pb = PathBuilder::new();

    // Start at top
    pb.move_to(center.x, center.y - radius);

    // Top-right quadrant
    pb.cubic_to(
      center.x + k,
      center.y - radius,
      center.x + radius,
      center.y - k,
      center.x + radius,
      center.y,
    );

    // Bottom-right quadrant
    pb.cubic_to(
      center.x + radius,
      center.y + k,
      center.x + k,
      center.y + radius,
      center.x,
      center.y + radius,
    );

    // Bottom-left quadrant
    pb.cubic_to(
      center.x - k,
      center.y + radius,
      center.x - radius,
      center.y + k,
      center.x - radius,
      center.y,
    );

    // Top-left quadrant
    pb.cubic_to(
      center.x - radius,
      center.y - k,
      center.x - k,
      center.y - radius,
      center.x,
      center.y - radius,
    );

    pb.close();
    pb.finish()
  }
}

fn combine_masks(into: &mut Mask, existing: &Mask) {
  if into.width() != existing.width() || into.height() != existing.height() {
    return;
  }

  for (dst, src) in into.data_mut().iter_mut().zip(existing.data().iter()) {
    let multiplied = (*dst as u16 * *src as u16 + 127) / 255;
    *dst = multiplied as u8;
  }
}

pub(crate) fn crop_mask(
  mask: &Mask,
  origin_x: u32,
  origin_y: u32,
  width: u32,
  height: u32,
) -> Option<Mask> {
  if width == 0 || height == 0 {
    return None;
  }

  let mask_width = mask.width();
  let mask_height = mask.height();
  if origin_x >= mask_width || origin_y >= mask_height {
    return None;
  }

  let crop_w = width.min(mask_width.saturating_sub(origin_x));
  let crop_h = height.min(mask_height.saturating_sub(origin_y));
  if crop_w == 0 || crop_h == 0 {
    return None;
  }

  let mut pixmap = Pixmap::new(crop_w, crop_h)?;
  let dst = pixmap.data_mut();
  let src = mask.data();
  let src_stride = mask_width as usize;
  let dst_stride = crop_w as usize * 4;
  for row in 0..crop_h as usize {
    let src_idx = (origin_y as usize + row) * src_stride + origin_x as usize;
    let dst_idx = row * dst_stride;
    for col in 0..crop_w as usize {
      let alpha = src[src_idx + col];
      let base = dst_idx + col * 4;
      dst[base] = 0;
      dst[base + 1] = 0;
      dst[base + 2] = 0;
      dst[base + 3] = alpha;
    }
  }

  Some(Mask::from_pixmap(pixmap.as_ref(), MaskType::Alpha))
}

// ============================================================================
// Blend Mode Conversion
// ============================================================================

/// Extension trait for converting BlendMode to tiny-skia
trait BlendModeExt {
  fn to_skia(self) -> SkiaBlendMode;
}

impl BlendModeExt for BlendMode {
  /// Converts to tiny-skia BlendMode
  fn to_skia(self) -> SkiaBlendMode {
    match self {
      BlendMode::Normal => SkiaBlendMode::SourceOver,
      BlendMode::Multiply => SkiaBlendMode::Multiply,
      BlendMode::Screen => SkiaBlendMode::Screen,
      BlendMode::Overlay => SkiaBlendMode::Overlay,
      BlendMode::Darken => SkiaBlendMode::Darken,
      BlendMode::Lighten => SkiaBlendMode::Lighten,
      BlendMode::ColorDodge => SkiaBlendMode::ColorDodge,
      BlendMode::ColorBurn => SkiaBlendMode::ColorBurn,
      BlendMode::HardLight => SkiaBlendMode::HardLight,
      BlendMode::SoftLight => SkiaBlendMode::SoftLight,
      BlendMode::Difference => SkiaBlendMode::Difference,
      BlendMode::Exclusion => SkiaBlendMode::Exclusion,
      BlendMode::Hue => SkiaBlendMode::Hue,
      BlendMode::Saturation => SkiaBlendMode::Saturation,
      BlendMode::Color => SkiaBlendMode::Color,
      BlendMode::Luminosity => SkiaBlendMode::Luminosity,
      BlendMode::PlusLighter => SkiaBlendMode::Plus,
      BlendMode::PlusDarker => SkiaBlendMode::Darken,
      BlendMode::HueHsv => SkiaBlendMode::Hue,
      BlendMode::SaturationHsv => SkiaBlendMode::Saturation,
      BlendMode::ColorHsv => SkiaBlendMode::Color,
      BlendMode::LuminosityHsv => SkiaBlendMode::Luminosity,
      BlendMode::HueOklch => SkiaBlendMode::Hue,
      BlendMode::ChromaOklch => SkiaBlendMode::Saturation,
      BlendMode::ColorOklch => SkiaBlendMode::Color,
      BlendMode::LuminosityOklch => SkiaBlendMode::Luminosity,
    }
  }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
  use super::*;

  fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
    let width = pixmap.width();
    let idx = ((y * width + x) * 4) as usize;
    let data = pixmap.data();
    (data[idx], data[idx + 1], data[idx + 2], data[idx + 3])
  }

  #[test]
  fn test_canvas_creation() {
    let canvas = Canvas::new(100, 100, Rgba::WHITE);
    assert!(canvas.is_ok());

    let canvas = canvas.unwrap();
    assert_eq!(canvas.width(), 100);
    assert_eq!(canvas.height(), 100);
  }

  #[test]
  fn test_canvas_creation_transparent() {
    let canvas = Canvas::new_transparent(50, 50);
    assert!(canvas.is_ok());
  }

  #[test]
  fn test_canvas_bounds() {
    let canvas = Canvas::new(200, 150, Rgba::WHITE).unwrap();
    let bounds = canvas.bounds();

    assert_eq!(bounds.x(), 0.0);
    assert_eq!(bounds.y(), 0.0);
    assert_eq!(bounds.width(), 200.0);
    assert_eq!(bounds.height(), 150.0);
  }

  #[test]
  fn test_canvas_size() {
    let canvas = Canvas::new(300, 200, Rgba::WHITE).unwrap();
    let size = canvas.size();

    assert_eq!(size.width, 300.0);
    assert_eq!(size.height, 200.0);
  }

  #[test]
  fn test_canvas_clear() {
    let mut canvas = Canvas::new(10, 10, Rgba::WHITE).unwrap();
    canvas.clear(Rgba::rgb(255, 0, 0));

    // tiny-skia uses premultiplied RGBA format
    // Verify first pixel is red
    let data = canvas.pixmap().data();
    assert_eq!(data[0], 255); // R
    assert_eq!(data[1], 0); // G
    assert_eq!(data[2], 0); // B
    assert_eq!(data[3], 255); // A
  }

  #[test]
  fn test_canvas_draw_rect() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();
    let rect = Rect::from_xywh(10.0, 10.0, 20.0, 20.0);
    canvas.draw_rect(rect, Rgba::rgb(255, 0, 0));

    // Verify the pixmap was modified
    let pixmap = canvas.into_pixmap();
    assert_eq!(pixmap.width(), 100);
  }

  #[test]
  fn test_canvas_state_save_restore() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    assert_eq!(canvas.state_depth(), 0);

    canvas.save();
    assert_eq!(canvas.state_depth(), 1);

    canvas.set_opacity(0.5);
    assert_eq!(canvas.opacity(), 0.5);

    canvas.save();
    assert_eq!(canvas.state_depth(), 2);

    canvas.restore();
    assert_eq!(canvas.state_depth(), 1);
    assert_eq!(canvas.opacity(), 0.5);

    canvas.restore();
    assert_eq!(canvas.state_depth(), 0);
    assert_eq!(canvas.opacity(), 1.0);
  }

  #[test]
  fn test_canvas_opacity() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    assert_eq!(canvas.opacity(), 1.0);

    canvas.set_opacity(0.5);
    assert_eq!(canvas.opacity(), 0.5);

    // Clamping
    canvas.set_opacity(1.5);
    assert_eq!(canvas.opacity(), 1.0);

    canvas.set_opacity(-0.5);
    assert_eq!(canvas.opacity(), 0.0);
  }

  #[test]
  fn test_canvas_transform() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    // Default is identity
    let t = canvas.transform();
    assert_eq!(t, Transform::identity());

    // Translate
    canvas.translate(10.0, 20.0);
    let t = canvas.transform();
    // Verify translation
    assert!((t.tx - 10.0).abs() < 0.001);
    assert!((t.ty - 20.0).abs() < 0.001);
  }

  #[test]
  fn test_border_radii_zero() {
    let radii = BorderRadii::ZERO;
    assert!(!radii.has_radius());
    assert_eq!(radii.max_radius(), 0.0);
  }

  #[test]
  fn test_border_radii_uniform() {
    let radii = BorderRadii::uniform(10.0);
    assert!(radii.has_radius());
    assert!(radii.is_uniform());
    assert_eq!(radii.max_radius(), 10.0);
  }

  #[test]
  fn test_border_radii_different() {
    let radii = BorderRadii::new(
      BorderRadius::uniform(5.0),
      BorderRadius::uniform(10.0),
      BorderRadius::uniform(15.0),
      BorderRadius::uniform(20.0),
    );
    assert!(radii.has_radius());
    assert!(!radii.is_uniform());
    assert_eq!(radii.max_radius(), 20.0);
  }

  #[test]
  fn test_canvas_draw_rounded_rect() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();
    let rect = Rect::from_xywh(10.0, 10.0, 50.0, 50.0);
    let radii = BorderRadii::uniform(5.0);
    canvas.draw_rounded_rect(rect, radii, Rgba::rgb(0, 0, 255));

    // Just verify it doesn't crash
    let _ = canvas.into_pixmap();
  }

  #[test]
  fn test_canvas_stroke_rect() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();
    let rect = Rect::from_xywh(10.0, 10.0, 50.0, 50.0);
    canvas.stroke_rect(rect, Rgba::BLACK, 2.0);

    let _ = canvas.into_pixmap();
  }

  #[test]
  fn test_canvas_draw_line() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();
    canvas.draw_line(
      Point::new(10.0, 10.0),
      Point::new(90.0, 90.0),
      Rgba::BLACK,
      1.0,
    );

    let _ = canvas.into_pixmap();
  }

  #[test]
  fn test_canvas_draw_circle() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();
    canvas.draw_circle(Point::new(50.0, 50.0), 20.0, Rgba::rgb(0, 255, 0));

    let _ = canvas.into_pixmap();
  }

  #[test]
  fn test_blend_mode_default() {
    assert_eq!(BlendMode::default(), BlendMode::Normal);
  }

  #[test]
  fn test_blend_mode_to_skia() {
    assert_eq!(BlendMode::Normal.to_skia(), SkiaBlendMode::SourceOver);
    assert_eq!(BlendMode::Multiply.to_skia(), SkiaBlendMode::Multiply);
    assert_eq!(BlendMode::Screen.to_skia(), SkiaBlendMode::Screen);
  }

  #[test]
  fn test_canvas_skip_transparent() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    // Drawing with transparent color should not crash
    canvas.draw_rect(Rect::from_xywh(10.0, 10.0, 20.0, 20.0), Rgba::TRANSPARENT);

    let _ = canvas.into_pixmap();
  }

  #[test]
  fn test_canvas_clip() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    canvas.set_clip(Rect::from_xywh(20.0, 20.0, 60.0, 60.0));

    // Draw a rectangle that extends beyond the clip
    canvas.draw_rect(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      Rgba::rgb(255, 0, 0),
    );

    canvas.clear_clip();

    let _ = canvas.into_pixmap();
  }

  #[test]
  fn clip_limits_rect_fill() {
    let mut canvas = Canvas::new(10, 10, Rgba::WHITE).unwrap();
    canvas.set_clip(Rect::from_xywh(2.0, 2.0, 4.0, 4.0));
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), Rgba::rgb(255, 0, 0));
    let pixmap = canvas.into_pixmap();

    assert_eq!(pixel(&pixmap, 3, 3), (255, 0, 0, 255));
    assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 255, 255));
  }

  #[test]
  fn rounded_clip_masks_corners() {
    let mut canvas = Canvas::new(12, 12, Rgba::WHITE).unwrap();
    canvas.set_clip_with_radii(
      Rect::from_xywh(2.0, 2.0, 8.0, 8.0),
      Some(BorderRadii::uniform(4.0)),
    );
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 12.0, 12.0), Rgba::rgb(0, 0, 255));
    let pixmap = canvas.into_pixmap();

    assert_eq!(pixel(&pixmap, 6, 6), (0, 0, 255, 255));
    assert_eq!(pixel(&pixmap, 2, 2), (255, 255, 255, 255));
  }

  #[test]
  fn translated_clip_tracks_device_bounds() {
    let mut canvas = Canvas::new(10, 10, Rgba::WHITE).unwrap();
    canvas.translate(2.0, 1.0);

    canvas.set_clip(Rect::from_xywh(1.0, 1.0, 4.0, 4.0));

    if let Some(bounds) = canvas.clip_bounds() {
      assert_eq!(bounds, Rect::from_xywh(3.0, 2.0, 4.0, 4.0));
    }

    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 6.0, 6.0), Rgba::rgb(255, 0, 0));
    let pixmap = canvas.into_pixmap();

    // Inside the translated clip
    assert_eq!(pixel(&pixmap, 4, 3), (255, 0, 0, 255));
    // Inside the draw rect but outside the clip bounds
    assert_eq!(pixel(&pixmap, 2, 2), (255, 255, 255, 255));
  }

  #[test]
  fn bounded_layer_matches_full_layer_output() {
    let mut full = Canvas::new(8, 8, Rgba::WHITE).unwrap();
    full.push_layer(1.0).unwrap();
    let rect = Rect::from_xywh(2.0, 3.0, 3.0, 2.0);
    full.draw_rect(rect, Rgba::RED);
    full.pop_layer().unwrap();
    let full_pixmap = full.into_pixmap();

    let mut bounded = Canvas::new(8, 8, Rgba::WHITE).unwrap();
    bounded
      .push_layer_bounded(1.0, None, rect)
      .expect("bounded layer");
    bounded.draw_rect(rect, Rgba::RED);
    bounded.pop_layer().unwrap();
    let bounded_pixmap = bounded.into_pixmap();

    assert_eq!(
      full_pixmap.data(),
      bounded_pixmap.data(),
      "bounded layer should match full layer rendering"
    );
  }

  #[test]
  fn rotated_clip_bounds_prevents_culling() {
    let mut canvas = Canvas::new(10, 10, Rgba::WHITE).unwrap();
    let rotate_90_about_center = Transform::from_row(0.0, 1.0, -1.0, 0.0, 10.0, 0.0);
    canvas.set_transform(rotate_90_about_center);

    let clip_rect = Rect::from_xywh(6.0, 2.0, 3.0, 5.0);
    canvas.set_clip(clip_rect);

    if let Some(bounds) = canvas.clip_bounds() {
      assert_eq!(bounds, Rect::from_xywh(3.0, 6.0, 5.0, 3.0));
    }

    canvas.draw_rect(Rect::from_xywh(4.0, 0.0, 6.0, 10.0), Rgba::rgb(0, 255, 0));
    let pixmap = canvas.into_pixmap();

    // Inside the rotated clip area
    assert_eq!(pixel(&pixmap, 4, 7), (0, 255, 0, 255));
    // Within the drawn rect but outside the rotated clip
    assert_eq!(pixel(&pixmap, 2, 7), (255, 255, 255, 255));
  }

  #[test]
  fn clip_path_scales_with_device_pixels() {
    let circle = ResolvedClipPath::Circle {
      center: Point::new(5.0, 5.0),
      radius: 4.0,
    };

    let mut canvas = Canvas::new(20, 15, Rgba::WHITE).unwrap();
    canvas.set_clip_path(&circle, 1.0);
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 20.0, 15.0), Rgba::rgb(255, 0, 0));
    let pixmap = canvas.into_pixmap();

    assert_eq!(pixel(&pixmap, 5, 5), (255, 0, 0, 255));
    assert_eq!(pixel(&pixmap, 12, 5), (255, 255, 255, 255));

    let mut hidpi = Canvas::new(40, 30, Rgba::WHITE).unwrap();
    hidpi.set_clip_path(&circle, 2.0);
    hidpi.draw_rect(Rect::from_xywh(0.0, 0.0, 40.0, 30.0), Rgba::rgb(0, 255, 0));
    let pixmap = hidpi.into_pixmap();

    assert_eq!(pixel(&pixmap, 15, 5), (0, 255, 0, 255));
    assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 255, 255));
    assert_eq!(pixel(&pixmap, 20, 5), (255, 255, 255, 255));
  }

  #[test]
  fn clip_path_follows_transforms() {
    let triangle = ResolvedClipPath::Polygon {
      points: vec![
        Point::new(0.0, 0.0),
        Point::new(4.0, 0.0),
        Point::new(0.0, 4.0),
      ],
      fill_rule: FillRule::Winding,
    };

    let mut canvas = Canvas::new(20, 15, Rgba::WHITE).unwrap();
    let transform = Transform::from_rotate(90.0).post_concat(Transform::from_translate(10.0, 0.0));
    canvas.set_transform(transform);
    canvas.set_clip_path(&triangle, 1.0);
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 20.0, 15.0), Rgba::rgb(0, 0, 255));

    let pixmap = canvas.into_pixmap();

    assert_eq!(pixel(&pixmap, 9, 2), (0, 0, 255, 255));
    assert_eq!(pixel(&pixmap, 1, 1), (255, 255, 255, 255));
  }

  #[test]
  fn bounded_layer_matches_full_layer() {
    let mut full = Canvas::new(12, 12, Rgba::WHITE).unwrap();
    full.push_layer(1.0).unwrap();
    full.translate(1.0, 1.0);
    full.set_clip(Rect::from_xywh(2.0, 2.0, 6.0, 6.0));
    full.draw_rect(Rect::from_xywh(2.0, 2.0, 3.0, 3.0), Rgba::rgb(255, 0, 0));
    full.pop_layer().unwrap();
    let full_pixmap = full.into_pixmap();

    let mut bounded = Canvas::new(12, 12, Rgba::WHITE).unwrap();
    bounded
      .push_layer_bounded(1.0, None, Rect::from_xywh(1.0, 1.0, 8.0, 8.0))
      .unwrap();
    bounded.translate(1.0, 1.0);
    bounded.set_clip(Rect::from_xywh(2.0, 2.0, 6.0, 6.0));
    bounded.draw_rect(Rect::from_xywh(2.0, 2.0, 3.0, 3.0), Rgba::rgb(255, 0, 0));
    bounded.pop_layer().unwrap();
    let bounded_pixmap = bounded.into_pixmap();

    assert_eq!(bounded_pixmap.data(), full_pixmap.data());
  }
}
