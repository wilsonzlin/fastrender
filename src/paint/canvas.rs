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
use super::display_list::FontVariation;
use crate::error::RenderError;
use crate::error::Result;
use crate::geometry::Point;
use crate::geometry::Rect;
use crate::geometry::Size;
use crate::paint::clip_path::ResolvedClipPath;
use crate::paint::display_list::GlyphInstance;
use crate::paint::pixmap::{new_pixmap, new_pixmap_with_context};
use crate::paint::text_rasterize::{
  concat_transforms, GlyphCacheStats, TextRasterizer, TextRenderState,
};
use crate::paint::text_shadow::PathBounds;
use crate::style::color::Rgba;
use crate::text::color_fonts::ColorGlyphRaster;
use crate::text::font_db::LoadedFont;
use crate::text::pipeline::{GlyphPosition, ShapedRun};
use rustybuzz::Variation as HbVariation;
use std::rc::Rc;
use tiny_skia::BlendMode as SkiaBlendMode;
use tiny_skia::FillRule;
use tiny_skia::IntSize;
use tiny_skia::Mask;
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
  clip_mask: Option<Rc<Mask>>,
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
  saved_state_depth: usize,
  parent_opacity: f32,
  parent_blend_mode: SkiaBlendMode,
  parent_transform: Transform,
  parent_clip_rect: Option<Rect>,
  parent_clip_mask: Option<Rc<Mask>>,
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
    Self::new_with_text_rasterizer(width, height, background, TextRasterizer::new())
  }

  /// Creates a new canvas with an explicit text rasterizer (shared caches, etc.).
  pub fn new_with_text_rasterizer(
    width: u32,
    height: u32,
    background: Rgba,
    text_rasterizer: TextRasterizer,
  ) -> Result<Self> {
    let pixmap = new_pixmap_with_context(width, height, "canvas")?;

    let mut canvas = Self {
      pixmap,
      state_stack: Vec::new(),
      layer_stack: Vec::new(),
      current_state: CanvasState::new(),
      text_rasterizer,
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

    let new_pixmap = new_pixmap_with_context(width, height, "layer")?;

    let record = LayerRecord {
      pixmap: std::mem::replace(&mut self.pixmap, new_pixmap),
      saved_state_depth: self.state_stack.len(),
      parent_opacity: self.current_state.opacity,
      parent_blend_mode: self.current_state.blend_mode,
      parent_transform: self.current_state.transform,
      parent_clip_rect: self.current_state.clip_rect,
      parent_clip_mask: self.current_state.clip_mask.clone(),
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
        self.current_state.clip_mask = crop_mask(
          mask.as_ref(),
          origin_x as u32,
          origin_y as u32,
          width,
          height,
        )
        .map(Rc::new);
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
    self.state_stack.truncate(record.saved_state_depth);
    self.current_state.opacity = record.parent_opacity;
    self.current_state.blend_mode = record.parent_blend_mode;
    self.current_state.transform = record.parent_transform;
    self.current_state.clip_rect = record.parent_clip_rect;
    self.current_state.clip_mask = record.parent_clip_mask;
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
    let clip = self.current_state.clip_mask.as_deref();
    let transform = self.current_state.transform;

    self
      .pixmap
      .draw_pixmap(origin.0, origin.1, layer.as_ref(), &paint, transform, clip);
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
        combine_masks(&mut next, existing.as_ref());
        Some(Rc::new(next))
      }
      (Some(mask), None) => Some(Rc::new(mask)),
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
        combine_masks(&mut next, existing.as_ref());
        Some(Rc::new(next))
      }
      (Some(mask), None) => Some(Rc::new(mask)),
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
    self.current_state.clip_mask.as_deref()
  }

  fn current_text_state<'a>(&self, clip_mask: Option<&'a Mask>) -> TextRenderState<'a> {
    TextRenderState {
      transform: self.current_state.transform,
      clip_mask,
      opacity: self.current_state.opacity,
      blend_mode: self.current_state.blend_mode,
    }
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
        self.current_state.clip_mask.as_deref(),
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
        self.current_state.clip_mask.as_deref(),
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
        self.current_state.clip_mask.as_deref(),
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
        self.current_state.clip_mask.as_deref(),
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
        self.current_state.clip_mask.as_deref(),
      );
    }
  }

  fn hb_variations(variations: &[FontVariation]) -> Vec<HbVariation> {
    variations
      .iter()
      .map(|v| HbVariation {
        tag: v.tag,
        value: v.value(),
      })
      .collect()
  }

  pub(crate) fn glyph_paths(
    &mut self,
    position: Point,
    glyphs: &[GlyphPosition],
    _font: &LoadedFont,
    font_size: f32,
    _synthetic_oblique: f32,
    _variations: &[FontVariation],
    _rotation: Option<Transform>,
  ) -> Result<(Vec<tiny_skia::Path>, PathBounds)> {
    // Approximate bounds using glyph advances to avoid needing full outline extraction.
    let mut min_x = position.x;
    let mut max_x = position.x;
    for glyph in glyphs {
      let gx = position.x + glyph.x_offset;
      min_x = min_x.min(gx);
      max_x = max_x.max(gx + glyph.x_advance);
    }
    let ascent = font_size;
    let descent = font_size * 0.25;
    let rect = tiny_skia::Rect::from_xywh(
      min_x,
      position.y - ascent,
      (max_x - min_x).max(0.0),
      ascent + descent,
    )
    .unwrap_or_else(|| tiny_skia::Rect::from_xywh(0.0, 0.0, 0.0, 0.0).unwrap());
    let mut bounds = PathBounds::new();
    bounds.include(&rect);
    Ok((Vec::new(), bounds))
  }

  /// Draws a shaped text run at the specified position.
  ///
  /// Applies the current canvas transform, clip, opacity, blend mode, palette
  /// index, and font variations to the provided [`ShapedRun`].
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// let pipeline = ShapingPipeline::new();
  /// let style = ComputedStyle::default();
  /// let runs = pipeline.shape("Hello", &style, &font_context)?;
  /// let run = &runs[0];
  /// canvas.draw_shaped_run(run, Point::new(10.0, 50.0), Rgba::BLACK)?;
  /// ```
  pub fn draw_shaped_run(&mut self, run: &ShapedRun, position: Point, color: Rgba) -> Result<()> {
    if run.glyphs.is_empty() || (color.a == 0.0 && self.current_state.opacity == 0.0) {
      return Ok(());
    }

    let state = self.current_text_state(self.current_state.clip_mask.as_deref());
    self.text_rasterizer.render_shaped_run_with_state(
      run,
      position.x,
      position.y,
      color,
      &mut self.pixmap,
      state,
    )?;
    Ok(())
  }

  /// Draws text glyphs at the specified position.
  ///
  /// Renders positioned glyphs from the text shaping pipeline using an explicit
  /// palette index and variation list.
  ///
  /// # Arguments
  ///
  /// * `position` - Baseline origin for the text
  /// * `glyphs` - Positioned glyphs from text shaping
  /// * `font` - Font containing glyph outlines
  /// * `font_size` - Font size in pixels
  /// * `color` - Text color
  /// * `synthetic_bold` - Additional stroke width to simulate bold
  /// * `synthetic_oblique` - Shear factor to simulate italics
  /// * `palette_index` - Color font palette index
  /// * `variations` - Active variation settings
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
  ///   &[],
  /// );
  /// ```
  pub fn draw_text(
    &mut self,
    position: Point,
    glyphs: &[GlyphInstance],
    font: &LoadedFont,
    font_size: f32,
    color: Rgba,
    synthetic_bold: f32,
    synthetic_oblique: f32,
    palette_index: u16,
    variations: &[FontVariation],
  ) -> Result<()> {
    if glyphs.is_empty() || (color.a == 0.0 && self.current_state.opacity == 0.0) {
      return Ok(());
    }

    let hb_variations = Self::hb_variations(variations);
    let state = self.current_text_state(self.current_state.clip_mask.as_deref());

    let positions: Vec<GlyphPosition> = glyphs
      .iter()
      .map(|g| GlyphPosition {
        glyph_id: g.glyph_id,
        cluster: 0,
        x_offset: g.offset.x,
        y_offset: g.offset.y,
        x_advance: g.advance,
        y_advance: 0.0,
      })
      .collect();

    self.text_rasterizer.render_glyph_run(
      &positions,
      font,
      font_size,
      synthetic_bold,
      synthetic_oblique,
      palette_index,
      &[],
      0,
      &hb_variations,
      None,
      position.x,
      position.y,
      color,
      state,
      &mut self.pixmap,
    )?;
    Ok(())
  }

  /// Draws a pre-rasterized color glyph pixmap.
  ///
  /// The provided `glyph_opacity` is multiplied by the current canvas state
  /// opacity so color glyphs participate in CSS opacity the same way outline
  /// fills do.
  pub fn draw_color_glyph(
    &mut self,
    position: Point,
    glyph: &ColorGlyphRaster,
    glyph_opacity: f32,
    glyph_transform: Option<Transform>,
  ) {
    let combined_opacity = (glyph_opacity * self.current_state.opacity).clamp(0.0, 1.0);
    if combined_opacity == 0.0 {
      return;
    }

    let mut paint = PixmapPaint::default();
    paint.opacity = combined_opacity;
    paint.blend_mode = self.current_state.blend_mode;
    let translation = Transform::from_translate(position.x + glyph.left, position.y + glyph.top);
    let mut transform = glyph_transform.unwrap_or_else(Transform::identity);
    transform = concat_transforms(transform, translation);
    transform = concat_transforms(self.current_state.transform, transform);
    let clip = self.current_state.clip_mask.as_deref();
    self
      .pixmap
      .draw_pixmap(0, 0, glyph.image.as_ref().as_ref(), &paint, transform, clip);
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
        self.current_state.clip_mask.as_deref(),
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
        self.current_state.clip_mask.as_deref(),
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
        self.current_state.clip_mask.as_deref(),
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
    if rect.width() <= 0.0
      || rect.height() <= 0.0
      || self.width() == 0
      || self.height() == 0
      || !rect.x().is_finite()
      || !rect.y().is_finite()
      || !rect.width().is_finite()
      || !rect.height().is_finite()
    {
      return None;
    }

    // Fast path: axis-aligned rectangular clips with identity transforms don't need the
    // full pixmap rasterization step. Filling the mask directly avoids allocating a
    // temporary RGBA pixmap (4 bytes/pixel) per clip.
    if radii.is_zero() && self.current_state.transform == Transform::identity() {
      return self.build_clip_mask_fast_rect(rect);
    }

    self.build_clip_mask_slow_path(rect, radii)
  }

  fn build_clip_mask_fast_rect(&self, rect: Rect) -> Option<Mask> {
    let mut mask = Mask::new(self.width(), self.height())?;
    mask.data_mut().fill(0);

    let paint = {
      let mut p = Paint::default();
      p.set_color_rgba8(255, 255, 255, 255);
      p
    };
    let path = self.build_rounded_rect_path(rect, BorderRadii::ZERO)?;
    mask.fill_path(
      &path,
      FillRule::Winding,
      paint.anti_alias,
      self.current_state.transform,
    );
    Some(mask)
  }

  fn build_clip_mask_slow_path(&self, rect: Rect, radii: BorderRadii) -> Option<Mask> {
    let mut mask = Mask::new(self.width(), self.height())?;
    mask.data_mut().fill(0);
    let paint = {
      let mut p = Paint::default();
      p.set_color_rgba8(255, 255, 255, 255);
      p
    };

    let path = self.build_rounded_rect_path(rect, radii)?;
    mask.fill_path(
      &path,
      FillRule::Winding,
      paint.anti_alias,
      self.current_state.transform,
    );
    Some(mask)
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

  #[inline]
  fn mul_div_255_round(value: u8, alpha: u8) -> u8 {
    // Match `tiny_skia::Pixmap::apply_mask` rounding behavior.
    let prod = value as u16 * alpha as u16;
    ((prod + 255) >> 8) as u8
  }

  for (dst, src) in into.data_mut().iter_mut().zip(existing.data().iter()) {
    *dst = mul_div_255_round(*dst, *src);
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

  let mut out = Mask::new(crop_w, crop_h)?;
  let src = mask.data();
  let dst = out.data_mut();
  let src_stride = mask_width as usize;
  let dst_stride = crop_w as usize;
  for row in 0..crop_h as usize {
    let src_idx = (origin_y as usize + row) * src_stride + origin_x as usize;
    let dst_idx = row * dst_stride;
    dst[dst_idx..dst_idx + dst_stride].copy_from_slice(&src[src_idx..src_idx + dst_stride]);
  }

  Some(out)
}

/// Applies a mask that is positioned in a larger coordinate space.
///
/// The provided `mask` is interpreted as covering the rectangle
/// `[mask_origin.x, mask_origin.y]..[mask_origin.x + mask.width, mask_origin.y + mask.height]`
/// in the same coordinate space as `pixmap_origin`.
///
/// Mask values outside of this rectangle are treated as `0` (fully transparent).
///
/// Returns `false` when the mask rectangle does not overlap the pixmap at all; callers can use
/// this to skip further work because the pixmap would become fully transparent.
pub(crate) fn apply_mask_with_offset(
  pixmap: &mut Pixmap,
  pixmap_origin: (i32, i32),
  mask: &Mask,
  mask_origin: (i32, i32),
) -> bool {
  #[inline]
  fn mul_div_255_round(value: u8, alpha: u8) -> u8 {
    // Match `tiny_skia::Pixmap::apply_mask` rounding behavior.
    let prod = value as u16 * alpha as u16;
    ((prod + 255) >> 8) as u8
  }

  let pixmap_w = pixmap.width() as i32;
  let pixmap_h = pixmap.height() as i32;
  if pixmap_w <= 0 || pixmap_h <= 0 {
    return false;
  }

  let pix_x0 = pixmap_origin.0;
  let pix_y0 = pixmap_origin.1;
  let pix_x1 = pix_x0 + pixmap_w;
  let pix_y1 = pix_y0 + pixmap_h;

  let mask_x0 = mask_origin.0;
  let mask_y0 = mask_origin.1;
  let mask_x1 = mask_x0 + mask.width() as i32;
  let mask_y1 = mask_y0 + mask.height() as i32;

  let inter_x0 = pix_x0.max(mask_x0);
  let inter_y0 = pix_y0.max(mask_y0);
  let inter_x1 = pix_x1.min(mask_x1);
  let inter_y1 = pix_y1.min(mask_y1);

  if inter_x1 <= inter_x0 || inter_y1 <= inter_y0 {
    return false;
  }

  let local_x0 = (inter_x0 - pix_x0) as usize;
  let local_y0 = (inter_y0 - pix_y0) as usize;
  let local_x1 = (inter_x1 - pix_x0) as usize;
  let local_y1 = (inter_y1 - pix_y0) as usize;

  let mask_local_x0 = (inter_x0 - mask_x0) as usize;
  let mask_local_y0 = (inter_y0 - mask_y0) as usize;

  let pixmap_stride = pixmap.width() as usize * 4;
  let pixmap_height = pixmap.height() as usize;
  let pix_data = pixmap.data_mut();

  // Clear rows above/below the intersection. Use one contiguous fill per region rather than
  // per-row loops to keep the hot path in `memset`.
  if local_y0 > 0 {
    pix_data[..local_y0 * pixmap_stride].fill(0);
  }
  if local_y1 < pixmap_height {
    pix_data[local_y1 * pixmap_stride..].fill(0);
  }

  let mask_stride = mask.width() as usize;
  let mask_data = mask.data();
  for row_offset in 0..(local_y1 - local_y0) {
    let y = local_y0 + row_offset;
    let pix_row = &mut pix_data[y * pixmap_stride..(y + 1) * pixmap_stride];

    // Clear left/right segments where mask is implicitly zero.
    if local_x0 > 0 {
      pix_row[..local_x0 * 4].fill(0);
    }
    if local_x1 < pixmap_w as usize {
      pix_row[local_x1 * 4..].fill(0);
    }

    let mask_y = mask_local_y0 + row_offset;
    if mask_y >= mask.height() as usize {
      continue;
    }
    let mask_row = &mask_data[mask_y * mask_stride..(mask_y + 1) * mask_stride];
    let mask_slice = &mask_row[mask_local_x0..mask_local_x0 + (local_x1 - local_x0)];

    let mut base = local_x0 * 4;
    for m in mask_slice.iter().copied() {
      if m == 255 {
        base += 4;
        continue;
      }
      if m == 0 {
        pix_row[base..base + 4].fill(0);
        base += 4;
        continue;
      }
      pix_row[base] = mul_div_255_round(pix_row[base], m);
      pix_row[base + 1] = mul_div_255_round(pix_row[base + 1], m);
      pix_row[base + 2] = mul_div_255_round(pix_row[base + 2], m);
      pix_row[base + 3] = mul_div_255_round(pix_row[base + 3], m);
      base += 4;
    }
  }

  true
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
  use crate::paint::pixmap::NewPixmapAllocRecorder;
  use tiny_skia::MaskType;

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

  #[test]
  fn save_restore_survives_layer_push_pop() {
    let mut canvas = Canvas::new(4, 4, Rgba::WHITE).unwrap();
    canvas.set_opacity(0.5);
    canvas.set_blend_mode(BlendMode::Screen);
    canvas.save();
    canvas.set_opacity(0.25);

    canvas.push_layer(0.8).unwrap();
    canvas.set_opacity(0.1);
    canvas.save();
    canvas.set_blend_mode(BlendMode::Multiply);
    canvas.restore();

    assert_eq!(canvas.state_depth(), 1);
    assert_eq!(canvas.opacity(), 0.1);

    canvas.pop_layer().unwrap();
    assert_eq!(canvas.state_depth(), 1);
    assert_eq!(canvas.opacity(), 0.25);
    assert_eq!(canvas.blend_mode(), SkiaBlendMode::Screen);

    canvas.restore();
    assert_eq!(canvas.state_depth(), 0);
    assert_eq!(canvas.opacity(), 0.5);
  }

  #[test]
  fn bounded_layer_preserves_parent_clip_mask() {
    let mut canvas = Canvas::new(8, 8, Rgba::WHITE).unwrap();
    canvas.set_clip_with_radii(
      Rect::from_xywh(1.0, 1.0, 6.0, 6.0),
      Some(BorderRadii::uniform(1.0)),
    );

    canvas
      .push_layer_bounded(1.0, None, Rect::from_xywh(1.0, 1.0, 6.0, 6.0))
      .unwrap();
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 8.0, 8.0), Rgba::rgb(255, 0, 0));
    canvas.pop_layer().unwrap();

    // Clip mask should be restored and still apply to subsequent draws.
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 8.0, 8.0), Rgba::rgb(0, 255, 0));
    let pixmap = canvas.into_pixmap();

    assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 255, 255));
    assert_eq!(pixel(&pixmap, 7, 7), (255, 255, 255, 255));
    assert_eq!(pixel(&pixmap, 3, 3), (0, 255, 0, 255));
  }

  #[test]
  fn fast_rect_clip_mask_matches_slow_path() {
    let canvas = Canvas::new_transparent(24, 24).unwrap();
    let rects = [
      // Integer-aligned.
      Rect::from_xywh(2.0, 3.0, 8.0, 6.0),
      // Fractional coordinates.
      Rect::from_xywh(1.2, 2.8, 10.7, 5.3),
      // Very thin rectangles.
      Rect::from_xywh(5.0, 5.0, 0.3, 10.0),
      Rect::from_xywh(7.4, 9.1, 8.0, 0.4),
      // Edge-clamped rectangles.
      Rect::from_xywh(-3.0, -2.0, 6.0, 5.0),
      Rect::from_xywh(20.0, 20.0, 10.0, 10.0),
    ];

    for rect in rects {
      let fast = canvas
        .build_clip_mask(rect, BorderRadii::ZERO)
        .expect("fast mask");
      let slow = canvas
        .build_clip_mask_slow_path(rect, BorderRadii::ZERO)
        .expect("slow mask");
      assert_eq!(
        fast.data(),
        slow.data(),
        "fast-path mask differs from slow-path for {rect:?}"
      );
    }
  }

  #[test]
  fn rect_clip_fast_path_avoids_pixmap_allocation() {
    let mut canvas = Canvas::new_transparent(8, 8).unwrap();
    let recorder = NewPixmapAllocRecorder::start();
    canvas.set_clip_with_radii(Rect::from_xywh(1.0, 1.0, 6.0, 6.0), None);
    let allocations = recorder.take();
    assert!(
      allocations.is_empty(),
      "expected no new_pixmap allocations for rect clip fast-path, got {allocations:?}"
    );
  }

  fn build_clip_mask_slow_path_reference(
    canvas: &Canvas,
    rect: Rect,
    radii: BorderRadii,
  ) -> Option<Mask> {
    let mut mask_pixmap = new_pixmap(canvas.width(), canvas.height())?;
    let paint = {
      let mut p = Paint::default();
      p.set_color_rgba8(255, 255, 255, 255);
      p
    };

    let path = canvas.build_rounded_rect_path(rect, radii)?;
    mask_pixmap.fill_path(
      &path,
      &paint,
      FillRule::Winding,
      canvas.current_state.transform,
      None,
    );
    Some(Mask::from_pixmap(mask_pixmap.as_ref(), MaskType::Alpha))
  }

  #[test]
  fn slow_path_clip_mask_matches_reference_for_rounded_rect_with_transform() {
    let mut canvas = Canvas::new_transparent(32, 32).unwrap();
    canvas.set_transform(Transform::from_translate(1.25, -0.5));

    let rect = Rect::from_xywh(4.2, 3.7, 12.8, 9.5);
    let radii = BorderRadii::uniform(3.3);

    let optimized = canvas
      .build_clip_mask_slow_path(rect, radii)
      .expect("optimized mask");
    let reference =
      build_clip_mask_slow_path_reference(&canvas, rect, radii).expect("reference mask");
    assert_eq!(optimized.data(), reference.data());
  }

  #[test]
  fn rounded_rect_clip_slow_path_avoids_pixmap_allocation() {
    let mut canvas = Canvas::new_transparent(8, 8).unwrap();
    let recorder = NewPixmapAllocRecorder::start();
    canvas.set_clip_with_radii(
      Rect::from_xywh(1.0, 1.0, 6.0, 6.0),
      Some(BorderRadii::uniform(2.0)),
    );
    let allocations = recorder.take();
    assert!(
      allocations.is_empty(),
      "expected no new_pixmap allocations for rounded clip masks, got {allocations:?}"
    );
  }

  #[test]
  fn transformed_rect_clip_slow_path_avoids_pixmap_allocation() {
    let mut canvas = Canvas::new_transparent(8, 8).unwrap();
    canvas.translate(0.5, 0.25);
    let recorder = NewPixmapAllocRecorder::start();
    canvas.set_clip(Rect::from_xywh(1.0, 1.0, 6.0, 6.0));
    let allocations = recorder.take();
    assert!(
      allocations.is_empty(),
      "expected no new_pixmap allocations for transformed rect clip masks, got {allocations:?}"
    );
  }

  #[test]
  fn crop_mask_extracts_expected_bytes() {
    let mut mask = Mask::new(5, 4).unwrap();
    for (idx, dst) in mask.data_mut().iter_mut().enumerate() {
      *dst = (idx as u8).wrapping_mul(17);
    }

    let cropped = crop_mask(&mask, 1, 1, 10, 10).unwrap();
    assert_eq!(cropped.width(), 4);
    assert_eq!(cropped.height(), 3);

    let mut expected = Vec::new();
    let src = mask.data();
    let src_stride = mask.width() as usize;
    for row in 0..3usize {
      let src_idx = (1 + row) * src_stride + 1;
      expected.extend_from_slice(&src[src_idx..src_idx + 4]);
    }

    assert_eq!(cropped.data(), expected.as_slice());
  }

  #[test]
  fn crop_mask_does_not_allocate_pixmaps() {
    let mask = Mask::new(16, 16).unwrap();

    let recorder = NewPixmapAllocRecorder::start();
    let _ = crop_mask(&mask, 0, 0, 8, 8).unwrap();
    assert!(
      recorder.take().is_empty(),
      "crop_mask should not allocate Pixmaps"
    );
  }

  #[test]
  fn apply_mask_with_offset_matches_tiny_skia_when_aligned() {
    let mut base = new_pixmap(8, 8).expect("pixmap");
    for (idx, chunk) in base.data_mut().chunks_exact_mut(4).enumerate() {
      let v = (idx as u8).wrapping_mul(37).wrapping_add(11);
      chunk.copy_from_slice(&[v, v.rotate_left(1), v.rotate_left(2), v.rotate_left(3)]);
    }

    let mut mask = Mask::new(8, 8).expect("mask");
    for y in 0..8u32 {
      for x in 0..8u32 {
        mask.data_mut()[(y * 8 + x) as usize] = (x * 17 + y * 13) as u8;
      }
    }

    let mut expected = base.clone();
    expected.apply_mask(&mask);

    let mut actual = base.clone();
    assert!(apply_mask_with_offset(&mut actual, (0, 0), &mask, (0, 0)));

    assert_eq!(actual.data(), expected.data());
  }
}
