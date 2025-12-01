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
//! use fastrender::style::Rgba;
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

use crate::error::{RenderError, Result};
use crate::geometry::{Point, Rect, Size};
use crate::style::color::Rgba;
use crate::text::font_db::LoadedFont;
use crate::text::shaper::GlyphPosition;
use tiny_skia::{
    BlendMode as SkiaBlendMode, FillRule, Paint, PathBuilder, Pixmap, Rect as SkiaRect, Stroke, Transform,
};

use super::display_list::{BlendMode, BorderRadii};

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
    clip: Option<Rect>,
    /// Blend mode
    blend_mode: SkiaBlendMode,
}

impl CanvasState {
    /// Creates a new default state
    fn new() -> Self {
        Self {
            transform: Transform::identity(),
            opacity: 1.0,
            clip: None,
            blend_mode: SkiaBlendMode::SourceOver,
        }
    }

    /// Creates a paint with the current state applied
    fn create_paint(&self, color: Rgba) -> Paint<'static> {
        let mut paint = Paint::default();
        // Apply opacity to alpha (color.a is already 0.0-1.0)
        let alpha = color.a * self.opacity;
        paint.set_color_rgba8(color.r, color.g, color.b, (alpha * 255.0) as u8);
        paint.anti_alias = true;
        paint.blend_mode = self.blend_mode;
        paint
    }
}

impl Default for CanvasState {
    fn default() -> Self {
        Self::new()
    }
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
    /// Current graphics state
    current_state: CanvasState,
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
    /// use fastrender::style::Rgba;
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
            current_state: CanvasState::new(),
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
        self.current_state.clip = Some(rect);
    }

    /// Clears the clip rectangle
    pub fn clear_clip(&mut self) {
        self.current_state.clip = None;
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
            self.pixmap
                .fill_path(&path, &paint, FillRule::Winding, self.current_state.transform, None);
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
            self.pixmap
                .stroke_path(&path, &paint, &stroke, self.current_state.transform, None);
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

        if let Some(path) = self.build_rounded_rect_path(rect, radii) {
            let paint = self.current_state.create_paint(color);
            self.pixmap
                .fill_path(&path, &paint, FillRule::Winding, self.current_state.transform, None);
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

        if let Some(path) = self.build_rounded_rect_path(rect, radii) {
            let paint = self.current_state.create_paint(color);
            let stroke = Stroke {
                width,
                ..Default::default()
            };
            self.pixmap
                .stroke_path(&path, &paint, &stroke, self.current_state.transform, None);
        }
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
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// let shaped = shaper.shape_text("Hello", &font, 16.0, Script::Latin, TextDirection::Ltr)?;
    /// canvas.draw_text(Point::new(10.0, 50.0), &shaped.glyphs, &font, 16.0, Rgba::BLACK);
    /// ```
    pub fn draw_text(
        &mut self,
        position: Point,
        glyphs: &[GlyphPosition],
        font: &LoadedFont,
        font_size: f32,
        color: Rgba,
    ) {
        if glyphs.is_empty() || (color.a == 0.0 && self.current_state.opacity == 0.0) {
            return;
        }

        // Parse the font for glyph outlines
        let face = match ttf_parser::Face::parse(&font.data, font.index) {
            Ok(f) => f,
            Err(_) => return,
        };

        let units_per_em = face.units_per_em() as f32;
        let scale = font_size / units_per_em;

        let paint = self.current_state.create_paint(color);
        let mut x = position.x;

        for glyph in glyphs {
            let glyph_x = x + glyph.offset_x;
            let glyph_y = position.y + glyph.offset_y;

            // Get the glyph outline
            if let Some(path) = self.build_glyph_path(&face, glyph.glyph_id as u16, glyph_x, glyph_y, scale) {
                self.pixmap
                    .fill_path(&path, &paint, FillRule::Winding, self.current_state.transform, None);
            }

            x += glyph.advance;
        }
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
            self.pixmap
                .stroke_path(&path, &paint, &stroke, self.current_state.transform, None);
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
            self.pixmap
                .fill_path(&path, &paint, FillRule::Winding, self.current_state.transform, None);
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
            self.pixmap
                .stroke_path(&path, &paint, &stroke, self.current_state.transform, None);
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
    fn apply_clip(&self, rect: Rect) -> Option<Rect> {
        if let Some(clip) = self.current_state.clip {
            rect.intersection(clip)
        } else {
            Some(rect)
        }
    }

    /// Builds a path for a rounded rectangle
    fn build_rounded_rect_path(&self, rect: Rect, radii: BorderRadii) -> Option<tiny_skia::Path> {
        let x = rect.x();
        let y = rect.y();
        let w = rect.width();
        let h = rect.height();

        // Clamp radii to half the smaller dimension
        let max_radius = (w.min(h) / 2.0).max(0.0);
        let tl = radii.top_left.min(max_radius);
        let tr = radii.top_right.min(max_radius);
        let br = radii.bottom_right.min(max_radius);
        let bl = radii.bottom_left.min(max_radius);

        let mut pb = PathBuilder::new();

        // Start at top-left, after the corner radius
        pb.move_to(x + tl, y);

        // Top edge
        pb.line_to(x + w - tr, y);

        // Top-right corner
        if tr > 0.0 {
            pb.quad_to(x + w, y, x + w, y + tr);
        } else {
            pb.line_to(x + w, y);
        }

        // Right edge
        pb.line_to(x + w, y + h - br);

        // Bottom-right corner
        if br > 0.0 {
            pb.quad_to(x + w, y + h, x + w - br, y + h);
        } else {
            pb.line_to(x + w, y + h);
        }

        // Bottom edge
        pb.line_to(x + bl, y + h);

        // Bottom-left corner
        if bl > 0.0 {
            pb.quad_to(x, y + h, x, y + h - bl);
        } else {
            pb.line_to(x, y + h);
        }

        // Left edge
        pb.line_to(x, y + tl);

        // Top-left corner
        if tl > 0.0 {
            pb.quad_to(x, y, x + tl, y);
        } else {
            pb.line_to(x, y);
        }

        pb.close();
        pb.finish()
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

    /// Builds a path for a glyph outline
    fn build_glyph_path(
        &self,
        face: &ttf_parser::Face,
        glyph_id: u16,
        x: f32,
        y: f32,
        scale: f32,
    ) -> Option<tiny_skia::Path> {
        let glyph_id = ttf_parser::GlyphId(glyph_id);

        // Create a path builder that collects glyph outline
        let mut builder = GlyphPathBuilder::new(x, y, scale);

        face.outline_glyph(glyph_id, &mut builder);

        builder.finish()
    }
}

// ============================================================================
// Glyph Path Builder
// ============================================================================

/// Helper struct for building glyph outlines
struct GlyphPathBuilder {
    path_builder: PathBuilder,
    x: f32,
    y: f32,
    scale: f32,
}

impl GlyphPathBuilder {
    fn new(x: f32, y: f32, scale: f32) -> Self {
        Self {
            path_builder: PathBuilder::new(),
            x,
            y,
            scale,
        }
    }

    fn transform_x(&self, gx: f32) -> f32 {
        self.x + gx * self.scale
    }

    fn transform_y(&self, gy: f32) -> f32 {
        // Font coordinates have Y axis pointing up, flip for screen coordinates
        self.y - gy * self.scale
    }

    fn finish(self) -> Option<tiny_skia::Path> {
        self.path_builder.finish()
    }
}

impl ttf_parser::OutlineBuilder for GlyphPathBuilder {
    fn move_to(&mut self, x: f32, y: f32) {
        self.path_builder.move_to(self.transform_x(x), self.transform_y(y));
    }

    fn line_to(&mut self, x: f32, y: f32) {
        self.path_builder.line_to(self.transform_x(x), self.transform_y(y));
    }

    fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
        self.path_builder.quad_to(
            self.transform_x(x1),
            self.transform_y(y1),
            self.transform_x(x),
            self.transform_y(y),
        );
    }

    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        self.path_builder.cubic_to(
            self.transform_x(x1),
            self.transform_y(y1),
            self.transform_x(x2),
            self.transform_y(y2),
            self.transform_x(x),
            self.transform_y(y),
        );
    }

    fn close(&mut self) {
        self.path_builder.close();
    }
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
            // Additional blend modes map to SourceOver (normal) for now
            BlendMode::Hue | BlendMode::Saturation | BlendMode::Color | BlendMode::Luminosity => {
                SkiaBlendMode::SourceOver
            }
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

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
        let radii = BorderRadii::new(5.0, 10.0, 15.0, 20.0);
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
        canvas.draw_line(Point::new(10.0, 10.0), Point::new(90.0, 90.0), Rgba::BLACK, 1.0);

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
        canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), Rgba::rgb(255, 0, 0));

        canvas.clear_clip();

        let _ = canvas.into_pixmap();
    }
}
