# Phase 4: Rasterization with tiny-skia

**Duration:** Week 3 of Phase 4 (7-10 days)
**Prerequisites:**
- Phase 1-3 complete (layout, text shaping, fonts)
- Display list construction (04-display-list.md)
- Stacking contexts (04-stacking-contexts.md)
**Dependencies:**
- Sorted DisplayList from stacking context system
- tiny-skia for 2D rendering
- FontFace and ShapedText from text system
**Output:** Pixel-perfect rendering of display list to RGBA bitmap

## Objectives

Implement the rasterization system that converts the sorted display list into pixels using the tiny-skia 2D graphics library. This is the final step in the rendering pipeline that produces the actual image.

The rasterization system provides:
- Display item rendering (fill, stroke, text, images)
- Path rendering (borders, border-radius, box-shadow)
- Text rendering with glyph rasterization
- Image decoding and drawing
- Gradient rendering (linear, radial)
- Clipping and masking
- Blend modes and compositing
- Anti-aliasing and subpixel rendering
- Performance optimization (canvas reuse, caching)

## Context

Rasterization is the final stage of the rendering pipeline:

**Complete Pipeline:**
1. **HTML Parsing** → DOM Tree
2. **Style Resolution** → Computed Styles
3. **Layout** → Fragment Tree
4. **Display List Construction** → Display Items
5. **Stacking Context Sorting** → Correct Paint Order
6. **Rasterization** ← **WE ARE HERE** → Pixels!

**Why tiny-skia?**

- **Pure Rust** - No C dependencies
- **Fast** - Optimized software rendering
- **Small** - Minimal binary size (~100KB)
- **Correct** - Follows Skia (used by Chrome/Flutter)
- **Safe** - Memory-safe implementation
- **CPU-based** - Works without GPU

**From CSS Backgrounds and Borders Module Level 3:**
> "The border can have rounded corners. The border edge is the outer edge of the border, the padding edge is the inner edge of the border."

## The Problem V1 Has

V1 has minimal rendering:
- No real rasterization (stub implementation)
- Cannot render borders, shadows, gradients
- No text rendering
- No image support
- No anti-aliasing
- Produces incorrect output

## The Solution

Implement complete rasterization using tiny-skia:

1. **Canvas management** - Create/resize tiny-skia pixmaps
2. **Display item rendering** - Convert each item type to tiny-skia calls
3. **Path rendering** - Borders, rounded corners, shadows
4. **Text rendering** - Glyph outlines and positioning
5. **Image rendering** - Decode and draw images
6. **Effects** - Opacity, transforms, blend modes
7. **Optimization** - Caching, dirty regions

## CSS Specification References

**Primary:**
- **CSS Backgrounds and Borders Module Level 3:** Border and background rendering
  - https://www.w3.org/TR/css-backgrounds-3/
- **CSS Images Module Level 3:** Image rendering
  - https://www.w3.org/TR/css-images-3/
- **CSS Transforms Module Level 1:** Transform rendering
  - https://www.w3.org/TR/css-transforms-1/

**Related:**
- **CSS Color Module Level 4:** Color spaces and compositing
- **CSS Compositing and Blending Level 1:** Blend modes
- **CSS Filter Effects Module Level 1:** Filter rendering

## Step-by-Step Implementation

### Step 1: Setup tiny-skia Integration (Day 1 Morning)

```bash
mkdir -p /home/user/fastrender/src/paint/raster
touch /home/user/fastrender/src/paint/raster/mod.rs
touch /home/user/fastrender/src/paint/raster/canvas.rs
touch /home/user/fastrender/src/paint/raster/renderer.rs
touch /home/user/fastrender/src/paint/raster/paths.rs
touch /home/user/fastrender/src/paint/raster/text.rs
```

**File: `Cargo.toml` (add dependencies)**

```toml
[dependencies]
# 2D Graphics rendering
tiny-skia = "0.11"

# Image decoding
image = "0.24"

# For color conversion
rgb = "0.8"

# Already have from previous phases
rustybuzz = "0.11"     # Text shaping
ttf-parser = "0.20"    # Font parsing
```

**File: `src/paint/raster/mod.rs`**

```rust
//! Rasterization
//!
//! Converts display list to pixels using tiny-skia.

pub mod canvas;
pub mod renderer;
pub mod paths;
pub mod text;

pub use canvas::Canvas;
pub use renderer::DisplayListRenderer;

use crate::error::Result;
```

### Step 2: Implement Canvas Management (Day 1 Afternoon)

**File: `src/paint/raster/canvas.rs`**

```rust
//! Canvas management
//!
//! Manages tiny-skia pixmaps and provides high-level drawing API.

use tiny_skia::{Pixmap, Paint, PathBuilder, Stroke, FillRule, BlendMode as SkiaBlendMode};
use tiny_skia::{Transform as SkiaTransform, ClipMask, Color as SkiaColor};
use crate::geometry::{Point, Size, Rect};
use crate::style::Color;
use crate::error::{Result, Error};

/// Canvas for rendering
///
/// Wraps tiny-skia Pixmap and provides drawing operations.
pub struct Canvas {
    /// The pixel buffer
    pixmap: Pixmap,

    /// Current transform stack
    transform_stack: Vec<SkiaTransform>,

    /// Current clip stack
    clip_stack: Vec<ClipMask>,
}

impl Canvas {
    /// Create a new canvas
    pub fn new(width: u32, height: u32) -> Result<Self> {
        let pixmap = Pixmap::new(width, height)
            .ok_or_else(|| Error::Paint("Failed to create pixmap".into()))?;

        Ok(Self {
            pixmap,
            transform_stack: vec![SkiaTransform::identity()],
            clip_stack: Vec::new(),
        })
    }

    /// Get canvas width
    pub fn width(&self) -> u32 {
        self.pixmap.width()
    }

    /// Get canvas height
    pub fn height(&self) -> u32 {
        self.pixmap.height()
    }

    /// Clear canvas with color
    pub fn clear(&mut self, color: Color) {
        let skia_color = self.color_to_skia(color);
        self.pixmap.fill(skia_color);
    }

    /// Fill a rectangle
    pub fn fill_rect(&mut self, rect: Rect, color: Color) {
        let path = PathBuilder::from_rect(self.rect_to_skia(rect));

        let mut paint = Paint::default();
        paint.set_color(self.color_to_skia(color));

        let transform = self.current_transform();

        self.pixmap.fill_path(
            &path,
            &paint,
            FillRule::Winding,
            transform,
            None,
        );
    }

    /// Stroke a rectangle
    pub fn stroke_rect(&mut self, rect: Rect, color: Color, width: f32) {
        let path = PathBuilder::from_rect(self.rect_to_skia(rect));

        let mut paint = Paint::default();
        paint.set_color(self.color_to_skia(color));

        let stroke = Stroke {
            width,
            ..Default::default()
        };

        let transform = self.current_transform();

        self.pixmap.stroke_path(
            &path,
            &paint,
            &stroke,
            transform,
            None,
        );
    }

    /// Fill a path
    pub fn fill_path(&mut self, path: &tiny_skia::Path, color: Color) {
        let mut paint = Paint::default();
        paint.set_color(self.color_to_skia(color));

        let transform = self.current_transform();

        self.pixmap.fill_path(
            path,
            &paint,
            FillRule::Winding,
            transform,
            None,
        );
    }

    /// Stroke a path
    pub fn stroke_path(&mut self, path: &tiny_skia::Path, color: Color, width: f32) {
        let mut paint = Paint::default();
        paint.set_color(self.color_to_skia(color));

        let stroke = Stroke {
            width,
            ..Default::default()
        };

        let transform = self.current_transform();

        self.pixmap.stroke_path(
            path,
            &paint,
            &stroke,
            transform,
            None,
        );
    }

    /// Draw an image
    pub fn draw_image(&mut self, image: &Pixmap, dest: Rect) {
        // Scale image to destination rectangle
        let scale_x = dest.width() / (image.width() as f32);
        let scale_y = dest.height() / (image.height() as f32);

        let mut transform = self.current_transform();
        transform = transform.pre_translate(dest.min_x(), dest.min_y());
        transform = transform.pre_scale(scale_x, scale_y);

        self.pixmap.draw_pixmap(
            0, 0,
            image.as_ref(),
            &PixmapPaint::default(),
            transform,
            None,
        );
    }

    /// Push transform
    pub fn push_transform(&mut self, transform: SkiaTransform) {
        let current = self.current_transform();
        let new_transform = current.pre_concat(transform);
        self.transform_stack.push(new_transform);
    }

    /// Pop transform
    pub fn pop_transform(&mut self) {
        if self.transform_stack.len() > 1 {
            self.transform_stack.pop();
        }
    }

    /// Get current transform
    fn current_transform(&self) -> SkiaTransform {
        *self.transform_stack.last().unwrap()
    }

    /// Push clip region
    pub fn push_clip(&mut self, rect: Rect) {
        // Create clip mask
        let mut clip_mask = ClipMask::new();
        let path = PathBuilder::from_rect(self.rect_to_skia(rect));

        clip_mask.set_path(
            self.pixmap.width(),
            self.pixmap.height(),
            &path,
            FillRule::Winding,
            true, // anti-alias
        );

        self.clip_stack.push(clip_mask);
    }

    /// Pop clip region
    pub fn pop_clip(&mut self) {
        self.clip_stack.pop();
    }

    /// Get pixel data
    pub fn pixels(&self) -> &[u8] {
        self.pixmap.data()
    }

    /// Save to PNG file
    pub fn save_png(&self, path: &str) -> Result<()> {
        self.pixmap.save_png(path)
            .map_err(|e| Error::Paint(format!("Failed to save PNG: {}", e)))
    }

    /// Convert our Color to tiny-skia Color
    fn color_to_skia(&self, color: Color) -> SkiaColor {
        SkiaColor::from_rgba8(color.r, color.g, color.b, color.a)
    }

    /// Convert our Rect to tiny-skia Rect
    fn rect_to_skia(&self, rect: Rect) -> tiny_skia::Rect {
        tiny_skia::Rect::from_xywh(
            rect.min_x(),
            rect.min_y(),
            rect.width(),
            rect.height(),
        ).unwrap()
    }
}

use tiny_skia::PixmapPaint;
```

### Step 3: Implement Path Rendering (Day 2)

**File: `src/paint/raster/paths.rs`**

```rust
//! Path rendering utilities
//!
//! Helpers for creating paths for borders, rounded rectangles, shadows, etc.

use tiny_skia::{PathBuilder, Path};
use crate::geometry::{Point, Rect};
use crate::paint::display_list::BorderRadii;

/// Build a rounded rectangle path
pub fn rounded_rect(rect: Rect, radii: BorderRadii) -> Option<Path> {
    let mut pb = PathBuilder::new();

    let x = rect.min_x();
    let y = rect.min_y();
    let w = rect.width();
    let h = rect.height();

    // Top-left corner
    pb.move_to(x + radii.top_left, y);

    // Top edge
    pb.line_to(x + w - radii.top_right, y);

    // Top-right corner
    if radii.top_right > 0.0 {
        pb.quad_to(
            x + w, y,
            x + w, y + radii.top_right,
        );
    }

    // Right edge
    pb.line_to(x + w, y + h - radii.bottom_right);

    // Bottom-right corner
    if radii.bottom_right > 0.0 {
        pb.quad_to(
            x + w, y + h,
            x + w - radii.bottom_right, y + h,
        );
    }

    // Bottom edge
    pb.line_to(x + radii.bottom_left, y + h);

    // Bottom-left corner
    if radii.bottom_left > 0.0 {
        pb.quad_to(
            x, y + h,
            x, y + h - radii.bottom_left,
        );
    }

    // Left edge
    pb.line_to(x, y + radii.top_left);

    // Close to top-left corner
    if radii.top_left > 0.0 {
        pb.quad_to(
            x, y,
            x + radii.top_left, y,
        );
    }

    pb.close();
    pb.finish()
}

/// Build a box shadow path
///
/// Box shadows are complex:
/// 1. Create outer path (shadow bounds)
/// 2. Create inner path (box bounds)
/// 3. Fill the difference (ring)
/// 4. Apply blur
pub fn box_shadow_path(
    rect: Rect,
    radii: BorderRadii,
    offset: Point,
    blur_radius: f32,
    spread_radius: f32,
) -> Option<Path> {
    // Expand rectangle by spread
    let shadow_rect = rect.inflate(spread_radius);

    // Offset by shadow offset
    let shadow_rect = shadow_rect.translate(offset);

    // Create rounded rect with same radii (adjusted for spread)
    let adjusted_radii = BorderRadii {
        top_left: radii.top_left + spread_radius,
        top_right: radii.top_right + spread_radius,
        bottom_right: radii.bottom_right + spread_radius,
        bottom_left: radii.bottom_left + spread_radius,
    };

    rounded_rect(shadow_rect, adjusted_radii)
}

/// Build a circle path
pub fn circle(center: Point, radius: f32) -> Option<Path> {
    let mut pb = PathBuilder::new();

    // Approximate circle with 4 cubic bezier curves
    let kappa = 0.5522847498; // Magic number for circle approximation
    let control_dist = radius * kappa;

    let cx = center.x;
    let cy = center.y;

    // Start at top
    pb.move_to(cx, cy - radius);

    // Top-right quadrant
    pb.cubic_to(
        cx + control_dist, cy - radius,
        cx + radius, cy - control_dist,
        cx + radius, cy,
    );

    // Bottom-right quadrant
    pb.cubic_to(
        cx + radius, cy + control_dist,
        cx + control_dist, cy + radius,
        cx, cy + radius,
    );

    // Bottom-left quadrant
    pb.cubic_to(
        cx - control_dist, cy + radius,
        cx - radius, cy + control_dist,
        cx - radius, cy,
    );

    // Top-left quadrant
    pb.cubic_to(
        cx - radius, cy - control_dist,
        cx - control_dist, cy - radius,
        cx, cy - radius,
    );

    pb.close();
    pb.finish()
}

/// Build an ellipse path
pub fn ellipse(center: Point, radius_x: f32, radius_y: f32) -> Option<Path> {
    let mut pb = PathBuilder::new();

    let kappa = 0.5522847498;
    let control_x = radius_x * kappa;
    let control_y = radius_y * kappa;

    let cx = center.x;
    let cy = center.y;

    pb.move_to(cx, cy - radius_y);

    pb.cubic_to(
        cx + control_x, cy - radius_y,
        cx + radius_x, cy - control_y,
        cx + radius_x, cy,
    );

    pb.cubic_to(
        cx + radius_x, cy + control_y,
        cx + control_x, cy + radius_y,
        cx, cy + radius_y,
    );

    pb.cubic_to(
        cx - control_x, cy + radius_y,
        cx - radius_x, cy + control_y,
        cx - radius_x, cy,
    );

    pb.cubic_to(
        cx - radius_x, cy - control_y,
        cx - control_x, cy - radius_y,
        cx, cy - radius_y,
    );

    pb.close();
    pb.finish()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rounded_rect_creation() {
        let rect = Rect::new(Point::new(10.0, 10.0), Size::new(100.0, 50.0));
        let radii = BorderRadii::uniform(5.0);

        let path = rounded_rect(rect, radii);
        assert!(path.is_some());
    }

    #[test]
    fn test_circle_creation() {
        let center = Point::new(50.0, 50.0);
        let path = circle(center, 25.0);
        assert!(path.is_some());
    }
}

use crate::geometry::Size;
```

### Step 4: Implement Text Rendering (Day 3-4)

**File: `src/paint/raster/text.rs`**

```rust
//! Text rendering
//!
//! Renders shaped text using font glyph outlines.

use tiny_skia::{PathBuilder, Path, Paint, FillRule, Transform as SkiaTransform};
use crate::text::{ShapedText, FontFace};
use crate::geometry::Point;
use crate::style::Color;
use crate::error::Result;
use ttf_parser::Face;

/// Text renderer
pub struct TextRenderer<'a> {
    /// Shaped text to render
    text: &'a ShapedText,

    /// Font face
    font: &'a FontFace,

    /// Font size
    font_size: f32,
}

impl<'a> TextRenderer<'a> {
    /// Create a new text renderer
    pub fn new(text: &'a ShapedText, font: &'a FontFace, font_size: f32) -> Self {
        Self {
            text,
            font,
            font_size,
        }
    }

    /// Render text to a path
    ///
    /// This converts glyph outlines to a path that can be filled.
    pub fn render_to_path(&self, origin: Point) -> Result<Option<Path>> {
        let mut pb = PathBuilder::new();

        // Parse font
        let face = self.font.as_ttf_parser()?;

        // Get font units per em for scaling
        let units_per_em = face.units_per_em() as f32;
        let scale = self.font_size / units_per_em;

        let mut current_x = origin.x;
        let current_y = origin.y;

        // Iterate through glyphs
        for glyph_info in &self.text.glyphs {
            let glyph_id = ttf_parser::GlyphId(glyph_info.glyph_id as u16);

            // Get glyph outline
            if let Some(outline) = face.outline_glyph(glyph_id, &mut GlyphOutlineBuilder {
                builder: &mut pb,
                x_offset: current_x,
                y_offset: current_y,
                scale,
            }) {
                // Glyph rendered
            }

            // Advance
            current_x += glyph_info.x_advance * scale;
        }

        Ok(pb.finish())
    }

    /// Render text directly to canvas
    pub fn render(&self, canvas: &mut super::Canvas, origin: Point, color: Color) -> Result<()> {
        if let Some(path) = self.render_to_path(origin)? {
            canvas.fill_path(&path, color);
        }
        Ok(())
    }
}

/// Glyph outline builder
///
/// Converts ttf-parser outline commands to tiny-skia path commands.
struct GlyphOutlineBuilder<'a> {
    builder: &'a mut PathBuilder,
    x_offset: f32,
    y_offset: f32,
    scale: f32,
}

impl ttf_parser::OutlineBuilder for GlyphOutlineBuilder<'_> {
    fn move_to(&mut self, x: f32, y: f32) {
        let px = self.x_offset + x * self.scale;
        let py = self.y_offset - y * self.scale; // Flip Y (font coords are upside down)
        self.builder.move_to(px, py);
    }

    fn line_to(&mut self, x: f32, y: f32) {
        let px = self.x_offset + x * self.scale;
        let py = self.y_offset - y * self.scale;
        self.builder.line_to(px, py);
    }

    fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
        let px1 = self.x_offset + x1 * self.scale;
        let py1 = self.y_offset - y1 * self.scale;
        let px = self.x_offset + x * self.scale;
        let py = self.y_offset - y * self.scale;
        self.builder.quad_to(px1, py1, px, py);
    }

    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        let px1 = self.x_offset + x1 * self.scale;
        let py1 = self.y_offset - y1 * self.scale;
        let px2 = self.x_offset + x2 * self.scale;
        let py2 = self.y_offset - y2 * self.scale;
        let px = self.x_offset + x * self.scale;
        let py = self.y_offset - y * self.scale;
        self.builder.cubic_to(px1, py1, px2, py2, px, py);
    }

    fn close(&mut self) {
        self.builder.close();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_text_renderer_creation() {
        // Would need actual font and shaped text
        // This is a placeholder
    }
}
```

### Step 5: Implement Display List Renderer (Day 5-6)

**File: `src/paint/raster/renderer.rs`**

```rust
//! Display list renderer
//!
//! Renders display list to canvas using tiny-skia.

use super::Canvas;
use super::paths::{rounded_rect, box_shadow_path};
use super::text::TextRenderer;
use crate::paint::display_list::{DisplayList, DisplayItem};
use crate::paint::display_list::{FillRectItem, StrokeRectItem, FillRoundedRectItem};
use crate::paint::display_list::{TextItem, ImageItem, BoxShadowItem};
use crate::paint::display_list::{OpacityItem, TransformItem, ClipItem};
use crate::error::Result;
use tiny_skia::Transform as SkiaTransform;

/// Display list renderer
pub struct DisplayListRenderer {
    /// Canvas to render to
    canvas: Canvas,

    /// Opacity stack
    opacity_stack: Vec<f32>,
}

impl DisplayListRenderer {
    /// Create a new renderer
    pub fn new(width: u32, height: u32) -> Result<Self> {
        Ok(Self {
            canvas: Canvas::new(width, height)?,
            opacity_stack: vec![1.0],
        })
    }

    /// Render a display list
    pub fn render(&mut self, display_list: &DisplayList) -> Result<()> {
        for item in display_list.items() {
            self.render_item(item)?;
        }
        Ok(())
    }

    /// Render a single display item
    fn render_item(&mut self, item: &DisplayItem) -> Result<()> {
        match item {
            DisplayItem::FillRect(item) => self.render_fill_rect(item),
            DisplayItem::StrokeRect(item) => self.render_stroke_rect(item),
            DisplayItem::FillRoundedRect(item) => self.render_fill_rounded_rect(item),
            DisplayItem::StrokeRoundedRect(item) => self.render_stroke_rounded_rect(item),
            DisplayItem::Text(item) => self.render_text(item),
            DisplayItem::Image(item) => self.render_image(item),
            DisplayItem::BoxShadow(item) => self.render_box_shadow(item),
            DisplayItem::LinearGradient(item) => self.render_linear_gradient(item),
            DisplayItem::RadialGradient(item) => self.render_radial_gradient(item),
            DisplayItem::PushClip(item) => self.push_clip(item),
            DisplayItem::PopClip => self.pop_clip(),
            DisplayItem::PushOpacity(item) => self.push_opacity(item),
            DisplayItem::PopOpacity => self.pop_opacity(),
            DisplayItem::PushTransform(item) => self.push_transform(item),
            DisplayItem::PopTransform => self.pop_transform(),
            DisplayItem::PushBlendMode(_) => Ok(()), // TODO: Implement blend modes
            DisplayItem::PopBlendMode => Ok(()),
            DisplayItem::PushStackingContext(_) => Ok(()), // No-op (already sorted)
            DisplayItem::PopStackingContext => Ok(()),
        }
    }

    /// Render fill rectangle
    fn render_fill_rect(&mut self, item: &FillRectItem) -> Result<()> {
        let color = self.apply_opacity(item.color);
        self.canvas.fill_rect(item.rect, color);
        Ok(())
    }

    /// Render stroke rectangle
    fn render_stroke_rect(&mut self, item: &StrokeRectItem) -> Result<()> {
        let color = self.apply_opacity(item.color);
        self.canvas.stroke_rect(item.rect, color, item.width);
        Ok(())
    }

    /// Render filled rounded rectangle
    fn render_fill_rounded_rect(&mut self, item: &FillRoundedRectItem) -> Result<()> {
        if let Some(path) = rounded_rect(item.rect, item.radii) {
            let color = self.apply_opacity(item.color);
            self.canvas.fill_path(&path, color);
        }
        Ok(())
    }

    /// Render stroked rounded rectangle
    fn render_stroke_rounded_rect(&mut self, item: &super::super::display_list::StrokeRoundedRectItem) -> Result<()> {
        if let Some(path) = rounded_rect(item.rect, item.radii) {
            let color = self.apply_opacity(item.color);
            self.canvas.stroke_path(&path, color, item.width);
        }
        Ok(())
    }

    /// Render text
    fn render_text(&mut self, item: &TextItem) -> Result<()> {
        // Get font from text
        // In practice, need to load font from font system
        // For now, simplified

        // let font = get_font_for_text(&item.text)?;
        // let renderer = TextRenderer::new(&item.text, &font, item.font_size);
        // let color = self.apply_opacity(item.color);
        // renderer.render(&mut self.canvas, item.origin, color)?;

        Ok(())
    }

    /// Render image
    fn render_image(&mut self, item: &ImageItem) -> Result<()> {
        // Decode image if needed
        // let pixmap = decode_image(&item.image)?;
        // self.canvas.draw_image(&pixmap, item.dest_rect);

        Ok(())
    }

    /// Render box shadow
    fn render_box_shadow(&mut self, item: &BoxShadowItem) -> Result<()> {
        // Box shadows are complex:
        // 1. Create shadow path
        // 2. Apply blur filter
        // 3. Fill with shadow color

        if let Some(shadow_path) = box_shadow_path(
            item.rect,
            item.radii,
            item.offset,
            item.blur_radius,
            item.spread_radius,
        ) {
            // Apply blur
            // tiny-skia doesn't have built-in blur, need to implement
            // For now, just fill without blur
            let color = self.apply_opacity(item.color);
            self.canvas.fill_path(&shadow_path, color);
        }

        Ok(())
    }

    /// Render linear gradient
    fn render_linear_gradient(&mut self, item: &super::super::display_list::LinearGradientItem) -> Result<()> {
        // TODO: Implement gradient rendering
        // tiny-skia supports gradients via Shader
        Ok(())
    }

    /// Render radial gradient
    fn render_radial_gradient(&mut self, item: &super::super::display_list::RadialGradientItem) -> Result<()> {
        // TODO: Implement gradient rendering
        Ok(())
    }

    /// Push clip region
    fn push_clip(&mut self, item: &ClipItem) -> Result<()> {
        self.canvas.push_clip(item.rect);
        Ok(())
    }

    /// Pop clip region
    fn pop_clip(&mut self) -> Result<()> {
        self.canvas.pop_clip();
        Ok(())
    }

    /// Push opacity
    fn push_opacity(&mut self, item: &OpacityItem) -> Result<()> {
        let current_opacity = self.current_opacity();
        let new_opacity = current_opacity * item.opacity;
        self.opacity_stack.push(new_opacity);
        Ok(())
    }

    /// Pop opacity
    fn pop_opacity(&mut self) -> Result<()> {
        if self.opacity_stack.len() > 1 {
            self.opacity_stack.pop();
        }
        Ok(())
    }

    /// Get current opacity
    fn current_opacity(&self) -> f32 {
        *self.opacity_stack.last().unwrap()
    }

    /// Apply current opacity to color
    fn apply_opacity(&self, color: crate::style::Color) -> crate::style::Color {
        let opacity = self.current_opacity();
        crate::style::Color {
            r: color.r,
            g: color.g,
            b: color.b,
            a: (color.a as f32 * opacity) as u8,
        }
    }

    /// Push transform
    fn push_transform(&mut self, item: &TransformItem) -> Result<()> {
        let transform = self.transform_to_skia(item.transform);
        self.canvas.push_transform(transform);
        Ok(())
    }

    /// Pop transform
    fn pop_transform(&mut self) -> Result<()> {
        self.canvas.pop_transform();
        Ok(())
    }

    /// Convert our Transform2D to tiny-skia Transform
    fn transform_to_skia(&self, t: crate::paint::display_list::Transform2D) -> SkiaTransform {
        SkiaTransform::from_row(t.a, t.b, t.c, t.d, t.e, t.f)
    }

    /// Get the rendered canvas
    pub fn canvas(&self) -> &Canvas {
        &self.canvas
    }

    /// Get mutable canvas
    pub fn canvas_mut(&mut self) -> &mut Canvas {
        &mut self.canvas
    }

    /// Get pixel data
    pub fn pixels(&self) -> &[u8] {
        self.canvas.pixels()
    }

    /// Save to PNG
    pub fn save_png(&self, path: &str) -> Result<()> {
        self.canvas.save_png(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::paint::display_list::*;
    use crate::geometry::{Point, Size, Rect};
    use crate::style::Color;

    #[test]
    fn test_renderer_creation() {
        let renderer = DisplayListRenderer::new(800, 600);
        assert!(renderer.is_ok());
    }

    #[test]
    fn test_render_fill_rect() {
        let mut renderer = DisplayListRenderer::new(100, 100).unwrap();

        let mut list = DisplayList::new();
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::new(Point::new(10.0, 10.0), Size::new(50.0, 50.0)),
            color: Color::rgb(255, 0, 0),
        }));

        let result = renderer.render(&list);
        assert!(result.is_ok());
    }

    #[test]
    fn test_opacity_stack() {
        let mut renderer = DisplayListRenderer::new(100, 100).unwrap();

        renderer.push_opacity(&OpacityItem { opacity: 0.5 }).unwrap();
        assert_eq!(renderer.current_opacity(), 0.5);

        renderer.push_opacity(&OpacityItem { opacity: 0.8 }).unwrap();
        assert_eq!(renderer.current_opacity(), 0.4); // 0.5 * 0.8

        renderer.pop_opacity().unwrap();
        assert_eq!(renderer.current_opacity(), 0.5);

        renderer.pop_opacity().unwrap();
        assert_eq!(renderer.current_opacity(), 1.0);
    }
}
```

### Step 6: Add Gradient and Image Support (Day 7)

**File: `src/paint/raster/gradients.rs`**

```rust
//! Gradient rendering
//!
//! Support for linear and radial gradients.

use tiny_skia::{Shader, GradientStop as SkiaGradientStop, SpreadMode, LinearGradient as SkiaLinearGradient};
use tiny_skia::{RadialGradient as SkiaRadialGradient, Point as SkiaPoint};
use crate::paint::display_list::{GradientStop, LinearGradientItem, RadialGradientItem};
use crate::geometry::Point;
use crate::style::Color;

/// Create linear gradient shader
pub fn create_linear_gradient(item: &LinearGradientItem) -> Option<Shader> {
    // Convert gradient stops
    let stops: Vec<SkiaGradientStop> = item.stops.iter().map(|stop| {
        SkiaGradientStop::new(
            stop.position,
            color_to_skia(stop.color),
        )
    }).collect();

    // Create gradient
    let gradient = SkiaLinearGradient::new(
        point_to_skia(item.start),
        point_to_skia(item.end),
        stops,
        SpreadMode::Pad,
        tiny_skia::Transform::identity(),
    )?;

    Some(gradient)
}

/// Create radial gradient shader
pub fn create_radial_gradient(item: &RadialGradientItem) -> Option<Shader> {
    let stops: Vec<SkiaGradientStop> = item.stops.iter().map(|stop| {
        SkiaGradientStop::new(
            stop.position,
            color_to_skia(stop.color),
        )
    }).collect();

    let gradient = SkiaRadialGradient::new(
        point_to_skia(item.center),
        point_to_skia(item.center), // Start and end are same for radial
        item.radius,
        0.0, // Start radius
        stops,
        SpreadMode::Pad,
        tiny_skia::Transform::identity(),
    )?;

    Some(gradient)
}

fn color_to_skia(color: Color) -> tiny_skia::Color {
    tiny_skia::Color::from_rgba8(color.r, color.g, color.b, color.a)
}

fn point_to_skia(point: Point) -> SkiaPoint {
    SkiaPoint::from_xy(point.x, point.y)
}
```

**File: `src/paint/raster/images.rs`**

```rust
//! Image decoding and rendering

use tiny_skia::Pixmap;
use crate::paint::display_list::ImageData;
use crate::error::{Result, Error};

/// Decode image data to pixmap
pub fn decode_image(image_data: &ImageData) -> Result<Pixmap> {
    // Convert RGBA8 data to pixmap
    Pixmap::from_vec(
        image_data.pixels.to_vec(),
        tiny_skia::IntSize::from_wh(image_data.width, image_data.height)
            .ok_or_else(|| Error::Paint("Invalid image size".into()))?,
    )
    .ok_or_else(|| Error::Paint("Failed to create pixmap from image data".into()))
}

/// Load image from file
pub fn load_image(path: &str) -> Result<Pixmap> {
    // Use image crate to decode
    let img = image::open(path)
        .map_err(|e| Error::Paint(format!("Failed to load image: {}", e)))?;

    let rgba = img.to_rgba8();
    let (width, height) = rgba.dimensions();

    Pixmap::from_vec(
        rgba.into_raw(),
        tiny_skia::IntSize::from_wh(width, height)
            .ok_or_else(|| Error::Paint("Invalid image dimensions".into()))?,
    )
    .ok_or_else(|| Error::Paint("Failed to create pixmap".into()))
}
```

### Step 7: Comprehensive Tests (Day 8-9)

**File: `tests/paint/rasterization_test.rs`**

```rust
//! Tests for rasterization

use fastrender::paint::raster::*;
use fastrender::paint::display_list::*;
use fastrender::geometry::{Point, Size, Rect};
use fastrender::style::Color;

#[test]
fn test_canvas_creation() {
    let canvas = Canvas::new(800, 600);
    assert!(canvas.is_ok());

    let canvas = canvas.unwrap();
    assert_eq!(canvas.width(), 800);
    assert_eq!(canvas.height(), 600);
}

#[test]
fn test_canvas_clear() {
    let mut canvas = Canvas::new(100, 100).unwrap();
    canvas.clear(Color::rgb(255, 0, 0));

    // All pixels should be red
    let pixels = canvas.pixels();
    assert_eq!(pixels[0], 255); // R
    assert_eq!(pixels[1], 0);   // G
    assert_eq!(pixels[2], 0);   // B
    assert_eq!(pixels[3], 255); // A
}

#[test]
fn test_fill_rect() {
    let mut canvas = Canvas::new(100, 100).unwrap();

    canvas.fill_rect(
        Rect::new(Point::new(10.0, 10.0), Size::new(20.0, 20.0)),
        Color::rgb(0, 255, 0),
    );

    // Pixels at (10, 10) should be green
    // (Would need to actually check pixel data)
}

#[test]
fn test_renderer_simple() {
    let mut renderer = DisplayListRenderer::new(200, 200).unwrap();

    let mut list = DisplayList::new();

    // Red background
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::new(Point::zero(), Size::new(200.0, 200.0)),
        color: Color::rgb(255, 0, 0),
    }));

    // Blue square in center
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::new(Point::new(75.0, 75.0), Size::new(50.0, 50.0)),
        color: Color::rgb(0, 0, 255),
    }));

    renderer.render(&list).unwrap();

    // Could save and visually inspect
    // renderer.save_png("test_output.png").unwrap();
}

#[test]
fn test_rounded_rect_rendering() {
    let mut renderer = DisplayListRenderer::new(200, 200).unwrap();

    let mut list = DisplayList::new();

    list.push(DisplayItem::FillRoundedRect(FillRoundedRectItem {
        rect: Rect::new(Point::new(50.0, 50.0), Size::new(100.0, 100.0)),
        color: Color::rgb(0, 255, 0),
        radii: BorderRadii::uniform(10.0),
    }));

    renderer.render(&list).unwrap();
}

#[test]
fn test_opacity_rendering() {
    let mut renderer = DisplayListRenderer::new(200, 200).unwrap();

    let mut list = DisplayList::new();

    // Background
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::new(Point::zero(), Size::new(200.0, 200.0)),
        color: Color::rgb(255, 255, 255),
    }));

    // Semi-transparent red square
    list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 0.5 }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::new(Point::new(50.0, 50.0), Size::new(100.0, 100.0)),
        color: Color::rgb(255, 0, 0),
    }));
    list.push(DisplayItem::PopOpacity);

    renderer.render(&list).unwrap();
}

#[test]
fn test_transform_rendering() {
    let mut renderer = DisplayListRenderer::new(200, 200).unwrap();

    let mut list = DisplayList::new();

    // Translate and draw
    list.push(DisplayItem::PushTransform(TransformItem {
        transform: Transform2D::translate(50.0, 50.0),
    }));

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::new(Point::zero(), Size::new(50.0, 50.0)),
        color: Color::rgb(0, 255, 0),
    }));

    list.push(DisplayItem::PopTransform);

    renderer.render(&list).unwrap();

    // Rectangle should appear at (50, 50) due to transform
}

#[test]
fn test_clip_rendering() {
    let mut renderer = DisplayListRenderer::new(200, 200).unwrap();

    let mut list = DisplayList::new();

    // Set clip region
    list.push(DisplayItem::PushClip(ClipItem {
        rect: Rect::new(Point::new(50.0, 50.0), Size::new(100.0, 100.0)),
        radii: None,
    }));

    // Draw large rectangle (should be clipped)
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::new(Point::zero(), Size::new(200.0, 200.0)),
        color: Color::rgb(255, 0, 0),
    }));

    list.push(DisplayItem::PopClip);

    renderer.render(&list).unwrap();

    // Only the clipped region should be painted
}
```

## Acceptance Criteria

- [ ] Canvas can be created with arbitrary dimensions
- [ ] Fill rect renders correctly
- [ ] Stroke rect renders correctly
- [ ] Rounded rectangles render with proper border-radius
- [ ] Text renders with correct glyphs and positioning
- [ ] Images can be decoded and rendered
- [ ] Box shadows render (even without blur initially)
- [ ] Linear gradients render correctly
- [ ] Radial gradients render correctly
- [ ] Clipping works correctly
- [ ] Opacity is applied correctly (multiplicative)
- [ ] Transforms work (translate, scale, rotate)
- [ ] Blend modes work (at least Normal and Multiply)
- [ ] Anti-aliasing is enabled by default
- [ ] Can save output to PNG for testing
- [ ] All tests pass: `cargo test raster`
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Coordinate System Confusion

**Wrong:**
```rust
// Forgetting that font coordinates are upside-down
let y = baseline + glyph_y; // Wrong! Y goes down in fonts
```

**Right:**
```rust
// Flip Y coordinate for fonts
let y = baseline - glyph_y; // Correct!
```

### Pitfall 2: Forgetting to Apply Opacity

**Wrong:**
```rust
// Using color directly without current opacity
canvas.fill_rect(rect, color); // Missing opacity!
```

**Right:**
```rust
// Apply current opacity to color
let color_with_opacity = self.apply_opacity(color);
canvas.fill_rect(rect, color_with_opacity);
```

### Pitfall 3: Unbalanced Push/Pop

**Wrong:**
```rust
canvas.push_clip(rect);
// ... render ...
// Forgot to pop! Stack imbalance!
```

**Right:**
```rust
canvas.push_clip(rect);
// ... render ...
canvas.pop_clip(); // Always pop!
```

### Pitfall 4: Transform Order

**Wrong:**
```rust
// Applying transform after drawing
canvas.fill_rect(rect, color);
canvas.push_transform(t); // Too late!
```

**Right:**
```rust
// Push transform BEFORE drawing
canvas.push_transform(t);
canvas.fill_rect(rect, color); // Now affected by transform
canvas.pop_transform();
```

## Performance Optimizations

1. **Canvas Reuse** - Don't recreate canvas every frame (10x speedup)
2. **Dirty Regions** - Only repaint changed areas (5-10x speedup)
3. **Glyph Caching** - Cache rendered glyphs (3-5x speedup for text)
4. **Path Caching** - Cache complex paths (2-3x speedup)
5. **Early Culling** - Skip items outside viewport before rasterization
6. **SIMD** - tiny-skia uses SIMD internally for performance

## Integration with Full Pipeline

**Complete flow:**

```rust
// 1. Parse HTML
let dom = parse_html(html);

// 2. Resolve styles
let styled_tree = resolve_styles(&dom, &css);

// 3. Layout
let fragment_tree = layout(&styled_tree);

// 4. Build display list
let display_list = build_display_list(&fragment_tree);

// 5. Build stacking context tree
let stacking_tree = build_stacking_tree(&fragment_tree);

// 6. Sort display list by z-order
let sorted_list = sort_by_paint_order(display_list, &stacking_tree);

// 7. Rasterize
let mut renderer = DisplayListRenderer::new(800, 600)?;
renderer.render(&sorted_list)?;

// 8. Output
renderer.save_png("output.png")?;
```

## Debugging Tips

1. **Save intermediate outputs** - Save display list to JSON, stacking tree to text
2. **Visual debugging** - Draw bounding boxes, show clip regions
3. **Color-code layers** - Show stacking contexts in different colors
4. **Compare with browsers** - Use browser DevTools to verify paint order
5. **Unit test individual items** - Test each display item type separately

## Next Steps

After rasterization is complete, the core rendering pipeline is done! Next phases:

- **Phase 5:** Interactive features (hit testing, scrolling)
- **Phase 6:** Performance optimization (dirty regions, caching)
- **Phase 7:** Advanced features (animations, filters)

## References

**Primary:**
- **CSS Backgrounds and Borders Module Level 3:** https://www.w3.org/TR/css-backgrounds-3/
- **CSS Images Module Level 3:** https://www.w3.org/TR/css-images-3/
- **tiny-skia documentation:** https://docs.rs/tiny-skia/

**Related:**
- **Skia Graphics Library:** https://skia.org/ (tiny-skia is based on this)
- **ttf-parser documentation:** https://docs.rs/ttf-parser/
- **CSS Color Module Level 4:** https://www.w3.org/TR/css-color-4/
- **CSS Compositing and Blending:** https://www.w3.org/TR/compositing-1/

---

**Last Updated:** 2025-11-19
**Status:** Ready for Implementation
