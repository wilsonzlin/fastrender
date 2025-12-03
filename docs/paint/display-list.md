# Phase 4: Display List Construction

**Duration:** Week 1 of Phase 4 (5-7 days)
**Prerequisites:**
- Phase 1 complete (type system, formatting contexts, box generation)
- Phase 2 complete (block layout, inline layout, positioned layout)
- Phase 3 complete (font system, text shaping)
**Dependencies:**
- FragmentNode tree from layout
- ComputedStyle with paint properties
- Stacking context tree (next step)
**Output:** Complete display list intermediate representation ready for rasterization

## Objectives

Implement the display list construction system that converts the fragment tree (output of layout) into an intermediate representation optimized for painting. The display list is a flat, ordered list of paint commands that can be efficiently executed by the rasterizer.

The display list provides:
- Conversion from hierarchical fragment tree to flat display items
- Correct paint order according to CSS stacking context rules
- Viewport culling (skip items outside visible area)
- Effect stack management (opacity, blend modes, filters, transforms)
- Display list optimization (batching, merging)
- Integration with stacking contexts for z-index ordering

## Context

The display list sits between layout and rasterization in the rendering pipeline:

**Rendering Pipeline:**
1. **HTML Parsing** → DOM Tree
2. **Style Resolution** → Computed Styles
3. **Layout** → Fragment Tree (positioned boxes with sizes)
4. **Display List Construction** ← **WE ARE HERE**
5. **Stacking Context Sorting** → Correct z-order
6. **Rasterization** → Pixels

**Why a Display List?**

The fragment tree is optimized for layout (hierarchical, parent-child relationships). The display list is optimized for painting:

- **Flat structure** - No tree traversal during painting
- **Paint order** - Items already sorted by z-index and stacking rules
- **Culling** - Items outside viewport removed
- **Batching** - Similar items grouped for GPU efficiency
- **Effects** - Opacity/blend modes applied correctly

**From CSS 2.1 Appendix E:**
> "The order in which the rendering tree is painted to the canvas is not necessarily the same order in which the elements appear in the document tree."

## The Problem V1 Has

V1 doesn't have a display list:
- Paints directly from layout tree (inefficient)
- No proper paint ordering (z-index issues)
- No culling optimization
- Cannot handle complex effects (opacity, blend modes)
- Repeats work on every frame

## The Solution

Implement a proper display list with:
1. **Display items** - Typed paint commands (fill, stroke, text, image, etc.)
2. **Builder** - Converts fragment tree to display list
3. **Culling** - Skip items outside viewport
4. **Effect stack** - Track opacity, blend modes, transforms
5. **Optimization** - Merge adjacent fills, batch similar items

## CSS Specification References

**Primary:**
- **CSS 2.1 Appendix E:** Elaborate description of Stacking Contexts
  - https://www.w3.org/TR/CSS21/zindex.html
- **CSS 2.2 Section 9.9:** Layered presentation
  - https://www.w3.org/TR/CSS22/visuren.html#z-index

**Related:**
- **CSS Backgrounds and Borders Module Level 3:** Background/border painting
- **CSS Transforms Module Level 1:** Transform effects
- **CSS Compositing and Blending Level 1:** Blend modes and compositing

## Step-by-Step Implementation

### Step 1: Define Display Item Types (Day 1 Morning)

```bash
mkdir -p /home/user/fastrender/src/paint/display_list
touch /home/user/fastrender/src/paint/display_list/mod.rs
touch /home/user/fastrender/src/paint/display_list/items.rs
touch /home/user/fastrender/src/paint/display_list/builder.rs
```

**File: `src/paint/display_list/items.rs`**

```rust
//! Display list items
//!
//! Typed paint commands that represent what to draw.

use crate::geometry::{Point, Size, Rect};
use crate::style::{Color, ComputedStyle};
use crate::text::ShapedText;
use std::sync::Arc;

/// A single display item
///
/// Represents one paint operation (draw rectangle, draw text, etc.)
#[derive(Debug, Clone)]
pub enum DisplayItem {
    /// Fill a rectangle with a solid color
    FillRect(FillRectItem),

    /// Stroke a rectangle outline
    StrokeRect(StrokeRectItem),

    /// Fill a rounded rectangle (border-radius)
    FillRoundedRect(FillRoundedRectItem),

    /// Stroke a rounded rectangle
    StrokeRoundedRect(StrokeRoundedRectItem),

    /// Draw a text run
    Text(TextItem),

    /// Draw an image
    Image(ImageItem),

    /// Draw a box shadow
    BoxShadow(BoxShadowItem),

    /// Draw a linear gradient
    LinearGradient(LinearGradientItem),

    /// Draw a radial gradient
    RadialGradient(RadialGradientItem),

    /// Begin a clip region
    PushClip(ClipItem),

    /// End a clip region
    PopClip,

    /// Begin an opacity layer
    PushOpacity(OpacityItem),

    /// End an opacity layer
    PopOpacity,

    /// Begin a transform
    PushTransform(TransformItem),

    /// End a transform
    PopTransform,

    /// Begin a blend mode
    PushBlendMode(BlendModeItem),

    /// End a blend mode
    PopBlendMode,

    /// Begin a stacking context
    PushStackingContext(StackingContextItem),

    /// End a stacking context
    PopStackingContext,
}

/// Fill a rectangle with a solid color
#[derive(Debug, Clone)]
pub struct FillRectItem {
    /// Rectangle to fill
    pub rect: Rect,

    /// Fill color
    pub color: Color,
}

/// Stroke a rectangle outline
#[derive(Debug, Clone)]
pub struct StrokeRectItem {
    /// Rectangle to stroke
    pub rect: Rect,

    /// Stroke color
    pub color: Color,

    /// Stroke width
    pub width: f32,
}

/// Fill a rounded rectangle
#[derive(Debug, Clone)]
pub struct FillRoundedRectItem {
    /// Rectangle bounds
    pub rect: Rect,

    /// Fill color
    pub color: Color,

    /// Border radii (top-left, top-right, bottom-right, bottom-left)
    pub radii: BorderRadii,
}

/// Stroke a rounded rectangle
#[derive(Debug, Clone)]
pub struct StrokeRoundedRectItem {
    /// Rectangle bounds
    pub rect: Rect,

    /// Stroke color
    pub color: Color,

    /// Stroke width
    pub width: f32,

    /// Border radii
    pub radii: BorderRadii,
}

/// Border radii for rounded rectangles
#[derive(Debug, Clone, Copy)]
pub struct BorderRadii {
    pub top_left: f32,
    pub top_right: f32,
    pub bottom_right: f32,
    pub bottom_left: f32,
}

impl BorderRadii {
    /// Create uniform border radius
    pub fn uniform(radius: f32) -> Self {
        Self {
            top_left: radius,
            top_right: radius,
            bottom_right: radius,
            bottom_left: radius,
        }
    }

    /// Check if any radius is non-zero
    pub fn has_radius(&self) -> bool {
        self.top_left > 0.0 || self.top_right > 0.0
            || self.bottom_right > 0.0 || self.bottom_left > 0.0
    }
}

/// Draw a text run
#[derive(Debug, Clone)]
pub struct TextItem {
    /// Position to draw text (baseline origin)
    pub origin: Point,

    /// Shaped text with glyphs
    pub text: Arc<ShapedText>,

    /// Text color
    pub color: Color,

    /// Font size
    pub font_size: f32,
}

/// Draw an image
#[derive(Debug, Clone)]
pub struct ImageItem {
    /// Destination rectangle
    pub dest_rect: Rect,

    /// Image data
    pub image: Arc<ImageData>,

    /// Source rectangle (for sprite sheets, etc.)
    pub src_rect: Option<Rect>,
}

/// Image data
#[derive(Debug, Clone)]
pub struct ImageData {
    /// Image width
    pub width: u32,

    /// Image height
    pub height: u32,

    /// Pixel data (RGBA8)
    pub pixels: Arc<Vec<u8>>,
}

/// Draw a box shadow
#[derive(Debug, Clone)]
pub struct BoxShadowItem {
    /// Box bounds
    pub rect: Rect,

    /// Border radii (if rounded)
    pub radii: BorderRadii,

    /// Shadow offset
    pub offset: Point,

    /// Blur radius
    pub blur_radius: f32,

    /// Spread radius
    pub spread_radius: f32,

    /// Shadow color
    pub color: Color,

    /// Inset shadow?
    pub inset: bool,
}

/// Linear gradient
#[derive(Debug, Clone)]
pub struct LinearGradientItem {
    /// Rectangle to fill
    pub rect: Rect,

    /// Gradient start point (relative to rect)
    pub start: Point,

    /// Gradient end point (relative to rect)
    pub end: Point,

    /// Color stops
    pub stops: Vec<GradientStop>,
}

/// Radial gradient
#[derive(Debug, Clone)]
pub struct RadialGradientItem {
    /// Rectangle to fill
    pub rect: Rect,

    /// Gradient center
    pub center: Point,

    /// Gradient radius
    pub radius: f32,

    /// Color stops
    pub stops: Vec<GradientStop>,
}

/// Gradient color stop
#[derive(Debug, Clone)]
pub struct GradientStop {
    /// Position (0.0 to 1.0)
    pub position: f32,

    /// Color at this stop
    pub color: Color,
}

/// Clip region
#[derive(Debug, Clone)]
pub struct ClipItem {
    /// Clip rectangle
    pub rect: Rect,

    /// Border radii (for rounded clips)
    pub radii: Option<BorderRadii>,
}

/// Opacity layer
#[derive(Debug, Clone)]
pub struct OpacityItem {
    /// Opacity value (0.0 to 1.0)
    pub opacity: f32,
}

/// Transform
#[derive(Debug, Clone)]
pub struct TransformItem {
    /// Transform matrix (2D affine transform)
    pub transform: Transform2D,
}

/// 2D affine transform
///
/// Represents: [a c e]
///            [b d f]
///            [0 0 1]
#[derive(Debug, Clone, Copy)]
pub struct Transform2D {
    pub a: f32,
    pub b: f32,
    pub c: f32,
    pub d: f32,
    pub e: f32,
    pub f: f32,
}

impl Transform2D {
    /// Identity transform
    pub fn identity() -> Self {
        Self {
            a: 1.0, b: 0.0,
            c: 0.0, d: 1.0,
            e: 0.0, f: 0.0,
        }
    }

    /// Translation
    pub fn translate(x: f32, y: f32) -> Self {
        Self {
            a: 1.0, b: 0.0,
            c: 0.0, d: 1.0,
            e: x, f: y,
        }
    }

    /// Scale
    pub fn scale(sx: f32, sy: f32) -> Self {
        Self {
            a: sx, b: 0.0,
            c: 0.0, d: sy,
            e: 0.0, f: 0.0,
        }
    }

    /// Rotation (angle in radians)
    pub fn rotate(angle: f32) -> Self {
        let cos = angle.cos();
        let sin = angle.sin();
        Self {
            a: cos, b: sin,
            c: -sin, d: cos,
            e: 0.0, f: 0.0,
        }
    }

    /// Multiply two transforms
    pub fn multiply(&self, other: &Transform2D) -> Transform2D {
        Transform2D {
            a: self.a * other.a + self.c * other.b,
            b: self.b * other.a + self.d * other.b,
            c: self.a * other.c + self.c * other.d,
            d: self.b * other.c + self.d * other.d,
            e: self.a * other.e + self.c * other.f + self.e,
            f: self.b * other.e + self.d * other.f + self.f,
        }
    }

    /// Transform a point
    pub fn transform_point(&self, p: Point) -> Point {
        Point {
            x: self.a * p.x + self.c * p.y + self.e,
            y: self.b * p.x + self.d * p.y + self.f,
        }
    }
}

/// Blend mode
#[derive(Debug, Clone)]
pub struct BlendModeItem {
    /// Blend mode
    pub mode: BlendMode,
}

/// Blend modes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlendMode {
    Normal,
    Multiply,
    Screen,
    Overlay,
    Darken,
    Lighten,
    ColorDodge,
    ColorBurn,
    HardLight,
    SoftLight,
    Difference,
    Exclusion,
    Hue,
    Saturation,
    Color,
    Luminosity,
}

/// Stacking context
#[derive(Debug, Clone)]
pub struct StackingContextItem {
    /// Z-index
    pub z_index: i32,

    /// Creates stacking context?
    pub creates_stacking_context: bool,
}
```

### Step 2: Implement Display List Structure (Day 1 Afternoon)

**File: `src/paint/display_list/mod.rs`**

```rust
//! Display list
//!
//! Intermediate representation for painting.
//! Converts fragment tree to flat list of paint commands.

pub mod items;
pub mod builder;

pub use items::*;
pub use builder::DisplayListBuilder;

use crate::geometry::Rect;

/// Display list
///
/// Flat list of display items in correct paint order.
#[derive(Debug, Clone)]
pub struct DisplayList {
    /// Display items in paint order
    items: Vec<DisplayItem>,

    /// Bounding rectangle of all items
    bounds: Rect,
}

impl DisplayList {
    /// Create an empty display list
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            bounds: Rect::zero(),
        }
    }

    /// Create with items
    pub fn with_items(items: Vec<DisplayItem>) -> Self {
        let bounds = Self::compute_bounds(&items);
        Self { items, bounds }
    }

    /// Add an item
    pub fn push(&mut self, item: DisplayItem) {
        self.items.push(item);
        // TODO: Update bounds
    }

    /// Get items
    pub fn items(&self) -> &[DisplayItem] {
        &self.items
    }

    /// Get mutable items
    pub fn items_mut(&mut self) -> &mut Vec<DisplayItem> {
        &mut self.items
    }

    /// Get bounds
    pub fn bounds(&self) -> Rect {
        self.bounds
    }

    /// Filter items within viewport
    ///
    /// Returns a new display list with only items that intersect the viewport.
    /// This is the culling optimization.
    pub fn cull(&self, viewport: Rect) -> DisplayList {
        let mut culled_items = Vec::new();

        for item in &self.items {
            if self.item_intersects_viewport(item, viewport) {
                culled_items.push(item.clone());
            }
        }

        DisplayList::with_items(culled_items)
    }

    /// Check if item intersects viewport
    fn item_intersects_viewport(&self, item: &DisplayItem, viewport: Rect) -> bool {
        match item {
            DisplayItem::FillRect(item) => viewport.intersects(&item.rect),
            DisplayItem::StrokeRect(item) => viewport.intersects(&item.rect),
            DisplayItem::FillRoundedRect(item) => viewport.intersects(&item.rect),
            DisplayItem::StrokeRoundedRect(item) => viewport.intersects(&item.rect),
            DisplayItem::Text(item) => {
                // Approximate text bounds
                // TODO: Use actual glyph bounds
                let text_rect = Rect::new(item.origin, Size::new(100.0, item.font_size));
                viewport.intersects(&text_rect)
            }
            DisplayItem::Image(item) => viewport.intersects(&item.dest_rect),
            DisplayItem::BoxShadow(item) => {
                // Box shadow extends beyond rect by blur + spread
                let extended = item.rect.inflate(item.blur_radius + item.spread_radius);
                viewport.intersects(&extended)
            }
            DisplayItem::LinearGradient(item) => viewport.intersects(&item.rect),
            DisplayItem::RadialGradient(item) => viewport.intersects(&item.rect),

            // Stack operations always included (they affect subsequent items)
            DisplayItem::PushClip(_) | DisplayItem::PopClip |
            DisplayItem::PushOpacity(_) | DisplayItem::PopOpacity |
            DisplayItem::PushTransform(_) | DisplayItem::PopTransform |
            DisplayItem::PushBlendMode(_) | DisplayItem::PopBlendMode |
            DisplayItem::PushStackingContext(_) | DisplayItem::PopStackingContext => true,
        }
    }

    /// Compute bounds of all items
    fn compute_bounds(items: &[DisplayItem]) -> Rect {
        let mut bounds = Rect::zero();

        for item in items {
            let item_bounds = match item {
                DisplayItem::FillRect(item) => item.rect,
                DisplayItem::StrokeRect(item) => item.rect,
                DisplayItem::FillRoundedRect(item) => item.rect,
                DisplayItem::StrokeRoundedRect(item) => item.rect,
                DisplayItem::Text(item) => Rect::new(item.origin, Size::new(100.0, item.font_size)),
                DisplayItem::Image(item) => item.dest_rect,
                DisplayItem::BoxShadow(item) => item.rect.inflate(item.blur_radius + item.spread_radius),
                DisplayItem::LinearGradient(item) => item.rect,
                DisplayItem::RadialGradient(item) => item.rect,
                _ => continue,
            };

            bounds = if bounds.is_empty() {
                item_bounds
            } else {
                bounds.union(&item_bounds)
            };
        }

        bounds
    }

    /// Optimize display list
    ///
    /// Performs optimizations like:
    /// - Merging adjacent fills with same color
    /// - Removing fully transparent items
    /// - Collapsing redundant transforms
    pub fn optimize(&mut self) {
        self.remove_transparent_items();
        self.merge_adjacent_fills();
        // TODO: More optimizations
    }

    /// Remove fully transparent items
    fn remove_transparent_items(&mut self) {
        self.items.retain(|item| {
            match item {
                DisplayItem::FillRect(item) => item.color.a > 0,
                DisplayItem::StrokeRect(item) => item.color.a > 0,
                DisplayItem::FillRoundedRect(item) => item.color.a > 0,
                DisplayItem::StrokeRoundedRect(item) => item.color.a > 0,
                DisplayItem::Text(item) => item.color.a > 0,
                DisplayItem::BoxShadow(item) => item.color.a > 0,
                _ => true, // Keep everything else
            }
        });
    }

    /// Merge adjacent fills with same color
    fn merge_adjacent_fills(&mut self) {
        // Simplified: In practice, this is complex due to overlaps
        // For now, just a placeholder
        // TODO: Implement proper merging
    }
}

impl Default for DisplayList {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 3: Implement Display List Builder (Day 2-3)

**File: `src/paint/display_list/builder.rs`**

```rust
//! Display list builder
//!
//! Converts fragment tree to display list.

use super::{DisplayList, DisplayItem, FillRectItem, StrokeRectItem};
use super::{FillRoundedRectItem, BorderRadii, TextItem, BoxShadowItem};
use super::{ClipItem, OpacityItem, TransformItem, Transform2D};
use crate::tree::FragmentNode;
use crate::geometry::{Point, Rect};
use crate::style::{ComputedStyle, Display};

/// Display list builder
///
/// Walks the fragment tree and generates display items.
pub struct DisplayListBuilder {
    /// Current display list being built
    list: DisplayList,

    /// Effect stack
    effect_stack: EffectStack,
}

impl DisplayListBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            list: DisplayList::new(),
            effect_stack: EffectStack::new(),
        }
    }

    /// Build display list from fragment tree
    pub fn build(mut self, root: &FragmentNode) -> DisplayList {
        self.build_fragment(root, Point::zero());
        self.list
    }

    /// Build display items for a fragment
    fn build_fragment(&mut self, fragment: &FragmentNode, offset: Point) {
        let rect = fragment.rect();
        let absolute_rect = rect.translate(offset);

        // Paint order per CSS 2.1 Appendix E:
        // 1. Background color
        // 2. Background image
        // 3. Border
        // 4. Children
        // 5. Outline (not in display list, painted on top)

        let style = fragment.style();

        // 1. Paint background color
        if let Some(bg_color) = style.background_color {
            if bg_color.a > 0 {
                self.paint_background(absolute_rect, bg_color, style);
            }
        }

        // 2. Paint background image
        if let Some(ref bg_image) = style.background_image {
            self.paint_background_image(absolute_rect, bg_image, style);
        }

        // 3. Paint border
        if style.has_border() {
            self.paint_border(absolute_rect, style);
        }

        // Apply effects before painting children
        self.push_effects(style);

        // 4. Paint children
        for child in fragment.children() {
            self.build_fragment(child, absolute_rect.origin);
        }

        // Pop effects
        self.pop_effects(style);

        // Paint text content if any
        if let Some(ref text_content) = fragment.text_content() {
            self.paint_text(absolute_rect.origin, text_content, style);
        }

        // Paint box shadow if specified
        if let Some(ref shadows) = style.box_shadow {
            for shadow in shadows {
                self.paint_box_shadow(absolute_rect, shadow, style);
            }
        }
    }

    /// Paint background
    fn paint_background(
        &mut self,
        rect: Rect,
        color: crate::style::Color,
        style: &ComputedStyle,
    ) {
        // Check for border-radius
        let radii = self.get_border_radii(style);

        if radii.has_radius() {
            // Rounded rectangle
            self.list.push(DisplayItem::FillRoundedRect(FillRoundedRectItem {
                rect,
                color,
                radii,
            }));
        } else {
            // Simple rectangle
            self.list.push(DisplayItem::FillRect(FillRectItem {
                rect,
                color,
            }));
        }
    }

    /// Paint background image
    fn paint_background_image(
        &mut self,
        rect: Rect,
        bg_image: &BackgroundImage,
        style: &ComputedStyle,
    ) {
        // TODO: Implement background image painting
        // This involves:
        // - Loading the image
        // - Positioning according to background-position
        // - Sizing according to background-size
        // - Repeating according to background-repeat
    }

    /// Paint border
    fn paint_border(&mut self, rect: Rect, style: &ComputedStyle) {
        // Simplified border painting
        // In practice, borders are complex (different colors per side, etc.)

        let radii = self.get_border_radii(style);

        if radii.has_radius() {
            // Rounded border
            // For now, paint as 4 separate strokes
            // TODO: Implement proper rounded border rendering

            // Top border
            if style.border_top_width > 0.0 {
                self.list.push(DisplayItem::StrokeRoundedRect(super::StrokeRoundedRectItem {
                    rect,
                    color: style.border_top_color,
                    width: style.border_top_width,
                    radii,
                }));
            }

            // Right border
            if style.border_right_width > 0.0 {
                // TODO: Stroke right side only
            }

            // Bottom border
            if style.border_bottom_width > 0.0 {
                // TODO: Stroke bottom only
            }

            // Left border
            if style.border_left_width > 0.0 {
                // TODO: Stroke left only
            }
        } else {
            // Straight borders
            // Paint each side separately

            // Top border
            if style.border_top_width > 0.0 {
                let top_rect = Rect::new(
                    rect.origin,
                    Size::new(rect.width(), style.border_top_width),
                );
                self.list.push(DisplayItem::FillRect(FillRectItem {
                    rect: top_rect,
                    color: style.border_top_color,
                }));
            }

            // Right border
            if style.border_right_width > 0.0 {
                let right_rect = Rect::new(
                    Point::new(rect.max_x() - style.border_right_width, rect.min_y()),
                    Size::new(style.border_right_width, rect.height()),
                );
                self.list.push(DisplayItem::FillRect(FillRectItem {
                    rect: right_rect,
                    color: style.border_right_color,
                }));
            }

            // Bottom border
            if style.border_bottom_width > 0.0 {
                let bottom_rect = Rect::new(
                    Point::new(rect.min_x(), rect.max_y() - style.border_bottom_width),
                    Size::new(rect.width(), style.border_bottom_width),
                );
                self.list.push(DisplayItem::FillRect(FillRectItem {
                    rect: bottom_rect,
                    color: style.border_bottom_color,
                }));
            }

            // Left border
            if style.border_left_width > 0.0 {
                let left_rect = Rect::new(
                    rect.origin,
                    Size::new(style.border_left_width, rect.height()),
                );
                self.list.push(DisplayItem::FillRect(FillRectItem {
                    rect: left_rect,
                    color: style.border_left_color,
                }));
            }
        }
    }

    /// Paint text
    fn paint_text(
        &mut self,
        origin: Point,
        text_content: &crate::text::ShapedText,
        style: &ComputedStyle,
    ) {
        use std::sync::Arc;

        self.list.push(DisplayItem::Text(TextItem {
            origin,
            text: Arc::new(text_content.clone()),
            color: style.color,
            font_size: style.font_size,
        }));
    }

    /// Paint box shadow
    fn paint_box_shadow(
        &mut self,
        rect: Rect,
        shadow: &BoxShadow,
        style: &ComputedStyle,
    ) {
        let radii = self.get_border_radii(style);

        self.list.push(DisplayItem::BoxShadow(BoxShadowItem {
            rect,
            radii,
            offset: Point::new(shadow.offset_x, shadow.offset_y),
            blur_radius: shadow.blur_radius,
            spread_radius: shadow.spread_radius,
            color: shadow.color,
            inset: shadow.inset,
        }));
    }

    /// Get border radii from style
    fn get_border_radii(&self, style: &ComputedStyle) -> BorderRadii {
        BorderRadii {
            top_left: style.border_top_left_radius,
            top_right: style.border_top_right_radius,
            bottom_right: style.border_bottom_right_radius,
            bottom_left: style.border_bottom_left_radius,
        }
    }

    /// Push effects from style
    fn push_effects(&mut self, style: &ComputedStyle) {
        // Push opacity
        if style.opacity < 1.0 {
            self.list.push(DisplayItem::PushOpacity(OpacityItem {
                opacity: style.opacity,
            }));
            self.effect_stack.push_opacity(style.opacity);
        }

        // Push transform
        if let Some(transform) = style.transform {
            self.list.push(DisplayItem::PushTransform(TransformItem {
                transform: self.css_transform_to_transform2d(transform),
            }));
            self.effect_stack.push_transform(transform);
        }

        // Push blend mode
        if style.mix_blend_mode != BlendMode::Normal {
            self.list.push(DisplayItem::PushBlendMode(super::BlendModeItem {
                mode: style.mix_blend_mode,
            }));
            self.effect_stack.push_blend_mode(style.mix_blend_mode);
        }

        // Push clip
        if style.overflow == Overflow::Hidden {
            // Clip to content box
            // TODO: Get actual content box rect
            // self.list.push(DisplayItem::PushClip(ClipItem { ... }));
        }
    }

    /// Pop effects from style
    fn pop_effects(&mut self, style: &ComputedStyle) {
        // Pop in reverse order

        if style.overflow == Overflow::Hidden {
            // self.list.push(DisplayItem::PopClip);
        }

        if style.mix_blend_mode != BlendMode::Normal {
            self.list.push(DisplayItem::PopBlendMode);
            self.effect_stack.pop_blend_mode();
        }

        if style.transform.is_some() {
            self.list.push(DisplayItem::PopTransform);
            self.effect_stack.pop_transform();
        }

        if style.opacity < 1.0 {
            self.list.push(DisplayItem::PopOpacity);
            self.effect_stack.pop_opacity();
        }
    }

    /// Convert CSS transform to Transform2D
    fn css_transform_to_transform2d(&self, transform: CSSTransform) -> Transform2D {
        // Simplified: In practice, CSS transforms are more complex
        // They can be a list of transform functions
        Transform2D::identity()
    }
}

impl Default for DisplayListBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Effect stack
///
/// Tracks the current rendering state (opacity, transforms, etc.)
#[derive(Debug)]
struct EffectStack {
    opacity_stack: Vec<f32>,
    transform_stack: Vec<Transform2D>,
    blend_mode_stack: Vec<super::BlendMode>,
}

impl EffectStack {
    fn new() -> Self {
        Self {
            opacity_stack: Vec::new(),
            transform_stack: Vec::new(),
            blend_mode_stack: Vec::new(),
        }
    }

    fn push_opacity(&mut self, opacity: f32) {
        self.opacity_stack.push(opacity);
    }

    fn pop_opacity(&mut self) {
        self.opacity_stack.pop();
    }

    fn current_opacity(&self) -> f32 {
        self.opacity_stack.last().copied().unwrap_or(1.0)
    }

    fn push_transform(&mut self, transform: Transform2D) {
        self.transform_stack.push(transform);
    }

    fn pop_transform(&mut self) {
        self.transform_stack.pop();
    }

    fn current_transform(&self) -> Transform2D {
        self.transform_stack.last().copied().unwrap_or_else(Transform2D::identity)
    }

    fn push_blend_mode(&mut self, mode: super::BlendMode) {
        self.blend_mode_stack.push(mode);
    }

    fn pop_blend_mode(&mut self) {
        self.blend_mode_stack.pop();
    }

    fn current_blend_mode(&self) -> super::BlendMode {
        self.blend_mode_stack.last().copied().unwrap_or(super::BlendMode::Normal)
    }
}

// Placeholder types (should be defined elsewhere)
use super::BlendMode;

struct BackgroundImage;
struct BoxShadow {
    offset_x: f32,
    offset_y: f32,
    blur_radius: f32,
    spread_radius: f32,
    color: crate::style::Color,
    inset: bool,
}

struct CSSTransform;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Overflow {
    Visible,
    Hidden,
    Scroll,
    Auto,
}
```

### Step 4: Comprehensive Tests (Day 4-5)

**File: `tests/paint/display_list_test.rs`**

```rust
//! Tests for display list construction

use fastrender::paint::display_list::*;
use fastrender::tree::FragmentNode;
use fastrender::geometry::{Point, Size, Rect};
use fastrender::style::{ComputedStyle, Color};

#[test]
fn test_empty_display_list() {
    let list = DisplayList::new();
    assert_eq!(list.items().len(), 0);
    assert!(list.bounds().is_empty());
}

#[test]
fn test_single_fill_rect() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::new(Point::new(10.0, 10.0), Size::new(100.0, 50.0)),
        color: Color::rgb(255, 0, 0),
    }));

    assert_eq!(list.items().len(), 1);
}

#[test]
fn test_display_list_culling() {
    let mut list = DisplayList::new();

    // Item inside viewport
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::new(Point::new(10.0, 10.0), Size::new(100.0, 50.0)),
        color: Color::rgb(255, 0, 0),
    }));

    // Item outside viewport
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::new(Point::new(1000.0, 1000.0), Size::new(100.0, 50.0)),
        color: Color::rgb(0, 255, 0),
    }));

    // Item partially in viewport
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::new(Point::new(50.0, 50.0), Size::new(100.0, 50.0)),
        color: Color::rgb(0, 0, 255),
    }));

    // Cull to viewport
    let viewport = Rect::new(Point::zero(), Size::new(200.0, 200.0));
    let culled = list.cull(viewport);

    // Should include 2 items (first and third)
    assert_eq!(culled.items().len(), 2);
}

#[test]
fn test_display_list_builder_simple() {
    let fragment = create_simple_fragment();

    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    // Should have background and border items
    assert!(list.items().len() > 0);
}

#[test]
fn test_display_list_builder_with_children() {
    let child1 = create_fragment_with_background(Color::rgb(255, 0, 0));
    let child2 = create_fragment_with_background(Color::rgb(0, 255, 0));

    let parent = FragmentNode::new(
        Rect::new(Point::zero(), Size::new(400.0, 300.0)),
        default_style(),
        vec![child1, child2],
    );

    let builder = DisplayListBuilder::new();
    let list = builder.build(&parent);

    // Should have items for parent and both children
    assert!(list.items().len() >= 2);
}

#[test]
fn test_border_radii() {
    let radii = BorderRadii::uniform(10.0);
    assert_eq!(radii.top_left, 10.0);
    assert_eq!(radii.top_right, 10.0);
    assert_eq!(radii.bottom_right, 10.0);
    assert_eq!(radii.bottom_left, 10.0);
    assert!(radii.has_radius());
}

#[test]
fn test_transform_identity() {
    let t = Transform2D::identity();
    let p = Point::new(10.0, 20.0);
    let transformed = t.transform_point(p);

    assert_eq!(transformed.x, 10.0);
    assert_eq!(transformed.y, 20.0);
}

#[test]
fn test_transform_translate() {
    let t = Transform2D::translate(5.0, 10.0);
    let p = Point::new(10.0, 20.0);
    let transformed = t.transform_point(p);

    assert_eq!(transformed.x, 15.0);
    assert_eq!(transformed.y, 30.0);
}

#[test]
fn test_transform_scale() {
    let t = Transform2D::scale(2.0, 3.0);
    let p = Point::new(10.0, 20.0);
    let transformed = t.transform_point(p);

    assert_eq!(transformed.x, 20.0);
    assert_eq!(transformed.y, 60.0);
}

#[test]
fn test_transform_multiply() {
    let t1 = Transform2D::translate(10.0, 20.0);
    let t2 = Transform2D::scale(2.0, 2.0);
    let combined = t1.multiply(&t2);

    let p = Point::new(5.0, 5.0);
    let transformed = combined.transform_point(p);

    // Scale then translate: (5*2 + 10, 5*2 + 20) = (20, 30)
    assert_eq!(transformed.x, 20.0);
    assert_eq!(transformed.y, 30.0);
}

#[test]
fn test_opacity_stack() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 0.5 }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::new(Point::zero(), Size::new(100.0, 100.0)),
        color: Color::rgb(255, 0, 0),
    }));
    list.push(DisplayItem::PopOpacity);

    assert_eq!(list.items().len(), 3);
}

// Helper functions
fn create_simple_fragment() -> FragmentNode {
    FragmentNode::new(
        Rect::new(Point::zero(), Size::new(100.0, 100.0)),
        style_with_background(Color::rgb(255, 255, 255)),
        vec![],
    )
}

fn create_fragment_with_background(color: Color) -> FragmentNode {
    FragmentNode::new(
        Rect::new(Point::zero(), Size::new(50.0, 50.0)),
        style_with_background(color),
        vec![],
    )
}

fn default_style() -> ComputedStyle {
    ComputedStyle::default()
}

fn style_with_background(color: Color) -> ComputedStyle {
    let mut style = ComputedStyle::default();
    style.background_color = Some(color);
    style
}
```

## Acceptance Criteria

- [ ] Display list can represent all CSS paint primitives (fill, stroke, text, image, shadow, gradient)
- [ ] Display list builder converts fragment tree to display items
- [ ] Paint order follows CSS 2.1 Appendix E (background, border, children)
- [ ] Viewport culling removes off-screen items
- [ ] Effect stack handles opacity, transforms, blend modes correctly
- [ ] Push/Pop operations balance correctly
- [ ] Border-radius rendering works
- [ ] Box shadows render correctly
- [ ] Transform2D math is correct (translate, scale, rotate, multiply)
- [ ] Display list can be serialized/deserialized (for caching)
- [ ] All tests pass: `cargo test display_list`
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Incorrect Paint Order

**Wrong:**
```rust
// Painting border before background
self.paint_border(rect, style);
self.paint_background(rect, style); // Wrong order!
```

**Right:**
```rust
// CSS 2.1 Appendix E order:
self.paint_background(rect, style);  // 1. Background
self.paint_border(rect, style);      // 2. Border
self.paint_children(fragment);       // 3. Children
```

### Pitfall 2: Forgetting to Pop Effects

**Wrong:**
```rust
self.list.push(DisplayItem::PushOpacity(0.5));
// ... paint items ...
// Forgot to pop! Stack imbalance!
```

**Right:**
```rust
self.list.push(DisplayItem::PushOpacity(0.5));
// ... paint items ...
self.list.push(DisplayItem::PopOpacity); // Always pop!
```

### Pitfall 3: Culling Stack Operations

**Wrong:**
```rust
// Removing PushClip because it has no bounds
if item_has_bounds(item) {
    culled_items.push(item); // Wrong! Missing stack ops!
}
```

**Right:**
```rust
// Always include stack operations
if item_has_bounds(item) || item_is_stack_op(item) {
    culled_items.push(item);
}
```

### Pitfall 4: Transform Order

**Wrong:**
```rust
// Multiplying in wrong order
let combined = transform2.multiply(&transform1); // Wrong order!
```

**Right:**
```rust
// Transforms multiply right-to-left (like matrix multiplication)
let combined = transform1.multiply(&transform2);
// Equivalent to: combined = transform1 * transform2
```

## Performance Optimizations

1. **Culling** - Skip items outside viewport (30-50% reduction)
2. **Batching** - Group similar items for GPU (2-3x speedup)
3. **Merging** - Combine adjacent fills (10-20% reduction)
4. **Caching** - Reuse display list across frames (10x speedup)
5. **Early rejection** - Quick bounds checks before detailed intersection

## Integration with Stacking Contexts

The display list builder works with the stacking context system:

1. **Builder creates initial display list** - Items in tree order
2. **Stacking context sorter reorders** - Items grouped by z-index
3. **Final display list** - Correct paint order

See **04-stacking-contexts.md** for details.

## Next Steps

- **04-stacking-contexts.md** - Z-index and paint order sorting
- **04-rasterization.md** - Converting display list to pixels

## References

- **CSS 2.1 Appendix E:** Elaborate description of Stacking Contexts
  - https://www.w3.org/TR/CSS21/zindex.html
- **CSS 2.2 Section 9.9:** Layered presentation
  - https://www.w3.org/TR/CSS22/visuren.html#z-index
- **WebRender Display List:** Mozilla's GPU renderer
  - https://github.com/servo/webrender
- **Servo Display List:** Servo's display list implementation
  - https://github.com/servo/servo

---

**Last Updated:** 2025-11-19
**Status:** Ready for Implementation
