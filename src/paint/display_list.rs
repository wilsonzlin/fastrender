//! Display List Types
//!
//! This module provides the display list intermediate representation for painting.
//! The display list is a flat, ordered list of paint commands that can be
//! efficiently executed by the rasterizer.
//!
//! # Overview
//!
//! The display list sits between layout and rasterization in the rendering pipeline:
//!
//! ```text
//! Fragment Tree → Display List → Rasterization → Pixels
//! ```
//!
//! # Display Items
//!
//! Display items are typed paint commands representing what to draw:
//! - `Rectangle` - Fill a rectangle with solid color
//! - `Text` - Draw shaped text glyphs
//! - `Image` - Draw an image
//! - `BoxShadow` - Draw a box shadow
//! - `LinearGradient` / `RadialGradient` - Draw gradients
//! - Push/Pop operations for effects (opacity, transforms, clips)
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::paint::display_list::{DisplayList, DisplayItem, FillRectItem};
//! use fastrender::{Rect, Point, Size};
//! use fastrender::Rgba;
//!
//! let mut list = DisplayList::new();
//! list.push(DisplayItem::FillRect(FillRectItem {
//!     rect: Rect::from_xywh(10.0, 10.0, 100.0, 50.0),
//!     color: Rgba::RED,
//! }));
//! ```

use crate::geometry::Point;
use crate::geometry::Rect;
use crate::geometry::Size;
use crate::paint::clip_path::ResolvedClipPath;
use crate::paint::homography::Homography;
use crate::paint::optimize::DisplayListOptimizer;
use crate::style::color::Rgba;
use crate::style::types::BackfaceVisibility;
use crate::style::types::BackgroundImage;
use crate::style::types::BackgroundPosition;
use crate::style::types::BackgroundRepeat;
use crate::style::types::BackgroundSize;
use crate::style::types::BorderImageOutset;
use crate::style::types::BorderImageRepeat;
use crate::style::types::BorderImageSlice;
use crate::style::types::BorderImageWidth;
use crate::style::types::BorderStyle as CssBorderStyle;
use crate::style::types::MaskClip;
use crate::style::types::MaskComposite;
use crate::style::types::MaskMode;
use crate::style::types::MaskOrigin;
use crate::style::types::ResolvedTextDecoration;
use crate::style::types::TextEmphasisPosition;
use crate::style::types::TextEmphasisStyle;
use crate::style::types::TransformStyle;
use crate::text::font_db::LoadedFont;
pub use crate::text::font_fallback::FontId;
use std::fmt;
use std::sync::Arc;
use tiny_skia::FilterQuality;
use ttf_parser::Tag;

// ============================================================================
// Display Item Types
// ============================================================================

/// A single display item representing a paint operation
///
/// Display items are the building blocks of the display list. Each item
/// represents one paint operation (draw rectangle, draw text, etc.).
#[derive(Debug, Clone)]
pub enum DisplayItem {
  /// Fill a rectangle with a solid color
  FillRect(FillRectItem),

  /// Stroke a rectangle outline
  StrokeRect(StrokeRectItem),

  /// Outline drawn outside the border box with CSS outline semantics
  Outline(OutlineItem),

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

  /// Draw a list marker (text-based marker)
  ListMarker(ListMarkerItem),

  /// Draw a linear gradient
  LinearGradient(LinearGradientItem),

  /// Draw a radial gradient
  RadialGradient(RadialGradientItem),

  /// Draw a conic gradient
  ConicGradient(ConicGradientItem),

  /// Draw CSS borders with per-side styles
  Border(Box<BorderItem>),

  /// Draw text decorations for an inline fragment
  TextDecoration(TextDecorationItem),

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

impl DisplayItem {
  /// Returns the bounding rectangle of this display item, if applicable
  ///
  /// Stack operations (Push/Pop) return None as they don't have bounds.
  pub fn bounds(&self) -> Option<Rect> {
    match self {
      DisplayItem::FillRect(item) => Some(item.rect),
      DisplayItem::StrokeRect(item) => Some(item.rect.inflate(item.width * 0.5)),
      DisplayItem::FillRoundedRect(item) => Some(item.rect),
      DisplayItem::StrokeRoundedRect(item) => Some(item.rect.inflate(item.width * 0.5)),
      DisplayItem::Outline(item) => Some(item.outer_rect()),
      DisplayItem::Text(item) => Some(text_bounds(item)),
      DisplayItem::Image(item) => Some(item.dest_rect),
      DisplayItem::BoxShadow(item) => {
        if item.inset {
          Some(item.rect)
        } else {
          let blur_outset = item.blur_radius.abs() * 3.0;
          let spread = item.spread_radius;
          let shadow_rect = Rect::from_xywh(
            item.rect.x() + item.offset.x - spread,
            item.rect.y() + item.offset.y - spread,
            item.rect.width() + spread * 2.0,
            item.rect.height() + spread * 2.0,
          )
          .inflate(blur_outset);
          Some(shadow_rect)
        }
      }
      DisplayItem::LinearGradient(item) => Some(item.rect),
      DisplayItem::RadialGradient(item) => Some(item.rect),
      DisplayItem::ConicGradient(item) => Some(item.rect),
      DisplayItem::Border(item) => {
        let max_w = item
          .top
          .width
          .max(item.right.width)
          .max(item.bottom.width)
          .max(item.left.width);
        Some(item.rect.inflate(max_w * 0.5))
      }
      DisplayItem::ListMarker(item) => Some(list_marker_bounds(item)),
      DisplayItem::PushClip(item) => Some(match &item.shape {
        ClipShape::Rect { rect, .. } => *rect,
        ClipShape::Path { path } => path.bounds(),
      }),
      DisplayItem::TextDecoration(item) => Some(item.bounds),
      // Stack operations don't have bounds
      DisplayItem::PopClip
      | DisplayItem::PushOpacity(_)
      | DisplayItem::PopOpacity
      | DisplayItem::PushTransform(_)
      | DisplayItem::PopTransform
      | DisplayItem::PushBlendMode(_)
      | DisplayItem::PopBlendMode
      | DisplayItem::PushStackingContext(_)
      | DisplayItem::PopStackingContext => None,
    }
  }

  /// Returns true if this is a stack operation (Push/Pop)
  ///
  /// Stack operations must be preserved during culling to maintain
  /// correct rendering state.
  pub fn is_stack_operation(&self) -> bool {
    matches!(
      self,
      DisplayItem::PushClip(_)
        | DisplayItem::PopClip
        | DisplayItem::PushOpacity(_)
        | DisplayItem::PopOpacity
        | DisplayItem::PushTransform(_)
        | DisplayItem::PopTransform
        | DisplayItem::PushBlendMode(_)
        | DisplayItem::PopBlendMode
        | DisplayItem::PushStackingContext(_)
        | DisplayItem::PopStackingContext
    )
  }
}

// ============================================================================
// Primitive Items
// ============================================================================

/// Fill a rectangle with a solid color
#[derive(Debug, Clone)]
pub struct FillRectItem {
  /// Rectangle to fill
  pub rect: Rect,

  /// Fill color
  pub color: Rgba,
}

/// Stroke a rectangle outline
#[derive(Debug, Clone)]
pub struct StrokeRectItem {
  /// Rectangle to stroke
  pub rect: Rect,

  /// Stroke color
  pub color: Rgba,

  /// Stroke width in pixels
  pub width: f32,

  /// Blend mode for the stroke (defaults to normal)
  pub blend_mode: BlendMode,
}

/// Outline item
#[derive(Debug, Clone)]
pub struct OutlineItem {
  /// Border-rect in CSS px (before offset expansion)
  pub rect: Rect,

  /// Outline width in CSS px
  pub width: f32,

  /// Outline style resolved to a border style
  pub style: CssBorderStyle,

  /// Outline color
  pub color: Rgba,

  /// Outline offset in CSS px
  pub offset: f32,

  /// Whether to invert using difference blend mode
  pub invert: bool,
}

impl OutlineItem {
  pub fn outer_rect(&self) -> Rect {
    let expand = self.offset + self.width * 0.5;
    Rect::from_xywh(
      self.rect.x() - expand,
      self.rect.y() - expand,
      self.rect.width() + 2.0 * expand,
      self.rect.height() + 2.0 * expand,
    )
  }
}

/// Fill a rounded rectangle
#[derive(Debug, Clone)]
pub struct FillRoundedRectItem {
  /// Rectangle bounds
  pub rect: Rect,

  /// Fill color
  pub color: Rgba,

  /// Border radii (top-left, top-right, bottom-right, bottom-left)
  pub radii: BorderRadii,
}

/// Stroke a rounded rectangle
#[derive(Debug, Clone)]
pub struct StrokeRoundedRectItem {
  /// Rectangle bounds
  pub rect: Rect,

  /// Stroke color
  pub color: Rgba,

  /// Stroke width in pixels
  pub width: f32,

  /// Border radii
  pub radii: BorderRadii,
}

/// Border radii for rounded rectangles
///
/// Represents the corner radii for CSS border-radius property.
/// Each corner can have a different radius.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BorderRadius {
  /// Horizontal radius
  pub x: f32,
  /// Vertical radius
  pub y: f32,
}

impl BorderRadius {
  pub const ZERO: Self = Self { x: 0.0, y: 0.0 };

  pub fn uniform(radius: f32) -> Self {
    Self {
      x: radius,
      y: radius,
    }
  }

  pub fn is_zero(&self) -> bool {
    self.x == 0.0 && self.y == 0.0
  }

  pub fn max_component(&self) -> f32 {
    self.x.max(self.y)
  }

  pub fn scale(&self, factor: f32) -> Self {
    Self {
      x: (self.x * factor).max(0.0),
      y: (self.y * factor).max(0.0),
    }
  }

  pub fn shrink(&self, dx: f32, dy: f32) -> Self {
    Self {
      x: (self.x - dx).max(0.0),
      y: (self.y - dy).max(0.0),
    }
  }
}

impl std::ops::Add<f32> for BorderRadius {
  type Output = Self;

  fn add(self, rhs: f32) -> Self::Output {
    Self {
      x: self.x + rhs,
      y: self.y + rhs,
    }
  }
}

impl std::ops::Add for BorderRadius {
  type Output = Self;

  fn add(self, rhs: Self) -> Self::Output {
    Self {
      x: self.x + rhs.x,
      y: self.y + rhs.y,
    }
  }
}

impl std::ops::Sub for BorderRadius {
  type Output = Self;

  fn sub(self, rhs: Self) -> Self::Output {
    Self {
      x: self.x - rhs.x,
      y: self.y - rhs.y,
    }
  }
}

impl std::ops::Mul<f32> for BorderRadius {
  type Output = Self;

  fn mul(self, rhs: f32) -> Self::Output {
    Self {
      x: self.x * rhs,
      y: self.y * rhs,
    }
  }
}

/// Border radii for rounded rectangles (per-corner).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BorderRadii {
  /// Top-left corner radius
  pub top_left: BorderRadius,

  /// Top-right corner radius
  pub top_right: BorderRadius,

  /// Bottom-right corner radius
  pub bottom_right: BorderRadius,

  /// Bottom-left corner radius
  pub bottom_left: BorderRadius,
}

impl BorderRadii {
  /// Zero radii (no rounding)
  pub const ZERO: Self = Self {
    top_left: BorderRadius::ZERO,
    top_right: BorderRadius::ZERO,
    bottom_right: BorderRadius::ZERO,
    bottom_left: BorderRadius::ZERO,
  };

  /// Create uniform border radius (same for all corners)
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// let radii = BorderRadii::uniform(10.0);
  /// assert_eq!(radii.top_left.x, 10.0);
  /// assert_eq!(radii.bottom_right.y, 10.0);
  /// ```
  pub fn uniform(radius: f32) -> Self {
    Self {
      top_left: BorderRadius::uniform(radius),
      top_right: BorderRadius::uniform(radius),
      bottom_right: BorderRadius::uniform(radius),
      bottom_left: BorderRadius::uniform(radius),
    }
  }

  /// Create border radii with individual values for each corner
  pub fn new(
    top_left: BorderRadius,
    top_right: BorderRadius,
    bottom_right: BorderRadius,
    bottom_left: BorderRadius,
  ) -> Self {
    Self {
      top_left,
      top_right,
      bottom_right,
      bottom_left,
    }
  }

  /// Check if any radius is non-zero
  ///
  /// Returns true if at least one corner has a radius > 0.
  pub fn has_radius(&self) -> bool {
    !self.top_left.is_zero()
      || !self.top_right.is_zero()
      || !self.bottom_right.is_zero()
      || !self.bottom_left.is_zero()
  }

  /// Check if all radii are the same
  pub fn is_uniform(&self) -> bool {
    self.top_left == self.top_right
      && self.top_right == self.bottom_right
      && self.bottom_right == self.bottom_left
  }

  /// Get the maximum radius
  pub fn max_radius(&self) -> f32 {
    self
      .top_left
      .max_component()
      .max(self.top_right.max_component())
      .max(self.bottom_right.max_component())
      .max(self.bottom_left.max_component())
  }

  /// Check if all radii are zero
  pub fn is_zero(&self) -> bool {
    !self.has_radius()
  }

  /// Create zero border radii
  pub const fn zero() -> Self {
    Self::ZERO
  }

  /// Clamps radii to prevent overlap
  ///
  /// Per CSS spec, if the sum of any two adjacent radii exceeds
  /// the box dimension, all radii are scaled down proportionally.
  pub fn clamped(self, width: f32, height: f32) -> Self {
    if width <= 0.0 || height <= 0.0 {
      return Self::ZERO;
    }

    let top_sum = self.top_left.x + self.top_right.x;
    let bottom_sum = self.bottom_left.x + self.bottom_right.x;
    let left_sum = self.top_left.y + self.bottom_left.y;
    let right_sum = self.top_right.y + self.bottom_right.y;

    let mut scale_x: f32 = 1.0;
    if top_sum > 0.0 {
      scale_x = scale_x.min(width / top_sum);
    }
    if bottom_sum > 0.0 {
      scale_x = scale_x.min(width / bottom_sum);
    }

    let mut scale_y: f32 = 1.0;
    if left_sum > 0.0 {
      scale_y = scale_y.min(height / left_sum);
    }
    if right_sum > 0.0 {
      scale_y = scale_y.min(height / right_sum);
    }

    if scale_x >= 1.0 && scale_y >= 1.0 {
      return self;
    }

    Self {
      top_left: BorderRadius {
        x: self.top_left.x * scale_x,
        y: self.top_left.y * scale_y,
      },
      top_right: BorderRadius {
        x: self.top_right.x * scale_x,
        y: self.top_right.y * scale_y,
      },
      bottom_right: BorderRadius {
        x: self.bottom_right.x * scale_x,
        y: self.bottom_right.y * scale_y,
      },
      bottom_left: BorderRadius {
        x: self.bottom_left.x * scale_x,
        y: self.bottom_left.y * scale_y,
      },
    }
  }

  /// Shrinks radii by a given amount (for inset borders)
  ///
  /// Used when calculating inner radii for borders.
  pub fn shrink(self, amount: f32) -> Self {
    Self {
      top_left: self.top_left.shrink(amount, amount),
      top_right: self.top_right.shrink(amount, amount),
      bottom_right: self.bottom_right.shrink(amount, amount),
      bottom_left: self.bottom_left.shrink(amount, amount),
    }
  }
}

impl Default for BorderRadii {
  fn default() -> Self {
    Self::ZERO
  }
}

// ============================================================================
// Text Item
// ============================================================================

/// Variation axis/value pair for variable fonts.
///
/// Values are stored as raw bits to keep equality/hash semantics stable
/// even when NaNs are involved.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FontVariation {
  pub tag: Tag,
  pub value_bits: u32,
}

impl FontVariation {
  pub fn new(tag: Tag, value: f32) -> Self {
    Self {
      tag,
      value_bits: value.to_bits(),
    }
  }

  pub fn value(&self) -> f32 {
    f32::from_bits(self.value_bits)
  }
}

impl From<crate::text::variations::FontVariation> for FontVariation {
  fn from(v: crate::text::variations::FontVariation) -> Self {
    FontVariation::new(v.tag, v.value)
  }
}

impl From<rustybuzz::Variation> for FontVariation {
  fn from(v: rustybuzz::Variation) -> Self {
    FontVariation::new(v.tag, v.value)
  }
}

/// Draw a text run
///
/// Represents shaped text ready for rendering. The glyphs have already
/// been positioned by the text shaping system.
#[derive(Debug, Clone)]
pub struct TextItem {
  /// Position to draw text (baseline origin)
  pub origin: Point,

  /// Glyph instances with positions
  pub glyphs: Vec<GlyphInstance>,

  /// Text color
  pub color: Rgba,

  /// Selected CPAL palette index for color fonts.
  pub palette_index: u16,

  /// Shadows to paint before the fill
  pub shadows: Vec<TextShadowItem>,

  /// Font size in pixels
  pub font_size: f32,

  /// Total advance width of the text run
  pub advance_width: f32,

  /// Exact font bytes used for shaping. Carrying the resolved font keeps rasterization
  /// consistent even if the font database changes between list construction and rendering.
  /// Renderers may fall back to a generic sans-serif when absent.
  pub font: Option<Arc<LoadedFont>>,

  /// Identifier for resolving fonts via fallback chains.
  pub font_id: Option<FontId>,

  /// Active variation coordinates for this run.
  pub variations: Vec<FontVariation>,

  /// Synthetic bold stroke width in pixels (0 = none).
  pub synthetic_bold: f32,

  /// Synthetic oblique shear factor (tan(angle); 0 = none).
  pub synthetic_oblique: f32,

  /// Optional emphasis marks to render for this run.
  pub emphasis: Option<TextEmphasis>,

  /// Decorations to paint for this run's fragment.
  ///
  /// Decorations are emitted as separate display items; this field is unused by text rendering
  /// but kept for forward compatibility.
  #[allow(dead_code)]
  pub decorations: Vec<ResolvedTextDecoration>,
}

impl Default for TextItem {
  fn default() -> Self {
    Self {
      origin: Point::new(0.0, 0.0),
      glyphs: Vec::new(),
      color: Rgba::default(),
      palette_index: 0,
      shadows: Vec::new(),
      font_size: 0.0,
      advance_width: 0.0,
      font: None,
      font_id: None,
      variations: Vec::new(),
      synthetic_bold: 0.0,
      synthetic_oblique: 0.0,
      emphasis: None,
      decorations: Vec::new(),
    }
  }
}

/// A single glyph instance for rendering
#[derive(Debug, Clone)]
pub struct GlyphInstance {
  /// Glyph index in the font
  pub glyph_id: u32,

  /// Position offset from text origin
  pub offset: Point,

  /// Advance width to next glyph
  pub advance: f32,
}

/// Emphasis mark to render relative to the text run.
#[derive(Debug, Clone)]
pub struct EmphasisMark {
  pub center: Point,
}

/// Shaped emphasis string glyphs (for string emphasis styles).
#[derive(Debug, Clone)]
pub struct EmphasisText {
  pub glyphs: Vec<GlyphInstance>,
  pub font: Option<Arc<LoadedFont>>,
  pub font_id: Option<FontId>,
  pub font_size: f32,
  pub width: f32,
  pub height: f32,
  pub baseline_offset: f32,
  pub variations: Vec<FontVariation>,
  pub palette_index: u16,
}

/// Resolved emphasis data for a text run.
#[derive(Debug, Clone)]
pub struct TextEmphasis {
  pub style: TextEmphasisStyle,
  pub color: Rgba,
  pub position: TextEmphasisPosition,
  pub size: f32,
  pub marks: Vec<EmphasisMark>,
  pub inline_vertical: bool,
  pub text: Option<EmphasisText>,
}

/// A resolved text shadow ready for painting
#[derive(Debug, Clone)]
pub struct TextShadowItem {
  pub offset: Point,
  pub blur_radius: f32,
  pub color: Rgba,
}

/// A resolved text decoration set to paint over a fragment.
#[derive(Debug, Clone)]
pub struct TextDecorationItem {
  /// Bounding box for culling/optimization
  pub bounds: Rect,
  /// Line start position for decorations
  pub line_start: f32,
  /// Total inline length available for the decoration
  pub line_width: f32,
  /// Whether the inline axis is vertical (lines run vertically)
  pub inline_vertical: bool,
  /// Decorations to paint
  pub decorations: Vec<DecorationPaint>,
}

/// Paint data for a single resolved decoration (underline/overline/line-through).
#[derive(Debug, Clone)]
pub struct DecorationPaint {
  pub style: crate::style::types::TextDecorationStyle,
  pub color: Rgba,
  pub underline: Option<DecorationStroke>,
  pub overline: Option<DecorationStroke>,
  pub line_through: Option<DecorationStroke>,
}

/// Geometry for one decoration stroke.
#[derive(Debug, Clone)]
pub struct DecorationStroke {
  pub center: f32,
  pub thickness: f32,
  /// Optional carved segments for underline skip-ink handling.
  pub segments: Option<Vec<(f32, f32)>>,
}

/// Conservative bounds for a text item using glyph offsets and font size.
pub fn text_bounds(item: &TextItem) -> Rect {
  let mut min_x = item.origin.x;
  let mut max_x = item.origin.x + item.advance_width;
  for glyph in &item.glyphs {
    let gx = item.origin.x + glyph.offset.x;
    min_x = min_x.min(gx);
    max_x = max_x.max(gx + glyph.advance);
  }
  // Assume glyph outlines extend roughly one font-size above the baseline and a quarter below.
  let ascent = item.font_size;
  let descent = item.font_size * 0.25;
  Rect::from_xywh(
    min_x,
    item.origin.y - ascent,
    (max_x - min_x).max(0.0),
    ascent + descent,
  )
}

/// Conservative bounds for a list marker item.
pub fn list_marker_bounds(item: &ListMarkerItem) -> Rect {
  let mut min_x = item.origin.x;
  let mut max_x = item.origin.x + item.advance_width;
  for glyph in &item.glyphs {
    let gx = item.origin.x + glyph.offset.x;
    min_x = min_x.min(gx);
    max_x = max_x.max(gx + glyph.advance);
  }

  let ascent = item.font_size;
  let descent = item.font_size * 0.25;
  Rect::from_xywh(
    min_x,
    item.origin.y - ascent,
    (max_x - min_x).max(0.0),
    ascent + descent,
  )
}

/// List marker paint item
#[derive(Debug, Clone)]
pub struct ListMarkerItem {
  /// Origin in CSS px (baseline-aligned)
  pub origin: Point,

  /// Shaped glyphs for the marker text
  pub glyphs: Vec<GlyphInstance>,

  /// Text color
  pub color: Rgba,

  /// Selected CPAL palette index for color fonts.
  pub palette_index: u16,

  /// Text shadows applied to the marker
  pub shadows: Vec<TextShadowItem>,

  /// Font size in CSS px
  pub font_size: f32,

  /// Total advance width for the marker run
  pub advance_width: f32,

  /// Exact font bytes used for shaping. When not provided, renderers may choose a generic
  /// fallback to keep markers visible.
  pub font: Option<Arc<LoadedFont>>,

  /// Identifier for resolving fonts via fallback chains.
  pub font_id: Option<FontId>,

  /// Active variation coordinates for this run.
  pub variations: Vec<FontVariation>,

  /// Synthetic bold stroke width in CSS px (0 = none)
  pub synthetic_bold: f32,

  /// Synthetic oblique shear factor (tan(angle); 0 = none)
  pub synthetic_oblique: f32,

  /// Optional text emphasis marks to render over the marker
  pub emphasis: Option<TextEmphasis>,

  /// Optional background behind the marker (CSS px)
  pub background: Option<Rgba>,
}

impl Default for ListMarkerItem {
  fn default() -> Self {
    Self {
      origin: Point::new(0.0, 0.0),
      glyphs: Vec::new(),
      color: Rgba::default(),
      palette_index: 0,
      shadows: Vec::new(),
      font_size: 0.0,
      advance_width: 0.0,
      font: None,
      font_id: None,
      variations: Vec::new(),
      synthetic_bold: 0.0,
      synthetic_oblique: 0.0,
      emphasis: None,
      background: None,
    }
  }
}

// ============================================================================
// Image Item
// ============================================================================

/// Draw an image
#[derive(Debug, Clone)]
pub struct ImageItem {
  /// Destination rectangle (where to draw)
  pub dest_rect: Rect,

  /// Image data
  pub image: Arc<ImageData>,

  /// Sampling quality to apply when scaling
  pub filter_quality: ImageFilterQuality,

  /// Source rectangle (for sprite sheets, etc.)
  /// If None, uses the entire image
  pub src_rect: Option<Rect>,
}

/// Sampling quality for raster images
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImageFilterQuality {
  Nearest,
  Linear,
}

impl From<ImageFilterQuality> for FilterQuality {
  fn from(q: ImageFilterQuality) -> Self {
    match q {
      ImageFilterQuality::Nearest => FilterQuality::Nearest,
      ImageFilterQuality::Linear => FilterQuality::Bilinear,
    }
  }
}

/// Image data for rendering
#[derive(Debug, Clone)]
pub struct ImageData {
  /// Image width in pixels
  pub width: u32,

  /// Image height in pixels
  pub height: u32,

  /// Natural image width in CSS px after applying image-resolution/orientation
  pub css_width: f32,

  /// Natural image height in CSS px after applying image-resolution/orientation
  pub css_height: f32,

  /// Pixel data in RGBA8 format (4 bytes per pixel)
  pub pixels: Arc<Vec<u8>>,
}

impl ImageData {
  /// Create new image data
  ///
  /// # Arguments
  ///
  /// * `width` - Image width in pixels
  /// * `height` - Image height in pixels
  /// * `css_width` - Natural width in CSS px
  /// * `css_height` - Natural height in CSS px
  /// * `pixels` - Pixel data in RGBA8 format
  pub fn new(width: u32, height: u32, css_width: f32, css_height: f32, pixels: Vec<u8>) -> Self {
    debug_assert_eq!(
      pixels.len(),
      (width * height * 4) as usize,
      "Pixel data size mismatch"
    );
    Self {
      width,
      height,
      css_width,
      css_height,
      pixels: Arc::new(pixels),
    }
  }

  /// Creates image data assuming 1dppx (CSS size equals pixel size).
  pub fn new_pixels(width: u32, height: u32, pixels: Vec<u8>) -> Self {
    Self::new(width, height, width as f32, height as f32, pixels)
  }

  /// Get the size of the image as a Size
  pub fn size(&self) -> Size {
    Size::new(self.width as f32, self.height as f32)
  }

  /// Natural CSS size of the image
  pub fn css_size(&self) -> Size {
    Size::new(self.css_width, self.css_height)
  }
}

// ============================================================================
// Box Shadow Item
// ============================================================================

/// Draw a box shadow
#[derive(Debug, Clone)]
pub struct BoxShadowItem {
  /// Box bounds (the element casting the shadow)
  pub rect: Rect,

  /// Border radii (if rounded)
  pub radii: BorderRadii,

  /// Shadow offset from box
  pub offset: Point,

  /// Blur radius
  pub blur_radius: f32,

  /// Spread radius
  pub spread_radius: f32,

  /// Shadow color
  pub color: Rgba,

  /// Inset shadow (inside the box)?
  pub inset: bool,
}

// ============================================================================
// Gradient Items
// ============================================================================

/// Draw a linear gradient
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

  /// Spread mode for tiling beyond 0..1
  pub spread: GradientSpread,
}

/// Draw a radial gradient
#[derive(Debug, Clone)]
pub struct RadialGradientItem {
  /// Rectangle to fill
  pub rect: Rect,

  /// Gradient center (relative to rect)
  pub center: Point,

  /// Gradient radii on the x/y axes (relative to rect)
  pub radii: Point,

  /// Color stops
  pub stops: Vec<GradientStop>,

  /// Spread mode for tiling beyond 0..1
  pub spread: GradientSpread,
}

/// Draw a conic gradient
#[derive(Debug, Clone)]
pub struct ConicGradientItem {
  /// Rectangle to fill
  pub rect: Rect,

  /// Gradient center (relative to rect)
  pub center: Point,

  /// Start angle in degrees
  pub from_angle: f32,

  /// Color stops
  pub stops: Vec<GradientStop>,

  /// Repeating?
  pub repeating: bool,
}

/// Gradient color stop
#[derive(Debug, Clone)]
pub struct GradientStop {
  /// Position (0.0 to 1.0)
  pub position: f32,

  /// Color at this stop
  pub color: Rgba,
}

/// Spread mode for gradients
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GradientSpread {
  Pad,
  Repeat,
  Reflect,
}

/// CSS border with per-side styles/colors/widths.
#[derive(Debug, Clone)]
pub struct BorderItem {
  /// Border rectangle (outer border box)
  pub rect: Rect,

  /// Top border side
  pub top: BorderSide,

  /// Right border side
  pub right: BorderSide,

  /// Bottom border side
  pub bottom: BorderSide,

  /// Left border side
  pub left: BorderSide,

  /// Optional border-image to draw instead of styled strokes.
  pub image: Option<BorderImageItem>,

  /// Border corner radii (currently informational)
  pub radii: BorderRadii,
}

/// One border side definition.
#[derive(Debug, Clone)]
pub struct BorderSide {
  /// Stroke width in CSS px after resolution
  pub width: f32,

  /// Border style
  pub style: CssBorderStyle,

  /// Border color
  pub color: Rgba,
}

/// Resolved border-image data for rendering.
#[derive(Debug, Clone)]
pub struct BorderImageItem {
  /// The source image: either pre-decoded pixels or a generated background.
  pub source: BorderImageSourceItem,

  /// Slice geometry.
  pub slice: BorderImageSlice,

  /// Target border widths (length or percent).
  pub width: BorderImageWidth,

  /// Border image outset.
  pub outset: BorderImageOutset,

  /// Repeat modes for x/y.
  pub repeat: (BorderImageRepeat, BorderImageRepeat),

  /// Current color for resolving `currentColor` stops.
  pub current_color: Rgba,

  /// Font size at the element for resolving font-relative lengths.
  pub font_size: f32,

  /// Root font size for rem units.
  pub root_font_size: f32,

  /// Viewport used to resolve viewport-relative units.
  pub viewport: Option<(f32, f32)>,
}

/// Border-image source variants.
#[derive(Debug, Clone)]
pub enum BorderImageSourceItem {
  /// Pre-decoded raster pixels.
  Raster(ImageData),

  /// Generated image such as a gradient.
  Generated(Box<BackgroundImage>),
}

// ============================================================================
// Mask Items
// ============================================================================

/// Resolved mask applied to a stacking context.
#[derive(Debug, Clone)]
pub struct ResolvedMask {
  /// Individual mask layers (ordered from closest to the element outward).
  pub layers: Vec<ResolvedMaskLayer>,

  /// Current color for resolving `currentColor` stops.
  pub color: Rgba,

  /// Font size at the element for resolving font-relative lengths.
  pub font_size: f32,

  /// Root font size for rem units.
  pub root_font_size: f32,

  /// Precomputed reference rectangles for mask-origin/clip.
  pub rects: MaskReferenceRects,
}

/// Precomputed mask layer with decoded image.
#[derive(Debug, Clone)]
pub struct ResolvedMaskLayer {
  /// Decoded or generated image for this layer.
  pub image: ResolvedMaskImage,

  /// Repeat behavior along the x/y axes.
  pub repeat: BackgroundRepeat,

  /// Position of the mask image.
  pub position: BackgroundPosition,

  /// Size of the mask image.
  pub size: BackgroundSize,

  /// Which box the image is positioned relative to.
  pub origin: MaskOrigin,

  /// Which box clips the painted mask.
  pub clip: MaskClip,

  /// Interpretation of the mask values.
  pub mode: MaskMode,

  /// How to composite this layer with the accumulated mask.
  pub composite: MaskComposite,
}

/// Decoded mask image payload.
#[derive(Debug, Clone)]
pub enum ResolvedMaskImage {
  /// Raster image decoded from `url(...)`.
  Raster(ImageData),

  /// Generated image such as a gradient.
  Generated(Box<BackgroundImage>),
}

/// Reference rectangles for applying mask origin/clip.
#[derive(Debug, Clone, Copy)]
pub struct MaskReferenceRects {
  pub border: Rect,
  pub padding: Rect,
  pub content: Rect,
}

// ============================================================================
// Effect Items (Push/Pop)
// ============================================================================

/// Clip region
#[derive(Debug, Clone)]
pub struct ClipItem {
  pub shape: ClipShape,
}

#[derive(Debug, Clone)]
pub enum ClipShape {
  Rect {
    rect: Rect,
    radii: Option<BorderRadii>,
  },
  Path {
    path: ResolvedClipPath,
  },
}

/// Opacity layer
#[derive(Debug, Clone)]
pub struct OpacityItem {
  /// Opacity value (0.0 = fully transparent, 1.0 = fully opaque)
  pub opacity: f32,
}

/// Transform
#[derive(Debug, Clone)]
pub struct TransformItem {
  /// Transform matrix (3D, column-major order)
  pub transform: Transform3D,
}

/// 3D transform matrix stored in column-major order
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Transform3D {
  /// Column-major 4x4 matrix
  pub m: [f32; 16],
}

impl Transform3D {
  /// Identity transform (no transformation)
  pub const IDENTITY: Self = Self {
    m: [
      1.0, 0.0, 0.0, 0.0, // column 1
      0.0, 1.0, 0.0, 0.0, // column 2
      0.0, 0.0, 1.0, 0.0, // column 3
      0.0, 0.0, 0.0, 1.0, // column 4
    ],
  };
  /// Minimum absolute w value for projective projections to be considered valid.
  pub const MIN_PROJECTIVE_W: f32 = 1e-3;

  /// Create identity transform
  pub const fn identity() -> Self {
    Self::IDENTITY
  }

  /// Check if this is the identity transform
  pub fn is_identity(&self) -> bool {
    const EPS: f32 = 1e-6;
    self
      .m
      .iter()
      .zip(Self::IDENTITY.m.iter())
      .all(|(a, b)| (a - b).abs() < EPS)
  }

  /// Multiply two transforms (concatenate)
  ///
  /// The result represents applying `other` first, then `self`.
  pub fn multiply(&self, other: &Transform3D) -> Transform3D {
    let mut out = [0.0_f32; 16];
    for row in 0..4 {
      for col in 0..4 {
        out[col * 4 + row] = self.m[0 * 4 + row] * other.m[col * 4 + 0]
          + self.m[1 * 4 + row] * other.m[col * 4 + 1]
          + self.m[2 * 4 + row] * other.m[col * 4 + 2]
          + self.m[3 * 4 + row] * other.m[col * 4 + 3];
      }
    }
    Transform3D { m: out }
  }

  /// Create translation transform
  pub fn translate(x: f32, y: f32, z: f32) -> Self {
    let mut m = Self::IDENTITY.m;
    m[12] = x;
    m[13] = y;
    m[14] = z;
    Self { m }
  }

  /// Create scale transform
  pub fn scale(sx: f32, sy: f32, sz: f32) -> Self {
    let mut m = Self::IDENTITY.m;
    m[0] = sx;
    m[5] = sy;
    m[10] = sz;
    Self { m }
  }

  /// Create rotation transform around the X axis (angle in radians)
  pub fn rotate_x(angle: f32) -> Self {
    let sin = angle.sin();
    let cos = angle.cos();
    let mut m = Self::IDENTITY.m;
    m[5] = cos;
    m[6] = sin;
    m[9] = -sin;
    m[10] = cos;
    Self { m }
  }

  /// Create rotation transform around the Y axis (angle in radians)
  pub fn rotate_y(angle: f32) -> Self {
    let sin = angle.sin();
    let cos = angle.cos();
    let mut m = Self::IDENTITY.m;
    m[0] = cos;
    m[2] = -sin;
    m[8] = sin;
    m[10] = cos;
    Self { m }
  }

  /// Create rotation transform around the Z axis (angle in radians)
  pub fn rotate_z(angle: f32) -> Self {
    let sin = angle.sin();
    let cos = angle.cos();
    let mut m = Self::IDENTITY.m;
    m[0] = cos;
    m[1] = sin;
    m[4] = -sin;
    m[5] = cos;
    Self { m }
  }

  /// Create skew transform along X and Y (angles in radians)
  pub fn skew(ax: f32, ay: f32) -> Self {
    let mut m = Self::IDENTITY.m;
    m[1] = ay.tan();
    m[4] = ax.tan();
    Self { m }
  }

  /// Create perspective transform with the given distance
  pub fn perspective(distance: f32) -> Self {
    let mut m = Self::IDENTITY.m;
    if distance.abs() > f32::EPSILON {
      m[11] = -1.0 / distance;
    }
    Self { m }
  }

  /// Create a transform from a 2D matrix
  pub fn from_2d(t: &Transform2D) -> Self {
    let mut m = Self::IDENTITY.m;
    m[0] = t.a;
    m[1] = t.b;
    m[4] = t.c;
    m[5] = t.d;
    m[12] = t.e;
    m[13] = t.f;
    Self { m }
  }

  /// Transform a point (x, y, z, 1.0). Returns (x, y, z, w).
  pub fn transform_point(&self, x: f32, y: f32, z: f32) -> (f32, f32, f32, f32) {
    let tx = self.m[0] * x + self.m[4] * y + self.m[8] * z + self.m[12];
    let ty = self.m[1] * x + self.m[5] * y + self.m[9] * z + self.m[13];
    let tz = self.m[2] * x + self.m[6] * y + self.m[10] * z + self.m[14];
    let tw = self.m[3] * x + self.m[7] * y + self.m[11] * z + self.m[15];
    (tx, ty, tz, tw)
  }

  /// Project a point on the z=0 plane, returning normalized coordinates if w is valid.
  pub fn project_point_2d(&self, x: f32, y: f32) -> Option<Point> {
    let (tx, ty, _tz, tw) = self.transform_point(x, y, 0.0);
    if !tw.is_finite()
      || tw.abs() < Self::MIN_PROJECTIVE_W
      || tw < 0.0
      || !tx.is_finite()
      || !ty.is_finite()
    {
      return None;
    }
    Some(Point::new(tx / tw, ty / tw))
  }

  /// Project a point in 3D, returning normalized coordinates if w is valid.
  pub fn project_point(&self, x: f32, y: f32, z: f32) -> Option<[f32; 3]> {
    let (tx, ty, tz, tw) = self.transform_point(x, y, z);
    if !tw.is_finite() || tw.abs() < Self::MIN_PROJECTIVE_W || tw < 0.0 {
      return None;
    }
    if !tx.is_finite() || !ty.is_finite() || !tz.is_finite() {
      return None;
    }
    Some([tx / tw, ty / tw, tz / tw])
  }

  /// Transform a direction vector using the linear part of the matrix (ignoring translation)
  pub fn transform_direction(&self, x: f32, y: f32, z: f32) -> [f32; 3] {
    let tx = self.m[0] * x + self.m[4] * y + self.m[8] * z;
    let ty = self.m[1] * x + self.m[5] * y + self.m[9] * z;
    let tz = self.m[2] * x + self.m[6] * y + self.m[10] * z;
    [tx, ty, tz]
  }

  /// Attempt to extract a 2D affine transform if the matrix is 2D-compatible
  pub fn to_2d(&self) -> Option<Transform2D> {
    const EPS: f32 = 1e-6;
    if (self.m[2]).abs() > EPS
      || (self.m[3]).abs() > EPS
      || (self.m[6]).abs() > EPS
      || (self.m[7]).abs() > EPS
      || (self.m[8]).abs() > EPS
      || (self.m[9]).abs() > EPS
      || (self.m[11]).abs() > EPS
      || (self.m[14]).abs() > EPS
      || (self.m[10] - 1.0).abs() > EPS
      || (self.m[15] - 1.0).abs() > EPS
    {
      return None;
    }

    Some(Transform2D {
      a: self.m[0],
      b: self.m[1],
      c: self.m[4],
      d: self.m[5],
      e: self.m[12],
      f: self.m[13],
    })
  }

  /// Approximate this 3D transform as a 2D affine transform by projecting basis vectors
  pub fn approximate_2d_with_validity(&self) -> (Transform2D, bool) {
    let p0 = self.project_point_2d(0.0, 0.0);
    let p1 = self.project_point_2d(1.0, 0.0);
    let p2 = self.project_point_2d(0.0, 1.0);

    let valid = p0.is_some() && p1.is_some() && p2.is_some();

    let p0 = p0.unwrap_or(Point::ZERO);
    let p1 = p1.unwrap_or(Point::new(1.0, 0.0));
    let p2 = p2.unwrap_or(Point::new(0.0, 1.0));

    let dx1 = (p1.x - p0.x, p1.y - p0.y);
    let dx2 = (p2.x - p0.x, p2.y - p0.y);

    (
      Transform2D {
        a: dx1.0,
        b: dx1.1,
        c: dx2.0,
        d: dx2.1,
        e: p0.x,
        f: p0.y,
      },
      valid,
    )
  }

  /// Approximate this 3D transform as a 2D affine transform by projecting basis vectors
  pub fn approximate_2d(&self) -> Transform2D {
    self.approximate_2d_with_validity().0
  }

  /// Transform an axis-aligned rectangle using a 2D projection of this matrix.
  pub fn transform_rect(&self, rect: Rect) -> Rect {
    if let Some(transform_2d) = self.to_2d() {
      return transform_2d.transform_rect(rect);
    }

    if let Some(quad) = self.project_quad(rect) {
      let mut min_x = f32::INFINITY;
      let mut min_y = f32::INFINITY;
      let mut max_x = f32::NEG_INFINITY;
      let mut max_y = f32::NEG_INFINITY;

      for p in quad {
        min_x = min_x.min(p.x);
        min_y = min_y.min(p.y);
        max_x = max_x.max(p.x);
        max_y = max_y.max(p.y);
      }

      return Rect::from_xywh(min_x, min_y, max_x - min_x, max_y - min_y);
    }

    if let Some(projected) = Homography::from_transform3d_z0(self).map_rect_aabb(rect) {
      return projected;
    }

    self.approximate_2d().transform_rect(rect)
  }

  /// Project a rectangle to a quad; returns None if any corner has an invalid w.
  pub fn project_quad(&self, rect: Rect) -> Option<[Point; 4]> {
    let corners = [
      (rect.min_x(), rect.min_y()),
      (rect.max_x(), rect.min_y()),
      (rect.max_x(), rect.max_y()),
      (rect.min_x(), rect.max_y()),
    ];

    let mut projected = [Point::ZERO; 4];
    for (i, (x, y)) in corners.iter().enumerate() {
      projected[i] = self.project_point_2d(*x, *y)?;
    }

    Some(projected)
  }
}

/// Project a rectangle with a 3D transform into a quad on the z=0 plane.
/// Returns None if any corner maps to an invalid w (zero, negative, or non-finite).
pub fn quad_from_transform3d(rect: Rect, transform: &Transform3D) -> Option<[Point; 4]> {
  transform.project_quad(rect)
}

/// 2D affine transform matrix
///
/// Represents a 3x3 matrix in the form:
/// ```text
/// [a c e]
/// [b d f]
/// [0 0 1]
/// ```
///
/// Used for translation, rotation, scaling, and skewing.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Transform2D {
  /// Scale X (m11)
  pub a: f32,
  /// Skew Y (m12)
  pub b: f32,
  /// Skew X (m21)
  pub c: f32,
  /// Scale Y (m22)
  pub d: f32,
  /// Translate X (m31)
  pub e: f32,
  /// Translate Y (m32)
  pub f: f32,
}

impl Transform2D {
  /// Identity transform (no transformation)
  pub const IDENTITY: Self = Self {
    a: 1.0,
    b: 0.0,
    c: 0.0,
    d: 1.0,
    e: 0.0,
    f: 0.0,
  };

  /// Create identity transform
  pub fn identity() -> Self {
    Self::IDENTITY
  }

  /// Create translation transform
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// let t = Transform2D::translate(10.0, 20.0);
  /// let p = t.transform_point(Point::ZERO);
  /// assert_eq!(p, Point::new(10.0, 20.0));
  /// ```
  pub fn translate(x: f32, y: f32) -> Self {
    Self {
      a: 1.0,
      b: 0.0,
      c: 0.0,
      d: 1.0,
      e: x,
      f: y,
    }
  }

  /// Create scale transform
  pub fn scale(sx: f32, sy: f32) -> Self {
    Self {
      a: sx,
      b: 0.0,
      c: 0.0,
      d: sy,
      e: 0.0,
      f: 0.0,
    }
  }

  /// Create uniform scale transform
  pub fn scale_uniform(s: f32) -> Self {
    Self::scale(s, s)
  }

  /// Create rotation transform
  ///
  /// # Arguments
  ///
  /// * `angle` - Rotation angle in radians (positive = clockwise)
  pub fn rotate(angle: f32) -> Self {
    let cos = angle.cos();
    let sin = angle.sin();
    Self {
      a: cos,
      b: sin,
      c: -sin,
      d: cos,
      e: 0.0,
      f: 0.0,
    }
  }

  /// Create skew transform
  ///
  /// # Arguments
  ///
  /// * `ax` - Skew angle in X direction (radians)
  /// * `ay` - Skew angle in Y direction (radians)
  pub fn skew(ax: f32, ay: f32) -> Self {
    Self {
      a: 1.0,
      b: ay.tan(),
      c: ax.tan(),
      d: 1.0,
      e: 0.0,
      f: 0.0,
    }
  }

  /// Multiply two transforms (concatenate)
  ///
  /// The result represents applying `other` first, then `self`.
  /// This is the standard matrix multiplication order.
  ///
  /// # Examples
  ///
  /// ```rust,ignore
  /// let translate = Transform2D::translate(10.0, 0.0);
  /// let scale = Transform2D::scale(2.0, 2.0);
  /// let combined = translate.multiply(&scale);
  /// // Equivalent to: scale first, then translate
  /// ```
  #[allow(clippy::suspicious_operation_groupings)]
  pub fn multiply(&self, other: &Transform2D) -> Transform2D {
    // Standard 2D affine matrix multiplication:
    // [a c e]   [a' c' e']   [a*a'+c*b'  a*c'+c*d'  a*e'+c*f'+e]
    // [b d f] * [b' d' f'] = [b*a'+d*b'  b*c'+d*d'  b*e'+d*f'+f]
    // [0 0 1]   [0  0  1 ]   [0          0          1          ]
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
  ///
  /// Applies this transform to a point and returns the result.
  pub fn transform_point(&self, p: Point) -> Point {
    Point {
      x: self.a * p.x + self.c * p.y + self.e,
      y: self.b * p.x + self.d * p.y + self.f,
    }
  }

  /// Transform a rectangle
  ///
  /// Returns the axis-aligned bounding box of the transformed rectangle.
  /// Note: The result may be larger than the original if rotation is involved.
  pub fn transform_rect(&self, rect: Rect) -> Rect {
    let p1 = self.transform_point(rect.origin);
    let p2 = self.transform_point(Point::new(rect.max_x(), rect.min_y()));
    let p3 = self.transform_point(Point::new(rect.min_x(), rect.max_y()));
    let p4 = self.transform_point(Point::new(rect.max_x(), rect.max_y()));

    let min_x = p1.x.min(p2.x).min(p3.x).min(p4.x);
    let min_y = p1.y.min(p2.y).min(p3.y).min(p4.y);
    let max_x = p1.x.max(p2.x).max(p3.x).max(p4.x);
    let max_y = p1.y.max(p2.y).max(p3.y).max(p4.y);

    Rect::from_xywh(min_x, min_y, max_x - min_x, max_y - min_y)
  }

  /// Check if this is the identity transform
  pub fn is_identity(&self) -> bool {
    *self == Self::IDENTITY
  }

  /// Get the inverse of this transform, if it exists
  ///
  /// Returns None if the transform is not invertible (determinant is zero).
  pub fn inverse(&self) -> Option<Transform2D> {
    let det = self.a * self.d - self.b * self.c;
    if det.abs() < f32::EPSILON {
      return None;
    }

    let inv_det = 1.0 / det;
    Some(Transform2D {
      a: self.d * inv_det,
      b: -self.b * inv_det,
      c: -self.c * inv_det,
      d: self.a * inv_det,
      e: (self.c * self.f - self.d * self.e) * inv_det,
      f: (self.b * self.e - self.a * self.f) * inv_det,
    })
  }
}

impl Default for Transform2D {
  fn default() -> Self {
    Self::IDENTITY
  }
}

/// Blend mode
#[derive(Debug, Clone)]
pub struct BlendModeItem {
  /// Blend mode to apply
  pub mode: BlendMode,
}

/// CSS blend modes
///
/// Defines how colors blend when overlapping.
/// See CSS Compositing and Blending Level 1.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BlendMode {
  /// Normal blending (source over)
  #[default]
  Normal,
  /// Multiply
  Multiply,
  /// Screen
  Screen,
  /// Overlay
  Overlay,
  /// Darken
  Darken,
  /// Lighten
  Lighten,
  /// Color dodge
  ColorDodge,
  /// Color burn
  ColorBurn,
  /// Hard light
  HardLight,
  /// Soft light
  SoftLight,
  /// Difference
  Difference,
  /// Exclusion
  Exclusion,
  /// Hue
  Hue,
  /// Saturation
  Saturation,
  /// Color
  Color,
  /// Luminosity
  Luminosity,
  /// Plus-lighter
  PlusLighter,
  /// Plus-darker (additive darkening)
  PlusDarker,
  /// Hue blend in HSV/HSB space
  HueHsv,
  /// Saturation blend in HSV/HSB space
  SaturationHsv,
  /// Color blend in HSV/HSB space (hue+saturation)
  ColorHsv,
  /// Luminosity blend in HSV/HSB space (value component)
  LuminosityHsv,
  /// Hue blend in OKLCH space
  HueOklch,
  /// Chroma blend in OKLCH space
  ChromaOklch,
  /// Color blend in OKLCH space (hue+chroma)
  ColorOklch,
  /// Luminosity blend in OKLCH space (lightness component)
  LuminosityOklch,
}

/// Stacking context
#[derive(Debug, Clone)]
pub struct StackingContextItem {
  /// Z-index for ordering
  pub z_index: i32,

  /// Whether this element creates a new stacking context
  pub creates_stacking_context: bool,

  /// Bounds of the stacking context
  pub bounds: Rect,

  /// Local plane used for transform reference (e.g., transform-box)
  ///
  /// This is the element's own reference box, without descendant overflow.
  pub plane_rect: Rect,

  /// mix-blend-mode applied when compositing this stacking context
  pub mix_blend_mode: BlendMode,

  /// Whether the stacking context is isolated from backdrop blending
  pub is_isolated: bool,

  /// Optional transform applied to this stacking context
  pub transform: Option<Transform3D>,

  /// Perspective that applies to child contexts only (CSS perspective property)
  pub child_perspective: Option<Transform3D>,

  /// 3D rendering context preservation
  pub transform_style: TransformStyle,

  /// Whether to render when facing away from the viewer
  pub backface_visibility: BackfaceVisibility,

  /// Resolved filter() list applied to this context
  pub filters: Vec<ResolvedFilter>,

  /// Resolved backdrop-filter() list applied behind this context
  pub backdrop_filters: Vec<ResolvedFilter>,

  /// Border radii used for filter/backdrop clipping
  pub radii: BorderRadii,

  /// Optional mask applied to this stacking context
  pub mask: Option<ResolvedMask>,
}

/// Resolved filter functions (after length resolution).
#[derive(Debug, Clone)]
pub enum ResolvedFilter {
  Blur(f32),
  Brightness(f32),
  Contrast(f32),
  Grayscale(f32),
  Sepia(f32),
  Saturate(f32),
  HueRotate(f32),
  Invert(f32),
  Opacity(f32),
  DropShadow {
    offset_x: f32,
    offset_y: f32,
    blur_radius: f32,
    spread: f32,
    color: Rgba,
  },
  SvgFilter(Arc<crate::paint::svg_filter::SvgFilter>),
}

// ============================================================================
// Display List
// ============================================================================

/// A lightweight view over a subset of a display list.
///
/// This avoids cloning display items by keeping a slice of the source list plus
/// the indices of items that should be visited. Any synthetic stack pops needed
/// to balance the view are stored in `tail` and yielded after the indexed items.
#[derive(Clone, Debug)]
pub struct DisplayListView<'a> {
  items: &'a [DisplayItem],
  indices: Vec<usize>,
  tail: Vec<DisplayItem>,
}

impl<'a> DisplayListView<'a> {
  pub fn new(items: &'a [DisplayItem], indices: Vec<usize>, tail: Vec<DisplayItem>) -> Self {
    Self {
      items,
      indices,
      tail,
    }
  }

  pub fn len(&self) -> usize {
    self.indices.len() + self.tail.len()
  }

  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  pub fn get(&self, index: usize) -> Option<&DisplayItem> {
    if index < self.indices.len() {
      return self.items.get(self.indices[index]);
    }
    let tail_index = index.checked_sub(self.indices.len())?;
    self.tail.get(tail_index)
  }

  pub fn iter(&'a self) -> impl Iterator<Item = &'a DisplayItem> {
    self
      .indices
      .iter()
      .map(|&idx| &self.items[idx])
      .chain(self.tail.iter())
  }
}

/// Display list - flat list of display items in paint order
///
/// The display list is the intermediate representation between layout
/// and rasterization. It contains all paint operations in the correct
/// order for rendering.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::paint::display_list::{DisplayList, DisplayItem, FillRectItem};
/// use fastrender::Rect;
/// use fastrender::Rgba;
///
/// let mut list = DisplayList::new();
/// list.push(DisplayItem::FillRect(FillRectItem {
///     rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
///     color: Rgba::RED,
/// }));
///
/// assert_eq!(list.len(), 1);
/// ```
#[derive(Debug, Clone)]
pub struct DisplayList {
  /// Display items in paint order
  items: Vec<DisplayItem>,

  /// Cached bounding rectangle of all items
  bounds: Option<Rect>,
}

impl DisplayList {
  /// Create an empty display list
  pub fn new() -> Self {
    Self {
      items: Vec::new(),
      bounds: None,
    }
  }

  /// Create a display list with pre-allocated capacity
  pub fn with_capacity(capacity: usize) -> Self {
    Self {
      items: Vec::with_capacity(capacity),
      bounds: None,
    }
  }

  /// Create a display list from a vector of items
  pub fn from_items(items: Vec<DisplayItem>) -> Self {
    let bounds = Self::compute_bounds(&items);
    Self { items, bounds }
  }

  /// Add a display item to the list
  ///
  /// Items are added in paint order (first added = painted first = behind).
  pub fn push(&mut self, item: DisplayItem) {
    // Invalidate cached bounds
    self.bounds = None;
    self.items.push(item);
  }

  /// Convenience for linear gradients
  pub fn push_linear_gradient(
    &mut self,
    rect: Rect,
    start: Point,
    end: Point,
    stops: Vec<GradientStop>,
    spread: GradientSpread,
  ) {
    self.push(DisplayItem::LinearGradient(LinearGradientItem {
      rect,
      start,
      end,
      stops,
      spread,
    }));
  }

  /// Convenience for radial gradients
  pub fn push_radial_gradient(
    &mut self,
    rect: Rect,
    center: Point,
    radii: Point,
    stops: Vec<GradientStop>,
    spread: GradientSpread,
  ) {
    self.push(DisplayItem::RadialGradient(RadialGradientItem {
      rect,
      center,
      radii,
      stops,
      spread,
    }));
  }

  /// Convenience for conic gradients
  pub fn push_conic_gradient(
    &mut self,
    rect: Rect,
    center: Point,
    from_angle: f32,
    stops: Vec<GradientStop>,
    repeating: bool,
  ) {
    self.push(DisplayItem::ConicGradient(ConicGradientItem {
      rect,
      center,
      from_angle,
      stops,
      repeating,
    }));
  }

  /// Extend the display list with items from an iterator
  pub fn extend(&mut self, items: impl IntoIterator<Item = DisplayItem>) {
    self.bounds = None;
    self.items.extend(items);
  }

  /// Appends another display list onto this one, preserving order and invalidating bounds.
  pub fn append(&mut self, mut other: DisplayList) {
    self.bounds = None;
    self.items.append(&mut other.items);
  }

  /// Get the display items
  pub fn items(&self) -> &[DisplayItem] {
    &self.items
  }

  /// Get mutable access to display items
  pub fn items_mut(&mut self) -> &mut Vec<DisplayItem> {
    self.bounds = None;
    &mut self.items
  }

  /// Get the number of items
  pub fn len(&self) -> usize {
    self.items.len()
  }

  /// Check if the display list is empty
  pub fn is_empty(&self) -> bool {
    self.items.is_empty()
  }

  /// Clear all items
  pub fn clear(&mut self) {
    self.items.clear();
    self.bounds = None;
  }

  /// Get the bounding rectangle of all items
  ///
  /// Computes and caches the minimal rectangle containing all display items.
  pub fn bounds(&mut self) -> Rect {
    if self.bounds.is_none() {
      self.bounds = Self::compute_bounds(&self.items);
    }
    self.bounds.unwrap_or(Rect::ZERO)
  }

  /// Compute bounds of items
  fn compute_bounds(items: &[DisplayItem]) -> Option<Rect> {
    let mut result: Option<Rect> = None;

    for item in items {
      if let Some(item_bounds) = item.bounds() {
        result = Some(match result {
          Some(r) => r.union(item_bounds),
          None => item_bounds,
        });
      }
    }

    result
  }

  /// Create a culled display list containing only items within the viewport
  ///
  /// This is an optimization that removes items completely outside the
  /// visible area. Stack operations are preserved to maintain correct state.
  ///
  /// # Arguments
  ///
  /// * `viewport` - The visible area rectangle
  ///
  /// # Returns
  ///
  /// A new display list with only visible items
  pub fn cull(&self, viewport: Rect) -> DisplayList {
    let mut culled_items = Vec::new();

    for item in &self.items {
      let should_include = match item.bounds() {
        Some(bounds) => viewport.intersects(bounds),
        None => true, // Stack operations always included
      };

      if should_include {
        culled_items.push(item.clone());
      }
    }

    DisplayList::from_items(culled_items)
  }

  /// Create a display list containing only items that intersect the viewport.
  ///
  /// This preserves the necessary stack operations (clips, transforms, opacity)
  /// required for correct rendering even when some items are excluded.
  pub fn intersecting(&self, viewport: Rect) -> DisplayList {
    DisplayListOptimizer::new().intersect(self, viewport)
  }

  /// Optimize the display list
  ///
  /// Performs various optimizations:
  /// - Removes fully transparent items
  /// - Could merge adjacent fills with same color (future)
  /// - Could collapse redundant transforms (future)
  pub fn optimize(&mut self) {
    self.remove_transparent_items();
    // Future: self.merge_adjacent_fills();
    // Future: self.collapse_transforms();
    self.bounds = None;
  }

  /// Remove fully transparent items
  fn remove_transparent_items(&mut self) {
    self.items.retain(|item| match item {
      DisplayItem::FillRect(item) => item.color.a > 0.0,
      DisplayItem::StrokeRect(item) => item.color.a > 0.0,
      DisplayItem::FillRoundedRect(item) => item.color.a > 0.0,
      DisplayItem::StrokeRoundedRect(item) => item.color.a > 0.0,
      DisplayItem::Text(item) => item.color.a > 0.0,
      DisplayItem::BoxShadow(item) => item.color.a > 0.0,
      _ => true, // Keep everything else
    });
  }

  /// Get an iterator over the display items
  pub fn iter(&self) -> impl Iterator<Item = &DisplayItem> {
    self.items.iter()
  }
}

impl Default for DisplayList {
  fn default() -> Self {
    Self::new()
  }
}

impl fmt::Display for DisplayList {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "DisplayList({} items)", self.items.len())
  }
}

// ============================================================================
// Unit Tests
// ============================================================================

#[cfg(test)]
mod tests {
  use super::*;

  // ========================================================================
  // BorderRadii Tests
  // ========================================================================

  #[test]
  fn test_border_radii_zero() {
    let radii = BorderRadii::ZERO;
    assert_eq!(radii.top_left, BorderRadius::ZERO);
    assert_eq!(radii.top_right, BorderRadius::ZERO);
    assert_eq!(radii.bottom_right, BorderRadius::ZERO);
    assert_eq!(radii.bottom_left, BorderRadius::ZERO);
    assert!(!radii.has_radius());
  }

  #[test]
  fn test_border_radii_uniform() {
    let radii = BorderRadii::uniform(10.0);
    assert_eq!(radii.top_left, BorderRadius::uniform(10.0));
    assert_eq!(radii.top_right, BorderRadius::uniform(10.0));
    assert_eq!(radii.bottom_right, BorderRadius::uniform(10.0));
    assert_eq!(radii.bottom_left, BorderRadius::uniform(10.0));
    assert!(radii.has_radius());
    assert!(radii.is_uniform());
  }

  #[test]
  fn test_border_radii_individual() {
    let radii = BorderRadii::new(
      BorderRadius::uniform(1.0),
      BorderRadius::uniform(2.0),
      BorderRadius::uniform(3.0),
      BorderRadius::uniform(4.0),
    );
    assert_eq!(radii.top_left, BorderRadius::uniform(1.0));
    assert_eq!(radii.top_right, BorderRadius::uniform(2.0));
    assert_eq!(radii.bottom_right, BorderRadius::uniform(3.0));
    assert_eq!(radii.bottom_left, BorderRadius::uniform(4.0));
    assert!(radii.has_radius());
    assert!(!radii.is_uniform());
    assert_eq!(radii.max_radius(), 4.0);
  }

  #[test]
  fn clamped_radii_scale_axes_independently() {
    let radii = BorderRadii::new(
      BorderRadius { x: 60.0, y: 10.0 },
      BorderRadius { x: 60.0, y: 10.0 },
      BorderRadius { x: 60.0, y: 10.0 },
      BorderRadius { x: 60.0, y: 10.0 },
    );
    // Width forces horizontal radii to shrink; height is ample so vertical radii remain.
    let clamped = radii.clamped(100.0, 200.0);
    assert!(clamped.top_left.x < 60.0);
    assert!((clamped.top_left.y - 10.0).abs() < 1e-6);
  }

  // ========================================================================
  // Transform2D Tests
  // ========================================================================

  #[test]
  fn test_transform_identity() {
    let t = Transform2D::identity();
    assert!(t.is_identity());
    let p = Point::new(10.0, 20.0);
    let transformed = t.transform_point(p);
    assert_eq!(transformed, p);
  }

  #[test]
  fn test_transform_translate() {
    let t = Transform2D::translate(5.0, 10.0);
    let p = Point::new(10.0, 20.0);
    let transformed = t.transform_point(p);
    assert_eq!(transformed, Point::new(15.0, 30.0));
  }

  #[test]
  fn test_transform_scale() {
    let t = Transform2D::scale(2.0, 3.0);
    let p = Point::new(10.0, 20.0);
    let transformed = t.transform_point(p);
    assert_eq!(transformed, Point::new(20.0, 60.0));
  }

  #[test]
  fn test_transform_scale_uniform() {
    let t = Transform2D::scale_uniform(2.0);
    let p = Point::new(10.0, 20.0);
    let transformed = t.transform_point(p);
    assert_eq!(transformed, Point::new(20.0, 40.0));
  }

  #[test]
  fn test_transform_rotate_90() {
    let t = Transform2D::rotate(std::f32::consts::FRAC_PI_2);
    let p = Point::new(1.0, 0.0);
    let transformed = t.transform_point(p);
    // After 90 degree rotation, (1, 0) becomes approximately (0, 1)
    assert!((transformed.x - 0.0).abs() < 0.001);
    assert!((transformed.y - 1.0).abs() < 0.001);
  }

  #[test]
  fn test_transform_multiply() {
    let t1 = Transform2D::translate(10.0, 20.0);
    let t2 = Transform2D::scale(2.0, 2.0);
    let combined = t1.multiply(&t2);

    let p = Point::new(5.0, 5.0);
    let transformed = combined.transform_point(p);

    // Scale then translate: (5*2 + 10, 5*2 + 20) = (20, 30)
    assert_eq!(transformed, Point::new(20.0, 30.0));
  }

  #[test]
  fn test_transform_inverse() {
    let t = Transform2D::translate(10.0, 20.0);
    let inv = t.inverse().unwrap();
    let _p = Point::new(15.0, 30.0);

    let transformed = t.transform_point(Point::new(5.0, 10.0));
    let back = inv.transform_point(transformed);

    assert!((back.x - 5.0).abs() < 0.001);
    assert!((back.y - 10.0).abs() < 0.001);
  }

  #[test]
  fn test_transform_rect() {
    let t = Transform2D::translate(10.0, 20.0);
    let rect = Rect::from_xywh(0.0, 0.0, 100.0, 50.0);
    let transformed = t.transform_rect(rect);

    assert_eq!(transformed.x(), 10.0);
    assert_eq!(transformed.y(), 20.0);
    assert_eq!(transformed.width(), 100.0);
    assert_eq!(transformed.height(), 50.0);
  }

  // ========================================================================
  // Transform3D Tests
  // ========================================================================

  #[test]
  fn test_transform3d_rect_perspective_bounds_cover_projected_corners() {
    let rect = Rect::from_xywh(30.0, 20.0, 120.0, 90.0);
    let transform =
      Transform3D::perspective(150.0).multiply(&Transform3D::rotate_y(std::f32::consts::FRAC_PI_4));

    let homography_bounds = transform.transform_rect(rect);
    let affine_bounds = transform.approximate_2d().transform_rect(rect);

    assert!(
      (homography_bounds.x() - affine_bounds.x()).abs() > 0.001
        || (homography_bounds.y() - affine_bounds.y()).abs() > 0.001
        || (homography_bounds.width() - affine_bounds.width()).abs() > 0.001
        || (homography_bounds.height() - affine_bounds.height()).abs() > 0.001
    );

    for (x, y) in [
      (rect.min_x(), rect.min_y()),
      (rect.max_x(), rect.min_y()),
      (rect.min_x(), rect.max_y()),
      (rect.max_x(), rect.max_y()),
    ] {
      let (tx, ty, _tz, tw) = transform.transform_point(x, y, 0.0);
      assert!(tw.abs() > 1e-6);
      let projected = Point::new(tx / tw, ty / tw);
      assert!(homography_bounds.contains_point(projected));
    }
  }

  // ========================================================================
  // DisplayList Tests
  // ========================================================================

  #[test]
  fn test_display_list_new() {
    let list = DisplayList::new();
    assert!(list.is_empty());
    assert_eq!(list.len(), 0);
  }

  #[test]
  fn test_display_list_push() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(10.0, 10.0, 100.0, 50.0),
      color: Rgba::RED,
    }));

    assert_eq!(list.len(), 1);
    assert!(!list.is_empty());
  }

  #[test]
  fn test_display_list_bounds() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(10.0, 10.0, 100.0, 50.0),
      color: Rgba::RED,
    }));

    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(50.0, 30.0, 80.0, 40.0),
      color: Rgba::BLUE,
    }));

    let bounds = list.bounds();
    assert_eq!(bounds.x(), 10.0);
    assert_eq!(bounds.y(), 10.0);
    // Union of (10,10,100,50) and (50,30,80,40)
    // Max X: max(10+100, 50+80) = max(110, 130) = 130
    // Max Y: max(10+50, 30+40) = max(60, 70) = 70
    assert_eq!(bounds.width(), 120.0); // 130 - 10
    assert_eq!(bounds.height(), 60.0); // 70 - 10
  }

  #[test]
  fn test_display_list_cull() {
    let mut list = DisplayList::new();

    // Item inside viewport
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(10.0, 10.0, 100.0, 50.0),
      color: Rgba::RED,
    }));

    // Item outside viewport
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(1000.0, 1000.0, 100.0, 50.0),
      color: Rgba::GREEN,
    }));

    // Item partially in viewport
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(150.0, 150.0, 100.0, 50.0),
      color: Rgba::BLUE,
    }));

    let viewport = Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
    let culled = list.cull(viewport);

    // Should include first and third items (inside/partially inside)
    assert_eq!(culled.len(), 2);
  }

  #[test]
  fn test_display_list_cull_preserves_stack_ops() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 0.5 }));
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(1000.0, 1000.0, 100.0, 50.0),
      color: Rgba::RED,
    }));
    list.push(DisplayItem::PopOpacity);

    let viewport = Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
    let culled = list.cull(viewport);

    // Stack ops should be preserved even though fill is outside
    assert_eq!(culled.len(), 2); // PushOpacity + PopOpacity
  }

  #[test]
  fn test_display_list_optimize_removes_transparent() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(10.0, 10.0, 100.0, 50.0),
      color: Rgba::RED,
    }));

    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(50.0, 50.0, 100.0, 50.0),
      color: Rgba::TRANSPARENT,
    }));

    list.optimize();

    assert_eq!(list.len(), 1);
  }

  #[test]
  fn test_display_list_clear() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(10.0, 10.0, 100.0, 50.0),
      color: Rgba::RED,
    }));

    list.clear();
    assert!(list.is_empty());
  }

  // ========================================================================
  // DisplayItem Tests
  // ========================================================================

  #[test]
  fn test_display_item_bounds() {
    let fill = DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(10.0, 20.0, 100.0, 50.0),
      color: Rgba::RED,
    });
    assert_eq!(
      fill.bounds(),
      Some(Rect::from_xywh(10.0, 20.0, 100.0, 50.0))
    );

    let pop = DisplayItem::PopOpacity;
    assert_eq!(pop.bounds(), None);
  }

  #[test]
  fn test_display_item_is_stack_operation() {
    assert!(!DisplayItem::FillRect(FillRectItem {
      rect: Rect::ZERO,
      color: Rgba::RED,
    })
    .is_stack_operation());

    assert!(DisplayItem::PushOpacity(OpacityItem { opacity: 0.5 }).is_stack_operation());
    assert!(DisplayItem::PopOpacity.is_stack_operation());
    assert!(DisplayItem::PushTransform(TransformItem {
      transform: Transform3D::identity()
    })
    .is_stack_operation());
    assert!(DisplayItem::PopTransform.is_stack_operation());
  }

  // ========================================================================
  // ImageData Tests
  // ========================================================================

  #[test]
  fn test_image_data() {
    let pixels = vec![255u8; 100 * 100 * 4];
    let image = ImageData::new(100, 100, 100.0, 100.0, pixels);

    assert_eq!(image.width, 100);
    assert_eq!(image.height, 100);
    assert_eq!(image.size(), Size::new(100.0, 100.0));
    assert_eq!(image.css_size(), Size::new(100.0, 100.0));
  }

  // ========================================================================
  // GradientStop Tests
  // ========================================================================

  #[test]
  fn test_gradient_stop() {
    let stop = GradientStop {
      position: 0.5,
      color: Rgba::RED,
    };

    assert_eq!(stop.position, 0.5);
    assert_eq!(stop.color, Rgba::RED);
  }

  // ========================================================================
  // BlendMode Tests
  // ========================================================================

  #[test]
  fn test_blend_mode_default() {
    let mode = BlendMode::default();
    assert_eq!(mode, BlendMode::Normal);
  }
}
