//! Computed style values
//!
//! This module provides the PositionedStyle struct which contains resolved
//! CSS property values for a single element.
//!
//! # Computed Values
//!
//! Computed values are partially resolved:
//! - Relative units (em, rem) → absolute (px)
//! - inherit keyword → inherited value
//! - Percentages kept (resolved during layout)
//! - Colors fully resolved
//!
//! Reference: CSS Cascading and Inheritance Level 3
//! <https://www.w3.org/TR/css-cascade-3/>
//!
//! # Example
//!
//! ```
//! use fastrender::PositionedStyle;
//!
//! let style = PositionedStyle::default();
//! assert_eq!(style.font_size, 16.0); // Default font size
//! ```

use crate::geometry::EdgeOffsets;
use crate::style::color::{Color, Rgba};
use crate::style::display::Display;
use crate::style::position::Position;
use crate::style::values::{Length, LengthOrAuto};

/// Computed CSS styles for an element
///
/// Contains all resolved CSS property values. Properties are grouped
/// by category for better organization.
///
/// # Memory Layout
///
/// This struct is designed to be wrapped in Arc and shared between
/// BoxNode and fragments. Size is ~200-300 bytes.
///
/// # Property Groups
///
/// - **Box model**: Dimensions, margin, padding, border
/// - **Positioning**: position, offsets, z-index
/// - **Display**: display, visibility, opacity
/// - **Colors**: Text and background colors
/// - **Text**: Font properties, line-height, text-align
/// - **Flexbox**: Flex container and item properties
///
/// # Examples
///
/// ```
/// use fastrender::PositionedStyle;
///
/// let mut style = PositionedStyle::default();
/// // Modify as needed
/// // Then wrap in Arc for sharing
/// let shared = std::sync::Arc::new(style);
/// ```
#[derive(Debug, Clone)]
pub struct PositionedStyle {
    // ===== BOX MODEL =====
    /// Width property
    ///
    /// CSS: `width`
    /// Initial: auto
    /// Percentages: relative to containing block width
    pub width: LengthOrAuto,

    /// Height property
    ///
    /// CSS: `height`
    /// Initial: auto
    /// Percentages: relative to containing block height
    pub height: LengthOrAuto,

    /// Minimum width
    ///
    /// CSS: `min-width`
    /// Initial: 0
    pub min_width: Length,

    /// Maximum width
    ///
    /// CSS: `max-width`
    /// Initial: none (represented as f32::INFINITY)
    pub max_width: Length,

    /// Minimum height
    ///
    /// CSS: `min-height`
    /// Initial: 0
    pub min_height: Length,

    /// Maximum height
    ///
    /// CSS: `max-height`
    /// Initial: none
    pub max_height: Length,

    /// Margin on all sides
    ///
    /// CSS: `margin-top`, `margin-right`, `margin-bottom`, `margin-left`
    /// Initial: 0
    /// Note: Auto margins are handled during layout
    pub margin: EdgeOffsets,

    /// Padding on all sides
    ///
    /// CSS: `padding-*`
    /// Initial: 0
    /// Note: Padding cannot be auto or negative
    pub padding: EdgeOffsets,

    /// Border width on all sides
    ///
    /// CSS: `border-*-width`
    /// Initial: medium (3px)
    pub border_width: EdgeOffsets,

    /// Border color on all sides
    ///
    /// CSS: `border-*-color`
    /// Initial: currentColor
    pub border_color: BorderColors,

    // ===== POSITIONING =====
    /// Positioning scheme
    ///
    /// CSS: `position`
    /// Initial: static
    pub position: Position,

    /// Top offset for positioned elements
    ///
    /// CSS: `top`
    /// Initial: auto
    pub top: LengthOrAuto,

    /// Right offset
    ///
    /// CSS: `right`
    /// Initial: auto
    pub right: LengthOrAuto,

    /// Bottom offset
    ///
    /// CSS: `bottom`
    /// Initial: auto
    pub bottom: LengthOrAuto,

    /// Left offset
    ///
    /// CSS: `left`
    /// Initial: auto
    pub left: LengthOrAuto,

    /// Z-index for stacking contexts
    ///
    /// CSS: `z-index`
    /// Initial: auto (None)
    pub z_index: Option<i32>,

    // ===== DISPLAY =====
    /// Display type
    ///
    /// CSS: `display`
    /// Initial: inline
    pub display: Display,

    /// Visibility
    ///
    /// CSS: `visibility`
    /// Initial: visible
    pub visibility: Visibility,

    /// Opacity
    ///
    /// CSS: `opacity`
    /// Initial: 1.0
    /// Range: 0.0 to 1.0
    pub opacity: f32,

    // ===== COLORS =====
    /// Text color
    ///
    /// CSS: `color`
    /// Initial: black (ish, depends on UA)
    pub color: Color,

    /// Background color
    ///
    /// CSS: `background-color`
    /// Initial: transparent
    pub background_color: Color,

    // ===== TEXT =====
    /// Font family names
    ///
    /// CSS: `font-family`
    /// Initial: depends on UA
    pub font_family: Vec<String>,

    /// Font size in pixels
    ///
    /// CSS: `font-size`
    /// Initial: 16px (medium)
    /// Note: Already resolved to px at computed value time
    pub font_size: f32,

    /// Font weight
    ///
    /// CSS: `font-weight`
    /// Initial: 400 (normal)
    /// Range: 100-900
    pub font_weight: u16,

    /// Font style
    ///
    /// CSS: `font-style`
    /// Initial: normal
    pub font_style: FontStyle,

    /// Line height
    ///
    /// CSS: `line-height`
    /// Initial: normal (1.2)
    /// Note: Can be length or number
    pub line_height: LineHeight,

    /// Text alignment
    ///
    /// CSS: `text-align`
    /// Initial: start
    pub text_align: TextAlign,

    // ===== FLEXBOX =====
    /// Flex direction
    ///
    /// CSS: `flex-direction`
    /// Initial: row
    pub flex_direction: FlexDirection,

    /// Flex wrap
    ///
    /// CSS: `flex-wrap`
    /// Initial: nowrap
    pub flex_wrap: FlexWrap,

    /// Justify content
    ///
    /// CSS: `justify-content`
    /// Initial: flex-start
    pub justify_content: JustifyContent,

    /// Align items
    ///
    /// CSS: `align-items`
    /// Initial: stretch
    pub align_items: AlignItems,

    /// Flex grow factor
    ///
    /// CSS: `flex-grow`
    /// Initial: 0
    pub flex_grow: f32,

    /// Flex shrink factor
    ///
    /// CSS: `flex-shrink`
    /// Initial: 1
    pub flex_shrink: f32,

    /// Flex basis
    ///
    /// CSS: `flex-basis`
    /// Initial: auto
    pub flex_basis: LengthOrAuto,
}

// Supporting types

/// Border colors for all four sides
///
/// Allows different colors per side
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BorderColors {
    pub top: Color,
    pub right: Color,
    pub bottom: Color,
    pub left: Color,
}

impl BorderColors {
    /// Creates border colors with the same color on all sides
    pub const fn all(color: Color) -> Self {
        Self {
            top: color,
            right: color,
            bottom: color,
            left: color,
        }
    }
}

/// CSS visibility property values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Visible,
    Hidden,
    Collapse,
}

/// CSS font-style property values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FontStyle {
    Normal,
    Italic,
    Oblique,
}

/// CSS line-height property values
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LineHeight {
    /// Normal line height (typically 1.2)
    Normal,
    /// Number (multiplier of font-size)
    Number(f32),
    /// Length in pixels
    Length(f32),
}

/// CSS text-align property values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextAlign {
    Left,
    Right,
    Center,
    Justify,
    Start,
    End,
}

/// CSS flex-direction property values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlexDirection {
    Row,
    RowReverse,
    Column,
    ColumnReverse,
}

/// CSS flex-wrap property values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlexWrap {
    Nowrap,
    Wrap,
    WrapReverse,
}

/// CSS justify-content property values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JustifyContent {
    FlexStart,
    FlexEnd,
    Center,
    SpaceBetween,
    SpaceAround,
    SpaceEvenly,
}

/// CSS align-items property values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlignItems {
    FlexStart,
    FlexEnd,
    Center,
    Baseline,
    Stretch,
}

impl Default for PositionedStyle {
    /// Creates a PositionedStyle with CSS initial values
    ///
    /// This represents the default styling when no CSS is applied.
    /// Values match CSS specifications for initial values.
    fn default() -> Self {
        Self {
            // Box model defaults
            width: LengthOrAuto::Auto,
            height: LengthOrAuto::Auto,
            min_width: Length::px(0.0),
            max_width: Length::px(f32::INFINITY),
            min_height: Length::px(0.0),
            max_height: Length::px(f32::INFINITY),
            margin: EdgeOffsets::ZERO,
            padding: EdgeOffsets::ZERO,
            border_width: EdgeOffsets::all(3.0), // medium = 3px
            border_color: BorderColors::all(Color::Rgba(Rgba::BLACK)),

            // Positioning defaults
            position: Position::Static,
            top: LengthOrAuto::Auto,
            right: LengthOrAuto::Auto,
            bottom: LengthOrAuto::Auto,
            left: LengthOrAuto::Auto,
            z_index: None,

            // Display defaults
            display: Display::Inline,
            visibility: Visibility::Visible,
            opacity: 1.0,

            // Color defaults
            color: Color::Rgba(Rgba::BLACK),
            background_color: Color::Rgba(Rgba::TRANSPARENT),

            // Text defaults
            font_family: vec!["serif".to_string()],
            font_size: 16.0,  // medium = 16px
            font_weight: 400, // normal
            font_style: FontStyle::Normal,
            line_height: LineHeight::Normal,
            text_align: TextAlign::Start,

            // Flexbox defaults
            flex_direction: FlexDirection::Row,
            flex_wrap: FlexWrap::Nowrap,
            justify_content: JustifyContent::FlexStart,
            align_items: AlignItems::Stretch,
            flex_grow: 0.0,
            flex_shrink: 1.0,
            flex_basis: LengthOrAuto::Auto,
        }
    }
}

impl PositionedStyle {
    // === Box Model Helpers ===

    /// Returns the total horizontal margin
    pub fn margin_horizontal(&self) -> f32 {
        self.margin.horizontal()
    }

    /// Returns the total vertical margin
    pub fn margin_vertical(&self) -> f32 {
        self.margin.vertical()
    }

    /// Returns the total horizontal padding
    pub fn padding_horizontal(&self) -> f32 {
        self.padding.horizontal()
    }

    /// Returns the total vertical padding
    pub fn padding_vertical(&self) -> f32 {
        self.padding.vertical()
    }

    /// Returns the total horizontal border width
    pub fn border_width_horizontal(&self) -> f32 {
        self.border_width.horizontal()
    }

    /// Returns the total vertical border width
    pub fn border_width_vertical(&self) -> f32 {
        self.border_width.vertical()
    }

    /// Returns the total horizontal spacing (margin + padding + border)
    ///
    /// Useful for computing content box from border box
    pub fn horizontal_spacing(&self) -> f32 {
        self.margin_horizontal() + self.padding_horizontal() + self.border_width_horizontal()
    }

    /// Returns the total vertical spacing
    pub fn vertical_spacing(&self) -> f32 {
        self.margin_vertical() + self.padding_vertical() + self.border_width_vertical()
    }

    // === Positioning Helpers ===

    /// Returns true if this element is positioned (not static)
    pub fn is_positioned(&self) -> bool {
        self.position.is_positioned()
    }

    /// Returns true if this element is absolutely positioned
    pub fn is_absolutely_positioned(&self) -> bool {
        self.position.is_absolutely_positioned()
    }

    /// Returns true if this element creates a stacking context
    ///
    /// Simplified check - real implementation has more conditions
    pub fn creates_stacking_context(&self) -> bool {
        self.is_positioned() && self.z_index.is_some() || self.opacity < 1.0
    }

    // === Display Helpers ===

    /// Returns true if display is none
    pub fn is_display_none(&self) -> bool {
        self.display.is_none()
    }

    /// Returns true if element is visible
    pub fn is_visible(&self) -> bool {
        !self.is_display_none() && matches!(self.visibility, Visibility::Visible)
    }

    // === Text Helpers ===

    /// Returns true if font weight is bold (>= 700)
    pub fn is_bold(&self) -> bool {
        self.font_weight >= 700
    }

    /// Returns true if font style is italic
    pub fn is_italic(&self) -> bool {
        matches!(self.font_style, FontStyle::Italic)
    }

    /// Returns the computed line height in pixels
    ///
    /// Resolves number and normal to pixel values
    pub fn computed_line_height(&self) -> f32 {
        match self.line_height {
            LineHeight::Normal => self.font_size * 1.2,
            LineHeight::Number(n) => self.font_size * n,
            LineHeight::Length(px) => px,
        }
    }

    // === Flexbox Helpers ===

    /// Returns true if this is a flex container
    pub fn is_flex_container(&self) -> bool {
        matches!(self.display, Display::Flex | Display::InlineFlex)
    }

    /// Returns true if flex direction is horizontal
    pub fn is_flex_row(&self) -> bool {
        matches!(self.flex_direction, FlexDirection::Row | FlexDirection::RowReverse)
    }

    /// Returns true if flex direction is vertical
    pub fn is_flex_column(&self) -> bool {
        !self.is_flex_row()
    }

    /// Creates a builder for constructing test styles
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::PositionedStyle;
    /// use fastrender::Display;
    ///
    /// let style = PositionedStyle::builder()
    ///     .display(Display::Block)
    ///     .font_size(20.0)
    ///     .build();
    ///
    /// assert_eq!(style.display, Display::Block);
    /// assert_eq!(style.font_size, 20.0);
    /// ```
    pub fn builder() -> PositionedStyleBuilder {
        PositionedStyleBuilder::new()
    }
}

/// Builder for PositionedStyle (useful for tests)
pub struct PositionedStyleBuilder {
    style: PositionedStyle,
}

impl PositionedStyleBuilder {
    pub fn new() -> Self {
        Self {
            style: PositionedStyle::default(),
        }
    }

    pub fn display(mut self, display: Display) -> Self {
        self.style.display = display;
        self
    }

    pub fn font_size(mut self, size: f32) -> Self {
        self.style.font_size = size;
        self
    }

    pub fn width(mut self, width: LengthOrAuto) -> Self {
        self.style.width = width;
        self
    }

    pub fn height(mut self, height: LengthOrAuto) -> Self {
        self.style.height = height;
        self
    }

    pub fn color(mut self, color: Color) -> Self {
        self.style.color = color;
        self
    }

    pub fn background_color(mut self, color: Color) -> Self {
        self.style.background_color = color;
        self
    }

    pub fn position(mut self, position: Position) -> Self {
        self.style.position = position;
        self
    }

    pub fn build(self) -> PositionedStyle {
        self.style
    }
}

impl Default for PositionedStyleBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_values() {
        let style = PositionedStyle::default();

        // Box model
        assert!(matches!(style.width, LengthOrAuto::Auto));
        assert!(matches!(style.height, LengthOrAuto::Auto));
        assert_eq!(style.margin, EdgeOffsets::ZERO);
        assert_eq!(style.padding, EdgeOffsets::ZERO);

        // Display
        assert_eq!(style.display, Display::Inline);
        assert_eq!(style.visibility, Visibility::Visible);
        assert_eq!(style.opacity, 1.0);

        // Text
        assert_eq!(style.font_size, 16.0);
        assert_eq!(style.font_weight, 400);
    }

    #[test]
    fn test_spacing_helpers() {
        let mut style = PositionedStyle::default();
        style.margin = EdgeOffsets::all(10.0);
        style.padding = EdgeOffsets::all(5.0);
        style.border_width = EdgeOffsets::all(2.0);

        assert_eq!(style.margin_horizontal(), 20.0);
        assert_eq!(style.padding_horizontal(), 10.0);
        assert_eq!(style.border_width_horizontal(), 4.0);
        assert_eq!(style.horizontal_spacing(), 34.0);
    }

    #[test]
    fn test_is_positioned() {
        let mut style = PositionedStyle::default();
        assert!(!style.is_positioned());

        style.position = Position::Relative;
        assert!(style.is_positioned());

        style.position = Position::Absolute;
        assert!(style.is_absolutely_positioned());
    }

    #[test]
    fn test_is_visible() {
        let mut style = PositionedStyle::default();
        assert!(style.is_visible());

        style.display = Display::None;
        assert!(!style.is_visible());

        style.display = Display::Block;
        style.visibility = Visibility::Hidden;
        assert!(!style.is_visible());
    }

    #[test]
    fn test_text_helpers() {
        let mut style = PositionedStyle::default();
        assert!(!style.is_bold());
        assert!(!style.is_italic());

        style.font_weight = 700;
        assert!(style.is_bold());

        style.font_style = FontStyle::Italic;
        assert!(style.is_italic());
    }

    #[test]
    fn test_computed_line_height() {
        let mut style = PositionedStyle::default();
        style.font_size = 20.0;

        // Normal
        style.line_height = LineHeight::Normal;
        assert_eq!(style.computed_line_height(), 24.0); // 20 * 1.2

        // Number
        style.line_height = LineHeight::Number(1.5);
        assert_eq!(style.computed_line_height(), 30.0); // 20 * 1.5

        // Length
        style.line_height = LineHeight::Length(25.0);
        assert_eq!(style.computed_line_height(), 25.0);
    }

    #[test]
    fn test_flex_helpers() {
        let mut style = PositionedStyle::default();
        assert!(!style.is_flex_container());

        style.display = Display::Flex;
        assert!(style.is_flex_container());

        assert!(style.is_flex_row());
        assert!(!style.is_flex_column());

        style.flex_direction = FlexDirection::Column;
        assert!(style.is_flex_column());
        assert!(!style.is_flex_row());
    }

    #[test]
    fn test_builder() {
        let style = PositionedStyle::builder()
            .display(Display::Block)
            .font_size(20.0)
            .color(Color::Rgba(Rgba::RED))
            .build();

        assert_eq!(style.display, Display::Block);
        assert_eq!(style.font_size, 20.0);
        assert_eq!(style.color, Color::Rgba(Rgba::RED));
    }

    #[test]
    fn test_creates_stacking_context() {
        let mut style = PositionedStyle::default();
        assert!(!style.creates_stacking_context());

        style.position = Position::Relative;
        style.z_index = Some(1);
        assert!(style.creates_stacking_context());

        style = PositionedStyle::default();
        style.opacity = 0.5;
        assert!(style.creates_stacking_context());
    }

    #[test]
    fn test_border_colors() {
        let colors = BorderColors::all(Color::Rgba(Rgba::RED));
        assert_eq!(colors.top, Color::Rgba(Rgba::RED));
        assert_eq!(colors.right, Color::Rgba(Rgba::RED));
        assert_eq!(colors.bottom, Color::Rgba(Rgba::RED));
        assert_eq!(colors.left, Color::Rgba(Rgba::RED));
    }

    #[test]
    fn test_vertical_spacing() {
        let mut style = PositionedStyle::default();
        style.margin = EdgeOffsets::new(10.0, 5.0, 15.0, 5.0);
        style.padding = EdgeOffsets::new(3.0, 2.0, 4.0, 2.0);
        style.border_width = EdgeOffsets::new(1.0, 1.0, 2.0, 1.0);

        assert_eq!(style.margin_vertical(), 25.0); // 10 + 15
        assert_eq!(style.padding_vertical(), 7.0); // 3 + 4
        assert_eq!(style.border_width_vertical(), 3.0); // 1 + 2
        assert_eq!(style.vertical_spacing(), 35.0); // 25 + 7 + 3
    }

    #[test]
    fn test_font_style_enum() {
        let style1 = FontStyle::Normal;
        let style2 = FontStyle::Italic;
        let style3 = FontStyle::Oblique;

        assert_ne!(style1, style2);
        assert_ne!(style2, style3);
    }

    #[test]
    fn test_text_align_enum() {
        let align = TextAlign::Center;
        assert_eq!(align, TextAlign::Center);
        assert_ne!(align, TextAlign::Left);
    }

    #[test]
    fn test_visibility_enum() {
        let vis = Visibility::Visible;
        assert_eq!(vis, Visibility::Visible);
        assert_ne!(vis, Visibility::Hidden);
    }

    #[test]
    fn test_flex_direction_enum() {
        let dir = FlexDirection::Row;
        assert_eq!(dir, FlexDirection::Row);
        assert_ne!(dir, FlexDirection::Column);
    }

    #[test]
    fn test_builder_chaining() {
        let style = PositionedStyle::builder()
            .display(Display::Flex)
            .width(LengthOrAuto::Length(Length::px(100.0)))
            .height(LengthOrAuto::Length(Length::px(50.0)))
            .font_size(14.0)
            .position(Position::Relative)
            .build();

        assert_eq!(style.display, Display::Flex);
        assert_eq!(style.font_size, 14.0);
        assert_eq!(style.position, Position::Relative);
    }
}
