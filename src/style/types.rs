//! Style type definitions
//!
//! This module contains all the enum types used in computed styles.
//! These types represent CSS property values that can be applied to elements.

use crate::css::types::ColorStop;
use crate::style::values::Length;
pub use crate::text::hyphenation::HyphensMode;

/// Text direction
///
/// CSS: `direction`
/// Reference: CSS Writing Modes Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Ltr,
    Rtl,
}

/// Controls bidi embedding/override behavior
///
/// CSS: `unicode-bidi`
/// Reference: CSS Writing Modes Level 3, CSS2.1 9.10
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnicodeBidi {
    Normal,
    Embed,
    BidiOverride,
    Isolate,
    IsolateOverride,
    Plaintext,
}

/// Overflow behavior for content that exceeds container bounds
///
/// CSS: `overflow-x`, `overflow-y`, `overflow`
/// Reference: CSS Overflow Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Overflow {
    Visible,
    Hidden,
    Scroll,
    Auto,
}

/// Border collapsing model for tables
///
/// CSS 2.1 §17.6.1: initial value is `separate`
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BorderCollapse {
    Separate,
    Collapse,
}

/// Table layout algorithm selection
///
/// CSS: `table-layout`
/// Reference: CSS 2.1 §17.5.2
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TableLayout {
    Auto,
    Fixed,
}

/// Border line style
///
/// CSS: `border-style`, `border-*-style`
/// Reference: CSS Backgrounds and Borders Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BorderStyle {
    None,
    Hidden,
    Solid,
    Dashed,
    Dotted,
    Double,
    Groove,
    Ridge,
    Inset,
    Outset,
}

/// Flex container main axis direction
///
/// CSS: `flex-direction`
/// Reference: CSS Flexible Box Layout Module Level 1
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlexDirection {
    Row,
    RowReverse,
    Column,
    ColumnReverse,
}

/// Flex item wrapping behavior
///
/// CSS: `flex-wrap`
/// Reference: CSS Flexible Box Layout Module Level 1
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlexWrap {
    NoWrap,
    Wrap,
    WrapReverse,
}

/// How replaced content is resized within its box
///
/// CSS: `object-fit`
/// Reference: CSS Images Module Level 4
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjectFit {
    Fill,
    Contain,
    Cover,
    None,
    ScaleDown,
}

/// Logical alignment for object-position
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PositionKeyword {
    Start,
    Center,
    End,
}

/// Position component for object positioning
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PositionComponent {
    Keyword(PositionKeyword),
    Length(Length),
    Percentage(f32),
}

/// Object position along x/y
///
/// CSS: `object-position`
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ObjectPosition {
    pub x: PositionComponent,
    pub y: PositionComponent,
}

/// Mix-blend mode values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MixBlendMode {
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

/// Isolation for stacking contexts
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Isolation {
    Auto,
    Isolate,
}

/// Transform origin for x/y axes
///
/// CSS: `transform-origin`
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TransformOrigin {
    pub x: Length,
    pub y: Length,
}

/// Main axis alignment for flex items
///
/// CSS: `justify-content`
/// Reference: CSS Flexible Box Layout Module Level 1
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JustifyContent {
    FlexStart,
    FlexEnd,
    Center,
    SpaceBetween,
    SpaceAround,
    SpaceEvenly,
}

/// Cross axis alignment for flex items
///
/// CSS: `align-items`
/// Reference: CSS Flexible Box Layout Module Level 1
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlignItems {
    FlexStart,
    FlexEnd,
    Center,
    Baseline,
    Stretch,
}

/// Multi-line cross axis alignment
///
/// CSS: `align-content`
/// Reference: CSS Flexible Box Layout Module Level 1
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlignContent {
    FlexStart,
    FlexEnd,
    Center,
    SpaceBetween,
    SpaceAround,
    Stretch,
}

/// Flex item initial main size
///
/// CSS: `flex-basis`
/// Reference: CSS Flexible Box Layout Module Level 1
#[derive(Debug, Clone, PartialEq)]
pub enum FlexBasis {
    Auto,
    Length(Length),
}

/// Grid track size specification
///
/// CSS: `grid-template-columns`, `grid-template-rows`
/// Reference: CSS Grid Layout Module Level 1
#[derive(Debug, Clone, PartialEq)]
pub enum GridTrack {
    Length(Length),
    Fr(f32),
    Auto,
    MinMax(Box<GridTrack>, Box<GridTrack>),
}

/// Font weight
///
/// CSS: `font-weight`
/// Reference: CSS Fonts Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FontWeight {
    Normal,
    Bold,
    Bolder,
    Lighter,
    Number(u16),
}

impl FontWeight {
    /// Converts font weight to numeric u16 value (100-900)
    pub fn to_u16(self) -> u16 {
        match self {
            FontWeight::Normal => 400,
            FontWeight::Bold => 700,
            FontWeight::Bolder => 700,  // Simplified - should be relative to parent
            FontWeight::Lighter => 300, // Simplified - should be relative to parent
            FontWeight::Number(n) => n,
        }
    }
}

/// Font style (normal, italic, oblique)
///
/// CSS: `font-style`
/// Reference: CSS Fonts Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FontStyle {
    Normal,
    Italic,
    Oblique,
}

/// Line height specification
///
/// CSS: `line-height`
/// Reference: CSS 2.1 Section 10.8
#[derive(Debug, Clone, PartialEq)]
pub enum LineHeight {
    Normal,
    Number(f32),
    Length(Length),
}

/// Vertical alignment
///
/// CSS: `vertical-align`
/// Reference: CSS 2.1 §10.8.1
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum VerticalAlign {
    /// Align baseline with parent's baseline (initial value)
    #[default]
    Baseline,
    /// Lower baseline to parent's subscript position
    Sub,
    /// Raise baseline to parent's superscript position
    Super,
    /// Align box top with the parent's text-top edge
    TextTop,
    /// Align box bottom with the parent's text-bottom edge
    TextBottom,
    /// Center box within available space
    Middle,
    /// Align box top with container top
    Top,
    /// Align box bottom with container bottom
    Bottom,
    /// Shift baseline by a specific length (positive = up)
    Length(Length),
    /// Shift baseline by a percentage of the line-height
    Percentage(f32),
}

impl VerticalAlign {
    /// Returns true if the value participates in baseline alignment
    pub fn is_baseline_relative(self) -> bool {
        matches!(
            self,
            VerticalAlign::Baseline
                | VerticalAlign::Sub
                | VerticalAlign::Super
                | VerticalAlign::TextTop
                | VerticalAlign::TextBottom
                | VerticalAlign::Length(_)
                | VerticalAlign::Percentage(_)
        )
    }
}

/// Text horizontal alignment
///
/// CSS: `text-align`
/// Reference: CSS Text Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextAlign {
    Left,
    Right,
    Center,
    Justify,
}

/// Text decoration lines
///
/// CSS: `text-decoration`
/// Reference: CSS Text Decoration Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextDecoration {
    None,
    Underline,
    Overline,
    LineThrough,
}

/// Text case transformation
///
/// CSS: `text-transform`
/// Reference: CSS Text Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextTransform {
    None,
    Uppercase,
    Lowercase,
    Capitalize,
}

/// White space handling mode
///
/// CSS: `white-space`
/// Reference: CSS Text Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WhiteSpace {
    Normal,
    Nowrap,
    Pre,
    PreWrap,
    PreLine,
}

/// CSS `word-break`
///
/// Reference: CSS Text Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WordBreak {
    Normal,
    BreakAll,
    KeepAll,
    BreakWord,
}

/// CSS `overflow-wrap` (formerly `word-wrap`)
///
/// Reference: CSS Text Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OverflowWrap {
    Normal,
    BreakWord,
    Anywhere,
}

/// Background image specification
///
/// CSS: `background-image`
/// Reference: CSS Backgrounds and Borders Module Level 3
#[derive(Debug, Clone, PartialEq)]
pub enum BackgroundImage {
    Url(String),
    LinearGradient { angle: f32, stops: Vec<ColorStop> },
    RadialGradient { stops: Vec<ColorStop> },
}

/// Background image sizing
///
/// CSS: `background-size`
/// Reference: CSS Backgrounds and Borders Module Level 3
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BackgroundSize {
    Auto,
    Cover,
    Contain,
    Length(Length, Length),
}

/// Box reference for background painting/positioning
///
/// CSS: `background-origin`, `background-clip`
/// Reference: CSS Backgrounds and Borders Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackgroundBox {
    BorderBox,
    PaddingBox,
    ContentBox,
}

/// Background image positioning
///
/// CSS: `background-position`
/// Reference: CSS Backgrounds and Borders Module Level 3
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BackgroundPosition {
    Center,
    Position(Length, Length),
}

/// Background image repeat mode
///
/// CSS: `background-repeat`
/// Reference: CSS Backgrounds and Borders Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackgroundRepeat {
    Repeat,
    RepeatX,
    RepeatY,
    NoRepeat,
}
