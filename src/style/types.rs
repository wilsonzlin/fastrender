//! Style type definitions
//!
//! This module contains all the enum types used in computed styles.
//! These types represent CSS property values that can be applied to elements.

use crate::css::types::ColorStop;
use crate::style::color::Rgba;
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

/// Color value that can defer to currentcolor
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FilterColor {
    CurrentColor,
    Color(Rgba),
}

/// Shadow parameters for drop-shadow()
#[derive(Debug, Clone, PartialEq)]
pub struct FilterShadow {
    pub offset_x: Length,
    pub offset_y: Length,
    pub blur_radius: Length,
    pub spread: Length,
    pub color: FilterColor,
}

/// CSS filter() functions
#[derive(Debug, Clone, PartialEq)]
pub enum FilterFunction {
    Blur(Length),
    Brightness(f32),
    Contrast(f32),
    Grayscale(f32),
    Sepia(f32),
    Saturate(f32),
    HueRotate(f32), // degrees
    Invert(f32),
    Opacity(f32),
    DropShadow(FilterShadow),
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
    Start,
    End,
    Left,
    Right,
    Center,
    Justify,
    MatchParent,
}

/// CSS `text-align-last`
///
/// Reference: CSS Text Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextAlignLast {
    Auto,
    Start,
    End,
    Left,
    Right,
    Center,
    Justify,
}

/// CSS `text-justify`
///
/// Reference: CSS Text Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextJustify {
    Auto,
    None,
    InterWord,
    InterCharacter,
    Distribute,
}

/// CSS `text-indent`
///
/// Reference: CSS Text Module Level 3
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TextIndent {
    pub length: Length,
    pub hanging: bool,
    pub each_line: bool,
}

impl Default for TextIndent {
    fn default() -> Self {
        Self {
            length: Length::px(0.0),
            hanging: false,
            each_line: false,
        }
    }
}

/// Text decoration lines
///
/// CSS: `text-decoration`
/// Reference: CSS Text Decoration Module Level 3
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TextDecoration {
    pub lines: TextDecorationLine,
    pub style: TextDecorationStyle,
    /// None means currentColor
    pub color: Option<Rgba>,
    pub thickness: TextDecorationThickness,
}

impl Default for TextDecoration {
    fn default() -> Self {
        Self {
            lines: TextDecorationLine::NONE,
            style: TextDecorationStyle::Solid,
            color: None,
            thickness: TextDecorationThickness::Auto,
        }
    }
}

/// Individual text-decoration-line flags
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextDecorationLine(pub u8);

impl TextDecorationLine {
    pub const NONE: Self = Self(0);
    pub const UNDERLINE: Self = Self(1 << 0);
    pub const OVERLINE: Self = Self(1 << 1);
    pub const LINE_THROUGH: Self = Self(1 << 2);

    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 != 0
    }

    pub fn insert(&mut self, other: Self) {
        self.0 |= other.0;
    }

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
}

/// Stroke style for text decorations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextDecorationStyle {
    Solid,
    Double,
    Dotted,
    Dashed,
    Wavy,
}

/// Thickness of text decorations
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TextDecorationThickness {
    Auto,
    FromFont,
    Length(Length),
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
    BreakSpaces,
}

/// Tab stop sizing
///
/// CSS: `tab-size`
/// Reference: CSS Text Module Level 3
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TabSize {
    /// Width expressed as a number of space advances
    Number(f32),
    /// Explicit length for each tab stop interval
    Length(Length),
}

impl Default for TabSize {
    fn default() -> Self {
        TabSize::Number(8.0)
    }
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
    RepeatingLinearGradient { angle: f32, stops: Vec<ColorStop> },
    RepeatingRadialGradient { stops: Vec<ColorStop> },
}

/// Background sizing keywords
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackgroundSizeKeyword {
    Cover,
    Contain,
}

/// Background sizing component (per axis)
///
/// CSS: `background-size`
/// Reference: CSS Backgrounds and Borders Module Level 3
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BackgroundSizeComponent {
    Auto,
    Length(Length),
}

/// Background image sizing
///
/// CSS: `background-size`
/// Reference: CSS Backgrounds and Borders Module Level 3
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BackgroundSize {
    Keyword(BackgroundSizeKeyword),
    Explicit(BackgroundSizeComponent, BackgroundSizeComponent),
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

/// Background position component with alignment (percentage of available space)
/// and an offset applied after alignment.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BackgroundPositionComponent {
    /// Alignment fraction in the range [0,1] (e.g., 0 = start, 0.5 = center, 1 = end)
    pub alignment: f32,
    /// Offset applied after alignment; percentages resolve against the remaining space.
    pub offset: Length,
}

/// Background image positioning
///
/// CSS: `background-position`
/// Reference: CSS Backgrounds and Borders Module Level 3
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BackgroundPosition {
    Position {
        x: BackgroundPositionComponent,
        y: BackgroundPositionComponent,
    },
}

/// Background attachment behavior
///
/// CSS: `background-attachment`
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackgroundAttachment {
    Scroll,
    Fixed,
    Local,
}

/// Background image repeat mode
///
/// CSS: `background-repeat`
/// Reference: CSS Backgrounds and Borders Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackgroundRepeatKeyword {
    Repeat,
    Space,
    Round,
    NoRepeat,
}

/// Per-axis repeat keywords (x then y)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BackgroundRepeat {
    pub x: BackgroundRepeatKeyword,
    pub y: BackgroundRepeatKeyword,
}

impl BackgroundRepeat {
    pub const fn repeat() -> Self {
        Self {
            x: BackgroundRepeatKeyword::Repeat,
            y: BackgroundRepeatKeyword::Repeat,
        }
    }

    pub const fn repeat_x() -> Self {
        Self {
            x: BackgroundRepeatKeyword::Repeat,
            y: BackgroundRepeatKeyword::NoRepeat,
        }
    }

    pub const fn repeat_y() -> Self {
        Self {
            x: BackgroundRepeatKeyword::NoRepeat,
            y: BackgroundRepeatKeyword::Repeat,
        }
    }

    pub const fn no_repeat() -> Self {
        Self {
            x: BackgroundRepeatKeyword::NoRepeat,
            y: BackgroundRepeatKeyword::NoRepeat,
        }
    }
}
