//! Style type definitions
//!
//! This module contains all the enum types used in computed styles.
//! These types represent CSS property values that can be applied to elements.

use crate::css::types::{ColorStop, RadialGradientShape, RadialGradientSize};
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
    Clip,
}

/// Determines which box the width/height properties apply to.
///
/// CSS: `box-sizing`
/// Reference: CSS Box Sizing Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoxSizing {
    ContentBox,
    BorderBox,
}

/// Border collapsing model for tables
///
/// CSS 2.1 §17.6.1: initial value is `separate`
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BorderCollapse {
    Separate,
    Collapse,
}

/// Whether borders/backgrounds are drawn for empty table cells.
///
/// CSS 2.1 §17.6.1: initial value is `show`, applies to table cells and inherits.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmptyCells {
    Show,
    Hide,
}

/// Caption placement relative to the table box.
///
/// CSS 2.1 §17.4: initial value is `top`, applies to table captions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptionSide {
    Top,
    Bottom,
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

/// Outline line style
///
/// CSS: `outline-style`
/// Reference: CSS Basic User Interface Level 4 (outline)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutlineStyle {
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
    Auto,
}

impl OutlineStyle {
    /// Returns true if the outline would paint (non-none/hidden)
    pub fn paints(self) -> bool {
        !matches!(self, OutlineStyle::None | OutlineStyle::Hidden)
    }

    /// Converts to the closest border style for painting
    pub fn to_border_style(self) -> BorderStyle {
        match self {
            OutlineStyle::None => BorderStyle::None,
            OutlineStyle::Hidden => BorderStyle::Hidden,
            OutlineStyle::Solid => BorderStyle::Solid,
            OutlineStyle::Dashed => BorderStyle::Dashed,
            OutlineStyle::Dotted => BorderStyle::Dotted,
            OutlineStyle::Double => BorderStyle::Double,
            OutlineStyle::Groove => BorderStyle::Groove,
            OutlineStyle::Ridge => BorderStyle::Ridge,
            OutlineStyle::Inset => BorderStyle::Inset,
            OutlineStyle::Outset => BorderStyle::Outset,
            OutlineStyle::Auto => BorderStyle::Solid,
        }
    }
}

/// Outline color value (can reference currentColor or invert)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OutlineColor {
    CurrentColor,
    Color(crate::style::color::Rgba),
    Invert,
}

impl OutlineColor {
    /// Resolves the outline color to an RGBA and whether it should invert destination pixels.
    pub fn resolve(self, current_color: crate::style::color::Rgba) -> (crate::style::color::Rgba, bool) {
        match self {
            OutlineColor::CurrentColor => (current_color, false),
            OutlineColor::Color(c) => (c, false),
            OutlineColor::Invert => (crate::style::color::Rgba::WHITE, true),
        }
    }
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

/// Image scaling quality hint
///
/// CSS: `image-rendering`
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImageRendering {
    Auto,
    Smooth,
    CrispEdges,
    Pixelated,
}

/// Computed aspect-ratio value
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AspectRatio {
    Auto,
    Ratio(f32),
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

/// Writing mode for block/inline axis orientation
///
/// CSS: `writing-mode`
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WritingMode {
    HorizontalTb,
    VerticalRl,
    VerticalLr,
    SidewaysRl,
    SidewaysLr,
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

/// Cursor keywords (fallbacks for custom cursor images)
///
/// CSS UI Level 4 cursor values (subset relevant to rendering hints)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorKeyword {
    Auto,
    Default,
    None,
    ContextMenu,
    Help,
    Pointer,
    Progress,
    Wait,
    Cell,
    Crosshair,
    Text,
    VerticalText,
    Alias,
    Copy,
    Move,
    NoDrop,
    NotAllowed,
    Grab,
    Grabbing,
    AllScroll,
    ColResize,
    RowResize,
    NResize,
    SResize,
    EResize,
    WResize,
    NeResize,
    NwResize,
    SeResize,
    SwResize,
    EwResize,
    NsResize,
    NeswResize,
    NwseResize,
    ZoomIn,
    ZoomOut,
}

impl Default for CursorKeyword {
    fn default() -> Self {
        CursorKeyword::Auto
    }
}

/// A custom cursor image with an optional hotspot (x, y) in CSS pixels
#[derive(Debug, Clone, PartialEq)]
pub struct CursorImage {
    pub url: String,
    pub hotspot: Option<(f32, f32)>,
}

/// CSS will-change hints
///
/// CSS: `will-change`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WillChange {
    /// Default value – no proactive optimizations
    Auto,
    /// Explicit list of features the author expects to change
    Hints(Vec<WillChangeHint>),
}

impl Default for WillChange {
    fn default() -> Self {
        Self::Auto
    }
}

/// Individual will-change hints
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WillChangeHint {
    ScrollPosition,
    Contents,
    /// A property name (lowercased)
    Property(String),
}

impl WillChange {
    /// Returns true if the hint set should proactively create a stacking context.
    ///
    /// CSS Will Change: "will-change set to a property that would create a stacking context
    /// when not at its initial value" requires creating a stacking context up-front.
    pub fn creates_stacking_context(&self) -> bool {
        match self {
            WillChange::Auto => false,
            WillChange::Hints(hints) => hints.iter().any(WillChangeHint::creates_stacking_context),
        }
    }
}

impl WillChangeHint {
    fn creates_stacking_context(&self) -> bool {
        match self {
            WillChangeHint::ScrollPosition | WillChangeHint::Contents => true,
            WillChangeHint::Property(name) => matches!(
                name.as_str(),
                // Properties that create stacking contexts when non-initial
                "transform"
                    | "opacity"
                    | "filter"
                    | "backdrop-filter"
                    | "perspective"
                    | "clip-path"
                    | "mask"
                    | "mask-image"
                    | "mask-border"
                    | "mix-blend-mode"
                    | "isolation"
                    | "contain"
            ),
        }
    }
}

/// CSS containment model
///
/// CSS: `contain`
/// Reference: CSS Containment Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Containment {
    pub size: bool,
    pub inline_size: bool,
    pub layout: bool,
    pub style: bool,
    pub paint: bool,
}

impl Containment {
    pub const fn none() -> Self {
        Self {
            size: false,
            inline_size: false,
            layout: false,
            style: false,
            paint: false,
        }
    }

    pub const fn strict() -> Self {
        Self {
            size: true,
            inline_size: false,
            layout: true,
            style: true,
            paint: true,
        }
    }

    pub const fn content() -> Self {
        Self {
            size: false,
            inline_size: false,
            layout: true,
            style: true,
            paint: true,
        }
    }

    pub fn with_flags(size: bool, inline_size: bool, layout: bool, style: bool, paint: bool) -> Self {
        Self {
            size,
            inline_size,
            layout,
            style,
            paint,
        }
    }

    pub fn creates_stacking_context(&self) -> bool {
        self.paint
    }
}

impl Default for Containment {
    fn default() -> Self {
        Self::none()
    }
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
    Start,
    End,
    SelfStart,
    SelfEnd,
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
    SpaceEvenly,
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
    MinContent,
    MaxContent,
    FitContent(Length),
    MinMax(Box<GridTrack>, Box<GridTrack>),
    RepeatAutoFill {
        tracks: Vec<GridTrack>,
        line_names: Vec<Vec<String>>,
    },
    RepeatAutoFit {
        tracks: Vec<GridTrack>,
        line_names: Vec<Vec<String>>,
    },
}

/// Auto-placement direction and density for implicit grid items
///
/// CSS: `grid-auto-flow`
/// Reference: CSS Grid Layout Module Level 1
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GridAutoFlow {
    Row,
    Column,
    RowDense,
    ColumnDense,
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
    /// Clamp a numeric weight to the CSS valid range (1-1000)
    fn clamp(weight: u16) -> u16 {
        weight.clamp(1, 1000)
    }

    /// Resolve the `bolder` keyword relative to a parent weight using the CSS Fonts Level 4 table.
    ///
    /// See: https://www.w3.org/TR/css-fonts-4/#relative-weights
    fn relative_bolder(parent_weight: u16) -> u16 {
        let w = Self::clamp(parent_weight);
        if w < 100 {
            400
        } else if w < 350 {
            400
        } else if w < 550 {
            700
        } else if w < 750 {
            900
        } else if w < 900 {
            900
        } else {
            w
        }
    }

    /// Resolve the `lighter` keyword relative to a parent weight using the CSS Fonts Level 4 table.
    ///
    /// See: https://www.w3.org/TR/css-fonts-4/#relative-weights
    fn relative_lighter(parent_weight: u16) -> u16 {
        let w = Self::clamp(parent_weight);
        if w < 100 {
            w
        } else if w < 350 {
            100
        } else if w < 550 {
            100
        } else if w < 750 {
            400
        } else if w < 900 {
            700
        } else {
            700
        }
    }

    /// Resolve relative keywords (bolder/lighter) against the parent weight and clamp numeric values.
    pub(crate) fn resolve_relative(self, parent_weight: u16) -> Self {
        match self {
            FontWeight::Bolder => FontWeight::Number(Self::relative_bolder(parent_weight)),
            FontWeight::Lighter => FontWeight::Number(Self::relative_lighter(parent_weight)),
            FontWeight::Number(n) => FontWeight::Number(Self::clamp(n)),
            other => other,
        }
    }

    /// Converts font weight to numeric u16 value (1-1000)
    pub fn to_u16(self) -> u16 {
        match self {
            FontWeight::Normal => 400,
            FontWeight::Bold => 700,
            FontWeight::Bolder => Self::relative_bolder(400),
            FontWeight::Lighter => Self::relative_lighter(400),
            FontWeight::Number(n) => Self::clamp(n),
        }
    }
}

/// Font style (normal, italic, oblique)
///
/// CSS: `font-style`
/// Reference: CSS Fonts Module Level 3
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FontStyle {
    Normal,
    Italic,
    /// Oblique may carry an optional angle (deg)
    Oblique(Option<f32>),
}

impl FontStyle {
    pub fn is_italic(self) -> bool {
        matches!(self, FontStyle::Italic)
    }

    pub fn is_oblique(self) -> bool {
        matches!(self, FontStyle::Oblique(_))
    }

    pub fn oblique_angle(self) -> Option<f32> {
        match self {
            FontStyle::Oblique(angle) => angle,
            _ => None,
        }
    }
}

/// Font variant
///
/// CSS: `font-variant`
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FontVariant {
    Normal,
    SmallCaps,
}

/// Caps variants (font-variant-caps)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FontVariantCaps {
    Normal,
    SmallCaps,
    AllSmallCaps,
    PetiteCaps,
    AllPetiteCaps,
    Unicase,
    TitlingCaps,
}

impl Default for FontVariantCaps {
    fn default() -> Self {
        FontVariantCaps::Normal
    }
}

/// Alternates (`font-variant-alternates`)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FontVariantAlternates {
    pub historical_forms: bool,
    pub stylistic: Option<u8>,
    pub stylesets: Vec<u8>,
    pub character_variants: Vec<u8>,
    pub swash: Option<u8>,
    pub ornaments: Option<u8>,
    pub annotation: Option<String>,
}

impl Default for FontVariantAlternates {
    fn default() -> Self {
        Self {
            historical_forms: false,
            stylistic: None,
            stylesets: Vec::new(),
            character_variants: Vec::new(),
            swash: None,
            ornaments: None,
            annotation: None,
        }
    }
}

/// Numeric variants (`font-variant-numeric`)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericFigure {
    Normal,
    Lining,
    Oldstyle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericSpacing {
    Normal,
    Proportional,
    Tabular,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericFraction {
    Normal,
    Diagonal,
    Stacked,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FontVariantNumeric {
    pub figure: NumericFigure,
    pub spacing: NumericSpacing,
    pub fraction: NumericFraction,
    pub ordinal: bool,
    pub slashed_zero: bool,
}

impl Default for FontVariantNumeric {
    fn default() -> Self {
        Self {
            figure: NumericFigure::Normal,
            spacing: NumericSpacing::Normal,
            fraction: NumericFraction::Normal,
            ordinal: false,
            slashed_zero: false,
        }
    }
}

/// Font ligature controls (font-variant-ligatures)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FontVariantLigatures {
    pub common: bool,
    pub discretionary: bool,
    pub historical: bool,
    pub contextual: bool,
}

impl Default for FontVariantLigatures {
    fn default() -> Self {
        // Initial value "normal": common + contextual on; discretionary/historical off.
        Self {
            common: true,
            discretionary: false,
            historical: false,
            contextual: true,
        }
    }
}

/// Low-level OpenType feature override (`font-feature-settings`)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FontFeatureSetting {
    pub tag: [u8; 4],
    pub value: u32,
}

/// Low-level font variation override (`font-variation-settings`)
#[derive(Debug, Clone, PartialEq)]
pub struct FontVariationSetting {
    pub tag: [u8; 4],
    pub value: f32,
}

/// Overrides the OpenType language system when shaping text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FontLanguageOverride {
    /// Use the element/Document language (default)
    Normal,
    /// Override with an explicit OpenType language system tag (1–4 ASCII letters)
    Override(String),
}

/// Optical sizing control (`font-optical-sizing`)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FontOpticalSizing {
    Auto,
    None,
}

/// East Asian variants (`font-variant-east-asian`)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EastAsianVariant {
    Jis78,
    Jis83,
    Jis90,
    Jis04,
    Simplified,
    Traditional,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EastAsianWidth {
    FullWidth,
    ProportionalWidth,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FontVariantEastAsian {
    pub variant: Option<EastAsianVariant>,
    pub width: Option<EastAsianWidth>,
    pub ruby: bool,
}

impl Default for FontVariantEastAsian {
    fn default() -> Self {
        Self {
            variant: None,
            width: None,
            ruby: false,
        }
    }
}

/// Positional variants (`font-variant-position`)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FontVariantPosition {
    Normal,
    Sub,
    Super,
}

/// Controls which font properties may be synthetically generated (`font-synthesis`)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FontSynthesis {
    pub weight: bool,
    pub style: bool,
    pub small_caps: bool,
}

impl Default for FontSynthesis {
    fn default() -> Self {
        Self {
            weight: true,
            style: true,
            small_caps: true,
        }
    }
}

/// Font size adjustment ratio (`font-size-adjust`)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FontSizeAdjust {
    None,
    Number(f32),
    FromFont,
}

impl Default for FontSizeAdjust {
    fn default() -> Self {
        FontSizeAdjust::None
    }
}

/// Kerning control (`font-kerning`)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FontKerning {
    Auto,
    Normal,
    None,
}

impl Default for FontKerning {
    fn default() -> Self {
        FontKerning::Auto
    }
}

/// Font stretch
///
/// CSS: `font-stretch`
/// Reference: CSS Fonts Module Level 4
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FontStretch {
    UltraCondensed,
    ExtraCondensed,
    Condensed,
    SemiCondensed,
    Normal,
    SemiExpanded,
    Expanded,
    ExtraExpanded,
    UltraExpanded,
    /// Percentage stretch (50%-200%)
    Percentage(f32),
}

impl FontStretch {
    /// Creates a FontStretch from a percentage value, clamped to the spec range (50%-200%).
    pub fn from_percentage(percent: f32) -> Self {
        let clamped = percent.clamp(50.0, 200.0);
        FontStretch::Percentage(clamped)
    }

    /// Returns the percentage representation of this stretch value.
    pub fn to_percentage(self) -> f32 {
        match self {
            FontStretch::UltraCondensed => 50.0,
            FontStretch::ExtraCondensed => 62.5,
            FontStretch::Condensed => 75.0,
            FontStretch::SemiCondensed => 87.5,
            FontStretch::Normal => 100.0,
            FontStretch::SemiExpanded => 112.5,
            FontStretch::Expanded => 125.0,
            FontStretch::ExtraExpanded => 150.0,
            FontStretch::UltraExpanded => 200.0,
            FontStretch::Percentage(p) => p.clamp(50.0, 200.0),
        }
    }
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
    MatchParent,
}

/// CSS `text-orientation`
///
/// Reference: CSS Writing Modes Level 4
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextOrientation {
    Mixed,
    Upright,
    Sideways,
    SidewaysRight,
}

impl Default for TextOrientation {
    fn default() -> Self {
        TextOrientation::Mixed
    }
}

/// CSS `text-combine-upright`
///
/// Reference: CSS Writing Modes Level 4
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextCombineUpright {
    None,
    All,
    Digits(u8),
}

impl Default for TextCombineUpright {
    fn default() -> Self {
        TextCombineUpright::None
    }
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

/// Whether underlines skip glyph ink.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextDecorationSkipInk {
    Auto,
    None,
    All,
}

/// Resolved text-decoration to apply after propagation.
#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedTextDecoration {
    pub decoration: TextDecoration,
    pub skip_ink: TextDecorationSkipInk,
    pub underline_offset: TextUnderlineOffset,
    pub underline_position: TextUnderlinePosition,
}

/// Thickness of text decorations
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TextDecorationThickness {
    Auto,
    FromFont,
    Length(Length),
}

/// Controls underline offset relative to the default position.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TextUnderlineOffset {
    Auto,
    Length(Length),
}

impl Default for TextUnderlineOffset {
    fn default() -> Self {
        TextUnderlineOffset::Auto
    }
}

/// Placement of underlines relative to the text and inline axis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextUnderlinePosition {
    Auto,
    FromFont,
    Under,
    Left,
    Right,
    UnderLeft,
    UnderRight,
}

impl Default for TextUnderlinePosition {
    fn default() -> Self {
        TextUnderlinePosition::Auto
    }
}

/// Fill mode for emphasis marks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextEmphasisFill {
    Filled,
    Open,
}

/// Shape of emphasis marks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextEmphasisShape {
    Dot,
    Circle,
    DoubleCircle,
    Triangle,
    Sesame,
}

/// Emphasis style (mark or custom string).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TextEmphasisStyle {
    None,
    Mark {
        fill: TextEmphasisFill,
        shape: TextEmphasisShape,
    },
    String(String),
}

impl Default for TextEmphasisStyle {
    fn default() -> Self {
        TextEmphasisStyle::None
    }
}

impl TextEmphasisStyle {
    pub fn is_none(&self) -> bool {
        matches!(self, TextEmphasisStyle::None)
    }
}

/// Placement of emphasis marks relative to text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextEmphasisPosition {
    Auto,
    Over,
    Under,
    OverLeft,
    OverRight,
    UnderLeft,
    UnderRight,
}

impl Default for TextEmphasisPosition {
    fn default() -> Self {
        TextEmphasisPosition::Auto
    }
}

/// list-style-type values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ListStyleType {
    Disc,
    Circle,
    Square,
    Decimal,
    DecimalLeadingZero,
    LowerRoman,
    UpperRoman,
    LowerAlpha,
    UpperAlpha,
    Armenian,
    LowerArmenian,
    Georgian,
    LowerGreek,
    None,
}

/// list-style-position values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ListStylePosition {
    Outside,
    Inside,
}

/// list-style-image values
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ListStyleImage {
    None,
    Url(String),
}

/// Text case transformation
///
/// CSS: `text-transform`
/// Reference: CSS Text Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaseTransform {
    None,
    Uppercase,
    Lowercase,
    Capitalize,
}

/// Combination of text transformation effects
///
/// The grammar allows one case transform and optional width/kana transforms.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextTransform {
    pub case: CaseTransform,
    pub full_width: bool,
    pub full_size_kana: bool,
}

impl Default for TextTransform {
    fn default() -> Self {
        Self {
            case: CaseTransform::None,
            full_width: false,
            full_size_kana: false,
        }
    }
}

impl TextTransform {
    pub const fn none() -> Self {
        Self {
            case: CaseTransform::None,
            full_width: false,
            full_size_kana: false,
        }
    }

    pub const fn with_case(case: CaseTransform) -> Self {
        Self {
            case,
            full_width: false,
            full_size_kana: false,
        }
    }

    pub const fn full_width() -> Self {
        Self {
            case: CaseTransform::None,
            full_width: true,
            full_size_kana: false,
        }
    }
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

/// Line break strictness
///
/// CSS: `line-break`
/// Reference: CSS Text Module Level 3
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineBreak {
    Auto,
    Loose,
    Normal,
    Strict,
    Anywhere,
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
    None,
    Url(String),
    LinearGradient { angle: f32, stops: Vec<ColorStop> },
    RadialGradient {
        shape: RadialGradientShape,
        size: RadialGradientSize,
        position: BackgroundPosition,
        stops: Vec<ColorStop>,
    },
    RepeatingLinearGradient { angle: f32, stops: Vec<ColorStop> },
    RepeatingRadialGradient {
        shape: RadialGradientShape,
        size: RadialGradientSize,
        position: BackgroundPosition,
        stops: Vec<ColorStop>,
    },
    ConicGradient {
        from_angle: f32,
        position: BackgroundPosition,
        stops: Vec<ColorStop>,
    },
    RepeatingConicGradient {
        from_angle: f32,
        position: BackgroundPosition,
        stops: Vec<ColorStop>,
    },
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

/// Single background layer with per-layer properties.
#[derive(Debug, Clone, PartialEq)]
pub struct BackgroundLayer {
    pub image: Option<BackgroundImage>,
    pub position: BackgroundPosition,
    pub size: BackgroundSize,
    pub repeat: BackgroundRepeat,
    pub attachment: BackgroundAttachment,
    pub origin: BackgroundBox,
    pub clip: BackgroundBox,
    pub blend_mode: MixBlendMode,
}

impl Default for BackgroundLayer {
    fn default() -> Self {
        Self {
            image: None,
            position: BackgroundPosition::Position {
                x: BackgroundPositionComponent {
                    alignment: 0.0,
                    offset: Length::percent(0.0),
                },
                y: BackgroundPositionComponent {
                    alignment: 0.0,
                    offset: Length::percent(0.0),
                },
            },
            size: BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto),
            repeat: BackgroundRepeat::repeat(),
            attachment: BackgroundAttachment::Scroll,
            origin: BackgroundBox::PaddingBox,
            clip: BackgroundBox::BorderBox,
            blend_mode: MixBlendMode::Normal,
        }
    }
}
