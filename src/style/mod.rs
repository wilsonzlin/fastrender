//! Style system types
//!
//! This module contains types related to CSS styling, including colors,
//! computed styles, and style properties.

pub mod cascade;
pub mod color;
pub mod computed;
pub mod content;
pub mod counters;
pub mod defaults;
pub mod display;
pub mod float;
pub mod grid;
pub mod media;
pub mod position;
pub mod properties;
pub mod types;
pub mod values;
pub mod var_resolution;
pub mod variables;

// Internal imports used by ComputedStyle
use crate::css::types::{BoxShadow, TextShadow, Transform};
use crate::style::computed::Visibility;
use crate::style::float::{Clear, Float};
use color::Rgba;
use counters::CounterProperties;
use display::Display;
use position::Position;
use std::collections::HashMap;
use types::{
    AlignContent, AlignItems, BackgroundAttachment, BackgroundBox, BackgroundImage, BackgroundPosition,
    BackgroundPositionComponent, BackgroundRepeat, BackgroundSize, BackgroundSizeComponent, BorderCollapse,
    BorderStyle, BoxSizing, CaptionSide, Direction, EmptyCells, FilterFunction, FlexBasis, FlexDirection, FlexWrap,
    FontFeatureSetting, FontKerning, FontSizeAdjust, FontStretch, FontStyle, FontSynthesis, FontVariant,
    FontVariantAlternates, FontVariantCaps, FontVariantEastAsian, FontVariantLigatures, FontVariantNumeric,
    FontVariantPosition, FontWeight, GridTrack, HyphensMode, Isolation, JustifyContent, LineBreak, LineHeight,
    ListStyleImage, ListStylePosition, ListStyleType, MixBlendMode, ObjectFit, ObjectPosition, OutlineColor,
    OutlineStyle, Overflow, OverflowWrap, TabSize, TableLayout, TextAlign, TextAlignLast, TextDecoration,
    TextDecorationSkipInk, TextEmphasisPosition, TextEmphasisStyle, TextIndent, TextJustify, TextTransform,
    TextUnderlineOffset, TextUnderlinePosition, TransformOrigin, UnicodeBidi, VerticalAlign, WhiteSpace, WordBreak,
};
use values::Length;

// Re-export common types from values module
// These are now public via the module system

#[derive(Debug, Clone, PartialEq)]
pub struct ComputedStyle {
    // Display and positioning
    pub display: Display,
    pub position: Position,
    pub top: Option<Length>,
    pub right: Option<Length>,
    pub bottom: Option<Length>,
    pub left: Option<Length>,
    pub float: Float,
    pub clear: Clear,
    /// Stacking order for positioned elements (`auto` = None)
    pub z_index: Option<i32>,
    pub visibility: Visibility,
    pub outline_color: OutlineColor,
    pub outline_style: OutlineStyle,
    pub outline_width: Length,
    pub outline_offset: Length,
    pub box_sizing: BoxSizing,

    // Box model
    pub width: Option<Length>,
    pub height: Option<Length>,
    pub min_width: Option<Length>,
    pub min_height: Option<Length>,
    pub max_width: Option<Length>,
    pub max_height: Option<Length>,

    pub margin_top: Option<Length>,
    pub margin_right: Option<Length>,
    pub margin_bottom: Option<Length>,
    pub margin_left: Option<Length>,

    pub padding_top: Length,
    pub padding_right: Length,
    pub padding_bottom: Length,
    pub padding_left: Length,

    pub border_top_width: Length,
    pub border_right_width: Length,
    pub border_bottom_width: Length,
    pub border_left_width: Length,

    pub border_top_color: Rgba,
    pub border_right_color: Rgba,
    pub border_bottom_color: Rgba,
    pub border_left_color: Rgba,

    pub border_top_style: BorderStyle,
    pub border_right_style: BorderStyle,
    pub border_bottom_style: BorderStyle,
    pub border_left_style: BorderStyle,

    pub border_top_left_radius: Length,
    pub border_top_right_radius: Length,
    pub border_bottom_left_radius: Length,
    pub border_bottom_right_radius: Length,

    // Flexbox
    pub flex_direction: FlexDirection,
    pub flex_wrap: FlexWrap,
    pub justify_content: JustifyContent,
    pub align_items: AlignItems,
    pub align_content: AlignContent,
    pub flex_grow: f32,
    pub flex_shrink: f32,
    pub flex_basis: FlexBasis,

    // Grid
    pub grid_template_columns: Vec<GridTrack>,
    pub grid_template_rows: Vec<GridTrack>,
    /// Parsed grid-template-areas rows (None for empty cells)
    pub grid_template_areas: Vec<Vec<Option<String>>>,
    /// Sizes for implicitly created rows
    pub grid_auto_rows: Vec<GridTrack>,
    /// Sizes for implicitly created columns
    pub grid_auto_columns: Vec<GridTrack>,
    /// Auto-placement direction/density
    pub grid_auto_flow: types::GridAutoFlow,
    pub grid_column_names: HashMap<String, Vec<usize>>, // Named grid lines for columns
    pub grid_row_names: HashMap<String, Vec<usize>>,    // Named grid lines for rows
    pub grid_column_line_names: Vec<Vec<String>>,       // Line names per column line (tracks+1)
    pub grid_row_line_names: Vec<Vec<String>>,          // Line names per row line (tracks+1)
    pub grid_gap: Length,
    pub grid_row_gap: Length,
    pub grid_column_gap: Length,
    pub grid_column_start: i32,
    pub grid_column_end: i32,
    pub grid_row_start: i32,
    pub grid_row_end: i32,
    // Raw grid-column/row values (before resolving named lines)
    pub(crate) grid_column_raw: Option<String>,
    pub(crate) grid_row_raw: Option<String>,

    // Typography
    pub font_family: Vec<String>,
    pub font_size: f32,
    pub font_weight: FontWeight,
    pub font_style: FontStyle,
    pub font_variant: FontVariant,
    pub font_variant_caps: FontVariantCaps,
    pub font_variant_alternates: FontVariantAlternates,
    pub font_variant_numeric: FontVariantNumeric,
    pub font_variant_east_asian: FontVariantEastAsian,
    pub font_variant_ligatures: FontVariantLigatures,
    pub font_variant_position: FontVariantPosition,
    pub font_size_adjust: FontSizeAdjust,
    pub font_synthesis: FontSynthesis,
    pub font_feature_settings: Vec<FontFeatureSetting>,
    pub font_stretch: FontStretch,
    pub font_kerning: FontKerning,
    pub line_height: LineHeight,
    pub direction: Direction,
    pub unicode_bidi: UnicodeBidi,
    /// Computed root element font size for resolving rem units
    pub root_font_size: f32,
    pub text_align: TextAlign,
    pub text_align_last: TextAlignLast,
    pub text_justify: TextJustify,
    pub text_indent: TextIndent,
    pub text_decoration: TextDecoration,
    /// Whether a text-decoration line declaration was authored (shorthand or longhand).
    pub text_decoration_line_specified: bool,
    /// Propagated decorations from ancestors and this element.
    pub applied_text_decorations: Vec<types::ResolvedTextDecoration>,
    pub text_decoration_skip_ink: TextDecorationSkipInk,
    pub text_underline_offset: TextUnderlineOffset,
    pub text_underline_position: TextUnderlinePosition,
    pub text_emphasis_style: TextEmphasisStyle,
    /// None means currentColor.
    pub text_emphasis_color: Option<Rgba>,
    pub text_emphasis_position: TextEmphasisPosition,
    pub text_transform: TextTransform,
    pub letter_spacing: f32,
    pub word_spacing: f32,
    pub white_space: WhiteSpace,
    pub line_break: LineBreak,
    pub tab_size: TabSize,
    pub hyphens: HyphensMode,
    pub word_break: WordBreak,
    pub overflow_wrap: OverflowWrap,
    pub vertical_align: VerticalAlign,
    /// BCP47 language tag inherited from DOM (lang/xml:lang)
    pub language: String,
    pub list_style_type: ListStyleType,
    pub list_style_position: ListStylePosition,
    pub list_style_image: ListStyleImage,
    /// Counter properties (reset/increment/set)
    pub counters: CounterProperties,

    // Color and background
    pub color: Rgba,
    pub background_color: Rgba,
    pub background_image: Option<BackgroundImage>,
    pub background_size: BackgroundSize,
    pub background_position: BackgroundPosition,
    pub background_attachment: BackgroundAttachment,
    pub background_repeat: BackgroundRepeat,
    pub background_origin: BackgroundBox,
    pub background_clip: BackgroundBox,
    pub object_fit: ObjectFit,
    pub object_position: ObjectPosition,
    /// CSS filter effects applied to this element
    pub filter: Vec<FilterFunction>,
    /// Backdrop filters applied to the backdrop behind this element
    pub backdrop_filter: Vec<FilterFunction>,
    pub mix_blend_mode: MixBlendMode,
    pub isolation: Isolation,

    // Visual effects
    pub opacity: f32,
    pub box_shadow: Vec<BoxShadow>,
    pub text_shadow: Vec<TextShadow>,
    pub transform: Vec<Transform>,
    pub transform_origin: TransformOrigin,
    pub overflow_x: Overflow,
    pub overflow_y: Overflow,
    pub border_spacing_horizontal: Length,
    pub border_spacing_vertical: Length,
    pub border_collapse: BorderCollapse,
    pub table_layout: TableLayout,
    pub caption_side: CaptionSide,
    pub empty_cells: EmptyCells,

    // CSS Custom Properties (variables)
    pub custom_properties: HashMap<String, String>,

    // Generated content (for ::before and ::after pseudo-elements)
    pub content: String,
}

impl Default for ComputedStyle {
    fn default() -> Self {
        Self {
            display: Display::Inline,
            position: Position::Static,
            top: None,
            right: None,
            bottom: None,
            left: None,
            float: Float::None,
            clear: Clear::None,
            z_index: None,
            visibility: Visibility::Visible,
            outline_color: OutlineColor::Invert,
            outline_style: OutlineStyle::None,
            outline_width: Length::px(3.0),
            outline_offset: Length::px(0.0),
            box_sizing: BoxSizing::ContentBox,

            width: None,
            height: None,
            min_width: None,
            min_height: None,
            max_width: None,
            max_height: None,

            margin_top: Some(Length::px(0.0)),
            margin_right: Some(Length::px(0.0)),
            margin_bottom: Some(Length::px(0.0)),
            margin_left: Some(Length::px(0.0)),

            padding_top: Length::px(0.0),
            padding_right: Length::px(0.0),
            padding_bottom: Length::px(0.0),
            padding_left: Length::px(0.0),

            border_top_width: Length::px(0.0),
            border_right_width: Length::px(0.0),
            border_bottom_width: Length::px(0.0),
            border_left_width: Length::px(0.0),

            border_top_color: Rgba::BLACK,
            border_right_color: Rgba::BLACK,
            border_bottom_color: Rgba::BLACK,
            border_left_color: Rgba::BLACK,

            border_top_style: BorderStyle::None,
            border_right_style: BorderStyle::None,
            border_bottom_style: BorderStyle::None,
            border_left_style: BorderStyle::None,

            border_top_left_radius: Length::px(0.0),
            border_top_right_radius: Length::px(0.0),
            border_bottom_left_radius: Length::px(0.0),
            border_bottom_right_radius: Length::px(0.0),

            flex_direction: FlexDirection::Row,
            flex_wrap: FlexWrap::NoWrap,
            justify_content: JustifyContent::FlexStart,
            align_items: AlignItems::Stretch,
            align_content: AlignContent::Stretch,
            flex_grow: 0.0,
            flex_shrink: 1.0,
            flex_basis: FlexBasis::Auto,

            grid_template_columns: Vec::new(),
            grid_template_rows: Vec::new(),
            grid_template_areas: Vec::new(),
            grid_auto_rows: vec![GridTrack::Auto],
            grid_auto_columns: vec![GridTrack::Auto],
            grid_auto_flow: types::GridAutoFlow::Row,
            grid_column_names: HashMap::new(),
            grid_row_names: HashMap::new(),
            grid_column_line_names: Vec::new(),
            grid_row_line_names: Vec::new(),
            grid_gap: Length::px(0.0),
            grid_row_gap: Length::px(0.0),
            grid_column_gap: Length::px(0.0),
            grid_column_start: 0,
            grid_column_end: 0,
            grid_row_start: 0,
            grid_row_end: 0,
            grid_column_raw: None,
            grid_row_raw: None,

            font_family: vec!["serif".to_string()],
            font_size: 16.0,
            font_weight: FontWeight::Normal,
            font_style: FontStyle::Normal,
            font_variant: FontVariant::Normal,
            font_variant_caps: FontVariantCaps::default(),
            font_variant_alternates: FontVariantAlternates::default(),
            font_variant_numeric: FontVariantNumeric::default(),
            font_variant_east_asian: FontVariantEastAsian::default(),
            font_variant_ligatures: FontVariantLigatures::default(),
            font_variant_position: FontVariantPosition::Normal,
            font_size_adjust: FontSizeAdjust::None,
            font_synthesis: FontSynthesis::default(),
            font_feature_settings: Vec::new(),
            font_stretch: FontStretch::Normal,
            font_kerning: FontKerning::Auto,
            line_height: LineHeight::Normal,
            direction: Direction::Ltr,
            unicode_bidi: UnicodeBidi::Normal,
            root_font_size: 16.0,
            text_align: TextAlign::Start,
            text_align_last: TextAlignLast::Auto,
            text_justify: TextJustify::Auto,
            text_indent: TextIndent::default(),
            text_decoration: TextDecoration::default(),
            text_decoration_line_specified: false,
            applied_text_decorations: Vec::new(),
            text_decoration_skip_ink: TextDecorationSkipInk::Auto,
            text_underline_offset: TextUnderlineOffset::default(),
            text_underline_position: TextUnderlinePosition::default(),
            text_emphasis_style: TextEmphasisStyle::default(),
            text_emphasis_color: None,
            text_emphasis_position: TextEmphasisPosition::default(),
            text_transform: TextTransform::default(),
            letter_spacing: 0.0,
            word_spacing: 0.0,
            white_space: WhiteSpace::Normal,
            line_break: LineBreak::Auto,
            tab_size: TabSize::default(),
            hyphens: HyphensMode::Manual,
            word_break: WordBreak::Normal,
            overflow_wrap: OverflowWrap::Normal,
            vertical_align: VerticalAlign::Baseline,
            language: "en".to_string(),
            list_style_type: ListStyleType::Disc,
            list_style_position: ListStylePosition::Outside,
            list_style_image: ListStyleImage::None,
            counters: CounterProperties::default(),

            color: Rgba::BLACK,
            background_color: Rgba::TRANSPARENT,
            background_image: None,
            background_size: BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto),
            background_position: BackgroundPosition::Position {
                x: BackgroundPositionComponent {
                    alignment: 0.0,
                    offset: Length::px(0.0),
                },
                y: BackgroundPositionComponent {
                    alignment: 0.0,
                    offset: Length::px(0.0),
                },
            },
            background_repeat: BackgroundRepeat::repeat(),
            background_attachment: BackgroundAttachment::Scroll,
            background_origin: BackgroundBox::PaddingBox,
            background_clip: BackgroundBox::BorderBox,
            object_fit: ObjectFit::Fill,
            object_position: ObjectPosition {
                x: types::PositionComponent::Keyword(types::PositionKeyword::Center),
                y: types::PositionComponent::Keyword(types::PositionKeyword::Center),
            },
            filter: Vec::new(),
            backdrop_filter: Vec::new(),
            mix_blend_mode: MixBlendMode::Normal,
            isolation: Isolation::Auto,

            opacity: 1.0,
            box_shadow: Vec::new(),
            text_shadow: Vec::new(),
            transform: Vec::new(),
            transform_origin: TransformOrigin {
                x: Length::percent(50.0),
                y: Length::percent(50.0),
            },
            overflow_x: Overflow::Visible,
            overflow_y: Overflow::Visible,
            border_spacing_horizontal: Length::px(0.0),
            border_spacing_vertical: Length::px(0.0),
            border_collapse: BorderCollapse::Separate,
            table_layout: TableLayout::Auto,
            caption_side: CaptionSide::Top,
            empty_cells: EmptyCells::Show,

            custom_properties: HashMap::new(),

            content: String::new(),
        }
    }
}

/// Normalize a language tag for internal use.
///
/// - Trims surrounding whitespace
/// - Converts underscores to hyphens
/// - Lowercases all subtags (BCP47 is case-insensitive; lowercase helps downstream consumers)
pub(crate) fn normalize_language_tag(tag: &str) -> String {
    let trimmed = tag.trim();
    if trimmed.is_empty() {
        return String::new();
    }

    trimmed
        .replace('_', "-")
        .split('-')
        .filter(|s| !s.is_empty())
        .map(|s| s.to_ascii_lowercase())
        .collect::<Vec<_>>()
        .join("-")
}

#[cfg(test)]
mod tests {
    use super::normalize_language_tag;

    #[test]
    fn normalizes_language_tags_to_lower_hyphenated() {
        assert_eq!(normalize_language_tag("En-US"), "en-us");
        assert_eq!(normalize_language_tag(" sr_Cyrl_RS "), "sr-cyrl-rs");
        assert_eq!(normalize_language_tag(""), "");
    }
}
