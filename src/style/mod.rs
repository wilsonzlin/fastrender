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
use color::Rgba;
use display::Display;
use position::Position;
use std::collections::HashMap;
use types::{
    AlignContent, AlignItems, BackgroundImage, BackgroundPosition, BackgroundRepeat, BackgroundSize, BorderCollapse,
    BorderStyle, Direction, FlexBasis, FlexDirection, FlexWrap, FontStyle, FontWeight, GridTrack, JustifyContent,
    LineHeight, Overflow, TextAlign, TextDecoration, TextTransform, UnicodeBidi, WhiteSpace,
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
    pub z_index: i32,

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
    pub grid_column_names: HashMap<String, Vec<usize>>, // Named grid lines for columns
    pub grid_row_names: HashMap<String, Vec<usize>>,    // Named grid lines for rows
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
    pub line_height: LineHeight,
    pub direction: Direction,
    pub unicode_bidi: UnicodeBidi,
    pub text_align: TextAlign,
    pub text_decoration: TextDecoration,
    pub text_transform: TextTransform,
    pub letter_spacing: f32,
    pub word_spacing: f32,
    pub white_space: WhiteSpace,

    // Color and background
    pub color: Rgba,
    pub background_color: Rgba,
    pub background_image: Option<BackgroundImage>,
    pub background_size: BackgroundSize,
    pub background_position: BackgroundPosition,
    pub background_repeat: BackgroundRepeat,

    // Visual effects
    pub opacity: f32,
    pub box_shadow: Vec<BoxShadow>,
    pub text_shadow: Vec<TextShadow>,
    pub transform: Vec<Transform>,
    pub overflow_x: Overflow,
    pub overflow_y: Overflow,
    pub border_spacing_horizontal: Length,
    pub border_spacing_vertical: Length,
    pub border_collapse: BorderCollapse,

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
            z_index: 0,

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
            grid_column_names: HashMap::new(),
            grid_row_names: HashMap::new(),
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
            line_height: LineHeight::Normal,
            direction: Direction::Ltr,
            unicode_bidi: UnicodeBidi::Normal,
            text_align: TextAlign::Left,
            text_decoration: TextDecoration::None,
            text_transform: TextTransform::None,
            letter_spacing: 0.0,
            word_spacing: 0.0,
            white_space: WhiteSpace::Normal,

            color: Rgba::BLACK,
            background_color: Rgba::TRANSPARENT,
            background_image: None,
            background_size: BackgroundSize::Auto,
            background_position: BackgroundPosition::Center,
            background_repeat: BackgroundRepeat::Repeat,

            opacity: 1.0,
            box_shadow: Vec::new(),
            text_shadow: Vec::new(),
            transform: Vec::new(),
            overflow_x: Overflow::Visible,
            overflow_y: Overflow::Visible,
            border_spacing_horizontal: Length::px(0.0),
            border_spacing_vertical: Length::px(0.0),
            border_collapse: BorderCollapse::Separate,

            custom_properties: HashMap::new(),

            content: String::new(),
        }
    }
}
