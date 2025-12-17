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
    AccentColor, AlignContent, AlignItems, Appearance, AspectRatio, BackfaceVisibility, BackgroundAttachment,
    BackgroundBox, BackgroundImage, BackgroundLayer, BackgroundPosition, BackgroundRepeat, BackgroundSize,
    BorderCollapse, BorderImage, BorderStyle, BoxSizing, CaptionSide, CaretColor, ClipPath, ClipRect,
    ColorSchemePreference, ContainerType, Containment, CursorImage, CursorKeyword, Direction, EmptyCells,
    FilterFunction, FlexBasis, FlexDirection, FlexWrap, FontFeatureSetting, FontKerning, FontLanguageOverride,
    FontOpticalSizing, FontSizeAdjust, FontStretch, FontStyle, FontSynthesis, FontVariant, FontVariantAlternates,
    FontVariantCaps, FontVariantEastAsian, FontVariantEmoji, FontVariantLigatures, FontVariantNumeric,
    FontVariantPosition, FontVariationSetting, FontWeight, GridTrack, HyphensMode, ImageOrientation, ImageRendering,
    ImageResolution, Isolation, JustifyContent, LineBreak, LineHeight, ListStyleImage, ListStylePosition,
    ListStyleType, MixBlendMode, ObjectFit, ObjectPosition, OutlineColor, OutlineStyle, Overflow, OverflowAnchor,
    OverflowWrap, OverscrollBehavior, PointerEvents, Resize, ScrollBehavior, ScrollbarColor, ScrollbarWidth, TabSize,
    TableLayout, TextAlign, TextAlignLast, TextCombineUpright, TextDecoration, TextDecorationSkipInk,
    TextEmphasisPosition, TextEmphasisStyle, TextIndent, TextJustify, TextOrientation, TextOverflow, TextRendering,
    TextTransform, TextUnderlineOffset, TextUnderlinePosition, TouchAction, TransformBox, TransformOrigin,
    TransformStyle, UnicodeBidi, UserSelect, VerticalAlign, WhiteSpace, WillChange, WordBreak, WritingMode,
};
use values::Length;

// Re-export common types from values module
// These are now public via the module system

/// Physical box sides used when resolving logical properties.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PhysicalSide {
    Top,
    Right,
    Bottom,
    Left,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SideOrders {
    pub top: i32,
    pub right: i32,
    pub bottom: i32,
    pub left: i32,
}

impl Default for SideOrders {
    fn default() -> Self {
        Self {
            top: -1,
            right: -1,
            bottom: -1,
            left: -1,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CornerOrders {
    pub top_left: i32,
    pub top_right: i32,
    pub bottom_right: i32,
    pub bottom_left: i32,
}

impl Default for CornerOrders {
    fn default() -> Self {
        Self {
            top_left: -1,
            top_right: -1,
            bottom_right: -1,
            bottom_left: -1,
        }
    }
}

/// Logical axis for start/end mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalAxis {
    Inline,
    Block,
}

/// Pending logical properties (margin/padding/border) to resolve after writing-mode is known.
#[derive(Debug, Clone, PartialEq)]
pub enum LogicalProperty {
    Margin {
        axis: LogicalAxis,
        start: Option<Option<Length>>,
        end: Option<Option<Length>>,
    },
    Padding {
        axis: LogicalAxis,
        start: Option<Length>,
        end: Option<Length>,
    },
    InlineSize {
        value: Option<Option<Length>>,
    },
    BlockSize {
        value: Option<Option<Length>>,
    },
    MinInlineSize {
        value: Option<Option<Length>>,
    },
    MinBlockSize {
        value: Option<Option<Length>>,
    },
    MaxInlineSize {
        value: Option<Option<Length>>,
    },
    MaxBlockSize {
        value: Option<Option<Length>>,
    },
    Inset {
        axis: LogicalAxis,
        start: Option<Option<Length>>,
        end: Option<Option<Length>>,
    },
    BorderWidth {
        axis: LogicalAxis,
        start: Option<Length>,
        end: Option<Length>,
    },
    BorderStyle {
        axis: LogicalAxis,
        start: Option<BorderStyle>,
        end: Option<BorderStyle>,
    },
    BorderColor {
        axis: LogicalAxis,
        start: Option<Rgba>,
        end: Option<Rgba>,
    },
    BorderCorner {
        block_start: bool,
        inline_start: bool,
        value: Option<Length>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct PendingLogical {
    pub order: i32,
    pub property: LogicalProperty,
}

/// Tracks cascade ordering and deferred logical properties while computing styles.
#[derive(Debug, Clone, PartialEq)]
pub struct LogicalState {
    pub pending: Vec<PendingLogical>,
    pub margin_orders: SideOrders,
    pub padding_orders: SideOrders,
    pub border_width_orders: SideOrders,
    pub border_style_orders: SideOrders,
    pub border_color_orders: SideOrders,
    pub inset_orders: SideOrders,
    pub corner_orders: CornerOrders,
    pub width_order: i32,
    pub height_order: i32,
    pub min_width_order: i32,
    pub min_height_order: i32,
    pub max_width_order: i32,
    pub max_height_order: i32,
    next_order: i32,
}

impl Default for LogicalState {
    fn default() -> Self {
        Self {
            pending: Vec::new(),
            margin_orders: SideOrders::default(),
            padding_orders: SideOrders::default(),
            border_width_orders: SideOrders::default(),
            border_style_orders: SideOrders::default(),
            border_color_orders: SideOrders::default(),
            inset_orders: SideOrders::default(),
            corner_orders: CornerOrders::default(),
            width_order: -1,
            height_order: -1,
            min_width_order: -1,
            min_height_order: -1,
            max_width_order: -1,
            max_height_order: -1,
            next_order: 0,
        }
    }
}

impl LogicalState {
    pub fn next_order(&mut self) -> i32 {
        let order = self.next_order;
        self.next_order += 1;
        order
    }

    pub fn next_order_value(&self) -> i32 {
        self.next_order
    }

    pub fn set_next_order(&mut self, value: i32) {
        self.next_order = value;
    }

    pub fn reset(&mut self) {
        *self = LogicalState::default();
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComputedStyle {
    // Display and positioning
    pub display: Display,
    pub position: Position,
    pub appearance: Appearance,
    pub scroll_behavior: ScrollBehavior,
    pub overscroll_behavior_x: OverscrollBehavior,
    pub overscroll_behavior_y: OverscrollBehavior,
    pub pointer_events: PointerEvents,
    pub user_select: UserSelect,
    pub touch_action: TouchAction,
    pub scrollbar_width: ScrollbarWidth,
    pub scrollbar_color: ScrollbarColor,
    pub top: Option<Length>,
    pub right: Option<Length>,
    pub bottom: Option<Length>,
    pub left: Option<Length>,
    pub float: Float,
    pub clear: Clear,
    /// Stacking order for positioned elements (`auto` = None)
    pub z_index: Option<i32>,
    pub visibility: Visibility,
    /// Cursor property (inherited)
    pub cursor: CursorKeyword,
    /// Custom cursor images (in order of preference)
    pub cursor_images: Vec<CursorImage>,
    pub outline_color: OutlineColor,
    pub outline_style: OutlineStyle,
    pub outline_width: Length,
    pub outline_offset: Length,
    pub box_sizing: BoxSizing,
    pub container_type: ContainerType,
    /// Space-separated list of container names (empty when none).
    pub container_name: Vec<String>,

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
    /// Tracks cascade ordering and deferred logical box properties
    pub(crate) logical: LogicalState,

    // Flexbox
    pub flex_direction: FlexDirection,
    pub flex_wrap: FlexWrap,
    pub justify_content: JustifyContent,
    pub align_items: AlignItems,
    pub align_content: AlignContent,
    /// Align-self overrides the parent's align-items for this item (None represents `auto`)
    pub align_self: Option<AlignItems>,
    pub flex_grow: f32,
    pub flex_shrink: f32,
    pub flex_basis: FlexBasis,
    /// Flex item order (reordering within flex lines)
    pub order: i32,
    /// Justify-items / justify-self are used for grid/inflow alignment on the inline axis.
    pub justify_items: AlignItems,
    pub justify_self: Option<AlignItems>,

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
    pub font_variation_settings: Vec<FontVariationSetting>,
    pub font_optical_sizing: FontOpticalSizing,
    pub font_language_override: FontLanguageOverride,
    pub font_variant_emoji: FontVariantEmoji,
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
    pub text_rendering: TextRendering,
    pub text_indent: TextIndent,
    pub text_overflow: TextOverflow,
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
    pub text_combine_upright: TextCombineUpright,
    pub text_orientation: TextOrientation,
    pub writing_mode: WritingMode,
    pub letter_spacing: f32,
    pub word_spacing: f32,
    pub white_space: WhiteSpace,
    pub line_break: LineBreak,
    pub tab_size: TabSize,
    pub hyphens: HyphensMode,
    pub word_break: WordBreak,
    pub overflow_wrap: OverflowWrap,
    pub overflow_anchor: OverflowAnchor,
    pub vertical_align: VerticalAlign,
    /// True when vertical-align came from an authored/UA declaration or presentational hint.
    /// Used to distinguish default UA values from explicit ones when applying table row fallbacks.
    pub vertical_align_specified: bool,
    /// BCP47 language tag inherited from DOM (lang/xml:lang)
    pub language: String,
    pub list_style_type: ListStyleType,
    pub list_style_position: ListStylePosition,
    pub list_style_image: ListStyleImage,
    /// Counter properties (reset/increment/set)
    pub counters: CounterProperties,

    // Color and background
    pub color_scheme: ColorSchemePreference,
    pub caret_color: CaretColor,
    pub accent_color: AccentColor,
    pub color: Rgba,
    pub background_color: Rgba,
    /// Author-specified background values (lists preserved for layer repetition rules)
    pub background_images: Vec<Option<BackgroundImage>>,
    pub background_positions: Vec<BackgroundPosition>,
    pub background_sizes: Vec<BackgroundSize>,
    pub background_repeats: Vec<BackgroundRepeat>,
    pub background_attachments: Vec<BackgroundAttachment>,
    pub background_origins: Vec<BackgroundBox>,
    pub background_clips: Vec<BackgroundBox>,
    pub background_layers: Vec<BackgroundLayer>,
    pub background_blend_modes: Vec<MixBlendMode>,
    pub object_fit: ObjectFit,
    pub object_position: ObjectPosition,
    pub image_resolution: ImageResolution,
    pub image_orientation: ImageOrientation,
    pub image_rendering: ImageRendering,
    pub border_image: BorderImage,
    pub aspect_ratio: AspectRatio,
    /// CSS filter effects applied to this element
    pub filter: Vec<FilterFunction>,
    /// Backdrop filters applied to the backdrop behind this element
    pub backdrop_filter: Vec<FilterFunction>,
    /// Clip-path applied to the element and its contents
    pub clip_path: ClipPath,
    /// Legacy clip property (rect())
    pub clip: Option<ClipRect>,
    pub mix_blend_mode: MixBlendMode,
    pub isolation: Isolation,
    /// Author hints for upcoming changes
    pub will_change: WillChange,
    /// CSS containment
    pub containment: Containment,

    // Visual effects
    pub opacity: f32,
    pub box_shadow: Vec<BoxShadow>,
    pub text_shadow: Vec<TextShadow>,
    pub transform: Vec<Transform>,
    pub transform_box: TransformBox,
    pub transform_origin: TransformOrigin,
    pub transform_style: TransformStyle,
    pub backface_visibility: BackfaceVisibility,
    pub resize: Resize,
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
    pub content_value: crate::style::content::ContentValue,

    /// Quotes used by open-quote/close-quote
    pub quotes: Vec<(String, String)>,
}

impl Default for ComputedStyle {
    fn default() -> Self {
        let default_layer = BackgroundLayer::default();
        Self {
            display: Display::Inline,
            position: Position::Static,
            appearance: Appearance::Auto,
            scroll_behavior: ScrollBehavior::Auto,
            overscroll_behavior_x: OverscrollBehavior::Auto,
            overscroll_behavior_y: OverscrollBehavior::Auto,
            pointer_events: PointerEvents::Auto,
            user_select: UserSelect::Auto,
            touch_action: TouchAction::auto(),
            scrollbar_width: ScrollbarWidth::Auto,
            scrollbar_color: ScrollbarColor::Auto,
            top: None,
            right: None,
            bottom: None,
            left: None,
            float: Float::None,
            clear: Clear::None,
            z_index: None,
            visibility: Visibility::Visible,
            cursor: CursorKeyword::Auto,
            cursor_images: Vec::new(),
            outline_color: OutlineColor::Invert,
            outline_style: OutlineStyle::None,
            outline_width: Length::px(3.0),
            outline_offset: Length::px(0.0),
            box_sizing: BoxSizing::ContentBox,
            container_type: ContainerType::Normal,
            container_name: Vec::new(),

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
            logical: LogicalState::default(),

            flex_direction: FlexDirection::Row,
            flex_wrap: FlexWrap::NoWrap,
            justify_content: JustifyContent::FlexStart,
            align_items: AlignItems::Stretch,
            align_content: AlignContent::Stretch,
            align_self: None,
            flex_grow: 0.0,
            flex_shrink: 1.0,
            flex_basis: FlexBasis::Auto,
            order: 0,
            justify_items: AlignItems::Stretch,
            justify_self: None,

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
            font_variation_settings: Vec::new(),
            font_optical_sizing: FontOpticalSizing::Auto,
            font_language_override: FontLanguageOverride::Normal,
            font_variant_emoji: FontVariantEmoji::Normal,
            font_stretch: FontStretch::Normal,
            font_kerning: FontKerning::Auto,
            line_height: LineHeight::Normal,
            direction: Direction::Ltr,
            unicode_bidi: UnicodeBidi::Normal,
            root_font_size: 16.0,
            text_align: TextAlign::Start,
            text_align_last: TextAlignLast::Auto,
            text_justify: TextJustify::Auto,
            text_rendering: TextRendering::Auto,
            text_indent: TextIndent::default(),
            text_overflow: TextOverflow::clip(),
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
            text_combine_upright: TextCombineUpright::None,
            text_orientation: TextOrientation::Mixed,
            writing_mode: WritingMode::HorizontalTb,
            letter_spacing: 0.0,
            word_spacing: 0.0,
            white_space: WhiteSpace::Normal,
            line_break: LineBreak::Auto,
            tab_size: TabSize::default(),
            hyphens: HyphensMode::Manual,
            word_break: WordBreak::Normal,
            overflow_wrap: OverflowWrap::Normal,
            overflow_anchor: OverflowAnchor::Auto,
            vertical_align: VerticalAlign::Baseline,
            vertical_align_specified: false,
            language: "en".to_string(),
            list_style_type: ListStyleType::Disc,
            list_style_position: ListStylePosition::Outside,
            list_style_image: ListStyleImage::None,
            counters: CounterProperties::default(),

            color_scheme: ColorSchemePreference::Normal,
            caret_color: CaretColor::Auto,
            accent_color: AccentColor::Auto,
            color: Rgba::BLACK,
            background_color: Rgba::TRANSPARENT,
            background_images: vec![default_layer.image.clone()],
            background_positions: vec![default_layer.position.clone()],
            background_sizes: vec![default_layer.size.clone()],
            background_repeats: vec![default_layer.repeat],
            background_attachments: vec![default_layer.attachment],
            background_origins: vec![default_layer.origin],
            background_clips: vec![default_layer.clip],
            background_layers: vec![default_layer.clone()],
            background_blend_modes: vec![default_layer.blend_mode],
            object_fit: ObjectFit::Fill,
            object_position: ObjectPosition {
                x: types::PositionComponent::Keyword(types::PositionKeyword::Center),
                y: types::PositionComponent::Keyword(types::PositionKeyword::Center),
            },
            image_resolution: ImageResolution::default(),
            image_orientation: ImageOrientation::FromImage,
            image_rendering: ImageRendering::Auto,
            border_image: BorderImage::default(),
            aspect_ratio: AspectRatio::Auto,
            filter: Vec::new(),
            backdrop_filter: Vec::new(),
            clip_path: ClipPath::None,
            clip: None,
            mix_blend_mode: MixBlendMode::Normal,
            isolation: Isolation::Auto,
            will_change: WillChange::Auto,
            containment: Containment::none(),

            opacity: 1.0,
            box_shadow: Vec::new(),
            text_shadow: Vec::new(),
            transform: Vec::new(),
            transform_box: TransformBox::BorderBox,
            transform_origin: TransformOrigin {
                x: Length::percent(50.0),
                y: Length::percent(50.0),
            },
            transform_style: TransformStyle::Flat,
            backface_visibility: BackfaceVisibility::Visible,
            resize: Resize::None,
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
            content_value: crate::style::content::ContentValue::Normal,
            quotes: crate::style::content::default_quotes(),
        }
    }
}

impl ComputedStyle {
    fn ensure_background_lists(&mut self) {
        let defaults = BackgroundLayer::default();
        if self.background_images.is_empty() {
            self.background_images.push(defaults.image.clone());
        }
        if self.background_positions.is_empty() {
            self.background_positions.push(defaults.position.clone());
        }
        if self.background_sizes.is_empty() {
            self.background_sizes.push(defaults.size.clone());
        }
        if self.background_repeats.is_empty() {
            self.background_repeats.push(defaults.repeat);
        }
        if self.background_attachments.is_empty() {
            self.background_attachments.push(defaults.attachment);
        }
        if self.background_origins.is_empty() {
            self.background_origins.push(defaults.origin);
        }
        if self.background_clips.is_empty() {
            self.background_clips.push(defaults.clip);
        }
        if self.background_blend_modes.is_empty() {
            self.background_blend_modes.push(defaults.blend_mode);
        }
    }

    /// Rebuild per-layer background data from the stored author lists, repeating
    /// shorter lists by repeating their last value and truncating longer lists
    /// to the number of background-image layers.
    pub fn rebuild_background_layers(&mut self) {
        self.ensure_background_lists();
        let layer_count = self.background_images.len().max(1);
        self.background_layers.clear();
        for idx in 0..layer_count {
            let mut layer = BackgroundLayer::default();
            let img_idx = self.background_images.len().saturating_sub(1).min(idx);
            let pos_idx = self.background_positions.len().saturating_sub(1).min(idx);
            let size_idx = self.background_sizes.len().saturating_sub(1).min(idx);
            let rep_idx = self.background_repeats.len().saturating_sub(1).min(idx);
            let att_idx = self.background_attachments.len().saturating_sub(1).min(idx);
            let origin_idx = self.background_origins.len().saturating_sub(1).min(idx);
            let clip_idx = self.background_clips.len().saturating_sub(1).min(idx);
            let blend_idx = self.background_blend_modes.len().saturating_sub(1).min(idx);

            layer.image = self.background_images[img_idx].clone();
            layer.position = self.background_positions[pos_idx].clone();
            layer.size = self.background_sizes[size_idx].clone();
            layer.repeat = self.background_repeats[rep_idx];
            layer.attachment = self.background_attachments[att_idx];
            layer.origin = self.background_origins[origin_idx];
            layer.clip = self.background_clips[clip_idx];
            layer.blend_mode = self.background_blend_modes[blend_idx];
            self.background_layers.push(layer);
        }
    }

    /// Set background layers directly and derive the stored property lists from them.
    pub fn set_background_layers(&mut self, layers: Vec<BackgroundLayer>) {
        let default_layer = BackgroundLayer::default();
        let normalized = if layers.is_empty() {
            vec![default_layer.clone()]
        } else {
            layers
        };
        self.background_images = normalized.iter().map(|l| l.image.clone()).collect();
        self.background_positions = normalized.iter().map(|l| l.position.clone()).collect();
        self.background_sizes = normalized.iter().map(|l| l.size.clone()).collect();
        self.background_repeats = normalized.iter().map(|l| l.repeat).collect();
        self.background_attachments = normalized.iter().map(|l| l.attachment).collect();
        self.background_origins = normalized.iter().map(|l| l.origin).collect();
        self.background_clips = normalized.iter().map(|l| l.clip).collect();
        self.background_blend_modes = normalized.iter().map(|l| l.blend_mode).collect();
        self.background_layers = normalized;
    }

    /// Reset background color and layer properties to their initial values.
    pub fn reset_background_to_initial(&mut self) {
        self.background_color = Rgba::TRANSPARENT;
        self.set_background_layers(vec![BackgroundLayer::default()]);
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
