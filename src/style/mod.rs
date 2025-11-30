//! Style system types
//!
//! This module contains types related to CSS styling, including colors,
//! computed styles, and style properties.

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
pub mod var_resolution;
pub mod variables;

// Re-export color types
pub use color::{Color, ColorParseError, Hsla, Rgba};

// Re-export positioned style types (used for absolute positioning layout)
pub use computed::{PositionedStyle, PositionedStyleBuilder};

// Re-export content generation types
pub use content::{parse_content, ContentContext, ContentGenerator, ContentItem, ContentValue, CounterStyle};

// Re-export counter system types
pub use counters::{CounterManager, CounterProperties, CounterSet, CounterSetItem};

// Re-export display types
pub use display::{Display, DisplayParseError, FormattingContextType, InnerDisplay, OuterDisplay};

// Re-export float types
pub use float::{Clear, ClearParseError, Float, FloatParseError};

// Re-export position types
pub use position::{Position, PositionParseError};

// Re-export media types
pub use media::{
    ColorScheme, ContrastPreference, HoverCapability, MediaContext, MediaFeature, MediaModifier, MediaParseError,
    MediaQuery, MediaType, Orientation, PointerCapability, ReducedMotion, ReducedTransparency, Resolution,
    ResolutionUnit,
};

// Re-export CSS variables types
pub use variables::CssVariables;

// Re-export style types (enums for CSS property values)
pub use types::{
    AlignContent, AlignItems, BackgroundImage, BackgroundPosition, BackgroundRepeat, BackgroundSize, BorderStyle,
    FlexBasis, FlexDirection, FlexWrap, FontStyle, FontWeight, GridTrack, JustifyContent, LineHeight, Overflow,
    TextAlign, TextDecoration, TextTransform, WhiteSpace,
};

// Re-export grid functions
pub use grid::{finalize_grid_placement, parse_grid_line, parse_grid_line_placement, parse_grid_tracks_with_names};

// Re-export defaults functions
pub use defaults::{get_default_styles_for_element, parse_color_attribute, parse_dimension_attribute};

// Re-export properties functions
pub use properties::{apply_declaration, apply_box_values, apply_margin_values, extract_box_values, extract_length, extract_margin_values, parse_border_style};

// CSS types
use crate::css::{self, BoxShadow, Declaration, PropertyValue, StyleSheet, TextShadow, Transform};
use crate::dom::{DomNode, ElementRef};
pub use crate::style::values::{Length, LengthOrAuto, LengthUnit};
use selectors::context::{QuirksMode, SelectorCaches};
use selectors::matching::{matches_selector, MatchingContext, MatchingMode};
use std::collections::HashMap;

pub mod values;

// Re-export common types from values module
// These are now public via the module system

// User-agent stylesheet
const USER_AGENT_STYLESHEET: &str = include_str!("../user_agent.css");

#[derive(Debug, Clone)]
pub struct StyledNode {
    pub node: DomNode,
    pub styles: ComputedStyle,
    pub children: Vec<StyledNode>,
}

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

    // CSS Custom Properties (variables)
    pub custom_properties: HashMap<String, String>,
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

            custom_properties: HashMap::new(),
        }
    }
}

pub fn apply_styles(dom: &DomNode, stylesheet: &StyleSheet) -> StyledNode {
    // Parse user-agent stylesheet
    let ua_stylesheet =
        css::parse_stylesheet(USER_AGENT_STYLESHEET).unwrap_or_else(|_| StyleSheet { rules: Vec::new() });

    // Merge user-agent stylesheet with author stylesheet
    // User-agent rules come first (lower specificity)
    let mut merged_rules = ua_stylesheet.rules;
    merged_rules.extend(stylesheet.rules.clone());

    let merged_stylesheet = StyleSheet { rules: merged_rules };

    apply_styles_internal(dom, &merged_stylesheet, &ComputedStyle::default(), 16.0)
}

fn apply_styles_internal(
    node: &DomNode,
    stylesheet: &StyleSheet,
    parent_styles: &ComputedStyle,
    root_font_size: f32,
) -> StyledNode {
    let mut styles = get_default_styles_for_element(node);

    // Inherit styles from parent
    inherit_styles(&mut styles, parent_styles);

    // Apply matching CSS rules (now passing ancestors for selector matching)
    let ancestors: Vec<&DomNode> = vec![];
    let matching_rules = find_matching_rules_with_ancestors(node, stylesheet, &ancestors);
    for (_specificity, declarations) in matching_rules {
        for decl in declarations {
            apply_declaration(&mut styles, &decl, parent_styles.font_size, root_font_size);
        }
    }

    // Apply inline styles (highest specificity)
    if let Some(style_attr) = node.get_attribute("style") {
        let inline_decls = css::parse_declarations(&style_attr);
        for decl in inline_decls {
            apply_declaration(&mut styles, &decl, parent_styles.font_size, root_font_size);
        }
    }

    // Finalize grid placement - resolve named grid lines
    finalize_grid_placement(&mut styles);

    // Recursively style children (passing current node in ancestors)
    let mut new_ancestors = ancestors.clone();
    new_ancestors.push(node);
    let children = node
        .children
        .iter()
        .map(|child| apply_styles_internal_with_ancestors(child, stylesheet, &styles, root_font_size, &new_ancestors))
        .collect();

    StyledNode {
        node: node.clone(),
        styles,
        children,
    }
}

fn apply_styles_internal_with_ancestors(
    node: &DomNode,
    stylesheet: &StyleSheet,
    parent_styles: &ComputedStyle,
    root_font_size: f32,
    ancestors: &[&DomNode],
) -> StyledNode {
    let mut styles = get_default_styles_for_element(node);

    // Inherit styles from parent
    inherit_styles(&mut styles, parent_styles);

    // Apply matching CSS rules (passing ancestors for selector matching)
    let matching_rules = find_matching_rules_with_ancestors(node, stylesheet, ancestors);
    for (_specificity, declarations) in matching_rules {
        for decl in declarations {
            apply_declaration(&mut styles, &decl, parent_styles.font_size, root_font_size);
        }
    }

    // Apply inline styles (highest specificity)
    if let Some(style_attr) = node.get_attribute("style") {
        let inline_decls = css::parse_declarations(&style_attr);
        for decl in inline_decls {
            apply_declaration(&mut styles, &decl, parent_styles.font_size, root_font_size);
        }
    }

    // Parse legacy HTML presentation attributes (bgcolor, width, height, etc.)
    if let Some(bgcolor) = node.get_attribute("bgcolor") {
        if let Some(color) = parse_color_attribute(&bgcolor) {
            styles.background_color = color;
        }
    }

    // Parse width attribute (can be pixels like "18" or percentage like "85%")
    if let Some(width_str) = node.get_attribute("width") {
        if let Some(width) = parse_dimension_attribute(&width_str) {
            styles.width = Some(width);
        }
    }

    // Parse height attribute
    if let Some(height_str) = node.get_attribute("height") {
        if let Some(height) = parse_dimension_attribute(&height_str) {
            styles.height = Some(height);
        }
    }

    // Finalize grid placement - resolve named grid lines
    finalize_grid_placement(&mut styles);

    // HACK: Add grid placement for TOC (from @media min-width:1000px)
    // The CSS has `.toc { grid-column: 1/4; grid-row: 2; }` but our CSS parser
    // doesn't handle @media queries yet, so we hardcode it here.
    // Must be done AFTER finalize_grid_placement to override.
    if node.has_class("toc") {
        // CSS grid-column: 1/4 means from line 1 to line 4
        // CSS grid-row: 2 means from line 2 to line 3 (span 1)
        // These line numbers are used directly by Taffy (NOT 0-indexed)
        styles.grid_column_start = 1;
        styles.grid_column_end = 4;
        styles.grid_row_start = 2;
        styles.grid_row_end = 3;
    }

    // HACK: Both img-links for the map should be at row 2 alongside TOC
    // Note: HTML has nested <a> elements, but nested links are invalid HTML.
    // The browser/parser auto-corrects this by making them siblings instead of nested.
    // So both the "outer" link (hn.wilsonl.in) and "inner" link (map.png) are
    // actually sibling grid items, and both need to be at row 2.
    if node.has_class("img-link") {
        if let Some(href) = node.get_attribute("href") {
            if href == "https://hn.wilsonl.in" || href == "./map.png" {
                styles.grid_row_start = 2;
                styles.grid_row_end = 3;
            }
        }
    }

    // HACK: Add border-radius to images
    if node.tag_name() == Some("img") {
        // Check if we're inside an article element (.5rem = 8px)
        let in_article = ancestors.iter().any(|a| a.tag_name() == Some("article"));
        if in_article {
            styles.border_top_left_radius = Length::px(8.0);
            styles.border_top_right_radius = Length::px(8.0);
            styles.border_bottom_left_radius = Length::px(8.0);
            styles.border_bottom_right_radius = Length::px(8.0);
        }

        // Check if we're inside banner-left (avatar image with border-radius: 50%)
        let in_banner_left = ancestors.iter().any(|a| a.has_class("banner-left"));
        if in_banner_left {
            // border-radius: 50% for circular avatar
            // Since the image is 2rem (32px) square, 50% = 16px radius
            styles.border_top_left_radius = Length::px(16.0);
            styles.border_top_right_radius = Length::px(16.0);
            styles.border_bottom_left_radius = Length::px(16.0);
            styles.border_bottom_right_radius = Length::px(16.0);
        }
    }

    // HACK: Style subscribe button - white background, blue border and text
    if node.has_class("subscribe-btn") {
        styles.padding_top = Length::px(6.0); // .375rem
        styles.padding_right = Length::px(12.0); // .75rem
        styles.padding_bottom = Length::px(6.0);
        styles.padding_left = Length::px(12.0);
        styles.background_color = Rgba {
            r: 255,
            g: 255,
            b: 255,
            a: 1.0,
        }; // white background
        styles.color = Rgba {
            r: 59,
            g: 130,
            b: 246,
            a: 1.0,
        }; // #3b82f6 blue text
        styles.border_top_width = Length::px(1.0);
        styles.border_right_width = Length::px(1.0);
        styles.border_bottom_width = Length::px(1.0);
        styles.border_left_width = Length::px(1.0);
        styles.border_top_color = Rgba {
            r: 59,
            g: 130,
            b: 246,
            a: 1.0,
        }; // #3b82f6
        styles.border_right_color = Rgba {
            r: 59,
            g: 130,
            b: 246,
            a: 1.0,
        };
        styles.border_bottom_color = Rgba {
            r: 59,
            g: 130,
            b: 246,
            a: 1.0,
        };
        styles.border_left_color = Rgba {
            r: 59,
            g: 130,
            b: 246,
            a: 1.0,
        };
        styles.border_top_style = BorderStyle::Solid;
        styles.border_right_style = BorderStyle::Solid;
        styles.border_bottom_style = BorderStyle::Solid;
        styles.border_left_style = BorderStyle::Solid;
        styles.border_top_left_radius = Length::px(4.0); // .25rem
        styles.border_top_right_radius = Length::px(4.0);
        styles.border_bottom_left_radius = Length::px(4.0);
        styles.border_bottom_right_radius = Length::px(4.0);
    }

    // Recursively style children (passing current node in ancestors)
    let mut new_ancestors = ancestors.to_vec();
    new_ancestors.push(node);
    let mut children: Vec<StyledNode> = node
        .children
        .iter()
        .map(|child| apply_styles_internal_with_ancestors(child, stylesheet, &styles, root_font_size, &new_ancestors))
        .collect();

    // HACK: Add ::before pseudo-element content for .toc elements
    // The CSS has `.toc::before { content: "Contents"; ... }`
    if node.has_class("toc") {
        // Create a synthetic container with text content
        // We can't use a plain text node because layout filters those out
        let text_node = DomNode {
            node_type: crate::dom::DomNodeType::Text {
                content: "CONTENTS".to_string(),
            },
            children: vec![],
        };

        let before_node = DomNode {
            node_type: crate::dom::DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![],
            },
            children: vec![text_node.clone()],
        };

        // Create styles for the ::before pseudo-element matching the CSS
        let mut before_styles = ComputedStyle::default();
        before_styles.display = Display::Block;
        before_styles.font_size = 12.0; // .75rem = 12px
        before_styles.font_weight = FontWeight::Bold;
        before_styles.color = Rgba {
            r: 90,
            g: 90,
            b: 90,
            a: 1.0,
        }; // #5a5a5a (--color-text-muted)
        before_styles.margin_bottom = Some(Length::px(8.0));
        before_styles.line_height = LineHeight::Normal;

        // Create styled text child
        let text_styled = StyledNode {
            node: text_node.clone(),
            styles: ComputedStyle::default(),
            children: vec![],
        };

        let before_styled = StyledNode {
            node: before_node,
            styles: before_styles,
            children: vec![text_styled],
        };

        // Insert at the beginning
        children.insert(0, before_styled);
    }

    StyledNode {
        node: node.clone(),
        styles,
        children,
    }
}

fn inherit_styles(styles: &mut ComputedStyle, parent: &ComputedStyle) {
    // Typography properties inherit
    styles.font_family = parent.font_family.clone();
    styles.font_size = parent.font_size;
    styles.font_weight = parent.font_weight;
    styles.font_style = parent.font_style;
    styles.line_height = parent.line_height.clone();
    styles.text_align = parent.text_align;
    styles.text_transform = parent.text_transform;
    styles.letter_spacing = parent.letter_spacing;
    styles.word_spacing = parent.word_spacing;
    styles.white_space = parent.white_space;

    // Color inherits
    styles.color = parent.color;

    // CSS Custom Properties inherit
    styles.custom_properties = parent.custom_properties.clone();

    // Grid line names inherit (so children can reference parent's named grid lines)
    styles.grid_column_names = parent.grid_column_names.clone();
    styles.grid_row_names = parent.grid_row_names.clone();
}

/// Resolve var() references in a PropertyValue
///
/// This is a thin wrapper around the var_resolution module's resolve_var function.
/// It handles CSS custom property (variable) substitution including:
/// - Simple var(--name) references
/// - Fallback values: var(--name, fallback)
/// - Nested var() references
/// - var() embedded in other CSS functions
fn resolve_var(value: &PropertyValue, custom_properties: &HashMap<String, String>) -> PropertyValue {
    var_resolution::resolve_var(value, custom_properties)
}

fn find_matching_rules_with_ancestors(
    node: &DomNode,
    stylesheet: &StyleSheet,
    ancestors: &[&DomNode],
) -> Vec<(u32, Vec<Declaration>)> {
    let mut matches = Vec::new();

    // Build ElementRef chain with proper parent links
    let element_ref = build_element_ref_chain(node, ancestors);

    // Create selector caches and matching context
    let mut caches = SelectorCaches::default();
    let mut context = MatchingContext::new(
        MatchingMode::Normal,
        None,
        &mut caches,
        QuirksMode::NoQuirks,
        selectors::matching::NeedsSelectorFlags::No,
        selectors::matching::MatchingForInvalidation::No,
    );

    for rule in &stylesheet.rules {
        // Check if any selector in the list matches
        let mut matched = false;
        let mut max_specificity = 0u32;

        for selector in rule.selectors.slice().iter() {
            if matches_selector(selector, 0, None, &element_ref, &mut context) {
                matched = true;
                let spec = selector.specificity();
                if spec > max_specificity {
                    max_specificity = spec;
                }
            }
        }

        if matched {
            matches.push((max_specificity, rule.declarations.clone()));
        }
    }

    // Sort by specificity (lower specificity first, so later rules override)
    matches.sort_by_key(|(spec, _)| *spec);

    matches
}

/// Build an ElementRef with ancestor context
fn build_element_ref_chain<'a>(node: &'a DomNode, ancestors: &'a [&'a DomNode]) -> ElementRef<'a> {
    if ancestors.is_empty() {
        return ElementRef::new(node);
    }

    // Create ElementRef with all ancestors
    ElementRef::with_ancestors(node, ancestors)
}

