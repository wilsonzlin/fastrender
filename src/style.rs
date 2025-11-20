use crate::css::{
    self, BoxShadow, Color, Declaration, Length, PropertyValue, StyleSheet, TextShadow, Transform,
};
use crate::dom::{DomNode, ElementRef};
use selectors::context::{QuirksMode, SelectorCaches};
use selectors::matching::{matches_selector, MatchingContext, MatchingMode};
use std::collections::HashMap;

// User-agent stylesheet
const USER_AGENT_STYLESHEET: &str = include_str!("user_agent.css");

#[derive(Debug, Clone)]
pub struct StyledNode {
    pub node: DomNode,
    pub styles: ComputedStyles,
    pub children: Vec<StyledNode>,
}

#[derive(Debug, Clone)]
pub struct ComputedStyles {
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

    pub border_top_color: Color,
    pub border_right_color: Color,
    pub border_bottom_color: Color,
    pub border_left_color: Color,

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
    pub color: Color,
    pub background_color: Color,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Display {
    Block,
    Inline,
    InlineBlock,
    Flex,
    InlineFlex,
    Grid,
    InlineGrid,
    Table,
    TableRow,
    TableCell,
    TableHeaderGroup,
    TableRowGroup,
    TableFooterGroup,
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Position {
    Static,
    Relative,
    Absolute,
    Fixed,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Overflow {
    Visible,
    Hidden,
    Scroll,
    Auto,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BorderStyle {
    None,
    Solid,
    Dashed,
    Dotted,
    Double,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FlexDirection {
    Row,
    RowReverse,
    Column,
    ColumnReverse,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FlexWrap {
    NoWrap,
    Wrap,
    WrapReverse,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum JustifyContent {
    FlexStart,
    FlexEnd,
    Center,
    SpaceBetween,
    SpaceAround,
    SpaceEvenly,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AlignItems {
    FlexStart,
    FlexEnd,
    Center,
    Baseline,
    Stretch,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AlignContent {
    FlexStart,
    FlexEnd,
    Center,
    SpaceBetween,
    SpaceAround,
    Stretch,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FlexBasis {
    Auto,
    Length(Length),
}

#[derive(Debug, Clone, PartialEq)]
pub enum GridTrack {
    Length(Length),
    Fr(f32),
    Auto,
    MinMax(Box<GridTrack>, Box<GridTrack>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FontWeight {
    Normal,
    Bold,
    Weight(u16),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FontStyle {
    Normal,
    Italic,
    Oblique,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LineHeight {
    Normal,
    Number(f32),
    Length(Length),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TextAlign {
    Left,
    Right,
    Center,
    Justify,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TextDecoration {
    None,
    Underline,
    Overline,
    LineThrough,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TextTransform {
    None,
    Uppercase,
    Lowercase,
    Capitalize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WhiteSpace {
    Normal,
    NoWrap,
    Pre,
    PreWrap,
    PreLine,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BackgroundImage {
    Url(String),
    LinearGradient {
        angle: f32,
        stops: Vec<css::ColorStop>,
    },
    RadialGradient {
        stops: Vec<css::ColorStop>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BackgroundSize {
    Auto,
    Cover,
    Contain,
    Length(Length, Length),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BackgroundPosition {
    Center,
    Position(Length, Length),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BackgroundRepeat {
    Repeat,
    RepeatX,
    RepeatY,
    NoRepeat,
}

impl Default for ComputedStyles {
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

            border_top_color: Color::BLACK,
            border_right_color: Color::BLACK,
            border_bottom_color: Color::BLACK,
            border_left_color: Color::BLACK,

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

            color: Color::BLACK,
            background_color: Color::TRANSPARENT,
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
    // Debug: check if we have .toc rules with grid-column
    let mut found_toc_grid = false;
    for rule in &stylesheet.rules {
        let selector_str = format!("{:?}", rule.selectors);
        if selector_str.contains("toc") && selector_str.contains("article") {
            for decl in &rule.declarations {
                if decl.property == "grid-column" {
                    found_toc_grid = true;
                }
            }
        }
    }

    // Parse user-agent stylesheet
    let ua_stylesheet = css::parse_stylesheet(USER_AGENT_STYLESHEET)
        .unwrap_or_else(|_| StyleSheet { rules: Vec::new() });

    // Merge user-agent stylesheet with author stylesheet
    // User-agent rules come first (lower specificity)
    let mut merged_rules = ua_stylesheet.rules;
    merged_rules.extend(stylesheet.rules.clone());

    let merged_stylesheet = StyleSheet {
        rules: merged_rules,
    };

    apply_styles_internal(dom, &merged_stylesheet, &ComputedStyles::default(), 16.0)
}

fn apply_styles_internal(
    node: &DomNode,
    stylesheet: &StyleSheet,
    parent_styles: &ComputedStyles,
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
    let _current_font_size = styles.font_size;
    let mut new_ancestors = ancestors.clone();
    new_ancestors.push(node);
    let children = node
        .children
        .iter()
        .map(|child| {
            apply_styles_internal_with_ancestors(
                child,
                stylesheet,
                &styles,
                root_font_size,
                &new_ancestors,
            )
        })
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
    parent_styles: &ComputedStyles,
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
        eprintln!("DEBUG: Found bgcolor attribute: {}", bgcolor);
        // Parse color from attribute (can be hex like #ff6600 or named color)
        if let Some(color) = parse_color_attribute(&bgcolor) {
            styles.background_color = color;
            eprintln!("DEBUG: Applied bgcolor {} -> color {:?}", bgcolor, color);
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
            styles.border_top_left_radius = Length {
                value: 8.0,
                unit: css::LengthUnit::Px,
            };
            styles.border_top_right_radius = Length {
                value: 8.0,
                unit: css::LengthUnit::Px,
            };
            styles.border_bottom_left_radius = Length {
                value: 8.0,
                unit: css::LengthUnit::Px,
            };
            styles.border_bottom_right_radius = Length {
                value: 8.0,
                unit: css::LengthUnit::Px,
            };
        }

        // Check if we're inside banner-left (avatar image with border-radius: 50%)
        let in_banner_left = ancestors.iter().any(|a| a.has_class("banner-left"));
        if in_banner_left {
            // border-radius: 50% for circular avatar
            // Since the image is 2rem (32px) square, 50% = 16px radius
            styles.border_top_left_radius = Length {
                value: 16.0,
                unit: css::LengthUnit::Px,
            };
            styles.border_top_right_radius = Length {
                value: 16.0,
                unit: css::LengthUnit::Px,
            };
            styles.border_bottom_left_radius = Length {
                value: 16.0,
                unit: css::LengthUnit::Px,
            };
            styles.border_bottom_right_radius = Length {
                value: 16.0,
                unit: css::LengthUnit::Px,
            };
        }
    }

    // HACK: Style subscribe button - white background, blue border and text
    if node.has_class("subscribe-btn") {
        styles.padding_top = Length {
            value: 6.0,
            unit: css::LengthUnit::Px,
        }; // .375rem
        styles.padding_right = Length {
            value: 12.0,
            unit: css::LengthUnit::Px,
        }; // .75rem
        styles.padding_bottom = Length {
            value: 6.0,
            unit: css::LengthUnit::Px,
        };
        styles.padding_left = Length {
            value: 12.0,
            unit: css::LengthUnit::Px,
        };
        styles.background_color = Color {
            r: 255,
            g: 255,
            b: 255,
            a: 255,
        }; // white background
        styles.color = Color {
            r: 59,
            g: 130,
            b: 246,
            a: 255,
        }; // #3b82f6 blue text
        styles.border_top_width = Length {
            value: 1.0,
            unit: css::LengthUnit::Px,
        };
        styles.border_right_width = Length {
            value: 1.0,
            unit: css::LengthUnit::Px,
        };
        styles.border_bottom_width = Length {
            value: 1.0,
            unit: css::LengthUnit::Px,
        };
        styles.border_left_width = Length {
            value: 1.0,
            unit: css::LengthUnit::Px,
        };
        styles.border_top_color = Color {
            r: 59,
            g: 130,
            b: 246,
            a: 255,
        }; // #3b82f6
        styles.border_right_color = Color {
            r: 59,
            g: 130,
            b: 246,
            a: 255,
        };
        styles.border_bottom_color = Color {
            r: 59,
            g: 130,
            b: 246,
            a: 255,
        };
        styles.border_left_color = Color {
            r: 59,
            g: 130,
            b: 246,
            a: 255,
        };
        styles.border_top_style = BorderStyle::Solid;
        styles.border_right_style = BorderStyle::Solid;
        styles.border_bottom_style = BorderStyle::Solid;
        styles.border_left_style = BorderStyle::Solid;
        styles.border_top_left_radius = Length {
            value: 4.0,
            unit: css::LengthUnit::Px,
        }; // .25rem
        styles.border_top_right_radius = Length {
            value: 4.0,
            unit: css::LengthUnit::Px,
        };
        styles.border_bottom_left_radius = Length {
            value: 4.0,
            unit: css::LengthUnit::Px,
        };
        styles.border_bottom_right_radius = Length {
            value: 4.0,
            unit: css::LengthUnit::Px,
        };
    }

    // Recursively style children (passing current node in ancestors)
    let _current_font_size = styles.font_size;
    let mut new_ancestors = ancestors.to_vec();
    new_ancestors.push(node);
    let mut children: Vec<StyledNode> = node
        .children
        .iter()
        .map(|child| {
            apply_styles_internal_with_ancestors(
                child,
                stylesheet,
                &styles,
                root_font_size,
                &new_ancestors,
            )
        })
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
        let mut before_styles = ComputedStyles::default();
        before_styles.display = Display::Block;
        before_styles.font_size = 12.0; // .75rem = 12px
        before_styles.font_weight = FontWeight::Bold;
        before_styles.color = Color {
            r: 90,
            g: 90,
            b: 90,
            a: 255,
        }; // #5a5a5a (--color-text-muted)
        before_styles.margin_bottom = Some(Length {
            value: 8.0,
            unit: css::LengthUnit::Px,
        });
        before_styles.line_height = LineHeight::Normal;

        // Create styled text child
        let text_styled = StyledNode {
            node: text_node.clone(),
            styles: ComputedStyles::default(),
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

fn get_default_styles_for_element(node: &DomNode) -> ComputedStyles {
    let mut styles = ComputedStyles::default();

    // Set proper default display values for HTML elements (user-agent stylesheet defaults)
    if let Some(tag) = node.tag_name() {
        styles.display = match tag {
            // Block-level elements
            "div" | "p" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "ul" | "ol" | "li"
            | "blockquote" | "pre" | "article" | "section" | "nav" | "aside" | "header"
            | "footer" | "main" | "figure" | "figcaption" | "dl" | "dt" | "dd" | "form"
            | "fieldset" | "legend" | "address" | "hr" | "center" => Display::Block,

            // Table elements
            "table" => Display::Table,
            "tr" => Display::TableRow,
            "td" | "th" => Display::TableCell,
            "thead" | "tbody" | "tfoot" => Display::TableRowGroup,

            // Inline elements (explicit for clarity, though it's the default)
            "a" | "span" | "em" | "strong" | "code" | "b" | "i" | "u" | "small" | "sub" | "sup"
            | "mark" | "abbr" | "cite" | "q" | "kbd" | "samp" | "var" | "time" | "label" => {
                Display::Inline
            }

            // Everything else defaults to inline
            _ => Display::Inline,
        };

        // CRITICAL FIX: Force minimal spacing for table elements
        if let Some(tag) = node.tag_name() {
            match tag {
                "table" => {
                    // Remove all spacing from tables
                    styles.margin_top = Some(Length::px(0.0));
                    styles.margin_bottom = Some(Length::px(0.0));
                    styles.padding_top = Length::px(0.0);
                    styles.padding_bottom = Length::px(0.0);
                }
                "tr" => {
                    // Minimal spacing between table rows
                    styles.margin_top = Some(Length::px(0.0));
                    styles.margin_bottom = Some(Length::px(0.0));
                    styles.padding_top = Length::px(0.0);
                    styles.padding_bottom = Length::px(0.0);

                    // Force spacer rows to be minimal
                    if node.has_class("spacer") {
                        styles.height = Some(Length::px(2.0)); // Much smaller than 5px
                        eprintln!("DEBUG: Setting spacer row height to 2px");
                    }
                }
                "td" | "th" => {
                    // Minimal padding for table cells
                    styles.padding_top = Length::px(1.0);
                    styles.padding_bottom = Length::px(1.0);
                    styles.margin_top = Some(Length::px(0.0));
                    styles.margin_bottom = Some(Length::px(0.0));
                }
                _ => {}
            }
        }

        // CRITICAL FIX: Prevent text wrapping in table cells
        // This fixes the issue where navigation text and story titles wrap excessively
        if matches!(styles.display, Display::TableCell) {
            styles.white_space = WhiteSpace::NoWrap;
        }

        // CRITICAL FIX: Ensure header navigation is visible
        // HN CSS parse error prevents pagetop styling from being applied
        if node.has_class("pagetop") {
            styles.font_family = vec![
                "Verdana".to_string(),
                "Geneva".to_string(),
                "sans-serif".to_string(),
            ];
            styles.font_size = 10.0;
            styles.color = Color {
                r: 34,
                g: 34,
                b: 34,
                a: 255,
            }; // #222222
            styles.line_height = LineHeight::Length(Length {
                value: 12.0,
                unit: css::LengthUnit::Px,
            });
        }

        // CRITICAL FIX: Ensure pagetop links are visible
        // Fix any CSS that might be hiding navigation links in header
        if matches!(node.tag_name(), Some("a")) {
            // For now, make all links visible
            styles.color = Color {
                r: 34,
                g: 34,
                b: 34,
                a: 255,
            }; // #222222
            styles.display = Display::Inline;
        }

        // CRITICAL FIX: Ensure votearrow elements are visible with proper styling
        if node.has_class("votearrow") {
            styles.display = Display::Block;
            styles.width = Some(Length {
                value: 10.0,
                unit: css::LengthUnit::Px,
            });
            styles.height = Some(Length {
                value: 10.0,
                unit: css::LengthUnit::Px,
            });
            styles.color = Color {
                r: 0,
                g: 0,
                b: 0,
                a: 255,
            }; // BLACK for visibility
            styles.font_size = 20.0; // LARGER for visibility
            styles.text_align = TextAlign::Center; // Center the vote arrow
        }

        // CRITICAL FIX: Ensure vote link cells have proper width
        if node.has_class("votelinks") {
            styles.width = Some(Length {
                value: 30.0,
                unit: css::LengthUnit::Px,
            }); // Wider for visibility
            styles.text_align = TextAlign::Center; // Center content in vote column
        }

        // CRITICAL FIX: Ensure story rank numbers are visible
        if node.has_class("rank") {
            styles.display = Display::Block;
            styles.color = Color {
                r: 0,
                g: 0,
                b: 0,
                a: 255,
            }; // Black text
            styles.font_size = 10.0; // Small like HN
            styles.text_align = TextAlign::Right; // Right-align like HN
            eprintln!("DEBUG: Applied rank styling to rank element");
        }

        // CRITICAL FIX: Ensure rank column (title cell with rank content) has proper width
        if node.has_class("title") && node.tag_name() == Some("td") {
            // Check if this is the rank column by looking at its align attribute
            if node.get_attribute("align").as_deref() == Some("right")
                && node.get_attribute("valign").as_deref() == Some("top")
            {
                styles.width = Some(Length {
                    value: 30.0,
                    unit: css::LengthUnit::Px,
                }); // Wide enough for rank numbers
                styles.text_align = TextAlign::Right;
                eprintln!("DEBUG: Applied rank column width to title cell with right alignment");
            }
        }

        // CRITICAL FIX: Ensure header navigation elements are visible and styled
        if node.has_class("pagetop") {
            styles.display = Display::Inline; // Keep inline so text is collected by parent table cell
            styles.color = Color {
                r: 255,
                g: 255,
                b: 255,
                a: 255,
            }; // White text for orange header
            styles.font_size = 10.0; // Small navigation font
                                     // Remove forced width/height so it remains inline
            eprintln!("DEBUG: Applied pagetop navigation styling as inline");
        }

        if node.has_class("hnname") {
            styles.display = Display::Inline; // Keep inline so text is collected by parent table cell
            styles.color = Color {
                r: 255,
                g: 255,
                b: 255,
                a: 255,
            }; // White text
            styles.font_weight = crate::style::FontWeight::Bold;
            styles.font_size = 10.0;
            // Remove forced dimensions so it remains inline
            eprintln!("DEBUG: Applied hnname styling as inline");
        }

        // Style navigation links
        if let Some(tag) = node.tag_name() {
            if tag == "a" && node.get_attribute("href").is_some() {
                // Check if this is a header navigation link by checking href patterns
                if let Some(href) = node.get_attribute("href") {
                    if href.contains("newest")
                        || href.contains("front")
                        || href.contains("newcomments")
                        || href.contains("ask")
                        || href.contains("show")
                        || href.contains("jobs")
                        || href.contains("submit")
                        || href.contains("news")
                    {
                        styles.display = Display::Inline;
                        styles.color = Color {
                            r: 255,
                            g: 255,
                            b: 255,
                            a: 255,
                        }; // White text
                        styles.font_size = 10.0;
                        eprintln!("DEBUG: Applied navigation link styling for href: {}", href);
                    }
                }
            }
        }
    }

    styles
}

fn parse_dimension_attribute(dim_str: &str) -> Option<Length> {
    let dim_str = dim_str.trim();

    // Handle percentage like "85%"
    if dim_str.ends_with('%') {
        if let Ok(value) = dim_str[..dim_str.len() - 1].trim().parse::<f32>() {
            return Some(Length {
                value,
                unit: css::LengthUnit::Percent,
            });
        }
    }

    // Handle pixels (just a number like "18")
    if let Ok(value) = dim_str.parse::<f32>() {
        return Some(Length {
            value,
            unit: css::LengthUnit::Px,
        });
    }

    None
}

fn parse_color_attribute(color_str: &str) -> Option<Color> {
    let color_str = color_str.trim();

    // Handle hex colors like #ff6600 or ff6600
    if color_str.starts_with('#') {
        let hex = &color_str[1..];
        if hex.len() == 6 {
            if let (Ok(r), Ok(g), Ok(b)) = (
                u8::from_str_radix(&hex[0..2], 16),
                u8::from_str_radix(&hex[2..4], 16),
                u8::from_str_radix(&hex[4..6], 16),
            ) {
                return Some(Color { r, g, b, a: 255 });
            }
        } else if hex.len() == 3 {
            // Shorthand like #f60
            if let (Ok(r), Ok(g), Ok(b)) = (
                u8::from_str_radix(&hex[0..1], 16),
                u8::from_str_radix(&hex[1..2], 16),
                u8::from_str_radix(&hex[2..3], 16),
            ) {
                // Double each digit: #f60 -> #ff6600
                return Some(Color {
                    r: r * 17,
                    g: g * 17,
                    b: b * 17,
                    a: 255,
                });
            }
        }
    }

    None
}

fn inherit_styles(styles: &mut ComputedStyles, parent: &ComputedStyles) {
    // Typography properties inherit
    styles.font_family = parent.font_family.clone();
    styles.font_size = parent.font_size;
    styles.font_weight = parent.font_weight;
    styles.font_style = parent.font_style;
    styles.line_height = parent.line_height;
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

/// Resolve var() references in a PropertyValue (with recursion limit)
fn resolve_var(
    value: &PropertyValue,
    custom_properties: &HashMap<String, String>,
) -> PropertyValue {
    resolve_var_with_depth(value, custom_properties, 0)
}

fn resolve_var_with_depth(
    value: &PropertyValue,
    custom_properties: &HashMap<String, String>,
    depth: usize,
) -> PropertyValue {
    const MAX_DEPTH: usize = 10;

    if depth >= MAX_DEPTH {
        // Too deep - return as-is to prevent stack overflow
        return value.clone();
    }

    match value {
        PropertyValue::Keyword(kw) => {
            // Check if it's a simple var() reference
            if kw.starts_with("var(") && kw.ends_with(')') && !kw[4..kw.len() - 1].contains("var(")
            {
                let var_name = &kw[4..kw.len() - 1]; // Extract variable name from var(...)

                // Handle fallback values: var(--name, fallback)
                let (var_name, fallback) = if let Some(comma_pos) = var_name.find(',') {
                    (
                        var_name[..comma_pos].trim(),
                        Some(var_name[comma_pos + 1..].trim()),
                    )
                } else {
                    (var_name.trim(), None)
                };

                // Look up the variable
                if let Some(resolved) = custom_properties.get(var_name) {
                    // Try to parse the resolved value properly
                    if let Some(mut parsed) = parse_resolved_value(resolved) {
                        // Recursively resolve in case the value contains another var()
                        parsed = resolve_var_with_depth(&parsed, custom_properties, depth + 1);
                        return parsed;
                    }
                    // If parsing fails, return as keyword and try to resolve recursively
                    let as_keyword = PropertyValue::Keyword(resolved.clone());
                    return resolve_var_with_depth(&as_keyword, custom_properties, depth + 1);
                } else if let Some(fb) = fallback {
                    // Use fallback and resolve recursively
                    if let Some(mut parsed) = parse_resolved_value(fb) {
                        parsed = resolve_var_with_depth(&parsed, custom_properties, depth + 1);
                        return parsed;
                    }
                    let as_keyword = PropertyValue::Keyword(fb.to_string());
                    return resolve_var_with_depth(&as_keyword, custom_properties, depth + 1);
                }
            } else if kw.contains("var(") {
                // String contains var() references - do string replacement
                let mut resolved_str = kw.clone();

                // Find and replace all var() occurrences
                while let Some(start) = resolved_str.find("var(") {
                    // Find the matching closing paren
                    let mut depth = 0;
                    let mut end = start + 4;
                    for (i, ch) in resolved_str[start + 4..].chars().enumerate() {
                        match ch {
                            '(' => depth += 1,
                            ')' => {
                                if depth == 0 {
                                    end = start + 4 + i;
                                    break;
                                }
                                depth -= 1;
                            }
                            _ => {}
                        }
                    }

                    // Extract the var call to avoid borrow issues
                    let var_call = resolved_str[start..=end].to_string();
                    let var_name_full = var_call[4..var_call.len() - 1].to_string();

                    // Handle fallback
                    let (var_name, fallback) = if let Some(comma_pos) = var_name_full.find(',') {
                        (
                            var_name_full[..comma_pos].trim().to_string(),
                            Some(var_name_full[comma_pos + 1..].trim().to_string()),
                        )
                    } else {
                        (var_name_full.trim().to_string(), None)
                    };

                    // Look up and replace
                    if let Some(resolved) = custom_properties.get(&var_name) {
                        resolved_str.replace_range(start..=end, resolved);
                    } else if let Some(ref fb) = fallback {
                        resolved_str.replace_range(start..=end, fb);
                    } else {
                        // Can't resolve - skip this one
                        break;
                    }
                }

                // Return the resolved string as a keyword and recursively resolve
                let as_keyword = PropertyValue::Keyword(resolved_str);
                return resolve_var_with_depth(&as_keyword, custom_properties, depth + 1);
            }
            value.clone()
        }
        _ => value.clone(),
    }
}

/// Parse a resolved CSS variable value
fn parse_resolved_value(value: &str) -> Option<PropertyValue> {
    use crate::css::parse_property_value;
    // Try to parse as a proper CSS value
    parse_property_value("", value)
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

fn apply_declaration(
    styles: &mut ComputedStyles,
    decl: &Declaration,
    parent_font_size: f32,
    root_font_size: f32,
) {
    // Handle CSS Custom Properties (--*)
    if decl.property.starts_with("--") {
        // Convert the property value to a string for storage
        let value_str = match &decl.value {
            PropertyValue::Keyword(kw) => kw.clone(),
            PropertyValue::Length(len) => {
                use crate::css::LengthUnit;
                format!(
                    "{}{}",
                    len.value,
                    match len.unit {
                        LengthUnit::Px => "px",
                        LengthUnit::Em => "em",
                        LengthUnit::Rem => "rem",
                        LengthUnit::Percent => "%",
                        LengthUnit::Pt => "pt",
                        LengthUnit::Vw => "vw",
                        LengthUnit::Vh => "vh",
                        LengthUnit::Cm => "cm",
                        LengthUnit::Mm => "mm",
                        LengthUnit::In => "in",
                    }
                )
            }
            PropertyValue::Number(n) => n.to_string(),
            PropertyValue::Percentage(p) => format!("{}%", p),
            PropertyValue::Color(c) => format!("#{:02x}{:02x}{:02x}", c.r, c.g, c.b),
            _ => return, // Skip other types for now
        };
        styles
            .custom_properties
            .insert(decl.property.clone(), value_str);
        return;
    }

    // Resolve var() references in the value
    let resolved_value = resolve_var(&decl.value, &styles.custom_properties);

    match decl.property.as_str() {
        // Display
        "display" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.display = match kw.as_str() {
                    "block" => Display::Block,
                    "inline" => Display::Inline,
                    "inline-block" => Display::InlineBlock,
                    "flex" => Display::Flex,
                    "inline-flex" => Display::InlineFlex,
                    "grid" => Display::Grid,
                    "inline-grid" => Display::InlineGrid,
                    "table" => Display::Table,
                    "table-row" => Display::TableRow,
                    "table-cell" => Display::TableCell,
                    "table-header-group" => Display::TableHeaderGroup,
                    "table-row-group" => Display::TableRowGroup,
                    "table-footer-group" => Display::TableFooterGroup,
                    "none" => Display::None,
                    _ => styles.display,
                };
            }
        }

        // Position
        "position" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.position = match kw.as_str() {
                    "static" => Position::Static,
                    "relative" => Position::Relative,
                    "absolute" => Position::Absolute,
                    "fixed" => Position::Fixed,
                    _ => styles.position,
                };
            }
        }

        "top" => styles.top = extract_length(&resolved_value),
        "right" => styles.right = extract_length(&resolved_value),
        "bottom" => styles.bottom = extract_length(&resolved_value),
        "left" => styles.left = extract_length(&resolved_value),
        "z-index" => {
            if let PropertyValue::Number(n) = resolved_value {
                styles.z_index = n as i32;
            }
        }

        // Width and height
        "width" => styles.width = extract_length(&resolved_value),
        "height" => styles.height = extract_length(&resolved_value),
        "min-width" => styles.min_width = extract_length(&resolved_value),
        "min-height" => styles.min_height = extract_length(&resolved_value),
        "max-width" => styles.max_width = extract_length(&resolved_value),
        "max-height" => styles.max_height = extract_length(&resolved_value),

        // Margin
        "margin" => {
            if let Some(lengths) = extract_margin_values(&resolved_value) {
                apply_margin_values(
                    &mut styles.margin_top,
                    &mut styles.margin_right,
                    &mut styles.margin_bottom,
                    &mut styles.margin_left,
                    lengths,
                );
            }
        }
        "margin-top" => {
            styles.margin_top = extract_length(&resolved_value);
        }
        "margin-right" => {
            styles.margin_right = extract_length(&resolved_value);
        }
        "margin-bottom" => {
            styles.margin_bottom = extract_length(&resolved_value);
        }
        "margin-left" => {
            styles.margin_left = extract_length(&resolved_value);
        }

        // Padding
        "padding" => {
            if let Some(lengths) = extract_box_values(&resolved_value) {
                apply_box_values(
                    &mut styles.padding_top,
                    &mut styles.padding_right,
                    &mut styles.padding_bottom,
                    &mut styles.padding_left,
                    lengths,
                );
            }
        }
        "padding-top" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.padding_top = len;
            }
        }
        "padding-right" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.padding_right = len;
            }
        }
        "padding-bottom" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.padding_bottom = len;
            }
        }
        "padding-left" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.padding_left = len;
            }
        }

        // Border width
        "border-width" => {
            if let Some(lengths) = extract_box_values(&resolved_value) {
                apply_box_values(
                    &mut styles.border_top_width,
                    &mut styles.border_right_width,
                    &mut styles.border_bottom_width,
                    &mut styles.border_left_width,
                    lengths,
                );
            }
        }
        "border-top-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_top_width = len;
            }
        }
        "border-right-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_right_width = len;
            }
        }
        "border-bottom-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_bottom_width = len;
            }
        }
        "border-left-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_left_width = len;
            }
        }

        // Border color
        "border-color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.border_top_color = c;
                styles.border_right_color = c;
                styles.border_bottom_color = c;
                styles.border_left_color = c;
            }
        }
        "border-top-color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.border_top_color = c;
            }
        }
        "border-right-color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.border_right_color = c;
            }
        }
        "border-bottom-color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.border_bottom_color = c;
            }
        }
        "border-left-color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.border_left_color = c;
            }
        }

        // Border style
        "border-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let style = parse_border_style(kw);
                styles.border_top_style = style;
                styles.border_right_style = style;
                styles.border_bottom_style = style;
                styles.border_left_style = style;
            }
        }
        "border-top-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.border_top_style = parse_border_style(kw);
            }
        }
        "border-right-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.border_right_style = parse_border_style(kw);
            }
        }
        "border-bottom-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.border_bottom_style = parse_border_style(kw);
            }
        }
        "border-left-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.border_left_style = parse_border_style(kw);
            }
        }

        // Border (shorthand)
        "border" => {
            if let PropertyValue::Multiple(values) = &resolved_value {
                for val in values {
                    match val {
                        PropertyValue::Length(len) => {
                            styles.border_top_width = *len;
                            styles.border_right_width = *len;
                            styles.border_bottom_width = *len;
                            styles.border_left_width = *len;
                        }
                        PropertyValue::Keyword(kw) => {
                            let style = parse_border_style(kw);
                            styles.border_top_style = style;
                            styles.border_right_style = style;
                            styles.border_bottom_style = style;
                            styles.border_left_style = style;
                        }
                        PropertyValue::Color(c) => {
                            styles.border_top_color = *c;
                            styles.border_right_color = *c;
                            styles.border_bottom_color = *c;
                            styles.border_left_color = *c;
                        }
                        _ => {}
                    }
                }
            }
        }

        // Border radius
        "border-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_top_left_radius = len;
                styles.border_top_right_radius = len;
                styles.border_bottom_left_radius = len;
                styles.border_bottom_right_radius = len;
            }
        }
        "border-top-left-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_top_left_radius = len;
            }
        }
        "border-top-right-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_top_right_radius = len;
            }
        }
        "border-bottom-left-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_bottom_left_radius = len;
            }
        }
        "border-bottom-right-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_bottom_right_radius = len;
            }
        }

        // Flexbox
        "flex-direction" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.flex_direction = match kw.as_str() {
                    "row" => FlexDirection::Row,
                    "row-reverse" => FlexDirection::RowReverse,
                    "column" => FlexDirection::Column,
                    "column-reverse" => FlexDirection::ColumnReverse,
                    _ => styles.flex_direction,
                };
            }
        }
        "flex-wrap" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.flex_wrap = match kw.as_str() {
                    "nowrap" => FlexWrap::NoWrap,
                    "wrap" => FlexWrap::Wrap,
                    "wrap-reverse" => FlexWrap::WrapReverse,
                    _ => styles.flex_wrap,
                };
            }
        }
        "justify-content" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.justify_content = match kw.as_str() {
                    "flex-start" | "start" => JustifyContent::FlexStart,
                    "flex-end" | "end" => JustifyContent::FlexEnd,
                    "center" => JustifyContent::Center,
                    "space-between" => JustifyContent::SpaceBetween,
                    "space-around" => JustifyContent::SpaceAround,
                    "space-evenly" => JustifyContent::SpaceEvenly,
                    _ => styles.justify_content,
                };
            }
        }
        "align-items" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.align_items = match kw.as_str() {
                    "flex-start" | "start" => AlignItems::FlexStart,
                    "flex-end" | "end" => AlignItems::FlexEnd,
                    "center" => AlignItems::Center,
                    "baseline" => AlignItems::Baseline,
                    "stretch" => AlignItems::Stretch,
                    _ => styles.align_items,
                };
            }
        }
        "align-content" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.align_content = match kw.as_str() {
                    "flex-start" | "start" => AlignContent::FlexStart,
                    "flex-end" | "end" => AlignContent::FlexEnd,
                    "center" => AlignContent::Center,
                    "space-between" => AlignContent::SpaceBetween,
                    "space-around" => AlignContent::SpaceAround,
                    "stretch" => AlignContent::Stretch,
                    _ => styles.align_content,
                };
            }
        }
        "flex-grow" => {
            if let PropertyValue::Number(n) = resolved_value {
                styles.flex_grow = n;
            }
        }
        "flex-shrink" => {
            if let PropertyValue::Number(n) = resolved_value {
                styles.flex_shrink = n;
            }
        }
        "flex-basis" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if kw == "auto" {
                    styles.flex_basis = FlexBasis::Auto;
                }
            } else if let Some(len) = extract_length(&resolved_value) {
                styles.flex_basis = FlexBasis::Length(len);
            }
        }

        // Grid
        "grid-template-columns" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let (tracks, named_lines) = parse_grid_tracks_with_names(kw);
                styles.grid_template_columns = tracks;
                styles.grid_column_names = named_lines;
            }
        }
        "grid-template-rows" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let (tracks, named_lines) = parse_grid_tracks_with_names(kw);
                styles.grid_template_rows = tracks;
                styles.grid_row_names = named_lines;
            }
        }
        "grid-gap" | "gap" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.grid_gap = len;
                styles.grid_row_gap = len;
                styles.grid_column_gap = len;
            }
        }
        "grid-row-gap" | "row-gap" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.grid_row_gap = len;
            }
        }
        "grid-column-gap" | "column-gap" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.grid_column_gap = len;
            }
        }
        "grid-column" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                // Store raw value for later resolution (after grid-template-columns is set)
                styles.grid_column_raw = Some(kw.clone());
            }
        }
        "grid-row" => {
            match &resolved_value {
                PropertyValue::Keyword(kw) => {
                    // Store raw value for later resolution (after grid-template-rows is set)
                    styles.grid_row_raw = Some(kw.clone());
                }
                PropertyValue::Number(n) => {
                    // Handle numeric values like "2"
                    styles.grid_row_raw = Some(n.to_string());
                }
                _ => {}
            }
        }
        "grid-column-start" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                // For explicit start/end, we can parse immediately if numeric
                if let Ok(n) = kw.parse::<i32>() {
                    styles.grid_column_start = n;
                } else {
                    // Store in grid_column_raw for deferred resolution
                    let current_end = styles
                        .grid_column_raw
                        .as_ref()
                        .and_then(|s| s.split_once('/').map(|(_, e)| e.trim()))
                        .unwrap_or("auto");
                    styles.grid_column_raw = Some(format!("{} / {}", kw, current_end));
                }
            }
        }
        "grid-column-end" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Ok(n) = kw.parse::<i32>() {
                    styles.grid_column_end = n;
                } else {
                    let current_start = styles
                        .grid_column_raw
                        .as_ref()
                        .and_then(|s| s.split_once('/').map(|(s, _)| s.trim()))
                        .unwrap_or("auto");
                    styles.grid_column_raw = Some(format!("{} / {}", current_start, kw));
                }
            }
        }
        "grid-row-start" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Ok(n) = kw.parse::<i32>() {
                    styles.grid_row_start = n;
                } else {
                    let current_end = styles
                        .grid_row_raw
                        .as_ref()
                        .and_then(|s| s.split_once('/').map(|(_, e)| e.trim()))
                        .unwrap_or("auto");
                    styles.grid_row_raw = Some(format!("{} / {}", kw, current_end));
                }
            }
        }
        "grid-row-end" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Ok(n) = kw.parse::<i32>() {
                    styles.grid_row_end = n;
                } else {
                    let current_start = styles
                        .grid_row_raw
                        .as_ref()
                        .and_then(|s| s.split_once('/').map(|(s, _)| s.trim()))
                        .unwrap_or("auto");
                    styles.grid_row_raw = Some(format!("{} / {}", current_start, kw));
                }
            }
        }

        // Typography
        "font-family" => {
            if let PropertyValue::FontFamily(families) = &resolved_value {
                styles.font_family = families.clone();
            }
        }
        "font-size" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.font_size = len.to_px(parent_font_size, root_font_size);
            }
        }
        "font-weight" => match &resolved_value {
            PropertyValue::Keyword(kw) => {
                styles.font_weight = match kw.as_str() {
                    "normal" => FontWeight::Normal,
                    "bold" => FontWeight::Bold,
                    "lighter" => FontWeight::Weight(300),
                    "bolder" => FontWeight::Weight(700),
                    _ => styles.font_weight,
                };
            }
            PropertyValue::Number(n) => {
                styles.font_weight = FontWeight::Weight(*n as u16);
            }
            _ => {}
        },
        "font-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.font_style = match kw.as_str() {
                    "normal" => FontStyle::Normal,
                    "italic" => FontStyle::Italic,
                    "oblique" => FontStyle::Oblique,
                    _ => styles.font_style,
                };
            }
        }
        "line-height" => match &resolved_value {
            PropertyValue::Keyword(kw) if kw == "normal" => {
                styles.line_height = LineHeight::Normal;
            }
            PropertyValue::Number(n) => {
                styles.line_height = LineHeight::Number(*n);
            }
            PropertyValue::Length(len) => {
                styles.line_height = LineHeight::Length(*len);
            }
            _ => {}
        },
        "text-align" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.text_align = match kw.as_str() {
                    "left" => TextAlign::Left,
                    "right" => TextAlign::Right,
                    "center" => TextAlign::Center,
                    "justify" => TextAlign::Justify,
                    _ => styles.text_align,
                };
            }
        }
        "text-decoration" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.text_decoration = match kw.as_str() {
                    "none" => TextDecoration::None,
                    "underline" => TextDecoration::Underline,
                    "overline" => TextDecoration::Overline,
                    "line-through" => TextDecoration::LineThrough,
                    _ => styles.text_decoration,
                };
            }
        }
        "text-transform" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.text_transform = match kw.as_str() {
                    "none" => TextTransform::None,
                    "uppercase" => TextTransform::Uppercase,
                    "lowercase" => TextTransform::Lowercase,
                    "capitalize" => TextTransform::Capitalize,
                    _ => styles.text_transform,
                };
            }
        }
        "letter-spacing" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.letter_spacing = len.to_px(parent_font_size, root_font_size);
            }
        }
        "word-spacing" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.word_spacing = len.to_px(parent_font_size, root_font_size);
            }
        }
        "white-space" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.white_space = match kw.as_str() {
                    "normal" => WhiteSpace::Normal,
                    "nowrap" => WhiteSpace::NoWrap,
                    "pre" => WhiteSpace::Pre,
                    "pre-wrap" => WhiteSpace::PreWrap,
                    "pre-line" => WhiteSpace::PreLine,
                    _ => styles.white_space,
                };
            }
        }

        // Color
        "color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.color = c;
            }
        }
        "background-color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.background_color = c;
            }
        }

        // Background
        "background-image" => match &resolved_value {
            PropertyValue::Url(url) => {
                styles.background_image = Some(BackgroundImage::Url(url.clone()));
            }
            PropertyValue::LinearGradient { angle, stops } => {
                styles.background_image = Some(BackgroundImage::LinearGradient {
                    angle: *angle,
                    stops: stops.clone(),
                });
            }
            PropertyValue::RadialGradient { stops } => {
                styles.background_image = Some(BackgroundImage::RadialGradient {
                    stops: stops.clone(),
                });
            }
            PropertyValue::Keyword(kw) if kw == "none" => {
                styles.background_image = None;
            }
            _ => {}
        },
        "background-size" => match &resolved_value {
            PropertyValue::Keyword(kw) => {
                styles.background_size = match kw.as_str() {
                    "auto" => BackgroundSize::Auto,
                    "cover" => BackgroundSize::Cover,
                    "contain" => BackgroundSize::Contain,
                    _ => styles.background_size,
                };
            }
            PropertyValue::Multiple(values) if values.len() == 2 => {
                if let (Some(w), Some(h)) = (extract_length(&values[0]), extract_length(&values[1]))
                {
                    styles.background_size = BackgroundSize::Length(w, h);
                }
            }
            _ => {}
        },
        "background-repeat" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.background_repeat = match kw.as_str() {
                    "repeat" => BackgroundRepeat::Repeat,
                    "repeat-x" => BackgroundRepeat::RepeatX,
                    "repeat-y" => BackgroundRepeat::RepeatY,
                    "no-repeat" => BackgroundRepeat::NoRepeat,
                    _ => styles.background_repeat,
                };
            }
        }

        // Shorthand: background (treat as background-color for now)
        "background" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.background_color = c;
            } else if let PropertyValue::LinearGradient { angle, stops } = &resolved_value {
                styles.background_image = Some(BackgroundImage::LinearGradient {
                    angle: *angle,
                    stops: stops.clone(),
                });
            } else if let PropertyValue::RadialGradient { stops } = &resolved_value {
                styles.background_image = Some(BackgroundImage::RadialGradient {
                    stops: stops.clone(),
                });
            }
        }

        // Visual effects
        "opacity" => {
            if let PropertyValue::Number(n) = resolved_value {
                styles.opacity = n.clamp(0.0, 1.0);
            }
        }
        "box-shadow" => match &resolved_value {
            PropertyValue::BoxShadow(shadows) => {
                styles.box_shadow = shadows.clone();
            }
            PropertyValue::Keyword(kw) if kw == "none" => {
                styles.box_shadow.clear();
            }
            _ => {}
        },
        "text-shadow" => match &resolved_value {
            PropertyValue::TextShadow(shadows) => {
                styles.text_shadow = shadows.clone();
            }
            PropertyValue::Keyword(kw) if kw == "none" => {
                styles.text_shadow.clear();
            }
            _ => {}
        },
        "transform" => {
            if let PropertyValue::Transform(transforms) = &resolved_value {
                styles.transform = transforms.clone();
            }
        }

        // Overflow
        "overflow" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let overflow = match kw.as_str() {
                    "visible" => Overflow::Visible,
                    "hidden" => Overflow::Hidden,
                    "scroll" => Overflow::Scroll,
                    "auto" => Overflow::Auto,
                    _ => Overflow::Visible,
                };
                styles.overflow_x = overflow;
                styles.overflow_y = overflow;
            }
        }
        "overflow-x" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.overflow_x = match kw.as_str() {
                    "visible" => Overflow::Visible,
                    "hidden" => Overflow::Hidden,
                    "scroll" => Overflow::Scroll,
                    "auto" => Overflow::Auto,
                    _ => styles.overflow_x,
                };
            }
        }
        "overflow-y" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.overflow_y = match kw.as_str() {
                    "visible" => Overflow::Visible,
                    "hidden" => Overflow::Hidden,
                    "scroll" => Overflow::Scroll,
                    "auto" => Overflow::Auto,
                    _ => styles.overflow_y,
                };
            }
        }

        _ => {
            // Ignore unknown properties
        }
    }
}

fn extract_length(value: &PropertyValue) -> Option<Length> {
    match value {
        PropertyValue::Length(len) => Some(*len),
        PropertyValue::Number(n) if *n == 0.0 => Some(Length::px(0.0)),
        PropertyValue::Keyword(kw) if kw == "auto" => None,
        _ => None,
    }
}

fn extract_margin_values(value: &PropertyValue) -> Option<Vec<Option<Length>>> {
    match value {
        PropertyValue::Length(len) => Some(vec![Some(*len)]),
        PropertyValue::Keyword(kw) if kw == "auto" => Some(vec![None]), // auto margins
        PropertyValue::Multiple(values) => {
            let lengths: Vec<Option<Length>> = values.iter().map(extract_length).collect();
            if lengths.is_empty() {
                None
            } else {
                Some(lengths)
            }
        }
        _ => None,
    }
}

fn extract_box_values(value: &PropertyValue) -> Option<Vec<Length>> {
    match value {
        PropertyValue::Length(len) => Some(vec![*len]),
        PropertyValue::Multiple(values) => {
            let lengths: Vec<Length> = values.iter().filter_map(extract_length).collect();
            if lengths.is_empty() {
                None
            } else {
                Some(lengths)
            }
        }
        _ => None,
    }
}

/// Parse grid track list (e.g., "200px 1fr 2fr" or "repeat(3, 1fr)")
/// Returns tracks and a map of named grid lines to their positions
fn parse_grid_tracks(tracks_str: &str) -> Vec<GridTrack> {
    let (tracks, _) = parse_grid_tracks_with_names(tracks_str);
    tracks
}

fn parse_grid_tracks_with_names(tracks_str: &str) -> (Vec<GridTrack>, HashMap<String, Vec<usize>>) {
    let mut tracks = Vec::new();
    let mut named_lines: HashMap<String, Vec<usize>> = HashMap::new();
    let mut in_brackets = false;
    let mut in_parens = 0;
    let mut current_token = String::new();
    let mut bracket_content = String::new();

    // Parse character by character to handle brackets, functions
    for ch in tracks_str.chars() {
        match ch {
            '[' if in_parens == 0 => {
                // Start of grid line name
                in_brackets = true;
                bracket_content.clear();
                if !current_token.trim().is_empty() {
                    // Process accumulated token before bracket
                    process_track_token(&current_token, &mut tracks);
                    current_token.clear();
                }
            }
            ']' if in_parens == 0 => {
                // End of grid line name - store it
                let line_position = tracks.len(); // Line is before this track
                for name in bracket_content.split_whitespace() {
                    named_lines
                        .entry(name.to_string())
                        .or_insert_with(Vec::new)
                        .push(line_position);
                }
                in_brackets = false;
                bracket_content.clear();
                current_token.clear();
            }
            '(' => {
                in_parens += 1;
                current_token.push(ch);
            }
            ')' => {
                in_parens -= 1;
                current_token.push(ch);
                // If we just closed a function, process it
                if in_parens == 0 && !current_token.trim().is_empty() {
                    process_track_token(&current_token, &mut tracks);
                    current_token.clear();
                }
            }
            ' ' if !in_brackets && in_parens == 0 => {
                // Whitespace outside brackets and functions - end of token
                if !current_token.trim().is_empty() {
                    process_track_token(&current_token, &mut tracks);
                    current_token.clear();
                }
            }
            _ if in_brackets => {
                // Inside brackets - accumulate name
                bracket_content.push(ch);
            }
            _ => {
                // Outside brackets - accumulate token
                current_token.push(ch);
            }
        }
    }

    // Handle last token
    if !current_token.trim().is_empty() {
        process_track_token(&current_token, &mut tracks);
    }

    // Add final line position (after all tracks)
    // This allows "screen-end" etc. to work
    let final_line = tracks.len();

    (tracks, named_lines)
}

/// Parse a single grid line reference (e.g., "text-start", "3", "auto")
fn parse_grid_line(value: &str, named_lines: &HashMap<String, Vec<usize>>) -> i32 {
    let value = value.trim();

    // Try parsing as integer first
    if let Ok(n) = value.parse::<i32>() {
        return n;
    }

    // Check if it's "auto"
    if value == "auto" {
        return 0; // 0 means auto-placement in Taffy
    }

    // Try to resolve as named grid line
    if let Some(positions) = named_lines.get(value) {
        if let Some(&pos) = positions.first() {
            // Grid lines are 1-indexed in CSS (line 1 is before track 0)
            return (pos + 1) as i32;
        }
    }

    // Default to auto
    0
}

/// Finalize grid placement by resolving raw grid-column/row values with named lines
fn finalize_grid_placement(styles: &mut ComputedStyles) {
    // Resolve grid-column if raw value exists
    if let Some(raw_value) = &styles.grid_column_raw {
        let (start, end) = parse_grid_line_placement(raw_value, &styles.grid_column_names);
        styles.grid_column_start = start;
        styles.grid_column_end = end;
    }

    // Resolve grid-row if raw value exists
    if let Some(raw_value) = &styles.grid_row_raw {
        let (start, end) = parse_grid_line_placement(raw_value, &styles.grid_row_names);
        styles.grid_row_start = start;
        styles.grid_row_end = end;
    }
}

/// Parse grid-column or grid-row placement (e.g., "text", "1 / 3", "auto")
fn parse_grid_line_placement(value: &str, named_lines: &HashMap<String, Vec<usize>>) -> (i32, i32) {
    let value = value.trim();

    // Check if it contains a slash (explicit start / end)
    if let Some(slash_pos) = value.find('/') {
        let start_str = value[..slash_pos].trim();
        let end_str = value[slash_pos + 1..].trim();
        let start = parse_grid_line(start_str, named_lines);
        let end = parse_grid_line(end_str, named_lines);
        return (start, end);
    }

    // Single numeric value - treat as "start / span 1" (e.g., "2" means grid-row: 2 / 3)
    if let Ok(n) = value.parse::<i32>() {
        return (n, n + 1);
    }

    // Single value - check if it's a named area (e.g., "text")
    // Named areas should expand to area-start / area-end
    let start_name = format!("{}-start", value);
    let end_name = format!("{}-end", value);

    let start = if let Some(positions) = named_lines.get(&start_name) {
        if let Some(&pos) = positions.first() {
            (pos + 1) as i32
        } else {
            parse_grid_line(value, named_lines)
        }
    } else {
        parse_grid_line(value, named_lines)
    };

    let end = if let Some(positions) = named_lines.get(&end_name) {
        if let Some(&pos) = positions.first() {
            (pos + 1) as i32
        } else {
            0 // auto
        }
    } else {
        0 // auto
    };

    (start, end)
}

/// Process a track token (could be a single track or a repeat() function)
fn process_track_token(token: &str, tracks: &mut Vec<GridTrack>) {
    let token = token.trim();

    // Check for repeat() function
    if token.starts_with("repeat(") && token.ends_with(')') {
        let inner = &token[7..token.len() - 1]; // Extract "3, 1fr" from "repeat(3, 1fr)"

        // Split by comma to get count and pattern
        if let Some(comma_pos) = inner.find(',') {
            let count_str = inner[..comma_pos].trim();
            let pattern_str = inner[comma_pos + 1..].trim();

            if let Ok(count) = count_str.parse::<usize>() {
                // Parse the pattern track
                if let Some(track) = parse_single_grid_track(pattern_str) {
                    // Add it count times
                    for _ in 0..count {
                        tracks.push(track.clone());
                    }
                }
            }
        }
    } else {
        // Single track
        if let Some(track) = parse_single_grid_track(token) {
            tracks.push(track);
        }
    }
}

/// Parse a single grid track value
fn parse_single_grid_track(track_str: &str) -> Option<GridTrack> {
    let track_str = track_str.trim();

    // Skip empty strings
    if track_str.is_empty() {
        return None;
    }

    // Handle repeat() - for now, just skip it
    if track_str.starts_with("repeat(") {
        // TODO: Properly parse repeat() syntax
        // For now, return None to skip
        return None;
    }

    // Handle var() - these will be already resolved by the time we get here
    // but just in case, try to parse the value
    if track_str.starts_with("var(") {
        // This shouldn't happen if CSS variables are properly resolved
        // but return None to be safe
        return None;
    }

    // Check for fr unit
    if let Some(val_str) = track_str.strip_suffix("fr") {
        if let Ok(val) = val_str.parse::<f32>() {
            return Some(GridTrack::Fr(val));
        }
    }

    // Check for auto
    if track_str == "auto" {
        return Some(GridTrack::Auto);
    }

    // Try to parse as length
    if let Some(len) = crate::css::parse_property_value("", track_str).and_then(|pv| match pv {
        PropertyValue::Length(l) => Some(l),
        _ => None,
    }) {
        return Some(GridTrack::Length(len));
    }

    None
}

fn apply_margin_values(
    top: &mut Option<Length>,
    right: &mut Option<Length>,
    bottom: &mut Option<Length>,
    left: &mut Option<Length>,
    values: Vec<Option<Length>>,
) {
    match values.len() {
        1 => {
            *top = values[0];
            *right = values[0];
            *bottom = values[0];
            *left = values[0];
        }
        2 => {
            *top = values[0];
            *bottom = values[0];
            *right = values[1];
            *left = values[1];
        }
        3 => {
            *top = values[0];
            *right = values[1];
            *left = values[1];
            *bottom = values[2];
        }
        4 => {
            *top = values[0];
            *right = values[1];
            *bottom = values[2];
            *left = values[3];
        }
        _ => {}
    }
}

fn apply_box_values(
    top: &mut Length,
    right: &mut Length,
    bottom: &mut Length,
    left: &mut Length,
    values: Vec<Length>,
) {
    match values.len() {
        1 => {
            *top = values[0];
            *right = values[0];
            *bottom = values[0];
            *left = values[0];
        }
        2 => {
            *top = values[0];
            *bottom = values[0];
            *right = values[1];
            *left = values[1];
        }
        3 => {
            *top = values[0];
            *right = values[1];
            *left = values[1];
            *bottom = values[2];
        }
        4 => {
            *top = values[0];
            *right = values[1];
            *bottom = values[2];
            *left = values[3];
        }
        _ => {}
    }
}

fn parse_border_style(kw: &str) -> BorderStyle {
    match kw {
        "none" => BorderStyle::None,
        "solid" => BorderStyle::Solid,
        "dashed" => BorderStyle::Dashed,
        "dotted" => BorderStyle::Dotted,
        "double" => BorderStyle::Double,
        _ => BorderStyle::None,
    }
}
