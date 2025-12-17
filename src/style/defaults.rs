//! Default styles for HTML elements
//!
//! Provides initial/default computed style values for each HTML element type.
//! These are applied before author styles in the cascade.
//!
//! Reference: HTML5 Living Standard - Rendering
//! <https://html.spec.whatwg.org/multipage/rendering.html>

use crate::dom::{DomNode, DomNodeType};
use crate::style::{ComputedStyle, Display, Length, Rgba};

/// Get default styles for an HTML element
///
/// Returns a ComputedStyle with appropriate default values for the given element.
/// These defaults match the user-agent stylesheet behavior for HTML elements.
///
/// Note: All styling should come from CSS (user-agent.css or author styles),
/// not from class-name checks in Rust code. This function only sets tag-based defaults.
pub fn get_default_styles_for_element(node: &DomNode) -> ComputedStyle {
    let mut styles = ComputedStyle::default();

    // Handle Document node type - must be block to establish formatting context at root
    if matches!(node.node_type, DomNodeType::Document) {
        styles.display = Display::Block;
        return styles;
    }

    // Set proper default display values for HTML elements (user-agent stylesheet defaults)
    if let Some(tag) = node.tag_name() {
        styles.display = match tag {
            // Document structure elements (must be block to establish formatting context)
            "html" | "body" => Display::Block,

            // Block-level elements
            "div" | "p" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "ul" | "ol" | "blockquote" | "pre" | "article"
            | "section" | "nav" | "aside" | "header" | "footer" | "main" | "figure" | "figcaption" | "dl" | "dt"
            | "dd" | "form" | "fieldset" | "legend" | "address" | "hr" => Display::Block,

            // Lists
            "li" => Display::ListItem,

            // Center element - centers its contents
            "center" => Display::Block,

            // Table elements
            "table" => Display::Table,
            "caption" => Display::TableCaption,
            "tr" => Display::TableRow,
            "td" | "th" => Display::TableCell,
            "thead" | "tbody" | "tfoot" => Display::TableRowGroup,

            // Inline elements (explicit for clarity, though it's the default)
            "a" | "span" | "em" | "strong" | "code" | "b" | "i" | "u" | "small" | "sub" | "sup" | "mark" | "abbr"
            | "cite" | "q" | "kbd" | "samp" | "var" | "time" | "label" => Display::Inline,

            // Replaced elements: keep them inline by default
            "img" | "video" | "audio" | "canvas" | "svg" => Display::Inline,

            // Hidden elements (display: none - not rendered)
            "head" | "style" | "script" | "meta" | "link" | "title" | "noscript" | "template" => Display::None,

            // Everything else defaults to inline
            _ => Display::Inline,
        };

        // Force minimal spacing for table elements (consistent with user-agent.css)
        match tag {
            "html" => {
                // Canvas default background per UA stylesheet
                styles.background_color = Rgba::WHITE;
            }
            "body" => {
                // UA default margins: 8px on all sides
                let default_margin = Some(Length::px(8.0));
                styles.margin_top = default_margin;
                styles.margin_right = default_margin;
                styles.margin_bottom = default_margin;
                styles.margin_left = default_margin;
            }
            "table" => {
                // Remove all spacing from tables
                styles.margin_top = Some(Length::px(0.0));
                styles.margin_bottom = Some(Length::px(0.0));
                styles.padding_top = Length::px(0.0);
                styles.padding_bottom = Length::px(0.0);
                // UA default border-spacing per HTML/CSS UA stylesheets (CSS2 §17.6.1).
                styles.border_spacing_horizontal = Length::px(2.0);
                styles.border_spacing_vertical = Length::px(2.0);
            }
            "tr" => {
                // Minimal spacing between table rows
                styles.margin_top = Some(Length::px(0.0));
                styles.margin_bottom = Some(Length::px(0.0));
                styles.padding_top = Length::px(0.0);
                styles.padding_bottom = Length::px(0.0);
            }
            "td" => {
                // Minimal padding for table cells
                styles.padding_top = Length::px(1.0);
                styles.padding_bottom = Length::px(1.0);
                styles.padding_left = Length::px(1.0);
                styles.padding_right = Length::px(1.0);
                styles.margin_top = Some(Length::px(0.0));
                styles.margin_bottom = Some(Length::px(0.0));
                // CSS 2.1 §17.5.3: table cells default to middle alignment
                styles.vertical_align = crate::style::types::VerticalAlign::Middle;
            }
            "legend" => {
                styles.shrink_to_fit_inline_size = true;
            }
            "th" => {
                // Header cells inherit td defaults plus bold/centered text
                styles.padding_top = Length::px(1.0);
                styles.padding_bottom = Length::px(1.0);
                styles.padding_left = Length::px(1.0);
                styles.padding_right = Length::px(1.0);
                styles.margin_top = Some(Length::px(0.0));
                styles.margin_bottom = Some(Length::px(0.0));
                styles.vertical_align = crate::style::types::VerticalAlign::Middle;
                styles.text_align = crate::style::types::TextAlign::Center;
                styles.font_weight = crate::style::FontWeight::Bold;
            }
            "b" | "strong" => {
                // Bold text
                styles.font_weight = crate::style::FontWeight::Bold;
            }
            "i" | "em" => {
                // Italic text - using Oblique since we may not have true italics
                styles.font_style = crate::style::FontStyle::Oblique(None);
            }
            "img" | "video" | "audio" | "canvas" | "svg" => {
                // Responsive default: limit replaced elements to their containing block
                styles.max_width = Some(Length::percent(100.0));
            }
            _ => {}
        }
    }

    styles
}

/// Parse HTML width/height attribute
///
/// Handles both percentage values like "85%" and pixel values like "18".
pub fn parse_dimension_attribute(dim_str: &str) -> Option<Length> {
    let dim_str = dim_str.trim();

    // Handle percentage like "85%"
    if dim_str.ends_with('%') {
        if let Ok(value) = dim_str[..dim_str.len() - 1].trim().parse::<f32>() {
            return Some(Length::percent(value));
        }
    }

    // Handle pixels (just a number like "18")
    if let Ok(value) = dim_str.parse::<f32>() {
        return Some(Length::px(value));
    }

    None
}

/// Parse HTML bgcolor attribute
///
/// Handles hex colors like #ff6600 or ff6600, with 3 or 6 digit variants.
pub fn parse_color_attribute(color_str: &str) -> Option<Rgba> {
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
                return Some(Rgba { r, g, b, a: 1.0 });
            }
        } else if hex.len() == 3 {
            // Shorthand like #f60
            if let (Ok(r), Ok(g), Ok(b)) = (
                u8::from_str_radix(&hex[0..1], 16),
                u8::from_str_radix(&hex[1..2], 16),
                u8::from_str_radix(&hex[2..3], 16),
            ) {
                // Double each digit: #f60 -> #ff6600
                return Some(Rgba {
                    r: r * 17,
                    g: g * 17,
                    b: b * 17,
                    a: 1.0,
                });
            }
        }
    }

    // Fallback to CSS color parsing for rgb()/named colors.
    if let Ok(color) = crate::style::color::Color::parse(color_str) {
        return Some(color.to_rgba(Rgba::BLACK));
    }

    None
}
