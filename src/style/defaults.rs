//! Default styles for HTML elements
//!
//! Provides initial/default computed style values for each HTML element type.
//! These are applied before author styles in the cascade.
//!
//! Reference: HTML5 Living Standard - Rendering
//! <https://html.spec.whatwg.org/multipage/rendering.html>

use crate::dom::{DomNode, DomNodeType};
use crate::style::types::WhiteSpace;
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
            "div" | "p" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "ul" | "ol" | "li" | "blockquote" | "pre"
            | "article" | "section" | "nav" | "aside" | "header" | "footer" | "main" | "figure" | "figcaption"
            | "dl" | "dt" | "dd" | "form" | "fieldset" | "legend" | "address" | "hr" | "center" => Display::Block,

            // Table elements
            "table" => Display::Table,
            "tr" => Display::TableRow,
            "td" | "th" => Display::TableCell,
            "thead" | "tbody" | "tfoot" => Display::TableRowGroup,

            // Inline elements (explicit for clarity, though it's the default)
            "a" | "span" | "em" | "strong" | "code" | "b" | "i" | "u" | "small" | "sub" | "sup" | "mark" | "abbr"
            | "cite" | "q" | "kbd" | "samp" | "var" | "time" | "label" => Display::Inline,

            // Hidden elements (display: none - not rendered)
            "head" | "style" | "script" | "meta" | "link" | "title" | "noscript" | "template" => Display::None,

            // Everything else defaults to inline
            _ => Display::Inline,
        };

        // Force minimal spacing for table elements (consistent with user-agent.css)
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

        // Prevent text wrapping in table cells by default
        if matches!(styles.display, Display::TableCell) {
            styles.white_space = WhiteSpace::Nowrap;
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

    None
}
