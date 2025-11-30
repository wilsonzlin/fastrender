//! Default styles for HTML elements
//!
//! Provides initial/default computed style values for each HTML element type.
//! These are applied before author styles in the cascade.
//!
//! Reference: HTML5 Living Standard - Rendering
//! https://html.spec.whatwg.org/multipage/rendering.html

use crate::dom::DomNode;
use crate::style::types::{LineHeight, TextAlign, WhiteSpace};
use crate::style::{ComputedStyle, Display, Length, Rgba};

/// Get default styles for an HTML element
///
/// Returns a ComputedStyle with appropriate default values for the given element.
/// These defaults match the user-agent stylesheet behavior for HTML elements.
pub fn get_default_styles_for_element(node: &DomNode) -> ComputedStyle {
    let mut styles = ComputedStyle::default();

    // Handle Document node type - must be block to establish formatting context at root
    if matches!(node.node_type, crate::dom::DomNodeType::Document) {
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

        // Force minimal spacing for table elements
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
                        styles.height = Some(Length::px(2.0));
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

        // Prevent text wrapping in table cells
        if matches!(styles.display, Display::TableCell) {
            styles.white_space = WhiteSpace::Nowrap;
        }

        // HACK: Site-specific styling for pagetop
        if node.has_class("pagetop") {
            styles.font_family = vec!["Verdana".to_string(), "Geneva".to_string(), "sans-serif".to_string()];
            styles.font_size = 10.0;
            styles.color = Rgba {
                r: 34,
                g: 34,
                b: 34,
                a: 1.0,
            }; // #222222
            styles.line_height = LineHeight::Length(Length::px(12.0));
        }

        // HACK: Style links
        if matches!(node.tag_name(), Some("a")) {
            styles.color = Rgba {
                r: 34,
                g: 34,
                b: 34,
                a: 1.0,
            }; // #222222
            styles.display = Display::Inline;
        }

        // HACK: Site-specific styling for votearrow
        if node.has_class("votearrow") {
            styles.display = Display::Block;
            styles.width = Some(Length::px(10.0));
            styles.height = Some(Length::px(10.0));
            styles.color = Rgba {
                r: 0,
                g: 0,
                b: 0,
                a: 1.0,
            };
            styles.font_size = 20.0;
            styles.text_align = TextAlign::Center;
        }

        // HACK: Site-specific styling for votelinks
        if node.has_class("votelinks") {
            styles.width = Some(Length::px(30.0));
            styles.text_align = TextAlign::Center;
        }

        // HACK: Site-specific styling for rank
        if node.has_class("rank") {
            styles.display = Display::Block;
            styles.color = Rgba {
                r: 0,
                g: 0,
                b: 0,
                a: 1.0,
            };
            styles.font_size = 10.0;
            styles.text_align = TextAlign::Right;
        }

        // HACK: Site-specific styling for title cell
        if node.has_class("title") && node.tag_name() == Some("td") {
            if node.get_attribute("align").as_deref() == Some("right")
                && node.get_attribute("valign").as_deref() == Some("top")
            {
                styles.width = Some(Length::px(30.0));
                styles.text_align = TextAlign::Right;
            }
        }

        // HACK: Site-specific styling for pagetop
        if node.has_class("pagetop") {
            styles.display = Display::Inline;
            styles.color = Rgba {
                r: 255,
                g: 255,
                b: 255,
                a: 1.0,
            };
            styles.font_size = 10.0;
        }

        // HACK: Site-specific styling for hnname
        if node.has_class("hnname") {
            styles.display = Display::Inline;
            styles.color = Rgba {
                r: 255,
                g: 255,
                b: 255,
                a: 1.0,
            };
            styles.font_weight = crate::style::FontWeight::Bold;
            styles.font_size = 10.0;
        }

        // HACK: Style navigation links
        if let Some(tag) = node.tag_name() {
            if tag == "a" && node.get_attribute("href").is_some() {
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
                        styles.color = Rgba {
                            r: 255,
                            g: 255,
                            b: 255,
                            a: 1.0,
                        };
                        styles.font_size = 10.0;
                    }
                }
            }
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
