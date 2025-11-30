//! Box Tree Generation
//!
//! This module handles the conversion of styled DOM trees into CSS box trees.
//! It is used by both the legacy `Renderer` and the new `FastRender` APIs.
//!
//! # Box Generation Algorithm
//!
//! The box generation algorithm follows CSS 2.1 Section 9.2:
//! 1. Each element in the DOM generates zero or more boxes
//! 2. The type of box depends on the element's `display` property
//! 3. Replaced elements (img, video, etc.) generate replaced boxes
//! 4. Text nodes generate text boxes
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::box_generation::generate_box_tree;
//! use fastrender::style::apply_styles;
//!
//! let styled = apply_styles(&dom, &stylesheet);
//! let box_tree = generate_box_tree(&styled);
//! ```

use crate::css::{self, StyleSheet};
use crate::dom::DomNode;
use crate::error::Result;
use crate::geometry::Size;
use crate::style::display::FormattingContextType;
use crate::style::{ComputedStyles, Display, StyledNode};
use crate::tree::box_tree::{BoxNode, BoxTree, BoxType, ReplacedBox, ReplacedType};
use std::sync::Arc;

/// Generates a BoxTree from a StyledNode tree
///
/// This is the main entry point for box generation. It recursively
/// converts each StyledNode into the appropriate BoxNode type.
///
/// # Arguments
///
/// * `styled` - The root of the styled node tree
///
/// # Returns
///
/// A `BoxTree` containing the generated box structure
pub fn generate_box_tree(styled: &StyledNode) -> BoxTree {
    let root = generate_box_node(styled);
    BoxTree { root }
}

/// Recursively generates BoxNode from StyledNode
fn generate_box_node(styled: &StyledNode) -> BoxNode {
    let style = Arc::new(styled.styles.clone());

    // Check if this is display: none
    if styled.styles.display == Display::None {
        // Create an empty block (will have no effect on layout)
        return BoxNode::new_block(style, FormattingContextType::Block, vec![]);
    }

    // Check for text node
    if let Some(text) = styled.node.text_content() {
        if !text.trim().is_empty() {
            return BoxNode::new_text(style, text.to_string());
        }
    }

    // Check for replaced elements (images, etc.)
    if let Some(tag) = styled.node.tag_name() {
        if is_replaced_element(tag) {
            return create_replaced_box(styled, style);
        }
    }

    // Generate children
    let children: Vec<BoxNode> = styled.children.iter().map(generate_box_node).collect();

    // Determine box type based on display
    let fc_type = styled
        .styles
        .display
        .formatting_context_type()
        .unwrap_or(FormattingContextType::Block);

    match styled.styles.display {
        Display::Block | Display::Flex | Display::Grid | Display::Table => {
            BoxNode::new_block(style, fc_type, children)
        }
        Display::Inline => BoxNode::new_inline(style, children),
        Display::InlineBlock => BoxNode::new_inline_block(style, fc_type, children),
        Display::None => BoxNode::new_block(style, FormattingContextType::Block, vec![]),
        _ => BoxNode::new_block(style, fc_type, children),
    }
}

/// Checks if an element is a replaced element
///
/// Replaced elements are those whose content is replaced by an external resource,
/// such as images, videos, iframes, etc. These elements have intrinsic dimensions.
fn is_replaced_element(tag: &str) -> bool {
    matches!(
        tag.to_lowercase().as_str(),
        "img" | "video" | "canvas" | "svg" | "iframe" | "embed" | "object"
    )
}

/// Creates a BoxNode for a replaced element
fn create_replaced_box(styled: &StyledNode, style: Arc<ComputedStyles>) -> BoxNode {
    let tag = styled.node.tag_name().unwrap_or("img");

    // Get src attribute if available
    let src = styled.node.get_attribute("src").unwrap_or_default();

    // Determine replaced type
    let replaced_type = match tag.to_lowercase().as_str() {
        "img" => ReplacedType::Image { src },
        "video" => ReplacedType::Video { src },
        "canvas" => ReplacedType::Canvas,
        "svg" => ReplacedType::Svg {
            content: String::new(),
        },
        "iframe" => ReplacedType::Iframe { src },
        _ => ReplacedType::Image { src },
    };

    // Get intrinsic size from attributes or use default
    let intrinsic_width = styled
        .node
        .get_attribute("width")
        .and_then(|w| w.parse::<f32>().ok())
        .unwrap_or(300.0);

    let intrinsic_height = styled
        .node
        .get_attribute("height")
        .and_then(|h| h.parse::<f32>().ok())
        .unwrap_or(150.0);

    let replaced_box = ReplacedBox {
        replaced_type,
        intrinsic_size: Some(Size::new(intrinsic_width, intrinsic_height)),
        aspect_ratio: Some(intrinsic_width / intrinsic_height),
    };

    BoxNode {
        box_type: BoxType::Replaced(replaced_box),
        style,
        children: vec![],
        debug_info: None,
    }
}

/// Extracts CSS from style tags in the DOM
///
/// Walks the DOM tree looking for `<style>` elements and concatenates
/// their text content to form a stylesheet.
///
/// # Arguments
///
/// * `dom` - The root DOM node to search
///
/// # Returns
///
/// A `StyleSheet` parsed from all found CSS content, or an empty
/// stylesheet if no styles were found.
pub fn extract_css(dom: &DomNode) -> Result<StyleSheet> {
    let mut css_content = String::new();

    dom.walk_tree(&mut |node| {
        if let Some(tag) = node.tag_name() {
            if tag == "style" {
                for child in &node.children {
                    if let Some(text) = child.text_content() {
                        css_content.push_str(text);
                        css_content.push('\n');
                    }
                }
            }
        }
    });

    if css_content.is_empty() {
        Ok(StyleSheet { rules: Vec::new() })
    } else {
        css::parse_stylesheet(&css_content)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::ComputedStyles;

    #[test]
    fn test_is_replaced_element() {
        assert!(is_replaced_element("img"));
        assert!(is_replaced_element("IMG"));
        assert!(is_replaced_element("video"));
        assert!(is_replaced_element("canvas"));
        assert!(is_replaced_element("svg"));
        assert!(is_replaced_element("iframe"));
        assert!(is_replaced_element("embed"));
        assert!(is_replaced_element("object"));

        assert!(!is_replaced_element("div"));
        assert!(!is_replaced_element("span"));
        assert!(!is_replaced_element("p"));
    }
}
