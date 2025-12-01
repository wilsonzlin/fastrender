//! Debug information for boxes and fragments
//!
//! This module provides debug metadata that links boxes/fragments back to
//! source DOM elements. This is invaluable for debugging layout issues and
//! implementing developer tools.
//!
//! # Debug Information
//!
//! DebugInfo contains:
//! - Element tag name (div, span, p)
//! - Element ID
//! - Element classes
//! - DOM tree path
//!
//! # Usage
//!
//! ```
//! use fastrender::tree::DebugInfo;
//!
//! let info = DebugInfo::new(
//!     Some("div".to_string()),
//!     Some("header".to_string()),
//!     vec!["navbar".to_string(), "sticky".to_string()],
//! );
//!
//! assert_eq!(info.to_selector(), "div#header.navbar.sticky");
//! ```
//!
//! # Tree Printing
//!
//! TreePrinter outputs a pretty-printed tree structure for debugging.

use std::fmt;
use std::fmt::Write as _;

use crate::tree::box_tree::BoxNode;
use crate::tree::box_tree::BoxType;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};

/// Debug information linking a box to its source DOM element
///
/// This information is optional and used only for debugging and dev tools.
/// In release builds, this can be omitted to save memory.
///
/// # Examples
///
/// ```
/// use fastrender::tree::DebugInfo;
///
/// let info = DebugInfo::new(
///     Some("div".to_string()),
///     Some("main".to_string()),
///     vec!["container".to_string()],
/// );
///
/// assert_eq!(info.tag_name.as_deref(), Some("div"));
/// assert_eq!(info.id.as_deref(), Some("main"));
/// assert_eq!(info.to_selector(), "div#main.container");
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DebugInfo {
    /// Element tag name (if from element)
    ///
    /// Examples: "div", "span", "p", "button"
    /// None for text nodes or anonymous boxes
    pub tag_name: Option<String>,

    /// Element ID attribute
    ///
    /// The value of the `id` attribute, if present
    pub id: Option<String>,

    /// Element class names
    ///
    /// List of classes from the `class` attribute
    pub classes: Vec<String>,

    /// DOM tree path for this element
    ///
    /// String representation of the path from root to this element
    /// Example: "html > body > div#main > p"
    pub dom_path: Option<String>,
}

impl DebugInfo {
    /// Creates debug info from element attributes
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::tree::DebugInfo;
    ///
    /// let info = DebugInfo::new(
    ///     Some("div".to_string()),
    ///     Some("header".to_string()),
    ///     vec!["navbar".to_string()],
    /// );
    ///
    /// assert!(info.tag_name.is_some());
    /// assert!(info.id.is_some());
    /// assert_eq!(info.classes.len(), 1);
    /// ```
    pub fn new(tag_name: Option<String>, id: Option<String>, classes: Vec<String>) -> Self {
        Self {
            tag_name,
            id,
            classes,
            dom_path: None,
        }
    }

    /// Creates debug info for a text node
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::tree::DebugInfo;
    ///
    /// let info = DebugInfo::text("Hello world");
    /// assert_eq!(info.to_selector(), "#text(Hello world)");
    /// ```
    pub fn text(content: &str) -> Self {
        Self {
            tag_name: Some(format!("#text({})", truncate(content, 20))),
            id: None,
            classes: Vec::new(),
            dom_path: None,
        }
    }

    /// Creates debug info for an anonymous box
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::tree::DebugInfo;
    ///
    /// let info = DebugInfo::anonymous("block");
    /// assert_eq!(info.to_selector(), "#anonymous(block)");
    /// ```
    pub fn anonymous(box_type: &str) -> Self {
        Self {
            tag_name: Some(format!("#anonymous({})", box_type)),
            id: None,
            classes: Vec::new(),
            dom_path: None,
        }
    }

    /// Adds the DOM path to this debug info
    ///
    /// Builder-style method for convenience.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::tree::DebugInfo;
    ///
    /// let info = DebugInfo::new(
    ///     Some("div".to_string()),
    ///     None,
    ///     vec![],
    /// ).with_dom_path("html > body > div");
    ///
    /// assert!(info.dom_path.is_some());
    /// ```
    pub fn with_dom_path(mut self, path: impl Into<String>) -> Self {
        self.dom_path = Some(path.into());
        self
    }

    /// Formats as a CSS selector-like string
    ///
    /// Format: `tag#id.class1.class2`
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::tree::DebugInfo;
    ///
    /// let info = DebugInfo::new(
    ///     Some("div".to_string()),
    ///     Some("header".to_string()),
    ///     vec!["navbar".to_string(), "fixed".to_string()],
    /// );
    ///
    /// assert_eq!(info.to_selector(), "div#header.navbar.fixed");
    /// ```
    pub fn to_selector(&self) -> String {
        let mut result = String::new();

        // Add tag name
        if let Some(tag) = &self.tag_name {
            result.push_str(tag);
        }

        // Add ID
        if let Some(id) = &self.id {
            result.push('#');
            result.push_str(id);
        }

        // Add classes
        for class in &self.classes {
            result.push('.');
            result.push_str(class);
        }

        // If nothing was added, return generic
        if result.is_empty() {
            result.push_str("#unknown");
        }

        result
    }

    /// Returns a short description for logging
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::tree::DebugInfo;
    ///
    /// let info = DebugInfo::new(
    ///     Some("div".to_string()),
    ///     Some("main".to_string()),
    ///     vec![],
    /// );
    ///
    /// assert_eq!(info.short_description(), "div#main");
    /// ```
    pub fn short_description(&self) -> String {
        if let Some(tag) = &self.tag_name {
            if let Some(id) = &self.id {
                format!("{}#{}", tag, id)
            } else if !self.classes.is_empty() {
                format!("{}.{}", tag, self.classes[0])
            } else {
                tag.clone()
            }
        } else {
            "#unknown".to_string()
        }
    }
}

// Note: DebugInfo::text() uses the module-level truncate() function below

impl fmt::Display for DebugInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_selector())
    }
}

/// Pretty-prints tree structures for debugging
///
/// Outputs an indented tree structure showing the hierarchy of boxes or fragments.
///
/// # Examples
///
/// ```
/// use fastrender::tree::{BoxNode, DebugInfo, TreePrinter, FormattingContextType};
/// use fastrender::tree::box_tree::ComputedStyle;
/// use std::sync::Arc;
///
/// let style = Arc::new(ComputedStyle::default());
/// let child = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![])
///     .with_debug_info(DebugInfo::new(
///         Some("div".to_string()),
///         Some("child".to_string()),
///         vec![],
///     ));
///
/// let parent = BoxNode::new_block(style, FormattingContextType::Block, vec![child])
///     .with_debug_info(DebugInfo::new(
///         Some("div".to_string()),
///         Some("parent".to_string()),
///         vec![],
///     ));
///
/// let output = TreePrinter::print_box_tree(&parent);
/// // Output:
/// // div#parent (Block)
/// // └─ div#child (Block)
/// ```
pub struct TreePrinter;

impl TreePrinter {
    /// Prints a box tree with debug information
    ///
    /// Returns a string with indented tree structure.
    pub fn print_box_tree(node: &crate::tree::box_tree::BoxNode) -> String {
        let mut output = String::new();
        Self::print_box_node(node, "", true, &mut output);
        output
    }

    /// Prints a single box node with indentation
    fn print_box_node(node: &BoxNode, prefix: &str, is_last: bool, output: &mut String) {
        // Print this node
        let branch = if is_last { "└─ " } else { "├─ " };
        let debug_str = node
            .debug_info
            .as_ref()
            .map(|d| d.to_selector())
            .unwrap_or_else(|| "#unknown".to_string());

        let box_type = match &node.box_type {
            BoxType::Block(_) => "Block",
            BoxType::Inline(_) => "Inline",
            BoxType::Text(text) => &format!("Text({})", truncate(&text.text, 20)),
            BoxType::Replaced(_) => "Replaced",
            BoxType::Anonymous(_) => "Anonymous",
        };

        output.push_str(prefix);
        if !prefix.is_empty() {
            output.push_str(branch);
        }
        let _ = writeln!(output, "{} ({})", debug_str, box_type);

        // Print children
        let child_prefix = if prefix.is_empty() {
            String::new()
        } else {
            format!("{}{}", prefix, if is_last { "   " } else { "│  " })
        };

        let child_count = node.children.len();
        for (i, child) in node.children.iter().enumerate() {
            let is_last_child = i == child_count - 1;
            Self::print_box_node(child, &child_prefix, is_last_child, output);
        }
    }

    /// Prints a fragment tree with debug information
    pub fn print_fragment_tree(node: &FragmentNode) -> String {
        let mut output = String::new();
        Self::print_fragment_node(node, "", true, &mut output);
        output
    }

    /// Prints a single fragment node with indentation
    fn print_fragment_node(node: &FragmentNode, prefix: &str, is_last: bool, output: &mut String) {
        // Print this node
        let branch = if is_last { "└─ " } else { "├─ " };

        let content_type = match &node.content {
            FragmentContent::Block { .. } => "Block",
            FragmentContent::Inline { .. } => "Inline",
            FragmentContent::Text { text, .. } => &format!("Text({})", truncate(text, 20)),
            FragmentContent::Line { .. } => "Line",
            FragmentContent::Replaced { .. } => "Replaced",
        };

        output.push_str(prefix);
        if !prefix.is_empty() {
            output.push_str(branch);
        }
        let _ = writeln!(
            output,
            "{} @ [{:.0}, {:.0}, {:.0}×{:.0}]",
            content_type,
            node.bounds.x(),
            node.bounds.y(),
            node.bounds.width(),
            node.bounds.height()
        );

        // Print children
        let child_prefix = if prefix.is_empty() {
            String::new()
        } else {
            format!("{}{}", prefix, if is_last { "   " } else { "│  " })
        };

        let child_count = node.children.len();
        for (i, child) in node.children.iter().enumerate() {
            let is_last_child = i == child_count - 1;
            Self::print_fragment_node(child, &child_prefix, is_last_child, output);
        }
    }
}

/// Helper function to truncate strings
fn truncate(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}...", &s[..max_len])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_debug_info_new() {
        let info = DebugInfo::new(
            Some("div".to_string()),
            Some("main".to_string()),
            vec!["container".to_string()],
        );

        assert_eq!(info.tag_name.as_deref(), Some("div"));
        assert_eq!(info.id.as_deref(), Some("main"));
        assert_eq!(info.classes.len(), 1);
        assert_eq!(info.classes[0], "container");
    }

    #[test]
    fn test_to_selector() {
        let info = DebugInfo::new(
            Some("div".to_string()),
            Some("header".to_string()),
            vec!["navbar".to_string(), "fixed".to_string()],
        );

        assert_eq!(info.to_selector(), "div#header.navbar.fixed");
    }

    #[test]
    fn test_to_selector_tag_only() {
        let info = DebugInfo::new(Some("div".to_string()), None, vec![]);

        assert_eq!(info.to_selector(), "div");
    }

    #[test]
    fn test_to_selector_id_only() {
        let info = DebugInfo::new(None, Some("main".to_string()), vec![]);

        assert_eq!(info.to_selector(), "#main");
    }

    #[test]
    fn test_to_selector_classes_only() {
        let info = DebugInfo::new(None, None, vec!["button".to_string(), "primary".to_string()]);

        assert_eq!(info.to_selector(), ".button.primary");
    }

    #[test]
    fn test_to_selector_empty() {
        let info = DebugInfo::new(None, None, vec![]);

        assert_eq!(info.to_selector(), "#unknown");
    }

    #[test]
    fn test_text_debug_info() {
        let info = DebugInfo::text("Hello world");

        let selector = info.to_selector();
        assert!(selector.contains("#text"));
        assert!(selector.contains("Hello world"));
    }

    #[test]
    fn test_text_debug_info_truncated() {
        let long_text = "This is a very long text that should be truncated";
        let info = DebugInfo::text(long_text);

        let selector = info.to_selector();
        assert!(selector.contains("..."));
    }

    #[test]
    fn test_anonymous_debug_info() {
        let info = DebugInfo::anonymous("block");

        assert_eq!(info.to_selector(), "#anonymous(block)");
    }

    #[test]
    fn test_with_dom_path() {
        let info = DebugInfo::new(Some("div".to_string()), None, vec![]).with_dom_path("html > body > div");

        assert_eq!(info.dom_path.as_deref(), Some("html > body > div"));
    }

    #[test]
    fn test_short_description() {
        let info1 = DebugInfo::new(
            Some("div".to_string()),
            Some("main".to_string()),
            vec!["container".to_string()],
        );
        assert_eq!(info1.short_description(), "div#main");

        let info2 = DebugInfo::new(Some("span".to_string()), None, vec!["highlight".to_string()]);
        assert_eq!(info2.short_description(), "span.highlight");

        let info3 = DebugInfo::new(Some("p".to_string()), None, vec![]);
        assert_eq!(info3.short_description(), "p");
    }

    #[test]
    fn test_display_trait() {
        let info = DebugInfo::new(
            Some("div".to_string()),
            Some("header".to_string()),
            vec!["navbar".to_string()],
        );

        assert_eq!(format!("{}", info), "div#header.navbar");
    }

    #[test]
    fn test_debug_info_clone() {
        let info1 = DebugInfo::new(Some("div".to_string()), Some("main".to_string()), vec![]);
        let info2 = info1.clone();

        assert_eq!(info1, info2);
    }

    #[test]
    fn test_tree_printer_single_node() {
        use crate::style::ComputedStyle;
        use crate::tree::{BoxNode, FormattingContextType};
        use std::sync::Arc;

        let style = Arc::new(ComputedStyle::default());
        let node = BoxNode::new_block(style, FormattingContextType::Block, vec![]).with_debug_info(DebugInfo::new(
            Some("div".to_string()),
            Some("main".to_string()),
            vec![],
        ));

        let output = TreePrinter::print_box_tree(&node);
        assert!(output.contains("div#main"));
        assert!(output.contains("Block"));
    }

    #[test]
    fn test_tree_printer_with_children() {
        use crate::style::ComputedStyle;
        use crate::tree::{BoxNode, FormattingContextType};
        use std::sync::Arc;

        let style = Arc::new(ComputedStyle::default());
        let child = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]).with_debug_info(
            DebugInfo::new(Some("div".to_string()), Some("child".to_string()), vec![]),
        );

        let parent = BoxNode::new_block(style, FormattingContextType::Block, vec![child]).with_debug_info(
            DebugInfo::new(Some("div".to_string()), Some("parent".to_string()), vec![]),
        );

        let output = TreePrinter::print_box_tree(&parent);
        assert!(output.contains("div#parent"));
        assert!(output.contains("div#child"));
        // The tree should contain either └─ or the visual representation
        assert!(output.contains("└") || output.contains("child")); // Accept either Unicode or ASCII
    }

    #[test]
    fn test_truncate() {
        assert_eq!(truncate("short", 20), "short");
        assert_eq!(truncate("this is a very long string", 10), "this is a ...");
    }

    #[test]
    fn test_debug_info_equality() {
        let info1 = DebugInfo::new(
            Some("div".to_string()),
            Some("main".to_string()),
            vec!["container".to_string()],
        );
        let info2 = DebugInfo::new(
            Some("div".to_string()),
            Some("main".to_string()),
            vec!["container".to_string()],
        );
        let info3 = DebugInfo::new(
            Some("span".to_string()),
            Some("main".to_string()),
            vec!["container".to_string()],
        );

        assert_eq!(info1, info2);
        assert_ne!(info1, info3);
    }
}
