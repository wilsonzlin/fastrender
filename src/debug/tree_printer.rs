//! Enhanced tree printing and visualization utilities

use crate::tree::{BoxNode, BoxType, FragmentContent, FragmentNode};
use std::fmt::Write as _;

/// Color mode for terminal output
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ColorMode {
    /// No colors (plain text)
    #[default]
    None,
    /// ANSI 16-color mode
    Ansi16,
    /// ANSI 256-color mode
    Ansi256,
}

/// Configuration for tree printing
#[derive(Debug, Clone)]
pub struct PrintConfig {
    /// Whether to use colors
    pub color_mode: ColorMode,
    /// Maximum depth to print (None = unlimited)
    pub max_depth: Option<usize>,
    /// Whether to show box types
    pub show_box_types: bool,
    /// Whether to show formatting context types
    pub show_fc_types: bool,
    /// Whether to use Unicode box-drawing characters
    pub use_unicode: bool,
}

impl PrintConfig {
    /// Creates default configuration (colors, Unicode)
    pub fn new() -> Self {
        Self {
            color_mode: ColorMode::Ansi16,
            max_depth: None,
            show_box_types: true,
            show_fc_types: true,
            use_unicode: true,
        }
    }

    /// Minimal output (no colors, no extra info)
    pub fn minimal() -> Self {
        Self {
            color_mode: ColorMode::None,
            max_depth: None,
            show_box_types: false,
            show_fc_types: false,
            use_unicode: false,
        }
    }

    /// Detailed output (all info, colors)
    pub fn detailed() -> Self {
        Self {
            color_mode: ColorMode::Ansi16,
            max_depth: None,
            show_box_types: true,
            show_fc_types: true,
            use_unicode: true,
        }
    }

    /// Output suitable for tests (no colors, deterministic)
    pub fn test() -> Self {
        Self {
            color_mode: ColorMode::None,
            max_depth: None,
            show_box_types: true,
            show_fc_types: false,
            use_unicode: false,
        }
    }
}

impl Default for PrintConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// ANSI color codes (debug infrastructure)
#[allow(dead_code)]
mod colors {
    pub const RESET: &str = "\x1b[0m";
    pub const GRAY: &str = "\x1b[90m";
    pub const RED: &str = "\x1b[31m";
    pub const GREEN: &str = "\x1b[32m";
    pub const YELLOW: &str = "\x1b[33m";
    pub const BLUE: &str = "\x1b[34m";
    pub const CYAN: &str = "\x1b[36m";
}

/// Enhanced tree printer with configurable output
pub struct EnhancedTreePrinter {
    config: PrintConfig,
}

impl EnhancedTreePrinter {
    /// Creates a new tree printer with default configuration
    pub fn new() -> Self {
        Self {
            config: PrintConfig::default(),
        }
    }

    /// Creates a tree printer with custom configuration
    pub fn with_config(config: PrintConfig) -> Self {
        Self { config }
    }

    /// Prints a box tree with configurable formatting
    pub fn print_box_tree(&self, node: &BoxNode) -> String {
        let mut output = String::new();
        self.print_box_node(node, 0, "", true, &mut output);
        output
    }

    fn print_box_node(&self, node: &BoxNode, depth: usize, prefix: &str, is_last: bool, output: &mut String) {
        if let Some(max_depth) = self.config.max_depth {
            if depth >= max_depth {
                return;
            }
        }

        if depth > 0 {
            output.push_str(prefix);
            output.push_str(if self.config.use_unicode {
                if is_last {
                    "└─ "
                } else {
                    "├─ "
                }
            } else {
                if is_last {
                    "`- "
                } else {
                    "|- "
                }
            });
        }

        output.push_str(&self.format_box_node(node));
        output.push('\n');

        let child_count = node.children.len();
        for (i, child) in node.children.iter().enumerate() {
            let is_last_child = i == child_count - 1;
            let child_prefix = if depth > 0 {
                let ext = if self.config.use_unicode {
                    if is_last {
                        "   "
                    } else {
                        "│  "
                    }
                } else {
                    if is_last {
                        "   "
                    } else {
                        "|  "
                    }
                };
                format!("{}{}", prefix, ext)
            } else {
                String::new()
            };
            self.print_box_node(child, depth + 1, &child_prefix, is_last_child, output);
        }
    }

    fn format_box_node(&self, node: &BoxNode) -> String {
        let mut label = String::new();

        let selector = node
            .debug_info
            .as_ref()
            .map(|d| d.to_selector())
            .unwrap_or_else(|| "<anonymous>".to_string());

        label.push_str(&self.colorize(&selector, colors::CYAN));

        if self.config.show_box_types {
            label.push_str(&self.colorize(" (", colors::GRAY));
            let type_str = match &node.box_type {
                BoxType::Block(_) => "Block",
                BoxType::Inline(_) => "Inline",
                BoxType::Text(t) => {
                    let text = truncate(&t.text, 20);
                    return format!(
                        "{} Text: \"{}\"",
                        self.colorize(&selector, colors::CYAN),
                        self.colorize(&text, colors::GREEN)
                    );
                }
                BoxType::Replaced(_) => "Replaced",
                BoxType::Anonymous(_) => "Anonymous",
            };
            label.push_str(&self.colorize(type_str, colors::YELLOW));
            label.push_str(&self.colorize(")", colors::GRAY));
        }

        if self.config.show_fc_types {
            if let Some(fc) = node.formatting_context() {
                label.push_str(&self.colorize(&format!(" {:?}", fc), colors::BLUE));
            }
        }

        label
    }

    /// Prints a fragment tree with positions
    pub fn print_fragment_tree(&self, fragment: &FragmentNode) -> String {
        let mut output = String::new();
        self.print_fragment_node(fragment, 0, "", true, &mut output);
        output
    }

    fn print_fragment_node(
        &self,
        fragment: &FragmentNode,
        depth: usize,
        prefix: &str,
        is_last: bool,
        output: &mut String,
    ) {
        if let Some(max_depth) = self.config.max_depth {
            if depth >= max_depth {
                return;
            }
        }

        if depth > 0 {
            output.push_str(prefix);
            output.push_str(if self.config.use_unicode {
                if is_last {
                    "└─ "
                } else {
                    "├─ "
                }
            } else {
                if is_last {
                    "`- "
                } else {
                    "|- "
                }
            });
        }

        output.push_str(&self.format_fragment(fragment));
        output.push('\n');

        let child_count = fragment.children.len();
        for (i, child) in fragment.children.iter().enumerate() {
            let is_last_child = i == child_count - 1;
            let child_prefix = if depth > 0 {
                let ext = if self.config.use_unicode {
                    if is_last {
                        "   "
                    } else {
                        "│  "
                    }
                } else {
                    if is_last {
                        "   "
                    } else {
                        "|  "
                    }
                };
                format!("{}{}", prefix, ext)
            } else {
                String::new()
            };
            self.print_fragment_node(child, depth + 1, &child_prefix, is_last_child, output);
        }
    }

    fn format_fragment(&self, fragment: &FragmentNode) -> String {
        let type_str = match &fragment.content {
            FragmentContent::Block { .. } => "Block",
            FragmentContent::Inline { .. } => "Inline",
            FragmentContent::Text { text, .. } => {
                let truncated = truncate(text, 20);
                return format!(
                    "{} \"{}\" {}",
                    self.colorize("Text", colors::YELLOW),
                    self.colorize(&truncated, colors::GREEN),
                    self.format_bounds(&fragment.bounds)
                );
            }
            FragmentContent::Line { .. } => "Line",
            FragmentContent::Replaced { .. } => "Replaced",
        };

        format!(
            "{} {}",
            self.colorize(type_str, colors::YELLOW),
            self.format_bounds(&fragment.bounds)
        )
    }

    fn format_bounds(&self, bounds: &crate::geometry::Rect) -> String {
        self.colorize(
            &format!(
                "[@({:.1}, {:.1}), {:.1}x{:.1}]",
                bounds.x(),
                bounds.y(),
                bounds.width(),
                bounds.height()
            ),
            colors::CYAN,
        )
    }

    fn colorize(&self, text: &str, color: &str) -> String {
        match self.config.color_mode {
            ColorMode::None => text.to_string(),
            ColorMode::Ansi16 | ColorMode::Ansi256 => format!("{}{}{}", color, text, colors::RESET),
        }
    }
}

impl Default for EnhancedTreePrinter {
    fn default() -> Self {
        Self::new()
    }
}

/// Tree difference mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DiffMode {
    /// Show full trees with differences highlighted
    #[default]
    FullTree,
    /// Show only differences
    OnlyDiffs,
}

/// Tree differ - compares two trees
pub struct TreeDiff {
    #[allow(dead_code)]
    config: PrintConfig,
    #[allow(dead_code)]
    mode: DiffMode,
}

impl TreeDiff {
    pub fn new() -> Self {
        Self {
            config: PrintConfig::default(),
            mode: DiffMode::FullTree,
        }
    }

    pub fn with_config(config: PrintConfig, mode: DiffMode) -> Self {
        Self { config, mode }
    }

    /// Compares two box trees
    pub fn diff_box_trees(&self, old: &BoxNode, new: &BoxNode) -> String {
        let printer = EnhancedTreePrinter::with_config(self.config.clone());
        let mut output = String::new();
        output.push_str("=== Box Tree Diff ===\n\nOld:\n");
        output.push_str(&printer.print_box_tree(old));
        output.push_str("\nNew:\n");
        output.push_str(&printer.print_box_tree(new));
        output.push_str("\n=== End Diff ===\n");
        output
    }

    /// Compares two fragment trees
    pub fn diff_fragment_trees(&self, old: &FragmentNode, new: &FragmentNode) -> String {
        let mut output = String::new();
        output.push_str("=== Fragment Tree Diff ===\n\n");

        if old.bounds != new.bounds {
            let _ = writeln!(output, "Position changed: {:?} -> {:?}", old.bounds, new.bounds);
        }
        if old.children.len() != new.children.len() {
            let _ = writeln!(
                output,
                "Child count changed: {} -> {}",
                old.children.len(),
                new.children.len()
            );
        }

        output.push_str("\n=== End Diff ===\n");
        output
    }
}

impl Default for TreeDiff {
    fn default() -> Self {
        Self::new()
    }
}

/// DOT graph exporter for Graphviz
pub struct DotExporter;

impl DotExporter {
    pub fn new() -> Self {
        Self
    }

    /// Exports a box tree to DOT format
    pub fn export_box_tree(&self, node: &BoxNode) -> String {
        let mut output = String::from("digraph BoxTree {\n  node [shape=box, fontname=\"monospace\"];\n\n");
        let mut node_id = 0;
        export_box_node_recursive(node, &mut node_id, None, &mut output);
        output.push_str("}\n");
        output
    }

    /// Exports a fragment tree to DOT format
    pub fn export_fragment_tree(&self, fragment: &FragmentNode) -> String {
        let mut output = String::from("digraph FragmentTree {\n  node [shape=box, fontname=\"monospace\"];\n\n");
        let mut node_id = 0;
        export_fragment_recursive(fragment, &mut node_id, None, &mut output);
        output.push_str("}\n");
        output
    }
}

impl Default for DotExporter {
    fn default() -> Self {
        Self::new()
    }
}

fn export_box_node_recursive(node: &BoxNode, node_id: &mut usize, parent_id: Option<usize>, output: &mut String) {
    let current_id = *node_id;
    *node_id += 1;

    let label = node
        .debug_info
        .as_ref()
        .map(|d| d.to_selector())
        .unwrap_or_else(|| "<anonymous>".to_string());

    let _ = writeln!(output, "  n{} [label=\"{}\"];", current_id, label.replace('"', "\\\""));

    if let Some(parent) = parent_id {
        let _ = writeln!(output, "  n{} -> n{};", parent, current_id);
    }

    for child in &node.children {
        export_box_node_recursive(child, node_id, Some(current_id), output);
    }
}

fn export_fragment_recursive(
    fragment: &FragmentNode,
    node_id: &mut usize,
    parent_id: Option<usize>,
    output: &mut String,
) {
    let current_id = *node_id;
    *node_id += 1;

    let label = format!(
        "[@({:.0}, {:.0}), {:.0}x{:.0}]",
        fragment.bounds.x(),
        fragment.bounds.y(),
        fragment.bounds.width(),
        fragment.bounds.height()
    );

    let _ = writeln!(output, "  n{} [label=\"{}\"];", current_id, label);

    if let Some(parent) = parent_id {
        let _ = writeln!(output, "  n{} -> n{};", parent, current_id);
    }

    for child in &fragment.children {
        export_fragment_recursive(child, node_id, Some(current_id), output);
    }
}

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
    use crate::geometry::{Point, Rect, Size};
    use crate::style::ComputedStyle;
    use crate::tree::{DebugInfo, FormattingContextType};
    use std::sync::Arc;

    fn default_style() -> Arc<ComputedStyle> {
        Arc::new(ComputedStyle::default())
    }

    #[test]
    fn test_print_config_creation() {
        let config = PrintConfig::new();
        assert_eq!(config.color_mode, ColorMode::Ansi16);
        assert!(config.use_unicode);
    }

    #[test]
    fn test_print_config_minimal() {
        let config = PrintConfig::minimal();
        assert_eq!(config.color_mode, ColorMode::None);
        assert!(!config.show_box_types);
    }

    #[test]
    fn test_print_config_test() {
        let config = PrintConfig::test();
        assert_eq!(config.color_mode, ColorMode::None);
        assert!(!config.use_unicode);
    }

    #[test]
    fn test_enhanced_printer_single_block() {
        let printer = EnhancedTreePrinter::with_config(PrintConfig::test());
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let output = printer.print_box_tree(&root);
        assert!(output.contains("Block"));
    }

    #[test]
    fn test_enhanced_printer_with_debug_info() {
        let printer = EnhancedTreePrinter::with_config(PrintConfig::test());
        let debug_info = DebugInfo::new(
            Some("div".to_string()),
            Some("header".to_string()),
            vec!["navbar".to_string()],
        );
        let root =
            BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]).with_debug_info(debug_info);
        let output = printer.print_box_tree(&root);
        assert!(output.contains("div#header.navbar"));
    }

    #[test]
    fn test_enhanced_printer_nested() {
        let printer = EnhancedTreePrinter::with_config(PrintConfig::test());
        let child = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child]);
        let output = printer.print_box_tree(&root);
        assert!(output.contains("`-") || output.contains("|-"));
    }

    #[test]
    fn test_enhanced_printer_text() {
        let printer = EnhancedTreePrinter::with_config(PrintConfig::test());
        let text = BoxNode::new_text(default_style(), "Hello, world!".to_string());
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text]);
        let output = printer.print_box_tree(&root);
        assert!(output.contains("Text"));
        assert!(output.contains("Hello"));
    }

    #[test]
    fn test_enhanced_printer_max_depth() {
        let mut config = PrintConfig::test();
        config.max_depth = Some(1);
        let printer = EnhancedTreePrinter::with_config(config);

        let leaf = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let middle = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![leaf]);
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![middle]);

        let output = printer.print_box_tree(&root);
        let line_count = output.lines().count();
        assert!(line_count <= 2);
    }

    #[test]
    fn test_fragment_printer() {
        let printer = EnhancedTreePrinter::with_config(PrintConfig::test());
        let fragment = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 800.0, 600.0), vec![]);
        let output = printer.print_fragment_tree(&fragment);
        assert!(output.contains("Block"));
        assert!(output.contains("800"));
    }

    #[test]
    fn test_tree_diff() {
        let differ = TreeDiff::new();
        let node1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let node2 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let output = differ.diff_box_trees(&node1, &node2);
        assert!(output.contains("Old:"));
        assert!(output.contains("New:"));
    }

    #[test]
    fn test_dot_export_box() {
        let exporter = DotExporter::new();
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let dot = exporter.export_box_tree(&root);
        assert!(dot.contains("digraph BoxTree"));
        assert!(dot.contains("n0"));
    }

    #[test]
    fn test_dot_export_nested() {
        let exporter = DotExporter::new();
        let child = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child]);
        let dot = exporter.export_box_tree(&root);
        assert!(dot.contains("n0"));
        assert!(dot.contains("n1"));
        assert!(dot.contains("->"));
    }

    #[test]
    fn test_dot_export_fragment() {
        let exporter = DotExporter::new();
        let fragment = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 800.0, 600.0), vec![]);
        let dot = exporter.export_fragment_tree(&fragment);
        assert!(dot.contains("digraph FragmentTree"));
    }

    #[test]
    fn test_colorize_with_colors() {
        let printer = EnhancedTreePrinter::with_config(PrintConfig::new());
        let colored = printer.colorize("test", colors::RED);
        assert!(colored.contains("\x1b[31m"));
        assert!(colored.contains("\x1b[0m"));
    }

    #[test]
    fn test_colorize_without_colors() {
        let printer = EnhancedTreePrinter::with_config(PrintConfig::minimal());
        let not_colored = printer.colorize("test", colors::RED);
        assert_eq!(not_colored, "test");
    }

    #[test]
    fn test_truncate() {
        assert_eq!(truncate("short", 20), "short");
        assert_eq!(truncate("this is a very long string", 10), "this is a ...");
    }
}
