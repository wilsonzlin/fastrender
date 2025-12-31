//! Enhanced tree printing and visualization utilities

use crate::dom::{DomNode, DomNodeType};
use crate::geometry::{Point, Rect};
use crate::paint::display_list::{DisplayItem, DisplayList};
use crate::style::cascade::StyledNode;
use crate::style::color::Rgba;
use crate::style::ComputedStyle;
use crate::tree::box_tree::BoxNode;
use crate::tree::box_tree::BoxType;
use crate::tree::box_tree::MarkerContent;
use crate::tree::fragment_tree::FragmentContent;
use crate::tree::fragment_tree::FragmentNode;
use crate::tree::fragment_tree::FragmentTree;
use serde_json::{Map, Value};
use std::collections::BTreeMap;
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

  fn print_box_node(
    &self,
    node: &BoxNode,
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
        BoxType::Marker(m) => match &m.content {
          MarkerContent::Text(t) => {
            let text = truncate(t, 20);
            return format!(
              "{} Marker: \"{}\"",
              self.colorize(&selector, colors::CYAN),
              self.colorize(&text, colors::GREEN)
            );
          }
          MarkerContent::Image(_) => {
            return format!(
              "{} Marker: {}",
              self.colorize(&selector, colors::CYAN),
              self.colorize("[image]", colors::GREEN)
            );
          }
        },
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

  /// Prints all roots in a fragment tree (pagination/columns)
  pub fn print_fragment_forest(&self, tree: &FragmentTree) -> String {
    let mut output = String::new();

    let total_roots = 1 + tree.additional_fragments.len();
    for (i, root) in std::iter::once(&tree.root)
      .chain(tree.additional_fragments.iter())
      .enumerate()
    {
      let _ = writeln!(
        output,
        "== root[{i}] fragmentainer_index={} bounds={} ==",
        root.fragmentainer_index,
        self.format_plain_bounds(&root.bounds)
      );
      self.print_fragment_node(root, 0, "", true, &mut output);

      if i + 1 < total_roots {
        output.push('\n');
      }
    }

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
    for (i, child) in fragment.children().enumerate() {
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
    let metadata = self.format_fragment_metadata(fragment);
    let type_str = match &fragment.content {
      FragmentContent::Block { .. } => "Block",
      FragmentContent::Inline { .. } => "Inline",
      FragmentContent::Text { text, .. } => {
        let truncated = truncate(text, 20);
        return format!(
          "{} {} \"{}\" {}",
          self.colorize("Text", colors::YELLOW),
          metadata,
          self.colorize(&truncated, colors::GREEN),
          self.format_bounds(&fragment.bounds)
        );
      }
      FragmentContent::Line { .. } => "Line",
      FragmentContent::Replaced { .. } => "Replaced",
      FragmentContent::RunningAnchor { .. } => "RunningAnchor",
    };

    format!(
      "{} {} {}",
      self.colorize(type_str, colors::YELLOW),
      metadata,
      self.format_bounds(&fragment.bounds)
    )
  }

  fn format_bounds(&self, bounds: &Rect) -> String {
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

  fn format_plain_bounds(&self, bounds: &Rect) -> String {
    format!(
      "({:.1}, {:.1}, {:.1}, {:.1})",
      bounds.x(),
      bounds.y(),
      bounds.width(),
      bounds.height()
    )
  }

  fn format_fragment_metadata(&self, fragment: &FragmentNode) -> String {
    self.colorize(
      &format!(
        "[fragment_index={}/{} fragmentainer_index={}]",
        fragment.fragment_index, fragment.fragment_count, fragment.fragmentainer_index
      ),
      colors::GRAY,
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
      let _ = writeln!(
        output,
        "Position changed: {:?} -> {:?}",
        old.bounds, new.bounds
      );
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
    let mut output =
      String::from("digraph BoxTree {\n  node [shape=box, fontname=\"monospace\"];\n\n");
    let mut node_id = 0;
    export_box_node_recursive(node, &mut node_id, None, &mut output);
    output.push_str("}\n");
    output
  }

  /// Exports a fragment tree to DOT format
  pub fn export_fragment_tree(&self, fragment: &FragmentNode) -> String {
    let mut output =
      String::from("digraph FragmentTree {\n  node [shape=box, fontname=\"monospace\"];\n\n");
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

fn export_box_node_recursive(
  node: &BoxNode,
  node_id: &mut usize,
  parent_id: Option<usize>,
  output: &mut String,
) {
  let current_id = *node_id;
  *node_id += 1;

  let label = node
    .debug_info
    .as_ref()
    .map(|d| d.to_selector())
    .unwrap_or_else(|| "<anonymous>".to_string());

  let _ = writeln!(
    output,
    "  n{} [label=\"{}\"];",
    current_id,
    label.replace('"', "\\\"")
  );

  if let Some(parent) = parent_id {
    let _ = writeln!(output, "  n{} -> n{};", parent, current_id);
  }

  for child in node.children.iter() {
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

  for child in fragment.children() {
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

/// Configuration for JSON exports.
#[derive(Debug, Clone, Default)]
pub struct JsonExportConfig {
  /// Optional scroll offset applied to fragment bounds.
  pub scroll_offset: Option<Point>,
}

/// Serializes trees into deterministic JSON structures.
///
/// The exporter focuses on lightweight, stable summaries suitable for
/// debugging and test fixtures rather than full-fidelity serialization.
pub struct TreeJsonExporter {
  config: JsonExportConfig,
}

impl TreeJsonExporter {
  /// Creates an exporter with default configuration.
  pub fn new() -> Self {
    Self {
      config: JsonExportConfig::default(),
    }
  }

  /// Creates an exporter with explicit configuration.
  pub fn with_config(config: JsonExportConfig) -> Self {
    Self { config }
  }

  /// Serializes a DOM subtree.
  pub fn export_dom(&self, node: &DomNode) -> Value {
    self.export_dom_node(node)
  }

  /// Serializes a styled tree.
  pub fn export_styled_tree(&self, node: &StyledNode) -> Value {
    let mut entries = vec![
      ("node_id", Value::from(node.node_id as u64)),
      ("node", self.export_dom_brief(&node.node)),
      ("styles", style_summary(&node.styles)),
    ];

    if let Some(before) = &node.before_styles {
      entries.push(("before", style_summary(before)));
    }
    if let Some(after) = &node.after_styles {
      entries.push(("after", style_summary(after)));
    }
    if let Some(marker) = &node.marker_styles {
      entries.push(("marker", style_summary(marker)));
    }

    let children: Vec<Value> = node
      .children
      .iter()
      .map(|child| self.export_styled_tree(child))
      .collect();
    entries.push(("children", Value::Array(children)));

    map_from_pairs(entries)
  }

  /// Serializes a box tree.
  pub fn export_box_tree(&self, node: &BoxNode) -> Value {
    let mut entries = vec![
      ("id", Value::from(node.id as u64)),
      ("type", Value::String(format!("{:?}", node.box_type))),
      ("style", style_summary(&node.style)),
    ];

    if let Some(info) = &node.debug_info {
      entries.push(("debug", Value::String(info.to_selector())));
    }
    if let Some(styled_node_id) = node.styled_node_id {
      entries.push(("styled_node_id", Value::from(styled_node_id as u64)));
    }

    let children: Vec<Value> = node
      .children
      .iter()
      .map(|child| self.export_box_tree(child))
      .collect();
    entries.push(("children", Value::Array(children)));

    map_from_pairs(entries)
  }

  /// Serializes a fragment tree. Bounds are emitted both in local and absolute coordinates.
  pub fn export_fragment_tree(&self, fragment: &FragmentNode) -> Value {
    self.export_fragment_with_offset(fragment, Point::ZERO)
  }

  fn export_fragment_with_offset(&self, fragment: &FragmentNode, parent_offset: Point) -> Value {
    let scroll = self.config.scroll_offset.unwrap_or(Point::new(0.0, 0.0));
    let abs_rect = Rect::from_xywh(
      fragment.bounds.x() + parent_offset.x + scroll.x,
      fragment.bounds.y() + parent_offset.y + scroll.y,
      fragment.bounds.width(),
      fragment.bounds.height(),
    );

    let mut entries = vec![
      ("kind", Value::String(fragment_kind(fragment))),
      ("local_bounds", rect_value(fragment.bounds, None)),
      ("absolute_bounds", rect_value(abs_rect, None)),
      (
        "fragment_index",
        Value::from(fragment.fragment_index as u64),
      ),
      (
        "fragment_count",
        Value::from(fragment.fragment_count as u64),
      ),
      (
        "fragmentainer_index",
        Value::from(fragment.fragmentainer_index as u64),
      ),
      (
        "scroll_overflow",
        rect_value(
          fragment.scroll_overflow,
          Some(Point::new(
            parent_offset.x + scroll.x,
            parent_offset.y + scroll.y,
          )),
        ),
      ),
    ];

    match &fragment.content {
      FragmentContent::Block { box_id }
      | FragmentContent::Inline { box_id, .. }
      | FragmentContent::Replaced { box_id, .. }
      | FragmentContent::Text { box_id, .. } => {
        if let Some(id) = box_id {
          entries.push(("box_id", Value::from(*id as u64)));
        }
      }
      FragmentContent::Line { .. } => {}
      FragmentContent::RunningAnchor { .. } => {}
    }

    if let FragmentContent::Text { text, .. } = &fragment.content {
      entries.push(("text", Value::String(truncate(text, 120))));
    }

    if let Some(style) = fragment.style.as_deref() {
      entries.push(("style", style_summary(style)));
    }

    let next_offset = Point::new(
      parent_offset.x + fragment.bounds.x(),
      parent_offset.y + fragment.bounds.y(),
    );
    let children: Vec<Value> = fragment
      .children()
      .map(|child| self.export_fragment_with_offset(child, next_offset))
      .collect();
    entries.push(("children", Value::Array(children)));

    map_from_pairs(entries)
  }

  /// Serializes a display list into a compact JSON representation.
  pub fn export_display_list(&self, list: &DisplayList) -> Value {
    let items: Vec<Value> = list.items().iter().map(display_item_value).collect();
    let mut entries = vec![("items", Value::Array(items))];

    if let Some(bounds) = list
      .items()
      .iter()
      .fold(None, |acc, item| match (acc, item.bounds()) {
        (None, Some(rect)) => Some(rect),
        (Some(prev), Some(rect)) => Some(prev.union(rect)),
        (acc, None) => acc,
      })
    {
      entries.push(("bounds", rect_value(bounds, None)));
    }

    map_from_pairs(entries)
  }

  fn export_dom_brief(&self, node: &DomNode) -> Value {
    match &node.node_type {
      DomNodeType::Element {
        tag_name,
        namespace,
        attributes,
      } => map_from_pairs(vec![
        ("type", Value::String("element".into())),
        ("tag", Value::String(tag_name.clone())),
        ("namespace", Value::String(namespace.clone())),
        ("attributes", attributes_value(attributes)),
      ]),
      DomNodeType::Slot {
        namespace,
        attributes,
        ..
      } => map_from_pairs(vec![
        ("type", Value::String("slot".into())),
        ("namespace", Value::String(namespace.clone())),
        ("attributes", attributes_value(attributes)),
      ]),
      DomNodeType::Text { content } => map_from_pairs(vec![
        ("type", Value::String("text".into())),
        ("text", Value::String(truncate(content, 120))),
      ]),
      DomNodeType::Document { quirks_mode } => map_from_pairs(vec![
        ("type", Value::String("document".into())),
        ("quirks_mode", Value::String(format!("{:?}", quirks_mode))),
      ]),
      DomNodeType::ShadowRoot { .. } => {
        map_from_pairs(vec![("type", Value::String("shadow_root".into()))])
      }
    }
  }

  fn export_dom_node(&self, node: &DomNode) -> Value {
    let mut entries = match &node.node_type {
      DomNodeType::Element {
        tag_name,
        namespace,
        attributes,
      } => vec![
        ("type", Value::String("element".into())),
        ("tag", Value::String(tag_name.clone())),
        ("namespace", Value::String(namespace.clone())),
        ("attributes", attributes_value(attributes)),
      ],
      DomNodeType::Slot {
        namespace,
        attributes,
        ..
      } => vec![
        ("type", Value::String("slot".into())),
        ("namespace", Value::String(namespace.clone())),
        ("attributes", attributes_value(attributes)),
      ],
      DomNodeType::ShadowRoot { mode, .. } => vec![
        ("type", Value::String("shadow_root".into())),
        ("mode", Value::String(format!("{:?}", mode))),
      ],
      DomNodeType::Text { content } => vec![
        ("type", Value::String("text".into())),
        ("text", Value::String(truncate(content, 200))),
      ],
      DomNodeType::Document { quirks_mode } => vec![
        ("type", Value::String("document".into())),
        ("quirks_mode", Value::String(format!("{:?}", quirks_mode))),
      ],
    };

    let children: Vec<Value> = node
      .children
      .iter()
      .map(|child| self.export_dom_node(child))
      .collect();
    entries.push(("children", Value::Array(children)));

    map_from_pairs(entries)
  }
}

fn display_item_value(item: &DisplayItem) -> Value {
  match item {
    DisplayItem::FillRect(it) => map_from_pairs(vec![
      ("type", Value::String("FillRect".into())),
      ("rect", rect_value(it.rect, None)),
      ("color", color_value(it.color)),
    ]),
    DisplayItem::StrokeRect(it) => map_from_pairs(vec![
      ("type", Value::String("StrokeRect".into())),
      ("rect", rect_value(it.rect, None)),
      ("width", Value::from(it.width)),
      ("color", color_value(it.color)),
    ]),
    DisplayItem::Outline(it) => map_from_pairs(vec![
      ("type", Value::String("Outline".into())),
      ("rect", rect_value(it.outer_rect(), None)),
      ("width", Value::from(it.width)),
      ("color", color_value(it.color)),
    ]),
    DisplayItem::FillRoundedRect(it) => map_from_pairs(vec![
      ("type", Value::String("FillRoundedRect".into())),
      ("rect", rect_value(it.rect, None)),
      ("color", color_value(it.color)),
    ]),
    DisplayItem::StrokeRoundedRect(it) => map_from_pairs(vec![
      ("type", Value::String("StrokeRoundedRect".into())),
      ("rect", rect_value(it.rect, None)),
      ("width", Value::from(it.width)),
      ("color", color_value(it.color)),
    ]),
    DisplayItem::Text(it) => map_from_pairs(vec![
      ("type", Value::String("Text".into())),
      ("origin", point_value(it.origin)),
      ("glyphs", Value::from(it.glyphs.len() as u64)),
      ("font_size", Value::from(it.font_size)),
      ("color", color_value(it.color)),
    ]),
    DisplayItem::Image(it) => map_from_pairs(vec![
      ("type", Value::String("Image".into())),
      ("dest_rect", rect_value(it.dest_rect, None)),
      (
        "filter_quality",
        Value::String(format!("{:?}", it.filter_quality)),
      ),
    ]),
    DisplayItem::ImagePattern(it) => map_from_pairs(vec![
      ("type", Value::String("ImagePattern".into())),
      ("dest_rect", rect_value(it.dest_rect, None)),
      (
        "tile_size",
        map_from_pairs(vec![
          ("width", Value::from(it.tile_size.width)),
          ("height", Value::from(it.tile_size.height)),
        ]),
      ),
      ("origin", point_value(it.origin)),
      ("repeat", Value::String(format!("{:?}", it.repeat))),
      (
        "filter_quality",
        Value::String(format!("{:?}", it.filter_quality)),
      ),
    ]),
    DisplayItem::BoxShadow(it) => map_from_pairs(vec![
      ("type", Value::String("BoxShadow".into())),
      ("rect", rect_value(it.rect, None)),
      ("color", color_value(it.color)),
    ]),
    DisplayItem::ListMarker(it) => map_from_pairs(vec![
      ("type", Value::String("ListMarker".into())),
      ("origin", point_value(it.origin)),
      ("glyphs", Value::from(it.glyphs.len() as u64)),
      ("color", color_value(it.color)),
    ]),
    DisplayItem::LinearGradient(it) => map_from_pairs(vec![
      ("type", Value::String("LinearGradient".into())),
      ("rect", rect_value(it.rect, None)),
      ("stops", Value::from(it.stops.len() as u64)),
    ]),
    DisplayItem::RadialGradient(it) => map_from_pairs(vec![
      ("type", Value::String("RadialGradient".into())),
      ("rect", rect_value(it.rect, None)),
      ("stops", Value::from(it.stops.len() as u64)),
    ]),
    DisplayItem::ConicGradient(it) => map_from_pairs(vec![
      ("type", Value::String("ConicGradient".into())),
      ("rect", rect_value(it.rect, None)),
      ("stops", Value::from(it.stops.len() as u64)),
    ]),
    DisplayItem::Border(it) => map_from_pairs(vec![
      ("type", Value::String("Border".into())),
      ("rect", rect_value(it.rect, None)),
      ("top", border_side_value(&it.top)),
      ("right", border_side_value(&it.right)),
      ("bottom", border_side_value(&it.bottom)),
      ("left", border_side_value(&it.left)),
      ("has_image", Value::from(it.image.is_some())),
    ]),
    DisplayItem::TableCollapsedBorders(it) => map_from_pairs(vec![
      ("type", Value::String("TableCollapsedBorders".into())),
      ("rows", Value::from(it.borders.row_count as u64)),
      ("columns", Value::from(it.borders.column_count as u64)),
    ]),
    DisplayItem::TextDecoration(it) => map_from_pairs(vec![
      ("type", Value::String("TextDecoration".into())),
      ("rect", rect_value(it.bounds, None)),
    ]),
    DisplayItem::PushClip(it) => map_from_pairs(vec![
      ("type", Value::String("PushClip".into())),
      (
        "shape",
        match &it.shape {
          crate::paint::display_list::ClipShape::Rect { rect, .. } => rect_value(*rect, None),
          crate::paint::display_list::ClipShape::Path { .. } => Value::String("path".into()),
        },
      ),
    ]),
    DisplayItem::PopClip => map_from_pairs(vec![("type", Value::String("PopClip".into()))]),
    DisplayItem::PushOpacity(it) => map_from_pairs(vec![
      ("type", Value::String("PushOpacity".into())),
      ("opacity", Value::from(it.opacity)),
    ]),
    DisplayItem::PopOpacity => map_from_pairs(vec![("type", Value::String("PopOpacity".into()))]),
    DisplayItem::PushTransform(it) => map_from_pairs(vec![
      ("type", Value::String("PushTransform".into())),
      ("is_identity", Value::from(it.transform.is_identity())),
      ("matrix", transform3d_value(&it.transform)),
    ]),
    DisplayItem::PopTransform => {
      map_from_pairs(vec![("type", Value::String("PopTransform".into()))])
    }
    DisplayItem::PushBlendMode(it) => map_from_pairs(vec![
      ("type", Value::String("PushBlendMode".into())),
      ("mode", Value::String(format!("{:?}", it.mode))),
    ]),
    DisplayItem::PopBlendMode => {
      map_from_pairs(vec![("type", Value::String("PopBlendMode".into()))])
    }
    DisplayItem::PushStackingContext(it) => map_from_pairs(vec![
      ("type", Value::String("PushStackingContext".into())),
      ("bounds", rect_value(it.bounds, None)),
      ("z_index", Value::from(it.z_index)),
      (
        "mix_blend_mode",
        Value::String(format!("{:?}", it.mix_blend_mode)),
      ),
      ("is_isolated", Value::from(it.is_isolated)),
      (
        "creates_stacking_context",
        Value::from(it.creates_stacking_context),
      ),
      ("filters", Value::from(it.filters.len() as u64)),
      (
        "backdrop_filters",
        Value::from(it.backdrop_filters.len() as u64),
      ),
      ("has_transform", Value::from(it.transform.is_some())),
      ("has_mask", Value::from(it.mask.is_some())),
    ]),
    DisplayItem::PopStackingContext => {
      map_from_pairs(vec![("type", Value::String("PopStackingContext".into()))])
    }
  }
}

fn fragment_kind(fragment: &FragmentNode) -> String {
  match &fragment.content {
    FragmentContent::Block { .. } => "block".into(),
    FragmentContent::Inline { .. } => "inline".into(),
    FragmentContent::Text { .. } => "text".into(),
    FragmentContent::Line { .. } => "line".into(),
    FragmentContent::Replaced { .. } => "replaced".into(),
    FragmentContent::RunningAnchor { .. } => "running-anchor".into(),
  }
}

fn style_summary(style: &ComputedStyle) -> Value {
  let line_height = match style.line_height {
    crate::style::types::LineHeight::Normal => style.font_size * 1.2,
    crate::style::types::LineHeight::Number(n) => style.font_size * n,
    crate::style::types::LineHeight::Length(len) => len
      .resolve_with_context(None, 0.0, 0.0, style.font_size, style.root_font_size)
      .unwrap_or_else(|| len.to_px()),
    crate::style::types::LineHeight::Percentage(pct) => style.font_size * (pct / 100.0),
  };

  let mut entries = vec![
    ("display", Value::String(format!("{:?}", style.display))),
    ("position", Value::String(format!("{:?}", style.position))),
    (
      "visibility",
      Value::String(format!("{:?}", style.visibility)),
    ),
    ("opacity", Value::from(style.opacity)),
    (
      "z_index",
      style.z_index.map(Value::from).unwrap_or(Value::Null),
    ),
    ("color", color_value(style.color)),
    ("background_color", color_value(style.background_color)),
    (
      "overflow",
      Value::String(format!("{:?}/{:?}", style.overflow_x, style.overflow_y)),
    ),
    ("transform_ops", Value::from(style.transform.len() as u64)),
    ("font_size", Value::from(style.font_size)),
    ("line_height", Value::from(line_height)),
  ];

  if let Some(bg) = style.background_layers.first() {
    entries.push((
      "background_image",
      Value::String(match &bg.image {
        Some(crate::style::types::BackgroundImage::Url(url)) => format!("url({})", url),
        Some(crate::style::types::BackgroundImage::None) => "none".into(),
        Some(_) => "gradient".into(),
        None => "none".into(),
      }),
    ));
  }

  map_from_pairs(entries)
}

fn attributes_value(attrs: &[(String, String)]) -> Value {
  let mut map = BTreeMap::new();
  for (name, value) in attrs {
    map.insert(name.clone(), Value::String(value.clone()));
  }
  let mut obj = Map::new();
  for (k, v) in map {
    obj.insert(k, v);
  }
  Value::Object(obj)
}

fn color_value(color: Rgba) -> Value {
  map_from_pairs(vec![
    ("r", Value::from(color.r)),
    ("g", Value::from(color.g)),
    ("b", Value::from(color.b)),
    ("a", Value::from(color.a)),
  ])
}

fn border_side_value(side: &crate::paint::display_list::BorderSide) -> Value {
  map_from_pairs(vec![
    ("width", Value::from(side.width)),
    ("style", Value::String(format!("{:?}", side.style))),
    ("color", color_value(side.color)),
  ])
}

fn transform3d_value(transform: &crate::paint::display_list::Transform3D) -> Value {
  Value::Array(transform.m.iter().map(|v| Value::from(*v)).collect())
}

fn rect_value(rect: Rect, offset: Option<Point>) -> Value {
  let (x, y) = if let Some(off) = offset {
    (rect.x() + off.x, rect.y() + off.y)
  } else {
    (rect.x(), rect.y())
  };
  map_from_pairs(vec![
    ("x", Value::from(x)),
    ("y", Value::from(y)),
    ("width", Value::from(rect.width())),
    ("height", Value::from(rect.height())),
  ])
}

fn point_value(point: Point) -> Value {
  map_from_pairs(vec![
    ("x", Value::from(point.x)),
    ("y", Value::from(point.y)),
  ])
}

fn map_from_pairs(entries: Vec<(&str, Value)>) -> Value {
  let mut sorted: BTreeMap<String, Value> = BTreeMap::new();
  for (key, value) in entries {
    sorted.insert(key.to_string(), value);
  }
  let mut map = Map::new();
  for (k, v) in sorted {
    map.insert(k, v);
  }
  Value::Object(map)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::Rect;
  use crate::geometry::Size;
  use crate::style::display::FormattingContextType;
  use crate::style::ComputedStyle;
  use crate::tree::debug::DebugInfo;
  use crate::tree::fragment_tree::FragmentTree;
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
    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![])
      .with_debug_info(debug_info);
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
    assert!(output.contains("fragment_index"));
  }

  #[test]
  fn test_fragment_forest_printer_multiple_roots() {
    let printer = EnhancedTreePrinter::with_config(PrintConfig::test());
    let mut root0 = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 800.0, 600.0), vec![]);
    root0.fragment_count = 2;
    let mut root1 = FragmentNode::new_block(Rect::from_xywh(0.0, 600.0, 800.0, 600.0), vec![]);
    root1.fragment_index = 1;
    root1.fragment_count = 2;
    root1.fragmentainer_index = 1;
    let tree = FragmentTree::from_fragments(vec![root0, root1], Size::new(800.0, 1200.0));

    let output = printer.print_fragment_forest(&tree);
    assert!(output.contains("root[0]"));
    assert!(output.contains("root[1]"));
    assert!(output.contains("fragmentainer_index=1"));
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
