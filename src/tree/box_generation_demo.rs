//! Demo/test box generation helpers built on placeholder DOM nodes.
//!
//! These APIs provide a lightweight way to exercise the box generation
//! pipeline without relying on the full DOM/style machinery. They are
//! intended for tests and examples rather than production rendering.

use crate::dom::{DomNode, DomNodeType, HTML_NAMESPACE};
use crate::geometry::Size;
use crate::style::cascade::StartingStyleSet;
use crate::style::counters::{CounterManager, CounterSet};
use crate::style::display::{Display, FormattingContextType};
use crate::style::types::ListStyleType;
use crate::style::types::TextTransform;
use crate::style::ComputedStyle;
use crate::tree::anonymous::AnonymousBoxCreator;
use crate::tree::box_generation::{marker_content_from_style, parse_srcset};
use crate::tree::box_tree::{BoxNode, BoxTree, ReplacedType, SvgContent};
use crate::tree::debug::DebugInfo;
use std::sync::Arc;

/// Simplified DOM node representation
///
/// This is a placeholder for the real DOM implementation. In production, this
/// is replaced by the actual DOM node tree; this version is kept for tests and
/// documentation examples that want a small, self-contained API.
#[derive(Debug, Clone)]
pub struct DOMNode {
  /// Element tag name (None for text nodes)
  pub tag_name: Option<String>,

  /// Element ID attribute
  pub id: Option<String>,

  /// Element classes
  pub classes: Vec<String>,

  /// Computed style for this element
  pub style: Arc<ComputedStyle>,

  /// Text content (for text nodes)
  pub text: Option<String>,

  /// Child nodes
  pub children: Vec<DOMNode>,

  /// Intrinsic size for replaced elements (images, video, etc.)
  pub intrinsic_size: Option<Size>,

  /// Source URL for replaced elements (img src, video src, etc.)
  pub src: Option<String>,

  /// Alternative text for replaced elements (e.g., <img alt="...">)
  pub alt: Option<String>,

  /// Srcset candidates for replaced elements
  pub srcset: Option<String>,

  /// Poster image for video elements
  pub poster: Option<String>,

  /// Inline HTML for iframe srcdoc
  pub srcdoc: Option<String>,
}

impl DOMNode {
  /// Creates a new element node
  pub fn new_element(
    tag_name: impl Into<String>,
    style: Arc<ComputedStyle>,
    children: Vec<DOMNode>,
  ) -> Self {
    Self {
      tag_name: Some(tag_name.into()),
      id: None,
      classes: Vec::new(),
      style,
      text: None,
      children,
      intrinsic_size: None,
      src: None,
      alt: None,
      srcset: None,
      poster: None,
      srcdoc: None,
    }
  }

  /// Creates a new text node
  pub fn new_text(text: impl Into<String>, style: Arc<ComputedStyle>) -> Self {
    Self {
      tag_name: None,
      id: None,
      classes: Vec::new(),
      style,
      text: Some(text.into()),
      children: Vec::new(),
      intrinsic_size: None,
      src: None,
      alt: None,
      srcset: None,
      poster: None,
      srcdoc: None,
    }
  }

  /// Creates a new replaced element (img, video, canvas, etc.)
  pub fn new_replaced(
    tag_name: impl Into<String>,
    style: Arc<ComputedStyle>,
    src: impl Into<String>,
    intrinsic_size: Option<Size>,
  ) -> Self {
    Self {
      tag_name: Some(tag_name.into()),
      id: None,
      classes: Vec::new(),
      style,
      text: None,
      children: Vec::new(),
      intrinsic_size,
      src: Some(src.into()),
      alt: None,
      srcset: None,
      poster: None,
      srcdoc: None,
    }
  }

  /// Creates a new replaced element with optional srcdoc (for iframes).
  pub fn new_replaced_with_srcdoc(
    tag_name: impl Into<String>,
    style: Arc<ComputedStyle>,
    src: impl Into<String>,
    intrinsic_size: Option<Size>,
    srcdoc: Option<String>,
  ) -> Self {
    Self {
      tag_name: Some(tag_name.into()),
      id: None,
      classes: Vec::new(),
      style,
      text: None,
      children: Vec::new(),
      intrinsic_size,
      src: Some(src.into()),
      alt: None,
      srcset: None,
      poster: None,
      srcdoc,
    }
  }

  /// Sets element ID (builder pattern)
  pub fn with_id(mut self, id: impl Into<String>) -> Self {
    self.id = Some(id.into());
    self
  }

  /// Adds a class (builder pattern)
  pub fn with_class(mut self, class: impl Into<String>) -> Self {
    self.classes.push(class.into());
    self
  }

  /// Sets the alt text (builder pattern)
  pub fn with_alt(mut self, alt: impl Into<String>) -> Self {
    self.alt = Some(alt.into());
    self
  }

  /// Sets poster (builder pattern)
  pub fn with_poster(mut self, poster: impl Into<String>) -> Self {
    self.poster = Some(poster.into());
    self
  }

  /// Sets intrinsic size (builder pattern)
  pub fn with_intrinsic_size(mut self, size: Size) -> Self {
    self.intrinsic_size = Some(size);
    self
  }

  /// Sets source URL (builder pattern)
  pub fn with_src(mut self, src: impl Into<String>) -> Self {
    self.src = Some(src.into());
    self
  }

  /// Returns true if this is a text node
  pub fn is_text(&self) -> bool {
    self.text.is_some()
  }

  /// Returns true if this is an element node
  pub fn is_element(&self) -> bool {
    self.tag_name.is_some()
  }

  /// Returns true if this is a replaced element
  pub fn is_replaced_element(&self) -> bool {
    if let Some(tag) = &self.tag_name {
      matches!(
        tag.as_str(),
        "img" | "video" | "canvas" | "svg" | "iframe" | "embed" | "object" | "audio" | "math"
      )
    } else {
      false
    }
  }

  /// Gets the display value from computed style
  pub fn display(&self) -> Display {
    self.style.display
  }

  /// Computes the aspect ratio from intrinsic size
  pub fn aspect_ratio(&self) -> Option<f32> {
    self.intrinsic_size.and_then(|size| {
      if size.height > 0.0 {
        Some(size.width / size.height)
      } else {
        None
      }
    })
  }

  /// Gets the replaced element type based on tag name
  pub fn replaced_type(&self) -> Option<ReplacedType> {
    if !self.is_replaced_element() {
      return None;
    }

    let tag = self.tag_name.as_ref()?;
    let src = self.src.clone().unwrap_or_default();
    let alt = self.alt.clone().filter(|s| !s.is_empty());
    let srcset = self
      .srcset
      .as_ref()
      .map(|v| parse_srcset(v))
      .unwrap_or_default();
    let poster = self.poster.clone().filter(|s| !s.is_empty());
    let srcdoc = self.srcdoc.clone().filter(|s| !s.is_empty());

    match tag.as_str() {
      "img" => Some(ReplacedType::Image {
        src,
        alt,
        srcset,
        sizes: None,
        picture_sources: Vec::new(),
      }),
      "video" => Some(ReplacedType::Video { src, poster }),
      "canvas" => Some(ReplacedType::Canvas),
      "svg" => Some(ReplacedType::Svg {
        content: SvgContent::raw(src),
      }),
      "iframe" => Some(ReplacedType::Iframe { src, srcdoc }),
      _ => None,
    }
  }
}

/// Configuration for box generation
#[derive(Debug, Clone)]
pub struct BoxGenerationConfig {
  /// Whether to generate debug info for boxes
  pub include_debug_info: bool,

  /// Whether to insert anonymous boxes (Wave 3)
  pub insert_anonymous_boxes: bool,
}

impl BoxGenerationConfig {
  pub fn new() -> Self {
    Self {
      include_debug_info: true,
      insert_anonymous_boxes: false,
    }
  }

  pub fn production() -> Self {
    Self {
      include_debug_info: false,
      insert_anonymous_boxes: false,
    }
  }

  pub fn development() -> Self {
    Self {
      include_debug_info: true,
      insert_anonymous_boxes: false,
    }
  }
}

impl Default for BoxGenerationConfig {
  fn default() -> Self {
    Self::new()
  }
}

/// Errors that can occur during box generation
#[derive(Debug)]
pub enum BoxGenerationError {
  RootDisplayNone,
  RootDisplayContents,
  InvalidDisplay(String),
  Unsupported(String),
  InvalidStructure(String),
}

impl std::fmt::Display for BoxGenerationError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::RootDisplayNone => write!(f, "Root element has display: none"),
      Self::RootDisplayContents => write!(f, "Root element has display: contents"),
      Self::InvalidDisplay(msg) => write!(f, "Invalid display value: {}", msg),
      Self::Unsupported(msg) => write!(f, "Unsupported feature: {}", msg),
      Self::InvalidStructure(msg) => write!(f, "Invalid structure: {}", msg),
    }
  }
}

impl std::error::Error for BoxGenerationError {}

/// Box generator - transforms placeholder DOM tree into Box tree
pub struct BoxGenerator {
  config: BoxGenerationConfig,
}

impl BoxGenerator {
  /// Creates a new box generator with default configuration
  pub fn new() -> Self {
    Self {
      config: BoxGenerationConfig::default(),
    }
  }

  /// Creates a box generator with custom configuration
  pub fn with_config(config: BoxGenerationConfig) -> Self {
    Self { config }
  }

  /// Generates a box tree from a DOM tree
  pub fn generate(&self, dom_root: &DOMNode) -> Result<BoxTree, BoxGenerationError> {
    if self.should_skip_element(dom_root) {
      return Err(BoxGenerationError::RootDisplayNone);
    }

    if self.is_display_contents(dom_root) {
      return Err(BoxGenerationError::RootDisplayContents);
    }

    let mut counters = CounterManager::new_with_styles(dom_root.style.counter_styles.clone());
    counters.enter_scope();
    let root_result = self.generate_box_for_element(dom_root, &mut counters);
    counters.leave_scope();
    let root_box = root_result?;

    let final_root = if self.config.insert_anonymous_boxes {
      AnonymousBoxCreator::fixup_tree(root_box)
    } else {
      root_box
    };

    Ok(BoxTree::new(final_root))
  }

  /// Generates a box tree with anonymous box fixup
  pub fn generate_with_anonymous_fixup(
    &self,
    dom_root: &DOMNode,
  ) -> Result<BoxTree, BoxGenerationError> {
    if self.should_skip_element(dom_root) {
      return Err(BoxGenerationError::RootDisplayNone);
    }

    if self.is_display_contents(dom_root) {
      return Err(BoxGenerationError::RootDisplayContents);
    }

    let mut counters = CounterManager::new_with_styles(dom_root.style.counter_styles.clone());
    counters.enter_scope();
    let root_result = self.generate_box_for_element(dom_root, &mut counters);
    counters.leave_scope();

    let root_box = root_result?;
    let fixed_root = AnonymousBoxCreator::fixup_tree(root_box);

    Ok(BoxTree::new(fixed_root))
  }

  fn generate_box_for_element(
    &self,
    node: &DOMNode,
    counters: &mut CounterManager,
  ) -> Result<BoxNode, BoxGenerationError> {
    if node.is_text() {
      return Ok(self.create_text_box(node));
    }

    counters.enter_scope();
    let result = self.generate_box_for_element_in_scope(node, counters);
    counters.leave_scope();
    result
  }

  fn generate_box_for_element_in_scope(
    &self,
    node: &DOMNode,
    counters: &mut CounterManager,
  ) -> Result<BoxNode, BoxGenerationError> {
    self.apply_counter_properties(node, counters);

    if node.is_replaced_element() {
      return self.create_replaced_box(node);
    }

    let child_boxes = self.generate_child_boxes(node, counters)?;

    let display = node.display();
    let fc_type = display.formatting_context_type();

    let box_node = match display {
      Display::Block | Display::FlowRoot | Display::ListItem => BoxNode::new_block(
        node.style.clone(),
        fc_type.unwrap_or(FormattingContextType::Block),
        child_boxes,
      ),

      Display::Inline => BoxNode::new_inline(node.style.clone(), child_boxes),

      Display::Ruby
      | Display::RubyBase
      | Display::RubyText
      | Display::RubyBaseContainer
      | Display::RubyTextContainer => BoxNode::new_inline(node.style.clone(), child_boxes),

      Display::InlineBlock => BoxNode::new_inline_block(
        node.style.clone(),
        FormattingContextType::Block,
        child_boxes,
      ),

      Display::Flex | Display::InlineFlex => {
        BoxNode::new_block(node.style.clone(), FormattingContextType::Flex, child_boxes)
      }

      Display::Grid | Display::InlineGrid => {
        BoxNode::new_block(node.style.clone(), FormattingContextType::Grid, child_boxes)
      }

      Display::Table | Display::InlineTable => BoxNode::new_block(
        node.style.clone(),
        FormattingContextType::Table,
        child_boxes,
      ),

      Display::TableRow
      | Display::TableCell
      | Display::TableRowGroup
      | Display::TableHeaderGroup
      | Display::TableFooterGroup
      | Display::TableColumn
      | Display::TableColumnGroup
      | Display::TableCaption => BoxNode::new_block(
        node.style.clone(),
        FormattingContextType::Block,
        child_boxes,
      ),

      Display::None | Display::Contents => {
        return Err(BoxGenerationError::InvalidDisplay(format!("{:?}", display)))
      }
    };

    let mut box_node = if box_node.style.display == Display::ListItem {
      self.attach_list_marker(box_node, counters)
    } else {
      box_node
    };

    if self.config.include_debug_info {
      let debug_info = self.create_debug_info(node);
      box_node = box_node.with_debug_info(debug_info);
    }

    Ok(box_node)
  }

  fn create_replaced_box(&self, node: &DOMNode) -> Result<BoxNode, BoxGenerationError> {
    let replaced_type = node.replaced_type().ok_or_else(|| {
      BoxGenerationError::Unsupported(format!("Not a replaced element: {:?}", node.tag_name))
    })?;

    let intrinsic_size = node.intrinsic_size;
    let aspect_ratio = node.aspect_ratio();

    let box_node = BoxNode::new_replaced(
      node.style.clone(),
      replaced_type,
      intrinsic_size,
      aspect_ratio,
    );

    if self.config.include_debug_info {
      let debug_info = self.create_debug_info(node);
      Ok(box_node.with_debug_info(debug_info))
    } else {
      Ok(box_node)
    }
  }

  fn generate_child_boxes(
    &self,
    parent: &DOMNode,
    counters: &mut CounterManager,
  ) -> Result<Vec<BoxNode>, BoxGenerationError> {
    let mut child_boxes = Vec::new();

    for child in &parent.children {
      if self.should_skip_element(child) {
        continue;
      }

      if self.is_display_contents(child) {
        counters.enter_scope();
        self.apply_counter_properties(child, counters);
        let grandchild_boxes = self.generate_child_boxes(child, counters)?;
        counters.leave_scope();

        child_boxes.extend(grandchild_boxes);
        continue;
      }

      let child_box = self.generate_box_for_element(child, counters)?;

      child_boxes.push(child_box);
    }

    Ok(child_boxes)
  }

  fn create_text_box(&self, node: &DOMNode) -> BoxNode {
    let text = node.text.as_ref().unwrap().clone();
    let box_node = BoxNode::new_text(node.style.clone(), text);

    if self.config.include_debug_info {
      let debug_info = DebugInfo::new(Some("text".to_string()), None, vec![]);
      box_node.with_debug_info(debug_info)
    } else {
      box_node
    }
  }

  fn create_debug_info(&self, node: &DOMNode) -> DebugInfo {
    DebugInfo::new(node.tag_name.clone(), node.id.clone(), node.classes.clone())
  }

  fn should_skip_element(&self, node: &DOMNode) -> bool {
    if let Some(tag) = node.tag_name.as_deref() {
      if matches!(
        tag.to_ascii_lowercase().as_str(),
        "head" | "style" | "script" | "meta" | "link" | "title" | "base" | "template"
      ) {
        return true;
      }
    }

    if node.is_text() {
      return false;
    }

    matches!(node.display(), Display::None)
  }

  fn is_display_contents(&self, node: &DOMNode) -> bool {
    if node.is_text() {
      return false;
    }

    matches!(node.display(), Display::Contents)
  }

  fn apply_counter_properties(&self, node: &DOMNode, counters: &mut CounterManager) {
    if node.is_text() {
      return;
    }

    let applied_default_reset = if let Some(reset) = &node.style.counters.counter_reset {
      counters.apply_reset(reset);
      true
    } else {
      false
    };

    if !applied_default_reset {
      if let Some(tag) = &node.tag_name {
        if matches!(
          tag.to_ascii_lowercase().as_str(),
          "ol" | "ul" | "menu" | "dir"
        ) {
          let default_reset = CounterSet::single("list-item", 0);
          counters.apply_reset(&default_reset);
        }
      }
    }

    if let Some(set) = &node.style.counters.counter_set {
      counters.apply_set(set);
    }

    if let Some(increment) = &node.style.counters.counter_increment {
      counters.apply_increment(increment);
    } else if node.display() == Display::ListItem {
      counters.apply_increment(&CounterSet::single("list-item", 1));
    }
  }

  fn attach_list_marker(&self, mut list_item: BoxNode, counters: &CounterManager) -> BoxNode {
    let style = list_item.style.clone();
    let dummy_dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "li".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let dummy_styled = crate::style::cascade::StyledNode {
      node_id: 0,
      node: dummy_dom,
      styles: (*style).clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };
    let marker_content = marker_content_from_style(&dummy_styled, &style, counters);
    if let Some(content) = marker_content {
      let mut marker_style = (*style).clone();
      marker_style.display = Display::Inline;
      crate::style::cascade::reset_marker_box_properties(&mut marker_style);
      marker_style.list_style_type = ListStyleType::None;
      marker_style.list_style_image = crate::style::types::ListStyleImage::None;
      marker_style.text_transform = TextTransform::none();

      let marker_node = BoxNode::new_marker(Arc::new(marker_style), content);
      list_item.children.insert(0, marker_node);
    }
    list_item
  }

  /// Counts total boxes in a generated tree (including root)
  pub fn count_boxes(box_node: &BoxNode) -> usize {
    1 + box_node
      .children
      .iter()
      .map(|child| Self::count_boxes(child))
      .sum::<usize>()
  }

  pub fn find_boxes_by_predicate<'a, F>(box_node: &'a BoxNode, predicate: F) -> Vec<&'a BoxNode>
  where
    F: Fn(&BoxNode) -> bool + Copy,
  {
    let mut result = Vec::new();

    if predicate(box_node) {
      result.push(box_node);
    }

    for child in &box_node.children {
      result.extend(Self::find_boxes_by_predicate(child, predicate));
    }

    result
  }

  pub fn find_block_boxes(box_node: &BoxNode) -> Vec<&BoxNode> {
    Self::find_boxes_by_predicate(box_node, |b| b.is_block_level())
  }

  pub fn find_inline_boxes(box_node: &BoxNode) -> Vec<&BoxNode> {
    Self::find_boxes_by_predicate(box_node, |b| b.is_inline_level())
  }

  pub fn find_text_boxes(box_node: &BoxNode) -> Vec<&BoxNode> {
    Self::find_boxes_by_predicate(box_node, |b| b.is_text())
  }

  pub fn find_replaced_boxes(box_node: &BoxNode) -> Vec<&BoxNode> {
    Self::find_boxes_by_predicate(box_node, |b| b.is_replaced())
  }

  pub fn validate_box_tree(box_node: &BoxNode) -> Result<(), BoxGenerationError> {
    if box_node.is_text() && !box_node.children.is_empty() {
      return Err(BoxGenerationError::InvalidStructure(
        "Text box cannot have children".to_string(),
      ));
    }

    if box_node.is_replaced() && !box_node.children.is_empty() {
      return Err(BoxGenerationError::InvalidStructure(
        "Replaced box cannot have children".to_string(),
      ));
    }

    for child in &box_node.children {
      Self::validate_box_tree(child)?;
    }

    Ok(())
  }

  pub fn tree_depth(box_node: &BoxNode) -> usize {
    if box_node.children.is_empty() {
      1
    } else {
      1 + box_node
        .children
        .iter()
        .map(Self::tree_depth)
        .max()
        .unwrap_or(0)
    }
  }
}

impl Default for BoxGenerator {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::style::display::Display;
  use crate::tree::box_tree::ReplacedType;

  fn default_style() -> Arc<ComputedStyle> {
    Arc::new(ComputedStyle::default())
  }

  fn style_with_display(display: Display) -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.display = display;
    Arc::new(style)
  }

  #[test]
  fn test_config_creation() {
    let config = BoxGenerationConfig::new();
    assert!(config.include_debug_info);
    assert!(!config.insert_anonymous_boxes);
  }

  #[test]
  fn test_config_production() {
    let config = BoxGenerationConfig::production();
    assert!(!config.include_debug_info);
  }

  #[test]
  fn test_config_development() {
    let config = BoxGenerationConfig::development();
    assert!(config.include_debug_info);
  }

  #[test]
  fn test_generator_creation() {
    let generator = BoxGenerator::new();
    assert!(generator.config.include_debug_info);
  }

  #[test]
  fn test_generator_with_config() {
    let config = BoxGenerationConfig::production();
    let generator = BoxGenerator::with_config(config);
    assert!(!generator.config.include_debug_info);
  }

  #[test]
  fn test_dom_node_element() {
    let dom = DOMNode::new_element("div", default_style(), vec![]);
    assert!(dom.is_element());
    assert!(!dom.is_text());
    assert_eq!(dom.tag_name, Some("div".to_string()));
  }

  #[test]
  fn test_dom_node_text() {
    let dom = DOMNode::new_text("Hello", default_style());
    assert!(dom.is_text());
    assert!(!dom.is_element());
    assert_eq!(dom.text, Some("Hello".to_string()));
  }

  #[test]
  fn test_dom_node_with_id() {
    let dom = DOMNode::new_element("div", default_style(), vec![]).with_id("header");
    assert_eq!(dom.id, Some("header".to_string()));
  }

  #[test]
  fn test_dom_node_with_class() {
    let dom = DOMNode::new_element("div", default_style(), vec![]).with_class("navbar");
    assert_eq!(dom.classes.len(), 1);
    assert_eq!(dom.classes[0], "navbar");
  }

  #[test]
  fn test_generate_single_block() {
    let generator = BoxGenerator::new();
    let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);

    let box_tree = generator.generate(&dom).unwrap();
    assert!(box_tree.root.is_block_level());
    assert_eq!(box_tree.count_boxes(), 1);
  }

  #[test]
  fn test_generate_block_with_text() {
    let generator = BoxGenerator::new();
    let text = DOMNode::new_text("Hello", default_style());
    let dom = DOMNode::new_element("p", style_with_display(Display::Block), vec![text]);

    let box_tree = generator.generate(&dom).unwrap();
    assert_eq!(box_tree.count_boxes(), 2);
    assert_eq!(box_tree.count_text_boxes(), 1);
  }

  #[test]
  fn test_generate_nested_blocks() {
    let generator = BoxGenerator::new();

    let inner = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
    let outer = DOMNode::new_element("div", style_with_display(Display::Block), vec![inner]);

    let box_tree = generator.generate(&outer).unwrap();
    assert_eq!(box_tree.count_boxes(), 2);
    assert_eq!(box_tree.root.children.len(), 1);
  }

  #[test]
  fn test_generate_multiple_children() {
    let generator = BoxGenerator::new();

    let child1 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
    let child2 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
    let child3 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);

    let dom = DOMNode::new_element(
      "div",
      style_with_display(Display::Block),
      vec![child1, child2, child3],
    );

    let box_tree = generator.generate(&dom).unwrap();
    assert_eq!(box_tree.count_boxes(), 4);
    assert_eq!(box_tree.root.children.len(), 3);
  }

  #[test]
  fn test_skip_display_none_child() {
    let generator = BoxGenerator::new();

    let child1 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
    let child2 = DOMNode::new_element("p", style_with_display(Display::None), vec![]);
    let child3 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);

    let dom = DOMNode::new_element(
      "div",
      style_with_display(Display::Block),
      vec![child1, child2, child3],
    );

    let box_tree = generator.generate(&dom).unwrap();
    assert_eq!(box_tree.count_boxes(), 3);
    assert_eq!(box_tree.root.children.len(), 2);
  }

  #[test]
  fn test_display_contents_adopts_children() {
    let generator = BoxGenerator::new();

    let grandchild1 = DOMNode::new_element("span", style_with_display(Display::Inline), vec![]);
    let grandchild2 = DOMNode::new_element("span", style_with_display(Display::Inline), vec![]);

    let child = DOMNode::new_element(
      "p",
      style_with_display(Display::Contents),
      vec![grandchild1, grandchild2],
    );

    let root = DOMNode::new_element("div", style_with_display(Display::Block), vec![child]);

    let box_tree = generator.generate(&root).unwrap();
    assert_eq!(box_tree.count_boxes(), 3);
    assert_eq!(box_tree.root.children.len(), 2);
  }

  #[test]
  fn test_text_box_creation() {
    let generator = BoxGenerator::new();
    let text = DOMNode::new_text("Hello, world!", default_style());

    let text_box = generator.create_text_box(&text);

    assert!(text_box.is_text());
    assert_eq!(text_box.text(), Some("Hello, world!"));
  }

  #[test]
  fn test_debug_info_included() {
    let config = BoxGenerationConfig::development();
    let generator = BoxGenerator::with_config(config);

    let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![])
      .with_id("header")
      .with_class("navbar");

    let box_tree = generator.generate(&dom).unwrap();
    assert!(box_tree.root.debug_info.is_some());

    let debug_info = box_tree.root.debug_info.as_ref().unwrap();
    assert_eq!(debug_info.tag_name, Some("div".to_string()));
    assert_eq!(debug_info.id, Some("header".to_string()));
    assert_eq!(debug_info.classes.len(), 1);
  }

  #[test]
  fn test_debug_info_excluded() {
    let config = BoxGenerationConfig::production();
    let generator = BoxGenerator::with_config(config);

    let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);

    let box_tree = generator.generate(&dom).unwrap();
    assert!(box_tree.root.debug_info.is_none());
  }

  #[test]
  fn test_complex_tree_structure() {
    let generator = BoxGenerator::new();

    let text1 = DOMNode::new_text("Text 1", default_style());
    let text2 = DOMNode::new_text("Text 2", default_style());
    let text3 = DOMNode::new_text("Text 3", default_style());

    let p1 = DOMNode::new_element("p", style_with_display(Display::Block), vec![text1]);
    let p2 = DOMNode::new_element("p", style_with_display(Display::Block), vec![text2]);
    let p3 = DOMNode::new_element("p", style_with_display(Display::Block), vec![text3]);

    let inner_div = DOMNode::new_element("div", style_with_display(Display::Block), vec![p3]);
    let outer_div = DOMNode::new_element(
      "div",
      style_with_display(Display::Block),
      vec![p1, p2, inner_div],
    );

    let box_tree = generator.generate(&outer_div).unwrap();

    assert_eq!(box_tree.count_boxes(), 8);
    assert_eq!(box_tree.count_text_boxes(), 3);
  }

  #[test]
  fn test_inline_box_generation() {
    let generator = BoxGenerator::new();

    let text = DOMNode::new_text("Hello", default_style());
    let span = DOMNode::new_element("span", style_with_display(Display::Inline), vec![text]);
    let p = DOMNode::new_element("p", style_with_display(Display::Block), vec![span]);

    let box_tree = generator.generate(&p).unwrap();

    assert_eq!(box_tree.count_boxes(), 3);
    assert!(box_tree.root.is_block_level());
    assert!(box_tree.root.children[0].is_inline_level());
  }

  #[test]
  fn test_document_order_preserved() {
    let generator = BoxGenerator::new();

    let child1 =
      DOMNode::new_element("p", style_with_display(Display::Block), vec![]).with_id("first");
    let child2 =
      DOMNode::new_element("p", style_with_display(Display::Block), vec![]).with_id("second");
    let child3 =
      DOMNode::new_element("p", style_with_display(Display::Block), vec![]).with_id("third");

    let dom = DOMNode::new_element(
      "div",
      style_with_display(Display::Block),
      vec![child1, child2, child3],
    );

    let box_tree = generator.generate(&dom).unwrap();

    assert_eq!(box_tree.root.children.len(), 3);
    let ids: Vec<_> = box_tree
      .root
      .children
      .iter()
      .filter_map(|c| c.debug_info.as_ref())
      .filter_map(|d| d.id.as_ref())
      .map(|s| s.as_str())
      .collect();

    assert_eq!(ids, vec!["first", "second", "third"]);
  }

  #[test]
  fn test_mixed_element_and_text_nodes() {
    let generator = BoxGenerator::new();

    let text1 = DOMNode::new_text("Before", default_style());
    let em = DOMNode::new_element("em", style_with_display(Display::Inline), vec![]);
    let text2 = DOMNode::new_text("After", default_style());

    let p = DOMNode::new_element(
      "p",
      style_with_display(Display::Block),
      vec![text1, em, text2],
    );

    let box_tree = generator.generate(&p).unwrap();

    assert_eq!(box_tree.count_boxes(), 4);
    assert_eq!(box_tree.count_text_boxes(), 2);
    assert_eq!(box_tree.root.children.len(), 3);
  }

  #[test]
  fn test_generator_reuse() {
    let generator = BoxGenerator::new();

    for i in 0..10 {
      let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![])
        .with_id(format!("div-{}", i));

      let result = generator.generate(&dom);
      assert!(result.is_ok());
    }
  }

  #[test]
  fn test_empty_element() {
    let generator = BoxGenerator::new();
    let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);

    let box_tree = generator.generate(&dom).unwrap();
    assert_eq!(box_tree.count_boxes(), 1);
    assert_eq!(box_tree.root.children.len(), 0);
  }

  #[test]
  fn test_deeply_nested_structure() {
    let generator = BoxGenerator::new();

    let mut current = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
    for _ in 0..4 {
      current = DOMNode::new_element("div", style_with_display(Display::Block), vec![current]);
    }

    let box_tree = generator.generate(&current).unwrap();
    assert_eq!(box_tree.count_boxes(), 5);
  }

  #[test]
  fn test_flex_container() {
    let generator = BoxGenerator::new();

    let child = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);
    let flex_container =
      DOMNode::new_element("div", style_with_display(Display::Flex), vec![child]);

    let box_tree = generator.generate(&flex_container).unwrap();

    assert_eq!(box_tree.count_boxes(), 2);
    assert_eq!(
      box_tree.root.formatting_context(),
      Some(FormattingContextType::Flex)
    );
  }

  #[test]
  fn test_grid_container() {
    let generator = BoxGenerator::new();

    let child = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);
    let grid_container =
      DOMNode::new_element("div", style_with_display(Display::Grid), vec![child]);

    let box_tree = generator.generate(&grid_container).unwrap();

    assert_eq!(box_tree.count_boxes(), 2);
    assert_eq!(
      box_tree.root.formatting_context(),
      Some(FormattingContextType::Grid)
    );
  }

  #[test]
  fn test_root_display_none_error() {
    let generator = BoxGenerator::new();
    let dom = DOMNode::new_element("div", style_with_display(Display::None), vec![]);

    let result = generator.generate(&dom);
    assert!(matches!(result, Err(BoxGenerationError::RootDisplayNone)));
  }

  #[test]
  fn test_root_display_contents_error() {
    let generator = BoxGenerator::new();
    let dom = DOMNode::new_element("div", style_with_display(Display::Contents), vec![]);

    let result = generator.generate(&dom);
    assert!(matches!(
      result,
      Err(BoxGenerationError::RootDisplayContents)
    ));
  }

  #[test]
  fn test_dom_node_is_replaced_element() {
    let style = default_style();

    assert!(DOMNode::new_element("img", style.clone(), vec![]).is_replaced_element());
    assert!(DOMNode::new_element("video", style.clone(), vec![]).is_replaced_element());
    assert!(DOMNode::new_element("canvas", style.clone(), vec![]).is_replaced_element());
    assert!(DOMNode::new_element("svg", style.clone(), vec![]).is_replaced_element());
    assert!(DOMNode::new_element("iframe", style.clone(), vec![]).is_replaced_element());
    assert!(DOMNode::new_element("embed", style.clone(), vec![]).is_replaced_element());
    assert!(DOMNode::new_element("object", style.clone(), vec![]).is_replaced_element());
    assert!(DOMNode::new_element("audio", style.clone(), vec![]).is_replaced_element());
    assert!(DOMNode::new_element("math", style.clone(), vec![]).is_replaced_element());

    assert!(!DOMNode::new_element("div", style.clone(), vec![]).is_replaced_element());
    assert!(!DOMNode::new_element("span", style.clone(), vec![]).is_replaced_element());
    assert!(!DOMNode::new_element("p", style.clone(), vec![]).is_replaced_element());
    assert!(!DOMNode::new_text("text", style.clone()).is_replaced_element());
  }

  #[test]
  fn test_new_replaced_constructor() {
    let style = default_style();
    let size = Size::new(100.0, 50.0);

    let img = DOMNode::new_replaced("img", style.clone(), "image.png", Some(size));

    assert_eq!(img.tag_name.as_deref(), Some("img"));
    assert_eq!(img.src.as_deref(), Some("image.png"));
    assert_eq!(img.intrinsic_size, Some(size));
    assert!(img.is_replaced_element());
  }

  #[test]
  fn test_replaced_element_aspect_ratio() {
    let style = default_style();
    let size = Size::new(100.0, 50.0);

    let img = DOMNode::new_replaced("img", style.clone(), "test.png", Some(size));
    assert_eq!(img.aspect_ratio(), Some(2.0));

    let img_no_size = DOMNode::new_replaced("img", style.clone(), "test.png", None);
    assert_eq!(img_no_size.aspect_ratio(), None);

    let zero_height = DOMNode::new_replaced("img", style, "test.png", Some(Size::new(100.0, 0.0)));
    assert_eq!(zero_height.aspect_ratio(), None);
  }

  #[test]
  fn test_replaced_element_type_detection() {
    let style = default_style();

    let img = DOMNode::new_replaced("img", style.clone(), "test.png", None);
    assert!(matches!(
      img.replaced_type(),
      Some(ReplacedType::Image { .. })
    ));

    let video = DOMNode::new_replaced("video", style.clone(), "test.mp4", None);
    assert!(matches!(
      video.replaced_type(),
      Some(ReplacedType::Video { .. })
    ));

    let canvas = DOMNode::new_element("canvas", style.clone(), vec![]);
    assert!(matches!(canvas.replaced_type(), Some(ReplacedType::Canvas)));

    let div = DOMNode::new_element("div", style, vec![]);
    assert!(div.replaced_type().is_none());
  }

  #[test]
  fn test_generate_replaced_box() {
    let generator = BoxGenerator::new();
    let style = default_style();
    let size = Size::new(200.0, 100.0);

    let img = DOMNode::new_replaced("img", style.clone(), "test.png", Some(size));
    let wrapper = DOMNode::new_element("div", style_with_display(Display::Block), vec![img]);

    let box_tree = generator.generate(&wrapper).unwrap();

    assert_eq!(box_tree.count_boxes(), 2);

    let img_box = &box_tree.root.children[0];
    assert!(img_box.is_replaced());
  }

  #[test]
  fn test_generate_multiple_replaced_elements() {
    let generator = BoxGenerator::new();
    let style = default_style();

    let img1 = DOMNode::new_replaced(
      "img",
      style.clone(),
      "img1.png",
      Some(Size::new(100.0, 100.0)),
    );
    let video = DOMNode::new_replaced(
      "video",
      style.clone(),
      "video.mp4",
      Some(Size::new(640.0, 480.0)),
    );
    let img2 = DOMNode::new_replaced("img", style.clone(), "img2.png", None);

    let container = DOMNode::new_element(
      "div",
      style_with_display(Display::Block),
      vec![img1, video, img2],
    );

    let box_tree = generator.generate(&container).unwrap();

    assert_eq!(box_tree.count_boxes(), 4);
    assert_eq!(BoxGenerator::find_replaced_boxes(&box_tree.root).len(), 3);
  }

  #[test]
  fn test_replaced_element_with_display_none_child() {
    let generator = BoxGenerator::new();
    let style = default_style();

    let img = DOMNode::new_replaced("img", style.clone(), "test.png", None);
    let container = DOMNode::new_element("div", style_with_display(Display::Block), vec![img]);

    let box_tree = generator.generate(&container).unwrap();

    assert_eq!(box_tree.count_boxes(), 2);
    let replaced_boxes = BoxGenerator::find_replaced_boxes(&box_tree.root);
    assert_eq!(replaced_boxes.len(), 1);
    assert!(replaced_boxes[0].children.is_empty());
  }

  #[test]
  fn test_replaced_element_inline_context() {
    let generator = BoxGenerator::new();
    let style = default_style();

    let text = DOMNode::new_text("Hello ", style.clone());
    let img = DOMNode::new_replaced(
      "img",
      style.clone(),
      "icon.png",
      Some(Size::new(16.0, 16.0)),
    );
    let text2 = DOMNode::new_text(" World", style.clone());

    let span = DOMNode::new_element(
      "span",
      style_with_display(Display::Inline),
      vec![text, img, text2],
    );
    let p = DOMNode::new_element("p", style_with_display(Display::Block), vec![span]);

    let box_tree = generator.generate(&p).unwrap();

    assert_eq!(box_tree.count_boxes(), 5);
    assert_eq!(BoxGenerator::find_replaced_boxes(&box_tree.root).len(), 1);
    assert_eq!(BoxGenerator::find_text_boxes(&box_tree.root).len(), 2);
  }

  #[test]
  fn test_count_boxes_utility() {
    let style = default_style();

    let single = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    assert_eq!(BoxGenerator::count_boxes(&single), 1);

    let children = vec![
      BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]),
      BoxNode::new_inline(style.clone(), vec![]),
      BoxNode::new_text(style.clone(), "text".to_string()),
    ];
    let parent = BoxNode::new_block(style, FormattingContextType::Block, children);
    assert_eq!(BoxGenerator::count_boxes(&parent), 4);
  }

  #[test]
  fn test_find_boxes_by_predicate() {
    let style = default_style();

    let text1 = BoxNode::new_text(style.clone(), "Hello".to_string());
    let text2 = BoxNode::new_text(style.clone(), "World".to_string());
    let inline = BoxNode::new_inline(style.clone(), vec![text1]);
    let block = BoxNode::new_block(
      style.clone(),
      FormattingContextType::Block,
      vec![inline, text2],
    );

    let text_boxes = BoxGenerator::find_boxes_by_predicate(&block, |b| b.is_text());
    assert_eq!(text_boxes.len(), 2);

    let inline_boxes = BoxGenerator::find_boxes_by_predicate(&block, |b| b.is_inline_level());
    assert_eq!(inline_boxes.len(), 3);
  }

  #[test]
  fn test_find_block_boxes() {
    let style = default_style();

    let text = BoxNode::new_text(style.clone(), "text".to_string());
    let inline = BoxNode::new_inline(style.clone(), vec![]);
    let block1 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    let block2 = BoxNode::new_block(style.clone(), FormattingContextType::Flex, vec![]);
    let root = BoxNode::new_block(
      style,
      FormattingContextType::Block,
      vec![text, inline, block1, block2],
    );

    let blocks = BoxGenerator::find_block_boxes(&root);
    assert_eq!(blocks.len(), 3);
  }

  #[test]
  fn test_find_inline_boxes() {
    let style = default_style();

    let text = BoxNode::new_text(style.clone(), "text".to_string());
    let inline1 = BoxNode::new_inline(style.clone(), vec![text]);
    let inline2 = BoxNode::new_inline(style.clone(), vec![]);
    let root = BoxNode::new_block(style, FormattingContextType::Block, vec![inline1, inline2]);

    let inlines = BoxGenerator::find_inline_boxes(&root);
    assert_eq!(inlines.len(), 3);
  }

  #[test]
  fn test_find_text_boxes() {
    let style = default_style();

    let text1 = BoxNode::new_text(style.clone(), "Hello".to_string());
    let text2 = BoxNode::new_text(style.clone(), "World".to_string());
    let inline = BoxNode::new_inline(style.clone(), vec![text1]);
    let root = BoxNode::new_block(style, FormattingContextType::Block, vec![inline, text2]);

    let texts = BoxGenerator::find_text_boxes(&root);
    assert_eq!(texts.len(), 2);
  }

  #[test]
  fn test_find_replaced_boxes_utility() {
    let style = default_style();

    let img = BoxNode::new_replaced(
      style.clone(),
      ReplacedType::Image {
        src: "test.png".to_string(),
        alt: None,
        sizes: None,
        srcset: Vec::new(),
        picture_sources: Vec::new(),
      },
      Some(Size::new(100.0, 100.0)),
      Some(1.0),
    );
    let video = BoxNode::new_replaced(
      style.clone(),
      ReplacedType::Video {
        src: "test.mp4".to_string(),
        poster: None,
      },
      None,
      None,
    );
    let text = BoxNode::new_text(style.clone(), "text".to_string());
    let root = BoxNode::new_block(style, FormattingContextType::Block, vec![img, video, text]);

    let replaced = BoxGenerator::find_replaced_boxes(&root);
    assert_eq!(replaced.len(), 2);
  }

  #[test]
  fn test_validate_box_tree_valid() {
    let style = default_style();

    let text = BoxNode::new_text(style.clone(), "Hello".to_string());
    let inline = BoxNode::new_inline(style.clone(), vec![text]);
    let root = BoxNode::new_block(style, FormattingContextType::Block, vec![inline]);

    assert!(BoxGenerator::validate_box_tree(&root).is_ok());
  }

  #[test]
  fn test_tree_depth() {
    let style = default_style();

    let single = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    assert_eq!(BoxGenerator::tree_depth(&single), 1);

    let child = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    let root = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![child]);
    assert_eq!(BoxGenerator::tree_depth(&root), 2);

    let leaf = BoxNode::new_text(style.clone(), "text".to_string());
    let level3 = BoxNode::new_inline(style.clone(), vec![leaf]);
    let level2 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![level3]);
    let level1 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![level2]);
    assert_eq!(BoxGenerator::tree_depth(&level1), 4);
  }

  #[test]
  fn test_with_builder_methods() {
    let style = default_style();
    let size = Size::new(50.0, 25.0);

    let node = DOMNode::new_element("img", style, vec![])
      .with_id("my-image")
      .with_class("thumbnail")
      .with_class("responsive")
      .with_src("path/to/image.png")
      .with_intrinsic_size(size);

    assert_eq!(node.id.as_deref(), Some("my-image"));
    assert_eq!(node.classes.len(), 2);
    assert_eq!(node.src.as_deref(), Some("path/to/image.png"));
    assert_eq!(node.intrinsic_size, Some(size));
  }

  #[test]
  fn test_empty_replaced_src() {
    let style = default_style();

    let img = DOMNode::new_replaced("img", style.clone(), "", None);
    let wrapper = DOMNode::new_element("div", style_with_display(Display::Block), vec![img]);

    let generator = BoxGenerator::new();
    let box_tree = generator.generate(&wrapper).unwrap();

    assert_eq!(box_tree.count_boxes(), 2);
  }

  #[test]
  fn test_mixed_content_with_replaced() {
    let generator = BoxGenerator::new();
    let style = default_style();

    let text1 = DOMNode::new_text("Before image ", style.clone());
    let img = DOMNode::new_replaced(
      "img",
      style.clone(),
      "test.png",
      Some(Size::new(100.0, 50.0)),
    );
    let text2 = DOMNode::new_text(" after image ", style.clone());
    let video = DOMNode::new_replaced("video", style.clone(), "test.mp4", None);
    let nested_div = DOMNode::new_element(
      "div",
      style_with_display(Display::Block),
      vec![DOMNode::new_text("Nested", style.clone())],
    );

    let root = DOMNode::new_element(
      "div",
      style_with_display(Display::Block),
      vec![text1, img, text2, video, nested_div],
    );

    let box_tree = generator.generate(&root).unwrap();

    assert_eq!(box_tree.count_boxes(), 7);
    assert_eq!(BoxGenerator::find_replaced_boxes(&box_tree.root).len(), 2);
    assert_eq!(BoxGenerator::find_text_boxes(&box_tree.root).len(), 3);
  }

  #[test]
  fn test_svg_replaced_element() {
    let generator = BoxGenerator::new();
    let style = default_style();

    let svg = DOMNode::new_replaced(
      "svg",
      style.clone(),
      "<svg>...</svg>",
      Some(Size::new(100.0, 100.0)),
    );
    let wrapper = DOMNode::new_element("div", style_with_display(Display::Block), vec![svg]);

    let box_tree = generator.generate(&wrapper).unwrap();

    assert_eq!(BoxGenerator::find_replaced_boxes(&box_tree.root).len(), 1);
  }

  // Anonymous box fixup toggles
  #[test]
  fn test_generator_with_anonymous_fixup_enabled() {
    let mut config = BoxGenerationConfig::new();
    config.insert_anonymous_boxes = true;

    let generator = BoxGenerator::with_config(config);

    let text = DOMNode::new_text("Hello", style_with_display(Display::Block));
    let block = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);

    let root = DOMNode::new_element("div", style_with_display(Display::Block), vec![text, block]);

    let box_tree = generator.generate(&root).unwrap();

    assert!(box_tree.root.children.iter().any(|c| c.is_anonymous()));
  }

  #[test]
  fn test_generator_with_anonymous_fixup_disabled() {
    let config = BoxGenerationConfig::new();
    let generator = BoxGenerator::with_config(config);

    let text = DOMNode::new_text("Hello", style_with_display(Display::Block));
    let block = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);

    let root = DOMNode::new_element("div", style_with_display(Display::Block), vec![text, block]);

    let box_tree = generator.generate(&root).unwrap();

    assert!(!box_tree.root.children.iter().any(|c| c.is_anonymous()));
  }

  #[test]
  fn test_generate_with_anonymous_fixup_method() {
    let generator = BoxGenerator::new();

    let text = DOMNode::new_text("Hello", style_with_display(Display::Block));
    let block = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);

    let root = DOMNode::new_element("div", style_with_display(Display::Block), vec![text, block]);

    let box_tree = generator.generate_with_anonymous_fixup(&root).unwrap();

    assert!(box_tree.root.children.iter().any(|c| c.is_anonymous()));
  }
}
