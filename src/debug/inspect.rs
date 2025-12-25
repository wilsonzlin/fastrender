use crate::css::selectors::{FastRenderSelectorImpl, PseudoClassParser};
use crate::dom::{DomNode, DomNodeType, ElementRef};
use crate::error::{Error, RenderError};
use crate::geometry::Rect;
use crate::scroll::{ScrollMetadata, ScrollSnapContainer};
use crate::style::cascade::StyledNode;
use crate::style::color::Rgba;
use crate::style::display::{Display, FormattingContextType};
use crate::style::float::Float;
use crate::style::position::Position;
use crate::style::types::{LineHeight, Overflow};
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::tree::box_tree::{BoxNode, BoxType};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use cssparser::{Parser, ParserInput};
use selectors::matching::{
  matches_selector, MatchingContext, MatchingForInvalidation, MatchingMode, NeedsSelectorFlags,
  QuirksMode, SelectorCaches,
};
use selectors::parser::SelectorList;
use serde::Serialize;
use std::collections::HashMap;

/// Query used to locate nodes for inspection.
#[derive(Debug, Clone)]
pub enum InspectQuery {
  /// CSS selector (supports the same parser as the style system).
  Selector(String),
  /// Match the `id` attribute exactly.
  Id(String),
  /// Match a pre-order styled node id.
  NodeId(usize),
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct NodeSnapshot {
  pub node_id: usize,
  pub tag_name: Option<String>,
  pub id: Option<String>,
  pub classes: Vec<String>,
  pub attributes: Vec<(String, String)>,
  pub ancestry: Vec<String>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct ColorSnapshot {
  pub r: u8,
  pub g: u8,
  pub b: u8,
  pub a: f32,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct EdgeSnapshot<T> {
  pub top: T,
  pub right: T,
  pub bottom: T,
  pub left: T,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct ComputedStyleSnapshot {
  pub display: String,
  pub position: String,
  pub float: String,
  pub overflow_x: String,
  pub overflow_y: String,
  pub width: Option<f32>,
  pub height: Option<f32>,
  pub min_width: Option<f32>,
  pub min_height: Option<f32>,
  pub margin: EdgeSnapshot<Option<f32>>,
  pub padding: EdgeSnapshot<f32>,
  pub border: EdgeSnapshot<f32>,
  pub color: ColorSnapshot,
  pub background_color: ColorSnapshot,
  pub font_size: f32,
  pub line_height: f32,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct BoxSnapshot {
  pub box_id: usize,
  pub styled_node_id: Option<usize>,
  pub debug_selector: Option<String>,
  pub box_type: String,
  pub display: String,
  pub formatting_context: Option<String>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct RectSnapshot {
  pub x: f32,
  pub y: f32,
  pub width: f32,
  pub height: f32,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct SizeSnapshot {
  pub width: f32,
  pub height: f32,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct ScrollContainerSnapshot {
  pub viewport: SizeSnapshot,
  pub scroll_bounds: RectSnapshot,
  pub snap_x: bool,
  pub snap_y: bool,
  pub strictness: String,
  pub behavior: String,
  pub uses_viewport_scroll: bool,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct FragmentSnapshot {
  pub box_id: Option<usize>,
  pub kind: String,
  pub bounds: RectSnapshot,
  pub baseline: Option<f32>,
  pub fragment_index: usize,
  pub fragment_count: usize,
  pub fragmentainer: usize,
  pub scroll_overflow: RectSnapshot,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub scroll_container: Option<ScrollContainerSnapshot>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub text: Option<String>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct InspectionSnapshot {
  pub node: NodeSnapshot,
  pub style: ComputedStyleSnapshot,
  pub boxes: Vec<BoxSnapshot>,
  pub fragments: Vec<FragmentSnapshot>,
}

/// Inspect a styled/layouted tree using a simple query.
///
/// The output mirrors the public snapshot structs to avoid leaking internal
/// engine types.
pub fn inspect(
  styled_root: &StyledNode,
  box_root: &BoxNode,
  fragments: &FragmentTree,
  query: InspectQuery,
) -> Result<Vec<InspectionSnapshot>, Error> {
  let mut styled_boxes: HashMap<usize, Vec<&BoxNode>> = HashMap::new();
  collect_styled_boxes(box_root, &mut styled_boxes);

  let mut fragments_by_box: HashMap<usize, Vec<&FragmentNode>> = HashMap::new();
  collect_fragments_for_box(&fragments.root, &mut fragments_by_box);
  for root in &fragments.additional_fragments {
    collect_fragments_for_box(root, &mut fragments_by_box);
  }
  let scroll_map = fragments
    .scroll_metadata
    .as_ref()
    .map(scroll_containers_by_box)
    .unwrap_or_default();

  let matches = match query {
    InspectQuery::NodeId(id) => collect_matches(styled_root, &mut Vec::new(), &mut |node, _| {
      node.node_id == id
    }),
    InspectQuery::Id(id) => collect_matches(styled_root, &mut Vec::new(), &mut |node, _| {
      node
        .node
        .get_attribute_ref("id")
        .is_some_and(|value| value == id)
    }),
    InspectQuery::Selector(selector) => {
      let selectors = parse_selectors(&selector)?;
      let mut caches = SelectorCaches::default();
      collect_matches(styled_root, &mut Vec::new(), &mut |node, ancestors| {
        node_matches_selector(node, ancestors, &selectors, &mut caches)
      })
    }
  };

  Ok(
    matches
      .into_iter()
      .map(|(ancestors, node)| {
        build_snapshot(
          node,
          &ancestors,
          &styled_boxes,
          &fragments_by_box,
          &scroll_map,
        )
      })
      .collect(),
  )
}

fn parse_selectors(selector: &str) -> Result<SelectorList<FastRenderSelectorImpl>, Error> {
  let mut input = ParserInput::new(selector);
  let mut parser = Parser::new(&mut input);
  SelectorList::parse(
    &PseudoClassParser,
    &mut parser,
    selectors::parser::ParseRelative::Normal,
  )
  .map_err(|e| {
    Error::Render(RenderError::InvalidParameters {
      message: format!("Invalid selector {selector:?}: {e}"),
    })
  })
}

fn collect_matches<'a, F>(
  node: &'a StyledNode,
  ancestors: &mut Vec<&'a StyledNode>,
  predicate: &mut F,
) -> Vec<(Vec<&'a StyledNode>, &'a StyledNode)>
where
  F: FnMut(&'a StyledNode, &[&'a StyledNode]) -> bool,
{
  let mut out = Vec::new();
  walk_matches(node, ancestors, predicate, &mut out);
  out
}

fn walk_matches<'a, F>(
  node: &'a StyledNode,
  ancestors: &mut Vec<&'a StyledNode>,
  predicate: &mut F,
  out: &mut Vec<(Vec<&'a StyledNode>, &'a StyledNode)>,
) where
  F: FnMut(&'a StyledNode, &[&'a StyledNode]) -> bool,
{
  if predicate(node, ancestors) {
    out.push((ancestors.clone(), node));
  }

  ancestors.push(node);
  for child in &node.children {
    walk_matches(child, ancestors, predicate, out);
  }
  ancestors.pop();
}

fn node_matches_selector(
  node: &StyledNode,
  ancestors: &[&StyledNode],
  selectors: &SelectorList<FastRenderSelectorImpl>,
  caches: &mut SelectorCaches,
) -> bool {
  if !node.node.is_element() {
    return false;
  }

  let dom_ancestors: Vec<&DomNode> = ancestors.iter().map(|a| &a.node).collect();
  let element_ref = ElementRef::with_ancestors(&node.node, &dom_ancestors);
  let mut context = MatchingContext::new(
    MatchingMode::Normal,
    None,
    caches,
    QuirksMode::NoQuirks,
    NeedsSelectorFlags::No,
    MatchingForInvalidation::No,
  );
  selectors
    .slice()
    .iter()
    .any(|sel| matches_selector(sel, 0, None, &element_ref, &mut context))
}

fn collect_styled_boxes<'a>(node: &'a BoxNode, out: &mut HashMap<usize, Vec<&'a BoxNode>>) {
  if let Some(id) = node.styled_node_id {
    out.entry(id).or_default().push(node);
  }
  for child in &node.children {
    collect_styled_boxes(child, out);
  }
}

fn collect_fragments_for_box<'a>(
  fragment: &'a FragmentNode,
  out: &mut HashMap<usize, Vec<&'a FragmentNode>>,
) {
  if let Some(box_id) = fragment_box_id(fragment) {
    out.entry(box_id).or_default().push(fragment);
  }
  for child in &fragment.children {
    collect_fragments_for_box(child, out);
  }
}

fn fragment_box_id(fragment: &FragmentNode) -> Option<usize> {
  match &fragment.content {
    FragmentContent::Block { box_id }
    | FragmentContent::Inline { box_id, .. }
    | FragmentContent::Text { box_id, .. }
    | FragmentContent::Replaced { box_id, .. } => *box_id,
    FragmentContent::Line { .. } => None,
  }
}

fn scroll_containers_by_box(
  metadata: &ScrollMetadata,
) -> HashMap<Option<usize>, &ScrollSnapContainer> {
  metadata
    .containers
    .iter()
    .map(|container| (container.box_id, container))
    .collect()
}

fn build_snapshot(
  node: &StyledNode,
  ancestors: &[&StyledNode],
  styled_boxes: &HashMap<usize, Vec<&BoxNode>>,
  fragments: &HashMap<usize, Vec<&FragmentNode>>,
  scroll: &HashMap<Option<usize>, &ScrollSnapContainer>,
) -> InspectionSnapshot {
  let boxes = styled_boxes.get(&node.node_id).cloned().unwrap_or_default();

  let box_snapshots: Vec<BoxSnapshot> = boxes.iter().map(|b| box_snapshot(b)).collect();

  let mut fragment_snaps: Vec<FragmentSnapshot> = Vec::new();
  for b in &boxes {
    if let Some(fragments_for_box) = fragments.get(&b.id) {
      fragment_snaps.extend(
        fragments_for_box
          .iter()
          .map(|f| fragment_snapshot(f, scroll.get(&Some(b.id)).copied())),
      );
    }
  }

  InspectionSnapshot {
    node: node_snapshot(node, ancestors),
    style: style_snapshot(&node.styles),
    boxes: box_snapshots,
    fragments: fragment_snaps,
  }
}

fn node_snapshot(node: &StyledNode, ancestors: &[&StyledNode]) -> NodeSnapshot {
  let dom = &node.node;
  let attributes = match &dom.node_type {
    DomNodeType::Element { attributes, .. } | DomNodeType::Slot { attributes, .. } => {
      attributes.clone()
    }
    _ => Vec::new(),
  };
  let classes = dom
    .get_attribute_ref("class")
    .map(|value| {
      value
        .split_whitespace()
        .map(|c| c.to_string())
        .collect::<Vec<_>>()
    })
    .unwrap_or_default();
  let ancestry = ancestors
    .iter()
    .map(|ancestor| dom_selector(&ancestor.node))
    .collect();

  NodeSnapshot {
    node_id: node.node_id,
    tag_name: dom.tag_name().map(str::to_string),
    id: dom.get_attribute_ref("id").map(str::to_string),
    classes,
    attributes,
    ancestry,
  }
}

fn dom_selector(node: &DomNode) -> String {
  let mut selector = match &node.node_type {
    DomNodeType::Document => "document".to_string(),
    DomNodeType::ShadowRoot { .. } => "shadow-root".to_string(),
    DomNodeType::Text { .. } => "#text".to_string(),
    _ => node.tag_name().unwrap_or("#element").to_string(),
  };
  if let Some(id) = node.get_attribute_ref("id") {
    selector.push('#');
    selector.push_str(id);
  }
  if let Some(class_attr) = node.get_attribute_ref("class") {
    for class in class_attr.split_whitespace() {
      selector.push('.');
      selector.push_str(class);
    }
  }
  selector
}

fn style_snapshot(style: &ComputedStyle) -> ComputedStyleSnapshot {
  ComputedStyleSnapshot {
    display: display_name(style.display),
    position: position_name(style.position),
    float: float_name(style.float),
    overflow_x: overflow_name(style.overflow_x),
    overflow_y: overflow_name(style.overflow_y),
    width: length_opt_px(style.width),
    height: length_opt_px(style.height),
    min_width: length_opt_px(style.min_width),
    min_height: length_opt_px(style.min_height),
    margin: EdgeSnapshot {
      top: length_opt_px(style.margin_top),
      right: length_opt_px(style.margin_right),
      bottom: length_opt_px(style.margin_bottom),
      left: length_opt_px(style.margin_left),
    },
    padding: EdgeSnapshot {
      top: style.padding_top.to_px(),
      right: style.padding_right.to_px(),
      bottom: style.padding_bottom.to_px(),
      left: style.padding_left.to_px(),
    },
    border: EdgeSnapshot {
      top: style.border_top_width.to_px(),
      right: style.border_right_width.to_px(),
      bottom: style.border_bottom_width.to_px(),
      left: style.border_left_width.to_px(),
    },
    color: color_snapshot(style.color),
    background_color: color_snapshot(style.background_color),
    font_size: style.font_size,
    line_height: resolve_line_height(style.line_height, style.font_size),
  }
}

fn box_snapshot(node: &BoxNode) -> BoxSnapshot {
  BoxSnapshot {
    box_id: node.id,
    styled_node_id: node.styled_node_id,
    debug_selector: node.debug_info.as_ref().map(|d| d.to_selector()),
    box_type: match &node.box_type {
      BoxType::Block(_) => "block".to_string(),
      BoxType::Inline(_) => "inline".to_string(),
      BoxType::Text(_) => "text".to_string(),
      BoxType::Marker(_) => "marker".to_string(),
      BoxType::Replaced(_) => "replaced".to_string(),
      BoxType::Anonymous(_) => "anonymous".to_string(),
    },
    display: display_name(node.style.display),
    formatting_context: node.formatting_context().map(formatting_context_name),
  }
}

fn fragment_snapshot(
  fragment: &FragmentNode,
  scroll: Option<&ScrollSnapContainer>,
) -> FragmentSnapshot {
  let (kind, box_id, text) = match &fragment.content {
    FragmentContent::Block { box_id } => ("block".to_string(), *box_id, None),
    FragmentContent::Inline { box_id, .. } => ("inline".to_string(), *box_id, None),
    FragmentContent::Text { box_id, text, .. } => (
      "text".to_string(),
      *box_id,
      Some(text.chars().take(80).collect()),
    ),
    FragmentContent::Line { .. } => ("line".to_string(), None, None),
    FragmentContent::Replaced { box_id, .. } => ("replaced".to_string(), *box_id, None),
  };

  FragmentSnapshot {
    box_id,
    kind,
    bounds: rect_snapshot(fragment.bounds),
    baseline: fragment.baseline,
    fragment_index: fragment.fragment_index,
    fragment_count: fragment.fragment_count,
    fragmentainer: fragment.fragmentainer_index,
    scroll_overflow: rect_snapshot(fragment.scroll_overflow),
    scroll_container: scroll.map(scroll_snapshot),
    text,
  }
}

fn scroll_snapshot(container: &ScrollSnapContainer) -> ScrollContainerSnapshot {
  ScrollContainerSnapshot {
    viewport: SizeSnapshot {
      width: container.viewport.width,
      height: container.viewport.height,
    },
    scroll_bounds: rect_snapshot(container.scroll_bounds),
    snap_x: container.snap_x,
    snap_y: container.snap_y,
    strictness: format!("{:?}", container.strictness).to_ascii_lowercase(),
    behavior: format!("{:?}", container.behavior).to_ascii_lowercase(),
    uses_viewport_scroll: container.uses_viewport_scroll,
  }
}

fn rect_snapshot(rect: Rect) -> RectSnapshot {
  RectSnapshot {
    x: rect.x(),
    y: rect.y(),
    width: rect.width(),
    height: rect.height(),
  }
}

fn resolve_line_height(line_height: LineHeight, font_size: f32) -> f32 {
  match line_height {
    LineHeight::Normal => font_size * 1.2,
    LineHeight::Number(n) => font_size * n,
    LineHeight::Length(len) => len.to_px(),
    LineHeight::Percentage(pct) => font_size * (pct / 100.0),
  }
}

fn length_opt_px(len: Option<Length>) -> Option<f32> {
  len.map(|l| l.to_px())
}

fn color_snapshot(color: Rgba) -> ColorSnapshot {
  ColorSnapshot {
    r: color.r,
    g: color.g,
    b: color.b,
    a: color.a,
  }
}

fn display_name(display: Display) -> String {
  format!("{display:?}").to_ascii_lowercase()
}

fn position_name(position: Position) -> String {
  format!("{position:?}").to_ascii_lowercase()
}

fn float_name(float: Float) -> String {
  format!("{float:?}").to_ascii_lowercase()
}

fn overflow_name(overflow: Overflow) -> String {
  format!("{overflow:?}").to_ascii_lowercase()
}

fn formatting_context_name(fc: FormattingContextType) -> String {
  format!("{fc:?}").to_ascii_lowercase()
}
