use crate::geometry::Point;
use crate::layout::axis::FragmentAxes;
use crate::style::content::{RunningElementSelect, RunningElementValues};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};
use std::collections::HashMap;
use std::sync::Arc;

const EPSILON: f32 = 0.01;

/// A running element occurrence extracted from the laid-out fragment tree.
#[derive(Debug, Clone)]
pub struct RunningElementEvent {
  /// Absolute block-axis position of the running element.
  pub abs_block: f32,
  /// Running name from `position: running(<name>)`.
  pub name: String,
  /// Snapshot of the laid-out element subtree.
  pub snapshot: FragmentNode,
}

/// Global state for running elements across pages.
#[derive(Debug, Default, Clone)]
pub struct RunningElementState {
  /// First occurrence in the document for each name.
  pub first: HashMap<String, FragmentNode>,
  /// Last occurrence seen so far (up to the start of the current page).
  pub last: HashMap<String, FragmentNode>,
}

/// Collect all running element occurrences from the laid-out fragment tree.
pub fn collect_running_element_events(
  root: &FragmentNode,
  axes: FragmentAxes,
) -> Vec<RunningElementEvent> {
  let mut events = Vec::new();
  collect_running_element_occurrences(
    root,
    Point::ZERO,
    0.0,
    axes.block_size(&root.logical_bounds()),
    axes,
    &mut events,
  );
  events.sort_by(|a, b| {
    a.abs_block
      .partial_cmp(&b.abs_block)
      .unwrap_or(std::cmp::Ordering::Equal)
  });
  events
}

/// Compute running element values for a page range.
///
/// Events are consumed in-order as pages advance to avoid re-scanning the fragment tree.
pub fn running_elements_for_page(
  events: &[RunningElementEvent],
  idx: &mut usize,
  state: &mut RunningElementState,
  start: f32,
  end: f32,
) -> HashMap<String, RunningElementValues> {
  let boundary = start - EPSILON;
  while *idx < events.len() && events[*idx].abs_block < boundary {
    let event = &events[*idx];
    state
      .first
      .entry(event.name.clone())
      .or_insert_with(|| event.snapshot.clone());
    state
      .last
      .insert(event.name.clone(), event.snapshot.clone());
    *idx += 1;
  }

  let mut values: HashMap<String, RunningElementValues> = HashMap::new();
  for (name, last) in state.last.iter() {
    values.insert(
      name.clone(),
      RunningElementValues {
        start: Some(last.clone()),
        first: None,
        last: None,
      },
    );
  }

  while *idx < events.len() && events[*idx].abs_block < end {
    let event = &events[*idx];
    let entry = values
      .entry(event.name.clone())
      .or_insert_with(|| RunningElementValues {
        start: state.last.get(&event.name).cloned(),
        first: None,
        last: None,
      });
    if entry.first.is_none() {
      entry.first = Some(event.snapshot.clone());
    }
    entry.last = Some(event.snapshot.clone());
    state
      .first
      .entry(event.name.clone())
      .or_insert_with(|| event.snapshot.clone());
    state
      .last
      .insert(event.name.clone(), event.snapshot.clone());
    *idx += 1;
  }

  values
}

/// Select a running element snapshot for the current page.
pub fn select_running_element(
  ident: &str,
  select: RunningElementSelect,
  page_values: &HashMap<String, RunningElementValues>,
  state: &RunningElementState,
) -> Option<FragmentNode> {
  let page = page_values.get(ident);
  match select {
    RunningElementSelect::First => state
      .first
      .get(ident)
      .cloned()
      .or_else(|| page.and_then(|v| v.first.clone().or_else(|| v.start.clone()))),
    RunningElementSelect::Start => page.and_then(|v| v.first.clone().or_else(|| v.start.clone())),
    RunningElementSelect::Last => page.and_then(|v| {
      v.last
        .clone()
        .or_else(|| v.first.clone())
        .or_else(|| v.start.clone())
    }),
  }
}

fn collect_running_element_occurrences(
  node: &FragmentNode,
  origin: Point,
  abs_block_start: f32,
  parent_block_size: f32,
  axes: FragmentAxes,
  out: &mut Vec<RunningElementEvent>,
) {
  let logical_bounds = node.logical_bounds();
  let abs_origin = Point::new(origin.x + logical_bounds.x(), origin.y + logical_bounds.y());
  let node_abs_block = axes.abs_block_start(&logical_bounds, abs_block_start, parent_block_size);
  let node_block_size = axes.block_size(&logical_bounds);

  match &node.content {
    FragmentContent::RunningAnchor { name, snapshot } => {
      let mut cleaned = (**snapshot).clone();
      clean_running_snapshot(&mut cleaned);
      out.push(RunningElementEvent {
        abs_block: node_abs_block,
        name: name.to_string(),
        snapshot: cleaned,
      });
    }
    _ if node.content.is_block() || node.content.is_inline() || node.content.is_replaced() => {
      if let Some(name) = node
        .style
        .as_deref()
        .and_then(|style| style.running_position.as_ref())
      {
        let mut clone = node.clone();
        clean_running_snapshot(&mut clone);
        out.push(RunningElementEvent {
          abs_block: node_abs_block,
          name: name.clone(),
          snapshot: clone,
        });
      }
    }
    _ => {}
  }

  for child in node.children() {
    collect_running_element_occurrences(
      child,
      abs_origin,
      node_abs_block,
      node_block_size,
      axes,
      out,
    );
  }
}

fn clean_running_snapshot(node: &mut FragmentNode) {
  strip_running_anchor_fragments(node);
  clear_running_position(node);
  let logical = node.logical_bounds();
  let offset = Point::new(-logical.x(), -logical.y());
  translate_fragment(node, offset.x, offset.y);
}

fn strip_running_anchor_fragments(node: &mut FragmentNode) {
  let children = node.children_mut();
  let mut kept: Vec<FragmentNode> = Vec::with_capacity(children.len());
  for mut child in children.drain(..) {
    if matches!(child.content, FragmentContent::RunningAnchor { .. }) {
      continue;
    }
    strip_running_anchor_fragments(&mut child);
    kept.push(child);
  }
  *children = kept;
}

fn clear_running_position(node: &mut FragmentNode) {
  if let Some(style) = node.style.as_deref() {
    if style.running_position.is_some() {
      let mut owned = style.clone();
      owned.running_position = None;
      node.style = Some(Arc::new(owned));
    }
  }
  for child in node.children_mut().iter_mut() {
    clear_running_position(child);
  }
}

fn translate_fragment(node: &mut FragmentNode, dx: f32, dy: f32) {
  node.bounds = crate::geometry::Rect::from_xywh(
    node.bounds.x() + dx,
    node.bounds.y() + dy,
    node.bounds.width(),
    node.bounds.height(),
  );
  if let Some(logical) = node.logical_override {
    node.logical_override = Some(crate::geometry::Rect::from_xywh(
      logical.x() + dx,
      logical.y() + dy,
      logical.width(),
      logical.height(),
    ));
  }
  for child in node.children_mut().iter_mut() {
    translate_fragment(child, dx, dy);
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::Rect;
  use crate::style::ComputedStyle;

  #[test]
  fn clean_running_snapshot_strips_and_normalizes() {
    let mut root_style = ComputedStyle::default();
    root_style.running_position = Some("root".into());
    let root_style = Arc::new(root_style);

    let mut child_style = ComputedStyle::default();
    child_style.running_position = Some("child".into());
    let child_style = Arc::new(child_style);

    let anchor_snapshot = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 5.0, 5.0), Vec::new());
    let anchor = FragmentNode::new_running_anchor(
      Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
      "anchor".to_string(),
      anchor_snapshot,
    );
    let child = FragmentNode::new_block_styled(
      Rect::from_xywh(4.0, 6.0, 10.0, 10.0),
      Vec::new(),
      child_style,
    );

    let mut root = FragmentNode::new_block_styled(
      Rect::from_xywh(10.0, 20.0, 30.0, 30.0),
      vec![anchor, child],
      root_style,
    );

    clean_running_snapshot(&mut root);

    assert!(!root
      .iter_fragments()
      .any(|frag| matches!(frag.content, FragmentContent::RunningAnchor { .. })));

    assert!(root.iter_fragments().all(|frag| {
      frag
        .style
        .as_deref()
        .map_or(true, |style| style.running_position.is_none())
    }));

    let logical = root.logical_bounds();
    assert!(logical.x().abs() < EPSILON);
    assert!(logical.y().abs() < EPSILON);
  }
}
