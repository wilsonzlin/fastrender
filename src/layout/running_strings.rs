use crate::tree::box_tree::{BoxNode, BoxTree, BoxType, MarkerContent};
use crate::style::content::{StringSetAssignment, StringSetValue};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// A single string-set assignment event, positioned on the block axis.
#[derive(Debug, Clone)]
pub struct StringSetEvent {
  /// Absolute block-axis position of the assigning fragment.
  pub abs_y: f32,
  /// Name of the string being assigned.
  pub name: String,
  /// Resolved value for the assignment.
  pub value: String,
}

/// Collect all string-set assignments from a laid-out fragment tree.
///
/// The traversal uses the original (unclipped) fragment tree and records the absolute
/// block-axis position of each fragment carrying a `string-set` declaration.
pub fn collect_string_set_events(root: &FragmentNode, box_tree: &BoxTree) -> Vec<StringSetEvent> {
  let mut styles_by_id = HashMap::new();
  collect_box_styles(&box_tree.root, &mut styles_by_id);

  let mut parent_by_id = HashMap::new();
  collect_box_parents(&box_tree.root, None, &mut parent_by_id);

  let mut box_text = HashMap::new();
  collect_box_text(&box_tree.root, &mut box_text);

  let mut events = Vec::new();
  let mut seen_boxes = HashSet::new();
  collect_string_set_events_inner(
    root,
    0.0,
    &mut events,
    &styles_by_id,
    &parent_by_id,
    &box_text,
    &mut seen_boxes,
  );
  events
}

fn collect_box_styles(node: &BoxNode, out: &mut HashMap<usize, Arc<crate::style::ComputedStyle>>) {
  out.insert(node.id, node.style.clone());
  for child in &node.children {
    collect_box_styles(child, out);
  }
}

fn collect_box_parents(node: &BoxNode, parent: Option<usize>, out: &mut HashMap<usize, usize>) {
  if let Some(parent_id) = parent {
    if node.id != 0 && parent_id != 0 {
      out.insert(node.id, parent_id);
    }
  }
  let next_parent = if node.id != 0 { Some(node.id) } else { parent };
  for child in &node.children {
    collect_box_parents(child, next_parent, out);
  }
}

fn collect_box_text(node: &BoxNode, out: &mut HashMap<usize, String>) -> String {
  let mut text = String::new();
  match &node.box_type {
    BoxType::Text(t) => text.push_str(&t.text),
    BoxType::Marker(marker) => {
      if let MarkerContent::Text(t) = &marker.content {
        text.push_str(t);
      }
    }
    _ => {}
  }

  for child in &node.children {
    text.push_str(&collect_box_text(child, out));
  }

  out.insert(node.id, text.clone());
  text
}

fn collect_string_set_events_inner(
  node: &FragmentNode,
  abs_start: f32,
  out: &mut Vec<StringSetEvent>,
  styles_by_id: &HashMap<usize, Arc<crate::style::ComputedStyle>>,
  parent_by_id: &HashMap<usize, usize>,
  box_text: &HashMap<usize, String>,
  seen_boxes: &mut HashSet<usize>,
) {
  let start = abs_start + node.bounds.y();

  let mut assignments: Option<&[StringSetAssignment]> = None;
  let fragment_box_id: Option<usize> = fragment_box_id(node);
  let mut assignments_box_id: Option<usize> = None;
  if let Some(mut probe) = fragment_box_id {
    loop {
      if let Some(style) = styles_by_id.get(&probe) {
        if !style.string_set.is_empty() {
          assignments = Some(style.string_set.as_slice());
          assignments_box_id = Some(probe);
          break;
        }
      }
      let Some(parent) = parent_by_id.get(&probe).copied() else {
        break;
      };
      probe = parent;
    }
  }

  if assignments.is_none() {
    if let Some(style) = node.style.as_deref() {
      if !style.string_set.is_empty() {
        assignments = Some(style.string_set.as_slice());
        assignments_box_id = fragment_box_id;
      }
    }
  }

  if let Some(assignments) = assignments {
    let should_emit = assignments_box_id.map(|box_id| seen_boxes.insert(box_id)).unwrap_or(true);

    if should_emit {
      for StringSetAssignment { name, value } in assignments {
        let resolved = resolve_string_set_value(node, value, assignments_box_id, box_text);
        out.push(StringSetEvent {
          abs_y: start,
          name: name.clone(),
          value: resolved,
        });
      }
    }
  }

  for child in &node.children {
    collect_string_set_events_inner(
      child,
      start,
      out,
      styles_by_id,
      parent_by_id,
      box_text,
      seen_boxes,
    );
  }
}

fn fragment_box_id(node: &FragmentNode) -> Option<usize> {
  match &node.content {
    FragmentContent::Block { box_id } => *box_id,
    FragmentContent::Inline { box_id, .. } => *box_id,
    FragmentContent::Text { box_id, .. } => *box_id,
    FragmentContent::Replaced { box_id, .. } => *box_id,
    _ => None,
  }
}

fn resolve_string_set_value(
  node: &FragmentNode,
  value: &StringSetValue,
  box_id: Option<usize>,
  box_text: &HashMap<usize, String>,
) -> String {
  match value {
    StringSetValue::Content => {
      if let Some(box_id) = box_id {
        if let Some(text) = box_text.get(&box_id) {
          return text.clone();
        }
      }
      let mut fallback = String::new();
      collect_text(node, &mut fallback);
      fallback
    }
    StringSetValue::Literal(s) => s.clone(),
  }
}

fn collect_text(node: &FragmentNode, out: &mut String) {
  if let FragmentContent::Text { text, .. } = &node.content {
    out.push_str(text);
  }
  for child in &node.children {
    collect_text(child, out);
  }
}
