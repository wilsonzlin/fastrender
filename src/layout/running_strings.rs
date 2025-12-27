use crate::tree::box_tree::{BoxNode, BoxTree};
use crate::style::content::{StringSetAssignment, StringSetValue};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};
use std::collections::HashMap;
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

  let mut events = Vec::new();
  collect_string_set_events_inner(root, 0.0, &mut events, &styles_by_id);
  events
}

fn collect_box_styles(node: &BoxNode, out: &mut HashMap<usize, Arc<crate::style::ComputedStyle>>) {
  out.insert(node.id, node.style.clone());
  for child in &node.children {
    collect_box_styles(child, out);
  }
}

fn collect_string_set_events_inner(
  node: &FragmentNode,
  abs_start: f32,
  out: &mut Vec<StringSetEvent>,
  styles_by_id: &HashMap<usize, Arc<crate::style::ComputedStyle>>,
) {
  let start = abs_start + node.bounds.y();

  let mut assignments: Option<&[StringSetAssignment]> = None;
  if let Some(style) = node.style.as_deref() {
    if !style.string_set.is_empty() {
      assignments = Some(style.string_set.as_slice());
    }
  }

  if assignments.is_none() {
    if let Some(box_id) = fragment_box_id(node) {
      if let Some(style) = styles_by_id.get(&box_id) {
        if !style.string_set.is_empty() {
          assignments = Some(style.string_set.as_slice());
        }
      }
    }
  }

  if let Some(assignments) = assignments {
    for StringSetAssignment { name, value } in assignments {
      let resolved = resolve_string_set_value(node, value);
      out.push(StringSetEvent {
        abs_y: start,
        name: name.clone(),
        value: resolved,
      });
    }
  }

  for child in &node.children {
    collect_string_set_events_inner(child, start, out, styles_by_id);
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

fn resolve_string_set_value(node: &FragmentNode, value: &StringSetValue) -> String {
  match value {
    StringSetValue::Content => {
      let mut text = String::new();
      collect_text(node, &mut text);
      text
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
