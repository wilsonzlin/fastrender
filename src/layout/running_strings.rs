use crate::layout::axis::FragmentAxes;
use crate::style::content::{StringSetAssignment, StringSetValue};
use crate::tree::box_tree::{BoxNode, BoxTree, BoxType, MarkerContent};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// A single string-set assignment event, positioned on the block axis.
#[derive(Debug, Clone)]
pub struct StringSetEvent {
  /// Absolute block-axis position of the assigning fragment.
  pub abs_block: f32,
  /// Name of the string being assigned.
  pub name: String,
  /// Resolved value for the assignment.
  pub value: String,
}

/// Collect all string-set assignments from a laid-out fragment tree.
///
/// The traversal uses the original (unclipped) fragment tree and records the absolute
/// block-axis position of each fragment carrying a `string-set` declaration.
pub fn collect_string_set_events(
  root: &FragmentNode,
  box_tree: &BoxTree,
  axes: FragmentAxes,
) -> Vec<StringSetEvent> {
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
    axes.block_size(&root.logical_bounds()),
    axes,
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
  for child in node.children.iter() {
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
  for child in node.children.iter() {
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

  for child in node.children.iter() {
    text.push_str(&collect_box_text(child, out));
  }

  out.insert(node.id, text.clone());
  text
}

fn collect_string_set_events_inner(
  node: &FragmentNode,
  abs_start: f32,
  parent_block_size: f32,
  axes: FragmentAxes,
  out: &mut Vec<StringSetEvent>,
  styles_by_id: &HashMap<usize, Arc<crate::style::ComputedStyle>>,
  parent_by_id: &HashMap<usize, usize>,
  box_text: &HashMap<usize, String>,
  seen_boxes: &mut HashSet<usize>,
) {
  let logical_bounds = node.logical_bounds();
  let start = axes.abs_block_start(&logical_bounds, abs_start, parent_block_size);
  let node_block_size = axes.block_size(&logical_bounds);

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
    let should_emit = assignments_box_id
      .map(|box_id| seen_boxes.insert(box_id))
      .unwrap_or(true);

    if should_emit {
      for StringSetAssignment { name, value } in assignments {
        let resolved = resolve_string_set_value(node, value, assignments_box_id, box_text);
        out.push(StringSetEvent {
          abs_block: start,
          name: name.clone(),
          value: resolved,
        });
      }
    }
  }

  for child in node.children() {
    collect_string_set_events_inner(
      child,
      start,
      node_block_size,
      axes,
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
  for child in node.children() {
    collect_text(child, out);
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::Rect;
  use crate::layout::axis::FragmentAxes;
  use crate::style::content::{StringSetAssignment, StringSetValue};
  use crate::style::display::FormattingContextType;
  use crate::style::ComputedStyle;
  use crate::tree::box_tree::{BoxNode, BoxTree};
  use crate::tree::fragment_tree::{FragmentContent, FragmentNode};
  use std::sync::Arc;

  #[test]
  fn collect_string_set_events_traverses_descendants() {
    let mut string_set_style = ComputedStyle::default();
    string_set_style.string_set = vec![StringSetAssignment {
      name: "chapter".into(),
      value: StringSetValue::Content,
    }];

    let text_box = BoxNode::new_text(Arc::new(ComputedStyle::default()), "Box Value".into());
    let box_with_string_set = BoxNode::new_block(
      Arc::new(string_set_style),
      FormattingContextType::Block,
      vec![text_box],
    );
    let root_box = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![box_with_string_set],
    );
    let box_tree = BoxTree::new(root_box);

    let string_set_box = &box_tree.root.children[0];
    let string_set_text_box = &string_set_box.children[0];

    let text_fragment = FragmentNode::new(
      Rect::from_xywh(0.0, 0.0, 10.0, 5.0),
      FragmentContent::Text {
        text: "Box Value".into(),
        box_id: Some(string_set_text_box.id),
        baseline_offset: 0.0,
        shaped: None,
        is_marker: false,
      },
      vec![],
    );
    let child_fragment = FragmentNode::new_block_with_id(
      Rect::from_xywh(0.0, 5.0, 10.0, 10.0),
      string_set_box.id,
      vec![text_fragment],
    );

    let mut inline_style = ComputedStyle::default();
    inline_style.string_set = vec![StringSetAssignment {
      name: "note".into(),
      value: StringSetValue::Content,
    }];
    let inline_text = FragmentNode::new_text(
      Rect::from_xywh(0.0, 0.0, 10.0, 5.0),
      "Inline fallback".into(),
      0.0,
    );
    let mut inline_fragment =
      FragmentNode::new_inline(Rect::from_xywh(0.0, 20.0, 10.0, 5.0), 0, vec![inline_text]);
    inline_fragment.style = Some(Arc::new(inline_style));

    let root_fragment = FragmentNode::new_block_with_id(
      Rect::from_xywh(0.0, 0.0, 10.0, 40.0),
      box_tree.root.id,
      vec![child_fragment, inline_fragment],
    );

    let events = collect_string_set_events(&root_fragment, &box_tree, FragmentAxes::default());

    assert_eq!(events.len(), 2);
    assert_eq!(events[0].name, "chapter");
    assert_eq!(events[0].value, "Box Value");
    assert!((events[0].abs_block - 5.0).abs() < 0.001);

    assert_eq!(events[1].name, "note");
    assert_eq!(events[1].value, "Inline fallback");
    assert!((events[1].abs_block - 20.0).abs() < 0.001);
  }
}
