use crate::style::content::{StringSetAssignment, StringSetValue};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};

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
pub fn collect_string_set_events(root: &FragmentNode) -> Vec<StringSetEvent> {
  let mut events = Vec::new();
  collect_string_set_events_inner(root, 0.0, &mut events);
  events
}

fn collect_string_set_events_inner(
  node: &FragmentNode,
  abs_start: f32,
  out: &mut Vec<StringSetEvent>,
) {
  let start = abs_start + node.bounds.y();

  if let Some(style) = node.style.as_ref() {
    for StringSetAssignment { name, value } in &style.string_set {
      let resolved = resolve_string_set_value(node, value);
      out.push(StringSetEvent {
        abs_y: start,
        name: name.clone(),
        value: resolved,
      });
    }
  }

  for child in &node.children {
    collect_string_set_events_inner(child, start, out);
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
