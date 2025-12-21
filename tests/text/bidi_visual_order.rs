use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::types::Direction;
use fastrender::style::types::UnicodeBidi;
use fastrender::tree::box_tree::BoxNode;
use fastrender::FormattingContext;
use fastrender::FormattingContextType;
use fastrender::LayoutConstraints;
use std::sync::Arc;

fn default_style() -> Arc<fastrender::ComputedStyle> {
  Arc::new(fastrender::ComputedStyle::default())
}

fn collect_text_with_x(
  fragment: &fastrender::tree::fragment_tree::FragmentNode,
) -> Vec<(String, f32)> {
  let mut out = Vec::new();
  let mut stack = vec![fragment];
  while let Some(node) = stack.pop() {
    if let fastrender::tree::fragment_tree::FragmentContent::Text { text, .. } = &node.content {
      out.push((text.clone(), node.bounds.x()));
    }
    for child in &node.children {
      stack.push(child);
    }
  }
  out
}

#[test]
fn bidi_visual_order_handles_mixed_arabic_ltr() {
  // Sequence: LTR "A ", RTL arabic digits, LTR trailing.
  let mut rtl_style = fastrender::ComputedStyle::default();
  rtl_style.direction = Direction::Rtl;
  rtl_style.unicode_bidi = UnicodeBidi::Embed;
  let rtl_style = Arc::new(rtl_style);

  let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![
    BoxNode::new_text(default_style(), "A ".to_string()),
    BoxNode::new_inline(rtl_style.clone(), vec![BoxNode::new_text(
      rtl_style.clone(),
      "123".to_string(),
    )]),
    BoxNode::new_text(default_style(), " B".to_string()),
  ]);

  let ifc = InlineFormattingContext::new();
  let constraints = LayoutConstraints::definite_width(400.0);
  let fragment = ifc.layout(&root, &constraints).expect("layout");

  let mut texts = collect_text_with_x(&fragment);
  texts.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
  let labels: String = texts.into_iter().map(|(t, _)| t).collect();
  // Visual order should keep the LTR run first, then the embedded digits together, then trailing B.
  assert_eq!(labels, "A 123B");
}

#[test]
fn bidi_isolate_keeps_neutral_brackets_outside() {
  // In LTR text, an RTL isolate should not flip surrounding neutral parentheses.
  let mut rtl_isolate = fastrender::ComputedStyle::default();
  rtl_isolate.direction = Direction::Rtl;
  rtl_isolate.unicode_bidi = UnicodeBidi::Isolate;
  let rtl_isolate = Arc::new(rtl_isolate);

  let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![
    BoxNode::new_text(default_style(), "(".to_string()),
    BoxNode::new_inline(rtl_isolate.clone(), vec![BoxNode::new_text(
      rtl_isolate.clone(),
      "אב".to_string(),
    )]),
    BoxNode::new_text(default_style(), ")".to_string()),
  ]);

  let ifc = InlineFormattingContext::new();
  let constraints = LayoutConstraints::definite_width(400.0);
  let fragment = ifc.layout(&root, &constraints).expect("layout");

  let mut texts = collect_text_with_x(&fragment);
  texts.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));

  assert!(texts.len() >= 3, "expected at least three text fragments");
  assert_eq!(texts.first().unwrap().0, "(");
  assert_eq!(texts.last().unwrap().0, ")");
  // The isolate should stay grouped between the parentheses; the glyph order inside the RTL run can be visual (e.g. "בא").
  let middle: String = texts[1..texts.len() - 1]
    .iter()
    .map(|(t, _)| t.as_str())
    .collect();
  assert!(middle.contains('א') && middle.contains('ב'));
}

#[test]
fn bidi_isolate_override_keeps_neutral_brackets_outside() {
  // Isolate override should also keep surrounding parentheses out of the RTL run.
  let mut rtl_iso_override = fastrender::ComputedStyle::default();
  rtl_iso_override.direction = Direction::Rtl;
  rtl_iso_override.unicode_bidi = UnicodeBidi::IsolateOverride;
  let rtl_iso_override = Arc::new(rtl_iso_override);

  let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![
    BoxNode::new_text(default_style(), "(".to_string()),
    BoxNode::new_inline(rtl_iso_override.clone(), vec![BoxNode::new_text(
      rtl_iso_override.clone(),
      "אב".to_string(),
    )]),
    BoxNode::new_text(default_style(), ")".to_string()),
  ]);

  let ifc = InlineFormattingContext::new();
  let constraints = LayoutConstraints::definite_width(400.0);
  let fragment = ifc.layout(&root, &constraints).expect("layout");

  let mut texts = collect_text_with_x(&fragment);
  texts.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));

  assert!(texts.len() >= 3, "expected at least three text fragments");
  assert_eq!(texts.first().unwrap().0, "(");
  assert_eq!(texts.last().unwrap().0, ")");
  let middle: String = texts[1..texts.len() - 1]
    .iter()
    .map(|(t, _)| t.as_str())
    .collect();
  assert!(middle.contains('א') && middle.contains('ב'));
}
