use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::block::BlockFormattingContext;
use fastrender::style::color::Rgba;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::BorderStyle;
use fastrender::style::types::ColumnSpan;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::BoxNode;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode};
use fastrender::FormattingContext;
use std::sync::Arc;

fn find_fragment<'a>(fragment: &'a FragmentNode, id: usize) -> Option<&'a FragmentNode> {
  if let FragmentContent::Block { box_id: Some(b) } = fragment.content {
    if b == id {
      return Some(fragment);
    }
  }
  for child in &fragment.children {
    if let Some(found) = find_fragment(child, id) {
      return Some(found);
    }
  }
  None
}

fn find_rule_fragment<'a>(fragment: &'a FragmentNode, color: Rgba) -> Option<&'a FragmentNode> {
  if matches!(fragment.content, FragmentContent::Block { box_id: None })
    && fragment
      .style
      .as_ref()
      .is_some_and(|s| s.background_color == color)
  {
    return Some(fragment);
  }
  for child in &fragment.children {
    if let Some(found) = find_rule_fragment(child, color) {
      return Some(found);
    }
  }
  None
}

#[test]
fn multicol_layout_balances_children_and_rules() {
  let mut parent_style = ComputedStyle::default();
  parent_style.width = Some(Length::px(400.0));
  parent_style.column_count = Some(2);
  parent_style.column_gap = Length::px(20.0);
  parent_style.column_rule_style = BorderStyle::Solid;
  parent_style.column_rule_width = Length::px(6.0);
  let rule_color = Rgba::new(255, 0, 0, 1.0);
  parent_style.column_rule_color = Some(rule_color);
  let parent_style = Arc::new(parent_style);

  let child_style = |height: f32| -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.height = Some(Length::px(height));
    Arc::new(style)
  };

  let mut first = BoxNode::new_block(child_style(80.0), FormattingContextType::Block, vec![]);
  first.id = 1;
  let mut second = BoxNode::new_block(child_style(80.0), FormattingContextType::Block, vec![]);
  second.id = 2;

  let mut span_style = ComputedStyle::default();
  span_style.height = Some(Length::px(30.0));
  span_style.column_span = ColumnSpan::All;
  let mut span = BoxNode::new_block(Arc::new(span_style), FormattingContextType::Block, vec![]);
  span.id = 3;

  let mut parent = BoxNode::new_block(
    parent_style,
    FormattingContextType::Block,
    vec![first.clone(), second.clone(), span.clone()],
  );
  parent.id = 10;

  let fc = BlockFormattingContext::new();
  let fragment = fc
    .layout(&parent, &LayoutConstraints::definite_width(400.0))
    .expect("layout");

  let first_frag = find_fragment(&fragment, first.id).expect("first fragment");
  assert!((first_frag.bounds.x() - 0.0).abs() < 0.1);
  assert!((first_frag.bounds.y() - 0.0).abs() < 0.1);

  let second_frag = find_fragment(&fragment, second.id).expect("second fragment");
  assert!((second_frag.bounds.x() - 210.0).abs() < 0.2);
  assert!((second_frag.bounds.y() - 0.0).abs() < 0.1);

  let span_frag = find_fragment(&fragment, span.id).expect("span fragment");
  assert!((span_frag.bounds.x()).abs() < 0.1);
  assert!(span_frag.bounds.width() > 399.0);
  assert!((span_frag.bounds.y() - 80.0).abs() < 0.2);

  let rule_frag = find_rule_fragment(&fragment, rule_color).expect("column rule fragment");
  assert!((rule_frag.bounds.width() - 6.0).abs() < 0.1);
  assert!((rule_frag.bounds.height() - 80.0).abs() < 0.2);
  assert!((rule_frag.bounds.x() - 197.0).abs() < 0.5);
}
