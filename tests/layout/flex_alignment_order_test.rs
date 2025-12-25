use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::flex::FlexFormattingContext;
use fastrender::style::display::Display;
use fastrender::style::position::Position;
use fastrender::style::types::AlignItems;
use fastrender::style::values::Length;
use fastrender::tree::fragment_tree::FragmentContent;
use fastrender::BoxNode;
use fastrender::ComputedStyle;
use fastrender::FormattingContext;
use fastrender::FormattingContextType;
use std::sync::Arc;

#[test]
fn align_self_respects_in_flow_child_order() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Flex;
  parent_style.width = Some(Length::px(200.0));
  parent_style.height = Some(Length::px(40.0));
  parent_style.align_items = AlignItems::FlexStart;

  let mut abs_style = ComputedStyle::default();
  abs_style.display = Display::Block;
  abs_style.position = Position::Absolute;
  abs_style.width = Some(Length::px(10.0));
  abs_style.height = Some(Length::px(10.0));
  let mut abs_child = BoxNode::new_block(Arc::new(abs_style), FormattingContextType::Block, vec![]);
  abs_child.id = 3;

  let mut end_style = ComputedStyle::default();
  end_style.display = Display::Block;
  end_style.width = Some(Length::px(20.0));
  end_style.height = Some(Length::px(10.0));
  end_style.align_self = Some(AlignItems::FlexEnd);
  end_style.flex_shrink = 0.0;
  let mut end_child = BoxNode::new_block(Arc::new(end_style), FormattingContextType::Block, vec![]);
  end_child.id = 1;

  let mut start_style = ComputedStyle::default();
  start_style.display = Display::Block;
  start_style.width = Some(Length::px(20.0));
  start_style.height = Some(Length::px(10.0));
  start_style.flex_shrink = 0.0;
  let mut start_child =
    BoxNode::new_block(Arc::new(start_style), FormattingContextType::Block, vec![]);
  start_child.id = 2;

  let end_id = end_child.id;
  let start_id = start_child.id;

  let parent = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Flex,
    vec![abs_child, end_child, start_child],
  );

  let fc = FlexFormattingContext::new();
  let fragment = fc
    .layout(&parent, &LayoutConstraints::definite(200.0, 40.0))
    .expect("layout succeeds");

  let mut end_y = None;
  let mut start_y = None;
  let mut debug_children = Vec::new();
  for child in &fragment.children {
    let id = match &child.content {
      FragmentContent::Block { box_id }
      | FragmentContent::Inline { box_id, .. }
      | FragmentContent::Replaced { box_id, .. }
      | FragmentContent::Text { box_id, .. } => *box_id,
      FragmentContent::Line { .. } | FragmentContent::RunningAnchor { .. } => None,
    };
    debug_children.push((
      id,
      child.bounds.x(),
      child.bounds.y(),
      child.bounds.width(),
      child.bounds.height(),
    ));
    match id {
      Some(id) if id == end_id => end_y = Some(child.bounds.y()),
      Some(id) if id == start_id => start_y = Some(child.bounds.y()),
      _ => {}
    }
  }

  let end_y = end_y.unwrap_or_else(|| panic!("end child present: {:?}", debug_children));
  let start_y = start_y.unwrap_or_else(|| panic!("start child present: {:?}", debug_children));

  assert!(
    (end_y - 30.0).abs() < 1e-3,
    "end child should align to flex-end"
  );
  assert!(
    (start_y - 0.0).abs() < 1e-3,
    "start child should remain at the cross-axis start"
  );
}
