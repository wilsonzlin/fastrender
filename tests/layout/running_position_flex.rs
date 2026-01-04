use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::flex::FlexFormattingContext;
use fastrender::style::display::Display;
use fastrender::style::types::{FlexDirection, JustifyContent};
use fastrender::style::values::Length;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode};
use fastrender::{BoxNode, ComputedStyle, FormattingContext, FormattingContextType};
use std::sync::Arc;

fn find_child_by_id<'a>(fragment: &'a FragmentNode, id: usize) -> Option<&'a FragmentNode> {
  fragment.children.iter().find(|child| {
    matches!(
      child.content,
      FragmentContent::Block { box_id: Some(box_id) }
        | FragmentContent::Inline { box_id: Some(box_id), .. }
        | FragmentContent::Text { box_id: Some(box_id), .. }
        | FragmentContent::Replaced { box_id: Some(box_id), .. }
        if box_id == id
    )
  })
}

fn find_running_anchor(fragment: &FragmentNode) -> Option<&FragmentNode> {
  fragment
    .children
    .iter()
    .find(|child| matches!(child.content, FragmentContent::RunningAnchor { .. }))
}

#[test]
fn running_position_in_flex_respects_order() {
  let mut flex_style = ComputedStyle::default();
  flex_style.display = Display::Flex;
  flex_style.flex_direction = FlexDirection::Column;
  flex_style.justify_content = JustifyContent::FlexEnd;
  flex_style.width = Some(Length::px(100.0));
  flex_style.height = Some(Length::px(100.0));

  let mut first_style = ComputedStyle::default();
  first_style.display = Display::Block;
  first_style.height = Some(Length::px(10.0));
  first_style.order = 2;
  let mut first = BoxNode::new_block(Arc::new(first_style), FormattingContextType::Block, vec![]);
  first.id = 1;

  let mut second_style = ComputedStyle::default();
  second_style.display = Display::Block;
  second_style.height = Some(Length::px(10.0));
  second_style.order = 3;
  let mut second = BoxNode::new_block(Arc::new(second_style), FormattingContextType::Block, vec![]);
  second.id = 2;

  let mut running_style = ComputedStyle::default();
  running_style.display = Display::Block;
  running_style.running_position = Some("banner".into());
  running_style.order = 1;
  let mut running = BoxNode::new_block(
    Arc::new(running_style),
    FormattingContextType::Block,
    vec![],
  );
  running.id = 3;

  // DOM order differs from flex order due to `order`.
  let flex = BoxNode::new_block(
    Arc::new(flex_style),
    FormattingContextType::Flex,
    vec![first.clone(), second, running],
  );

  let fc = FlexFormattingContext::new();
  let fragment = fc
    .layout(&flex, &LayoutConstraints::definite(100.0, 100.0))
    .expect("layout succeeds");

  let first_fragment =
    find_child_by_id(&fragment, first.id).unwrap_or_else(|| panic!("missing first child"));
  let expected_anchor_y = first_fragment.bounds.y();
  assert!(
    expected_anchor_y > 0.0,
    "test setup expects justify-content to offset children"
  );

  let anchor =
    find_running_anchor(&fragment).unwrap_or_else(|| panic!("missing running anchor fragment"));
  assert!(
    (anchor.bounds.y() - expected_anchor_y).abs() < 1e-3,
    "anchor y should match the next in flex order, not DOM sibling"
  );
}

#[test]
fn running_position_in_flex_last_item_anchor_not_origin() {
  let mut flex_style = ComputedStyle::default();
  flex_style.display = Display::Flex;
  flex_style.flex_direction = FlexDirection::Column;
  flex_style.width = Some(Length::px(100.0));
  flex_style.height = Some(Length::px(100.0));

  let mut first_style = ComputedStyle::default();
  first_style.display = Display::Block;
  first_style.height = Some(Length::px(10.0));
  first_style.order = 1;
  let mut first = BoxNode::new_block(Arc::new(first_style), FormattingContextType::Block, vec![]);
  first.id = 10;

  let mut second_style = ComputedStyle::default();
  second_style.display = Display::Block;
  second_style.height = Some(Length::px(20.0));
  second_style.order = 2;
  let mut second = BoxNode::new_block(Arc::new(second_style), FormattingContextType::Block, vec![]);
  second.id = 11;

  let mut running_style = ComputedStyle::default();
  running_style.display = Display::Block;
  running_style.running_position = Some("tail".into());
  running_style.order = 3;
  let mut running = BoxNode::new_block(
    Arc::new(running_style),
    FormattingContextType::Block,
    vec![],
  );
  running.id = 12;

  let flex = BoxNode::new_block(
    Arc::new(flex_style),
    FormattingContextType::Flex,
    vec![first.clone(), second.clone(), running],
  );

  let fc = FlexFormattingContext::new();
  let fragment = fc
    .layout(&flex, &LayoutConstraints::definite(100.0, 100.0))
    .expect("layout succeeds");

  let second_fragment =
    find_child_by_id(&fragment, second.id).unwrap_or_else(|| panic!("missing second child"));
  let expected_end = second_fragment.bounds.y() + second_fragment.bounds.height();
  assert!(expected_end > 0.0, "test setup expects a non-zero flow end");

  let anchor =
    find_running_anchor(&fragment).unwrap_or_else(|| panic!("missing running anchor fragment"));
  assert!(
    (anchor.bounds.y() - expected_end).abs() < 1e-3,
    "anchor y should land at the end-of-flow, not the origin"
  );
}

#[test]
fn running_position_in_flex_column_reverse_is_axis_correct() {
  let mut flex_style = ComputedStyle::default();
  flex_style.display = Display::Flex;
  flex_style.flex_direction = FlexDirection::ColumnReverse;
  flex_style.width = Some(Length::px(100.0));
  flex_style.height = Some(Length::px(30.0));

  let mut first_style = ComputedStyle::default();
  first_style.display = Display::Block;
  first_style.height = Some(Length::px(10.0));
  first_style.order = 1;
  let mut first = BoxNode::new_block(Arc::new(first_style), FormattingContextType::Block, vec![]);
  first.id = 21;

  let mut second_style = ComputedStyle::default();
  second_style.display = Display::Block;
  second_style.height = Some(Length::px(20.0));
  second_style.order = 2;
  let mut second = BoxNode::new_block(Arc::new(second_style), FormattingContextType::Block, vec![]);
  second.id = 22;

  let mut running_style = ComputedStyle::default();
  running_style.display = Display::Block;
  running_style.running_position = Some("rev".into());
  running_style.order = 0;
  let mut running = BoxNode::new_block(
    Arc::new(running_style),
    FormattingContextType::Block,
    vec![],
  );
  running.id = 23;

  let flex = BoxNode::new_block(
    Arc::new(flex_style),
    FormattingContextType::Flex,
    vec![running, first.clone(), second],
  );

  let fc = FlexFormattingContext::new();
  let fragment = fc
    .layout(&flex, &LayoutConstraints::definite(100.0, 30.0))
    .expect("layout succeeds");

  let first_fragment =
    find_child_by_id(&fragment, first.id).unwrap_or_else(|| panic!("missing first child"));
  let expected_anchor_y = first_fragment.bounds.y() + first_fragment.bounds.height();

  let anchor =
    find_running_anchor(&fragment).unwrap_or_else(|| panic!("missing running anchor fragment"));
  assert!(
    (anchor.bounds.y() - expected_anchor_y).abs() < 1e-3,
    "column-reverse anchor should use the main-start edge (block-end) of the next item"
  );
}
