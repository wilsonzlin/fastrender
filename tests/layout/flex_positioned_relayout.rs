use fastrender::geometry::Size;
use fastrender::layout::constraints::{AvailableSpace, LayoutConstraints};
use fastrender::layout::contexts::flex::FlexFormattingContext;
use fastrender::layout::formatting_context::FormattingContext;
use fastrender::style::display::{Display, FormattingContextType};
use fastrender::style::position::Position;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::BoxNode;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode};
use std::sync::Arc;

fn positioned_child(position: Position, left: f32, right: f32, top: f32, text: &str) -> BoxNode {
  let mut style = ComputedStyle::default();
  style.display = Display::Block;
  style.position = position;
  style.left = Some(Length::px(left));
  style.right = Some(Length::px(right));
  style.top = Some(Length::px(top));

  let text_node = BoxNode::new_text(Arc::new(ComputedStyle::default()), text.to_string());
  BoxNode::new_block(
    Arc::new(style),
    FormattingContextType::Block,
    vec![text_node],
  )
}

fn descendant_max_right(fragment: &FragmentNode) -> f32 {
  let origin_x = fragment.bounds.x();
  fragment
    .iter_fragments()
    .map(|f| f.bounds.max_x() - origin_x)
    .fold(0.0, f32::max)
}

fn line_count(fragment: &FragmentNode) -> usize {
  fragment
    .iter_fragments()
    .filter(|f| matches!(f.content, FragmentContent::Line { .. }))
    .count()
}

#[test]
fn flex_positioned_children_relayout_is_stable() {
  let mut container_style = ComputedStyle::default();
  container_style.display = Display::Flex;
  container_style.position = Position::Relative;
  container_style.width = Some(Length::px(220.0));
  container_style.height = Some(Length::px(160.0));

  let abs_child = positioned_child(
    Position::Absolute,
    30.0,
    90.0,
    0.0,
    "Positioned flex item that should wrap once the available width is recomputed with insets.",
  );
  let fixed_child = positioned_child(
    Position::Fixed,
    40.0,
    40.0,
    48.0,
    "Fixed positioned item that shares the flex caches while being remeasured after absolute sizing.",
  );

  let container = BoxNode::new_block(
    Arc::new(container_style),
    FormattingContextType::Flex,
    vec![abs_child, fixed_child],
  );

  let fc = FlexFormattingContext::with_viewport(Size::new(260.0, 200.0));
  let constraints = LayoutConstraints::new(
    AvailableSpace::Definite(220.0),
    AvailableSpace::Definite(160.0),
  );

  let first = fc.layout(&container, &constraints).expect("first layout");
  let second = fc.layout(&container, &constraints).expect("second layout");

  assert_eq!(first.children.len(), 2);
  assert_eq!(second.children.len(), 2);

  let expected_abs_width = 100.0;
  let expected_fixed_width = 180.0;

  for (fragment, expected_width) in [
    (&first.children[0], expected_abs_width),
    (&first.children[1], expected_fixed_width),
  ] {
    let max_right = descendant_max_right(fragment);
    assert!(
      (fragment.bounds.width() - expected_width).abs() < 0.5,
      "positioned child should size against insets (expected {expected_width}, got {})",
      fragment.bounds.width()
    );
    assert!(
      max_right <= fragment.bounds.width() + 0.5,
      "descendants should be laid out for the used width (max_right={max_right}, width={})",
      fragment.bounds.width()
    );
    assert!(
      line_count(fragment) > 1,
      "positioned children should wrap when width shrinks (lines={})",
      line_count(fragment)
    );
  }

  for (first_child, second_child) in first.children.iter().zip(second.children.iter()) {
    assert!(
      (first_child.bounds.x() - second_child.bounds.x()).abs() < 0.1
        && (first_child.bounds.y() - second_child.bounds.y()).abs() < 0.1,
      "positions should be stable across relayouts"
    );
    assert!(
      (first_child.bounds.width() - second_child.bounds.width()).abs() < 0.1
        && (first_child.bounds.height() - second_child.bounds.height()).abs() < 0.1,
      "sizes should be stable across relayouts"
    );
    assert_eq!(
      line_count(first_child),
      line_count(second_child),
      "line breaking should stay consistent when caches are reused"
    );
  }
}
