use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::flex::FlexFormattingContext;
use fastrender::layout::contexts::grid::GridFormattingContext;
use fastrender::layout::formatting_context::FormattingContext;
use fastrender::style::display::Display;
use fastrender::style::display::FormattingContextType;
use fastrender::style::position::Position;
use fastrender::style::types::GridTrack;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::BoxNode;
use std::sync::Arc;

#[test]
fn absolute_child_inherits_flex_static_position() {
  let mut container_style = ComputedStyle::default();
  container_style.display = Display::Flex;
  container_style.position = Position::Relative;
  container_style.width = Some(Length::px(200.0));

  let mut first_style = ComputedStyle::default();
  first_style.width = Some(Length::px(50.0));
  first_style.height = Some(Length::px(10.0));

  let mut abs_style = ComputedStyle::default();
  abs_style.position = Position::Absolute;
  abs_style.width = Some(Length::px(20.0));
  abs_style.height = Some(Length::px(10.0));

  let first = BoxNode::new_block(Arc::new(first_style), FormattingContextType::Block, vec![]);
  let abs_child = BoxNode::new_block(Arc::new(abs_style), FormattingContextType::Block, vec![]);
  let container = BoxNode::new_block(
    Arc::new(container_style),
    FormattingContextType::Flex,
    vec![first, abs_child],
  );

  let constraints = LayoutConstraints::definite(200.0, 100.0);
  let fc = FlexFormattingContext::new();
  let fragment = fc.layout(&container, &constraints).expect("flex layout");

  let abs_fragment = fragment
    .children
    .iter()
    .find(|child| {
      matches!(
        child.style.as_ref().map(|s| s.position),
        Some(Position::Absolute)
      )
    })
    .expect("absolute fragment present");

  assert!(
    (abs_fragment.bounds.x() - 50.0).abs() < 0.1,
    "static position should follow first flex item (got x = {})",
    abs_fragment.bounds.x()
  );
}

#[test]
fn absolute_child_uses_grid_track_static_position() {
  let mut container_style = ComputedStyle::default();
  container_style.display = Display::Grid;
  container_style.position = Position::Relative;
  container_style.width = Some(Length::px(100.0));
  container_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(40.0)),
    GridTrack::Length(Length::px(60.0)),
  ];

  let mut flow_style = ComputedStyle::default();
  flow_style.width = Some(Length::px(20.0));
  flow_style.height = Some(Length::px(10.0));

  let mut abs_style = ComputedStyle::default();
  abs_style.position = Position::Absolute;
  abs_style.width = Some(Length::px(10.0));
  abs_style.height = Some(Length::px(10.0));
  abs_style.grid_column_start = 2;
  abs_style.grid_column_end = 3;

  let flow_child = BoxNode::new_block(Arc::new(flow_style), FormattingContextType::Block, vec![]);
  let abs_child = BoxNode::new_block(Arc::new(abs_style), FormattingContextType::Block, vec![]);
  let container = BoxNode::new_block(
    Arc::new(container_style),
    FormattingContextType::Grid,
    vec![flow_child, abs_child],
  );

  let constraints = LayoutConstraints::definite(100.0, 100.0);
  let fc = GridFormattingContext::new();
  let fragment = fc.layout(&container, &constraints).expect("grid layout");

  let abs_fragment = fragment
    .children
    .iter()
    .find(|child| {
      matches!(
        child.style.as_ref().map(|s| s.position),
        Some(Position::Absolute)
      )
    })
    .expect("absolute fragment present");

  assert!(
    (abs_fragment.bounds.x() - 40.0).abs() < 0.1,
    "static position should align with second grid column start (got x = {})",
    abs_fragment.bounds.x()
  );
}
