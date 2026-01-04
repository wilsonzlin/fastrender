use fastrender::layout::constraints::LayoutConstraints;
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
fn absolute_child_in_subgrid_uses_grid_track_static_position() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.position = Position::Relative;
  parent_style.width = Some(Length::px(100.0));
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(40.0)),
    GridTrack::Length(Length::px(60.0)),
  ];

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut abs_style = ComputedStyle::default();
  abs_style.position = Position::Absolute;
  abs_style.width = Some(Length::px(10.0));
  abs_style.height = Some(Length::px(10.0));
  abs_style.grid_column_start = 2;
  abs_style.grid_column_end = 3;

  let abs_child = BoxNode::new_block(Arc::new(abs_style), FormattingContextType::Block, vec![]);
  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![abs_child],
  );
  let container = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid],
  );

  let constraints = LayoutConstraints::definite(100.0, 100.0);
  let fc = GridFormattingContext::new();
  let fragment = fc.layout(&container, &constraints).expect("grid layout");

  let abs_fragment = fragment
    .iter_fragments()
    .find(|node| {
      matches!(
        node.style.as_ref().map(|s| s.position),
        Some(Position::Absolute)
      )
    })
    .expect("absolute fragment present");

  assert!(
    (abs_fragment.bounds.x() - 40.0).abs() < 0.1,
    "static position should align with second inherited grid column start (got x = {})",
    abs_fragment.bounds.x()
  );
}

