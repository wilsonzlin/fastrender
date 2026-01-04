use std::sync::Arc;

use fastrender::layout::constraints::{AvailableSpace, LayoutConstraints};
use fastrender::layout::contexts::grid::GridFormattingContext;
use fastrender::layout::formatting_context::FormattingContext;
use fastrender::style::display::Display;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::GridTrack;
use fastrender::style::values::Length;
use fastrender::tree::box_tree::BoxNode;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode};
use fastrender::ComputedStyle;

fn layout_grid_with_running_item() -> FragmentNode {
  let mut grid_style = ComputedStyle::default();
  grid_style.display = Display::Grid;
  grid_style.width = Some(Length::px(100.0));
  grid_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(40.0)),
    GridTrack::Length(Length::px(60.0)),
  ];

  let mut running_style = ComputedStyle::default();
  running_style.display = Display::Block;
  running_style.running_position = Some("header".to_string());
  running_style.grid_column_start = 2;
  running_style.grid_column_end = 3;

  let running_text = BoxNode::new_text(Arc::new(ComputedStyle::default()), "GRID HEADER".into());
  let mut running = BoxNode::new_block(
    Arc::new(running_style),
    FormattingContextType::Block,
    vec![running_text],
  );
  running.id = 10;

  let cell_text = BoxNode::new_text(Arc::new(ComputedStyle::default()), "Cell".into());
  let mut cell = BoxNode::new_block(
    Arc::new(ComputedStyle::default()),
    FormattingContextType::Block,
    vec![cell_text],
  );
  cell.id = 20;

  let container = BoxNode::new_block(
    Arc::new(grid_style),
    FormattingContextType::Grid,
    vec![running, cell],
  );

  let grid_fc = GridFormattingContext::new();
  let constraints =
    LayoutConstraints::new(AvailableSpace::Definite(100.0), AvailableSpace::Indefinite);
  grid_fc
    .layout(&container, &constraints)
    .expect("grid layout should succeed")
}

#[test]
fn running_position_in_grid_creates_anchor_and_removes_flow_content() {
  let fragment = layout_grid_with_running_item();

  let anchor_count = fragment
    .iter_fragments()
    .filter(|frag| matches!(frag.content, FragmentContent::RunningAnchor { .. }))
    .count();
  assert_eq!(anchor_count, 1, "expected one running anchor fragment");

  let mut flow_text = fragment
    .iter_fragments()
    .filter_map(|frag| {
      if let FragmentContent::Text { text, .. } = &frag.content {
        Some(text.to_string())
      } else {
        None
      }
    })
    .collect::<Vec<_>>()
    .join("");
  flow_text.retain(|c| !c.is_whitespace());

  assert!(
    flow_text.contains("Cell"),
    "expected in-flow grid item content"
  );
  assert!(
    !flow_text.contains("GRIDHEADER"),
    "running content should not appear in normal flow"
  );
}

#[test]
fn running_anchor_in_grid_aligns_to_track_start() {
  let fragment = layout_grid_with_running_item();

  let anchor = fragment
    .iter_fragments()
    .find(|frag| matches!(frag.content, FragmentContent::RunningAnchor { .. }))
    .expect("running anchor fragment");
  let x = anchor.bounds.x();
  assert!(
    (x - 40.0).abs() < 0.1,
    "expected anchor x to align to start of column 2 (40px), got {x}"
  );
}

