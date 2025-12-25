use std::sync::Arc;

use fastrender::geometry::{Point, Rect, Size};
use fastrender::layout::fragmentation::{fragment_tree, FragmentationOptions};
use fastrender::scroll::{apply_scroll_snap, ScrollState};
use fastrender::style::types::{ScrollSnapAlign, ScrollSnapAxis, ScrollSnapStrictness};
use fastrender::{ComputedStyle, FragmentContent, FragmentNode, FragmentTree};

#[test]
fn scroll_snap_targets_include_fragment_offsets() {
  let mut container_style = ComputedStyle::default();
  container_style.scroll_snap_type.axis = ScrollSnapAxis::Y;
  container_style.scroll_snap_type.strictness = ScrollSnapStrictness::Mandatory;
  let container_style = Arc::new(container_style);

  let mut target_style = ComputedStyle::default();
  target_style.scroll_snap_align.block = ScrollSnapAlign::Start;
  let target_style = Arc::new(target_style);

  let target1 = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 0.0, 200.0, 100.0),
    FragmentContent::Block { box_id: Some(2) },
    vec![],
    target_style.clone(),
  );
  let spacer = FragmentNode::new_block(Rect::from_xywh(0.0, 100.0, 200.0, 100.0), vec![]);
  let target2 = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 200.0, 200.0, 100.0),
    FragmentContent::Block { box_id: Some(3) },
    vec![],
    target_style,
  );

  let root = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 0.0, 200.0, 400.0),
    FragmentContent::Block { box_id: Some(1) },
    vec![target1, spacer, target2],
    container_style,
  );

  let fragments = fragment_tree(&root, &FragmentationOptions::new(200.0).with_gap(30.0));
  assert_eq!(
    fragments.len(),
    2,
    "pagination should produce two fragments"
  );

  let viewport = Size::new(200.0, 200.0);
  let mut tree = FragmentTree::from_fragments(fragments, viewport);

  let second_page = tree.additional_fragments.first().expect("second page");
  let expected_target_y = second_page.bounds.y()
    + second_page
      .children
      .first()
      .expect("target on second page")
      .bounds
      .y();

  let scroll_state = ScrollState::with_viewport(Point::new(0.0, expected_target_y - 20.0));
  let snapped = apply_scroll_snap(&mut tree, &scroll_state);

  assert!(
    (snapped.state.viewport.y - expected_target_y).abs() < 0.1,
    "snap should reach target on stacked page (expected {expected_target_y}, got {})",
    snapped.state.viewport.y
  );
  assert!(
    snapped.state.viewport.y >= expected_target_y - 0.1,
    "snap should not clamp to the first page"
  );
}
