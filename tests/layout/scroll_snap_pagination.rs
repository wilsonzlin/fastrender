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
  assert!(
    fragments.len() > 1,
    "pagination should produce multiple fragments"
  );

  let viewport = Size::new(200.0, 200.0);
  let mut tree = FragmentTree::from_fragments(fragments, viewport);

  fn find_box_id_abs_y(node: &FragmentNode, origin: Point, box_id: usize) -> Option<f32> {
    let abs = Point::new(origin.x + node.bounds.x(), origin.y + node.bounds.y());
    if matches!(node.content, FragmentContent::Block { box_id: Some(id) } if id == box_id) {
      return Some(abs.y);
    }
    for child in &node.children {
      if let Some(found) = find_box_id_abs_y(child, abs, box_id) {
        return Some(found);
      }
    }
    None
  }

  let mut expected_target_y: Option<f32> = None;
  let mut target_root_index: Option<usize> = None;

  for (root, root_idx) in std::iter::once((&tree.root, 0usize)).chain(
    tree
      .additional_fragments
      .iter()
      .enumerate()
      .map(|(idx, root)| (root, idx + 1)),
  ) {
    if let Some(y) = find_box_id_abs_y(root, Point::ZERO, 3) {
      expected_target_y = Some(y);
      target_root_index = Some(root_idx);
      break;
    }
  }

  let expected_target_y = expected_target_y.expect("target on a later page");
  assert!(
    target_root_index.unwrap_or(0) > 0,
    "target should appear in an additional fragment"
  );

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
