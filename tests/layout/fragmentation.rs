use std::sync::Arc;

use fastrender::layout::fragmentation::{fragment_tree, FragmentationOptions};
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::{BreakBetween, BreakInside};
use fastrender::style::values::Length;
use fastrender::tree::box_tree::BoxNode;
use fastrender::{
  BoxTree, ComputedStyle, FragmentContent, FragmentNode, LayoutConfig, LayoutEngine, Rect, Size,
};

#[test]
fn pagination_respects_gap_and_forced_break() {
  let mut breaker_style = ComputedStyle::default();
  breaker_style.break_after = BreakBetween::Always;
  let breaker = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 50.0, 20.0),
    vec![],
    Arc::new(breaker_style),
  );
  let follower = FragmentNode::new_block(Rect::from_xywh(0.0, 30.0, 50.0, 20.0), vec![]);
  let root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 100.0, 140.0),
    vec![breaker, follower],
  );

  let options = FragmentationOptions::new(80.0).with_gap(20.0);
  let fragments = fragment_tree(&root, &options);

  assert!(
    (fragments[0].bounds.height() - 20.0).abs() < 0.1,
    "forced break should end the first fragment at the forced boundary even if later candidates fit"
  );
  assert_eq!(
    fragments[0]
      .children
      .iter()
      .filter(|child| matches!(child.content, FragmentContent::Block { .. }))
      .count(),
    1,
    "forced break should precede later auto break opportunities"
  );
  assert!(
    fragments.len() >= 3,
    "forced break + overflow should yield multiple fragments"
  );
  assert!((fragments[1].bounds.y() - 100.0).abs() < 0.01);
  assert!((fragments[2].bounds.y() - 200.0).abs() < 0.01);
  assert!(fragments[0]
    .children
    .iter()
    .all(|child| child.bounds.y() >= 0.0 && child.bounds.max_y() <= 80.0));
  assert!(fragments[1]
    .children
    .iter()
    .any(|child| matches!(child.content, FragmentContent::Block { .. })));
}

#[test]
fn pagination_without_candidates_uses_fragmentainer_size() {
  // Single tall block without explicit break opportunities should still fragment by height.
  let block = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 40.0, 150.0), vec![]);
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 150.0), vec![block]);

  let fragments = fragment_tree(&root, &FragmentationOptions::new(60.0));

  assert_eq!(fragments.len(), 3);
  assert!((fragments[1].bounds.y() - 60.0).abs() < 0.1);
  assert!((fragments[2].bounds.y() - 120.0).abs() < 0.1);
}

#[test]
fn break_inside_avoid_is_best_effort() {
  let mut avoid_style = ComputedStyle::default();
  avoid_style.break_inside = BreakInside::Avoid;

  // Case A: an avoided block that fits should be moved intact to the next fragmentainer.
  let intro = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 80.0, 30.0), vec![]);
  let avoided = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 30.0, 80.0, 30.0),
    vec![],
    Arc::new(avoid_style.clone()),
  );
  let trailing = FragmentNode::new_block(Rect::from_xywh(0.0, 60.0, 80.0, 10.0), vec![]);
  let root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 120.0, 70.0),
    vec![intro, avoided, trailing],
  );

  let fragments = fragment_tree(&root, &FragmentationOptions::new(50.0));

  assert_eq!(fragments.len(), 2);
  assert_eq!(
    fragments[0].children.len(),
    1,
    "avoid block should be moved rather than split when it fits in the next fragment"
  );
  assert_eq!(
    fragments[1].children.len(),
    2,
    "content after the avoided block should continue flowing"
  );
  assert!(
    (fragments[1].children[0].bounds.height() - 30.0).abs() < 0.1,
    "avoid block should remain intact when moved"
  );

  // Case B: avoid is best-effort for content taller than a fragmentainer.
  let tall_avoid = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 80.0, 140.0),
    vec![],
    Arc::new(avoid_style),
  );
  let after = FragmentNode::new_block(Rect::from_xywh(0.0, 140.0, 80.0, 10.0), vec![]);
  let tall_root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 120.0, 150.0),
    vec![tall_avoid, after],
  );

  let tall_fragments = fragment_tree(&tall_root, &FragmentationOptions::new(80.0));

  assert!(
    tall_fragments.len() >= 2,
    "avoid blocks taller than the fragmentainer should still be fragmented"
  );
  let first_piece = tall_fragments[0]
    .children
    .first()
    .expect("first fragment should carry avoided content");
  assert!(
    first_piece.bounds.height() > 70.0,
    "first fragment should contain a clipped portion of the tall avoid block"
  );
  assert!(
    first_piece.bounds.height() <= 80.1,
    "avoided block should be clipped to the fragmentainer height when it cannot fit"
  );
  assert!(
    !tall_fragments[1].children.is_empty(),
    "subsequent fragments should continue flowing the avoided content and what follows"
  );
}

#[test]
fn line_fragments_are_atomic() {
  let line = FragmentNode::new_line(Rect::from_xywh(0.0, 0.0, 80.0, 30.0), 12.0, vec![]);
  let paragraph = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 30.0), vec![line]);

  let fragments = fragment_tree(&paragraph, &FragmentationOptions::new(20.0));

  assert_eq!(fragments.len(), 2);
  let first_lines: Vec<_> = fragments[0]
    .children
    .iter()
    .filter(|c| matches!(c.content, FragmentContent::Line { .. }))
    .collect();
  assert_eq!(first_lines.len(), 1);
  assert!(
    (first_lines[0].bounds.height() - 30.0).abs() < 0.1,
    "lines should stay intact even when the chosen break slices through them"
  );
  let second_lines: Vec<_> = fragments[1]
    .children
    .iter()
    .filter(|c| matches!(c.content, FragmentContent::Line { .. }))
    .collect();
  assert!(
    second_lines.is_empty(),
    "line content should not be duplicated or partially clipped in later fragments"
  );
}

#[test]
fn positioned_children_follow_fragmentainers() {
  let normal = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 60.0, 40.0), vec![]);
  let abs_child = FragmentNode::new_block(Rect::from_xywh(0.0, 120.0, 40.0, 20.0), vec![]);
  let root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 100.0, 180.0),
    vec![normal, abs_child],
  );

  let fragments = fragment_tree(&root, &FragmentationOptions::new(80.0));

  assert_eq!(fragments.len(), 3);
  let positioned_home = fragments.iter().position(|fragment| {
    fragment
      .children
      .iter()
      .any(|c| (c.bounds.height() - 20.0).abs() < 0.1)
  });
  assert!(positioned_home.expect("positioned fragment placed") > 0);
}

#[test]
fn column_fragmentation_places_fragments_in_grid() {
  let mut children = Vec::new();
  let mut y = 0.0;
  for _ in 0..5 {
    children.push(FragmentNode::new_block(
      Rect::from_xywh(0.0, y, 180.0, 40.0),
      vec![],
    ));
    y += 40.0;
  }

  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 200.0, y), children);
  let options = FragmentationOptions::new(80.0)
    .with_gap(15.0)
    .with_columns(2, 10.0);

  let fragments = fragment_tree(&root, &options);

  assert_eq!(fragments.len(), 3);

  let column_width = ((root.bounds.width()
    - (options.column_count.saturating_sub(1) as f32 * options.column_gap))
    .max(0.0))
    / options.column_count as f32;
  let expected_xs = [0.0, column_width + options.column_gap, 0.0];
  let expected_ys = [
    0.0,
    0.0,
    options.fragmentainer_size + options.fragmentainer_gap,
  ];

  for (fragment, (&expected_x, &expected_y)) in fragments
    .iter()
    .zip(expected_xs.iter().zip(expected_ys.iter()))
  {
    assert!((fragment.bounds.width() - column_width).abs() < 0.01);
    assert!((fragment.bounds.x() - expected_x).abs() < 0.01);
    assert!((fragment.bounds.y() - expected_y).abs() < 0.01);

    assert_eq!(fragment.fragmentainer_index, fragment.fragment_index);
    for child in &fragment.children {
      assert_eq!(child.fragmentainer_index, fragment.fragmentainer_index);
      let relative_start = child.bounds.y() - fragment.bounds.y();
      let relative_end = child.bounds.max_y() - fragment.bounds.y();
      assert!(
        relative_start >= -0.01 && relative_end <= fragment.bounds.height() + 0.01,
        "child extends outside fragment: child {:?} fragment {:?}",
        child.bounds,
        fragment.bounds
      );
    }
  }

  assert_eq!(fragments[0].children.len(), 2);
  assert_eq!(fragments[1].children.len(), 2);
  assert_eq!(fragments[2].children.len(), 1);
}

#[test]
fn column_fragmentation_places_columns_side_by_side() {
  let fragmentainer_size = 60.0;
  let fragmentainer_gap = 5.0;
  let column_gap = 10.0;
  let epsilon = 0.01;

  let tall_child = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 260.0), vec![]);
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 260.0), vec![tall_child]);

  let options = FragmentationOptions::new(fragmentainer_size)
    .with_gap(fragmentainer_gap)
    .with_columns(2, column_gap);
  let fragments = fragment_tree(&root, &options);

  assert!(
    fragments.len() >= 4,
    "tall content should generate multiple fragmentainers"
  );

  let first = &fragments[0];
  let second = &fragments[1];
  assert!(
    (first.bounds.y() - second.bounds.y()).abs() < epsilon,
    "first column set should share vertical origin"
  );
  assert_ne!(
    first.bounds.x(),
    second.bounds.x(),
    "columns should be positioned side by side"
  );
  assert!(
    (second.bounds.x() - (root.bounds.width() + column_gap)).abs() < epsilon,
    "column gap should affect horizontal placement"
  );

  let third = &fragments[2];
  assert!(
    third.bounds.y() >= fragmentainer_size + fragmentainer_gap - epsilon,
    "new column set should be translated down by fragmentainer height and gap"
  );
  assert!(
    (third.bounds.x() - first.bounds.x()).abs() < epsilon,
    "column sets should reset to the first column horizontally"
  );
}

#[test]
fn layout_engine_pagination_splits_pages() {
  let mut style = ComputedStyle::default();
  style.height = Some(Length::px(150.0));
  let root_box = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]);
  let box_tree = BoxTree::new(root_box);

  let config = LayoutConfig::for_pagination(Size::new(200.0, 60.0), 10.0);
  let engine = LayoutEngine::new(config);
  let fragments = engine.layout_tree(&box_tree).expect("layout");

  assert_eq!(fragments.additional_fragments.len(), 2);
  assert!(fragments.root.fragment_count >= 3);
  assert!((fragments.additional_fragments[0].bounds.y() - 70.0).abs() < 0.1);
  assert!((fragments.additional_fragments[1].bounds.y() - 140.0).abs() < 0.1);
}
