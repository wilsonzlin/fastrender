use std::sync::Arc;

use fastrender::layout::fragmentation::{fragment_tree, FragmentationOptions};
use fastrender::style::types::{BreakBetween, BreakInside};
use fastrender::{ComputedStyle, FragmentContent, FragmentNode, Rect};

#[test]
fn block_fragmentation_splits_by_fragmentainer_height() {
  let child1 = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 80.0), vec![]);
  let child2 = FragmentNode::new_block(Rect::from_xywh(0.0, 120.0, 50.0, 30.0), vec![]);
  let root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 200.0, 200.0),
    vec![child1, child2],
  );

  let fragments = fragment_tree(&root, &FragmentationOptions::new(100.0));
  let child_counts: Vec<_> = fragments.iter().map(|f| f.children.len()).collect();
  assert_eq!(fragments.len(), 2, "fragments={:?}", child_counts);
  assert_eq!(fragments[0].fragment_index, 0);
  assert_eq!(fragments[1].fragment_index, 1);
  assert_eq!(
    fragments[0].children.len(),
    1,
    "first fragment should keep first child only"
  );
  assert_eq!(
    fragments[1].children.len(),
    1,
    "second fragment should start with overflowing child"
  );
  assert!((fragments[1].children[0].bounds.y() - 20.0).abs() < 0.01);
}

#[test]
fn break_after_forces_new_fragment_boundary() {
  let mut style = ComputedStyle::default();
  style.break_after = BreakBetween::Always;
  let breaker = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 50.0, 20.0),
    vec![],
    Arc::new(style),
  );
  let follower = FragmentNode::new_block(Rect::from_xywh(0.0, 30.0, 50.0, 20.0), vec![]);
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 120.0), vec![breaker, follower]);

  let fragments = fragment_tree(&root, &FragmentationOptions::new(80.0));

  assert!(fragments.len() >= 2, "forced break should create a new fragment");
  assert_eq!(fragments[0].children.len(), 1, "first fragment should stop after break-after child");
  assert!(fragments[1]
    .children
    .iter()
    .any(|child| matches!(child.content, FragmentContent::Block { .. })),
    "follower should move to next fragment");
}

#[test]
fn avoid_inside_and_widows_keep_block_together() {
  let mut para_style = ComputedStyle::default();
  para_style.break_inside = BreakInside::Avoid;
  para_style.widows = 3;
  para_style.orphans = 3;

  let mut lines = Vec::new();
  for i in 0..4 {
    lines.push(FragmentNode::new_line(
      Rect::from_xywh(0.0, (i as f32) * 20.0, 80.0, 15.0),
      12.0,
      vec![],
    ));
  }

  let paragraph = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 100.0, 90.0),
    lines,
    Arc::new(para_style),
  );
  let footer = FragmentNode::new_block(Rect::from_xywh(0.0, 100.0, 100.0, 10.0), vec![]);
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 120.0), vec![paragraph, footer]);

  let fragments = fragment_tree(&root, &FragmentationOptions::new(50.0));

  assert!(fragments.len() >= 2);
  // Paragraph should only appear in the first fragment despite spanning beyond it.
  assert!(fragments[0]
    .children
    .iter()
    .any(|child| matches!(child.content, FragmentContent::Block { .. })),
    "paragraph should live in first fragment");
  assert!(fragments[1]
    .children
    .iter()
    .all(|child| !matches!(child.content, FragmentContent::Line { .. })),
    "continuations should not split paragraph lines");
}
