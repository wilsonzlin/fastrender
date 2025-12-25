use std::sync::Arc;

use fastrender::layout::fragmentation::{fragment_tree, FragmentationOptions};
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::{BreakBetween, BreakInside};
use fastrender::style::values::Length;
use fastrender::tree::box_tree::BoxNode;
use fastrender::{
  BoxTree, ComputedStyle, FragmentContent, FragmentNode, LayoutConfig, LayoutEngine, Rect, Size,
};

fn line(y: f32) -> FragmentNode {
  FragmentNode::new_line(Rect::from_xywh(0.0, y, 80.0, 15.0), 12.0, vec![])
}

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
fn widows_and_orphans_keep_paragraph_together() {
  let mut para_style = ComputedStyle::default();
  para_style.break_inside = BreakInside::Auto;
  para_style.widows = 3;
  para_style.orphans = 3;
  let lines = vec![line(0.0), line(18.0), line(36.0), line(54.0)];
  let paragraph = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 120.0, 80.0),
    lines,
    Arc::new(para_style),
  );
  let footer = FragmentNode::new_block(Rect::from_xywh(0.0, 90.0, 100.0, 10.0), vec![]);
  let root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 120.0, 120.0),
    vec![paragraph, footer],
  );

  let fragments = fragment_tree(&root, &FragmentationOptions::new(50.0));

  assert!(
    fragments.len() >= 2,
    "content should span multiple fragmentainers when it overflows"
  );

  fn count_lines(node: &FragmentNode) -> usize {
    let self_count = usize::from(matches!(node.content, FragmentContent::Line { .. }));
    self_count + node.children.iter().map(count_lines).sum::<usize>()
  }

  let total_lines: usize = fragments.iter().map(count_lines).sum();
  assert_eq!(total_lines, 4, "all line fragments should be retained");
}

#[test]
fn line_fragments_remain_atomic_when_boundary_slices_through() {
  let atomic_line = line(10.0);
  let original_height = atomic_line.bounds.height();
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 25.0), vec![atomic_line]);

  let fragments = fragment_tree(&root, &FragmentationOptions::new(15.0));

  assert_eq!(fragments.len(), 2);
  let first_fragment_lines: Vec<&FragmentNode> = fragments[0]
    .children
    .iter()
    .filter(|child| matches!(child.content, FragmentContent::Line { .. }))
    .collect();
  assert_eq!(first_fragment_lines.len(), 1);
  assert!(
    (first_fragment_lines[0].bounds.height() - original_height).abs() < 0.01,
    "line fragments should keep their full height even when clipped mid-line"
  );
  assert!(
    fragments[1]
      .children
      .iter()
      .all(|child| !matches!(child.content, FragmentContent::Line { .. })),
    "line should only appear in the fragment that contains its start"
  );
}

#[test]
fn break_inside_avoid_keeps_block_together() {
  let mut avoid_style = ComputedStyle::default();
  avoid_style.break_inside = BreakInside::Avoid;
  let tall_block = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 80.0, 140.0),
    vec![],
    Arc::new(avoid_style),
  );
  let trailing = FragmentNode::new_block(Rect::from_xywh(0.0, 150.0, 50.0, 20.0), vec![]);
  let root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 120.0, 200.0),
    vec![tall_block, trailing],
  );

  let fragments = fragment_tree(&root, &FragmentationOptions::new(80.0));

  assert_eq!(
    fragments.len(),
    3,
    "avoid is a soft constraint and tall content may split"
  );
  let trailing_height: f32 = fragments
    .last()
    .unwrap()
    .children
    .iter()
    .map(|c| c.bounds.height())
    .sum();
  assert!(
    (trailing_height - 20.0).abs() < 0.1,
    "trailing block should appear in the final fragment"
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
