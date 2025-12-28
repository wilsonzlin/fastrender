use std::sync::Arc;

use fastrender::layout::fragmentation::{
  fragment_tree, resolve_fragmentation_boundaries_with_context, FragmentationContext,
  FragmentationOptions,
};
use fastrender::style::display::{Display, FormattingContextType};
use fastrender::style::position::Position;
use fastrender::style::types::{BreakBetween, BreakInside, WritingMode};
use fastrender::style::values::Length;
use fastrender::tree::box_tree::BoxNode;
use fastrender::{
  BoxTree, ComputedStyle, FastRender, FragmentContent, FragmentNode, FragmentTree, LayoutConfig,
  LayoutEngine, Point, Rect, Size,
};

fn line(y: f32, height: f32) -> FragmentNode {
  FragmentNode::new_line(Rect::from_xywh(0.0, y, 80.0, height), height * 0.8, vec![])
}

fn count_lines(node: &FragmentNode) -> usize {
  node
    .iter_fragments()
    .filter(|fragment| matches!(fragment.content, FragmentContent::Line { .. }))
    .count()
}

#[test]
fn paragraph_breaks_on_line_boundaries() {
  let lines: Vec<_> = (0..6).map(|i| line(i as f32 * 12.0, 12.0)).collect();
  let para_height = lines.last().map(|l| l.bounds.max_y()).unwrap_or(0.0);
  let paragraph =
    FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, para_height), lines.clone());
  let root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 120.0, para_height),
    vec![paragraph],
  );

  let fragments = fragment_tree(&root, &FragmentationOptions::new(24.0));

  assert_eq!(fragments.len(), 3);
  let per_fragment: Vec<_> = fragments.iter().map(count_lines).collect();
  assert_eq!(per_fragment, vec![2, 2, 2]);
  assert_eq!(per_fragment.iter().sum::<usize>(), lines.len());

  for fragment in &fragments {
    for line in fragment
      .iter_fragments()
      .filter(|f| matches!(f.content, FragmentContent::Line { .. }))
    {
      assert!(
        (line.bounds.height() - 12.0).abs() < 0.01,
        "lines must not be clipped"
      );
    }
  }
}

fn collect_lines<'a>(fragment: &'a FragmentNode) -> Vec<&'a FragmentNode> {
  let mut lines = Vec::new();
  let mut stack = vec![fragment];
  while let Some(node) = stack.pop() {
    if matches!(node.content, FragmentContent::Line { .. }) {
      lines.push(node);
    }
    for child in &node.children {
      stack.push(child);
    }
  }
  lines
}

fn fragments_with_id<'a>(fragment: &'a FragmentNode, id: usize) -> Vec<&'a FragmentNode> {
  let mut out = Vec::new();
  let mut stack = vec![fragment];
  while let Some(node) = stack.pop() {
    if let FragmentContent::Block { box_id: Some(b) } = node.content {
      if b == id {
        out.push(node);
      }
    }
    for child in &node.children {
      stack.push(child);
    }
  }
  out
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
fn vertical_writing_fragment_tree_columns_use_inline_axis() {
  let mut style = ComputedStyle::default();
  style.display = Display::Block;
  style.writing_mode = WritingMode::VerticalLr;
  let style = Arc::new(style);

  let child = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 150.0, 40.0),
    vec![],
    style.clone(),
  );
  let root =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 150.0, 40.0), vec![child], style);

  let fragments = fragment_tree(
    &root,
    &FragmentationOptions::new(60.0).with_columns(2, 10.0),
  );

  assert_eq!(fragments.len(), 3);
  assert_eq!(fragments[0].bounds.origin, Point::ZERO);
  assert!((fragments[1].bounds.x()).abs() < 0.01);
  assert!((fragments[1].bounds.y() - (40.0 + 10.0)).abs() < 0.01);
  assert!((fragments[2].bounds.x() - 60.0).abs() < 0.01);
  assert!((fragments[2].bounds.y()).abs() < 0.01);

  for (idx, fragment) in fragments.iter().enumerate() {
    assert_eq!(
      fragment.children.len(),
      1,
      "fragment {idx} should preserve the lone child"
    );
    let fragment_child = &fragment.children[0];
    let slice_start = (idx as f32) * 60.0;
    let expected_block_size = (root.bounds.width() - slice_start).min(60.0);
    assert!(
      (fragment_child.bounds.width() - expected_block_size).abs() < 0.01,
      "fragment {idx} child width should match the clipped block slice"
    );
  }
}

#[test]
fn vertical_lr_fragmentation_clips_on_x_and_sets_slice_info() {
  let mut style = ComputedStyle::default();
  style.display = Display::Block;
  style.writing_mode = WritingMode::VerticalLr;
  let style = Arc::new(style);

  let mut child1 =
    FragmentNode::new_block_with_id(Rect::from_xywh(0.0, 0.0, 40.0, 60.0), 1, vec![]);
  child1.style = Some(style.clone());
  let mut child2 =
    FragmentNode::new_block_with_id(Rect::from_xywh(40.0, 0.0, 40.0, 60.0), 2, vec![]);
  child2.style = Some(style.clone());

  let root = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 80.0, 60.0),
    vec![child1, child2],
    style,
  );

  let fragments = fragment_tree(&root, &FragmentationOptions::new(50.0));

  assert_eq!(fragments.len(), 2);

  let first_child2 = fragments_with_id(&fragments[0], 2);
  let second_child2 = fragments_with_id(&fragments[1], 2);

  assert_eq!(first_child2.len(), 1);
  assert_eq!(second_child2.len(), 1);

  let first_slice = first_child2[0];
  let second_slice = second_child2[0];

  assert!((first_slice.bounds.width() - 10.0).abs() < 0.01);
  assert!((second_slice.bounds.width() - 30.0).abs() < 0.01);

  assert!(first_slice.slice_info.is_first);
  assert!(!first_slice.slice_info.is_last);
  assert!(first_slice.slice_info.slice_offset.abs() < 0.01);
  assert!((first_slice.slice_info.original_block_size - 40.0).abs() < 0.01);

  assert!(!second_slice.slice_info.is_first);
  assert!(second_slice.slice_info.is_last);
  assert!((second_slice.slice_info.slice_offset - 10.0).abs() < 0.01);
  assert!((second_slice.slice_info.original_block_size - 40.0).abs() < 0.01);
}

#[test]
fn vertical_rl_fragmentation_block_negative_slice_info() {
  let mut style = ComputedStyle::default();
  style.display = Display::Block;
  style.writing_mode = WritingMode::VerticalRl;
  let style = Arc::new(style);

  let mut child1 =
    FragmentNode::new_block_with_id(Rect::from_xywh(40.0, 0.0, 40.0, 60.0), 1, vec![]);
  child1.style = Some(style.clone());
  let mut child2 =
    FragmentNode::new_block_with_id(Rect::from_xywh(0.0, 0.0, 40.0, 60.0), 2, vec![]);
  child2.style = Some(style.clone());

  let root = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 80.0, 60.0),
    vec![child1, child2],
    style,
  );

  let fragments = fragment_tree(&root, &FragmentationOptions::new(50.0));

  assert_eq!(fragments.len(), 2);

  let first_child2 = fragments_with_id(&fragments[0], 2);
  let second_child2 = fragments_with_id(&fragments[1], 2);

  assert_eq!(first_child2.len(), 1);
  assert_eq!(second_child2.len(), 1);

  let first_slice = first_child2[0];
  let second_slice = second_child2[0];

  assert!((first_slice.bounds.width() - 10.0).abs() < 0.01);
  assert!((second_slice.bounds.width() - 30.0).abs() < 0.01);

  assert!(first_slice.slice_info.is_first);
  assert!(!first_slice.slice_info.is_last);
  assert!(first_slice.slice_info.slice_offset.abs() < 0.01);
  assert!((first_slice.slice_info.original_block_size - 40.0).abs() < 0.01);

  assert!(!second_slice.slice_info.is_first);
  assert!(second_slice.slice_info.is_last);
  assert!((second_slice.slice_info.slice_offset - 10.0).abs() < 0.01);
  assert!((second_slice.slice_info.original_block_size - 40.0).abs() < 0.01);
}

#[test]
fn vertical_writing_fragment_stacking_uses_x_for_gap() {
  let mut style = ComputedStyle::default();
  style.display = Display::Block;
  style.writing_mode = WritingMode::VerticalLr;
  let style = Arc::new(style);

  let child =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 80.0, 20.0), vec![], style.clone());
  let root =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 80.0, 20.0), vec![child], style);

  let fragments = fragment_tree(&root, &FragmentationOptions::new(50.0).with_gap(20.0));

  assert_eq!(fragments.len(), 2);
  assert!(fragments[0].bounds.x().abs() < 0.01);
  assert!(fragments[0].bounds.y().abs() < 0.01);
  assert!((fragments[1].bounds.x() - 70.0).abs() < 0.01);
  assert!(fragments[1].bounds.y().abs() < 0.01);
}

#[test]
fn widows_and_orphans_keep_paragraph_together() {
  let mut para_style = ComputedStyle::default();
  para_style.break_inside = BreakInside::Auto;
  para_style.widows = 3;
  para_style.orphans = 3;
  let lines = vec![
    line(0.0, 15.0),
    line(18.0, 15.0),
    line(36.0, 15.0),
    line(54.0, 15.0),
  ];
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

  let total_lines: usize = fragments.iter().map(count_lines).sum();
  assert_eq!(total_lines, 4, "all line fragments should be retained");
}

#[test]
fn line_fragments_remain_atomic_when_boundary_slices_through() {
  let atomic_line = line(10.0, 15.0);
  let original_height = atomic_line.bounds.height();
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 25.0), vec![atomic_line]);

  let fragments = fragment_tree(&root, &FragmentationOptions::new(15.0));

  assert_eq!(fragments.len(), 2);
  let first_fragment_lines = count_lines(&fragments[0]);
  let second_fragment_lines: Vec<&FragmentNode> = fragments[1]
    .children
    .iter()
    .filter(|child| matches!(child.content, FragmentContent::Line { .. }))
    .collect();
  assert_eq!(first_fragment_lines, 0);
  assert_eq!(second_fragment_lines.len(), 1);
  assert!(
    (second_fragment_lines[0].bounds.height() - original_height).abs() < 0.01,
    "line fragments should keep their full height even when clipped mid-line"
  );
  assert!(
    fragments.iter().map(count_lines).sum::<usize>() == 1,
    "line should only appear once across fragments"
  );
}

#[test]
fn widows_and_orphans_enforced_across_multiple_breaks() {
  let mut style = ComputedStyle::default();
  style.widows = 2;
  style.orphans = 2;

  let lines: Vec<_> = (0..10).map(|i| line(i as f32 * 10.0, 10.0)).collect();
  let para_height = lines.last().map(|l| l.bounds.max_y()).unwrap_or(0.0);
  let paragraph = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 120.0, para_height),
    lines,
    Arc::new(style),
  );
  let root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 120.0, para_height),
    vec![paragraph],
  );

  let fragments = fragment_tree(&root, &FragmentationOptions::new(30.0));
  let per_fragment: Vec<_> = fragments
    .iter()
    .map(count_lines)
    .filter(|c| *c > 0)
    .collect();

  assert_eq!(per_fragment.iter().sum::<usize>(), 10);
  assert_eq!(per_fragment, vec![3, 3, 2, 2]);
  assert!(per_fragment.iter().all(|count| *count >= 2));
}

#[test]
fn break_inside_avoid_prefers_unbroken_but_splits_when_needed() {
  let mut avoid_style = ComputedStyle::default();
  avoid_style.break_inside = BreakInside::Avoid;
  let avoid_style = Arc::new(avoid_style);

  // Fits entirely within the first fragmentainer.
  let fitting_lines: Vec<_> = (0..3).map(|i| line(i as f32 * 12.0, 12.0)).collect();
  let fitting_block = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 100.0, 36.0),
    fitting_lines,
    avoid_style.clone(),
  );
  let trailing = FragmentNode::new_block(Rect::from_xywh(0.0, 40.0, 100.0, 20.0), vec![]);
  let root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 120.0, 60.0),
    vec![fitting_block, trailing],
  );

  let fragments = fragment_tree(&root, &FragmentationOptions::new(50.0));
  let per_fragment: Vec<_> = fragments.iter().map(count_lines).collect();
  assert_eq!(per_fragment.iter().sum::<usize>(), 3);
  assert_eq!(per_fragment[0], 3);

  // Taller than a fragmentainer: must break even with avoid.
  let tall_lines: Vec<_> = (0..6).map(|i| line(i as f32 * 12.0, 12.0)).collect();
  let tall_block = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 100.0, 72.0),
    tall_lines.clone(),
    avoid_style,
  );
  let tall_root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 72.0), vec![tall_block]);

  let tall_fragments = fragment_tree(&tall_root, &FragmentationOptions::new(40.0));
  let tall_counts: Vec<_> = tall_fragments.iter().map(count_lines).collect();
  assert!(tall_fragments.len() > 1);
  assert_eq!(tall_counts.iter().sum::<usize>(), tall_lines.len());
  assert!(tall_counts.iter().all(|count| *count > 0));
}

#[test]
fn forced_break_overrides_natural_flow() {
  let mut breaker_style = ComputedStyle::default();
  breaker_style.break_after = BreakBetween::Always;
  let breaker = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 50.0, 30.0),
    vec![],
    Arc::new(breaker_style),
  );
  let follower = FragmentNode::new_block(Rect::from_xywh(0.0, 30.0, 50.0, 30.0), vec![]);
  let root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 50.0, 60.0),
    vec![breaker, follower],
  );

  let fragments = fragment_tree(&root, &FragmentationOptions::new(200.0));

  assert_eq!(fragments.len(), 2);
  assert_eq!(fragments[0].children.len(), 1);
  assert_eq!(fragments[1].children.len(), 1);
}

#[test]
fn column_break_hints_follow_column_context() {
  let mut first_style = ComputedStyle::default();
  first_style.break_after = BreakBetween::Column;
  let first = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 50.0, 20.0),
    vec![],
    Arc::new(first_style),
  );
  let second = FragmentNode::new_block(Rect::from_xywh(0.0, 20.0, 50.0, 20.0), vec![]);
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 40.0), vec![first, second]);

  let options = FragmentationOptions::new(80.0).with_columns(2, 0.0);
  let fragments = fragment_tree(&root, &options);

  assert_eq!(
    fragments.len(),
    2,
    "forced column break should split fragments even when content fits"
  );

  let first_children: Vec<_> = fragments[0]
    .children
    .iter()
    .filter(|child| matches!(child.content, FragmentContent::Block { .. }))
    .collect();
  let second_children: Vec<_> = fragments[1]
    .children
    .iter()
    .filter(|child| matches!(child.content, FragmentContent::Block { .. }))
    .collect();

  assert_eq!(
    first_children.len(),
    1,
    "first fragment should only include the pre-break block"
  );
  assert_eq!(
    second_children.len(),
    1,
    "second fragment should only include the post-break block"
  );
  assert_eq!(
    first_children[0]
      .style
      .as_ref()
      .map(|s| s.break_after)
      .unwrap_or(BreakBetween::Auto),
    BreakBetween::Column
  );
  assert_eq!(
    second_children[0]
      .style
      .as_ref()
      .map(|s| s.break_after)
      .unwrap_or(BreakBetween::Auto),
    BreakBetween::Auto
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
fn avoid_page_blocks_aren_t_split_across_pages() {
  let mut avoid_style = ComputedStyle::default();
  avoid_style.break_inside = BreakInside::AvoidPage;
  let avoid_style = Arc::new(avoid_style);

  let leading = FragmentNode::new_block_with_id(Rect::from_xywh(0.0, 0.0, 40.0, 6.0), 1, vec![]);
  let mut avoid = FragmentNode::new_block_with_id(Rect::from_xywh(0.0, 6.0, 40.0, 8.0), 2, vec![]);
  avoid.style = Some(avoid_style);
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 20.0), vec![leading, avoid]);

  let fragments = fragment_tree(&root, &FragmentationOptions::new(10.0));

  assert!(
    fragments.len() >= 2,
    "overflowing content should fragment across pages"
  );

  assert!(
    fragments_with_id(&fragments[0], 2).is_empty(),
    "avoid-page content should be pushed out of the fragment that would slice it"
  );
  let avoid_fragments: Vec<_> = fragments
    .iter()
    .flat_map(|fragment| fragments_with_id(fragment, 2))
    .collect();
  assert_eq!(
    avoid_fragments.len(),
    1,
    "avoid-page content should stay intact across pagination"
  );
  assert!(
    avoid_fragments[0].fragment_index > 0,
    "avoid-page block should be moved wholly into a later fragmentainer"
  );
  assert!(
    (avoid_fragments[0].bounds.height() - 8.0).abs() < 0.1,
    "avoid-page block should retain its full height when fragmented"
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

#[test]
fn pagination_keeps_fragment_boundary_margins_separate() {
  let mut first_style = ComputedStyle::default();
  first_style.height = Some(Length::px(10.0));
  first_style.margin_bottom = Some(Length::px(30.0));

  let mut second_style = ComputedStyle::default();
  second_style.height = Some(Length::px(10.0));
  second_style.margin_top = Some(Length::px(40.0));
  second_style.break_before = BreakBetween::Page;

  let first = BoxNode::new_block(Arc::new(first_style), FormattingContextType::Block, vec![]);
  let second = BoxNode::new_block(Arc::new(second_style), FormattingContextType::Block, vec![]);
  let root = BoxNode::new_block(
    Arc::new(ComputedStyle::default()),
    FormattingContextType::Block,
    vec![first, second],
  );
  let box_tree = BoxTree::new(root);

  let config = LayoutConfig::for_pagination(Size::new(200.0, 60.0), 0.0);
  let engine = LayoutEngine::new(config);
  let fragments = engine.layout_tree(&box_tree).expect("layout");
  assert_eq!(fragments.additional_fragments.len(), 1);

  let first_page = &fragments.root;
  let second_page = &fragments.additional_fragments[0];

  let first_block = first_page
    .children
    .iter()
    .find(|c| matches!(c.content, FragmentContent::Block { .. }))
    .expect("first page block");
  let second_block = second_page
    .children
    .iter()
    .find(|c| matches!(c.content, FragmentContent::Block { .. }))
    .expect("second page block");

  assert!(
    (second_block.bounds.y() - 40.0).abs() < 0.1,
    "second page should honor its own top margin instead of inheriting a collapsed value"
  );
  let trailing_space = first_page.bounds.height() - first_block.bounds.max_y();
  assert!(
    trailing_space + 0.1 >= 30.0,
    "first page keeps the prior block's bottom margin instead of collapsing it away"
  );
}

#[test]
fn multicolumn_breaks_do_not_carry_collapsed_margins() {
  let mut root_style = ComputedStyle::default();
  root_style.column_count = Some(2);
  root_style.column_gap = Length::px(0.0);
  root_style.width = Some(Length::px(200.0));

  let mut first_style = ComputedStyle::default();
  first_style.height = Some(Length::px(20.0));
  first_style.margin_bottom = Some(Length::px(60.0));

  let mut second_style = ComputedStyle::default();
  second_style.height = Some(Length::px(20.0));
  second_style.margin_top = Some(Length::px(20.0));

  let first = BoxNode::new_block(Arc::new(first_style), FormattingContextType::Block, vec![]);
  let second = BoxNode::new_block(Arc::new(second_style), FormattingContextType::Block, vec![]);
  let root = BoxNode::new_block(
    Arc::new(root_style),
    FormattingContextType::Block,
    vec![first, second],
  );
  let box_tree = BoxTree::new(root);

  let engine = LayoutEngine::with_defaults();
  let fragments = engine.layout_tree(&box_tree).expect("layout");

  let mut block_fragments: Vec<_> = fragments
    .root
    .children
    .iter()
    .filter(|c| {
      matches!(c.content, FragmentContent::Block { .. }) && (c.bounds.height() - 20.0).abs() < 0.1
    })
    .collect();
  block_fragments.sort_by(|a, b| {
    a.bounds
      .x()
      .partial_cmp(&b.bounds.x())
      .unwrap_or(std::cmp::Ordering::Equal)
  });

  assert_eq!(
    block_fragments.len(),
    2,
    "expected one fragment per column for the two blocks"
  );
  let second_column = block_fragments[1];
  assert!(
    (second_column.bounds.y() - 20.0).abs() < 0.1,
    "second column should restart margin collapsing at the column boundary"
  );
}

#[test]
fn sticky_offsets_apply_to_additional_fragments() {
  let mut spacer_style = ComputedStyle::default();
  spacer_style.height = Some(Length::px(150.0));
  let spacer = BoxNode::new_block(Arc::new(spacer_style), FormattingContextType::Block, vec![]);

  let mut sticky_style = ComputedStyle::default();
  sticky_style.position = Position::Sticky;
  sticky_style.top = Some(Length::px(0.0));
  sticky_style.height = Some(Length::px(20.0));
  let sticky = BoxNode::new_block(Arc::new(sticky_style), FormattingContextType::Block, vec![]);

  let mut tail_style = ComputedStyle::default();
  tail_style.height = Some(Length::px(40.0));
  let tail = BoxNode::new_block(Arc::new(tail_style), FormattingContextType::Block, vec![]);

  let root = BoxNode::new_block(
    Arc::new(ComputedStyle::default()),
    FormattingContextType::Block,
    vec![spacer, sticky, tail],
  );
  let box_tree = BoxTree::new(root);

  let engine = LayoutEngine::new(LayoutConfig::for_pagination(Size::new(100.0, 100.0), 0.0));
  let mut tree = engine.layout_tree(&box_tree).expect("layout tree");

  let (before_pos, fragment_index, _sticky_fragment) =
    sticky_abs_position(&tree).expect("sticky fragment present");
  assert!(
    fragment_index > 0,
    "sticky element should live in an additional fragment"
  );

  let scroll_y = before_pos.y + 10.0;

  let renderer = FastRender::new().expect("renderer");
  renderer.apply_sticky_offsets_for_tree(&mut tree, Point::new(0.0, scroll_y));

  let (after_pos, after_fragment_index, _) =
    sticky_abs_position(&tree).expect("sticky fragment after offsets");
  assert_eq!(fragment_index, after_fragment_index);
  assert!(
    after_pos.y != before_pos.y,
    "sticky fragment in additional fragment should be repositioned after applying offsets"
  );
}

fn sticky_abs_position<'a>(tree: &'a FragmentTree) -> Option<(Point, usize, &'a FragmentNode)> {
  let mut roots = Vec::new();
  roots.push((&tree.root, 0usize));
  for (idx, root) in tree.additional_fragments.iter().enumerate() {
    roots.push((root, idx + 1));
  }

  for (root, idx) in roots {
    if let Some((pos, node)) = find_sticky(root) {
      return Some((pos, idx, node));
    }
  }

  None
}

fn find_sticky<'a>(node: &'a FragmentNode) -> Option<(Point, &'a FragmentNode)> {
  let is_sticky = node
    .style
    .as_ref()
    .map(|s| s.position.is_sticky())
    .unwrap_or(false);

  let abs_pos = Point::new(node.bounds.x(), node.bounds.y());
  if is_sticky {
    return Some((abs_pos, node));
  }

  for child in &node.children {
    if let Some(found) = find_sticky(child) {
      return Some(found);
    }
  }

  None
}

#[test]
fn column_fragmentation_uses_column_width_for_layout() {
  let text = "Wrap this text inside columns";
  let text_node = BoxNode::new_text(Arc::new(ComputedStyle::default()), text.to_string());
  let root = BoxNode::new_block(
    Arc::new(ComputedStyle::default()),
    FormattingContextType::Block,
    vec![text_node],
  );
  let box_tree = BoxTree::new(root);

  let viewport = Size::new(320.0, 400.0);
  let base_engine = LayoutEngine::new(LayoutConfig::for_viewport(viewport));
  let base_tree = base_engine
    .layout_tree(&box_tree)
    .expect("layout without fragmentation");
  let base_lines = collect_lines(&base_tree.root);
  assert_eq!(
    base_lines.len(),
    1,
    "text should fit on one line without column fragmentation"
  );

  let fragmentation = FragmentationOptions::new(400.0).with_columns(2, 20.0);
  let engine =
    LayoutEngine::new(LayoutConfig::for_viewport(viewport).with_fragmentation(fragmentation));
  let tree = engine.layout_tree(&box_tree).expect("layout with columns");

  let expected_column_width = (viewport.width - 20.0) / 2.0;
  assert!((tree.root.bounds.width() - expected_column_width).abs() < 0.1);
  assert_eq!(tree.viewport_size().width, viewport.width);

  let column_lines = collect_lines(&tree.root);
  assert!(
    column_lines.len() > 1,
    "text should wrap when constrained to column width"
  );
}

#[test]
fn break_before_column_only_applies_in_column_context() {
  let lead = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 10.0), vec![]);
  let mut breaker_style = ComputedStyle::default();
  breaker_style.break_before = BreakBetween::Column;
  let breaker = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 10.0, 50.0, 10.0),
    vec![],
    Arc::new(breaker_style),
  );
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 20.0), vec![lead, breaker]);

  let page_boundaries =
    resolve_fragmentation_boundaries_with_context(&root, 100.0, FragmentationContext::Page);
  assert_eq!(
    page_boundaries.len(),
    2,
    "page context ignores column breaks"
  );

  let column_boundaries =
    resolve_fragmentation_boundaries_with_context(&root, 100.0, FragmentationContext::Column);
  assert!(
    column_boundaries.len() > 2,
    "column context should honor column-forced breaks"
  );
  assert!(
    column_boundaries
      .iter()
      .any(|pos| (*pos - 10.0).abs() < 0.1),
    "break should align with the second block's start"
  );
}

#[test]
fn table_headers_repeat_across_fragments() {
  let make = |display: Display, bounds: Rect, children: Vec<FragmentNode>| {
    let mut style = ComputedStyle::default();
    style.display = display;
    FragmentNode::new_block_styled(bounds, children, Arc::new(style))
  };

  let header_cell = make(
    Display::TableCell,
    Rect::from_xywh(0.0, 0.0, 100.0, 12.0),
    vec![],
  );
  let header_row = make(
    Display::TableRow,
    Rect::from_xywh(0.0, 0.0, 100.0, 12.0),
    vec![header_cell],
  );
  let header_group = make(
    Display::TableHeaderGroup,
    Rect::from_xywh(0.0, 0.0, 100.0, 12.0),
    vec![header_row],
  );

  let mut rows = Vec::new();
  let mut y = 12.0;
  for _ in 0..6 {
    let cell = make(
      Display::TableCell,
      Rect::from_xywh(0.0, 0.0, 100.0, 12.0),
      vec![],
    );
    let row = make(
      Display::TableRow,
      Rect::from_xywh(0.0, 0.0, 100.0, 12.0),
      vec![cell],
    );
    let row_group = make(
      Display::TableRowGroup,
      Rect::from_xywh(0.0, y, 100.0, 12.0),
      vec![row],
    );
    rows.push(row_group);
    y += 12.0;
  }

  let mut table_style = ComputedStyle::default();
  table_style.display = Display::Table;
  let mut table_children = Vec::new();
  table_children.push(header_group.clone());
  table_children.extend(rows);
  let table = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 100.0, y),
    table_children,
    Arc::new(table_style),
  );
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, y), vec![table]);

  let fragments = fragment_tree(&root, &FragmentationOptions::new(36.0));
  assert!(fragments.len() >= 2, "table should fragment across pages");

  for fragment in &fragments {
    let header_count = fragment
      .iter_fragments()
      .filter(|node| {
        node
          .style
          .as_ref()
          .is_some_and(|style| matches!(style.display, Display::TableHeaderGroup))
      })
      .count();
    assert!(
      header_count >= 1,
      "each fragment should receive a repeated table header"
    );
  }
}
