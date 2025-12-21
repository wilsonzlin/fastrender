use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::grid::GridFormattingContext;
use fastrender::style::display::Display;
use fastrender::style::types::GridTrack;
use fastrender::style::values::Length;
use fastrender::tree::fragment_tree::FragmentContent;
use fastrender::tree::fragment_tree::FragmentNode;
use fastrender::BoxNode;
use fastrender::ComputedStyle;
use fastrender::FormattingContext;
use fastrender::FormattingContextType;
use std::sync::Arc;

fn grid_container(children: Vec<BoxNode>) -> BoxNode {
  let mut style = ComputedStyle::default();
  style.display = Display::Grid;
  style.width = Some(Length::px(200.0));
  style.height = Some(Length::px(100.0));
  style.grid_template_columns = vec![GridTrack::Fr(1.0)];

  BoxNode::new_block(Arc::new(style), FormattingContextType::Grid, children)
}

fn grid_container_with_padding(
  children: Vec<BoxNode>,
  padding_top: f32,
  padding_bottom: f32,
) -> BoxNode {
  let mut style = ComputedStyle::default();
  style.display = Display::Grid;
  style.width = Some(Length::px(200.0));
  style.height = Some(Length::px(100.0));
  style.grid_template_columns = vec![GridTrack::Fr(1.0)];
  style.padding_top = Length::px(padding_top);
  style.padding_bottom = Length::px(padding_bottom);

  BoxNode::new_block(Arc::new(style), FormattingContextType::Grid, children)
}

fn grid_item() -> BoxNode {
  let mut style = ComputedStyle::default();
  style.display = Display::Block;
  style.width = Some(Length::px(50.0));
  style.height = Some(Length::px(10.0));
  // Auto inline margins should absorb remaining inline space and center the item.
  style.margin_left = None;
  style.margin_right = None;

  BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![])
}

fn grid_item_with_block_margins() -> BoxNode {
  let mut style = ComputedStyle::default();
  style.display = Display::Block;
  style.width = Some(Length::px(50.0));
  style.height = Some(Length::px(20.0));
  // Auto block margins should absorb remaining block space and center the item vertically.
  style.margin_top = None;
  style.margin_bottom = None;

  BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![])
}

fn count_text_fragments(fragment: &FragmentNode) -> usize {
  let mut count = 0usize;
  let mut stack = vec![fragment];
  while let Some(node) = stack.pop() {
    if matches!(node.content, FragmentContent::Text { .. }) {
      count += 1;
    }
    stack.extend(node.children.iter());
  }
  count
}

#[test]
fn grid_item_auto_margins_center_item() {
  let container = grid_container(vec![grid_item()]);
  let fc = GridFormattingContext::new();

  let fragment = fc
    .layout(&container, &LayoutConstraints::definite(200.0, 100.0))
    .expect("layout succeeds");

  assert_eq!(fragment.children.len(), 1);
  let child = &fragment.children[0];

  // With a 200px container, 50px item, and auto inline margins, the item should be centered.
  assert!(
    (child.bounds.x() - 75.0).abs() <= 0.5,
    "expected centered item, got x={}",
    child.bounds.x()
  );
  assert_eq!(child.bounds.width(), 50.0);
}

#[test]
fn grid_item_auto_block_margins_center_vertically() {
  let container = grid_container_with_padding(vec![grid_item_with_block_margins()], 10.0, 10.0);

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&container, &LayoutConstraints::definite(200.0, 100.0))
    .expect("layout succeeds");

  let child = &fragment.children[0];
  // Content height is 100 (content-box) with 10px padding on each side, so the padding pushes the child down.
  // With a 20px tall item and auto block margins, it should center in the 100px content and start around y=50.
  assert!(
    (child.bounds.y() - 50.0).abs() <= 0.5,
    "expected vertically centered item, got y={}",
    child.bounds.y()
  );
  assert_eq!(child.bounds.height(), 20.0);
}

#[test]
fn grid_children_are_laid_out_even_when_taffy_reports_zero_width() {
  // Reproduce the w3.org grid root where Taffy returns zero-width children when
  // it has no intrinsic measurement for grid items: a grid container with auto width
  // and a child block containing inline text. The child must still be laid out using
  // its own formatting context so text fragments appear.
  let mut grid_style = ComputedStyle::default();
  grid_style.display = Display::Grid;
  grid_style.min_height = Some(Length::new(
    100.0,
    fastrender::style::values::LengthUnit::Vh,
  ));
  let text_style = Arc::new(ComputedStyle::default());
  let text = BoxNode::new_text(text_style.clone(), "Grid child text".to_string());
  let child = BoxNode::new_block(
    Arc::new(ComputedStyle::default()),
    FormattingContextType::Block,
    vec![text],
  );
  let grid = BoxNode::new_block(Arc::new(grid_style), FormattingContextType::Grid, vec![
    child,
  ]);

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(300.0, 400.0))
    .expect("layout succeeds");

  assert_eq!(
    fragment.children.len(),
    1,
    "grid should produce a child fragment"
  );
  let child_frag = &fragment.children[0];

  // The child should occupy space in the grid instead of collapsing to zero width, and
  // its subtree should contain the laid-out text fragments.
  assert!(child_frag.bounds.width() > 0.0);
  assert!(
    count_text_fragments(child_frag) > 0,
    "expected text fragments inside grid child"
  );
}

#[test]
fn nested_grid_items_preserve_measured_children() {
  // Grid items are treated as leaves in the Taffy tree; ensure that when a grid item itself
  // establishes a grid formatting context, its measured fragment (with children) is reused
  // instead of producing an empty fragment tree.
  let mut outer_style = ComputedStyle::default();
  outer_style.display = Display::Grid;
  outer_style.grid_auto_flow = fastrender::style::types::GridAutoFlow::Column;
  outer_style.grid_auto_columns = vec![GridTrack::Fr(1.0)];

  let mut inner_style = ComputedStyle::default();
  inner_style.display = Display::Grid;
  inner_style.grid_auto_flow = fastrender::style::types::GridAutoFlow::Row;
  inner_style.grid_auto_rows = vec![GridTrack::Fr(1.0)];

  let text_style = Arc::new(ComputedStyle::default());
  let text = BoxNode::new_text(text_style, "Nested grid child".to_string());
  let inner_block = BoxNode::new_block(
    Arc::new(ComputedStyle::default()),
    FormattingContextType::Block,
    vec![text],
  );
  let inner_grid = BoxNode::new_block(Arc::new(inner_style), FormattingContextType::Grid, vec![
    inner_block,
  ]);
  let outer_grid = BoxNode::new_block(Arc::new(outer_style), FormattingContextType::Grid, vec![
    inner_grid,
  ]);

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&outer_grid, &LayoutConstraints::definite(400.0, 200.0))
    .expect("layout succeeds");

  assert_eq!(
    fragment.children.len(),
    1,
    "outer grid should have one item fragment"
  );
  let inner_fragment = &fragment.children[0];

  assert!(
    !inner_fragment.children.is_empty(),
    "nested grid fragment should preserve its laid-out children"
  );
  assert!(
    count_text_fragments(inner_fragment) > 0,
    "expected text content inside nested grid fragment"
  );
}
