use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::grid::GridFormattingContext;
use fastrender::style::display::Display;
use fastrender::style::types::GridTrack;
use fastrender::style::types::WritingMode;
use fastrender::style::values::Length;
use fastrender::BoxNode;
use fastrender::ComputedStyle;
use fastrender::FormattingContext;
use fastrender::FormattingContextType;
use std::collections::HashMap;
use std::sync::Arc;

fn assert_approx(val: f32, expected: f32, msg: &str) {
  assert!((val - expected).abs() <= 0.5, "{}: got {} expected {}", msg, val, expected);
}

#[test]
fn row_subgrid_uses_parent_tracks() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_rows = vec![GridTrack::Auto, GridTrack::Auto];
  parent_style.grid_template_columns = vec![GridTrack::Auto];
  parent_style.width = Some(Length::px(200.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_row_subgrid = true;
  subgrid_style.grid_row_start = 1;
  subgrid_style.grid_row_end = 3;

  let mut child1_style = ComputedStyle::default();
  child1_style.display = Display::Block;
  child1_style.height = Some(Length::px(10.0));
  child1_style.grid_row_start = 1;
  child1_style.grid_row_end = 2;

  let mut child2_style = ComputedStyle::default();
  child2_style.display = Display::Block;
  child2_style.height = Some(Length::px(50.0));
  child2_style.grid_row_start = 2;
  child2_style.grid_row_end = 3;

  let child1 = BoxNode::new_block(
    Arc::new(child1_style),
    FormattingContextType::Block,
    vec![],
  );
  let child2 = BoxNode::new_block(
    Arc::new(child2_style),
    FormattingContextType::Block,
    vec![],
  );

  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![child1, child2],
  );

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite_width(200.0))
    .expect("layout succeeds");

  assert_eq!(fragment.children.len(), 1);
  let subgrid_fragment = &fragment.children[0];
  assert_eq!(subgrid_fragment.children.len(), 2);
  let first = &subgrid_fragment.children[0];
  let second = &subgrid_fragment.children[1];

  assert_approx(first.bounds.y(), 0.0, "first row starts at 0");
  assert_approx(first.bounds.height(), 10.0, "first row height");
  assert_approx(second.bounds.y(), 10.0, "second row offset matches first height");
  assert_approx(second.bounds.height(), 50.0, "second row height");
}

#[test]
fn column_subgrid_aligns_with_parent_tracks() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![GridTrack::Length(Length::px(40.0)), GridTrack::Length(Length::px(60.0))];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.width = Some(Length::px(100.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut col_child1 = ComputedStyle::default();
  col_child1.display = Display::Block;
  col_child1.height = Some(Length::px(10.0));

  let mut col_child2 = ComputedStyle::default();
  col_child2.display = Display::Block;
  col_child2.height = Some(Length::px(10.0));

  let sub_child1 = BoxNode::new_block(Arc::new(col_child1), FormattingContextType::Block, vec![]);
  let sub_child2 = BoxNode::new_block(Arc::new(col_child2), FormattingContextType::Block, vec![]);

  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![sub_child1, sub_child2],
  );

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(200.0, 200.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  assert_eq!(subgrid_fragment.children.len(), 2);
  let first = &subgrid_fragment.children[0];
  let second = &subgrid_fragment.children[1];

  assert_approx(first.bounds.x(), 0.0, "first column origin");
  assert_approx(first.bounds.width(), 40.0, "first column width");
  assert_approx(second.bounds.x(), 40.0, "second column offset");
  assert_approx(second.bounds.width(), 60.0, "second column width");
}

#[test]
fn subgrid_autoplacement_uses_parent_rows_and_gaps() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_rows = vec![GridTrack::Auto, GridTrack::Auto];
  parent_style.grid_template_columns = vec![GridTrack::Auto];
  parent_style.grid_row_gap = Length::px(5.0);
  parent_style.width = Some(Length::px(200.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_row_subgrid = true;
  subgrid_style.grid_row_start = 1;
  subgrid_style.grid_row_end = 3;

  let mut child1_style = ComputedStyle::default();
  child1_style.display = Display::Block;
  child1_style.height = Some(Length::px(15.0));

  let mut child2_style = ComputedStyle::default();
  child2_style.display = Display::Block;
  child2_style.height = Some(Length::px(25.0));

  let child1 = BoxNode::new_block(Arc::new(child1_style), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(child2_style), FormattingContextType::Block, vec![]);

  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![child1, child2],
  );

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(200.0, 200.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  assert_eq!(subgrid_fragment.children.len(), 2);
  let first = &subgrid_fragment.children[0];
  let second = &subgrid_fragment.children[1];

  assert_approx(first.bounds.y(), 0.0, "first row origin");
  assert_approx(first.bounds.height(), 15.0, "first row size");
  assert_approx(second.bounds.y(), 20.0, "second row offset includes gap");
  assert_approx(second.bounds.height(), 25.0, "second row size");
}

#[test]
fn column_subgrid_inherits_gaps_for_autoplacement() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![GridTrack::Length(Length::px(30.0)), GridTrack::Length(Length::px(40.0))];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.grid_column_gap = Length::px(5.0);
  parent_style.width = Some(Length::px(200.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut child1_style = ComputedStyle::default();
  child1_style.display = Display::Block;
  child1_style.height = Some(Length::px(10.0));

  let mut child2_style = ComputedStyle::default();
  child2_style.display = Display::Block;
  child2_style.height = Some(Length::px(10.0));

  let child1 = BoxNode::new_block(Arc::new(child1_style), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(child2_style), FormattingContextType::Block, vec![]);

  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![child1, child2],
  );

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(200.0, 200.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  let first = &subgrid_fragment.children[0];
  let second = &subgrid_fragment.children[1];

  assert_approx(first.bounds.x(), 0.0, "first column origin");
  assert_approx(first.bounds.width(), 30.0, "first column width");
  assert_approx(second.bounds.x(), 35.0, "second column offset includes gap");
  assert_approx(second.bounds.width(), 40.0, "second column width");
}

#[test]
fn nested_subgrid_propagates_descendant_sizes() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_rows = vec![
    GridTrack::Length(Length::px(18.0)),
    GridTrack::Length(Length::px(26.0)),
  ];
  parent_style.grid_template_columns = vec![GridTrack::Auto];
  parent_style.width = Some(Length::px(200.0));

  let mut outer_subgrid_style = ComputedStyle::default();
  outer_subgrid_style.display = Display::Grid;
  outer_subgrid_style.grid_row_subgrid = true;
  outer_subgrid_style.grid_row_start = 1;
  outer_subgrid_style.grid_row_end = 3;

  let mut inner_subgrid_style = ComputedStyle::default();
  inner_subgrid_style.display = Display::Grid;
  inner_subgrid_style.grid_row_subgrid = true;
  inner_subgrid_style.grid_row_start = 1;
  inner_subgrid_style.grid_row_end = 3;

  let mut inner_child1 = ComputedStyle::default();
  inner_child1.display = Display::Block;
  inner_child1.height = Some(Length::px(5.0));

  let mut inner_child2 = ComputedStyle::default();
  inner_child2.display = Display::Block;
  inner_child2.height = Some(Length::px(5.0));

  let grandchild1 = BoxNode::new_block(Arc::new(inner_child1), FormattingContextType::Block, vec![]);
  let grandchild2 = BoxNode::new_block(Arc::new(inner_child2), FormattingContextType::Block, vec![]);

  let inner_subgrid = BoxNode::new_block(
    Arc::new(inner_subgrid_style),
    FormattingContextType::Grid,
    vec![grandchild1, grandchild2],
  );

  let outer_subgrid = BoxNode::new_block(
    Arc::new(outer_subgrid_style),
    FormattingContextType::Grid,
    vec![inner_subgrid],
  );

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![outer_subgrid],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(200.0, 200.0))
    .expect("layout succeeds");

  let outer_fragment = &fragment.children[0];
  let inner_fragment = &outer_fragment.children[0];
  assert_eq!(inner_fragment.children.len(), 2);

  let first = &inner_fragment.children[0];
  let second = &inner_fragment.children[1];

  assert_approx(first.bounds.y(), 0.0, "first nested row origin");
  assert_approx(second.bounds.y(), 18.0, "second nested row offset");
  assert!(first.bounds.height() <= 18.1, "first item fits within inherited track");
  assert!(second.bounds.height() <= 26.1, "second item fits within inherited track");
}

#[test]
fn subgrid_extends_named_lines() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![GridTrack::Length(Length::px(20.0)), GridTrack::Length(Length::px(30.0))];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.width = Some(Length::px(200.0));
  parent_style.grid_column_line_names = vec![vec!["a".into()], Vec::new(), vec!["b".into()]];
  let mut names: HashMap<String, Vec<usize>> = HashMap::new();
  names.insert("a".into(), vec![0]);
  names.insert("b".into(), vec![2]);
  parent_style.grid_column_names = names;

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;
  subgrid_style.subgrid_column_line_names = vec![
    vec!["a".into(), "sub-start".into()],
    vec!["c".into()],
    vec!["b".into()],
  ];
  subgrid_style.grid_column_line_names = subgrid_style.subgrid_column_line_names.clone();

  let mut child1_style = ComputedStyle::default();
  child1_style.display = Display::Block;
  child1_style.height = Some(Length::px(10.0));
  child1_style.grid_column_raw = Some("a / c".into());

  let mut child2_style = ComputedStyle::default();
  child2_style.display = Display::Block;
  child2_style.height = Some(Length::px(10.0));
  child2_style.grid_column_raw = Some("c / b".into());

  let child1 = BoxNode::new_block(Arc::new(child1_style), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(child2_style), FormattingContextType::Block, vec![]);

  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![child1, child2],
  );

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(200.0, 200.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  let first = &subgrid_fragment.children[0];
  let second = &subgrid_fragment.children[1];

  assert_approx(first.bounds.x(), 0.0, "named start aligns to first line");
  assert_approx(first.bounds.width(), 20.0, "span to c covers first track");
  assert_approx(second.bounds.x(), 20.0, "c starts after first track");
  assert_approx(second.bounds.width(), 30.0, "c to b spans second track");
}

#[test]
fn column_subgrid_respects_vertical_writing_mode() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.writing_mode = WritingMode::VerticalRl;
  parent_style.grid_template_columns = vec![GridTrack::Length(Length::px(30.0)), GridTrack::Length(Length::px(40.0))];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.width = Some(Length::px(100.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.writing_mode = WritingMode::VerticalRl;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut child1_style = ComputedStyle::default();
  child1_style.display = Display::Block;
  child1_style.height = Some(Length::px(5.0));
  child1_style.grid_column_start = 1;
  child1_style.grid_column_end = 2;
  child1_style.grid_row_start = 1;
  child1_style.grid_row_end = 2;

  let mut child2_style = ComputedStyle::default();
  child2_style.display = Display::Block;
  child2_style.height = Some(Length::px(5.0));
  child2_style.grid_column_start = 2;
  child2_style.grid_column_end = 3;
  child2_style.grid_row_start = 1;
  child2_style.grid_row_end = 2;

  let child1 = BoxNode::new_block(Arc::new(child1_style), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(child2_style), FormattingContextType::Block, vec![]);

  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![child1, child2],
  );

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(200.0, 200.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  let first = &subgrid_fragment.children[0];
  let second = &subgrid_fragment.children[1];

  assert_approx(first.bounds.y(), 0.0, "first column maps to top in vertical mode");
  assert_approx(second.bounds.y(), 30.0, "second column starts after first track");
  assert_approx(first.bounds.height(), 5.0, "item keeps intrinsic height in column track");
  assert_approx(second.bounds.height(), 5.0, "item keeps intrinsic height in column track");
}
