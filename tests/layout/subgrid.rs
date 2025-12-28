use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::grid::GridFormattingContext;
use fastrender::layout::formatting_context::IntrinsicSizingMode;
use fastrender::style::display::Display;
use fastrender::style::types::AlignItems;
use fastrender::style::types::Direction;
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
  assert!(
    (val - expected).abs() <= 0.5,
    "{}: got {} expected {}",
    msg,
    val,
    expected
  );
}

fn synthesize_area_line_names(style: &mut ComputedStyle) {
  if style.grid_template_areas.is_empty() {
    return;
  }
  if let Some(bounds) =
    fastrender::style::grid::validate_area_rectangles(&style.grid_template_areas)
  {
    let ensure_line = |lines: &mut Vec<Vec<String>>,
                       names: &mut HashMap<String, Vec<usize>>,
                       idx: usize,
                       name: String| {
      if lines.len() <= idx {
        lines.resize(idx + 1, Vec::new());
      }
      if !lines[idx].contains(&name) {
        lines[idx].push(name.clone());
        names.entry(name).or_default().push(idx);
      }
    };

    if style.grid_column_line_names.len() < style.grid_template_columns.len() + 1 {
      style
        .grid_column_line_names
        .resize(style.grid_template_columns.len() + 1, Vec::new());
    }
    if style.grid_row_line_names.len() < style.grid_template_rows.len() + 1 {
      style
        .grid_row_line_names
        .resize(style.grid_template_rows.len() + 1, Vec::new());
    }

    for (name, (top, bottom, left, right)) in bounds {
      let col_start = left;
      let col_end = right + 1;
      let row_start = top;
      let row_end = bottom + 1;

      ensure_line(
        &mut style.grid_column_line_names,
        &mut style.grid_column_names,
        col_start,
        format!("{name}-start"),
      );
      ensure_line(
        &mut style.grid_column_line_names,
        &mut style.grid_column_names,
        col_end,
        format!("{name}-end"),
      );
      ensure_line(
        &mut style.grid_row_line_names,
        &mut style.grid_row_names,
        row_start,
        format!("{name}-start"),
      );
      ensure_line(
        &mut style.grid_row_line_names,
        &mut style.grid_row_names,
        row_end,
        format!("{name}-end"),
      );
    }
  }
}

#[test]
fn subgrid_contributes_to_parent_row_track_sizing() {
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

  let inner1 = BoxNode::new_block(Arc::new(child1_style), FormattingContextType::Block, vec![]);
  let inner2 = BoxNode::new_block(Arc::new(child2_style), FormattingContextType::Block, vec![]);

  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![inner1, inner2],
  );

  let mut sibling_style = ComputedStyle::default();
  sibling_style.display = Display::Block;
  sibling_style.height = Some(Length::px(5.0));
  sibling_style.grid_row_start = 2;
  sibling_style.grid_row_end = 3;
  let sibling = BoxNode::new_block(
    Arc::new(sibling_style),
    FormattingContextType::Block,
    vec![],
  );

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid, sibling],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(200.0, 200.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  let sibling_fragment = &fragment.children[1];
  let inner_second = &subgrid_fragment.children[1];

  assert_approx(sibling_fragment.bounds.y(), 10.0, "second row start");
  assert_approx(
    sibling_fragment.bounds.y(),
    inner_second.bounds.y(),
    "parent row line matches subgrid row",
  );
}

#[test]
fn subgrid_contributes_to_parent_column_track_sizing() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![GridTrack::Auto, GridTrack::Auto];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.width = Some(Length::px(200.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut col1_style = ComputedStyle::default();
  col1_style.display = Display::Block;
  col1_style.width = Some(Length::px(20.0));
  col1_style.grid_column_start = 1;
  col1_style.grid_column_end = 2;

  let mut col2_style = ComputedStyle::default();
  col2_style.display = Display::Block;
  col2_style.width = Some(Length::px(60.0));
  col2_style.grid_column_start = 2;
  col2_style.grid_column_end = 3;

  let inner1 = BoxNode::new_block(Arc::new(col1_style), FormattingContextType::Block, vec![]);
  let inner2 = BoxNode::new_block(Arc::new(col2_style), FormattingContextType::Block, vec![]);

  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![inner1, inner2],
  );

  let mut sibling_style = ComputedStyle::default();
  sibling_style.display = Display::Block;
  sibling_style.width = Some(Length::px(5.0));
  sibling_style.grid_column_start = 2;
  sibling_style.grid_column_end = 3;
  let sibling = BoxNode::new_block(
    Arc::new(sibling_style),
    FormattingContextType::Block,
    vec![],
  );

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid, sibling],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(200.0, 200.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  let sibling_fragment = &fragment.children[1];
  let inner_second = &subgrid_fragment.children[1];

  assert_approx(sibling_fragment.bounds.x(), 20.0, "second column start");
  assert_approx(
    sibling_fragment.bounds.x(),
    inner_second.bounds.x(),
    "parent column line matches subgrid column",
  );
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
    .layout(&grid, &LayoutConstraints::definite_width(200.0))
    .expect("layout succeeds");

  assert_eq!(fragment.children.len(), 1);
  let subgrid_fragment = &fragment.children[0];
  assert_eq!(subgrid_fragment.children.len(), 2);
  let first = &subgrid_fragment.children[0];
  let second = &subgrid_fragment.children[1];

  assert_approx(first.bounds.y(), 0.0, "first row starts at 0");
  assert_approx(first.bounds.height(), 10.0, "first row height");
  assert_approx(
    second.bounds.y(),
    10.0,
    "second row offset matches first height",
  );
  assert_approx(second.bounds.height(), 50.0, "second row height");
}

#[test]
fn column_subgrid_aligns_with_parent_tracks() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(40.0)),
    GridTrack::Length(Length::px(60.0)),
  ];
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
  eprintln!(
    "subgrid origin x={} child0 x={} child1 x={}",
    subgrid_fragment.bounds.x(),
    subgrid_fragment.children[0].bounds.x(),
    subgrid_fragment.children[1].bounds.x()
  );
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
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(30.0)),
    GridTrack::Length(Length::px(40.0)),
  ];
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
fn column_subgrid_with_mismatched_writing_mode_inherits_gaps() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(24.0)),
    GridTrack::Length(Length::px(36.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.grid_column_gap = Length::px(7.0);
  parent_style.width = Some(Length::px(120.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.writing_mode = WritingMode::VerticalRl;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut child1_style = ComputedStyle::default();
  child1_style.display = Display::Block;
  child1_style.height = Some(Length::px(6.0));

  let mut child2_style = ComputedStyle::default();
  child2_style.display = Display::Block;
  child2_style.height = Some(Length::px(6.0));

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
  assert_approx(
    first.bounds.x(),
    0.0,
    "first column origin with inherited tracks",
  );
  assert_approx(first.bounds.width(), 24.0, "first column width preserved");
  assert_approx(
    second.bounds.x(),
    31.0,
    "second column offset keeps parent gap",
  );
  assert_approx(second.bounds.width(), 36.0, "second column width preserved");
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

  let grandchild1 =
    BoxNode::new_block(Arc::new(inner_child1), FormattingContextType::Block, vec![]);
  let grandchild2 =
    BoxNode::new_block(Arc::new(inner_child2), FormattingContextType::Block, vec![]);

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
  assert!(
    first.bounds.height() <= 18.1,
    "first item fits within inherited track"
  );
  assert!(
    second.bounds.height() <= 26.1,
    "second item fits within inherited track"
  );
}

#[test]
fn nested_column_subgrid_respects_inherited_axes_overrides() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(28.0)),
    GridTrack::Length(Length::px(42.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.grid_column_gap = Length::px(5.0);
  parent_style.width = Some(Length::px(75.0));

  let mut outer_subgrid_style = ComputedStyle::default();
  outer_subgrid_style.display = Display::Grid;
  outer_subgrid_style.writing_mode = WritingMode::VerticalRl;
  outer_subgrid_style.grid_column_subgrid = true;
  outer_subgrid_style.grid_column_start = 1;
  outer_subgrid_style.grid_column_end = 3;

  let mut inner_subgrid_style = ComputedStyle::default();
  inner_subgrid_style.display = Display::Grid;
  inner_subgrid_style.writing_mode = WritingMode::VerticalRl;
  inner_subgrid_style.grid_column_subgrid = true;
  inner_subgrid_style.grid_column_start = 1;
  inner_subgrid_style.grid_column_end = 3;

  let mut first_child = ComputedStyle::default();
  first_child.display = Display::Block;
  first_child.grid_column_start = 1;
  first_child.grid_column_end = 2;
  first_child.height = Some(Length::px(6.0));

  let mut second_child = ComputedStyle::default();
  second_child.display = Display::Block;
  second_child.grid_column_start = 2;
  second_child.grid_column_end = 3;
  second_child.height = Some(Length::px(6.0));

  let child1 = BoxNode::new_block(Arc::new(first_child), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(second_child), FormattingContextType::Block, vec![]);

  let inner_subgrid = BoxNode::new_block(
    Arc::new(inner_subgrid_style),
    FormattingContextType::Grid,
    vec![child1, child2],
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
  assert_approx(first.bounds.x(), 0.0, "first track starts at origin");
  assert_approx(
    first.bounds.width(),
    28.0,
    "first track inherits parent sizing",
  );
  assert_approx(
    second.bounds.x(),
    33.0,
    "gap and first track size carry through nested subgrid",
  );
  assert_approx(
    second.bounds.width(),
    42.0,
    "second track inherits parent sizing",
  );
}

#[test]
fn subgrid_extends_named_lines() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(20.0)),
    GridTrack::Length(Length::px(30.0)),
  ];
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
fn subgrid_inherits_area_generated_line_names() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(30.0)),
    GridTrack::Length(Length::px(50.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto, GridTrack::Auto];
  parent_style.grid_template_areas = vec![
    vec![Some("nav".into()), Some("main".into())],
    vec![Some("nav".into()), Some("main".into())],
  ];
  parent_style.width = Some(Length::px(80.0));

  synthesize_area_line_names(&mut parent_style);

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut nav_child = ComputedStyle::default();
  nav_child.display = Display::Block;
  nav_child.height = Some(Length::px(10.0));
  nav_child.grid_column_raw = Some("nav-start / nav-end".into());
  nav_child.grid_row_start = 1;
  nav_child.grid_row_end = 2;

  let mut main_child = ComputedStyle::default();
  main_child.display = Display::Block;
  main_child.height = Some(Length::px(10.0));
  main_child.grid_column_raw = Some("main-start / main-end".into());
  main_child.grid_row_start = 2;
  main_child.grid_row_end = 3;

  let nav = BoxNode::new_block(Arc::new(nav_child), FormattingContextType::Block, vec![]);
  let main = BoxNode::new_block(Arc::new(main_child), FormattingContextType::Block, vec![]);

  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![nav, main],
  );

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(120.0, 120.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  assert_approx(
    subgrid_fragment.children[0].bounds.x(),
    0.0,
    "nav starts on first track",
  );
  assert_approx(
    subgrid_fragment.children[0].bounds.width(),
    30.0,
    "nav spans nav column",
  );
  assert_approx(
    subgrid_fragment.children[1].bounds.x(),
    30.0,
    "main begins after nav end",
  );
  assert_approx(
    subgrid_fragment.children[1].bounds.width(),
    50.0,
    "main spans second column",
  );
}

#[test]
fn column_subgrid_respects_vertical_writing_mode() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.writing_mode = WritingMode::VerticalRl;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(30.0)),
    GridTrack::Length(Length::px(40.0)),
  ];
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

  assert_approx(
    first.bounds.y(),
    0.0,
    "first column maps to top in vertical mode",
  );
  assert_approx(
    second.bounds.y(),
    30.0,
    "second column starts after first track",
  );
  assert_approx(
    first.bounds.height(),
    5.0,
    "item keeps intrinsic height in column track",
  );
  assert_approx(
    second.bounds.height(),
    5.0,
    "item keeps intrinsic height in column track",
  );
}

#[test]
fn subgrid_tracks_follow_parent_axes_when_writing_mode_differs() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(32.0)),
    GridTrack::Length(Length::px(48.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.grid_column_gap = Length::px(8.0);
  parent_style.width = Some(Length::px(88.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;
  // Flip writing mode to ensure axes come from the parent grid, not the subgrid's own mode.
  subgrid_style.writing_mode = WritingMode::VerticalRl;

  let mut first_child = ComputedStyle::default();
  first_child.display = Display::Block;
  first_child.height = Some(Length::px(12.0));
  first_child.grid_column_start = 1;
  first_child.grid_column_end = 2;

  let mut second_child = ComputedStyle::default();
  second_child.display = Display::Block;
  second_child.height = Some(Length::px(12.0));
  second_child.grid_column_start = 2;
  second_child.grid_column_end = 3;

  let child1 = BoxNode::new_block(Arc::new(first_child), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(second_child), FormattingContextType::Block, vec![]);

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
  assert_approx(
    subgrid_fragment.children[0].bounds.x(),
    0.0,
    "first column stays on parent's start edge",
  );
  assert_approx(
    subgrid_fragment.children[0].bounds.width(),
    32.0,
    "first column inherits parent sizing",
  );
  assert_approx(
    subgrid_fragment.children[1].bounds.x(),
    40.0,
    "second column offset includes parent gap",
  );
  assert_approx(
    subgrid_fragment.children[1].bounds.width(),
    48.0,
    "second column inherits parent sizing",
  );
}

#[test]
fn row_subgrid_inherits_block_axis_from_vertical_parent() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.writing_mode = WritingMode::VerticalRl;
  parent_style.grid_template_columns = vec![GridTrack::Auto];
  parent_style.grid_template_rows = vec![
    GridTrack::Length(Length::px(40.0)),
    GridTrack::Length(Length::px(60.0)),
  ];
  parent_style.grid_row_gap = Length::px(10.0);
  parent_style.width = Some(Length::px(200.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.writing_mode = WritingMode::HorizontalTb;
  subgrid_style.grid_row_subgrid = true;
  subgrid_style.grid_row_start = 1;
  subgrid_style.grid_row_end = 3;
  subgrid_style.grid_template_columns = vec![GridTrack::Auto];

  let mut first_child = ComputedStyle::default();
  first_child.display = Display::Block;
  first_child.height = Some(Length::px(6.0));
  first_child.grid_row_start = 1;
  first_child.grid_row_end = 2;

  let mut second_child = ComputedStyle::default();
  second_child.display = Display::Block;
  second_child.height = Some(Length::px(6.0));
  second_child.grid_row_start = 2;
  second_child.grid_row_end = 3;

  let child1 = BoxNode::new_block(Arc::new(first_child), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(second_child), FormattingContextType::Block, vec![]);

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
    .layout(&grid, &LayoutConstraints::definite(240.0, 120.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  assert_approx(
    subgrid_fragment.bounds.width(),
    110.0,
    "subgrid inherits row sizing in the block axis",
  );
  assert_eq!(subgrid_fragment.children.len(), 2);
}

#[test]
fn row_subgrid_with_vertical_writing_mode_stacks_inline_tracks_vertically() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![GridTrack::Auto];
  parent_style.grid_template_rows = vec![
    GridTrack::Length(Length::px(40.0)),
    GridTrack::Length(Length::px(60.0)),
  ];
  parent_style.width = Some(Length::px(120.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.writing_mode = WritingMode::VerticalRl;
  subgrid_style.grid_row_subgrid = true;
  subgrid_style.grid_row_start = 1;
  subgrid_style.grid_row_end = 3;
  subgrid_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(15.0)),
    GridTrack::Length(Length::px(25.0)),
  ];

  let mut first_child = ComputedStyle::default();
  first_child.display = Display::Block;
  first_child.grid_column_start = 1;
  first_child.grid_column_end = 2;
  first_child.grid_row_start = 1;
  first_child.grid_row_end = 2;

  let mut second_child = ComputedStyle::default();
  second_child.display = Display::Block;
  second_child.grid_column_start = 2;
  second_child.grid_column_end = 3;
  second_child.grid_row_start = 1;
  second_child.grid_row_end = 2;

  let child1 = BoxNode::new_block(Arc::new(first_child), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(second_child), FormattingContextType::Block, vec![]);

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
    .layout(&grid, &LayoutConstraints::definite(240.0, 200.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  assert_approx(
    subgrid_fragment.children[0].bounds.width(),
    40.0,
    "row inherits width",
  );
  assert_approx(
    subgrid_fragment.children[0].bounds.height(),
    15.0,
    "first inline track height",
  );
  assert_approx(
    subgrid_fragment.children[1].bounds.y() - subgrid_fragment.bounds.y(),
    15.0,
    "second column stacks vertically in inline axis",
  );
  assert_approx(
    subgrid_fragment.children[1].bounds.height(),
    25.0,
    "second inline track height",
  );
}

#[test]
fn subgrid_respects_its_own_justify_items() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(60.0)),
    GridTrack::Length(Length::px(60.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.width = Some(Length::px(120.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;
  subgrid_style.justify_items = AlignItems::Start;

  let mut text_style = ComputedStyle::default();
  text_style.display = Display::Inline;
  text_style.font_size = 14.0;
  let text = BoxNode::new_text(Arc::new(text_style.clone()), "content".into());
  let inline = BoxNode::new_block(
    Arc::new(text_style),
    FormattingContextType::Inline,
    vec![text],
  );

  let mut child_style = ComputedStyle::default();
  child_style.display = Display::Block;
  let child = BoxNode::new_block(
    Arc::new(child_style),
    FormattingContextType::Block,
    vec![inline],
  );

  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![child],
  );

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(160.0, 160.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  assert_eq!(subgrid_fragment.children.len(), 1);
  let child_fragment = &subgrid_fragment.children[0];
  assert!(
    child_fragment.bounds.width() < 60.0,
    "child should shrink to its content instead of stretching to the track"
  );
  assert_approx(
    child_fragment.bounds.x() - subgrid_fragment.bounds.x(),
    0.0,
    "child stays at the start edge under justify-items:start",
  );
}

#[test]
fn subgrid_children_shape_parent_tracks_and_gaps() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![GridTrack::Auto, GridTrack::Auto];
  parent_style.grid_template_rows = vec![GridTrack::Auto, GridTrack::Auto];
  parent_style.grid_column_gap = Length::px(5.0);
  parent_style.grid_row_gap = Length::px(6.0);
  parent_style.width = Some(Length::px(300.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_row_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;
  subgrid_style.grid_row_start = 1;
  subgrid_style.grid_row_end = 3;

  let mut sub_child1 = ComputedStyle::default();
  sub_child1.display = Display::Block;
  sub_child1.width = Some(Length::px(40.0));
  sub_child1.height = Some(Length::px(20.0));
  sub_child1.grid_column_start = 1;
  sub_child1.grid_column_end = 2;
  sub_child1.grid_row_start = 1;
  sub_child1.grid_row_end = 2;

  let mut sub_child2 = ComputedStyle::default();
  sub_child2.display = Display::Block;
  sub_child2.width = Some(Length::px(70.0));
  sub_child2.height = Some(Length::px(30.0));
  sub_child2.grid_column_start = 2;
  sub_child2.grid_column_end = 3;
  sub_child2.grid_row_start = 2;
  sub_child2.grid_row_end = 3;

  let grand1 = BoxNode::new_block(Arc::new(sub_child1), FormattingContextType::Block, vec![]);
  let grand2 = BoxNode::new_block(Arc::new(sub_child2), FormattingContextType::Block, vec![]);

  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![grand1, grand2],
  );

  let mut marker_style = ComputedStyle::default();
  marker_style.display = Display::Block;
  marker_style.width = Some(Length::px(50.0));
  marker_style.height = Some(Length::px(5.0));
  marker_style.grid_column_start = 2;
  marker_style.grid_column_end = 3;
  marker_style.grid_row_start = 2;
  marker_style.grid_row_end = 3;
  let marker = BoxNode::new_block(Arc::new(marker_style), FormattingContextType::Block, vec![]);

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid, marker],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(300.0, 200.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  let marker_fragment = &fragment.children[1];
  assert_approx(
    subgrid_fragment.children[0].bounds.width(),
    40.0,
    "first track matches grandchild",
  );
  assert_approx(
    subgrid_fragment.children[1].bounds.x(),
    45.0,
    "second track start accounts for gap",
  );
  assert_approx(
    marker_fragment.bounds.x(),
    subgrid_fragment.children[1].bounds.x(),
    "parent sibling aligns to inherited track",
  );
  assert_approx(
    marker_fragment.bounds.y(),
    26.0,
    "second row offset matches inherited sizing and gap",
  );
}

#[test]
fn subgrid_intrinsic_inline_size_accounts_for_children() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![GridTrack::Auto, GridTrack::Auto];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.grid_column_gap = Length::px(6.0);

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut wide = ComputedStyle::default();
  wide.display = Display::Block;
  wide.width = Some(Length::px(30.0));
  wide.grid_column_start = 1;
  wide.grid_column_end = 2;

  let mut tall = ComputedStyle::default();
  tall.display = Display::Block;
  tall.width = Some(Length::px(55.0));
  tall.grid_column_start = 2;
  tall.grid_column_end = 3;

  let child1 = BoxNode::new_block(Arc::new(wide), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(tall), FormattingContextType::Block, vec![]);
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
  let min_content = fc
    .compute_intrinsic_inline_size(&grid, IntrinsicSizingMode::MinContent)
    .expect("intrinsic size");
  assert_approx(
    min_content,
    91.0,
    "subgrid children contribute to min-content size",
  );
}

#[test]
fn rtl_direction_reverses_subgrid_inline_axis() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.direction = Direction::Rtl;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(30.0)),
    GridTrack::Length(Length::px(20.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.width = Some(Length::px(50.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.direction = Direction::Rtl;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut child1_style = ComputedStyle::default();
  child1_style.display = Display::Block;
  child1_style.height = Some(Length::px(5.0));
  child1_style.grid_column_start = 1;
  child1_style.grid_column_end = 2;

  let mut child2_style = ComputedStyle::default();
  child2_style.display = Display::Block;
  child2_style.height = Some(Length::px(5.0));
  child2_style.grid_column_start = 2;
  child2_style.grid_column_end = 3;

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
    .layout(&grid, &LayoutConstraints::definite(100.0, 100.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  eprintln!(
    "rtl subgrid origin x={} first x={} second x={}",
    subgrid_fragment.bounds.x(),
    subgrid_fragment.children[0].bounds.x(),
    subgrid_fragment.children[1].bounds.x()
  );
  let first = &subgrid_fragment.children[0];
  let second = &subgrid_fragment.children[1];
  assert_approx(first.bounds.width(), 30.0, "first track width preserved");
  assert_approx(second.bounds.width(), 20.0, "second track width preserved");
  assert!(
    first.bounds.x() > second.bounds.x(),
    "rtl places the first track on the inline end",
  );
}

#[test]
fn subgrid_uses_parent_direction_even_when_local_differs() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.direction = Direction::Rtl;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(28.0)),
    GridTrack::Length(Length::px(22.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.width = Some(Length::px(50.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.direction = Direction::Ltr;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut child1_style = ComputedStyle::default();
  child1_style.display = Display::Block;
  child1_style.height = Some(Length::px(18.0));
  child1_style.grid_column_start = 1;
  child1_style.grid_column_end = 2;

  let mut child2_style = ComputedStyle::default();
  child2_style.display = Display::Block;
  child2_style.height = Some(Length::px(22.0));
  child2_style.grid_column_start = 2;
  child2_style.grid_column_end = 3;

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
    .layout(&grid, &LayoutConstraints::definite(120.0, 120.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  eprintln!(
    "ltr subgrid origin x={} first x={} second x={}",
    subgrid_fragment.bounds.x(),
    subgrid_fragment.children[0].bounds.x(),
    subgrid_fragment.children[1].bounds.x()
  );
  let first = &subgrid_fragment.children[0];
  let second = &subgrid_fragment.children[1];
  assert_approx(first.bounds.width(), 28.0, "first track width");
  assert_approx(second.bounds.width(), 22.0, "second track width");
  assert!(
    first.bounds.x() > second.bounds.x(),
    "direction is inherited from the parent grid"
  );
  assert_approx(
    first.bounds.x() - second.bounds.x(),
    22.0,
    "tracks remain adjacent in rtl ordering",
  );
}

#[test]
fn row_subgrid_respects_local_direction_for_columns() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![GridTrack::Length(Length::px(60.0))];
  parent_style.grid_template_rows = vec![GridTrack::Auto, GridTrack::Auto];
  parent_style.width = Some(Length::px(60.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_row_subgrid = true;
  subgrid_style.grid_row_start = 1;
  subgrid_style.grid_row_end = 3;
  subgrid_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(20.0)),
    GridTrack::Length(Length::px(30.0)),
  ];
  subgrid_style.direction = Direction::Rtl;

  let mut first_child = ComputedStyle::default();
  first_child.display = Display::Block;
  first_child.height = Some(Length::px(5.0));
  first_child.grid_column_start = 1;
  first_child.grid_column_end = 2;
  first_child.grid_row_start = 1;
  first_child.grid_row_end = 2;

  let mut second_child = ComputedStyle::default();
  second_child.display = Display::Block;
  second_child.height = Some(Length::px(5.0));
  second_child.grid_column_start = 2;
  second_child.grid_column_end = 3;
  second_child.grid_row_start = 1;
  second_child.grid_row_end = 2;

  let child1 = BoxNode::new_block(Arc::new(first_child), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(second_child), FormattingContextType::Block, vec![]);

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
    .layout(&grid, &LayoutConstraints::definite(120.0, 120.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  let first = &subgrid_fragment.children[0];
  let second = &subgrid_fragment.children[1];
  let origin_x = subgrid_fragment.bounds.x();

  eprintln!(
    "subgrid origin {:?} first {:?} second {:?}",
    subgrid_fragment.bounds, first.bounds, second.bounds
  );
  assert!(
    first.bounds.x() - origin_x > second.bounds.x() - origin_x,
    "rtl direction on row subgrid should mirror the inline axis"
  );
  assert_approx(first.bounds.width(), 20.0, "first column width preserved");
  assert_approx(second.bounds.width(), 30.0, "second column width preserved");
}

#[test]
fn subgrid_inherits_named_lines_with_offset() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(25.0)),
    GridTrack::Length(Length::px(35.0)),
    GridTrack::Length(Length::px(45.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.width = Some(Length::px(105.0));
  parent_style.grid_column_line_names = vec![
    vec!["one".into()],
    vec!["two".into()],
    vec!["three".into()],
    vec!["four".into()],
  ];
  let mut names: HashMap<String, Vec<usize>> = HashMap::new();
  names.insert("one".into(), vec![0]);
  names.insert("two".into(), vec![1]);
  names.insert("three".into(), vec![2]);
  names.insert("four".into(), vec![3]);
  parent_style.grid_column_names = names;

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 2;
  subgrid_style.grid_column_end = 4;

  let mut first_child = ComputedStyle::default();
  first_child.display = Display::Block;
  first_child.height = Some(Length::px(10.0));
  first_child.grid_column_raw = Some("two / three".into());

  let mut second_child = ComputedStyle::default();
  second_child.display = Display::Block;
  second_child.height = Some(Length::px(10.0));
  second_child.grid_column_raw = Some("three / four".into());

  let child1 = BoxNode::new_block(Arc::new(first_child), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(second_child), FormattingContextType::Block, vec![]);

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
  let child0 = &subgrid_fragment.children[0];
  let child1 = &subgrid_fragment.children[1];
  let origin_x = subgrid_fragment.bounds.x();
  assert_approx(
    child0.bounds.x() - origin_x,
    0.0,
    "first child starts at inherited line two",
  );
  assert_approx(
    child0.bounds.width(),
    35.0,
    "first child spans the second parent track",
  );
  assert_approx(
    child1.bounds.x() - origin_x,
    35.0,
    "second child begins after the inherited track",
  );
  assert_approx(
    child1.bounds.width(),
    45.0,
    "second child spans the third parent track",
  );
}

#[test]
fn subgrid_inherits_area_line_names_with_offset() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(25.0)),
    GridTrack::Length(Length::px(35.0)),
    GridTrack::Length(Length::px(45.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto, GridTrack::Auto];
  parent_style.grid_template_areas = vec![
    vec![
      Some("left".into()),
      Some("main".into()),
      Some("right".into()),
    ],
    vec![
      Some("left".into()),
      Some("main".into()),
      Some("right".into()),
    ],
  ];
  parent_style.width = Some(Length::px(105.0));
  synthesize_area_line_names(&mut parent_style);

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 2;
  subgrid_style.grid_column_end = 4;

  let mut first_child = ComputedStyle::default();
  first_child.display = Display::Block;
  first_child.height = Some(Length::px(10.0));
  first_child.grid_column_raw = Some("main-start / main-end".into());

  let mut second_child = ComputedStyle::default();
  second_child.display = Display::Block;
  second_child.height = Some(Length::px(10.0));
  second_child.grid_column_raw = Some("right-start / right-end".into());

  let child1 = BoxNode::new_block(Arc::new(first_child), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(second_child), FormattingContextType::Block, vec![]);

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
  assert_approx(
    first.bounds.x() - subgrid_fragment.bounds.x(),
    0.0,
    "main-start maps to subgrid start",
  );
  assert_approx(
    first.bounds.width(),
    35.0,
    "main area width preserved inside subgrid",
  );
  assert_approx(
    second.bounds.x() - subgrid_fragment.bounds.x(),
    35.0,
    "right area starts after main",
  );
  assert_approx(
    second.bounds.width(),
    45.0,
    "right area width preserved inside subgrid",
  );
}

#[test]
fn column_subgrid_inherits_gap_in_vertical_parent() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.writing_mode = WritingMode::VerticalRl;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(18.0)),
    GridTrack::Length(Length::px(22.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.grid_column_gap = Length::px(6.0);
  parent_style.width = Some(Length::px(60.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.writing_mode = WritingMode::VerticalRl;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut child1_style = ComputedStyle::default();
  child1_style.display = Display::Block;
  child1_style.height = Some(Length::px(4.0));
  child1_style.grid_column_start = 1;
  child1_style.grid_column_end = 2;

  let mut child2_style = ComputedStyle::default();
  child2_style.display = Display::Block;
  child2_style.height = Some(Length::px(4.0));
  child2_style.grid_column_start = 2;
  child2_style.grid_column_end = 3;

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
  assert_approx(
    first.bounds.y(),
    0.0,
    "first column origin follows parent track",
  );
  assert_approx(
    subgrid_fragment.bounds.height(),
    46.0,
    "subgrid height matches inherited column tracks",
  );
  assert_approx(
    second.bounds.y(),
    24.0,
    "gap between inherited column tracks is preserved",
  );
}

#[test]
fn subgrid_max_content_inline_size_accounts_for_children() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![GridTrack::Auto, GridTrack::Auto];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.grid_column_gap = Length::px(4.0);

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut wide = ComputedStyle::default();
  wide.display = Display::Block;
  wide.width = Some(Length::px(25.0));
  wide.grid_column_start = 1;
  wide.grid_column_end = 2;

  let mut tall = ComputedStyle::default();
  tall.display = Display::Block;
  tall.width = Some(Length::px(40.0));
  tall.grid_column_start = 2;
  tall.grid_column_end = 3;

  let child1 = BoxNode::new_block(Arc::new(wide), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(tall), FormattingContextType::Block, vec![]);
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
  let max_content = fc
    .compute_intrinsic_inline_size(&grid, IntrinsicSizingMode::MaxContent)
    .expect("intrinsic size");
  assert_approx(
    max_content,
    69.0,
    "max-content spans inherited tracks and gaps",
  );
}

#[test]
fn subgrid_inherits_tracks_on_both_axes_across_writing_modes() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(40.0)),
    GridTrack::Length(Length::px(50.0)),
  ];
  parent_style.grid_template_rows = vec![
    GridTrack::Length(Length::px(25.0)),
    GridTrack::Length(Length::px(35.0)),
  ];
  parent_style.grid_column_gap = Length::px(6.0);
  parent_style.grid_row_gap = Length::px(4.0);
  parent_style.width = Some(Length::px(96.0));
  parent_style.height = Some(Length::px(64.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.writing_mode = WritingMode::VerticalRl;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_row_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;
  subgrid_style.grid_row_start = 1;
  subgrid_style.grid_row_end = 3;

  let mut first_child = ComputedStyle::default();
  first_child.display = Display::Block;
  first_child.grid_column_start = 1;
  first_child.grid_column_end = 2;
  first_child.grid_row_start = 1;
  first_child.grid_row_end = 2;

  let mut second_child = ComputedStyle::default();
  second_child.display = Display::Block;
  second_child.grid_column_start = 2;
  second_child.grid_column_end = 3;
  second_child.grid_row_start = 2;
  second_child.grid_row_end = 3;

  let child1 = BoxNode::new_block(Arc::new(first_child), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(second_child), FormattingContextType::Block, vec![]);
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
  assert_approx(
    subgrid_fragment.bounds.width(),
    96.0,
    "subgrid width matches parent tracks",
  );
  assert_approx(
    subgrid_fragment.bounds.height(),
    64.0,
    "subgrid height matches parent tracks",
  );

  let first = &subgrid_fragment.children[0];
  let second = &subgrid_fragment.children[1];
  assert_approx(first.bounds.x(), 0.0, "first column starts at parent start");
  assert_approx(first.bounds.width(), 40.0, "first column width inherited");
  assert_approx(first.bounds.y(), 0.0, "first row starts at top");
  assert_approx(first.bounds.height(), 25.0, "first row height inherited");

  assert_approx(second.bounds.x(), 46.0, "second column offset includes gap");
  assert_approx(second.bounds.width(), 50.0, "second column width inherited");
  assert_approx(second.bounds.y(), 29.0, "second row offset includes gap");
  assert_approx(second.bounds.height(), 35.0, "second row height inherited");
}

#[test]
fn subgrid_named_lines_survive_writing_mode_transpose() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(25.0)),
    GridTrack::Length(Length::px(35.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.grid_column_gap = Length::px(5.0);
  parent_style.width = Some(Length::px(65.0));
  parent_style.grid_column_line_names =
    vec![vec!["start".into()], vec!["mid".into()], vec!["end".into()]];
  let mut names: HashMap<String, Vec<usize>> = HashMap::new();
  names.insert("start".into(), vec![0]);
  names.insert("mid".into(), vec![1]);
  names.insert("end".into(), vec![2]);
  parent_style.grid_column_names = names;

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.writing_mode = WritingMode::VerticalRl;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;

  let mut first_child = ComputedStyle::default();
  first_child.display = Display::Block;
  first_child.grid_column_raw = Some("start / mid".into());

  let mut second_child = ComputedStyle::default();
  second_child.display = Display::Block;
  second_child.grid_column_raw = Some("mid / end".into());

  let child1 = BoxNode::new_block(Arc::new(first_child), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(second_child), FormattingContextType::Block, vec![]);
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

  assert_approx(first.bounds.width(), 25.0, "start/mid span first column");
  assert_approx(
    first.bounds.x() - subgrid_fragment.bounds.x(),
    0.0,
    "first column starts at origin",
  );
  assert_approx(second.bounds.width(), 35.0, "mid/end span second column");
  assert_approx(
    second.bounds.x() - subgrid_fragment.bounds.x(),
    30.0,
    "second column offset respects parent gap and track",
  );
}

#[test]
fn subgrid_inherits_area_lines_when_axes_differ() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.writing_mode = WritingMode::VerticalRl;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(30.0)),
    GridTrack::Length(Length::px(40.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto, GridTrack::Auto];
  parent_style.grid_column_gap = Length::px(4.0);
  parent_style.grid_template_areas = vec![
    vec![Some("nav".into()), Some("main".into())],
    vec![Some("nav".into()), Some("main".into())],
  ];
  parent_style.width = Some(Length::px(74.0));
  synthesize_area_line_names(&mut parent_style);

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 1;
  subgrid_style.grid_column_end = 3;
  subgrid_style.writing_mode = WritingMode::HorizontalTb;

  let mut nav_child = ComputedStyle::default();
  nav_child.display = Display::Block;
  nav_child.grid_column_raw = Some("nav-start / nav-end".into());
  nav_child.grid_row_start = 1;
  nav_child.grid_row_end = 2;

  let mut main_child = ComputedStyle::default();
  main_child.display = Display::Block;
  main_child.grid_column_raw = Some("main-start / main-end".into());
  main_child.grid_row_start = 2;
  main_child.grid_row_end = 3;

  let nav = BoxNode::new_block(Arc::new(nav_child), FormattingContextType::Block, vec![]);
  let main = BoxNode::new_block(Arc::new(main_child), FormattingContextType::Block, vec![]);

  let subgrid = BoxNode::new_block(
    Arc::new(subgrid_style),
    FormattingContextType::Grid,
    vec![nav, main],
  );

  let grid = BoxNode::new_block(
    Arc::new(parent_style),
    FormattingContextType::Grid,
    vec![subgrid],
  );

  let fc = GridFormattingContext::new();
  let fragment = fc
    .layout(&grid, &LayoutConstraints::definite(120.0, 120.0))
    .expect("layout succeeds");

  let subgrid_fragment = &fragment.children[0];
  let nav_fragment = &subgrid_fragment.children[0];
  let main_fragment = &subgrid_fragment.children[1];
  eprintln!(
    "subgrid {:?} nav {:?} main {:?}",
    subgrid_fragment.bounds, nav_fragment.bounds, main_fragment.bounds
  );

  assert_approx(
    nav_fragment.bounds.y() - subgrid_fragment.bounds.y(),
    0.0,
    "nav stays in the first inherited track",
  );
  assert_approx(
    nav_fragment.bounds.height(),
    30.0,
    "nav spans first column height",
  );
  assert_approx(
    main_fragment.bounds.y() - subgrid_fragment.bounds.y(),
    34.0,
    "gap and track offset preserved for main",
  );
  assert_approx(
    main_fragment.bounds.height(),
    40.0,
    "main spans the second inherited column height",
  );
}

#[test]
fn row_subgrid_inherits_tracks_from_vertical_parent() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.writing_mode = WritingMode::VerticalRl;
  parent_style.grid_template_rows = vec![
    GridTrack::Length(Length::px(40.0)),
    GridTrack::Length(Length::px(60.0)),
  ];
  parent_style.grid_template_columns = vec![GridTrack::Auto];
  parent_style.grid_row_gap = Length::px(8.0);
  parent_style.width = Some(Length::px(108.0));

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.grid_row_subgrid = true;
  subgrid_style.grid_row_start = 1;
  subgrid_style.grid_row_end = 3;
  subgrid_style.writing_mode = WritingMode::HorizontalTb;

  let mut first_child = ComputedStyle::default();
  first_child.display = Display::Block;
  first_child.grid_row_start = 1;
  first_child.grid_row_end = 2;

  let mut second_child = ComputedStyle::default();
  second_child.display = Display::Block;
  second_child.grid_row_start = 2;
  second_child.grid_row_end = 3;

  let child1 = BoxNode::new_block(Arc::new(first_child), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(second_child), FormattingContextType::Block, vec![]);

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

  assert_approx(
    subgrid_fragment.bounds.width(),
    108.0,
    "subgrid width matches inherited rows",
  );
  assert_approx(
    first.bounds.width(),
    40.0,
    "first row size inherited on horizontal axis",
  );
  assert_approx(
    second.bounds.x() - subgrid_fragment.bounds.x(),
    48.0,
    "second row offset includes gap",
  );
  assert_approx(
    second.bounds.width(),
    60.0,
    "second row size inherited on horizontal axis",
  );
}

#[test]
fn subgrid_named_lines_resolve_in_vertical_mode() {
  let mut parent_style = ComputedStyle::default();
  parent_style.display = Display::Grid;
  parent_style.writing_mode = WritingMode::VerticalRl;
  parent_style.grid_template_columns = vec![
    GridTrack::Length(Length::px(25.0)),
    GridTrack::Length(Length::px(35.0)),
    GridTrack::Length(Length::px(45.0)),
  ];
  parent_style.grid_template_rows = vec![GridTrack::Auto];
  parent_style.width = Some(Length::px(105.0));
  parent_style.grid_column_line_names = vec![
    vec!["one".into()],
    vec!["two".into()],
    vec!["three".into()],
    vec!["four".into()],
  ];
  let mut names: HashMap<String, Vec<usize>> = HashMap::new();
  names.insert("one".into(), vec![0]);
  names.insert("two".into(), vec![1]);
  names.insert("three".into(), vec![2]);
  names.insert("four".into(), vec![3]);
  parent_style.grid_column_names = names;

  let mut subgrid_style = ComputedStyle::default();
  subgrid_style.display = Display::Grid;
  subgrid_style.writing_mode = WritingMode::VerticalRl;
  subgrid_style.grid_column_subgrid = true;
  subgrid_style.grid_column_start = 2;
  subgrid_style.grid_column_end = 4;

  let mut first_child = ComputedStyle::default();
  first_child.display = Display::Block;
  first_child.grid_column_raw = Some("two / three".into());

  let mut second_child = ComputedStyle::default();
  second_child.display = Display::Block;
  second_child.grid_column_raw = Some("three / four".into());

  let child1 = BoxNode::new_block(Arc::new(first_child), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(Arc::new(second_child), FormattingContextType::Block, vec![]);

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
  let child0 = &subgrid_fragment.children[0];
  let child1 = &subgrid_fragment.children[1];
  assert_approx(
    child0.bounds.y() - subgrid_fragment.bounds.y(),
    0.0,
    "first named slice starts at inherited line two",
  );
  assert_approx(
    child0.bounds.height(),
    35.0,
    "first named span inherits second track size",
  );
  assert_approx(
    child1.bounds.y() - subgrid_fragment.bounds.y(),
    35.0,
    "second named slice begins after inherited track",
  );
  assert_approx(
    child1.bounds.height(),
    45.0,
    "second named span inherits third track size",
  );
}
