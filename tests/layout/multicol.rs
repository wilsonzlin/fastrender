use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::block::BlockFormattingContext;
use fastrender::style::color::Rgba;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::BorderStyle;
use fastrender::style::types::ColumnFill;
use fastrender::style::types::ColumnSpan;
use fastrender::style::types::WhiteSpace;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::BoxNode;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode};
use fastrender::FormattingContext;
use std::collections::HashMap;
use std::sync::Arc;

fn find_fragment<'a>(fragment: &'a FragmentNode, id: usize) -> Option<&'a FragmentNode> {
  if let FragmentContent::Block { box_id: Some(b) } = fragment.content {
    if b == id {
      return Some(fragment);
    }
  }
  for child in &fragment.children {
    if let Some(found) = find_fragment(child, id) {
      return Some(found);
    }
  }
  None
}

fn find_rule_fragment<'a>(fragment: &'a FragmentNode, color: Rgba) -> Option<&'a FragmentNode> {
  if matches!(fragment.content, FragmentContent::Block { box_id: None })
    && fragment
      .style
      .as_ref()
      .is_some_and(|s| s.background_color == color)
  {
    return Some(fragment);
  }
  for child in &fragment.children {
    if let Some(found) = find_rule_fragment(child, color) {
      return Some(found);
    }
  }
  None
}

fn collect_line_positions(fragment: &FragmentNode, out: &mut Vec<(f32, f32)>) {
  if matches!(fragment.content, FragmentContent::Line { .. }) {
    out.push((fragment.bounds.x(), fragment.bounds.y()));
  }
  for child in &fragment.children {
    collect_line_positions(child, out);
  }
}

#[test]
fn long_paragraph_splits_across_columns() {
  let mut parent_style = ComputedStyle::default();
  parent_style.width = Some(Length::px(300.0));
  parent_style.column_count = Some(2);
  parent_style.column_gap = Length::px(20.0);
  let parent_style = Arc::new(parent_style);

  let mut para_style = ComputedStyle::default();
  para_style.white_space = WhiteSpace::Pre;
  let para_style = Arc::new(para_style);

  let text: String = (0..20).map(|i| format!("line {}\n", i)).collect();
  let mut para = BoxNode::new_block(
    para_style.clone(),
    FormattingContextType::Block,
    vec![BoxNode::new_text(para_style.clone(), text)],
  );
  para.id = 11;

  let mut parent = BoxNode::new_block(parent_style, FormattingContextType::Block, vec![para]);
  parent.id = 12;

  let fc = BlockFormattingContext::new();
  let fragment = fc
    .layout(&parent, &LayoutConstraints::definite_width(300.0))
    .expect("layout");

  let mut lines = Vec::new();
  collect_line_positions(&fragment, &mut lines);

  assert!(
    lines.iter().any(|(x, _)| *x >= 150.0),
    "lines should continue into the second column"
  );
}

#[test]
fn balanced_fill_spreads_lines_evenly() {
  let mut parent_style = ComputedStyle::default();
  parent_style.width = Some(Length::px(360.0));
  parent_style.column_count = Some(3);
  parent_style.column_gap = Length::px(10.0);
  let parent_style = Arc::new(parent_style);

  let mut para_style = ComputedStyle::default();
  para_style.white_space = WhiteSpace::Pre;
  let para_style = Arc::new(para_style);

  let text: String = (0..15).map(|i| format!("l{}\n", i)).collect();
  let para = BoxNode::new_block(
    para_style.clone(),
    FormattingContextType::Block,
    vec![BoxNode::new_text(para_style.clone(), text)],
  );

  let root = BoxNode::new_block(parent_style, FormattingContextType::Block, vec![para]);

  let fc = BlockFormattingContext::new();
  let fragment = fc
    .layout(&root, &LayoutConstraints::definite_width(360.0))
    .expect("layout");

  let mut lines = Vec::new();
  collect_line_positions(&fragment, &mut lines);

  let column_width = (360.0 - 10.0 * 2.0) / 3.0;
  let stride = column_width + 10.0;
  let mut counts: HashMap<usize, usize> = HashMap::new();
  for (x, _) in lines {
    let col = ((x / stride).floor() as usize).min(4);
    *counts.entry(col).or_default() += 1;
  }

  assert_eq!(counts.len(), 3, "all columns should receive content");
  let min = counts.values().copied().min().unwrap_or(0);
  let max = counts.values().copied().max().unwrap_or(0);
  assert!(
    max.saturating_sub(min) <= 1,
    "balanced fill should distribute lines evenly"
  );
}

#[test]
fn column_fill_auto_uses_definite_height() {
  let mut parent_style = ComputedStyle::default();
  parent_style.width = Some(Length::px(200.0));
  parent_style.height = Some(Length::px(60.0));
  parent_style.column_count = Some(2);
  parent_style.column_gap = Length::px(10.0);
  parent_style.column_fill = ColumnFill::Auto;
  let parent_style = Arc::new(parent_style);

  let child_style = |height: f32| -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.height = Some(Length::px(height));
    Arc::new(style)
  };

  let mut first = BoxNode::new_block(child_style(40.0), FormattingContextType::Block, vec![]);
  first.id = 41;
  let mut second = BoxNode::new_block(child_style(40.0), FormattingContextType::Block, vec![]);
  second.id = 42;
  let mut third = BoxNode::new_block(child_style(40.0), FormattingContextType::Block, vec![]);
  third.id = 43;

  let mut parent = BoxNode::new_block(
    parent_style,
    FormattingContextType::Block,
    vec![first.clone(), second.clone(), third.clone()],
  );
  parent.id = 40;

  let fc = BlockFormattingContext::new();
  let fragment = fc
    .layout(&parent, &LayoutConstraints::definite_width(200.0))
    .expect("layout");

  let first_frag = find_fragment(&fragment, first.id).expect("first fragment");
  let second_frag = find_fragment(&fragment, second.id).expect("second fragment");
  let third_frag = find_fragment(&fragment, third.id).expect("third fragment");

  assert!(
    (first_frag.bounds.y() - 0.0).abs() < 0.1,
    "first column should start at y=0"
  );
  assert!(
    (second_frag.bounds.y() - 0.0).abs() < 0.1,
    "second column should share the same set origin"
  );
  assert!(
    (third_frag.bounds.y() - 60.0).abs() < 0.6,
    "overflow content should begin a new set using the definite height"
  );
}

#[test]
fn multicol_layout_balances_children_and_rules() {
  let mut parent_style = ComputedStyle::default();
  parent_style.width = Some(Length::px(400.0));
  parent_style.column_count = Some(2);
  parent_style.column_gap = Length::px(20.0);
  parent_style.column_rule_style = BorderStyle::Solid;
  parent_style.column_rule_width = Length::px(6.0);
  let rule_color = Rgba::new(255, 0, 0, 1.0);
  parent_style.column_rule_color = Some(rule_color);
  let parent_style = Arc::new(parent_style);

  let child_style = |height: f32| -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.height = Some(Length::px(height));
    Arc::new(style)
  };

  let mut first = BoxNode::new_block(child_style(80.0), FormattingContextType::Block, vec![]);
  first.id = 1;
  let mut second = BoxNode::new_block(child_style(80.0), FormattingContextType::Block, vec![]);
  second.id = 2;

  let mut span_style = ComputedStyle::default();
  span_style.height = Some(Length::px(30.0));
  span_style.column_span = ColumnSpan::All;
  let mut span = BoxNode::new_block(Arc::new(span_style), FormattingContextType::Block, vec![]);
  span.id = 3;

  let mut parent = BoxNode::new_block(
    parent_style,
    FormattingContextType::Block,
    vec![first.clone(), second.clone(), span.clone()],
  );
  parent.id = 10;

  let fc = BlockFormattingContext::new();
  let fragment = fc
    .layout(&parent, &LayoutConstraints::definite_width(400.0))
    .expect("layout");

  let first_frag = find_fragment(&fragment, first.id).expect("first fragment");
  assert!((first_frag.bounds.x() - 0.0).abs() < 0.1);
  assert!((first_frag.bounds.y() - 0.0).abs() < 0.1);

  let second_frag = find_fragment(&fragment, second.id).expect("second fragment");
  assert!((second_frag.bounds.x() - 210.0).abs() < 0.2);
  assert!((second_frag.bounds.y() - 0.0).abs() < 0.1);

  let span_frag = find_fragment(&fragment, span.id).expect("span fragment");
  assert!((span_frag.bounds.x()).abs() < 0.1);
  assert!(span_frag.bounds.width() > 399.0);
  assert!((span_frag.bounds.y() - 80.0).abs() < 0.2);

  let rule_frag = find_rule_fragment(&fragment, rule_color).expect("column rule fragment");
  assert!((rule_frag.bounds.width() - 6.0).abs() < 0.1);
  assert!((rule_frag.bounds.height() - 80.0).abs() < 0.2);
  assert!((rule_frag.bounds.x() - 197.0).abs() < 0.5);
}

#[test]
fn column_span_creates_new_segment() {
  let mut parent_style = ComputedStyle::default();
  parent_style.width = Some(Length::px(400.0));
  parent_style.column_count = Some(2);
  parent_style.column_gap = Length::px(20.0);
  let parent_style = Arc::new(parent_style);

  let child_style = |height: f32| -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.height = Some(Length::px(height));
    Arc::new(style)
  };

  let mut first = BoxNode::new_block(child_style(60.0), FormattingContextType::Block, vec![]);
  first.id = 1;
  let mut second = BoxNode::new_block(child_style(60.0), FormattingContextType::Block, vec![]);
  second.id = 2;

  let mut span_style = ComputedStyle::default();
  span_style.height = Some(Length::px(30.0));
  span_style.column_span = ColumnSpan::All;
  let mut span = BoxNode::new_block(Arc::new(span_style), FormattingContextType::Block, vec![]);
  span.id = 3;

  let mut trailing = BoxNode::new_block(child_style(40.0), FormattingContextType::Block, vec![]);
  trailing.id = 4;

  let mut parent = BoxNode::new_block(
    parent_style,
    FormattingContextType::Block,
    vec![
      first.clone(),
      second.clone(),
      span.clone(),
      trailing.clone(),
    ],
  );
  parent.id = 20;

  let fc = BlockFormattingContext::new();
  let fragment = fc
    .layout(&parent, &LayoutConstraints::definite_width(400.0))
    .expect("layout");

  let span_frag = find_fragment(&fragment, span.id).expect("span fragment");
  assert!((span_frag.bounds.y() - 60.0).abs() < 0.5);

  let trailing_frag = find_fragment(&fragment, trailing.id).expect("trailing fragment");
  assert!(trailing_frag.bounds.y() >= span_frag.bounds.max_y());
}

#[test]
fn nested_multicol_layouts_columns() {
  let mut inner_style = ComputedStyle::default();
  inner_style.width = Some(Length::px(240.0));
  inner_style.column_count = Some(2);
  inner_style.column_gap = Length::px(16.0);
  let inner_style = Arc::new(inner_style);

  let child_style = |height: f32| -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.height = Some(Length::px(height));
    Arc::new(style)
  };

  let mut inner_child1 =
    BoxNode::new_block(child_style(30.0), FormattingContextType::Block, vec![]);
  inner_child1.id = 5;
  let mut inner_child2 =
    BoxNode::new_block(child_style(30.0), FormattingContextType::Block, vec![]);
  inner_child2.id = 6;

  let mut inner = BoxNode::new_block(
    inner_style,
    FormattingContextType::Block,
    vec![inner_child1.clone(), inner_child2.clone()],
  );
  inner.id = 30;

  let outer_style = Arc::new(ComputedStyle::default());
  let root = BoxNode::new_block(
    outer_style,
    FormattingContextType::Block,
    vec![inner.clone()],
  );

  let fc = BlockFormattingContext::new();
  let fragment = fc
    .layout(&root, &LayoutConstraints::definite_width(400.0))
    .expect("layout");

  let first_frag = find_fragment(&fragment, inner_child1.id).expect("first fragment");
  let second_frag = find_fragment(&fragment, inner_child2.id).expect("second fragment");

  assert!(first_frag.bounds.x() < second_frag.bounds.x());
  assert!((second_frag.bounds.x() - first_frag.bounds.x()) > 60.0);
  assert!((first_frag.bounds.y() - second_frag.bounds.y()).abs() < 0.1);
}
