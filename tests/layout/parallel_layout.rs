use fastrender::render_control::RenderDeadline;
use fastrender::snapshot_fragment_tree;
use fastrender::layout::engine::{
  enable_layout_parallel_debug_counters, layout_parallel_debug_counters,
  reset_layout_parallel_debug_counters, DEFAULT_LAYOUT_MIN_FANOUT,
};
use fastrender::style::display::Display;
use fastrender::style::position::Position;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::text::font_loader::FontContext;
use fastrender::{
  error::LayoutError, BoxNode, BoxTree, FormattingContextType, FragmentNodeSnapshot,
  FragmentTreeSnapshot, LayoutConfig, LayoutEngine, LayoutParallelism, Size,
};
use std::sync::Arc;
use std::time::Duration;

struct EnvGuard(&'static str);

impl EnvGuard {
  fn set(key: &'static str, value: &str) -> Self {
    std::env::set_var(key, value);
    Self(key)
  }
}

impl Drop for EnvGuard {
  fn drop(&mut self) {
    std::env::remove_var(self.0);
  }
}

fn approx(a: f32, b: f32) -> bool {
  (a - b).abs() < 0.001
}

fn assert_content_eq(
  a: &fastrender::tree::fragment_tree::FragmentContent,
  b: &fastrender::tree::fragment_tree::FragmentContent,
) {
  use fastrender::tree::fragment_tree::FragmentContent::*;
  match (a, b) {
    (Block { box_id: a_id }, Block { box_id: b_id }) => assert_eq!(a_id, b_id),
    (
      Inline {
        box_id: a_id,
        fragment_index: a_idx,
      },
      Inline {
        box_id: b_id,
        fragment_index: b_idx,
      },
    ) => {
      assert_eq!(a_id, b_id);
      assert_eq!(a_idx, b_idx);
    }
    (Line { baseline: a }, Line { baseline: b }) => assert!(approx(*a, *b)),
    (
      Text {
        text: a_text,
        baseline_offset: a_base,
        is_marker: a_marker,
        ..
      },
      Text {
        text: b_text,
        baseline_offset: b_base,
        is_marker: b_marker,
        ..
      },
    ) => {
      assert_eq!(a_text, b_text);
      assert!(approx(*a_base, *b_base));
      assert_eq!(a_marker, b_marker);
    }
    (
      Replaced {
        replaced_type: a_type,
        box_id: a_id,
      },
      Replaced {
        replaced_type: b_type,
        box_id: b_id,
      },
    ) => {
      assert_eq!(a_type, b_type);
      assert_eq!(a_id, b_id);
    }
    (RunningAnchor { name: a_name, .. }, RunningAnchor { name: b_name, .. }) => {
      assert_eq!(a_name, b_name)
    }
    _ => panic!("Fragment content mismatch: {a:?} vs {b:?}"),
  }
}

fn assert_fragment_eq(a: &fastrender::FragmentNode, b: &fastrender::FragmentNode) {
  assert!(approx(a.bounds.x(), b.bounds.x()));
  assert!(approx(a.bounds.y(), b.bounds.y()));
  assert!(approx(a.bounds.width(), b.bounds.width()));
  assert!(approx(a.bounds.height(), b.bounds.height()));
  if let (Some(a_base), Some(b_base)) = (a.baseline, b.baseline) {
    assert!(approx(a_base, b_base));
  } else {
    assert_eq!(a.baseline.is_some(), b.baseline.is_some());
  }
  assert_eq!(a.fragment_index, b.fragment_index);
  assert_eq!(a.fragment_count, b.fragment_count);
  assert_eq!(a.fragmentainer_index, b.fragmentainer_index);
  assert_eq!(a.fragmentainer, b.fragmentainer);
  assert_content_eq(&a.content, &b.content);
  assert_eq!(a.children.len(), b.children.len());
  for (left, right) in a.children.iter().zip(&b.children) {
    assert_fragment_eq(left, right);
  }
}

fn assert_trees_match(a: &fastrender::FragmentTree, b: &fastrender::FragmentTree) {
  assert!(approx(a.viewport_size().width, b.viewport_size().width));
  assert!(approx(a.viewport_size().height, b.viewport_size().height));
  assert_eq!(a.additional_fragments.len(), b.additional_fragments.len());
  assert_fragment_eq(&a.root, &b.root);
  for (left, right) in a
    .additional_fragments
    .iter()
    .zip(b.additional_fragments.iter())
  {
    assert_fragment_eq(left, right);
  }
}

fn build_table(rows: usize, cols: usize) -> BoxTree {
  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;
  let mut table_style = ComputedStyle::default();
  table_style.display = Display::Table;
  table_style.width = Some(Length::px(640.0));

  let mut group_style = ComputedStyle::default();
  group_style.display = Display::TableRowGroup;

  let mut row_style = ComputedStyle::default();
  row_style.display = Display::TableRow;

  let mut cell_style = ComputedStyle::default();
  cell_style.display = Display::TableCell;
  cell_style.padding_top = Length::px(4.0);
  cell_style.padding_bottom = Length::px(4.0);

  let mut row_nodes = Vec::new();
  for r in 0..rows {
    let mut cells = Vec::new();
    for c in 0..cols {
      let text = BoxNode::new_text(Arc::new(cell_style.clone()), format!("cell-{r}-{c}"));
      let cell = BoxNode::new_block(
        Arc::new(cell_style.clone()),
        FormattingContextType::Block,
        vec![text],
      );
      cells.push(cell);
    }
    let row = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      cells,
    );
    row_nodes.push(row);
  }

  let row_group = BoxNode::new_block(
    Arc::new(group_style),
    FormattingContextType::Block,
    row_nodes,
  );
  let table = BoxNode::new_block(
    Arc::new(table_style),
    FormattingContextType::Table,
    vec![row_group],
  );
  let root = BoxNode::new_block(
    Arc::new(root_style),
    FormattingContextType::Block,
    vec![table],
  );
  BoxTree::new(root)
}

fn build_block_stack(children: usize) -> BoxTree {
  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;

  let mut child_style = ComputedStyle::default();
  child_style.display = Display::Block;

  let mut nodes = Vec::new();
  for idx in 0..children {
    let text = BoxNode::new_text(Arc::new(child_style.clone()), format!("block-{idx}"));
    let child = BoxNode::new_block(
      Arc::new(child_style.clone()),
      FormattingContextType::Block,
      vec![text],
    );
    nodes.push(child);
  }

  let root = BoxNode::new_block(Arc::new(root_style), FormattingContextType::Block, nodes);
  BoxTree::new(root)
}

fn build_flex_container(children: usize) -> BoxNode {
  let mut flex_style = ComputedStyle::default();
  flex_style.display = Display::Flex;

  let mut item_style = ComputedStyle::default();
  item_style.display = Display::Block;
  item_style.padding_left = Length::px(2.0);
  item_style.padding_right = Length::px(2.0);
  let item_style = Arc::new(item_style);

  let mut items = Vec::new();
  for idx in 0..children {
    let text = BoxNode::new_text(
      item_style.clone(),
      format!("flex-item-{idx} lorem ipsum dolor sit amet"),
    );
    let child = BoxNode::new_block(item_style.clone(), FormattingContextType::Block, vec![text]);
    items.push(child);
  }

  let mut overlay_style = (*item_style).clone();
  overlay_style.position = Position::Absolute;
  overlay_style.top = Some(Length::px(6.0));
  overlay_style.left = Some(Length::px(8.0));
  let overlay_style = Arc::new(overlay_style);
  let overlay_text = BoxNode::new_text(overlay_style.clone(), "overlay".to_string());
  items.push(BoxNode::new_block(
    overlay_style,
    FormattingContextType::Block,
    vec![overlay_text],
  ));

  BoxNode::new_block(Arc::new(flex_style), FormattingContextType::Flex, items)
}

fn build_mixed_layout() -> BoxTree {
  let table = build_table(6, 4).root;
  let flex = build_flex_container(10);
  let block_stack = build_block_stack(18).root;

  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;
  let root = BoxNode::new_block(
    Arc::new(root_style),
    FormattingContextType::Block,
    vec![table, flex, block_stack],
  );
  BoxTree::new(root)
}

fn build_grid(rows: usize, cols: usize) -> BoxTree {
  use fastrender::style::types::GridAutoFlow;

  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;

  let mut grid_style = ComputedStyle::default();
  grid_style.display = Display::Grid;
  grid_style.grid_auto_flow = GridAutoFlow::RowDense;
  grid_style.width = Some(Length::px(960.0));

  let mut item_style = ComputedStyle::default();
  item_style.display = Display::Block;
  item_style.padding_left = Length::px(2.0);
  item_style.padding_right = Length::px(2.0);
  let item_style = Arc::new(item_style);

  let mut items = Vec::new();
  for r in 0..rows {
    for c in 0..cols {
      let text = BoxNode::new_text(item_style.clone(), format!("grid-{r}-{c} lorem ipsum"));
      let child = BoxNode::new_block(item_style.clone(), FormattingContextType::Block, vec![text]);
      items.push(child);
    }
  }

  let grid = BoxNode::new_block(Arc::new(grid_style), FormattingContextType::Grid, items);
  let root = BoxNode::new_block(
    Arc::new(root_style),
    FormattingContextType::Block,
    vec![grid],
  );
  BoxTree::new(root)
}

fn diff_nodes(path: &str, a: &FragmentNodeSnapshot, b: &FragmentNodeSnapshot) -> Option<String> {
  if a.bounds != b.bounds {
    return Some(format!("{path}.bounds {:?} vs {:?}", a.bounds, b.bounds));
  }
  if a.scroll_overflow != b.scroll_overflow {
    return Some(format!(
      "{path}.scroll_overflow {:?} vs {:?}",
      a.scroll_overflow, b.scroll_overflow
    ));
  }
  if a.baseline != b.baseline {
    return Some(format!(
      "{path}.baseline {:?} vs {:?}",
      a.baseline, b.baseline
    ));
  }
  if a.fragment_index != b.fragment_index {
    return Some(format!(
      "{path}.fragment_index {} vs {}",
      a.fragment_index, b.fragment_index
    ));
  }
  if a.fragment_count != b.fragment_count {
    return Some(format!(
      "{path}.fragment_count {} vs {}",
      a.fragment_count, b.fragment_count
    ));
  }
  if a.fragmentainer_index != b.fragmentainer_index {
    return Some(format!(
      "{path}.fragmentainer_index {} vs {}",
      a.fragmentainer_index, b.fragmentainer_index
    ));
  }
  if a.content != b.content {
    return Some(format!("{path}.content {:?} vs {:?}", a.content, b.content));
  }
  if a.children.len() != b.children.len() {
    return Some(format!(
      "{path}.children len {} vs {}",
      a.children.len(),
      b.children.len()
    ));
  }
  for (idx, (left, right)) in a.children.iter().zip(&b.children).enumerate() {
    let child_path = format!("{path}.children[{idx}]");
    if let Some(diff) = diff_nodes(&child_path, left, right) {
      return Some(diff);
    }
  }
  None
}

fn diff_trees(a: &FragmentTreeSnapshot, b: &FragmentTreeSnapshot) -> Option<String> {
  if a.viewport != b.viewport {
    return Some(format!("viewport {:?} vs {:?}", a.viewport, b.viewport));
  }
  if a.roots.len() != b.roots.len() {
    return Some(format!("roots len {} vs {}", a.roots.len(), b.roots.len()));
  }
  for (idx, (left, right)) in a.roots.iter().zip(&b.roots).enumerate() {
    if let Some(diff) = diff_nodes(&format!("roots[{idx}]"), left, right) {
      return Some(diff);
    }
  }
  None
}

#[test]
fn parallel_layout_matches_serial_fragments() {
  let box_tree = build_table(12, 8);
  let viewport = Size::new(1024.0, 768.0);

  let serial_config = LayoutConfig::for_viewport(viewport);
  let parallelism = LayoutParallelism::enabled(1).with_max_threads(Some(4));
  let parallel_config = LayoutConfig::for_viewport(viewport).with_parallelism(parallelism);

  let font_ctx = FontContext::new();
  let serial_engine = LayoutEngine::with_font_context(serial_config, font_ctx.clone());
  let parallel_engine = LayoutEngine::with_font_context(parallel_config, font_ctx);

  let serial_tree = serial_engine.layout_tree(&box_tree).expect("serial layout");
  let parallel_tree = parallel_engine
    .layout_tree(&box_tree)
    .expect("parallel layout");

  assert_trees_match(&serial_tree, &parallel_tree);
}

#[test]
fn parallel_block_children_match_serial_fragments() {
  let box_tree = build_block_stack(48);
  let viewport = Size::new(1200.0, 900.0);

  let serial_config = LayoutConfig::for_viewport(viewport);
  let parallelism = LayoutParallelism::enabled(2).with_max_threads(Some(4));
  let parallel_config = LayoutConfig::for_viewport(viewport).with_parallelism(parallelism);

  let font_ctx = FontContext::new();
  let serial_engine = LayoutEngine::with_font_context(serial_config, font_ctx.clone());
  let parallel_engine = LayoutEngine::with_font_context(parallel_config, font_ctx);

  let serial_tree = serial_engine.layout_tree(&box_tree).expect("serial layout");
  let parallel_tree = parallel_engine
    .layout_tree(&box_tree)
    .expect("parallel layout");

  assert_trees_match(&serial_tree, &parallel_tree);
}

#[test]
fn parallel_grid_children_match_serial_fragments() {
  let box_tree = build_grid(12, 10);
  let viewport = Size::new(1200.0, 960.0);

  let serial_engine =
    LayoutEngine::with_font_context(LayoutConfig::for_viewport(viewport), FontContext::new());
  let parallel_engine = LayoutEngine::with_font_context(
    LayoutConfig::for_viewport(viewport).with_parallelism(
      LayoutParallelism::enabled(4).with_max_threads(Some(num_cpus::get().max(2))),
    ),
    FontContext::new(),
  );

  let serial_tree = serial_engine.layout_tree(&box_tree).expect("serial layout");
  let parallel_tree = parallel_engine
    .layout_tree(&box_tree)
    .expect("parallel layout");

  assert_trees_match(&serial_tree, &parallel_tree);
}

#[test]
fn parallel_snapshots_match_serial_snapshots() {
  let box_tree = build_mixed_layout();
  let viewport = Size::new(1180.0, 860.0);

  let serial_config = LayoutConfig::for_viewport(viewport);
  let parallelism = LayoutParallelism::enabled(2).with_max_threads(Some(4));
  let parallel_config = LayoutConfig::for_viewport(viewport).with_parallelism(parallelism);

  let font_ctx = FontContext::new();
  let serial_engine = LayoutEngine::with_font_context(serial_config, font_ctx.clone());
  let parallel_engine = LayoutEngine::with_font_context(parallel_config, font_ctx);

  let serial_snapshot =
    snapshot_fragment_tree(&serial_engine.layout_tree(&box_tree).expect("serial layout"));
  let parallel_snapshot = snapshot_fragment_tree(
    &parallel_engine
      .layout_tree(&box_tree)
      .expect("parallel layout"),
  );

  if let Some(diff) = diff_trees(&serial_snapshot, &parallel_snapshot) {
    eprintln!(
      "serial root overflow: {:?} parallel root overflow: {:?}",
      serial_snapshot
        .roots
        .first()
        .map(|n| n.scroll_overflow.clone()),
      parallel_snapshot
        .roots
        .first()
        .map(|n| n.scroll_overflow.clone())
    );
    if let (Some(serial_root), Some(parallel_root)) = (
      serial_snapshot.roots.first(),
      parallel_snapshot.roots.first(),
    ) {
      for (idx, (left, right)) in serial_root
        .children
        .iter()
        .zip(&parallel_root.children)
        .enumerate()
      {
        eprintln!(
          "child {idx} bounds {:?} overflow {:?} || parallel {:?} overflow {:?}",
          left.bounds, left.scroll_overflow, right.bounds, right.scroll_overflow
        );
      }
    }
    panic!("snapshot mismatch: {diff}");
  }
}

#[test]
fn parallel_layout_is_reproducible() {
  let box_tree = build_table(10, 6);
  let viewport = Size::new(1024.0, 768.0);

  let serial_config = LayoutConfig::for_viewport(viewport);
  let parallelism = LayoutParallelism::enabled(2).with_max_threads(Some(4));
  let parallel_config = LayoutConfig::for_viewport(viewport).with_parallelism(parallelism);

  let font_ctx = FontContext::new();
  let serial_engine = LayoutEngine::with_font_context(serial_config, font_ctx.clone());
  let parallel_engine = LayoutEngine::with_font_context(parallel_config, font_ctx);

  let serial_snapshot =
    snapshot_fragment_tree(&serial_engine.layout_tree(&box_tree).expect("serial layout"));
  let parallel_first = snapshot_fragment_tree(
    &parallel_engine
      .layout_tree(&box_tree)
      .expect("first parallel layout"),
  );
  let parallel_second = snapshot_fragment_tree(
    &parallel_engine
      .layout_tree(&box_tree)
      .expect("second parallel layout"),
  );

  assert_eq!(parallel_first, parallel_second);
  if let Some(diff) = diff_trees(&serial_snapshot, &parallel_first) {
    panic!("serial vs parallel diff: {diff}");
  }
}

#[test]
fn parallel_layout_respects_deadline() {
  let _delay_guard = EnvGuard::set("FASTR_TEST_RENDER_DELAY_MS", "20");
  let parallelism = LayoutParallelism::enabled(1).with_max_threads(Some(4));
  let config = LayoutConfig::for_viewport(Size::new(800.0, 600.0)).with_parallelism(parallelism);
  let engine = LayoutEngine::with_font_context(config, FontContext::new());
  let box_tree = build_block_stack(48);
  let deadline = RenderDeadline::new(Some(Duration::from_millis(1)), None);

  let err = engine
    .layout_tree_with_deadline(&box_tree, Some(&deadline))
    .unwrap_err();
  match err {
    LayoutError::Timeout { .. } => {}
    other => panic!("expected timeout, got {other:?}"),
  }
}

#[test]
fn auto_parallel_layout_spawns_workers() {
  let box_tree = build_block_stack(1024);
  let viewport = Size::new(1200.0, 900.0);
  let threads = num_cpus::get().max(2);
  let parallelism = LayoutParallelism::auto(DEFAULT_LAYOUT_MIN_FANOUT)
    .with_max_threads(Some(threads));
  let config = LayoutConfig::for_viewport(viewport).with_parallelism(parallelism);
  let engine = LayoutEngine::with_font_context(config, FontContext::new());

  enable_layout_parallel_debug_counters(true);
  reset_layout_parallel_debug_counters();
  let _ = engine.layout_tree(&box_tree).expect("auto parallel layout");
  let counters = layout_parallel_debug_counters();
  enable_layout_parallel_debug_counters(false);
  reset_layout_parallel_debug_counters();

  assert!(
    counters.worker_threads > 1,
    "expected auto parallel layout to use multiple workers, counters={counters:?}"
  );
  assert!(
    counters.work_items > 0,
    "expected auto parallel layout to record parallel work"
  );
}
