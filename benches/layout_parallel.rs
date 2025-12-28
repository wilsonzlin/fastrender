use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::layout::engine::LayoutParallelism;
use fastrender::style::display::Display;
use fastrender::style::position::Position;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::text::font_loader::FontContext;
use fastrender::{BoxNode, BoxTree, FormattingContextType, LayoutConfig, LayoutEngine, Size};
use std::sync::Arc;

fn build_table(rows: usize, cols: usize) -> BoxTree {
  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;
  let mut table_style = ComputedStyle::default();
  table_style.display = Display::Table;
  table_style.width = Some(Length::px(960.0));

  let mut group_style = ComputedStyle::default();
  group_style.display = Display::TableRowGroup;

  let mut row_style = ComputedStyle::default();
  row_style.display = Display::TableRow;

  let mut cell_style = ComputedStyle::default();
  cell_style.display = Display::TableCell;
  cell_style.padding_top = Length::px(2.0);
  cell_style.padding_bottom = Length::px(2.0);

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
  let shared = Arc::new(child_style);
  for idx in 0..children {
    let text = BoxNode::new_text(
      shared.clone(),
      format!("paragraph-{idx} lorem ipsum dolor sit amet"),
    );
    let child = BoxNode::new_block(shared.clone(), FormattingContextType::Block, vec![text]);
    nodes.push(child);
  }

  let root = BoxNode::new_block(Arc::new(root_style), FormattingContextType::Block, nodes);
  BoxTree::new(root)
}

fn build_flex_row(children: usize) -> BoxTree {
  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;

  let mut flex_style = ComputedStyle::default();
  flex_style.display = Display::Flex;

  let mut item_style = ComputedStyle::default();
  item_style.display = Display::Block;
  item_style.padding_left = Length::px(2.0);
  item_style.padding_right = Length::px(2.0);
  let shared_item_style = Arc::new(item_style);

  let mut items = Vec::new();
  for idx in 0..children {
    let text = BoxNode::new_text(
      shared_item_style.clone(),
      format!("flex-{idx} lorem ipsum dolor sit amet"),
    );
    let child = BoxNode::new_block(
      shared_item_style.clone(),
      FormattingContextType::Block,
      vec![text],
    );
    items.push(child);
  }

  // Add a positioned overlay to exercise absolute positioning paths.
  let mut abs_style = (*shared_item_style).clone();
  abs_style.position = Position::Absolute;
  abs_style.top = Some(Length::px(6.0));
  abs_style.left = Some(Length::px(10.0));
  let abs_style = Arc::new(abs_style);
  let abs_text = BoxNode::new_text(abs_style.clone(), "overlay".to_string());
  items.push(BoxNode::new_block(
    abs_style,
    FormattingContextType::Block,
    vec![abs_text],
  ));

  let flex = BoxNode::new_block(Arc::new(flex_style), FormattingContextType::Flex, items);
  let root = BoxNode::new_block(
    Arc::new(root_style),
    FormattingContextType::Block,
    vec![flex],
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
  grid_style.width = Some(Length::px(1200.0));

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

fn bench_layout_parallel(c: &mut Criterion) {
  let box_tree = build_table(40, 18);
  let viewport = Size::new(1280.0, 900.0);
  let font_ctx = FontContext::new();

  let serial_engine =
    LayoutEngine::with_font_context(LayoutConfig::for_viewport(viewport), font_ctx.clone());
  let parallelism = LayoutParallelism::enabled(4).with_max_threads(Some(num_cpus::get().max(2)));
  let parallel_engine = LayoutEngine::with_font_context(
    LayoutConfig::for_viewport(viewport).with_parallelism(parallelism),
    font_ctx,
  );

  let mut group = c.benchmark_group("layout_table_parallel");
  group.bench_function("serial", |b| {
    b.iter(|| serial_engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.bench_function("parallel", |b| {
    b.iter(|| parallel_engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.finish();
}

fn bench_layout_parallel_dense(c: &mut Criterion) {
  let box_tree = build_table(80, 32);
  let viewport = Size::new(1440.0, 960.0);
  let font_ctx = FontContext::new();

  let serial_engine =
    LayoutEngine::with_font_context(LayoutConfig::for_viewport(viewport), font_ctx.clone());
  let parallelism = LayoutParallelism::enabled(8)
    .with_min_fanout(4)
    .with_max_threads(Some(num_cpus::get().max(2)));
  let parallel_engine = LayoutEngine::with_font_context(
    LayoutConfig::for_viewport(viewport).with_parallelism(parallelism),
    font_ctx,
  );

  let mut group = c.benchmark_group("layout_table_parallel_dense");
  group.bench_function("serial", |b| {
    b.iter(|| serial_engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.bench_function("parallel", |b| {
    b.iter(|| parallel_engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.finish();
}

fn bench_grid_parallel(c: &mut Criterion) {
  let box_tree = build_grid(36, 18);
  let viewport = Size::new(1400.0, 960.0);
  let font_ctx = FontContext::new();

  let serial_engine =
    LayoutEngine::with_font_context(LayoutConfig::for_viewport(viewport), font_ctx.clone());
  let parallelism = LayoutParallelism::enabled(8).with_max_threads(Some(num_cpus::get().max(2)));
  let parallel_engine = LayoutEngine::with_font_context(
    LayoutConfig::for_viewport(viewport).with_parallelism(parallelism),
    font_ctx,
  );

  let mut group = c.benchmark_group("layout_grid_parallel");
  group.bench_function("serial", |b| {
    b.iter(|| serial_engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.bench_function("parallel", |b| {
    b.iter(|| parallel_engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.finish();
}

fn bench_block_parallel(c: &mut Criterion) {
  let box_tree = build_block_stack(180);
  let viewport = Size::new(1280.0, 900.0);
  let font_ctx = FontContext::new();

  let serial_engine =
    LayoutEngine::with_font_context(LayoutConfig::for_viewport(viewport), font_ctx.clone());
  let parallelism = LayoutParallelism::enabled(8).with_max_threads(Some(num_cpus::get().max(2)));
  let parallel_engine = LayoutEngine::with_font_context(
    LayoutConfig::for_viewport(viewport).with_parallelism(parallelism),
    font_ctx,
  );

  let mut group = c.benchmark_group("layout_block_parallel");
  group.bench_function("serial", |b| {
    b.iter(|| serial_engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.bench_function("parallel", |b| {
    b.iter(|| parallel_engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.finish();
}

fn bench_flex_parallel(c: &mut Criterion) {
  let box_tree = build_flex_row(160);
  let viewport = Size::new(1280.0, 900.0);
  let font_ctx = FontContext::new();

  let serial_engine =
    LayoutEngine::with_font_context(LayoutConfig::for_viewport(viewport), font_ctx.clone());
  let parallelism = LayoutParallelism::enabled(8).with_max_threads(Some(num_cpus::get().max(2)));
  let parallel_engine = LayoutEngine::with_font_context(
    LayoutConfig::for_viewport(viewport).with_parallelism(parallelism),
    font_ctx,
  );

  let mut group = c.benchmark_group("layout_flex_parallel");
  group.bench_function("serial", |b| {
    b.iter(|| serial_engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.bench_function("parallel", |b| {
    b.iter(|| parallel_engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.finish();
}

criterion_group!(
  layout_parallel,
  bench_layout_parallel,
  bench_layout_parallel_dense,
  bench_block_parallel,
  bench_flex_parallel,
  bench_grid_parallel
);
criterion_main!(layout_parallel);
