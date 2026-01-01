//! Microbenchmarks for layout hotspots.
//!
//! These benchmarks are intentionally small and synthetic: they run quickly, stay offline,
//! and isolate known layout hotspots so regressions show up in a targeted `cargo bench`.
//!
//! The goal is not to mimic full-page rendering. Instead, each benchmark stresses a tight loop
//! that has historically been performance sensitive:
//! - Flex item measurement (Taffy measure callback)
//! - Block intrinsic sizing (min/max-content width)
//! - Table cell intrinsic sizing + column width distribution

use std::sync::Arc;
use std::time::Duration;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::layout::contexts::block::BlockFormattingContext;
use fastrender::layout::engine::LayoutParallelism;
use fastrender::layout::table::{TableFormattingContext, TableStructure};
use fastrender::layout::taffy_integration::{
  enable_taffy_counters, reset_taffy_counters, taffy_counters, taffy_perf_counters,
  TaffyPerfCountersGuard,
};
use fastrender::style::display::Display;
use fastrender::style::types::{BorderCollapse, BorderStyle, FlexWrap, TableLayout};
use fastrender::style::values::Length;
use fastrender::{
  BoxNode, BoxTree, ComputedStyle, FormattingContext, FormattingContextFactory, FormattingContextType,
  IntrinsicSizingMode, LayoutConfig, LayoutEngine, Size,
};

mod common;

fn micro_criterion() -> Criterion {
  // Keep this bench target quick to run locally. Some of these hotspots can take hundreds
  // of milliseconds per iteration, so prefer fewer samples over long measurement windows.
  Criterion::default()
    .sample_size(10)
    .warm_up_time(Duration::from_millis(200))
    .measurement_time(Duration::from_millis(600))
    .configure_from_args()
}

fn build_flex_measure_tree(item_count: usize) -> BoxTree {
  // Regression protected:
  // - Flex layout can become dominated by repeated item measurement (Taffy calling the measure
  //   callback for leaf nodes). This tree is constructed so every flex item requires intrinsic
  //   measurement (text + inline formatting context).
  const TEXT: &str = "lorem ipsum dolor sit amet consectetur adipiscing elit";

  let mut flex_style = ComputedStyle::default();
  flex_style.display = Display::Flex;
  flex_style.flex_wrap = FlexWrap::Wrap;
  // A narrow container forces wrap decisions and tends to increase measurement pressure.
  flex_style.width = Some(Length::px(420.0));
  let flex_style = Arc::new(flex_style);

  let mut item_style = ComputedStyle::default();
  item_style.display = Display::Block;
  item_style.padding_left = Length::px(2.0);
  item_style.padding_right = Length::px(2.0);
  let item_style = Arc::new(item_style);

  let mut inline_style = ComputedStyle::default();
  inline_style.display = Display::Block;
  let inline_style = Arc::new(inline_style);

  let mut text_style = ComputedStyle::default();
  text_style.display = Display::Inline;
  let text_style = Arc::new(text_style);

  let mut children = Vec::with_capacity(item_count);
  for idx in 0..item_count {
    let text = BoxNode::new_text(text_style.clone(), format!("item-{idx} {TEXT}"));
    let inline = BoxNode::new_block(
      inline_style.clone(),
      FormattingContextType::Inline,
      vec![text],
    );
    children.push(BoxNode::new_block(
      item_style.clone(),
      FormattingContextType::Block,
      vec![inline],
    ));
  }

  let root = BoxNode::new_block(flex_style, FormattingContextType::Flex, children);
  BoxTree::new(root)
}

fn build_block_intrinsic_tree(span_count: usize) -> BoxTree {
  // Regression protected:
  // - Block intrinsic sizing relies heavily on inline item collection + text measurement.
  //   Changes that accidentally re-shape text repeatedly or allocate excessively will show up
  //   here without the noise of full layout.
  const WORD: &str = "supercalifragilisticexpialidocious";
  const FILL: &str = "the quick brown fox jumps over the lazy dog";

  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;
  let root_style = Arc::new(root_style);

  let mut span_style = ComputedStyle::default();
  span_style.display = Display::Inline;
  let span_style = Arc::new(span_style);

  let mut text_style = ComputedStyle::default();
  text_style.display = Display::Inline;
  let text_style = Arc::new(text_style);

  let mut children = Vec::with_capacity(span_count * 2);
  for idx in 0..span_count {
    let payload = if idx % 8 == 0 {
      format!("{WORD} {FILL} {FILL}")
    } else {
      format!("{FILL} {FILL}")
    };
    let text = BoxNode::new_text(text_style.clone(), payload);
    let span = BoxNode::new_inline(span_style.clone(), vec![text]);
    children.push(span);
    // Explicit separator so max-content width includes multiple segments.
    children.push(BoxNode::new_text(text_style.clone(), " ".to_string()));
  }

  let root = BoxNode::new_block(root_style, FormattingContextType::Block, children);
  BoxTree::new(root)
}

fn build_block_intrinsic_many_runs_tree(run_count: usize) -> BoxTree {
  // Regression protected:
  // - Block intrinsic sizing may have to flush many short inline runs when block-level boxes are
  //   mixed into otherwise inline content. This stresses the inline-run cache keying path in
  //   `BlockFormattingContext::compute_intrinsic_inline_size`.

  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;
  let root_style = Arc::new(root_style);

  let mut inline_style = ComputedStyle::default();
  inline_style.display = Display::Inline;
  let inline_style = Arc::new(inline_style);

  let mut separator_style = ComputedStyle::default();
  separator_style.display = Display::Block;
  separator_style.containment.inline_size = true;
  let separator_style = Arc::new(separator_style);

  let mut children = Vec::with_capacity(run_count.saturating_mul(2).saturating_sub(1));
  for idx in 0..run_count {
    children.push(BoxNode::new_inline(inline_style.clone(), Vec::new()));
    if idx + 1 < run_count {
      children.push(BoxNode::new_block(
        separator_style.clone(),
        FormattingContextType::Block,
        vec![],
      ));
    }
  }

  let root = BoxNode::new_block(root_style, FormattingContextType::Block, children);
  BoxTree::new(root)
}

fn build_table_tree(rows: usize, cols: usize) -> BoxNode {
  // Regression protected:
  // - Table auto layout measures min/max-content widths for every cell, then distributes
  //   widths across columns. The complexity tends to scale with cell count, so keep the
  //   table moderate but non-trivial.
  const CELL_TEXT: &str = "Table cell text: lorem ipsum dolor sit amet";

  let mut table_style = ComputedStyle::default();
  table_style.display = Display::Table;
  table_style.table_layout = TableLayout::Auto;
  table_style.border_collapse = BorderCollapse::Separate;
  table_style.width = Some(Length::px(960.0));
  let table_style = Arc::new(table_style);

  let mut group_style = ComputedStyle::default();
  group_style.display = Display::TableRowGroup;
  let group_style = Arc::new(group_style);

  let mut row_style = ComputedStyle::default();
  row_style.display = Display::TableRow;
  let row_style = Arc::new(row_style);

  let mut cell_style = ComputedStyle::default();
  cell_style.display = Display::TableCell;
  cell_style.padding_left = Length::px(4.0);
  cell_style.padding_right = Length::px(4.0);
  cell_style.padding_top = Length::px(2.0);
  cell_style.padding_bottom = Length::px(2.0);
  cell_style.border_left_width = Length::px(1.0);
  cell_style.border_right_width = Length::px(1.0);
  cell_style.border_top_width = Length::px(1.0);
  cell_style.border_bottom_width = Length::px(1.0);
  cell_style.border_left_style = BorderStyle::Solid;
  cell_style.border_right_style = BorderStyle::Solid;
  cell_style.border_top_style = BorderStyle::Solid;
  cell_style.border_bottom_style = BorderStyle::Solid;
  let cell_style = Arc::new(cell_style);

  let mut inline_style = ComputedStyle::default();
  inline_style.display = Display::Block;
  let inline_style = Arc::new(inline_style);

  let mut text_style = ComputedStyle::default();
  text_style.display = Display::Inline;
  let text_style = Arc::new(text_style);

  let mut row_nodes = Vec::with_capacity(rows);
  for r in 0..rows {
    let mut cells = Vec::with_capacity(cols);
    for c in 0..cols {
      let text = BoxNode::new_text(
        text_style.clone(),
        format!("r{r}c{c} {CELL_TEXT} {CELL_TEXT}"),
      );
      let inline = BoxNode::new_block(
        inline_style.clone(),
        FormattingContextType::Inline,
        vec![text],
      );
      cells.push(BoxNode::new_block(
        cell_style.clone(),
        FormattingContextType::Block,
        vec![inline],
      ));
    }
    row_nodes.push(BoxNode::new_block(
      row_style.clone(),
      FormattingContextType::Block,
      cells,
    ));
  }

  let row_group = BoxNode::new_block(group_style, FormattingContextType::Block, row_nodes);
  BoxNode::new_block(table_style, FormattingContextType::Table, vec![row_group])
}

fn bench_flex_measure_hot_path(c: &mut Criterion) {
  let viewport = Size::new(800.0, 600.0);
  let font_ctx = common::fixed_font_context();
  let engine = LayoutEngine::with_font_context(
    LayoutConfig::for_viewport(viewport).with_parallelism(LayoutParallelism::disabled()),
    font_ctx,
  );
  // 96 flex items * ~3 nodes/item ~= 289 nodes total.
  let box_tree = build_flex_measure_tree(96);

  // Capture some counters once so the benchmark documents its own workload.
  {
    let _taffy_stats_guard = enable_taffy_counters(true);
    reset_taffy_counters();
    let _taffy_perf_guard = TaffyPerfCountersGuard::new();
    let _ = engine
      .layout_tree(black_box(&box_tree))
      .expect("flex layout should succeed");
    let perf = taffy_perf_counters();
    let usage = taffy_counters();
    // If these ever hit zero, this benchmark is no longer exercising the flex measurement path.
    assert!(perf.flex_measure_calls > 0);
    eprintln!(
      "layout_hotspots flex_measure: taffy_flex_measure_calls={} taffy_flex_compute_cpu_ms={:.2} taffy_nodes_built={} taffy_nodes_reused={}",
      perf.flex_measure_calls,
      perf.flex_compute_ns as f64 / 1_000_000.0,
      usage.flex_nodes_built,
      usage.flex_nodes_reused,
    );
  }

  let mut group = c.benchmark_group("layout_hotspots_flex_measure");
  group.bench_function("flex_layout_single_pass", |b| {
    b.iter(|| {
      let fragments = engine
        .layout_tree(black_box(&box_tree))
        .expect("flex layout should succeed");
      black_box(fragments);
    })
  });
  group.finish();
}

fn bench_block_intrinsic_sizing(c: &mut Criterion) {
  let viewport = Size::new(800.0, 600.0);
  let font_ctx = common::fixed_font_context();
  let bfc = BlockFormattingContext::with_font_context_and_viewport(font_ctx, viewport)
    .with_parallelism(LayoutParallelism::disabled());
  let mut tree = build_block_intrinsic_tree(64);
  // Disable global intrinsic caching for the root so each iteration recomputes the intrinsic width.
  tree.root.id = 0;
  let node = &tree.root;

  let mut group = c.benchmark_group("layout_hotspots_block_intrinsic");
  group.bench_function("min_content", |b| {
    b.iter(|| {
      let width = bfc
        .compute_intrinsic_inline_size(black_box(node), IntrinsicSizingMode::MinContent)
        .expect("intrinsic sizing should succeed");
      black_box(width);
    })
  });
  group.bench_function("max_content", |b| {
    b.iter(|| {
      let width = bfc
        .compute_intrinsic_inline_size(black_box(node), IntrinsicSizingMode::MaxContent)
        .expect("intrinsic sizing should succeed");
      black_box(width);
    })
  });
  group.bench_function("min_and_max", |b| {
    b.iter(|| {
      let min = bfc
        .compute_intrinsic_inline_size(black_box(node), IntrinsicSizingMode::MinContent)
        .expect("intrinsic sizing should succeed");
      let max = bfc
        .compute_intrinsic_inline_size(black_box(node), IntrinsicSizingMode::MaxContent)
        .expect("intrinsic sizing should succeed");
      black_box((min, max));
    })
  });
  group.finish();
}

fn bench_block_intrinsic_many_inline_runs(c: &mut Criterion) {
  let viewport = Size::new(800.0, 600.0);
  let font_ctx = common::fixed_font_context();
  let bfc = BlockFormattingContext::with_font_context_and_viewport(font_ctx, viewport)
    .with_parallelism(LayoutParallelism::disabled());
  let mut tree = build_block_intrinsic_many_runs_tree(512);
  // Disable global intrinsic caching for the root so each iteration recomputes the intrinsic width.
  tree.root.id = 0;
  let node = &tree.root;

  let mut group = c.benchmark_group("layout_hotspots_block_intrinsic_many_runs");
  group.bench_function("min_content", |b| {
    b.iter(|| {
      let width = bfc
        .compute_intrinsic_inline_size(black_box(node), IntrinsicSizingMode::MinContent)
        .expect("intrinsic sizing should succeed");
      black_box(width);
    })
  });
  group.bench_function("max_content", |b| {
    b.iter(|| {
      let width = bfc
        .compute_intrinsic_inline_size(black_box(node), IntrinsicSizingMode::MaxContent)
        .expect("intrinsic sizing should succeed");
      black_box(width);
    })
  });
  group.finish();
}

fn bench_table_cell_intrinsic_and_distribution(c: &mut Criterion) {
  let viewport = Size::new(960.0, 720.0);
  let font_ctx = common::fixed_font_context();
  let factory = FormattingContextFactory::with_font_context_and_viewport(font_ctx, viewport)
    .with_parallelism(LayoutParallelism::disabled());
  let tfc = TableFormattingContext::with_factory(factory);

  let table_box = build_table_tree(12, 12);
  let structure = TableStructure::from_box_tree(&table_box);
  let available_content_width = 960.0;
  let percent_base = Some(available_content_width);

  let mut group = c.benchmark_group("layout_hotspots_table_intrinsic");
  group.bench_function("cell_intrinsic_and_distribution_12x12", |b| {
    b.iter(|| {
      let widths = tfc.bench_column_constraints_and_distribute(
        black_box(&table_box),
        black_box(&structure),
        black_box(available_content_width),
        percent_base,
      );
      black_box(widths);
    })
  });
  group.finish();
}

criterion_group!(
  name = benches;
  config = micro_criterion();
  targets =
    bench_flex_measure_hot_path,
    bench_block_intrinsic_sizing,
    bench_block_intrinsic_many_inline_runs,
    bench_table_cell_intrinsic_and_distribution
);
criterion_main!(benches);
