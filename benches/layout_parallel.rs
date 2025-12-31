use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::layout::engine::LayoutParallelism;
use fastrender::style::display::Display;
use fastrender::style::position::Position;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::text::font_loader::FontContext;
use fastrender::{BoxNode, BoxTree, FormattingContextType, LayoutConfig, LayoutEngine, Size};
use std::{env, sync::Arc};

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

fn build_flex_sibling_containers(
  flex_containers: usize,
  children_per_flex: usize,
  nested_flex: bool,
) -> BoxTree {
  // Layout fan-out at the root block exercises shared flex measurement cache contention when
  // multiple flex formatting contexts run in parallel. Optional nested flex containers make the
  // inner measure cache hotter and increase cross-thread reuse pressure.
  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;

  let mut flex_style = ComputedStyle::default();
  flex_style.display = Display::Flex;

  let mut item_style = ComputedStyle::default();
  item_style.display = Display::Block;
  item_style.padding_left = Length::px(2.0);
  item_style.padding_right = Length::px(2.0);

  let mut nested_item_style = ComputedStyle::default();
  nested_item_style.display = Display::Block;
  nested_item_style.padding_left = Length::px(1.0);
  nested_item_style.padding_right = Length::px(1.0);

  let mut nested_flex_style = ComputedStyle::default();
  nested_flex_style.display = Display::Flex;

  let item_style = Arc::new(item_style);
  let nested_item_style = Arc::new(nested_item_style);
  let nested_flex_style = Arc::new(nested_flex_style);
  let flex_style = Arc::new(flex_style);
  let root_style = Arc::new(root_style);

  let mut flex_nodes = Vec::with_capacity(flex_containers);
  for flex_idx in 0..flex_containers {
    let mut items = Vec::with_capacity(children_per_flex);
    for child_idx in 0..children_per_flex {
      let mut child_contents = Vec::new();
      let text = BoxNode::new_text(
        item_style.clone(),
        format!(
          "flex-{flex_idx}-{child_idx} lorem ipsum dolor sit amet, consectetur adipiscing elit"
        ),
      );
      child_contents.push(text);

      if nested_flex {
        let mut nested_items = Vec::new();
        for nested_idx in 0..3 {
          let nested_text = BoxNode::new_text(
            nested_item_style.clone(),
            format!("nested-{flex_idx}-{child_idx}-{nested_idx} quick fox jumps over the lazy dog"),
          );
          let nested_child = BoxNode::new_block(
            nested_item_style.clone(),
            FormattingContextType::Block,
            vec![nested_text],
          );
          nested_items.push(nested_child);
        }
        child_contents.push(BoxNode::new_block(
          nested_flex_style.clone(),
          FormattingContextType::Flex,
          nested_items,
        ));
      }

      let child = BoxNode::new_block(
        item_style.clone(),
        FormattingContextType::Block,
        child_contents,
      );
      items.push(child);
    }

    let flex = BoxNode::new_block(flex_style.clone(), FormattingContextType::Flex, items);
    flex_nodes.push(flex);
  }

  let root = BoxNode::new_block(root_style, FormattingContextType::Block, flex_nodes);
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

fn build_flex_sibling_containers_fixed(
  flex_containers: usize,
  children_per_flex: usize,
) -> BoxTree {
  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;

  let mut flex_style = ComputedStyle::default();
  flex_style.display = Display::Flex;

  let mut item_style = ComputedStyle::default();
  item_style.display = Display::Block;
  item_style.width = Some(Length::px(8.0));
  item_style.height = Some(Length::px(8.0));

  let root_style = Arc::new(root_style);
  let flex_style = Arc::new(flex_style);
  let item_style = Arc::new(item_style);

  let mut flex_nodes = Vec::with_capacity(flex_containers);
  for _ in 0..flex_containers {
    let mut items = Vec::with_capacity(children_per_flex);
    for _ in 0..children_per_flex {
      items.push(BoxNode::new_block(
        item_style.clone(),
        FormattingContextType::Block,
        vec![],
      ));
    }
    flex_nodes.push(BoxNode::new_block(
      flex_style.clone(),
      FormattingContextType::Flex,
      items,
    ));
  }

  let root = BoxNode::new_block(root_style, FormattingContextType::Block, flex_nodes);
  BoxTree::new(root)
}

fn build_grid_sibling_containers_fixed(
  grid_containers: usize,
  children_per_grid: usize,
) -> BoxTree {
  use fastrender::style::types::GridAutoFlow;

  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;

  let mut grid_style = ComputedStyle::default();
  grid_style.display = Display::Grid;
  grid_style.grid_auto_flow = GridAutoFlow::Row;
  grid_style.width = Some(Length::px(640.0));

  let mut item_style = ComputedStyle::default();
  item_style.display = Display::Block;
  item_style.width = Some(Length::px(8.0));
  item_style.height = Some(Length::px(8.0));

  let root_style = Arc::new(root_style);
  let grid_style = Arc::new(grid_style);
  let item_style = Arc::new(item_style);

  let mut grids = Vec::with_capacity(grid_containers);
  for _ in 0..grid_containers {
    let mut items = Vec::with_capacity(children_per_grid);
    for _ in 0..children_per_grid {
      items.push(BoxNode::new_block(
        item_style.clone(),
        FormattingContextType::Block,
        vec![],
      ));
    }
    grids.push(BoxNode::new_block(
      grid_style.clone(),
      FormattingContextType::Grid,
      items,
    ));
  }

  let root = BoxNode::new_block(root_style, FormattingContextType::Block, grids);
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

fn bench_flex_cache_contention_case(
  c: &mut Criterion,
  flex_containers: usize,
  children_per_flex: usize,
  nested_flex: bool,
) {
  // Construct many sibling flex containers to trigger layout fan-out. Each benchmark run compares
  // serial vs parallel layout with cold caches as well as a two-pass sequence that reuses
  // flex/layout caches on the second pass to quantify contention and reuse wins.
  let box_tree = build_flex_sibling_containers(flex_containers, children_per_flex, nested_flex);
  let viewport = Size::new(1440.0, 960.0);
  let font_ctx = FontContext::new();

  let mut serial_config = LayoutConfig::for_viewport(viewport);
  serial_config.enable_cache = true;
  let serial_engine = LayoutEngine::with_font_context(serial_config, font_ctx.clone());

  let parallelism = LayoutParallelism::enabled(8)
    .with_min_fanout(4)
    .with_max_threads(Some(num_cpus::get().max(2)));
  let mut parallel_config = LayoutConfig::for_viewport(viewport).with_parallelism(parallelism);
  parallel_config.enable_cache = true;
  let parallel_engine = LayoutEngine::with_font_context(parallel_config, font_ctx);

  let flex_cache_disabled = env::var("FASTR_DISABLE_FLEX_CACHE")
    .ok()
    .map(|v| {
      let trimmed = v.trim();
      !(trimmed.is_empty() || trimmed == "0" || trimmed.eq_ignore_ascii_case("false"))
    })
    .unwrap_or(false);
  let cache_suffix = if flex_cache_disabled {
    "flex_cache_disabled"
  } else {
    "flex_cache_enabled"
  };
  let nested_suffix = if nested_flex { "nested" } else { "flat" };

  let mut group = c.benchmark_group(format!(
    "layout_flex_cache_contention_{}x{}_{}_{}",
    flex_containers, children_per_flex, nested_suffix, cache_suffix
  ));
  group.bench_function("serial_reset_caches", |b| {
    b.iter(|| serial_engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.bench_function("serial_reuse_two_pass", |b| {
    b.iter(|| {
      serial_engine.layout_tree(black_box(&box_tree)).unwrap();
      serial_engine
        .layout_tree_reuse_caches(black_box(&box_tree))
        .unwrap();
    })
  });
  group.bench_function("parallel_reset_caches", |b| {
    b.iter(|| parallel_engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.bench_function("parallel_reuse_two_pass", |b| {
    b.iter(|| {
      parallel_engine.layout_tree(black_box(&box_tree)).unwrap();
      parallel_engine
        .layout_tree_reuse_caches(black_box(&box_tree))
        .unwrap();
    })
  });
  group.finish();
}

fn bench_flex_cache_contention(c: &mut Criterion) {
  bench_flex_cache_contention_case(c, 8, 24, false);
  bench_flex_cache_contention_case(c, 32, 64, true);
}

fn bench_taffy_node_cache_contention(c: &mut Criterion) {
  let viewport = Size::new(1280.0, 900.0);
  let font_ctx = FontContext::new();

  let mut serial_config = LayoutConfig::for_viewport(viewport);
  serial_config.enable_cache = true;
  let serial_engine = LayoutEngine::with_font_context(serial_config, font_ctx.clone());

  let parallelism = LayoutParallelism::enabled(8)
    .with_min_fanout(4)
    .with_max_threads(Some(num_cpus::get().max(2)));
  let mut parallel_config = LayoutConfig::for_viewport(viewport).with_parallelism(parallelism);
  parallel_config.enable_cache = true;
  let parallel_engine = LayoutEngine::with_font_context(parallel_config, font_ctx);

  let flex_tree = build_flex_sibling_containers_fixed(256, 8);
  serial_engine.layout_tree(&flex_tree).unwrap();
  parallel_engine.layout_tree(&flex_tree).unwrap();

  let mut group = c.benchmark_group("layout_taffy_node_cache_contention_flex");
  group.bench_function("serial_reuse", |b| {
    b.iter(|| {
      serial_engine
        .layout_tree_reuse_caches(black_box(&flex_tree))
        .unwrap()
    })
  });
  group.bench_function("parallel_reuse", |b| {
    b.iter(|| {
      parallel_engine
        .layout_tree_reuse_caches(black_box(&flex_tree))
        .unwrap()
    })
  });
  group.finish();

  let grid_tree = build_grid_sibling_containers_fixed(256, 16);
  serial_engine.layout_tree(&grid_tree).unwrap();
  parallel_engine.layout_tree(&grid_tree).unwrap();

  let mut group = c.benchmark_group("layout_taffy_node_cache_contention_grid");
  group.bench_function("serial_reuse", |b| {
    b.iter(|| {
      serial_engine
        .layout_tree_reuse_caches(black_box(&grid_tree))
        .unwrap()
    })
  });
  group.bench_function("parallel_reuse", |b| {
    b.iter(|| {
      parallel_engine
        .layout_tree_reuse_caches(black_box(&grid_tree))
        .unwrap()
    })
  });
  group.finish();
}

criterion_group!(
  layout_parallel,
  bench_layout_parallel,
  bench_layout_parallel_dense,
  bench_block_parallel,
  bench_flex_parallel,
  bench_flex_cache_contention,
  bench_taffy_node_cache_contention,
  bench_grid_parallel
);
criterion_main!(layout_parallel);
