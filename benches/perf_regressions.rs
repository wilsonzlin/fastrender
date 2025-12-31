use std::sync::Arc;
use std::time::Duration;

use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};
use fastrender::geometry::Rect;
use fastrender::layout::fragmentation::{fragment_tree, FragmentationOptions};
use fastrender::layout::table::{TableFormattingContext, TableStructure};
use fastrender::style::display::Display;
use fastrender::style::types::BorderCollapse;
use fastrender::tree::fragment_tree::FragmentNode;
use fastrender::{
  BoxNode, BoxTree, ComputedStyle, FormattingContextFactory, FormattingContextType, LayoutConfig,
  LayoutEngine, Length, Size,
};

mod common;

const FLEX_TEXT: &str =
  "FastRender should reuse shaped text for repeated carousels instead of reshaping every item.";

fn flex_text_tree(item_count: usize) -> BoxTree {
  let mut flex_style = ComputedStyle::default();
  flex_style.display = Display::Flex;
  let flex_style = Arc::new(flex_style);
  let block_style = Arc::new(ComputedStyle::default());
  let inline_style = Arc::new(ComputedStyle::default());
  let text_style = Arc::new(ComputedStyle::default());

  let mut children = Vec::with_capacity(item_count);
  for idx in 0..item_count {
    let text = format!("Item {}: {} {}", idx, FLEX_TEXT, FLEX_TEXT);
    let text_node = BoxNode::new_text(text_style.clone(), text);
    let inline = BoxNode::new_block(
      inline_style.clone(),
      FormattingContextType::Inline,
      vec![text_node],
    );
    children.push(BoxNode::new_block(
      block_style.clone(),
      FormattingContextType::Block,
      vec![inline],
    ));
  }

  let root = BoxNode::new_block(flex_style, FormattingContextType::Flex, children);
  BoxTree::new(root)
}

fn clear_box_ids(node: &mut BoxNode) {
  node.id = 0;
  for child in &mut node.children {
    clear_box_ids(child);
  }
}

fn find_first_table<'a>(node: &'a BoxNode) -> Option<&'a BoxNode> {
  if matches!(node.style.display, Display::Table | Display::InlineTable)
    || matches!(
      node.formatting_context(),
      Some(FormattingContextType::Table)
    )
  {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_first_table(child) {
      return Some(found);
    }
  }
  None
}

fn perf_criterion() -> Criterion {
  Criterion::default()
    .sample_size(25)
    .warm_up_time(Duration::from_secs(1))
    .measurement_time(Duration::from_secs(4))
    .configure_from_args()
}

fn bench_parse_dom(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_parse_dom");

  for (name, html) in [
    ("block_simple", common::BLOCK_SIMPLE_HTML),
    ("form_controls", common::FORM_CONTROLS_HTML),
  ] {
    group.bench_function(name, |b| b.iter(|| common::parse_dom(black_box(html))));
  }

  group.finish();
}

fn bench_css_parse(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_css_parse");

  let small_media = common::media_context(common::SMALL_VIEWPORT);
  let small_dom = common::parse_dom(common::BLOCK_SIMPLE_HTML);
  let small_css = common::inline_css_text(&small_dom, &small_media);
  group.bench_function("block_simple", |b| {
    b.iter(|| common::parse_stylesheet_text(black_box(&small_css)))
  });

  let realistic_media = common::media_context(common::REALISTIC_VIEWPORT);
  let realistic_dom = common::parse_dom(common::FORM_CONTROLS_HTML);
  let realistic_css = common::inline_css_text(&realistic_dom, &realistic_media);
  group.bench_function("form_controls", |b| {
    b.iter(|| common::parse_stylesheet_text(black_box(&realistic_css)))
  });

  group.finish();
}

fn bench_cascade(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_cascade");

  let small_media = common::media_context(common::SMALL_VIEWPORT);
  let small_dom = common::parse_dom(common::BLOCK_SIMPLE_HTML);
  let small_sheet = common::stylesheet_for_dom(&small_dom, &small_media);
  group.bench_function("block_simple", |b| {
    b.iter(|| common::cascade(black_box(&small_dom), black_box(&small_sheet), &small_media))
  });

  let realistic_media = common::media_context(common::REALISTIC_VIEWPORT);
  let realistic_dom = common::parse_dom(common::FORM_CONTROLS_HTML);
  let realistic_sheet = common::stylesheet_for_dom(&realistic_dom, &realistic_media);
  group.bench_function("form_controls", |b| {
    b.iter(|| {
      common::cascade(
        black_box(&realistic_dom),
        black_box(&realistic_sheet),
        &realistic_media,
      )
    })
  });

  group.finish();
}

fn bench_box_generation(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_box_generation");

  let media = common::media_context(common::REALISTIC_VIEWPORT);
  let dom = common::parse_dom(common::FORM_CONTROLS_HTML);
  let sheet = common::stylesheet_for_dom(&dom, &media);
  let styled = common::cascade(&dom, &sheet, &media);
  group.bench_function("form_controls", |b| {
    b.iter(|| common::box_tree_from_styled(black_box(&styled)))
  });

  group.finish();
}

fn bench_layout_block(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_layout_block");
  let viewport = (960, 720);
  let media = common::media_context(viewport);
  let dom = common::parse_dom(common::BLOCK_SIMPLE_HTML);
  let sheet = common::stylesheet_for_dom(&dom, &media);
  let styled = common::cascade(&dom, &sheet, &media);
  let box_tree = common::box_tree_from_styled(&styled);
  let font_ctx = common::fixed_font_context();
  let engine = common::layout_engine(viewport, &font_ctx);

  group.bench_function("block_simple", |b| {
    b.iter(|| engine.layout_tree(black_box(&box_tree)).unwrap())
  });

  group.finish();
}

fn bench_layout_flex(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_layout_flex");
  let viewport = (960, 720);
  let media = common::media_context(viewport);
  let dom = common::parse_dom(common::FLEX_HTML);
  let sheet = common::stylesheet_for_dom(&dom, &media);
  let styled = common::cascade(&dom, &sheet, &media);
  let box_tree = common::box_tree_from_styled(&styled);
  let font_ctx = common::fixed_font_context();
  let engine = common::layout_engine(viewport, &font_ctx);
  let positioned_dom = common::parse_dom(common::FLEX_POSITIONED_HTML);
  let positioned_sheet = common::stylesheet_for_dom(&positioned_dom, &media);
  let positioned_styled = common::cascade(&positioned_dom, &positioned_sheet, &media);
  let positioned_box_tree = common::box_tree_from_styled(&positioned_styled);
  let positioned_engine = common::layout_engine(viewport, &font_ctx);

  group.bench_function("flex_grow_shrink", |b| {
    b.iter(|| engine.layout_tree(black_box(&box_tree)).unwrap())
  });
  group.bench_function("flex_positioned_children", |b| {
    b.iter(|| {
      positioned_engine
        .layout_tree(black_box(&positioned_box_tree))
        .unwrap()
    })
  });

  group.finish();
}

fn bench_layout_flex_cached_text(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_layout_flex_cached_text");
  let viewport = common::REALISTIC_VIEWPORT;
  let font_ctx = common::fixed_font_context();
  let mut config = LayoutConfig::for_viewport(Size::new(viewport.0 as f32, viewport.1 as f32));
  config.enable_cache = true;
  let engine = LayoutEngine::with_font_context(config, font_ctx);
  let box_tree = flex_text_tree(48);

  group.bench_function("cached_second_pass", |b| {
    b.iter_batched(
      || {
        engine.layout_tree(&box_tree).expect("seed flex layout");
      },
      |_| {
        engine
          .layout_tree_reuse_caches(&box_tree)
          .expect("cached flex layout");
        black_box(engine.shaping_cache_size());
      },
      BatchSize::SmallInput,
    )
  });

  group.finish();
}

fn bench_layout_grid(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_layout_grid");
  let viewport = (1100, 820);
  let media = common::media_context(viewport);
  let dom = common::parse_dom(common::GRID_HTML);
  let sheet = common::stylesheet_for_dom(&dom, &media);
  let styled = common::cascade(&dom, &sheet, &media);
  let box_tree = common::box_tree_from_styled(&styled);
  let font_ctx = common::fixed_font_context();
  let engine = common::layout_engine(viewport, &font_ctx);

  group.bench_function("grid_template", |b| {
    b.iter(|| engine.layout_tree(black_box(&box_tree)).unwrap())
  });

  group.finish();
}

fn bench_layout_table(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_layout_table");
  let viewport = (960, 720);
  let media = common::media_context(viewport);
  let dom = common::parse_dom(common::TABLE_HTML);
  let sheet = common::stylesheet_for_dom(&dom, &media);
  let styled = common::cascade(&dom, &sheet, &media);
  let box_tree = common::box_tree_from_styled(&styled);
  let font_ctx = common::fixed_font_context();
  let engine = common::layout_engine(viewport, &font_ctx);

  group.bench_function("table_span", |b| {
    b.iter(|| engine.layout_tree(black_box(&box_tree)).unwrap())
  });

  group.finish();
}

fn bench_layout_table_stress(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_layout_table_stress");
  let viewport = (1400, 1200);
  let media = common::media_context(viewport);
  let font_ctx = common::fixed_font_context();

  {
    let dom = common::parse_dom(common::TABLE_LARGE_ROWSPAN_HTML);
    let sheet = common::stylesheet_for_dom(&dom, &media);
    let styled = common::cascade(&dom, &sheet, &media);
    let box_tree = common::box_tree_from_styled(&styled);
    let engine = common::layout_engine(viewport, &font_ctx);

    group.bench_function("table_large_rowspan", |b| {
      b.iter(|| engine.layout_tree(black_box(&box_tree)).unwrap())
    });
  }

  {
    let dom = common::parse_dom(common::TABLE_COLLAPSE_LARGE_HTML);
    let sheet = common::stylesheet_for_dom(&dom, &media);
    let styled = common::cascade(&dom, &sheet, &media);
    let box_tree = common::box_tree_from_styled(&styled);
    let engine = common::layout_engine(viewport, &font_ctx);

    group.bench_function("table_collapse_large", |b| {
      b.iter(|| engine.layout_tree(black_box(&box_tree)).unwrap())
    });
  }

  group.finish();
}

fn bench_table_intrinsic(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_table_intrinsic");
  let viewport = (1400, 1200);
  let media = common::media_context(viewport);
  let font_ctx = common::fixed_font_context();
  let factory = FormattingContextFactory::with_font_context_and_viewport(
    font_ctx,
    Size::new(viewport.0 as f32, viewport.1 as f32),
  );
  let tfc = TableFormattingContext::with_factory(factory);

  // Benchmark A: column constraints + distribution for a real large table fixture.
  {
    let dom = common::parse_dom(common::TABLE_LARGE_ROWSPAN_HTML);
    let sheet = common::stylesheet_for_dom(&dom, &media);
    let styled = common::cascade(&dom, &sheet, &media);
    let box_tree = common::box_tree_from_styled(&styled);
    let table_box = find_first_table(&box_tree.root).expect("fixture should contain a table");

    // Avoid layout cache interactions so the benchmark reflects real per-run work.
    let mut table_box = table_box.clone();
    clear_box_ids(&mut table_box);
    let structure = TableStructure::from_box_tree(&table_box);

    // The fixture uses a fixed width (1200px). Feed the same width as the available content size
    // so this bench focuses on intrinsic sizing + constraint distribution.
    let available_content_width = 1200.0;
    let percent_base = Some(available_content_width);
    group.bench_function("table_column_constraints_only", |b| {
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
  }

  // Benchmark B: intrinsic cell measurement in isolation on a synthetic workload.
  {
    const CELL_COUNT: usize = 1024;
    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.padding_left = Length::px(4.0);
    cell_style.padding_right = Length::px(4.0);
    let cell_style = Arc::new(cell_style);
    let inline_style = Arc::new(ComputedStyle::default());
    let text_style = Arc::new(ComputedStyle::default());
    let mut cells = Vec::with_capacity(CELL_COUNT);
    for _ in 0..CELL_COUNT {
      let text_node = BoxNode::new_text(text_style.clone(), "Synthetic cell content".to_string());
      let inline = BoxNode::new_block(
        inline_style.clone(),
        FormattingContextType::Inline,
        vec![text_node],
      );
      cells.push(BoxNode::new_block(
        cell_style.clone(),
        FormattingContextType::Block,
        vec![inline],
      ));
    }

    group.bench_function("table_cell_intrinsic_only", |b| {
      b.iter(|| {
        let totals = tfc.bench_measure_cells_intrinsic_widths(
          black_box(&cells),
          BorderCollapse::Separate,
          Some(800.0),
        );
        black_box(totals);
      })
    });
  }

  group.finish();
}

fn bench_fragmentation_many_pages(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_fragmentation");
  let blocks_per_fragment = 8;
  let block_height = 40.0;
  let page_count = 64;
  let fragmentainer_size = blocks_per_fragment as f32 * block_height;
  let root = tall_fragment_tree(page_count * blocks_per_fragment, block_height, 600.0);
  let options = FragmentationOptions::new(fragmentainer_size).with_gap(8.0);

  group.bench_function("tall_stack", |b| {
    b.iter(|| black_box(fragment_tree(black_box(&root), black_box(&options))))
  });

  group.finish();
}

fn tall_fragment_tree(block_count: usize, block_height: f32, inline_size: f32) -> FragmentNode {
  let mut children = Vec::with_capacity(block_count);
  let mut y = 0.0;
  for _ in 0..block_count {
    children.push(FragmentNode::new_block(
      Rect::from_xywh(0.0, y, inline_size, block_height),
      Vec::new(),
    ));
    y += block_height;
  }

  FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, inline_size, y), children)
}

fn bench_paint_display_list_build(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_paint_display_list_build");
  let viewport = common::REALISTIC_VIEWPORT;
  let media = common::media_context(viewport);
  let dom = common::parse_dom(common::FORM_CONTROLS_HTML);
  let sheet = common::stylesheet_for_dom(&dom, &media);
  let styled = common::cascade(&dom, &sheet, &media);
  let box_tree = common::box_tree_from_styled(&styled);
  let font_ctx = common::fixed_font_context();
  let engine = common::layout_engine(viewport, &font_ctx);
  let fragments = common::layout_fragment_tree(&engine, &box_tree);

  group.bench_function("form_controls", |b| {
    b.iter(|| common::build_display_list(black_box(&fragments), &font_ctx))
  });

  group.finish();
}

fn bench_paint_optimize(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_paint_optimize");
  let viewport = common::REALISTIC_VIEWPORT;
  let media = common::media_context(viewport);
  let dom = common::parse_dom(common::FORM_CONTROLS_HTML);
  let sheet = common::stylesheet_for_dom(&dom, &media);
  let styled = common::cascade(&dom, &sheet, &media);
  let box_tree = common::box_tree_from_styled(&styled);
  let font_ctx = common::fixed_font_context();
  let engine = common::layout_engine(viewport, &font_ctx);
  let fragments = common::layout_fragment_tree(&engine, &box_tree);
  let base_list = common::build_display_list(&fragments, &font_ctx);

  group.bench_function("form_controls", |b| {
    b.iter(|| common::optimize_display_list(black_box(&base_list), viewport))
  });

  group.finish();
}

fn bench_paint_rasterize(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_paint_rasterize");
  let viewport = common::REALISTIC_VIEWPORT;
  let media = common::media_context(viewport);
  let dom = common::parse_dom(common::FORM_CONTROLS_HTML);
  let sheet = common::stylesheet_for_dom(&dom, &media);
  let styled = common::cascade(&dom, &sheet, &media);
  let box_tree = common::box_tree_from_styled(&styled);
  let font_ctx = common::fixed_font_context();
  let engine = common::layout_engine(viewport, &font_ctx);
  let fragments = common::layout_fragment_tree(&engine, &box_tree);
  let base_list = common::build_display_list(&fragments, &font_ctx);
  let optimized = common::optimize_display_list(&base_list, viewport);

  group.bench_function("form_controls", |b| {
    b.iter(|| common::rasterize_display_list(black_box(&optimized), viewport, &font_ctx))
  });

  group.finish();
}

fn bench_end_to_end_small(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_end_to_end_small");
  let viewport = common::SMALL_VIEWPORT;
  let font_ctx = common::fixed_font_context();

  group.bench_function("block_simple", |b| {
    b.iter(|| common::render_pipeline(common::BLOCK_SIMPLE_HTML, viewport, &font_ctx))
  });

  group.finish();
}

fn bench_end_to_end_realistic(c: &mut Criterion) {
  let mut group = c.benchmark_group("bench_end_to_end_realistic");
  let viewport = common::REALISTIC_VIEWPORT;
  let font_ctx = common::fixed_font_context();

  group.bench_function("form_controls", |b| {
    b.iter(|| common::render_pipeline(common::FORM_CONTROLS_HTML, viewport, &font_ctx))
  });

  group.finish();
}

criterion_group!(
  name = benches;
  config = perf_criterion();
  targets =
    bench_parse_dom,
    bench_css_parse,
    bench_cascade,
    bench_box_generation,
    bench_layout_block,
    bench_layout_flex,
    bench_layout_flex_cached_text,
    bench_layout_grid,
    bench_layout_table,
    bench_layout_table_stress,
    bench_table_intrinsic,
    bench_fragmentation_many_pages,
    bench_paint_display_list_build,
    bench_paint_optimize,
    bench_paint_rasterize,
    bench_end_to_end_small,
    bench_end_to_end_realistic
);
criterion_main!(benches);
