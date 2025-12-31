//! Paint-Specific Benchmarks for FastRender
//!
//! This module provides detailed benchmarks for the paint subsystem:
//! - Display list creation and manipulation
//! - Display list builder
//! - Display list optimization
//! - Stacking context tree building
//! - Canvas operations
//! - Rasterization operations
//!
//! # Running Benchmarks
//!
//! ```bash
//! cargo bench --bench paint_benches
//! ```

use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::BenchmarkId;
use criterion::Criterion;
use fastrender::build_stacking_tree;
use fastrender::geometry::Point;
use fastrender::geometry::Rect;
use fastrender::paint::blur::{apply_gaussian_blur, apply_gaussian_blur_anisotropic};
use fastrender::paint::display_list::BorderRadius;
use fastrender::paint::display_list::ClipShape;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::paint::gradient::{
  gradient_bucket, gradient_period, rasterize_conic_gradient, GradientLutCache,
};
use fastrender::style::color::Rgba;
use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::GlyphPosition;
use fastrender::tree::fragment_tree::FragmentNode;
use fastrender::tree::fragment_tree::FragmentTree;
use fastrender::BlendMode;
use fastrender::BorderRadii;
use fastrender::ClipItem;
use fastrender::DisplayItem;
use fastrender::DisplayList;
use fastrender::DisplayListOptimizer;
use fastrender::FillRectItem;
use fastrender::FillRoundedRectItem;
use fastrender::OpacityItem;
use fastrender::OptimizationConfig;
use fastrender::StrokeRectItem;
use fastrender::TextRasterizer;
use tiny_skia::Pixmap;
use tiny_skia::PremultipliedColorU8;
use tiny_skia::SpreadMode;

// ============================================================================
// Helper Functions
// ============================================================================

/// Creates a simple fragment tree for testing
fn create_simple_fragment_tree(child_count: usize) -> FragmentTree {
  let children: Vec<FragmentNode> = (0..child_count)
    .map(|i| {
      FragmentNode::new_block(
        Rect::from_xywh(10.0, (i as f32) * 50.0 + 10.0, 200.0, 40.0),
        vec![],
      )
    })
    .collect();

  let root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 800.0, (child_count as f32) * 50.0 + 20.0),
    children,
  );

  FragmentTree::new(root)
}

/// Creates a deeply nested fragment tree
fn create_deep_fragment_tree(depth: usize) -> FragmentTree {
  let mut current = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 50.0, 50.0), vec![]);

  for i in 0..depth {
    let size = (depth - i) as f32 * 20.0;
    current = FragmentNode::new_block(
      Rect::from_xywh(i as f32 * 5.0, i as f32 * 5.0, size + 100.0, size + 100.0),
      vec![current],
    );
  }

  FragmentTree::new(current)
}

/// Creates a fragment tree with text nodes
fn create_text_fragment_tree(text_count: usize) -> FragmentTree {
  let children: Vec<FragmentNode> = (0..text_count)
    .map(|i| {
      FragmentNode::new_text(
        Rect::from_xywh(10.0, (i as f32) * 20.0 + 10.0, 300.0, 16.0),
        format!("Sample text line number {}", i + 1),
        12.0,
      )
    })
    .collect();

  let root = FragmentNode::new_block(
    Rect::from_xywh(0.0, 0.0, 800.0, (text_count as f32) * 20.0 + 20.0),
    children,
  );

  FragmentTree::new(root)
}

/// Creates a display list with N items
fn create_display_list(item_count: usize) -> DisplayList {
  let mut list = DisplayList::new();

  for i in 0..item_count {
    let y = (i as f32) * 20.0;
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(10.0, y, 100.0, 15.0),
      color: Rgba::new(100, 150, 200, 1.0),
    }));
  }

  list
}

/// Creates a complex display list with various item types
fn create_complex_display_list(item_count: usize) -> DisplayList {
  let mut list = DisplayList::new();

  for i in 0..item_count {
    let y = (i as f32) * 30.0;
    let item_type = i % 5;

    match item_type {
      0 => {
        // Fill rect
        list.push(DisplayItem::FillRect(FillRectItem {
          rect: Rect::from_xywh(10.0, y, 100.0, 25.0),
          color: Rgba::new(100, 150, 200, 1.0),
        }));
      }
      1 => {
        // Rounded rect
        list.push(DisplayItem::FillRoundedRect(FillRoundedRectItem {
          rect: Rect::from_xywh(120.0, y, 100.0, 25.0),
          color: Rgba::new(200, 100, 150, 1.0),
          radii: BorderRadii::uniform(5.0),
        }));
      }
      2 => {
        // Stroke rect
        list.push(DisplayItem::StrokeRect(StrokeRectItem {
          rect: Rect::from_xywh(230.0, y, 100.0, 25.0),
          color: Rgba::new(50, 50, 50, 1.0),
          width: 2.0,
          blend_mode: BlendMode::Normal,
        }));
      }
      3 => {
        // Push/pop opacity
        list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 0.8 }));
        list.push(DisplayItem::FillRect(FillRectItem {
          rect: Rect::from_xywh(340.0, y, 100.0, 25.0),
          color: Rgba::new(150, 200, 100, 1.0),
        }));
        list.push(DisplayItem::PopOpacity);
      }
      4 => {
        // Clip
        list.push(DisplayItem::PushClip(ClipItem {
          shape: ClipShape::Rect {
            rect: Rect::from_xywh(450.0, y, 100.0, 25.0),
            radii: None,
          },
        }));
        list.push(DisplayItem::FillRect(FillRectItem {
          rect: Rect::from_xywh(440.0, y - 5.0, 120.0, 35.0),
          color: Rgba::new(100, 100, 200, 1.0),
        }));
        list.push(DisplayItem::PopClip);
      }
      _ => unreachable!(),
    }
  }

  list
}

/// Creates a display list with items both inside and outside a viewport
fn create_display_list_for_culling(total_items: usize, _viewport_height: f32) -> DisplayList {
  let mut list = DisplayList::new();
  let item_height = 20.0;
  let _total_height = (total_items as f32) * item_height;

  for i in 0..total_items {
    let y = (i as f32) * item_height;
    list.push(DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(10.0, y, 100.0, item_height - 5.0),
      color: Rgba::new(100, 150, 200, 1.0),
    }));
  }

  list
}

// ============================================================================
// Display List Benchmarks
// ============================================================================

fn bench_display_list_creation(c: &mut Criterion) {
  let mut group = c.benchmark_group("display_list_creation");

  // Empty list creation
  group.bench_function("empty_list", |b| b.iter(DisplayList::new));

  // Adding items
  group.bench_function("add_fill_rect", |b| {
    let item = DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(10.0, 20.0, 100.0, 50.0),
      color: Rgba::new(100, 150, 200, 1.0),
    });
    b.iter(|| {
      let mut list = DisplayList::new();
      list.push(black_box(item.clone()));
      list
    })
  });

  // Building lists of various sizes
  for count in [10, 50, 100, 500].iter() {
    group.bench_with_input(BenchmarkId::new("build_list", count), count, |b, &count| {
      b.iter(|| create_display_list(black_box(count)))
    });
  }

  // Complex lists
  for count in [10, 50, 100].iter() {
    group.bench_with_input(
      BenchmarkId::new("complex_list", count),
      count,
      |b, &count| b.iter(|| create_complex_display_list(black_box(count))),
    );
  }

  group.finish();
}

fn bench_display_list_operations(c: &mut Criterion) {
  let mut group = c.benchmark_group("display_list_operations");

  // List iteration
  let list_100 = create_display_list(100);
  group.bench_function("iterate_100_items", |b| {
    b.iter(|| {
      let mut count = 0;
      for item in black_box(&list_100).iter() {
        if item.bounds().is_some() {
          count += 1;
        }
      }
      count
    })
  });

  // Bounds calculation
  group.bench_function("bounds_calculation", |b| {
    let item = DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(10.0, 20.0, 100.0, 50.0),
      color: Rgba::new(100, 150, 200, 1.0),
    });
    b.iter(|| black_box(&item).bounds())
  });

  // List length
  let list_500 = create_display_list(500);
  group.bench_function("list_length", |b| b.iter(|| black_box(&list_500).len()));

  // List is_empty check
  group.bench_function("is_empty_check", |b| {
    b.iter(|| black_box(&list_500).is_empty())
  });

  group.finish();
}

// ============================================================================
// Display List Builder Benchmarks
// ============================================================================

fn bench_display_list_builder(c: &mut Criterion) {
  let mut group = c.benchmark_group("display_list_builder");

  // Builder creation
  group.bench_function("builder_creation", |b| b.iter(DisplayListBuilder::new));

  // Building from simple trees
  for count in [5, 10, 25, 50].iter() {
    let tree = create_simple_fragment_tree(*count);
    group.bench_with_input(BenchmarkId::new("simple_tree", count), &tree, |b, tree| {
      b.iter(|| {
        let builder = DisplayListBuilder::new();
        builder.build_tree(black_box(tree))
      })
    });
  }

  // Building from deep trees
  for depth in [5, 10, 20].iter() {
    let tree = create_deep_fragment_tree(*depth);
    group.bench_with_input(BenchmarkId::new("deep_tree", depth), &tree, |b, tree| {
      b.iter(|| {
        let builder = DisplayListBuilder::new();
        builder.build_tree(black_box(tree))
      })
    });
  }

  // Building from text-heavy trees
  for count in [10, 25, 50].iter() {
    let tree = create_text_fragment_tree(*count);
    group.bench_with_input(BenchmarkId::new("text_tree", count), &tree, |b, tree| {
      b.iter(|| {
        let builder = DisplayListBuilder::new();
        builder.build_tree(black_box(tree))
      })
    });
  }

  group.finish();
}

// ============================================================================
// Display List Optimization Benchmarks
// ============================================================================

fn bench_display_list_optimization(c: &mut Criterion) {
  let mut group = c.benchmark_group("display_list_optimization");

  // Optimizer creation
  group.bench_function("optimizer_creation", |b| b.iter(DisplayListOptimizer::new));

  // Config creation
  group.bench_function("config_default", |b| b.iter(OptimizationConfig::default));

  // Full optimization on various list sizes
  let viewport = Rect::from_xywh(0.0, 0.0, 800.0, 600.0);

  for count in [50, 100, 200, 500].iter() {
    let list = create_display_list(*count);
    group.bench_with_input(
      BenchmarkId::new("full_optimize", count),
      &list,
      |b, list| {
        let optimizer = DisplayListOptimizer::new();
        b.iter(|| optimizer.optimize(black_box(list.clone()), black_box(viewport)))
      },
    );
  }

  // Optimization with culling (items outside viewport)
  for count in [100, 200, 500].iter() {
    let list = create_display_list_for_culling(*count, 300.0);
    let viewport = Rect::from_xywh(0.0, 0.0, 800.0, 300.0); // Half the items outside
    group.bench_with_input(BenchmarkId::new("with_culling", count), &list, |b, list| {
      let optimizer = DisplayListOptimizer::new();
      b.iter(|| optimizer.optimize(black_box(list.clone()), black_box(viewport)))
    });
  }

  // Complex lists optimization
  for count in [25, 50, 100].iter() {
    let list = create_complex_display_list(*count);
    group.bench_with_input(
      BenchmarkId::new("complex_optimize", count),
      &list,
      |b, list| {
        let optimizer = DisplayListOptimizer::new();
        b.iter(|| optimizer.optimize(black_box(list.clone()), black_box(viewport)))
      },
    );
  }

  // Optimization with different configs
  let list = create_display_list(100);

  group.bench_function("no_culling", |b| {
    let config = OptimizationConfig {
      enable_culling: false,
      ..Default::default()
    };
    let optimizer = DisplayListOptimizer::with_config(config);
    b.iter(|| optimizer.optimize(black_box(list.clone()), black_box(viewport)))
  });

  group.bench_function("no_fill_merging", |b| {
    let config = OptimizationConfig {
      enable_fill_merging: false,
      ..Default::default()
    };
    let optimizer = DisplayListOptimizer::with_config(config);
    b.iter(|| optimizer.optimize(black_box(list.clone()), black_box(viewport)))
  });

  group.bench_function("minimal_optimization", |b| {
    let config = OptimizationConfig {
      enable_culling: false,
      enable_transparent_removal: false,
      enable_fill_merging: false,
      enable_noop_removal: false,
    };
    let optimizer = DisplayListOptimizer::with_config(config);
    b.iter(|| optimizer.optimize(black_box(list.clone()), black_box(viewport)))
  });

  group.finish();
}

// ============================================================================
// Stacking Context Benchmarks
// ============================================================================

fn bench_stacking_context(c: &mut Criterion) {
  let mut group = c.benchmark_group("stacking_context");

  // Building stacking tree from simple fragment trees
  for count in [5, 10, 25, 50].iter() {
    let tree = create_simple_fragment_tree(*count);
    group.bench_with_input(BenchmarkId::new("simple_tree", count), &tree, |b, tree| {
      b.iter(|| build_stacking_tree(black_box(&tree.root), None, true))
    });
  }

  // Building stacking tree from deep fragment trees
  for depth in [5, 10, 20].iter() {
    let tree = create_deep_fragment_tree(*depth);
    group.bench_with_input(BenchmarkId::new("deep_tree", depth), &tree, |b, tree| {
      b.iter(|| build_stacking_tree(black_box(&tree.root), None, true))
    });
  }

  group.finish();
}

// ============================================================================
// Display Item Creation Benchmarks
// ============================================================================

fn bench_display_item_creation(c: &mut Criterion) {
  let mut group = c.benchmark_group("display_item_creation");

  // FillRect item
  group.bench_function("fill_rect", |b| {
    b.iter(|| {
      DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(
          black_box(10.0),
          black_box(20.0),
          black_box(100.0),
          black_box(50.0),
        ),
        color: Rgba::new(
          black_box(100),
          black_box(150),
          black_box(200),
          black_box(1.0),
        ),
      })
    })
  });

  // FillRoundedRect item
  group.bench_function("fill_rounded_rect", |b| {
    b.iter(|| {
      DisplayItem::FillRoundedRect(FillRoundedRectItem {
        rect: Rect::from_xywh(
          black_box(10.0),
          black_box(20.0),
          black_box(100.0),
          black_box(50.0),
        ),
        color: Rgba::new(
          black_box(100),
          black_box(150),
          black_box(200),
          black_box(1.0),
        ),
        radii: BorderRadii::uniform(black_box(5.0)),
      })
    })
  });

  // StrokeRect item
  group.bench_function("stroke_rect", |b| {
    b.iter(|| {
      DisplayItem::StrokeRect(StrokeRectItem {
        rect: Rect::from_xywh(
          black_box(10.0),
          black_box(20.0),
          black_box(100.0),
          black_box(50.0),
        ),
        color: Rgba::new(black_box(50), black_box(50), black_box(50), black_box(1.0)),
        width: black_box(2.0),
        blend_mode: BlendMode::Normal,
      })
    })
  });

  // Opacity item
  group.bench_function("opacity_item", |b| {
    b.iter(|| {
      DisplayItem::PushOpacity(OpacityItem {
        opacity: black_box(0.5),
      })
    })
  });

  // Clip item
  group.bench_function("clip_item", |b| {
    b.iter(|| {
      DisplayItem::PushClip(ClipItem {
        shape: ClipShape::Rect {
          rect: Rect::from_xywh(
            black_box(10.0),
            black_box(20.0),
            black_box(100.0),
            black_box(50.0),
          ),
          radii: None,
        },
      })
    })
  });

  // BorderRadii creation
  group.bench_function("border_radii_uniform", |b| {
    b.iter(|| BorderRadii::uniform(black_box(5.0)))
  });

  group.bench_function("border_radii_custom", |b| {
    b.iter(|| {
      BorderRadii::new(
        BorderRadius::uniform(black_box(5.0)),
        BorderRadius::uniform(black_box(10.0)),
        BorderRadius::uniform(black_box(15.0)),
        BorderRadius::uniform(black_box(20.0)),
      )
    })
  });

  // Rgba color creation
  group.bench_function("rgba_creation", |b| {
    b.iter(|| {
      Rgba::new(
        black_box(100),
        black_box(150),
        black_box(200),
        black_box(1.0),
      )
    })
  });

  group.finish();
}

// ============================================================================
// Fragment Tree Operations Benchmarks
// ============================================================================

fn bench_fragment_tree_operations(c: &mut Criterion) {
  let mut group = c.benchmark_group("fragment_tree_operations");

  // Fragment node creation
  group.bench_function("new_block_fragment", |b| {
    b.iter(|| {
      FragmentNode::new_block(
        Rect::from_xywh(
          black_box(10.0),
          black_box(20.0),
          black_box(100.0),
          black_box(50.0),
        ),
        vec![],
      )
    })
  });

  group.bench_function("new_text_fragment", |b| {
    b.iter(|| {
      FragmentNode::new_text(
        Rect::from_xywh(
          black_box(10.0),
          black_box(20.0),
          black_box(200.0),
          black_box(16.0),
        ),
        String::from("Sample text content"),
        black_box(12.0),
      )
    })
  });

  // Fragment tree creation
  group.bench_function("tree_creation", |b| {
    b.iter(|| {
      let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 800.0, 600.0), vec![]);
      FragmentTree::new(root)
    })
  });

  // Fragment tree with children
  for count in [5, 10, 25].iter() {
    group.bench_with_input(
      BenchmarkId::new("tree_with_children", count),
      count,
      |b, &count| b.iter(|| create_simple_fragment_tree(black_box(count))),
    );
  }

  // Contains point check
  let fragment = FragmentNode::new_block(Rect::from_xywh(10.0, 20.0, 100.0, 50.0), vec![]);
  group.bench_function("contains_point", |b| {
    let point = Point::new(50.0, 40.0);
    b.iter(|| black_box(&fragment).contains_point(black_box(point)))
  });

  // Bounds access
  group.bench_function("bounds_access", |b| b.iter(|| black_box(&fragment).bounds));

  group.finish();
}

// ============================================================================
// Stress Tests
// ============================================================================

fn bench_paint_stress_tests(c: &mut Criterion) {
  let mut group = c.benchmark_group("paint_stress_tests");
  group.sample_size(10); // Fewer samples for expensive tests

  // Very large display list
  let large_list = create_display_list(2000);
  let viewport = Rect::from_xywh(0.0, 0.0, 800.0, 600.0);
  group.bench_function("optimize_2000_items", |b| {
    let optimizer = DisplayListOptimizer::new();
    b.iter(|| optimizer.optimize(black_box(large_list.clone()), black_box(viewport)))
  });

  // Very deep fragment tree
  let deep_tree = create_deep_fragment_tree(100);
  group.bench_function("build_depth_100", |b| {
    b.iter(|| {
      let builder = DisplayListBuilder::new();
      builder.build_tree(black_box(&deep_tree))
    })
  });

  // Very wide fragment tree
  let wide_tree = create_simple_fragment_tree(500);
  group.bench_function("build_500_children", |b| {
    b.iter(|| {
      let builder = DisplayListBuilder::new();
      builder.build_tree(black_box(&wide_tree))
    })
  });

  // Complex display list with all item types
  let complex_list = create_complex_display_list(500);
  group.bench_function("optimize_complex_500", |b| {
    let optimizer = DisplayListOptimizer::new();
    b.iter(|| optimizer.optimize(black_box(complex_list.clone()), black_box(viewport)))
  });

  group.finish();
}

// ============================================================================
// Text Rasterization Benchmarks
// ============================================================================

fn bench_text_rasterizer_cache(c: &mut Criterion) {
  let font_ctx = FontContext::new();
  if !font_ctx.has_fonts() {
    return;
  }

  let font = match font_ctx.get_sans_serif() {
    Some(font) => font,
    None => return,
  };

  let face = match font.as_ttf_face() {
    Ok(face) => face,
    Err(_) => return,
  };

  let glyph_id = match face.glyph_index('a') {
    Some(glyph) => glyph.0 as u32,
    None => return,
  };

  let glyphs: Vec<GlyphPosition> = (0..20)
    .map(|i| GlyphPosition {
      glyph_id,
      cluster: i,
      x_offset: 0.0,
      y_offset: 0.0,
      x_advance: 12.0,
      y_advance: 0.0,
    })
    .collect();

  let mut rasterizer = TextRasterizer::new();
  let mut pixmap = Pixmap::new(400, 200).unwrap();
  let mut group = c.benchmark_group("text_rasterizer_cache");
  group.bench_function("reuse_cached_glyphs", |b| {
    b.iter(|| {
      rasterizer.reset_cache_stats();
      // Warm the cache once
      let _ = rasterizer.render_glyphs(&glyphs, &font, 18.0, 10.0, 50.0, Rgba::BLACK, &mut pixmap);

      for i in 0..5 {
        let baseline = 20.0 + (i as f32) * 6.0;
        let _ = rasterizer.render_glyphs(
          &glyphs,
          &font,
          18.0,
          10.0,
          baseline,
          Rgba::BLACK,
          &mut pixmap,
        );
      }

      black_box(rasterizer.cache_stats());
    })
  });

  group.finish();
}

/// Benchmark serial vs parallel display list painting to highlight tiling gains.
fn bench_parallel_display_list_raster(c: &mut Criterion) {
  let mut list = DisplayList::new();
  for y in 0..32 {
    for x in 0..32 {
      let px = (x * 10) as f32;
      let py = (y * 10) as f32;
      list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(px, py, 8.0, 8.0),
        color: Rgba::new((x * 7) as u8, (y * 5) as u8, 120, 1.0),
      }));
    }
  }

  let font_ctx = FontContext::new();
  let width = 512;
  let height = 512;

  let mut group = c.benchmark_group("display_list_paint_parallelism");
  group.bench_function("serial", |b| {
    b.iter(|| {
      let renderer =
        DisplayListRenderer::new(width, height, Rgba::WHITE, font_ctx.clone()).unwrap();
      black_box(renderer.render(&list).unwrap());
    });
  });
  group.bench_function("parallel_tiles", |b| {
    let parallelism = PaintParallelism {
      enabled: true,
      tile_size: 128,
      log_timing: false,
      min_display_items: 1,
      min_tiles: 1,
      min_build_fragments: 1,
      build_chunk_size: 1,
      ..PaintParallelism::default()
    };
    b.iter(|| {
      let renderer = DisplayListRenderer::new(width, height, Rgba::WHITE, font_ctx.clone())
        .unwrap()
        .with_parallelism(parallelism);
      black_box(renderer.render(&list).unwrap());
    });
  });
  group.finish();
}

fn bench_filter_blur(c: &mut Criterion) {
  let mut group = c.benchmark_group("filters_blur");
  group.sample_size(10);

  for size in [256u32, 1024, 2048] {
    let mut base = Pixmap::new(size, size).expect("pixmap");
    {
      let width = base.width() as usize;
      let pixels = base.pixels_mut();
      let color = PremultipliedColorU8::from_rgba(200, 80, 60, 255).unwrap();
      let start = size / 3;
      let end = (size * 2) / 3;
      for y in start..end {
        let row = y as usize * width;
        for x in start..end {
          pixels[row + x as usize] = color;
        }
      }
    }

    group.bench_with_input(
      BenchmarkId::new("gaussian_blur_isotropic_sigma3", size),
      &base,
      |b, base| {
        b.iter(|| {
          let mut pixmap = base.clone();
          apply_gaussian_blur(&mut pixmap, 3.0).unwrap();
          black_box(pixmap);
        })
      },
    );

    group.bench_with_input(
      BenchmarkId::new("gaussian_blur_anisotropic_sigma2_5", size),
      &base,
      |b, base| {
        b.iter(|| {
          let mut pixmap = base.clone();
          apply_gaussian_blur_anisotropic(&mut pixmap, 2.0, 5.0).unwrap();
          black_box(pixmap);
        })
      },
    );
  }

  group.finish();
}

fn naive_conic_gradient(
  width: u32,
  height: u32,
  center: Point,
  start_angle: f32,
  stops: &[(f32, Rgba)],
  spread: SpreadMode,
  period: f32,
) -> Pixmap {
  let mut pixmap = Pixmap::new(width, height).expect("pixmap");
  let stride = width as usize;
  let pixels = pixmap.pixels_mut();
  let inv_two_pi = 0.5 / std::f32::consts::PI;
  for y in 0..height as usize {
    let dy = y as f32 + 0.5 - center.y;
    for x in 0..width as usize {
      let dx = x as f32 + 0.5 - center.x;
      let mut pos = (dx.atan2(-dy) + start_angle) * inv_two_pi;
      pos = pos.rem_euclid(1.0) * period;
      match spread {
        SpreadMode::Repeat => {
          if period > 0.0 {
            pos = pos.rem_euclid(period);
          }
        }
        SpreadMode::Reflect => {
          let two_p = period * 2.0;
          let mut v = pos.rem_euclid(two_p);
          if v > period {
            v = two_p - v;
          }
          pos = v;
        }
        SpreadMode::Pad => {
          pos = pos.clamp(0.0, period);
        }
      }
      let color = sample_stop_color(stops, pos);
      pixels[y * stride + x] = PremultipliedColorU8::from_rgba(
        color.r,
        color.g,
        color.b,
        (color.a * 255.0).round().clamp(0.0, 255.0) as u8,
      )
      .unwrap_or(PremultipliedColorU8::TRANSPARENT);
    }
  }
  pixmap
}

fn sample_stop_color(stops: &[(f32, Rgba)], pos: f32) -> Rgba {
  if stops.is_empty() {
    return Rgba::TRANSPARENT;
  }
  if pos <= stops[0].0 {
    return stops[0].1;
  }
  for window in stops.windows(2) {
    let (p0, c0) = window[0];
    let (p1, c1) = window[1];
    if pos <= p1 {
      let span = (p1 - p0).max(1e-6);
      let frac = ((pos - p0) / span).clamp(0.0, 1.0);
      return Rgba {
        r: (c0.r as f32 + (c1.r as f32 - c0.r as f32) * frac)
          .round()
          .clamp(0.0, 255.0) as u8,
        g: (c0.g as f32 + (c1.g as f32 - c0.g as f32) * frac)
          .round()
          .clamp(0.0, 255.0) as u8,
        b: (c0.b as f32 + (c1.b as f32 - c0.b as f32) * frac)
          .round()
          .clamp(0.0, 255.0) as u8,
        a: c0.a + (c1.a - c0.a) * frac,
      };
    }
  }
  stops.last().map(|(_, c)| *c).unwrap_or(Rgba::TRANSPARENT)
}

fn bench_conic_gradient(c: &mut Criterion) {
  let width: u32 = 256;
  let height: u32 = 256;
  let center = Point::new(width as f32 / 2.0, height as f32 / 2.0);
  let stops = vec![
    (0.0, Rgba::new(255, 0, 0, 1.0)),
    (0.45, Rgba::new(0, 255, 0, 1.0)),
    (1.0, Rgba::new(0, 0, 255, 1.0)),
  ];
  let cache = GradientLutCache::default();
  let bucket = gradient_bucket(width.max(height).saturating_mul(2));
  let period = gradient_period(&stops);

  let mut group = c.benchmark_group("conic_gradient_fill");
  group.bench_function("cached_lut", |b| {
    b.iter(|| {
      black_box(
        rasterize_conic_gradient(
          width,
          height,
          center,
          0.25,
          SpreadMode::Repeat,
          &stops,
          &cache,
          bucket,
        )
        .expect("pixmap"),
      )
    });
  });
  group.bench_function("naive", |b| {
    b.iter(|| {
      black_box(naive_conic_gradient(
        width,
        height,
        center,
        0.25,
        &stops,
        SpreadMode::Repeat,
        period,
      ))
    });
  });
  group.finish();
}

// ============================================================================
// Criterion Groups and Main
// ============================================================================

criterion_group!(
  benches,
  bench_display_list_creation,
  bench_display_list_operations,
  bench_display_list_builder,
  bench_display_list_optimization,
  bench_stacking_context,
  bench_display_item_creation,
  bench_fragment_tree_operations,
  bench_paint_stress_tests,
  bench_parallel_display_list_raster,
  bench_text_rasterizer_cache,
  bench_filter_blur,
  bench_conic_gradient,
);

criterion_main!(benches);
