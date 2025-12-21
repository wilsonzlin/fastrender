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
use fastrender::paint::display_list::ClipShape;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::style::color::Rgba;
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
        black_box(5.0),
        black_box(10.0),
        black_box(15.0),
        black_box(20.0),
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
);

criterion_main!(benches);
