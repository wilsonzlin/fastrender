//! Layout-Specific Benchmarks for FastRender
//!
//! This module provides detailed benchmarks for the layout subsystem:
//! - Block formatting context layout
//! - Flex layout (via Taffy)
//! - Grid layout (via Taffy)
//! - Intrinsic sizing
//! - Layout constraints
//!
//! # Running Benchmarks
//!
//! ```bash
//! cargo bench --bench layout_benches
//! ```

use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::BenchmarkId;
use criterion::Criterion;
use fastrender::geometry::Size;
use fastrender::style::display::Display;
use fastrender::style::display::FormattingContextType;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::BoxNode;
use fastrender::tree::box_tree::BoxTree;
use fastrender::AvailableSpace;
use fastrender::IntrinsicSizingMode;
use fastrender::LayoutConfig;
use fastrender::LayoutConstraints;
use fastrender::LayoutEngine;
use std::sync::Arc;

// ============================================================================
// Helper Functions
// ============================================================================

/// Creates a default computed style
fn default_style() -> Arc<ComputedStyle> {
  Arc::new(ComputedStyle::default())
}

/// Creates a block style with specified dimensions
fn block_style(width: Option<f32>, height: Option<f32>) -> Arc<ComputedStyle> {
  let mut style = ComputedStyle::default();
  style.display = Display::Block;
  if let Some(w) = width {
    style.width = Some(Length::px(w));
  }
  if let Some(h) = height {
    style.height = Some(Length::px(h));
  }
  Arc::new(style)
}

/// Creates a style with margins
fn style_with_margins(top: f32, right: f32, bottom: f32, left: f32) -> Arc<ComputedStyle> {
  let mut style = ComputedStyle::default();
  style.display = Display::Block;
  style.margin_top = Some(Length::px(top));
  style.margin_right = Some(Length::px(right));
  style.margin_bottom = Some(Length::px(bottom));
  style.margin_left = Some(Length::px(left));
  Arc::new(style)
}

/// Creates a style with padding
fn style_with_padding(top: f32, right: f32, bottom: f32, left: f32) -> Arc<ComputedStyle> {
  let mut style = ComputedStyle::default();
  style.display = Display::Block;
  style.padding_top = Length::px(top);
  style.padding_right = Length::px(right);
  style.padding_bottom = Length::px(bottom);
  style.padding_left = Length::px(left);
  Arc::new(style)
}

/// Creates a flex container style
fn flex_style() -> Arc<ComputedStyle> {
  let mut style = ComputedStyle::default();
  style.display = Display::Flex;
  Arc::new(style)
}

/// Creates a grid container style
fn grid_style() -> Arc<ComputedStyle> {
  let mut style = ComputedStyle::default();
  style.display = Display::Grid;
  Arc::new(style)
}

/// Creates a box tree with N children
fn create_wide_tree(child_count: usize) -> BoxTree {
  let children: Vec<BoxNode> = (0..child_count)
    .map(|_| BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]))
    .collect();
  let root = BoxNode::new_block(default_style(), FormattingContextType::Block, children);
  BoxTree::new(root)
}

/// Creates a deeply nested box tree
fn create_deep_tree(depth: usize) -> BoxTree {
  let mut current = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  for _ in 0..depth {
    current = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![current]);
  }
  BoxTree::new(current)
}

/// Creates a flex container with N items
fn create_flex_tree(item_count: usize) -> BoxTree {
  let children: Vec<BoxNode> = (0..item_count)
    .map(|_| {
      BoxNode::new_block(
        block_style(Some(100.0), Some(50.0)),
        FormattingContextType::Block,
        vec![],
      )
    })
    .collect();
  let root = BoxNode::new_block(flex_style(), FormattingContextType::Flex, children);
  BoxTree::new(root)
}

/// Creates a grid container with N items
fn create_grid_tree(item_count: usize) -> BoxTree {
  let children: Vec<BoxNode> = (0..item_count)
    .map(|_| {
      BoxNode::new_block(
        block_style(None, Some(50.0)),
        FormattingContextType::Block,
        vec![],
      )
    })
    .collect();
  let root = BoxNode::new_block(grid_style(), FormattingContextType::Grid, children);
  BoxTree::new(root)
}

// ============================================================================
// Layout Constraints Benchmarks
// ============================================================================

fn bench_layout_constraints(c: &mut Criterion) {
  let mut group = c.benchmark_group("layout_constraints");

  // Constraint creation
  group.bench_function("definite_both", |b| {
    b.iter(|| LayoutConstraints::definite(black_box(800.0), black_box(600.0)))
  });

  group.bench_function("definite_width_only", |b| {
    b.iter(|| LayoutConstraints::definite_width(black_box(800.0)))
  });

  group.bench_function("indefinite", |b| b.iter(LayoutConstraints::indefinite));

  // Constraint access
  let constraints = LayoutConstraints::definite(800.0, 600.0);
  group.bench_function("width_access", |b| {
    b.iter(|| black_box(&constraints).width())
  });

  group.bench_function("height_access", |b| {
    b.iter(|| black_box(&constraints).height())
  });

  group.bench_function("is_fully_definite_check", |b| {
    b.iter(|| black_box(&constraints).is_fully_definite())
  });

  // Available space
  group.bench_function("available_space_definite", |b| {
    b.iter(|| AvailableSpace::Definite(black_box(800.0)))
  });

  group.bench_function("available_space_indefinite", |b| {
    b.iter(|| AvailableSpace::Indefinite)
  });

  group.finish();
}

// ============================================================================
// Block Layout Benchmarks
// ============================================================================

fn bench_block_layout(c: &mut Criterion) {
  let mut group = c.benchmark_group("block_layout");

  let engine = LayoutEngine::with_defaults();

  // Empty block layout
  group.bench_function("empty_block", |b| {
    let tree = BoxTree::new(BoxNode::new_block(
      default_style(),
      FormattingContextType::Block,
      vec![],
    ));
    b.iter(|| engine.layout_tree(black_box(&tree)).unwrap())
  });

  // Block with explicit dimensions
  group.bench_function("sized_block", |b| {
    let tree = BoxTree::new(BoxNode::new_block(
      block_style(Some(400.0), Some(300.0)),
      FormattingContextType::Block,
      vec![],
    ));
    b.iter(|| engine.layout_tree(black_box(&tree)).unwrap())
  });

  // Block with margins
  group.bench_function("block_with_margins", |b| {
    let tree = BoxTree::new(BoxNode::new_block(
      style_with_margins(20.0, 10.0, 20.0, 10.0),
      FormattingContextType::Block,
      vec![],
    ));
    b.iter(|| engine.layout_tree(black_box(&tree)).unwrap())
  });

  // Block with padding
  group.bench_function("block_with_padding", |b| {
    let tree = BoxTree::new(BoxNode::new_block(
      style_with_padding(20.0, 15.0, 20.0, 15.0),
      FormattingContextType::Block,
      vec![],
    ));
    b.iter(|| engine.layout_tree(black_box(&tree)).unwrap())
  });

  // Nested blocks (various depths)
  for depth in [5, 10, 20, 50].iter() {
    let tree = create_deep_tree(*depth);
    group.bench_with_input(BenchmarkId::new("nested_depth", depth), &tree, |b, tree| {
      b.iter(|| engine.layout_tree(black_box(tree)).unwrap())
    });
  }

  // Wide blocks (various child counts)
  for count in [10, 25, 50, 100].iter() {
    let tree = create_wide_tree(*count);
    group.bench_with_input(
      BenchmarkId::new("children_count", count),
      &tree,
      |b, tree| b.iter(|| engine.layout_tree(black_box(tree)).unwrap()),
    );
  }

  group.finish();
}

// ============================================================================
// Flex Layout Benchmarks
// ============================================================================

fn bench_flex_layout(c: &mut Criterion) {
  let mut group = c.benchmark_group("flex_layout");

  let engine = LayoutEngine::with_defaults();

  // Empty flex container
  group.bench_function("empty_flex", |b| {
    let tree = BoxTree::new(BoxNode::new_block(
      flex_style(),
      FormattingContextType::Flex,
      vec![],
    ));
    b.iter(|| engine.layout_tree(black_box(&tree)).unwrap())
  });

  // Flex with various item counts
  for count in [5, 10, 25, 50].iter() {
    let tree = create_flex_tree(*count);
    group.bench_with_input(BenchmarkId::new("items", count), &tree, |b, tree| {
      b.iter(|| engine.layout_tree(black_box(tree)).unwrap())
    });
  }

  group.finish();
}

// ============================================================================
// Grid Layout Benchmarks
// ============================================================================

fn bench_grid_layout(c: &mut Criterion) {
  let mut group = c.benchmark_group("grid_layout");

  let engine = LayoutEngine::with_defaults();

  // Empty grid container
  group.bench_function("empty_grid", |b| {
    let tree = BoxTree::new(BoxNode::new_block(
      grid_style(),
      FormattingContextType::Grid,
      vec![],
    ));
    b.iter(|| engine.layout_tree(black_box(&tree)).unwrap())
  });

  // Grid with various item counts
  for count in [9, 16, 36, 64].iter() {
    let tree = create_grid_tree(*count);
    group.bench_with_input(BenchmarkId::new("items", count), &tree, |b, tree| {
      b.iter(|| engine.layout_tree(black_box(tree)).unwrap())
    });
  }

  group.finish();
}

// ============================================================================
// Intrinsic Sizing Benchmarks
// ============================================================================

fn bench_intrinsic_sizing(c: &mut Criterion) {
  let mut group = c.benchmark_group("intrinsic_sizing");

  let engine = LayoutEngine::with_defaults();

  // Min-content sizing
  group.bench_function("min_content_empty", |b| {
    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    b.iter(|| engine.compute_intrinsic_size(black_box(&box_node), IntrinsicSizingMode::MinContent))
  });

  group.bench_function("min_content_with_children", |b| {
    let children: Vec<BoxNode> = (0..10)
      .map(|_| {
        BoxNode::new_block(
          block_style(Some(100.0), None),
          FormattingContextType::Block,
          vec![],
        )
      })
      .collect();
    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, children);
    b.iter(|| engine.compute_intrinsic_size(black_box(&box_node), IntrinsicSizingMode::MinContent))
  });

  // Max-content sizing
  group.bench_function("max_content_empty", |b| {
    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    b.iter(|| engine.compute_intrinsic_size(black_box(&box_node), IntrinsicSizingMode::MaxContent))
  });

  group.bench_function("max_content_with_children", |b| {
    let children: Vec<BoxNode> = (0..10)
      .map(|_| {
        BoxNode::new_block(
          block_style(Some(100.0), None),
          FormattingContextType::Block,
          vec![],
        )
      })
      .collect();
    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, children);
    b.iter(|| engine.compute_intrinsic_size(black_box(&box_node), IntrinsicSizingMode::MaxContent))
  });

  group.finish();
}

// ============================================================================
// Layout Engine Configuration Benchmarks
// ============================================================================

fn bench_layout_engine(c: &mut Criterion) {
  let mut group = c.benchmark_group("layout_engine_config");

  // Engine creation
  group.bench_function("engine_creation_default", |b| {
    b.iter(LayoutEngine::with_defaults)
  });

  group.bench_function("engine_creation_custom", |b| {
    b.iter(|| {
      let config = LayoutConfig::for_viewport(Size::new(1920.0, 1080.0));
      LayoutEngine::new(config)
    })
  });

  // Config creation
  group.bench_function("config_for_viewport", |b| {
    b.iter(|| LayoutConfig::for_viewport(Size::new(black_box(1920.0), black_box(1080.0))))
  });

  group.bench_function("config_default", |b| b.iter(LayoutConfig::default));

  // Engine reuse (multiple layouts with same engine)
  group.bench_function("engine_reuse_10x", |b| {
    let engine = LayoutEngine::with_defaults();
    b.iter(|| {
      for _ in 0..10 {
        let tree = BoxTree::new(BoxNode::new_block(
          default_style(),
          FormattingContextType::Block,
          vec![],
        ));
        let _ = engine.layout_tree(black_box(&tree));
      }
    })
  });

  group.finish();
}

// ============================================================================
// Criterion Groups and Main
// ============================================================================

criterion_group!(
  benches,
  bench_layout_constraints,
  bench_block_layout,
  bench_flex_layout,
  bench_grid_layout,
  bench_intrinsic_sizing,
  bench_layout_engine,
);

criterion_main!(benches);
