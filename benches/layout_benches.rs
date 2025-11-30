//! Layout-Specific Benchmarks for FastRender
//!
//! This module provides detailed benchmarks for the layout subsystem:
//! - Block formatting context layout
//! - Flex layout (via Taffy)
//! - Grid layout (via Taffy)
//! - Table layout
//! - Margin collapsing
//! - Width computation
//! - Intrinsic sizing
//! - Layout constraints
//! - Float context
//!
//! # Running Benchmarks
//!
//! ```bash
//! cargo bench --bench layout_benches
//! ```

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use fastrender::geometry::{Rect, Size};
use fastrender::layout::constraints::{AvailableSpace, LayoutConstraints};
use fastrender::layout::contexts::block::{
    collapse_margins, compute_block_width, CollapsibleMargin, MarginCollapseContext,
};
use fastrender::layout::float_context::{FloatContext, FloatInfo, FloatSide};
use fastrender::layout::formatting_context::IntrinsicSizingMode;
use fastrender::layout::{LayoutConfig, LayoutEngine};
use fastrender::style::{ComputedStyles, Display, Length};
use fastrender::tree::{BoxNode, BoxTree, FormattingContextType};
use std::sync::Arc;

// ============================================================================
// Helper Functions
// ============================================================================

/// Creates a default computed style
fn default_style() -> Arc<ComputedStyles> {
    Arc::new(ComputedStyles::default())
}

/// Creates a block style with specified dimensions
fn block_style(width: Option<f32>, height: Option<f32>) -> Arc<ComputedStyles> {
    let mut style = ComputedStyles::default();
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
fn style_with_margins(top: f32, right: f32, bottom: f32, left: f32) -> Arc<ComputedStyles> {
    let mut style = ComputedStyles::default();
    style.display = Display::Block;
    style.margin_top = Some(Length::px(top));
    style.margin_right = Some(Length::px(right));
    style.margin_bottom = Some(Length::px(bottom));
    style.margin_left = Some(Length::px(left));
    Arc::new(style)
}

/// Creates a style with padding
fn style_with_padding(top: f32, right: f32, bottom: f32, left: f32) -> Arc<ComputedStyles> {
    let mut style = ComputedStyles::default();
    style.display = Display::Block;
    style.padding_top = Length::px(top);
    style.padding_right = Length::px(right);
    style.padding_bottom = Length::px(bottom);
    style.padding_left = Length::px(left);
    Arc::new(style)
}

/// Creates a flex container style
fn flex_style() -> Arc<ComputedStyles> {
    let mut style = ComputedStyles::default();
    style.display = Display::Flex;
    Arc::new(style)
}

/// Creates a grid container style
fn grid_style() -> Arc<ComputedStyles> {
    let mut style = ComputedStyles::default();
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
        .map(|_| BoxNode::new_block(block_style(Some(100.0), Some(50.0)), FormattingContextType::Block, vec![]))
        .collect();
    let root = BoxNode::new_block(flex_style(), FormattingContextType::Flex, children);
    BoxTree::new(root)
}

/// Creates a grid container with N items
fn create_grid_tree(item_count: usize) -> BoxTree {
    let children: Vec<BoxNode> = (0..item_count)
        .map(|_| BoxNode::new_block(block_style(None, Some(50.0)), FormattingContextType::Block, vec![]))
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

    group.bench_function("indefinite", |b| {
        b.iter(|| LayoutConstraints::indefinite())
    });

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
        group.bench_with_input(BenchmarkId::new("children_count", count), &tree, |b, tree| {
            b.iter(|| engine.layout_tree(black_box(tree)).unwrap())
        });
    }

    group.finish();
}

// ============================================================================
// Width Computation Benchmarks
// ============================================================================

fn bench_width_computation(c: &mut Criterion) {
    let mut group = c.benchmark_group("width_computation");

    // Auto width
    let auto_style = ComputedStyles::default();
    group.bench_function("auto_width", |b| {
        b.iter(|| compute_block_width(black_box(&auto_style), black_box(800.0)))
    });

    // Explicit width
    let mut explicit_style = ComputedStyles::default();
    explicit_style.width = Some(Length::px(400.0));
    group.bench_function("explicit_width", |b| {
        b.iter(|| compute_block_width(black_box(&explicit_style), black_box(800.0)))
    });

    // Percentage width
    let mut percent_style = ComputedStyles::default();
    percent_style.width = Some(Length::percent(50.0));
    group.bench_function("percent_width", |b| {
        b.iter(|| compute_block_width(black_box(&percent_style), black_box(800.0)))
    });

    // With min-width
    let mut min_style = ComputedStyles::default();
    min_style.min_width = Some(Length::px(200.0));
    group.bench_function("with_min_width", |b| {
        b.iter(|| compute_block_width(black_box(&min_style), black_box(800.0)))
    });

    // With max-width
    let mut max_style = ComputedStyles::default();
    max_style.max_width = Some(Length::px(600.0));
    group.bench_function("with_max_width", |b| {
        b.iter(|| compute_block_width(black_box(&max_style), black_box(800.0)))
    });

    // Complex style with all properties
    let mut complex_style = ComputedStyles::default();
    complex_style.width = Some(Length::percent(75.0));
    complex_style.min_width = Some(Length::px(200.0));
    complex_style.max_width = Some(Length::px(600.0));
    complex_style.margin_left = Some(Length::px(20.0));
    complex_style.margin_right = Some(Length::px(20.0));
    complex_style.padding_left = Length::px(15.0);
    complex_style.padding_right = Length::px(15.0);
    group.bench_function("complex_width", |b| {
        b.iter(|| compute_block_width(black_box(&complex_style), black_box(800.0)))
    });

    group.finish();
}

// ============================================================================
// Margin Collapse Benchmarks
// ============================================================================

fn bench_margin_collapse(c: &mut Criterion) {
    let mut group = c.benchmark_group("margin_collapse");

    // Simple collapse (two positive margins)
    group.bench_function("simple_positive", |b| {
        b.iter(|| collapse_margins(black_box(20.0), black_box(30.0)))
    });

    // Negative margins
    group.bench_function("with_negative", |b| {
        b.iter(|| collapse_margins(black_box(20.0), black_box(-10.0)))
    });

    // Both negative
    group.bench_function("both_negative", |b| {
        b.iter(|| collapse_margins(black_box(-15.0), black_box(-25.0)))
    });

    // Zero margins
    group.bench_function("zero_margins", |b| {
        b.iter(|| collapse_margins(black_box(0.0), black_box(0.0)))
    });

    // Collapsible margin creation
    group.bench_function("collapsible_margin_new", |b| {
        b.iter(|| CollapsibleMargin::new(black_box(20.0), black_box(0.0)))
    });

    // Margin collapse context operations
    group.bench_function("context_push_resolve", |b| {
        b.iter(|| {
            let mut ctx = MarginCollapseContext::new();
            ctx.push_margin(black_box(20.0));
            ctx.push_margin(black_box(30.0));
            ctx.resolve()
        })
    });

    // Multiple margin collapse sequence
    group.bench_function("multiple_collapses", |b| {
        b.iter(|| {
            let mut ctx = MarginCollapseContext::new();
            for margin in [10.0, 20.0, 15.0, 30.0, 25.0].iter() {
                ctx.push_margin(black_box(*margin));
            }
            ctx.resolve()
        })
    });

    group.finish();
}

// ============================================================================
// Float Context Benchmarks
// ============================================================================

fn bench_float_context(c: &mut Criterion) {
    let mut group = c.benchmark_group("float_context");

    // Float context creation
    group.bench_function("context_creation", |b| {
        b.iter(|| FloatContext::new(black_box(800.0)))
    });

    // Adding floats
    group.bench_function("add_float_left", |b| {
        let mut ctx = FloatContext::new(800.0);
        let float_info = FloatInfo {
            rect: Rect::from_xywh(0.0, 0.0, 100.0, 50.0),
            side: FloatSide::Left,
        };
        b.iter(|| ctx.add_float(black_box(float_info.clone())))
    });

    group.bench_function("add_float_right", |b| {
        let mut ctx = FloatContext::new(800.0);
        let float_info = FloatInfo {
            rect: Rect::from_xywh(700.0, 0.0, 100.0, 50.0),
            side: FloatSide::Right,
        };
        b.iter(|| ctx.add_float(black_box(float_info.clone())))
    });

    // Available width calculation with floats
    group.bench_function("available_width_empty", |b| {
        let ctx = FloatContext::new(800.0);
        b.iter(|| ctx.available_width_at_y(black_box(0.0)))
    });

    group.bench_function("available_width_with_floats", |b| {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float(FloatInfo {
            rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
            side: FloatSide::Left,
        });
        ctx.add_float(FloatInfo {
            rect: Rect::from_xywh(700.0, 0.0, 100.0, 80.0),
            side: FloatSide::Right,
        });
        b.iter(|| ctx.available_width_at_y(black_box(50.0)))
    });

    // Many floats scenario
    group.bench_function("many_floats", |b| {
        let mut ctx = FloatContext::new(800.0);
        for i in 0..20 {
            let side = if i % 2 == 0 { FloatSide::Left } else { FloatSide::Right };
            let x = if side == FloatSide::Left { 0.0 } else { 700.0 };
            ctx.add_float(FloatInfo {
                rect: Rect::from_xywh(x, (i * 30) as f32, 100.0, 25.0),
                side,
            });
        }
        b.iter(|| ctx.available_width_at_y(black_box(150.0)))
    });

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
        let tree = BoxTree::new(BoxNode::new_block(flex_style(), FormattingContextType::Flex, vec![]));
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
        let tree = BoxTree::new(BoxNode::new_block(grid_style(), FormattingContextType::Grid, vec![]));
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
            .map(|_| BoxNode::new_block(block_style(Some(100.0), None), FormattingContextType::Block, vec![]))
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
            .map(|_| BoxNode::new_block(block_style(Some(100.0), None), FormattingContextType::Block, vec![]))
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
        b.iter(|| LayoutEngine::with_defaults())
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

    group.bench_function("config_default", |b| {
        b.iter(|| LayoutConfig::default())
    });

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
    bench_width_computation,
    bench_margin_collapse,
    bench_float_context,
    bench_flex_layout,
    bench_grid_layout,
    bench_intrinsic_sizing,
    bench_layout_engine,
);

criterion_main!(benches);
