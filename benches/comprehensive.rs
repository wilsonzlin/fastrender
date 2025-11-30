//! Comprehensive Benchmark Suite for FastRender
//!
//! This module provides comprehensive benchmarks covering the entire rendering pipeline:
//! - End-to-end rendering
//! - Individual pipeline stages
//! - Various document complexities
//! - Edge cases and stress tests
//!
//! # Running Benchmarks
//!
//! ```bash
//! cargo bench --bench comprehensive
//! ```

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use fastrender::geometry::{Point, Rect, Size};
use fastrender::layout::{LayoutConfig, LayoutConstraints, LayoutEngine};
use fastrender::text::line_break::find_break_opportunities;
use fastrender::text::shaper::Script;
use fastrender::tree::{BoxNode, BoxTree, FormattingContextType};
use fastrender::FastRender;
use std::sync::Arc;

// ============================================================================
// Test Data Generation Helpers
// ============================================================================

/// Generates HTML for a simple document with N paragraphs
fn generate_simple_document(paragraph_count: usize) -> String {
    let mut html = String::from(
        r#"<html><head><style>
        body { font-family: sans-serif; margin: 20px; }
        p { margin: 10px 0; }
    </style></head><body>"#,
    );

    for i in 0..paragraph_count {
        html.push_str(&format!(
            "<p>This is paragraph {} with some sample text content that spans multiple words.</p>",
            i + 1
        ));
    }

    html.push_str("</body></html>");
    html
}

/// Generates HTML for nested divs (tree depth benchmark)
fn generate_nested_divs(depth: usize) -> String {
    let mut html = String::from(
        r#"<html><head><style>
        div { padding: 5px; border: 1px solid #ccc; }
    </style></head><body>"#,
    );

    for _ in 0..depth {
        html.push_str("<div>");
    }
    html.push_str("Content at the deepest level");
    for _ in 0..depth {
        html.push_str("</div>");
    }

    html.push_str("</body></html>");
    html
}

/// Generates HTML for a flex container with N items
fn generate_flex_container(item_count: usize) -> String {
    let mut html = String::from(
        r#"<html><head><style>
        .flex-container {
            display: flex;
            flex-wrap: wrap;
            gap: 10px;
        }
        .flex-item {
            width: 100px;
            height: 80px;
            background-color: #3498db;
            border-radius: 4px;
        }
    </style></head><body><div class="flex-container">"#,
    );

    for i in 0..item_count {
        html.push_str(&format!("<div class=\"flex-item\">Item {}</div>", i + 1));
    }

    html.push_str("</div></body></html>");
    html
}

/// Generates HTML for a grid layout with N items
fn generate_grid_layout(columns: usize, rows: usize) -> String {
    let mut html = format!(
        r#"<html><head><style>
        .grid {{
            display: grid;
            grid-template-columns: repeat({}, 1fr);
            grid-gap: 10px;
        }}
        .grid-item {{
            height: 60px;
            background-color: #e74c3c;
            border-radius: 4px;
        }}
    </style></head><body><div class="grid">"#,
        columns
    );

    let total_items = columns * rows;
    for i in 0..total_items {
        html.push_str(&format!("<div class=\"grid-item\">{}</div>", i + 1));
    }

    html.push_str("</div></body></html>");
    html
}

/// Generates HTML for a table with rows and columns
fn generate_table(rows: usize, columns: usize) -> String {
    let mut html = String::from(
        r#"<html><head><style>
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #4CAF50; color: white; }
        tr:nth-child(even) { background-color: #f2f2f2; }
    </style></head><body><table><thead><tr>"#,
    );

    // Header row
    for col in 0..columns {
        html.push_str(&format!("<th>Column {}</th>", col + 1));
    }
    html.push_str("</tr></thead><tbody>");

    // Data rows
    for row in 0..rows {
        html.push_str("<tr>");
        for col in 0..columns {
            html.push_str(&format!("<td>Row {} Col {}</td>", row + 1, col + 1));
        }
        html.push_str("</tr>");
    }

    html.push_str("</tbody></table></body></html>");
    html
}

/// Generates HTML with mixed content (blocks, inlines, text)
fn generate_mixed_content() -> String {
    String::from(
        r##"<html><head><style>
        body { font-family: sans-serif; line-height: 1.6; }
        h1 { color: #333; border-bottom: 2px solid #333; }
        h2 { color: #666; }
        p { margin: 1em 0; }
        .highlight { background-color: yellow; padding: 2px 4px; }
        .code { font-family: monospace; background-color: #f4f4f4; padding: 2px 6px; }
        blockquote { border-left: 4px solid #ccc; margin-left: 0; padding-left: 16px; color: #666; }
        ul { padding-left: 20px; }
    </style></head><body>
        <h1>Main Heading</h1>
        <p>This is an introductory paragraph with <span class="highlight">highlighted text</span> and some <span class="code">inline code</span>.</p>
        <h2>Secondary Heading</h2>
        <p>Another paragraph with <strong>bold text</strong>, <em>italic text</em>, and a <a href="#">link</a>.</p>
        <blockquote>This is a blockquote with some quoted text that might span multiple lines when rendered in a narrow viewport.</blockquote>
        <ul>
            <li>First list item with some text</li>
            <li>Second list item</li>
            <li>Third list item with more content</li>
        </ul>
        <p>Final paragraph to close out the content section.</p>
    </body></html>"##,
    )
}

/// Generates HTML with styled cards (common UI pattern)
fn generate_card_layout(card_count: usize) -> String {
    let mut html = String::from(
        r#"<html><head><style>
        body { font-family: sans-serif; background: #f0f0f0; padding: 20px; }
        .card-container { display: flex; flex-wrap: wrap; gap: 20px; }
        .card {
            width: 280px;
            background: white;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            overflow: hidden;
        }
        .card-header {
            background: linear-gradient(135deg, #667eea, #764ba2);
            color: white;
            padding: 16px;
        }
        .card-body { padding: 16px; }
        .card-title { margin: 0 0 8px 0; }
        .card-text { color: #666; margin: 0; }
    </style></head><body><div class="card-container">"#,
    );

    for i in 0..card_count {
        html.push_str(&format!(
            r#"<div class="card">
                <div class="card-header">
                    <h3 class="card-title">Card {}</h3>
                </div>
                <div class="card-body">
                    <p class="card-text">This is the body content for card number {}. It contains some descriptive text.</p>
                </div>
            </div>"#,
            i + 1,
            i + 1
        ));
    }

    html.push_str("</div></body></html>");
    html
}

// ============================================================================
// End-to-End Rendering Benchmarks
// ============================================================================

fn bench_end_to_end_rendering(c: &mut Criterion) {
    let mut group = c.benchmark_group("end_to_end_rendering");

    // Simple document benchmark
    let simple_html = generate_simple_document(5);
    group.throughput(Throughput::Bytes(simple_html.len() as u64));
    group.bench_function("simple_5_paragraphs", |b| {
        let mut renderer = FastRender::new().unwrap();
        b.iter(|| renderer.render_to_png(black_box(&simple_html), 800, 600).unwrap())
    });

    // Medium document
    let medium_html = generate_simple_document(20);
    group.bench_function("medium_20_paragraphs", |b| {
        let mut renderer = FastRender::new().unwrap();
        b.iter(|| renderer.render_to_png(black_box(&medium_html), 800, 1200).unwrap())
    });

    // Large document
    let large_html = generate_simple_document(50);
    group.bench_function("large_50_paragraphs", |b| {
        let mut renderer = FastRender::new().unwrap();
        b.iter(|| renderer.render_to_png(black_box(&large_html), 800, 3000).unwrap())
    });

    // Mixed content
    let mixed_html = generate_mixed_content();
    group.bench_function("mixed_content", |b| {
        let mut renderer = FastRender::new().unwrap();
        b.iter(|| renderer.render_to_png(black_box(&mixed_html), 800, 800).unwrap())
    });

    group.finish();
}

fn bench_layout_types(c: &mut Criterion) {
    let mut group = c.benchmark_group("layout_types");

    // Flex layout with varying item counts
    for item_count in [10, 25, 50, 100].iter() {
        let html = generate_flex_container(*item_count);
        group.bench_with_input(BenchmarkId::new("flex", item_count), &html, |b, html| {
            let mut renderer = FastRender::new().unwrap();
            b.iter(|| renderer.render_to_png(black_box(html), 1200, 800).unwrap())
        });
    }

    // Grid layout
    for (cols, rows) in [(4, 4), (8, 8), (10, 10)].iter() {
        let html = generate_grid_layout(*cols, *rows);
        let label = format!("{}x{}", cols, rows);
        group.bench_with_input(BenchmarkId::new("grid", &label), &html, |b, html| {
            let mut renderer = FastRender::new().unwrap();
            b.iter(|| renderer.render_to_png(black_box(html), 1200, 800).unwrap())
        });
    }

    // Table layout
    for (rows, cols) in [(10, 5), (25, 8), (50, 10)].iter() {
        let html = generate_table(*rows, *cols);
        let label = format!("{}x{}", rows, cols);
        group.bench_with_input(BenchmarkId::new("table", &label), &html, |b, html| {
            let mut renderer = FastRender::new().unwrap();
            b.iter(|| renderer.render_to_png(black_box(html), 1200, 1200).unwrap())
        });
    }

    group.finish();
}

fn bench_tree_depth(c: &mut Criterion) {
    let mut group = c.benchmark_group("tree_depth");

    for depth in [5, 10, 20, 50].iter() {
        let html = generate_nested_divs(*depth);
        group.bench_with_input(BenchmarkId::new("nested_divs", depth), &html, |b, html| {
            let mut renderer = FastRender::new().unwrap();
            b.iter(|| renderer.render_to_png(black_box(html), 800, 600).unwrap())
        });
    }

    group.finish();
}

fn bench_viewport_sizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("viewport_sizes");

    let html = generate_card_layout(12);

    // Different viewport sizes
    for (width, height) in [(320, 480), (768, 1024), (1920, 1080), (2560, 1440)].iter() {
        let label = format!("{}x{}", width, height);
        group.bench_with_input(BenchmarkId::new("cards", &label), &(&html, *width, *height), |b, (html, w, h)| {
            let mut renderer = FastRender::new().unwrap();
            b.iter(|| renderer.render_to_png(black_box(html), *w, *h).unwrap())
        });
    }

    group.finish();
}

// ============================================================================
// Component-Level Benchmarks
// ============================================================================

fn bench_geometry_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("geometry");

    // Point operations
    group.bench_function("point_translate", |b| {
        let p1 = Point::new(100.0, 200.0);
        let p2 = Point::new(50.0, 75.0);
        b.iter(|| black_box(p1).translate(black_box(p2)))
    });

    group.bench_function("point_distance", |b| {
        let p1 = Point::new(0.0, 0.0);
        let p2 = Point::new(100.0, 100.0);
        b.iter(|| black_box(p1).distance_to(black_box(p2)))
    });

    // Rect operations
    group.bench_function("rect_creation", |b| {
        b.iter(|| Rect::from_xywh(black_box(10.0), black_box(20.0), black_box(100.0), black_box(50.0)))
    });

    group.bench_function("rect_contains_point", |b| {
        let rect = Rect::from_xywh(10.0, 20.0, 100.0, 50.0);
        let point = Point::new(50.0, 40.0);
        b.iter(|| black_box(rect).contains_point(black_box(point)))
    });

    group.bench_function("rect_intersection", |b| {
        let r1 = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
        let r2 = Rect::from_xywh(50.0, 50.0, 100.0, 100.0);
        b.iter(|| black_box(r1).intersection(black_box(r2)))
    });

    group.bench_function("rect_union", |b| {
        let r1 = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
        let r2 = Rect::from_xywh(50.0, 50.0, 100.0, 100.0);
        b.iter(|| black_box(r1).union(black_box(r2)))
    });

    // Size operations
    group.bench_function("size_area", |b| {
        let size = Size::new(100.0, 50.0);
        b.iter(|| black_box(size).area())
    });

    group.bench_function("size_scale", |b| {
        let size = Size::new(100.0, 50.0);
        b.iter(|| black_box(size).scale(black_box(2.0)))
    });

    group.finish();
}

fn bench_text_processing(c: &mut Criterion) {
    let mut group = c.benchmark_group("text_processing");

    // Line break detection
    let short_text = "Hello world, this is a test.";
    let medium_text = "The quick brown fox jumps over the lazy dog. Pack my box with five dozen liquor jugs. How vexingly quick daft zebras jump!";
    let long_text = medium_text.repeat(10);

    group.bench_function("line_breaks_short", |b| {
        b.iter(|| find_break_opportunities(black_box(short_text)))
    });

    group.bench_function("line_breaks_medium", |b| {
        b.iter(|| find_break_opportunities(black_box(medium_text)))
    });

    group.bench_function("line_breaks_long", |b| {
        b.iter(|| find_break_opportunities(black_box(&long_text)))
    });

    // Script detection
    group.bench_function("script_detect_latin", |b| {
        b.iter(|| Script::detect(black_box('A')))
    });

    group.bench_function("script_detect_cjk", |b| {
        b.iter(|| Script::detect(black_box('\u{4E00}'))) // CJK character
    });

    group.bench_function("script_detect_arabic", |b| {
        b.iter(|| Script::detect(black_box('\u{0627}'))) // Arabic character
    });

    group.bench_function("script_detect_text_latin", |b| {
        let text = "Hello, world!";
        b.iter(|| Script::detect_text(black_box(text)))
    });

    group.bench_function("script_detect_text_mixed", |b| {
        let text = "Hello world with some data: 12345 and symbols @#$%";
        b.iter(|| Script::detect_text(black_box(text)))
    });

    group.finish();
}

fn bench_layout_engine(c: &mut Criterion) {
    let mut group = c.benchmark_group("layout_engine");

    // Layout config creation
    group.bench_function("config_creation", |b| {
        b.iter(|| LayoutConfig::for_viewport(Size::new(black_box(1920.0), black_box(1080.0))))
    });

    // Layout constraints
    group.bench_function("constraints_definite", |b| {
        b.iter(|| LayoutConstraints::definite(black_box(800.0), black_box(600.0)))
    });

    group.bench_function("constraints_definite_width", |b| {
        b.iter(|| LayoutConstraints::definite_width(black_box(800.0)))
    });

    // Create default style for box nodes
    fn default_style() -> Arc<fastrender::style::ComputedStyles> {
        Arc::new(fastrender::style::ComputedStyles::default())
    }

    // Simple box tree layout
    group.bench_function("layout_empty_block", |b| {
        let engine = LayoutEngine::with_defaults();
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let box_tree = BoxTree::new(root);
        b.iter(|| engine.layout_tree(black_box(&box_tree)).unwrap())
    });

    // Shallow tree layout
    group.bench_function("layout_shallow_tree", |b| {
        let engine = LayoutEngine::with_defaults();
        let children: Vec<BoxNode> = (0..10)
            .map(|_| BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]))
            .collect();
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, children);
        let box_tree = BoxTree::new(root);
        b.iter(|| engine.layout_tree(black_box(&box_tree)).unwrap())
    });

    // Deep tree layout
    group.bench_function("layout_deep_tree", |b| {
        let engine = LayoutEngine::with_defaults();
        let mut current = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        for _ in 0..20 {
            current = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![current]);
        }
        let box_tree = BoxTree::new(current);
        b.iter(|| engine.layout_tree(black_box(&box_tree)).unwrap())
    });

    // Wide tree layout
    group.bench_function("layout_wide_tree", |b| {
        let engine = LayoutEngine::with_defaults();
        let children: Vec<BoxNode> = (0..50)
            .map(|_| BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]))
            .collect();
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, children);
        let box_tree = BoxTree::new(root);
        b.iter(|| engine.layout_tree(black_box(&box_tree)).unwrap())
    });

    group.finish();
}

// ============================================================================
// Memory/Allocation Benchmarks
// ============================================================================

fn bench_allocations(c: &mut Criterion) {
    let mut group = c.benchmark_group("allocations");

    // Box tree construction
    fn default_style() -> Arc<fastrender::style::ComputedStyles> {
        Arc::new(fastrender::style::ComputedStyles::default())
    }

    group.bench_function("box_node_creation", |b| {
        b.iter(|| BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]))
    });

    group.bench_function("box_tree_with_children", |b| {
        b.iter(|| {
            let children: Vec<BoxNode> = (0..10)
                .map(|_| BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]))
                .collect();
            BoxNode::new_block(default_style(), FormattingContextType::Block, children)
        })
    });

    // String allocations in HTML generation
    group.bench_function("html_generation_small", |b| {
        b.iter(|| generate_simple_document(black_box(5)))
    });

    group.bench_function("html_generation_large", |b| {
        b.iter(|| generate_simple_document(black_box(100)))
    });

    group.finish();
}

// ============================================================================
// Stress Tests
// ============================================================================

fn bench_stress_tests(c: &mut Criterion) {
    let mut group = c.benchmark_group("stress_tests");
    group.sample_size(10); // Fewer samples for expensive tests

    // Very large document
    let huge_html = generate_simple_document(200);
    group.bench_function("huge_document", |b| {
        let mut renderer = FastRender::new().unwrap();
        b.iter(|| renderer.render_to_png(black_box(&huge_html), 800, 10000).unwrap())
    });

    // Many flex items
    let many_flex = generate_flex_container(200);
    group.bench_function("many_flex_items", |b| {
        let mut renderer = FastRender::new().unwrap();
        b.iter(|| renderer.render_to_png(black_box(&many_flex), 1920, 2000).unwrap())
    });

    // Large grid
    let large_grid = generate_grid_layout(20, 20);
    group.bench_function("large_grid", |b| {
        let mut renderer = FastRender::new().unwrap();
        b.iter(|| renderer.render_to_png(black_box(&large_grid), 2000, 2000).unwrap())
    });

    // Large table
    let large_table = generate_table(100, 15);
    group.bench_function("large_table", |b| {
        let mut renderer = FastRender::new().unwrap();
        b.iter(|| renderer.render_to_png(black_box(&large_table), 1600, 4000).unwrap())
    });

    group.finish();
}

// ============================================================================
// Criterion Groups and Main
// ============================================================================

criterion_group!(
    benches,
    bench_end_to_end_rendering,
    bench_layout_types,
    bench_tree_depth,
    bench_viewport_sizes,
    bench_geometry_operations,
    bench_text_processing,
    bench_layout_engine,
    bench_allocations,
    bench_stress_tests,
);

criterion_main!(benches);
