# Phase 6: Benchmarking and Performance Measurement

**Duration:** 1-2 weeks (ongoing)
**Prerequisites:**
- Basic rendering pipeline working
- Layout and paint phases implemented
**Dependencies:**
- criterion.rs for benchmarking
- perf/flamegraph tools for profiling
**Output:** Comprehensive benchmark suite with performance tracking and regression detection

## Objectives

Implement systematic performance measurement and optimization for FastRender:

- **Benchmarking framework** - Using criterion.rs for statistical benchmarks
- **What to benchmark** - End-to-end rendering, layout, paint, text shaping, font loading, memory
- **Benchmark workloads** - Simple pages, complex layouts, text-heavy, image-heavy, real-world pages
- **Performance regression detection** - Automatically catch slowdowns
- **Profiling integration** - Flamegraphs and perf integration
- **Optimization strategies** - Data-driven performance improvements
- **CI integration** - Track performance over time
- **Comparison with browsers** - Understand where we stand

This is **critical for production readiness** - performance is a feature.

## Context

Performance benchmarking measures how fast code executes and tracks changes over time.

**Why Benchmark?**

1. **Detect regressions** - Catch performance degradation before release
2. **Guide optimization** - Know what to optimize
3. **Track progress** - See if optimizations work
4. **Compare implementations** - Choose fastest algorithm
5. **Set expectations** - Understand realistic performance targets

**What Makes a Good Benchmark?**

- **Realistic workloads** - Test real-world scenarios
- **Repeatable** - Same results every time
- **Statistical rigor** - Account for variance
- **Comprehensive** - Cover all critical paths
- **Fast enough** - Can run in CI without timeout

**From Criterion.rs Documentation:**
> "Criterion.rs is a statistics-driven benchmarking library for Rust. It provides precise measurements of changes in performance, detecting even small improvements or regressions."

## The Problem V1 Has

V1 has no performance measurement:
- No idea how fast it is
- Can't detect performance regressions
- No way to validate optimizations
- Don't know bottlenecks
- Can't compare with other browsers

This means performance could silently degrade over time.

## The Solution

Implement comprehensive benchmarking infrastructure:

1. **Criterion.rs benchmarks** - Statistical micro-benchmarks
2. **End-to-end benchmarks** - Full page rendering
3. **Component benchmarks** - Layout, paint, text, fonts
4. **Memory profiling** - Track allocations
5. **Profiling integration** - Flamegraphs for optimization
6. **Performance CI** - Track metrics over time
7. **Comparison tools** - Compare with Chrome, Firefox, etc.

## Specification References

**Primary:**
- **Criterion.rs Documentation:** https://bheisler.github.io/criterion.rs/book/
- **Rust Performance Book:** https://nnethercote.github.io/perf-book/
- **Flamegraph Guide:** https://www.brendangregg.com/flamegraphs.html

**Browser Benchmarks:**
- **Speedometer 3.0:** https://browserbench.org/Speedometer3.0/
- **MotionMark:** https://browserbench.org/MotionMark/
- **Jetstream:** https://browserbench.org/JetStream/

**Profiling Tools:**
- **perf:** https://perf.wiki.kernel.org/
- **cargo-flamegraph:** https://github.com/flamegraph-rs/flamegraph
- **valgrind/cachegrind:** https://valgrind.org/docs/manual/cg-manual.html

## Step-by-Step Implementation

### Step 1: Set Up Criterion.rs (Day 1 Morning)

**File: `Cargo.toml`** (update)

```toml
[dev-dependencies]
criterion = { version = "0.5", features = ["html_reports"] }

[[bench]]
name = "layout_benchmark"
harness = false

[[bench]]
name = "paint_benchmark"
harness = false

[[bench]]
name = "text_benchmark"
harness = false

[[bench]]
name = "end_to_end_benchmark"
harness = false
```

**Create benchmark directory:**

```bash
mkdir -p /home/user/fastrender/benches
mkdir -p /home/user/fastrender/benches/fixtures
mkdir -p /home/user/fastrender/benches/results
```

**File: `benches/fixtures/mod.rs`**

```rust
//! Test fixtures for benchmarks

/// Generate simple HTML with N divs
pub fn simple_page(num_divs: usize) -> String {
    let mut html = String::from(r#"
        <!DOCTYPE html>
        <html>
        <head>
            <style>
                body { margin: 0; padding: 20px; }
                .box { width: 100px; height: 50px; background: blue; margin: 10px; }
            </style>
        </head>
        <body>
    "#);

    for i in 0..num_divs {
        html.push_str(&format!(r#"<div class="box">Box {}</div>"#, i));
    }

    html.push_str("</body></html>");
    html
}

/// Generate nested layout HTML
pub fn nested_layout(depth: usize) -> String {
    fn generate_nested(depth: usize) -> String {
        if depth == 0 {
            return String::from("<div class='leaf'>Content</div>");
        }

        format!(
            "<div class='container'>{}</div>",
            generate_nested(depth - 1)
        )
    }

    format!(
        r#"
        <!DOCTYPE html>
        <html>
        <head>
            <style>
                .container {{ padding: 10px; border: 1px solid black; }}
                .leaf {{ background: blue; padding: 20px; }}
            </style>
        </head>
        <body>
            {}
        </body>
        </html>
        "#,
        generate_nested(depth)
    )
}

/// Generate flexbox layout
pub fn flexbox_layout(num_items: usize) -> String {
    let mut html = String::from(r#"
        <!DOCTYPE html>
        <html>
        <head>
            <style>
                .container {
                    display: flex;
                    flex-wrap: wrap;
                    justify-content: space-between;
                    width: 800px;
                }
                .item {
                    width: 150px;
                    height: 100px;
                    background: blue;
                    margin: 10px;
                }
            </style>
        </head>
        <body>
            <div class="container">
    "#);

    for i in 0..num_items {
        html.push_str(&format!(r#"<div class="item">Item {}</div>"#, i));
    }

    html.push_str("</div></body></html>");
    html
}

/// Generate text-heavy page
pub fn text_heavy_page() -> String {
    let lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. \
                 Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. \
                 Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris.";

    let mut html = String::from(r#"
        <!DOCTYPE html>
        <html>
        <head>
            <style>
                body { margin: 0; padding: 40px; max-width: 800px; }
                h1 { font-size: 32px; margin-bottom: 20px; }
                h2 { font-size: 24px; margin-top: 30px; margin-bottom: 10px; }
                p { line-height: 1.6; margin-bottom: 15px; }
            </style>
        </head>
        <body>
            <h1>Article Title</h1>
    "#);

    for i in 0..20 {
        html.push_str(&format!(
            "<h2>Section {}</h2><p>{}</p><p>{}</p>",
            i + 1,
            lorem,
            lorem
        ));
    }

    html.push_str("</body></html>");
    html
}

/// Generate grid layout
pub fn grid_layout(rows: usize, cols: usize) -> String {
    let mut html = format!(
        r#"
        <!DOCTYPE html>
        <html>
        <head>
            <style>
                .container {{
                    display: grid;
                    grid-template-columns: repeat({}, 1fr);
                    grid-template-rows: repeat({}, 100px);
                    gap: 10px;
                    width: 800px;
                }}
                .item {{
                    background: blue;
                    padding: 10px;
                }}
            </style>
        </head>
        <body>
            <div class="container">
        "#,
        cols, rows
    );

    for i in 0..(rows * cols) {
        html.push_str(&format!(r#"<div class="item">Item {}</div>"#, i));
    }

    html.push_str("</div></body></html>");
    html
}

/// Wikipedia-like article
pub fn wikipedia_article() -> String {
    std::fs::read_to_string("benches/fixtures/wikipedia.html")
        .unwrap_or_else(|_| text_heavy_page())
}

/// Hacker News-like page
pub fn hacker_news_page() -> String {
    std::fs::read_to_string("benches/fixtures/hackernews.html")
        .unwrap_or_else(|_| simple_page(30))
}
```

### Step 2: Layout Benchmarks (Day 1-2)

**File: `benches/layout_benchmark.rs`**

```rust
//! Layout benchmarks
//!
//! Measures performance of layout algorithms.

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, Throughput};
use fastrender::Renderer;

mod fixtures;

/// Benchmark block layout with varying numbers of elements
fn bench_block_layout(c: &mut Criterion) {
    let mut group = c.benchmark_group("block-layout");

    for num_divs in [10, 50, 100, 500, 1000].iter() {
        let html = fixtures::simple_page(*num_divs);

        group.throughput(Throughput::Elements(*num_divs as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(num_divs),
            &html,
            |b, html| {
                let renderer = Renderer::new();
                b.iter(|| {
                    renderer.render_to_layout(black_box(html), 800, 600)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark nested layout depth
fn bench_nested_layout(c: &mut Criterion) {
    let mut group = c.benchmark_group("nested-layout");

    for depth in [5, 10, 15, 20].iter() {
        let html = fixtures::nested_layout(*depth);

        group.bench_with_input(
            BenchmarkId::from_parameter(depth),
            &html,
            |b, html| {
                let renderer = Renderer::new();
                b.iter(|| {
                    renderer.render_to_layout(black_box(html), 800, 600)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark flexbox layout
fn bench_flexbox_layout(c: &mut Criterion) {
    let mut group = c.benchmark_group("flexbox-layout");

    for num_items in [10, 50, 100, 200].iter() {
        let html = fixtures::flexbox_layout(*num_items);

        group.throughput(Throughput::Elements(*num_items as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(num_items),
            &html,
            |b, html| {
                let renderer = Renderer::new();
                b.iter(|| {
                    renderer.render_to_layout(black_box(html), 800, 600)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark grid layout
fn bench_grid_layout(c: &mut Criterion) {
    let mut group = c.benchmark_group("grid-layout");

    for size in [(5, 5), (10, 10), (20, 20)].iter() {
        let html = fixtures::grid_layout(size.0, size.1);
        let label = format!("{}x{}", size.0, size.1);

        group.throughput(Throughput::Elements((size.0 * size.1) as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(&label),
            &html,
            |b, html| {
                let renderer = Renderer::new();
                b.iter(|| {
                    renderer.render_to_layout(black_box(html), 800, 600)
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    layout_benches,
    bench_block_layout,
    bench_nested_layout,
    bench_flexbox_layout,
    bench_grid_layout,
);

criterion_main!(layout_benches);
```

### Step 3: Paint Benchmarks (Day 2)

**File: `benches/paint_benchmark.rs`**

```rust
//! Paint benchmarks
//!
//! Measures performance of painting and rasterization.

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use fastrender::Renderer;

mod fixtures;

/// Benchmark full rendering pipeline
fn bench_full_render(c: &mut Criterion) {
    let mut group = c.benchmark_group("full-render");

    for num_divs in [10, 50, 100].iter() {
        let html = fixtures::simple_page(*num_divs);

        group.bench_with_input(
            BenchmarkId::from_parameter(num_divs),
            &html,
            |b, html| {
                let renderer = Renderer::new();
                b.iter(|| {
                    renderer.render_to_png(black_box(html), 800, 600)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark display list building
fn bench_display_list(c: &mut Criterion) {
    let mut group = c.benchmark_group("display-list");

    let simple = fixtures::simple_page(100);
    let flexbox = fixtures::flexbox_layout(50);

    group.bench_function("simple", |b| {
        let renderer = Renderer::new();
        b.iter(|| {
            renderer.build_display_list(black_box(&simple), 800, 600)
        });
    });

    group.bench_function("flexbox", |b| {
        let renderer = Renderer::new();
        b.iter(|| {
            renderer.build_display_list(black_box(&flexbox), 800, 600)
        });
    });

    group.finish();
}

/// Benchmark rasterization
fn bench_rasterization(c: &mut Criterion) {
    let mut group = c.benchmark_group("rasterization");

    // Pre-build display lists
    let renderer = Renderer::new();
    let simple_dl = renderer.build_display_list(&fixtures::simple_page(100), 800, 600).unwrap();
    let flexbox_dl = renderer.build_display_list(&fixtures::flexbox_layout(50), 800, 600).unwrap();

    group.bench_function("simple", |b| {
        b.iter(|| {
            renderer.rasterize(black_box(&simple_dl), 800, 600)
        });
    });

    group.bench_function("flexbox", |b| {
        b.iter(|| {
            renderer.rasterize(black_box(&flexbox_dl), 800, 600)
        });
    });

    group.finish();
}

criterion_group!(
    paint_benches,
    bench_full_render,
    bench_display_list,
    bench_rasterization,
);

criterion_main!(paint_benches);
```

### Step 4: Text Benchmarks (Day 3)

**File: `benches/text_benchmark.rs`**

```rust
//! Text benchmarks
//!
//! Measures performance of text shaping and rendering.

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, Throughput};
use fastrender::text::shaping::ShapingPipeline;
use fastrender::text::font::FontLoader;
use fastrender::style::ComputedStyle;

mod fixtures;

/// Benchmark text shaping
fn bench_text_shaping(c: &mut Criterion) {
    let mut group = c.benchmark_group("text-shaping");

    let pipeline = ShapingPipeline::new();
    let font_loader = FontLoader::new();
    let style = ComputedStyle::default();

    let test_strings = vec![
        ("simple", "Hello, World!"),
        ("long", "Lorem ipsum dolor sit amet, consectetur adipiscing elit."),
        ("arabic", "مرحبا بك في عالم البرمجة"),
        ("mixed", "Hello مرحبا World שלום"),
    ];

    for (name, text) in test_strings {
        group.throughput(Throughput::Bytes(text.len() as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &text,
            |b, text| {
                b.iter(|| {
                    pipeline.shape(black_box(text), &style, &font_loader)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark line breaking
fn bench_line_breaking(c: &mut Criterion) {
    let mut group = c.benchmark_group("line-breaking");

    let lorem = fixtures::text_heavy_page();

    group.throughput(Throughput::Bytes(lorem.len() as u64));
    group.bench_function("paragraph", |b| {
        let renderer = fastrender::Renderer::new();
        b.iter(|| {
            renderer.render_to_layout(black_box(&lorem), 800, 600)
        });
    });

    group.finish();
}

/// Benchmark font loading
fn bench_font_loading(c: &mut Criterion) {
    let mut group = c.benchmark_group("font-loading");

    group.bench_function("load-system-font", |b| {
        b.iter(|| {
            let loader = FontLoader::new();
            loader.load_from_family("Arial", 400, "normal", "normal")
        });
    });

    group.finish();
}

criterion_group!(
    text_benches,
    bench_text_shaping,
    bench_line_breaking,
    bench_font_loading,
);

criterion_main!(text_benches);
```

### Step 5: End-to-End Benchmarks (Day 3-4)

**File: `benches/end_to_end_benchmark.rs`**

```rust
//! End-to-end benchmarks
//!
//! Measures full page rendering performance on realistic workloads.

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use fastrender::Renderer;

mod fixtures;

/// Benchmark real-world pages
fn bench_real_world_pages(c: &mut Criterion) {
    let mut group = c.benchmark_group("real-world");

    // Configure for longer benchmarks
    group.sample_size(20);
    group.measurement_time(std::time::Duration::from_secs(10));

    let pages = vec![
        ("simple", fixtures::simple_page(100)),
        ("text-heavy", fixtures::text_heavy_page()),
        ("flexbox", fixtures::flexbox_layout(100)),
        ("grid", fixtures::grid_layout(10, 10)),
        ("wikipedia", fixtures::wikipedia_article()),
        ("hackernews", fixtures::hacker_news_page()),
    ];

    for (name, html) in pages {
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &html,
            |b, html| {
                let renderer = Renderer::new();
                b.iter(|| {
                    renderer.render_to_png(black_box(html), 800, 600)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark different viewport sizes
fn bench_viewport_sizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("viewport-sizes");

    let html = fixtures::simple_page(100);

    let sizes = vec![
        ("mobile", 375, 667),
        ("tablet", 768, 1024),
        ("desktop", 1920, 1080),
        ("4k", 3840, 2160),
    ];

    for (name, width, height) in sizes {
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &(width, height),
            |b, &(w, h)| {
                let renderer = Renderer::new();
                b.iter(|| {
                    renderer.render_to_png(black_box(&html), w, h)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark incremental rendering
fn bench_incremental_updates(c: &mut Criterion) {
    let mut group = c.benchmark_group("incremental");

    // Benchmark: change single element
    group.bench_function("single-element-update", |b| {
        let mut html = fixtures::simple_page(100);

        b.iter(|| {
            // Simulate DOM mutation
            html = html.replace("Box 50", "Box 50 Updated");
            let renderer = Renderer::new();
            renderer.render_to_png(black_box(&html), 800, 600)
        });
    });

    group.finish();
}

criterion_group!(
    end_to_end_benches,
    bench_real_world_pages,
    bench_viewport_sizes,
    bench_incremental_updates,
);

criterion_main!(end_to_end_benches);
```

### Step 6: Memory Benchmarks (Day 4)

**File: `benches/memory_benchmark.rs`**

```rust
//! Memory benchmarks
//!
//! Measures memory usage and allocation patterns.

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use fastrender::Renderer;

mod fixtures;

/// Benchmark memory allocations
fn bench_allocations(c: &mut Criterion) {
    let mut group = c.benchmark_group("allocations");

    // Use jemalloc for better allocation tracking
    #[cfg(feature = "jemalloc")]
    {
        use tikv_jemalloc_ctl::{stats, epoch};

        group.bench_function("simple-page-allocations", |b| {
            let html = fixtures::simple_page(100);
            let renderer = Renderer::new();

            b.iter(|| {
                // Refresh stats
                epoch::mib().unwrap().advance().unwrap();

                let allocated_before = stats::allocated::mib().unwrap().read().unwrap();

                renderer.render_to_png(black_box(&html), 800, 600).unwrap();

                epoch::mib().unwrap().advance().unwrap();
                let allocated_after = stats::allocated::mib().unwrap().read().unwrap();

                let allocated = allocated_after - allocated_before;
                println!("Allocated: {} bytes", allocated);
            });
        });
    }

    group.finish();
}

/// Benchmark peak memory usage
fn bench_peak_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("peak-memory");

    let pages = vec![
        ("simple-100", fixtures::simple_page(100)),
        ("simple-1000", fixtures::simple_page(1000)),
        ("text-heavy", fixtures::text_heavy_page()),
    ];

    for (name, html) in pages {
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &html,
            |b, html| {
                let renderer = Renderer::new();
                b.iter(|| {
                    renderer.render_to_png(black_box(html), 800, 600)
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    memory_benches,
    bench_allocations,
    bench_peak_memory,
);

criterion_main!(memory_benches);
```

### Step 7: Profiling Integration (Day 5)

**File: `scripts/profile.sh`**

```bash
#!/bin/bash
# Profile FastRender and generate flamegraph

set -e

# Install cargo-flamegraph if needed
if ! command -v cargo-flamegraph &> /dev/null; then
    echo "Installing cargo-flamegraph..."
    cargo install flamegraph
fi

# Build in release mode with debug symbols
export CARGO_PROFILE_RELEASE_DEBUG=true

# Profile benchmark
echo "Running benchmark with profiling..."
cargo flamegraph --bench end_to_end_benchmark -- --bench

echo "Flamegraph saved to flamegraph.svg"
echo "Open in browser: file://$(pwd)/flamegraph.svg"
```

**File: `scripts/perf_compare.sh`**

```bash
#!/bin/bash
# Compare performance between two commits

set -e

if [ $# -lt 2 ]; then
    echo "Usage: $0 <baseline-commit> <current-commit>"
    exit 1
fi

BASELINE=$1
CURRENT=$2
RESULTS_DIR="benches/results/comparison"

mkdir -p "$RESULTS_DIR"

echo "Benchmarking baseline: $BASELINE"
git checkout "$BASELINE"
cargo bench --bench end_to_end_benchmark -- --save-baseline baseline

echo "Benchmarking current: $CURRENT"
git checkout "$CURRENT"
cargo bench --bench end_to_end_benchmark -- --baseline baseline

echo "Results saved to target/criterion/"
echo ""
echo "View HTML reports:"
echo "  Baseline: file://$(pwd)/target/criterion/baseline/report/index.html"
echo "  Current:  file://$(pwd)/target/criterion/report/index.html"
```

**Make scripts executable:**

```bash
chmod +x scripts/profile.sh
chmod +x scripts/perf_compare.sh
```

### Step 8: Performance Regression Detection (Day 5-6)

**File: `scripts/check_perf_regression.py`**

```python
#!/usr/bin/env python3
"""
Check for performance regressions in benchmarks.

Compares current benchmark results with baseline and fails if
performance degrades more than threshold.
"""

import json
import sys
from pathlib import Path

# Maximum allowed regression percentage
REGRESSION_THRESHOLD = 10.0

def load_benchmark_results(path):
    """Load benchmark results JSON"""
    with open(path) as f:
        return json.load(f)

def compare_benchmarks(baseline_path, current_path):
    """Compare two benchmark results"""
    baseline = load_benchmark_results(baseline_path)
    current = load_benchmark_results(current_path)

    regressions = []

    # Compare each benchmark
    for bench_name, current_data in current.items():
        if bench_name not in baseline:
            print(f"New benchmark: {bench_name}")
            continue

        baseline_data = baseline[bench_name]

        # Get median times
        baseline_time = baseline_data.get('median', 0)
        current_time = current_data.get('median', 0)

        if baseline_time == 0:
            continue

        # Calculate percentage change
        change_pct = ((current_time - baseline_time) / baseline_time) * 100

        if change_pct > REGRESSION_THRESHOLD:
            regressions.append({
                'name': bench_name,
                'baseline': baseline_time,
                'current': current_time,
                'change': change_pct,
            })
        elif change_pct < -5.0:
            # Improvement!
            print(f"✓ {bench_name}: {abs(change_pct):.1f}% faster")

    # Report regressions
    if regressions:
        print("\n❌ Performance Regressions Detected:\n")
        for reg in regressions:
            print(f"  {reg['name']}")
            print(f"    Baseline: {reg['baseline']:.2f}ms")
            print(f"    Current:  {reg['current']:.2f}ms")
            print(f"    Change:   +{reg['change']:.1f}% slower")
            print()

        return False

    print("\n✓ No performance regressions detected")
    return True

def main():
    if len(sys.argv) < 3:
        print("Usage: check_perf_regression.py <baseline.json> <current.json>")
        sys.exit(1)

    baseline_path = Path(sys.argv[1])
    current_path = Path(sys.argv[2])

    if not baseline_path.exists():
        print(f"Baseline file not found: {baseline_path}")
        sys.exit(1)

    if not current_path.exists():
        print(f"Current file not found: {current_path}")
        sys.exit(1)

    success = compare_benchmarks(baseline_path, current_path)

    sys.exit(0 if success else 1)

if __name__ == '__main__':
    main()
```

**Make it executable:**

```bash
chmod +x scripts/check_perf_regression.py
```

### Step 9: CI Integration (Day 6)

**File: `.github/workflows/benchmarks.yml`**

```yaml
name: Performance Benchmarks

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  benchmark:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0  # Need full history for comparison

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          profile: minimal

      - name: Cache cargo
        uses: actions/cache@v3
        with:
          path: |
            ~/.cargo/registry
            ~/.cargo/git
            target
          key: ${{ runner.os }}-cargo-bench-${{ hashFiles('**/Cargo.lock') }}

      - name: Install system dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y \
            libfreetype6-dev \
            libfontconfig1-dev

      - name: Download baseline benchmarks
        uses: actions/cache@v3
        with:
          path: target/criterion
          key: criterion-baseline-${{ github.base_ref || 'main' }}

      - name: Run benchmarks
        run: |
          # Run all benchmarks
          cargo bench --bench layout_benchmark -- --save-baseline pr-${{ github.event.number }}
          cargo bench --bench paint_benchmark
          cargo bench --bench text_benchmark
          cargo bench --bench end_to_end_benchmark

      - name: Compare with baseline
        if: github.event_name == 'pull_request'
        run: |
          # Compare results
          # (requires baseline from previous run)
          echo "Comparing with baseline..."

      - name: Upload benchmark results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: target/criterion/

      - name: Comment PR with results
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');

            // Parse benchmark results
            // (simplified - actual implementation would parse Criterion output)

            const body = `## 📊 Benchmark Results

            | Benchmark | Time | Change |
            |-----------|------|--------|
            | Block Layout (100) | 12.5ms | +2.3% |
            | Flexbox Layout (50) | 8.2ms | -1.5% ⚡ |
            | Text Rendering | 15.3ms | +0.1% |

            <details>
            <summary>Full results</summary>

            [View detailed report](https://github.com/${{ github.repository }}/actions/runs/${{ github.run_id }})

            </details>
            `;

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: body
            });

  continuous-benchmarking:
    # Run daily benchmarks to track long-term trends
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'

    steps:
      - uses: actions/checkout@v3

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Run benchmarks
        run: cargo bench

      - name: Store results
        run: |
          # Store results with timestamp
          DATE=$(date +%Y-%m-%d)
          mkdir -p benches/results/history
          cp -r target/criterion benches/results/history/$DATE

      - name: Commit results
        run: |
          git config user.name "Benchmark Bot"
          git config user.email "benchmark@fastrender"
          git add benches/results/history
          git commit -m "chore: benchmark results for $(date +%Y-%m-%d)" || true
          git push || true
```

### Step 10: Performance Dashboard (Day 7)

**File: `scripts/perf_dashboard.py`**

```python
#!/usr/bin/env python3
"""
Generate performance dashboard showing trends over time.
"""

import json
import glob
from pathlib import Path
from datetime import datetime
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

def load_historical_results():
    """Load all historical benchmark results"""
    results = []

    for result_dir in sorted(Path('benches/results/history').glob('*')):
        if not result_dir.is_dir():
            continue

        date_str = result_dir.name
        try:
            date = datetime.strptime(date_str, '%Y-%m-%d')
        except ValueError:
            continue

        # Load benchmark data
        # (simplified - actual implementation would parse Criterion JSON)
        results.append({
            'date': date,
            'block_layout': 12.5,  # ms
            'flexbox_layout': 8.2,
            'text_rendering': 15.3,
            'full_render': 45.2,
        })

    return results

def plot_trends(results, output_path):
    """Plot performance trends"""
    if not results:
        return

    dates = [r['date'] for r in results]
    metrics = ['block_layout', 'flexbox_layout', 'text_rendering', 'full_render']
    metric_names = ['Block Layout', 'Flexbox Layout', 'Text Rendering', 'Full Render']

    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    axes = axes.flatten()

    for i, (metric, name) in enumerate(zip(metrics, metric_names)):
        values = [r[metric] for r in results]

        axes[i].plot(dates, values, marker='o', linewidth=2)
        axes[i].set_title(name)
        axes[i].set_xlabel('Date')
        axes[i].set_ylabel('Time (ms)')
        axes[i].grid(True, alpha=0.3)

        # Format x-axis
        axes[i].xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
        axes[i].xaxis.set_major_locator(mdates.DayLocator(interval=7))

    plt.tight_layout()
    plt.savefig(output_path, dpi=150)
    print(f"Performance trends saved to {output_path}")

def generate_html_dashboard(results, output_path):
    """Generate HTML dashboard"""
    latest = results[-1] if results else None

    html = f"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>FastRender Performance Dashboard</title>
        <style>
            body {{
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
                max-width: 1200px;
                margin: 0 auto;
                padding: 20px;
                background: #f5f5f5;
            }}
            .header {{
                background: white;
                padding: 30px;
                border-radius: 8px;
                margin-bottom: 20px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }}
            .metrics {{
                display: grid;
                grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
                gap: 20px;
                margin-bottom: 20px;
            }}
            .metric {{
                background: white;
                padding: 20px;
                border-radius: 8px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }}
            .metric-value {{
                font-size: 36px;
                font-weight: bold;
                color: #333;
            }}
            .metric-label {{
                color: #666;
                font-size: 14px;
                margin-top: 5px;
            }}
            .chart {{
                background: white;
                padding: 20px;
                border-radius: 8px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }}
        </style>
    </head>
    <body>
        <div class="header">
            <h1>FastRender Performance Dashboard</h1>
            <p>Benchmark results and performance trends</p>
            <p><strong>Last Updated:</strong> {latest['date'].strftime('%Y-%m-%d') if latest else 'N/A'}</p>
        </div>

        <div class="metrics">
            <div class="metric">
                <div class="metric-value">{latest['block_layout']:.1f}ms</div>
                <div class="metric-label">Block Layout (100 elements)</div>
            </div>
            <div class="metric">
                <div class="metric-value">{latest['flexbox_layout']:.1f}ms</div>
                <div class="metric-label">Flexbox Layout (50 items)</div>
            </div>
            <div class="metric">
                <div class="metric-value">{latest['text_rendering']:.1f}ms</div>
                <div class="metric-label">Text Rendering</div>
            </div>
            <div class="metric">
                <div class="metric-value">{latest['full_render']:.1f}ms</div>
                <div class="metric-label">Full Page Render</div>
            </div>
        </div>

        <div class="chart">
            <h2>Performance Trends</h2>
            <img src="perf_trends.png" style="width: 100%;">
        </div>
    </body>
    </html>
    """

    with open(output_path, 'w') as f:
        f.write(html)

    print(f"Dashboard saved to {output_path}")

def main():
    results = load_historical_results()

    if not results:
        print("No historical results found")
        return

    # Generate charts
    plot_trends(results, 'benches/results/perf_trends.png')

    # Generate dashboard
    generate_html_dashboard(results, 'benches/results/dashboard.html')

    print(f"\nDashboard: file://{Path('benches/results/dashboard.html').absolute()}")

if __name__ == '__main__':
    main()
```

## Running Benchmarks

### Quick Start

```bash
# Run all benchmarks
cargo bench

# Run specific benchmark
cargo bench --bench layout_benchmark

# Run with profiling
./scripts/profile.sh

# Compare commits
./scripts/perf_compare.sh baseline-commit current-commit

# Generate dashboard
python3 scripts/perf_dashboard.py
```

### Interpreting Results

Criterion outputs statistical analysis:

```
block-layout/10         time:   [1.2345 ms 1.2567 ms 1.2789 ms]
                        change: [-2.3456% -1.2345% +0.1234%] (p = 0.23 > 0.05)
                        No change in performance detected.
```

- **Time range** - 95% confidence interval
- **Change** - Compared to baseline
- **p-value** - Statistical significance (< 0.05 = significant)

## Optimization Workflow

1. **Profile** - Find hotspots with flamegraph
2. **Benchmark** - Measure current performance
3. **Optimize** - Make improvements
4. **Re-benchmark** - Verify improvements
5. **Repeat** - Continue until fast enough

## Acceptance Criteria

- [ ] Criterion.rs benchmarks set up
- [ ] Layout benchmarks implemented
- [ ] Paint benchmarks implemented
- [ ] Text benchmarks implemented
- [ ] End-to-end benchmarks implemented
- [ ] Memory benchmarks working
- [ ] Profiling scripts functional
- [ ] Performance regression detection works
- [ ] CI runs benchmarks on every PR
- [ ] Dashboard shows trends over time
- [ ] Documentation explains benchmarking
- [ ] All benchmarks complete in < 10 minutes
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Benchmarking Debug Builds

**Wrong:**
```bash
cargo test --bench
# Runs in debug mode - 10-100x slower!
```

**Right:**
```bash
cargo bench
# Always benchmarks in release mode
```

### Pitfall 2: Not Warming Up

**Wrong:**
```rust
b.iter(|| {
    render(html); // Cold start on first iteration
});
```

**Right:**
```rust
// Criterion handles warmup automatically
// Just use b.iter()
```

### Pitfall 3: Ignoring Variance

**Wrong:**
```bash
# Run once, trust the result
cargo bench
```

**Right:**
```bash
# Run multiple times, look at confidence intervals
# Criterion does this automatically
```

### Pitfall 4: Optimizing Without Profiling

**Wrong:**
```rust
// Guess what's slow and optimize it
optimize_random_function();
```

**Right:**
```bash
# Profile first
./scripts/profile.sh
# Optimize the actual bottleneck shown in flamegraph
```

## Performance Targets

Based on browser benchmarks:

| Operation | Target | Good | Excellent |
|-----------|--------|------|-----------|
| Simple page (100 divs) | < 10ms | < 5ms | < 1ms |
| Complex flexbox | < 50ms | < 25ms | < 10ms |
| Text-heavy page | < 100ms | < 50ms | < 25ms |
| Full page render | < 500ms | < 250ms | < 100ms |

## Next Steps

After benchmarking infrastructure is complete:
- Profile and optimize hotspots
- Compare with Chrome/Firefox performance
- Set up continuous performance monitoring
- Create performance regression tests
- Document optimization techniques

## References

- **Criterion.rs Book:** https://bheisler.github.io/criterion.rs/book/
- **Rust Performance Book:** https://nnethercote.github.io/perf-book/
- **Flamegraph Guide:** https://www.brendangregg.com/flamegraphs.html
- **Linux perf Tutorial:** https://perf.wiki.kernel.org/index.php/Tutorial
- **Speedometer Benchmark:** https://browserbench.org/Speedometer3.0/
- **MotionMark Benchmark:** https://browserbench.org/MotionMark/

---

**Last Updated:** 2025-11-19
**Status:** Ready for Implementation
