# Phase 6: Testing Strategy

**Status:** Continuous (all phases)
**Prerequisites:** None (start immediately)
**Output:** Comprehensive test infrastructure and test suite

## Overview

Testing is NOT a phase that happens after implementation. It's continuous, integrated into every phase.

**Rule:** Write tests BEFORE implementation, not after.

## Test Pyramid

```
                /\
               /  \
              /    \
             / WPT  \          ~10,000 tests
            /--------\
           /          \
          / Reference  \       ~1,000 tests
         /--------------\
        /                \
       /   Integration    \    ~100 tests
      /--------------------\
     /                      \
    /       Unit Tests       \  ~1,000 tests
   /--------------------------\
```

## Test Types

### 1. Unit Tests

**Purpose:** Verify individual functions and algorithms work correctly

**Location:** `tests/unit/`

**Characteristics:**
- Fast (<1ms each)
- Isolated (no I/O, no network)
- Deterministic (same input → same output)
- Focused (test one thing)

**Example Structure:**

```
tests/unit/
├── geom/
│   └── rect_test.rs            # Test geometry primitives
├── css/
│   ├── parser_test.rs          # Test CSS parsing
│   ├── selector_test.rs        # Test selector matching
│   └── cascade_test.rs         # Test cascade algorithm
├── layout/
│   ├── block_test.rs           # Test block layout algorithm
│   ├── inline_test.rs          # Test inline layout algorithm
│   ├── flex_test.rs            # Test flex wrapper
│   ├── grid_test.rs            # Test grid wrapper
│   ├── table/
│   │   ├── structure_test.rs   # Test table structure analysis
│   │   ├── width_test.rs       # Test column width computation
│   │   └── height_test.rs      # Test row height computation
│   └── margin_collapse_test.rs # Test margin collapsing
├── text/
│   ├── shaping_test.rs         # Test text shaping
│   ├── line_break_test.rs      # Test line breaking
│   └── bidi_test.rs            # Test bidi algorithm
└── paint/
    ├── display_list_test.rs    # Test display list building
    └── raster_test.rs          # Test rasterization
```

**Example Unit Test:**

```rust
// tests/unit/layout/block_test.rs

use fastrender::layout::block::*;
use fastrender::tree::box_tree::*;
use fastrender::geom::*;
use fastrender::style::ComputedStyle;
use std::sync::Arc;

#[test]
fn test_block_layout_stacks_vertically() {
    // Create two block boxes
    let style = Arc::new(ComputedStyle {
        width: Some(Length::px(100.0)),
        height: Some(Length::px(50.0)),
        ..Default::default()
    });

    let child1 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    let child2 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);

    let parent_style = Arc::new(ComputedStyle::default());
    let parent = BoxNode::new_block(
        parent_style,
        FormattingContextType::Block,
        vec![child1, child2],
    );

    // Layout
    let mut layout = BlockLayout::new();
    let constraints = Constraints::definite(800.0, 600.0);
    let fragment = layout.layout(&parent, constraints);

    // Verify: children stack vertically
    assert_eq!(fragment.children.len(), 2);
    assert_eq!(fragment.children[0].position().y, 0.0);
    assert_eq!(fragment.children[1].position().y, 50.0); // After first child
}

#[test]
fn test_block_layout_auto_width() {
    // Block with auto width should fill containing block
    let style = Arc::new(ComputedStyle {
        width: None, // auto
        height: Some(Length::px(50.0)),
        ..Default::default()
    });

    let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

    let mut layout = BlockLayout::new();
    let constraints = Constraints::definite(800.0, 600.0);
    let fragment = layout.layout(&box_node, constraints);

    // Should fill width
    assert_eq!(fragment.size().width, 800.0);
    assert_eq!(fragment.size().height, 50.0);
}

#[test]
fn test_margin_collapse_adjacent_siblings() {
    // Two blocks with margins should collapse
    let style1 = Arc::new(ComputedStyle {
        height: Some(Length::px(50.0)),
        margin_bottom: Length::px(20.0),
        ..Default::default()
    });

    let style2 = Arc::new(ComputedStyle {
        height: Some(Length::px(50.0)),
        margin_top: Length::px(30.0),
        ..Default::default()
    });

    let child1 = BoxNode::new_block(style1, FormattingContextType::Block, vec![]);
    let child2 = BoxNode::new_block(style2, FormattingContextType::Block, vec![]);

    let parent_style = Arc::new(ComputedStyle::default());
    let parent = BoxNode::new_block(
        parent_style,
        FormattingContextType::Block,
        vec![child1, child2],
    );

    let mut layout = BlockLayout::new();
    let fragment = layout.layout(&parent, Constraints::definite(800.0, 600.0));

    // Margins should collapse - use larger of the two (30px)
    assert_eq!(fragment.children[0].position().y, 0.0);
    assert_eq!(fragment.children[1].position().y, 50.0 + 30.0); // Not 50 + 20 + 30!
}
```

**Coverage Goal:** >80% line coverage for all modules

**Verification:**
```bash
cargo tarpaulin --out Html --output-dir coverage/
```

### 2. Integration Tests

**Purpose:** Verify features work end-to-end

**Location:** `tests/integration/`

**Characteristics:**
- Slower (~10-100ms each)
- Test full rendering pipeline
- Verify feature completeness
- Use realistic HTML/CSS

**Example Structure:**

```
tests/integration/
├── flexbox_test.rs         # All flexbox features
├── grid_test.rs            # All grid features
├── table_test.rs           # All table features
├── text_test.rs            # Text rendering
├── transform_test.rs       # CSS transforms
├── gradient_test.rs        # Gradients
└── pseudo_elements_test.rs # ::before, ::after
```

**Example Integration Test:**

```rust
// tests/integration/flexbox_test.rs

use fastrender::Renderer;

#[test]
fn test_flexbox_justify_content_space_between() {
    let html = r#"
        <html>
            <head>
                <style>
                    .container {
                        display: flex;
                        justify-content: space-between;
                        width: 400px;
                        height: 100px;
                    }
                    .item {
                        width: 50px;
                        height: 50px;
                        background-color: red;
                    }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="item"></div>
                    <div class="item"></div>
                    <div class="item"></div>
                </div>
            </body>
        </html>
    "#;

    let renderer = Renderer::new();
    let result = renderer.render_to_png(html, 400, 100);

    assert!(result.is_ok(), "Rendering should succeed");

    // TODO: Verify pixel output (see reference tests)
}

#[test]
fn test_flexbox_flex_grow() {
    let html = r#"
        <html>
            <head>
                <style>
                    .container {
                        display: flex;
                        width: 400px;
                    }
                    .item1 {
                        flex-grow: 1;
                        background-color: red;
                    }
                    .item2 {
                        flex-grow: 2;
                        background-color: blue;
                    }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="item1">Item 1</div>
                    <div class="item2">Item 2</div>
                </div>
            </body>
        </html>
    "#;

    let renderer = Renderer::new();
    let result = renderer.render_to_png(html, 400, 100);

    assert!(result.is_ok());

    // Item 2 should be twice as wide as Item 1
    // Verify with reference test or fragment inspection
}
```

### 3. Reference Tests (Pixel Comparison)

**Purpose:** Verify visual correctness

**Location:** `tests/reference/`

**How It Works:**
1. Render HTML to PNG
2. Compare with reference image
3. Allow small tolerance for anti-aliasing differences

**Structure:**

```
tests/reference/
├── tests/
│   ├── block-001.html          # Test case
│   ├── block-002.html
│   ├── flex-001.html
│   ├── grid-001.html
│   ├── table-001.html
│   └── ...
├── expected/
│   ├── block-001.png           # Reference image
│   ├── block-002.png
│   └── ...
├── actual/                      # Generated (gitignored)
│   ├── block-001.png
│   └── ...
├── diffs/                       # Differences (gitignored)
│   ├── block-001-diff.png
│   └── ...
└── harness.rs                   # Test runner
```

**Test Harness:**

```rust
// tests/reference/harness.rs

use fastrender::Renderer;
use std::path::{Path, PathBuf};
use std::fs;
use image::{ImageBuffer, Rgba};

struct ReferenceTest {
    name: String,
    html_path: PathBuf,
    expected_path: PathBuf,
}

impl ReferenceTest {
    fn run(&self) -> Result<(), String> {
        // 1. Render HTML
        let html = fs::read_to_string(&self.html_path)
            .map_err(|e| format!("Failed to read HTML: {}", e))?;

        let renderer = Renderer::new();
        let png_data = renderer.render_to_png(&html, 800, 600)
            .map_err(|e| format!("Rendering failed: {}", e))?;

        // 2. Load expected image
        let expected_img = image::load_from_memory(&fs::read(&self.expected_path)?)
            .map_err(|e| format!("Failed to load expected image: {}", e))?
            .to_rgba8();

        // 3. Load actual image
        let actual_img = image::load_from_memory(&png_data)
            .map_err(|e| format!("Failed to decode rendered image: {}", e))?
            .to_rgba8();

        // 4. Compare
        let diff = compare_images(&expected_img, &actual_img)?;

        if diff.mismatch_percentage > 0.01 {
            // Save diff image
            save_diff_image(&diff, &self.name)?;

            return Err(format!(
                "Images differ by {:.2}%\nSee: tests/reference/diffs/{}-diff.png",
                diff.mismatch_percentage * 100.0,
                self.name
            ));
        }

        Ok(())
    }
}

struct ImageDiff {
    mismatch_percentage: f32,
    diff_image: ImageBuffer<Rgba<u8>, Vec<u8>>,
}

fn compare_images(
    expected: &ImageBuffer<Rgba<u8>, Vec<u8>>,
    actual: &ImageBuffer<Rgba<u8>, Vec<u8>>,
) -> Result<ImageDiff, String> {
    if expected.dimensions() != actual.dimensions() {
        return Err(format!(
            "Image dimensions differ: expected {:?}, got {:?}",
            expected.dimensions(),
            actual.dimensions()
        ));
    }

    let (width, height) = expected.dimensions();
    let mut diff_image = ImageBuffer::new(width, height);
    let mut mismatch_count = 0;
    let total_pixels = (width * height) as f32;

    for y in 0..height {
        for x in 0..width {
            let expected_pixel = expected.get_pixel(x, y);
            let actual_pixel = actual.get_pixel(x, y);

            if pixels_differ(expected_pixel, actual_pixel) {
                mismatch_count += 1;
                // Highlight difference in red
                diff_image.put_pixel(x, y, Rgba([255, 0, 0, 255]));
            } else {
                // Keep original pixel
                diff_image.put_pixel(x, y, *actual_pixel);
            }
        }
    }

    Ok(ImageDiff {
        mismatch_percentage: mismatch_count as f32 / total_pixels,
        diff_image,
    })
}

fn pixels_differ(a: &Rgba<u8>, b: &Rgba<u8>) -> bool {
    // Allow small tolerance for anti-aliasing differences
    const TOLERANCE: u8 = 2;

    for i in 0..4 {
        if a[i].abs_diff(b[i]) > TOLERANCE {
            return true;
        }
    }

    false
}

fn save_diff_image(diff: &ImageDiff, name: &str) -> Result<(), String> {
    let path = format!("tests/reference/diffs/{}-diff.png", name);
    diff.diff_image.save(&path)
        .map_err(|e| format!("Failed to save diff image: {}", e))
}

// Generate tests
#[test]
fn run_reference_tests() {
    let test_dir = Path::new("tests/reference/tests");
    let mut failures = Vec::new();

    for entry in fs::read_dir(test_dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("html") {
            let name = path.file_stem().unwrap().to_str().unwrap();

            let test = ReferenceTest {
                name: name.to_string(),
                html_path: path.clone(),
                expected_path: Path::new("tests/reference/expected")
                    .join(format!("{}.png", name)),
            };

            if let Err(e) = test.run() {
                failures.push(format!("{}: {}", name, e));
            }
        }
    }

    if !failures.is_empty() {
        panic!(
            "Reference tests failed:\n{}",
            failures.join("\n")
        );
    }
}
```

**Creating Reference Images:**

```bash
# First, manually verify rendering is correct
cargo run --bin render_test tests/reference/tests/block-001.html

# If correct, save as reference
mv output.png tests/reference/expected/block-001.png
```

**Goal:** >1,000 reference tests covering all CSS features

### 4. Web Platform Tests (WPT)

**Purpose:** Verify specification compliance

**Location:** `tests/wpt/` (git submodule)

**Setup:**

```bash
# Add WPT as submodule
cd /home/user/fastrender
git submodule add https://github.com/web-platform-tests/wpt.git tests/wpt
git submodule update --init --recursive
```

**Test Runner:**

```rust
// tests/wpt_runner.rs

use fastrender::Renderer;
use std::path::{Path, PathBuf};
use std::fs;

/// Run WPT test suite
///
/// WPT tests are organized by spec:
/// - css/css-flexbox/
/// - css/css-grid/
/// - css/css-tables/
/// - css/CSS2/
/// etc.
pub struct WptRunner {
    wpt_root: PathBuf,
    renderer: Renderer,
}

impl WptRunner {
    pub fn new() -> Self {
        Self {
            wpt_root: PathBuf::from("tests/wpt"),
            renderer: Renderer::new(),
        }
    }

    /// Run all tests in a directory
    pub fn run_suite(&self, suite_path: &str) -> WptResults {
        let full_path = self.wpt_root.join(suite_path);
        let mut results = WptResults::new();

        self.run_directory(&full_path, &mut results);

        results
    }

    fn run_directory(&self, dir: &Path, results: &mut WptResults) {
        for entry in fs::read_dir(dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();

            if path.is_dir() {
                self.run_directory(&path, results);
            } else if is_test_file(&path) {
                self.run_test(&path, results);
            }
        }
    }

    fn run_test(&self, path: &Path, results: &mut WptResults) {
        // WPT tests have a specific format:
        // - HTML file with test
        // - Optional -ref.html reference file
        // - <link rel="match" href="..."> specifies reference

        let test_name = path.to_string_lossy().to_string();

        match self.execute_test(path) {
            Ok(()) => results.pass(&test_name),
            Err(e) => results.fail(&test_name, e),
        }
    }

    fn execute_test(&self, path: &Path) -> Result<(), String> {
        // Parse test HTML
        let html = fs::read_to_string(path)
            .map_err(|e| format!("Failed to read test: {}", e))?;

        // Extract reference file (if any)
        let reference_path = extract_reference_path(&html, path)?;

        // Render test
        let test_output = self.renderer.render_to_png(&html, 800, 600)
            .map_err(|e| format!("Test rendering failed: {}", e))?;

        // If there's a reference, compare
        if let Some(ref_path) = reference_path {
            let ref_html = fs::read_to_string(&ref_path)
                .map_err(|e| format!("Failed to read reference: {}", e))?;

            let ref_output = self.renderer.render_to_png(&ref_html, 800, 600)
                .map_err(|e| format!("Reference rendering failed: {}", e))?;

            // Compare outputs
            if test_output != ref_output {
                return Err("Test output differs from reference".to_string());
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct WptResults {
    passed: Vec<String>,
    failed: Vec<(String, String)>,
}

impl WptResults {
    fn new() -> Self {
        Self {
            passed: Vec::new(),
            failed: Vec::new(),
        }
    }

    fn pass(&mut self, test: &str) {
        self.passed.push(test.to_string());
    }

    fn fail(&mut self, test: &str, error: String) {
        self.failed.push((test.to_string(), error));
    }

    pub fn summary(&self) -> String {
        format!(
            "WPT Results: {} passed, {} failed",
            self.passed.len(),
            self.failed.len()
        )
    }
}

fn is_test_file(path: &Path) -> bool {
    path.extension().and_then(|s| s.to_str()) == Some("html")
        && !path.to_string_lossy().contains("-ref.html")
}

fn extract_reference_path(html: &str, test_path: &Path) -> Result<Option<PathBuf>, String> {
    // Look for: <link rel="match" href="...">
    if let Some(start) = html.find(r#"<link rel="match" href=""#) {
        let after_href = &html[start + 26..]; // Skip to href value
        if let Some(end) = after_href.find('"') {
            let href = &after_href[..end];

            // Resolve relative to test file
            let ref_path = test_path.parent().unwrap().join(href);
            return Ok(Some(ref_path));
        }
    }

    Ok(None)
}

// Run WPT suites
#[test]
fn wpt_css_flexbox() {
    let runner = WptRunner::new();
    let results = runner.run_suite("css/css-flexbox");

    if !results.failed.is_empty() {
        panic!("WPT css-flexbox failures:\n{:#?}", results.failed);
    }
}

#[test]
fn wpt_css_grid() {
    let runner = WptRunner::new();
    let results = runner.run_suite("css/css-grid");

    if !results.failed.is_empty() {
        panic!("WPT css-grid failures:\n{:#?}", results.failed);
    }
}

#[test]
fn wpt_css_tables() {
    let runner = WptRunner::new();
    let results = runner.run_suite("css/css-tables");

    if !results.failed.is_empty() {
        panic!("WPT css-tables failures:\n{:#?}", results.failed);
    }
}
```

**Goal:** Pass all WPT tests for implemented features

### 5. Fuzz Testing

**Purpose:** Find crashes and panics with random input

**Location:** `fuzz/`

**Setup:**

```bash
cargo install cargo-fuzz
cargo fuzz init
```

**Fuzz Targets:**

```rust
// fuzz/fuzz_targets/html_parse.rs

#![no_main]
use libfuzzer_sys::fuzz_target;
use fastrender::Renderer;

fuzz_target!(|data: &[u8]| {
    if let Ok(html) = std::str::from_utf8(data) {
        let renderer = Renderer::new();

        // Should never panic, even on invalid HTML
        let _ = renderer.render_to_png(html, 800, 600);
    }
});
```

```rust
// fuzz/fuzz_targets/css_parse.rs

#![no_main]
use libfuzzer_sys::fuzz_target;
use fastrender::css::parse_stylesheet;

fuzz_target!(|data: &[u8]| {
    if let Ok(css) = std::str::from_utf8(data) {
        // Should never panic on any CSS
        let _ = parse_stylesheet(css);
    }
});
```

**Running Fuzz Tests:**

```bash
# Run for 1 hour
cargo fuzz run html_parse -- -max_total_time=3600

# Run for 10 million iterations
cargo fuzz run css_parse -- -runs=10000000
```

**Goal:** No crashes after 24 hours of fuzzing

### 6. Benchmark Tests

**Purpose:** Track performance over time

**Location:** `benches/`

**Setup:** Already exists, but expand it

```rust
// benches/layout_benchmark.rs

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use fastrender::Renderer;

fn bench_block_layout(c: &mut Criterion) {
    let mut group = c.benchmark_group("block-layout");

    for num_blocks in [10, 100, 1000].iter() {
        let html = generate_nested_blocks(*num_blocks);

        group.bench_with_input(
            BenchmarkId::from_parameter(num_blocks),
            &html,
            |b, html| {
                let renderer = Renderer::new();
                b.iter(|| {
                    renderer.render_to_png(black_box(html), 800, 600).unwrap()
                });
            },
        );
    }

    group.finish();
}

fn generate_nested_blocks(count: usize) -> String {
    let mut html = String::from("<html><body>");

    for _ in 0..count {
        html.push_str("<div style='padding: 10px;'>Block</div>");
    }

    html.push_str("</body></html>");
    html
}

criterion_group!(benches, bench_block_layout);
criterion_main!(benches);
```

**Running Benchmarks:**

```bash
cargo bench --bench layout_benchmark
```

**Goal:** <100ms for typical web page, <1s for complex page

## Test Organization

```
/home/user/fastrender/
├── tests/
│   ├── unit/                  # Fast, isolated tests
│   ├── integration/           # End-to-end feature tests
│   ├── reference/             # Pixel comparison
│   │   ├── tests/            # HTML files
│   │   ├── expected/         # Reference images
│   │   ├── actual/           # Generated (gitignored)
│   │   └── diffs/            # Differences (gitignored)
│   ├── wpt/                  # Web Platform Tests (submodule)
│   └── wpt_runner.rs         # WPT test harness
├── fuzz/
│   └── fuzz_targets/         # Fuzz tests
└── benches/                  # Performance benchmarks
```

## CI/CD Integration

**File:** `.github/workflows/ci.yml` (NEW)

```yaml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v2
        with:
          submodules: recursive  # Get WPT

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Run unit tests
        run: cargo test --tests

      - name: Run integration tests
        run: cargo test --test '*'

      - name: Run reference tests
        run: cargo test --test reference_tests

      - name: Run WPT tests
        run: cargo test --test wpt_runner

      - name: Check code coverage
        run: |
          cargo install cargo-tarpaulin
          cargo tarpaulin --out Xml

      - name: Upload coverage
        uses: codecov/codecov-action@v2

  fuzz:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v2

      - name: Install cargo-fuzz
        run: cargo install cargo-fuzz

      - name: Fuzz HTML parsing (1 minute)
        run: cargo fuzz run html_parse -- -max_total_time=60

      - name: Fuzz CSS parsing (1 minute)
        run: cargo fuzz run css_parse -- -max_total_time=60

  benchmark:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v2

      - name: Run benchmarks
        run: cargo bench

      - name: Compare with baseline
        run: |
          # TODO: Compare with previous benchmarks
          # Alert if performance degrades >10%
```

## Test-Driven Development Workflow

For every new feature:

1. **Write failing test first**
   ```bash
   # Create test file
   touch tests/unit/layout/new_feature_test.rs

   # Write test that fails
   cargo test new_feature
   # Should fail (feature not implemented)
   ```

2. **Implement minimal code to pass**
   ```bash
   # Implement feature
   # ...

   cargo test new_feature
   # Should pass
   ```

3. **Add reference test**
   ```bash
   # Create HTML test case
   echo '<html>...</html>' > tests/reference/tests/new-feature-001.html

   # Render and verify manually
   cargo run --bin render_test tests/reference/tests/new-feature-001.html

   # If correct, save as reference
   mv output.png tests/reference/expected/new-feature-001.png
   ```

4. **Run full test suite**
   ```bash
   cargo test
   ```

5. **Check coverage**
   ```bash
   cargo tarpaulin
   # Ensure coverage didn't drop
   ```

## Acceptance Criteria

Testing infrastructure is complete when:

- [ ] Unit test framework works (`cargo test`)
- [ ] Integration tests run end-to-end
- [ ] Reference test harness compares images
- [ ] WPT test runner executes spec tests
- [ ] Fuzz tests run without crashes
- [ ] Benchmark suite tracks performance
- [ ] CI runs all test types on every commit
- [ ] Coverage reporting works
- [ ] Test documentation is complete

## Metrics to Track

- **Test Count:** Total number of tests
- **Coverage:** Percentage of code covered by tests
- **WPT Pass Rate:** Percentage of WPT tests passing
- **Fuzz Duration:** Hours of fuzzing without crash
- **Performance:** Benchmark results over time

## Common Pitfalls

1. **Writing tests after implementation** - Defeats the purpose, write tests first
2. **Only testing happy path** - Test edge cases and errors
3. **Tests that don't fail** - If test never fails, it's not testing anything
4. **Flaky tests** - All tests must be deterministic
5. **Slow tests** - Keep unit tests <1ms each

## Next Steps

Start writing tests NOW:
- Create test directory structure
- Write first unit tests
- Set up reference test harness
- Add WPT submodule
- Configure CI

**Remember:** Every line of code should have a test. No exceptions.
