# Phase 6: Reference Tests (Pixel-Comparison Testing)

**Duration:** 1-2 weeks (ongoing)
**Prerequisites:**
- Basic rendering pipeline working
- PNG output capability
**Dependencies:**
- Image comparison library (image, pixelmatch)
- Test fixtures and golden images
**Output:** Comprehensive reference test suite with visual regression detection

## Objectives

Implement pixel-comparison reference testing to verify visual correctness of FastRender:

- **Reference test methodology** - Render two things, compare pixels
- **Test case structure** - Organized test.html + test-ref.html pairs
- **Pixel comparison algorithm** - Smart comparison allowing for anti-aliasing
- **Creating reference tests** - Test creation workflow
- **Image diffing tools** - Visualization of differences
- **CI integration** - Automated visual regression testing
- **Platform-specific handling** - Font and rendering differences
- **Test fixtures and golden images** - Reusable test components

This is **critical for visual correctness** - many layout bugs are only visible when rendered.

## Context

Reference tests (also called "reftests") verify that rendering is visually correct by comparing pixel output.

**How Reference Tests Work:**

1. **Render test case** (`test.html`) → Image A
2. **Render reference** (`test-ref.html`) → Image B
3. **Compare images** → Pass if visually identical

**Example Test Pair:**

**File: `test-flexbox-001.html`** (Test case - uses flexbox)
```html
<!DOCTYPE html>
<title>Flexbox: space-between justification</title>
<link rel="match" href="test-flexbox-001-ref.html">
<style>
.container {
    display: flex;
    justify-content: space-between;
    width: 400px;
    height: 100px;
    background: lightgray;
}
.item {
    width: 50px;
    height: 50px;
    background: blue;
}
</style>
<div class="container">
    <div class="item"></div>
    <div class="item"></div>
    <div class="item"></div>
</div>
```

**File: `test-flexbox-001-ref.html`** (Reference - uses absolute positioning)
```html
<!DOCTYPE html>
<title>Reference for test-flexbox-001</title>
<style>
.container {
    position: relative;
    width: 400px;
    height: 100px;
    background: lightgray;
}
.item {
    position: absolute;
    top: 0;
    width: 50px;
    height: 50px;
    background: blue;
}
.item1 { left: 0; }
.item2 { left: 175px; }
.item3 { left: 350px; }
</style>
<div class="container">
    <div class="item item1"></div>
    <div class="item item2"></div>
    <div class="item item3"></div>
</div>
```

Both should render identically, but using different CSS features. This tests that flexbox works correctly.

**From Servo Reference Tests:**
> "Reference tests are a way of testing the visual output of a browser. They compare the rendering of a test page against a reference rendering, and pass if they match."

## The Problem V1 Has

V1 has no visual testing:
- Can't detect visual regressions
- No way to verify pixel-perfect rendering
- Manual visual inspection only
- No systematic coverage of visual features
- Can't catch subtle rendering bugs

This means visual bugs can slip through unnoticed.

## The Solution

Implement a comprehensive reference test infrastructure:

1. **Test structure** - Organized test/reference pairs
2. **Test harness** - Automatically find and run tests
3. **Image comparison** - Smart pixel comparison with tolerance
4. **Diff visualization** - Show exactly what differs
5. **Golden images** - Store expected outputs for comparison
6. **CI integration** - Fail builds on visual regressions
7. **Test creation tools** - Easy workflow for creating new tests
8. **Platform handling** - Deal with font/rendering variations

## Specification References

**Primary:**
- **WPT Reftest Documentation:** https://web-platform-tests.org/writing-tests/reftests.html
- **CSS Test Format:** https://github.com/w3c/csswg-test/blob/master/README.md
- **Servo Reftest Documentation:** https://github.com/servo/servo/wiki/Reftest

**Image Comparison:**
- **Pixelmatch Algorithm:** https://github.com/mapbox/pixelmatch
- **CIEDE2000 Color Difference:** http://zschuessler.github.io/DeltaE/learn/

**Best Practices:**
- Mozilla RefTest Best Practices
- Chromium Layout Test Guidelines

## Step-by-Step Implementation

### Step 1: Create Reference Test Directory Structure (Day 1 Morning)

```bash
# Create directory structure
mkdir -p /home/user/fastrender/tests/reference/tests/block
mkdir -p /home/user/fastrender/tests/reference/tests/inline
mkdir -p /home/user/fastrender/tests/reference/tests/flexbox
mkdir -p /home/user/fastrender/tests/reference/tests/grid
mkdir -p /home/user/fastrender/tests/reference/tests/table
mkdir -p /home/user/fastrender/tests/reference/tests/position
mkdir -p /home/user/fastrender/tests/reference/tests/text
mkdir -p /home/user/fastrender/tests/reference/tests/paint
mkdir -p /home/user/fastrender/tests/reference/fixtures
mkdir -p /home/user/fastrender/tests/reference/actual
mkdir -p /home/user/fastrender/tests/reference/diffs

# Create .gitignore for generated files
cat > /home/user/fastrender/tests/reference/.gitignore << 'EOF'
# Generated test outputs
actual/
diffs/
*.png
!fixtures/*.png
EOF
```

**Directory Layout:**

```
tests/reference/
├── tests/                      # Test cases organized by feature
│   ├── block/                 # Block layout tests
│   │   ├── block-001.html
│   │   ├── block-001-ref.html
│   │   ├── block-002.html
│   │   └── block-002-ref.html
│   ├── inline/                # Inline layout tests
│   ├── flexbox/               # Flexbox tests
│   ├── grid/                  # Grid tests
│   ├── table/                 # Table tests
│   ├── position/              # Positioning tests
│   ├── text/                  # Text rendering tests
│   └── paint/                 # Painting tests
├── fixtures/                   # Reusable components
│   ├── fonts/                 # Test fonts
│   ├── images/                # Test images
│   └── common.css            # Common styles
├── actual/                     # Generated (gitignored)
│   ├── block-001.png
│   └── ...
├── diffs/                      # Diff images (gitignored)
│   ├── block-001-diff.png
│   └── ...
└── harness.rs                 # Test runner
```

### Step 2: Create Reference Test Harness (Day 1-2)

**File: `tests/reference/harness.rs`**

```rust
//! Reference test harness
//!
//! Runs reference tests and compares pixel output.

use fastrender::Renderer;
use std::path::{Path, PathBuf};
use std::fs;
use image::{ImageBuffer, Rgba, RgbaImage, DynamicImage};

/// Reference test
struct ReferenceTest {
    /// Test name (e.g., "block-001")
    name: String,

    /// Path to test HTML
    test_path: PathBuf,

    /// Path to reference HTML
    ref_path: PathBuf,

    /// Test type
    test_type: TestType,
}

/// Test type
#[derive(Debug, Clone, Copy)]
enum TestType {
    /// Test should match reference
    Match,

    /// Test should NOT match reference
    Mismatch,
}

impl ReferenceTest {
    /// Discover reference tests in directory
    fn discover(dir: &Path) -> Result<Vec<Self>> {
        let mut tests = Vec::new();

        // Find all HTML files
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if !is_test_file(&path) {
                continue;
            }

            // Parse test to find reference
            let html = fs::read_to_string(&path)?;
            let (ref_path, test_type) = parse_reference_link(&html, &path)?;

            if let Some(ref_path) = ref_path {
                let name = path.file_stem()
                    .and_then(|s| s.to_str())
                    .ok_or("Invalid test name")?
                    .to_string();

                tests.push(ReferenceTest {
                    name,
                    test_path: path,
                    ref_path,
                    test_type,
                });
            }
        }

        // Recurse into subdirectories
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() && !path.file_name().unwrap().to_str().unwrap().starts_with('.') {
                tests.extend(Self::discover(&path)?);
            }
        }

        Ok(tests)
    }

    /// Run this test
    fn run(&self, renderer: &Renderer) -> TestResult {
        println!("Running: {}", self.name);

        // Render test
        let test_output = match self.render_html(&self.test_path, renderer) {
            Ok(output) => output,
            Err(e) => {
                return TestResult::Error {
                    test: self.name.clone(),
                    error: format!("Failed to render test: {}", e),
                };
            }
        };

        // Render reference
        let ref_output = match self.render_html(&self.ref_path, renderer) {
            Ok(output) => output,
            Err(e) => {
                return TestResult::Error {
                    test: self.name.clone(),
                    error: format!("Failed to render reference: {}", e),
                };
            }
        };

        // Save actual output
        let actual_path = format!("tests/reference/actual/{}.png", self.name);
        if let Err(e) = test_output.save(&actual_path) {
            eprintln!("Warning: Failed to save actual output: {}", e);
        }

        // Compare
        let comparison = compare_images(&test_output, &ref_output);

        match self.test_type {
            TestType::Match => {
                if comparison.matches() {
                    TestResult::Pass {
                        test: self.name.clone(),
                    }
                } else {
                    // Save diff image
                    let diff_path = format!("tests/reference/diffs/{}-diff.png", self.name);
                    if let Err(e) = comparison.diff_image.save(&diff_path) {
                        eprintln!("Warning: Failed to save diff image: {}", e);
                    }

                    TestResult::Fail {
                        test: self.name.clone(),
                        reason: format!(
                            "{:.3}% mismatch (threshold: {:.3}%)\nDiff: {}",
                            comparison.mismatch_percentage * 100.0,
                            MISMATCH_THRESHOLD * 100.0,
                            diff_path
                        ),
                    }
                }
            }
            TestType::Mismatch => {
                if !comparison.matches() {
                    TestResult::Pass {
                        test: self.name.clone(),
                    }
                } else {
                    TestResult::Fail {
                        test: self.name.clone(),
                        reason: "Test and reference should NOT match, but they do".to_string(),
                    }
                }
            }
        }
    }

    /// Render HTML to image
    fn render_html(&self, path: &Path, renderer: &Renderer) -> Result<RgbaImage> {
        let html = fs::read_to_string(path)?;

        // Render to PNG bytes
        let png_bytes = renderer.render_to_png(&html, 800, 600)?;

        // Load as image
        let img = image::load_from_memory(&png_bytes)?;

        Ok(img.to_rgba8())
    }
}

/// Test result
#[derive(Debug)]
enum TestResult {
    Pass { test: String },
    Fail { test: String, reason: String },
    Error { test: String, error: String },
}

/// Image comparison result
struct ImageComparison {
    width: u32,
    height: u32,
    total_pixels: u32,
    diff_pixels: u32,
    mismatch_percentage: f32,
    diff_image: RgbaImage,
}

impl ImageComparison {
    fn matches(&self) -> bool {
        self.mismatch_percentage <= MISMATCH_THRESHOLD
    }
}

/// Maximum allowed mismatch percentage
const MISMATCH_THRESHOLD: f32 = 0.001; // 0.1%

/// Compare two images
fn compare_images(test: &RgbaImage, reference: &RgbaImage) -> ImageComparison {
    assert_eq!(
        test.dimensions(),
        reference.dimensions(),
        "Image dimensions must match"
    );

    let (width, height) = test.dimensions();
    let total_pixels = (width * height) as f32;

    let mut diff_count = 0u32;
    let mut diff_image = ImageBuffer::new(width, height);

    for y in 0..height {
        for x in 0..width {
            let test_pixel = test.get_pixel(x, y);
            let ref_pixel = reference.get_pixel(x, y);

            if pixels_differ(test_pixel, ref_pixel) {
                diff_count += 1;

                // Highlight difference
                diff_image.put_pixel(x, y, highlight_diff(test_pixel, ref_pixel));
            } else {
                // Keep test pixel
                diff_image.put_pixel(x, y, *test_pixel);
            }
        }
    }

    ImageComparison {
        width,
        height,
        total_pixels: total_pixels as u32,
        diff_pixels: diff_count,
        mismatch_percentage: diff_count as f32 / total_pixels,
        diff_image,
    }
}

/// Check if two pixels differ (with tolerance)
fn pixels_differ(a: &Rgba<u8>, b: &Rgba<u8>) -> bool {
    // Per-channel tolerance for anti-aliasing differences
    const CHANNEL_TOLERANCE: u8 = 3;

    for i in 0..4 {
        let diff = (a[i] as i16 - b[i] as i16).unsigned_abs() as u8;
        if diff > CHANNEL_TOLERANCE {
            return true;
        }
    }

    false
}

/// Highlight difference between pixels
fn highlight_diff(test: &Rgba<u8>, _ref: &Rgba<u8>) -> Rgba<u8> {
    // Show test pixel with red overlay
    Rgba([
        test[0].saturating_add(100).min(255),
        test[1].saturating_div(2),
        test[2].saturating_div(2),
        test[3],
    ])
}

/// Check if file is a test file (not a reference)
fn is_test_file(path: &Path) -> bool {
    if path.extension().and_then(|s| s.to_str()) != Some("html") {
        return false;
    }

    let name = path.file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("");

    !name.ends_with("-ref.html") && !name.starts_with("ref-")
}

/// Parse reference link from HTML
///
/// Returns (reference_path, test_type)
fn parse_reference_link(html: &str, test_path: &Path) -> Result<(Option<PathBuf>, TestType)> {
    // Look for <link rel="match" href="...">
    if let Some(href) = extract_link_href(html, "match") {
        let ref_path = resolve_reference_path(&href, test_path)?;
        return Ok((Some(ref_path), TestType::Match));
    }

    // Look for <link rel="mismatch" href="...">
    if let Some(href) = extract_link_href(html, "mismatch") {
        let ref_path = resolve_reference_path(&href, test_path)?;
        return Ok((Some(ref_path), TestType::Mismatch));
    }

    Ok((None, TestType::Match))
}

/// Extract href from link tag
fn extract_link_href(html: &str, rel: &str) -> Option<String> {
    use regex::Regex;

    let pattern = format!(r#"<link\s+rel="{}"\s+href="([^"]+)""#, rel);
    let re = Regex::new(&pattern).ok()?;

    re.captures(html)
        .and_then(|c| c.get(1))
        .map(|m| m.as_str().to_string())
}

/// Resolve reference path relative to test
fn resolve_reference_path(href: &str, test_path: &Path) -> Result<PathBuf> {
    let test_dir = test_path.parent()
        .ok_or("Test has no parent directory")?;

    Ok(test_dir.join(href))
}

/// Run all reference tests
#[test]
fn run_all_reference_tests() {
    // Ensure output directories exist
    fs::create_dir_all("tests/reference/actual").unwrap();
    fs::create_dir_all("tests/reference/diffs").unwrap();

    // Discover tests
    let tests = ReferenceTest::discover(Path::new("tests/reference/tests"))
        .expect("Failed to discover tests");

    println!("Found {} reference tests", tests.len());

    // Create renderer
    let renderer = Renderer::new();

    // Run tests
    let mut results = Vec::new();
    for test in &tests {
        results.push(test.run(&renderer));
    }

    // Print summary
    let passed = results.iter().filter(|r| matches!(r, TestResult::Pass { .. })).count();
    let failed = results.iter().filter(|r| matches!(r, TestResult::Fail { .. })).count();
    let errors = results.iter().filter(|r| matches!(r, TestResult::Error { .. })).count();

    println!("\n=== Reference Test Results ===");
    println!("Passed: {}", passed);
    println!("Failed: {}", failed);
    println!("Errors: {}", errors);
    println!("Total:  {}", results.len());

    // Print failures
    if failed > 0 || errors > 0 {
        println!("\n=== Failures ===");
        for result in &results {
            match result {
                TestResult::Fail { test, reason } => {
                    println!("FAIL: {}\n  {}", test, reason);
                }
                TestResult::Error { test, error } => {
                    println!("ERROR: {}\n  {}", test, error);
                }
                _ => {}
            }
        }

        panic!("{} reference tests failed", failed + errors);
    }
}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
```

### Step 3: Create Initial Reference Tests (Day 2-3)

**File: `tests/reference/tests/block/block-001.html`**

```html
<!DOCTYPE html>
<link rel="match" href="block-001-ref.html">
<title>Block layout: vertical stacking</title>
<style>
body { margin: 0; padding: 0; font-family: monospace; }
.block {
    width: 200px;
    height: 50px;
    background: blue;
    margin: 10px;
}
</style>
<div class="block"></div>
<div class="block"></div>
<div class="block"></div>
```

**File: `tests/reference/tests/block/block-001-ref.html`**

```html
<!DOCTYPE html>
<title>Reference for block-001</title>
<style>
body { margin: 0; padding: 0; }
.box {
    position: absolute;
    width: 200px;
    height: 50px;
    background: blue;
    left: 10px;
}
</style>
<div class="box" style="top: 10px;"></div>
<div class="box" style="top: 70px;"></div>
<div class="box" style="top: 130px;"></div>
```

**File: `tests/reference/tests/block/block-002.html`**

```html
<!DOCTYPE html>
<link rel="match" href="block-002-ref.html">
<title>Block layout: auto width</title>
<style>
body { margin: 0; padding: 20px; }
.container {
    width: 400px;
    border: 2px solid black;
}
.block {
    /* auto width should fill container */
    height: 50px;
    background: green;
}
</style>
<div class="container">
    <div class="block"></div>
</div>
```

**File: `tests/reference/tests/block/block-002-ref.html`**

```html
<!DOCTYPE html>
<title>Reference for block-002</title>
<style>
body { margin: 0; padding: 20px; }
.container {
    width: 400px;
    border: 2px solid black;
}
.block {
    width: 400px;
    height: 50px;
    background: green;
}
</style>
<div class="container">
    <div class="block"></div>
</div>
```

**File: `tests/reference/tests/inline/inline-001.html`**

```html
<!DOCTYPE html>
<link rel="match" href="inline-001-ref.html">
<title>Inline layout: horizontal flow</title>
<style>
body { margin: 0; padding: 10px; font-family: monospace; font-size: 16px; }
span {
    background: yellow;
    padding: 5px;
}
</style>
<div>
    <span>One</span>
    <span>Two</span>
    <span>Three</span>
</div>
```

**File: `tests/reference/tests/inline/inline-001-ref.html`**

```html
<!DOCTYPE html>
<title>Reference for inline-001</title>
<style>
body { margin: 0; padding: 10px; font-family: monospace; font-size: 16px; }
.box {
    display: inline-block;
    background: yellow;
    padding: 5px;
}
</style>
<div>
    <div class="box">One</div><!--
 --><div class="box">Two</div><!--
 --><div class="box">Three</div>
</div>
```

**File: `tests/reference/tests/flexbox/flex-001.html`**

```html
<!DOCTYPE html>
<link rel="match" href="flex-001-ref.html">
<title>Flexbox: basic horizontal layout</title>
<style>
body { margin: 0; padding: 20px; }
.container {
    display: flex;
    width: 400px;
    height: 100px;
    background: lightgray;
}
.item {
    width: 100px;
    height: 50px;
    background: blue;
}
</style>
<div class="container">
    <div class="item"></div>
    <div class="item"></div>
    <div class="item"></div>
</div>
```

**File: `tests/reference/tests/flexbox/flex-001-ref.html`**

```html
<!DOCTYPE html>
<title>Reference for flex-001</title>
<style>
body { margin: 0; padding: 20px; }
.container {
    position: relative;
    width: 400px;
    height: 100px;
    background: lightgray;
}
.item {
    position: absolute;
    width: 100px;
    height: 50px;
    background: blue;
    top: 0;
}
.item1 { left: 0; }
.item2 { left: 100px; }
.item3 { left: 200px; }
</style>
<div class="container">
    <div class="item item1"></div>
    <div class="item item2"></div>
    <div class="item item3"></div>
</div>
```

### Step 4: Create Test Fixtures (Day 3)

**File: `tests/reference/fixtures/common.css`**

```css
/* Common styles for reference tests */

/* Reset */
* {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}

/* Use consistent font */
body {
    font-family: monospace;
    font-size: 16px;
    line-height: 1.5;
}

/* Test utilities */
.red { background: red; }
.green { background: green; }
.blue { background: blue; }
.yellow { background: yellow; }

.border { border: 2px solid black; }

.w100 { width: 100px; }
.w200 { width: 200px; }
.w400 { width: 400px; }

.h50 { height: 50px; }
.h100 { height: 100px; }

.p10 { padding: 10px; }
.p20 { padding: 20px; }

.m10 { margin: 10px; }
.m20 { margin: 20px; }
```

**File: `tests/reference/fixtures/test-font.html`**

```html
<!-- Include a known test font to avoid platform differences -->
<style>
@font-face {
    font-family: 'TestFont';
    src: url('fonts/Ahem.ttf');
}
body {
    font-family: 'TestFont', monospace;
}
</style>
```

### Step 5: Create Test Creation Tool (Day 4)

**File: `scripts/create_reftest.sh`**

```bash
#!/bin/bash
# Create a new reference test

set -e

if [ $# -lt 2 ]; then
    echo "Usage: $0 <category> <test-name>"
    echo "Example: $0 flexbox justify-content-space-between"
    exit 1
fi

CATEGORY=$1
TEST_NAME=$2
TEST_DIR="tests/reference/tests/$CATEGORY"
TEST_FILE="$TEST_DIR/$TEST_NAME.html"
REF_FILE="$TEST_DIR/$TEST_NAME-ref.html"

# Create category directory if needed
mkdir -p "$TEST_DIR"

# Check if test already exists
if [ -f "$TEST_FILE" ]; then
    echo "Error: Test already exists: $TEST_FILE"
    exit 1
fi

# Create test file
cat > "$TEST_FILE" << 'EOF'
<!DOCTYPE html>
<link rel="match" href="TESTNAME-ref.html">
<title>CATEGORY: DESCRIPTION</title>
<style>
body {
    margin: 0;
    padding: 20px;
    font-family: monospace;
}

/* Add your test styles here */

</style>

<!-- Add your test HTML here -->

EOF

# Create reference file
cat > "$REF_FILE" << 'EOF'
<!DOCTYPE html>
<title>Reference for TESTNAME</title>
<style>
body {
    margin: 0;
    padding: 20px;
    font-family: monospace;
}

/* Add your reference styles here */

</style>

<!-- Add your reference HTML here -->

EOF

# Replace placeholders
sed -i "s/TESTNAME/$TEST_NAME/g" "$TEST_FILE" "$REF_FILE"
sed -i "s/CATEGORY/$CATEGORY/g" "$TEST_FILE" "$REF_FILE"

echo "Created test pair:"
echo "  Test:      $TEST_FILE"
echo "  Reference: $REF_FILE"
echo ""
echo "Next steps:"
echo "  1. Edit both files to create your test"
echo "  2. Run: cargo test --test reference_harness"
echo "  3. Check: tests/reference/diffs/ for any failures"
```

**Make it executable:**

```bash
chmod +x scripts/create_reftest.sh
```

### Step 6: Create Visual Diff Viewer (Day 4-5)

**File: `scripts/view_diffs.py`**

```python
#!/usr/bin/env python3
"""
Visual diff viewer for reference tests

Generates HTML page showing test failures with diffs.
"""

import os
import glob
from pathlib import Path

def generate_diff_viewer():
    """Generate HTML diff viewer"""

    diffs_dir = Path('tests/reference/diffs')
    actual_dir = Path('tests/reference/actual')

    if not diffs_dir.exists():
        print("No diffs directory found")
        return

    diff_images = sorted(diffs_dir.glob('*-diff.png'))

    if not diff_images:
        print("No diff images found - all tests passed!")
        return

    html = """
    <!DOCTYPE html>
    <html>
    <head>
        <title>Reference Test Failures</title>
        <style>
            body {
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
                max-width: 1400px;
                margin: 0 auto;
                padding: 20px;
                background: #f5f5f5;
            }
            h1 {
                color: #333;
            }
            .failure {
                background: white;
                margin: 20px 0;
                padding: 20px;
                border-radius: 8px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }
            .failure h2 {
                margin-top: 0;
                color: #e53e3e;
            }
            .images {
                display: grid;
                grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
                gap: 20px;
                margin-top: 20px;
            }
            .image-container {
                text-align: center;
            }
            .image-container h3 {
                font-size: 14px;
                color: #666;
                margin-bottom: 10px;
            }
            .image-container img {
                max-width: 100%;
                border: 1px solid #ddd;
                border-radius: 4px;
            }
            .diff-img {
                border-color: #e53e3e !important;
            }
        </style>
    </head>
    <body>
        <h1>Reference Test Failures</h1>
        <p>Found """ + str(len(diff_images)) + """ failing tests</p>
    """

    for diff_path in diff_images:
        test_name = diff_path.stem.replace('-diff', '')
        actual_path = actual_dir / f"{test_name}.png"

        html += f"""
        <div class="failure">
            <h2>{test_name}</h2>
            <div class="images">
                <div class="image-container">
                    <h3>Actual Output</h3>
                    <img src="../actual/{test_name}.png" alt="Actual">
                </div>
                <div class="image-container">
                    <h3>Diff (red = mismatch)</h3>
                    <img src="../diffs/{diff_path.name}" alt="Diff" class="diff-img">
                </div>
            </div>
        </div>
        """

    html += """
    </body>
    </html>
    """

    output_path = diffs_dir / 'viewer.html'
    with open(output_path, 'w') as f:
        f.write(html)

    print(f"Diff viewer generated: file://{output_path.absolute()}")

if __name__ == '__main__':
    generate_diff_viewer()
```

**Make it executable:**

```bash
chmod +x scripts/view_diffs.py
```

### Step 7: CI Integration (Day 5)

**File: `.github/workflows/reference-tests.yml`**

```yaml
name: Reference Tests

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  reference-tests:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          profile: minimal

      - name: Install system dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y \
            libfreetype6-dev \
            libfontconfig1-dev \
            fonts-liberation

      - name: Cache cargo
        uses: actions/cache@v3
        with:
          path: |
            ~/.cargo/registry
            ~/.cargo/git
            target
          key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}

      - name: Run reference tests
        run: cargo test --test reference_harness

      - name: Generate diff viewer
        if: failure()
        run: |
          python3 scripts/view_diffs.py

      - name: Upload test outputs
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: reference-test-failures
          path: |
            tests/reference/actual/
            tests/reference/diffs/

      - name: Comment PR with failures
        if: failure() && github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const diffs = fs.readdirSync('tests/reference/diffs')
              .filter(f => f.endsWith('-diff.png'));

            const body = `## ❌ Reference Tests Failed

            ${diffs.length} visual regression(s) detected.

            Download artifacts to view differences.
            `;

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: body
            });
```

## Creating New Reference Tests

### Workflow

1. **Create test pair:**
   ```bash
   ./scripts/create_reftest.sh flexbox my-test
   ```

2. **Edit test and reference:**
   ```bash
   # Edit test case
   vim tests/reference/tests/flexbox/my-test.html

   # Edit reference
   vim tests/reference/tests/flexbox/my-test-ref.html
   ```

3. **Run tests:**
   ```bash
   cargo test --test reference_harness
   ```

4. **View failures:**
   ```bash
   python3 scripts/view_diffs.py
   # Opens browser to view diffs
   ```

5. **Fix and re-run:**
   - Fix the rendering bug
   - Re-run tests
   - Repeat until passing

### Best Practices

**DO:**
- Use simple, minimal test cases
- Make reference use different CSS features than test
- Use consistent fonts (monospace or test font)
- Reset margins/padding
- Test one thing per test case
- Name tests descriptively

**DON'T:**
- Use platform-specific fonts
- Rely on default styles
- Make tests too complex
- Use external resources
- Test multiple features in one test

## Handling Platform Differences

### Font Differences

**Problem:** Different platforms render fonts differently

**Solution:** Use Ahem font or monospace

```html
<style>
@font-face {
    font-family: 'Ahem';
    src: url('../fixtures/fonts/Ahem.ttf');
}
body {
    font-family: 'Ahem', monospace;
}
</style>
```

### Anti-Aliasing Differences

**Problem:** Sub-pixel rendering varies

**Solution:** Use pixel tolerance in comparison

```rust
const CHANNEL_TOLERANCE: u8 = 3; // Allow small differences
```

### Rendering Differences

**Problem:** Operating systems render differently

**Solution:** Run tests in consistent environment (Docker)

```dockerfile
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    libfreetype6-dev \
    libfontconfig1-dev \
    fonts-liberation

# Set font rendering to consistent mode
ENV FREETYPE_PROPERTIES="truetype:interpreter-version=35"
```

## Acceptance Criteria

- [ ] Reference test directory structure created
- [ ] Test harness discovers and runs tests
- [ ] Image comparison works with tolerance
- [ ] Diff images are generated and saved
- [ ] At least 50 reference tests created
- [ ] Tests cover all major layout features
- [ ] Test creation script works
- [ ] Diff viewer generates HTML report
- [ ] CI runs reference tests on every commit
- [ ] CI uploads artifacts on failure
- [ ] Platform differences are handled
- [ ] Documentation explains test creation
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Pixel-Perfect Comparison

**Wrong:**
```rust
if test_img == ref_img {
    Pass
}
// Fails on tiny differences!
```

**Right:**
```rust
if compare_images(test_img, ref_img).mismatch_percentage <= 0.001 {
    Pass
}
// Tolerates anti-aliasing
```

### Pitfall 2: Platform-Specific Fonts

**Wrong:**
```html
<style>
body { font-family: Arial; }
</style>
<!-- Arial looks different on different platforms!
```

**Right:**
```html
<style>
body { font-family: monospace; }
</style>
<!-- Monospace is more consistent -->
```

### Pitfall 3: Complex Test Cases

**Wrong:**
```html
<!-- Test multiple features at once -->
<div style="display: flex; justify-content: space-between;
            align-items: center; flex-wrap: wrap;">
    <!-- 10 complex children -->
</div>
<!-- Hard to debug when it fails! -->
```

**Right:**
```html
<!-- Test one feature -->
<div style="display: flex; justify-content: space-between;">
    <div></div>
    <div></div>
</div>
<!-- Easy to understand and debug -->
```

### Pitfall 4: No Test/Reference Difference

**Wrong:**
```html
<!-- test.html and test-ref.html are identical -->
<!-- This doesn't test anything! -->
```

**Right:**
```html
<!-- test.html uses flexbox -->
<!-- test-ref.html uses absolute positioning -->
<!-- Same visual result, different implementation -->
```

## Performance Considerations

1. **Parallel test execution** - Run tests in parallel
2. **Incremental testing** - Only re-run changed tests
3. **Image caching** - Cache rendered images
4. **Fast comparison** - Use efficient pixel comparison
5. **Progressive rendering** - Render at lower resolution first

## Next Steps

After reference tests are complete:
- **06-wpt-integration.md** - Integrate WPT tests
- **06-benchmarking.md** - Performance benchmarking
- Create tests for every new feature
- Track visual regression trends

## References

- **WPT Reftest Docs:** https://web-platform-tests.org/writing-tests/reftests.html
- **Servo Reftest:** https://github.com/servo/servo/wiki/Reftest
- **CSS Test Guidelines:** https://github.com/w3c/csswg-test/blob/master/README.md
- **Pixelmatch:** https://github.com/mapbox/pixelmatch
- **Image Comparison Algorithms:** https://en.wikipedia.org/wiki/Peak_signal-to-noise_ratio

---

**Last Updated:** 2025-11-19
**Status:** Ready for Implementation
