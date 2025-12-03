# Phase 6: Web Platform Tests Integration

**Duration:** 2-3 weeks (ongoing)
**Prerequisites:**
- Basic rendering pipeline working
- Reference test infrastructure (06-reference-tests.md)
**Dependencies:**
- WPT test suite (git submodule)
- Image comparison library
- HTML/CSS parser
**Output:** Automated WPT test runner with CI integration and progress tracking

## Objectives

Integrate the official W3C Web Platform Tests (WPT) to validate FastRender's compliance with web standards:

- **Download and organize WPT tests** - Official test suite from web-platform-tests.org
- **Implement WPT test runner** - Parse and execute different test types
- **Handle test formats** - Reftests, testharness.js, and manual tests
- **Track progress** - Measure how many tests pass over time
- **CI integration** - Run WPT tests on every commit

This is **critical for standards compliance** - WPT is the official test suite used by all major browsers.

## Context

Web Platform Tests (WPT) is the official W3C test suite containing over **1.8 million tests** covering all web platform specifications. Every major browser runs WPT to ensure standards compliance.

**What are WPT tests?**

WPT tests are HTML files that verify browser behavior:

1. **Reftests** - Visual comparison tests
   ```html
   <!DOCTYPE html>
   <link rel="match" href="reference.html">
   <title>CSS Test: Basic block layout</title>
   <div style="width: 100px; height: 100px; background: blue;"></div>
   ```

2. **Testharness.js tests** - JavaScript assertion tests
   ```html
   <!DOCTYPE html>
   <script src="/resources/testharness.js"></script>
   <script src="/resources/testharnessreport.js"></script>
   <script>
   test(() => {
     assert_equals(document.body.offsetWidth, 800);
   }, "Body width should be 800px");
   </script>
   ```

3. **Manual tests** - Require human verification
   ```html
   <!DOCTYPE html>
   <p>Do you see a blue square?</p>
   <div style="width: 100px; height: 100px; background: blue;"></div>
   ```

**WPT Directory Structure:**

```
wpt/
├── css/
│   ├── css-flexbox/          # ~500 flexbox tests
│   ├── css-grid/             # ~1000 grid tests
│   ├── css-tables/           # ~300 table tests
│   ├── css-inline/           # ~200 inline layout tests
│   ├── css-position/         # ~150 positioning tests
│   ├── css-backgrounds/      # ~200 background tests
│   ├── css-text/             # ~300 text tests
│   ├── css-fonts/            # ~150 font tests
│   ├── css-transforms/       # ~100 transform tests
│   ├── css-values/           # ~100 value tests
│   └── CSS2/                 # ~2000 CSS 2.1 tests
├── html/
├── dom/
└── ...
```

**From WPT Documentation:**
> "Web Platform Tests is a cross-browser test suite for the Web-platform stack. Writing tests in a way that allows them to be run in all browsers gives browser projects confidence that they are shipping software that is compatible with other implementations."

## The Problem V1 Has

V1 has no WPT integration:
- No way to verify standards compliance
- No systematic test coverage
- No comparison with other browsers
- No regression detection for spec changes
- Manual testing only

This means we can't be confident FastRender actually implements CSS correctly.

## The Solution

Implement a comprehensive WPT test runner that:

1. **Downloads WPT as git submodule** - Always have latest tests
2. **Parses test manifests** - Understand test types and metadata
3. **Executes different test types** - Reftests, testharness.js, manual
4. **Compares results** - Pixel comparison for reftests, assertion checking for JS tests
5. **Reports results** - Generate test reports and track progress
6. **Integrates with CI** - Run on every commit

## Specification References

**Primary:**
- **Web Platform Tests:** https://web-platform-tests.org/
- **WPT Documentation:** https://web-platform-tests.org/writing-tests/
- **WPT GitHub Repository:** https://github.com/web-platform-tests/wpt

**Test Format References:**
- **Reftest Format:** https://web-platform-tests.org/writing-tests/reftests.html
- **testharness.js API:** https://web-platform-tests.org/writing-tests/testharness-api.html
- **Test Metadata:** https://web-platform-tests.org/writing-tests/metadata.html

**CSS Test Suites:**
- **CSS Test Suites:** https://www.w3.org/Style/CSS/Test/
- **CSS WG Test Repository:** https://github.com/w3c/csswg-test

## Step-by-Step Implementation

### Step 1: Add WPT as Submodule (Day 1 Morning)

```bash
cd /home/user/fastrender

# Add WPT as git submodule
git submodule add https://github.com/web-platform-tests/wpt.git tests/wpt

# Initialize and update submodule
git submodule update --init --recursive

# WPT is large (~2GB), so we'll use sparse checkout for specific directories
cd tests/wpt
git config core.sparseCheckout true

# Only checkout CSS tests initially
echo "css/css-flexbox/*" >> .git/info/sparse-checkout
echo "css/css-grid/*" >> .git/info/sparse-checkout
echo "css/css-tables/*" >> .git/info/sparse-checkout
echo "css/css-inline/*" >> .git/info/sparse-checkout
echo "css/css-position/*" >> .git/info/sparse-checkout
echo "css/CSS2/*" >> .git/info/sparse-checkout
echo "resources/*" >> .git/info/sparse-checkout

git read-tree -mu HEAD
```

**File: `.gitmodules`**

```ini
[submodule "tests/wpt"]
    path = tests/wpt
    url = https://github.com/web-platform-tests/wpt.git
    # Don't update automatically - we control which version we test against
    update = none
```

**Create WPT directory structure:**

```bash
mkdir -p /home/user/fastrender/tests/wpt_runner
mkdir -p /home/user/fastrender/tests/wpt_results
mkdir -p /home/user/fastrender/tests/wpt_expectations
```

### Step 2: Create WPT Test Runner Module (Day 1)

**File: `tests/wpt_runner/mod.rs`**

```rust
//! Web Platform Tests (WPT) runner
//!
//! Executes WPT tests and compares with expected results.
//!
//! WPT test types:
//! 1. Reftests - Visual comparison with reference
//! 2. Testharness.js - JavaScript assertion tests
//! 3. Manual - Require human verification (we skip these)

pub mod parser;
pub mod executor;
pub mod reftest;
pub mod testharness;
pub mod results;
pub mod manifest;

use crate::Renderer;
use std::path::{Path, PathBuf};
use std::collections::HashMap;

/// WPT test runner
pub struct WptRunner {
    /// Path to WPT repository
    wpt_root: PathBuf,

    /// Renderer for executing tests
    renderer: Renderer,

    /// Test expectations (known failures)
    expectations: HashMap<String, TestExpectation>,

    /// Results accumulator
    results: results::WptResults,
}

impl WptRunner {
    /// Create new WPT runner
    pub fn new(wpt_root: impl AsRef<Path>) -> Self {
        Self {
            wpt_root: wpt_root.as_ref().to_path_buf(),
            renderer: Renderer::new(),
            expectations: HashMap::new(),
            results: results::WptResults::new(),
        }
    }

    /// Load test expectations from file
    pub fn load_expectations(&mut self, path: impl AsRef<Path>) -> Result<()> {
        let content = std::fs::read_to_string(path)?;
        self.expectations = parser::parse_expectations(&content)?;
        Ok(())
    }

    /// Run all tests in a directory
    pub fn run_suite(&mut self, suite_path: &str) -> Result<&results::WptResults> {
        let full_path = self.wpt_root.join(suite_path);

        eprintln!("Running WPT suite: {}", suite_path);

        // Parse manifest to find all tests
        let manifest = manifest::parse_manifest(&full_path)?;

        // Execute each test
        for test_path in manifest.tests() {
            self.run_test(&test_path)?;
        }

        Ok(&self.results)
    }

    /// Run a single test
    fn run_test(&mut self, test_path: &Path) -> Result<()> {
        let test_name = test_path.strip_prefix(&self.wpt_root)
            .unwrap_or(test_path)
            .to_string_lossy()
            .to_string();

        eprintln!("Running: {}", test_name);

        // Parse test to determine type
        let test_type = self.determine_test_type(test_path)?;

        // Execute based on type
        let result = match test_type {
            TestType::Reftest { reference } => {
                self.run_reftest(test_path, &reference)
            }
            TestType::Testharness => {
                self.run_testharness(test_path)
            }
            TestType::Manual => {
                // Skip manual tests
                self.results.skip(&test_name, "Manual test");
                return Ok(());
            }
            TestType::Unknown => {
                self.results.skip(&test_name, "Unknown test type");
                return Ok(());
            }
        };

        // Check expectations
        let expected = self.expectations.get(&test_name);

        match result {
            Ok(TestResult::Pass) => {
                if let Some(TestExpectation::Fail) = expected {
                    self.results.unexpected_pass(&test_name);
                } else {
                    self.results.pass(&test_name);
                }
            }
            Ok(TestResult::Fail(reason)) => {
                if let Some(TestExpectation::Fail) = expected {
                    self.results.expected_fail(&test_name, reason);
                } else {
                    self.results.fail(&test_name, reason);
                }
            }
            Err(e) => {
                self.results.error(&test_name, e.to_string());
            }
        }

        Ok(())
    }

    /// Determine test type from HTML content
    fn determine_test_type(&self, path: &Path) -> Result<TestType> {
        let html = std::fs::read_to_string(path)?;

        // Check for reftest
        if let Some(reference) = parser::extract_reftest_reference(&html, path)? {
            return Ok(TestType::Reftest { reference });
        }

        // Check for testharness.js
        if html.contains("testharness.js") {
            return Ok(TestType::Testharness);
        }

        // Check for manual test
        if html.contains("manual") || path.to_string_lossy().contains("manual") {
            return Ok(TestType::Manual);
        }

        Ok(TestType::Unknown)
    }

    /// Run a reftest
    fn run_reftest(&self, test_path: &Path, ref_path: &Path) -> Result<TestResult> {
        // Render test
        let test_html = std::fs::read_to_string(test_path)?;
        let test_png = self.renderer.render_to_png(&test_html, 800, 600)?;

        // Render reference
        let ref_html = std::fs::read_to_string(ref_path)?;
        let ref_png = self.renderer.render_to_png(&ref_html, 800, 600)?;

        // Compare images
        let comparison = reftest::compare_images(&test_png, &ref_png)?;

        if comparison.matches() {
            Ok(TestResult::Pass)
        } else {
            Ok(TestResult::Fail(format!(
                "Images differ: {:.2}% mismatch",
                comparison.mismatch_percentage() * 100.0
            )))
        }
    }

    /// Run a testharness.js test
    fn run_testharness(&self, test_path: &Path) -> Result<TestResult> {
        // For now, we'll skip testharness.js tests since we don't have
        // JavaScript execution yet. This would require a JS engine.
        Ok(TestResult::Fail("JavaScript tests not yet supported".to_string()))
    }
}

/// Test type
#[derive(Debug)]
enum TestType {
    Reftest { reference: PathBuf },
    Testharness,
    Manual,
    Unknown,
}

/// Test result
#[derive(Debug)]
enum TestResult {
    Pass,
    Fail(String),
}

/// Test expectation
#[derive(Debug, Clone)]
enum TestExpectation {
    Pass,
    Fail,
    Skip,
}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
```

### Step 3: Implement Test Parser (Day 2)

**File: `tests/wpt_runner/parser.rs`**

```rust
//! WPT test parser
//!
//! Parses HTML test files to extract metadata.

use std::path::{Path, PathBuf};
use std::collections::HashMap;
use regex::Regex;

/// Extract reftest reference from HTML
///
/// Looks for: <link rel="match" href="...">
/// or: <link rel="mismatch" href="...">
pub fn extract_reftest_reference(html: &str, test_path: &Path) -> Result<Option<PathBuf>> {
    // Match reftest links
    let re = Regex::new(r#"<link\s+rel="(match|mismatch)"\s+href="([^"]+)""#)?;

    if let Some(captures) = re.captures(html) {
        let rel = &captures[1];
        let href = &captures[2];

        // Currently only support "match" reftests
        if rel == "match" {
            // Resolve reference path relative to test file
            let ref_path = resolve_reference_path(href, test_path)?;
            return Ok(Some(ref_path));
        }
    }

    Ok(None)
}

/// Resolve reference path relative to test file
fn resolve_reference_path(href: &str, test_path: &Path) -> Result<PathBuf> {
    let test_dir = test_path.parent()
        .ok_or("Test file has no parent directory")?;

    // Handle absolute paths (relative to WPT root)
    if href.starts_with('/') {
        // Find WPT root by walking up
        let mut current = test_dir;
        while current.file_name() != Some(std::ffi::OsStr::new("wpt")) {
            current = current.parent()
                .ok_or("Could not find WPT root")?;
        }

        Ok(current.join(&href[1..]))
    } else {
        // Relative path
        Ok(test_dir.join(href))
    }
}

/// Extract test metadata from HTML
pub fn extract_metadata(html: &str) -> TestMetadata {
    let mut metadata = TestMetadata::default();

    // Extract title
    if let Some(title) = extract_tag_content(html, "title") {
        metadata.title = Some(title);
    }

    // Extract meta tags
    let re = Regex::new(r#"<meta\s+name="([^"]+)"\s+content="([^"]+)""#).unwrap();
    for captures in re.captures_iter(html) {
        let name = &captures[1];
        let content = &captures[2];

        match name {
            "assert" => metadata.assert = Some(content.to_string()),
            "flags" => metadata.flags = content.split_whitespace()
                .map(String::from)
                .collect(),
            "timeout" => metadata.timeout = content.parse().ok(),
            _ => {}
        }
    }

    metadata
}

/// Extract content from HTML tag
fn extract_tag_content(html: &str, tag: &str) -> Option<String> {
    let re = Regex::new(&format!(r"<{tag}[^>]*>([^<]+)</{tag}>", tag=tag)).ok()?;
    re.captures(html)
        .and_then(|c| c.get(1))
        .map(|m| m.as_str().trim().to_string())
}

/// Test metadata
#[derive(Debug, Default)]
pub struct TestMetadata {
    pub title: Option<String>,
    pub assert: Option<String>,
    pub flags: Vec<String>,
    pub timeout: Option<u32>,
}

/// Parse expectations file
///
/// Format:
/// ```
/// # Comments start with #
/// css/css-flexbox/test-001.html: FAIL
/// css/css-grid/test-002.html: PASS
/// css/css-tables/test-003.html: SKIP
/// ```
pub fn parse_expectations(content: &str) -> Result<HashMap<String, super::TestExpectation>> {
    let mut expectations = HashMap::new();

    for line in content.lines() {
        let line = line.trim();

        // Skip comments and empty lines
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        // Parse "path: EXPECTATION"
        let parts: Vec<&str> = line.splitn(2, ':').collect();
        if parts.len() != 2 {
            continue;
        }

        let path = parts[0].trim();
        let expectation = parts[1].trim();

        let exp = match expectation {
            "PASS" => super::TestExpectation::Pass,
            "FAIL" => super::TestExpectation::Fail,
            "SKIP" => super::TestExpectation::Skip,
            _ => continue,
        };

        expectations.insert(path.to_string(), exp);
    }

    Ok(expectations)
}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
```

### Step 4: Implement Reftest Comparison (Day 2-3)

**File: `tests/wpt_runner/reftest.rs`**

```rust
//! Reftest comparison
//!
//! Compares rendered images for reftests.

use image::{ImageBuffer, Rgba, RgbaImage};

/// Compare two PNG images
pub fn compare_images(test_png: &[u8], ref_png: &[u8]) -> Result<ImageComparison> {
    // Decode images
    let test_img = image::load_from_memory(test_png)?.to_rgba8();
    let ref_img = image::load_from_memory(ref_png)?.to_rgba8();

    // Check dimensions
    if test_img.dimensions() != ref_img.dimensions() {
        return Err(format!(
            "Image dimensions differ: test {:?} vs ref {:?}",
            test_img.dimensions(),
            ref_img.dimensions()
        ).into());
    }

    let (width, height) = test_img.dimensions();
    let total_pixels = (width * height) as f32;

    // Compare pixel by pixel
    let mut diff_count = 0;
    let mut max_diff = 0u32;
    let mut diff_image = ImageBuffer::new(width, height);

    for y in 0..height {
        for x in 0..width {
            let test_pixel = test_img.get_pixel(x, y);
            let ref_pixel = ref_img.get_pixel(x, y);

            let pixel_diff = pixel_difference(test_pixel, ref_pixel);

            if pixel_diff > PIXEL_TOLERANCE {
                diff_count += 1;
                max_diff = max_diff.max(pixel_diff);

                // Highlight difference in red
                diff_image.put_pixel(x, y, Rgba([255, 0, 0, 255]));
            } else {
                // Show test pixel
                diff_image.put_pixel(x, y, *test_pixel);
            }
        }
    }

    let mismatch_percentage = diff_count as f32 / total_pixels;

    Ok(ImageComparison {
        width,
        height,
        total_pixels: total_pixels as u32,
        diff_pixels: diff_count,
        mismatch_percentage,
        max_diff,
        diff_image,
    })
}

/// Calculate difference between two pixels
///
/// Returns maximum channel difference.
fn pixel_difference(a: &Rgba<u8>, b: &Rgba<u8>) -> u32 {
    let r_diff = (a[0] as i32 - b[0] as i32).unsigned_abs();
    let g_diff = (a[1] as i32 - b[1] as i32).unsigned_abs();
    let b_diff = (a[2] as i32 - b[2] as i32).unsigned_abs();
    let a_diff = (a[3] as i32 - b[3] as i32).unsigned_abs();

    r_diff.max(g_diff).max(b_diff).max(a_diff)
}

/// Tolerance for pixel differences
///
/// Allow small differences due to:
/// - Anti-aliasing differences
/// - Floating-point rounding
/// - Font rendering differences
const PIXEL_TOLERANCE: u32 = 3;

/// Maximum allowed mismatch percentage
///
/// WPT allows up to 0.1% mismatch for reftests
const MISMATCH_THRESHOLD: f32 = 0.001;

/// Image comparison result
#[derive(Debug)]
pub struct ImageComparison {
    pub width: u32,
    pub height: u32,
    pub total_pixels: u32,
    pub diff_pixels: u32,
    pub mismatch_percentage: f32,
    pub max_diff: u32,
    pub diff_image: RgbaImage,
}

impl ImageComparison {
    /// Check if images match (within tolerance)
    pub fn matches(&self) -> bool {
        self.mismatch_percentage <= MISMATCH_THRESHOLD
    }

    /// Save diff image to file
    pub fn save_diff(&self, path: impl AsRef<std::path::Path>) -> Result<()> {
        self.diff_image.save(path)?;
        Ok(())
    }
}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
```

### Step 5: Implement Results Tracking (Day 3)

**File: `tests/wpt_runner/results.rs`**

```rust
//! WPT results tracking and reporting

use std::collections::HashMap;
use std::io::Write;
use serde::{Serialize, Deserialize};

/// WPT test results
#[derive(Debug, Default)]
pub struct WptResults {
    /// Tests that passed
    passed: Vec<String>,

    /// Tests that failed
    failed: Vec<(String, String)>,

    /// Tests that were skipped
    skipped: Vec<(String, String)>,

    /// Tests that had errors
    errors: Vec<(String, String)>,

    /// Expected failures that passed
    unexpected_passes: Vec<String>,

    /// Expected failures that failed
    expected_failures: Vec<(String, String)>,
}

impl WptResults {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn pass(&mut self, test: &str) {
        self.passed.push(test.to_string());
    }

    pub fn fail(&mut self, test: &str, reason: String) {
        self.failed.push((test.to_string(), reason));
    }

    pub fn skip(&mut self, test: &str, reason: &str) {
        self.skipped.push((test.to_string(), reason.to_string()));
    }

    pub fn error(&mut self, test: &str, error: String) {
        self.errors.push((test.to_string(), error));
    }

    pub fn unexpected_pass(&mut self, test: &str) {
        self.unexpected_passes.push(test.to_string());
    }

    pub fn expected_fail(&mut self, test: &str, reason: String) {
        self.expected_failures.push((test.to_string(), reason));
    }

    /// Get total test count
    pub fn total(&self) -> usize {
        self.passed.len() +
        self.failed.len() +
        self.skipped.len() +
        self.errors.len() +
        self.unexpected_passes.len() +
        self.expected_failures.len()
    }

    /// Get pass rate
    pub fn pass_rate(&self) -> f32 {
        let total = self.total();
        if total == 0 {
            return 0.0;
        }

        (self.passed.len() + self.expected_failures.len()) as f32 / total as f32
    }

    /// Print summary
    pub fn print_summary(&self) {
        println!("\n=== WPT Test Results ===");
        println!("Passed:             {}", self.passed.len());
        println!("Failed:             {}", self.failed.len());
        println!("Skipped:            {}", self.skipped.len());
        println!("Errors:             {}", self.errors.len());
        println!("Unexpected passes:  {}", self.unexpected_passes.len());
        println!("Expected failures:  {}", self.expected_failures.len());
        println!("------------------------");
        println!("Total:              {}", self.total());
        println!("Pass rate:          {:.1}%", self.pass_rate() * 100.0);

        if !self.failed.is_empty() {
            println!("\n=== Failures ===");
            for (test, reason) in &self.failed {
                println!("  {} - {}", test, reason);
            }
        }

        if !self.errors.is_empty() {
            println!("\n=== Errors ===");
            for (test, error) in &self.errors {
                println!("  {} - {}", test, error);
            }
        }

        if !self.unexpected_passes.is_empty() {
            println!("\n=== Unexpected Passes ===");
            for test in &self.unexpected_passes {
                println!("  {}", test);
            }
        }
    }

    /// Save results to JSON file
    pub fn save_json(&self, path: impl AsRef<std::path::Path>) -> Result<()> {
        let json_results = JsonResults {
            passed: &self.passed,
            failed: &self.failed,
            skipped: &self.skipped,
            errors: &self.errors,
            unexpected_passes: &self.unexpected_passes,
            expected_failures: &self.expected_failures,
            total: self.total(),
            pass_rate: self.pass_rate(),
        };

        let json = serde_json::to_string_pretty(&json_results)?;
        std::fs::write(path, json)?;

        Ok(())
    }
}

/// JSON results format
#[derive(Serialize)]
struct JsonResults<'a> {
    passed: &'a [String],
    failed: &'a [(String, String)],
    skipped: &'a [(String, String)],
    errors: &'a [(String, String)],
    unexpected_passes: &'a [String],
    expected_failures: &'a [(String, String)],
    total: usize,
    pass_rate: f32,
}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
```

### Step 6: Implement Manifest Parser (Day 4)

**File: `tests/wpt_runner/manifest.rs`**

```rust
//! WPT manifest parser
//!
//! Discovers tests in a directory tree.

use std::path::{Path, PathBuf};
use std::fs;

/// Test manifest
///
/// Lists all tests in a directory.
pub struct Manifest {
    tests: Vec<PathBuf>,
}

impl Manifest {
    /// Get list of tests
    pub fn tests(&self) -> &[PathBuf] {
        &self.tests
    }

    /// Get test count
    pub fn count(&self) -> usize {
        self.tests.len()
    }
}

/// Parse manifest from directory
///
/// Recursively finds all HTML test files.
pub fn parse_manifest(dir: &Path) -> Result<Manifest> {
    let mut tests = Vec::new();

    discover_tests(dir, &mut tests)?;

    // Sort tests for deterministic ordering
    tests.sort();

    Ok(Manifest { tests })
}

/// Recursively discover test files
fn discover_tests(dir: &Path, tests: &mut Vec<PathBuf>) -> Result<()> {
    if !dir.exists() {
        return Err(format!("Directory does not exist: {}", dir.display()).into());
    }

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            // Skip certain directories
            let dir_name = path.file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("");

            if should_skip_directory(dir_name) {
                continue;
            }

            // Recurse into subdirectory
            discover_tests(&path, tests)?;
        } else if is_test_file(&path) {
            tests.push(path);
        }
    }

    Ok(())
}

/// Check if file is a test
fn is_test_file(path: &Path) -> bool {
    // Must be HTML file
    if path.extension().and_then(|s| s.to_str()) != Some("html") {
        return false;
    }

    let file_name = path.file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("");

    // Skip reference files
    if file_name.ends_with("-ref.html") ||
       file_name.ends_with("-notref.html") ||
       file_name.starts_with("ref-") ||
       file_name.starts_with("notref-") {
        return false;
    }

    // Skip support files
    if file_name.starts_with("support-") ||
       file_name.contains("support/") {
        return false;
    }

    true
}

/// Check if directory should be skipped
fn should_skip_directory(name: &str) -> bool {
    matches!(name,
        ".git" |
        "resources" |
        "support" |
        "tools" |
        "common" |
        "_*"  // Directories starting with underscore
    )
}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
```

### Step 7: Create Test Expectations File (Day 4)

**File: `tests/wpt_expectations/expectations.txt`**

```text
# WPT Test Expectations
#
# Format: path/to/test.html: PASS|FAIL|SKIP
#
# This file tracks known failures. As we fix bugs, remove entries.
# As we discover new failures, add entries.

# CSS Flexbox - Initial implementation incomplete
css/css-flexbox/flexbox-align-baseline-001.html: FAIL
css/css-flexbox/flexbox-baseline-001.html: FAIL
css/css-flexbox/flexbox-writing-mode-001.html: FAIL

# CSS Grid - Not yet implemented
css/css-grid/*: SKIP

# CSS Tables - Partial implementation
css/css-tables/border-collapse-001.html: FAIL
css/css-tables/empty-cells-001.html: FAIL

# CSS Inline - Complex features not yet supported
css/css-inline/inline-block-baseline-001.html: FAIL
css/css-inline/vertical-align-sub-001.html: FAIL

# CSS Positioning - Sticky positioning not implemented
css/css-position/sticky-001.html: SKIP
css/css-position/sticky-002.html: SKIP
```

### Step 8: Create Test Runner Binary (Day 5)

**File: `tests/wpt_runner/main.rs`**

```rust
//! WPT test runner binary

use std::path::PathBuf;
use clap::{Parser, Subcommand};

mod parser;
mod executor;
mod reftest;
mod results;
mod manifest;

#[derive(Parser)]
#[clap(name = "wpt-runner")]
#[clap(about = "Run Web Platform Tests for FastRender")]
struct Cli {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a test suite
    Run {
        /// Suite path (e.g., "css/css-flexbox")
        #[clap(value_parser)]
        suite: String,

        /// Path to WPT repository
        #[clap(long, default_value = "tests/wpt")]
        wpt_root: PathBuf,

        /// Path to expectations file
        #[clap(long, default_value = "tests/wpt_expectations/expectations.txt")]
        expectations: PathBuf,

        /// Save results to JSON file
        #[clap(long)]
        output: Option<PathBuf>,
    },

    /// List all tests in a suite
    List {
        /// Suite path
        suite: String,

        /// Path to WPT repository
        #[clap(long, default_value = "tests/wpt")]
        wpt_root: PathBuf,
    },

    /// Update expectations file
    Update {
        /// Suite path
        suite: String,

        /// Path to WPT repository
        #[clap(long, default_value = "tests/wpt")]
        wpt_root: PathBuf,

        /// Path to expectations file
        #[clap(long, default_value = "tests/wpt_expectations/expectations.txt")]
        expectations: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run {
            suite,
            wpt_root,
            expectations,
            output,
        } => {
            // Create runner
            let mut runner = executor::WptRunner::new(&wpt_root);

            // Load expectations
            if expectations.exists() {
                runner.load_expectations(&expectations)?;
            }

            // Run tests
            let results = runner.run_suite(&suite)?;

            // Print summary
            results.print_summary();

            // Save results
            if let Some(output_path) = output {
                results.save_json(&output_path)?;
                println!("\nResults saved to: {}", output_path.display());
            }

            // Exit with error code if tests failed
            if results.pass_rate() < 1.0 {
                std::process::exit(1);
            }
        }

        Commands::List { suite, wpt_root } => {
            let suite_path = wpt_root.join(&suite);
            let manifest = manifest::parse_manifest(&suite_path)?;

            println!("Tests in {}:", suite);
            for test in manifest.tests() {
                println!("  {}", test.display());
            }
            println!("\nTotal: {} tests", manifest.count());
        }

        Commands::Update { suite, wpt_root, expectations } => {
            // Run tests and update expectations
            let mut runner = executor::WptRunner::new(&wpt_root);
            let results = runner.run_suite(&suite)?;

            // TODO: Update expectations file based on results
            println!("Updating expectations...");
            println!("(Not yet implemented)");
        }
    }

    Ok(())
}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
```

### Step 9: CI Integration (Day 6)

**File: `.github/workflows/wpt.yml`**

```yaml
name: WPT Tests

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]
  # Run weekly to catch new WPT tests
  schedule:
    - cron: '0 0 * * 0'

jobs:
  wpt-flexbox:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3
        with:
          submodules: recursive

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          profile: minimal

      - name: Cache cargo registry
        uses: actions/cache@v3
        with:
          path: ~/.cargo/registry
          key: ${{ runner.os }}-cargo-registry-${{ hashFiles('**/Cargo.lock') }}

      - name: Cache cargo index
        uses: actions/cache@v3
        with:
          path: ~/.cargo/git
          key: ${{ runner.os }}-cargo-index-${{ hashFiles('**/Cargo.lock') }}

      - name: Cache target directory
        uses: actions/cache@v3
        with:
          path: target
          key: ${{ runner.os }}-target-${{ hashFiles('**/Cargo.lock') }}

      - name: Install system dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y libfreetype6-dev libfontconfig1-dev

      - name: Run WPT CSS Flexbox tests
        run: |
          cargo run --bin wpt-runner -- run css/css-flexbox \
            --output tests/wpt_results/flexbox.json

      - name: Upload results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: wpt-flexbox-results
          path: tests/wpt_results/flexbox*.json

      - name: Comment PR with results
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const results = JSON.parse(
              fs.readFileSync('tests/wpt_results/flexbox.json', 'utf8')
            );

            const body = `## WPT Flexbox Results

            - ✅ Passed: ${results.passed.length}
            - ❌ Failed: ${results.failed.length}
            - ⏭️ Skipped: ${results.skipped.length}
            - 🔥 Errors: ${results.errors.length}
            - **Pass rate: ${(results.pass_rate * 100).toFixed(1)}%**
            `;

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: body
            });

  wpt-block:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3
        with:
          submodules: recursive

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Run WPT CSS2 Block tests
        run: |
          cargo run --bin wpt-runner -- run css/CSS2/normal-flow \
            --output tests/wpt_results/block.json

      - name: Upload results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: wpt-block-results
          path: tests/wpt_results/block.json

  wpt-inline:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3
        with:
          submodules: recursive

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Run WPT CSS Inline tests
        run: |
          cargo run --bin wpt-runner -- run css/css-inline \
            --output tests/wpt_results/inline.json

      - name: Upload results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: wpt-inline-results
          path: tests/wpt_results/inline.json

```

### Step 10: Progress Tracking Dashboard (Day 7)

**File: `scripts/wpt_dashboard.py`**

```python
#!/usr/bin/env python3
"""
WPT Progress Dashboard

Generates HTML dashboard showing WPT test progress over time.
"""

import json
import glob
from pathlib import Path
from datetime import datetime
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

def load_results():
    """Load all historical WPT results"""
    results = []

    for result_file in sorted(glob.glob('tests/wpt_results/**/*.json', recursive=True)):
        with open(result_file) as f:
            data = json.load(f)

            # Extract timestamp from filename or file modification time
            timestamp = Path(result_file).stat().st_mtime

            results.append({
                'timestamp': timestamp,
                'file': result_file,
                'passed': len(data.get('passed', [])),
                'failed': len(data.get('failed', [])),
                'total': data.get('total', 0),
                'pass_rate': data.get('pass_rate', 0),
            })

    return results

def generate_progress_chart(results, output_path):
    """Generate progress chart over time"""
    if not results:
        return

    dates = [datetime.fromtimestamp(r['timestamp']) for r in results]
    pass_rates = [r['pass_rate'] * 100 for r in results]

    plt.figure(figsize=(12, 6))
    plt.plot(dates, pass_rates, marker='o', linewidth=2, markersize=8)
    plt.xlabel('Date')
    plt.ylabel('Pass Rate (%)')
    plt.title('WPT Test Pass Rate Over Time')
    plt.grid(True, alpha=0.3)
    plt.ylim(0, 100)

    # Format x-axis
    plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
    plt.gca().xaxis.set_major_locator(mdates.DayLocator(interval=7))
    plt.gcf().autofmt_xdate()

    plt.tight_layout()
    plt.savefig(output_path, dpi=150)
    print(f"Progress chart saved to {output_path}")

def generate_html_dashboard(results, output_path):
    """Generate HTML dashboard"""
    latest = results[-1] if results else None

    html = f"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>FastRender WPT Dashboard</title>
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
            .stats {{
                display: grid;
                grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
                gap: 20px;
                margin-bottom: 20px;
            }}
            .stat {{
                background: white;
                padding: 20px;
                border-radius: 8px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }}
            .stat-value {{
                font-size: 36px;
                font-weight: bold;
                color: #333;
            }}
            .stat-label {{
                color: #666;
                font-size: 14px;
                text-transform: uppercase;
                margin-top: 5px;
            }}
            .chart {{
                background: white;
                padding: 20px;
                border-radius: 8px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }}
            .pass {{ color: #22c55e; }}
            .fail {{ color: #ef4444; }}
        </style>
    </head>
    <body>
        <div class="header">
            <h1>FastRender WPT Dashboard</h1>
            <p>Web Platform Tests Progress Tracking</p>
            <p><strong>Last Updated:</strong> {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
        </div>

        <div class="stats">
            <div class="stat">
                <div class="stat-value pass">{latest['passed'] if latest else 0}</div>
                <div class="stat-label">Tests Passed</div>
            </div>
            <div class="stat">
                <div class="stat-value fail">{latest['failed'] if latest else 0}</div>
                <div class="stat-label">Tests Failed</div>
            </div>
            <div class="stat">
                <div class="stat-value">{latest['total'] if latest else 0}</div>
                <div class="stat-label">Total Tests</div>
            </div>
            <div class="stat">
                <div class="stat-value">{(latest['pass_rate'] * 100):.1f}%</div>
                <div class="stat-label">Pass Rate</div>
            </div>
        </div>

        <div class="chart">
            <h2>Progress Over Time</h2>
            <img src="wpt_progress.png" style="width: 100%;">
        </div>
    </body>
    </html>
    """

    with open(output_path, 'w') as f:
        f.write(html)

    print(f"Dashboard saved to {output_path}")

def main():
    results = load_results()

    if not results:
        print("No WPT results found")
        return

    # Generate charts
    generate_progress_chart(results, 'tests/wpt_results/wpt_progress.png')

    # Generate dashboard
    generate_html_dashboard(results, 'tests/wpt_results/dashboard.html')

    print(f"\nProcessed {len(results)} result files")
    print(f"Dashboard: file://{Path('tests/wpt_results/dashboard.html').absolute()}")

if __name__ == '__main__':
    main()
```

## Running WPT Tests

### Quick Start

```bash
# Build the runner
cargo build --bin wpt-runner

# Run CSS Flexbox tests
cargo run --bin wpt-runner -- run css/css-flexbox

# Run with custom expectations
cargo run --bin wpt-runner -- run css/css-flexbox \
    --expectations tests/wpt_expectations/expectations.txt

# Save results
cargo run --bin wpt-runner -- run css/css-flexbox \
    --output results.json

# List all tests in a suite
cargo run --bin wpt-runner -- list css/css-grid
```

### Running Specific Test Suites

```bash
# Block layout tests
cargo run --bin wpt-runner -- run css/CSS2/normal-flow

# Inline layout tests
cargo run --bin wpt-runner -- run css/css-inline

# Flexbox tests
cargo run --bin wpt-runner -- run css/css-flexbox

# Grid tests (when implemented)
cargo run --bin wpt-runner -- run css/css-grid

# Table tests
cargo run --bin wpt-runner -- run css/css-tables

# Positioning tests
cargo run --bin wpt-runner -- run css/css-position

# All CSS tests (warning: very slow!)
cargo run --bin wpt-runner -- run css
```

### Updating Test Expectations

When you fix a bug:

1. Run the tests
2. Remove passing tests from `expectations.txt`
3. Commit the updated expectations

When you discover a new failure:

1. Add the failing test to `expectations.txt`
2. Mark it as `FAIL` with a comment explaining why
3. Create an issue to track fixing it

## Acceptance Criteria

- [ ] WPT submodule is added and initialized
- [ ] Test runner can discover tests in a directory
- [ ] Reftests execute and compare images
- [ ] Test results are tracked (pass/fail/skip/error)
- [ ] Expectations file is loaded and respected
- [ ] Results can be saved to JSON
- [ ] CI runs WPT tests on every commit
- [ ] Progress dashboard shows improvement over time
- [ ] At least 50% of CSS Flexbox WPT tests pass
- [ ] At least 70% of CSS2 block layout WPT tests pass
- [ ] Documentation explains how to run WPT tests
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Not Handling Reference Paths Correctly

**Wrong:**
```rust
// Assuming reference is in same directory
let ref_path = test_path.with_file_name(reference);
// Fails for references in other directories!
```

**Right:**
```rust
// Resolve relative to test file
let ref_path = resolve_reference_path(reference, test_path)?;
// Handles ../ref.html, /common/ref.html, etc.
```

### Pitfall 2: Pixel-Perfect Comparison

**Wrong:**
```rust
// Require exact match
if test_png == ref_png {
    TestResult::Pass
}
// Fails due to anti-aliasing!
```

**Right:**
```rust
// Allow small tolerance
let diff = compare_images(test_png, ref_png)?;
if diff.mismatch_percentage <= 0.001 {
    TestResult::Pass
}
```

### Pitfall 3: Not Skipping Reference Files

**Wrong:**
```rust
// Running *-ref.html as tests
for file in html_files {
    run_test(file);
}
// Reference files aren't tests!
```

**Right:**
```rust
// Skip reference files
if !file.ends_with("-ref.html") &&
   !file.ends_with("-notref.html") {
    run_test(file);
}
```

### Pitfall 4: Ignoring Test Metadata

**Wrong:**
```rust
// Running all tests the same way
run_test(test_path);
// Some tests need special handling!
```

**Right:**
```rust
// Parse metadata
let metadata = extract_metadata(&html);

// Handle flags
if metadata.flags.contains("may-gc") {
    // Allow garbage collection
}
```

## Performance Considerations

1. **Parallel test execution** - Run tests in parallel when possible
2. **Caching rendered images** - Don't re-render if test hasn't changed
3. **Sparse WPT checkout** - Only download needed test directories
4. **Incremental results** - Save results after each test
5. **Fast-fail mode** - Stop on first failure during development

## Next Steps

After WPT integration:
- **06-reference-tests.md** - Create FastRender-specific reference tests
- **06-benchmarking.md** - Performance benchmarking
- Fix failing tests systematically
- Track progress toward 100% pass rate

## References

- **Web Platform Tests:** https://web-platform-tests.org/
- **WPT GitHub:** https://github.com/web-platform-tests/wpt
- **Writing Reftests:** https://web-platform-tests.org/writing-tests/reftests.html
- **testharness.js API:** https://web-platform-tests.org/writing-tests/testharness-api.html
- **Servo WPT Integration:** https://github.com/servo/servo/tree/main/tests/wpt
- **Chromium WPT Integration:** https://chromium.googlesource.com/chromium/src/+/main/docs/testing/web_platform_tests.md

---

**Last Updated:** 2025-11-19
**Status:** Ready for Implementation
