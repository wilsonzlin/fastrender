# Web Platform Tests (WPT) Runner for FastRender

This module provides infrastructure for running Web Platform Tests against the FastRender HTML/CSS rendering engine.

## Overview

[Web Platform Tests (WPT)](https://web-platform-tests.org/) are a cross-browser test suite for web platform features. This runner executes a subset of these tests that are relevant to a rendering engine focused on HTML/CSS to image conversion.

## Quick Start

### Running Tests Programmatically

```rust
use fastrender::Renderer;
use wpt::{WptRunner, HarnessConfig};
use std::path::Path;

fn main() {
    // Create runner with default configuration
    let renderer = Renderer::new();
    let mut runner = WptRunner::new(renderer);

    // Run a single test
    let result = runner.run_test(Path::new("tests/wpt/css/box-model/margin-001.html"));
    println!("Test {}: {}", result.metadata.id, result.status);

    // Run all tests in a directory
    let suite = runner.run_suite_aggregated(Path::new("tests/wpt/css/box-model/"));
    println!("{}", suite);
}
```

### Running Tests via Cargo

```bash
# Run all WPT tests
cargo test wpt

# Run specific WPT tests
cargo test wpt::integration_tests

# Run with output
cargo test wpt -- --nocapture
```

## Test Types

The runner supports several types of tests:

### Reference Tests (reftests)

Compare rendered output of a test file against a reference HTML file that should produce identical output.

**Structure:**
```
test-001.html      # Test file
test-001-ref.html  # Reference file that should render identically
```

### Visual Tests

Compare rendered output against expected PNG images.

**Structure:**
```
tests/wpt/css/test.html           # Test file
tests/wpt/expected/css/test.png   # Expected image
```

### Crash Tests

Verify that rendering specific content doesn't cause a crash.

**Structure:**
```
crashtest-001.html  # Files containing "crash" in the name
```

## Directory Structure

```
tests/wpt/
├── css/
│   ├── CSS2/
│   │   ├── box-model/
│   │   │   ├── margin-001.html           # Test file
│   │   │   ├── margin-001-ref.html       # Reference for reftest
│   │   │   └── padding-001.html
│   │   └── visual-formatting/
│   │       └── ...
│   └── css-backgrounds/
│       └── ...
├── expected/
│   └── css/
│       └── CSS2/
│           └── box-model/
│               └── padding-001.png       # Expected image
├── harness.rs                            # Test harness infrastructure
├── runner.rs                             # WptRunner implementation
├── mod.rs                                # Module exports
└── README.md                             # This file
```

## Configuration

### HarnessConfig Options

| Option | Default | Description |
|--------|---------|-------------|
| `test_dir` | `tests/wpt` | Base directory for test files |
| `expected_dir` | `tests/wpt/expected` | Directory for expected images |
| `output_dir` | `target/wpt-output` | Directory for test artifacts |
| `save_rendered` | `true` | Save rendered images |
| `save_diffs` | `true` | Save diff images on failure |
| `pixel_tolerance` | `0` | Per-channel pixel tolerance (0-255) |
| `max_diff_percentage` | `0.0` | Maximum allowed difference percentage |
| `default_timeout_ms` | `30000` | Test timeout in milliseconds |
| `fail_fast` | `false` | Stop on first failure |
| `update_expected` | `false` | Update expected images on failure |
| `filter` | `None` | Filter tests by name pattern |

### Example Configuration

```rust
use wpt::{WptRunner, WptRunnerBuilder, HarnessConfig};

// Using builder pattern
let runner = WptRunnerBuilder::new()
    .renderer(Renderer::new())
    .test_dir("custom/tests")
    .expected_dir("custom/expected")
    .output_dir("target/wpt-output")
    .tolerance(5)           // Allow 5 units difference per channel
    .max_diff(0.1)          // Allow 0.1% pixel difference
    .fail_fast()            // Stop on first failure
    .filter("box-model")    // Only run box-model tests
    .save_rendered()        // Save rendered images
    .save_diffs()           // Save diff images
    .build();

// Or using config directly
let config = HarnessConfig::with_test_dir("custom/tests")
    .with_tolerance(5)
    .with_max_diff(0.1)
    .fail_fast();
let runner = WptRunner::with_config(Renderer::new(), config);
```

## Test Results

### TestStatus Values

| Status | Description |
|--------|-------------|
| `Pass` | Test passed |
| `Fail` | Test failed (rendering mismatch) |
| `Error` | Test encountered an error |
| `Skip` | Test was skipped |
| `Timeout` | Test timed out |

### Result Information

Each `TestResult` contains:

- `metadata`: Test ID, path, type, configuration
- `status`: Pass/Fail/Error/Skip/Timeout
- `duration`: Execution time
- `message`: Error message (if applicable)
- `rendered_image`: Rendered PNG bytes (if saved)
- `expected_image`: Expected PNG bytes (if available)
- `pixel_diff`: Number of different pixels
- `diff_percentage`: Percentage of different pixels

### Suite Results

```rust
let suite = runner.run_suite_aggregated(Path::new("tests/wpt/css/"));

println!("Total:   {}", suite.total());
println!("Passed:  {}", suite.passed());
println!("Failed:  {}", suite.failed());
println!("Errors:  {}", suite.errors());
println!("Skipped: {}", suite.skipped());
println!("Pass Rate: {:.1}%", suite.pass_rate());
println!("Duration: {:?}", suite.duration);
```

## Image Comparison

The harness provides pixel-level image comparison:

```rust
use wpt::harness::compare_images;

let (diff_pixels, total_pixels, diff_percentage) = compare_images(
    &rendered_png,
    &expected_png,
    5,  // tolerance per channel
)?;

println!("Different pixels: {}/{}", diff_pixels, total_pixels);
println!("Difference: {:.2}%", diff_percentage);
```

### Tolerance

- `pixel_tolerance`: Maximum difference allowed per color channel (0-255)
  - `0`: Exact match required
  - `5`: Allow slight variations (antialiasing differences)
  - `10+`: Allow larger variations

- `max_diff_percentage`: Maximum percentage of pixels that can differ
  - `0.0`: No pixels can differ
  - `0.1`: Up to 0.1% of pixels can differ
  - `1.0`: Up to 1% of pixels can differ

## CI Integration

### Fail-Fast Mode

Stop testing on first failure:

```rust
let runner = WptRunnerBuilder::new()
    .renderer(Renderer::new())
    .fail_fast()
    .build();
```

### Filtering

Run subset of tests:

```rust
let runner = WptRunnerBuilder::new()
    .renderer(Renderer::new())
    .filter("box-model")  // Only tests containing "box-model"
    .build();
```

### Saving Artifacts

```rust
let runner = WptRunnerBuilder::new()
    .renderer(Renderer::new())
    .output_dir("target/wpt-artifacts")
    .save_rendered()  // Save rendered images
    .save_diffs()     // Save diff images on failure
    .build();
```

### Updating Expected Images

When adding new tests or intentionally changing rendering:

```rust
let runner = WptRunnerBuilder::new()
    .renderer(Renderer::new())
    .update_expected()  // Update expected images
    .build();
```

## Adding New Tests

### Reference Test

1. Create test HTML file:
```html
<!-- margin-001.html -->
<!DOCTYPE html>
<html>
<head>
    <title>Margin Test</title>
    <style>
        .box { margin: 10px; background: blue; width: 100px; height: 100px; }
    </style>
</head>
<body>
    <div class="box"></div>
</body>
</html>
```

2. Create reference HTML file:
```html
<!-- margin-001-ref.html -->
<!DOCTYPE html>
<html>
<head>
    <title>Margin Reference</title>
    <style>
        .box { padding: 10px; background: blue; width: 100px; height: 100px; }
    </style>
</head>
<body>
    <div class="box"></div>
</body>
</html>
```

### Visual Test

1. Create test HTML file in `tests/wpt/css/`
2. Generate expected image:
```rust
let runner = WptRunnerBuilder::new()
    .renderer(Renderer::new())
    .update_expected()
    .build();
runner.run_test(Path::new("tests/wpt/css/new-test.html"));
```
3. Verify the generated image is correct
4. Commit both the test file and expected image

## API Reference

### WptRunner

Main runner struct:

```rust
impl WptRunner {
    // Creation
    fn new(renderer: Renderer) -> Self;
    fn with_config(renderer: Renderer, config: HarnessConfig) -> Self;

    // Configuration
    fn config(&self) -> &HarnessConfig;
    fn config_mut(&mut self) -> &mut HarnessConfig;

    // Statistics
    fn stats(&self) -> &RunnerStats;
    fn reset_stats(&mut self);

    // Execution
    fn run_test(&mut self, path: &Path) -> TestResult;
    fn run_suite(&mut self, dir: &Path) -> Vec<TestResult>;
    fn run_suite_aggregated(&mut self, dir: &Path) -> SuiteResult;
}
```

### WptRunnerBuilder

Builder for fluent construction:

```rust
WptRunnerBuilder::new()
    .renderer(renderer)
    .test_dir("path")
    .expected_dir("path")
    .output_dir("path")
    .tolerance(5)
    .max_diff(0.1)
    .fail_fast()
    .filter("pattern")
    .save_rendered()
    .save_diffs()
    .update_expected()
    .build()
```

### HarnessConfig

Configuration struct:

```rust
HarnessConfig::default()
HarnessConfig::with_test_dir("path")
    .with_tolerance(5)
    .with_max_diff(0.1)
    .fail_fast()
    .parallel(4)
    .with_filter("pattern")
    .update_expected()
```

## Troubleshooting

### Test Not Found

Ensure the test file:
- Has `.html` or `.htm` extension
- Is not named with `-ref` suffix (reference files are not tests)
- Is not in a `support` directory

### Image Mismatch

1. Check `target/wpt-output/` for rendered images
2. Compare visually with expected images
3. If rendering is correct, update expected with `update_expected()`
4. If rendering is wrong, fix the rendering engine

### Reference File Not Found

For reftests, ensure:
- Reference file exists with `-ref.html` suffix
- Reference file is in the same directory as test

## Performance Tips

1. **Filter tests** during development to run subset
2. **Use fail-fast** to stop early on failures
3. **Disable artifact saving** for faster runs:
   ```rust
   config.save_rendered = false;
   config.save_diffs = false;
   ```

## Future Improvements

- Parallel test execution
- Testharness.js support
- Fuzzy image comparison algorithms
- Test result caching
- HTML report generation

## References

- [Web Platform Tests](https://web-platform-tests.org/)
- [WPT Documentation](https://web-platform-tests.org/writing-tests/)
- [Reftest Documentation](https://web-platform-tests.org/writing-tests/reftests.html)
- [CSS WG Test Suite](https://test.csswg.org/suites/)
