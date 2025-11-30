//! Reference testing framework for FastRender
//!
//! This module provides a comprehensive reference testing framework for validating
//! rendered output against expected reference images. It's designed to catch visual
//! regressions in the rendering engine by comparing pixel-by-pixel output.
//!
//! # Overview
//!
//! The reference testing framework consists of two main components:
//!
//! - **`compare`**: Image comparison utilities with configurable tolerance
//! - **`harness`**: Test harness for running reference tests
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    Reference Test Harness                     │
//! ├─────────────────────────────────────────────────────────────┤
//! │                                                               │
//! │  ┌─────────────┐      ┌─────────────┐      ┌─────────────┐ │
//! │  │  Test Files │ ───► │   Renderer  │ ───► │   Pixmap    │ │
//! │  │ (HTML/CSS)  │      │ (FastRender)│      │  (Actual)   │ │
//! │  └─────────────┘      └─────────────┘      └──────┬──────┘ │
//! │                                                    │        │
//! │  ┌─────────────┐                           ┌──────▼──────┐ │
//! │  │  Reference  │ ─────────────────────────►│   Compare   │ │
//! │  │    Image    │                           │   Module    │ │
//! │  └─────────────┘                           └──────┬──────┘ │
//! │                                                    │        │
//! │                                             ┌──────▼──────┐ │
//! │                                             │  ImageDiff  │ │
//! │                                             │  (Result)   │ │
//! │                                             └─────────────┘ │
//! │                                                               │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! # Quick Start
//!
//! ## Running a Single Reference Test
//!
//! ```rust,ignore
//! use tests::ref::{RefTestHarness, RefTestConfig};
//! use std::path::Path;
//!
//! let harness = RefTestHarness::new();
//!
//! let result = harness.run_ref_test(
//!     Path::new("tests/ref/fixtures/simple_box"),
//!     Path::new("tests/ref/fixtures/simple_box/reference.png"),
//! );
//!
//! assert!(result.passed, "Test failed: {}", result.summary());
//! ```
//!
//! ## Running All Reference Tests
//!
//! ```rust,ignore
//! use tests::ref::{RefTestHarness, RefTestConfig};
//! use std::path::Path;
//!
//! let harness = RefTestHarness::new();
//! let results = harness.run_all_tests(Path::new("tests/ref/fixtures"))?;
//!
//! println!("{}", results.summary());
//! assert!(results.all_passed());
//! ```
//!
//! ## Comparing Images Directly
//!
//! ```rust,ignore
//! use tests::ref::{compare_images, CompareConfig, load_png};
//! use std::path::Path;
//!
//! let actual = load_png(Path::new("actual.png"))?;
//! let expected = load_png(Path::new("expected.png"))?;
//!
//! let diff = compare_images(&actual, &expected, &CompareConfig::lenient());
//!
//! if !diff.is_match() {
//!     println!("Images differ: {}", diff.summary());
//!     diff.save_diff_image(Path::new("diff.png"))?;
//! }
//! ```
//!
//! # Test File Convention
//!
//! Reference tests follow a standard directory structure:
//!
//! ```text
//! tests/ref/fixtures/
//! ├── test_name/
//! │   ├── input.html       # Required: HTML to render
//! │   ├── style.css        # Optional: CSS styles
//! │   ├── reference.png    # Required: Expected output
//! │   └── config.toml      # Optional: Test configuration
//! ```
//!
//! # Comparison Modes
//!
//! The framework supports different comparison strictness levels:
//!
//! - **Strict**: Exact pixel match (default)
//! - **Lenient**: Allows minor anti-aliasing differences (5 tolerance, 0.1% different)
//! - **Fuzzy**: For testing general layout (10 tolerance, 1% different)
//!
//! # Creating Reference Images
//!
//! To create initial reference images for new tests:
//!
//! ```rust,ignore
//! let harness = RefTestHarness::new();
//!
//! let html = r#"
//!     <html>
//!         <body>
//!             <div style="width: 100px; height: 100px; background: red;"></div>
//!         </body>
//!     </html>
//! "#;
//!
//! harness.create_reference(html, Path::new("tests/ref/fixtures/red_box/reference.png"))?;
//! ```
//!
//! # Failure Artifacts
//!
//! When a test fails, the harness can save:
//!
//! - **Actual output**: The rendered image that was produced
//! - **Diff image**: Visual representation of differences (red = different pixels)
//!
//! These artifacts are saved to a configurable directory (default: `test_dir/failures/`).
//!
//! # Performance
//!
//! - Image comparison is O(width × height)
//! - PSNR and MSE calculations are included for quality metrics
//! - Diff image generation can be disabled for faster comparisons
//!
//! # Module Organization
//!
//! - `compare` - Image comparison utilities
//!   - `CompareConfig` - Configuration for comparison strictness
//!   - `ImageDiff` - Result of comparison with statistics
//!   - `DiffStatistics` - Detailed difference metrics
//!   - `compare_images()` - Main comparison function
//!   - `load_png()` / `save_png()` - PNG I/O helpers
//!
//! - `harness` - Test harness
//!   - `RefTestHarness` - Main test runner
//!   - `RefTestConfig` - Test configuration
//!   - `RefTestResult` - Single test result
//!   - `RefTestResults` - Batch test results

pub mod compare;
pub mod harness;

// Re-export main types for convenience
pub use compare::{compare_images, create_solid_pixmap, load_png, save_png, CompareConfig, DiffStatistics, ImageDiff};
pub use harness::{RefTestConfig, RefTestHarness, RefTestResult, RefTestResults};
