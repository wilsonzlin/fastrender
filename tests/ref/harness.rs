//! Reference test harness for visual regression testing
//!
//! This module provides a comprehensive test harness for running reference tests
//! that compare rendered HTML/CSS output against expected reference images.
//! It's essential for catching visual regressions in the rendering engine.
//!
//! # Overview
//!
//! The reference test harness:
//! - Loads HTML test files and optional CSS
//! - Renders them using FastRender
//! - Compares output against reference images
//! - Reports detailed differences
//! - Supports test discovery and batch execution
//!
//! # Test File Convention
//!
//! Reference tests follow this directory structure:
//! ```text
//! tests/ref/fixtures/
//! ├── test_name/
//! │   ├── input.html       # Required: HTML to render
//! │   ├── style.css        # Optional: CSS styles
//! │   ├── reference.png    # Required: Expected output
//! │   └── config.toml      # Optional: Test configuration
//! ```
//!
//! # Example
//!
//! ```rust,ignore
//! use ref::harness::{RefTestHarness, RefTestConfig};
//!
//! let harness = RefTestHarness::new();
//!
//! // Run a single test
//! let result = harness.run_ref_test(
//!     Path::new("tests/ref/fixtures/simple_box"),
//!     Path::new("tests/ref/fixtures/simple_box/reference.png"),
//! )?;
//!
//! assert!(result.passed, "Test failed: {}", result.summary());
//!
//! // Run all tests in a directory
//! let results = harness.run_all_tests(Path::new("tests/ref/fixtures"))?;
//! assert!(results.all_passed(), "Some tests failed");
//! ```

use super::compare::{compare_images, CompareConfig, ImageDiff};
use fastrender::Renderer;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use tiny_skia::Pixmap;

/// Configuration for a reference test
#[derive(Debug, Clone)]
pub struct RefTestConfig {
    /// Viewport width for rendering
    pub viewport_width: u32,

    /// Viewport height for rendering
    pub viewport_height: u32,

    /// Image comparison configuration
    pub compare_config: CompareConfig,

    /// Whether to save the actual rendered output on failure
    pub save_actual_on_failure: bool,

    /// Whether to save the diff image on failure
    pub save_diff_on_failure: bool,

    /// Output directory for failure artifacts
    pub failure_output_dir: Option<PathBuf>,

    /// Timeout for rendering (in seconds)
    pub render_timeout_secs: u64,
}

impl Default for RefTestConfig {
    fn default() -> Self {
        Self {
            viewport_width: 800,
            viewport_height: 600,
            compare_config: CompareConfig::strict(),
            save_actual_on_failure: true,
            save_diff_on_failure: true,
            failure_output_dir: None,
            render_timeout_secs: 30,
        }
    }
}

impl RefTestConfig {
    /// Creates a new config with specified viewport dimensions
    pub fn with_viewport(width: u32, height: u32) -> Self {
        Self {
            viewport_width: width,
            viewport_height: height,
            ..Default::default()
        }
    }

    /// Sets the comparison config
    pub fn with_compare_config(mut self, config: CompareConfig) -> Self {
        self.compare_config = config;
        self
    }

    /// Sets whether to save actual output on failure
    pub fn with_save_actual_on_failure(mut self, save: bool) -> Self {
        self.save_actual_on_failure = save;
        self
    }

    /// Sets whether to save diff image on failure
    pub fn with_save_diff_on_failure(mut self, save: bool) -> Self {
        self.save_diff_on_failure = save;
        self
    }

    /// Sets the failure output directory
    pub fn with_failure_output_dir<P: AsRef<Path>>(mut self, dir: P) -> Self {
        self.failure_output_dir = Some(dir.as_ref().to_path_buf());
        self
    }
}

/// Result of running a reference test
#[derive(Debug)]
pub struct RefTestResult {
    /// Name of the test
    pub test_name: String,

    /// Whether the test passed
    pub passed: bool,

    /// Detailed image difference information
    pub image_diff: Option<ImageDiff>,

    /// Error message if test failed due to error (not visual difference)
    pub error: Option<String>,

    /// Time taken to render
    pub render_time: Duration,

    /// Time taken to compare
    pub compare_time: Duration,

    /// Path to actual output (if saved)
    pub actual_output_path: Option<PathBuf>,

    /// Path to diff image (if saved)
    pub diff_output_path: Option<PathBuf>,
}

impl RefTestResult {
    /// Creates a passed result
    fn passed(test_name: String, image_diff: ImageDiff, render_time: Duration, compare_time: Duration) -> Self {
        Self {
            test_name,
            passed: true,
            image_diff: Some(image_diff),
            error: None,
            render_time,
            compare_time,
            actual_output_path: None,
            diff_output_path: None,
        }
    }

    /// Creates a failed result due to image mismatch
    fn failed_mismatch(
        test_name: String,
        image_diff: ImageDiff,
        render_time: Duration,
        compare_time: Duration,
    ) -> Self {
        Self {
            test_name,
            passed: false,
            image_diff: Some(image_diff),
            error: None,
            render_time,
            compare_time,
            actual_output_path: None,
            diff_output_path: None,
        }
    }

    /// Creates a failed result due to an error
    fn failed_error(test_name: String, error: String) -> Self {
        Self {
            test_name,
            passed: false,
            image_diff: None,
            error: Some(error),
            render_time: Duration::ZERO,
            compare_time: Duration::ZERO,
            actual_output_path: None,
            diff_output_path: None,
        }
    }

    /// Returns a human-readable summary of the test result
    pub fn summary(&self) -> String {
        if self.passed {
            format!(
                "PASS: {} (render: {:?}, compare: {:?})",
                self.test_name, self.render_time, self.compare_time
            )
        } else if let Some(ref error) = self.error {
            format!("FAIL: {} - Error: {}", self.test_name, error)
        } else if let Some(ref diff) = self.image_diff {
            format!("FAIL: {} - {}", self.test_name, diff.summary())
        } else {
            format!("FAIL: {} - Unknown error", self.test_name)
        }
    }
}

/// Results from running multiple reference tests
#[derive(Debug)]
pub struct RefTestResults {
    /// Individual test results
    pub results: Vec<RefTestResult>,

    /// Total time for all tests
    pub total_time: Duration,
}

impl RefTestResults {
    /// Returns true if all tests passed
    pub fn all_passed(&self) -> bool {
        self.results.iter().all(|r| r.passed)
    }

    /// Returns the number of passed tests
    pub fn passed_count(&self) -> usize {
        self.results.iter().filter(|r| r.passed).count()
    }

    /// Returns the number of failed tests
    pub fn failed_count(&self) -> usize {
        self.results.iter().filter(|r| !r.passed).count()
    }

    /// Returns a human-readable summary
    pub fn summary(&self) -> String {
        let passed = self.passed_count();
        let failed = self.failed_count();
        let total = self.results.len();

        let mut summary = format!(
            "Reference Tests: {}/{} passed, {} failed (total time: {:?})\n",
            passed, total, failed, self.total_time
        );

        for result in &self.results {
            summary.push_str(&format!("  {}\n", result.summary()));
        }

        summary
    }

    /// Returns an iterator over failed tests
    pub fn failed_tests(&self) -> impl Iterator<Item = &RefTestResult> {
        self.results.iter().filter(|r| !r.passed)
    }
}

/// Main reference test harness
///
/// Manages the FastRender renderer and provides methods for running
/// reference tests against expected images.
pub struct RefTestHarness {
    renderer: Renderer,
    config: RefTestConfig,
}

impl RefTestHarness {
    /// Creates a new reference test harness with default configuration
    pub fn new() -> Self {
        Self {
            renderer: Renderer::new(),
            config: RefTestConfig::default(),
        }
    }

    /// Creates a new harness with custom configuration
    pub fn with_config(config: RefTestConfig) -> Self {
        Self {
            renderer: Renderer::new(),
            config,
        }
    }

    /// Returns a reference to the configuration
    pub fn config(&self) -> &RefTestConfig {
        &self.config
    }

    /// Returns a mutable reference to the configuration
    pub fn config_mut(&mut self) -> &mut RefTestConfig {
        &mut self.config
    }

    /// Runs a reference test comparing rendered output against a reference image
    ///
    /// # Arguments
    /// * `test_dir` - Directory containing test files (input.html, style.css, etc.)
    /// * `reference_path` - Path to the reference PNG image
    ///
    /// # Returns
    /// `RefTestResult` with detailed information about the test outcome
    pub fn run_ref_test(&self, test_dir: &Path, reference_path: &Path) -> RefTestResult {
        let test_name = test_dir
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_else(|| "unknown".to_string());

        // Load test files
        let html_path = test_dir.join("input.html");
        let css_path = test_dir.join("style.css");

        let html = match fs::read_to_string(&html_path) {
            Ok(content) => content,
            Err(e) => {
                return RefTestResult::failed_error(
                    test_name,
                    format!("Failed to read HTML file '{}': {}", html_path.display(), e),
                );
            }
        };

        // CSS is optional
        let css = fs::read_to_string(&css_path).unwrap_or_default();

        // Combine HTML and CSS if CSS is separate
        let full_html = if !css.is_empty() && !html.contains("<style>") {
            format!(
                r#"<!DOCTYPE html>
<html>
<head>
<style>
{}
</style>
</head>
<body>
{}
</body>
</html>"#,
                css,
                extract_body_content(&html)
            )
        } else {
            html
        };

        // Load reference image
        let expected = match super::compare::load_png(reference_path) {
            Ok(pixmap) => pixmap,
            Err(e) => {
                return RefTestResult::failed_error(test_name, format!("Failed to load reference image: {}", e));
            }
        };

        // Render the test
        let render_start = Instant::now();
        let actual = match self.render_html(&full_html) {
            Ok(pixmap) => pixmap,
            Err(e) => {
                return RefTestResult::failed_error(test_name, format!("Rendering failed: {}", e));
            }
        };
        let render_time = render_start.elapsed();

        // Compare images
        let compare_start = Instant::now();
        let image_diff = compare_images(&actual, &expected, &self.config.compare_config);
        let compare_time = compare_start.elapsed();

        // Create result
        let mut result = if image_diff.is_match() {
            RefTestResult::passed(test_name.clone(), image_diff, render_time, compare_time)
        } else {
            RefTestResult::failed_mismatch(test_name.clone(), image_diff, render_time, compare_time)
        };

        // Save failure artifacts if configured
        if !result.passed {
            self.save_failure_artifacts(&mut result, &actual, test_dir);
        }

        result
    }

    /// Runs a reference test with inline HTML content
    ///
    /// # Arguments
    /// * `test_name` - Name for the test
    /// * `html` - HTML content to render
    /// * `reference_path` - Path to the reference PNG image
    ///
    /// # Returns
    /// `RefTestResult` with detailed information about the test outcome
    pub fn run_ref_test_inline(&self, test_name: &str, html: &str, reference_path: &Path) -> RefTestResult {
        // Load reference image
        let expected = match super::compare::load_png(reference_path) {
            Ok(pixmap) => pixmap,
            Err(e) => {
                return RefTestResult::failed_error(
                    test_name.to_string(),
                    format!("Failed to load reference image: {}", e),
                );
            }
        };

        // Render the test
        let render_start = Instant::now();
        let actual = match self.render_html(html) {
            Ok(pixmap) => pixmap,
            Err(e) => {
                return RefTestResult::failed_error(test_name.to_string(), format!("Rendering failed: {}", e));
            }
        };
        let render_time = render_start.elapsed();

        // Compare images
        let compare_start = Instant::now();
        let image_diff = compare_images(&actual, &expected, &self.config.compare_config);
        let compare_time = compare_start.elapsed();

        if image_diff.is_match() {
            RefTestResult::passed(test_name.to_string(), image_diff, render_time, compare_time)
        } else {
            RefTestResult::failed_mismatch(test_name.to_string(), image_diff, render_time, compare_time)
        }
    }

    /// Compares two pixmaps directly
    ///
    /// # Arguments
    /// * `actual` - The rendered image
    /// * `expected` - The reference image
    ///
    /// # Returns
    /// `ImageDiff` with comparison results
    pub fn compare_images(&self, actual: &Pixmap, expected: &Pixmap) -> ImageDiff {
        compare_images(actual, expected, &self.config.compare_config)
    }

    /// Runs all reference tests in a directory
    ///
    /// Discovers tests by looking for directories containing `input.html`
    /// and `reference.png` files.
    ///
    /// # Arguments
    /// * `test_root` - Root directory containing test directories
    ///
    /// # Returns
    /// `RefTestResults` with results for all discovered tests
    pub fn run_all_tests(&self, test_root: &Path) -> Result<RefTestResults, String> {
        let total_start = Instant::now();
        let mut results = Vec::new();

        // Discover test directories
        let test_dirs = self.discover_tests(test_root)?;

        for test_dir in test_dirs {
            let reference_path = test_dir.join("reference.png");

            if reference_path.exists() {
                let result = self.run_ref_test(&test_dir, &reference_path);
                results.push(result);
            }
        }

        Ok(RefTestResults {
            results,
            total_time: total_start.elapsed(),
        })
    }

    /// Discovers test directories in the given root
    fn discover_tests(&self, root: &Path) -> Result<Vec<PathBuf>, String> {
        let mut test_dirs = Vec::new();

        if !root.exists() {
            return Err(format!("Test root directory does not exist: {}", root.display()));
        }

        let entries =
            fs::read_dir(root).map_err(|e| format!("Failed to read directory '{}': {}", root.display(), e))?;

        for entry in entries {
            let entry = entry.map_err(|e| format!("Failed to read directory entry: {}", e))?;
            let path = entry.path();

            if path.is_dir() {
                let html_path = path.join("input.html");
                if html_path.exists() {
                    test_dirs.push(path);
                }
            }
        }

        test_dirs.sort();
        Ok(test_dirs)
    }

    /// Renders HTML to a Pixmap
    fn render_html(&self, html: &str) -> Result<Pixmap, String> {
        let result = self
            .renderer
            .render_to_png(html, self.config.viewport_width, self.config.viewport_height);

        match result {
            Ok(png_data) => {
                // Decode PNG to Pixmap
                png_to_pixmap(&png_data)
            }
            Err(e) => Err(format!("Rendering failed: {:?}", e)),
        }
    }

    /// Saves failure artifacts (actual image, diff image)
    fn save_failure_artifacts(&self, result: &mut RefTestResult, actual: &Pixmap, test_dir: &Path) {
        let output_dir = self
            .config
            .failure_output_dir
            .clone()
            .unwrap_or_else(|| test_dir.join("failures"));

        // Create output directory if needed
        if let Err(e) = fs::create_dir_all(&output_dir) {
            eprintln!("Warning: Failed to create failure output directory: {}", e);
            return;
        }

        // Save actual output
        if self.config.save_actual_on_failure {
            let actual_path = output_dir.join(format!("{}_actual.png", result.test_name));
            if let Err(e) = actual.save_png(&actual_path) {
                eprintln!("Warning: Failed to save actual output: {}", e);
            } else {
                result.actual_output_path = Some(actual_path);
            }
        }

        // Save diff image
        if self.config.save_diff_on_failure {
            if let Some(ref image_diff) = result.image_diff {
                let diff_path = output_dir.join(format!("{}_diff.png", result.test_name));
                if let Err(e) = image_diff.save_diff_image(&diff_path) {
                    eprintln!("Warning: Failed to save diff image: {}", e);
                } else {
                    result.diff_output_path = Some(diff_path);
                }
            }
        }
    }

    /// Creates a reference image from HTML
    ///
    /// Useful for generating initial reference images for new tests.
    ///
    /// # Arguments
    /// * `html` - HTML content to render
    /// * `output_path` - Path to save the reference image
    ///
    /// # Returns
    /// `Ok(())` if saved successfully, `Err` with message otherwise
    pub fn create_reference(&self, html: &str, output_path: &Path) -> Result<(), String> {
        let pixmap = self.render_html(html)?;
        pixmap
            .save_png(output_path)
            .map_err(|e| format!("Failed to save reference image: {}", e))
    }

    /// Updates a reference image for an existing test
    ///
    /// # Arguments
    /// * `test_dir` - Directory containing the test
    ///
    /// # Returns
    /// `Ok(())` if updated successfully, `Err` with message otherwise
    pub fn update_reference(&self, test_dir: &Path) -> Result<(), String> {
        let html_path = test_dir.join("input.html");
        let css_path = test_dir.join("style.css");
        let reference_path = test_dir.join("reference.png");

        let html = fs::read_to_string(&html_path).map_err(|e| format!("Failed to read HTML file: {}", e))?;

        let css = fs::read_to_string(&css_path).unwrap_or_default();

        let full_html = if !css.is_empty() && !html.contains("<style>") {
            format!(
                r#"<!DOCTYPE html>
<html>
<head>
<style>
{}
</style>
</head>
<body>
{}
</body>
</html>"#,
                css,
                extract_body_content(&html)
            )
        } else {
            html
        };

        self.create_reference(&full_html, &reference_path)
    }
}

impl Default for RefTestHarness {
    fn default() -> Self {
        Self::new()
    }
}

/// Extracts body content from HTML, handling various formats
fn extract_body_content(html: &str) -> String {
    // Try to find <body> tag content
    if let Some(body_start) = html.find("<body>") {
        if let Some(body_end) = html.find("</body>") {
            let start = body_start + 6; // length of "<body>"
            if start <= body_end {
                return html[start..body_end].to_string();
            }
        }
    }

    // If no body tag, return the whole content
    html.to_string()
}

/// Converts PNG data to a Pixmap
fn png_to_pixmap(png_data: &[u8]) -> Result<Pixmap, String> {
    use std::io::Cursor;

    let cursor = Cursor::new(png_data);
    let decoder = image::codecs::png::PngDecoder::new(cursor).map_err(|e| format!("Failed to decode PNG: {}", e))?;

    use image::ImageDecoder;
    let (width, height) = decoder.dimensions();

    let mut rgba_data = vec![0u8; (width * height * 4) as usize];
    decoder
        .read_image(&mut rgba_data)
        .map_err(|e| format!("Failed to read PNG data: {}", e))?;

    let mut pixmap =
        Pixmap::new(width, height).ok_or_else(|| format!("Failed to create pixmap {}x{}", width, height))?;

    // Convert from RGBA to BGRA premultiplied (tiny-skia format)
    let dst_data = pixmap.data_mut();
    for i in 0..(width * height) as usize {
        let src_idx = i * 4;
        let dst_idx = i * 4;

        let r = rgba_data[src_idx];
        let g = rgba_data[src_idx + 1];
        let b = rgba_data[src_idx + 2];
        let a = rgba_data[src_idx + 3];

        // Premultiply alpha
        let alpha = a as f32 / 255.0;
        let pm_r = (r as f32 * alpha) as u8;
        let pm_g = (g as f32 * alpha) as u8;
        let pm_b = (b as f32 * alpha) as u8;

        // Store as BGRA
        dst_data[dst_idx] = pm_b;
        dst_data[dst_idx + 1] = pm_g;
        dst_data[dst_idx + 2] = pm_r;
        dst_data[dst_idx + 3] = a;
    }

    Ok(pixmap)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ref_test_config_default() {
        let config = RefTestConfig::default();
        assert_eq!(config.viewport_width, 800);
        assert_eq!(config.viewport_height, 600);
        assert!(config.save_actual_on_failure);
        assert!(config.save_diff_on_failure);
    }

    #[test]
    fn test_ref_test_config_with_viewport() {
        let config = RefTestConfig::with_viewport(1024, 768);
        assert_eq!(config.viewport_width, 1024);
        assert_eq!(config.viewport_height, 768);
    }

    #[test]
    fn test_ref_test_config_builder() {
        let config = RefTestConfig::default()
            .with_compare_config(CompareConfig::lenient())
            .with_save_actual_on_failure(false)
            .with_save_diff_on_failure(false);

        assert!(!config.save_actual_on_failure);
        assert!(!config.save_diff_on_failure);
        assert_eq!(config.compare_config.channel_tolerance, 5);
    }

    #[test]
    fn test_harness_creation() {
        let harness = RefTestHarness::new();
        assert_eq!(harness.config().viewport_width, 800);
        assert_eq!(harness.config().viewport_height, 600);
    }

    #[test]
    fn test_harness_with_config() {
        let config = RefTestConfig::with_viewport(1920, 1080);
        let harness = RefTestHarness::with_config(config);

        assert_eq!(harness.config().viewport_width, 1920);
        assert_eq!(harness.config().viewport_height, 1080);
    }

    #[test]
    fn test_extract_body_content() {
        let html_with_body = "<html><body><div>Content</div></body></html>";
        assert_eq!(extract_body_content(html_with_body), "<div>Content</div>");

        let html_without_body = "<div>Just content</div>";
        assert_eq!(extract_body_content(html_without_body), "<div>Just content</div>");

        let empty_body = "<html><body></body></html>";
        assert_eq!(extract_body_content(empty_body), "");
    }

    #[test]
    fn test_ref_test_result_summary_passed() {
        let result = RefTestResult {
            test_name: "test_box".to_string(),
            passed: true,
            image_diff: None,
            error: None,
            render_time: Duration::from_millis(100),
            compare_time: Duration::from_millis(50),
            actual_output_path: None,
            diff_output_path: None,
        };

        let summary = result.summary();
        assert!(summary.contains("PASS"));
        assert!(summary.contains("test_box"));
    }

    #[test]
    fn test_ref_test_result_summary_failed_error() {
        let result = RefTestResult {
            test_name: "test_box".to_string(),
            passed: false,
            image_diff: None,
            error: Some("File not found".to_string()),
            render_time: Duration::ZERO,
            compare_time: Duration::ZERO,
            actual_output_path: None,
            diff_output_path: None,
        };

        let summary = result.summary();
        assert!(summary.contains("FAIL"));
        assert!(summary.contains("File not found"));
    }

    #[test]
    fn test_ref_test_results_all_passed() {
        let results = RefTestResults {
            results: vec![
                RefTestResult {
                    test_name: "test1".to_string(),
                    passed: true,
                    image_diff: None,
                    error: None,
                    render_time: Duration::ZERO,
                    compare_time: Duration::ZERO,
                    actual_output_path: None,
                    diff_output_path: None,
                },
                RefTestResult {
                    test_name: "test2".to_string(),
                    passed: true,
                    image_diff: None,
                    error: None,
                    render_time: Duration::ZERO,
                    compare_time: Duration::ZERO,
                    actual_output_path: None,
                    diff_output_path: None,
                },
            ],
            total_time: Duration::from_secs(1),
        };

        assert!(results.all_passed());
        assert_eq!(results.passed_count(), 2);
        assert_eq!(results.failed_count(), 0);
    }

    #[test]
    fn test_ref_test_results_some_failed() {
        let results = RefTestResults {
            results: vec![
                RefTestResult {
                    test_name: "test1".to_string(),
                    passed: true,
                    image_diff: None,
                    error: None,
                    render_time: Duration::ZERO,
                    compare_time: Duration::ZERO,
                    actual_output_path: None,
                    diff_output_path: None,
                },
                RefTestResult {
                    test_name: "test2".to_string(),
                    passed: false,
                    image_diff: None,
                    error: Some("Error".to_string()),
                    render_time: Duration::ZERO,
                    compare_time: Duration::ZERO,
                    actual_output_path: None,
                    diff_output_path: None,
                },
            ],
            total_time: Duration::from_secs(1),
        };

        assert!(!results.all_passed());
        assert_eq!(results.passed_count(), 1);
        assert_eq!(results.failed_count(), 1);
        assert_eq!(results.failed_tests().count(), 1);
    }

    #[test]
    fn test_ref_test_results_summary() {
        let results = RefTestResults {
            results: vec![RefTestResult {
                test_name: "test1".to_string(),
                passed: true,
                image_diff: None,
                error: None,
                render_time: Duration::from_millis(100),
                compare_time: Duration::from_millis(50),
                actual_output_path: None,
                diff_output_path: None,
            }],
            total_time: Duration::from_millis(150),
        };

        let summary = results.summary();
        assert!(summary.contains("1/1 passed"));
        assert!(summary.contains("0 failed"));
    }

    #[test]
    fn test_compare_images_via_harness() {
        let harness = RefTestHarness::new();

        let pixmap1 = super::super::compare::create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();
        let pixmap2 = super::super::compare::create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();

        let diff = harness.compare_images(&pixmap1, &pixmap2);
        assert!(diff.is_match());
    }

    #[test]
    fn test_run_ref_test_missing_html() {
        let harness = RefTestHarness::new();
        let test_dir = Path::new("/nonexistent/test/dir");
        let reference = Path::new("/nonexistent/reference.png");

        let result = harness.run_ref_test(test_dir, reference);

        assert!(!result.passed);
        assert!(result.error.is_some());
        assert!(result.error.as_ref().unwrap().contains("Failed to read HTML"));
    }

    #[test]
    fn test_discover_tests_nonexistent_dir() {
        let harness = RefTestHarness::new();
        let result = harness.discover_tests(Path::new("/nonexistent/dir"));

        assert!(result.is_err());
    }
}
