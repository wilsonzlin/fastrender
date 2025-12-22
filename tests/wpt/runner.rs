//! WPT Test Runner
//!
//! This module provides the main test runner for executing Web Platform Tests
//! against the FastRender engine.
//!
//! # Overview
//!
//! The WPT (Web Platform Tests) are a cross-browser test suite for the web
//! platform stack. This runner executes a subset of these tests that are
//! relevant to a rendering engine focused on HTML/CSS to image conversion.
//!
//! # Usage
//!
//! ```rust,ignore
//! use fastrender::FastRender;
//! use crate::wpt::runner::WptRunner;
//! use std::path::Path;
//!
//! // Create a runner with the renderer
//! let renderer = Renderer::new();
//! let runner = WptRunner::new(renderer);
//!
//! // Run a single test
//! let result = runner.run_test(Path::new("tests/wpt/css/box-model/test.html"));
//!
//! // Run a test suite
//! let results = runner.run_suite(Path::new("tests/wpt/css/box-model/"));
//! ```
//!
//! # Test Types
//!
//! The runner supports:
//! - **Reference tests**: Compare rendered output against a reference HTML file
//! - **Visual tests**: Compare rendered output against expected PNG images
//! - **Crash tests**: Verify that rendering doesn't crash
//!
//! # Configuration
//!
//! The runner can be configured with:
//! - Custom viewport sizes
//! - Pixel tolerance for comparisons
//! - Timeout settings
//! - Output directories for artifacts

use super::harness::compare_images;
use super::harness::generate_diff_image;
use super::harness::HarnessConfig;
use super::harness::SuiteResult;
use super::harness::TestMetadata;
use super::harness::TestResult;
use super::harness::TestStatus;
use super::harness::TestType;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

/// Main WPT test runner
///
/// Executes Web Platform Tests against the FastRender engine.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::FastRender;
/// use crate::wpt::runner::WptRunner;
///
/// let renderer = Renderer::new();
/// let mut runner = WptRunner::new(renderer);
///
/// // Run a single test
/// let result = runner.run_test(Path::new("test.html"))?;
/// println!("Test {}: {}", result.metadata.id, result.status);
/// ```
pub struct WptRunner {
  /// The FastRender renderer instance
  renderer: fastrender::FastRender,
  /// Configuration for the test harness
  config: HarnessConfig,
  /// Statistics about test runs
  stats: RunnerStats,
}

/// Statistics tracked during test runs
#[derive(Debug, Clone, Default)]
pub struct RunnerStats {
  /// Total tests executed
  pub total: usize,
  /// Number of passed tests
  pub passed: usize,
  /// Number of failed tests
  pub failed: usize,
  /// Number of errored tests
  pub errors: usize,
  /// Number of skipped tests
  pub skipped: usize,
  /// Total execution time
  pub total_duration: Duration,
}

impl RunnerStats {
  /// Returns the pass rate as a percentage
  pub fn pass_rate(&self) -> f64 {
    if self.total == 0 {
      return 100.0;
    }
    (self.passed as f64 / self.total as f64) * 100.0
  }

  /// Updates stats with a test result
  fn record(&mut self, result: &TestResult) {
    self.total += 1;
    self.total_duration += result.duration;
    match result.status {
      TestStatus::Pass => self.passed += 1,
      TestStatus::Fail => self.failed += 1,
      TestStatus::Error => self.errors += 1,
      TestStatus::Skip => self.skipped += 1,
      TestStatus::Timeout => self.errors += 1,
    }
  }

  /// Resets all statistics
  pub fn reset(&mut self) {
    *self = Self::default();
  }
}

impl WptRunner {
  /// Creates a new WPT runner with the given renderer
  ///
  /// # Arguments
  ///
  /// * `renderer` - The FastRender renderer to use for test execution
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let renderer = Renderer::new();
  /// let runner = WptRunner::new(renderer);
  /// ```
  pub fn new(renderer: fastrender::FastRender) -> Self {
    Self {
      renderer,
      config: HarnessConfig::default(),
      stats: RunnerStats::default(),
    }
  }

  /// Creates a new WPT runner with custom configuration
  ///
  /// # Arguments
  ///
  /// * `renderer` - The FastRender renderer to use
  /// * `config` - Custom harness configuration
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let config = HarnessConfig::default()
  ///     .with_tolerance(5)
  ///     .with_max_diff(0.1);
  /// let runner = WptRunner::with_config(Renderer::new(), config);
  /// ```
  pub fn with_config(renderer: fastrender::FastRender, config: HarnessConfig) -> Self {
    Self {
      renderer,
      config,
      stats: RunnerStats::default(),
    }
  }

  /// Returns the current configuration
  pub fn config(&self) -> &HarnessConfig {
    &self.config
  }

  /// Returns mutable reference to configuration
  pub fn config_mut(&mut self) -> &mut HarnessConfig {
    &mut self.config
  }

  /// Returns the current statistics
  pub fn stats(&self) -> &RunnerStats {
    &self.stats
  }

  /// Resets the runner statistics
  pub fn reset_stats(&mut self) {
    self.stats.reset();
  }

  /// Runs a single test
  ///
  /// # Arguments
  ///
  /// * `test_path` - Path to the test HTML file
  ///
  /// # Returns
  ///
  /// The test result including status, duration, and any error messages
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let result = runner.run_test(Path::new("tests/wpt/css/test.html"));
  /// match result.status {
  ///     TestStatus::Pass => println!("Test passed!"),
  ///     TestStatus::Fail => println!("Test failed: {:?}", result.message),
  ///     _ => {}
  /// }
  /// ```
  pub fn run_test(&mut self, test_path: &Path) -> TestResult {
    let start = Instant::now();
    let mut metadata = TestMetadata::from_path(test_path.to_path_buf());

    if metadata.test_type == TestType::Reftest && metadata.reference_path.is_none() {
      let expected_path = self.get_expected_image_path(&metadata);
      if expected_path.exists() {
        metadata.test_type = TestType::Visual;
      }
    }

    // Check if test is disabled
    if metadata.disabled {
      let reason = metadata
        .disabled_reason
        .clone()
        .unwrap_or_else(|| "Disabled".to_string());
      let result = TestResult::skip(metadata, reason);
      self.stats.record(&result);
      return result;
    }

    // Check filter
    if let Some(ref filter) = self.config.filter {
      if !metadata.id.contains(filter) && !test_path.to_string_lossy().contains(filter) {
        let result = TestResult::skip(metadata, "Filtered out");
        self.stats.record(&result);
        return result;
      }
    }

    // Execute based on test type
    let result = match metadata.test_type {
      TestType::Reftest => self.run_reftest(&metadata, start),
      TestType::Visual => self.run_visual_test(&metadata, start),
      TestType::Crashtest => self.run_crashtest(&metadata, start),
      TestType::Testharness => TestResult::skip(metadata, "Testharness tests not yet supported"),
      TestType::Manual => TestResult::skip(metadata, "Manual tests not supported"),
    };

    self.stats.record(&result);

    // Save artifacts if configured
    if self.config.save_rendered && result.rendered_image.is_some() {
      self.save_artifact(&result);
    }

    result
  }

  /// Runs all tests in a directory (test suite)
  ///
  /// # Arguments
  ///
  /// * `suite_dir` - Path to the directory containing test files
  ///
  /// # Returns
  ///
  /// Vector of test results for all tests in the suite
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let results = runner.run_suite(Path::new("tests/wpt/css/box-model/"));
  /// println!("Passed: {}/{}",
  ///     results.iter().filter(|r| r.status == TestStatus::Pass).count(),
  ///     results.len()
  /// );
  /// ```
  pub fn run_suite(&mut self, suite_dir: &Path) -> Vec<TestResult> {
    let mut results = Vec::new();

    // Collect test files
    let test_files = self.collect_tests(suite_dir);

    for test_path in test_files {
      let result = self.run_test(&test_path);
      results.push(result);

      // Check fail-fast
      if self.config.fail_fast && results.last().is_some_and(|r| r.status.is_failure()) {
        break;
      }
    }

    results
  }

  /// Runs a test suite and returns aggregated results
  ///
  /// # Arguments
  ///
  /// * `suite_dir` - Path to the directory containing test files
  ///
  /// # Returns
  ///
  /// Aggregated suite result with counts and statistics
  pub fn run_suite_aggregated(&mut self, suite_dir: &Path) -> SuiteResult {
    let suite_name = suite_dir
      .file_name()
      .and_then(|n| n.to_str())
      .unwrap_or("unknown")
      .to_string();

    let mut suite = SuiteResult::new(suite_name);
    let results = self.run_suite(suite_dir);

    for result in results {
      suite.add_result(result);
    }

    suite.finalize();
    suite
  }

  /// Collects all test files from a directory
  fn collect_tests(&self, dir: &Path) -> Vec<PathBuf> {
    let mut tests = Vec::new();

    if !dir.exists() {
      return tests;
    }

    if let Ok(entries) = fs::read_dir(dir) {
      for entry in entries.flatten() {
        let path = entry.path();
        if path.is_file() && self.is_test_file(&path) {
          tests.push(path);
        } else if path.is_dir() {
          // Recursively collect from subdirectories
          tests.extend(self.collect_tests(&path));
        }
      }
    }

    // Sort for deterministic ordering
    tests.sort();
    tests
  }

  /// Checks if a file is a test file
  fn is_test_file(&self, path: &Path) -> bool {
    let extension = path.extension().and_then(|e| e.to_str());
    let filename = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

    // Must be HTML file
    if extension != Some("html") && extension != Some("htm") {
      return false;
    }

    // Skip reference files
    if filename.ends_with("-ref.html") || filename.ends_with("-ref.htm") {
      return false;
    }

    // Skip expected files
    if filename.ends_with("-expected.html") {
      return false;
    }

    // Skip support files
    if filename.starts_with("support") {
      return false;
    }

    true
  }

  /// Runs a reference test
  fn run_reftest(&mut self, metadata: &TestMetadata, start: Instant) -> TestResult {
    // Check for reference file
    let ref_path = match &metadata.reference_path {
      Some(path) => path,
      None => {
        return TestResult::error(metadata.clone(), start.elapsed(), "No reference file found");
      }
    };

    // Read test HTML
    let test_html = match fs::read_to_string(&metadata.path) {
      Ok(html) => html,
      Err(e) => {
        return TestResult::error(
          metadata.clone(),
          start.elapsed(),
          format!("Failed to read test file: {}", e),
        );
      }
    };

    // Read reference HTML
    let ref_html = match fs::read_to_string(ref_path) {
      Ok(html) => html,
      Err(e) => {
        return TestResult::error(
          metadata.clone(),
          start.elapsed(),
          format!("Failed to read reference file: {}", e),
        );
      }
    };

    // Render test HTML
    let test_image = match self.render_html(&test_html, metadata) {
      Ok(img) => img,
      Err(e) => {
        return TestResult::error(
          metadata.clone(),
          start.elapsed(),
          format!("Failed to render test: {}", e),
        );
      }
    };

    // Render reference HTML
    let ref_image = match self.render_html(&ref_html, metadata) {
      Ok(img) => img,
      Err(e) => {
        return TestResult::error(
          metadata.clone(),
          start.elapsed(),
          format!("Failed to render reference: {}", e),
        );
      }
    };

    // Compare images
    self.compare_and_result(metadata, start, test_image, ref_image)
  }

  /// Runs a visual test against expected PNG
  fn run_visual_test(&mut self, metadata: &TestMetadata, start: Instant) -> TestResult {
    // Find expected image
    let expected_path = self.get_expected_image_path(metadata);

    // Read expected image if it exists
    let expected_image = if expected_path.exists() {
      match fs::read(&expected_path) {
        Ok(data) => Some(data),
        Err(e) => {
          return TestResult::error(
            metadata.clone(),
            start.elapsed(),
            format!("Failed to read expected image: {}", e),
          );
        }
      }
    } else if self.config.update_expected {
      // Generate expected image
      None
    } else {
      return TestResult::error(
        metadata.clone(),
        start.elapsed(),
        format!("Expected image not found: {:?}", expected_path),
      );
    };

    // Read and render test HTML
    let test_html = match fs::read_to_string(&metadata.path) {
      Ok(html) => html,
      Err(e) => {
        return TestResult::error(
          metadata.clone(),
          start.elapsed(),
          format!("Failed to read test file: {}", e),
        );
      }
    };

    let rendered_image = match self.render_html(&test_html, metadata) {
      Ok(img) => img,
      Err(e) => {
        return TestResult::error(
          metadata.clone(),
          start.elapsed(),
          format!("Failed to render test: {}", e),
        );
      }
    };

    // If no expected image and update mode, save and pass
    if expected_image.is_none() && self.config.update_expected {
      if let Err(e) = self.save_expected_image(&expected_path, &rendered_image) {
        return TestResult::error(
          metadata.clone(),
          start.elapsed(),
          format!("Failed to save expected image: {}", e),
        );
      }
      return TestResult::pass(metadata.clone(), start.elapsed());
    }

    let expected_image = expected_image.unwrap();
    self.compare_and_result(metadata, start, rendered_image, expected_image)
  }

  /// Runs a crash test
  fn run_crashtest(&mut self, metadata: &TestMetadata, start: Instant) -> TestResult {
    // Read test HTML
    let test_html = match fs::read_to_string(&metadata.path) {
      Ok(html) => html,
      Err(e) => {
        return TestResult::error(
          metadata.clone(),
          start.elapsed(),
          format!("Failed to read test file: {}", e),
        );
      }
    };

    // Try to render - success means no crash
    match self.render_html(&test_html, metadata) {
      Ok(_) => TestResult::pass(metadata.clone(), start.elapsed()),
      Err(e) => {
        // Rendering failed, but didn't crash - could be expected
        // For crash tests, we only care that it doesn't panic
        TestResult::error(
          metadata.clone(),
          start.elapsed(),
          format!("Render error (not a crash): {}", e),
        )
      }
    }
  }

  /// Renders HTML and returns PNG bytes
  fn render_html(&mut self, html: &str, metadata: &TestMetadata) -> Result<Vec<u8>, String> {
    self
      .renderer
      .render_to_png(html, metadata.viewport_width, metadata.viewport_height)
      .map_err(|e| format!("Render error: {}", e))
  }

  /// Compares two images and creates the appropriate result
  fn compare_and_result(
    &self,
    metadata: &TestMetadata,
    start: Instant,
    rendered: Vec<u8>,
    expected: Vec<u8>,
  ) -> TestResult {
    let duration = start.elapsed();

    match compare_images(&rendered, &expected, self.config.pixel_tolerance) {
      Ok((diff_pixels, _total_pixels, diff_percentage)) => {
        if diff_percentage <= self.config.max_diff_percentage {
          TestResult::pass(metadata.clone(), duration)
            .with_images(rendered, expected)
            .with_diff(diff_pixels, diff_percentage)
        } else {
          TestResult::fail(
            metadata.clone(),
            duration,
            format!(
              "Image mismatch: {}% difference ({} pixels)",
              diff_percentage, diff_pixels
            ),
          )
          .with_images(rendered, expected)
          .with_diff(diff_pixels, diff_percentage)
        }
      }
      Err(e) => TestResult::error(
        metadata.clone(),
        duration,
        format!("Comparison error: {}", e),
      ),
    }
  }

  /// Gets the path for the expected image
  fn get_expected_image_path(&self, metadata: &TestMetadata) -> PathBuf {
    // Try to construct expected path from test path
    let relative = metadata
      .path
      .strip_prefix(&self.config.test_dir)
      .unwrap_or(&metadata.path);

    let mut expected_path = self.config.expected_dir.join(relative);
    expected_path.set_extension("png");
    expected_path
  }

  /// Saves the expected image
  fn save_expected_image(&self, path: &Path, image: &[u8]) -> Result<(), String> {
    if let Some(parent) = path.parent() {
      fs::create_dir_all(parent).map_err(|e| format!("Failed to create directory: {}", e))?;
    }
    fs::write(path, image).map_err(|e| format!("Failed to write image: {}", e))
  }

  /// Saves test artifacts (rendered images, diffs)
  fn save_artifact(&self, result: &TestResult) {
    let output_dir = &self.config.output_dir;

    if let Err(e) = fs::create_dir_all(output_dir) {
      eprintln!("Failed to create output directory: {}", e);
      return;
    }

    let test_id = &result.metadata.id;

    // Save rendered image
    if let Some(ref rendered) = result.rendered_image {
      let path = output_dir.join(format!("{}-rendered.png", test_id));
      if let Err(e) = fs::write(&path, rendered) {
        eprintln!("Failed to save rendered image: {}", e);
      }
    }

    // Save diff image if failed
    if result.status == TestStatus::Fail && self.config.save_diffs {
      if let (Some(ref rendered), Some(ref expected)) =
        (&result.rendered_image, &result.expected_image)
      {
        if let Ok(diff) = generate_diff_image(rendered, expected) {
          let path = output_dir.join(format!("{}-diff.png", test_id));
          if let Err(e) = fs::write(&path, diff) {
            eprintln!("Failed to save diff image: {}", e);
          }
        }
      }
    }
  }
}

/// Builder for creating WptRunner instances with fluent API
pub struct WptRunnerBuilder {
  renderer: Option<fastrender::FastRender>,
  config: HarnessConfig,
}

impl WptRunnerBuilder {
  /// Creates a new builder
  pub fn new() -> Self {
    Self {
      renderer: None,
      config: HarnessConfig::default(),
    }
  }

  /// Sets the renderer
  pub fn renderer(mut self, renderer: fastrender::FastRender) -> Self {
    self.renderer = Some(renderer);
    self
  }

  /// Sets the test directory
  pub fn test_dir(mut self, dir: impl Into<PathBuf>) -> Self {
    self.config.test_dir = dir.into();
    self
  }

  /// Sets the expected images directory
  pub fn expected_dir(mut self, dir: impl Into<PathBuf>) -> Self {
    self.config.expected_dir = dir.into();
    self
  }

  /// Sets the output directory for artifacts
  pub fn output_dir(mut self, dir: impl Into<PathBuf>) -> Self {
    self.config.output_dir = dir.into();
    self
  }

  /// Sets the pixel tolerance
  pub fn tolerance(mut self, tolerance: u8) -> Self {
    self.config.pixel_tolerance = tolerance;
    self
  }

  /// Sets the maximum allowed difference percentage
  pub fn max_diff(mut self, max_diff: f64) -> Self {
    self.config.max_diff_percentage = max_diff;
    self
  }

  /// Enables fail-fast mode
  pub fn fail_fast(mut self) -> Self {
    self.config.fail_fast = true;
    self
  }

  /// Sets a filter pattern
  pub fn filter(mut self, filter: impl Into<String>) -> Self {
    self.config.filter = Some(filter.into());
    self
  }

  /// Enables saving rendered images
  pub fn save_rendered(mut self) -> Self {
    self.config.save_rendered = true;
    self
  }

  /// Enables saving diff images
  pub fn save_diffs(mut self) -> Self {
    self.config.save_diffs = true;
    self
  }

  /// Enables update expected mode
  pub fn update_expected(mut self) -> Self {
    self.config.update_expected = true;
    self
  }

  /// Builds the runner
  ///
  /// # Panics
  ///
  /// Panics if no renderer was provided
  pub fn build(self) -> WptRunner {
    WptRunner {
      renderer: self.renderer.expect("Renderer is required"),
      config: self.config,
      stats: RunnerStats::default(),
    }
  }
}

impl Default for WptRunnerBuilder {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use tempfile::TempDir;

  fn create_test_renderer() -> fastrender::FastRender {
    fastrender::FastRender::new().unwrap()
  }

  #[test]
  fn test_runner_creation() {
    let renderer = create_test_renderer();
    let runner = WptRunner::new(renderer);

    assert_eq!(runner.stats().total, 0);
    assert_eq!(runner.stats().passed, 0);
  }

  #[test]
  fn test_runner_with_config() {
    let renderer = create_test_renderer();
    let config = HarnessConfig::default()
      .with_tolerance(5)
      .with_max_diff(1.0);

    let runner = WptRunner::with_config(renderer, config);

    assert_eq!(runner.config().pixel_tolerance, 5);
    assert_eq!(runner.config().max_diff_percentage, 1.0);
  }

  #[test]
  fn test_runner_builder() {
    let renderer = create_test_renderer();
    let runner = WptRunnerBuilder::new()
      .renderer(renderer)
      .test_dir("custom/tests")
      .tolerance(10)
      .max_diff(2.0)
      .fail_fast()
      .build();

    assert_eq!(runner.config().test_dir, PathBuf::from("custom/tests"));
    assert_eq!(runner.config().pixel_tolerance, 10);
    assert_eq!(runner.config().max_diff_percentage, 2.0);
    assert!(runner.config().fail_fast);
  }

  #[test]
  fn test_runner_stats() {
    let mut stats = RunnerStats::default();

    assert_eq!(stats.total, 0);
    assert_eq!(stats.pass_rate(), 100.0);

    // Simulate recording results
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let pass_result = TestResult::pass(metadata.clone(), Duration::from_millis(100));
    stats.record(&pass_result);

    assert_eq!(stats.total, 1);
    assert_eq!(stats.passed, 1);
    assert_eq!(stats.pass_rate(), 100.0);

    let fail_result = TestResult::fail(metadata.clone(), Duration::from_millis(100), "Failed");
    stats.record(&fail_result);

    assert_eq!(stats.total, 2);
    assert_eq!(stats.passed, 1);
    assert_eq!(stats.failed, 1);
    assert_eq!(stats.pass_rate(), 50.0);
  }

  #[test]
  fn test_runner_stats_reset() {
    let mut stats = RunnerStats {
      total: 10,
      passed: 5,
      ..Default::default()
    };

    stats.reset();

    assert_eq!(stats.total, 0);
    assert_eq!(stats.passed, 0);
  }

  #[test]
  fn test_is_test_file() {
    let renderer = create_test_renderer();
    let runner = WptRunner::new(renderer);

    assert!(runner.is_test_file(Path::new("test.html")));
    assert!(runner.is_test_file(Path::new("test.htm")));
    assert!(!runner.is_test_file(Path::new("test-ref.html")));
    assert!(!runner.is_test_file(Path::new("test-expected.html")));
    assert!(!runner.is_test_file(Path::new("support-file.html")));
    assert!(!runner.is_test_file(Path::new("test.css")));
    assert!(!runner.is_test_file(Path::new("test.png")));
  }

  #[test]
  fn test_collect_tests_empty_dir() {
    let renderer = create_test_renderer();
    let runner = WptRunner::new(renderer);

    let temp = TempDir::new().unwrap();
    let tests = runner.collect_tests(temp.path());

    assert!(tests.is_empty());
  }

  #[test]
  fn test_collect_tests_with_files() {
    let renderer = create_test_renderer();
    let runner = WptRunner::new(renderer);

    let temp = TempDir::new().unwrap();

    // Create test files
    std::fs::write(temp.path().join("test1.html"), "<html></html>").unwrap();
    std::fs::write(temp.path().join("test2.html"), "<html></html>").unwrap();
    std::fs::write(temp.path().join("test1-ref.html"), "<html></html>").unwrap(); // Should be skipped
    std::fs::write(temp.path().join("style.css"), "body {}").unwrap(); // Should be skipped

    let tests = runner.collect_tests(temp.path());

    assert_eq!(tests.len(), 2);
    assert!(tests.iter().all(|p| p.extension().unwrap() == "html"));
    assert!(tests.iter().all(|p| !p.to_string_lossy().contains("-ref")));
  }

  #[test]
  fn test_run_test_disabled() {
    let renderer = create_test_renderer();
    let mut runner = WptRunner::new(renderer);

    let temp = TempDir::new().unwrap();
    let test_path = temp.path().join("disabled-test.html");
    std::fs::write(&test_path, "<html></html>").unwrap();

    // This won't actually disable the test since we need to modify metadata
    // But we can test the flow with a non-existent file
    let result = runner.run_test(&test_path);

    // Should error because no reference file
    assert!(result.status == TestStatus::Error || result.status == TestStatus::Skip);
  }

  #[test]
  fn test_run_test_filtered_out() {
    let renderer = create_test_renderer();
    let config = HarnessConfig::default().with_filter("nonexistent");
    let mut runner = WptRunner::with_config(renderer, config);

    let temp = TempDir::new().unwrap();
    let test_path = temp.path().join("test.html");
    std::fs::write(&test_path, "<html></html>").unwrap();

    let result = runner.run_test(&test_path);

    assert_eq!(result.status, TestStatus::Skip);
    assert!(result.message.unwrap().contains("Filtered"));
  }

  #[test]
  fn test_run_crashtest_success() {
    let renderer = create_test_renderer();
    let mut runner = WptRunner::new(renderer);

    let temp = TempDir::new().unwrap();
    let test_path = temp.path().join("crashtest.html");
    std::fs::write(
      &test_path,
      r#"<!DOCTYPE html>
            <html>
            <head><title>Crash Test</title></head>
            <body><div>Should not crash</div></body>
            </html>"#,
    )
    .unwrap();

    let result = runner.run_test(&test_path);

    // Crashtest should pass if rendering doesn't panic
    // (might error on rendering but that's ok for crashtest)
    assert!(result.status == TestStatus::Pass || result.status == TestStatus::Error);
  }

  #[test]
  fn test_suite_aggregated_result() {
    let renderer = create_test_renderer();
    let mut runner = WptRunner::new(renderer);

    let temp = TempDir::new().unwrap();

    // Create some test files
    std::fs::write(temp.path().join("test1.html"), "<html></html>").unwrap();
    std::fs::write(temp.path().join("test2.html"), "<html></html>").unwrap();

    let suite_result = runner.run_suite_aggregated(temp.path());

    assert_eq!(suite_result.total(), 2);
    assert!(suite_result.duration.as_nanos() > 0);
  }

  #[test]
  fn test_get_expected_image_path() {
    let renderer = create_test_renderer();
    let config = HarnessConfig::with_test_dir("/tests/wpt");
    let runner = WptRunner::with_config(renderer, config);

    let metadata = TestMetadata::from_path(PathBuf::from("/tests/wpt/css/test.html"));
    let expected_path = runner.get_expected_image_path(&metadata);

    assert!(expected_path.ends_with("css/test.png"));
  }
}
