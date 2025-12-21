//! WPT Test Harness
//!
//! This module provides the test harness infrastructure for running Web Platform Tests.
//! It handles test case parsing, comparison logic, and result aggregation.
//!
//! # WPT Test Format
//!
//! Web Platform Tests (WPT) are standardized tests for web browsers.
//! FastRender supports two primary test formats:
//!
//! 1. **Reftest (Reference Test)**: Compare rendered output against a reference image
//! 2. **Testharness**: JavaScript-based tests (not yet supported)
//!
//! # Reference Tests
//!
//! Reference tests consist of:
//! - A test HTML file
//! - A reference HTML file (or expected image)
//! - Metadata specifying the comparison type
//!
//! ## Example Directory Structure
//! ```text
//! tests/wpt/
//! ├── css/
//! │   └── CSS2/
//! │       └── box-model/
//! │           ├── margin-applies-to-001.html     # Test file
//! │           ├── margin-applies-to-001-ref.html # Reference file
//! │           └── margin-applies-to-002.html
//! └── expected/
//!     └── css/
//!         └── CSS2/
//!             └── box-model/
//!                 └── margin-applies-to-002.png  # Expected image
//! ```

use std::collections::HashMap;
use std::fmt;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

/// Result of a single test assertion
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssertionResult {
  /// Assertion passed
  Pass,
  /// Assertion failed with a message
  Fail(String),
  /// Assertion could not be evaluated
  Error(String),
}

impl AssertionResult {
  /// Returns true if this assertion passed
  pub fn is_pass(&self) -> bool {
    matches!(self, AssertionResult::Pass)
  }

  /// Returns true if this assertion failed
  pub fn is_fail(&self) -> bool {
    matches!(self, AssertionResult::Fail(_))
  }

  /// Returns true if this assertion errored
  pub fn is_error(&self) -> bool {
    matches!(self, AssertionResult::Error(_))
  }
}

impl fmt::Display for AssertionResult {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      AssertionResult::Pass => write!(f, "PASS"),
      AssertionResult::Fail(msg) => write!(f, "FAIL: {}", msg),
      AssertionResult::Error(msg) => write!(f, "ERROR: {}", msg),
    }
  }
}

/// Overall status of a test
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TestStatus {
  /// Test passed
  Pass,
  /// Test failed
  Fail,
  /// Test encountered an error during execution
  Error,
  /// Test was skipped
  Skip,
  /// Test timed out
  Timeout,
}

impl TestStatus {
  /// Returns true if this represents a successful outcome
  pub fn is_success(&self) -> bool {
    matches!(self, TestStatus::Pass | TestStatus::Skip)
  }

  /// Returns true if this represents a failure
  pub fn is_failure(&self) -> bool {
    matches!(
      self,
      TestStatus::Fail | TestStatus::Error | TestStatus::Timeout
    )
  }
}

impl fmt::Display for TestStatus {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TestStatus::Pass => write!(f, "PASS"),
      TestStatus::Fail => write!(f, "FAIL"),
      TestStatus::Error => write!(f, "ERROR"),
      TestStatus::Skip => write!(f, "SKIP"),
      TestStatus::Timeout => write!(f, "TIMEOUT"),
    }
  }
}

/// Type of WPT test
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestType {
  /// Reference test - compare rendered output
  Reftest,
  /// Visual test with expected image
  Visual,
  /// Testharness.js test (not yet supported)
  Testharness,
  /// Manual test (not supported)
  Manual,
  /// Crashtests (should not crash)
  Crashtest,
}

impl TestType {
  /// Parses test type from file metadata or naming convention
  pub fn from_path(path: &Path) -> Self {
    let filename = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

    if filename.ends_with("-ref.html") || filename.ends_with("-ref.htm") {
      // This is a reference file, not a test
      TestType::Reftest
    } else if filename.contains("crash") {
      TestType::Crashtest
    } else if filename.contains("manual") {
      TestType::Manual
    } else {
      // Default to reftest for visual comparison
      TestType::Reftest
    }
  }
}

/// Metadata about a test case
#[derive(Debug, Clone)]
pub struct TestMetadata {
  /// Unique test identifier
  pub id: String,
  /// Path to the test file
  pub path: PathBuf,
  /// Type of test
  pub test_type: TestType,
  /// Optional reference file for reftests
  pub reference_path: Option<PathBuf>,
  /// Expected result (for tests marked as expected fail)
  pub expected_status: Option<TestStatus>,
  /// Test timeout in milliseconds
  pub timeout_ms: u64,
  /// Whether the test is disabled
  pub disabled: bool,
  /// Reason for being disabled
  pub disabled_reason: Option<String>,
  /// Custom viewport size
  pub viewport_width: u32,
  pub viewport_height: u32,
}

impl TestMetadata {
  /// Creates new test metadata from a path
  ///
  /// # Arguments
  ///
  /// * `path` - Path to the test file
  ///
  /// # Returns
  ///
  /// New TestMetadata with defaults
  pub fn from_path(path: PathBuf) -> Self {
    let id = path
      .file_stem()
      .and_then(|n| n.to_str())
      .unwrap_or("unknown")
      .to_string();

    let test_type = TestType::from_path(&path);

    // Look for reference file
    let reference_path = Self::find_reference_file(&path);

    Self {
      id,
      path,
      test_type,
      reference_path,
      expected_status: None,
      timeout_ms: 30000, // 30 second default timeout
      disabled: false,
      disabled_reason: None,
      viewport_width: 800,
      viewport_height: 600,
    }
  }

  /// Finds the reference file for a test
  fn find_reference_file(test_path: &Path) -> Option<PathBuf> {
    let parent = test_path.parent()?;
    let stem = test_path.file_stem()?.to_str()?;

    // Try common reference file patterns
    let patterns = [
      format!("{}-ref.html", stem),
      format!("{}-ref.htm", stem),
      format!("{}-expected.html", stem),
      format!("{}_ref.html", stem),
    ];

    for pattern in &patterns {
      let ref_path = parent.join(pattern);
      if ref_path.exists() {
        return Some(ref_path);
      }
    }

    None
  }

  /// Creates a new test metadata with custom viewport
  pub fn with_viewport(mut self, width: u32, height: u32) -> Self {
    self.viewport_width = width;
    self.viewport_height = height;
    self
  }

  /// Creates a new test metadata with custom timeout
  pub fn with_timeout(mut self, timeout_ms: u64) -> Self {
    self.timeout_ms = timeout_ms;
    self
  }

  /// Marks this test as disabled
  pub fn disable(mut self, reason: impl Into<String>) -> Self {
    self.disabled = true;
    self.disabled_reason = Some(reason.into());
    self
  }

  /// Sets the expected status for expected failures
  pub fn expect(mut self, status: TestStatus) -> Self {
    self.expected_status = Some(status);
    self
  }
}

/// Result of a single test execution
#[derive(Debug, Clone)]
pub struct TestResult {
  /// Test metadata
  pub metadata: TestMetadata,
  /// Overall test status
  pub status: TestStatus,
  /// Individual assertion results
  pub assertions: Vec<AssertionResult>,
  /// Execution time
  pub duration: Duration,
  /// Optional error message
  pub message: Option<String>,
  /// Rendered image (if applicable)
  pub rendered_image: Option<Vec<u8>>,
  /// Expected image (if applicable)
  pub expected_image: Option<Vec<u8>>,
  /// Pixel difference count (for visual tests)
  pub pixel_diff: Option<u64>,
  /// Difference percentage (0.0 to 100.0)
  pub diff_percentage: Option<f64>,
}

impl TestResult {
  /// Creates a new passing test result
  pub fn pass(metadata: TestMetadata, duration: Duration) -> Self {
    Self {
      metadata,
      status: TestStatus::Pass,
      assertions: vec![AssertionResult::Pass],
      duration,
      message: None,
      rendered_image: None,
      expected_image: None,
      pixel_diff: None,
      diff_percentage: None,
    }
  }

  /// Creates a new failing test result
  pub fn fail(metadata: TestMetadata, duration: Duration, message: impl Into<String>) -> Self {
    let msg = message.into();
    Self {
      metadata,
      status: TestStatus::Fail,
      assertions: vec![AssertionResult::Fail(msg.clone())],
      duration,
      message: Some(msg),
      rendered_image: None,
      expected_image: None,
      pixel_diff: None,
      diff_percentage: None,
    }
  }

  /// Creates a new error test result
  pub fn error(metadata: TestMetadata, duration: Duration, message: impl Into<String>) -> Self {
    let msg = message.into();
    Self {
      metadata,
      status: TestStatus::Error,
      assertions: vec![AssertionResult::Error(msg.clone())],
      duration,
      message: Some(msg),
      rendered_image: None,
      expected_image: None,
      pixel_diff: None,
      diff_percentage: None,
    }
  }

  /// Creates a skipped test result
  pub fn skip(metadata: TestMetadata, reason: impl Into<String>) -> Self {
    Self {
      metadata,
      status: TestStatus::Skip,
      assertions: vec![],
      duration: Duration::ZERO,
      message: Some(reason.into()),
      rendered_image: None,
      expected_image: None,
      pixel_diff: None,
      diff_percentage: None,
    }
  }

  /// Creates a timeout test result
  pub fn timeout(metadata: TestMetadata, duration: Duration) -> Self {
    Self {
      metadata,
      status: TestStatus::Timeout,
      assertions: vec![],
      duration,
      message: Some("Test execution timed out".to_string()),
      rendered_image: None,
      expected_image: None,
      pixel_diff: None,
      diff_percentage: None,
    }
  }

  /// Attaches rendered and expected images for comparison
  pub fn with_images(mut self, rendered: Vec<u8>, expected: Vec<u8>) -> Self {
    self.rendered_image = Some(rendered);
    self.expected_image = Some(expected);
    self
  }

  /// Sets the pixel difference information
  pub fn with_diff(mut self, pixel_diff: u64, diff_percentage: f64) -> Self {
    self.pixel_diff = Some(pixel_diff);
    self.diff_percentage = Some(diff_percentage);
    self
  }
}

/// Aggregated results from a test suite
#[derive(Debug, Clone)]
pub struct SuiteResult {
  /// Name of the test suite
  pub name: String,
  /// All test results
  pub results: Vec<TestResult>,
  /// Total execution time
  pub duration: Duration,
  /// Timestamp when suite started
  pub started_at: Instant,
}

impl SuiteResult {
  /// Creates a new suite result
  pub fn new(name: impl Into<String>) -> Self {
    Self {
      name: name.into(),
      results: Vec::new(),
      duration: Duration::ZERO,
      started_at: Instant::now(),
    }
  }

  /// Adds a test result to the suite
  pub fn add_result(&mut self, result: TestResult) {
    self.results.push(result);
  }

  /// Finalizes the suite result
  pub fn finalize(&mut self) {
    self.duration = self.started_at.elapsed();
  }

  /// Returns the count of tests by status
  pub fn counts(&self) -> HashMap<TestStatus, usize> {
    let mut counts = HashMap::new();
    for result in &self.results {
      *counts.entry(result.status).or_insert(0) += 1;
    }
    counts
  }

  /// Returns the number of passed tests
  pub fn passed(&self) -> usize {
    self
      .results
      .iter()
      .filter(|r| r.status == TestStatus::Pass)
      .count()
  }

  /// Returns the number of failed tests
  pub fn failed(&self) -> usize {
    self
      .results
      .iter()
      .filter(|r| r.status == TestStatus::Fail)
      .count()
  }

  /// Returns the number of error tests
  pub fn errors(&self) -> usize {
    self
      .results
      .iter()
      .filter(|r| r.status == TestStatus::Error)
      .count()
  }

  /// Returns the number of skipped tests
  pub fn skipped(&self) -> usize {
    self
      .results
      .iter()
      .filter(|r| r.status == TestStatus::Skip)
      .count()
  }

  /// Returns the total number of tests
  pub fn total(&self) -> usize {
    self.results.len()
  }

  /// Returns true if all tests passed or were skipped
  pub fn is_success(&self) -> bool {
    self.results.iter().all(|r| r.status.is_success())
  }

  /// Returns the pass rate as a percentage (0.0 to 100.0)
  pub fn pass_rate(&self) -> f64 {
    if self.results.is_empty() {
      return 100.0;
    }
    (self.passed() as f64 / self.total() as f64) * 100.0
  }
}

impl fmt::Display for SuiteResult {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "Suite: {}", self.name)?;
    writeln!(f, "  Duration: {:?}", self.duration)?;
    writeln!(f, "  Total: {}", self.total())?;
    writeln!(f, "  Passed: {}", self.passed())?;
    writeln!(f, "  Failed: {}", self.failed())?;
    writeln!(f, "  Errors: {}", self.errors())?;
    writeln!(f, "  Skipped: {}", self.skipped())?;
    writeln!(f, "  Pass Rate: {:.1}%", self.pass_rate())
  }
}

/// Configuration for the test harness
#[derive(Debug, Clone)]
pub struct HarnessConfig {
  /// Base directory for test files
  pub test_dir: PathBuf,
  /// Directory for expected images
  pub expected_dir: PathBuf,
  /// Output directory for test artifacts
  pub output_dir: PathBuf,
  /// Whether to save rendered images
  pub save_rendered: bool,
  /// Whether to save diff images
  pub save_diffs: bool,
  /// Tolerance for pixel comparison (0-255)
  pub pixel_tolerance: u8,
  /// Maximum allowed percentage difference
  pub max_diff_percentage: f64,
  /// Default timeout in milliseconds
  pub default_timeout_ms: u64,
  /// Whether to fail fast on first failure
  pub fail_fast: bool,
  /// Whether to run tests in parallel
  pub parallel: bool,
  /// Number of parallel workers
  pub workers: usize,
  /// Whether to update expected images on failure
  pub update_expected: bool,
  /// Filter tests by name pattern
  pub filter: Option<String>,
}

impl Default for HarnessConfig {
  fn default() -> Self {
    Self {
      test_dir: PathBuf::from("tests/wpt"),
      expected_dir: PathBuf::from("tests/wpt/expected"),
      output_dir: PathBuf::from("target/wpt-output"),
      save_rendered: true,
      save_diffs: true,
      pixel_tolerance: 0,
      max_diff_percentage: 0.0,
      default_timeout_ms: 30000,
      fail_fast: false,
      parallel: false,
      workers: 4,
      update_expected: false,
      filter: None,
    }
  }
}

impl HarnessConfig {
  /// Creates a new configuration with custom test directory
  pub fn with_test_dir(test_dir: impl Into<PathBuf>) -> Self {
    let test_dir = test_dir.into();
    let expected_dir = test_dir.join("expected");
    Self {
      test_dir,
      expected_dir,
      ..Default::default()
    }
  }

  /// Sets the pixel tolerance for comparisons
  pub fn with_tolerance(mut self, tolerance: u8) -> Self {
    self.pixel_tolerance = tolerance;
    self
  }

  /// Sets the maximum allowed difference percentage
  pub fn with_max_diff(mut self, max_diff: f64) -> Self {
    self.max_diff_percentage = max_diff;
    self
  }

  /// Enables fail-fast mode
  pub fn fail_fast(mut self) -> Self {
    self.fail_fast = true;
    self
  }

  /// Enables parallel execution
  pub fn parallel(mut self, workers: usize) -> Self {
    self.parallel = true;
    self.workers = workers;
    self
  }

  /// Sets a filter pattern for test names
  pub fn with_filter(mut self, filter: impl Into<String>) -> Self {
    self.filter = Some(filter.into());
    self
  }

  /// Enables updating expected images
  pub fn update_expected(mut self) -> Self {
    self.update_expected = true;
    self
  }
}

/// Compares two images and returns the difference
///
/// # Arguments
///
/// * `rendered` - The rendered image bytes (PNG)
/// * `expected` - The expected image bytes (PNG)
/// * `tolerance` - Per-channel tolerance (0-255)
///
/// # Returns
///
/// Tuple of (different_pixels, total_pixels, diff_percentage)
pub fn compare_images(
  rendered: &[u8],
  expected: &[u8],
  tolerance: u8,
) -> Result<(u64, u64, f64), String> {
  // Decode PNG images
  let rendered_img =
    decode_png(rendered).map_err(|e| format!("Failed to decode rendered image: {}", e))?;
  let expected_img =
    decode_png(expected).map_err(|e| format!("Failed to decode expected image: {}", e))?;

  // Check dimensions match
  if rendered_img.width != expected_img.width || rendered_img.height != expected_img.height {
    return Err(format!(
      "Image dimensions differ: rendered {}x{}, expected {}x{}",
      rendered_img.width, rendered_img.height, expected_img.width, expected_img.height
    ));
  }

  let total_pixels = (rendered_img.width * rendered_img.height) as u64;
  let mut diff_pixels = 0u64;

  // Compare pixel by pixel
  for (r, e) in rendered_img.data.iter().zip(expected_img.data.iter()) {
    let diff = if *r > *e { r - e } else { e - r };
    if diff > tolerance {
      diff_pixels += 1;
    }
  }

  // Each pixel has 4 components (RGBA), so divide by 4
  let diff_pixels = diff_pixels / 4;

  let diff_percentage = if total_pixels == 0 {
    0.0
  } else {
    (diff_pixels as f64 / total_pixels as f64) * 100.0
  };

  Ok((diff_pixels, total_pixels, diff_percentage))
}

/// Simple PNG image data structure
struct PngImage {
  width: u32,
  height: u32,
  data: Vec<u8>,
}

/// Decodes a PNG image from bytes
fn decode_png(data: &[u8]) -> Result<PngImage, String> {
  // Simple PNG decoding - in a real implementation, use image crate
  // For now, we'll do a simplified check
  if data.len() < 8 {
    return Err("Data too short for PNG".to_string());
  }

  // Check PNG signature
  let png_sig = [137u8, 80, 78, 71, 13, 10, 26, 10];
  if data[..8] != png_sig {
    return Err("Invalid PNG signature".to_string());
  }

  // For actual implementation, parse IHDR chunk for dimensions
  // For now, use placeholder dimensions
  // This is a simplified implementation - real code would use a PNG library

  // Read IHDR chunk (starts at byte 8)
  // Skip length (4 bytes), chunk type (4 bytes = IHDR)
  if data.len() < 24 {
    return Err("PNG too short to contain IHDR".to_string());
  }

  // IHDR is at offset 8 (after signature)
  // Format: length(4) + "IHDR"(4) + width(4) + height(4) + ...
  let width = u32::from_be_bytes([data[16], data[17], data[18], data[19]]);
  let height = u32::from_be_bytes([data[20], data[21], data[22], data[23]]);

  // For comparison purposes, we'll use the raw data after the header
  // This is simplified - real implementation would decompress the image data
  let data = data[33..].to_vec();

  Ok(PngImage {
    width,
    height,
    data,
  })
}

/// Generates a visual diff image between two images
///
/// # Arguments
///
/// * `rendered` - The rendered image bytes (PNG)
/// * `expected` - The expected image bytes (PNG)
///
/// # Returns
///
/// PNG bytes of the diff image where differences are highlighted
pub fn generate_diff_image(rendered: &[u8], expected: &[u8]) -> Result<Vec<u8>, String> {
  // This would generate an actual diff image
  // For now, return a placeholder
  let _rendered_img =
    decode_png(rendered).map_err(|e| format!("Failed to decode rendered image: {}", e))?;
  let _expected_img =
    decode_png(expected).map_err(|e| format!("Failed to decode expected image: {}", e))?;

  // In a real implementation, we would:
  // 1. Compare pixels
  // 2. Create a new image highlighting differences
  // 3. Encode as PNG

  // For now, return the rendered image as a placeholder
  Ok(rendered.to_vec())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_assertion_result_pass() {
    let result = AssertionResult::Pass;
    assert!(result.is_pass());
    assert!(!result.is_fail());
    assert!(!result.is_error());
    assert_eq!(format!("{}", result), "PASS");
  }

  #[test]
  fn test_assertion_result_fail() {
    let result = AssertionResult::Fail("Test failed".to_string());
    assert!(!result.is_pass());
    assert!(result.is_fail());
    assert!(!result.is_error());
    assert!(format!("{}", result).contains("FAIL"));
  }

  #[test]
  fn test_assertion_result_error() {
    let result = AssertionResult::Error("Runtime error".to_string());
    assert!(!result.is_pass());
    assert!(!result.is_fail());
    assert!(result.is_error());
    assert!(format!("{}", result).contains("ERROR"));
  }

  #[test]
  fn test_test_status_success() {
    assert!(TestStatus::Pass.is_success());
    assert!(TestStatus::Skip.is_success());
    assert!(!TestStatus::Fail.is_success());
    assert!(!TestStatus::Error.is_success());
    assert!(!TestStatus::Timeout.is_success());
  }

  #[test]
  fn test_test_status_failure() {
    assert!(!TestStatus::Pass.is_failure());
    assert!(!TestStatus::Skip.is_failure());
    assert!(TestStatus::Fail.is_failure());
    assert!(TestStatus::Error.is_failure());
    assert!(TestStatus::Timeout.is_failure());
  }

  #[test]
  fn test_test_type_from_path() {
    assert_eq!(
      TestType::from_path(Path::new("test-ref.html")),
      TestType::Reftest
    );
    assert_eq!(
      TestType::from_path(Path::new("crashtest.html")),
      TestType::Crashtest
    );
    assert_eq!(
      TestType::from_path(Path::new("manual-test.html")),
      TestType::Manual
    );
    assert_eq!(
      TestType::from_path(Path::new("normal-test.html")),
      TestType::Reftest
    );
  }

  #[test]
  fn test_test_metadata_from_path() {
    let path = PathBuf::from("/tests/wpt/css/box-model/test-001.html");
    let metadata = TestMetadata::from_path(path.clone());

    assert_eq!(metadata.id, "test-001");
    assert_eq!(metadata.path, path);
    assert_eq!(metadata.timeout_ms, 30000);
    assert!(!metadata.disabled);
  }

  #[test]
  fn test_test_metadata_with_viewport() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html")).with_viewport(1024, 768);
    assert_eq!(metadata.viewport_width, 1024);
    assert_eq!(metadata.viewport_height, 768);
  }

  #[test]
  fn test_test_metadata_with_timeout() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html")).with_timeout(5000);
    assert_eq!(metadata.timeout_ms, 5000);
  }

  #[test]
  fn test_test_metadata_disable() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html")).disable("Not implemented");
    assert!(metadata.disabled);
    assert_eq!(
      metadata.disabled_reason,
      Some("Not implemented".to_string())
    );
  }

  #[test]
  fn test_test_result_pass() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let result = TestResult::pass(metadata, Duration::from_millis(100));
    assert_eq!(result.status, TestStatus::Pass);
    assert_eq!(result.assertions.len(), 1);
    assert!(result.assertions[0].is_pass());
  }

  #[test]
  fn test_test_result_fail() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let result = TestResult::fail(metadata, Duration::from_millis(100), "Mismatch");
    assert_eq!(result.status, TestStatus::Fail);
    assert_eq!(result.message, Some("Mismatch".to_string()));
  }

  #[test]
  fn test_test_result_error() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let result = TestResult::error(metadata, Duration::from_millis(100), "Parse failed");
    assert_eq!(result.status, TestStatus::Error);
    assert_eq!(result.message, Some("Parse failed".to_string()));
  }

  #[test]
  fn test_test_result_skip() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let result = TestResult::skip(metadata, "Not implemented");
    assert_eq!(result.status, TestStatus::Skip);
    assert_eq!(result.duration, Duration::ZERO);
  }

  #[test]
  fn test_test_result_timeout() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let result = TestResult::timeout(metadata, Duration::from_secs(30));
    assert_eq!(result.status, TestStatus::Timeout);
    assert!(result.message.unwrap().contains("timed out"));
  }

  #[test]
  fn test_suite_result_counts() {
    let mut suite = SuiteResult::new("test-suite");

    suite.add_result(TestResult::pass(
      TestMetadata::from_path(PathBuf::from("test1.html")),
      Duration::from_millis(100),
    ));
    suite.add_result(TestResult::pass(
      TestMetadata::from_path(PathBuf::from("test2.html")),
      Duration::from_millis(100),
    ));
    suite.add_result(TestResult::fail(
      TestMetadata::from_path(PathBuf::from("test3.html")),
      Duration::from_millis(100),
      "Failed",
    ));
    suite.add_result(TestResult::skip(
      TestMetadata::from_path(PathBuf::from("test4.html")),
      "Skipped",
    ));

    assert_eq!(suite.total(), 4);
    assert_eq!(suite.passed(), 2);
    assert_eq!(suite.failed(), 1);
    assert_eq!(suite.skipped(), 1);
    assert_eq!(suite.errors(), 0);
    assert!(!suite.is_success());
    assert_eq!(suite.pass_rate(), 50.0);
  }

  #[test]
  fn test_suite_result_all_pass() {
    let mut suite = SuiteResult::new("passing-suite");

    suite.add_result(TestResult::pass(
      TestMetadata::from_path(PathBuf::from("test1.html")),
      Duration::from_millis(100),
    ));
    suite.add_result(TestResult::pass(
      TestMetadata::from_path(PathBuf::from("test2.html")),
      Duration::from_millis(100),
    ));

    assert!(suite.is_success());
    assert_eq!(suite.pass_rate(), 100.0);
  }

  #[test]
  fn test_harness_config_default() {
    let config = HarnessConfig::default();
    assert_eq!(config.pixel_tolerance, 0);
    assert_eq!(config.max_diff_percentage, 0.0);
    assert!(!config.fail_fast);
    assert!(!config.parallel);
  }

  #[test]
  fn test_harness_config_builder() {
    let config = HarnessConfig::with_test_dir("custom/tests")
      .with_tolerance(5)
      .with_max_diff(0.5)
      .fail_fast()
      .parallel(8)
      .with_filter("css");

    assert_eq!(config.test_dir, PathBuf::from("custom/tests"));
    assert_eq!(config.pixel_tolerance, 5);
    assert_eq!(config.max_diff_percentage, 0.5);
    assert!(config.fail_fast);
    assert!(config.parallel);
    assert_eq!(config.workers, 8);
    assert_eq!(config.filter, Some("css".to_string()));
  }

  #[test]
  fn test_harness_config_update_expected() {
    let config = HarnessConfig::default().update_expected();
    assert!(config.update_expected);
  }
}
