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

use image::RgbaImage;
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
  /// Device pixel ratio for the render
  pub device_pixel_ratio: f32,
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
      device_pixel_ratio: 1.0,
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

  /// Creates a new test metadata with custom device pixel ratio
  pub fn with_device_pixel_ratio(mut self, dpr: f32) -> Self {
    self.device_pixel_ratio = dpr;
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
  /// Optional manifest path for test discovery
  pub manifest_path: Option<PathBuf>,
  /// Whether to emit a human-readable report
  pub write_report: bool,
}

impl Default for HarnessConfig {
  fn default() -> Self {
    Self {
      test_dir: PathBuf::from("tests/wpt/tests"),
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
      manifest_path: None,
      write_report: true,
    }
  }
}

impl HarnessConfig {
  /// Creates a new configuration with custom test directory
  pub fn with_test_dir(test_dir: impl Into<PathBuf>) -> Self {
    let test_dir = test_dir.into();
    let expected_dir = test_dir
      .parent()
      .map(|p| p.join("expected"))
      .unwrap_or_else(|| test_dir.join("expected"));
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

  /// Sets a custom manifest path for discovery
  pub fn with_manifest(mut self, manifest: impl Into<PathBuf>) -> Self {
    self.manifest_path = Some(manifest.into());
    self
  }

  /// Disables report generation
  pub fn without_report(mut self) -> Self {
    self.write_report = false;
    self
  }
}

/// Compares two images and returns the difference metrics.
///
/// # Returns
///
/// Tuple of (different_pixels, total_pixels, diff_percentage)
pub fn compare_images(
  rendered: &[u8],
  expected: &[u8],
  tolerance: u8,
  max_diff_percentage: Option<f64>,
) -> Result<(u64, u64, f64), String> {
  // Decode PNG images
  let rendered_img =
    decode_png(rendered).map_err(|e| format!("Failed to decode rendered image: {}", e))?;
  let expected_img =
    decode_png(expected).map_err(|e| format!("Failed to decode expected image: {}", e))?;

  // Check dimensions match
  let (rendered_width, rendered_height) = rendered_img.dimensions();
  let (expected_width, expected_height) = expected_img.dimensions();
  if rendered_width != expected_width || rendered_height != expected_height {
    return Err(format!(
      "Image dimensions differ: rendered {}x{}, expected {}x{}",
      rendered_width, rendered_height, expected_width, expected_height
    ));
  }

  let total_pixels = (rendered_width as u64) * (rendered_height as u64);
  let mut diff_pixels = 0u64;

  // Precompute allowed differing pixels for early exit if threshold provided and > 0.
  let stop_after = match max_diff_percentage {
    Some(max_pct) if max_pct > 0.0 => {
      let allowed = ((max_pct / 100.0) * total_pixels as f64).ceil() as u64;
      Some(allowed)
    }
    _ => None,
  };

  let tolerance = tolerance as i16;
  let rendered_data = rendered_img.as_raw();
  let expected_data = expected_img.as_raw();

  for (rendered_px, expected_px) in rendered_data
    .chunks_exact(4)
    .zip(expected_data.chunks_exact(4))
  {
    let differs = rendered_px
      .iter()
      .zip(expected_px.iter())
      .any(|(&r, &e)| (r as i16 - e as i16).abs() > tolerance);

    if differs {
      diff_pixels += 1;

      if let Some(limit) = stop_after {
        if diff_pixels > limit {
          break;
        }
      }
    }
  }

  let diff_percentage = if total_pixels == 0 {
    0.0
  } else {
    (diff_pixels as f64 / total_pixels as f64) * 100.0
  };

  Ok((diff_pixels, total_pixels, diff_percentage))
}

/// Decodes a PNG image from bytes into an RGBA buffer
fn decode_png(data: &[u8]) -> Result<RgbaImage, image::ImageError> {
  image::load_from_memory_with_format(data, image::ImageFormat::Png).map(|img| img.to_rgba8())
}

/// Generates a visual diff image between two images
///
/// # Arguments
///
/// * `rendered` - The rendered image bytes (PNG)
/// * `expected` - The expected image bytes (PNG)
/// * `tolerance` - Per-channel tolerance for considering pixels different
///
/// # Returns
///
/// PNG bytes of the diff image where differences are highlighted
pub fn generate_diff_image(
  rendered: &[u8],
  expected: &[u8],
  tolerance: u8,
) -> Result<Vec<u8>, String> {
  fastrender::image_output::diff_png(rendered, expected, tolerance)
    .map(|(_, diff)| diff)
    .map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
  use super::*;
  use image::codecs::png::PngEncoder;
  use image::{ColorType, ImageEncoder, Rgba, RgbaImage};

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
    assert!((metadata.device_pixel_ratio - 1.0).abs() < f32::EPSILON);
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
  fn test_test_metadata_with_dpr() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html")).with_device_pixel_ratio(2.0);
    assert!((metadata.device_pixel_ratio - 2.0).abs() < f32::EPSILON);
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
    assert!(config.manifest_path.is_none());
    assert!(config.write_report);
  }

  #[test]
  fn test_harness_config_builder() {
    let config = HarnessConfig::with_test_dir("custom/tests")
      .with_tolerance(5)
      .with_max_diff(0.5)
      .fail_fast()
      .parallel(8)
      .with_filter("css")
      .with_manifest("manifest.json")
      .without_report();

    assert_eq!(config.test_dir, PathBuf::from("custom/tests"));
    assert_eq!(config.pixel_tolerance, 5);
    assert_eq!(config.max_diff_percentage, 0.5);
    assert!(config.fail_fast);
    assert!(config.parallel);
    assert_eq!(config.workers, 8);
    assert_eq!(config.filter, Some("css".to_string()));
    assert_eq!(config.manifest_path, Some(PathBuf::from("manifest.json")));
    assert!(!config.write_report);
  }

  #[test]
  fn test_harness_config_update_expected() {
    let config = HarnessConfig::default().update_expected();
    assert!(config.update_expected);
  }

  fn encode_png(image: &RgbaImage) -> Vec<u8> {
    let mut buffer = Vec::new();
    PngEncoder::new(&mut buffer)
      .write_image(
        image.as_raw(),
        image.width(),
        image.height(),
        ColorType::Rgba8.into(),
      )
      .unwrap();
    buffer
  }

  fn solid_png(width: u32, height: u32, color: [u8; 4]) -> Vec<u8> {
    let image = RgbaImage::from_pixel(width, height, Rgba(color));
    encode_png(&image)
  }

  #[test]
  fn test_compare_images_identical() {
    let png = solid_png(2, 2, [10, 20, 30, 255]);
    let (diff_pixels, total_pixels, diff_percentage) = compare_images(&png, &png, 0, None).unwrap();

    assert_eq!(diff_pixels, 0);
    assert_eq!(total_pixels, 4);
    assert_eq!(diff_percentage, 0.0);
  }

  #[test]
  fn test_compare_images_single_pixel_difference() {
    let mut rendered_img = RgbaImage::from_pixel(2, 2, Rgba([0, 0, 0, 255]));
    rendered_img.put_pixel(1, 1, Rgba([255, 0, 0, 255]));

    let expected_png = solid_png(2, 2, [0, 0, 0, 255]);
    let rendered_png = encode_png(&rendered_img);

    let (diff_pixels, total_pixels, diff_percentage) =
      compare_images(&rendered_png, &expected_png, 0, None).unwrap();

    assert_eq!(diff_pixels, 1);
    assert_eq!(total_pixels, 4);
    assert!((diff_percentage - 25.0).abs() < f64::EPSILON);
  }

  #[test]
  fn test_compare_images_respects_tolerance() {
    let expected_png = solid_png(1, 1, [10, 10, 10, 255]);

    let mut rendered_img = RgbaImage::from_pixel(1, 1, Rgba([10, 10, 10, 255]));
    rendered_img.put_pixel(0, 0, Rgba([12, 10, 10, 255]));
    let rendered_png = encode_png(&rendered_img);

    let (diff_pixels, _total_pixels, diff_percentage) =
      compare_images(&rendered_png, &expected_png, 5, None).unwrap();

    assert_eq!(diff_pixels, 0);
    assert_eq!(diff_percentage, 0.0);
  }

  #[test]
  fn test_compare_images_dimension_mismatch() {
    let small = solid_png(1, 1, [0, 0, 0, 255]);
    let large = solid_png(2, 1, [0, 0, 0, 255]);

    let err = compare_images(&small, &large, 0, None).unwrap_err();
    assert!(err.contains("dimensions differ"));
  }

  #[test]
  fn test_generate_diff_image_marks_differences() {
    let rendered = solid_png(1, 1, [255, 255, 255, 255]);
    let expected = solid_png(1, 1, [0, 0, 0, 255]);

    let diff_png = generate_diff_image(&rendered, &expected, 0).unwrap();
    let diff_image = decode_png(&diff_png).unwrap();

    assert_eq!(diff_image.dimensions(), (1, 1));
    assert_eq!(*diff_image.get_pixel(0, 0), Rgba([255, 0, 0, 255]));
  }

  #[test]
  fn test_compare_images_with_max_diff_early_exit() {
    let expected = solid_png(3, 3, [0, 0, 0, 255]);

    let mut rendered_img = RgbaImage::from_pixel(3, 3, Rgba([0, 0, 0, 255]));
    // Flip five pixels to white
    for &(x, y) in &[(0, 0), (1, 0), (2, 0), (0, 1), (1, 1)] {
      rendered_img.put_pixel(x, y, Rgba([255, 255, 255, 255]));
    }
    let rendered = encode_png(&rendered_img);

    let (full_diff, _, _) = compare_images(&rendered, &expected, 0, None).unwrap();
    assert_eq!(full_diff, 5);

    // With a 10% max diff threshold (ceil(0.1 * 9) = 1 allowed), we should early exit
    // once we exceed the allowance. The reported diff count may be lower than the true
    // count but must still exceed the threshold percentage.
    let (limited_diff, _, limited_pct) =
      compare_images(&rendered, &expected, 0, Some(10.0)).unwrap();
    assert!(limited_diff <= full_diff);
    assert!(limited_pct > 10.0);
  }
}
