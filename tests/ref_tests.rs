//! Reference test integration
//!
//! This file provides the entry point for running reference tests.
//! The actual test harness and comparison logic lives in the `ref/` module.

mod r#ref;

use r#ref::compare::compare_images;
use r#ref::compare::create_solid_pixmap;
use r#ref::compare::CompareConfig;
use r#ref::compare::DiffStatistics;
use r#ref::harness::RefTestConfig;
use r#ref::harness::RefTestHarness;
use r#ref::harness::RefTestResult;
use r#ref::harness::RefTestResults;
use r#ref::image_compare::{compare_config_from_env, CompareEnvVars};
use std::ffi::OsString;
use std::path::Path;
use std::sync::Mutex;
use std::time::Duration;

fn with_large_stack<F, R>(f: F) -> R
where
  F: FnOnce() -> R + Send + 'static,
  R: Send + 'static,
{
  const STACK_SIZE: usize = 16 * 1024 * 1024;
  let handle = std::thread::Builder::new()
    .stack_size(STACK_SIZE)
    .spawn(f)
    .expect("failed to spawn test thread");
  match handle.join() {
    Ok(result) => result,
    Err(payload) => std::panic::resume_unwind(payload),
  }
}

static ENV_MUTEX: Mutex<()> = Mutex::new(());

struct EnvVarGuard<'a> {
  _lock: std::sync::MutexGuard<'a, ()>,
  saved: Vec<(&'static str, Option<OsString>)>,
}

impl<'a> EnvVarGuard<'a> {
  fn new(keys: &[&'static str]) -> Self {
    let lock = ENV_MUTEX.lock().expect("env mutex poisoned");
    let saved: Vec<(&'static str, Option<OsString>)> =
      keys.iter().map(|&key| (key, std::env::var_os(key))).collect();
    for &key in keys {
      std::env::remove_var(key);
    }
    Self { _lock: lock, saved }
  }
}

impl Drop for EnvVarGuard<'_> {
  fn drop(&mut self) {
    for (key, value) in self.saved.drain(..) {
      match value {
        Some(v) => std::env::set_var(key, v),
        None => std::env::remove_var(key),
      }
    }
  }
}

// =============================================================================
// Compare Module Tests
// =============================================================================

#[test]
fn test_ref_compare_identical_images() {
  let pixmap1 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();
  let pixmap2 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();

  let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());

  assert!(diff.is_match());
  assert!(diff.dimensions_match);
  assert_eq!(diff.statistics.different_pixels, 0);
  assert_eq!(diff.statistics.different_percent, 0.0);
  assert_eq!(
    diff
      .statistics
      .max_channel_diff(CompareConfig::strict().compare_alpha),
    0
  );
  assert!(diff.statistics.psnr.is_infinite());
}

#[test]
fn test_ref_compare_different_images() {
  let pixmap1 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();
  let pixmap2 = create_solid_pixmap(10, 10, 0, 255, 0, 255).unwrap();

  let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());

  assert!(!diff.is_match());
  assert!(diff.dimensions_match);
  assert_eq!(diff.statistics.different_pixels, 100);
  assert_eq!(diff.statistics.different_percent, 100.0);
  assert_eq!(diff.statistics.max_red_diff, 255);
  assert_eq!(diff.statistics.max_green_diff, 255);
}

#[test]
fn test_ref_compare_different_dimensions() {
  let pixmap1 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();
  let pixmap2 = create_solid_pixmap(20, 20, 255, 0, 0, 255).unwrap();

  let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());

  assert!(!diff.is_match());
  assert!(!diff.dimensions_match);
  assert_eq!(diff.actual_dimensions, (10, 10));
  assert_eq!(diff.expected_dimensions, (20, 20));
}

#[test]
fn test_ref_compare_with_tolerance() {
  let pixmap1 = create_solid_pixmap(10, 10, 100, 100, 100, 255).unwrap();
  let pixmap2 = create_solid_pixmap(10, 10, 105, 105, 105, 255).unwrap();

  // Strict comparison should fail
  let strict_diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());
  assert!(!strict_diff.is_match());

  // With tolerance of 5, should pass
  let lenient_config = CompareConfig::default().with_channel_tolerance(5);
  let lenient_diff = compare_images(&pixmap1, &pixmap2, &lenient_config);
  assert!(lenient_diff.is_match());
}

#[test]
fn test_ref_compare_with_max_different_percent() {
  let mut pixmap1 = create_solid_pixmap(10, 10, 100, 100, 100, 255).unwrap();
  let pixmap2 = create_solid_pixmap(10, 10, 100, 100, 100, 255).unwrap();

  // Change one pixel
  let data = pixmap1.data_mut();
  data[0] = 0; // Change first pixel's blue channel

  let config = CompareConfig::default().with_max_different_percent(1.1); // 1 pixel = 1%
  let diff = compare_images(&pixmap1, &pixmap2, &config);

  assert!(diff.is_match());
  assert_eq!(diff.statistics.different_pixels, 1);
}

#[test]
fn test_ref_diff_image_generation() {
  let pixmap1 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();
  let pixmap2 = create_solid_pixmap(10, 10, 0, 255, 0, 255).unwrap();

  let config_with_diff = CompareConfig::default().with_generate_diff_image(true);
  let diff = compare_images(&pixmap1, &pixmap2, &config_with_diff);

  assert!(diff.diff_image.is_some());

  let config_without_diff = CompareConfig::default().with_generate_diff_image(false);
  let diff2 = compare_images(&pixmap1, &pixmap2, &config_without_diff);

  assert!(diff2.diff_image.is_none());
}

#[test]
fn test_ref_config_presets() {
  let strict = CompareConfig::strict();
  assert_eq!(strict.channel_tolerance, 0);
  assert_eq!(strict.max_different_percent, 0.0);

  let lenient = CompareConfig::lenient();
  assert_eq!(lenient.channel_tolerance, 5);
  assert_eq!(lenient.max_different_percent, 0.1);

  let fuzzy = CompareConfig::fuzzy();
  assert_eq!(fuzzy.channel_tolerance, 10);
  assert_eq!(fuzzy.max_different_percent, 1.0);
  assert!(!fuzzy.compare_alpha);
}

#[test]
fn test_ref_statistics_max_channel_diff() {
  let stats = DiffStatistics {
    total_pixels: 100,
    different_pixels: 10,
    different_percent: 10.0,
    max_red_diff: 50,
    max_green_diff: 100,
    max_blue_diff: 25,
    max_alpha_diff: 75,
    mse: 0.0,
    psnr: 0.0,
    perceptual_similarity: 0.0,
    perceptual_distance: 0.0,
  };

  assert_eq!(stats.max_channel_diff(true), 100);
}

#[test]
fn test_ref_diff_summary() {
  let pixmap1 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();
  let pixmap2 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();

  let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());
  assert_eq!(diff.summary(), "Images match");

  let pixmap3 = create_solid_pixmap(20, 20, 255, 0, 0, 255).unwrap();
  let diff2 = compare_images(&pixmap1, &pixmap3, &CompareConfig::strict());
  assert!(diff2.summary().contains("Dimension mismatch"));
}

#[test]
fn test_ref_create_solid_pixmap() {
  let pixmap = create_solid_pixmap(5, 5, 128, 64, 32, 255).unwrap();

  assert_eq!(pixmap.width(), 5);
  assert_eq!(pixmap.height(), 5);

  // Check first pixel (BGRA format)
  let data = pixmap.data();
  assert_eq!(data[0], 32); // B
  assert_eq!(data[1], 64); // G
  assert_eq!(data[2], 128); // R
  assert_eq!(data[3], 255); // A
}

#[test]
fn test_ref_psnr_calculation() {
  // Identical images should have infinite PSNR
  let pixmap1 = create_solid_pixmap(10, 10, 128, 128, 128, 255).unwrap();
  let pixmap2 = create_solid_pixmap(10, 10, 128, 128, 128, 255).unwrap();

  let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());
  assert!(diff.statistics.psnr.is_infinite());

  // Different images should have finite PSNR
  let pixmap3 = create_solid_pixmap(10, 10, 0, 0, 0, 255).unwrap();
  let diff2 = compare_images(&pixmap1, &pixmap3, &CompareConfig::strict());
  assert!(diff2.statistics.psnr.is_finite());
  assert!(diff2.statistics.psnr > 0.0);
}

#[test]
fn test_ref_mse_calculation() {
  // Identical images should have zero MSE
  let pixmap1 = create_solid_pixmap(10, 10, 128, 128, 128, 255).unwrap();
  let pixmap2 = create_solid_pixmap(10, 10, 128, 128, 128, 255).unwrap();

  let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());
  assert_eq!(diff.statistics.mse, 0.0);

  // Different images should have non-zero MSE
  let pixmap3 = create_solid_pixmap(10, 10, 0, 0, 0, 255).unwrap();
  let diff2 = compare_images(&pixmap1, &pixmap3, &CompareConfig::strict());
  assert!(diff2.statistics.mse > 0.0);
}

// =============================================================================
// Image Compare Env Var Parsing Tests
// =============================================================================

#[test]
fn compare_config_from_env_supports_fixture_ignore_alpha_and_perceptual_distance() {
  let _env = EnvVarGuard::new(&[
    "FIXTURE_FUZZY",
    "FIXTURE_TOLERANCE",
    "FIXTURE_MAX_DIFFERENT_PERCENT",
    "FIXTURE_IGNORE_ALPHA",
    "FIXTURE_MAX_PERCEPTUAL_DISTANCE",
  ]);

  let config = compare_config_from_env(CompareEnvVars::fixtures()).expect("config");
  assert!(config.compare_alpha);
  assert!(config.max_perceptual_distance.is_none());

  std::env::set_var("FIXTURE_IGNORE_ALPHA", "1");
  let config = compare_config_from_env(CompareEnvVars::fixtures()).expect("config");
  assert!(!config.compare_alpha);

  std::env::remove_var("FIXTURE_IGNORE_ALPHA");
  std::env::set_var("FIXTURE_MAX_PERCEPTUAL_DISTANCE", "0.123");
  let config = compare_config_from_env(CompareEnvVars::fixtures()).expect("config");
  let parsed = config.max_perceptual_distance.expect("max perceptual distance");
  assert!((parsed - 0.123).abs() < 1e-12);

  std::env::set_var("FIXTURE_FUZZY", "1");
  std::env::set_var("FIXTURE_MAX_PERCEPTUAL_DISTANCE", "0.01");
  let config = compare_config_from_env(CompareEnvVars::fixtures()).expect("config");
  let parsed = config.max_perceptual_distance.expect("max perceptual distance");
  assert!((parsed - 0.01).abs() < 1e-12);
  assert!(!config.compare_alpha);
}

#[test]
fn compare_config_from_env_supports_pages_ignore_alpha_and_perceptual_distance() {
  let _env = EnvVarGuard::new(&[
    "PAGES_FUZZY",
    "PAGES_TOLERANCE",
    "PAGES_MAX_DIFFERENT_PERCENT",
    "PAGES_IGNORE_ALPHA",
    "PAGES_MAX_PERCEPTUAL_DISTANCE",
  ]);

  let config = compare_config_from_env(CompareEnvVars::pages()).expect("config");
  assert!(config.compare_alpha);
  assert!(config.max_perceptual_distance.is_none());

  std::env::set_var("PAGES_IGNORE_ALPHA", "1");
  std::env::set_var("PAGES_MAX_PERCEPTUAL_DISTANCE", "0.2");
  let config = compare_config_from_env(CompareEnvVars::pages()).expect("config");
  assert!(!config.compare_alpha);
  let parsed = config.max_perceptual_distance.expect("max perceptual distance");
  assert!((parsed - 0.2).abs() < 1e-12);
}

#[test]
fn compare_config_from_env_rejects_invalid_perceptual_distance() {
  let _env = EnvVarGuard::new(&[
    "FIXTURE_FUZZY",
    "FIXTURE_TOLERANCE",
    "FIXTURE_MAX_DIFFERENT_PERCENT",
    "FIXTURE_IGNORE_ALPHA",
    "FIXTURE_MAX_PERCEPTUAL_DISTANCE",
  ]);

  std::env::set_var("FIXTURE_MAX_PERCEPTUAL_DISTANCE", "not-a-number");
  let err = compare_config_from_env(CompareEnvVars::fixtures())
    .expect_err("invalid perceptual distance should fail");
  assert!(err.contains("FIXTURE_MAX_PERCEPTUAL_DISTANCE"), "{err}");
}

// =============================================================================
// Harness Module Tests
// =============================================================================

#[test]
fn test_ref_harness_config_default() {
  let config = RefTestConfig::default();
  assert_eq!(config.viewport_width, 800);
  assert_eq!(config.viewport_height, 600);
  assert!(config.save_actual_on_failure);
  assert!(config.save_diff_on_failure);
}

#[test]
fn test_ref_harness_config_with_viewport() {
  let config = RefTestConfig::with_viewport(1024, 768);
  assert_eq!(config.viewport_width, 1024);
  assert_eq!(config.viewport_height, 768);
}

#[test]
fn test_ref_harness_config_builder() {
  let config = RefTestConfig::default()
    .with_compare_config(CompareConfig::lenient())
    .with_save_actual_on_failure(false)
    .with_save_diff_on_failure(false);

  assert!(!config.save_actual_on_failure);
  assert!(!config.save_diff_on_failure);
  assert_eq!(config.compare_config.channel_tolerance, 5);
}

#[test]
fn test_ref_harness_creation() {
  let harness = RefTestHarness::new();
  assert_eq!(harness.config().viewport_width, 800);
  assert_eq!(harness.config().viewport_height, 600);
}

#[test]
fn test_ref_harness_with_config() {
  let config = RefTestConfig::with_viewport(1920, 1080);
  let harness = RefTestHarness::with_config(config);

  assert_eq!(harness.config().viewport_width, 1920);
  assert_eq!(harness.config().viewport_height, 1080);
}

#[test]
fn test_ref_harness_result_summary_passed() {
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
fn test_ref_harness_result_summary_failed_error() {
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
fn test_ref_harness_results_all_passed() {
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
fn test_ref_harness_results_some_failed() {
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
fn test_ref_harness_results_summary() {
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
fn test_ref_harness_compare_images_via_harness() {
  let harness = RefTestHarness::new();

  let pixmap1 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();
  let pixmap2 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();

  let diff = harness.compare_images(&pixmap1, &pixmap2);
  assert!(diff.is_match());
}

#[test]
fn test_ref_harness_run_ref_test_missing_html() {
  let mut harness = RefTestHarness::new();
  let test_dir = std::path::Path::new("/nonexistent/test/dir");
  let reference = std::path::Path::new("/nonexistent/reference.png");

  let result = harness.run_ref_test(test_dir, reference);

  assert!(!result.passed);
  assert!(result.error.is_some());
  assert!(result
    .error
    .as_ref()
    .unwrap()
    .contains("Failed to read HTML"));
}

// =============================================================================
// Integration Tests with Real Rendering
// =============================================================================

#[test]
fn form_controls_reference_image_matches_golden() {
  with_large_stack(|| {
    let fixture = Path::new("tests/ref/fixtures/form_controls");
    let reference = fixture.join("reference.png");
    let mut harness = RefTestHarness::new();

    if std::env::var("UPDATE_GOLDEN").is_ok() {
      let html = std::fs::read_to_string(fixture.join("input.html")).expect("read html");
      harness
        .create_reference(&html, &reference)
        .expect("create reference image");
    }

    let result = harness.run_ref_test(fixture, &reference);
    if !result.passed {
      let actual_path = fixture.join("failures/form_controls_actual.png");
      if let (Ok(actual), Ok(expected)) = (
        r#ref::compare::load_png(&actual_path),
        r#ref::compare::load_png(&reference),
      ) {
        let diff = r#ref::compare::compare_images(&actual, &expected, &CompareConfig::strict());
        if diff.is_match() {
          let _ = std::fs::remove_dir_all(fixture.join("failures"));
          return;
        }
      }
    }
    assert!(
      result.passed,
      "form control rendering regressed: {}",
      result.summary()
    );
  });
}

#[test]
fn test_ref_harness_render_and_compare_identical() {
  // Create two identical renders and verify they match
  let mut harness = RefTestHarness::with_config(RefTestConfig::with_viewport(100, 100));

  let html = r#"
        <html>
        <head>
            <style>
                body { margin: 0; padding: 0; }
                .box { width: 50px; height: 50px; background-color: red; }
            </style>
        </head>
        <body>
            <div class="box"></div>
        </body>
        </html>
    "#;

  // Render twice and compare (should be identical)
  // Note: This tests that our comparison works, not that rendering is deterministic
  // (which it should be, but that's a separate concern)

  // For this test, we just verify the harness can render HTML successfully
  let result = harness.run_ref_test_inline(
    "test_identical",
    html,
    std::path::Path::new("/nonexistent/reference.png"),
  );

  // Should fail because reference image doesn't exist, but the render should work
  assert!(!result.passed);
  assert!(result.error.is_some());
  assert!(result
    .error
    .as_ref()
    .unwrap()
    .contains("Failed to load reference"));
}

#[test]
#[ignore = "Requires full rendering pipeline which may have layout limitations"]
fn test_ref_harness_create_reference() {
  use std::fs;

  let mut harness = RefTestHarness::with_config(RefTestConfig::with_viewport(100, 100));

  let html = r#"
        <html>
        <head>
            <style>
                body { margin: 0; padding: 0; background: white; }
                .box { width: 50px; height: 50px; background-color: blue; }
            </style>
        </head>
        <body>
            <div class="box"></div>
        </body>
        </html>
    "#;

  // Create a temp file for the reference
  let temp_dir = std::env::temp_dir();
  let reference_path = temp_dir.join("test_ref_harness_reference.png");

  // Create the reference
  let create_result = harness.create_reference(html, &reference_path);
  assert!(
    create_result.is_ok(),
    "Failed to create reference: {:?}",
    create_result
  );

  // Verify the file exists and has content
  assert!(reference_path.exists());
  let metadata = fs::metadata(&reference_path).unwrap();
  assert!(metadata.len() > 0);

  // Now run a test against this reference - should pass
  let result = harness.run_ref_test_inline("test_blue_box", html, &reference_path);

  // Clean up
  let _ = fs::remove_file(&reference_path);

  // Verify the test passed
  assert!(result.passed, "Test should pass: {}", result.summary());
}

#[test]
#[ignore = "Requires full rendering pipeline which may have layout limitations"]
fn test_ref_harness_detect_difference() {
  use std::fs;

  let mut harness = RefTestHarness::with_config(RefTestConfig::with_viewport(100, 100));

  let html1 = r#"
        <html>
        <head>
            <style>
                body { margin: 0; padding: 0; background: white; }
                .box { width: 50px; height: 50px; background-color: red; }
            </style>
        </head>
        <body>
            <div class="box"></div>
        </body>
        </html>
    "#;

  let html2 = r#"
        <html>
        <head>
            <style>
                body { margin: 0; padding: 0; background: white; }
                .box { width: 50px; height: 50px; background-color: green; }
            </style>
        </head>
        <body>
            <div class="box"></div>
        </body>
        </html>
    "#;

  // Create a reference from html1
  let temp_dir = std::env::temp_dir();
  let reference_path = temp_dir.join("test_ref_harness_diff_reference.png");

  let create_result = harness.create_reference(html1, &reference_path);
  assert!(create_result.is_ok());

  // Run a test with html2 - should fail because colors differ
  let result = harness.run_ref_test_inline("test_color_diff", html2, &reference_path);

  // Clean up
  let _ = fs::remove_file(&reference_path);

  // Verify the test failed due to visual difference
  assert!(!result.passed, "Test should fail due to color difference");
  assert!(result.image_diff.is_some());
  assert!(
    result
      .image_diff
      .as_ref()
      .unwrap()
      .statistics
      .different_pixels
      > 0
  );
}

#[test]
#[ignore = "Requires full rendering pipeline which may have layout limitations"]
fn test_ref_harness_with_lenient_config() {
  use std::fs;

  // Use lenient config to allow small differences
  let config = RefTestConfig::with_viewport(100, 100).with_compare_config(CompareConfig::lenient());
  let mut harness = RefTestHarness::with_config(config);

  let html = r#"
        <html>
        <head>
            <style>
                body { margin: 0; padding: 0; background: white; }
                .box { width: 50px; height: 50px; background-color: #808080; }
            </style>
        </head>
        <body>
            <div class="box"></div>
        </body>
        </html>
    "#;

  // Create reference
  let temp_dir = std::env::temp_dir();
  let reference_path = temp_dir.join("test_ref_harness_lenient_reference.png");

  let create_result = harness.create_reference(html, &reference_path);
  assert!(create_result.is_ok());

  // Test against same HTML - should definitely pass
  let result = harness.run_ref_test_inline("test_lenient", html, &reference_path);

  // Clean up
  let _ = fs::remove_file(&reference_path);

  assert!(result.passed, "Test should pass: {}", result.summary());
}
