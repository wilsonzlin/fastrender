//! WPT Test Runner Integration Tests
//!
//! This file integrates the WPT test runner module into the test suite.

mod wpt;

// Re-export for test discovery
pub use wpt::*;

#[cfg(test)]
mod wpt_runner_tests {
  use super::wpt::DiscoveryMode;
  use super::wpt::HarnessConfig;
  use super::wpt::ImageComparisonResult;
  use super::wpt::RunnerStats;
  use super::wpt::SuiteResult;
  use super::wpt::TestMetadata;
  use super::wpt::TestResult;
  use super::wpt::TestStatus;
  use super::wpt::TestType;
  use super::wpt::WptRunner;
  use super::wpt::WptRunnerBuilder;
  use std::collections::HashMap;
  use std::path::Path;
  use std::path::PathBuf;
  use std::sync::Once;
  use std::time::Duration;
  use tempfile::TempDir;

  static SET_BUNDLED_FONTS: Once = Once::new();

  fn ensure_bundled_fonts() {
    SET_BUNDLED_FONTS.call_once(|| {
      std::env::set_var("FASTR_USE_BUNDLED_FONTS", "1");
    });
  }

  fn create_test_renderer() -> fastrender::FastRender {
    ensure_bundled_fonts();
    fastrender::FastRender::builder()
      .resource_policy(
        fastrender::ResourcePolicy::default()
          .allow_http(false)
          .allow_https(false),
      )
      .build()
      .unwrap()
  }

  // =========================================================================
  // WptRunner Tests
  // =========================================================================

  #[test]
  fn test_wpt_runner_new() {
    let renderer = create_test_renderer();
    let runner = WptRunner::new(renderer);

    assert_eq!(runner.stats().total, 0);
    assert_eq!(runner.config().pixel_tolerance, 0);
  }

  #[test]
  fn test_wpt_runner_with_config() {
    let renderer = create_test_renderer();
    let config = HarnessConfig::default()
      .with_tolerance(10)
      .with_max_diff(0.5);
    let runner = WptRunner::with_config(renderer, config);

    assert_eq!(runner.config().pixel_tolerance, 10);
    assert_eq!(runner.config().max_diff_percentage, 0.5);
  }

  #[test]
  fn test_wpt_runner_builder() {
    let renderer = create_test_renderer();
    let runner = WptRunnerBuilder::new()
      .renderer(renderer)
      .test_dir("custom/tests")
      .expected_dir("custom/expected")
      .output_dir("target/custom-output")
      .tolerance(5)
      .max_diff(1.0)
      .fail_fast()
      .save_rendered()
      .save_diffs()
      .parallel(2)
      .manifest("custom/manifest.toml")
      .discovery_mode(DiscoveryMode::ManifestOnly)
      .font_dir("fonts/ci-builder")
      .no_report()
      .build();

    assert_eq!(runner.config().test_dir, PathBuf::from("custom/tests"));
    assert_eq!(
      runner.config().expected_dir,
      PathBuf::from("custom/expected")
    );
    assert_eq!(
      runner.config().output_dir,
      PathBuf::from("target/custom-output")
    );
    assert_eq!(runner.config().pixel_tolerance, 5);
    assert_eq!(runner.config().max_diff_percentage, 1.0);
    assert!(runner.config().fail_fast);
    assert!(runner.config().save_rendered);
    assert!(runner.config().save_diffs);
    assert!(runner.config().parallel);
    assert_eq!(runner.config().workers, 2);
    assert_eq!(
      runner.config().manifest_path,
      Some(PathBuf::from("custom/manifest.toml"))
    );
    assert!(!runner.config().write_report);
    assert_eq!(runner.config().discovery_mode, DiscoveryMode::ManifestOnly);
    assert_eq!(
      runner.config().font_dirs,
      vec![PathBuf::from("fonts/ci-builder")]
    );
  }

  #[test]
  fn test_wpt_runner_stats_tracking() {
    let mut stats = RunnerStats::default();

    assert_eq!(stats.total, 0);
    assert_eq!(stats.passed, 0);
    assert_eq!(stats.failed, 0);
    assert_eq!(stats.pass_rate(), 100.0);

    stats.total = 10;
    stats.passed = 7;
    stats.failed = 2;
    stats.errors = 1;

    assert_eq!(stats.pass_rate(), 70.0);
  }

  #[test]
  fn test_wpt_runner_stats_reset() {
    let renderer = create_test_renderer();
    let mut runner = WptRunner::new(renderer);

    // Run some tests to populate stats
    let temp = TempDir::new().unwrap();
    std::fs::write(
      temp.path().join("crashtest.html"),
      "<!DOCTYPE html><html><body>Test</body></html>",
    )
    .unwrap();

    runner.run_test(&temp.path().join("crashtest.html"));
    assert!(runner.stats().total > 0);

    runner.reset_stats();
    assert_eq!(runner.stats().total, 0);
  }

  #[test]
  fn test_wpt_runner_empty_suite() {
    let renderer = create_test_renderer();
    let mut runner = WptRunner::new(renderer);

    let temp = TempDir::new().unwrap();
    let results = runner.run_suite(temp.path());

    assert!(results.is_empty());
  }

  #[test]
  fn test_wpt_runner_suite_with_tests() {
    let renderer = create_test_renderer();
    let mut runner = WptRunner::new(renderer);

    let temp = TempDir::new().unwrap();

    // Create crash tests (don't need reference files)
    std::fs::write(
      temp.path().join("crashtest1.html"),
      "<!DOCTYPE html><html><body><div>Test 1</div></body></html>",
    )
    .unwrap();
    std::fs::write(
      temp.path().join("crashtest2.html"),
      "<!DOCTYPE html><html><body><div>Test 2</div></body></html>",
    )
    .unwrap();

    let results = runner.run_suite(temp.path());

    assert_eq!(results.len(), 2);
  }

  #[test]
  fn test_wpt_runner_filter() {
    let renderer = create_test_renderer();
    let config = HarnessConfig::default().with_filter("box-model");
    let mut runner = WptRunner::with_config(renderer, config);

    let temp = TempDir::new().unwrap();

    // Create tests with different names
    std::fs::write(temp.path().join("box-model-test.html"), "<html></html>").unwrap();
    std::fs::write(temp.path().join("margin-test.html"), "<html></html>").unwrap();

    let results = runner.run_suite(temp.path());

    // margin-test should be filtered out (skipped)
    let skipped = results
      .iter()
      .filter(|r| r.status == TestStatus::Skip)
      .count();
    assert!(skipped >= 1);
  }

  #[test]
  fn test_wpt_runner_sidecar_discovery() {
    let renderer = create_test_renderer();
    let mut config = HarnessConfig::with_test_dir("tests/wpt/tests/discovery")
      .with_discovery_mode(DiscoveryMode::MetadataOnly);
    let temp = TempDir::new().unwrap();
    config.output_dir = temp.path().join("output");
    let mut runner = WptRunner::with_config(renderer, config);

    let results = runner.run_suite(Path::new("tests/wpt/tests/discovery"));
    let map: HashMap<_, _> = results
      .into_iter()
      .map(|r| (r.metadata.id.clone(), r.status))
      .collect();

    assert_eq!(map.get("link-match"), Some(&TestStatus::Pass));
    assert_eq!(map.get("link-mismatch"), Some(&TestStatus::Pass));
    assert_eq!(map.get("ini-expected-fail"), Some(&TestStatus::Pass));
    assert_eq!(map.get("ini-disabled"), Some(&TestStatus::Skip));
  }

  #[test]
  fn wpt_relative_stylesheet_loads_with_base_url() {
    ensure_bundled_fonts();
    let temp = TempDir::new().unwrap();
    let support_dir = temp.path().join("support");
    std::fs::create_dir_all(&support_dir).unwrap();

    std::fs::write(
      support_dir.join("style.css"),
      r#"body { margin: 0; }
.box { width: 100px; height: 100px; background: rgb(0, 255, 0); }"#,
    )
    .unwrap();

    std::fs::write(
      temp.path().join("test.html"),
      r#"<!doctype html>
<html>
  <head>
    <link rel="match" href="ref.html">
    <link rel="stylesheet" href="support/style.css">
  </head>
  <body><div class="box"></div></body>
</html>"#,
    )
    .unwrap();

    // Inline styles in the reference ensure this only passes if the linked stylesheet in the test
    // document is resolved relative to the test file (base_url set correctly).
    std::fs::write(
      temp.path().join("ref.html"),
      r#"<!doctype html>
<html>
  <head>
    <style>
      body { margin: 0; }
      .box { width: 100px; height: 100px; background: rgb(0, 255, 0); }
    </style>
  </head>
  <body><div class="box"></div></body>
</html>"#,
    )
    .unwrap();

    let mut runner = WptRunnerBuilder::new()
      .test_dir(temp.path())
      .expected_dir(temp.path().join("expected"))
      .output_dir(temp.path().join("out"))
      .no_report()
      .build();

    let result = runner.run_test(&temp.path().join("test.html"));
    assert_eq!(
      result.status,
      TestStatus::Pass,
      "expected PASS, got {:?}: {:?}",
      result.status,
      result.message
    );
  }

  #[test]
  fn wpt_reftest_base_url_isolated_per_document() {
    ensure_bundled_fonts();
    let temp = TempDir::new().unwrap();

    let test_dir = temp.path().join("test");
    let ref_dir = temp.path().join("ref");
    std::fs::create_dir_all(&test_dir).unwrap();
    std::fs::create_dir_all(ref_dir.join("support")).unwrap();

    // Test uses inline styles.
    std::fs::write(
      test_dir.join("test.html"),
      r#"<!doctype html>
<html>
  <head>
    <link rel="match" href="../ref/ref.html">
    <style>
      body { margin: 0; }
      .box { width: 100px; height: 100px; background: rgb(0, 255, 0); }
    </style>
  </head>
  <body><div class="box"></div></body>
</html>"#,
    )
    .unwrap();

    // Reference loads its stylesheet relative to *its own* directory.
    std::fs::write(
      ref_dir.join("support/style.css"),
      r#"body { margin: 0; }
.box { width: 100px; height: 100px; background: rgb(0, 255, 0); }"#,
    )
    .unwrap();
    std::fs::write(
      ref_dir.join("ref.html"),
      r#"<!doctype html>
<html>
  <head>
    <link rel="stylesheet" href="support/style.css">
  </head>
  <body><div class="box"></div></body>
</html>"#,
    )
    .unwrap();

    let mut runner = WptRunnerBuilder::new()
      .test_dir(temp.path())
      .expected_dir(temp.path().join("expected"))
      .output_dir(temp.path().join("out"))
      .no_report()
      .build();

    let result = runner.run_test(&test_dir.join("test.html"));
    assert_eq!(
      result.status,
      TestStatus::Pass,
      "expected PASS, got {:?}: {:?}",
      result.status,
      result.message
    );
  }

  #[test]
  fn wpt_runner_default_is_offline() {
    ensure_bundled_fonts();
    let temp = TempDir::new().unwrap();

    let css = r#"body { margin: 0; }
.box { width: 100px; height: 100px; background: rgb(0, 255, 0); }"#;

    let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();

    let saw_request = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
    let saw_request_thread = std::sync::Arc::clone(&saw_request);

    let server = std::thread::spawn(move || {
      use std::io::{Read, Write};
      use std::sync::atomic::Ordering;
      use std::time::{Duration, Instant};

      listener.set_nonblocking(true).unwrap();
      let start = Instant::now();
      while start.elapsed() < Duration::from_millis(500) {
        match listener.accept() {
          Ok((mut stream, _)) => {
            saw_request_thread.store(true, Ordering::SeqCst);
            let mut buf = [0u8; 1024];
            let _ = stream.read(&mut buf);
            let response = format!(
              "HTTP/1.1 200 OK\r\nContent-Type: text/css\r\nContent-Length: {}\r\n\r\n{}",
              css.len(),
              css
            );
            let _ = stream.write_all(response.as_bytes());
            return;
          }
          Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
            std::thread::sleep(Duration::from_millis(10));
          }
          Err(_) => return,
        }
      }
    });

    let css_url = format!("http://{addr}/style.css");

    std::fs::write(
      temp.path().join("test.html"),
      format!(
        r#"<!doctype html>
<html>
  <head>
    <link rel="match" href="ref.html">
    <link rel="stylesheet" href="{css_url}">
  </head>
  <body><div class="box"></div></body>
</html>"#
      ),
    )
    .unwrap();
    std::fs::write(
      temp.path().join("ref.html"),
      r#"<!doctype html>
<html>
  <head>
    <style>
      body { margin: 0; }
      .box { width: 100px; height: 100px; background: rgb(0, 255, 0); }
    </style>
  </head>
  <body><div class="box"></div></body>
</html>"#,
    )
    .unwrap();

    // Build without supplying a renderer. This should default to an offline renderer with
    // http/https disabled.
    let mut runner = WptRunnerBuilder::new()
      .test_dir(temp.path())
      .expected_dir(temp.path().join("expected"))
      .output_dir(temp.path().join("out"))
      .no_report()
      .build();

    let result = runner.run_test(&temp.path().join("test.html"));
    assert!(result.status.is_failure(), "expected failure, got {result:?}");

    server.join().unwrap();
    assert!(
      !saw_request.load(std::sync::atomic::Ordering::SeqCst),
      "offline policy should block HTTP requests"
    );
  }

  #[test]
  fn test_wpt_runner_suite_aggregated() {
    let renderer = create_test_renderer();
    let mut runner = WptRunner::new(renderer);

    let temp = TempDir::new().unwrap();

    std::fs::write(
      temp.path().join("crashtest1.html"),
      "<!DOCTYPE html><html><body>Test</body></html>",
    )
    .unwrap();
    std::fs::write(
      temp.path().join("crashtest2.html"),
      "<!DOCTYPE html><html><body>Test</body></html>",
    )
    .unwrap();

    let suite = runner.run_suite_aggregated(temp.path());

    assert_eq!(suite.total(), 2);
    assert!(suite.duration.as_nanos() > 0);
  }

  #[test]
  fn wpt_local_suite_passes() {
    std::thread::Builder::new()
      .stack_size(64 * 1024 * 1024)
      .spawn(|| {
        let renderer = create_test_renderer();
        let mut config = HarnessConfig::default();
        // The discovery directory under `tests/wpt/tests/` contains harness-focused metadata
        // fixtures (expected failures, disables, etc.). Keep the smoke-test suite focused on the
        // curated manifest entries so UPDATE_WPT_EXPECTED mode doesn't trip over those fixtures.
        config.discovery_mode = DiscoveryMode::ManifestOnly;
        config.expected_dir = PathBuf::from("target/wpt-expected");
        if std::env::var("UPDATE_WPT_EXPECTED").is_ok() {
          config = config.update_expected();
        } else {
          // Default to generating expected images into a temp dir so new tests
          // don't require checked-in PNGs to run locally.
          config.update_expected = true;
        }

        let mut runner = WptRunner::with_config(renderer, config);

        let results = runner.run_suite(Path::new("tests/wpt/tests"));
        assert!(!results.is_empty());

        for result in &results {
          assert!(
            !result.status.is_failure(),
            "{} failed with status {:?}",
            result.metadata.id,
            result.status
          );
        }
      })
      .unwrap()
      .join()
      .unwrap();
  }

  // =========================================================================
  // TestMetadata Tests
  // =========================================================================

  #[test]
  fn test_metadata_from_path() {
    let path = PathBuf::from("/path/to/test-001.html");
    let metadata = TestMetadata::from_path(path.clone());

    assert_eq!(metadata.id, "test-001");
    assert_eq!(metadata.path, path);
    assert_eq!(metadata.viewport_width, 800);
    assert_eq!(metadata.viewport_height, 600);
    assert_eq!(metadata.timeout_ms, 30000);
    assert!(!metadata.disabled);
  }

  #[test]
  fn test_metadata_with_viewport() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html")).with_viewport(1024, 768);

    assert_eq!(metadata.viewport_width, 1024);
    assert_eq!(metadata.viewport_height, 768);
  }

  #[test]
  fn test_metadata_with_timeout() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html")).with_timeout(5000);

    assert_eq!(metadata.timeout_ms, 5000);
  }

  #[test]
  fn test_metadata_disabled() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html")).disable("Not implemented");

    assert!(metadata.disabled);
    assert_eq!(
      metadata.disabled_reason,
      Some("Not implemented".to_string())
    );
  }

  #[test]
  fn test_metadata_expect_status() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html")).expect(TestStatus::Fail);

    assert_eq!(metadata.expected_status, Some(TestStatus::Fail));
  }

  // =========================================================================
  // TestType Tests
  // =========================================================================

  #[test]
  fn test_type_from_path_reftest() {
    use std::path::Path;

    assert_eq!(
      TestType::from_path(Path::new("test-ref.html")),
      TestType::Reftest
    );
    assert_eq!(
      TestType::from_path(Path::new("test-ref.htm")),
      TestType::Reftest
    );
  }

  #[test]
  fn test_type_from_path_crashtest() {
    use std::path::Path;

    assert_eq!(
      TestType::from_path(Path::new("crashtest.html")),
      TestType::Crashtest
    );
    assert_eq!(
      TestType::from_path(Path::new("crash-001.html")),
      TestType::Crashtest
    );
  }

  #[test]
  fn test_type_from_path_manual() {
    use std::path::Path;

    assert_eq!(
      TestType::from_path(Path::new("manual-test.html")),
      TestType::Manual
    );
  }

  // =========================================================================
  // TestResult Tests
  // =========================================================================

  #[test]
  fn test_result_pass() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let result = TestResult::pass(metadata, Duration::from_millis(100));

    assert_eq!(result.status, TestStatus::Pass);
    assert_eq!(result.duration, Duration::from_millis(100));
    assert!(result.message.is_none());
  }

  #[test]
  fn test_result_fail() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let result = TestResult::fail(metadata, Duration::from_millis(50), "Image mismatch");

    assert_eq!(result.status, TestStatus::Fail);
    assert_eq!(result.message, Some("Image mismatch".to_string()));
  }

  #[test]
  fn test_result_error() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let result = TestResult::error(metadata, Duration::from_millis(10), "Parse error");

    assert_eq!(result.status, TestStatus::Error);
    assert_eq!(result.message, Some("Parse error".to_string()));
  }

  #[test]
  fn test_result_skip() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let result = TestResult::skip(metadata, "Not supported");

    assert_eq!(result.status, TestStatus::Skip);
    assert_eq!(result.duration, Duration::ZERO);
    assert_eq!(result.message, Some("Not supported".to_string()));
  }

  #[test]
  fn test_result_timeout() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let result = TestResult::timeout(metadata, Duration::from_secs(30));

    assert_eq!(result.status, TestStatus::Timeout);
    assert!(result.message.unwrap().contains("timed out"));
  }

  #[test]
  fn test_result_with_images() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let result = TestResult::pass(metadata, Duration::from_millis(100))
      .with_images(vec![1, 2, 3], vec![4, 5, 6]);

    assert!(result.rendered_image.is_some());
    assert!(result.expected_image.is_some());
    assert_eq!(result.rendered_image.unwrap(), vec![1, 2, 3]);
    assert_eq!(result.expected_image.unwrap(), vec![4, 5, 6]);
  }

  #[test]
  fn test_result_with_diff() {
    let metadata = TestMetadata::from_path(PathBuf::from("test.html"));
    let comparison = ImageComparisonResult {
      diff_pixels: 100,
      total_pixels: 200,
      diff_percentage: 0.5,
      max_channel_diff: 10,
      samples: Vec::new(),
      dimensions: (10, 20),
    };
    let result = TestResult::pass(metadata, Duration::from_millis(100)).with_diff(&comparison);

    assert_eq!(result.pixel_diff, Some(100));
    assert_eq!(result.diff_percentage, Some(0.5));
    assert_eq!(result.max_channel_diff, Some(10));
    assert_eq!(result.image_dimensions, Some((10, 20)));
  }

  // =========================================================================
  // SuiteResult Tests
  // =========================================================================

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
    suite.add_result(TestResult::error(
      TestMetadata::from_path(PathBuf::from("test5.html")),
      Duration::from_millis(100),
      "Error",
    ));
    suite.finalize();

    assert_eq!(suite.total(), 5);
    assert_eq!(suite.passed(), 2);
    assert_eq!(suite.failed(), 1);
    assert_eq!(suite.skipped(), 1);
    assert_eq!(suite.errors(), 1);
    assert_eq!(suite.pass_rate(), 40.0); // 2 out of 5
  }

  #[test]
  fn test_suite_result_success() {
    let mut suite = SuiteResult::new("passing-suite");

    suite.add_result(TestResult::pass(
      TestMetadata::from_path(PathBuf::from("test1.html")),
      Duration::from_millis(100),
    ));
    suite.add_result(TestResult::skip(
      TestMetadata::from_path(PathBuf::from("test2.html")),
      "Skipped",
    ));

    assert!(suite.is_success()); // Pass and Skip are both success
  }

  #[test]
  fn test_suite_result_failure() {
    let mut suite = SuiteResult::new("failing-suite");

    suite.add_result(TestResult::pass(
      TestMetadata::from_path(PathBuf::from("test1.html")),
      Duration::from_millis(100),
    ));
    suite.add_result(TestResult::fail(
      TestMetadata::from_path(PathBuf::from("test2.html")),
      Duration::from_millis(100),
      "Failed",
    ));

    assert!(!suite.is_success()); // Has a failure
  }

  #[test]
  fn test_suite_result_display() {
    let mut suite = SuiteResult::new("test-suite");
    suite.add_result(TestResult::pass(
      TestMetadata::from_path(PathBuf::from("test.html")),
      Duration::from_millis(100),
    ));
    suite.finalize();

    let display = format!("{}", suite);
    assert!(display.contains("Suite: test-suite"));
    assert!(display.contains("Total: 1"));
    assert!(display.contains("Passed: 1"));
  }

  // =========================================================================
  // HarnessConfig Tests
  // =========================================================================

  #[test]
  fn test_harness_config_default() {
    let config = HarnessConfig::default();

    assert_eq!(config.test_dir, PathBuf::from("tests/wpt/tests"));
    assert_eq!(config.expected_dir, PathBuf::from("tests/wpt/expected"));
    assert_eq!(config.pixel_tolerance, 0);
    assert_eq!(config.max_diff_percentage, 0.0);
    assert_eq!(config.default_timeout_ms, 30000);
    assert!(!config.fail_fast);
    assert!(!config.parallel);
    assert!(!config.update_expected);
    assert!(config.manifest_path.is_none());
    assert!(config.write_report);
    assert_eq!(config.discovery_mode, DiscoveryMode::ManifestWithFallback);
    assert!(config.font_dirs.is_empty());
  }

  #[test]
  fn test_harness_config_with_test_dir() {
    let config = HarnessConfig::with_test_dir("custom/tests");

    assert_eq!(config.test_dir, PathBuf::from("custom/tests"));
    assert_eq!(config.expected_dir, PathBuf::from("custom/expected"));
  }

  #[test]
  fn test_harness_config_builder_methods() {
    let config = HarnessConfig::default()
      .with_tolerance(10)
      .with_max_diff(0.5)
      .fail_fast()
      .parallel(8)
      .with_filter("css")
      .update_expected()
      .with_manifest("custom/manifest.toml")
      .without_report()
      .with_discovery_mode(DiscoveryMode::MetadataOnly)
      .with_font_dir("fonts/ci");

    assert_eq!(config.pixel_tolerance, 10);
    assert_eq!(config.max_diff_percentage, 0.5);
    assert!(config.fail_fast);
    assert!(config.parallel);
    assert_eq!(config.workers, 8);
    assert_eq!(config.filter, Some("css".to_string()));
    assert!(config.update_expected);
    assert_eq!(
      config.manifest_path,
      Some(PathBuf::from("custom/manifest.toml"))
    );
    assert!(!config.write_report);
    assert_eq!(config.discovery_mode, DiscoveryMode::MetadataOnly);
    assert_eq!(config.font_dirs, vec![PathBuf::from("fonts/ci")]);
  }

  // =========================================================================
  // TestStatus Tests
  // =========================================================================

  #[test]
  fn test_status_success() {
    assert!(TestStatus::Pass.is_success());
    assert!(TestStatus::Skip.is_success());
    assert!(!TestStatus::Fail.is_success());
    assert!(!TestStatus::Error.is_success());
    assert!(!TestStatus::Timeout.is_success());
  }

  #[test]
  fn test_status_failure() {
    assert!(!TestStatus::Pass.is_failure());
    assert!(!TestStatus::Skip.is_failure());
    assert!(TestStatus::Fail.is_failure());
    assert!(TestStatus::Error.is_failure());
    assert!(TestStatus::Timeout.is_failure());
  }

  #[test]
  fn test_status_display() {
    assert_eq!(format!("{}", TestStatus::Pass), "PASS");
    assert_eq!(format!("{}", TestStatus::Fail), "FAIL");
    assert_eq!(format!("{}", TestStatus::Error), "ERROR");
    assert_eq!(format!("{}", TestStatus::Skip), "SKIP");
    assert_eq!(format!("{}", TestStatus::Timeout), "TIMEOUT");
  }

  // =========================================================================
  // AssertionResult Tests
  // =========================================================================

  #[test]
  fn test_assertion_result_variants() {
    use super::wpt::AssertionResult;

    let pass = AssertionResult::Pass;
    assert!(pass.is_pass());
    assert!(!pass.is_fail());
    assert!(!pass.is_error());

    let fail = AssertionResult::Fail("Failed".to_string());
    assert!(!fail.is_pass());
    assert!(fail.is_fail());
    assert!(!fail.is_error());

    let error = AssertionResult::Error("Error".to_string());
    assert!(!error.is_pass());
    assert!(!error.is_fail());
    assert!(error.is_error());
  }

  #[test]
  fn test_assertion_result_display() {
    use super::wpt::AssertionResult;

    assert_eq!(format!("{}", AssertionResult::Pass), "PASS");
    assert!(format!("{}", AssertionResult::Fail("msg".to_string())).contains("FAIL"));
    assert!(format!("{}", AssertionResult::Error("msg".to_string())).contains("ERROR"));
  }
}
