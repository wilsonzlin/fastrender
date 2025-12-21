//! WPT-style harness for FastRender.
//!
//! This is an internal “render and compare” test harness. It is not a full integration
//! with the upstream Web Platform Tests suite and is expected to evolve alongside the
//! renderer internals.
//!
//! For repo-level guidance on how this fits into testing workflows, see `docs/testing.md`.

pub mod harness;
pub mod runner;

// Re-export main types for convenience
pub use harness::{
    compare_images, generate_diff_image, AssertionResult, HarnessConfig, SuiteResult, TestMetadata, TestResult,
    TestStatus, TestType,
};
pub use runner::{RunnerStats, WptRunner, WptRunnerBuilder};

#[cfg(test)]
mod integration_tests {
    use super::*;
    use std::path::PathBuf;
    use tempfile::TempDir;

    /// Test that we can create a runner and run tests
    #[test]
    fn test_wpt_runner_integration() {
        let renderer = fastrender::FastRender::new().unwrap();
        let runner = WptRunner::new(renderer);

        // Runner should start with empty stats
        assert_eq!(runner.stats().total, 0);
        assert_eq!(runner.stats().passed, 0);
        assert_eq!(runner.stats().failed, 0);
    }

    /// Test running a suite on an empty directory
    #[test]
    fn test_empty_suite() {
        let renderer = fastrender::FastRender::new().unwrap();
        let mut runner = WptRunner::new(renderer);

        let temp = TempDir::new().unwrap();
        let results = runner.run_suite(temp.path());

        assert!(results.is_empty());
    }

    /// Test suite aggregation
    #[test]
    fn test_suite_aggregation() {
        let renderer = fastrender::FastRender::new().unwrap();
        let mut runner = WptRunner::new(renderer);

        let temp = TempDir::new().unwrap();

        // Create test files
        std::fs::write(
            temp.path().join("test1.html"),
            "<!DOCTYPE html><html><body><div>Test 1</div></body></html>",
        )
        .unwrap();
        std::fs::write(
            temp.path().join("test2.html"),
            "<!DOCTYPE html><html><body><div>Test 2</div></body></html>",
        )
        .unwrap();

        let suite = runner.run_suite_aggregated(temp.path());

        assert_eq!(suite.total(), 2);
        assert!(suite.duration.as_nanos() > 0);
    }

    /// Test filtering by pattern
    #[test]
    fn test_filter_pattern() {
        let renderer = fastrender::FastRender::new().unwrap();
        let config = HarnessConfig::default().with_filter("box-model");
        let mut runner = WptRunner::with_config(renderer, config);

        let temp = TempDir::new().unwrap();

        // Create test files with different names
        std::fs::write(temp.path().join("box-model-test.html"), "<html></html>").unwrap();
        std::fs::write(temp.path().join("margin-test.html"), "<html></html>").unwrap();

        let results = runner.run_suite(temp.path());

        // One should pass filter (box-model), one should be skipped
        let skipped = results.iter().filter(|r| r.status == TestStatus::Skip).count();
        let not_skipped = results.iter().filter(|r| r.status != TestStatus::Skip).count();

        // box-model-test.html passes filter, margin-test.html is filtered out
        assert_eq!(skipped, 1);
        assert!(not_skipped >= 1);
    }

    /// Test harness config builder pattern
    #[test]
    fn test_harness_config_builder() {
        let config = HarnessConfig::with_test_dir("custom/path")
            .with_tolerance(10)
            .with_max_diff(0.5)
            .fail_fast()
            .with_filter("css");

        assert_eq!(config.test_dir, PathBuf::from("custom/path"));
        assert_eq!(config.pixel_tolerance, 10);
        assert_eq!(config.max_diff_percentage, 0.5);
        assert!(config.fail_fast);
        assert_eq!(config.filter, Some("css".to_string()));
    }

    /// Test runner builder pattern
    #[test]
    fn test_runner_builder_pattern() {
        let renderer = fastrender::FastRender::new().unwrap();
        let runner = WptRunnerBuilder::new()
            .renderer(renderer)
            .test_dir("tests/custom")
            .expected_dir("tests/expected")
            .output_dir("target/output")
            .tolerance(5)
            .max_diff(1.0)
            .fail_fast()
            .save_rendered()
            .save_diffs()
            .build();

        assert_eq!(runner.config().test_dir, PathBuf::from("tests/custom"));
        assert_eq!(runner.config().expected_dir, PathBuf::from("tests/expected"));
        assert_eq!(runner.config().output_dir, PathBuf::from("target/output"));
        assert_eq!(runner.config().pixel_tolerance, 5);
        assert_eq!(runner.config().max_diff_percentage, 1.0);
        assert!(runner.config().fail_fast);
        assert!(runner.config().save_rendered);
        assert!(runner.config().save_diffs);
    }

    /// Test statistics tracking
    #[test]
    fn test_stats_tracking() {
        let renderer = fastrender::FastRender::new().unwrap();
        let mut runner = WptRunner::new(renderer);

        let temp = TempDir::new().unwrap();

        // Create a crashtest (doesn't need reference file)
        std::fs::write(
            temp.path().join("crashtest.html"),
            "<!DOCTYPE html><html><body>Crash test</body></html>",
        )
        .unwrap();

        let _result = runner.run_test(&temp.path().join("crashtest.html"));

        assert_eq!(runner.stats().total, 1);

        // Reset stats
        runner.reset_stats();
        assert_eq!(runner.stats().total, 0);
    }

    /// Test TestResult creation helpers
    #[test]
    fn test_result_creation() {
        let metadata = TestMetadata::from_path(PathBuf::from("test.html"));

        let pass = TestResult::pass(metadata.clone(), std::time::Duration::from_millis(100));
        assert_eq!(pass.status, TestStatus::Pass);

        let fail = TestResult::fail(metadata.clone(), std::time::Duration::from_millis(50), "Image mismatch");
        assert_eq!(fail.status, TestStatus::Fail);
        assert!(fail.message.is_some());

        let error = TestResult::error(metadata.clone(), std::time::Duration::from_millis(10), "Parse error");
        assert_eq!(error.status, TestStatus::Error);

        let skip = TestResult::skip(metadata.clone(), "Not supported");
        assert_eq!(skip.status, TestStatus::Skip);
        assert_eq!(skip.duration, std::time::Duration::ZERO);

        let timeout = TestResult::timeout(metadata.clone(), std::time::Duration::from_secs(30));
        assert_eq!(timeout.status, TestStatus::Timeout);
    }

    /// Test metadata from path extraction
    #[test]
    fn test_metadata_from_path() {
        let path = PathBuf::from("/path/to/tests/wpt/css/box-model-001.html");
        let metadata = TestMetadata::from_path(path.clone());

        assert_eq!(metadata.id, "box-model-001");
        assert_eq!(metadata.path, path);
        assert_eq!(metadata.test_type, TestType::Reftest);
        assert!(!metadata.disabled);
    }

    /// Test suite result display
    #[test]
    fn test_suite_result_display() {
        let mut suite = SuiteResult::new("test-suite");

        suite.add_result(TestResult::pass(
            TestMetadata::from_path(PathBuf::from("test1.html")),
            std::time::Duration::from_millis(100),
        ));
        suite.add_result(TestResult::fail(
            TestMetadata::from_path(PathBuf::from("test2.html")),
            std::time::Duration::from_millis(50),
            "Failed",
        ));
        suite.finalize();

        let display = format!("{}", suite);

        assert!(display.contains("Suite: test-suite"));
        assert!(display.contains("Total: 2"));
        assert!(display.contains("Passed: 1"));
        assert!(display.contains("Failed: 1"));
        assert!(display.contains("Pass Rate: 50.0%"));
    }
}
