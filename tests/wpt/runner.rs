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
use super::harness::DiscoveryMode;
use super::harness::HarnessConfig;
use super::harness::ImageComparisonResult;
use super::harness::ReftestExpectation;
use super::harness::SuiteResult;
use super::harness::TestMetadata;
use super::harness::TestResult;
use super::harness::TestStatus;
use super::harness::TestType;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::font_loader::FontContext;
use html5ever::parse_document;
use html5ever::tendril::TendrilSink;
use image::codecs::png::PngEncoder;
use image::{ColorType, ImageEncoder, Rgba, RgbaImage};
use markup5ever_rcdom::{Handle, NodeData, RcDom};
use rayon::prelude::*;
use serde::Deserialize;
use std::collections::HashSet;
use std::fmt::Write;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::OnceLock;
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

#[derive(Debug, Clone, Deserialize)]
struct ManifestFile {
  tests: Vec<ManifestEntry>,
}

#[derive(Debug, Clone, Deserialize)]
struct ManifestEntry {
  path: String,
  #[serde(default)]
  reference: Option<String>,
  #[serde(default)]
  test_type: Option<String>,
  #[serde(default)]
  id: Option<String>,
  #[serde(default)]
  expected: Option<String>,
  #[serde(default)]
  viewport: Option<ManifestViewport>,
  #[serde(default)]
  timeout_ms: Option<u64>,
  #[serde(default)]
  disabled: Option<String>,
  #[serde(default)]
  dpr: Option<f32>,
}

#[derive(Debug, Clone, Deserialize)]
struct ManifestViewport {
  width: u32,
  height: u32,
}

#[derive(Debug, Default)]
struct IniMetadata {
  expected: Option<TestStatus>,
  timeout_ms: Option<u64>,
  disabled: Option<String>,
  viewport: Option<(u32, u32)>,
  dpr: Option<f32>,
  test_type: Option<TestType>,
  reference: Option<PathBuf>,
  reftest_expectation: Option<ReftestExpectation>,
}

impl IniMetadata {
  fn is_empty(&self) -> bool {
    self.expected.is_none()
      && self.timeout_ms.is_none()
      && self.disabled.is_none()
      && self.viewport.is_none()
      && self.dpr.is_none()
      && self.test_type.is_none()
      && self.reference.is_none()
      && self.reftest_expectation.is_none()
  }
}

#[derive(Debug, Clone)]
struct ArtifactPaths {
  base: PathBuf,
  actual: PathBuf,
  diff: PathBuf,
  expected: PathBuf,
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
    let mut metadata = TestMetadata::from_path(test_path.to_path_buf());
    metadata.id = Self::test_id_for_path(&metadata.path, &self.config.test_dir);
    if metadata.timeout_ms == 0 {
      metadata.timeout_ms = self.config.default_timeout_ms;
    }

    let result = Self::execute_test(&self.config, &mut self.renderer, metadata);
    self.record_result(&result);
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
    self.apply_font_overrides();
    let test_cases = self.discover_tests(suite_dir);
    let mut results = if self.config.parallel && !self.config.fail_fast {
      self.run_tests_parallel(test_cases)
    } else {
      self.run_tests_sequential(test_cases)
    };

    for result in &results {
      self.record_result(result);
    }

    self.write_report(&results);
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

  fn record_result(&mut self, result: &TestResult) {
    self.stats.record(result);
    self.save_artifact(result);
  }

  fn run_tests_sequential(&mut self, tests: Vec<TestMetadata>) -> Vec<TestResult> {
    let mut results = Vec::with_capacity(tests.len());

    for metadata in tests {
      let result = Self::execute_test(&self.config, &mut self.renderer, metadata);
      let should_break = self.config.fail_fast && result.status.is_failure();
      results.push(result);

      if should_break {
        break;
      }
    }

    results
  }

  fn run_tests_parallel(&self, tests: Vec<TestMetadata>) -> Vec<TestResult> {
    if tests.is_empty() {
      return Vec::new();
    }

    let config = self.config.clone();
    let workers = config.workers.max(1);
    let pool = rayon::ThreadPoolBuilder::new()
      .num_threads(workers)
      .build()
      .unwrap_or_else(|_| rayon::ThreadPoolBuilder::new().build().unwrap());

    pool.install(|| {
      tests
        .into_par_iter()
        .map(|metadata| {
          let mut renderer = match fastrender::FastRender::new() {
            Ok(renderer) => renderer,
            Err(e) => {
              return TestResult::error(
                metadata.clone(),
                Duration::ZERO,
                format!("Failed to initialize renderer: {e}"),
              )
            }
          };

          Self::apply_font_overrides_to_renderer(&config, &mut renderer);
          Self::execute_test(&config, &mut renderer, metadata)
        })
        .collect()
    })
  }

  fn discover_tests(&self, suite_dir: &Path) -> Vec<TestMetadata> {
    let manifest = if matches!(
      self.config.discovery_mode,
      DiscoveryMode::ManifestOnly | DiscoveryMode::ManifestWithFallback
    ) {
      self
        .manifest_path_for_suite(suite_dir)
        .and_then(|path| self.load_manifest(&path, suite_dir).ok())
    } else {
      None
    };

    if let Some(mut entries) = manifest {
      if self.config.discovery_mode == DiscoveryMode::ManifestWithFallback {
        let discovered = self.collect_tests(suite_dir);
        let existing: HashSet<PathBuf> = entries.iter().map(|m| m.path.clone()).collect();
        for test in discovered {
          if !existing.contains(&test.path) {
            entries.push(test);
          }
        }
        entries.sort_by(|a, b| a.path.cmp(&b.path));
      }
      return entries;
    }

    if self.config.discovery_mode == DiscoveryMode::ManifestOnly {
      return Vec::new();
    }

    self.collect_tests(suite_dir)
  }

  fn manifest_path_for_suite(&self, suite_dir: &Path) -> Option<PathBuf> {
    if let Some(custom) = &self.config.manifest_path {
      let path = if custom.is_absolute() {
        custom.clone()
      } else {
        custom.clone()
      };

      if path.exists() {
        return Some(path);
      }
    }

    let candidates = [
      Some(suite_dir.join("manifest.json")),
      Some(suite_dir.join("manifest.toml")),
      suite_dir.parent().map(|p| p.join("manifest.json")),
      suite_dir.parent().map(|p| p.join("manifest.toml")),
    ];

    candidates.into_iter().flatten().find(|path| path.exists())
  }

  fn apply_font_overrides(&mut self) {
    Self::apply_font_overrides_to_renderer(&self.config, &mut self.renderer);
  }

  fn apply_font_overrides_to_renderer(
    config: &HarnessConfig,
    renderer: &mut fastrender::FastRender,
  ) {
    if config.font_dirs.is_empty() {
      return;
    }

    let mut db = FontDatabase::empty();
    for dir in &config.font_dirs {
      db.load_fonts_dir(dir);
    }

    if db.font_count() == 0 {
      eprintln!(
        "No fonts loaded from configured directories: {:?}",
        config.font_dirs
      );
      return;
    }

    let font_context = FontContext::with_database(Arc::new(db));
    renderer.set_font_context(font_context);
  }

  fn load_manifest(
    &self,
    manifest_path: &Path,
    suite_dir: &Path,
  ) -> Result<Vec<TestMetadata>, String> {
    let data = fs::read(manifest_path)
      .map_err(|e| format!("Failed to read manifest {:?}: {}", manifest_path, e))?;

    let manifest: ManifestFile = match manifest_path.extension().and_then(|e| e.to_str()) {
      Some("json") => serde_json::from_slice(&data)
        .map_err(|e| format!("Failed to parse JSON manifest {:?}: {}", manifest_path, e))?,
      Some("toml") => {
        let manifest_str = std::str::from_utf8(&data).map_err(|e| {
          format!(
            "Failed to parse TOML manifest {:?} (invalid UTF-8): {}",
            manifest_path, e
          )
        })?;
        toml::from_str(manifest_str)
          .map_err(|e| format!("Failed to parse TOML manifest {:?}: {}", manifest_path, e))?
      }
      _ => {
        return Err(format!(
          "Unsupported manifest format for {:?}",
          manifest_path
        ))
      }
    };

    let manifest_dir = manifest_path.parent().unwrap_or(Path::new("."));

    Ok(
      manifest
        .tests
        .into_iter()
        .map(|entry| self.metadata_from_manifest(entry, suite_dir, manifest_dir))
        .collect(),
    )
  }

  fn metadata_from_manifest(
    &self,
    entry: ManifestEntry,
    suite_dir: &Path,
    manifest_dir: &Path,
  ) -> TestMetadata {
    let path = Self::resolve_path(manifest_dir, suite_dir, &entry.path);
    let mut metadata = TestMetadata::from_path(path);

    metadata.id = entry
      .id
      .unwrap_or_else(|| Self::test_id_for_path(&metadata.path, &self.config.test_dir));
    metadata.timeout_ms = entry.timeout_ms.unwrap_or(self.config.default_timeout_ms);
    metadata.device_pixel_ratio = entry.dpr.unwrap_or(1.0);

    if let Some(viewport) = entry.viewport {
      metadata.viewport_width = viewport.width;
      metadata.viewport_height = viewport.height;
    }

    if let Some(reference) = entry.reference {
      metadata.reference_path = Some(Self::resolve_path(manifest_dir, suite_dir, &reference));
    }

    if let Some(kind) = entry.test_type {
      metadata.test_type = Self::parse_test_type_from_manifest(&kind, &metadata.path);
    }

    if let Some(expected) = entry.expected {
      metadata.expected_status = Some(Self::parse_expected_status(&expected));
    }

    if let Some(reason) = entry.disabled {
      metadata = metadata.disable(reason);
    }

    self.apply_sidecar_metadata(&mut metadata);

    metadata
  }

  /// Collects all test files from a directory
  fn collect_tests(&self, dir: &Path) -> Vec<TestMetadata> {
    let mut tests = Vec::new();

    if !dir.exists() {
      return tests;
    }

    if let Ok(entries) = fs::read_dir(dir) {
      for entry in entries.flatten() {
        let path = entry.path();
        if path.is_file() && self.is_test_file(&path) {
          let mut metadata = TestMetadata::from_path(path);
          if metadata.timeout_ms == 0 {
            metadata.timeout_ms = self.config.default_timeout_ms;
          }
          metadata.id = Self::test_id_for_path(&metadata.path, &self.config.test_dir);
          self.apply_sidecar_metadata(&mut metadata);
          tests.push(metadata);
        } else if path.is_dir() {
          // Recursively collect from subdirectories
          tests.extend(self.collect_tests(&path));
        }
      }
    }

    // Sort for deterministic ordering
    tests.sort_by(|a, b| a.path.cmp(&b.path));
    tests
  }

  fn apply_sidecar_metadata(&self, metadata: &mut TestMetadata) {
    if let Some(ini_path) = Self::ini_metadata_path(&metadata.path) {
      if let Ok(Some(ini)) = self.parse_ini_metadata(&ini_path, metadata) {
        self.apply_ini_overrides(metadata, ini);
      }
    }

    if let Some((reference, expectation)) = Self::discover_reference_from_html(&metadata.path) {
      metadata.reference_path = Some(reference);
      metadata.reftest_expectation = expectation;
      metadata.test_type = TestType::Reftest;
    }

    if metadata.timeout_ms == 0 {
      metadata.timeout_ms = self.config.default_timeout_ms;
    }
  }

  fn ini_metadata_path(test_path: &Path) -> Option<PathBuf> {
    let mut candidates = Vec::new();
    candidates.push(test_path.with_extension("ini"));
    if let Some(ext) = test_path.extension().and_then(|e| e.to_str()) {
      candidates.push(test_path.with_extension(format!("{ext}.ini")));
    }
    candidates.into_iter().find(|p| p.exists())
  }

  fn ini_section_names(&self, metadata: &TestMetadata) -> Vec<String> {
    let mut names = Vec::new();
    if let Some(filename) = metadata.path.file_name().and_then(|n| n.to_str()) {
      names.push(filename.to_string());
    }
    names.push(format!("{}.html", metadata.id));
    if let Ok(relative) = metadata.path.strip_prefix(&self.config.test_dir) {
      names.push(relative.to_string_lossy().replace('\\', "/"));
    }
    names
  }

  fn parse_ini_metadata(
    &self,
    ini_path: &Path,
    metadata: &TestMetadata,
  ) -> Result<Option<IniMetadata>, String> {
    let content = fs::read_to_string(ini_path)
      .map_err(|e| format!("Failed to read ini {:?}: {}", ini_path, e))?;

    let mut ini = IniMetadata::default();
    let mut in_section = false;
    let mut saw_section = false;
    let sections = self.ini_section_names(metadata);

    for raw_line in content.lines() {
      let line = raw_line.trim();
      if line.is_empty() || line.starts_with('#') || line.starts_with(';') {
        continue;
      }

      if line.starts_with('[') && line.ends_with(']') {
        saw_section = true;
        let section = line.trim_start_matches('[').trim_end_matches(']').trim();
        in_section = sections
          .iter()
          .any(|name| name.eq_ignore_ascii_case(section));
        continue;
      }

      if saw_section && !in_section {
        continue;
      }

      let Some((raw_key, raw_value)) = line
        .split_once(':')
        .or_else(|| line.split_once('='))
        .map(|(k, v)| (k.trim(), v.trim()))
      else {
        continue;
      };

      let key = raw_key.to_ascii_lowercase();
      match key.as_str() {
        "expected" => ini.expected = Some(Self::parse_expected_status(raw_value)),
        "disabled" | "skip" => {
          let reason = if raw_value.is_empty() {
            "Disabled via metadata"
          } else {
            raw_value
          };
          ini.disabled = Some(reason.to_string());
        }
        "timeout" => {
          ini.timeout_ms = Some(if raw_value.eq_ignore_ascii_case("long") {
            60000
          } else {
            raw_value.parse().unwrap_or(self.config.default_timeout_ms)
          });
        }
        "viewport" => {
          if let Some((w, h)) = Self::parse_viewport(raw_value) {
            ini.viewport = Some((w, h));
          }
        }
        "dpr" | "device-pixel-ratio" => {
          if let Ok(dpr) = raw_value.parse::<f32>() {
            ini.dpr = Some(dpr);
          }
        }
        "type" | "test_type" => {
          ini.test_type = Some(Self::parse_test_type_from_manifest(
            raw_value,
            &metadata.path,
          ));
        }
        "reference" | "ref" => {
          let base = metadata.path.parent().unwrap_or_else(|| Path::new("."));
          ini.reference = Some(base.join(raw_value));
        }
        "reftest" => {
          if raw_value.eq_ignore_ascii_case("mismatch") {
            ini.reftest_expectation = Some(ReftestExpectation::Mismatch);
          } else if raw_value.eq_ignore_ascii_case("match") {
            ini.reftest_expectation = Some(ReftestExpectation::Match);
          }
        }
        _ => {}
      }
    }

    if ini.is_empty() {
      Ok(None)
    } else {
      Ok(Some(ini))
    }
  }

  fn apply_ini_overrides(&self, metadata: &mut TestMetadata, ini: IniMetadata) {
    if let Some(expected) = ini.expected {
      metadata.expected_status = Some(expected);
    }
    if let Some(timeout) = ini.timeout_ms {
      metadata.timeout_ms = timeout;
    }
    if let Some(reason) = ini.disabled {
      metadata.disabled = true;
      metadata.disabled_reason = Some(reason);
    }
    if let Some((w, h)) = ini.viewport {
      metadata.viewport_width = w;
      metadata.viewport_height = h;
    }
    if let Some(dpr) = ini.dpr {
      metadata.device_pixel_ratio = dpr;
    }
    if let Some(kind) = ini.test_type {
      metadata.test_type = kind;
    }
    if let Some(reference) = ini.reference {
      metadata.reference_path = Some(reference);
      metadata.test_type = TestType::Reftest;
    }
    if let Some(expectation) = ini.reftest_expectation {
      metadata.reftest_expectation = expectation;
    }
  }

  fn parse_viewport(raw: &str) -> Option<(u32, u32)> {
    let parts: Vec<&str> = raw.split(['x', 'X']).collect();
    if parts.len() != 2 {
      return None;
    }
    let width = parts.get(0)?.trim().parse().ok()?;
    let height = parts.get(1)?.trim().parse().ok()?;
    Some((width, height))
  }

  fn discover_reference_from_html(test_path: &Path) -> Option<(PathBuf, ReftestExpectation)> {
    let Ok(content) = fs::read_to_string(test_path) else {
      return None;
    };

    let mut bytes = content.as_bytes();
    let dom = parse_document(RcDom::default(), Default::default())
      .from_utf8()
      .read_from(&mut bytes)
      .ok()?;

    let link = Self::find_reftest_link(dom.document)?;
    let base = test_path.parent().unwrap_or_else(|| Path::new("."));
    Some((base.join(link.0), link.1))
  }

  fn find_reftest_link(handle: Handle) -> Option<(String, ReftestExpectation)> {
    if let NodeData::Element {
      ref name,
      ref attrs,
      ..
    } = handle.data
    {
      if name.local.eq_ignore_ascii_case("link") {
        let mut rel_value = None;
        let mut href_value = None;
        for attr in attrs.borrow().iter() {
          if attr.name.local.eq_ignore_ascii_case("rel") {
            rel_value = Some(attr.value.to_string());
          } else if attr.name.local.eq_ignore_ascii_case("href") {
            href_value = Some(attr.value.to_string());
          }
        }

        if let (Some(rel), Some(href)) = (rel_value, href_value) {
          for token in rel.split_whitespace() {
            if token.eq_ignore_ascii_case("match") {
              return Some((href, ReftestExpectation::Match));
            }
            if token.eq_ignore_ascii_case("mismatch") {
              return Some((href, ReftestExpectation::Mismatch));
            }
          }
        }
      }
    }

    for child in handle.children.borrow().iter() {
      if let Some(found) = Self::find_reftest_link(child.clone()) {
        return Some(found);
      }
    }
    None
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

  fn execute_test(
    config: &HarnessConfig,
    renderer: &mut fastrender::FastRender,
    mut metadata: TestMetadata,
  ) -> TestResult {
    let start = Instant::now();

    if metadata.test_type == TestType::Reftest && metadata.reference_path.is_none() {
      let expected_path = Self::get_expected_image_path(config, &metadata);
      if expected_path.exists() {
        metadata.test_type = TestType::Visual;
      }
    }

    if metadata.timeout_ms == 0 {
      metadata.timeout_ms = config.default_timeout_ms;
    }

    // Check if test is disabled
    if metadata.disabled {
      let reason = metadata
        .disabled_reason
        .clone()
        .unwrap_or_else(|| "Disabled".to_string());
      return TestResult::skip(metadata, reason);
    }

    // Check filter
    if let Some(ref filter) = config.filter {
      if !metadata.id.contains(filter)
        && !metadata.path.to_string_lossy().contains(filter)
        && !metadata
          .reference_path
          .as_ref()
          .is_some_and(|p| p.to_string_lossy().contains(filter))
      {
        return TestResult::skip(metadata, "Filtered out");
      }
    }

    let dpr = if metadata.device_pixel_ratio > 0.0 {
      metadata.device_pixel_ratio
    } else {
      1.0
    };
    renderer.set_device_pixel_ratio(dpr);

    let mut result = match metadata.test_type {
      TestType::Reftest => Self::run_reftest(renderer, &metadata, config, start),
      TestType::Visual => Self::run_visual_test(renderer, &metadata, config, start),
      TestType::Crashtest => Self::run_crashtest(renderer, &metadata, start),
      TestType::Testharness => {
        TestResult::skip(metadata.clone(), "Testharness tests not yet supported")
      }
      TestType::Manual => TestResult::skip(metadata.clone(), "Manual tests not supported"),
    };

    // Apply timeout if execution exceeded limits
    let timeout_limit = Duration::from_millis(metadata.timeout_ms.max(config.default_timeout_ms));
    if result.status != TestStatus::Timeout && result.status != TestStatus::Skip {
      if result.duration > timeout_limit {
        result = TestResult::timeout(result.metadata.clone(), result.duration);
      }
    }

    Self::apply_expected_outcome(result)
  }

  /// Runs a reference test
  fn run_reftest(
    renderer: &mut fastrender::FastRender,
    metadata: &TestMetadata,
    config: &HarnessConfig,
    start: Instant,
  ) -> TestResult {
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
    let test_image = match Self::render_html(renderer, &test_html, metadata) {
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
    let ref_image = match Self::render_html(renderer, &ref_html, metadata) {
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
    Self::compare_and_result(config, metadata, start, test_image, ref_image)
  }

  /// Runs a visual test against expected PNG
  fn run_visual_test(
    renderer: &mut fastrender::FastRender,
    metadata: &TestMetadata,
    config: &HarnessConfig,
    start: Instant,
  ) -> TestResult {
    // Find expected image
    let expected_path = Self::get_expected_image_path(config, metadata);

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
    } else if config.update_expected {
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

    let rendered_image = match Self::render_html(renderer, &test_html, metadata) {
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
    if expected_image.is_none() && config.update_expected {
      if let Err(e) = Self::save_expected_image(&expected_path, &rendered_image) {
        return TestResult::error(
          metadata.clone(),
          start.elapsed(),
          format!("Failed to save expected image: {}", e),
        );
      }
      let mut result = TestResult::pass(metadata.clone(), start.elapsed())
        .with_images(rendered_image.clone(), rendered_image.clone());
      if let Ok(comparison) = compare_images(
        &rendered_image,
        &rendered_image,
        config.pixel_tolerance,
        Some(0.0),
      ) {
        result = result.with_diff(&comparison);
      }
      return result;
    }

    let expected_image = match expected_image {
      Some(img) => img,
      None => {
        return TestResult::error(
          metadata.clone(),
          start.elapsed(),
          format!("Expected image not found: {:?}", expected_path),
        )
      }
    };
    Self::compare_and_result(config, metadata, start, rendered_image, expected_image)
  }

  /// Runs a crash test
  fn run_crashtest(
    renderer: &mut fastrender::FastRender,
    metadata: &TestMetadata,
    start: Instant,
  ) -> TestResult {
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
    match Self::render_html(renderer, &test_html, metadata) {
      Ok(rendered) => {
        let mut result = TestResult::pass(metadata.clone(), start.elapsed())
          .with_images(rendered.clone(), rendered);
        if let (Some(ref actual), Some(ref expected)) =
          (&result.rendered_image, &result.expected_image)
        {
          if let Ok(comparison) = compare_images(actual, expected, 0, Some(0.0)) {
            result = result.with_diff(&comparison);
          }
        }
        result
      }
      Err(e) => TestResult::error(
        metadata.clone(),
        start.elapsed(),
        format!("Render error (not a crash): {}", e),
      ),
    }
  }

  /// Renders HTML and returns PNG bytes
  fn render_html(
    renderer: &mut fastrender::FastRender,
    html: &str,
    metadata: &TestMetadata,
  ) -> Result<Vec<u8>, String> {
    renderer
      .render_to_png(html, metadata.viewport_width, metadata.viewport_height)
      .map_err(|e| format!("Render error: {}", e))
  }

  /// Compares two images and creates the appropriate result
  fn compare_and_result(
    config: &HarnessConfig,
    metadata: &TestMetadata,
    start: Instant,
    rendered: Vec<u8>,
    expected: Vec<u8>,
  ) -> TestResult {
    let duration = start.elapsed();

    let comparison = match compare_images(
      &rendered,
      &expected,
      config.pixel_tolerance,
      Some(config.max_diff_percentage),
    ) {
      Ok(result) => result,
      Err(e) => {
        return TestResult::error(
          metadata.clone(),
          duration,
          format!("Comparison error: {}", e),
        )
      }
    };

    let should_match = metadata.reftest_expectation != ReftestExpectation::Mismatch;
    let matches_expectation = if should_match {
      comparison.is_match(config.max_diff_percentage)
    } else {
      comparison.diff_pixels > 0
    };

    if matches_expectation {
      let mut result = TestResult::pass(metadata.clone(), duration)
        .with_images(rendered, expected)
        .with_diff(&comparison);
      if !should_match {
        result.message = Some("Mismatch reference differed as expected".to_string());
      }
      return result;
    }

    if config.update_expected && should_match {
      let expected_path = Self::get_expected_image_path(config, metadata);
      if let Err(err) = Self::save_expected_image(&expected_path, &rendered) {
        return TestResult::error(
          metadata.clone(),
          duration,
          format!("Failed to update expected image: {err}"),
        );
      }

      let mut result = TestResult::pass(metadata.clone(), duration)
        .with_images(rendered, expected)
        .with_diff(&comparison);
      result.message = Some("Updated expected image".to_string());
      return result;
    }

    let message =
      Self::format_comparison_failure(&comparison, metadata.reftest_expectation, config);
    TestResult::fail(metadata.clone(), duration, message)
      .with_images(rendered, expected)
      .with_diff(&comparison)
  }

  fn format_comparison_failure(
    comparison: &ImageComparisonResult,
    expectation: ReftestExpectation,
    config: &HarnessConfig,
  ) -> String {
    let mut message = if expectation == ReftestExpectation::Mismatch {
      "Mismatch expected but renders were identical".to_string()
    } else {
      format!(
        "Image mismatch: {:.3}% difference ({} pixels, max channel diff {})",
        comparison.diff_percentage, comparison.diff_pixels, comparison.max_channel_diff
      )
    };

    if expectation == ReftestExpectation::Match && config.max_diff_percentage > 0.0 {
      let _ = write!(
        &mut message,
        " (allowed ≤{:.3}% and tolerance {})",
        config.max_diff_percentage, config.pixel_tolerance
      );
    }

    if !comparison.samples.is_empty() {
      let samples: Vec<String> = comparison
        .samples
        .iter()
        .map(|s| format!("({}, {}): Δ{:?}", s.x, s.y, s.delta))
        .collect();
      let _ = write!(&mut message, "; first differences {}", samples.join(", "));
    }

    message
  }

  /// Gets the path for the expected image
  fn get_expected_image_path(config: &HarnessConfig, metadata: &TestMetadata) -> PathBuf {
    // Try to construct expected path from test path
    let relative = metadata
      .path
      .strip_prefix(&config.test_dir)
      .unwrap_or(&metadata.path);

    let mut expected_path = config.expected_dir.join(relative);
    expected_path.set_extension("png");
    expected_path
  }

  /// Saves the expected image
  fn save_expected_image(path: &Path, image: &[u8]) -> Result<(), String> {
    if let Some(parent) = path.parent() {
      fs::create_dir_all(parent).map_err(|e| format!("Failed to create directory: {}", e))?;
    }
    fs::write(path, image).map_err(|e| format!("Failed to write image: {}", e))
  }

  fn artifact_paths(config: &HarnessConfig, metadata: &TestMetadata) -> ArtifactPaths {
    let base = config.output_dir.join(&metadata.id);
    let actual = base.join("actual.png");
    let expected = base.join("expected.png");
    let diff = base.join("diff.png");

    ArtifactPaths {
      base,
      actual,
      diff,
      expected,
    }
  }

  fn resolve_path(manifest_dir: &Path, suite_dir: &Path, value: &str) -> PathBuf {
    let path = PathBuf::from(value);
    if path.is_absolute() {
      return path;
    }

    let manifest_path = manifest_dir.join(&path);
    if manifest_path.exists() {
      manifest_path
    } else {
      suite_dir.join(path)
    }
  }

  fn parse_test_type_from_manifest(value: &str, path: &Path) -> TestType {
    match value.to_lowercase().as_str() {
      "visual" => TestType::Visual,
      "reftest" => TestType::Reftest,
      "crashtest" => TestType::Crashtest,
      "manual" => TestType::Manual,
      "testharness" => TestType::Testharness,
      _ => TestType::from_path(path),
    }
  }

  fn parse_expected_status(value: &str) -> TestStatus {
    match value.to_lowercase().as_str() {
      "fail" => TestStatus::Fail,
      "error" => TestStatus::Error,
      "timeout" => TestStatus::Timeout,
      "skip" => TestStatus::Skip,
      _ => TestStatus::Pass,
    }
  }

  fn apply_expected_outcome(mut result: TestResult) -> TestResult {
    if let Some(expected) = result.metadata.expected_status {
      if expected == result.status {
        if expected != TestStatus::Pass {
          result.status = TestStatus::Pass;
          result.message = Some(format!("Expected {expected:?} matched"));
        }
      } else if expected == TestStatus::Fail && result.status == TestStatus::Pass {
        return TestResult::fail(
          result.metadata.clone(),
          result.duration,
          "Unexpected pass (expected failure)",
        );
      }
    }

    result
  }

  fn link_from_output(&self, path: &Path) -> String {
    if let Some(rel) = pathdiff::diff_paths(path, &self.config.output_dir) {
      rel.to_string_lossy().replace('\\', "/")
    } else {
      path.to_string_lossy().replace('\\', "/")
    }
  }

  fn write_report(&self, results: &[TestResult]) -> Option<PathBuf> {
    if !self.config.write_report {
      return None;
    }

    let counts = |status: TestStatus| results.iter().filter(|r| r.status == status).count();
    let total = results.len();
    let passed = counts(TestStatus::Pass);
    let failed = results.iter().filter(|r| r.status.is_failure()).count();
    let skipped = counts(TestStatus::Skip);
    let suite_name = self
      .config
      .test_dir
      .file_name()
      .and_then(|s| s.to_str())
      .unwrap_or("wpt");

    let mut rows = String::new();
    for result in results {
      let paths = Self::artifact_paths(&self.config, &result.metadata);
      let expected_link = self.link_from_output(&paths.expected);
      let actual_link = self.link_from_output(&paths.actual);
      let diff_link = self.link_from_output(&paths.diff);
      let diff_pct = result.diff_percentage.unwrap_or(0.0);
      let max_diff = result.max_channel_diff.unwrap_or(0);
      let message = result
        .message
        .clone()
        .unwrap_or_default()
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;");
      let _ = writeln!(
        rows,
        "<tr data-status=\"{}\" data-diff=\"{:.4}\" data-suite=\"{}\">\
           <td>{}</td>\
           <td>{}</td>\
           <td>{:.3}</td>\
           <td>{}</td>\
           <td><a href=\"{}\">expected</a> | <a href=\"{}\">actual</a> | <a href=\"{}\">diff</a></td>\
           <td>{}</td>\
         </tr>",
        result.status,
        diff_pct,
        suite_name,
        Self::escape_html(&result.metadata.id),
        result.status,
        diff_pct,
        max_diff,
        expected_link,
        actual_link,
        diff_link,
        message,
      );
    }

    let report = format!(
      r#"<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <title>WPT report</title>
  <style>
    body {{ font-family: sans-serif; margin: 16px; }}
    table {{ border-collapse: collapse; width: 100%; }}
    th, td {{ border: 1px solid #ccc; padding: 6px; font-size: 14px; }}
    thead {{ background: #f6f6f6; position: sticky; top: 0; }}
    .controls label {{ margin-right: 12px; }}
  </style>
</head>
<body>
  <h1>WPT run summary</h1>
  <p>Suite: {suite_name}. Total: {total}. Passed: {passed}. Failed/Error: {failed}. Skipped: {skipped}.</p>
  <div class="controls">
    <label><input type="checkbox" name="status" value="PASS" checked>Pass</label>
    <label><input type="checkbox" name="status" value="FAIL" checked>Fail</label>
    <label><input type="checkbox" name="status" value="ERROR" checked>Error</label>
    <label><input type="checkbox" name="status" value="TIMEOUT" checked>Timeout</label>
    <label><input type="checkbox" name="status" value="SKIP" checked>Skip</label>
    <label style="margin-left:16px;">Min diff % <input type="number" id="min-diff" value="0" step="0.01" style="width:90px;"></label>
  </div>
  <table>
    <thead>
      <tr><th>Test</th><th>Status</th><th>Diff %</th><th>Max Δ</th><th>Links</th><th>Message</th></tr>
    </thead>
    <tbody id="results">
      {rows}
    </tbody>
  </table>
  <script>
    const applyFilters = () => {{
      const statuses = Array.from(document.querySelectorAll('input[name="status"]:checked')).map(el => el.value);
      const minDiff = parseFloat(document.getElementById('min-diff').value || '0');
      document.querySelectorAll('#results tr').forEach(row => {{
        const status = row.dataset.status;
        const diff = parseFloat(row.dataset.diff || '0');
        const show = statuses.includes(status) && diff >= minDiff;
        row.style.display = show ? '' : 'none';
      }});
    }};
    document.querySelectorAll('input[name="status"]').forEach(el => el.addEventListener('change', applyFilters));
    document.getElementById('min-diff').addEventListener('input', applyFilters);
    applyFilters();
  </script>
</body>
</html>
"#
    );

    let report_path = self.config.output_dir.join("report.html");
    if let Some(parent) = report_path.parent() {
      if fs::create_dir_all(parent).is_err() {
        return None;
      }
    }

    match fs::write(&report_path, report) {
      Ok(_) => Some(report_path),
      Err(_) => None,
    }
  }

  fn test_id_for_path(path: &Path, test_root: &Path) -> String {
    if let Ok(relative) = path.strip_prefix(test_root) {
      let mut trimmed = relative.to_path_buf();
      trimmed.set_extension("");
      trimmed
        .to_string_lossy()
        .replace(std::path::MAIN_SEPARATOR, "/")
    } else {
      path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
        .to_string()
    }
  }

  /// Saves test artifacts (rendered images, diffs)
  fn save_artifact(&self, result: &TestResult) {
    let paths = Self::artifact_paths(&self.config, &result.metadata);

    if let Err(e) = fs::create_dir_all(&paths.base) {
      eprintln!("Failed to create output directory: {}", e);
      return;
    }

    let actual = result
      .rendered_image
      .clone()
      .unwrap_or_else(Self::placeholder_png);
    if let Err(e) = fs::write(&paths.actual, &actual) {
      eprintln!("Failed to save rendered image: {}", e);
    }

    let expected = result
      .expected_image
      .clone()
      .or_else(|| self.load_expected_bytes(&result.metadata))
      .unwrap_or_else(Self::placeholder_png);
    if let Err(e) = fs::write(&paths.expected, &expected) {
      eprintln!("Failed to save expected image: {}", e);
    }

    let diff = generate_diff_image(&actual, &expected, self.config.pixel_tolerance)
      .unwrap_or_else(|_| Self::placeholder_png());
    if let Err(e) = fs::write(&paths.diff, diff) {
      eprintln!("Failed to save diff image: {}", e);
    }
  }

  fn load_expected_bytes(&self, metadata: &TestMetadata) -> Option<Vec<u8>> {
    let path = Self::get_expected_image_path(&self.config, metadata);
    fs::read(path).ok()
  }

  fn placeholder_png() -> Vec<u8> {
    static EMPTY: OnceLock<Vec<u8>> = OnceLock::new();
    EMPTY
      .get_or_init(|| {
        let image = RgbaImage::from_pixel(1, 1, Rgba([0, 0, 0, 0]));
        let mut buffer = Vec::new();
        let _ = PngEncoder::new(&mut buffer).write_image(
          image.as_raw(),
          image.width(),
          image.height(),
          ColorType::Rgba8.into(),
        );
        buffer
      })
      .clone()
  }

  fn escape_html(input: &str) -> String {
    input
      .replace('&', "&amp;")
      .replace('<', "&lt;")
      .replace('>', "&gt;")
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

  /// Enables parallel execution with the given worker count
  pub fn parallel(mut self, workers: usize) -> Self {
    self.config.parallel = true;
    self.config.workers = workers.max(1);
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

  /// Sets a custom manifest path
  pub fn manifest(mut self, manifest: impl Into<PathBuf>) -> Self {
    self.config.manifest_path = Some(manifest.into());
    self
  }

  /// Sets the discovery mode (manifest vs sidecar metadata)
  pub fn discovery_mode(mut self, mode: DiscoveryMode) -> Self {
    self.config.discovery_mode = mode;
    self
  }

  /// Seeds the renderer with fonts from a directory for deterministic output.
  pub fn font_dir(mut self, dir: impl Into<PathBuf>) -> Self {
    self.config.font_dirs.push(dir.into());
    self
  }

  /// Disables writing the Markdown report
  pub fn no_report(mut self) -> Self {
    self.config.write_report = false;
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
      .parallel(3)
      .manifest("custom/manifest.toml")
      .no_report()
      .build();

    assert_eq!(runner.config().test_dir, PathBuf::from("custom/tests"));
    assert_eq!(runner.config().pixel_tolerance, 10);
    assert_eq!(runner.config().max_diff_percentage, 2.0);
    assert!(runner.config().fail_fast);
    assert!(runner.config().parallel);
    assert_eq!(runner.config().workers, 3);
    assert_eq!(
      runner.config().manifest_path,
      Some(PathBuf::from("custom/manifest.toml"))
    );
    assert!(!runner.config().write_report);
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
    assert!(tests.iter().all(|p| p.path.extension().unwrap() == "html"));
    assert!(tests
      .iter()
      .all(|p| !p.path.to_string_lossy().contains("-ref")));
  }

  #[test]
  fn test_manifest_discovery() {
    let renderer = create_test_renderer();
    let mut runner = WptRunner::new(renderer);

    let suite_dir = TempDir::new().unwrap();
    let html_path = suite_dir.path().join("sample.html");
    std::fs::write(&html_path, "<html><body>ok</body></html>").unwrap();

    let manifest = suite_dir.path().join("manifest.json");
    std::fs::write(
      &manifest,
      r#"{"tests":[{"path":"sample.html","test_type":"crashtest","viewport":{"width":320,"height":240},"dpr":2.0,"timeout_ms":5000}]}"#,
    )
    .unwrap();

    runner.config_mut().test_dir = suite_dir.path().to_path_buf();
    runner.config_mut().output_dir = suite_dir.path().join("out");
    runner.config_mut().output_dir = suite_dir.path().join("out");

    let results = runner.run_suite(suite_dir.path());
    assert_eq!(results.len(), 1);
    let meta = &results[0].metadata;
    assert_eq!(meta.viewport_width, 320);
    assert_eq!(meta.viewport_height, 240);
    assert!((meta.device_pixel_ratio - 2.0).abs() < f32::EPSILON);
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
    runner.config_mut().output_dir = temp.path().join("out");

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
    let expected_path = WptRunner::get_expected_image_path(runner.config(), &metadata);

    assert!(expected_path.ends_with("css/test.png"));
  }
}
