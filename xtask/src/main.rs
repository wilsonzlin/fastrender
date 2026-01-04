#![recursion_limit = "256"]

use anyhow::{anyhow, bail, Context, Result};
use clap::{Args, Parser, Subcommand, ValueEnum};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;
use url::Url;

mod chrome_baseline_fixtures;
mod fixture_chrome_diff;
mod fixture_determinism;
mod import_page_fixture;
mod recapture_page_fixtures;
mod sync_progress_accuracy;
mod update_pageset_guardrails;
mod update_pageset_guardrails_budgets;
mod validate_page_fixtures;

fn main() -> Result<()> {
  let cli = Cli::parse();

  match cli.command {
    Commands::Test(args) => run_tests(args),
    Commands::UpdateGoldens(args) => run_update_goldens(args),
    Commands::RenderPage(args) => run_render_page(args),
    Commands::Pageset(args) => run_pageset(args),
    Commands::PagesetDiff(args) => run_pageset_diff(args),
    Commands::DiffRenders(args) => run_diff_renders(args),
    Commands::FixtureChromeDiff(args) => fixture_chrome_diff::run_fixture_chrome_diff(args),
    Commands::FixtureDeterminism(args) => fixture_determinism::run_fixture_determinism(args),
    Commands::ImportPageFixture(args) => import_page_fixture::run_import_page_fixture(args),
    Commands::CaptureAccuracyFixtures(args) => run_capture_accuracy_fixtures(args),
    Commands::ChromeBaselineFixtures(args) => {
      chrome_baseline_fixtures::run_chrome_baseline_fixtures(args)
    }
    Commands::RecapturePageFixtures(args) => {
      recapture_page_fixtures::run_recapture_page_fixtures(args)
    }
    Commands::SyncProgressAccuracy(args) => {
      sync_progress_accuracy::run_sync_progress_accuracy(args)
    }
    Commands::UpdatePagesetGuardrails(args) => {
      update_pageset_guardrails::run_update_pageset_guardrails(args)
    }
    Commands::UpdatePagesetGuardrailsBudgets(args) => {
      update_pageset_guardrails_budgets::run_update_pageset_guardrails_budgets(args)
    }
    Commands::ValidatePageFixtures(args) => {
      validate_page_fixtures::run_validate_page_fixtures(args)
    }
    Commands::PerfSmoke(args) => run_perf_smoke(args),
  }
}

#[derive(Parser)]
#[command(
  name = "xtask",
  about = "Developer workflows for FastRender",
  arg_required_else_help = true,
  version
)]
struct Cli {
  #[command(subcommand)]
  command: Commands,
}

#[derive(Subcommand)]
enum Commands {
  /// Run common test subsets (core/style/fixtures/WPT)
  Test(TestArgs),
  /// Refresh checked-in render goldens (fixtures, reference images, WPT)
  UpdateGoldens(UpdateGoldensArgs),
  /// Render a single page via the fetch_and_render binary
  RenderPage(RenderPageArgs),
  /// Fetch pages, prefetch subresources, and update the committed pageset scoreboard (`progress/pages/*.json`)
  Pageset(PagesetArgs),
  /// Refresh the pageset scoreboard and compare against a baseline
  PagesetDiff(PagesetDiffArgs),
  /// Compare two render outputs and write visual diffs
  DiffRenders(DiffRendersArgs),
  /// Render offline fixtures in both Chrome and FastRender, then produce a diff report
  FixtureChromeDiff(fixture_chrome_diff::FixtureChromeDiffArgs),
  /// Render offline fixtures multiple times and report nondeterministic fixtures (pixel diffs between runs)
  FixtureDeterminism(fixture_determinism::FixtureDeterminismArgs),
  /// Convert a captured bundle into a pages_regression fixture
  ImportPageFixture(import_page_fixture::ImportPageFixtureArgs),
  /// Capture and import offline page fixtures for ok pages with the worst accuracy diffs.
  CaptureAccuracyFixtures(CaptureAccuracyFixturesArgs),
  /// Render offline page fixtures in headless Chrome/Chromium (deterministic JS-off baseline).
  ChromeBaselineFixtures(chrome_baseline_fixtures::ChromeBaselineFixturesArgs),
  /// (Re)capture and (re)import offline page fixtures from a manifest
  RecapturePageFixtures(recapture_page_fixtures::RecapturePageFixturesArgs),
  /// Sync deterministic fixture accuracy metrics into `progress/pages/*.json`
  SyncProgressAccuracy(sync_progress_accuracy::SyncProgressAccuracyArgs),
  /// Update `tests/pages/pageset_guardrails.json` based on `progress/pages/*.json`
  #[command(alias = "update-pageset-timeouts")]
  UpdatePagesetGuardrails(update_pageset_guardrails::UpdatePagesetGuardrailsArgs),
  /// Update `budget_ms` entries in `tests/pages/pageset_guardrails.json` based on offline `perf_smoke` timings
  ///
  /// Note: the legacy `update-pageset-timeout-budgets` command name is kept as an alias.
  #[command(alias = "update-pageset-timeout-budgets")]
  UpdatePagesetGuardrailsBudgets(
    update_pageset_guardrails_budgets::UpdatePagesetGuardrailsBudgetsArgs,
  ),
  /// Run the offline perf smoke harness (`perf_smoke` binary) over curated fixtures
  PerfSmoke(PerfSmokeArgs),
  /// Ensure checked-in page fixtures do not reference network resources (offline invariant)
  ValidatePageFixtures(validate_page_fixtures::ValidatePageFixturesArgs),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum PagesetAccuracyBaseline {
  /// Compare against an existing baseline directory only (do not invoke Chrome).
  Existing,
  /// Compare against headless Chrome baselines, auto-generating missing PNGs.
  Chrome,
}

#[derive(Args)]
struct PagesetArgs {
  /// Number of parallel fetch/render jobs
  #[arg(long, short, default_value_t = default_jobs())]
  jobs: usize,

  /// Timeout for network fetches (seconds)
  #[arg(long, default_value_t = 30)]
  fetch_timeout: u64,

  /// Allow caching pages that return HTTP error statuses (>= 400)
  ///
  /// This forwards `--allow-http-error-status` to the `fetch_pages` step only so callers can
  /// capture transient bot-mitigation pages (403/5xx) for debugging without breaking
  /// `pageset_progress` arg parsing.
  #[arg(long)]
  allow_http_error_status: bool,

  /// Allow duplicate stems by appending a deterministic hash suffix.
  ///
  /// This forwards `--allow-collisions` to `fetch_pages`. Most runs should leave this disabled so
  /// unexpected pageset stem collisions fail fast.
  #[arg(long)]
  allow_collisions: bool,

  /// Print per-page fetch durations for the `fetch_pages` step.
  #[arg(long = "timings")]
  fetch_timings: bool,

  /// Hard per-page render timeout (seconds)
  #[arg(long, default_value_t = 5)]
  render_timeout: u64,

  /// Fetch/render only listed pages (comma-separated URLs or cache stems)
  #[arg(long, value_delimiter = ',')]
  pages: Option<Vec<String>>,

  /// Process only a deterministic shard of the page set (index/total, 0-based)
  #[arg(long, value_parser = parse_shard)]
  shard: Option<(usize, usize)>,

  /// Skip fetching HTML and only prefetch/render from the existing cache
  #[arg(long)]
  no_fetch: bool,

  /// Re-fetch all pages even if cached (forwards `--refresh` to `fetch_pages`)
  #[arg(long, conflicts_with = "no_fetch")]
  refresh: bool,

  /// Force-enable the disk-backed cache (overrides NO_DISK_CACHE/DISK_CACHE)
  #[arg(long, conflicts_with = "no_disk_cache")]
  disk_cache: bool,

  /// Disable the disk-backed cache (defaults on; see NO_DISK_CACHE/DISK_CACHE)
  #[arg(long = "no-disk-cache", conflicts_with = "disk_cache")]
  no_disk_cache: bool,

  /// Override disk-backed subresource cache directory (defaults to fetches/assets)
  ///
  /// This is forwarded to both `prefetch_assets` and `pageset_progress` so the pageset workflow can
  /// use a non-default cache location (for example when keeping multiple caches side-by-side).
  #[arg(long, value_name = "DIR")]
  cache_dir: Option<PathBuf>,

  /// Override the User-Agent header (propagated to fetch/prefetch/render steps)
  #[arg(long)]
  user_agent: Option<String>,

  /// Override the Accept-Language header (propagated to fetch/prefetch/render steps)
  #[arg(long)]
  accept_language: Option<String>,

  /// Viewport size as WxH (e.g. 1200x800; propagated to prefetch/render steps)
  #[arg(long, value_parser = parse_viewport)]
  viewport: Option<(u32, u32)>,

  /// Device pixel ratio for media queries/srcset (propagated to prefetch/render steps)
  #[arg(long)]
  dpr: Option<f32>,

  /// Populate `diagnostics.stats.cascade` by re-running slow cascade pages/timeouts with cascade profiling enabled.
  ///
  /// This forwards `--cascade-diagnostics` to `pageset_progress run`.
  #[arg(long)]
  cascade_diagnostics: bool,

  /// Re-run ok pages whose cascade stage exceeds this threshold (ms) with cascade profiling enabled
  ///
  /// Defaults to the pageset_progress default (500ms) when unset.
  #[arg(long, value_name = "MS", requires = "cascade_diagnostics")]
  cascade_diagnostics_slow_ms: Option<u64>,

  /// Capture pixel-diff accuracy metrics against baseline renders (stored in progress JSON).
  #[arg(long)]
  accuracy: bool,

  /// Baseline strategy for accuracy capture.
  ///
  /// - `existing` (default): compare against an existing baseline directory; do not invoke Chrome.
  /// - `chrome`: forward `--baseline=chrome` so `pageset_progress` auto-generates missing baselines.
  #[arg(long, value_enum, requires = "accuracy")]
  accuracy_baseline: Option<PagesetAccuracyBaseline>,

  /// Directory containing baseline PNGs (e.g. `fetches/chrome_renders/<stem>.png`).
  ///
  /// Defaults to `fetches/chrome_renders` (matching `pageset_progress --baseline=chrome`).
  #[arg(long, value_name = "DIR", requires = "accuracy")]
  accuracy_baseline_dir: Option<PathBuf>,

  /// Per-channel tolerance for pixel diffs (0-255).
  ///
  /// Forwarded to `pageset_progress run --tolerance`.
  #[arg(long, requires = "accuracy")]
  accuracy_tolerance: Option<u8>,

  /// Maximum percent of pixels allowed to differ (0-100).
  ///
  /// Forwarded to `pageset_progress run --max-diff-percent`.
  #[arg(long, requires = "accuracy", value_name = "PERCENT")]
  accuracy_max_diff_percent: Option<f64>,

  /// Directory to write diff PNGs into (not committed).
  ///
  /// Forwarded to `pageset_progress run --diff-dir`.
  #[arg(long, value_name = "DIR", requires = "accuracy")]
  accuracy_diff_dir: Option<PathBuf>,

  /// After the pageset run, auto-capture deterministic offline fixtures for failing pages whose
  /// fixtures are missing.
  ///
  /// This scans `progress/pages/*.json`, finds pages with `status != ok`, and (when
  /// `tests/pages/fixtures/<stem>/index.html` is missing) captures a bundle from the warmed disk
  /// cache via `bundle_page cache`, then imports it via `cargo xtask import-page-fixture`.
  #[arg(long)]
  capture_missing_failure_fixtures: bool,

  /// Where to write intermediate `bundle_page cache` archives when capturing missing failure fixtures.
  #[arg(
    long,
    value_name = "DIR",
    default_value = "target/pageset_failure_fixture_bundles"
  )]
  capture_missing_failure_fixtures_out_dir: PathBuf,

  /// Allow missing subresources when capturing/importing missing failure fixtures.
  #[arg(long, requires = "capture_missing_failure_fixtures")]
  capture_missing_failure_fixtures_allow_missing_resources: bool,

  /// Allow replacing existing fixture directories when importing missing failure fixtures.
  #[arg(long, requires = "capture_missing_failure_fixtures")]
  capture_missing_failure_fixtures_overwrite: bool,

  /// After the pageset run, auto-capture deterministic offline fixtures for ok pages with the
  /// worst accuracy diffs whose fixtures are missing.
  ///
  /// This scans `progress/pages/*.json`, selects `status=ok` pages with an `accuracy.diff_percent`
  /// above `--capture-worst-accuracy-fixtures-min-diff-percent` and/or in the top-N worst diffs,
  /// then captures/imports missing fixtures from the warmed disk cache via `bundle_page cache` and
  /// `cargo xtask import-page-fixture`.
  #[arg(long)]
  capture_worst_accuracy_fixtures: bool,

  /// Where to write intermediate `bundle_page cache` archives when capturing worst accuracy fixtures.
  #[arg(
    long,
    value_name = "DIR",
    default_value = "target/pageset_accuracy_fixture_bundles"
  )]
  capture_worst_accuracy_fixtures_out_dir: PathBuf,

  /// Minimum diff percent threshold (0-100) for selecting ok pages with accuracy metrics.
  #[arg(long, default_value_t = 0.5)]
  capture_worst_accuracy_fixtures_min_diff_percent: f64,

  /// Include the top-N worst diff ok pages when selecting accuracy fixtures.
  #[arg(long, default_value_t = 10)]
  capture_worst_accuracy_fixtures_top: usize,

  /// Allow missing subresources when capturing/importing worst accuracy fixtures.
  #[arg(long, requires = "capture_worst_accuracy_fixtures")]
  capture_worst_accuracy_fixtures_allow_missing_resources: bool,

  /// Allow replacing existing fixture directories when importing worst accuracy fixtures.
  #[arg(long, requires = "capture_worst_accuracy_fixtures")]
  capture_worst_accuracy_fixtures_overwrite: bool,

  /// Extra arguments forwarded to `pageset_progress run` (use `--` before these).
  ///
  /// Note: when `prefetch_assets` supports prefetch toggles like `--prefetch-fonts` or
  /// `--prefetch-images`, those flags are intercepted here and forwarded to `prefetch_assets` (when
  /// it runs) instead so users can override the wrapper defaults without breaking
  /// `pageset_progress` arg parsing. Likewise, `--cache-dir` and `--disk-cache-*` flags are
  /// forwarded to `prefetch_assets` when possible so the warmed cache matches the render step.
  #[arg(last = true)]
  extra: Vec<String>,
}

#[derive(Args)]
struct CaptureAccuracyFixturesArgs {
  /// Directory containing pageset progress artifacts (`progress/pages/*.json`)
  #[arg(long, default_value = "progress/pages")]
  progress_dir: PathBuf,

  /// Root directory for offline page fixtures (defaults to tests/pages/fixtures)
  #[arg(long, default_value = "tests/pages/fixtures")]
  fixtures_root: PathBuf,

  /// Disk-backed subresource cache directory to read subresources from (`fetches/assets` by default).
  #[arg(long, value_name = "DIR", default_value = "fetches/assets")]
  asset_cache_dir: PathBuf,

  /// Where to write intermediate `bundle_page cache` archives.
  #[arg(
    long,
    value_name = "DIR",
    default_value = "target/pageset_accuracy_fixture_bundles"
  )]
  bundle_out_dir: PathBuf,

  /// Minimum diff percent threshold for selecting ok pages with accuracy metrics (0-100).
  #[arg(long, default_value_t = 0.5)]
  min_diff_percent: f64,

  /// Include the top-N worst diff pages (after filtering to ok pages with accuracy metrics).
  #[arg(long, default_value_t = 10)]
  top: usize,

  /// Replace missing subresources with empty placeholder assets instead of failing the capture/import.
  #[arg(long)]
  allow_missing_resources: bool,

  /// Allow replacing existing fixture directories when importing.
  #[arg(long)]
  overwrite: bool,

  /// Override the User-Agent header (propagated to `bundle_page cache`).
  #[arg(long)]
  user_agent: Option<String>,

  /// Override the Accept-Language header (propagated to `bundle_page cache`).
  #[arg(long)]
  accept_language: Option<String>,

  /// Viewport size as WxH (e.g. 1200x800; propagated to `bundle_page cache`).
  #[arg(long, value_parser = parse_viewport)]
  viewport: Option<(u32, u32)>,

  /// Device pixel ratio for media queries/srcset (propagated to `bundle_page cache`).
  #[arg(long)]
  dpr: Option<f32>,
}

fn default_jobs() -> usize {
  cpu_budget()
}

fn cpu_budget() -> usize {
  fastrender::system::cpu_budget()
}

#[derive(Args)]
struct PagesetDiffArgs {
  /// Skip running `cargo xtask pageset` before comparing progress files
  #[arg(long)]
  no_run: bool,

  /// Baseline progress directory (defaults to extracting progress/pages from a git ref)
  #[arg(long)]
  baseline: Option<PathBuf>,

  /// Git ref to read `progress/pages` from when building the baseline (defaults to HEAD)
  #[arg(long)]
  baseline_ref: Option<String>,

  /// Exit non-zero when regressions are detected relative to the baseline
  #[arg(long)]
  fail_on_regression: bool,

  /// Exit non-zero when visual diff accuracy regresses vs the baseline (when `accuracy` metrics are present)
  #[arg(long)]
  fail_on_accuracy_regression: bool,

  /// Accuracy regression threshold (percent) for `--fail-on-accuracy-regression`
  #[arg(long, value_name = "PERCENT", requires = "fail_on_accuracy_regression")]
  accuracy_regression_threshold_percent: Option<f64>,

  /// Exit non-zero when timeout/panic/error pages are missing `failure_stage`/`timeout_stage`
  ///
  /// When `--fail-on-regression` is set, this gate is enabled automatically unless explicitly
  /// disabled with `--no-fail-on-missing-stages`.
  #[arg(long)]
  fail_on_missing_stages: bool,

  /// Disable the implicit `--fail-on-missing-stages` gate enabled by `--fail-on-regression`
  #[arg(
    long = "no-fail-on-missing-stages",
    conflicts_with = "fail_on_missing_stages"
  )]
  no_fail_on_missing_stages: bool,

  /// Exit non-zero when ok pages are missing per-stage timings (`stages_ms`)
  ///
  /// When `--fail-on-regression` is set, this gate is enabled automatically unless explicitly
  /// disabled with `--no-fail-on-missing-stage-timings`.
  #[arg(long)]
  fail_on_missing_stage_timings: bool,

  /// Disable the implicit `--fail-on-missing-stage-timings` gate enabled by `--fail-on-regression`
  #[arg(
    long = "no-fail-on-missing-stage-timings",
    conflicts_with = "fail_on_missing_stage_timings"
  )]
  no_fail_on_missing_stage_timings: bool,

  /// Exit non-zero when ok pages exceed this total_ms threshold (ms)
  ///
  /// When `--fail-on-regression` is set, this gate defaults to 5000ms unless explicitly disabled
  /// with `--no-fail-on-slow-ok`.
  #[arg(long, value_name = "MS")]
  fail_on_slow_ok_ms: Option<u64>,

  /// Disable the implicit `--fail-on-slow-ok-ms=5000` gate enabled by `--fail-on-regression`
  #[arg(long = "no-fail-on-slow-ok", conflicts_with = "fail_on_slow_ok_ms")]
  no_fail_on_slow_ok: bool,

  #[command(flatten)]
  pageset: PagesetArgs,
}

#[derive(Args)]
struct TestArgs {
  /// Which test suite to run (core by default)
  #[arg(value_enum, default_value_t = TestSuite::Core)]
  suite: TestSuite,

  /// Build and run tests in release mode
  #[arg(long)]
  release: bool,

  /// Extra arguments forwarded to `cargo test`
  #[arg(last = true)]
  extra: Vec<String>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum TestSuite {
  /// Full default test suite (`cargo test --quiet`)
  Core,
  /// Style regression harness (`cargo test --quiet --test style_tests`)
  Style,
  /// Visual fixtures (`cargo test --quiet fixtures`)
  Fixtures,
  /// Local WPT harness (`cargo test --quiet wpt_local_suite_passes -- --exact`)
  Wpt,
  /// Run all of the above sequentially
  All,
}

#[derive(Args)]
struct UpdateGoldensArgs {
  /// Which golden set to refresh (fixtures + ref + WPT by default)
  #[arg(value_enum, default_value_t = GoldenSuite::All)]
  suite: GoldenSuite,

  /// Build in release mode for faster re-renders
  #[arg(long)]
  release: bool,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum GoldenSuite {
  /// Refresh fixtures, reference images, and WPT expected outputs
  All,
  /// Refresh `tests/fixtures/golden/*`
  Fixtures,
  /// Refresh reference images under `tests/ref/fixtures/*`
  Reference,
  /// Refresh WPT expected images under `tests/wpt/expected/*`
  Wpt,
}

#[derive(Args)]
struct RenderPageArgs {
  /// HTTP/HTTPS URL to fetch and render
  #[arg(long, conflicts_with = "file", required_unless_present = "file")]
  url: Option<String>,

  /// Local HTML file to render (converted to file://)
  #[arg(long, conflicts_with = "url", required_unless_present = "url")]
  file: Option<PathBuf>,

  /// Output PNG path (defaults to fetch_and_render's <url>.png naming)
  #[arg(long)]
  output: Option<PathBuf>,

  /// Viewport size as WxH (e.g. 1200x800)
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset
  #[arg(long, default_value_t = 1.0)]
  dpr: f32,

  /// Expand render target to full content height
  #[arg(long)]
  full_page: bool,

  /// Per-page timeout in seconds (0 = no timeout)
  #[arg(long, default_value_t = 0)]
  timeout: u64,

  /// Run fetch_and_render in debug mode (release by default)
  #[arg(long)]
  debug: bool,

  /// Additional arguments forwarded directly to fetch_and_render
  #[arg(last = true)]
  extra: Vec<String>,
}

#[derive(Args)]
struct DiffRendersArgs {
  /// Directory or PNG file with baseline renders
  #[arg(long)]
  before: PathBuf,

  /// Directory or PNG file with new renders
  #[arg(long)]
  after: PathBuf,

  /// Directory to write the diff report into (`diff_report.html`/`diff_report.json` + artifacts).
  #[arg(long, default_value = "target/render-diffs")]
  output: PathBuf,

  /// Exit non-zero when differences/missing/errors are present in the report.
  ///
  /// By default `cargo xtask diff-renders` exits 0 even when diffs are found so it can be used as a
  /// non-gating local inspection loop.
  #[arg(long)]
  fail_on_differences: bool,

  /// Skip building `diff_renders` and reuse an existing `target/release/diff_renders` binary.
  ///
  /// Useful when iterating quickly on report parameters without rebuilding.
  #[arg(long)]
  no_build: bool,

  /// Per-channel tolerance (0 = exact match, 5-10 to ignore tiny AA differences)
  #[arg(long, default_value_t = 0)]
  threshold: u8,

  /// Maximum percent of pixels allowed to differ (0-100).
  ///
  /// Defaults to 0 so any differences are flagged, matching the historical `cargo xtask diff-renders`
  /// behavior.
  #[arg(long, default_value_t = 0.0)]
  max_diff_percent: f64,

  /// Maximum allowed perceptual distance (0.0 = identical).
  ///
  /// When unset, only pixel-based thresholds are enforced.
  #[arg(long)]
  max_perceptual_distance: Option<f64>,

  /// Sort report entries by metric within each status group.
  #[arg(long, value_enum, default_value_t = DiffRendersSortBy::Percent)]
  sort_by: DiffRendersSortBy,

  /// Ignore alpha differences (forwarded to `diff_renders --ignore-alpha`).
  #[arg(long)]
  ignore_alpha: bool,

  /// Only diff a deterministic shard of the inputs (index/total, 0-based)
  #[arg(long, value_parser = parse_shard)]
  shard: Option<(usize, usize)>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum DiffRendersSortBy {
  Pixel,
  Percent,
  Perceptual,
}

impl DiffRendersSortBy {
  fn as_cli_value(self) -> &'static str {
    match self {
      Self::Pixel => "pixel",
      Self::Percent => "percent",
      Self::Perceptual => "perceptual",
    }
  }
}

#[derive(Args)]
struct PerfSmokeArgs {
  /// Which fixture suite to run (core/pageset-guardrails/all)
  #[arg(long, value_enum, default_value_t = PerfSmokeSuite::Core)]
  suite: PerfSmokeSuite,

  /// Only run fixtures matching these names (comma-separated)
  #[arg(long, value_delimiter = ',')]
  only: Option<Vec<String>>,

  /// Run each fixture in its own process to keep memory bounded.
  ///
  /// The `pageset-guardrails` suite enables this automatically (unless `--no-isolate` is set).
  #[arg(long, conflicts_with = "no_isolate")]
  isolate: bool,

  /// Disable per-fixture isolation.
  ///
  /// Note: the underlying `perf_smoke` binary auto-enables isolation for `--suite pageset-guardrails`.
  #[arg(long, conflicts_with = "isolate")]
  no_isolate: bool,

  /// Print the slowest N fixtures
  #[arg(long)]
  top: Option<usize>,

  /// Baseline JSON path for regression detection
  #[arg(long)]
  baseline: Option<PathBuf>,

  /// Relative regression threshold (e.g. 0.05 = 5%)
  ///
  /// Defaults to the `perf_smoke` binary default.
  #[arg(long)]
  threshold: Option<f64>,

  /// Relative regression threshold for deterministic count metrics (e.g. 0.20 = 20%)
  ///
  /// Applies to stable complexity counters such as DOM nodes, fragments, glyphs, and display
  /// items. Defaults to the `perf_smoke` binary default.
  #[arg(long)]
  count_threshold: Option<f64>,

  /// Where to write the perf smoke JSON report
  ///
  /// Defaults to the `perf_smoke` binary default (`target/perf_smoke.json`).
  #[arg(long)]
  output: Option<PathBuf>,

  /// Fail when regressions are detected
  #[arg(long)]
  fail_on_regression: bool,

  /// Stop after the first fixture failure instead of continuing to the end of the suite.
  #[arg(long)]
  fail_fast: bool,

  /// Fail the command when any fixture fails (status=error|panic|timeout).
  ///
  /// Defaults to the `perf_smoke` binary default (enabled in CI).
  #[arg(long, conflicts_with = "no_fail_on_failure")]
  fail_on_failure: bool,

  /// Disable the default CI behavior that enables `--fail-on-failure`.
  #[arg(long = "no-fail-on-failure", conflicts_with = "fail_on_failure")]
  no_fail_on_failure: bool,

  /// Fail when a pageset-guardrails manifest fixture is missing locally.
  ///
  /// When running `--suite pageset-guardrails` via `cargo xtask perf-smoke`, this gate is enabled by
  /// default unless explicitly disabled with `--allow-missing-fixtures`.
  #[arg(long, conflicts_with = "allow_missing_fixtures")]
  fail_on_missing_fixtures: bool,

  /// Allow missing pageset-guardrails fixtures (skip them instead of failing).
  ///
  /// This disables the default `--fail-on-missing-fixtures` behavior for the `pageset-guardrails`
  /// suite when running via `cargo xtask perf-smoke`.
  #[arg(long, conflicts_with = "fail_on_missing_fixtures")]
  allow_missing_fixtures: bool,

  /// Fail when any fixture exceeds its `budget_ms` (if provided).
  #[arg(long)]
  fail_on_budget: bool,

  /// Fail when any fixture encounters subresource fetch errors (offline fixture completeness).
  #[arg(long)]
  fail_on_fetch_errors: bool,

  /// Run the perf smoke harness in debug mode instead of release
  #[arg(long)]
  debug: bool,

  /// Extra arguments forwarded to the `perf_smoke` binary (use `--` before these)
  #[arg(last = true)]
  extra: Vec<String>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum PerfSmokeSuite {
  Core,
  #[value(alias = "pageset-timeouts")]
  PagesetGuardrails,
  All,
}

impl PerfSmokeSuite {
  fn as_cli_value(self) -> &'static str {
    match self {
      Self::Core => "core",
      Self::PagesetGuardrails => "pageset-guardrails",
      Self::All => "all",
    }
  }
}

fn run_tests(args: TestArgs) -> Result<()> {
  // Always run from the repository root so any relative fixture paths used by tests behave
  // consistently even when `cargo xtask` is invoked from a subdirectory.
  let repo_root = repo_root();

  let suites = match args.suite {
    TestSuite::All => vec![
      TestSuite::Core,
      TestSuite::Style,
      TestSuite::Fixtures,
      TestSuite::Wpt,
    ],
    suite => vec![suite],
  };

  if args.suite == TestSuite::All && !args.extra.is_empty() {
    bail!("extra arguments are not supported with the \"all\" suite");
  }

  for suite in suites {
    let mut cmd = Command::new("cargo");
    cmd.arg("test").arg("--quiet");
    if args.release {
      cmd.arg("--release");
    }

    match suite {
      TestSuite::Core => {}
      TestSuite::Style => {
        cmd.args(["--test", "style_tests"]);
      }
      TestSuite::Fixtures => {
        cmd.arg("fixtures");
      }
      TestSuite::Wpt => {
        cmd.arg("wpt_local_suite_passes");
        cmd.arg("--");
        cmd.arg("--exact");
      }
      TestSuite::All => unreachable!("expanded above"),
    }

    cmd.args(&args.extra);

    println!("Running {suite:?} tests...");
    cmd.current_dir(&repo_root);
    run_command(cmd)?;
  }

  Ok(())
}

fn run_update_goldens(args: UpdateGoldensArgs) -> Result<()> {
  // Like `run_tests`, ensure the invoked `cargo test` runs from the repo root so relative paths are
  // stable.
  let repo_root = repo_root();

  let suites = match args.suite {
    GoldenSuite::All => vec![
      GoldenSuite::Fixtures,
      GoldenSuite::Reference,
      GoldenSuite::Wpt,
    ],
    suite => vec![suite],
  };

  for suite in suites {
    let mut cmd = Command::new("cargo");
    cmd.arg("test").arg("--quiet");
    if args.release {
      cmd.arg("--release");
    }

    match suite {
      GoldenSuite::Fixtures => {
        cmd.env("UPDATE_GOLDEN", "1");
        cmd.arg("fixtures");
      }
      GoldenSuite::Reference => {
        cmd.env("UPDATE_GOLDEN", "1");
        cmd.args([
          "--test",
          "ref_tests",
          "form_controls_reference_image_matches_golden",
        ]);
        cmd.arg("--");
        cmd.arg("--exact");
      }
      GoldenSuite::Wpt => {
        cmd.env("UPDATE_WPT_EXPECTED", "1");
        cmd.arg("wpt_local_suite_passes");
        cmd.arg("--");
        cmd.arg("--exact");
      }
      GoldenSuite::All => unreachable!("expanded above"),
    }

    println!("Updating {suite:?} goldens...");
    cmd.current_dir(&repo_root);
    run_command(cmd)?;
  }

  Ok(())
}

fn run_pageset(args: PagesetArgs) -> Result<()> {
  // Always run pageset tooling from the repository root so progress files, caches, and fixtures
  // land in the expected locations regardless of the caller's current working directory.
  let repo_root = repo_root();

  let mut jobs = args.jobs;
  let mut fetch_timeout = args.fetch_timeout;
  let mut render_timeout = args.render_timeout;
  let mut no_fetch = args.no_fetch;

  let rayon_threads_env = std::env::var_os("RAYON_NUM_THREADS");
  let layout_parallel_env = std::env::var_os("FASTR_LAYOUT_PARALLEL");

  let mut pages_arg = args.pages.as_ref().map(|pages| pages.join(","));
  let mut shard_arg = args.shard.map(|(index, total)| format!("{index}/{total}"));
  let mut user_agent_arg = args.user_agent.clone();
  let mut accept_language_arg = args.accept_language.clone();
  let mut viewport_arg = args
    .viewport
    .map(|(width, height)| format!("{width}x{height}"));
  let mut dpr_arg = args.dpr.map(|dpr| dpr.to_string());
  let mut cache_dir_arg = args.cache_dir.clone();

  let mut pageset_extra_args = args.extra.clone();

  let mut fetch_refresh = args.refresh;
  let mut fetch_allow_http_error_status = args.allow_http_error_status;
  let mut fetch_allow_collisions = args.allow_collisions;
  let mut fetch_timings = args.fetch_timings;
  let (filtered_pageset_extra_args, fetch_overrides) =
    xtask::extract_fetch_pages_flag_overrides(&pageset_extra_args);
  pageset_extra_args = filtered_pageset_extra_args;
  fetch_refresh |= fetch_overrides.refresh;
  fetch_allow_http_error_status |= fetch_overrides.allow_http_error_status;
  fetch_allow_collisions |= fetch_overrides.allow_collisions;
  fetch_timings |= fetch_overrides.timings;

  let (filtered_pageset_extra_args, pageset_overrides) =
    xtask::extract_pageset_extra_arg_overrides(&pageset_extra_args);
  pageset_extra_args = filtered_pageset_extra_args;

  if let Some(jobs_override) = pageset_overrides.jobs {
    jobs = jobs_override
      .parse::<usize>()
      .with_context(|| format!("failed to parse --jobs value \"{jobs_override}\""))?;
  }
  if jobs == 0 {
    bail!("jobs must be > 0");
  }

  // `pageset_progress` runs up to `jobs` worker processes in parallel (one per page). The renderer
  // itself can also use Rayon threads (e.g., layout fan-out, CSS selector work, etc). Without a
  // cap, running N worker processes on a machine with M logical CPUs can oversubscribe
  // catastrophically (N * M threads).
  //
  // Mirror `scripts/pageset.sh`: if the user hasn't provided `RAYON_NUM_THREADS`, divide the
  // available CPU budget across the configured worker processes.
  let total_cpus = cpu_budget();
  let mut threads_per_worker = total_cpus / jobs;
  if threads_per_worker == 0 {
    threads_per_worker = 1;
  }

  if let Some(pages) = pageset_overrides.pages {
    match &pages_arg {
      Some(existing) if existing != &pages => bail!(
        "--pages cannot be passed both before and after `--` with different values (before: {existing}, after: {pages})"
      ),
      _ => pages_arg = Some(pages),
    }
  }
  if let Some(shard) = pageset_overrides.shard {
    match &shard_arg {
      Some(existing) if existing != &shard => bail!(
        "--shard cannot be passed both before and after `--` with different values (before: {existing}, after: {shard})"
      ),
      _ => shard_arg = Some(shard),
    }
  }
  if let Some(user_agent) = pageset_overrides.user_agent {
    match &user_agent_arg {
      Some(existing) if existing != &user_agent => bail!(
        "--user-agent cannot be passed both before and after `--` with different values (before: {existing}, after: {user_agent})"
      ),
      _ => user_agent_arg = Some(user_agent),
    }
  }
  if let Some(accept_language) = pageset_overrides.accept_language {
    match &accept_language_arg {
      Some(existing) if existing != &accept_language => bail!(
        "--accept-language cannot be passed both before and after `--` with different values (before: {existing}, after: {accept_language})"
      ),
      _ => accept_language_arg = Some(accept_language),
    }
  }
  if let Some(viewport) = pageset_overrides.viewport {
    match &viewport_arg {
      Some(existing) if existing != &viewport => bail!(
        "--viewport cannot be passed both before and after `--` with different values (before: {existing}, after: {viewport})"
      ),
      _ => viewport_arg = Some(viewport),
    }
  }
  if let Some(dpr) = pageset_overrides.dpr {
    match &dpr_arg {
      Some(existing) if existing != &dpr => bail!(
        "--dpr cannot be passed both before and after `--` with different values (before: {existing}, after: {dpr})"
      ),
      _ => dpr_arg = Some(dpr),
    }
  }
  if let Some(cache_dir) = pageset_overrides.cache_dir {
    let cache_dir = PathBuf::from(cache_dir);
    match &cache_dir_arg {
      Some(existing) if existing != &cache_dir => bail!(
        "--cache-dir cannot be passed both before and after `--` with different values (before: {}, after: {})",
        existing.display(),
        cache_dir.display()
      ),
      _ => cache_dir_arg = Some(cache_dir),
    }
  }
  if pageset_overrides.no_fetch {
    no_fetch = true;
  }
  if let Some(timeout) = pageset_overrides.fetch_timeout {
    fetch_timeout = timeout
      .parse::<u64>()
      .with_context(|| format!("failed to parse --fetch-timeout value \"{timeout}\""))?;
  }
  if let Some(timeout) = pageset_overrides.render_timeout {
    render_timeout = timeout
      .parse::<u64>()
      .with_context(|| format!("failed to parse --render-timeout value \"{timeout}\""))?;
  }
  if no_fetch && fetch_refresh {
    bail!("--refresh cannot be used with --no-fetch");
  }

  let mut disk_cache_override = if args.disk_cache {
    Some(true)
  } else if args.no_disk_cache {
    Some(false)
  } else {
    None
  };
  if let Some(disk_cache) = pageset_overrides.disk_cache {
    match disk_cache_override {
      Some(existing) if existing != disk_cache => bail!(
        "--disk-cache/--no-disk-cache cannot be passed both before and after `--` with different values"
      ),
      _ => disk_cache_override = Some(disk_cache),
    }
  }

  let disk_cache_enabled = disk_cache_enabled(disk_cache_override);
  let cache_dir = cache_dir_arg.unwrap_or_else(|| PathBuf::from("fetches/assets"));
  let disk_cache_status = if disk_cache_enabled {
    "enabled"
  } else {
    "disabled"
  };
  let apply_disk_cache_env = |cmd: &mut Command| {
    if disk_cache_enabled {
      cmd.env("DISK_CACHE", "1");
      cmd.env_remove("NO_DISK_CACHE");
    } else {
      cmd.env("DISK_CACHE", "0");
      cmd.env("NO_DISK_CACHE", "1");
    }
  };

  let prefetch_support = if disk_cache_enabled {
    query_prefetch_assets_support(disk_cache_enabled)?
  } else {
    // Mirror `scripts/pageset.sh`: prefetch-specific flags don't apply when disk cache is disabled
    // (the prefetch step is skipped), but we still intercept them so pageset_progress arg parsing
    // stays forgiving.
    xtask::PrefetchAssetsSupport::assume_supported()
  };
  let (mut prefetch_asset_args, filtered_pageset_extra_args) =
    xtask::extract_prefetch_assets_args(&pageset_extra_args, prefetch_support);
  pageset_extra_args = filtered_pageset_extra_args;

  let mut disk_cache_extra_args = extract_disk_cache_args(&pageset_extra_args);
  if disk_cache_enabled
    && std::env::var_os("FASTR_DISK_CACHE_ALLOW_NO_STORE").is_none()
    && !disk_cache_extra_args.iter().any(|arg| {
      arg == "--disk-cache-allow-no-store" || arg.starts_with("--disk-cache-allow-no-store=")
    })
  {
    disk_cache_extra_args.push("--disk-cache-allow-no-store".to_string());
    pageset_extra_args.push("--disk-cache-allow-no-store".to_string());
  }

  if disk_cache_enabled
    && prefetch_support.prefetch_images
    && !prefetch_asset_args
      .iter()
      .any(|arg| arg == "--prefetch-images" || arg.starts_with("--prefetch-images="))
  {
    prefetch_asset_args.push("--prefetch-images".to_string());
  }
  if disk_cache_enabled
    && prefetch_support.prefetch_css_url_assets
    && !prefetch_asset_args.iter().any(|arg| {
      arg == "--prefetch-css-url-assets" || arg.starts_with("--prefetch-css-url-assets=")
    })
  {
    prefetch_asset_args.push("--prefetch-css-url-assets".to_string());
  }
  if disk_cache_enabled {
    println!(
      "Disk cache enabled (persisting subresources under {}). \
       Set NO_DISK_CACHE=1, DISK_CACHE=0, or --no-disk-cache to disable.",
      cache_dir.display()
    );
  } else {
    println!(
      "Disk cache disabled (NO_DISK_CACHE/DISK_CACHE/--no-disk-cache); subresources will be refetched each run. \
       Pass --disk-cache to force-enable."
    );
  }

  if !no_fetch {
    let mut cmd = Command::new("cargo");
    cmd
      .arg("run")
      .arg("--release")
      .apply_disk_cache_feature(disk_cache_enabled)
      .args(["--bin", "fetch_pages"])
      .arg("--")
      .arg("--jobs")
      .arg(jobs.to_string())
      .arg("--timeout")
      .arg(fetch_timeout.to_string());
    if let Some(pages) = &pages_arg {
      cmd.arg("--pages").arg(pages);
    }
    if let Some(shard) = &shard_arg {
      cmd.arg("--shard").arg(shard);
    }
    if let Some(user_agent) = &user_agent_arg {
      cmd.arg("--user-agent").arg(user_agent);
    }
    if let Some(accept_language) = &accept_language_arg {
      cmd.arg("--accept-language").arg(accept_language);
    }
    if fetch_allow_collisions {
      cmd.arg("--allow-collisions");
    }
    if fetch_refresh {
      cmd.arg("--refresh");
    }
    if fetch_allow_http_error_status {
      cmd.arg("--allow-http-error-status");
    }
    if fetch_timings {
      cmd.arg("--timings");
    }
    if rayon_threads_env.is_none() {
      cmd.env("RAYON_NUM_THREADS", threads_per_worker.to_string());
    }
    if layout_parallel_env.is_none() {
      cmd.env("FASTR_LAYOUT_PARALLEL", "auto");
    }
    apply_disk_cache_env(&mut cmd);
    cmd.current_dir(&repo_root);
    println!(
      "Updating cached pages (jobs={}, timeout={}s, disk_cache={})...",
      jobs, fetch_timeout, disk_cache_status
    );
    run_command(cmd)?;
  }

  if disk_cache_enabled {
    let mut cmd = Command::new("cargo");
    cmd
      .arg("run")
      .arg("--release")
      .apply_disk_cache_feature(true)
      .args(["--bin", "prefetch_assets"])
      .arg("--")
      .arg("--jobs")
      .arg(jobs.to_string())
      .arg("--timeout")
      .arg(fetch_timeout.to_string());
    if let Some(pages) = &pages_arg {
      cmd.arg("--pages").arg(pages);
    }
    if let Some(shard) = &shard_arg {
      cmd.arg("--shard").arg(shard);
    }
    if let Some(user_agent) = &user_agent_arg {
      cmd.arg("--user-agent").arg(user_agent);
    }
    if let Some(accept_language) = &accept_language_arg {
      cmd.arg("--accept-language").arg(accept_language);
    }
    if let Some(viewport) = &viewport_arg {
      cmd.arg("--viewport").arg(viewport);
    }
    if let Some(dpr) = &dpr_arg {
      cmd.arg("--dpr").arg(dpr);
    }
    cmd.args(&prefetch_asset_args);
    cmd.args(&disk_cache_extra_args);
    if rayon_threads_env.is_none() {
      cmd.env("RAYON_NUM_THREADS", threads_per_worker.to_string());
    }
    if layout_parallel_env.is_none() {
      cmd.env("FASTR_LAYOUT_PARALLEL", "auto");
    }
    apply_disk_cache_env(&mut cmd);
    cmd.current_dir(&repo_root);
    println!(
      "Prefetching subresources into {} (jobs={}, timeout={}s)...",
      cache_dir.display(),
      jobs,
      fetch_timeout
    );
    run_command(cmd)?;
  }

  let mut cmd = Command::new("cargo");
  cmd
    .arg("run")
    .arg("--release")
    .apply_disk_cache_feature(disk_cache_enabled)
    .args(["--bin", "pageset_progress"])
    .arg("--")
    .arg("run")
    .arg("--jobs")
    .arg(jobs.to_string())
    .arg("--timeout")
    .arg(render_timeout.to_string())
    .arg("--bundled-fonts");
  cmd.arg("--cache-dir").arg(&cache_dir);
  if let Some(pages) = &pages_arg {
    cmd.arg("--pages").arg(pages);
  }
  if let Some(shard) = &shard_arg {
    cmd.arg("--shard").arg(shard);
  }
  if let Some(user_agent) = &user_agent_arg {
    cmd.arg("--user-agent").arg(user_agent);
  }
  if let Some(accept_language) = &accept_language_arg {
    cmd.arg("--accept-language").arg(accept_language);
  }
  if let Some(viewport) = &viewport_arg {
    cmd.arg("--viewport").arg(viewport);
  }
  if let Some(dpr) = &dpr_arg {
    cmd.arg("--dpr").arg(dpr);
  }
  if args.cascade_diagnostics {
    cmd.arg("--cascade-diagnostics");
    if let Some(ms) = args.cascade_diagnostics_slow_ms {
      cmd.arg("--cascade-diagnostics-slow-ms").arg(ms.to_string());
    }
  }
  if args.accuracy {
    let extra_has_accuracy = pageset_extra_args.iter().any(|arg| arg == "--accuracy");
    let extra_has_baseline = pageset_extra_args
      .iter()
      .any(|arg| arg == "--baseline" || arg.starts_with("--baseline="));
    let extra_has_baseline_dir = pageset_extra_args
      .iter()
      .any(|arg| arg == "--baseline-dir" || arg.starts_with("--baseline-dir="));
    let extra_has_tolerance = pageset_extra_args
      .iter()
      .any(|arg| arg == "--tolerance" || arg.starts_with("--tolerance="));
    let extra_has_max_diff_percent = pageset_extra_args
      .iter()
      .any(|arg| arg == "--max-diff-percent" || arg.starts_with("--max-diff-percent="));
    let extra_has_diff_dir = pageset_extra_args
      .iter()
      .any(|arg| arg == "--diff-dir" || arg.starts_with("--diff-dir="));

    if !extra_has_accuracy {
      cmd.arg("--accuracy");
    }

    let baseline = args.accuracy_baseline.unwrap_or(PagesetAccuracyBaseline::Existing);
    let baseline_dir = args
      .accuracy_baseline_dir
      .clone()
      .unwrap_or_else(|| PathBuf::from("fetches/chrome_renders"));

    match baseline {
      PagesetAccuracyBaseline::Existing => {
        if !extra_has_baseline && !extra_has_baseline_dir {
          cmd.arg("--baseline-dir").arg(&baseline_dir);
        }
      }
      PagesetAccuracyBaseline::Chrome => {
        if !extra_has_baseline {
          cmd.arg("--baseline=chrome");
        }
        if !extra_has_baseline_dir {
          cmd.arg("--baseline-dir").arg(&baseline_dir);
        }
      }
    }

    if let Some(tolerance) = args.accuracy_tolerance {
      if !extra_has_tolerance {
        cmd.arg("--tolerance").arg(tolerance.to_string());
      }
    }
    if let Some(max) = args.accuracy_max_diff_percent {
      if !extra_has_max_diff_percent {
        cmd.arg("--max-diff-percent").arg(max.to_string());
      }
    }
    if let Some(dir) = &args.accuracy_diff_dir {
      if !extra_has_diff_dir {
        cmd.arg("--diff-dir").arg(dir);
      }
    }
  }
  cmd.args(&pageset_extra_args);
  if rayon_threads_env.is_none() {
    cmd.env("RAYON_NUM_THREADS", threads_per_worker.to_string());
  }
  if layout_parallel_env.is_none() {
    cmd.env("FASTR_LAYOUT_PARALLEL", "auto");
  }
  apply_disk_cache_env(&mut cmd);
  cmd.current_dir(&repo_root);
  println!(
    "Updating progress/pages scoreboard (jobs={}, hard timeout={}s, disk_cache={}, cache_dir={})...",
    jobs,
    render_timeout,
    disk_cache_status,
    cache_dir.display()
  );
  run_command(cmd)?;

  if args.capture_missing_failure_fixtures {
    if !disk_cache_enabled {
      eprintln!(
        "Warning: --capture-missing-failure-fixtures requested, but the pageset disk cache is disabled. \
         Skipping because `bundle_page cache` requires cached subresources."
      );
    } else {
      println!();
      println!("Capturing missing failure fixtures from the warmed disk cache...");

      let plan_args = xtask::capture_missing_failure_fixtures::CaptureMissingFailureFixturesArgs {
        progress_dir: repo_root.join("progress/pages"),
        fixtures_root: repo_root.join("tests/pages/fixtures"),
        bundle_out_dir: if args.capture_missing_failure_fixtures_out_dir.is_absolute() {
          args.capture_missing_failure_fixtures_out_dir.clone()
        } else {
          repo_root.join(&args.capture_missing_failure_fixtures_out_dir)
        },
        asset_cache_dir: cache_dir.clone(),
        user_agent: user_agent_arg.clone(),
        accept_language: accept_language_arg.clone(),
        viewport: viewport_arg.clone(),
        dpr: dpr_arg.clone(),
        allow_missing_resources: args.capture_missing_failure_fixtures_allow_missing_resources,
        overwrite: args.capture_missing_failure_fixtures_overwrite,
      };

      let plan =
        xtask::capture_missing_failure_fixtures::plan_capture_missing_failure_fixtures(&plan_args)?;
      let xtask::capture_missing_failure_fixtures::CaptureMissingFailureFixturesPlan {
        failing_pages_total,
        fixtures_already_present,
        captures,
      } = plan;

      println!("Failing pages discovered: {failing_pages_total}");
      println!("Fixtures already present: {fixtures_already_present}");
      println!("Missing fixtures to capture: {}", captures.len());

      if !captures.is_empty() {
        fs::create_dir_all(&plan_args.bundle_out_dir).with_context(|| {
          format!(
            "failed to create bundle output directory {}",
            plan_args.bundle_out_dir.display()
          )
        })?;
      }

      let mut bundles_captured = 0usize;
      let mut fixtures_imported = 0usize;
      let mut failures: Vec<(String, String)> = Vec::new();

      for capture in captures {
        if capture.bundle_path.exists() {
          if capture.bundle_path.is_dir() {
            fs::remove_dir_all(&capture.bundle_path).with_context(|| {
              format!(
                "failed to remove existing bundle {}",
                capture.bundle_path.display()
              )
            })?;
          } else {
            fs::remove_file(&capture.bundle_path).with_context(|| {
              format!(
                "failed to remove existing bundle {}",
                capture.bundle_path.display()
              )
            })?;
          }
        }

        let mut bundle_cmd = capture.bundle_command.to_command();
        bundle_cmd.current_dir(&repo_root);
        let bundle_result =
          run_command(bundle_cmd).with_context(|| format!("bundle_page cache {}", capture.stem));
        if let Err(err) = bundle_result {
          failures.push((capture.stem.clone(), format!("capture failed: {err}")));
          continue;
        }
        bundles_captured += 1;

        let mut import_cmd = capture.import_command.to_command();
        import_cmd.current_dir(&repo_root);
        let import_result =
          run_command(import_cmd).with_context(|| format!("import-page-fixture {}", capture.stem));
        if let Err(err) = import_result {
          failures.push((capture.stem.clone(), format!("import failed: {err}")));
          continue;
        }
        fixtures_imported += 1;
      }

      println!();
      println!("capture-missing-failure-fixtures summary:");
      println!("  failing pages discovered   {failing_pages_total}");
      println!("  fixtures already present   {fixtures_already_present}");
      println!("  bundles captured           {bundles_captured}");
      println!("  fixtures imported          {fixtures_imported}");
      println!("  failures                   {}", failures.len());

      if !failures.is_empty() {
        println!();
        println!("Failures:");
        for (stem, message) in &failures {
          println!("  - {stem}: {message}");
        }
        bail!(
          "capture-missing-failure-fixtures failed for {} page(s)",
          failures.len()
        );
      }
    }
  }

  if args.capture_worst_accuracy_fixtures {
    if !disk_cache_enabled {
      eprintln!(
        "Warning: --capture-worst-accuracy-fixtures requested, but the pageset disk cache is disabled. \
         Skipping because `bundle_page cache` requires cached subresources."
      );
    } else {
      println!();
      println!("Capturing worst accuracy fixtures from the warmed disk cache...");

      let plan_args = xtask::capture_accuracy_fixtures::CaptureAccuracyFixturesArgs {
        progress_dir: repo_root.join("progress/pages"),
        fixtures_root: repo_root.join("tests/pages/fixtures"),
        bundle_out_dir: if args.capture_worst_accuracy_fixtures_out_dir.is_absolute() {
          args.capture_worst_accuracy_fixtures_out_dir.clone()
        } else {
          repo_root.join(&args.capture_worst_accuracy_fixtures_out_dir)
        },
        asset_cache_dir: cache_dir.clone(),
        user_agent: user_agent_arg.clone(),
        accept_language: accept_language_arg.clone(),
        viewport: viewport_arg.clone(),
        dpr: dpr_arg.clone(),
        allow_missing_resources: args.capture_worst_accuracy_fixtures_allow_missing_resources,
        overwrite: args.capture_worst_accuracy_fixtures_overwrite,
        min_diff_percent: args.capture_worst_accuracy_fixtures_min_diff_percent,
        top: args.capture_worst_accuracy_fixtures_top,
      };

      capture_accuracy_fixtures_with_plan(
        "capture-worst-accuracy-fixtures",
        &repo_root,
        plan_args,
      )?;
    }
  }

  Ok(())
}

fn run_capture_accuracy_fixtures(args: CaptureAccuracyFixturesArgs) -> Result<()> {
  if !(0.0..=100.0).contains(&args.min_diff_percent) || !args.min_diff_percent.is_finite() {
    bail!("--min-diff-percent must be a finite number between 0 and 100");
  }

  let repo_root = repo_root();

  let progress_dir = if args.progress_dir.is_absolute() {
    args.progress_dir
  } else {
    repo_root.join(args.progress_dir)
  };
  let fixtures_root = if args.fixtures_root.is_absolute() {
    args.fixtures_root
  } else {
    repo_root.join(args.fixtures_root)
  };
  let bundle_out_dir = if args.bundle_out_dir.is_absolute() {
    args.bundle_out_dir
  } else {
    repo_root.join(args.bundle_out_dir)
  };
  let asset_cache_dir = if args.asset_cache_dir.is_absolute() {
    args.asset_cache_dir
  } else {
    repo_root.join(args.asset_cache_dir)
  };

  if !asset_cache_dir.is_dir() {
    bail!(
      "asset cache directory {} does not exist; run pageset with disk cache enabled \
       (or pass --asset-cache-dir to point at an existing cache)",
      asset_cache_dir.display()
    );
  }

  let viewport = args.viewport.map(|(w, h)| format!("{w}x{h}"));
  let dpr = args.dpr.map(|dpr| dpr.to_string());

  let plan_args = xtask::capture_accuracy_fixtures::CaptureAccuracyFixturesArgs {
    progress_dir,
    fixtures_root,
    bundle_out_dir,
    asset_cache_dir,
    user_agent: args.user_agent,
    accept_language: args.accept_language,
    viewport,
    dpr,
    allow_missing_resources: args.allow_missing_resources,
    overwrite: args.overwrite,
    min_diff_percent: args.min_diff_percent,
    top: args.top,
  };

  capture_accuracy_fixtures_with_plan("capture-accuracy-fixtures", &repo_root, plan_args)
}

fn capture_accuracy_fixtures_with_plan(
  label: &str,
  repo_root: &Path,
  plan_args: xtask::capture_accuracy_fixtures::CaptureAccuracyFixturesArgs,
) -> Result<()> {
  if !(0.0..=100.0).contains(&plan_args.min_diff_percent) || !plan_args.min_diff_percent.is_finite()
  {
    bail!("min_diff_percent must be a finite number between 0 and 100");
  }

  let plan = xtask::capture_accuracy_fixtures::plan_capture_accuracy_fixtures(&plan_args)?;
  let xtask::capture_accuracy_fixtures::CaptureAccuracyFixturesPlan {
    ok_accuracy_pages_total,
    selected_pages_total,
    fixtures_already_present,
    captures,
  } = plan;

  println!("Ok pages with accuracy discovered: {ok_accuracy_pages_total}");
  println!("Selected pages: {selected_pages_total}");
  println!("Fixtures already present: {fixtures_already_present}");
  println!("Selected fixtures to capture: {}", captures.len());

  if !captures.is_empty() {
    fs::create_dir_all(&plan_args.bundle_out_dir).with_context(|| {
      format!(
        "failed to create bundle output directory {}",
        plan_args.bundle_out_dir.display()
      )
    })?;
  }

  let mut bundles_captured = 0usize;
  let mut fixtures_imported = 0usize;
  let mut failures: Vec<(String, String)> = Vec::new();

  for capture in captures {
    let fixture_summary = format!("diff={:.4}%", capture.diff_percent);

    if capture.bundle_path.exists() {
      if capture.bundle_path.is_dir() {
        fs::remove_dir_all(&capture.bundle_path).with_context(|| {
          format!(
            "failed to remove existing bundle {}",
            capture.bundle_path.display()
          )
        })?;
      } else {
        fs::remove_file(&capture.bundle_path).with_context(|| {
          format!(
            "failed to remove existing bundle {}",
            capture.bundle_path.display()
          )
        })?;
      }
    }

    println!("Capturing {} ({fixture_summary})...", capture.stem);

    let mut bundle_cmd = capture.bundle_command.to_command();
    bundle_cmd.current_dir(repo_root);
    let bundle_result =
      run_command(bundle_cmd).with_context(|| format!("bundle_page cache {}", capture.stem));
    if let Err(err) = bundle_result {
      failures.push((capture.stem.clone(), format!("capture failed: {err}")));
      continue;
    }
    bundles_captured += 1;

    let mut import_cmd = capture.import_command.to_command();
    import_cmd.current_dir(repo_root);
    let import_result =
      run_command(import_cmd).with_context(|| format!("import-page-fixture {}", capture.stem));
    if let Err(err) = import_result {
      failures.push((capture.stem.clone(), format!("import failed: {err}")));
      continue;
    }
    fixtures_imported += 1;
  }

  println!();
  println!("{label} summary:");
  println!("  ok pages with accuracy       {ok_accuracy_pages_total}");
  println!("  selected pages               {selected_pages_total}");
  println!("  fixtures already present     {fixtures_already_present}");
  println!("  bundles captured             {bundles_captured}");
  println!("  fixtures imported            {fixtures_imported}");
  println!("  failures                     {}", failures.len());

  if !failures.is_empty() {
    println!();
    println!("Failures:");
    for (stem, message) in &failures {
      println!("  - {stem}: {message}");
    }
    bail!("{label} failed for {} page(s)", failures.len());
  }

  Ok(())
}

fn query_prefetch_assets_support(disk_cache_feature: bool) -> Result<xtask::PrefetchAssetsSupport> {
  let mut cmd = Command::new("cargo");
  cmd
    .arg("run")
    .arg("--release")
    .apply_disk_cache_feature(disk_cache_feature)
    .args(["--bin", "prefetch_assets"])
    .arg("--")
    .arg("--capabilities")
    .current_dir(repo_root());

  let output = cmd
    .output()
    .with_context(|| format!("failed to run {:?}", cmd.get_program()))?;
  if !output.status.success() {
    bail!(
      "`prefetch_assets --capabilities` failed with status {}.\nstdout:\n{}\nstderr:\n{}",
      output.status,
      String::from_utf8_lossy(&output.stdout),
      String::from_utf8_lossy(&output.stderr)
    );
  }

  let stdout = String::from_utf8(output.stdout).context("decode prefetch_assets capabilities")?;
  let capabilities = xtask::parse_prefetch_assets_capabilities(&stdout).with_context(|| {
    format!(
      "parse JSON from `prefetch_assets --capabilities` (stdout={})",
      stdout.trim()
    )
  })?;
  if capabilities.name != "prefetch_assets" {
    bail!(
      "unexpected `prefetch_assets --capabilities` name field: expected \"prefetch_assets\", got {:?}",
      capabilities.name
    );
  }
  if disk_cache_feature && !capabilities.disk_cache_feature {
    bail!(
      "`prefetch_assets --capabilities` reported disk_cache_feature=false even though disk_cache was requested"
    );
  }

  Ok(capabilities.flags)
}

fn extract_disk_cache_args(extra: &[String]) -> Vec<String> {
  xtask::extract_disk_cache_args(extra)
}

fn run_pageset_diff(args: PagesetDiffArgs) -> Result<()> {
  if args.baseline.is_some() && args.baseline_ref.is_some() {
    bail!("provide at most one of --baseline and --baseline-ref");
  }

  let repo_root = repo_root();
  let cwd = std::env::current_dir().context("resolve current directory")?;

  if !args.no_run {
    run_pageset(args.pageset)?;
  }

  let progress_dir = repo_root.join("progress/pages");
  if !progress_dir.is_dir() {
    bail!(
      "progress directory {} is missing; run pageset first or point to the correct directory",
      progress_dir.display()
    );
  }

  let mut baseline_temp: Option<TempDir> = None;
  let baseline_dir = if let Some(dir) = args.baseline {
    let absolute = if dir.is_absolute() {
      dir
    } else {
      cwd.join(&dir)
    };
    if !absolute.is_dir() {
      bail!("baseline directory {} does not exist", absolute.display());
    }
    absolute
  } else {
    let baseline_ref = args.baseline_ref.unwrap_or_else(|| "HEAD".to_string());
    let (tempdir, dir) = extract_progress_from_ref(&baseline_ref)?;
    println!(
      "Using baseline progress from {baseline_ref} (extracted to {}).",
      dir.display()
    );
    baseline_temp = Some(tempdir);
    dir
  };

  println!(
    "Comparing {} against baseline {}...",
    progress_dir.display(),
    baseline_dir.display()
  );

  let fail_on_missing_stages =
    (args.fail_on_missing_stages || args.fail_on_regression) && !args.no_fail_on_missing_stages;
  let fail_on_missing_stage_timings = (args.fail_on_missing_stage_timings
    || args.fail_on_regression)
    && !args.no_fail_on_missing_stage_timings;
  let fail_on_slow_ok_ms = if args.no_fail_on_slow_ok {
    None
  } else if let Some(ms) = args.fail_on_slow_ok_ms {
    Some(ms)
  } else if args.fail_on_regression {
    Some(5000)
  } else {
    None
  };

  let mut cmd = Command::new("cargo");
  cmd
    .arg("run")
    .arg("--release")
    .args(["--bin", "pageset_progress"])
    .arg("--")
    .arg("report")
    .arg("--progress-dir")
    .arg(&progress_dir)
    .arg("--compare")
    .arg(&baseline_dir);
  if args.fail_on_regression {
    cmd.arg("--fail-on-regression");
  }
  if args.fail_on_accuracy_regression {
    cmd.arg("--fail-on-accuracy-regression");
  }
  if let Some(percent) = args.accuracy_regression_threshold_percent {
    cmd
      .arg("--accuracy-regression-threshold-percent")
      .arg(percent.to_string());
  }
  if fail_on_missing_stages {
    cmd.arg("--fail-on-missing-stages");
  }
  if fail_on_missing_stage_timings {
    cmd.arg("--fail-on-missing-stage-timings");
  }
  if let Some(ms) = fail_on_slow_ok_ms {
    cmd.arg("--fail-on-slow-ok-ms").arg(ms.to_string());
  }

  cmd.current_dir(&repo_root);
  print_command(&cmd);
  let status = cmd
    .status()
    .with_context(|| format!("failed to run {:?}", cmd.get_program()))?;
  if !status.success() {
    if status.code() == Some(2)
      && (fail_on_missing_stages
        || fail_on_missing_stage_timings
        || fail_on_slow_ok_ms.is_some()
        || args.fail_on_accuracy_regression
        || args.accuracy_regression_threshold_percent.is_some())
    {
      bail!(
        "pageset_progress report failed with status {status}. \
         If the error above mentions an unexpected argument, ensure pageset_progress includes \
         the report diagnostic gates or re-run with \
         --no-fail-on-missing-stages/--no-fail-on-missing-stage-timings/--no-fail-on-slow-ok \
         and/or remove the --fail-on-accuracy-regression flags."
      );
    }
    bail!("command failed with status {status}");
  }
  drop(baseline_temp);
  Ok(())
}

fn extract_progress_from_ref(reference: &str) -> Result<(TempDir, PathBuf)> {
  let list = Command::new("git")
    .args(["ls-tree", "-r", "--name-only", reference, "progress/pages"])
    .output()
    .with_context(|| format!("list progress/pages at {reference}"))?;
  if !list.status.success() {
    bail!(
      "git ls-tree failed for {reference}: {}",
      String::from_utf8_lossy(&list.stderr)
    );
  }

  let paths = String::from_utf8(list.stdout).context("decode git ls-tree output")?;
  let files: Vec<&str> = paths
    .lines()
    .map(str::trim)
    .filter(|line| !line.is_empty())
    .collect();
  if files.is_empty() {
    bail!("no progress/pages files found at {reference}");
  }

  let tempdir = tempfile::tempdir().context("create temp directory for baseline progress")?;
  let baseline_root = tempdir.path().join("progress/pages");

  for path in files {
    let relative = Path::new(path)
      .strip_prefix("progress/pages")
      .with_context(|| format!("unexpected path outside progress/pages: {path}"))?;
    let object = format!("{reference}:{path}");
    let contents = Command::new("git")
      .args(["show", &object])
      .output()
      .with_context(|| format!("read {object}"))?;
    if !contents.status.success() {
      bail!(
        "git show {object} failed: {}",
        String::from_utf8_lossy(&contents.stderr)
      );
    }

    let dest = baseline_root.join(relative);
    if let Some(parent) = dest.parent() {
      fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
    }
    fs::write(&dest, contents.stdout).with_context(|| format!("write {}", dest.display()))?;
  }

  Ok((tempdir, baseline_root))
}

fn disk_cache_enabled(cli_override: Option<bool>) -> bool {
  if let Some(enabled) = cli_override {
    return enabled;
  }

  if std::env::var("NO_DISK_CACHE")
    .map(|value| !value.is_empty())
    .unwrap_or(false)
  {
    return false;
  }

  std::env::var("DISK_CACHE")
    .map(|value| value != "0")
    .unwrap_or(true)
}

trait DiskCacheFeatureExt {
  fn apply_disk_cache_feature(&mut self, enabled: bool) -> &mut Command;
}

impl DiskCacheFeatureExt for Command {
  fn apply_disk_cache_feature(&mut self, enabled: bool) -> &mut Command {
    if enabled {
      self.args(["--features", "disk_cache"]);
    }
    self
  }
}
fn run_perf_smoke(args: PerfSmokeArgs) -> Result<()> {
  if let Some(threshold) = args.threshold {
    if threshold < 0.0 {
      bail!("threshold must be >= 0");
    }
  }
  if let Some(count_threshold) = args.count_threshold {
    if count_threshold < 0.0 {
      bail!("count-threshold must be >= 0");
    }
  }

  let fail_on_missing_fixtures = if args.allow_missing_fixtures {
    false
  } else {
    args.fail_on_missing_fixtures || matches!(args.suite, PerfSmokeSuite::PagesetGuardrails)
  };

  // Keep renders deterministic across machines.
  let mut cmd = Command::new("cargo");
  cmd.env("FASTR_USE_BUNDLED_FONTS", "1");
  cmd.arg("run");
  if !args.debug {
    cmd.arg("--release");
  }
  cmd.args(["--bin", "perf_smoke", "--"]);
  cmd.args(["--suite", args.suite.as_cli_value()]);
  if let Some(output) = &args.output {
    cmd.arg("--output").arg(output);
  }
  if let Some(threshold) = args.threshold {
    cmd.arg("--threshold").arg(threshold.to_string());
  }
  if let Some(count_threshold) = args.count_threshold {
    cmd
      .arg("--count-threshold")
      .arg(count_threshold.to_string());
  }
  if let Some(only) = &args.only {
    cmd.arg("--only").arg(only.join(","));
  }
  if let Some(top) = args.top {
    cmd.arg("--top").arg(top.to_string());
  }
  if let Some(baseline) = &args.baseline {
    cmd.arg("--baseline").arg(baseline);
  }
  if args.fail_on_regression {
    cmd.arg("--fail-on-regression");
  }
  if args.fail_fast {
    cmd.arg("--fail-fast");
  }
  if args.fail_on_failure {
    cmd.arg("--fail-on-failure");
  } else if args.no_fail_on_failure {
    cmd.arg("--no-fail-on-failure");
  }
  if fail_on_missing_fixtures {
    cmd.arg("--fail-on-missing-fixtures");
  }
  if args.fail_on_budget {
    cmd.arg("--fail-on-budget");
  }
  if args.fail_on_fetch_errors {
    cmd.arg("--fail-on-fetch-errors");
  }
  if args.isolate {
    cmd.arg("--isolate");
  }
  if args.no_isolate {
    cmd.arg("--no-isolate");
  }

  cmd.args(&args.extra);

  cmd.current_dir(repo_root());
  println!("Running perf_smoke...");
  run_command(cmd)
}

fn repo_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .expect("xtask manifest should be in repository root")
    .to_path_buf()
}

fn resolve_cargo_target_dir(repo_root: &Path, cargo_target_dir: Option<&Path>) -> PathBuf {
  match cargo_target_dir {
    Some(path) if path.as_os_str().is_empty() => repo_root.join("target"),
    Some(path) if path.is_absolute() => path.to_path_buf(),
    Some(path) => repo_root.join(path),
    None => repo_root.join("target"),
  }
}

fn cargo_target_dir(repo_root: &Path) -> PathBuf {
  let cargo_target_dir = std::env::var_os("CARGO_TARGET_DIR").map(PathBuf::from);
  resolve_cargo_target_dir(repo_root, cargo_target_dir.as_deref())
}

fn diff_renders_executable(repo_root: &Path) -> PathBuf {
  cargo_target_dir(repo_root)
    .join("release")
    .join(format!("diff_renders{}", std::env::consts::EXE_SUFFIX))
}

fn run_render_page(args: RenderPageArgs) -> Result<()> {
  // Historically `cargo xtask render-page` interpreted relative file/output paths relative to the
  // caller's current directory. Keep that behaviour even though we run the underlying `cargo run`
  // from the repo root so default cache paths (`fetches/assets`, etc) stay stable.
  let cwd = std::env::current_dir().context("resolve current directory")?;

  let url = match (args.url, args.file) {
    (Some(url), None) => url,
    (None, Some(path)) => {
      let absolute = if path.is_absolute() {
        path
      } else {
        cwd.join(path)
      };
      Url::from_file_path(&absolute)
        .map(|u| u.to_string())
        .map_err(|_| anyhow!("could not convert {} to a file:// URL", absolute.display()))?
    }
    _ => bail!("provide exactly one of --url or --file"),
  };

  // Like pageset, run the underlying render binary from the repo root so default cache/asset paths
  // (`fetches/assets`, `fetches/html`, etc.) are stable.
  let repo_root = repo_root();

  let mut cmd = Command::new("cargo");
  cmd.arg("run");
  if !args.debug {
    cmd.arg("--release");
  }
  cmd.args(["--bin", "fetch_and_render", "--"]);

  let viewport = format!("{}x{}", args.viewport.0, args.viewport.1);
  cmd.args(["--viewport", &viewport]);

  if args.dpr != 1.0 {
    cmd.args(["--dpr", &args.dpr.to_string()]);
  }

  if args.full_page {
    cmd.arg("--full-page");
  }

  if args.timeout > 0 {
    cmd.args(["--timeout", &args.timeout.to_string()]);
  }

  cmd.args(&args.extra);

  cmd.arg(&url);
  if let Some(output) = args.output {
    let output = if output.is_absolute() {
      output
    } else {
      cwd.join(output)
    };
    cmd.arg(output);
  }

  println!("Rendering page with fetch_and_render...");
  cmd.current_dir(&repo_root);
  run_command(cmd)
}

fn run_diff_renders(args: DiffRendersArgs) -> Result<()> {
  if !(0.0..=100.0).contains(&args.max_diff_percent) || !args.max_diff_percent.is_finite() {
    bail!("--max-diff-percent must be a finite number between 0 and 100");
  }
  if let Some(max) = args.max_perceptual_distance {
    if !(0.0..=1.0).contains(&max) || !max.is_finite() {
      bail!("--max-perceptual-distance must be a finite number between 0 and 1");
    }
  }

  // Historically `cargo xtask diff-renders` interpreted relative paths relative to the caller's
  // current directory (because it used `std::fs` directly). Keep that behaviour even though we run
  // `cargo` from the repo root.
  let cwd = std::env::current_dir().context("resolve current directory")?;
  let before = if args.before.is_absolute() {
    args.before
  } else {
    cwd.join(&args.before)
  };
  let after = if args.after.is_absolute() {
    args.after
  } else {
    cwd.join(&args.after)
  };
  let output_dir = if args.output.is_absolute() {
    args.output
  } else {
    cwd.join(&args.output)
  };

  fs::create_dir_all(&output_dir).context("create diff output directory")?;

  let html_path = output_dir.join("diff_report.html");
  let json_path = output_dir.join("diff_report.json");

  // Note: we build + execute the binary directly instead of using `cargo run`. The `diff_renders`
  // binary returns exit code 1 when diffs are found, and `cargo run` would print a scary
  // "process didn't exit successfully" error even though we still want to keep the report.
  let repo_root = repo_root();
  let exe = diff_renders_executable(&repo_root);
  if args.no_build {
    if !exe.is_file() {
      bail!(
        "--no-build was set, but diff_renders executable does not exist at {}",
        exe.display()
      );
    }
  } else {
    let mut build_cmd = Command::new("cargo");
    build_cmd
      .arg("build")
      .arg("--release")
      .args(["--bin", "diff_renders"])
      .current_dir(&repo_root);
    println!("Building diff_renders...");
    run_command(build_cmd)?;
  }

  let mut cmd = Command::new(&exe);
  cmd
    .arg("--before")
    .arg(&before)
    .arg("--after")
    .arg(&after)
    .arg("--tolerance")
    .arg(args.threshold.to_string())
    .arg("--max-diff-percent")
    .arg(args.max_diff_percent.to_string())
    .arg("--sort-by")
    .arg(args.sort_by.as_cli_value());
  if let Some(max) = args.max_perceptual_distance {
    cmd.arg("--max-perceptual-distance").arg(max.to_string());
  }
  if args.ignore_alpha {
    cmd.arg("--ignore-alpha");
  }
  if let Some((index, total)) = args.shard {
    cmd.arg("--shard").arg(format!("{index}/{total}"));
  }
  cmd
    .arg("--json")
    .arg(&json_path)
    .arg("--html")
    .arg(&html_path)
    .current_dir(&repo_root);

  println!("Running diff_renders...");
  print_command(&cmd);

  let status = cmd
    .status()
    .with_context(|| format!("failed to run {exe:?}"))?;

  if !json_path.exists() {
    bail!(
      "diff_renders failed (status {status}) and did not produce {}",
      json_path.display()
    );
  }

  let report: serde_json::Value =
    serde_json::from_str(&fs::read_to_string(&json_path).context("read diff_renders json report")?)
      .context("parse diff_renders json report")?;

  let totals = &report["totals"];
  let discovered = totals["discovered"].as_u64().unwrap_or(0);
  let processed = totals["processed"].as_u64().unwrap_or(0);
  let diffs = totals["differences"].as_u64().unwrap_or(0);
  let missing = totals["missing"].as_u64().unwrap_or(0);
  let errors = totals["errors"].as_u64().unwrap_or(0);

  println!(
    "Processed {processed} of {discovered} candidate(s); {diffs} diffs, {missing} missing, {errors} error(s)."
  );
  println!("HTML report: {}", html_path.display());
  println!("JSON report: {}", json_path.display());

  if args.fail_on_differences && (diffs + missing + errors) > 0 {
    bail!(
      "diff_renders reported differences; report: {}",
      html_path.display()
    );
  }

  Ok(())
}

fn parse_shard(s: &str) -> Result<(usize, usize), String> {
  let parts: Vec<&str> = s.split('/').collect();
  if parts.len() != 2 {
    return Err("shard must be index/total (e.g., 0/4)".to_string());
  }
  let index = parts[0]
    .parse::<usize>()
    .map_err(|_| "invalid shard index".to_string())?;
  let total = parts[1]
    .parse::<usize>()
    .map_err(|_| "invalid shard total".to_string())?;
  if total == 0 {
    return Err("shard total must be > 0".to_string());
  }
  if index >= total {
    return Err("shard index must be < total".to_string());
  }
  Ok((index, total))
}

fn run_command(mut cmd: Command) -> Result<()> {
  print_command(&cmd);

  let status = cmd
    .status()
    .with_context(|| format!("failed to run {:?}", cmd.get_program()))?;
  if !status.success() {
    bail!("command failed with status {status}");
  }
  Ok(())
}

fn print_command(cmd: &Command) {
  let envs = cmd
    .get_envs()
    .filter_map(|(k, v)| v.map(|v| format!("{}={}", k.to_string_lossy(), v.to_string_lossy())))
    .collect::<Vec<_>>();

  if !envs.is_empty() {
    print!("$ {} ", envs.join(" "));
  } else {
    print!("$ ");
  }

  print!("{}", cmd.get_program().to_string_lossy());
  for arg in cmd.get_args() {
    print!(" {}", arg.to_string_lossy());
  }
  println!();
}

fn parse_viewport(raw: &str) -> Result<(u32, u32)> {
  let (width, height) = raw
    .split_once('x')
    .ok_or_else(|| anyhow!("viewport must be formatted as WxH"))?;

  let width = width
    .parse::<u32>()
    .context("failed to parse viewport width")?;
  let height = height
    .parse::<u32>()
    .context("failed to parse viewport height")?;

  if width == 0 || height == 0 {
    bail!("viewport dimensions must be greater than zero");
  }

  Ok((width, height))
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn resolve_cargo_target_dir_uses_repo_target_by_default() {
    let repo_root = PathBuf::from("/repo");
    assert_eq!(
      resolve_cargo_target_dir(&repo_root, None),
      repo_root.join("target")
    );
    assert_eq!(
      resolve_cargo_target_dir(&repo_root, Some(Path::new(""))),
      repo_root.join("target")
    );
  }

  #[test]
  fn resolve_cargo_target_dir_resolves_relative_paths_from_repo_root() {
    let repo_root = PathBuf::from("/repo");
    assert_eq!(
      resolve_cargo_target_dir(&repo_root, Some(Path::new("custom_target"))),
      repo_root.join("custom_target")
    );
  }

  #[test]
  fn resolve_cargo_target_dir_preserves_absolute_paths() {
    let repo_root = PathBuf::from("/repo");
    assert_eq!(
      resolve_cargo_target_dir(&repo_root, Some(Path::new("/abs/target"))),
      PathBuf::from("/abs/target")
    );
  }
}
