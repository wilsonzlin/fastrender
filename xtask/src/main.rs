#![recursion_limit = "256"]

use anyhow::{anyhow, bail, Context, Result};
use clap::{Args, Parser, Subcommand, ValueEnum};
use image::{Rgba, RgbaImage};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;
use url::Url;
use walkdir::WalkDir;

mod import_page_fixture;
mod recapture_page_fixtures;
mod update_pageset_guardrails;
mod update_pageset_guardrails_budgets;

fn main() -> Result<()> {
  let cli = Cli::parse();

  match cli.command {
    Commands::Test(args) => run_tests(args),
    Commands::UpdateGoldens(args) => run_update_goldens(args),
    Commands::RenderPage(args) => run_render_page(args),
    Commands::Pageset(args) => run_pageset(args),
    Commands::PagesetDiff(args) => run_pageset_diff(args),
    Commands::DiffRenders(args) => run_diff_renders(args),
    Commands::ImportPageFixture(args) => import_page_fixture::run_import_page_fixture(args),
    Commands::RecapturePageFixtures(args) => {
      recapture_page_fixtures::run_recapture_page_fixtures(args)
    }
    Commands::UpdatePagesetGuardrails(args) => {
      update_pageset_guardrails::run_update_pageset_guardrails(args)
    }
    Commands::UpdatePagesetGuardrailsBudgets(args) => {
      update_pageset_guardrails_budgets::run_update_pageset_guardrails_budgets(args)
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
  /// Convert a captured bundle into a pages_regression fixture
  ImportPageFixture(import_page_fixture::ImportPageFixtureArgs),
  /// (Re)capture and (re)import offline page fixtures from a manifest
  RecapturePageFixtures(recapture_page_fixtures::RecapturePageFixturesArgs),
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

fn default_jobs() -> usize {
  std::thread::available_parallelism()
    .map(|n| n.get())
    .unwrap_or(1)
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

  /// Directory to write diff images into (mirrors input structure)
  #[arg(long, default_value = "target/render-diffs")]
  output: PathBuf,

  /// Per-channel tolerance (0 = exact match, 5-10 to ignore tiny AA differences)
  #[arg(long, default_value_t = 0)]
  threshold: u8,
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

struct DiffOutcome {
  changed: bool,
  dimensions_match: bool,
  total_pixels: u64,
  different_pixels: u64,
  different_percent: f64,
  max_channel_diff: u8,
  diff_path: Option<PathBuf>,
}

fn run_tests(args: TestArgs) -> Result<()> {
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
    run_command(cmd)?;
  }

  Ok(())
}

fn run_update_goldens(args: UpdateGoldensArgs) -> Result<()> {
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
    run_command(cmd)?;
  }

  Ok(())
}

fn run_pageset(args: PagesetArgs) -> Result<()> {
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

  let prefetch_support = PrefetchAssetsSupport::detect();
  let (mut prefetch_asset_args, mut pageset_extra_args) = if prefetch_support.any() {
    extract_prefetch_assets_args(&args.extra, prefetch_support)
  } else {
    (Vec::new(), args.extra.clone())
  };

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
  let total_cpus = std::thread::available_parallelism()
    .map(|n| n.get())
    .unwrap_or(1);
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
  cmd.args(&pageset_extra_args);
  if rayon_threads_env.is_none() {
    cmd.env("RAYON_NUM_THREADS", threads_per_worker.to_string());
  }
  if layout_parallel_env.is_none() {
    cmd.env("FASTR_LAYOUT_PARALLEL", "auto");
  }
  apply_disk_cache_env(&mut cmd);
  println!(
    "Updating progress/pages scoreboard (jobs={}, hard timeout={}s, disk_cache={}, cache_dir={})...",
    jobs,
    render_timeout,
    disk_cache_status,
    cache_dir.display()
  );
  run_command(cmd)?;

  Ok(())
}

#[derive(Copy, Clone)]
struct PrefetchAssetsSupport {
  prefetch_fonts: bool,
  prefetch_images: bool,
  prefetch_iframes: bool,
  prefetch_embeds: bool,
  prefetch_icons: bool,
  prefetch_video_posters: bool,
  prefetch_css_url_assets: bool,
  max_discovered_assets_per_page: bool,
  max_images_per_page: bool,
  max_image_urls_per_element: bool,
}

impl PrefetchAssetsSupport {
  fn any(self) -> bool {
    self.prefetch_fonts
      || self.prefetch_images
      || self.prefetch_iframes
      || self.prefetch_embeds
      || self.prefetch_icons
      || self.prefetch_video_posters
      || self.prefetch_css_url_assets
      || self.max_discovered_assets_per_page
      || self.max_images_per_page
      || self.max_image_urls_per_element
  }

  fn detect() -> Self {
    let path = repo_root().join("src/bin/prefetch_assets.rs");
    let Ok(contents) = fs::read_to_string(path) else {
      return Self {
        prefetch_fonts: false,
        prefetch_images: false,
        prefetch_iframes: false,
        prefetch_embeds: false,
        prefetch_icons: false,
        prefetch_video_posters: false,
        prefetch_css_url_assets: false,
        max_discovered_assets_per_page: false,
        max_images_per_page: false,
        max_image_urls_per_element: false,
      };
    };

    Self {
      prefetch_fonts: contents.contains("prefetch_fonts"),
      prefetch_images: contents.contains("prefetch_images"),
      prefetch_iframes: contents.contains("prefetch_iframes"),
      prefetch_embeds: contents.contains("prefetch_embeds"),
      prefetch_icons: contents.contains("prefetch_icons"),
      prefetch_video_posters: contents.contains("prefetch_video_posters"),
      prefetch_css_url_assets: contents.contains("prefetch_css_url_assets"),
      max_discovered_assets_per_page: contents.contains("max_discovered_assets_per_page"),
      max_images_per_page: contents.contains("max_images_per_page"),
      max_image_urls_per_element: contents.contains("max_image_urls_per_element"),
    }
  }
}

fn extract_prefetch_assets_args(
  extra: &[String],
  support: PrefetchAssetsSupport,
) -> (Vec<String>, Vec<String>) {
  let mut prefetch_args = Vec::new();
  let mut pageset_args = Vec::new();

  let mut iter = extra.iter().peekable();
  while let Some(arg) = iter.next() {
    let is_prefetch_arg = (support.prefetch_fonts
      && (arg == "--prefetch-fonts" || arg.starts_with("--prefetch-fonts=")))
      || (support.prefetch_images
        && (arg == "--prefetch-images" || arg.starts_with("--prefetch-images=")))
      || (support.prefetch_iframes
        && (arg == "--prefetch-iframes"
          || arg.starts_with("--prefetch-iframes=")
          || arg == "--prefetch-documents"
          || arg.starts_with("--prefetch-documents=")))
      || (support.prefetch_embeds
        && (arg == "--prefetch-embeds" || arg.starts_with("--prefetch-embeds=")))
      || (support.prefetch_icons
        && (arg == "--prefetch-icons" || arg.starts_with("--prefetch-icons=")))
      || (support.prefetch_video_posters
        && (arg == "--prefetch-video-posters" || arg.starts_with("--prefetch-video-posters=")))
      || (support.prefetch_css_url_assets
        && (arg == "--prefetch-css-url-assets" || arg.starts_with("--prefetch-css-url-assets=")));
    let is_prefetch_arg = is_prefetch_arg
      || (support.max_discovered_assets_per_page
        && (arg == "--max-discovered-assets-per-page"
          || arg.starts_with("--max-discovered-assets-per-page=")));
    let is_prefetch_arg = is_prefetch_arg
      || (support.max_images_per_page
        && (arg == "--max-images-per-page" || arg.starts_with("--max-images-per-page=")));
    let is_prefetch_arg = is_prefetch_arg
      || (support.max_image_urls_per_element
        && (arg == "--max-image-urls-per-element"
          || arg.starts_with("--max-image-urls-per-element=")));

    if is_prefetch_arg {
      prefetch_args.push(arg.clone());

      if !arg.contains('=') {
        if let Some(next) = iter.peek() {
          if !next.starts_with('-') {
            prefetch_args.push((*next).clone());
            iter.next();
          }
        }
      }
    } else {
      pageset_args.push(arg.clone());
    }
  }

  (prefetch_args, pageset_args)
}

fn extract_disk_cache_args(extra: &[String]) -> Vec<String> {
  xtask::extract_disk_cache_args(extra)
}

fn run_pageset_diff(args: PagesetDiffArgs) -> Result<()> {
  if args.baseline.is_some() && args.baseline_ref.is_some() {
    bail!("provide at most one of --baseline and --baseline-ref");
  }

  if !args.no_run {
    run_pageset(args.pageset)?;
  }

  let progress_dir = PathBuf::from("progress/pages");
  if !progress_dir.is_dir() {
    bail!(
      "progress directory {} is missing; run pageset first or point to the correct directory",
      progress_dir.display()
    );
  }

  let mut baseline_temp: Option<TempDir> = None;
  let baseline_dir = if let Some(dir) = args.baseline {
    if !dir.is_dir() {
      bail!("baseline directory {} does not exist", dir.display());
    }
    dir
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
  if fail_on_missing_stages {
    cmd.arg("--fail-on-missing-stages");
  }
  if fail_on_missing_stage_timings {
    cmd.arg("--fail-on-missing-stage-timings");
  }
  if let Some(ms) = fail_on_slow_ok_ms {
    cmd.arg("--fail-on-slow-ok-ms").arg(ms.to_string());
  }

  print_command(&cmd);
  let status = cmd
    .status()
    .with_context(|| format!("failed to run {:?}", cmd.get_program()))?;
  if !status.success() {
    if status.code() == Some(2)
      && (fail_on_missing_stages || fail_on_missing_stage_timings || fail_on_slow_ok_ms.is_some())
    {
      bail!(
        "pageset_progress report failed with status {status}. \
         If the error above mentions an unexpected argument, ensure pageset_progress includes \
         the report diagnostic gates or re-run with \
         --no-fail-on-missing-stages/--no-fail-on-missing-stage-timings/--no-fail-on-slow-ok."
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

fn run_render_page(args: RenderPageArgs) -> Result<()> {
  let url = match (args.url, args.file) {
    (Some(url), None) => url,
    (None, Some(path)) => file_to_url(&path)?,
    _ => bail!("provide exactly one of --url or --file"),
  };

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
  if let Some(output) = &args.output {
    cmd.arg(output);
  }

  println!("Rendering page with fetch_and_render...");
  run_command(cmd)
}

fn run_diff_renders(args: DiffRendersArgs) -> Result<()> {
  if args.before.is_file() != args.after.is_file() {
    bail!("--before and --after must both be files or both be directories");
  }

  fs::create_dir_all(&args.output).context("create diff output directory")?;

  if args.before.is_file() {
    let diff_path = args.output.join("diff.png");
    let outcome = diff_pair(&args.before, &args.after, &diff_path, args.threshold)?;
    print_diff_summary(Path::new("diff.png"), &outcome);
    return Ok(());
  }

  let before_dir = &args.before;
  let after_dir = &args.after;

  let mut pairs = Vec::new();
  let mut missing_in_before = Vec::new();

  for entry in WalkDir::new(after_dir) {
    let entry = entry.context("walk after directory")?;
    if !entry.file_type().is_file() {
      continue;
    }
    if entry
      .path()
      .extension()
      .and_then(|e| e.to_str())
      .map(|ext| ext.eq_ignore_ascii_case("png"))
      != Some(true)
    {
      continue;
    }

    let rel = entry
      .path()
      .strip_prefix(after_dir)
      .context("strip after prefix")?
      .to_path_buf();
    let before_path = before_dir.join(&rel);
    if before_path.exists() {
      pairs.push((rel, before_path, entry.path().to_path_buf()));
    } else {
      missing_in_before.push(rel);
    }
  }

  let mut missing_in_after = Vec::new();
  for entry in WalkDir::new(before_dir) {
    let entry = entry.context("walk before directory")?;
    if !entry.file_type().is_file() {
      continue;
    }
    if entry
      .path()
      .extension()
      .and_then(|e| e.to_str())
      .map(|ext| ext.eq_ignore_ascii_case("png"))
      != Some(true)
    {
      continue;
    }

    let rel = entry
      .path()
      .strip_prefix(before_dir)
      .context("strip before prefix")?
      .to_path_buf();
    let after_path = after_dir.join(&rel);
    if !after_path.exists() {
      missing_in_after.push(rel);
    }
  }

  if pairs.is_empty() {
    println!("No PNG files found under {}", after_dir.display());
    return Ok(());
  }

  pairs.sort_by(|a, b| a.0.cmp(&b.0));
  missing_in_before.sort();
  missing_in_after.sort();

  let mut changed = 0usize;
  let mut compared = 0usize;

  for (rel, before, after) in pairs {
    compared += 1;
    let diff_path = args.output.join(&rel).with_extension("diff.png");
    let outcome = diff_pair(&before, &after, &diff_path, args.threshold)?;
    if outcome.changed {
      changed += 1;
    }
    print_diff_summary(&rel, &outcome);
  }

  println!(
    "Compared {compared} render(s); {changed} differed (threshold {})",
    args.threshold
  );

  if !missing_in_before.is_empty() {
    println!(
      "Files only in --after (new renders): {}",
      format_path_list(&missing_in_before)
    );
  }
  if !missing_in_after.is_empty() {
    println!(
      "Files only in --before (removed renders): {}",
      format_path_list(&missing_in_after)
    );
  }

  Ok(())
}

fn format_path_list(paths: &[PathBuf]) -> String {
  const MAX_ENTRIES: usize = 8;
  if paths.len() <= MAX_ENTRIES {
    return paths
      .iter()
      .map(|p| p.display().to_string())
      .collect::<Vec<_>>()
      .join(", ");
  }

  let shown = paths
    .iter()
    .take(MAX_ENTRIES)
    .map(|p| p.display().to_string())
    .collect::<Vec<_>>()
    .join(", ");
  format!("{shown}, … ({} more)", paths.len() - MAX_ENTRIES)
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

fn file_to_url(path: &Path) -> Result<String> {
  let absolute = if path.is_absolute() {
    path.to_path_buf()
  } else {
    std::env::current_dir()
      .context("resolve current directory")?
      .join(path)
  };

  Url::from_file_path(&absolute)
    .map(|u| u.to_string())
    .map_err(|_| anyhow!("could not convert {} to a file:// URL", absolute.display()))
}

fn diff_pair(before: &Path, after: &Path, diff_path: &Path, threshold: u8) -> Result<DiffOutcome> {
  let before_img = load_png(before)?;
  let after_img = load_png(after)?;

  let max_width = before_img.width().max(after_img.width());
  let max_height = before_img.height().max(after_img.height());

  let mut diff_image = RgbaImage::new(max_width, max_height);
  let mut different_pixels = 0u64;
  let mut max_channel_diff = 0u8;

  for y in 0..max_height {
    for x in 0..max_width {
      let before_px = if x < before_img.width() && y < before_img.height() {
        Some(*before_img.get_pixel(x, y))
      } else {
        None
      };
      let after_px = if x < after_img.width() && y < after_img.height() {
        Some(*after_img.get_pixel(x, y))
      } else {
        None
      };

      let (diff_pixel, max_diff) = match (before_px, after_px) {
        (Some(before_px), Some(after_px)) => {
          let diff_r = before_px.0[0].abs_diff(after_px.0[0]);
          let diff_g = before_px.0[1].abs_diff(after_px.0[1]);
          let diff_b = before_px.0[2].abs_diff(after_px.0[2]);
          let diff_a = before_px.0[3].abs_diff(after_px.0[3]);
          let max_diff = *[diff_r, diff_g, diff_b, diff_a].iter().max().unwrap();
          (Rgba([diff_r, diff_g, diff_b, max_diff.max(10)]), max_diff)
        }
        (Some(_), None) | (None, Some(_)) => (Rgba([255, 0, 255, 255]), 255),
        (None, None) => unreachable!("loop bounds ensure at least one pixel"),
      };

      if max_diff > threshold {
        different_pixels += 1;
      }

      max_channel_diff = max_channel_diff.max(max_diff);
      diff_image.put_pixel(x, y, diff_pixel);
    }
  }

  let total_pixels = (max_width as u64) * (max_height as u64);
  let different_percent = if total_pixels == 0 {
    0.0
  } else {
    (different_pixels as f64 / total_pixels as f64) * 100.0
  };

  let dimensions_match =
    before_img.width() == after_img.width() && before_img.height() == after_img.height();

  let diff_path_written = if different_pixels > 0 || !dimensions_match {
    if let Some(parent) = diff_path.parent() {
      fs::create_dir_all(parent).with_context(|| {
        format!(
          "failed to create diff output directory {}",
          parent.display()
        )
      })?;
    }
    diff_image
      .save(diff_path)
      .with_context(|| format!("failed to write diff image to {}", diff_path.display()))?;
    Some(diff_path.to_path_buf())
  } else {
    None
  };

  Ok(DiffOutcome {
    changed: different_pixels > 0 || !dimensions_match,
    dimensions_match,
    total_pixels,
    different_pixels,
    different_percent,
    max_channel_diff,
    diff_path: diff_path_written,
  })
}

fn load_png(path: &Path) -> Result<RgbaImage> {
  Ok(
    image::open(path)
      .with_context(|| format!("failed to read image at {}", path.display()))?
      .to_rgba8(),
  )
}

fn print_diff_summary(relative_path: &Path, outcome: &DiffOutcome) {
  let status = if outcome.changed { "DIFF" } else { "OK" };
  let percent = format!("{:.4}", outcome.different_percent);
  let mut parts = vec![format!(
    "{status} {} – {} differing pixels of {} ({percent}%), max channel diff {}",
    relative_path.display(),
    outcome.different_pixels,
    outcome.total_pixels,
    outcome.max_channel_diff
  )];

  if !outcome.dimensions_match {
    parts.push("dimensions differ".to_string());
  }
  if let Some(path) = &outcome.diff_path {
    parts.push(format!("diff: {}", path.display()));
  }

  println!("{}", parts.join(" | "));
}
