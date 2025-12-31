use anyhow::{anyhow, bail, Context, Result};
use clap::{ArgAction, Args, Parser, Subcommand, ValueEnum};
use fastrender::style::media::MediaType;
use fastrender::{
  DiagnosticsLevel, FastRender, FastRenderConfig, FontConfig, RenderOptions, RenderStageTimings,
  RenderStats, ResourceKind, ResourcePolicy,
};
use image::{Rgba, RgbaImage};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;
use tempfile::TempDir;
use url::Url;
use walkdir::WalkDir;

mod import_page_fixture;
mod update_pageset_timeouts;

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
    Commands::UpdatePagesetTimeouts(args) => {
      update_pageset_timeouts::run_update_pageset_timeouts(args)
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
  /// Fetch pages and update the committed pageset scoreboard (`progress/pages/*.json`)
  Pageset(PagesetArgs),
  /// Refresh the pageset scoreboard and compare against a baseline
  PagesetDiff(PagesetDiffArgs),
  /// Compare two render outputs and write visual diffs
  DiffRenders(DiffRendersArgs),
  /// Convert a captured bundle into a pages_regression fixture
  ImportPageFixture(import_page_fixture::ImportPageFixtureArgs),
  /// Update `tests/pages/pageset_timeouts.json` based on `progress/pages/*.json`
  UpdatePagesetTimeouts(update_pageset_timeouts::UpdatePagesetTimeoutsArgs),
  /// Run the offline perf smoke harness over a curated fixture set
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

  /// Hard per-page render timeout (seconds)
  #[arg(long, default_value_t = 5)]
  render_timeout: u64,

  /// Fetch/render only listed pages (comma-separated URLs or cache stems)
  #[arg(long, value_delimiter = ',')]
  pages: Option<Vec<String>>,

  /// Process only a deterministic shard of the page set (index/total, 0-based)
  #[arg(long, value_parser = parse_shard)]
  shard: Option<(usize, usize)>,

  /// Skip fetching and only update progress from the existing cache
  #[arg(long)]
  no_fetch: bool,

  /// Disable the disk-backed cache (defaults on; see NO_DISK_CACHE/DISK_CACHE)
  #[arg(long = "no-disk-cache", default_value_t = true, action = ArgAction::SetFalse)]
  disk_cache: bool,

  /// Extra arguments forwarded to `pageset_progress run` (use `--` before these)
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
  /// Print the slowest N fixtures
  #[arg(long)]
  top: Option<usize>,

  /// Baseline JSON path for regression detection
  #[arg(long)]
  baseline: Option<PathBuf>,

  /// Relative regression threshold (e.g. 0.05 = 5%)
  #[arg(long, default_value_t = 0.05)]
  threshold: f64,

  /// Where to write the perf smoke JSON report
  #[arg(long, default_value = "target/perf-smoke.json")]
  output: PathBuf,

  /// Fail when regressions are detected (enabled automatically when --baseline is provided)
  #[arg(long)]
  fail_on_regression: bool,

  /// Run the perf smoke harness in debug mode instead of release
  #[arg(long)]
  debug: bool,
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
  if args.jobs == 0 {
    bail!("jobs must be > 0");
  }

  let pages_arg = args.pages.as_ref().map(|pages| pages.join(","));
  let shard_arg = args.shard.map(|(index, total)| format!("{index}/{total}"));

  let disk_cache_enabled = disk_cache_enabled(args.disk_cache);
  let disk_cache_status = if disk_cache_enabled {
    "enabled"
  } else {
    "disabled"
  };
  if disk_cache_enabled {
    println!(
      "Disk cache enabled (persisting subresources under fetches/assets/). \
       Set NO_DISK_CACHE=1, DISK_CACHE=0, or --no-disk-cache to disable."
    );
  } else {
    println!(
      "Disk cache disabled (NO_DISK_CACHE/DISK_CACHE/--no-disk-cache); subresources will be refetched each run."
    );
  }

  if !args.no_fetch {
    let mut cmd = Command::new("cargo");
    cmd
      .arg("run")
      .arg("--release")
      .apply_disk_cache_feature(disk_cache_enabled)
      .args(["--bin", "fetch_pages"])
      .arg("--")
      .arg("--jobs")
      .arg(args.jobs.to_string())
      .arg("--timeout")
      .arg(args.fetch_timeout.to_string());
    if let Some(pages) = &pages_arg {
      cmd.arg("--pages").arg(pages);
    }
    if let Some(shard) = &shard_arg {
      cmd.arg("--shard").arg(shard);
    }
    println!(
      "Updating cached pages (jobs={}, timeout={}s, disk_cache={})...",
      args.jobs, args.fetch_timeout, disk_cache_status
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
      .arg(args.jobs.to_string())
      .arg("--timeout")
      .arg(args.fetch_timeout.to_string());
    if let Some(pages) = &pages_arg {
      cmd.arg("--pages").arg(pages);
    }
    if let Some(shard) = &shard_arg {
      cmd.arg("--shard").arg(shard);
    }
    println!(
      "Prefetching subresources into fetches/assets/ (jobs={}, timeout={}s)...",
      args.jobs, args.fetch_timeout
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
    .arg(args.jobs.to_string())
    .arg("--timeout")
    .arg(args.render_timeout.to_string())
    .arg("--bundled-fonts");
  if let Some(pages) = &pages_arg {
    cmd.arg("--pages").arg(pages);
  }
  if let Some(shard) = &shard_arg {
    cmd.arg("--shard").arg(shard);
  }
  cmd.args(&args.extra);
  println!(
    "Updating progress/pages scoreboard (jobs={}, hard timeout={}s, disk_cache={})...",
    args.jobs, args.render_timeout, disk_cache_status
  );
  run_command(cmd)?;

  Ok(())
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

  run_command(cmd)?;
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

fn disk_cache_enabled(cli_disk_cache_enabled: bool) -> bool {
  if !cli_disk_cache_enabled {
    return false;
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

const PERF_SMOKE_REPORT_VERSION: u32 = 1;

#[derive(Clone, Copy)]
struct PerfFixture {
  name: &'static str,
  html: &'static str,
  shots: &'static [PerfShot],
}

#[derive(Clone, Copy)]
struct PerfShot {
  label: &'static str,
  viewport: (u32, u32),
  dpr: f32,
  media: MediaType,
}

const DEFAULT_SHOT: PerfShot = PerfShot {
  label: "default",
  viewport: (1040, 1240),
  dpr: 1.0,
  media: MediaType::Screen,
};

const PRINT_SHOT: PerfShot = PerfShot {
  label: "print",
  viewport: (920, 1180),
  dpr: 1.0,
  media: MediaType::Print,
};

const DEFAULT_SHOTS: &[PerfShot] = &[DEFAULT_SHOT];
const PRINT_SHOTS: &[PerfShot] = &[PRINT_SHOT];

const PERF_FIXTURES: &[PerfFixture] = &[
  PerfFixture {
    name: "flex_dashboard",
    html: "flex_dashboard/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "grid_news",
    html: "grid_news/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "table_financial",
    html: "table_financial/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "multicol_article",
    html: "multicol_article/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "paginated_report",
    html: "paginated_report/index.html",
    shots: PRINT_SHOTS,
  },
  PerfFixture {
    name: "fragmentation_showcase",
    html: "fragmentation_showcase/index.html",
    shots: PRINT_SHOTS,
  },
  PerfFixture {
    name: "mask_filter_showcase",
    html: "mask_filter_showcase/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "svg_embed",
    html: "svg_embed/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "writing_modes",
    html: "writing_modes/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "subgrid_showcase",
    html: "subgrid_showcase/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "subgrid_alignment",
    html: "subgrid_alignment/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "subgrid_writing_mode_gap",
    html: "subgrid_writing_mode_gap/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "subgrid_vertical_inheritance",
    html: "subgrid_vertical_inheritance/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "subgrid_vertical_stack",
    html: "subgrid_vertical_stack/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "subgrid_nested_axes",
    html: "subgrid_nested_axes/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "form_controls",
    html: "form_controls/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "positioned_badge_regression",
    html: "positioned_badge_regression/index.html",
    shots: DEFAULT_SHOTS,
  },
  PerfFixture {
    name: "running_elements",
    html: "running_elements/index.html",
    shots: PRINT_SHOTS,
  },
];

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PerfSmokeReport {
  version: u32,
  totals: PerfSmokeTotals,
  fixtures: Vec<FixtureReport>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PerfSmokeTotals {
  fixtures: usize,
  wall_time_ms: f64,
  render_wall_time_ms: f64,
  stage_time_ms: RenderStageTimings,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct FixtureReport {
  id: String,
  fixture: String,
  shot: String,
  html: String,
  viewport: (u32, u32),
  dpr: f32,
  media: String,
  wall_time_ms: f64,
  stage_total_ms: Option<f64>,
  stats: SerializableRenderStats,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SerializableRenderStats {
  timings: RenderStageTimings,
  counts: fastrender::RenderCounts,
  cascade: fastrender::CascadeDiagnostics,
  layout: fastrender::LayoutDiagnostics,
  paint: fastrender::PaintDiagnostics,
  resources: SerializableResourceDiagnostics,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SerializableResourceDiagnostics {
  fetch_counts: BTreeMap<String, usize>,
  image_cache_hits: Option<usize>,
  image_cache_misses: Option<usize>,
}

fn run_perf_smoke(args: PerfSmokeArgs) -> Result<()> {
  if args.threshold < 0.0 {
    bail!("threshold must be >= 0");
  }
  let fail_on_regression = args.fail_on_regression || args.baseline.is_some();

  // Keep renders deterministic across machines.
  std::env::set_var("FASTR_USE_BUNDLED_FONTS", "1");

  let fixtures_dir = perf_fixture_dir();
  let output_path = if args.output.is_absolute() {
    args.output.clone()
  } else {
    repo_root().join(&args.output)
  };
  let baseline_path = args.baseline.as_ref().map(|path| {
    if path.is_absolute() {
      path.clone()
    } else {
      repo_root().join(path)
    }
  });
  let run_start = Instant::now();
  let mut fixtures = Vec::new();
  let mut render_wall_time_ms = 0.0f64;
  let mut stage_totals = RenderStageTimings::default();

  for fixture in PERF_FIXTURES {
    let html_path = fixtures_dir.join(fixture.html);
    let html = fs::read_to_string(&html_path)
      .with_context(|| format!("read fixture HTML at {}", html_path.display()))?;
    let base_url = fixture_base_url(&html_path)?;
    let mut renderer = build_perf_renderer(&base_url)?;

    for shot in fixture.shots {
      let id = fixture_id(fixture, shot);
      println!("Rendering perf fixture {id}...");

      let options = RenderOptions::new()
        .with_viewport(shot.viewport.0, shot.viewport.1)
        .with_device_pixel_ratio(shot.dpr)
        .with_media_type(shot.media)
        .with_diagnostics_level(DiagnosticsLevel::Basic);

      let render_start = Instant::now();
      let render = renderer
        .render_html_with_diagnostics(&html, options)
        .with_context(|| format!("render fixture {id}"))?;
      let wall_time_ms = render_start.elapsed().as_secs_f64() * 1000.0;
      render_wall_time_ms += wall_time_ms;

      let stats = render
        .diagnostics
        .stats
        .ok_or_else(|| anyhow!("RenderStats missing for {id}"))?;
      let stage_total_ms = sum_timings(&stats.timings);
      accumulate_timings(&mut stage_totals, &stats.timings);

      fixtures.push(FixtureReport::from_components(
        fixture,
        shot,
        wall_time_ms,
        stage_total_ms,
        stats,
      ));
    }
  }

  let report = PerfSmokeReport {
    version: PERF_SMOKE_REPORT_VERSION,
    totals: PerfSmokeTotals {
      fixtures: fixtures.len(),
      wall_time_ms: run_start.elapsed().as_secs_f64() * 1000.0,
      render_wall_time_ms,
      stage_time_ms: stage_totals,
    },
    fixtures,
  };

  write_perf_smoke_report(&output_path, &report)?;
  println!("Wrote perf smoke report to {}", output_path.display());

  if let Some(top) = args.top {
    print_top_fixtures(&report, top);
  }

  if let Some(baseline) = &baseline_path {
    let baseline_report = load_perf_smoke_report(baseline)?;
    let regressions = compare_baseline(&report, &baseline_report, args.threshold, baseline)?;
    if regressions.is_empty() {
      println!(
        "No perf regressions detected vs {} (threshold {:.2}%)",
        baseline.display(),
        args.threshold * 100.0
      );
    } else {
      eprintln!(
        "Perf regressions vs {} (threshold {:.2}%):",
        baseline.display(),
        args.threshold * 100.0
      );
      for entry in &regressions {
        eprintln!("- {entry}");
      }
      if fail_on_regression {
        bail!("perf-smoke regressions detected");
      }
    }
  }

  Ok(())
}

fn build_perf_renderer(base_url: &str) -> Result<FastRender> {
  let policy = ResourcePolicy::default()
    .allow_http(false)
    .allow_https(false)
    .allow_file(true)
    .allow_data(true);

  let config = FastRenderConfig::new()
    .with_base_url(base_url.to_string())
    .with_resource_policy(policy)
    .with_font_sources(FontConfig::bundled_only());
  Ok(FastRender::with_config(config)?)
}

fn perf_fixture_dir() -> PathBuf {
  repo_root().join("tests/pages/fixtures")
}

fn repo_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .expect("xtask manifest should be in repository root")
    .to_path_buf()
}

fn fixture_base_url(html_path: &Path) -> Result<String> {
  let Some(parent) = html_path.parent() else {
    bail!("Fixture {} has no parent directory", html_path.display());
  };
  Url::from_directory_path(parent)
    .map(|u| u.to_string())
    .map_err(|_| anyhow!("Failed to build base URL for {}", html_path.display()))
}

fn fixture_id(fixture: &PerfFixture, shot: &PerfShot) -> String {
  if shot.label == "default" {
    fixture.name.to_string()
  } else {
    format!("{}_{}", fixture.name, shot.label)
  }
}

fn media_label(media: MediaType) -> &'static str {
  match media {
    MediaType::All => "all",
    MediaType::Screen => "screen",
    MediaType::Print => "print",
    MediaType::Speech => "speech",
  }
}

fn accumulate_timings(target: &mut RenderStageTimings, timings: &RenderStageTimings) {
  add_timing(&mut target.html_decode_ms, timings.html_decode_ms);
  add_timing(&mut target.dom_parse_ms, timings.dom_parse_ms);
  add_timing(&mut target.css_inlining_ms, timings.css_inlining_ms);
  add_timing(&mut target.css_parse_ms, timings.css_parse_ms);
  add_timing(&mut target.cascade_ms, timings.cascade_ms);
  add_timing(&mut target.box_tree_ms, timings.box_tree_ms);
  add_timing(&mut target.layout_ms, timings.layout_ms);
  add_timing(&mut target.paint_build_ms, timings.paint_build_ms);
  add_timing(&mut target.paint_optimize_ms, timings.paint_optimize_ms);
  add_timing(&mut target.paint_rasterize_ms, timings.paint_rasterize_ms);
  add_timing(&mut target.encode_ms, timings.encode_ms);
}

fn add_timing(target: &mut Option<f64>, value: Option<f64>) {
  if let Some(value) = value {
    *target = Some(target.unwrap_or(0.0) + value);
  }
}

fn sum_timings(timings: &RenderStageTimings) -> Option<f64> {
  let mut total = 0.0;
  let mut has_value = false;

  for value in [
    timings.html_decode_ms,
    timings.dom_parse_ms,
    timings.css_inlining_ms,
    timings.css_parse_ms,
    timings.cascade_ms,
    timings.box_tree_ms,
    timings.layout_ms,
    timings.paint_build_ms,
    timings.paint_optimize_ms,
    timings.paint_rasterize_ms,
    timings.encode_ms,
  ] {
    if let Some(value) = value {
      total += value;
      has_value = true;
    }
  }

  has_value.then_some(total)
}

fn write_perf_smoke_report(path: &Path, report: &PerfSmokeReport) -> Result<()> {
  if let Some(parent) = path.parent() {
    fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
  }
  let contents =
    serde_json::to_string_pretty(report).context("serialize perf smoke report to JSON")?;
  fs::write(path, contents).with_context(|| format!("write report to {}", path.display()))
}

fn load_perf_smoke_report(path: &Path) -> Result<PerfSmokeReport> {
  let data = fs::read_to_string(path)
    .with_context(|| format!("read baseline report at {}", path.display()))?;
  serde_json::from_str(&data)
    .with_context(|| format!("parse baseline report from {}", path.display()))
}

fn compare_baseline(
  current: &PerfSmokeReport,
  baseline: &PerfSmokeReport,
  threshold: f64,
  baseline_path: &Path,
) -> Result<Vec<String>> {
  if baseline.version != current.version {
    bail!(
      "Baseline version {} does not match current version {}",
      baseline.version,
      current.version
    );
  }

  let mut baseline_fixtures = HashMap::new();
  for fixture in &baseline.fixtures {
    baseline_fixtures.insert(fixture.id.as_str(), fixture);
  }

  let mut regressions = Vec::new();
  for fixture in &current.fixtures {
    let Some(base) = baseline_fixtures.get(fixture.id.as_str()) else {
      bail!(
        "Baseline {} is missing fixture {} (fixtures changed?)",
        baseline_path.display(),
        fixture.id
      );
    };

    if let Some(delta) = percent_change(
      fixture.stage_total_ms.unwrap_or(fixture.wall_time_ms),
      base.stage_total_ms.unwrap_or(base.wall_time_ms),
    ) {
      if delta > threshold {
        regressions.push(format!(
          "{}: +{:.2}% ({:.2}ms -> {:.2}ms)",
          fixture.id,
          delta * 100.0,
          base.stage_total_ms.unwrap_or(base.wall_time_ms),
          fixture.stage_total_ms.unwrap_or(fixture.wall_time_ms),
        ));
      }
    }
  }

  if let Some(delta) = percent_change(
    current.totals.render_wall_time_ms,
    baseline.totals.render_wall_time_ms,
  ) {
    if delta > threshold {
      regressions.push(format!(
        "overall render wall time: +{:.2}% ({:.2}ms -> {:.2}ms)",
        delta * 100.0,
        baseline.totals.render_wall_time_ms,
        current.totals.render_wall_time_ms
      ));
    }
  }

  Ok(regressions)
}

fn percent_change(current: f64, baseline: f64) -> Option<f64> {
  if baseline <= 0.0 {
    return (current > 0.0).then_some(f64::INFINITY);
  }
  Some((current - baseline) / baseline)
}

fn print_top_fixtures(report: &PerfSmokeReport, count: usize) {
  if count == 0 {
    return;
  }
  let mut fixtures = report.fixtures.clone();
  fixtures.sort_by(|a, b| {
    fixture_total_ms(b)
      .total_cmp(&fixture_total_ms(a))
      .then_with(|| a.id.cmp(&b.id))
  });
  println!("Slowest {} perf smoke render(s):", count);
  for fixture in fixtures.into_iter().take(count) {
    println!(
      "- {}: {:.2}ms (wall {:.2}ms)",
      fixture.id,
      fixture.stage_total_ms.unwrap_or(fixture.wall_time_ms),
      fixture.wall_time_ms
    );
  }
}

fn fixture_total_ms(fixture: &FixtureReport) -> f64 {
  fixture.stage_total_ms.unwrap_or(fixture.wall_time_ms)
}

impl FixtureReport {
  fn from_components(
    fixture: &PerfFixture,
    shot: &PerfShot,
    wall_time_ms: f64,
    stage_total_ms: Option<f64>,
    stats: RenderStats,
  ) -> Self {
    Self {
      id: fixture_id(fixture, shot),
      fixture: fixture.name.to_string(),
      shot: shot.label.to_string(),
      html: fixture.html.to_string(),
      viewport: shot.viewport,
      dpr: shot.dpr,
      media: media_label(shot.media).to_string(),
      wall_time_ms,
      stage_total_ms,
      stats: stats.into(),
    }
  }
}

impl From<RenderStats> for SerializableRenderStats {
  fn from(stats: RenderStats) -> Self {
    Self {
      timings: stats.timings,
      counts: stats.counts,
      cascade: stats.cascade,
      layout: stats.layout,
      paint: stats.paint,
      resources: stats.resources.into(),
    }
  }
}

impl From<fastrender::ResourceDiagnostics> for SerializableResourceDiagnostics {
  fn from(resources: fastrender::ResourceDiagnostics) -> Self {
    let mut fetch_counts = BTreeMap::new();
    for (kind, count) in resources.fetch_counts {
      fetch_counts.insert(resource_kind_key(kind).to_string(), count);
    }
    Self {
      fetch_counts,
      image_cache_hits: resources.image_cache_hits,
      image_cache_misses: resources.image_cache_misses,
    }
  }
}

fn resource_kind_key(kind: ResourceKind) -> &'static str {
  match kind {
    ResourceKind::Document => "document",
    ResourceKind::Stylesheet => "stylesheet",
    ResourceKind::Image => "image",
    ResourceKind::Font => "font",
    ResourceKind::Other => "other",
  }
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

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json::json;
  use std::collections::BTreeMap;

  #[test]
  fn perf_smoke_report_schema_is_stable() {
    let mut fetch_counts = BTreeMap::new();
    fetch_counts.insert("document".to_string(), 1);
    fetch_counts.insert("image".to_string(), 2);

    let report = PerfSmokeReport {
      version: PERF_SMOKE_REPORT_VERSION,
      totals: PerfSmokeTotals {
        fixtures: 1,
        wall_time_ms: 12.5,
        render_wall_time_ms: 10.0,
        stage_time_ms: RenderStageTimings {
          html_decode_ms: Some(1.0),
          dom_parse_ms: Some(2.0),
          ..RenderStageTimings::default()
        },
      },
      fixtures: vec![FixtureReport {
        id: "demo".to_string(),
        fixture: "demo".to_string(),
        shot: "default".to_string(),
        html: "demo/index.html".to_string(),
        viewport: (800, 600),
        dpr: 1.0,
        media: "screen".to_string(),
        wall_time_ms: 10.0,
        stage_total_ms: Some(5.5),
        stats: SerializableRenderStats {
          timings: RenderStageTimings {
            html_decode_ms: Some(1.0),
            dom_parse_ms: Some(2.0),
            cascade_ms: Some(2.5),
            ..RenderStageTimings::default()
          },
          counts: fastrender::RenderCounts {
            dom_nodes: Some(4),
            styled_nodes: Some(3),
            fragments: Some(5),
            ..Default::default()
          },
          cascade: fastrender::CascadeDiagnostics {
            nodes: Some(10),
            rule_candidates: Some(4),
            ..Default::default()
          },
          layout: fastrender::LayoutDiagnostics {
            intrinsic_lookups: Some(1),
            ..Default::default()
          },
          paint: fastrender::PaintDiagnostics {
            display_items: Some(8),
            ..Default::default()
          },
          resources: SerializableResourceDiagnostics {
            fetch_counts,
            image_cache_hits: None,
            image_cache_misses: None,
          },
        },
      }],
    };

    let serialized = serde_json::to_value(&report).expect("serialize report");
    let expected = json!({
      "version": PERF_SMOKE_REPORT_VERSION,
      "totals": {
        "fixtures": 1,
        "wall_time_ms": 12.5,
        "render_wall_time_ms": 10.0,
        "stage_time_ms": {
          "html_decode_ms": 1.0,
          "dom_parse_ms": 2.0,
          "css_inlining_ms": null,
          "css_parse_ms": null,
          "cascade_ms": null,
          "box_tree_ms": null,
          "layout_ms": null,
          "text_fallback_ms": null,
          "text_shape_ms": null,
          "paint_build_ms": null,
          "paint_optimize_ms": null,
          "paint_rasterize_ms": null,
          "text_rasterize_ms": null,
          "encode_ms": null
        }
      },
      "fixtures": [
        {
          "id": "demo",
          "fixture": "demo",
          "shot": "default",
          "html": "demo/index.html",
          "viewport": [800, 600],
          "dpr": 1.0,
          "media": "screen",
          "wall_time_ms": 10.0,
          "stage_total_ms": 5.5,
          "stats": {
            "timings": {
              "html_decode_ms": 1.0,
              "dom_parse_ms": 2.0,
              "css_inlining_ms": null,
              "css_parse_ms": null,
              "cascade_ms": 2.5,
              "box_tree_ms": null,
              "layout_ms": null,
              "text_fallback_ms": null,
              "text_shape_ms": null,
              "paint_build_ms": null,
              "paint_optimize_ms": null,
              "paint_rasterize_ms": null,
              "text_rasterize_ms": null,
              "encode_ms": null
            },
            "counts": {
              "dom_nodes": 4,
              "styled_nodes": 3,
              "box_nodes": null,
              "fragments": 5,
              "shaped_runs": null,
              "glyphs": null,
              "color_glyph_rasters": null,
              "fallback_cache_hits": null,
              "fallback_cache_misses": null,
              "last_resort_font_fallbacks": null,
              "glyph_cache_hits": null,
              "glyph_cache_misses": null,
              "glyph_cache_evictions": null,
              "glyph_cache_bytes": null,
              "color_glyph_cache_hits": null,
              "color_glyph_cache_misses": null,
              "color_glyph_cache_evictions": null,
              "color_glyph_cache_bytes": null
            },
            "cascade": {
              "nodes": 10,
              "rule_candidates": 4,
              "rule_matches": null,
              "rule_candidates_pruned": null,
              "rule_candidates_by_id": null,
              "rule_candidates_by_class": null,
              "rule_candidates_by_tag": null,
              "rule_candidates_by_attr": null,
              "rule_candidates_universal": null,
              "selector_time_ms": null,
              "declaration_time_ms": null,
              "pseudo_time_ms": null,
              "has_evals": null,
              "has_cache_hits": null,
              "has_prunes": null,
              "has_bloom_prunes": null,
              "has_filter_prunes": null,
              "has_evaluated": null
            },
            "layout": {
              "intrinsic_lookups": 1,
              "intrinsic_hits": null,
              "intrinsic_stores": null,
              "block_intrinsic": null,
              "flex_intrinsic": null,
              "inline_intrinsic": null,
              "layout_cache_lookups": null,
              "layout_cache_hits": null,
              "layout_cache_stores": null,
              "layout_cache_evictions": null,
              "layout_cache_clones": null,
              "flex_cache_clones": null,
              "taffy_nodes_built": null,
              "taffy_nodes_reused": null,
              "taffy_style_cache_hits": null,
              "taffy_style_cache_misses": null,
              "fragment_deep_clones": null,
              "fragment_traversed": null
            },
            "paint": {
              "display_items": 8,
              "optimized_items": null,
              "culled_items": null,
              "transparent_removed": null,
              "noop_removed": null,
              "merged_items": null,
              "gradient_ms": null,
              "gradient_pixels": null,
              "parallel_tasks": null,
              "parallel_threads": null,
              "parallel_ms": null,
              "serial_ms": null,
              "filter_cache_hits": null,
              "filter_cache_misses": null,
              "blur_cache_hits": null,
              "blur_cache_misses": null,
              "blur_tiles": null
            },
            "resources": {
              "fetch_counts": { "document": 1, "image": 2 },
              "image_cache_hits": null,
              "image_cache_misses": null
            }
          }
        }
      ]
    });

    assert_eq!(serialized, expected);
  }
}
