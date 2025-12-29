use anyhow::{anyhow, bail, Context, Result};
use clap::{Args, Parser, Subcommand, ValueEnum};
use image::{Rgba, RgbaImage};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use url::Url;
use walkdir::WalkDir;

fn main() -> Result<()> {
  let cli = Cli::parse();

  match cli.command {
    Commands::Test(args) => run_tests(args),
    Commands::UpdateGoldens(args) => run_update_goldens(args),
    Commands::RenderPage(args) => run_render_page(args),
    Commands::Pageset(args) => run_pageset(args),
    Commands::DiffRenders(args) => run_diff_renders(args),
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
  /// Compare two render outputs and write visual diffs
  DiffRenders(DiffRendersArgs),
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

  /// Skip fetching and only update progress from the existing cache
  #[arg(long)]
  no_fetch: bool,

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

  if !args.no_fetch {
    let mut cmd = Command::new("cargo");
    cmd.arg("run")
      .arg("--release")
      .args(["--bin", "fetch_pages"])
      .arg("--")
      .arg("--jobs")
      .arg(args.jobs.to_string())
      .arg("--timeout")
      .arg(args.fetch_timeout.to_string());
    println!("Updating cached pages...");
    run_command(cmd)?;
  }

  let mut cmd = Command::new("cargo");
  cmd.arg("run")
    .arg("--release")
    .args(["--bin", "pageset_progress"])
    .arg("--")
    .arg("run")
    .arg("--jobs")
    .arg(args.jobs.to_string())
    .arg("--timeout")
    .arg(args.render_timeout.to_string());
  cmd.args(&args.extra);
  println!("Updating progress/pages scoreboard...");
  run_command(cmd)?;

  Ok(())
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
