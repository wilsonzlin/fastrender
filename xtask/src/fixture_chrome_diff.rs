use anyhow::{bail, Context, Result};
use clap::{Args, ValueEnum};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const DEFAULT_FIXTURES_DIR: &str = "tests/pages/fixtures";
const DEFAULT_OUT_DIR: &str = "target/fixture_chrome_diff";
const DEFAULT_VIEWPORT: &str = "1040x1240";
const DEFAULT_DPR: f32 = 1.0;
const DEFAULT_TIMEOUT: u64 = 15;

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
#[clap(rename_all = "lowercase")]
enum SortBy {
  Pixel,
  Percent,
  Perceptual,
}

impl SortBy {
  fn as_cli_value(self) -> &'static str {
    match self {
      Self::Pixel => "pixel",
      Self::Percent => "percent",
      Self::Perceptual => "perceptual",
    }
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
#[clap(rename_all = "lowercase")]
enum MediaMode {
  Screen,
  Print,
}

impl MediaMode {
  fn as_cli_value(self) -> &'static str {
    match self {
      Self::Screen => "screen",
      Self::Print => "print",
    }
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
#[clap(rename_all = "lowercase")]
enum JsMode {
  On,
  Off,
}

impl JsMode {
  fn as_cli_value(self) -> &'static str {
    match self {
      Self::On => "on",
      Self::Off => "off",
    }
  }
}

#[derive(Args, Debug)]
pub struct FixtureChromeDiffArgs {
  /// Root directory containing offline fixture directories.
  #[arg(long, value_name = "DIR", default_value = DEFAULT_FIXTURES_DIR)]
  pub fixtures_dir: PathBuf,

  /// Only render fixtures matching these names (comma-separated stems).
  #[arg(long, value_delimiter = ',', value_name = "STEM,...")]
  pub fixtures: Option<Vec<String>>,

  /// Only process a deterministic shard of the fixtures (index/total, 0-based).
  #[arg(long, value_parser = crate::parse_shard)]
  pub shard: Option<(usize, usize)>,

  /// Root directory to write output artifacts into.
  #[arg(long, value_name = "DIR", default_value = DEFAULT_OUT_DIR)]
  pub out_dir: PathBuf,

  /// Viewport size as WxH (e.g. 1040x1240; forwarded to both renderers).
  #[arg(long, value_parser = crate::parse_viewport, default_value = DEFAULT_VIEWPORT)]
  pub viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset (forwarded to both renderers).
  #[arg(long, default_value_t = DEFAULT_DPR)]
  pub dpr: f32,

  /// Media type for evaluating media queries in the FastRender fixture step.
  #[arg(long, value_enum, default_value_t = MediaMode::Screen)]
  pub media: MediaMode,

  /// Per-fixture hard timeout in seconds (forwarded to both Chrome and FastRender steps).
  #[arg(long, default_value_t = DEFAULT_TIMEOUT, value_name = "SECS")]
  pub timeout: u64,

  /// Enable or disable JavaScript in the Chrome baseline step.
  #[arg(long, value_enum, default_value_t = JsMode::Off)]
  pub js: JsMode,

  /// Per-channel tolerance forwarded to `diff_renders`.
  #[arg(long, default_value_t = 0)]
  pub tolerance: u8,

  /// Maximum percent of pixels allowed to differ (0-100) forwarded to `diff_renders`.
  #[arg(long, default_value_t = 0.0)]
  pub max_diff_percent: f64,

  /// Maximum allowed perceptual distance (0.0 = identical) forwarded to `diff_renders`.
  #[arg(long)]
  pub max_perceptual_distance: Option<f64>,

  /// Sort report entries by metric within each status group (forwarded to `diff_renders`).
  #[arg(long, value_enum, default_value_t = SortBy::Percent)]
  pub sort_by: SortBy,
  /// Ignore alpha differences forwarded to `diff_renders --ignore-alpha`.
  #[arg(long)]
  pub ignore_alpha: bool,

  /// Exit non-zero when `diff_renders` reports differences.
  ///
  /// By default this command exits 0 even when differences are found so it can be used as a local
  /// inspection loop that always produces a report.
  #[arg(long)]
  pub fail_on_differences: bool,

  /// Skip rendering fixtures with FastRender and reuse the existing `<out>/fastrender` output dir.
  #[arg(long)]
  pub no_fastrender: bool,

  /// Explicit Chrome/Chromium binary path forwarded to the Chrome baseline step.
  #[arg(long, value_name = "PATH", conflicts_with = "chrome_dir")]
  pub chrome: Option<PathBuf>,

  /// Directory searched for a Chrome binary before PATH (useful for tests).
  #[arg(long, value_name = "DIR", conflicts_with = "chrome")]
  pub chrome_dir: Option<PathBuf>,

  /// Skip generating Chrome screenshots and only diff against the existing Chrome output dir.
  #[arg(long)]
  pub no_chrome: bool,

  /// Print the computed plan (commands + output paths) without executing.
  #[arg(long, hide = true)]
  pub dry_run: bool,
}

pub fn run_fixture_chrome_diff(args: FixtureChromeDiffArgs) -> Result<()> {
  validate_args(&args)?;

  let repo_root = crate::repo_root();
  let fixtures_root = resolve_repo_path(&repo_root, &args.fixtures_dir);
  if !fixtures_root.is_dir() {
    bail!(
      "fixtures directory does not exist: {}",
      fixtures_root.display()
    );
  }

  let out_root = resolve_repo_path(&repo_root, &args.out_dir);
  let layout = Layout::new(&out_root);

  let render_fixtures = if args.no_fastrender {
    None
  } else {
    Some(build_render_fixtures_command(
      &repo_root,
      &fixtures_root,
      &args,
      &layout,
    )?)
  };
  let chrome_baseline = if args.no_chrome {
    None
  } else {
    Some(build_chrome_baseline_command(
      &repo_root,
      &fixtures_root,
      &args,
      &layout,
    )?)
  };
  let diff_renders_exe = crate::diff_renders_executable(&repo_root);
  let diff_renders = build_diff_renders_command(&diff_renders_exe, &layout, &args)?;

  if args.dry_run {
    println!("fixture-chrome-diff plan:");
    println!("  out_dir: {}", layout.root.display());
    println!("  fastrender: {}", layout.fastrender.display());
    println!("  chrome: {}", layout.chrome.display());
    println!("  report: {}", layout.report_html.display());
    println!("  json: {}", layout.report_json.display());
    println!();

    if let Some(cmd) = render_fixtures.as_ref() {
      crate::print_command(cmd);
    }
    if let Some(cmd) = chrome_baseline.as_ref() {
      crate::print_command(cmd);
    }
    crate::print_command(&diff_renders);
    return Ok(());
  }

  fs::create_dir_all(&layout.root).with_context(|| {
    format!(
      "failed to create fixture-chrome-diff output dir {}",
      layout.root.display()
    )
  })?;

  if args.no_fastrender {
    if !layout.fastrender.is_dir() {
      bail!(
        "--no-fastrender was set, but FastRender output dir does not exist: {}",
        layout.fastrender.display()
      );
    }
  } else {
    clear_dir(&layout.fastrender).context("clear FastRender output dir")?;
  }
  if args.no_chrome {
    if !layout.chrome.is_dir() {
      bail!(
        "--no-chrome was set, but chrome output dir does not exist: {}",
        layout.chrome.display()
      );
    }
  } else {
    clear_dir(&layout.chrome).context("clear Chrome output dir")?;
  }
  remove_file_if_exists(&layout.report_html).context("clear existing report.html")?;
  remove_file_if_exists(&layout.report_json).context("clear existing report.json")?;

  if let Some(cmd) = render_fixtures {
    println!("Rendering fixtures with FastRender...");
    crate::run_command(cmd).context("render_fixtures failed")?;
  }

  if let Some(cmd) = chrome_baseline {
    println!("Rendering fixtures with Chrome baseline...");
    crate::run_command(cmd).context("chrome-baseline-fixtures failed")?;
  }

  // Avoid `cargo run` here since `diff_renders` intentionally exits 1 when differences are found,
  // and `cargo run` would wrap that with a scary `error: process didn't exit successfully` line.
  let mut build_cmd = Command::new("cargo");
  build_cmd
    .arg("build")
    .arg("--release")
    .args(["--bin", "diff_renders"])
    .current_dir(&repo_root);
  println!("Building diff_renders...");
  crate::run_command(build_cmd).context("build diff_renders failed")?;

  println!("Diffing renders...");
  run_diff_renders_allowing_differences(diff_renders, &layout, args.fail_on_differences)?;

  println!("Report written to {}", layout.report_html.display());
  Ok(())
}

fn validate_args(args: &FixtureChromeDiffArgs) -> Result<()> {
  if args.dpr <= 0.0 || !args.dpr.is_finite() {
    bail!("--dpr must be a positive, finite number");
  }
  if args.timeout == 0 {
    bail!("--timeout must be > 0");
  }
  if !(0.0..=100.0).contains(&args.max_diff_percent) || !args.max_diff_percent.is_finite() {
    bail!("--max-diff-percent must be between 0 and 100");
  }
  if let Some(dist) = args.max_perceptual_distance {
    if !(0.0..=1.0).contains(&dist) || !dist.is_finite() {
      bail!("--max-perceptual-distance must be a finite number between 0 and 1");
    }
  }
  Ok(())
}

struct Layout {
  root: PathBuf,
  fastrender: PathBuf,
  chrome: PathBuf,
  report_html: PathBuf,
  report_json: PathBuf,
}

impl Layout {
  fn new(root: &Path) -> Self {
    Self {
      root: root.to_path_buf(),
      fastrender: root.join("fastrender"),
      chrome: root.join("chrome"),
      report_html: root.join("report.html"),
      report_json: root.join("report.json"),
    }
  }
}

fn resolve_repo_path(repo_root: &Path, path: &Path) -> PathBuf {
  if path.is_absolute() {
    path.to_path_buf()
  } else {
    repo_root.join(path)
  }
}

fn build_render_fixtures_command(
  repo_root: &Path,
  fixtures_root: &Path,
  args: &FixtureChromeDiffArgs,
  layout: &Layout,
) -> Result<Command> {
  let mut cmd = Command::new("cargo");
  cmd.env("FASTR_USE_BUNDLED_FONTS", "1");
  cmd
    .arg("run")
    .arg("--release")
    .args(["--bin", "render_fixtures", "--"]);
  cmd.arg("--fixtures-dir").arg(fixtures_root);
  cmd.arg("--out-dir").arg(&layout.fastrender);
  cmd
    .arg("--viewport")
    .arg(format!("{}x{}", args.viewport.0, args.viewport.1));
  cmd.arg("--dpr").arg(args.dpr.to_string());
  cmd.arg("--media").arg(args.media.as_cli_value());
  cmd.arg("--timeout").arg(args.timeout.to_string());
  if let Some(fixtures) = &args.fixtures {
    cmd.arg("--fixtures").arg(fixtures.join(","));
  }
  if let Some((index, total)) = args.shard {
    cmd.arg("--shard").arg(format!("{index}/{total}"));
  }
  cmd.current_dir(repo_root);
  Ok(cmd)
}

fn build_chrome_baseline_command(
  repo_root: &Path,
  fixtures_root: &Path,
  args: &FixtureChromeDiffArgs,
  layout: &Layout,
) -> Result<Command> {
  let xtask = std::env::current_exe().context("resolve current xtask executable path")?;
  let mut cmd = Command::new(xtask);
  cmd.arg("chrome-baseline-fixtures");
  cmd.arg("--fixture-dir").arg(fixtures_root);
  cmd.arg("--out-dir").arg(&layout.chrome);
  cmd
    .arg("--viewport")
    .arg(format!("{}x{}", args.viewport.0, args.viewport.1));
  cmd.arg("--dpr").arg(args.dpr.to_string());
  cmd.arg("--timeout").arg(args.timeout.to_string());
  cmd.arg("--js").arg(args.js.as_cli_value());
  if let Some(chrome) = &args.chrome {
    cmd.arg("--chrome").arg(chrome);
  }
  if let Some(chrome_dir) = &args.chrome_dir {
    cmd.arg("--chrome-dir").arg(chrome_dir);
  }
  if let Some(fixtures) = &args.fixtures {
    cmd.arg("--fixtures").arg(fixtures.join(","));
  }
  if let Some((index, total)) = args.shard {
    cmd.arg("--shard").arg(format!("{index}/{total}"));
  }
  cmd.current_dir(repo_root);
  Ok(cmd)
}

fn build_diff_renders_command(
  diff_renders_exe: &Path,
  layout: &Layout,
  args: &FixtureChromeDiffArgs,
) -> Result<Command> {
  let mut cmd = Command::new(diff_renders_exe);
  cmd.arg("--before").arg(&layout.chrome);
  cmd.arg("--after").arg(&layout.fastrender);
  cmd.arg("--html").arg(&layout.report_html);
  cmd.arg("--json").arg(&layout.report_json);
  cmd.arg("--tolerance").arg(args.tolerance.to_string());
  cmd
    .arg("--max-diff-percent")
    .arg(args.max_diff_percent.to_string());
  if let Some(dist) = args.max_perceptual_distance {
    cmd
      .arg("--max-perceptual-distance")
      .arg(dist.to_string());
  }
  cmd.arg("--sort-by").arg(args.sort_by.as_cli_value());
  if args.ignore_alpha {
    cmd.arg("--ignore-alpha");
  }
  Ok(cmd)
}

fn clear_dir(path: &Path) -> Result<()> {
  if path.exists() {
    fs::remove_dir_all(path).with_context(|| format!("remove {}", path.display()))?;
  }
  fs::create_dir_all(path).with_context(|| format!("create {}", path.display()))?;
  Ok(())
}

fn remove_file_if_exists(path: &Path) -> Result<()> {
  if path.exists() {
    fs::remove_file(path).with_context(|| format!("remove {}", path.display()))?;
  }
  Ok(())
}

fn run_diff_renders_allowing_differences(
  mut cmd: Command,
  layout: &Layout,
  fail_on_differences: bool,
) -> Result<()> {
  crate::print_command(&cmd);
  let output = cmd
    .output()
    .with_context(|| format!("failed to run {:?}", cmd.get_program()))?;

  if !output.stdout.is_empty() {
    print!("{}", String::from_utf8_lossy(&output.stdout));
  }
  if !output.stderr.is_empty() {
    eprint!("{}", String::from_utf8_lossy(&output.stderr));
  }

  if output.status.success() {
    return Ok(());
  }

  if output.status.code() == Some(1) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    if stderr.trim_start().starts_with("error:") {
      bail!("diff_renders failed (see output above)");
    }
    if fail_on_differences {
      bail!(
        "diff_renders reported differences; report: {}",
        layout.report_html.display()
      );
    }
    eprintln!(
      "diff_renders reported differences; report: {}",
      layout.report_html.display()
    );
    return Ok(());
  }

  bail!("diff_renders failed with status {}", output.status);
}
