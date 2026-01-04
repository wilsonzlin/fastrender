use anyhow::{bail, Context, Result};
use clap::{Args, ValueEnum};
use regex::Regex;
use serde::Deserialize;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const DEFAULT_FIXTURES_DIR: &str = "tests/pages/fixtures";
const DEFAULT_OUT_DIR: &str = "target/fixture_chrome_diff";
const DEFAULT_VIEWPORT: &str = "1040x1240";
const DEFAULT_DPR: f32 = 1.0;
const DEFAULT_TIMEOUT: u64 = 15;
const PAGES_REGRESSION_TEST_RS: &str = "tests/pages_regression_test.rs";

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

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum, Deserialize)]
#[clap(rename_all = "lowercase")]
#[serde(rename_all = "lowercase")]
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

#[derive(Debug, Deserialize)]
struct ChromeFixtureMetadata {
  viewport: (u32, u32),
  dpr: f32,
  js: JsMode,
}

#[derive(Args, Debug)]
pub struct FixtureChromeDiffArgs {
  /// Root directory containing offline fixture directories.
  #[arg(long, value_name = "DIR", default_value = DEFAULT_FIXTURES_DIR)]
  pub fixtures_dir: PathBuf,

  /// Only render fixtures matching these names (comma-separated stems).
  ///
  /// When omitted, the default fixture set is derived from `tests/pages_regression_test.rs` (the
  /// curated pages_regression suite). Pass `--all-fixtures` to render everything under
  /// `--fixtures-dir` instead.
  #[arg(long, value_delimiter = ',', value_name = "STEM,...")]
  pub fixtures: Option<Vec<String>>,

  /// Render every fixture under `--fixtures-dir` (disable the default pages_regression fixture set).
  #[arg(long, conflicts_with = "fixtures")]
  pub all_fixtures: bool,

  /// Only process a deterministic shard of the fixtures (index/total, 0-based).
  #[arg(long, value_parser = crate::parse_shard)]
  pub shard: Option<(usize, usize)>,

  /// Number of parallel fixture renders for the FastRender step (forwarded to `render_fixtures --jobs/-j`).
  #[arg(long, short, value_name = "N")]
  pub jobs: Option<usize>,

  /// Also write per-fixture snapshots/diagnostics (forwarded to `render_fixtures --write-snapshot`).
  #[arg(long)]
  pub write_snapshot: bool,

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

  /// Expand the FastRender paint canvas to fit the laid-out content bounds (forwarded to `render_fixtures --fit-canvas-to-content`).
  ///
  /// This is useful for paginated print fixtures, where pages are stacked in the fragment tree and
  /// would otherwise be clipped to the viewport-sized output canvas.
  #[arg(long)]
  pub fit_canvas_to_content: bool,

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

  /// When reusing an existing `<out>/fastrender` directory (`--no-fastrender`), require per-fixture
  /// FastRender metadata files (`<stem>.json`) to be present.
  ///
  /// Without this flag, missing metadata produces a warning and the command continues (to remain
  /// backwards compatible with older output directories).
  #[arg(long)]
  pub require_fastrender_metadata: bool,

  /// Explicit Chrome/Chromium binary path forwarded to the Chrome baseline step.
  #[arg(long, value_name = "PATH", conflicts_with = "chrome_dir")]
  pub chrome: Option<PathBuf>,

  /// Directory searched for a Chrome binary before PATH (useful for tests).
  #[arg(long, value_name = "DIR", conflicts_with = "chrome")]
  pub chrome_dir: Option<PathBuf>,

  /// Skip generating Chrome screenshots and only diff against the existing Chrome output dir.
  #[arg(long)]
  pub no_chrome: bool,

  /// When `--no-chrome` is set, require per-fixture Chrome metadata JSON to exist.
  ///
  /// Older output directories may not contain `<out>/chrome/<fixture>.json`. By default this
  /// command will warn and continue when metadata is missing to keep older dirs usable; set this
  /// flag to treat missing metadata as an error.
  #[arg(long)]
  pub require_chrome_metadata: bool,

  /// Print the computed plan (commands + output paths) without executing.
  #[arg(long, hide = true)]
  pub dry_run: bool,

  /// Alias for `--no-chrome --no-fastrender`.
  #[arg(long)]
  pub diff_only: bool,

  /// Skip building `diff_renders` and reuse an existing release binary under CARGO_TARGET_DIR.
  #[arg(long)]
  pub no_build: bool,
}

pub fn run_fixture_chrome_diff(args: FixtureChromeDiffArgs) -> Result<()> {
  let mut args = args;
  if args.diff_only {
    args.no_chrome = true;
    args.no_fastrender = true;
  }

  validate_args(&args)?;

  let repo_root = crate::repo_root();
  let fixtures_root = resolve_repo_path(&repo_root, &args.fixtures_dir);
  if !fixtures_root.is_dir() {
    bail!(
      "fixtures directory does not exist: {}",
      fixtures_root.display()
    );
  }

  apply_default_fixture_selection(&repo_root, &mut args);

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
  if args.no_build && !diff_renders_exe.is_file() {
    bail!(
      "--no-build was set, but diff_renders executable does not exist at {}",
      diff_renders_exe.display()
    );
  }
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

  let selected_fixture_stems = if args.no_chrome {
    Some(discover_selected_fixture_stems(
      &fixtures_root,
      args.fixtures.as_deref(),
      args.shard,
    )?)
  } else {
    None
  };

  fs::create_dir_all(&layout.root).with_context(|| {
    format!(
      "failed to create fixture-chrome-diff output dir {}",
      layout.root.display()
    )
  })?;

  if args.no_chrome {
    if !layout.chrome.is_dir() {
      bail!(
        "--no-chrome was set, but chrome output dir does not exist: {}",
        layout.chrome.display()
      );
    }
    if let Some(stems) = selected_fixture_stems.as_deref() {
      validate_chrome_baseline_metadata(&layout.chrome, stems, &args)?;
    }
  } else {
    clear_dir(&layout.chrome).context("clear Chrome output dir")?;
  }
  if args.no_fastrender {
    if !layout.fastrender.is_dir() {
      bail!(
        "--no-fastrender was set, but FastRender output dir does not exist: {}",
        layout.fastrender.display()
      );
    }
    validate_fastrender_output_metadata(&fixtures_root, &layout, &args)
      .context("validate reused FastRender metadata")?;
  } else {
    clear_dir(&layout.fastrender).context("clear FastRender output dir")?;
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

  if args.no_build {
    println!("Skipping diff_renders build (--no-build set)...");
  } else {
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
  }

  println!("Diffing renders...");
  run_diff_renders_allowing_differences(diff_renders, &layout, args.fail_on_differences)?;

  println!("Report written to {}", layout.report_html.display());
  Ok(())
}

fn validate_args(args: &FixtureChromeDiffArgs) -> Result<()> {
  if args.require_chrome_metadata && !args.no_chrome {
    bail!("--require-chrome-metadata requires --no-chrome");
  }
  if args.dpr <= 0.0 || !args.dpr.is_finite() {
    bail!("--dpr must be a positive, finite number");
  }
  if let Some(jobs) = args.jobs {
    if jobs == 0 {
      bail!("--jobs must be > 0");
    }
  }
  if args.timeout == 0 {
    bail!("--timeout must be > 0");
  }
  if args.require_fastrender_metadata && !args.no_fastrender {
    bail!("--require-fastrender-metadata is only meaningful with --no-fastrender");
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

fn discover_selected_fixture_stems(
  fixtures_root: &Path,
  requested: Option<&[String]>,
  shard: Option<(usize, usize)>,
) -> Result<Vec<String>> {
  let mut stems = Vec::new();
  for entry in fs::read_dir(fixtures_root)
    .with_context(|| format!("read fixture directory {}", fixtures_root.display()))?
  {
    let entry = entry.context("read fixture dir entry")?;
    let file_type = entry.file_type().context("read fixture entry type")?;
    if !file_type.is_dir() {
      continue;
    }
    let dir = entry.path();
    if !dir.join("index.html").is_file() {
      continue;
    }
    stems.push(entry.file_name().to_string_lossy().to_string());
  }

  stems.sort();
  if stems.is_empty() {
    bail!(
      "no fixtures found under {} (expected <fixture>/index.html)",
      fixtures_root.display()
    );
  }

  if let Some(requested) = requested {
    let mut normalized = requested
      .iter()
      .map(|s| s.trim())
      .filter(|s| !s.is_empty())
      .collect::<Vec<_>>();
    normalized.sort();
    normalized.dedup();

    let want: HashSet<&str> = normalized.iter().copied().collect();
    let mut found = HashSet::<String>::new();
    stems.retain(|stem| {
      if want.contains(stem.as_str()) {
        found.insert(stem.clone());
        true
      } else {
        false
      }
    });

    let mut missing = normalized
      .iter()
      .filter(|stem| !found.contains::<str>(*stem))
      .map(|stem| stem.to_string())
      .collect::<Vec<_>>();
    missing.sort();
    missing.dedup();
    if !missing.is_empty() {
      bail!("unknown fixture stem(s): {}", missing.join(", "));
    }
  }

  if let Some((index, total)) = shard {
    stems = stems
      .into_iter()
      .enumerate()
      .filter(|(i, _)| i % total == index)
      .map(|(_, stem)| stem)
      .collect();
  }

  if stems.is_empty() {
    bail!("no fixtures selected");
  }

  Ok(stems)
}

fn validate_chrome_baseline_metadata(
  chrome_dir: &Path,
  stems: &[String],
  args: &FixtureChromeDiffArgs,
) -> Result<()> {
  for stem in stems {
    let metadata_path = chrome_dir.join(format!("{stem}.json"));
    if !metadata_path.exists() {
      if args.require_chrome_metadata {
        bail!(
          "missing chrome baseline metadata for fixture '{stem}' at {}.\n\
           This output directory may be stale; rerun without --no-chrome (or regenerate baselines).",
          metadata_path.display()
        );
      }
      eprintln!(
        "warning: missing chrome baseline metadata for fixture '{stem}' at {}; skipping baseline validation",
        metadata_path.display()
      );
      continue;
    }

    let bytes = fs::read(&metadata_path)
      .with_context(|| format!("read chrome fixture metadata {}", metadata_path.display()))?;
    let metadata: ChromeFixtureMetadata = serde_json::from_slice(&bytes)
      .with_context(|| format!("parse chrome fixture metadata {}", metadata_path.display()))?;

    let mismatch =
      metadata.viewport != args.viewport || metadata.dpr != args.dpr || metadata.js != args.js;
    if mismatch {
      bail!(
        "chrome baseline mismatch for fixture '{stem}'.\n\
         Baseline ({}): viewport {}x{}, dpr {}, js {}.\n\
         Current invocation: viewport {}x{}, dpr {}, js {}.\n\
         Rerun without --no-chrome (or regenerate baselines) so diffs are meaningful.",
        metadata_path.display(),
        metadata.viewport.0,
        metadata.viewport.1,
        metadata.dpr,
        metadata.js.as_cli_value(),
        args.viewport.0,
        args.viewport.1,
        args.dpr,
        args.js.as_cli_value(),
      );
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

#[derive(Debug, Deserialize)]
struct FastRenderFixtureMetadata {
  viewport: (u32, u32),
  dpr: f32,
  media: String,
  timeout_secs: u64,
}

fn validate_fastrender_output_metadata(
  fixtures_root: &Path,
  layout: &Layout,
  args: &FixtureChromeDiffArgs,
) -> Result<()> {
  let fixtures =
    discover_selected_fixture_stems(fixtures_root, args.fixtures.as_deref(), args.shard)?;

  let mut missing_metadata = Vec::<String>::new();
  for stem in fixtures {
    let metadata_path = layout.fastrender.join(format!("{stem}.json"));
    if !metadata_path.is_file() {
      if args.require_fastrender_metadata {
        bail!(
          "FastRender metadata is missing for fixture '{stem}'.\n\
           Expected: {}\n\
           Rerun without --no-fastrender to regenerate FastRender renders, or point --out-dir at a directory that contains matching metadata.",
          metadata_path.display()
        );
      }
      missing_metadata.push(stem);
      continue;
    }

    let bytes = fs::read(&metadata_path)
      .with_context(|| format!("read {}", metadata_path.display()))?;
    let metadata: FastRenderFixtureMetadata = serde_json::from_slice(&bytes)
      .with_context(|| format!("parse {}", metadata_path.display()))?;

    let wanted_media = args.media.as_cli_value();
    let mut mismatch = false;
    mismatch |= metadata.viewport != args.viewport;
    mismatch |= !metadata.dpr.is_finite() || (metadata.dpr - args.dpr).abs() > 1e-6;
    mismatch |= metadata.media != wanted_media;
    mismatch |= metadata.timeout_secs != args.timeout;

    if mismatch {
      bail!(
        "FastRender metadata mismatch for fixture '{stem}'. This likely means you are reusing stale FastRender renders.\n\
         Metadata: {}\n\
         Wanted: viewport {}x{}, dpr {}, media {}, timeout {}s\n\
         Found:  viewport {}x{}, dpr {}, media {}, timeout {}s\n\
         Rerun without --no-fastrender to regenerate FastRender renders, or choose an --out-dir that was rendered with matching settings.",
        metadata_path.display(),
        args.viewport.0,
        args.viewport.1,
        args.dpr,
        wanted_media,
        args.timeout,
        metadata.viewport.0,
        metadata.viewport.1,
        metadata.dpr,
        metadata.media,
        metadata.timeout_secs
      );
    }
  }

  if !missing_metadata.is_empty() {
    let sample = missing_metadata
      .iter()
      .take(5)
      .cloned()
      .collect::<Vec<_>>()
      .join(", ");
    let suffix = if missing_metadata.len() > 5 {
      format!(", ... (+{} more)", missing_metadata.len() - 5)
    } else {
      String::new()
    };
    eprintln!(
      "warning: FastRender metadata is missing for {} fixture(s) ({}{}). \
       Continuing without validation; rerun without --no-fastrender to regenerate, \
       or pass --require-fastrender-metadata to fail on missing metadata.",
      missing_metadata.len(),
      sample,
      suffix
    );
  }

  Ok(())
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
  if args.fit_canvas_to_content {
    cmd.arg("--fit-canvas-to-content");
  }
  if let Some(jobs) = args.jobs {
    cmd.arg("--jobs").arg(jobs.to_string());
  }
  if args.write_snapshot {
    cmd.arg("--write-snapshot");
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
  cmd.arg("--media").arg(args.media.as_cli_value());
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
    cmd.arg("--max-perceptual-distance").arg(dist.to_string());
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

fn apply_default_fixture_selection(repo_root: &Path, args: &mut FixtureChromeDiffArgs) {
  if args.all_fixtures || args.fixtures.is_some() {
    return;
  }

  let source = match fs::read_to_string(repo_root.join(PAGES_REGRESSION_TEST_RS)) {
    Ok(source) => source,
    Err(_) => return,
  };

  let fixtures = extract_pages_regression_fixture_stems(&source);
  if fixtures.is_empty() {
    return;
  }
  args.fixtures = Some(fixtures);
}

fn extract_pages_regression_fixture_stems(source: &str) -> Vec<String> {
  // Pages fixtures in `tests/pages_regression_test.rs` look like:
  //   html: "flex_dashboard/index.html",
  // so we can extract the fixture stem by matching the string literal up to `/index.html`.
  let re = Regex::new(r#"html:\s*"([^"]+)/index\.html""#)
    .expect("regex for pages_regression html paths should compile");
  let mut out = re
    .captures_iter(source)
    .filter_map(|caps| caps.get(1).map(|m| m.as_str().trim().to_string()))
    .filter(|stem| !stem.is_empty())
    .collect::<Vec<_>>();
  out.sort();
  out.dedup();
  out
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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn extract_pages_regression_fixture_stems_dedups_and_sorts() {
    let source = r#"
      const PAGE_FIXTURES: &[PageFixture] = &[
        PageFixture { name: "a", html: "zeta/index.html", shots: DEFAULT_SHOTS },
        PageFixture { name: "b", html:"alpha/index.html", shots: DEFAULT_SHOTS },
        PageFixture { name: "c", html: "alpha/index.html", shots: DEFAULT_SHOTS },
        // Ignore non-index references.
        PageFixture { name: "d", html: "not_a_fixture/other.html", shots: DEFAULT_SHOTS },
      ];
    "#;

    assert_eq!(
      extract_pages_regression_fixture_stems(source),
      vec!["alpha".to_string(), "zeta".to_string()]
    );
  }
}
