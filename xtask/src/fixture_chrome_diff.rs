use anyhow::{bail, Context, Result};
use clap::{Args, ValueEnum};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
pub enum JsMode {
  On,
  Off,
}

impl JsMode {
  fn as_cli_value(self) -> &'static str {
    match self {
      JsMode::On => "on",
      JsMode::Off => "off",
    }
  }
}

#[derive(Args, Debug)]
pub struct FixtureChromeDiffArgs {
  /// Root directory containing offline fixtures.
  #[arg(long, default_value = "tests/pages/fixtures", value_name = "DIR")]
  fixtures_root: PathBuf,

  /// Only process listed fixtures (comma-separated).
  #[arg(long, value_delimiter = ',')]
  only: Option<Vec<String>>,

  /// Positional fixture filters (equivalent to `--only`)
  #[arg(value_name = "FIXTURE")]
  fixtures: Vec<String>,

  /// Viewport size as WxH (e.g. 1200x800).
  #[arg(long, value_parser = crate::parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset.
  #[arg(long, default_value_t = 1.0)]
  dpr: f32,

  /// Per-page hard timeout for Chrome (seconds).
  #[arg(long, default_value_t = 15)]
  timeout: u64,

  /// Per-fixture hard timeout for FastRender (seconds).
  #[arg(long, default_value_t = 5)]
  render_timeout: u64,

  /// Chrome/Chromium binary path override.
  #[arg(long)]
  chrome: Option<PathBuf>,

  /// Enable JavaScript in the Chrome baseline render.
  #[arg(long, value_enum, default_value_t = JsMode::Off)]
  js: JsMode,

  /// Per-channel diff tolerance (0 = exact match).
  #[arg(long, default_value_t = 0)]
  tolerance: u8,

  /// Maximum percent of pixels allowed to differ (0-100).
  #[arg(long, default_value_t = 0.0)]
  max_diff_percent: f64,

  /// Where to write outputs (renders + report).
  #[arg(long, default_value = "target/fixture_chrome_diff", value_name = "DIR")]
  out: PathBuf,

  /// Skip Chrome baseline rendering and use this directory as the "before" renders.
  #[arg(long, value_name = "DIR")]
  chrome_dir: Option<PathBuf>,

  /// Alias for "require --chrome-dir" (useful in CI where Chrome is unavailable).
  #[arg(long, requires = "chrome_dir")]
  no_chrome: bool,
}

pub fn run_fixture_chrome_diff(args: FixtureChromeDiffArgs) -> Result<()> {
  if !args.dpr.is_finite() || args.dpr <= 0.0 {
    bail!("--dpr must be a finite number > 0");
  }
  if !(0.0..=100.0).contains(&args.max_diff_percent) {
    bail!("--max-diff-percent must be between 0 and 100");
  }

  let repo_root = crate::repo_root();
  let fixtures_root = resolve_repo_path(&repo_root, &args.fixtures_root);
  let out_dir = resolve_repo_path(&repo_root, &args.out);

  let filter = merge_filters(&args.only, &args.fixtures);

  if !fixtures_root.is_dir() {
    bail!(
      "--fixtures-root {} is not a directory",
      fixtures_root.display()
    );
  }

  fs::create_dir_all(&out_dir)
    .with_context(|| format!("create output directory {}", out_dir.display()))?;

  let viewport = format!("{}x{}", args.viewport.0, args.viewport.1);
  let fastrender_dir = out_dir.join("fastrender");

  ensure_clean_dir(&fastrender_dir)?;

  // Step A: FastRender renders.
  let mut render_cmd = Command::new("cargo");
  // Keep fixture renders deterministic across machines.
  render_cmd.env("FASTR_USE_BUNDLED_FONTS", "1");
  render_cmd
    .arg("run")
    .args(["--bin", "render_fixtures", "--"])
    .arg("--fixtures-dir")
    .arg(&fixtures_root)
    .arg("--out-dir")
    .arg(&fastrender_dir)
    .arg("--viewport")
    .arg(&viewport)
    .arg("--dpr")
    .arg(args.dpr.to_string())
    .arg("--timeout")
    .arg(args.render_timeout.to_string());
  if let Some(filter) = &filter {
    render_cmd.arg("--fixtures").arg(filter);
  }
  render_cmd.current_dir(&repo_root);

  println!("Rendering fixtures with FastRender...");
  crate::run_command(render_cmd)?;

  // Step B: Chrome baseline renders.
  let chrome_before_dir = if let Some(chrome_dir) = &args.chrome_dir {
    resolve_repo_path(&repo_root, chrome_dir)
  } else {
    let chrome_out_dir = out_dir.join("chrome");
    ensure_clean_dir(&chrome_out_dir)?;

    let mut chrome_cmd = Command::new("cargo");
    chrome_cmd
      .arg("xtask")
      .arg("chrome-baseline-fixtures")
      .arg("--fixtures-root")
      .arg(&fixtures_root)
      .arg("--out-dir")
      .arg(&chrome_out_dir)
      .arg("--viewport")
      .arg(&viewport)
      .arg("--dpr")
      .arg(args.dpr.to_string())
      .arg("--timeout")
      .arg(args.timeout.to_string())
      .arg("--js")
      .arg(args.js.as_cli_value());
    if let Some(chrome) = &args.chrome {
      chrome_cmd.arg("--chrome").arg(chrome);
    }
    if let Some(filter) = &filter {
      chrome_cmd.arg("--only").arg(filter);
    }
    chrome_cmd.current_dir(&repo_root);

    println!("Rendering fixtures with headless Chrome...");
    crate::run_command(chrome_cmd)?;
    chrome_out_dir
  };

  if !chrome_before_dir.is_dir() {
    bail!(
      "--chrome-dir {} is not a directory",
      chrome_before_dir.display()
    );
  }

  // Step C: Diff.
  let report_json = out_dir.join("report.json");
  let report_html = out_dir.join("report.html");

  let mut diff_cmd = Command::new("cargo");
  diff_cmd
    .arg("run")
    .args(["--bin", "diff_renders", "--"])
    .arg("--before")
    .arg(&chrome_before_dir)
    .arg("--after")
    .arg(&fastrender_dir)
    .arg("--tolerance")
    .arg(args.tolerance.to_string())
    .arg("--max-diff-percent")
    .arg(args.max_diff_percent.to_string())
    .arg("--json")
    .arg(&report_json)
    .arg("--html")
    .arg(&report_html);
  diff_cmd.current_dir(&repo_root);

  println!("Diffing Chrome vs FastRender renders...");
  crate::print_command(&diff_cmd);
  let status = diff_cmd
    .status()
    .with_context(|| format!("failed to run {:?}", diff_cmd.get_program()))?;
  println!("Report: {}", report_html.display());
  if !status.success() {
    bail!(
      "diff_renders failed with status {status} (report written to {})",
      report_html.display()
    );
  }

  Ok(())
}

fn merge_filters(flag: &Option<Vec<String>>, positional: &[String]) -> Option<String> {
  let mut parts = Vec::new();
  if let Some(flag) = flag {
    parts.extend(flag.iter().cloned());
  }
  parts.extend(positional.iter().cloned());

  if parts.is_empty() {
    None
  } else {
    Some(parts.join(","))
  }
}

fn resolve_repo_path(repo_root: &Path, path: &Path) -> PathBuf {
  if path.is_absolute() {
    path.to_path_buf()
  } else {
    repo_root.join(path)
  }
}

fn ensure_clean_dir(path: &Path) -> Result<()> {
  if path.exists() {
    if path.is_file() {
      bail!("expected directory at {}, found a file", path.display());
    }
    fs::remove_dir_all(path)
      .with_context(|| format!("remove existing directory {}", path.display()))?;
  }
  fs::create_dir_all(path).with_context(|| format!("create directory {}", path.display()))?;
  Ok(())
}
