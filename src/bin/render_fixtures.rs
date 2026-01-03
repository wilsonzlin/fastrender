use clap::Parser;
use fastrender::image_output::encode_image;
use fastrender::{FastRender, FontConfig, RenderOptions, ResourcePolicy};
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;
use url::Url;

/// Render offline page fixtures under `tests/pages/fixtures` into a flat PNG directory.
///
/// This binary is intended for deterministic offline evidence loops (e.g. comparing FastRender
/// output against a headless Chrome baseline).
#[derive(Parser, Debug)]
#[command(name = "render_fixtures", version, about)]
struct Args {
  /// Root directory containing fixture subdirectories.
  ///
  /// Each fixture is expected to live under `<fixtures-root>/<fixture>/index.html`.
  #[arg(long, default_value = "tests/pages/fixtures", value_name = "DIR")]
  fixtures_root: PathBuf,

  /// Only render fixtures matching these names (comma-separated).
  #[arg(long, value_delimiter = ',')]
  only: Option<Vec<String>>,

  /// Viewport size as WxH (e.g. 1200x800).
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset.
  #[arg(long, default_value_t = 1.0)]
  dpr: f32,

  /// Hard per-fixture timeout in seconds (0 disables).
  #[arg(long, default_value_t = 5)]
  timeout: u64,

  /// Output directory to write PNGs into.
  #[arg(long, default_value = "target/render_fixtures", value_name = "DIR")]
  out: PathBuf,

  /// Positional fixture names (equivalent to `--only`).
  #[arg(value_name = "FIXTURE")]
  fixtures: Vec<String>,
}

fn main() {
  if let Err(err) = run() {
    eprintln!("error: {err}");
    std::process::exit(1);
  }
}

fn run() -> Result<(), String> {
  let args = Args::parse();

  if !args.dpr.is_finite() || args.dpr <= 0.0 {
    return Err("--dpr must be a finite number > 0".to_string());
  }

  if !args.fixtures_root.is_dir() {
    return Err(format!(
      "--fixtures-root {} is not a directory",
      args.fixtures_root.display()
    ));
  }

  let mut requested = Vec::new();
  if let Some(only) = args.only.clone() {
    requested.extend(only);
  }
  requested.extend(args.fixtures.clone());
  let requested: Option<BTreeSet<String>> = if requested.is_empty() {
    None
  } else {
    Some(requested.into_iter().collect())
  };

  let mut candidates = discover_fixtures(&args.fixtures_root)?;
  candidates.sort();

  let fixtures: Vec<String> = if let Some(requested) = requested {
    let candidate_set: BTreeSet<String> = candidates.iter().cloned().collect();
    let missing: Vec<String> = requested
      .iter()
      .filter(|name| !candidate_set.contains(*name))
      .cloned()
      .collect();
    if !missing.is_empty() {
      return Err(format!(
        "unknown fixture(s) under {}: {}",
        args.fixtures_root.display(),
        missing.join(", ")
      ));
    }
    requested.into_iter().collect()
  } else {
    candidates
  };

  if fixtures.is_empty() {
    return Err(format!(
      "no fixtures discovered under {}",
      args.fixtures_root.display()
    ));
  }

  fs::create_dir_all(&args.out)
    .map_err(|e| format!("failed to create output dir {}: {e}", args.out.display()))?;

  // Keep renders deterministic across machines by using the bundled font set and blocking
  // network fetches.
  let fetch_policy = ResourcePolicy::default()
    .allow_http(false)
    .allow_https(false)
    .allow_file(true)
    .allow_data(true);

  let mut renderer = FastRender::builder()
    .font_sources(FontConfig::bundled_only())
    .resource_policy(fetch_policy)
    .build()
    .map_err(|e| format!("failed to build renderer: {e:?}"))?;

  let timeout = if args.timeout == 0 {
    None
  } else {
    Some(Duration::from_secs(args.timeout))
  };

  let mut failures = Vec::new();

  for fixture in fixtures {
    let dir = args.fixtures_root.join(&fixture);
    let html_path = dir.join("index.html");
    if !html_path.is_file() {
      failures.push(format!("{fixture}: missing {}/index.html", dir.display()));
      continue;
    }

    let canonical_dir = fs::canonicalize(&dir)
      .map_err(|e| format!("failed to canonicalize {}: {e}", dir.display()))?;
    let base_url = Url::from_directory_path(&canonical_dir)
      .map_err(|_| format!("failed to build base_url for {}", dir.display()))?
      .to_string();
    renderer.set_base_url(base_url);

    let html =
      fs::read_to_string(&html_path).map_err(|e| format!("read {}: {e}", html_path.display()))?;

    let options = RenderOptions::new()
      .with_viewport(args.viewport.0, args.viewport.1)
      .with_device_pixel_ratio(args.dpr)
      .with_timeout(timeout);

    let rendered = match renderer.render_html_with_options(&html, options) {
      Ok(pixmap) => pixmap,
      Err(err) => {
        failures.push(format!("{fixture}: render failed: {err:?}"));
        continue;
      }
    };

    let png = encode_image(&rendered, fastrender::OutputFormat::Png)
      .map_err(|e| format!("{fixture}: failed to encode PNG: {e:?}"))?;

    let out_path = args.out.join(format!("{fixture}.png"));
    if let Err(err) = fs::write(&out_path, png) {
      failures.push(format!(
        "{fixture}: failed to write {}: {err}",
        out_path.display()
      ));
      continue;
    }

    println!("✓ {fixture}");
  }

  if !failures.is_empty() {
    eprintln!("render_fixtures failures ({}):", failures.len());
    for failure in &failures {
      eprintln!("  {failure}");
    }
    return Err("one or more fixtures failed".to_string());
  }

  Ok(())
}

fn discover_fixtures(root: &Path) -> Result<Vec<String>, String> {
  let mut fixtures = Vec::new();
  let entries = fs::read_dir(root).map_err(|e| format!("read {}: {e}", root.display()))?;

  for entry in entries {
    let entry = entry.map_err(|e| format!("read entry under {}: {e}", root.display()))?;
    let ty = entry
      .file_type()
      .map_err(|e| format!("stat {}: {e}", entry.path().display()))?;
    if !ty.is_dir() {
      continue;
    }
    let path = entry.path();
    if path.join("index.html").is_file() {
      fixtures.push(entry.file_name().to_string_lossy().to_string());
    }
  }

  Ok(fixtures)
}

fn parse_viewport(raw: &str) -> Result<(u32, u32), String> {
  let (width, height) = raw
    .split_once('x')
    .ok_or_else(|| "viewport must be WxH (e.g. 1200x800)".to_string())?;

  let width = width.parse::<u32>().map_err(|_| "invalid width".to_string())?;
  let height = height.parse::<u32>().map_err(|_| "invalid height".to_string())?;

  if width == 0 || height == 0 {
    return Err("viewport width/height must be > 0".to_string());
  }

  Ok((width, height))
}
