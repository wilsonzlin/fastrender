use anyhow::{anyhow, bail, Context, Result};
use clap::{Args, ValueEnum};
use image::{GenericImage, ImageBuffer, Rgba};
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::collections::HashSet;
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};
use tempfile::TempDir;
use url::Url;
use walkdir::WalkDir;

#[derive(Args, Debug)]
pub struct ChromeBaselineFixturesArgs {
  /// Root directory containing fixture directories (each must contain an index.html).
  #[arg(
    long,
    visible_aliases = ["fixtures-dir", "fixtures-root"],
    default_value = "tests/pages/fixtures",
    value_name = "DIR"
  )]
  fixture_dir: PathBuf,

  /// Directory to write PNGs/logs into.
  #[arg(
    long,
    default_value = "target/chrome_fixture_renders",
    value_name = "DIR"
  )]
  out_dir: PathBuf,

  /// Only render listed fixture directory names (comma-separated).
  #[arg(long, alias = "only", value_delimiter = ',', value_name = "STEM,...")]
  fixtures: Option<Vec<String>>,

  /// Positional fixture filters (equivalent to `--fixtures`).
  #[arg(value_name = "FIXTURE", conflicts_with = "fixtures", num_args = 0..)]
  fixtures_pos: Vec<String>,

  /// Process only a deterministic shard of discovered fixtures (index/total, 0-based).
  #[arg(long, value_parser = crate::parse_shard)]
  shard: Option<(usize, usize)>,

  /// Chrome/Chromium binary to run (defaults to auto-detect; can also be set via CHROME_BIN).
  #[arg(long, value_name = "PATH")]
  chrome: Option<PathBuf>,

  /// Directory to search for a `chrome`/`chromium` binary.
  ///
  /// When provided, auto-detection via PATH is disabled. This is primarily intended for tests.
  #[arg(long, value_name = "DIR")]
  chrome_dir: Option<PathBuf>,

  /// Viewport size as WxH (e.g. 1040x1240).
  #[arg(long, value_parser = crate::parse_viewport, default_value = "1040x1240")]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset.
  #[arg(long, default_value_t = 1.0)]
  dpr: f32,

  /// Media type for the baseline render.
  ///
  /// `screen` uses a viewport screenshot (like typical web rendering), while `print` uses Chrome's
  /// print-to-PDF pipeline and converts the resulting PDF into a stacked PNG so pagination can be
  /// diffed against FastRender.
  #[arg(long, value_enum, default_value_t = MediaMode::Screen)]
  media: MediaMode,

  /// Per-fixture hard timeout in seconds (0 = no timeout).
  #[arg(long, default_value_t = 15, value_name = "SECS")]
  timeout: u64,

  /// Enable or disable JavaScript (default: off).
  #[arg(long, value_enum, default_value_t = JsMode::Off)]
  js: JsMode,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum JsMode {
  On,
  Off,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
#[clap(rename_all = "lowercase")]
enum MediaMode {
  Screen,
  Print,
}

impl MediaMode {
  fn as_str(self) -> &'static str {
    match self {
      Self::Screen => "screen",
      Self::Print => "print",
    }
  }
}

#[derive(Debug, Clone)]
struct Fixture {
  stem: String,
  dir: PathBuf,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum HeadlessMode {
  New,
  Legacy,
}

#[derive(Debug, Serialize)]
struct FixtureMetadata {
  fixture: String,
  fixture_dir: PathBuf,
  viewport: (u32, u32),
  dpr: f32,
  media: &'static str,
  js: JsModeMetadata,
  input_sha256: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  assets_sha256: Option<String>,
  headless: &'static str,
  chrome_version: Option<String>,
  elapsed_ms: f64,
}

#[derive(Copy, Clone, Debug, Serialize)]
#[serde(rename_all = "lowercase")]
enum JsModeMetadata {
  On,
  Off,
}

pub fn run_chrome_baseline_fixtures(args: ChromeBaselineFixturesArgs) -> Result<()> {
  if args.dpr <= 0.0 || !args.dpr.is_finite() {
    bail!("--dpr must be a positive, finite number");
  }

  let repo_root = crate::repo_root();
  let fixture_root = absolutize_path(&repo_root, &args.fixture_dir);
  let out_dir = absolutize_path(&repo_root, &args.out_dir);
  fs::create_dir_all(&out_dir)
    .with_context(|| format!("create output dir {}", out_dir.display()))?;

  if !fixture_root.is_dir() {
    bail!("fixture dir not found: {}", fixture_root.display());
  }

  let chrome =
    resolve_chrome_binary(&args).with_context(|| "failed to locate a Chrome/Chromium binary")?;
  let chrome_version = chrome_version(&chrome).ok();

  let temp_root = create_temp_root(&chrome)?;

  let fixtures = discover_fixtures(&fixture_root)?;
  let requested = if let Some(list) = args.fixtures.as_deref() {
    Some(list)
  } else if !args.fixtures_pos.is_empty() {
    Some(args.fixtures_pos.as_slice())
  } else {
    None
  };
  let fixtures = select_fixtures(fixtures, requested, args.shard)?;

  println!("Chrome: {}", chrome.display());
  println!("Input:  {}", fixture_root.display());
  println!("Output: {}", out_dir.display());
  println!(
    "Viewport: {}x{}  DPR: {}  Media: {}  JS: {}  Timeout: {}s",
    args.viewport.0,
    args.viewport.1,
    args.dpr,
    args.media.as_str(),
    match args.js {
      JsMode::On => "on",
      JsMode::Off => "off",
    },
    args.timeout
  );
  println!();

  let mut ok = 0usize;
  let mut fail = 0usize;

  for fixture in fixtures {
    match render_fixture(
      &fixture,
      &chrome,
      chrome_version.as_deref(),
      &out_dir,
      temp_root.path(),
      &args,
    ) {
      Ok(()) => {
        ok += 1;
        println!("✓ {}", fixture.stem);
      }
      Err(err) => {
        fail += 1;
        eprintln!("✗ {}: {err:#}", fixture.stem);
      }
    }
  }

  println!();
  println!("Done: {ok} ok, {fail} failed");
  if fail > 0 {
    bail!(
      "{fail} fixture(s) failed (see logs under {})",
      out_dir.display()
    );
  }
  Ok(())
}

fn render_fixture(
  fixture: &Fixture,
  chrome: &Path,
  chrome_version: Option<&str>,
  out_dir: &Path,
  temp_root: &Path,
  args: &ChromeBaselineFixturesArgs,
) -> Result<()> {
  let index_path = fixture.dir.join("index.html");
  if !index_path.is_file() {
    bail!("missing index.html: {}", index_path.display());
  }
  let index_bytes =
    fs::read(&index_path).with_context(|| format!("read {}", index_path.display()))?;

  let output_png = out_dir.join(format!("{}.png", fixture.stem));
  let chrome_log = out_dir.join(format!("{}.chrome.log", fixture.stem));
  let metadata_path = out_dir.join(format!("{}.json", fixture.stem));

  // Avoid leaving stale output artifacts around when a fixture fails to render (for example when
  // Chrome times out or crashes). We treat each run as authoritative; if it fails, callers should
  // not accidentally reuse a PNG/metadata from an earlier successful run.
  for path in [&output_png, &chrome_log, &metadata_path] {
    if path.exists() {
      let _ = fs::remove_file(path);
    }
  }

  let profile_dir = temp_root.join("profile").join(&fixture.stem);
  fs::create_dir_all(&profile_dir)
    .with_context(|| format!("create chrome profile dir {}", profile_dir.display()))?;

  let base_url = Url::from_directory_path(&fixture.dir)
    .map(|u| u.to_string())
    .map_err(|_| {
      anyhow!(
        "could not convert {} to a file:// base URL",
        fixture.dir.display()
      )
    })?;
  let patched_dir = temp_root.join("html");
  fs::create_dir_all(&patched_dir).context("create patched HTML directory")?;
  let patched_html = patched_dir.join(format!("{}.html", fixture.stem));
  let patched = patch_html_bytes(
    &index_bytes,
    Some(&base_url),
    matches!(args.js, JsMode::Off),
  );
  fs::write(&patched_html, patched).with_context(|| format!("write {}", patched_html.display()))?;
  let url = file_url(&patched_html)?;

  let timeout = if args.timeout == 0 {
    None
  } else {
    Some(Duration::from_secs(args.timeout))
  };

  let start = Instant::now();
  let headless_used = match args.media {
    MediaMode::Screen => {
      let tmp_png_dir = temp_root.join("screenshots");
      fs::create_dir_all(&tmp_png_dir).context("create temp screenshot directory")?;
      let tmp_png_path = tmp_png_dir.join(format!("{}.png", fixture.stem));

      if tmp_png_path.exists() {
        let _ = fs::remove_file(&tmp_png_path);
      }

      let headless_used = run_chrome_screenshot(
        chrome,
        &url,
        &profile_dir,
        args.viewport,
        args.dpr,
        &tmp_png_path,
        &chrome_log,
        timeout,
      )?;

      let screenshot_len = fs::metadata(&tmp_png_path)
        .with_context(|| {
          format!(
            "chrome did not produce a screenshot for {} (see {})",
            fixture.stem,
            chrome_log.display()
          )
        })?
        .len();
      if screenshot_len == 0 {
        bail!(
          "chrome produced an empty screenshot for {} (see {})",
          fixture.stem,
          chrome_log.display()
        );
      }

      fs::copy(&tmp_png_path, &output_png).with_context(|| {
        format!(
          "copy screenshot from {} to {}",
          tmp_png_path.display(),
          output_png.display()
        )
      })?;

      headless_used
    }
    MediaMode::Print => {
      let print_dir = temp_root.join("print").join(&fixture.stem);
      if print_dir.exists() {
        let _ = fs::remove_dir_all(&print_dir);
      }
      fs::create_dir_all(&print_dir)
        .with_context(|| format!("create print scratch dir {}", print_dir.display()))?;

      let tmp_pdf_path = print_dir.join(format!("{}.pdf", fixture.stem));
      if tmp_pdf_path.exists() {
        let _ = fs::remove_file(&tmp_pdf_path);
      }

      let headless_used = run_chrome_print_to_pdf(
        chrome,
        &url,
        &profile_dir,
        args.viewport,
        args.dpr,
        &tmp_pdf_path,
        &chrome_log,
        timeout,
      )?;

      convert_pdf_to_stacked_png(
        &tmp_pdf_path,
        &output_png,
        args.dpr,
        &print_dir,
        &chrome_log,
      )?;

      headless_used
    }
  };

  let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;

  let metadata = build_fixture_metadata(
    fixture,
    args,
    headless_used,
    chrome_version,
    elapsed_ms,
    &index_bytes,
  )?;
  let json = serde_json::to_vec_pretty(&metadata).context("serialize chrome fixture metadata")?;
  fs::write(&metadata_path, json).with_context(|| format!("write {}", metadata_path.display()))?;

  Ok(())
}

fn build_fixture_metadata(
  fixture: &Fixture,
  args: &ChromeBaselineFixturesArgs,
  headless_used: HeadlessMode,
  chrome_version: Option<&str>,
  elapsed_ms: f64,
  index_html: &[u8],
) -> Result<FixtureMetadata> {
  let input_sha256 = sha256_hex(index_html);
  let assets_sha256 = compute_assets_sha256(&fixture.dir)?;

  Ok(FixtureMetadata {
    fixture: fixture.stem.clone(),
    fixture_dir: fixture.dir.clone(),
    viewport: args.viewport,
    dpr: args.dpr,
    media: args.media.as_str(),
    js: match args.js {
      JsMode::On => JsModeMetadata::On,
      JsMode::Off => JsModeMetadata::Off,
    },
    input_sha256,
    assets_sha256,
    headless: match headless_used {
      HeadlessMode::New => "new",
      HeadlessMode::Legacy => "legacy",
    },
    chrome_version: chrome_version.map(|v| v.to_string()),
    elapsed_ms,
  })
}

fn sha256_hex(bytes: &[u8]) -> String {
  let digest = Sha256::digest(bytes);
  digest.iter().map(|b| format!("{b:02x}")).collect()
}

fn compute_assets_sha256(fixture_dir: &Path) -> Result<Option<String>> {
  let assets_dir = fixture_dir.join("assets");
  if !assets_dir.is_dir() {
    return Ok(None);
  }

  let mut files = WalkDir::new(&assets_dir)
    .follow_links(false)
    .into_iter()
    .filter_map(|entry| entry.ok())
    .filter(|entry| entry.file_type().is_file())
    .map(|entry| entry.into_path())
    .collect::<Vec<_>>();

  files.sort_by(|a, b| {
    let a_rel = a.strip_prefix(&assets_dir).unwrap_or(a);
    let b_rel = b.strip_prefix(&assets_dir).unwrap_or(b);
    a_rel.to_string_lossy().cmp(&b_rel.to_string_lossy())
  });

  let mut hasher = Sha256::new();
  for path in files {
    let rel = path.strip_prefix(&assets_dir).unwrap_or(&path);
    hasher.update(rel.to_string_lossy().as_bytes());
    hasher.update([0u8]);
    let bytes = fs::read(&path).with_context(|| format!("read {}", path.display()))?;
    hasher.update(bytes);
    hasher.update([0u8]);
  }

  let digest = hasher.finalize();
  Ok(Some(digest.iter().map(|b| format!("{b:02x}")).collect()))
}

fn convert_pdf_to_stacked_png(
  pdf_path: &Path,
  output_png: &Path,
  dpr: f32,
  scratch_dir: &Path,
  log_path: &Path,
) -> Result<()> {
  if !pdf_path.is_file() {
    bail!("missing PDF output at {}", pdf_path.display());
  }

  let pdftoppm = find_in_path("pdftoppm").ok_or_else(|| {
    anyhow!(
      "pdftoppm not found in PATH.\n\
       Install poppler-utils (Ubuntu: `sudo apt-get install poppler-utils`) to enable --media print baselines."
    )
  })?;

  let dpi = (96.0 * dpr).round().max(1.0) as u32;
  let prefix = scratch_dir.join("pages");
  let prefix_str = prefix.to_string_lossy();

  let output = Command::new(&pdftoppm)
    .args(["-png", "-r"])
    .arg(dpi.to_string())
    .arg(pdf_path)
    .arg(prefix_str.as_ref())
    .output()
    .with_context(|| format!("run {}", pdftoppm.display()))?;

  if let Some(parent) = log_path.parent() {
    let _ = fs::create_dir_all(parent);
  }
  let mut log = OpenOptions::new()
    .create(true)
    .append(true)
    .open(log_path)
    .with_context(|| format!("open log file {}", log_path.display()))?;
  writeln!(log, "\n\n# pdf->png: {} -png -r {} {} {}", pdftoppm.display(), dpi, pdf_path.display(), prefix.display()).ok();
  if !output.stdout.is_empty() {
    let _ = log.write_all(&output.stdout);
  }
  if !output.stderr.is_empty() {
    let _ = log.write_all(&output.stderr);
  }

  if !output.status.success() {
    bail!("pdftoppm failed with status {}", output.status);
  }

  let mut pages: Vec<PathBuf> = fs::read_dir(scratch_dir)
    .with_context(|| format!("read {}", scratch_dir.display()))?
    .filter_map(|entry| entry.ok())
    .map(|entry| entry.path())
    .filter(|path| {
      path.extension().is_some_and(|ext| ext == "png")
        && path
          .file_name()
          .and_then(|name| name.to_str())
          .is_some_and(|name| name.starts_with("pages-"))
    })
    .collect();
  pages.sort_by_key(|path| {
    path
      .file_stem()
      .and_then(|stem| stem.to_str())
      .and_then(|stem| stem.rsplit('-').next())
      .and_then(|n| n.parse::<u32>().ok())
      .unwrap_or(u32::MAX)
  });
  if pages.is_empty() {
    bail!(
      "pdftoppm produced no PNG pages for {} (see {})",
      pdf_path.display(),
      log_path.display()
    );
  }

  let mut decoded = Vec::with_capacity(pages.len());
  let mut total_width: u32 = 0;
  let mut total_height: u32 = 0;
  for path in &pages {
    let image = image::open(path).with_context(|| format!("decode {}", path.display()))?;
    let rgba = image.to_rgba8();
    total_width = total_width.max(rgba.width());
    total_height = total_height.saturating_add(rgba.height());
    decoded.push(rgba);
  }

  let mut stacked: ImageBuffer<Rgba<u8>, Vec<u8>> =
    ImageBuffer::from_pixel(total_width, total_height, Rgba([255, 255, 255, 255]));
  let mut y: i64 = 0;
  for page in decoded {
    stacked
      .copy_from(&page, 0, y as u32)
      .context("stack PDF pages")?;
    y += page.height() as i64;
  }

  image::DynamicImage::ImageRgba8(stacked)
    .save_with_format(output_png, image::ImageFormat::Png)
    .with_context(|| format!("write {}", output_png.display()))?;

  Ok(())
}

fn run_chrome_print_to_pdf(
  chrome: &Path,
  url: &str,
  profile_dir: &Path,
  viewport: (u32, u32),
  dpr: f32,
  pdf_path: &Path,
  log_path: &Path,
  timeout: Option<Duration>,
) -> Result<HeadlessMode> {
  let mut args = build_chrome_print_args(
    HeadlessMode::New,
    profile_dir,
    viewport,
    dpr,
    pdf_path,
  )?;

  let mut last_status = run_chrome_with_timeout(chrome, &args, url, log_path, timeout, false)?;
  if last_status.success() && pdf_path.is_file() {
    return Ok(HeadlessMode::New);
  }

  let log = fs::read_to_string(log_path).unwrap_or_default();
  let log_lower = log.to_ascii_lowercase();
  let headless_new_unsupported = log_lower.contains("--headless=new")
    && (log_lower.contains("unknown flag")
      || log_lower.contains("unrecognized option")
      || log_lower.contains("unknown option"));
  if headless_new_unsupported {
    args = build_chrome_print_args(
      HeadlessMode::Legacy,
      profile_dir,
      viewport,
      dpr,
      pdf_path,
    )?;
    if pdf_path.exists() {
      let _ = fs::remove_file(pdf_path);
    }
    let mut file = OpenOptions::new()
      .create(true)
      .append(true)
      .open(log_path)
      .with_context(|| format!("open log file {}", log_path.display()))?;
    writeln!(file, "\n\n# Retrying with --headless\n").ok();
    last_status = run_chrome_with_timeout(chrome, &args, url, log_path, timeout, true)?;
    if last_status.success() && pdf_path.is_file() {
      return Ok(HeadlessMode::Legacy);
    }
  }

  if !last_status.success() {
    bail!(
      "chrome exited with {}; see {}",
      last_status,
      log_path.display()
    );
  }

  bail!(
    "chrome did not produce a PDF; see {}",
    log_path.display()
  );
}

fn build_chrome_print_args(
  headless: HeadlessMode,
  profile_dir: &Path,
  viewport: (u32, u32),
  dpr: f32,
  pdf_path: &Path,
) -> Result<Vec<String>> {
  let mut args = build_chrome_common_args(headless, profile_dir, viewport, dpr)?;
  args.push(format!("--print-to-pdf={}", pdf_path.display()));
  // Prefer the CSS @page header/footer content and keep PDF output deterministic.
  args.push("--print-to-pdf-no-header".to_string());
  Ok(args)
}

fn discover_fixtures(fixture_root: &Path) -> Result<Vec<Fixture>> {
  let mut fixtures = Vec::new();
  for entry in fs::read_dir(fixture_root)
    .with_context(|| format!("read fixture directory {}", fixture_root.display()))?
  {
    let entry = entry.context("read fixture dir entry")?;
    let file_type = entry.file_type().context("read fixture entry type")?;
    if !file_type.is_dir() {
      continue;
    }

    let stem = entry.file_name().to_string_lossy().to_string();
    let dir = entry.path();
    // The fixture root also contains shared support assets (e.g. `tests/pages/fixtures/assets`).
    // Only treat directories containing an `index.html` as renderable fixtures.
    if !dir.join("index.html").is_file() {
      continue;
    }
    fixtures.push(Fixture { stem, dir });
  }

  fixtures.sort_by(|a, b| a.stem.cmp(&b.stem));
  if fixtures.is_empty() {
    bail!(
      "no fixtures found under {} (expected <fixture>/index.html)",
      fixture_root.display()
    );
  }

  Ok(fixtures)
}

fn select_fixtures(
  mut fixtures: Vec<Fixture>,
  stems: Option<&[String]>,
  shard: Option<(usize, usize)>,
) -> Result<Vec<Fixture>> {
  if let Some(stems) = stems {
    let mut normalized = stems
      .iter()
      .map(|s| s.trim())
      .filter(|s| !s.is_empty())
      .collect::<Vec<_>>();
    normalized.sort();
    normalized.dedup();

    let want: HashSet<&str> = normalized.iter().copied().collect();
    let mut found = HashSet::<String>::new();
    fixtures.retain(|fixture| {
      if want.contains(fixture.stem.as_str()) {
        found.insert(fixture.stem.clone());
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
    fixtures = fixtures
      .into_iter()
      .enumerate()
      .filter(|(i, _)| i % total == index)
      .map(|(_, fixture)| fixture)
      .collect();
  }

  if fixtures.is_empty() {
    bail!("no fixtures selected");
  }

  Ok(fixtures)
}

fn resolve_chrome_binary(args: &ChromeBaselineFixturesArgs) -> Result<PathBuf> {
  if let Some(chrome) = &args.chrome {
    return resolve_program_path(chrome)
      .with_context(|| format!("invalid --chrome {}", chrome.display()));
  }

  if let Ok(value) = std::env::var("CHROME_BIN") {
    let trimmed = value.trim();
    if !trimmed.is_empty() {
      return resolve_program_path(Path::new(trimmed))
        .with_context(|| format!("invalid CHROME_BIN={trimmed}"));
    }
  }

  const CANDIDATES: &[&str] = &[
    "google-chrome-stable",
    "google-chrome",
    "chromium",
    "chromium-browser",
    "chrome",
  ];

  if let Some(dir) = &args.chrome_dir {
    let dir = absolutize_path(&crate::repo_root(), dir);
    for name in CANDIDATES {
      let candidate = dir.join(name);
      if candidate.is_file() {
        return Ok(candidate);
      }
    }

    bail!(
      "No Chrome/Chromium binary found in {} (looked for {}).\n\
       Pass --chrome /path/to/chrome, set CHROME_BIN, or place a stub `chrome` binary in that directory.",
      dir.display(),
      CANDIDATES.join(", ")
    );
  }

  for name in CANDIDATES {
    if let Some(path) = find_in_path(name) {
      return Ok(path);
    }
  }

  bail!(
    "No Chrome/Chromium binary found.\n\
     Install one (e.g. google-chrome or chromium), pass --chrome /path/to/chrome, or set CHROME_BIN."
  );
}

fn resolve_program_path(program: &Path) -> Result<PathBuf> {
  let has_separator = program.components().count() > 1;
  if has_separator || program.is_absolute() {
    if program.is_file() {
      return Ok(program.to_path_buf());
    }
    bail!("not found: {}", program.display());
  }

  let name = program
    .to_str()
    .ok_or_else(|| anyhow!("chrome program name is not valid UTF-8"))?;
  find_in_path(name).ok_or_else(|| anyhow!("not found in PATH: {}", program.display()))
}

fn find_in_path(program: &str) -> Option<PathBuf> {
  let path_var = std::env::var_os("PATH")?;
  for dir in std::env::split_paths(&path_var) {
    let candidate = dir.join(program);
    if candidate.is_file() {
      return Some(candidate);
    }
  }
  None
}

fn is_snap_chromium(chrome: &Path) -> bool {
  const SNAP_PREFIX: &str = "/snap/bin/chromium";
  if chrome.to_string_lossy().starts_with(SNAP_PREFIX) {
    return true;
  }

  // Some installations put a symlink/wrapper elsewhere (e.g. /usr/bin/chromium) that points into
  // the snap. If the canonicalized path resolves to the snap binary, treat it as snap Chromium.
  if let Ok(canon) = fs::canonicalize(chrome) {
    if canon.to_string_lossy().starts_with(SNAP_PREFIX) {
      return true;
    }
  }

  // Fall back to sniffing wrapper scripts. Avoid reading the full Chrome binary by only sampling
  // a small prefix.
  let mut file = match File::open(chrome) {
    Ok(f) => f,
    Err(_) => return false,
  };
  let mut buf = [0u8; 4096];
  let n = match file.read(&mut buf) {
    Ok(n) => n,
    Err(_) => return false,
  };
  let haystack = &buf[..n];
  for needle in [
    b"/snap/bin/chromium".as_slice(),
    b"snap run chromium".as_slice(),
    b"snap run chromium-browser".as_slice(),
  ] {
    if needle.len() <= haystack.len()
      && haystack
        .windows(needle.len())
        .any(|window| window == needle)
    {
      return true;
    }
  }
  false
}

fn create_temp_root(chrome: &Path) -> Result<TempDir> {
  // Snap-packaged Chromium is sandboxed from writing to arbitrary repo paths. Use a temp directory
  // under the snap's shared location when available so screenshot output is readable.
  let snap_common = if is_snap_chromium(chrome) {
    if let Ok(home) = std::env::var("HOME") {
      Some(PathBuf::from(home).join("snap/chromium/common"))
    } else {
      None
    }
  } else {
    None
  };

  if let Some(dir) = snap_common {
    let _ = fs::create_dir_all(&dir);
    if dir.is_dir() {
      return tempfile::Builder::new()
        .prefix("fastrender-chrome-fixtures.")
        .tempdir_in(&dir)
        .context("create snap temp dir for chrome fixtures");
    }
  }

  tempfile::Builder::new()
    .prefix("fastrender-chrome-fixtures.")
    .tempdir()
    .context("create temp dir for chrome fixtures")
}

fn chrome_version(chrome: &Path) -> Result<String> {
  let output = Command::new(chrome)
    .arg("--version")
    .output()
    .with_context(|| format!("run {} --version", chrome.display()))?;
  if !output.status.success() {
    bail!(
      "{} --version exited with {}",
      chrome.display(),
      output.status
    );
  }

  let stdout = String::from_utf8_lossy(&output.stdout);
  let stderr = String::from_utf8_lossy(&output.stderr);
  let version = stdout.trim();
  if !version.is_empty() {
    return Ok(version.to_string());
  }

  let version = stderr.trim();
  if !version.is_empty() {
    return Ok(version.to_string());
  }

  bail!("{} --version produced no output", chrome.display());
}

fn run_chrome_screenshot(
  chrome: &Path,
  url: &str,
  profile_dir: &Path,
  viewport: (u32, u32),
  dpr: f32,
  screenshot_path: &Path,
  log_path: &Path,
  timeout: Option<Duration>,
) -> Result<HeadlessMode> {
  let mut args = build_chrome_args(
    HeadlessMode::New,
    profile_dir,
    viewport,
    dpr,
    screenshot_path,
  )?;

  let mut last_status = run_chrome_with_timeout(chrome, &args, url, log_path, timeout, false)?;
  if last_status.success() && screenshot_path.is_file() {
    return Ok(HeadlessMode::New);
  }

  // `--headless=new` isn't supported on older Chrome releases. Fall back to legacy headless mode if
  // the log suggests it's unsupported.
  let log = fs::read_to_string(log_path).unwrap_or_default();
  let log_lower = log.to_ascii_lowercase();
  let headless_new_unsupported = log_lower.contains("--headless=new")
    && (log_lower.contains("unknown flag")
      || log_lower.contains("unrecognized option")
      || log_lower.contains("unknown option"));
  if headless_new_unsupported {
    args = build_chrome_args(
      HeadlessMode::Legacy,
      profile_dir,
      viewport,
      dpr,
      screenshot_path,
    )?;
    if screenshot_path.exists() {
      let _ = fs::remove_file(screenshot_path);
    }
    let mut file = OpenOptions::new()
      .create(true)
      .append(true)
      .open(log_path)
      .with_context(|| format!("open log file {}", log_path.display()))?;
    writeln!(file, "\n\n# Retrying with --headless\n").ok();
    last_status = run_chrome_with_timeout(chrome, &args, url, log_path, timeout, true)?;
    if last_status.success() && screenshot_path.is_file() {
      return Ok(HeadlessMode::Legacy);
    }
  }

  if !last_status.success() {
    bail!(
      "chrome exited with {}; see {}",
      last_status,
      log_path.display()
    );
  }

  bail!(
    "chrome did not produce a screenshot; see {}",
    log_path.display()
  );
}

fn build_chrome_args(
  headless: HeadlessMode,
  profile_dir: &Path,
  viewport: (u32, u32),
  dpr: f32,
  screenshot_path: &Path,
) -> Result<Vec<String>> {
  let mut args = build_chrome_common_args(headless, profile_dir, viewport, dpr)?;
  args.push(format!("--screenshot={}", screenshot_path.display()));
  Ok(args)
}

fn build_chrome_common_args(
  headless: HeadlessMode,
  profile_dir: &Path,
  viewport: (u32, u32),
  dpr: f32,
) -> Result<Vec<String>> {
  let headless_flag = match headless {
    HeadlessMode::New => "--headless=new",
    HeadlessMode::Legacy => "--headless",
  };

  let viewport_arg = format!("--window-size={},{}", viewport.0, viewport.1);
  let dpr_arg = format!("--force-device-scale-factor={}", dpr);
  let profile_arg = format!("--user-data-dir={}", profile_dir.display());

  Ok(vec![
    headless_flag.to_string(),
    "--disable-gpu".to_string(),
    "--no-sandbox".to_string(),
    "--disable-dev-shm-usage".to_string(),
    "--hide-scrollbars".to_string(),
    viewport_arg,
    dpr_arg,
    // Keep behaviour consistent with scripts/chrome_baseline.sh when loading local fixtures.
    "--disable-web-security".to_string(),
    "--allow-file-access-from-files".to_string(),
    // Keep renders deterministic/offline when fixture HTML accidentally references http(s).
    "--disable-background-networking".to_string(),
    "--dns-prefetch-disable".to_string(),
    "--no-first-run".to_string(),
    "--no-default-browser-check".to_string(),
    "--disable-component-update".to_string(),
    "--disable-default-apps".to_string(),
    "--disable-sync".to_string(),
    "--host-resolver-rules=MAP * ~NOTFOUND, EXCLUDE localhost".to_string(),
    profile_arg,
  ])
}

fn run_chrome_with_timeout(
  chrome: &Path,
  args: &[String],
  url: &str,
  log_path: &Path,
  timeout: Option<Duration>,
  append: bool,
) -> Result<ExitStatus> {
  if let Some(parent) = log_path.parent() {
    fs::create_dir_all(parent)
      .with_context(|| format!("create log directory {}", parent.display()))?;
  }

  let mut options = OpenOptions::new();
  options.create(true).write(true);
  if append {
    options.append(true);
  } else {
    options.truncate(true);
  }
  let mut log_file = options
    .open(log_path)
    .with_context(|| format!("open log file {}", log_path.display()))?;
  // Persist the command line used so logs are actionable even when Chrome itself produces no output.
  writeln!(log_file, "# chrome: {}", chrome.display()).ok();
  writeln!(log_file, "# url: {url}").ok();
  writeln!(log_file, "# args: {}", args.join(" ")).ok();
  writeln!(log_file).ok();
  let stderr = log_file
    .try_clone()
    .with_context(|| format!("clone log file handle for {}", log_path.display()))?;

  let mut cmd = Command::new(chrome);
  cmd.args(args).arg(url);
  cmd
    .stdout(Stdio::from(log_file))
    .stderr(Stdio::from(stderr));

  let mut child = cmd
    .spawn()
    .with_context(|| format!("failed to launch chrome at {}", chrome.display()))?;
  let pid = child.id();

  if let Some(timeout) = timeout {
    let start = Instant::now();
    loop {
      if let Some(status) = child.try_wait().context("poll chrome status")? {
        return Ok(status);
      }
      if start.elapsed() >= timeout {
        let _ = child.kill();
        let _ = child.wait();
        bail!(
          "chrome timed out after {}s (pid {}); see {}",
          timeout.as_secs(),
          pid,
          log_path.display()
        );
      }
      std::thread::sleep(Duration::from_millis(50));
    }
  }

  child.wait().context("wait for chrome")
}

fn file_url(path: &Path) -> Result<String> {
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

fn patch_html_bytes(data: &[u8], base_url: Option<&str>, disable_js: bool) -> Vec<u8> {
  let mut inserts = Vec::new();
  if let Some(base_url) = base_url {
    inserts.extend_from_slice(format!("<base href=\"{base_url}\">\n").as_bytes());
  }
  // Enforce a deterministic/offline page load: allow only file/data subresources.
  // If JS is enabled, allow inline/file scripts for experimentation; otherwise block scripts.
  let csp = if disable_js {
    "default-src file: data:; style-src file: data: 'unsafe-inline'; script-src 'none';"
  } else {
    "default-src file: data:; style-src file: data: 'unsafe-inline'; script-src file: data: 'unsafe-inline' 'unsafe-eval';"
  };
  inserts.extend_from_slice(
    format!("<meta http-equiv=\"Content-Security-Policy\" content=\"{csp}\">\n").as_bytes(),
  );

  if inserts.is_empty() {
    return data.to_vec();
  }

  let lower = data
    .iter()
    .map(|b| b.to_ascii_lowercase())
    .collect::<Vec<_>>();

  if let Some(out) = insert_after_open_tag(data, &lower, b"<head", &inserts) {
    return out;
  }

  let wrapped = [
    b"<head>\n".as_slice(),
    inserts.as_slice(),
    b"</head>\n".as_slice(),
  ]
  .concat();
  if let Some(out) = insert_after_open_tag(data, &lower, b"<html", &wrapped) {
    return out;
  }

  // Some fixtures omit `<html>`/`<head>` but still include a `<!doctype html>` declaration. Do not
  // inject anything before the doctype because that would flip the document into quirks mode in
  // Chrome and make the baseline useless. Instead, inject our tags immediately after the doctype.
  if let Some(out) = insert_after_doctype(data, &lower, &inserts) {
    return out;
  }

  // Fall back to prefixing the tags; the HTML parser will usually move them into an implicit head
  // element.
  [inserts, data.to_vec()].concat()
}

fn insert_after_open_tag(
  data: &[u8],
  lower: &[u8],
  tag: &[u8],
  insertion: &[u8],
) -> Option<Vec<u8>> {
  let mut search_start = 0usize;
  while let Some(pos) = lower[search_start..]
    .windows(tag.len())
    .position(|window| window == tag)
    .map(|rel| rel + search_start)
  {
    let after = lower.get(pos + tag.len());
    let boundary_ok = matches!(
      after,
      Some(b'>') | Some(b' ') | Some(b'\n') | Some(b'\r') | Some(b'\t') | Some(b'/')
    );
    if !boundary_ok {
      search_start = pos + tag.len();
      continue;
    }

    let end = lower[pos..].iter().position(|&b| b == b'>')? + pos + 1;
    let mut out = Vec::with_capacity(data.len() + insertion.len() + 1);
    out.extend_from_slice(&data[..end]);
    out.extend_from_slice(b"\n");
    out.extend_from_slice(insertion);
    out.extend_from_slice(&data[end..]);
    return Some(out);
  }
  None
}

fn insert_after_doctype(data: &[u8], lower: &[u8], insertion: &[u8]) -> Option<Vec<u8>> {
  const DOCTYPE: &[u8] = b"<!doctype";
  let mut search_start = 0usize;
  while let Some(pos) = lower[search_start..]
    .windows(DOCTYPE.len())
    .position(|window| window == DOCTYPE)
    .map(|rel| rel + search_start)
  {
    let after = lower.get(pos + DOCTYPE.len());
    let boundary_ok = matches!(
      after,
      Some(b'>') | Some(b' ') | Some(b'\n') | Some(b'\r') | Some(b'\t')
    );
    if !boundary_ok {
      search_start = pos + DOCTYPE.len();
      continue;
    }

    let end = lower[pos..].iter().position(|&b| b == b'>')? + pos + 1;
    let mut out = Vec::with_capacity(data.len() + insertion.len() + 1);
    out.extend_from_slice(&data[..end]);
    out.extend_from_slice(b"\n");
    out.extend_from_slice(insertion);
    out.extend_from_slice(&data[end..]);
    return Some(out);
  }
  None
}

fn absolutize_path(repo_root: &Path, path: &Path) -> PathBuf {
  if path.is_absolute() {
    path.to_path_buf()
  } else {
    repo_root.join(path)
  }
}

#[cfg(test)]
mod tests {
  use super::{
    build_fixture_metadata, is_snap_chromium, patch_html_bytes, ChromeBaselineFixturesArgs,
  };
  use sha2::{Digest, Sha256};
  use std::fs;
  use std::path::{Path, PathBuf};
  use tempfile::tempdir;

  #[test]
  fn patch_html_keeps_doctype_first_when_head_missing() {
    let input = b"<!doctype html>\n<meta charset=\"utf-8\">\n<body>Hello</body>\n";
    let output = patch_html_bytes(input, Some("file:///tmp/fixture/"), true);
    assert!(
      output.starts_with(b"<!doctype html>"),
      "doctype must remain the first token to avoid quirks mode"
    );

    let output_str = String::from_utf8_lossy(&output);
    assert!(
      output_str.contains("Content-Security-Policy"),
      "patched HTML should include CSP injection"
    );
    assert!(
      output_str.contains("<base href=\"file:///tmp/fixture/\">"),
      "patched HTML should include base href injection"
    );
  }

  #[test]
  fn snap_detection_matches_direct_path() {
    assert!(is_snap_chromium(Path::new("/snap/bin/chromium")));
  }

  #[test]
  fn snap_detection_matches_wrapper_script() {
    let temp = tempdir().expect("tempdir");
    let script = temp.path().join("chromium");
    fs::write(&script, "#!/bin/sh\nexec /snap/bin/chromium \"$@\"\n").expect("write wrapper");
    assert!(is_snap_chromium(&script));
  }

  #[test]
  fn snap_detection_matches_snap_run_wrapper_script() {
    let temp = tempdir().expect("tempdir");
    let script = temp.path().join("chromium-browser");
    fs::write(&script, "#!/bin/sh\nexec snap run chromium \"$@\"\n").expect("write wrapper");
    assert!(is_snap_chromium(&script));
  }

  #[test]
  fn fixture_metadata_records_input_sha256() {
    let temp = tempdir().expect("tempdir");
    let fixture_dir = temp.path().join("fixture");
    fs::create_dir_all(&fixture_dir).expect("create fixture dir");

    let html = b"<!doctype html><title>fixture</title>";
    fs::write(fixture_dir.join("index.html"), html).expect("write index.html");

    let fixture = super::Fixture {
      stem: "fixture".to_string(),
      dir: fixture_dir,
    };

    let args = ChromeBaselineFixturesArgs {
      fixture_dir: PathBuf::from("fixtures"),
      out_dir: PathBuf::from("out"),
      fixtures: None,
      fixtures_pos: Vec::new(),
      shard: None,
      chrome: None,
      chrome_dir: None,
      viewport: (1040, 1240),
      dpr: 1.0,
      timeout: 15,
      js: super::JsMode::Off,
    };

    let metadata = build_fixture_metadata(
      &fixture,
      &args,
      super::HeadlessMode::New,
      Some("Chromium 123.0.0.0"),
      12.0,
      html,
    )
    .expect("build fixture metadata");

    let digest = Sha256::digest(html);
    let expected = digest
      .iter()
      .map(|b| format!("{b:02x}"))
      .collect::<String>();
    assert_eq!(metadata.input_sha256, expected);

    let json = serde_json::to_string(&metadata).expect("serialize metadata");
    assert!(
      json.contains("\"input_sha256\""),
      "metadata JSON should include input_sha256; got: {json}"
    );
  }
}
