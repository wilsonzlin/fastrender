use anyhow::{anyhow, bail, Context, Result};
use clap::{Args, ValueEnum};
use std::collections::BTreeSet;
use std::fs::{self, File, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};
use tempfile::TempDir;
use url::Url;
use walkdir::WalkDir;

const DEFAULT_FIXTURES_ROOT: &str = "tests/pages/fixtures";
const DEFAULT_OUT_DIR: &str = "target/chrome_fixture_renders";
const DEFAULT_VIEWPORT: &str = "1200x800";
const DEFAULT_DPR: f32 = 1.0;
const DEFAULT_TIMEOUT_SECS: u64 = 15;

const CSP_DISABLE_SCRIPT_META: &[u8] =
  b"<meta http-equiv=\"Content-Security-Policy\" content=\"script-src 'none';\">\n";

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum JsMode {
  On,
  Off,
}

#[derive(Args, Debug)]
pub struct ChromeBaselineFixturesArgs {
  /// Root directory containing page fixtures (directories containing index.html).
  #[arg(long, default_value = DEFAULT_FIXTURES_ROOT)]
  pub fixtures_root: PathBuf,

  /// Directory to write PNG screenshots + Chrome logs into.
  #[arg(long, default_value = DEFAULT_OUT_DIR)]
  pub out_dir: PathBuf,

  /// Viewport size as WxH (e.g. 1200x800).
  #[arg(long, value_parser = crate::parse_viewport, default_value = DEFAULT_VIEWPORT)]
  pub viewport: (u32, u32),

  /// Device pixel ratio used by Chrome (for media queries/srcset).
  #[arg(long, default_value_t = DEFAULT_DPR)]
  pub dpr: f32,

  /// Per-fixture hard timeout (seconds).
  #[arg(long, default_value_t = DEFAULT_TIMEOUT_SECS)]
  pub timeout: u64,

  /// Chrome/Chromium binary path.
  ///
  /// When omitted, we auto-detect `google-chrome-stable|google-chrome|chromium|chromium-browser`
  /// (in that order) using PATH probing.
  #[arg(long)]
  pub chrome: Option<PathBuf>,

  /// Enable or disable JavaScript (default: off).
  #[arg(long, value_enum, default_value_t = JsMode::Off)]
  pub js: JsMode,

  /// Render only these fixtures (comma-separated).
  #[arg(long, value_delimiter = ',', conflicts_with = "fixtures")]
  pub only: Option<Vec<String>>,

  /// Fixture names to render (positional alternative to --only).
  #[arg(value_name = "FIXTURE", num_args = 0..)]
  pub fixtures: Vec<String>,
}

#[derive(Debug, Clone)]
struct FixtureSpec {
  name: String,
  dir: PathBuf,
}

#[derive(Debug)]
struct ChromeRunOutcome {
  status: ExitStatus,
  timed_out: bool,
}

pub fn run_chrome_baseline_fixtures(args: ChromeBaselineFixturesArgs) -> Result<()> {
  if args.timeout == 0 {
    bail!("--timeout must be greater than zero");
  }
  if !args.dpr.is_finite() || args.dpr <= 0.0 {
    bail!("--dpr must be a finite number greater than zero");
  }

  let chrome_bin = resolve_chrome_binary(args.chrome.as_deref())?;
  let temp_root = create_temp_root(&chrome_bin)?;

  let fixtures_root = &args.fixtures_root;
  if !fixtures_root.is_dir() {
    bail!("fixtures root does not exist: {}", fixtures_root.display());
  }

  let out_dir = &args.out_dir;
  fs::create_dir_all(out_dir)
    .with_context(|| format!("failed to create output directory {}", out_dir.display()))?;

  let requested = fixture_filter(&args);
  let fixtures = discover_fixtures(fixtures_root, requested.as_deref())?;
  if fixtures.is_empty() {
    bail!(
      "no fixtures found under {} (expected subdirectories containing index.html)",
      fixtures_root.display()
    );
  }

  stage_shared_assets(fixtures_root, temp_root.path())?;

  println!("Chrome: {}", chrome_bin.display());
  println!("Input:  {}", fixtures_root.display());
  println!("Output: {}", out_dir.display());
  println!(
    "Viewport: {}x{}  DPR: {}  JS: {}  Timeout: {}s",
    args.viewport.0,
    args.viewport.1,
    args.dpr,
    match args.js {
      JsMode::On => "on",
      JsMode::Off => "off",
    },
    args.timeout
  );
  println!();

  let mut ok = 0usize;
  let mut failed = 0usize;
  let total = fixtures.len();

  for fixture in fixtures {
    let log_path = out_dir.join(format!("{}.chrome.log", fixture.name));
    File::create(&log_path)
      .with_context(|| format!("failed to create log file {}", log_path.display()))?;

    match render_fixture(
      &fixture,
      &args,
      temp_root.path(),
      out_dir,
      &chrome_bin,
      &log_path,
    ) {
      Ok(true) => {
        ok += 1;
        println!("OK  {}", fixture.name);
      }
      Ok(false) => {
        failed += 1;
        eprintln!("FAIL {} (see {})", fixture.name, log_path.display());
      }
      Err(err) => {
        failed += 1;
        append_log(
          &log_path,
          &format!("internal error while rendering fixture: {err:#}\n"),
        )
        .ok();
        eprintln!(
          "FAIL {} (internal error; see {})",
          fixture.name,
          log_path.display()
        );
      }
    }
  }

  println!();
  println!("Done: {ok} ok, {failed} failed (out of {total})");
  println!("PNGs:  {}/*.png", out_dir.display());
  println!("Logs:  {}/*.chrome.log", out_dir.display());

  if failed > 0 {
    bail!("{failed} fixture(s) failed");
  }

  Ok(())
}

fn fixture_filter(args: &ChromeBaselineFixturesArgs) -> Option<Vec<String>> {
  if let Some(only) = &args.only {
    return Some(only.clone());
  }
  if !args.fixtures.is_empty() {
    return Some(args.fixtures.clone());
  }
  None
}

fn discover_fixtures(root: &Path, only: Option<&[String]>) -> Result<Vec<FixtureSpec>> {
  let mut fixtures = Vec::new();
  let mut present = BTreeSet::new();

  for entry in
    fs::read_dir(root).with_context(|| format!("read fixtures dir {}", root.display()))?
  {
    let entry = entry.context("read fixtures directory entry")?;
    if !entry
      .file_type()
      .with_context(|| format!("stat {}", entry.path().display()))?
      .is_dir()
    {
      continue;
    }
    let name = entry.file_name().to_string_lossy().to_string();
    let dir = entry.path();
    if dir.join("index.html").is_file() {
      present.insert(name.clone());
      fixtures.push(FixtureSpec { name, dir });
    }
  }

  fixtures.sort_by(|a, b| a.name.cmp(&b.name));

  if let Some(only) = only {
    let requested: BTreeSet<String> = only.iter().map(|s| s.to_string()).collect();
    let missing: Vec<String> = requested
      .iter()
      .filter(|name| !present.contains(*name))
      .cloned()
      .collect();
    if !missing.is_empty() {
      bail!(
        "unknown fixture(s): {}",
        missing
          .iter()
          .map(|s| format!("\"{s}\""))
          .collect::<Vec<_>>()
          .join(", ")
      );
    }

    fixtures.retain(|fixture| requested.contains(&fixture.name));
  }

  Ok(fixtures)
}

fn stage_shared_assets(fixtures_root: &Path, temp_root: &Path) -> Result<()> {
  let src = fixtures_root.join("assets");
  if !src.is_dir() {
    return Ok(());
  }
  let dst = temp_root.join("assets");
  copy_dir_all(&src, &dst).with_context(|| {
    format!(
      "failed to stage shared assets from {} into {}",
      src.display(),
      dst.display()
    )
  })
}

fn render_fixture(
  fixture: &FixtureSpec,
  args: &ChromeBaselineFixturesArgs,
  temp_root: &Path,
  out_dir: &Path,
  chrome_bin: &Path,
  log_path: &Path,
) -> Result<bool> {
  let stage_dir = temp_root.join(&fixture.name);
  if stage_dir.exists() {
    fs::remove_dir_all(&stage_dir)
      .with_context(|| format!("failed to clear {}", stage_dir.display()))?;
  }

  if let Err(err) = copy_dir_all(&fixture.dir, &stage_dir) {
    append_log(
      log_path,
      &format!(
        "failed to stage fixture directory {}: {err:#}\n",
        fixture.dir.display()
      ),
    )?;
    return Ok(false);
  }

  let index_path = stage_dir.join("index.html");
  if matches!(args.js, JsMode::Off) {
    let data = match fs::read(&index_path) {
      Ok(bytes) => bytes,
      Err(err) => {
        append_log(
          log_path,
          &format!(
            "failed to read staged HTML {}: {err}\n",
            index_path.display()
          ),
        )?;
        return Ok(false);
      }
    };
    let patched = inject_disable_js_csp(&data);
    if let Err(err) = fs::write(&index_path, patched) {
      append_log(
        log_path,
        &format!(
          "failed to write patched HTML {}: {err}\n",
          index_path.display()
        ),
      )?;
      return Ok(false);
    }
  }

  let url = Url::from_file_path(&index_path)
    .map(|u| u.to_string())
    .map_err(|_| {
      anyhow!(
        "could not convert {} to a file:// URL",
        index_path.display()
      )
    })?;

  let screenshot_dir = temp_root.join("screenshots");
  fs::create_dir_all(&screenshot_dir)
    .with_context(|| format!("create {}", screenshot_dir.display()))?;
  let screenshot_tmp = screenshot_dir.join(format!("{}.png", fixture.name));
  let _ = fs::remove_file(&screenshot_tmp);

  let png_path = out_dir.join(format!("{}.png", fixture.name));
  let profile_dir = temp_root.join("profiles").join(&fixture.name);

  let outcome = match run_chrome_screenshot(
    chrome_bin,
    &url,
    &screenshot_tmp,
    &profile_dir,
    log_path,
    args.viewport,
    args.dpr,
    Duration::from_secs(args.timeout),
  ) {
    Ok(outcome) => outcome,
    Err(err) => {
      append_log(log_path, &format!("failed to launch Chrome: {err:#}\n"))?;
      return Ok(false);
    }
  };

  if outcome.timed_out {
    append_log(
      log_path,
      &format!("Chrome timed out after {} seconds\n", args.timeout),
    )?;
    return Ok(false);
  }

  if !outcome.status.success() {
    append_log(
      log_path,
      &format!("Chrome exited with status {}\n", outcome.status),
    )?;
    return Ok(false);
  }

  let metadata = match fs::metadata(&screenshot_tmp) {
    Ok(m) => m,
    Err(err) => {
      append_log(
        log_path,
        &format!(
          "Chrome succeeded but screenshot is missing at {}: {err}\n",
          screenshot_tmp.display()
        ),
      )?;
      return Ok(false);
    }
  };
  if metadata.len() == 0 {
    append_log(
      log_path,
      &format!(
        "Chrome succeeded but screenshot is empty at {}\n",
        screenshot_tmp.display()
      ),
    )?;
    return Ok(false);
  }

  if let Err(err) = fs::copy(&screenshot_tmp, &png_path) {
    append_log(
      log_path,
      &format!(
        "failed to copy screenshot into output dir ({} -> {}): {err}\n",
        screenshot_tmp.display(),
        png_path.display()
      ),
    )?;
    return Ok(false);
  }

  Ok(true)
}

fn run_chrome_screenshot(
  chrome_bin: &Path,
  url: &str,
  screenshot_path: &Path,
  profile_dir: &Path,
  log_path: &Path,
  viewport: (u32, u32),
  dpr: f32,
  timeout: Duration,
) -> Result<ChromeRunOutcome> {
  if let Some(parent) = profile_dir.parent() {
    fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
  }
  fs::create_dir_all(profile_dir).with_context(|| format!("create {}", profile_dir.display()))?;

  let window_size = format!("--window-size={},{}", viewport.0, viewport.1);
  let scale_factor = format!("--force-device-scale-factor={dpr}");
  let user_data_dir = format!("--user-data-dir={}", profile_dir.display());
  let screenshot_arg = format!("--screenshot={}", screenshot_path.display());

  let args = vec![
    "--headless=new".to_string(),
    "--no-sandbox".to_string(),
    "--disable-dev-shm-usage".to_string(),
    "--disable-gpu".to_string(),
    "--hide-scrollbars".to_string(),
    window_size,
    scale_factor,
    "--disable-web-security".to_string(),
    "--allow-file-access-from-files".to_string(),
    user_data_dir,
    screenshot_arg,
    url.to_string(),
  ];

  let mut log_file =
    File::create(log_path).with_context(|| format!("failed to create {}", log_path.display()))?;
  writeln!(log_file, "Chrome: {}", chrome_bin.display()).ok();
  writeln!(log_file, "URL: {url}").ok();
  writeln!(
    log_file,
    "Args: {}",
    args
      .iter()
      .map(|s| {
        if s.contains(' ') {
          format!("{s:?}")
        } else {
          s.clone()
        }
      })
      .collect::<Vec<_>>()
      .join(" ")
  )
  .ok();
  writeln!(log_file).ok();

  let mut cmd = Command::new(chrome_bin);
  cmd.args(&args);

  cmd.stdout(Stdio::from(
    log_file
      .try_clone()
      .context("failed to clone chrome log file handle")?,
  ));
  cmd.stderr(Stdio::from(log_file));

  let mut child = cmd
    .spawn()
    .with_context(|| format!("failed to run {}", chrome_bin.display()))?;

  let start = Instant::now();
  loop {
    if let Some(status) = child
      .try_wait()
      .context("failed to poll chrome child process")?
    {
      return Ok(ChromeRunOutcome {
        status,
        timed_out: false,
      });
    }

    if start.elapsed() > timeout {
      child.kill().ok();
      let status = child
        .wait()
        .context("failed to wait for chrome after timeout")?;
      return Ok(ChromeRunOutcome {
        status,
        timed_out: true,
      });
    }

    std::thread::sleep(Duration::from_millis(50));
  }
}

fn resolve_chrome_binary(explicit: Option<&Path>) -> Result<PathBuf> {
  if let Some(chrome) = explicit {
    return resolve_explicit_chrome(chrome);
  }

  for candidate in [
    "google-chrome-stable",
    "google-chrome",
    "chromium",
    "chromium-browser",
  ] {
    if let Some(path) = find_in_path(candidate) {
      return Ok(path);
    }
  }

  bail!(
    "No Chrome/Chromium binary found.\nInstall one (e.g. google-chrome or chromium) or pass --chrome /path/to/chrome."
  );
}

fn resolve_explicit_chrome(chrome: &Path) -> Result<PathBuf> {
  if chrome.exists() {
    return Ok(
      chrome
        .canonicalize()
        .unwrap_or_else(|_| chrome.to_path_buf()),
    );
  }

  let raw = chrome.to_string_lossy();
  let looks_like_program =
    !raw.contains(std::path::MAIN_SEPARATOR) && !raw.contains('/') && !raw.contains('\\');

  if looks_like_program {
    if let Some(path) = find_in_path(&raw) {
      return Ok(path);
    }
  }

  bail!("Chrome binary not found: {}", chrome.display());
}

fn find_in_path(program: &str) -> Option<PathBuf> {
  let path_var = std::env::var_os("PATH")?;
  for dir in std::env::split_paths(&path_var) {
    let candidate = dir.join(program);
    if candidate.is_file() {
      return Some(candidate);
    }
    #[cfg(windows)]
    {
      let candidate = dir.join(format!("{program}.exe"));
      if candidate.is_file() {
        return Some(candidate);
      }
    }
  }
  None
}

fn create_temp_root(chrome_bin: &Path) -> Result<TempDir> {
  let chrome = chrome_bin.to_string_lossy();
  if chrome.starts_with("/snap/bin/chromium") {
    if let Ok(home) = std::env::var("HOME") {
      let snap_common = Path::new(&home).join("snap/chromium/common");
      let _ = fs::create_dir_all(&snap_common);
      if snap_common.is_dir() {
        return tempfile::Builder::new()
          .prefix("fastrender-chrome-baseline-fixtures.")
          .tempdir_in(&snap_common)
          .context("create temp directory under snap chromium common dir");
      }
    }
  }

  tempfile::Builder::new()
    .prefix("fastrender-chrome-baseline-fixtures.")
    .tempdir()
    .context("create temp directory for chrome baseline fixtures")
}

fn copy_dir_all(src: &Path, dst: &Path) -> Result<()> {
  fs::create_dir_all(dst).with_context(|| format!("create {}", dst.display()))?;

  for entry in WalkDir::new(src) {
    let entry = entry.with_context(|| format!("walk {}", src.display()))?;
    let rel = entry
      .path()
      .strip_prefix(src)
      .with_context(|| format!("strip prefix {}", src.display()))?;
    if rel.as_os_str().is_empty() {
      continue;
    }
    let target = dst.join(rel);
    let ty = entry.file_type();
    if ty.is_dir() {
      fs::create_dir_all(&target).with_context(|| format!("create {}", target.display()))?;
    } else if ty.is_file() {
      if let Some(parent) = target.parent() {
        fs::create_dir_all(parent)
          .with_context(|| format!("create parent {}", parent.display()))?;
      }
      fs::copy(entry.path(), &target)
        .with_context(|| format!("copy {} -> {}", entry.path().display(), target.display()))?;
    }
  }

  Ok(())
}

fn append_log(path: &Path, msg: &str) -> Result<()> {
  let mut file = OpenOptions::new()
    .create(true)
    .append(true)
    .open(path)
    .with_context(|| format!("open {}", path.display()))?;
  file
    .write_all(msg.as_bytes())
    .with_context(|| format!("write {}", path.display()))?;
  Ok(())
}

fn inject_disable_js_csp(data: &[u8]) -> Vec<u8> {
  let lower: Vec<u8> = data.iter().map(|b| b.to_ascii_lowercase()).collect();
  if let Some(out) = insert_after_tag(data, &lower, b"<head", CSP_DISABLE_SCRIPT_META) {
    return out;
  }

  let wrapped = [&b"<head>\n"[..], CSP_DISABLE_SCRIPT_META, &b"</head>\n"[..]].concat();
  if let Some(out) = insert_after_tag(data, &lower, b"<html", &wrapped) {
    return out;
  }

  [
    &b"<head>\n"[..],
    CSP_DISABLE_SCRIPT_META,
    &b"</head>\n"[..],
    data,
  ]
  .concat()
}

fn insert_after_tag(data: &[u8], lower: &[u8], tag: &[u8], insertion: &[u8]) -> Option<Vec<u8>> {
  let idx = find_subsequence(lower, tag)?;
  let end = lower[idx..].iter().position(|&b| b == b'>')? + idx + 1;
  let mut out = Vec::with_capacity(data.len() + 1 + insertion.len());
  out.extend_from_slice(&data[..end]);
  out.extend_from_slice(b"\n");
  out.extend_from_slice(insertion);
  out.extend_from_slice(&data[end..]);
  Some(out)
}

fn find_subsequence(haystack: &[u8], needle: &[u8]) -> Option<usize> {
  if needle.is_empty() || haystack.len() < needle.len() {
    return None;
  }
  haystack
    .windows(needle.len())
    .position(|window| window == needle)
}

#[cfg(test)]
mod tests {
  use super::*;
  use clap::Parser;
  use std::fs;
  use tempfile::tempdir;

  #[test]
  fn injects_csp_when_head_present() {
    let html = b"<!doctype html><html><head><title>x</title></head><body></body></html>";
    let out = inject_disable_js_csp(html);
    let out = String::from_utf8(out).expect("utf8");
    assert!(
      out.contains("<head>\n<meta http-equiv=\"Content-Security-Policy\" content=\"script-src 'none';\">\n<title>x</title>"),
      "unexpected injection output: {out}"
    );
  }

  #[test]
  fn injects_csp_when_head_missing() {
    let html = b"<!doctype html><html><body>hi</body></html>";
    let out = inject_disable_js_csp(html);
    let out = String::from_utf8(out).expect("utf8");
    assert!(
      out.contains("<html>\n<head>\n<meta http-equiv=\"Content-Security-Policy\" content=\"script-src 'none';\">\n</head>\n<body>hi</body>"),
      "unexpected injection output: {out}"
    );
  }

  #[test]
  fn injects_csp_when_html_missing() {
    let html = b"<body>hi</body>";
    let out = inject_disable_js_csp(html);
    let out = String::from_utf8(out).expect("utf8");
    assert!(
      out.starts_with("<head>\n<meta http-equiv=\"Content-Security-Policy\" content=\"script-src 'none';\">\n</head>\n"),
      "unexpected injection output: {out}"
    );
  }

  #[test]
  fn cli_parses_defaults() {
    let cli = crate::Cli::try_parse_from(["xtask", "chrome-baseline-fixtures"]).expect("parse cli");
    match cli.command {
      crate::Commands::ChromeBaselineFixtures(args) => {
        assert_eq!(args.fixtures_root, PathBuf::from(DEFAULT_FIXTURES_ROOT));
        assert_eq!(args.out_dir, PathBuf::from(DEFAULT_OUT_DIR));
        assert_eq!(args.viewport, (1200, 800));
        assert_eq!(args.dpr, 1.0);
        assert_eq!(args.timeout, 15);
        assert_eq!(args.js, JsMode::Off);
        assert!(args.only.is_none());
        assert!(args.fixtures.is_empty());
      }
      _ => panic!("expected chrome-baseline-fixtures subcommand"),
    }
  }

  #[test]
  fn cli_parses_fixture_filter_positional() {
    let cli = crate::Cli::try_parse_from(["xtask", "chrome-baseline-fixtures", "a", "b"])
      .expect("parse cli");
    match cli.command {
      crate::Commands::ChromeBaselineFixtures(args) => {
        assert_eq!(args.fixtures, vec!["a".to_string(), "b".to_string()]);
        assert!(args.only.is_none());
      }
      _ => panic!("expected chrome-baseline-fixtures subcommand"),
    }
  }

  #[test]
  fn resolves_explicit_chrome_path_without_path_probe() -> Result<()> {
    let dir = tempdir()?;
    let chrome = dir.path().join("chrome");
    fs::write(&chrome, b"dummy")?;
    let resolved = resolve_chrome_binary(Some(&chrome))?;
    assert_eq!(resolved, chrome.canonicalize()?);
    Ok(())
  }
}
