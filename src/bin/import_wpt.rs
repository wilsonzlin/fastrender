//! Offline importer for a subset of Web Platform Tests.
//!
//! This tool copies HTML/CSS/assets from a local WPT checkout into
//! `tests/wpt/tests`, rewriting absolute URLs so everything is self-contained.
//! It never touches the network.
use clap::Parser;
use glob::glob;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::io;
use std::path::{Component, Path, PathBuf};
use thiserror::Error;

type Result<T> = std::result::Result<T, ImportError>;

fn main() {
  if let Err(err) = run() {
    eprintln!("import_wpt: {err}");
    std::process::exit(1);
  }
}

fn run() -> Result<()> {
  let args = Args::parse();
  let config = ImportConfig::from_args(args)?;
  let summary = run_import(config.clone())?;

  if config.dry_run {
    println!("Dry run: no files were written");
  }

  println!(
    "Imported {} file(s), skipped {}, overwritten {}",
    summary.copied.len(),
    summary.skipped.len(),
    summary.overwritten.len()
  );

  if let Some(path) = config.manifest_path {
    if summary.manifest_written {
      println!("Updated manifest at {}", path.display());
    } else {
      println!("Manifest unchanged at {}", path.display());
    }
  }

  Ok(())
}

/// Import tests from a local WPT checkout.
#[derive(Parser, Debug)]
#[command(name = "import_wpt")]
struct Args {
  /// Path to a local WPT checkout
  #[arg(long)]
  wpt_root: PathBuf,

  /// Test suite glob(s) relative to the WPT root (e.g. css/css-text/*)
  #[arg(long, required = true)]
  suite: Vec<String>,

  /// Output directory for imported tests (defaults to tests/wpt/tests)
  #[arg(long, default_value = "tests/wpt/tests")]
  out: PathBuf,

  /// Optional manifest path; defaults to tests/wpt/manifest.toml
  #[arg(long)]
  manifest: Option<PathBuf>,

  /// Skip manifest updates entirely
  #[arg(long)]
  no_manifest: bool,

  /// Preview actions without writing files
  #[arg(long)]
  dry_run: bool,

  /// Allow overwriting existing files/manifest entries
  #[arg(long)]
  overwrite: bool,
}

#[derive(Clone, Debug)]
struct ImportConfig {
  wpt_root: PathBuf,
  suites: Vec<String>,
  out_dir: PathBuf,
  manifest_path: Option<PathBuf>,
  dry_run: bool,
  overwrite: bool,
}

impl ImportConfig {
  fn from_args(args: Args) -> Result<Self> {
    if args.no_manifest && args.manifest.is_some() {
      return Err(ImportError::Message(
        "cannot set --manifest and --no-manifest together".to_string(),
      ));
    }

    let wpt_root = canonical_existing_dir(&args.wpt_root).map_err(|_| {
      ImportError::Message(format!("WPT root not found: {}", args.wpt_root.display()))
    })?;

    let out_dir = absolutize(&args.out)?;
    let manifest_path = if args.no_manifest {
      None
    } else {
      Some(absolutize(
        &args
          .manifest
          .unwrap_or_else(|| PathBuf::from("tests/wpt/manifest.toml")),
      )?)
    };

    Ok(Self {
      wpt_root,
      suites: args.suite,
      out_dir,
      manifest_path,
      dry_run: args.dry_run,
      overwrite: args.overwrite,
    })
  }
}

#[derive(Debug, Default)]
struct ImportSummary {
  copied: Vec<PathBuf>,
  skipped: Vec<PathBuf>,
  overwritten: Vec<PathBuf>,
  manifest_written: bool,
}

#[derive(Debug, Clone)]
struct Reference {
  original: String,
  new_value: String,
  source_path: PathBuf,
  dest_path: PathBuf,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
struct ManifestFile {
  #[serde(default)]
  tests: Vec<ManifestEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
struct ManifestEntry {
  id: String,
  path: String,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  reference: Option<String>,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  test_type: Option<String>,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  expected: Option<String>,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  viewport: Option<ManifestViewport>,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  timeout_ms: Option<u64>,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  disabled: Option<String>,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  dpr: Option<f32>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
struct ManifestViewport {
  width: u32,
  height: u32,
}

#[derive(Debug, Error)]
enum ImportError {
  #[error("{0}")]
  Message(String),
  #[error("no tests matched suite {0}")]
  NoMatches(String),
  #[error("failed to read {0}: {1}")]
  Io(PathBuf, #[source] io::Error),
  #[error("destination exists with different contents: {0}")]
  WouldOverwrite(PathBuf),
  #[error("referenced file is missing: {0}")]
  MissingReference(PathBuf),
  #[error("path escapes configured root: {0}")]
  OutsideRoot(PathBuf),
  #[error("invalid UTF-8 while reading {0}")]
  InvalidUtf8(PathBuf),
  #[error("manifest has conflicting entry for {0}; use --overwrite to replace")]
  ManifestConflict(String),
  #[error("glob error: {0}")]
  Glob(#[from] glob::PatternError),
  #[error("glob iteration error: {0}")]
  Globwalk(#[from] glob::GlobError),
  #[error("toml error: {0}")]
  Toml(#[from] toml::de::Error),
  #[error("toml serialization error: {0}")]
  TomlSer(#[from] toml::ser::Error),
}

fn run_import(config: ImportConfig) -> Result<ImportSummary> {
  let tests = discover_tests(&config)?;
  let mut summary = ImportSummary::default();
  let mut manifest_entries = Vec::new();

  for test in tests {
    manifest_entries.push(import_test(&config, &test, &mut summary)?);
  }

  if let Some(manifest_path) = &config.manifest_path {
    if !manifest_entries.is_empty() {
      summary.manifest_written = update_manifest(manifest_path, &manifest_entries, &config)?;
    }
  }

  Ok(summary)
}

fn discover_tests(config: &ImportConfig) -> Result<Vec<PathBuf>> {
  let mut results = Vec::new();
  let mut seen = HashSet::new();

  for suite in &config.suites {
    let pattern = config.wpt_root.join(suite);
    if has_glob_pattern(suite) {
      let pattern_str = pattern.to_string_lossy().to_string();
      for entry in glob(&pattern_str)? {
        let path = entry?;
        if path.is_file() && is_root_test_file(&path) {
          if seen.insert(path.clone()) {
            results.push(path);
          }
        }
      }
    } else if pattern.is_dir() {
      collect_html_files(&pattern, &mut results, &mut seen)?;
    } else if pattern.is_file() {
      if is_root_test_file(&pattern) && seen.insert(pattern.clone()) {
        results.push(pattern);
      }
    } else {
      return Err(ImportError::NoMatches(suite.clone()));
    }
  }

  if results.is_empty() {
    return Err(ImportError::NoMatches(config.suites.join(", ").to_string()));
  }

  results.sort();
  Ok(results)
}

fn collect_html_files(
  dir: &Path,
  acc: &mut Vec<PathBuf>,
  seen: &mut HashSet<PathBuf>,
) -> Result<()> {
  let entries = fs::read_dir(dir).map_err(|e| ImportError::Io(dir.to_path_buf(), e))?;
  for entry in entries.flatten() {
    let path = entry.path();
    if path.is_dir() {
      collect_html_files(&path, acc, seen)?;
    } else if path.is_file() && is_root_test_file(&path) && seen.insert(path.clone()) {
      acc.push(path);
    }
  }
  Ok(())
}

fn import_test(
  config: &ImportConfig,
  src_path: &Path,
  summary: &mut ImportSummary,
) -> Result<ManifestEntry> {
  ensure_within_root(src_path, &config.wpt_root)?;
  let relative = src_path
    .strip_prefix(&config.wpt_root)
    .map_err(|_| ImportError::OutsideRoot(src_path.to_path_buf()))?;
  let dest_path = normalize_path(config.out_dir.join(relative));

  let mut processed: HashSet<PathBuf> = HashSet::new();
  let mut queue: VecDeque<(PathBuf, PathBuf)> = VecDeque::new();
  let mut reftest_reference: Option<Reference> = None;

  processed.insert(dest_path.clone());
  let outcome = rewrite_and_copy(
    config,
    src_path,
    &dest_path,
    true,
    &mut reftest_reference,
    summary,
  )?;
  for reference in outcome {
    queue.push_back((reference.source_path, reference.dest_path));
  }

  while let Some((src, dest)) = queue.pop_front() {
    if processed.contains(&dest) {
      continue;
    }
    processed.insert(dest.clone());
    let references = rewrite_and_copy(config, &src, &dest, false, &mut None, summary)?;
    for reference in references {
      if !processed.contains(&reference.dest_path) {
        queue.push_back((reference.source_path, reference.dest_path));
      }
    }
  }

  let id = manifest_id_from_relative(relative);
  let manifest_entry = ManifestEntry {
    id,
    path: normalize_to_forward_slashes(relative),
    reference: reftest_reference.as_ref().map(|r| {
      normalize_to_forward_slashes(
        r.dest_path
          .strip_prefix(&config.out_dir)
          .unwrap_or(&r.dest_path),
      )
    }),
    test_type: Some(
      reftest_reference
        .as_ref()
        .map(|_| "reftest")
        .unwrap_or("visual")
        .to_string(),
    ),
    expected: Some("pass".to_string()),
    viewport: Some(ManifestViewport {
      width: 800,
      height: 600,
    }),
    timeout_ms: Some(20000),
    disabled: None,
    dpr: Some(1.0),
  };

  Ok(manifest_entry)
}

fn rewrite_and_copy(
  config: &ImportConfig,
  src_path: &Path,
  dest_path: &Path,
  track_reftest: bool,
  reftest_reference: &mut Option<Reference>,
  summary: &mut ImportSummary,
) -> Result<Vec<Reference>> {
  ensure_within_root(src_path, &config.wpt_root)?;

  let kind = file_kind(src_path);
  match kind {
    FileKind::Html | FileKind::Css => {
      let content =
        fs::read_to_string(src_path).map_err(|e| ImportError::Io(src_path.to_path_buf(), e))?;
      let (rewritten, references, reftest) = if kind == FileKind::Html {
        rewrite_html(config, src_path, dest_path, &content)?
      } else {
        rewrite_css(config, src_path, dest_path, &content)?
      };

      if track_reftest {
        *reftest_reference = reftest;
      }

      write_file(dest_path, rewritten.as_bytes(), config, summary)?;
      Ok(references)
    }
    FileKind::Other => {
      let data = fs::read(src_path).map_err(|e| ImportError::Io(src_path.to_path_buf(), e))?;
      write_file(dest_path, &data, config, summary)?;
      Ok(Vec::new())
    }
  }
}

fn rewrite_html(
  config: &ImportConfig,
  src_path: &Path,
  dest_path: &Path,
  content: &str,
) -> Result<(String, Vec<Reference>, Option<Reference>)> {
  let src_dir = src_path
    .parent()
    .ok_or_else(|| ImportError::Message(format!("missing parent for {}", src_path.display())))?;
  let dest_dir = dest_path
    .parent()
    .ok_or_else(|| ImportError::Message(format!("missing parent for {}", dest_path.display())))?;

  let mut references = Vec::new();
  let mut seen = HashSet::new();
  let mut reftest_reference: Option<Reference> = None;

  if let Some(cap) = Regex::new("(?i)<link[^>]*rel=[\"']match[\"'][^>]*href=[\"']([^\"']+)[\"']")
    .unwrap()
    .captures(content)
  {
    if let Some(href) = cap.get(1) {
      if let Some(reference) = resolve_reference(config, src_dir, dest_dir, href.as_str().trim())? {
        reftest_reference = Some(reference.clone());
        if seen.insert(reference.dest_path.clone()) {
          references.push(reference);
        }
      }
    }
  }

  let attr_regex =
    Regex::new("(?i)(?P<prefix>(?:src|href)\\s*=\\s*[\"'])(?P<url>[^\"'>]+)(?P<suffix>[\"'])")
      .unwrap();
  let url_regex =
    Regex::new("(?i)(?P<prefix>url\\(\\s*[\"']?)(?P<url>[^\"')]+)(?P<suffix>[\"']?\\s*\\))")
      .unwrap();
  let import_regex =
    Regex::new("(?i)(?P<prefix>@import\\s+['\"])(?P<url>[^\"']+)(?P<suffix>['\"])").unwrap();

  let mut rewritten = apply_rewrite(
    &attr_regex,
    content,
    config,
    src_dir,
    dest_dir,
    &mut references,
    &mut seen,
  )?;

  rewritten = apply_rewrite(
    &url_regex,
    &rewritten,
    config,
    src_dir,
    dest_dir,
    &mut references,
    &mut seen,
  )?;

  rewritten = apply_rewrite(
    &import_regex,
    &rewritten,
    config,
    src_dir,
    dest_dir,
    &mut references,
    &mut seen,
  )?;

  Ok((rewritten, references, reftest_reference))
}

fn rewrite_css(
  config: &ImportConfig,
  src_path: &Path,
  dest_path: &Path,
  content: &str,
) -> Result<(String, Vec<Reference>, Option<Reference>)> {
  let src_dir = src_path
    .parent()
    .ok_or_else(|| ImportError::Message(format!("missing parent for {}", src_path.display())))?;
  let dest_dir = dest_path
    .parent()
    .ok_or_else(|| ImportError::Message(format!("missing parent for {}", dest_path.display())))?;

  let mut references = Vec::new();
  let mut seen = HashSet::new();
  let url_regex =
    Regex::new("(?i)(?P<prefix>url\\(\\s*[\"']?)(?P<url>[^\"')]+)(?P<suffix>[\"']?\\s*\\))")
      .unwrap();
  let import_regex =
    Regex::new("(?i)(?P<prefix>@import\\s+['\"])(?P<url>[^\"']+)(?P<suffix>['\"])").unwrap();

  let mut rewritten = apply_rewrite(
    &url_regex,
    content,
    config,
    src_dir,
    dest_dir,
    &mut references,
    &mut seen,
  )?;

  rewritten = apply_rewrite(
    &import_regex,
    &rewritten,
    config,
    src_dir,
    dest_dir,
    &mut references,
    &mut seen,
  )?;

  Ok((rewritten, references, None))
}

fn apply_rewrite(
  regex: &Regex,
  input: &str,
  config: &ImportConfig,
  src_dir: &Path,
  dest_dir: &Path,
  references: &mut Vec<Reference>,
  seen: &mut HashSet<PathBuf>,
) -> Result<String> {
  let mut error: Option<ImportError> = None;
  let rewritten = regex
    .replace_all(input, |caps: &regex::Captures<'_>| match rewrite_reference(
      config,
      src_dir,
      dest_dir,
      &caps["url"],
      references,
      seen,
    ) {
      Ok(Some(new_value)) => format!("{}{}{}", &caps["prefix"], new_value, &caps["suffix"]),
      Ok(None) => caps[0].to_string(),
      Err(err) => {
        error = Some(err);
        caps[0].to_string()
      }
    })
    .to_string();

  if let Some(err) = error {
    return Err(err);
  }

  Ok(rewritten)
}

fn rewrite_reference(
  config: &ImportConfig,
  src_dir: &Path,
  dest_dir: &Path,
  url: &str,
  references: &mut Vec<Reference>,
  seen: &mut HashSet<PathBuf>,
) -> Result<Option<String>> {
  match resolve_reference(config, src_dir, dest_dir, url)? {
    Some(reference) => {
      if seen.insert(reference.dest_path.clone()) {
        references.push(reference.clone());
      }
      Ok(Some(reference.new_value))
    }
    None => Ok(None),
  }
}

fn resolve_reference(
  config: &ImportConfig,
  src_dir: &Path,
  dest_dir: &Path,
  value: &str,
) -> Result<Option<Reference>> {
  let trimmed = value.trim();
  if trimmed.is_empty()
    || trimmed.starts_with('#')
    || trimmed.starts_with("data:")
    || trimmed.starts_with("http://")
    || trimmed.starts_with("https://")
    || trimmed.starts_with("//")
    || trimmed.starts_with("about:")
    || trimmed.starts_with("javascript:")
  {
    return Ok(None);
  }

  let (path_part, suffix) = split_path_and_suffix(trimmed);
  if path_part.is_empty() {
    return Ok(None);
  }

  let is_absolute = path_part.starts_with('/');
  let clean = if is_absolute {
    PathBuf::from(path_part.trim_start_matches('/'))
  } else {
    PathBuf::from(path_part.clone())
  };

  let source_path = if is_absolute {
    normalize_path(config.wpt_root.join(&clean))
  } else {
    normalize_path(src_dir.join(&clean))
  };
  ensure_within_root(&source_path, &config.wpt_root)?;
  if !source_path.is_file() {
    return Err(ImportError::MissingReference(source_path));
  }

  let dest_path = if is_absolute {
    normalize_path(config.out_dir.join(&clean))
  } else {
    normalize_path(dest_dir.join(&clean))
  };
  ensure_within_root(&dest_path, &config.out_dir)?;

  let rel = pathdiff::diff_paths(&dest_path, dest_dir).unwrap_or_else(|| {
    dest_path
      .strip_prefix(dest_dir)
      .unwrap_or(&dest_path)
      .to_path_buf()
  });
  let mut new_value = normalize_to_forward_slashes(&rel);
  new_value.push_str(&suffix);

  Ok(Some(Reference {
    original: value.to_string(),
    new_value,
    source_path,
    dest_path,
  }))
}

fn split_path_and_suffix(value: &str) -> (String, String) {
  if let Some(pos) = value.find(|c| c == '?' || c == '#') {
    (value[..pos].to_string(), value[pos..].to_string())
  } else {
    (value.to_string(), String::new())
  }
}

fn write_file(
  dest_path: &Path,
  data: &[u8],
  config: &ImportConfig,
  summary: &mut ImportSummary,
) -> Result<()> {
  let existed_before = dest_path.exists();

  if existed_before {
    let existing = fs::read(dest_path).map_err(|e| ImportError::Io(dest_path.to_path_buf(), e))?;
    if existing == data {
      summary
        .skipped
        .push(make_relative(dest_path, &config.out_dir));
      return Ok(());
    }

    if !config.overwrite {
      return Err(ImportError::WouldOverwrite(dest_path.to_path_buf()));
    }
  }

  if !config.dry_run {
    if let Some(parent) = dest_path.parent() {
      fs::create_dir_all(parent).map_err(|e| ImportError::Io(parent.to_path_buf(), e))?;
    }
    fs::write(dest_path, data).map_err(|e| ImportError::Io(dest_path.to_path_buf(), e))?;
  }

  let rel = make_relative(dest_path, &config.out_dir);
  if existed_before {
    summary.overwritten.push(rel);
  } else {
    summary.copied.push(rel);
  }
  Ok(())
}

fn update_manifest(
  manifest_path: &Path,
  new_entries: &[ManifestEntry],
  config: &ImportConfig,
) -> Result<bool> {
  let existing_raw = if manifest_path.exists() {
    Some(
      fs::read_to_string(manifest_path)
        .map_err(|e| ImportError::Io(manifest_path.to_path_buf(), e))?,
    )
  } else {
    None
  };

  let manifest = if let Some(raw) = &existing_raw {
    toml::from_str::<ManifestFile>(raw)?
  } else {
    ManifestFile { tests: Vec::new() }
  };

  let mut by_id: HashMap<String, ManifestEntry> = manifest
    .tests
    .into_iter()
    .map(|entry| (entry.id.clone(), entry))
    .collect();

  let mut changed = false;
  for entry in new_entries {
    if let Some(existing) = by_id.get(&entry.id) {
      if existing != entry {
        if config.overwrite {
          by_id.insert(entry.id.clone(), entry.clone());
          changed = true;
        } else {
          return Err(ImportError::ManifestConflict(entry.id.clone()));
        }
      }
    } else {
      by_id.insert(entry.id.clone(), entry.clone());
      changed = true;
    }
  }

  let mut updated_entries: Vec<ManifestEntry> = by_id.into_values().collect();
  updated_entries.sort_by(|a, b| a.id.cmp(&b.id));

  let updated_manifest = ManifestFile {
    tests: updated_entries,
  };

  let serialized = toml::to_string_pretty(&updated_manifest)?;

  let needs_write = if let Some(raw) = &existing_raw {
    raw != &serialized || changed
  } else {
    changed && !new_entries.is_empty()
  };

  if config.dry_run || !needs_write {
    return Ok(false);
  }

  if let Some(parent) = manifest_path.parent() {
    fs::create_dir_all(parent).map_err(|e| ImportError::Io(parent.to_path_buf(), e))?;
  }
  fs::write(manifest_path, serialized)
    .map_err(|e| ImportError::Io(manifest_path.to_path_buf(), e))?;
  Ok(true)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum FileKind {
  Html,
  Css,
  Other,
}

fn file_kind(path: &Path) -> FileKind {
  match path
    .extension()
    .and_then(|e| e.to_str())
    .map(|s| s.to_ascii_lowercase())
  {
    Some(ext) if ext == "html" || ext == "htm" => FileKind::Html,
    Some(ext) if ext == "css" => FileKind::Css,
    _ => FileKind::Other,
  }
}

fn is_root_test_file(path: &Path) -> bool {
  if !path.is_file() {
    return false;
  }

  match path
    .extension()
    .and_then(|e| e.to_str())
    .map(|s| s.to_ascii_lowercase())
  {
    Some(ext) if ext == "html" || ext == "htm" => {}
    _ => return false,
  }

  let filename = path.file_name().and_then(|f| f.to_str()).unwrap_or("");
  if filename.ends_with("-ref.html") || filename.ends_with("-ref.htm") {
    return false;
  }
  if filename.ends_with("-expected.html") || filename.starts_with("support") {
    return false;
  }

  true
}

fn normalize_path(path: PathBuf) -> PathBuf {
  let mut normalized = PathBuf::new();
  for component in path.components() {
    match component {
      Component::CurDir => {}
      Component::ParentDir => {
        normalized.pop();
      }
      Component::RootDir | Component::Prefix(_) => normalized.push(component.as_os_str()),
      Component::Normal(part) => normalized.push(part),
    }
  }
  normalized
}

fn make_relative(path: &Path, root: &Path) -> PathBuf {
  path
    .strip_prefix(root)
    .map(|p| p.to_path_buf())
    .unwrap_or_else(|_| path.to_path_buf())
}

fn normalize_to_forward_slashes(path: &Path) -> String {
  path.to_string_lossy().replace('\\', "/")
}

fn manifest_id_from_relative(relative: &Path) -> String {
  let mut stem = relative.to_path_buf();
  stem.set_extension("");
  normalize_to_forward_slashes(&stem)
}

fn has_glob_pattern(value: &str) -> bool {
  value
    .chars()
    .any(|c| matches!(c, '*' | '?' | '[' | ']' | '{' | '}'))
}

fn ensure_within_root(path: &Path, root: &Path) -> Result<()> {
  if !path.starts_with(root) {
    return Err(ImportError::OutsideRoot(path.to_path_buf()));
  }
  Ok(())
}

fn absolutize(path: &Path) -> Result<PathBuf> {
  if path.is_absolute() {
    Ok(normalize_path(path.to_path_buf()))
  } else {
    Ok(normalize_path(
      std::env::current_dir()
        .map_err(|e| ImportError::Io(path.to_path_buf(), e))?
        .join(path),
    ))
  }
}

fn canonical_existing_dir(path: &Path) -> io::Result<PathBuf> {
  let canonical = fs::canonicalize(path)?;
  if !canonical.is_dir() {
    return Err(io::Error::new(io::ErrorKind::NotFound, "not a directory"));
  }
  Ok(canonical)
}

#[cfg(test)]
mod tests {
  use super::*;
  use tempfile::TempDir;

  fn fixture_root() -> PathBuf {
    absolutize(Path::new("tests/wpt/_import_testdata/wpt")).unwrap()
  }

  #[test]
  fn imports_synthetic_reftest_suite() {
    let out_dir = TempDir::new().unwrap();
    let manifest_path = out_dir.path().join("manifest.toml");
    let config = ImportConfig {
      wpt_root: fixture_root(),
      suites: vec!["css/simple".to_string()],
      out_dir: out_dir.path().join("wpt"),
      manifest_path: Some(manifest_path.clone()),
      dry_run: false,
      overwrite: false,
    };

    let summary = run_import(config.clone()).expect("import should succeed");
    assert!(!summary.copied.is_empty());

    let test_html = fs::read_to_string(out_dir.path().join("wpt/css/simple/reftest.html")).unwrap();
    assert!(!test_html.contains("/resources/"));
    assert!(test_html.contains("reftest-ref.html"));

    let css =
      fs::read_to_string(out_dir.path().join("wpt/css/simple/support/relative.css")).unwrap();
    assert!(css.contains("resources/green.png"));
    assert!(!css.contains("/resources/"));

    let manifest = fs::read_to_string(manifest_path).unwrap();
    assert!(manifest.contains("reftest-ref.html"));
    assert!(manifest.contains("test_type = \"reftest\""));
  }

  #[test]
  fn importer_is_idempotent_without_overwrite() {
    let out_dir = TempDir::new().unwrap();
    let manifest_path = out_dir.path().join("manifest.toml");
    let config = ImportConfig {
      wpt_root: fixture_root(),
      suites: vec!["html/standalone".to_string()],
      out_dir: out_dir.path().join("out"),
      manifest_path: Some(manifest_path),
      dry_run: false,
      overwrite: false,
    };

    run_import(config.clone()).unwrap();
    let first_html =
      fs::read_to_string(out_dir.path().join("out/html/standalone/visual-asset.html")).unwrap();
    let summary = run_import(config.clone()).unwrap();
    assert!(!summary.skipped.is_empty());
    let second_html =
      fs::read_to_string(out_dir.path().join("out/html/standalone/visual-asset.html")).unwrap();
    assert_eq!(first_html, second_html);
  }

  #[test]
  fn dry_run_does_not_write_files() {
    let out_dir = TempDir::new().unwrap();
    let manifest_path = out_dir.path().join("manifest.toml");
    let config = ImportConfig {
      wpt_root: fixture_root(),
      suites: vec!["css/simple/reftest.html".to_string()],
      out_dir: out_dir.path().join("dry"),
      manifest_path: Some(manifest_path.clone()),
      dry_run: true,
      overwrite: false,
    };

    let summary = run_import(config).unwrap();
    assert!(!summary.copied.is_empty());
    assert!(!out_dir.path().join("dry").exists());
    assert!(!manifest_path.exists());
  }
}
