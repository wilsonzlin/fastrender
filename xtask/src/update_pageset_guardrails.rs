use anyhow::{bail, Context, Result};
use clap::{Args, ValueEnum};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const DEFAULT_VIEWPORT: [u32; 2] = [1200, 800];
const DEFAULT_DPR: f32 = 1.0;
const DEFAULT_MEDIA: &str = "screen";
const FALLBACK_DEFAULT_BUDGET_MS: f64 = 5000.0;
const DEFAULT_MANIFEST_PATH: &str = "tests/pages/pageset_guardrails.json";
const LEGACY_MANIFEST_PATH: &str = "tests/pages/pageset_timeouts.json";

#[derive(Args, Debug)]
pub struct UpdatePagesetGuardrailsArgs {
  /// Directory containing `progress/pages/*.json`
  #[arg(long, default_value = "progress/pages")]
  pub progress_dir: PathBuf,

  /// Path to the pageset guardrails manifest to rewrite
  #[arg(long, default_value = DEFAULT_MANIFEST_PATH)]
  pub manifest: PathBuf,

  /// Minimum number of pages to include (defaults to the existing manifest length, or 10 if empty)
  ///
  /// Pages that are currently failing in the pageset scoreboard (status `timeout`, `panic`, or
  /// `error`) are always included even if that exceeds this value. When the failing pages are
  /// fewer than `--count`, the remaining slots are filled with `ok` pages according to
  /// `--strategy`.
  #[arg(long)]
  pub count: Option<usize>,

  /// Root directory containing offline fixtures
  #[arg(long, default_value = "tests/pages/fixtures")]
  pub fixtures_root: PathBuf,

  /// Capture missing fixtures by running `bundle_page` + `xtask import-page-fixture`
  ///
  /// By default the command only prints instructions. When enabled:
  /// - `--capture-mode render|crawl` fetches live subresources and requires network access.
  /// - `--capture-mode cache` reads from warmed pageset caches (`fetches/html` + `fetches/assets`)
  ///   and does not require network access.
  #[arg(long)]
  pub capture_missing_fixtures: bool,

  /// Override the User-Agent value passed to `bundle_page cache`.
  ///
  /// This must match the User-Agent used when warming the pageset disk cache, otherwise cache
  /// capture will miss subresources.
  #[arg(long)]
  pub user_agent: Option<String>,

  /// Override the Accept-Language value passed to `bundle_page cache`.
  ///
  /// This must match the Accept-Language used when warming the pageset disk cache, otherwise cache
  /// capture will miss subresources.
  #[arg(long)]
  pub accept_language: Option<String>,

  /// Disk-backed subresource cache directory passed to `bundle_page cache`.
  ///
  /// This should match the `--cache-dir` value used when warming the pageset disk cache.
  #[arg(long, default_value = "fetches/assets", value_name = "DIR", alias = "cache-dir")]
  pub asset_cache_dir: PathBuf,

  /// Overwrite existing fixture directories when capturing missing fixtures
  #[arg(long)]
  pub overwrite_fixtures: bool,

  /// Allow missing subresources while importing bundles into fixtures
  #[arg(long)]
  pub allow_missing_resources: bool,

  /// Capture mode used when running `bundle_page fetch` for missing fixtures.
  #[arg(long, value_enum, default_value_t = FixtureCaptureMode::Crawl)]
  pub capture_mode: FixtureCaptureMode,

  /// Per-request fetch timeout (seconds) passed to `bundle_page fetch` when capturing missing fixtures.
  ///
  /// This is useful when crawling pages that reference slow/unresponsive origins.
  #[arg(long)]
  pub bundle_fetch_timeout_secs: Option<u64>,

  /// Print what would change without writing files
  #[arg(long)]
  pub dry_run: bool,

  /// Selection strategy for picking fixtures from the pageset progress report
  #[arg(long, value_enum, default_value_t = PagesetGuardrailsSelectionStrategy::Coverage)]
  pub strategy: PagesetGuardrailsSelectionStrategy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
#[clap(rename_all = "snake_case")]
pub enum PagesetGuardrailsSelectionStrategy {
  /// Pick the globally slowest pages by total_ms.
  ///
  /// Timeout/panic/error pages are always included first; remaining slots (up to `--count`) are
  /// filled with the slowest OK pages.
  Slowest,
  /// Prioritize coverage across OK-page hotspots.
  ///
  /// Timeout/panic/error pages are always included first, and a small number of OK pages are
  /// selected to cover hotspot categories even when failures exceed `--count`.
  Coverage,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
#[clap(rename_all = "snake_case")]
pub enum FixtureCaptureMode {
  /// Fetch the page and capture subresources by rendering it once (historical behavior).
  Render,
  /// Fetch the page and discover subresources by parsing HTML + CSS without rendering.
  Crawl,
  /// Bundle a cached pageset entry (HTML + disk-backed assets) with no network access.
  Cache,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
enum ProgressStatus {
  Ok,
  Timeout,
  Panic,
  Error,
}

impl ProgressStatus {
  fn is_failure(self) -> bool {
    matches!(
      self,
      ProgressStatus::Timeout | ProgressStatus::Panic | ProgressStatus::Error
    )
  }

  fn selection_priority(self) -> u8 {
    match self {
      ProgressStatus::Panic => 0,
      ProgressStatus::Error => 1,
      ProgressStatus::Timeout => 2,
      ProgressStatus::Ok => 3,
    }
  }
}

#[derive(Debug, Deserialize)]
struct PageProgress {
  url: String,
  status: ProgressStatus,
  #[serde(default)]
  total_ms: Option<f64>,
  #[serde(default)]
  hotspot: Option<String>,
}

#[derive(Debug, Clone)]
struct ProgressEntry {
  name: String,
  url: String,
  status: ProgressStatus,
  total_ms: f64,
  hotspot: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
struct PagesetGuardrailsManifest {
  schema_version: u32,
  #[serde(default)]
  default_budget_ms: Option<f64>,
  fixtures: Vec<PagesetGuardrailsFixture>,
  #[serde(flatten)]
  extra: BTreeMap<String, serde_json::Value>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct PagesetGuardrailsFixture {
  name: String,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  url: Option<String>,
  viewport: [u32; 2],
  dpr: f32,
  media: String,
  #[serde(default)]
  budget_ms: Option<f64>,
}

pub fn run_update_pageset_guardrails(args: UpdatePagesetGuardrailsArgs) -> Result<()> {
  if args.dry_run && args.capture_missing_fixtures {
    bail!("cannot combine --dry-run with --capture-missing-fixtures");
  }
  let existing = load_manifest(&args.manifest)?;
  let count = args.count.unwrap_or_else(|| {
    if existing.fixtures.is_empty() {
      10
    } else {
      existing.fixtures.len()
    }
  });
  if count == 0 {
    bail!("--count must be > 0");
  }

  let progress = read_progress_entries(&args.progress_dir)?;
  let selected = select_pages(&progress, count, args.strategy);
  let failing_pages = progress.iter().filter(|e| e.status.is_failure()).count();
  if failing_pages > count {
    let ok_pages = selected.iter().filter(|e| e.status == ProgressStatus::Ok).count();
    eprintln!(
      "Warning: progress contains {failing_pages} failing page(s) (timeout/panic/error), which exceeds \
requested --count={count}. The manifest will include all failures and {ok_pages} ok page(s) \
({} total).",
      selected.len()
    );
  }
  if selected.is_empty() {
    bail!(
      "no pages selected from {}; expected at least one timeout or ok page",
      args.progress_dir.display()
    );
  }

  let updated = update_manifest(existing, &selected);
  let json = serde_json::to_string_pretty(&updated).context("serialize manifest")?;
  if args.dry_run {
    println!("{json}");
  } else {
    fs::write(&args.manifest, format!("{json}\n")).with_context(|| {
      format!(
        "failed to write pageset guardrails manifest to {}",
        args.manifest.display()
      )
    })?;
    println!(
      "✓ Updated {} ({} fixture(s))",
      args.manifest.display(),
      updated.fixtures.len()
    );

    // Backwards compatibility: keep the historical pageset_timeouts.json in sync as a plain file
    // (no symlink; Windows-friendly).
    let manifest_path = normalize_manifest_path(&args.manifest);
    let default_manifest = normalize_manifest_path(Path::new(DEFAULT_MANIFEST_PATH));
    let legacy_manifest = normalize_manifest_path(Path::new(LEGACY_MANIFEST_PATH));
    if manifest_path == default_manifest {
      fs::write(&legacy_manifest, format!("{json}\n"))
        .context("failed to write legacy pageset_timeouts manifest copy")?;
    } else if manifest_path == legacy_manifest {
      fs::write(&default_manifest, format!("{json}\n"))
        .context("failed to write pageset_guardrails manifest copy")?;
    }
  }

  let missing = missing_fixtures(&selected, &updated, &args.fixtures_root);
  if missing.is_empty() {
    println!("✓ All selected pages have offline fixtures.");
    return Ok(());
  }

  eprintln!(
    "Missing {} offline fixture(s) under {}:",
    missing.len(),
    args.fixtures_root.display()
  );
  for entry in &missing {
    let fixture_dir = args.fixtures_root.join(&entry.name);
    let bundle_path = default_bundle_path(&entry.name);
    let fetch_timeout_flag = args
      .bundle_fetch_timeout_secs
      .map(|secs| format!(" --fetch-timeout-secs {secs}"))
      .unwrap_or_default();
    let asset_cache_flag = format!(" --asset-cache-dir '{}'", args.asset_cache_dir.display());
    let overwrite_flag = if args.overwrite_fixtures {
      " --overwrite"
    } else {
      ""
    };
    let allow_missing_flag = if args.allow_missing_resources {
      " --allow-missing"
    } else {
      ""
    };
    eprintln!("  - {} ({})", entry.name, entry.url);
    eprintln!("    Create it with:");
    match args.capture_mode {
      FixtureCaptureMode::Render => {
        eprintln!(
          "      cargo run --release --bin bundle_page -- fetch '{}' --out '{}'{} --viewport {}x{} --dpr {}",
          entry.url,
          bundle_path.display(),
          fetch_timeout_flag,
          entry.viewport[0],
          entry.viewport[1],
          entry.dpr
        );
      }
      FixtureCaptureMode::Crawl => {
        eprintln!(
          "      cargo run --release --bin bundle_page -- fetch '{}' --out '{}' --no-render{} --viewport {}x{} --dpr {}",
          entry.url,
          bundle_path.display(),
          fetch_timeout_flag,
          entry.viewport[0],
          entry.viewport[1],
          entry.dpr
        );
      }
      FixtureCaptureMode::Cache => {
        eprintln!(
          "      cargo run --release --features disk_cache --bin bundle_page -- cache '{}' --out '{}'{}{}{}{} --viewport {}x{} --dpr {}",
          entry.name,
          bundle_path.display(),
          asset_cache_flag,
          args
            .user_agent
            .as_deref()
            .map(|ua| format!(" --user-agent '{ua}'"))
            .unwrap_or_default(),
          args
            .accept_language
            .as_deref()
            .map(|lang| format!(" --accept-language '{lang}'"))
            .unwrap_or_default(),
          allow_missing_flag,
          entry.viewport[0],
          entry.viewport[1],
          entry.dpr
        );
      }
    }
    eprintln!(
      "      cargo xtask import-page-fixture '{}' '{}' --output-root '{}'{}{}",
      bundle_path.display(),
      entry.name,
      args.fixtures_root.display(),
      overwrite_flag,
      allow_missing_flag
    );
    eprintln!(
      "    Expected output: {}",
      fixture_dir.join("index.html").display()
    );
  }

  if args.capture_missing_fixtures {
    capture_missing(&missing, &args)?;
  }

  Ok(())
}

fn default_bundle_path(name: &str) -> PathBuf {
  PathBuf::from("target/pageset-guardrails/bundles").join(format!("{name}.tar"))
}

fn normalize_manifest_path(path: &Path) -> PathBuf {
  let resolved = if path.is_absolute() {
    path.to_path_buf()
  } else {
    crate::repo_root().join(path)
  };
  resolved.canonicalize().unwrap_or(resolved)
}

fn capture_missing(missing: &[MissingFixture], args: &UpdatePagesetGuardrailsArgs) -> Result<()> {
  if missing.is_empty() {
    return Ok(());
  }
  eprintln!("Capturing {} fixture(s)...", missing.len());
  for entry in missing {
    let bundle_path = default_bundle_path(&entry.name);
    if let Some(parent) = bundle_path.parent() {
      fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
    }

    let mut bundle_cmd = Command::new("cargo");
    bundle_cmd.args(["run", "--release"]);
    if args.capture_mode == FixtureCaptureMode::Cache {
      use crate::DiskCacheFeatureExt;
      bundle_cmd.apply_disk_cache_feature(true);
    }
    bundle_cmd.args(["--bin", "bundle_page", "--"]);
    match args.capture_mode {
      FixtureCaptureMode::Render => {
        bundle_cmd.args(["fetch", &entry.url]);
      }
      FixtureCaptureMode::Crawl => {
        bundle_cmd.args(["fetch", &entry.url]).arg("--no-render");
      }
      FixtureCaptureMode::Cache => {
        bundle_cmd.args(["cache", &entry.name]);
        bundle_cmd.args([
          "--asset-cache-dir",
          args.asset_cache_dir.to_string_lossy().as_ref(),
        ]);
        if args.allow_missing_resources {
          bundle_cmd.arg("--allow-missing");
        }
        if let Some(user_agent) = args.user_agent.as_deref() {
          bundle_cmd.args(["--user-agent", user_agent]);
        }
        if let Some(accept_language) = args.accept_language.as_deref() {
          bundle_cmd.args(["--accept-language", accept_language]);
        }
      }
    }
    bundle_cmd.args(["--out", bundle_path.to_string_lossy().as_ref()]);
    if let Some(secs) = args.bundle_fetch_timeout_secs {
      if args.capture_mode != FixtureCaptureMode::Cache {
        bundle_cmd.args(["--fetch-timeout-secs", &secs.to_string()]);
      }
    }
    bundle_cmd
      .args([
        "--viewport",
        &format!("{}x{}", entry.viewport[0], entry.viewport[1]),
      ])
      .args(["--dpr", &entry.dpr.to_string()]);
    bundle_cmd.current_dir(crate::repo_root());
    crate::run_command(bundle_cmd).with_context(|| format!("bundle {}", entry.name))?;

    crate::import_page_fixture::run_import_page_fixture(
      crate::import_page_fixture::ImportPageFixtureArgs {
        bundle: bundle_path,
        fixture_name: entry.name.clone(),
        output_root: args.fixtures_root.clone(),
        overwrite: args.overwrite_fixtures,
        allow_missing: args.allow_missing_resources,
        allow_http_references: false,
        legacy_rewrite: false,
        dry_run: false,
      },
    )
    .with_context(|| format!("import {}", entry.name))?;
  }

  Ok(())
}

#[derive(Debug, Clone)]
struct MissingFixture {
  name: String,
  url: String,
  viewport: [u32; 2],
  dpr: f32,
}

fn missing_fixtures(
  selected: &[ProgressEntry],
  manifest: &PagesetGuardrailsManifest,
  fixtures_root: &Path,
) -> Vec<MissingFixture> {
  let fixture_map: BTreeMap<&str, &PagesetGuardrailsFixture> = manifest
    .fixtures
    .iter()
    .map(|fixture| (fixture.name.as_str(), fixture))
    .collect();

  selected
    .iter()
    .filter_map(|entry| {
      let html = fixtures_root.join(&entry.name).join("index.html");
      if html.exists() {
        return None;
      }

      let config = fixture_map.get(entry.name.as_str());
      Some(MissingFixture {
        name: entry.name.clone(),
        url: entry.url.clone(),
        viewport: config.map(|c| c.viewport).unwrap_or(DEFAULT_VIEWPORT),
        dpr: config.map(|c| c.dpr).unwrap_or(DEFAULT_DPR),
      })
    })
    .collect()
}

fn load_manifest(path: &Path) -> Result<PagesetGuardrailsManifest> {
  let data = fs::read_to_string(path)
    .with_context(|| format!("failed to read pageset guardrails manifest {}", path.display()))?;
  serde_json::from_str(&data).with_context(|| format!("invalid JSON in {}", path.display()))
}

fn read_progress_entries(progress_dir: &Path) -> Result<Vec<ProgressEntry>> {
  let mut files: Vec<PathBuf> = fs::read_dir(progress_dir)
    .with_context(|| format!("read progress dir {}", progress_dir.display()))?
    .filter_map(|entry| entry.ok().map(|e| e.path()))
    .filter(|path| path.extension() == Some(OsStr::new("json")))
    .collect();
  files.sort();

  let mut entries = Vec::new();
  for path in files {
    let name = path
      .file_stem()
      .and_then(|s| s.to_str())
      .map(|s| s.to_string())
      .unwrap_or_else(|| path.to_string_lossy().to_string());
    let raw = fs::read_to_string(&path).with_context(|| format!("read {}", path.display()))?;
    let parsed: PageProgress =
      serde_json::from_str(&raw).with_context(|| format!("parse {}", path.display()))?;
    entries.push(ProgressEntry {
      name,
      url: parsed.url,
      status: parsed.status,
      total_ms: parsed.total_ms.unwrap_or(0.0),
      hotspot: parsed.hotspot,
    });
  }

  Ok(entries)
}

fn ok_page_reserve(count: usize) -> usize {
  let target = ((count as f64) * 0.2).ceil() as usize;
  target.clamp(1, 5)
}

fn select_pages(
  entries: &[ProgressEntry],
  count: usize,
  strategy: PagesetGuardrailsSelectionStrategy,
) -> Vec<ProgressEntry> {
  match strategy {
    PagesetGuardrailsSelectionStrategy::Slowest => select_pages_slowest(entries, count),
    PagesetGuardrailsSelectionStrategy::Coverage => select_pages_coverage(entries, count),
  }
}

fn entry_slowest_first(a: &ProgressEntry, b: &ProgressEntry) -> std::cmp::Ordering {
  b.total_ms
    .partial_cmp(&a.total_ms)
    .unwrap_or(std::cmp::Ordering::Equal)
    .then_with(|| a.name.cmp(&b.name))
}

fn entry_failure_first(a: &ProgressEntry, b: &ProgressEntry) -> std::cmp::Ordering {
  a.status
    .selection_priority()
    .cmp(&b.status.selection_priority())
    .then_with(|| {
      b.total_ms
        .partial_cmp(&a.total_ms)
        .unwrap_or(std::cmp::Ordering::Equal)
    })
    .then_with(|| a.name.cmp(&b.name))
}

fn select_pages_slowest(entries: &[ProgressEntry], count: usize) -> Vec<ProgressEntry> {
  let mut failures: Vec<ProgressEntry> = entries
    .iter()
    .filter(|e| e.status.is_failure())
    .cloned()
    .collect();
  failures.sort_by(entry_failure_first);

  let ok_slots = count.saturating_sub(failures.len());
  if ok_slots == 0 {
    return failures;
  }

  let mut ok: Vec<ProgressEntry> = entries
    .iter()
    .filter(|e| e.status == ProgressStatus::Ok)
    .cloned()
    .collect();
  ok.sort_by(entry_slowest_first);
  failures.extend(ok.into_iter().take(ok_slots));
  failures
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum HotspotCategory {
  Css,
  Cascade,
  BoxTree,
  Layout,
  Paint,
  Unknown,
}

impl HotspotCategory {
  fn from_hotspot(hotspot: Option<&str>) -> Self {
    let Some(raw) = hotspot else {
      return HotspotCategory::Unknown;
    };
    let normalized = raw.trim().to_ascii_lowercase();
    match normalized.as_str() {
      "css" => HotspotCategory::Css,
      "cascade" => HotspotCategory::Cascade,
      "box_tree" => HotspotCategory::BoxTree,
      "layout" => HotspotCategory::Layout,
      "paint" => HotspotCategory::Paint,
      "unknown" | "" => HotspotCategory::Unknown,
      // Treat any unexpected value (including "fetch") as "unknown" for the purposes of
      // hotspot diversity.
      _ => HotspotCategory::Unknown,
    }
  }
}

fn select_pages_coverage(entries: &[ProgressEntry], count: usize) -> Vec<ProgressEntry> {
  let mut failures: Vec<ProgressEntry> = entries
    .iter()
    .filter(|e| e.status.is_failure())
    .cloned()
    .collect();
  failures.sort_by(entry_failure_first);

  let mut ok: Vec<ProgressEntry> = entries
    .iter()
    .filter(|e| e.status == ProgressStatus::Ok)
    .cloned()
    .collect();
  ok.sort_by(entry_slowest_first);

  let ok_slots = if failures.len() < count {
    count.saturating_sub(failures.len()).min(ok.len())
  } else {
    let mut hotspot_categories_present: BTreeSet<HotspotCategory> = BTreeSet::new();
    for entry in &ok {
      hotspot_categories_present.insert(HotspotCategory::from_hotspot(entry.hotspot.as_deref()));
    }
    let desired_ok_slots = hotspot_categories_present.len();
    desired_ok_slots
      .max(ok_page_reserve(count))
      .min(ok.len())
  };

  if ok_slots == 0 {
    return failures;
  }

  let selected_ok = select_ok_pages_coverage(&ok, ok_slots);
  failures.extend(selected_ok);
  failures
}

fn select_ok_pages_coverage(ok: &[ProgressEntry], slots: usize) -> Vec<ProgressEntry> {
  if slots == 0 {
    return Vec::new();
  }

  let mut ok_reps_by_category: BTreeMap<HotspotCategory, ProgressEntry> = BTreeMap::new();
  for entry in ok {
    let category = HotspotCategory::from_hotspot(entry.hotspot.as_deref());
    ok_reps_by_category
      .entry(category)
      .or_insert_with(|| entry.clone());
  }
  let mut ok_reps: Vec<ProgressEntry> = ok_reps_by_category.into_values().collect();
  ok_reps.sort_by(entry_slowest_first);

  let mut selected_ok = Vec::new();
  let mut selected_ok_names = BTreeSet::new();
  for entry in ok_reps.into_iter().take(slots) {
    selected_ok_names.insert(entry.name.clone());
    selected_ok.push(entry);
  }
  for entry in ok {
    if selected_ok.len() >= slots {
      break;
    }
    if selected_ok_names.insert(entry.name.clone()) {
      selected_ok.push(entry.clone());
    }
  }
  selected_ok.sort_by(entry_slowest_first);

  selected_ok.truncate(slots);
  selected_ok
}

fn update_manifest(
  mut existing: PagesetGuardrailsManifest,
  selected: &[ProgressEntry],
) -> PagesetGuardrailsManifest {
  let mut existing_map = BTreeMap::new();
  for fixture in existing.fixtures {
    existing_map.insert(fixture.name.clone(), fixture);
  }

  let default_budget = existing
    .default_budget_ms
    .unwrap_or(FALLBACK_DEFAULT_BUDGET_MS);
  existing.fixtures = selected
    .iter()
    .map(|entry| {
      if let Some(existing) = existing_map.get(&entry.name) {
        let mut fixture = existing.clone();
        fixture.url = Some(entry.url.clone());
        fixture
      } else {
        PagesetGuardrailsFixture {
          name: entry.name.clone(),
          url: Some(entry.url.clone()),
          viewport: DEFAULT_VIEWPORT,
          dpr: DEFAULT_DPR,
          media: DEFAULT_MEDIA.to_string(),
          budget_ms: Some(default_budget),
        }
      }
    })
    .collect();

  existing
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json::json;
  use tempfile::TempDir;

  fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .parent()
      .expect("xtask has parent")
      .to_path_buf()
  }

  fn fixture_progress_dir() -> PathBuf {
    repo_root().join("tests/fixtures/pageset_guardrails_progress")
  }

  #[test]
  fn selects_failures_then_slowest_ok_pages() {
    let entries = read_progress_entries(&fixture_progress_dir()).expect("read fixture progress");
    let selected = select_pages(&entries, 5, PagesetGuardrailsSelectionStrategy::Slowest);
    let names: Vec<&str> = selected.iter().map(|e| e.name.as_str()).collect();
    assert_eq!(
      names,
      vec![
        "panic.test",
        "error.test",
        "timeout-a.test",
        "timeout-b.test",
        "slow-ok.test"
      ]
    );
  }

  #[test]
  fn failures_are_always_included_even_when_count_is_smaller() {
    let entries = read_progress_entries(&fixture_progress_dir()).expect("read fixture progress");
    for strategy in [
      PagesetGuardrailsSelectionStrategy::Slowest,
      PagesetGuardrailsSelectionStrategy::Coverage,
    ] {
      let selected = select_pages(&entries, 1, strategy);
      let names: BTreeSet<&str> = selected.iter().map(|e| e.name.as_str()).collect();
      assert!(names.contains("panic.test"));
      assert!(names.contains("error.test"));
      assert!(names.contains("timeout-a.test"));
      assert!(names.contains("timeout-b.test"));
    }
  }

  #[test]
  fn rewrites_manifest_deterministically_preserving_schema_version_and_existing_fixtures() {
    let tempdir = TempDir::new().expect("tempdir");
    let manifest_path = tempdir.path().join("pageset_guardrails.json");
    let original = json!({
      "schema_version": 99,
      "default_budget_ms": 7777.0,
      "fixtures": [
        { "name": "timeout-b.test", "url": "https://override.test/", "viewport": [800, 600], "dpr": 2.0, "media": "print", "budget_ms": 1234.0 },
        { "name": "slow-ok.test", "viewport": [640, 480], "dpr": 1.5, "media": "screen", "budget_ms": 4321.0 }
      ]
    });
    fs::write(
      &manifest_path,
      serde_json::to_string_pretty(&original).unwrap(),
    )
    .unwrap();

    let existing = load_manifest(&manifest_path).expect("load manifest");
    let entries = read_progress_entries(&fixture_progress_dir()).expect("read fixture progress");
    let selected = select_pages(&entries, 5, PagesetGuardrailsSelectionStrategy::Slowest);
    let updated = update_manifest(existing, &selected);
    fs::write(
      &manifest_path,
      serde_json::to_string_pretty(&updated).unwrap(),
    )
    .unwrap();

    let reparsed: serde_json::Value =
      serde_json::from_str(&fs::read_to_string(&manifest_path).unwrap()).unwrap();
    let expected = json!({
      "schema_version": 99,
      "default_budget_ms": 7777.0,
      "fixtures": [
        { "name": "panic.test", "url": "https://panic.test/", "viewport": [1200, 800], "dpr": 1.0, "media": "screen", "budget_ms": 7777.0 },
        { "name": "error.test", "url": "https://error.test/", "viewport": [1200, 800], "dpr": 1.0, "media": "screen", "budget_ms": 7777.0 },
        { "name": "timeout-a.test", "url": "https://timeout-a.test/", "viewport": [1200, 800], "dpr": 1.0, "media": "screen", "budget_ms": 7777.0 },
        { "name": "timeout-b.test", "url": "https://timeout-b.test/", "viewport": [800, 600], "dpr": 2.0, "media": "print", "budget_ms": 1234.0 },
        { "name": "slow-ok.test", "url": "https://slow-ok.test/", "viewport": [640, 480], "dpr": 1.5, "media": "screen", "budget_ms": 4321.0 }
      ]
    });
    assert_eq!(reparsed, expected);
  }

  #[test]
  fn preserves_manifest_root_extra_fields_via_flattened_extra_map() {
    let tempdir = TempDir::new().expect("tempdir");
    let manifest_path = tempdir.path().join("pageset_guardrails.json");
    let original = json!({
      "schema_version": 1,
      "default_budget_ms": 5000.0,
      "extra_root_field": { "answer": 42 },
      "fixtures": [
        { "name": "timeout-a.test", "viewport": [1200, 800], "dpr": 1.0, "media": "screen", "budget_ms": 5000.0 }
      ]
    });
    fs::write(
      &manifest_path,
      serde_json::to_string_pretty(&original).unwrap(),
    )
    .unwrap();

    let existing = load_manifest(&manifest_path).expect("load manifest");
    let entries = read_progress_entries(&fixture_progress_dir()).expect("read fixture progress");
    let selected = select_pages(&entries, 1, PagesetGuardrailsSelectionStrategy::Slowest);
    let updated = update_manifest(existing, &selected);
    fs::write(
      &manifest_path,
      serde_json::to_string_pretty(&updated).unwrap(),
    )
    .unwrap();

    let reparsed: serde_json::Value =
      serde_json::from_str(&fs::read_to_string(&manifest_path).unwrap()).unwrap();
    assert_eq!(reparsed.get("extra_root_field"), Some(&json!({ "answer": 42 })));
  }

  fn mk_entry(
    name: &str,
    status: ProgressStatus,
    total_ms: f64,
    hotspot: Option<&str>,
  ) -> ProgressEntry {
    ProgressEntry {
      name: name.to_string(),
      url: format!("https://{name}/"),
      status,
      total_ms,
      hotspot: hotspot.map(|s| s.to_string()),
    }
  }

  #[test]
  fn coverage_strategy_includes_ok_pages_even_when_failures_exceed_count() {
    let entries = vec![
      mk_entry(
        "timeout-layout",
        ProgressStatus::Timeout,
        5000.0,
        None,
      ),
      mk_entry(
        "timeout-css",
        ProgressStatus::Timeout,
        5000.0,
        None,
      ),
      mk_entry(
        "slow-ok-paint",
        ProgressStatus::Ok,
        4500.0,
        Some("paint"),
      ),
      mk_entry("slow-ok-css", ProgressStatus::Ok, 4400.0, Some("css")),
    ];

    // count=1 would normally truncate to a single timeout fixture; coverage keeps OK hotspot
    // coverage even when failures exceed the target count.
    let selected = select_pages(&entries, 1, PagesetGuardrailsSelectionStrategy::Coverage);
    let names: Vec<&str> = selected.iter().map(|e| e.name.as_str()).collect();
    assert_eq!(
      names,
      vec!["timeout-css", "timeout-layout", "slow-ok-paint", "slow-ok-css"]
    );
  }

  #[test]
  fn coverage_strategy_is_deterministic() {
    let entries = vec![
      mk_entry(
        "timeout-layout",
        ProgressStatus::Timeout,
        5000.0,
        None,
      ),
      mk_entry(
        "timeout-css",
        ProgressStatus::Timeout,
        5000.0,
        None,
      ),
      mk_entry(
        "slow-ok-paint",
        ProgressStatus::Ok,
        4500.0,
        Some("paint"),
      ),
      mk_entry("slow-ok-css", ProgressStatus::Ok, 4400.0, Some("css")),
    ];

    let a = select_pages(&entries, 3, PagesetGuardrailsSelectionStrategy::Coverage);
    let b = select_pages(&entries, 3, PagesetGuardrailsSelectionStrategy::Coverage);
    let a_names: Vec<&str> = a.iter().map(|e| e.name.as_str()).collect();
    let b_names: Vec<&str> = b.iter().map(|e| e.name.as_str()).collect();
    assert_eq!(a_names, b_names);
  }

  #[test]
  fn parses_progress_entries_missing_hotspot() {
    let temp = TempDir::new().expect("tempdir");
    let progress_dir = temp.path();
    fs::write(
      progress_dir.join("missing-fields.json"),
      r#"{"url":"https://example.test","status":"ok","total_ms":123.0}"#,
    )
    .unwrap();

    let parsed = read_progress_entries(progress_dir).expect("read progress entries");
    assert_eq!(parsed.len(), 1);
    assert_eq!(parsed[0].name, "missing-fields");
    assert_eq!(parsed[0].url, "https://example.test");
    assert_eq!(parsed[0].status, ProgressStatus::Ok);
    assert_eq!(parsed[0].total_ms, 123.0);
    assert_eq!(parsed[0].hotspot, None);
  }

  #[test]
  fn parses_box_tree_hotspot_category() {
    assert_eq!(
      HotspotCategory::from_hotspot(Some("box_tree")),
      HotspotCategory::BoxTree
    );
    assert_eq!(
      HotspotCategory::from_hotspot(Some("BOX_TREE")),
      HotspotCategory::BoxTree
    );
  }

  #[test]
  fn repo_guardrails_manifest_covers_all_current_failures() {
    let progress_dir = repo_root().join("progress/pages");
    let manifest_path = repo_root().join(DEFAULT_MANIFEST_PATH);
    let progress = read_progress_entries(&progress_dir).expect("read progress/pages");
    let manifest = load_manifest(&manifest_path).expect("load pageset guardrails manifest");

    let failing: BTreeSet<String> = progress
      .iter()
      .filter(|entry| entry.status.is_failure())
      .map(|entry| entry.name.clone())
      .collect();
    let fixtures: BTreeSet<String> = manifest
      .fixtures
      .iter()
      .map(|fixture| fixture.name.clone())
      .collect();
    let missing: Vec<String> = failing.difference(&fixtures).cloned().collect();
    assert!(
      missing.is_empty(),
      "{} is missing failing pages from progress/pages: {}",
      manifest_path.display(),
      missing.join(", ")
    );
  }

  #[test]
  fn repo_guardrails_manifest_fixtures_exist() {
    let root = repo_root();
    let fixtures_root = root.join("tests/pages/fixtures");
    let manifest_path = root.join(DEFAULT_MANIFEST_PATH);
    let manifest = load_manifest(&manifest_path).expect("load pageset guardrails manifest");

    assert!(
      !manifest.fixtures.is_empty(),
      "{} should contain at least one fixture (empty manifest disables perf guardrails)",
      manifest_path.display()
    );

    let mut missing = Vec::new();
    for fixture in &manifest.fixtures {
      let html_path = fixtures_root.join(&fixture.name).join("index.html");
      if !html_path.is_file() {
        missing.push(format!("{} ({})", fixture.name, html_path.display()));
      }
    }

    assert!(
      missing.is_empty(),
      "{} references fixture(s) missing under tests/pages/fixtures: {}",
      manifest_path.display(),
      missing.join(", ")
    );
  }

  #[test]
  fn normalize_manifest_path_matches_common_spellings() {
    let root = repo_root();
    let relative = super::normalize_manifest_path(Path::new(DEFAULT_MANIFEST_PATH));
    let absolute = super::normalize_manifest_path(&root.join(DEFAULT_MANIFEST_PATH));
    let dotted = super::normalize_manifest_path(Path::new(&format!("./{DEFAULT_MANIFEST_PATH}")));

    assert_eq!(relative, absolute);
    assert_eq!(relative, dotted);
  }

  #[test]
  fn legacy_manifest_is_kept_in_sync_with_guardrails() {
    let root = repo_root();
    let guardrails_path = root.join(DEFAULT_MANIFEST_PATH);
    let legacy_path = root.join(LEGACY_MANIFEST_PATH);

    let guardrails_raw =
      fs::read_to_string(&guardrails_path).expect("read pageset_guardrails manifest");
    let legacy_raw = fs::read_to_string(&legacy_path).expect("read pageset_timeouts manifest");

    let guardrails: serde_json::Value =
      serde_json::from_str(&guardrails_raw).expect("parse pageset_guardrails manifest");
    let legacy: serde_json::Value =
      serde_json::from_str(&legacy_raw).expect("parse pageset_timeouts manifest");

    assert_eq!(
      guardrails,
      legacy,
      "legacy manifest {} should mirror {}",
      legacy_path.display(),
      guardrails_path.display()
    );
  }

  #[test]
  fn repo_guardrails_manifest_urls_match_progress() {
    let progress_dir = repo_root().join("progress/pages");
    let manifest_path = repo_root().join(DEFAULT_MANIFEST_PATH);
    let progress = read_progress_entries(&progress_dir).expect("read progress/pages");
    let manifest = load_manifest(&manifest_path).expect("load pageset guardrails manifest");

    let progress_urls: BTreeMap<String, String> = progress
      .into_iter()
      .map(|entry| (entry.name, entry.url))
      .collect();

    let mut mismatches = Vec::new();
    for fixture in &manifest.fixtures {
      let expected = match progress_urls.get(&fixture.name) {
        Some(url) => url,
        None => {
          mismatches.push(format!(
            "{} missing progress/pages/{}.json",
            fixture.name, fixture.name
          ));
          continue;
        }
      };

      match fixture.url.as_deref() {
        Some(actual) if actual == expected => {}
        Some(actual) => mismatches.push(format!(
          "{} url mismatch: manifest={} progress={}",
          fixture.name, actual, expected
        )),
        None => mismatches.push(format!(
          "{} missing url field (expected {})",
          fixture.name, expected
        )),
      }
    }

    assert!(
      mismatches.is_empty(),
      "{} fixture URLs drifted from progress/pages:\n{}",
      manifest_path.display(),
      mismatches.join("\n")
    );
  }
} 
