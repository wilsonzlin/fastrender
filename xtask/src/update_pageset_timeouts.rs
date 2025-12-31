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

#[derive(Args, Debug)]
pub struct UpdatePagesetTimeoutsArgs {
  /// Directory containing `progress/pages/*.json`
  #[arg(long, default_value = "progress/pages")]
  pub progress_dir: PathBuf,

  /// Path to the pageset-timeouts manifest to rewrite
  #[arg(long, default_value = "tests/pages/pageset_timeouts.json")]
  pub manifest: PathBuf,

  /// Number of pages to include (defaults to the existing manifest length, or 10 if empty)
  #[arg(long)]
  pub count: Option<usize>,

  /// Root directory containing offline fixtures
  #[arg(long, default_value = "tests/pages/fixtures")]
  pub fixtures_root: PathBuf,

  /// Capture missing fixtures by running `bundle_page fetch` + `xtask import-page-fixture`
  ///
  /// This is slow and requires network access; by default the command only prints instructions.
  #[arg(long)]
  pub capture_missing_fixtures: bool,

  /// Overwrite existing fixture directories when capturing missing fixtures
  #[arg(long)]
  pub overwrite_fixtures: bool,

  /// Allow missing subresources while importing bundles into fixtures
  #[arg(long)]
  pub allow_missing_resources: bool,

  /// Print what would change without writing files
  #[arg(long)]
  pub dry_run: bool,

  /// Selection strategy for picking fixtures from the pageset progress report
  #[arg(long, value_enum, default_value_t = PagesetTimeoutSelectionStrategy::Slowest)]
  pub strategy: PagesetTimeoutSelectionStrategy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
#[clap(rename_all = "snake_case")]
pub enum PagesetTimeoutSelectionStrategy {
  /// Preserve the current behavior: pick the globally slowest pages by total_ms, preferring
  /// timeouts first.
  Slowest,
  /// Prioritize coverage across timeout stages and OK-page hotspots.
  Coverage,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
enum ProgressStatus {
  Ok,
  Timeout,
  Panic,
  Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize)]
#[serde(rename_all = "snake_case")]
enum ProgressStage {
  DomParse,
  Css,
  Cascade,
  Layout,
  Paint,
}

#[derive(Debug, Deserialize)]
struct PageProgress {
  url: String,
  status: ProgressStatus,
  #[serde(default)]
  total_ms: Option<f64>,
  #[serde(default)]
  timeout_stage: Option<ProgressStage>,
  #[serde(default)]
  hotspot: Option<String>,
}

#[derive(Debug, Clone)]
struct ProgressEntry {
  name: String,
  url: String,
  status: ProgressStatus,
  total_ms: f64,
  timeout_stage: Option<ProgressStage>,
  hotspot: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
struct PagesetTimeoutManifest {
  schema_version: u32,
  #[serde(default)]
  default_budget_ms: Option<f64>,
  fixtures: Vec<PagesetTimeoutFixture>,
  #[serde(flatten)]
  extra: BTreeMap<String, serde_json::Value>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct PagesetTimeoutFixture {
  name: String,
  viewport: [u32; 2],
  dpr: f32,
  media: String,
  #[serde(default)]
  budget_ms: Option<f64>,
}

pub fn run_update_pageset_timeouts(args: UpdatePagesetTimeoutsArgs) -> Result<()> {
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
        "failed to write pageset timeout manifest to {}",
        args.manifest.display()
      )
    })?;
    println!(
      "✓ Updated {} ({} fixture(s))",
      args.manifest.display(),
      updated.fixtures.len()
    );
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
    eprintln!("  - {} ({})", entry.name, entry.url);
    eprintln!("    Create it with:");
    eprintln!(
      "      cargo run --release --bin bundle_page -- fetch '{}' --out '{}' --viewport {}x{} --dpr {}",
      entry.url,
      bundle_path.display(),
      entry.viewport[0],
      entry.viewport[1],
      entry.dpr
    );
    eprintln!(
      "      cargo xtask import-page-fixture '{}' '{}' --output-root '{}'",
      bundle_path.display(),
      entry.name,
      args.fixtures_root.display()
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
  PathBuf::from("target/pageset-timeouts/bundles").join(format!("{name}.tar"))
}

fn capture_missing(missing: &[MissingFixture], args: &UpdatePagesetTimeoutsArgs) -> Result<()> {
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
    bundle_cmd
      .args(["run", "--release", "--bin", "bundle_page", "--"])
      .args(["fetch", &entry.url])
      .args(["--out", bundle_path.to_string_lossy().as_ref()])
      .args([
        "--viewport",
        &format!("{}x{}", entry.viewport[0], entry.viewport[1]),
      ])
      .args(["--dpr", &entry.dpr.to_string()]);
    crate::run_command(bundle_cmd).with_context(|| format!("bundle {}", entry.name))?;

    crate::import_page_fixture::run_import_page_fixture(
      crate::import_page_fixture::ImportPageFixtureArgs {
        bundle: bundle_path,
        fixture_name: entry.name.clone(),
        output_root: args.fixtures_root.clone(),
        overwrite: args.overwrite_fixtures,
        allow_missing: args.allow_missing_resources,
        allow_http_references: false,
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
  manifest: &PagesetTimeoutManifest,
  fixtures_root: &Path,
) -> Vec<MissingFixture> {
  let fixture_map: BTreeMap<&str, &PagesetTimeoutFixture> = manifest
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

fn load_manifest(path: &Path) -> Result<PagesetTimeoutManifest> {
  let data = fs::read_to_string(path)
    .with_context(|| format!("failed to read pageset timeout manifest {}", path.display()))?;
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
      timeout_stage: parsed.timeout_stage,
      hotspot: parsed.hotspot,
    });
  }

  Ok(entries)
}

fn select_pages(
  entries: &[ProgressEntry],
  count: usize,
  strategy: PagesetTimeoutSelectionStrategy,
) -> Vec<ProgressEntry> {
  match strategy {
    PagesetTimeoutSelectionStrategy::Slowest => select_pages_slowest(entries, count),
    PagesetTimeoutSelectionStrategy::Coverage => select_pages_coverage(entries, count),
  }
}

fn entry_slowest_first(a: &ProgressEntry, b: &ProgressEntry) -> std::cmp::Ordering {
  b.total_ms
    .partial_cmp(&a.total_ms)
    .unwrap_or(std::cmp::Ordering::Equal)
    .then_with(|| a.name.cmp(&b.name))
}

fn select_pages_slowest(entries: &[ProgressEntry], count: usize) -> Vec<ProgressEntry> {
  let mut timeouts: Vec<ProgressEntry> = entries
    .iter()
    .filter(|e| e.status == ProgressStatus::Timeout)
    .cloned()
    .collect();
  let mut ok: Vec<ProgressEntry> = entries
    .iter()
    .filter(|e| e.status == ProgressStatus::Ok)
    .cloned()
    .collect();

  timeouts.sort_by(entry_slowest_first);
  ok.sort_by(entry_slowest_first);

  timeouts
    .into_iter()
    .chain(ok.into_iter())
    .take(count)
    .collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum HotspotCategory {
  Css,
  Cascade,
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
  let mut timeouts: Vec<ProgressEntry> = entries
    .iter()
    .filter(|e| e.status == ProgressStatus::Timeout)
    .cloned()
    .collect();
  let mut ok: Vec<ProgressEntry> = entries
    .iter()
    .filter(|e| e.status == ProgressStatus::Ok)
    .cloned()
    .collect();

  timeouts.sort_by(entry_slowest_first);
  ok.sort_by(entry_slowest_first);

  // Determine one representative timeout per stage (if the stage is known), and one
  // representative OK page per hotspot category.
  let stage_order = [
    ProgressStage::DomParse,
    ProgressStage::Css,
    ProgressStage::Cascade,
    ProgressStage::Layout,
    ProgressStage::Paint,
  ];
  let mut stage_reps: Vec<ProgressEntry> = stage_order
    .iter()
    .filter_map(|stage| {
      timeouts
        .iter()
        .find(|e| e.timeout_stage == Some(*stage))
        .cloned()
    })
    .collect();
  stage_reps.sort_by(entry_slowest_first);

  let mut hotspot_categories_present: BTreeSet<HotspotCategory> = BTreeSet::new();
  for entry in &ok {
    hotspot_categories_present.insert(HotspotCategory::from_hotspot(entry.hotspot.as_deref()));
  }
  let desired_ok_slots = hotspot_categories_present.len();

  // Reserve enough slots for one OK page per hotspot category when possible, but do not reserve
  // more than we can fit alongside the timeout-stage representatives.
  let max_ok_slots = count.saturating_sub(stage_reps.len().min(count));
  let ok_slots = desired_ok_slots.min(max_ok_slots);
  let timeout_slots = count.saturating_sub(ok_slots);

  let mut selected_timeouts = Vec::new();
  let mut selected_timeout_names = BTreeSet::new();
  for entry in stage_reps.into_iter().take(timeout_slots) {
    selected_timeout_names.insert(entry.name.clone());
    selected_timeouts.push(entry);
  }
  for entry in &timeouts {
    if selected_timeouts.len() >= timeout_slots {
      break;
    }
    if selected_timeout_names.insert(entry.name.clone()) {
      selected_timeouts.push(entry.clone());
    }
  }
  selected_timeouts.sort_by(entry_slowest_first);

  let remaining_slots = count.saturating_sub(selected_timeouts.len());
  if remaining_slots == 0 {
    return selected_timeouts;
  }

  let mut ok_reps_by_category: BTreeMap<HotspotCategory, ProgressEntry> = BTreeMap::new();
  for entry in &ok {
    let category = HotspotCategory::from_hotspot(entry.hotspot.as_deref());
    ok_reps_by_category
      .entry(category)
      .or_insert_with(|| entry.clone());
  }
  let mut ok_reps: Vec<ProgressEntry> = ok_reps_by_category.into_values().collect();
  ok_reps.sort_by(entry_slowest_first);

  let mut selected_ok = Vec::new();
  let mut selected_ok_names = BTreeSet::new();
  for entry in ok_reps.into_iter().take(remaining_slots) {
    selected_ok_names.insert(entry.name.clone());
    selected_ok.push(entry);
  }
  for entry in &ok {
    if selected_ok.len() >= remaining_slots {
      break;
    }
    if selected_ok_names.insert(entry.name.clone()) {
      selected_ok.push(entry.clone());
    }
  }
  selected_ok.sort_by(entry_slowest_first);

  selected_timeouts.extend(selected_ok);
  selected_timeouts.truncate(count);
  selected_timeouts
}

fn update_manifest(
  mut existing: PagesetTimeoutManifest,
  selected: &[ProgressEntry],
) -> PagesetTimeoutManifest {
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
        existing.clone()
      } else {
        PagesetTimeoutFixture {
          name: entry.name.clone(),
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
    repo_root().join("tests/fixtures/pageset_timeouts_progress")
  }

  #[test]
  fn selects_timeouts_then_slowest_ok_pages() {
    let entries = read_progress_entries(&fixture_progress_dir()).expect("read fixture progress");
    let selected = select_pages(&entries, 3, PagesetTimeoutSelectionStrategy::Slowest);
    let names: Vec<&str> = selected.iter().map(|e| e.name.as_str()).collect();
    assert_eq!(
      names,
      vec!["timeout-a.test", "timeout-b.test", "slow-ok.test"]
    );
  }

  #[test]
  fn rewrites_manifest_deterministically_preserving_schema_version_and_existing_fixtures() {
    let tempdir = TempDir::new().expect("tempdir");
    let manifest_path = tempdir.path().join("pageset_timeouts.json");
    let original = json!({
      "schema_version": 99,
      "default_budget_ms": 7777.0,
      "fixtures": [
        { "name": "timeout-b.test", "viewport": [800, 600], "dpr": 2.0, "media": "print", "budget_ms": 1234.0 }
      ]
    });
    fs::write(
      &manifest_path,
      serde_json::to_string_pretty(&original).unwrap(),
    )
    .unwrap();

    let existing = load_manifest(&manifest_path).expect("load manifest");
    let entries = read_progress_entries(&fixture_progress_dir()).expect("read fixture progress");
    let selected = select_pages(&entries, 3, PagesetTimeoutSelectionStrategy::Slowest);
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
        { "name": "timeout-a.test", "viewport": [1200, 800], "dpr": 1.0, "media": "screen", "budget_ms": 7777.0 },
        { "name": "timeout-b.test", "viewport": [800, 600], "dpr": 2.0, "media": "print", "budget_ms": 1234.0 },
        { "name": "slow-ok.test", "viewport": [1200, 800], "dpr": 1.0, "media": "screen", "budget_ms": 7777.0 }
      ]
    });
    assert_eq!(reparsed, expected);
  }

  fn mk_entry(
    name: &str,
    status: ProgressStatus,
    total_ms: f64,
    timeout_stage: Option<ProgressStage>,
    hotspot: Option<&str>,
  ) -> ProgressEntry {
    ProgressEntry {
      name: name.to_string(),
      url: format!("https://{name}/"),
      status,
      total_ms,
      timeout_stage,
      hotspot: hotspot.map(|s| s.to_string()),
    }
  }

  #[test]
  fn coverage_strategy_picks_one_timeout_per_stage_when_available() {
    let entries = vec![
      mk_entry(
        "dom-slowest",
        ProgressStatus::Timeout,
        5000.0,
        Some(ProgressStage::DomParse),
        None,
      ),
      mk_entry(
        "dom-fast",
        ProgressStatus::Timeout,
        4000.0,
        Some(ProgressStage::DomParse),
        None,
      ),
      mk_entry(
        "css-timeout",
        ProgressStatus::Timeout,
        4900.0,
        Some(ProgressStage::Css),
        None,
      ),
      mk_entry(
        "cascade-timeout",
        ProgressStatus::Timeout,
        4800.0,
        Some(ProgressStage::Cascade),
        None,
      ),
      mk_entry(
        "layout-timeout",
        ProgressStatus::Timeout,
        4700.0,
        Some(ProgressStage::Layout),
        None,
      ),
      mk_entry(
        "paint-timeout",
        ProgressStatus::Timeout,
        4600.0,
        Some(ProgressStage::Paint),
        None,
      ),
    ];

    let selected = select_pages(&entries, 5, PagesetTimeoutSelectionStrategy::Coverage);
    let names: Vec<&str> = selected.iter().map(|e| e.name.as_str()).collect();
    assert_eq!(
      names,
      vec![
        "dom-slowest",
        "css-timeout",
        "cascade-timeout",
        "layout-timeout",
        "paint-timeout"
      ]
    );
  }

  #[test]
  fn coverage_strategy_is_deterministic() {
    let entries = vec![
      mk_entry(
        "timeout-layout",
        ProgressStatus::Timeout,
        5000.0,
        Some(ProgressStage::Layout),
        None,
      ),
      mk_entry(
        "timeout-css",
        ProgressStatus::Timeout,
        5000.0,
        Some(ProgressStage::Css),
        None,
      ),
      mk_entry(
        "slow-ok-paint",
        ProgressStatus::Ok,
        4500.0,
        None,
        Some("paint"),
      ),
      mk_entry("slow-ok-css", ProgressStatus::Ok, 4400.0, None, Some("css")),
    ];

    let a = select_pages(&entries, 3, PagesetTimeoutSelectionStrategy::Coverage);
    let b = select_pages(&entries, 3, PagesetTimeoutSelectionStrategy::Coverage);
    let a_names: Vec<&str> = a.iter().map(|e| e.name.as_str()).collect();
    let b_names: Vec<&str> = b.iter().map(|e| e.name.as_str()).collect();
    assert_eq!(a_names, b_names);
  }

  #[test]
  fn parses_progress_entries_missing_hotspot_and_timeout_stage() {
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
    assert_eq!(parsed[0].timeout_stage, None);
    assert_eq!(parsed[0].hotspot, None);
  }
}
