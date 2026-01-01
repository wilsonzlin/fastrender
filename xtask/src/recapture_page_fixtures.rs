use anyhow::{bail, Context, Result};
use clap::{ArgAction, Args, ValueEnum};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const DEFAULT_MANIFEST: &str = "tests/pages/pageset_guardrails.json";
const DEFAULT_FIXTURES_ROOT: &str = "tests/pages/fixtures";
const DEFAULT_BUNDLE_OUT_DIR: &str = "target/page-fixture-bundles";

const DEFAULT_VIEWPORT: [u32; 2] = [1200, 800];
const DEFAULT_DPR: f32 = 1.0;

#[derive(Args, Debug)]
pub struct RecapturePageFixturesArgs {
  /// Path to a fixture manifest (defaults to the pageset-guardrails manifest).
  ///
  /// Note: the legacy `tests/pages/pageset_timeouts.json` file is kept in sync for backwards
  /// compatibility.
  ///
  /// The manifest must contain a `fixtures` array with at least `name` fields. Optional fields:
  /// - `url` (preferred; used directly when present)
  /// - `viewport` (defaults to 1200x800 if absent)
  /// - `dpr` (defaults to 1.0 if absent)
  ///
  /// When `url` is absent, the command falls back to reading `progress/pages/<name>.json`.
  #[arg(long, default_value = DEFAULT_MANIFEST)]
  pub manifest: PathBuf,

  /// Root directory containing offline fixtures.
  #[arg(long, default_value = DEFAULT_FIXTURES_ROOT)]
  pub fixtures_root: PathBuf,

  /// Directory for writing intermediate `bundle_page` archives.
  #[arg(long, default_value = DEFAULT_BUNDLE_OUT_DIR)]
  pub bundle_out_dir: PathBuf,

  /// How to capture subresources for the bundle.
  #[arg(long, value_enum, default_value_t = CaptureMode::Crawl)]
  pub capture_mode: CaptureMode,

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

  /// Per-request fetch timeout (seconds) passed to `bundle_page fetch`.
  #[arg(long)]
  pub bundle_fetch_timeout_secs: Option<u64>,

  /// Restrict subresource loads to the document origin unless allowlisted.
  #[arg(long)]
  pub same_origin_subresources: bool,

  /// Allow additional origins when blocking cross-origin subresources (repeatable).
  #[arg(long, value_name = "ORIGIN")]
  pub allow_subresource_origin: Vec<String>,

  /// Replace existing fixture directories under `--fixtures-root`.
  #[arg(long)]
  pub overwrite: bool,

  /// Replace missing resources with empty placeholder assets instead of failing the import.
  #[arg(long)]
  pub allow_missing_resources: bool,

  /// Only operate on the listed fixture names (comma-separated).
  #[arg(long, value_delimiter = ',')]
  pub only: Option<Vec<String>>,

  /// Only operate on fixtures whose `<fixture>/index.html` is missing.
  #[arg(long)]
  pub missing_only: bool,

  /// Continue after failures to collect a full report (default true).
  #[arg(long = "no-keep-going", default_value_t = true, action = ArgAction::SetFalse)]
  pub keep_going: bool,

  /// Run `bundle_page fetch` in debug mode (release by default).
  #[arg(long)]
  pub debug: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
#[clap(rename_all = "kebab_case")]
pub enum CaptureMode {
  /// Fetch and render once to discover subresources (historical behavior).
  Render,
  /// Fetch and discover subresources by parsing HTML/CSS without rendering.
  Crawl,
  /// Bundle a cached pageset entry (HTML + disk-backed assets) with no network access.
  Cache,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum RawManifest {
  Object { fixtures: Vec<RawFixture> },
  List(Vec<RawFixture>),
}

#[derive(Debug, Deserialize, Clone)]
struct RawFixture {
  name: String,
  #[serde(default)]
  url: Option<String>,
  #[serde(default)]
  viewport: Option<[u32; 2]>,
  #[serde(default)]
  dpr: Option<f32>,
}

#[derive(Debug, Clone)]
struct FixtureDefinition {
  name: String,
  url: Option<String>,
  viewport: [u32; 2],
  dpr: f32,
}

#[derive(Debug)]
struct PlannedFixture {
  fixture: FixtureDefinition,
  skip_reason: Option<String>,
}

#[derive(Debug, Deserialize)]
struct ProgressFile {
  url: String,
}

#[derive(Debug)]
enum StepResult {
  Ok,
  Skipped(String),
  Failed(String),
}

#[derive(Debug)]
struct FixtureReportRow {
  name: String,
  captured: StepResult,
  imported: StepResult,
}

pub fn run_recapture_page_fixtures(args: RecapturePageFixturesArgs) -> Result<()> {
  let fixtures = load_manifest(&args.manifest)?;
  if fixtures.is_empty() {
    bail!("manifest {} contains no fixtures", args.manifest.display());
  }

  let plan = plan_fixtures(&fixtures, &args)?;
  if plan.is_empty() {
    println!("No fixtures selected.");
    return Ok(());
  }

  fs::create_dir_all(&args.bundle_out_dir).with_context(|| {
    format!(
      "failed to create bundle output directory {}",
      args.bundle_out_dir.display()
    )
  })?;

  let mut rows: Vec<FixtureReportRow> = Vec::new();
  let mut failed = 0usize;
  let mut skipped = 0usize;
  let mut captured_ok = 0usize;
  let mut imported_ok = 0usize;

  for entry in plan {
    let name = entry.fixture.name.clone();
    if let Some(reason) = entry.skip_reason {
      skipped += 1;
      rows.push(FixtureReportRow {
        name,
        captured: StepResult::Skipped(reason.clone()),
        imported: StepResult::Skipped(reason),
      });
      continue;
    }

    let url = if args.capture_mode == CaptureMode::Cache {
      None
    } else {
      match resolve_fixture_url(&entry.fixture) {
        Ok(url) => Some(url),
        Err(err) => {
          failed += 1;
          let message = err.to_string();
          rows.push(FixtureReportRow {
            name,
            captured: StepResult::Failed(message.clone()),
            imported: StepResult::Skipped("capture failed".to_string()),
          });
          if !args.keep_going {
            break;
          }
          continue;
        }
      }
    };

    let bundle_path = args
      .bundle_out_dir
      .join(format!("{}.tar", entry.fixture.name));
    if bundle_path.exists() {
      remove_path(&bundle_path)
        .with_context(|| format!("failed to clear existing bundle {}", bundle_path.display()))?;
    }

    let capture_result = capture_bundle(&args, &entry.fixture, url.as_deref(), &bundle_path);
    let captured = match capture_result {
      Ok(()) => {
        captured_ok += 1;
        StepResult::Ok
      }
      Err(err) => StepResult::Failed(err.to_string()),
    };

    let imported = if matches!(captured, StepResult::Ok) {
      import_fixture(&args, &entry.fixture, &bundle_path)
    } else {
      StepResult::Skipped("capture failed".to_string())
    };

    if matches!(imported, StepResult::Ok) {
      imported_ok += 1;
    }

    let fixture_failed =
      matches!(captured, StepResult::Failed(_)) || matches!(imported, StepResult::Failed(_));
    let fixture_skipped = !fixture_failed
      && (matches!(captured, StepResult::Skipped(_)) || matches!(imported, StepResult::Skipped(_)));

    if fixture_failed {
      failed += 1;
    } else if fixture_skipped {
      skipped += 1;
    }

    rows.push(FixtureReportRow {
      name,
      captured,
      imported,
    });

    if failed > 0 && !args.keep_going {
      break;
    }
  }

  print_report(&rows, captured_ok, imported_ok, skipped, failed);

  if failed > 0 {
    bail!("{failed} fixture(s) failed");
  }
  Ok(())
}

fn print_report(
  rows: &[FixtureReportRow],
  captured_ok: usize,
  imported_ok: usize,
  skipped: usize,
  failed: usize,
) {
  println!();
  println!("Fixture results:");
  for row in rows {
    println!(
      "  {:<40} capture: {:<10} import: {:<10}{}",
      row.name,
      summarize_step(&row.captured),
      summarize_step(&row.imported),
      format_step_note(&row.captured, &row.imported)
    );
  }

  println!();
  println!("Summary:");
  println!("  captured  {captured_ok}");
  println!("  imported  {imported_ok}");
  println!("  skipped   {skipped}");
  println!("  failed    {failed}");
}

fn summarize_step(step: &StepResult) -> &'static str {
  match step {
    StepResult::Ok => "ok",
    StepResult::Skipped(_) => "skipped",
    StepResult::Failed(_) => "failed",
  }
}

fn format_step_note(captured: &StepResult, imported: &StepResult) -> String {
  let mut notes = Vec::new();
  match captured {
    StepResult::Skipped(reason) => notes.push(format!("capture: {reason}")),
    StepResult::Failed(reason) => notes.push(format!("capture: {reason}")),
    StepResult::Ok => {}
  }
  match imported {
    StepResult::Skipped(reason) => notes.push(format!("import: {reason}")),
    StepResult::Failed(reason) => notes.push(format!("import: {reason}")),
    StepResult::Ok => {}
  }
  if notes.is_empty() {
    String::new()
  } else {
    format!(" ({})", notes.join("; "))
  }
}

fn load_manifest(path: &Path) -> Result<Vec<FixtureDefinition>> {
  let raw = fs::read_to_string(path)
    .with_context(|| format!("failed to read fixture manifest {}", path.display()))?;
  let parsed: RawManifest =
    serde_json::from_str(&raw).with_context(|| format!("invalid JSON in {}", path.display()))?;
  let fixtures = match parsed {
    RawManifest::Object { fixtures } => fixtures,
    RawManifest::List(fixtures) => fixtures,
  };

  let mut seen: BTreeSet<String> = BTreeSet::new();
  let mut out = Vec::new();
  for fixture in fixtures {
    let name = fixture.name.trim().to_string();
    if name.is_empty() {
      bail!("fixture manifest contains an entry with an empty name");
    }
    if !seen.insert(name.clone()) {
      bail!("fixture manifest contains duplicate fixture name: {name}");
    }

    out.push(FixtureDefinition {
      name,
      url: fixture.url,
      viewport: fixture.viewport.unwrap_or(DEFAULT_VIEWPORT),
      dpr: fixture.dpr.unwrap_or(DEFAULT_DPR),
    });
  }

  Ok(out)
}

fn plan_fixtures(
  fixtures: &[FixtureDefinition],
  args: &RecapturePageFixturesArgs,
) -> Result<Vec<PlannedFixture>> {
  let fixture_map: BTreeMap<&str, &FixtureDefinition> =
    fixtures.iter().map(|f| (f.name.as_str(), f)).collect();

  if let Some(only) = &args.only {
    let mut missing = Vec::new();
    for name in only {
      if !fixture_map.contains_key(name.as_str()) {
        missing.push(name.clone());
      }
    }
    if !missing.is_empty() {
      missing.sort();
      bail!(
        "--only referenced fixture(s) not present in {}: {}",
        args.manifest.display(),
        missing.join(", ")
      );
    }
  }

  let mut selected: Vec<&FixtureDefinition> = match &args.only {
    Some(only) => only
      .iter()
      .filter_map(|name| fixture_map.get(name.as_str()).copied())
      .collect(),
    None => fixture_map.values().copied().collect(),
  };
  selected.sort_by(|a, b| a.name.cmp(&b.name));

  let mut plan = Vec::new();
  for fixture in selected {
    let index_path = args.fixtures_root.join(&fixture.name).join("index.html");
    let exists = index_path.is_file();
    let skip_reason = if args.missing_only && exists {
      Some("index.html exists (--missing-only)".to_string())
    } else {
      None
    };

    plan.push(PlannedFixture {
      fixture: fixture.clone(),
      skip_reason,
    });
  }

  Ok(plan)
}

fn resolve_fixture_url(fixture: &FixtureDefinition) -> Result<String> {
  if let Some(url) = &fixture.url {
    return Ok(url.clone());
  }

  let progress_path = crate::repo_root()
    .join("progress/pages")
    .join(format!("{}.json", fixture.name));
  let raw = fs::read_to_string(&progress_path).with_context(|| {
    format!(
      "fixture {} missing url and progress file {} could not be read",
      fixture.name,
      progress_path.display()
    )
  })?;
  let parsed: ProgressFile = serde_json::from_str(&raw).with_context(|| {
    format!(
      "fixture {} progress file {} is missing a valid url field",
      fixture.name,
      progress_path.display()
    )
  })?;
  Ok(parsed.url)
}

fn capture_bundle(
  args: &RecapturePageFixturesArgs,
  fixture: &FixtureDefinition,
  url: Option<&str>,
  bundle_path: &Path,
) -> Result<()> {
  let mut cmd = Command::new("cargo");
  cmd.arg("run").arg("--quiet");
  if !args.debug {
    cmd.arg("--release");
  }
  if args.capture_mode == CaptureMode::Cache {
    use crate::DiskCacheFeatureExt;
    cmd.apply_disk_cache_feature(true);
  }
  cmd.args(["--bin", "bundle_page", "--"]);
  match args.capture_mode {
    CaptureMode::Render => {
      let Some(url) = url else {
        bail!("capture mode render requires a url (this is a bug)");
      };
      cmd.args(["fetch", url]);
    }
    CaptureMode::Crawl => {
      let Some(url) = url else {
        bail!("capture mode crawl requires a url (this is a bug)");
      };
      cmd.args(["fetch", url]);
      cmd.arg("--no-render");
    }
    CaptureMode::Cache => {
      cmd.args(["cache", &fixture.name]);
      if args.allow_missing_resources {
        cmd.arg("--allow-missing");
      }
      if let Some(user_agent) = args.user_agent.as_deref() {
        cmd.args(["--user-agent", user_agent]);
      }
      if let Some(accept_language) = args.accept_language.as_deref() {
        cmd.args(["--accept-language", accept_language]);
      }
    }
  }
  cmd.args(["--out", bundle_path.to_string_lossy().as_ref()]);
  if let Some(secs) = args.bundle_fetch_timeout_secs {
    if args.capture_mode != CaptureMode::Cache {
      cmd.args(["--fetch-timeout-secs", &secs.to_string()]);
    }
  }
  cmd.args([
    "--viewport",
    &format!("{}x{}", fixture.viewport[0], fixture.viewport[1]),
  ]);
  cmd.args(["--dpr", &fixture.dpr.to_string()]);
  if args.same_origin_subresources {
    cmd.arg("--same-origin-subresources");
  }
  for origin in &args.allow_subresource_origin {
    cmd.args(["--allow-subresource-origin", origin]);
  }

  cmd.current_dir(crate::repo_root());
  crate::run_command(cmd).with_context(|| format!("bundle {}", fixture.name))
}

fn import_fixture(
  args: &RecapturePageFixturesArgs,
  fixture: &FixtureDefinition,
  bundle_path: &Path,
) -> StepResult {
  let index_path = args.fixtures_root.join(&fixture.name).join("index.html");
  if index_path.is_file() && !args.overwrite {
    return StepResult::Skipped("fixture exists (pass --overwrite)".to_string());
  }

  match crate::import_page_fixture::run_import_page_fixture(
    crate::import_page_fixture::ImportPageFixtureArgs {
      bundle: bundle_path.to_path_buf(),
      fixture_name: fixture.name.clone(),
      output_root: args.fixtures_root.clone(),
      overwrite: args.overwrite,
      allow_missing: args.allow_missing_resources,
      allow_http_references: false,
      legacy_rewrite: false,
      dry_run: false,
    },
  )
  .with_context(|| format!("import {}", fixture.name))
  {
    Ok(()) => StepResult::Ok,
    Err(err) => StepResult::Failed(err.to_string()),
  }
}

fn remove_path(path: &Path) -> Result<()> {
  if path.is_dir() {
    fs::remove_dir_all(path).with_context(|| format!("remove {}", path.display()))?;
  } else if path.exists() {
    fs::remove_file(path).with_context(|| format!("remove {}", path.display()))?;
  }
  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;
  use clap::Parser;
  use tempfile::TempDir;

  #[test]
  fn parses_recapture_cli_args() {
    let cli = crate::Cli::try_parse_from([
      "xtask",
      "recapture-page-fixtures",
      "--only",
      "b.test,a.test",
      "--missing-only",
      "--overwrite",
      "--allow-missing-resources",
      "--capture-mode",
      "render",
      "--bundle-fetch-timeout-secs",
      "15",
      "--no-keep-going",
    ])
    .expect("parse args");

    let crate::Commands::RecapturePageFixtures(args) = cli.command else {
      panic!("expected recapture-page-fixtures command");
    };

    assert_eq!(args.manifest, PathBuf::from(DEFAULT_MANIFEST));
    assert_eq!(args.fixtures_root, PathBuf::from(DEFAULT_FIXTURES_ROOT));
    assert_eq!(args.bundle_out_dir, PathBuf::from(DEFAULT_BUNDLE_OUT_DIR));
    assert_eq!(args.user_agent, None);
    assert_eq!(args.accept_language, None);
    assert_eq!(
      args.only,
      Some(vec!["b.test".to_string(), "a.test".to_string()])
    );
    assert!(args.missing_only);
    assert!(args.overwrite);
    assert!(args.allow_missing_resources);
    assert_eq!(args.capture_mode, CaptureMode::Render);
    assert_eq!(args.bundle_fetch_timeout_secs, Some(15));
    assert!(!args.keep_going);
  }

  #[test]
  fn selection_is_sorted_and_respects_missing_only() {
    let fixtures = vec![
      FixtureDefinition {
        name: "b".to_string(),
        url: Some("file:///b".to_string()),
        viewport: DEFAULT_VIEWPORT,
        dpr: DEFAULT_DPR,
      },
      FixtureDefinition {
        name: "a".to_string(),
        url: Some("file:///a".to_string()),
        viewport: DEFAULT_VIEWPORT,
        dpr: DEFAULT_DPR,
      },
    ];

    let temp = TempDir::new().expect("tempdir");
    let fixtures_root = temp.path().join("fixtures");
    fs::create_dir_all(fixtures_root.join("b")).unwrap();
    fs::write(fixtures_root.join("b/index.html"), "existing").unwrap();

    let args = RecapturePageFixturesArgs {
      manifest: PathBuf::from("manifest.json"),
      fixtures_root: fixtures_root.clone(),
      bundle_out_dir: temp.path().join("bundles"),
      capture_mode: CaptureMode::Crawl,
      user_agent: None,
      accept_language: None,
      bundle_fetch_timeout_secs: None,
      same_origin_subresources: false,
      allow_subresource_origin: Vec::new(),
      overwrite: false,
      allow_missing_resources: false,
      only: None,
      missing_only: true,
      keep_going: true,
      debug: true,
    };

    let plan = plan_fixtures(&fixtures, &args).expect("plan fixtures");
    assert_eq!(plan.len(), 2);
    assert_eq!(plan[0].fixture.name, "a");
    assert_eq!(plan[0].skip_reason, None);
    assert_eq!(plan[1].fixture.name, "b");
    assert_eq!(
      plan[1].skip_reason.as_deref(),
      Some("index.html exists (--missing-only)")
    );
  }

  #[test]
  fn only_filter_rejects_unknown_fixture_names() {
    let fixtures = vec![FixtureDefinition {
      name: "known".to_string(),
      url: None,
      viewport: DEFAULT_VIEWPORT,
      dpr: DEFAULT_DPR,
    }];
    let temp = TempDir::new().expect("tempdir");
    let args = RecapturePageFixturesArgs {
      manifest: PathBuf::from("manifest.json"),
      fixtures_root: temp.path().join("fixtures"),
      bundle_out_dir: temp.path().join("bundles"),
      capture_mode: CaptureMode::Crawl,
      user_agent: None,
      accept_language: None,
      bundle_fetch_timeout_secs: None,
      same_origin_subresources: false,
      allow_subresource_origin: Vec::new(),
      overwrite: false,
      allow_missing_resources: false,
      only: Some(vec!["missing".to_string()]),
      missing_only: false,
      keep_going: true,
      debug: true,
    };

    let err = plan_fixtures(&fixtures, &args).expect_err("should fail");
    let msg = err.to_string();
    assert!(msg.contains("--only referenced fixture"));
    assert!(msg.contains("missing"));
  }
}
