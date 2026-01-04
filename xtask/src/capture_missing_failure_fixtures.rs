use anyhow::{anyhow, bail, Context, Result};
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct CaptureMissingFailureFixturesArgs {
  pub progress_dir: PathBuf,
  pub fixtures_root: PathBuf,
  pub bundle_out_dir: PathBuf,
  pub asset_cache_dir: PathBuf,
  pub user_agent: Option<String>,
  pub accept_language: Option<String>,
  pub viewport: Option<String>,
  pub dpr: Option<String>,
  pub allow_missing_resources: bool,
  pub overwrite: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommandSpec {
  pub program: String,
  pub args: Vec<String>,
}

impl CommandSpec {
  pub fn to_command(&self) -> std::process::Command {
    let mut cmd = std::process::Command::new(&self.program);
    cmd.args(&self.args);
    cmd
  }
}

#[derive(Debug, Clone)]
pub struct CaptureMissingFailureFixturePlan {
  pub stem: String,
  pub bundle_path: PathBuf,
  pub bundle_command: CommandSpec,
  pub import_command: CommandSpec,
}

#[derive(Debug, Clone)]
pub struct CaptureMissingFailureFixturesPlan {
  pub failing_pages_total: usize,
  pub fixtures_already_present: usize,
  pub captures: Vec<CaptureMissingFailureFixturePlan>,
}

#[derive(Debug, Deserialize)]
struct ProgressStatus {
  status: String,
}

pub fn plan_capture_missing_failure_fixtures(
  args: &CaptureMissingFailureFixturesArgs,
) -> Result<CaptureMissingFailureFixturesPlan> {
  if !args.progress_dir.is_dir() {
    bail!(
      "progress directory {} does not exist",
      args.progress_dir.display()
    );
  }

  let mut failing_stems = Vec::new();
  for entry in fs::read_dir(&args.progress_dir)
    .with_context(|| format!("read {}", args.progress_dir.display()))?
  {
    let entry = entry.context("read progress directory entry")?;
    if !entry
      .file_type()
      .with_context(|| format!("read file type {}", entry.path().display()))?
      .is_file()
    {
      continue;
    }

    let path = entry.path();
    if path.extension().and_then(|ext| ext.to_str()) != Some("json") {
      continue;
    }

    let stem = path
      .file_stem()
      .and_then(|stem| stem.to_str())
      .ok_or_else(|| anyhow!("progress file name is not valid UTF-8: {}", path.display()))?
      .to_string();

    let raw =
      fs::read_to_string(&path).with_context(|| format!("read {}", path.display()))?;
    let parsed: ProgressStatus =
      serde_json::from_str(&raw).with_context(|| format!("parse {}", path.display()))?;
    if parsed.status != "ok" {
      failing_stems.push(stem);
    }
  }

  failing_stems.sort();

  let failing_pages_total = failing_stems.len();
  let mut fixtures_already_present = 0usize;
  let mut captures = Vec::new();

  for stem in failing_stems {
    let index_path = fixture_index_path(&args.fixtures_root, &stem);
    if index_path.is_file() {
      fixtures_already_present += 1;
      continue;
    }

    let bundle_path = args.bundle_out_dir.join(format!("{stem}.tar"));
    captures.push(CaptureMissingFailureFixturePlan {
      stem: stem.clone(),
      bundle_path: bundle_path.clone(),
      bundle_command: build_bundle_page_cache_command(&stem, &bundle_path, args),
      import_command: build_import_page_fixture_command(&stem, &bundle_path, args),
    });
  }

  Ok(CaptureMissingFailureFixturesPlan {
    failing_pages_total,
    fixtures_already_present,
    captures,
  })
}

fn fixture_index_path(fixtures_root: &Path, stem: &str) -> PathBuf {
  fixtures_root.join(stem).join("index.html")
}

fn build_bundle_page_cache_command(
  stem: &str,
  bundle_path: &Path,
  args: &CaptureMissingFailureFixturesArgs,
) -> CommandSpec {
  let bundle_path = bundle_path.to_string_lossy().to_string();
  let cache_dir = args.asset_cache_dir.to_string_lossy().to_string();

  let mut cmd = vec![
    "run".to_string(),
    "--release".to_string(),
    "--features".to_string(),
    "disk_cache".to_string(),
    "--bin".to_string(),
    "bundle_page".to_string(),
    "--".to_string(),
    "cache".to_string(),
    stem.to_string(),
    "--out".to_string(),
    bundle_path,
    "--asset-cache-dir".to_string(),
    cache_dir,
  ];

  if args.allow_missing_resources {
    cmd.push("--allow-missing".to_string());
  }
  if let Some(user_agent) = args.user_agent.as_deref() {
    cmd.push("--user-agent".to_string());
    cmd.push(user_agent.to_string());
  }
  if let Some(accept_language) = args.accept_language.as_deref() {
    cmd.push("--accept-language".to_string());
    cmd.push(accept_language.to_string());
  }
  if let Some(viewport) = args.viewport.as_deref() {
    cmd.push("--viewport".to_string());
    cmd.push(viewport.to_string());
  }
  if let Some(dpr) = args.dpr.as_deref() {
    cmd.push("--dpr".to_string());
    cmd.push(dpr.to_string());
  }

  CommandSpec {
    program: "cargo".to_string(),
    args: cmd,
  }
}

fn build_import_page_fixture_command(
  stem: &str,
  bundle_path: &Path,
  args: &CaptureMissingFailureFixturesArgs,
) -> CommandSpec {
  let bundle_path = bundle_path.to_string_lossy().to_string();
  let fixtures_root = args.fixtures_root.to_string_lossy().to_string();

  let mut cmd = vec![
    "xtask".to_string(),
    "import-page-fixture".to_string(),
    bundle_path,
    stem.to_string(),
    "--output-root".to_string(),
    fixtures_root,
  ];

  if args.overwrite {
    cmd.push("--overwrite".to_string());
  }
  if args.allow_missing_resources {
    cmd.push("--allow-missing".to_string());
  }

  CommandSpec {
    program: "cargo".to_string(),
    args: cmd,
  }
}

