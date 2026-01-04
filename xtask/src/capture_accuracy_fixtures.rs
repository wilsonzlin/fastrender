use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct CaptureAccuracyFixturesArgs {
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
  pub min_diff_percent: f64,
  pub top: usize,
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

#[derive(Debug, Clone, PartialEq)]
pub struct PagesetAccuracyPage {
  pub stem: String,
  pub url: Option<String>,
  pub diff_percent: f64,
  pub progress_path: PathBuf,
  pub fixture_index_path: PathBuf,
  pub has_fixture: bool,
}

#[derive(Debug, Clone)]
pub struct CaptureAccuracyFixturePlan {
  pub stem: String,
  pub diff_percent: f64,
  pub bundle_path: PathBuf,
  pub bundle_command: CommandSpec,
  pub import_command: CommandSpec,
}

#[derive(Debug, Clone)]
pub struct CaptureAccuracyFixturesPlan {
  pub ok_accuracy_pages_total: usize,
  pub selected_pages_total: usize,
  pub fixtures_already_present: usize,
  pub captures: Vec<CaptureAccuracyFixturePlan>,
}

pub fn plan_capture_accuracy_fixtures(
  args: &CaptureAccuracyFixturesArgs,
) -> Result<CaptureAccuracyFixturesPlan> {
  let mut pages = read_ok_accuracy_pages(&args.progress_dir, &args.fixtures_root)?;
  let ok_accuracy_pages_total = pages.len();

  pages.sort_by(|a, b| {
    b.diff_percent
      .total_cmp(&a.diff_percent)
      .then_with(|| a.stem.cmp(&b.stem))
  });

  let mut selected: Vec<PagesetAccuracyPage> = Vec::new();
  for (idx, page) in pages.iter().enumerate() {
    if idx < args.top || page.diff_percent >= args.min_diff_percent {
      selected.push(page.clone());
    }
  }

  let selected_pages_total = selected.len();
  let fixtures_already_present = selected.iter().filter(|p| p.has_fixture).count();

  let mut captures = Vec::new();
  for page in selected {
    if page.has_fixture && !args.overwrite {
      continue;
    }
    let bundle_path = args.bundle_out_dir.join(format!("{}.tar", page.stem));
    captures.push(CaptureAccuracyFixturePlan {
      stem: page.stem.clone(),
      diff_percent: page.diff_percent,
      bundle_path: bundle_path.clone(),
      bundle_command: build_bundle_page_cache_command(&page.stem, &bundle_path, args),
      import_command: build_import_page_fixture_command(&page.stem, &bundle_path, args),
    });
  }

  Ok(CaptureAccuracyFixturesPlan {
    ok_accuracy_pages_total,
    selected_pages_total,
    fixtures_already_present,
    captures,
  })
}

fn read_ok_accuracy_pages(
  progress_pages_dir: &Path,
  fixtures_root: &Path,
) -> Result<Vec<PagesetAccuracyPage>> {
  let mut pages = Vec::new();

  for entry in fs::read_dir(progress_pages_dir).with_context(|| {
    format!(
      "failed to read progress directory {}",
      progress_pages_dir.display()
    )
  })? {
    let entry = entry.with_context(|| {
      format!(
        "failed to read directory entry in {}",
        progress_pages_dir.display()
      )
    })?;
    let path = entry.path();

    if !path.is_file() {
      continue;
    }
    if path.extension().and_then(|ext| ext.to_str()) != Some("json") {
      continue;
    }

    let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
      continue;
    };
    let contents =
      fs::read_to_string(&path).with_context(|| format!("failed to read {}", path.display()))?;
    let json: serde_json::Value = serde_json::from_str(&contents)
      .with_context(|| format!("failed to parse {}", path.display()))?;

    let status = json.get("status").and_then(|v| v.as_str()).unwrap_or("ok");
    if status != "ok" {
      continue;
    }

    let diff_percent = json
      .get("accuracy")
      .and_then(|a| a.get("diff_percent"))
      .and_then(|v| v.as_f64());
    let Some(diff_percent) = diff_percent.filter(|v| v.is_finite()) else {
      continue;
    };

    let url = json
      .get("url")
      .and_then(|v| v.as_str())
      .map(|s| s.to_string());
    let fixture_index_path = fixtures_root.join(stem).join("index.html");
    let has_fixture = fixture_index_path.is_file();

    pages.push(PagesetAccuracyPage {
      stem: stem.to_string(),
      url,
      diff_percent,
      progress_path: path,
      fixture_index_path,
      has_fixture,
    });
  }

  Ok(pages)
}

fn build_bundle_page_cache_command(
  stem: &str,
  bundle_path: &Path,
  args: &CaptureAccuracyFixturesArgs,
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
  args: &CaptureAccuracyFixturesArgs,
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
