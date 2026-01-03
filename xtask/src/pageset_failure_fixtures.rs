use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PagesetFailurePage {
  pub stem: String,
  pub status: String,
  pub url: Option<String>,
  pub progress_path: PathBuf,
  pub fixture_index_path: PathBuf,
  pub has_fixture: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PagesetFailureFixturesPlan {
  pub failing_pages: Vec<PagesetFailurePage>,
  pub missing_fixtures: Vec<PagesetFailurePage>,
  pub existing_fixtures: Vec<PagesetFailurePage>,
}

/// Scan pageset progress artifacts and determine which failing pages are missing offline fixtures.
///
/// The repository's "offline repro" contract is:
/// - any `progress/pages/<stem>.json` entry whose `status != ok` should have an offline fixture at
///   `tests/pages/fixtures/<stem>/index.html`.
pub fn plan_missing_failure_fixtures(
  progress_pages_dir: &Path,
  fixtures_root: &Path,
) -> Result<PagesetFailureFixturesPlan> {
  let mut failing_pages = Vec::new();

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
    let status = json
      .get("status")
      .and_then(|v| v.as_str())
      .unwrap_or("ok")
      .to_string();
    if status == "ok" {
      continue;
    }

    let url = json.get("url").and_then(|v| v.as_str()).map(|s| s.to_string());
    let fixture_index_path = fixtures_root.join(stem).join("index.html");
    let has_fixture = fixture_index_path.is_file();

    failing_pages.push(PagesetFailurePage {
      stem: stem.to_string(),
      status,
      url,
      progress_path: path,
      fixture_index_path,
      has_fixture,
    });
  }

  failing_pages.sort_by(|a, b| a.stem.cmp(&b.stem));

  let mut existing_fixtures = Vec::new();
  let mut missing_fixtures = Vec::new();
  for page in &failing_pages {
    if page.has_fixture {
      existing_fixtures.push(page.clone());
    } else {
      missing_fixtures.push(page.clone());
    }
  }

  Ok(PagesetFailureFixturesPlan {
    failing_pages,
    missing_fixtures,
    existing_fixtures,
  })
}

