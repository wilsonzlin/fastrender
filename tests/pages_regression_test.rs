mod r#ref;

use fastrender::image_output::{encode_image, OutputFormat};
use fastrender::style::media::MediaType;
use fastrender::{FastRender, RenderOptions};
use r#ref::image_compare::{compare_config_from_env, compare_pngs, CompareEnvVars};
use r#ref::CompareConfig;
use std::fs;
use std::path::{Path, PathBuf};
use std::thread;
use url::Url;

#[derive(Clone, Copy)]
struct PageShot {
  label: &'static str,
  viewport: (u32, u32),
  dpr: f32,
  media: MediaType,
}

impl PageShot {
  fn golden_name(&self, page: &str) -> String {
    if self.label == "default" {
      page.to_string()
    } else {
      format!("{}_{}", page, self.label)
    }
  }
}

struct PageFixture {
  name: &'static str,
  html: &'static str,
  shots: &'static [PageShot],
}

const DEFAULT_SHOT: PageShot = PageShot {
  label: "default",
  viewport: (1040, 1240),
  dpr: 1.0,
  media: MediaType::Screen,
};

const PRINT_SHOT: PageShot = PageShot {
  label: "print",
  viewport: (920, 1180),
  dpr: 1.0,
  media: MediaType::Print,
};

const DEFAULT_SHOTS: &[PageShot] = &[DEFAULT_SHOT];
const PRINT_SHOTS: &[PageShot] = &[PRINT_SHOT];

const PAGE_FIXTURES: &[PageFixture] = &[
  PageFixture {
    name: "flex_dashboard",
    html: "flex_dashboard/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "grid_news",
    html: "grid_news/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "table_financial",
    html: "table_financial/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "multicol_article",
    html: "multicol_article/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "paginated_report",
    html: "paginated_report/index.html",
    shots: PRINT_SHOTS,
  },
  PageFixture {
    name: "mask_filter_showcase",
    html: "mask_filter_showcase/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "svg_embed",
    html: "svg_embed/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "writing_modes",
    html: "writing_modes/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "form_controls",
    html: "form_controls/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "positioned_badge_regression",
    html: "positioned_badge_regression/index.html",
    shots: DEFAULT_SHOTS,
  },
];

fn fixtures_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/pages/fixtures")
}

fn golden_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/pages/golden")
}

fn diff_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/pages_diffs")
}

fn golden_path(name: &str) -> PathBuf {
  golden_dir().join(format!("{name}.png"))
}

fn should_update_goldens() -> bool {
  std::env::var("UPDATE_PAGES_GOLDEN").is_ok()
}

fn base_url_for(html_path: &Path) -> Result<String, String> {
  let dir = html_path
    .parent()
    .ok_or_else(|| format!("No parent directory for {}", html_path.display()))?;
  Url::from_directory_path(dir)
    .map_err(|_| format!("Failed to build file:// base URL for {}", dir.display()))
    .map(|url| url.to_string())
}

fn render_page(renderer: &mut FastRender, html: &str, shot: &PageShot) -> Result<Vec<u8>, String> {
  let options = RenderOptions::new()
    .with_viewport(shot.viewport.0, shot.viewport.1)
    .with_device_pixel_ratio(shot.dpr)
    .with_media_type(shot.media);

  let pixmap = renderer
    .render_html_with_options(html, options)
    .map_err(|e| format!("Render failed: {:?}", e))?;
  encode_image(&pixmap, OutputFormat::Png).map_err(|e| format!("Encode failed: {:?}", e))
}

fn run_fixture(fixture: &PageFixture, compare_config: &CompareConfig) -> Result<(), String> {
  let html_path = fixtures_dir().join(fixture.html);
  let html = fs::read_to_string(&html_path)
    .map_err(|e| format!("Failed to read {}: {}", html_path.display(), e))?;
  let base_url = base_url_for(&html_path)?;

  let mut renderer = FastRender::builder()
    .base_url(base_url)
    .build()
    .map_err(|e| format!("Failed to create renderer: {:?}", e))?;

  for shot in fixture.shots {
    let rendered = render_page(&mut renderer, &html, shot)?;
    let golden_name = shot.golden_name(fixture.name);
    let golden_path = golden_path(&golden_name);

    if should_update_goldens() {
      fs::create_dir_all(golden_dir()).map_err(|e| {
        format!(
          "Failed to create golden dir {}: {}",
          golden_dir().display(),
          e
        )
      })?;
      fs::write(&golden_path, &rendered)
        .map_err(|e| format!("Failed to write golden {}: {}", golden_path.display(), e))?;
      eprintln!("Updated golden for {}", golden_name);
      continue;
    }

    let golden = fs::read(&golden_path).map_err(|e| {
      format!(
        "Missing golden {} ({}). Set UPDATE_PAGES_GOLDEN=1 to regenerate. Error: {}",
        golden_name,
        golden_path.display(),
        e
      )
    })?;

    compare_pngs(
      &golden_name,
      &rendered,
      &golden,
      compare_config,
      &diff_dir(),
    )?;
  }

  Ok(())
}

#[test]
fn pages_regression_suite() {
  let compare_config =
    compare_config_from_env(CompareEnvVars::pages()).expect("invalid comparison configuration");

  thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(move || {
      for fixture in PAGE_FIXTURES {
        run_fixture(fixture, &compare_config)
          .unwrap_or_else(|e| panic!("Page '{}' failed: {}", fixture.name, e));
      }
    })
    .unwrap()
    .join()
    .unwrap();
}

#[test]
fn page_fixtures_present() {
  for fixture in PAGE_FIXTURES {
    let path = fixtures_dir().join(fixture.html);
    assert!(path.exists(), "Fixture HTML missing: {}", path.display());
  }
}
