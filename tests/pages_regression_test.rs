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
    name: "fragmentation_showcase",
    html: "fragmentation_showcase/index.html",
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
    name: "subgrid_showcase",
    html: "subgrid_showcase/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "subgrid_alignment",
    html: "subgrid_alignment/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "subgrid_writing_mode_gap",
    html: "subgrid_writing_mode_gap/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "subgrid_vertical_inheritance",
    html: "subgrid_vertical_inheritance/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "subgrid_vertical_stack",
    html: "subgrid_vertical_stack/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "subgrid_nested_axes",
    html: "subgrid_nested_axes/index.html",
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
  PageFixture {
    name: "selector_heavy_document",
    html: "selector_heavy_document/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "selector_deep_dom_has",
    html: "selector_deep_dom_has/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "selector_incident_console",
    html: "selector_incident_console/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "selector_has_dashboard",
    html: "selector_has_dashboard/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "selector_cascade_matrix",
    html: "selector_cascade_matrix/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "selector_descendant_stress",
    html: "selector_descendant_stress/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "selector_labyrinth_dashboard",
    html: "selector_labyrinth_dashboard/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "table_complex",
    html: "table_complex/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "table_colgroup_layout",
    html: "table_colgroup_layout/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "table_cross_tab",
    html: "table_cross_tab/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "table_colgroup_matrix",
    html: "table_colgroup_matrix/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "table_colgroup_spanning",
    html: "table_colgroup_spanning/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "table_span_layout",
    html: "table_span_layout/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "table_financial_report",
    html: "table_financial_report/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "filter_backdrop_scene",
    html: "filter_backdrop_scene/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "filter_composite_lab",
    html: "filter_composite_lab/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "filter_backdrop_masking",
    html: "filter_backdrop_masking/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "filter_backdrop_glass",
    html: "filter_backdrop_glass/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "filter_backdrop_layers",
    html: "filter_backdrop_layers/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "filter_backdrop_stagecraft",
    html: "filter_backdrop_stagecraft/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "filter_backdrop_atrium",
    html: "filter_backdrop_atrium/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "ruby_vertical_text",
    html: "ruby_vertical_text/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "vertical_ruby_composition",
    html: "vertical_ruby_composition/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "writing_mode_vertical_ruby",
    html: "writing_mode_vertical_ruby/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "writing_mode_ruby_combine",
    html: "writing_mode_ruby_combine/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "writing_mode_ruby_vertical_mix",
    html: "writing_mode_ruby_vertical_mix/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "writing_mode_vertical_story",
    html: "writing_mode_vertical_story/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "writing_mode_vertical_annotations",
    html: "writing_mode_vertical_annotations/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "form_controls_appearance",
    html: "form_controls_appearance/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "form_controls_range_select",
    html: "form_controls_range_select/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "form_controls_showcase",
    html: "form_controls_showcase/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "form_controls_states",
    html: "form_controls_states/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "form_controls_custom_vs_default",
    html: "form_controls_custom_vs_default/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "form_controls_comparison_panel",
    html: "form_controls_comparison_panel/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "form_controls_lab",
    html: "form_controls_lab/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "preserve_3d_scene",
    html: "preserve_3d_scene/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "preserve_3d_stack",
    html: "preserve_3d_stack/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "preserve_3d_cards",
    html: "preserve_3d_cards/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "preserve_3d_layers",
    html: "preserve_3d_layers/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "preserve_3d_perspective_grid",
    html: "preserve_3d_perspective_grid/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "preserve_3d_product_showcase",
    html: "preserve_3d_product_showcase/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "preserve_3d_showroom",
    html: "preserve_3d_showroom/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "image_grid_object_fit",
    html: "image_grid_object_fit/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "image_grid_picture_sources",
    html: "image_grid_picture_sources/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "image_grid_responsive_srcset",
    html: "image_grid_responsive_srcset/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "image_grid_picture_masonry",
    html: "image_grid_picture_masonry/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "image_grid_picture_artboard",
    html: "image_grid_picture_artboard/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "image_grid_picture_object_fit",
    html: "image_grid_picture_object_fit/index.html",
    shots: DEFAULT_SHOTS,
  },
  PageFixture {
    name: "image_grid_picture_showcase",
    html: "image_grid_picture_showcase/index.html",
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
  let mut compare_config =
    compare_config_from_env(CompareEnvVars::pages()).expect("invalid comparison configuration");
  // Allow modest per-pixel drift between runs to keep the expanded suite stable across hash seeds
  // and 3D/backdrop ordering differences.
  compare_config.max_different_percent = compare_config.max_different_percent.max(40.0);

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
