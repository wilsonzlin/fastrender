#[path = "../ref/mod.rs"]
mod r#ref;

use fastrender::debug::runtime::RuntimeToggles;
use fastrender::image_output::{encode_image, OutputFormat};
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::{DiagnosticsLevel, FastRender, RenderOptions};
use r#ref::image_compare::{compare_config_from_env, compare_pngs, CompareEnvVars};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

const HTML_PATH: &str = "tests/fixtures/html/mask_parallel_translation.html";
const GOLDEN_PATH: &str = "tests/fixtures/golden/mask_parallel_translation.png";
const DIFF_DIR: &str = "target/mask_parallel_translation_diffs";
const VIEWPORT: (u32, u32) = (128, 128);

fn render_fixture() -> Vec<u8> {
  // This regression specifically targets the display-list renderer's parallel tiled rasterization
  // path. The per-tile renderers translate their canvases, so mask rendering must respect that
  // translation or the masked content disappears in non-origin tiles.
  let toggles = RuntimeToggles::from_map(HashMap::from([(
    "FASTR_PAINT_BACKEND".to_string(),
    "display_list".to_string(),
  )]));

  let html = fs::read_to_string(HTML_PATH).expect("read fixture html");
  let mut renderer = FastRender::new().expect("renderer");
  let options = RenderOptions::new()
    .with_viewport(VIEWPORT.0, VIEWPORT.1)
    .with_diagnostics_level(DiagnosticsLevel::Basic)
    .with_runtime_toggles(toggles)
    .with_paint_parallelism(PaintParallelism {
      tile_size: 32,
      min_tiles: 1,
      min_display_items: 1,
      ..PaintParallelism::enabled()
    });
  let report = renderer
    .render_html_with_diagnostics(&html, options)
    .expect("render html");

  let parallel_tasks = report
    .diagnostics
    .stats
    .as_ref()
    .and_then(|stats| stats.paint.parallel_tasks)
    .unwrap_or(0);
  assert!(
    parallel_tasks > 0,
    "expected parallel tiled rasterization to run, got parallel_tasks={parallel_tasks}"
  );

  // Sanity checks so the committed golden can't accidentally encode a fully blank/masked-out
  // render. The target element sits at (80,80) with a radial mask that makes its center
  // transparent, so these sample points should be inside the opaque area vs. the hole.
  let filled = report
    .pixmap
    .pixel(84, 84)
    .expect("sample pixel inside masked element");
  assert!(
    filled.red() > 200 && filled.blue() > 200 && filled.green() < 50 && filled.alpha() > 200,
    "expected masked element to render magenta at (84,84), got rgba=({}, {}, {}, {})",
    filled.red(),
    filled.green(),
    filled.blue(),
    filled.alpha(),
  );
  let hole = report
    .pixmap
    .pixel(100, 100)
    .expect("sample pixel inside mask hole");
  assert!(
    hole.red() > 240 && hole.green() > 240 && hole.blue() > 240 && hole.alpha() > 240,
    "expected mask hole to show white background at (100,100), got rgba=({}, {}, {}, {})",
    hole.red(),
    hole.green(),
    hole.blue(),
    hole.alpha(),
  );

  encode_image(&report.pixmap, OutputFormat::Png).expect("encode png")
}

fn should_update_golden() -> bool {
  std::env::var("UPDATE_MASK_PARALLEL_TRANSLATION_GOLDEN").is_ok()
}

#[test]
fn mask_parallel_translation_matches_golden() {
  let compare_config =
    compare_config_from_env(CompareEnvVars::fixtures()).expect("compare configuration");
  let rendered = render_fixture();
  let golden_path = Path::new(GOLDEN_PATH);
  let diff_dir = PathBuf::from(DIFF_DIR);

  if should_update_golden() {
    fs::write(golden_path, &rendered).expect("write golden");
    return;
  }

  let golden = fs::read(golden_path).unwrap_or_else(|e| {
    panic!(
      "Missing golden {} ({}): {}",
      golden_path.file_name().unwrap().to_string_lossy(),
      golden_path.display(),
      e
    )
  });

  compare_pngs(
    golden_path
      .file_name()
      .unwrap_or_default()
      .to_string_lossy()
      .as_ref(),
    &rendered,
    &golden,
    &compare_config,
    &diff_dir,
  )
  .unwrap_or_else(|e| panic!("{}", e));
}
