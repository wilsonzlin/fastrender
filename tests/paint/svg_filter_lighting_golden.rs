#[path = "../ref/mod.rs"]
mod r#ref;

use fastrender::image_output::{encode_image, OutputFormat};
use fastrender::{FastRender, RenderOptions};
use r#ref::image_compare::{compare_config_from_env, compare_pngs, CompareEnvVars};
use std::fs;
use std::path::{Path, PathBuf};

const HTML_PATH: &str = "tests/fixtures/html/svg_filter_lighting.html";
const GOLDEN_PATH: &str = "tests/fixtures/golden/svg_filter_lighting.png";
const DIFF_DIR: &str = "target/svg_filter_lighting_diffs";
const VIEWPORT: (u32, u32) = (210, 90);

fn render_fixture() -> Vec<u8> {
  let mut renderer = FastRender::new().expect("renderer");
  let options = RenderOptions::new().with_viewport(VIEWPORT.0, VIEWPORT.1);
  let pixmap = renderer
    .render_html_with_options(
      &fs::read_to_string(HTML_PATH).expect("read lighting fixture"),
      options,
    )
    .expect("render lighting html");
  encode_image(&pixmap, OutputFormat::Png).expect("encode png")
}

fn should_update_golden() -> bool {
  std::env::var("UPDATE_SVG_FILTER_LIGHTING_GOLDEN").is_ok()
}

#[test]
fn svg_filter_lighting_matches_golden() {
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
