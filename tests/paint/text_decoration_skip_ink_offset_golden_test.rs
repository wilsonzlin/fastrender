use crate::r#ref::image_compare::{compare_config_from_env, compare_pngs, CompareEnvVars};
use fastrender::image_output::{encode_image, OutputFormat};
use fastrender::{FastRender, RenderOptions};
use std::fs;
use std::path::{Path, PathBuf};

const HTML_PATH: &str = "tests/fixtures/html/text_decoration_skip_ink_offset.html";
const GOLDEN_PATH: &str = "tests/fixtures/golden/text_decoration_skip_ink_offset.png";
const DIFF_DIR: &str = "target/text_decoration_skip_ink_offset_diffs";
const VIEWPORT: (u32, u32) = (420, 160);

fn render_fixture() -> Vec<u8> {
  // Make the output deterministic across machines (matches CI defaults).
  std::env::set_var("FASTR_USE_BUNDLED_FONTS", "1");

  let html = fs::read_to_string(HTML_PATH).expect("read html fixture");
  let mut renderer = FastRender::new().expect("renderer");
  let options = RenderOptions::new().with_viewport(VIEWPORT.0, VIEWPORT.1);
  let pixmap = renderer
    .render_html_with_options(&html, options)
    .expect("render html");
  encode_image(&pixmap, OutputFormat::Png).expect("encode png")
}

#[test]
fn text_decoration_skip_ink_offset_matches_golden() {
  let compare_config =
    compare_config_from_env(CompareEnvVars::fixtures()).expect("compare configuration");
  let rendered = render_fixture();
  let golden_path = Path::new(GOLDEN_PATH);
  let diff_dir = PathBuf::from(DIFF_DIR);

  if std::env::var("UPDATE_GOLDEN").is_ok() {
    fs::write(golden_path, &rendered).expect("write golden");
    return;
  }

  let golden = fs::read(golden_path).unwrap_or_else(|e| {
    panic!(
      "Missing golden {} ({}): {}",
      golden_path.file_name().unwrap_or_default().to_string_lossy(),
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

