#[path = "../ref/mod.rs"]
mod r#ref;

use fastrender::image_output::{encode_image, OutputFormat};
use fastrender::paint::display_list::DisplayItem;
use fastrender::text::font_db::FontConfig;
use fastrender::{FastRender, RenderArtifactRequest, RenderArtifacts, RenderOptions};
use r#ref::image_compare::{compare_config_from_env, compare_pngs, CompareEnvVars};
use std::fs;
use std::path::{Path, PathBuf};

const HTML_PATH: &str = "tests/fixtures/html/text_shadow_color_emoji.html";
const GOLDEN_DIR: &str = "tests/fixtures/golden";
const GOLDEN_NAME: &str = "text_shadow_color_emoji.png";
const DIFF_DIR: &str = "target/text_shadow_color_emoji_diffs";
const VIEWPORT: (u32, u32) = (300, 200);

fn should_update_golden() -> bool {
  std::env::var("UPDATE_TEXT_SHADOW_GOLDEN").is_ok()
}

fn render_fixture(html: &str) -> Vec<u8> {
  let font_config = FontConfig::default()
    .with_system_fonts(false)
    .with_bundled_fonts(true);
  let mut renderer = FastRender::builder()
    .font_sources(font_config)
    .build()
    .expect("renderer");
  let options = RenderOptions::new().with_viewport(VIEWPORT.0, VIEWPORT.1);
  let mut artifacts = RenderArtifacts::new(RenderArtifactRequest {
    display_list: true,
    ..Default::default()
  });
  let pixmap = renderer
    .render_html_with_options_and_artifacts(html, options, &mut artifacts)
    .expect("render html");

  // Ensure the fixture actually exercises the text-shadow path. A blank render would make the
  // golden test meaningless (and could happen if the emoji font fails to load or the shadow is
  // optimized away).
  let display_list = artifacts.display_list.as_ref().expect("display list captured");
  let mut saw_shadowed_text = false;
  for item in display_list.items() {
    if let DisplayItem::Text(text) = item {
      if !text.shadows.is_empty() {
        saw_shadowed_text = true;
        break;
      }
    }
  }
  assert!(
    saw_shadowed_text,
    "fixture did not produce any Text items with shadows; cannot validate emoji text-shadow output"
  );

  let non_white = pixmap
    .pixels()
    .iter()
    .filter(|px| px.red() != 255 || px.green() != 255 || px.blue() != 255)
    .count();
  assert!(
    non_white > 0,
    "fixture rendered as a blank white image; cannot validate emoji text-shadow output"
  );
  encode_image(&pixmap, OutputFormat::Png).expect("encode png")
}

#[test]
fn text_shadow_color_emoji_matches_golden() {
  let compare_config =
    compare_config_from_env(CompareEnvVars::fixtures()).expect("compare configuration");
  let html = fs::read_to_string(HTML_PATH).expect("read html fixture");
  let rendered = render_fixture(&html);

  let golden_path = Path::new(GOLDEN_DIR).join(GOLDEN_NAME);
  if should_update_golden() {
    fs::create_dir_all(GOLDEN_DIR).expect("create golden dir");
    fs::write(&golden_path, &rendered)
      .unwrap_or_else(|e| panic!("Failed to write golden {}: {}", golden_path.display(), e));
    return;
  }

  let golden = fs::read(&golden_path)
    .unwrap_or_else(|e| panic!("Missing golden {} ({}): {}", GOLDEN_NAME, golden_path.display(), e));
  let diff_dir = PathBuf::from(DIFF_DIR);
  compare_pngs(
    GOLDEN_NAME,
    &rendered,
    &golden,
    &compare_config,
    &diff_dir,
  )
  .unwrap_or_else(|e| panic!("{}", e));
}
