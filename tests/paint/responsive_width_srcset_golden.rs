#[path = "../ref/mod.rs"]
mod r#ref;

use fastrender::image_output::{encode_image, OutputFormat};
use fastrender::{FastRender, RenderOptions};
use r#ref::image_compare::{compare_config_from_env, compare_pngs, CompareEnvVars};
use std::fs;
use std::path::{Path, PathBuf};

const HTML_PATH: &str = "tests/fixtures/html/responsive_width_srcset.html";
const GOLDEN_DIR: &str = "tests/fixtures/golden";
const DIFF_DIR: &str = "target/responsive_width_srcset_diffs";
const VIEWPORT: (u32, u32) = (32, 32);

fn should_update_golden() -> bool {
  std::env::var("UPDATE_RESPONSIVE_WIDTH_GOLDEN").is_ok()
}

fn render_fixture(html: &str, dpr: f32) -> Vec<u8> {
  let mut renderer = FastRender::builder()
    .device_pixel_ratio(dpr)
    .build()
    .expect("renderer");
  let options = RenderOptions::new().with_viewport(VIEWPORT.0, VIEWPORT.1);
  let pixmap = renderer
    .render_html_with_options(html, options)
    .expect("render html");
  encode_image(&pixmap, OutputFormat::Png).expect("encode png")
}

#[test]
fn responsive_width_srcset_golden_across_dpr() {
  let compare_config =
    compare_config_from_env(CompareEnvVars::fixtures()).expect("compare configuration");
  let html = fs::read_to_string(HTML_PATH).expect("read html fixture");
  let outputs = [
    (1.0_f32, "responsive_width_srcset.png"),
    (2.0_f32, "responsive_width_srcset_dpr2.png"),
  ];
  let diff_dir = PathBuf::from(DIFF_DIR);

  for (dpr, golden_name) in outputs {
    let rendered = render_fixture(&html, dpr);
    let golden_path = Path::new(GOLDEN_DIR).join(golden_name);
    if should_update_golden() {
      fs::write(&golden_path, &rendered)
        .unwrap_or_else(|e| panic!("Failed to write golden {}: {}", golden_path.display(), e));
      continue;
    }
    let golden = fs::read(&golden_path).unwrap_or_else(|e| {
      panic!(
        "Missing golden {} ({}): {}",
        golden_name,
        golden_path.display(),
        e
      )
    });
    compare_pngs(golden_name, &rendered, &golden, &compare_config, &diff_dir)
      .unwrap_or_else(|e| panic!("{}", e));
  }
}
