use fastrender::geometry::Point;
use fastrender::image_loader::ImageCache;
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::paint::painter::{paint_tree_with_resources_scaled_offset_backend, PaintBackend};
use fastrender::scroll::ScrollState;
use fastrender::style::color::Rgba;
use fastrender::{FastRender, Pixmap};
use std::fs;

const HTML_PATH: &str = "tests/fixtures/html/svg_filter_background_image.html";

fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let px = pixmap.pixel(x, y).expect("pixel in bounds");
  (px.red(), px.green(), px.blue(), px.alpha())
}

#[test]
fn svg_filter_background_image_uses_backdrop_pixels() {
  let html = fs::read_to_string(HTML_PATH).expect("read fixture");
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(&html).expect("parse html");
  let fragments = renderer.layout_document(&dom, 30, 30).expect("layout document");

  let pixmap = paint_tree_with_resources_scaled_offset_backend(
    &fragments,
    30,
    30,
    Rgba::WHITE,
    renderer.font_context().clone(),
    ImageCache::new(),
    1.0,
    Point::ZERO,
    PaintParallelism::default(),
    &ScrollState::default(),
    PaintBackend::DisplayList,
  )
  .expect("paint");

  assert_eq!(
    pixel(&pixmap, 10, 10),
    (0, 0, 0, 255),
    "expected multiply(red, blue) == black when BackgroundImage is supported"
  );
}

