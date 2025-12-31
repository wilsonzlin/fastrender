use base64::engine::general_purpose::STANDARD;
use base64::Engine;
use fastrender::image_loader::ImageCache;
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::paint::painter::{paint_tree_with_resources_scaled_offset_backend, PaintBackend};
use fastrender::scroll::ScrollState;
use fastrender::{FastRender, Point, Rgba};
use image::codecs::png::PngEncoder;
use image::ColorType;
use image::ImageEncoder;

fn make_striped_png_data_url() -> String {
  let mut img = image::RgbaImage::new(2, 1);
  img.put_pixel(0, 0, image::Rgba([255, 0, 0, 255]));
  img.put_pixel(1, 0, image::Rgba([0, 0, 255, 255]));

  let mut bytes = Vec::new();
  PngEncoder::new(&mut bytes)
    .write_image(
      img.as_raw(),
      img.width(),
      img.height(),
      ColorType::Rgba8.into(),
    )
    .expect("encode png");

  format!("data:image/png;base64,{}", STANDARD.encode(bytes))
}

fn pixel(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let p = pixmap.pixel(x, y).unwrap();
  (p.red(), p.green(), p.blue(), p.alpha())
}

fn assert_close(actual: (u8, u8, u8, u8), expected: (u8, u8, u8, u8), tol: u8) {
  let diff = (
    actual.0.abs_diff(expected.0),
    actual.1.abs_diff(expected.1),
    actual.2.abs_diff(expected.2),
    actual.3.abs_diff(expected.3),
  );
  assert!(
    diff.0 <= tol && diff.1 <= tol && diff.2 <= tol && diff.3 <= tol,
    "pixel {:?} differed from {:?} by {:?} (tol {tol})",
    actual,
    expected,
    diff
  );
}

#[test]
fn legacy_repeat_pattern_respects_stacking_context_origin_offset() {
  let img_data_url = make_striped_png_data_url();

  let html = format!(
    r#"<!doctype html>
<style>
  html, body {{ margin: 0; padding: 0; background: white; }}
  .sc {{
    position: absolute;
    left: 13px;
    top: 0px;
    width: 4px;
    height: 1px;
    opacity: 0.5;
  }}
  .tile {{
    width: 4px;
    height: 1px;
    background-image: url("{img_data_url}");
    background-repeat: repeat;
    background-size: 2px 1px;
    image-rendering: pixelated;
  }}
</style>
<div class="sc"><div class="tile"></div></div>
"#
  );

  let viewport_w = 32;
  let viewport_h = 4;
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(&html).expect("parsed");
  let fragment_tree = renderer
    .layout_document(&dom, viewport_w, viewport_h)
    .expect("laid out");

  let pixmap = paint_tree_with_resources_scaled_offset_backend(
    &fragment_tree,
    viewport_w,
    viewport_h,
    Rgba::WHITE,
    renderer.font_context().clone(),
    ImageCache::new(),
    1.0,
    Point::ZERO,
    PaintParallelism::default(),
    &ScrollState::default(),
    PaintBackend::Legacy,
  )
  .expect("painted");

  // The stacking context starts at x=13. The 2px-wide striped image should align so that the
  // left-most pixel of the element is red. A missing origin offset adjustment shifts the pattern
  // by 13px, effectively swapping the stripe order (13 mod 2 == 1).
  assert_close(pixel(&pixmap, 12, 0), (255, 255, 255, 255), 0);
  assert_close(pixel(&pixmap, 13, 0), (255, 128, 128, 255), 2);
  assert_close(pixel(&pixmap, 14, 0), (128, 128, 255, 255), 2);
  assert_close(pixel(&pixmap, 15, 0), (255, 128, 128, 255), 2);
  assert_close(pixel(&pixmap, 16, 0), (128, 128, 255, 255), 2);
}

