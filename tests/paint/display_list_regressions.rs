use fastrender::image_loader::ImageCache;
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::paint::painter::paint_tree_with_resources_scaled_offset;
use fastrender::scroll::ScrollState;
use fastrender::{FastRender, Point, Rgba};

fn render(html: &str, width: u32, height: u32) -> tiny_skia::Pixmap {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(html).expect("parsed");
  let fragment_tree = renderer
    .layout_document(&dom, width, height)
    .expect("laid out");

  let font_ctx = renderer.font_context().clone();
  let image_cache = ImageCache::new();

  paint_tree_with_resources_scaled_offset(
    &fragment_tree,
    width,
    height,
    Rgba::WHITE,
    font_ctx,
    image_cache,
    1.0,
    Point::ZERO,
    PaintParallelism::default(),
    &ScrollState::default(),
  )
  .expect("painted")
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
fn stacking_and_opacity_rendering() {
  let html = r#"
    <!doctype html>
    <style>
      body { margin: 0; background: white; }
      .container { position: relative; width: 40px; height: 40px; }
      .base {
        position: absolute;
        left: 0;
        top: 0;
        width: 30px;
        height: 30px;
        background: rgb(255 0 0);
        opacity: 0.5;
      }
      .top {
        position: absolute;
        left: 10px;
        top: 10px;
        width: 30px;
        height: 30px;
        background: rgb(0 0 255);
        opacity: 0.5;
        z-index: 1;
      }
    </style>
    <div class="container">
      <div class="base"></div>
      <div class="top"></div>
    </div>
  "#;

  let pixmap = render(html, 64, 64);
  // Red over white at (5,5)
  assert_close(pixel(&pixmap, 5, 5), (255, 128, 128, 255), 2);
  // Blue over white at (35,35) (only top box)
  assert_close(pixel(&pixmap, 35, 35), (128, 128, 255, 255), 2);
  // Overlap region should blend both
  assert_close(pixel(&pixmap, 20, 20), (128, 64, 191, 255), 3);
}

#[test]
fn overflow_clip_masks_content() {
  let html = r#"
    <!doctype html>
    <style>
      body { margin: 0; background: rgb(230 230 230); }
      .clip {
        width: 40px;
        height: 40px;
        overflow: hidden;
        background: rgb(200 200 200);
      }
      .content {
        width: 60px;
        height: 60px;
        background: rgb(0 128 0);
        position: relative;
        left: -10px;
        top: -10px;
      }
    </style>
    <div class="clip">
      <div class="content"></div>
    </div>
  "#;

  let pixmap = render(html, 64, 64);
  assert_close(pixel(&pixmap, 20, 20), (0, 128, 0, 255), 2);
  assert_close(pixel(&pixmap, 45, 20), (230, 230, 230, 255), 1);
}

#[test]
fn blend_mode_screen_applies() {
  let html = r#"
    <!doctype html>
    <style>
      body { margin: 0; background: rgb(120 120 120); }
      .scene { position: relative; width: 50px; height: 50px; }
      .base {
        position: absolute;
        inset: 0;
        background: rgb(200 0 0);
      }
      .blend {
        position: absolute;
        left: 10px;
        top: 10px;
        width: 30px;
        height: 30px;
        background: rgb(0 200 0);
        mix-blend-mode: screen;
      }
    </style>
    <div class="scene">
      <div class="base"></div>
      <div class="blend"></div>
    </div>
  "#;

  let pixmap = render(html, 64, 64);
  assert_close(pixel(&pixmap, 5, 5), (200, 0, 0, 255), 1);
  assert_close(pixel(&pixmap, 20, 20), (200, 200, 0, 255), 5);
  assert_close(pixel(&pixmap, 60, 60), (120, 120, 120, 255), 1);
}

#[test]
fn filter_blur_renders() {
  let blurred_html = r#"
    <!doctype html>
    <style>
      body { margin: 0; background: white; }
      .blurred {
        width: 30px;
        height: 30px;
        margin: 5px;
        background: rgb(50 0 200);
        filter: blur(2px);
      }
    </style>
    <div class="blurred"></div>
  "#;
  let crisp_html = r#"
    <!doctype html>
    <style>
      body { margin: 0; background: white; }
      .blurred {
        width: 30px;
        height: 30px;
        margin: 5px;
        background: rgb(50 0 200);
      }
    </style>
    <div class="blurred"></div>
  "#;

  let blurred = render(blurred_html, 48, 48);
  let crisp = render(crisp_html, 48, 48);
  assert_ne!(
    blurred.data(),
    crisp.data(),
    "blur filter should change the output"
  );
  assert_ne!(
    pixel(&blurred, 2, 2),
    pixel(&crisp, 2, 2),
    "blur should bleed color beyond the element bounds"
  );
}
