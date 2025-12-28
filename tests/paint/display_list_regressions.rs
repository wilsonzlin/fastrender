use fastrender::image_loader::ImageCache;
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::paint::painter::{paint_tree_with_resources_scaled_offset_backend, PaintBackend};
use fastrender::{FastRender, Point, Rgba};

fn render_with_backend(
  html: &str,
  width: u32,
  height: u32,
  backend: PaintBackend,
) -> tiny_skia::Pixmap {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(html).expect("parsed");
  let fragment_tree = renderer
    .layout_document(&dom, width, height)
    .expect("laid out");

  let font_ctx = renderer.font_context().clone();
  let image_cache = ImageCache::new();

  paint_tree_with_resources_scaled_offset_backend(
    &fragment_tree,
    width,
    height,
    Rgba::WHITE,
    font_ctx.clone(),
    image_cache.clone(),
    1.0,
    Point::ZERO,
    PaintParallelism::default(),
    backend,
  )
  .expect("painted")
}

fn assert_parity(html: &str, width: u32, height: u32) {
  let legacy = render_with_backend(html, width, height, PaintBackend::Legacy);
  let display_list = render_with_backend(html, width, height, PaintBackend::DisplayList);

  assert_eq!(legacy.width(), display_list.width());
  assert_eq!(legacy.height(), display_list.height());
  assert_eq!(legacy.data(), display_list.data());
}

#[test]
fn stacking_and_opacity_parity() {
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

  assert_parity(html, 64, 64);
}

#[test]
fn overflow_clip_parity() {
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

  assert_parity(html, 64, 64);
}

#[test]
fn blend_mode_parity() {
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

  assert_parity(html, 64, 64);
}

#[test]
fn filter_blur_parity() {
  let html = r#"
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

  assert_parity(html, 48, 48);
}
