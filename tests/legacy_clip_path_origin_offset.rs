use fastrender::image_loader::ImageCache;
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::paint::painter::{paint_tree_with_resources_scaled_offset_backend, PaintBackend};
use fastrender::scroll::ScrollState;
use fastrender::{FastRender, Point, Rgba};

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
fn legacy_clip_path_respects_stacking_context_origin_offset() {
  // Clip-path uses an alpha mask built in device space. When stacking contexts render into
  // smaller intermediate pixmaps, the mask must account for the painter's CSS-space origin offset
  // or it will clip the wrong pixels.
  let html = r#"<!doctype html>
<style>
  html, body { margin: 0; padding: 0; background: white; }
  .sc {
    position: absolute;
    left: 13px;
    top: 0px;
    width: 4px;
    height: 1px;
    background: rgb(255 0 0);
    clip-path: inset(0 0 0 1px);
  }
</style>
<div class="sc"></div>
"#;

  let viewport_w = 32;
  let viewport_h = 4;
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(html).expect("parsed");
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

  // The element begins at x=13 and is clipped to start at x=14.
  assert_close(pixel(&pixmap, 12, 0), (255, 255, 255, 255), 0);
  assert_close(pixel(&pixmap, 13, 0), (255, 255, 255, 255), 0);
  assert_close(pixel(&pixmap, 14, 0), (255, 0, 0, 255), 0);
  assert_close(pixel(&pixmap, 15, 0), (255, 0, 0, 255), 0);
  assert_close(pixel(&pixmap, 16, 0), (255, 0, 0, 255), 0);
  assert_close(pixel(&pixmap, 17, 0), (255, 255, 255, 255), 0);
}

