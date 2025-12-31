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
fn legacy_mask_respects_stacking_context_origin_offset() {
  // `mask-image` is rasterized into an alpha mask. When a stacking context is rendered into a
  // smaller intermediate pixmap, mask tile placement must subtract the painter's CSS-space origin
  // offset or the mask will be shifted by the stacking context's position.
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
    mask-image: linear-gradient(
      to right,
      rgba(0 0 0 / 1) 0%,
      rgba(0 0 0 / 1) 50%,
      rgba(0 0 0 / 0) 50%,
      rgba(0 0 0 / 0) 100%
    );
    mask-size: 2px 1px;
    mask-repeat: repeat;
    mask-position: 0 0;
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

  // The mask tile is 2px wide, opaque on the left 1px and transparent on the right 1px.
  assert_close(pixel(&pixmap, 12, 0), (255, 255, 255, 255), 0);
  assert_close(pixel(&pixmap, 13, 0), (255, 0, 0, 255), 1);
  assert_close(pixel(&pixmap, 14, 0), (255, 255, 255, 255), 1);
  assert_close(pixel(&pixmap, 15, 0), (255, 0, 0, 255), 1);
  assert_close(pixel(&pixmap, 16, 0), (255, 255, 255, 255), 1);
  assert_close(pixel(&pixmap, 17, 0), (255, 255, 255, 255), 0);
}

