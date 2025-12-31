use fastrender::image_loader::ImageCache;
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::paint::painter::{paint_tree_with_resources_scaled_offset_backend, PaintBackend};
use fastrender::scroll::ScrollState;
use fastrender::{FastRender, Point, Rgba};

fn bounding_box_for_non_black(pixmap: &tiny_skia::Pixmap) -> Option<(u32, u32, u32, u32)> {
  let mut min_x = u32::MAX;
  let mut min_y = u32::MAX;
  let mut max_x = 0u32;
  let mut max_y = 0u32;
  let mut seen = false;

  for y in 0..pixmap.height() {
    for x in 0..pixmap.width() {
      let p = pixmap.pixel(x, y).unwrap();
      if p.red() != 0 || p.green() != 0 || p.blue() != 0 {
        seen = true;
        min_x = min_x.min(x);
        min_y = min_y.min(y);
        max_x = max_x.max(x);
        max_y = max_y.max(y);
      }
    }
  }

  if seen {
    Some((min_x, min_y, max_x, max_y))
  } else {
    None
  }
}

#[test]
fn legacy_projective_transform_respects_stacking_context_origin_offset() {
  // Projective (non-affine) transforms go through the homography warp path. When painting inside
  // nested stacking contexts, the projected destination quad must be computed in the parent
  // painter's local device space (i.e., subtracting origin_offset_css).
  let html = r#"<!doctype html>
<style>
  html, body { margin: 0; padding: 0; background: black; }
  .outer {
    position: absolute;
    left: 10px;
    top: 0px;
    width: 30px;
    height: 20px;
    isolation: isolate; /* forces an intermediate layer without changing colors */
  }
  .inner {
    width: 8px;
    height: 8px;
    background: white;
    transform-origin: 0 0;
    transform: perspective(40px) rotateY(60deg);
  }
</style>
<div class="outer"><div class="inner"></div></div>
"#;

  let viewport_w = 64;
  let viewport_h = 24;
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(html).expect("parsed");
  let fragment_tree = renderer
    .layout_document(&dom, viewport_w, viewport_h)
    .expect("laid out");

  let pixmap = paint_tree_with_resources_scaled_offset_backend(
    &fragment_tree,
    viewport_w,
    viewport_h,
    Rgba::new(0, 0, 0, 1.0),
    renderer.font_context().clone(),
    ImageCache::new(),
    1.0,
    Point::ZERO,
    PaintParallelism::default(),
    &ScrollState::default(),
    PaintBackend::Legacy,
  )
  .expect("painted");

  let (min_x, _min_y, _max_x, _max_y) =
    bounding_box_for_non_black(&pixmap).expect("expected transformed content to be visible");
  assert!(
    min_x <= 12,
    "expected projective content to start near x=10 (min_x={min_x})"
  );
}

