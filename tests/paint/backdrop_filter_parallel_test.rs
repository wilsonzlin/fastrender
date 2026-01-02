use fastrender::image_loader::ImageCache;
use fastrender::paint::display_list::DisplayList;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::paint::display_list_renderer::{DisplayListRenderer, PaintParallelism};
use fastrender::scroll::ScrollState;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::fragment_tree::FragmentNode;
use fastrender::{FastRender, FontConfig, Point, Rgba};
use rayon::ThreadPoolBuilder;

fn pixel(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let p = pixmap.pixel(x, y).unwrap();
  (p.red(), p.green(), p.blue(), p.alpha())
}

fn build_display_list(html: &str, width: u32, height: u32) -> (DisplayList, FontContext) {
  let mut renderer = FastRender::builder()
    .font_sources(FontConfig::bundled_only())
    .build()
    .expect("renderer");

  let dom = renderer.parse_html(html).expect("parsed");
  let tree = renderer
    .layout_document(&dom, width, height)
    .expect("laid out");
  let font_ctx = renderer.font_context().clone();
  let image_cache = ImageCache::new();
  let viewport = tree.viewport_size();

  let build_for_root = |root: &FragmentNode| -> DisplayList {
    DisplayListBuilder::with_image_cache(image_cache.clone())
      .with_font_context(font_ctx.clone())
      .with_svg_filter_defs(tree.svg_filter_defs.clone())
      .with_scroll_state(ScrollState::default())
      .with_device_pixel_ratio(1.0)
      // Keep display-list building deterministic; these tests focus on renderer tiling.
      .with_parallelism(&PaintParallelism::disabled())
      .with_viewport_size(viewport.width, viewport.height)
      .build_with_stacking_tree_offset_checked(root, Point::ZERO)
      .expect("display list")
  };

  let mut list = build_for_root(&tree.root);
  for extra in &tree.additional_fragments {
    list.append(build_for_root(extra));
  }
  (list, font_ctx)
}

#[test]
fn backdrop_filter_clips_to_border_radius() {
  let html = r#"<!doctype html>
    <style>
      html, body { margin: 0; padding: 0; }
      #bg { position: absolute; inset: 0; background: rgb(255 0 0); }
      #overlay {
        position: absolute;
        left: 0;
        top: 0;
        width: 40px;
        height: 40px;
        border-radius: 20px;
        backdrop-filter: invert(1);
      }
    </style>
    <div id="bg"></div>
    <div id="overlay"></div>
  "#;

  let (list, font_ctx) = build_display_list(html, 128, 128);
  let pixmap = DisplayListRenderer::new(128, 128, Rgba::WHITE, font_ctx)
    .expect("renderer")
    .with_parallelism(PaintParallelism::disabled())
    .render(&list)
    .expect("render");

  // Outside the rounded corner (top-left pixel) remains red.
  assert_eq!(pixel(&pixmap, 0, 0), (255, 0, 0, 255));
  // Inside the rounded rect, the red backdrop is inverted to cyan.
  assert_eq!(pixel(&pixmap, 20, 20), (0, 255, 255, 255));
  assert_eq!(pixel(&pixmap, 35, 20), (0, 255, 255, 255));
  // Outside the element bounds remains untouched.
  assert_eq!(pixel(&pixmap, 60, 60), (255, 0, 0, 255));
}

#[test]
fn parallel_paint_is_used_with_backdrop_filter_and_matches_serial() {
  let html = r#"<!doctype html>
    <style>
      html, body { margin: 0; padding: 0; }
      .left {
        position: absolute;
        left: 0;
        top: 0;
        width: 64px;
        height: 64px;
        background: rgb(255 0 0);
      }
      .right {
        position: absolute;
        left: 64px;
        top: 0;
        width: 64px;
        height: 64px;
        background: rgb(0 0 255);
      }
      #overlay {
        position: absolute;
        left: 32px;
        top: 0;
        width: 64px;
        height: 64px;
        backdrop-filter: blur(6px);
      }
    </style>
    <div class="left"></div>
    <div class="right"></div>
    <div id="overlay"></div>
  "#;

  let (list, font_ctx) = build_display_list(html, 128, 64);
  let serial = DisplayListRenderer::new(128, 64, Rgba::WHITE, font_ctx.clone())
    .expect("renderer")
    .with_parallelism(PaintParallelism::disabled())
    .render(&list)
    .expect("serial render");

  let parallelism = PaintParallelism {
    tile_size: 32,
    ..PaintParallelism::enabled()
  };
  let pool = ThreadPoolBuilder::new()
    .num_threads(4)
    .build()
    .expect("rayon pool");
  let report = pool.install(|| {
    DisplayListRenderer::new(128, 64, Rgba::WHITE, font_ctx)
      .expect("renderer")
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("parallel render")
  });

  assert!(
    report.parallel_used,
    "expected backdrop-filter scene to use parallel tiling (fallback={:?})",
    report.fallback_reason
  );
  assert!(report.tiles > 1, "expected multiple tiles to be rendered");
  assert_eq!(
    serial.data(),
    report.pixmap.data(),
    "parallel backdrop-filter output diverged from serial"
  );
}
