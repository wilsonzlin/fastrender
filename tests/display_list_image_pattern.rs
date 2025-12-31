use base64::Engine as _;
use fastrender::api::FastRender;
use fastrender::geometry::{Point, Rect};
use fastrender::image_loader::ImageCache;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::paint::painter::{paint_tree_with_resources_scaled_offset_backend, PaintBackend};
use fastrender::scroll::ScrollState;
use fastrender::style::color::Rgba;
use fastrender::{DisplayItem, DisplayListOptimizer, FontContext, PaintParallelism};
use image::codecs::png::PngEncoder;
use image::{ColorType, ImageEncoder};

#[test]
fn repeating_background_emits_single_image_pattern_item() {
  // Build a 1x1 opaque red PNG and embed it as a data: URL.
  let mut png = Vec::new();
  let encoder = PngEncoder::new(&mut png);
  encoder
    .write_image(&[255u8, 0, 0, 255], 1, 1, ColorType::Rgba8.into())
    .expect("encode 1x1 PNG");
  let data_url = format!(
    "data:image/png;base64,{}",
    base64::engine::general_purpose::STANDARD.encode(&png)
  );

  let html = format!(
    r#"
    <html>
      <head>
        <style>
          html, body {{ margin: 0; padding: 0; }}
          #box {{
            width: 32px;
            height: 32px;
            background-image: url("{data_url}");
            background-repeat: repeat;
          }}
        </style>
      </head>
      <body>
        <div id="box"></div>
      </body>
    </html>
    "#
  );

  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(&html).expect("parse html");
  let fragments = renderer
    .layout_document(&dom, 32, 32)
    .expect("layout document");

  let image_cache = ImageCache::new();
  let list = DisplayListBuilder::with_image_cache(image_cache.clone())
    .build_with_stacking_tree(&fragments.root);

  let pattern_items = list
    .items()
    .iter()
    .filter(|item| matches!(item, DisplayItem::ImagePattern(_)))
    .count();
  let image_items = list
    .items()
    .iter()
    .filter(|item| matches!(item, DisplayItem::Image(_)))
    .count();

  assert_eq!(
    pattern_items, 1,
    "expected a single pattern item for repeating background (got {pattern_items})"
  );
  assert_eq!(
    image_items, 0,
    "expected no per-tile Image items for repeating background (got {image_items})"
  );
  assert!(
    list.len() < 200,
    "display list should stay O(1) for repeats (got {} items)",
    list.len()
  );

  // Ensure the optimizer preserves the item and the renderer produces non-empty output.
  let viewport = Rect::from_xywh(0.0, 0.0, 32.0, 32.0);
  let (optimized, _) = DisplayListOptimizer::new().optimize_checked(list, viewport).unwrap();
  assert_eq!(
    optimized
      .items()
      .iter()
      .filter(|item| matches!(item, DisplayItem::ImagePattern(_)))
      .count(),
    1,
    "optimizer should preserve the pattern fill"
  );

  let pixmap = DisplayListRenderer::new(32, 32, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&optimized)
    .unwrap();
  let top_left = pixmap.pixel(0, 0).expect("pixel in bounds");
  assert!(
    top_left.red() > 200 && top_left.green() < 50 && top_left.blue() < 50 && top_left.alpha() == 255,
    "expected non-white pixels from repeated background (got {top_left:?})"
  );

  // Parity check against the legacy painter (which still paints tiled backgrounds).
  let scroll_state = ScrollState::default();
  let legacy = paint_tree_with_resources_scaled_offset_backend(
    &fragments,
    32,
    32,
    Rgba::WHITE,
    FontContext::new(),
    image_cache,
    1.0,
    Point::ZERO,
    PaintParallelism::disabled(),
    &scroll_state,
    PaintBackend::Legacy,
  )
  .unwrap();

  assert_eq!(
    pixmap.data(),
    legacy.data(),
    "pattern-based display list output should match legacy painter"
  );
}
