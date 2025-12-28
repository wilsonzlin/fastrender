use fastrender::geometry::Point;
use fastrender::image_loader::ImageCache;
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::paint::painter::{paint_tree_with_resources_scaled_offset_backend, PaintBackend};
use fastrender::scroll::ScrollState;
use fastrender::style::color::Rgba;
use fastrender::FastRender;
use fastrender::Pixmap;

fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let p = pixmap.pixel(x, y).expect("pixel in bounds");
  (p.red(), p.green(), p.blue(), p.alpha())
}

#[test]
fn display_list_backend_resolves_display_none_svg_defs() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r#"
    <style>
      body { margin: 0; }
      .box {
        width: 10px;
        height: 10px;
        background: rgb(0, 0, 255);
        filter: url(#recolor);
      }
    </style>
    <svg style="display: none">
      <defs>
        <filter id="recolor" color-interpolation-filters="sRGB">
          <feColorMatrix type="matrix"
            values="0 0 0 0 1  0 0 0 0 0  0 0 0 0 0  0 0 0 1 0" />
        </filter>
      </defs>
    </svg>
    <div class="box"></div>
  "#;

  let dom = renderer.parse_html(html).expect("parse html");
  let fragments = renderer
    .layout_document(&dom, 20, 20)
    .expect("layout document");
  assert!(
    fragments
      .svg_filter_defs
      .as_ref()
      .is_some_and(|defs| defs.contains_key("recolor")),
    "layout should retain defs from display:none SVG"
  );

  let pixmap = paint_tree_with_resources_scaled_offset_backend(
    &fragments,
    20,
    20,
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

  let sample = pixel(&pixmap, 5, 5);
  assert!(
    sample.0 > sample.2,
    "filter:url(#recolor) should recolor the blue box when painted via the display-list backend (got {:?})",
    sample
  );
}
