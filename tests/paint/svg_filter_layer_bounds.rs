use fastrender::FastRender;
use resvg::tiny_skia::Pixmap;

fn color_at(pixmap: &Pixmap, x: u32, y: u32) -> [u8; 4] {
  let pixel = pixmap.pixel(x, y).expect("pixel");
  [pixel.red(), pixel.green(), pixel.blue(), pixel.alpha()]
}

fn blurred_box_fixture(left: i32, top: i32, size: i32) -> String {
  format!(
    r#"
    <style>
      body {{ margin: 0; background: rgb(0, 0, 0); }}
      #target {{
        position: absolute;
        left: {left}px;
        top: {top}px;
        width: {size}px;
        height: {size}px;
        background: rgb(255, 0, 0);
        filter: url(#blur);
      }}
      svg {{ position: absolute; width: 0; height: 0; }}
    </style>
    <svg width="0" height="0" aria-hidden="true">
      <defs>
        <filter id="blur" x="-50%" y="-50%" width="200%" height="200%">
          <feGaussianBlur stdDeviation="4" />
        </filter>
      </defs>
    </svg>
    <div id="target"></div>
    "#
  )
}

#[test]
fn svg_filter_blur_bleeds_outside_bounds() {
  let html = blurred_box_fixture(20, 20, 40);

  let mut renderer = FastRender::new().expect("renderer");
  let pixmap = renderer.render_html(&html, 120, 120).expect("render");

  let outside = color_at(&pixmap, 15, 40);
  assert!(
    outside[0] > outside[1] && outside[0] > outside[2] && outside[0] > 0,
    "expected blur bleed outside the border box, got {:?}",
    outside
  );
}

#[test]
fn svg_filter_blur_near_viewport_edge_expands_layer() {
  let html = blurred_box_fixture(0, 0, 20);

  let mut renderer = FastRender::new().expect("renderer");
  let pixmap = renderer.render_html(&html, 60, 40).expect("render");

  let outside = color_at(&pixmap, 10, 25);
  assert!(
    outside[0] > outside[1] && outside[0] > outside[2] && outside[0] > 0,
    "blur should remain visible near viewport edges, got {:?}",
    outside
  );
}
