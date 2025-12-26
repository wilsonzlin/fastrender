use base64::{engine::general_purpose, Engine as _};
use fastrender::FastRender;
use resvg::tiny_skia::Pixmap;

fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> [u8; 4] {
  let idx = (y as usize * pixmap.width() as usize + x as usize) * 4;
  let data = pixmap.data();
  [data[idx], data[idx + 1], data[idx + 2], data[idx + 3]]
}

#[test]
fn svg_url_filter_applies_in_painter_pipeline() {
  let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='2' height='2'><filter id='f'><feFlood flood-color='rgb(255,0,0)' flood-opacity='1' result='f'/><feComposite in='f' in2='SourceAlpha' operator='in'/></filter></svg>";
  let data_url = format!(
    "data:image/svg+xml;base64,{}",
    general_purpose::STANDARD.encode(svg)
  );

  let html = format!(
    r#"
    <style>
      body {{ margin: 0; background: white; }}
      .box {{
        width: 10px;
        height: 10px;
        background: rgb(0, 0, 255);
        filter: url("{}");
      }}
    </style>
    <div class="box"></div>
    "#,
    data_url
  );

  let mut renderer = FastRender::new().expect("renderer");
  let pixmap = renderer
    .render_html(&html, 20, 20)
    .expect("render with svg filter");
  let center = pixel(&pixmap, 5, 5);
  assert!(
    center[0] > center[2],
    "svg filter flood should push output toward red (got {:?})",
    center
  );
  assert!(
    center[3] > 0,
    "filtered output should remain visible within the element bounds"
  );
  assert_eq!(
    pixel(&pixmap, 15, 15),
    [255, 255, 255, 255],
    "filter region should stay clipped to the element"
  );
}
