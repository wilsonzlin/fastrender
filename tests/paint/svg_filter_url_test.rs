use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use fastrender::FastRender;
use resvg::tiny_skia::Pixmap;

fn color_at(pixmap: &Pixmap, x: u32, y: u32) -> [u8; 4] {
  let pixel = pixmap.pixel(x, y).expect("pixel");
  [pixel.red(), pixel.green(), pixel.blue(), pixel.alpha()]
}

#[test]
fn filter_url_fragment_uses_inline_svg_filter() {
  let html = r#"
  <style>
    body { margin: 0; }
    #box { width: 20px; height: 20px; background: rgb(255, 0, 0); filter: url(#recolor); }
    svg { position: absolute; width: 0; height: 0; }
  </style>
  <svg width="0" height="0" aria-hidden="true">
    <defs>
      <filter id="recolor">
        <feFlood flood-color="rgb(0, 255, 0)" result="flood" />
        <feComposite in="flood" in2="SourceAlpha" operator="in" />
      </filter>
    </defs>
  </svg>
  <div id="box"></div>
  "#;

  let mut renderer = FastRender::new().expect("renderer");
  let pixmap = renderer.render_html(html, 30, 30).expect("render");

  assert_eq!(color_at(&pixmap, 10, 10), [0, 255, 0, 255]);
}

#[test]
fn filter_url_data_svg_is_applied() {
  let filter_svg = r#"
    <svg xmlns="http://www.w3.org/2000/svg">
      <filter id="recolor">
        <feFlood flood-color="rgb(0, 255, 0)" result="flood" />
        <feComposite in="flood" in2="SourceAlpha" operator="in" />
      </filter>
    </svg>
  "#;
  let encoded = BASE64.encode(filter_svg);
  let data_url = format!("data:image/svg+xml;base64,{}#recolor", encoded);
  let html = format!(
    r#"
    <style>
      body {{ margin: 0; }}
      #box {{ width: 20px; height: 20px; background: rgb(255, 0, 0); filter: url("{}"); }}
    </style>
    <div id="box"></div>
    "#,
    data_url
  );

  let mut renderer = FastRender::new().expect("renderer");
  let pixmap = renderer.render_html(&html, 30, 30).expect("render");

  assert_eq!(color_at(&pixmap, 10, 10), [0, 255, 0, 255]);
}

#[test]
fn missing_fragment_filter_is_ignored() {
  let html = r#"
  <style>
    body { margin: 0; }
    #box { width: 20px; height: 20px; background: rgb(255, 0, 0); filter: url(#missing); }
  </style>
  <div id="box"></div>
  "#;

  let mut renderer = FastRender::new().expect("renderer");
  let pixmap = renderer.render_html(html, 30, 30).expect("render");

  assert_eq!(color_at(&pixmap, 10, 10), [255, 0, 0, 255]);
}
