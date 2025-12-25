use fastrender::FastRender;

fn pixel_rgba(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let p = pixmap.pixel(x, y).expect("pixel in bounds");
  (p.red(), p.green(), p.blue(), p.alpha())
}

#[test]
fn margin_boxes_are_painted() {
  // Regression coverage: @page margin boxes should paint their backgrounds into the page raster.
  let mut renderer = FastRender::new().expect("renderer");
  let html = r#"
    <style>
      @page {
        size: 200px 200px;
        margin: 10px;
        @top-left-corner {
          background: rgb(255, 0, 0);
          content: "";
        }
      }
      body { margin: 0; }
    </style>
    <div style="width: 180px; height: 180px; background: rgb(0, 255, 0)"></div>
  "#;

  let pixmap = renderer
    .render_html(html, 200, 200)
    .expect("render paged document");

  let (r, g, b, a) = pixel_rgba(&pixmap, 5, 5);
  assert!(
    r > 240 && g < 16 && b < 16 && a > 240,
    "top-left margin box should paint red, got ({r},{g},{b},{a})"
  );

  let (r, g, b, a) = pixel_rgba(&pixmap, 15, 15);
  assert!(
    g > 240 && r < 16 && b < 16 && a > 240,
    "content area should paint green, got ({r},{g},{b},{a})"
  );
}
