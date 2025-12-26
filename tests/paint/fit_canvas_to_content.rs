use fastrender::api::FastRender;

#[test]
fn fit_canvas_to_content_renders_all_pages() {
  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 200px 200px; margin: 0; }
          body { margin: 0; }
        </style>
      </head>
      <body>
        <div style="height: 220px; background: red;"></div>
        <div style="height: 220px; background: blue;"></div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::builder()
    .viewport_size(200, 200)
    .fit_canvas_to_content(true)
    .build()
    .unwrap();

  let pixmap = renderer.render_html(html, 200, 200).unwrap();

  assert!(
    pixmap.height() >= 400,
    "expected pixmap height to include multiple pages, got {}",
    pixmap.height()
  );
}
