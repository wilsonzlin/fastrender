use fastrender::api::FastRender;

#[test]
fn builder_columns_layout_into_multiple_roots() {
  let mut renderer = FastRender::builder()
    .columns(2, 24.0, 180.0)
    .build()
    .unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          body { margin: 0; }
          .tall { height: 300px; }
        </style>
      </head>
      <body>
        <div class="tall"></div>
      </body>
    </html>
  "#;

  let dom = renderer.parse_html(html).unwrap();
  let fragments = renderer.layout_document(&dom, 200, 200).unwrap();

  assert!(
    !fragments.additional_fragments.is_empty(),
    "column fragmentation should create multiple root fragments"
  );
  assert!(
    fragments
      .additional_fragments
      .iter()
      .any(|f| f.bounds.x() > 0.0),
    "columns should be positioned horizontally"
  );
}
