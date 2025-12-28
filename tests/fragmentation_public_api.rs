use fastrender::api::FastRender;

#[test]
fn builder_pagination_without_page_rules_fragments() {
  let mut renderer = FastRender::builder().paginate(120.0, 8.0).build().unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          body { margin: 0; }
          .tall { height: 360px; }
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
    "manual pagination should create additional fragments when no @page rules are present"
  );
}

#[test]
fn page_rules_override_manual_fragmentation() {
  let mut renderer = FastRender::builder().paginate(60.0, 0.0).build().unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 400px 400px; margin: 0; }
          body { margin: 0; }
          .content { height: 180px; }
        </style>
      </head>
      <body>
        <div class="content"></div>
      </body>
    </html>
  "#;

  let dom = renderer.parse_html(html).unwrap();
  let fragments = renderer.layout_document(&dom, 400, 400).unwrap();

  assert!(
    fragments.additional_fragments.is_empty(),
    "CSS @page rules should take precedence over manual pagination settings"
  );
}

#[test]
fn pagination_respects_vertical_writing_mode() {
  let mut renderer = FastRender::builder().paginate(120.0, 8.0).build().unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          html { writing-mode: vertical-rl; }
          body { margin: 0; }
          .tall { height: 360px; }
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
    fragments.additional_fragments.len() >= 2,
    "manual pagination should fragment vertical writing mode documents"
  );
}
