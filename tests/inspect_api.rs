use fastrender::{FastRender, InspectQuery};

#[test]
fn inspect_reports_style_and_layout_for_selector() {
  std::thread::Builder::new()
    .stack_size(8 * 1024 * 1024)
    .spawn(|| {
      let mut renderer = FastRender::new().expect("renderer");
      let html = r#"
        <html>
          <head>
            <style>
              body { margin: 0; padding: 0; }
              #container { margin: 0; padding: 0; }
              .item { display: block; width: 50px; height: 10px; margin: 0; padding: 0; border: 0 solid transparent; }
            </style>
          </head>
          <body>
            <div id="container">
              <div class="item" id="first"></div>
              <div class="item" id="second"></div>
              <div class="item"></div>
            </div>
          </body>
        </html>
      "#;
      let dom = renderer.parse_html(html).expect("parse");
      let results = renderer
        .inspect(
          &dom,
          200,
          200,
          InspectQuery::Selector("#container .item:nth-child(2)".to_string()),
        )
        .expect("inspection results");

      assert_eq!(results.len(), 1);
      let snapshot = &results[0];
      assert_eq!(snapshot.node.id.as_deref(), Some("second"));
      assert_eq!(snapshot.node.tag_name.as_deref(), Some("div"));
      assert_eq!(snapshot.style.display, "block");
      assert_eq!(snapshot.style.width, Some(50.0));
      assert_eq!(snapshot.style.height, Some(10.0));

      let block_fragment = snapshot
        .fragments
        .iter()
        .find(|f| f.kind == "block")
        .expect("block fragment");
      assert!((block_fragment.bounds.width - 50.0).abs() < f32::EPSILON);
      assert!((block_fragment.bounds.height - 10.0).abs() < f32::EPSILON);
      assert!((block_fragment.bounds.y - 10.0).abs() < f32::EPSILON);
    })
    .expect("spawn test thread")
    .join()
    .expect("join test thread");
}

#[test]
fn inspect_selector_respects_document_quirks_mode() {
  std::thread::Builder::new()
    .stack_size(8 * 1024 * 1024)
    .spawn(|| {
      let mut renderer = FastRender::new().expect("renderer");

      // Missing doctype triggers quirks mode; class selectors should match ASCII case-insensitively.
      let quirks_html = "<html><body><div class='item'></div></body></html>";
      let quirks_dom = renderer.parse_html(quirks_html).expect("parse quirks");
      let quirks_results = renderer
        .inspect(
          &quirks_dom,
          100,
          100,
          InspectQuery::Selector(".ITEM".to_string()),
        )
        .expect("quirks inspection");
      assert_eq!(
        quirks_results.len(),
        1,
        "quirks mode should match case-insensitive classes"
      );

      // Standards mode keeps class matching case-sensitive.
      let standards_html = "<!doctype html><html><body><div class='item'></div></body></html>";
      let standards_dom = renderer
        .parse_html(standards_html)
        .expect("parse standards");
      let standards_results = renderer
        .inspect(
          &standards_dom,
          100,
          100,
          InspectQuery::Selector(".ITEM".to_string()),
        )
        .expect("standards inspection");
      assert!(
        standards_results.is_empty(),
        "standards mode should keep class matching case-sensitive"
      );
    })
    .expect("spawn test thread")
    .join()
    .expect("join test thread");
}
