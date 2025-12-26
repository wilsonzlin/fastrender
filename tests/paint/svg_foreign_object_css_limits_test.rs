use super::svg_inline_test::{pixel, serialized_inline_svg};
use fastrender::FastRender;

#[test]
fn foreign_object_shared_css_respects_limit() {
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(|| {
      let large_css = "body { color: black; }\n".repeat(20_000);
      let html = format!(
        r#"
        <style>
          body {{ margin: 0; background: white; }}
          {}
        </style>
        <svg width="32" height="16" viewBox="0 0 32 16">
          <foreignObject x="0" y="0" width="16" height="16">
            <div xmlns="http://www.w3.org/1999/xhtml" style="width:16px;height:16px;background: rgb(255, 0, 0);"></div>
          </foreignObject>
        </svg>
        "#,
        large_css
      );

      let serialized =
        serialized_inline_svg(&html, 32.0, 16.0).expect("serialize svg with foreignObject");
      assert!(
        serialized.shared_css.is_empty(),
        "shared CSS should be dropped when it exceeds the limit (len={})",
        large_css.len()
      );

      let mut renderer = FastRender::new().expect("renderer");
      let pixmap = renderer
        .render_html(&html, 32, 16)
        .expect("render svg with large CSS");
      assert_eq!(pixel(&pixmap, 8, 8), [255, 0, 0, 255]);
    })
    .unwrap()
    .join()
    .unwrap();
}

#[test]
fn foreign_object_css_sanitizes_style_tag_sequences() {
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(|| {
      let html = r#"
      <body style="margin: 0">
        <svg width="16" height="16" viewBox="0 0 16 16">
          <style><![CDATA[
            /* stray </style> token that must not terminate the style element */
            .embed {
              width: 12px;
              height: 12px;
              background: rgb(0, 255, 0);
            }
          ]]></style>
          <foreignObject x="0" y="0" width="16" height="16">
            <div xmlns="http://www.w3.org/1999/xhtml" class="embed"></div>
          </foreignObject>
        </svg>
      </body>
      "#;

      let serialized =
        serialized_inline_svg(html, 16.0, 16.0).expect("serialize svg with foreignObject");
      assert!(
        !serialized.shared_css.is_empty(),
        "CSS under the limit should be preserved for foreignObject rendering"
      );
      assert!(
        serialized
          .shared_css
          .to_ascii_lowercase()
          .contains("</style"),
        "shared CSS should retain literal </style> sequences for sanitization"
      );

      let mut renderer = FastRender::new().expect("renderer");
      let pixmap = renderer
        .render_html(html, 16, 16)
        .expect("render foreignObject with sanitized CSS");
      assert_eq!(pixel(&pixmap, 8, 8), [0, 255, 0, 255]);
    })
    .unwrap()
    .join()
    .unwrap();
}
