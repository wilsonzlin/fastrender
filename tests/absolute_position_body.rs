use fastrender::FastRender;

fn is_all_white(pixmap: &resvg::tiny_skia::Pixmap) -> bool {
    // Expect white background (255,255,255,255) when nothing is painted.
    // If any pixel differs, content rendered.
    pixmap.data().chunks_exact(4).all(|px| px == [255, 255, 255, 255])
}

#[test]
fn absolutely_positioned_body_with_insets_renders_content() {
    std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(|| {
            let mut renderer = FastRender::new().expect("renderer");
            let html = r#"
            <html id='project-main'>
              <style>
              body {
                background: white;
                color: black;
                margin: 0;
              }
              #content { padding: 8px; background: #eef; }
              </style>
              <body style="position:absolute;top:0;left:0;right:0;bottom:0;height:100%;max-width:100%;margin:0;padding:0;">
                <div id="content">Hello world</div>
              </body>
            </html>
            "#;
            let pixmap = renderer.render_html(html, 800, 400).expect("render");
            assert!(
                !is_all_white(&pixmap),
                "expected content to render for absolutely positioned body"
            );
        })
        .unwrap()
        .join()
        .unwrap();
}
