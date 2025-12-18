use fastrender::FastRender;

fn is_all_white(pixmap: &resvg::tiny_skia::Pixmap) -> bool {
    // Expect white background (255,255,255,255) when nothing is painted.
    // If any pixel differs, content rendered.
    pixmap.data().chunks_exact(4).all(|px| px == [255, 255, 255, 255])
}

fn pixel(pixmap: &resvg::tiny_skia::Pixmap, x: u32, y: u32) -> [u8; 4] {
    let idx = (y as usize * pixmap.width() as usize + x as usize) * 4;
    let data = pixmap.data();
    [data[idx], data[idx + 1], data[idx + 2], data[idx + 3]]
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

#[test]
fn fixed_positioned_inset_auto_width_fills_viewport() {
    std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(|| {
            let mut renderer = FastRender::new().expect("renderer");
            let html = r#"
            <style>
              body { margin: 0; background: white; }
              .bar { position: fixed; left: 0; right: 0; top: 0; height: 40px; background: rgb(255, 0, 0); }
            </style>
            <div class="bar"></div>
            "#;
            let pixmap = renderer.render_html(html, 200, 100).expect("render");
            assert_eq!(
                pixel(&pixmap, 100, 10),
                [255, 0, 0, 255],
                "fixed inset element should span the viewport width"
            );
        })
        .unwrap()
        .join()
        .unwrap();
}
