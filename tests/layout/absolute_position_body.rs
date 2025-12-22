use fastrender::FastRender;

fn is_all_white(pixmap: &resvg::tiny_skia::Pixmap) -> bool {
  // Expect white background (255,255,255,255) when nothing is painted.
  // If any pixel differs, content rendered.
  pixmap
    .data()
    .chunks_exact(4)
    .all(|px| px == [255, 255, 255, 255])
}

fn pixel(pixmap: &resvg::tiny_skia::Pixmap, x: u32, y: u32) -> [u8; 4] {
  let idx = (y as usize * pixmap.width() as usize + x as usize) * 4;
  let data = pixmap.data();
  [data[idx], data[idx + 1], data[idx + 2], data[idx + 3]]
}

fn is_red(pixel: [u8; 4]) -> bool {
  pixel == [255, 0, 0, 255]
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

#[test]
fn absolute_inset_auto_width_fills_parent() {
  std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(|| {
            let mut renderer = FastRender::new().expect("renderer");
            let html = r#"
            <style>
              body { margin: 0; }
              .parent { position: relative; width: 300px; height: 80px; background: white; }
              .child { position: absolute; left: 0; right: 0; top: 0; height: 30px; background: rgb(0, 255, 0); }
            </style>
            <div class="parent"><div class="child"></div></div>
            "#;
            let pixmap = renderer.render_html(html, 400, 120).expect("render");
            assert_eq!(
                pixel(&pixmap, 150, 10),
                [0, 255, 0, 255],
                "absolute inset element should span its parent's width"
            );
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn fixed_flex_container_centers_children_with_insets() {
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(|| {
      let mut renderer = FastRender::new().expect("renderer");
      let html = r#"
            <style>
              body { margin: 0; background: white; }
              .overlay {
                position: fixed;
                top: 0;
                left: 0;
                right: 0;
                bottom: 0;
                display: flex;
                flex-direction: column;
                align-items: center;
                justify-content: center;
              }
              .message { width: 120px; height: 20px; background: rgb(255, 0, 0); }
            </style>
            <div class="overlay"><div class="message"></div></div>
            "#;

      let pixmap = renderer.render_html(html, 200, 200).expect("render");

      // Centered vertically means the red bar should appear near the middle, not stuck at the top.
      assert!(
        is_red(pixel(&pixmap, 100, 100)),
        "center pixel should be red"
      );
      assert!(
        !is_red(pixel(&pixmap, 100, 10)),
        "top padding should remain white when justified center"
      );
    })
    .unwrap()
    .join()
    .unwrap();
}

#[test]
fn fixed_inside_positioned_parent_uses_viewport() {
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(|| {
      let mut renderer = FastRender::new().expect("renderer");
      let html = r#"
            <style>
              body { margin: 0; background: white; }
              .container { position: relative; width: 120px; height: 120px; }
              .fixed { position: fixed; top: 0; left: 0; right: 0; height: 30px; background: rgb(255, 0, 0); }
            </style>
            <div class="container"><div class="fixed"></div></div>
            "#;

      // If the fixed element incorrectly uses the positioned parent, pixels outside the parent's
      // width would remain white. The viewport is 200px wide, so x=150 should be inside the bar.
      let pixmap = renderer.render_html(html, 200, 120).expect("render");
      assert!(is_red(pixel(&pixmap, 150, 10)), "fixed bar should span the viewport");
    })
    .unwrap()
    .join()
    .unwrap();
}

#[test]
fn fixed_inside_transformed_parent_uses_transformed_cb() {
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(|| {
      let mut renderer = FastRender::new().expect("renderer");
      let html = r#"
            <style>
              body { margin: 0; background: white; }
              .transformed { transform: translateZ(0); width: 120px; height: 120px; }
              .fixed { position: fixed; top: 0; left: 0; right: 0; height: 30px; background: rgb(255, 0, 0); }
            </style>
            <div class="transformed"><div class="fixed"></div></div>
            "#;

      // When a transform is present, it establishes the containing block for fixed descendants.
      // The bar should be constrained to 120px, so x=150 should remain white.
      let pixmap = renderer.render_html(html, 200, 120).expect("render");
      assert!(!is_red(pixel(&pixmap, 150, 10)), "transform should capture fixed containing block");
      assert!(is_red(pixel(&pixmap, 60, 10)), "bar should still render inside transformed ancestor");
    })
    .unwrap()
    .join()
    .unwrap();
}
