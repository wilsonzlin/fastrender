use fastrender::api::FastRender;

#[test]
fn root_font_size_percentage_uses_initial_value() {
    // Root font-size percentages resolve against the initial font size (16px by default),
    // so 62.5% should yield 10px. rem lengths should then scale from that root size.
    let html = r#"
        <style>
            html { font-size: 62.5%; }
            body { margin: 0; }
            .target {
                width: 54.6rem;
                height: 32.5rem;
                background: red;
            }
        </style>
        <div class="target"></div>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let pixmap = renderer.render_html(html, 1200, 800).unwrap();
    let data = pixmap.data();

    let mut min_x = 1200;
    let mut min_y = 800;
    let mut max_x = 0;
    let mut max_y = 0;

    for y in 0..800 {
        for x in 0..1200 {
            let i = (y * 1200 + x) * 4;
            let r = data[i];
            let g = data[i + 1];
            let b = data[i + 2];
            let a = data[i + 3];
            // Background is white; identify painted content via non-white, non-transparent pixels.
            if a > 0 && (r, g, b) != (255, 255, 255) {
                min_x = min_x.min(x);
                min_y = min_y.min(y);
                max_x = max_x.max(x);
                max_y = max_y.max(y);
            }
        }
    }

    let painted_width = if max_x >= min_x { max_x - min_x + 1 } else { 0 };
    let painted_height = if max_y >= min_y { max_y - min_y + 1 } else { 0 };

    assert!(
        (painted_width as f32 - 546.0).abs() < 1.0,
        "expected ~546px width, got {}",
        painted_width
    );
    assert!(
        (painted_height as f32 - 325.0).abs() < 1.0,
        "expected ~325px height, got {}",
        painted_height
    );
}
