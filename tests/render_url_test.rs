use fastrender::api::FastRender;
use tiny_skia::Color;

fn pixel(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let px = pixmap.pixel(x, y).unwrap();
  (px.red(), px.green(), px.blue(), px.alpha())
}

#[test]
fn render_url_loads_external_css_and_images_from_file() {
  let dir = tempfile::tempdir().expect("temp dir");

  let css = r#"
    body { margin: 0; background: rgb(20, 30, 40); }
    img { display: block; }
  "#;
  std::fs::write(dir.path().join("styles.css"), css).expect("write css");

  let images_dir = dir.path().join("images");
  std::fs::create_dir_all(&images_dir).expect("images dir");
  let image_path = images_dir.join("green.png");
  let mut png = fastrender::Pixmap::new(10, 10).expect("png");
  png.fill(Color::from_rgba8(0, 200, 0, 255));
  png.save_png(&image_path).expect("save png");

  let html = r#"
    <html>
      <head>
        <link rel="stylesheet" href="styles.css">
      </head>
      <body>
        <img src="images/green.png" alt="green square">
      </body>
    </html>
  "#;
  let html_path = dir.path().join("page.html");
  std::fs::write(&html_path, html).expect("write html");

  let file_url = format!("file://{}", html_path.display());
  let mut renderer = FastRender::new().expect("renderer");
  let result = renderer.render_url(&file_url).expect("render url");
  let pixmap = result.pixmap;

  assert_eq!(pixel(&pixmap, 1, 1), (0, 200, 0, 255));
  assert_eq!(pixel(&pixmap, 15, 15), (20, 30, 40, 255));
}
