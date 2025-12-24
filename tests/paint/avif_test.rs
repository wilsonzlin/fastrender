use fastrender::FastRender;
use std::path::PathBuf;
use tiny_skia::Pixmap;
use url::Url;

fn pixel_rgba(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let p = pixmap.pixel(x, y).unwrap();
  (p.red(), p.green(), p.blue(), p.alpha())
}

fn avif_fixture_url(name: &str) -> String {
  let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests/fixtures/avif")
    .join(name);
  Url::from_file_path(path).unwrap().to_string()
}

#[test]
fn background_image_set_renders_avif_candidate() {
  let mut renderer = FastRender::new().expect("renderer");
  let avif_url = avif_fixture_url("solid.avif");
  let webp_url = avif_fixture_url("solid.webp");

  let html = format!(
    r#"
    <style>
      body {{
        margin: 0;
        width: 8px;
        height: 8px;
        background-image: image-set(url("{avif}") 1x, url("{webp}") 2x);
        background-size: cover;
      }}
    </style>
	    <div style="width: 8px; height: 8px;"></div>
	    "#,
    avif = avif_url,
    webp = webp_url
  );

  let pixmap = renderer
    .render_html(&html, 8, 8)
    .expect("render avif background");
  let (r, g, b, _) = pixel_rgba(&pixmap, 4, 4);

  assert!(g > 170, "background should preserve green channel (g={g})");
  assert!(
    r < 60 && b < 60,
    "background should favor green over red/blue (r={r}, b={b})"
  );
}

#[test]
fn img_element_renders_avif_source() {
  let mut renderer = FastRender::new().expect("renderer");
  let avif_url = avif_fixture_url("solid.avif");

  let html = format!(
    r#"
    <style>
      body {{ margin: 0; }}
      img {{ display: block; width: 8px; height: 8px; }}
    </style>
    <img src="{src}" alt="avif" />
    "#,
    src = avif_url
  );

  let pixmap = renderer
    .render_html(&html, 8, 8)
    .expect("render avif image");
  let (r, g, b, a) = pixel_rgba(&pixmap, 3, 3);

  assert_eq!(a, 255, "image pixels should be opaque");
  assert!(g > 170, "img element should decode avif (g={g})");
  assert!(
    r < 60 && b < 60,
    "img element should preserve green tint (r={r}, b={b})"
  );
}
