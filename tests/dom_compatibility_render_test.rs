use fastrender::dom::DomCompatibilityMode;
use fastrender::{FastRender, RenderOptions, ResourcePolicy};
use std::fs;
use std::path::PathBuf;
use tiny_skia::Pixmap;
use url::Url;

fn get_pixel(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let idx = (y * pixmap.width() + x) as usize * 4;
  let data = pixmap.data();
  let a = data[idx + 3];
  if a == 0 {
    return (0, 0, 0, 0);
  }
  // tiny-skia uses premultiplied alpha.
  let r = ((data[idx] as u16 * 255) / a as u16) as u8;
  let g = ((data[idx + 1] as u16 * 255) / a as u16) as u8;
  let b = ((data[idx + 2] as u16 * 255) / a as u16) as u8;
  (r, g, b, a)
}

#[test]
fn dom_compatibility_lifts_lazy_loaded_images_at_render_time() {
  let fixture_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests/pages/fixtures/dom_compat_lazy_load");
  let html_path = fixture_dir.join("index.html");
  let html = fs::read_to_string(&html_path).expect("read fixture HTML");

  let base_url = Url::from_directory_path(&fixture_dir)
    .expect("build file:// base url")
    .to_string();
  let policy = ResourcePolicy::default()
    .allow_http(false)
    .allow_https(false)
    .allow_file(true)
    .allow_data(true);

  let mut standard = FastRender::builder()
    .base_url(base_url.clone())
    .resource_policy(policy.clone())
    .dom_compatibility_mode(DomCompatibilityMode::Standard)
    .build()
    .expect("build standard renderer");

  let mut compat = FastRender::builder()
    .base_url(base_url)
    .resource_policy(policy)
    .dom_compatibility_mode(DomCompatibilityMode::Compatibility)
    .build()
    .expect("build compat renderer");

  let standard_pixmap = standard
    .render_html_with_options(&html, RenderOptions::new().with_viewport(64, 64))
    .expect("render standard fixture");
  let compat_pixmap = compat
    .render_html_with_options(&html, RenderOptions::new().with_viewport(64, 64))
    .expect("render compat fixture");

  let is_red = |r: u8, g: u8, b: u8| r > 200 && g < 80 && b < 80;
  let is_blue = |r: u8, g: u8, b: u8| b > 200 && r < 80 && g < 80;

  // Without compatibility mode the images should stay on their placeholders, so we shouldn't see
  // the red/blue SVGs.
  let (r, g, b, _) = get_pixel(&standard_pixmap, 32, 16);
  assert!(
    !is_red(r, g, b),
    "standard render should not load the lazy img; got rgb({r},{g},{b})"
  );
  let (r, g, b, _) = get_pixel(&standard_pixmap, 32, 48);
  assert!(
    !is_blue(r, g, b),
    "standard render should not load the lazy picture source; got rgb({r},{g},{b})"
  );

  // With compatibility mode, the top half should be replaced by a red SVG (data-src) and the
  // bottom half by a blue SVG (<picture><source data-srcset>).
  let (r, g, b, _) = get_pixel(&compat_pixmap, 32, 16);
  assert!(
    is_red(r, g, b),
    "compat render should load the lazy img (red); got rgb({r},{g},{b})"
  );
  let (r, g, b, _) = get_pixel(&compat_pixmap, 32, 48);
  assert!(
    is_blue(r, g, b),
    "compat render should load the lazy picture source (blue); got rgb({r},{g},{b})"
  );
}
