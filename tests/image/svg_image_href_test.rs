use base64::engine::general_purpose::STANDARD;
use base64::Engine;
use fastrender::image_loader::ImageCache;
use image::{ImageFormat, Rgba, RgbaImage};
use std::fs;
use std::io::Cursor;
use url::Url;

#[test]
fn svg_image_href_resolves_against_svg_url() {
  let dir = tempfile::tempdir().expect("temp dir");
  let png_path = dir.path().join("img.png");
  let png = RgbaImage::from_pixel(4, 4, Rgba([255, 0, 0, 255]));
  png.save(&png_path).expect("write png");

  let svg_path = dir.path().join("icon.svg");
  let svg_content = r#"
    <svg xmlns="http://www.w3.org/2000/svg" width="4" height="4">
      <image href="img.png" width="4" height="4" />
    </svg>
  "#;
  fs::write(&svg_path, svg_content).expect("write svg");

  let svg_url = Url::from_file_path(&svg_path).unwrap().to_string();

  let mut cache = ImageCache::new();
  cache.set_base_url("file:///not-used-for-svg-base/");

  let image = cache.load(&svg_url).expect("render svg with image href");
  let rgba = image.image.to_rgba8();

  assert_eq!(rgba.dimensions(), (4, 4));
  assert_eq!(*rgba.get_pixel(0, 0), Rgba([255, 0, 0, 255]));
  assert_eq!(*rgba.get_pixel(3, 3), Rgba([255, 0, 0, 255]));
}

#[test]
fn svg_image_href_supports_data_url() {
  let mut cache = ImageCache::new();

  let data_image = RgbaImage::from_pixel(2, 2, Rgba([0, 0, 255, 255]));
  let mut buf = Vec::new();
  data_image
    .write_to(&mut Cursor::new(&mut buf), ImageFormat::Png)
    .expect("encode png");
  let data_url = format!("data:image/png;base64,{}", STANDARD.encode(&buf));

  let svg = format!(
    r#"<svg xmlns="http://www.w3.org/2000/svg" width="2" height="2">
        <image href="{data_url}" width="2" height="2" />
      </svg>"#
  );

  let (rendered, _, _) = cache.render_svg_to_image(&svg).expect("render svg");
  let rgba = rendered.to_rgba8();
  assert_eq!(*rgba.get_pixel(0, 0), Rgba([0, 0, 255, 255]));
  assert_eq!(*rgba.get_pixel(1, 1), Rgba([0, 0, 255, 255]));
}
