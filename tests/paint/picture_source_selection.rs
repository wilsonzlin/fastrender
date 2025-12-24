use base64::prelude::BASE64_STANDARD;
use base64::Engine as _;
use fastrender::api::FastRender;
use image::codecs::png::PngEncoder;
use image::ExtendedColorType;
use image::ImageEncoder;

fn solid_color_png(r: u8, g: u8, b: u8, a: u8) -> String {
  let mut buf = Vec::new();
  PngEncoder::new(&mut buf)
    .write_image(&[r, g, b, a], 1, 1, ExtendedColorType::Rgba8)
    .expect("encode png");
  format!("data:image/png;base64,{}", BASE64_STANDARD.encode(&buf))
}

#[test]
fn picture_prefers_source_over_img_fallback_for_dpr() {
  let green = solid_color_png(0, 255, 0, 255);
  let red = solid_color_png(255, 0, 0, 255);
  let blue = solid_color_png(0, 0, 255, 255);

  let html = format!(
    r#"
    <style>body {{ margin: 0; }}</style>
    <picture>
      <source srcset="{green} 2x, {red} 1x" type="image/png">
      <img src="{blue}" srcset="{blue} 1x" width="10" height="10">
    </picture>
    "#
  );

  let mut renderer = FastRender::builder()
    .device_pixel_ratio(2.0)
    .build()
    .expect("renderer");
  let pixmap = renderer.render_html(&html, 10, 10).expect("render picture");

  let px = pixmap.pixel(5, 5).expect("center pixel");
  assert_eq!((px.red(), px.green(), px.blue()), (0, 255, 0));
}
