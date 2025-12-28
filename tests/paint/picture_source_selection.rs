use base64::prelude::BASE64_STANDARD;
use base64::Engine as _;
use fastrender::api::FastRender;
use fastrender::RenderOptions;
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

#[test]
fn picture_filters_by_media_and_type() {
  let fallback = solid_color_png(255, 0, 0, 255);
  let unsupported = solid_color_png(0, 255, 0, 255);
  let supported = solid_color_png(0, 0, 255, 255);

  let html = format!(
    r#"
    <style>body {{ margin: 0; }}</style>
    <picture>
      <source srcset="{unsupported} 1x" media="(min-width: 1px)" type="image/unsupported">
      <source srcset="{supported} 1x" media="(min-width: 300px)" type="image/png">
      <img src="{fallback}" width="10" height="10">
    </picture>
    "#
  );

  let mut renderer = FastRender::new().expect("renderer");
  let narrow_options = RenderOptions::new().with_viewport(200, 200);
  let narrow = renderer
    .render_html_with_options(&html, narrow_options)
    .expect("render narrow picture");
  let narrow_px = narrow.pixel(5, 5).expect("narrow pixel");
  assert_eq!(
    (narrow_px.red(), narrow_px.green(), narrow_px.blue()),
    (255, 0, 0),
    "unsupported type should fall back to <img> when media fails"
  );

  let wide_options = RenderOptions::new().with_viewport(400, 200);
  let wide = renderer
    .render_html_with_options(&html, wide_options)
    .expect("render wide picture");
  let wide_px = wide.pixel(5, 5).expect("wide pixel");
  assert_eq!(
    (wide_px.red(), wide_px.green(), wide_px.blue()),
    (0, 0, 255),
    "supported source with matching media should be chosen"
  );
}
