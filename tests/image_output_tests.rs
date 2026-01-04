//! Tests for image encoding helpers

use fastrender::image_output::encode_image;
use fastrender::image_output::diff_png_with_alpha;
use fastrender::image_output::OutputFormat;
use fastrender::Pixmap;
use image::DynamicImage;
use image::ImageFormat;
use image::RgbaImage;
use std::io::Cursor;
use tiny_skia::ColorU8;

fn create_test_pixmap() -> Pixmap {
  let width = 8;
  let height = 8;
  let mut pixmap = Pixmap::new(width, height).expect("failed to create pixmap");

  for (idx, pixel) in pixmap.pixels_mut().iter_mut().enumerate() {
    let x = (idx as u32 % width) as u8;
    let y = (idx as u32 / width) as u8;
    let alpha = 128 + (((x as u16 + y as u16) / 2) as u8);
    // Vary all channels so lossy encoders have something to compress.
    let r = x.saturating_mul(20).wrapping_add(15);
    let g = y.saturating_mul(18).wrapping_add(30);
    let b = r ^ g ^ 0xAA;

    *pixel = ColorU8::from_rgba(r, g, b, alpha).premultiply();
  }

  pixmap
}

fn decode(bytes: &[u8]) -> DynamicImage {
  image::load_from_memory(bytes).expect("encoded bytes should be decodable")
}

fn encode_solid_png_bytes(color: [u8; 4]) -> Vec<u8> {
  let img = RgbaImage::from_pixel(2, 2, image::Rgba(color));
  let mut out = Vec::new();
  img
    .write_to(&mut Cursor::new(&mut out), ImageFormat::Png)
    .expect("encode png");
  out
}

#[test]
fn webp_respects_quality_setting() {
  let pixmap = create_test_pixmap();

  let low_quality = encode_image(&pixmap, OutputFormat::WebP(20)).expect("webp encode q20");
  let high_quality = encode_image(&pixmap, OutputFormat::WebP(90)).expect("webp encode q90");

  assert_ne!(
    low_quality, high_quality,
    "quality should affect output bytes"
  );

  let decoded_low = decode(&low_quality).to_rgba8();
  let decoded_high = decode(&high_quality).to_rgba8();

  assert_ne!(
    decoded_low.as_raw(),
    decoded_high.as_raw(),
    "quality should produce visibly different decoded pixels"
  );
}

#[test]
fn common_formats_encode_and_decode() {
  let pixmap = create_test_pixmap();

  let png = encode_image(&pixmap, OutputFormat::Png).expect("png encode");
  assert!(!png.is_empty(), "png output should not be empty");
  decode(&png);

  let jpeg = encode_image(&pixmap, OutputFormat::Jpeg(80)).expect("jpeg encode");
  assert!(!jpeg.is_empty(), "jpeg output should not be empty");
  decode(&jpeg);

  let webp = encode_image(&pixmap, OutputFormat::WebP(80)).expect("webp encode");
  assert!(!webp.is_empty(), "webp output should not be empty");
  decode(&webp);
}

#[test]
fn diff_png_reports_max_channel_delta() {
  let before = encode_solid_png_bytes([255, 0, 0, 255]);
  let after = encode_solid_png_bytes([0, 0, 255, 255]);

  let (metrics, diff) =
    diff_png_with_alpha(&after, &before, 0, true).expect("diff_png_with_alpha");
  assert!(metrics.pixel_diff > 0);
  assert_eq!(metrics.max_channel_diff, 255);
  assert!(!diff.is_empty(), "expected diff image bytes");
}

#[test]
fn diff_png_ignores_alpha_when_requested() {
  let before = encode_solid_png_bytes([0, 0, 0, 0]);
  let after = encode_solid_png_bytes([0, 0, 0, 255]);

  let (metrics, _diff) =
    diff_png_with_alpha(&after, &before, 0, false).expect("diff_png_with_alpha ignore alpha");
  assert_eq!(metrics.pixel_diff, 0);
  assert_eq!(
    metrics.max_channel_diff, 0,
    "max_channel_diff should ignore alpha when compare_alpha=false"
  );
}
