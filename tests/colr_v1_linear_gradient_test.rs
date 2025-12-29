mod r#ref;

use fastrender::image_compare::encode_png;
use fastrender::style::color::Rgba;
use fastrender::text::color_fonts::{ColorFontRenderer, ColorGlyphRaster};
use fastrender::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
use fastrender::text::font_instance::FontInstance;
use image::RgbaImage;
use r#ref::compare::{compare_images, load_png, load_png_from_bytes, CompareConfig};
use std::path::PathBuf;
use std::sync::Arc;
use tiny_skia::Pixmap;

fn fixtures_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("fixtures")
}

fn load_sheared_font() -> LoadedFont {
  let data = std::fs::read(fixtures_path().join("fonts/colrv1-linear-shear.ttf")).unwrap();
  LoadedFont {
    id: None,
    data: Arc::new(data),
    index: 0,
    family: "ColrV1LinearShear".into(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  }
}

fn render_sheared_glyph(font: &LoadedFont) -> ColorGlyphRaster {
  let face = font.as_ttf_face().unwrap();
  let gid = face.glyph_index('G').unwrap();
  let instance = FontInstance::new(font, &[]).unwrap();

  ColorFontRenderer::new()
    .render(
      font,
      &instance,
      gid.0 as u32,
      64.0,
      0,
      &[],
      0,
      Rgba::BLACK,
      0.0,
      &[],
      None,
    )
    .expect("expected color glyph")
}

fn save_or_compare(name: &str, raster: &ColorGlyphRaster) {
  let path = fixtures_path().join("golden").join(name);
  if std::env::var("UPDATE_GOLDEN").is_ok() {
    let encoded =
      encode_png(&pixmap_to_rgba_image(&raster.image)).expect("failed to encode golden");
    std::fs::write(&path, encoded).expect("failed to write golden");
  }

  let golden = load_png(&path).expect("missing golden image; set UPDATE_GOLDEN=1 to create");
  let actual = load_png_from_bytes(
    &encode_png(&pixmap_to_rgba_image(&raster.image)).expect("failed to encode render to PNG"),
  )
  .expect("failed to decode encoded render");
  let byte_mismatches = actual
    .data()
    .iter()
    .zip(golden.data())
    .filter(|(a, b)| a != b)
    .count();
  let diff = compare_images(&actual, &golden, &CompareConfig::strict());
  assert!(
    diff.is_match(),
    "rendered image {} did not match golden ({} byte mismatches): {:?}\n{}",
    name,
    byte_mismatches,
    diff.statistics,
    sample_pixels(&raster.image, &golden),
  );
}

fn pixmap_to_rgba_image(pixmap: &Pixmap) -> RgbaImage {
  let width = pixmap.width();
  let height = pixmap.height();
  let mut rgba = RgbaImage::new(width, height);

  for (dst, src) in rgba
    .as_mut()
    .chunks_exact_mut(4)
    .zip(pixmap.data().chunks_exact(4))
  {
    let r = src[0];
    let g = src[1];
    let b = src[2];
    let a = src[3];

    if a == 0 {
      dst.copy_from_slice(&[0, 0, 0, 0]);
      continue;
    }

    let alpha = a as f32 / 255.0;
    dst[0] = ((r as f32 / alpha).min(255.0)) as u8;
    dst[1] = ((g as f32 / alpha).min(255.0)) as u8;
    dst[2] = ((b as f32 / alpha).min(255.0)) as u8;
    dst[3] = a;
  }

  rgba
}

fn sample_pixels(actual: &Pixmap, expected: &Pixmap) -> String {
  let coords = [
    (0, 0),
    (actual.width() / 2, actual.height() / 2),
    (
      actual.width().saturating_sub(1),
      actual.height().saturating_sub(1),
    ),
  ];
  let mut lines = Vec::new();
  for (x, y) in coords {
    let idx = ((y as usize * actual.width() as usize) + x as usize) * 4;
    let actual_px = [
      actual.data()[idx],
      actual.data()[idx + 1],
      actual.data()[idx + 2],
      actual.data()[idx + 3],
    ];
    let expected_px = [
      expected.data()[idx],
      expected.data()[idx + 1],
      expected.data()[idx + 2],
      expected.data()[idx + 3],
    ];
    lines.push(format!(
      "sample ({}, {}): actual {:?}, expected {:?}",
      x, y, actual_px, expected_px
    ));
  }

  let mut mismatches = Vec::new();
  for y in 0..actual.height() {
    for x in 0..actual.width() {
      let idx = ((y as usize * actual.width() as usize) + x as usize) * 4;
      let actual_px = [
        actual.data()[idx],
        actual.data()[idx + 1],
        actual.data()[idx + 2],
        actual.data()[idx + 3],
      ];
      let expected_px = [
        expected.data()[idx],
        expected.data()[idx + 1],
        expected.data()[idx + 2],
        expected.data()[idx + 3],
      ];
      if actual_px != expected_px {
        mismatches.push(format!(
          "diff ({}, {}): actual {:?}, expected {:?}",
          x, y, actual_px, expected_px
        ));
        if mismatches.len() >= 5 {
          break;
        }
      }
    }
    if mismatches.len() >= 5 {
      break;
    }
  }

  lines.extend(mismatches);
  lines.join("\n")
}

#[test]
fn sheared_linear_gradient_respects_third_point() {
  let font = load_sheared_font();
  let raster = render_sheared_glyph(&font);
  save_or_compare("colrv1_linear_shear.png", &raster);
}
