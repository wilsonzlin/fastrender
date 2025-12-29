mod r#ref;

use fastrender::style::color::Rgba;
use fastrender::text::color_fonts::{ColorFontRenderer, ColorGlyphRaster};
use fastrender::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
use fastrender::text::font_instance::FontInstance;
use image::RgbaImage;
use r#ref::compare::{compare_images, load_png, CompareConfig};
use std::path::PathBuf;
use std::sync::Arc;
use tiny_skia::Pixmap;

fn fixtures_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("fixtures")
}

fn load_sweep_font() -> LoadedFont {
  let data = std::fs::read(fixtures_path().join("fonts/colrv1-sweep-test.ttf")).unwrap();
  LoadedFont {
    id: None,
    data: Arc::new(data),
    index: 0,
    family: "ColrV1Sweep".into(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  }
}

fn render_glyph(font: &LoadedFont, ch: char) -> ColorGlyphRaster {
  let face = font.as_ttf_face().unwrap();
  let gid = face.glyph_index(ch).unwrap();
  let instance = FontInstance::new(font, &[]).unwrap();

  ColorFontRenderer::new()
    .render(
      font,
      &instance,
      gid.0 as u32,
      64.0,
      0,
      &[],
      Rgba::BLACK,
      0.0,
      &[],
      None,
    )
    .expect("expected color glyph")
}

fn to_straight_rgba(pixmap: &Pixmap) -> RgbaImage {
  let mut rgba = RgbaImage::new(pixmap.width(), pixmap.height());

  for (dst, src) in rgba
    .as_mut()
    .chunks_exact_mut(4)
    .zip(pixmap.data().chunks_exact(4))
  {
    let a = src[3];
    if a == 0 {
      dst.copy_from_slice(&[0, 0, 0, 0]);
      continue;
    }

    let alpha = a as f32 / 255.0;
    dst[0] = ((src[0] as f32 / alpha).min(255.0)) as u8;
    dst[1] = ((src[1] as f32 / alpha).min(255.0)) as u8;
    dst[2] = ((src[2] as f32 / alpha).min(255.0)) as u8;
    dst[3] = a;
  }

  rgba
}

fn save_unpremultiplied_png(path: &std::path::Path, raster: &ColorGlyphRaster) {
  to_straight_rgba(raster.image.as_ref())
    .save(path)
    .expect("failed to write png");
}

fn roundtrip_pixmap(pixmap: &Pixmap) -> Pixmap {
  let rgba = to_straight_rgba(pixmap);
  let mut roundtrip =
    Pixmap::new(rgba.width(), rgba.height()).expect("failed to allocate roundtrip pixmap");
  let src = rgba.as_raw();
  let dst = roundtrip.data_mut();

  for i in 0..(rgba.width() * rgba.height()) as usize {
    let src_idx = i * 4;
    let dst_idx = i * 4;
    let r = src[src_idx];
    let g = src[src_idx + 1];
    let b = src[src_idx + 2];
    let a = src[src_idx + 3];

    let alpha = a as f32 / 255.0;
    dst[dst_idx] = (r as f32 * alpha) as u8;
    dst[dst_idx + 1] = (g as f32 * alpha) as u8;
    dst[dst_idx + 2] = (b as f32 * alpha) as u8;
    dst[dst_idx + 3] = a;
  }

  roundtrip
}

fn save_or_compare(name: &str, raster: &ColorGlyphRaster) {
  let path = fixtures_path().join("golden").join(name);
  if std::env::var("UPDATE_GOLDEN").is_ok() {
    save_unpremultiplied_png(&path, raster);
    return;
  }

  if std::env::var("SAVE_ACTUAL").is_ok() {
    let actual_path = path.with_extension("actual.png");
    save_unpremultiplied_png(&actual_path, raster);
  }

  let golden = load_png(&path).expect("missing golden image; set UPDATE_GOLDEN=1 to create");
  let diff = compare_images(&roundtrip_pixmap(&raster.image), &golden, &CompareConfig::strict());
  assert!(
    diff.is_match(),
    "rendered image {} did not match golden: {:?}",
    name,
    diff.statistics
  );
}

#[test]
fn sweep_gradient_respects_transforms() {
  let font = load_sweep_font();
  let pad = render_glyph(&font, 'G');
  let pad_repeat = render_glyph(&font, 'G');
  let self_diff = compare_images(&pad.image, &pad_repeat.image, &CompareConfig::strict());
  assert!(
    self_diff.is_match(),
    "pad glyph rendering should be deterministic: {:?}",
    self_diff.statistics
  );
  // Glyph `J` wraps the sweep gradient in translate/scale/transform paints to exercise transform
  // accumulation in the sweep shader path.
  let transformed = render_glyph(&font, 'J');
  save_or_compare("colrv1_sweep_pad.png", &pad);
  save_or_compare("colrv1_sweep_transformed.png", &transformed);

  let diff = compare_images(&pad.image, &transformed.image, &CompareConfig::strict());
  assert!(
    !diff.is_match(),
    "transformed sweep gradient should differ from untransformed: {:?}",
    diff.statistics
  );
}
