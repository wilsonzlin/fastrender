mod r#ref;

use fastrender::style::color::Rgba;
use fastrender::text::color_fonts::ColorFontRenderer;
use fastrender::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
use fastrender::text::font_instance::FontInstance;
use r#ref::compare::{compare_images, load_png, CompareConfig};
use rustybuzz::Variation;
use std::path::PathBuf;
use std::sync::Arc;
use ttf_parser::Tag;

fn fixtures_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("fixtures")
}

fn load_test_font() -> LoadedFont {
  let data = std::fs::read(fixtures_path().join("fonts/colrv1-test.ttf")).unwrap();
  LoadedFont {
    data: Arc::new(data),
    index: 0,
    family: "ColrV1Test".into(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  }
}

fn render_glyph(
  font: &LoadedFont,
  palette_index: u16,
  text_color: Rgba,
  variations: &[Variation],
) -> fastrender::text::color_fonts::ColorGlyphRaster {
  let face = font.as_ttf_face().unwrap();
  let gid = face.glyph_index('A').unwrap();
  let instance = FontInstance::new(font, variations).expect("font instance");

  ColorFontRenderer::new()
    .render(
      font,
      &instance,
      gid.0 as u32,
      64.0,
      palette_index,
      text_color,
      0.0,
      variations,
    )
    .expect("expected color glyph")
}

fn save_or_compare(name: &str, raster: &fastrender::text::color_fonts::ColorGlyphRaster) {
  let path = fixtures_path().join("golden").join(name);
  if std::env::var("UPDATE_GOLDEN").is_ok() {
    raster
      .image
      .save_png(&path)
      .expect("failed to write golden");
  }

  let golden = load_png(&path).expect("missing golden image; set UPDATE_GOLDEN=1 to create");
  let diff = compare_images(&raster.image, &golden, &CompareConfig::strict());
  assert!(
    diff.is_match(),
    "rendered image {} did not match golden: {:?}",
    name,
    diff.statistics
  );
}

#[test]
fn renders_colrv1_palette0_matches_golden() {
  let font = load_test_font();
  let raster = render_glyph(&font, 0, Rgba::from_rgba8(40, 60, 210, 255), &[]);
  save_or_compare("colrv1_palette0.png", &raster);
}

#[test]
fn palette_selection_updates_colors() {
  let font = load_test_font();
  let raster = render_glyph(&font, 1, Rgba::from_rgba8(40, 60, 210, 255), &[]);
  save_or_compare("colrv1_palette1.png", &raster);
}

#[test]
fn malformed_colrv1_table_falls_back() {
  let font = load_test_font();
  let mut data = (*font.data).clone();
  if let Some((offset, _)) = find_table(&data, b"COLR") {
    if offset + 2 <= data.len() {
      data[offset] = 0xFF;
      data[offset + 1] = 0xFF;
    }
  }
  let corrupted = LoadedFont {
    data: Arc::new(data),
    ..font
  };
  let instance =
    FontInstance::new(&corrupted, &[]).expect("corrupted COLR font should still parse instance");
  let face = corrupted.as_ttf_face().unwrap();
  let gid = face.glyph_index('A').unwrap();
  let rendered =
    ColorFontRenderer::new()
      .render(&corrupted, &instance, gid.0 as u32, 64.0, 0, Rgba::BLACK, 0.0, &[]);
  assert!(rendered.is_none(), "malformed COLR should not render");
}

fn load_variable_test_font() -> LoadedFont {
  let data = std::fs::read(fixtures_path().join("fonts/colrv1-var-test.ttf")).unwrap();
  LoadedFont {
    data: Arc::new(data),
    index: 0,
    family: "ColrV1VarTest".into(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  }
}

#[test]
fn variable_colrv1_changes_with_variations() {
  let font = load_variable_test_font();
  let text_color = Rgba::from_rgba8(30, 40, 220, 255);
  let base = render_glyph(&font, 0, text_color, &[]);
  let varied = render_glyph(
    &font,
    0,
    text_color,
    &[Variation {
      tag: Tag::from_bytes(b"wght"),
      value: 1.0,
    }],
  );
  save_or_compare("colrv1_var_default.png", &base);
  save_or_compare("colrv1_var_wght1.png", &varied);

  let diff = compare_images(&base.image, &varied.image, &CompareConfig::strict());
  assert!(
    !diff.is_match(),
    "variable render should differ between instances"
  );
}

fn find_table(data: &[u8], tag: &[u8; 4]) -> Option<(usize, usize)> {
  if data.len() < 12 {
    return None;
  }
  let num_tables = u16::from_be_bytes([data[4], data[5]]) as usize;
  let mut offset = 12;
  for _ in 0..num_tables {
    if offset + 16 > data.len() {
      return None;
    }
    let table_tag = &data[offset..offset + 4];
    let table_offset = u32::from_be_bytes([
      data[offset + 8],
      data[offset + 9],
      data[offset + 10],
      data[offset + 11],
    ]) as usize;
    let length = u32::from_be_bytes([
      data[offset + 12],
      data[offset + 13],
      data[offset + 14],
      data[offset + 15],
    ]) as usize;
    if table_tag == tag {
      return Some((table_offset, length));
    }
    offset += 16;
  }
  None
}
