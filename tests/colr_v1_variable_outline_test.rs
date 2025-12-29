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

fn load_variable_outline_font() -> LoadedFont {
  let data = std::fs::read(fixtures_path().join("fonts/colrv1-var-outline-test.ttf")).unwrap();
  LoadedFont {
    id: None,
    data: Arc::new(data),
    index: 0,
    family: "ColrV1VarOutlineTest".into(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  }
}

fn render_glyph(
  font: &LoadedFont,
  text_color: Rgba,
  variations: &[Variation],
) -> fastrender::text::color_fonts::ColorGlyphRaster {
  let face = font.as_ttf_face().unwrap();
  let gid = face.glyph_index('A').unwrap();
  let instance = FontInstance::new(font, variations).unwrap();

  ColorFontRenderer::new()
    .render(
      font,
      &instance,
      gid.0 as u32,
      64.0,
      0,
      &[],
      0,
      text_color,
      0.0,
      variations,
      None,
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

  // Round-trip the rendered pixmap through PNG encoding/decoding so the comparison matches the
  // golden load path (unpremultiply/premultiply) rather than raw in-memory pixels.
  let current_path = path.with_extension("actual.png");
  raster
    .image
    .save_png(&current_path)
    .expect("failed to write actual render");
  let golden = load_png(&path).expect("missing golden image; set UPDATE_GOLDEN=1 to create");
  let actual = load_png(&current_path).expect("failed to load actual render");
  let diff = compare_images(&actual, &golden, &CompareConfig::strict());
  let _ = std::fs::remove_file(&current_path);
  assert!(
    diff.is_match(),
    "rendered image {} did not match golden: {:?}",
    name,
    diff.statistics
  );
}

#[test]
fn variable_outline_changes_with_axis() {
  let font = load_variable_outline_font();
  let text_color = Rgba::from_rgba8(40, 80, 210, 255);

  let base = render_glyph(&font, text_color, &[]);
  let varied = render_glyph(
    &font,
    text_color,
    &[Variation {
      tag: Tag::from_bytes(b"wght"),
      value: 1.0,
    }],
  );

  save_or_compare("colrv1_var_outline_wght0.png", &base);
  save_or_compare("colrv1_var_outline_wght1.png", &varied);

  let diff = compare_images(&base.image, &varied.image, &CompareConfig::strict());
  assert!(
    !diff.is_match(),
    "variable render should differ between outline instances"
  );
}
