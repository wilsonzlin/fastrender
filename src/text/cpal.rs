use crate::css::types::FontPaletteBase;
use crate::style::types::FontPalette;

pub use crate::text::color_fonts::{
  parse_cpal_palette as parse_cpal_palette_data, select_cpal_palette, ParsedPalette,
};

/// Parse a CPAL palette from a font face using the canonical color-font parser.
pub fn parse_cpal_palette(
  face: &ttf_parser::Face<'_>,
  palette_index: u16,
) -> Option<ParsedPalette> {
  face
    .raw_face()
    .table(ttf_parser::Tag::from_bytes(b"CPAL"))
    .and_then(|data| parse_cpal_palette_data(data, palette_index))
}

/// Select a CPAL palette index for a face using CSS font-palette preferences.
///
/// The mapping from [`FontPalette`] to [`FontPaletteBase`] is kept here so callers
/// that reach for `text::cpal` continue to hit the shared CPAL implementation in
/// `text::color_fonts::cpal`.
pub fn select_palette_index(face: &ttf_parser::Face<'_>, preference: FontPalette) -> u16 {
  let base = match preference {
    FontPalette::Normal => FontPaletteBase::Normal,
    FontPalette::Light => FontPaletteBase::Light,
    FontPalette::Dark => FontPaletteBase::Dark,
    // Named palettes should have been resolved into a base palette via @font-palette-values.
    FontPalette::Named(_) => FontPaletteBase::Normal,
  };
  select_cpal_palette(face, base)
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::fs;

  #[test]
  fn maps_font_palette_variants_to_base_selection() {
    let data = fs::read("tests/fixtures/fonts/PaletteTestCOLRv1.ttf")
      .expect("test font with palette types should load");
    let face = ttf_parser::Face::parse(&data, 0).expect("test font should parse");

    assert_eq!(select_palette_index(&face, FontPalette::Normal), 0);
    assert_eq!(select_palette_index(&face, FontPalette::Light), 0);
    assert_eq!(select_palette_index(&face, FontPalette::Dark), 1);
    assert_eq!(
      select_palette_index(&face, FontPalette::Named("custom".into())),
      0
    );
  }

  #[test]
  fn parses_palette_via_color_font_module() {
    let data = fs::read("tests/fixtures/fonts/PaletteTestCOLRv1.ttf")
      .expect("test font with palette types should load");
    let face = ttf_parser::Face::parse(&data, 0).expect("test font should parse");

    let palette = parse_cpal_palette(&face, 1).expect("palette should parse");
    assert!(!palette.colors.is_empty());
    assert_eq!(palette.palette_type, Some(0x0000_0002));
  }
}
