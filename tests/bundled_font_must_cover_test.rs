use fastrender::FontDatabase;

#[test]
fn bundled_font_coverage_includes_pageset_must_cover_codepoints() {
  let db = FontDatabase::shared_bundled();

  let must_cover = [
    ("Myanmar sample U+1019", char::from_u32(0x1019).expect("Myanmar scalar")),
    ("Telugu sample U+0C24", char::from_u32(0x0C24).expect("Telugu scalar")),
    (
      "Gujarati sample U+0A97",
      char::from_u32(0x0A97).expect("Gujarati scalar"),
    ),
    (
      "Gujarati sample U+0AC1",
      char::from_u32(0x0AC1).expect("Gujarati scalar"),
    ),
    (
      "Armenian sample U+0540",
      char::from_u32(0x0540).expect("Armenian scalar"),
    ),
    (
      "Ethiopic sample U+12A0",
      char::from_u32(0x12A0).expect("Ethiopic scalar"),
    ),
    (
      "Tibetan sample U+0F44",
      char::from_u32(0x0F44).expect("Tibetan scalar"),
    ),
    (
      "Regional indicator U+1F1FA",
      char::from_u32(0x1F1FA).expect("regional indicator scalar"),
    ),
    (
      "Regional indicator U+1F1F8",
      char::from_u32(0x1F1F8).expect("regional indicator scalar"),
    ),
  ];

  for (label, ch) in must_cover {
    assert!(
      db.any_face_has_glyph_cached(ch),
      "{label} should be covered by bundled fonts"
    );
  }
}
