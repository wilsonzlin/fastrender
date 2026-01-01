use fastrender::FontDatabase;

#[test]
fn bundled_font_coverage_includes_pageset_must_cover_codepoints() {
  let db = FontDatabase::shared_bundled();

  let must_cover = [
    ("Myanmar sample U+1019", char::from_u32(0x1019).expect("Myanmar scalar")),
    ("Telugu sample U+0C24", char::from_u32(0x0C24).expect("Telugu scalar")),
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
