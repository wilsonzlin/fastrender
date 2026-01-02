use fastrender::FontDatabase;

#[test]
fn bundled_font_coverage_includes_pageset_must_cover_codepoints() {
  let db = FontDatabase::shared_bundled();

  let must_cover = [
    ("Myanmar sample U+1019", char::from_u32(0x1019).expect("Myanmar scalar")),
    ("Telugu sample U+0C24", char::from_u32(0x0C24).expect("Telugu scalar")),
    (
      "Celsius sign U+2103",
      char::from_u32(0x2103).expect("Celsius scalar"),
    ),
    (
      "Box drawing sample U+250A",
      char::from_u32(0x250A).expect("box drawing scalar"),
    ),
    (
      "Small form variant sample U+FE5D",
      char::from_u32(0xFE5D).expect("small form variant scalar"),
    ),
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
    // Pageset emoji/symbol regressions (e.g. ebay.com, espn.com) that should never hit the
    // last-resort tofu path when using bundled fonts.
    ("Trigram for water U+2636", char::from_u32(0x2636).expect("emoji scalar")),
    (
      "Black right-pointing triangle U+25B6",
      char::from_u32(0x25B6).expect("emoji scalar"),
    ),
    ("Check mark button U+2705", char::from_u32(0x2705).expect("emoji scalar")),
    ("Writing hand U+270D", char::from_u32(0x270D).expect("emoji scalar")),
    ("Black four pointed star U+2726", char::from_u32(0x2726).expect("emoji scalar")),
    ("Sparkles U+2728", char::from_u32(0x2728).expect("emoji scalar")),
    (
      "Heavy left-pointing angle quotation mark ornament U+276E",
      char::from_u32(0x276E).expect("emoji scalar"),
    ),
    (
      "Heavy right-pointing angle quotation mark ornament U+276F",
      char::from_u32(0x276F).expect("emoji scalar"),
    ),
    ("Black diamond minus white x U+2756", char::from_u32(0x2756).expect("emoji scalar")),
    ("Up arrow U+2B06", char::from_u32(0x2B06).expect("emoji scalar")),
    ("Down arrow U+2B07", char::from_u32(0x2B07).expect("emoji scalar")),
    ("White medium star U+2B50", char::from_u32(0x2B50).expect("emoji scalar")),
    ("Crystal ball U+1F52E", char::from_u32(0x1F52E).expect("emoji scalar")),
    ("Police car light U+1F6A8", char::from_u32(0x1F6A8).expect("emoji scalar")),
    ("Party popper U+1F389", char::from_u32(0x1F389).expect("emoji scalar")),
    ("Confetti ball U+1F38A", char::from_u32(0x1F38A).expect("emoji scalar")),
    ("Trophy U+1F3C6", char::from_u32(0x1F3C6).expect("emoji scalar")),
    ("Goat U+1F410", char::from_u32(0x1F410).expect("emoji scalar")),
    ("Calendar U+1F4C5", char::from_u32(0x1F4C5).expect("emoji scalar")),
    ("Round pushpin U+1F4CD", char::from_u32(0x1F4CD).expect("emoji scalar")),
    ("Growing heart U+1F497", char::from_u32(0x1F497).expect("emoji scalar")),
    ("Fire U+1F525", char::from_u32(0x1F525).expect("emoji scalar")),
    ("Thinking face U+1F914", char::from_u32(0x1F914).expect("emoji scalar")),
    (
      "Cowboy hat face U+1F920",
      char::from_u32(0x1F920).expect("emoji scalar"),
    ),
    ("Handbag U+1F45C", char::from_u32(0x1F45C).expect("emoji scalar")),
    // Pageset icon font / private-use regressions that should not hit last-resort tofu when using
    // bundled fonts.
    ("Braille pattern U+28FE", char::from_u32(0x28FE).expect("braille scalar")),
    ("Microsoft PUA U+E909", char::from_u32(0xE909).expect("PUA scalar")),
    ("HBR PUA U+E021", char::from_u32(0xE021).expect("PUA scalar")),
    ("HBR PUA U+E022", char::from_u32(0xE022).expect("PUA scalar")),
    ("HBR PUA U+E031", char::from_u32(0xE031).expect("PUA scalar")),
    ("HBR PUA U+E083", char::from_u32(0xE083).expect("PUA scalar")),
    ("HBR PUA U+E085", char::from_u32(0xE085).expect("PUA scalar")),
    ("Apple PUA U+F301", char::from_u32(0xF301).expect("PUA scalar")),
    ("Apple PUA U+F8FF", char::from_u32(0xF8FF).expect("PUA scalar")),
  ];

  for (label, ch) in must_cover {
    assert!(
      db.any_face_has_glyph_cached(ch),
      "{label} should be covered by bundled fonts"
    );
  }
}
