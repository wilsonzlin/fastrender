use fastrender::{FontConfig, FontDatabase, FontStyleDb as FontStyle, FontWeightDb as FontWeight};

#[test]
fn bundled_fonts_cover_thaana_syriac_and_nko() {
  let db = FontDatabase::with_config(&FontConfig::bundled_only());

  let thaana = db
    .query("Noto Sans Thaana", FontWeight::NORMAL, FontStyle::Normal)
    .expect("bundled Noto Sans Thaana should be present");
  assert!(
    db.has_glyph(thaana, 'އ'),
    "bundled Noto Sans Thaana should cover U+0787 (އ)"
  );

  let syriac = db
    .query("Noto Sans Syriac", FontWeight::NORMAL, FontStyle::Normal)
    .expect("bundled Noto Sans Syriac should be present");
  assert!(
    db.has_glyph(syriac, 'ܫ'),
    "bundled Noto Sans Syriac should cover U+072B (ܫ)"
  );
  assert!(
    db.has_glyph(syriac, '܅'),
    "bundled Noto Sans Syriac should cover U+0705 (܅)"
  );

  let nko = db
    .query("Noto Sans NKo", FontWeight::NORMAL, FontStyle::Normal)
    .expect("bundled Noto Sans NKo should be present");
  assert!(
    db.has_glyph(nko, 'ߊ'),
    "bundled Noto Sans NKo should cover U+07CA (ߊ)"
  );
  assert!(
    db.has_glyph(nko, '߫'),
    "bundled Noto Sans NKo should cover U+07EB (߫)"
  );
}

