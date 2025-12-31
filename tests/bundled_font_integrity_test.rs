use fastrender::{FontConfig, FontContext, FontStyleDb as FontStyle, FontWeightDb as FontWeight};
use std::collections::BTreeSet;

fn loaded_family_names(ctx: &FontContext) -> Vec<String> {
  let mut families = BTreeSet::new();
  for face in ctx.database().faces() {
    for (name, _) in &face.families {
      families.insert(name.clone());
    }
  }
  families.into_iter().collect()
}

fn assert_family_has_glyph(ctx: &FontContext, family: &str, sample: char) {
  let db = ctx.database();
  let families = loaded_family_names(ctx);
  let id = db
    .query(family, FontWeight::NORMAL, FontStyle::Normal)
    .unwrap_or_else(|| {
      panic!("expected bundled family {family:?} to be queryable; loaded families: {families:?}")
    });

  let loaded = db
    .load_font(id)
    .unwrap_or_else(|| panic!("bundled family {family:?} resolved but failed to load"));

  let face = loaded.as_ttf_face().unwrap_or_else(|err| {
    panic!("bundled family {family:?} resolved but could not be parsed: {err}")
  });

  assert!(
    face.glyph_index(sample).is_some(),
    "bundled family {family:?} loaded as {:?} but does not cover U+{:04X} ({:?})",
    loaded.family,
    sample as u32,
    sample,
  );
}

#[test]
fn bundled_font_set_integrity() {
  let ctx = FontContext::with_config(FontConfig::bundled_only());

  let count = ctx.font_count();
  let families = loaded_family_names(&ctx);
  assert!(
    count >= 10,
    "bundled_only() loaded too few fonts (font_count={count}). This value is what pageset progress files log as `fonts=N`; a drop usually means bundled font fixtures failed to load. Loaded families: {families:?}",
  );

  assert_family_has_glyph(&ctx, "Noto Sans", 'A');
  assert_family_has_glyph(&ctx, "Noto Serif", 'A');
  assert_family_has_glyph(&ctx, "Noto Sans Mono", '■');
  assert_family_has_glyph(&ctx, "Noto Sans Arabic", 'م');
  assert_family_has_glyph(&ctx, "Noto Sans Devanagari", 'न');
  assert_family_has_glyph(&ctx, "Noto Sans Bengali", 'ব');
  assert_family_has_glyph(&ctx, "Noto Sans SC", '中');
  assert_family_has_glyph(&ctx, "Noto Sans JP", 'あ');
  assert_family_has_glyph(&ctx, "Noto Sans KR", '한');
  assert_family_has_glyph(&ctx, "Noto Sans Symbols", '→');
  assert_family_has_glyph(&ctx, "Noto Sans Symbols 2", '✓');
  assert_family_has_glyph(&ctx, "STIX Two Math", '∑');
  assert_family_has_glyph(&ctx, "DejaVu Sans", 'W');
}
