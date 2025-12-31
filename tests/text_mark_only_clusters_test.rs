use fastrender::{ComputedStyle, FontConfig, FontContext, ShapingPipeline};

fn assert_mark_only_cluster_visible(text: &str) {
  let font_context = FontContext::with_config(FontConfig::bundled_only());
  let style = ComputedStyle::default();
  let pipeline = ShapingPipeline::new();

  let runs = pipeline.shape(text, &style, &font_context).expect("shape");
  assert!(
    !runs.is_empty(),
    "expected at least one shaped run for {text:?}"
  );

  let glyph_ids: Vec<u32> = runs
    .iter()
    .flat_map(|run| run.glyphs.iter().map(|glyph| glyph.glyph_id))
    .collect();

  let has_non_notdef_glyph = runs
    .iter()
    .any(|run| run.glyphs.iter().any(|glyph| glyph.glyph_id != 0));
  assert!(
    has_non_notdef_glyph,
    "expected at least one non-.notdef glyph for mark-only cluster {text:?}; got glyph IDs {glyph_ids:?}"
  );

  let total_advance: f32 = runs.iter().map(|run| run.advance).sum();
  assert!(
    total_advance > 0.0,
    "expected positive advance for mark-only cluster {text:?}; got {total_advance} (glyph IDs {glyph_ids:?})"
  );
}

#[test]
fn bundled_fonts_render_mark_only_clusters() {
  // Page-set regressions: clusters that are only combining marks must not be erased by
  // "optional mark" fallback logic.
  assert_mark_only_cluster_visible("\u{065B}\u{031C}"); // Vogue: Arabic mark + combining mark
  assert_mark_only_cluster_visible("\u{0613}\u{06DA}"); // Wired: Arabic marks
}
