use crate::paint::color_font_helpers::{load_fixture_font, shaped_run};
use fastrender::paint::text_rasterize::TextRasterizer;
use fastrender::style::color::Rgba;
use tiny_skia::Pixmap;

#[test]
fn color_font_caching_keeps_output_stable() {
  let font = load_fixture_font("colrv1-test.ttf");
  let run = shaped_run(&font, 'A', 64.0, 0);

  let mut rasterizer = TextRasterizer::new();
  rasterizer.reset_cache_stats();

  let mut cold = Pixmap::new(128, 128).expect("pixmap allocation");
  cold.fill(tiny_skia::Color::WHITE);
  rasterizer
    .render_shaped_run(&run, 16.0, 96.0, Rgba::BLACK, &mut cold)
    .expect("first render");
  let cold_stats = rasterizer.cache_stats();

  let mut warmed = Pixmap::new(128, 128).expect("pixmap allocation");
  warmed.fill(tiny_skia::Color::WHITE);
  rasterizer
    .render_shaped_run(&run, 16.0, 96.0, Rgba::BLACK, &mut warmed)
    .expect("second render");
  let warmed_stats = rasterizer.cache_stats();

  assert_eq!(
    cold.data(),
    warmed.data(),
    "cached color glyphs must preserve output"
  );
  assert!(
    warmed_stats.hits > cold_stats.hits,
    "expected glyph caching to register hits after warming"
  );
}
