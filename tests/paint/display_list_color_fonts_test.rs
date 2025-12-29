mod color_font_helpers;

use color_font_helpers::{
  load_fixture_font, non_white_colors, render_backends_for_run, shaped_run,
};
use fastrender::style::color::Rgba;

#[test]
fn colrv1_display_list_matches_legacy() {
  let font = load_fixture_font("colrv1-test.ttf");
  let run = shaped_run(&font, 'G', 96.0, 1);
  let (dl, legacy) = render_backends_for_run(&run, Rgba::BLACK, 200, 180, 20.0, 150.0);

  assert_eq!(
    dl.data(),
    legacy.data(),
    "display-list rendering should match legacy rasterizer output for COLRv1"
  );
  assert!(
    non_white_colors(&dl) > 1,
    "color glyph should preserve embedded palette layers"
  );
}

#[test]
fn svg_color_glyph_uses_current_color_in_display_list() {
  let font = load_fixture_font("svg-color-glyph-test.ttf");
  let run = shaped_run(&font, 'A', 64.0, 0);
  let color = Rgba::new(200, 30, 30, 0.8);
  let (dl, legacy) = render_backends_for_run(&run, color, 140, 140, 10.0, 110.0);

  assert_eq!(
    dl.data(),
    legacy.data(),
    "display-list SVG color glyphs should match legacy rendering"
  );
  assert!(
    non_white_colors(&dl) > 1,
    "SVG glyph should retain multiple painted colors"
  );
}
