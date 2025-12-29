use crate::color_font_helpers::{load_fixture_font, render_backends_for_run, shaped_run};
use fastrender::style::color::Rgba;

#[test]
fn bitmap_color_font_matches_between_backends() {
  let font = load_fixture_font("TestSbixJPEG.ttf");
  let run = shaped_run(&font, 'A', 48.0, 0);
  let (dl, legacy) = render_backends_for_run(&run, Rgba::BLACK, 120, 120, 10.0, 90.0);

  assert_eq!(
    dl.data(),
    legacy.data(),
    "display-list backend should render bitmap color glyphs the same as legacy"
  );
}
