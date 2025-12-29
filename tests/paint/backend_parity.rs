use super::color_font_helpers::{load_fixture_font, render_backends_for_run, shaped_run};
use fastrender::style::color::Rgba;

#[test]
fn bitmap_color_font_matches_between_backends() {
  let font = load_fixture_font("TestSbixJPEG.ttf");
  let run = shaped_run(&font, 'A', 48.0, 0);
  let (dl, legacy) = render_backends_for_run(&run, Rgba::BLACK, 120, 120, 10.0, 90.0);
  let dl_data: Vec<u8> = dl.data().to_vec();
  let legacy_data: Vec<u8> = legacy.data().to_vec();

  assert_eq!(
    dl_data, legacy_data,
    "display-list backend should render bitmap color glyphs the same as legacy"
  );
}
