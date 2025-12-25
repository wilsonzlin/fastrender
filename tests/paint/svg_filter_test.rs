use fastrender::geometry::Rect;
use fastrender::paint::svg_filter::{
  apply_svg_filter, ChannelSelector, FilterInput, FilterPrimitive, FilterStep, SvgFilter,
  SvgFilterRegion, SvgFilterUnits, SvgLength,
};
use tiny_skia::{Pixmap, PremultipliedColorU8};

fn gradient_pixmap() -> Pixmap {
  let mut pixmap = Pixmap::new(3, 1).unwrap();
  let colors = [(255, 0, 0, 255), (0, 255, 0, 255), (0, 0, 255, 255)];
  for (idx, px) in pixmap.pixels_mut().iter_mut().enumerate() {
    let (r, g, b, a) = colors[idx];
    *px = PremultipliedColorU8::from_rgba(r, g, b, a).unwrap_or(PremultipliedColorU8::TRANSPARENT);
  }
  pixmap
}

fn displacement_map_pixmap() -> Pixmap {
  let mut pixmap = Pixmap::new(3, 1).unwrap();
  for px in pixmap.pixels_mut() {
    *px = PremultipliedColorU8::from_rgba(255, 0, 255, 255)
      .unwrap_or(PremultipliedColorU8::TRANSPARENT);
  }
  pixmap
}

fn pixel(pixmap: &Pixmap, x: u32) -> (u8, u8, u8, u8) {
  let px = pixmap.pixel(x, 0).unwrap();
  (px.red(), px.green(), px.blue(), px.alpha())
}

fn displacement_filter(scale: f32) -> SvgFilter {
  let map = displacement_map_pixmap();
  SvgFilter {
    steps: vec![
      FilterStep {
        result: Some("map".to_string()),
        primitive: FilterPrimitive::Image(map),
      },
      FilterStep {
        result: None,
        primitive: FilterPrimitive::DisplacementMap {
          in1: FilterInput::SourceGraphic,
          in2: FilterInput::Reference("map".to_string()),
          scale,
          x_channel: ChannelSelector::R,
          y_channel: ChannelSelector::B,
        },
      },
    ],
    region: SvgFilterRegion {
      x: SvgLength::Percent(-0.1),
      y: SvgLength::Percent(-0.1),
      width: SvgLength::Percent(1.2),
      height: SvgLength::Percent(1.2),
      units: SvgFilterUnits::ObjectBoundingBox,
    },
  }
}

#[test]
fn displacement_map_shifts_pixels_right() {
  let mut pixmap = gradient_pixmap();
  let filter = displacement_filter(2.0);

  apply_svg_filter(
    &filter,
    &mut pixmap,
    Rect::from_xywh(0.0, 0.0, pixmap.width() as f32, pixmap.height() as f32),
  );

  assert_eq!(pixel(&pixmap, 0), (0, 255, 0, 255));
  assert_eq!(pixel(&pixmap, 1), (0, 0, 255, 255));
  assert_eq!(pixel(&pixmap, 2), (0, 0, 0, 0));
}

#[test]
fn displacement_map_scale_zero_is_identity() {
  let mut pixmap = gradient_pixmap();
  let expected: Vec<_> = (0..pixmap.width()).map(|x| pixel(&pixmap, x)).collect();

  let filter = displacement_filter(0.0);
  apply_svg_filter(
    &filter,
    &mut pixmap,
    Rect::from_xywh(0.0, 0.0, pixmap.width() as f32, pixmap.height() as f32),
  );

  for x in 0..pixmap.width() {
    assert_eq!(pixel(&pixmap, x), expected[x as usize]);
  }
}
