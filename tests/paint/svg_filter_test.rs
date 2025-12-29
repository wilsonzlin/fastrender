use fastrender::geometry::Rect;
use fastrender::paint::svg_filter::{
  apply_svg_filter, ChannelSelector, ColorInterpolationFilters, FilterInput, FilterPrimitive,
  FilterStep, ImagePrimitive, SvgFilter, SvgFilterRegion, SvgFilterUnits, SvgLength,
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
  let mut filter = SvgFilter {
    color_interpolation_filters: ColorInterpolationFilters::LinearRGB,
    steps: vec![
      FilterStep {
        result: Some("map".to_string()),
        color_interpolation_filters: None,
        primitive: FilterPrimitive::Image(ImagePrimitive::from_pixmap(map)),
        region: None,
      },
      FilterStep {
        result: None,
        color_interpolation_filters: None,
        primitive: FilterPrimitive::DisplacementMap {
          in1: FilterInput::SourceGraphic,
          in2: FilterInput::Reference("map".to_string()),
          scale,
          x_channel: ChannelSelector::R,
          y_channel: ChannelSelector::B,
        },
        region: None,
      },
    ],
    region: SvgFilterRegion {
      x: SvgLength::Percent(-0.1),
      y: SvgLength::Percent(-0.1),
      width: SvgLength::Percent(1.2),
      height: SvgLength::Percent(1.2),
      units: SvgFilterUnits::ObjectBoundingBox,
    },
    filter_res: None,
    primitive_units: SvgFilterUnits::ObjectBoundingBox,
    fingerprint: 0,
  };
  filter
}

#[test]
fn displacement_map_shifts_pixels_right() {
  let mut pixmap = gradient_pixmap();
  let filter = displacement_filter(2.0);
  let bbox = Rect::from_xywh(0.0, 0.0, pixmap.width() as f32, pixmap.height() as f32);

  apply_svg_filter(&filter, &mut pixmap, 1.0, bbox);

  assert_eq!(pixel(&pixmap, 0), (0, 255, 0, 255));
  assert_eq!(pixel(&pixmap, 1), (0, 0, 255, 255));
  assert_eq!(pixel(&pixmap, 2), (0, 0, 0, 0));
}

#[test]
fn displacement_map_scale_zero_is_identity() {
  let mut pixmap = gradient_pixmap();
  let expected: Vec<_> = (0..pixmap.width()).map(|x| pixel(&pixmap, x)).collect();

  let filter = displacement_filter(0.0);
  let bbox = Rect::from_xywh(0.0, 0.0, pixmap.width() as f32, pixmap.height() as f32);
  apply_svg_filter(&filter, &mut pixmap, 1.0, bbox);

  for x in 0..pixmap.width() {
    assert_eq!(pixel(&pixmap, x), expected[x as usize]);
  }
}

#[test]
fn displacement_map_interprets_map_in_color_interpolation_space() {
  let mut primary = gradient_pixmap();
  let mut map = Pixmap::new(primary.width(), primary.height()).unwrap();
  let half = PremultipliedColorU8::from_rgba(128, 128, 128, 128).unwrap();
  for px in map.pixels_mut() {
    *px = half;
  }

  let mut filter = SvgFilter {
    color_interpolation_filters: ColorInterpolationFilters::LinearRGB,
    steps: vec![
      FilterStep {
        result: Some("map".to_string()),
        color_interpolation_filters: None,
        primitive: FilterPrimitive::Image(ImagePrimitive::from_pixmap(map)),
        region: None,
      },
      FilterStep {
        result: None,
        color_interpolation_filters: None,
        primitive: FilterPrimitive::DisplacementMap {
          in1: FilterInput::SourceGraphic,
          in2: FilterInput::Reference("map".to_string()),
          scale: 4.0,
          x_channel: ChannelSelector::R,
          y_channel: ChannelSelector::A,
        },
        region: None,
      },
    ],
    region: SvgFilterRegion {
      x: SvgLength::Percent(-0.1),
      y: SvgLength::Percent(-0.1),
      width: SvgLength::Percent(1.2),
      height: SvgLength::Percent(1.2),
      units: SvgFilterUnits::ObjectBoundingBox,
    },
    filter_res: None,
    primitive_units: SvgFilterUnits::ObjectBoundingBox,
    fingerprint: 0,
  };

  let bbox = Rect::from_xywh(0.0, 0.0, primary.width() as f32, primary.height() as f32);
  apply_svg_filter(&filter, &mut primary, 1.0, bbox);

  let center = pixel(&primary, 1);
  assert!(
    center.0 > center.1,
    "linearRGB map values should pull color toward red, got {:?}",
    center
  );
  assert!(center.3 > 0, "displacement map should preserve coverage");
}
