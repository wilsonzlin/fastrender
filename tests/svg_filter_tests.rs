use fastrender::geometry::Rect;
use fastrender::paint::svg_filter::{
  apply_svg_filter, ColorInterpolationFilters, CompositeOperator, FilterInput, FilterPrimitive,
  FilterStep, SvgFilter, SvgFilterRegion, SvgFilterUnits, SvgLength, TurbulenceType,
};
use fastrender::Rgba;
use tiny_skia::{Pixmap, PremultipliedColorU8};

fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let px = pixmap.pixel(x, y).unwrap();
  (px.red(), px.green(), px.blue(), px.alpha())
}

fn base_filter(steps: Vec<FilterStep>) -> SvgFilter {
  SvgFilter {
    color_interpolation_filters: ColorInterpolationFilters::LinearRGB,
    steps,
    region: SvgFilterRegion {
      x: SvgLength::Percent(-0.1),
      y: SvgLength::Percent(-0.1),
      width: SvgLength::Percent(1.2),
      height: SvgLength::Percent(1.2),
      units: SvgFilterUnits::ObjectBoundingBox,
    },
    filter_res: None,
    primitive_units: SvgFilterUnits::UserSpaceOnUse,
  }
}

#[test]
fn missing_reference_defaults_to_transparent_integration() {
  let mut pixmap = Pixmap::new(2, 1).unwrap();
  pixmap.pixels_mut()[0] = PremultipliedColorU8::from_rgba(0, 0, 255, 255).unwrap();

  let steps = vec![
    FilterStep {
      result: Some("filled".into()),
      color_interpolation_filters: None,
      primitive: FilterPrimitive::Flood {
        color: Rgba::from_rgba8(255, 0, 0, 255),
        opacity: 1.0,
      },
      region: None,
    },
    FilterStep {
      result: None,
      color_interpolation_filters: None,
      primitive: FilterPrimitive::Composite {
        input1: FilterInput::Reference("filled".into()),
        input2: FilterInput::Reference("does-not-exist".into()),
        operator: CompositeOperator::Over,
      },
      region: None,
    },
  ];

  let filter = base_filter(steps);
  let bbox = Rect::from_xywh(0.0, 0.0, 2.0, 1.0);
  apply_svg_filter(&filter, &mut pixmap, 1.0, bbox);

  assert_eq!(pixel(&pixmap, 0, 0), (255, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 1, 0), (255, 0, 0, 255));
}

#[test]
fn primitive_region_userspace_respects_scale() {
  let mut pixmap = Pixmap::new(4, 4).unwrap();
  let steps = vec![FilterStep {
    result: None,
    color_interpolation_filters: None,
    primitive: FilterPrimitive::Flood {
      color: Rgba::from_rgba8(0, 255, 0, 255),
      opacity: 1.0,
    },
    region: Some(SvgFilterRegion {
      x: SvgLength::Number(0.0),
      y: SvgLength::Number(0.0),
      width: SvgLength::Number(1.0),
      height: SvgLength::Number(1.0),
      units: SvgFilterUnits::UserSpaceOnUse,
    }),
  }];
  let mut filter = base_filter(steps);
  filter.primitive_units = SvgFilterUnits::UserSpaceOnUse;

  let bbox = Rect::from_xywh(0.0, 0.0, 4.0, 4.0);
  apply_svg_filter(&filter, &mut pixmap, 2.0, bbox);

  // Region should be 2x2 after scaling (1px * scale=2).
  assert_eq!(pixel(&pixmap, 0, 0), (0, 255, 0, 255));
  assert_eq!(pixel(&pixmap, 1, 1), (0, 255, 0, 255));
  assert_eq!(pixel(&pixmap, 2, 0), (0, 0, 0, 0));
  assert_eq!(pixel(&pixmap, 3, 3), (0, 0, 0, 0));
}

#[test]
fn turbulence_is_deterministic() {
  let mut pixmap = Pixmap::new(2, 2).unwrap();
  let steps = vec![FilterStep {
    result: None,
    color_interpolation_filters: None,
    primitive: FilterPrimitive::Turbulence {
      base_frequency: (0.5, 0.5),
      seed: 2,
      octaves: 1,
      kind: TurbulenceType::Turbulence,
    },
    region: None,
  }];
  let filter = base_filter(steps);
  let bbox = Rect::from_xywh(0.0, 0.0, 2.0, 2.0);
  apply_svg_filter(&filter, &mut pixmap, 1.0, bbox);

  let checksum: u32 = pixmap.data().iter().map(|b| *b as u32).sum();
  assert_eq!(checksum, 2709);
}
