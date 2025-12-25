use fastrender::geometry::Rect;
use fastrender::paint::svg_filter::{
  apply_svg_filter, ColorInterpolationFilters, ColorMatrixKind, FilterInput, FilterPrimitive,
  FilterStep, SvgFilter, SvgFilterRegion, SvgFilterUnits, SvgLength,
};
use tiny_skia::{ColorU8, Pixmap, PremultipliedColorU8};

fn double_matrix() -> [f32; 20] {
  [
    2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
    0.0,
  ]
}

fn identity_matrix() -> [f32; 20] {
  [
    1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
    0.0,
  ]
}

fn make_filter(kind: ColorMatrixKind, color_space: ColorInterpolationFilters) -> SvgFilter {
  SvgFilter {
    color_interpolation_filters: color_space,
    steps: vec![FilterStep {
      result: None,
      color_interpolation_filters: None,
      primitive: FilterPrimitive::ColorMatrix {
        input: FilterInput::SourceGraphic,
        kind,
      },
      region: None,
    }],
    region: SvgFilterRegion {
      x: SvgLength::Percent(-0.1),
      y: SvgLength::Percent(-0.1),
      width: SvgLength::Percent(1.2),
      height: SvgLength::Percent(1.2),
      units: SvgFilterUnits::ObjectBoundingBox,
    },
    primitive_units: SvgFilterUnits::ObjectBoundingBox,
  }
}

#[test]
fn color_matrix_respects_color_interpolation_filters() {
  let mut base = Pixmap::new(1, 1).unwrap();
  base.pixels_mut()[0] = PremultipliedColorU8::from_rgba(128, 128, 128, 255).unwrap();

  let filter_linear = make_filter(
    ColorMatrixKind::Matrix(double_matrix()),
    ColorInterpolationFilters::LinearRGB,
  );
  let mut linear_result = base.clone();
  let bbox = Rect::from_xywh(
    0.0,
    0.0,
    linear_result.width() as f32,
    linear_result.height() as f32,
  );
  apply_svg_filter(&filter_linear, &mut linear_result, 1.0, bbox);

  let mut srgb_filter = filter_linear.clone();
  srgb_filter.color_interpolation_filters = ColorInterpolationFilters::SRGB;
  let mut srgb_result = base.clone();
  apply_svg_filter(&srgb_filter, &mut srgb_result, 1.0, bbox);

  let linear_px = linear_result.pixels()[0];
  let srgb_px = srgb_result.pixels()[0];

  assert!(srgb_px.red() > linear_px.red());
  assert_eq!(srgb_px.red(), 255);
  assert_eq!(srgb_px.red(), srgb_px.green());
  assert_eq!(srgb_px.red(), srgb_px.blue());
}

#[test]
fn identity_matrix_is_a_noop_in_all_color_spaces() {
  let mut base = Pixmap::new(1, 1).unwrap();
  base.pixels_mut()[0] = ColorU8::from_rgba(50, 100, 150, 128).premultiply();

  let identity = ColorMatrixKind::Matrix(identity_matrix());

  let mut linear_result = base.clone();
  let linear_bbox = Rect::from_xywh(
    0.0,
    0.0,
    linear_result.width() as f32,
    linear_result.height() as f32,
  );
  apply_svg_filter(
    &make_filter(identity.clone(), ColorInterpolationFilters::LinearRGB),
    &mut linear_result,
    1.0,
    linear_bbox,
  );

  let mut srgb_result = base.clone();
  let srgb_bbox = Rect::from_xywh(
    0.0,
    0.0,
    srgb_result.width() as f32,
    srgb_result.height() as f32,
  );
  apply_svg_filter(
    &make_filter(identity, ColorInterpolationFilters::SRGB),
    &mut srgb_result,
    1.0,
    srgb_bbox,
  );

  assert_eq!(base.data(), linear_result.data());
  assert_eq!(base.data(), srgb_result.data());
}
