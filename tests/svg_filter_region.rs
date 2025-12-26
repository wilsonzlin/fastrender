use fastrender::geometry::Rect;
use fastrender::paint::svg_filter::{
  apply_svg_filter, ColorInterpolationFilters, FilterPrimitive, FilterStep, SvgFilter,
  SvgFilterRegion, SvgFilterUnits, SvgLength,
};
use fastrender::Rgba;
use tiny_skia::Pixmap;

fn assert_rect_close(actual: Rect, expected: Rect) {
  let eps = 0.001;
  assert!(
    (actual.x() - expected.x()).abs() <= eps,
    "x: expected {} got {}",
    expected.x(),
    actual.x()
  );
  assert!(
    (actual.y() - expected.y()).abs() <= eps,
    "y: expected {} got {}",
    expected.y(),
    actual.y()
  );
  assert!(
    (actual.width() - expected.width()).abs() <= eps,
    "width: expected {} got {}",
    expected.width(),
    actual.width()
  );
  assert!(
    (actual.height() - expected.height()).abs() <= eps,
    "height: expected {} got {}",
    expected.height(),
    actual.height()
  );
}

#[test]
fn default_filter_region_expands_bbox() {
  let bbox = Rect::from_xywh(10.0, 20.0, 100.0, 200.0);
  let region = SvgFilterRegion {
    x: SvgLength::Percent(-0.1),
    y: SvgLength::Percent(-0.1),
    width: SvgLength::Percent(1.2),
    height: SvgLength::Percent(1.2),
    units: SvgFilterUnits::ObjectBoundingBox,
  }
  .resolve(bbox);

  assert_rect_close(region, Rect::from_xywh(0.0, 0.0, 120.0, 240.0));
}

#[test]
fn filter_units_control_resolution() {
  let bbox = Rect::from_xywh(10.0, 20.0, 200.0, 100.0);

  let object_region = SvgFilterRegion {
    x: SvgLength::Number(0.5),
    y: SvgLength::Percent(0.1),
    width: SvgLength::Number(0.5),
    height: SvgLength::Percent(0.3),
    units: SvgFilterUnits::ObjectBoundingBox,
  }
  .resolve(bbox);
  assert_rect_close(object_region, Rect::from_xywh(110.0, 30.0, 100.0, 30.0));

  let user_region = SvgFilterRegion {
    x: SvgLength::Number(0.5),
    y: SvgLength::Percent(0.1),
    width: SvgLength::Number(0.5),
    height: SvgLength::Percent(0.3),
    units: SvgFilterUnits::UserSpaceOnUse,
  }
  .resolve(bbox);
  assert_rect_close(user_region, Rect::from_xywh(0.5, 30.0, 0.5, 30.0));
}

#[test]
fn filter_region_respects_bbox_offset() {
  let mut filter = SvgFilter {
    color_interpolation_filters: ColorInterpolationFilters::SRGB,
    steps: Vec::new(),
    region: SvgFilterRegion {
      x: SvgLength::Percent(0.0),
      y: SvgLength::Percent(0.0),
      width: SvgLength::Percent(1.0),
      height: SvgLength::Percent(1.0),
      units: SvgFilterUnits::ObjectBoundingBox,
    },
    primitive_units: SvgFilterUnits::ObjectBoundingBox,
  };
  filter.steps.push(FilterStep {
    result: None,
    color_interpolation_filters: None,
    primitive: FilterPrimitive::Flood {
      color: Rgba::new(0, 255, 0, 1.0),
      opacity: 1.0,
    },
    region: None,
  });

  let mut pixmap = Pixmap::new(20, 20).unwrap();
  let bbox = Rect::from_xywh(5.0, 6.0, 10.0, 8.0);
  apply_svg_filter(&filter, &mut pixmap, 1.0, bbox);

  let pixels = pixmap.pixels();
  let width = pixmap.width() as usize;
  for y in 0..pixmap.height() as usize {
    for x in 0..pixmap.width() as usize {
      let px = pixels[y * width + x];
      let inside = x >= 5 && x < 15 && y >= 6 && y < 14;
      if inside {
        assert_eq!(px.alpha(), 255, "pixel at ({x},{y}) should be opaque");
      } else {
        assert_eq!(px.alpha(), 0, "pixel at ({x},{y}) should be transparent");
      }
    }
  }
}
