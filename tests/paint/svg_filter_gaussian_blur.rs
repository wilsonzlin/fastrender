use fastrender::geometry::Rect;
use fastrender::image_loader::ImageCache;
use fastrender::paint::svg_filter::{
  apply_svg_filter, parse_svg_filter_from_svg_document, ColorInterpolationFilters, FilterInput,
  FilterPrimitive, FilterStep, SvgFilter, SvgFilterRegion, SvgFilterUnits, SvgLength,
};
use tiny_skia::{Pixmap, PremultipliedColorU8};

fn parse_blur(std_deviation: &str) -> (f32, f32) {
  let svg = format!(
    r#"
    <svg xmlns="http://www.w3.org/2000/svg">
      <filter id="f">
        <feGaussianBlur stdDeviation="{}" />
      </filter>
    </svg>
  "#,
    std_deviation
  );
  let filter =
    parse_svg_filter_from_svg_document(&svg, Some("f"), &ImageCache::new()).expect("parse filter");
  match &filter.steps[0].primitive {
    FilterPrimitive::GaussianBlur { std_dev, .. } => *std_dev,
    other => panic!("expected gaussian blur primitive, got {:?}", other),
  }
}

#[test]
fn gaussian_blur_single_value_applies_to_both_axes() {
  let (x, y) = parse_blur("4");
  assert!((x - 4.0).abs() < f32::EPSILON);
  assert!((y - 4.0).abs() < f32::EPSILON);
}

#[test]
fn gaussian_blur_two_values_are_parsed() {
  let (x, y) = parse_blur("3 1");
  assert!((x - 3.0).abs() < f32::EPSILON);
  assert!((y - 1.0).abs() < f32::EPSILON);
}

#[test]
fn gaussian_blur_negative_values_are_clamped() {
  let (x, y) = parse_blur("-3 -5");
  assert_eq!(x, 0.0);
  assert_eq!(y, 0.0);
}

fn opaque_bounds(pixmap: &Pixmap) -> Option<(u32, u32, u32, u32)> {
  let mut min_x = pixmap.width();
  let mut min_y = pixmap.height();
  let mut max_x = 0;
  let mut max_y = 0;
  let mut seen = false;
  for y in 0..pixmap.height() {
    for x in 0..pixmap.width() {
      let px = pixmap.pixel(x, y).unwrap();
      if px.alpha() > 0 {
        seen = true;
        min_x = min_x.min(x);
        min_y = min_y.min(y);
        max_x = max_x.max(x);
        max_y = max_y.max(y);
      }
    }
  }
  if seen {
    Some((min_x, min_y, max_x - min_x + 1, max_y - min_y + 1))
  } else {
    None
  }
}

fn draw_rect(
  pixmap: &mut Pixmap,
  x: u32,
  y: u32,
  width: u32,
  height: u32,
  color: PremultipliedColorU8,
) {
  let stride = pixmap.width() as usize;
  for iy in y..y + height {
    for ix in x..x + width {
      let idx = (iy * stride as u32 + ix) as usize;
      pixmap.pixels_mut()[idx] = color;
    }
  }
}

#[test]
fn gaussian_blur_resolves_single_value_per_axis_in_object_bbox_units() {
  let bbox = Rect::from_xywh(0.0, 0.0, 100.0, 50.0);
  let mut pixmap = Pixmap::new(bbox.width() as u32, bbox.height() as u32).expect("pixmap");
  let opaque = PremultipliedColorU8::from_rgba(255, 255, 255, 255).expect("color");
  draw_rect(&mut pixmap, 45, 20, 10, 10, opaque);

  let mut filter = SvgFilter {
    color_interpolation_filters: ColorInterpolationFilters::SRGB,
    steps: vec![FilterStep {
      result: None,
      color_interpolation_filters: None,
      primitive: FilterPrimitive::GaussianBlur {
        input: FilterInput::SourceGraphic,
        std_dev: (0.1, 0.1),
      },
      region: None,
    }],
    region: SvgFilterRegion {
      x: SvgLength::Number(0.0),
      y: SvgLength::Number(0.0),
      width: SvgLength::Number(bbox.width()),
      height: SvgLength::Number(bbox.height()),
      units: SvgFilterUnits::UserSpaceOnUse,
    },
    filter_res: None,
    primitive_units: SvgFilterUnits::ObjectBoundingBox,
    fingerprint: 0,
  };
  apply_svg_filter(&filter, &mut pixmap, 1.0, bbox).unwrap();

  let bounds = opaque_bounds(&pixmap).expect("blurred content should be visible");
  let (min_x, min_y, width, height) = bounds;

  assert!(
    (min_x as i32 - 15).abs() <= 1,
    "expected blur to expand more along x (min_x={min_x})"
  );
  assert!(
    (min_y as i32 - 5).abs() <= 1,
    "expected blur to expand based on bbox height (min_y={min_y})"
  );
  assert!(
    (width as i32 - 70).abs() <= 2,
    "expected horizontal spread to reflect bbox width (width={width})"
  );
  assert!(
    (height as i32 - 40).abs() <= 2,
    "expected vertical spread to reflect bbox height (height={height})"
  );
}
