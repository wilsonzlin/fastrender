use fastrender::geometry::Rect;
use fastrender::paint::svg_filter::{
  apply_svg_filter, ColorInterpolationFilters, FilterPrimitive, FilterStep, SvgFilter,
  SvgFilterRegion, SvgFilterUnits, SvgLength, TurbulenceType,
};
use tiny_skia::Pixmap;

fn turbulence_filter(primitive: FilterPrimitive) -> SvgFilter {
  let mut filter = SvgFilter {
    color_interpolation_filters: ColorInterpolationFilters::LinearRGB,
    steps: vec![FilterStep {
      result: None,
      color_interpolation_filters: None,
      primitive,
      region: None,
    }],
    region: SvgFilterRegion {
      x: SvgLength::Percent(0.0),
      y: SvgLength::Percent(0.0),
      width: SvgLength::Percent(1.0),
      height: SvgLength::Percent(1.0),
      units: SvgFilterUnits::ObjectBoundingBox,
    },
    filter_res: None,
    primitive_units: SvgFilterUnits::ObjectBoundingBox,
    fingerprint: 0,
  };
  filter
}

fn apply_filter(filter: &SvgFilter, pixmap: &mut Pixmap) {
  let bbox = Rect::from_xywh(0.0, 0.0, pixmap.width() as f32, pixmap.height() as f32);
  apply_svg_filter(filter, pixmap, 1.0, bbox).unwrap();
}

#[test]
fn turbulence_is_deterministic() {
  let filter = turbulence_filter(FilterPrimitive::Turbulence {
    base_frequency: (0.15, 0.2),
    seed: 42,
    octaves: 3,
    stitch_tiles: false,
    kind: TurbulenceType::FractalNoise,
  });

  let mut first = Pixmap::new(32, 24).unwrap();
  let mut second = Pixmap::new(32, 24).unwrap();
  apply_filter(&filter, &mut first);
  apply_filter(&filter, &mut second);

  assert_eq!(first.data(), second.data());
}

#[test]
fn turbulence_seed_changes_output() {
  let filter_a = turbulence_filter(FilterPrimitive::Turbulence {
    base_frequency: (0.1, 0.12),
    seed: 1,
    octaves: 2,
    stitch_tiles: false,
    kind: TurbulenceType::Turbulence,
  });
  let filter_b = turbulence_filter(FilterPrimitive::Turbulence {
    base_frequency: (0.1, 0.12),
    seed: 99,
    octaves: 2,
    stitch_tiles: false,
    kind: TurbulenceType::Turbulence,
  });

  let mut first = Pixmap::new(24, 24).unwrap();
  let mut second = Pixmap::new(24, 24).unwrap();
  apply_filter(&filter_a, &mut first);
  apply_filter(&filter_b, &mut second);

  assert_ne!(first.data(), second.data());
}

#[test]
fn turbulence_stitches_edges() {
  let filter = turbulence_filter(FilterPrimitive::Turbulence {
    base_frequency: (0.08, 0.1),
    seed: 7,
    octaves: 2,
    stitch_tiles: true,
    kind: TurbulenceType::Turbulence,
  });

  let mut pixmap = Pixmap::new(64, 32).unwrap();
  apply_filter(&filter, &mut pixmap);

  let pixels = pixmap.pixels();
  let width = pixmap.width() as usize;
  for (row_idx, row) in pixels.chunks(width).enumerate() {
    let first = row.first().unwrap();
    let last = row.last().unwrap();
    assert_eq!(first.red(), last.red(), "red mismatch on row {row_idx}");
    assert_eq!(
      first.green(),
      last.green(),
      "green mismatch on row {row_idx}"
    );
    assert_eq!(first.blue(), last.blue(), "blue mismatch on row {row_idx}");
  }
}
