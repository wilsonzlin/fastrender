use fastrender::geometry::Rect;
use fastrender::paint::svg_filter::{
  apply_svg_filter, ChannelSelector, ColorInterpolationFilters, FilterInput, FilterPrimitive,
  FilterStep, SvgFilter, SvgFilterRegion, SvgFilterUnits, SvgLength, TurbulenceType,
};
use tiny_skia::{Pixmap, PremultipliedColorU8};

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
  filter.refresh_fingerprint();
  filter
}

fn turbulence_displacement_filter(scale: f32) -> SvgFilter {
  let mut filter = SvgFilter {
    color_interpolation_filters: ColorInterpolationFilters::LinearRGB,
    steps: vec![
      FilterStep {
        result: Some("noise".to_string()),
        color_interpolation_filters: None,
        primitive: FilterPrimitive::Turbulence {
          base_frequency: (0.0, 0.0),
          seed: 0,
          octaves: 1,
          stitch_tiles: false,
          kind: TurbulenceType::FractalNoise,
        },
        region: None,
      },
      FilterStep {
        result: None,
        color_interpolation_filters: None,
        primitive: FilterPrimitive::DisplacementMap {
          in1: FilterInput::SourceGraphic,
          in2: FilterInput::Reference("noise".to_string()),
          scale,
          x_channel: ChannelSelector::R,
          y_channel: ChannelSelector::G,
        },
        region: None,
      },
    ],
    region: SvgFilterRegion {
      x: SvgLength::Percent(0.0),
      y: SvgLength::Percent(0.0),
      width: SvgLength::Percent(1.0),
      height: SvgLength::Percent(1.0),
      units: SvgFilterUnits::ObjectBoundingBox,
    },
    filter_res: None,
    primitive_units: SvgFilterUnits::UserSpaceOnUse,
    fingerprint: 0,
  };
  filter.refresh_fingerprint();
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

#[test]
fn turbulence_midgray_displacement_map_is_nearly_identity_in_linear_rgb() {
  // Regression test for CIF=linearRGB pipelines where feTurbulence feeds feDisplacementMap.
  //
  // With baseFrequency=0, feTurbulence produces a constant mapped=0.5 everywhere. Under the
  // filter engine’s linearRGB model, generator primitives must encode that linear 0.5 as sRGB
  // bytes so later srgb_to_linear recovers (approximately) 0.5 instead of ~0.214 (mid-gray in
  // sRGB), which would cause a large displacement.
  //
  // The linear<->sRGB conversion is quantized through an 8-bit LUT, so we allow a small
  // per-channel tolerance instead of asserting exact identity.
  let filter = turbulence_displacement_filter(1.0);

  let mut pixmap = Pixmap::new(8, 8).unwrap();
  {
    let width = pixmap.width() as usize;
    let height = pixmap.height() as usize;
    let pixels = pixmap.pixels_mut();
    for y in 0..height {
      for x in 0..width {
        let is_white = (x + y) % 2 == 0;
        pixels[y * width + x] = if is_white {
          PremultipliedColorU8::from_rgba(255, 255, 255, 255).unwrap()
        } else {
          PremultipliedColorU8::from_rgba(0, 0, 0, 255).unwrap()
        };
      }
    }
  }
  let original = pixmap.clone();
  apply_filter(&filter, &mut pixmap);

  let mut max_delta = 0u8;
  let mut max_at = (0usize, 0usize, 'r', 0u8, 0u8);

  for (idx, (out_px, src_px)) in pixmap
    .pixels()
    .iter()
    .zip(original.pixels().iter())
    .enumerate()
  {
    let x = idx % pixmap.width() as usize;
    let y = idx / pixmap.width() as usize;
    for (name, out, src) in [
      ('r', out_px.red(), src_px.red()),
      ('g', out_px.green(), src_px.green()),
      ('b', out_px.blue(), src_px.blue()),
      ('a', out_px.alpha(), src_px.alpha()),
    ] {
      let delta = out.abs_diff(src);
      if delta > max_delta {
        max_delta = delta;
        max_at = (x, y, name, out, src);
      }
    }
  }

  assert!(
    max_delta <= 20,
    "expected displacement output to stay close to the source (max channel Δ <= 20), got Δ={max_delta} at ({},{}) channel {} (out={} src={})",
    max_at.0,
    max_at.1,
    max_at.2,
    max_at.3,
    max_at.4
  );
}
