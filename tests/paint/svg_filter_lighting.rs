use fastrender::geometry::Rect;
use fastrender::image_loader::ImageCache;
use fastrender::paint::svg_filter::{
  apply_svg_filter, parse_svg_filter_from_svg_document, ColorInterpolationFilters, FilterInput,
  FilterPrimitive, FilterStep, LightSource, SvgFilter, SvgFilterRegion, SvgFilterUnits, SvgLength,
};
use fastrender::Rgba;
use tiny_skia::{Pixmap, PremultipliedColorU8};

fn solid_pixmap(width: u32, height: u32, color: PremultipliedColorU8) -> Pixmap {
  let mut pixmap = Pixmap::new(width, height).expect("pixmap");
  for px in pixmap.pixels_mut() {
    *px = color;
  }
  pixmap
}

#[test]
fn lighting_primitives_parse_light_sources() {
  let svg = r##"
  <svg xmlns="http://www.w3.org/2000/svg">
    <filter id="f" primitiveUnits="objectBoundingBox" color-interpolation-filters="sRGB">
      <feDiffuseLighting in="SourceAlpha" surfaceScale="3" diffuseConstant="2" kernelUnitLength="2 4" lighting-color="rgb(10, 20, 30)" result="diffuse">
        <feSpotLight x="10%" y="20%" z="30" pointsAtX="0" pointsAtY="0" pointsAtZ="10" specularExponent="8" limitingConeAngle="45" />
      </feDiffuseLighting>
      <feSpecularLighting in="SourceGraphic" surfaceScale="1.5" specularConstant="0.5" specularExponent="32" kernelUnitLength="5" lighting-color="#abcdef">
        <feDistantLight azimuth="15" elevation="60" />
      </feSpecularLighting>
    </filter>
  </svg>
  "##;

  let filter =
    parse_svg_filter_from_svg_document(svg, Some("f"), &ImageCache::new()).expect("filter");

  assert_eq!(filter.primitive_units, SvgFilterUnits::ObjectBoundingBox);
  assert_eq!(
    filter.color_interpolation_filters,
    ColorInterpolationFilters::SRGB
  );

  match &filter.steps[0].primitive {
    FilterPrimitive::DiffuseLighting {
      surface_scale,
      diffuse_constant,
      kernel_unit_length,
      light,
      lighting_color,
      ..
    } => {
      assert_eq!(*surface_scale, 3.0);
      assert_eq!(*diffuse_constant, 2.0);
      assert_eq!(kernel_unit_length, &Some((2.0, 4.0)));
      assert_eq!(lighting_color.r, 10);
      assert_eq!(lighting_color.g, 20);
      assert_eq!(lighting_color.b, 30);
      assert!(matches!(
        light,
        LightSource::Spot {
          x: SvgLength::Percent(x),
          y: SvgLength::Percent(y),
          z: SvgLength::Number(z),
          points_at: (SvgLength::Number(_), SvgLength::Number(_), SvgLength::Number(_)),
          specular_exponent,
          limiting_cone_angle: Some(angle),
        } if (*x - 0.10).abs() < f32::EPSILON
          && (*y - 0.20).abs() < f32::EPSILON
          && (*z - 30.0).abs() < f32::EPSILON
          && (*specular_exponent - 8.0).abs() < f32::EPSILON
          && (*angle - 45.0).abs() < f32::EPSILON
      ));
    }
    other => panic!("unexpected first primitive {other:?}"),
  }

  match &filter.steps[1].primitive {
    FilterPrimitive::SpecularLighting {
      surface_scale,
      specular_constant,
      specular_exponent,
      kernel_unit_length,
      light,
      lighting_color,
      ..
    } => {
      assert_eq!(*surface_scale, 1.5);
      assert_eq!(*specular_constant, 0.5);
      assert_eq!(*specular_exponent, 32.0);
      assert_eq!(kernel_unit_length, &Some((5.0, 5.0)));
      assert_eq!(lighting_color.r, 0xab);
      assert_eq!(lighting_color.g, 0xcd);
      assert_eq!(lighting_color.b, 0xef);
      assert!(matches!(
        light,
        LightSource::Distant {
          azimuth,
          elevation
        } if (*azimuth - 15.0).abs() < f32::EPSILON && (*elevation - 60.0).abs() < f32::EPSILON
      ));
    }
    other => panic!("unexpected second primitive {other:?}"),
  }
}

#[test]
fn diffuse_lighting_colors_flat_surface() {
  let mut pixmap = solid_pixmap(1, 1, PremultipliedColorU8::from_rgba(0, 0, 0, 255).unwrap());
  let bbox = Rect::from_xywh(0.0, 0.0, 1.0, 1.0);

  let filter = SvgFilter {
    color_interpolation_filters: ColorInterpolationFilters::LinearRGB,
    steps: vec![FilterStep {
      result: None,
      color_interpolation_filters: None,
      primitive: FilterPrimitive::DiffuseLighting {
        input: FilterInput::SourceAlpha,
        surface_scale: 1.0,
        diffuse_constant: 1.0,
        kernel_unit_length: None,
        light: LightSource::Distant {
          azimuth: 0.0,
          elevation: 90.0,
        },
        lighting_color: Rgba::RED,
      },
      region: None,
    }],
    region: SvgFilterRegion {
      x: SvgLength::Number(0.0),
      y: SvgLength::Number(0.0),
      width: SvgLength::Number(1.0),
      height: SvgLength::Number(1.0),
      units: SvgFilterUnits::UserSpaceOnUse,
    },
    filter_res: None,
    primitive_units: SvgFilterUnits::UserSpaceOnUse,
  };

  apply_svg_filter(&filter, &mut pixmap, 1.0, bbox);

  let px = pixmap.pixel(0, 0).unwrap();
  assert_eq!(
    (px.red(), px.green(), px.blue(), px.alpha()),
    (255, 0, 0, 255)
  );
}

fn render_diffuse(color_space: ColorInterpolationFilters) -> PremultipliedColorU8 {
  let mut pixmap = solid_pixmap(1, 1, PremultipliedColorU8::from_rgba(0, 0, 0, 255).unwrap());
  let bbox = Rect::from_xywh(0.0, 0.0, 1.0, 1.0);
  let filter = SvgFilter {
    color_interpolation_filters: color_space,
    steps: vec![FilterStep {
      result: None,
      color_interpolation_filters: None,
      primitive: FilterPrimitive::DiffuseLighting {
        input: FilterInput::SourceAlpha,
        surface_scale: 1.0,
        diffuse_constant: 0.5,
        kernel_unit_length: None,
        light: LightSource::Distant {
          azimuth: 0.0,
          elevation: 90.0,
        },
        lighting_color: Rgba::new(128, 128, 128, 1.0),
      },
      region: None,
    }],
    region: SvgFilterRegion {
      x: SvgLength::Number(0.0),
      y: SvgLength::Number(0.0),
      width: SvgLength::Number(1.0),
      height: SvgLength::Number(1.0),
      units: SvgFilterUnits::UserSpaceOnUse,
    },
    filter_res: None,
    primitive_units: SvgFilterUnits::UserSpaceOnUse,
  };

  apply_svg_filter(&filter, &mut pixmap, 1.0, bbox);
  *pixmap.pixel(0, 0).unwrap()
}

#[test]
fn lighting_respects_color_interpolation_filters() {
  let srgb = render_diffuse(ColorInterpolationFilters::SRGB);
  let linear = render_diffuse(ColorInterpolationFilters::LinearRGB);

  assert_ne!(srgb.red(), linear.red());
  assert!(
    linear.red() > srgb.red(),
    "linear result should be brighter due to linear scaling"
  );
}

fn opaque_bounds(pixmap: &Pixmap) -> Option<(u32, u32, u32, u32)> {
  let mut min_x = pixmap.width();
  let mut min_y = pixmap.height();
  let mut max_x = 0;
  let mut max_y = 0;
  let mut seen = false;
  for y in 0..pixmap.height() {
    for x in 0..pixmap.width() {
      if pixmap.pixel(x, y).unwrap().alpha() > 0 {
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

#[test]
fn userspace_percent_regions_resolve_against_bbox() {
  let mut pixmap = Pixmap::new(80, 40).expect("pixmap");
  let bbox = Rect::from_xywh(10.0, 10.0, 80.0, 40.0);
  let filter = SvgFilter {
    color_interpolation_filters: ColorInterpolationFilters::SRGB,
    steps: vec![FilterStep {
      result: None,
      color_interpolation_filters: None,
      primitive: FilterPrimitive::Flood {
        color: Rgba::GREEN,
        opacity: 1.0,
      },
      region: Some(SvgFilterRegion {
        x: SvgLength::Percent(0.25),
        y: SvgLength::Percent(0.25),
        width: SvgLength::Percent(0.5),
        height: SvgLength::Percent(0.5),
        units: SvgFilterUnits::UserSpaceOnUse,
      }),
    }],
    region: SvgFilterRegion {
      x: SvgLength::Number(0.0),
      y: SvgLength::Number(0.0),
      width: SvgLength::Number(bbox.width()),
      height: SvgLength::Number(bbox.height()),
      units: SvgFilterUnits::UserSpaceOnUse,
    },
    filter_res: None,
    primitive_units: SvgFilterUnits::UserSpaceOnUse,
  };

  apply_svg_filter(&filter, &mut pixmap, 1.0, bbox);

  let bounds = opaque_bounds(&pixmap).expect("flood should produce opaque pixels");
  assert_eq!(bounds, (30, 20, 40, 20));
}
