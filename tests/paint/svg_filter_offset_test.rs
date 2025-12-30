use fastrender::paint::svg_filter::{
  apply_svg_filter, ColorInterpolationFilters, FilterInput, FilterPrimitive, FilterStep, SvgFilter,
  SvgFilterRegion, SvgFilterUnits, SvgLength,
};
use fastrender::{Rect, Rgba};
use tiny_skia::{Pixmap, PremultipliedColorU8};

fn vertical_line_pixmap() -> Pixmap {
  let mut pixmap = Pixmap::new(5, 3).expect("pixmap");
  let black = PremultipliedColorU8::from_rgba(0, 0, 0, 255).expect("black");
  let width = pixmap.width() as usize;
  for (idx, px) in pixmap.pixels_mut().iter_mut().enumerate() {
    if idx % width == 2 {
      *px = black;
    }
  }
  pixmap
}

fn render_offset(dx: f32) -> Pixmap {
  let mut pixmap = vertical_line_pixmap();
  let bbox = Rect::from_xywh(0.0, 0.0, pixmap.width() as f32, pixmap.height() as f32);
  let mut filter = SvgFilter {
    color_interpolation_filters: ColorInterpolationFilters::SRGB,
    steps: vec![FilterStep {
      result: None,
      color_interpolation_filters: None,
      primitive: FilterPrimitive::Offset {
        input: FilterInput::SourceGraphic,
        dx,
        dy: 0.0,
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
  primitive_units: SvgFilterUnits::UserSpaceOnUse,
  fingerprint: 0,
};
  apply_svg_filter(&filter, &mut pixmap, 1.0, bbox).unwrap();
  pixmap
}

fn render_drop_shadow(dx: f32) -> Pixmap {
  let mut pixmap = vertical_line_pixmap();
  let bbox = Rect::from_xywh(0.0, 0.0, pixmap.width() as f32, pixmap.height() as f32);
  let mut filter = SvgFilter {
    color_interpolation_filters: ColorInterpolationFilters::SRGB,
    steps: vec![FilterStep {
      result: None,
      color_interpolation_filters: None,
      primitive: FilterPrimitive::DropShadow {
        input: FilterInput::SourceGraphic,
        dx,
        dy: 0.0,
        std_dev: (0.0, 0.0),
        color: Rgba::new(0, 0, 0, 1.0),
        opacity: 1.0,
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
  primitive_units: SvgFilterUnits::UserSpaceOnUse,
  fingerprint: 0,
};
  apply_svg_filter(&filter, &mut pixmap, 1.0, bbox).unwrap();
  pixmap
}

#[test]
fn fe_offset_preserves_subpixel_offsets() {
  let offset_0 = render_offset(0.0);
  let offset_half = render_offset(0.5);
  let offset_1 = render_offset(1.0);

  let mid_y = 1;
  let line_x = 2;
  let alpha_at = |pixmap: &Pixmap, x: u32| pixmap.pixel(x, mid_y).unwrap().alpha();

  assert_eq!(alpha_at(&offset_0, line_x), 255);
  assert_eq!(alpha_at(&offset_1, line_x + 1), 255);

  let left_alpha = alpha_at(&offset_half, line_x);
  let right_alpha = alpha_at(&offset_half, line_x + 1);

  assert!(
    left_alpha > 0 && left_alpha < 255,
    "left edge should be partially covered"
  );
  assert!(
    right_alpha > 0 && right_alpha < 255,
    "right edge should be partially covered"
  );

  assert_ne!(
    offset_half.data(),
    offset_0.data(),
    "subpixel offset should differ from no offset"
  );
  assert_ne!(
    offset_half.data(),
    offset_1.data(),
    "subpixel offset should differ from whole pixel offset"
  );
}

#[test]
fn drop_shadow_preserves_subpixel_offsets() {
  let shadow_0 = render_drop_shadow(0.0);
  let shadow_half = render_drop_shadow(0.5);
  let shadow_1 = render_drop_shadow(1.0);

  let mid_y = 1;
  let right_of_line = 3;
  let alpha_at = |pixmap: &Pixmap, x: u32| pixmap.pixel(x, mid_y).unwrap().alpha();

  assert_eq!(alpha_at(&shadow_0, right_of_line), 0);
  assert_eq!(alpha_at(&shadow_1, right_of_line), 255);

  let mid_alpha = alpha_at(&shadow_half, right_of_line);
  assert!(
    mid_alpha > 0 && mid_alpha < 255,
    "subpixel offset should yield partial coverage"
  );
}
