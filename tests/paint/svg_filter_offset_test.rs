use fastrender::paint::svg_filter::{apply_svg_filter, FilterInput, FilterPrimitive, FilterStep, SvgFilter};
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
  let filter = SvgFilter {
    steps: vec![FilterStep {
      result: None,
      primitive: FilterPrimitive::Offset {
        input: FilterInput::SourceGraphic,
        dx,
        dy: 0.0,
      },
    }],
  };
  apply_svg_filter(&filter, &mut pixmap);
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

  assert!(left_alpha > 0 && left_alpha < 255, "left edge should be partially covered");
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
