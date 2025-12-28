use fastrender::paint::display_list::{
  BlendMode, BoxShadowItem, DisplayItem, DisplayList, FillRectItem, ResolvedFilter,
  StackingContextItem, StrokeRectItem, StrokeRoundedRectItem,
};
use fastrender::paint::display_list_renderer::{DisplayListRenderer, PaintParallelism};
use fastrender::style::types::{BackfaceVisibility, TransformStyle};
use fastrender::text::font_loader::FontContext;
use fastrender::{BorderRadii, Point, Rect, Rgba};

fn basic_list() -> DisplayList {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 128.0, 128.0),
    color: Rgba::WHITE,
  }));
  list.push(DisplayItem::BoxShadow(BoxShadowItem {
    rect: Rect::from_xywh(20.0, 20.0, 48.0, 48.0),
    radii: BorderRadii::ZERO,
    offset: Point::new(6.0, 6.0),
    blur_radius: 6.0,
    spread_radius: 4.0,
    color: Rgba::new(0, 0, 0, 0.4),
    inset: false,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(24.0, 24.0, 40.0, 40.0),
    color: Rgba::new(30, 160, 220, 1.0),
  }));
  list
}

#[test]
fn parallel_paint_matches_serial_output() {
  let list = basic_list();
  let font_ctx = FontContext::new();

  let serial = DisplayListRenderer::new(128, 128, Rgba::WHITE, font_ctx.clone())
    .unwrap()
    .render(&list)
    .expect("serial paint");

  let parallelism = PaintParallelism {
    enabled: true,
    tile_size: 32,
    log_timing: false,
  };
  let report = DisplayListRenderer::new(128, 128, Rgba::WHITE, font_ctx)
    .unwrap()
    .with_parallelism(parallelism)
    .render_with_report(&list)
    .expect("parallel paint");

  assert!(report.parallel_used, "expected tiling to be used");
  assert_eq!(serial.data(), report.pixmap.data());
}

#[test]
fn thick_strokes_survive_tiling() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 96.0, 96.0),
    color: Rgba::WHITE,
  }));
  list.push(DisplayItem::StrokeRect(StrokeRectItem {
    rect: Rect::from_xywh(8.0, 8.0, 80.0, 80.0),
    color: Rgba::new(20, 120, 200, 1.0),
    width: 18.0,
    blend_mode: BlendMode::Normal,
  }));
  list.push(DisplayItem::StrokeRoundedRect(StrokeRoundedRectItem {
    rect: Rect::from_xywh(20.0, 20.0, 56.0, 56.0),
    color: Rgba::new(200, 60, 120, 1.0),
    width: 12.0,
    radii: BorderRadii::uniform(10.0),
  }));

  let font_ctx = FontContext::new();

  let serial = DisplayListRenderer::new(112, 112, Rgba::WHITE, font_ctx.clone())
    .unwrap()
    .render(&list)
    .expect("serial paint");

  let parallelism = PaintParallelism {
    enabled: true,
    tile_size: 24,
    log_timing: false,
  };
  let report = DisplayListRenderer::new(112, 112, Rgba::WHITE, font_ctx)
    .unwrap()
    .with_parallelism(parallelism)
    .render_with_report(&list)
    .expect("parallel paint");

  assert!(report.parallel_used, "expected tiling to be used");
  assert_eq!(serial.data(), report.pixmap.data());
}

#[test]
fn backdrop_filters_trigger_serial_fallback() {
  let mut list = DisplayList::new();
  let stacking = StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 80.0, 80.0),
    plane_rect: Rect::from_xywh(0.0, 0.0, 80.0, 80.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: None,
    child_perspective: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: vec![ResolvedFilter::Blur(3.0)],
    radii: BorderRadii::ZERO,
    mask: None,
  };
  list.push(DisplayItem::PushStackingContext(stacking));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(8.0, 8.0, 40.0, 40.0),
    color: Rgba::new(0, 200, 120, 0.8),
  }));
  list.push(DisplayItem::PopStackingContext);

  let font_ctx = FontContext::new();
  let serial = DisplayListRenderer::new(96, 96, Rgba::WHITE, font_ctx.clone())
    .unwrap()
    .render(&list)
    .expect("serial paint");

  let parallelism = PaintParallelism {
    enabled: true,
    tile_size: 24,
    log_timing: false,
  };
  let report = DisplayListRenderer::new(96, 96, Rgba::WHITE, font_ctx)
    .unwrap()
    .with_parallelism(parallelism)
    .render_with_report(&list)
    .expect("parallel paint");

  assert!(
    !report.parallel_used,
    "backdrop filters should disable parallel painting"
  );
  assert_eq!(serial.data(), report.pixmap.data());
}
