use fastrender::error::{Error, RenderError, RenderStage};
use fastrender::paint::display_list::{
  BlendMode, BoxShadowItem, ClipItem, ClipShape, DisplayItem, DisplayList, FillRectItem,
  ResolvedFilter, StackingContextItem, StrokeRectItem, StrokeRoundedRectItem, Transform3D,
  TransformItem,
};
use fastrender::paint::display_list_renderer::{DisplayListRenderer, PaintParallelism};
use fastrender::render_control::{DeadlineGuard, RenderDeadline};
use fastrender::style::types::{BackfaceVisibility, TransformStyle};
use fastrender::text::font_loader::FontContext;
use fastrender::{BorderRadii, Point, Rect, Rgba};
use rayon::ThreadPoolBuilder;
use std::time::Duration;

struct EnvGuard(&'static str);

impl EnvGuard {
  fn set(key: &'static str, value: &str) -> Self {
    std::env::set_var(key, value);
    Self(key)
  }
}

impl Drop for EnvGuard {
  fn drop(&mut self) {
    std::env::remove_var(self.0);
  }
}

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
    .with_parallelism(PaintParallelism::disabled())
    .render(&list)
    .expect("serial paint");

  let parallelism = PaintParallelism {
    tile_size: 32,
    log_timing: false,
    min_display_items: 1,
    min_tiles: 1,
    min_build_fragments: 1,
    build_chunk_size: 1,
    ..PaintParallelism::enabled()
  };
  let pool = ThreadPoolBuilder::new().num_threads(4).build().unwrap();
  let report = pool.install(|| {
    DisplayListRenderer::new(128, 128, Rgba::WHITE, font_ctx)
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("parallel paint")
  });

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
    .with_parallelism(PaintParallelism::disabled())
    .render(&list)
    .expect("serial paint");

  let parallelism = PaintParallelism {
    tile_size: 24,
    log_timing: false,
    min_display_items: 1,
    min_tiles: 1,
    min_build_fragments: 1,
    build_chunk_size: 1,
    ..PaintParallelism::enabled()
  };
  let pool = ThreadPoolBuilder::new().num_threads(4).build().unwrap();
  let report = pool.install(|| {
    DisplayListRenderer::new(112, 112, Rgba::WHITE, font_ctx)
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("parallel paint")
  });

  assert!(report.parallel_used, "expected tiling to be used");
  assert_eq!(serial.data(), report.pixmap.data());
}

#[test]
fn clip_transform_and_stacking_context_match_serial_output() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 96.0, 96.0),
    color: Rgba::new(245, 245, 245, 1.0),
  }));
  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Rect {
      rect: Rect::from_xywh(10.0, 10.0, 60.0, 50.0),
      radii: Some(BorderRadii::uniform(4.0)),
    },
  }));
  list.push(DisplayItem::PushTransform(TransformItem {
    transform: Transform3D::translate(8.0, -6.0, 0.0),
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(8.0, 22.0, 40.0, 24.0),
    color: Rgba::new(40, 160, 220, 1.0),
  }));
  list.push(DisplayItem::PopTransform);
  list.push(DisplayItem::PopClip);

  let stacking = StackingContextItem {
    z_index: 1,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(18.0, 26.0, 40.0, 36.0),
    plane_rect: Rect::from_xywh(18.0, 26.0, 40.0, 36.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: Some(Transform3D::translate(6.0, 4.0, 0.0)),
    child_perspective: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::uniform(2.0),
    mask: None,
  };
  list.push(DisplayItem::PushStackingContext(stacking));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(12.0, 12.0, 24.0, 18.0),
    color: Rgba::new(200, 90, 120, 0.95),
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(30.0, 16.0, 12.0, 12.0),
    color: Rgba::new(90, 200, 160, 1.0),
  }));
  list.push(DisplayItem::PopStackingContext);

  let font_ctx = FontContext::new();
  let serial = DisplayListRenderer::new(96, 96, Rgba::WHITE, font_ctx.clone())
    .unwrap()
    .with_parallelism(PaintParallelism::disabled())
    .render(&list)
    .expect("serial paint");

  let parallelism = PaintParallelism {
    tile_size: 24,
    log_timing: false,
    min_display_items: 1,
    min_tiles: 1,
    min_build_fragments: 1,
    build_chunk_size: 1,
    ..PaintParallelism::enabled()
  };
  let pool = ThreadPoolBuilder::new().num_threads(4).build().unwrap();
  let report = pool.install(|| {
    DisplayListRenderer::new(96, 96, Rgba::WHITE, font_ctx)
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("parallel paint")
  });

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
    .with_parallelism(PaintParallelism::disabled())
    .render(&list)
    .expect("serial paint");

  let parallelism = PaintParallelism {
    tile_size: 24,
    log_timing: false,
    min_display_items: 1,
    min_tiles: 1,
    min_build_fragments: 1,
    build_chunk_size: 1,
    ..PaintParallelism::enabled()
  };
  let pool = ThreadPoolBuilder::new().num_threads(4).build().unwrap();
  let report = pool.install(|| {
    DisplayListRenderer::new(96, 96, Rgba::WHITE, font_ctx)
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("parallel paint")
  });

  assert!(
    !report.parallel_used,
    "backdrop filters should disable parallel painting"
  );
  assert_eq!(serial.data(), report.pixmap.data());
}

#[test]
fn parallel_paint_respects_deadline() {
  let _delay_guard = EnvGuard::set("FASTR_TEST_RENDER_DELAY_MS", "20");
  let parallelism = PaintParallelism {
    tile_size: 16,
    log_timing: false,
    min_display_items: 1,
    min_tiles: 1,
    min_build_fragments: 1,
    build_chunk_size: 1,
    ..PaintParallelism::enabled()
  };
  let pool = ThreadPoolBuilder::new().num_threads(4).build().unwrap();
  let err = pool.install(|| {
    let deadline = RenderDeadline::new(Some(Duration::from_millis(1)), None);
    let _guard = DeadlineGuard::install(Some(&deadline));
    DisplayListRenderer::new(128, 128, Rgba::WHITE, FontContext::new())
      .unwrap()
      .with_parallelism(parallelism)
      .render(&basic_list())
      .unwrap_err()
  });

  match err {
    Error::Render(RenderError::Timeout { stage, .. }) => assert_eq!(stage, RenderStage::Paint),
    other => panic!("expected paint timeout, got {other:?}"),
  }
}

#[test]
fn auto_parallelizes_expensive_gradients() {
  use fastrender::paint::display_list::{GradientSpread, GradientStop, LinearGradientItem};

  let mut list = DisplayList::new();
  list.push(DisplayItem::LinearGradient(LinearGradientItem {
    rect: Rect::from_xywh(0.0, 0.0, 512.0, 512.0),
    start: Point::new(0.0, 0.0),
    end: Point::new(512.0, 0.0),
    spread: GradientSpread::Pad,
    stops: vec![
      GradientStop {
        position: 0.0,
        color: Rgba::rgb(255, 0, 0),
      },
      GradientStop {
        position: 1.0,
        color: Rgba::rgb(0, 0, 255),
      },
    ],
  }));

  let font_ctx = FontContext::new();
  let serial = DisplayListRenderer::new(512, 512, Rgba::WHITE, font_ctx.clone())
    .unwrap()
    .with_parallelism(PaintParallelism::disabled())
    .render(&list)
    .expect("serial render");

  // Force the item-count heuristic to fail so AUTO must rely on pixel-based cost estimation.
  let parallelism = PaintParallelism {
    tile_size: 64,
    min_display_items: 10_000,
    ..PaintParallelism::auto()
  };
  let pool = ThreadPoolBuilder::new().num_threads(4).build().unwrap();
  let report = pool.install(|| {
    DisplayListRenderer::new(512, 512, Rgba::WHITE, font_ctx)
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("auto render")
  });

  assert!(report.parallel_used, "expected AUTO to enable tiling");
  assert!(report.parallel_threads > 1, "expected multiple threads");
  assert_eq!(serial.data(), report.pixmap.data());
}
