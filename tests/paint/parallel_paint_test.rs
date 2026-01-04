use fastrender::error::{Error, RenderError, RenderStage};
use fastrender::paint::clip_path::ResolvedClipPath;
use fastrender::paint::display_list::{
  BlendMode, BoxShadowItem, ClipItem, ClipShape, DisplayItem, DisplayList, FillRectItem, ImageData,
  MaskReferenceRects, ResolvedFilter, ResolvedMask, ResolvedMaskImage, ResolvedMaskLayer,
  StackingContextItem, StrokeRectItem, StrokeRoundedRectItem, Transform3D, TransformItem,
};
use fastrender::paint::display_list_renderer::{DisplayListRenderer, PaintParallelism};
use fastrender::render_control::{DeadlineGuard, RenderDeadline};
use fastrender::style::types::{
  BackfaceVisibility, BackgroundBox, BackgroundPosition, BackgroundPositionComponent,
  BackgroundRepeat, BackgroundSize, BackgroundSizeComponent, MaskClip, MaskComposite, MaskMode,
  TransformStyle,
};
use fastrender::style::values::Length;
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
fn clip_path_polygon_matches_serial_output_under_tiling() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 96.0, 96.0),
    color: Rgba::new(245, 245, 245, 1.0),
  }));

  // Place the polygon away from the origin so it falls within a non-origin tile when using a
  // small tile size. This exercises the per-tile canvas translation in the parallel renderer.
  let polygon = ResolvedClipPath::Polygon {
    points: vec![
      Point::new(28.0, 30.0),
      Point::new(42.0, 30.0),
      Point::new(44.0, 40.0),
      Point::new(36.0, 46.0),
      Point::new(28.0, 38.0),
    ],
    fill_rule: tiny_skia::FillRule::Winding,
  };
  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Path { path: polygon },
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    // Deliberately larger than the polygon bounds so the clip-path mask controls the output.
    rect: Rect::from_xywh(26.0, 27.0, 21.0, 20.0),
    color: Rgba::new(40, 160, 220, 1.0),
  }));
  list.push(DisplayItem::PopClip);

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
fn mask_layers_survive_tiling() {
  // Place a masked stacking context entirely inside a tile whose renderer will translate the
  // canvas origin. This exercises `StackingContextItem.mask` / `DisplayListRenderer::render_mask`
  // under parallel tiling.
  let mut list = DisplayList::new();

  let element_rect = Rect::from_xywh(80.0, 80.0, 8.0, 8.0);

  let mask = ResolvedMask {
    layers: vec![ResolvedMaskLayer {
      // Trivial fully-opaque alpha mask that should preserve visibility.
      image: ResolvedMaskImage::Raster(ImageData::new_pixels(
        1,
        1,
        vec![0, 0, 0, 255], // RGBA8, alpha=255 => fully opaque in alpha mask mode.
      )),
      repeat: BackgroundRepeat::repeat(),
      position: BackgroundPosition::Position {
        x: BackgroundPositionComponent {
          alignment: 0.0,
          offset: Length::px(0.0),
        },
        y: BackgroundPositionComponent {
          alignment: 0.0,
          offset: Length::px(0.0),
        },
      },
      size: BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto),
      origin: BackgroundBox::BorderBox,
      clip: MaskClip::BorderBox,
      mode: MaskMode::Alpha,
      composite: MaskComposite::Add,
    }],
    color: Rgba::BLACK,
    font_size: 16.0,
    root_font_size: 16.0,
    rects: MaskReferenceRects {
      border: element_rect,
      padding: element_rect,
      content: element_rect,
    },
  };

  let stacking = StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: element_rect,
    plane_rect: element_rect,
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: None,
    child_perspective: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: Some(mask),
  };

  list.push(DisplayItem::PushStackingContext(stacking));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: element_rect,
    color: Rgba::new(200, 60, 140, 1.0),
  }));
  list.push(DisplayItem::PopStackingContext);

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
fn preserve_3d_stacking_contexts_trigger_serial_fallback() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 256.0, 256.0),
    color: Rgba::WHITE,
  }));

  let root_bounds = Rect::from_xywh(0.0, 0.0, 256.0, 256.0);
  let root = StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: root_bounds,
    plane_rect: root_bounds,
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: None,
    child_perspective: None,
    transform_style: TransformStyle::Preserve3d,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  };
  list.push(DisplayItem::PushStackingContext(root));

  let plane_bounds = Rect::from_xywh(48.0, 48.0, 64.0, 64.0);
  let plane = StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: plane_bounds,
    plane_rect: plane_bounds,
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: Some(Transform3D::translate(0.0, 0.0, 20.0)),
    child_perspective: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  };
  list.push(DisplayItem::PushStackingContext(plane));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: plane_bounds,
    color: Rgba::new(200, 60, 120, 1.0),
  }));
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);

  let font_ctx = FontContext::new();
  let serial = DisplayListRenderer::new(256, 256, Rgba::WHITE, font_ctx.clone())
    .unwrap()
    .with_parallelism(PaintParallelism::disabled())
    .render(&list)
    .expect("serial paint");

  let parallelism = PaintParallelism {
    tile_size: 64,
    log_timing: false,
    min_display_items: 1,
    min_tiles: 1,
    min_build_fragments: 1,
    build_chunk_size: 1,
    ..PaintParallelism::enabled()
  };
  let pool = ThreadPoolBuilder::new().num_threads(4).build().unwrap();
  let report = pool.install(|| {
    DisplayListRenderer::new(256, 256, Rgba::WHITE, font_ctx)
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("paint")
  });

  assert!(
    !report.parallel_used,
    "preserve-3d should disable parallel painting"
  );
  let reason = report.fallback_reason.as_deref().unwrap_or_default();
  assert!(
    reason.contains("preserve"),
    "expected preserve-3d fallback reason, got {reason:?}"
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
fn mask_composite_respects_deadline() {
  // Regression test for `mask-composite` per-pixel compositing loops: under a tight deadline the
  // renderer must abort (instead of finishing the O(pixels) loop and only noticing the timeout
  // afterwards).
  let size = 2048u32;
  let bounds = Rect::from_xywh(0.0, 0.0, size as f32, size as f32);
  let mask_rects = MaskReferenceRects {
    border: bounds,
    padding: bounds,
    content: bounds,
  };

  let image = ImageData::new_pixels(1, 1, vec![0, 0, 0, 255]);
  let layer_template = ResolvedMaskLayer {
    image: ResolvedMaskImage::Raster(image),
    repeat: BackgroundRepeat::no_repeat(),
    position: BackgroundPosition::Position {
      x: BackgroundPositionComponent {
        alignment: 0.0,
        offset: Length::px(0.0),
      },
      y: BackgroundPositionComponent {
        alignment: 0.0,
        offset: Length::px(0.0),
      },
    },
    size: BackgroundSize::Explicit(
      BackgroundSizeComponent::Length(Length::percent(100.0)),
      BackgroundSizeComponent::Length(Length::percent(100.0)),
    ),
    origin: BackgroundBox::BorderBox,
    clip: MaskClip::BorderBox,
    mode: MaskMode::Alpha,
    composite: MaskComposite::Add,
  };

  let mask = ResolvedMask {
    layers: vec![
      layer_template.clone(),
      layer_template.clone(),
      layer_template.clone(),
      layer_template,
    ],
    color: Rgba::BLACK,
    font_size: 16.0,
    root_font_size: 16.0,
    rects: mask_rects,
  };

  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds,
    plane_rect: bounds,
    mix_blend_mode: BlendMode::Normal,
    is_isolated: true,
    transform: None,
    child_perspective: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: Some(mask),
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: bounds,
    color: Rgba::new(30, 160, 220, 1.0),
  }));
  list.push(DisplayItem::PopStackingContext);

  let renderer = DisplayListRenderer::new(size, size, Rgba::WHITE, FontContext::new())
    .unwrap()
    .with_parallelism(PaintParallelism::disabled());

  let deadline = RenderDeadline::new(Some(Duration::from_millis(1)), None);
  let _guard = DeadlineGuard::install(Some(&deadline));
  let err = renderer.render(&list).unwrap_err();

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

#[test]
fn huge_effect_halo_triggers_serial_fallback() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 128.0, 128.0),
    color: Rgba::WHITE,
  }));
  list.push(DisplayItem::BoxShadow(BoxShadowItem {
    rect: Rect::from_xywh(32.0, 32.0, 64.0, 64.0),
    radii: BorderRadii::ZERO,
    offset: Point::new(0.0, 0.0),
    blur_radius: 60.0,
    spread_radius: 0.0,
    color: Rgba::new(0, 0, 0, 0.6),
    inset: false,
  }));

  let font_ctx = FontContext::new();
  let serial = DisplayListRenderer::new(128, 128, Rgba::WHITE, font_ctx.clone())
    .unwrap()
    .with_parallelism(PaintParallelism::disabled())
    .render(&list)
    .expect("serial paint");

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
  let report = pool.install(|| {
    DisplayListRenderer::new(128, 128, Rgba::WHITE, font_ctx)
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("parallel paint")
  });

  assert!(
    !report.parallel_used,
    "expected tiling to be disabled due to huge halo amplification"
  );
  assert!(
    report
      .fallback_reason
      .as_deref()
      .unwrap_or_default()
      .contains("halo amplification"),
    "expected halo amplification fallback reason, got {:?}",
    report.fallback_reason
  );
  assert_eq!(serial.data(), report.pixmap.data());
}

#[test]
fn modest_halo_allows_parallel_tiling() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 256.0, 256.0),
    color: Rgba::WHITE,
  }));
  list.push(DisplayItem::BoxShadow(BoxShadowItem {
    rect: Rect::from_xywh(64.0, 64.0, 64.0, 64.0),
    radii: BorderRadii::ZERO,
    offset: Point::new(4.0, 4.0),
    blur_radius: 6.0,
    spread_radius: 2.0,
    color: Rgba::new(0, 0, 0, 0.4),
    inset: false,
  }));

  let parallelism = PaintParallelism {
    tile_size: 64,
    min_display_items: 1,
    min_tiles: 1,
    min_build_fragments: 1,
    build_chunk_size: 1,
    ..PaintParallelism::auto()
  };
  let pool = ThreadPoolBuilder::new().num_threads(4).build().unwrap();
  let report = pool.install(|| {
    DisplayListRenderer::new(256, 256, Rgba::WHITE, FontContext::new())
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("render")
  });

  assert!(
    report.parallel_used,
    "expected modest halo to keep parallel tiling enabled (fallback={:?})",
    report.fallback_reason
  );
}
