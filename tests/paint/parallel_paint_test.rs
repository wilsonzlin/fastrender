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
  BackfaceVisibility, BackgroundPosition, BackgroundPositionComponent, BackgroundRepeat,
  BackgroundSize, BackgroundSizeComponent, MaskClip, MaskComposite, MaskLayer, MaskMode,
  MaskOrigin, TransformStyle,
};
use fastrender::text::font_loader::FontContext;
use fastrender::{BorderRadii, Length, LengthUnit, Point, Rect, Rgba};
use rayon::ThreadPoolBuilder;
use std::sync::Arc;
use std::time::Duration;
use tiny_skia::Pixmap;

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

fn assert_rgba8888_pixels_eq(
  width: u32,
  height: u32,
  expected: &[u8],
  actual: &[u8],
  label: &str,
) {
  assert_eq!(
    expected.len(),
    actual.len(),
    "{label}: pixel buffer sizes differ"
  );
  assert_eq!(
    expected.len(),
    width as usize * height as usize * 4,
    "{label}: expected buffer is not width*height*4"
  );

  if expected == actual {
    return;
  }

  let mut mismatched_bytes = 0usize;
  let mut mismatched_pixels = 0usize;
  let mut first: Option<(usize, [u8; 4], [u8; 4])> = None;
  let mut min_x = usize::MAX;
  let mut min_y = usize::MAX;
  let mut max_x = 0usize;
  let mut max_y = 0usize;
  let mut samples: Vec<(usize, usize, [u8; 4], [u8; 4])> = Vec::new();
  for (idx, (a, b)) in expected
    .chunks_exact(4)
    .zip(actual.chunks_exact(4))
    .enumerate()
  {
    let a = [a[0], a[1], a[2], a[3]];
    let b = [b[0], b[1], b[2], b[3]];
    if a != b {
      mismatched_pixels += 1;
      mismatched_bytes += a.iter().zip(b.iter()).filter(|(x, y)| x != y).count();
      if first.is_none() {
        first = Some((idx, a, b));
      }
      let x = idx % (width as usize);
      let y = idx / (width as usize);
      min_x = min_x.min(x);
      min_y = min_y.min(y);
      max_x = max_x.max(x);
      max_y = max_y.max(y);
      if samples.len() < 16 {
        samples.push((x, y, a, b));
      }
    }
  }

  if let Some((idx, a, b)) = first {
    let x = idx % (width as usize);
    let y = idx / (width as usize);
    panic!(
      "{label}: {mismatched_pixels} pixels ({mismatched_bytes} bytes) differ; bounds=({min_x},{min_y})..=({max_x},{max_y}); first at ({x}, {y}) expected={a:?} actual={b:?}; sample={samples:?}"
    );
  }
  panic!("{label}: buffers differ");
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

fn top_left_position() -> BackgroundPosition {
  BackgroundPosition::Position {
    x: BackgroundPositionComponent {
      alignment: 0.0,
      offset: Length::percent(0.0),
    },
    y: BackgroundPositionComponent {
      alignment: 0.0,
      offset: Length::percent(0.0),
    },
  }
}

fn patterned_mask(bounds: Rect) -> ResolvedMask {
  const SIZE: u32 = 8;
  let mut pixels = Vec::with_capacity((SIZE * SIZE * 4) as usize);
  for y in 0..SIZE {
    for x in 0..SIZE {
      let base = x * 32 + y * 4;
      let alpha = if base < 24 {
        0
      } else if base > 224 {
        255
      } else {
        base as u8
      };
      pixels.extend_from_slice(&[0, 0, 0, alpha]);
    }
  }

  ResolvedMask {
    layers: vec![ResolvedMaskLayer {
      image: ResolvedMaskImage::Raster(ImageData::new_pixels(SIZE, SIZE, pixels)),
      repeat: BackgroundRepeat::repeat(),
      position: top_left_position(),
      size: BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto),
      origin: MaskOrigin::BorderBox,
      clip: MaskClip::BorderBox,
      mode: MaskMode::Alpha,
      composite: MaskComposite::Add,
    }],
    color: Rgba::BLACK,
    font_size: 16.0,
    root_font_size: 16.0,
    viewport: None,
    rects: MaskReferenceRects {
      border: bounds,
      padding: bounds,
      content: bounds,
    },
  }
}

fn assert_pixmap_eq(serial: &Pixmap, parallel: &Pixmap) {
  assert_eq!(serial.width(), parallel.width(), "pixmap width mismatch");
  assert_eq!(serial.height(), parallel.height(), "pixmap height mismatch");
  let serial_data = serial.data();
  let parallel_data = parallel.data();
  if serial_data == parallel_data {
    return;
  }

  let width = serial.width() as usize;
  let height = serial.height() as usize;
  let mut first_mismatch: Option<(usize, usize, [u8; 4], [u8; 4])> = None;
  let mut diff_min_x = u32::MAX;
  let mut diff_min_y = u32::MAX;
  let mut diff_max_x = 0u32;
  let mut diff_max_y = 0u32;

  for y in 0..height {
    for x in 0..width {
      let base = (y * width + x) * 4;
      let sa = &serial_data[base..base + 4];
      let pa = &parallel_data[base..base + 4];
      if sa == pa {
        continue;
      }
      if first_mismatch.is_none() {
        first_mismatch = Some((x, y, sa.try_into().unwrap(), pa.try_into().unwrap()));
      }
      diff_min_x = diff_min_x.min(x as u32);
      diff_min_y = diff_min_y.min(y as u32);
      diff_max_x = diff_max_x.max(x as u32);
      diff_max_y = diff_max_y.max(y as u32);
    }
  }

  if let Some((x, y, sa, pa)) = first_mismatch {
    panic!(
      "pixmaps differ at ({x},{y}): serial={sa:?} parallel={pa:?}; diff_bbox=({diff_min_x},{diff_min_y})-({diff_max_x},{diff_max_y})"
    );
  }

  panic!("pixmaps differ, but could not locate mismatch");
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
  assert_pixmap_eq(&serial, &report.pixmap);
}

#[test]
fn mask_parallel_paint_matches_serial_output() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 128.0, 128.0),
    color: Rgba::WHITE,
  }));

  // Keep the masked element well inside a single tile so serial/parallel outputs should be
  // byte-identical once mask translation is handled correctly.
  let bounds = Rect::from_xywh(70.0, 70.0, 20.0, 20.0);
  let mut layer = MaskLayer::default();
  layer.repeat = BackgroundRepeat::no_repeat();
  layer.size = BackgroundSize::Explicit(
    BackgroundSizeComponent::Length(Length::percent(100.0)),
    BackgroundSizeComponent::Length(Length::percent(100.0)),
  );

  let image = ImageData::new_pixels(1, 1, vec![0, 0, 0, 255]);
  let mask = ResolvedMask {
    layers: vec![ResolvedMaskLayer {
      image: ResolvedMaskImage::Raster(image),
      repeat: layer.repeat,
      position: layer.position,
      size: layer.size,
      origin: layer.origin,
      clip: layer.clip,
      mode: layer.mode,
      composite: layer.composite,
    }],
    color: Rgba::BLACK,
    font_size: 16.0,
    root_font_size: 16.0,
    viewport: None,
    rects: MaskReferenceRects {
      border: bounds,
      padding: bounds,
      content: bounds,
    },
  };

  let stacking = StackingContextItem {
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
  };
  list.push(DisplayItem::PushStackingContext(stacking));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: bounds,
    color: Rgba::new(30, 160, 220, 1.0),
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
  let serial_data = serial.data();
  let parallel_data = report.pixmap.data();
  if serial_data != parallel_data {
    let mut first = None;
    let mut diff = 0usize;
    for (idx, (&a, &b)) in serial_data.iter().zip(parallel_data.iter()).enumerate() {
      if a != b {
        diff += 1;
        if first.is_none() {
          let pixel = idx / 4;
          let x = pixel % 128;
          let y = pixel / 128;
          first = Some((idx, x, y, a, b));
        }
      }
    }
    panic!(
      "parallel output differs from serial: diff_bytes={diff} first_mismatch={first:?}"
    );
  }
}

#[test]
fn viewport_unit_masks_match_serial_output() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 256.0, 256.0),
    color: Rgba::WHITE,
  }));

  let bounds = Rect::from_xywh(96.0, 96.0, 128.0, 128.0);
  let layer = ResolvedMaskLayer {
    image: ResolvedMaskImage::Raster(ImageData::new_pixels(1, 1, vec![0, 0, 0, 255])),
    repeat: BackgroundRepeat::no_repeat(),
    position: BackgroundPosition::Position {
      x: BackgroundPositionComponent {
        alignment: 0.0,
        offset: Length::percent(0.0),
      },
      y: BackgroundPositionComponent {
        alignment: 0.0,
        offset: Length::percent(0.0),
      },
    },
    size: BackgroundSize::Explicit(
      BackgroundSizeComponent::Length(Length::new(50.0, LengthUnit::Vw)),
      BackgroundSizeComponent::Length(Length::new(50.0, LengthUnit::Vh)),
    ),
    origin: MaskOrigin::BorderBox,
    clip: MaskClip::BorderBox,
    mode: MaskMode::Alpha,
    composite: MaskComposite::Add,
  };
  let mask = ResolvedMask {
    layers: vec![layer],
    color: Rgba::BLACK,
    font_size: 16.0,
    root_font_size: 16.0,
    viewport: Some((256.0, 256.0)),
    rects: MaskReferenceRects {
      border: bounds,
      padding: bounds,
      content: bounds,
    },
  };

  let stacking = StackingContextItem {
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
  };
  list.push(DisplayItem::PushStackingContext(stacking));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: bounds,
    color: Rgba::new(30, 160, 220, 1.0),
  }));
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
      .expect("parallel paint")
  });

  assert!(report.parallel_used, "expected tiling to be used");
  assert_pixmap_eq(&serial, &report.pixmap);
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
  assert_pixmap_eq(&serial, &report.pixmap);
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
  assert_pixmap_eq(&serial, &report.pixmap);
}

#[test]
fn stacking_context_mask_matches_serial_output() {
  let canvas_rect = Rect::from_xywh(0.0, 0.0, 96.0, 96.0);
  let masked_bounds = Rect::from_xywh(40.0, 40.0, 44.0, 44.0);

  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: canvas_rect,
    color: Rgba::WHITE,
  }));

  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: masked_bounds,
    plane_rect: masked_bounds,
    mix_blend_mode: BlendMode::Normal,
    is_isolated: true,
    transform: None,
    child_perspective: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: Some(patterned_mask(masked_bounds)),
  }));
  // Fill the masked bounds with a solid color so the mask alpha pattern is visible.
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: masked_bounds,
    color: Rgba::new(220, 40, 60, 1.0),
  }));
  // Add a second rect so the mask has to affect multiple overlapping primitives.
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(
      masked_bounds.x() + 10.0,
      masked_bounds.y() + 6.0,
      masked_bounds.width() - 14.0,
      masked_bounds.height() - 18.0,
    ),
    color: Rgba::new(40, 140, 220, 0.9),
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
  assert_pixmap_eq(&serial, &report.pixmap);
}

#[test]
fn stacking_context_isolated_layer_matches_serial_output() {
  let canvas_rect = Rect::from_xywh(0.0, 0.0, 96.0, 96.0);
  let bounds = Rect::from_xywh(40.0, 40.0, 44.0, 44.0);

  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: canvas_rect,
    color: Rgba::WHITE,
  }));

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
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: bounds,
    color: Rgba::new(220, 40, 60, 1.0),
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
  assert_pixmap_eq(&serial, &report.pixmap);
}

#[test]
fn svg_filter_and_rounded_clip_match_serial_output_in_translated_tiles() {
  use fastrender::paint::svg_filter::{
    ColorInterpolationFilters, FilterPrimitive, FilterStep, SvgFilter, SvgFilterRegion,
    SvgFilterUnits, SvgLength,
  };

  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 256.0, 256.0),
    color: Rgba::WHITE,
  }));

  let mut filter = SvgFilter {
    color_interpolation_filters: ColorInterpolationFilters::SRGB,
    steps: vec![FilterStep {
      result: None,
      color_interpolation_filters: None,
      primitive: FilterPrimitive::Flood {
        color: Rgba::new(20, 120, 220, 1.0),
        opacity: 1.0,
      },
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
  let filter = Arc::new(filter);

  // Place the stacking context inside a non-origin tile to exercise the per-tile canvas
  // translation when computing effect bounds.
  let bounds = Rect::from_xywh(150.0, 150.0, 40.0, 40.0);
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
    filters: vec![ResolvedFilter::SvgFilter(filter)],
    backdrop_filters: Vec::new(),
    radii: BorderRadii::uniform(10.0),
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(150.0, 150.0, 40.0, 40.0),
    color: Rgba::new(200, 60, 120, 1.0),
  }));
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
      .expect("parallel paint")
  });

  assert!(report.parallel_used, "expected tiling to be used");
  assert_eq!(serial.data(), report.pixmap.data());
}

#[test]
fn mix_blend_mode_triggers_serial_fallback_without_isolation() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 96.0, 96.0),
    color: Rgba::rgb(255, 0, 0),
  }));
  let stacking = StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 80.0, 80.0),
    plane_rect: Rect::from_xywh(0.0, 0.0, 80.0, 80.0),
    mix_blend_mode: BlendMode::Multiply,
    is_isolated: false,
    transform: None,
    child_perspective: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  };
  list.push(DisplayItem::PushStackingContext(stacking));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(8.0, 8.0, 40.0, 40.0),
    color: Rgba::new(0, 0, 255, 1.0),
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
    "mix-blend-mode on non-isolated context should disable parallel painting (fallback={:?})",
    report.fallback_reason
  );
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
      Point::new(33.0, 29.0),
      Point::new(72.0, 33.0),
      Point::new(80.0, 56.0),
      Point::new(60.0, 78.0),
      Point::new(34.0, 60.0),
    ],
    fill_rule: tiny_skia::FillRule::Winding,
  };
  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Path { path: polygon },
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    // Deliberately larger than the polygon bounds so the clip-path mask controls the output.
    rect: Rect::from_xywh(24.0, 24.0, 64.0, 64.0),
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
  assert_pixmap_eq(&serial, &report.pixmap);
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
      origin: MaskOrigin::BorderBox,
      clip: MaskClip::BorderBox,
      mode: MaskMode::Alpha,
      composite: MaskComposite::Add,
    }],
    color: Rgba::BLACK,
    font_size: 16.0,
    root_font_size: 16.0,
    viewport: None,
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
  assert_pixmap_eq(&serial, &report.pixmap);
}

#[test]
fn mask_viewport_units_match_serial_output() {
  let viewport_w = 128;
  let viewport_h = 128;
  let bounds = Rect::from_xywh(0.0, 0.0, 30.0, 30.0);

  let mask = ResolvedMask {
    layers: vec![ResolvedMaskLayer {
      image: ResolvedMaskImage::Raster(ImageData::new_pixels(1, 1, vec![0, 0, 0, 255])),
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
        BackgroundSizeComponent::Length(Length::new(25.0, LengthUnit::Vw)),
        BackgroundSizeComponent::Length(Length::new(25.0, LengthUnit::Vh)),
      ),
      origin: MaskOrigin::BorderBox,
      clip: MaskClip::BorderBox,
      mode: MaskMode::Alpha,
      composite: MaskComposite::Add,
    }],
    color: Rgba::BLACK,
    font_size: 16.0,
    root_font_size: 16.0,
    viewport: Some((viewport_w as f32, viewport_h as f32)),
    rects: MaskReferenceRects {
      border: bounds,
      padding: bounds,
      content: bounds,
    },
  };
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, viewport_w as f32, viewport_h as f32),
    color: Rgba::WHITE,
  }));
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

  let font_ctx = FontContext::new();
  let serial = DisplayListRenderer::new(viewport_w, viewport_h, Rgba::WHITE, font_ctx.clone())
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
    DisplayListRenderer::new(viewport_w, viewport_h, Rgba::WHITE, font_ctx)
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("parallel paint")
  });

  assert!(report.parallel_used, "expected tiling to be used");
  assert_pixmap_eq(&serial, &report.pixmap);
}

#[test]
fn stacking_context_filter_radii_match_serial_output_under_tiling() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 128.0, 64.0),
    color: Rgba::WHITE,
  }));

  let bounds = Rect::from_xywh(70.0, 10.0, 20.0, 20.0);
  let stacking = StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds,
    plane_rect: bounds,
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: None,
    child_perspective: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: vec![ResolvedFilter::Brightness(1.0)],
    backdrop_filters: Vec::new(),
    radii: BorderRadii::uniform(6.0),
    mask: None,
  };
  list.push(DisplayItem::PushStackingContext(stacking));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: bounds,
    color: Rgba::new(40, 160, 220, 1.0),
  }));
  list.push(DisplayItem::PopStackingContext);

  let font_ctx = FontContext::new();
  let serial = DisplayListRenderer::new(128, 64, Rgba::WHITE, font_ctx.clone())
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
    DisplayListRenderer::new(128, 64, Rgba::WHITE, font_ctx)
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("parallel paint")
  });

  assert!(report.parallel_used, "expected tiling to be used");
  assert_eq!(serial.data(), report.pixmap.data());
}

#[test]
fn path_clips_survive_tiling() {
  let (width, height) = (192, 192);
  let mut list = DisplayList::new();
  // Add an invisible shadow solely to inflate the per-tile halo region. Tiny-skia's
  // path mask rasterizer can produce subtly different antialiasing coverage when a
  // polygon is clipped to the tile's mask bounds, so we make each tile render a
  // large enough region that the full clip path fits inside.
  list.push(DisplayItem::BoxShadow(BoxShadowItem {
    rect: Rect::from_xywh(0.0, 0.0, 8.0, 8.0),
    radii: BorderRadii::ZERO,
    offset: Point::new(0.0, 0.0),
    blur_radius: 20.0,
    spread_radius: 0.0,
    color: Rgba::new(0, 0, 0, 0.0),
    inset: false,
  }));
  let polygon = ResolvedClipPath::Polygon {
    points: vec![
      Point::new(72.0, 90.0),
      Point::new(148.0, 72.0),
      Point::new(150.0, 148.0),
      Point::new(90.0, 156.0),
    ],
    fill_rule: tiny_skia::FillRule::Winding,
  };
  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Path { path: polygon },
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, width as f32, height as f32),
    color: Rgba::rgb(20, 120, 200),
  }));
  list.push(DisplayItem::PopClip);

  let font_ctx = FontContext::new();
  let serial = DisplayListRenderer::new(width, height, Rgba::WHITE, font_ctx.clone())
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
    DisplayListRenderer::new(width, height, Rgba::WHITE, font_ctx)
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("parallel paint")
  });

  assert!(report.parallel_used, "expected tiling to be used");
  assert!(report.tiles > 1, "expected multiple tiles to be rendered");
  assert_rgba8888_pixels_eq(
    width,
    height,
    serial.data(),
    report.pixmap.data(),
    &format!("path clip tiling mismatch (tiles={})", report.tiles),
  );
}

#[test]
fn backdrop_filters_survive_tiling() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 64.0, 64.0),
    color: Rgba::rgb(255, 0, 0),
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(64.0, 0.0, 64.0, 64.0),
    color: Rgba::rgb(0, 0, 255),
  }));
  let stacking = StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(32.0, 0.0, 64.0, 64.0),
    plane_rect: Rect::from_xywh(32.0, 0.0, 64.0, 64.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: None,
    child_perspective: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: vec![ResolvedFilter::Blur(6.0)],
    radii: BorderRadii::ZERO,
    mask: None,
  };
  list.push(DisplayItem::PushStackingContext(stacking));
  list.push(DisplayItem::PopStackingContext);

  let font_ctx = FontContext::new();
  let serial = DisplayListRenderer::new(128, 64, Rgba::WHITE, font_ctx.clone())
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
    DisplayListRenderer::new(128, 64, Rgba::WHITE, font_ctx)
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("parallel paint")
  });

  assert!(
    report.parallel_used,
    "expected backdrop-filter rendering to use parallel painting (fallback={:?})",
    report.fallback_reason
  );
  assert!(report.tiles > 1, "expected multiple tiles to be rendered");
  assert_rgba8888_pixels_eq(
    128,
    64,
    serial.data(),
    report.pixmap.data(),
    &format!("backdrop filter tiling mismatch (tiles={})", report.tiles),
  );
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
  assert_pixmap_eq(&serial, &report.pixmap);
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
    origin: MaskOrigin::BorderBox,
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
    viewport: None,
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
fn parallel_paint_masked_element_matches_serial_off_origin_tiles() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 96.0, 32.0),
    color: Rgba::WHITE,
  }));

  let bounds = Rect::from_xywh(72.0, 8.0, 12.0, 12.0);
  let mut layer = MaskLayer::default();
  layer.repeat = BackgroundRepeat::no_repeat();
  layer.size = BackgroundSize::Explicit(
    BackgroundSizeComponent::Length(Length::percent(100.0)),
    BackgroundSizeComponent::Length(Length::percent(100.0)),
  );

  let image = ImageData::new_pixels(1, 1, vec![0, 0, 0, 255]);
  let mask = ResolvedMask {
    layers: vec![ResolvedMaskLayer {
      image: ResolvedMaskImage::Raster(image),
      repeat: layer.repeat,
      position: layer.position,
      size: layer.size,
      origin: MaskOrigin::BorderBox,
      clip: MaskClip::BorderBox,
      mode: MaskMode::Alpha,
      composite: MaskComposite::Add,
    }],
    color: Rgba::BLACK,
    font_size: 16.0,
    root_font_size: 16.0,
    viewport: None,
    rects: MaskReferenceRects {
      border: bounds,
      padding: bounds,
      content: bounds,
    },
  };

  let stacking = StackingContextItem {
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
  };

  list.push(DisplayItem::PushStackingContext(stacking));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: bounds,
    color: Rgba::new(0, 0, 255, 1.0),
  }));
  list.push(DisplayItem::PopStackingContext);

  let font_ctx = FontContext::new();
  let serial = DisplayListRenderer::new(96, 32, Rgba::WHITE, font_ctx.clone())
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
    DisplayListRenderer::new(96, 32, Rgba::WHITE, font_ctx)
      .unwrap()
      .with_parallelism(parallelism)
      .render_with_report(&list)
      .expect("parallel paint")
  });

  assert!(report.parallel_used, "expected tiling to be used");
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
