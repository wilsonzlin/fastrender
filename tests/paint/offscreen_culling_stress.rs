use fastrender::css::types::BoxShadow;
use fastrender::geometry::{Point, Rect};
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::paint::display_list_renderer::{DisplayListRenderer, PaintParallelism};
use fastrender::style::values::Length;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::fragment_tree::FragmentNode;
use fastrender::{ComputedStyle, DisplayListOptimizer, Rgba};
use std::sync::Arc;
use std::time::{Duration, Instant};

#[test]
fn offscreen_fragments_are_culled_fast() {
  let viewport = Rect::from_xywh(0.0, 0.0, 256.0, 256.0);

  let mut shared_style = ComputedStyle::default();
  shared_style.background_color = Rgba::new(40, 140, 220, 1.0);
  shared_style.box_shadow = vec![BoxShadow {
    offset_x: Length::px(3.0),
    offset_y: Length::px(3.0),
    blur_radius: Length::px(6.0),
    spread_radius: Length::px(2.0),
    color: Rgba::new(0, 0, 0, 0.3),
    inset: false,
  }];
  let shared_style = Arc::new(shared_style);

  let mut children = Vec::new();
  let visible_rows = 5;
  let visible_cols = 5;
  for y in 0..visible_rows {
    for x in 0..visible_cols {
      let origin = Point::new(12.0 + x as f32 * 48.0, 12.0 + y as f32 * 48.0);
      children.push(FragmentNode::new_block_styled(
        Rect::from_xywh(origin.x, origin.y, 32.0, 32.0),
        vec![],
        Arc::clone(&shared_style),
      ));
    }
  }

  let offscreen = 2500;
  for i in 0..offscreen {
    let origin = Point::new(
      800.0 + (i % 60) as f32 * 40.0,
      800.0 + (i / 60) as f32 * 40.0,
    );
    children.push(FragmentNode::new_block_styled(
      Rect::from_xywh(origin.x, origin.y, 32.0, 32.0),
      vec![],
      Arc::clone(&shared_style),
    ));
  }
  let total_fragments = children.len();

  let mut root_style = ComputedStyle::default();
  root_style.background_color = Rgba::WHITE;
  let root = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 2048.0, 2048.0),
    children,
    Arc::new(root_style.clone()),
  );

  let list_full = DisplayListBuilder::new()
    .with_viewport_size(viewport.width(), viewport.height())
    .build_checked(&root)
    .expect("display list build");
  assert!(
    list_full.len() < total_fragments / 4,
    "expected heavy culling during build ({} items from {} fragments)",
    list_full.len(),
    total_fragments
  );

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer
    .optimize_checked(&list_full, viewport)
    .expect("optimize");
  assert!(
    optimized.len() <= list_full.len(),
    "optimizer should not grow the display list"
  );
  assert!(
    stats.original_count == list_full.len() && stats.final_count == optimized.len(),
    "stats should reflect optimized list counts"
  );

  let mut visible_children = Vec::new();
  for y in 0..visible_rows {
    for x in 0..visible_cols {
      let origin = Point::new(12.0 + x as f32 * 48.0, 12.0 + y as f32 * 48.0);
      visible_children.push(FragmentNode::new_block_styled(
        Rect::from_xywh(origin.x, origin.y, 32.0, 32.0),
        vec![],
        Arc::clone(&shared_style),
      ));
    }
  }
  let visible_root = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, viewport.width(), viewport.height()),
    visible_children,
    Arc::new(root_style),
  );
  let visible_list = DisplayListBuilder::new()
    .with_viewport_size(viewport.width(), viewport.height())
    .build_checked(&visible_root)
    .expect("visible list build");
  let (visible_opt, _) = optimizer
    .optimize_checked(&visible_list, viewport)
    .expect("visible optimize");

  let font_ctx = FontContext::new();
  let render_start = Instant::now();
  let report = DisplayListRenderer::new(
    viewport.width() as u32,
    viewport.height() as u32,
    Rgba::WHITE,
    font_ctx.clone(),
  )
  .expect("renderer")
  .with_parallelism(PaintParallelism::adaptive())
  .render_with_report(&optimized)
  .expect("render");
  let max_duration = Duration::from_millis(900);
  assert!(
    report.duration < max_duration,
    "raster time should stay bounded (got {:?})",
    report.duration
  );

  let expected = DisplayListRenderer::new(
    viewport.width() as u32,
    viewport.height() as u32,
    Rgba::WHITE,
    font_ctx,
  )
  .expect("expected renderer")
  .render(&visible_opt)
  .expect("expected render");

  assert_eq!(
    expected.data(),
    report.pixmap.data(),
    "culled render should match visible-only output"
  );

  // Make sure the whole pipeline ran within a reasonable deadline so regressions stay visible.
  assert!(
    render_start.elapsed() < Duration::from_secs(2),
    "stress render should not approach timeout budgets"
  );
}
