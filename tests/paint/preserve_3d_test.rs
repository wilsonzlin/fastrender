use super::util::bounding_box_for_color;
use fastrender::debug::runtime::{with_thread_runtime_toggles, RuntimeToggles};
use fastrender::paint::display_list::{
  BlendMode, BorderRadii, DisplayItem, DisplayList, FillRectItem, StackingContextItem, Transform3D,
};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Rgba;
use fastrender::style::types::{BackfaceVisibility, TransformStyle};
use fastrender::text::font_loader::FontContext;
use fastrender::Rect;
use std::collections::HashMap;
use std::sync::Arc;

#[test]
fn preserve3d_degenerate_transform_renders() {
  let mut list = DisplayList::new();

  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    plane_rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
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
  }));

  let mut degenerate = Transform3D::IDENTITY;
  // Zero out w to force degenerate projection; compositor should flatten gracefully.
  degenerate.m[15] = 0.0;

  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 1,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(1.0, 1.0, 8.0, 8.0),
    plane_rect: Rect::from_xywh(1.0, 1.0, 8.0, 8.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: Some(degenerate),
    child_perspective: None,
    transform_style: TransformStyle::Preserve3d,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));

  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(1.0, 1.0, 8.0, 8.0),
    color: Rgba::BLACK,
  }));

  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);

  let pixmap = DisplayListRenderer::new(16, 16, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();
  assert!(
    bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0
      && r < 200
      && g < 200
      && b < 200)
    .is_some(),
    "expected degenerate preserve-3d scene to paint something"
  );
}

fn scanline_width_variation<F>(pixmap: &tiny_skia::Pixmap, predicate: F) -> u32
where
  F: Copy + Fn((u8, u8, u8, u8)) -> bool,
{
  let Some((min_x, min_y, max_x, max_y)) = bounding_box_for_color(pixmap, predicate) else {
    return 0;
  };

  let mut min_width = u32::MAX;
  let mut max_width = 0u32;
  for y in min_y..=max_y {
    let mut row_min = u32::MAX;
    let mut row_max = 0u32;
    let mut seen = false;
    for x in min_x..=max_x {
      let p = pixmap.pixel(x, y).unwrap();
      let rgba = (p.red(), p.green(), p.blue(), p.alpha());
      if predicate(rgba) {
        seen = true;
        row_min = row_min.min(x);
        row_max = row_max.max(x);
      }
    }
    if !seen {
      continue;
    }
    let width = row_max.saturating_sub(row_min).saturating_add(1);
    min_width = min_width.min(width);
    max_width = max_width.max(width);
  }

  if min_width == u32::MAX {
    0
  } else {
    max_width.saturating_sub(min_width)
  }
}

#[test]
fn preserve3d_disable_warp_toggle_forces_affine_approximation() {
  fn render_with_toggles(list: &DisplayList, toggles: Arc<RuntimeToggles>) -> tiny_skia::Pixmap {
    with_thread_runtime_toggles(toggles, || {
      DisplayListRenderer::new(120, 120, Rgba::WHITE, FontContext::new())
        .unwrap()
        .render(list)
        .unwrap()
    })
  }

  fn toggles(disable_warp: bool) -> Arc<RuntimeToggles> {
    // Install the preserve-3d toggles as thread-local runtime config so we don't mutate process
    // environment variables (tests in this crate execute in parallel).
    let mut raw = HashMap::new();
    raw.insert("FASTR_PRESERVE3D_WARP".to_string(), "1".to_string());
    raw.insert(
      "FASTR_PRESERVE3D_DISABLE_WARP".to_string(),
      if disable_warp { "1" } else { "0" }.to_string(),
    );
    Arc::new(RuntimeToggles::from_map(raw))
  }

  let plane = Rect::from_xywh(10.0, 10.0, 40.0, 40.0);
  let perspective = Transform3D::perspective(50.0);
  let center = (
    plane.x() + plane.width() * 0.5,
    plane.y() + plane.height() * 0.5,
  );
  let rotate = Transform3D::translate(center.0, center.1, 0.0)
    .multiply(&Transform3D::rotate_x(60_f32.to_radians()))
    .multiply(&Transform3D::translate(-center.0, -center.1, 0.0));

  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 120.0, 120.0),
    plane_rect: Rect::from_xywh(0.0, 0.0, 120.0, 120.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: None,
    child_perspective: Some(perspective),
    transform_style: TransformStyle::Preserve3d,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 1,
    creates_stacking_context: true,
    bounds: plane,
    plane_rect: plane,
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: Some(rotate),
    child_perspective: None,
    transform_style: TransformStyle::Preserve3d,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: plane,
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);

  let predicate = |(r, g, b, a): (u8, u8, u8, u8)| a > 0 && r > 200 && g < 250 && b < 250;

  let warped = render_with_toggles(&list, toggles(false));
  let warped_variation = scanline_width_variation(&warped, predicate);
  assert!(
    warped_variation >= 8,
    "expected projective warp to create a trapezoid-like shape (scanline width variation {warped_variation})"
  );

  let affine = render_with_toggles(&list, toggles(true));
  let affine_variation = scanline_width_variation(&affine, predicate);
  assert!(
    affine_variation <= 4,
    "expected affine approximation to be roughly parallelogram-like (scanline width variation {affine_variation})"
  );
}
