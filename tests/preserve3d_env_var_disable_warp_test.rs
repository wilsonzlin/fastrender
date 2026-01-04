use fastrender::debug::runtime::{with_runtime_toggles, RuntimeToggles};
use fastrender::paint::display_list::{
  BlendMode, BorderRadii, DisplayItem, DisplayList, FillRectItem, StackingContextItem, Transform3D,
};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Rgba;
use fastrender::style::types::{BackfaceVisibility, TransformStyle};
use fastrender::text::font_loader::FontContext;
use fastrender::Rect;
use std::ffi::{OsStr, OsString};
use std::sync::Arc;

struct EnvVarGuard {
  key: &'static str,
  previous: Option<OsString>,
}

impl EnvVarGuard {
  fn set(key: &'static str, value: impl AsRef<OsStr>) -> Self {
    let previous = std::env::var_os(key);
    std::env::set_var(key, value);
    Self { key, previous }
  }
}

impl Drop for EnvVarGuard {
  fn drop(&mut self) {
    match self.previous.take() {
      Some(value) => std::env::set_var(self.key, value),
      None => std::env::remove_var(self.key),
    }
  }
}

fn bounding_box_for_color<F>(
  pixmap: &tiny_skia::Pixmap,
  predicate: F,
) -> Option<(u32, u32, u32, u32)>
where
  F: Copy + Fn((u8, u8, u8, u8)) -> bool,
{
  let mut min_x = u32::MAX;
  let mut min_y = u32::MAX;
  let mut max_x = 0u32;
  let mut max_y = 0u32;
  let mut seen = false;

  for y in 0..pixmap.height() {
    for x in 0..pixmap.width() {
      let p = pixmap.pixel(x, y).unwrap();
      let rgba = (p.red(), p.green(), p.blue(), p.alpha());
      if predicate(rgba) {
        seen = true;
        min_x = min_x.min(x);
        min_y = min_y.min(y);
        max_x = max_x.max(x);
        max_y = max_y.max(y);
      }
    }
  }

  seen.then_some((min_x, min_y, max_x, max_y))
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
fn preserve3d_disable_warp_env_forces_affine_approximation() {
  // Force the warp path to be "available", then ensure the disable flag still wins. This matches
  // how the CLI/libraries consume FASTR_PRESERVE3D_* via RuntimeToggles sourced from env.
  let _warp_guard = EnvVarGuard::set("FASTR_PRESERVE3D_WARP", "1");
  let _disable_guard = EnvVarGuard::set("FASTR_PRESERVE3D_DISABLE_WARP", "1");
  let toggles = Arc::new(RuntimeToggles::from_env());

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
  let pixmap = with_runtime_toggles(toggles, || {
    DisplayListRenderer::new(120, 120, Rgba::WHITE, FontContext::new())
      .unwrap()
      .render(&list)
      .unwrap()
  });

  let variation = scanline_width_variation(&pixmap, predicate);
  assert!(
    variation <= 4,
    "expected disable_warp to force affine approximation (scanline width variation {variation})"
  );
}
