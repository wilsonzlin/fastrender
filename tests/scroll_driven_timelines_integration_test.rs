use fastrender::{
  FastRender, FragmentNode, FragmentTree, Point, PreparedPaintOptions, Rect, RenderOptions, Result,
  Rgba,
};

fn pixel(pixmap: &fastrender::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let px = pixmap.pixel(x, y).unwrap();
  (px.red(), px.green(), px.blue(), px.alpha())
}

fn find_view_timeline_rect(tree: &FragmentTree, name: &str) -> Option<Rect> {
  fn rec(node: &FragmentNode, origin: Point, name: &str) -> Option<Rect> {
    if let Some(style) = node.style.as_ref() {
      if style
        .view_timelines
        .iter()
        .any(|tl| tl.name.as_deref() == Some(name))
      {
        return Some(Rect::from_xywh(
          origin.x,
          origin.y,
          node.bounds.width(),
          node.bounds.height(),
        ));
      }
    }

    for child in node.children.iter() {
      let child_origin = Point::new(origin.x + child.bounds.x(), origin.y + child.bounds.y());
      if let Some(found) = rec(child, child_origin, name) {
        return Some(found);
      }
    }
    None
  }

  let root_origin = Point::new(tree.root.bounds.x(), tree.root.bounds.y());
  if let Some(found) = rec(&tree.root, root_origin, name) {
    return Some(found);
  }
  for frag in tree.additional_fragments.iter() {
    let origin = Point::new(frag.bounds.x(), frag.bounds.y());
    if let Some(found) = rec(frag, origin, name) {
      return Some(found);
    }
  }
  None
}

fn sample_point_in_rect(
  rect: Rect,
  scroll_x: f32,
  scroll_y: f32,
  viewport_width: u32,
  viewport_height: u32,
) -> (u32, u32) {
  let left = rect.x() - scroll_x;
  let top = rect.y() - scroll_y;
  let right = left + rect.width();
  let bottom = top + rect.height();

  let visible_left = left.max(0.0);
  let visible_top = top.max(0.0);
  let visible_right = right.min(viewport_width as f32);
  let visible_bottom = bottom.min(viewport_height as f32);

  assert!(
    visible_right > visible_left && visible_bottom > visible_top,
    "expected target rect to intersect viewport; rect={rect:?}, scroll=({scroll_x},{scroll_y}), viewport=({viewport_width},{viewport_height})",
  );

  let sample_x = ((visible_left + visible_right) * 0.5)
    .clamp(0.0, viewport_width.saturating_sub(1) as f32)
    .floor() as u32;
  let sample_y = ((visible_top + visible_bottom) * 0.5)
    .clamp(0.0, viewport_height.saturating_sub(1) as f32)
    .floor() as u32;
  (sample_x, sample_y)
}

#[test]
fn scroll_self_timeline_inactive_when_scroll_range_is_zero() -> Result<()> {
  let mut renderer = FastRender::new()?;
  let html = r#"
    <style>
      html, body { margin: 0; }
      .box {
        position: absolute;
        top: 0;
        width: 50px;
        height: 50px;
        background: rgb(255, 0, 0);
        overflow: auto;
        animation-timeline: scroll(self);
        animation-name: fade;
      }
      .scrollable { left: 0; }
      .not-scrollable { left: 60px; }
      .scrollable .inner { height: 120px; }
      .not-scrollable .inner { height: 20px; }
      @keyframes fade { from { opacity: 0; } to { opacity: 1; } }
    </style>
    <div class="box scrollable"><div class="inner"></div></div>
    <div class="box not-scrollable"><div class="inner"></div></div>
  "#;

  let prepared = renderer.prepare_html(html, RenderOptions::new().with_viewport(110, 50))?;
  let pixmap = prepared.paint_with_options(
    PreparedPaintOptions::new()
      .with_scroll(0.0, 0.0)
      .with_background(Rgba::new(0, 255, 0, 1.0)),
  )?;

  // With scroll range > 0, scroll(self) is active and at progress 0 so opacity is 0.
  assert_eq!(pixel(&pixmap, 25, 25), (0, 255, 0, 255));
  // With scroll range == 0, scroll(self) is inactive so the animation should not apply.
  assert_eq!(pixel(&pixmap, 85, 25), (255, 0, 0, 255));
  Ok(())
}

#[test]
fn view_timeline_animation_range_entry_length_offsets_map_to_scroll_positions() -> Result<()> {
  let viewport_width = 200;
  let viewport_height = 200;
  let mut renderer = FastRender::new()?;
  let html = r#"
    <style>
      html, body { margin: 0; background: rgb(0, 0, 0); }
      .spacer { height: 300px; }
      .target {
        width: 200px;
        height: 400px;
        background: rgb(255, 255, 255);
        view-timeline: --t block;
        animation-timeline: --t;
        animation-range: entry 100px entry 500px;
        animation-fill-mode: both;
        animation-name: fade;
      }
      @keyframes fade { from { opacity: 0; } to { opacity: 1; } }
    </style>
    <div class="spacer"></div>
    <div class="target"></div>
    <div class="spacer"></div>
  "#;

  let prepared = renderer.prepare_html(
    html,
    RenderOptions::new().with_viewport(viewport_width, viewport_height),
  )?;
  let target_rect = find_view_timeline_rect(prepared.fragment_tree(), "--t")
    .expect("target fragment with view-timeline");

  let entry = target_rect.y() - viewport_height as f32;
  let samples = [
    (entry + 100.0, 0.0_f32),
    (entry + 300.0, 0.5_f32),
    (entry + 500.0, 1.0_f32),
  ];

  for (scroll_y, expected_progress) in samples {
    let pixmap = prepared.paint_with_options(
      PreparedPaintOptions::new()
        .with_scroll(0.0, scroll_y)
        .with_background(Rgba::new(0, 0, 0, 1.0)),
    )?;
    let (sx, sy) =
      sample_point_in_rect(target_rect, 0.0, scroll_y, viewport_width, viewport_height);
    let (r, g, b, a) = pixel(&pixmap, sx, sy);
    assert_eq!(a, 255, "expected fully opaque output pixel");
    let expected = (expected_progress.clamp(0.0, 1.0) * 255.0).round() as i32;
    let tol = match expected_progress {
      p if (p - 0.0).abs() < f32::EPSILON || (p - 1.0).abs() < f32::EPSILON => 5,
      _ => 18,
    };
    for (chan, name) in [(r, "r"), (g, "g"), (b, "b")] {
      let diff = (chan as i32 - expected).abs();
      assert!(
        diff <= tol,
        "expected {name}≈{expected}±{tol} at scroll_y={scroll_y} (got {chan})",
      );
    }
  }

  Ok(())
}
