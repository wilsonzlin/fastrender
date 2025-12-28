use std::time::Duration;

use fastrender::{
  FastRender, FragmentContent, FragmentNode, Point, PreparedPaintOptions, RenderOptions, Result,
};
use fastrender::style::types::Overflow;
use fastrender::scroll::ScrollState;

fn pixel(pixmap: &fastrender::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let px = pixmap.pixel(x, y).unwrap();
  (px.red(), px.green(), px.blue(), px.alpha())
}

fn fragment_box_id(node: &FragmentNode) -> Option<usize> {
  match &node.content {
    FragmentContent::Block { box_id }
    | FragmentContent::Inline { box_id, .. }
    | FragmentContent::Text { box_id, .. }
    | FragmentContent::Replaced { box_id, .. } => *box_id,
    FragmentContent::RunningAnchor { .. } | FragmentContent::Line { .. } => None,
  }
}

fn find_scroll_container_id(node: &FragmentNode) -> Option<usize> {
  let is_scroll_container = node
    .style
    .as_ref()
    .map(|style| {
      matches!(style.overflow_x, Overflow::Scroll | Overflow::Auto)
        || matches!(style.overflow_y, Overflow::Scroll | Overflow::Auto)
    })
    .unwrap_or(false);
  if is_scroll_container {
    if let Some(id) = fragment_box_id(node) {
      return Some(id);
    }
  }
  for child in &node.children {
    if let Some(id) = find_scroll_container_id(child) {
      return Some(id);
    }
  }
  None
}

#[test]
fn repaint_with_different_scroll_offsets_changes_pixels() -> Result<()> {
  let mut renderer = FastRender::new()?;
  let html = r#"
    <style>
      body { margin: 0; }
      .section { width: 100px; height: 100px; }
      .top { background: rgb(255, 0, 0); }
      .bottom { background: rgb(0, 0, 255); }
    </style>
    <div class="section top"></div>
    <div class="section bottom"></div>
  "#;
  let prepared = renderer.prepare_html(html, RenderOptions::new().with_viewport(100, 100))?;

  let top_first = prepared.paint_with_options(PreparedPaintOptions::new().with_scroll(0.0, 0.0))?;
  let bottom = prepared.paint_with_options(PreparedPaintOptions::new().with_scroll(0.0, 100.0))?;
  let top_second =
    prepared.paint_with_options(PreparedPaintOptions::new().with_scroll(0.0, 0.0))?;

  assert_ne!(pixel(&top_first, 50, 50), pixel(&bottom, 50, 50));
  assert_eq!(top_first.data(), top_second.data());
  Ok(())
}

#[test]
fn repaint_with_different_animation_times_changes_pixels() -> Result<()> {
  let mut renderer = FastRender::new()?;
  let html = r#"
    <style>
      body { margin: 0; }
      .box { width: 100px; height: 100px; animation-name: fade; }
      @keyframes fade {
        from { background-color: rgb(255, 0, 0); }
        to { background-color: rgb(0, 255, 0); }
      }
    </style>
    <div class="box"></div>
  "#;
  let prepared = renderer.prepare_html(html, RenderOptions::new().with_viewport(100, 100))?;

  let early =
    prepared.paint_with_options(PreparedPaintOptions::new().with_animation_time(Duration::ZERO))?;
  let later = prepared.paint_with_options(
    PreparedPaintOptions::new().with_animation_time(Duration::from_millis(800)),
  )?;
  let repeat =
    prepared.paint_with_options(PreparedPaintOptions::new().with_animation_time(Duration::ZERO))?;

  assert_ne!(pixel(&early, 50, 50), pixel(&later, 50, 50));
  assert_eq!(early.data(), repeat.data());
  Ok(())
}

#[test]
fn repaint_with_element_scroll_offsets_changes_pixels() -> Result<()> {
  let mut renderer = FastRender::new()?;
  let html = r#"
    <style>
      body { margin: 0; }
      .scroller { width: 100px; height: 100px; overflow: scroll; }
      .section { width: 100px; height: 100px; }
      .top { background: rgb(255, 0, 0); }
      .bottom { background: rgb(0, 0, 255); }
    </style>
    <div class="scroller">
      <div class="section top"></div>
      <div class="section bottom"></div>
    </div>
  "#;
  let prepared = renderer.prepare_html(html, RenderOptions::new().with_viewport(100, 100))?;

  let scroller_id = find_scroll_container_id(&prepared.fragment_tree().root)
    .expect("scroll container fragment with overflow");

  let base = prepared.paint_with_options(
    PreparedPaintOptions::new().with_scroll_state(ScrollState::with_viewport(Point::ZERO)),
  )?;

  let mut scrolled_state = ScrollState::with_viewport(Point::ZERO);
  scrolled_state.elements.insert(scroller_id, Point::new(0.0, 100.0));
  let scrolled = prepared.paint_with_options(
    PreparedPaintOptions::new().with_scroll_state(scrolled_state.clone()),
  )?;
  let scrolled_repeat = prepared.paint_with_options(
    PreparedPaintOptions::new().with_scroll_state(scrolled_state),
  )?;
  let base_repeat = prepared.paint_with_options(
    PreparedPaintOptions::new().with_scroll_state(ScrollState::with_viewport(Point::ZERO)),
  )?;

  assert_ne!(pixel(&base, 50, 50), pixel(&scrolled, 50, 50));
  assert_eq!(base.data(), base_repeat.data());
  assert_eq!(scrolled.data(), scrolled_repeat.data());
  Ok(())
}
