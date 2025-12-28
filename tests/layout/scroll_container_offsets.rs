use std::collections::HashMap;
use std::sync::Arc;

use fastrender::api::RenderOptions;
use fastrender::geometry::{Point, Rect, Size};
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::paint::painter::paint_tree_display_list_with_resources_scaled_offset;
use fastrender::scroll::ScrollState;
use fastrender::style::color::Rgba;
use fastrender::style::types::Overflow;
use fastrender::style::ComputedStyle;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::box_tree::BoxNode;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use fastrender::{FastRender, Length, Position};

fn box_id_by_element_id(node: &BoxNode, target_id: &str) -> Option<usize> {
  if let Some(debug) = node.debug_info.as_ref() {
    if debug.id.as_deref() == Some(target_id) {
      return Some(node.id);
    }
  }
  node
    .children
    .iter()
    .find_map(|child| box_id_by_element_id(child, target_id))
}

#[test]
fn element_scroll_translates_descendants() {
  let mut scroller_style = ComputedStyle::default();
  scroller_style.overflow_x = Overflow::Scroll;
  scroller_style.overflow_y = Overflow::Scroll;
  let scroller_style = Arc::new(scroller_style);

  let mut red = ComputedStyle::default();
  red.background_color = Rgba::rgb(255, 0, 0);
  let red = Arc::new(red);
  let mut green = ComputedStyle::default();
  green.background_color = Rgba::rgb(0, 255, 0);
  let green = Arc::new(green);

  let red_block = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 0.0, 50.0, 50.0),
    FragmentContent::Block { box_id: None },
    vec![],
    red,
  );
  let green_block = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 50.0, 50.0, 50.0),
    FragmentContent::Block { box_id: None },
    vec![],
    green,
  );

  let scroller = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 0.0, 50.0, 50.0),
    FragmentContent::Block { box_id: Some(1) },
    vec![red_block, green_block],
    scroller_style,
  );
  let root = FragmentNode::new(
    Rect::from_xywh(0.0, 0.0, 50.0, 50.0),
    FragmentContent::Block { box_id: None },
    vec![scroller],
  );
  let mut tree = FragmentTree::with_viewport(root, Size::new(50.0, 50.0));

  let scroll_state = ScrollState::from_parts(
    Point::ZERO,
    HashMap::from([(1usize, Point::new(0.0, 50.0))]),
  );

  fastrender::scroll::apply_scroll_offsets(&mut tree, &scroll_state);

  let pixmap = paint_tree_display_list_with_resources_scaled_offset(
    &tree,
    50,
    50,
    Rgba::WHITE,
    FontContext::new(),
    fastrender::image_loader::ImageCache::new(),
    1.0,
    Point::ZERO,
    PaintParallelism::default(),
  )
  .expect("paint scrolled fragment tree");

  let pixel = pixmap.pixel(10, 10).expect("pixel in viewport");
  assert_eq!(
    (pixel.red(), pixel.green(), pixel.blue(), pixel.alpha()),
    (0, 255, 0, 255),
    "element scroll offset should reveal the second child"
  );
}

#[test]
fn sticky_offsets_use_element_scroll_containers() {
  let mut sticky_style = ComputedStyle::default();
  sticky_style.position = Position::Sticky;
  sticky_style.top = Some(Length::px(0.0));
  let sticky_style = Arc::new(sticky_style);

  let mut scroller_style = ComputedStyle::default();
  scroller_style.overflow_y = Overflow::Scroll;
  let scroller_style = Arc::new(scroller_style);

  let sticky = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 0.0, 100.0, 20.0),
    FragmentContent::Block { box_id: None },
    vec![],
    sticky_style,
  );
  let filler = FragmentNode::new_block(Rect::from_xywh(0.0, 100.0, 100.0, 200.0), vec![]);
  let scroller = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
    FragmentContent::Block { box_id: Some(1) },
    vec![sticky, filler],
    scroller_style,
  );
  let root = FragmentNode::new(
    Rect::from_xywh(0.0, 0.0, 100.0, 200.0),
    FragmentContent::Block { box_id: None },
    vec![scroller],
  );
  let mut tree = FragmentTree::with_viewport(root, Size::new(100.0, 100.0));

  let scroll_state = ScrollState::from_parts(
    Point::ZERO,
    HashMap::from([(1usize, Point::new(0.0, 50.0))]),
  );

  FastRender::new()
    .expect("renderer")
    .apply_sticky_offsets_to_tree_with_scroll_state(&mut tree, &scroll_state);

  let scroller = &tree.root.children[0];
  let sticky = &scroller.children[0];
  assert!(
    (sticky.bounds.y() - 50.0).abs() < 0.01,
    "sticky element should be pinned using the element scroll offset"
  );
  assert!(
    (sticky.bounds.y() - scroll_state.element_offset(1).y).abs() < 0.01,
    "sticky offset should match the provided scroll position"
  );
  assert!(
    (scroller.children[1].bounds.y() - 100.0).abs() < 0.01,
    "non-sticky siblings should retain their original positions"
  );
}

#[test]
fn nested_scroller_offsets_flow_from_render_options() {
  let html = r#"
    <style>
      body { margin: 0; }
      #outer { width: 80px; height: 60px; overflow: scroll; background: white; }
      #spacer { height: 40px; background: rgb(0, 0, 255); }
      #inner { width: 80px; height: 60px; overflow: scroll; }
      #first { height: 60px; background: rgb(255, 0, 0); }
      #second { height: 60px; background: rgb(0, 255, 0); }
    </style>
    <div id="outer">
      <div id="spacer"></div>
      <div id="inner">
        <div id="first"></div>
        <div id="second"></div>
      </div>
    </div>
  "#;

  let mut renderer = FastRender::new().expect("renderer");
  let base_options = RenderOptions::new().with_viewport(80, 60);
  let prepared = renderer
    .prepare_html(html, base_options.clone())
    .expect("prepare html");
  let outer_id = box_id_by_element_id(&prepared.box_tree().root, "outer").expect("outer box id");
  let inner_id = box_id_by_element_id(&prepared.box_tree().root, "inner").expect("inner box id");

  let scrolls = HashMap::from([
    (outer_id, Point::new(0.0, 40.0)),
    (inner_id, Point::new(0.0, 60.0)),
  ]);

  let baseline = renderer
    .render_html_with_options(html, base_options.clone())
    .expect("baseline render");
  let base_pixel = baseline.pixel(5, 5).expect("baseline pixel");
  assert_eq!(
    (base_pixel.red(), base_pixel.green(), base_pixel.blue()),
    (0, 0, 255),
    "without element scroll offsets the spacer should cover the viewport"
  );

  let scrolled = renderer
    .render_html_with_options(
      html,
      base_options
        .clone()
        .with_element_scroll_offsets(scrolls.clone()),
    )
    .expect("scrolled render");
  let scrolled_pixel = scrolled.pixel(5, 5).expect("scrolled pixel");
  assert_eq!(
    (
      scrolled_pixel.red(),
      scrolled_pixel.green(),
      scrolled_pixel.blue()
    ),
    (0, 255, 0),
    "element scroll offsets should reveal the second inner block inside nested scrollers"
  );

  let prepared_scrolled = renderer
    .prepare_html(html, base_options.with_element_scroll_offsets(scrolls))
    .expect("prepare with scrolls");
  let prepared_pixmap = prepared_scrolled.paint_default().expect("paint default");
  let prepared_pixel = prepared_pixmap.pixel(5, 5).expect("prepared pixel");
  assert_eq!(
    (
      prepared_pixel.red(),
      prepared_pixel.green(),
      prepared_pixel.blue()
    ),
    (0, 255, 0),
    "prepared documents should retain element scroll offsets when painting"
  );
}

#[test]
fn sticky_in_scroller_honors_element_scroll_offsets() {
  let html = r#"
    <style>
      body { margin: 0; }
      #scroller { width: 80px; height: 50px; overflow: scroll; }
      #sticky { position: sticky; top: 0; height: 20px; background: rgb(255, 0, 0); }
      #filler { height: 100px; background: rgb(0, 255, 0); }
    </style>
    <div id="scroller">
      <div id="sticky"></div>
      <div id="filler"></div>
    </div>
  "#;

  let mut renderer = FastRender::new().expect("renderer");
  let options = RenderOptions::new().with_viewport(80, 50);
  let prepared = renderer
    .prepare_html(html, options.clone())
    .expect("prepare sticky html");
  let scroller_id =
    box_id_by_element_id(&prepared.box_tree().root, "scroller").expect("scroller box id");

  let scrolls = HashMap::from([(scroller_id, Point::new(0.0, 30.0))]);
  let pixmap = renderer
    .render_html_with_options(html, options.with_element_scroll_offsets(scrolls))
    .expect("render with scroll");

  let top_pixel = pixmap.pixel(5, 5).expect("top pixel");
  assert_eq!(
    (
      top_pixel.red(),
      top_pixel.green(),
      top_pixel.blue(),
      top_pixel.alpha()
    ),
    (255, 0, 0, 255),
    "sticky element should remain pinned when the scroll offset is supplied via render options"
  );

  let below_sticky = pixmap.pixel(5, 30).expect("below sticky pixel");
  assert_eq!(
    (
      below_sticky.red(),
      below_sticky.green(),
      below_sticky.blue(),
      below_sticky.alpha()
    ),
    (0, 255, 0, 255),
    "content below the sticky header should still scroll into view"
  );
}
