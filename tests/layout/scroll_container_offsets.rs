use std::collections::HashMap;
use std::sync::Arc;

use fastrender::geometry::{Point, Rect, Size};
use fastrender::paint::painter::paint_tree_display_list_with_resources_scaled_offset;
use fastrender::scroll::ScrollState;
use fastrender::style::color::Rgba;
use fastrender::style::types::Overflow;
use fastrender::style::ComputedStyle;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use fastrender::{FastRender, Length, Position};

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
  let tree = FragmentTree::with_viewport(root, Size::new(50.0, 50.0));

  let scroll_state = ScrollState::from_parts(
    Point::ZERO,
    HashMap::from([(1usize, Point::new(0.0, 50.0))]),
  );

  let pixmap = paint_tree_display_list_with_resources_scaled_offset(
    &tree,
    50,
    50,
    Rgba::WHITE,
    FontContext::new(),
    fastrender::image_loader::ImageCache::new(),
    1.0,
    Point::ZERO,
    &scroll_state,
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
