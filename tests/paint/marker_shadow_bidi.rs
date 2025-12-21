use super::util::bounding_box_for_color;
use fastrender::css::types::TextShadow;
use fastrender::geometry::Rect;
use fastrender::paint::painter::paint_tree;
use fastrender::style::color::Rgba;
use fastrender::style::ComputedStyle;
use fastrender::tree::fragment_tree::FragmentContent;
use fastrender::tree::fragment_tree::FragmentNode;
use fastrender::tree::fragment_tree::FragmentTree;
use std::sync::Arc;

#[test]
fn marker_shadow_respects_rtl_position() {
  let mut style = ComputedStyle::default();
  style.color = Rgba::BLACK;
  style.direction = fastrender::style::types::Direction::Rtl;
  style.text_shadow.push(TextShadow {
    offset_x: fastrender::style::values::Length::px(3.0),
    offset_y: fastrender::style::values::Length::px(0.0),
    blur_radius: fastrender::style::values::Length::px(0.0),
    color: Some(Rgba::from_rgba8(255, 0, 0, 255)),
  });
  let style = Arc::new(style);

  let marker = FragmentNode::new_with_style(
    Rect::from_xywh(20.0, 10.0, 10.0, 10.0),
    FragmentContent::Text {
      text: "•".to_string(),
      box_id: None,
      baseline_offset: 10.0,
      shaped: None,
      is_marker: true,
    },
    vec![],
    style,
  );
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 40.0, 30.0), vec![marker]);
  let tree = FragmentTree::new(root);

  let pixmap = paint_tree(&tree, 60, 40, Rgba::WHITE).expect("paint");

  let glyph_bbox =
    bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32)
      .expect("glyph");
  let shadow_bbox =
    bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80)
      .expect("shadow");

  let dx = shadow_bbox.0 as i32 - glyph_bbox.0 as i32;
  assert!(
    (2..=4).contains(&dx),
    "shadow should offset toward inline-start in RTL (dx={})",
    dx
  );
}
