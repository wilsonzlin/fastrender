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
fn marker_text_shadow_in_vertical_writing() {
  let mut style = ComputedStyle::default();
  style.color = Rgba::BLACK;
  style.font_size = 16.0;
  style.text_shadow = vec![TextShadow {
    offset_x: fastrender::style::values::Length::px(0.0),
    offset_y: fastrender::style::values::Length::px(4.0),
    blur_radius: fastrender::style::values::Length::px(0.0),
    color: Some(Rgba::from_rgba8(255, 0, 0, 255)),
  }]
  .into();
  let style = Arc::new(style);

  let marker = FragmentNode::new_with_style(
    Rect::from_xywh(10.0, 10.0, 10.0, 10.0),
    FragmentContent::Text {
      text: "•".to_string().into(),
      box_id: None,
      baseline_offset: 10.0,
      shaped: None,
      is_marker: true,
    },
    vec![],
    style,
  );
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 30.0, 40.0), vec![marker]);
  let tree = FragmentTree::new(root);

  let pixmap = paint_tree(&tree, 40, 60, Rgba::WHITE).expect("paint");

  let glyph_bbox =
    bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32)
      .expect("marker glyph");
  let shadow_bbox =
    bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80)
      .expect("marker shadow");

  let dy = shadow_bbox.1 as i32 - glyph_bbox.1 as i32;
  assert!(
    (3..=5).contains(&dy),
    "vertical shadow should offset downward by ~4px (dy={})",
    dy
  );
}
