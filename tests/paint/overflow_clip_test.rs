use fastrender::geometry::Rect;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::position::Position;
use fastrender::style::types::{ClipComponent, ClipRect, Overflow};
use fastrender::style::values::Length;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::fragment_tree::FragmentNode;
use fastrender::ComputedStyle;
use fastrender::Rgba;
use std::sync::Arc;

fn pixel(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let px = pixmap.pixel(x, y).expect("pixel inside viewport");
  (px.red(), px.green(), px.blue(), px.alpha())
}

#[test]
fn overflow_hidden_clips_non_stacking_children() {
  let mut parent_style = ComputedStyle::default();
  parent_style.overflow_x = Overflow::Hidden;
  parent_style.overflow_y = Overflow::Hidden;
  let parent_style = Arc::new(parent_style);

  let mut child_style = ComputedStyle::default();
  child_style.background_color = Rgba::RED;
  let child_style = Arc::new(child_style);

  let child =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 6.0, 4.0), vec![], child_style);

  let parent = FragmentNode::new_block_styled(
    Rect::from_xywh(2.0, 2.0, 4.0, 4.0),
    vec![child],
    parent_style,
  );

  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 8.0, 8.0), vec![parent]);

  let list = DisplayListBuilder::new().build_with_stacking_tree(&root);
  let pixmap = DisplayListRenderer::new(8, 8, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();

  // Red background from child is clipped to the parent's padding box.
  assert_eq!(pixel(&pixmap, 7, 3), (255, 255, 255, 255));
  assert_eq!(pixel(&pixmap, 3, 3), (255, 0, 0, 255));
}

#[test]
fn overflow_clip_respects_border_radii_for_children() {
  let mut parent_style = ComputedStyle::default();
  parent_style.overflow_x = Overflow::Hidden;
  parent_style.overflow_y = Overflow::Hidden;
  parent_style.border_top_left_radius = Length::px(2.0);
  parent_style.border_top_right_radius = Length::px(2.0);
  parent_style.border_bottom_right_radius = Length::px(2.0);
  parent_style.border_bottom_left_radius = Length::px(2.0);
  let parent_style = Arc::new(parent_style);

  let mut child_style = ComputedStyle::default();
  child_style.background_color = Rgba::RED;
  let child_style = Arc::new(child_style);

  let child =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 4.0, 4.0), vec![], child_style);
  let parent = FragmentNode::new_block_styled(
    Rect::from_xywh(1.0, 1.0, 4.0, 4.0),
    vec![child],
    parent_style,
  );
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 6.0, 6.0), vec![parent]);

  let list = DisplayListBuilder::new().build_with_stacking_tree(&root);
  let pixmap = DisplayListRenderer::new(6, 6, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();

  // Corner outside the rounded clip stays white while the center paints red.
  assert_ne!(pixel(&pixmap, 1, 1), (255, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 3, 3), (255, 0, 0, 255));
}

#[test]
fn clip_rect_clips_descendants_without_stacking_context() {
  let mut parent_style = ComputedStyle::default();
  parent_style.position = Position::Absolute;
  parent_style.clip = Some(ClipRect {
    top: ClipComponent::Length(Length::px(1.0)),
    right: ClipComponent::Length(Length::px(5.0)),
    bottom: ClipComponent::Length(Length::px(5.0)),
    left: ClipComponent::Length(Length::px(1.0)),
  });
  let parent_style = Arc::new(parent_style);

  let mut child_style = ComputedStyle::default();
  child_style.background_color = Rgba::RED;
  let child_style = Arc::new(child_style);

  let child =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 6.0, 6.0), vec![], child_style);
  let parent = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
    vec![child],
    parent_style,
  );
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 6.0, 6.0), vec![parent]);

  let list = DisplayListBuilder::new().build_with_stacking_tree(&root);
  let pixmap = DisplayListRenderer::new(6, 6, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();

  // Only the region inside the clip rect paints red.
  assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 255, 255));
  assert_eq!(pixel(&pixmap, 5, 2), (255, 255, 255, 255));
  assert_eq!(pixel(&pixmap, 2, 2), (255, 0, 0, 255));
}
