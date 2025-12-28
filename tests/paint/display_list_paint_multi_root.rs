use fastrender::geometry::{Point, Rect};
use fastrender::image_loader::ImageCache;
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::paint::painter::paint_tree_display_list_with_resources_scaled_offset;
use fastrender::scroll::ScrollState;
use fastrender::style::color::Rgba;
use fastrender::style::display::Display;
use fastrender::style::ComputedStyle;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::fragment_tree::{FragmentNode, FragmentTree};
use fastrender::Pixmap;
use std::sync::Arc;

fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let p = pixmap.pixel(x, y).expect("pixel in bounds");
  (p.red(), p.green(), p.blue(), p.alpha())
}

#[test]
fn display_list_paints_all_fragment_roots_with_offset() {
  let mut red_style = ComputedStyle::default();
  red_style.display = Display::Block;
  red_style.background_color = Rgba::RED;
  let red_style = Arc::new(red_style);

  let mut blue_style = ComputedStyle::default();
  blue_style.display = Display::Block;
  blue_style.background_color = Rgba::BLUE;
  let blue_style = Arc::new(blue_style);

  let first_root =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], red_style);
  let second_root =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 20.0, 10.0, 10.0), vec![], blue_style);

  let mut tree = FragmentTree::new(first_root);
  tree.additional_fragments.push(second_root);

  let offset = Point::new(3.0, 4.0);
  let bounds = tree.content_size();
  let width = ((bounds.max_x() + offset.x + 1.0).ceil()).max(1.0) as u32;
  let height = ((bounds.max_y() + offset.y + 1.0).ceil()).max(1.0) as u32;

  let pixmap = paint_tree_display_list_with_resources_scaled_offset(
    &tree,
    width,
    height,
    Rgba::WHITE,
    FontContext::new(),
    ImageCache::new(),
    1.0,
    offset,
    PaintParallelism::default(),
    &ScrollState::default(),
  )
  .expect("paint");

  // Content should be translated by `offset`, so the origin remains background.
  let origin = pixel(&pixmap, 1, 1);
  assert_eq!(origin, (255, 255, 255, 255));

  let red_sample = pixel(&pixmap, (offset.x + 2.0) as u32, (offset.y + 2.0) as u32);
  assert_eq!(red_sample, (255, 0, 0, 255));

  // The second fragment root should also render and respect the offset.
  let blue_sample = pixel(&pixmap, (offset.x + 2.0) as u32, (offset.y + 22.0) as u32);
  assert_eq!(blue_sample, (0, 0, 255, 255));

  // Without the offset, this position would have landed inside the second root.
  let unshifted_blue = pixel(&pixmap, 1, 22);
  assert_eq!(unshifted_blue, (255, 255, 255, 255));
}
