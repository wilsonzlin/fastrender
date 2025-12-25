use std::sync::Arc;

use fastrender::css::types::Transform;
use fastrender::geometry::Rect;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::style::types::BackfaceVisibility;
use fastrender::style::values::Length;
use fastrender::ComputedStyle;
use fastrender::FragmentNode;
use fastrender::Rgba;

#[test]
fn backface_hidden_fragments_are_not_painted() {
  let mut style = ComputedStyle::default();
  style.backface_visibility = BackfaceVisibility::Hidden;
  style.transform.push(Transform::RotateY(180.0));
  style.background_color = Rgba::RED;
  style.border_top_width = Length::px(0.0);
  style.border_right_width = Length::px(0.0);
  style.border_bottom_width = Length::px(0.0);
  style.border_left_width = Length::px(0.0);

  let fragment = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    vec![],
    Arc::new(style),
  );

  let list = DisplayListBuilder::new().build(&fragment);

  assert!(
    list.is_empty(),
    "backface-hidden fragments facing away should emit no display items"
  );
}
