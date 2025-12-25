use std::sync::Arc;

use fastrender::{
  ComputedStyle, FastRender, FragmentContent, FragmentNode, FragmentTree, LengthOrAuto, Point,
  Position, Rect, Size,
};

#[test]
fn nested_sticky_descendants_use_updated_containing_rect() {
  let renderer = FastRender::new().expect("create renderer");

  let mut outer_style = ComputedStyle::default();
  outer_style.position = Position::Sticky;
  outer_style.top = LengthOrAuto::px(0.0);

  let mut inner_style = ComputedStyle::default();
  inner_style.position = Position::Sticky;
  inner_style.top = LengthOrAuto::px(5.0);

  let inner = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 10.0, 100.0, 20.0),
    FragmentContent::Block { box_id: None },
    vec![],
    Arc::new(inner_style),
  );

  let outer = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 100.0, 200.0, 50.0),
    FragmentContent::Block { box_id: None },
    vec![inner],
    Arc::new(outer_style),
  );

  let root = FragmentNode::new(
    Rect::from_xywh(0.0, 0.0, 200.0, 300.0),
    FragmentContent::Block { box_id: None },
    vec![outer],
  );

  let mut tree = FragmentTree::with_viewport(root, Size::new(200.0, 200.0));
  let scroll = Point::new(0.0, 120.0);

  renderer.apply_sticky_offsets_to_tree(&mut tree, scroll);

  let outer = &tree.root.children[0];
  assert!(
    (outer.bounds.y() - 120.0).abs() < 0.01,
    "outer sticky element should pin to the top of the viewport"
  );

  let inner = &outer.children[0];
  assert!(
    (inner.bounds.y() - 10.0).abs() < 0.01,
    "inner sticky element should retain its local offset after parent moves"
  );

  let inner_abs_y = outer.bounds.y() + inner.bounds.y();
  assert!(
    (inner_abs_y - 130.0).abs() < 0.01,
    "inner absolute position should incorporate the parent's sticky offset"
  );
  assert!(
    (inner_abs_y - scroll.y - 10.0).abs() < 0.01,
    "inner should remain 10px from the viewport after scrolling when parent sticks"
  );
}
