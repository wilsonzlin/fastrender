use fastrender::geometry::Rect;
use fastrender::paint::display_list::{
  BlendMode, BorderRadii, ClipItem, ClipShape, DisplayItem, DisplayList, FillRectItem, StackingContextItem, Transform3D,
};
use fastrender::paint::scene_tree::{build_stacking_tree, collect_scene_items, NodeChild, SceneItemContent};
use fastrender::style::color::Rgba;
use fastrender::style::types::{BackfaceVisibility, TransformStyle};

fn stacking_context(
  transform_style: TransformStyle,
  transform: Option<Transform3D>,
  backface_visibility: BackfaceVisibility,
  mix_blend_mode: BlendMode,
) -> StackingContextItem {
  StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 0.0, 0.0),
    plane_rect: Rect::from_xywh(0.0, 0.0, 0.0, 0.0),
    mix_blend_mode,
    is_isolated: false,
    transform,
    transform_style,
    backface_visibility,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }
}

#[test]
fn stacking_tree_preserves_runs_and_children() {
  let root_sc = stacking_context(
    TransformStyle::Flat,
    None,
    BackfaceVisibility::Visible,
    BlendMode::Normal,
  );
  let child_sc = stacking_context(
    TransformStyle::Flat,
    None,
    BackfaceVisibility::Hidden,
    BlendMode::Normal,
  );

  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(root_sc));
  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Rect {
      rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      radii: None,
    },
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopClip);
  list.push(DisplayItem::PushStackingContext(child_sc));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(2.0, 2.0, 3.0, 3.0),
    color: Rgba::BLUE,
  }));
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);

  let tree = build_stacking_tree(&list);
  assert_eq!(tree.len(), 1);

  let root = match &tree[0] {
    NodeChild::ChildContext(node) => node,
    _ => panic!("expected root stacking context"),
  };
  assert_eq!(root.children.len(), 2);

  match &root.children[0] {
    NodeChild::PrimitiveRun(run) => match run.as_slice() {
      [DisplayItem::PushClip(_), DisplayItem::FillRect(_), DisplayItem::PopClip] => {}
      other => panic!("unexpected root run order: {:?}", other),
    },
    _ => panic!("expected primitive run"),
  }

  match &root.children[1] {
    NodeChild::ChildContext(child) => {
      assert_eq!(child.children.len(), 1);
      match &child.children[0] {
        NodeChild::PrimitiveRun(run) => {
          assert_eq!(run.len(), 1);
          assert!(matches!(run[0], DisplayItem::FillRect(_)));
        }
        _ => panic!("expected primitive run inside child"),
      }
    }
    _ => panic!("expected child stacking context"),
  }
}

#[test]
fn collect_scene_items_inlines_preserve_3d_and_flattens_flat() {
  let root_sc = stacking_context(
    TransformStyle::Preserve3d,
    Some(Transform3D::translate(10.0, 0.0, 0.0)),
    BackfaceVisibility::Visible,
    BlendMode::Normal,
  );
  let preserve_child = stacking_context(
    TransformStyle::Preserve3d,
    Some(Transform3D::translate(5.0, 0.0, 0.0)),
    BackfaceVisibility::Hidden,
    BlendMode::Normal,
  );
  let preserve_grandchild = stacking_context(
    TransformStyle::Preserve3d,
    Some(Transform3D::translate(2.0, 0.0, 0.0)),
    BackfaceVisibility::Visible,
    BlendMode::Normal,
  );
  let flat_child = stacking_context(
    TransformStyle::Flat,
    Some(Transform3D::translate(3.0, 0.0, 0.0)),
    BackfaceVisibility::Hidden,
    BlendMode::Normal,
  );

  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(root_sc));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 1.0, 1.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PushStackingContext(preserve_child));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(1.0, 0.0, 1.0, 1.0),
    color: Rgba::GREEN,
  }));
  list.push(DisplayItem::PushStackingContext(preserve_grandchild));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(2.0, 0.0, 1.0, 1.0),
    color: Rgba::BLUE,
  }));
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PushStackingContext(flat_child));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(3.0, 0.0, 1.0, 1.0),
    color: Rgba::BLACK,
  }));
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);

  let tree = build_stacking_tree(&list);
  let root = match &tree[0] {
    NodeChild::ChildContext(node) => node,
    _ => panic!("expected root stacking context"),
  };

  let scene = collect_scene_items(root);
  assert_eq!(scene.len(), 4);

  assert_eq!(scene[0].paint_order, 0);
  assert_eq!(scene[0].backface_visibility, BackfaceVisibility::Visible);
  assert!(matches!(
    scene[0].content,
    SceneItemContent::PrimitiveRun(ref run) if matches!(run.as_slice(), [DisplayItem::FillRect(_)])
  ));
  assert_eq!(scene[0].accumulated_transform.m[12], 10.0);

  assert_eq!(scene[1].paint_order, 1);
  assert_eq!(scene[1].backface_visibility, BackfaceVisibility::Hidden);
  assert!(matches!(
    scene[1].content,
    SceneItemContent::PrimitiveRun(ref run) if matches!(run.as_slice(), [DisplayItem::FillRect(_)])
  ));
  assert_eq!(scene[1].accumulated_transform.m[12], 15.0);

  assert_eq!(scene[2].paint_order, 2);
  assert_eq!(scene[2].backface_visibility, BackfaceVisibility::Visible);
  assert!(matches!(
    scene[2].content,
    SceneItemContent::PrimitiveRun(ref run) if matches!(run.as_slice(), [DisplayItem::FillRect(_)])
  ));
  assert_eq!(scene[2].accumulated_transform.m[12], 17.0);

  assert_eq!(scene[3].paint_order, 3);
  assert_eq!(scene[3].backface_visibility, BackfaceVisibility::Hidden);
  match &scene[3].content {
    SceneItemContent::FlattenedSubtree(node) => {
      assert_eq!(node.item.transform_style, TransformStyle::Flat);
    }
    _ => panic!("expected flattened subtree for flat child"),
  }
  assert_eq!(scene[3].accumulated_transform.m[12], 13.0);
}
