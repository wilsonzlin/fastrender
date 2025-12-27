use fastrender::paint::display_list::Transform3D;
use fastrender::paint::preserve3d::{cull_backfaces, depth_sort_scene, SceneItem};
use fastrender::style::types::BackfaceVisibility;

#[test]
fn rotated_y_backface_hidden_culls_plane() {
  let plane = SceneItem {
    item: "culled",
    accumulated_transform: Transform3D::rotate_y(std::f32::consts::PI),
    backface_visibility: BackfaceVisibility::Hidden,
  };

  assert!(cull_backfaces(vec![plane]).is_empty());
}

#[test]
fn rotated_y_backface_visible_keeps_plane() {
  let plane = SceneItem {
    item: "kept",
    accumulated_transform: Transform3D::rotate_y(std::f32::consts::PI),
    backface_visibility: BackfaceVisibility::Visible,
  };

  let kept = cull_backfaces(vec![plane.clone()]);
  assert_eq!(kept.len(), 1);
  assert_eq!(kept[0].item, plane.item);
}

#[test]
fn culled_backface_plane_does_not_occlude_in_depth_sort() {
  let hidden_front = SceneItem {
    item: "front",
    accumulated_transform: Transform3D::translate(0.0, 0.0, 10.0)
      .multiply(&Transform3D::rotate_y(std::f32::consts::PI)),
    backface_visibility: BackfaceVisibility::Hidden,
  };
  let visible_back = SceneItem {
    item: "behind",
    accumulated_transform: Transform3D::translate(0.0, 0.0, 1.0),
    backface_visibility: BackfaceVisibility::Visible,
  };

  let sorted = depth_sort_scene(vec![hidden_front, visible_back.clone()]);
  assert_eq!(sorted.len(), 1, "hidden plane should be culled before sorting");
  assert_eq!(sorted[0].item, visible_back.item);
}
