use fastrender::geometry::Rect;
use fastrender::paint::display_list::DisplayItem;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::paint::stacking::build_stacking_tree_from_tree;
use fastrender::style::ComputedStyle;
use fastrender::tree::fragment_tree::{FragmentNode, FragmentTree};
use std::sync::Arc;

#[test]
fn display_lists_include_additional_fragment_roots() {
  let text_style = Arc::new(ComputedStyle::default());

  let root_text = FragmentNode::new_text_styled(
    Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    "root".into(),
    0.0,
    text_style.clone(),
  );
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 20.0), vec![root_text]);

  let extra_text = FragmentNode::new_text_styled(
    Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    "extra".into(),
    0.0,
    text_style,
  );
  let extra_root = FragmentNode::new_block(Rect::from_xywh(0.0, 100.0, 50.0, 20.0), vec![extra_text]);

  let mut tree = FragmentTree::new(root);
  tree.additional_fragments.push(extra_root);

  let list = DisplayListBuilder::new().build_tree(&tree);
  let mut text_positions: Vec<f32> = list
    .items()
    .iter()
    .filter_map(|item| match item {
      DisplayItem::Text(text) => Some(text.origin.y),
      _ => None,
    })
    .collect();
  text_positions.sort_by(|a, b| a.partial_cmp(b).unwrap());
  assert_eq!(text_positions.len(), 2);
  assert!((text_positions[1] - text_positions[0] - 100.0).abs() < 0.1);

  let stackings = build_stacking_tree_from_tree(&tree);
  let stacking_list = DisplayListBuilder::new().build_from_stacking_contexts(&stackings);
  let mut stacking_positions: Vec<f32> = stacking_list
    .items()
    .iter()
    .filter_map(|item| match item {
      DisplayItem::Text(text) => Some(text.origin.y),
      _ => None,
    })
    .collect();
  stacking_positions.sort_by(|a, b| a.partial_cmp(b).unwrap());
  assert_eq!(stacking_positions.len(), 2);
  assert!((stacking_positions[1] - stacking_positions[0] - 100.0).abs() < 0.1);
}
