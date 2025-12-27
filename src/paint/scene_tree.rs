use crate::paint::display_list::BlendMode;
use crate::paint::display_list::DisplayItem;
use crate::paint::display_list::DisplayList;
use crate::paint::display_list::StackingContextItem;
use crate::paint::display_list::Transform3D;
use crate::style::types::BackfaceVisibility;
use crate::style::types::TransformStyle;

/// A stacking context node reconstructed from the flat display list.
#[derive(Debug, Clone)]
pub struct StackingNode {
  pub item: StackingContextItem,
  pub children: Vec<NodeChild>,
}

/// A child of a stacking context in paint order.
///
/// Primitive runs contain only non-stacking-context items (including other push/pop
/// effects like clips or opacity), while [`ChildContext`] holds a nested stacking context.
#[derive(Debug, Clone)]
pub enum NodeChild {
  PrimitiveRun(Vec<DisplayItem>),
  ChildContext(Box<StackingNode>),
}

/// A flattened scene item ready for depth sorting inside a preserve-3d context.
#[derive(Debug, Clone)]
pub struct SceneItem {
  /// Stable paint order derived from the original display list order.
  pub paint_order: u32,
  /// Accumulated transform from the preserve-3d root to this item's plane.
  pub accumulated_transform: Transform3D,
  /// Backface visibility of the originating stacking context.
  pub backface_visibility: BackfaceVisibility,
  /// Render payload for this scene item.
  pub content: SceneItemContent,
}

/// Render payload for a scene item.
#[derive(Debug, Clone)]
pub enum SceneItemContent {
  /// A run of primitives and non-stacking-context push/pop operations.
  PrimitiveRun(Vec<DisplayItem>),
  /// A subtree that must be flattened before entering the 3D context.
  FlattenedSubtree(StackingNode),
}

/// Build a hierarchical representation of the display list preserving stacking context nesting.
///
/// Returns a list of root-level children in paint order. Each child is either a primitive
/// run (with no stacking contexts inside) or an actual stacking context subtree.
pub fn build_stacking_tree(list: &DisplayList) -> Vec<NodeChild> {
  #[derive(Debug)]
  struct Frame {
    node: StackingNode,
    pending_run: Vec<DisplayItem>,
  }

  let mut roots: Vec<NodeChild> = Vec::new();
  let mut stack: Vec<Frame> = Vec::new();
  let mut top_level_run: Vec<DisplayItem> = Vec::new();

  let mut flush_run = |run: &mut Vec<DisplayItem>, target: &mut Vec<NodeChild>| {
    if run.is_empty() {
      return;
    }
    let mut current = Vec::new();
    std::mem::swap(run, &mut current);
    target.push(NodeChild::PrimitiveRun(current));
  };

  for item in list.items() {
    match item {
      DisplayItem::PushStackingContext(sc) => {
        if let Some(frame) = stack.last_mut() {
          flush_run(&mut frame.pending_run, &mut frame.node.children);
        } else {
          flush_run(&mut top_level_run, &mut roots);
        }

        stack.push(Frame {
          node: StackingNode {
            item: sc.clone(),
            children: Vec::new(),
          },
          pending_run: Vec::new(),
        });
      }
      DisplayItem::PopStackingContext => {
        if let Some(mut frame) = stack.pop() {
          flush_run(&mut frame.pending_run, &mut frame.node.children);
          let node = frame.node;
          if let Some(parent) = stack.last_mut() {
            parent
              .node
              .children
              .push(NodeChild::ChildContext(Box::new(node)));
          } else {
            roots.push(NodeChild::ChildContext(Box::new(node)));
          }
        }
      }
      other => {
        if let Some(frame) = stack.last_mut() {
          frame.pending_run.push(other.clone());
        } else {
          top_level_run.push(other.clone());
        }
      }
    }
  }

  while let Some(mut frame) = stack.pop() {
    flush_run(&mut frame.pending_run, &mut frame.node.children);
    let node = frame.node;
    if let Some(parent) = stack.last_mut() {
      parent
        .node
        .children
        .push(NodeChild::ChildContext(Box::new(node)));
    } else {
      roots.push(NodeChild::ChildContext(Box::new(node)));
    }
  }

  flush_run(&mut top_level_run, &mut roots);
  roots
}

/// Collect scene items for depth sorting within a preserve-3d context root.
pub fn collect_scene_items(root: &StackingNode) -> Vec<SceneItem> {
  let mut items = Vec::new();
  let mut paint_order: u32 = 0;
  let base_transform = root.item.transform.unwrap_or(Transform3D::IDENTITY);
  collect_scene_items_inner(root, base_transform, &mut paint_order, &mut items);
  items
}

fn collect_scene_items_inner(
  node: &StackingNode,
  current_transform: Transform3D,
  paint_order: &mut u32,
  items: &mut Vec<SceneItem>,
) {
  for child in &node.children {
    match child {
      NodeChild::PrimitiveRun(run) => {
        if run.is_empty() {
          continue;
        }
        items.push(SceneItem {
          paint_order: *paint_order,
          accumulated_transform: current_transform,
          backface_visibility: node.item.backface_visibility,
          content: SceneItemContent::PrimitiveRun(run.clone()),
        });
        *paint_order = paint_order.saturating_add(1);
      }
      NodeChild::ChildContext(child_node) => {
        let child_transform = child_node.item.transform.unwrap_or(Transform3D::IDENTITY);
        let accumulated = current_transform.multiply(&child_transform);

        if extends_3d_context(&child_node.item) {
          collect_scene_items_inner(child_node, accumulated, paint_order, items);
        } else {
          items.push(SceneItem {
            paint_order: *paint_order,
            accumulated_transform: accumulated,
            backface_visibility: child_node.item.backface_visibility,
            content: SceneItemContent::FlattenedSubtree((**child_node).clone()),
          });
          *paint_order = paint_order.saturating_add(1);
        }
      }
    }
  }
}

fn extends_3d_context(item: &StackingContextItem) -> bool {
  item.transform_style == TransformStyle::Preserve3d && !is_flattening_boundary(item)
}

fn is_flattening_boundary(item: &StackingContextItem) -> bool {
  item.is_isolated
    || item.mask.is_some()
    || !item.filters.is_empty()
    || !item.backdrop_filters.is_empty()
    || item.mix_blend_mode != BlendMode::Normal
}
