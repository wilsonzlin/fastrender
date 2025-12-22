use std::collections::HashMap;

use crate::geometry::{Point, Rect, Size};
use crate::style::types::{
  Direction, ScrollBehavior, ScrollSnapAlign, ScrollSnapAxis, ScrollSnapStop, ScrollSnapStrictness,
  WritingMode,
};
use crate::style::{block_axis_is_horizontal, inline_axis_is_horizontal, PhysicalSide};
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};

/// Viewport and element scroll offsets used when applying scroll snap.
#[derive(Debug, Clone, PartialEq)]
pub struct ScrollState {
  /// Document/viewport scroll offset
  pub viewport: Point,
  /// Scroll offsets for element scroll containers keyed by box_id
  pub elements: HashMap<usize, Point>,
}

impl ScrollState {
  /// Creates a scroll state with only a viewport offset.
  pub fn with_viewport(viewport: Point) -> Self {
    Self {
      viewport,
      elements: HashMap::new(),
    }
  }

  /// Returns the stored scroll offset for an element, if present.
  pub fn element_offset(&self, id: usize) -> Point {
    self.elements.get(&id).copied().unwrap_or(Point::ZERO)
  }
}

impl Default for ScrollState {
  fn default() -> Self {
    Self::with_viewport(Point::ZERO)
  }
}

/// A single snap target along an axis.
#[derive(Debug, Clone, PartialEq)]
pub struct ScrollSnapTarget {
  pub position: f32,
  pub stop: ScrollSnapStop,
}

/// Metadata for a scroll snap container.
#[derive(Debug, Clone, PartialEq)]
pub struct ScrollSnapContainer {
  pub box_id: Option<usize>,
  pub viewport: Size,
  pub strictness: ScrollSnapStrictness,
  pub behavior: ScrollBehavior,
  pub snap_x: bool,
  pub snap_y: bool,
  pub padding_x: (f32, f32),
  pub padding_y: (f32, f32),
  pub scroll_bounds: Rect,
  pub targets_x: Vec<ScrollSnapTarget>,
  pub targets_y: Vec<ScrollSnapTarget>,
  pub uses_viewport_scroll: bool,
}

/// Aggregated scroll snap metadata for a fragment tree.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct ScrollMetadata {
  pub containers: Vec<ScrollSnapContainer>,
}

/// Result of applying scroll snap to a scroll state.
#[derive(Debug, Clone, PartialEq)]
pub struct ScrollSnapUpdate {
  pub container: Option<usize>,
  pub offset: Point,
  pub behavior: ScrollBehavior,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScrollSnapResult {
  pub state: ScrollState,
  pub updates: Vec<ScrollSnapUpdate>,
}

fn is_vertical_writing_mode(mode: WritingMode) -> bool {
  matches!(
    mode,
    WritingMode::VerticalRl | WritingMode::VerticalLr | WritingMode::SidewaysRl | WritingMode::SidewaysLr
  )
}

fn inline_axis_positive(mode: WritingMode, direction: Direction) -> bool {
  match mode {
    WritingMode::HorizontalTb => direction != Direction::Rtl,
    WritingMode::VerticalRl | WritingMode::VerticalLr | WritingMode::SidewaysRl | WritingMode::SidewaysLr => {
      true
    }
  }
}

fn block_axis_positive(mode: WritingMode) -> bool {
  match mode {
    WritingMode::VerticalRl | WritingMode::SidewaysRl => false,
    _ => true,
  }
}

fn axis_sides(mode: WritingMode, direction: Direction, inline_axis: bool) -> (PhysicalSide, PhysicalSide) {
  let horizontal = if inline_axis {
    inline_axis_is_horizontal(mode)
  } else {
    block_axis_is_horizontal(mode)
  };
  let positive = if inline_axis {
    inline_axis_positive(mode, direction)
  } else {
    block_axis_positive(mode)
  };

  if horizontal {
    if positive {
      (PhysicalSide::Left, PhysicalSide::Right)
    } else {
      (PhysicalSide::Right, PhysicalSide::Left)
    }
  } else if positive {
    (PhysicalSide::Top, PhysicalSide::Bottom)
  } else {
    (PhysicalSide::Bottom, PhysicalSide::Top)
  }
}

fn base_for_side(side: PhysicalSide, viewport: Size) -> f32 {
  match side {
    PhysicalSide::Left | PhysicalSide::Right => viewport.width,
    PhysicalSide::Top | PhysicalSide::Bottom => viewport.height,
  }
}

fn resolve_snap_length(len: Length, percentage_base: f32) -> f32 {
  len
    .resolve_against(percentage_base)
    .unwrap_or_else(|| len.to_px())
}

fn scroll_padding_for_side(style: &ComputedStyle, side: PhysicalSide) -> Length {
  match side {
    PhysicalSide::Top => style.scroll_padding_top,
    PhysicalSide::Right => style.scroll_padding_right,
    PhysicalSide::Bottom => style.scroll_padding_bottom,
    PhysicalSide::Left => style.scroll_padding_left,
  }
}

fn scroll_margin_for_side(style: &ComputedStyle, side: PhysicalSide) -> Length {
  match side {
    PhysicalSide::Top => style.scroll_margin_top,
    PhysicalSide::Right => style.scroll_margin_right,
    PhysicalSide::Bottom => style.scroll_margin_bottom,
    PhysicalSide::Left => style.scroll_margin_left,
  }
}

fn snap_axis_flags(axis: ScrollSnapAxis, inline_vertical: bool) -> (bool, bool) {
  match axis {
    ScrollSnapAxis::None => (false, false),
    ScrollSnapAxis::Both => (true, true),
    ScrollSnapAxis::X => (true, false),
    ScrollSnapAxis::Y => (false, true),
    ScrollSnapAxis::Inline => {
      if inline_vertical {
        (false, true)
      } else {
        (true, false)
      }
    }
    ScrollSnapAxis::Block => {
      if inline_vertical {
        (true, false)
      } else {
        (false, true)
      }
    }
  }
}

fn snap_position(
  alignment: ScrollSnapAlign,
  phys_start: f32,
  phys_end: f32,
  viewport_extent: f32,
  padding_start: f32,
  padding_end: f32,
  margin_start: f32,
  margin_end: f32,
  axis_positive: bool,
) -> Option<f32> {
  let target_start = if axis_positive {
    phys_start - margin_start
  } else {
    phys_end + margin_start
  };
  let target_end = if axis_positive {
    phys_end + margin_end
  } else {
    phys_start - margin_end
  };

  let snapport_start_offset = if axis_positive {
    padding_start
  } else {
    viewport_extent - padding_start
  };
  let snapport_end_offset = if axis_positive {
    viewport_extent - padding_end
  } else {
    padding_end
  };

  match alignment {
    ScrollSnapAlign::None => None,
    ScrollSnapAlign::Start => Some(target_start - snapport_start_offset),
    ScrollSnapAlign::End => Some(target_end - snapport_end_offset),
    ScrollSnapAlign::Center => {
      let target_center = (target_start + target_end) * 0.5;
      let snapport_center = (snapport_start_offset + snapport_end_offset) * 0.5;
      Some(target_center - snapport_center)
    }
  }
}

fn pick_snap_target(
  current: f32,
  max_scroll: f32,
  strictness: ScrollSnapStrictness,
  threshold: f32,
  candidates: &[(f32, ScrollSnapStop)],
) -> f32 {
  if candidates.is_empty() {
    return current.min(max_scroll).max(0.0);
  }

  let mut best = current;
  let mut best_dist = f32::INFINITY;
  let mut best_stop_always = false;

  for &(candidate, stop) in candidates {
    let clamped = candidate.min(max_scroll).max(0.0);
    let dist = (clamped - current).abs();
    let prefer = dist + 1e-3 < best_dist
      || ((dist - best_dist).abs() <= 1e-3 && stop == ScrollSnapStop::Always && !best_stop_always);
    if prefer {
      best = clamped;
      best_dist = dist;
      best_stop_always = stop == ScrollSnapStop::Always;
    }
  }

  match strictness {
    ScrollSnapStrictness::Mandatory => best,
    ScrollSnapStrictness::Proximity => {
      if best_dist <= threshold {
        best
      } else {
        current
      }
    }
  }
}

fn fragment_box_id(fragment: &FragmentNode) -> Option<usize> {
  match &fragment.content {
    FragmentContent::Block { box_id }
    | FragmentContent::Inline { box_id, .. }
    | FragmentContent::Text { box_id, .. }
    | FragmentContent::Replaced { box_id, .. } => *box_id,
    FragmentContent::Line { .. } => None,
  }
}

fn annotate_overflow(node: &mut FragmentNode) -> Rect {
  let mut overflow = Rect::from_xywh(0.0, 0.0, node.bounds.width(), node.bounds.height());
  for child in &mut node.children {
    let child_overflow = annotate_overflow(child);
    let translated = child_overflow.translate(Point::new(child.bounds.x(), child.bounds.y()));
    overflow = overflow.union(translated);
  }
  node.scroll_overflow = overflow;
  overflow
}

struct PendingContainer {
  origin: Point,
  viewport: Size,
  strictness: ScrollSnapStrictness,
  behavior: ScrollBehavior,
  snap_x: bool,
  snap_y: bool,
  inline_sides: (PhysicalSide, PhysicalSide),
  block_sides: (PhysicalSide, PhysicalSide),
  axis_is_inline_for_x: bool,
  axis_is_inline_for_y: bool,
  start_positive_x: bool,
  start_positive_y: bool,
  padding_x: (f32, f32),
  padding_y: (f32, f32),
  content_bounds: Rect,
  targets_x: Vec<ScrollSnapTarget>,
  targets_y: Vec<ScrollSnapTarget>,
  box_id: Option<usize>,
  uses_viewport_scroll: bool,
}

impl PendingContainer {
  fn new(
    node: &FragmentNode,
    style: &ComputedStyle,
    origin: Point,
    uses_viewport_scroll: bool,
    viewport: Size,
  ) -> Option<Self> {
    let inline_vertical = is_vertical_writing_mode(style.writing_mode);
    let (snap_x, snap_y) = snap_axis_flags(style.scroll_snap_type.axis, inline_vertical);
    if !snap_x && !snap_y {
      return None;
    }

    let axis_is_inline_for_x = !inline_vertical;
    let axis_is_inline_for_y = inline_vertical;
    let inline_sides = axis_sides(style.writing_mode, style.direction, true);
    let block_sides = axis_sides(style.writing_mode, style.direction, false);
    let inline_positive = inline_axis_positive(style.writing_mode, style.direction);
    let block_positive = block_axis_positive(style.writing_mode);
    let padding_x_sides = if axis_is_inline_for_x {
      inline_sides
    } else {
      block_sides
    };
    let padding_y_sides = if axis_is_inline_for_y {
      inline_sides
    } else {
      block_sides
    };
    let padding_x = (
      resolve_snap_length(
        scroll_padding_for_side(style, padding_x_sides.0),
        base_for_side(padding_x_sides.0, viewport),
      )
      .max(0.0),
      resolve_snap_length(
        scroll_padding_for_side(style, padding_x_sides.1),
        base_for_side(padding_x_sides.1, viewport),
      )
      .max(0.0),
    );
    let padding_y = (
      resolve_snap_length(
        scroll_padding_for_side(style, padding_y_sides.0),
        base_for_side(padding_y_sides.0, viewport),
      )
      .max(0.0),
      resolve_snap_length(
        scroll_padding_for_side(style, padding_y_sides.1),
        base_for_side(padding_y_sides.1, viewport),
      )
      .max(0.0),
    );

    Some(Self {
      origin: origin,
      viewport,
      strictness: style.scroll_snap_type.strictness,
      behavior: style.scroll_behavior,
      snap_x,
      snap_y,
      inline_sides,
      block_sides,
      axis_is_inline_for_x,
      axis_is_inline_for_y,
      start_positive_x: if axis_is_inline_for_x {
        inline_positive
      } else {
        block_positive
      },
      start_positive_y: if axis_is_inline_for_y {
        inline_positive
      } else {
        block_positive
      },
      padding_x,
      padding_y,
      content_bounds: node.scroll_overflow,
      targets_x: Vec::new(),
      targets_y: Vec::new(),
      box_id: fragment_box_id(node),
      uses_viewport_scroll,
    })
  }

  fn collect_targets(&mut self, style: &ComputedStyle, node: &FragmentNode, origin: Point) {
    let rel_bounds = Rect::from_xywh(
      origin.x - self.origin.x,
      origin.y - self.origin.y,
      node.bounds.width(),
      node.bounds.height(),
    );

    if self.snap_x {
      let sides = if self.axis_is_inline_for_x {
        self.inline_sides
      } else {
        self.block_sides
      };
      let base = base_for_side(sides.0, self.viewport);
      let margin_start = resolve_snap_length(scroll_margin_for_side(style, sides.0), base);
      let margin_end = resolve_snap_length(scroll_margin_for_side(style, sides.1), base);
      let align = if self.axis_is_inline_for_x {
        style.scroll_snap_align.inline
      } else {
        style.scroll_snap_align.block
      };
      if let Some(pos) = snap_position(
        align,
        rel_bounds.min_x(),
        rel_bounds.max_x(),
        self.viewport.width,
        self.padding_x.0,
        self.padding_x.1,
        margin_start,
        margin_end,
        self.start_positive_x,
      ) {
        self
          .targets_x
          .push(ScrollSnapTarget { position: pos, stop: style.scroll_snap_stop });
      }
    }

    if self.snap_y {
      let sides = if self.axis_is_inline_for_y {
        self.inline_sides
      } else {
        self.block_sides
      };
      let base = base_for_side(sides.0, self.viewport);
      let margin_start = resolve_snap_length(scroll_margin_for_side(style, sides.0), base);
      let margin_end = resolve_snap_length(scroll_margin_for_side(style, sides.1), base);
      let align = if self.axis_is_inline_for_y {
        style.scroll_snap_align.inline
      } else {
        style.scroll_snap_align.block
      };
      if let Some(pos) = snap_position(
        align,
        rel_bounds.min_y(),
        rel_bounds.max_y(),
        self.viewport.height,
        self.padding_y.0,
        self.padding_y.1,
        margin_start,
        margin_end,
        self.start_positive_y,
      ) {
        self
          .targets_y
          .push(ScrollSnapTarget { position: pos, stop: style.scroll_snap_stop });
      }
    }
  }

  fn finalize(self) -> ScrollSnapContainer {
    let mut min_x = self.content_bounds.min_x();
    let mut max_x = self.content_bounds.max_x();
    if self.snap_x {
      if let Some(max_target_x) = self.targets_x.iter().map(|t| t.position).max_by(|a, b| {
        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
      }) {
        max_x = max_x.max(max_target_x + self.viewport.width);
      }
      if let Some(min_target_x) = self.targets_x.iter().map(|t| t.position).min_by(|a, b| {
        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
      }) {
        min_x = min_x.min(min_target_x);
      }
    }

    let mut min_y = self.content_bounds.min_y();
    let mut max_y = self.content_bounds.max_y();
    if self.snap_y {
      if let Some(max_target_y) = self.targets_y.iter().map(|t| t.position).max_by(|a, b| {
        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
      }) {
        max_y = max_y.max(max_target_y + self.viewport.height);
      }
      if let Some(min_target_y) = self.targets_y.iter().map(|t| t.position).min_by(|a, b| {
        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
      }) {
        min_y = min_y.min(min_target_y);
      }
    }

    let scroll_bounds = Rect::from_xywh(
      min_x,
      min_y,
      (max_x - min_x).max(0.0),
      (max_y - min_y).max(0.0),
    );

    ScrollSnapContainer {
      box_id: self.box_id,
      viewport: self.viewport,
      strictness: self.strictness,
      behavior: self.behavior,
      snap_x: self.snap_x,
      snap_y: self.snap_y,
      padding_x: self.padding_x,
      padding_y: self.padding_y,
      scroll_bounds,
      targets_x: self.targets_x,
      targets_y: self.targets_y,
      uses_viewport_scroll: self.uses_viewport_scroll,
    }
  }
}

fn collect_scroll_metadata(
  node: &mut FragmentNode,
  origin: Point,
  stack: &mut Vec<PendingContainer>,
  metadata: &mut ScrollMetadata,
  root_viewport: Size,
) {
  let style = node.style.clone();

  let mut pushed = false;
  if let Some(style) = style.as_ref() {
    let uses_viewport_scroll = stack.is_empty();
    let viewport = if uses_viewport_scroll {
      root_viewport
    } else {
      Size::new(node.bounds.width(), node.bounds.height())
    };
    if let Some(container) = PendingContainer::new(node, style, origin, uses_viewport_scroll, viewport) {
      stack.push(container);
      pushed = true;
    }
  }

  let overflow_abs = node.scroll_overflow.translate(origin);
  for container in stack.iter_mut() {
    let relative = overflow_abs.translate(Point::new(-container.origin.x, -container.origin.y));
    container.content_bounds = container.content_bounds.union(relative);
  }

  if let Some(style) = style.as_ref() {
    for container in stack.iter_mut() {
      container.collect_targets(style, node, origin);
    }
  }

  for child in &mut node.children {
    let child_origin = Point::new(origin.x + child.bounds.x(), origin.y + child.bounds.y());
    collect_scroll_metadata(child, child_origin, stack, metadata, root_viewport);
  }

  if pushed {
    if let Some(container) = stack.pop() {
      metadata.containers.push(container.finalize());
    }
  }
}

/// Computes scrollable overflow areas and snap target lists for a fragment tree.
pub(crate) fn build_scroll_metadata(tree: &mut FragmentTree) -> ScrollMetadata {
  annotate_overflow(&mut tree.root);
  for fragment in &mut tree.additional_fragments {
    annotate_overflow(fragment);
  }

  let mut metadata = ScrollMetadata::default();
  let mut stack = Vec::new();
  let root_viewport = tree.viewport_size();
  collect_scroll_metadata(&mut tree.root, Point::ZERO, &mut stack, &mut metadata, root_viewport);

  for fragment in &mut tree.additional_fragments {
    collect_scroll_metadata(
      fragment,
      Point::new(fragment.bounds.x(), fragment.bounds.y()),
      &mut stack,
      &mut metadata,
      root_viewport,
    );
  }

  metadata
}

fn snap_axis(
  current: f32,
  viewport_extent: f32,
  strictness: ScrollSnapStrictness,
  targets: &[ScrollSnapTarget],
  bounds: &Rect,
  vertical: bool,
) -> f32 {
  let origin = if vertical { bounds.min_y() } else { bounds.min_x() };
  let max_scroll = if vertical {
    (bounds.max_y() - origin - viewport_extent).max(0.0)
  } else {
    (bounds.max_x() - origin - viewport_extent).max(0.0)
  };
  let candidates: Vec<(f32, ScrollSnapStop)> = targets
    .iter()
    .map(|t| (t.position - origin, t.stop))
    .collect();
  pick_snap_target(current - origin, max_scroll, strictness, viewport_extent * 0.5, &candidates)
    + origin
}

/// Applies scroll snap to all snap containers in the fragment tree.
pub(crate) fn apply_scroll_snap(tree: &mut FragmentTree, scroll: &ScrollState) -> ScrollSnapResult {
  tree.ensure_scroll_metadata();
  let metadata = tree.scroll_metadata.clone().unwrap_or_default();

  let mut state = scroll.clone();
  let mut updates = Vec::new();

  for container in metadata.containers {
    let element_offset = container
      .box_id
      .and_then(|id| state.elements.get(&id).copied());
    if !container.uses_viewport_scroll && element_offset.is_none() {
      continue;
    }

    let mut current = if container.uses_viewport_scroll {
      state.viewport
    } else if let Some(offset) = element_offset {
      offset
    } else {
      state.viewport
    };

    if container.snap_x {
      current.x = snap_axis(
        current.x,
        container.viewport.width,
        container.strictness,
        &container.targets_x,
        &container.scroll_bounds,
        false,
      );
    }
    if container.snap_y {
      current.y = snap_axis(
        current.y,
        container.viewport.height,
        container.strictness,
        &container.targets_y,
        &container.scroll_bounds,
        true,
      );
    }

    let changed;
    if container.uses_viewport_scroll {
      changed = current != state.viewport;
      state.viewport = current;
    } else if let Some(id) = container.box_id {
      let prev = state.elements.insert(id, current);
      changed = prev.map(|p| p != current).unwrap_or(true);
    } else {
      changed = current != state.viewport;
      state.viewport = current;
    }

    if changed {
      updates.push(ScrollSnapUpdate {
        container: container.box_id,
        offset: current,
        behavior: container.behavior,
      });
    }
  }

  ScrollSnapResult { state, updates }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::{Point, Rect, Size};
  use std::sync::Arc;
  use crate::tree::fragment_tree::FragmentContent;

  fn container_style(axis: ScrollSnapAxis, strictness: ScrollSnapStrictness) -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.scroll_snap_type.axis = axis;
    style.scroll_snap_type.strictness = strictness;
    Arc::new(style)
  }

  fn target_style(inline: ScrollSnapAlign, block: ScrollSnapAlign) -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.scroll_snap_align.inline = inline;
    style.scroll_snap_align.block = block;
    Arc::new(style)
  }

  #[test]
  fn both_axes_snap_to_start() {
    let mut container_style = ComputedStyle::default();
    container_style.scroll_snap_type.axis = ScrollSnapAxis::Both;
    container_style.scroll_snap_type.strictness = ScrollSnapStrictness::Mandatory;
    let container_style = Arc::new(container_style);

    let mut child_style = ComputedStyle::default();
    child_style.scroll_snap_align.inline = ScrollSnapAlign::Start;
    child_style.scroll_snap_align.block = ScrollSnapAlign::Start;
    let child_style = Arc::new(child_style);

    let child = FragmentNode::new_block_styled(
      Rect::from_xywh(120.0, 150.0, 50.0, 50.0),
      vec![],
      child_style,
    );

    let container = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 300.0, 300.0),
      vec![child],
      container_style,
    );
    let mut tree = FragmentTree::with_viewport(container, Size::new(100.0, 100.0));

    let snapped = apply_scroll_snap(&mut tree, &ScrollState::with_viewport(Point::new(90.0, 160.0)))
      .state
      .viewport;
    assert!((snapped.x - 120.0).abs() < 0.1);
    assert!((snapped.y - 150.0).abs() < 0.1);
  }

  #[test]
  fn nested_containers_snap_independently() {
    let outer_style = container_style(ScrollSnapAxis::Y, ScrollSnapStrictness::Mandatory);
    let inner_style = container_style(ScrollSnapAxis::X, ScrollSnapStrictness::Mandatory);

    let mut inner_child = FragmentNode::new_block_styled(
      Rect::from_xywh(50.0, 0.0, 120.0, 80.0),
      vec![FragmentNode::new_block_styled(
        Rect::from_xywh(0.0, 0.0, 60.0, 80.0),
        vec![],
        target_style(ScrollSnapAlign::Start, ScrollSnapAlign::Start),
      )],
      inner_style.clone(),
    );
    if let FragmentContent::Block { box_id } = &mut inner_child.content {
      *box_id = Some(2);
    }

    let mut outer = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 200.0),
      vec![
        FragmentNode::new_block_styled(
          Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
          vec![],
          target_style(ScrollSnapAlign::Start, ScrollSnapAlign::Start),
        ),
        FragmentNode::new_block_styled(
          Rect::from_xywh(0.0, 120.0, 100.0, 100.0),
          vec![inner_child],
          target_style(ScrollSnapAlign::Start, ScrollSnapAlign::Start),
        ),
      ],
      outer_style,
    );

    if let FragmentContent::Block { box_id } = &mut outer.content {
      *box_id = Some(1);
    }
    let mut tree = FragmentTree::with_viewport(outer, Size::new(100.0, 100.0));

    let mut state = ScrollState::with_viewport(Point::new(0.0, 130.0));
    state.elements.insert(2, Point::new(70.0, 0.0));

    let snapped = apply_scroll_snap(&mut tree, &state);
    let outer_offset = snapped.state.viewport;
    assert!((outer_offset.y - 120.0).abs() < 0.1);

    let inner_offset = snapped.state.elements.get(&2).copied().unwrap();
    assert!((inner_offset.x - 50.0).abs() < 0.1);
  }

  #[test]
  fn rtl_inline_start_uses_physical_right() {
    let mut container_style = ComputedStyle::default();
    container_style.scroll_snap_type.axis = ScrollSnapAxis::X;
    container_style.scroll_snap_type.strictness = ScrollSnapStrictness::Mandatory;
    container_style.direction = Direction::Rtl;
    let container_style = Arc::new(container_style);

    let mut target_style = ComputedStyle::default();
    target_style.scroll_snap_align.inline = ScrollSnapAlign::Start;
    let target_style = Arc::new(target_style);

    let target = FragmentNode::new_block_styled(
      Rect::from_xywh(80.0, 0.0, 80.0, 50.0),
      vec![],
      target_style,
    );

    let container = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 80.0),
      vec![target],
      container_style,
    );
    let mut tree = FragmentTree::with_viewport(container, Size::new(100.0, 80.0));

    let state = ScrollState::with_viewport(Point::new(70.0, 0.0));
    let snapped = apply_scroll_snap(&mut tree, &state);
    assert!((snapped.state.viewport.x - 60.0).abs() < 0.1);
  }
}
