use std::collections::HashMap;

use crate::geometry::{Point, Rect, Size};
use crate::style::types::{
  Direction, Overflow, OverscrollBehavior, ScrollBehavior, ScrollSnapAlign, ScrollSnapAxis,
  ScrollSnapStop, ScrollSnapStrictness, WritingMode,
};
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::style::{block_axis_is_horizontal, inline_axis_is_horizontal, PhysicalSide};
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
    WritingMode::VerticalRl
      | WritingMode::VerticalLr
      | WritingMode::SidewaysRl
      | WritingMode::SidewaysLr
  )
}

fn inline_axis_positive(mode: WritingMode, direction: Direction) -> bool {
  match mode {
    WritingMode::HorizontalTb => direction != Direction::Rtl,
    WritingMode::VerticalRl
    | WritingMode::VerticalLr
    | WritingMode::SidewaysRl
    | WritingMode::SidewaysLr => true,
  }
}

fn block_axis_positive(mode: WritingMode) -> bool {
  match mode {
    WritingMode::VerticalRl | WritingMode::SidewaysRl => false,
    _ => true,
  }
}

fn axis_sides(
  mode: WritingMode,
  direction: Direction,
  inline_axis: bool,
) -> (PhysicalSide, PhysicalSide) {
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
      origin,
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
        self.targets_x.push(ScrollSnapTarget {
          position: pos,
          stop: style.scroll_snap_stop,
        });
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
        self.targets_y.push(ScrollSnapTarget {
          position: pos,
          stop: style.scroll_snap_stop,
        });
      }
    }
  }

  fn finalize(self) -> ScrollSnapContainer {
    let mut targets_x = self.targets_x;
    let mut targets_y = self.targets_y;

    let mut min_x = self.content_bounds.min_x();
    let mut max_x = self.content_bounds.max_x();
    if self.snap_x {
      if let Some(max_target_x) = targets_x
        .iter()
        .map(|t| t.position)
        .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
      {
        max_x = max_x.max(max_target_x + self.viewport.width);
      }
      if let Some(min_target_x) = targets_x
        .iter()
        .map(|t| t.position)
        .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
      {
        min_x = min_x.min(min_target_x);
      }
    }

    let mut min_y = self.content_bounds.min_y();
    let mut max_y = self.content_bounds.max_y();
    if self.snap_y {
      if let Some(max_target_y) = targets_y
        .iter()
        .map(|t| t.position)
        .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
      {
        max_y = max_y.max(max_target_y + self.viewport.height);
      }
      if let Some(min_target_y) = targets_y
        .iter()
        .map(|t| t.position)
        .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
      {
        min_y = min_y.min(min_target_y);
      }
    }

    let mut scroll_bounds = Rect::from_xywh(
      min_x,
      min_y,
      (max_x - min_x).max(0.0),
      (max_y - min_y).max(0.0),
    )
    .translate(self.origin);

    for target in &mut targets_x {
      target.position += self.origin.x;
    }
    for target in &mut targets_y {
      target.position += self.origin.y;
    }

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
      targets_x,
      targets_y,
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
    if let Some(container) =
      PendingContainer::new(node, style, origin, uses_viewport_scroll, viewport)
    {
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

fn merge_containers(containers: Vec<ScrollSnapContainer>) -> Vec<ScrollSnapContainer> {
  let mut merged = Vec::new();
  let mut by_id: HashMap<usize, usize> = HashMap::new();

  for container in containers {
    if let Some(box_id) = container.box_id {
      if let Some(idx) = by_id.get(&box_id).copied() {
        let existing = &mut merged[idx];
        debug_assert_eq!(
          existing.uses_viewport_scroll,
          container.uses_viewport_scroll
        );
        debug_assert_eq!(existing.viewport, container.viewport);
        debug_assert_eq!(existing.strictness, container.strictness);
        debug_assert_eq!(existing.behavior, container.behavior);
        debug_assert_eq!(existing.snap_x, container.snap_x);
        debug_assert_eq!(existing.snap_y, container.snap_y);
        debug_assert_eq!(existing.padding_x, container.padding_x);
        debug_assert_eq!(existing.padding_y, container.padding_y);

        existing.scroll_bounds = existing.scroll_bounds.union(container.scroll_bounds);
        existing.targets_x.extend(container.targets_x);
        existing.targets_y.extend(container.targets_y);
      } else {
        by_id.insert(box_id, merged.len());
        merged.push(container);
      }
    } else {
      merged.push(container);
    }
  }

  merged
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
  collect_scroll_metadata(
    &mut tree.root,
    Point::ZERO,
    &mut stack,
    &mut metadata,
    root_viewport,
  );

  for fragment in &mut tree.additional_fragments {
    collect_scroll_metadata(
      fragment,
      Point::new(fragment.bounds.x(), fragment.bounds.y()),
      &mut stack,
      &mut metadata,
      root_viewport,
    );
  }

  metadata.containers = merge_containers(metadata.containers);
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
  let origin = if vertical {
    bounds.min_y()
  } else {
    bounds.min_x()
  };
  let max_scroll = if vertical {
    (bounds.max_y() - origin - viewport_extent).max(0.0)
  } else {
    (bounds.max_x() - origin - viewport_extent).max(0.0)
  };
  let candidates: Vec<(f32, ScrollSnapStop)> = targets
    .iter()
    .map(|t| (t.position - origin, t.stop))
    .collect();
  pick_snap_target(
    current - origin,
    max_scroll,
    strictness,
    viewport_extent * 0.5,
    &candidates,
  ) + origin
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ScrollBounds {
  pub min_x: f32,
  pub min_y: f32,
  pub max_x: f32,
  pub max_y: f32,
}

impl ScrollBounds {
  pub fn clamp(&self, scroll: Point) -> Point {
    Point::new(
      scroll.x.clamp(self.min_x, self.max_x),
      scroll.y.clamp(self.min_y, self.max_y),
    )
  }
}

#[derive(Debug, Clone, Copy)]
struct Bounds {
  min_x: f32,
  min_y: f32,
  max_x: f32,
  max_y: f32,
}

impl Bounds {
  fn new(rect: Rect) -> Self {
    Self {
      min_x: rect.min_x(),
      min_y: rect.min_y(),
      max_x: rect.max_x(),
      max_y: rect.max_y(),
    }
  }

  fn update(&mut self, rect: Rect) {
    self.min_x = self.min_x.min(rect.min_x());
    self.min_y = self.min_y.min(rect.min_y());
    self.max_x = self.max_x.max(rect.max_x());
    self.max_y = self.max_y.max(rect.max_y());
  }
}

fn collect_bounds(node: &FragmentNode, origin: Point, bounds: &mut Bounds) {
  let abs_rect = Rect::from_xywh(
    origin.x,
    origin.y,
    node.bounds.width(),
    node.bounds.height(),
  );
  bounds.update(abs_rect);

  for child in &node.children {
    let child_origin = Point::new(
      abs_rect.x() + child.bounds.x(),
      abs_rect.y() + child.bounds.y(),
    );
    collect_bounds(child, child_origin, bounds);
  }
}

pub(crate) fn scroll_bounds_for_fragment(
  container: &FragmentNode,
  origin: Point,
  viewport: Size,
) -> ScrollBounds {
  let mut bounds = Bounds::new(Rect::from_xywh(
    origin.x,
    origin.y,
    viewport.width,
    viewport.height,
  ));
  collect_bounds(container, origin, &mut bounds);

  let min_x = (bounds.min_x - origin.x).min(0.0);
  let min_y = (bounds.min_y - origin.y).min(0.0);
  let max_x = (bounds.max_x - origin.x - viewport.width).max(min_x);
  let max_y = (bounds.max_y - origin.y - viewport.height).max(min_y);

  ScrollBounds {
    min_x,
    min_y,
    max_x,
    max_y,
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScrollSource {
  User,
  Programmatic,
}

#[derive(Debug, Clone, Copy)]
pub struct ScrollOptions {
  pub source: ScrollSource,
  pub simulate_overscroll: bool,
}

impl Default for ScrollOptions {
  fn default() -> Self {
    Self {
      source: ScrollSource::User,
      simulate_overscroll: false,
    }
  }
}

#[derive(Debug, Clone)]
pub struct ScrollChainSnapInfo<'a> {
  pub container: &'a FragmentNode,
  pub origin: Point,
}

#[derive(Debug, Clone)]
pub struct ScrollChainState<'a> {
  pub container: &'a FragmentNode,
  pub origin: Point,
  pub viewport: Size,
  pub bounds: ScrollBounds,
  pub scroll: Point,
  pub overscroll_behavior_x: OverscrollBehavior,
  pub overscroll_behavior_y: OverscrollBehavior,
  pub snap: Option<ScrollChainSnapInfo<'a>>,
}

impl<'a> ScrollChainState<'a> {
  pub fn from_fragment(
    node: &'a FragmentNode,
    origin: Point,
    viewport: Size,
    treat_as_root: bool,
  ) -> Option<Self> {
    let style = node.style.as_ref();
    let overscroll_behavior_x = style
      .map(|s| s.overscroll_behavior_x)
      .unwrap_or(OverscrollBehavior::Auto);
    let overscroll_behavior_y = style
      .map(|s| s.overscroll_behavior_y)
      .unwrap_or(OverscrollBehavior::Auto);
    let overflow_x = style.map(|s| s.overflow_x).unwrap_or(Overflow::Visible);
    let overflow_y = style.map(|s| s.overflow_y).unwrap_or(Overflow::Visible);

    let snap = style.and_then(|s| {
      if s.scroll_snap_type.axis != ScrollSnapAxis::None {
        Some(ScrollChainSnapInfo {
          container: node,
          origin,
        })
      } else {
        None
      }
    });

    let is_scroll_container = treat_as_root
      || matches!(overflow_x, Overflow::Auto | Overflow::Scroll)
      || matches!(overflow_y, Overflow::Auto | Overflow::Scroll)
      || snap.is_some();

    if !is_scroll_container {
      return None;
    }

    let bounds = scroll_bounds_for_fragment(node, origin, viewport);
    Some(Self {
      container: node,
      origin,
      viewport,
      bounds,
      scroll: Point::ZERO,
      overscroll_behavior_x,
      overscroll_behavior_y,
      snap,
    })
  }
}

pub fn build_scroll_chain<'a>(
  root: &'a FragmentNode,
  viewport: Size,
  path: &[usize],
) -> Vec<ScrollChainState<'a>> {
  let mut stack: Vec<(&FragmentNode, Point, Size, bool)> = Vec::new();
  let mut current = root;
  let mut origin = Point::new(root.bounds.x(), root.bounds.y());
  let mut current_viewport = viewport;
  stack.push((current, origin, current_viewport, true));

  for &idx in path {
    if let Some(child) = current.children.get(idx) {
      origin = Point::new(origin.x + child.bounds.x(), origin.y + child.bounds.y());
      current_viewport = child.bounds.size;
      stack.push((child, origin, current_viewport, false));
      current = child;
    } else {
      break;
    }
  }

  let mut out = Vec::new();
  for (node, origin, viewport, is_root) in stack.into_iter().rev() {
    if let Some(state) = ScrollChainState::from_fragment(node, origin, viewport, is_root) {
      out.push(state);
    }
  }

  out
}

#[derive(Debug, Clone)]
pub struct ScrollChainResult {
  pub remaining: Point,
  pub overscroll: Vec<Point>,
}

fn apply_scroll_to_state(
  state: &mut ScrollChainState,
  delta: Point,
  options: ScrollOptions,
) -> (Point, Point) {
  let target_x = state.scroll.x + delta.x;
  let target_y = state.scroll.y + delta.y;

  let clamped_x = target_x.clamp(state.bounds.min_x, state.bounds.max_x);
  let clamped_y = target_y.clamp(state.bounds.min_y, state.bounds.max_y);

  state.scroll = Point::new(clamped_x, clamped_y);

  let overshoot_x = target_x - clamped_x;
  let overshoot_y = target_y - clamped_y;

  let propagate_x = matches!(options.source, ScrollSource::User)
    && matches!(state.overscroll_behavior_x, OverscrollBehavior::Auto);
  let propagate_y = matches!(options.source, ScrollSource::User)
    && matches!(state.overscroll_behavior_y, OverscrollBehavior::Auto);

  let overscroll_x = if options.simulate_overscroll
    && !matches!(state.overscroll_behavior_x, OverscrollBehavior::None)
  {
    overshoot_x
  } else {
    0.0
  };

  let overscroll_y = if options.simulate_overscroll
    && !matches!(state.overscroll_behavior_y, OverscrollBehavior::None)
  {
    overshoot_y
  } else {
    0.0
  };

  (
    Point::new(
      if propagate_x { overshoot_x } else { 0.0 },
      if propagate_y { overshoot_y } else { 0.0 },
    ),
    Point::new(overscroll_x, overscroll_y),
  )
}

pub fn apply_scroll_chain(
  states: &mut [ScrollChainState],
  delta: Point,
  options: ScrollOptions,
) -> ScrollChainResult {
  let mut overscroll = Vec::with_capacity(states.len());
  let mut remaining = delta;

  for state in states.iter_mut() {
    let (leftover, over) = apply_scroll_to_state(state, remaining, options);
    remaining = leftover;

    if let Some(snap) = state.snap.as_ref() {
      if let Some(style) = snap.container.style.as_ref() {
        state.scroll = apply_scroll_snap_for_container(
          snap.container,
          style,
          state.viewport,
          state.scroll,
          snap.origin,
        );
      }
    }

    state.scroll = state.bounds.clamp(state.scroll);
    overscroll.push(over);
  }

  ScrollChainResult {
    remaining,
    overscroll,
  }
}

fn apply_scroll_snap_for_container(
  container: &FragmentNode,
  style: &ComputedStyle,
  viewport: Size,
  scroll: Point,
  container_origin: Point,
) -> Point {
  let scroll_bounds = scroll_bounds_for_fragment(container, container_origin, viewport);
  if style.scroll_snap_type.axis == ScrollSnapAxis::None {
    return scroll_bounds.clamp(scroll);
  }

  let inline_vertical = is_vertical_writing_mode(style.writing_mode);
  let (snap_x, snap_y) = snap_axis_flags(style.scroll_snap_type.axis, inline_vertical);
  if !snap_x && !snap_y {
    return scroll;
  }

  let padding_x = (
    resolve_snap_length(style.scroll_padding_left, viewport.width).max(0.0),
    resolve_snap_length(style.scroll_padding_right, viewport.width).max(0.0),
  );
  let padding_y = (
    resolve_snap_length(style.scroll_padding_top, viewport.height).max(0.0),
    resolve_snap_length(style.scroll_padding_bottom, viewport.height).max(0.0),
  );
  let mut targets_x = Vec::new();
  let mut targets_y = Vec::new();
  let mut snap_bounds = SnapBounds::default();
  let container_offset = Point::new(-container_origin.x, -container_origin.y);
  collect_snap_targets(
    container,
    container_offset,
    inline_vertical,
    snap_x,
    snap_y,
    viewport,
    padding_x,
    padding_y,
    &mut snap_bounds,
    &mut targets_x,
    &mut targets_y,
  );

  if let Some(max_target_x) = targets_x
    .iter()
    .map(|(p, _)| *p)
    .max_by(|a, b| a.partial_cmp(b).unwrap())
  {
    snap_bounds.max_x = snap_bounds.max_x.max(max_target_x + viewport.width);
  }
  if let Some(max_target_y) = targets_y
    .iter()
    .map(|(p, _)| *p)
    .max_by(|a, b| a.partial_cmp(b).unwrap())
  {
    snap_bounds.max_y = snap_bounds.max_y.max(max_target_y + viewport.height);
  }

  let min_target_x = targets_x
    .iter()
    .map(|(p, _)| *p)
    .min_by(|a, b| a.partial_cmp(b).unwrap())
    .unwrap_or(0.0);
  let min_target_y = targets_y
    .iter()
    .map(|(p, _)| *p)
    .min_by(|a, b| a.partial_cmp(b).unwrap())
    .unwrap_or(0.0);
  if min_target_x > 0.0 {
    for (p, _) in &mut targets_x {
      *p -= min_target_x;
    }
    snap_bounds.max_x = (snap_bounds.max_x - min_target_x).max(0.0);
  }
  if min_target_y > 0.0 {
    for (p, _) in &mut targets_y {
      *p -= min_target_y;
    }
    snap_bounds.max_y = (snap_bounds.max_y - min_target_y).max(0.0);
  }

  let container_rect = Rect::from_xywh(
    container.bounds.x() + container_offset.x,
    container.bounds.y() + container_offset.y,
    container.bounds.width(),
    container.bounds.height(),
  );
  snap_bounds.update(container_rect);

  let max_scroll_x = (snap_bounds.max_x - viewport.width).max(0.0);
  let max_scroll_y = (snap_bounds.max_y - viewport.height).max(0.0);
  let strictness = style.scroll_snap_type.strictness;
  let shift_x = if min_target_x > 0.0 {
    min_target_x
  } else {
    0.0
  };
  let shift_y = if min_target_y > 0.0 {
    min_target_y
  } else {
    0.0
  };

  let snapped_x = if snap_x {
    pick_snap_target(
      scroll.x - shift_x,
      max_scroll_x,
      strictness,
      viewport.width * 0.5,
      &targets_x,
    ) + shift_x
  } else {
    scroll.x
  };
  let snapped_y = if snap_y {
    pick_snap_target(
      scroll.y - shift_y,
      max_scroll_y,
      strictness,
      viewport.height * 0.5,
      &targets_y,
    ) + shift_y
  } else {
    scroll.y
  };

  scroll_bounds.clamp(Point::new(snapped_x, snapped_y))
}

#[derive(Default, Debug)]
pub struct SnapBounds {
  pub max_x: f32,
  pub max_y: f32,
}

impl SnapBounds {
  pub fn update(&mut self, rect: Rect) {
    self.max_x = self.max_x.max(rect.max_x());
    self.max_y = self.max_y.max(rect.max_y());
  }
}

pub(crate) fn collect_snap_targets(
  node: &FragmentNode,
  offset: Point,
  inline_vertical: bool,
  snap_x: bool,
  snap_y: bool,
  viewport: Size,
  padding_x: (f32, f32),
  padding_y: (f32, f32),
  bounds: &mut SnapBounds,
  targets_x: &mut Vec<(f32, ScrollSnapStop)>,
  targets_y: &mut Vec<(f32, ScrollSnapStop)>,
) {
  let abs_bounds = Rect::from_xywh(
    node.bounds.x() + offset.x,
    node.bounds.y() + offset.y,
    node.bounds.width(),
    node.bounds.height(),
  );
  bounds.update(abs_bounds);

  if let Some(style) = node.style.as_ref() {
    if snap_x {
      let margin_start = resolve_snap_length(style.scroll_margin_left, viewport.width);
      let margin_end = resolve_snap_length(style.scroll_margin_right, viewport.width);
      let (padding_start, padding_end) = padding_x;
      let align_x = if inline_vertical {
        style.scroll_snap_align.block
      } else {
        style.scroll_snap_align.inline
      };
      let axis_positive = if inline_vertical {
        // Physical x maps to the block axis in vertical writing modes.
        !matches!(
          style.writing_mode,
          WritingMode::VerticalRl | WritingMode::SidewaysRl
        )
      } else {
        // Physical x maps to the inline axis in horizontal writing modes.
        matches!(style.direction, Direction::Ltr)
      };
      if let Some(pos) = snap_position(
        align_x,
        abs_bounds.x(),
        abs_bounds.max_x(),
        viewport.width,
        padding_start,
        padding_end,
        margin_start,
        margin_end,
        axis_positive,
      ) {
        targets_x.push((pos, style.scroll_snap_stop));
      }
    }
    if snap_y {
      let margin_start = resolve_snap_length(style.scroll_margin_top, viewport.height);
      let margin_end = resolve_snap_length(style.scroll_margin_bottom, viewport.height);
      let (padding_start, padding_end) = padding_y;
      let align_y = if inline_vertical {
        style.scroll_snap_align.inline
      } else {
        style.scroll_snap_align.block
      };
      let axis_positive = if inline_vertical {
        // Physical y maps to the inline axis in vertical writing modes.
        matches!(style.direction, Direction::Ltr)
      } else {
        // Physical y maps to the block axis in horizontal writing modes.
        true
      };
      if let Some(pos) = snap_position(
        align_y,
        abs_bounds.y(),
        abs_bounds.max_y(),
        viewport.height,
        padding_start,
        padding_end,
        margin_start,
        margin_end,
        axis_positive,
      ) {
        targets_y.push((pos, style.scroll_snap_stop));
      }
    }
  }

  let child_offset = Point::new(abs_bounds.x(), abs_bounds.y());
  for child in &node.children {
    collect_snap_targets(
      child,
      child_offset,
      inline_vertical,
      snap_x,
      snap_y,
      viewport,
      padding_x,
      padding_y,
      bounds,
      targets_x,
      targets_y,
    );
  }
}

pub(crate) fn find_snap_container<'a>(
  node: &'a FragmentNode,
  origin: Point,
) -> Option<(&'a FragmentNode, &'a ComputedStyle, Point)> {
  if let Some(style) = node.style.as_ref() {
    if style.scroll_snap_type.axis != ScrollSnapAxis::None {
      return Some((node, style, origin));
    }
  }

  for child in &node.children {
    let child_origin = Point::new(origin.x + child.bounds.x(), origin.y + child.bounds.y());
    if let Some(found) = find_snap_container(child, child_origin) {
      return Some(found);
    }
  }

  None
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::{Point, Rect, Size};
  use crate::tree::fragment_tree::FragmentContent;
  use std::sync::Arc;

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

    let snapped = apply_scroll_snap(
      &mut tree,
      &ScrollState::with_viewport(Point::new(90.0, 160.0)),
    )
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

    let target =
      FragmentNode::new_block_styled(Rect::from_xywh(80.0, 0.0, 80.0, 50.0), vec![], target_style);

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

  fn make_vertical_nested(inner_behavior: OverscrollBehavior) -> FragmentNode {
    let mut inner_style = ComputedStyle::default();
    inner_style.overflow_y = Overflow::Scroll;
    inner_style.overscroll_behavior_y = inner_behavior;

    let inner_child = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 300.0), vec![]);
    let inner = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![inner_child],
      Arc::new(inner_style),
    );

    let mut outer_style = ComputedStyle::default();
    outer_style.overflow_y = Overflow::Scroll;

    let trailing = FragmentNode::new_block(Rect::from_xywh(0.0, 250.0, 100.0, 150.0), vec![]);
    FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![inner, trailing],
      Arc::new(outer_style),
    )
  }

  #[test]
  fn overscroll_contain_blocks_chaining() {
    let outer = make_vertical_nested(OverscrollBehavior::Contain);
    let mut chain = build_scroll_chain(&outer, Size::new(100.0, 100.0), &[0]);
    let result = apply_scroll_chain(&mut chain, Point::new(0.0, 400.0), ScrollOptions::default());

    assert_eq!(chain.len(), 2, "inner and outer should both participate");
    assert!(
      (chain[0].scroll.y - 200.0).abs() < 1e-3,
      "inner should clamp to max"
    );
    assert!(
      chain[1].scroll.y.abs() < 1e-3,
      "outer should not chain past contain"
    );
    assert!(
      result.remaining.y.abs() < 1e-3,
      "no scroll should leak past outer"
    );
  }

  #[test]
  fn overscroll_auto_chains_to_parent() {
    let outer = make_vertical_nested(OverscrollBehavior::Auto);
    let mut chain = build_scroll_chain(&outer, Size::new(100.0, 100.0), &[0]);
    let result = apply_scroll_chain(&mut chain, Point::new(0.0, 400.0), ScrollOptions::default());

    assert!(
      (chain[0].scroll.y - 200.0).abs() < 1e-3,
      "inner should still clamp"
    );
    assert!(
      chain[1].scroll.y > 0.0,
      "outer should receive chained delta"
    );
    assert!(
      result.remaining.y.abs() < 1e-3,
      "all scroll should be consumed"
    );
  }

  #[test]
  fn overscroll_none_suppresses_indicator() {
    let outer = make_vertical_nested(OverscrollBehavior::None);
    let mut chain = build_scroll_chain(&outer, Size::new(100.0, 100.0), &[0]);
    let mut options = ScrollOptions::default();
    options.simulate_overscroll = true;
    let result = apply_scroll_chain(&mut chain, Point::new(0.0, 300.0), options);

    assert!(
      result.overscroll[0].y.abs() < 1e-3,
      "overscroll glow should be suppressed"
    );
    assert!(
      chain[1].scroll.y.abs() < 1e-3,
      "outer should not chain when none is set"
    );
  }

  #[test]
  fn horizontal_contain_respects_rtl_and_vertical_writing() {
    let mut inner_style = ComputedStyle::default();
    inner_style.overflow_x = Overflow::Scroll;
    inner_style.overscroll_behavior_x = OverscrollBehavior::Contain;
    inner_style.writing_mode = WritingMode::VerticalRl;

    let inner_child = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 300.0, 100.0), vec![]);
    let inner = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![inner_child],
      Arc::new(inner_style),
    );

    let mut outer_style = ComputedStyle::default();
    outer_style.overflow_x = Overflow::Scroll;
    outer_style.direction = Direction::Rtl;

    let sibling = FragmentNode::new_block(Rect::from_xywh(220.0, 0.0, 200.0, 100.0), vec![]);
    let outer = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![inner, sibling],
      Arc::new(outer_style),
    );

    let mut chain = build_scroll_chain(&outer, Size::new(100.0, 100.0), &[0]);
    let result = apply_scroll_chain(&mut chain, Point::new(400.0, 0.0), ScrollOptions::default());

    assert!(
      (chain[0].scroll.x - 200.0).abs() < 1e-3,
      "inner should clamp on x"
    );
    assert!(
      chain[1].scroll.x.abs() < 1e-3,
      "outer should not receive chained x scroll"
    );
    assert!(
      result.remaining.x.abs() < 1e-3,
      "scroll should stop at contained edge"
    );
  }

  #[test]
  fn chained_scroll_still_snaps_outer_container() {
    let mut inner_style = ComputedStyle::default();
    inner_style.overflow_y = Overflow::Scroll;
    inner_style.scroll_snap_align.block = ScrollSnapAlign::Start;
    inner_style.scroll_snap_align.inline = ScrollSnapAlign::Start;

    let inner_child = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 120.0), vec![]);
    let inner = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![inner_child],
      Arc::new(inner_style),
    );

    let mut snap_child_style = ComputedStyle::default();
    snap_child_style.scroll_snap_align.block = ScrollSnapAlign::Start;
    snap_child_style.scroll_snap_align.inline = ScrollSnapAlign::Start;

    let second = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 200.0, 100.0, 200.0),
      vec![],
      Arc::new(snap_child_style.clone()),
    );

    let mut outer_style = ComputedStyle::default();
    outer_style.overflow_y = Overflow::Scroll;
    outer_style.scroll_snap_type.axis = ScrollSnapAxis::Y;
    outer_style.scroll_snap_type.strictness = ScrollSnapStrictness::Mandatory;

    let outer = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![
        FragmentNode::new_block_styled(
          Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
          vec![FragmentNode::new_block(
            Rect::from_xywh(0.0, 0.0, 100.0, 120.0),
            vec![],
          )],
          Arc::new(ComputedStyle {
            scroll_snap_align: snap_child_style.scroll_snap_align,
            overflow_y: Overflow::Scroll,
            ..ComputedStyle::default()
          }),
        ),
        inner,
        second,
      ],
      Arc::new(outer_style),
    );

    let mut chain = build_scroll_chain(&outer, Size::new(100.0, 100.0), &[1]);
    let result = apply_scroll_chain(&mut chain, Point::new(0.0, 180.0), ScrollOptions::default());

    assert!(result.remaining.y.abs() < 1e-3);
    assert!(
      (chain[1].scroll.y - 200.0).abs() < 1e-2,
      "outer should snap to next item"
    );
  }
}
