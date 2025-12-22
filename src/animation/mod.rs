//! Scroll-driven animation utilities.
//!
//! This module provides lightweight timeline evaluation for scroll and view
//! timelines along with keyframe sampling helpers. It is intentionally small
//! and self contained so it can be reused by layout/paint and tests without
//! wiring a full animation engine.

use std::collections::HashMap;

use crate::css::types::{Declaration, Keyframe, KeyframesRule};
use crate::css::types::PropertyValue;
use crate::geometry::{Point, Rect};
use crate::style::inline_axis_is_horizontal;
use crate::style::types::AnimationRange;
use crate::style::types::RangeOffset;
use crate::style::types::ScrollTimeline;
use crate::style::types::TimelineAxis;
use crate::style::types::TimelineOffset;
use crate::style::types::ViewTimeline;
use crate::style::types::ViewTimelinePhase;
use crate::style::types::AnimationTimeline;
use crate::style::types::WritingMode;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::tree::fragment_tree::{FragmentNode, FragmentTree};
use std::sync::Arc;

fn axis_is_horizontal(axis: TimelineAxis, writing_mode: WritingMode) -> bool {
  match axis {
    TimelineAxis::X => true,
    TimelineAxis::Y => false,
    TimelineAxis::Inline => inline_axis_is_horizontal(writing_mode),
    TimelineAxis::Block => crate::style::block_axis_is_horizontal(writing_mode),
  }
}

fn resolve_offset_value(
  offset: &TimelineOffset,
  scroll_range: f32,
  viewport_size: f32,
  is_end: bool,
) -> f32 {
  match offset {
    TimelineOffset::Auto => {
      if is_end {
        scroll_range.max(0.0)
      } else {
        0.0
      }
    }
    TimelineOffset::Length(len) => len
      .resolve_against(scroll_range)
      .unwrap_or_else(|| len.to_px().max(0.0)),
    TimelineOffset::Percentage(pct) => (pct / 100.0) * scroll_range,
  }
  .clamp(0.0, scroll_range.max(0.0).max(viewport_size))
}

fn resolve_progress_offset(offset: &RangeOffset, base_start: f32, base_end: f32, view_size: f32, phases: Option<(f32, f32, f32)>) -> f32 {
  match offset {
    RangeOffset::Progress(p) => base_start + (base_end - base_start) * p.clamp(0.0, 1.0),
    RangeOffset::View(phase, adj) => {
      let Some((entry, cross, exit)) = phases else {
        return base_start;
      };
      let base = match phase {
        ViewTimelinePhase::Entry => entry,
        ViewTimelinePhase::Cross => cross,
        ViewTimelinePhase::Exit => exit,
      };
      base + view_size * adj
    }
  }
}

fn clamp_progress(value: f32) -> f32 {
  value.clamp(0.0, 1.0)
}

/// Computes scroll timeline progress given the current scroll offset and
/// resolved geometry.
pub fn scroll_timeline_progress(
  timeline: &ScrollTimeline,
  scroll_position: f32,
  scroll_range: f32,
  viewport_size: f32,
  range: &AnimationRange,
) -> f32 {
  let start_base = resolve_offset_value(&timeline.start, scroll_range, viewport_size, false);
  let end_base = resolve_offset_value(&timeline.end, scroll_range, viewport_size, true);
  let start = resolve_progress_offset(range.start(), start_base, end_base, viewport_size, None);
  let end = resolve_progress_offset(range.end(), start_base, end_base, viewport_size, None);
  if (end - start).abs() < f32::EPSILON {
    return 0.0;
  }
  clamp_progress((scroll_position - start) / (end - start))
}

/// Computes view timeline progress using the target position relative to the
/// containing scroll port.
pub fn view_timeline_progress(
  _timeline: &ViewTimeline,
  target_start: f32,
  target_end: f32,
  view_size: f32,
  scroll_offset: f32,
  range: &AnimationRange,
) -> f32 {
  let entry = target_start - view_size;
  let exit = target_end;
  let cross = (target_start + target_end) * 0.5 - view_size * 0.5;
  let start_base = entry;
  let end_base = exit;
  let phases = Some((entry, cross, exit));
  let start = resolve_progress_offset(range.start(), start_base, end_base, view_size, phases);
  let end = resolve_progress_offset(range.end(), start_base, end_base, view_size, phases);
  if (end - start).abs() < f32::EPSILON {
    return clamp_progress(if scroll_offset >= end { 1.0 } else { 0.0 });
  }
  clamp_progress((scroll_offset - start) / (end - start))
}

/// Determines the scroll position and range along the requested axis given
/// container and content sizes. The returned tuple is `(position, range, size)`.
pub fn axis_scroll_state(
  axis: TimelineAxis,
  writing_mode: WritingMode,
  scroll_x: f32,
  scroll_y: f32,
  view_width: f32,
  view_height: f32,
  content_width: f32,
  content_height: f32,
) -> (f32, f32, f32) {
  let horizontal = axis_is_horizontal(axis, writing_mode);
  if horizontal {
    let range = (content_width - view_width).max(0.0);
    (scroll_x.clamp(0.0, range), range, view_width)
  } else {
    let range = (content_height - view_height).max(0.0);
    (scroll_y.clamp(0.0, range), range, view_height)
  }
}

fn declarations_to_map(decls: &[Declaration]) -> HashMap<String, PropertyValue> {
  let mut map = HashMap::new();
  for decl in decls {
    map.insert(decl.property.to_ascii_lowercase(), decl.value.clone());
  }
  map
}

fn interpolate_values(a: &PropertyValue, b: &PropertyValue, t: f32) -> Option<PropertyValue> {
  match (a, b) {
    (PropertyValue::Number(x), PropertyValue::Number(y)) => {
      Some(PropertyValue::Number(x + (y - x) * t))
    }
    (PropertyValue::Length(la), PropertyValue::Length(lb)) => match (la.unit, lb.unit) {
      (ua, ub) if ua == ub => Some(PropertyValue::Length(Length::new(
        la.value + (lb.value - la.value) * t,
        ua,
      ))),
      _ => {
        let ax = la.to_px();
        let bx = lb.to_px();
        Some(PropertyValue::Length(Length::px(ax + (bx - ax) * t)))
      }
    },
    _ => None,
  }
}

/// Samples a @keyframes rule at the given progress, returning a property map of
/// interpolated values.
pub fn sample_keyframes(rule: &KeyframesRule, progress: f32) -> HashMap<String, PropertyValue> {
  let mut frames = rule.keyframes.clone();
  if frames.is_empty() {
    return HashMap::new();
  }
  frames.sort_by(|a, b| a
    .offset
    .partial_cmp(&b.offset)
    .unwrap_or(std::cmp::Ordering::Equal));
  let progress = clamp_progress(progress);

  let mut prev: &Keyframe = &frames[0];
  let mut next: &Keyframe = &frames[frames.len() - 1];
  for frame in &frames {
    if frame.offset <= progress + f32::EPSILON {
      prev = frame;
    }
    if frame.offset + f32::EPSILON >= progress {
      next = frame;
      break;
    }
  }

  let start = prev.offset;
  let end = next.offset;
  let local_t = if end - start > f32::EPSILON {
    (progress - start) / (end - start)
  } else {
    0.0
  };

  let from = declarations_to_map(&prev.declarations);
  let to = declarations_to_map(&next.declarations);
  let mut result = HashMap::new();

  for (prop, from_val) in &from {
    if let Some(to_val) = to.get(prop) {
      if let Some(interp) = interpolate_values(from_val, to_val, local_t) {
        result.insert(prop.clone(), interp);
        continue;
      }
    }
    result.insert(prop.clone(), from_val.clone());
  }

  for (prop, to_val) in to {
    result.entry(prop).or_insert(to_val);
  }

  result
}

/// Applies a subset of animated properties to the computed style.
pub fn apply_animated_properties(style: &mut ComputedStyle, values: &HashMap<String, PropertyValue>) {
  if let Some(val) = values.get("opacity") {
    if let PropertyValue::Number(n) = val {
      style.opacity = clamp_progress(*n);
    }
  }
}

enum TimelineState {
  Scroll {
    timeline: ScrollTimeline,
    scroll_pos: f32,
    scroll_range: f32,
    viewport_size: f32,
  },
  View {
    timeline: ViewTimeline,
    target_start: f32,
    target_end: f32,
    view_size: f32,
    scroll_offset: f32,
  },
}

fn pick<'a, T: Clone>(list: &'a [T], idx: usize, default: T) -> T {
  if list.is_empty() {
    return default;
  }
  list
    .get(idx)
    .cloned()
    .unwrap_or_else(|| list.last().cloned().unwrap_or(default))
}

fn collect_timelines(
  node: &FragmentNode,
  offset: Point,
  viewport: Rect,
  content: Rect,
  scroll: Point,
  map: &mut HashMap<String, TimelineState>,
) {
  let abs = Rect::from_xywh(
    node.bounds.x() + offset.x,
    node.bounds.y() + offset.y,
    node.bounds.width(),
    node.bounds.height(),
  );

  if let Some(style) = node.style.as_ref() {
    for tl in &style.scroll_timelines {
      if let Some(name) = &tl.name {
        let (scroll_pos, scroll_range, viewport_size) = axis_scroll_state(
          tl.axis,
          style.writing_mode,
          scroll.x,
          scroll.y,
          viewport.width(),
          viewport.height(),
          content.width(),
          content.height(),
        );
        map.entry(name.clone()).or_insert(TimelineState::Scroll {
          timeline: tl.clone(),
          scroll_pos,
          scroll_range,
          viewport_size,
        });
      }
    }

    for tl in &style.view_timelines {
      if let Some(name) = &tl.name {
        let horizontal = axis_is_horizontal(tl.axis, style.writing_mode);
        let target_start = if horizontal { abs.x() } else { abs.y() };
        let target_end = if horizontal {
          abs.x() + abs.width()
        } else {
          abs.y() + abs.height()
        };
        let view_size = if horizontal {
          viewport.width()
        } else {
          viewport.height()
        };
        let scroll_offset = if horizontal { scroll.x } else { scroll.y };
        map.entry(name.clone()).or_insert(TimelineState::View {
          timeline: tl.clone(),
          target_start,
          target_end,
          view_size,
          scroll_offset,
        });
      }
    }
  }

  let child_offset = Point::new(abs.x(), abs.y());
  for child in &node.children {
    collect_timelines(child, child_offset, viewport, content, scroll, map);
  }
}

fn apply_animations_to_node(
  node: &mut FragmentNode,
  offset: Point,
  viewport: Rect,
  scroll: Point,
  keyframes: &HashMap<String, KeyframesRule>,
  timelines: &HashMap<String, TimelineState>,
) {
  let abs = Rect::from_xywh(
    node.bounds.x() + offset.x,
    node.bounds.y() + offset.y,
    node.bounds.width(),
    node.bounds.height(),
  );

  if let Some(style_arc) = node.style.clone() {
    let names = &style_arc.animation_names;
    if !names.is_empty() {
      let timelines_list = &style_arc.animation_timelines;
      let ranges_list = &style_arc.animation_ranges;

      let mut animated = (*style_arc).clone();
      let mut changed = false;

      for (idx, name) in names.iter().enumerate() {
        let timeline_ref = pick(
          timelines_list,
          idx,
          AnimationTimeline::Auto,
        );
        let range = pick(ranges_list, idx, AnimationRange::default());
        let progress = match timeline_ref {
          AnimationTimeline::Auto => None,
          AnimationTimeline::None => None,
          AnimationTimeline::Named(ref timeline_name) => {
            timelines.get(timeline_name).map(|state| match state {
              TimelineState::Scroll {
                timeline,
                scroll_pos,
                scroll_range,
                viewport_size,
              } => scroll_timeline_progress(timeline, *scroll_pos, *scroll_range, *viewport_size, &range),
              TimelineState::View {
                timeline,
                target_start,
                target_end,
                view_size,
                scroll_offset,
              } => view_timeline_progress(
                timeline,
                *target_start,
                *target_end,
                *view_size,
                *scroll_offset,
                &range,
              ),
            })
          }
        };

        let Some(progress) = progress else { continue }; // skip if no timeline
        if let Some(rule) = keyframes.get(name) {
          let values = sample_keyframes(rule, progress);
          apply_animated_properties(&mut animated, &values);
          changed = true;
        }
      }

      if changed {
        node.style = Some(Arc::new(animated));
      }
    }
  }

  let child_offset = Point::new(abs.x(), abs.y());
  for child in &mut node.children {
    apply_animations_to_node(child, child_offset, viewport, scroll, keyframes, timelines);
  }
}

/// Applies scroll/view timeline-driven animations to the fragment tree by sampling
/// matching @keyframes rules and applying animated properties (currently opacity).
///
/// This is a lightweight hook to make scroll-driven effects visible without a full
/// animation engine. It uses the provided scroll offsets for the root viewport.
pub fn apply_scroll_driven_animations(tree: &mut FragmentTree, scroll: Point) {
  if tree.keyframes.is_empty() {
    return;
  }

  let viewport = Rect::from_xywh(0.0, 0.0, tree.viewport_size().width, tree.viewport_size().height);
  let content = tree.content_size();
  let mut timelines: HashMap<String, TimelineState> = HashMap::new();
  collect_timelines(&tree.root, Point::ZERO, viewport, content, scroll, &mut timelines);
  for frag in &tree.additional_fragments {
    collect_timelines(frag, Point::ZERO, viewport, content, scroll, &mut timelines);
  }

  apply_animations_to_node(
    &mut tree.root,
    Point::ZERO,
    viewport,
    scroll,
    &tree.keyframes,
    &timelines,
  );
  for frag in &mut tree.additional_fragments {
    apply_animations_to_node(
      frag,
      Point::ZERO,
      viewport,
      scroll,
      &tree.keyframes,
      &timelines,
    );
  }
}

trait AnimationRangeExt {
  fn start(&self) -> &RangeOffset;
  fn end(&self) -> &RangeOffset;
}

impl AnimationRangeExt for AnimationRange {
  fn start(&self) -> &RangeOffset {
    &self.start
  }

  fn end(&self) -> &RangeOffset {
    &self.end
  }
}
