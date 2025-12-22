//! Scroll-driven animation utilities.
//!
//! This module provides lightweight timeline evaluation for scroll and view
//! timelines along with keyframe sampling helpers. It is intentionally small
//! and self contained so it can be reused by layout/paint and tests without
//! wiring a full animation engine.

use std::collections::HashMap;

use crate::css::types::{Declaration, Keyframe, KeyframesRule};
use crate::css::types::PropertyValue;
use crate::style::inline_axis_is_horizontal;
use crate::style::types::AnimationRange;
use crate::style::types::RangeOffset;
use crate::style::types::ScrollTimeline;
use crate::style::types::TimelineAxis;
use crate::style::types::TimelineOffset;
use crate::style::types::ViewTimeline;
use crate::style::types::ViewTimelinePhase;
use crate::style::types::WritingMode;
use crate::style::values::Length;
use crate::style::ComputedStyle;

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
