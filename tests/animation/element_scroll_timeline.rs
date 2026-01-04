use std::collections::HashMap;
use std::sync::Arc;

use fastrender::animation::apply_scroll_driven_animations;
use fastrender::css::types::{Declaration, Keyframe, KeyframesRule, PropertyValue};
use fastrender::geometry::{Point, Rect, Size};
use fastrender::scroll::ScrollState;
use fastrender::style::types::TransitionTimingFunction;
use fastrender::style::types::{
  AnimationDirection, AnimationIterationCount, AnimationRange, AnimationTimeline, ScrollTimeline,
  StepPosition, TimelineAxis,
};
use fastrender::style::ComputedStyle;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};

fn fade_keyframes(name: &str) -> KeyframesRule {
  KeyframesRule {
    name: name.to_string(),
    keyframes: vec![
      Keyframe {
        offset: 0.0,
        declarations: vec![Declaration {
          property: "opacity".into(),
          value: PropertyValue::Number(0.0),
          contains_var: false,
          raw_value: String::new(),
          important: false,
        }],
      },
      Keyframe {
        offset: 1.0,
        declarations: vec![Declaration {
          property: "opacity".into(),
          value: PropertyValue::Number(1.0),
          contains_var: false,
          raw_value: String::new(),
          important: false,
        }],
      },
    ],
  }
}

fn opacity_for_scroll(
  animated_style: &Arc<ComputedStyle>,
  scroller_style: &Arc<ComputedStyle>,
  scroll_y: f32,
) -> f32 {
  let animation_name = animated_style
    .animation_names
    .first()
    .expect("animated style should contain animation name")
    .clone();

  let animated = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 0.0, 50.0, 20.0),
    FragmentContent::Block { box_id: None },
    vec![],
    Arc::clone(animated_style),
  );

  let mut scroller = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 0.0, 50.0, 100.0),
    FragmentContent::Block { box_id: Some(1) },
    vec![animated],
    Arc::clone(scroller_style),
  );
  scroller.scroll_overflow = Rect::from_xywh(0.0, 0.0, 50.0, 200.0);

  let root = FragmentNode::new(
    Rect::from_xywh(0.0, 0.0, 50.0, 100.0),
    FragmentContent::Block { box_id: None },
    vec![scroller],
  );
  let mut tree = FragmentTree::with_viewport(root, Size::new(50.0, 100.0));
  tree
    .keyframes
    .insert(animation_name.clone(), fade_keyframes(&animation_name));

  let scroll_state = ScrollState::from_parts(
    Point::ZERO,
    HashMap::from([(1usize, Point::new(0.0, scroll_y))]),
  );

  apply_scroll_driven_animations(&mut tree, &scroll_state);

  tree.root.children[0].children[0]
    .style
    .as_ref()
    .expect("animated style present")
    .opacity
}

#[test]
fn scroll_timeline_uses_element_scroll_offsets() {
  let animation_name = "fade";
  let timeline_name = "scroller";

  let mut scroller_style = ComputedStyle::default();
  scroller_style.scroll_timelines = vec![ScrollTimeline {
    name: Some(timeline_name.to_string()),
    axis: TimelineAxis::Block,
    ..ScrollTimeline::default()
  }];
  let scroller_style = Arc::new(scroller_style);

  let mut animated_style = ComputedStyle::default();
  animated_style.animation_names = vec![animation_name.to_string()];
  animated_style.animation_ranges = vec![AnimationRange::default()];
  animated_style.animation_timelines = vec![AnimationTimeline::Named(timeline_name.to_string())];
  animated_style.animation_timing_functions = vec![TransitionTimingFunction::Linear].into();
  let animated_style = Arc::new(animated_style);

  assert!(
    (opacity_for_scroll(&animated_style, &scroller_style, 50.0) - 0.5).abs() < 0.05,
    "opacity should reflect element scroll timeline progress"
  );
}

#[test]
fn scroll_timeline_respects_iteration_and_direction() {
  let animation_name = "fade";
  let timeline_name = "scroller";

  let mut scroller_style = ComputedStyle::default();
  scroller_style.scroll_timelines = vec![ScrollTimeline {
    name: Some(timeline_name.to_string()),
    axis: TimelineAxis::Block,
    ..ScrollTimeline::default()
  }];
  let scroller_style = Arc::new(scroller_style);

  let mut animated_style = ComputedStyle::default();
  animated_style.animation_names = vec![animation_name.to_string()];
  animated_style.animation_ranges = vec![AnimationRange::default()];
  animated_style.animation_timelines = vec![AnimationTimeline::Named(timeline_name.to_string())];
  animated_style.animation_iteration_counts = vec![AnimationIterationCount::Count(2.0)].into();
  animated_style.animation_directions = vec![AnimationDirection::Alternate].into();
  animated_style.animation_timing_functions = vec![TransitionTimingFunction::Linear].into();
  let animated_style = Arc::new(animated_style);

  // With 2 iterations, raw timeline progress spans both iterations.
  // - raw=0.25 -> 0.5 through the first iteration.
  // - raw=0.75 -> 0.5 through the second iteration.
  assert!(
    (opacity_for_scroll(&animated_style, &scroller_style, 25.0) - 0.5).abs() < 0.05,
    "expected 0.25 raw progress to map into first iteration"
  );
  assert!(
    (opacity_for_scroll(&animated_style, &scroller_style, 75.0) - 0.5).abs() < 0.05,
    "expected 0.75 raw progress to map into second iteration"
  );

  // Direction alternate reverses the second iteration; raw=0.6 lands in the second iteration
  // with directed progress 0.8 (instead of 0.2 without reversal).
  assert!(
    (opacity_for_scroll(&animated_style, &scroller_style, 60.0) - 0.8).abs() < 0.05,
    "expected alternate direction to reverse the second iteration"
  );
}

#[test]
fn scroll_timeline_respects_steps_timing_function() {
  let animation_name = "fade";
  let timeline_name = "scroller";

  let mut scroller_style = ComputedStyle::default();
  scroller_style.scroll_timelines = vec![ScrollTimeline {
    name: Some(timeline_name.to_string()),
    axis: TimelineAxis::Block,
    ..ScrollTimeline::default()
  }];
  let scroller_style = Arc::new(scroller_style);

  let mut animated_style = ComputedStyle::default();
  animated_style.animation_names = vec![animation_name.to_string()];
  animated_style.animation_ranges = vec![AnimationRange::default()];
  animated_style.animation_timelines = vec![AnimationTimeline::Named(timeline_name.to_string())];
  animated_style.animation_timing_functions =
    vec![TransitionTimingFunction::Steps(2, StepPosition::End)].into();
  let animated_style = Arc::new(animated_style);

  assert!(
    (opacity_for_scroll(&animated_style, &scroller_style, 25.0) - 0.0).abs() < 0.05,
    "expected steps(2, end) to hold at 0 until halfway"
  );
  assert!(
    (opacity_for_scroll(&animated_style, &scroller_style, 75.0) - 0.5).abs() < 0.05,
    "expected steps(2, end) to advance to the second step"
  );
}
