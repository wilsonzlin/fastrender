use std::collections::HashMap;
use std::sync::Arc;

use fastrender::animation::apply_scroll_driven_animations;
use fastrender::css::types::{Declaration, Keyframe, KeyframesRule, PropertyValue};
use fastrender::geometry::{Point, Rect, Size};
use fastrender::scroll::ScrollState;
use fastrender::style::types::{AnimationRange, AnimationTimeline, ScrollTimeline, TimelineAxis};
use fastrender::style::ComputedStyle;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};

fn fade_keyframes(name: &str) -> KeyframesRule {
  KeyframesRule {
    name: name.to_string(),
    keyframes: vec![
      Keyframe {
        offset: 0.0,
        declarations: vec![Declaration {
          property: "opacity".to_string(),
          value: PropertyValue::Number(0.0),
          raw_value: String::new(),
          important: false,
        }],
      },
      Keyframe {
        offset: 1.0,
        declarations: vec![Declaration {
          property: "opacity".to_string(),
          value: PropertyValue::Number(1.0),
          raw_value: String::new(),
          important: false,
        }],
      },
    ],
  }
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
  let animated_style = Arc::new(animated_style);

  let animated = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 0.0, 50.0, 20.0),
    FragmentContent::Block { box_id: None },
    vec![],
    animated_style,
  );

  let mut scroller = FragmentNode::new_with_style(
    Rect::from_xywh(0.0, 0.0, 50.0, 100.0),
    FragmentContent::Block { box_id: Some(1) },
    vec![animated],
    scroller_style,
  );
  scroller.scroll_overflow = Rect::from_xywh(0.0, 0.0, 50.0, 200.0);

  let root = FragmentNode::new(
    Rect::from_xywh(0.0, 0.0, 50.0, 100.0),
    FragmentContent::Block { box_id: None },
    vec![scroller],
  );
  let mut tree = FragmentTree::with_viewport(root, Size::new(50.0, 100.0));
  let mut keyframes = HashMap::new();
  keyframes.insert(animation_name.to_string(), fade_keyframes(animation_name));
  tree.keyframes = keyframes;

  let scroll_state = ScrollState::from_parts(
    Point::ZERO,
    HashMap::from([(1usize, Point::new(0.0, 50.0))]),
  );

  apply_scroll_driven_animations(&mut tree, &scroll_state);

  let animated_fragment = &tree.root.children[0].children[0];
  let opacity = animated_fragment
    .style
    .as_ref()
    .expect("animated style present")
    .opacity;
  assert!(
    (opacity - 0.5).abs() < 0.05,
    "opacity should reflect element scroll timeline progress, got {}",
    opacity
  );
}
