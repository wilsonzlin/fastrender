use std::collections::HashMap;
use std::sync::Arc;

use fastrender::animation::apply_scroll_driven_animations;
use fastrender::css::types::{Declaration, Keyframe, KeyframesRule, PropertyValue};
use fastrender::geometry::{Point, Rect, Size};
use fastrender::style::types::{AnimationRange, AnimationTimeline, TimelineAxis, ViewTimeline};
use fastrender::style::ComputedStyle;
use fastrender::tree::fragment_tree::{FragmentNode, FragmentTree};

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

fn animated_style(timeline: &str, animation: &str) -> Arc<ComputedStyle> {
  let mut style = ComputedStyle::default();
  style.view_timelines = vec![ViewTimeline {
    name: Some(timeline.to_string()),
    axis: TimelineAxis::Block,
  }];
  style.animation_timelines = vec![AnimationTimeline::Named(timeline.to_string())];
  style.animation_ranges = vec![AnimationRange::default()];
  style.animation_names = vec![animation.to_string()];
  Arc::new(style)
}

#[test]
fn view_timeline_accounts_for_translated_fragment_root() {
  let animation_name = "fade";
  let timeline_name = "view";

  let mut target = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 50.0), vec![]);
  target.style = Some(animated_style(timeline_name, animation_name));

  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![]);
  let translated_root =
    FragmentNode::new_block(Rect::from_xywh(0.0, 200.0, 100.0, 100.0), vec![target]);
  let mut tree = FragmentTree::from_fragments(vec![root, translated_root], Size::new(100.0, 100.0));

  let mut keyframes = HashMap::new();
  keyframes.insert(animation_name.to_string(), fade_keyframes(animation_name));
  tree.keyframes = keyframes;

  apply_scroll_driven_animations(&mut tree, Point::ZERO);

  let animated = tree.additional_fragments[0].children[0]
    .style
    .as_ref()
    .expect("animated style present");
  assert!(
    animated.opacity < 0.05,
    "opacity should remain at start when fragment is translated: {}",
    animated.opacity
  );
}
