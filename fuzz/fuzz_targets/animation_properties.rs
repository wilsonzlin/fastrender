#![no_main]

use arbitrary::Arbitrary;
use fastrender::animation::{apply_animated_properties, sample_keyframes};
use fastrender::css::parser::parse_stylesheet_with_errors;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaContext;
use libfuzzer_sys::fuzz_target;
use std::cmp::max;

const MAX_FRAMES: usize = 4;

#[derive(Debug, Arbitrary)]
struct AnimationCase {
  name_seed: String,
  progress: f32,
  viewport: (u16, u16),
  element_size: (u16, u16),
  duration_ms: u16,
  delay_ms: i16,
  transition_ms: u16,
  transition_delay_ms: i16,
  frames: Vec<FrameSpec>,
}

#[derive(Debug, Arbitrary, Clone)]
struct FrameSpec {
  opacity: f32,
  translate: (f32, f32),
  scale: f32,
  color: (u8, u8, u8),
  blur: f32,
  radius: f32,
  background: (f32, f32),
  shadow: f32,
  angle: f32,
}

fn sanitize_ident(raw: &str, fallback: &str) -> String {
  let filtered: String = raw
    .chars()
    .filter(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_'))
    .take(32)
    .collect();
  if filtered.is_empty() {
    fallback.to_string()
  } else {
    filtered
  }
}

fn clamp_pct(v: f32) -> f32 {
  if v.is_finite() {
    v.clamp(-50.0, 150.0)
  } else {
    0.0
  }
}

fn clamp_px(v: f32, limit: f32) -> f32 {
  if v.is_finite() {
    v.clamp(-limit, limit)
  } else {
    0.0
  }
}

fn frame_declarations(frame: &FrameSpec) -> String {
  let opacity = frame.opacity.clamp(0.0, 2.0);
  let tx = clamp_px(frame.translate.0, 256.0);
  let ty = clamp_px(frame.translate.1, 256.0);
  let scale = if frame.scale.is_finite() {
    frame.scale.abs().clamp(0.1, 4.0)
  } else {
    1.0
  };
  let blur = frame.blur.abs().min(30.0);
  let radius = frame.radius.abs().min(200.0);
  let bg_x = clamp_pct(frame.background.0);
  let bg_y = clamp_pct(frame.background.1);
  let shadow = frame.shadow.abs().min(40.0);
  let angle = clamp_px(frame.angle, 360.0);
  format!(
    "opacity: {opacity:.3};\
     transform: translate({tx:.1}px, {ty:.1}px) scale({scale:.2}) rotate({angle:.1}deg);\
     background-color: rgba({},{},{},0.8);\
     background-position: {bg_x:.1}% {bg_y:.1}%;\
     background-size: {}% {}%;\
     filter: blur({blur:.2}px) brightness(0.8) drop-shadow({shadow:.1}px {shadow:.1}px {shadow:.1}px rgba(0,0,0,0.4));\
     border-radius: {radius:.1}px {radius:.1}px {radius:.1}px {radius:.1}px;\
     clip-path: inset(5% 5% 5% 5% round {radius:.1}px);",
    frame.color.0,
    frame.color.1,
    frame.color.2,
    (bg_x.abs().min(150.0) + 10.0) as i32,
    (bg_y.abs().min(150.0) + 20.0) as i32,
  )
}

fn pick_frame<'a>(frames: &'a [FrameSpec], idx: usize) -> FrameSpec {
  if frames.is_empty() {
    FrameSpec {
      opacity: 1.0,
      translate: (0.0, 0.0),
      scale: 1.0,
      color: (200, 80, 120),
      blur: 2.0,
      radius: 8.0,
      background: (50.0, 50.0),
      shadow: 2.0,
      angle: 0.0,
    }
  } else {
    frames[idx % frames.len()].clone()
  }
}

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .map(|v| v.eq_ignore_ascii_case(id))
    .unwrap_or(false)
  {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

fn build_animation_css(input: &AnimationCase) -> (String, String) {
  let name = sanitize_ident(&input.name_seed, "spin");
  let frames: Vec<FrameSpec> = input.frames.iter().take(MAX_FRAMES).cloned().collect();
  let frame0 = pick_frame(&frames, 0);
  let frame_mid = pick_frame(&frames, 1);
  let frame_end = pick_frame(&frames, max(2, frames.len().saturating_sub(1)));
  let duration = (input.duration_ms as u32).max(50).min(5_000);
  let delay = input.delay_ms.clamp(-1000, 2000);
  let transition = (input.transition_ms as u32).max(10).min(3_000);
  let transition_delay = input.transition_delay_ms.clamp(-1000, 2000);
  let timing = [
    "linear",
    "ease",
    "ease-in",
    "ease-out",
    "ease-in-out",
    "steps(4, end)",
  ];
  let direction = ["normal", "reverse", "alternate", "alternate-reverse"];
  let timing_fn = timing[(input.name_seed.len() + timing.len()) % timing.len()];
  let direction_val = direction[(frames.len() + direction.len()) % direction.len()];
  let keyframes = format!(
    "@keyframes {name} {{\
       0% {{ {} }}\
       50% {{ {} }}\
       100% {{ {} }}\
     }}",
    frame_declarations(&frame0),
    frame_declarations(&frame_mid),
    frame_declarations(&frame_end)
  );
  let style = format!(
    "#target {{\
       animation-name: {name};\
       animation-duration: {duration}ms;\
       animation-delay: {delay}ms;\
       animation-iteration-count: {};\
       animation-direction: {direction_val};\
       animation-fill-mode: both;\
       animation-timing-function: {timing_fn};\
       transition-property: opacity, transform, filter, background-color;\
       transition-duration: {transition}ms;\
       transition-delay: {transition_delay}ms;\
       transition-timing-function: ease-in-out;\
     }}",
    1 + (frames.len() % 4)
  );
  let css = format!("{style}{keyframes}");
  (css, name)
}

fuzz_target!(|input: AnimationCase| {
  let (css, name) = build_animation_css(&input);
  let parsed = parse_stylesheet_with_errors(&css);

  // Use a stable, small DOM so selector matching/cascade stays bounded.
  let dom = dom::parse_html("<div id=\"target\"></div>").unwrap_or_else(|_| dom::DomNode {
    node_type: dom::DomNodeType::Document,
    children: vec![dom::DomNode {
      node_type: dom::DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: dom::HTML_NAMESPACE.to_string(),
        attributes: vec![("id".to_string(), "target".to_string())],
      },
      children: vec![],
    }],
  });

  let viewport_width = (input.viewport.0 as f32).max(64.0).min(2048.0);
  let viewport_height = (input.viewport.1 as f32).max(64.0).min(2048.0);
  let media = MediaContext::screen(viewport_width, viewport_height);
  let styled = apply_styles_with_media(&dom, &parsed.stylesheet, &media);
  let Some(target) = find_by_id(&styled, "target") else {
    return;
  };

  let mut style = target.styles.clone();
  let element_size = fastrender::Size::new(
    (input.element_size.0 as f32).max(8.0).min(1024.0),
    (input.element_size.1 as f32).max(8.0).min(1024.0),
  );
  let progress = if input.progress.is_finite() {
    input.progress.clamp(-0.5, 1.5)
  } else {
    0.5
  };

  let keyframes = parsed.stylesheet.collect_keyframes(&media);
  if let Some(rule) = keyframes
    .iter()
    .find(|r| r.name == name)
    .or_else(|| keyframes.first())
  {
    let sampled = sample_keyframes(
      rule,
      progress,
      &style,
      fastrender::Size::new(viewport_width, viewport_height),
      element_size,
    );
    apply_animated_properties(&mut style, &sampled);
  }
});
