use fastrender::animation::{
  axis_scroll_state, sample_keyframes, scroll_timeline_progress, view_timeline_progress,
};
use fastrender::css::parser::parse_stylesheet;
use fastrender::css::types::PropertyValue;
use fastrender::dom;
use fastrender::api::FastRender;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaContext;
use fastrender::style::types::{
  AnimationRange, ScrollTimeline, TimelineAxis, TimelineOffset, ViewTimeline, WritingMode,
};

fn find_by_tag<'a>(node: &'a StyledNode, tag: &str) -> Option<&'a StyledNode> {
  if let Some(name) = node.node.tag_name() {
    if name.eq_ignore_ascii_case(tag) {
      return Some(node);
    }
  }
  for child in &node.children {
    if let Some(found) = find_by_tag(child, tag) {
      return Some(found);
    }
  }
  None
}

#[test]
fn parses_timelines_and_keyframes() {
  let css = r#"
    #box {
      scroll-timeline: main block 0% 100%;
      view-timeline: viewy inline;
      animation-timeline: main, viewy;
      animation-range: 20% 80%, entry 0% exit 100%;
      animation-name: fade, move;
    }
    @keyframes fade { from { opacity: 0; } to { opacity: 1; } }
  "#;
  let html = r#"<div id="box"></div>"#;
  let dom = dom::parse_html(html).unwrap();
  let sheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &sheet, &MediaContext::screen(800.0, 600.0));
  let div = find_by_tag(&styled, "div").expect("div present");
  assert_eq!(div.styles.animation_names.len(), 2);
  assert_eq!(div.styles.animation_names[0], "fade");
  assert_eq!(div.styles.animation_timelines.len(), 2);
  assert_eq!(div.styles.animation_ranges.len(), 2);
  assert_eq!(div.styles.scroll_timelines.len(), 1);
  assert_eq!(div.styles.view_timelines.len(), 1);

  let timeline = &div.styles.scroll_timelines[0];
  assert_eq!(timeline.name.as_deref(), Some("main"));
  assert!(matches!(timeline.start, TimelineOffset::Length(_)));
  assert!(matches!(timeline.end, TimelineOffset::Length(_)));
  let view_tl = &div.styles.view_timelines[0];
  assert_eq!(view_tl.name.as_deref(), Some("viewy"));
  assert_eq!(view_tl.axis, TimelineAxis::Inline);

  let keyframes = sheet.collect_keyframes(&MediaContext::screen(800.0, 600.0));
  assert_eq!(keyframes.len(), 1);
  assert_eq!(keyframes[0].name, "fade");
  assert_eq!(keyframes[0].keyframes.len(), 2);
}

#[test]
fn scroll_timeline_progress_tracks_scroll() {
  let timeline = ScrollTimeline::default();
  let range = AnimationRange::default();
  let progress0 = scroll_timeline_progress(&timeline, 0.0, 200.0, 100.0, &range);
  let progress_mid = scroll_timeline_progress(&timeline, 50.0, 200.0, 100.0, &range);
  let progress_end = scroll_timeline_progress(&timeline, 200.0, 200.0, 100.0, &range);
  assert!((progress0 - 0.0).abs() < 1e-6);
  assert!((progress_mid - 0.25).abs() < 1e-6);
  assert!((progress_end - 1.0).abs() < 1e-6);
}

#[test]
fn view_timeline_progress_respects_entry_and_exit() {
  let timeline = ViewTimeline::default();
  let range = AnimationRange::default();
  let progress_start = view_timeline_progress(&timeline, 150.0, 200.0, 100.0, 50.0, &range);
  let progress_mid = view_timeline_progress(&timeline, 150.0, 200.0, 100.0, 125.0, &range);
  let progress_end = view_timeline_progress(&timeline, 150.0, 200.0, 100.0, 200.0, &range);
  assert!((progress_start - 0.0).abs() < 1e-6);
  assert!((progress_mid - 0.5).abs() < 1e-6);
  assert!((progress_end - 1.0).abs() < 1e-6);
}

#[test]
fn keyframes_sample_interpolates_opacity() {
  let sheet = parse_stylesheet("@keyframes fade { 0% { opacity: 0; } 100% { opacity: 1; } }").unwrap();
  let keyframes = sheet.collect_keyframes(&MediaContext::screen(800.0, 600.0));
  let rule = &keyframes[0];
  let sampled = sample_keyframes(rule, 0.25);
  let opacity = match sampled.get("opacity") {
    Some(PropertyValue::Number(n)) => *n,
    other => panic!("unexpected value {other:?}"),
  };
  assert!((opacity - 0.25).abs() < 1e-6);
}

#[test]
fn inline_axis_uses_writing_mode_direction() {
  let timeline = ScrollTimeline {
    axis: TimelineAxis::Inline,
    ..ScrollTimeline::default()
  };
  let range = AnimationRange::default();
  let (scroll_pos, scroll_range, view_size) = axis_scroll_state(
    timeline.axis,
    WritingMode::VerticalRl,
    10.0,
    30.0,
    100.0,
    200.0,
    100.0,
    400.0,
  );
  let progress = scroll_timeline_progress(&timeline, scroll_pos, scroll_range, view_size, &range);
  assert!((scroll_pos - 30.0).abs() < 1e-6);
  assert!((scroll_range - 200.0).abs() < 1e-6);
  assert!(progress > 0.0 && progress < 1.0);
}

#[test]
fn nested_scroll_timelines_progress_independently() {
  let outer = ScrollTimeline {
    axis: TimelineAxis::Block,
    ..ScrollTimeline::default()
  };
  let inner = ScrollTimeline {
    axis: TimelineAxis::Inline,
    ..ScrollTimeline::default()
  };
  let range = AnimationRange::default();

  let (outer_pos, outer_range, outer_size) = axis_scroll_state(
    outer.axis,
    WritingMode::HorizontalTb,
    0.0,
    120.0,
    240.0,
    240.0,
    400.0,
    700.0,
  );
  let (inner_pos, inner_range, inner_size) = axis_scroll_state(
    inner.axis,
    WritingMode::HorizontalTb,
    80.0,
    0.0,
    180.0,
    180.0,
    360.0,
    360.0,
  );

  let outer_progress = scroll_timeline_progress(&outer, outer_pos, outer_range, outer_size, &range);
  let inner_progress = scroll_timeline_progress(&inner, inner_pos, inner_range, inner_size, &range);

  assert!((outer_progress - 0.3).abs() < 0.05, "outer progress {outer_progress}");
  assert!((inner_progress - 0.44).abs() < 0.05, "inner progress {inner_progress}");
  assert!((outer_progress - inner_progress).abs() > 0.05);
}

fn pixel(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let px = pixmap.pixel(x, y).unwrap();
  (px.red(), px.green(), px.blue(), px.alpha())
}

fn red_pixels(pixmap: &tiny_skia::Pixmap) -> usize {
  let mut count = 0usize;
  for y in 0..pixmap.height() {
    for x in 0..pixmap.width() {
      let p = pixmap.pixel(x, y).unwrap();
      if p.red() == 255 && p.green() == 0 && p.blue() == 0 && p.alpha() == 255 {
        count += 1;
      }
    }
  }
  count
}

#[test]
fn scroll_timeline_drives_animation_during_render() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r#"
    <style>
      html, body { margin: 0; height: 100%; }
      body { background: black; scroll-timeline: main block; }
      .box { display: block; position: sticky; top: 0; left: 0; width: 100px; height: 100px; background: red; animation-timeline: main; animation-name: fade; }
      @keyframes fade { from { opacity: 0; } to { opacity: 1; } }
    </style>
    <div class="box"></div>
    <div style="height: 300px;"></div>
  "#;

  // Ensure content exceeds the viewport so scroll progress can advance.
  let dom = renderer.parse_html(html).expect("parse html");
  let tree = renderer.layout_document(&dom, 100, 100).expect("layout");
  let content_height = tree.content_size().height();
  assert!(content_height > 100.0, "content height must exceed viewport: {content_height}");
  let max_scroll = (content_height - tree.viewport_size().height).max(0.0);
  assert!(max_scroll > 0.0, "expected scrollable range, got {max_scroll}");
  let timeline_check = ScrollTimeline::default();
  let (pos, range, view_size) = axis_scroll_state(
    timeline_check.axis,
    WritingMode::HorizontalTb,
    0.0,
    max_scroll,
    tree.viewport_size().width,
    tree.viewport_size().height,
    tree.content_size().width(),
    tree.content_size().height(),
  );
  let prog = scroll_timeline_progress(&timeline_check, pos, range, view_size, &AnimationRange::default());
  assert!(prog > 0.9, "expected near-complete progress ({} / {}) -> {prog}", pos, range);

  let pixmap_top = renderer
    .render_html_with_scroll(html, 100, 100, 0.0, 0.0)
    .expect("render top");
  assert_eq!(pixel(&pixmap_top, 50, 50), (0, 0, 0, 255));
  assert_eq!(red_pixels(&pixmap_top), 0, "no red content when progress at start");

  let pixmap_bottom = renderer
    .render_html_with_scroll(html, 100, 100, 0.0, max_scroll)
    .expect("render bottom");
  assert_eq!(pixel(&pixmap_bottom, 50, 50), (255, 0, 0, 255));
  assert!(red_pixels(&pixmap_bottom) > 0, "red content should appear when fully scrolled");
}
