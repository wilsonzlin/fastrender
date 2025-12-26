use fastrender::css::parser::{extract_scoped_css_sources, parse_stylesheet, StylesheetSource};
use fastrender::css::types::StyleSheet;
use fastrender::dom;
use fastrender::style::cascade::apply_style_set_with_media_target_and_imports;
use fastrender::style::cascade::StyledNode;
use fastrender::style::color::Rgba;
use fastrender::style::media::MediaContext;
use fastrender::style::style_set::StyleSet;

fn stylesheet_from_sources(sources: &[StylesheetSource]) -> StyleSheet {
  let mut combined = Vec::new();
  for source in sources {
    let StylesheetSource::Inline(inline) = source else {
      continue;
    };
    if inline.disabled || inline.css.trim().is_empty() {
      continue;
    }
    if let Ok(sheet) = parse_stylesheet(&inline.css) {
      combined.extend(sheet.rules);
    }
  }
  StyleSheet { rules: combined }
}

fn find_styled_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .is_some_and(|value| value.eq_ignore_ascii_case(id))
  {
    return Some(node);
  }
  node
    .children
    .iter()
    .find_map(|child| find_styled_by_id(child, id))
}

#[test]
fn shadow_styles_are_scoped() {
  let html = r#"
    <style>
      .inside { color: rgb(255, 0, 0); }
      #shadow1 { color: rgb(255, 0, 0) !important; }
    </style>
    <div id="host1" class="foo">
      <template shadowroot="open">
        <style>
          .inside { color: rgb(0, 0, 255); }
          :host { background-color: rgb(10, 20, 30); }
          :host(.foo) { opacity: 0.5; }
        </style>
        <div class="inside" id="shadow1">shadow</div>
        <slot></slot>
      </template>
      <div class="inside" id="light1">light</div>
    </div>
    <div id="host2" class="bar">
      <template shadowroot="open">
        <style>
          .inside { color: rgb(0, 0, 255); }
          :host { background-color: rgb(10, 20, 30); }
          :host(.foo) { opacity: 0.5; }
        </style>
        <div class="inside" id="shadow2">shadow2</div>
      </template>
    </div>
  "#;

  let dom = dom::parse_html(html).expect("parse html");
  let scoped_sources = extract_scoped_css_sources(&dom);
  let mut shadows = std::collections::HashMap::new();
  for (host, sources) in scoped_sources.shadows {
    shadows.insert(host, stylesheet_from_sources(&sources));
  }
  let style_set = StyleSet {
    document: stylesheet_from_sources(&scoped_sources.document),
    shadows,
  };

  let media = MediaContext::screen(800.0, 600.0);
  let styled =
    apply_style_set_with_media_target_and_imports(&dom, &style_set, &media, None, None, None, None, None, None);

  let shadow_inside = find_styled_by_id(&styled, "shadow1").expect("shadow child");
  assert_eq!(shadow_inside.styles.color, Rgba::rgb(0, 0, 255));

  let slotted_light = find_styled_by_id(&styled, "light1").expect("light child");
  assert_eq!(slotted_light.styles.color, Rgba::rgb(0, 0, 255));

  let host_one = find_styled_by_id(&styled, "host1").expect("host1");
  assert_eq!(host_one.node.get_attribute_ref("class"), Some("foo"));
  assert_eq!(host_one.styles.background_color, Rgba::rgb(10, 20, 30));
  assert!(
    (host_one.styles.opacity - 0.5).abs() < f32::EPSILON,
    "host1 opacity was {}",
    host_one.styles.opacity
  );

  let host_two = find_styled_by_id(&styled, "host2").expect("host2");
  assert_eq!(host_two.styles.background_color, Rgba::rgb(10, 20, 30));
  assert!((host_two.styles.opacity - 1.0).abs() < f32::EPSILON);
}
