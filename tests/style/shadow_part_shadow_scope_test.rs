use fastrender::css::parser::{extract_scoped_css_sources, StylesheetSource};
use fastrender::css::types::StyleSheet;
use fastrender::dom::parse_html;
use fastrender::style::cascade::{apply_style_set_with_media_target_and_imports, StyledNode};
use fastrender::style::media::MediaContext;
use fastrender::style::style_set::StyleSet;
use fastrender::Rgba;
use std::collections::HashMap;

fn stylesheet_from_sources(sources: &[StylesheetSource]) -> StyleSheet {
  let mut combined = Vec::new();
  for source in sources {
    let StylesheetSource::Inline(inline) = source else {
      continue;
    };
    if inline.disabled || inline.css.trim().is_empty() {
      continue;
    }
    if let Ok(sheet) = fastrender::css::parser::parse_stylesheet(&inline.css) {
      combined.extend(sheet.rules);
    }
  }
  StyleSheet { rules: combined }
}

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node.node.get_attribute_ref("id") == Some(id) {
    return Some(node);
  }
  node.children.iter().find_map(|child| find_by_id(child, id))
}

fn style_set_from_html(html: &str) -> (StyleSet, fastrender::dom::DomNode) {
  let dom = parse_html(html).expect("parsed html");
  let scoped_sources = extract_scoped_css_sources(&dom);
  let mut shadows = HashMap::new();
  for (host, sources) in scoped_sources.shadows {
    shadows.insert(host, stylesheet_from_sources(&sources));
  }
  let style_set = StyleSet {
    document: stylesheet_from_sources(&scoped_sources.document),
    shadows,
  };
  (style_set, dom)
}

#[test]
fn shadow_scoped_part_rules_cross_nested_scopes() {
  let html = r#"
    <x-app id="app">
      <template shadowroot="open">
        <style>
          x-outer::part(outer-label) { color: rgb(12, 34, 56); }
        </style>
        <x-outer id="outer">
          <template shadowroot="open">
            <x-inner id="inner" exportparts="label:outer-label">
              <template shadowroot="open">
                <span id="target" part="label">Hello</span>
              </template>
            </x-inner>
          </template>
        </x-outer>
      </template>
    </x-app>
  "#;

  let (style_set, dom) = style_set_from_html(html);
  let media = MediaContext::screen(800.0, 600.0);
  let styled = apply_style_set_with_media_target_and_imports(
    &dom, &style_set, &media, None, None, None, None, None, None,
  );

  let target = find_by_id(&styled, "target").expect("shadow part");
  assert_eq!(target.styles.color, Rgba::rgb(12, 34, 56));
}

#[test]
fn part_rules_do_not_apply_without_exportparts_forwarding() {
  let html = r#"
    <x-app id="app">
      <template shadowroot="open">
        <style>
          x-outer::part(outer-label) { color: rgb(200, 10, 20); }
        </style>
        <x-outer id="outer">
          <template shadowroot="open">
            <x-inner id="inner">
              <template shadowroot="open">
                <span id="target" part="label">Hello</span>
              </template>
            </x-inner>
          </template>
        </x-outer>
      </template>
    </x-app>
  "#;

  let (style_set, dom) = style_set_from_html(html);
  let media = MediaContext::screen(800.0, 600.0);
  let styled = apply_style_set_with_media_target_and_imports(
    &dom, &style_set, &media, None, None, None, None, None, None,
  );
  let target = find_by_id(&styled, "target").expect("shadow part");

  let baseline_style_set = StyleSet {
    document: StyleSheet::new(),
    shadows: HashMap::new(),
  };
  let baseline = apply_style_set_with_media_target_and_imports(
    &dom,
    &baseline_style_set,
    &media,
    None,
    None,
    None,
    None,
    None,
    None,
  );
  let baseline_target = find_by_id(&baseline, "target").expect("baseline");

  assert_eq!(target.styles.color, baseline_target.styles.color);
  assert_ne!(target.styles.color, Rgba::rgb(200, 10, 20));
}
