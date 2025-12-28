use fastrender::css::parser::{extract_scoped_css_sources, parse_stylesheet, StylesheetSource};
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
    if let Ok(sheet) = parse_stylesheet(&inline.css) {
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

fn apply_scoped_styles(html: &str) -> StyledNode {
  let dom = parse_html(html).expect("parse html");
  let scoped_sources = extract_scoped_css_sources(&dom);

  let document = stylesheet_from_sources(&scoped_sources.document);
  let mut shadows = HashMap::new();
  for (host, sources) in scoped_sources.shadows {
    shadows.insert(host, stylesheet_from_sources(&sources));
  }

  let style_set = StyleSet { document, shadows };
  let media = MediaContext::screen(800.0, 600.0);
  apply_style_set_with_media_target_and_imports(
    &dom, &style_set, &media, None, None, None, None, None, None,
  )
}

#[test]
fn document_rules_outrank_shadow_host_rules() {
  let html = r#"
    <style>
      x-host { color: rgb(255, 0, 0); }
    </style>
    <x-host id="host">
      <template shadowroot="open">
        <style>
          :host { color: rgb(0, 0, 255); }
        </style>
        <slot></slot>
      </template>
    </x-host>
  "#;

  let styled = apply_scoped_styles(html);
  let host = find_by_id(&styled, "host").expect("styled host");
  assert_eq!(host.styles.color, Rgba::rgb(255, 0, 0));
}

#[test]
fn important_shadow_host_rules_override_document_important() {
  let html = r#"
    <style>
      x-host { color: rgb(255, 0, 0) !important; }
    </style>
    <x-host id="host">
      <template shadowroot="open">
        <style>
          :host { color: rgb(0, 0, 255) !important; }
        </style>
        <slot></slot>
      </template>
    </x-host>
  "#;

  let styled = apply_scoped_styles(html);
  let host = find_by_id(&styled, "host").expect("styled host");
  assert_eq!(host.styles.color, Rgba::rgb(0, 0, 255));
}

#[test]
fn document_source_order_still_applies_before_shadow_host() {
  let html = r#"
    <style>
      x-host { color: rgb(200, 0, 0); }
      x-host { color: rgb(10, 200, 30); }
    </style>
    <x-host id="host">
      <template shadowroot="open">
        <style>
          :host { color: rgb(0, 0, 255); }
        </style>
        <slot></slot>
      </template>
    </x-host>
  "#;

  let styled = apply_scoped_styles(html);
  let host = find_by_id(&styled, "host").expect("styled host");
  assert_eq!(host.styles.color, Rgba::rgb(10, 200, 30));
}
