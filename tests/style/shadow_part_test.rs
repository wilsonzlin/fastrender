use fastrender::css::parser::parse_stylesheet;
use fastrender::css::types::StyleSheet;
use fastrender::dom::parse_html;
use fastrender::style::cascade::{
  apply_style_set_with_media_target_and_imports, apply_styles_with_media_target_and_imports,
  StyledNode,
};
use fastrender::style::media::MediaContext;
use fastrender::style::style_set::StyleSet;
use fastrender::style::values::Length;
use fastrender::Rgba;
use std::collections::HashMap;

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node.node.get_attribute_ref("id") == Some(id) {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

#[test]
fn part_selector_styles_shadow_content() {
  let html = r#"
    <x-host id="host">
      <template shadowroot="open">
        <span id="label" part="label">Hello</span>
      </template>
    </x-host>
  "#;

  let dom = parse_html(html).expect("parsed html");
  let stylesheet =
    parse_stylesheet("x-host::part(label) { color: rgb(1, 2, 3); }").expect("stylesheet");
  let style_set = StyleSet {
    document: stylesheet,
    shadows: HashMap::new(),
  };
  let media = MediaContext::screen(800.0, 600.0);
  let styled = apply_style_set_with_media_target_and_imports(
    &dom, &style_set, &media, None, None, None, None, None, None,
  );
  let label = find_by_id(&styled, "label").expect("shadow element");

  assert_eq!(label.styles.color, Rgba::rgb(1, 2, 3));
}

#[test]
fn part_selector_with_invalid_syntax_is_ignored() {
  let html = r#"
    <x-host id="host">
      <template shadowroot="open">
        <span id="label" part="label">Hello</span>
      </template>
    </x-host>
  "#;

  let dom = parse_html(html).expect("parsed html");
  let empty_style_set = StyleSet {
    document: StyleSheet::new(),
    shadows: HashMap::new(),
  };
  let media = MediaContext::screen(800.0, 600.0);
  let baseline = apply_style_set_with_media_target_and_imports(
    &dom,
    &empty_style_set,
    &media,
    None,
    None,
    None,
    None,
    None,
    None,
  );
  let baseline_label = find_by_id(&baseline, "label").expect("shadow element");
  let baseline_color = baseline_label.styles.color;

  let stylesheet = parse_stylesheet("x-host::part(name badge) { color: rgb(4, 5, 6); }")
    .expect("stylesheet still parses with errors dropped");
  let style_set = StyleSet {
    document: stylesheet,
    shadows: HashMap::new(),
  };
  let styled = apply_style_set_with_media_target_and_imports(
    &dom, &style_set, &media, None, None, None, None, None, None,
  );
  let label = find_by_id(&styled, "label").expect("shadow element");

  assert_eq!(label.styles.color, baseline_color);
}

#[test]
fn light_dom_selector_does_not_cross_shadow_boundary() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <span id="shadow-label" class="label" part="label">Shadow</span>
      </template>
    </div>
  "#;

  let dom = parse_html(html).expect("parsed html");
  let empty_style_set = StyleSet {
    document: StyleSheet::new(),
    shadows: HashMap::new(),
  };
  let media = MediaContext::screen(800.0, 600.0);
  let baseline = apply_style_set_with_media_target_and_imports(
    &dom,
    &empty_style_set,
    &media,
    None,
    None,
    None,
    None,
    None,
    None,
  );
  let baseline_color = find_by_id(&baseline, "shadow-label")
    .expect("shadow element")
    .styles
    .color;

  let stylesheet = parse_stylesheet(".label { color: rgb(9, 8, 7); }").expect("stylesheet");
  let style_set = StyleSet {
    document: stylesheet,
    shadows: HashMap::new(),
  };
  let styled = apply_style_set_with_media_target_and_imports(
    &dom, &style_set, &media, None, None, None, None, None, None,
  );
  let label = find_by_id(&styled, "shadow-label").expect("shadow element");

  assert_eq!(label.styles.color, baseline_color);
}

#[test]
fn exportparts_chain_maps_part_names() {
  let html = r#"
    <x-outer id="outer">
      <template shadowroot="open">
        <x-inner id="inner" exportparts="label:outer-label">
          <template shadowroot="open">
            <span id="inner-label" part="label">Inner</span>
          </template>
        </x-inner>
      </template>
    </x-outer>
  "#;

  let dom = parse_html(html).expect("parsed html");
  let stylesheet =
    parse_stylesheet("x-outer::part(outer-label) { color: rgb(10, 20, 30); }").expect("stylesheet");
  let style_set = StyleSet {
    document: stylesheet,
    shadows: HashMap::new(),
  };
  let media = MediaContext::screen(800.0, 600.0);
  let styled = apply_style_set_with_media_target_and_imports(
    &dom, &style_set, &media, None, None, None, None, None, None,
  );
  let label = find_by_id(&styled, "inner-label").expect("inner part");

  assert_eq!(label.styles.color, Rgba::rgb(10, 20, 30));
}

#[test]
fn nested_shadow_roots_resolve_part_order_stably() {
  let html = r#"
    <x-outer id="outer">
      <template shadowroot="open">
        <style>
          .inner::part(label) {
            color: rgb(1, 2, 3);
            border-top: 7px solid rgb(1, 2, 3);
          }
        </style>
        <x-inner id="inner" class="inner">
          <template shadowroot="open">
            <style>
              .target {
                color: rgb(4, 5, 6);
              }
            </style>
            <span id="part" class="target" part="label">Inner</span>
          </template>
        </x-inner>
      </template>
    </x-outer>
  "#;

  let dom = parse_html(html).expect("parsed html");
  let media = MediaContext::screen(800.0, 600.0);
  let styled = apply_styles_with_media_target_and_imports(
    &dom,
    &StyleSheet::new(),
    &media,
    None,
    None,
    None,
    None,
    None,
    None,
  );
  let part = find_by_id(&styled, "part").expect("shadow part");

  assert_eq!(part.styles.border_top_width, Length::px(7.0));
  assert_eq!(part.styles.color, Rgba::rgb(4, 5, 6));
}
