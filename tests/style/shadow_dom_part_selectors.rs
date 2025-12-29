use fastrender::css::parser::parse_stylesheet;
use fastrender::css::types::StyleSheet;
use fastrender::dom::parse_html;
use fastrender::style::cascade::apply_style_set_with_media_target_and_imports;
use fastrender::style::media::MediaContext;
use fastrender::style::style_set::StyleSet;
use fastrender::Rgba;
use std::collections::HashMap;

fn find_by_id<'a>(
  node: &'a fastrender::style::cascade::StyledNode,
  id: &str,
) -> Option<&'a fastrender::style::cascade::StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .is_some_and(|value| value.eq_ignore_ascii_case(id))
  {
    return Some(node);
  }
  for child in node.children.iter() {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

fn apply_styles(html: &str, css: &str) -> fastrender::style::cascade::StyledNode {
  let dom = parse_html(html).expect("parsed html");
  let stylesheet = parse_stylesheet(css).expect("stylesheet");
  let style_set = StyleSet {
    document: stylesheet,
    shadows: HashMap::new(),
  };
  let media = MediaContext::screen(800.0, 600.0);
  apply_style_set_with_media_target_and_imports(
    &dom, &style_set, &media, None, None, None, None, None, None,
  )
}

#[test]
fn part_matches_shadow_content() {
  let html = r#"
    <x-host id="host">
      <template shadowroot="open">
        <span id="label" part="label">Hello</span>
      </template>
    </x-host>
  "#;

  let styled = apply_styles(html, "x-host::part(label) { color: rgb(1, 2, 3); }");
  let label = find_by_id(&styled, "label").expect("shadow part");
  assert_eq!(label.styles.color, Rgba::rgb(1, 2, 3));
}

#[test]
fn exportparts_renames_parts() {
  let html = r#"
    <x-host id="host" exportparts="label:outer">
      <template shadowroot="open">
        <span id="label" part="label">Hello</span>
      </template>
    </x-host>
  "#;

  let styled = apply_styles(html, "x-host::part(outer) { color: rgb(4, 5, 6); }");
  let label = find_by_id(&styled, "label").expect("renamed part");
  assert_eq!(label.styles.color, Rgba::rgb(4, 5, 6));
}

#[test]
fn nested_exportparts_chain_is_resolved() {
  let html = r#"
    <x-outer id="outer" exportparts="inner-label:outer-label">
      <template shadowroot="open">
        <x-inner id="inner" exportparts="leaf:inner-label">
          <template shadowroot="open">
            <span id="leaf" part="leaf">Leaf</span>
          </template>
        </x-inner>
      </template>
    </x-outer>
  "#;

  let styled = apply_styles(
    html,
    "x-outer::part(outer-label) { color: rgb(7, 8, 9); }",
  );
  let leaf = find_by_id(&styled, "leaf").expect("deeply exported part");
  assert_eq!(leaf.styles.color, Rgba::rgb(7, 8, 9));
}

#[test]
fn parts_not_exported_do_not_match() {
  let html = r#"
    <x-outer id="outer">
      <template shadowroot="open">
        <x-inner id="inner">
          <template shadowroot="open">
            <span id="leaf" part="label">Leaf</span>
          </template>
        </x-inner>
      </template>
    </x-outer>
  "#;

  let empty_set = StyleSet {
    document: StyleSheet::new(),
    shadows: HashMap::new(),
  };
  let media = MediaContext::screen(800.0, 600.0);
  let dom = parse_html(html).expect("parsed html");
  let baseline = apply_style_set_with_media_target_and_imports(
    &dom, &empty_set, &media, None, None, None, None, None, None,
  );
  let baseline_color = find_by_id(&baseline, "leaf")
    .expect("baseline part")
    .styles
    .color;

  let styled = apply_styles(html, "x-outer::part(label) { color: rgb(10, 11, 12); }");
  let leaf = find_by_id(&styled, "leaf").expect("unexported part");
  assert_eq!(leaf.styles.color, baseline_color);
}
