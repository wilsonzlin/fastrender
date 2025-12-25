use fastrender::css::parser::parse_stylesheet;
use fastrender::css::types::StyleSheet;
use fastrender::dom::parse_html;
use fastrender::style::cascade::{apply_styles, StyledNode};
use fastrender::Rgba;

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
  let styled = apply_styles(&dom, &stylesheet);
  let label = find_by_id(&styled, "label").expect("shadow element");

  assert_eq!(label.styles.color, Rgba::rgb(1, 2, 3));
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
  let baseline = apply_styles(&dom, &StyleSheet::new());
  let baseline_color = find_by_id(&baseline, "shadow-label")
    .expect("shadow element")
    .styles
    .color;

  let stylesheet = parse_stylesheet(".label { color: rgb(9, 8, 7); }").expect("stylesheet");
  let styled = apply_styles(&dom, &stylesheet);
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
  let styled = apply_styles(&dom, &stylesheet);
  let label = find_by_id(&styled, "inner-label").expect("inner part");

  assert_eq!(label.styles.color, Rgba::rgb(10, 20, 30));
}
