use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::StyledNode;
use fastrender::style::cascade::{apply_styles, apply_styles_with_target};
use fastrender::style::display::Display;

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .is_some_and(|value| value.eq_ignore_ascii_case(id))
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

#[test]
fn focus_within_considers_slotted_descendants() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <div id="wrap"><slot></slot></div>
      </template>
      <input id="slotted" type="text" data-fastr-focus="true" />
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet =
    parse_stylesheet("#wrap:focus-within { display: inline; } #wrap { display: block; }")
      .expect("stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let wrap = find_by_id(&styled, "wrap").expect("wrap element");
  assert_eq!(wrap.styles.display, Display::Inline);
}

#[test]
fn target_within_considers_slotted_descendants() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <div id="wrap"><slot></slot></div>
      </template>
      <input id="slotted" type="text" />
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet =
    parse_stylesheet("#wrap:target-within { display: inline; } #wrap { display: block; }")
      .expect("stylesheet");
  let styled = apply_styles_with_target(&dom, &stylesheet, Some("#slotted"));

  let wrap = find_by_id(&styled, "wrap").expect("wrap element");
  assert_eq!(wrap.styles.display, Display::Inline);
}
