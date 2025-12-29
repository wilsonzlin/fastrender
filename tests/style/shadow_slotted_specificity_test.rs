use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles;
use fastrender::style::cascade::StyledNode;
use fastrender::style::color::Rgba;

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
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

#[test]
fn slotted_specificity_includes_pseudo_element() {
  // The document rule has a type+class selector (specificity 0,1,1). The slotted rule
  // only has a class selector in its argument, so without the ::slotted() pseudo-element
  // contributing its own element-level specificity the document rule would outrank it.
  // With the pseudo-element bump, the slotted rule should win.
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <style>
          ::slotted(.target) { color: rgb(255, 0, 0); }
        </style>
        <slot></slot>
      </template>
      <span id="light" class="target">Light</span>
    </div>
  "#;

  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet =
    parse_stylesheet("span.target { color: rgb(0, 0, 255); }").expect("parse document stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let slotted = find_by_id(&styled, "light").expect("slotted element");
  assert_eq!(slotted.styles.color, Rgba::rgb(255, 0, 0));
}
