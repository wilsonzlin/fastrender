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
  node.children.iter().find_map(|child| find_by_id(child, id))
}

#[test]
fn slotted_only_matches_assigned_light_dom() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <style>
          ::slotted(.target) { color: rgb(10, 20, 30); }
        </style>
        <slot></slot>
      </template>
      <span id="assigned" class="target">assigned</span>
    </div>
    <span id="outside" class="target">outside</span>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = fastrender::css::parser::parse_stylesheet("").expect("empty stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let assigned = find_by_id(&styled, "assigned").expect("assigned element");
  assert_eq!(assigned.styles.color, Rgba::rgb(10, 20, 30));

  let outside = find_by_id(&styled, "outside").expect("outside element");
  assert_ne!(outside.styles.color, Rgba::rgb(10, 20, 30));
}

#[test]
fn slotted_order_respected_for_structural_pseudos() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <style>
          ::slotted(:first-child) { color: rgb(200, 0, 0); }
          ::slotted(:last-child) { color: rgb(0, 0, 200); }
        </style>
        <slot></slot>
      </template>
      <span id="first">one</span>
      <span id="second">two</span>
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = fastrender::css::parser::parse_stylesheet("").expect("empty stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let first = find_by_id(&styled, "first").expect("first child");
  assert_eq!(first.styles.color, Rgba::rgb(200, 0, 0));

  let second = find_by_id(&styled, "second").expect("second child");
  assert_eq!(second.styles.color, Rgba::rgb(0, 0, 200));
}
