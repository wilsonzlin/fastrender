use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaContext;

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

fn display(node: &StyledNode) -> String {
  node.styles.display.to_string()
}

#[test]
fn nth_child_of_selector_list_matches_all_alternatives() {
  let html = r#"
    <ul>
      <li class="a" id="a1"></li>
      <li class="x" id="x"></li>
      <li class="b" id="b1"></li>
      <li class="b" id="b2"></li>
    </ul>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    li { display: block; }
    :nth-child(odd of .a, .b) { display: inline; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(display(find_by_id(&styled, "a1").expect("a1")), "inline");
  assert_eq!(display(find_by_id(&styled, "x").expect("x")), "block");
  assert_eq!(display(find_by_id(&styled, "b1").expect("b1")), "block");
  assert_eq!(display(find_by_id(&styled, "b2").expect("b2")), "inline");
}
