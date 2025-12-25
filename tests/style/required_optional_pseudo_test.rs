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
  for child in &node.children {
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
fn required_and_optional_match_disabled_controls() {
  let html = r#"
    <input id='r' required disabled>
    <input id='o' disabled>
    <input id='h' type='hidden' required>
  "#;
  let css = r#"
    input:required { display: inline; }
    input:optional { display: inline-block; }
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet(css).expect("parse stylesheet");
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "r").expect("required input")),
    "inline"
  );
  assert_eq!(
    display(find_by_id(&styled, "o").expect("optional input")),
    "inline-block"
  );
  // type=hidden doesn't participate in required/optional; its default display stays untouched.
  assert_eq!(
    display(find_by_id(&styled, "h").expect("hidden input")),
    "none"
  );
}
