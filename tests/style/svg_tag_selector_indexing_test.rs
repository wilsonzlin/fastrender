use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
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
fn foreign_object_selectors_match_camel_case_dom_tag() {
  let html =
    r#"<svg><foreignObject id="fo" x="0" y="0" width="10" height="10"></foreignObject></svg>"#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"foreignObject { display: none; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  let fo = find_by_id(&styled, "fo").expect("foreignObject");
  assert_eq!(display(fo), "none");
}

#[test]
fn linear_gradient_selectors_match_camel_case_dom_tag() {
  let html = r#"<svg><linearGradient id="lg"></linearGradient></svg>"#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"linearGradient { display: block; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  let gradient = find_by_id(&styled, "lg").expect("linearGradient");
  assert_eq!(display(gradient), "block");
}
