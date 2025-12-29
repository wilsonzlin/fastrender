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
fn is_selector_is_forgiving_of_unknown_pseudo() {
  let dom = dom::parse_html(r#"<div id="hit" class="ok"></div><div id="miss"></div>"#).unwrap();
  let css = r#"div:is(.ok, :unknown-pseudo) { display: inline; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(display(find_by_id(&styled, "hit").expect("hit")), "inline");
  assert_eq!(display(find_by_id(&styled, "miss").expect("miss")), "block");
}

#[test]
fn where_selector_is_forgiving_of_unknown_pseudo() {
  let dom = dom::parse_html(r#"<div id="hit" class="ok"></div><div id="miss"></div>"#).unwrap();
  let css = r#":where(.ok, :unknown-pseudo) { display: inline; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(display(find_by_id(&styled, "hit").expect("hit")), "inline");
  assert_eq!(display(find_by_id(&styled, "miss").expect("miss")), "block");
}
