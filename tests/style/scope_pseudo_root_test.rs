use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaContext;

fn find_tag<'a>(node: &'a StyledNode, tag: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .tag_name()
    .is_some_and(|name| name.eq_ignore_ascii_case(tag))
  {
    return Some(node);
  }

  for child in &node.children {
    if let Some(found) = find_tag(child, tag) {
      return Some(found);
    }
  }

  None
}

#[test]
fn scope_applies_to_document_root_element() {
  let dom = dom::parse_html("<html><body></body></html>").unwrap();
  let stylesheet = parse_stylesheet(":scope { display: inline; }").unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  let html = find_tag(&styled, "html").expect("html element");
  assert_eq!(html.styles.display.to_string(), "inline");

  let body = find_tag(&styled, "body").expect("body element");
  assert_eq!(body.styles.display.to_string(), "block");
}
