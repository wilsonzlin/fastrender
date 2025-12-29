use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::{self, DomNode, DomNodeType};
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

fn uppercase_tag_by_id(node: &mut DomNode, id: &str) -> bool {
  match &mut node.node_type {
    DomNodeType::Element {
      tag_name,
      attributes,
      ..
    } => {
      if attributes
        .iter()
        .any(|(k, v)| k.eq_ignore_ascii_case("id") && v.eq_ignore_ascii_case(id))
      {
        *tag_name = tag_name.to_ascii_uppercase();
        return true;
      }
    }
    _ => {}
  }

  for child in &mut node.children {
    if uppercase_tag_by_id(child, id) {
      return true;
    }
  }

  false
}

#[test]
fn mixed_case_attribute_selector_remains_matchable() {
  let html = r#"<div id="t" data-hit="x"></div>"#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"div[DaTa-HiT] { display: inline; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(display(find_by_id(&styled, "t").expect("target")), "inline");
}

#[test]
fn mixed_case_tag_selector_handles_uppercase_dom_nodes() {
  let html = r#"<DIV id="t"></DIV>"#;
  let dom = dom::parse_html(html).unwrap();
  let mut upper_dom = dom.clone();
  uppercase_tag_by_id(&mut upper_dom, "t");
  let css = r#"DIV { display: inline; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();

  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  assert_eq!(
    display(find_by_id(&styled, "t").expect("parsed dom")),
    "inline"
  );

  let styled_upper =
    apply_styles_with_media(&upper_dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  assert_eq!(
    display(find_by_id(&styled_upper, "t").expect("manual uppercase dom")),
    "inline"
  );
}
