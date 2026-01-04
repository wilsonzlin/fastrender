use fastrender::css::parser::{parse_declarations, parse_stylesheet};
use fastrender::css::types::PropertyName;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;

fn find_first<'a>(node: &'a StyledNode, tag: &str) -> Option<&'a StyledNode> {
  if let Some(name) = node.node.tag_name() {
    if name.eq_ignore_ascii_case(tag) {
      return Some(node);
    }
  }
  for child in node.children.iter() {
    if let Some(found) = find_first(child, tag) {
      return Some(found);
    }
  }
  None
}

fn render_div_display(css: &str) -> String {
  let dom = dom::parse_html(r#"<div></div>"#).unwrap();
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let div = find_first(&styled, "div").expect("div");
  div.styles.display.to_string()
}

#[test]
fn parses_vendor_prefixed_properties_as_canonical() {
  let decls = parse_declarations("-webkit-transform: rotate(10deg);");
  assert_eq!(decls.len(), 1);
  match &decls[0].property {
    PropertyName::Known(name) => assert_eq!(*name, "transform"),
    PropertyName::Custom(_) => panic!("expected known property"),
  }
}

#[test]
fn supports_vendor_prefixed_transform_matches_like_unprefixed() {
  let css_prefixed = r"@supports (-webkit-transform: rotate(10deg)) { div { display: inline; } }";
  let css_unprefixed = r"@supports (transform: rotate(10deg)) { div { display: inline; } }";
  assert_eq!(render_div_display(css_prefixed), "inline");
  assert_eq!(render_div_display(css_unprefixed), "inline");
}

