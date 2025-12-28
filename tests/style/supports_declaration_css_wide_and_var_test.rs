use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node.node.get_attribute_ref("id") == Some(id) {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

fn render_display(css: &str) -> String {
  let dom = dom::parse_html(r#"<div id="t"></div>"#).unwrap();
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let target = find_by_id(&styled, "t").expect("element with id t");
  target.styles.display.to_string()
}

#[test]
fn css_wide_keywords_are_supported_in_feature_queries() {
  let css = r#"@supports (display: inherit) { #t { display: inline; } }"#;
  assert_eq!(render_display(css), "inline");
}

#[test]
fn var_values_are_treated_as_supported_in_feature_queries() {
  let css = r#"@supports (color: var(--x)) { #t { display: inline; } }"#;
  assert_eq!(render_display(css), "inline");
}

#[test]
fn unknown_properties_with_css_wide_keywords_remain_unsupported() {
  let css = r#"@supports (not-a-prop: inherit) { #t { display: inline; } }"#;
  assert_eq!(render_display(css), "block");
}
