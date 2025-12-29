use fastrender::css::parser::parse_stylesheet;
use fastrender::css::types::StyleSheet;
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

fn display_for_hit(stylesheet: &StyleSheet, ctx: &MediaContext) -> String {
  let dom = dom::parse_html(r#"<div id="hit"></div>"#).unwrap();
  let styled = apply_styles_with_media(&dom, stylesheet, ctx);
  display(find_by_id(&styled, "hit").expect("hit element"))
}

#[test]
fn media_rule_supports_query_list_or_semantics() {
  let css = "@media print, screen and (min-width: 700px) { #hit { display: inline; } }";
  let stylesheet = parse_stylesheet(css).unwrap();

  assert_eq!(
    display_for_hit(&stylesheet, &MediaContext::screen(800.0, 600.0)),
    "inline"
  );
  assert_eq!(
    display_for_hit(&stylesheet, &MediaContext::print(800.0, 600.0)),
    "inline"
  );
  assert_eq!(
    display_for_hit(&stylesheet, &MediaContext::screen(600.0, 600.0)),
    "block"
  );
}

#[test]
fn media_rule_preserves_range_syntax_tokens() {
  let css = "@media (400px < width < 800px) { #hit { display: inline; } }";
  let stylesheet = parse_stylesheet(css).unwrap();

  assert_eq!(
    display_for_hit(&stylesheet, &MediaContext::screen(600.0, 600.0)),
    "inline"
  );
  assert_eq!(
    display_for_hit(&stylesheet, &MediaContext::screen(900.0, 600.0)),
    "block"
  );
}

#[test]
fn invalid_media_prelude_does_not_match() {
  let css = "@media (min-width: 50%) { #hit { display: inline; } }";
  let stylesheet = parse_stylesheet(css).unwrap();

  assert_eq!(
    display_for_hit(&stylesheet, &MediaContext::screen(800.0, 600.0)),
    "block"
  );
}
