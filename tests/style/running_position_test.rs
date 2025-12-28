use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;
use fastrender::style::position::Position;

fn find_first<'a>(node: &'a StyledNode, tag: &str) -> Option<&'a StyledNode> {
  if let Some(name) = node.node.tag_name() {
    if name.eq_ignore_ascii_case(tag) {
      return Some(node);
    }
  }
  for child in &node.children {
    if let Some(found) = find_first(child, tag) {
      return Some(found);
    }
  }
  None
}

#[test]
fn running_function_sets_running_position() {
  let dom = dom::parse_html(r#"<div style="position: RUNNING( header-name )"></div>"#).unwrap();
  let stylesheet = parse_stylesheet("").unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let div = find_first(&styled, "div").expect("div element");

  assert_eq!(div.styles.running_position.as_deref(), Some("header-name"));
  assert_eq!(div.styles.position, Position::Static);
}

#[test]
fn position_keyword_clears_running_position() {
  let dom =
    dom::parse_html(r#"<div style="position: running(foo); position: absolute;"></div>"#).unwrap();
  let stylesheet = parse_stylesheet("").unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let div = find_first(&styled, "div").expect("div element");

  assert_eq!(div.styles.position, Position::Absolute);
  assert!(div.styles.running_position.is_none());
}
