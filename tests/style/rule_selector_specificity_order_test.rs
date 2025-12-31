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

#[test]
fn rule_specificity_uses_strongest_matching_selector_in_rule() {
  let dom = dom::parse_html(r#"<div id="target" class="a"></div>"#).unwrap();
  let stylesheet = parse_stylesheet(
    r#"
      .a { display: inline; }
      div, #target { display: block; }
    "#,
  )
  .unwrap();

  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let target = find_by_id(&styled, "target").expect("target element");

  assert_eq!(target.styles.display.to_string(), "block");
}
