use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
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

#[test]
fn parses_integer_order() {
  let dom = dom::parse_html(
    r#"<div><span style="order: -2">a</span><span style="order: 5">b</span></div>"#,
  )
  .unwrap();
  let stylesheet = parse_stylesheet("").unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let div = find_first(&styled, "div").expect("div");
  let orders: Vec<i32> = div.children.iter().map(|n| n.styles.order).collect();
  assert_eq!(orders, vec![-2, 5]);
}

#[test]
fn ignores_non_integer_order() {
  let dom = dom::parse_html(r#"<div><span style="order: 1.5">a</span></div>"#).unwrap();
  let stylesheet = parse_stylesheet("").unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let div = find_first(&styled, "div").expect("div");
  let order = div.children[0].styles.order;
  assert_eq!(order, 0);
}
