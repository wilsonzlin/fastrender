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

fn display(node: &StyledNode) -> String {
  node.styles.display.to_string()
}

#[test]
fn nth_child_of_filters_siblings() {
  let html = r#"
    <ul>
      <li class="a" id="x"></li>
      <li id="y"></li>
      <li class="a" id="z"></li>
    </ul>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    li { display: block; }
    li:nth-child(2 of .a) { display: inline; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(display(find_by_id(&styled, "x").expect("x")), "block");
  assert_eq!(display(find_by_id(&styled, "y").expect("y")), "block");
  assert_eq!(display(find_by_id(&styled, "z").expect("z")), "inline");
}

#[test]
fn nth_last_child_of_counts_matching_siblings() {
  let html = r#"
    <ul>
      <li class="a" id="x"></li>
      <li id="y"></li>
      <li class="a" id="z"></li>
    </ul>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    li { display: block; }
    li:nth-last-child(2 of .a) { display: inline; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(display(find_by_id(&styled, "x").expect("x")), "inline");
  assert_eq!(display(find_by_id(&styled, "y").expect("y")), "block");
  assert_eq!(display(find_by_id(&styled, "z").expect("z")), "block");
}

#[test]
fn nth_child_without_of_still_matches() {
  let html = r#"
    <ul>
      <li id="x"></li>
      <li id="y"></li>
      <li id="z"></li>
    </ul>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    li { display: block; }
    li:nth-child(2n+1) { display: inline; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(display(find_by_id(&styled, "x").expect("x")), "inline");
  assert_eq!(display(find_by_id(&styled, "y").expect("y")), "block");
  assert_eq!(display(find_by_id(&styled, "z").expect("z")), "inline");
}
