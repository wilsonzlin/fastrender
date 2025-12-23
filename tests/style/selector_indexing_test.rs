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
  for child in &node.children {
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
fn is_selector_indexes_all_alternatives() {
  let html = r#"
    <div id="one" class="a"></div>
    <div id="two" class="b"></div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#":is(.a, .b) { display: inline; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(display(find_by_id(&styled, "one").expect(".a")), "inline");
  assert_eq!(display(find_by_id(&styled, "two").expect(".b")), "inline");
}

#[test]
fn where_selector_matches_ids_and_classes() {
  let html = r#"
    <div id="match-id"></div>
    <div id="match-class" class="cls"></div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#":where(#match-id, .cls) { display: inline; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "match-id").expect("id")),
    "inline"
  );
  assert_eq!(
    display(find_by_id(&styled, "match-class").expect("class")),
    "inline"
  );
}

#[test]
fn tag_with_is_alternatives_matches_all_options() {
  let html = r#"
    <div id="first" class="a"></div>
    <div id="second" class="b"></div>
    <span id="other" class="a"></span>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"div:is(.a, .b) { display: inline-block; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "first").expect("first")),
    "inline-block"
  );
  assert_eq!(
    display(find_by_id(&styled, "second").expect("second")),
    "inline-block"
  );
  // Non-div elements should not match even if they share classes.
  assert_eq!(
    display(find_by_id(&styled, "other").expect("other")),
    "inline"
  );
}

#[test]
fn negation_does_not_hide_rules() {
  let html = r#"
    <div id="match"></div>
    <div id="skip" class="a"></div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    div:not(.a) { display: inline; }
    .a { display: block; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "match").expect("match")),
    "inline"
  );
  assert_eq!(display(find_by_id(&styled, "skip").expect("skip")), "block");
}
