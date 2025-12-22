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
fn descendant_and_child_has_match() {
  let html = r#"
    <div id="desc"><span></span></div>
    <div id="child"><span class="hit"></span></div>
    <div id="miss"></div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    div:has(span) { display: inline; }
    div:has(> .hit) { display: inline-block; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "desc").expect("desc")),
    "inline"
  );
  assert_eq!(
    display(find_by_id(&styled, "child").expect("child")),
    "inline-block"
  );
  assert_eq!(display(find_by_id(&styled, "miss").expect("miss")), "block");
}

#[test]
fn sibling_variants_match_has() {
  let html = r#"
    <p id="first"></p>
    <span class="marker"></span>
    <p id="second"></p>
    <div id="before"></div>
    <div></div>
    <p class="note"></p>
    <div id="after"></div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    p:has(+ .marker) { display: inline; }
    div:has(~ p.note) { display: inline-flex; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "first").expect("first")),
    "inline"
  );
  assert_eq!(
    display(find_by_id(&styled, "second").expect("second")),
    "block"
  );
  assert_eq!(
    display(find_by_id(&styled, "before").expect("before")),
    "inline-flex"
  );
  assert_eq!(
    display(find_by_id(&styled, "after").expect("after")),
    "block"
  );
}

#[test]
fn has_combines_with_is_and_not() {
  let html = r#"
    <section id="wrap"><div class="match"><span></span></div></section>
    <section id="miss"><div></div></section>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    :is(section, article):has(.match) { display: inline; }
    section:not(:has(span)) { display: table; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "wrap").expect("wrap")),
    "inline"
  );
  assert_eq!(display(find_by_id(&styled, "miss").expect("miss")), "table");
}

#[test]
fn has_contributes_specificity() {
  let html = r#"<div id="spec" class="highlight"><span class="target"></span></div>"#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    div.highlight { display: block; }
    div:has(.target) { display: inline; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  assert_eq!(
    display(find_by_id(&styled, "spec").expect("spec")),
    "inline"
  );
}
