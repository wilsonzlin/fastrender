use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaContext;
use selectors::context::QuirksMode;

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
fn quirks_mode_has_attribute_selector_matches() {
  // Intentionally omit <!DOCTYPE html> so html5ever enters QuirksMode::Quirks.
  let html = r#"
    <div id="hit"><span data-x></span></div>
    <div id="miss"><span></span></div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  assert!(
    matches!(dom.document_quirks_mode(), QuirksMode::Quirks),
    "expected parse_html() without a doctype to enter quirks mode"
  );

  // Keep :has() nested so rule-index pruning doesn't skip the :has evaluation entirely.
  let css = r#"div:is(:has([data-x]), .noop) { display: inline; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "hit").expect("hit")),
    "inline"
  );
  assert_eq!(display(find_by_id(&styled, "miss").expect("miss")), "block");
}

