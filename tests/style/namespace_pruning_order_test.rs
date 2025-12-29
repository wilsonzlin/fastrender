use fastrender::css::parser::parse_stylesheet;
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

#[test]
fn namespaced_selectors_preserve_layer_and_scope_order() {
  let html = r#"
    <svg id="outer"><rect id="outside"></rect></svg>
    <div class="scoped">
      <svg>
        <rect id="inside"></rect>
        <rect id="important" class="strong"></rect>
      </svg>
    </div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    @namespace svg url("http://www.w3.org/2000/svg");
    @layer base { svg|rect { display: block; } }
    @layer theme { svg|rect { display: inline; } }
    @scope (.scoped) {
      rect { display: flex; }
      rect.strong { display: grid !important; }
    }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "outside").expect("outside rect")),
    "inline"
  );
  assert_eq!(
    display(find_by_id(&styled, "inside").expect("scoped rect")),
    "flex"
  );
  assert_eq!(
    display(find_by_id(&styled, "important").expect("important rect")),
    "grid"
  );
}
