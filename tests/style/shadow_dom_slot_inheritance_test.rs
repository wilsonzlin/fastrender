use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media_target_and_imports;
use fastrender::style::cascade::StyledNode;
use fastrender::style::color::Rgba;
use fastrender::style::media::MediaContext;

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .is_some_and(|value| value.eq_ignore_ascii_case(id))
  {
    return Some(node);
  }
  node.children.iter().find_map(|child| find_by_id(child, id))
}

fn find_text<'a>(node: &'a StyledNode, text: &str) -> Option<&'a StyledNode> {
  if node.node.text_content().is_some_and(|t| t == text) {
    return Some(node);
  }
  node
    .children
    .iter()
    .find_map(|child| find_text(child, text))
}

#[test]
fn slotted_nodes_inherit_from_slot_computed_style() {
  // No ::slotted rules: assigned nodes should still inherit inherited properties (color, font-size)
  // through the flattened-tree parent (the <slot> element).
  let html = r#"<div id="host"><template shadowroot="open"><style>.wrap{color:rgb(255,0,0);font-size:32px;}</style><div class="wrap"><slot></slot></div></template>slotted-text<span id="assigned">assigned</span></div>"#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet("").expect("empty stylesheet");
  let media_ctx = MediaContext::screen(1200.0, 800.0);
  let styled = apply_styles_with_media_target_and_imports(
    &dom,
    &stylesheet,
    &media_ctx,
    None,
    None,
    None,
    None,
    None,
    None,
  );

  let assigned = find_by_id(&styled, "assigned").expect("assigned element");
  assert_eq!(assigned.styles.color, Rgba::rgb(255, 0, 0));
  assert_eq!(assigned.styles.font_size, 32.0);

  let slotted_text = find_text(&styled, "slotted-text").expect("slotted text node");
  assert_eq!(slotted_text.styles.color, Rgba::rgb(255, 0, 0));
  assert_eq!(slotted_text.styles.font_size, 32.0);
}
