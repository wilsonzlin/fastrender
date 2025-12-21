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
  for child in &node.children {
    if let Some(found) = find_first(child, tag) {
      return Some(found);
    }
  }
  None
}

fn styled_children(html: &str) -> Vec<StyledNode> {
  let dom = dom::parse_html(html).unwrap();
  let stylesheet = parse_stylesheet("").unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let container = find_first(&styled, "div").expect("div container");
  container
    .children
    .iter()
    .filter(|child| {
      child
        .node
        .tag_name()
        .map(|t| t.eq_ignore_ascii_case("span"))
        .unwrap_or(false)
    })
    .cloned()
    .collect()
}

#[test]
fn flex_grow_ignores_negative_numbers() {
  let children = styled_children(
    r#"<div style="display:flex"><span style="flex-grow: -1"></span><span style="flex-grow: 2"></span></div>"#,
  );
  assert_eq!(children[0].styles.flex_grow, 0.0);
  assert_eq!(children[1].styles.flex_grow, 2.0);
}

#[test]
fn flex_shrink_ignores_negative_numbers() {
  let children = styled_children(
    r#"<div style="display:flex"><span style="flex-shrink: -3"></span><span style="flex-shrink: 0.5"></span></div>"#,
  );
  // Initial flex-shrink is 1.0; invalid negative keeps the default.
  assert_eq!(children[0].styles.flex_shrink, 1.0);
  assert!((children[1].styles.flex_shrink - 0.5).abs() < 1e-6);
}

#[test]
fn flex_shorthand_none_and_auto_keywords() {
  let children = styled_children(
    r#"<div style="display:flex"><span style="flex: none"></span><span style="flex: auto"></span></div>"#,
  );
  assert_eq!(children[0].styles.flex_grow, 0.0);
  assert_eq!(children[0].styles.flex_shrink, 0.0);
  assert!(matches!(
    children[0].styles.flex_basis,
    fastrender::style::types::FlexBasis::Auto
  ));

  assert_eq!(children[1].styles.flex_grow, 1.0);
  assert_eq!(children[1].styles.flex_shrink, 1.0);
  assert!(matches!(
    children[1].styles.flex_basis,
    fastrender::style::types::FlexBasis::Auto
  ));
}

#[test]
fn flex_shorthand_numbers_and_basis() {
  use fastrender::style::types::FlexBasis;
  use fastrender::style::values::Length;
  use fastrender::style::values::LengthUnit;

  let children = styled_children(
    r#"<div style="display:flex">
            <span style="flex: 2 3 10px"></span>
            <span style="flex: 4 5"></span>
            <span style="flex: 6"></span>
            <span style="flex: 12px"></span>
        </div>"#,
  );

  assert_eq!(children[0].styles.flex_grow, 2.0);
  assert_eq!(children[0].styles.flex_shrink, 3.0);
  assert_eq!(
    children[0].styles.flex_basis,
    FlexBasis::Length(Length::px(10.0))
  );

  assert_eq!(children[1].styles.flex_grow, 4.0);
  assert_eq!(children[1].styles.flex_shrink, 5.0);
  assert_eq!(
    children[1].styles.flex_basis,
    FlexBasis::Length(Length::new(0.0, LengthUnit::Percent))
  );

  assert_eq!(children[2].styles.flex_grow, 6.0);
  assert_eq!(children[2].styles.flex_shrink, 1.0);
  assert_eq!(
    children[2].styles.flex_basis,
    FlexBasis::Length(Length::new(0.0, LengthUnit::Percent))
  );

  assert_eq!(children[3].styles.flex_grow, 1.0);
  assert_eq!(children[3].styles.flex_shrink, 1.0);
  assert_eq!(
    children[3].styles.flex_basis,
    FlexBasis::Length(Length::px(12.0))
  );
}
