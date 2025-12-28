use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::{self, DomNode};
use fastrender::style::cascade::{
  apply_styles_with_media_target_and_imports, ContainerQueryContext, ContainerQueryInfo, StyledNode,
};
use fastrender::style::media::MediaContext;
use fastrender::style::types::ContainerType;
use fastrender::style::ComputedStyle;
use std::collections::HashMap;
use std::sync::Arc;

const HTML: &str = r#"<div id="c" class="container"><div id="t" class="target"></div></div>"#;

fn find_dom_by_id<'a>(node: &'a DomNode, id: &str) -> Option<&'a DomNode> {
  if node
    .get_attribute_ref("id")
    .is_some_and(|value| value.eq_ignore_ascii_case(id))
  {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_dom_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

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

fn cascade_with_container(css: &str, inline_size: f32, names: Vec<String>) -> StyledNode {
  let dom = dom::parse_html(HTML).unwrap();
  let ids = dom::enumerate_dom_ids(&dom);
  let container_node = find_dom_by_id(&dom, "c").expect("container node");
  let container_id = *ids
    .get(&(container_node as *const DomNode))
    .expect("id for container");

  let base_media = MediaContext::screen(800.0, 600.0);
  let mut containers = HashMap::new();
  containers.insert(
    container_id,
    ContainerQueryInfo {
      inline_size,
      block_size: 300.0,
      container_type: ContainerType::InlineSize,
      names,
      font_size: 16.0,
      styles: Arc::new(ComputedStyle::default()),
    },
  );
  let ctx = ContainerQueryContext {
    base_media: base_media.clone(),
    containers,
  };
  let stylesheet = parse_stylesheet(css).unwrap();

  apply_styles_with_media_target_and_imports(
    &dom,
    &stylesheet,
    &base_media,
    None,
    None,
    None,
    Some(&ctx),
    None,
    None,
  )
}

#[test]
fn container_query_applies_when_size_matches() {
  let css = r#"
    @container (min-width: 400px) {
      .target { display: inline; }
    }
  "#;
  let styled = cascade_with_container(css, 500.0, vec![]);

  assert_eq!(display(find_by_id(&styled, "t").expect("target")), "inline");
}

#[test]
fn not_container_query_parses_and_evaluates() {
  let css = r#"
    .target { display: block; }
    @container not (min-width: 400px) {
      .target { display: inline; }
    }
  "#;

  let styled_small = cascade_with_container(css, 300.0, vec![]);
  assert_eq!(
    display(find_by_id(&styled_small, "t").expect("target")),
    "inline"
  );

  let styled_large = cascade_with_container(css, 500.0, vec![]);
  assert_eq!(
    display(find_by_id(&styled_large, "t").expect("target")),
    "block"
  );
}

#[test]
fn named_container_selection() {
  let css = r#"
    .target { display: block; }
    @container sidebar (min-width: 400px) {
      .target { display: inline; }
    }
  "#;

  let styled_named = cascade_with_container(css, 500.0, vec!["sidebar".into()]);
  assert_eq!(
    display(find_by_id(&styled_named, "t").expect("target")),
    "inline"
  );

  let styled_unnamed = cascade_with_container(css, 500.0, vec![]);
  assert_eq!(
    display(find_by_id(&styled_unnamed, "t").expect("target")),
    "block"
  );
}

#[test]
fn container_query_list_uses_or_semantics() {
  let css = r#"
    .target { display: block; }
    @container (min-width: 600px), (max-width: 200px) {
      .target { display: inline; }
    }
  "#;

  let styled_wide = cascade_with_container(css, 650.0, vec![]);
  assert_eq!(
    display(find_by_id(&styled_wide, "t").expect("target")),
    "inline"
  );

  let styled_narrow = cascade_with_container(css, 150.0, vec![]);
  assert_eq!(
    display(find_by_id(&styled_narrow, "t").expect("target")),
    "inline"
  );

  let styled_middle = cascade_with_container(css, 300.0, vec![]);
  assert_eq!(
    display(find_by_id(&styled_middle, "t").expect("target")),
    "block"
  );
}
