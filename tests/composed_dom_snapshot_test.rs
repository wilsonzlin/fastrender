use fastrender::dom::{self, DomNode, DomNodeType};

fn find_by_id<'a>(root: &'a DomNode, id: &str) -> Option<&'a DomNode> {
  let mut stack = vec![root];
  while let Some(node) = stack.pop() {
    if node.get_attribute_ref("id") == Some(id) {
      return Some(node);
    }
    for child in node.children.iter().rev() {
      stack.push(child);
    }
  }
  None
}

fn find_by_tag<'a>(root: &'a DomNode, tag: &str) -> Option<&'a DomNode> {
  let mut stack = vec![root];
  while let Some(node) = stack.pop() {
    if node
      .tag_name()
      .is_some_and(|name| name.eq_ignore_ascii_case(tag))
    {
      return Some(node);
    }
    for child in node.children.iter().rev() {
      stack.push(child);
    }
  }
  None
}

#[test]
fn composed_snapshot_places_slotted_nodes_under_slot() {
  let html = r#"<div id="host"><template shadowroot="open"><slot></slot></template><span id="light">hi</span></div>"#;
  let dom = dom::parse_html(html).expect("parse html");
  let snapshot = dom::composed_dom_snapshot(&dom).expect("compose snapshot");

  let host = find_by_id(&snapshot, "host").expect("host element");
  assert_eq!(host.children.len(), 1, "shadow host should expose only its shadow root");
  assert!(
    matches!(host.children[0].node_type, DomNodeType::ShadowRoot { .. }),
    "host child should be shadow root"
  );

  let shadow_root = &host.children[0];
  let slot = find_by_tag(shadow_root, "slot").expect("slot element");
  assert_eq!(slot.children.len(), 1, "slot should contain the assigned node");
  assert_eq!(
    slot.children[0].get_attribute_ref("id"),
    Some("light"),
    "assigned node should appear under slot"
  );
}

#[test]
fn composed_snapshot_slot_fallback_is_used_when_unassigned() {
  let html = r#"<div id="host"><template shadowroot="open"><slot><span id="fallback">fb</span></slot></template></div>"#;
  let dom = dom::parse_html(html).expect("parse html");
  let snapshot = dom::composed_dom_snapshot(&dom).expect("compose snapshot");

  let slot = find_by_tag(&snapshot, "slot").expect("slot element");
  assert_eq!(slot.children.len(), 1, "fallback content should be kept");
  assert_eq!(
    slot.children[0].get_attribute_ref("id"),
    Some("fallback"),
    "fallback node should remain under slot"
  );
}

#[test]
fn composed_snapshot_respects_nested_shadow_root_boundaries() {
  let html = r#"<div id="outer"><template shadowroot="open"><slot></slot><div id="inner-host"><template shadowroot="open"><slot name="inner"><span id="inner-fallback">fallback</span></slot></template></div></template><span id="outer-light" slot="inner">outer</span></div>"#;
  let dom = dom::parse_html(html).expect("parse html");
  let snapshot = dom::composed_dom_snapshot(&dom).expect("compose snapshot");

  let outer = find_by_id(&snapshot, "outer").expect("outer host");
  assert!(
    matches!(outer.children.first().map(|c| &c.node_type), Some(DomNodeType::ShadowRoot { .. })),
    "outer host should expose its shadow root"
  );
  let outer_shadow = &outer.children[0];
  let outer_slot = outer_shadow
    .children
    .iter()
    .find(|child| matches!(child.node_type, DomNodeType::Slot { .. }))
    .expect("outer default slot");
  assert_eq!(
    outer_slot
      .children
      .first()
      .and_then(|node| node.get_attribute_ref("id")),
    Some("outer-light"),
    "outer light DOM should be assigned to the outer default slot"
  );

  let inner_host = find_by_id(&snapshot, "inner-host").expect("inner host");
  assert!(
    matches!(
      inner_host.children.first().map(|c| &c.node_type),
      Some(DomNodeType::ShadowRoot { .. })
    ),
    "inner host should expose its shadow root"
  );
  let inner_shadow = &inner_host.children[0];
  let inner_slot = inner_shadow
    .children
    .iter()
    .find(|child| matches!(child.node_type, DomNodeType::Slot { .. }))
    .expect("inner slot");
  assert_eq!(
    inner_slot
      .children
      .first()
      .and_then(|node| node.get_attribute_ref("id")),
    Some("inner-fallback"),
    "inner slot should use fallback when it has no light DOM assigned"
  );
}

