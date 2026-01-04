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
  assert_eq!(host.children.len(), 1, "host should expose shadow root children");
  assert!(
    matches!(host.children[0].node_type, DomNodeType::Slot { .. }),
    "host child should be the shadow slot"
  );

  let slot = &host.children[0];
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

  let host = find_by_id(&snapshot, "host").expect("host element");
  let slot = find_by_tag(host, "slot").expect("slot element");
  assert_eq!(slot.children.len(), 1, "fallback content should be kept");
  assert_eq!(
    slot.children[0].get_attribute_ref("id"),
    Some("fallback"),
    "fallback node should remain under slot"
  );
}

#[test]
fn composed_snapshot_named_and_default_slots_use_assignment_and_suppress_fallback() {
  let html = r#"<div id="host"><template shadowroot="open"><slot name="title" id="title-slot"><span id="fallback-title">Fallback title</span></slot><slot id="default-slot"><span id="fallback-default">Fallback default</span></slot></template><span slot="title" id="title">Title</span><span id="body">Body</span></div>"#;
  let dom = dom::parse_html(html).expect("parse html");
  let ids = dom::enumerate_dom_ids(&dom);
  let assignment = dom::compute_slot_assignment_with_ids(&dom, &ids).expect("slot assignment");
  let snapshot =
    dom::composed_dom_snapshot_with_ids_and_assignment(&dom, &ids, &assignment).expect("snapshot");

  let host = find_by_id(&snapshot, "host").expect("host element");
  assert_eq!(host.children.len(), 2, "host should expose its shadow root children");
  let title_slot = find_by_id(host, "title-slot").expect("title slot");
  assert!(
    matches!(title_slot.node_type, DomNodeType::Slot { assigned: true, .. }),
    "slot should be marked assigned when it receives nodes"
  );
  assert_eq!(title_slot.children.len(), 1);
  assert_eq!(title_slot.children[0].get_attribute_ref("id"), Some("title"));
  assert!(
    find_by_id(title_slot, "fallback-title").is_none(),
    "fallback subtree should be suppressed when assigned"
  );

  let default_slot = find_by_id(host, "default-slot").expect("default slot");
  assert!(
    matches!(default_slot.node_type, DomNodeType::Slot { assigned: true, .. }),
    "default slot should be marked assigned when it receives nodes"
  );
  assert_eq!(default_slot.children.len(), 1);
  assert_eq!(default_slot.children[0].get_attribute_ref("id"), Some("body"));
  assert!(
    find_by_id(default_slot, "fallback-default").is_none(),
    "fallback subtree should be suppressed when assigned"
  );
}

#[test]
fn composed_snapshot_respects_nested_shadow_root_boundaries() {
  let html = r#"<div id="outer"><template shadowroot="open"><slot></slot><div id="inner-host"><template shadowroot="open"><slot name="inner"><span id="inner-fallback">fallback</span></slot></template></div></template><span id="outer-light" slot="inner">outer</span></div>"#;
  let dom = dom::parse_html(html).expect("parse html");
  let snapshot = dom::composed_dom_snapshot(&dom).expect("compose snapshot");

  let outer = find_by_id(&snapshot, "outer").expect("outer host");
  let outer_slot = outer
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
  let inner_slot = inner_host
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

#[test]
fn composed_snapshot_skips_inert_template_contents() {
  let html = r#"<div id="root"><template id="t"><span id="inert">X</span></template><span id="other">Y</span></div>"#;
  let dom = dom::parse_html(html).expect("parse html");
  let snapshot = dom::composed_dom_snapshot(&dom).expect("compose snapshot");

  let template = find_by_id(&snapshot, "t").expect("template element should remain in tree");
  assert!(
    find_by_id(&snapshot, "inert").is_none(),
    "template contents should be skipped in composed snapshots"
  );
  assert_eq!(
    template.children.len(),
    0,
    "template element should have no composed children"
  );
  assert!(
    find_by_id(&snapshot, "other").is_some(),
    "non-template siblings should still be traversed"
  );
}
