use fastrender::dom::{parse_html, DomNode, DomNodeType, ShadowRootMode};

fn find_by_id<'a>(node: &'a DomNode, id: &str) -> Option<&'a DomNode> {
  if node.get_attribute_ref("id") == Some(id) {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

fn find_first_slot<'a>(node: &'a DomNode) -> Option<&'a DomNode> {
  match node.node_type {
    DomNodeType::Slot { .. } => Some(node),
    _ => node.children.iter().find_map(find_first_slot),
  }
}

#[test]
fn declarative_shadow_dom_attaches_shadow_root() {
  let html =
    "<div id='host'><template shadowroot=\"open\"><span id='shadow'><slot></slot></span></template><p id='light'>Light</p></div>";
  let dom = parse_html(html).expect("parse html");

  let host = find_by_id(&dom, "host").expect("host element");
  assert_eq!(
    host.children.len(),
    1,
    "host should only expose shadow root child"
  );

  let shadow_root = &host.children[0];
  match shadow_root.node_type {
    DomNodeType::ShadowRoot { mode } => assert_eq!(mode, ShadowRootMode::Open),
    _ => panic!("expected shadow root child"),
  }

  let slot = find_first_slot(shadow_root).expect("slot in shadow root");
  assert_eq!(
    slot.children.len(),
    1,
    "slot should receive light DOM child"
  );
  assert_eq!(
    slot.children[0].get_attribute_ref("id"),
    Some("light"),
    "light DOM child should be assigned to default slot",
  );
}

#[test]
fn slot_uses_fallback_when_unassigned() {
  let html =
    "<div id='host'><template shadowroot='closed'><slot id='slot'>fallback</slot></template></div>";
  let dom = parse_html(html).expect("parse html");
  let host = find_by_id(&dom, "host").expect("host element");
  let shadow_root = host.children.first().expect("shadow root");
  match shadow_root.node_type {
    DomNodeType::ShadowRoot { mode } => assert_eq!(mode, ShadowRootMode::Closed),
    _ => panic!("expected shadow root"),
  }

  let slot = find_by_id(shadow_root, "slot").expect("slot element");
  assert!(slot.children.iter().any(
    |c| matches!(c.node_type, DomNodeType::Text { ref content } if content.contains("fallback"))
  ));
}

#[test]
fn named_slots_receive_matching_light_dom() {
  let html = "<div id='host'><template shadowroot='open'><slot name='title' id='title-slot'></slot><slot id='default-slot'></slot></template><span slot='title' id='title'>Title</span><span id='body'>Body</span></div>";
  let dom = parse_html(html).expect("parse html");
  let host = find_by_id(&dom, "host").expect("host element");
  let shadow_root = host.children.first().expect("shadow root");

  let title_slot = find_by_id(shadow_root, "title-slot").expect("title slot");
  assert_eq!(title_slot.children.len(), 1);
  assert_eq!(
    title_slot.children[0].get_attribute_ref("id"),
    Some("title")
  );

  let default_slot = find_by_id(shadow_root, "default-slot").expect("default slot");
  assert_eq!(default_slot.children.len(), 1);
  assert_eq!(
    default_slot.children[0].get_attribute_ref("id"),
    Some("body")
  );
}

#[test]
fn unmatched_named_content_falls_back_to_default_slot() {
  let html = "<div id='host'><template shadowroot='open'><slot id='default-slot'></slot></template><span slot='missing' id='named'>Named</span><span id='plain'>Plain</span></div>";
  let dom = parse_html(html).expect("parse html");
  let host = find_by_id(&dom, "host").expect("host element");
  let shadow_root = host.children.first().expect("shadow root");

  let default_slot = find_by_id(shadow_root, "default-slot").expect("default slot");
  let ids: Vec<_> = default_slot
    .children
    .iter()
    .filter_map(|c| c.get_attribute_ref("id"))
    .collect();

  assert_eq!(ids, vec!["named", "plain"]);
}
