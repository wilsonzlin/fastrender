use fastrender::dom::{
  compute_slot_assignment_with_ids, enumerate_dom_ids, parse_html, parse_html_with_options, DomNode,
  DomNodeType, DomParseOptions,
};
use std::collections::HashMap;

fn find_element<'a>(node: &'a DomNode, tag: &str) -> Option<&'a DomNode> {
  if matches!(node.tag_name(), Some(t) if t.eq_ignore_ascii_case(tag)) {
    return Some(node);
  }

  for child in node.children.iter() {
    if let Some(found) = find_element(child, tag) {
      return Some(found);
    }
  }

  None
}

fn collect_classes(node: &DomNode) -> Vec<String> {
  match &node.node_type {
    DomNodeType::Element { attributes, .. } | DomNodeType::Slot { attributes, .. } => attributes
      .iter()
      .find(|(k, _)| k.eq_ignore_ascii_case("class"))
      .map(|(_, v)| v.split_whitespace().map(|s| s.to_string()).collect())
      .unwrap_or_default(),
    _ => Vec::new(),
  }
}

fn find_by_id<'a>(node: &'a DomNode, id: &str) -> Option<&'a DomNode> {
  if node.has_id(id) {
    return Some(node);
  }
  node.children.iter().find_map(|child| find_by_id(child, id))
}

fn find_slot_by_name<'a>(node: &'a DomNode, slot_name: Option<&str>) -> Option<&'a DomNode> {
  if matches!(node.node_type, DomNodeType::Slot { .. }) && node.get_attribute_ref("name") == slot_name
  {
    return Some(node);
  }
  node
    .children
    .iter()
    .find_map(|child| find_slot_by_name(child, slot_name))
}

fn build_id_to_node<'a>(
  node: &'a DomNode,
  ids: &HashMap<*const DomNode, usize>,
  out: &mut HashMap<usize, &'a DomNode>,
) {
  if let Some(id) = ids.get(&(node as *const DomNode)) {
    out.insert(*id, node);
  }
  for child in node.children.iter() {
    build_id_to_node(child, ids, out);
  }
}

#[test]
fn compatibility_mode_flips_expected_classes() {
  let html = "<html class='no-js foo'><body class='bar'></body></html>";

  let standard_dom = parse_html(html).expect("parse standard DOM");
  let compat_dom =
    parse_html_with_options(html, DomParseOptions::compatibility()).expect("parse compat DOM");

  let standard_html = find_element(&standard_dom, "html").expect("standard html element");
  let standard_body = find_element(standard_html, "body").expect("standard body element");

  let compat_html = find_element(&compat_dom, "html").expect("compat html element");
  let compat_body = find_element(compat_html, "body").expect("compat body element");

  let standard_html_classes = collect_classes(standard_html);
  assert!(standard_html_classes.contains(&"no-js".to_string()));
  assert!(!standard_html_classes.contains(&"js-enabled".to_string()));
  assert!(!standard_html_classes.contains(&"jsl10n-visible".to_string()));

  let standard_body_classes = collect_classes(standard_body);
  assert!(!standard_body_classes.contains(&"jsl10n-visible".to_string()));

  let compat_html_classes = collect_classes(compat_html);
  assert!(!compat_html_classes.contains(&"no-js".to_string()));
  assert!(compat_html_classes.contains(&"js-enabled".to_string()));
  assert!(compat_html_classes.contains(&"foo".to_string()));
  assert!(compat_html_classes.contains(&"jsl10n-visible".to_string()));

  let compat_body_classes = collect_classes(compat_body);
  assert!(compat_body_classes.contains(&"bar".to_string()));
  assert!(compat_body_classes.contains(&"jsl10n-visible".to_string()));
}

#[test]
fn compatibility_mode_preserves_shadow_slot_distribution() {
  let html = "<html><body><div id='host'><template shadowroot='open'><div id='shadow'><slot name='named'></slot><slot></slot></div></template><span slot='named'>named</span><span>default</span></div></body></html>";

  let standard_dom = parse_html(html).expect("standard dom");
  let compat_dom =
    parse_html_with_options(html, DomParseOptions::compatibility()).expect("compat dom");

  let standard_ids = enumerate_dom_ids(&standard_dom);
  let compat_ids = enumerate_dom_ids(&compat_dom);

  let standard_assignment = compute_slot_assignment_with_ids(&standard_dom, &standard_ids);
  let compat_assignment = compute_slot_assignment_with_ids(&compat_dom, &compat_ids);

  let mut standard_id_to_node = HashMap::new();
  build_id_to_node(&standard_dom, &standard_ids, &mut standard_id_to_node);
  let mut compat_id_to_node = HashMap::new();
  build_id_to_node(&compat_dom, &compat_ids, &mut compat_id_to_node);

  let standard_host = find_by_id(&standard_dom, "host").expect("standard host element");
  assert!(
    standard_host.is_shadow_host(),
    "standard parse should attach declarative shadow root"
  );
  let compat_host = find_by_id(&compat_dom, "host").expect("compat host element");
  assert!(
    compat_host.is_shadow_host(),
    "compat parse should attach declarative shadow root"
  );

  assert_eq!(
    standard_assignment.slot_to_nodes, compat_assignment.slot_to_nodes,
    "compatibility mode should not alter slot assignment"
  );

  let standard_shadow_root = standard_host
    .children
    .iter()
    .find(|child| matches!(child.node_type, DomNodeType::ShadowRoot { .. }))
    .expect("standard shadow root");
  let named_slot = find_slot_by_name(standard_shadow_root, Some("named")).expect("named slot");
  let default_slot = find_slot_by_name(standard_shadow_root, None).expect("default slot");
  let named_slot_id = *standard_ids
    .get(&(named_slot as *const DomNode))
    .expect("named slot id");
  let default_slot_id = *standard_ids
    .get(&(default_slot as *const DomNode))
    .expect("default slot id");

  let named_span = standard_host
    .children
    .iter()
    .find(|child| {
      matches!(child.tag_name(), Some(t) if t.eq_ignore_ascii_case("span"))
        && child.get_attribute_ref("slot") == Some("named")
    })
    .expect("named light DOM span");
  let default_span = standard_host
    .children
    .iter()
    .find(|child| {
      matches!(child.tag_name(), Some(t) if t.eq_ignore_ascii_case("span"))
        && child.get_attribute_ref("slot").is_none()
    })
    .expect("default light DOM span");
  let named_span_id = *standard_ids
    .get(&(named_span as *const DomNode))
    .expect("named span id");
  let default_span_id = *standard_ids
    .get(&(default_span as *const DomNode))
    .expect("default span id");

  let assigned_named = standard_assignment
    .slot_to_nodes
    .get(&named_slot_id)
    .cloned()
    .unwrap_or_default();
  assert_eq!(assigned_named, vec![named_span_id]);
  assert_eq!(
    standard_id_to_node
      .get(&assigned_named[0])
      .and_then(|node| node.get_attribute_ref("slot")),
    Some("named")
  );

  let assigned_default = standard_assignment
    .slot_to_nodes
    .get(&default_slot_id)
    .cloned()
    .unwrap_or_default();
  assert_eq!(assigned_default, vec![default_span_id]);
  assert_eq!(
    standard_id_to_node
      .get(&assigned_default[0])
      .and_then(|node| node.get_attribute_ref("slot")),
    None
  );

  // Ensure the compat id lookup stays in sync with the assignment ids so future debug assertions
  // can safely translate them back into nodes.
  assert!(compat_assignment
    .slot_to_nodes
    .values()
    .flatten()
    .all(|id| compat_id_to_node.contains_key(id)));
}
