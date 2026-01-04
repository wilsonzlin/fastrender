use fastrender::dom::{
  compute_slot_assignment_with_ids, enumerate_dom_ids, parse_html, DomNode, DomNodeType,
  ShadowRootMode,
};
use fastrender::error::{Error, RenderError, RenderStage};
use fastrender::render_control::{with_deadline, RenderDeadline};
use std::collections::HashMap;
use std::time::Duration;

fn find_by_id<'a>(node: &'a DomNode, id: &str) -> Option<&'a DomNode> {
  if node.get_attribute_ref("id") == Some(id) {
    return Some(node);
  }
  for child in node.children.iter() {
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

fn find_shadow_root<'a>(node: &'a DomNode) -> Option<&'a DomNode> {
  match node.node_type {
    DomNodeType::ShadowRoot { .. } => Some(node),
    _ => node.children.iter().find_map(find_shadow_root),
  }
}

fn build_id_lookup<'a>(
  node: &'a DomNode,
  ids: &HashMap<*const DomNode, usize>,
  out: &mut HashMap<usize, &'a DomNode>,
) {
  if let Some(id) = ids.get(&(node as *const DomNode)) {
    out.insert(*id, node);
  }
  for child in node.children.iter() {
    build_id_lookup(child, ids, out);
  }
}

#[test]
fn declarative_shadow_dom_attaches_shadow_root() {
  let html =
    "<div id='host'><template shadowroot=\"open\"><span id='shadow'><slot></slot></span></template><p id='light'>Light</p></div>";
  let dom = parse_html(html).expect("parse html");
  let ids = enumerate_dom_ids(&dom);
  let assignments = compute_slot_assignment_with_ids(&dom, &ids).expect("slot assignment");
  let mut lookup = HashMap::new();
  build_id_lookup(&dom, &ids, &mut lookup);

  let host = find_by_id(&dom, "host").expect("host element");
  assert_eq!(
    host.children.len(),
    2,
    "host should expose shadow root and retain light DOM children"
  );

  let shadow_root = find_shadow_root(host).expect("shadow root child");
  match shadow_root.node_type {
    DomNodeType::ShadowRoot { mode, .. } => assert_eq!(mode, ShadowRootMode::Open),
    _ => panic!("expected shadow root child"),
  }

  let slot = find_first_slot(shadow_root).expect("slot in shadow root");
  let slot_id = *ids.get(&(slot as *const DomNode)).expect("slot id");
  let assigned = assignments
    .slot_to_nodes
    .get(&slot_id)
    .cloned()
    .unwrap_or_default();
  assert_eq!(assigned.len(), 1, "default slot should be assigned");
  let light_id = assigned[0];
  let light_node = lookup.get(&light_id).expect("assigned node");
  assert_eq!(light_node.get_attribute_ref("id"), Some("light"));
}

#[test]
fn slot_uses_fallback_when_unassigned() {
  let html =
    "<div id='host'><template shadowroot='closed'><slot id='slot'>fallback</slot></template></div>";
  let dom = parse_html(html).expect("parse html");
  let ids = enumerate_dom_ids(&dom);
  let assignments = compute_slot_assignment_with_ids(&dom, &ids).expect("slot assignment");
  let host = find_by_id(&dom, "host").expect("host element");
  let shadow_root = host.children.first().expect("shadow root");
  match shadow_root.node_type {
    DomNodeType::ShadowRoot { mode, .. } => assert_eq!(mode, ShadowRootMode::Closed),
    _ => panic!("expected shadow root"),
  }

  let slot = find_by_id(shadow_root, "slot").expect("slot element");
  let slot_id = *ids.get(&(slot as *const DomNode)).expect("slot id");
  assert!(
    !assignments.slot_to_nodes.contains_key(&slot_id),
    "slot should be unassigned"
  );
  assert!(slot.children.iter().any(
    |c| matches!(c.node_type, DomNodeType::Text { ref content } if content.contains("fallback"))
  ));
}

#[test]
fn named_slots_receive_matching_light_dom() {
  let html = "<div id='host'><template shadowroot='open'><slot name='title' id='title-slot'></slot><slot id='default-slot'></slot></template><span slot='title' id='title'>Title</span><span id='body'>Body</span></div>";
  let dom = parse_html(html).expect("parse html");
  let ids = enumerate_dom_ids(&dom);
  let assignments = compute_slot_assignment_with_ids(&dom, &ids).expect("slot assignment");
  let mut lookup = HashMap::new();
  build_id_lookup(&dom, &ids, &mut lookup);
  let host = find_by_id(&dom, "host").expect("host element");
  let shadow_root = host.children.first().expect("shadow root");

  let title_slot = find_by_id(shadow_root, "title-slot").expect("title slot");
  let title_slot_id = *ids.get(&(title_slot as *const DomNode)).expect("slot id");
  let title_assigned = assignments
    .slot_to_nodes
    .get(&title_slot_id)
    .cloned()
    .unwrap_or_default();
  assert_eq!(title_assigned.len(), 1);
  assert_eq!(
    lookup
      .get(&title_assigned[0])
      .and_then(|n| n.get_attribute_ref("id")),
    Some("title")
  );

  let default_slot = find_by_id(shadow_root, "default-slot").expect("default slot");
  let default_slot_id = *ids.get(&(default_slot as *const DomNode)).expect("slot id");
  let default_assigned = assignments
    .slot_to_nodes
    .get(&default_slot_id)
    .cloned()
    .unwrap_or_default();
  assert_eq!(default_assigned.len(), 1);
  assert_eq!(
    lookup
      .get(&default_assigned[0])
      .and_then(|n| n.get_attribute_ref("id")),
    Some("body")
  );
}

#[test]
fn unmatched_named_content_falls_back_to_default_slot() {
  let html = "<div id='host'><template shadowroot='open'><slot id='default-slot'></slot></template><span slot='missing' id='named'>Named</span><span id='plain'>Plain</span></div>";
  let dom = parse_html(html).expect("parse html");
  let ids = enumerate_dom_ids(&dom);
  let assignments = compute_slot_assignment_with_ids(&dom, &ids).expect("slot assignment");
  let mut lookup = HashMap::new();
  build_id_lookup(&dom, &ids, &mut lookup);
  let host = find_by_id(&dom, "host").expect("host element");
  let shadow_root = host.children.first().expect("shadow root");

  let default_slot = find_by_id(shadow_root, "default-slot").expect("default slot");
  let slot_id = *ids.get(&(default_slot as *const DomNode)).expect("slot id");
  let assigned = assignments
    .slot_to_nodes
    .get(&slot_id)
    .cloned()
    .unwrap_or_default();
  let assigned_ids: Vec<_> = assigned
    .iter()
    .filter_map(|id| lookup.get(id).and_then(|n| n.get_attribute_ref("id")))
    .collect();

  assert_eq!(assigned_ids, vec!["named", "plain"]);
}

#[test]
fn svg_template_is_not_declarative_shadow_dom() {
  let html = "<svg id='icon'><template shadowroot='open'><text>ignored</text></template></svg>";
  let dom = parse_html(html).expect("parse html");

  let svg = find_by_id(&dom, "icon").expect("svg element");
  assert!(
    find_shadow_root(svg).is_none(),
    "Templates in the SVG namespace should not attach shadow roots"
  );
}

#[test]
fn first_template_wins_for_multiple_declarative_shadow_roots() {
  let html = "<div id='host'><template shadowroot='open'><div id='first'>first</div></template><template shadowroot='closed'><div id='second'>second</div></template></div>";
  let dom = parse_html(html).expect("parse html");

  let host = find_by_id(&dom, "host").expect("host element");
  assert_eq!(
    host.children.len(),
    2,
    "host retains light DOM children alongside the attached shadow root"
  );

  let shadow_root = find_shadow_root(host).expect("shadow root child");
  match shadow_root.node_type {
    DomNodeType::ShadowRoot { mode, .. } => assert_eq!(mode, ShadowRootMode::Open),
    _ => panic!("expected shadow root child"),
  }

  assert!(
    find_by_id(shadow_root, "first").is_some(),
    "first template content should populate the shadow root"
  );
  assert!(
    find_by_id(shadow_root, "second").is_none(),
    "subsequent templates should not populate the shadow root"
  );
}

#[test]
fn nested_shadow_root_inside_unused_declarative_template_is_not_attached() {
  let html = "<div id='host'>\n  <template shadowroot='open'>\n    <div id='shadow-ok'></div>\n  </template>\n  <template shadowroot='closed'>\n    <div id='inner-host'>\n      <template shadowroot='open'>\n        <div id='should-not-attach'></div>\n      </template>\n    </div>\n  </template>\n</div>";
  let dom = parse_html(html).expect("parse html");

  let host = find_by_id(&dom, "host").expect("host element");
  let shadow_roots: Vec<&DomNode> = host
    .children
    .iter()
    .filter(|child| matches!(child.node_type, DomNodeType::ShadowRoot { .. }))
    .collect();
  assert_eq!(
    shadow_roots.len(),
    1,
    "only the first declarative shadow template should attach"
  );

  let inner_host = find_by_id(&dom, "inner-host").expect("inner-host element");
  assert!(
    inner_host
      .children
      .iter()
      .all(|child| !matches!(child.node_type, DomNodeType::ShadowRoot { .. })),
    "nested shadow roots should not attach inside inert declarative template contents"
  );

  let unused_template = host
    .children
    .iter()
    .find(|child| {
      child
        .tag_name()
        .map(|name| name.eq_ignore_ascii_case("template"))
        .unwrap_or(false)
    })
    .expect("unused declarative template should remain in light DOM");
  assert!(
    find_shadow_root(unused_template).is_none(),
    "unused declarative templates must remain inert for shadow root attachment"
  );
}

#[test]
fn nested_shadow_root_inside_first_declarative_template_is_attached() {
  let html = "<div id='host'>\n  <template shadowroot='open'>\n    <div id='inner-host'>\n      <template shadowroot='open'>\n        <div id='nested-shadow'></div>\n      </template>\n    </div>\n  </template>\n</div>";
  let dom = parse_html(html).expect("parse html");

  let inner_host = find_by_id(&dom, "inner-host").expect("inner host");
  let nested_shadow = inner_host
    .children
    .iter()
    .find(|child| matches!(child.node_type, DomNodeType::ShadowRoot { .. }))
    .expect("nested shadow root should attach inside promoted template contents");
  assert!(
    find_by_id(nested_shadow, "nested-shadow").is_some(),
    "nested shadow root should be populated from the inner declarative template"
  );
}

#[test]
fn nested_default_slot_in_fallback_prefers_outer_slot() {
  let html = "<div id='host'><template shadowroot='open'><slot id='outer'><div><slot id='inner'></slot></div></slot></template><span id='light'>X</span></div>";
  let dom = parse_html(html).expect("parse html");
  let ids = enumerate_dom_ids(&dom);
  let assignments = compute_slot_assignment_with_ids(&dom, &ids).expect("slot assignment");
  let mut lookup = HashMap::new();
  build_id_lookup(&dom, &ids, &mut lookup);
  let host = find_by_id(&dom, "host").expect("host element");
  let shadow_root = host.children.first().expect("shadow root");

  let outer_slot = find_by_id(shadow_root, "outer").expect("outer slot");
  let outer_slot_id = *ids.get(&(outer_slot as *const DomNode)).expect("slot id");
  let assigned = assignments
    .slot_to_nodes
    .get(&outer_slot_id)
    .cloned()
    .unwrap_or_default();
  assert_eq!(assigned.len(), 1);
  assert_eq!(
    lookup
      .get(&assigned[0])
      .and_then(|n| n.get_attribute_ref("id")),
    Some("light")
  );

  let inner_slot = find_by_id(shadow_root, "inner").expect("inner slot");
  let inner_slot_id = *ids.get(&(inner_slot as *const DomNode)).expect("inner id");
  assert!(
    !assignments.slot_to_nodes.contains_key(&inner_slot_id),
    "fallback subtree should be ignored when the outer slot is assigned"
  );
}

#[test]
fn nested_named_slots_in_fallback_receive_assignments_when_outer_is_unassigned() {
  let html = "<div id='host'><template shadowroot='open'><slot name='outer' id='outer'><slot name='inner' id='inner'></slot></slot></template><span slot='inner' id='light-inner'>Y</span></div>";
  let dom = parse_html(html).expect("parse html");
  let ids = enumerate_dom_ids(&dom);
  let assignments = compute_slot_assignment_with_ids(&dom, &ids).expect("slot assignment");
  let mut lookup = HashMap::new();
  build_id_lookup(&dom, &ids, &mut lookup);
  let host = find_by_id(&dom, "host").expect("host element");
  let shadow_root = host.children.first().expect("shadow root");

  let outer_slot = find_by_id(shadow_root, "outer").expect("outer slot");
  let outer_slot_id = *ids.get(&(outer_slot as *const DomNode)).expect("outer id");
  assert!(
    !assignments.slot_to_nodes.contains_key(&outer_slot_id),
    "outer slot should retain fallback when unassigned"
  );

  let inner_slot = find_by_id(shadow_root, "inner").expect("inner slot");
  let inner_slot_id = *ids.get(&(inner_slot as *const DomNode)).expect("inner id");
  let assigned = assignments
    .slot_to_nodes
    .get(&inner_slot_id)
    .cloned()
    .unwrap_or_default();
  assert_eq!(assigned.len(), 1);
  assert_eq!(
    lookup
      .get(&assigned[0])
      .and_then(|n| n.get_attribute_ref("id")),
    Some("light-inner")
  );
}

#[test]
fn nested_shadow_root_slots_do_not_capture_outer_light_dom() {
  let html = r#"
    <div id='outer-host'>
      <template shadowroot='open'>
        <x-inner id='inner-host'>
          <template shadowroot='open'>
            <slot name='inner' id='inner-slot'></slot>
          </template>
        </x-inner>
        <slot name='outer' id='outer-slot'></slot>
        <slot id='default-slot'></slot>
      </template>
      <span slot='inner' id='light-inner'>A</span>
      <span slot='outer' id='light-outer'>B</span>
      <span id='light-default'>C</span>
    </div>
  "#;
  let dom = parse_html(html).expect("parse html");
  let ids = enumerate_dom_ids(&dom);
  let assignments = compute_slot_assignment_with_ids(&dom, &ids).expect("slot assignment");
  let mut lookup = HashMap::new();
  build_id_lookup(&dom, &ids, &mut lookup);

  let outer_slot = find_by_id(&dom, "outer-slot").expect("outer slot");
  let outer_slot_id = *ids
    .get(&(outer_slot as *const DomNode))
    .expect("outer slot id");
  let outer_assigned = assignments
    .slot_to_nodes
    .get(&outer_slot_id)
    .cloned()
    .unwrap_or_default();
  assert_eq!(
    outer_assigned
      .iter()
      .filter_map(|id| lookup.get(id).and_then(|n| n.get_attribute_ref("id")))
      .collect::<Vec<_>>(),
    vec!["light-outer"]
  );

  let default_slot = find_by_id(&dom, "default-slot").expect("default slot");
  let default_slot_id = *ids
    .get(&(default_slot as *const DomNode))
    .expect("default slot id");
  let default_assigned = assignments
    .slot_to_nodes
    .get(&default_slot_id)
    .cloned()
    .unwrap_or_default();
  assert_eq!(
    default_assigned
      .iter()
      .filter_map(|id| lookup.get(id).and_then(|n| n.get_attribute_ref("id")))
      .collect::<Vec<_>>(),
    vec!["light-inner", "light-default"]
  );

  let inner_slot = find_by_id(&dom, "inner-slot").expect("inner slot");
  let inner_slot_id = *ids
    .get(&(inner_slot as *const DomNode))
    .expect("inner slot id");
  assert!(
    !assignments.slot_to_nodes.contains_key(&inner_slot_id),
    "slots inside nested shadow roots should not be assigned by outer host distribution"
  );
}

#[test]
fn nested_shadow_root_slot_names_do_not_affect_outer_default_slot() {
  let html = r#"
    <div id='host'>
      <template shadowroot='open'>
        <slot id='outer-default'></slot>
        <x-inner id='inner-host'>
          <template shadowroot='open'>
            <slot name='foo' id='inner-slot'></slot>
          </template>
        </x-inner>
      </template>
      <span slot='foo' id='light'>Light</span>
    </div>
  "#;
  let dom = parse_html(html).expect("parse html");
  let ids = enumerate_dom_ids(&dom);
  let assignments = compute_slot_assignment_with_ids(&dom, &ids).expect("slot assignment");
  let mut lookup = HashMap::new();
  build_id_lookup(&dom, &ids, &mut lookup);

  let outer_slot = find_by_id(&dom, "outer-default").expect("outer default slot");
  let outer_slot_id = *ids.get(&(outer_slot as *const DomNode)).expect("outer slot id");
  let assigned = assignments
    .slot_to_nodes
    .get(&outer_slot_id)
    .cloned()
    .unwrap_or_default();
  assert_eq!(assigned.len(), 1);
  assert_eq!(
    lookup
      .get(&assigned[0])
      .and_then(|n| n.get_attribute_ref("id")),
    Some("light")
  );

  let inner_slot = find_by_id(&dom, "inner-slot").expect("inner slot");
  let inner_slot_id = *ids.get(&(inner_slot as *const DomNode)).expect("inner slot id");
  assert!(
    !assignments.slot_to_nodes.contains_key(&inner_slot_id),
    "slots inside nested shadow roots must never receive outer host assignments"
  );
}

#[test]
fn slots_inside_inert_template_are_ignored_for_assignment() {
  let html = r#"
    <div id='host'>
      <template shadowroot='open'>
        <template>
          <slot name='foo' id='slot-in-template'></slot>
        </template>
        <slot name='foo' id='real-slot'></slot>
      </template>
      <span slot='foo' id='light-foo'>X</span>
    </div>
  "#;
  let dom = parse_html(html).expect("parse html");
  let ids = enumerate_dom_ids(&dom);
  let assignments = compute_slot_assignment_with_ids(&dom, &ids).expect("slot assignment");
  let mut lookup = HashMap::new();
  build_id_lookup(&dom, &ids, &mut lookup);

  let real_slot = find_by_id(&dom, "real-slot").expect("real slot");
  let real_slot_id = *ids
    .get(&(real_slot as *const DomNode))
    .expect("real slot id");
  let real_assigned = assignments
    .slot_to_nodes
    .get(&real_slot_id)
    .cloned()
    .unwrap_or_default();
  assert_eq!(
    real_assigned
      .iter()
      .filter_map(|id| lookup.get(id).and_then(|n| n.get_attribute_ref("id")))
      .collect::<Vec<_>>(),
    vec!["light-foo"]
  );

  let slot_in_template = find_by_id(&dom, "slot-in-template").expect("slot in inert template");
  let slot_in_template_id = *ids
    .get(&(slot_in_template as *const DomNode))
    .expect("slot-in-template id");
  assert!(
    !assignments.slot_to_nodes.contains_key(&slot_in_template_id),
    "slots inside inert template contents should not be considered for assignment"
  );
}

#[test]
fn slot_names_inside_inert_templates_do_not_affect_distribution() {
  let html = r#"
    <div id='host'>
      <template shadowroot='open'>
        <template>
          <slot name='foo' id='template-slot'></slot>
        </template>
        <slot id='default-slot'></slot>
      </template>
      <span slot='foo' id='light'>Light</span>
    </div>
  "#;
  let dom = parse_html(html).expect("parse html");
  let ids = enumerate_dom_ids(&dom);
  let assignments = compute_slot_assignment_with_ids(&dom, &ids).expect("slot assignment");
  let mut lookup = HashMap::new();
  build_id_lookup(&dom, &ids, &mut lookup);

  let default_slot = find_by_id(&dom, "default-slot").expect("default slot");
  let default_slot_id = *ids
    .get(&(default_slot as *const DomNode))
    .expect("default slot id");
  let assigned = assignments
    .slot_to_nodes
    .get(&default_slot_id)
    .cloned()
    .unwrap_or_default();
  assert_eq!(assigned.len(), 1);
  assert_eq!(
    lookup
      .get(&assigned[0])
      .and_then(|n| n.get_attribute_ref("id")),
    Some("light")
  );

  let template_slot = find_by_id(&dom, "template-slot").expect("slot in inert template");
  let template_slot_id = *ids
    .get(&(template_slot as *const DomNode))
    .expect("slot in inert template id");
  assert!(
    !assignments.slot_to_nodes.contains_key(&template_slot_id),
    "slot elements inside inert templates must never receive assignments"
  );
}

#[test]
fn slot_assignment_times_out_under_expired_deadline() {
  let depth = 4096usize;
  let mut html = String::new();
  html.push_str("<div id='host'><template shadowroot='open'>");
  for _ in 0..depth {
    html.push_str("<div>");
  }
  html.push_str("<slot></slot>");
  for _ in 0..depth {
    html.push_str("</div>");
  }
  html.push_str("</template><span id='light'>Light</span></div>");

  let dom = parse_html(&html).expect("parse html");
  let ids = enumerate_dom_ids(&dom);

  let deadline = RenderDeadline::new(Some(Duration::from_millis(0)), None);
  let result = with_deadline(Some(&deadline), || compute_slot_assignment_with_ids(&dom, &ids));
  let err = result.expect_err("expected slot assignment timeout");
  match err {
    Error::Render(RenderError::Timeout { stage, .. }) => assert_eq!(stage, RenderStage::Cascade),
    other => panic!("expected timeout error, got {other:?}"),
  }
}
