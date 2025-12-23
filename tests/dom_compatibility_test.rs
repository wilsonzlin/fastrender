use fastrender::dom::{parse_html, parse_html_with_options, DomNode, DomNodeType, DomParseOptions};

fn find_element<'a>(node: &'a DomNode, tag: &str) -> Option<&'a DomNode> {
  if matches!(node.tag_name(), Some(t) if t.eq_ignore_ascii_case(tag)) {
    return Some(node);
  }

  for child in &node.children {
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

fn collect_slots<'a>(node: &'a DomNode, out: &mut Vec<&'a DomNode>) {
  if matches!(node.node_type, DomNodeType::Slot { .. }) {
    out.push(node);
  }

  for child in &node.children {
    collect_slots(child, out);
  }
}

fn slot_texts_by_name(dom: &DomNode) -> Vec<(Option<String>, Vec<String>)> {
  let mut slots = Vec::new();
  collect_slots(dom, &mut slots);

  slots
    .into_iter()
    .map(|slot| {
      let name = match &slot.node_type {
        DomNodeType::Slot { attributes, .. } => attributes
          .iter()
          .find(|(k, _)| k.eq_ignore_ascii_case("name"))
          .map(|(_, v)| v.clone()),
        _ => None,
      };

      let mut texts = Vec::new();
      collect_text(slot, &mut texts);
      (name, texts)
    })
    .collect()
}

fn collect_text(node: &DomNode, texts: &mut Vec<String>) {
  if let DomNodeType::Text { content } = &node.node_type {
    texts.push(content.clone());
  }

  for child in &node.children {
    collect_text(child, texts);
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

  let standard_slots = slot_texts_by_name(&parse_html(html).expect("standard dom"));
  let compat_slots = slot_texts_by_name(
    &parse_html_with_options(html, DomParseOptions::compatibility()).expect("compat dom"),
  );

  assert_eq!(standard_slots, compat_slots);
  assert_eq!(
    standard_slots,
    vec![
      (Some("named".to_string()), vec!["named".to_string()]),
      (None, vec!["default".to_string()])
    ]
  );
}
