use fastrender::css::parser::{extract_css_sources, CssTreeScope, StylesheetSource};
use fastrender::dom::{parse_html, DomNode, DomNodeType};

fn node_contributes_to_scope_path(node: &DomNode) -> bool {
  !matches!(node.node_type, DomNodeType::Text { .. } | DomNodeType::Document)
}

fn element_path_by_id(root: &DomNode, id: &str) -> Option<Vec<usize>> {
  fn walk(node: &DomNode, id: &str, path: &mut Vec<usize>) -> Option<Vec<usize>> {
    if node
      .get_attribute_ref("id")
      .map(|value| value == id)
      .unwrap_or(false)
    {
      return Some(path.clone());
    }

    let mut child_index = 0;
    for child in &node.children {
      let contributes = node_contributes_to_scope_path(child);
      if contributes {
        path.push(child_index);
        child_index += 1;
      }

      if let Some(found) = walk(child, id, path) {
        return Some(found);
      }

      if contributes {
        path.pop();
      }
    }

    None
  }

  walk(root, id, &mut Vec::new())
}

#[test]
fn extract_css_sources_scopes_shadow_root_styles() {
  let html = r#"
    <style>body { color: green; }</style>
    <div id="host">
      <template shadowroot="open">
        <style>.scoped { color: purple; }</style>
        <span class="scoped">shadow content</span>
      </template>
    </div>
  "#;

  let dom = parse_html(html).expect("parse HTML");
  let sources = extract_css_sources(&dom);
  assert_eq!(sources.len(), 2);

  let document_style = &sources[0];
  assert_eq!(document_style.scope, CssTreeScope::Document);
  match &document_style.source {
    StylesheetSource::Inline(inline) => assert!(inline.css.contains("body")),
    other => panic!("expected inline document style, got {:?}", other),
  }

  let shadow_style = &sources[1];
  let host_path = match &shadow_style.scope {
    CssTreeScope::ShadowRoot { host_path } => host_path,
    other => panic!("expected shadow root scope, got {:?}", other),
  };
  let expected_host_path =
    element_path_by_id(&dom, "host").expect("host element path should exist");
  assert_eq!(*host_path, expected_host_path);
  match &shadow_style.source {
    StylesheetSource::Inline(inline) => assert!(inline.css.contains(".scoped")),
    other => panic!("expected inline shadow style, got {:?}", other),
  }
}
