use fastrender::api::{FastRender, RenderOptions};
use serde_json::Value;

fn render_accessibility_json(html: &str) -> Value {
  let mut renderer = FastRender::new().expect("renderer");
  renderer
    .accessibility_tree_html_json(html, RenderOptions::new().with_viewport(800, 600))
    .expect("accessibility tree json")
}

fn find_json_node<'a>(node: &'a Value, id: &str) -> Option<&'a Value> {
  if node
    .get("id")
    .and_then(|v| v.as_str())
    .is_some_and(|v| v == id)
  {
    return Some(node);
  }

  if let Some(children) = node.get("children").and_then(|c| c.as_array()) {
    for child in children {
      if let Some(found) = find_json_node(child, id) {
        return Some(found);
      }
    }
  }

  None
}

#[test]
fn referenced_label_allows_display_none() {
  let html = r#"
    <html>
      <body>
        <div id="labelled" aria-labelledby="hidden-label" tabindex="0"></div>
        <span id="hidden-label" style="display:none">Hidden label text</span>
      </body>
    </html>
  "#;

  let tree = render_accessibility_json(html);
  let labelled = find_json_node(&tree, "labelled").expect("labelled node present");

  assert_eq!(
    labelled.get("name").and_then(|v| v.as_str()),
    Some("Hidden label text")
  );
  assert!(find_json_node(&tree, "hidden-label").is_none());
}

#[test]
fn referenced_description_allows_visibility_hidden() {
  let html = r#"
    <html>
      <body>
        <div id="described" aria-describedby="hidden-desc" tabindex="0"></div>
        <div id="hidden-desc" style="visibility:hidden">Hidden description text</div>
      </body>
    </html>
  "#;

  let tree = render_accessibility_json(html);
  let described = find_json_node(&tree, "described").expect("described node present");

  assert_eq!(
    described.get("description").and_then(|v| v.as_str()),
    Some("Hidden description text")
  );
  assert!(find_json_node(&tree, "hidden-desc").is_none());
}

#[test]
fn aria_hidden_references_are_ignored() {
  let html = r#"
    <html>
      <body>
        <div id="blocked-name" aria-labelledby="blocked-label" tabindex="0"></div>
        <div id="blocked-desc" aria-describedby="blocked-desc-text" tabindex="0"></div>

        <div id="blocked-label" style="display:none" aria-hidden="true">
          Hidden but aria-hidden
        </div>
        <div id="blocked-desc-text" style="visibility:hidden" aria-hidden="true">
          Description should be ignored
        </div>
      </body>
    </html>
  "#;

  let tree = render_accessibility_json(html);

  let blocked_name = find_json_node(&tree, "blocked-name").expect("blocked-name node present");
  assert_eq!(blocked_name.get("name").and_then(|v| v.as_str()), Some(""));

  let blocked_desc = find_json_node(&tree, "blocked-desc").expect("blocked-desc node present");
  assert_eq!(
    blocked_desc.get("description").and_then(|v| v.as_str()),
    None
  );

  assert!(find_json_node(&tree, "blocked-label").is_none());
  assert!(find_json_node(&tree, "blocked-desc-text").is_none());
}
