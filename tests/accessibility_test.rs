use fastrender::accessibility::{AccessibilityNode, CheckState, PressedState};
use fastrender::api::{FastRender, RenderOptions};
use serde_json::{json, Value};
use std::fs;

fn find_by_id<'a>(node: &'a AccessibilityNode, id: &str) -> Option<&'a AccessibilityNode> {
  if node.id.as_deref() == Some(id) {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

fn collect_by_role<'a>(
  node: &'a AccessibilityNode,
  role: &str,
  out: &mut Vec<&'a AccessibilityNode>,
) {
  if node.role == role {
    out.push(node);
  }
  for child in &node.children {
    collect_by_role(child, role, out);
  }
}

fn count_by_id(node: &AccessibilityNode, id: &str) -> usize {
  let mut count = 0usize;
  if node.id.as_deref() == Some(id) {
    count += 1;
  }
  for child in &node.children {
    count += count_by_id(child, id);
  }
  count
}

fn find_path<'a>(node: &'a AccessibilityNode, id: &str) -> Option<Vec<&'a AccessibilityNode>> {
  if node.id.as_deref() == Some(id) {
    return Some(vec![node]);
  }
  for child in &node.children {
    if let Some(mut path) = find_path(child, id) {
      path.insert(0, node);
      return Some(path);
    }
  }
  None
}

fn render_accessibility_json(html: &str) -> Value {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(html).expect("parse");
  let json = renderer
    .accessibility_tree_json(&dom, 800, 600)
    .expect("accessibility tree json");
  serde_json::from_str(&json).expect("parse json")
}

fn render_accessibility_json_with_options(html: &str, options: RenderOptions) -> Value {
  let mut renderer = FastRender::new().expect("renderer");
  renderer
    .accessibility_tree_html_json(html, options)
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

fn snapshot_subset(root: &Value, ids: &[&str]) -> Value {
  let mut out = serde_json::Map::new();
  for id in ids {
    if let Some(node) = find_json_node(root, id) {
      let mut entry = serde_json::Map::new();
      entry.insert(
        "role".into(),
        node.get("role").cloned().unwrap_or(Value::Null),
      );
      if let Some(role_description) = node.get("role_description") {
        entry.insert("role_description".into(), role_description.clone());
      }
      entry.insert(
        "name".into(),
        node.get("name").cloned().unwrap_or(Value::Null),
      );
      entry.insert(
        "description".into(),
        node.get("description").cloned().unwrap_or(Value::Null),
      );
      entry.insert(
        "value".into(),
        node.get("value").cloned().unwrap_or(Value::Null),
      );
      entry.insert(
        "level".into(),
        node.get("level").cloned().unwrap_or(Value::Null),
      );
      entry.insert(
        "html_tag".into(),
        node.get("html_tag").cloned().unwrap_or(Value::Null),
      );
      entry.insert(
        "states".into(),
        node.get("states").cloned().unwrap_or_else(|| json!({})),
      );
      out.insert((*id).to_string(), Value::Object(entry));
    }
  }

  Value::Object(out)
}

#[test]
fn accessibility_roles_and_states_basic() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r##"
    <html>
      <body>
        <button id="btn">Press me</button>
        <input id="check" type="checkbox" checked aria-label="Check me" />
        <a id="link" href="#" data-fastr-visited="true">Go</a>
        <img id="logo" src="example.png" alt="Logo text" />
      </body>
    </html>
  "##;
  let dom = renderer.parse_html(html).expect("parse");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");

  let btn = find_by_id(&tree, "btn").expect("button node");
  assert_eq!(btn.role, "button");
  assert_eq!(btn.name.as_deref(), Some("Press me"));
  assert!(btn.states.focusable);
  assert!(!btn.states.disabled);

  let checkbox = find_by_id(&tree, "check").expect("checkbox node");
  assert_eq!(checkbox.role, "checkbox");
  assert_eq!(checkbox.states.checked, Some(CheckState::True));

  let link = find_by_id(&tree, "link").expect("link node");
  assert_eq!(link.role, "link");
  assert!(link.states.visited);
  assert!(link.states.focusable);

  let img = find_by_id(&tree, "logo").expect("image node");
  assert_eq!(img.role, "img");
  assert_eq!(img.name.as_deref(), Some("Logo text"));
}

#[test]
fn accessibility_decorative_images_with_empty_alt_are_hidden() {
  let html = r##"
    <html>
      <body>
        <img id="decor" alt="" src="x" />
        <img id="unknown" alt="" role="unknown" src="x" />
        <img id="labeled" alt="" aria-label="Logo" src="x" />
      </body>
    </html>
  "##;

  let tree = render_accessibility_json(html);

  assert!(
    find_json_node(&tree, "decor").is_none(),
    "decorative image should be omitted"
  );
  assert!(
    find_json_node(&tree, "unknown").is_none(),
    "invalid role tokens should not prevent decorative handling"
  );

  let labeled = find_json_node(&tree, "labeled").expect("labeled image present");
  assert_eq!(labeled.get("role").and_then(|v| v.as_str()), Some("img"));
  assert_eq!(labeled.get("name").and_then(|v| v.as_str()), Some("Logo"));
}

#[test]
fn accessibility_role_tokenization_uses_first_token() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r##"
    <html>
      <body>
        <div id="x" role="button link"></div>
        <div id="multi" role="button link">Press</div>
        <div id="uppercase" role="  LINK  ">Go</div>
        <div id="fallback" role="unknown button">Click</div>
      </body>
    </html>
  "##;
  let dom = renderer.parse_html(html).expect("parse");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");

  let node = find_by_id(&tree, "x").expect("node with id x");
  assert_eq!(node.role, "button");

  let multi = find_by_id(&tree, "multi").expect("multi node");
  assert_eq!(multi.role, "button");

  let uppercase = find_by_id(&tree, "uppercase").expect("uppercase node");
  assert_eq!(uppercase.role, "link");

  let fallback = find_by_id(&tree, "fallback").expect("fallback node");
  assert_eq!(fallback.role, "button");
}

#[test]
fn accessibility_presentational_role_suppresses_semantics() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r##"
    <html>
      <body>
        <div id="parent">
          <div id="x" role="none">Text</div>
          <div id="labeled" role="presentation" aria-label="Labelled"></div>
        </div>
        <button id="presentational" role="presentation">Native</button>
      </body>
    </html>
  "##;
  let dom = renderer.parse_html(html).expect("parse");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");

  assert!(find_by_id(&tree, "x").is_none());
  let parent = find_by_id(&tree, "parent").expect("parent node");
  assert_eq!(parent.name.as_deref(), Some("Text"));

  let labeled = find_by_id(&tree, "labeled").expect("presentational with label");
  assert_eq!(labeled.name.as_deref(), Some("Labelled"));

  let presentational = find_by_id(&tree, "presentational").expect("presentational node");
  assert_eq!(presentational.role, "generic");
  assert_eq!(presentational.name, None);
  assert!(presentational.states.focusable);
}

#[test]
fn accessibility_details_expanded_state() {
  let html = r##"
    <html>
      <body>
        <details id="d1" open>
          <summary id="s">More</summary>
          <div>Content</div>
        </details>
        <details id="d2">
          <summary id="s2">More</summary>
        </details>
      </body>
    </html>
  "##;

  let tree = render_accessibility_json(html);

  let d1 = find_json_node(&tree, "d1").expect("open details node");
  assert_eq!(d1.get("role").and_then(|v| v.as_str()), Some("group"));

  let d2 = find_json_node(&tree, "d2").expect("closed details node");
  assert_eq!(d2.get("role").and_then(|v| v.as_str()), Some("group"));

  let subset = snapshot_subset(&tree, &["s", "s2"]);

  assert_eq!(
    subset,
    json!({
      "s": {
        "role": "button",
        "name": "More",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "summary",
        "states": {
          "focusable": true,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false,
          "expanded": true
        }
      },
      "s2": {
        "role": "button",
        "name": "More",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "summary",
        "states": {
          "focusable": true,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false,
          "expanded": false
        }
      }
    })
  );
}

#[test]
fn accessibility_lists_and_tables() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r##"
    <html>
      <body>
        <ul id="list">
          <li id="li1">First</li>
          <li id="li2">Second</li>
        </ul>
        <table id="table">
          <tr id="row1">
            <th id="h1" scope="col">H1</th>
            <th id="h2" scope="row">Row H</th>
            <td id="c1">C1</td>
          </tr>
          <tr id="row2">
            <td id="c2">C2</td>
            <td id="c3">C3</td>
            <td id="c4">C4</td>
          </tr>
        </table>
      </body>
    </html>
  "##;
  let dom = renderer.parse_html(html).expect("parse");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");

  let list = find_by_id(&tree, "list").expect("list node");
  assert_eq!(list.role, "list");
  let mut items = Vec::new();
  collect_by_role(list, "listitem", &mut items);
  assert_eq!(items.len(), 2);
  assert_eq!(items[0].name.as_deref(), Some("First"));
  assert_eq!(items[1].name.as_deref(), Some("Second"));

  let table = find_by_id(&tree, "table").expect("table node");
  assert_eq!(table.role, "table");
  let h1 = find_by_id(&tree, "h1").expect("header one");
  let h2 = find_by_id(&tree, "h2").expect("header two");
  assert_eq!(h1.role, "columnheader");
  assert_eq!(h2.role, "rowheader");

  let mut rows = Vec::new();
  collect_by_role(table, "row", &mut rows);
  assert_eq!(rows.len(), 2);
  let mut cells = Vec::new();
  collect_by_role(table, "cell", &mut cells);
  assert!(cells.iter().any(|c| c.id.as_deref() == Some("c1")));
}

#[test]
fn accessibility_relations_and_visibility() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r##"
    <html>
      <body>
        <div id="labeled" aria-labelledby="lbl" aria-describedby="desc"></div>
        <span id="lbl">Label text</span>
        <span id="desc">Description text</span>
        <div id="hidden1" style="display:none">hide</div>
        <div id="hidden2" style="visibility:hidden">hide</div>
        <div id="hidden3" aria-hidden="true">hide</div>
        <input id="text" type="text" aria-label="Username" aria-required="true" aria-invalid="true" value="alice" />
        <select id="select" aria-label="Choices">
          <option id="opt1">One</option>
          <option id="opt2" selected>Two</option>
        </select>
        <button id="toggle" aria-pressed="mixed">Toggle</button>
      </body>
    </html>
  "##;
  let dom = renderer.parse_html(html).expect("parse");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");

  let labelled = find_by_id(&tree, "labeled").expect("labeled node");
  assert_eq!(labelled.name.as_deref(), Some("Label text"));
  assert_eq!(labelled.description.as_deref(), Some("Description text"));

  assert!(find_by_id(&tree, "hidden1").is_none());
  assert!(find_by_id(&tree, "hidden2").is_none());
  assert!(find_by_id(&tree, "hidden3").is_none());

  let text = find_by_id(&tree, "text").expect("text input");
  assert_eq!(text.role, "textbox");
  assert!(text.states.required);
  assert!(text.states.invalid);
  assert_eq!(text.value.as_deref(), Some("alice"));

  let select = find_by_id(&tree, "select").expect("select node");
  assert_eq!(select.role, "combobox");
  assert_eq!(select.value.as_deref(), Some("Two"));

  let opt2 = find_by_id(&tree, "opt2").expect("selected option");
  assert_eq!(opt2.states.selected, Some(true));

  let toggle = find_by_id(&tree, "toggle").expect("toggle button");
  assert_eq!(toggle.states.pressed, Some(PressedState::Mixed));
}

#[test]
fn accessibility_range_slider_exposes_default_value() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r#"<input id="r" type="range" min="0" max="10" />"#;
  let dom = renderer.parse_html(html).expect("parse");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");

  let slider = find_by_id(&tree, "r").expect("slider node");
  assert_eq!(slider.role, "slider");
  assert_eq!(slider.value.as_deref(), Some("5"));
}

#[test]
fn accessibility_invalid_aria_boolean_values() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r##"
    <html>
      <body>
        <div id="visible" aria-hidden="nope">Visible content</div>
        <button id="maybe-disabled" aria-disabled="nope">Click</button>
        <input id="typo-invalid" aria-invalid="grammar" />
      </body>
    </html>
  "##;

  let dom = renderer.parse_html(html).expect("parse");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");

  let visible = find_by_id(&tree, "visible").expect("visible node");
  assert_eq!(visible.name.as_deref(), Some("Visible content"));

  let maybe_disabled = find_by_id(&tree, "maybe-disabled").expect("button node");
  assert!(!maybe_disabled.states.disabled);
  assert!(maybe_disabled.states.focusable);

  let typo_invalid = find_by_id(&tree, "typo-invalid").expect("input node");
  assert!(typo_invalid.states.invalid);
}

#[test]
fn accessibility_aria_states() {
  let html =
    fs::read_to_string("tests/fixtures/accessibility/aria_states.html").expect("read fixture");
  let tree = render_accessibility_json(&html);

  let busy = find_json_node(&tree, "busy-region").expect("busy region");
  let busy_states = busy
    .get("states")
    .and_then(|s| s.as_object())
    .expect("busy states");
  assert_eq!(busy_states.get("busy"), Some(&json!(true)));

  let idle = find_json_node(&tree, "idle-region").expect("idle region");
  let idle_states = idle
    .get("states")
    .and_then(|s| s.as_object())
    .expect("idle states");
  assert!(!idle_states.contains_key("busy"));

  let current_expectations = [
    ("current-page", Some("page")),
    ("current-step", Some("step")),
    ("current-location", Some("location")),
    ("current-date", Some("date")),
    ("current-time", Some("time")),
    ("current-true", Some("true")),
    ("current-false", None),
    ("current-invalid", None),
    ("current-empty", None),
  ];

  for (id, expected) in current_expectations {
    let node = find_json_node(&tree, id).unwrap_or_else(|| panic!("missing node {id}"));
    let states = node
      .get("states")
      .and_then(|s| s.as_object())
      .unwrap_or_else(|| panic!("missing states for {id}"));

    match expected {
      Some(token) => assert_eq!(states.get("current"), Some(&json!(token)), "{id} current"),
      None => assert!(
        states.get("current").is_none(),
        "{id} should not have current"
      ),
    }
  }
}

#[test]
fn accessibility_details_expanded_state() {
  let html = r##"
    <html>
      <body>
        <details id="d1" open>
          <summary id="s">More</summary>
          <div>Content</div>
        </details>
        <details id="d2">
          <summary id="s2">More</summary>
        </details>
      </body>
    </html>
  "##;

  let tree = render_accessibility_json(html);

  let d1 = find_json_node(&tree, "d1").expect("open details node");
  assert_eq!(d1.get("role").and_then(|v| v.as_str()), Some("group"));

  let d2 = find_json_node(&tree, "d2").expect("closed details node");
  assert_eq!(d2.get("role").and_then(|v| v.as_str()), Some("group"));

  let subset = snapshot_subset(&tree, &["s", "s2"]);

  assert_eq!(
    subset,
    json!({
      "s": {
        "role": "button",
        "name": "More",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "summary",
        "states": {
          "focusable": true,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "expanded": true
        }
      },
      "s2": {
        "role": "button",
        "name": "More",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "summary",
        "states": {
          "focusable": true,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "expanded": false
        }
      }
    })
  );
}

#[test]
fn accessibility_tabindex_parsing() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r##"
    <html>
      <body>
        <div id="t1" tabindex="-1"></div>
        <div id="t2" tabindex="bogus"></div>
      </body>
    </html>
  "##;
  let dom = renderer.parse_html(html).expect("parse");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");

  let t1 = find_by_id(&tree, "t1").expect("tabindex -1 node");
  assert!(t1.states.focusable);

  assert!(
    find_by_id(&tree, "t2").is_none(),
    "invalid tabindex should not make node focusable"
  );
}

#[test]
fn accessibility_label_snapshot_json() {
  let html = r##"
    <html>
      <body>
        <label for="text">Text Label</label>
        <input id="text" type="text" />
        <label>Wrapped <input id="check" type="checkbox" /></label>
        <input id="desc" aria-describedby="help" aria-description="extra detail" />
        <div id="help">Helpful</div>
        <label for="hidden-target" style="display:none">Hidden</label>
        <input id="hidden-target" type="text" />
      </body>
    </html>
  "##;

  let tree = render_accessibility_json(html);
  let subset = snapshot_subset(&tree, &["text", "check", "desc", "hidden-target"]);

  assert_eq!(
    subset,
    json!({
      "text": {
        "role": "textbox",
        "name": "Text Label",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "input",
        "states": {
          "focusable": true,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false,
          "multiline": false
        }
      },
      "check": {
        "role": "checkbox",
        "name": "Wrapped",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "input",
        "states": {
          "focusable": true,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false,
          "checked": "false"
        }
      },
      "desc": {
        "role": "textbox",
        "name": null,
        "description": "Helpful extra detail",
        "value": null,
        "level": null,
        "html_tag": "input",
        "states": {
          "focusable": true,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false,
          "multiline": false
        }
      },
      "hidden-target": {
        "role": "textbox",
        "name": null,
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "input",
        "states": {
          "focusable": true,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false,
          "multiline": false
        }
      }
    })
  );
}

#[test]
fn accessibility_label_dom_descendant_with_shadow_dom() {
  let html = r##"
    <html>
      <body>
        <label>Label text
          <div id="host">
            <template shadowroot="open"><slot></slot></template>
            <input id="slotted" type="text" />
          </div>
        </label>
      </body>
    </html>
  "##;

  let tree = render_accessibility_json(html);
  let slotted = find_json_node(&tree, "slotted").expect("slotted input");

  assert_eq!(
    slotted.get("name").and_then(|v| v.as_str()),
    Some("Label text")
  );
}

#[test]
fn accessibility_table_snapshot_json() {
  let html = r##"
    <html>
      <body>
        <table id="table">
          <caption id="caption">Summary</caption>
          <thead>
            <tr id="head-row">
              <th id="h1">Head 1</th>
              <th id="h2" scope="row">Row Head</th>
            </tr>
          </thead>
          <tbody>
            <tr id="r1">
              <td id="c1">Cell 1</td>
              <td id="c2">Cell 2</td>
            </tr>
          </tbody>
        </table>
      </body>
    </html>
  "##;

  let tree = render_accessibility_json(html);
  let subset = snapshot_subset(
    &tree,
    &["table", "caption", "head-row", "h1", "h2", "r1", "c1"],
  );

  assert_eq!(
    subset,
    json!({
      "table": {
        "role": "table",
        "name": "Summary",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "table",
        "states": {
          "focusable": false,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      },
      "caption": {
        "role": "caption",
        "name": "Summary",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "caption",
        "states": {
          "focusable": false,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      },
      "head-row": {
        "role": "row",
        "name": "Head 1 Row Head",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "tr",
        "states": {
          "focusable": false,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      },
      "h1": {
        "role": "columnheader",
        "name": "Head 1",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "th",
        "states": {
          "focusable": false,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      },
      "h2": {
        "role": "rowheader",
        "name": "Row Head",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "th",
        "states": {
          "focusable": false,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      },
      "r1": {
        "role": "row",
        "name": "Cell 1 Cell 2",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "tr",
        "states": {
          "focusable": false,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      },
      "c1": {
        "role": "cell",
        "name": "Cell 1",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "td",
        "states": {
          "focusable": false,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      }
    })
  );
}

#[test]
fn accessibility_figure_figcaption_name() {
  let html = r##"
    <html>
      <body>
        <figure id="figure1">
          <figcaption id="figcaption1">Primary caption</figcaption>
          <img src="example.png" alt="Alt text" />
        </figure>
        <figure id="figure2" aria-label="ARIA label">
          <figcaption id="figcaption2">Should not be used</figcaption>
        </figure>
        <figure id="figure3">
          <div>
            <figcaption id="nested-caption">Nested caption</figcaption>
          </div>
          <figcaption id="hidden-caption" style="display:none">Hidden caption</figcaption>
          <figcaption id="figcaption3">Visible caption</figcaption>
        </figure>
      </body>
    </html>
  "##;

  let tree = render_accessibility_json(html);

  let figure1 = find_json_node(&tree, "figure1").expect("figure1 node");
  assert_eq!(
    figure1.get("name").and_then(|v| v.as_str()),
    Some("Primary caption")
  );
  assert_eq!(figure1.get("role").and_then(|v| v.as_str()), Some("figure"));
  assert_eq!(
    figure1.get("html_tag").and_then(|v| v.as_str()),
    Some("figure")
  );

  let figure2 = find_json_node(&tree, "figure2").expect("figure2 node");
  assert_eq!(
    figure2.get("name").and_then(|v| v.as_str()),
    Some("ARIA label")
  );

  let figure3 = find_json_node(&tree, "figure3").expect("figure3 node");
  assert_eq!(
    figure3.get("name").and_then(|v| v.as_str()),
    Some("Visible caption")
  );
  assert_eq!(figure3.get("role").and_then(|v| v.as_str()), Some("figure"));
  assert_eq!(
    figure3.get("html_tag").and_then(|v| v.as_str()),
    Some("figure")
  );
}

#[test]
fn accessibility_form_controls_snapshot_json() {
  let html = r##"
    <html>
      <body>
        <form>
          <label for="search">Search</label>
          <input id="search" type="search" value="Query" />
          <label for="slider">Volume</label>
          <input id="slider" type="range" value="5" min="0" max="10" aria-valuetext="Quiet" />
          <progress id="progress" value="0.4" max="1"></progress>
          <meter id="meter" min="0" max="10" value="7"></meter>
        </form>
      </body>
    </html>
  "##;

  let tree = render_accessibility_json(html);
  let subset = snapshot_subset(&tree, &["search", "slider", "progress", "meter"]);

  assert_eq!(
    subset,
    json!({
      "search": {
        "role": "searchbox",
        "name": "Search",
        "description": null,
        "value": "Query",
        "level": null,
        "html_tag": "input",
        "states": {
          "focusable": true,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      },
      "slider": {
        "role": "slider",
        "name": "Volume",
        "description": null,
        "value": "Quiet",
        "level": null,
        "html_tag": "input",
        "states": {
          "focusable": true,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      },
      "progress": {
        "role": "progressbar",
        "name": null,
        "description": null,
        "value": "0.4",
        "level": null,
        "html_tag": "progress",
        "states": {
          "focusable": false,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      },
      "meter": {
        "role": "meter",
        "name": null,
        "description": null,
        "value": "7",
        "level": null,
        "html_tag": "meter",
        "states": {
          "focusable": false,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      }
    })
  );
}

#[test]
fn accessibility_fieldset_legend_name() {
  let html = r##"
    <html>
      <body>
        <fieldset id="contact">
          <legend>Contact Details</legend>
          <input />
        </fieldset>
        <fieldset id="hidden-legend">
          <legend style="display:none">Hidden Legend</legend>
          <legend>Second Legend</legend>
        </fieldset>
      </body>
    </html>
  "##;

  let tree = render_accessibility_json(html);
  let subset = snapshot_subset(&tree, &["contact", "hidden-legend"]);

  assert_eq!(
    subset,
    json!({
      "contact": {
        "role": "group",
        "name": "Contact Details",
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "fieldset",
        "states": {
          "focusable": false,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      },
      "hidden-legend": {
        "role": "group",
        "name": null,
        "description": null,
        "value": null,
        "level": null,
        "html_tag": "fieldset",
        "states": {
          "focusable": false,
          "disabled": false,
          "required": false,
          "invalid": false,
          "visited": false,
          "readonly": false
        }
      }
    })
  );
}

#[test]
fn accessibility_multi_select_value() {
  let html = r##"
    <html>
      <body>
        <select id="m" multiple aria-label="M">
          <option selected>One</option>
          <option selected>Two</option>
        </select>
      </body>
    </html>
  "##;

  let tree = render_accessibility_json(html);
  let select = find_json_node(&tree, "m").expect("select node");

  assert_eq!(select.get("role").and_then(|v| v.as_str()), Some("listbox"));
  assert_eq!(
    select.get("value").and_then(|v| v.as_str()),
    Some("One, Two")
  );
}

#[test]
fn accessibility_textarea_value_strips_leading_newline() {
  let html = "<html><body><textarea id=\"area\">\nHello</textarea></body></html>";

  let tree = render_accessibility_json(html);
  let value = find_json_node(&tree, "area")
    .and_then(|node| node.get("value"))
    .and_then(|v| v.as_str());

  assert_eq!(value, Some("Hello"));
}

#[test]
fn accessibility_fixture_snapshots() {
  let fixtures = [
    "headings_links",
    "labels",
    "multiline_textbox",
    "form_controls",
    "inert_and_hidden",
    "details_summary_states",
    "modal_top_layer",
    "modal_and_readonly",
    "header_footer_landmarks",
    "form_role_gating",
    "aria_states",
    "shadow_dom_slotting",
    "native_names",
    "img_alt_presentational",
    "summary_context",
    "summary_context",
    "html_aam_roles",
    "placeholder_labeling",
    "aria_roledescription",
  ];

  for name in fixtures {
    let html = fs::read_to_string(format!("tests/fixtures/accessibility/{name}.html"))
      .expect("read html fixture");
    let expected: Value = serde_json::from_str(
      &fs::read_to_string(format!("tests/fixtures/accessibility/{name}.json"))
        .expect("read json fixture"),
    )
    .expect("parse expected json");

    let tree =
      render_accessibility_json_with_options(&html, RenderOptions::new().with_viewport(800, 600));

    let present = expected
      .get("present")
      .and_then(|v| v.as_object())
      .cloned()
      .unwrap_or_default();
    let absent: Vec<String> = expected
      .get("absent")
      .and_then(|v| v.as_array())
      .map(|arr| {
        arr
          .iter()
          .filter_map(|v| v.as_str().map(str::to_string))
          .collect()
      })
      .unwrap_or_default();

    let present_ids: Vec<String> = present.keys().cloned().collect();
    let id_refs: Vec<&str> = present_ids.iter().map(|s| s.as_str()).collect();
    let subset = snapshot_subset(&tree, &id_refs);

    assert_eq!(subset, Value::Object(present), "fixture {name}");
    for missing in absent {
      assert!(
        find_json_node(&tree, &missing).is_none(),
        "expected node {missing} to be absent in fixture {name}"
      );
    }
  }
}

#[test]
fn accessibility_shadow_dom_slotting() {
  let html = fs::read_to_string("tests/fixtures/accessibility/shadow_dom_slotting.html")
    .expect("read shadow dom fixture");

  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(&html).expect("parse html");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");

  assert_eq!(count_by_id(&tree, "light"), 1, "light should not duplicate");
  assert_eq!(
    count_by_id(&tree, "ignored"),
    0,
    "unslotted light children should be skipped"
  );
  assert_eq!(
    count_by_id(&tree, "fallback"),
    0,
    "fallback slot content should not render when assignments exist"
  );

  let path = find_path(&tree, "light").expect("path to light");
  assert!(
    path.iter().any(|n| n.id.as_deref() == Some("shadow")),
    "light should appear within the shadow root subtree"
  );
  assert!(
    path.iter().any(|n| n.id.as_deref() == Some("slot-s")),
    "light should be projected through its assigned slot"
  );
}
