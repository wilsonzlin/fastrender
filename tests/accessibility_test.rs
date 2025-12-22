use fastrender::accessibility::{AccessibilityNode, CheckState, PressedState};
use fastrender::api::FastRender;

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
