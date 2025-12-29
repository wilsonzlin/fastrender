use fastrender::accessibility::AccessibilityNode;
use fastrender::api::FastRender;

fn render_accessibility_tree(html: &str) -> AccessibilityNode {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(html).expect("parse html");
  renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree")
}

fn find_by_id<'a>(node: &'a AccessibilityNode, id: &str) -> Option<&'a AccessibilityNode> {
  if node.id.as_deref() == Some(id) {
    return Some(node);
  }
  for child in node.children.iter() {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

#[test]
fn aria_labelledby_token_order_and_hidden() {
  let html = r#"
    <html>
      <body>
        <div id="target" aria-labelledby="target first hidden second"></div>
        <span id="first">First</span>
        <span id="hidden" style="display:none">Ignored</span>
        <span id="second" aria-label="Second"></span>
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);
  let target = find_by_id(&tree, "target").expect("target node");
  assert_eq!(target.name.as_deref(), Some("First Ignored Second"));
}

#[test]
fn nested_label_contents_are_traversed() {
  let html = r#"
    <html>
      <body>
        <label>
          Outer
          <span aria-label="Inner"></span>
          <input id="field" type="text" />
        </label>
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);
  let field = find_by_id(&tree, "field").expect("input field");
  assert_eq!(field.name.as_deref(), Some("Outer Inner"));
}

#[test]
fn aria_label_overrides_alt_on_images() {
  let html = r#"
    <html>
      <body>
        <img id="photo" alt="Fallback alt" aria-label="Preferred label" />
        <img id="photo-alt" alt="Alt only" />
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);
  let photo = find_by_id(&tree, "photo").expect("photo node");
  assert_eq!(photo.name.as_deref(), Some("Preferred label"));

  let photo_alt = find_by_id(&tree, "photo-alt").expect("alt node");
  assert_eq!(photo_alt.name.as_deref(), Some("Alt only"));
}

#[test]
fn control_value_and_placeholder_fallbacks() {
  let html = r#"
    <html>
      <body>
        <input id="value-fallback" type="text" value="Typed value" />
        <input id="placeholder" type="text" placeholder="Hint text" />
        <select id="chooser">
          <option>One</option>
          <option selected>Two</option>
        </select>
        <textarea id="area">Area text</textarea>
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);

  let value = find_by_id(&tree, "value-fallback").expect("value fallback");
  assert_eq!(value.name.as_deref(), Some("Typed value"));

  let placeholder = find_by_id(&tree, "placeholder").expect("placeholder");
  assert_eq!(placeholder.name.as_deref(), Some("Hint text"));

  let chooser = find_by_id(&tree, "chooser").expect("select");
  assert_eq!(chooser.name.as_deref(), Some("Two"));

  let area = find_by_id(&tree, "area").expect("textarea");
  assert_eq!(area.name.as_deref(), Some("Area text"));
}

#[test]
fn aria_describedby_respects_visibility() {
  let html = r#"
    <html>
      <body>
        <button id="described" aria-describedby="d1 d2" aria-description="extra detail">
          Action
        </button>
        <div id="d1">Helpful</div>
        <div id="d2" style="visibility:hidden">Hidden text</div>
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);
  let described = find_by_id(&tree, "described").expect("button");
  assert_eq!(described.name.as_deref(), Some("Action"));
  assert_eq!(
    described.description.as_deref(),
    Some("Helpful extra detail")
  );
}

#[test]
fn aria_label_empty_blocks_fallbacks() {
  let html = r#"
    <html>
      <body>
        <input id="empty-label" aria-label="" type="text" placeholder="Hint" value="Has value" />
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);
  let field = find_by_id(&tree, "empty-label").expect("input field");
  assert_eq!(field.name.as_deref(), Some(""));
}

#[test]
fn aria_labelledby_missing_ids_block_fallbacks() {
  let html = r#"
    <html>
      <body>
        <button id="fallback" aria-labelledby="missing" aria-label="Label fallback">Click</button>
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);
  let button = find_by_id(&tree, "fallback").expect("button");
  assert_eq!(button.name.as_deref(), Some(""));
}

#[test]
fn aria_labelledby_cycles_are_handled() {
  let html = r#"
    <html>
      <body>
        <div id="outer" aria-labelledby="inner">
          <span id="inner" aria-labelledby="outer">Inner text</span>
        </div>
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);
  let outer = find_by_id(&tree, "outer").expect("outer element");
  assert_eq!(outer.name.as_deref(), Some(""));
}

#[test]
fn title_falls_back_when_no_other_name() {
  let html = r#"
    <html>
      <body>
        <div id="titled" title="Help text"></div>
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);
  let titled = find_by_id(&tree, "titled").expect("titled element");
  assert_eq!(titled.name.as_deref(), Some("Help text"));
}

#[test]
fn descendant_accnames_contribute_to_content_names() {
  let html = r#"
    <html>
      <body>
        <button id="composed">
          <span aria-label="Composed name"></span>
        </button>
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);
  let composed = find_by_id(&tree, "composed").expect("composed button");
  assert_eq!(composed.name.as_deref(), Some("Composed name"));
}

#[test]
fn css_hidden_labels_still_associate() {
  let html = r#"
    <html>
      <body>
        <label for="hidden-labeled" style="display:none">Hidden label text</label>
        <input id="hidden-labeled" type="text" />

        <label for="aria-hidden-label" aria-hidden="true">Blocked</label>
        <input id="aria-hidden-label" type="text" placeholder="Fallback placeholder" />
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);
  let hidden_labeled = find_by_id(&tree, "hidden-labeled").expect("hidden labeled input");
  assert_eq!(hidden_labeled.name.as_deref(), Some("Hidden label text"));

  let aria_hidden_label =
    find_by_id(&tree, "aria-hidden-label").expect("aria-hidden labeled input");
  assert_eq!(
    aria_hidden_label.name.as_deref(),
    Some("Fallback placeholder")
  );
}

#[test]
fn content_precedes_title_tooltips() {
  let html = r#"
    <html>
      <body>
        <button id="tooltip" title="Tooltip name">Visible text</button>
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);
  let tooltip = find_by_id(&tree, "tooltip").expect("button with title");
  assert_eq!(tooltip.name.as_deref(), Some("Visible text"));
}

#[test]
fn presentational_roles_can_label_targets() {
  let html = r#"
    <html>
      <body>
        <div id="label" role="presentation">Presentational label</div>
        <button id="target" aria-labelledby="label"></button>
      </body>
    </html>
  "#;

  let tree = render_accessibility_tree(html);
  let target = find_by_id(&tree, "target").expect("button target");
  assert_eq!(target.name.as_deref(), Some("Presentational label"));
}
