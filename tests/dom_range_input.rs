use fastrender::accessibility::AccessibilityNode;
use fastrender::api::FastRender;

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

fn slider_value(html: &str, id: &str) -> Option<String> {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(html).expect("parse html");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");
  find_by_id(&tree, id).and_then(|node| node.value.clone())
}

#[test]
fn default_range_value_uses_default_bounds_and_step() {
  let value =
    slider_value(r#"<input id="slider" type="range" />"#, "slider").expect("default slider value");
  assert_eq!(value, "50");
}

#[test]
fn range_values_snap_to_step_increments() {
  let value = slider_value(
    r#"<input id="slider" type="range" min="0" max="5" step="2" value="5" />"#,
    "slider",
  )
  .expect("slider value");
  assert_eq!(value, "4");
}

#[test]
fn range_step_alignment_uses_minimum_as_base() {
  let value = slider_value(
    r#"<input id="slider" type="range" min="5" max="9" step="2" value="6.1" />"#,
    "slider",
  )
  .expect("slider value");
  assert_eq!(value, "7");
}

#[test]
fn invalid_step_values_fall_back_to_default_step() {
  let value = slider_value(
    r#"<input id="slider" type="range" min="0" max="10" step="foo" value="4.7" />"#,
    "slider",
  )
  .expect("slider value");
  assert_eq!(value, "5");
}

#[test]
fn step_any_preserves_authored_value() {
  let value = slider_value(
    r#"<input id="slider" type="range" min="0" max="10" step="any" value="4.7" />"#,
    "slider",
  )
  .expect("slider value");
  assert_eq!(value, "4.7");
}
