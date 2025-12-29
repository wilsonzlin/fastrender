use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::color::Rgba;
use fastrender::style::media::MediaContext;

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .is_some_and(|value| value.eq_ignore_ascii_case(id))
  {
    return Some(node);
  }
  for child in node.children.iter() {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

fn display(node: &StyledNode) -> String {
  node.styles.display.to_string()
}

#[test]
fn nested_rules_respect_scope_context() {
  let html = r#"
    <div class="scope">
      <div class="parent">
        <div id="scoped" class="child"></div>
      </div>
    </div>
    <div class="parent">
      <div id="unscoped" class="child"></div>
    </div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    @scope (.scope) {
      .parent {
        .child { display: inline; }
      }
    }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "scoped").expect("scoped child")),
    "inline"
  );
  assert_eq!(
    display(find_by_id(&styled, "unscoped").expect("unscoped child")),
    "block"
  );
}

#[test]
fn nested_rules_preserve_layer_order_inside_layers() {
  let html = r#"
    <div class="parent">
      <div id="layered" class="child"></div>
    </div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    @layer base {
      .parent {
        .child { color: red; }
      }
    }

    @layer overrides {
      .child { color: blue; }
    }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(1024.0, 768.0));

  let layered = find_by_id(&styled, "layered").expect("layered child");
  assert_eq!(layered.styles.color, Rgba::rgb(0, 0, 255));
}
