use fastrender::api::FastRender;
use fastrender::style::cascade::StyledNode;
use fastrender::style::color::Rgba;
use fastrender::style::media::MediaType;

fn styled_tree_for(html: &str) -> StyledNode {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(html).expect("parsed dom");
  renderer
    .layout_document_for_media_intermediates(&dom, 800, 600, MediaType::Screen)
    .expect("laid out")
    .styled_tree
}

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .map(|value| value == id)
    .unwrap_or(false)
  {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

#[test]
fn container_style_query_matches_custom_property() {
  let html = r#"
    <style>
      .container { container-type: inline-size; container-name: demo; --foo: bar; }
      .child { color: rgb(0 0 255); }
      @container demo style(--foo: bar) {
        .child { color: rgb(255 0 0); }
      }
    </style>
    <div id="container" class="container">
      <div id="target" class="child">hello</div>
    </div>
  "#;

  let styled = styled_tree_for(html);
  let target = find_by_id(&styled, "target").expect("target element");
  assert_eq!(target.styles.color, Rgba::rgb(255, 0, 0));
}

#[test]
fn container_style_query_respects_container_type() {
  let html = r#"
    <style>
      .container { --foo: bar; }
      .child { color: rgb(0 0 255); }
      @container style(--foo: bar) {
        .child { color: rgb(255 0 0); }
      }
    </style>
    <div id="container" class="container">
      <div id="target" class="child">hello</div>
    </div>
  "#;

  let styled = styled_tree_for(html);
  let target = find_by_id(&styled, "target").expect("target element");
  assert_eq!(target.styles.color, Rgba::rgb(0, 0, 255));
}

#[test]
fn container_style_query_falls_back_when_variable_unset() {
  let html = r#"
    <style>
      .container { container-type: inline-size; }
      .child { color: rgb(0 0 255); }
      @container style(--missing: present) {
        .child { color: rgb(5 6 7); }
      }
    </style>
    <div class="container">
      <div id="target" class="child">hello</div>
    </div>
  "#;

  let styled = styled_tree_for(html);
  let target = find_by_id(&styled, "target").expect("target element");
  assert_eq!(target.styles.color, Rgba::rgb(0, 0, 255));
}

#[test]
fn container_style_query_interacts_with_layers_and_important() {
  let html = r#"
    <style>
      @layer defaults, utilities;
      @layer defaults {
        .child { color: rgb(0 128 0) !important; }
      }
      @layer utilities {
        @container style(--theme: dark) {
          .child { color: rgb(128 0 128); }
        }
      }
      .container { container-type: inline-size; --theme: dark; }
    </style>
    <div class="container">
      <div id="layered" class="child">hello</div>
    </div>
  "#;

  let styled = styled_tree_for(html);
  let target = find_by_id(&styled, "layered").expect("layered element");
  assert_eq!(target.styles.color, Rgba::rgb(0, 128, 0));
}

#[test]
fn nested_containers_use_nearest_style_query_match() {
  let html = r#"
    <style>
      .outer { container-type: inline-size; --theme: outer; }
      .inner { container-type: inline-size; --theme: inner; }
      #nested { color: black; }
      @container style(--theme: outer) {
        #nested { color: rgb(255 105 180); }
      }
      @container style(--theme: inner) {
        #nested { color: rgb(255 165 0); }
      }
    </style>
    <div class="outer">
      <div class="inner">
        <div id="nested">hello</div>
      </div>
    </div>
  "#;

  let styled = styled_tree_for(html);
  let target = find_by_id(&styled, "nested").expect("nested element");
  assert_eq!(target.styles.color, Rgba::rgb(255, 165, 0));
}
