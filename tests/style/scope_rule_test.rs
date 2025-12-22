use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaContext;

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .is_some_and(|value| value.eq_ignore_ascii_case(id))
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

fn display(node: &StyledNode) -> String {
  node.styles.display.to_string()
}

#[test]
fn scope_limits_to_root_descendants() {
  let html = r#"
    <div class="scope">
      <p id="in" class="target"></p>
    </div>
    <p id="out" class="target"></p>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    @scope (.scope) {
      .target { display: inline; }
    }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "in").expect("in scope")),
    "inline"
  );
  assert_eq!(
    display(find_by_id(&styled, "out").expect("out of scope")),
    "block"
  );
}

#[test]
fn scope_respects_limit_boundary() {
  let html = r#"
    <div class="scope">
      <div class="limit"><p id="blocked" class="target"></p></div>
      <p id="allowed" class="target"></p>
    </div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    @scope (.scope) to (.limit) {
      .target { display: inline; }
    }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "allowed").expect("allowed")),
    "inline"
  );
  assert_eq!(
    display(find_by_id(&styled, "blocked").expect("blocked")),
    "block"
  );
}

#[test]
fn nested_scopes_require_inner_root() {
  let html = r#"
    <div class="outer">
      <div class="inner">
        <p id="hit" class="target"></p>
      </div>
      <p id="miss" class="target"></p>
    </div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    @scope (.outer) {
      @scope (.inner) {
        .target { display: inline; }
      }
    }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(display(find_by_id(&styled, "hit").expect("hit")), "inline");
  assert_eq!(display(find_by_id(&styled, "miss").expect("miss")), "block");
}

#[test]
fn scope_supports_scope_pseudo_and_relative_prelude() {
  let html = r#"
    <div class="root" id="root">
      <p id="inside" class="target"></p>
    </div>
    <p id="outside" class="target"></p>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    @scope (:scope .root) {
      :scope { display: inline-block; }
      .target { display: inline; }
    }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "root").expect("root")),
    "inline-block"
  );
  assert_eq!(
    display(find_by_id(&styled, "inside").expect("inside")),
    "inline"
  );
  assert_eq!(
    display(find_by_id(&styled, "outside").expect("outside")),
    "block"
  );
}
