use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::DomNode;
use fastrender::dom::DomNodeType;
use fastrender::dom::HTML_NAMESPACE;
use fastrender::style::cascade::apply_styles;
use fastrender::Rgba;

fn target_node() -> DomNode {
  DomNode {
    node_type: DomNodeType::Element {
      tag_name: "div".to_string(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: vec![("id".to_string(), "t".to_string())],
    },
    children: vec![],
  }
}

fn color_for(css: &str) -> Rgba {
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles(&target_node(), &stylesheet);
  styled.styles.color
}

#[test]
fn important_inverts_layer_order() {
  let css = r#"
    @layer a { #t { color: rgb(1, 2, 3) !important; } }
    @layer b { #t { color: rgb(4, 5, 6) !important; } }
  "#;

  assert_eq!(color_for(css), Rgba::rgb(1, 2, 3));
}

#[test]
fn layered_important_beats_unlayered_important() {
  let css = r#"
    @layer a { #t { color: rgb(1, 2, 3) !important; } }
    #t { color: rgb(4, 5, 6) !important; }
  "#;

  assert_eq!(color_for(css), Rgba::rgb(1, 2, 3));
}

#[test]
fn revert_layer_uses_important_stratum_base() {
  let css = r#"
    @layer a { #t { color: rgb(1, 2, 3) !important; } }
    @layer b { #t { color: rgb(9, 9, 9) !important; color: revert-layer !important; } }
  "#;

  assert_eq!(color_for(css), Rgba::rgb(1, 2, 3));
}

#[test]
fn normal_layer_cannot_override_important_in_earlier_layer() {
  let css = r#"
    @layer a { #t { color: rgb(1, 1, 1) !important; } }
    @layer b { #t { color: rgb(2, 2, 2); } }
  "#;

  assert_eq!(color_for(css), Rgba::rgb(1, 1, 1));
}
