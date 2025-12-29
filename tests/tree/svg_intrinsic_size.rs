use fastrender::css::types::StyleSheet;
use fastrender::dom;
use fastrender::geometry::Size;
use fastrender::style::cascade;
use fastrender::tree::box_generation::generate_box_tree;
use fastrender::tree::box_tree::{BoxNode, BoxType, ReplacedBox, ReplacedType};

fn find_inline_svg(node: &BoxNode) -> Option<&ReplacedBox> {
  if let BoxType::Replaced(replaced) = &node.box_type {
    if matches!(replaced.replaced_type, ReplacedType::Svg { .. }) {
      return Some(replaced);
    }
  }
  for child in &node.children {
    if let Some(found) = find_inline_svg(child) {
      return Some(found);
    }
  }
  None
}

fn svg_replaced_box(svg_markup: &str) -> ReplacedBox {
  let html = format!("<html><body>{}</body></html>", svg_markup);
  let dom = dom::parse_html(&html).expect("parse html");
  let styled = cascade::apply_styles(&dom, &StyleSheet::new());
  let tree = generate_box_tree(&styled).expect("box tree");
  find_inline_svg(&tree.root)
    .cloned()
    .expect("inline svg replaced box")
}

#[test]
fn svg_numeric_width_height_used_as_intrinsic_size() {
  let replaced = svg_replaced_box(r#"<svg width="200" height="100"></svg>"#);
  assert_eq!(replaced.intrinsic_size, Some(Size::new(200.0, 100.0)));
}

#[test]
fn svg_parses_absolute_length_units() {
  let replaced = svg_replaced_box(r#"<svg width="2in" height="1in"></svg>"#);
  assert_eq!(replaced.intrinsic_size, Some(Size::new(192.0, 96.0)));
}

#[test]
fn svg_percentage_lengths_fall_back_to_default_intrinsic_size() {
  let replaced = svg_replaced_box(r#"<svg width="100%" height="100%"></svg>"#);
  assert_eq!(replaced.intrinsic_size, Some(Size::new(300.0, 150.0)));
  assert_eq!(replaced.aspect_ratio, None);
}

#[test]
fn svg_viewbox_sets_aspect_ratio_with_default_dimensions() {
  let replaced = svg_replaced_box(r#"<svg viewBox="0 0 40 20"></svg>"#);
  assert_eq!(replaced.intrinsic_size, Some(Size::new(300.0, 150.0)));
  assert_eq!(replaced.aspect_ratio, Some(2.0));
}
