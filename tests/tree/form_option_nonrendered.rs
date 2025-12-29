use fastrender::css::types::StyleSheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles;
use fastrender::tree::box_generation::generate_box_tree;
use fastrender::tree::box_tree::{BoxNode, BoxType};

fn contains_tag(node: &BoxNode, tag: &str) -> bool {
  if let Some(info) = &node.debug_info {
    if info.tag_name.as_deref() == Some(tag) {
      return true;
    }
  }

  node.children.iter().any(|child| contains_tag(child, tag))
}

fn collect_replaced_tag_names(node: &BoxNode, out: &mut Vec<String>) {
  if let BoxType::Replaced(_) = &node.box_type {
    if let Some(tag) = node.debug_info.as_ref().and_then(|info| info.tag_name.clone()) {
      out.push(tag);
    }
  }

  for child in &node.children {
    collect_replaced_tag_names(child, out);
  }
}

#[test]
fn option_like_elements_outside_select_do_not_generate_boxes() {
  let html = "<html><body><option id=\"orphan\">Loose</option><optgroup label=\"g\"><option>One</option></optgroup></body></html>";
  let dom = dom::parse_html(html).expect("parse html");
  let styled = apply_styles(&dom, &StyleSheet::new());
  let box_tree = generate_box_tree(&styled).expect("box tree");

  assert!(contains_tag(&box_tree.root, "html"));
  assert!(!contains_tag(&box_tree.root, "option"));
  assert!(!contains_tag(&box_tree.root, "optgroup"));
}

#[test]
fn select_generates_single_replaced_box_without_option_children() {
  let html = "<html><body><select id=\"flavors\"><option>Vanilla</option><optgroup label=\"sweet\"><option selected>Chocolate</option></optgroup></select></body></html>";
  let dom = dom::parse_html(html).expect("parse html");
  let styled = apply_styles(&dom, &StyleSheet::new());
  let box_tree = generate_box_tree(&styled).expect("box tree");

  assert!(contains_tag(&box_tree.root, "select"));
  assert!(!contains_tag(&box_tree.root, "option"));
  assert!(!contains_tag(&box_tree.root, "optgroup"));

  let mut replaced_tags = Vec::new();
  collect_replaced_tag_names(&box_tree.root, &mut replaced_tags);
  assert_eq!(replaced_tags, vec!["select".to_string()]);
}
