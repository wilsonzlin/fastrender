use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::cascade::apply_styles;
use fastrender::tree::box_generation::generate_box_tree;
use fastrender::tree::box_tree::BoxNode;
use fastrender::FormattingContext;
use fastrender::FragmentContent;
use fastrender::FragmentNode;
use fastrender::Rgba;

fn find_first<'a>(node: &'a BoxNode, tag: &str) -> Option<&'a BoxNode> {
  if let Some(name) = node.debug_info.as_ref().and_then(|d| d.tag_name.as_ref()) {
    if name.eq_ignore_ascii_case(tag) {
      return Some(node);
    }
  }
  for child in &node.children {
    if let Some(found) = find_first(child, tag) {
      return Some(found);
    }
  }
  None
}

fn collect_texts<'a>(fragment: &'a FragmentNode, out: &mut Vec<(&'a str, Rgba)>) {
  if let FragmentContent::Text {
    text, is_marker, ..
  } = &fragment.content
  {
    if !is_marker {
      let color = fragment
        .style
        .as_ref()
        .map(|s| s.color)
        .unwrap_or_else(|| Rgba::BLACK);
      out.push((text.as_str(), color));
    }
  }
  for child in &fragment.children {
    collect_texts(child, out);
  }
}

#[test]
fn first_line_and_first_letter_styles_flow_through_pipeline() {
  let html = "<p>hello world</p>";
  let css = "p::first-letter { color: rgb(200, 0, 0); } p::first-line { color: rgb(0, 0, 255); }";

  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet(css).expect("parse stylesheet");
  let styled = apply_styles(&dom, &stylesheet);
  let box_tree = generate_box_tree(&styled);

  let paragraph = find_first(&box_tree.root, "p").expect("paragraph box");

  let ifc = InlineFormattingContext::new();
  let fragment = ifc
    .layout(paragraph, &LayoutConstraints::definite_width(200.0))
    .expect("inline layout");

  let mut texts = Vec::new();
  collect_texts(&fragment, &mut texts);

  assert!(
    texts
      .iter()
      .any(|(text, color)| text.starts_with('h') && *color == Rgba::rgb(200, 0, 0)),
    "first-letter fragment should carry the first-letter color"
  );
  assert!(
    texts
      .iter()
      .any(|(text, color)| text.contains("ello") && *color == Rgba::rgb(0, 0, 255)),
    "remaining first-line text should use the first-line color"
  );
}
