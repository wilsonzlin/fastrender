use fastrender::css::parser::extract_css;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::tree::box_generation::generate_box_tree;
use fastrender::tree::box_tree::{BoxNode, BoxType, ReplacedType, SvgContent};

fn find_svg_content(node: &BoxNode) -> Option<SvgContent> {
  if let BoxType::Replaced(replaced) = &node.box_type {
    if let ReplacedType::Svg { content } = &replaced.replaced_type {
      return Some(content.clone());
    }
  }
  for child in &node.children {
    if let Some(found) = find_svg_content(child) {
      return Some(found);
    }
  }
  None
}

#[test]
fn inert_template_styles_are_not_collected_into_document_css() {
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(|| {
      let html = r#"
      <html>
        <head>
          <template><style>body { color: red; }</style></template>
          <style>body { color: green; }</style>
        </head>
        <body>
          <svg width="10" height="10" viewBox="0 0 10 10">
            <foreignObject x="0" y="0" width="10" height="10">
              <div xmlns="http://www.w3.org/1999/xhtml" style="width:10px;height:10px;"></div>
            </foreignObject>
          </svg>
        </body>
      </html>
      "#;

      let dom = dom::parse_html(html).expect("parse html");
      let stylesheet = extract_css(&dom).expect("extract css");
      let media = MediaContext::screen(10.0, 10.0);
      let styled = apply_styles_with_media(&dom, &stylesheet, &media);
      let box_tree = generate_box_tree(&styled);
      let svg = find_svg_content(&box_tree.root).expect("svg content");
      let compact_css = svg
        .shared_css
        .to_ascii_lowercase()
        .split_whitespace()
        .collect::<String>();

      assert!(
        !compact_css.is_empty(),
        "document CSS should be embedded for foreignObject serialization"
      );
      assert!(
        compact_css.contains("color:green"),
        "non-template styles should be collected for foreignObject CSS"
      );
      assert!(
        !compact_css.contains("color:red"),
        "inert template styles should not appear in collected document CSS"
      );
    })
    .unwrap()
    .join()
    .unwrap();
}
