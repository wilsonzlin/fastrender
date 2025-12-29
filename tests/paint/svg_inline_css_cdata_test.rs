use fastrender::css::parser::extract_css;
use fastrender::dom;
use fastrender::image_loader::ImageCache;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::tree::box_generation::generate_box_tree;
use fastrender::tree::box_tree::{BoxNode, BoxType, ReplacedType, SvgContent};
use roxmltree::Document;

fn serialized_inline_svg(html: &str, width: f32, height: f32) -> Option<SvgContent> {
  let dom = dom::parse_html(html).ok()?;
  let stylesheet = extract_css(&dom).ok()?;
  let media = MediaContext::screen(width, height);
  let styled = apply_styles_with_media(&dom, &stylesheet, &media);
  let box_tree = generate_box_tree(&styled).ok()?;

  fn find_svg(node: &BoxNode) -> Option<SvgContent> {
    if let BoxType::Replaced(repl) = &node.box_type {
      if let ReplacedType::Svg { content } = &repl.replaced_type {
        return Some(content.clone());
      }
    }
    for child in node.children.iter() {
      if let Some(content) = find_svg(child) {
        return Some(content);
      }
    }
    None
  }

  find_svg(&box_tree.root)
}

#[test]
fn inline_svg_wraps_document_css_in_cdata() {
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(|| {
      let html = r#"
      <style>
        svg .shape {
          background-image: url("data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg'><rect width='1' height='1'/></svg>?a&b]]>");
        }
      </style>
      <svg width="10" height="10" viewBox="0 0 10 10">
        <rect class="shape" width="10" height="10" />
      </svg>
      "#;

      let serialized = serialized_inline_svg(html, 20.0, 20.0).expect("serialize svg");
      assert!(
        serialized.svg.contains("<![CDATA["),
        "embedded document CSS should be wrapped in CDATA"
      );
      assert!(
        serialized.svg.contains("]]]]><![CDATA[>"),
        "]]> terminators inside CSS should be split across CDATA sections"
      );
      assert!(
        serialized.fallback_svg.contains("<![CDATA["),
        "fallback SVG should also wrap document CSS in CDATA"
      );
      assert!(
        serialized.fallback_svg.contains("]]]]><![CDATA[>"),
        "fallback SVG should split embedded CDATA terminators"
      );

      Document::parse(&serialized.svg).expect("serialized svg should be parseable XML");
      Document::parse(&serialized.fallback_svg)
        .expect("fallback svg should also be parseable XML");

      let doc = Document::parse(&serialized.svg).expect("parse serialized svg");
      let style_text = doc
        .descendants()
        .find(|n| n.is_element() && n.tag_name().name() == "style")
        .map(|n| {
          n.descendants()
            .filter(|t| t.is_text())
            .filter_map(|t| t.text())
            .collect::<String>()
        })
        .expect("style element text");
      assert!(
        style_text.contains("background-image"),
        "style text should be preserved after CDATA wrapping"
      );
      assert!(
        style_text.contains("]]>"),
        "original CSS content containing CDATA terminators should round-trip"
      );

      let cache = ImageCache::new();
      cache
        .render_svg(&serialized.svg)
        .expect("render serialized svg with CDATA-wrapped CSS");
    })
    .unwrap()
    .join()
    .unwrap();
}
