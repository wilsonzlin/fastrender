use fastrender::css::parser::extract_css;
use fastrender::debug::runtime::{self, RuntimeToggles};
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::tree::box_generation::generate_box_tree;
use fastrender::tree::box_tree::{BoxNode, BoxType, ReplacedType, SvgContent};
use std::collections::HashMap;
use std::sync::Arc;

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
fn svg_document_css_embedding_policy_respects_svg_count_overrides_and_size_limit() {
  let html_many_svgs = r#"
    <style>
      svg.icon .shape { fill: red; }
    </style>
    <svg class="icon" width="10" height="10" viewBox="0 0 10 10">
      <rect class="shape" width="10" height="10" />
    </svg>
    <svg class="icon" width="10" height="10" viewBox="0 0 10 10">
      <rect class="shape" width="10" height="10" />
    </svg>
  "#;

  // When the document contains more replaced SVGs than allowed (and embedding is not forced),
  // skip embedding document CSS to avoid O(svg_count × css_bytes) blowups.
  let disabled = runtime::with_runtime_toggles(
    Arc::new(RuntimeToggles::from_map(HashMap::from([(
      "FASTR_SVG_EMBED_DOCUMENT_CSS_MAX_SVGS".to_string(),
      "1".to_string(),
    )]))),
    || serialized_inline_svg(html_many_svgs, 20.0, 20.0).expect("serialize svg"),
  );
  assert!(
    !disabled.svg.contains("<style><![CDATA["),
    "document CSS embedding should be disabled when SVG count exceeds the max"
  );
  assert!(
    disabled.fallback_svg.is_empty(),
    "fallback SVG should remain empty when document CSS embedding is disabled"
  );

  // Forcing embedding on should override the SVG count guard (while still honoring the CSS size cap).
  let forced_on = runtime::with_runtime_toggles(
    Arc::new(RuntimeToggles::from_map(HashMap::from([
      (
        "FASTR_SVG_EMBED_DOCUMENT_CSS".to_string(),
        "1".to_string(),
      ),
      (
        "FASTR_SVG_EMBED_DOCUMENT_CSS_MAX_SVGS".to_string(),
        "1".to_string(),
      ),
    ]))),
    || serialized_inline_svg(html_many_svgs, 20.0, 20.0).expect("serialize svg"),
  );
  assert!(
    forced_on.svg.contains("<style><![CDATA["),
    "document CSS should be embedded when forced on"
  );
  assert_eq!(
    forced_on.svg, forced_on.fallback_svg,
    "fallback SVG should mirror the primary SVG when embedding document CSS"
  );

  // Forcing embedding off should disable it even for a single inline SVG.
  let html_one_svg = r#"
    <style>
      svg.icon .shape { fill: red; }
    </style>
    <svg class="icon" width="10" height="10" viewBox="0 0 10 10">
      <rect class="shape" width="10" height="10" />
    </svg>
  "#;
  let forced_off = runtime::with_runtime_toggles(
    Arc::new(RuntimeToggles::from_map(HashMap::from([(
      "FASTR_SVG_EMBED_DOCUMENT_CSS".to_string(),
      "0".to_string(),
    )]))),
    || serialized_inline_svg(html_one_svg, 20.0, 20.0).expect("serialize svg"),
  );
  assert!(
    !forced_off.svg.contains("<style><![CDATA["),
    "document CSS embedding should be disabled when forced off"
  );
  assert!(
    forced_off.fallback_svg.is_empty(),
    "fallback SVG should remain empty when forced off"
  );

  // The embed override must still honor the 64KiB embedded CSS cap.
  let oversized_css = ".x{fill:red;}\n".repeat(5000);
  let html_oversized_css = format!(
    "<style>{}</style><svg class=\"icon\" width=\"10\" height=\"10\" viewBox=\"0 0 10 10\"><rect class=\"shape\" width=\"10\" height=\"10\" /></svg>",
    oversized_css
  );
  let forced_on_oversized = runtime::with_runtime_toggles(
    Arc::new(RuntimeToggles::from_map(HashMap::from([(
      "FASTR_SVG_EMBED_DOCUMENT_CSS".to_string(),
      "1".to_string(),
    )]))),
    || serialized_inline_svg(&html_oversized_css, 20.0, 20.0).expect("serialize svg"),
  );
  assert!(
    !forced_on_oversized.svg.contains("<style><![CDATA["),
    "document CSS should not be embedded when it exceeds the embedded CSS cap"
  );
  assert!(
    forced_on_oversized.fallback_svg.is_empty(),
    "fallback SVG should remain empty when embedding is disabled due to size limit"
  );
}

