use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaContext;
use fastrender::style::types::BackgroundBox;

fn display(node: &StyledNode) -> String {
  node.styles.display.to_string()
}

fn find_first<'a>(node: &'a StyledNode, tag: &str) -> Option<&'a StyledNode> {
  if let Some(name) = node.node.tag_name() {
    if name.eq_ignore_ascii_case(tag) {
      return Some(node);
    }
  }
  for child in node.children.iter() {
    if let Some(found) = find_first(child, tag) {
      return Some(found);
    }
  }
  None
}

fn render_div_display(css: &str) -> String {
  let dom = dom::parse_html(r#"<div></div>"#).unwrap();
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let div = find_first(&styled, "div").expect("div");
  display(div)
}

fn render_div_background_clip(css: &str) -> BackgroundBox {
  let dom = dom::parse_html(r#"<div></div>"#).unwrap();
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let div = find_first(&styled, "div").expect("div");
  div
    .styles
    .background_layers
    .first()
    .expect("background layer")
    .clip
}

#[test]
fn supports_declaration_matches() {
  let css = r"@supports (display: grid) { div { display: inline; } }";
  assert_eq!(render_div_display(css), "inline");
}

#[test]
fn supports_not_negates() {
  let css = r"@supports not (display: grid) { div { display: inline; } }";
  assert_eq!(render_div_display(css), "block");
}

#[test]
fn supports_nested_conditions_combine_correctly() {
  let css = r"@supports ((display: grid) and (color: red)) or (selector(:has(*))) { div { display: inline; } }";
  assert_eq!(render_div_display(css), "inline");
}

#[test]
fn supports_and_inside_url_is_treated_as_value() {
  let css = r"@supports (background: url(and.png)) { div { display: inline; } }";
  assert_eq!(render_div_display(css), "inline");
}

#[test]
fn supports_background_clip_text() {
  let css = r"@supports (background-clip: text) { div { display: inline; } }";
  assert_eq!(render_div_display(css), "inline");
}

#[test]
fn supports_vendor_prefixed_properties_match_when_unprefixed_supported() {
  let css = r"@supports (-webkit-transform: rotate(10deg)) { div { display: inline; } }";
  assert_eq!(render_div_display(css), "inline");
}

#[test]
fn supports_vendor_prefixed_unknown_properties_are_unsupported() {
  let css = r"@supports (-webkit-not-a-property: 1) { div { display: inline; } }";
  assert_eq!(render_div_display(css), "block");
}

#[test]
fn webkit_background_clip_text_is_aliased() {
  let css = r"div { -webkit-background-clip: text; }";
  assert_eq!(render_div_background_clip(css), BackgroundBox::Text);
}
