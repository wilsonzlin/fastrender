use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaContext;
use fastrender::style::types::TextCombineUpright;
use fastrender::style::types::TextOrientation;
use fastrender::style::types::WritingMode;

fn first_node(html: &str, css: &str) -> StyledNode {
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet(css).expect("parse css");
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  fn find_div(node: &StyledNode) -> Option<StyledNode> {
    if let fastrender::dom::DomNodeType::Element { tag_name, .. } = &node.node.node_type {
      if tag_name.eq_ignore_ascii_case("div") {
        return Some(node.clone());
      }
    }
    for child in &node.children {
      if let Some(found) = find_div(child) {
        return Some(found);
      }
    }
    None
  }

  find_div(&styled).expect("div node present")
}

#[test]
fn text_orientation_parses_upright_in_vertical_writing_mode() {
  let node = first_node(
    r#"<div></div>"#,
    r#"div { writing-mode: vertical-rl; text-orientation: upright; }"#,
  );
  assert_eq!(node.styles.text_orientation, TextOrientation::Upright);
}

#[test]
fn text_orientation_sideways_right_aliases_sideways() {
  let node = first_node(
    r#"<div></div>"#,
    r#"div { writing-mode: vertical-rl; text-orientation: sideways-right; }"#,
  );

  assert_eq!(node.styles.text_orientation, TextOrientation::Sideways);
}

#[test]
fn text_orientation_and_writing_mode_are_case_insensitive() {
  let node = first_node(
    r#"<div></div>"#,
    r#"div { writing-mode: VERTICAL-RL; text-orientation: Upright; }"#,
  );
  assert_eq!(node.styles.writing_mode, WritingMode::VerticalRl);
  assert_eq!(node.styles.text_orientation, TextOrientation::Upright);

  let node = first_node(
    r#"<div></div>"#,
    r#"div { writing-mode: Sideways-LR; text-orientation: SIDEWAYS-LEFT; }"#,
  );
  assert_eq!(node.styles.writing_mode, WritingMode::SidewaysLr);
  assert_eq!(node.styles.text_orientation, TextOrientation::SidewaysLeft);
}

#[test]
fn text_combine_upright_accepts_digits_range() {
  let node = first_node(
    r#"<div></div>"#,
    r#"div { text-combine-upright: digits 3; }"#,
  );
  assert_eq!(
    node.styles.text_combine_upright,
    TextCombineUpright::Digits(3)
  );

  let node = first_node(
    r#"<div></div>"#,
    r#"div { text-combine-upright: digits 2; }"#,
  );
  assert_eq!(
    node.styles.text_combine_upright,
    TextCombineUpright::Digits(2)
  );

  let node = first_node(
    r#"<div></div>"#,
    r#"div { text-combine-upright: digits 4; }"#,
  );
  assert_eq!(
    node.styles.text_combine_upright,
    TextCombineUpright::Digits(4)
  );

  // Allow compact keyword form when the parser surfaces a single token.
  let node = first_node(
    r#"<div></div>"#,
    r#"div { text-combine-upright: digits3; }"#,
  );
  assert_eq!(
    node.styles.text_combine_upright,
    TextCombineUpright::Digits(3)
  );

  let node = first_node(
    r#"<div></div>"#,
    r#"div { text-combine-upright: digits2; }"#,
  );
  assert_eq!(
    node.styles.text_combine_upright,
    TextCombineUpright::Digits(2)
  );

  let node = first_node(
    r#"<div></div>"#,
    r#"div { text-combine-upright: digits4; }"#,
  );
  assert_eq!(
    node.styles.text_combine_upright,
    TextCombineUpright::Digits(4)
  );
}

#[test]
fn text_combine_upright_digits_defaults_to_two() {
  let node = first_node(r#"<div></div>"#, r#"div { text-combine-upright: digits; }"#);
  assert_eq!(
    node.styles.text_combine_upright,
    TextCombineUpright::Digits(2)
  );
}

#[test]
fn text_combine_upright_keywords_are_case_insensitive() {
  let node = first_node(
    r#"<div></div>"#,
    r#"div { text-combine-upright: DIGITS 3; }"#,
  );
  assert_eq!(
    node.styles.text_combine_upright,
    TextCombineUpright::Digits(3)
  );

  let node = first_node(
    r#"<div></div>"#,
    r#"div { text-combine-upright: DiGiTs4; }"#,
  );
  assert_eq!(
    node.styles.text_combine_upright,
    TextCombineUpright::Digits(4)
  );

  let node = first_node(r#"<div></div>"#, r#"div { text-combine-upright: AlL; }"#);
  assert_eq!(node.styles.text_combine_upright, TextCombineUpright::All);
}

#[test]
fn text_combine_upright_rejects_invalid_digits() {
  let node = first_node(
    r#"<div></div>"#,
    r#"div { text-combine-upright: digits 5; }"#,
  );
  assert_eq!(node.styles.text_combine_upright, TextCombineUpright::None);

  let node = first_node(
    r#"<div></div>"#,
    r#"div { text-combine-upright: digits 1; }"#,
  );
  assert_eq!(node.styles.text_combine_upright, TextCombineUpright::None);

  let node = first_node(
    r#"<div></div>"#,
    r#"div { text-combine-upright: digits5; }"#,
  );
  assert_eq!(node.styles.text_combine_upright, TextCombineUpright::None);

  let node = first_node(
    r#"<div></div>"#,
    r#"div { text-combine-upright: digits1; }"#,
  );
  assert_eq!(node.styles.text_combine_upright, TextCombineUpright::None);
}
