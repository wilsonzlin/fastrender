use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::display::Display;
use fastrender::style::media::MediaContext;
use fastrender::style::types::CaseTransform;
use fastrender::style::types::TextTransform;
use fastrender::ComputedStyle;

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

fn styled_list_item(css: &str, html: &str) -> StyledNode {
  let dom = dom::parse_html(html).unwrap();
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  find_first(&styled, "li").cloned().expect("list item")
}

#[test]
fn marker_disallows_box_and_background_properties() {
  // Apply forbidden properties to the marker via authored styles
  let style_rules = "
        li::marker { background: red; padding: 10px; margin: 5px 7px; width: 50px; height: 40px; border: 2px solid blue; }
        li::marker { background-position: center; background-size: 10px 10px; }
        li::marker { background-repeat: repeat-x; background-clip: content-box; }
        li::marker { border-radius: 4px; box-shadow: 0 0 5px black; }
        li::marker { padding-left: 12px; padding-top: 8px; padding-bottom: 6px; }
        li::marker { display: block; }
    ";

  let li = styled_list_item(style_rules, "<ul><li></li></ul>");
  let marker_style = li.marker_styles.as_ref().expect("marker styles");
  let defaults = ComputedStyle::default();

  // Marker should still be inline and ignore box/background overrides
  assert_eq!(marker_style.display, Display::Inline);
  assert_eq!(marker_style.padding_top, defaults.padding_top);
  assert_eq!(marker_style.padding_left, defaults.padding_left);
  assert_eq!(marker_style.width, defaults.width);
  assert_eq!(marker_style.height, defaults.height);
  assert_eq!(marker_style.background_color, defaults.background_color);
  assert_eq!(marker_style.background_images, defaults.background_images);
  assert_eq!(
    marker_style.background_positions,
    defaults.background_positions
  );
  assert_eq!(marker_style.background_repeats, defaults.background_repeats);
  assert_eq!(marker_style.background_clips, defaults.background_clips);
  assert_eq!(marker_style.background_origins, defaults.background_origins);
  assert_eq!(marker_style.margin_top, defaults.margin_top);
  assert_eq!(marker_style.margin_left, defaults.margin_left);
  assert_eq!(marker_style.border_top_width, defaults.border_top_width);
  assert_eq!(marker_style.border_left_width, defaults.border_left_width);
  assert_eq!(marker_style.box_shadow.len(), 0);

  // The parent list item should remain a list item
  assert_eq!(li.styles.display, Display::ListItem);
}

#[test]
fn marker_allows_text_affecting_properties() {
  let rules =
        "li::marker { color: rgb(10,20,30); font-size: 24px; text-transform: uppercase; text-shadow: 1px 1px red; letter-spacing: 2px; }";

  let li = styled_list_item(rules, "<ul><li></li></ul>");
  let marker_style = li.marker_styles.as_ref().expect("marker styles");

  assert_eq!(marker_style.color, fastrender::Rgba::rgb(10, 20, 30));
  assert_eq!(marker_style.font_size, 24.0);
  assert_eq!(
    marker_style.text_transform,
    TextTransform::with_case(CaseTransform::Uppercase)
  );
}

#[test]
fn marker_ignores_outline_properties() {
  let rules =
        "li { outline: 2px solid rgb(1,2,3); } li::marker { outline: 4px dotted rgb(4,5,6); outline-offset: 8px; }";
  let li = styled_list_item(rules, "<ul><li></li></ul>");
  let marker_style = li.marker_styles.as_ref().expect("marker styles");
  let defaults = ComputedStyle::default();

  // Outline on markers should be cleared to defaults and not inherit from the list item.
  assert!(matches!(
    marker_style.outline_style,
    fastrender::style::types::OutlineStyle::None
  ));
  assert_eq!(marker_style.outline_width, defaults.outline_width);
  assert!(matches!(
    marker_style.outline_color,
    fastrender::style::types::OutlineColor::Invert
  ));
  assert_eq!(marker_style.outline_offset, defaults.outline_offset);
}

#[test]
fn marker_ignores_opacity_and_transform() {
  let rules = "li::marker { opacity: 0.2; transform: rotate(45deg); }";
  let li = styled_list_item(rules, "<ul><li></li></ul>");
  let marker_style = li.marker_styles.as_ref().expect("marker styles");

  assert_eq!(
    marker_style.opacity, 1.0,
    "marker opacity should remain at the default"
  );
  assert!(
    marker_style.transform.is_empty(),
    "marker transform should be cleared"
  );
}

#[test]
fn marker_ignores_vertical_align() {
  let rules = "li::marker { vertical-align: middle; }";
  let li = styled_list_item(rules, "<ul><li></li></ul>");
  let marker_style = li.marker_styles.as_ref().expect("marker styles");

  assert!(matches!(
    marker_style.vertical_align,
    fastrender::style::types::VerticalAlign::Baseline
  ));
}
