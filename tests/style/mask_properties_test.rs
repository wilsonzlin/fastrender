use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaContext;
use fastrender::style::types::{MaskClip, MaskComposite, MaskOrigin};

fn find_first<'a>(node: &'a StyledNode, tag: &str) -> Option<&'a StyledNode> {
  if let Some(name) = node.node.tag_name() {
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

#[test]
fn mask_longhands_normalize_layer_lists() {
  let dom = dom::parse_html(
    r#"<div
      style="mask-image: linear-gradient(black, black);
             mask-clip: padding-box, content-box;
             mask-origin: content-box;
             mask-composite: intersect;"
    ></div>"#,
  )
  .unwrap();
  let stylesheet = parse_stylesheet("").unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  let style = &find_first(&styled, "div").expect("div").styles;
  assert_eq!(
    style.mask_layers.len(),
    2,
    "layer count should follow longest list"
  );

  assert_eq!(style.mask_layers[0].clip, MaskClip::PaddingBox);
  assert_eq!(style.mask_layers[1].clip, MaskClip::ContentBox);
  assert_eq!(style.mask_layers[0].origin, MaskOrigin::ContentBox);
  assert_eq!(style.mask_layers[1].origin, MaskOrigin::ContentBox);
  assert_eq!(style.mask_layers[0].composite, MaskComposite::Intersect);
  assert_eq!(style.mask_layers[1].composite, MaskComposite::Intersect);
  assert!(
    style.mask_layers.iter().all(|layer| layer.image.is_some()),
    "mask images should repeat to match the longest list"
  );
}

#[test]
fn mask_none_still_tracks_layer_lengths() {
  let dom =
    dom::parse_html(r#"<div style="mask-image: none; mask-clip: padding-box, border-box"></div>"#)
      .unwrap();
  let stylesheet = parse_stylesheet("").unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  let style = &find_first(&styled, "div").expect("div").styles;
  assert_eq!(style.mask_layers.len(), 2);
  assert!(style.mask_layers.iter().all(|layer| layer.image.is_none()));
  assert_eq!(style.mask_layers[0].clip, MaskClip::PaddingBox);
  assert_eq!(style.mask_layers[1].clip, MaskClip::BorderBox);
}
