use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaContext;
use fastrender::style::types::{
  BackgroundImage, BackgroundPosition, BackgroundRepeatKeyword, BackgroundSize,
  BackgroundSizeKeyword, MaskClip, MaskComposite, MaskOrigin,
};
use fastrender::style::values::Length;

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

#[test]
fn mask_shorthand_parses_position_size_and_repeat() {
  let dom =
    dom::parse_html(r#"<div style="mask: url(a) center/contain no-repeat;"></div>"#).unwrap();
  let stylesheet = parse_stylesheet("").unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  let layer = &find_first(&styled, "div").expect("div").styles.mask_layers[0];
  assert!(matches!(layer.image, Some(BackgroundImage::Url(ref url)) if url == "a"));
  let BackgroundPosition::Position { x, y } = &layer.position;
  assert!((x.alignment - 0.5).abs() < 1e-6);
  assert_eq!(x.offset, Length::percent(0.0));
  assert!((y.alignment - 0.5).abs() < 1e-6);
  assert_eq!(y.offset, Length::percent(0.0));
  assert_eq!(
    layer.size,
    BackgroundSize::Keyword(BackgroundSizeKeyword::Contain)
  );
  assert_eq!(layer.repeat.x, BackgroundRepeatKeyword::NoRepeat);
  assert_eq!(layer.repeat.y, BackgroundRepeatKeyword::NoRepeat);
}

#[test]
fn mask_shorthand_sets_origin_and_clip() {
  let dom =
    dom::parse_html(r#"<div style="mask: url(a) content-box padding-box;"></div>"#).unwrap();
  let stylesheet = parse_stylesheet("").unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  let layer = &find_first(&styled, "div").expect("div").styles.mask_layers[0];
  assert_eq!(layer.origin, MaskOrigin::ContentBox);
  assert_eq!(layer.clip, MaskClip::PaddingBox);
}

#[test]
fn mask_layers_repeat_longhands_to_match_images() {
  let dom =
    dom::parse_html(r#"<div style="mask: url(a), url(b); mask-repeat: repeat-x;"></div>"#).unwrap();
  let stylesheet = parse_stylesheet("").unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  let style = &find_first(&styled, "div").expect("div").styles;
  assert_eq!(style.mask_layers.len(), 2);
  assert!(matches!(
    style.mask_layers[0].image,
    Some(BackgroundImage::Url(ref url)) if url == "a"
  ));
  assert!(matches!(
    style.mask_layers[1].image,
    Some(BackgroundImage::Url(ref url)) if url == "b"
  ));
  for layer in &style.mask_layers {
    assert_eq!(layer.repeat.x, BackgroundRepeatKeyword::Repeat);
    assert_eq!(layer.repeat.y, BackgroundRepeatKeyword::NoRepeat);
  }
}
