use fastrender::css::parser::parse_stylesheet;
use fastrender::css::selectors::ShadowMatchData;
use fastrender::css::types::CssRule;
use fastrender::dom::{
  next_selector_cache_epoch, DomNode, DomNodeType, ElementRef, HTML_NAMESPACE,
};
use fastrender::style::cascade::apply_styles;
use fastrender::style::defaults::get_default_styles_for_element;
use selectors::matching::{
  matches_selector, MatchingContext, MatchingForInvalidation, MatchingMode, NeedsSelectorFlags,
  QuirksMode, SelectorCaches,
};
use selectors::Element;

fn host_element() -> DomNode {
  DomNode {
    node_type: DomNodeType::Element {
      tag_name: "div".into(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: vec![],
    },
    children: vec![],
  }
}

#[test]
fn document_host_selector_is_ignored() {
  let dom = host_element();
  let stylesheet = parse_stylesheet(":host { padding-left: 5px; }").unwrap();

  let styled = apply_styles(&dom, &stylesheet);
  let defaults = get_default_styles_for_element(&dom);

  assert_eq!(
    styled.styles.padding_left, defaults.padding_left,
    ":host from document stylesheets should not match"
  );
}

#[test]
fn shadow_stylesheet_host_matches_only_host() {
  let host = host_element();
  let element_ref = ElementRef::new(&host);
  let stylesheet = parse_stylesheet(":host { padding-left: 5px; }").unwrap();
  let selector = stylesheet
    .rules
    .iter()
    .find_map(|rule| match rule {
      CssRule::Style(style_rule) => style_rule.selectors.slice().first(),
      _ => None,
    })
    .expect("style selector");

  let mut caches = SelectorCaches::default();
  caches.set_epoch(next_selector_cache_epoch());
  let mut context = MatchingContext::new(
    MatchingMode::Normal,
    None,
    &mut caches,
    QuirksMode::NoQuirks,
    NeedsSelectorFlags::No,
    MatchingForInvalidation::No,
  );
  context.extra_data = ShadowMatchData::for_shadow_host(element_ref.opaque());

  assert!(
    matches_selector(selector, 0, None, &element_ref, &mut context),
    ":host should match the owning shadow host"
  );

  let mut caches = SelectorCaches::default();
  caches.set_epoch(next_selector_cache_epoch());
  let mut context = MatchingContext::new(
    MatchingMode::Normal,
    None,
    &mut caches,
    QuirksMode::NoQuirks,
    NeedsSelectorFlags::No,
    MatchingForInvalidation::No,
  );
  let other = DomNode {
    node_type: DomNodeType::Element {
      tag_name: "span".into(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: vec![],
    },
    children: vec![],
  };
  context.extra_data = ShadowMatchData::for_shadow_host(ElementRef::new(&other).opaque());

  assert!(
    !matches_selector(selector, 0, None, &element_ref, &mut context),
    ":host should not match when the shadow host differs"
  );

  let mut caches = SelectorCaches::default();
  caches.set_epoch(next_selector_cache_epoch());
  let mut context = MatchingContext::new(
    MatchingMode::Normal,
    None,
    &mut caches,
    QuirksMode::NoQuirks,
    NeedsSelectorFlags::No,
    MatchingForInvalidation::No,
  );
  context.extra_data = ShadowMatchData::for_document();

  assert!(
    !matches_selector(selector, 0, None, &element_ref, &mut context),
    ":host should not match outside a shadow stylesheet"
  );
}
