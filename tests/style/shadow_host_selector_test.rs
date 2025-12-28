use fastrender::css::parser::parse_stylesheet;
use fastrender::css::selectors::ShadowMatchData;
use fastrender::css::types::CssRule;
use fastrender::dom::{
  self, next_selector_cache_epoch, DomNode, DomNodeType, ElementRef, HTML_NAMESPACE,
};
use fastrender::style::cascade::{
  apply_style_set_with_media_target_and_imports, apply_styles, StyledNode,
};
use fastrender::style::defaults::get_default_styles_for_element;
use fastrender::style::media::MediaContext;
use fastrender::style::style_set::StyleSet;
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

fn find_styled_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .is_some_and(|value| value.eq_ignore_ascii_case(id))
  {
    return Some(node);
  }

  node
    .children
    .iter()
    .find_map(|child| find_styled_by_id(child, id))
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
fn document_host_selector_is_ignored_for_real_shadow_host() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <div>shadow child</div>
      </template>
    </div>
  "#;

  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet(":host { padding-left: 5px; }").unwrap();
  let style_set = StyleSet::from_document(stylesheet);
  let media = MediaContext::screen(800.0, 600.0);
  let styled = apply_style_set_with_media_target_and_imports(
    &dom, &style_set, &media, None, None, None, None, None, None,
  );

  let host = find_styled_by_id(&styled, "host").expect("host element styled");
  let defaults = get_default_styles_for_element(&host.node);

  assert_eq!(
    host.styles.padding_left, defaults.padding_left,
    "Document :host selectors should not match real shadow hosts"
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
