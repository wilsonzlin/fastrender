use cssparser::{Parser, ParserInput, ToCss};
use fastrender::css::parser::parse_stylesheet;
use fastrender::css::selectors::{PseudoClassParser, ShadowMatchData};
use fastrender::dom::{next_selector_cache_epoch, DomNode, ElementRef};
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::color::Rgba;
use fastrender::style::media::MediaContext;
use selectors::parser::SelectorList;
use selectors::Element;

fn selector_roundtrip(selector: &str) -> String {
  let mut input = ParserInput::new(selector);
  let mut parser = Parser::new(&mut input);
  SelectorList::parse(
    &PseudoClassParser,
    &mut parser,
    selectors::parser::ParseRelative::No,
  )
  .expect("selector should parse")
  .to_css_string()
}

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .is_some_and(|value| value.eq_ignore_ascii_case(id))
  {
    return Some(node);
  }
  for child in node.children.iter() {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

fn find_node_with_ancestors<'a>(
  node: &'a DomNode,
  id: &str,
  ancestors: &mut Vec<&'a DomNode>,
) -> Option<(&'a DomNode, Vec<&'a DomNode>)> {
  if node
    .get_attribute_ref("id")
    .is_some_and(|value| value.eq_ignore_ascii_case(id))
  {
    return Some((node, ancestors.clone()));
  }
  for child in node.children.iter() {
    ancestors.push(node);
    let found = find_node_with_ancestors(child, id, ancestors);
    ancestors.pop();
    if found.is_some() {
      return found;
    }
  }
  None
}

#[test]
fn parses_shadow_host_pseudo_classes() {
  assert_eq!(selector_roundtrip(":host"), ":host");
  assert_eq!(selector_roundtrip(":host(.a)"), ":host(.a)");
  assert_eq!(selector_roundtrip(":host-context(.a)"), ":host-context(.a)");

  let mut input = ParserInput::new(":host(.a .b)");
  let mut parser = Parser::new(&mut input);
  let parsed = SelectorList::parse(
    &PseudoClassParser,
    &mut parser,
    selectors::parser::ParseRelative::No,
  );
  assert!(
    parsed.is_err(),
    ":host() should reject selectors with combinators"
  );
}

#[test]
fn matches_host_and_host_context_selectors() {
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(|| {
      let html = r#"
        <div class="ctx">
          <div id="host" class="foo">
            <template shadowroot="open">
              <div id="inner" class="x"></div>
            </template>
          </div>
        </div>
      "#;
      let dom = fastrender::dom::parse_html(html).unwrap();
      let css = r#"
        :host { color: rgb(1, 2, 3); }
        :host(.foo) { background-color: rgb(4, 5, 6); }
        :host-context(.ctx) .x { color: rgb(7, 8, 9); }
      "#;
      let stylesheet = parse_stylesheet(css).unwrap();

      let (host_node, host_ancestors) =
        find_node_with_ancestors(&dom, "host", &mut Vec::new()).expect("host node");
      let host_ref = ElementRef::with_ancestors(host_node, &host_ancestors);

      let mut host_ctx_input = ParserInput::new(":host-context(.ctx)");
      let mut host_ctx_parser = Parser::new(&mut host_ctx_input);
      let host_ctx_selector_list = SelectorList::parse(
        &PseudoClassParser,
        &mut host_ctx_parser,
        selectors::parser::ParseRelative::No,
      )
      .unwrap();
      let host_ctx_selector = &host_ctx_selector_list.slice()[0];
      let mut host_ctx_caches = selectors::context::SelectorCaches::default();
      host_ctx_caches.set_epoch(next_selector_cache_epoch());
      let mut host_ctx = selectors::matching::MatchingContext::new_for_visited(
        selectors::matching::MatchingMode::Normal,
        None,
        &mut host_ctx_caches,
        selectors::context::VisitedHandlingMode::AllLinksVisitedAndUnvisited,
        selectors::context::IncludeStartingStyle::No,
        selectors::context::QuirksMode::NoQuirks,
        selectors::matching::NeedsSelectorFlags::No,
        selectors::matching::MatchingForInvalidation::No,
      );
      host_ctx.extra_data = ShadowMatchData::for_shadow_host(host_ref.opaque());
      assert!(
        host_ctx.with_shadow_host(Some(host_ref), |ctx| {
          selectors::matching::matches_selector(host_ctx_selector, 0, None, &host_ref, ctx)
        }),
        ":host-context(.ctx) should match the shadow host"
      );

      let mut host_input = ParserInput::new(":host(.foo)");
      let mut host_parser = Parser::new(&mut host_input);
      let host_selector_list = SelectorList::parse(
        &PseudoClassParser,
        &mut host_parser,
        selectors::parser::ParseRelative::No,
      )
      .unwrap();
      let host_selector = &host_selector_list.slice()[0];
      let mut host_caches = selectors::context::SelectorCaches::default();
      host_caches.set_epoch(next_selector_cache_epoch());
      let mut host_ctx = selectors::matching::MatchingContext::new_for_visited(
        selectors::matching::MatchingMode::Normal,
        None,
        &mut host_caches,
        selectors::context::VisitedHandlingMode::AllLinksVisitedAndUnvisited,
        selectors::context::IncludeStartingStyle::No,
        selectors::context::QuirksMode::NoQuirks,
        selectors::matching::NeedsSelectorFlags::No,
        selectors::matching::MatchingForInvalidation::No,
      );
      host_ctx.extra_data = ShadowMatchData::for_shadow_host(host_ref.opaque());
      let host_matches = host_ctx.with_shadow_host(Some(host_ref), |ctx| {
        selectors::matching::matches_selector(host_selector, 0, None, &host_ref, ctx)
      });
      assert!(
        host_matches,
        ":host(.foo) should match a shadow host with class foo"
      );

      let (inner_node, inner_ancestors) =
        find_node_with_ancestors(&dom, "inner", &mut Vec::new()).expect("inner node");
      let inner_ref = ElementRef::with_ancestors(inner_node, &inner_ancestors);
      let mut inner_input = ParserInput::new(":host-context(.ctx) .x");
      let mut inner_parser = Parser::new(&mut inner_input);
      let inner_selector_list = SelectorList::parse(
        &PseudoClassParser,
        &mut inner_parser,
        selectors::parser::ParseRelative::No,
      )
      .unwrap();
      let inner_selector = &inner_selector_list.slice()[0];
      let mut inner_caches = selectors::context::SelectorCaches::default();
      inner_caches.set_epoch(next_selector_cache_epoch());
      let mut inner_ctx = selectors::matching::MatchingContext::new_for_visited(
        selectors::matching::MatchingMode::Normal,
        None,
        &mut inner_caches,
        selectors::context::VisitedHandlingMode::AllLinksVisitedAndUnvisited,
        selectors::context::IncludeStartingStyle::No,
        selectors::context::QuirksMode::NoQuirks,
        selectors::matching::NeedsSelectorFlags::No,
        selectors::matching::MatchingForInvalidation::No,
      );
      inner_ctx.extra_data = ShadowMatchData::for_shadow_host(host_ref.opaque());
      let mut inner_matches = inner_ctx.with_shadow_host(Some(host_ref), |ctx| {
        selectors::matching::matches_selector(inner_selector, 0, None, &inner_ref, ctx)
      });
      if !inner_matches {
        inner_matches = inner_ctx.with_shadow_host(Some(host_ref), |ctx| {
          ctx.with_featureless(false, |ctx| {
            selectors::matching::matches_selector(inner_selector, 0, None, &inner_ref, ctx)
          })
        });
      }
      assert!(
        inner_matches,
        ":host-context(.ctx) .x should match shadow tree content"
      );

      let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

      let host = find_by_id(&styled, "host").expect("host element");
      assert_eq!(host.styles.color, Rgba::new(1, 2, 3, 1.0));
      assert_eq!(host.styles.background_color, Rgba::new(4, 5, 6, 1.0));

      let inner = find_by_id(&styled, "inner").expect("shadow content");
      assert_eq!(inner.styles.color, Rgba::new(7, 8, 9, 1.0));
    })
    .unwrap()
    .join()
    .unwrap();
}
