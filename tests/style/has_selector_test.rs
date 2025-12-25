use fastrender::css::parser::parse_stylesheet;
use fastrender::css::types::CssRule;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaContext;

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .is_some_and(|value| value.eq_ignore_ascii_case(id))
  {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

fn display(node: &StyledNode) -> String {
  node.styles.display.to_string()
}

fn selector_specificity(css: &str) -> u32 {
  let stylesheet = parse_stylesheet(css).expect("stylesheet parses");
  let style_rule = stylesheet
    .rules
    .iter()
    .find_map(|rule| {
      if let CssRule::Style(rule) = rule {
        Some(rule)
      } else {
        None
      }
    })
    .expect("style rule");
  style_rule
    .selectors
    .slice()
    .first()
    .expect("selector")
    .specificity()
}

#[test]
fn descendant_and_child_has_match() {
  let html = r#"
    <div id="desc"><span></span></div>
    <div id="child"><span class="hit"></span></div>
    <div id="miss"></div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    div:has(span) { display: inline; }
    div:has(> .hit) { display: inline-block; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "desc").expect("desc")),
    "inline"
  );
  assert_eq!(
    display(find_by_id(&styled, "child").expect("child")),
    "inline-block"
  );
  assert_eq!(display(find_by_id(&styled, "miss").expect("miss")), "block");
}

#[test]
fn sibling_variants_match_has() {
  let html = r#"
    <p id="first"></p>
    <span class="marker"></span>
    <p id="second"></p>
    <div id="before"></div>
    <div></div>
    <p class="note"></p>
    <div id="after"></div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    p:has(+ .marker) { display: inline; }
    div:has(~ p.note) { display: inline-flex; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "first").expect("first")),
    "inline"
  );
  assert_eq!(
    display(find_by_id(&styled, "second").expect("second")),
    "block"
  );
  assert_eq!(
    display(find_by_id(&styled, "before").expect("before")),
    "inline-flex"
  );
  assert_eq!(
    display(find_by_id(&styled, "after").expect("after")),
    "block"
  );
}

#[test]
fn has_combines_with_is_and_not() {
  let html = r#"
    <section id="wrap"><div class="match"><span></span></div></section>
    <section id="miss"><div></div></section>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    :is(section, article):has(.match) { display: inline; }
    section:not(:has(span)) { display: table; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "wrap").expect("wrap")),
    "inline"
  );
  assert_eq!(display(find_by_id(&styled, "miss").expect("miss")), "table");
}

#[test]
fn has_contributes_specificity() {
  let html = r#"<div id="spec" class="highlight"><span class="target"></span></div>"#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    div.highlight { display: block; }
    div:has(.target) { display: inline; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  assert_eq!(
    display(find_by_id(&styled, "spec").expect("spec")),
    "inline"
  );
}

#[test]
fn has_specificity_uses_most_specific_argument() {
  let div_spec = selector_specificity("div {}");
  let class_spec = selector_specificity(".a {}");
  let id_spec = selector_specificity("#child {}");

  assert_eq!(
    selector_specificity("div:has(.a) {}"),
    div_spec + class_spec
  );
  assert_eq!(
    selector_specificity("div:has(#child) {}"),
    div_spec + id_spec
  );
  assert_eq!(
    selector_specificity("div:has(:where(.a, span)) {}"),
    div_spec
  );
}

#[test]
fn has_matches_or_list() {
  let html = r#"<div id="host"><span class="a"></span></div><div id="miss"></div>"#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"div:has(.a, .b) { display: inline; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "host").expect("host")),
    "inline"
  );
  assert_eq!(display(find_by_id(&styled, "miss").expect("miss")), "block");
}

#[test]
fn has_supports_is_and_where_in_argument() {
  let html = r#"
    <div id="span-hit"><span></span></div>
    <div id="class-hit"><p class="a"></p></div>
    <div id="miss"></div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    div:has(:is(.a, span)) { display: inline-block; }
    div:has(:where(.a, span)) { display: table; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "span-hit").expect("span-hit")),
    "inline-block"
  );
  assert_eq!(
    display(find_by_id(&styled, "class-hit").expect("class-hit")),
    "inline-block"
  );
  assert_eq!(display(find_by_id(&styled, "miss").expect("miss")), "block");
}

#[test]
fn has_matches_descendant_adjacent_pattern() {
  let html = r#"
    <div id="hit"><p><span></span><em></em></p></div>
    <div id="miss"><p><span></span></p></div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"div:has(span + em) { display: inline; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(display(find_by_id(&styled, "hit").expect("hit")), "inline");
  assert_eq!(display(find_by_id(&styled, "miss").expect("miss")), "block");
}

#[test]
fn has_matches_attribute_selector_argument() {
  let html = r#"
    <div id="hit"><span data-hit></span></div>
    <div id="miss"><span></span></div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"div:has([data-hit]) { display: inline; }"#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(display(find_by_id(&styled, "hit").expect("hit")), "inline");
  assert_eq!(display(find_by_id(&styled, "miss").expect("miss")), "block");
}

#[test]
fn has_uses_most_specific_argument_for_specificity() {
  let html = r#"<div id="host"><span id="child" class="a"></span></div>"#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    div:has(#child) { display: inline; }
    div:has(.a) { display: inline-block; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "host").expect("host")),
    "inline"
  );
}

#[test]
fn has_respects_shadow_boundaries() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <slot></slot>
        <div class="shadow-child"></div>
      </template>
      <div class="light-child"></div>
    </div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    #host { display: block; }
    #host:has(.light-child) { display: inline; }
    #host:has(.shadow-child) { display: inline-block; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "host").expect("host")),
    "inline"
  );
}

#[test]
fn has_inside_shadow_root_skips_nested_shadow_trees() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <div id="outer">
          <div class="light"></div>
          <div id="nested-host">
            <template shadowroot="open">
              <div class="inner"></div>
            </template>
          </div>
        </div>
      </template>
    </div>
  "#;
  let dom = dom::parse_html(html).unwrap();
  let css = r#"
    #outer { display: block; }
    #outer:has(.light) { display: inline; }
    #outer:has(.inner) { display: inline-block; }
  "#;
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(
    display(find_by_id(&styled, "outer").expect("outer")),
    "inline"
  );
}
