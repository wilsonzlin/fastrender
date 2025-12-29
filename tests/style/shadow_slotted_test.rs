use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles;
use fastrender::style::cascade::StyledNode;
use fastrender::style::color::Rgba;

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

#[test]
fn slotted_element_receives_slotted_shadow_styles() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <style>
          ::slotted(.a) { color: rgb(255, 0, 0); }
          .a { color: rgb(0, 255, 0); }
        </style>
        <slot></slot>
      </template>
      <span id="light" class="a">Light</span>
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet("").expect("empty stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let light = find_by_id(&styled, "light").expect("slotted element");
  assert_eq!(light.styles.color, Rgba::rgb(255, 0, 0));
}

#[test]
fn shadow_styles_without_slotted_do_not_apply_to_light_dom() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <style>
          .a { color: rgb(0, 255, 0); }
        </style>
        <slot></slot>
      </template>
      <span id="light" class="a">Light</span>
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet("").expect("empty stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let light = find_by_id(&styled, "light").expect("slotted element");
  assert_ne!(light.styles.color, Rgba::rgb(0, 255, 0));
}

#[test]
fn document_styles_still_apply_to_slotted_element() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <style>
          ::slotted(.a) { color: rgb(255, 0, 0); }
        </style>
        <slot></slot>
      </template>
      <span id="light" class="a">Light</span>
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet(".a { opacity: 0.5; }").expect("doc stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let light = find_by_id(&styled, "light").expect("slotted element");
  assert_eq!(light.styles.opacity, 0.5);
  assert_eq!(light.styles.color, Rgba::rgb(255, 0, 0));
}

#[test]
fn invalid_slotted_selector_is_ignored() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <style>
          ::slotted(.a .b) { color: rgb(10, 20, 30); }
        </style>
        <slot></slot>
      </template>
      <span id="light" class="a">Light</span>
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet(".a { color: rgb(1, 2, 3); }").expect("doc stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let light = find_by_id(&styled, "light").expect("slotted element");
  assert_eq!(light.styles.color, Rgba::rgb(1, 2, 3));
}

#[test]
fn slotted_prelude_matches_implicit_universal() {
  let html = r#"
    <div id="host" class="foo">
      <template shadowroot="open">
        <style>
          :host(.foo) ::slotted(.a) { color: rgb(255, 0, 0); }
        </style>
        <slot></slot>
      </template>
      <span id="light" class="a">Light</span>
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet(".a { color: rgb(0, 255, 0); }").expect("doc stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let light = find_by_id(&styled, "light").expect("slotted element");
  assert_eq!(light.styles.color, Rgba::rgb(255, 0, 0));
}

#[test]
fn slotted_prelude_respects_host_filter() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <style>
          :host(.foo) ::slotted(.a) { color: rgb(255, 0, 0); }
        </style>
        <slot></slot>
      </template>
      <span id="light" class="a">Light</span>
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet(".a { color: rgb(0, 255, 0); }").expect("doc stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let light = find_by_id(&styled, "light").expect("slotted element");
  assert_eq!(light.styles.color, Rgba::rgb(0, 255, 0));
}

#[test]
fn slotted_rule_targets_named_slot() {
  let html = r#"
    <div id="host">
      <template shadowroot="open">
        <style>
          slot[name=foo]::slotted(.a) { color: rgb(10, 20, 30); }
        </style>
        <slot name="foo"></slot>
        <slot name="bar"></slot>
      </template>
      <span id="foo-light" class="a" slot="foo">Foo</span>
      <span id="bar-light" class="a" slot="bar">Bar</span>
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet(".a { color: rgb(0, 0, 255); }").expect("doc stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let foo = find_by_id(&styled, "foo-light").expect("foo slotted element");
  let bar = find_by_id(&styled, "bar-light").expect("bar slotted element");
  assert_eq!(foo.styles.color, Rgba::rgb(10, 20, 30));
  assert_eq!(bar.styles.color, Rgba::rgb(0, 0, 255));
}

#[test]
fn slotted_prelude_honors_host_selector() {
  let html = r#"
    <div class="ctx">
      <div id="host-with" class="foo">
        <template shadowroot="open">
          <style>
            :host(.foo) slot::slotted(.a) { color: rgb(10, 20, 30); }
          </style>
          <slot></slot>
        </template>
        <span id="light-with" class="a">Light</span>
      </div>
      <div id="host-without">
        <template shadowroot="open">
          <style>
            :host(.foo) slot::slotted(.a) { color: rgb(10, 20, 30); }
          </style>
          <slot></slot>
        </template>
        <span id="light-without" class="a">Light</span>
      </div>
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet("").expect("empty stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let light_with = find_by_id(&styled, "light-with").expect("slotted element in matching host");
  let light_without =
    find_by_id(&styled, "light-without").expect("slotted element in non-matching host");

  assert_eq!(light_with.styles.color, Rgba::rgb(10, 20, 30));
  assert_ne!(light_without.styles.color, Rgba::rgb(10, 20, 30));
}

#[test]
fn slotted_prelude_honors_host_context_selector() {
  let html = r#"
    <div class="ctx">
      <div id="host-inside">
        <template shadowroot="open">
          <style>
            :host-context(.ctx) ::slotted(.a) { color: rgb(50, 60, 70); }
          </style>
          <slot></slot>
        </template>
        <span id="light-inside" class="a">Light</span>
      </div>
    </div>
    <div id="host-outside">
      <template shadowroot="open">
        <style>
          :host-context(.ctx) ::slotted(.a) { color: rgb(50, 60, 70); }
        </style>
        <slot></slot>
      </template>
      <span id="light-outside" class="a">Light</span>
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet("").expect("empty stylesheet");
  let styled = apply_styles(&dom, &stylesheet);

  let light_inside =
    find_by_id(&styled, "light-inside").expect("slotted element inside matching context");
  let light_outside =
    find_by_id(&styled, "light-outside").expect("slotted element outside matching context");

  assert_eq!(light_inside.styles.color, Rgba::rgb(50, 60, 70));
  assert_ne!(light_outside.styles.color, Rgba::rgb(50, 60, 70));
}
