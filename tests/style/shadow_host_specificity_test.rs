use fastrender::css::parser::{extract_scoped_css_sources, parse_stylesheet, StylesheetSource};
use fastrender::css::types::StyleSheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_style_set_with_media_target_and_imports, StyledNode};
use fastrender::style::color::Rgba;
use fastrender::style::media::MediaContext;
use fastrender::style::style_set::StyleSet;
use std::collections::HashMap;

fn stylesheet_from_sources(sources: &[StylesheetSource]) -> StyleSheet {
  let mut combined = Vec::new();
  for source in sources {
    let StylesheetSource::Inline(inline) = source else {
      continue;
    };
    if inline.disabled || inline.css.trim().is_empty() {
      continue;
    }
    if let Ok(sheet) = parse_stylesheet(&inline.css) {
      combined.extend(sheet.rules);
    }
  }
  StyleSheet { rules: combined }
}

fn styled_tree_for(html: &str) -> StyledNode {
  let dom = dom::parse_html(html).expect("parse html");
  let scoped_sources = extract_scoped_css_sources(&dom);
  let mut shadows = HashMap::new();
  for (host, sources) in scoped_sources.shadows {
    shadows.insert(host, stylesheet_from_sources(&sources));
  }
  let style_set = StyleSet {
    document: stylesheet_from_sources(&scoped_sources.document),
    shadows,
  };
  let media = MediaContext::screen(800.0, 600.0);
  apply_style_set_with_media_target_and_imports(
    &dom, &style_set, &media, None, None, None, None, None, None,
  )
}

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .is_some_and(|value| value.eq_ignore_ascii_case(id))
  {
    return Some(node);
  }
  node.children.iter().find_map(|child| find_by_id(child, id))
}

#[test]
fn host_argument_specificity_beats_source_order() {
  let styled = styled_tree_for(
    r#"
      <div id="host" class="foo">
        <template shadowroot="open">
          <style>
            :host(#host) { color: rgb(1, 2, 3); }
            :host(.foo) { color: rgb(9, 8, 7); }
          </style>
          <div id="inside"></div>
        </template>
      </div>
    "#,
  );

  let host = find_by_id(&styled, "host").expect("host element");
  assert_eq!(host.styles.color, Rgba::new(1, 2, 3, 1.0));
}

#[test]
fn host_context_argument_specificity_beats_source_order() {
  let styled = styled_tree_for(
    r#"
      <section id="ctx" class="ctx">
        <div id="host" class="foo">
          <template shadowroot="open">
            <style>
              :host-context(#ctx) { background-color: rgb(4, 5, 6); }
              :host-context(.ctx) { background-color: rgb(7, 8, 9); }
            </style>
            <div>shadow</div>
          </template>
        </div>
      </section>
    "#,
  );

  let host = find_by_id(&styled, "host").expect("host element");
  assert_eq!(host.styles.background_color, Rgba::new(4, 5, 6, 1.0));
}
