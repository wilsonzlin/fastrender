use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::parse_html;
use fastrender::style::cascade::{
  apply_style_set_with_media_target_and_imports, capture_cascade_profile, cascade_profile_enabled,
  reset_cascade_profile, set_cascade_profile_enabled, StyledNode,
};
use fastrender::style::media::MediaContext;
use fastrender::style::style_set::StyleSet;
use fastrender::Rgba;
use std::collections::HashMap;

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node.node.get_attribute_ref("id") == Some(id) {
    return Some(node);
  }
  for child in node.children.iter() {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

fn color_for(idx: usize) -> (u8, u8, u8) {
  let r = (idx * 13 % 256) as u8;
  let g = (idx * 7 % 256) as u8;
  let b = (idx * 3 % 256) as u8;
  (r, g, b)
}

#[test]
fn many_part_rules_scale_and_style_correctly() {
  // Stress a large ::part() selector set with many hosts and an exportparts hop per host.
  // When FASTR_CASCADE_PROFILE=1 this test also emits cascade profiling counters, which
  // should show that rule candidates stay bounded by touched part names instead of all
  // part selectors.
  let part_rule_count = 120usize;
  let host_count = 80usize;

  let mut css = String::new();
  for idx in 0..part_rule_count {
    let (r, g, b) = color_for(idx);
    css.push_str(&format!(
      "x-outer::part(alias-{idx}) {{ color: rgb({r}, {g}, {b}); }}\n"
    ));
  }
  let stylesheet = parse_stylesheet(&css).expect("parsed stylesheet");

  let mut html = String::from("<div id=\"root\">");
  for idx in 0..host_count {
    let leaf_part = format!("leaf-{}", idx % part_rule_count);
    let alias = format!("alias-{}", idx % part_rule_count);
    html.push_str(&format!(
      r#"
        <x-outer id="outer-{idx}">
          <template shadowroot="open">
            <x-inner exportparts="{leaf_part}:{alias}">
              <template shadowroot="open">
                <span id="leaf-{idx}" part="{leaf_part} shared">Leaf {idx}</span>
              </template>
            </x-inner>
          </template>
        </x-outer>
      "#
    ));
  }
  html.push_str("</div>");

  let dom = parse_html(&html).expect("parsed html");
  let style_set = StyleSet {
    document: stylesheet,
    shadows: HashMap::new(),
  };
  let media = MediaContext::screen(1200.0, 800.0);

  let profile_before = cascade_profile_enabled();
  let profile_enabled = std::env::var("FASTR_CASCADE_PROFILE")
    .ok()
    .map(|v| v != "0")
    .unwrap_or(false);
  if profile_enabled {
    set_cascade_profile_enabled(true);
    reset_cascade_profile();
  }

  let styled = apply_style_set_with_media_target_and_imports(
    &dom, &style_set, &media, None, None, None, None, None, None,
  );

  let sample_ids = [0, host_count / 2, host_count - 1];
  for idx in sample_ids {
    let node = find_by_id(&styled, &format!("leaf-{idx}")).expect("leaf");
    let (r, g, b) = color_for(idx % part_rule_count);
    assert_eq!(node.styles.color, Rgba::rgb(r, g, b));
  }

  if profile_enabled {
    let stats = capture_cascade_profile();
    eprintln!(
      "[::part stress] nodes={} candidates={} matches={} find_ms={:.2}",
      stats.nodes,
      stats.rule_candidates,
      stats.rule_matches,
      stats.selector_time_ns as f64 / 1_000_000.0
    );
    set_cascade_profile_enabled(profile_before);
  }
}
