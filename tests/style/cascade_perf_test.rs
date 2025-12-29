use std::time::{Duration, Instant};

use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::parse_html;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;

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

fn display(node: &StyledNode) -> String {
  node.styles.display.to_string()
}

#[test]
fn cascade_handles_large_rule_sets_under_budget() {
  let variants = 600usize;
  let node_count = 180usize;

  let mut css = String::from(".card { display: inline-flex; }\n");
  for idx in 0..variants {
    css.push_str(&format!(
      ".c{idx} {{ padding: {}px; border-width: {}px; }}\n",
      (idx % 5) + 1,
      idx % 4
    ));
    css.push_str(&format!(
      ".c{idx}:has(.flag.f{}) {{ margin-left: {}px; }}\n",
      idx % 50,
      idx % 9
    ));
    css.push_str(&format!(
      ".c{idx} .content {{ min-height: {}px; }}\n",
      24 + (idx % 10)
    ));
  }
  let stylesheet = parse_stylesheet(&css).expect("stylesheet parses");

  let mut html = String::from("<div id=\"root\">");
  for idx in 0..node_count {
    html.push_str(&format!(
      "<div id=\"card{idx}\" class=\"card c{cls}\"><div class=\"content\"><span class=\"flag f{flag}\"></span></div></div>",
      cls = idx % variants,
      flag = idx % 50
    ));
  }
  html.push_str("</div>");
  let dom = parse_html(&html).expect("html parses");

  let media = MediaContext::screen(1280.0, 720.0);
  let start = Instant::now();
  let styled = apply_styles_with_media(&dom, &stylesheet, &media);
  let elapsed = start.elapsed();

  assert!(
    elapsed < Duration::from_millis(1500),
    "cascade perf regression: {}ms for {} rules over {} nodes",
    elapsed.as_millis(),
    variants * 3 + 1,
    node_count
  );

  assert_eq!(
    display(find_by_id(&styled, "card0").expect("card0 styled")),
    "inline-flex"
  );
}

#[test]
fn cascade_handles_thousands_of_has_rules_under_budget() {
  let variants = 1200usize;
  let node_count = 160usize;

  let mut css = String::from(".item { display: inline-flex; }\n");
  for idx in 0..variants {
    css.push_str(&format!(
      ".v{idx} {{ padding: {}px; border-width: {}px; }}\n",
      (idx % 5) + 1,
      idx % 3
    ));
    css.push_str(&format!(
      ".v{idx}:has(.flag{}) {{ margin-left: {}px; }}\n",
      idx % 60,
      (idx % 7) + 1
    ));
    css.push_str(&format!(
      ".v{idx} .body {{ min-height: {}px; }}\n",
      12 + (idx % 16)
    ));
  }
  let stylesheet = parse_stylesheet(&css).expect("stylesheet parses");

  let mut html = String::from("<div id=\"root\">");
  for idx in 0..node_count {
    html.push_str(&format!(
      "<div id=\"item{idx}\" class=\"item v{class}\"><div class=\"body\"><span class=\"flag{flag}\"></span></div></div>",
      class = idx % variants,
      flag = idx % 60,
    ));
  }
  html.push_str("</div>");
  let dom = parse_html(&html).expect("html parses");

  let media = MediaContext::screen(1440.0, 900.0);
  let start = Instant::now();
  let styled = apply_styles_with_media(&dom, &stylesheet, &media);
  let elapsed = start.elapsed();

  assert!(
    elapsed < Duration::from_millis(2000),
    "cascade perf regression: {}ms for {} rules over {} nodes",
    elapsed.as_millis(),
    variants * 3 + 1,
    node_count
  );

  assert_eq!(
    display(find_by_id(&styled, "item0").expect("item0 styled")),
    "inline-flex"
  );
}
