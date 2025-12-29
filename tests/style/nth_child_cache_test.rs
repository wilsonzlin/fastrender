use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::color::Rgba;
use fastrender::style::media::MediaContext;
use fastrender::style::values::Length;
use std::time::{Duration, Instant};

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
fn structural_pseudos_match_correctly_with_cached_positions() {
  let html = r#"
    <div id="root">
      <p id="a"></p>
      <p id="b"></p>
      <span id="c"></span>
      <p id="d"></p>
      <span id="e"></span>
    </div>
  "#;
  let dom = dom::parse_html(html).expect("parse html");
  let css = r#"
    #root > * { display: block; color: rgb(10 10 10); }
    #root > p:first-child { display: inline; }
    #root > p:nth-of-type(2) { display: inline-block; }
    #root > p:nth-last-child(2) { color: rgb(1 2 3); }
    #root > p:last-of-type { display: list-item; }
    #root > span:nth-child(3) { display: table; }
    #root > span:nth-last-child(1) { display: flex; }
  "#;
  let stylesheet = parse_stylesheet(css).expect("stylesheet");
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  assert_eq!(display(find_by_id(&styled, "a").expect("a")), "inline");
  assert_eq!(
    display(find_by_id(&styled, "b").expect("b")),
    "inline-block"
  );
  let d = find_by_id(&styled, "d").expect("d");
  assert_eq!(d.styles.color, Rgba::rgb(1, 2, 3));
  assert_eq!(display(d), "list-item");
  assert_eq!(display(find_by_id(&styled, "c").expect("c")), "table");
  assert_eq!(display(find_by_id(&styled, "e").expect("e")), "flex");
}

#[test]
#[ignore = "performance-sensitive"]
fn many_sibling_structural_pseudos_finish_quickly() {
  let sibling_count = 6_000usize;
  let mut html = String::from("<ul>");
  for idx in 0..sibling_count {
    html.push_str(&format!(r#"<li id="n{idx}"></li>"#));
  }
  html.push_str("</ul>");

  let dom = dom::parse_html(&html).expect("parse html");
  let css = r#"
    li:nth-child(2n) { color: rgb(1 2 3); }
    li:nth-last-child(3) { color: rgb(4 5 6); }
    li:first-child { padding-left: 2px; }
    li:nth-of-type(3n+1) { display: inline-block; }
  "#;
  let stylesheet = parse_stylesheet(css).expect("stylesheet");
  let media = MediaContext::screen(1200.0, 800.0);

  let start = Instant::now();
  let styled = apply_styles_with_media(&dom, &stylesheet, &media);
  let elapsed = start.elapsed();

  // Spot-check a few nodes to ensure the cascade touched expected rules.
  assert_eq!(
    styled
      .children
      .first()
      .expect("first li")
      .styles
      .padding_left,
    Length::px(2.0)
  );
  assert_eq!(
    display(find_by_id(&styled, "n0").expect("first li")),
    "inline-block"
  );
  assert_eq!(
    find_by_id(&styled, "n1").expect("second li").styles.color,
    Rgba::rgb(1, 2, 3)
  );
  assert_eq!(
    find_by_id(&styled, &format!("n{}", sibling_count - 3))
      .expect("third-from-last")
      .styles
      .color,
    Rgba::rgb(4, 5, 6)
  );

  assert!(
    elapsed < Duration::from_millis(750),
    "cascade over {sibling_count} siblings with structural pseudos took {elapsed:?}"
  );
}
