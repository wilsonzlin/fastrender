use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::cascade::StyledNode;
use fastrender::style::media::MediaContext;
use fastrender::style::string_set::StringSetValue;

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
fn computed_style_captures_string_set_assignments() {
  let dom = dom::parse_html(
    r#"<main>
         <div class="chapter"></div>
         <p class="title"></p>
       </main>"#,
  )
  .unwrap();
  let stylesheet = parse_stylesheet(
    r#"
      div.chapter { string-set: chapter content(); }
      p.title { string-set: heading "Intro"; }
    "#,
  )
  .unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

  let chapter = &find_first(&styled, "div").expect("div");
  assert_eq!(chapter.styles.string_set.len(), 1);
  assert!(matches!(
    chapter.styles.string_set[0].value,
    StringSetValue::Content
  ));

  let heading = &find_first(&styled, "p").expect("p");
  assert_eq!(heading.styles.string_set.len(), 1);
  assert!(matches!(
    heading.styles.string_set[0].value,
    StringSetValue::Literal(ref s) if s == "Intro"
  ));
}
