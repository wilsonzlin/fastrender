use fastrender::tree::fragment_tree::FragmentContent;
use fastrender::FastRender;

fn collect_text(fragment: &fastrender::FragmentNode, texts: &mut Vec<String>) {
  if let FragmentContent::Text { text, .. } = &fragment.content {
    texts.push(text.to_string());
  }
  for child in fragment.children.iter() {
    collect_text(child, texts);
  }
}

#[test]
fn noscript_content_is_rendered_when_scripting_disabled() {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer
    .parse_html(
      r#"
        <html><body>
            <noscript><div>noscript text</div></noscript>
            <div>live text</div>
        </body></html>
    "#,
    )
    .expect("parse");

  let tree = renderer.layout_document(&dom, 200, 100).expect("layout");
  let mut texts = Vec::new();
  collect_text(&tree.root, &mut texts);

  assert!(
    texts.iter().any(|t| t.contains("noscript text")),
    "expected noscript text"
  );
  assert!(
    texts.iter().any(|t| t.contains("live text")),
    "expected normal text"
  );
}
