use fastrender::api::FastRender;
use fastrender::style::color::Rgba;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentTree};
use std::fs;
use tempfile::tempdir;

fn text_color_for(tree: &FragmentTree, needle: &str) -> Option<Rgba> {
  tree.iter_fragments().find_map(|frag| match &frag.content {
    FragmentContent::Text { text, .. } if text.contains(needle) => {
      frag.style.as_ref().map(|s| s.color)
    }
    _ => None,
  })
}

#[test]
fn external_file_stylesheet_affects_layout() {
  let dir = tempdir().unwrap();
  let css_path = dir.path().join("style.css");
  fs::write(&css_path, "body { color: rgb(12, 34, 56); }").unwrap();

  let base_url = format!("file://{}/index.html", dir.path().display());
  let html = r#"
    <html>
      <head>
        <link rel="STYLESHEET" href="style.css">
      </head>
      <body>
        <div id="text">Hello external</div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::builder().base_url(base_url).build().unwrap();

  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 200, 200).unwrap();

  let color = text_color_for(&tree, "Hello external").expect("text fragment");
  assert_eq!(color, Rgba::rgb(12, 34, 56));
}
