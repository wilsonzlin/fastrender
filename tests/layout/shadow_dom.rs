use fastrender::api::FastRender;
use fastrender::tree::fragment_tree::FragmentContent;

fn collect_text_fragments(node: &fastrender::FragmentNode, out: &mut Vec<String>) {
  if let FragmentContent::Text { text, .. } = &node.content {
    out.push(text.clone());
  }
  for child in &node.children {
    collect_text_fragments(child, out);
  }
}

#[test]
fn declarative_shadow_dom_renders_shadow_tree_and_slots() {
  let html = r#"
    <x-card>
      <template shadowroot="open">
        <div>shadow:<slot name="title"></slot><slot></slot></div>
      </template>
      <span slot="title">Title</span>
      <p>Body</p>
    </x-card>
  "#;

  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(html).expect("dom");
  let tree = renderer
    .layout_document(&dom, 400, 200)
    .expect("layout should succeed");

  let mut texts = Vec::new();
  collect_text_fragments(&tree.root, &mut texts);
  let joined = texts.join(" ");

  assert!(joined.contains("shadow"), "shadow content should render");
  assert!(
    joined.contains("Title"),
    "named slot should render assigned content"
  );
  assert!(
    joined.contains("Body"),
    "default slot should render light DOM"
  );
}
