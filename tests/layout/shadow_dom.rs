use fastrender::api::FastRender;
use fastrender::tree::fragment_tree::FragmentContent;

fn with_large_stack<F, R>(f: F) -> R
where
  F: FnOnce() -> R + Send + 'static,
  R: Send + 'static,
{
  const STACK_SIZE: usize = 8 * 1024 * 1024;
  let handle = std::thread::Builder::new()
    .stack_size(STACK_SIZE)
    .spawn(f)
    .expect("failed to spawn test thread");
  match handle.join() {
    Ok(result) => result,
    Err(payload) => std::panic::resume_unwind(payload),
  }
}

fn collect_text_fragments(node: &fastrender::FragmentNode, out: &mut Vec<String>) {
  let mut stack = vec![node];
  while let Some(node) = stack.pop() {
    if let FragmentContent::Text { text, .. } = &node.content {
      out.push(text.to_string());
    }
    for child in node.children.iter().rev() {
      stack.push(child);
    }
  }
}

#[test]
fn declarative_shadow_dom_renders_shadow_tree_and_slots() {
  with_large_stack(|| {
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
  });
}
