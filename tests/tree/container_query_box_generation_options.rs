use fastrender::api::{FastRender, RenderArtifactRequest, RenderOptions};
use fastrender::tree::box_tree::BoxNode;

fn contains_class(node: &BoxNode, class: &str) -> bool {
  if let Some(info) = &node.debug_info {
    if info.classes.iter().any(|c| c == class) {
      return true;
    }
  }
  node
    .children
    .iter()
    .any(|child| contains_class(child, class))
}

#[test]
fn container_query_rel_layout_uses_box_generation_options() {
  std::thread::Builder::new()
    .stack_size(8 * 1024 * 1024)
    .spawn(|| {
      // Force a container-query second pass (fingerprints differ) while running under the
      // site-compatibility profile. The second-pass box generation must keep using the
      // compatibility options; otherwise empty ad placeholders would reappear.
      let html = r#"
        <style>
          .container {
            container-type: inline-size;
            width: 200px;
          }
          .target { display: none; }
          @container (min-width: 150px) {
            .target { display: block; }
          }
        </style>
        <div class="container">
          <div class="ad-height-hold"></div>
          <div class="target">hello</div>
        </div>
      "#;

      let mut renderer = FastRender::builder()
        .viewport_size(800, 600)
        .with_site_compat_hacks()
        .build()
        .expect("build renderer");

      let report = renderer
        .render_html_with_stylesheets_report(
          html,
          "https://example.com",
          RenderOptions::default(),
          RenderArtifactRequest {
            box_tree: true,
            ..RenderArtifactRequest::default()
          },
        )
        .expect("render html");

      let box_tree = report
        .artifacts
        .box_tree
        .expect("expected box tree artifact");
      assert!(
        !contains_class(&box_tree.root, "ad-height-hold"),
        "empty ad placeholders should remain dropped after container-query relayout",
      );
    })
    .expect("spawn test thread")
    .join()
    .expect("join test thread");
}
