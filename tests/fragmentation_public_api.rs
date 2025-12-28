use fastrender::api::FastRender;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};

fn fragment_roots(tree: &FragmentTree) -> Vec<&FragmentNode> {
  let mut roots = vec![&tree.root];
  roots.extend(tree.additional_fragments.iter());
  roots
}

fn fragment_contains_text(node: &FragmentNode, needle: &str) -> bool {
  if let FragmentContent::Text { text, .. } = &node.content {
    if text.contains(needle) {
      return true;
    }
  }
  node
    .children
    .iter()
    .any(|child| fragment_contains_text(child, needle))
}

#[test]
fn builder_pagination_without_page_rules_fragments() {
  let mut renderer = FastRender::builder().paginate(120.0, 8.0).build().unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          body { margin: 0; }
          .tall { height: 360px; }
        </style>
      </head>
      <body>
        <div class="tall"></div>
      </body>
    </html>
  "#;

  let dom = renderer.parse_html(html).unwrap();
  let fragments = renderer.layout_document(&dom, 200, 200).unwrap();

  assert!(
    !fragments.additional_fragments.is_empty(),
    "manual pagination should create additional fragments when no @page rules are present"
  );
}

#[test]
fn page_rules_override_manual_fragmentation() {
  let mut renderer = FastRender::builder().paginate(60.0, 0.0).build().unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 400px 400px; margin: 0; }
          body { margin: 0; }
          .content { height: 180px; }
        </style>
      </head>
      <body>
        <div class="content"></div>
      </body>
    </html>
  "#;

  let dom = renderer.parse_html(html).unwrap();
  let fragments = renderer.layout_document(&dom, 400, 400).unwrap();

  assert!(
    fragments.additional_fragments.is_empty(),
    "CSS @page rules should take precedence over manual pagination settings"
  );
}

#[test]
fn pagination_respects_vertical_writing_mode() {
  let mut renderer = FastRender::builder().paginate(120.0, 8.0).build().unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          html { writing-mode: vertical-rl; }
          body { margin: 0; }
          .tall { height: 360px; }
        </style>
      </head>
      <body>
        <div class="tall"></div>
      </body>
    </html>
  "#;

  let dom = renderer.parse_html(html).unwrap();
  let fragments = renderer.layout_document(&dom, 200, 200).unwrap();

  assert!(
    fragments.additional_fragments.len() >= 2,
    "manual pagination should fragment vertical writing mode documents"
  );
}

#[test]
fn manual_pagination_honors_break_before_in_horizontal_writing_mode() {
  let mut renderer = FastRender::builder().paginate(80.0, 0.0).build().unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          body { margin: 0; }
          .block { block-size: 40px; inline-size: 40px; }
          .break { break-before: page; }
        </style>
      </head>
      <body>
        <div class="block">A</div>
        <div class="block break">B</div>
        <div class="block">C</div>
      </body>
    </html>
  "#;

  let dom = renderer.parse_html(html).unwrap();
  let fragments = renderer.layout_document(&dom, 200, 200).unwrap();
  let roots = fragment_roots(&fragments);

  assert!(
    fragments.additional_fragments.len() >= 1,
    "forced break should create an additional fragmentainer root"
  );

  let first = roots.first().expect("first fragment root");
  assert!(
    fragment_contains_text(first, "A"),
    "first fragment should keep pre-break content"
  );
  assert!(
    !fragment_contains_text(first, "B"),
    "break-before: page must move following content to the next fragment"
  );
  assert!(
    !fragment_contains_text(first, "C"),
    "content after the forced break should not remain in the first fragment"
  );

  let later = roots
    .iter()
    .skip(1)
    .find(|root| fragment_contains_text(root, "B"))
    .expect("second fragment with forced-break content");
  assert!(
    fragment_contains_text(later, "C"),
    "content following the break should flow with the forced-break fragment"
  );
}

#[test]
fn manual_pagination_honors_break_before_in_vertical_writing_mode() {
  let mut renderer = FastRender::builder().paginate(80.0, 0.0).build().unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          html { writing-mode: vertical-rl; }
          body { margin: 0; }
          .block { block-size: 40px; inline-size: 40px; }
          .break { break-before: page; }
        </style>
      </head>
      <body>
        <div class="block">A</div>
        <div class="block break">B</div>
        <div class="block">C</div>
      </body>
    </html>
  "#;

  let dom = renderer.parse_html(html).unwrap();
  let fragments = renderer.layout_document(&dom, 200, 200).unwrap();
  let roots = fragment_roots(&fragments);

  assert!(
    fragments.additional_fragments.len() >= 1,
    "forced break should create an additional fragmentainer root even in vertical writing mode"
  );

  let first = roots.first().expect("first fragment root");
  assert!(
    fragment_contains_text(first, "A"),
    "first fragment should contain content before the forced break"
  );
  assert!(
    !fragment_contains_text(first, "B"),
    "forced break should prevent following content from appearing in the first fragment"
  );

  let later = roots
    .iter()
    .skip(1)
    .find(|root| fragment_contains_text(root, "B"))
    .expect("fragment containing forced-break content");
  assert!(
    fragment_contains_text(later, "C"),
    "content after the forced break should flow into the new fragmentainer"
  );
}
