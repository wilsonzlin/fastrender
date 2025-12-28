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
fn builder_columns_layout_into_multiple_roots() {
  let mut renderer = FastRender::builder()
    .columns(2, 24.0, 180.0)
    .build()
    .unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          body { margin: 0; }
          .tall { height: 300px; }
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
    "column fragmentation should create multiple root fragments"
  );
  assert!(
    fragments
      .additional_fragments
      .iter()
      .any(|f| f.bounds.x() > 0.0),
    "columns should be positioned horizontally"
  );
}

#[test]
fn break_before_column_moves_content_to_next_manual_column() {
  let mut renderer = FastRender::builder()
    .columns(2, 10.0, 80.0)
    .build()
    .unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          body { margin: 0; }
          .item { block-size: 40px; inline-size: 40px; }
          .second { break-before: column; }
        </style>
      </head>
      <body>
        <div class="item first">First</div>
        <div class="item second">Second</div>
      </body>
    </html>
  "#;

  let dom = renderer.parse_html(html).unwrap();
  let fragments = renderer.layout_document(&dom, 200, 200).unwrap();
  let roots = fragment_roots(&fragments);

  assert!(
    fragments.additional_fragments.len() >= 1,
    "forced column break should start a new fragmentainer column"
  );

  let first_column = roots
    .iter()
    .find(|root| fragment_contains_text(root, "First"))
    .expect("first column root");
  let second_column = roots
    .iter()
    .find(|root| fragment_contains_text(root, "Second"))
    .expect("second column root");

  let expected_offset = first_column.bounds.width() + 10.0;
  assert!(
    second_column.bounds.x() > first_column.bounds.x(),
    "break-before: column should move content to a later column"
  );
  assert!(
    (second_column.bounds.x() - expected_offset).abs() < 0.1,
    "second column should start after the column width plus gap (expected ~{}, got {})",
    expected_offset,
    second_column.bounds.x()
  );
  assert!(
    !fragment_contains_text(first_column, "Second"),
    "second block should not remain in the first column"
  );
}
