use fastrender::api::FastRender;
use fastrender::tree::box_tree::ReplacedType;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};

fn pages<'a>(tree: &'a FragmentTree) -> Vec<&'a FragmentNode> {
  let mut roots = vec![&tree.root];
  roots.extend(tree.additional_fragments.iter());
  roots
}

fn find_text<'a>(node: &'a FragmentNode, needle: &str) -> Option<&'a FragmentNode> {
  if let FragmentContent::Text { text, .. } = &node.content {
    if text.contains(needle) {
      return Some(node);
    }
  }
  for child in &node.children {
    if let Some(found) = find_text(child, needle) {
      return Some(found);
    }
  }
  None
}

fn find_replaced_image<'a>(node: &'a FragmentNode) -> Option<&'a FragmentNode> {
  if let FragmentContent::Replaced { replaced_type, .. } = &node.content {
    if matches!(replaced_type, ReplacedType::Image { .. }) {
      return Some(node);
    }
  }

  for child in &node.children {
    if let Some(found) = find_replaced_image(child) {
      return Some(found);
    }
  }

  None
}

#[test]
fn page_rule_sets_size_and_margins() {
  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 200px 400px; margin: 20px 30px 40px 50px; }
        </style>
      </head>
      <body>
        <div style="height: 700px"></div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 800, 1000).unwrap();
  let page_roots = pages(&tree);

  assert!(page_roots.len() >= 2);
  assert!((page_roots[0].bounds.width() - 200.0).abs() < 0.1);
  assert!((page_roots[0].bounds.height() - 400.0).abs() < 0.1);

  let content = page_roots[0].children.first().expect("page content");
  assert!((content.bounds.x() - 50.0).abs() < 0.1);
  assert!((content.bounds.y() - 20.0).abs() < 0.1);
  assert!((content.bounds.height() - 340.0).abs() < 0.1);
}

#[test]
fn page_rule_left_and_right_offsets_differ() {
  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 200px 300px; margin-top: 0; margin-bottom: 0; }
          @page :right { margin-left: 10px; margin-right: 30px; }
          @page :left { margin-left: 40px; margin-right: 5px; }
        </style>
      </head>
      <body>
        <div style="height: 600px"></div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let page_roots = pages(&tree);

  assert!(page_roots.len() >= 2);
  let first = page_roots[0].children.first().unwrap();
  let second = page_roots[1].children.first().unwrap();

  assert!((first.bounds.x() - 10.0).abs() < 0.1);
  assert!((second.bounds.x() - 40.0).abs() < 0.1);
  assert!((first.bounds.width() - 160.0).abs() < 0.1);
  assert!((second.bounds.width() - 155.0).abs() < 0.1);
}

#[test]
fn named_pages_change_page_size() {
  let html = r#"
    <html>
      <head>
        <style>
          @page chapter { size: 300px 200px; margin: 10px; }
          @page { size: 200px 200px; margin: 10px; }
          div { page: chapter; }
        </style>
      </head>
      <body>
        <div style="height: 260px"></div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let page_roots = pages(&tree);

  assert!((page_roots[0].bounds.width() - 300.0).abs() < 0.1);
  let content = page_roots[0].children.first().unwrap();
  assert!((content.bounds.width() - 280.0).abs() < 0.1);
}

#[test]
fn margin_box_content_is_positioned_in_margins() {
  let html = r#"
    <html>
      <head>
        <style>
          @page {
            size: 200px 200px;
            margin: 20px;
            @top-center { content: "Header"; }
            @bottom-center { content: "Footer"; }
          }
        </style>
      </head>
      <body>
        <div style="height: 50px"></div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let page_roots = pages(&tree);
  let page = page_roots[0];

  let header = find_text(page, "Header").expect("header margin box");
  let footer = find_text(page, "Footer").expect("footer margin box");
  let content = page.children.first().expect("content");

  assert!(header.bounds.y() < content.bounds.y());
  assert!(footer.bounds.y() > content.bounds.y());
}

#[test]
fn margin_box_url_content_creates_replaced_fragment() {
  let html = r#"
    <html>
      <head>
        <style>
          @page {
            size: 200px 120px;
            margin: 10px;
            @top-center { content: url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMB/6XWcJAAAAAASUVORK5CYII="); }
          }
        </style>
      </head>
      <body>
        <div>content</div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let first_page = pages(&tree)[0];

  assert!(
    find_replaced_image(first_page).is_some(),
    "expected a replaced image fragment in margin boxes"
  );
}
