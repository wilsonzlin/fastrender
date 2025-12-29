use fastrender::api::FastRender;
use fastrender::geometry::Point;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};

fn pages<'a>(tree: &'a FragmentTree) -> Vec<&'a FragmentNode> {
  let mut roots = vec![&tree.root];
  roots.extend(tree.additional_fragments.iter());
  roots
}

#[derive(Debug, Clone)]
struct PositionedText {
  text: String,
  x: f32,
  y: f32,
}

fn collect_text_fragments(node: &FragmentNode, origin: Point, out: &mut Vec<PositionedText>) {
  let abs_x = origin.x + node.bounds.x();
  let abs_y = origin.y + node.bounds.y();
  if let FragmentContent::Text { text, .. } = &node.content {
    out.push(PositionedText {
      text: text.to_string(),
      x: abs_x,
      y: abs_y,
    });
  }
  for child in node.children.iter() {
    collect_text_fragments(child, Point::new(abs_x, abs_y), out);
  }
}

fn collected_text_compacted(node: &FragmentNode) -> String {
  let mut texts = Vec::new();
  collect_text_fragments(node, Point::ZERO, &mut texts);
  texts.sort_by(|a, b| {
    a.y
      .partial_cmp(&b.y)
      .unwrap_or(std::cmp::Ordering::Equal)
      .then(a.x.partial_cmp(&b.x).unwrap_or(std::cmp::Ordering::Equal))
  });
  let mut out = String::new();
  for t in texts {
    out.push_str(&t.text);
  }
  out.retain(|c| !c.is_whitespace());
  out
}

fn margin_texts(page: &FragmentNode) -> Vec<String> {
  page
    .children
    .iter()
    .skip(1)
    .map(collected_text_compacted)
    .collect()
}

#[test]
fn running_headers_follow_page_start() {
  let html = r#"
    <html>
      <head>
        <style>
          @page {
            size: 200px 200px;
            margin: 20px;
            @top-center { content: element(header, start); }
          }
          body { margin: 0; }
          h1 { position: running(header); margin: 0; font-size: 16px; }
          .spacer { height: 220px; }
        </style>
      </head>
      <body>
        <h1>First Title</h1>
        <div class="spacer"></div>
        <h1>Second Title</h1>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let page_roots = pages(&tree);

  assert!(page_roots.len() >= 2);
  let first_margin = margin_texts(page_roots[0]);
  let second_margin = margin_texts(page_roots[1]);
  assert!(
    first_margin.iter().any(|t| t.contains("FirstTitle")),
    "first page header should use first running element"
  );
  assert!(
    second_margin.iter().any(|t| t.contains("SecondTitle")),
    "second page header should use start-most running element on that page"
  );
}

#[test]
fn first_start_last_selection_differs() {
  let html = r#"
    <html>
      <head>
        <style>
          @page {
            size: 200px 200px;
            margin: 20px;
            @top-left { content: element(header); }
            @top-center { content: element(header, start); }
            @top-right { content: element(header, last); }
          }
          body { margin: 0; }
          h2 { position: running(header); margin: 0; font-size: 14px; }
          .fill { height: 210px; }
        </style>
      </head>
      <body>
        <h2>Alpha</h2>
        <div class="fill"></div>
        <h2>Beta</h2>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let page_roots = pages(&tree);
  assert!(page_roots.len() >= 2);
  let second_page = page_roots[1];
  let texts = margin_texts(second_page);

  assert!(
    texts.iter().any(|t| t.contains("Alpha")),
    "element(header) should keep the first occurrence in document"
  );
  assert!(
    texts.iter().any(|t| t.contains("Beta")),
    "start/last selections should pick the running element on the current page"
  );
}

#[test]
fn running_elements_and_strings_coexist() {
  let html = r#"
    <html>
      <head>
        <style>
          h1 { string-set: chapter content(); }
          h2 { position: running(header); }
          @page {
            size: 200px 220px;
            margin: 20px;
            @top-center { content: string(chapter) " - " element(header, last); }
          }
          body { margin: 0; }
          .push { height: 210px; }
        </style>
      </head>
      <body>
        <h1>Chapter One</h1>
        <h2>Intro Header</h2>
        <div class="push"></div>
        <h2>Next Header</h2>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let page_roots = pages(&tree);
  assert!(page_roots.len() >= 2);

  for page in page_roots {
    let texts = margin_texts(page);
    assert!(
      texts.iter().any(|t| t.contains("ChapterOne")),
      "string-set value should be available alongside running elements"
    );
    assert!(
      texts.iter().any(|t| t.contains("Header")),
      "running element content should render in the same margin box"
    );
  }
}
