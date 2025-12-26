use fastrender::api::{FastRender, LayoutDocumentOptions, PageStacking};
use fastrender::Rgba;
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

fn find_fragment_by_background<'a>(node: &'a FragmentNode, color: Rgba) -> Option<&'a FragmentNode> {
  if node.content.is_block()
    && node
      .style
      .as_ref()
      .is_some_and(|style| style.background_color == color)
  {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_fragment_by_background(child, color) {
      return Some(found);
    }
  }
  None
}

fn assert_bounds_close(bounds: &fastrender::geometry::Rect, expected: (f32, f32, f32, f32)) {
  let (x, y, width, height) = expected;
  let epsilon = 0.01;
  assert!(
    (bounds.x() - x).abs() < epsilon,
    "x mismatch: actual {}, expected {}",
    bounds.x(),
    x
  );
  assert!(
    (bounds.y() - y).abs() < epsilon,
    "y mismatch: actual {}, expected {}",
    bounds.y(),
    y
  );
  assert!(
    (bounds.width() - width).abs() < epsilon,
    "width mismatch: actual {}, expected {}",
    bounds.width(),
    width
  );
  assert!(
    (bounds.height() - height).abs() < epsilon,
    "height mismatch: actual {}, expected {}",
    bounds.height(),
    height
  );
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

fn collect_line_widths(node: &FragmentNode, out: &mut Vec<f32>) {
  if let FragmentContent::Line { .. } = node.content {
    out.push(node.bounds.width());
  }
  for child in &node.children {
    collect_line_widths(child, out);
  }
}

#[test]
fn line_wrapping_respects_page_side_widths() {
  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 200px 140px; margin: 10px; }
          @page :right { margin-left: 10px; margin-right: 10px; }
          @page :left { margin-left: 40px; margin-right: 10px; }
          body { margin: 0; }
          p { margin: 0; font-size: 16px; line-height: 16px; }
        </style>
      </head>
      <body>
        <p>
          This is a very long line of text that should wrap across multiple lines and pages so that
          we can verify pagination reflows content differently on right and left pages when margins
          change between them. The content intentionally repeats to ensure it spans at least two
          pages worth of text.
          This is a very long line of text that should wrap across multiple lines and pages so that
          we can verify pagination reflows content differently on right and left pages when margins
          change between them. The content intentionally repeats to ensure it spans at least two
          pages worth of text.
          This is a very long line of text that should wrap across multiple lines and pages so that
          we can verify pagination reflows content differently on right and left pages when margins
          change between them. The content intentionally repeats to ensure it spans at least two
          pages worth of text.
        </p>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let page_roots = pages(&tree);

  assert!(
    page_roots.len() >= 2,
    "expected at least two pages for wrapping test"
  );

  let mut first_lines = Vec::new();
  collect_line_widths(page_roots[0], &mut first_lines);
  let mut second_lines = Vec::new();
  collect_line_widths(page_roots[1], &mut second_lines);

  assert!(!first_lines.is_empty());
  assert!(!second_lines.is_empty());

  let first_max = first_lines.iter().cloned().fold(0.0, f32::max);
  let second_max = second_lines.iter().cloned().fold(0.0, f32::max);

  assert!(
    first_max > second_max + 5.0,
    "expected wider right page lines ({first_max}) than left page ({second_max})"
  );
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
fn margin_box_uses_custom_counter_style() {
  let html = r#"
    <html>
      <head>
        <style>
          @counter-style alpha2 { system: fixed 1; symbols: "A" "B" "C"; }
          @page {
            size: 200px 100px;
            margin: 10px;
            @bottom-center { content: counter(page, alpha2); }
          }
        </style>
      </head>
      <body>
        <div style="height: 120px"></div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let page_roots = pages(&tree);

  assert!(
    page_roots.len() >= 2,
    "expected at least two pages for page counter test"
  );

  let first = page_roots[0];
  let second = page_roots[1];

  assert!(
    find_text(first, "A").is_some(),
    "page 1 should render counter(page) with the first symbol"
  );
  assert!(
    find_text(second, "B").is_some(),
    "page 2 should render counter(page) with the second symbol"
  );
}

#[test]
fn margin_box_text_is_shaped() {
  let html = r#"
    <html>
      <head>
        <style>
          @page {
            size: 200px 200px;
            margin: 20px;
            @top-center { content: "Header"; }
          }
        </style>
      </head>
      <body>
        <div>Content</div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let page_roots = pages(&tree);
  let page = page_roots[0];

  let header = find_text(page, "Header").expect("header margin box fragment");

  assert!(matches!(
    header.content,
    FragmentContent::Text {
      shaped: Some(ref runs),
      ..
    } if !runs.is_empty()
  ));
}

#[test]
fn margin_box_bounds_cover_all_areas() {
  let html = r#"
    <html>
      <head>
        <style>
          @page {
            size: 200px 200px;
            margin: 10px;
            @top-left-corner { background: rgb(255, 0, 0); }
            @top-left { background: rgb(255, 32, 0); }
            @top-center { background: rgb(255, 64, 0); }
            @top-right { background: rgb(255, 96, 0); }
            @top-right-corner { background: rgb(255, 128, 0); }
            @right-top { background: rgb(255, 160, 0); }
            @right-middle { background: rgb(255, 192, 0); }
            @right-bottom { background: rgb(255, 224, 0); }
            @bottom-right-corner { background: rgb(0, 255, 0); }
            @bottom-right { background: rgb(0, 255, 32); }
            @bottom-center { background: rgb(0, 255, 64); }
            @bottom-left { background: rgb(0, 255, 96); }
            @bottom-left-corner { background: rgb(0, 255, 128); }
            @left-bottom { background: rgb(0, 0, 255); }
            @left-middle { background: rgb(32, 0, 255); }
            @left-top { background: rgb(64, 0, 255); }
          }
        </style>
      </head>
      <body></body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let page = *pages(&tree).first().expect("at least one page");

  let expectations = vec![
    (Rgba::rgb(255, 0, 0), (0.0, 0.0, 10.0, 10.0)),
    (Rgba::rgb(255, 32, 0), (10.0, 0.0, 60.0, 10.0)),
    (Rgba::rgb(255, 64, 0), (70.0, 0.0, 60.0, 10.0)),
    (Rgba::rgb(255, 96, 0), (130.0, 0.0, 60.0, 10.0)),
    (Rgba::rgb(255, 128, 0), (190.0, 0.0, 10.0, 10.0)),
    (Rgba::rgb(255, 160, 0), (190.0, 10.0, 10.0, 60.0)),
    (Rgba::rgb(255, 192, 0), (190.0, 70.0, 10.0, 60.0)),
    (Rgba::rgb(255, 224, 0), (190.0, 130.0, 10.0, 60.0)),
    (Rgba::rgb(0, 255, 0), (190.0, 190.0, 10.0, 10.0)),
    (Rgba::rgb(0, 255, 32), (130.0, 190.0, 60.0, 10.0)),
    (Rgba::rgb(0, 255, 64), (70.0, 190.0, 60.0, 10.0)),
    (Rgba::rgb(0, 255, 96), (10.0, 190.0, 60.0, 10.0)),
    (Rgba::rgb(0, 255, 128), (0.0, 190.0, 10.0, 10.0)),
    (Rgba::rgb(0, 0, 255), (0.0, 130.0, 10.0, 60.0)),
    (Rgba::rgb(32, 0, 255), (0.0, 70.0, 10.0, 60.0)),
    (Rgba::rgb(64, 0, 255), (0.0, 10.0, 10.0, 60.0)),
  ];

  for (color, expected_bounds) in expectations {
    let fragment = find_fragment_by_background(page, color)
      .unwrap_or_else(|| panic!("missing margin box for color {:?}", color));
    assert_bounds_close(&fragment.bounds, expected_bounds);
  }
}

#[test]
fn margin_box_page_counters_page_and_pages() {
  let html = r#"
    <html>
      <head>
        <style>
          @page {
            size: 200px 100px;
            margin: 10px;
            @bottom-center { content: "Page " counter(page) " / " counter(pages); }
          }
        </style>
      </head>
      <body>
        <div style="height: 150px"></div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let page_roots = pages(&tree);
  assert!(page_roots.len() >= 2);

  let page_count = page_roots.len();
  for (idx, page) in page_roots.iter().enumerate() {
    let expected = format!("Page {} / {}", idx + 1, page_count);
    find_text(page, &expected)
      .unwrap_or_else(|| panic!("missing page counter text on page {}", idx + 1));
  }
}

#[test]
fn paginated_pages_are_stacked_vertically() {
  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 100px 120px; margin: 0; }
        </style>
      </head>
      <body>
        <div style="height: 250px"></div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 200, 200).unwrap();
  let page_roots = pages(&tree);

  assert!(page_roots.len() >= 2);
  assert!(page_roots[1].bounds.y() > page_roots[0].bounds.y());
  assert!(
    page_roots[1].bounds.y() - page_roots[0].bounds.y() >= page_roots[0].bounds.height() - 0.1
  );
}

#[test]
fn page_stacking_can_be_disabled() {
  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 100px 120px; margin: 0; }
        </style>
      </head>
      <body>
        <div style="height: 250px"></div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let options = LayoutDocumentOptions::new().with_page_stacking(PageStacking::Untranslated);
  let tree = renderer
    .layout_document_with_options(&dom, 200, 200, options)
    .unwrap();
  let page_roots = pages(&tree);

  assert!(page_roots.len() >= 2);
  assert!((page_roots[0].bounds.y() - page_roots[1].bounds.y()).abs() < 0.01);
}

fn find_text_position(node: &FragmentNode, needle: &str, origin: (f32, f32)) -> Option<(f32, f32)> {
  let current = (origin.0 + node.bounds.x(), origin.1 + node.bounds.y());
  if let FragmentContent::Text { text, .. } = &node.content {
    if text.contains(needle) {
      return Some(current);
    }
  }
  for child in &node.children {
    if let Some(pos) = find_text_position(child, needle, current) {
      return Some(pos);
    }
  }
  None
}

#[test]
fn fixed_headers_repeat_per_page() {
  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 200px 200px; margin: 0; }
          body { margin: 0; }
          .header { position: fixed; top: 0; left: 0; height: 20px; }
          .spacer { height: 500px; }
        </style>
      </head>
      <body>
        <div class="header">FixedHeader</div>
        <div class="spacer"></div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 200, 200).unwrap();
  let page_roots = pages(&tree);

  assert!(page_roots.len() >= 2);

  let first_header_y = find_text(page_roots[0], "FixedHeader")
    .expect("header on first page")
    .bounds
    .y();

  for (index, page) in page_roots.iter().enumerate() {
    let header = find_text(page, "FixedHeader")
      .unwrap_or_else(|| panic!("missing header on page {}", index + 1));
    assert!(
      (header.bounds.y() - first_header_y).abs() < 0.1,
      "header should be consistently positioned across pages"
    );
  }
}

#[test]
fn multicol_columns_continue_across_pages() {
  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 200px 200px; margin: 0; }
          body { margin: 0; }
          .multi { column-count: 2; column-gap: 0; }
          .section { height: 150px; }
        </style>
      </head>
      <body>
        <div class="multi">
          <div class="section">One</div>
          <div class="section">Two</div>
          <div class="section">Three</div>
        </div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let options = LayoutDocumentOptions::new().with_page_stacking(PageStacking::Untranslated);
  let tree = renderer
    .layout_document_with_options(&dom, 400, 400, options)
    .unwrap();
  let page_roots = pages(&tree);

  let first = page_roots[0];
  let second = page_roots[1];

  let pos_one = find_text_position(first, "One", (0.0, 0.0)).expect("first column");
  let pos_two = find_text_position(first, "Two", (0.0, 0.0)).expect("second column");
  assert!(find_text_position(first, "Three", (0.0, 0.0)).is_none());

  assert!(
    pos_two.0 > pos_one.0,
    "second column should be to the right"
  );
  assert!(
    pos_one.1 < 200.0 && pos_two.1 < 200.0,
    "page 1 content fits height"
  );

  let pos_three = find_text_position(second, "Three", (0.0, 0.0)).expect("continued content");
  assert!(
    pos_three.1 < 20.0,
    "next column set starts at top of next page"
  );
  assert!(find_text_position(second, "One", (0.0, 0.0)).is_none());
}

#[test]
fn page_break_before_forces_new_page() {
  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 200px 200px; margin: 0; }
          body { margin: 0; }
          .block { height: 80px; }
          #forced { page-break-before: always; }
        </style>
      </head>
      <body>
        <div class="block">Before</div>
        <div id="forced" class="block">Forced break</div>
        <div class="block">After</div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();
  let page_roots = pages(&tree);

  assert!(
    page_roots.len() >= 2,
    "expected pagination to create multiple pages"
  );

  let first = page_roots[0];
  let second = page_roots[1];

  assert!(
    find_text(first, "Before").is_some(),
    "first page should contain the preceding content"
  );
  assert!(
    find_text(first, "Forced break").is_none(),
    "forced break content must start a new page"
  );
  assert!(
    find_text(first, "After").is_none(),
    "content after the break should not remain on the first page"
  );

  assert!(
    find_text(second, "Forced break").is_some(),
    "forced break content should move to the next page"
  );
  assert!(
    find_text(second, "After").is_some(),
    "following content should flow after the forced page break"
  );
}

#[test]
fn break_before_page_forces_new_page() {
  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 100px 100px; margin: 0; }
          div { height: 40px; }
          .page-break { break-before: page; }
        </style>
      </head>
      <body>
        <div>A</div>
        <div class="page-break">B</div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 200, 200).unwrap();
  let page_roots = pages(&tree);

  assert!(find_text(page_roots[0], "A").is_some());
  assert!(find_text(page_roots[0], "B").is_none());
  assert!(page_roots
    .iter()
    .skip(1)
    .any(|page| find_text(page, "B").is_some()));
}

#[test]
fn break_before_column_does_not_force_page_without_columns() {
  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 100px 100px; margin: 0; }
          div { height: 40px; }
          .column-break { break-before: column; }
        </style>
      </head>
      <body>
        <div>A</div>
        <div class="column-break">B</div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 200, 200).unwrap();
  let page_roots = pages(&tree);

  assert_eq!(page_roots.len(), 1);
  let page = page_roots[0];
  assert!(find_text(page, "A").is_some());
  assert!(find_text(page, "B").is_some());
}
