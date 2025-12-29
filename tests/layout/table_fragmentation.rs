use fastrender::api::FastRender;
use fastrender::style::types::WritingMode;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};

fn pages(tree: &FragmentTree) -> Vec<&FragmentNode> {
  let mut roots = vec![&tree.root];
  roots.extend(tree.additional_fragments.iter());
  roots
}

#[derive(Debug, Clone)]
struct TextFragment {
  text: String,
  x: f32,
  y: f32,
}

fn collect_text_fragments(node: &FragmentNode, out: &mut Vec<TextFragment>) {
  collect_text_fragments_with_origin(node, (0.0, 0.0), out);
}

fn collect_text_fragments_with_origin(
  node: &FragmentNode,
  origin: (f32, f32),
  out: &mut Vec<TextFragment>,
) {
  let abs_x = origin.0 + node.bounds.x();
  let abs_y = origin.1 + node.bounds.y();
  if let FragmentContent::Text { text, .. } = &node.content {
    out.push(TextFragment {
      text: text.to_string(),
      x: abs_x,
      y: abs_y,
    });
  }
  for child in node.children.iter() {
    collect_text_fragments_with_origin(child, (abs_x, abs_y), out);
  }
}

fn inline_position(fragment: &TextFragment, writing_mode: WritingMode) -> f32 {
  match writing_mode {
    WritingMode::HorizontalTb => fragment.x,
    WritingMode::VerticalRl
    | WritingMode::VerticalLr
    | WritingMode::SidewaysRl
    | WritingMode::SidewaysLr => fragment.y,
  }
}

#[test]
fn table_headers_repeat_across_pages() {
  let body_rows: String = (1..=12)
    .map(|i| format!(r#"<tr><td>Row {i}</td></tr>"#))
    .collect();
  let html = format!(
    r#"
    <html>
      <head>
        <style>
          @page {{ size: 200px 120px; margin: 0; }}
          table {{ border-collapse: collapse; width: 100%; }}
          td, th {{ padding: 2px; height: 30px; }}
        </style>
      </head>
      <body>
        <table>
          <thead><tr><th>Header</th></tr></thead>
          <tbody>{body_rows}</tbody>
        </table>
      </body>
    </html>
  "#
  );

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(&html).unwrap();
  let tree = renderer.layout_document(&dom, 200, 300).unwrap();
  let page_roots = pages(&tree);

  assert!(page_roots.len() > 1, "table should span multiple pages");

  let mut seen_rows = Vec::new();
  for page in &page_roots {
    let mut texts = Vec::new();
    collect_text_fragments(page, &mut texts);
    assert!(
      texts.iter().any(|t| t.text.contains("Header")),
      "header should appear on every page"
    );
    for t in texts {
      if let Some(num) = t.text.strip_prefix("Row ") {
        if let Ok(n) = num.parse::<usize>() {
          seen_rows.push(n);
        }
      }
    }
  }

  let expected: Vec<usize> = (1..=12).collect();
  assert_eq!(seen_rows, expected);
}

#[test]
fn table_headers_repeat_in_multicol() {
  let body_rows: String = (1..=8)
    .map(|i| format!(r#"<tr><td>Row {i}</td></tr>"#))
    .collect();
  let html = format!(
    r#"
    <html>
      <head>
        <style>
          div.columns {{
            column-count: 2;
            column-gap: 20px;
            width: 260px;
          }}
          table {{ width: 100%; border-collapse: collapse; }}
          td, th {{ height: 32px; padding: 2px; }}
        </style>
      </head>
      <body>
        <div class="columns">
          <table>
            <thead><tr><th>Header</th></tr></thead>
            <tbody>{body_rows}</tbody>
          </table>
        </div>
      </body>
    </html>
  "#
  );

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(&html).unwrap();
  let tree = renderer.layout_document(&dom, 320, 400).unwrap();

  let mut texts = Vec::new();
  collect_text_fragments(&tree.root, &mut texts);

  let header_positions: Vec<f32> = texts
    .iter()
    .filter(|t| t.text.contains("Header"))
    .map(|t| t.x)
    .collect();
  assert!(
    header_positions.len() >= 2,
    "header should repeat for each column fragment"
  );
  let min_x = header_positions
    .iter()
    .copied()
    .fold(f32::INFINITY, f32::min);
  let max_x = header_positions
    .iter()
    .copied()
    .fold(f32::NEG_INFINITY, f32::max);
  assert!(
    (max_x - min_x) > 10.0,
    "headers should appear in distinct columns"
  );

  let midpoint = (min_x + max_x) / 2.0;
  let mut first_col_rows = Vec::new();
  let mut second_col_rows = Vec::new();
  for t in texts {
    if let Some(num) = t.text.strip_prefix("Row ") {
      if let Ok(n) = num.parse::<usize>() {
        if t.x < midpoint {
          first_col_rows.push(n);
        } else {
          second_col_rows.push(n);
        }
      }
    }
  }

  first_col_rows.extend(second_col_rows.iter().copied());
  first_col_rows.sort_unstable();
  let expected: Vec<usize> = (1..=8).collect();
  assert_eq!(first_col_rows, expected);
  assert!(
    !second_col_rows.is_empty(),
    "rows should flow into the second column"
  );
}

#[test]
fn table_headers_repeat_across_pages_vertical_writing() {
  let body_rows: String = (1..=12)
    .map(|i| format!(r#"<tr><td>Row {i}</td></tr>"#))
    .collect();
  let html = format!(
    r#"
    <html>
      <head>
        <style>
          html {{ writing-mode: vertical-rl; }}
          @page {{ size: 200px 120px; margin: 0; }}
          table {{ border-collapse: collapse; width: 100%; }}
          td, th {{ padding: 2px; height: 30px; }}
        </style>
      </head>
      <body>
        <table>
          <thead><tr><th>Header</th></tr></thead>
          <tbody>{body_rows}</tbody>
        </table>
      </body>
    </html>
  "#
  );

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(&html).unwrap();
  let tree = renderer.layout_document(&dom, 200, 300).unwrap();
  let page_roots = pages(&tree);

  assert!(page_roots.len() > 1, "table should span multiple pages");

  let mut seen_rows = Vec::new();
  for page in &page_roots {
    let mut texts = Vec::new();
    collect_text_fragments(page, &mut texts);
    assert!(
      texts.iter().any(|t| t.text.contains("Header")),
      "header should appear on every page"
    );
    for t in texts {
      if let Some(num) = t.text.strip_prefix("Row ") {
        if let Ok(n) = num.parse::<usize>() {
          seen_rows.push(n);
        }
      }
    }
  }

  let expected: Vec<usize> = (1..=12).collect();
  assert_eq!(seen_rows, expected);
}

#[test]
fn table_headers_repeat_in_multicol_vertical_writing() {
  let body_rows: String = (1..=8)
    .map(|i| format!(r#"<tr><td>Row {i}</td></tr>"#))
    .collect();
  let writing_mode = WritingMode::VerticalRl;
  let html = format!(
    r#"
    <html>
      <head>
        <style>
          div.columns {{
            writing-mode: vertical-rl;
            column-count: 2;
            column-gap: 20px;
            width: 260px;
          }}
          table {{ width: 100%; border-collapse: collapse; }}
          td, th {{ height: 32px; padding: 2px; }}
        </style>
      </head>
      <body>
        <div class="columns">
          <table>
            <thead><tr><th>Header</th></tr></thead>
            <tbody>{body_rows}</tbody>
          </table>
        </div>
      </body>
    </html>
  "#
  );

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(&html).unwrap();
  let tree = renderer.layout_document(&dom, 320, 400).unwrap();

  let mut texts = Vec::new();
  collect_text_fragments(&tree.root, &mut texts);

  let header_positions: Vec<f32> = texts
    .iter()
    .filter(|t| t.text.contains("Header"))
    .map(|t| inline_position(t, writing_mode))
    .collect();
  assert!(
    header_positions.len() >= 2,
    "header should repeat for each column fragment"
  );
  let min_inline = header_positions
    .iter()
    .copied()
    .fold(f32::INFINITY, f32::min);
  let max_inline = header_positions
    .iter()
    .copied()
    .fold(f32::NEG_INFINITY, f32::max);
  assert!(
    (max_inline - min_inline) > 10.0,
    "headers should appear in distinct columns"
  );

  let midpoint = (min_inline + max_inline) / 2.0;
  let mut first_col_rows = Vec::new();
  let mut second_col_rows = Vec::new();
  for t in texts {
    if let Some(num) = t.text.strip_prefix("Row ") {
      if let Ok(n) = num.parse::<usize>() {
        let pos = inline_position(&t, writing_mode);
        if pos < midpoint {
          first_col_rows.push(n);
        } else {
          second_col_rows.push(n);
        }
      }
    }
  }

  first_col_rows.extend(second_col_rows.iter().copied());
  first_col_rows.sort_unstable();
  let expected: Vec<usize> = (1..=8).collect();
  assert_eq!(first_col_rows, expected);
  assert!(
    !second_col_rows.is_empty(),
    "rows should flow into the second column"
  );
}
