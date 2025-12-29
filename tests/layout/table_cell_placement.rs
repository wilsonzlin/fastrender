use std::collections::HashMap;

use fastrender::api::FastRender;
use fastrender::geometry::Rect;
use fastrender::style::display::Display;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode};

#[derive(Debug, Clone)]
struct CellInfo {
  rect: Rect,
  baseline: Option<f32>,
}

fn find_table<'a>(node: &'a FragmentNode) -> Option<&'a FragmentNode> {
  if matches!(node.style.as_ref().map(|s| s.display), Some(Display::Table)) {
    return Some(node);
  }
  node.children.iter().find_map(find_table)
}

fn collect_text(node: &FragmentNode, out: &mut String) {
  if let FragmentContent::Text { text, .. } = &node.content {
    out.push_str(text);
  }
  for child in node.children.iter() {
    collect_text(child, out);
  }
}

fn collect_cells(node: &FragmentNode, origin: (f32, f32), cells: &mut HashMap<char, CellInfo>) {
  let pos = (origin.0 + node.bounds.x(), origin.1 + node.bounds.y());
  if matches!(
    node.style.as_ref().map(|s| s.display),
    Some(Display::TableCell)
  ) {
    let mut text = String::new();
    collect_text(node, &mut text);
    let label = text
      .trim()
      .chars()
      .find(|c| c.is_ascii_alphabetic())
      .unwrap();
    let rect = Rect::from_xywh(pos.0, pos.1, node.bounds.width(), node.bounds.height());
    cells.insert(
      label,
      CellInfo {
        rect,
        baseline: node.baseline,
      },
    );
  }
  for child in node.children.iter() {
    collect_cells(child, pos, cells);
  }
}

#[test]
fn table_cell_offsets_and_spans_use_cached_metrics() {
  let html = r#"
    <html>
      <head>
        <style>
          body { margin: 0; }
          table { border-collapse: separate; border-spacing: 10px 6px; table-layout: fixed; }
          col.col1 { width: 40px; }
          col.col2 { width: 60px; }
          col.col3 { width: 50px; }
          td { padding: 0; margin: 0; border: 0; font-size: 12px; line-height: 12px; }
        </style>
      </head>
      <body>
        <table>
          <col class=\"col1\" />
          <col class=\"col2\" />
          <col class=\"col3\" />
          <tr>
            <td style=\"height: 14px;\">A</td>
            <td style=\"height: 14px;\">B</td>
            <td style=\"height: 14px;\">C</td>
          </tr>
          <tr>
            <td rowspan=\"2\" style=\"height: 22px;\">D</td>
            <td colspan=\"2\" style=\"height: 16px;\">E</td>
          </tr>
          <tr>
            <td style=\"height: 12px;\">F</td>
            <td style=\"height: 18px;\">G</td>
          </tr>
        </table>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 400).unwrap();

  let table = find_table(&tree.root).expect("table fragment present");
  let mut cells = HashMap::new();
  collect_cells(table, (0.0, 0.0), &mut cells);

  assert_eq!(cells.len(), 7, "expected to collect all table cells");

  let a = cells.get(&'A').unwrap();
  let b = cells.get(&'B').unwrap();
  let c = cells.get(&'C').unwrap();
  let d = cells.get(&'D').unwrap();
  let e = cells.get(&'E').unwrap();
  let f = cells.get(&'F').unwrap();

  let spacing_x1 = b.rect.x() - (a.rect.x() + a.rect.width());
  let spacing_x2 = c.rect.x() - (b.rect.x() + b.rect.width());
  assert!(
    (spacing_x1 - 10.0).abs() < 0.1 && (spacing_x1 - spacing_x2).abs() < 0.05,
    "horizontal spacing should match border-spacing (got {spacing_x1} and {spacing_x2})"
  );

  let expected_e_width = (c.rect.x() + c.rect.width()) - b.rect.x();
  assert!(
    (e.rect.width() - expected_e_width).abs() < 0.05,
    "colspan cell width should span adjacent columns (expected {expected_e_width}, got {})",
    e.rect.width()
  );

  let spacing_y1 = e.rect.y() - (a.rect.y() + a.rect.height());
  let spacing_y2 = f.rect.y() - (e.rect.y() + e.rect.height());
  assert!(
    (spacing_y1 - 6.0).abs() < 0.1 && (spacing_y1 - spacing_y2).abs() < 0.05,
    "vertical spacing should match border-spacing (got {spacing_y1} and {spacing_y2})"
  );

  let expected_d_height = (f.rect.y() + f.rect.height()) - d.rect.y();
  assert!(
    (d.rect.height() - expected_d_height).abs() < 0.05,
    "rowspan height should include intermediate spacing (expected {expected_d_height}, got {})",
    d.rect.height()
  );

  let table_baseline = table.baseline.expect("table baseline");
  let a_baseline = a.baseline.expect("cell baseline");
  let a_baseline_abs = a.rect.y() + a_baseline;
  assert!(
    (table_baseline - a_baseline_abs).abs() < 0.1,
    "table baseline should align with first baseline cell (table {table_baseline}, cell {a_baseline_abs})"
  );
}
