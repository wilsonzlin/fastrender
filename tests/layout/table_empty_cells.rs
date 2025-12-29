use fastrender::api::FastRender;
use fastrender::style::color::Rgba;
use fastrender::style::display::Display;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode};

fn count_table_cells(node: &FragmentNode) -> usize {
  let mut count = 0;
  if node
    .style
    .as_ref()
    .map(|s| matches!(s.display, Display::TableCell))
    .unwrap_or(false)
  {
    count += 1;
  }
  for child in &node.children {
    count += count_table_cells(child);
  }
  count
}

fn fragment_contains_text(node: &FragmentNode, needle: &str) -> bool {
  match &node.content {
    FragmentContent::Text { text, .. } if text.contains(needle) => true,
    _ => node
      .children
      .iter()
      .any(|child| fragment_contains_text(child, needle)),
  }
}

fn find_cell_fragment_with_text<'a>(
  node: &'a FragmentNode,
  needle: &str,
) -> Option<&'a FragmentNode> {
  if fragment_contains_text(node, needle)
    && node
      .style
      .as_ref()
      .map(|s| matches!(s.display, Display::TableCell))
      .unwrap_or(false)
  {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_cell_fragment_with_text(child, needle) {
      return Some(found);
    }
  }
  None
}

#[test]
fn empty_cells_hide_layouts_many_empty_cells() {
  let empty_row: String = (0..32).map(|_| "<td></td>").collect();
  let html = format!(
    r#"
    <html>
      <head>
        <style>
          table {{ empty-cells: hide; border-collapse: separate; border-spacing: 1px; }}
          td {{ width: 8px; height: 8px; border: 1px solid black; }}
        </style>
      </head>
      <body>
        <table>
          <tr>{row}</tr>
          <tr>{row}</tr>
          <tr>{row}</tr>
        </table>
      </body>
    </html>
  "#,
    row = empty_row
  );

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(&html).unwrap();
  let tree = renderer.layout_document(&dom, 400, 300).unwrap();

  let mut cell_count = count_table_cells(&tree.root);
  for fragment in &tree.additional_fragments {
    cell_count += count_table_cells(fragment);
  }

  assert!(
    cell_count >= 96,
    "expected empty cells to produce fragments, saw {cell_count}"
  );
}

#[test]
fn non_empty_cells_keep_background_with_empty_cells_hide() {
  let html = r#"
    <html>
      <head>
        <style>
          table { empty-cells: hide; border-collapse: separate; }
          td { background: rgb(200, 20, 30); border: 1px solid black; }
        </style>
      </head>
      <body>
        <table>
          <tr><td></td><td>filled</td></tr>
        </table>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 200, 200).unwrap();

  let filled_cell = find_cell_fragment_with_text(&tree.root, "filled")
    .or_else(|| {
      tree
        .additional_fragments
        .iter()
        .find_map(|fragment| find_cell_fragment_with_text(fragment, "filled"))
    })
    .expect("non-empty cell fragment should be present");

  let style = filled_cell
    .style
    .as_ref()
    .expect("table cell fragments should carry styles");
  assert!(
    !style.background_color.is_transparent(),
    "non-empty cell should not be treated as visually empty when empty-cells: hide is used"
  );
  assert!(
    !matches!(style.background_color, Rgba::TRANSPARENT),
    "non-empty cell background should be preserved"
  );
}
