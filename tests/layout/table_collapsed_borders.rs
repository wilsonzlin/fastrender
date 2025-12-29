use fastrender::api::FastRender;
use fastrender::paint::display_list::DisplayItem;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::tree::fragment_tree::{FragmentNode, TableCollapsedBorders};

fn count_fragments(node: &FragmentNode) -> usize {
  1 + node.children.iter().map(count_fragments).sum::<usize>()
}

fn find_table_borders(node: &FragmentNode) -> Option<&TableCollapsedBorders> {
  if let Some(borders) = node.table_borders.as_ref() {
    return Some(borders.as_ref());
  }
  node.children.iter().find_map(find_table_borders)
}

fn build_table_html(rows: usize, cols: usize, collapse: bool) -> String {
  let cells = "<td></td>".repeat(cols);
  let body: String = (0..rows).map(|_| format!("<tr>{cells}</tr>")).collect();
  let collapse_rule = if collapse {
    "collapse;"
  } else {
    "separate; border-spacing: 0;"
  };

  format!(
    r#"
    <html>
      <head>
        <style>
          table {{
            border-collapse: {collapse_rule}
          }}
          td {{
            border: 2px solid black;
            width: 10px;
            height: 10px;
            padding: 0;
            margin: 0;
          }}
        </style>
      </head>
      <body>
        <table>{body}</table>
      </body>
    </html>
  "#
  )
}

#[test]
fn collapsed_table_uses_compact_borders() {
  const ROWS: usize = 20;
  const COLS: usize = 20;

  let collapsed_html = build_table_html(ROWS, COLS, true);
  let separate_html = build_table_html(ROWS, COLS, false);

  let mut collapsed = FastRender::new().unwrap();
  let collapsed_tree = collapsed
    .layout_document(&collapsed.parse_html(&collapsed_html).unwrap(), 800, 800)
    .unwrap();

  let mut separate = FastRender::new().unwrap();
  let separate_tree = separate
    .layout_document(&separate.parse_html(&separate_html).unwrap(), 800, 800)
    .unwrap();

  let collapsed_fragments = count_fragments(&collapsed_tree.root);
  let separate_fragments = count_fragments(&separate_tree.root);
  assert!(
    collapsed_fragments <= separate_fragments + 50,
    "collapsed borders should not inflate fragment count (collapsed={collapsed_fragments}, separate={separate_fragments})"
  );
  assert!(
    collapsed_fragments < ROWS * COLS * 5,
    "collapsed border fragment count should stay near cell count (got {collapsed_fragments})"
  );

  let table_borders =
    find_table_borders(&collapsed_tree.root).expect("collapsed table attaches border metadata");
  assert_eq!(table_borders.column_count, COLS);
  assert_eq!(table_borders.row_count, ROWS);
  assert_eq!(table_borders.column_line_positions.len(), COLS + 1);
  assert_eq!(table_borders.row_line_positions.len(), ROWS + 1);
  assert!(
    table_borders
      .vertical_borders
      .iter()
      .chain(table_borders.horizontal_borders.iter())
      .chain(table_borders.corner_borders.iter())
      .any(|b| b.is_visible()),
    "collapsed borders should include visible segments"
  );

  let list = DisplayListBuilder::new().build(&collapsed_tree.root);
  let collapsed_items = list
    .items()
    .iter()
    .filter(|item| matches!(item, DisplayItem::TableCollapsedBorders(_)))
    .count();
  assert_eq!(
    collapsed_items, 1,
    "collapsed borders should be emitted as a single paint primitive"
  );
}
