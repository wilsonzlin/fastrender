use fastrender::style::display::{Display, FormattingContextType};
use fastrender::tree::box_tree::{AnonymousType, BoxNode, BoxType};
use fastrender::tree::table_fixup::TableStructureFixer;
use fastrender::{FastRender, RenderOptions};

fn find_table_box(root: &BoxNode) -> &BoxNode {
  fn walk<'a>(node: &'a BoxNode) -> Option<&'a BoxNode> {
    if node.formatting_context() == Some(FormattingContextType::Table) {
      return Some(node);
    }
    node.children.iter().find_map(walk)
  }

  walk(root).expect("table box present")
}

fn first_non_caption_child(box_node: &BoxNode) -> Option<&BoxNode> {
  box_node
    .children
    .iter()
    .find(|child| child.style.display != Display::TableCaption)
}

fn contains_table_wrapper(node: &BoxNode) -> bool {
  if matches!(
    &node.box_type,
    BoxType::Anonymous(anon) if anon.anonymous_type == AnonymousType::TableWrapper
  ) {
    return true;
  }

  node.children.iter().any(contains_table_wrapper)
}

fn extract_text(node: &BoxNode) -> String {
  let mut text = String::new();
  if let Some(t) = node.text() {
    text.push_str(t);
  }
  for child in &node.children {
    text.push_str(&extract_text(child));
  }
  text
}

#[test]
fn loose_cells_are_wrapped_into_row_groups_during_box_generation() {
  let mut renderer = FastRender::new().unwrap();
  let prepared = renderer
    .prepare_html("<table><td>Loose</td></table>", RenderOptions::new())
    .unwrap();
  let box_tree = prepared.box_tree();

  assert!(
    !contains_table_wrapper(&box_tree.root),
    "box generation should not create table wrappers"
  );

  let table = find_table_box(&box_tree.root);
  let row_group = first_non_caption_child(table).expect("row group created");
  assert!(TableStructureFixer::is_table_row_group(row_group));

  let row = row_group.children.first().expect("row created");
  assert!(TableStructureFixer::is_table_row(row));

  let cell = row.children.first().expect("cell created");
  assert!(TableStructureFixer::is_table_cell(cell));
}

#[test]
fn text_inside_rows_is_wrapped_in_anonymous_cells() {
  let mut renderer = FastRender::new().unwrap();
  let prepared = renderer
    .prepare_html(
      // Use styled divs rather than literal <table> markup so the text node isn't
      // foster-parented out of the table by the HTML parser (per the table
      // insertion mode rules). This exercises row-level anonymous cell generation
      // in the box tree.
      r#"<div style="display: table"><div style="display: table-row">oops<div style="display: table-cell">Cell</div></div></div>"#,
      RenderOptions::new(),
    )
    .unwrap();
  let box_tree = prepared.box_tree();

  assert!(
    !contains_table_wrapper(&box_tree.root),
    "box generation should not create table wrappers"
  );

  let table = find_table_box(&box_tree.root);
  let row_group = first_non_caption_child(table).expect("row group created");
  let row = row_group.children.first().expect("row created");

  assert_eq!(row.children.len(), 2);
  assert!(row
    .children
    .iter()
    .all(|child| TableStructureFixer::is_table_cell(child)));
  assert!(matches!(
    &row.children[0].box_type,
    BoxType::Anonymous(anon) if anon.anonymous_type == AnonymousType::TableCell
  ));
  assert!(extract_text(&row.children[0]).contains("oops"));
}
