use fastrender::style::display::{Display, FormattingContextType};
use fastrender::style::types::{Direction, WritingMode};
use fastrender::tree::table_fixup::TableStructureFixer;
use fastrender::{BoxNode, ComputedStyle};
use std::sync::Arc;

fn table_with_style(mut style: ComputedStyle, child: BoxNode) -> BoxNode {
  style.display = Display::Table;
  let style = Arc::new(style);
  BoxNode::new_block(style.clone(), FormattingContextType::Table, vec![child])
}

fn text_with_inherited_style(base: &ComputedStyle, text: &str) -> BoxNode {
  let mut text_style = base.clone();
  text_style.display = Display::Inline;
  BoxNode::new_text(Arc::new(text_style), text.to_string())
}

#[test]
fn anonymous_table_boxes_inherit_font_size() {
  let mut style = ComputedStyle::default();
  style.font_size = 40.0;

  let text = text_with_inherited_style(&style, "cell");
  let table = table_with_style(style, text);

  let fixed = TableStructureFixer::fixup_table(table).unwrap();

  let row_group = fixed.children.first().expect("row group created");
  assert_eq!(row_group.style.display, Display::TableRowGroup);
  assert_eq!(row_group.style.font_size, 40.0);

  let row = row_group.children.first().expect("row created");
  assert_eq!(row.style.display, Display::TableRow);
  assert_eq!(row.style.font_size, 40.0);

  let cell = row.children.first().expect("cell created");
  assert_eq!(cell.style.display, Display::TableCell);
  assert_eq!(cell.style.font_size, 40.0);
}

#[test]
fn anonymous_table_boxes_inherit_direction_and_writing_mode() {
  let mut style = ComputedStyle::default();
  style.direction = Direction::Rtl;
  style.writing_mode = WritingMode::VerticalRl;

  let text = text_with_inherited_style(&style, "rtl");
  let table = table_with_style(style, text);

  let fixed = TableStructureFixer::fixup_table(table).unwrap();

  let row_group = fixed.children.first().expect("row group created");
  assert_eq!(row_group.style.direction, Direction::Rtl);
  assert_eq!(row_group.style.writing_mode, WritingMode::VerticalRl);

  let row = row_group.children.first().expect("row created");
  assert_eq!(row.style.direction, Direction::Rtl);
  assert_eq!(row.style.writing_mode, WritingMode::VerticalRl);

  let cell = row.children.first().expect("cell created");
  assert_eq!(cell.style.direction, Direction::Rtl);
  assert_eq!(cell.style.writing_mode, WritingMode::VerticalRl);
}
