use fastrender::style::display::{Display, FormattingContextType};
use fastrender::style::types::CaptionSide;
use fastrender::tree::box_tree::{AnonymousBox, AnonymousType, BoxNode, BoxType};
use fastrender::tree::table_fixup::TableStructureFixer;
use fastrender::ComputedStyle;
use std::sync::Arc;

fn style_with_display(display: Display) -> Arc<ComputedStyle> {
  let mut style = ComputedStyle::default();
  style.display = display;
  Arc::new(style)
}

fn table_box(children: Vec<BoxNode>) -> BoxNode {
  BoxNode::new_block(
    style_with_display(Display::Table),
    FormattingContextType::Table,
    children,
  )
}

fn caption_box(caption_side: CaptionSide) -> BoxNode {
  let mut style = ComputedStyle::default();
  style.display = Display::TableCaption;
  style.caption_side = caption_side;
  BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![])
}

fn row_box(cells: Vec<BoxNode>) -> BoxNode {
  BoxNode::new_block(
    style_with_display(Display::TableRow),
    FormattingContextType::Block,
    cells,
  )
}

fn cell_box() -> BoxNode {
  BoxNode::new_block(
    style_with_display(Display::TableCell),
    FormattingContextType::Block,
    vec![],
  )
}

fn row_group_box(rows: Vec<BoxNode>) -> BoxNode {
  BoxNode::new_block(
    style_with_display(Display::TableRowGroup),
    FormattingContextType::Block,
    rows,
  )
}

#[test]
fn wrapper_places_top_caption_before_table_grid() {
  let caption = caption_box(CaptionSide::Top);
  let tbody = row_group_box(vec![row_box(vec![cell_box()])]);

  let table = table_box(vec![caption, tbody]);
  let fixed = TableStructureFixer::fixup_table(table).unwrap();

  assert!(matches!(
    &fixed.box_type,
    BoxType::Anonymous(AnonymousBox {
      anonymous_type: AnonymousType::TableWrapper
    })
  ));
  assert_eq!(fixed.children.len(), 2);
  assert!(TableStructureFixer::is_table_caption(&fixed.children[0]));
  let grid = &fixed.children[1];
  assert!(TableStructureFixer::is_table_box(grid));
  assert!(!grid
    .children
    .iter()
    .any(TableStructureFixer::is_table_caption));
}

#[test]
fn wrapper_places_bottom_caption_after_table_grid() {
  let caption = caption_box(CaptionSide::Bottom);
  let tbody = row_group_box(vec![row_box(vec![cell_box()])]);

  let table = table_box(vec![tbody, caption]);
  let fixed = TableStructureFixer::fixup_table(table).unwrap();

  assert!(matches!(
    &fixed.box_type,
    BoxType::Anonymous(AnonymousBox {
      anonymous_type: AnonymousType::TableWrapper
    })
  ));
  assert_eq!(fixed.children.len(), 2);
  let grid = &fixed.children[0];
  assert!(TableStructureFixer::is_table_box(grid));
  assert!(TableStructureFixer::is_table_caption(&fixed.children[1]));
  assert!(!grid
    .children
    .iter()
    .any(TableStructureFixer::is_table_caption));
}
