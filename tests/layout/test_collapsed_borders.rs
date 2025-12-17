use fastrender::color::Rgba;
use fastrender::layout::table::{compute_collapsed_borders_for_test, TableStructure};
use fastrender::style::display::Display;
use fastrender::style::types::{BorderCollapse, BorderStyle};
use fastrender::style::values::Length;
use fastrender::{BoxNode, ComputedStyle, FormattingContextType};
use std::sync::Arc;

fn build_two_cell_row(left: ComputedStyle, right: ComputedStyle) -> BoxNode {
    let left = BoxNode::new_block(Arc::new(left), FormattingContextType::Block, vec![]);
    let right = BoxNode::new_block(Arc::new(right), FormattingContextType::Block, vec![]);
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![left, right])
}

fn build_table_with_row(row: BoxNode) -> BoxNode {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;
    BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row])
}

#[test]
fn collapsed_borders_prefer_hidden_over_style() {
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    row_style.border_top_style = BorderStyle::Hidden;
    row_style.border_top_width = Length::px(2.0);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.border_top_style = BorderStyle::Solid;
    cell_style.border_top_width = Length::px(10.0);

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);
    let table = build_table_with_row(row);

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders_for_test(&table, &structure);

    assert_eq!(borders.horizontal[0][0].style, BorderStyle::None);
    assert!((borders.horizontal[0][0].width - 0.0).abs() < f32::EPSILON);
}

#[test]
fn collapsed_borders_prefer_wider_when_style_equal() {
    let mut left = ComputedStyle::default();
    left.display = Display::TableCell;
    left.border_right_style = BorderStyle::Solid;
    left.border_right_width = Length::px(8.0);
    left.border_right_color = Rgba::from_rgba8(255, 0, 0, 255);

    let mut right = ComputedStyle::default();
    right.display = Display::TableCell;
    right.border_left_style = BorderStyle::Solid;
    right.border_left_width = Length::px(2.0);
    right.border_left_color = Rgba::from_rgba8(0, 0, 255, 255);

    let row = build_two_cell_row(left, right);
    let table = build_table_with_row(row);

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders_for_test(&table, &structure);

    let middle = borders.vertical[1][0];
    assert_eq!(middle.style, BorderStyle::Solid);
    assert!((middle.width - 8.0).abs() < 0.01, "widest equal-style border should win");
    assert_eq!(middle.color, Rgba::from_rgba8(255, 0, 0, 255));
}

#[test]
fn collapsed_borders_tie_on_style_and_width_prefers_cell() {
    let mut row_left = ComputedStyle::default();
    row_left.display = Display::TableCell;
    row_left.border_right_style = BorderStyle::Solid;
    row_left.border_right_width = Length::px(4.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    row_style.border_right_style = BorderStyle::Solid;
    row_style.border_right_width = Length::px(4.0);

    let mut row_right = ComputedStyle::default();
    row_right.display = Display::TableCell;
    row_right.border_left_style = BorderStyle::Solid;
    row_right.border_left_width = Length::px(4.0);

    let left = BoxNode::new_block(Arc::new(row_left), FormattingContextType::Block, vec![]);
    let right = BoxNode::new_block(Arc::new(row_right), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![left, right]);
    let table = build_table_with_row(row);

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders_for_test(&table, &structure);

    let middle = borders.vertical[1][0];
    assert_eq!(middle.style, BorderStyle::Solid);
    // Cell should win the tie over the row border.
    assert_eq!(middle.width, 4.0);
}

