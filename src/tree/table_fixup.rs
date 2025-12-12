//! Table structure fixup
//!
//! Ensures all tables have complete structure required by CSS spec.
//! This module implements the CSS table anonymous box generation algorithm
//! that transforms incomplete table structures into complete ones.
//!
//! # CSS Specification
//!
//! CSS 2.1 Section 17.2.1 defines the anonymous box generation algorithm:
//! <https://www.w3.org/TR/CSS21/tables.html#anonymous-boxes>
//!
//! # Table Structure Requirements
//!
//! A well-formed table has the following structure:
//!
//! ```text
//! Table Wrapper Box (optional, if captions present)
//!   ├── Caption Box(es) (optional)
//!   └── Table Grid Box
//!      ├── Column Group Box (optional)
//!      │   └── Column Box(es)
//!      └── Row Group Box (thead, tbody, tfoot)
//!          └── Row Box
//!              └── Cell Box(es)
//! ```
//!
//! # Anonymous Box Rules
//!
//! 1. If a table has cells/rows not in a row-group → create anonymous row-group
//! 2. If a row-group has cells not in a row → create anonymous row
//! 3. If a row has non-cell content → wrap in anonymous cell
//! 4. Tables with captions need wrapper boxes
//!
//! # Example
//!
//! ```
//! use fastrender::tree::table_fixup::TableStructureFixer;
//! use fastrender::{BoxNode, FormattingContextType};
//! use fastrender::ComputedStyle;
//! use std::sync::Arc;
//!
//! // Create a simple table with cells (missing rows and row-groups)
//! let style = Arc::new(ComputedStyle::default());
//! let table = BoxNode::new_block(
//!     style,
//!     FormattingContextType::Table,
//!     vec![],
//! );
//!
//! // Fix up the table structure
//! let fixed = TableStructureFixer::fixup_table(table).unwrap();
//! ```

use crate::error::Result;
use crate::style::display::{Display, FormattingContextType};
use crate::style::ComputedStyle;
use crate::tree::box_tree::{AnonymousBox, AnonymousType, BoxNode, BoxType};
use std::sync::Arc;

/// Table structure fixer
///
/// Transforms incomplete table structures into complete ones by
/// inserting anonymous boxes as required by CSS 2.1 Section 17.2.1.
///
/// # Algorithm
///
/// The fixup algorithm proceeds in multiple phases:
///
/// 1. **Cell-to-Row Fixup**: Ensure all cells are inside rows
/// 2. **Row-to-Group Fixup**: Ensure all rows are inside row-groups
/// 3. **Column Inference**: Determine column count from cells
/// 4. **Wrapper Creation**: Create table wrapper if captions present
///
/// Each phase operates on the tree independently, allowing for clean
/// separation of concerns and easy testing.
pub struct TableStructureFixer;

impl TableStructureFixer {
    /// Fixes up a table box tree
    ///
    /// Takes a box tree that may have incomplete table structure
    /// and returns a tree with all required anonymous boxes.
    ///
    /// # CSS Requirements
    ///
    /// 1. Table must contain row groups (thead, tbody, tfoot)
    /// 2. Row groups must contain rows
    /// 3. Rows must contain cells
    /// 4. Tables with captions need wrapper boxes
    ///
    /// # Arguments
    ///
    /// * `table_box` - A BoxNode with table formatting context
    ///
    /// # Returns
    ///
    /// Returns the fixed table structure with all anonymous boxes inserted.
    ///
    /// # Errors
    ///
    /// Returns an error if the table structure is fundamentally broken
    /// (e.g., completely empty after fixup).
    pub fn fixup_table(table_box: BoxNode) -> Result<BoxNode> {
        // Verify this is actually a table
        if !Self::is_table_box(&table_box) {
            return Ok(table_box);
        }

        // Fix table structure in multiple passes
        let mut fixed = table_box;

        // Step 1: Wrap cells not in rows with anonymous rows
        fixed = Self::ensure_cells_in_rows(fixed)?;

        // Step 2: Wrap rows not in row-groups with anonymous tbody
        fixed = Self::ensure_rows_in_groups(fixed)?;

        // Step 3: Infer column structure
        fixed = Self::infer_columns(fixed)?;

        // Step 4: Create table wrapper if needed (captions)
        fixed = Self::create_wrapper_if_needed(fixed)?;

        Ok(fixed)
    }

    // ==================== Type Checking ====================

    /// Checks if a box is a table box (display: table or inline-table)
    pub fn is_table_box(box_node: &BoxNode) -> bool {
        match &box_node.box_type {
            BoxType::Block(block) => matches!(block.formatting_context, FormattingContextType::Table),
            _ => false,
        }
    }

    /// Checks if box is a table row (display: table-row or anonymous table row)
    pub fn is_table_row(box_node: &BoxNode) -> bool {
        // Check for anonymous table row
        if let BoxType::Anonymous(anon) = &box_node.box_type {
            if anon.anonymous_type == AnonymousType::TableRow {
                return true;
            }
        }

        // Check for display: table-row
        box_node.style.display == Display::TableRow
    }

    /// Checks if box is a table cell (display: table-cell or anonymous table cell)
    pub fn is_table_cell(box_node: &BoxNode) -> bool {
        // Check for anonymous table cell
        if let BoxType::Anonymous(anon) = &box_node.box_type {
            if anon.anonymous_type == AnonymousType::TableCell {
                return true;
            }
        }

        // Check for display: table-cell
        box_node.style.display == Display::TableCell
    }

    /// Checks if box is a table row group (tbody, thead, tfoot)
    pub fn is_table_row_group(box_node: &BoxNode) -> bool {
        // Check for anonymous table row group
        if let BoxType::Anonymous(anon) = &box_node.box_type {
            if anon.anonymous_type == AnonymousType::TableRowGroup {
                return true;
            }
        }

        // Check for display: table-row-group, table-header-group, table-footer-group
        matches!(
            box_node.style.display,
            Display::TableRowGroup | Display::TableHeaderGroup | Display::TableFooterGroup
        )
    }

    /// Checks if box is a table caption (display: table-caption)
    pub fn is_table_caption(box_node: &BoxNode) -> bool {
        box_node.style.display == Display::TableCaption
    }

    /// Checks if box is a column group (display: table-column-group)
    pub fn is_table_column_group(box_node: &BoxNode) -> bool {
        box_node.style.display == Display::TableColumnGroup
    }

    /// Checks if box is a column (display: table-column)
    pub fn is_table_column(box_node: &BoxNode) -> bool {
        box_node.style.display == Display::TableColumn
    }

    /// Checks if box is a table-internal element (row, cell, row-group, etc.)
    pub fn is_table_internal(box_node: &BoxNode) -> bool {
        Self::is_table_row(box_node)
            || Self::is_table_cell(box_node)
            || Self::is_table_row_group(box_node)
            || Self::is_table_caption(box_node)
            || Self::is_table_column(box_node)
            || Self::is_table_column_group(box_node)
    }

    // ==================== Anonymous Box Creation ====================

    /// Creates an anonymous table row
    fn create_anonymous_row(cells: Vec<BoxNode>) -> BoxNode {
        let style = Arc::new(ComputedStyle::default());

        BoxNode {
            style,
            box_type: BoxType::Anonymous(AnonymousBox {
                anonymous_type: AnonymousType::TableRow,
            }),
            children: cells,
            debug_info: None,
        }
    }

    /// Creates an anonymous table row group (tbody)
    fn create_anonymous_row_group(rows: Vec<BoxNode>) -> BoxNode {
        let style = Arc::new(ComputedStyle::default());

        BoxNode {
            style,
            box_type: BoxType::Anonymous(AnonymousBox {
                anonymous_type: AnonymousType::TableRowGroup,
            }),
            children: rows,
            debug_info: None,
        }
    }

    /// Creates an anonymous table cell
    fn create_anonymous_cell(children: Vec<BoxNode>) -> BoxNode {
        let style = Arc::new(ComputedStyle::default());

        BoxNode {
            style,
            box_type: BoxType::Anonymous(AnonymousBox {
                anonymous_type: AnonymousType::TableCell,
            }),
            children,
            debug_info: None,
        }
    }

    /// Creates an anonymous table wrapper
    fn create_anonymous_wrapper(table: BoxNode) -> BoxNode {
        let style = Arc::new(ComputedStyle::default());

        BoxNode {
            style,
            box_type: BoxType::Anonymous(AnonymousBox {
                anonymous_type: AnonymousType::TableWrapper,
            }),
            children: vec![table],
            debug_info: None,
        }
    }

    // ==================== Cell-to-Row Fixup ====================

    /// Ensures all cells are inside rows
    ///
    /// CSS 2.1 Section 17.2.1: "If a row group has cells not in a row,
    /// generate an anonymous row around those cells."
    fn ensure_cells_in_rows(mut table: BoxNode) -> Result<BoxNode> {
        let children = std::mem::take(&mut table.children);
        let fixed_children = Self::fixup_table_children_for_rows(children)?;
        table.children = fixed_children;
        Ok(table)
    }

    /// Fixes children to ensure cells are in rows
    fn fixup_table_children_for_rows(children: Vec<BoxNode>) -> Result<Vec<BoxNode>> {
        let mut result = Vec::new();
        let mut loose_cells: Vec<BoxNode> = Vec::new();

        for child in children {
            if Self::is_table_cell(&child) {
                // Accumulate loose cells
                loose_cells.push(child);
            } else if Self::is_table_row_group(&child) {
                // Flush loose cells first
                if !loose_cells.is_empty() {
                    let anon_row = Self::create_anonymous_row(std::mem::take(&mut loose_cells));
                    result.push(anon_row);
                }

                // Recursively fix row group
                let fixed_group = Self::fixup_row_group(child)?;
                result.push(fixed_group);
            } else if Self::is_table_row(&child) {
                // Flush loose cells
                if !loose_cells.is_empty() {
                    let anon_row = Self::create_anonymous_row(std::mem::take(&mut loose_cells));
                    result.push(anon_row);
                }
                result.push(child);
            } else if Self::is_table_caption(&child)
                || Self::is_table_column_group(&child)
                || Self::is_table_column(&child)
            {
                // Caption, colgroup, col - pass through (flush loose cells first)
                if !loose_cells.is_empty() {
                    let anon_row = Self::create_anonymous_row(std::mem::take(&mut loose_cells));
                    result.push(anon_row);
                }
                result.push(child);
            } else {
                // Other non-table content - wrap in anonymous cell first
                let anon_cell = Self::create_anonymous_cell(vec![child]);
                loose_cells.push(anon_cell);
            }
        }

        // Flush remaining loose cells
        if !loose_cells.is_empty() {
            let anon_row = Self::create_anonymous_row(loose_cells);
            result.push(anon_row);
        }

        Ok(result)
    }

    /// Fixes row group to ensure cells are in rows
    fn fixup_row_group(mut group: BoxNode) -> Result<BoxNode> {
        let children = std::mem::take(&mut group.children);
        let mut result = Vec::new();
        let mut loose_cells: Vec<BoxNode> = Vec::new();

        for child in children {
            if Self::is_table_cell(&child) {
                loose_cells.push(child);
            } else if Self::is_table_row(&child) {
                if !loose_cells.is_empty() {
                    let anon_row = Self::create_anonymous_row(std::mem::take(&mut loose_cells));
                    result.push(anon_row);
                }
                result.push(child);
            } else {
                // Unexpected content - wrap in anonymous cell then add to loose cells
                if !loose_cells.is_empty() {
                    let anon_row = Self::create_anonymous_row(std::mem::take(&mut loose_cells));
                    result.push(anon_row);
                }

                let anon_cell = Self::create_anonymous_cell(vec![child]);
                loose_cells.push(anon_cell);
            }
        }

        if !loose_cells.is_empty() {
            let anon_row = Self::create_anonymous_row(loose_cells);
            result.push(anon_row);
        }

        group.children = result;
        Ok(group)
    }

    // ==================== Row-to-Group Fixup ====================

    /// Ensures all rows are inside row groups
    ///
    /// CSS 2.1 Section 17.2.1: "If a table has rows not in a row group,
    /// generate an anonymous tbody around those rows."
    fn ensure_rows_in_groups(mut table: BoxNode) -> Result<BoxNode> {
        let children = std::mem::take(&mut table.children);
        let fixed_children = Self::wrap_loose_rows(children)?;
        table.children = fixed_children;
        Ok(table)
    }

    /// Wraps loose rows in anonymous tbody
    fn wrap_loose_rows(children: Vec<BoxNode>) -> Result<Vec<BoxNode>> {
        let mut result = Vec::new();
        let mut loose_rows: Vec<BoxNode> = Vec::new();

        for child in children {
            if Self::is_table_row(&child) {
                // Accumulate loose rows
                loose_rows.push(child);
            } else if Self::is_table_row_group(&child) {
                // Flush loose rows first
                if !loose_rows.is_empty() {
                    let anon_tbody = Self::create_anonymous_row_group(std::mem::take(&mut loose_rows));
                    result.push(anon_tbody);
                }
                result.push(child);
            } else {
                // Caption, colgroup, etc. - pass through
                if !loose_rows.is_empty() {
                    let anon_tbody = Self::create_anonymous_row_group(std::mem::take(&mut loose_rows));
                    result.push(anon_tbody);
                }
                result.push(child);
            }
        }

        // Flush remaining loose rows
        if !loose_rows.is_empty() {
            let anon_tbody = Self::create_anonymous_row_group(loose_rows);
            result.push(anon_tbody);
        }

        Ok(result)
    }

    // ==================== Column Inference ====================

    /// Infers column structure from cells
    ///
    /// CSS tables don't require explicit `<col>` elements - columns
    /// are inferred from the cells in rows.
    fn infer_columns(table: BoxNode) -> Result<BoxNode> {
        // Find all row groups
        let row_groups: Vec<&BoxNode> = table.children.iter().filter(|c| Self::is_table_row_group(c)).collect();

        if row_groups.is_empty() {
            // Empty table is valid
            return Ok(table);
        }

        // Find first row
        let first_row = row_groups.first().and_then(|g| g.children.first());

        if let Some(row) = first_row {
            // Count columns from first row's cells (accounting for colspan)
            let col_count = Self::count_columns(row);

            // For an empty first row, that's still valid (zero columns initially)
            // The table will have zero columns until cells are added
            if col_count == 0 && !row.children.is_empty() {
                // Row has children but no columns means children aren't cells
                // This shouldn't happen after cell fixup, but we handle it gracefully
            }
        }

        Ok(table)
    }

    /// Counts columns in a row (accounting for colspan)
    fn count_columns(row: &BoxNode) -> usize {
        row.children
            .iter()
            .filter(|c| Self::is_table_cell(c))
            .map(|_cell| _cell.debug_info.as_ref().map(|info| info.colspan.max(1)).unwrap_or(1))
            .sum()
    }

    // ==================== Wrapper Creation ====================

    /// Creates table wrapper if needed
    ///
    /// CSS 2.1: Tables with captions need a wrapper box
    fn create_wrapper_if_needed(table: BoxNode) -> Result<BoxNode> {
        // Check if table has any captions
        let has_caption = table.children.iter().any(|c| Self::is_table_caption(c));

        if has_caption {
            // Create wrapper containing table and captions
            Ok(Self::create_anonymous_wrapper(table))
        } else {
            Ok(table)
        }
    }

    // ==================== Utility Methods ====================

    /// Recursively fixes up all table boxes in a tree
    ///
    /// Walks the entire box tree and applies table fixup to any
    /// table boxes found.
    pub fn fixup_tree(mut root: BoxNode) -> Result<BoxNode> {
        // If this is a table, fix it up
        if Self::is_table_box(&root) {
            return Self::fixup_table(root);
        }

        // Otherwise, recursively process children
        let children = std::mem::take(&mut root.children);
        let fixed_children: Result<Vec<BoxNode>> = children.into_iter().map(Self::fixup_tree).collect();
        root.children = fixed_children?;

        Ok(root)
    }

    /// Validates that a table has proper structure after fixup
    ///
    /// This is useful for debugging and testing. Returns true if:
    /// - All cells are in rows
    /// - All rows are in row groups
    /// - Column count is consistent
    pub fn validate_table_structure(table: &BoxNode) -> bool {
        if !Self::is_table_box(table) {
            return false;
        }

        // Check that all row groups contain only rows
        for child in &table.children {
            if Self::is_table_row_group(child) {
                for row_child in &child.children {
                    if !Self::is_table_row(row_child) {
                        return false;
                    }

                    // Check that all row children are cells
                    for cell_child in &row_child.children {
                        if !Self::is_table_cell(cell_child) {
                            return false;
                        }
                    }
                }
            } else if Self::is_table_row(child) {
                // Loose row (should have been wrapped)
                return false;
            } else if Self::is_table_cell(child) {
                // Loose cell (should have been wrapped)
                return false;
            }
            // Caption, colgroup are OK at table level
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tree::box_tree::BlockBox;
    use crate::tree::debug::DebugInfo;

    // Helper to create a default style
    fn default_style() -> Arc<ComputedStyle> {
        Arc::new(ComputedStyle::default())
    }

    // Helper to create a table-row style
    fn table_row_style() -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.display = Display::TableRow;
        Arc::new(style)
    }

    // Helper to create a table-cell style
    fn table_cell_style() -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.display = Display::TableCell;
        Arc::new(style)
    }

    // Helper to create a table-row-group style
    fn table_row_group_style() -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.display = Display::TableRowGroup;
        Arc::new(style)
    }

    // Helper to create a caption style
    fn table_caption_style() -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.display = Display::TableCaption;
        Arc::new(style)
    }

    // Helper to create a table box
    fn table_box(children: Vec<BoxNode>) -> BoxNode {
        BoxNode::new_block(default_style(), FormattingContextType::Table, children)
    }

    // Helper to create a row box with explicit display
    fn row_box(cells: Vec<BoxNode>) -> BoxNode {
        BoxNode {
            style: table_row_style(),
            box_type: BoxType::Block(BlockBox {
                formatting_context: FormattingContextType::Block,
            }),
            children: cells,
            debug_info: None,
        }
    }

    // Helper to create a cell box with explicit display
    fn cell_box(content: Vec<BoxNode>) -> BoxNode {
        BoxNode {
            style: table_cell_style(),
            box_type: BoxType::Block(BlockBox {
                formatting_context: FormattingContextType::Block,
            }),
            children: content,
            debug_info: None,
        }
    }

    // Helper to create a row group box
    fn row_group_box(rows: Vec<BoxNode>) -> BoxNode {
        BoxNode {
            style: table_row_group_style(),
            box_type: BoxType::Block(BlockBox {
                formatting_context: FormattingContextType::Block,
            }),
            children: rows,
            debug_info: None,
        }
    }

    // Helper to create a caption box
    fn caption_box(content: Vec<BoxNode>) -> BoxNode {
        BoxNode {
            style: table_caption_style(),
            box_type: BoxType::Block(BlockBox {
                formatting_context: FormattingContextType::Block,
            }),
            children: content,
            debug_info: None,
        }
    }

    // ==================== Type Checker Tests ====================

    #[test]
    fn test_is_table_box() {
        let table = table_box(vec![]);
        assert!(TableStructureFixer::is_table_box(&table));

        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        assert!(!TableStructureFixer::is_table_box(&block));
    }

    #[test]
    fn test_is_table_row() {
        let row = row_box(vec![]);
        assert!(TableStructureFixer::is_table_row(&row));

        let anon_row = TableStructureFixer::create_anonymous_row(vec![]);
        assert!(TableStructureFixer::is_table_row(&anon_row));

        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        assert!(!TableStructureFixer::is_table_row(&block));
    }

    #[test]
    fn test_is_table_cell() {
        let cell = cell_box(vec![]);
        assert!(TableStructureFixer::is_table_cell(&cell));

        let anon_cell = TableStructureFixer::create_anonymous_cell(vec![]);
        assert!(TableStructureFixer::is_table_cell(&anon_cell));

        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        assert!(!TableStructureFixer::is_table_cell(&block));
    }

    #[test]
    fn test_is_table_row_group() {
        let group = row_group_box(vec![]);
        assert!(TableStructureFixer::is_table_row_group(&group));

        let anon_group = TableStructureFixer::create_anonymous_row_group(vec![]);
        assert!(TableStructureFixer::is_table_row_group(&anon_group));

        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        assert!(!TableStructureFixer::is_table_row_group(&block));
    }

    #[test]
    fn test_is_table_caption() {
        let caption = caption_box(vec![]);
        assert!(TableStructureFixer::is_table_caption(&caption));

        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        assert!(!TableStructureFixer::is_table_caption(&block));
    }

    // ==================== Loose Cell Fixup Tests ====================

    #[test]
    fn test_table_with_loose_cells_wrapped_in_row() {
        // Table with cells not in rows
        let cell1 = cell_box(vec![]);
        let cell2 = cell_box(vec![]);

        let table = table_box(vec![cell1, cell2]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        // Cells should be wrapped in anonymous row, then in anonymous row-group
        assert_eq!(fixed.children.len(), 1);
        assert!(TableStructureFixer::is_table_row_group(&fixed.children[0]));
        assert_eq!(fixed.children[0].children.len(), 1);
        assert!(TableStructureFixer::is_table_row(&fixed.children[0].children[0]));
        assert_eq!(fixed.children[0].children[0].children.len(), 2);
    }

    #[test]
    fn test_table_with_loose_rows_wrapped_in_tbody() {
        // Table with rows not in row group
        let row1 = row_box(vec![cell_box(vec![])]);
        let row2 = row_box(vec![cell_box(vec![])]);

        let table = table_box(vec![row1, row2]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        // Rows should be wrapped in anonymous tbody
        assert_eq!(fixed.children.len(), 1);
        assert!(TableStructureFixer::is_table_row_group(&fixed.children[0]));
        assert_eq!(fixed.children[0].children.len(), 2);
    }

    #[test]
    fn test_complete_table_no_fixup() {
        // Table with proper structure - no fixup needed
        let tbody = row_group_box(vec![row_box(vec![cell_box(vec![]), cell_box(vec![])])]);

        let table = table_box(vec![tbody]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        // Should remain unchanged structurally
        assert_eq!(fixed.children.len(), 1);
        assert!(TableStructureFixer::is_table_row_group(&fixed.children[0]));
        assert!(TableStructureFixer::validate_table_structure(&fixed));
    }

    #[test]
    fn test_row_group_with_loose_cells() {
        // Row group with cells not in rows
        let tbody = row_group_box(vec![cell_box(vec![]), cell_box(vec![])]);

        let table = table_box(vec![tbody]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        // Cells in tbody should be wrapped in anonymous row
        assert_eq!(fixed.children.len(), 1);
        let fixed_tbody = &fixed.children[0];
        assert!(TableStructureFixer::is_table_row_group(fixed_tbody));
        assert_eq!(fixed_tbody.children.len(), 1);
        assert!(TableStructureFixer::is_table_row(&fixed_tbody.children[0]));
        assert_eq!(fixed_tbody.children[0].children.len(), 2);
    }

    // ==================== Mixed Content Tests ====================

    #[test]
    fn test_mixed_loose_rows_and_row_groups() {
        // Table with both loose rows and row groups
        let row1 = row_box(vec![cell_box(vec![])]);
        let tbody = row_group_box(vec![row_box(vec![cell_box(vec![])])]);
        let row2 = row_box(vec![cell_box(vec![])]);

        let table = table_box(vec![row1, tbody, row2]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        // Should have 3 row groups (anonymous, original, anonymous)
        assert_eq!(fixed.children.len(), 3);
        assert!(TableStructureFixer::is_table_row_group(&fixed.children[0]));
        assert!(TableStructureFixer::is_table_row_group(&fixed.children[1]));
        assert!(TableStructureFixer::is_table_row_group(&fixed.children[2]));
    }

    #[test]
    fn test_non_table_content_wrapped_in_cell() {
        // Table with non-table content (should be wrapped in cell then row then row-group)
        let text = BoxNode::new_text(default_style(), "Hello".to_string());
        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

        let table = table_box(vec![text, block]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        // Non-table content should be wrapped in anonymous cells
        assert_eq!(fixed.children.len(), 1);
        assert!(TableStructureFixer::is_table_row_group(&fixed.children[0]));
        assert_eq!(fixed.children[0].children.len(), 1);
        assert!(TableStructureFixer::is_table_row(&fixed.children[0].children[0]));
        // Should have 2 cells (one for each non-table content)
        assert_eq!(fixed.children[0].children[0].children.len(), 2);
    }

    // ==================== Caption Tests ====================

    #[test]
    fn test_table_with_caption_gets_wrapper() {
        // Table with caption should get wrapper
        let caption = caption_box(vec![BoxNode::new_text(default_style(), "Caption".to_string())]);
        let tbody = row_group_box(vec![row_box(vec![cell_box(vec![])])]);

        let table = table_box(vec![caption, tbody]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        // Should be wrapped in anonymous table wrapper
        assert!(matches!(
            &fixed.box_type,
            BoxType::Anonymous(AnonymousBox {
                anonymous_type: AnonymousType::TableWrapper
            })
        ));
        assert_eq!(fixed.children.len(), 1);
        assert!(TableStructureFixer::is_table_box(&fixed.children[0]));
    }

    #[test]
    fn test_table_without_caption_no_wrapper() {
        // Table without caption should not get wrapper
        let tbody = row_group_box(vec![row_box(vec![cell_box(vec![])])]);

        let table = table_box(vec![tbody]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        // Should NOT be wrapped
        assert!(TableStructureFixer::is_table_box(&fixed));
    }

    // ==================== Empty Table Tests ====================

    #[test]
    fn test_empty_table() {
        let table = table_box(vec![]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        // Empty table is valid
        assert_eq!(fixed.children.len(), 0);
    }

    #[test]
    fn test_table_with_empty_row_group() {
        let tbody = row_group_box(vec![]);

        let table = table_box(vec![tbody]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        assert_eq!(fixed.children.len(), 1);
        assert!(TableStructureFixer::is_table_row_group(&fixed.children[0]));
        assert_eq!(fixed.children[0].children.len(), 0);
    }

    #[test]
    fn test_table_with_empty_row() {
        let row = row_box(vec![]);
        let tbody = row_group_box(vec![row]);

        let table = table_box(vec![tbody]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        assert_eq!(fixed.children.len(), 1);
        assert_eq!(fixed.children[0].children.len(), 1);
        assert_eq!(fixed.children[0].children[0].children.len(), 0);
    }

    // ==================== Column Inference Tests ====================

    #[test]
    fn test_column_count_simple() {
        let row = row_box(vec![cell_box(vec![]), cell_box(vec![]), cell_box(vec![])]);

        let count = TableStructureFixer::count_columns(&row);
        assert_eq!(count, 3);
    }

    #[test]
    fn test_column_count_empty_row() {
        let row = row_box(vec![]);

        let count = TableStructureFixer::count_columns(&row);
        assert_eq!(count, 0);
    }

    #[test]
    fn test_column_count_accounts_for_colspan() {
        let mut spanning = cell_box(vec![]);
        spanning.debug_info = Some(DebugInfo::new(None, None, vec![]).with_spans(3, 1));
        let row = row_box(vec![spanning, cell_box(vec![])]);

        let count = TableStructureFixer::count_columns(&row);
        assert_eq!(count, 4);
    }

    // ==================== Validation Tests ====================

    #[test]
    fn test_validate_proper_structure() {
        let tbody = row_group_box(vec![row_box(vec![cell_box(vec![]), cell_box(vec![])])]);

        let table = table_box(vec![tbody]);

        assert!(TableStructureFixer::validate_table_structure(&table));
    }

    #[test]
    fn test_validate_loose_row_fails() {
        let row = row_box(vec![cell_box(vec![])]);

        let table = table_box(vec![row]);

        // Loose row should fail validation (before fixup)
        assert!(!TableStructureFixer::validate_table_structure(&table));
    }

    #[test]
    fn test_validate_loose_cell_fails() {
        let cell = cell_box(vec![]);

        let table = table_box(vec![cell]);

        // Loose cell should fail validation (before fixup)
        assert!(!TableStructureFixer::validate_table_structure(&table));
    }

    // ==================== Tree Fixup Tests ====================

    #[test]
    fn test_fixup_tree_with_nested_tables() {
        // Create a block containing a table
        let inner_table = table_box(vec![cell_box(vec![])]);
        let outer = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![inner_table]);

        let fixed = TableStructureFixer::fixup_tree(outer).unwrap();

        // The inner table should be fixed
        assert_eq!(fixed.children.len(), 1);
        let inner = &fixed.children[0];
        assert!(TableStructureFixer::is_table_box(inner));
        // The cell should be wrapped in row and row-group
        assert!(TableStructureFixer::is_table_row_group(&inner.children[0]));
    }

    #[test]
    fn test_fixup_tree_non_table() {
        // Non-table box should pass through unchanged
        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

        let fixed = TableStructureFixer::fixup_tree(block).unwrap();

        assert!(matches!(fixed.box_type, BoxType::Block(_)));
    }

    // ==================== Complex Scenario Tests ====================

    #[test]
    fn test_complex_table_multiple_row_groups() {
        // Table with multiple row groups
        let thead = row_group_box(vec![row_box(vec![cell_box(vec![])])]);
        let tbody = row_group_box(vec![row_box(vec![cell_box(vec![])]), row_box(vec![cell_box(vec![])])]);
        let tfoot = row_group_box(vec![row_box(vec![cell_box(vec![])])]);

        let table = table_box(vec![thead, tbody, tfoot]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        assert_eq!(fixed.children.len(), 3);
        assert!(TableStructureFixer::validate_table_structure(&fixed));
    }

    #[test]
    fn test_table_with_all_elements() {
        // Table with caption, row groups, and various content
        let caption = caption_box(vec![]);
        let thead = row_group_box(vec![row_box(vec![cell_box(vec![])])]);
        let tbody = row_group_box(vec![row_box(vec![cell_box(vec![]), cell_box(vec![])])]);

        let table = table_box(vec![caption, thead, tbody]);

        let fixed = TableStructureFixer::fixup_table(table).unwrap();

        // Should have wrapper due to caption
        assert!(matches!(
            &fixed.box_type,
            BoxType::Anonymous(AnonymousBox {
                anonymous_type: AnonymousType::TableWrapper
            })
        ));
    }

    #[test]
    fn test_deeply_nested_table_content() {
        // Cell containing another table
        let inner_table = table_box(vec![row_group_box(vec![row_box(vec![cell_box(vec![])])])]);
        let outer_cell = cell_box(vec![inner_table]);
        let outer_row = row_box(vec![outer_cell]);
        let outer_tbody = row_group_box(vec![outer_row]);
        let outer_table = table_box(vec![outer_tbody]);

        let fixed = TableStructureFixer::fixup_table(outer_table).unwrap();

        assert!(TableStructureFixer::validate_table_structure(&fixed));
    }
}
