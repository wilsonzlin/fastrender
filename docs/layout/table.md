# Phase 2.5: Table Layout Implementation

**Duration:** 2 weeks
**Prerequisites:** Block layout, inline layout complete
**Dependencies:** src/layout/block.rs, src/layout/inline.rs
**Output:** Spec-compliant table layout algorithm

## Objectives

Implement CSS table layout from scratch, replacing the broken table→flexbox hack.

**Critical Context:** This is the most important layout mode to get right. The current codebase's fundamental flaw is trying to fake tables with flexbox. We will implement the actual CSS table algorithm.

## Why This Is Hard

Tables are complex because:
1. **Bidirectional constraints**: Column widths affect row heights and vice versa
2. **Global analysis**: Width of ANY cell affects width of entire column
3. **Anonymous box generation**: Table structure has many implicit boxes
4. **Two algorithms**: Fixed layout vs auto layout (completely different)
5. **Border collapse**: Complex precedence rules for adjacent borders

## Specification References

**Primary:** CSS Tables Module Level 3
- Section 3: Table structure
- Section 4: Width computation
- Section 5: Height computation
- Section 6: Fixed table layout
- Section 7: Automatic table layout

**Supporting:** CSS 2.1 Chapter 17 (Tables)

## Table Structure

### CSS Table Model

```
display: table          → Table Wrapper Box
                           ├─ display: table-caption → Table Caption Box(es)
                           └─ Table Grid Box
                              ├─ display: table-column-group → Column Group Box
                              │  └─ display: table-column → Column Box
                              ├─ display: table-row-group → Row Group Box (thead, tbody, tfoot)
                              │  └─ display: table-row → Row Box
                              │     └─ display: table-cell → Cell Box
```

### Anonymous Box Generation

The table structure requires many anonymous boxes. Example:

```html
<div style="display: table">
  <div style="display: table-cell">Cell</div>
</div>
```

Missing structure (must be generated):
- Anonymous table-row wrapping the cell
- Anonymous table-row-group wrapping the row

**Algorithm:** Anonymous Table Box Generation (from spec)

```
Input: Box with display: table and children
Output: Properly structured table box tree

1. Wrap all consecutive table-caption boxes in anonymous table-wrapper
2. Create anonymous table grid box if not present
3. For each child of table grid:
   a. If not row-group/row: create anonymous row
   b. If consecutive non-row-groups: wrap in anonymous row-group
4. For each row:
   a. If child is not cell: wrap in anonymous cell
```

## Step 1: Table Structure Types

**File:** `src/layout/table/mod.rs` (NEW)

```rust
//! Table Layout Algorithm
//!
//! Implements CSS Tables Module Level 3
//! https://www.w3.org/TR/css-tables-3/

use crate::geom::{Size, Rect, EdgeOffsets};
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment::Fragment;
use crate::layout::formatting_context::{FormattingContext, Constraints};
use std::sync::Arc;

pub mod structure;
pub mod width;
pub mod height;
pub mod border_collapse;

use structure::TableStructure;

/// Table layout algorithm
pub struct TableLayout {
    /// Optional logger for debugging
    debug: bool,
}

impl TableLayout {
    pub fn new() -> Self {
        Self { debug: false }
    }

    pub fn with_debug(mut self) -> Self {
        self.debug = true;
        self
    }
}

impl FormattingContext for TableLayout {
    fn layout(&mut self, box_node: &BoxNode, constraints: Constraints) -> Fragment {
        // Three-phase algorithm:
        // 1. Analyze structure (rows, columns, cells)
        // 2. Compute column widths
        // 3. Compute row heights and position cells

        // Phase 1: Build table structure
        let structure = TableStructure::from_box_tree(box_node);

        if self.debug {
            eprintln!("Table structure: {} rows, {} cols",
                structure.row_count(), structure.column_count());
        }

        // Phase 2: Compute widths
        let column_widths = if is_fixed_table_layout(&box_node.style) {
            width::compute_fixed_widths(&structure, &constraints)
        } else {
            width::compute_auto_widths(&structure, &constraints)
        };

        // Phase 3: Compute heights
        let row_heights = height::compute_row_heights(&structure, &column_widths);

        // Phase 4: Create fragments
        self.create_table_fragment(&structure, &column_widths, &row_heights)
    }
}

/// Check if table uses fixed layout algorithm
fn is_fixed_table_layout(style: &crate::style::ComputedStyle) -> bool {
    // Check table-layout property
    // Default is auto
    false // TODO: implement when we add table-layout property
}
```

**File:** `src/layout/table/structure.rs` (NEW)

```rust
//! Table structure analysis
//!
//! Analyzes the box tree to extract table structure (rows, columns, cells)

use crate::tree::box_tree::BoxNode;
use std::collections::HashMap;

/// Analyzed table structure
///
/// After anonymous box generation and structure analysis,
/// this represents the logical table grid.
#[derive(Debug)]
pub struct TableStructure {
    /// All rows in order
    rows: Vec<TableRow>,

    /// Number of columns (max cells in any row, accounting for colspan)
    column_count: usize,

    /// Column boxes (if present)
    columns: Vec<Option<BoxNode>>,

    /// Column groups (if present)
    column_groups: Vec<ColumnGroup>,
}

#[derive(Debug)]
pub struct TableRow {
    /// Row box
    pub row_box: BoxNode,

    /// Cells in this row
    pub cells: Vec<TableCell>,

    /// Row index
    pub index: usize,
}

#[derive(Debug)]
pub struct TableCell {
    /// Cell box
    pub cell_box: BoxNode,

    /// Column index (accounting for previous rowspans)
    pub column_index: usize,

    /// How many columns this cell spans
    pub colspan: usize,

    /// How many rows this cell spans
    pub rowspan: usize,

    /// Baseline for alignment
    pub baseline: Option<f32>,
}

#[derive(Debug)]
pub struct ColumnGroup {
    /// Column group box
    pub group_box: BoxNode,

    /// Columns in this group
    pub columns: Vec<usize>,
}

impl TableStructure {
    /// Build table structure from box tree
    pub fn from_box_tree(table_box: &BoxNode) -> Self {
        let mut builder = TableStructureBuilder::new();
        builder.analyze(table_box);
        builder.build()
    }

    pub fn row_count(&self) -> usize {
        self.rows.len()
    }

    pub fn column_count(&self) -> usize {
        self.column_count
    }

    pub fn rows(&self) -> &[TableRow] {
        &self.rows
    }

    pub fn get_cell(&self, row: usize, col: usize) -> Option<&TableCell> {
        self.rows.get(row)?.cells.iter().find(|c| {
            c.column_index <= col && col < c.column_index + c.colspan
        })
    }
}

/// Builder for table structure
struct TableStructureBuilder {
    rows: Vec<TableRow>,
    column_count: usize,
    /// Track which cells are occupied by rowspan from previous rows
    /// Map: (row, col) → rowspan_remaining
    occupied_cells: HashMap<(usize, usize), usize>,
}

impl TableStructureBuilder {
    fn new() -> Self {
        Self {
            rows: Vec::new(),
            column_count: 0,
            occupied_cells: HashMap::new(),
        }
    }

    fn analyze(&mut self, table_box: &BoxNode) {
        // Find table-row-group and table-row boxes
        for child in &table_box.children {
            match get_display_type(child) {
                DisplayType::TableRowGroup => {
                    self.analyze_row_group(child);
                }
                DisplayType::TableRow => {
                    self.analyze_row(child, self.rows.len());
                }
                DisplayType::TableCell => {
                    // Naked cell - need anonymous row
                    // This is a spec violation but we handle it gracefully
                    eprintln!("Warning: table-cell without table-row");
                }
                _ => {
                    // Other children are ignored or wrapped in anonymous boxes
                }
            }
        }
    }

    fn analyze_row_group(&mut self, group_box: &BoxNode) {
        for row_box in &group_box.children {
            if matches!(get_display_type(row_box), DisplayType::TableRow) {
                self.analyze_row(row_box, self.rows.len());
            }
        }
    }

    fn analyze_row(&mut self, row_box: &BoxNode, row_index: usize) {
        let mut cells = Vec::new();
        let mut col_index = 0;

        for cell_box in &row_box.children {
            if matches!(get_display_type(cell_box), DisplayType::TableCell) {
                // Skip columns occupied by previous rowspans
                while self.is_occupied(row_index, col_index) {
                    col_index += 1;
                }

                let colspan = get_colspan(cell_box);
                let rowspan = get_rowspan(cell_box);

                cells.push(TableCell {
                    cell_box: cell_box.clone(),
                    column_index: col_index,
                    colspan,
                    rowspan,
                    baseline: None,
                });

                // Mark cells as occupied for future rows
                if rowspan > 1 {
                    for r in 1..rowspan {
                        for c in 0..colspan {
                            self.occupied_cells.insert(
                                (row_index + r, col_index + c),
                                rowspan - r,
                            );
                        }
                    }
                }

                col_index += colspan;
            }
        }

        // Update total column count
        self.column_count = self.column_count.max(col_index);

        self.rows.push(TableRow {
            row_box: row_box.clone(),
            cells,
            index: row_index,
        });
    }

    fn is_occupied(&self, row: usize, col: usize) -> bool {
        self.occupied_cells.contains_key(&(row, col))
    }

    fn build(self) -> TableStructure {
        TableStructure {
            rows: self.rows,
            column_count: self.column_count,
            columns: Vec::new(), // TODO: extract from column boxes
            column_groups: Vec::new(), // TODO: extract from colgroup boxes
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum DisplayType {
    Table,
    TableRowGroup,
    TableRow,
    TableCell,
    TableColumn,
    TableColumnGroup,
    Other,
}

fn get_display_type(box_node: &BoxNode) -> DisplayType {
    // TODO: Check box_node.style.display
    DisplayType::Other
}

/// Get colspan attribute from cell
fn get_colspan(cell_box: &BoxNode) -> usize {
    // TODO: Parse colspan attribute from original element
    // For now, default to 1
    1
}

/// Get rowspan attribute from cell
fn get_rowspan(cell_box: &BoxNode) -> usize {
    // TODO: Parse rowspan attribute from original element
    // For now, default to 1
    1
}
```

## Step 2: Column Width Computation

**File:** `src/layout/table/width.rs` (NEW)

This is the CRITICAL algorithm. Get this right.

```rust
//! Table column width computation
//!
//! Implements both fixed and automatic table layout algorithms
//! Reference: CSS Tables Module Level 3, Sections 6-7

use super::structure::TableStructure;
use crate::layout::formatting_context::Constraints;

/// Computed width for each column
#[derive(Debug, Clone)]
pub struct ColumnWidths {
    pub widths: Vec<f32>,
    pub total_width: f32,
}

/// Fixed table layout algorithm
///
/// Spec: https://www.w3.org/TR/css-tables-3/#fixedwidth-algorithm
///
/// This is MUCH simpler than auto layout. Column widths are determined
/// solely from the first row and any <col> elements.
pub fn compute_fixed_widths(
    structure: &TableStructure,
    constraints: &Constraints,
) -> ColumnWidths {
    let column_count = structure.column_count();
    let mut widths = vec![0.0; column_count];

    // Step 1: Get widths from <col> elements
    // TODO: implement when we support <col>

    // Step 2: Get widths from first row cells
    if let Some(first_row) = structure.rows().first() {
        for cell in &first_row.cells {
            if cell.colspan == 1 {
                // Only consider cells that don't span
                if let Some(width) = get_specified_width(&cell.cell_box) {
                    widths[cell.column_index] = width;
                }
            }
        }
    }

    // Step 3: Distribute table width among columns
    let total_specified: f32 = widths.iter().sum();
    let available_width = get_table_width(constraints);

    if total_specified > 0.0 {
        // Scale to fit available width
        let scale = available_width / total_specified;
        for width in &mut widths {
            *width *= scale;
        }
    } else {
        // No widths specified - divide equally
        let equal_width = available_width / column_count as f32;
        for width in &mut widths {
            *width = equal_width;
        }
    }

    ColumnWidths {
        widths,
        total_width: available_width,
    }
}

/// Automatic table layout algorithm
///
/// Spec: https://www.w3.org/TR/css-tables-3/#autowidth-algorithm
///
/// This is MUCH more complex than fixed layout. Must analyze all cells
/// to determine column widths.
///
/// High-level algorithm:
/// 1. Compute min-content width for each cell
/// 2. Compute max-content width for each cell
/// 3. Compute min-content width for each column (max of cell min-widths)
/// 4. Compute max-content width for each column (max of cell max-widths)
/// 5. Distribute available width among columns using complex rules
pub fn compute_auto_widths(
    structure: &TableStructure,
    constraints: &Constraints,
) -> ColumnWidths {
    let column_count = structure.column_count();

    // Step 1: Compute cell min/max widths
    let cell_widths = compute_cell_widths(structure);

    // Step 2: Compute column min/max widths
    let mut min_widths = vec![0.0; column_count];
    let mut max_widths = vec![0.0; column_count];

    for (row_idx, row) in structure.rows().iter().enumerate() {
        for cell in &row.cells {
            let (cell_min, cell_max) = cell_widths[row_idx][cell.column_index];

            if cell.colspan == 1 {
                // Simple case: cell doesn't span
                min_widths[cell.column_index] =
                    min_widths[cell.column_index].max(cell_min);
                max_widths[cell.column_index] =
                    max_widths[cell.column_index].max(cell_max);
            } else {
                // Complex case: cell spans multiple columns
                // Must distribute cell width among spanned columns
                distribute_spanning_cell_width(
                    cell, cell_min, cell_max,
                    &mut min_widths, &mut max_widths,
                );
            }
        }
    }

    // Step 3: Distribute available width
    let available_width = get_table_width(constraints);
    let widths = distribute_width_to_columns(
        &min_widths,
        &max_widths,
        available_width,
    );

    ColumnWidths {
        widths,
        total_width: available_width,
    }
}

/// Compute min/max widths for all cells
fn compute_cell_widths(structure: &TableStructure) -> Vec<Vec<(f32, f32)>> {
    let mut result = Vec::new();

    for row in structure.rows() {
        let mut row_widths = Vec::new();

        for cell in &row.cells {
            // Compute min-content width (narrowest cell can be)
            let min_width = compute_cell_min_width(&cell.cell_box);

            // Compute max-content width (widest cell wants to be)
            let max_width = compute_cell_max_width(&cell.cell_box);

            row_widths.push((min_width, max_width));
        }

        result.push(row_widths);
    }

    result
}

/// Compute minimum content width for a cell
///
/// This is the width needed to fit the content without breaking
/// (except at forced break points)
fn compute_cell_min_width(cell_box: &BoxNode) -> f32 {
    // TODO: Layout cell content with min-content constraint
    // For now, use a simple heuristic
    100.0 // Placeholder
}

/// Compute maximum content width for a cell
///
/// This is the width the cell would take if given infinite space
fn compute_cell_max_width(cell_box: &BoxNode) -> f32 {
    // TODO: Layout cell content with max-content constraint
    // For now, use a simple heuristic
    300.0 // Placeholder
}

/// Distribute spanning cell width among columns
///
/// This is complex because we need to:
/// 1. Not make columns smaller than their current min
/// 2. Prefer distributing to columns with more flexible width
/// 3. Handle multiple spanning cells (might conflict)
fn distribute_spanning_cell_width(
    cell: &super::structure::TableCell,
    cell_min: f32,
    cell_max: f32,
    column_mins: &mut [f32],
    column_maxs: &mut [f32],
) {
    let start_col = cell.column_index;
    let end_col = start_col + cell.colspan;

    // Sum current widths for spanned columns
    let current_min: f32 = column_mins[start_col..end_col].iter().sum();
    let current_max: f32 = column_maxs[start_col..end_col].iter().sum();

    // If cell needs more space, distribute it
    if cell_min > current_min {
        let extra = cell_min - current_min;
        distribute_extra_width(&mut column_mins[start_col..end_col], extra);
    }

    if cell_max > current_max {
        let extra = cell_max - current_max;
        distribute_extra_width(&mut column_maxs[start_col..end_col], extra);
    }
}

/// Distribute extra width among columns
fn distribute_extra_width(columns: &mut [f32], extra: f32) {
    // Simple strategy: distribute equally
    // TODO: More sophisticated distribution based on current widths
    let per_column = extra / columns.len() as f32;
    for col in columns {
        *col += per_column;
    }
}

/// Distribute available width to columns
///
/// This is the core of the auto layout algorithm.
/// We have min/max constraints for each column and need to
/// allocate available width.
///
/// Algorithm (simplified from spec):
/// 1. If total max-width <= available: give each column its max
/// 2. If total min-width >= available: give each column its min
/// 3. Otherwise: distribute proportionally between min and max
fn distribute_width_to_columns(
    min_widths: &[f32],
    max_widths: &[f32],
    available_width: f32,
) -> Vec<f32> {
    let total_min: f32 = min_widths.iter().sum();
    let total_max: f32 = max_widths.iter().sum();

    if available_width >= total_max {
        // Enough space for all columns at max width
        max_widths.to_vec()
    } else if available_width <= total_min {
        // Not enough space even for min width
        // Scale down min widths proportionally
        let scale = available_width / total_min;
        min_widths.iter().map(|w| w * scale).collect()
    } else {
        // Available width is between min and max
        // Distribute proportionally
        let range = total_max - total_min;
        let excess = available_width - total_min;
        let scale = excess / range;

        min_widths
            .iter()
            .zip(max_widths.iter())
            .map(|(min, max)| min + (max - min) * scale)
            .collect()
    }
}

/// Get table width from constraints or style
fn get_table_width(constraints: &Constraints) -> f32 {
    constraints.available_width.to_option().unwrap_or(800.0)
}

/// Get specified width from cell style
fn get_specified_width(cell_box: &BoxNode) -> Option<f32> {
    // TODO: Check cell_box.style.width
    None
}
```

## Step 3: Tests

**File:** `tests/unit/layout/table_test.rs` (NEW)

```rust
use fastrender::layout::table::*;
use fastrender::tree::box_tree::*;
use fastrender::style::ComputedStyle;
use std::sync::Arc;

#[test]
fn test_table_structure_simple() {
    // Create simple 2x2 table
    let style = Arc::new(ComputedStyle::default());

    let cell1 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    let cell2 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    let row1 = BoxNode::new_block(
        style.clone(),
        FormattingContextType::Block,
        vec![cell1, cell2],
    );

    let cell3 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    let cell4 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    let row2 = BoxNode::new_block(
        style.clone(),
        FormattingContextType::Block,
        vec![cell3, cell4],
    );

    let table = BoxNode::new_block(
        style.clone(),
        FormattingContextType::Table,
        vec![row1, row2],
    );

    let structure = structure::TableStructure::from_box_tree(&table);

    assert_eq!(structure.row_count(), 2);
    assert_eq!(structure.column_count(), 2);
}

#[test]
fn test_column_width_distribution_exact_fit() {
    let min_widths = vec![100.0, 150.0, 200.0];
    let max_widths = vec![200.0, 300.0, 400.0];
    let available = 900.0; // Exactly total max

    let widths = width::distribute_width_to_columns(
        &min_widths,
        &max_widths,
        available,
    );

    assert_eq!(widths, max_widths);
}

#[test]
fn test_column_width_distribution_between_min_max() {
    let min_widths = vec![100.0, 100.0];
    let max_widths = vec![200.0, 200.0];
    let available = 300.0; // Between min (200) and max (400)

    let widths = width::distribute_width_to_columns(
        &min_widths,
        &max_widths,
        available,
    );

    // Should be between min and max for each column
    assert!(widths[0] >= min_widths[0] && widths[0] <= max_widths[0]);
    assert!(widths[1] >= min_widths[1] && widths[1] <= max_widths[1]);

    // Should sum to available
    let total: f32 = widths.iter().sum();
    assert!((total - available).abs() < 0.01);
}
```

## Acceptance Criteria

Table layout is complete when:

- [ ] `TableStructure` correctly analyzes table hierarchy
- [ ] Anonymous box generation works (wraps naked cells)
- [ ] Colspan/rowspan are tracked correctly
- [ ] Fixed layout algorithm passes WPT tests
- [ ] Auto layout algorithm passes WPT tests
- [ ] No element-specific hacks (no checking for class names!)
- [ ] All WPT css/css-tables/* tests pass

## Testing Against Real Tables

Create test cases that currently fail:

```html
<!-- Test 1: Auto width with spanning cell -->
<table>
  <tr>
    <td>Short</td>
    <td>Long content that should make column wide</td>
  </tr>
  <tr>
    <td colspan="2">Spanning cell</td>
  </tr>
</table>

<!-- Test 2: Percentage widths -->
<table style="width: 800px">
  <tr>
    <td style="width: 25%">25%</td>
    <td style="width: 75%">75%</td>
  </tr>
</table>

<!-- Test 3: Rowspan -->
<table>
  <tr>
    <td rowspan="2">Tall cell</td>
    <td>Cell 1</td>
  </tr>
  <tr>
    <td>Cell 2</td>
  </tr>
</table>
```

All of these should render correctly without any element-specific code.

## Common Pitfalls

1. **Don't peek at actual content** - Use min/max content constraints
2. **Don't forget rowspan tracking** - Affects column indices
3. **Don't mix fixed and auto** - Separate algorithms
4. **Don't modify box tree** - Read-only during layout
5. **Don't use heuristics** - Follow spec algorithm exactly

## Next Steps

After table layout works:
- Proceed to [`docs/layout/float.md`](02-float-layout.md)
- Update migration guide with table rendering differences

This implementation will prove that we no longer need hacks!
