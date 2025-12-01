//! Table Layout Algorithm (CSS Tables Module Level 3)
//!
//! This module implements the CSS table layout algorithm for rendering HTML tables.
//! Tables have a unique two-pass layout model that first determines column widths
//! and then row heights.
//!
//! # Table Layout Model
//!
//! CSS tables follow a specific layout model defined in CSS 2.1 Section 17:
//!
//! 1. **Structure Analysis**: Build grid from table, row groups, rows, and cells
//! 2. **Column Width Calculation**: Determine column widths (fixed or auto)
//! 3. **Row Height Calculation**: Determine row heights based on content
//! 4. **Cell Positioning**: Place cells in their grid positions
//!
//! # Layout Algorithms
//!
//! Two table layout algorithms are supported:
//!
//! - **Fixed**: `table-layout: fixed` - Fast, predictable, uses first row only
//! - **Auto**: `table-layout: auto` (default) - Content-aware, requires full scan
//!
//! # References
//!
//! - CSS 2.1 Section 17: Tables
//! - CSS Tables Module Level 3
//! - HTML 5.2 Section 4.9: Tabular data

use crate::geometry::Rect;
use crate::layout::constraints::{AvailableSpace, LayoutConstraints};
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};

// ============================================================================
// Table Structure Types
// ============================================================================

/// Represents the resolved structure of a table
///
/// A TableStructure is built by analyzing the box tree and resolving
/// the grid structure including row spans and column spans.
///
/// # Structure
///
/// ```text
/// +--------------------------------------------------+
/// | TableStructure                                   |
/// |  - columns: Vec<ColumnInfo>                      |
/// |  - rows: Vec<RowInfo>                            |
/// |  - cells: Vec<CellInfo>                          |
/// |  - grid: 2D array of cell references             |
/// +--------------------------------------------------+
/// ```
#[derive(Debug, Clone)]
pub struct TableStructure {
    /// Number of columns in the table
    pub column_count: usize,

    /// Number of rows in the table
    pub row_count: usize,

    /// Column definitions
    pub columns: Vec<ColumnInfo>,

    /// Row definitions
    pub rows: Vec<RowInfo>,

    /// Cell information
    pub cells: Vec<CellInfo>,

    /// Grid mapping: grid[row][col] = cell_index or None for spanned cells
    pub grid: Vec<Vec<Option<usize>>>,

    /// Table border spacing (horizontal, vertical)
    pub border_spacing: (f32, f32),

    /// Whether table uses fixed layout
    pub is_fixed_layout: bool,
}

/// Information about a table column
#[derive(Debug, Clone)]
pub struct ColumnInfo {
    /// Column index (0-based)
    pub index: usize,

    /// Specified width (from col element or first cell)
    pub specified_width: Option<SpecifiedWidth>,

    /// Computed minimum width
    pub min_width: f32,

    /// Computed maximum width
    pub max_width: f32,

    /// Final computed width after distribution
    pub computed_width: f32,
}

impl ColumnInfo {
    /// Creates a new column with no width constraints
    pub fn new(index: usize) -> Self {
        Self {
            index,
            specified_width: None,
            min_width: 0.0,
            max_width: f32::INFINITY,
            computed_width: 0.0,
        }
    }
}

/// Information about a table row
#[derive(Debug, Clone)]
pub struct RowInfo {
    /// Row index (0-based)
    pub index: usize,

    /// Specified height (from row element)
    pub specified_height: Option<SpecifiedHeight>,

    /// Computed minimum height based on cell content
    pub min_height: f32,

    /// Final computed height after distribution
    pub computed_height: f32,

    /// Y position of the row
    pub y_position: f32,
}

impl RowInfo {
    /// Creates a new row with no height constraints
    pub fn new(index: usize) -> Self {
        Self {
            index,
            specified_height: None,
            min_height: 0.0,
            computed_height: 0.0,
            y_position: 0.0,
        }
    }
}

/// Specified width for a column
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpecifiedWidth {
    /// Fixed pixel width
    Fixed(f32),
    /// Percentage of table width
    Percent(f32),
    /// Auto width (content-based)
    Auto,
}

/// Specified height for a row
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpecifiedHeight {
    /// Fixed pixel height
    Fixed(f32),
    /// Percentage of table height
    Percent(f32),
    /// Auto height (content-based)
    Auto,
}

/// Information about a table cell
#[derive(Debug, Clone)]
pub struct CellInfo {
    /// Cell index in the cells array
    pub index: usize,

    /// Starting row (0-based)
    pub row: usize,

    /// Starting column (0-based)
    pub col: usize,

    /// Row span (default 1)
    pub rowspan: usize,

    /// Column span (default 1)
    pub colspan: usize,

    /// Reference to the cell's box node
    pub box_index: usize,

    /// Computed minimum width (intrinsic)
    pub min_width: f32,

    /// Computed maximum width (intrinsic)
    pub max_width: f32,

    /// Computed minimum height
    pub min_height: f32,

    /// Final computed bounds
    pub bounds: Rect,
}

impl CellInfo {
    /// Creates a new cell at the given position
    pub fn new(index: usize, row: usize, col: usize) -> Self {
        Self {
            index,
            row,
            col,
            rowspan: 1,
            colspan: 1,
            box_index: 0,
            min_width: 0.0,
            max_width: f32::INFINITY,
            min_height: 0.0,
            bounds: Rect::ZERO,
        }
    }

    /// Returns true if this cell spans multiple columns
    pub fn is_column_spanning(&self) -> bool {
        self.colspan > 1
    }

    /// Returns true if this cell spans multiple rows
    pub fn is_row_spanning(&self) -> bool {
        self.rowspan > 1
    }
}

impl TableStructure {
    /// Creates a new empty table structure
    pub fn new() -> Self {
        Self {
            column_count: 0,
            row_count: 0,
            columns: Vec::new(),
            rows: Vec::new(),
            cells: Vec::new(),
            grid: Vec::new(),
            border_spacing: (0.0, 0.0),
            is_fixed_layout: false,
        }
    }

    /// Builds table structure from a table box node
    ///
    /// This method analyzes the box tree structure to identify:
    /// - Table rows and row groups
    /// - Table cells with their spans
    /// - Column structure
    ///
    /// # Arguments
    ///
    /// * `table_box` - The root table box node
    ///
    /// # Returns
    ///
    /// A fully resolved TableStructure ready for layout
    pub fn from_box_tree(table_box: &BoxNode) -> Self {
        let mut structure = TableStructure::new();

        // Extract border-spacing from style (simplified - use defaults)
        structure.border_spacing = (2.0, 2.0);

        // Check for fixed layout
        structure.is_fixed_layout = false; // Default to auto

        // Phase 1: Count rows and discover maximum column count
        let mut current_row = 0;
        let mut max_cols = 0;

        // Collect all cells first
        let mut cell_data: Vec<(usize, usize, usize, usize, usize)> = Vec::new(); // (row, col, rowspan, colspan, box_idx)

        for (child_idx, child) in table_box.children.iter().enumerate() {
            match Self::get_table_element_type(child) {
                TableElementType::RowGroup | TableElementType::HeaderGroup | TableElementType::FooterGroup => {
                    // Process rows within the group
                    for (row_child_idx, row_child) in child.children.iter().enumerate() {
                        if matches!(Self::get_table_element_type(row_child), TableElementType::Row) {
                            let row_cells =
                                Self::process_row(row_child, current_row, row_child_idx, &mut structure.grid);
                            for cell in row_cells {
                                let col_end = cell.1 + cell.3;
                                if col_end > max_cols {
                                    max_cols = col_end;
                                }
                                cell_data.push(cell);
                            }
                            current_row += 1;
                        }
                    }
                }
                TableElementType::Row => {
                    let row_cells = Self::process_row(child, current_row, child_idx, &mut structure.grid);
                    for cell in row_cells {
                        let col_end = cell.1 + cell.3;
                        if col_end > max_cols {
                            max_cols = col_end;
                        }
                        cell_data.push(cell);
                    }
                    current_row += 1;
                }
                _ => {}
            }
        }

        structure.row_count = current_row;
        structure.column_count = max_cols;

        // Initialize columns
        for i in 0..structure.column_count {
            structure.columns.push(ColumnInfo::new(i));
        }

        // Initialize rows
        for i in 0..structure.row_count {
            structure.rows.push(RowInfo::new(i));
        }

        // Initialize grid
        structure.grid = vec![vec![None; structure.column_count]; structure.row_count];

        // Create cells and populate grid
        for (idx, (row, col, rowspan, colspan, box_idx)) in cell_data.iter().enumerate() {
            let mut cell = CellInfo::new(idx, *row, *col);
            cell.rowspan = *rowspan;
            cell.colspan = *colspan;
            cell.box_index = *box_idx;

            // Mark grid cells
            for r in *row..(*row + *rowspan).min(structure.row_count) {
                for c in *col..(*col + *colspan).min(structure.column_count) {
                    structure.grid[r][c] = Some(idx);
                }
            }

            structure.cells.push(cell);
        }

        structure
    }

    /// Gets the table element type from a box node
    fn get_table_element_type(node: &BoxNode) -> TableElementType {
        // In a real implementation, we would check the display property
        // For now, we infer from box type and debug info
        if let Some(ref debug_info) = node.debug_info {
            if let Some(ref tag) = debug_info.tag_name {
                let tag_lower = tag.to_lowercase();
                return match tag_lower.as_str() {
                    "table" => TableElementType::Table,
                    "thead" => TableElementType::HeaderGroup,
                    "tbody" => TableElementType::RowGroup,
                    "tfoot" => TableElementType::FooterGroup,
                    "tr" => TableElementType::Row,
                    "td" | "th" => TableElementType::Cell,
                    "caption" => TableElementType::Caption,
                    "col" => TableElementType::Column,
                    "colgroup" => TableElementType::ColumnGroup,
                    _ => TableElementType::Unknown,
                };
            }
        }

        // Fall back to anonymous type checking
        use crate::tree::box_tree::{AnonymousType, BoxType};
        match &node.box_type {
            BoxType::Anonymous(anon) => match anon.anonymous_type {
                AnonymousType::TableWrapper => TableElementType::Table,
                AnonymousType::TableRow => TableElementType::Row,
                AnonymousType::TableCell => TableElementType::Cell,
                _ => TableElementType::Unknown,
            },
            _ => TableElementType::Unknown,
        }
    }

    /// Processes a table row and returns cell information
    fn process_row(
        row: &BoxNode,
        row_idx: usize,
        _box_idx: usize,
        _grid: &mut Vec<Vec<Option<usize>>>,
    ) -> Vec<(usize, usize, usize, usize, usize)> {
        let mut cells = Vec::new();
        let mut col_idx = 0;

        for (cell_idx, cell_child) in row.children.iter().enumerate() {
            if matches!(Self::get_table_element_type(cell_child), TableElementType::Cell) {
                // Extract colspan and rowspan (simplified - would come from attributes)
                let colspan = 1;
                let rowspan = 1;

                cells.push((row_idx, col_idx, rowspan, colspan, cell_idx));
                col_idx += colspan;
            }
        }

        cells
    }

    /// Gets the cell at a grid position, if any
    pub fn get_cell_at(&self, row: usize, col: usize) -> Option<&CellInfo> {
        self.grid
            .get(row)
            .and_then(|r| r.get(col))
            .and_then(|idx| idx.as_ref())
            .and_then(|idx| self.cells.get(*idx))
    }

    /// Returns the total width of border spacing
    pub fn total_horizontal_spacing(&self) -> f32 {
        if self.column_count == 0 {
            return 0.0;
        }
        self.border_spacing.0 * (self.column_count + 1) as f32
    }

    /// Returns the total height of border spacing
    pub fn total_vertical_spacing(&self) -> f32 {
        if self.row_count == 0 {
            return 0.0;
        }
        self.border_spacing.1 * (self.row_count + 1) as f32
    }
}

impl Default for TableStructure {
    fn default() -> Self {
        Self::new()
    }
}

/// Types of table elements for structure analysis
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TableElementType {
    Table,
    HeaderGroup,
    RowGroup,
    FooterGroup,
    Row,
    Cell,
    Caption,
    Column,
    ColumnGroup,
    Unknown,
}

// ============================================================================
// Column Width Algorithms
// ============================================================================

/// Calculates column widths for fixed table layout
///
/// Fixed table layout (table-layout: fixed) determines column widths
/// from the table width and column widths specified in the first row.
/// This is faster than auto layout because it doesn't require measuring
/// all cell content.
///
/// # Algorithm
///
/// 1. Use specified widths from `<col>` elements or first row cells
/// 2. Distribute remaining space equally among auto columns
/// 3. Never shrink columns below specified widths
///
/// # Arguments
///
/// * `structure` - The table structure to update
/// * `available_width` - The available width for the table
pub fn calculate_fixed_layout_widths(structure: &mut TableStructure, available_width: f32) {
    if structure.column_count == 0 {
        return;
    }

    let spacing = structure.total_horizontal_spacing();
    let content_width = (available_width - spacing).max(0.0);

    // Phase 1: Collect specified widths
    let mut specified_total = 0.0;
    let mut auto_count = 0;

    for col in &structure.columns {
        match col.specified_width {
            Some(SpecifiedWidth::Fixed(w)) => specified_total += w,
            Some(SpecifiedWidth::Percent(p)) => specified_total += content_width * p / 100.0,
            _ => auto_count += 1,
        }
    }

    // Phase 2: Calculate auto column width
    let remaining = (content_width - specified_total).max(0.0);
    let auto_width = if auto_count > 0 {
        remaining / auto_count as f32
    } else {
        0.0
    };

    // Phase 3: Assign computed widths
    for col in &mut structure.columns {
        col.computed_width = match col.specified_width {
            Some(SpecifiedWidth::Fixed(w)) => w,
            Some(SpecifiedWidth::Percent(p)) => content_width * p / 100.0,
            _ => auto_width,
        };
    }
}

/// Calculates column widths for automatic table layout
///
/// Auto table layout (table-layout: auto, the default) determines column
/// widths based on cell content. This requires measuring all cells but
/// produces better results for content of varying widths.
///
/// # Algorithm (based on CSS 2.1 Section 17.5.2.2)
///
/// 1. Calculate minimum and maximum widths for each cell
/// 2. For each column, take the maximum of all cell minimums
/// 3. For each column, take the maximum of all cell maximums
/// 4. Distribute available space proportionally
///
/// # Arguments
///
/// * `structure` - The table structure with cell min/max widths populated
/// * `available_width` - The available width for the table
pub fn calculate_auto_layout_widths(structure: &mut TableStructure, available_width: f32) {
    if structure.column_count == 0 {
        return;
    }

    let spacing = structure.total_horizontal_spacing();
    let content_width = (available_width - spacing).max(0.0);

    // Phase 1: Calculate column min/max from non-spanning cells
    for cell in &structure.cells {
        if cell.colspan == 1 {
            let col = &mut structure.columns[cell.col];
            col.min_width = col.min_width.max(cell.min_width);
            col.max_width = col.max_width.min(cell.max_width).max(col.min_width);
        }
    }

    // Phase 2: Distribute spanning cell widths
    for cell in &structure.cells {
        if cell.colspan > 1 {
            // Calculate current total for spanned columns
            let span_start = cell.col;
            let span_end = (cell.col + cell.colspan).min(structure.column_count);

            let current_min: f32 = structure.columns[span_start..span_end]
                .iter()
                .map(|c| c.min_width)
                .sum();

            // If cell needs more, distribute proportionally
            if cell.min_width > current_min {
                let extra = cell.min_width - current_min;
                let per_col = extra / cell.colspan as f32;
                for c in span_start..span_end {
                    structure.columns[c].min_width += per_col;
                }
            }
        }
    }

    // Phase 3: Calculate totals
    let total_min: f32 = structure.columns.iter().map(|c| c.min_width).sum();
    let total_max: f32 = structure.columns.iter().map(|c| c.max_width).sum();

    // Phase 4: Distribute available width
    if content_width <= total_min {
        // Not enough space - use minimum widths
        for col in &mut structure.columns {
            col.computed_width = col.min_width;
        }
    } else if content_width >= total_max {
        // More than enough space - use maximum widths and distribute extra
        let extra = content_width - total_max;
        let per_col = extra / structure.column_count as f32;
        for col in &mut structure.columns {
            col.computed_width = col.max_width + per_col;
        }
    } else {
        // Between min and max - distribute proportionally
        let range = content_width - total_min;
        let total_flex: f32 = structure.columns.iter().map(|c| c.max_width - c.min_width).sum();

        if total_flex > 0.0 {
            for col in &mut structure.columns {
                let flex = col.max_width - col.min_width;
                let extra = range * (flex / total_flex);
                col.computed_width = col.min_width + extra;
            }
        } else {
            // All columns are fixed-width
            let per_col = range / structure.column_count as f32;
            for col in &mut structure.columns {
                col.computed_width = col.min_width + per_col;
            }
        }
    }
}

// ============================================================================
// Row Height Algorithm
// ============================================================================

/// Calculates row heights based on cell content
///
/// Row heights are determined by the tallest cell in each row, taking
/// into account rowspan cells which contribute to multiple rows.
///
/// # Arguments
///
/// * `structure` - The table structure with cell heights populated
/// * `available_height` - Optional height constraint for the table
pub fn calculate_row_heights(structure: &mut TableStructure, _available_height: Option<f32>) {
    if structure.row_count == 0 {
        return;
    }

    // Phase 1: Calculate row heights from non-spanning cells
    for cell in &structure.cells {
        if cell.rowspan == 1 {
            let row = &mut structure.rows[cell.row];
            row.min_height = row.min_height.max(cell.min_height);
        }
    }

    // Phase 2: Distribute spanning cell heights
    for cell in &structure.cells {
        if cell.rowspan > 1 {
            let span_start = cell.row;
            let span_end = (cell.row + cell.rowspan).min(structure.row_count);

            let current_height: f32 = structure.rows[span_start..span_end].iter().map(|r| r.min_height).sum();

            let spacing = structure.border_spacing.1 * (cell.rowspan - 1) as f32;

            if cell.min_height > current_height + spacing {
                let extra = cell.min_height - current_height - spacing;
                let per_row = extra / cell.rowspan as f32;
                for r in span_start..span_end {
                    structure.rows[r].min_height += per_row;
                }
            }
        }
    }

    // Phase 3: Apply specified heights and calculate positions
    let mut y = structure.border_spacing.1;
    for row in &mut structure.rows {
        row.computed_height = match row.specified_height {
            Some(SpecifiedHeight::Fixed(h)) => h.max(row.min_height),
            _ => row.min_height,
        };
        row.y_position = y;
        y += row.computed_height + structure.border_spacing.1;
    }
}

// ============================================================================
// Table Formatting Context
// ============================================================================

/// Table Formatting Context
///
/// Implements the FormattingContext trait for table layout. This handles
/// the complete table layout algorithm including structure analysis,
/// column width calculation, row height calculation, and cell positioning.
///
/// # Usage
///
/// ```rust,ignore
/// use fastrender::layout::table::TableFormattingContext;
/// use fastrender::layout::FormattingContext;
///
/// let tfc = TableFormattingContext::new();
/// let fragment = tfc.layout(table_box, constraints)?;
/// ```
#[derive(Debug, Clone)]
pub struct TableFormattingContext {
    /// Table structure (lazily built during layout)
    structure: Option<TableStructure>,
}

impl TableFormattingContext {
    /// Creates a new table formatting context
    pub fn new() -> Self {
        Self { structure: None }
    }

    /// Gets the table structure, building it if necessary
    pub fn structure(&self) -> Option<&TableStructure> {
        self.structure.as_ref()
    }

    /// Measures cell content for min/max width calculation
    fn measure_cell_intrinsic_widths(&self, _cell_box: &BoxNode, _mode: IntrinsicSizingMode) -> (f32, f32) {
        // Simplified implementation - in reality we'd recursively layout cell content
        // For now, return default minimums
        let min_width = 10.0; // Minimum cell width
        let max_width = 200.0; // Default maximum
        (min_width, max_width)
    }

    /// Measures cell content height given a width constraint
    fn measure_cell_height(&self, _cell_box: &BoxNode, _available_width: f32) -> f32 {
        // Simplified - would recursively layout cell content with width constraint
        20.0 // Default cell height
    }

    /// Creates fragments for all table cells
    fn create_cell_fragments(&self, table_box: &BoxNode, structure: &TableStructure) -> Vec<FragmentNode> {
        let mut fragments = Vec::new();

        // Create a fragment for each cell
        for cell in &structure.cells {
            // Calculate cell bounds
            let x = structure.border_spacing.0
                + structure.columns[..cell.col]
                    .iter()
                    .map(|c| c.computed_width + structure.border_spacing.0)
                    .sum::<f32>();

            let y = structure.rows[cell.row].y_position;

            let width: f32 = structure.columns[cell.col..(cell.col + cell.colspan).min(structure.column_count)]
                .iter()
                .map(|c| c.computed_width)
                .sum::<f32>()
                + structure.border_spacing.0 * (cell.colspan - 1).max(0) as f32;

            let height: f32 = structure.rows[cell.row..(cell.row + cell.rowspan).min(structure.row_count)]
                .iter()
                .map(|r| r.computed_height)
                .sum::<f32>()
                + structure.border_spacing.1 * (cell.rowspan - 1).max(0) as f32;

            let bounds = Rect::from_xywh(x, y, width, height);

            // Get the cell box node
            let cell_box = self.get_cell_box(table_box, cell);

            // Create cell fragment with children
            let cell_children = if let Some(cell_node) = cell_box {
                self.create_cell_content_fragments(cell_node, width, height)
            } else {
                Vec::new()
            };

            let cell_fragment = FragmentNode::new(
                bounds,
                FragmentContent::Block {
                    box_id: Some(cell.index),
                },
                cell_children,
            );

            fragments.push(cell_fragment);
        }

        fragments
    }

    /// Gets the box node for a cell
    fn get_cell_box<'a>(&self, table_box: &'a BoxNode, cell: &CellInfo) -> Option<&'a BoxNode> {
        // Navigate to the cell in the box tree
        let mut row_idx = 0;
        for child in &table_box.children {
            match TableStructure::get_table_element_type(child) {
                TableElementType::RowGroup | TableElementType::HeaderGroup | TableElementType::FooterGroup => {
                    for row_child in &child.children {
                        if row_idx == cell.row {
                            return row_child.children.get(cell.box_index);
                        }
                        row_idx += 1;
                    }
                }
                TableElementType::Row => {
                    if row_idx == cell.row {
                        return child.children.get(cell.box_index);
                    }
                    row_idx += 1;
                }
                _ => {}
            }
        }
        None
    }

    /// Creates fragments for cell content
    fn create_cell_content_fragments(&self, _cell_node: &BoxNode, _width: f32, _height: f32) -> Vec<FragmentNode> {
        // Simplified - would recursively layout cell content
        Vec::new()
    }
}

impl Default for TableFormattingContext {
    fn default() -> Self {
        Self::new()
    }
}

impl FormattingContext for TableFormattingContext {
    /// Performs full table layout
    ///
    /// # Algorithm
    ///
    /// 1. Build table structure from box tree
    /// 2. Measure cell intrinsic sizes
    /// 3. Calculate column widths (fixed or auto algorithm)
    /// 4. Calculate row heights
    /// 5. Position cells and create fragments
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        // Phase 1: Build table structure
        let mut structure = TableStructure::from_box_tree(box_node);

        // Phase 2: Determine available width
        let available_width = match constraints.available_width {
            AvailableSpace::Definite(w) => w,
            AvailableSpace::Indefinite => 10000.0, // Large default for indefinite
            AvailableSpace::MaxContent => 10000.0, // Large default
            AvailableSpace::MinContent => 0.0,
        };

        // Phase 3: Measure cell intrinsic widths
        self.populate_cell_intrinsic_widths(box_node, &mut structure);

        // Phase 4: Calculate column widths
        if structure.is_fixed_layout {
            calculate_fixed_layout_widths(&mut structure, available_width);
        } else {
            calculate_auto_layout_widths(&mut structure, available_width);
        }

        // Phase 5: Calculate row heights
        self.populate_cell_heights(box_node, &mut structure);
        calculate_row_heights(&mut structure, None);

        // Phase 6: Create cell fragments
        let cell_fragments = self.create_cell_fragments(box_node, &structure);

        // Calculate final table size
        let table_width: f32 =
            structure.columns.iter().map(|c| c.computed_width).sum::<f32>() + structure.total_horizontal_spacing();

        let table_height: f32 =
            structure.rows.iter().map(|r| r.computed_height).sum::<f32>() + structure.total_vertical_spacing();

        // Create table fragment
        let table_bounds = Rect::from_xywh(0.0, 0.0, table_width, table_height);
        let table_fragment = FragmentNode::new(table_bounds, FragmentContent::Block { box_id: None }, cell_fragments);

        Ok(table_fragment)
    }

    /// Calculates intrinsic inline size for the table
    fn compute_intrinsic_inline_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError> {
        let mut structure = TableStructure::from_box_tree(box_node);
        self.populate_cell_intrinsic_widths(box_node, &mut structure);

        let width = match mode {
            IntrinsicSizingMode::MinContent => {
                structure.columns.iter().map(|c| c.min_width).sum::<f32>() + structure.total_horizontal_spacing()
            }
            IntrinsicSizingMode::MaxContent => {
                structure.columns.iter().map(|c| c.max_width.min(1000.0)).sum::<f32>()
                    + structure.total_horizontal_spacing()
            }
        };

        Ok(width)
    }
}

impl TableFormattingContext {
    /// Populates cell intrinsic widths by measuring content
    fn populate_cell_intrinsic_widths(&self, table_box: &BoxNode, structure: &mut TableStructure) {
        for cell in &mut structure.cells {
            if let Some(cell_box) = self.get_cell_box_mut(table_box, cell) {
                let (min_w, max_w) = self.measure_cell_intrinsic_widths(cell_box, IntrinsicSizingMode::MinContent);
                cell.min_width = min_w;
                cell.max_width = max_w;
            }
        }
    }

    /// Populates cell heights by measuring content with column widths
    fn populate_cell_heights(&self, table_box: &BoxNode, structure: &mut TableStructure) {
        for cell in &mut structure.cells {
            let cell_width: f32 = structure.columns[cell.col..(cell.col + cell.colspan).min(structure.column_count)]
                .iter()
                .map(|c| c.computed_width)
                .sum::<f32>();

            if let Some(cell_box) = self.get_cell_box_mut(table_box, cell) {
                cell.min_height = self.measure_cell_height(cell_box, cell_width);
            }
        }
    }

    /// Gets mutable reference to cell box (workaround for borrow checker)
    fn get_cell_box_mut<'a>(&self, table_box: &'a BoxNode, cell: &CellInfo) -> Option<&'a BoxNode> {
        self.get_cell_box(table_box, cell)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::display::FormattingContextType;
    use crate::style::ComputedStyle;
    use crate::tree::debug::DebugInfo;
    use std::sync::Arc;

    fn create_test_style() -> Arc<ComputedStyle> {
        Arc::new(ComputedStyle::default())
    }

    fn create_table_cell(content: &str) -> BoxNode {
        let style = create_test_style();
        BoxNode::new_block(style, FormattingContextType::Block, vec![]).with_debug_info(DebugInfo::new(
            Some("td".to_string()),
            None,
            vec![content.to_string()],
        ))
    }

    fn create_table_row(cells: Vec<BoxNode>) -> BoxNode {
        let style = create_test_style();
        BoxNode::new_block(style, FormattingContextType::Block, cells).with_debug_info(DebugInfo::new(
            Some("tr".to_string()),
            None,
            vec![],
        ))
    }

    fn create_simple_table(rows: usize, cols: usize) -> BoxNode {
        let style = create_test_style();
        let mut table_rows = Vec::new();

        for _r in 0..rows {
            let mut cells = Vec::new();
            for c in 0..cols {
                cells.push(create_table_cell(&format!("cell_{}", c)));
            }
            table_rows.push(create_table_row(cells));
        }

        BoxNode::new_block(style, FormattingContextType::Table, table_rows).with_debug_info(DebugInfo::new(
            Some("table".to_string()),
            None,
            vec![],
        ))
    }

    // -------------------------------------------------------------------------
    // TableStructure Tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_table_structure_new() {
        let structure = TableStructure::new();
        assert_eq!(structure.column_count, 0);
        assert_eq!(structure.row_count, 0);
        assert!(structure.cells.is_empty());
        assert!(structure.columns.is_empty());
        assert!(structure.rows.is_empty());
    }

    #[test]
    fn test_table_structure_default() {
        let structure = TableStructure::default();
        assert_eq!(structure.column_count, 0);
        assert!(!structure.is_fixed_layout);
    }

    #[test]
    fn test_table_structure_from_simple_table() {
        let table = create_simple_table(2, 3);
        let structure = TableStructure::from_box_tree(&table);

        assert_eq!(structure.row_count, 2);
        assert_eq!(structure.column_count, 3);
        assert_eq!(structure.cells.len(), 6);
    }

    #[test]
    fn test_table_structure_empty_table() {
        let style = create_test_style();
        let table = BoxNode::new_block(style, FormattingContextType::Table, vec![]).with_debug_info(DebugInfo::new(
            Some("table".to_string()),
            None,
            vec![],
        ));

        let structure = TableStructure::from_box_tree(&table);
        assert_eq!(structure.row_count, 0);
        assert_eq!(structure.column_count, 0);
    }

    #[test]
    fn test_table_structure_single_cell() {
        let table = create_simple_table(1, 1);
        let structure = TableStructure::from_box_tree(&table);

        assert_eq!(structure.row_count, 1);
        assert_eq!(structure.column_count, 1);
        assert_eq!(structure.cells.len(), 1);
    }

    #[test]
    fn test_column_info_new() {
        let col = ColumnInfo::new(5);
        assert_eq!(col.index, 5);
        assert!(col.specified_width.is_none());
        assert_eq!(col.min_width, 0.0);
        assert_eq!(col.computed_width, 0.0);
    }

    #[test]
    fn test_row_info_new() {
        let row = RowInfo::new(3);
        assert_eq!(row.index, 3);
        assert!(row.specified_height.is_none());
        assert_eq!(row.min_height, 0.0);
        assert_eq!(row.y_position, 0.0);
    }

    #[test]
    fn test_cell_info_new() {
        let cell = CellInfo::new(0, 2, 3);
        assert_eq!(cell.index, 0);
        assert_eq!(cell.row, 2);
        assert_eq!(cell.col, 3);
        assert_eq!(cell.rowspan, 1);
        assert_eq!(cell.colspan, 1);
    }

    #[test]
    fn test_cell_spanning_detection() {
        let mut cell = CellInfo::new(0, 0, 0);
        assert!(!cell.is_column_spanning());
        assert!(!cell.is_row_spanning());

        cell.colspan = 2;
        assert!(cell.is_column_spanning());
        assert!(!cell.is_row_spanning());

        cell.rowspan = 3;
        assert!(cell.is_column_spanning());
        assert!(cell.is_row_spanning());
    }

    #[test]
    fn test_horizontal_spacing_calculation() {
        let mut structure = TableStructure::new();
        structure.column_count = 3;
        structure.border_spacing = (5.0, 5.0);

        // 4 gaps for 3 columns: left + between1 + between2 + right
        assert_eq!(structure.total_horizontal_spacing(), 20.0);
    }

    #[test]
    fn test_vertical_spacing_calculation() {
        let mut structure = TableStructure::new();
        structure.row_count = 2;
        structure.border_spacing = (5.0, 10.0);

        // 3 gaps for 2 rows: top + between + bottom
        assert_eq!(structure.total_vertical_spacing(), 30.0);
    }

    #[test]
    fn test_zero_column_spacing() {
        let structure = TableStructure::new();
        assert_eq!(structure.total_horizontal_spacing(), 0.0);
    }

    // -------------------------------------------------------------------------
    // Column Width Algorithm Tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_fixed_layout_equal_distribution() {
        let mut structure = TableStructure::new();
        structure.column_count = 4;
        structure.columns = (0..4).map(ColumnInfo::new).collect();
        structure.border_spacing = (0.0, 0.0);

        calculate_fixed_layout_widths(&mut structure, 400.0);

        for col in &structure.columns {
            assert!((col.computed_width - 100.0).abs() < 0.01);
        }
    }

    #[test]
    fn test_fixed_layout_with_specified_widths() {
        let mut structure = TableStructure::new();
        structure.column_count = 3;
        structure.columns = vec![
            ColumnInfo {
                index: 0,
                specified_width: Some(SpecifiedWidth::Fixed(100.0)),
                min_width: 0.0,
                max_width: f32::INFINITY,
                computed_width: 0.0,
            },
            ColumnInfo {
                index: 1,
                specified_width: None,
                min_width: 0.0,
                max_width: f32::INFINITY,
                computed_width: 0.0,
            },
            ColumnInfo {
                index: 2,
                specified_width: Some(SpecifiedWidth::Fixed(50.0)),
                min_width: 0.0,
                max_width: f32::INFINITY,
                computed_width: 0.0,
            },
        ];
        structure.border_spacing = (0.0, 0.0);

        calculate_fixed_layout_widths(&mut structure, 300.0);

        assert!((structure.columns[0].computed_width - 100.0).abs() < 0.01);
        assert!((structure.columns[1].computed_width - 150.0).abs() < 0.01);
        assert!((structure.columns[2].computed_width - 50.0).abs() < 0.01);
    }

    #[test]
    fn test_fixed_layout_percentage_width() {
        let mut structure = TableStructure::new();
        structure.column_count = 2;
        structure.columns = vec![
            ColumnInfo {
                index: 0,
                specified_width: Some(SpecifiedWidth::Percent(25.0)),
                min_width: 0.0,
                max_width: f32::INFINITY,
                computed_width: 0.0,
            },
            ColumnInfo {
                index: 1,
                specified_width: Some(SpecifiedWidth::Percent(75.0)),
                min_width: 0.0,
                max_width: f32::INFINITY,
                computed_width: 0.0,
            },
        ];
        structure.border_spacing = (0.0, 0.0);

        calculate_fixed_layout_widths(&mut structure, 400.0);

        assert!((structure.columns[0].computed_width - 100.0).abs() < 0.01);
        assert!((structure.columns[1].computed_width - 300.0).abs() < 0.01);
    }

    #[test]
    fn test_auto_layout_minimum_widths() {
        let mut structure = TableStructure::new();
        structure.column_count = 2;
        structure.columns = vec![ColumnInfo::new(0), ColumnInfo::new(1)];
        structure.cells = vec![
            CellInfo {
                index: 0,
                row: 0,
                col: 0,
                rowspan: 1,
                colspan: 1,
                box_index: 0,
                min_width: 50.0,
                max_width: 100.0,
                min_height: 20.0,
                bounds: Rect::ZERO,
            },
            CellInfo {
                index: 1,
                row: 0,
                col: 1,
                rowspan: 1,
                colspan: 1,
                box_index: 1,
                min_width: 80.0,
                max_width: 150.0,
                min_height: 20.0,
                bounds: Rect::ZERO,
            },
        ];
        structure.border_spacing = (0.0, 0.0);

        calculate_auto_layout_widths(&mut structure, 100.0); // Less than total min

        // Should use minimum widths
        assert!(structure.columns[0].computed_width >= 50.0);
        assert!(structure.columns[1].computed_width >= 80.0);
    }

    #[test]
    fn test_auto_layout_maximum_widths() {
        let mut structure = TableStructure::new();
        structure.column_count = 2;
        structure.columns = vec![ColumnInfo::new(0), ColumnInfo::new(1)];
        structure.cells = vec![
            CellInfo {
                index: 0,
                row: 0,
                col: 0,
                rowspan: 1,
                colspan: 1,
                box_index: 0,
                min_width: 50.0,
                max_width: 100.0,
                min_height: 20.0,
                bounds: Rect::ZERO,
            },
            CellInfo {
                index: 1,
                row: 0,
                col: 1,
                rowspan: 1,
                colspan: 1,
                box_index: 1,
                min_width: 80.0,
                max_width: 150.0,
                min_height: 20.0,
                bounds: Rect::ZERO,
            },
        ];
        structure.border_spacing = (0.0, 0.0);

        calculate_auto_layout_widths(&mut structure, 500.0); // More than total max

        // Should use max widths + extra distribution
        assert!(structure.columns[0].computed_width >= 100.0);
        assert!(structure.columns[1].computed_width >= 150.0);
    }

    #[test]
    fn test_auto_layout_spanning_cell() {
        let mut structure = TableStructure::new();
        structure.column_count = 2;
        structure.columns = vec![ColumnInfo::new(0), ColumnInfo::new(1)];
        structure.cells = vec![CellInfo {
            index: 0,
            row: 0,
            col: 0,
            rowspan: 1,
            colspan: 2,
            box_index: 0,
            min_width: 200.0, // Spans both columns
            max_width: 300.0,
            min_height: 20.0,
            bounds: Rect::ZERO,
        }];
        structure.border_spacing = (0.0, 0.0);

        calculate_auto_layout_widths(&mut structure, 200.0);

        let total_width: f32 = structure.columns.iter().map(|c| c.computed_width).sum();
        assert!(total_width >= 200.0);
    }

    #[test]
    fn test_auto_layout_empty_table() {
        let mut structure = TableStructure::new();
        structure.column_count = 0;

        calculate_auto_layout_widths(&mut structure, 400.0);
        // Should not panic
    }

    // -------------------------------------------------------------------------
    // Row Height Algorithm Tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_row_height_from_cells() {
        let mut structure = TableStructure::new();
        structure.row_count = 2;
        structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
        structure.cells = vec![
            CellInfo {
                index: 0,
                row: 0,
                col: 0,
                rowspan: 1,
                colspan: 1,
                box_index: 0,
                min_width: 50.0,
                max_width: 100.0,
                min_height: 30.0,
                bounds: Rect::ZERO,
            },
            CellInfo {
                index: 1,
                row: 0,
                col: 1,
                rowspan: 1,
                colspan: 1,
                box_index: 1,
                min_width: 80.0,
                max_width: 150.0,
                min_height: 50.0, // Taller cell
                bounds: Rect::ZERO,
            },
            CellInfo {
                index: 2,
                row: 1,
                col: 0,
                rowspan: 1,
                colspan: 1,
                box_index: 0,
                min_width: 50.0,
                max_width: 100.0,
                min_height: 25.0,
                bounds: Rect::ZERO,
            },
        ];
        structure.border_spacing = (0.0, 0.0);

        calculate_row_heights(&mut structure, None);

        // Row 0 should use the taller cell (50.0)
        assert!((structure.rows[0].computed_height - 50.0).abs() < 0.01);
        // Row 1 should use its single cell (25.0)
        assert!((structure.rows[1].computed_height - 25.0).abs() < 0.01);
    }

    #[test]
    fn test_row_height_spanning_cell() {
        let mut structure = TableStructure::new();
        structure.row_count = 2;
        structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
        structure.cells = vec![CellInfo {
            index: 0,
            row: 0,
            col: 0,
            rowspan: 2,
            colspan: 1,
            box_index: 0,
            min_width: 50.0,
            max_width: 100.0,
            min_height: 100.0, // Spans both rows
            bounds: Rect::ZERO,
        }];
        structure.border_spacing = (0.0, 0.0);

        calculate_row_heights(&mut structure, None);

        let total_height: f32 = structure.rows.iter().map(|r| r.computed_height).sum();
        assert!(total_height >= 100.0);
    }

    #[test]
    fn test_row_position_calculation() {
        let mut structure = TableStructure::new();
        structure.row_count = 3;
        structure.rows = vec![RowInfo::new(0), RowInfo::new(1), RowInfo::new(2)];
        structure.rows[0].min_height = 30.0;
        structure.rows[1].min_height = 40.0;
        structure.rows[2].min_height = 50.0;
        structure.border_spacing = (0.0, 10.0);

        calculate_row_heights(&mut structure, None);

        // Verify positions
        assert!((structure.rows[0].y_position - 10.0).abs() < 0.01);
        assert!((structure.rows[1].y_position - 50.0).abs() < 0.01); // 10 + 30 + 10
        assert!((structure.rows[2].y_position - 100.0).abs() < 0.01); // 50 + 40 + 10
    }

    #[test]
    fn test_row_height_empty_table() {
        let mut structure = TableStructure::new();
        structure.row_count = 0;

        calculate_row_heights(&mut structure, None);
        // Should not panic
    }

    // -------------------------------------------------------------------------
    // TableFormattingContext Tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_table_formatting_context_new() {
        let tfc = TableFormattingContext::new();
        assert!(tfc.structure().is_none());
    }

    #[test]
    fn test_table_formatting_context_default() {
        let tfc = TableFormattingContext::default();
        assert!(tfc.structure().is_none());
    }

    #[test]
    fn test_table_layout_simple() {
        let tfc = TableFormattingContext::new();
        let table = create_simple_table(2, 2);
        let constraints = LayoutConstraints::definite_width(400.0);

        let result = tfc.layout(&table, &constraints);
        assert!(result.is_ok());

        let fragment = result.unwrap();
        assert!(fragment.bounds.width() > 0.0);
        assert!(fragment.bounds.height() > 0.0);
    }

    #[test]
    fn test_table_layout_empty() {
        let tfc = TableFormattingContext::new();
        let style = create_test_style();
        let table = BoxNode::new_block(style, FormattingContextType::Table, vec![]).with_debug_info(DebugInfo::new(
            Some("table".to_string()),
            None,
            vec![],
        ));
        let constraints = LayoutConstraints::definite_width(400.0);

        let result = tfc.layout(&table, &constraints);
        assert!(result.is_ok());
    }

    #[test]
    fn test_table_intrinsic_sizes_min_content() {
        let tfc = TableFormattingContext::new();
        let table = create_simple_table(2, 3);

        let result = tfc.compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MinContent);
        assert!(result.is_ok());

        let width = result.unwrap();
        assert!(width >= 0.0);
    }

    #[test]
    fn test_table_intrinsic_sizes_max_content() {
        let tfc = TableFormattingContext::new();
        let table = create_simple_table(2, 3);

        let result = tfc.compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MaxContent);
        assert!(result.is_ok());

        let width = result.unwrap();
        assert!(width >= 0.0);
    }

    // -------------------------------------------------------------------------
    // Edge Case Tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_single_column_table() {
        let table = create_simple_table(5, 1);
        let structure = TableStructure::from_box_tree(&table);

        assert_eq!(structure.column_count, 1);
        assert_eq!(structure.row_count, 5);
    }

    #[test]
    fn test_single_row_table() {
        let table = create_simple_table(1, 5);
        let structure = TableStructure::from_box_tree(&table);

        assert_eq!(structure.row_count, 1);
        assert_eq!(structure.column_count, 5);
    }

    #[test]
    fn test_large_table() {
        let table = create_simple_table(100, 50);
        let structure = TableStructure::from_box_tree(&table);

        assert_eq!(structure.row_count, 100);
        assert_eq!(structure.column_count, 50);
        assert_eq!(structure.cells.len(), 5000);
    }

    #[test]
    fn test_zero_available_width() {
        let mut structure = TableStructure::new();
        structure.column_count = 3;
        structure.columns = (0..3).map(ColumnInfo::new).collect();
        structure.border_spacing = (0.0, 0.0);

        calculate_auto_layout_widths(&mut structure, 0.0);

        // All columns should have zero or minimum width
        for col in &structure.columns {
            assert!(col.computed_width >= 0.0);
        }
    }

    #[test]
    fn test_negative_spacing_prevention() {
        let mut structure = TableStructure::new();
        structure.column_count = 3;
        structure.columns = (0..3).map(ColumnInfo::new).collect();
        structure.border_spacing = (10.0, 10.0);

        // Available width less than spacing
        calculate_fixed_layout_widths(&mut structure, 20.0);

        for col in &structure.columns {
            assert!(col.computed_width >= 0.0);
        }
    }
}
