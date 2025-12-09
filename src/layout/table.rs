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

use crate::geometry::{Point, Rect};
use crate::layout::constraints::{AvailableSpace, LayoutConstraints};
use crate::layout::contexts::block::BlockFormattingContext;
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::contexts::table::column_distribution::{
    distribute_spanning_cell_width, ColumnConstraints, ColumnDistributor, DistributionMode,
};
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::style::types::{BorderCollapse, TableLayout, VerticalAlign};
use crate::style::values::LengthUnit;
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

    /// Grid mapping: `grid[row][col]` = cell_index or None for spanned cells
    pub grid: Vec<Vec<Option<usize>>>,

    /// Table border spacing (horizontal, vertical)
    pub border_spacing: (f32, f32),

    /// Border collapsing model
    pub border_collapse: BorderCollapse,

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
            // CSS 2.1 initial value is 0; UA stylesheet may override (e.g., 2px 2px).
            border_spacing: (0.0, 0.0),
            border_collapse: BorderCollapse::Separate,
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

        // Extract border model and spacing from style (UA stylesheet can override)
        structure.border_collapse = table_box.style.border_collapse;
        structure.border_spacing = match table_box.style.border_collapse {
            BorderCollapse::Collapse => (0.0, 0.0),
            BorderCollapse::Separate => resolve_border_spacing(&table_box.style),
        };

        // Check for fixed layout
        structure.is_fixed_layout = matches!(table_box.style.table_layout, TableLayout::Fixed);

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

        // Apply column widths from <col>/<colgroup> if present
        let mut col_cursor = 0;
        for child in &table_box.children {
            match Self::get_table_element_type(child) {
                TableElementType::Column => {
                    if let Some(width) = &child.style.width {
                        if let Some(col) = structure.columns.get_mut(col_cursor) {
                            col.specified_width = Some(Self::length_to_specified_width(width));
                        }
                    }
                    col_cursor += 1;
                }
                TableElementType::ColumnGroup => {
                    // Apply group width to contained columns (or next column if none)
                    if !child.children.is_empty() {
                        for group_child in &child.children {
                            if Self::get_table_element_type(group_child) == TableElementType::Column {
                                if let Some(width) = &group_child.style.width {
                                    if let Some(col) = structure.columns.get_mut(col_cursor) {
                                        col.specified_width = Some(Self::length_to_specified_width(width));
                                    }
                                }
                                col_cursor += 1;
                            }
                        }
                    } else if let Some(width) = &child.style.width {
                        if let Some(col) = structure.columns.get_mut(col_cursor) {
                            col.specified_width = Some(Self::length_to_specified_width(width));
                        }
                        col_cursor += 1;
                    }
                }
                _ => {}
            }
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
        use crate::style::display::Display;
        use crate::tree::box_tree::{AnonymousType, BoxType};

        // Prefer computed display over tag hints
        match node.style.display {
            Display::Table | Display::InlineTable => return TableElementType::Table,
            Display::TableRow => return TableElementType::Row,
            Display::TableCell => return TableElementType::Cell,
            Display::TableCaption => return TableElementType::Caption,
            Display::TableRowGroup => return TableElementType::RowGroup,
            Display::TableHeaderGroup => return TableElementType::HeaderGroup,
            Display::TableFooterGroup => return TableElementType::FooterGroup,
            Display::TableColumn => return TableElementType::Column,
            Display::TableColumnGroup => return TableElementType::ColumnGroup,
            _ => {}
        }

        // Fall back to anonymous boxes
        if let BoxType::Anonymous(anon) = &node.box_type {
            return match anon.anonymous_type {
                AnonymousType::TableWrapper => TableElementType::Table,
                AnonymousType::TableRow => TableElementType::Row,
                AnonymousType::TableCell => TableElementType::Cell,
                AnonymousType::TableRowGroup => TableElementType::RowGroup,
                _ => TableElementType::Unknown,
            };
        }

        // As a last resort, use debug info tags if present
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

        TableElementType::Unknown
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
                // Extract colspan and rowspan from debug info
                let (colspan, rowspan) = if let Some(ref debug_info) = cell_child.debug_info {
                    (debug_info.colspan, debug_info.rowspan)
                } else {
                    (1, 1)
                };

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

    fn length_to_specified_width(length: &crate::style::values::Length) -> SpecifiedWidth {
        use crate::style::values::LengthUnit;
        match length.unit {
            LengthUnit::Percent => SpecifiedWidth::Percent(length.value),
            _ => SpecifiedWidth::Fixed(length.to_px()),
        }
    }
}

impl Default for TableStructure {
    fn default() -> Self {
        Self::new()
    }
}

fn resolve_border_spacing_length(length: &crate::style::values::Length, font_size: f32) -> f32 {
    match length.unit {
        LengthUnit::Em | LengthUnit::Rem => length.value * font_size,
        _ if length.unit.is_absolute() => length.to_px(),
        _ => 0.0,
    }
}

fn resolve_border_spacing(style: &crate::style::ComputedStyle) -> (f32, f32) {
    let font_size = style.font_size;
    (
        resolve_border_spacing_length(&style.border_spacing_horizontal, font_size),
        resolve_border_spacing_length(&style.border_spacing_vertical, font_size),
    )
}

fn resolve_length_against(
    length: &crate::style::values::Length,
    font_size: f32,
    containing_width: Option<f32>,
) -> Option<f32> {
    match length.unit {
        LengthUnit::Percent => containing_width.map(|w| (length.value / 100.0) * w),
        LengthUnit::Em | LengthUnit::Rem => Some(length.value * font_size),
        _ if length.unit.is_absolute() => Some(length.to_px()),
        _ => None,
    }
}

fn horizontal_padding_and_borders(style: &crate::style::ComputedStyle) -> f32 {
    // Percentages would need containing block; treat them as zero for intrinsic measurement fallback.
    let resolve_abs = |l: &crate::style::values::Length| match l.unit {
        LengthUnit::Percent => 0.0,
        _ if l.unit.is_absolute() => l.to_px(),
        _ => l.value,
    };

    resolve_abs(&style.padding_left)
        + resolve_abs(&style.padding_right)
        + resolve_abs(&style.border_left_width)
        + resolve_abs(&style.border_right_width)
}

fn horizontal_padding(style: &crate::style::ComputedStyle) -> f32 {
    let resolve_abs = |l: &crate::style::values::Length| match l.unit {
        LengthUnit::Percent => 0.0,
        _ if l.unit.is_absolute() => l.to_px(),
        _ => l.value,
    };

    resolve_abs(&style.padding_left) + resolve_abs(&style.padding_right)
}

fn find_first_baseline(fragment: &FragmentNode, parent_offset: f32) -> Option<f32> {
    let origin = parent_offset + fragment.bounds.y();
    match &fragment.content {
        FragmentContent::Line { baseline } => return Some(origin + *baseline),
        FragmentContent::Text { baseline_offset, .. } => return Some(origin + *baseline_offset),
        _ => {}
    }

    for child in &fragment.children {
        if let Some(b) = find_first_baseline(child, origin) {
            return Some(b);
        }
    }

    None
}

fn cell_baseline(fragment: &FragmentNode) -> Option<f32> {
    find_first_baseline(fragment, 0.0)
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

#[derive(Debug, Clone)]
struct RowMetrics {
    height: f32,
    baseline_top: f32,
    baseline_bottom: f32,
    has_baseline: bool,
    max_cell_height: f32,
}

impl RowMetrics {
    fn new(height: f32) -> Self {
        Self {
            height,
            baseline_top: 0.0,
            baseline_bottom: 0.0,
            has_baseline: false,
            max_cell_height: height,
        }
    }

    fn baseline_height(&self) -> f32 {
        if self.has_baseline {
            self.baseline_top + self.baseline_bottom
        } else {
            0.0
        }
    }
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
        // More than enough space - use maximum widths
        // Give extra space proportionally to columns based on their max_width
        // This keeps narrow columns (rank, arrow) small while content columns grow
        let extra = content_width - total_max;
        if total_max > 0.0 {
            for col in &mut structure.columns {
                let proportion = col.max_width / total_max;
                col.computed_width = col.max_width + extra * proportion;
            }
        } else {
            // Fallback to equal distribution if no max widths
            let per_col = extra / structure.column_count as f32;
            for col in &mut structure.columns {
                col.computed_width = col.max_width + per_col;
            }
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
/// use fastrender::FormattingContext;
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

    /// Measures cell intrinsic widths using inline min/max content rules
    fn measure_cell_intrinsic_widths(&self, cell_box: &BoxNode, border_collapse: BorderCollapse) -> (f32, f32) {
        let fc_type = cell_box
            .formatting_context()
            .unwrap_or(crate::style::display::FormattingContextType::Block);
        let fc = FormattingContextFactory::new().create(fc_type);

        let mut min = fc
            .compute_intrinsic_inline_size(cell_box, IntrinsicSizingMode::MinContent)
            .unwrap_or(0.0);
        let mut max = fc
            .compute_intrinsic_inline_size(cell_box, IntrinsicSizingMode::MaxContent)
            .unwrap_or(min);

        // Add horizontal padding (and borders in separate model) to intrinsic widths
        let style = &cell_box.style;
        let padding_and_borders = match border_collapse {
            BorderCollapse::Separate => horizontal_padding_and_borders(style),
            BorderCollapse::Collapse => {
                // Collapsed borders don't add to box width; include padding only.
                horizontal_padding(style)
            }
        };
        min += padding_and_borders;
        max += padding_and_borders;

        (min.max(0.0), max.max(min))
    }

    /// Populates column constraints from cell intrinsic widths and explicit widths
    fn populate_column_constraints(
        &self,
        table_box: &BoxNode,
        structure: &TableStructure,
        constraints: &mut [ColumnConstraints],
        mode: DistributionMode,
    ) {
        for cell in &structure.cells {
            if matches!(mode, DistributionMode::Fixed) && cell.row > 0 {
                // Fixed layout only inspects the first row.
                continue;
            }
            let Some(cell_box) = self.get_cell_box(table_box, cell) else {
                continue;
            };
            let (min_w, max_w) = match mode {
                DistributionMode::Fixed => (0.0, 0.0), // content is ignored in fixed layout
                _ => self.measure_cell_intrinsic_widths(cell_box, structure.border_collapse),
            };

            if cell.colspan == 1 {
                if let Some(width) = &cell_box.style.width {
                    match width.unit {
                        LengthUnit::Percent => {
                            let col = &mut constraints[cell.col];
                            col.set_percentage(width.value);
                            col.min_width = col.min_width.max(min_w);
                            col.max_width = col.max_width.max(max_w);
                            continue;
                        }
                        _ => {
                            let px = width.to_px().max(min_w);
                            let col = &mut constraints[cell.col];
                            col.fixed_width = Some(px);
                            col.min_width = col.min_width.max(min_w);
                            col.max_width = col.max_width.max(px.max(max_w));
                            continue;
                        }
                    }
                }

                let col = &mut constraints[cell.col];
                col.min_width = col.min_width.max(min_w);
                col.max_width = col.max_width.max(max_w);
            } else {
                let start = cell.col;
                let end = (cell.col + cell.colspan).min(constraints.len());
                distribute_spanning_cell_width(constraints, start, end, min_w, max_w);
            }
        }

        // Apply specified widths from column info (<col>/<colgroup>)
        for (i, col_info) in structure.columns.iter().enumerate() {
            let Some(constraint) = constraints.get_mut(i) else { continue };
            if let Some(specified) = col_info.specified_width {
                match specified {
                    SpecifiedWidth::Fixed(px) => {
                        constraint.fixed_width = Some(px.max(constraint.min_width));
                        constraint.is_flexible = false;
                    }
                    SpecifiedWidth::Percent(pct) => {
                        constraint.set_percentage(pct);
                    }
                    SpecifiedWidth::Auto => {}
                }
            }
        }
    }

    /// Layout a single cell with the given width
    fn layout_cell(&self, cell_box: &BoxNode, cell_width: f32) -> Result<FragmentNode, LayoutError> {
        let bfc = BlockFormattingContext::new();
        let constraints = LayoutConstraints::definite_width(cell_width.max(0.0));
        bfc.layout(cell_box, &constraints)
    }

    /// Gets the box node for a cell
    fn get_cell_box<'a>(&self, table_box: &'a BoxNode, cell: &CellInfo) -> Option<&'a BoxNode> {
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
}

impl Default for TableFormattingContext {
    fn default() -> Self {
        Self::new()
    }
}

impl FormattingContext for TableFormattingContext {
    /// Performs full table layout following CSS table algorithms (auto layout)
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        let structure = TableStructure::from_box_tree(box_node);

        if structure.column_count == 0 || structure.row_count == 0 {
            let bounds = Rect::from_xywh(0.0, 0.0, 0.0, 0.0);
            return Ok(FragmentNode::new_with_style(
                bounds,
                FragmentContent::Block { box_id: None },
                vec![],
                box_node.style.clone(),
            ));
        }

        let containing_width = match constraints.available_width {
            AvailableSpace::Definite(w) => Some(w),
            _ => None,
        };

        // Honor explicit table width if present.
        let table_width = box_node
            .style
            .width
            .as_ref()
            .and_then(|len| resolve_length_against(len, box_node.style.font_size, containing_width))
            .or(containing_width);

        let mut column_constraints: Vec<ColumnConstraints> = (0..structure.column_count)
            .map(|_| ColumnConstraints::new(0.0, 0.0))
            .collect();
        let mode = if structure.is_fixed_layout {
            DistributionMode::Fixed
        } else {
            DistributionMode::Auto
        };
        self.populate_column_constraints(box_node, &structure, &mut column_constraints, mode);

        let spacing = structure.total_horizontal_spacing();
        let available_content = match (table_width, constraints.available_width) {
            (Some(w), _) => (w - spacing).max(0.0),
            (None, AvailableSpace::Definite(w)) => (w - spacing).max(0.0),
            (None, AvailableSpace::MinContent) => 0.0,
            (None, AvailableSpace::MaxContent) | (None, AvailableSpace::Indefinite) => {
                column_constraints.iter().map(|c| c.max_width).sum::<f32>()
            }
        };

        let distributor = ColumnDistributor::new(mode).with_min_column_width(0.0);
        let distribution = distributor.distribute(&column_constraints, available_content);
        let mut col_widths = if distribution.widths.len() == structure.column_count {
            distribution.widths
        } else {
            vec![0.0; structure.column_count]
        };

        // Fallback: if all columns computed to zero, distribute available space equally
        if col_widths.iter().all(|w| *w == 0.0) && available_content > 0.0 && !col_widths.is_empty() {
            let per = available_content / structure.column_count as f32;
            col_widths = vec![per; structure.column_count];
        }

        let mut fragments = Vec::new();
        let h_spacing = structure.border_spacing.0;
        let v_spacing = structure.border_spacing.1;

        struct LaidOutCell<'a> {
            cell: &'a CellInfo,
            fragment: FragmentNode,
            vertical_align: VerticalAlign,
            baseline: Option<f32>,
            height: f32,
        }

        // Layout all cells to obtain their fragments and measure heights, then distribute row heights with rowspans.
        let mut laid_out_cells: Vec<LaidOutCell> = Vec::new();
        for cell in &structure.cells {
            let span_end = (cell.col + cell.colspan).min(col_widths.len());
            let width: f32 = col_widths[cell.col..span_end].iter().sum::<f32>()
                + h_spacing * (cell.colspan.saturating_sub(1) as f32);

            if let Some(cell_box) = self.get_cell_box(box_node, cell) {
                if let Ok(fragment) = self.layout_cell(cell_box, width) {
                    let height = fragment.bounds.height();
                    let baseline = cell_baseline(&fragment);
                    laid_out_cells.push(LaidOutCell {
                        cell,
                        fragment,
                        vertical_align: cell_box.style.vertical_align,
                        baseline,
                        height,
                    });
                }
            }
        }

        // Compute row heights, accounting for rowspans and vertical spacing.
        let mut row_heights = vec![0.0f32; structure.row_count];
        for laid in &laid_out_cells {
            if laid.cell.rowspan == 1 {
                row_heights[laid.cell.row] = row_heights[laid.cell.row].max(laid.height);
            } else {
                let span_start = laid.cell.row;
                let span_end = (laid.cell.row + laid.cell.rowspan).min(structure.row_count);
                let spacing_total = v_spacing * (laid.cell.rowspan.saturating_sub(1) as f32);
                let current: f32 = row_heights[span_start..span_end].iter().sum();
                if laid.height > current + spacing_total {
                    let extra = laid.height - current - spacing_total;
                    let per_row = extra / (span_end - span_start) as f32;
                    for r in span_start..span_end {
                        row_heights[r] += per_row;
                    }
                }
            }
        }

        let mut row_metrics: Vec<RowMetrics> = row_heights.iter().map(|h| RowMetrics::new(*h)).collect();

        for laid in &laid_out_cells {
            if laid.cell.rowspan != 1 {
                continue;
            }

            let row = &mut row_metrics[laid.cell.row];
            row.max_cell_height = row.max_cell_height.max(laid.height);

            if laid.vertical_align.is_baseline_relative() {
                let baseline = laid.baseline.unwrap_or(laid.height);
                let bottom = (laid.height - baseline).max(0.0);
                row.has_baseline = true;
                row.baseline_top = row.baseline_top.max(baseline);
                row.baseline_bottom = row.baseline_bottom.max(bottom);
            }
        }

        for row in &mut row_metrics {
            let baseline_height = row.baseline_height();
            let max_required = row.max_cell_height.max(baseline_height);
            if max_required > row.height {
                row.height = max_required;
            }
            if row.height <= 0.0 {
                row.height = 1.0;
            }
        }

        // Compute row offsets
        let mut row_offsets = Vec::with_capacity(structure.row_count);
        let mut y = v_spacing;
        for row in &row_metrics {
            row_offsets.push(y);
            y += row.height + v_spacing;
        }

        // Position fragments with vertical alignment within their row block
        for laid in laid_out_cells {
            let cell = laid.cell;
            let mut x = h_spacing;
            for col_idx in 0..cell.col {
                x += col_widths[col_idx] + h_spacing;
            }

            let row_start = cell.row;
            let span_end = (cell.row + cell.rowspan).min(row_metrics.len());
            let spanned_height: f32 = row_metrics[row_start..span_end].iter().map(|r| r.height).sum::<f32>()
                + v_spacing * cell.rowspan.saturating_sub(1) as f32;

            let y_offset = match laid.vertical_align {
                VerticalAlign::Top => 0.0,
                VerticalAlign::Bottom => (spanned_height - laid.height).max(0.0),
                VerticalAlign::Middle => ((spanned_height - laid.height) / 2.0).max(0.0),
                _ => {
                    let baseline = laid.baseline.unwrap_or(laid.height);
                    let row_base = row_metrics
                        .get(row_start)
                        .map(|r| if r.has_baseline { r.baseline_top } else { r.height })
                        .unwrap_or(0.0);
                    (row_base - baseline).max(0.0)
                }
            };

            let y = row_offsets.get(row_start).cloned().unwrap_or(0.0) + y_offset;
            fragments.push(laid.fragment.translate(Point::new(x, y)));
        }

        let total_width: f32 = col_widths.iter().sum::<f32>() + spacing;
        let total_height = if structure.row_count > 0 {
            row_offsets
                .last()
                .map(|start| start + row_metrics.last().map(|r| r.height).unwrap_or(0.0) + v_spacing)
                .unwrap_or(0.0)
        } else {
            0.0
        };
        let table_bounds = Rect::from_xywh(0.0, 0.0, total_width.max(0.0), total_height);

        Ok(FragmentNode::new_with_style(
            table_bounds,
            FragmentContent::Block { box_id: None },
            fragments,
            box_node.style.clone(),
        ))
    }

    /// Calculates intrinsic inline size for the table
    fn compute_intrinsic_inline_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError> {
        let structure = TableStructure::from_box_tree(box_node);
        let mut column_constraints: Vec<ColumnConstraints> = (0..structure.column_count)
            .map(|_| ColumnConstraints::new(0.0, 0.0))
            .collect();
        self.populate_column_constraints(
            box_node,
            &structure,
            &mut column_constraints,
            DistributionMode::Auto,
        );

        let spacing = structure.total_horizontal_spacing();
        let width = match mode {
            IntrinsicSizingMode::MinContent => column_constraints.iter().map(|c| c.min_width).sum::<f32>() + spacing,
            IntrinsicSizingMode::MaxContent => column_constraints.iter().map(|c| c.max_width).sum::<f32>() + spacing,
        };

        Ok(width.max(0.0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::layout::constraints::LayoutConstraints;
    use crate::style::display::FormattingContextType;
    use crate::style::display::Display;
    use crate::style::types::{TableLayout, VerticalAlign};
    use crate::style::values::Length;
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
    fn test_table_structure_respects_table_layout_property() {
        let mut style = ComputedStyle::default();
        style.display = Display::Table;
        style.table_layout = TableLayout::Fixed;
        let table = BoxNode::new_block(Arc::new(style), FormattingContextType::Table, vec![]).with_debug_info(
            DebugInfo::new(Some("table".to_string()), None, vec![]),
        );

        let structure = TableStructure::from_box_tree(&table);
        assert!(structure.is_fixed_layout);
    }

    #[test]
    fn test_table_respects_explicit_width() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.width = Some(Length::px(300.0));
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        let cell_a = BoxNode::new_block(Arc::new(cell_style.clone()), FormattingContextType::Block, vec![]);
        let cell_b = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell_a, cell_b]);

        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let tfc = TableFormattingContext::new();
        let constraints = LayoutConstraints::definite_width(800.0);
        let fragment = tfc.layout(&table, &constraints).expect("table layout");

        assert!((fragment.bounds.width() - 300.0).abs() < 0.1);
        assert_eq!(fragment.children.len(), 2);
        assert!((fragment.children[0].bounds.width() - 150.0).abs() < 0.1);
        assert!((fragment.children[1].bounds.width() - 150.0).abs() < 0.1);
    }

    #[test]
    fn test_fixed_layout_uses_first_row_only() {
        // First row specifies widths; second row should not affect fixed layout distribution.
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.table_layout = TableLayout::Fixed;

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell1_style = ComputedStyle::default();
        cell1_style.display = Display::TableCell;
        cell1_style.width = Some(Length::percent(50.0));
        let cell1 = BoxNode::new_block(Arc::new(cell1_style), FormattingContextType::Block, vec![]);

        let cell2_style = ComputedStyle::default();
        let cell2 = BoxNode::new_block(Arc::new(cell2_style), FormattingContextType::Block, vec![]);

        let first_row = BoxNode::new_block(
            Arc::new(row_style.clone()),
            FormattingContextType::Block,
            vec![cell1, cell2],
        );

        // Second row with explicit widths should be ignored in fixed layout sizing.
        let mut second_cell_style = ComputedStyle::default();
        second_cell_style.display = Display::TableCell;
        second_cell_style.width = Some(Length::px(400.0));
        let second_cell = BoxNode::new_block(Arc::new(second_cell_style), FormattingContextType::Block, vec![]);
        let second_row =
            BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![second_cell.clone()]);

        let table = BoxNode::new_block(
            Arc::new(table_style),
            FormattingContextType::Table,
            vec![first_row, second_row],
        );

        let tfc = TableFormattingContext::new();
        let constraints = LayoutConstraints::definite_width(200.0);
        let fragment = tfc.layout(&table, &constraints).expect("table layout");

        // Expect first column to take ~50% of available space regardless of second row width.
        let widths: Vec<f32> = fragment.children.iter().map(|c| c.bounds.width()).collect();
        assert_eq!(widths.len(), 2);
        assert!((widths[0] - 100.0).abs() < 0.1);
        assert!((widths[1] - 100.0).abs() < 0.1);
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
    fn test_vertical_align_bottom_positions_cell_at_row_end() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut tall_style = ComputedStyle::default();
        tall_style.display = Display::TableCell;
        tall_style.height = Some(Length::px(100.0));

        let tall_cell = BoxNode::new_block(Arc::new(tall_style), FormattingContextType::Block, vec![]);

        let mut short_style = ComputedStyle::default();
        short_style.display = Display::TableCell;
        short_style.height = Some(Length::px(20.0));
        short_style.vertical_align = VerticalAlign::Bottom;

        let short_cell = BoxNode::new_block(Arc::new(short_style), FormattingContextType::Block, vec![]);
        let row = BoxNode::new_block(
            Arc::new(row_style),
            FormattingContextType::Block,
            vec![tall_cell, short_cell],
        );
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let tfc = TableFormattingContext::new();
        let constraints = LayoutConstraints::definite_width(200.0);
        let fragment = tfc.layout(&table, &constraints).expect("table layout");

        assert_eq!(fragment.children.len(), 2);
        let first = &fragment.children[0];
        let second = &fragment.children[1];

        assert!((first.bounds.y() - 0.0).abs() < 0.01);
        assert!((second.bounds.y() - 80.0).abs() < 0.01);
        assert!((second.bounds.height() - 20.0).abs() < 0.01);
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
