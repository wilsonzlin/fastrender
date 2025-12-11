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
use crate::layout::absolute_positioning::{resolve_positioned_style, AbsoluteLayout, AbsoluteLayoutInput};
use crate::layout::constraints::{AvailableSpace, LayoutConstraints};
use crate::layout::contexts::block::BlockFormattingContext;
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::contexts::positioned::ContainingBlock;
use crate::layout::contexts::table::column_distribution::{
    distribute_spanning_cell_width, distribute_spanning_percentage, ColumnConstraints, ColumnDistributor,
    DistributionMode,
};
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::style::color::Rgba;
use crate::style::computed::Visibility;
use crate::style::display::Display;
use crate::style::types::{
    BorderCollapse, BorderStyle, CaptionSide, Direction, EmptyCells, TableLayout, VerticalAlign,
};
use crate::style::values::{Length, LengthUnit};
use crate::style::ComputedStyle;
use crate::tree::box_tree::{BoxNode, BoxType, MarkerContent};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};
use std::sync::Arc;

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

    /// Source column index prior to any filtering (0-based)
    pub source_index: usize,

    /// Visibility state for this column
    pub visibility: Visibility,

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
            source_index: index,
            visibility: Visibility::Visible,
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

    /// Source row index prior to any filtering (0-based)
    pub source_index: usize,

    /// Visibility state for this row
    pub visibility: Visibility,

    /// Specified height (from row element)
    pub specified_height: Option<SpecifiedHeight>,

    /// Author min-height (from CSS)
    pub author_min_height: Option<SpecifiedHeight>,

    /// Author max-height (from CSS)
    pub author_max_height: Option<SpecifiedHeight>,

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
            source_index: index,
            visibility: Visibility::Visible,
            specified_height: None,
            author_min_height: None,
            author_max_height: None,
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

fn resolve_row_min_max(row: &RowInfo, percent_base: Option<f32>) -> (Option<f32>, Option<f32>) {
    let min = match row.author_min_height {
        Some(SpecifiedHeight::Fixed(h)) => Some(h),
        Some(SpecifiedHeight::Percent(p)) => percent_base.map(|t| (p / 100.0) * t),
        _ => None,
    };
    let max = match row.author_max_height {
        Some(SpecifiedHeight::Fixed(h)) => Some(h),
        Some(SpecifiedHeight::Percent(p)) => percent_base.map(|t| (p / 100.0) * t),
        _ => None,
    };
    (min, max)
}

/// Information about a table cell
#[derive(Debug, Clone)]
pub struct CellInfo {
    /// Cell index in the cells array
    pub index: usize,

    /// Source row index prior to any filtering
    pub source_row: usize,

    /// Source column index prior to any filtering
    pub source_col: usize,

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
            source_row: row,
            source_col: col,
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

fn node_has_visible_content(node: &BoxNode) -> bool {
    match &node.box_type {
        BoxType::Text(text) => !text.text.trim().is_empty(),
        BoxType::Marker(marker) => match &marker.content {
            MarkerContent::Text(t) => !t.trim().is_empty(),
            MarkerContent::Image(_) => true,
        },
        BoxType::Replaced(_) => true,
        _ => node.children.iter().any(node_has_visible_content),
    }
}

fn cell_is_visually_empty(cell: &BoxNode) -> bool {
    if cell.style.background_layers.iter().any(|l| l.image.is_some()) || !cell.style.background_color.is_transparent() {
        return false;
    }
    !node_has_visible_content(cell)
}

fn style_paints_background_or_border(style: &ComputedStyle, allow_borders: bool) -> bool {
    if !style.background_color.is_transparent()
        || style.background_layers.iter().any(|l| l.image.is_some())
        || !style.box_shadow.is_empty()
    {
        return true;
    }
    if !allow_borders {
        return false;
    }
    let paints_border = |style: BorderStyle, width: &Length| {
        !matches!(style, BorderStyle::None | BorderStyle::Hidden) && width.to_px() > 0.0
    };
    paints_border(style.border_top_style, &style.border_top_width)
        || paints_border(style.border_right_style, &style.border_right_width)
        || paints_border(style.border_bottom_style, &style.border_bottom_width)
        || paints_border(style.border_left_style, &style.border_left_width)
}

fn strip_borders(style: &ComputedStyle) -> Arc<ComputedStyle> {
    let mut clone = style.clone();
    clone.border_top_width = Length::px(0.0);
    clone.border_right_width = Length::px(0.0);
    clone.border_bottom_width = Length::px(0.0);
    clone.border_left_width = Length::px(0.0);
    clone.border_top_style = BorderStyle::None;
    clone.border_right_style = BorderStyle::None;
    clone.border_bottom_style = BorderStyle::None;
    clone.border_left_style = BorderStyle::None;
    Arc::new(clone)
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
        let mut row_heights: Vec<Option<SpecifiedHeight>> = Vec::new();
        let mut row_min_heights: Vec<Option<SpecifiedHeight>> = Vec::new();
        let mut row_max_heights: Vec<Option<SpecifiedHeight>> = Vec::new();
        let mut row_visibilities: Vec<Visibility> = Vec::new();

        // Collect all cells first
        let mut cell_data: Vec<(usize, usize, usize, usize, usize)> = Vec::new(); // (row, col, rowspan, colspan, box_idx)

        for (child_idx, child) in table_box.children.iter().enumerate() {
            match Self::get_table_element_type(child) {
                TableElementType::RowGroup | TableElementType::HeaderGroup | TableElementType::FooterGroup => {
                    let group_visibility = child.style.visibility;
                    // Process rows within the group
                    for (row_child_idx, row_child) in child.children.iter().enumerate() {
                        if matches!(Self::get_table_element_type(row_child), TableElementType::Row) {
                            let spec_height = Self::length_to_specified_height_opt(
                                row_child.style.height.as_ref(),
                                row_child.style.font_size,
                            );
                            let min_h = Self::length_to_specified_height_opt(
                                row_child.style.min_height.as_ref(),
                                row_child.style.font_size,
                            );
                            let max_h = Self::length_to_specified_height_opt(
                                row_child.style.max_height.as_ref(),
                                row_child.style.font_size,
                            );
                            let row_visibility = if matches!(group_visibility, Visibility::Collapse) {
                                Visibility::Collapse
                            } else {
                                row_child.style.visibility
                            };
                            row_visibilities.push(row_visibility);
                            row_heights.push(spec_height);
                            row_min_heights.push(min_h);
                            row_max_heights.push(max_h);
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
                    let spec_height =
                        Self::length_to_specified_height_opt(child.style.height.as_ref(), child.style.font_size);
                    let min_h =
                        Self::length_to_specified_height_opt(child.style.min_height.as_ref(), child.style.font_size);
                    let max_h =
                        Self::length_to_specified_height_opt(child.style.max_height.as_ref(), child.style.font_size);
                    row_visibilities.push(child.style.visibility);
                    row_heights.push(spec_height);
                    row_min_heights.push(min_h);
                    row_max_heights.push(max_h);
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
                    if let Some(col) = structure.columns.get_mut(col_cursor) {
                        col.visibility = child.style.visibility;
                        if let Some(width) = &child.style.width {
                            col.specified_width = Some(Self::length_to_specified_width(width));
                        }
                    }
                    col_cursor += 1;
                }
                TableElementType::ColumnGroup => {
                    let group_visibility = child.style.visibility;
                    // Apply group width to contained columns (or next column if none)
                    if !child.children.is_empty() {
                        for group_child in &child.children {
                            if Self::get_table_element_type(group_child) == TableElementType::Column {
                                if let Some(col) = structure.columns.get_mut(col_cursor) {
                                    col.visibility = if matches!(group_visibility, Visibility::Collapse) {
                                        Visibility::Collapse
                                    } else {
                                        group_child.style.visibility
                                    };
                                    if let Some(width) = &group_child.style.width {
                                        col.specified_width = Some(Self::length_to_specified_width(width));
                                    }
                                }
                                col_cursor += 1;
                            }
                        }
                    } else {
                        if let Some(col) = structure.columns.get_mut(col_cursor) {
                            col.visibility = child.style.visibility;
                            if let Some(width) = &child.style.width {
                                col.specified_width = Some(Self::length_to_specified_width(width));
                            }
                        }
                        col_cursor += 1;
                    }
                }
                _ => {}
            }
        }

        // Initialize rows
        for i in 0..structure.row_count {
            let mut row = RowInfo::new(i);
            row.specified_height = row_heights.get(i).cloned().unwrap_or(None);
            row.author_min_height = row_min_heights.get(i).cloned().unwrap_or(None);
            row.author_max_height = row_max_heights.get(i).cloned().unwrap_or(None);
            row.visibility = row_visibilities.get(i).cloned().unwrap_or(Visibility::Visible);
            row.source_index = i;
            structure.rows.push(row);
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

        structure.apply_visibility_collapse()
    }

    /// Removes rows and columns with `visibility: collapse` from the structure
    /// while preserving source indices so cell lookup and styling can map back
    /// to the original table tree.
    fn apply_visibility_collapse(self) -> Self {
        let mut row_map: Vec<Option<usize>> = Vec::with_capacity(self.row_count);
        let mut next_row = 0usize;
        for row in &self.rows {
            if matches!(row.visibility, Visibility::Collapse) {
                row_map.push(None);
            } else {
                row_map.push(Some(next_row));
                next_row += 1;
            }
        }

        let mut col_map: Vec<Option<usize>> = Vec::with_capacity(self.column_count);
        let mut next_col = 0usize;
        for col in &self.columns {
            if matches!(col.visibility, Visibility::Collapse) {
                col_map.push(None);
            } else {
                col_map.push(Some(next_col));
                next_col += 1;
            }
        }

        if next_row == self.row_count && next_col == self.column_count {
            return self;
        }

        let mut new_structure = TableStructure {
            column_count: next_col,
            row_count: next_row,
            columns: Vec::with_capacity(next_col),
            rows: Vec::with_capacity(next_row),
            cells: Vec::new(),
            grid: vec![vec![None; next_col]; next_row],
            border_spacing: self.border_spacing,
            border_collapse: self.border_collapse,
            is_fixed_layout: self.is_fixed_layout,
        };

        for (old_idx, mut col) in self.columns.into_iter().enumerate() {
            if let Some(new_idx) = col_map.get(old_idx).and_then(|m| *m) {
                col.index = new_idx;
                new_structure.columns.push(col);
            }
        }

        for (old_idx, mut row) in self.rows.into_iter().enumerate() {
            if let Some(new_idx) = row_map.get(old_idx).and_then(|m| *m) {
                row.index = new_idx;
                new_structure.rows.push(row);
            }
        }

        for cell in self.cells.into_iter() {
            let Some(new_row) = row_map.get(cell.row).and_then(|m| *m) else {
                continue;
            };
            let row_span_end = (cell.row + cell.rowspan).min(row_map.len());
            let mut visible_rows = 0usize;
            for r in cell.row..row_span_end {
                if row_map.get(r).and_then(|m| *m).is_some() {
                    visible_rows += 1;
                }
            }
            if visible_rows == 0 {
                continue;
            }

            let col_span_end = (cell.col + cell.colspan).min(col_map.len());
            let mut mapped_cols: Vec<usize> = (cell.col..col_span_end)
                .filter_map(|c| col_map.get(c).and_then(|m| *m))
                .collect();
            if mapped_cols.is_empty() {
                continue;
            }
            mapped_cols.sort_unstable();
            mapped_cols.dedup();

            let mut new_cell = cell;
            new_cell.row = new_row;
            new_cell.col = *mapped_cols.first().unwrap_or(&0);
            new_cell.rowspan = visible_rows;
            new_cell.colspan = mapped_cols.len();
            new_cell.index = new_structure.cells.len();
            new_structure.cells.push(new_cell);
        }

        for cell in &new_structure.cells {
            let row_end = (cell.row + cell.rowspan).min(new_structure.row_count);
            let col_end = (cell.col + cell.colspan).min(new_structure.column_count);
            for r in cell.row..row_end {
                for c in cell.col..col_end {
                    if let Some(slot) = new_structure.grid.get_mut(r).and_then(|row| row.get_mut(c)) {
                        *slot = Some(cell.index);
                    }
                }
            }
        }

        new_structure
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

    fn length_to_specified_height(length: &crate::style::values::Length, font_size: f32) -> SpecifiedHeight {
        use crate::style::values::LengthUnit;
        match length.unit {
            LengthUnit::Percent => SpecifiedHeight::Percent(length.value),
            LengthUnit::Em | LengthUnit::Rem => SpecifiedHeight::Fixed(length.value * font_size),
            _ if length.unit.is_absolute() => SpecifiedHeight::Fixed(length.to_px()),
            _ => SpecifiedHeight::Auto,
        }
    }

    fn length_to_specified_height_opt(
        length: Option<&crate::style::values::Length>,
        font_size: f32,
    ) -> Option<SpecifiedHeight> {
        length.map(|len| Self::length_to_specified_height(len, font_size))
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

fn resolve_opt_length_against(
    length: Option<&crate::style::values::Length>,
    font_size: f32,
    containing_width: Option<f32>,
) -> Option<f32> {
    length.and_then(|l| resolve_length_against(l, font_size, containing_width))
}

fn clamp_to_min_max(value: f32, min: Option<f32>, max: Option<f32>) -> f32 {
    let mut v = value;
    if let Some(min) = min {
        v = v.max(min);
    }
    if let Some(max) = max {
        v = v.min(max);
    }
    v
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

#[derive(Debug, Clone, Copy)]
struct ResolvedBorder {
    width: f32,
    style: BorderStyle,
    color: Rgba,
}

impl ResolvedBorder {
    fn none() -> Self {
        Self {
            width: 0.0,
            style: BorderStyle::None,
            color: Rgba::TRANSPARENT,
        }
    }
}

#[derive(Debug, Clone)]
struct CollapsedBorders {
    /// Vertical grid lines: index is column boundary (0..=columns), inner vec is per row segment.
    vertical: Vec<Vec<ResolvedBorder>>,
    /// Horizontal grid lines: index is row boundary (0..=rows), inner vec is per column segment.
    horizontal: Vec<Vec<ResolvedBorder>>,
    /// Corner joins: index [row][col] for grid junctions.
    corners: Vec<Vec<ResolvedBorder>>,
}

/// Resolve the border widths for a table in collapsed border model.
/// Returns resolved borders for vertical and horizontal grid lines.
/// Lengths: vertical = columns + 1, horizontal = rows + 1.
fn compute_collapsed_borders(table_box: &BoxNode, structure: &TableStructure) -> CollapsedBorders {
    #[derive(Clone, Copy)]
    enum BorderOrigin {
        Table,
        ColumnGroup,
        Column,
        RowGroup,
        Row,
        Cell,
    }

    #[derive(Clone, Copy)]
    struct BorderCandidate {
        width: f32,
        style: BorderStyle,
        color: Rgba,
        origin: BorderOrigin,
        source_order: u32,
        row: usize,
        col: usize,
    }

    impl BorderCandidate {
        fn none() -> Self {
            Self {
                width: 0.0,
                style: BorderStyle::None,
                color: Rgba::TRANSPARENT,
                origin: BorderOrigin::Table,
                source_order: 0,
                row: 0,
                col: 0,
            }
        }
    }

    fn style_rank(style: BorderStyle) -> u8 {
        match style {
            BorderStyle::Double => 8,
            BorderStyle::Solid => 7,
            BorderStyle::Dashed => 6,
            BorderStyle::Dotted => 5,
            BorderStyle::Ridge => 4,
            BorderStyle::Outset => 3,
            BorderStyle::Groove => 2,
            BorderStyle::Inset => 1,
            BorderStyle::Hidden | BorderStyle::None => 0,
        }
    }

    fn origin_priority(origin: BorderOrigin) -> u8 {
        match origin {
            BorderOrigin::Cell => 6,
            BorderOrigin::Row => 5,
            BorderOrigin::RowGroup => 4,
            BorderOrigin::Column => 3,
            BorderOrigin::ColumnGroup => 2,
            BorderOrigin::Table => 1,
        }
    }

    fn pick_by_position(current: BorderCandidate, candidate: BorderCandidate, direction: Direction) -> BorderCandidate {
        if candidate.row < current.row {
            return candidate;
        }
        if candidate.row > current.row {
            return current;
        }

        let ltr = matches!(direction, Direction::Ltr);
        if ltr {
            if candidate.col < current.col {
                return candidate;
            }
            if candidate.col > current.col {
                return current;
            }
        } else {
            if candidate.col > current.col {
                return candidate;
            }
            if candidate.col < current.col {
                return current;
            }
        }

        if candidate.source_order >= current.source_order {
            candidate
        } else {
            current
        }
    }

    fn resolve_candidates(candidates: &[BorderCandidate], direction: Direction) -> BorderCandidate {
        if candidates.is_empty() {
            return BorderCandidate::none();
        }

        if let Some(hidden) = candidates.iter().find(|c| c.style == BorderStyle::Hidden) {
            return *hidden;
        }

        let has_non_none = candidates.iter().any(|c| c.style != BorderStyle::None);
        if !has_non_none {
            return BorderCandidate::none();
        }

        let max_width = candidates
            .iter()
            .fold(0.0f32, |acc, c| if c.width > acc { c.width } else { acc });
        let mut contenders: Vec<&BorderCandidate> = candidates
            .iter()
            .filter(|c| (c.width - max_width).abs() < 0.01)
            .collect();

        if contenders.len() > 1 {
            let best_style = contenders.iter().map(|c| style_rank(c.style)).max().unwrap_or(0);
            contenders.retain(|c| style_rank(c.style) == best_style);
        }

        if contenders.len() > 1 {
            let best_origin = contenders.iter().map(|c| origin_priority(c.origin)).max().unwrap_or(0);
            contenders.retain(|c| origin_priority(c.origin) == best_origin);
        }

        let mut winner = **contenders.first().unwrap_or(&&BorderCandidate::none());
        for cand in contenders.into_iter().skip(1) {
            winner = pick_by_position(winner, *cand, direction);
        }
        winner
    }

    fn length_to_px(len: &crate::style::values::Length) -> f32 {
        if len.unit.is_absolute() {
            len.to_px()
        } else {
            len.value
        }
    }

    fn resolved_border_width(style: BorderStyle, len: &crate::style::values::Length) -> f32 {
        if matches!(style, BorderStyle::None | BorderStyle::Hidden) {
            0.0
        } else {
            length_to_px(len)
        }
    }

    let direction = table_box.style.direction;

    let mut vertical: Vec<Vec<Vec<BorderCandidate>>> =
        vec![vec![Vec::new(); structure.row_count]; structure.column_count + 1];
    let mut horizontal: Vec<Vec<Vec<BorderCandidate>>> =
        vec![vec![Vec::new(); structure.column_count]; structure.row_count + 1];

    let max_source_row = structure.rows.iter().map(|r| r.source_index).max().unwrap_or(0);
    let mut source_row_to_visible = if structure.rows.is_empty() {
        Vec::new()
    } else {
        vec![None; max_source_row + 1]
    };
    for row in &structure.rows {
        if row.source_index >= source_row_to_visible.len() {
            source_row_to_visible.resize(row.source_index + 1, None);
        }
        source_row_to_visible[row.source_index] = Some(row.index);
    }

    let max_source_col = structure.columns.iter().map(|c| c.source_index).max().unwrap_or(0);
    let mut source_col_to_visible = if structure.columns.is_empty() {
        Vec::new()
    } else {
        vec![None; max_source_col + 1]
    };
    for col in &structure.columns {
        if col.source_index >= source_col_to_visible.len() {
            source_col_to_visible.resize(col.source_index + 1, None);
        }
        source_col_to_visible[col.source_index] = Some(col.index);
    }

    // Gather row, row-group, column, and column-group styles with document order.
    let mut row_styles: Vec<Option<(std::sync::Arc<crate::style::ComputedStyle>, u32)>> =
        vec![None; structure.row_count];
    let mut row_groups: Vec<(usize, usize, std::sync::Arc<crate::style::ComputedStyle>, u32)> = Vec::new();
    let mut column_styles: Vec<Option<(std::sync::Arc<crate::style::ComputedStyle>, u32)>> =
        vec![None; structure.column_count];
    let mut column_groups: Vec<(usize, usize, std::sync::Arc<crate::style::ComputedStyle>, u32)> = Vec::new();

    let mut source_counter: u32 = 1;
    let mut source_row_idx = 0usize;
    let mut source_col_idx = 0usize;

    for child in &table_box.children {
        match TableStructure::get_table_element_type(child) {
            TableElementType::RowGroup | TableElementType::HeaderGroup | TableElementType::FooterGroup => {
                let group_order = source_counter;
                source_counter += 1;
                let mut first_visible = None;
                let mut last_visible = None;
                for row_child in &child.children {
                    if TableStructure::get_table_element_type(row_child) == TableElementType::Row {
                        if let Some(visible) = source_row_to_visible.get(source_row_idx).and_then(|m| *m) {
                            row_styles[visible] = Some((row_child.style.clone(), source_counter));
                            source_counter += 1;
                            first_visible.get_or_insert(visible);
                            last_visible = Some(visible);
                        }
                        source_row_idx += 1;
                    }
                }
                if let (Some(start), Some(end)) = (first_visible, last_visible) {
                    row_groups.push((start, end + 1, child.style.clone(), group_order));
                }
            }
            TableElementType::Row => {
                if let Some(visible) = source_row_to_visible.get(source_row_idx).and_then(|m| *m) {
                    row_styles[visible] = Some((child.style.clone(), source_counter));
                    source_counter += 1;
                }
                source_row_idx += 1;
            }
            TableElementType::Column => {
                if let Some(visible) = source_col_to_visible.get(source_col_idx).and_then(|m| *m) {
                    column_styles[visible] = Some((child.style.clone(), source_counter));
                    source_counter += 1;
                }
                source_col_idx += 1;
            }
            TableElementType::ColumnGroup => {
                let group_order = source_counter;
                source_counter += 1;
                let mut first_visible = None;
                let mut last_visible = None;
                if child.children.is_empty() {
                    if let Some(visible) = source_col_to_visible.get(source_col_idx).and_then(|m| *m) {
                        column_styles[visible] = column_styles[visible]
                            .take()
                            .or_else(|| Some((child.style.clone(), source_counter)));
                        source_counter += 1;
                        first_visible.get_or_insert(visible);
                        last_visible = Some(visible);
                    }
                    source_col_idx += 1;
                } else {
                    for group_child in &child.children {
                        if TableStructure::get_table_element_type(group_child) == TableElementType::Column {
                            if let Some(visible) = source_col_to_visible.get(source_col_idx).and_then(|m| *m) {
                                column_styles[visible] = Some((group_child.style.clone(), source_counter));
                                source_counter += 1;
                                first_visible.get_or_insert(visible);
                                last_visible = Some(visible);
                            }
                            source_col_idx += 1;
                        }
                    }
                }
                if let (Some(start), Some(end)) = (first_visible, last_visible) {
                    column_groups.push((start, end + 1, child.style.clone(), group_order));
                }
            }
            _ => {}
        }
    }

    for entry in row_styles.iter_mut() {
        if entry.is_none() {
            *entry = Some((table_box.style.clone(), source_counter));
            source_counter += 1;
        }
    }

    let mut apply_vertical = |col_idx: usize,
                              row_start: usize,
                              row_end: usize,
                              style: BorderStyle,
                              width: &crate::style::values::Length,
                              color: &Rgba,
                              origin: BorderOrigin,
                              source_order: u32,
                              origin_row: usize,
                              origin_col: usize| {
        if col_idx >= vertical.len() || row_start >= row_end {
            return;
        }
        let candidate = BorderCandidate {
            width: resolved_border_width(style, width),
            style,
            color: *color,
            origin,
            source_order,
            row: origin_row,
            col: origin_col,
        };
        for row in row_start..row_end.min(vertical[col_idx].len()) {
            vertical[col_idx][row].push(candidate);
        }
    };

    let mut apply_horizontal = |row_idx: usize,
                                col_start: usize,
                                col_end: usize,
                                style: BorderStyle,
                                width: &crate::style::values::Length,
                                color: &Rgba,
                                origin: BorderOrigin,
                                source_order: u32,
                                origin_row: usize,
                                origin_col: usize| {
        if row_idx >= horizontal.len() || col_start >= col_end {
            return;
        }
        let candidate = BorderCandidate {
            width: resolved_border_width(style, width),
            style,
            color: *color,
            origin,
            source_order,
            row: origin_row,
            col: origin_col,
        };
        for col in col_start..col_end.min(horizontal[row_idx].len()) {
            horizontal[row_idx][col].push(candidate);
        }
    };

    // Table outer borders
    let tstyle = &table_box.style;
    apply_vertical(
        0,
        0,
        structure.row_count,
        tstyle.border_left_style,
        &tstyle.border_left_width,
        &tstyle.border_left_color,
        BorderOrigin::Table,
        0,
        0,
        0,
    );
    apply_vertical(
        structure.column_count,
        0,
        structure.row_count,
        tstyle.border_right_style,
        &tstyle.border_right_width,
        &tstyle.border_right_color,
        BorderOrigin::Table,
        0,
        0,
        structure.column_count,
    );
    apply_horizontal(
        0,
        0,
        structure.column_count,
        tstyle.border_top_style,
        &tstyle.border_top_width,
        &tstyle.border_top_color,
        BorderOrigin::Table,
        0,
        0,
        0,
    );
    apply_horizontal(
        structure.row_count,
        0,
        structure.column_count,
        tstyle.border_bottom_style,
        &tstyle.border_bottom_width,
        &tstyle.border_bottom_color,
        BorderOrigin::Table,
        0,
        structure.row_count,
        0,
    );

    // Row group borders
    for (start, end, style, order) in &row_groups {
        apply_horizontal(
            *start,
            0,
            structure.column_count,
            style.border_top_style,
            &style.border_top_width,
            &style.border_top_color,
            BorderOrigin::RowGroup,
            *order,
            *start,
            0,
        );
        apply_horizontal(
            *end,
            0,
            structure.column_count,
            style.border_bottom_style,
            &style.border_bottom_width,
            &style.border_bottom_color,
            BorderOrigin::RowGroup,
            *order,
            *start,
            0,
        );
    }

    // Row borders
    for (row_idx, entry) in row_styles.iter().enumerate() {
        if let Some((style, order)) = entry {
            apply_horizontal(
                row_idx,
                0,
                structure.column_count,
                style.border_top_style,
                &style.border_top_width,
                &style.border_top_color,
                BorderOrigin::Row,
                *order,
                row_idx,
                0,
            );
            apply_horizontal(
                row_idx + 1,
                0,
                structure.column_count,
                style.border_bottom_style,
                &style.border_bottom_width,
                &style.border_bottom_color,
                BorderOrigin::Row,
                *order,
                row_idx,
                0,
            );
        }
    }

    // Column group borders
    for (start, end, style, order) in &column_groups {
        apply_vertical(
            *start,
            0,
            structure.row_count,
            style.border_left_style,
            &style.border_left_width,
            &style.border_left_color,
            BorderOrigin::ColumnGroup,
            *order,
            0,
            *start,
        );
        apply_vertical(
            *end,
            0,
            structure.row_count,
            style.border_right_style,
            &style.border_right_width,
            &style.border_right_color,
            BorderOrigin::ColumnGroup,
            *order,
            0,
            *start,
        );
    }

    // Column borders
    for (col_idx, col_style) in column_styles.iter().enumerate() {
        if let Some((style, order)) = col_style {
            apply_vertical(
                col_idx,
                0,
                structure.row_count,
                style.border_left_style,
                &style.border_left_width,
                &style.border_left_color,
                BorderOrigin::Column,
                *order,
                0,
                col_idx,
            );
            apply_vertical(
                col_idx + 1,
                0,
                structure.row_count,
                style.border_right_style,
                &style.border_right_width,
                &style.border_right_color,
                BorderOrigin::Column,
                *order,
                0,
                col_idx,
            );
        }
    }

    // Cells
    let tfc = TableFormattingContext::new();
    for cell in &structure.cells {
        if let Some(cell_box) = tfc.get_cell_box(table_box, cell) {
            // In the collapsed border model the `empty-cells` property has no effect (CSS 2.1 §17.6.1),
            // so even visually empty cells participate in border conflict resolution.
            let style = &cell_box.style;
            let start_col = cell.col;
            let end_col = (cell.col + cell.colspan).min(structure.column_count);
            let start_row = cell.row;
            let end_row = (cell.row + cell.rowspan).min(structure.row_count);

            apply_vertical(
                start_col,
                start_row,
                end_row,
                style.border_left_style,
                &style.border_left_width,
                &style.border_left_color,
                BorderOrigin::Cell,
                cell.index as u32,
                cell.row,
                cell.col,
            );
            apply_vertical(
                end_col,
                start_row,
                end_row,
                style.border_right_style,
                &style.border_right_width,
                &style.border_right_color,
                BorderOrigin::Cell,
                cell.index as u32,
                cell.row,
                cell.col,
            );
            apply_horizontal(
                start_row,
                start_col,
                end_col,
                style.border_top_style,
                &style.border_top_width,
                &style.border_top_color,
                BorderOrigin::Cell,
                cell.index as u32,
                cell.row,
                cell.col,
            );
            apply_horizontal(
                end_row,
                start_col,
                end_col,
                style.border_bottom_style,
                &style.border_bottom_width,
                &style.border_bottom_color,
                BorderOrigin::Cell,
                cell.index as u32,
                cell.row,
                cell.col,
            );
        }
    }

    let candidate_to_resolved = |candidate: BorderCandidate| {
        if matches!(candidate.style, BorderStyle::None | BorderStyle::Hidden) || candidate.width <= 0.0 {
            ResolvedBorder::none()
        } else {
            ResolvedBorder {
                width: candidate.width,
                style: candidate.style,
                color: candidate.color,
            }
        }
    };

    let resolved_vertical: Vec<Vec<ResolvedBorder>> = vertical
        .iter()
        .map(|line| {
            line.iter()
                .map(|candidates| candidate_to_resolved(resolve_candidates(candidates, direction)))
                .collect()
        })
        .collect();

    let resolved_horizontal: Vec<Vec<ResolvedBorder>> = horizontal
        .iter()
        .map(|line| {
            line.iter()
                .map(|candidates| candidate_to_resolved(resolve_candidates(candidates, direction)))
                .collect()
        })
        .collect();

    let mut resolved_corners: Vec<Vec<ResolvedBorder>> = Vec::with_capacity(structure.row_count + 1);
    for r in 0..=structure.row_count {
        let mut row_vec: Vec<ResolvedBorder> = Vec::with_capacity(structure.column_count + 1);
        for c in 0..=structure.column_count {
            let mut candidates: Vec<BorderCandidate> = Vec::new();
            if c < vertical.len() && r > 0 && r - 1 < vertical[c].len() {
                candidates.extend_from_slice(&vertical[c][r - 1]);
            }
            if c < vertical.len() && r < vertical[c].len() {
                candidates.extend_from_slice(&vertical[c][r]);
            }
            if r < horizontal.len() && c > 0 && c - 1 < horizontal[r].len() {
                candidates.extend_from_slice(&horizontal[r][c - 1]);
            }
            if r < horizontal.len() && c < horizontal[r].len() {
                candidates.extend_from_slice(&horizontal[r][c]);
            }
            let resolved = resolve_candidates(&candidates, direction);
            row_vec.push(candidate_to_resolved(resolved));
        }
        resolved_corners.push(row_vec);
    }

    CollapsedBorders {
        vertical: resolved_vertical,
        horizontal: resolved_horizontal,
        corners: resolved_corners,
    }
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
    find_first_baseline(fragment, 0.0).or_else(|| {
        let origin = fragment.bounds.y();
        let height = fragment.bounds.height();
        if height.is_finite() && height > 0.0 {
            Some(origin + height)
        } else {
            None
        }
    })
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
                let weights: Vec<f32> = structure.columns[span_start..span_end]
                    .iter()
                    .map(|c| c.min_width.max(0.0))
                    .collect();
                let weight_sum: f32 = weights.iter().sum();
                let default_weight = if weight_sum == 0.0 {
                    1.0 / (span_end - span_start) as f32
                } else {
                    0.0
                };
                for (idx, col) in structure.columns[span_start..span_end].iter_mut().enumerate() {
                    let weight = if weight_sum == 0.0 {
                        default_weight
                    } else {
                        weights[idx] / weight_sum
                    };
                    col.min_width += extra * weight;
                }
            }

            // Distribute additional max width if the cell's max exceeds the span total.
            let current_max: f32 = structure.columns[span_start..span_end]
                .iter()
                .map(|c| c.max_width)
                .sum();
            if cell.max_width.is_finite() && cell.max_width > current_max {
                let extra = cell.max_width - current_max;
                let weights: Vec<f32> = structure.columns[span_start..span_end]
                    .iter()
                    .map(|c| {
                        c.max_width
                            .is_finite()
                            .then_some(c.max_width)
                            .unwrap_or(c.min_width)
                            .max(0.0)
                    })
                    .collect();
                let weight_sum: f32 = weights.iter().sum();
                let default_weight = if weight_sum == 0.0 {
                    1.0 / (span_end - span_start) as f32
                } else {
                    0.0
                };
                for (idx, col) in structure.columns[span_start..span_end].iter_mut().enumerate() {
                    let weight = if weight_sum == 0.0 {
                        default_weight
                    } else {
                        weights[idx] / weight_sum
                    };
                    // Keep max at least as large as min.
                    col.max_width = (col.max_width + extra * weight).max(col.min_width);
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

/// Calculates row heights based on cell content and optional available height.
///
/// This is a simplified stand-alone distributor used by unit tests and legacy
/// paths. It accounts for:
/// - min-height contributions from non-spanning cells
/// - rowspan contributions spread evenly across rows
/// - specified row heights (length or percentage when a definite available height is provided)
/// - distributing remaining space to auto rows when an available height is known
pub fn calculate_row_heights(structure: &mut TableStructure, available_height: Option<f32>) {
    if structure.row_count == 0 {
        return;
    }

    let spacing = if structure.border_collapse == BorderCollapse::Collapse {
        0.0
    } else {
        structure.border_spacing.1
    };
    let content_available = available_height.map(|h| {
        (h - spacing
            * (if spacing > 0.0 {
                structure.row_count as f32 + 1.0
            } else {
                0.0
            }))
        .max(0.0)
    });

    let row_floor = |row: &RowInfo| -> f32 {
        let mut floor = row.min_height;
        if let Some(spec) = row.specified_height {
            match spec {
                SpecifiedHeight::Fixed(h) => floor = floor.max(h),
                SpecifiedHeight::Percent(pct) => {
                    if let Some(base) = content_available {
                        floor = floor.max((pct / 100.0) * base);
                    }
                }
                SpecifiedHeight::Auto => {}
            }
        }
        floor
    };

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

            let spacing = structure.border_spacing.1 * (cell.rowspan - 1) as f32;
            let span_height = (cell.min_height - spacing).max(0.0);

            let auto_rows: Vec<usize> = (span_start..span_end)
                .filter(|idx| {
                    matches!(
                        structure.rows[*idx].specified_height,
                        Some(SpecifiedHeight::Auto) | None
                    )
                })
                .collect();
            let has_auto = !auto_rows.is_empty();
            let targets: Vec<usize> = if has_auto {
                auto_rows.clone()
            } else {
                (span_start..span_end).collect()
            };
            if !targets.is_empty() {
                let non_target_sum: f32 = (span_start..span_end)
                    .filter(|idx| !targets.contains(idx))
                    .map(|idx| row_floor(&structure.rows[idx]))
                    .sum();
                let remaining = (span_height - non_target_sum).max(0.0);
                let base_sum: f32 = targets.iter().map(|r| row_floor(&structure.rows[*r])).sum();
                let total_weight = if base_sum > 0.0 { base_sum } else { targets.len() as f32 };

                for &r in &targets {
                    let base = row_floor(&structure.rows[r]);
                    let weight = if base_sum > 0.0 {
                        base / total_weight
                    } else {
                        1.0 / targets.len() as f32
                    };
                    let share = remaining * weight;
                    structure.rows[r].min_height = structure.rows[r].min_height.max(share);
                }
            }
        }
    }

    // Phase 3: Apply specified/percentage heights when an available height is known.
    let mut fixed_sum = 0.0;
    let mut percent_rows = Vec::new();
    let mut auto_rows = Vec::new();

    for (idx, row) in structure.rows.iter().enumerate() {
        match row.specified_height {
            Some(SpecifiedHeight::Fixed(h)) => fixed_sum += h.max(row.min_height),
            Some(SpecifiedHeight::Percent(p)) => percent_rows.push((idx, p)),
            Some(SpecifiedHeight::Auto) | None => auto_rows.push(idx),
        }
    }

    let mut computed: Vec<f32> = structure.rows.iter().map(|r| r.min_height).collect();

    if let Some(base) = content_available {
        // Apply percentage rows against the definite base.
        for &(idx, pct) in &percent_rows {
            let target = (pct / 100.0) * base;
            computed[idx] = computed[idx].max(target);
        }

        // Recompute the fixed budget (fixed + percentage rows) using specified heights where applicable.
        fixed_sum = 0.0;
        for (idx, row) in structure.rows.iter().enumerate() {
            match row.specified_height {
                Some(SpecifiedHeight::Fixed(h)) => {
                    computed[idx] = computed[idx].max(h);
                    fixed_sum += computed[idx];
                }
                Some(SpecifiedHeight::Percent(_)) => fixed_sum += computed[idx],
                _ => {}
            }
        }

        // Fixed rows were already counted in fixed_sum via target or explicit fixed.
        // Compute remaining space for auto rows.
        let remaining = base - fixed_sum;
        if remaining > 0.0 && !auto_rows.is_empty() {
            let auto_sum: f32 = auto_rows.iter().map(|i| computed[*i]).sum();
            if auto_sum > 0.0 {
                for idx in auto_rows {
                    let weight = computed[idx] / auto_sum;
                    computed[idx] = computed[idx].max(remaining * weight);
                }
            } else {
                let per = remaining / auto_rows.len() as f32;
                for idx in auto_rows {
                    computed[idx] = computed[idx].max(per);
                }
            }
        }
    }

    // Fallback: no available height, just use min heights and explicit lengths.
    for (idx, row) in structure.rows.iter().enumerate() {
        if let Some(SpecifiedHeight::Fixed(h)) = row.specified_height {
            computed[idx] = computed[idx].max(h);
        }
    }

    // Phase 4: calculate positions.
    let mut y = spacing;
    for (idx, row) in structure.rows.iter_mut().enumerate() {
        row.computed_height = computed[idx];
        row.y_position = y;
        y += row.computed_height + spacing;
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
    /// Formatting context factory carrying shared font resources for cell layout.
    factory: FormattingContextFactory,
    viewport_size: crate::geometry::Size,
    nearest_positioned_cb: ContainingBlock,
}

impl TableFormattingContext {
    /// Creates a new table formatting context
    pub fn new() -> Self {
        Self::with_factory(FormattingContextFactory::new())
    }

    /// Creates a table formatting context that reuses the provided factory.
    pub fn with_factory(factory: FormattingContextFactory) -> Self {
        let viewport_size = factory.viewport_size();
        let nearest_positioned_cb = factory.nearest_positioned_cb();
        Self {
            structure: None,
            factory,
            viewport_size,
            nearest_positioned_cb,
        }
    }

    /// Percentages on columns rely on a definite table width. If the table width
    /// is indefinite (auto/max-content/min-content with no containing inline size),
    /// treat percentage constraints as auto so intrinsic sizing isn't forced to zero.
    fn normalize_percentage_constraints(
        &self,
        constraints: &mut [ColumnConstraints],
        table_width: Option<f32>,
        available_width: &AvailableSpace,
    ) {
        if table_width.is_some() {
            return;
        }

        if matches!(available_width, AvailableSpace::Definite(_)) {
            return;
        }

        for col in constraints.iter_mut() {
            if col.percentage.is_some() {
                col.percentage = None;
                col.fixed_width = None;
                col.is_flexible = true;
            }
        }
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
        let fc = self.factory.create(fc_type);

        // Measure intrinsic content widths without the cell's own padding/borders; we'll add them once below.
        let mut stripped_cell = cell_box.clone();
        let mut stripped_style = (*stripped_cell.style).clone();
        stripped_style.padding_left = Length::px(0.0);
        stripped_style.padding_right = Length::px(0.0);
        stripped_style.border_left_width = Length::px(0.0);
        stripped_style.border_right_width = Length::px(0.0);
        stripped_cell.style = Arc::new(stripped_style);

        let mut min = fc
            .compute_intrinsic_inline_size(&stripped_cell, IntrinsicSizingMode::MinContent)
            .unwrap_or(0.0);
        let mut max = fc
            .compute_intrinsic_inline_size(&stripped_cell, IntrinsicSizingMode::MaxContent)
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
        percent_base: Option<f32>,
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
                DistributionMode::Fixed => (0.0, f32::INFINITY), // content is ignored in fixed layout
                _ => self.measure_cell_intrinsic_widths(cell_box, structure.border_collapse),
            };
            let specified_width = cell_box.style.width.as_ref().and_then(|width| match width.unit {
                LengthUnit::Percent => percent_base.map(|base| (width.value / 100.0) * base),
                _ => Some(width.to_px()),
            });

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
                if let Some(width) = &cell_box.style.width {
                    if let LengthUnit::Percent = width.unit {
                        // Split percentage across spanned columns so the span consumes its share of the table width.
                        distribute_spanning_percentage(constraints, start, end, width.value);
                    }
                }
                let target_min = specified_width.map(|w| min_w.max(w)).unwrap_or(min_w);
                let target_max = specified_width.map(|w| max_w.max(w)).unwrap_or(max_w);
                distribute_spanning_cell_width(constraints, start, end, target_min, target_max);
            }
        }

        // Apply specified widths from column info (<col>/<colgroup>)
        for (i, col_info) in structure.columns.iter().enumerate() {
            let Some(constraint) = constraints.get_mut(i) else {
                continue;
            };
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
    fn layout_cell(
        &self,
        cell_box: &BoxNode,
        cell_width: f32,
        border_collapse: BorderCollapse,
    ) -> Result<FragmentNode, LayoutError> {
        let hide_empty = border_collapse == BorderCollapse::Separate
            && cell_box.style.empty_cells == EmptyCells::Hide
            && cell_is_visually_empty(cell_box);
        let mut cloned = cell_box.clone();
        if hide_empty {
            let mut style = (*cloned.style).clone();
            style.reset_background_to_initial();
            style.border_left_width = crate::style::values::Length::px(0.0);
            style.border_right_width = crate::style::values::Length::px(0.0);
            style.border_top_width = crate::style::values::Length::px(0.0);
            style.border_bottom_width = crate::style::values::Length::px(0.0);
            style.border_left_style = BorderStyle::None;
            style.border_right_style = BorderStyle::None;
            style.border_top_style = BorderStyle::None;
            style.border_bottom_style = BorderStyle::None;
            cloned.style = std::sync::Arc::new(style);
        }

        let bfc = BlockFormattingContext::with_font_context_and_viewport(
            self.factory.font_context().clone(),
            self.factory.viewport_size(),
        );
        let constraints = LayoutConstraints::definite_width(cell_width.max(0.0));
        if matches!(border_collapse, BorderCollapse::Collapse) {
            let mut style = (*cloned.style).clone();
            style.border_left_width = crate::style::values::Length::px(0.0);
            style.border_right_width = crate::style::values::Length::px(0.0);
            style.border_top_width = crate::style::values::Length::px(0.0);
            style.border_bottom_width = crate::style::values::Length::px(0.0);
            cloned.style = std::sync::Arc::new(style);
            bfc.layout(&cloned, &constraints)
        } else {
            bfc.layout(&cloned, &constraints)
        }
    }

    /// Gets the box node for a cell
    fn get_cell_box<'a>(&self, table_box: &'a BoxNode, cell: &CellInfo) -> Option<&'a BoxNode> {
        let mut row_idx = 0;
        for child in &table_box.children {
            match TableStructure::get_table_element_type(child) {
                TableElementType::RowGroup | TableElementType::HeaderGroup | TableElementType::FooterGroup => {
                    for row_child in &child.children {
                        if row_idx == cell.source_row {
                            return row_child.children.get(cell.box_index);
                        }
                        row_idx += 1;
                    }
                }
                TableElementType::Row => {
                    if row_idx == cell.source_row {
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
        let mut positioned_children: Vec<BoxNode> = Vec::new();
        for child in &box_node.children {
            if matches!(
                child.style.position,
                crate::style::position::Position::Absolute | crate::style::position::Position::Fixed
            ) {
                positioned_children.push(child.clone());
            }
        }

        let structure = TableStructure::from_box_tree(box_node);
        let captions: Vec<&BoxNode> = box_node
            .children
            .iter()
            .filter(|child| {
                !matches!(
                    child.style.position,
                    crate::style::position::Position::Absolute | crate::style::position::Position::Fixed
                ) && matches!(child.style.display, Display::TableCaption)
            })
            .collect();

        let max_source_row = structure.rows.iter().map(|r| r.source_index).max().unwrap_or(0);
        let mut source_row_to_visible = if structure.rows.is_empty() {
            Vec::new()
        } else {
            vec![None; max_source_row + 1]
        };
        for row in &structure.rows {
            if row.source_index >= source_row_to_visible.len() {
                source_row_to_visible.resize(row.source_index + 1, None);
            }
            source_row_to_visible[row.source_index] = Some(row.index);
        }

        let max_source_col = structure.columns.iter().map(|c| c.source_index).max().unwrap_or(0);
        let mut source_col_to_visible = if structure.columns.is_empty() {
            Vec::new()
        } else {
            vec![None; max_source_col + 1]
        };
        for col in &structure.columns {
            if col.source_index >= source_col_to_visible.len() {
                source_col_to_visible.resize(col.source_index + 1, None);
            }
            source_col_to_visible[col.source_index] = Some(col.index);
        }

        let containing_width = match constraints.available_width {
            AvailableSpace::Definite(w) => Some(w),
            _ => None,
        };
        let containing_height = match constraints.available_height {
            AvailableSpace::Definite(h) => Some(h),
            _ => None,
        };

        // Honor explicit table width if present.
        let font_size = box_node.style.font_size;
        let specified_width = box_node
            .style
            .width
            .as_ref()
            .and_then(|len| resolve_length_against(len, font_size, containing_width));
        let min_width = resolve_opt_length_against(box_node.style.min_width.as_ref(), font_size, containing_width);
        let max_width = resolve_opt_length_against(box_node.style.max_width.as_ref(), font_size, containing_width);

        let table_width = specified_width
            .or(containing_width)
            .map(|w| clamp_to_min_max(w, min_width, max_width));
        // Table padding and borders (ignored for box sizing under collapsed model per CSS 2.1),
        // but we still track outer borders for percentage-height resolution.
        let resolve_abs = |l: &crate::style::values::Length| match l.unit {
            LengthUnit::Percent => 0.0,
            _ if l.unit.is_absolute() => l.to_px(),
            _ => l.value,
        };
        let _outer_border_left = resolve_abs(&box_node.style.border_left_width);
        let _outer_border_right = resolve_abs(&box_node.style.border_right_width);
        let outer_border_top = resolve_abs(&box_node.style.border_top_width);
        let outer_border_bottom = resolve_abs(&box_node.style.border_bottom_width);
        let outer_border_v = outer_border_top + outer_border_bottom;
        let pad_left = resolve_abs(&box_node.style.padding_left);
        let pad_right = resolve_abs(&box_node.style.padding_right);
        let pad_top = resolve_abs(&box_node.style.padding_top);
        let pad_bottom = resolve_abs(&box_node.style.padding_bottom);
        let (border_left, border_right, border_top, border_bottom) =
            if structure.border_collapse == BorderCollapse::Collapse {
                (0.0, 0.0, 0.0, 0.0)
            } else {
                (
                    resolve_abs(&box_node.style.border_left_width),
                    resolve_abs(&box_node.style.border_right_width),
                    resolve_abs(&box_node.style.border_top_width),
                    resolve_abs(&box_node.style.border_bottom_width),
                )
            };
        let padding_h = pad_left + pad_right;
        let padding_v = pad_top + pad_bottom;
        let border_h = border_left + border_right;
        let border_v = border_top + border_bottom;

        let specified_height = box_node
            .style
            .height
            .as_ref()
            .and_then(|len| resolve_length_against(len, font_size, containing_height));
        let min_height = resolve_opt_length_against(box_node.style.min_height.as_ref(), font_size, containing_height);
        let max_height = resolve_opt_length_against(box_node.style.max_height.as_ref(), font_size, containing_height);
        let table_height = specified_height.map(|h| clamp_to_min_max(h, min_height, max_height));

        // Helper to position out-of-flow children against a containing block.
        let place_out_of_flow = |fragment: &mut FragmentNode, cb: ContainingBlock| -> Result<(), LayoutError> {
            if positioned_children.is_empty() {
                return Ok(());
            }
            let abs = AbsoluteLayout::new();
            for child in &positioned_children {
                let mut layout_child = child.clone();
                let mut style = (*layout_child.style).clone();
                style.position = crate::style::position::Position::Static;
                layout_child.style = Arc::new(style);

                let factory = self.factory.with_positioned_cb(cb);
                let fc_type = layout_child
                    .formatting_context()
                    .unwrap_or(crate::style::display::FormattingContextType::Block);
                let fc = factory.create(fc_type);
                let child_constraints = LayoutConstraints::new(
                    AvailableSpace::Definite(cb.rect.size.width),
                    AvailableSpace::Definite(cb.rect.size.height),
                );
                let mut child_fragment = fc.layout(&layout_child, &child_constraints)?;
                let positioned_style =
                    resolve_positioned_style(&child.style, &cb, self.viewport_size, self.factory.font_context());
                let input = AbsoluteLayoutInput::new(positioned_style, child_fragment.bounds.size, cb.rect.origin);
                let result = abs.layout_absolute(&input, &cb)?;
                child_fragment.bounds = Rect::new(result.position, result.size);
                fragment.children.push(child_fragment);
            }
            Ok(())
        };

        if (structure.column_count == 0 || structure.row_count == 0) && captions.is_empty() {
            let width = specified_width.or(containing_width).unwrap_or(0.0);
            let height = table_height.unwrap_or(0.0);
            let mut fragment = FragmentNode::new_with_style(
                Rect::from_xywh(0.0, 0.0, width, height),
                FragmentContent::Block { box_id: None },
                vec![],
                box_node.style.clone(),
            );
            if !positioned_children.is_empty() {
                let cb = if box_node.style.position.is_positioned() {
                    let padding_origin = Point::new(border_left + pad_left, border_top + pad_top);
                    let padding_rect = Rect::new(
                        padding_origin,
                        crate::geometry::Size::new(
                            width - border_left - border_right,
                            height - border_top - border_bottom,
                        ),
                    );
                    ContainingBlock::with_viewport(padding_rect, self.viewport_size)
                } else {
                    self.nearest_positioned_cb
                };
                place_out_of_flow(&mut fragment, cb)?;
            }
            return Ok(fragment);
        }

        let mut column_constraints: Vec<ColumnConstraints> = (0..structure.column_count)
            .map(|_| ColumnConstraints::new(0.0, 0.0))
            .collect();
        let mode = if structure.is_fixed_layout {
            DistributionMode::Fixed
        } else {
            DistributionMode::Auto
        };
        let spacing = structure.total_horizontal_spacing();
        let edge_consumption = padding_h
            + if structure.border_collapse == BorderCollapse::Collapse {
                0.0
            } else {
                border_h
            };
        let percent_base = match (table_width, constraints.available_width) {
            (Some(w), _) => Some((w - spacing - edge_consumption).max(0.0)),
            (None, AvailableSpace::Definite(w)) => Some((w - spacing - edge_consumption).max(0.0)),
            _ => None,
        };

        self.populate_column_constraints(box_node, &structure, &mut column_constraints, mode, percent_base);
        self.normalize_percentage_constraints(&mut column_constraints, table_width, &constraints.available_width);

        let min_content_sum: f32 = column_constraints.iter().map(|c| c.min_width).sum();
        let max_content_sum: f32 = column_constraints.iter().map(|c| c.max_width).sum();
        let available_content = match (table_width, constraints.available_width) {
            (Some(w), _) => (w - spacing - edge_consumption).max(0.0),
            (None, AvailableSpace::Definite(w)) => (w - spacing - edge_consumption).max(0.0),
            (None, AvailableSpace::MinContent) => min_content_sum,
            (None, AvailableSpace::MaxContent) | (None, AvailableSpace::Indefinite) => max_content_sum,
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

        // If the table width is specified and columns don't fill the available content width, expand flexible columns.
        if table_width.is_some()
            && mode == DistributionMode::Auto
            && available_content.is_finite()
            && !col_widths.is_empty()
        {
            let current: f32 = col_widths.iter().sum();
            if available_content > current + 0.01 {
                let mut flex_indices: Vec<usize> = column_constraints
                    .iter()
                    .enumerate()
                    .filter(|(_, c)| c.is_flexible && c.fixed_width.is_none() && c.percentage.is_none())
                    .map(|(i, _)| i)
                    .collect();
                if flex_indices.is_empty() {
                    flex_indices.extend(0..col_widths.len());
                }
                let extra = available_content - current;
                let share = extra / flex_indices.len() as f32;
                for i in flex_indices {
                    if let Some(col) = col_widths.get_mut(i) {
                        *col += share;
                    }
                }
            }
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
                if let Ok(fragment) = self.layout_cell(cell_box, width, structure.border_collapse) {
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
        for (idx, row) in structure.rows.iter().enumerate() {
            if let Some(slot) = row_heights.get_mut(idx) {
                *slot = row.min_height.max(*slot);
            }
        }

        // Baseline-aligned, single-row cells reserve baseline space in their own row.
        for laid in &laid_out_cells {
            if laid.cell.rowspan > 1 || !laid.vertical_align.is_baseline_relative() {
                continue;
            }
            if let Some(baseline) = laid.baseline {
                let clamped = baseline.min(laid.height);
                row_heights[laid.cell.row] = row_heights[laid.cell.row].max(clamped);
            }
        }

        let percent_height_base = table_height.map(|base| {
            let mut content_base = if structure.border_collapse == BorderCollapse::Collapse {
                (base - padding_v - outer_border_v).max(0.0)
            } else {
                (base - padding_v - border_v).max(0.0)
            };
            if structure.border_collapse != BorderCollapse::Collapse {
                let spacing_total = v_spacing * (structure.row_count as f32 + 1.0);
                content_base = (content_base - spacing_total).max(0.0);
            }
            content_base
        });

        let row_floor = |idx: usize, current: f32| -> f32 {
            let row = structure.rows.get(idx);
            if row.is_none() {
                return current;
            }
            let row = row.unwrap();
            let mut floor = current;
            if let Some((min_len, _)) = Some(resolve_row_min_max(row, percent_height_base)) {
                if let Some(min) = min_len {
                    floor = floor.max(min);
                }
            }
            if let Some(spec) = row.specified_height {
                match spec {
                    SpecifiedHeight::Fixed(h) => floor = floor.max(h),
                    SpecifiedHeight::Percent(pct) => {
                        if let Some(base) = percent_height_base {
                            floor = floor.max((pct / 100.0) * base);
                        }
                    }
                    SpecifiedHeight::Auto => {}
                }
            }
            floor
        };

        for laid in &laid_out_cells {
            if laid.cell.rowspan == 1 {
                row_heights[laid.cell.row] = row_heights[laid.cell.row].max(laid.height);
            } else {
                let span_start = laid.cell.row;
                let span_end = (laid.cell.row + laid.cell.rowspan).min(structure.row_count);
                let spacing_total = v_spacing * (laid.cell.rowspan.saturating_sub(1) as f32);
                let span_height = (laid.height - spacing_total).max(0.0);
                let auto_rows: Vec<usize> = (span_start..span_end)
                    .filter(|idx| {
                        matches!(
                            structure.rows[*idx].specified_height,
                            Some(SpecifiedHeight::Auto) | None
                        )
                    })
                    .collect();
                let has_auto = !auto_rows.is_empty();
                let targets: Vec<usize> = if has_auto {
                    auto_rows.clone()
                } else {
                    (span_start..span_end).collect()
                };
                if !targets.is_empty() {
                    let non_target_sum: f32 = (span_start..span_end)
                        .filter(|idx| !targets.contains(idx))
                        .map(|idx| row_floor(idx, *row_heights.get(idx).unwrap_or(&0.0)))
                        .sum();
                    let remaining = (span_height - non_target_sum).max(0.0);
                    let use_proportional = !has_auto;
                    let base_sum: f32 = targets
                        .iter()
                        .map(|idx| row_floor(*idx, *row_heights.get(*idx).unwrap_or(&0.0)))
                        .sum();
                    let total_weight = if use_proportional && base_sum > 0.0 {
                        base_sum
                    } else {
                        targets.len() as f32
                    };

                    for &idx in &targets {
                        let base = row_floor(idx, *row_heights.get(idx).unwrap_or(&0.0));
                        let weight = if use_proportional && base_sum > 0.0 {
                            base / total_weight
                        } else {
                            1.0 / targets.len() as f32
                        };
                        if let Some(row) = row_heights.get_mut(idx) {
                            let share = remaining * weight;
                            *row = row.max(share);
                        }
                    }
                }
            }
        }

        // Preserve the content-driven minimums so later distribution never shrinks below cell content.
        let content_min_heights = row_heights.clone();

        let percent_height_base = table_height.map(|base| {
            let mut content_base = if structure.border_collapse == BorderCollapse::Collapse {
                (base - padding_v - outer_border_v).max(0.0)
            } else {
                (base - padding_v - border_v).max(0.0)
            };
            if structure.border_collapse != BorderCollapse::Collapse {
                let spacing_total = v_spacing * (structure.row_count as f32 + 1.0);
                content_base = (content_base - spacing_total).max(0.0);
            }
            content_base
        });

        // Enforce row-specified minimums (length or percentage of table height) and percent targets.
        for (idx, row) in structure.rows.iter().enumerate() {
            let (min_len, max_len) = resolve_row_min_max(row, percent_height_base);
            if let Some(min) = min_len {
                row_heights[idx] = row_heights[idx].max(min);
            }
            if let Some(max) = max_len {
                row_heights[idx] = row_heights[idx].min(max);
            }
            if let (Some(base), Some(SpecifiedHeight::Percent(pct))) = (percent_height_base, row.specified_height) {
                let target = (pct / 100.0) * base;
                row_heights[idx] = row_heights[idx].max(target);
            }
        }

        // If the table has a definite height, adjust rows so percent rows meet their targets and remaining space is distributed.
        if let Some(percent_base) = percent_height_base {
            let spacing_total = if structure.border_collapse == BorderCollapse::Collapse {
                0.0
            } else {
                v_spacing * (structure.row_count as f32 + 1.0)
            };
            let target_rows = (percent_base + spacing_total).max(0.0);
            let mut percent_rows = Vec::new();
            let mut adjustable_rows = Vec::new();
            for (idx, row) in structure.rows.iter().enumerate() {
                if matches!(row.specified_height, Some(SpecifiedHeight::Percent(_))) {
                    percent_rows.push(idx);
                } else {
                    adjustable_rows.push(idx);
                }
            }
            let percent_total: f32 = percent_rows.iter().map(|i| row_heights[*i]).sum();
            let flex_indices = if !adjustable_rows.is_empty() {
                adjustable_rows
            } else {
                percent_rows.clone()
            };

            let available = target_rows - percent_total;
            let flex_total: f32 = flex_indices.iter().map(|i| row_heights[*i]).sum();

            if available > 0.0 {
                if flex_total > 0.0 {
                    let scale = available / flex_total;
                    for i in &flex_indices {
                        row_heights[*i] = (row_heights[*i] * scale).max(content_min_heights[*i]);
                    }
                } else if !flex_indices.is_empty() {
                    let share = available / flex_indices.len() as f32;
                    for i in &flex_indices {
                        row_heights[*i] = share;
                    }
                }
            }
            for (idx, min_content) in content_min_heights.iter().enumerate() {
                row_heights[idx] = row_heights[idx].max(*min_content);
            }
            for (idx, row) in structure.rows.iter().enumerate() {
                let (min_len, max_len) = resolve_row_min_max(row, percent_height_base);
                if let Some(min) = min_len {
                    row_heights[idx] = row_heights[idx].max(min);
                }
                if let Some(max) = max_len {
                    row_heights[idx] = row_heights[idx].min(max);
                }
            }
        }

        let mut row_metrics: Vec<RowMetrics> = row_heights.iter().map(|h| RowMetrics::new(*h)).collect();

        let clamp_baseline = |laid: &LaidOutCell, row_height: f32| -> f32 {
            let baseline = laid.baseline.unwrap_or(laid.height);
            if row_height.is_finite() && row_height > 0.0 {
                baseline.min(row_height)
            } else {
                baseline
            }
        };

        for laid in &laid_out_cells {
            let row = &mut row_metrics[laid.cell.row];
            let row_height = row_heights.get(laid.cell.row).copied().unwrap_or(row.height);
            let contribution = if laid.cell.rowspan == 1 {
                laid.height
            } else {
                row_height
            };
            row.max_cell_height = row.max_cell_height.max(contribution);

            if laid.vertical_align.is_baseline_relative() {
                let baseline = clamp_baseline(laid, row_height);
                let top = baseline.min(row_height);
                let bottom = (row_height - top).max(0.0);
                row.has_baseline = true;
                row.baseline_top = row.baseline_top.max(top);
                row.baseline_bottom = row.baseline_bottom.max(bottom);
            }
        }

        // Spanning baseline-aligned cells reserve baseline space in the first row of the span.
        for laid in &laid_out_cells {
            if laid.cell.rowspan <= 1 || !laid.vertical_align.is_baseline_relative() {
                continue;
            }
            let row = &mut row_metrics[laid.cell.row];
            let row_height = row_heights.get(laid.cell.row).copied().unwrap_or(row.height);
            let baseline = clamp_baseline(laid, row_height);
            let top = baseline.min(row_height);
            let bottom = (row_height - top).max(0.0);
            row.has_baseline = true;
            row.baseline_top = row.baseline_top.max(top);
            row.baseline_bottom = row.baseline_bottom.max(bottom);
        }

        for (idx, row) in row_metrics.iter_mut().enumerate() {
            let baseline_height = row.baseline_height();
            let max_required = row.max_cell_height.max(baseline_height);
            if max_required > row.height {
                row.height = max_required;
            }
            if row.height <= 0.0 {
                row.height = 1.0;
            }
            let (min_h, max_h) = resolve_row_min_max(&structure.rows[idx], percent_height_base);
            if let Some(min) = min_h {
                row.height = row.height.max(min);
            }
            if let Some(max) = max_h {
                row.height = row.height.min(max);
            }
        }

        // If the table has a definite (or minimum) height and rows don't fill it, distribute the extra.
        if let Some(target_height) = table_height {
            let rows_space: f32 = row_metrics.iter().map(|r| r.height).sum();
            let spacing_total = if structure.border_collapse == BorderCollapse::Collapse {
                0.0
            } else {
                v_spacing * (structure.row_count as f32 + 1.0)
            };
            let target_rows = (target_height - spacing_total).max(0.0);
            if target_rows > rows_space && !row_metrics.is_empty() {
                let extra = target_rows - rows_space;
                let mut flex_indices: Vec<usize> = structure
                    .rows
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, row)| match row.specified_height {
                        Some(SpecifiedHeight::Fixed(_)) | Some(SpecifiedHeight::Percent(_)) => None,
                        _ => Some(idx),
                    })
                    .collect();
                if flex_indices.is_empty() {
                    // No auto rows: distribute across all rows proportionally to their current height.
                    flex_indices = (0..structure.row_count).collect();
                }

                let flex_sum: f32 = flex_indices
                    .iter()
                    .map(|i| row_metrics.get(*i).map(|r| r.height).unwrap_or(0.0))
                    .sum();
                if flex_sum > 0.0 {
                    for idx in flex_indices {
                        if let Some(row) = row_metrics.get_mut(idx) {
                            let weight = row.height / flex_sum;
                            row.height += extra * weight;
                        }
                    }
                } else {
                    let share = extra / flex_indices.len() as f32;
                    for idx in flex_indices {
                        if let Some(row) = row_metrics.get_mut(idx) {
                            row.height += share;
                        }
                    }
                }
            }
        }

        // Border-collapse adjustments
        let collapsed_borders = if structure.border_collapse == BorderCollapse::Collapse {
            compute_collapsed_borders(box_node, &structure)
        } else {
            CollapsedBorders {
                vertical: vec![vec![ResolvedBorder::none(); structure.row_count]; structure.column_count + 1],
                horizontal: vec![vec![ResolvedBorder::none(); structure.column_count]; structure.row_count + 1],
                corners: vec![vec![ResolvedBorder::none(); structure.column_count + 1]; structure.row_count + 1],
            }
        };

        let vertical_line_max: Vec<f32> = collapsed_borders
            .vertical
            .iter()
            .map(|segments| segments.iter().map(|b| b.width).fold(0.0, f32::max))
            .collect();
        let horizontal_line_max: Vec<f32> = collapsed_borders
            .horizontal
            .iter()
            .map(|segments| segments.iter().map(|b| b.width).fold(0.0, f32::max))
            .collect();

        let content_origin_y = if structure.border_collapse == BorderCollapse::Collapse {
            pad_top
        } else {
            border_top + pad_top
        };
        let mut row_offsets = Vec::with_capacity(structure.row_count);
        let mut y = if structure.border_collapse == BorderCollapse::Collapse {
            content_origin_y
        } else {
            content_origin_y + v_spacing
        };

        for (row_idx, row) in row_metrics.iter().enumerate() {
            if structure.border_collapse == BorderCollapse::Collapse {
                y += horizontal_line_max.get(row_idx).copied().unwrap_or(0.0);
            }
            row_offsets.push(y);
            y += row.height;
            if structure.border_collapse != BorderCollapse::Collapse {
                y += v_spacing;
            }
        }

        // Precompute column offsets for positioning
        let content_origin_x = if structure.border_collapse == BorderCollapse::Collapse {
            pad_left
        } else {
            border_left + pad_left
        };
        let mut col_offsets = Vec::with_capacity(structure.column_count);
        if structure.border_collapse == BorderCollapse::Collapse {
            let mut x = content_origin_x;
            for col_idx in 0..structure.column_count {
                x += vertical_line_max.get(col_idx).copied().unwrap_or(0.0);
                col_offsets.push(x);
                x += col_widths[col_idx];
            }
        } else {
            let mut x = content_origin_x + h_spacing;
            for col_idx in 0..structure.column_count {
                col_offsets.push(x);
                x += col_widths[col_idx] + h_spacing;
            }
        }

        let content_width: f32 = if structure.border_collapse == BorderCollapse::Collapse {
            col_widths.iter().sum::<f32>() + vertical_line_max.iter().copied().sum::<f32>()
        } else {
            col_widths.iter().sum::<f32>() + spacing
        };

        let content_height = if structure.row_count > 0 {
            if structure.border_collapse == BorderCollapse::Collapse {
                let mut h = horizontal_line_max.get(0).copied().unwrap_or(0.0);
                for (idx, row) in row_metrics.iter().enumerate() {
                    h += row.height;
                    h += horizontal_line_max.get(idx + 1).copied().unwrap_or(0.0);
                }
                h
            } else {
                row_offsets
                    .last()
                    .map(|start| start + row_metrics.last().map(|r| r.height).unwrap_or(0.0))
                    .unwrap_or(0.0)
                    + v_spacing
                    - content_origin_y
            }
        } else {
            0.0
        };

        // Column group and column backgrounds precede row backgrounds/cells.
        let mut column_styles: Vec<Option<Arc<ComputedStyle>>> = vec![None; structure.column_count];
        let mut column_groups: Vec<(usize, usize, Arc<ComputedStyle>)> = Vec::new();
        let mut source_col_idx = 0usize;
        for child in &box_node.children {
            match TableStructure::get_table_element_type(child) {
                TableElementType::Column => {
                    if let Some(visible) = source_col_to_visible.get(source_col_idx).and_then(|m| *m) {
                        column_styles[visible] = Some(child.style.clone());
                    }
                    source_col_idx += 1;
                }
                TableElementType::ColumnGroup => {
                    let mut first_visible = None;
                    let mut last_visible = None;
                    if !child.children.is_empty() {
                        for col_child in &child.children {
                            if TableStructure::get_table_element_type(col_child) == TableElementType::Column {
                                if let Some(visible) = source_col_to_visible.get(source_col_idx).and_then(|m| *m) {
                                    column_styles[visible] = Some(col_child.style.clone());
                                    first_visible.get_or_insert(visible);
                                    last_visible = Some(visible);
                                }
                                source_col_idx += 1;
                            }
                        }
                    } else {
                        if let Some(visible) = source_col_to_visible.get(source_col_idx).and_then(|m| *m) {
                            column_styles[visible] = Some(child.style.clone());
                            first_visible.get_or_insert(visible);
                            last_visible = Some(visible);
                        }
                        source_col_idx += 1;
                    }
                    if let (Some(start), Some(end)) = (first_visible, last_visible) {
                        column_groups.push((start, end + 1, child.style.clone()));
                    }
                }
                _ => {}
            }
        }

        let push_column_span_fragment = |fragments: &mut Vec<FragmentNode>,
                                         style: Arc<ComputedStyle>,
                                         start: usize,
                                         end: usize| {
            if start >= end || start >= col_offsets.len() {
                return;
            }
            if !style_paints_background_or_border(&style, false) {
                return;
            }
            let style = strip_borders(&style);
            let mut x = if structure.border_collapse == BorderCollapse::Collapse {
                col_offsets.get(start).copied().unwrap_or(0.0) - vertical_line_max.get(start).copied().unwrap_or(0.0)
            } else {
                (col_offsets.get(start).copied().unwrap_or(content_origin_x) - h_spacing * 0.5).max(content_origin_x)
            };
            let mut right = if structure.border_collapse == BorderCollapse::Collapse {
                let base = col_offsets.get(end.saturating_sub(1)).copied().unwrap_or(x)
                    + col_widths.get(end.saturating_sub(1)).copied().unwrap_or(0.0);
                base + vertical_line_max.get(end).copied().unwrap_or(0.0)
            } else {
                let base = col_offsets.get(end.saturating_sub(1)).copied().unwrap_or(x)
                    + col_widths.get(end.saturating_sub(1)).copied().unwrap_or(0.0);
                base + h_spacing * 0.5
            };
            let max_x = content_origin_x + content_width;
            x = x.max(content_origin_x);
            right = right.min(max_x);
            let width = (right - x).max(0.0);
            if width <= 0.0 || content_height <= 0.0 {
                return;
            }
            let rect = Rect::from_xywh(x, content_origin_y, width, content_height);
            fragments.push(FragmentNode::new_with_style(
                rect,
                FragmentContent::Block { box_id: None },
                Vec::new(),
                style,
            ));
        };

        for (start, end, style) in column_groups {
            push_column_span_fragment(&mut fragments, style, start, end);
        }
        for (idx, style) in column_styles.into_iter().enumerate() {
            if let Some(style) = style {
                push_column_span_fragment(&mut fragments, style, idx, idx + 1);
            }
        }

        // Paint order backgrounds before cells.
        // Row groups
        let mut row_styles: Vec<Option<Arc<ComputedStyle>>> = vec![None; structure.row_count];
        let mut row_groups: Vec<(usize, usize, Arc<ComputedStyle>)> = Vec::new();
        let mut row_cursor = 0usize;
        for child in &box_node.children {
            match TableStructure::get_table_element_type(child) {
                TableElementType::RowGroup | TableElementType::HeaderGroup | TableElementType::FooterGroup => {
                    let mut first_visible = None;
                    let mut last_visible = None;
                    for row_child in &child.children {
                        if TableStructure::get_table_element_type(row_child) == TableElementType::Row {
                            if let Some(visible) = source_row_to_visible.get(row_cursor).and_then(|m| *m) {
                                row_styles[visible] = Some(row_child.style.clone());
                                first_visible.get_or_insert(visible);
                                last_visible = Some(visible);
                            }
                            row_cursor += 1;
                        }
                    }
                    if let (Some(start), Some(end)) = (first_visible, last_visible) {
                        row_groups.push((start, end + 1, child.style.clone()));
                    }
                }
                TableElementType::Row => {
                    if let Some(visible) = source_row_to_visible.get(row_cursor).and_then(|m| *m) {
                        row_styles[visible] = Some(child.style.clone());
                    }
                    row_cursor += 1;
                }
                _ => {}
            }
        }

        for (start, end, style) in row_groups {
            let style = strip_borders(&style);
            if !style_paints_background_or_border(&style, false) {
                continue;
            }
            if start >= row_offsets.len() {
                break;
            }
            let top = row_offsets[start];
            let bottom = if end < row_offsets.len() {
                row_offsets[end]
            } else {
                row_offsets.last().copied().unwrap_or(top)
                    + row_metrics.last().map(|r| r.height).unwrap_or(0.0)
                    + if structure.border_collapse != BorderCollapse::Collapse {
                        v_spacing
                    } else {
                        0.0
                    }
            };
            let height = (bottom - top).max(0.0);
            let rect = Rect::from_xywh(content_origin_x, top, content_width, height);
            fragments.push(FragmentNode::new_with_style(
                rect,
                FragmentContent::Block { box_id: None },
                Vec::new(),
                style,
            ));
        }

        // Rows
        for (idx, style) in row_styles.into_iter().enumerate() {
            if let Some(style) = style {
                let style = strip_borders(&style);
                if !style_paints_background_or_border(&style, false) {
                    continue;
                }
                let top = row_offsets.get(idx).copied().unwrap_or(0.0);
                let height = row_metrics.get(idx).map(|r| r.height).unwrap_or(0.0);
                let rect = Rect::from_xywh(content_origin_x, top, content_width, height);
                fragments.push(FragmentNode::new_with_style(
                    rect,
                    FragmentContent::Block { box_id: None },
                    Vec::new(),
                    style,
                ));
            }
        }

        // Position cell fragments with vertical alignment within their row block
        for laid in laid_out_cells {
            let cell = laid.cell;
            // Compute horizontal position
            let x = if structure.border_collapse == BorderCollapse::Collapse {
                col_offsets.get(cell.col).copied().unwrap_or(0.0)
            } else {
                let mut x = content_origin_x + h_spacing;
                for col_idx in 0..cell.col {
                    x += col_widths[col_idx] + h_spacing;
                }
                x
            };

            let row_start = cell.row;
            let span_end = (cell.row + cell.rowspan).min(row_metrics.len());
            let spanned_height: f32 = if structure.border_collapse == BorderCollapse::Collapse {
                let mut h = horizontal_line_max.get(row_start).copied().unwrap_or(0.0);
                for r in row_start..span_end {
                    h += row_metrics[r].height;
                    h += horizontal_line_max.get(r + 1).copied().unwrap_or(0.0);
                }
                h
            } else {
                row_metrics[row_start..span_end].iter().map(|r| r.height).sum::<f32>()
                    + v_spacing * cell.rowspan.saturating_sub(1) as f32
            };

            let y_offset = match laid.vertical_align {
                VerticalAlign::Top => 0.0,
                VerticalAlign::Bottom => (spanned_height - laid.height).max(0.0),
                VerticalAlign::Middle => ((spanned_height - laid.height) / 2.0).max(0.0),
                _ => {
                    let baseline = clamp_baseline(&laid, row_metrics.get(row_start).map(|r| r.height).unwrap_or(0.0));
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

        let total_width = if structure.border_collapse == BorderCollapse::Collapse {
            content_width + padding_h
        } else {
            content_width + padding_h + border_h
        };
        let total_height = if let Some(specified) = table_height {
            specified
        } else {
            let mut h = content_height
                + if structure.border_collapse == BorderCollapse::Collapse {
                    padding_v
                } else {
                    padding_v + border_v
                };
            if let Some(max_h) = max_height {
                h = h.min(max_h);
            }
            if let Some(min_h) = min_height {
                h = h.max(min_h);
            }
            h
        };
        let table_bounds = Rect::from_xywh(0.0, 0.0, total_width.max(0.0), total_height);

        if structure.border_collapse == BorderCollapse::Collapse {
            let make_border_style = |color: Rgba,
                                     left: f32,
                                     right: f32,
                                     top: f32,
                                     bottom: f32,
                                     left_style: BorderStyle,
                                     right_style: BorderStyle,
                                     top_style: BorderStyle,
                                     bottom_style: BorderStyle|
             -> Arc<crate::style::ComputedStyle> {
                let mut style = crate::style::ComputedStyle::default();
                style.display = Display::Block;
                style.border_left_width = Length::px(left);
                style.border_right_width = Length::px(right);
                style.border_top_width = Length::px(top);
                style.border_bottom_width = Length::px(bottom);
                style.border_left_color = color;
                style.border_right_color = color;
                style.border_top_color = color;
                style.border_bottom_color = color;
                style.border_left_style = left_style;
                style.border_right_style = right_style;
                style.border_top_style = top_style;
                style.border_bottom_style = bottom_style;
                Arc::new(style)
            };

            let mut column_line_pos = Vec::with_capacity(structure.column_count + 1);
            let mut x_cursor = 0.0;
            column_line_pos.push(x_cursor);
            for col_idx in 0..structure.column_count {
                x_cursor += vertical_line_max.get(col_idx).copied().unwrap_or(0.0) + col_widths[col_idx];
                column_line_pos.push(x_cursor);
            }
            if let Some(last) = vertical_line_max.get(structure.column_count) {
                if let Some(end) = column_line_pos.last_mut() {
                    *end += *last;
                }
            }

            let mut row_line_pos = Vec::with_capacity(structure.row_count + 1);
            let mut y_cursor = 0.0;
            row_line_pos.push(y_cursor);
            for row_idx in 0..structure.row_count {
                y_cursor += horizontal_line_max.get(row_idx).copied().unwrap_or(0.0) + row_metrics[row_idx].height;
                row_line_pos.push(y_cursor);
            }
            if let Some(last) = horizontal_line_max.get(structure.row_count) {
                if let Some(end) = row_line_pos.last_mut() {
                    *end += *last;
                }
            }

            // Vertical grid lines: border before column 0, between columns, after last column.
            for col_idx in 0..collapsed_borders.vertical.len() {
                let base_x = column_line_pos.get(col_idx).copied().unwrap_or(0.0);
                for row_idx in 0..structure.row_count {
                    let border = &collapsed_borders.vertical[col_idx][row_idx];
                    if border.width <= 0.0 || matches!(border.style, BorderStyle::None | BorderStyle::Hidden) {
                        continue;
                    }
                    let x = base_x - border.width * 0.5;
                    let row_start = row_line_pos.get(row_idx).copied().unwrap_or(0.0);
                    let row_height = row_metrics[row_idx].height;
                    let top_adjacent = collapsed_borders
                        .horizontal
                        .get(row_idx)
                        .and_then(|row| row.get(col_idx))
                        .map(|b| b.width * 0.5)
                        .unwrap_or(0.0);
                    let bottom_adjacent = collapsed_borders
                        .horizontal
                        .get(row_idx + 1)
                        .and_then(|row| row.get(col_idx))
                        .map(|b| b.width * 0.5)
                        .unwrap_or(0.0);
                    let height = row_height + top_adjacent + bottom_adjacent;
                    let y = row_start - top_adjacent;
                    let rect = Rect::from_xywh(x, y, border.width, height);
                    let style = make_border_style(
                        border.color,
                        border.width,
                        0.0,
                        0.0,
                        0.0,
                        border.style,
                        BorderStyle::None,
                        BorderStyle::None,
                        BorderStyle::None,
                    );
                    fragments.push(FragmentNode::new_with_style(
                        rect,
                        FragmentContent::Block { box_id: None },
                        vec![],
                        style,
                    ));
                }
            }

            // Horizontal grid lines: border before row 0, between rows, after last row.
            for row_idx in 0..collapsed_borders.horizontal.len() {
                let base_y = row_line_pos.get(row_idx).copied().unwrap_or(0.0);
                for col_idx in 0..structure.column_count {
                    let border = &collapsed_borders.horizontal[row_idx][col_idx];
                    if border.width <= 0.0 || matches!(border.style, BorderStyle::None | BorderStyle::Hidden) {
                        continue;
                    }
                    let y = base_y - border.width * 0.5;
                    let col_start = column_line_pos.get(col_idx).copied().unwrap_or(0.0);
                    let col_width = col_widths[col_idx];
                    let left_adjacent = collapsed_borders
                        .vertical
                        .get(col_idx)
                        .and_then(|col| col.get(row_idx))
                        .map(|b| b.width * 0.5)
                        .unwrap_or(0.0);
                    let right_adjacent = collapsed_borders
                        .vertical
                        .get(col_idx + 1)
                        .and_then(|col| col.get(row_idx))
                        .map(|b| b.width * 0.5)
                        .unwrap_or(0.0);
                    let width = col_width + left_adjacent + right_adjacent;
                    let x = col_start - left_adjacent;
                    let rect = Rect::from_xywh(x, y, width, border.width);
                    let style = make_border_style(
                        border.color,
                        0.0,
                        0.0,
                        border.width,
                        0.0,
                        BorderStyle::None,
                        BorderStyle::None,
                        border.style,
                        BorderStyle::None,
                    );
                    fragments.push(FragmentNode::new_with_style(
                        rect,
                        FragmentContent::Block { box_id: None },
                        vec![],
                        style,
                    ));
                }
            }

            // Corner joins
            for r in 0..=structure.row_count {
                for c in 0..=structure.column_count {
                    let corner = &collapsed_borders.corners[r][c];
                    if corner.width <= 0.0 || matches!(corner.style, BorderStyle::None | BorderStyle::Hidden) {
                        continue;
                    }
                    let x = column_line_pos.get(c).copied().unwrap_or(0.0) - corner.width * 0.5;
                    let y = row_line_pos.get(r).copied().unwrap_or(0.0) - corner.width * 0.5;
                    let rect = Rect::from_xywh(x, y, corner.width, corner.width);
                    let style = make_border_style(
                        corner.color,
                        corner.width,
                        corner.width,
                        corner.width,
                        corner.width,
                        corner.style,
                        corner.style,
                        corner.style,
                        corner.style,
                    );
                    fragments.push(FragmentNode::new_with_style(
                        rect,
                        FragmentContent::Block { box_id: None },
                        vec![],
                        style,
                    ));
                }
            }
        }

        let mut table_style = (*box_node.style).clone();
        if structure.border_collapse == BorderCollapse::Collapse {
            table_style.border_top_width = crate::style::values::Length::px(0.0);
            table_style.border_right_width = crate::style::values::Length::px(0.0);
            table_style.border_bottom_width = crate::style::values::Length::px(0.0);
            table_style.border_left_width = crate::style::values::Length::px(0.0);
        }
        if captions.is_empty() {
            return Ok(FragmentNode::new_with_style(
                table_bounds,
                FragmentContent::Block { box_id: None },
                fragments,
                Arc::new(table_style),
            ));
        }

        // Only the table box itself should render backgrounds/borders. Element-level visual
        // effects (opacity/filters/transforms) apply to the wrapper that contains captions.
        table_style.transform.clear();
        table_style.filter.clear();
        table_style.backdrop_filter.clear();
        table_style.opacity = 1.0;
        table_style.mix_blend_mode = crate::style::types::MixBlendMode::Normal;
        table_style.isolation = crate::style::types::Isolation::Auto;

        let table_fragment = FragmentNode::new_with_style(
            table_bounds,
            FragmentContent::Block { box_id: None },
            fragments,
            Arc::new(table_style),
        );

        // Layout captions relative to the table's width.
        let mut wrapper_children = Vec::new();
        let mut offset_y = 0.0;

        let layout_caption = |caption: &BoxNode, y: f32| -> Result<(FragmentNode, f32), LayoutError> {
            let fc_type = caption
                .formatting_context()
                .unwrap_or(crate::style::display::FormattingContextType::Block);
            let fc = self.factory.create(fc_type);
            let mut frag = fc.layout(
                caption,
                &LayoutConstraints::new(
                    AvailableSpace::Definite(table_bounds.width()),
                    constraints.available_height,
                ),
            )?;
            frag.bounds = Rect::from_xywh(0.0, 0.0, table_bounds.width(), frag.bounds.height());
            Ok((frag.translate(Point::new(0.0, y)), frag.bounds.height()))
        };

        for caption in captions
            .iter()
            .copied()
            .filter(|c| matches!(c.style.caption_side, CaptionSide::Top))
        {
            let (frag, h) = layout_caption(caption, offset_y)?;
            offset_y += h;
            wrapper_children.push(frag);
        }

        let table_origin_y = offset_y;
        let table_translated = table_fragment.translate(Point::new(0.0, table_origin_y));
        offset_y += table_translated.bounds.height();
        wrapper_children.push(table_translated);

        for caption in captions
            .iter()
            .copied()
            .filter(|c| matches!(c.style.caption_side, CaptionSide::Bottom))
        {
            let (frag, h) = layout_caption(caption, offset_y)?;
            offset_y += h;
            wrapper_children.push(frag);
        }

        let mut wrapper_style = (*box_node.style).clone();
        // Keep transforms/opacity/filters on the wrapper so they apply to both caption and table,
        // but avoid painting an extra background/border around the combined area.
        wrapper_style.reset_background_to_initial();
        wrapper_style.border_top_width = crate::style::values::Length::px(0.0);
        wrapper_style.border_right_width = crate::style::values::Length::px(0.0);
        wrapper_style.border_bottom_width = crate::style::values::Length::px(0.0);
        wrapper_style.border_left_width = crate::style::values::Length::px(0.0);
        wrapper_style.border_top_style = BorderStyle::None;
        wrapper_style.border_right_style = BorderStyle::None;
        wrapper_style.border_bottom_style = BorderStyle::None;
        wrapper_style.border_left_style = BorderStyle::None;
        wrapper_style.box_shadow.clear();

        let mut wrapper_fragment = FragmentNode::new_with_style(
            Rect::from_xywh(0.0, 0.0, table_bounds.width(), offset_y),
            FragmentContent::Block { box_id: None },
            wrapper_children,
            Arc::new(wrapper_style),
        );

        if !positioned_children.is_empty() {
            // Containing block is the table's padding box when the table itself is positioned; otherwise, inherit.
            let cb = if box_node.style.position.is_positioned() {
                let padding_origin = Point::new(border_left + pad_left, table_origin_y + border_top + pad_top);
                let padding_rect = Rect::new(
                    padding_origin,
                    crate::geometry::Size::new(
                        table_bounds.width() - border_left - border_right,
                        table_bounds.height() - border_top - border_bottom,
                    ),
                );
                ContainingBlock::with_viewport(padding_rect, self.viewport_size)
            } else {
                self.nearest_positioned_cb
            };

            place_out_of_flow(&mut wrapper_fragment, cb)?;
        }

        Ok(wrapper_fragment)
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
            None,
        );

        let spacing = structure.total_horizontal_spacing();
        let edges = if structure.border_collapse == BorderCollapse::Collapse {
            0.0
        } else {
            let resolve_abs = |l: &crate::style::values::Length| match l.unit {
                LengthUnit::Percent => 0.0,
                _ if l.unit.is_absolute() => l.to_px(),
                _ => l.value,
            };
            resolve_abs(&box_node.style.padding_left)
                + resolve_abs(&box_node.style.padding_right)
                + resolve_abs(&box_node.style.border_left_width)
                + resolve_abs(&box_node.style.border_right_width)
        };
        let width = match mode {
            IntrinsicSizingMode::MinContent => {
                column_constraints.iter().map(|c| c.min_width).sum::<f32>() + spacing + edges
            }
            IntrinsicSizingMode::MaxContent => {
                column_constraints.iter().map(|c| c.max_width).sum::<f32>() + spacing + edges
            }
        };

        Ok(width.max(0.0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::layout::constraints::AvailableSpace;
    use crate::layout::constraints::LayoutConstraints;
    use crate::style::color::Rgba;
    use crate::style::computed::Visibility;
    use crate::style::display::Display;
    use crate::style::display::FormattingContextType;
    use crate::style::position::Position;
    use crate::style::types::{BorderCollapse, BorderStyle, CaptionSide, Direction, TableLayout, VerticalAlign};
    use crate::style::values::Length;
    use crate::style::ComputedStyle;
    use crate::text::font_loader::FontContext;
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

    fn collect_table_cell_tops(fragment: &FragmentNode, tops: &mut Vec<f32>) {
        if fragment
            .style
            .as_ref()
            .map(|s| matches!(s.display, Display::TableCell))
            .unwrap_or(false)
        {
            tops.push(fragment.bounds.y());
        }
        for child in &fragment.children {
            collect_table_cell_tops(child, tops);
        }
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
        let table = BoxNode::new_block(Arc::new(style), FormattingContextType::Table, vec![])
            .with_debug_info(DebugInfo::new(Some("table".to_string()), None, vec![]));

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
    fn test_table_width_respects_min_max() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.width = Some(Length::px(50.0));
        table_style.min_width = Some(Length::px(200.0));
        table_style.max_width = Some(Length::px(250.0));
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

        assert!((fragment.bounds.width() - 200.0).abs() < 0.1);
    }

    #[test]
    fn colspan_specified_width_increases_spanned_columns() {
        // First row has a single cell spanning two columns with a fixed width; it should raise the
        // combined minimum of the spanned columns.
        let mut span_style = ComputedStyle::default();
        span_style.display = Display::TableCell;
        span_style.width = Some(Length::px(200.0));
        let span_cell = BoxNode::new_block(Arc::new(span_style), FormattingContextType::Block, vec![])
            .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(2, 1));

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        let cell_a = BoxNode::new_block(Arc::new(cell_style.clone()), FormattingContextType::Block, vec![]);
        let cell_b = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;
        let row_with_span = BoxNode::new_block(
            Arc::new(row_style.clone()),
            FormattingContextType::Block,
            vec![span_cell],
        );
        let row_normal = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell_a, cell_b]);

        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);
        let table = BoxNode::new_block(
            Arc::new(table_style),
            FormattingContextType::Table,
            vec![row_with_span, row_normal],
        );

        let structure = TableStructure::from_box_tree(&table);
        let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
            .map(|_| ColumnConstraints::new(0.0, 0.0))
            .collect();
        let tfc = TableFormattingContext::new();
        tfc.populate_column_constraints(&table, &structure, &mut constraints, DistributionMode::Auto, None);

        let total_min: f32 = constraints.iter().map(|c| c.min_width).sum();
        assert!(
            total_min >= 200.0,
            "spanned cell width should contribute to column minima: {:?}",
            constraints.iter().map(|c| c.min_width).collect::<Vec<_>>()
        );
    }

    #[test]
    fn collapsed_border_hidden_suppresses_other_styles() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;
        row_style.border_top_style = BorderStyle::Hidden;
        row_style.border_top_width = Length::px(10.0);

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        cell_style.border_top_style = BorderStyle::Solid;
        cell_style.border_top_width = Length::px(4.0);

        let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let structure = TableStructure::from_box_tree(&table);
        let borders = compute_collapsed_borders(&table, &structure);

        assert_eq!(borders.horizontal.len(), 2);
        assert!((borders.horizontal[0][0].width - 0.0).abs() < f32::EPSILON);
    }

    #[test]
    fn collapsed_border_width_beats_style() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;

        let mut cell_left = ComputedStyle::default();
        cell_left.display = Display::TableCell;
        cell_left.border_right_style = BorderStyle::Solid;
        cell_left.border_right_width = Length::px(8.0);

        let mut cell_right = ComputedStyle::default();
        cell_right.display = Display::TableCell;
        cell_right.border_left_style = BorderStyle::Double;
        cell_right.border_left_width = Length::px(2.0);

        let left = BoxNode::new_block(Arc::new(cell_left), FormattingContextType::Block, vec![]);
        let right = BoxNode::new_block(Arc::new(cell_right), FormattingContextType::Block, vec![]);
        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![left, right]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let structure = TableStructure::from_box_tree(&table);
        let borders = compute_collapsed_borders(&table, &structure);

        assert_eq!(borders.vertical.len(), 3);
        assert!((borders.vertical[1][0].width - 8.0).abs() < f32::EPSILON);
        assert_eq!(borders.vertical[1][0].style, BorderStyle::Solid);
    }

    #[test]
    fn collapsed_borders_prefer_cell_over_row_on_equal_style() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;
        row_style.border_top_style = BorderStyle::Solid;
        row_style.border_top_width = Length::px(4.0);
        row_style.border_top_color = Rgba::from_rgba8(0, 0, 0, 255);

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        cell_style.border_top_style = BorderStyle::Solid;
        cell_style.border_top_width = Length::px(4.0);
        cell_style.border_top_color = Rgba::from_rgba8(0, 0, 255, 255);

        let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let structure = TableStructure::from_box_tree(&table);
        let borders = compute_collapsed_borders(&table, &structure);

        assert_eq!(borders.horizontal.len(), 2);
        let top_border = &borders.horizontal[0][0];
        assert_eq!(top_border.style, BorderStyle::Solid);
        assert_eq!(top_border.color, Rgba::from_rgba8(0, 0, 255, 255));
        assert!((top_border.width - 4.0).abs() < f32::EPSILON);
    }

    #[test]
    fn collapsed_borders_choose_left_cell_in_ltr_direction() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;

        let mut left_cell_style = ComputedStyle::default();
        left_cell_style.display = Display::TableCell;
        left_cell_style.border_right_style = BorderStyle::Solid;
        left_cell_style.border_right_width = Length::px(3.0);
        left_cell_style.border_right_color = Rgba::from_rgba8(255, 0, 0, 255);

        let mut right_cell_style = ComputedStyle::default();
        right_cell_style.display = Display::TableCell;
        right_cell_style.border_left_style = BorderStyle::Solid;
        right_cell_style.border_left_width = Length::px(3.0);
        right_cell_style.border_left_color = Rgba::from_rgba8(0, 255, 0, 255);

        let left = BoxNode::new_block(Arc::new(left_cell_style), FormattingContextType::Block, vec![]);
        let right = BoxNode::new_block(Arc::new(right_cell_style), FormattingContextType::Block, vec![]);
        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![left, right]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let structure = TableStructure::from_box_tree(&table);
        let borders = compute_collapsed_borders(&table, &structure);

        let middle_border = &borders.vertical[1][0];
        assert_eq!(middle_border.color, Rgba::from_rgba8(255, 0, 0, 255));
    }

    #[test]
    fn collapsed_borders_choose_right_cell_in_rtl_direction() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;
        table_style.direction = Direction::Rtl;

        let mut left_cell_style = ComputedStyle::default();
        left_cell_style.display = Display::TableCell;
        left_cell_style.border_right_style = BorderStyle::Solid;
        left_cell_style.border_right_width = Length::px(3.0);
        left_cell_style.border_right_color = Rgba::from_rgba8(255, 0, 0, 255);

        let mut right_cell_style = ComputedStyle::default();
        right_cell_style.display = Display::TableCell;
        right_cell_style.border_left_style = BorderStyle::Solid;
        right_cell_style.border_left_width = Length::px(3.0);
        right_cell_style.border_left_color = Rgba::from_rgba8(0, 255, 0, 255);

        let left = BoxNode::new_block(Arc::new(left_cell_style), FormattingContextType::Block, vec![]);
        let right = BoxNode::new_block(Arc::new(right_cell_style), FormattingContextType::Block, vec![]);
        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![left, right]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let structure = TableStructure::from_box_tree(&table);
        let borders = compute_collapsed_borders(&table, &structure);

        let middle_border = &borders.vertical[1][0];
        assert_eq!(middle_border.color, Rgba::from_rgba8(0, 255, 0, 255));
    }

    #[test]
    fn collapsed_borders_ignore_empty_cells_property() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;

        let mut left_cell_style = ComputedStyle::default();
        left_cell_style.display = Display::TableCell;
        left_cell_style.border_right_style = BorderStyle::Solid;
        left_cell_style.border_right_width = Length::px(4.0);
        left_cell_style.empty_cells = EmptyCells::Hide;

        let mut right_cell_style = ComputedStyle::default();
        right_cell_style.display = Display::TableCell;
        right_cell_style.border_left_style = BorderStyle::Solid;
        right_cell_style.border_left_width = Length::px(2.0);

        let left = BoxNode::new_block(Arc::new(left_cell_style), FormattingContextType::Block, vec![]);
        let right = BoxNode::new_block(Arc::new(right_cell_style), FormattingContextType::Block, vec![]);
        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![left, right]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let structure = TableStructure::from_box_tree(&table);
        let borders = compute_collapsed_borders(&table, &structure);

        assert_eq!(borders.vertical.len(), 3);
        let middle = &borders.vertical[1][0];
        assert!((middle.width - 4.0).abs() < f32::EPSILON, "empty-cells is ignored when borders are collapsed");
    }

    fn find_cell_fragment<'a>(fragment: &'a FragmentNode) -> Option<&'a FragmentNode> {
        if fragment
            .style
            .as_ref()
            .map(|s| s.display == Display::TableCell)
            .unwrap_or(false)
        {
            return Some(fragment);
        }
        for child in &fragment.children {
            if let Some(found) = find_cell_fragment(child) {
                return Some(found);
            }
        }
        None
    }

    #[test]
    fn row_backgrounds_are_emitted_before_cells() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;
        row_style.background_color = Rgba::from_rgba8(200, 0, 0, 255);

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
        let fragment = fc
            .layout(
                &table,
                &LayoutConstraints::new(AvailableSpace::Definite(100.0), AvailableSpace::Indefinite),
            )
            .expect("layout");

        assert!(
            !fragment.children.is_empty(),
            "table fragment should contain row background and cell fragments"
        );
        let row_frag = &fragment.children[0];
        let row_color = row_frag
            .style
            .as_ref()
            .map(|s| s.background_color)
            .expect("row fragment has style");
        assert_eq!(row_color, Rgba::from_rgba8(200, 0, 0, 255));
        let cell_frag = find_cell_fragment(&fragment).expect("cell fragment");
        assert!(row_frag.bounds.y() <= cell_frag.bounds.y());
    }

    #[test]
    fn collapsed_table_padding_offsets_content_and_preserves_total_height() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;
        table_style.padding_top = Length::px(10.0);
        table_style.padding_bottom = Length::px(10.0);
        table_style.padding_left = Length::px(5.0);
        table_style.padding_right = Length::px(5.0);
        table_style.height = Some(Length::px(100.0));

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        let cell = BoxNode::new_block(
            Arc::new(cell_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(ComputedStyle::default()),
                "data".to_string(),
            )],
        );
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
        let fragment = fc
            .layout(
                &table,
                &LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Definite(100.0)),
            )
            .expect("layout");

        // Total height honors the specified height.
        assert!((fragment.bounds.height() - 100.0).abs() < 0.01);

        // Content (rows/cells) starts after padding top.
        let cell_frag = find_cell_fragment(&fragment).expect("cell fragment");
        assert!((cell_frag.bounds.y() - 10.0).abs() < 0.1);
    }

    #[test]
    fn column_and_colgroup_backgrounds_paint_before_rows() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut colgroup_style = ComputedStyle::default();
        colgroup_style.display = Display::TableColumnGroup;
        colgroup_style.background_color = Rgba::from_rgba8(255, 0, 0, 255);
        colgroup_style.border_top_width = Length::px(3.0);
        colgroup_style.border_top_style = BorderStyle::Solid;

        let mut col_style = ComputedStyle::default();
        col_style.display = Display::TableColumn;
        col_style.background_color = Rgba::from_rgba8(0, 0, 255, 255);
        col_style.border_left_width = Length::px(2.0);
        col_style.border_left_style = BorderStyle::Solid;

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;
        row_style.background_color = Rgba::from_rgba8(0, 200, 0, 255);

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        let mut child_style = ComputedStyle::default();
        child_style.display = Display::Block;
        child_style.height = Some(Length::px(10.0));
        let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);

        let cell1 = BoxNode::new_block(
            Arc::new(cell_style.clone()),
            FormattingContextType::Block,
            vec![child.clone()],
        );
        let cell2 = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![child]);
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell1, cell2]);

        let col = BoxNode::new_block(Arc::new(col_style), FormattingContextType::Block, vec![]);
        let colgroup = BoxNode::new_block(Arc::new(colgroup_style), FormattingContextType::Block, vec![col]);

        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![colgroup, row]);

        let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
        let fragment = fc
            .layout(
                &table,
                &LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite),
            )
            .expect("layout");

        let colors: Vec<Rgba> = fragment
            .children
            .iter()
            .filter_map(|f| f.style.as_ref().map(|s| s.background_color))
            .collect();
        assert!(
            colors.len() >= 2,
            "column group and column backgrounds should be present before cells"
        );
        assert_eq!(colors[0], Rgba::from_rgba8(255, 0, 0, 255));
        assert_eq!(colors[1], Rgba::from_rgba8(0, 0, 255, 255));
        assert!(
            colors.contains(&Rgba::from_rgba8(0, 200, 0, 255)),
            "row background should still be painted"
        );
    }

    #[test]
    fn empty_cells_hide_strips_borders_in_separate_model() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        cell_style.empty_cells = EmptyCells::Hide;
        cell_style.border_top_width = Length::px(3.0);
        cell_style.border_right_width = Length::px(3.0);
        cell_style.border_bottom_width = Length::px(3.0);
        cell_style.border_left_width = Length::px(3.0);
        cell_style.border_top_style = BorderStyle::Solid;
        cell_style.border_right_style = BorderStyle::Solid;
        cell_style.border_bottom_style = BorderStyle::Solid;
        cell_style.border_left_style = BorderStyle::Solid;

        let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
        let fragment = fc
            .layout(
                &table,
                &LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite),
            )
            .expect("layout");

        let cell_frag = find_cell_fragment(&fragment).expect("cell fragment");
        let style = cell_frag.style.as_ref().expect("style");
        assert!(style.border_top_width.to_px().abs() < f32::EPSILON);
        assert!(style.border_right_width.to_px().abs() < f32::EPSILON);
        assert!(style.border_bottom_width.to_px().abs() < f32::EPSILON);
        assert!(style.border_left_width.to_px().abs() < f32::EPSILON);
    }

    #[test]
    fn intrinsic_widths_add_padding_once() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        cell_style.padding_left = Length::px(10.0);
        cell_style.padding_right = Length::px(10.0);

        let mut child_style = ComputedStyle::default();
        child_style.display = Display::Block;
        child_style.width = Some(Length::px(50.0));
        let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);

        let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![child]);
        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let structure = TableStructure::from_box_tree(&table);
        let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
            .map(|_| ColumnConstraints::new(0.0, 0.0))
            .collect();
        let tfc = TableFormattingContext::new();
        tfc.populate_column_constraints(&table, &structure, &mut constraints, DistributionMode::Auto, None);

        assert_eq!(constraints.len(), 1);
        let col = &constraints[0];
        // 50 content + 10+10 padding
        assert!(
            (col.min_width - 70.0).abs() < 0.5,
            "min width should include padding only once"
        );
    }

    #[test]
    fn collapsed_borders_choose_higher_style_on_equal_widths() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;

        let mut left_cell_style = ComputedStyle::default();
        left_cell_style.display = Display::TableCell;
        left_cell_style.border_right_style = BorderStyle::Double;
        left_cell_style.border_right_width = Length::px(3.0);
        left_cell_style.border_right_color = Rgba::from_rgba8(255, 0, 0, 255);

        let mut right_cell_style = ComputedStyle::default();
        right_cell_style.display = Display::TableCell;
        right_cell_style.border_left_style = BorderStyle::Dashed;
        right_cell_style.border_left_width = Length::px(3.0);
        right_cell_style.border_left_color = Rgba::from_rgba8(0, 255, 0, 255);

        let left = BoxNode::new_block(Arc::new(left_cell_style), FormattingContextType::Block, vec![]);
        let right = BoxNode::new_block(Arc::new(right_cell_style), FormattingContextType::Block, vec![]);
        let row = BoxNode::new_block(
            Arc::new(ComputedStyle {
                display: Display::TableRow,
                ..ComputedStyle::default()
            }),
            FormattingContextType::Block,
            vec![left, right],
        );
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let structure = TableStructure::from_box_tree(&table);
        let borders = compute_collapsed_borders(&table, &structure);

        let middle_border = &borders.vertical[1][0];
        assert_eq!(middle_border.style, BorderStyle::Double);
        assert!((middle_border.width - 3.0).abs() < f32::EPSILON);
        assert_eq!(middle_border.color, Rgba::from_rgba8(255, 0, 0, 255));
    }

    #[test]
    fn collapsed_borders_choose_top_cell_on_equal_borders() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;

        let mut top_cell_style = ComputedStyle::default();
        top_cell_style.display = Display::TableCell;
        top_cell_style.border_bottom_style = BorderStyle::Solid;
        top_cell_style.border_bottom_width = Length::px(4.0);
        top_cell_style.border_bottom_color = Rgba::from_rgba8(255, 0, 0, 255);

        let mut bottom_cell_style = ComputedStyle::default();
        bottom_cell_style.display = Display::TableCell;
        bottom_cell_style.border_top_style = BorderStyle::Solid;
        bottom_cell_style.border_top_width = Length::px(4.0);
        bottom_cell_style.border_top_color = Rgba::from_rgba8(0, 0, 255, 255);

        let top_row = BoxNode::new_block(
            Arc::new(ComputedStyle {
                display: Display::TableRow,
                ..ComputedStyle::default()
            }),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(top_cell_style),
                FormattingContextType::Block,
                vec![],
            )],
        );

        let bottom_row = BoxNode::new_block(
            Arc::new(ComputedStyle {
                display: Display::TableRow,
                ..ComputedStyle::default()
            }),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(bottom_cell_style),
                FormattingContextType::Block,
                vec![],
            )],
        );

        let table = BoxNode::new_block(
            Arc::new(table_style),
            FormattingContextType::Table,
            vec![top_row, bottom_row],
        );

        let structure = TableStructure::from_box_tree(&table);
        let borders = compute_collapsed_borders(&table, &structure);

        let horizontal_border = &borders.horizontal[1][0];
        assert_eq!(horizontal_border.style, BorderStyle::Solid);
        assert!((horizontal_border.width - 4.0).abs() < f32::EPSILON);
        assert_eq!(horizontal_border.color, Rgba::from_rgba8(255, 0, 0, 255));
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

        let mut cell2_style = ComputedStyle::default();
        cell2_style.display = Display::TableCell;
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
        let second_row = BoxNode::new_block(
            Arc::new(row_style),
            FormattingContextType::Block,
            vec![second_cell.clone()],
        );

        let table = BoxNode::new_block(
            Arc::new(table_style),
            FormattingContextType::Table,
            vec![first_row, second_row],
        );

        let tfc = TableFormattingContext::new();
        let constraints = LayoutConstraints::definite_width(200.0);
        let structure = TableStructure::from_box_tree(&table);
        let mut column_constraints: Vec<ColumnConstraints> = (0..structure.column_count)
            .map(|_| ColumnConstraints::new(0.0, 0.0))
            .collect();
        let spacing = structure.total_horizontal_spacing();
        let edge_consumption = 0.0;
        let available_content = (200.0 - spacing - edge_consumption).max(0.0);
        tfc.populate_column_constraints(
            &table,
            &structure,
            &mut column_constraints,
            DistributionMode::Fixed,
            Some(available_content),
        );
        let distribution =
            ColumnDistributor::new(DistributionMode::Fixed).distribute(&column_constraints, available_content);
        let fragment = tfc.layout(&table, &constraints).expect("table layout");

        // Expect first column to take ~50% of available space regardless of second row width.
        assert_eq!(distribution.widths.len(), 2);
        assert!((distribution.widths[0] - 100.0).abs() < 0.1);
        assert!((distribution.widths[1] - 100.0).abs() < 0.1);
        assert!((fragment.bounds.width() - 200.0).abs() < 0.1);
    }

    #[test]
    fn table_respects_specified_height() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.height = Some(Length::px(100.0));
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;

        let cell1 = BoxNode::new_block(Arc::new(cell_style.clone()), FormattingContextType::Block, vec![]);
        let row1 = BoxNode::new_block(Arc::new(row_style.clone()), FormattingContextType::Block, vec![cell1]);
        let cell2 = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row2 = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell2]);

        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row1, row2]);
        let tfc = TableFormattingContext::new();
        let fragment = tfc
            .layout(&table, &LayoutConstraints::definite(100.0, 200.0))
            .expect("table layout");

        assert!((fragment.bounds.height() - 100.0).abs() < 0.1);
    }

    #[test]
    fn percent_row_heights_resolve_when_table_height_definite() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.height = Some(Length::px(120.0));
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut row1_style = ComputedStyle::default();
        row1_style.display = Display::TableRow;
        row1_style.height = Some(Length::percent(25.0));

        let mut row2_style = ComputedStyle::default();
        row2_style.display = Display::TableRow;
        row2_style.height = Some(Length::percent(75.0));

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;

        let row1 = BoxNode::new_block(
            Arc::new(row1_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(cell_style.clone()),
                FormattingContextType::Block,
                vec![],
            )],
        );
        let row2 = BoxNode::new_block(
            Arc::new(row2_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(cell_style),
                FormattingContextType::Block,
                vec![],
            )],
        );

        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row1, row2]);
        let tfc = TableFormattingContext::new();
        let fragment = tfc
            .layout(&table, &LayoutConstraints::definite(100.0, 200.0))
            .expect("table layout");

        assert!((fragment.bounds.height() - 120.0).abs() < 0.1);
        assert!(fragment.children.len() >= 2);
        let first_y = fragment.children[0].bounds.y();
        let second_y = fragment.children[1].bounds.y();
        assert!((second_y - first_y - 30.0).abs() < 0.1);
    }

    #[test]
    fn percent_row_heights_account_for_spacing() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.height = Some(Length::px(120.0));
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(10.0);

        let mut row1_style = ComputedStyle::default();
        row1_style.display = Display::TableRow;
        row1_style.height = Some(Length::percent(50.0));

        let mut row2_style = ComputedStyle::default();
        row2_style.display = Display::TableRow;
        row2_style.height = Some(Length::percent(50.0));

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;

        let row1 = BoxNode::new_block(
            Arc::new(row1_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(cell_style.clone()),
                FormattingContextType::Block,
                vec![],
            )],
        );
        let row2 = BoxNode::new_block(
            Arc::new(row2_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(cell_style),
                FormattingContextType::Block,
                vec![],
            )],
        );

        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row1, row2]);
        let tfc = TableFormattingContext::new();
        let fragment = tfc
            .layout(&table, &LayoutConstraints::definite(100.0, 200.0))
            .expect("table layout");

        // Total spacing = 3 gaps * 10 = 30, so rows share the remaining 90 -> 45 each.
        assert!((fragment.bounds.height() - 120.0).abs() < 0.1);
        assert!(fragment.children.len() >= 2);
        let first_y = fragment.children[0].bounds.y();
        let second_y = fragment.children[1].bounds.y();
        assert!((second_y - first_y - 55.0).abs() < 0.1); // 45 height + 10 spacing
    }

    #[test]
    fn percent_rows_ignored_without_table_height() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.min_height = Some(Length::px(100.0));
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;
        row_style.height = Some(Length::percent(50.0));

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;

        let row1 = BoxNode::new_block(
            Arc::new(row_style.clone()),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(cell_style.clone()),
                FormattingContextType::Block,
                vec![],
            )],
        );
        let row2 = BoxNode::new_block(
            Arc::new(row_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(cell_style),
                FormattingContextType::Block,
                vec![],
            )],
        );

        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row1, row2]);
        let tfc = TableFormattingContext::new();
        let fragment = tfc
            .layout(&table, &LayoutConstraints::definite(100.0, 400.0))
            .expect("table layout");

        // Percent rows should not use the table min-height as a percentage base; they should collapse to intrinsic size.
        assert!(fragment.children.len() >= 2);
        let first_y = fragment.children[0].bounds.y();
        let second_y = fragment.children[1].bounds.y();
        assert!((second_y - first_y) < 10.0);
        assert!(fragment.bounds.height() >= 100.0);
    }

    #[test]
    fn percent_rows_fill_target_height() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.height = Some(Length::px(200.0));
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut row1_style = ComputedStyle::default();
        row1_style.display = Display::TableRow;
        row1_style.height = Some(Length::percent(50.0));

        let mut row2_style = ComputedStyle::default();
        row2_style.display = Display::TableRow;
        // auto row with small intrinsic height

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;

        let row1 = BoxNode::new_block(
            Arc::new(row1_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(cell_style.clone()),
                FormattingContextType::Block,
                vec![],
            )],
        );
        let row2 = BoxNode::new_block(
            Arc::new(row2_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(cell_style),
                FormattingContextType::Block,
                vec![],
            )],
        );

        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row1, row2]);
        let tfc = TableFormattingContext::new();
        let fragment = tfc
            .layout(&table, &LayoutConstraints::definite(100.0, 300.0))
            .expect("table layout");

        assert!((fragment.bounds.height() - 200.0).abs() < 0.1);
        assert_eq!(fragment.children.len(), 2);
        let first_y = fragment.children[0].bounds.y();
        let second_y = fragment.children[1].bounds.y();
        assert!((second_y - first_y - 100.0).abs() < 0.5);
    }

    #[test]
    fn percent_rows_overflow_do_not_shrink_content() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.height = Some(Length::px(100.0));
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut row1_style = ComputedStyle::default();
        row1_style.display = Display::TableRow;
        row1_style.height = Some(Length::percent(60.0));

        let mut row2_style = ComputedStyle::default();
        row2_style.display = Display::TableRow;
        row2_style.height = Some(Length::percent(60.0));

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;

        let tall_child = BoxNode::new_block(
            Arc::new(ComputedStyle {
                height: Some(Length::px(80.0)),
                display: Display::Block,
                ..ComputedStyle::default()
            }),
            FormattingContextType::Block,
            vec![],
        );

        let row1 = BoxNode::new_block(
            Arc::new(row1_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(cell_style.clone()),
                FormattingContextType::Block,
                vec![tall_child.clone()],
            )],
        );
        let row2 = BoxNode::new_block(
            Arc::new(row2_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(cell_style),
                FormattingContextType::Block,
                vec![tall_child],
            )],
        );

        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row1, row2]);
        let tfc = TableFormattingContext::new();
        let fragment = tfc
            .layout(&table, &LayoutConstraints::definite(100.0, 300.0))
            .expect("table layout");

        assert_eq!(fragment.children.len(), 2);
        let first_y = fragment.children[0].bounds.y();
        let second_y = fragment.children[1].bounds.y();
        // Even though the percentages sum to 120% of the table height, rows must not shrink below their 80px content.
        assert!(second_y - first_y >= 79.9);
    }

    #[test]
    fn percent_rows_over_100_percent_do_not_scale_down() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.height = Some(Length::px(100.0));
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut row1_style = ComputedStyle::default();
        row1_style.display = Display::TableRow;
        row1_style.height = Some(Length::percent(60.0));

        let mut row2_style = ComputedStyle::default();
        row2_style.display = Display::TableRow;
        row2_style.height = Some(Length::percent(60.0));

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;

        let row1 = BoxNode::new_block(
            Arc::new(row1_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(cell_style.clone()),
                FormattingContextType::Block,
                vec![],
            )],
        );
        let row2 = BoxNode::new_block(
            Arc::new(row2_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(cell_style),
                FormattingContextType::Block,
                vec![],
            )],
        );

        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row1, row2]);
        let tfc = TableFormattingContext::new();
        let fragment = tfc
            .layout(&table, &LayoutConstraints::definite(200.0, 200.0))
            .expect("table layout");

        assert_eq!(fragment.children.len(), 2);
        let first_y = fragment.children[0].bounds.y();
        let second_y = fragment.children[1].bounds.y();
        // Percent rows totalling 120% of the table height should keep their targets even if the table overflows.
        assert!((second_y - first_y - 60.0).abs() < 0.5);
        assert!((fragment.bounds.height() - 100.0).abs() < 0.1);
    }

    #[test]
    fn table_padding_offsets_children_and_expands_bounds() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);
        table_style.padding_left = Length::px(5.0);
        table_style.padding_right = Length::px(5.0);
        table_style.padding_top = Length::px(6.0);
        table_style.padding_bottom = Length::px(4.0);

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        cell_style.width = Some(Length::px(10.0));

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut child_style = ComputedStyle::default();
        child_style.display = Display::Block;
        child_style.height = Some(Length::px(8.0));
        child_style.width = Some(Length::px(10.0));

        let cell = BoxNode::new_block(
            Arc::new(cell_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(child_style),
                FormattingContextType::Block,
                vec![],
            )],
        );
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let tfc = TableFormattingContext::new();
        let fragment = tfc
            .layout(&table, &LayoutConstraints::min_content())
            .expect("table layout");

        let child = &fragment.children[0];
        assert!((child.bounds.x() - 5.0).abs() < 0.1);
        assert!((child.bounds.y() - 6.0).abs() < 0.1);
        assert!((fragment.bounds.width() - 20.0).abs() < 0.1); // 10 content + 5+5 padding
        assert!((fragment.bounds.height() - 18.0).abs() < 0.1); // 8 content + 6+4 padding
    }

    #[test]
    fn table_borders_expand_bounds() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);
        table_style.border_left_width = Length::px(3.0);
        table_style.border_right_width = Length::px(2.0);
        table_style.border_top_width = Length::px(4.0);
        table_style.border_bottom_width = Length::px(1.0);

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        cell_style.width = Some(Length::px(10.0));

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut child_style = ComputedStyle::default();
        child_style.display = Display::Block;
        child_style.height = Some(Length::px(5.0));
        child_style.width = Some(Length::px(10.0));

        let cell = BoxNode::new_block(
            Arc::new(cell_style),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(child_style),
                FormattingContextType::Block,
                vec![],
            )],
        );
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let tfc = TableFormattingContext::new();
        let fragment = tfc
            .layout(&table, &LayoutConstraints::min_content())
            .expect("table layout");

        let child = &fragment.children[0];
        assert!((child.bounds.x() - 3.0).abs() < 0.1);
        assert!((child.bounds.y() - 4.0).abs() < 0.1);
        assert!((fragment.bounds.width() - 15.0).abs() < 0.1); // 10 content + 3 + 2 borders
        assert!((fragment.bounds.height() - 10.0).abs() < 0.1); // 5 content + 4 + 1 borders
    }

    #[test]
    fn table_padding_and_borders_reduce_available_width() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.width = Some(Length::px(100.0));
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);
        table_style.padding_left = Length::px(5.0);
        table_style.padding_right = Length::px(5.0);
        table_style.border_left_width = Length::px(2.0);
        table_style.border_right_width = Length::px(2.0);

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        cell_style.min_width = Some(Length::px(10.0));
        cell_style.max_width = Some(Length::px(1000.0));

        let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row = BoxNode::new_block(
            Arc::new({
                let mut s = ComputedStyle::default();
                s.display = Display::TableRow;
                s
            }),
            FormattingContextType::Block,
            vec![cell],
        );
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let tfc = TableFormattingContext::new();
        let fragment = tfc
            .layout(&table, &LayoutConstraints::definite(100.0, 200.0))
            .expect("table layout");

        let child = &fragment.children[0];
        // Content width should be table width minus padding/borders: 100 - (5+5) - (2+2) = 86.
        assert!((child.bounds.width() - 86.0).abs() < 0.1);
        // Child should start after left border+padding.
        assert!((child.bounds.x() - 7.0).abs() < 0.1);
        assert!((fragment.bounds.width() - 100.0).abs() < 0.1);
    }

    #[test]
    fn table_intrinsic_width_includes_padding_and_borders() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);
        table_style.padding_left = Length::px(5.0);
        table_style.padding_right = Length::px(5.0);
        table_style.border_left_width = Length::px(2.0);
        table_style.border_right_width = Length::px(2.0);

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        cell_style.min_width = Some(Length::px(10.0));

        let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row = BoxNode::new_block(
            Arc::new({
                let mut s = ComputedStyle::default();
                s.display = Display::TableRow;
                s
            }),
            FormattingContextType::Block,
            vec![cell],
        );
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let tfc = TableFormattingContext::new();
        let min_width = tfc
            .compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MinContent)
            .expect("intrinsic size");
        // Expect min-content = cell min (10) + padding (10) + borders (4) = 24.
        assert!((min_width - 24.0).abs() < 0.1);
    }

    #[test]
    fn table_max_height_clamps_bounds() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.max_height = Some(Length::px(40.0));
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;

        let tall_cell = BoxNode::new_block(
            Arc::new(cell_style.clone()),
            FormattingContextType::Block,
            vec![BoxNode::new_block(
                Arc::new(ComputedStyle {
                    height: Some(Length::px(100.0)),
                    display: Display::Block,
                    ..ComputedStyle::default()
                }),
                FormattingContextType::Block,
                vec![],
            )],
        );
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![tall_cell]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let tfc = TableFormattingContext::new();
        let fragment = tfc
            .layout(&table, &LayoutConstraints::definite(100.0, 200.0))
            .expect("table layout");
        assert!(fragment.bounds.height() <= 40.0 + 0.01);
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
    fn row_group_visibility_collapse_removes_rows_and_cells() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut collapsed_group_style = ComputedStyle::default();
        collapsed_group_style.display = Display::TableRowGroup;
        collapsed_group_style.visibility = Visibility::Collapse;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;

        let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);
        let row_group = BoxNode::new_block(Arc::new(collapsed_group_style), FormattingContextType::Block, vec![row]);

        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row_group]);
        let structure = TableStructure::from_box_tree(&table);

        assert_eq!(structure.row_count, 0);
        assert!(structure.cells.is_empty());
    }

    #[test]
    fn column_group_visibility_collapse_removes_columns() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut col_style = ComputedStyle::default();
        col_style.display = Display::TableColumn;

        let mut collapsed_col_group_style = ComputedStyle::default();
        collapsed_col_group_style.display = Display::TableColumnGroup;
        collapsed_col_group_style.visibility = Visibility::Collapse;

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![])
            .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(2, 1));
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);

        let table = BoxNode::new_block(
            Arc::new(table_style),
            FormattingContextType::Table,
            vec![
                BoxNode::new_block(Arc::new(col_style.clone()), FormattingContextType::Block, vec![]),
                BoxNode::new_block(
                    Arc::new(collapsed_col_group_style),
                    FormattingContextType::Block,
                    vec![],
                ),
                row,
            ],
        );

        let structure = TableStructure::from_box_tree(&table);
        assert_eq!(structure.column_count, 1);
        assert_eq!(structure.cells.len(), 1);
        let cell_info = &structure.cells[0];
        assert_eq!(cell_info.colspan, 1);
        assert_eq!(cell_info.col, 0);
    }

    #[test]
    fn visibility_collapse_filters_rows_and_cells() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut collapsed_row_style = row_style.clone();
        collapsed_row_style.visibility = Visibility::Collapse;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;

        let collapsed_cell = BoxNode::new_block(Arc::new(cell_style.clone()), FormattingContextType::Block, vec![]);
        let visible_cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let collapsed_row = BoxNode::new_block(
            Arc::new(collapsed_row_style),
            FormattingContextType::Block,
            vec![collapsed_cell],
        );
        let visible_row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![visible_cell]);

        let table = BoxNode::new_block(
            Arc::new(table_style),
            FormattingContextType::Table,
            vec![collapsed_row, visible_row],
        );

        let structure = TableStructure::from_box_tree(&table);
        assert_eq!(structure.row_count, 1);
        assert_eq!(structure.column_count, 1);
        assert_eq!(structure.rows[0].source_index, 1);
        assert_eq!(structure.cells.len(), 1);
        let cell = &structure.cells[0];
        assert_eq!(cell.row, 0);
        assert_eq!(cell.source_row, 1);
    }

    #[test]
    fn visibility_collapse_filters_columns_and_adjusts_colspan() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut col_style = ComputedStyle::default();
        col_style.display = Display::TableColumn;

        let mut collapsed_col_style = col_style.clone();
        collapsed_col_style.visibility = Visibility::Collapse;

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;

        let spanning_cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![])
            .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(2, 1));
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![spanning_cell]);

        let table = BoxNode::new_block(
            Arc::new(table_style),
            FormattingContextType::Table,
            vec![
                BoxNode::new_block(Arc::new(col_style), FormattingContextType::Block, vec![]),
                BoxNode::new_block(Arc::new(collapsed_col_style), FormattingContextType::Block, vec![]),
                row,
            ],
        );

        let structure = TableStructure::from_box_tree(&table);
        assert_eq!(structure.column_count, 1);
        assert_eq!(structure.columns[0].source_index, 0);
        assert_eq!(structure.cells.len(), 1);
        let cell = &structure.cells[0];
        assert_eq!(cell.colspan, 1);
        assert_eq!(cell.source_col, 0);
    }

    #[test]
    fn test_column_info_new() {
        let col = ColumnInfo::new(5);
        assert_eq!(col.index, 5);
        assert_eq!(col.source_index, 5);
        assert_eq!(col.visibility, Visibility::Visible);
        assert!(col.specified_width.is_none());
        assert_eq!(col.min_width, 0.0);
        assert_eq!(col.computed_width, 0.0);
    }

    #[test]
    fn test_row_info_new() {
        let row = RowInfo::new(3);
        assert_eq!(row.index, 3);
        assert_eq!(row.source_index, 3);
        assert_eq!(row.visibility, Visibility::Visible);
        assert!(row.specified_height.is_none());
        assert!(row.author_min_height.is_none());
        assert!(row.author_max_height.is_none());
        assert_eq!(row.min_height, 0.0);
        assert_eq!(row.y_position, 0.0);
    }

    #[test]
    fn test_cell_info_new() {
        let cell = CellInfo::new(0, 2, 3);
        assert_eq!(cell.index, 0);
        assert_eq!(cell.source_row, 2);
        assert_eq!(cell.source_col, 3);
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
                source_index: 0,
                visibility: Visibility::Visible,
                specified_width: Some(SpecifiedWidth::Fixed(100.0)),
                min_width: 0.0,
                max_width: f32::INFINITY,
                computed_width: 0.0,
            },
            ColumnInfo {
                index: 1,
                source_index: 1,
                visibility: Visibility::Visible,
                specified_width: None,
                min_width: 0.0,
                max_width: f32::INFINITY,
                computed_width: 0.0,
            },
            ColumnInfo {
                index: 2,
                source_index: 2,
                visibility: Visibility::Visible,
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
    fn absolute_child_inherits_nearest_positioned_cb_in_table() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;

        let mut abs_style = ComputedStyle::default();
        abs_style.display = Display::Block;
        abs_style.position = Position::Absolute;
        abs_style.left = Some(Length::px(5.0));
        abs_style.top = Some(Length::px(7.0));
        abs_style.width = Some(Length::px(12.0));
        abs_style.height = Some(Length::px(9.0));

        let abs_child = BoxNode::new_block(Arc::new(abs_style), FormattingContextType::Block, vec![]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![abs_child]);

        let viewport = crate::geometry::Size::new(400.0, 400.0);
        let cb_rect = crate::geometry::Rect::from_xywh(30.0, 40.0, 200.0, 200.0);
        let cb = ContainingBlock::with_viewport(cb_rect, viewport);
        let factory = FormattingContextFactory::with_font_context_viewport_and_cb(FontContext::new(), viewport, cb);
        let fc = TableFormattingContext::with_factory(factory);

        let constraints = LayoutConstraints::definite(120.0, 120.0);
        let fragment = fc.layout(&table, &constraints).unwrap();

        assert!(!fragment.children.is_empty());
        let abs_fragment = fragment.children.last().unwrap();
        assert_eq!(abs_fragment.bounds.x(), 35.0);
        assert_eq!(abs_fragment.bounds.y(), 47.0);
        assert_eq!(abs_fragment.bounds.width(), 12.0);
        assert_eq!(abs_fragment.bounds.height(), 9.0);
    }

    #[test]
    fn test_fixed_layout_percentage_width() {
        let mut structure = TableStructure::new();
        structure.column_count = 2;
        structure.columns = vec![
            ColumnInfo {
                index: 0,
                source_index: 0,
                visibility: Visibility::Visible,
                specified_width: Some(SpecifiedWidth::Percent(25.0)),
                min_width: 0.0,
                max_width: f32::INFINITY,
                computed_width: 0.0,
            },
            ColumnInfo {
                index: 1,
                source_index: 1,
                visibility: Visibility::Visible,
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
                source_row: 0,
                source_col: 0,
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
                source_row: 0,
                source_col: 1,
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
                source_row: 0,
                source_col: 0,
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
                source_row: 0,
                source_col: 1,
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
            source_row: 0,
            source_col: 0,
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
                source_row: 0,
                source_col: 0,
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
                source_row: 0,
                source_col: 1,
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
                source_row: 1,
                source_col: 0,
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
            source_row: 0,
            source_col: 0,
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
    fn test_row_percent_with_available_height() {
        let mut structure = TableStructure::new();
        structure.row_count = 2;
        structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
        structure.rows[0].specified_height = Some(SpecifiedHeight::Percent(50.0));
        structure.rows[1].min_height = 10.0;
        structure.border_spacing = (0.0, 0.0);

        calculate_row_heights(&mut structure, Some(200.0));

        assert!((structure.rows[0].computed_height - 100.0).abs() < 0.01);
        assert!((structure.rows[1].computed_height - 100.0).abs() < 0.01);
    }

    #[test]
    fn calculate_row_heights_respects_collapse_spacing() {
        let mut structure = TableStructure::new();
        structure.row_count = 2;
        structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
        structure.border_spacing = (5.0, 5.0);
        structure.border_collapse = BorderCollapse::Collapse;

        calculate_row_heights(&mut structure, Some(100.0));

        // In collapsed mode, spacing is ignored for row positioning and distribution.
        assert!((structure.rows[0].computed_height - 50.0).abs() < 0.01);
        assert!((structure.rows[1].computed_height - 50.0).abs() < 0.01);
        assert!((structure.rows[0].y_position - 0.0).abs() < 0.01);
        assert!((structure.rows[1].y_position - 50.0).abs() < 0.01);
    }

    #[test]
    fn cell_baseline_falls_back_to_bottom() {
        let fragment = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 10.0, 20.0), vec![]);
        let baseline = cell_baseline(&fragment).expect("baseline");
        assert!((baseline - 20.0).abs() < 0.01);
    }

    fn make_style(display: Display) -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.display = display;
        Arc::new(style)
    }

    #[test]
    fn caption_default_positions_above_table() {
        let cell = BoxNode::new_block(
            make_style(Display::TableCell),
            FormattingContextType::Block,
            vec![BoxNode::new_text(make_style(Display::Inline), "data".to_string())],
        );
        let row = BoxNode::new_block(make_style(Display::TableRow), FormattingContextType::Block, vec![cell]);
        let tbody = BoxNode::new_block(
            make_style(Display::TableRowGroup),
            FormattingContextType::Block,
            vec![row],
        );
        let caption = BoxNode::new_block(
            make_style(Display::TableCaption),
            FormattingContextType::Block,
            vec![BoxNode::new_text(make_style(Display::Inline), "caption".to_string())],
        );
        let table = BoxNode::new_block(
            make_style(Display::Table),
            FormattingContextType::Table,
            vec![caption, tbody],
        );

        let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
        let fragment = fc
            .layout(
                &table,
                &LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite),
            )
            .expect("layout");

        assert_eq!(fragment.children.len(), 2, "wrapper should contain caption and table");
        let caption_frag = &fragment.children[0];
        let table_frag = &fragment.children[1];
        assert!((fragment.bounds.width() - table_frag.bounds.width()).abs() < 0.01);
        assert!((caption_frag.bounds.width() - table_frag.bounds.width()).abs() < 0.01);
        assert!(caption_frag.bounds.y() <= 0.0 + 1e-3);
        assert!((table_frag.bounds.y() - caption_frag.bounds.height()).abs() < 0.2);
        assert!((fragment.bounds.height() - (caption_frag.bounds.height() + table_frag.bounds.height())).abs() < 0.2);
    }

    #[test]
    fn caption_side_bottom_positions_after_table() {
        let mut caption_style = ComputedStyle::default();
        caption_style.display = Display::TableCaption;
        caption_style.caption_side = CaptionSide::Bottom;

        let cell = BoxNode::new_block(
            make_style(Display::TableCell),
            FormattingContextType::Block,
            vec![BoxNode::new_text(make_style(Display::Inline), "data".to_string())],
        );
        let row = BoxNode::new_block(make_style(Display::TableRow), FormattingContextType::Block, vec![cell]);
        let tbody = BoxNode::new_block(
            make_style(Display::TableRowGroup),
            FormattingContextType::Block,
            vec![row],
        );
        let caption = BoxNode::new_block(
            Arc::new(caption_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(make_style(Display::Inline), "caption".to_string())],
        );
        let table = BoxNode::new_block(
            make_style(Display::Table),
            FormattingContextType::Table,
            vec![tbody, caption],
        );

        let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
        let fragment = fc
            .layout(
                &table,
                &LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite),
            )
            .expect("layout");

        assert_eq!(fragment.children.len(), 2, "wrapper should contain table then caption");
        let table_frag = &fragment.children[0];
        let caption_frag = &fragment.children[1];
        assert!(caption_frag.bounds.y() >= table_frag.bounds.height() - 0.2);
        assert!((fragment.bounds.height() - (caption_frag.bounds.height() + table_frag.bounds.height())).abs() < 0.2);
    }

    #[test]
    fn calculate_row_heights_prefers_auto_rows_for_rowspan_extra() {
        let mut structure = TableStructure::new();
        structure.row_count = 2;
        structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
        structure.rows[0].specified_height = Some(SpecifiedHeight::Fixed(50.0));
        structure.cells = vec![CellInfo {
            index: 0,
            source_row: 0,
            source_col: 0,
            row: 0,
            col: 0,
            rowspan: 2,
            colspan: 1,
            box_index: 0,
            min_width: 0.0,
            max_width: 0.0,
            min_height: 120.0,
            bounds: Rect::ZERO,
        }];
        structure.border_spacing = (0.0, 0.0);

        calculate_row_heights(&mut structure, None);

        assert!((structure.rows[0].computed_height - 50.0).abs() < 0.01);
        assert!((structure.rows[1].computed_height - 70.0).abs() < 0.01);
    }

    #[test]
    fn calculate_row_heights_with_percent_row_and_rowspan_extra() {
        let mut structure = TableStructure::new();
        structure.row_count = 2;
        structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
        structure.rows[0].specified_height = Some(SpecifiedHeight::Percent(50.0));
        structure.cells = vec![CellInfo {
            index: 0,
            source_row: 0,
            source_col: 0,
            row: 0,
            col: 0,
            rowspan: 2,
            colspan: 1,
            box_index: 0,
            min_width: 0.0,
            max_width: 0.0,
            min_height: 160.0,
            bounds: Rect::ZERO,
        }];
        structure.border_spacing = (0.0, 0.0);

        calculate_row_heights(&mut structure, Some(200.0));

        assert!((structure.rows[0].computed_height - 100.0).abs() < 0.01);
        assert!((structure.rows[1].computed_height - 100.0).abs() < 0.01);
    }

    #[test]
    fn calculate_row_heights_distributes_rowspan_extra_proportionally() {
        let mut structure = TableStructure::new();
        structure.row_count = 2;
        structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
        structure.rows[0].min_height = 10.0;
        structure.rows[1].min_height = 30.0;
        structure.cells = vec![CellInfo {
            index: 0,
            source_row: 0,
            source_col: 0,
            row: 0,
            col: 0,
            rowspan: 2,
            colspan: 1,
            box_index: 0,
            min_width: 0.0,
            max_width: 0.0,
            min_height: 120.0,
            bounds: Rect::ZERO,
        }];
        structure.border_spacing = (0.0, 0.0);

        calculate_row_heights(&mut structure, None);

        // Extra 80 should distribute in 1:3 ratio from initial 10:30 heights → 20/60 split.
        assert!((structure.rows[0].computed_height - 30.0).abs() < 0.01);
        assert!((structure.rows[1].computed_height - 90.0).abs() < 0.01);
    }

    #[test]
    fn column_percentages_normalize_when_over_100() {
        let mut constraints = vec![
            ColumnConstraints::percentage(70.0, 0.0, 100.0),
            ColumnConstraints::percentage(50.0, 0.0, 100.0),
        ];
        let tfc = TableFormattingContext::new();
        // Percentages should remain authored even when over 100%.
        tfc.normalize_percentage_constraints(&mut constraints, Some(200.0), &AvailableSpace::Definite(200.0));

        let sum: f32 = constraints.iter().filter_map(|c| c.percentage).sum();
        assert!((sum - 120.0).abs() < 0.01);
    }

    #[test]
    fn spanning_max_width_pushes_columns() {
        let mut structure = TableStructure::new();
        structure.column_count = 2;
        structure.columns = vec![ColumnInfo::new(0), ColumnInfo::new(1)];
        structure.cells = vec![CellInfo {
            index: 0,
            source_row: 0,
            source_col: 0,
            row: 0,
            col: 0,
            rowspan: 1,
            colspan: 2,
            box_index: 0,
            min_width: 0.0,
            max_width: 200.0,
            min_height: 0.0,
            bounds: Rect::ZERO,
        }];

        calculate_auto_layout_widths(&mut structure, 300.0);

        let total_max = structure.columns.iter().map(|c| c.max_width).sum::<f32>();
        assert!(
            total_max >= 200.0 - 0.01,
            "spanning max width should raise spanned columns (got {total_max})"
        );
        assert!(structure.columns[0].max_width > 0.0 && structure.columns[1].max_width > 0.0);
    }

    #[test]
    fn extra_table_height_distributes_proportionally_to_auto_rows() {
        let mut structure = TableStructure::new();
        structure.row_count = 2;
        structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
        // Give rows different intrinsic heights so proportional distribution is observable.
        structure.rows[0].min_height = 10.0;
        structure.rows[1].min_height = 30.0;
        structure.border_spacing = (0.0, 0.0);

        calculate_row_heights(&mut structure, Some(100.0));

        // Sum to table height.
        let total = structure.rows[0].computed_height + structure.rows[1].computed_height;
        assert!((total - 100.0).abs() < 0.01);
        // Extra space (60px) apportioned in 1:3 ratio -> 10+15 vs 30+45.
        assert!((structure.rows[0].computed_height - 25.0).abs() < 0.5);
        assert!((structure.rows[1].computed_height - 75.0).abs() < 0.5);
    }

    #[test]
    fn extra_table_height_skips_fixed_and_percent_rows() {
        let mut structure = TableStructure::new();
        structure.row_count = 3;
        structure.rows = vec![RowInfo::new(0), RowInfo::new(1), RowInfo::new(2)];
        structure.rows[0].specified_height = Some(SpecifiedHeight::Fixed(20.0));
        structure.rows[1].specified_height = Some(SpecifiedHeight::Percent(50.0));
        structure.rows[2].min_height = 10.0;
        structure.border_spacing = (0.0, 0.0);

        calculate_row_heights(&mut structure, Some(200.0));

        // Fixed row stays at 20, percent row at 100; remaining 80 goes to the auto row.
        assert!((structure.rows[0].computed_height - 20.0).abs() < 0.01);
        assert!((structure.rows[1].computed_height - 100.0).abs() < 0.01);
        assert!((structure.rows[2].computed_height - 80.0).abs() < 0.5);
    }

    #[test]
    fn test_vertical_align_with_rowspan() {
        let mut structure = TableStructure::new();
        structure.row_count = 2;
        structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
        structure.cells = vec![
            CellInfo {
                index: 0,
                source_row: 0,
                source_col: 0,
                row: 0,
                col: 0,
                rowspan: 2,
                colspan: 1,
                box_index: 0,
                min_width: 50.0,
                max_width: 100.0,
                min_height: 40.0, // spanning cell
                bounds: Rect::ZERO,
            },
            CellInfo {
                index: 1,
                source_row: 0,
                source_col: 1,
                row: 0,
                col: 1,
                rowspan: 1,
                colspan: 1,
                box_index: 1,
                min_width: 50.0,
                max_width: 100.0,
                min_height: 20.0,
                bounds: Rect::ZERO,
            },
            CellInfo {
                index: 2,
                source_row: 1,
                source_col: 1,
                row: 1,
                col: 1,
                rowspan: 1,
                colspan: 1,
                box_index: 2,
                min_width: 50.0,
                max_width: 100.0,
                min_height: 20.0,
                bounds: Rect::ZERO,
            },
        ];
        structure.border_spacing = (0.0, 0.0);

        calculate_row_heights(&mut structure, None);

        // Both rows should have positive heights and the spanning cell shouldn't collapse row 1 to zero.
        assert!(structure.rows[0].computed_height > 0.0);
        assert!(structure.rows[1].computed_height > 0.0);
        assert!((structure.rows[0].computed_height - structure.rows[1].computed_height).abs() < 25.0);
    }

    #[test]
    fn test_vertical_align_baseline_rowspan_reserves_baseline_space() {
        let mut structure = TableStructure::new();
        structure.row_count = 2;
        structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
        // Baseline-aligned spanning cell; its first-row baseline should lift row 0's baseline metrics.
        structure.cells = vec![CellInfo {
            index: 0,
            source_row: 0,
            source_col: 0,
            row: 0,
            col: 0,
            rowspan: 2,
            colspan: 1,
            box_index: 0,
            min_width: 50.0,
            max_width: 100.0,
            min_height: 40.0,
            bounds: Rect::ZERO,
        }];
        structure.border_spacing = (0.0, 0.0);

        // Manually set row height expectations to simulate a baseline offset.
        calculate_row_heights(&mut structure, None);
        // After baseline reservation, row 0 should carry baseline metrics and a non-zero height.
        assert!(structure.rows[0].computed_height >= 1.0);
        assert!(structure.rows[0].min_height >= 0.0);
    }

    #[test]
    fn rowspan_height_is_shared_across_rows() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;

        let mut tall_child_style = ComputedStyle::default();
        tall_child_style.display = Display::Block;
        tall_child_style.height = Some(Length::px(40.0));

        let mut span_child_style = ComputedStyle::default();
        span_child_style.display = Display::Block;
        span_child_style.height = Some(Length::px(100.0));

        let tall_child = BoxNode::new_block(Arc::new(tall_child_style), FormattingContextType::Block, vec![]);
        let cell_a = BoxNode::new_block(
            Arc::new(cell_style.clone()),
            FormattingContextType::Block,
            vec![tall_child],
        );

        let span_child = BoxNode::new_block(Arc::new(span_child_style), FormattingContextType::Block, vec![]);
        let span_cell = BoxNode::new_block(
            Arc::new(cell_style.clone()),
            FormattingContextType::Block,
            vec![span_child],
        )
        .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(1, 2));

        let row0 = BoxNode::new_block(
            Arc::new(row_style.clone()),
            FormattingContextType::Block,
            vec![cell_a, span_cell],
        );

        let row1_cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row1 = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![row1_cell]);

        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row0, row1]);

        let tfc = TableFormattingContext::new();
        let fragment = tfc
            .layout(&table, &LayoutConstraints::definite_width(200.0))
            .expect("table layout");

        let mut tops = Vec::new();
        collect_table_cell_tops(&fragment, &mut tops);
        tops.sort_by(|a, b| a.partial_cmp(b).unwrap());

        let row0_y = tops.first().copied().unwrap_or(0.0);
        let row1_y = tops.last().copied().unwrap_or(0.0);
        assert!(row1_y > row0_y, "row starts should ascend: {:?}", tops);

        let row0_height = row1_y - row0_y;
        let total_height = fragment.bounds.height();
        let row1_height = total_height - row1_y;

        assert!(
            row0_height > 50.0 && row0_height < 120.0,
            "row 0 height should carry part of the spanning cell but not all of it: {row0_height}"
        );
        assert!(
            row1_height > 15.0,
            "row 1 should retain a meaningful share of the spanning cell: {row1_height}"
        );
    }

    #[test]
    fn test_collapsed_borders_contribute_to_table_bounds() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;
        table_style.border_top_style = BorderStyle::Solid;
        table_style.border_bottom_style = BorderStyle::Solid;
        table_style.border_left_style = BorderStyle::Solid;
        table_style.border_right_style = BorderStyle::Solid;
        table_style.border_top_width = Length::px(2.0);
        table_style.border_bottom_width = Length::px(2.0);
        table_style.border_left_width = Length::px(3.0);
        table_style.border_right_width = Length::px(3.0);
        table_style.border_top_color = Rgba::from_rgba8(0, 0, 0, 255);
        table_style.border_bottom_color = Rgba::from_rgba8(0, 0, 0, 255);
        table_style.border_left_color = Rgba::from_rgba8(0, 0, 0, 255);
        table_style.border_right_color = Rgba::from_rgba8(0, 0, 0, 255);

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let tfc = TableFormattingContext::new();
        let constraints = LayoutConstraints::definite_width(100.0);
        let fragment = tfc.layout(&table, &constraints).expect("table layout");

        // Width should include left+right collapsed borders (3 + 3) even with zero cell width.
        assert!(fragment.bounds.width() >= 6.0);
        // Height should include top+bottom collapsed borders (2 + 2) plus at least a minimal row height.
        assert!(fragment.bounds.height() >= 4.0);
    }

    #[test]
    fn test_collapsed_borders_affect_row_offsets() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;
        table_style.border_top_style = BorderStyle::Solid;
        table_style.border_bottom_style = BorderStyle::Solid;
        table_style.border_top_width = Length::px(2.0);
        table_style.border_bottom_width = Length::px(2.0);

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        let cell_a = BoxNode::new_block(Arc::new(cell_style.clone()), FormattingContextType::Block, vec![]);
        let cell_b = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row1 = BoxNode::new_block(Arc::new(row_style.clone()), FormattingContextType::Block, vec![cell_a]);
        let row2 = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell_b]);

        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row1, row2]);

        let tfc = TableFormattingContext::new();
        let constraints = LayoutConstraints::definite_width(10.0);
        let fragment = tfc.layout(&table, &constraints).expect("table layout");

        // Expect top border + two minimal rows + bottom border.
        assert!((fragment.bounds.height() - 6.0).abs() < 0.51);
    }

    #[test]
    fn test_collapsed_borders_offset_cell_position() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;
        table_style.border_top_style = BorderStyle::Solid;
        table_style.border_bottom_style = BorderStyle::Solid;
        table_style.border_left_style = BorderStyle::Solid;
        table_style.border_right_style = BorderStyle::Solid;
        table_style.border_top_width = Length::px(4.0);
        table_style.border_bottom_width = Length::px(4.0);
        table_style.border_left_width = Length::px(6.0);
        table_style.border_right_width = Length::px(6.0);
        table_style.border_top_color = Rgba::from_rgba8(0, 0, 0, 255);
        table_style.border_bottom_color = Rgba::from_rgba8(0, 0, 0, 255);
        table_style.border_left_color = Rgba::from_rgba8(0, 0, 0, 255);
        table_style.border_right_color = Rgba::from_rgba8(0, 0, 0, 255);
        table_style.width = Some(Length::px(100.0));

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let tfc = TableFormattingContext::new();
        let constraints = LayoutConstraints::definite_width(100.0);
        let fragment = tfc.layout(&table, &constraints).expect("table layout");

        let cell_frag = fragment
            .children
            .iter()
            .find(|f| {
                f.style
                    .as_ref()
                    .map(|s| {
                        s.border_top_width.to_px() == 0.0
                            && s.border_left_width.to_px() == 0.0
                            && s.border_right_width.to_px() == 0.0
                            && s.border_bottom_width.to_px() == 0.0
                    })
                    .unwrap_or(false)
            })
            .expect("cell fragment");

        // Cell should be offset by the collapsed borders.
        assert!((cell_frag.bounds.x() - 6.0).abs() < 0.51);
        assert!((cell_frag.bounds.y() - 4.0).abs() < 0.51);
        // Table width should include both borders plus the cell.
        assert!(fragment.bounds.width() >= cell_frag.bounds.width() + 12.0);
    }

    #[test]
    fn percentages_ignored_without_definite_table_width() {
        let mut constraints = vec![
            ColumnConstraints::percentage(50.0, 10.0, 100.0),
            ColumnConstraints::percentage(50.0, 20.0, 100.0),
        ];

        let tfc = TableFormattingContext::new();
        tfc.normalize_percentage_constraints(&mut constraints, None, &AvailableSpace::MaxContent);

        assert!(constraints.iter().all(|c| c.percentage.is_none()));
        assert!(constraints.iter().all(|c| c.is_flexible));
    }

    #[test]
    fn min_content_available_width_uses_column_min_widths() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_spacing_horizontal = Length::px(0.0);
        table_style.border_spacing_vertical = Length::px(0.0);

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        cell_style.width = Some(Length::px(20.0));

        let cell_a = BoxNode::new_block(Arc::new(cell_style.clone()), FormattingContextType::Block, vec![]);
        let cell_b = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
        let row = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![cell_a, cell_b]);
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row]);

        let tfc = TableFormattingContext::new();
        let constraints = LayoutConstraints::new(AvailableSpace::MinContent, AvailableSpace::Indefinite);
        let fragment = tfc.layout(&table, &constraints).expect("table layout");

        // Two cells with 20px widths -> table min-content width should be ~40px.
        assert!((fragment.bounds.width() - 40.0).abs() < 0.5);
    }

    #[test]
    fn collapsed_borders_with_rowspan_and_vertical_align() {
        let mut table_style = ComputedStyle::default();
        table_style.display = Display::Table;
        table_style.border_collapse = BorderCollapse::Collapse;
        table_style.border_top_style = BorderStyle::Solid;
        table_style.border_bottom_style = BorderStyle::Solid;
        table_style.border_left_style = BorderStyle::Solid;
        table_style.border_right_style = BorderStyle::Solid;
        table_style.border_top_width = Length::px(2.0);
        table_style.border_bottom_width = Length::px(2.0);
        table_style.border_left_width = Length::px(2.0);
        table_style.border_right_width = Length::px(2.0);
        table_style.border_top_color = Rgba::from_rgba8(0, 0, 0, 255);
        table_style.border_bottom_color = Rgba::from_rgba8(0, 0, 0, 255);
        table_style.border_left_color = Rgba::from_rgba8(0, 0, 0, 255);
        table_style.border_right_color = Rgba::from_rgba8(0, 0, 0, 255);

        let mut row_style = ComputedStyle::default();
        row_style.display = Display::TableRow;

        let mut cell_top_style = ComputedStyle::default();
        cell_top_style.display = Display::TableCell;
        cell_top_style.vertical_align = VerticalAlign::Top;
        cell_top_style.min_height = Some(Length::px(10.0));

        let mut cell_span_style = ComputedStyle::default();
        cell_span_style.display = Display::TableCell;
        cell_span_style.vertical_align = VerticalAlign::Baseline;

        let mut cell_bottom_style = ComputedStyle::default();
        cell_bottom_style.display = Display::TableCell;
        cell_bottom_style.vertical_align = VerticalAlign::Bottom;
        cell_bottom_style.min_height = Some(Length::px(10.0));

        let top_cell = BoxNode::new_block(Arc::new(cell_top_style), FormattingContextType::Block, vec![]);
        let span_cell = BoxNode::new_block(Arc::new(cell_span_style), FormattingContextType::Block, vec![]);
        let bottom_cell = BoxNode::new_block(Arc::new(cell_bottom_style), FormattingContextType::Block, vec![]);

        let row1 = BoxNode::new_block(
            Arc::new(row_style.clone()),
            FormattingContextType::Block,
            vec![span_cell.clone(), top_cell],
        );
        let row2 = BoxNode::new_block(Arc::new(row_style), FormattingContextType::Block, vec![bottom_cell]);

        // Attach debug info to give row/col placement: row1 has two cells, span_cell should be col 0, top_cell col1.
        let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, vec![row1, row2]);

        let tfc = TableFormattingContext::new();
        let constraints = LayoutConstraints::definite_width(50.0);
        let fragment = tfc.layout(&table, &constraints).expect("table layout");

        // Find child fragments; first child is collapsed border, cell fragments follow.
        let mut cells: Vec<&FragmentNode> = fragment
            .children
            .iter()
            .filter(|f| matches!(f.content, FragmentContent::Block { .. }))
            .collect();
        cells.retain(|f| f.style.as_ref().map(|s| s.display == Display::Block).unwrap_or(true));

        // Verify vertical offsets respect collapsed borders (should start at or below 0 due to stroke centering).
        let min_y = cells.iter().map(|c| c.bounds.y()).fold(f32::INFINITY, f32::min);
        assert!(min_y <= 0.0 + 0.5);
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
