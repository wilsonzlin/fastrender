//! Table Column Width Distribution Algorithm
//!
//! Implements CSS table column width distribution as specified in:
//! - CSS Tables Module Level 3, Section 4 (Width computation)
//! - CSS 2.1 Section 17.5.2.2 (Automatic table layout algorithm)
//!
//! # Overview
//!
//! Column width distribution is a constraint-solving problem where available
//! table width must be distributed across columns while respecting:
//! - Minimum content widths (columns can't be narrower than content)
//! - Maximum content widths (columns prefer to fit content without wrapping)
//! - Fixed widths (explicit CSS `width` on cells or `<col>` elements)
//! - Percentage widths (relative to table width)
//!
//! # Algorithm
//!
//! The algorithm proceeds in phases:
//! 1. Apply fixed widths directly
//! 2. Resolve percentage widths based on available space
//! 3. Compute remaining space after fixed and percentage columns
//! 4. Distribute remaining space proportionally to flexible columns
//!
//! # References
//!
//! - CSS Tables Module Level 3: <https://www.w3.org/TR/css-tables-3/>
//! - CSS 2.1 Section 17.5: <https://www.w3.org/TR/CSS21/tables.html#width-layout>

use crate::error::{RenderError, RenderStage};
use crate::layout::formatting_context::LayoutError;
use crate::render_control::check_active_periodic;
use std::cell::{Cell, RefCell};
use std::fmt;
use std::time::Duration;

/// Constraints for a single table column
///
/// Each column has various width constraints that affect the distribution
/// algorithm. These constraints come from cell content analysis, explicit
/// CSS widths, and percentage specifications.
///
/// # Examples
///
/// ```ignore
/// use fastrender::layout::contexts::table::ColumnConstraints;
///
/// // A column with content-based sizing
/// let auto_column = ColumnConstraints::new(50.0, 150.0);
/// assert_eq!(auto_column.min_width, 50.0);
/// assert_eq!(auto_column.max_width, 150.0);
///
/// // A fixed-width column
/// let fixed_column = ColumnConstraints::fixed(100.0);
/// assert_eq!(fixed_column.fixed_width, Some(100.0));
///
/// // A percentage column
/// let percentage_column = ColumnConstraints::percentage(25.0, 30.0, 200.0);
/// assert_eq!(percentage_column.percentage, Some(25.0));
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct ColumnConstraints {
  /// Minimum content width (narrowest the column can be without overflow)
  ///
  /// This is the width of the widest unbreakable content (longest word,
  /// image, etc.) in any cell in this column.
  pub min_width: f32,

  /// Maximum content width (widest the column wants to be)
  ///
  /// This is the width at which all content in the column would fit
  /// without wrapping.
  pub max_width: f32,

  /// Whether `max_width` is an authored cap (from max-width on the cell/column)
  ///
  /// Content-based max widths should not cap specified percentage/fixed widths, but
  /// authored max-width values should.
  pub has_max_cap: bool,

  /// Fixed width specified by CSS `width` property
  ///
  /// If set, this column should be exactly this width (subject to min_width).
  /// Takes precedence over percentage and auto sizing.
  pub fixed_width: Option<f32>,

  /// Percentage width relative to table width (can exceed 100%)
  ///
  /// If set, this column's width is a percentage of the table's width.
  /// Values may exceed 100% and are treated as over-constrained requests.
  pub percentage: Option<f32>,

  /// Whether this column can be resized during distribution
  ///
  /// Fixed and percentage columns are generally not flexible.
  /// Auto columns are flexible and receive proportional distribution.
  pub is_flexible: bool,
}

impl ColumnConstraints {
  /// Creates new column constraints with min/max content widths
  ///
  /// Creates a flexible auto-sizing column that can receive extra space.
  ///
  /// # Arguments
  ///
  /// * `min_width` - Minimum content width
  /// * `max_width` - Maximum content width
  ///
  /// # Examples
  ///
  /// ```ignore
  /// use fastrender::layout::contexts::table::ColumnConstraints;
  ///
  /// let column = ColumnConstraints::new(50.0, 200.0);
  /// assert!(column.is_flexible);
  /// assert_eq!(column.min_width, 50.0);
  /// ```
  pub fn new(min_width: f32, max_width: f32) -> Self {
    Self {
      min_width,
      max_width: max_width.max(min_width), // max must be >= min
      has_max_cap: false,
      fixed_width: None,
      percentage: None,
      is_flexible: true,
    }
  }

  /// Creates a fixed-width column
  ///
  /// Fixed columns are not flexible and have explicit width.
  ///
  /// # Arguments
  ///
  /// * `width` - The fixed width for this column
  ///
  /// # Examples
  ///
  /// ```ignore
  /// use fastrender::layout::contexts::table::ColumnConstraints;
  ///
  /// let column = ColumnConstraints::fixed(150.0);
  /// assert_eq!(column.fixed_width, Some(150.0));
  /// assert!(!column.is_flexible);
  /// ```
  pub fn fixed(width: f32) -> Self {
    Self {
      min_width: width,
      max_width: width,
      has_max_cap: false,
      fixed_width: Some(width),
      percentage: None,
      is_flexible: false,
    }
  }

  /// Creates a percentage-width column
  ///
  /// Percentage columns have their width computed relative to table width.
  ///
  /// # Arguments
  ///
  /// * `percentage` - Percentage value (may exceed 100; negative clamped to 0)
  /// * `min_width` - Minimum content width (fallback)
  /// * `max_width` - Maximum content width
  ///
  /// # Examples
  ///
  /// ```ignore
  /// use fastrender::layout::contexts::table::ColumnConstraints;
  ///
  /// let column = ColumnConstraints::percentage(25.0, 50.0, 200.0);
  /// assert_eq!(column.percentage, Some(25.0));
  /// assert!(!column.is_flexible);
  /// ```
  pub fn percentage(percentage: f32, min_width: f32, max_width: f32) -> Self {
    Self {
      min_width,
      max_width: max_width.max(min_width),
      has_max_cap: false,
      fixed_width: None,
      percentage: Some(percentage.max(0.0)),
      is_flexible: false,
    }
  }

  /// Marks this column as percentage-based using the current min/max widths.
  pub fn set_percentage(&mut self, percentage: f32) {
    self.percentage = Some(percentage.max(0.0));
    self.fixed_width = None;
    self.is_flexible = false;
  }

  /// Creates a zero-sized column (placeholder)
  ///
  /// Used for empty columns or columns with no content.
  ///
  /// # Examples
  ///
  /// ```ignore
  /// use fastrender::layout::contexts::table::ColumnConstraints;
  ///
  /// let column = ColumnConstraints::zero();
  /// assert_eq!(column.min_width, 0.0);
  /// assert_eq!(column.max_width, 0.0);
  /// ```
  pub fn zero() -> Self {
    Self::new(0.0, 0.0)
  }

  /// Returns the flexibility range (max - min)
  ///
  /// This represents how much the column can grow from min to max.
  /// Used in proportional distribution.
  ///
  /// # Examples
  ///
  /// ```ignore
  /// use fastrender::layout::contexts::table::ColumnConstraints;
  ///
  /// let column = ColumnConstraints::new(50.0, 150.0);
  /// assert_eq!(column.flexibility_range(), 100.0);
  /// ```
  pub fn flexibility_range(&self) -> f32 {
    (self.max_width - self.min_width).max(0.0)
  }

  /// Updates constraints based on a cell with colspan
  ///
  /// When a cell spans multiple columns, its width requirements need
  /// to be distributed among the spanned columns.
  ///
  /// # Arguments
  ///
  /// * `cell_min` - Cell's minimum width
  /// * `cell_max` - Cell's maximum width
  /// * `colspan` - Number of columns spanned
  /// * `column_index` - Which column within the span (0-based)
  ///
  /// # Examples
  ///
  /// ```ignore
  /// use fastrender::layout::contexts::table::ColumnConstraints;
  ///
  /// let mut column = ColumnConstraints::new(50.0, 100.0);
  /// column.add_colspan_contribution(200.0, 400.0, 2, 0);
  /// // Column receives half of the spanning cell's width requirement
  /// assert!(column.min_width >= 50.0);
  /// ```
  pub fn add_colspan_contribution(
    &mut self,
    cell_min: f32,
    cell_max: f32,
    colspan: usize,
    _column_index: usize,
  ) {
    if colspan == 0 {
      return;
    }

    // Simple equal distribution among spanned columns
    let min_per_column = cell_min / colspan as f32;
    let max_per_column = cell_max / colspan as f32;

    self.min_width = self.min_width.max(min_per_column);
    self.max_width = self.max_width.max(max_per_column);
  }
}

impl Default for ColumnConstraints {
  fn default() -> Self {
    Self::new(0.0, f32::MAX)
  }
}

impl fmt::Display for ColumnConstraints {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(fixed) = self.fixed_width {
      write!(f, "Fixed({}px)", fixed)
    } else if let Some(pct) = self.percentage {
      write!(f, "{}%", pct)
    } else {
      write!(f, "Auto({}-{}px)", self.min_width, self.max_width)
    }
  }
}

/// Distribution mode for column width algorithm
///
/// Different table layout modes use different distribution strategies.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DistributionMode {
  /// Fixed table layout (`table-layout: fixed`)
  ///
  /// Column widths are determined only by the first row and <col> elements.
  /// Much simpler and faster than auto layout.
  Fixed,

  /// Auto table layout (`table-layout: auto`)
  ///
  /// Column widths are determined by analyzing all cell content.
  /// More complex but produces better results for variable content.
  Auto,
}

impl Default for DistributionMode {
  fn default() -> Self {
    Self::Auto
  }
}

/// Result of column width distribution
///
/// Contains the computed widths for all columns and metadata about
/// the distribution process.
#[derive(Debug, Clone, PartialEq)]
pub struct ColumnWidthDistributionResult {
  /// Computed width for each column
  pub widths: Vec<f32>,

  /// Total width of all columns
  pub total_width: f32,

  /// Whether the table is over-constrained (required > available)
  pub is_over_constrained: bool,

  /// Amount of width that couldn't be accommodated (if over-constrained)
  pub overflow_amount: f32,
}

impl ColumnWidthDistributionResult {
  /// Creates a new distribution result
  pub fn new(widths: Vec<f32>) -> Self {
    let total_width = widths.iter().sum();
    Self {
      widths,
      total_width,
      is_over_constrained: false,
      overflow_amount: 0.0,
    }
  }

  /// Returns the width of a specific column
  ///
  /// Returns 0.0 if the column index is out of bounds.
  pub fn column_width(&self, index: usize) -> f32 {
    self.widths.get(index).copied().unwrap_or(0.0)
  }

  /// Returns the number of columns
  pub fn column_count(&self) -> usize {
    self.widths.len()
  }
}

#[derive(Default)]
struct FlexScratch {
  infinite_indices: Vec<usize>,
}

#[derive(Default)]
struct SpanScratch {
  indices: Vec<usize>,
}

thread_local! {
  static FLEX_SCRATCH: RefCell<FlexScratch> = RefCell::new(FlexScratch::default());
  static SPAN_SCRATCH: RefCell<SpanScratch> = RefCell::new(SpanScratch::default());
}

/// Column width distribution algorithm
///
/// The main entry point for distributing table width across columns.
/// Implements both fixed and auto layout algorithms.
///
/// # Examples
///
/// ```ignore
/// use fastrender::layout::contexts::table::{
///     ColumnConstraints, ColumnDistributor, DistributionMode,
/// };
///
/// let constraints = vec![
///     ColumnConstraints::new(50.0, 150.0),
///     ColumnConstraints::new(100.0, 200.0),
///     ColumnConstraints::new(75.0, 175.0),
/// ];
///
/// let distributor = ColumnDistributor::new(DistributionMode::Auto);
/// let result = distributor.distribute(&constraints, 500.0);
///
/// assert_eq!(result.column_count(), 3);
/// assert!((result.total_width - 500.0).abs() < 0.01);
/// ```
#[derive(Debug, Clone)]
pub struct ColumnDistributor {
  /// Distribution mode (fixed or auto)
  mode: DistributionMode,

  /// Minimum column width (for empty columns)
  min_column_width: f32,

  /// Recorded layout timeout, if any checks exceeded the render deadline.
  timeout_elapsed: Cell<Option<Duration>>,
}

impl ColumnDistributor {
  /// Creates a new column distributor with the specified mode
  ///
  /// # Arguments
  ///
  /// * `mode` - Distribution mode (Fixed or Auto)
  ///
  /// # Examples
  ///
  /// ```ignore
  /// use fastrender::layout::contexts::table::{ColumnDistributor, DistributionMode};
  ///
  /// let distributor = ColumnDistributor::new(DistributionMode::Auto);
  /// ```
  pub fn new(mode: DistributionMode) -> Self {
    Self {
      mode,
      min_column_width: 0.0,
      timeout_elapsed: Cell::new(None),
    }
  }

  /// Sets the minimum width for any column
  ///
  /// Even empty columns will be at least this wide.
  ///
  /// # Examples
  ///
  /// ```ignore
  /// use fastrender::layout::contexts::table::{ColumnDistributor, DistributionMode};
  ///
  /// let distributor = ColumnDistributor::new(DistributionMode::Auto)
  ///     .with_min_column_width(10.0);
  /// ```
  pub fn with_min_column_width(mut self, width: f32) -> Self {
    self.min_column_width = width;
    self
  }

  fn record_timeout(&self, elapsed: Duration) {
    let existing = self.timeout_elapsed.get();
    if existing.map_or(true, |current| elapsed < current) {
      self.timeout_elapsed.set(Some(elapsed));
    }
  }

  pub fn take_timeout_error(&self) -> Option<LayoutError> {
    self
      .timeout_elapsed
      .take()
      .map(|elapsed| LayoutError::Timeout { elapsed })
  }

  fn check_deadline(&self, counter: &mut usize) -> bool {
    if self.timeout_elapsed.get().is_some() {
      return true;
    }
    if let Err(RenderError::Timeout { elapsed, .. }) =
      check_active_periodic(counter, 64, RenderStage::Layout)
    {
      self.record_timeout(elapsed);
      return true;
    }
    false
  }

  /// Distributes available width across columns
  ///
  /// This is the main entry point for the distribution algorithm.
  /// It selects the appropriate algorithm based on the mode and
  /// returns computed widths for all columns.
  ///
  /// # Arguments
  ///
  /// * `columns` - Constraints for each column
  /// * `available_width` - Total width available for the table
  ///
  /// # Returns
  ///
  /// A `ColumnWidthDistributionResult` containing the computed widths.
  ///
  /// # Examples
  ///
  /// ```ignore
  /// use fastrender::layout::contexts::table::{
  ///     ColumnConstraints, ColumnDistributor, DistributionMode,
  /// };
  ///
  /// let columns = vec![
  ///     ColumnConstraints::new(100.0, 200.0),
  ///     ColumnConstraints::new(100.0, 200.0),
  /// ];
  ///
  /// let distributor = ColumnDistributor::new(DistributionMode::Auto);
  /// let result = distributor.distribute(&columns, 400.0);
  ///
  /// assert_eq!(result.widths.len(), 2);
  /// ```
  pub fn distribute(
    &self,
    columns: &[ColumnConstraints],
    available_width: f32,
  ) -> ColumnWidthDistributionResult {
    if columns.is_empty() {
      return ColumnWidthDistributionResult::new(vec![]);
    }

    match self.mode {
      DistributionMode::Fixed => self.distribute_fixed(columns, available_width),
      DistributionMode::Auto => self.distribute_auto(columns, available_width),
    }
  }

  /// Fixed layout distribution
  ///
  /// In fixed layout, column widths are computed as follows:
  /// 1. Fixed widths are applied directly
  /// 2. Remaining space is divided equally among other columns
  fn distribute_fixed(
    &self,
    columns: &[ColumnConstraints],
    available_width: f32,
  ) -> ColumnWidthDistributionResult {
    let mut deadline_counter = 0usize;
    let mut widths = vec![0.0; columns.len()];
    let mut remaining_width = available_width;
    let mut flexible_indices = Vec::new();
    let mut percent_indices = Vec::new();

    // Phase 1: Apply fixed widths
    for (i, col) in columns.iter().enumerate() {
      if self.check_deadline(&mut deadline_counter) {
        return ColumnWidthDistributionResult::new(vec![]);
      }
      if let Some(fixed) = col.fixed_width {
        let mut width = fixed.max(col.min_width).max(self.min_column_width);
        if col.has_max_cap && col.max_width.is_finite() {
          width = width.min(col.max_width.max(col.min_width));
        }
        widths[i] = width;
        remaining_width -= width;
      } else if let Some(pct) = col.percentage {
        percent_indices.push((i, pct));
      } else {
        flexible_indices.push(i);
      }
    }

    // Phase 2: Allocate percentage columns. Percentages are relative to the table width and
    // can overrun the available width; the table expands when columns over-commit.
    for (idx, pct) in percent_indices {
      if self.check_deadline(&mut deadline_counter) {
        return ColumnWidthDistributionResult::new(vec![]);
      }
      let col = &columns[idx];
      let raw = (pct / 100.0) * available_width;
      let mut width = raw.max(col.min_width).max(self.min_column_width);
      if col.has_max_cap && col.max_width.is_finite() {
        width = width.min(col.max_width.max(col.min_width));
      }
      widths[idx] = width;
      remaining_width -= width;
    }

    // Phase 3: Distribute remaining to flexible columns (auto). If no space remains, auto
    // columns still honor their minimum widths.
    if !flexible_indices.is_empty() {
      if remaining_width > 0.0 {
        let per_column = remaining_width / flexible_indices.len() as f32;
        for (i, col) in columns.iter().enumerate() {
          if self.check_deadline(&mut deadline_counter) {
            return ColumnWidthDistributionResult::new(vec![]);
          }
          if col.fixed_width.is_none() && col.percentage.is_none() {
            widths[i] = per_column
              .max(col.min_width)
              .max(self.min_column_width)
              .min(col.max_width.max(col.min_width));
          }
        }
      } else {
        for &idx in &flexible_indices {
          if self.check_deadline(&mut deadline_counter) {
            return ColumnWidthDistributionResult::new(vec![]);
          }
          let col = &columns[idx];
          widths[idx] = col
            .min_width
            .max(self.min_column_width)
            .min(col.max_width.max(col.min_width));
        }
      }
    }

    // Handle over-constraint
    let total: f32 = widths.iter().sum();
    let is_over_constrained = total > available_width;
    let overflow_amount = if is_over_constrained {
      total - available_width
    } else {
      0.0
    };

    ColumnWidthDistributionResult {
      widths,
      total_width: total,
      is_over_constrained,
      overflow_amount,
    }
  }

  /// Auto layout distribution
  ///
  /// In auto layout, column widths are computed using a sophisticated
  /// algorithm that considers min/max content widths.
  ///
  /// The algorithm:
  /// 1. Apply fixed widths
  /// 2. Resolve percentage widths
  /// 3. Compute remaining space
  /// 4. Distribute proportionally to flexible columns
  fn distribute_auto(
    &self,
    columns: &[ColumnConstraints],
    available_width: f32,
  ) -> ColumnWidthDistributionResult {
    let mut deadline_counter = 0usize;
    if self.check_deadline(&mut deadline_counter) {
      return ColumnWidthDistributionResult::new(vec![]);
    }
    // Step 1: Compute sum of min and max widths
    let total_min: f32 = columns
      .iter()
      .map(|c| c.min_width.max(self.min_column_width))
      .sum();
    let total_max: f32 = columns
      .iter()
      .map(|c| c.max_width.max(self.min_column_width))
      .sum();
    let has_percentage = columns.iter().any(|c| c.percentage.is_some());

    // Guard against non-finite available width: fall back to the tightest finite bound we have.
    let mut available_width = available_width;
    if !available_width.is_finite() {
      if total_max.is_finite() {
        available_width = total_max;
      } else if total_min.is_finite() {
        available_width = total_min;
      } else {
        available_width = 0.0;
      }
    }

    // Step 2: Handle edge cases
    if available_width <= 0.0 {
      // No space available, use minimums
      return self.distribute_at_minimum(columns);
    }

    if available_width >= total_max && !has_percentage {
      // Plenty of space, give each column its max
      return self.distribute_at_maximum(columns);
    }

    if available_width <= total_min {
      // Not enough space, scale down proportionally
      return self.distribute_under_minimum(columns, available_width);
    }

    // Step 3: Normal case - between min and max
    self.distribute_proportionally(columns, available_width, total_min, total_max)
  }

  /// Distribute at minimum widths
  fn distribute_at_minimum(&self, columns: &[ColumnConstraints]) -> ColumnWidthDistributionResult {
    let mut widths = Vec::with_capacity(columns.len());
    let mut deadline_counter = 0usize;
    for col in columns {
      if self.check_deadline(&mut deadline_counter) {
        return ColumnWidthDistributionResult::new(Vec::new());
      }
      widths.push(col.min_width.max(self.min_column_width));
    }
    ColumnWidthDistributionResult::new(widths)
  }

  /// Distribute at maximum widths
  fn distribute_at_maximum(&self, columns: &[ColumnConstraints]) -> ColumnWidthDistributionResult {
    let mut widths = Vec::with_capacity(columns.len());
    let mut deadline_counter = 0usize;
    for col in columns {
      if self.check_deadline(&mut deadline_counter) {
        return ColumnWidthDistributionResult::new(Vec::new());
      }
      widths.push(col.max_width.max(self.min_column_width));
    }
    ColumnWidthDistributionResult::new(widths)
  }

  /// Distribute when available is less than total minimum
  fn distribute_under_minimum(
    &self,
    columns: &[ColumnConstraints],
    available_width: f32,
  ) -> ColumnWidthDistributionResult {
    let mut widths = Vec::with_capacity(columns.len());
    let mut deadline_counter = 0usize;
    for col in columns {
      if self.check_deadline(&mut deadline_counter) {
        return ColumnWidthDistributionResult::new(Vec::new());
      }
      widths.push(col.min_width.max(self.min_column_width));
    }
    let total_min: f32 = widths.iter().sum();

    ColumnWidthDistributionResult {
      widths,
      total_width: total_min,
      is_over_constrained: true,
      overflow_amount: (total_min - available_width).max(0.0),
    }
  }

  /// Distribute proportionally between min and max
  fn distribute_proportionally(
    &self,
    columns: &[ColumnConstraints],
    available_width: f32,
    _total_min: f32,
    _total_max: f32,
  ) -> ColumnWidthDistributionResult {
    let mut deadline_counter = 0usize;
    // First pass: Apply fixed and percentage widths
    let mut widths = vec![0.0; columns.len()];
    let mut remaining_width = available_width;
    let mut flexible_indices = Vec::new();
    let mut percent_indices = Vec::new();

    for (i, col) in columns.iter().enumerate() {
      if self.check_deadline(&mut deadline_counter) {
        return ColumnWidthDistributionResult::new(vec![]);
      }
      if let Some(fixed) = col.fixed_width {
        let width = fixed.max(col.min_width).max(self.min_column_width);
        widths[i] = width;
        remaining_width -= width;
      } else if let Some(pct) = col.percentage {
        percent_indices.push((i, pct));
      } else {
        flexible_indices.push(i);
      }
    }

    // Apply percentages relative to the table's available width. Percent columns can overrun the
    // remaining budget; over-constraint is reported to the caller instead of scaling them down.
    if !percent_indices.is_empty() {
      for (idx, pct) in percent_indices {
        if self.check_deadline(&mut deadline_counter) {
          return ColumnWidthDistributionResult::new(vec![]);
        }
        let col = &columns[idx];
        let raw = (pct / 100.0) * available_width;
        let min_bound = col.min_width.max(self.min_column_width);
        let unclamped = raw.max(min_bound);
        let cap = if col.has_max_cap && col.max_width.is_finite() {
          Some(col.max_width.max(min_bound))
        } else {
          None
        };
        let width = cap.map_or(unclamped, |cap| unclamped.min(cap));
        widths[idx] = width;
        remaining_width -= width;
      }
    }

    // Second pass: Distribute to flexible columns
    if !flexible_indices.is_empty() {
      self.distribute_to_flexible(
        columns,
        &mut widths,
        &flexible_indices,
        remaining_width,
        &mut deadline_counter,
      );
      if self.timeout_elapsed.get().is_some() {
        return ColumnWidthDistributionResult::new(vec![]);
      }
    }

    let total: f32 = widths.iter().sum();
    let is_over_constrained = total > available_width + 0.01; // Small tolerance

    ColumnWidthDistributionResult {
      widths,
      total_width: total,
      is_over_constrained,
      overflow_amount: if is_over_constrained {
        total - available_width
      } else {
        0.0
      },
    }
  }

  /// Distribute remaining width to flexible columns
  fn distribute_to_flexible(
    &self,
    columns: &[ColumnConstraints],
    widths: &mut [f32],
    flexible_indices: &[usize],
    remaining_width: f32,
    deadline_counter: &mut usize,
  ) {
    if self.check_deadline(deadline_counter) {
      return;
    }
    if flexible_indices.is_empty() || remaining_width <= 0.0 {
      // Assign minimums to flexible columns
      for &i in flexible_indices {
        if self.check_deadline(deadline_counter) {
          return;
        }
        widths[i] = columns[i].min_width.max(self.min_column_width);
      }
      return;
    }

    // Compute flexibility for proportional distribution
    let mut flexible_min = 0.0;
    let mut flexible_max = 0.0;
    for &i in flexible_indices {
      if self.check_deadline(deadline_counter) {
        return;
      }
      flexible_min += columns[i].min_width.max(self.min_column_width);
      flexible_max += columns[i].max_width.max(self.min_column_width);
    }

    if remaining_width >= flexible_max {
      // Enough space for all flexible columns at max
      for &i in flexible_indices {
        if self.check_deadline(deadline_counter) {
          return;
        }
        widths[i] = columns[i].max_width.max(self.min_column_width);
      }
      return;
    }

    if remaining_width <= flexible_min {
      // Not enough for minimums - keep mins and let caller mark over-constraint.
      for &i in flexible_indices {
        if self.check_deadline(deadline_counter) {
          return;
        }
        widths[i] = columns[i].min_width.max(self.min_column_width);
      }
      return;
    }

    // Between min and max - distribute proportionally to flexibility range, with explicit
    // handling for unbounded ranges so we don't end up dividing by infinity and producing NaN.
    let excess = remaining_width - flexible_min;
    FLEX_SCRATCH.with(|scratch_cell| {
      let mut scratch = scratch_cell.borrow_mut();
      scratch.infinite_indices.clear();
      let mut finite_flex_total = 0.0;
      for &i in flexible_indices {
        if self.check_deadline(deadline_counter) {
          return;
        }
        let range = columns[i].flexibility_range();
        if range.is_finite() {
          finite_flex_total += range;
        } else {
          scratch.infinite_indices.push(i);
        }
        widths[i] = columns[i].min_width.max(self.min_column_width);
      }

      let mut remaining_excess = excess;

      // First, allocate to columns with finite headroom proportionally to their range, capped at
      // their max. If the excess exceeds the total finite headroom, leave the remainder for
      // unbounded columns.
      if finite_flex_total > 0.0 {
        for &i in flexible_indices {
          if self.check_deadline(deadline_counter) {
            return;
          }
          let range = columns[i].flexibility_range();
          if !range.is_finite() {
            continue;
          }
          let share = excess * (range / finite_flex_total);
          let clamped = share.min(range);
          widths[i] =
            (columns[i].min_width.max(self.min_column_width) + clamped).min(columns[i].max_width);
          remaining_excess -= clamped;
        }
        remaining_excess = remaining_excess.max(0.0);
      }

      // Any leftover space goes to unbounded columns; divide evenly to keep the result stable.
      if remaining_excess > 0.0 {
        if !scratch.infinite_indices.is_empty() {
          let per = remaining_excess / scratch.infinite_indices.len() as f32;
          for idx in scratch.infinite_indices.drain(..) {
            if self.check_deadline(deadline_counter) {
              return;
            }
            widths[idx] += per;
          }
        } else if finite_flex_total == 0.0 {
          // No range information at all (min == max for every column); split evenly.
          let per_column = remaining_width / flexible_indices.len() as f32;
          for &i in flexible_indices {
            if self.check_deadline(deadline_counter) {
              return;
            }
            widths[i] = per_column;
          }
        }
      }
    });
  }
}

impl Default for ColumnDistributor {
  fn default() -> Self {
    Self::new(DistributionMode::Auto)
  }
}

/// Distribute extra width from spanning cells among columns
///
/// When a cell spans multiple columns, its width requirements need to
/// be distributed among the spanned columns. This function updates
/// column constraints to account for spanning cell requirements.
///
/// # Arguments
///
/// * `columns` - Mutable column constraints
/// * `start_col` - First column in the span
/// * `end_col` - One past the last column in the span
/// * `cell_min` - Spanning cell's minimum width
/// * `cell_max` - Spanning cell's maximum width
///
/// # Algorithm
///
/// 1. Compute current sum of spanned columns' widths
/// 2. If spanning cell requires more, distribute the extra
/// 3. Extra is distributed proportionally to column flexibility
///
/// # Examples
///
/// ```ignore
/// use fastrender::layout::contexts::table::{
///     ColumnConstraints, distribute_spanning_cell_width,
/// };
///
/// let mut columns = vec![
///     ColumnConstraints::new(50.0, 100.0),
///     ColumnConstraints::new(50.0, 100.0),
/// ];
///
/// // A spanning cell requires 250px minimum
/// distribute_spanning_cell_width(&mut columns, 0, 2, 250.0, 300.0);
///
/// // Columns are updated to accommodate
/// assert!(columns[0].min_width + columns[1].min_width >= 250.0);
/// ```
pub fn distribute_spanning_cell_width(
  columns: &mut [ColumnConstraints],
  start_col: usize,
  end_col: usize,
  cell_min: f32,
  cell_max: f32,
) {
  if start_col >= end_col || end_col > columns.len() {
    return;
  }

  // Ignore non-finite requirements; they cannot be satisfied meaningfully and would otherwise
  // contaminate the computation with NaNs.
  let required_min = if cell_min.is_finite() {
    cell_min.max(0.0)
  } else {
    0.0
  };
  let required_max = cell_max.is_finite().then_some(cell_max);

  let spanned = &mut columns[start_col..end_col];

  SPAN_SCRATCH.with(|scratch_cell| {
    let mut scratch = scratch_cell.borrow_mut();
    let indices = &mut scratch.indices;

    let distribute_min = |cols: &mut [ColumnConstraints], indices: &[usize], need: f32| -> f32 {
      if indices.is_empty() || need <= 0.0 {
        return need;
      }
      let mut remaining = need;

      let mut total_headroom = 0.0;
      let mut infinite = 0usize;
      for &i in indices {
        let headroom = (cols[i].max_width - cols[i].min_width).max(0.0);
        if headroom.is_finite() {
          if headroom > 0.0 {
            total_headroom += headroom;
          }
        } else {
          infinite += 1;
        }
      }

      if total_headroom > 0.0 && remaining.is_finite() {
        let mut distributed = 0.0;
        for &i in indices {
          let headroom = (cols[i].max_width - cols[i].min_width).max(0.0);
          if headroom.is_finite() && headroom > 0.0 {
            let share = remaining * (headroom / total_headroom);
            let delta = share.min(headroom);
            cols[i].min_width += delta;
            if cols[i].max_width < cols[i].min_width {
              cols[i].max_width = cols[i].min_width;
            }
            distributed += delta;
          }
        }
        remaining = (remaining - distributed).max(0.0);
      }

      if remaining > 0.0 {
        if infinite > 0 {
          let per = remaining / infinite as f32;
          for &i in indices {
            let headroom = (cols[i].max_width - cols[i].min_width).max(0.0);
            if !headroom.is_finite() {
              cols[i].min_width += per;
              if cols[i].max_width < cols[i].min_width {
                cols[i].max_width = cols[i].min_width;
              }
            }
          }
          remaining = 0.0;
        } else {
          let mut total_weight = 0.0;
          for &i in indices {
            total_weight += cols[i].min_width.max(1.0);
          }
          if total_weight > 0.0 {
            let mut distributed = 0.0;
            for &i in indices {
              let weight = cols[i].min_width.max(1.0);
              let delta = remaining * (weight / total_weight);
              cols[i].min_width += delta;
              if cols[i].max_width < cols[i].min_width {
                cols[i].max_width = cols[i].min_width;
              }
              distributed += delta;
            }
            remaining = (remaining - distributed).max(0.0);
          }
        }
      }

      remaining
    };

    let current_min_sum: f32 = spanned.iter().map(|c| c.min_width).sum();
    if required_min > current_min_sum {
      let mut need = required_min - current_min_sum;
      indices.clear();
      indices.extend(spanned.iter().enumerate().filter_map(|(i, c)| {
        if c.is_flexible {
          Some(i)
        } else {
          None
        }
      }));
      need = distribute_min(spanned, indices, need);

      if need > 0.0 {
        indices.clear();
        indices.extend(spanned.iter().enumerate().filter_map(|(i, c)| {
          if c.percentage.is_some() {
            Some(i)
          } else {
            None
          }
        }));
        need = distribute_min(spanned, indices, need);
      }

      if need > 0.0 {
        indices.clear();
        indices.extend(0..spanned.len());
        need = distribute_min(spanned, indices, need);
      }

      if need > 0.0 {
        let per = need / spanned.len() as f32;
        for col in spanned.iter_mut() {
          col.min_width += per;
          if col.max_width < col.min_width {
            col.max_width = col.min_width;
          }
        }
      }
    }

    // Then ensure the span can satisfy the cell's maximum width request, preferring flexible and percentage columns.
    if let Some(required_max) = required_max {
      let current_max_sum: f32 = spanned.iter().map(|c| c.max_width).sum();
      if required_max > current_max_sum {
        let distribute_max =
          |cols: &mut [ColumnConstraints], indices: &[usize], need: f32| -> f32 {
            if indices.is_empty() || need <= 0.0 {
              return need;
            }

            let mut remaining = need;
            let mut total_headroom = 0.0;
            let mut infinite = 0usize;
            for &i in indices {
              let headroom = (cols[i].max_width - cols[i].min_width).max(0.0);
              if headroom.is_finite() {
                total_headroom += headroom;
              } else {
                infinite += 1;
              }
            }

            if total_headroom > 0.0 && remaining.is_finite() {
              let mut distributed = 0.0;
              let to_distribute = remaining.min(total_headroom);
              for &i in indices {
                let headroom = (cols[i].max_width - cols[i].min_width).max(0.0);
                if headroom.is_finite() && headroom > 0.0 {
                  let delta = (to_distribute * (headroom / total_headroom)).min(headroom);
                  cols[i].max_width += delta;
                  if cols[i].max_width < cols[i].min_width {
                    cols[i].max_width = cols[i].min_width;
                  }
                  distributed += delta;
                }
              }
              remaining = (remaining - distributed).max(0.0);
            }

            if remaining > 0.0 {
              if infinite > 0 {
                let per = remaining / infinite as f32;
                for &i in indices {
                  let headroom = (cols[i].max_width - cols[i].min_width).max(0.0);
                  if !headroom.is_finite() {
                    cols[i].max_width += per;
                    if cols[i].max_width < cols[i].min_width {
                      cols[i].max_width = cols[i].min_width;
                    }
                  }
                }
                remaining = 0.0;
              } else {
                let mut total_weight = 0.0;
                for &i in indices {
                  total_weight += cols[i].max_width.max(1.0);
                }
                if total_weight > 0.0 {
                  let mut distributed = 0.0;
                  for &i in indices {
                    let weight = cols[i].max_width.max(1.0);
                    let delta = remaining * (weight / total_weight);
                    cols[i].max_width += delta;
                    if cols[i].max_width < cols[i].min_width {
                      cols[i].max_width = cols[i].min_width;
                    }
                    distributed += delta;
                  }
                  remaining = (remaining - distributed).max(0.0);
                }
              }
            }

            remaining
          };

        let mut extra = required_max - current_max_sum;
        indices.clear();
        indices.extend(spanned.iter().enumerate().filter_map(|(i, c)| {
          if c.max_width > c.min_width + 0.01 {
            Some(i)
          } else {
            None
          }
        }));
        if !indices.is_empty() {
          extra = distribute_max(spanned, indices, extra);
        }

        if extra > 0.0 {
          indices.clear();
          indices.extend(0..spanned.len());
          distribute_max(spanned, indices, extra);
        }
      }
    }
  });
}

/// Assign a percentage width to a spanning cell by splitting it across columns.
///
/// Each column receives an equal share of the percentage value; fixed widths remain untouched.
pub fn distribute_spanning_percentage(
  columns: &mut [ColumnConstraints],
  start_col: usize,
  end_col: usize,
  pct: f32,
) {
  if start_col >= end_col || end_col > columns.len() {
    return;
  }
  let target_pct = pct.max(0.0);
  if target_pct <= 0.0 {
    return;
  }

  let span = &mut columns[start_col..end_col];
  let mut existing_pct = 0.0;
  let mut adjustable = Vec::new();
  for (idx, col) in span.iter().enumerate() {
    if let Some(pct) = col.percentage {
      existing_pct += pct;
    }
    // Only columns without fixed widths can absorb additional percentage.
    if col.fixed_width.is_none() {
      adjustable.push(idx);
    }
  }

  // If authored percentages already satisfy the span or there are no adjustable columns,
  // leave the constraints untouched.
  if existing_pct >= target_pct || adjustable.is_empty() {
    return;
  }

  let remaining = target_pct - existing_pct;
  // Weight by intrinsic rigidity (min width) so wider content receives a proportionally larger share.
  let weights: Vec<f32> = adjustable
    .iter()
    .map(|&i| span[i].min_width.max(1.0))
    .collect();
  let total_weight: f32 = weights.iter().copied().sum::<f32>().max(f32::EPSILON);
  for (idx, weight) in adjustable.into_iter().zip(weights.into_iter()) {
    let current = span[idx].percentage.unwrap_or(0.0);
    let share = remaining * (weight / total_weight);
    span[idx].set_percentage(current + share);
  }
}

/// Compute column constraints from cell widths
///
/// Takes a 2D grid of cell (min, max) widths and computes the constraints
/// for each column.
///
/// # Arguments
///
/// * `cell_widths` - For each row, for each cell: (min_width, max_width, colspan)
/// * `column_count` - Number of columns in the table
///
/// # Returns
///
/// A vector of `ColumnConstraints`, one per column.
///
/// # Examples
///
/// ```ignore
/// use fastrender::layout::contexts::table::compute_column_constraints;
///
/// // 2x2 table with no colspan
/// let cell_widths = vec![
///     vec![(50.0, 100.0, 1), (75.0, 150.0, 1)],
///     vec![(60.0, 120.0, 1), (80.0, 160.0, 1)],
/// ];
///
/// let constraints = compute_column_constraints(&cell_widths, 2);
/// assert_eq!(constraints.len(), 2);
/// // Column 0 min = max(50, 60) = 60
/// assert_eq!(constraints[0].min_width, 60.0);
/// ```
pub fn compute_column_constraints(
  cell_widths: &[Vec<(f32, f32, usize)>],
  column_count: usize,
) -> Vec<ColumnConstraints> {
  let mut constraints: Vec<ColumnConstraints> = (0..column_count)
    .map(|_| ColumnConstraints::zero())
    .collect();

  // First pass: handle non-spanning cells
  for row in cell_widths {
    let mut col_idx = 0;
    for &(min_width, max_width, colspan) in row {
      if col_idx >= column_count {
        break;
      }

      if colspan == 1 {
        // Simple case: cell affects only one column
        constraints[col_idx].min_width = constraints[col_idx].min_width.max(min_width);
        constraints[col_idx].max_width = constraints[col_idx].max_width.max(max_width);
      }

      col_idx += colspan;
    }
  }

  // Second pass: handle spanning cells
  for row in cell_widths {
    let mut col_idx = 0;
    for &(min_width, max_width, colspan) in row {
      if col_idx >= column_count {
        break;
      }

      if colspan > 1 {
        let end_col = (col_idx + colspan).min(column_count);
        distribute_spanning_cell_width(&mut constraints, col_idx, end_col, min_width, max_width);
      }

      col_idx += colspan;
    }
  }

  // Ensure all columns have max >= min
  for col in &mut constraints {
    if col.max_width < col.min_width {
      col.max_width = col.min_width;
    }
  }

  constraints
}

#[cfg(test)]
mod tests {
  use super::*;

  // ========== ColumnConstraints Tests ==========

  #[test]
  fn test_column_constraints_new() {
    let col = ColumnConstraints::new(50.0, 150.0);
    assert_eq!(col.min_width, 50.0);
    assert_eq!(col.max_width, 150.0);
    assert!(col.is_flexible);
    assert!(col.fixed_width.is_none());
    assert!(col.percentage.is_none());
  }

  #[test]
  fn test_column_constraints_fixed() {
    let col = ColumnConstraints::fixed(100.0);
    assert_eq!(col.min_width, 100.0);
    assert_eq!(col.max_width, 100.0);
    assert_eq!(col.fixed_width, Some(100.0));
    assert!(!col.is_flexible);
  }

  #[test]
  fn test_column_constraints_percentage() {
    let col = ColumnConstraints::percentage(25.0, 30.0, 200.0);
    assert_eq!(col.percentage, Some(25.0));
    assert_eq!(col.min_width, 30.0);
    assert_eq!(col.max_width, 200.0);
    assert!(!col.is_flexible);
  }

  #[test]
  fn test_column_constraints_percentage_clamping() {
    let col = ColumnConstraints::percentage(150.0, 0.0, 100.0);
    assert_eq!(col.percentage, Some(150.0)); // Values above 100% are allowed

    let col2 = ColumnConstraints::percentage(-10.0, 0.0, 100.0);
    assert_eq!(col2.percentage, Some(0.0)); // Clamped to 0%
  }

  #[test]
  fn test_column_constraints_zero() {
    let col = ColumnConstraints::zero();
    assert_eq!(col.min_width, 0.0);
    assert_eq!(col.max_width, 0.0);
    assert!(col.is_flexible);
  }

  #[test]
  fn test_column_constraints_flexibility_range() {
    let col = ColumnConstraints::new(50.0, 150.0);
    assert_eq!(col.flexibility_range(), 100.0);

    let fixed = ColumnConstraints::fixed(100.0);
    assert_eq!(fixed.flexibility_range(), 0.0);
  }

  #[test]
  fn test_column_constraints_min_greater_than_max() {
    // Constructor should normalize max >= min
    let col = ColumnConstraints::new(150.0, 50.0);
    assert!(col.max_width >= col.min_width);
  }

  #[test]
  fn test_column_constraints_display() {
    let fixed = ColumnConstraints::fixed(100.0);
    assert!(format!("{}", fixed).contains("100"));

    let pct = ColumnConstraints::percentage(25.0, 0.0, 100.0);
    assert!(format!("{}", pct).contains("25%"));

    let auto = ColumnConstraints::new(50.0, 150.0);
    assert!(format!("{}", auto).contains("Auto"));
  }

  // ========== ColumnDistributor Tests - Basic ==========

  #[test]
  fn test_distributor_empty_columns() {
    let distributor = ColumnDistributor::new(DistributionMode::Auto);
    let result = distributor.distribute(&[], 500.0);

    assert_eq!(result.column_count(), 0);
    assert_eq!(result.total_width, 0.0);
  }

  #[test]
  fn test_distributor_single_column() {
    let columns = vec![ColumnConstraints::new(100.0, 200.0)];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 150.0);
    assert_eq!(result.column_count(), 1);
    assert!((result.widths[0] - 150.0).abs() < 0.01);
  }

  #[test]
  fn test_distributor_equal_columns() {
    let columns = vec![
      ColumnConstraints::new(100.0, 200.0),
      ColumnConstraints::new(100.0, 200.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 300.0);
    assert_eq!(result.column_count(), 2);
    // Both columns should get equal width
    assert!((result.widths[0] - result.widths[1]).abs() < 0.01);
    assert!((result.total_width - 300.0).abs() < 0.01);
  }

  // ========== ColumnDistributor Tests - Fixed Width ==========

  #[test]
  fn test_distributor_fixed_width_columns() {
    let columns = vec![
      ColumnConstraints::fixed(100.0),
      ColumnConstraints::fixed(150.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 500.0);
    assert_eq!(result.widths[0], 100.0);
    assert_eq!(result.widths[1], 150.0);
  }

  #[test]
  fn test_distributor_mixed_fixed_and_auto() {
    let columns = vec![
      ColumnConstraints::fixed(100.0),
      ColumnConstraints::new(50.0, 500.0), // Flexible with high max
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 500.0);
    assert_eq!(result.widths[0], 100.0);
    // Flexible column gets remaining space (up to its max)
    assert!((result.widths[1] - 400.0).abs() < 0.01);
  }

  // ========== ColumnDistributor Tests - Percentage Width ==========

  #[test]
  fn test_distributor_percentage_columns() {
    let columns = vec![
      ColumnConstraints::percentage(25.0, 50.0, 300.0),
      ColumnConstraints::percentage(75.0, 100.0, 900.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 400.0);
    // 25% of 400 = 100
    assert!((result.widths[0] - 100.0).abs() < 0.01);
    // 75% of 400 = 300
    assert!((result.widths[1] - 300.0).abs() < 0.01);
  }

  #[test]
  fn test_distributor_percentage_respects_minimum() {
    let columns = vec![
      ColumnConstraints::percentage(10.0, 100.0, 500.0), // Min is 100, max is 500
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 200.0);
    // 10% of 200 = 20, but min is 100, so should get 100
    assert_eq!(result.widths[0], 100.0);
  }

  #[test]
  fn percentage_columns_respect_max_width_caps() {
    // Percentage columns should still honor their max-width caps even when the requested percent is larger.
    let mut capped = ColumnConstraints::percentage(80.0, 5.0, 30.0);
    capped.has_max_cap = true;
    let columns = vec![capped];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 200.0);
    // 80% of 200 would be 160px, but the max-width cap keeps it at 30px.
    assert!((result.widths[0] - 30.0).abs() < 0.01);
    assert!(!result.is_over_constrained);
  }

  #[test]
  fn percentage_columns_ignore_content_max_without_cap() {
    // Content-based max widths should not cap percentages unless an authored cap exists.
    let columns = vec![ColumnConstraints::percentage(50.0, 5.0, 20.0)];
    let distributor = ColumnDistributor::new(DistributionMode::Fixed);

    let result = distributor.distribute(&columns, 100.0);
    // 50% of 100 is 50; since no authored cap is present, the content max of 20 should not clamp.
    assert!((result.widths[0] - 50.0).abs() < 0.01);
  }

  #[test]
  fn percentage_columns_honor_cap_in_fixed_layout() {
    let mut capped = ColumnConstraints::percentage(75.0, 5.0, 25.0);
    capped.has_max_cap = true;
    let columns = vec![capped];
    let distributor = ColumnDistributor::new(DistributionMode::Fixed);

    let result = distributor.distribute(&columns, 200.0);
    // 75% of 200 = 150, but the authored cap clamps to 25.
    assert!((result.widths[0] - 25.0).abs() < 0.01);
  }

  #[test]
  fn distributor_handles_infinite_max_widths_without_nan() {
    let mut wide = ColumnConstraints::new(50.0, f32::INFINITY);
    let mut capped = ColumnConstraints::new(50.0, 100.0);
    wide.is_flexible = true;
    capped.is_flexible = true;

    let columns = vec![wide, capped];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 300.0);
    assert!(result.widths.iter().all(|w| w.is_finite()));
    // The capped column should max out at 100, leaving the remainder to the unbounded one.
    assert!((result.widths[1] - 100.0).abs() < 0.01);
    assert!((result.widths[0] - 200.0).abs() < 0.01);
    assert!((result.total_width - 300.0).abs() < 0.01);
  }

  #[test]
  fn distributor_splits_excess_evenly_across_unbounded_columns() {
    let mut a = ColumnConstraints::new(30.0, f32::INFINITY);
    let mut b = ColumnConstraints::new(20.0, f32::INFINITY);
    a.is_flexible = true;
    b.is_flexible = true;

    let columns = vec![a, b];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 200.0);
    assert!(result.widths.iter().all(|w| w.is_finite()));
    // Min widths are 30 and 20, leaving 150px to share: 75px each.
    assert!((result.widths[0] - 105.0).abs() < 0.01);
    assert!((result.widths[1] - 95.0).abs() < 0.01);
    assert!((result.total_width - 200.0).abs() < 0.01);
  }

  #[test]
  fn percentage_columns_scale_when_over_budget() {
    // Two percentage columns that sum to 140% of the available width.
    let columns = vec![
      ColumnConstraints::percentage(60.0, 20.0, 500.0),
      ColumnConstraints::percentage(80.0, 20.0, 500.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 100.0);
    // Percentages are applied against the table width even when they exceed 100%, so the table
    // becomes over-constrained instead of scaling the percents down.
    assert!((result.widths[0] - 60.0).abs() < 0.1);
    assert!((result.widths[1] - 80.0).abs() < 0.1);
    assert!(result.is_over_constrained);
  }

  #[test]
  fn percentage_columns_scale_after_fixed_columns() {
    // A fixed column leaves less room than the percentages demand; the percents should normalize.
    let columns = vec![
      ColumnConstraints::fixed(200.0),
      ColumnConstraints::percentage(70.0, 10.0, 500.0),
      ColumnConstraints::percentage(50.0, 10.0, 500.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 400.0);
    assert!((result.widths[0] - 200.0).abs() < 0.1);
    assert!((result.widths[1] - 280.0).abs() < 0.1); // 70% of 400
    assert!((result.widths[2] - 200.0).abs() < 0.1); // 50% of 400
    assert!(result.is_over_constrained);
    assert!((result.total_width - 680.0).abs() < 0.5);
  }

  #[test]
  fn percentage_over_budget_still_clamps_to_min() {
    // Percentages over budget with mins that force an over-constraint.
    let columns = vec![
      ColumnConstraints::percentage(90.0, 60.0, 500.0),
      ColumnConstraints::percentage(30.0, 60.0, 500.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 100.0);
    // Available space is below the summed minimum, so we clamp to mins and report overflow.
    assert!((result.widths[0] - 60.0).abs() < 0.5);
    assert!((result.widths[1] - 60.0).abs() < 0.5);
    assert!(result.is_over_constrained);
  }

  // ========== ColumnDistributor Tests - Proportional ==========

  #[test]
  fn test_distributor_proportional_by_flexibility() {
    let columns = vec![
      ColumnConstraints::new(50.0, 100.0), // Range: 50
      ColumnConstraints::new(50.0, 200.0), // Range: 150
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 200.0);
    // Total min = 100, available = 200, excess = 100
    // Column 0 should get less of the excess
    assert!(result.widths[1] > result.widths[0]);
  }

  #[test]
  fn test_distributor_at_minimum() {
    let columns = vec![
      ColumnConstraints::new(100.0, 200.0),
      ColumnConstraints::new(150.0, 300.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 250.0);
    // Available = 250, total min = 250
    assert_eq!(result.widths[0], 100.0);
    assert_eq!(result.widths[1], 150.0);
  }

  #[test]
  fn test_distributor_at_maximum() {
    let columns = vec![
      ColumnConstraints::new(50.0, 100.0),
      ColumnConstraints::new(50.0, 100.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 500.0);
    // Available = 500, total max = 200
    assert_eq!(result.widths[0], 100.0);
    assert_eq!(result.widths[1], 100.0);
  }

  // ========== ColumnDistributor Tests - Over-constrained ==========

  #[test]
  fn test_distributor_over_constrained() {
    let columns = vec![
      ColumnConstraints::new(100.0, 200.0),
      ColumnConstraints::new(150.0, 300.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 200.0);
    // Total min = 250, but only 200 available
    assert!(result.is_over_constrained);
    assert!(result.overflow_amount > 0.0);
  }

  #[test]
  fn test_distributor_over_constrained_keeps_min_widths() {
    let columns = vec![
      ColumnConstraints::new(100.0, 200.0),
      ColumnConstraints::new(100.0, 200.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 100.0);
    // Total min = 200, available = 100.
    // Spec keeps minimum column widths and reports overflow instead of scaling below min.
    assert!((result.widths[0] - 100.0).abs() < 0.01);
    assert!((result.widths[1] - 100.0).abs() < 0.01);
    assert!(result.is_over_constrained);
    assert!((result.overflow_amount - 100.0).abs() < 0.01);
    assert!((result.total_width - 200.0).abs() < 0.01);
  }

  // ========== ColumnDistributor Tests - Fixed Mode ==========

  #[test]
  fn test_distributor_fixed_mode_equal() {
    let columns = vec![
      ColumnConstraints::new(50.0, 150.0),
      ColumnConstraints::new(50.0, 150.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Fixed);

    let result = distributor.distribute(&columns, 300.0);
    // In fixed mode, auto columns get equal share
    assert!((result.widths[0] - 150.0).abs() < 0.01);
    assert!((result.widths[1] - 150.0).abs() < 0.01);
  }

  #[test]
  fn test_distributor_fixed_mode_with_fixed_columns() {
    let columns = vec![
      ColumnConstraints::fixed(100.0),
      ColumnConstraints::new(50.0, 300.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Fixed);

    let result = distributor.distribute(&columns, 400.0);
    assert_eq!(result.widths[0], 100.0);
    // Remaining 300 goes to flexible column
    assert!((result.widths[1] - 300.0).abs() < 0.01);
  }

  // ========== ColumnDistributor Tests - Edge Cases ==========

  #[test]
  fn test_distributor_zero_available_width() {
    let columns = vec![
      ColumnConstraints::new(100.0, 200.0),
      ColumnConstraints::new(100.0, 200.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, 0.0);
    // Should use minimums
    assert_eq!(result.widths[0], 100.0);
    assert_eq!(result.widths[1], 100.0);
  }

  #[test]
  fn test_distributor_negative_available_width() {
    let columns = vec![ColumnConstraints::new(100.0, 200.0)];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, -100.0);
    // Should handle gracefully
    assert_eq!(result.widths[0], 100.0); // Falls back to min
  }

  #[test]
  fn distributor_handles_infinite_available_width() {
    let columns = vec![
      ColumnConstraints::new(50.0, 100.0),
      ColumnConstraints::new(75.0, 125.0),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, f32::INFINITY);

    // With unbounded space and finite maxima, we should land on max widths, not NaN/inf.
    assert!((result.widths[0] - 100.0).abs() < 0.01);
    assert!((result.widths[1] - 125.0).abs() < 0.01);
    assert!(result.widths.iter().all(|w| w.is_finite()));
  }

  #[test]
  fn distributor_handles_infinite_max_widths_with_infinite_available() {
    let columns = vec![
      ColumnConstraints::new(10.0, f32::INFINITY),
      ColumnConstraints::new(20.0, f32::INFINITY),
    ];
    let distributor = ColumnDistributor::new(DistributionMode::Auto);

    let result = distributor.distribute(&columns, f32::INFINITY);

    // Fall back to the finite minimums when nothing else is bounded.
    assert!((result.widths[0] - 10.0).abs() < 0.01);
    assert!((result.widths[1] - 20.0).abs() < 0.01);
    assert!(result.widths.iter().all(|w| w.is_finite()));
  }

  #[test]
  fn test_distributor_with_min_column_width() {
    let columns = vec![ColumnConstraints::zero(), ColumnConstraints::zero()];
    let distributor = ColumnDistributor::new(DistributionMode::Auto).with_min_column_width(10.0);

    let result = distributor.distribute(&columns, 100.0);
    assert!(result.widths[0] >= 10.0);
    assert!(result.widths[1] >= 10.0);
  }

  // ========== Spanning Cell Tests ==========

  #[test]
  fn test_distribute_spanning_cell_width_basic() {
    let mut columns = vec![
      ColumnConstraints::new(50.0, 100.0),
      ColumnConstraints::new(50.0, 100.0),
    ];

    distribute_spanning_cell_width(&mut columns, 0, 2, 200.0, 300.0);

    // Columns should be updated to accommodate spanning cell
    let total_min: f32 = columns.iter().map(|c| c.min_width).sum();
    assert!(total_min >= 200.0);
  }

  #[test]
  fn test_distribute_spanning_cell_width_no_change_needed() {
    let mut columns = vec![
      ColumnConstraints::new(100.0, 200.0),
      ColumnConstraints::new(100.0, 200.0),
    ];

    // Spanning cell requires less than current minimums
    distribute_spanning_cell_width(&mut columns, 0, 2, 100.0, 200.0);

    assert_eq!(columns[0].min_width, 100.0);
    assert_eq!(columns[1].min_width, 100.0);
  }

  #[test]
  fn test_distribute_spanning_cell_width_invalid_range() {
    let mut columns = vec![ColumnConstraints::new(50.0, 100.0)];

    // Invalid range - should not panic
    distribute_spanning_cell_width(&mut columns, 0, 5, 200.0, 300.0);
    distribute_spanning_cell_width(&mut columns, 2, 1, 200.0, 300.0);

    // Original values unchanged
    assert_eq!(columns[0].min_width, 50.0);
  }

  #[test]
  fn percentages_do_not_shrink_with_flex_even_when_overconstrained() {
    let columns = vec![
      ColumnConstraints {
        min_width: 200.0,
        max_width: 200.0,
        has_max_cap: false,
        fixed_width: Some(200.0),
        percentage: None,
        is_flexible: false,
      },
      ColumnConstraints::percentage(60.0, 50.0, 300.0),
      ColumnConstraints::new(100.0, 200.0),
    ];

    let distributor = ColumnDistributor::new(DistributionMode::Auto);
    let result = distributor.distribute(&columns, 400.0);

    // Percent column uses its authored percentage of the table width; flex stays at its min.
    assert!((result.widths[1] - 240.0).abs() < 0.5);
    assert!(result.widths[2] >= 100.0);
    assert!(result.is_over_constrained);
  }

  #[test]
  fn percentages_respect_min_even_when_target_is_smaller() {
    let columns = vec![
      ColumnConstraints::percentage(90.0, 120.0, 300.0),
      ColumnConstraints::new(80.0, 150.0),
    ];

    let distributor = ColumnDistributor::new(DistributionMode::Auto);
    let result = distributor.distribute(&columns, 150.0);

    // Percentage column keeps its min, so table is over-constrained.
    assert!((result.widths[0] - 120.0).abs() < 0.5);
    assert!((result.widths[1] - 80.0).abs() < 0.5);
    assert!(result.is_over_constrained);
  }

  #[test]
  fn percentages_over_hundred_normalize_without_flex() {
    let columns = vec![
      ColumnConstraints::percentage(60.0, 10.0, 500.0),
      ColumnConstraints::percentage(80.0, 10.0, 500.0),
    ];

    let distributor = ColumnDistributor::new(DistributionMode::Auto);
    let result = distributor.distribute(&columns, 400.0);

    // Percentages are applied as-authored; table overflows rather than scaling them down.
    assert!((result.widths[0] - 240.0).abs() < 0.5);
    assert!((result.widths[1] - 320.0).abs() < 0.5);
    assert!(result.is_over_constrained);
    assert!(result.widths[0] < result.widths[1]);
  }

  #[test]
  fn percentages_over_hundred_respect_min_and_overconstrain() {
    let columns = vec![
      ColumnConstraints::percentage(60.0, 300.0, 500.0),
      ColumnConstraints::percentage(80.0, 300.0, 500.0),
    ];

    let distributor = ColumnDistributor::new(DistributionMode::Auto);
    let result = distributor.distribute(&columns, 400.0);

    // Mins prevent fitting into 400px; we keep mins and report overflow.
    assert!((result.widths[0] - 300.0).abs() < 0.5);
    assert!((result.widths[1] - 300.0).abs() < 0.5);
    assert!(result.is_over_constrained);
    assert!(result.total_width > 400.0);
  }

  #[test]
  fn distribute_spanning_prefers_existing_headroom() {
    let mut columns = vec![
      ColumnConstraints::new(10.0, 100.0),
      ColumnConstraints::new(30.0, 60.0),
    ];

    distribute_spanning_cell_width(&mut columns, 0, 2, 150.0, 180.0);

    let total_min: f32 = columns.iter().map(|c| c.min_width).sum();
    let total_max: f32 = columns.iter().map(|c| c.max_width).sum();

    assert!((total_min - 150.0).abs() < 0.5);
    assert!((total_max - 180.0).abs() < 0.5);
    // The wider headroom column should absorb more of the required width.
    assert!(columns[0].min_width > columns[1].min_width + 30.0);
    assert!(columns[0].max_width > columns[1].max_width);
  }

  #[test]
  fn distribute_spanning_respects_fixed_columns() {
    // First column is fixed; span should grow only the flexible column.
    let mut columns = vec![
      ColumnConstraints::fixed(50.0),
      ColumnConstraints::new(10.0, 100.0),
    ];
    distribute_spanning_cell_width(&mut columns, 0, 2, 120.0, 180.0);
    assert!((columns[0].min_width - 50.0).abs() < 0.01);
    assert!((columns[0].max_width - 50.0).abs() < 0.01);
    // Flexible column absorbs the extra.
    assert!(columns[1].min_width > 60.0);
    assert!(columns[1].max_width > 120.0);
  }

  #[test]
  fn distribute_spanning_falls_back_to_even_when_no_headroom() {
    let mut columns = vec![
      ColumnConstraints::new(50.0, 50.0),
      ColumnConstraints::new(50.0, 50.0),
    ];

    distribute_spanning_cell_width(&mut columns, 0, 2, 150.0, 150.0);

    let total_min: f32 = columns.iter().map(|c| c.min_width).sum();
    let total_max: f32 = columns.iter().map(|c| c.max_width).sum();

    assert!((total_min - 150.0).abs() < 0.5);
    assert!((total_max - 150.0).abs() < 0.5);
    assert!((columns[0].min_width - 75.0).abs() < 0.5);
    assert!((columns[1].min_width - 75.0).abs() < 0.5);
  }

  #[test]
  fn spanning_min_with_unbounded_headroom_splits_evenly_without_nan() {
    let mut columns = vec![
      ColumnConstraints::new(10.0, f32::INFINITY),
      ColumnConstraints::new(10.0, f32::INFINITY),
    ];

    distribute_spanning_cell_width(&mut columns, 0, 2, 100.0, 150.0);

    for col in &columns {
      assert!(!col.min_width.is_nan());
      assert!(!col.max_width.is_nan());
    }
    let total_min: f32 = columns.iter().map(|c| c.min_width).sum();
    assert!(total_min >= 100.0 - 0.01);
    assert!((columns[0].min_width - columns[1].min_width).abs() < 0.5);
  }

  #[test]
  fn spanning_min_prefers_finite_headroom_before_unbounded() {
    let mut columns = vec![
      ColumnConstraints::new(10.0, f32::INFINITY),
      ColumnConstraints::new(40.0, 60.0),
    ];

    distribute_spanning_cell_width(&mut columns, 0, 2, 170.0, 200.0);

    assert!(!columns[0].min_width.is_nan());
    assert!(!columns[1].min_width.is_nan());
    // Finite column should rise to its cap before the unbounded column absorbs the remainder.
    assert!((columns[1].min_width - 60.0).abs() < 0.5);
    assert!(columns[0].min_width > 100.0);
    let total_min: f32 = columns.iter().map(|c| c.min_width).sum();
    assert!(total_min >= 170.0 - 0.01);
  }

  #[test]
  fn distribute_spanning_weights_minimums_by_current_widths() {
    // Wider column should receive a larger share of the required minimum.
    let mut columns = vec![
      ColumnConstraints::new(20.0, 200.0),
      ColumnConstraints::new(60.0, 200.0),
    ];
    distribute_spanning_cell_width(&mut columns, 0, 2, 120.0, 200.0);

    let total_min: f32 = columns.iter().map(|c| c.min_width).sum();
    assert!((total_min - 120.0).abs() < 0.5);
    assert!(columns[1].min_width > columns[0].min_width + 15.0);
  }

  #[test]
  fn distribute_spanning_max_scales_by_existing_widths() {
    let mut columns = vec![
      ColumnConstraints::new(20.0, 60.0), // smaller max
      ColumnConstraints::new(20.0, 100.0),
    ];
    // Sum max = 160; need 200 -> extra 40 should favor second column.
    distribute_spanning_cell_width(&mut columns, 0, 2, 80.0, 200.0);
    assert!((columns.iter().map(|c| c.max_width).sum::<f32>() - 200.0).abs() < 0.5);
    assert!(columns[1].max_width > columns[0].max_width + 10.0);
  }

  #[test]
  fn distribute_spanning_percentage_weights_by_widths() {
    let mut columns = vec![
      ColumnConstraints::new(20.0, 200.0),
      ColumnConstraints::new(80.0, 200.0),
    ];
    distribute_spanning_percentage(&mut columns, 0, 2, 60.0);
    // Heavier column should receive a larger percentage share.
    let pct0 = columns[0].percentage.unwrap();
    let pct1 = columns[1].percentage.unwrap();
    assert!(pct1 > pct0);
    assert!((pct0 + pct1 - 60.0).abs() < 0.5);
  }

  #[test]
  fn distribute_spanning_percentage_can_grow_existing_percentages() {
    let mut columns = vec![
      ColumnConstraints::percentage(20.0, 10.0, 200.0),
      ColumnConstraints::new(80.0, 200.0),
    ];
    distribute_spanning_percentage(&mut columns, 0, 2, 80.0);

    let pct0 = columns[0].percentage.unwrap();
    let pct1 = columns[1].percentage.unwrap();
    // Existing percentage should increase and the flexible column should absorb most of the remainder.
    assert!(pct0 > 20.0);
    assert!(pct1 > pct0);
    assert!((pct0 + pct1 - 80.0).abs() < 0.5);
  }

  #[test]
  fn distribute_spanning_percentage_skips_fixed_columns() {
    let mut columns = vec![
      ColumnConstraints::fixed(100.0),
      ColumnConstraints::percentage(30.0, 10.0, 200.0),
      ColumnConstraints::new(10.0, 200.0),
    ];
    distribute_spanning_percentage(&mut columns, 0, 3, 90.0);

    let pct0 = columns[0].percentage;
    let pct1 = columns[1].percentage.unwrap();
    let pct2 = columns[2].percentage.unwrap();
    // Fixed column should remain without percentage; remaining share goes to other columns.
    assert!(pct0.is_none());
    assert!(pct1 > 30.0);
    assert!(pct2 > 0.0);
    assert!((pct1 + pct2 - 90.0).abs() < 0.5);
  }

  #[test]
  fn distribute_spanning_max_prefers_headroom_then_even() {
    let mut columns = vec![
      ColumnConstraints::new(50.0, 70.0),
      ColumnConstraints::new(50.0, 120.0),
    ];

    // Need 260 total max; headroom is 20 + 70 before even split.
    distribute_spanning_cell_width(&mut columns, 0, 2, 100.0, 260.0);

    let total_max: f32 = columns.iter().map(|c| c.max_width).sum();
    assert!((total_max - 260.0).abs() < 0.5);
    assert!(columns[1].max_width > columns[0].max_width);
  }

  #[test]
  fn distribute_spanning_max_uses_headroom_before_busting_caps() {
    let mut columns = vec![
      ColumnConstraints::new(20.0, 60.0),
      ColumnConstraints::new(80.0, 90.0),
    ];
    // Sum max = 150; need 200. Prefer the 40px headroom before pushing the tight column.
    distribute_spanning_cell_width(&mut columns, 0, 2, 100.0, 200.0);

    let total_max: f32 = columns.iter().map(|c| c.max_width).sum();
    assert!((total_max - 200.0).abs() < 0.5);
    // The roomy column should grow substantially more than the tight one.
    let growth0 = columns[0].max_width - 60.0;
    let growth1 = columns[1].max_width - 90.0;
    assert!(growth0 > growth1 + 20.0);
  }

  #[test]
  fn spanning_growth_prefers_flexible_columns() {
    let mut columns = vec![
      ColumnConstraints::fixed(100.0),
      ColumnConstraints::new(10.0, 200.0),
    ];

    distribute_spanning_cell_width(&mut columns, 0, 2, 210.0, 210.0);

    // Fixed column stays at its authored width; flexible column absorbs the remainder.
    assert_eq!(columns[0].min_width, 100.0);
    assert!(columns[1].min_width > 100.0);
  }

  #[test]
  fn spanning_percentage_splits_across_columns() {
    let mut columns = vec![
      ColumnConstraints::new(0.0, 100.0),
      ColumnConstraints::new(0.0, 100.0),
    ];
    distribute_spanning_percentage(&mut columns, 0, 2, 50.0);

    assert_eq!(columns[0].percentage, Some(25.0));
    assert_eq!(columns[1].percentage, Some(25.0));
    assert!(!columns[0].is_flexible);
    assert!(!columns[1].is_flexible);
  }

  #[test]
  fn spanning_percentage_respects_existing_percentages_and_fills_rest() {
    let mut columns = vec![
      ColumnConstraints::percentage(20.0, 0.0, 100.0),
      ColumnConstraints::new(0.0, 100.0),
      ColumnConstraints::new(0.0, 100.0),
    ];
    distribute_spanning_percentage(&mut columns, 0, 3, 60.0);

    // Remaining share is split across all adjustable columns using intrinsic widths as weights.
    assert!((columns[0].percentage.unwrap() - 33.333).abs() < 0.5);
    assert!((columns[1].percentage.unwrap() - 13.333).abs() < 0.5);
    assert!((columns[2].percentage.unwrap() - 13.333).abs() < 0.5);
    let total: f32 = columns.iter().filter_map(|c| c.percentage).sum();
    assert!((total - 60.0).abs() < 0.5);
  }

  #[test]
  fn spanning_percentage_skips_when_only_fixed_or_percent_columns() {
    let mut columns = vec![
      ColumnConstraints::fixed(50.0),
      ColumnConstraints::percentage(30.0, 0.0, 100.0),
    ];

    distribute_spanning_percentage(&mut columns, 0, 2, 80.0);

    // No auto columns to receive the remainder; the percentage column absorbs the spanning request.
    assert_eq!(columns[0].fixed_width, Some(50.0));
    assert!((columns[1].percentage.unwrap() - 80.0).abs() < 0.5);
  }

  // ========== Compute Column Constraints Tests ==========

  #[test]
  fn test_compute_column_constraints_simple() {
    let cell_widths = vec![
      vec![(50.0, 100.0, 1), (75.0, 150.0, 1)],
      vec![(60.0, 120.0, 1), (80.0, 160.0, 1)],
    ];

    let constraints = compute_column_constraints(&cell_widths, 2);

    assert_eq!(constraints.len(), 2);
    // Column 0: max(50, 60) = 60
    assert_eq!(constraints[0].min_width, 60.0);
    // Column 1: max(75, 80) = 80
    assert_eq!(constraints[1].min_width, 80.0);
  }

  #[test]
  fn test_compute_column_constraints_with_colspan() {
    let cell_widths = vec![
      vec![(50.0, 100.0, 1), (50.0, 100.0, 1)],
      vec![(200.0, 400.0, 2)], // Spanning cell
    ];

    let constraints = compute_column_constraints(&cell_widths, 2);

    assert_eq!(constraints.len(), 2);
    // Spanning cell requires 200 min across 2 columns
    let total_min: f32 = constraints.iter().map(|c| c.min_width).sum();
    assert!(total_min >= 200.0);
  }

  #[test]
  fn test_compute_column_constraints_empty() {
    let cell_widths: Vec<Vec<(f32, f32, usize)>> = vec![];
    let constraints = compute_column_constraints(&cell_widths, 3);

    assert_eq!(constraints.len(), 3);
    for col in &constraints {
      assert_eq!(col.min_width, 0.0);
    }
  }

  // ========== ColumnWidthDistributionResult Tests ==========

  #[test]
  fn test_distribution_result_column_width() {
    let result = ColumnWidthDistributionResult::new(vec![100.0, 150.0, 200.0]);

    assert_eq!(result.column_width(0), 100.0);
    assert_eq!(result.column_width(1), 150.0);
    assert_eq!(result.column_width(2), 200.0);
    assert_eq!(result.column_width(10), 0.0); // Out of bounds
  }

  #[test]
  fn test_distribution_result_total_width() {
    let result = ColumnWidthDistributionResult::new(vec![100.0, 150.0, 200.0]);
    assert_eq!(result.total_width, 450.0);
  }
}
