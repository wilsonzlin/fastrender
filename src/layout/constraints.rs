//! Layout constraints
//!
//! Constraints describe the available space for layout. Every layout algorithm
//! takes constraints as input to determine sizing.
//!
//! # Available Space
//!
//! CSS defines several modes for available space:
//! - **Definite**: Specific size is available
//! - **Indefinite**: No constraint, size to content
//! - **MinContent**: Size to minimum without overflow
//! - **MaxContent**: Size to maximum without wrapping
//!
//! Reference: CSS Sizing Module Level 3, Section 2.4
//! <https://www.w3.org/TR/css-sizing-3/#available>
//!
//! # Examples
//!
//! ```
//! use fastrender::{AvailableSpace, LayoutConstraints};
//!
//! let constraints = LayoutConstraints::new(
//!     AvailableSpace::Definite(800.0),
//!     AvailableSpace::Indefinite,
//! );
//!
//! assert!(constraints.is_width_definite());
//! assert!(!constraints.is_height_definite());
//! ```

use crate::geometry::Size;
use std::fmt;

/// Available space for layout
///
/// Represents how much space is available in a particular dimension.
/// Used to communicate constraints from parent to child during layout.
///
/// # Variants
///
/// - **Definite**: Specific amount of space available
/// - **Indefinite**: No constraint, size to content
/// - **MinContent**: Size to minimum without overflow
/// - **MaxContent**: Size to maximum without wrapping
///
/// Reference: CSS Sizing Level 3, Section 2.4
///
/// # Examples
///
/// ```
/// use fastrender::AvailableSpace;
///
/// let definite = AvailableSpace::Definite(100.0);
/// assert!(definite.is_definite());
/// assert_eq!(definite.to_option(), Some(100.0));
///
/// let indefinite = AvailableSpace::Indefinite;
/// assert!(!indefinite.is_definite());
/// assert_eq!(indefinite.to_option(), None);
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AvailableSpace {
  /// Specific amount of space available (in CSS pixels)
  ///
  /// Example: Parent is 800px wide, so child has Definite(800.0) available
  Definite(f32),

  /// No constraint on size, should size to content
  ///
  /// Example: Height is usually indefinite, size based on content
  Indefinite,

  /// Size to minimum content without overflow
  ///
  /// Used for: Calculating intrinsic sizes, shrink-to-fit
  MinContent,

  /// Size to maximum content without wrapping
  ///
  /// Used for: Calculating intrinsic sizes, preferred widths
  MaxContent,
}

impl AvailableSpace {
  /// Returns true if this is a definite space
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::AvailableSpace;
  ///
  /// assert!(AvailableSpace::Definite(100.0).is_definite());
  /// assert!(!AvailableSpace::Indefinite.is_definite());
  /// ```
  pub fn is_definite(self) -> bool {
    matches!(self, Self::Definite(_))
  }

  /// Returns true if this is indefinite space
  pub fn is_indefinite(self) -> bool {
    matches!(self, Self::Indefinite)
  }

  /// Returns true if this is min-content sizing
  pub fn is_min_content(self) -> bool {
    matches!(self, Self::MinContent)
  }

  /// Returns true if this is max-content sizing
  pub fn is_max_content(self) -> bool {
    matches!(self, Self::MaxContent)
  }

  /// Returns the definite value if available, otherwise None
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::AvailableSpace;
  ///
  /// assert_eq!(AvailableSpace::Definite(100.0).to_option(), Some(100.0));
  /// assert_eq!(AvailableSpace::Indefinite.to_option(), None);
  /// ```
  pub fn to_option(self) -> Option<f32> {
    match self {
      Self::Definite(value) => Some(value),
      _ => None,
    }
  }

  /// Returns the definite value, or a default if not definite
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::AvailableSpace;
  ///
  /// assert_eq!(AvailableSpace::Definite(100.0).or_else(50.0), 100.0);
  /// assert_eq!(AvailableSpace::Indefinite.or_else(50.0), 50.0);
  /// ```
  pub fn or_else(self, default: f32) -> f32 {
    match self {
      Self::Definite(value) => value,
      _ => default,
    }
  }

  /// Subtracts a value from definite space, returning new space
  ///
  /// If space is not definite, returns unchanged.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::AvailableSpace;
  ///
  /// let space = AvailableSpace::Definite(100.0);
  /// let shrunk = space.shrink_by(20.0);
  /// assert_eq!(shrunk, AvailableSpace::Definite(80.0));
  ///
  /// let indefinite = AvailableSpace::Indefinite;
  /// assert_eq!(indefinite.shrink_by(20.0), AvailableSpace::Indefinite);
  /// ```
  pub fn shrink_by(self, amount: f32) -> Self {
    match self {
      Self::Definite(value) => Self::Definite((value - amount).max(0.0)),
      other => other,
    }
  }

  /// Maps a definite value, leaving other variants unchanged
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::AvailableSpace;
  ///
  /// let space = AvailableSpace::Definite(100.0);
  /// let doubled = space.map(|v| v * 2.0);
  /// assert_eq!(doubled, AvailableSpace::Definite(200.0));
  /// ```
  pub fn map<F>(self, f: F) -> Self
  where
    F: FnOnce(f32) -> f32,
  {
    match self {
      Self::Definite(value) => Self::Definite(f(value)),
      other => other,
    }
  }

  /// Clamps a definite value between min and max
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::AvailableSpace;
  ///
  /// let space = AvailableSpace::Definite(150.0);
  /// let clamped = space.clamp(50.0, 100.0);
  /// assert_eq!(clamped, AvailableSpace::Definite(100.0));
  /// ```
  pub fn clamp(self, min: f32, max: f32) -> Self {
    // CSS sizing rules treat a max value below min as if max equals min. Mirror
    // that to avoid panics from f32::clamp when callers provide inverted bounds.
    let adjusted_max = if max < min { min } else { max };
    self.map(|v| v.max(min).min(adjusted_max))
  }
}

impl fmt::Display for AvailableSpace {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Definite(value) => write!(f, "{}px", value),
      Self::Indefinite => write!(f, "auto"),
      Self::MinContent => write!(f, "min-content"),
      Self::MaxContent => write!(f, "max-content"),
    }
  }
}

/// Layout constraints describing available space
///
/// Constraints specify how much space is available in each dimension.
/// Every layout algorithm takes constraints as input.
///
/// # Examples
///
/// ```
/// use fastrender::{AvailableSpace, LayoutConstraints};
///
/// // Fixed width, flexible height (common for blocks)
/// let constraints = LayoutConstraints::new(
///     AvailableSpace::Definite(800.0),
///     AvailableSpace::Indefinite,
/// );
///
/// // Shrink for margins/padding
/// let inner = constraints.shrink_width_by(40.0);
/// assert_eq!(inner.available_width, AvailableSpace::Definite(760.0));
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LayoutConstraints {
  /// Available space in the inline direction (width for horizontal-tb)
  pub available_width: AvailableSpace,

  /// Available space in the block direction (height for horizontal-tb)
  pub available_height: AvailableSpace,

  /// Optional override for the box's used border-box inline size.
  ///
  /// This is primarily used by layout modes like flex/grid that compute a final
  /// used size for an item (via Taffy) and need to force that size during block
  /// layout without mutating the box tree.
  pub used_border_box_width: Option<f32>,

  /// Optional override for the box's used border-box block size.
  ///
  /// See `used_border_box_width`.
  pub used_border_box_height: Option<f32>,

  /// The containing block inline size used for resolving percentage-based lengths. This remains
  /// available even when `available_width` is intrinsic (min-/max-content) or indefinite so that
  /// percentage resolution can still use a definite parent width instead of falling back to the
  /// viewport.
  pub inline_percentage_base: Option<f32>,
}

impl LayoutConstraints {
  /// Creates new constraints with the given available space
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::{AvailableSpace, LayoutConstraints};
  ///
  /// let constraints = LayoutConstraints::new(
  ///     AvailableSpace::Definite(800.0),
  ///     AvailableSpace::Definite(600.0),
  /// );
  ///
  /// assert!(constraints.is_width_definite());
  /// assert!(constraints.is_height_definite());
  /// ```
  pub const fn new(available_width: AvailableSpace, available_height: AvailableSpace) -> Self {
    let inline_percentage_base = match available_width {
      AvailableSpace::Definite(w) => Some(w),
      _ => None,
    };
    Self {
      available_width,
      available_height,
      used_border_box_width: None,
      used_border_box_height: None,
      inline_percentage_base,
    }
  }

  /// Overrides the inline percentage base while keeping the available space unchanged.
  pub fn with_inline_percentage_base(mut self, base: Option<f32>) -> Self {
    self.inline_percentage_base = base;
    self
  }

  /// Overrides the box's used border-box size while keeping the available space unchanged.
  pub fn with_used_border_box_size(mut self, width: Option<f32>, height: Option<f32>) -> Self {
    self.used_border_box_width = width;
    self.used_border_box_height = height;
    self
  }

  /// Creates constraints with definite width and height
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::LayoutConstraints;
  ///
  /// let constraints = LayoutConstraints::definite(800.0, 600.0);
  /// assert!(constraints.is_width_definite());
  /// assert!(constraints.is_height_definite());
  /// ```
  pub const fn definite(width: f32, height: f32) -> Self {
    Self::new(
      AvailableSpace::Definite(width),
      AvailableSpace::Definite(height),
    )
  }

  /// Creates constraints with definite width and indefinite height
  ///
  /// This is the most common constraint for block layout.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::LayoutConstraints;
  ///
  /// let constraints = LayoutConstraints::definite_width(800.0);
  /// assert!(constraints.is_width_definite());
  /// assert!(constraints.is_height_indefinite());
  /// ```
  pub const fn definite_width(width: f32) -> Self {
    Self::new(AvailableSpace::Definite(width), AvailableSpace::Indefinite)
  }

  /// Creates constraints with indefinite dimensions
  ///
  /// Used for sizing to content in both dimensions.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::LayoutConstraints;
  ///
  /// let constraints = LayoutConstraints::indefinite();
  /// assert!(!constraints.is_width_definite());
  /// assert!(!constraints.is_height_definite());
  /// ```
  pub const fn indefinite() -> Self {
    Self::new(AvailableSpace::Indefinite, AvailableSpace::Indefinite)
  }

  /// Creates constraints for min-content sizing
  ///
  /// Used to compute the minimum size needed.
  pub const fn min_content() -> Self {
    Self::new(AvailableSpace::MinContent, AvailableSpace::MinContent)
  }

  /// Creates constraints for max-content sizing
  ///
  /// Used to compute the preferred size.
  pub const fn max_content() -> Self {
    Self::new(AvailableSpace::MaxContent, AvailableSpace::MaxContent)
  }

  // Modification methods (builder pattern)

  /// Returns new constraints with the specified width
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::{AvailableSpace, LayoutConstraints};
  ///
  /// let constraints = LayoutConstraints::indefinite()
  ///     .with_width(AvailableSpace::Definite(800.0));
  ///
  /// assert!(constraints.is_width_definite());
  /// ```
  pub const fn with_width(mut self, width: AvailableSpace) -> Self {
    self.available_width = width;
    self
  }

  /// Returns new constraints with the specified height
  pub const fn with_height(mut self, height: AvailableSpace) -> Self {
    self.available_height = height;
    self
  }

  /// Shrinks the available width by the given amount
  ///
  /// Used for accounting for margins, padding, and borders.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::{AvailableSpace, LayoutConstraints};
  ///
  /// let outer = LayoutConstraints::definite_width(800.0);
  /// let inner = outer.shrink_width_by(40.0); // 20px margins on each side
  ///
  /// assert_eq!(inner.available_width, AvailableSpace::Definite(760.0));
  /// ```
  pub fn shrink_width_by(mut self, amount: f32) -> Self {
    self.available_width = self.available_width.shrink_by(amount);
    self
  }

  /// Shrinks the available height by the given amount
  pub fn shrink_height_by(mut self, amount: f32) -> Self {
    self.available_height = self.available_height.shrink_by(amount);
    self
  }

  /// Shrinks both dimensions by the given amounts
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::LayoutConstraints;
  ///
  /// let outer = LayoutConstraints::definite(800.0, 600.0);
  /// let inner = outer.shrink_by(40.0, 30.0);
  ///
  /// // Width: 800 - 40 = 760
  /// // Height: 600 - 30 = 570
  /// ```
  pub fn shrink_by(self, width: f32, height: f32) -> Self {
    self.shrink_width_by(width).shrink_height_by(height)
  }

  // Query methods

  /// Returns true if width is definite
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::LayoutConstraints;
  ///
  /// let constraints = LayoutConstraints::definite_width(800.0);
  /// assert!(constraints.is_width_definite());
  /// ```
  pub fn is_width_definite(&self) -> bool {
    self.available_width.is_definite()
  }

  /// Returns true if height is definite
  pub fn is_height_definite(&self) -> bool {
    self.available_height.is_definite()
  }

  /// Returns true if width is indefinite
  pub fn is_width_indefinite(&self) -> bool {
    self.available_width.is_indefinite()
  }

  /// Returns true if height is indefinite
  pub fn is_height_indefinite(&self) -> bool {
    self.available_height.is_indefinite()
  }

  /// Returns true if both dimensions are definite
  pub fn is_fully_definite(&self) -> bool {
    self.is_width_definite() && self.is_height_definite()
  }

  /// Returns the definite width, if any
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::LayoutConstraints;
  ///
  /// let constraints = LayoutConstraints::definite_width(800.0);
  /// assert_eq!(constraints.width(), Some(800.0));
  /// ```
  pub fn width(&self) -> Option<f32> {
    self.available_width.to_option()
  }

  /// Returns the definite height, if any
  pub fn height(&self) -> Option<f32> {
    self.available_height.to_option()
  }

  /// Clamps a size to fit within definite constraints
  ///
  /// If constraints are indefinite, returns the size unchanged.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::LayoutConstraints;
  /// use fastrender::Size;
  ///
  /// let constraints = LayoutConstraints::definite(800.0, 600.0);
  /// let size = Size::new(1000.0, 400.0);
  /// let clamped = constraints.clamp_size(size);
  ///
  /// assert_eq!(clamped.width, 800.0); // Clamped to constraint
  /// assert_eq!(clamped.height, 400.0); // Within constraint
  /// ```
  pub fn clamp_size(&self, size: Size) -> Size {
    let width = if let Some(max_width) = self.width() {
      size.width.min(max_width)
    } else {
      size.width
    };

    let height = if let Some(max_height) = self.height() {
      size.height.min(max_height)
    } else {
      size.height
    };

    Size::new(width, height)
  }
}

impl fmt::Display for LayoutConstraints {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "Constraints(w: {}, h: {})",
      self.available_width, self.available_height
    )
  }
}

impl Default for LayoutConstraints {
  /// Default constraints are indefinite in both dimensions
  fn default() -> Self {
    Self::indefinite()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use Size;

  // AvailableSpace tests
  #[test]
  fn test_available_space_is_definite() {
    assert!(AvailableSpace::Definite(100.0).is_definite());
    assert!(!AvailableSpace::Indefinite.is_definite());
    assert!(!AvailableSpace::MinContent.is_definite());
    assert!(!AvailableSpace::MaxContent.is_definite());
  }

  #[test]
  fn test_available_space_to_option() {
    assert_eq!(AvailableSpace::Definite(100.0).to_option(), Some(100.0));
    assert_eq!(AvailableSpace::Indefinite.to_option(), None);
    assert_eq!(AvailableSpace::MinContent.to_option(), None);
  }

  #[test]
  fn test_available_space_or_else() {
    assert_eq!(AvailableSpace::Definite(100.0).or_else(50.0), 100.0);
    assert_eq!(AvailableSpace::Indefinite.or_else(50.0), 50.0);
    assert_eq!(AvailableSpace::MinContent.or_else(50.0), 50.0);
  }

  #[test]
  fn test_available_space_shrink_by() {
    assert_eq!(
      AvailableSpace::Definite(100.0).shrink_by(20.0),
      AvailableSpace::Definite(80.0)
    );
    assert_eq!(
      AvailableSpace::Indefinite.shrink_by(20.0),
      AvailableSpace::Indefinite
    );
  }

  #[test]
  fn test_available_space_shrink_by_negative() {
    // Shrinking should never go below 0
    assert_eq!(
      AvailableSpace::Definite(10.0).shrink_by(20.0),
      AvailableSpace::Definite(0.0)
    );
  }

  #[test]
  fn test_available_space_map() {
    let space = AvailableSpace::Definite(100.0);
    let doubled = space.map(|v| v * 2.0);
    assert_eq!(doubled, AvailableSpace::Definite(200.0));

    let indefinite = AvailableSpace::Indefinite;
    let mapped = indefinite.map(|v| v * 2.0);
    assert_eq!(mapped, AvailableSpace::Indefinite);
  }

  #[test]
  fn test_available_space_clamp() {
    assert_eq!(
      AvailableSpace::Definite(150.0).clamp(50.0, 100.0),
      AvailableSpace::Definite(100.0)
    );
    assert_eq!(
      AvailableSpace::Definite(25.0).clamp(50.0, 100.0),
      AvailableSpace::Definite(50.0)
    );
    assert_eq!(
      AvailableSpace::Definite(75.0).clamp(50.0, 100.0),
      AvailableSpace::Definite(75.0)
    );
  }

  // LayoutConstraints constructor tests
  #[test]
  fn test_constraints_new() {
    let constraints =
      LayoutConstraints::new(AvailableSpace::Definite(800.0), AvailableSpace::Indefinite);

    assert_eq!(constraints.available_width, AvailableSpace::Definite(800.0));
    assert_eq!(constraints.available_height, AvailableSpace::Indefinite);
  }

  #[test]
  fn test_constraints_definite() {
    let constraints = LayoutConstraints::definite(800.0, 600.0);

    assert!(constraints.is_width_definite());
    assert!(constraints.is_height_definite());
    assert_eq!(constraints.width(), Some(800.0));
    assert_eq!(constraints.height(), Some(600.0));
  }

  #[test]
  fn test_constraints_definite_width() {
    let constraints = LayoutConstraints::definite_width(800.0);

    assert!(constraints.is_width_definite());
    assert!(constraints.is_height_indefinite());
  }

  #[test]
  fn test_constraints_indefinite() {
    let constraints = LayoutConstraints::indefinite();

    assert!(!constraints.is_width_definite());
    assert!(!constraints.is_height_definite());
    assert!(constraints.is_width_indefinite());
    assert!(constraints.is_height_indefinite());
  }

  #[test]
  fn test_constraints_min_content() {
    let constraints = LayoutConstraints::min_content();

    assert!(constraints.available_width.is_min_content());
    assert!(constraints.available_height.is_min_content());
  }

  #[test]
  fn test_constraints_max_content() {
    let constraints = LayoutConstraints::max_content();

    assert!(constraints.available_width.is_max_content());
    assert!(constraints.available_height.is_max_content());
  }

  // Builder pattern tests
  #[test]
  fn test_constraints_with_width() {
    let constraints = LayoutConstraints::indefinite().with_width(AvailableSpace::Definite(800.0));

    assert!(constraints.is_width_definite());
    assert!(constraints.is_height_indefinite());
  }

  #[test]
  fn test_constraints_with_height() {
    let constraints = LayoutConstraints::indefinite().with_height(AvailableSpace::Definite(600.0));

    assert!(constraints.is_width_indefinite());
    assert!(constraints.is_height_definite());
  }

  // Shrinking tests
  #[test]
  fn test_constraints_shrink_width_by() {
    let outer = LayoutConstraints::definite_width(800.0);
    let inner = outer.shrink_width_by(40.0);

    assert_eq!(inner.available_width, AvailableSpace::Definite(760.0));
  }

  #[test]
  fn test_constraints_shrink_height_by() {
    let outer = LayoutConstraints::definite(800.0, 600.0);
    let inner = outer.shrink_height_by(30.0);

    assert_eq!(inner.available_height, AvailableSpace::Definite(570.0));
  }

  #[test]
  fn test_constraints_shrink_by() {
    let outer = LayoutConstraints::definite(800.0, 600.0);
    let inner = outer.shrink_by(40.0, 30.0);

    assert_eq!(inner.available_width, AvailableSpace::Definite(760.0));
    assert_eq!(inner.available_height, AvailableSpace::Definite(570.0));
  }

  #[test]
  fn test_constraints_shrink_indefinite() {
    let outer = LayoutConstraints::indefinite();
    let inner = outer.shrink_by(40.0, 30.0);

    // Shrinking indefinite should remain indefinite
    assert!(inner.is_width_indefinite());
    assert!(inner.is_height_indefinite());
  }

  // Query tests
  #[test]
  fn test_is_fully_definite() {
    assert!(LayoutConstraints::definite(800.0, 600.0).is_fully_definite());
    assert!(!LayoutConstraints::definite_width(800.0).is_fully_definite());
    assert!(!LayoutConstraints::indefinite().is_fully_definite());
  }

  // clamp_size tests
  #[test]
  fn test_clamp_size_within_bounds() {
    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let size = Size::new(400.0, 300.0);
    let clamped = constraints.clamp_size(size);

    assert_eq!(clamped.width, 400.0);
    assert_eq!(clamped.height, 300.0);
  }

  #[test]
  fn test_clamp_size_exceeds_width() {
    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let size = Size::new(1000.0, 300.0);
    let clamped = constraints.clamp_size(size);

    assert_eq!(clamped.width, 800.0);
    assert_eq!(clamped.height, 300.0);
  }

  #[test]
  fn test_clamp_size_exceeds_height() {
    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let size = Size::new(400.0, 800.0);
    let clamped = constraints.clamp_size(size);

    assert_eq!(clamped.width, 400.0);
    assert_eq!(clamped.height, 600.0);
  }

  #[test]
  fn test_clamp_size_indefinite() {
    let constraints = LayoutConstraints::indefinite();
    let size = Size::new(1000.0, 800.0);
    let clamped = constraints.clamp_size(size);

    // No clamping for indefinite
    assert_eq!(clamped.width, 1000.0);
    assert_eq!(clamped.height, 800.0);
  }

  #[test]
  fn test_clamp_size_partial_definite() {
    let constraints = LayoutConstraints::definite_width(800.0);
    let size = Size::new(1000.0, 800.0);
    let clamped = constraints.clamp_size(size);

    assert_eq!(clamped.width, 800.0); // Clamped
    assert_eq!(clamped.height, 800.0); // Not clamped
  }

  // Display tests
  #[test]
  fn test_available_space_display() {
    assert_eq!(format!("{}", AvailableSpace::Definite(100.0)), "100px");
    assert_eq!(format!("{}", AvailableSpace::Indefinite), "auto");
    assert_eq!(format!("{}", AvailableSpace::MinContent), "min-content");
    assert_eq!(format!("{}", AvailableSpace::MaxContent), "max-content");
  }

  #[test]
  fn test_constraints_display() {
    let constraints = LayoutConstraints::definite_width(800.0);
    let display = format!("{}", constraints);
    assert!(display.contains("800px"));
    assert!(display.contains("auto"));
  }

  // Default test
  #[test]
  fn test_constraints_default() {
    let constraints = LayoutConstraints::default();
    assert!(constraints.is_width_indefinite());
    assert!(constraints.is_height_indefinite());
  }
}
