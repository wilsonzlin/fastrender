//! Layout constraints and available space
//!
//! This module defines how available space is communicated to layout algorithms.
//! Formatting contexts receive constraints that specify what space is available
//! for layout and what the percentage resolution bases are.

use crate::geometry::Size;

/// Available space in a dimension
///
/// CSS layout can have three types of available space:
/// 1. **Definite**: Fixed size (e.g., `width: 800px`)
/// 2. **MinContent**: Shrink to minimum content size
/// 3. **MaxContent**: Expand to maximum content size
///
/// These correspond to the three sizing modes defined in CSS Sizing Module Level 3:
/// - Definite sizing: explicit lengths like `width: 800px`
/// - Min-content sizing: `width: min-content`
/// - Max-content sizing: `width: max-content`
///
/// # Examples
///
/// ```
/// use fastrender::layout::AvailableSpace;
///
/// // Fixed viewport width
/// let definite = AvailableSpace::Definite(1024.0);
/// assert!(definite.is_definite());
/// assert_eq!(definite.definite_value(), Some(1024.0));
///
/// // Shrink-to-fit scenarios
/// let min = AvailableSpace::MinContent;
/// assert!(!min.is_definite());
/// assert_eq!(min.definite_value(), None);
/// ```
///
/// # Reference
///
/// CSS Sizing Module Level 3: https://www.w3.org/TR/css-sizing-3/
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AvailableSpace {
    /// Definite size - a specific length value
    ///
    /// Used when the containing block has a fixed size.
    Definite(f32),

    /// Shrink to minimum content size
    ///
    /// The box should be as narrow as possible without overflowing content.
    /// For text, this is typically the longest word.
    MinContent,

    /// Expand to maximum content size
    ///
    /// The box should be wide enough to fit all content without wrapping.
    /// For text, this is the width of the longest line without any line breaks.
    MaxContent,
}

impl AvailableSpace {
    /// Returns true if this is a definite (fixed) size
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::AvailableSpace;
    ///
    /// assert!(AvailableSpace::Definite(800.0).is_definite());
    /// assert!(!AvailableSpace::MinContent.is_definite());
    /// assert!(!AvailableSpace::MaxContent.is_definite());
    /// ```
    pub fn is_definite(&self) -> bool {
        matches!(self, Self::Definite(_))
    }

    /// Returns the definite value if this is Definite, otherwise None
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::AvailableSpace;
    ///
    /// assert_eq!(AvailableSpace::Definite(800.0).definite_value(), Some(800.0));
    /// assert_eq!(AvailableSpace::MinContent.definite_value(), None);
    /// ```
    pub fn definite_value(&self) -> Option<f32> {
        match self {
            Self::Definite(v) => Some(*v),
            _ => None,
        }
    }
}

/// Layout constraints passed to formatting contexts
///
/// Contains all the information a layout algorithm needs about available space
/// and sizing context. This is passed down the tree during layout, with each
/// formatting context potentially modifying constraints for its children.
///
/// # CSS Specification
///
/// These constraints implement the "containing block" concept from CSS 2.1 Section 10.1.
/// The containing block determines:
/// - How percentage widths/heights are resolved
/// - What space is available for auto-sizing
///
/// # Examples
///
/// ```
/// use fastrender::layout::{LayoutConstraints, AvailableSpace};
///
/// // Create constraints for a 1024x768 viewport
/// let constraints = LayoutConstraints::with_definite_size(1024.0, 768.0);
/// assert_eq!(constraints.available_width, AvailableSpace::Definite(1024.0));
/// assert_eq!(constraints.percentage_base_width, 1024.0);
///
/// // Create indefinite constraints for intrinsic sizing
/// let auto = LayoutConstraints::new(
///     AvailableSpace::MaxContent,
///     AvailableSpace::MaxContent,
/// );
/// ```
///
/// # Reference
///
/// CSS 2.1 Section 10.1 - Containing Block:
/// https://www.w3.org/TR/CSS21/visudet.html#containing-block-details
#[derive(Debug, Clone, Copy)]
pub struct LayoutConstraints {
    /// Available width for layout
    ///
    /// Can be:
    /// - `Definite(w)`: Containing block has explicit width `w`
    /// - `MinContent`: Compute minimum content width
    /// - `MaxContent`: Compute maximum content width
    pub available_width: AvailableSpace,

    /// Available height for layout
    ///
    /// Can be:
    /// - `Definite(h)`: Containing block has explicit height `h`
    /// - `MinContent`: Compute minimum content height
    /// - `MaxContent`: Compute maximum content height
    pub available_height: AvailableSpace,

    /// Percentage base for width calculations
    ///
    /// When a child has `width: 50%`, it's 50% of this value.
    /// This is the containing block's content width.
    ///
    /// Note: This can be non-zero even when `available_width` is indefinite.
    /// For example, in a float context, available width may be indefinite but
    /// percentage base is still the containing block width.
    pub percentage_base_width: f32,

    /// Percentage base for height calculations
    ///
    /// When a child has `height: 50%`, it's 50% of this value.
    /// This is the containing block's content height.
    ///
    /// Note: Percentage heights are more restricted in CSS than widths.
    /// They only work if the containing block has an explicit height.
    pub percentage_base_height: f32,
}

impl LayoutConstraints {
    /// Creates new constraints with specified available space
    ///
    /// Percentage bases are initialized to 0.0 and should be set explicitly
    /// if needed.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::{LayoutConstraints, AvailableSpace};
    ///
    /// let constraints = LayoutConstraints::new(
    ///     AvailableSpace::Definite(800.0),
    ///     AvailableSpace::MaxContent,
    /// );
    /// ```
    pub fn new(width: AvailableSpace, height: AvailableSpace) -> Self {
        Self {
            available_width: width,
            available_height: height,
            percentage_base_width: 0.0,
            percentage_base_height: 0.0,
        }
    }

    /// Creates constraints with definite sizes
    ///
    /// This is the most common case: a containing block with explicit dimensions.
    /// Both available space and percentage bases are set to the given sizes.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::{LayoutConstraints, AvailableSpace};
    ///
    /// let constraints = LayoutConstraints::with_definite_size(1024.0, 768.0);
    /// assert_eq!(constraints.available_width, AvailableSpace::Definite(1024.0));
    /// assert_eq!(constraints.percentage_base_width, 1024.0);
    /// ```
    pub fn with_definite_size(width: f32, height: f32) -> Self {
        Self {
            available_width: AvailableSpace::Definite(width),
            available_height: AvailableSpace::Definite(height),
            percentage_base_width: width,
            percentage_base_height: height,
        }
    }

    /// Creates constraints with specified percentage bases
    ///
    /// Useful when available space is indefinite but percentage resolution
    /// should still work.
    pub fn with_percentage_bases(mut self, width: f32, height: f32) -> Self {
        self.percentage_base_width = width;
        self.percentage_base_height = height;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_available_space_definite() {
        let space = AvailableSpace::Definite(800.0);
        assert!(space.is_definite());
        assert_eq!(space.definite_value(), Some(800.0));
    }

    #[test]
    fn test_available_space_min_content() {
        let space = AvailableSpace::MinContent;
        assert!(!space.is_definite());
        assert_eq!(space.definite_value(), None);
    }

    #[test]
    fn test_available_space_max_content() {
        let space = AvailableSpace::MaxContent;
        assert!(!space.is_definite());
        assert_eq!(space.definite_value(), None);
    }

    #[test]
    fn test_layout_constraints_new() {
        let constraints = LayoutConstraints::new(AvailableSpace::Definite(800.0), AvailableSpace::MaxContent);

        assert_eq!(constraints.available_width, AvailableSpace::Definite(800.0));
        assert_eq!(constraints.available_height, AvailableSpace::MaxContent);
        assert_eq!(constraints.percentage_base_width, 0.0);
        assert_eq!(constraints.percentage_base_height, 0.0);
    }

    #[test]
    fn test_layout_constraints_with_definite_size() {
        let constraints = LayoutConstraints::with_definite_size(1024.0, 768.0);

        assert_eq!(constraints.available_width, AvailableSpace::Definite(1024.0));
        assert_eq!(constraints.available_height, AvailableSpace::Definite(768.0));
        assert_eq!(constraints.percentage_base_width, 1024.0);
        assert_eq!(constraints.percentage_base_height, 768.0);
    }

    #[test]
    fn test_layout_constraints_with_percentage_bases() {
        let constraints = LayoutConstraints::new(AvailableSpace::MaxContent, AvailableSpace::MaxContent)
            .with_percentage_bases(800.0, 600.0);

        assert_eq!(constraints.available_width, AvailableSpace::MaxContent);
        assert_eq!(constraints.percentage_base_width, 800.0);
    }
}
