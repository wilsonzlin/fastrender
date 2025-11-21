//! Layout constraints and available space
//!
//! This module defines how available space is communicated to layout algorithms.

use crate::geometry::Size;

/// Available space in a dimension
///
/// CSS layout can have three types of available space:
/// 1. **Definite**: Fixed size (width: 800px)
/// 2. **MinContent**: Shrink to minimum content size
/// 3. **MaxContent**: Expand to maximum content size
///
/// Reference: CSS Sizing Module Level 3
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AvailableSpace {
    /// Definite size
    Definite(f32),

    /// Shrink to minimum content size
    MinContent,

    /// Expand to maximum content size
    MaxContent,
}

impl AvailableSpace {
    /// Returns true if this is a definite size
    pub fn is_definite(&self) -> bool {
        matches!(self, Self::Definite(_))
    }

    /// Returns the definite value if this is definite
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
/// and sizing context.
#[derive(Debug, Clone, Copy)]
pub struct LayoutConstraints {
    /// Available width
    pub available_width: AvailableSpace,

    /// Available height
    pub available_height: AvailableSpace,

    /// Percentage base for width (containing block width)
    pub percentage_base_width: f32,

    /// Percentage base for height (containing block height)
    pub percentage_base_height: f32,
}

impl LayoutConstraints {
    /// Creates constraints with the given available space
    pub fn new(width: AvailableSpace, height: AvailableSpace) -> Self {
        Self {
            available_width: width,
            available_height: height,
            percentage_base_width: 0.0,
            percentage_base_height: 0.0,
        }
    }

    /// Creates constraints with definite size
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::constraints::LayoutConstraints;
    ///
    /// let constraints = LayoutConstraints::with_definite_size(800.0, 600.0);
    /// assert!(constraints.available_width.is_definite());
    /// ```
    pub fn with_definite_size(width: f32, height: f32) -> Self {
        Self {
            available_width: AvailableSpace::Definite(width),
            available_height: AvailableSpace::Definite(height),
            percentage_base_width: width,
            percentage_base_height: height,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_available_space_definite() {
        let space = AvailableSpace::Definite(100.0);
        assert!(space.is_definite());
        assert_eq!(space.definite_value(), Some(100.0));
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

        assert_eq!(constraints.available_width.definite_value(), Some(800.0));
        assert_eq!(constraints.available_height.definite_value(), None);
    }

    #[test]
    fn test_layout_constraints_with_definite_size() {
        let constraints = LayoutConstraints::with_definite_size(1024.0, 768.0);

        assert_eq!(constraints.available_width.definite_value(), Some(1024.0));
        assert_eq!(constraints.available_height.definite_value(), Some(768.0));
        assert_eq!(constraints.percentage_base_width, 1024.0);
        assert_eq!(constraints.percentage_base_height, 768.0);
    }
}
