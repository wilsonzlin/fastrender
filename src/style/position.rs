//! CSS Position property
//!
//! This module implements the CSS `position` property according to
//! CSS Positioned Layout Module Level 3.
//!
//! The position property determines:
//! - Whether an element participates in normal flow
//! - How the element's containing block is determined
//! - Whether top/right/bottom/left properties have effect
//!
//! # Position Types
//!
//! - **static**: Normal flow, no positioning offset (default)
//! - **relative**: Normal flow, but can be offset
//! - **absolute**: Out-of-flow, positioned relative to containing block
//! - **fixed**: Out-of-flow, positioned relative to viewport
//! - **sticky**: Hybrid between relative and fixed
//!
//! # Examples
//!
//! ```
//! use fastrender::style::Position;
//!
//! let pos = Position::parse("absolute").unwrap();
//! assert!(pos.is_positioned());
//! assert!(pos.is_absolutely_positioned());
//! ```

use std::fmt;

/// CSS position property value
///
/// Represents how an element is positioned in the document.
///
/// # Examples
///
/// ```
/// use fastrender::style::Position;
///
/// let static_pos = Position::Static;
/// assert!(!static_pos.is_positioned());
///
/// let absolute_pos = Position::Absolute;
/// assert!(absolute_pos.is_positioned());
/// assert!(absolute_pos.is_absolutely_positioned());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Position {
    /// Static positioning (normal flow, default)
    ///
    /// The element is positioned according to normal flow.
    /// The top, right, bottom, left, and z-index properties have no effect.
    ///
    /// This is the default value.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// let pos = Position::Static;
    /// assert!(!pos.is_positioned());
    /// assert!(pos.is_in_flow());
    /// ```
    Static,

    /// Relative positioning (normal flow + offset)
    ///
    /// The element is positioned according to normal flow, then offset
    /// relative to itself based on top, right, bottom, left values.
    ///
    /// The element still occupies space in normal flow.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// let pos = Position::Relative;
    /// assert!(pos.is_positioned());
    /// assert!(pos.is_in_flow());
    /// ```
    Relative,

    /// Absolute positioning (out of flow)
    ///
    /// The element is removed from normal flow and positioned relative
    /// to its containing block (nearest positioned ancestor).
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// let pos = Position::Absolute;
    /// assert!(pos.is_positioned());
    /// assert!(pos.is_absolutely_positioned());
    /// assert!(!pos.is_in_flow());
    /// ```
    Absolute,

    /// Fixed positioning (out of flow, viewport-relative)
    ///
    /// The element is removed from normal flow and positioned relative
    /// to the viewport (initial containing block).
    ///
    /// The element stays in place during scrolling.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// let pos = Position::Fixed;
    /// assert!(pos.is_positioned());
    /// assert!(pos.is_fixed());
    /// assert!(!pos.is_in_flow());
    /// ```
    Fixed,

    /// Sticky positioning (hybrid)
    ///
    /// The element is positioned according to normal flow until it
    /// crosses a threshold (based on top, right, bottom, left), then
    /// it behaves like fixed positioning.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// let pos = Position::Sticky;
    /// assert!(pos.is_positioned());
    /// assert!(pos.is_sticky());
    /// assert!(pos.is_in_flow()); // In flow until threshold
    /// ```
    Sticky,
}

impl Default for Position {
    fn default() -> Self {
        Position::Static
    }
}

impl Position {
    /// Returns true if this is a positioned element
    ///
    /// Positioned elements are those with position values other than static.
    /// They can use top, right, bottom, left, and z-index properties.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// assert!(!Position::Static.is_positioned());
    /// assert!(Position::Relative.is_positioned());
    /// assert!(Position::Absolute.is_positioned());
    /// assert!(Position::Fixed.is_positioned());
    /// assert!(Position::Sticky.is_positioned());
    /// ```
    pub fn is_positioned(self) -> bool {
        !matches!(self, Position::Static)
    }

    /// Returns true if the element participates in normal flow
    ///
    /// Static, relative, and sticky elements are in-flow.
    /// Absolute and fixed elements are out-of-flow.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// assert!(Position::Static.is_in_flow());
    /// assert!(Position::Relative.is_in_flow());
    /// assert!(Position::Sticky.is_in_flow());
    /// assert!(!Position::Absolute.is_in_flow());
    /// assert!(!Position::Fixed.is_in_flow());
    /// ```
    pub fn is_in_flow(self) -> bool {
        matches!(
            self,
            Position::Static | Position::Relative | Position::Sticky
        )
    }

    /// Returns true if the element is absolutely positioned
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// assert!(Position::Absolute.is_absolutely_positioned());
    /// assert!(!Position::Relative.is_absolutely_positioned());
    /// ```
    pub fn is_absolutely_positioned(self) -> bool {
        matches!(self, Position::Absolute)
    }

    /// Returns true if the element is fixed positioned
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// assert!(Position::Fixed.is_fixed());
    /// assert!(!Position::Absolute.is_fixed());
    /// ```
    pub fn is_fixed(self) -> bool {
        matches!(self, Position::Fixed)
    }

    /// Returns true if the element is sticky positioned
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// assert!(Position::Sticky.is_sticky());
    /// assert!(!Position::Relative.is_sticky());
    /// ```
    pub fn is_sticky(self) -> bool {
        matches!(self, Position::Sticky)
    }

    /// Returns true if the element is relatively positioned
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// assert!(Position::Relative.is_relative());
    /// assert!(!Position::Absolute.is_relative());
    /// ```
    pub fn is_relative(self) -> bool {
        matches!(self, Position::Relative)
    }

    /// Returns true if the element is statically positioned (default)
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// assert!(Position::Static.is_static());
    /// assert!(!Position::Relative.is_static());
    /// ```
    pub fn is_static(self) -> bool {
        matches!(self, Position::Static)
    }

    /// Returns true if this position value can establish a containing block
    /// for absolutely positioned descendants
    ///
    /// Any positioned element (non-static) can be a containing block
    /// for absolutely positioned descendants.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// assert!(!Position::Static.establishes_containing_block());
    /// assert!(Position::Relative.establishes_containing_block());
    /// assert!(Position::Absolute.establishes_containing_block());
    /// assert!(Position::Fixed.establishes_containing_block());
    /// assert!(Position::Sticky.establishes_containing_block());
    /// ```
    pub fn establishes_containing_block(self) -> bool {
        self.is_positioned()
    }

    /// Returns true if the element can use offset properties
    /// (top, right, bottom, left)
    ///
    /// All positioned elements (non-static) can use offset properties.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// assert!(!Position::Static.can_use_offsets());
    /// assert!(Position::Relative.can_use_offsets());
    /// assert!(Position::Absolute.can_use_offsets());
    /// ```
    pub fn can_use_offsets(self) -> bool {
        self.is_positioned()
    }

    /// Parse a position value from a CSS string
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Position;
    ///
    /// assert_eq!(Position::parse("static").unwrap(), Position::Static);
    /// assert_eq!(Position::parse("relative").unwrap(), Position::Relative);
    /// assert_eq!(Position::parse("absolute").unwrap(), Position::Absolute);
    /// assert_eq!(Position::parse("fixed").unwrap(), Position::Fixed);
    /// assert_eq!(Position::parse("sticky").unwrap(), Position::Sticky);
    ///
    /// // Case insensitive
    /// assert_eq!(Position::parse("ABSOLUTE").unwrap(), Position::Absolute);
    ///
    /// // Invalid values
    /// assert!(Position::parse("invalid").is_err());
    /// ```
    pub fn parse(s: &str) -> Result<Self, PositionParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "static" => Ok(Position::Static),
            "relative" => Ok(Position::Relative),
            "absolute" => Ok(Position::Absolute),
            "fixed" => Ok(Position::Fixed),
            "sticky" => Ok(Position::Sticky),
            _ => Err(PositionParseError::InvalidValue(s.to_string())),
        }
    }
}

/// Error when parsing position value
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PositionParseError {
    /// Invalid position value
    InvalidValue(String),
}

impl fmt::Display for PositionParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PositionParseError::InvalidValue(s) => {
                write!(f, "Invalid position value: '{}'", s)
            }
        }
    }
}

impl std::error::Error for PositionParseError {}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Position::Static => write!(f, "static"),
            Position::Relative => write!(f, "relative"),
            Position::Absolute => write!(f, "absolute"),
            Position::Fixed => write!(f, "fixed"),
            Position::Sticky => write!(f, "sticky"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Parsing tests
    #[test]
    fn test_parse_static() {
        assert_eq!(Position::parse("static").unwrap(), Position::Static);
    }

    #[test]
    fn test_parse_relative() {
        assert_eq!(Position::parse("relative").unwrap(), Position::Relative);
    }

    #[test]
    fn test_parse_absolute() {
        assert_eq!(Position::parse("absolute").unwrap(), Position::Absolute);
    }

    #[test]
    fn test_parse_fixed() {
        assert_eq!(Position::parse("fixed").unwrap(), Position::Fixed);
    }

    #[test]
    fn test_parse_sticky() {
        assert_eq!(Position::parse("sticky").unwrap(), Position::Sticky);
    }

    #[test]
    fn test_parse_case_insensitive() {
        assert_eq!(Position::parse("STATIC").unwrap(), Position::Static);
        assert_eq!(Position::parse("Relative").unwrap(), Position::Relative);
        assert_eq!(Position::parse("ABSOLUTE").unwrap(), Position::Absolute);
    }

    #[test]
    fn test_parse_with_whitespace() {
        assert_eq!(Position::parse("  static  ").unwrap(), Position::Static);
        assert_eq!(Position::parse("\trelative\n").unwrap(), Position::Relative);
    }

    #[test]
    fn test_parse_invalid() {
        assert!(Position::parse("invalid").is_err());
        assert!(Position::parse("").is_err());
        assert!(Position::parse("position").is_err());
    }

    // Default test
    #[test]
    fn test_default() {
        assert_eq!(Position::default(), Position::Static);
    }

    // is_positioned tests
    #[test]
    fn test_is_positioned() {
        assert!(!Position::Static.is_positioned());
        assert!(Position::Relative.is_positioned());
        assert!(Position::Absolute.is_positioned());
        assert!(Position::Fixed.is_positioned());
        assert!(Position::Sticky.is_positioned());
    }

    // is_in_flow tests
    #[test]
    fn test_is_in_flow() {
        assert!(Position::Static.is_in_flow());
        assert!(Position::Relative.is_in_flow());
        assert!(Position::Sticky.is_in_flow());
        assert!(!Position::Absolute.is_in_flow());
        assert!(!Position::Fixed.is_in_flow());
    }

    // is_absolutely_positioned tests
    #[test]
    fn test_is_absolutely_positioned() {
        assert!(Position::Absolute.is_absolutely_positioned());
        assert!(!Position::Static.is_absolutely_positioned());
        assert!(!Position::Relative.is_absolutely_positioned());
        assert!(!Position::Fixed.is_absolutely_positioned());
        assert!(!Position::Sticky.is_absolutely_positioned());
    }

    // is_fixed tests
    #[test]
    fn test_is_fixed() {
        assert!(Position::Fixed.is_fixed());
        assert!(!Position::Static.is_fixed());
        assert!(!Position::Relative.is_fixed());
        assert!(!Position::Absolute.is_fixed());
        assert!(!Position::Sticky.is_fixed());
    }

    // is_sticky tests
    #[test]
    fn test_is_sticky() {
        assert!(Position::Sticky.is_sticky());
        assert!(!Position::Static.is_sticky());
        assert!(!Position::Relative.is_sticky());
        assert!(!Position::Absolute.is_sticky());
        assert!(!Position::Fixed.is_sticky());
    }

    // is_relative tests
    #[test]
    fn test_is_relative() {
        assert!(Position::Relative.is_relative());
        assert!(!Position::Static.is_relative());
        assert!(!Position::Absolute.is_relative());
        assert!(!Position::Fixed.is_relative());
        assert!(!Position::Sticky.is_relative());
    }

    // is_static tests
    #[test]
    fn test_is_static() {
        assert!(Position::Static.is_static());
        assert!(!Position::Relative.is_static());
        assert!(!Position::Absolute.is_static());
        assert!(!Position::Fixed.is_static());
        assert!(!Position::Sticky.is_static());
    }

    // establishes_containing_block tests
    #[test]
    fn test_establishes_containing_block() {
        assert!(!Position::Static.establishes_containing_block());
        assert!(Position::Relative.establishes_containing_block());
        assert!(Position::Absolute.establishes_containing_block());
        assert!(Position::Fixed.establishes_containing_block());
        assert!(Position::Sticky.establishes_containing_block());
    }

    // can_use_offsets tests
    #[test]
    fn test_can_use_offsets() {
        assert!(!Position::Static.can_use_offsets());
        assert!(Position::Relative.can_use_offsets());
        assert!(Position::Absolute.can_use_offsets());
        assert!(Position::Fixed.can_use_offsets());
        assert!(Position::Sticky.can_use_offsets());
    }

    // Display trait tests
    #[test]
    fn test_display_formatting() {
        assert_eq!(format!("{}", Position::Static), "static");
        assert_eq!(format!("{}", Position::Relative), "relative");
        assert_eq!(format!("{}", Position::Absolute), "absolute");
        assert_eq!(format!("{}", Position::Fixed), "fixed");
        assert_eq!(format!("{}", Position::Sticky), "sticky");
    }

    // Round-trip test
    #[test]
    fn test_parse_display_roundtrip() {
        let values = vec![
            Position::Static,
            Position::Relative,
            Position::Absolute,
            Position::Fixed,
            Position::Sticky,
        ];

        for position in values {
            let string = format!("{}", position);
            let parsed = Position::parse(&string).unwrap();
            assert_eq!(parsed, position);
        }
    }

    // Combination tests
    #[test]
    fn test_positioned_and_in_flow() {
        // Relative is both positioned and in-flow
        assert!(Position::Relative.is_positioned());
        assert!(Position::Relative.is_in_flow());

        // Sticky is both positioned and in-flow
        assert!(Position::Sticky.is_positioned());
        assert!(Position::Sticky.is_in_flow());

        // Absolute is positioned but not in-flow
        assert!(Position::Absolute.is_positioned());
        assert!(!Position::Absolute.is_in_flow());

        // Static is neither positioned
        assert!(!Position::Static.is_positioned());
        assert!(Position::Static.is_in_flow());
    }
}
