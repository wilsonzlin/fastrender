//! CSS Float and Clear properties
//!
//! This module implements the CSS `float` and `clear` properties according to
//! CSS 2.1 Section 9.5.
//!
//! # Float Property
//!
//! The `float` property specifies whether a box should float to the left,
//! right, or not at all. Floated boxes are shifted to the left or right
//! until their outer edge touches the containing block edge or the outer
//! edge of another float.
//!
//! # Clear Property
//!
//! The `clear` property specifies which sides of an element's box may not
//! be adjacent to an earlier floating box. The element is moved down past
//! the float(s).
//!
//! # References
//!
//! - CSS 2.1 Section 9.5: https://www.w3.org/TR/CSS21/visuren.html#floats
//! - CSS 2.1 Section 9.5.2: https://www.w3.org/TR/CSS21/visuren.html#propdef-clear

use std::fmt;

/// CSS float property value
///
/// Specifies whether a box should float and to which side.
///
/// # Examples
///
/// ```
/// use fastrender::Float;
///
/// let float = Float::parse("left").unwrap();
/// assert_eq!(float, Float::Left);
/// assert!(float.is_floating());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Float {
    /// Box does not float (default)
    ///
    /// Corresponds to `float: none`
    #[default]
    None,

    /// Box floats to the left
    ///
    /// Corresponds to `float: left`
    /// Content flows on the right side of the float.
    Left,

    /// Box floats to the right
    ///
    /// Corresponds to `float: right`
    /// Content flows on the left side of the float.
    Right,
}

impl Float {
    /// Returns true if this value causes the box to float
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Float;
    ///
    /// assert!(Float::Left.is_floating());
    /// assert!(Float::Right.is_floating());
    /// assert!(!Float::None.is_floating());
    /// ```
    pub fn is_floating(self) -> bool {
        !matches!(self, Float::None)
    }

    /// Parse a float value from a CSS string
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Float;
    ///
    /// assert_eq!(Float::parse("left").unwrap(), Float::Left);
    /// assert_eq!(Float::parse("right").unwrap(), Float::Right);
    /// assert_eq!(Float::parse("none").unwrap(), Float::None);
    /// assert!(Float::parse("invalid").is_err());
    /// ```
    pub fn parse(s: &str) -> Result<Self, FloatParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "none" => Ok(Float::None),
            "left" => Ok(Float::Left),
            "right" => Ok(Float::Right),
            _ => Err(FloatParseError::InvalidValue(s.to_string())),
        }
    }
}

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Float::None => write!(f, "none"),
            Float::Left => write!(f, "left"),
            Float::Right => write!(f, "right"),
        }
    }
}

/// CSS clear property value
///
/// Specifies which sides of an element's box may not be adjacent to
/// an earlier floating box.
///
/// # Examples
///
/// ```
/// use fastrender::Clear;
///
/// let clear = Clear::parse("both").unwrap();
/// assert_eq!(clear, Clear::Both);
/// assert!(clear.clears_left());
/// assert!(clear.clears_right());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Clear {
    /// No constraint on the box's position relative to floats (default)
    ///
    /// Corresponds to `clear: none`
    #[default]
    None,

    /// Box is moved below any left-floating boxes
    ///
    /// Corresponds to `clear: left`
    Left,

    /// Box is moved below any right-floating boxes
    ///
    /// Corresponds to `clear: right`
    Right,

    /// Box is moved below any floating boxes (left or right)
    ///
    /// Corresponds to `clear: both`
    Both,
}

impl Clear {
    /// Returns true if this value clears left floats
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Clear;
    ///
    /// assert!(Clear::Left.clears_left());
    /// assert!(Clear::Both.clears_left());
    /// assert!(!Clear::Right.clears_left());
    /// assert!(!Clear::None.clears_left());
    /// ```
    pub fn clears_left(self) -> bool {
        matches!(self, Clear::Left | Clear::Both)
    }

    /// Returns true if this value clears right floats
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Clear;
    ///
    /// assert!(Clear::Right.clears_right());
    /// assert!(Clear::Both.clears_right());
    /// assert!(!Clear::Left.clears_right());
    /// assert!(!Clear::None.clears_right());
    /// ```
    pub fn clears_right(self) -> bool {
        matches!(self, Clear::Right | Clear::Both)
    }

    /// Returns true if this value clears any floats
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Clear;
    ///
    /// assert!(Clear::Left.is_clearing());
    /// assert!(Clear::Right.is_clearing());
    /// assert!(Clear::Both.is_clearing());
    /// assert!(!Clear::None.is_clearing());
    /// ```
    pub fn is_clearing(self) -> bool {
        !matches!(self, Clear::None)
    }

    /// Parse a clear value from a CSS string
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Clear;
    ///
    /// assert_eq!(Clear::parse("left").unwrap(), Clear::Left);
    /// assert_eq!(Clear::parse("right").unwrap(), Clear::Right);
    /// assert_eq!(Clear::parse("both").unwrap(), Clear::Both);
    /// assert_eq!(Clear::parse("none").unwrap(), Clear::None);
    /// assert!(Clear::parse("invalid").is_err());
    /// ```
    pub fn parse(s: &str) -> Result<Self, ClearParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "none" => Ok(Clear::None),
            "left" => Ok(Clear::Left),
            "right" => Ok(Clear::Right),
            "both" => Ok(Clear::Both),
            _ => Err(ClearParseError::InvalidValue(s.to_string())),
        }
    }
}

impl fmt::Display for Clear {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Clear::None => write!(f, "none"),
            Clear::Left => write!(f, "left"),
            Clear::Right => write!(f, "right"),
            Clear::Both => write!(f, "both"),
        }
    }
}

/// Error when parsing float value
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FloatParseError {
    /// Invalid float value
    InvalidValue(String),
}

impl fmt::Display for FloatParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatParseError::InvalidValue(s) => {
                write!(f, "Invalid float value: '{}'", s)
            }
        }
    }
}

impl std::error::Error for FloatParseError {}

/// Error when parsing clear value
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClearParseError {
    /// Invalid clear value
    InvalidValue(String),
}

impl fmt::Display for ClearParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClearParseError::InvalidValue(s) => {
                write!(f, "Invalid clear value: '{}'", s)
            }
        }
    }
}

impl std::error::Error for ClearParseError {}

#[cfg(test)]
mod tests {
    use super::*;

    // Float parsing tests
    #[test]
    fn test_parse_float_none() {
        assert_eq!(Float::parse("none").unwrap(), Float::None);
    }

    #[test]
    fn test_parse_float_left() {
        assert_eq!(Float::parse("left").unwrap(), Float::Left);
    }

    #[test]
    fn test_parse_float_right() {
        assert_eq!(Float::parse("right").unwrap(), Float::Right);
    }

    #[test]
    fn test_parse_float_case_insensitive() {
        assert_eq!(Float::parse("LEFT").unwrap(), Float::Left);
        assert_eq!(Float::parse("Right").unwrap(), Float::Right);
        assert_eq!(Float::parse("NONE").unwrap(), Float::None);
    }

    #[test]
    fn test_parse_float_with_whitespace() {
        assert_eq!(Float::parse("  left  ").unwrap(), Float::Left);
        assert_eq!(Float::parse("\tright\n").unwrap(), Float::Right);
    }

    #[test]
    fn test_parse_float_invalid() {
        assert!(Float::parse("invalid").is_err());
        assert!(Float::parse("").is_err());
        assert!(Float::parse("center").is_err());
    }

    #[test]
    fn test_float_is_floating() {
        assert!(Float::Left.is_floating());
        assert!(Float::Right.is_floating());
        assert!(!Float::None.is_floating());
    }

    #[test]
    fn test_float_display() {
        assert_eq!(format!("{}", Float::None), "none");
        assert_eq!(format!("{}", Float::Left), "left");
        assert_eq!(format!("{}", Float::Right), "right");
    }

    #[test]
    fn test_float_default() {
        assert_eq!(Float::default(), Float::None);
    }

    #[test]
    fn test_float_roundtrip() {
        for float in [Float::None, Float::Left, Float::Right] {
            let string = format!("{}", float);
            let parsed = Float::parse(&string).unwrap();
            assert_eq!(parsed, float);
        }
    }

    // Clear parsing tests
    #[test]
    fn test_parse_clear_none() {
        assert_eq!(Clear::parse("none").unwrap(), Clear::None);
    }

    #[test]
    fn test_parse_clear_left() {
        assert_eq!(Clear::parse("left").unwrap(), Clear::Left);
    }

    #[test]
    fn test_parse_clear_right() {
        assert_eq!(Clear::parse("right").unwrap(), Clear::Right);
    }

    #[test]
    fn test_parse_clear_both() {
        assert_eq!(Clear::parse("both").unwrap(), Clear::Both);
    }

    #[test]
    fn test_parse_clear_case_insensitive() {
        assert_eq!(Clear::parse("LEFT").unwrap(), Clear::Left);
        assert_eq!(Clear::parse("Right").unwrap(), Clear::Right);
        assert_eq!(Clear::parse("BOTH").unwrap(), Clear::Both);
    }

    #[test]
    fn test_parse_clear_with_whitespace() {
        assert_eq!(Clear::parse("  left  ").unwrap(), Clear::Left);
        assert_eq!(Clear::parse("\tboth\n").unwrap(), Clear::Both);
    }

    #[test]
    fn test_parse_clear_invalid() {
        assert!(Clear::parse("invalid").is_err());
        assert!(Clear::parse("").is_err());
        assert!(Clear::parse("all").is_err());
    }

    #[test]
    fn test_clear_clears_left() {
        assert!(Clear::Left.clears_left());
        assert!(Clear::Both.clears_left());
        assert!(!Clear::Right.clears_left());
        assert!(!Clear::None.clears_left());
    }

    #[test]
    fn test_clear_clears_right() {
        assert!(Clear::Right.clears_right());
        assert!(Clear::Both.clears_right());
        assert!(!Clear::Left.clears_right());
        assert!(!Clear::None.clears_right());
    }

    #[test]
    fn test_clear_is_clearing() {
        assert!(Clear::Left.is_clearing());
        assert!(Clear::Right.is_clearing());
        assert!(Clear::Both.is_clearing());
        assert!(!Clear::None.is_clearing());
    }

    #[test]
    fn test_clear_display() {
        assert_eq!(format!("{}", Clear::None), "none");
        assert_eq!(format!("{}", Clear::Left), "left");
        assert_eq!(format!("{}", Clear::Right), "right");
        assert_eq!(format!("{}", Clear::Both), "both");
    }

    #[test]
    fn test_clear_default() {
        assert_eq!(Clear::default(), Clear::None);
    }

    #[test]
    fn test_clear_roundtrip() {
        for clear in [Clear::None, Clear::Left, Clear::Right, Clear::Both] {
            let string = format!("{}", clear);
            let parsed = Clear::parse(&string).unwrap();
            assert_eq!(parsed, clear);
        }
    }

    // Error message tests
    #[test]
    fn test_float_error_message() {
        let err = Float::parse("invalid").unwrap_err();
        assert_eq!(err.to_string(), "Invalid float value: 'invalid'");
    }

    #[test]
    fn test_clear_error_message() {
        let err = Clear::parse("invalid").unwrap_err();
        assert_eq!(err.to_string(), "Invalid clear value: 'invalid'");
    }
}
