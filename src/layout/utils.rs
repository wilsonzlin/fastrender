//! Shared layout utilities
//!
//! Contains common functions used across multiple layout modules.

use crate::style::values::LengthOrAuto;

/// Resolves a length-or-auto value to an optional pixel value.
///
/// Used for CSS offset properties (top, right, bottom, left) in positioned layout.
///
/// # Arguments
///
/// * `value` - The length or auto value to resolve
/// * `percentage_base` - The base value for percentage resolution
///
/// # Returns
///
/// * `None` if the value is `auto`
/// * `Some(pixels)` if the value is a length
///
/// # CSS Reference
///
/// CSS 2.1 Section 10.3.7 and 10.6.4 describe how offsets are resolved
/// for absolutely positioned elements.
pub fn resolve_offset(value: &LengthOrAuto, percentage_base: f32) -> Option<f32> {
    match value {
        LengthOrAuto::Auto => None,
        LengthOrAuto::Length(length) => {
            if length.unit.is_percentage() {
                Some(length.resolve_against(percentage_base))
            } else if length.unit.is_absolute() {
                Some(length.to_px())
            } else {
                // For relative units (em, rem, etc.), we'd need font context
                // For now, treat as 0 - should be resolved earlier in the pipeline
                Some(0.0)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::values::{Length, LengthUnit};

    #[test]
    fn test_resolve_offset_auto() {
        assert_eq!(resolve_offset(&LengthOrAuto::Auto, 100.0), None);
    }

    #[test]
    fn test_resolve_offset_pixels() {
        let value = LengthOrAuto::Length(Length::new(50.0, LengthUnit::Px));
        assert_eq!(resolve_offset(&value, 100.0), Some(50.0));
    }

    #[test]
    fn test_resolve_offset_percentage() {
        let value = LengthOrAuto::Length(Length::new(25.0, LengthUnit::Percent));
        assert_eq!(resolve_offset(&value, 200.0), Some(50.0));
    }

    #[test]
    fn test_resolve_offset_em_fallback() {
        let value = LengthOrAuto::Length(Length::new(2.0, LengthUnit::Em));
        assert_eq!(resolve_offset(&value, 100.0), Some(0.0));
    }
}
