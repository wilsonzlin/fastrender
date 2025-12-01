//! CSS property value parsing
//!
//! Parses individual CSS property values.

use super::types::PropertyValue;
use crate::style::color::Rgba;
use crate::style::values::{Length, LengthUnit};

/// Parse a CSS property value
pub fn parse_property_value(property: &str, value_str: &str) -> Option<PropertyValue> {
    let value_str = value_str.trim();
    if value_str.is_empty() {
        return None;
    }

    // Remove trailing !important if present
    let value_str = value_str.trim_end_matches("!important").trim();

    // Try to parse as color first for color properties
    if matches!(
        property,
        "color"
            | "background"
            | "background-color"
            | "border-color"
            | "border-top-color"
            | "border-right-color"
            | "border-bottom-color"
            | "border-left-color"
    ) {
        if let Ok(color) = csscolorparser::parse(value_str) {
            return Some(PropertyValue::Color(Rgba::new(
                (color.r * 255.0) as u8,
                (color.g * 255.0) as u8,
                (color.b * 255.0) as u8,
                color.a as f32,
            )));
        }
    }

    // Try to parse as length
    if let Some(length) = parse_length(value_str) {
        return Some(PropertyValue::Length(length));
    }

    // Try to parse as number
    if let Ok(num) = value_str.parse::<f32>() {
        return Some(PropertyValue::Number(num));
    }

    // Check for percentage
    if value_str.ends_with('%') {
        if let Ok(num) = value_str[..value_str.len() - 1].parse::<f32>() {
            return Some(PropertyValue::Percentage(num));
        }
    }

    // Font family special handling
    if property == "font-family" {
        let families: Vec<String> = value_str
            .split(',')
            .map(|f| f.trim().trim_matches('"').trim_matches('\'').to_string())
            .collect();
        return Some(PropertyValue::FontFamily(families));
    }

    // Default to keyword
    Some(PropertyValue::Keyword(value_str.to_string()))
}

/// Parse a CSS length value
pub fn parse_length(s: &str) -> Option<Length> {
    let s = s.trim();

    if s == "0" {
        return Some(Length::px(0.0));
    }

    if let Some(rest) = s.strip_suffix("px") {
        return rest.parse::<f32>().ok().map(Length::px);
    }

    if let Some(rest) = s.strip_suffix("rem") {
        return rest.parse::<f32>().ok().map(Length::rem);
    }

    if let Some(rest) = s.strip_suffix("em") {
        return rest.parse::<f32>().ok().map(Length::em);
    }

    if let Some(rest) = s.strip_suffix("pt") {
        return rest.parse::<f32>().ok().map(|v| Length {
            value: v,
            unit: LengthUnit::Pt,
        });
    }

    if let Some(rest) = s.strip_suffix("%") {
        return rest.parse::<f32>().ok().map(Length::percent);
    }

    if let Some(rest) = s.strip_suffix("vw") {
        return rest.parse::<f32>().ok().map(|v| Length {
            value: v,
            unit: LengthUnit::Vw,
        });
    }

    if let Some(rest) = s.strip_suffix("vh") {
        return rest.parse::<f32>().ok().map(|v| Length {
            value: v,
            unit: LengthUnit::Vh,
        });
    }

    if let Some(rest) = s.strip_suffix("cm") {
        return rest.parse::<f32>().ok().map(|v| Length {
            value: v,
            unit: LengthUnit::Cm,
        });
    }

    if let Some(rest) = s.strip_suffix("mm") {
        return rest.parse::<f32>().ok().map(|v| Length {
            value: v,
            unit: LengthUnit::Mm,
        });
    }

    if let Some(rest) = s.strip_suffix("in") {
        return rest.parse::<f32>().ok().map(|v| Length {
            value: v,
            unit: LengthUnit::In,
        });
    }

    None
}
