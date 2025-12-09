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

    // Gradients
    if let Some(gradient) = parse_gradient(value_str) {
        return Some(gradient);
    }

    // Try to parse as a space-separated list first
    let tokens: Vec<&str> = value_str.split_whitespace().collect();
    if tokens.len() > 1
        && matches!(
            property,
            "object-position" | "border-spacing" | "background-position" | "transform-origin"
        )
    {
        let mut parts = Vec::new();
        for token in tokens {
            if let Some(v) = parse_simple_value(token) {
                parts.push(v);
            } else {
                parts.clear();
                break;
            }
        }
        if !parts.is_empty() {
            return Some(PropertyValue::Multiple(parts));
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

fn parse_simple_value(value_str: &str) -> Option<PropertyValue> {
    if let Some(length) = parse_length(value_str) {
        return Some(PropertyValue::Length(length));
    }

    if value_str.ends_with('%') {
        if let Ok(num) = value_str[..value_str.len() - 1].parse::<f32>() {
            return Some(PropertyValue::Percentage(num));
        }
    }

    if let Ok(num) = value_str.parse::<f32>() {
        return Some(PropertyValue::Number(num));
    }

    csscolorparser::parse(value_str)
        .ok()
        .map(|color| {
            PropertyValue::Color(Rgba::new(
                (color.r * 255.0) as u8,
                (color.g * 255.0) as u8,
                (color.b * 255.0) as u8,
                color.a as f32,
            ))
        })
        .or_else(|| Some(PropertyValue::Keyword(value_str.to_string())))
}

fn parse_gradient(value: &str) -> Option<PropertyValue> {
    let lower = value.trim().to_ascii_lowercase();
    if lower.starts_with("linear-gradient(") {
        return parse_linear_gradient(&lower, false);
    }
    if lower.starts_with("radial-gradient(") {
        return parse_radial_gradient(&lower, false);
    }
    if lower.starts_with("repeating-linear-gradient(") {
        return parse_linear_gradient(&lower, true);
    }
    if lower.starts_with("repeating-radial-gradient(") {
        return parse_radial_gradient(&lower, true);
    }
    None
}

fn parse_linear_gradient(value: &str, repeating: bool) -> Option<PropertyValue> {
    let inner = value
        .strip_prefix("linear-gradient(")
        .or_else(|| value.strip_prefix("repeating-linear-gradient("))?
        .strip_suffix(')')?;
    let parts = split_top_level_commas(inner);
    if parts.len() < 2 {
        return None;
    }

    let mut iter = parts.into_iter();
    let first = iter.next().unwrap();
    let (angle, first_stop) = match parse_gradient_angle(first) {
        Some(angle) => (angle, None),
        None => (180.0, Some(first)),
    };

    let mut stops = Vec::new();
    if let Some(stop) = first_stop {
        if let Some(cs) = parse_color_stop(stop) {
            stops.push(cs);
        }
    }
    for part in iter {
        if let Some(cs) = parse_color_stop(part) {
            stops.push(cs);
        }
    }

    if stops.len() < 2 {
        return None;
    }

    if repeating {
        Some(PropertyValue::RepeatingLinearGradient { angle, stops })
    } else {
        Some(PropertyValue::LinearGradient { angle, stops })
    }
}

fn parse_radial_gradient(value: &str, repeating: bool) -> Option<PropertyValue> {
    let inner = value
        .strip_prefix("radial-gradient(")
        .or_else(|| value.strip_prefix("repeating-radial-gradient("))?
        .strip_suffix(')')?;
    let parts = split_top_level_commas(inner);
    if parts.len() < 2 {
        return None;
    }

    let mut stops = Vec::new();
    for part in parts {
        if let Some(cs) = parse_color_stop(part) {
            stops.push(cs);
        }
    }

    if stops.len() < 2 {
        return None;
    }

    if repeating {
        Some(PropertyValue::RepeatingRadialGradient { stops })
    } else {
        Some(PropertyValue::RadialGradient { stops })
    }
}

fn split_top_level_commas(input: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0i32;
    let mut start = 0usize;
    for (i, ch) in input.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => depth -= 1,
            ',' if depth == 0 => {
                parts.push(input[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }
    if start < input.len() {
        parts.push(input[start..].trim());
    }
    parts
}

fn parse_gradient_angle(token: &str) -> Option<f32> {
    let token = token.trim();
    if token.starts_with("to ") {
        let dirs: Vec<&str> = token["to ".len()..].split_whitespace().collect();
        let mut dx = 0i32;
        let mut dy = 0i32;
        for dir in dirs {
            match dir {
                "left" => dx -= 1,
                "right" => dx += 1,
                "top" => dy -= 1,
                "bottom" => dy += 1,
                _ => {}
            }
        }
        if dx == 0 && dy == 0 {
            return None;
        }
        let angle = match (dx.signum(), dy.signum()) {
            (1, 0) => 90.0,
            (-1, 0) => 270.0,
            (0, 1) => 180.0,
            (0, -1) => 0.0,
            (1, -1) => 45.0,
            (1, 1) => 135.0,
            (-1, 1) => 225.0,
            (-1, -1) => 315.0,
            _ => 180.0,
        };
        return Some(angle);
    }

    parse_angle(token)
}

fn parse_angle(token: &str) -> Option<f32> {
    let token = token.trim();
    if token.ends_with("deg") {
        token[..token.len() - 3].trim().parse::<f32>().ok()
    } else if token.ends_with("rad") {
        token[..token.len() - 3]
            .trim()
            .parse::<f32>()
            .ok()
            .map(|r| r.to_degrees())
    } else if token.ends_with("turn") {
        token[..token.len() - 4]
            .trim()
            .parse::<f32>()
            .ok()
            .map(|t| t * 360.0)
    } else if token.ends_with("grad") {
        token[..token.len() - 4]
            .trim()
            .parse::<f32>()
            .ok()
            .map(|g| g * 0.9)
    } else {
        None
    }
}

fn parse_color_stop(token: &str) -> Option<crate::css::types::ColorStop> {
    let trimmed = token.trim();
    if trimmed.is_empty() {
        return None;
    }

    let (color_part, position_part) = split_color_and_position(trimmed);
    let color = csscolorparser::parse(color_part).ok()?;
    let position = position_part.and_then(parse_stop_position);

    Some(crate::css::types::ColorStop {
        color: Rgba::new((color.r * 255.0) as u8, (color.g * 255.0) as u8, (color.b * 255.0) as u8, color.a as f32),
        position,
    })
}

fn split_color_and_position(token: &str) -> (&str, Option<&str>) {
    let mut depth = 0i32;
    let mut split = None;
    for (idx, ch) in token.char_indices().rev() {
        match ch {
            ')' => depth += 1,
            '(' => depth -= 1,
            ' ' | '\t' if depth == 0 => {
                split = Some(idx);
                break;
            }
            _ => {}
        }
    }

    if let Some(idx) = split {
        let color = token[..idx].trim_end();
        let pos = token[idx..].trim();
        if pos.is_empty() {
            (color, None)
        } else {
            (color, Some(pos))
        }
    } else {
        (token, None)
    }
}

fn parse_stop_position(token: &str) -> Option<f32> {
    let t = token.trim();
    if t.ends_with('%') {
        t[..t.len() - 1].trim().parse::<f32>().ok().map(|p| (p / 100.0).clamp(0.0, 1.0))
    } else if let Some(num) = t.parse::<f32>().ok() {
        if num > 1.0 {
            Some((num / 100.0).clamp(0.0, 1.0))
        } else {
            Some(num.clamp(0.0, 1.0))
        }
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_space_separated_values_into_multiple() {
        let parsed = parse_property_value("object-position", "left 25%");
        let PropertyValue::Multiple(list) = parsed.expect("parsed") else {
            panic!("expected Multiple");
        };
        assert_eq!(list.len(), 2);
        assert!(matches!(list[0], PropertyValue::Keyword(ref k) if k == "left"));
        assert!(
            matches!(list[1], PropertyValue::Length(len) if (len.value - 25.0).abs() < 0.01 && len.unit.is_percentage())
                || matches!(list[1], PropertyValue::Percentage(p) if (p - 25.0).abs() < 0.01)
        );
    }

    #[test]
    fn parses_linear_gradient_with_angle_and_stops() {
        let value = "linear-gradient(45deg, red 0%, blue 100%)";
        let PropertyValue::LinearGradient { angle, stops } =
            parse_property_value("background-image", value).expect("gradient")
        else {
            panic!("expected linear gradient");
        };

        assert!((angle - 45.0).abs() < 0.01);
        assert_eq!(stops.len(), 2);
        assert_eq!(stops[0].position, Some(0.0));
        assert_eq!(stops[1].position, Some(1.0));
    }

    #[test]
    fn parses_linear_gradient_direction_keywords() {
        let value = "linear-gradient(to left, #000, #fff)";
        let PropertyValue::LinearGradient { angle, stops } =
            parse_property_value("background-image", value).expect("gradient")
        else {
            panic!("expected linear gradient");
        };
        assert!((angle - 270.0).abs() < 0.01);
        assert_eq!(stops.len(), 2);
    }

    #[test]
    fn parses_radial_gradient() {
        let value = "radial-gradient(red, blue 75%)";
        let PropertyValue::RadialGradient { stops } =
            parse_property_value("background-image", value).expect("gradient")
        else {
            panic!("expected radial gradient");
        };
        assert_eq!(stops.len(), 2);
        assert_eq!(stops[1].position, Some(0.75));
    }

    #[test]
    fn parses_repeating_linear_gradient() {
        let value = "repeating-linear-gradient(180deg, red 0%, blue 50%)";
        let PropertyValue::RepeatingLinearGradient { angle, stops } =
            parse_property_value("background-image", value).expect("gradient")
        else {
            panic!("expected repeating linear gradient");
        };
        assert!((angle - 180.0).abs() < 0.01);
        assert_eq!(stops.len(), 2);
    }

    #[test]
    fn parses_repeating_radial_gradient() {
        let value = "repeating-radial-gradient(red 10%, blue 60%)";
        let PropertyValue::RepeatingRadialGradient { stops } =
            parse_property_value("background-image", value).expect("gradient")
        else {
            panic!("expected repeating radial gradient");
        };
        assert_eq!(stops.len(), 2);
        assert_eq!(stops[0].position, Some(0.10));
        assert_eq!(stops[1].position, Some(0.60));
    }
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
