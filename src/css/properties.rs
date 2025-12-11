//! CSS property value parsing
//!
//! Parses individual CSS property values.

use super::types::{
    GradientPosition, GradientPositionComponent, PropertyValue, RadialGradientShape, RadialGradientSize,
};
use crate::style::color::{Color, Rgba};
use crate::style::values::{Length, LengthUnit};
use cssparser::{Parser, ParserInput, Token};

fn tokenize_property_value(value_str: &str, allow_commas: bool) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut in_string: Option<char> = None;
    let mut escape = false;

    for ch in value_str.chars() {
        if escape {
            current.push(ch);
            escape = false;
            continue;
        }
        if ch == '\\' {
            current.push(ch);
            escape = true;
            continue;
        }

        if let Some(q) = in_string {
            current.push(ch);
            if ch == q {
                in_string = None;
            }
            continue;
        }

        match ch {
            '"' | '\'' => {
                in_string = Some(ch);
                current.push(ch);
            }
            '(' => {
                paren += 1;
                current.push(ch);
            }
            ')' => {
                paren -= 1;
                current.push(ch);
            }
            '[' => {
                bracket += 1;
                current.push(ch);
            }
            ']' => {
                bracket -= 1;
                current.push(ch);
            }
            '{' => {
                brace += 1;
                current.push(ch);
            }
            '}' => {
                brace -= 1;
                current.push(ch);
            }
            ',' if allow_commas && paren == 0 && bracket == 0 && brace == 0 => {
                if !current.trim().is_empty() {
                    tokens.push(current.trim().to_string());
                }
                tokens.push(",".to_string());
                current.clear();
            }
            ch if ch.is_whitespace() && paren == 0 && bracket == 0 && brace == 0 => {
                if !current.trim().is_empty() {
                    tokens.push(current.trim().to_string());
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }
    if !current.trim().is_empty() {
        tokens.push(current.trim().to_string());
    }
    tokens
}

/// Parse a CSS property value
pub fn parse_property_value(property: &str, value_str: &str) -> Option<PropertyValue> {
    // Custom properties store their tokens verbatim (post !important stripping handled by caller).
    if property.starts_with("--") {
        return Some(PropertyValue::Custom(value_str.to_string()));
    }

    let value_str = value_str.trim();
    if value_str.is_empty() {
        return None;
    }

    // Remove trailing !important if present
    let value_str = value_str.trim_end_matches("!important").trim();

    let is_background_longhand = property.starts_with("background-") || property == "background";
    let allow_commas = is_background_longhand || property == "cursor";

    // Try to parse as color first for color properties (exclude shorthand background so we can parse layers)
    if matches!(
        property,
        "color"
            | "background-color"
            | "border-color"
            | "border-top-color"
            | "border-right-color"
            | "border-bottom-color"
            | "border-left-color"
    ) {
        if let Ok(color) = Color::parse(value_str) {
            return match color {
                Color::CurrentColor => Some(PropertyValue::Keyword("currentColor".to_string())),
                _ => Some(PropertyValue::Color(color.to_rgba(Rgba::BLACK))),
            };
        }
    }

    // Gradients
    if let Some(gradient) = parse_gradient(value_str) {
        return Some(gradient);
    }

    // Tokenize respecting commas (for background layering) and spaces.
    let tokens: Vec<String> = tokenize_property_value(value_str, allow_commas);
    if tokens.len() > 1
        && matches!(
            property,
            "object-position"
                | "border-spacing"
                | "background-position"
                | "transform-origin"
                | "background"
                | "background-image"
                | "background-repeat"
                | "background-size"
                | "background-attachment"
                | "background-origin"
                | "background-clip"
        )
    {
        let mut parts = Vec::new();
        for token in tokens {
            if token == "/" {
                parts.push(PropertyValue::Keyword("/".to_string()));
                continue;
            }
            if token == "," {
                parts.push(PropertyValue::Keyword(",".to_string()));
                continue;
            }
            if let Some(gradient) = parse_gradient(&token) {
                parts.push(gradient);
            } else if let Some(v) = parse_simple_value(&token) {
                parts.push(v);
            } else if let Ok(color) = Color::parse(&token) {
                match color {
                    Color::CurrentColor => parts.push(PropertyValue::Keyword("currentColor".to_string())),
                    _ => parts.push(PropertyValue::Color(color.to_rgba(Rgba::BLACK))),
                }
            } else {
                parts.push(PropertyValue::Keyword(token));
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

    if value_str.starts_with("url(") && value_str.ends_with(')') {
        let inner = value_str.trim_start_matches("url(").trim_end_matches(')').trim();
        let inner = inner.trim_matches(|c| c == '"' || c == '\'');
        return Some(PropertyValue::Url(inner.to_string()));
    }

    if value_str.ends_with('%') {
        if let Ok(num) = value_str[..value_str.len() - 1].parse::<f32>() {
            return Some(PropertyValue::Percentage(num));
        }
    }

    if let Ok(num) = value_str.parse::<f32>() {
        return Some(PropertyValue::Number(num));
    }

    if let Ok(color) = Color::parse(value_str) {
        return match color {
            Color::CurrentColor => Some(PropertyValue::Keyword("currentColor".to_string())),
            _ => Some(PropertyValue::Color(color.to_rgba(Rgba::BLACK))),
        };
    }

    Some(PropertyValue::Keyword(value_str.to_string()))
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

    let mut shape = RadialGradientShape::Ellipse;
    let mut size = RadialGradientSize::FarthestCorner;
    let mut position = GradientPosition {
        x: GradientPositionComponent {
            alignment: 0.5,
            offset: Length::px(0.0),
        },
        y: GradientPositionComponent {
            alignment: 0.5,
            offset: Length::px(0.0),
        },
    };

    let mut stops = Vec::new();
    let mut start_idx = 1;
    if let Some(cs) = parse_color_stop(parts[0]) {
        stops.push(cs);
    } else {
        start_idx = 1;
        let lower = parts[0].to_ascii_lowercase();
        let (prelude, pos_part) = match lower.find(" at ") {
            Some(idx) => (lower[..idx].trim(), Some(&parts[0][idx + 4..])),
            None => (parts[0], None),
        };

        if let Some(pos_str) = pos_part {
            if let Some(pos) = parse_radial_position(pos_str) {
                position = pos;
            }
        }

        let tokens: Vec<&str> = prelude.split_whitespace().filter(|t| !t.is_empty()).collect();
        let mut explicit_sizes = Vec::new();
        for token in tokens {
            let token_lower = token.to_ascii_lowercase();
            match token_lower.as_str() {
                "circle" => shape = RadialGradientShape::Circle,
                "ellipse" => shape = RadialGradientShape::Ellipse,
                "closest-side" => size = RadialGradientSize::ClosestSide,
                "farthest-side" => size = RadialGradientSize::FarthestSide,
                "closest-corner" => size = RadialGradientSize::ClosestCorner,
                "farthest-corner" => size = RadialGradientSize::FarthestCorner,
                _ => {
                    if let Some(len) = parse_length_token(token) {
                        explicit_sizes.push(len);
                    } else {
                        return None;
                    }
                }
            }
        }

        if !explicit_sizes.is_empty() {
            let first = explicit_sizes[0];
            let second = explicit_sizes.get(1).cloned();
            size = RadialGradientSize::Explicit { x: first, y: second };
        }
    }

    for part in parts.iter().skip(start_idx) {
        if let Some(cs) = parse_color_stop(part) {
            stops.push(cs);
        }
    }

    if stops.len() < 2 {
        return None;
    }

    if repeating {
        Some(PropertyValue::RepeatingRadialGradient {
            shape,
            size,
            position,
            stops,
        })
    } else {
        Some(PropertyValue::RadialGradient {
            shape,
            size,
            position,
            stops,
        })
    }
}

fn parse_length_token(token: &str) -> Option<Length> {
    match parse_property_value("width", token)? {
        PropertyValue::Length(l) => Some(l),
        PropertyValue::Percentage(p) => Some(Length::percent(p)),
        PropertyValue::Number(n) if n == 0.0 => Some(Length::px(0.0)),
        _ => None,
    }
}

fn parse_radial_position(text: &str) -> Option<GradientPosition> {
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum AxisKind {
        Horizontal,
        Vertical,
        Either,
    }

    #[derive(Clone, Copy)]
    enum Part {
        Keyword(AxisKind, f32),
        Offset(Length),
    }

    fn classify(value: &PropertyValue) -> Option<Part> {
        match value {
            PropertyValue::Keyword(kw) => match kw.to_ascii_lowercase().as_str() {
                "left" => Some(Part::Keyword(AxisKind::Horizontal, 0.0)),
                "right" => Some(Part::Keyword(AxisKind::Horizontal, 1.0)),
                "top" => Some(Part::Keyword(AxisKind::Vertical, 0.0)),
                "bottom" => Some(Part::Keyword(AxisKind::Vertical, 1.0)),
                "center" => Some(Part::Keyword(AxisKind::Either, 0.5)),
                _ => None,
            },
            PropertyValue::Length(l) => Some(Part::Offset(*l)),
            PropertyValue::Percentage(p) => Some(Part::Offset(Length::percent(*p))),
            PropertyValue::Number(n) if *n == 0.0 => Some(Part::Offset(Length::px(0.0))),
            _ => None,
        }
    }

    fn component_from_keyword(align: f32, offset: Option<Length>) -> GradientPositionComponent {
        let mut off = offset.unwrap_or_else(|| Length::px(0.0));
        if (align - 1.0).abs() < 1e-6 {
            off.value = -off.value;
        }
        GradientPositionComponent { alignment: align, offset: off }
    }

    fn component_from_single(part: &Part, axis: AxisKind) -> Option<GradientPositionComponent> {
        match (part, axis) {
            (Part::Keyword(kind, align), _) if *kind == AxisKind::Either => Some(component_from_keyword(*align, None)),
            (Part::Keyword(kind, align), AxisKind::Horizontal) if *kind == AxisKind::Horizontal => {
                Some(component_from_keyword(*align, None))
            }
            (Part::Keyword(kind, align), AxisKind::Vertical) if *kind == AxisKind::Vertical => {
                Some(component_from_keyword(*align, None))
            }
            (Part::Offset(len), AxisKind::Horizontal | AxisKind::Vertical) => {
                Some(component_from_keyword(0.0, Some(*len)))
            }
            _ => None,
        }
    }

    let tokens = tokenize_property_value(text, false);
    let parsed_tokens: Vec<PropertyValue> = tokens
        .into_iter()
        .filter_map(|t| parse_property_value("background-position", &t))
        .collect();
    if parsed_tokens.is_empty() {
        return None;
    }
    let parts: Vec<Part> = parsed_tokens.iter().filter_map(classify).collect();
    if parts.is_empty() || parts.len() > 4 {
        return None;
    }

    let mut x: Option<GradientPositionComponent> = None;
    let mut y: Option<GradientPositionComponent> = None;

    match parts.len() {
        1 => {
            x = component_from_single(&parts[0], AxisKind::Horizontal);
            y = component_from_single(&parts[0], AxisKind::Vertical);
        }
        2 => {
            // First token determines axis, second fills the other.
            if let Some(cx) = component_from_single(&parts[0], AxisKind::Horizontal) {
                x = Some(cx);
                y = component_from_single(&parts[1], AxisKind::Vertical).or_else(|| {
                    component_from_single(&parts[0], AxisKind::Vertical)
                });
            } else if let Some(cy) = component_from_single(&parts[0], AxisKind::Vertical) {
                y = Some(cy);
                x = component_from_single(&parts[1], AxisKind::Horizontal).or_else(|| {
                    component_from_single(&parts[0], AxisKind::Horizontal)
                });
            }
            if x.is_none() && y.is_none() {
                // Treat as generic x y ordering.
                x = component_from_single(&parts[0], AxisKind::Horizontal);
                y = component_from_single(&parts[1], AxisKind::Vertical);
            }
        }
        3 => {
            // x keyword [offset] y keyword
            if let Some(cx) = component_from_single(&parts[0], AxisKind::Horizontal) {
                x = Some(component_from_keyword(cx.alignment, match parts[1] {
                    Part::Offset(len) => Some(len),
                    _ => None,
                }));
                y = component_from_single(&parts[2], AxisKind::Vertical);
            } else if let Some(cy) = component_from_single(&parts[0], AxisKind::Vertical) {
                y = Some(component_from_keyword(cy.alignment, match parts[1] {
                    Part::Offset(len) => Some(len),
                    _ => None,
                }));
                x = component_from_single(&parts[2], AxisKind::Horizontal);
            }
        }
        4 => {
            x = component_from_single(&parts[0], AxisKind::Horizontal);
            y = component_from_single(&parts[2], AxisKind::Vertical);
            // offsets are parts[1] and parts[3]
            if let (Some(mut cx), Part::Offset(off)) = (x, parts[1]) {
                cx.offset = off;
                if (cx.alignment - 1.0).abs() < 1e-6 {
                    cx.offset.value = -cx.offset.value;
                }
                x = Some(cx);
            }
            if let (Some(mut cy), Part::Offset(off)) = (y, parts[3]) {
                cy.offset = off;
                if (cy.alignment - 1.0).abs() < 1e-6 {
                    cy.offset.value = -cy.offset.value;
                }
                y = Some(cy);
            }
        }
        _ => {}
    }

    let x = x?;
    let y = y.unwrap_or_else(|| GradientPositionComponent {
        alignment: 0.5,
        offset: Length::px(0.0),
    });
    Some(GradientPosition { x, y })
}

fn split_top_level_commas(input: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut start = 0usize;
    let mut in_string: Option<char> = None;
    let mut escape = false;

    for (i, ch) in input.char_indices() {
        if escape {
            escape = false;
            continue;
        }
        if ch == '\\' {
            escape = true;
            continue;
        }
        if let Some(q) = in_string {
            if ch == q {
                in_string = None;
            }
            continue;
        }
        match ch {
            '"' | '\'' => in_string = Some(ch),
            '(' => paren += 1,
            ')' => paren -= 1,
            '[' => bracket += 1,
            ']' => bracket -= 1,
            '{' => brace += 1,
            '}' => brace -= 1,
            ',' if paren == 0 && bracket == 0 && brace == 0 => {
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
        token[..token.len() - 4].trim().parse::<f32>().ok().map(|t| t * 360.0)
    } else if token.ends_with("grad") {
        token[..token.len() - 4].trim().parse::<f32>().ok().map(|g| g * 0.9)
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
    let color = Color::parse(color_part).ok()?;
    let position = position_part.and_then(parse_stop_position);

    Some(crate::css::types::ColorStop { color, position })
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
        t[..t.len() - 1]
            .trim()
            .parse::<f32>()
            .ok()
            .map(|p| (p / 100.0).clamp(0.0, 1.0))
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
    use crate::style::color::Color;

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
        let PropertyValue::RadialGradient { shape, size, position, stops } =
            parse_property_value("background-image", value).expect("gradient")
        else {
            panic!("expected radial gradient");
        };
        assert!(matches!(shape, RadialGradientShape::Ellipse));
        assert!(matches!(size, RadialGradientSize::FarthestCorner));
        assert!((position.x.alignment - 0.5).abs() < 1e-6);
        assert_eq!(stops.len(), 2);
        assert_eq!(stops[1].position, Some(0.75));
    }

    #[test]
    fn parses_radial_gradient_with_size_and_position() {
        let value = "radial-gradient(circle closest-side at 25% 75%, red, blue)";
        let PropertyValue::RadialGradient { shape, size, position, .. } =
            parse_property_value("background-image", value).expect("gradient")
        else {
            panic!("expected radial gradient");
        };
        assert!(matches!(shape, RadialGradientShape::Circle));
        assert!(matches!(size, RadialGradientSize::ClosestSide));
        assert_eq!(position.x.alignment, 0.0);
        assert_eq!(position.y.alignment, 0.0);
        assert_eq!(position.x.offset.unit, LengthUnit::Percent);
        assert_eq!(position.x.offset.value, 25.0);
        assert_eq!(position.y.offset.unit, LengthUnit::Percent);
        assert_eq!(position.y.offset.value, 75.0);
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
        let PropertyValue::RepeatingRadialGradient { shape, size, position, stops } =
            parse_property_value("background-image", value).expect("gradient")
        else {
            panic!("expected repeating radial gradient");
        };
        assert!(matches!(shape, RadialGradientShape::Ellipse));
        assert!(matches!(size, RadialGradientSize::FarthestCorner));
        assert!((position.y.alignment - 0.5).abs() < 1e-6);
        assert_eq!(stops.len(), 2);
        assert_eq!(stops[0].position, Some(0.10));
        assert_eq!(stops[1].position, Some(0.60));
    }

    #[test]
    fn gradients_accept_currentcolor_and_modern_colors() {
        let value = "linear-gradient(currentColor 10%, hwb(120 20% 10% / 0.5) 90%)";
        let PropertyValue::LinearGradient { stops, .. } =
            parse_property_value("background-image", value).expect("gradient")
        else {
            panic!("expected linear gradient");
        };

        assert_eq!(stops.len(), 2);
        assert_eq!(stops[0].position, Some(0.10));
        assert!(matches!(stops[0].color, Color::CurrentColor));

        match stops[1].color {
            Color::Rgba(c) => assert!((c.a - 0.5).abs() < 1e-6),
            Color::Hsla(h) => {
                let c = h.to_rgba();
                assert!((c.a - 0.5).abs() < 1e-6);
            }
            Color::CurrentColor => panic!("second stop should not be currentColor"),
            Color::Mix { .. } => panic!("second stop should not be a mix"),
        }
        assert_eq!(stops[1].position, Some(0.90));
    }

    #[test]
    fn tokenize_background_ignores_commas_in_functions() {
        let value =
            "linear-gradient(to right, color-mix(in srgb, red 30%, blue) 20%, green 80%), url(\"foo.png\") no-repeat";
        let parsed = parse_property_value("background", value).expect("background");
        let PropertyValue::Multiple(parts) = parsed else {
            panic!("expected layered background tokens");
        };
        assert!(matches!(parts[0], PropertyValue::LinearGradient { .. }));
        assert!(matches!(parts[1], PropertyValue::Keyword(ref k) if k == ","));
        assert!(matches!(parts[2], PropertyValue::Url(ref u) if u == "foo.png"));
        assert!(matches!(parts[3], PropertyValue::Keyword(ref k) if k.eq_ignore_ascii_case("no-repeat")));
    }

    #[test]
    fn parses_lengths_with_all_units_and_case_insensitivity() {
        let cases = [
            ("10px", 10.0, LengthUnit::Px),
            ("5PT", 5.0, LengthUnit::Pt),
            ("3pc", 3.0, LengthUnit::Pc),
            ("2in", 2.0, LengthUnit::In),
            ("1cm", 1.0, LengthUnit::Cm),
            ("4mm", 4.0, LengthUnit::Mm),
            ("8q", 8.0, LengthUnit::Q),
            ("2em", 2.0, LengthUnit::Em),
            ("3rem", 3.0, LengthUnit::Rem),
            ("1ex", 1.0, LengthUnit::Ex),
            ("1ch", 1.0, LengthUnit::Ch),
            ("25vw", 25.0, LengthUnit::Vw),
            ("30vh", 30.0, LengthUnit::Vh),
            ("15vmin", 15.0, LengthUnit::Vmin),
            ("40vmax", 40.0, LengthUnit::Vmax),
            ("75%", 75.0, LengthUnit::Percent),
        ];

        for (text, expected_value, expected_unit) in cases {
            let len = parse_length(text).unwrap_or_else(|| panic!("failed to parse {}", text));
            assert_eq!(len.value, expected_value);
            assert_eq!(len.unit, expected_unit);
        }

        assert_eq!(parse_length("0"), Some(Length::px(0.0)));
        assert_eq!(parse_length("-0"), Some(Length::px(0.0)));
    }

    #[test]
    fn parses_calc_length_when_units_match() {
        let len = parse_length("calc(10px + 5px)").expect("calc length");
        assert_eq!(len, Length::px(15.0));

        let percent = parse_length("calc(50% - 20%)").expect("calc percent");
        assert_eq!(percent, Length::percent(30.0));
    }

    #[test]
    fn rejects_calc_when_units_incompatible() {
        assert!(parse_length("calc(10px + 5%)").is_none());
        assert!(parse_length("calc(10px * 5px)").is_none());
        assert!(parse_length("calc(10px / 0px)").is_none());
    }

    #[test]
    fn parses_min_max_clamp_lengths() {
        assert_eq!(parse_length("min(10px, 20px)").unwrap(), Length::px(10.0));
        assert_eq!(parse_length("max(10px, 20px)").unwrap(), Length::px(20.0));
        assert_eq!(parse_length("clamp(10px, 5px, 15px)").unwrap(), Length::px(10.0));
        assert_eq!(parse_length("clamp(10px, 12px, 15px)").unwrap(), Length::px(12.0));
        assert_eq!(parse_length("clamp(10px, 30px, 15px)").unwrap(), Length::px(15.0));
        assert_eq!(parse_length("min(10%, 20%)").unwrap(), Length::percent(10.0));
    }
}

/// Parse a CSS length value
pub fn parse_length(s: &str) -> Option<Length> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }

    if let Ok(num) = s.parse::<f32>() {
        if num == 0.0 {
            return Some(Length::px(0.0));
        }
    }

    let lower = s.to_ascii_lowercase();
    if lower.starts_with("calc(")
        || lower.starts_with("min(")
        || lower.starts_with("max(")
        || lower.starts_with("clamp(")
    {
        if let Some(len) = parse_function_length(s) {
            return Some(len);
        }
    }

    for (suffix, unit) in [
        ("vmin", LengthUnit::Vmin),
        ("vmax", LengthUnit::Vmax),
        ("vw", LengthUnit::Vw),
        ("vh", LengthUnit::Vh),
        ("rem", LengthUnit::Rem),
        ("em", LengthUnit::Em),
        ("ex", LengthUnit::Ex),
        ("ch", LengthUnit::Ch),
        ("px", LengthUnit::Px),
        ("pc", LengthUnit::Pc),
        ("pt", LengthUnit::Pt),
        ("cm", LengthUnit::Cm),
        ("mm", LengthUnit::Mm),
        ("q", LengthUnit::Q),
        ("in", LengthUnit::In),
        ("%", LengthUnit::Percent),
    ] {
        if let Some(rest) = lower.strip_suffix(suffix) {
            if let Ok(value) = rest.trim().parse::<f32>() {
                return Some(Length::new(value, unit));
            }
        }
    }

    None
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct CalcComponent {
    value: f32,
    unit: Option<LengthUnit>,
}

fn parse_function_length(input: &str) -> Option<Length> {
    let mut parser_input = ParserInput::new(input);
    let mut parser = Parser::new(&mut parser_input);
    let result = parser.parse_entirely(|p| match p.next()? {
        Token::Function(ref name) if name.eq_ignore_ascii_case("calc") => p.parse_nested_block(parse_calc_sum),
        Token::Function(ref name) if name.eq_ignore_ascii_case("min") => {
            p.parse_nested_block(|block| parse_min_max(block, MathFn::Min))
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("max") => {
            p.parse_nested_block(|block| parse_min_max(block, MathFn::Max))
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("clamp") => p.parse_nested_block(parse_clamp),
        _ => Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Custom(()),
            location: p.current_source_location(),
        }),
    });

    match result {
        Ok(component) => calc_component_to_length(component),
        Err(_) => None,
    }
}

pub(crate) fn parse_calc_function_length<'i, 't>(
    input: &mut Parser<'i, 't>,
) -> Result<Length, cssparser::ParseError<'i, ()>> {
    let component = input.parse_nested_block(parse_calc_sum)?;
    calc_component_to_length(component).ok_or(cssparser::ParseError {
        kind: cssparser::ParseErrorKind::Custom(()),
        location: input.current_source_location(),
    })
}

fn calc_component_to_length(component: CalcComponent) -> Option<Length> {
    match component.unit {
        Some(unit) => Some(Length::new(component.value, unit)),
        None => None,
    }
}

fn parse_calc_sum<'i, 't>(input: &mut Parser<'i, 't>) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
    let mut left = parse_calc_product(input)?;
    loop {
        let op = match input.try_parse(|p| match p.next()? {
            Token::Delim('+') => Ok(1.0),
            Token::Delim('-') => Ok(-1.0),
            _ => Err(cssparser::ParseError {
                kind: cssparser::ParseErrorKind::Custom(()),
                location: p.current_source_location(),
            }),
        }) {
            Ok(sign) => sign,
            Err(_) => break,
        };
        let right = parse_calc_product(input)?;
        left = combine_sum(left, right, op)?;
    }
    Ok(left)
}

fn parse_calc_product<'i, 't>(input: &mut Parser<'i, 't>) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
    let mut left = parse_calc_factor(input)?;
    loop {
        let op = match input.try_parse(|p| match p.next()? {
            Token::Delim('*') => Ok('*'),
            Token::Delim('/') => Ok('/'),
            _ => Err(cssparser::ParseError {
                kind: cssparser::ParseErrorKind::Custom(()),
                location: p.current_source_location(),
            }),
        }) {
            Ok(op) => op,
            Err(_) => break,
        };
        let right = parse_calc_factor(input)?;
        left = combine_product(left, right, op)?;
    }
    Ok(left)
}

fn parse_calc_factor<'i, 't>(input: &mut Parser<'i, 't>) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
    let token = input.next()?;
    match token {
        Token::Number { value, .. } => Ok(CalcComponent {
            value: *value,
            unit: None,
        }),
        Token::Dimension { value, ref unit, .. } => {
            let unit = unit.as_ref().to_ascii_lowercase();
            let unit = match unit.as_str() {
                "px" => LengthUnit::Px,
                "em" => LengthUnit::Em,
                "rem" => LengthUnit::Rem,
                "ex" => LengthUnit::Ex,
                "ch" => LengthUnit::Ch,
                "pt" => LengthUnit::Pt,
                "pc" => LengthUnit::Pc,
                "in" => LengthUnit::In,
                "cm" => LengthUnit::Cm,
                "mm" => LengthUnit::Mm,
                "q" => LengthUnit::Q,
                "vw" => LengthUnit::Vw,
                "vh" => LengthUnit::Vh,
                "vmin" => LengthUnit::Vmin,
                "vmax" => LengthUnit::Vmax,
                _ => {
                    return Err(cssparser::ParseError {
                        kind: cssparser::ParseErrorKind::Custom(()),
                        location: input.current_source_location(),
                    })
                }
            };
            Ok(CalcComponent {
                value: *value,
                unit: Some(unit),
            })
        }
        Token::Percentage { unit_value, .. } => Ok(CalcComponent {
            value: *unit_value * 100.0,
            unit: Some(LengthUnit::Percent),
        }),
        Token::Function(ref name) if name.eq_ignore_ascii_case("calc") => input.parse_nested_block(parse_calc_sum),
        Token::Function(ref name) if name.eq_ignore_ascii_case("min") => {
            input.parse_nested_block(|block| parse_min_max(block, MathFn::Min))
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("max") => {
            input.parse_nested_block(|block| parse_min_max(block, MathFn::Max))
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("clamp") => input.parse_nested_block(parse_clamp),
        Token::ParenthesisBlock => input.parse_nested_block(parse_calc_sum),
        Token::Delim('+') => parse_calc_factor(input),
        Token::Delim('-') => {
            let inner = parse_calc_factor(input)?;
            Ok(CalcComponent {
                value: -inner.value,
                unit: inner.unit,
            })
        }
        _ => Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Custom(()),
            location: input.current_source_location(),
        }),
    }
}

#[derive(Clone, Copy)]
pub(crate) enum MathFn {
    Min,
    Max,
}

pub(crate) fn parse_min_max_function_length<'i, 't>(
    input: &mut Parser<'i, 't>,
    func: MathFn,
) -> Result<Length, cssparser::ParseError<'i, ()>> {
    let component = input.parse_nested_block(|block| parse_min_max(block, func))?;
    calc_component_to_length(component).ok_or(cssparser::ParseError {
        kind: cssparser::ParseErrorKind::Custom(()),
        location: input.current_source_location(),
    })
}

pub(crate) fn parse_clamp_function_length<'i, 't>(
    input: &mut Parser<'i, 't>,
) -> Result<Length, cssparser::ParseError<'i, ()>> {
    let component = input.parse_nested_block(parse_clamp)?;
    calc_component_to_length(component).ok_or(cssparser::ParseError {
        kind: cssparser::ParseErrorKind::Custom(()),
        location: input.current_source_location(),
    })
}

fn parse_min_max<'i, 't>(
    input: &mut Parser<'i, 't>,
    func: MathFn,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
    let mut values = Vec::new();
    loop {
        input.skip_whitespace();
        values.push(parse_calc_sum(input)?);
        input.skip_whitespace();
        if input.try_parse(|p| p.expect_comma()).is_err() {
            break;
        }
    }

    if values.len() < 2 {
        return Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Custom(()),
            location: input.current_source_location(),
        });
    }

    reduce_components(values, func)
}

fn parse_clamp<'i, 't>(input: &mut Parser<'i, 't>) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
    input.skip_whitespace();
    let min = parse_calc_sum(input)?;
    input.skip_whitespace();
    input.expect_comma()?;
    input.skip_whitespace();
    let preferred = parse_calc_sum(input)?;
    input.skip_whitespace();
    input.expect_comma()?;
    input.skip_whitespace();
    let max = parse_calc_sum(input)?;

    if !all_same_unit(&[min, preferred, max]) || min.unit.is_none() {
        return Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Custom(()),
            location: input.current_source_location(),
        });
    }

    let preferred_clamped = preferred.value.clamp(min.value, max.value);
    Ok(CalcComponent {
        value: preferred_clamped,
        unit: min.unit,
    })
}

fn all_same_unit(values: &[CalcComponent]) -> bool {
    if values.is_empty() {
        return false;
    }
    let first = values[0].unit;
    values.iter().all(|v| v.unit == first)
}

fn reduce_components<'i>(
    values: Vec<CalcComponent>,
    func: MathFn,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
    if !all_same_unit(&values) || values.iter().any(|v| v.unit.is_none()) {
        return Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Custom(()),
            location: cssparser::SourceLocation { line: 0, column: 0 },
        });
    }

    let unit = values[0].unit;
    let init = match func {
        MathFn::Min => f32::INFINITY,
        MathFn::Max => f32::NEG_INFINITY,
    };
    let result = values.into_iter().fold(init, |acc, v| match func {
        MathFn::Min => acc.min(v.value),
        MathFn::Max => acc.max(v.value),
    });

    Ok(CalcComponent { value: result, unit })
}

fn combine_sum<'i>(
    left: CalcComponent,
    right: CalcComponent,
    sign: f32,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
    let right_value = right.value * sign;
    match (left.unit, right.unit) {
        (Some(u1), Some(u2)) if u1 == u2 => Ok(CalcComponent {
            value: left.value + right_value,
            unit: Some(u1),
        }),
        (Some(_), None) if right.value == 0.0 => Ok(left),
        (None, Some(u2)) if left.value == 0.0 => Ok(CalcComponent {
            value: right_value,
            unit: Some(u2),
        }),
        _ => Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Custom(()),
            location: cssparser::SourceLocation { line: 0, column: 0 },
        }),
    }
}

fn combine_product<'i>(
    left: CalcComponent,
    right: CalcComponent,
    op: char,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
    match op {
        '*' => match (left.unit, right.unit) {
            (Some(_), Some(_)) => Err(cssparser::ParseError {
                kind: cssparser::ParseErrorKind::Custom(()),
                location: cssparser::SourceLocation { line: 0, column: 0 },
            }),
            (Some(unit), None) => Ok(CalcComponent {
                value: left.value * right.value,
                unit: Some(unit),
            }),
            (None, Some(unit)) => Ok(CalcComponent {
                value: left.value * right.value,
                unit: Some(unit),
            }),
            (None, None) => Ok(CalcComponent {
                value: left.value * right.value,
                unit: None,
            }),
        },
        '/' => {
            if right.value == 0.0 {
                return Err(cssparser::ParseError {
                    kind: cssparser::ParseErrorKind::Custom(()),
                    location: cssparser::SourceLocation { line: 0, column: 0 },
                });
            }
            match (left.unit, right.unit) {
                (_, Some(_)) => Err(cssparser::ParseError {
                    kind: cssparser::ParseErrorKind::Custom(()),
                    location: cssparser::SourceLocation { line: 0, column: 0 },
                }),
                (unit, None) => Ok(CalcComponent {
                    value: left.value / right.value,
                    unit,
                }),
            }
        }
        _ => Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Custom(()),
            location: cssparser::SourceLocation { line: 0, column: 0 },
        }),
    }
}
