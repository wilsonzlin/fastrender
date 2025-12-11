//! CSS property value parsing
//!
//! Parses individual CSS property values.

use super::types::PropertyValue;
use crate::style::color::Rgba;
use crate::style::values::{Length, LengthUnit};
use cssparser::{Parser, ParserInput, Token};

fn tokenize_property_value(value_str: &str, allow_commas: bool) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut depth = 0usize;
    for ch in value_str.chars() {
        match ch {
            '(' => {
                depth += 1;
                current.push(ch);
            }
            ')' => {
                if depth > 0 {
                    depth -= 1;
                }
                current.push(ch);
            }
            ',' if allow_commas && depth == 0 => {
                if !current.trim().is_empty() {
                    tokens.push(current.trim().to_string());
                }
                tokens.push(",".to_string());
                current.clear();
            }
            ch if ch.is_whitespace() && depth == 0 => {
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
    let allow_commas = is_background_longhand;

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
            } else if let Ok(color) = csscolorparser::parse(&token) {
                parts.push(PropertyValue::Color(Rgba::new(
                    (color.r * 255.0) as u8,
                    (color.g * 255.0) as u8,
                    (color.b * 255.0) as u8,
                    color.a as f32,
                )));
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
    let color = csscolorparser::parse(color_part).ok()?;
    let position = position_part.and_then(parse_stop_position);

    Some(crate::css::types::ColorStop {
        color: Rgba::new(
            (color.r * 255.0) as u8,
            (color.g * 255.0) as u8,
            (color.b * 255.0) as u8,
            color.a as f32,
        ),
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

    if s.to_ascii_lowercase().starts_with("calc(") {
        if let Some(len) = parse_calc_length(s) {
            return Some(len);
        }
    }

    let lower = s.to_ascii_lowercase();
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

fn parse_calc_length(input: &str) -> Option<Length> {
    let mut parser_input = ParserInput::new(input);
    let mut parser = Parser::new(&mut parser_input);
    let result = parser.parse_entirely(|p| {
        p.expect_function_matching("calc")?;
        p.parse_nested_block(parse_calc_sum)
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
        let op = match input.try_parse(|p| {
            match p.next()? {
                Token::Delim('+') => Ok(1.0),
                Token::Delim('-') => Ok(-1.0),
                _ => Err(cssparser::ParseError {
                    kind: cssparser::ParseErrorKind::Custom(()),
                    location: p.current_source_location(),
                }),
            }
        }) {
            Ok(sign) => sign,
            Err(_) => break,
        };
        let right = parse_calc_product(input)?;
        left = combine_sum(left, right, op)?;
    }
    Ok(left)
}

fn parse_calc_product<'i, 't>(
    input: &mut Parser<'i, 't>,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
    let mut left = parse_calc_factor(input)?;
    loop {
        let op = match input.try_parse(|p| {
            match p.next()? {
                Token::Delim('*') => Ok('*'),
                Token::Delim('/') => Ok('/'),
                _ => Err(cssparser::ParseError {
                    kind: cssparser::ParseErrorKind::Custom(()),
                    location: p.current_source_location(),
                }),
            }
        }) {
            Ok(op) => op,
            Err(_) => break,
        };
        let right = parse_calc_factor(input)?;
        left = combine_product(left, right, op)?;
    }
    Ok(left)
}

fn parse_calc_factor<'i, 't>(
    input: &mut Parser<'i, 't>,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
    let token = input.next()?;
    match token {
        Token::Number { value, .. } => Ok(CalcComponent { value: *value, unit: None }),
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
        Token::Function(ref name) if name.eq_ignore_ascii_case("calc") => {
            input.parse_nested_block(parse_calc_sum)
        }
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
