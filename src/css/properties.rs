//! CSS property value parsing
//!
//! Parses individual CSS property values.

use super::types::{
    GradientPosition, GradientPositionComponent, PropertyValue, RadialGradientShape, RadialGradientSize, TextShadow,
    Transform,
};
use crate::style::color::{Color, Rgba};
use crate::style::values::{CalcLength, Length, LengthUnit};
use cssparser::{BasicParseErrorKind, Parser, ParserInput, Token};

/// Set of property names that the engine understands.
///
/// Unknown properties are dropped during parsing (per CSS 2.1 §4.2) and treated as unsupported
/// for feature queries.
const KNOWN_PROPERTIES: &[&str] = &[
    "accent-color",
    "align-content",
    "align-items",
    "align-self",
    "all",
    "aspect-ratio",
    "backdrop-filter",
    "appearance",
    "background",
    "background-attachment",
    "background-blend-mode",
    "background-clip",
    "background-color",
    "background-image",
    "background-origin",
    "background-position",
    "background-position-x",
    "background-position-y",
    "background-repeat",
    "background-size",
    "block-size",
    "border",
    "border-block",
    "border-block-color",
    "border-block-end-color",
    "border-block-end-style",
    "border-block-end-width",
    "border-block-start-color",
    "border-block-start-style",
    "border-block-start-width",
    "border-block-style",
    "border-block-width",
    "border-bottom-color",
    "border-bottom-left-radius",
    "border-bottom-right-radius",
    "border-bottom-style",
    "border-bottom-width",
    "border-collapse",
    "border-color",
    "border-end-end-radius",
    "border-end-start-radius",
    "border-image",
    "border-image-outset",
    "border-image-repeat",
    "border-image-slice",
    "border-image-source",
    "border-image-width",
    "border-inline",
    "border-inline-color",
    "border-inline-end-color",
    "border-inline-end-style",
    "border-inline-end-width",
    "border-inline-start-color",
    "border-inline-start-style",
    "border-inline-start-width",
    "border-inline-style",
    "border-inline-width",
    "border-left-color",
    "border-left-style",
    "border-left-width",
    "border-radius",
    "border-right-color",
    "border-right-style",
    "border-right-width",
    "border-spacing",
    "border-start-end-radius",
    "border-start-start-radius",
    "border-style",
    "border-top-color",
    "border-top-left-radius",
    "border-top-right-radius",
    "border-top-style",
    "border-top-width",
    "border-width",
    "bottom",
    "box-shadow",
    "box-sizing",
    "caption-side",
    "caret-color",
    "clear",
    "clip-path",
    "color",
    "color-scheme",
    "contain",
    "container",
    "container-name",
    "container-type",
    "content",
    "counter-increment",
    "counter-reset",
    "counter-set",
    "cursor",
    "direction",
    "display",
    "empty-cells",
    "filter",
    "flex-basis",
    "flex-direction",
    "flex-flow",
    "flex-grow",
    "flex-shrink",
    "flex-wrap",
    "float",
    "font",
    "font-family",
    "font-feature-settings",
    "font-kerning",
    "font-language-override",
    "font-optical-sizing",
    "font-size",
    "font-size-adjust",
    "font-stretch",
    "font-style",
    "font-synthesis",
    "font-synthesis-position",
    "font-synthesis-small-caps",
    "font-synthesis-style",
    "font-synthesis-weight",
    "font-variant",
    "font-variant-alternates",
    "font-variant-caps",
    "font-variant-east-asian",
    "font-variant-emoji",
    "font-variant-ligatures",
    "font-variant-numeric",
    "font-variant-position",
    "font-variation-settings",
    "font-weight",
    "grid",
    "grid-area",
    "grid-auto-columns",
    "grid-auto-flow",
    "grid-auto-rows",
    "grid-column",
    "grid-column-end",
    "grid-column-start",
    "grid-row",
    "grid-row-end",
    "grid-row-start",
    "grid-template",
    "grid-template-areas",
    "grid-template-columns",
    "grid-template-rows",
    "height",
    "hyphens",
    "image-orientation",
    "image-rendering",
    "image-resolution",
    "inline-size",
    "inset",
    "inset-block",
    "inset-block-end",
    "inset-block-start",
    "inset-inline",
    "inset-inline-end",
    "inset-inline-start",
    "isolation",
    "justify-content",
    "justify-items",
    "justify-self",
    "left",
    "letter-spacing",
    "line-break",
    "line-height",
    "list-style",
    "list-style-image",
    "list-style-position",
    "list-style-type",
    "margin",
    "margin-block",
    "margin-block-end",
    "margin-block-start",
    "margin-bottom",
    "margin-inline",
    "margin-inline-end",
    "margin-inline-start",
    "margin-left",
    "margin-right",
    "margin-top",
    "max-block-size",
    "max-height",
    "max-inline-size",
    "max-width",
    "min-block-size",
    "min-height",
    "min-inline-size",
    "min-width",
    "mix-blend-mode",
    "object-fit",
    "object-position",
    "opacity",
    "outline",
    "outline-color",
    "outline-offset",
    "outline-style",
    "outline-width",
    "overscroll-behavior",
    "overscroll-behavior-x",
    "overscroll-behavior-y",
    "overflow",
    "overflow-x",
    "overflow-y",
    "padding",
    "padding-block",
    "padding-block-end",
    "padding-block-start",
    "padding-bottom",
    "padding-inline",
    "padding-inline-end",
    "padding-inline-start",
    "padding-left",
    "padding-right",
    "padding-top",
    "pointer-events",
    "place-content",
    "place-items",
    "place-self",
    "position",
    "quotes",
    "resize",
    "scroll-behavior",
    "right",
    "tab-size",
    "table-layout",
    "text-align",
    "text-align-all",
    "text-align-last",
    "text-combine-upright",
    "text-decoration",
    "text-decoration-color",
    "text-decoration-line",
    "text-decoration-skip-ink",
    "text-decoration-style",
    "text-decoration-thickness",
    "text-emphasis",
    "text-emphasis-color",
    "text-emphasis-position",
    "text-emphasis-style",
    "text-indent",
    "text-justify",
    "text-orientation",
    "text-overflow",
    "text-rendering",
    "text-size-adjust",
    "text-shadow",
    "text-transform",
    "text-underline-offset",
    "text-underline-position",
    "top",
    "touch-action",
    "scrollbar-color",
    "scrollbar-width",
    "transform",
    "transform-box",
    "transform-style",
    "transform-origin",
    "backface-visibility",
    "unicode-bidi",
    "user-select",
    "vertical-align",
    "visibility",
    "white-space",
    "width",
    "will-change",
    "word-break",
    "word-spacing",
    "writing-mode",
    "z-index",
];

fn is_known_property(property: &str) -> bool {
    KNOWN_PROPERTIES.contains(&property)
}

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

pub(crate) fn parse_transform_list(value: &str) -> Option<Vec<Transform>> {
    let mut input = ParserInput::new(value);
    let mut parser = Parser::new(&mut input);
    let mut transforms = Vec::new();
    let mut saw_none = false;

    loop {
        match parser.next() {
            Ok(Token::Function(function_name)) => {
                let name = function_name.as_ref().to_ascii_lowercase();
                let parsed = parser
                    .parse_nested_block(|p| {
                        match name.as_str() {
                            "translate" => parse_translate(p),
                            "translatex" => parse_translate_x(p),
                            "translatey" => parse_translate_y(p),
                            "translatez" => parse_translate_z(p),
                            "translate3d" => parse_translate_3d(p),
                            "scale" => parse_scale(p),
                            "scalex" => parse_scale_x(p),
                            "scaley" => parse_scale_y(p),
                            "scalez" => parse_scale_z(p),
                            "scale3d" => parse_scale_3d(p),
                            "rotate" => parse_rotate(p),
                            "rotatex" => parse_rotate_x(p),
                            "rotatey" => parse_rotate_y(p),
                            "rotatez" => parse_rotate_z(p),
                            "rotate3d" => parse_rotate_3d(p),
                            "skew" => parse_skew(p),
                            "skewx" => parse_skew_x(p),
                            "skewy" => parse_skew_y(p),
                            "matrix" => parse_matrix(p),
                            "matrix3d" => parse_matrix_3d(p),
                            "perspective" => parse_perspective(p),
                            _ => Err(()),
                        }
                        .map_err(|_| p.new_error::<()>(BasicParseErrorKind::QualifiedRuleInvalid))
                    })
                    .ok()?;
                transforms.push(parsed);
            }
            Ok(Token::Ident(ident)) => {
                if ident.eq_ignore_ascii_case("none") {
                    saw_none = true;
                    transforms.clear();
                } else {
                    return None;
                }
            }
            Ok(Token::WhiteSpace(_)) => continue,
            Err(_) => break,
            _ => return None,
        }
    }

    if saw_none {
        return Some(Vec::new());
    }
    if transforms.is_empty() {
        None
    } else {
        Some(transforms)
    }
}

fn parse_length_component(parser: &mut Parser) -> Result<Length, ()> {
    let token = parser.next().map_err(|_| ())?;
    match token {
        Token::Percentage { unit_value, .. } => Ok(Length::new(*unit_value, LengthUnit::Percent)),
        Token::Dimension { value, ref unit, .. } => {
            let u = unit.to_ascii_lowercase();
            let unit = match u.as_str() {
                "px" => LengthUnit::Px,
                "pt" => LengthUnit::Pt,
                "pc" => LengthUnit::Pc,
                "in" => LengthUnit::In,
                "cm" => LengthUnit::Cm,
                "mm" => LengthUnit::Mm,
                "q" => LengthUnit::Q,
                "em" => LengthUnit::Em,
                "rem" => LengthUnit::Rem,
                "ex" => LengthUnit::Ex,
                "ch" => LengthUnit::Ch,
                "vw" => LengthUnit::Vw,
                "vh" => LengthUnit::Vh,
                "vmin" => LengthUnit::Vmin,
                "vmax" => LengthUnit::Vmax,
                _ => return Err(()),
            };
            Ok(Length::new(*value, unit))
        }
        Token::Number { value, .. } => Ok(Length::px(*value)),
        Token::Function(ref name)
            if name.eq_ignore_ascii_case("calc")
                || name.eq_ignore_ascii_case("min")
                || name.eq_ignore_ascii_case("max")
                || name.eq_ignore_ascii_case("clamp") =>
        {
            let calc = parser.parse_nested_block(parse_calc_sum).map_err(|_| ())?;
            calc_component_to_length(calc).ok_or(())
        }
        _ => Err(()),
    }
}

fn parse_angle_component(parser: &mut Parser) -> Result<f32, ()> {
    let token = parser.next().map_err(|_| ())?;
    match token {
        Token::Dimension { value, ref unit, .. } => {
            let u = unit.to_ascii_lowercase();
            let deg = match u.as_str() {
                "deg" => *value,
                "grad" => *value * (360.0 / 400.0),
                "rad" => value.to_degrees(),
                "turn" => *value * 360.0,
                _ => return Err(()),
            };
            Ok(deg)
        }
        Token::Number { value, .. } if *value == 0.0 => Ok(0.0),
        Token::Function(ref name)
            if name.eq_ignore_ascii_case("calc")
                || name.eq_ignore_ascii_case("min")
                || name.eq_ignore_ascii_case("max")
                || name.eq_ignore_ascii_case("clamp") =>
        {
            let func = name.as_ref().to_ascii_lowercase();
            let component = match func.as_str() {
                "calc" => parser.parse_nested_block(parse_calc_angle_sum).map_err(|_| ())?,
                "min" => parser
                    .parse_nested_block(|block| parse_min_max_angle(block, MathFn::Min))
                    .map_err(|_| ())?,
                "max" => parser
                    .parse_nested_block(|block| parse_min_max_angle(block, MathFn::Max))
                    .map_err(|_| ())?,
                "clamp" => parser.parse_nested_block(parse_clamp_angle).map_err(|_| ())?,
                _ => return Err(()),
            };
            if component.is_angle {
                Ok(component.value)
            } else {
                Err(())
            }
        }
        _ => Err(()),
    }
}

fn parse_translate(parser: &mut Parser) -> Result<Transform, ()> {
    let x = parse_length_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let y = if parser.is_exhausted() {
        Length::px(0.0)
    } else {
        parser.skip_whitespace();
        parse_length_component(parser)?
    };
    Ok(Transform::Translate(x, y))
}

fn parse_translate_x(parser: &mut Parser) -> Result<Transform, ()> {
    let x = parse_length_component(parser)?;
    Ok(Transform::TranslateX(x))
}

fn parse_translate_y(parser: &mut Parser) -> Result<Transform, ()> {
    let y = parse_length_component(parser)?;
    Ok(Transform::TranslateY(y))
}

fn parse_translate_z(parser: &mut Parser) -> Result<Transform, ()> {
    let z = parse_length_component(parser)?;
    Ok(Transform::TranslateZ(z))
}

fn parse_translate_3d(parser: &mut Parser) -> Result<Transform, ()> {
    let x = parse_length_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let y = if parser.is_exhausted() {
        Length::px(0.0)
    } else {
        parser.skip_whitespace();
        parse_length_component(parser)?
    };
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let z = if parser.is_exhausted() {
        Length::px(0.0)
    } else {
        parser.skip_whitespace();
        parse_length_component(parser)?
    };
    Ok(Transform::Translate3d(x, y, z))
}

fn parse_scale(parser: &mut Parser) -> Result<Transform, ()> {
    let sx = parse_number_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let sy = if parser.is_exhausted() {
        sx
    } else {
        parser.skip_whitespace();
        parse_number_component(parser)?
    };
    Ok(Transform::Scale(sx, sy))
}

fn parse_scale_x(parser: &mut Parser) -> Result<Transform, ()> {
    let s = parse_number_component(parser)?;
    Ok(Transform::ScaleX(s))
}

fn parse_scale_y(parser: &mut Parser) -> Result<Transform, ()> {
    let s = parse_number_component(parser)?;
    Ok(Transform::ScaleY(s))
}

fn parse_scale_z(parser: &mut Parser) -> Result<Transform, ()> {
    let s = parse_number_component(parser)?;
    Ok(Transform::ScaleZ(s))
}

fn parse_scale_3d(parser: &mut Parser) -> Result<Transform, ()> {
    let sx = parse_number_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let sy = if parser.is_exhausted() {
        sx
    } else {
        parser.skip_whitespace();
        parse_number_component(parser)?
    };
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let sz = if parser.is_exhausted() {
        sx
    } else {
        parser.skip_whitespace();
        parse_number_component(parser)?
    };
    Ok(Transform::Scale3d(sx, sy, sz))
}

fn parse_rotate(parser: &mut Parser) -> Result<Transform, ()> {
    let deg = parse_angle_component(parser)?;
    Ok(Transform::Rotate(deg))
}

fn parse_rotate_z(parser: &mut Parser) -> Result<Transform, ()> {
    let deg = parse_angle_component(parser)?;
    Ok(Transform::RotateZ(deg))
}

fn parse_rotate_x(parser: &mut Parser) -> Result<Transform, ()> {
    let deg = parse_angle_component(parser)?;
    Ok(Transform::RotateX(deg))
}

fn parse_rotate_y(parser: &mut Parser) -> Result<Transform, ()> {
    let deg = parse_angle_component(parser)?;
    Ok(Transform::RotateY(deg))
}

fn parse_rotate_3d(parser: &mut Parser) -> Result<Transform, ()> {
    let x = parse_number_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let y = parse_number_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let z = parse_number_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let deg = parse_angle_component(parser)?;
    Ok(Transform::Rotate3d(x, y, z, deg))
}

fn parse_skew_x(parser: &mut Parser) -> Result<Transform, ()> {
    let deg = parse_angle_component(parser)?;
    Ok(Transform::SkewX(deg))
}

fn parse_skew_y(parser: &mut Parser) -> Result<Transform, ()> {
    let deg = parse_angle_component(parser)?;
    Ok(Transform::SkewY(deg))
}

fn parse_skew(parser: &mut Parser) -> Result<Transform, ()> {
    let ax = parse_angle_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let ay = if parser.is_exhausted() {
        0.0
    } else {
        parser.skip_whitespace();
        parse_angle_component(parser)?
    };
    Ok(Transform::Skew(ax, ay))
}

fn parse_matrix(parser: &mut Parser) -> Result<Transform, ()> {
    let a = parse_number_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let b = parse_number_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let c = parse_number_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let d = parse_number_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let e = parse_number_component(parser)?;
    parser.skip_whitespace();
    let _ = parser.try_parse(|p| p.expect_comma());
    let f = parse_number_component(parser)?;
    Ok(Transform::Matrix(a, b, c, d, e, f))
}

fn parse_matrix_3d(parser: &mut Parser) -> Result<Transform, ()> {
    let mut values = [0.0f32; 16];
    for i in 0..16 {
        values[i] = parse_number_component(parser)?;
        if i < 15 {
            parser.skip_whitespace();
            let _ = parser.try_parse(|p| p.expect_comma());
        }
    }
    Ok(Transform::Matrix3d(values))
}

fn parse_perspective(parser: &mut Parser) -> Result<Transform, ()> {
    let len = parse_length_component(parser)?;
    // perspective() only accepts positive lengths (no percentages)
    if len.unit.is_percentage() {
        return Err(());
    }
    if len.calc.is_none() && len.value <= 0.0 {
        return Err(());
    }
    Ok(Transform::Perspective(len))
}

fn parse_number_component(parser: &mut Parser) -> Result<f32, ()> {
    let token = parser.next().map_err(|_| ())?;
    match token {
        Token::Number { value, .. } => Ok(*value),
        Token::Dimension { value, .. } => Ok(*value),
        _ => Err(()),
    }
}

fn is_global_keyword_str(value: &str) -> bool {
    value.eq_ignore_ascii_case("inherit")
        || value.eq_ignore_ascii_case("initial")
        || value.eq_ignore_ascii_case("unset")
        || value.eq_ignore_ascii_case("revert")
        || value.eq_ignore_ascii_case("revert-layer")
}

fn parse_font_family_list(value_str: &str) -> Option<Vec<String>> {
    let mut input = ParserInput::new(value_str);
    let mut parser = Parser::new(&mut input);
    let mut families = Vec::new();

    loop {
        parser.skip_whitespace();
        if parser.is_exhausted() {
            break;
        }

        let family = parse_single_font_family(&mut parser)?;
        families.push(family);

        parser.skip_whitespace();
        if parser.is_exhausted() {
            break;
        }

        if parser.expect_comma().is_err() {
            return None;
        }
        // Trailing comma is invalid.
        if parser.is_exhausted() {
            return None;
        }
    }

    if families.is_empty() {
        None
    } else {
        Some(families)
    }
}

fn parse_single_font_family(parser: &mut Parser) -> Option<String> {
    if let Ok(s) = parser.try_parse(|p| p.expect_string().map(|s| s.as_ref().to_string())) {
        return Some(s);
    }

    let first = parser.expect_ident().map(|ident| ident.as_ref().to_string()).ok()?;
    let mut name = first;
    loop {
        match parser.try_parse(|p| p.expect_ident().map(|i| i.as_ref().to_string())) {
            Ok(ident) => {
                name.push(' ');
                name.push_str(&ident);
            }
            Err(_) => break,
        }
    }
    Some(name)
}

/// Parse a CSS property value
pub fn parse_property_value(property: &str, value_str: &str) -> Option<PropertyValue> {
    // Custom properties store their tokens verbatim (post !important stripping handled by caller).
    if property.starts_with("--") {
        return Some(PropertyValue::Custom(value_str.to_string()));
    }

    // When no property name is provided (utility parsing paths) accept generic values.
    if property.is_empty() {
        return parse_simple_value(value_str.trim());
    }

    // Unknown properties are ignored per the CSS error-handling rules.
    if !is_known_property(property) {
        return None;
    }

    let value_str = value_str.trim();
    if value_str.is_empty() {
        return None;
    }

    // Remove trailing !important if present
    let value_str = value_str.trim_end_matches("!important").trim();

    // If the value contains a CSS variable, keep the raw string so it can be resolved later
    // during cascade/computed value resolution. Many author styles use var() with colors,
    // sizes, etc., which would otherwise be rejected here.
    if crate::style::var_resolution::contains_var(value_str) {
        return Some(PropertyValue::Keyword(value_str.to_string()));
    }

    let is_background_longhand = property.starts_with("background-") || property == "background";
    let allow_commas = is_background_longhand || property == "cursor";

    if property == "text-shadow" {
        if let Some(shadows) = parse_text_shadow_list(value_str) {
            return Some(PropertyValue::TextShadow(shadows));
        }
        return None;
    }

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

    // Transform
    if property == "transform" {
        if value_str.eq_ignore_ascii_case("none") {
            return Some(PropertyValue::Transform(Vec::new()));
        }
        if let Some(ts) = parse_transform_list(value_str) {
            return Some(PropertyValue::Transform(ts));
        }
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
                | "touch-action"
                | "list-style"
                | "background"
                | "background-image"
                | "background-repeat"
                | "background-size"
                | "background-attachment"
                | "background-origin"
                | "background-clip"
                | "border"
                | "border-top"
                | "border-right"
                | "border-bottom"
                | "border-left"
                | "scrollbar-color"
                | "margin"
                | "padding"
                | "border-inline"
                | "border-inline-start"
                | "border-inline-end"
                | "border-block"
                | "border-block-start"
                | "border-block-end"
                | "outline"
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
        if is_global_keyword_str(value_str) {
            return Some(PropertyValue::Keyword(value_str.to_string()));
        }

        if let Some(families) = parse_font_family_list(value_str) {
            return Some(PropertyValue::FontFamily(families));
        }
        return None;
    }

    // Fallback: treat remaining tokens as keywords/numbers/percentages.
    parse_simple_value(value_str)
}

fn parse_text_shadow_list(value_str: &str) -> Option<Vec<TextShadow>> {
    if value_str.eq_ignore_ascii_case("none") {
        return Some(Vec::new());
    }

    let layers = split_shadow_layers(value_str)?;
    let mut shadows = Vec::new();
    for layer in layers {
        let mut color: Option<Option<Rgba>> = None;
        let mut lengths = Vec::new();
        for token in layer {
            if color.is_none() {
                if let Some(parsed_color) = parse_shadow_color(&token) {
                    color = Some(parsed_color);
                    continue;
                }
            }

            if let Some(len) = parse_length(&token) {
                lengths.push(len);
                continue;
            }

            return None;
        }

        if lengths.len() < 2 || lengths.len() > 3 {
            return None;
        }

        let blur = if lengths.len() == 3 {
            lengths[2]
        } else {
            Length::px(0.0)
        };
        shadows.push(TextShadow {
            offset_x: lengths[0],
            offset_y: lengths[1],
            blur_radius: blur,
            color: color.flatten(),
        });
    }

    Some(shadows)
}

fn split_shadow_layers(value_str: &str) -> Option<Vec<Vec<String>>> {
    let tokens = tokenize_property_value(value_str, true);
    if tokens.is_empty() {
        return None;
    }

    let mut layers = Vec::new();
    let mut current = Vec::new();
    for token in tokens {
        if token == "," {
            if current.is_empty() {
                return None;
            }
            layers.push(current);
            current = Vec::new();
        } else {
            current.push(token);
        }
    }

    if current.is_empty() {
        return None;
    }
    layers.push(current);
    Some(layers)
}

fn parse_shadow_color(token: &str) -> Option<Option<Rgba>> {
    Color::parse(token).ok().map(|color| match color {
        Color::CurrentColor => None,
        other => Some(other.to_rgba(Rgba::BLACK)),
    })
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
    if lower.starts_with("conic-gradient(") {
        return parse_conic_gradient(&lower, false);
    }
    if lower.starts_with("repeating-conic-gradient(") {
        return parse_conic_gradient(&lower, true);
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
        GradientPositionComponent {
            alignment: align,
            offset: off,
        }
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
                y = component_from_single(&parts[1], AxisKind::Vertical)
                    .or_else(|| component_from_single(&parts[0], AxisKind::Vertical));
            } else if let Some(cy) = component_from_single(&parts[0], AxisKind::Vertical) {
                y = Some(cy);
                x = component_from_single(&parts[1], AxisKind::Horizontal)
                    .or_else(|| component_from_single(&parts[0], AxisKind::Horizontal));
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
                x = Some(component_from_keyword(
                    cx.alignment,
                    match parts[1] {
                        Part::Offset(len) => Some(len),
                        _ => None,
                    },
                ));
                y = component_from_single(&parts[2], AxisKind::Vertical);
            } else if let Some(cy) = component_from_single(&parts[0], AxisKind::Vertical) {
                y = Some(component_from_keyword(
                    cy.alignment,
                    match parts[1] {
                        Part::Offset(len) => Some(len),
                        _ => None,
                    },
                ));
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

fn parse_conic_gradient(value: &str, repeating: bool) -> Option<PropertyValue> {
    let inner = value
        .strip_prefix("conic-gradient(")
        .or_else(|| value.strip_prefix("repeating-conic-gradient("))?
        .strip_suffix(')')?;
    let parts = split_top_level_commas(inner);
    if parts.len() < 2 {
        return None;
    }

    let mut from_angle = 0.0;
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

    let prelude = parts[0].trim();
    let mut first_stop_idx = 0;
    if let Some(_cs) = parse_color_stop(prelude) {
        // No prelude; this is a stop.
    } else {
        first_stop_idx = 1;
        let tokens: Vec<&str> = prelude.split_whitespace().collect();
        let mut idx = 0;
        while idx < tokens.len() {
            match tokens[idx] {
                "from" if idx + 1 < tokens.len() => {
                    if let Some(angle) = parse_stop_angle(tokens[idx + 1]) {
                        from_angle = angle;
                    }
                    idx += 2;
                }
                "at" if idx + 1 < tokens.len() => {
                    let pos_text = tokens[idx + 1..].join(" ");
                    if let Some(pos) = parse_radial_position(&pos_text) {
                        position = pos;
                        idx = tokens.len();
                    } else {
                        idx += 1;
                    }
                }
                _ => idx += 1,
            }
        }
    }

    let mut stops = Vec::new();
    if first_stop_idx == 0 {
        if let Some(cs) = parse_color_stop(parts[0]) {
            stops.push(cs);
        }
    }
    for part in parts.iter().skip(first_stop_idx) {
        if let Some(cs) = parse_color_stop(part) {
            stops.push(cs);
        }
    }
    if stops.len() < 2 {
        return None;
    }

    if repeating {
        Some(PropertyValue::RepeatingConicGradient {
            from_angle,
            position,
            stops,
        })
    } else {
        Some(PropertyValue::ConicGradient {
            from_angle,
            position,
            stops,
        })
    }
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
    } else if let Some(angle) = parse_stop_angle(t) {
        Some((angle.rem_euclid(360.0)) / 360.0)
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

fn parse_stop_angle(token: &str) -> Option<f32> {
    let t = token.trim();
    if t.ends_with("deg") {
        t[..t.len() - 3].trim().parse::<f32>().ok()
    } else if t.ends_with("turn") {
        t[..t.len() - 4].trim().parse::<f32>().ok().map(|v| v * 360.0)
    } else if t.ends_with("rad") {
        t[..t.len() - 3].trim().parse::<f32>().ok().map(|v| v.to_degrees())
    } else if t.ends_with("grad") {
        t[..t.len() - 4].trim().parse::<f32>().ok().map(|v| v * 0.9)
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
    fn parses_touch_action_keyword_list() {
        let parsed = parse_property_value("touch-action", "pan-x pan-y pinch-zoom");
        let PropertyValue::Multiple(list) = parsed.expect("parsed") else {
            panic!("expected Multiple");
        };
        assert_eq!(list.len(), 3);
        assert!(matches!(&list[0], PropertyValue::Keyword(k) if k == "pan-x"));
        assert!(matches!(&list[1], PropertyValue::Keyword(k) if k == "pan-y"));
        assert!(matches!(&list[2], PropertyValue::Keyword(k) if k == "pinch-zoom"));
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
        let PropertyValue::RadialGradient {
            shape,
            size,
            position,
            stops,
        } = parse_property_value("background-image", value).expect("gradient")
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
        let PropertyValue::RadialGradient {
            shape, size, position, ..
        } = parse_property_value("background-image", value).expect("gradient")
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
        let PropertyValue::RepeatingRadialGradient {
            shape,
            size,
            position,
            stops,
        } = parse_property_value("background-image", value).expect("gradient")
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
    fn parses_conic_gradient_with_from_and_at() {
        let value = "conic-gradient(from 90deg at 25% 75%, red 0deg, blue 180deg)";
        let PropertyValue::ConicGradient {
            from_angle,
            position,
            stops,
        } = parse_property_value("background-image", value).expect("gradient")
        else {
            panic!("expected conic gradient");
        };
        assert!((from_angle - 90.0).abs() < 0.01);
        assert_eq!(stops.len(), 2);
        assert_eq!(position.x.offset.unit, LengthUnit::Percent);
        assert!((position.x.offset.value - 25.0).abs() < 0.01);
        assert_eq!(position.y.offset.unit, LengthUnit::Percent);
        assert!((position.y.offset.value - 75.0).abs() < 0.01);
        assert_eq!(stops[0].position, Some(0.0));
        assert!(stops[1].position.unwrap() > 0.49 && stops[1].position.unwrap() < 0.51);
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
    fn handles_calc_with_mixed_units() {
        let mixed = parse_length("calc(100% - 32px)").expect("calc with percent");
        assert_eq!(mixed.unit, LengthUnit::Calc);
        let resolved = mixed
            .resolve_with_context(Some(200.0), 0.0, 0.0, 16.0, 16.0)
            .expect("resolve calc");
        assert!((resolved - 168.0).abs() < 1e-6);

        let viewport_mix = parse_length("calc(100vw + 10px)").expect("calc viewport");
        let resolved_viewport = viewport_mix
            .resolve_with_context(Some(0.0), 1200.0, 800.0, 16.0, 16.0)
            .expect("resolve viewport mix");
        assert!((resolved_viewport - 1210.0).abs() < 1e-3);

        assert!(parse_length("calc(10px * 5px)").is_none());
        assert!(parse_length("calc(10px / 0px)").is_none());
    }

    #[test]
    fn parses_common_3d_transforms() {
        assert!(parse_transform_list("translate3d(10px, 20px, 30px)").is_some());
        assert!(parse_transform_list("translateZ(5px)").is_some());
        assert!(parse_transform_list("scale3d(2, 3, 4)").is_some());
        assert!(parse_transform_list("scaleZ(5)").is_some());
        assert!(parse_transform_list("rotateX(10deg)").is_some());
        assert!(parse_transform_list("rotateY(20deg)").is_some());
        assert!(parse_transform_list("rotateZ(30deg)").is_some());
        assert!(parse_transform_list("rotate3d(0, 0, 1, 45deg)").is_some());
        assert!(parse_transform_list("perspective(500px)").is_some());
        assert!(parse_transform_list("skew(10deg, 5deg)").is_some());

        let transforms = parse_transform_list(
            "translate3d(10px, 20px, 30px) translateZ(5px) scale3d(2, 3, 4) scaleZ(5) rotateX(10deg) rotateY(20deg) rotate3d(0, 0, 1, 45deg) perspective(500px)",
        )
        .expect("parsed transforms");

        assert!(matches!(transforms[0], Transform::Translate3d(x, y, z)
            if x == Length::px(10.0) && y == Length::px(20.0) && z == Length::px(30.0)));
        assert!(matches!(transforms[1], Transform::TranslateZ(z) if z == Length::px(5.0)));
        assert!(matches!(transforms[2], Transform::Scale3d(sx, sy, sz)
            if (sx - 2.0).abs() < 0.001 && (sy - 3.0).abs() < 0.001 && (sz - 4.0).abs() < 0.001));
        assert!(matches!(transforms[3], Transform::ScaleZ(sz) if (sz - 5.0).abs() < 0.001));
        assert!(matches!(transforms[4], Transform::RotateX(deg) if (deg - 10.0).abs() < 0.001));
        assert!(matches!(transforms[5], Transform::RotateY(deg) if (deg - 20.0).abs() < 0.001));
        assert!(matches!(transforms[6], Transform::Rotate3d(x, y, z, deg)
            if (x.abs() < 0.001) && (y.abs() < 0.001) && (z - 1.0).abs() < 0.001 && (deg - 45.0).abs() < 0.001));
        assert!(matches!(transforms[7], Transform::Perspective(len) if len == Length::px(500.0)));
    }

    #[test]
    fn parses_transform_calc_lengths_and_angles() {
        let transforms =
            parse_transform_list("translate(calc(10px + 5%), calc(20% - 4px)) rotate(calc(45deg + 15deg))")
                .expect("parsed transforms with calc");

        assert_eq!(transforms.len(), 2);

        match &transforms[0] {
            Transform::Translate(x, y) => {
                let calc_x = x.calc.as_ref().expect("calc preserved for x");
                assert!(calc_x
                    .terms()
                    .iter()
                    .any(|t| t.unit == LengthUnit::Px && (t.value - 10.0).abs() < 0.01));
                assert!(calc_x
                    .terms()
                    .iter()
                    .any(|t| t.unit == LengthUnit::Percent && (t.value - 5.0).abs() < 0.01));

                let calc_y = y.calc.as_ref().expect("calc preserved for y");
                assert!(calc_y
                    .terms()
                    .iter()
                    .any(|t| t.unit == LengthUnit::Percent && (t.value - 20.0).abs() < 0.01));
                assert!(calc_y
                    .terms()
                    .iter()
                    .any(|t| t.unit == LengthUnit::Px && (t.value + 4.0).abs() < 0.01));
            }
            other => panic!("unexpected transform {other:?}"),
        }

        match &transforms[1] {
            Transform::Rotate(deg) => assert!((*deg - 60.0).abs() < 0.01),
            other => panic!("unexpected transform {other:?}"),
        }
    }

    #[test]
    fn parses_transform_min_max_clamp_angles() {
        let min_rot = parse_transform_list("rotate(min(90deg, 180deg))").expect("min angle");
        match &min_rot[0] {
            Transform::Rotate(deg) => assert!((*deg - 90.0).abs() < 0.01),
            other => panic!("unexpected transform {other:?}"),
        }

        let clamp_rot = parse_transform_list("rotate(clamp(10deg, 20deg, 15deg))").expect("clamp angle");
        match &clamp_rot[0] {
            Transform::Rotate(deg) => assert!((*deg - 15.0).abs() < 0.01),
            other => panic!("unexpected transform {other:?}"),
        }
    }

    #[test]
    fn rejects_invalid_perspective_lengths() {
        assert!(parse_transform_list("perspective(0px)").is_none());
        assert!(parse_transform_list("perspective(-10px)").is_none());
        assert!(parse_transform_list("perspective(50%)").is_none());

        // calc is accepted even if sign can’t be determined at parse time
        assert!(parse_transform_list("perspective(calc(10px + 5px))").is_some());
    }

    #[test]
    fn parses_matrix3d_with_translation() {
        let transforms = parse_transform_list("matrix3d(1,0,0,0, 0,1,0,0, 0,0,1,0, 5,6,0,1)").expect("parsed matrix3d");
        match &transforms[0] {
            Transform::Matrix3d(values) => {
                assert_eq!(values[0], 1.0);
                assert_eq!(values[5], 1.0);
                assert_eq!(values[10], 1.0);
                assert_eq!(values[12], 5.0);
                assert_eq!(values[13], 6.0);
                assert_eq!(values[15], 1.0);
            }
            other => panic!("unexpected transform {other:?}"),
        }
    }

    #[test]
    fn parses_skew_function() {
        let transforms = parse_transform_list("skew(10deg, 20deg)").expect("parsed skew");
        assert!(
            matches!(transforms[0], Transform::Skew(ax, ay) if (ax - 10.0).abs() < 0.01 && (ay - 20.0).abs() < 0.01)
        );

        // Single-argument skew should default the second angle to 0.
        let transforms_single = parse_transform_list("skew(15deg)").expect("parsed skew");
        assert!(matches!(transforms_single[0], Transform::Skew(ax, ay) if (ax - 15.0).abs() < 0.01 && ay.abs() < 0.01));
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
enum CalcComponent {
    Number(f32),
    Length(CalcLength),
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

    result.ok().and_then(calc_component_to_length)
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
    match component {
        CalcComponent::Number(_) => None,
        CalcComponent::Length(calc) => {
            if let Some(term) = calc.single_term() {
                Some(Length::new(term.value, term.unit))
            } else {
                Some(Length::calc(calc))
            }
        }
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
        Token::Number { value, .. } => Ok(CalcComponent::Number(*value)),
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
            Ok(CalcComponent::Length(CalcLength::single(unit, *value)))
        }
        Token::Percentage { unit_value, .. } => Ok(CalcComponent::Length(CalcLength::single(
            LengthUnit::Percent,
            *unit_value * 100.0,
        ))),
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
            match inner {
                CalcComponent::Number(v) => Ok(CalcComponent::Number(-v)),
                CalcComponent::Length(len) => Ok(CalcComponent::Length(len.scale(-1.0))),
            }
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

    if !all_same_dimension(&[min, preferred, max]) {
        return Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Custom(()),
            location: input.current_source_location(),
        });
    }

    match (min, preferred, max) {
        (CalcComponent::Number(a), CalcComponent::Number(b), CalcComponent::Number(c)) => {
            Ok(CalcComponent::Number(b.clamp(a, c)))
        }
        (CalcComponent::Length(a), CalcComponent::Length(b), CalcComponent::Length(c)) => {
            let a_term = a.single_term();
            let b_term = b.single_term();
            let c_term = c.single_term();
            if let (Some(min_term), Some(pref_term), Some(max_term)) = (a_term, b_term, c_term) {
                if min_term.unit == pref_term.unit && pref_term.unit == max_term.unit {
                    let clamped = pref_term.value.clamp(min_term.value, max_term.value);
                    return Ok(CalcComponent::Length(CalcLength::single(pref_term.unit, clamped)));
                }
            }
            Err(cssparser::ParseError {
                kind: cssparser::ParseErrorKind::Custom(()),
                location: input.current_source_location(),
            })
        }
        _ => Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Custom(()),
            location: input.current_source_location(),
        }),
    }
}

fn all_same_dimension(values: &[CalcComponent]) -> bool {
    if values.is_empty() {
        return false;
    }
    let first_is_number = matches!(values[0], CalcComponent::Number(_));
    values
        .iter()
        .all(|v| matches!(v, CalcComponent::Number(_)) == first_is_number)
}

fn reduce_components<'i>(
    values: Vec<CalcComponent>,
    func: MathFn,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
    if !all_same_dimension(&values) {
        return Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Custom(()),
            location: cssparser::SourceLocation { line: 0, column: 0 },
        });
    }

    match values[0] {
        CalcComponent::Number(_) => {
            let init = match func {
                MathFn::Min => f32::INFINITY,
                MathFn::Max => f32::NEG_INFINITY,
            };
            let result = values.into_iter().fold(init, |acc, v| match (func, v) {
                (MathFn::Min, CalcComponent::Number(n)) => acc.min(n),
                (MathFn::Max, CalcComponent::Number(n)) => acc.max(n),
                _ => acc,
            });
            Ok(CalcComponent::Number(result))
        }
        CalcComponent::Length(_) => {
            let mut iter = values.into_iter();
            let first = match iter.next().unwrap() {
                CalcComponent::Length(l) => l,
                _ => unreachable!(),
            };
            // Only support min/max when all lengths reduce to a single shared unit.
            let first_term = first.single_term().ok_or(cssparser::ParseError {
                kind: cssparser::ParseErrorKind::Custom(()),
                location: cssparser::SourceLocation { line: 0, column: 0 },
            })?;
            let mut extremum = first_term.value;
            for comp in iter {
                let CalcComponent::Length(calc) = comp else {
                    continue;
                };
                let Some(term) = calc.single_term() else {
                    return Err(cssparser::ParseError {
                        kind: cssparser::ParseErrorKind::Custom(()),
                        location: cssparser::SourceLocation { line: 0, column: 0 },
                    });
                };
                if term.unit != first_term.unit {
                    return Err(cssparser::ParseError {
                        kind: cssparser::ParseErrorKind::Custom(()),
                        location: cssparser::SourceLocation { line: 0, column: 0 },
                    });
                }
                extremum = match func {
                    MathFn::Min => extremum.min(term.value),
                    MathFn::Max => extremum.max(term.value),
                };
            }
            Ok(CalcComponent::Length(CalcLength::single(first_term.unit, extremum)))
        }
    }
}

#[derive(Clone, Copy)]
struct AngleComponent {
    value: f32,
    is_angle: bool,
}

fn angle_component_from_unit(unit: &str, value: f32) -> Option<AngleComponent> {
    match unit {
        "deg" => Some(AngleComponent { value, is_angle: true }),
        "grad" => Some(AngleComponent {
            value: value * 0.9,
            is_angle: true,
        }),
        "turn" => Some(AngleComponent {
            value: value * 360.0,
            is_angle: true,
        }),
        "rad" => Some(AngleComponent {
            value: value * (180.0 / std::f32::consts::PI),
            is_angle: true,
        }),
        _ => None,
    }
}

fn combine_angle_sum<'i>(
    left: AngleComponent,
    right: AngleComponent,
    sign: f32,
    location: cssparser::SourceLocation,
) -> Result<AngleComponent, cssparser::ParseError<'i, ()>> {
    if left.is_angle && right.is_angle {
        Ok(AngleComponent {
            value: left.value + sign * right.value,
            is_angle: true,
        })
    } else {
        Err(location.new_custom_error(()))
    }
}

fn combine_angle_product<'i>(
    left: AngleComponent,
    right: AngleComponent,
    op: char,
    location: cssparser::SourceLocation,
) -> Result<AngleComponent, cssparser::ParseError<'i, ()>> {
    match (left.is_angle, right.is_angle, op) {
        (true, false, '*') => Ok(AngleComponent {
            value: left.value * right.value,
            is_angle: true,
        }),
        (false, true, '*') => Ok(AngleComponent {
            value: left.value * right.value,
            is_angle: true,
        }),
        (true, false, '/') if right.value != 0.0 => Ok(AngleComponent {
            value: left.value / right.value,
            is_angle: true,
        }),
        (false, false, '*') => Ok(AngleComponent {
            value: left.value * right.value,
            is_angle: false,
        }),
        (false, false, '/') if right.value != 0.0 => Ok(AngleComponent {
            value: left.value / right.value,
            is_angle: false,
        }),
        _ => Err(location.new_custom_error(())),
    }
}

fn parse_calc_angle_sum<'i, 't>(input: &mut Parser<'i, 't>) -> Result<AngleComponent, cssparser::ParseError<'i, ()>> {
    let mut left = parse_calc_angle_product(input)?;
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
        let right = parse_calc_angle_product(input)?;
        let location = input.current_source_location();
        left = combine_angle_sum(left, right, op, location)?;
    }
    Ok(left)
}

fn parse_calc_angle_product<'i, 't>(
    input: &mut Parser<'i, 't>,
) -> Result<AngleComponent, cssparser::ParseError<'i, ()>> {
    let mut left = parse_calc_angle_factor(input)?;
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
        let right = parse_calc_angle_factor(input)?;
        let location = input.current_source_location();
        left = combine_angle_product(left, right, op, location)?;
    }
    Ok(left)
}

fn parse_calc_angle_factor<'i, 't>(
    input: &mut Parser<'i, 't>,
) -> Result<AngleComponent, cssparser::ParseError<'i, ()>> {
    let location = input.current_source_location();
    match input.next()? {
        Token::Number { value, .. } => Ok(AngleComponent {
            value: *value,
            is_angle: false,
        }),
        Token::Dimension { value, ref unit, .. } => {
            angle_component_from_unit(&unit.to_ascii_lowercase(), *value).ok_or(location.new_custom_error(()))
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("calc") => {
            input.parse_nested_block(parse_calc_angle_sum)
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("min") => {
            input.parse_nested_block(|block| parse_min_max_angle(block, MathFn::Min))
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("max") => {
            input.parse_nested_block(|block| parse_min_max_angle(block, MathFn::Max))
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("clamp") => input.parse_nested_block(parse_clamp_angle),
        Token::ParenthesisBlock => input.parse_nested_block(parse_calc_angle_sum),
        Token::Delim('+') => parse_calc_angle_factor(input),
        Token::Delim('-') => {
            let inner = parse_calc_angle_factor(input)?;
            Ok(AngleComponent {
                value: -inner.value,
                is_angle: inner.is_angle,
            })
        }
        _ => Err(location.new_custom_error(())),
    }
}

fn parse_min_max_angle<'i, 't>(
    input: &mut Parser<'i, 't>,
    func: MathFn,
) -> Result<AngleComponent, cssparser::ParseError<'i, ()>> {
    let mut values = Vec::new();
    loop {
        input.skip_whitespace();
        values.push(parse_calc_angle_sum(input)?);
        input.skip_whitespace();
        if input.try_parse(|p| p.expect_comma()).is_err() {
            break;
        }
    }

    if values.len() < 2 || values.iter().any(|v| !v.is_angle) {
        return Err(input.new_custom_error(()));
    }

    let init = match func {
        MathFn::Min => f32::INFINITY,
        MathFn::Max => f32::NEG_INFINITY,
    };
    let value = values.into_iter().fold(init, |acc, v| match func {
        MathFn::Min => acc.min(v.value),
        MathFn::Max => acc.max(v.value),
    });

    Ok(AngleComponent { value, is_angle: true })
}

fn parse_clamp_angle<'i, 't>(input: &mut Parser<'i, 't>) -> Result<AngleComponent, cssparser::ParseError<'i, ()>> {
    input.skip_whitespace();
    let min = parse_calc_angle_sum(input)?;
    input.skip_whitespace();
    input.expect_comma()?;
    input.skip_whitespace();
    let preferred = parse_calc_angle_sum(input)?;
    input.skip_whitespace();
    input.expect_comma()?;
    input.skip_whitespace();
    let max = parse_calc_angle_sum(input)?;

    if !min.is_angle || !preferred.is_angle || !max.is_angle {
        return Err(input.new_custom_error(()));
    }

    let upper = if max.value < min.value { min.value } else { max.value };
    let clamped = preferred.value.max(min.value).min(upper);
    Ok(AngleComponent {
        value: clamped,
        is_angle: true,
    })
}

fn combine_sum<'i>(
    left: CalcComponent,
    right: CalcComponent,
    sign: f32,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
    match (left, right) {
        (CalcComponent::Number(a), CalcComponent::Number(b)) => Ok(CalcComponent::Number(a + b * sign)),
        (CalcComponent::Length(l), CalcComponent::Length(r)) => l
            .add_scaled(&r, sign)
            .map(CalcComponent::Length)
            .ok_or(cssparser::ParseError {
                kind: cssparser::ParseErrorKind::Custom(()),
                location: cssparser::SourceLocation { line: 0, column: 0 },
            }),
        (CalcComponent::Length(l), CalcComponent::Number(n)) if n == 0.0 => Ok(CalcComponent::Length(l)),
        (CalcComponent::Number(n), CalcComponent::Length(l)) if n == 0.0 => Ok(CalcComponent::Length(l.scale(sign))),
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
        '*' => match (left, right) {
            (CalcComponent::Number(a), CalcComponent::Number(b)) => Ok(CalcComponent::Number(a * b)),
            (CalcComponent::Length(l), CalcComponent::Number(n))
            | (CalcComponent::Number(n), CalcComponent::Length(l)) => Ok(CalcComponent::Length(l.scale(n))),
            _ => Err(cssparser::ParseError {
                kind: cssparser::ParseErrorKind::Custom(()),
                location: cssparser::SourceLocation { line: 0, column: 0 },
            }),
        },
        '/' => match (left, right) {
            (_, CalcComponent::Number(0.0)) => Err(cssparser::ParseError {
                kind: cssparser::ParseErrorKind::Custom(()),
                location: cssparser::SourceLocation { line: 0, column: 0 },
            }),
            (CalcComponent::Number(a), CalcComponent::Number(b)) => Ok(CalcComponent::Number(a / b)),
            (CalcComponent::Length(l), CalcComponent::Number(n)) => Ok(CalcComponent::Length(l.scale(1.0 / n))),
            _ => Err(cssparser::ParseError {
                kind: cssparser::ParseErrorKind::Custom(()),
                location: cssparser::SourceLocation { line: 0, column: 0 },
            }),
        },
        _ => Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Custom(()),
            location: cssparser::SourceLocation { line: 0, column: 0 },
        }),
    }
}
