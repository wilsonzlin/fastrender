//! CSS property value parsing
//!
//! Parses individual CSS property values.

use super::types::GradientPosition;
use super::types::GradientPositionComponent;
use super::types::PropertyValue;
use super::types::RadialGradientShape;
use super::types::RadialGradientSize;
use super::types::TextShadow;
use super::types::Transform;
use crate::style::color::Color;
use crate::style::color::Rgba;
use crate::style::display::Display;
use crate::style::float::Clear;
use crate::style::float::Float;
use crate::style::position::Position;
use crate::style::values::CalcLength;
use crate::style::values::Length;
use crate::style::values::LengthUnit;
use crate::style::ComputedStyle;
use cssparser::BasicParseErrorKind;
use cssparser::Parser;
use cssparser::ParserInput;
use cssparser::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclarationContext {
  Style,
  Page,
}

/// Set of property names that the engine understands in standard style declarations.
///
/// Unknown properties are dropped during parsing (per CSS 2.1 §4.2) and treated as unsupported
/// for feature queries.
const KNOWN_STYLE_PROPERTIES: &[&str] = &[
  "accent-color",
  "align-content",
  "align-items",
  "align-self",
  "all",
  "animation-name",
  "animation-range",
  "animation-timeline",
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
  "background-position-inline",
  "background-position-block",
  "background-size-inline",
  "background-size-block",
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
  "break-after",
  "break-before",
  "break-inside",
  "caption-side",
  "caret-color",
  "clear",
  "clip",
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
  "flex",
  "flex-basis",
  "flex-direction",
  "flex-flow",
  "flex-grow",
  "flex-shrink",
  "flex-wrap",
  "order",
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
  "forced-color-adjust",
  "gap",
  "row-gap",
  "column-gap",
  "column-count",
  "column-width",
  "column-rule",
  "column-rule-color",
  "column-rule-style",
  "column-rule-width",
  "column-fill",
  "column-span",
  "columns",
  "grid-gap",
  "grid-row-gap",
  "grid-column-gap",
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
  "mask",
  "mask-image",
  "mask-position",
  "mask-size",
  "mask-repeat",
  "mask-clip",
  "mask-origin",
  "mask-composite",
  "mask-mode",
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
  "offset-anchor",
  "offset-distance",
  "offset-path",
  "offset-rotate",
  "opacity",
  "orphans",
  "outline",
  "outline-color",
  "outline-offset",
  "outline-style",
  "outline-width",
  "overflow-wrap",
  "overscroll-behavior",
  "overscroll-behavior-x",
  "overscroll-behavior-y",
  "scroll-snap-type",
  "scroll-snap-align",
  "scroll-snap-stop",
  "scroll-timeline",
  "view-timeline",
  "scroll-padding",
  "scroll-padding-top",
  "scroll-padding-right",
  "scroll-padding-bottom",
  "scroll-padding-left",
  "scroll-margin",
  "scroll-margin-top",
  "scroll-margin-right",
  "scroll-margin-bottom",
  "scroll-margin-left",
  "scrollbar-gutter",
  "overflow",
  "overflow-x",
  "overflow-y",
  "overflow-anchor",
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
  "perspective",
  "perspective-origin",
  "pointer-events",
  "place-content",
  "place-items",
  "place-self",
  "position",
  "quotes",
  "ruby-align",
  "ruby-merge",
  "ruby-position",
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
  "text-wrap",
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
  "shape-image-threshold",
  "shape-margin",
  "shape-outside",
  "transform",
  "transform-box",
  "transform-style",
  "transform-origin",
  "backface-visibility",
  "page",
  "page-break-after",
  "page-break-before",
  "page-break-inside",
  "unicode-bidi",
  "user-select",
  "vertical-align",
  "visibility",
  "white-space",
  "width",
  "will-change",
  "word-break",
  "word-wrap",
  "word-spacing",
  "widows",
  "writing-mode",
  "z-index",
];

/// Properties accepted in @page declaration blocks.
const KNOWN_PAGE_PROPERTIES: &[&str] = &[
  "bleed",
  "marks",
  "margin",
  "margin-bottom",
  "margin-left",
  "margin-right",
  "margin-top",
  "size",
  "trim",
];

fn is_known_style_property(property: &str) -> bool {
  KNOWN_STYLE_PROPERTIES.contains(&property)
}

fn is_known_page_property(property: &str) -> bool {
  KNOWN_PAGE_PROPERTIES.contains(&property)
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
      '/' if paren == 0 && bracket == 0 && brace == 0 => {
        if !current.trim().is_empty() {
          tokens.push(current.trim().to_string());
        }
        tokens.push("/".to_string());
        current.clear();
      }
      ',' if allow_commas && paren == 0 && bracket == 0 && brace == 0 => {
        if !current.trim().is_empty() {
          tokens.push(current.trim().to_string());
        }
        tokens.push(",".to_string());
        current.clear();
      }
      '/' if paren == 0 && bracket == 0 && brace == 0 => {
        if !current.trim().is_empty() {
          tokens.push(current.trim().to_string());
        }
        tokens.push("/".to_string());
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
            .map_err(|()| p.new_error::<()>(BasicParseErrorKind::QualifiedRuleInvalid))
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
  let component = parse_calc_sum(parser).map_err(|_| ())?;
  calc_component_to_length_allow_number(component).ok_or(())
}

pub(crate) fn parse_angle_component(parser: &mut Parser) -> Result<f32, ()> {
  let component = parse_calc_sum(parser).map_err(|_| ())?;
  calc_component_to_angle(component).ok_or(())
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
  for (i, slot) in values.iter_mut().enumerate() {
    *slot = parse_number_component(parser)?;
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
  let component = parse_calc_sum(parser).map_err(|_| ())?;
  calc_component_to_number(component).ok_or(())
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

  let first = parser
    .expect_ident()
    .map(|ident| ident.as_ref().to_string())
    .ok()?;
  let mut name = first;
  while let Ok(ident) = parser.try_parse(|p| p.expect_ident().map(|i| i.as_ref().to_string())) {
    name.push(' ');
    name.push_str(&ident);
  }
  Some(name)
}

/// Parse a CSS property value in a style declaration context.
pub fn parse_property_value(property: &str, value_str: &str) -> Option<PropertyValue> {
  parse_property_value_in_context(DeclarationContext::Style, property, value_str)
}

/// Parse a CSS property value with knowledge of the declaration context.
pub fn parse_property_value_in_context(
  context: DeclarationContext,
  property: &str,
  value_str: &str,
) -> Option<PropertyValue> {
  // Custom properties store their tokens verbatim (post !important stripping handled by caller).
  if property.starts_with("--") {
    return Some(PropertyValue::Custom(value_str.to_string()));
  }

  // When no property name is provided (utility parsing paths) accept generic values.
  if property.is_empty() {
    return parse_simple_value(value_str.trim());
  }

  // Unknown properties are ignored per the CSS error-handling rules.
  if !property_allowed_in_context(context, property) {
    return None;
  }

  parse_known_property_value(property, value_str)
}

fn property_allowed_in_context(context: DeclarationContext, property: &str) -> bool {
  match context {
    DeclarationContext::Style => is_known_style_property(property),
    DeclarationContext::Page => is_known_page_property(property),
  }
}

fn parse_known_property_value(property: &str, value_str: &str) -> Option<PropertyValue> {
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
  let is_mask_longhand = property.starts_with("mask-") || property == "mask";
  let allow_commas = is_background_longhand || is_mask_longhand || property == "cursor";

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
        _ => Some(PropertyValue::Color(color)),
      };
    }
  }

  if let Some(num) = parse_function_number(value_str) {
    return Some(PropertyValue::Number(num));
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

  // Unitless zero should parse as a number for numeric-only properties (opacity, z-index, etc.).
  // Our tokenize helper produces "0" so ensure we don't fall through to length parsing for those.
  if tokens.len() == 1 && tokens[0] == "0" && matches!(property, "opacity" | "z-index") {
    return Some(PropertyValue::Number(0.0));
  }
  if tokens.len() > 1
    && matches!(
      property,
      "object-position"
        | "border-spacing"
        | "shape-outside"
        | "background-position"
        | "background-position-x"
        | "background-position-y"
        | "transform-origin"
        | "touch-action"
        | "cursor"
        | "list-style"
        | "flex"
        | "background"
        | "background-image"
        | "background-repeat"
        | "background-size"
        | "background-attachment"
        | "background-origin"
        | "background-clip"
        | "background-position-inline"
        | "background-position-block"
        | "background-size-inline"
        | "background-size-block"
        | "border-image"
        | "border-image-source"
        | "border-image-slice"
        | "border-image-width"
        | "border-image-outset"
        | "border-image-repeat"
        | "mask"
        | "mask-image"
        | "mask-position"
        | "mask-size"
        | "mask-repeat"
        | "mask-mode"
        | "mask-origin"
        | "mask-clip"
        | "mask-composite"
        | "border"
        | "border-top"
        | "border-right"
        | "border-bottom"
        | "border-left"
        | "border-radius"
        | "border-top-left-radius"
        | "border-top-right-radius"
        | "border-bottom-left-radius"
        | "border-bottom-right-radius"
        | "image-orientation"
        | "image-resolution"
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
        | "size"
        | "text-combine-upright"
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
          _ => parts.push(PropertyValue::Color(color)),
        }
      } else {
        parts.push(PropertyValue::Keyword(token));
      }
    }
    if !parts.is_empty() {
      return Some(PropertyValue::Multiple(parts));
    }
  }

  // Try to parse as number
  if let Ok(num) = value_str.parse::<f32>() {
    return Some(PropertyValue::Number(num));
  }

  // Try to parse as length
  if let Some(length) = parse_length(value_str) {
    return Some(PropertyValue::Length(length));
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

fn keyword_in_list(value: &PropertyValue, allowed: &[&str]) -> bool {
  if let PropertyValue::Keyword(kw) = value {
    let kw_lower = kw.to_ascii_lowercase();
    allowed.iter().any(|v| kw_lower == *v)
  } else {
    false
  }
}

fn keyword_parse<T>(value: &PropertyValue, parse: impl Fn(&str) -> Option<T>) -> bool {
  if let PropertyValue::Keyword(kw) = value {
    parse(kw.as_str()).is_some()
  } else {
    false
  }
}

fn length_or_auto(value: &PropertyValue) -> bool {
  match value {
    PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("auto") => true,
    _ => extract_length(value).is_some(),
  }
}

fn background_shorthand_is_valid(value: &PropertyValue) -> bool {
  let tokens: Vec<PropertyValue> = match value {
    PropertyValue::Multiple(parts) => parts.clone(),
    other => vec![other.clone()],
  };
  if tokens.is_empty() {
    return false;
  }

  let layers = split_layers(&tokens);
  if layers.is_empty() {
    return false;
  }

  let current_color = ComputedStyle::default().color;
  layers
    .into_iter()
    .all(|layer_tokens| parse_background_shorthand(&layer_tokens, current_color).is_some())
}

pub(crate) fn supports_parsed_declaration_is_valid(
  property: &str,
  raw_value: &str,
  parsed: &PropertyValue,
) -> bool {
  if property.starts_with("--") {
    return true;
  }

  let prop = property.to_ascii_lowercase();
  match prop.as_str() {
    // Color properties
    "color"
    | "background-color"
    | "border-color"
    | "border-top-color"
    | "border-right-color"
    | "border-bottom-color"
    | "border-left-color"
    | "caret-color"
    | "outline-color"
    | "text-decoration-color"
    | "text-emphasis-color"
    | "column-rule-color" => return Color::parse(raw_value).is_ok(),

    // Basic keywords
    "display" => return keyword_parse(parsed, |kw| Display::parse(kw).ok()),
    "position" => return keyword_parse(parsed, |kw| Position::parse(kw).ok()),
    "float" => return keyword_parse(parsed, |kw| Float::parse(kw).ok()),
    "clear" => return keyword_parse(parsed, |kw| Clear::parse(kw).ok()),
    "overflow" | "overflow-x" | "overflow-y" => {
      return keyword_in_list(parsed, &["visible", "hidden", "scroll", "auto", "clip"])
    }
    "text-orientation" => {
      return keyword_in_list(
        parsed,
        &[
          "mixed",
          "upright",
          "sideways",
          "sideways-right",
          "sideways-left",
        ],
      )
    }
    "text-combine-upright" => {
      if let PropertyValue::Keyword(kw) = parsed {
        let lower = kw.to_ascii_lowercase();
        if lower == "none" || lower == "all" || lower == "digits" {
          return true;
        }
        if let Some(rest) = lower.strip_prefix("digits") {
          return rest
            .parse::<u8>()
            .map(|n| (2..=4).contains(&n))
            .unwrap_or(false);
        }
      }
      return false;
    }
    "writing-mode" => {
      return keyword_in_list(
        parsed,
        &[
          "horizontal-tb",
          "vertical-rl",
          "vertical-lr",
          "sideways-rl",
          "sideways-lr",
        ],
      )
    }
    "direction" => return keyword_in_list(parsed, &["ltr", "rtl"]),
    "visibility" => return keyword_in_list(parsed, &["visible", "hidden", "collapse"]),
    "flex-direction" => {
      return keyword_in_list(parsed, &["row", "row-reverse", "column", "column-reverse"])
    }
    "flex-wrap" => return keyword_in_list(parsed, &["nowrap", "wrap", "wrap-reverse"]),
    "align-items" | "align-self" | "align-content" | "justify-items" | "justify-self"
    | "justify-content" => {
      return keyword_in_list(
        parsed,
        &[
          "flex-start",
          "flex-end",
          "center",
          "start",
          "end",
          "self-start",
          "self-end",
          "baseline",
          "stretch",
          "space-between",
          "space-around",
          "space-evenly",
        ],
      )
    }
    "text-align" => {
      return keyword_in_list(
        parsed,
        &[
          "left",
          "right",
          "center",
          "justify",
          "start",
          "end",
          "match-parent",
        ],
      )
    }

    // Numeric types
    "opacity" => return matches!(parsed, PropertyValue::Number(_)),
    "z-index" => {
      return matches!(parsed, PropertyValue::Number(_)) || keyword_in_list(parsed, &["auto"]);
    }

    // Backgrounds and masks
    "background" => return background_shorthand_is_valid(parsed),
    "background-image" => return parse_background_image_list(parsed).is_some(),
    "background-size" => return parse_layer_list(parsed, parse_background_size).is_some(),
    "background-size-inline" => {
      return parse_layer_list(parsed, parse_background_size_component).is_some()
    }
    "background-size-block" => {
      return parse_layer_list(parsed, parse_background_size_component).is_some()
    }
    "background-position" => return parse_layer_list(parsed, parse_background_position).is_some(),
    "background-position-x" | "background-position-y" => {
      return parse_background_position(parsed).is_some()
    }
    "background-repeat" => return parse_layer_list(parsed, parse_background_repeat).is_some(),
    "background-attachment" => {
      return parse_layer_list(parsed, |v| {
        keyword_in_list(v, &["scroll", "fixed", "local"])
      })
      .is_some()
    }
    "background-origin" | "background-clip" => {
      return parse_layer_list(parsed, parse_background_box).is_some()
    }
    "background-blend-mode" => {
      return parse_layer_list(parsed, |v| {
        if let PropertyValue::Keyword(kw) = v {
          parse_mix_blend_mode(kw).is_some()
        } else {
          false
        }
      })
      .is_some()
    }
    "background-color" => return Color::parse(raw_value).is_ok(),
    "mask" => return parse_background_image_list(parsed).is_some(),
    "mask-image" => return parse_background_image_list(parsed).is_some(),
    "mask-position" => return parse_layer_list(parsed, parse_background_position).is_some(),
    "mask-size" => return parse_layer_list(parsed, parse_background_size).is_some(),
    "mask-repeat" => return parse_layer_list(parsed, parse_background_repeat).is_some(),
    "mask-mode" => return parse_layer_list(parsed, parse_mask_mode).is_some(),
    "mask-origin" => return parse_layer_list(parsed, parse_mask_origin).is_some(),
    "mask-clip" => return parse_layer_list(parsed, parse_mask_clip).is_some(),
    "mask-composite" => return parse_layer_list(parsed, parse_mask_composite).is_some(),

    // Border image
    "border-image" => return parse_border_image_shorthand(parsed).is_some(),
    "border-image-source" => return parse_border_image_source(parsed).is_some(),
    "border-image-slice" => return parse_border_image_slice(parsed).is_some(),
    "border-image-width" => return parse_border_image_width(parsed).is_some(),
    "border-image-outset" => return parse_border_image_outset(parsed).is_some(),
    "border-image-repeat" => return parse_border_image_repeat(parsed).is_some(),

    // Box model basics
    "margin"
    | "margin-top"
    | "margin-right"
    | "margin-bottom"
    | "margin-left"
    | "margin-inline"
    | "margin-inline-start"
    | "margin-inline-end"
    | "margin-block"
    | "margin-block-start"
    | "margin-block-end" => return extract_margin_values(parsed).is_some(),
    "padding"
    | "padding-top"
    | "padding-right"
    | "padding-bottom"
    | "padding-left"
    | "padding-inline"
    | "padding-inline-start"
    | "padding-inline-end"
    | "padding-block"
    | "padding-block-start"
    | "padding-block-end" => return extract_box_values(parsed).is_some(),
    "width" | "height" | "min-width" | "min-height" | "max-width" | "max-height"
    | "inline-size" | "block-size" | "min-inline-size" | "min-block-size" | "max-inline-size"
    | "max-block-size" => {
      if let PropertyValue::Keyword(kw) = parsed {
        let lower = kw.to_ascii_lowercase();
        if lower == "max-content" || lower == "-webkit-max-content" || lower == "-moz-max-content" {
          return true;
        }
      }
      return length_or_auto(parsed);
    }
    "top" | "right" | "bottom" | "left" | "inset" | "inset-inline" | "inset-block"
    | "inset-inline-start" | "inset-inline-end" | "inset-block-start" | "inset-block-end" => {
      return extract_margin_values(parsed).is_some()
    }

    // Transform
    "transform" => {
      return matches!(parsed, PropertyValue::Transform(_))
        || keyword_in_list(parsed, &["none"])
        || keyword_parse(parsed, |kw| parse_transform_list(kw).map(|_| ()));
    }

    _ => {}
  }

  match parsed {
    // For properties not explicitly handled above, fall back to accepting parsed values
    // that produced concrete types. Keywords are rejected here because our generic parser
    // often returns Keyword for unknown tokens, which would be too permissive for @supports.
    PropertyValue::Keyword(_) => false,
    _ => true,
  }
}

fn parse_text_shadow_list(value_str: &str) -> Option<Vec<TextShadow>> {
  if value_str.eq_ignore_ascii_case("none") {
    return Some(Vec::new());
  }

  let layers = split_shadow_layers(value_str)?;
  let mut shadows = Vec::new();
  for layer in layers {
    let mut color: Option<Rgba> = None;
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
      color,
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

fn parse_shadow_color(token: &str) -> Option<Rgba> {
  match Color::parse(token).ok()? {
    Color::CurrentColor => None,
    other => Some(other.to_rgba(Rgba::BLACK)),
  }
}

fn parse_simple_value(value_str: &str) -> Option<PropertyValue> {
  if let Some(num) = parse_function_number(value_str) {
    return Some(PropertyValue::Number(num));
  }
  if let Some(length) = parse_length(value_str) {
    return Some(PropertyValue::Length(length));
  }

  if value_str.starts_with("url(") && value_str.ends_with(')') {
    let inner = value_str
      .trim_start_matches("url(")
      .trim_end_matches(')')
      .trim();
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
      _ => Some(PropertyValue::Color(color)),
    };
  }

  Some(PropertyValue::Keyword(value_str.to_string()))
}

fn parse_gradient(value: &str) -> Option<PropertyValue> {
  fn is_single_function_call(value: &str, name: &str) -> bool {
    let trimmed = value.trim();
    let lower = trimmed.to_ascii_lowercase();
    let prefix = format!("{name}(");
    if !lower.starts_with(&prefix) {
      return false;
    }

    let mut depth: usize = 0;
    let mut in_string: Option<char> = None;
    let mut escape = false;

    for (idx, ch) in trimmed.char_indices() {
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
        '(' => depth += 1,
        ')' => {
          depth = depth.saturating_sub(1);
          if depth == 0 {
            return trimmed[idx + 1..].trim().is_empty();
          }
        }
        _ => {}
      }
    }

    false
  }

  let lower = value.trim().to_ascii_lowercase();
  if is_single_function_call(value, "linear-gradient") {
    return parse_linear_gradient(&lower, false);
  }
  if is_single_function_call(value, "radial-gradient") {
    return parse_radial_gradient(&lower, false);
  }
  if is_single_function_call(value, "repeating-linear-gradient") {
    return parse_linear_gradient(&lower, true);
  }
  if is_single_function_call(value, "repeating-radial-gradient") {
    return parse_radial_gradient(&lower, true);
  }
  if is_single_function_call(value, "conic-gradient") {
    return parse_conic_gradient(&lower, false);
  }
  if is_single_function_call(value, "repeating-conic-gradient") {
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
    if let Some(cs) = parse_color_stops(stop) {
      stops.extend(cs);
    }
  }
  for part in iter {
    if let Some(cs) = parse_color_stops(part) {
      stops.extend(cs);
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
  if let Some(cs) = parse_color_stops(parts[0]) {
    stops.extend(cs);
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

    let tokens: Vec<&str> = prelude
      .split_whitespace()
      .filter(|t| !t.is_empty())
      .collect();
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

    if let Some((&first, rest)) = explicit_sizes.split_first() {
      let second = rest.first().copied();
      size = RadialGradientSize::Explicit {
        x: first,
        y: second,
      };
    }
  }

  for part in parts.iter().skip(start_idx) {
    if let Some(cs) = parse_color_stops(part) {
      stops.extend(cs);
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
    PropertyValue::Number(0.0) => Some(Length::px(0.0)),
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
      (Part::Keyword(kind, align), _) if *kind == AxisKind::Either => {
        Some(component_from_keyword(*align, None))
      }
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
            Part::Keyword(_, _) => None,
          },
        ));
        y = component_from_single(&parts[2], AxisKind::Vertical);
      } else if let Some(cy) = component_from_single(&parts[0], AxisKind::Vertical) {
        y = Some(component_from_keyword(
          cy.alignment,
          match parts[1] {
            Part::Offset(len) => Some(len),
            Part::Keyword(_, _) => None,
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
  if parse_color_stops(prelude).is_some() {
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
  for part in parts.iter().skip(first_stop_idx) {
    if let Some(cs) = parse_color_stops(part) {
      stops.extend(cs);
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

fn parse_angle_expression(token: &str) -> Option<f32> {
  let mut input = ParserInput::new(token);
  let mut parser = Parser::new(&mut input);
  let component = parser.parse_entirely(parse_calc_sum).ok()?;
  calc_component_to_angle(component)
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

  if let Some(angle) = parse_angle_expression(token) {
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

fn parse_color_stops(token: &str) -> Option<Vec<crate::css::types::ColorStop>> {
  let trimmed = token.trim();
  if trimmed.is_empty() {
    return None;
  }

  let (color_part, position_parts) = split_color_and_positions(trimmed);
  let color = Color::parse(color_part).ok()?;

  match position_parts.len() {
    0 => Some(vec![crate::css::types::ColorStop {
      color,
      position: None,
    }]),
    1 => Some(vec![crate::css::types::ColorStop {
      color,
      position: parse_stop_position(position_parts[0]),
    }]),
    2 => {
      let first = parse_stop_position(position_parts[0])?;
      let second = parse_stop_position(position_parts[1])?;
      Some(vec![
        crate::css::types::ColorStop {
          color: color.clone(),
          position: Some(first),
        },
        crate::css::types::ColorStop {
          color,
          position: Some(second),
        },
      ])
    }
    _ => None,
  }
}

fn split_color_and_positions(token: &str) -> (&str, Vec<&str>) {
  fn split_last_segment(input: &str) -> Option<(&str, &str)> {
    let mut depth = 0i32;
    for (idx, ch) in input.char_indices().rev() {
      match ch {
        ')' => depth += 1,
        '(' => depth -= 1,
        ' ' | '\t' if depth == 0 => {
          let head = input[..idx].trim_end();
          let tail = input[idx..].trim();
          if head.is_empty() || tail.is_empty() {
            return None;
          }
          return Some((head, tail));
        }
        _ => {}
      }
    }
    None
  }

  let mut remainder = token;
  let mut positions = Vec::new();
  for _ in 0..2 {
    let Some((head, tail)) = split_last_segment(remainder) else {
      break;
    };
    if parse_stop_position(tail).is_some() {
      positions.push(tail);
      remainder = head;
    } else {
      break;
    }
  }

  positions.reverse();
  (remainder, positions)
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
  } else if let Ok(num) = t.parse::<f32>() {
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
    t[..t.len() - 4]
      .trim()
      .parse::<f32>()
      .ok()
      .map(|v| v * 360.0)
  } else if t.ends_with("rad") {
    t[..t.len() - 3]
      .trim()
      .parse::<f32>()
      .ok()
      .map(|v| v.to_degrees())
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
  use crate::style::properties::supported_properties;
  use std::collections::BTreeSet;

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
  fn cursor_tokenization_preserves_comma_separated_image_list() {
    let parsed = parse_property_value("cursor", "url(a.cur) 1 2, pointer");
    let PropertyValue::Multiple(list) = parsed.expect("parsed") else {
      panic!("expected Multiple");
    };
    assert_eq!(list.len(), 5);
    assert!(matches!(&list[0], PropertyValue::Url(url) if url == "a.cur"));
    assert!(matches!(&list[1], PropertyValue::Number(n) if (*n - 1.0).abs() < 1e-6));
    assert!(matches!(&list[2], PropertyValue::Number(n) if (*n - 2.0).abs() < 1e-6));
    assert!(matches!(&list[3], PropertyValue::Keyword(k) if k == ","));
    assert!(matches!(&list[4], PropertyValue::Keyword(k) if k.eq_ignore_ascii_case("pointer")));
  }

  #[test]
  fn border_image_slice_keeps_multiple_tokens() {
    let parsed = parse_property_value("border-image-slice", "30 30 fill");
    let PropertyValue::Multiple(list) = parsed.expect("parsed") else {
      panic!("expected Multiple");
    };
    assert_eq!(list.len(), 3);
    assert!(matches!(&list[0], PropertyValue::Number(n) if (*n - 30.0).abs() < 1e-6));
    assert!(matches!(&list[1], PropertyValue::Number(n) if (*n - 30.0).abs() < 1e-6));
    assert!(matches!(&list[2], PropertyValue::Keyword(k) if k.eq_ignore_ascii_case("fill")));
  }

  #[test]
  fn tokenizes_image_orientation_angle_and_flip() {
    let parsed = parse_property_value("image-orientation", "90deg flip");
    let PropertyValue::Multiple(list) = parsed.expect("parsed") else {
      panic!("expected Multiple");
    };
    assert_eq!(list.len(), 2);
    assert!(matches!(list[0], PropertyValue::Keyword(ref k) if k == "90deg"));
    assert!(matches!(list[1], PropertyValue::Keyword(ref k) if k == "flip"));
  }

  #[test]
  fn tokenizes_image_resolution_multi_keyword() {
    let parsed = parse_property_value("image-resolution", "from-image snap");
    let PropertyValue::Multiple(list) = parsed.expect("parsed") else {
      panic!("expected Multiple");
    };
    assert_eq!(list.len(), 2);
    assert!(matches!(list[0], PropertyValue::Keyword(ref k) if k == "from-image"));
    assert!(matches!(list[1], PropertyValue::Keyword(ref k) if k == "snap"));
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
  fn parses_two_position_gradient_color_stop() {
    let value = "linear-gradient(to right, black 0 50%, transparent 50%)";
    let PropertyValue::LinearGradient { stops, .. } =
      parse_property_value("background-image", value).expect("gradient")
    else {
      panic!("expected linear gradient");
    };

    assert_eq!(stops.len(), 3);
    assert_eq!(stops[0].position, Some(0.0));
    assert_eq!(stops[1].position, Some(0.5));
    assert_eq!(stops[2].position, Some(0.5));
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
  fn parse_property_value_respects_known_properties_list() {
    // Unknown properties should be rejected outright.
    assert!(parse_property_value("not-a-property", "5").is_none());
    // An empty property name is used for generic parsing helpers; it should still tokenize values.
    assert!(matches!(
        parse_property_value("", "5"),
        Some(PropertyValue::Number(n)) if (n - 5.0).abs() < 1e-6
    ));
  }

  #[test]
  fn known_properties_are_unique() {
    for (name, props) in [
      ("KNOWN_STYLE_PROPERTIES", KNOWN_STYLE_PROPERTIES),
      ("KNOWN_PAGE_PROPERTIES", KNOWN_PAGE_PROPERTIES),
    ] {
      let mut list = props.to_vec();
      let original_len = list.len();
      list.sort_unstable();
      list.dedup();
      assert_eq!(list.len(), original_len, "{} contains duplicates", name);
    }
  }

  #[test]
  fn page_properties_require_page_context() {
    assert!(parse_property_value("size", "A4").is_none());
    assert!(parse_property_value_in_context(DeclarationContext::Page, "size", "A4").is_some());
  }

  #[test]
  fn known_properties_match_applied_properties() {
    let supported: BTreeSet<_> = supported_properties().iter().copied().collect();
    let known: BTreeSet<_> = KNOWN_STYLE_PROPERTIES
      .iter()
      .chain(KNOWN_PAGE_PROPERTIES.iter())
      .copied()
      .collect();

    let missing: Vec<_> = supported.difference(&known).copied().collect();
    let extra: Vec<_> = known.difference(&supported).copied().collect();

    assert!(
      missing.is_empty() && extra.is_empty(),
      "KNOWN_PROPERTIES out of sync with apply_declaration_with_base.\nmissing: {:?}\nextra: {:?}",
      missing,
      extra
    );
  }

  #[test]
  fn parses_radial_gradient_with_size_and_position() {
    let value = "radial-gradient(circle closest-side at 25% 75%, red, blue)";
    let PropertyValue::RadialGradient {
      shape,
      size,
      position,
      ..
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
      _ => panic!("unexpected color variant"),
    }
    assert_eq!(stops[1].position, Some(0.90));
  }

  #[test]
  fn gradients_accept_relative_and_contrast_colors() {
    let value =
      "linear-gradient(to right, color(from red srgb r g b / 50%) 0%, color-contrast(blue vs white, black) 100%)";
    let PropertyValue::LinearGradient { stops, .. } =
      parse_property_value("background-image", value).expect("gradient")
    else {
      panic!("expected linear gradient");
    };

    assert_eq!(stops.len(), 2);
    let first = stops[0].color.to_rgba(Rgba::BLACK);
    assert!((first.a - 0.5).abs() < 1e-6);
    let second = stops[1].color.to_rgba(Rgba::BLACK);
    assert_eq!(second, Rgba::WHITE);
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
    assert!(
      matches!(parts[3], PropertyValue::Keyword(ref k) if k.eq_ignore_ascii_case("no-repeat"))
    );
  }

  #[test]
  fn tokenizes_background_slashes_without_whitespace() {
    let parsed =
      parse_property_value("background", "url(a) center/cover no-repeat").expect("background");
    let PropertyValue::Multiple(parts) = parsed else {
      panic!("expected layered background tokens");
    };
    assert_eq!(parts.len(), 5);
    assert!(matches!(parts[0], PropertyValue::Url(ref u) if u == "a"));
    assert!(matches!(parts[1], PropertyValue::Keyword(ref k) if k == "center"));
    assert!(matches!(parts[2], PropertyValue::Keyword(ref k) if k == "/"));
    assert!(matches!(parts[3], PropertyValue::Keyword(ref k) if k == "cover"));
    assert!(
      matches!(parts[4], PropertyValue::Keyword(ref k) if k.eq_ignore_ascii_case("no-repeat"))
    );
  }

  #[test]
  fn tokenizes_border_image_slash_segments() {
    let parsed =
      parse_property_value("border-image", "url(a) 30/10px/0 stretch").expect("border-image");
    let PropertyValue::Multiple(parts) = parsed else {
      panic!("expected multiple tokens");
    };
    assert_eq!(parts.len(), 7);
    assert!(matches!(parts[0], PropertyValue::Url(ref u) if u == "a"));
    assert!(matches!(parts[1], PropertyValue::Number(n) if (n - 30.0).abs() < 1e-6));
    assert!(matches!(parts[2], PropertyValue::Keyword(ref k) if k == "/"));
    assert!(
      matches!(parts[3], PropertyValue::Length(len) if (len.value - 10.0).abs() < 0.01 && len.unit == LengthUnit::Px)
    );
    assert!(matches!(parts[4], PropertyValue::Keyword(ref k) if k == "/"));
    assert!(
      matches!(parts[5], PropertyValue::Length(len) if len.value == 0.0 && len.unit == LengthUnit::Px)
    );
    assert!(matches!(parts[6], PropertyValue::Keyword(ref k) if k == "stretch"));
  }

  #[test]
  fn does_not_split_slash_inside_functions() {
    let tokens = tokenize_property_value("url(data:image/svg+xml;base64,abc/def==)", true);
    assert_eq!(
      tokens,
      vec!["url(data:image/svg+xml;base64,abc/def==)".to_string()]
    );
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
      ("10dvw", 10.0, LengthUnit::Dvw),
      ("12dvh", 12.0, LengthUnit::Dvh),
      ("8dvmin", 8.0, LengthUnit::Dvmin),
      ("9dvmax", 9.0, LengthUnit::Dvmax),
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

    let zero = parse_length("calc(0)").expect("calc zero");
    assert_eq!(zero, Length::px(0.0));

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
    let transforms = parse_transform_list(
      "translate(calc(10px + 5%), calc(20% - 4px)) rotate(calc(45deg + 15deg))",
    )
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

    let clamp_rot =
      parse_transform_list("rotate(clamp(10deg, 20deg, 15deg))").expect("clamp angle");
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
    let transforms = parse_transform_list("matrix3d(1,0,0,0, 0,1,0,0, 0,0,1,0, 5,6,0,1)")
      .expect("parsed matrix3d");
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
    assert!(
      matches!(transforms_single[0], Transform::Skew(ax, ay) if (ax - 15.0).abs() < 0.01 && ay.abs() < 0.01)
    );
  }

  #[test]
  fn parses_min_max_clamp_lengths() {
    assert_eq!(parse_length("min(10px, 20px)").unwrap(), Length::px(10.0));
    assert_eq!(parse_length("max(10px, 20px)").unwrap(), Length::px(20.0));
    assert_eq!(
      parse_length("clamp(10px, 5px, 15px)").unwrap(),
      Length::px(10.0)
    );
    assert_eq!(
      parse_length("clamp(10px, 12px, 15px)").unwrap(),
      Length::px(12.0)
    );
    assert_eq!(
      parse_length("clamp(10px, 30px, 15px)").unwrap(),
      Length::px(15.0)
    );
    assert_eq!(
      parse_length("min(10%, 20%)").unwrap(),
      Length::percent(10.0)
    );
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
  const MATH_PREFIXES: &[&str] = &[
    "calc(", "min(", "max(", "clamp(", "sin(", "cos(", "tan(", "asin(", "acos(", "atan(", "atan2(",
    "pow(", "sqrt(", "hypot(", "log(", "exp(", "sign(", "abs(", "round(", "mod(", "rem(",
    "clamped(",
  ];
  if MATH_PREFIXES.iter().any(|p| lower.starts_with(p)) {
    if let Some(len) = parse_function_length(s) {
      return Some(len);
    }
    if lower == "calc(0)" {
      return Some(Length::px(0.0));
    }
  }

  for (suffix, unit) in [
    ("dvmin", LengthUnit::Dvmin),
    ("dvmax", LengthUnit::Dvmax),
    ("dvw", LengthUnit::Dvw),
    ("dvh", LengthUnit::Dvh),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CalcDimension {
  Number,
  Length,
  Angle,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum CalcComponent {
  Number(f32),
  Length(CalcLength),
  /// Stored in degrees
  Angle(f32),
}

impl CalcComponent {
  fn dimension(&self) -> CalcDimension {
    match self {
      Self::Number(_) => CalcDimension::Number,
      Self::Length(_) => CalcDimension::Length,
      Self::Angle(_) => CalcDimension::Angle,
    }
  }
}

fn angle_from_unit(unit: &str, value: f32) -> Option<f32> {
  match unit {
    "deg" => Some(value),
    "grad" => Some(value * 0.9),
    "turn" => Some(value * 360.0),
    "rad" => Some(value.to_degrees()),
    _ => None,
  }
}

fn parse_function_length(input: &str) -> Option<Length> {
  let mut parser_input = ParserInput::new(input);
  let mut parser = Parser::new(&mut parser_input);
  let result = parser.parse_entirely(parse_calc_sum);

  result.ok().and_then(calc_component_to_length)
}

fn parse_function_number(input: &str) -> Option<f32> {
  let mut parser_input = ParserInput::new(input);
  let mut parser = Parser::new(&mut parser_input);
  let result = parser.parse_entirely(parse_calc_sum);

  result.ok().and_then(calc_component_to_number)
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
  calc_component_to_length_with_numbers(component, false)
}

fn calc_component_to_length_allow_number(component: CalcComponent) -> Option<Length> {
  calc_component_to_length_with_numbers(component, true)
}

fn calc_component_to_length_with_numbers(
  component: CalcComponent,
  allow_unitless: bool,
) -> Option<Length> {
  match component {
    CalcComponent::Number(n) => {
      if n == 0.0 || allow_unitless {
        Some(Length::px(n))
      } else {
        None
      }
    }
    CalcComponent::Length(calc) => {
      if calc.is_zero() {
        Some(Length::px(0.0))
      } else if let Some(term) = calc.single_term() {
        Some(Length::new(term.value, term.unit))
      } else {
        Some(Length::calc(calc))
      }
    }
    _ => None,
  }
}

fn calc_component_to_number(component: CalcComponent) -> Option<f32> {
  match component {
    CalcComponent::Number(n) => Some(n),
    _ => None,
  }
}

fn calc_component_to_angle(component: CalcComponent) -> Option<f32> {
  match component {
    CalcComponent::Angle(deg) => Some(deg),
    CalcComponent::Number(n) if n == 0.0 => Some(0.0),
    _ => None,
  }
}

fn all_same_dimension(values: &[CalcComponent]) -> bool {
  values
    .first()
    .map(|first| values.iter().all(|v| v.dimension() == first.dimension()))
    .unwrap_or(false)
}

fn extract_simple_length(len: &CalcLength) -> Option<(f32, LengthUnit)> {
  if len.is_zero() {
    return Some((0.0, LengthUnit::Px));
  }
  len.single_term().map(|t| (t.value, t.unit))
}

fn round_value<'i>(
  value: f32,
  step: f32,
  strategy: RoundStrategy,
  location: cssparser::SourceLocation,
) -> Result<f32, cssparser::ParseError<'i, ()>> {
  if step == 0.0 || !step.is_finite() || !value.is_finite() {
    return Err(location.new_custom_error(()));
  }
  let step = step.abs();
  let scaled = value / step;
  let rounded = match strategy {
    RoundStrategy::Nearest => scaled.round(),
    RoundStrategy::Up => scaled.ceil(),
    RoundStrategy::Down => scaled.floor(),
    RoundStrategy::ToZero => scaled.trunc(),
    RoundStrategy::AwayFromZero => {
      if scaled.is_sign_positive() {
        scaled.ceil()
      } else {
        scaled.floor()
      }
    }
  };
  let result = rounded * step;
  if result.is_finite() {
    Ok(result)
  } else {
    Err(location.new_custom_error(()))
  }
}

fn ensure_finite<'i>(
  value: f32,
  location: cssparser::SourceLocation,
) -> Result<f32, cssparser::ParseError<'i, ()>> {
  if value.is_finite() {
    Ok(value)
  } else {
    Err(location.new_custom_error(()))
  }
}

fn parse_calc_sum<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
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
    let location = input.current_source_location();
    left = combine_sum(left, right, op, location)?;
  }
  Ok(left)
}

fn parse_calc_product<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
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
    let location = input.current_source_location();
    left = combine_product(left, right, op, location)?;
  }
  Ok(left)
}

fn parse_calc_factor<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
  let location = input.current_source_location();
  let token = input.next()?;
  match token {
    Token::Number { value, .. } => Ok(CalcComponent::Number(*value)),
    Token::Dimension {
      value, ref unit, ..
    } => {
      let unit = unit.as_ref().to_ascii_lowercase();
      if let Some(angle) = angle_from_unit(&unit, *value) {
        return Ok(CalcComponent::Angle(angle));
      }
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
        "dvw" => LengthUnit::Dvw,
        "dvh" => LengthUnit::Dvh,
        "dvmin" => LengthUnit::Dvmin,
        "dvmax" => LengthUnit::Dvmax,
        _ => return Err(location.new_custom_error(())),
      };
      Ok(CalcComponent::Length(CalcLength::single(unit, *value)))
    }
    Token::Percentage { unit_value, .. } => Ok(CalcComponent::Length(CalcLength::single(
      LengthUnit::Percent,
      *unit_value * 100.0,
    ))),
    Token::Function(ref name) => {
      let func = name.as_ref().to_ascii_lowercase();
      parse_math_function(&func, input, location)
    }
    Token::ParenthesisBlock => input.parse_nested_block(parse_calc_sum),
    Token::Delim('+') => parse_calc_factor(input),
    Token::Delim('-') => {
      let inner = parse_calc_factor(input)?;
      match inner {
        CalcComponent::Number(v) => Ok(CalcComponent::Number(-v)),
        CalcComponent::Length(len) => Ok(CalcComponent::Length(len.scale(-1.0))),
        CalcComponent::Angle(v) => Ok(CalcComponent::Angle(-v)),
      }
    }
    _ => Err(location.new_custom_error(())),
  }
}

#[derive(Clone, Copy)]
pub(crate) enum MathFn {
  Min,
  Max,
}

#[derive(Clone, Copy)]
enum RoundStrategy {
  Nearest,
  Up,
  Down,
  ToZero,
  AwayFromZero,
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
    return Err(input.new_custom_error(()));
  }

  reduce_components(values, func, input.current_source_location())
}

fn parse_clamp<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
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
    return Err(input.new_custom_error(()));
  }

  let location = input.current_source_location();
  match (min, preferred, max) {
    (CalcComponent::Number(a), CalcComponent::Number(b), CalcComponent::Number(c)) => {
      Ok(CalcComponent::Number(b.clamp(a, c)))
    }
    (CalcComponent::Angle(a), CalcComponent::Angle(b), CalcComponent::Angle(c)) => {
      let upper = if c < a { a } else { c };
      Ok(CalcComponent::Angle(b.max(a).min(upper)))
    }
    (CalcComponent::Length(a), CalcComponent::Length(b), CalcComponent::Length(c)) => {
      let Some((min_value, unit)) = extract_simple_length(&a) else {
        return Err(location.new_custom_error(()));
      };
      let Some((pref_value, pref_unit)) = extract_simple_length(&b) else {
        return Err(location.new_custom_error(()));
      };
      let Some((max_value, max_unit)) = extract_simple_length(&c) else {
        return Err(location.new_custom_error(()));
      };
      if unit != pref_unit || unit != max_unit {
        return Err(location.new_custom_error(()));
      }
      let upper = if max_value < min_value {
        min_value
      } else {
        max_value
      };
      let clamped = pref_value.max(min_value).min(upper);
      Ok(CalcComponent::Length(CalcLength::single(unit, clamped)))
    }
    _ => Err(location.new_custom_error(())),
  }
}
fn reduce_components<'i>(
  values: Vec<CalcComponent>,
  func: MathFn,
  location: cssparser::SourceLocation,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
  if !all_same_dimension(&values) {
    return Err(location.new_custom_error(()));
  }

  match values[0].dimension() {
    CalcDimension::Number => {
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
    CalcDimension::Angle => {
      let init = match func {
        MathFn::Min => f32::INFINITY,
        MathFn::Max => f32::NEG_INFINITY,
      };
      let result = values.into_iter().fold(init, |acc, v| match (func, v) {
        (MathFn::Min, CalcComponent::Angle(n)) => acc.min(n),
        (MathFn::Max, CalcComponent::Angle(n)) => acc.max(n),
        _ => acc,
      });
      Ok(CalcComponent::Angle(result))
    }
    CalcDimension::Length => {
      let mut iter = values.into_iter();
      let first = match iter.next().unwrap() {
        CalcComponent::Length(l) => l,
        _ => return Err(location.new_custom_error(())),
      };
      let Some((mut extremum, unit)) = extract_simple_length(&first) else {
        return Err(location.new_custom_error(()));
      };
      for comp in iter {
        let CalcComponent::Length(calc) = comp else {
          continue;
        };
        let Some((value, this_unit)) = extract_simple_length(&calc) else {
          return Err(location.new_custom_error(()));
        };
        if this_unit != unit {
          return Err(location.new_custom_error(()));
        }
        extremum = match func {
          MathFn::Min => extremum.min(value),
          MathFn::Max => extremum.max(value),
        };
      }
      Ok(CalcComponent::Length(CalcLength::single(unit, extremum)))
    }
  }
}

fn combine_sum<'i>(
  left: CalcComponent,
  right: CalcComponent,
  sign: f32,
  location: cssparser::SourceLocation,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
  match (left, right) {
    (CalcComponent::Number(a), CalcComponent::Number(b)) => Ok(CalcComponent::Number(a + b * sign)),
    (CalcComponent::Angle(a), CalcComponent::Angle(b)) => Ok(CalcComponent::Angle(a + b * sign)),
    (CalcComponent::Length(l), CalcComponent::Length(r)) => l
      .add_scaled(&r, sign)
      .map(CalcComponent::Length)
      .ok_or_else(|| location.new_custom_error(())),
    (CalcComponent::Length(l), CalcComponent::Number(0.0)) => Ok(CalcComponent::Length(l)),
    (CalcComponent::Number(0.0), CalcComponent::Length(l)) => {
      Ok(CalcComponent::Length(l.scale(sign)))
    }
    (CalcComponent::Angle(a), CalcComponent::Number(0.0)) => Ok(CalcComponent::Angle(a)),
    (CalcComponent::Number(0.0), CalcComponent::Angle(a)) => Ok(CalcComponent::Angle(a * sign)),
    _ => Err(location.new_custom_error(())),
  }
}

fn combine_product<'i>(
  left: CalcComponent,
  right: CalcComponent,
  op: char,
  location: cssparser::SourceLocation,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
  match op {
    '*' => match (left, right) {
      (CalcComponent::Number(a), CalcComponent::Number(b)) => Ok(CalcComponent::Number(a * b)),
      (CalcComponent::Length(l), CalcComponent::Number(n))
      | (CalcComponent::Number(n), CalcComponent::Length(l)) => {
        Ok(CalcComponent::Length(l.scale(n)))
      }
      (CalcComponent::Angle(a), CalcComponent::Number(n))
      | (CalcComponent::Number(n), CalcComponent::Angle(a)) => Ok(CalcComponent::Angle(a * n)),
      _ => Err(location.new_custom_error(())),
    },
    '/' => match (left, right) {
      (_, CalcComponent::Number(0.0)) => Err(location.new_custom_error(())),
      (CalcComponent::Number(a), CalcComponent::Number(b)) => Ok(CalcComponent::Number(a / b)),
      (CalcComponent::Length(l), CalcComponent::Number(n)) => {
        Ok(CalcComponent::Length(l.scale(1.0 / n)))
      }
      (CalcComponent::Angle(a), CalcComponent::Number(n)) => Ok(CalcComponent::Angle(a / n)),
      _ => Err(location.new_custom_error(())),
    },
    _ => Err(location.new_custom_error(())),
  }
}

fn expect_number<'i>(
  component: CalcComponent,
  location: cssparser::SourceLocation,
) -> Result<f32, cssparser::ParseError<'i, ()>> {
  calc_component_to_number(component).ok_or_else(|| location.new_custom_error(()))
}

fn expect_angle<'i>(
  component: CalcComponent,
  location: cssparser::SourceLocation,
) -> Result<f32, cssparser::ParseError<'i, ()>> {
  calc_component_to_angle(component).ok_or_else(|| location.new_custom_error(()))
}

fn expect_simple_length<'i>(
  component: CalcComponent,
  location: cssparser::SourceLocation,
) -> Result<(f32, LengthUnit), cssparser::ParseError<'i, ()>> {
  match component {
    CalcComponent::Length(len) => {
      extract_simple_length(&len).ok_or_else(|| location.new_custom_error(()))
    }
    _ => Err(location.new_custom_error(())),
  }
}

fn apply_round_function<'i>(
  value: CalcComponent,
  step: Option<CalcComponent>,
  strategy: RoundStrategy,
  location: cssparser::SourceLocation,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
  match value {
    CalcComponent::Number(v) => {
      let step = match step {
        Some(CalcComponent::Number(s)) => s,
        None => 1.0,
        _ => return Err(location.new_custom_error(())),
      };
      round_value(v, step, strategy, location).map(CalcComponent::Number)
    }
    CalcComponent::Angle(deg) => {
      let step = match step {
        Some(CalcComponent::Angle(s)) => s,
        Some(CalcComponent::Number(s)) => s,
        None => 1.0,
        _ => return Err(location.new_custom_error(())),
      };
      round_value(deg, step, strategy, location).map(CalcComponent::Angle)
    }
    CalcComponent::Length(len) => {
      let (value, unit) =
        extract_simple_length(&len).ok_or_else(|| location.new_custom_error(()))?;
      let (step_value, step_unit) = match step {
        Some(CalcComponent::Length(step_len)) => {
          extract_simple_length(&step_len).ok_or_else(|| location.new_custom_error(()))?
        }
        None => (1.0, unit),
        _ => return Err(location.new_custom_error(())),
      };
      if unit != step_unit {
        return Err(location.new_custom_error(()));
      }
      round_value(value, step_value, strategy, location)
        .map(|v| CalcComponent::Length(CalcLength::single(unit, v)))
    }
  }
}

fn parse_round_function<'i, 't>(
  input: &mut Parser<'i, 't>,
  location: cssparser::SourceLocation,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
  input.skip_whitespace();
  let strategy = input
    .try_parse(|p| {
      p.expect_ident()
        .map(|ident| ident.as_ref().to_ascii_lowercase())
    })
    .ok()
    .and_then(|ident| match ident.as_str() {
      "nearest" => Some(RoundStrategy::Nearest),
      "up" => Some(RoundStrategy::Up),
      "down" => Some(RoundStrategy::Down),
      "to-zero" => Some(RoundStrategy::ToZero),
      "away-from-zero" => Some(RoundStrategy::AwayFromZero),
      _ => None,
    });

  let strategy = if let Some(s) = strategy {
    input.skip_whitespace();
    input.expect_comma()?;
    s
  } else {
    RoundStrategy::Nearest
  };

  input.skip_whitespace();
  let value = parse_calc_sum(input)?;
  input.skip_whitespace();
  let step = if input.try_parse(|p| p.expect_comma()).is_ok() {
    input.skip_whitespace();
    Some(parse_calc_sum(input)?)
  } else {
    None
  };

  apply_round_function(value, step, strategy, location)
}

fn apply_mod_rem<'i>(
  lhs: CalcComponent,
  rhs: CalcComponent,
  euclidean: bool,
  location: cssparser::SourceLocation,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
  match (lhs.dimension(), rhs.dimension()) {
    (CalcDimension::Number, CalcDimension::Number) => {
      let a = expect_number(lhs, location)?;
      let b = expect_number(rhs, location)?;
      if b == 0.0 {
        return Err(location.new_custom_error(()));
      }
      let result = if euclidean { a.rem_euclid(b) } else { a % b };
      ensure_finite(result, location).map(CalcComponent::Number)
    }
    (CalcDimension::Angle, CalcDimension::Angle) => {
      let a = expect_angle(lhs, location)?;
      let b = expect_angle(rhs, location)?;
      if b == 0.0 {
        return Err(location.new_custom_error(()));
      }
      let result = if euclidean { a.rem_euclid(b) } else { a % b };
      ensure_finite(result, location).map(CalcComponent::Angle)
    }
    (CalcDimension::Length, CalcDimension::Length) => {
      let (a, unit) = expect_simple_length(lhs, location)?;
      let (b, other_unit) = expect_simple_length(rhs, location)?;
      if b == 0.0 || unit != other_unit {
        return Err(location.new_custom_error(()));
      }
      let result = if euclidean { a.rem_euclid(b) } else { a % b };
      ensure_finite(result, location).map(|v| CalcComponent::Length(CalcLength::single(unit, v)))
    }
    _ => Err(location.new_custom_error(())),
  }
}

fn parse_hypot_function<'i, 't>(
  input: &mut Parser<'i, 't>,
  location: cssparser::SourceLocation,
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

  if values.len() < 2 || !all_same_dimension(&values) {
    return Err(location.new_custom_error(()));
  }

  match values[0].dimension() {
    CalcDimension::Number => {
      let mut acc: f32 = 0.0;
      for v in values {
        let n = expect_number(v, location)?;
        acc = acc.hypot(n);
      }
      ensure_finite(acc, location).map(CalcComponent::Number)
    }
    CalcDimension::Angle => {
      let mut acc: f32 = 0.0;
      for v in values {
        let n = expect_angle(v, location)?;
        acc = acc.hypot(n);
      }
      ensure_finite(acc, location).map(CalcComponent::Angle)
    }
    CalcDimension::Length => {
      let mut acc_unit: Option<LengthUnit> = None;
      let mut acc_value: f32 = 0.0;
      for v in values {
        let (val, unit) = expect_simple_length(v, location)?;
        if let Some(existing) = acc_unit {
          if existing != unit {
            return Err(location.new_custom_error(()));
          }
        } else {
          acc_unit = Some(unit);
        }
        acc_value = acc_value.hypot(val);
      }
      let unit = acc_unit.ok_or_else(|| location.new_custom_error(()))?;
      ensure_finite(acc_value, location).map(|v| CalcComponent::Length(CalcLength::single(unit, v)))
    }
  }
}

fn parse_math_function<'i, 't>(
  func: &str,
  input: &mut Parser<'i, 't>,
  location: cssparser::SourceLocation,
) -> Result<CalcComponent, cssparser::ParseError<'i, ()>> {
  match func {
    "calc" => input.parse_nested_block(parse_calc_sum),
    "min" => input.parse_nested_block(|block| parse_min_max(block, MathFn::Min)),
    "max" => input.parse_nested_block(|block| parse_min_max(block, MathFn::Max)),
    "clamp" => input.parse_nested_block(parse_clamp),
    "sin" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let angle = expect_angle(parse_calc_sum(block)?, location)?.to_radians();
      ensure_finite(angle.sin(), location).map(CalcComponent::Number)
    }),
    "cos" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let angle = expect_angle(parse_calc_sum(block)?, location)?.to_radians();
      ensure_finite(angle.cos(), location).map(CalcComponent::Number)
    }),
    "tan" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let angle = expect_angle(parse_calc_sum(block)?, location)?.to_radians();
      ensure_finite(angle.tan(), location).map(CalcComponent::Number)
    }),
    "asin" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let value = expect_number(parse_calc_sum(block)?, location)?;
      ensure_finite(value.asin().to_degrees(), location).map(CalcComponent::Angle)
    }),
    "acos" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let value = expect_number(parse_calc_sum(block)?, location)?;
      ensure_finite(value.acos().to_degrees(), location).map(CalcComponent::Angle)
    }),
    "atan" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let value = expect_number(parse_calc_sum(block)?, location)?;
      ensure_finite(value.atan().to_degrees(), location).map(CalcComponent::Angle)
    }),
    "atan2" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let y = parse_calc_sum(block)?;
      block.skip_whitespace();
      block.expect_comma()?;
      block.skip_whitespace();
      let x = parse_calc_sum(block)?;
      let yv = expect_number(y, location)?;
      let xv = expect_number(x, location)?;
      ensure_finite(yv.atan2(xv).to_degrees(), location).map(CalcComponent::Angle)
    }),
    "pow" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let base = parse_calc_sum(block)?;
      block.skip_whitespace();
      block.expect_comma()?;
      block.skip_whitespace();
      let exp = parse_calc_sum(block)?;
      let base_num = expect_number(base, location)?;
      let exp_num = expect_number(exp, location)?;
      ensure_finite(base_num.powf(exp_num), location).map(CalcComponent::Number)
    }),
    "sqrt" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let value = expect_number(parse_calc_sum(block)?, location)?;
      ensure_finite(value.sqrt(), location).map(CalcComponent::Number)
    }),
    "hypot" => input.parse_nested_block(|block| parse_hypot_function(block, location)),
    "log" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let value = expect_number(parse_calc_sum(block)?, location)?;
      block.skip_whitespace();
      let base = if block.try_parse(|p| p.expect_comma()).is_ok() {
        block.skip_whitespace();
        expect_number(parse_calc_sum(block)?, location)?
      } else {
        std::f32::consts::E
      };
      if value <= 0.0 || base <= 0.0 || (base - 1.0).abs() < f32::EPSILON {
        return Err(location.new_custom_error(()));
      }
      ensure_finite(value.log(base), location).map(CalcComponent::Number)
    }),
    "exp" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let value = expect_number(parse_calc_sum(block)?, location)?;
      ensure_finite(value.exp(), location).map(CalcComponent::Number)
    }),
    "sign" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let value = parse_calc_sum(block)?;
      match value.dimension() {
        CalcDimension::Number => Ok(CalcComponent::Number(
          expect_number(value, location)?.signum(),
        )),
        CalcDimension::Angle => Ok(CalcComponent::Number(
          expect_angle(value, location)?.signum(),
        )),
        CalcDimension::Length => {
          let (val, _) = expect_simple_length(value, location)?;
          Ok(CalcComponent::Number(val.signum()))
        }
      }
    }),
    "abs" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let value = parse_calc_sum(block)?;
      match value {
        CalcComponent::Number(v) => Ok(CalcComponent::Number(v.abs())),
        CalcComponent::Angle(v) => Ok(CalcComponent::Angle(v.abs())),
        CalcComponent::Length(len) => {
          let (v, unit) =
            extract_simple_length(&len).ok_or_else(|| location.new_custom_error(()))?;
          Ok(CalcComponent::Length(CalcLength::single(unit, v.abs())))
        }
      }
    }),
    "clamped" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let value = expect_number(parse_calc_sum(block)?, location)?;
      Ok(CalcComponent::Number(value.clamp(0.0, 1.0)))
    }),
    "round" => input.parse_nested_block(|block| parse_round_function(block, location)),
    "mod" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let lhs = parse_calc_sum(block)?;
      block.skip_whitespace();
      block.expect_comma()?;
      block.skip_whitespace();
      let rhs = parse_calc_sum(block)?;
      apply_mod_rem(lhs, rhs, true, location)
    }),
    "rem" => input.parse_nested_block(|block| {
      block.skip_whitespace();
      let lhs = parse_calc_sum(block)?;
      block.skip_whitespace();
      block.expect_comma()?;
      block.skip_whitespace();
      let rhs = parse_calc_sum(block)?;
      apply_mod_rem(lhs, rhs, false, location)
    }),
    _ => Err(location.new_custom_error(())),
  }
}
