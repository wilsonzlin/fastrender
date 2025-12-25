//! CSS Custom Property (var()) Resolution
//!
//! Implements token-based resolution of `var()` references using `cssparser`
//! so that nested functions, fallbacks with commas, and repeated substitutions
//! are handled correctly.

use crate::css::properties::{parse_length, parse_property_value};
use crate::css::types::PropertyValue;
use cssparser::ParseError;
use cssparser::ParseErrorKind;
use cssparser::Parser;
use cssparser::ParserInput;
use cssparser::ToCss;
use cssparser::Token;
use std::collections::HashMap;

/// Maximum depth for recursive var() resolution to prevent infinite loops
const MAX_RECURSION_DEPTH: usize = 10;

/// Result of a var() resolution attempt
#[derive(Debug, Clone)]
pub enum VarResolutionResult {
  /// Successfully resolved to a value
  Resolved {
    value: Box<PropertyValue>,
    css_text: String,
  },
  /// The variable was not found and no fallback was provided (or the fallback failed to resolve)
  NotFound(String),
  /// Recursion depth exceeded (possible circular reference)
  RecursionLimitExceeded,
  /// Invalid var() syntax
  InvalidSyntax(String),
}

impl VarResolutionResult {
  /// Returns the resolved value if successful, otherwise returns the original value
  pub fn unwrap_or(self, default: PropertyValue) -> PropertyValue {
    match self {
      VarResolutionResult::Resolved { value, .. } => *value,
      _ => default,
    }
  }

  /// Returns true if the resolution was successful
  pub fn is_resolved(&self) -> bool {
    matches!(self, VarResolutionResult::Resolved { .. })
  }

  /// Returns the CSS serialization of the resolved value if available.
  ///
  /// For invalid syntax, this returns the resolved string that failed to parse,
  /// which is still useful for consumers that need the token-stream result.
  pub fn css_text(&self) -> Option<&str> {
    match self {
      VarResolutionResult::Resolved { css_text, .. } => Some(css_text.as_str()),
      VarResolutionResult::InvalidSyntax(text) => Some(text.as_str()),
      _ => None,
    }
  }
}

/// Resolves CSS `var()` references using the provided custom properties.
///
/// This helper performs property-agnostic resolution (parses fallback/results without knowing
/// the destination property). For property-aware parsing, use `resolve_var_for_property`.
// Custom properties are keyed by String with the default hasher; this is fine for our usage.
#[allow(clippy::implicit_hasher)]
pub fn resolve_var(
  value: &PropertyValue,
  custom_properties: &HashMap<String, String>,
) -> PropertyValue {
  match resolve_var_recursive(value, custom_properties, 0, "") {
    VarResolutionResult::Resolved { value, .. } => *value,
    other => other.unwrap_or(value.clone()),
  }
}

/// Resolves CSS `var()` references with knowledge of the target property.
///
/// Passing the property name allows the resolver to parse the substituted value using the
/// appropriate grammar (e.g., background layers with commas), rather than the generic parser.
// As above, allow implicit_hasher for the property map rather than plumbing a custom hasher.
#[allow(clippy::implicit_hasher)]
pub fn resolve_var_for_property(
  value: &PropertyValue,
  custom_properties: &HashMap<String, String>,
  property_name: &str,
) -> VarResolutionResult {
  resolve_var_recursive(value, custom_properties, 0, property_name)
}

/// Resolves var() references with explicit depth tracking
///
/// This function is useful when you need to track the recursion depth,
/// for example when implementing custom resolution strategies.
#[allow(clippy::implicit_hasher)]
pub fn resolve_var_with_depth(
  value: &PropertyValue,
  custom_properties: &HashMap<String, String>,
  depth: usize,
) -> PropertyValue {
  match resolve_var_recursive(value, custom_properties, depth, "") {
    VarResolutionResult::Resolved { value, .. } => *value,
    other => other.unwrap_or(value.clone()),
  }
}

/// Internal recursive implementation of var() resolution
fn resolve_var_recursive(
  value: &PropertyValue,
  custom_properties: &HashMap<String, String>,
  depth: usize,
  property_name: &str,
) -> VarResolutionResult {
  if depth >= MAX_RECURSION_DEPTH {
    return VarResolutionResult::RecursionLimitExceeded;
  }

  match value {
    PropertyValue::Keyword(raw) => {
      resolve_from_string(raw, custom_properties, depth, property_name)
    }
    PropertyValue::Custom(raw) => resolve_from_string(raw, custom_properties, depth, property_name),
    _ => VarResolutionResult::Resolved {
      value: Box::new(value.clone()),
      css_text: String::new(),
    },
  }
}

fn resolve_from_string(
  raw: &str,
  custom_properties: &HashMap<String, String>,
  depth: usize,
  property_name: &str,
) -> VarResolutionResult {
  let mut stack = Vec::new();
  match resolve_value_tokens(raw, custom_properties, &mut stack, depth) {
    Ok(tokens) => {
      let resolved = tokens_to_css_string(&tokens);
      match parse_value_after_resolution(&resolved, property_name) {
        Some(value) => VarResolutionResult::Resolved {
          value: Box::new(value),
          css_text: resolved,
        },
        None => VarResolutionResult::InvalidSyntax(resolved),
      }
    }
    Err(err) => err,
  }
}

fn resolve_value_tokens(
  value: &str,
  custom_properties: &HashMap<String, String>,
  stack: &mut Vec<String>,
  depth: usize,
) -> Result<Vec<String>, VarResolutionResult> {
  if depth >= MAX_RECURSION_DEPTH {
    return Err(VarResolutionResult::RecursionLimitExceeded);
  }

  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  resolve_tokens_from_parser(&mut parser, custom_properties, stack, depth)
}

fn resolve_tokens_from_parser<'i, 't>(
  parser: &mut Parser<'i, 't>,
  custom_properties: &HashMap<String, String>,
  stack: &mut Vec<String>,
  depth: usize,
) -> Result<Vec<String>, VarResolutionResult> {
  let mut output = Vec::new();

  while let Ok(token) = parser.next_including_whitespace_and_comments() {
    match token {
      Token::Function(name) if name.eq_ignore_ascii_case("var") => {
        let nested = parser.parse_nested_block(|nested| {
          parse_var_function(nested, custom_properties, stack, depth)
            .map_err(|err| nested.new_custom_error(err))
        });
        let resolved = map_nested_result(nested, "var")?;
        output.extend(resolved);
      }
      Token::Function(name) => {
        let name = name.as_ref().to_string();
        let nested = parser.parse_nested_block(|nested| {
          resolve_tokens_from_parser(nested, custom_properties, stack, depth)
            .map_err(|err| nested.new_custom_error(err))
        });
        let resolved = map_nested_result(nested, &name)?;
        let mut text = name;
        text.push('(');
        text.push_str(&tokens_to_css_string(&resolved));
        text.push(')');
        output.push(text);
      }
      Token::ParenthesisBlock => {
        let nested = parser.parse_nested_block(|nested| {
          resolve_tokens_from_parser(nested, custom_properties, stack, depth)
            .map_err(|err| nested.new_custom_error(err))
        });
        let resolved = map_nested_result(nested, "()")?;
        output.push(format!("({})", tokens_to_css_string(&resolved)));
      }
      Token::SquareBracketBlock => {
        let nested = parser.parse_nested_block(|nested| {
          resolve_tokens_from_parser(nested, custom_properties, stack, depth)
            .map_err(|err| nested.new_custom_error(err))
        });
        let resolved = map_nested_result(nested, "[]")?;
        output.push(format!("[{}]", tokens_to_css_string(&resolved)));
      }
      Token::CurlyBracketBlock => {
        let nested = parser.parse_nested_block(|nested| {
          resolve_tokens_from_parser(nested, custom_properties, stack, depth)
            .map_err(|err| nested.new_custom_error(err))
        });
        let resolved = map_nested_result(nested, "{}")?;
        output.push(format!("{{{}}}", tokens_to_css_string(&resolved)));
      }
      other => output.push(token_to_css_string(&other)),
    }
  }

  Ok(output)
}

fn map_nested_result<'i>(
  result: Result<Vec<String>, ParseError<'i, VarResolutionResult>>,
  hint: &str,
) -> Result<Vec<String>, VarResolutionResult> {
  match result {
    Ok(tokens) => Ok(tokens),
    Err(err) => match err.kind {
      ParseErrorKind::Custom(inner) => Err(inner),
      _ => Err(VarResolutionResult::InvalidSyntax(hint.to_string())),
    },
  }
}

fn parse_var_function<'i, 't>(
  parser: &mut Parser<'i, 't>,
  custom_properties: &HashMap<String, String>,
  stack: &mut Vec<String>,
  depth: usize,
) -> Result<Vec<String>, VarResolutionResult> {
  let (var_name, fallback) = parse_var_function_arguments(parser)?;
  resolve_variable_reference(
    &var_name,
    fallback.as_deref(),
    custom_properties,
    stack,
    depth,
  )
}

fn parse_var_function_arguments<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> Result<(String, Option<String>), VarResolutionResult> {
  let mut var_name: Option<String> = None;

  while let Ok(token) = parser.next_including_whitespace_and_comments() {
    match token {
      Token::WhiteSpace(_) | Token::Comment(_) => continue,
      Token::Ident(ident) => {
        let name = ident.as_ref().to_string();
        if !name.starts_with("--") {
          return Err(VarResolutionResult::InvalidSyntax(name));
        }
        var_name = Some(name);
        break;
      }
      other => {
        return Err(VarResolutionResult::InvalidSyntax(token_to_css_string(
          &other,
        )))
      }
    }
  }

  let Some(name) = var_name else {
    return Err(VarResolutionResult::InvalidSyntax(String::new()));
  };

  let fallback_start = loop {
    match parser.next_including_whitespace_and_comments() {
      Ok(Token::WhiteSpace(_) | Token::Comment(_)) => continue,
      Ok(Token::Comma) => break parser.position(),
      Ok(other) => {
        return Err(VarResolutionResult::InvalidSyntax(token_to_css_string(
          &other,
        )))
      }
      Err(_) => return Ok((name, None)),
    }
  };

  while let Ok(_) = parser.next_including_whitespace_and_comments() {}
  let fallback_slice = parser.slice_from(fallback_start);
  Ok((name, Some(fallback_slice.to_string())))
}

fn resolve_variable_reference(
  name: &str,
  fallback: Option<&str>,
  custom_properties: &HashMap<String, String>,
  stack: &mut Vec<String>,
  depth: usize,
) -> Result<Vec<String>, VarResolutionResult> {
  if depth >= MAX_RECURSION_DEPTH {
    return Err(VarResolutionResult::RecursionLimitExceeded);
  }

  if stack.iter().any(|n| n == name) {
    return Err(VarResolutionResult::RecursionLimitExceeded);
  }

  if let Some(value) = custom_properties.get(name) {
    stack.push(name.to_string());
    let result = resolve_value_tokens(value, custom_properties, stack, depth + 1);
    stack.pop();
    return result;
  }

  if let Some(fallback_value) = fallback {
    return resolve_value_tokens(fallback_value, custom_properties, stack, depth + 1).map_err(
      |err| match err {
        VarResolutionResult::NotFound(_) => VarResolutionResult::NotFound(name.to_string()),
        other => other,
      },
    );
  }

  Err(VarResolutionResult::NotFound(name.to_string()))
}

fn parse_value_after_resolution(value: &str, property_name: &str) -> Option<PropertyValue> {
  if property_name.is_empty() {
    Some(parse_untyped_value(value))
  } else {
    parse_property_value(property_name, value)
  }
}

fn parse_untyped_value(value: &str) -> PropertyValue {
  let trimmed = value.trim();
  if let Some(len) = parse_length(trimmed) {
    return PropertyValue::Length(len);
  }
  if let Ok(num) = trimmed.parse::<f32>() {
    return PropertyValue::Number(num);
  }
  if trimmed.ends_with('%') {
    if let Ok(num) = trimmed[..trimmed.len() - 1].parse::<f32>() {
      return PropertyValue::Percentage(num);
    }
  }
  PropertyValue::Keyword(trimmed.to_string())
}

fn tokens_to_css_string(tokens: &[String]) -> String {
  tokens.concat()
}

fn token_to_css_string(token: &Token) -> String {
  match token {
    Token::WhiteSpace(ws) => ws.to_string(),
    Token::Comment(text) => format!("/*{}*/", text),
    _ => token.to_css_string(),
  }
}

/// Checks if a string contains any var() references (case-insensitive)
pub fn contains_var(value: &str) -> bool {
  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  contains_var_in_parser(&mut parser)
}

fn contains_var_in_parser<'i, 't>(parser: &mut Parser<'i, 't>) -> bool {
  let mut found = false;

  while let Ok(token) = parser.next_including_whitespace_and_comments() {
    match token {
      Token::Function(name) if name.eq_ignore_ascii_case("var") => {
        found = true;
        let _ = parser
          .parse_nested_block(|nested| Ok::<_, ParseError<'i, ()>>(contains_var_in_parser(nested)));
      }
      Token::Function(_)
      | Token::ParenthesisBlock
      | Token::SquareBracketBlock
      | Token::CurlyBracketBlock => {
        if let Ok(nested_found) = parser
          .parse_nested_block(|nested| Ok::<_, ParseError<'i, ()>>(contains_var_in_parser(nested)))
        {
          if nested_found {
            found = true;
          }
        }
      }
      _ => {}
    }
  }

  found
}

/// Extracts all custom property names referenced in a value
pub fn extract_var_references(value: &str) -> Vec<String> {
  let mut refs = Vec::new();
  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  collect_var_references_from_parser(&mut parser, &mut refs);
  refs
}

fn collect_var_references_from_parser<'i, 't>(parser: &mut Parser<'i, 't>, refs: &mut Vec<String>) {
  while let Ok(token) = parser.next_including_whitespace_and_comments() {
    match token {
      Token::Function(name) if name.eq_ignore_ascii_case("var") => {
        let _ = parser.parse_nested_block(|nested| {
          if let Ok((name, fallback)) = parse_var_function_arguments(nested) {
            refs.push(name.clone());
            if let Some(fallback_value) = fallback {
              let mut input = ParserInput::new(&fallback_value);
              let mut nested_parser = Parser::new(&mut input);
              collect_var_references_from_parser(&mut nested_parser, refs);
            }
          }
          Ok::<_, ParseError<'i, ()>>(())
        });
      }
      Token::Function(_)
      | Token::ParenthesisBlock
      | Token::SquareBracketBlock
      | Token::CurlyBracketBlock => {
        let _ = parser.parse_nested_block(|nested| {
          collect_var_references_from_parser(nested, refs);
          Ok::<_, ParseError<'i, ()>>(())
        });
      }
      _ => {}
    }
  }
}

/// Validates that a custom property name follows CSS naming rules
pub fn is_valid_custom_property_name(name: &str) -> bool {
  if !name.starts_with("--") {
    return false;
  }

  if name.len() <= 2 {
    return false; // Just "--" is not valid
  }

  // The rest can be any character except whitespace
  // (CSS spec allows almost any character in custom property names)
  !name[2..].chars().any(char::is_whitespace)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::style::values::Length;
  use crate::style::values::LengthUnit;

  fn make_props(pairs: &[(&str, &str)]) -> HashMap<String, String> {
    pairs
      .iter()
      .map(|(k, v)| ((*k).to_string(), (*v).to_string()))
      .collect()
  }

  // Basic var() resolution tests
  #[test]
  fn test_resolve_simple_var() {
    let props = make_props(&[("--color", "#ff0000")]);
    let value = PropertyValue::Keyword("var(--color)".to_string());
    let resolved = resolve_var(&value, &props);

    // Should resolve to a color keyword when property context is missing
    matches!(resolved, PropertyValue::Keyword(ref kw) if kw == "#ff0000");
  }

  #[test]
  fn test_resolve_var_with_length() {
    let props = make_props(&[("--size", "16px")]);
    let value = PropertyValue::Keyword("var(--size)".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Length(len) = resolved {
      assert_eq!(len.value, 16.0);
    } else {
      panic!("Expected Length, got {:?}", resolved);
    }
  }

  #[test]
  fn test_resolve_var_not_found() {
    let props = HashMap::new();
    let value = PropertyValue::Keyword("var(--missing)".to_string());
    let resolved = resolve_var(&value, &props);

    // Should return the original var() call
    if let PropertyValue::Keyword(kw) = resolved {
      assert!(kw.contains("var(--missing)"));
    } else {
      panic!("Expected Keyword, got {:?}", resolved);
    }
  }

  // Fallback value tests
  #[test]
  fn test_resolve_var_with_fallback_not_needed() {
    let props = make_props(&[("--color", "blue")]);
    let value = PropertyValue::Keyword("var(--color, red)".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Keyword(kw) = resolved {
      assert_eq!(kw, "blue");
    } else {
      panic!("Expected Keyword, got {:?}", resolved);
    }
  }

  #[test]
  fn test_resolve_var_with_fallback_used() {
    let props = HashMap::new();
    let value = PropertyValue::Keyword("var(--missing, red)".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Keyword(kw) = resolved {
      assert_eq!(kw, "red");
    } else {
      panic!("Expected Keyword 'red', got {:?}", resolved);
    }
  }

  #[test]
  fn test_resolve_var_with_fallback_length() {
    let props = HashMap::new();
    let value = PropertyValue::Keyword("var(--spacing, 10px)".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Length(len) = resolved {
      assert_eq!(len.value, 10.0);
    } else {
      panic!("Expected Length, got {:?}", resolved);
    }
  }

  // Nested var() tests
  #[test]
  fn test_resolve_nested_var_in_fallback() {
    let props = make_props(&[("--fallback-color", "green")]);
    let value = PropertyValue::Keyword("var(--color, var(--fallback-color))".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Keyword(kw) = resolved {
      assert_eq!(kw, "green");
    } else {
      panic!("Expected Keyword 'green', got {:?}", resolved);
    }
  }

  #[test]
  fn test_resolve_chained_vars() {
    let props = make_props(&[("--primary", "var(--base)"), ("--base", "#0000ff")]);
    let value = PropertyValue::Keyword("var(--primary)".to_string());
    let resolved = resolve_var(&value, &props);

    // Should resolve through the chain
    matches!(resolved, PropertyValue::Keyword(ref kw) if kw == "#0000ff");
  }

  // Embedded var() tests
  #[test]
  fn test_resolve_embedded_var_in_calc() {
    let props = make_props(&[("--size", "10px")]);
    let value = PropertyValue::Keyword("calc(var(--size) + 5px)".to_string());
    let resolved = resolve_var(&value, &props);

    assert!(
      matches!(resolved, PropertyValue::Length(len) if (len.value - 15.0).abs() < f32::EPSILON && len.unit == LengthUnit::Px),
      "Expected resolved calc length, got {:?}",
      resolved
    );
  }

  #[test]
  fn test_resolve_multiple_embedded_vars() {
    let props = make_props(&[("--x", "10px"), ("--y", "20px")]);
    let value = PropertyValue::Keyword("var(--x) var(--y)".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Keyword(kw) = resolved {
      assert!(kw.contains("10px"));
      assert!(kw.contains("20px"));
    } else {
      panic!("Expected Keyword, got {:?}", resolved);
    }
  }

  #[test]
  fn test_resolve_var_uses_property_specific_parser() {
    let props = make_props(&[("--bg", "url(image.png), linear-gradient(red, blue)")]);
    let value = PropertyValue::Keyword("var(--bg)".to_string());
    let resolved = resolve_var_for_property(&value, &props, "background-image");

    if let VarResolutionResult::Resolved { value: boxed, .. } = resolved {
      let list = match *boxed {
        PropertyValue::Multiple(list) => list,
        _ => panic!("Expected Multiple for background layers, got {:?}", boxed),
      };
      assert_eq!(list.len(), 3); // url, comma token, gradient
      assert!(matches!(list[0], PropertyValue::Url(ref u) if u == "image.png"));
      assert!(matches!(
        list[2],
        PropertyValue::LinearGradient { .. } | PropertyValue::RepeatingLinearGradient { .. }
      ));
    } else {
      panic!(
        "Expected Multiple for background layers, got {:?}",
        resolved
      );
    }
  }

  #[test]
  fn unresolved_var_marks_declaration_invalid() {
    let props = HashMap::new();
    let value = PropertyValue::Keyword("var(--missing)".to_string());
    let resolved = resolve_var_for_property(&value, &props, "color");
    assert!(matches!(resolved, VarResolutionResult::NotFound(_)));
  }

  #[test]
  fn unresolved_fallback_var_marks_declaration_invalid() {
    let props = make_props(&[("--fallback", "var(--still-missing)")]);
    let value = PropertyValue::Keyword("var(--missing, var(--fallback))".to_string());
    let resolved = resolve_var_for_property(&value, &props, "color");
    assert!(matches!(resolved, VarResolutionResult::NotFound(_)));
  }

  // Recursion limit tests
  #[test]
  fn test_recursion_limit() {
    // Create a circular reference
    let props = make_props(&[
      ("--a", "var(--b)"),
      ("--b", "var(--c)"),
      ("--c", "var(--a)"), // Circular!
    ]);
    let value = PropertyValue::Keyword("var(--a)".to_string());

    // Should not stack overflow - recursion limit should kick in
    let _resolved = resolve_var(&value, &props);
    // If we get here without panicking, the test passes
  }

  // Utility function tests
  #[test]
  fn test_contains_var() {
    assert!(contains_var("var(--color)"));
    assert!(contains_var("calc(var(--size) + 10px)"));
    assert!(contains_var("0 0 var(--blur) black"));
    assert!(!contains_var("10px"));
    assert!(!contains_var("red"));
    assert!(!contains_var(""));
  }

  #[test]
  fn test_extract_var_references() {
    let refs = extract_var_references("var(--color)");
    assert_eq!(refs, vec!["--color"]);

    let refs = extract_var_references("calc(var(--size) + var(--margin))");
    assert_eq!(refs, vec!["--size", "--margin"]);

    let refs = extract_var_references("var(--x, var(--y))");
    assert_eq!(refs, vec!["--x", "--y"]);

    let refs = extract_var_references("10px");
    assert!(refs.is_empty());
  }

  #[test]
  fn test_is_valid_custom_property_name() {
    assert!(is_valid_custom_property_name("--color"));
    assert!(is_valid_custom_property_name("--color-primary"));
    assert!(is_valid_custom_property_name("--_internal"));
    assert!(is_valid_custom_property_name("--123"));
    assert!(is_valid_custom_property_name("--myVar"));

    assert!(!is_valid_custom_property_name("color"));
    assert!(!is_valid_custom_property_name("-color"));
    assert!(!is_valid_custom_property_name("--"));
    assert!(!is_valid_custom_property_name("--has space"));
  }

  // Edge cases
  #[test]
  fn test_empty_var() {
    let props = HashMap::new();
    let value = PropertyValue::Keyword("var()".to_string());
    let resolved = resolve_var(&value, &props);

    // Should return the original malformed var()
    if let PropertyValue::Keyword(kw) = resolved {
      assert_eq!(kw, "var()");
    }
  }

  #[test]
  fn test_var_with_whitespace() {
    let props = make_props(&[("--color", "blue")]);
    let value = PropertyValue::Keyword("var(  --color  )".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Keyword(kw) = resolved {
      assert_eq!(kw, "blue");
    } else {
      panic!("Expected Keyword 'blue', got {:?}", resolved);
    }
  }

  #[test]
  fn test_non_var_value_unchanged() {
    let props = make_props(&[("--color", "blue")]);
    let value = PropertyValue::Length(Length::px(10.0));
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Length(len) = resolved {
      assert_eq!(len.value, 10.0);
    } else {
      panic!("Expected Length, got {:?}", resolved);
    }
  }

  #[test]
  fn test_resolve_var_result_methods() {
    let resolved = VarResolutionResult::Resolved {
      value: Box::new(PropertyValue::Keyword("blue".to_string())),
      css_text: "blue".to_string(),
    };
    assert!(resolved.is_resolved());

    let default = PropertyValue::Keyword("red".to_string());
    let result = VarResolutionResult::NotFound("--missing".to_string());
    let value = result.unwrap_or(default.clone());
    if let PropertyValue::Keyword(kw) = value {
      assert_eq!(kw, "red");
    }
  }
}
