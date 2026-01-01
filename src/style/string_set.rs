use crate::css::types::PropertyValue;
pub use crate::style::content::{StringSetAssignment, StringSetValue};
use cssparser::Parser;
use cssparser::ParserInput;
use cssparser::Token;

fn value_to_css_text(value: &PropertyValue) -> Option<String> {
  match value {
    PropertyValue::Keyword(kw) => Some(kw.clone()),
    PropertyValue::String(s) => Some(format!("\"{}\"", s)),
    PropertyValue::Multiple(list) => {
      let mut parts = Vec::new();
      for item in list {
        match item {
          PropertyValue::Keyword(k) if k != "," => parts.push(k.clone()),
          PropertyValue::String(s) => parts.push(format!("\"{}\"", s)),
          _ => return None,
        }
      }
      Some(parts.join(" "))
    }
    _ => None,
  }
}

/// Parse a `string-set` value from a property value.
///
/// Returns `None` when the value is invalid and the declaration should be ignored.
pub fn parse_string_set_value(value: &PropertyValue) -> Option<Vec<StringSetAssignment>> {
  let css_text = value_to_css_text(value)?;
  parse_string_set(&css_text)
}

/// Parse a `string-set` value from its CSS text representation.
pub fn parse_string_set(input: &str) -> Option<Vec<StringSetAssignment>> {
  let trimmed = input.trim();
  if trimmed.eq_ignore_ascii_case("none") {
    return Some(Vec::new());
  }
  if trimmed.is_empty() {
    return None;
  }

  let mut input = ParserInput::new(trimmed);
  let mut parser = Parser::new(&mut input);
  let mut assignments = Vec::new();

  while !parser.is_exhausted() {
    let name = parser.expect_ident().ok()?.to_string();
    let value = match parser.next() {
      Ok(Token::Function(name)) if name.as_ref().eq_ignore_ascii_case("content") => {
        let ok = parser
          .parse_nested_block(|nested| {
            nested.skip_whitespace();
            if nested.is_exhausted() {
              Ok::<(), cssparser::ParseError<'_, ()>>(())
            } else {
              Err(nested.new_custom_error(()))
            }
          })
          .is_ok();
        if !ok {
          return None;
        }
        StringSetValue::Content
      }
      Ok(Token::QuotedString(s)) => StringSetValue::Literal(s.to_string()),
      _ => return None,
    };
    assignments.push(StringSetAssignment { name, value });
  }

  Some(assignments)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn resolves_var_reference_in_string_set() {
    use crate::css::types::PropertyValue;
    use crate::style::content::StringSetValue;
    use crate::style::custom_property_store::CustomPropertyStore;
    use crate::style::values::CustomPropertyValue;
    use crate::style::var_resolution::{resolve_var_for_property, VarResolutionResult};

    let mut custom_properties = CustomPropertyStore::default();
    custom_properties.insert(
      "--title".to_string(),
      CustomPropertyValue::new(" \"Var Title\"", None),
    );

    let value = PropertyValue::Keyword("header var(--title)".to_string());
    let VarResolutionResult::Resolved { value, .. } =
      resolve_var_for_property(&value, &custom_properties, "string-set")
    else {
      panic!("expected var() resolution to succeed for string-set value");
    };

    let assignments = parse_string_set_value(&value).expect("parse string-set after resolution");
    assert_eq!(assignments.len(), 1);
    assert_eq!(assignments[0].name, "header");
    match &assignments[0].value {
      StringSetValue::Literal(value) => assert_eq!(value, "Var Title"),
      other => panic!("expected literal string value, got {other:?}"),
    }
  }

  #[test]
  fn parses_none_keyword() {
    assert_eq!(parse_string_set("none"), Some(Vec::new()));
    assert_eq!(parse_string_set("NONE"), Some(Vec::new()));
    assert_eq!(parse_string_set(" none "), Some(Vec::new()));
  }

  #[test]
  fn parses_multiple_assignments() {
    let parsed = parse_string_set("chapter content() section \"Intro\"").unwrap();
    assert_eq!(parsed.len(), 2);
    assert_eq!(parsed[0].name, "chapter");
    assert!(matches!(parsed[0].value, StringSetValue::Content));
    assert_eq!(parsed[1].name, "section");
    assert!(matches!(
      parsed[1].value,
      StringSetValue::Literal(ref s) if s == "Intro"
    ));
  }

  #[test]
  fn parses_from_property_value() {
    let value = PropertyValue::Keyword("title \"Preface\"".to_string());
    let parsed = parse_string_set_value(&value).unwrap();
    assert_eq!(parsed.len(), 1);
    assert_eq!(parsed[0].name, "title");
    assert!(matches!(
      parsed[0].value,
      StringSetValue::Literal(ref s) if s == "Preface"
    ));
  }

  #[test]
  fn rejects_invalid_values() {
    assert!(parse_string_set("").is_none());
    assert!(parse_string_set("chapter").is_none());
    assert!(parse_string_set("chapter attr(title)").is_none());
  }
}
