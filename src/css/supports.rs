use super::properties::{
  is_global_keyword_str, is_known_style_property, parse_property_value,
  supports_parsed_declaration_is_valid,
};
use crate::style::var_resolution::contains_var;

/// Validates a (property, value) pair for use in @supports queries.
///
/// Returns true when the property is recognized and either the value is a CSS-wide keyword,
/// contains a var() reference, or parses according to the engine's supported grammar.
pub fn supports_declaration(property: &str, value: &str) -> bool {
  let trimmed_property = property.trim();
  if trimmed_property.is_empty() {
    return false;
  }

  // Custom properties always accept any value.
  if trimmed_property.starts_with("--") {
    return true;
  }

  let normalized_property = trimmed_property.to_ascii_lowercase();
  if !is_known_style_property(&normalized_property) {
    return false;
  }

  let raw_value = value.trim().trim_end_matches(';');
  let value_without_important = raw_value.trim_end_matches("!important").trim();

  if is_global_keyword_str(value_without_important) {
    return true;
  }

  if contains_var(value_without_important) {
    return true;
  }

  let parsed = match parse_property_value(&normalized_property, value_without_important) {
    Some(v) => v,
    None => return false,
  };

  supports_parsed_declaration_is_valid(&normalized_property, value_without_important, &parsed)
}
