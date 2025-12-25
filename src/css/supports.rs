use super::properties::{parse_property_value, supports_parsed_declaration_is_valid};

/// Validates a (property, value) pair for use in @supports queries.
///
/// Returns true only when the property is recognized and the value parses
/// according to the engine's supported grammar.
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
  let raw_value = value.trim().trim_end_matches(';');
  let value_without_important = raw_value.trim_end_matches("!important").trim();

  let parsed = match parse_property_value(&normalized_property, value_without_important) {
    Some(v) => v,
    None => return false,
  };

  supports_parsed_declaration_is_valid(&normalized_property, value_without_important, &parsed)
}
