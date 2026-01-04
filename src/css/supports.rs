use super::properties::{
  is_global_keyword_str, is_known_style_property, parse_property_value,
  supports_parsed_declaration_is_valid, vendor_prefixed_property_alias,
};
use crate::style::var_resolution::contains_var;
use std::borrow::Cow;

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

  let normalized_property: Cow<'_, str> = if trimmed_property
    .as_bytes()
    .iter()
    .any(|b| b.is_ascii_uppercase())
  {
    Cow::Owned(trimmed_property.to_ascii_lowercase())
  } else {
    Cow::Borrowed(trimmed_property)
  };
  let raw_value = value.trim().trim_end_matches(';');
  let value_without_important = raw_value.trim_end_matches("!important").trim();

  // Tailwind v4 gates its `@layer properties` reset behind vendor-prefixed probes:
  // `(-webkit-hyphens:none)` and `(-moz-orient:inline)`. These should evaluate true so the global
  // `--tw-*` defaults are retained and participate in the cascade.
  //
  // Important: do not treat arbitrary vendor-prefixed properties as supported, since they are
  // frequently used inside `not(...)` and flipping them to true can invert unrelated feature tests.
  let normalized_property = match normalized_property.as_ref() {
    "-webkit-hyphens" => "hyphens",
    "-moz-orient" => {
      if value_without_important.eq_ignore_ascii_case("inline") {
        return true;
      }
      return false;
    }
    other => other,
  };

  let canonical_property = if is_known_style_property(normalized_property) {
    normalized_property
  } else if let Some(alias) = vendor_prefixed_property_alias(normalized_property) {
    alias
  } else {
    return false;
  };

  if is_global_keyword_str(value_without_important) {
    return true;
  }

  if contains_var(value_without_important) {
    return true;
  }

  let parsed = match parse_property_value(canonical_property, value_without_important) {
    Some(v) => v,
    None => return false,
  };

  supports_parsed_declaration_is_valid(canonical_property, value_without_important, &parsed)
}
