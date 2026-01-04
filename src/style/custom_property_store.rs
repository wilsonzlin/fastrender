use crate::style::values::CustomPropertyValue;
use cssparser::{Parser, ParserInput, Token};
use im::HashMap;
use rustc_hash::FxHasher;
#[cfg(test)]
use std::cell::Cell;
use std::hash::BuildHasherDefault;
use std::sync::Arc;

type CustomPropertyMap = HashMap<Arc<str>, CustomPropertyValue, BuildHasherDefault<FxHasher>>;

#[cfg(test)]
thread_local! {
  static TEST_HAMT_INSERTS: Cell<u64> = const { Cell::new(0) };
}

#[cfg(test)]
pub(crate) fn reset_test_hamt_inserts() {
  TEST_HAMT_INSERTS.with(|count| count.set(0));
}

#[cfg(test)]
pub(crate) fn test_hamt_inserts() -> u64 {
  TEST_HAMT_INSERTS.with(|count| count.get())
}

/// Structurally-shared custom property store.
///
/// This is backed by a persistent map so cloning a `ComputedStyle` (and inheriting custom
/// properties to descendants) is O(1) with structural sharing instead of O(vars).
#[derive(Debug, Clone, PartialEq)]
pub struct CustomPropertyStore {
  map: CustomPropertyMap,
  /// Count of custom property values containing the keyword `revert-layer`.
  ///
  /// This is an optimization for `apply_cascaded_declarations`, which otherwise would scan every
  /// custom-property value for the token on every element. Large real-world pages often define
  /// hundreds/thousands of custom properties, so keeping this metadata avoids an O(nodes * vars)
  /// walk in the common case where `revert-layer` is unused.
  revert_layer_values: usize,
}

impl Default for CustomPropertyStore {
  fn default() -> Self {
    Self {
      map: HashMap::default(),
      revert_layer_values: 0,
    }
  }
}

impl CustomPropertyStore {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn is_empty(&self) -> bool {
    self.map.is_empty()
  }

  pub fn len(&self) -> usize {
    self.map.len()
  }

  pub fn get(&self, name: &str) -> Option<&CustomPropertyValue> {
    self.map.get(name)
  }

  pub fn contains_key(&self, name: &str) -> bool {
    self.map.contains_key(name)
  }

  pub fn insert(&mut self, name: Arc<str>, value: CustomPropertyValue) {
    // Avoid the relatively expensive HAMT insert when the value is unchanged.
    //
    // Tailwind-style resets often assign the same custom property value on every element even
    // though the property is inherited. Without this guard we end up allocating new HAMT nodes for
    // no-op overwrites across the entire DOM.
    let prev = self.map.get(name.as_ref());
    if prev.is_some_and(|prev| prev == &value) {
      return;
    }

    let new_has_revert_layer = contains_revert_layer_token(value.value.as_str());
    let prev_has_revert_layer =
      prev.is_some_and(|prev| contains_revert_layer_token(prev.value.as_str()));

    if prev_has_revert_layer {
      self.revert_layer_values = self.revert_layer_values.saturating_sub(1);
    }
    if new_has_revert_layer {
      self.revert_layer_values = self.revert_layer_values.saturating_add(1);
    }

    #[cfg(test)]
    TEST_HAMT_INSERTS.with(|count| count.set(count.get() + 1));

    self.map.insert(name, value);
  }

  pub fn remove(&mut self, name: &str) {
    let prev_has_revert_layer = self
      .map
      .get(name)
      .is_some_and(|prev| contains_revert_layer_token(prev.value.as_str()));
    if prev_has_revert_layer {
      self.revert_layer_values = self.revert_layer_values.saturating_sub(1);
    }
    self.map.remove(name);
  }

  pub fn values(&self) -> impl Iterator<Item = &CustomPropertyValue> + '_ {
    self.map.values()
  }

  pub fn iter(&self) -> impl Iterator<Item = (&Arc<str>, &CustomPropertyValue)> + '_ {
    self.map.iter()
  }

  pub fn has_revert_layer_token(&self) -> bool {
    self.revert_layer_values > 0
  }
}

pub(crate) fn contains_revert_layer_token(haystack: &str) -> bool {
  const NEEDLE: &[u8] = b"revert-layer";
  let bytes = haystack.as_bytes();
  if bytes.len() < NEEDLE.len() {
    return false;
  }

  // Fast-path: most values do not mention `revert-layer` at all, so do a quick case-insensitive
  // substring scan first and bail early when it is absent.
  //
  // Important: this is only a *prefilter*. The substring may appear inside strings/URLs and does
  // not necessarily correspond to an identifier token (which is what matters for `revert-layer`
  // semantics after var() substitution). When the substring is present, fall back to a tokenizer
  // walk to avoid false positives that would otherwise force expensive cascade layer snapshotting.
  let mut has_candidate = false;
  for start in 0..=bytes.len().saturating_sub(NEEDLE.len()) {
    let mut matched = true;
    for (offset, &needle_byte) in NEEDLE.iter().enumerate() {
      if bytes[start + offset].to_ascii_lowercase() != needle_byte {
        matched = false;
        break;
      }
    }
    if matched {
      has_candidate = true;
      break;
    }
  }

  let has_backslash = bytes.contains(&b'\\');
  if !has_candidate && !has_backslash {
    return false;
  }

  // Extremely common case: the value *is* the keyword (possibly with surrounding whitespace).
  // Avoid the tokenizer walk for this hot path.
  if !has_backslash && haystack.trim().eq_ignore_ascii_case("revert-layer") {
    return true;
  }

  fn contains_revert_layer_in_parser<'i, 't>(parser: &mut Parser<'i, 't>) -> bool {
    while let Ok(token) = parser.next_including_whitespace_and_comments() {
      match token {
        Token::Ident(ident) if ident.eq_ignore_ascii_case("revert-layer") => return true,
        Token::Function(_)
        | Token::ParenthesisBlock
        | Token::SquareBracketBlock
        | Token::CurlyBracketBlock => {
          if let Ok(nested_found) = parser.parse_nested_block(|nested| {
            Ok::<_, cssparser::ParseError<'i, ()>>(contains_revert_layer_in_parser(nested))
          }) {
            if nested_found {
              return true;
            }
          }
        }
        _ => {}
      }
    }

    false
  }

  let mut input = ParserInput::new(haystack);
  let mut parser = Parser::new(&mut input);
  contains_revert_layer_in_parser(&mut parser)
}

#[cfg(test)]
mod tests {
  use super::contains_revert_layer_token;

  #[test]
  fn contains_revert_layer_token_requires_ident_token() {
    assert!(contains_revert_layer_token("revert-layer"));
    assert!(contains_revert_layer_token("revert\\-layer"));
    assert!(contains_revert_layer_token("revert-layer/*comment*/"));
    assert!(contains_revert_layer_token("foo revert-layer bar"));

    // Must not match the substring inside a larger identifier.
    assert!(!contains_revert_layer_token("revert-layerx"));

    // Must not match inside string/url tokens (they can't become the CSS-wide keyword).
    assert!(!contains_revert_layer_token("\"revert-layer\""));
    assert!(!contains_revert_layer_token("url(revert-layer.png)"));
  }
}
