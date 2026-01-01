use crate::style::values::CustomPropertyValue;
use cssparser::{Parser, ParserInput, Token};
use im::HashMap;

/// Structurally-shared custom property store.
///
/// This is backed by a persistent map so cloning a `ComputedStyle` (and inheriting custom
/// properties to descendants) is O(1) with structural sharing instead of O(vars).
#[derive(Debug, Clone, PartialEq)]
pub struct CustomPropertyStore {
  map: HashMap<String, CustomPropertyValue>,
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
      map: HashMap::new(),
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

  pub fn insert(&mut self, name: String, value: CustomPropertyValue) {
    let new_has_revert_layer = contains_revert_layer_token(value.value.as_str());
    let prev_has_revert_layer = self
      .map
      .get(name.as_str())
      .is_some_and(|prev| contains_revert_layer_token(prev.value.as_str()));

    if prev_has_revert_layer {
      self.revert_layer_values = self.revert_layer_values.saturating_sub(1);
    }
    if new_has_revert_layer {
      self.revert_layer_values = self.revert_layer_values.saturating_add(1);
    }

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

  pub fn iter(&self) -> impl Iterator<Item = (&String, &CustomPropertyValue)> + '_ {
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
  for start in 0..=bytes.len().saturating_sub(NEEDLE.len()) {
    let mut matched = true;
    for (offset, &needle_byte) in NEEDLE.iter().enumerate() {
      if bytes[start + offset].to_ascii_lowercase() != needle_byte {
        matched = false;
        break;
      }
    }
    if matched {
      return true;
    }
  }
  if !bytes.contains(&b'\\') {
    return false;
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
