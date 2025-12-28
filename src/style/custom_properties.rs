use crate::style::values::{CustomPropertySyntax, CustomPropertyValue};
use std::collections::HashMap;

/// Definition of a registered custom property from an @property rule.
#[derive(Debug, Clone, PartialEq)]
pub struct PropertyRule {
  pub name: String,
  pub syntax: CustomPropertySyntax,
  pub inherits: bool,
  pub initial_value: Option<CustomPropertyValue>,
}

/// Registry of custom property registrations from @property rules.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct CustomPropertyRegistry {
  definitions: HashMap<String, PropertyRule>,
}

impl CustomPropertyRegistry {
  pub fn new() -> Self {
    Self::default()
  }

  /// Register (or replace) a custom property definition.
  pub fn register(&mut self, rule: PropertyRule) {
    self.definitions.insert(rule.name.clone(), rule);
  }

  /// Look up a registered custom property definition by name.
  pub fn get(&self, name: &str) -> Option<&PropertyRule> {
    self.definitions.get(name)
  }

  /// Parse a custom property value according to its registered syntax.
  ///
  /// Returns `Err(())` when the value does not conform to the registered syntax.
  pub fn parse_value(&self, name: &str, raw: &str) -> Result<CustomPropertyValue, ()> {
    if let Some(rule) = self.get(name) {
      if matches!(rule.syntax, CustomPropertySyntax::Universal) {
        return Ok(CustomPropertyValue::new(raw, None));
      }
      if let Some(typed) = rule.syntax.parse_value(raw) {
        return Ok(CustomPropertyValue::new(raw, Some(typed)));
      }
      return Err(());
    }

    Ok(CustomPropertyValue::new(raw, None))
  }

  /// Initial values provided by registered custom properties.
  pub fn initial_values(&self) -> HashMap<String, CustomPropertyValue> {
    self
      .definitions
      .iter()
      .filter_map(|(name, rule)| {
        rule
          .initial_value
          .as_ref()
          .map(|v| (name.clone(), v.clone()))
      })
      .collect()
  }

  /// Whether a property is registered and the value of its `inherits` descriptor.
  pub fn inherits(&self, name: &str) -> Option<bool> {
    self.definitions.get(name).map(|r| r.inherits)
  }

  /// Iterator over registered properties.
  pub fn iter(&self) -> impl Iterator<Item = (&String, &PropertyRule)> {
    self.definitions.iter()
  }
}
