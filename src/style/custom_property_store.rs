use crate::style::values::CustomPropertyValue;
use im::HashMap;

/// Structurally-shared custom property store.
///
/// This is backed by a persistent map so cloning a `ComputedStyle` (and inheriting custom
/// properties to descendants) is O(1) with structural sharing instead of O(vars).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct CustomPropertyStore(HashMap<String, CustomPropertyValue>);

impl CustomPropertyStore {
  pub fn new() -> Self {
    Self(HashMap::new())
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  pub fn get(&self, name: &str) -> Option<&CustomPropertyValue> {
    self.0.get(name)
  }

  pub fn contains_key(&self, name: &str) -> bool {
    self.0.contains_key(name)
  }

  pub fn insert(&mut self, name: String, value: CustomPropertyValue) {
    self.0.insert(name, value);
  }

  pub fn remove(&mut self, name: &str) {
    self.0.remove(name);
  }

  pub fn values(&self) -> impl Iterator<Item = &CustomPropertyValue> + '_ {
    self.0.values()
  }

  pub fn iter(&self) -> impl Iterator<Item = (&String, &CustomPropertyValue)> + '_ {
    self.0.iter()
  }
}
