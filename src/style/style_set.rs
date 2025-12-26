use crate::css::types::StyleSheet;
use std::collections::HashMap;

/// Collection of author styles partitioned by tree scope.
///
/// The document stylesheet applies to the light DOM, while each entry in
/// `shadows` applies to the corresponding shadow tree keyed by the host
/// element's pre-order node id.
#[derive(Debug, Clone, Default)]
pub struct StyleSet {
  pub document: StyleSheet,
  pub shadows: HashMap<usize, StyleSheet>,
}

impl StyleSet {
  /// Construct a style set containing only document-level styles.
  pub fn from_document(document: StyleSheet) -> Self {
    Self {
      document,
      shadows: HashMap::new(),
    }
  }
}

