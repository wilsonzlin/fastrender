use crate::css::types::{FontFaceRule, KeyframesRule, StyleSheet};
use crate::style::media::{MediaContext, MediaQueryCache};
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

struct SheetsInCascadeOrder<'a> {
  style_set: &'a StyleSet,
  shadow_hosts: Vec<usize>,
  index: usize,
}

impl<'a> SheetsInCascadeOrder<'a> {
  fn new(style_set: &'a StyleSet) -> Self {
    // Shadow roots are relatively rare; sorting the host ids gives us deterministic iteration
    // without materializing/cloning the underlying rule lists.
    let mut shadow_hosts: Vec<usize> = style_set.shadows.keys().copied().collect();
    shadow_hosts.sort_unstable();
    Self {
      style_set,
      shadow_hosts,
      index: 0,
    }
  }
}

impl<'a> Iterator for SheetsInCascadeOrder<'a> {
  type Item = &'a StyleSheet;

  fn next(&mut self) -> Option<Self::Item> {
    if self.index == 0 {
      self.index = 1;
      return Some(&self.style_set.document);
    }

    let shadow_idx = self.index - 1;
    let host = *self.shadow_hosts.get(shadow_idx)?;
    self.index += 1;
    self.style_set.shadows.get(&host)
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    let remaining = (self.shadow_hosts.len() + 1).saturating_sub(self.index);
    (remaining, Some(remaining))
  }
}

impl ExactSizeIterator for SheetsInCascadeOrder<'_> {}

impl StyleSet {
  /// Construct a style set containing only document-level styles.
  pub fn from_document(document: StyleSheet) -> Self {
    Self {
      document,
      shadows: HashMap::new(),
    }
  }

  /// Iterate over all author stylesheets in deterministic cascade order.
  ///
  /// Order:
  /// 1. Document stylesheet
  /// 2. Shadow-root stylesheets, sorted by host node pre-order id (ascending)
  ///
  /// This iterator intentionally avoids cloning or concatenating rule vectors.
  pub fn sheets_in_cascade_order(&self) -> impl Iterator<Item = &StyleSheet> {
    SheetsInCascadeOrder::new(self)
  }

  pub fn collect_font_face_rules_all_scopes_with_cache(
    &self,
    media_ctx: &MediaContext,
    cache: Option<&mut MediaQueryCache>,
  ) -> Vec<FontFaceRule> {
    let mut result = Vec::new();
    let mut cache = cache;
    for sheet in self.sheets_in_cascade_order() {
      result.extend(sheet.collect_font_face_rules_with_cache(media_ctx, cache.as_deref_mut()));
    }
    result
  }

  pub fn collect_keyframes_all_scopes_with_cache(
    &self,
    media_ctx: &MediaContext,
    cache: Option<&mut MediaQueryCache>,
  ) -> Vec<KeyframesRule> {
    let mut result = Vec::new();
    let mut cache = cache;
    for sheet in self.sheets_in_cascade_order() {
      result.extend(sheet.collect_keyframes_with_cache(media_ctx, cache.as_deref_mut()));
    }
    result
  }

  pub fn has_container_rules_any_scope(&self) -> bool {
    self
      .sheets_in_cascade_order()
      .any(|sheet| sheet.has_container_rules())
  }

  pub fn has_starting_style_rules_any_scope(&self) -> bool {
    self
      .sheets_in_cascade_order()
      .any(|sheet| sheet.has_starting_style_rules())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::css::parser::parse_stylesheet;

  #[test]
  fn collectors_process_shadow_sheets_in_sorted_host_order() {
    let document =
      parse_stylesheet("@keyframes doc { from { opacity: 0; } to { opacity: 1; } }")
        .expect("document stylesheet parses");

    let shadow_low =
      parse_stylesheet("@keyframes shadow-low { from { opacity: 0; } to { opacity: 1; } }")
        .expect("shadow stylesheet parses");
    let shadow_high =
      parse_stylesheet("@keyframes shadow-high { from { opacity: 0; } to { opacity: 1; } }")
        .expect("shadow stylesheet parses");

    // Insert out of order; iteration should still use sorted host ids.
    let mut shadows = HashMap::new();
    shadows.insert(10, shadow_high);
    shadows.insert(2, shadow_low);

    let style_set = StyleSet { document, shadows };
    let media_ctx = MediaContext::screen(800.0, 600.0);

    let keyframes = style_set.collect_keyframes_all_scopes_with_cache(&media_ctx, None);
    let names: Vec<&str> = keyframes.iter().map(|rule| rule.name.as_str()).collect();
    assert_eq!(
      names,
      vec!["doc", "shadow-low", "shadow-high"],
      "shadow sheets should be processed in increasing host id order"
    );
  }

  #[test]
  fn collectors_match_document_only_stylesheet_semantics() {
    let stylesheet = parse_stylesheet(
      r#"
@font-face {
  font-family: "TestFamily";
  src: url("fonts/test.woff2");
}

@keyframes fade {
  from { opacity: 0; }
  to { opacity: 1; }
}

@container (min-width: 0px) {
  .a { color: red; }
}

@starting-style {
  .a { opacity: 0; }
}
"#,
    )
    .expect("stylesheet parses");

    let style_set = StyleSet::from_document(stylesheet.clone());
    let media_ctx = MediaContext::screen(800.0, 600.0);

    let mut cache_old = MediaQueryCache::default();
    let mut cache_new = MediaQueryCache::default();

    let expected_fonts =
      stylesheet.collect_font_face_rules_with_cache(&media_ctx, Some(&mut cache_old));
    let actual_fonts =
      style_set.collect_font_face_rules_all_scopes_with_cache(&media_ctx, Some(&mut cache_new));
    assert_eq!(
      format!("{expected_fonts:?}"),
      format!("{actual_fonts:?}"),
      "font-face collection should match document stylesheet behavior"
    );

    cache_old = MediaQueryCache::default();
    cache_new = MediaQueryCache::default();

    let expected_keyframes =
      stylesheet.collect_keyframes_with_cache(&media_ctx, Some(&mut cache_old));
    let actual_keyframes =
      style_set.collect_keyframes_all_scopes_with_cache(&media_ctx, Some(&mut cache_new));
    assert_eq!(
      format!("{expected_keyframes:?}"),
      format!("{actual_keyframes:?}"),
      "keyframes collection should match document stylesheet behavior"
    );

    assert_eq!(
      stylesheet.has_container_rules(),
      style_set.has_container_rules_any_scope(),
      "container-rule detection should match document stylesheet behavior"
    );

    assert_eq!(
      stylesheet.has_starting_style_rules(),
      style_set.has_starting_style_rules_any_scope(),
      "starting-style detection should match document stylesheet behavior"
    );
  }
}
