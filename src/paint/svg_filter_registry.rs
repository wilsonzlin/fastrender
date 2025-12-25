use crate::image_loader::ImageCache;
use crate::paint::svg_filter::collect_svg_filters;
use crate::paint::svg_filter::SvgFilter;
use crate::tree::box_tree::ReplacedType;
use crate::tree::box_tree::SvgContent;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use std::collections::HashMap;
use std::sync::Arc;

/// Registry of SVG filters defined within a document.
///
/// This is populated from inline `<svg>` content found in the fragment tree so
/// CSS `filter: url(#id)` references can be resolved without fetching
/// external resources.
#[derive(Clone, Default)]
pub struct SvgFilterRegistry {
  filters: HashMap<String, Arc<SvgFilter>>,
}

impl SvgFilterRegistry {
  /// Builds a registry from the provided fragment tree. Requires an image cache
  /// to resolve any `feImage` references during parsing.
  pub fn from_fragment_tree(tree: &FragmentTree, image_cache: Option<&ImageCache>) -> Option<Self> {
    let cache = image_cache?;
    let mut registry = SvgFilterRegistry::default();
    registry.collect_from_fragment(&tree.root, cache);
    for root in &tree.additional_fragments {
      registry.collect_from_fragment(root, cache);
    }
    Some(registry)
  }

  /// Builds a registry from a single fragment root (e.g., when only the root
  /// fragment is available).
  pub fn from_fragment_root(root: &FragmentNode, image_cache: Option<&ImageCache>) -> Option<Self> {
    let cache = image_cache?;
    let mut registry = SvgFilterRegistry::default();
    registry.collect_from_fragment(root, cache);
    Some(registry)
  }

  /// Returns a parsed filter definition for the given id, if present.
  pub fn get(&self, id: &str) -> Option<Arc<SvgFilter>> {
    self.filters.get(id).cloned()
  }

  fn collect_from_fragment(&mut self, fragment: &FragmentNode, cache: &ImageCache) {
    if let FragmentContent::Replaced { replaced_type, .. } = &fragment.content {
      if let ReplacedType::Svg { content } = replaced_type {
        self.register_svg_content(content, cache);
      }
    }

    for child in &fragment.children {
      self.collect_from_fragment(child, cache);
    }
  }

  fn register_svg_content(&mut self, content: &SvgContent, cache: &ImageCache) {
    for (id, filter) in collect_svg_filters(&content.svg, cache) {
      self.filters.entry(id).or_insert(filter);
    }
    if content.fallback_svg != content.svg {
      for (id, filter) in collect_svg_filters(&content.fallback_svg, cache) {
        self.filters.entry(id).or_insert(filter);
      }
    }
  }
}
