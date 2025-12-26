use crate::image_loader::ImageCache;
use crate::paint::svg_filter::{parse_svg_filter_from_svg_document, SvgFilter};
use crate::tree::box_tree::ReplacedType;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use roxmltree::Document;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Registry of document-local SVG filter definitions discovered in inline `<svg>` elements.
#[derive(Default)]
pub struct SvgFilterRegistry {
  definitions: HashMap<String, String>,
  cache: Mutex<HashMap<String, Arc<SvgFilter>>>,
}

impl Clone for SvgFilterRegistry {
  fn clone(&self) -> Self {
    let definitions = self.definitions.clone();
    let cache = if let Ok(guard) = self.cache.lock() {
      Mutex::new(guard.clone())
    } else {
      Mutex::new(HashMap::new())
    };
    Self { definitions, cache }
  }
}

impl SvgFilterRegistry {
  /// Build a registry by walking the fragment tree and collecting `<filter id="...">` definitions.
  ///
  /// Definitions are keyed by their `id` and store the full serialized SVG document string they
  /// were found in. Duplicate IDs keep the first occurrence in document order.
  pub fn from_fragment_tree(tree: &FragmentTree) -> Self {
    let mut definitions = HashMap::new();
    collect_filters(&tree.root, &mut definitions);
    for root in &tree.additional_fragments {
      collect_filters(root, &mut definitions);
    }
    Self {
      definitions,
      cache: Mutex::new(HashMap::new()),
    }
  }

  /// Build a registry from a single fragment root.
  pub fn from_fragment_root(root: &FragmentNode) -> Self {
    let mut definitions = HashMap::new();
    collect_filters(root, &mut definitions);
    Self {
      definitions,
      cache: Mutex::new(HashMap::new()),
    }
  }

  pub fn is_empty(&self) -> bool {
    self.definitions.is_empty()
  }

  /// Resolve a filter `id` against the registry, parsing it into an [`SvgFilter`] on first use.
  pub fn lookup(&self, id: &str, image_cache: &ImageCache) -> Option<Arc<SvgFilter>> {
    let id = id.strip_prefix('#').unwrap_or(id);
    if id.is_empty() {
      return None;
    }

    if let Ok(guard) = self.cache.lock() {
      if let Some(existing) = guard.get(id) {
        return Some(existing.clone());
      }
    }

    let svg = self.definitions.get(id)?;
    let parsed = parse_svg_filter_from_svg_document(svg, Some(id), image_cache)?;

    if let Ok(mut guard) = self.cache.lock() {
      guard.insert(id.to_string(), parsed.clone());
    }

    Some(parsed)
  }
}

fn collect_filters(node: &FragmentNode, definitions: &mut HashMap<String, String>) {
  if let FragmentContent::Replaced { replaced_type, .. } = &node.content {
    if let ReplacedType::Svg { content } = replaced_type {
      register_filters(&content.svg, definitions);
      if content.fallback_svg != content.svg {
        register_filters(&content.fallback_svg, definitions);
      }
    }
  }

  for child in &node.children {
    collect_filters(child, definitions);
  }
}

fn register_filters(svg: &str, definitions: &mut HashMap<String, String>) {
  if svg.trim().is_empty() {
    return;
  }

  let doc = match Document::parse(svg) {
    Ok(doc) => doc,
    Err(_) => return,
  };

  for node in doc.descendants().filter(|n| n.has_tag_name("filter")) {
    if let Some(id) = node.attribute("id") {
      if !id.is_empty() && !definitions.contains_key(id) {
        definitions.insert(id.to_string(), svg.to_string());
      }
    }
  }
}
