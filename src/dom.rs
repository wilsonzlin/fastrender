use crate::css::selectors::FastRenderSelectorImpl;
use crate::css::selectors::PseudoClass;
use crate::css::selectors::PseudoElement;
use crate::css::selectors::TextDirection;
use crate::css::types::CssString;
use crate::error::Error;
use crate::error::ParseError;
use crate::error::Result;
use html5ever::parse_document;
use html5ever::tendril::TendrilSink;
use html5ever::tree_builder::TreeBuilderOpts;
use html5ever::ParseOpts;
use markup5ever_rcdom::Handle;
use markup5ever_rcdom::NodeData;
use markup5ever_rcdom::RcDom;
use selectors::attr::AttrSelectorOperation;
use selectors::attr::CaseSensitivity;
use selectors::matching::matches_selector;
use selectors::matching::MatchingContext;
use selectors::parser::RelativeSelector;
use selectors::relative_selector::cache::RelativeSelectorCachedMatch;
use selectors::Element;
use selectors::OpaqueElement;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashSet;
use std::ptr;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::OnceLock;
use std::thread_local;
use unicode_bidi::bidi_class;

pub const HTML_NAMESPACE: &str = "http://www.w3.org/1999/xhtml";
pub const SVG_NAMESPACE: &str = "http://www.w3.org/2000/svg";
pub const MATHML_NAMESPACE: &str = "http://www.w3.org/1998/Math/MathML";

/// Controls whether non-standard DOM compatibility mutations are applied while parsing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DomCompatibilityMode {
  /// Parse the DOM without any FastRender-specific mutations.
  Standard,

  /// Apply compatibility mutations to mimic JS-driven class flips in static renders.
  Compatibility,
}

impl Default for DomCompatibilityMode {
  fn default() -> Self {
    Self::Standard
  }
}

/// Options for DOM parsing.
#[derive(Debug, Clone, Copy)]
pub struct DomParseOptions {
  /// Optional compatibility mutations applied after HTML parsing.
  pub compatibility_mode: DomCompatibilityMode,
}

impl Default for DomParseOptions {
  fn default() -> Self {
    Self {
      compatibility_mode: DomCompatibilityMode::Standard,
    }
  }
}

impl DomParseOptions {
  /// Enable compatibility DOM mutations (e.g., JS-managed class flips).
  pub fn compatibility() -> Self {
    Self {
      compatibility_mode: DomCompatibilityMode::Compatibility,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShadowRootMode {
  Open,
  Closed,
}

#[derive(Debug, Clone)]
pub struct DomNode {
  pub node_type: DomNodeType,
  pub children: Vec<DomNode>,
}

#[derive(Debug, Clone)]
pub enum DomNodeType {
  Document,
  ShadowRoot {
    mode: ShadowRootMode,
  },
  Slot {
    namespace: String,
    attributes: Vec<(String, String)>,
  },
  Element {
    tag_name: String,
    namespace: String,
    attributes: Vec<(String, String)>,
  },
  Text {
    content: String,
  },
}

thread_local! {
    static TARGET_FRAGMENT: RefCell<Option<String>> = const { RefCell::new(None) };
}

pub(crate) fn with_target_fragment<R, F: FnOnce() -> R>(target: Option<&str>, f: F) -> R {
  TARGET_FRAGMENT.with(|slot| {
    let previous = slot.borrow_mut().take();
    if let Some(t) = target {
      *slot.borrow_mut() = Some(t.trim_start_matches('#').to_string());
    }
    let result = f();
    *slot.borrow_mut() = previous;
    result
  })
}

fn current_target_fragment() -> Option<String> {
  TARGET_FRAGMENT.with(|slot| slot.borrow().clone())
}

static SELECTOR_BLOOM_ENV_INITIALIZED: OnceLock<()> = OnceLock::new();
static SELECTOR_BLOOM_ENABLED: AtomicBool = AtomicBool::new(true);

fn selector_bloom_enabled() -> bool {
  SELECTOR_BLOOM_ENV_INITIALIZED.get_or_init(|| {
    if let Ok(value) = std::env::var("FASTR_SELECTOR_BLOOM") {
      if value.trim() == "0" {
        SELECTOR_BLOOM_ENABLED.store(false, Ordering::Relaxed);
      }
    }
  });
  SELECTOR_BLOOM_ENABLED.load(Ordering::Relaxed)
}

/// Toggle selector bloom-filter insertion for benchmarking/testing.
pub fn set_selector_bloom_enabled(enabled: bool) {
  SELECTOR_BLOOM_ENV_INITIALIZED.get_or_init(|| ());
  SELECTOR_BLOOM_ENABLED.store(enabled, Ordering::Relaxed);
}

/// Resolve the first-strong direction within this subtree, skipping script/style contents.
pub fn resolve_first_strong_direction(node: &DomNode) -> Option<TextDirection> {
  let mut stack = vec![node];
  while let Some(current) = stack.pop() {
    match &current.node_type {
      DomNodeType::Text { content } => {
        for ch in content.chars() {
          match bidi_class(ch) {
            unicode_bidi::BidiClass::L => return Some(TextDirection::Ltr),
            unicode_bidi::BidiClass::R | unicode_bidi::BidiClass::AL => {
              return Some(TextDirection::Rtl)
            }
            _ => {}
          }
        }
      }
      DomNodeType::Element { tag_name, .. } => {
        let skip =
          tag_name.eq_ignore_ascii_case("script") || tag_name.eq_ignore_ascii_case("style");
        if skip {
          continue;
        }
        for child in &current.children {
          stack.push(child);
        }
      }
      DomNodeType::Slot { .. } => {
        for child in &current.children {
          stack.push(child);
        }
      }
      DomNodeType::ShadowRoot { .. } | DomNodeType::Document => {
        for child in &current.children {
          stack.push(child);
        }
      }
    }
  }
  None
}

fn node_is_hidden(attributes: &[(String, String)]) -> bool {
  attributes.iter().any(|(name, value)| {
    if name.eq_ignore_ascii_case("hidden") {
      return true;
    }
    if name.eq_ignore_ascii_case("aria-hidden") {
      return value.is_empty() || value.eq_ignore_ascii_case("true");
    }
    if name.eq_ignore_ascii_case("data-fastr-hidden") {
      return value.eq_ignore_ascii_case("true");
    }
    false
  })
}

fn node_is_inert_like(attributes: &[(String, String)]) -> bool {
  attributes.iter().any(|(name, value)| {
    if name.eq_ignore_ascii_case("inert") {
      return true;
    }
    if name.eq_ignore_ascii_case("data-fastr-inert") {
      return value.eq_ignore_ascii_case("true");
    }
    false
  })
}

/// Collects the set of Unicode codepoints present in text nodes.
///
/// Script and style contents are skipped to avoid counting non-visible text. Nodes marked as
/// hidden or inert are ignored along with their descendants to avoid fetching unnecessary font
/// subsets for invisible content.
pub fn collect_text_codepoints(node: &DomNode) -> Vec<u32> {
  let mut stack = vec![(node, false)];
  let mut seen: HashSet<u32> = HashSet::new();

  while let Some((current, suppressed)) = stack.pop() {
    if suppressed {
      continue;
    }
    match &current.node_type {
      DomNodeType::Text { content } => {
        for ch in content.chars() {
          seen.insert(ch as u32);
        }
      }
      DomNodeType::Element {
        tag_name,
        attributes,
        ..
      } => {
        let skip =
          tag_name.eq_ignore_ascii_case("script") || tag_name.eq_ignore_ascii_case("style");
        if skip {
          continue;
        }
        let suppress_children = node_is_hidden(attributes) || node_is_inert_like(attributes);
        for child in &current.children {
          stack.push((child, suppress_children));
        }
      }
      DomNodeType::Slot { attributes, .. } => {
        let suppress_children = node_is_hidden(attributes) || node_is_inert_like(attributes);
        for child in &current.children {
          stack.push((child, suppress_children));
        }
      }
      DomNodeType::ShadowRoot { .. } | DomNodeType::Document => {
        for child in &current.children {
          stack.push((child, false));
        }
      }
    }
  }

  let mut codepoints: Vec<u32> = seen.into_iter().collect();
  codepoints.sort_unstable();
  codepoints
}

pub fn parse_html(html: &str) -> Result<DomNode> {
  parse_html_with_options(html, DomParseOptions::default())
}

/// Parse HTML with explicit parsing options (e.g., DOM compatibility mode).
pub fn parse_html_with_options(html: &str, options: DomParseOptions) -> Result<DomNode> {
  let opts = ParseOpts {
    tree_builder: TreeBuilderOpts {
      scripting_enabled: false,
      ..Default::default()
    },
    ..Default::default()
  };

  let dom = parse_document(RcDom::default(), opts)
    .from_utf8()
    .read_from(&mut html.as_bytes())
    .map_err(|e| {
      Error::Parse(ParseError::InvalidHtml {
        message: format!("Failed to parse HTML: {}", e),
        line: 0,
      })
    })?;

  let mut root = convert_handle_to_node(&dom.document);
  attach_shadow_roots(&mut root);

  if matches!(
    options.compatibility_mode,
    DomCompatibilityMode::Compatibility
  ) {
    apply_dom_compatibility_mutations(&mut root);
  }

  Ok(root)
}

fn parse_shadow_root_mode(template: &DomNode) -> Option<ShadowRootMode> {
  if !template
    .tag_name()
    .map(|t| t.eq_ignore_ascii_case("template"))
    .unwrap_or(false)
  {
    return None;
  }

  // Declarative shadow DOM only applies to HTML templates, not e.g. SVG <template>.
  if !matches!(template.namespace(), Some(ns) if ns.is_empty() || ns == HTML_NAMESPACE) {
    return None;
  }

  let mode_attr = template
    .get_attribute_ref("shadowroot")
    .or_else(|| template.get_attribute_ref("shadowrootmode"))?;
  match mode_attr.to_ascii_lowercase().as_str() {
    "open" => Some(ShadowRootMode::Open),
    "closed" => Some(ShadowRootMode::Closed),
    _ => None,
  }
}

fn attach_shadow_roots(node: &mut DomNode) {
  for child in &mut node.children {
    attach_shadow_roots(child);
  }

  if !node.is_element() {
    return;
  }

  let mut shadow_template = None;
  for (idx, child) in node.children.iter().enumerate() {
    if let Some(mode) = parse_shadow_root_mode(child) {
      shadow_template = Some((idx, mode));
      break;
    }
  }

  let Some((template_idx, mode)) = shadow_template else {
    return;
  };

  let template = node.children.remove(template_idx);
  let mut shadow_root = DomNode {
    node_type: DomNodeType::ShadowRoot { mode },
    children: template.children,
  };

  let light_children = std::mem::take(&mut node.children);
  distribute_slots(&mut shadow_root, light_children);
  node.children = vec![shadow_root];
}

fn distribute_slots(shadow_root: &mut DomNode, light_children: Vec<DomNode>) {
  let mut available_slots: HashSet<String> = HashSet::new();
  collect_slot_names(shadow_root, &mut available_slots);

  let mut assignments: Vec<(Option<String>, DomNode)> = light_children
    .into_iter()
    .map(|child| {
      let slot_name = child
        .get_attribute_ref("slot")
        .map(|v| v.trim())
        .filter(|v| !v.is_empty())
        .map(|v| v.to_string());
      (slot_name, child)
    })
    .collect();

  fill_slot_tree(shadow_root, &mut assignments, &available_slots);
}

fn collect_slot_names(node: &DomNode, out: &mut HashSet<String>) {
  if matches!(node.node_type, DomNodeType::Slot { .. }) {
    out.insert(
      node
        .get_attribute_ref("name")
        .map(|v| v.to_string())
        .unwrap_or_default(),
    );
  }

  for child in &node.children {
    collect_slot_names(child, out);
  }
}

fn take_assignments_for_slot(
  assignments: &mut Vec<(Option<String>, DomNode)>,
  slot_name: &str,
  available_slots: &HashSet<String>,
) -> Vec<DomNode> {
  let mut taken = Vec::new();
  let mut remaining = Vec::with_capacity(assignments.len());

  for (name, node) in assignments.drain(..) {
    let target = name.as_deref().unwrap_or("");
    let matches = if slot_name.is_empty() {
      name.is_none() || !available_slots.contains(target)
    } else {
      target == slot_name
    };

    if matches {
      taken.push(node);
    } else {
      remaining.push((name, node));
    }
  }

  *assignments = remaining;
  taken
}

fn fill_slot_tree(
  node: &mut DomNode,
  assignments: &mut Vec<(Option<String>, DomNode)>,
  available_slots: &HashSet<String>,
) {
  if matches!(node.node_type, DomNodeType::Slot { .. }) {
    let slot_name = node.get_attribute_ref("name").unwrap_or("");
    let assigned = take_assignments_for_slot(assignments, slot_name, available_slots);
    if !assigned.is_empty() {
      node.children = assigned;
      return;
    }
  }

  for child in &mut node.children {
    fill_slot_tree(child, assignments, available_slots);
  }
}
/// Optional DOM compatibility tweaks applied after HTML parsing.
///
/// Some documents bootstrap by marking the root with `no-js` and replacing it with a
/// `js-enabled` class once scripts execute. Others toggle visibility gates like
/// `jsl10n-visible` after client-side localization. Since we do not run author scripts,
/// mirror those initializations so content that relies on the class flip (e.g., initial
/// opacity) is visible in static renders.
fn apply_dom_compatibility_mutations(node: &mut DomNode) {
  if let DomNodeType::Element {
    tag_name,
    attributes,
    ..
  } = &mut node.node_type
  {
    let mut classes: Vec<String> = attributes
      .iter()
      .find(|(k, _)| k.eq_ignore_ascii_case("class"))
      .map(|(_, v)| v.split_whitespace().map(|s| s.to_string()).collect())
      .unwrap_or_default();
    let mut changed = false;

    if tag_name.eq_ignore_ascii_case("html") {
      if classes.iter().any(|c| c == "no-js") {
        classes.retain(|c| c != "no-js");
        if !classes.iter().any(|c| c == "js-enabled") {
          classes.push("js-enabled".to_string());
        }
        changed = true;
      }
    }

    if tag_name.eq_ignore_ascii_case("html") || tag_name.eq_ignore_ascii_case("body") {
      if !classes.iter().any(|c| c == "jsl10n-visible") {
        classes.push("jsl10n-visible".to_string());
        changed = true;
      }
    }

    if changed {
      let class_value = classes.join(" ");
      if let Some((_, value)) = attributes
        .iter_mut()
        .find(|(k, _)| k.eq_ignore_ascii_case("class"))
      {
        *value = class_value;
      } else {
        attributes.push(("class".to_string(), class_value));
      }
    }
  }

  for child in &mut node.children {
    apply_dom_compatibility_mutations(child);
  }
}

fn convert_handle_to_node(handle: &Handle) -> DomNode {
  let node = handle;

  let node_type = match &node.data {
    NodeData::Document => DomNodeType::Document,
    NodeData::Element { name, attrs, .. } => {
      let tag_name = name.local.to_string();
      let namespace = name.ns.to_string();
      let attributes = attrs
        .borrow()
        .iter()
        .map(|attr| (attr.name.local.to_string(), attr.value.to_string()))
        .collect();

      if tag_name.eq_ignore_ascii_case("slot") {
        DomNodeType::Slot {
          namespace,
          attributes,
        }
      } else {
        DomNodeType::Element {
          tag_name,
          namespace,
          attributes,
        }
      }
    }
    NodeData::Text { contents } => {
      let content = contents.borrow().to_string();
      DomNodeType::Text { content }
    }
    _ => {
      // Skip comments, processing instructions, doctypes
      return DomNode {
        node_type: DomNodeType::Document,
        children: node
          .children
          .borrow()
          .iter()
          .map(convert_handle_to_node)
          .collect(),
      };
    }
  };

  let mut children: Vec<DomNode> = if let NodeData::Element {
    name,
    template_contents,
    ..
  } = &node.data
  {
    if name.local.as_ref().eq_ignore_ascii_case("template") {
      let borrowed = template_contents.borrow();
      match &*borrowed {
        Some(content) => content
          .children
          .borrow()
          .iter()
          .map(convert_handle_to_node)
          .collect(),
        None => Vec::new(),
      }
    } else {
      node
        .children
        .borrow()
        .iter()
        .map(convert_handle_to_node)
        .collect::<Vec<_>>()
    }
  } else {
    node
      .children
      .borrow()
      .iter()
      .map(convert_handle_to_node)
      .collect::<Vec<_>>()
  };

  // HTML <wbr> elements represent optional break opportunities. Synthesize a zero-width break
  // text node so line breaking can consider the opportunity while still allowing the element to
  // be styled/selected.
  if let DomNodeType::Element { tag_name, .. } = &node_type {
    if tag_name.eq_ignore_ascii_case("wbr") {
      children.push(DomNode {
        node_type: DomNodeType::Text {
          content: "\u{200B}".to_string(),
        },
        children: Vec::new(),
      });
    }
  }

  DomNode {
    node_type,
    children,
  }
}

impl DomNode {
  pub fn get_attribute_ref(&self, name: &str) -> Option<&str> {
    match &self.node_type {
      DomNodeType::Element { attributes, .. } => attributes
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case(name))
        .map(|(_, v)| v.as_str()),
      DomNodeType::Slot { attributes, .. } => attributes
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case(name))
        .map(|(_, v)| v.as_str()),
      _ => None,
    }
  }

  pub fn get_attribute(&self, name: &str) -> Option<String> {
    self.get_attribute_ref(name).map(|v| v.to_string())
  }

  pub fn tag_name(&self) -> Option<&str> {
    match &self.node_type {
      DomNodeType::Element { tag_name, .. } => Some(tag_name),
      DomNodeType::Slot { .. } => Some("slot"),
      _ => None,
    }
  }

  pub fn namespace(&self) -> Option<&str> {
    match &self.node_type {
      DomNodeType::Element { namespace, .. } => Some(namespace),
      DomNodeType::Slot { namespace, .. } => Some(namespace),
      _ => None,
    }
  }

  pub fn attributes_iter(&self) -> Box<dyn Iterator<Item = (&str, &str)> + '_> {
    match &self.node_type {
      DomNodeType::Element { attributes, .. } => {
        Box::new(attributes.iter().map(|(k, v)| (k.as_str(), v.as_str())))
      }
      DomNodeType::Slot { attributes, .. } => {
        Box::new(attributes.iter().map(|(k, v)| (k.as_str(), v.as_str())))
      }
      _ => Box::new(std::iter::empty()),
    }
  }

  pub fn is_element(&self) -> bool {
    matches!(
      self.node_type,
      DomNodeType::Element { .. } | DomNodeType::Slot { .. }
    )
  }

  pub fn is_text(&self) -> bool {
    matches!(self.node_type, DomNodeType::Text { .. })
  }

  pub fn text_content(&self) -> Option<&str> {
    match &self.node_type {
      DomNodeType::Text { content } => Some(content),
      _ => None,
    }
  }

  pub fn walk_tree<F>(&self, f: &mut F)
  where
    F: FnMut(&DomNode),
  {
    f(self);
    for child in &self.children {
      child.walk_tree(f);
    }
  }

  /// Get element children (skip text nodes)
  pub fn element_children(&self) -> Vec<&DomNode> {
    self.children.iter().filter(|c| c.is_element()).collect()
  }

  /// Check if this element has a specific class
  pub fn has_class(&self, class: &str) -> bool {
    if let Some(class_attr) = self.get_attribute_ref("class") {
      class_attr.split_whitespace().any(|c| c == class)
    } else {
      false
    }
  }

  /// Check if this element has a specific ID
  pub fn has_id(&self, id: &str) -> bool {
    self.get_attribute_ref("id") == Some(id)
  }
}

/// Wrapper for DomNode that implements Element trait for selector matching
/// This wrapper carries context needed for matching (parent, siblings)
#[derive(Debug, Clone, Copy)]
pub struct ElementRef<'a> {
  pub node: &'a DomNode,
  pub parent: Option<&'a DomNode>,
  all_ancestors: &'a [&'a DomNode],
}

impl<'a> ElementRef<'a> {
  pub fn new(node: &'a DomNode) -> Self {
    Self {
      node,
      parent: None,
      all_ancestors: &[],
    }
  }

  pub fn with_ancestors(node: &'a DomNode, ancestors: &'a [&'a DomNode]) -> Self {
    let parent = ancestors.last().copied();
    Self {
      node,
      parent,
      all_ancestors: ancestors,
    }
  }

  fn visited_flag(&self) -> bool {
    self
      .node
      .get_attribute_ref("data-fastr-visited")
      .map(|v| v.eq_ignore_ascii_case("true"))
      .unwrap_or(false)
  }

  fn active_flag(&self) -> bool {
    if self.inert_flag() {
      return false;
    }
    self
      .node
      .get_attribute_ref("data-fastr-active")
      .map(|v| v.eq_ignore_ascii_case("true"))
      .unwrap_or(false)
  }

  fn hover_flag(&self) -> bool {
    if self.inert_flag() {
      return false;
    }
    self
      .node
      .get_attribute_ref("data-fastr-hover")
      .map(|v| v.eq_ignore_ascii_case("true"))
      .unwrap_or(false)
  }

  fn node_is_inert(node: &DomNode) -> bool {
    matches!(
      node.node_type,
      DomNodeType::Element { .. } | DomNodeType::Slot { .. }
    ) && (node.get_attribute_ref("inert").is_some()
      || node
        .get_attribute_ref("data-fastr-inert")
        .map(|v| v.eq_ignore_ascii_case("true"))
        .unwrap_or(false))
  }

  fn inert_flag(&self) -> bool {
    if Self::node_is_inert(self.node) {
      return true;
    }
    self
      .all_ancestors
      .iter()
      .any(|ancestor| Self::node_is_inert(ancestor))
  }

  fn node_focus_flag(node: &DomNode) -> bool {
    if Self::node_is_inert(node) {
      return false;
    }
    if let DomNodeType::Element { namespace, .. } = &node.node_type {
      if namespace == SVG_NAMESPACE {
        let focusable = node
          .get_attribute_ref("focusable")
          .map(|v| v.eq_ignore_ascii_case("true"))
          .unwrap_or(false);
        if !focusable {
          return false;
        }
      }
    } else {
      return false;
    }

    node
      .get_attribute_ref("data-fastr-focus")
      .map(|v| v.eq_ignore_ascii_case("true"))
      .unwrap_or(false)
  }

  fn focus_flag(&self) -> bool {
    if self.inert_flag() {
      return false;
    }
    Self::node_focus_flag(self.node)
  }

  fn focus_visible_flag(&self) -> bool {
    if self.inert_flag() {
      return false;
    }
    if !Self::node_focus_flag(self.node) {
      return false;
    }

    self
      .node
      .get_attribute_ref("data-fastr-focus-visible")
      .map(|v| v.eq_ignore_ascii_case("true"))
      .unwrap_or(false)
  }

  fn subtree_contains_focus(&self) -> bool {
    if self.inert_flag() {
      return false;
    }
    Self::node_or_descendant_has_focus(self.node)
  }

  fn node_or_descendant_has_focus(node: &DomNode) -> bool {
    if Self::node_is_inert(node) {
      return false;
    }
    if Self::node_focus_flag(node) {
      return true;
    }

    node.children.iter().any(Self::node_or_descendant_has_focus)
  }

  fn subtree_has_content(node: &DomNode) -> bool {
    match node.node_type {
      DomNodeType::Text { .. } => true,
      DomNodeType::Element { .. } | DomNodeType::Slot { .. } => true,
      DomNodeType::ShadowRoot { .. } | DomNodeType::Document => {
        node.children.iter().any(Self::subtree_has_content)
      }
    }
  }

  /// Find index of this element among sibling elements and the total number of element siblings.
  fn element_index_and_len(&self) -> Option<(usize, usize)> {
    let parent = self.parent?;
    let mut index = None;
    let mut len = 0usize;
    for child in parent.children.iter() {
      if !child.is_element() {
        continue;
      }
      if ptr::eq(child, self.node) {
        index = Some(len);
      }
      len += 1;
    }
    index.map(|idx| (idx, len))
  }

  /// Find index of this element among siblings
  fn element_index(&self) -> Option<usize> {
    self.element_index_and_len().map(|(idx, _)| idx)
  }

  fn is_html_element(&self) -> bool {
    matches!(
      self.node.node_type,
      DomNodeType::Element { ref namespace, .. } | DomNodeType::Slot { ref namespace, .. }
        if namespace.is_empty() || namespace == HTML_NAMESPACE
    )
  }

  /// Position (index, total) among siblings filtered by a predicate.
  fn position_in_siblings<F>(&self, predicate: F) -> Option<(usize, usize)>
  where
    F: Fn(&DomNode) -> bool,
  {
    let parent = self.parent?;
    let mut index = None;
    let mut len = 0usize;
    for child in parent.children.iter() {
      if !child.is_element() || !predicate(child) {
        continue;
      }
      if ptr::eq(child, self.node) {
        index = Some(len);
      }
      len += 1;
    }
    index.map(|idx| (idx, len))
  }

  /// Position among siblings of the same element type (case-insensitive).
  fn position_in_type(&self) -> Option<(usize, usize)> {
    let tag = self.node.tag_name()?;
    let namespace = self.node.namespace();
    let is_html = self.is_html_element();
    self.position_in_siblings(|sibling| {
      if sibling.namespace() != namespace {
        return false;
      }
      sibling
        .tag_name()
        .map(|name| {
          if is_html {
            name.eq_ignore_ascii_case(tag)
          } else {
            name == tag
          }
        })
        .unwrap_or(false)
    })
  }

  fn position_in_selector_list(
    &self,
    selectors: &selectors::parser::SelectorList<FastRenderSelectorImpl>,
    context: &mut selectors::matching::MatchingContext<FastRenderSelectorImpl>,
  ) -> Option<(usize, usize)> {
    let parent = self.parent?;
    context.nest(|context| {
      let mut index = None;
      let mut len = 0usize;

      for child in parent.children.iter() {
        if !child.is_element() {
          continue;
        }
        let child_ref = ElementRef::with_ancestors(child, self.all_ancestors);
        let matches = selectors
          .slice()
          .iter()
          .any(|selector| matches_selector(selector, 0, None, &child_ref, context));
        if !matches {
          continue;
        }
        if ptr::eq(child, self.node) {
          index = Some(len);
        }
        len += 1;
      }

      index.map(|idx| (idx, len))
    })
  }

  /// Return the language of this element, inherited from ancestors if absent.
  fn language(&self) -> Option<String> {
    // Walk from self up through ancestors (closest first) for lang/xml:lang
    if let Some(lang) = self.lang_attribute(self.node) {
      return Some(lang);
    }

    for ancestor in self.all_ancestors.iter().rev() {
      if let Some(lang) = self.lang_attribute(ancestor) {
        return Some(lang);
      }
    }
    None
  }

  fn lang_attribute(&self, node: &DomNode) -> Option<String> {
    node
      .get_attribute_ref("lang")
      .or_else(|| node.get_attribute_ref("xml:lang"))
      .map(|l| l.to_ascii_lowercase())
  }

  fn supports_disabled(&self) -> bool {
    if !self.is_html_element() {
      return false;
    }
    self
      .node
      .tag_name()
      .map(|tag| match tag.to_ascii_lowercase().as_str() {
        "button" | "input" | "select" | "textarea" | "option" | "optgroup" | "fieldset" => true,
        _ => false,
      })
      .unwrap_or(false)
  }

  fn is_disabled(&self) -> bool {
    if let Some(tag) = self.node.tag_name() {
      let lower = tag.to_ascii_lowercase();

      if self.supports_disabled() && self.node.get_attribute_ref("disabled").is_some() {
        return true;
      }

      // Fieldset disabled state propagates to descendants except those inside the first legend.
      for (i, ancestor) in self.all_ancestors.iter().enumerate().rev() {
        if let Some(a_tag) = ancestor.tag_name() {
          if a_tag.eq_ignore_ascii_case("fieldset")
            && ancestor.get_attribute_ref("disabled").is_some()
          {
            // Find first legend child of this fieldset.
            let element_children = ancestor.element_children();
            let first_legend = element_children.iter().find(|child| {
              child
                .tag_name()
                .map(|t| t.eq_ignore_ascii_case("legend"))
                .unwrap_or(false)
            });

            if let Some(legend) = first_legend {
              // If we are inside this legend, the fieldset doesn't disable us.
              let in_legend = self
                .all_ancestors
                .get(i + 1..)
                .into_iter()
                .flatten()
                .any(|n| ptr::eq(*n, *legend));
              if in_legend {
                continue;
              }
            }

            return true;
          }
        }
      }

      if lower == "option" || lower == "optgroup" {
        for ancestor in self.all_ancestors.iter().rev() {
          if let Some(a_tag) = ancestor.tag_name() {
            let a_lower = a_tag.to_ascii_lowercase();
            if matches!(a_lower.as_str(), "select" | "optgroup" | "fieldset") {
              if ancestor.get_attribute_ref("disabled").is_some() {
                return true;
              }
            }
          }
        }
      }
    }

    false
  }

  fn is_contenteditable(&self) -> bool {
    if !self.is_html_element() {
      return false;
    }
    if let Some(value) = self.node.get_attribute_ref("contenteditable") {
      let v = value.to_ascii_lowercase();
      return v.is_empty() || v == "true";
    }
    false
  }

  fn is_text_editable_input(&self) -> bool {
    if !self.is_html_element() {
      return false;
    }
    let Some(tag) = self.node.tag_name() else {
      return false;
    };
    if !tag.eq_ignore_ascii_case("input") {
      return false;
    }

    let input_type = self
      .node
      .get_attribute_ref("type")
      .map(|t| t.to_ascii_lowercase());

    input_type
      .as_deref()
      .map(|t| {
        matches!(
          t,
          "text"
            | "search"
            | "url"
            | "tel"
            | "email"
            | "password"
            | "number"
            | "date"
            | "datetime-local"
            | "month"
            | "week"
            | "time"
        )
      })
      .unwrap_or(true)
  }

  fn is_option_selected(&self) -> bool {
    let explicitly_selected = self.node.get_attribute_ref("selected").is_some();
    if explicitly_selected {
      return true;
    }

    // Find the nearest select ancestor.
    let select = self.all_ancestors.iter().rev().copied().find(|ancestor| {
      ancestor
        .tag_name()
        .map(|t| t.eq_ignore_ascii_case("select"))
        .unwrap_or(false)
    });

    let Some(select_node) = select else {
      return false;
    };

    let is_multiple = select_node.get_attribute_ref("multiple").is_some();
    if is_multiple {
      // Multiple selects require explicit selection.
      return explicitly_selected;
    }

    // If any option under this select has an explicit selected attribute, only that one is selected.
    if select_has_explicit_selection(select_node) {
      return false;
    }

    // Otherwise, the first non-disabled option is selected by default.
    let first_option = first_enabled_option(select_node, false);
    matches!(first_option, Some(opt) if ptr::eq(opt, self.node))
  }

  fn is_checked(&self) -> bool {
    let Some(tag) = self.node.tag_name() else {
      return false;
    };

    if tag.eq_ignore_ascii_case("input") {
      let input_type = self
        .node
        .get_attribute_ref("type")
        .map(|t| t.to_ascii_lowercase());
      if matches!(input_type.as_deref(), Some("checkbox") | Some("radio")) {
        return self.node.get_attribute_ref("checked").is_some();
      }
      return false;
    }

    if tag.eq_ignore_ascii_case("option") {
      return self.is_option_selected();
    }

    false
  }

  fn is_read_write(&self) -> bool {
    if self.is_disabled() {
      return false;
    }

    if !self.is_html_element() {
      return false;
    }

    if self.is_text_editable_input() {
      return self.node.get_attribute_ref("readonly").is_none();
    }

    if let Some(tag) = self.node.tag_name() {
      if tag.eq_ignore_ascii_case("textarea") {
        return self.node.get_attribute_ref("readonly").is_none();
      }
      if tag.eq_ignore_ascii_case("select") {
        return true;
      }
    }

    self.is_contenteditable()
  }

  fn supports_required(&self) -> bool {
    if !self.is_html_element() {
      return false;
    }
    let Some(tag) = self.node.tag_name() else {
      return false;
    };

    let lower = tag.to_ascii_lowercase();
    match lower.as_str() {
      "select" | "textarea" => true,
      "input" => {
        let t = self
          .node
          .get_attribute_ref("type")
          .map(|s| s.to_ascii_lowercase())
          .unwrap_or_else(|| "text".to_string());

        !matches!(
          t.as_str(),
          "hidden" | "button" | "reset" | "submit" | "image"
        )
      }
      _ => false,
    }
  }

  fn is_required(&self) -> bool {
    self.supports_required()
      && !self.is_disabled()
      && self.node.get_attribute_ref("required").is_some()
  }

  fn supports_validation(&self) -> bool {
    if !self.is_html_element() {
      return false;
    }
    let Some(tag) = self.node.tag_name() else {
      return false;
    };
    let lower = tag.to_ascii_lowercase();
    match lower.as_str() {
      "textarea" | "select" => true,
      "input" => {
        let t = self
          .node
          .get_attribute_ref("type")
          .map(|s| s.to_ascii_lowercase())
          .unwrap_or_else(|| "text".to_string());
        !matches!(
          t.as_str(),
          "button" | "reset" | "submit" | "image" | "hidden"
        )
      }
      _ => false,
    }
  }

  fn control_value(&self) -> Option<String> {
    let tag = self.node.tag_name()?.to_ascii_lowercase();
    if tag == "textarea" {
      let mut combined = String::new();
      for child in &self.node.children {
        if let DomNodeType::Text { content } = &child.node_type {
          combined.push_str(content);
        }
      }
      return Some(combined);
    }
    if tag == "select" {
      return self.select_value();
    }
    if tag == "input" {
      return Some(
        self
          .node
          .get_attribute_ref("value")
          .map(|v| v.to_string())
          .unwrap_or_default(),
      );
    }
    None
  }

  fn select_value(&self) -> Option<String> {
    let multiple = self.node.get_attribute_ref("multiple").is_some();
    let explicit = find_selected_option_value(self.node, false);
    if explicit.is_some() {
      return explicit;
    }

    if multiple {
      return None;
    }

    first_enabled_option(self.node, false).map(option_value_from_node)
  }

  fn parse_number(value: &str) -> Option<f64> {
    value.trim().parse::<f64>().ok()
  }

  fn numeric_in_range(&self, value: f64) -> Option<bool> {
    let min = self
      .node
      .get_attribute_ref("min")
      .and_then(|m| Self::parse_number(m));
    let max = self
      .node
      .get_attribute_ref("max")
      .and_then(|m| Self::parse_number(m));

    if min.is_none() && max.is_none() {
      return None;
    }

    if let Some(min) = min {
      if value < min {
        return Some(false);
      }
    }
    if let Some(max) = max {
      if value > max {
        return Some(false);
      }
    }
    Some(true)
  }

  fn is_valid_control(&self) -> bool {
    if self.is_disabled() {
      return true;
    }
    if !self.supports_validation() {
      return false;
    }

    let Some(tag) = self.node.tag_name() else {
      return false;
    };
    let lower = tag.to_ascii_lowercase();

    let value = self.control_value().unwrap_or_default();

    if self.is_required() && value.trim().is_empty() {
      return false;
    }

    if lower == "select" {
      return true;
    }

    if lower == "textarea" {
      return true;
    }

    if lower == "input" {
      let input_type = self
        .node
        .get_attribute_ref("type")
        .map(|s| s.to_ascii_lowercase())
        .unwrap_or_else(|| "text".to_string());

      if matches!(
        input_type.as_str(),
        "text" | "search" | "url" | "tel" | "email" | "password"
      ) {
        return true;
      }

      if matches!(input_type.as_str(), "number" | "range") {
        if value.trim().is_empty() {
          return !self.is_required();
        }
        if let Some(num) = Self::parse_number(&value) {
          return self.numeric_in_range(num).unwrap_or(true);
        }
        return false;
      }

      if matches!(input_type.as_str(), "checkbox" | "radio") {
        if self.is_required() {
          return self.node.get_attribute_ref("checked").is_some();
        }
        return true;
      }

      return true;
    }

    true
  }

  fn range_state(&self) -> Option<bool> {
    let tag = self.node.tag_name()?;
    if !tag.eq_ignore_ascii_case("input") {
      return None;
    }
    let input_type = self
      .node
      .get_attribute_ref("type")
      .map(|s| s.to_ascii_lowercase())
      .unwrap_or_else(|| "text".to_string());
    if !matches!(input_type.as_str(), "number" | "range") {
      return None;
    }

    let value = self
      .node
      .get_attribute_ref("value")
      .map(|v| v.to_string())
      .unwrap_or_default();
    let num = Self::parse_number(&value)?;

    self.numeric_in_range(num)
  }

  fn is_indeterminate(&self) -> bool {
    let Some(tag) = self.node.tag_name() else {
      return false;
    };
    if tag.eq_ignore_ascii_case("input") {
      let input_type = self
        .node
        .get_attribute_ref("type")
        .map(|t| t.to_ascii_lowercase())
        .unwrap_or_else(|| "text".to_string());

      if input_type == "checkbox" {
        return self.node.get_attribute_ref("indeterminate").is_some();
      }
      return false;
    }

    if tag.eq_ignore_ascii_case("progress") {
      // Missing or invalid value makes progress indeterminate.
      let Some(value) = self.node.get_attribute_ref("value") else {
        return true;
      };
      return Self::parse_number(&value).is_none();
    }

    false
  }

  fn nearest_form(&self) -> Option<&DomNode> {
    self.all_ancestors.iter().rev().copied().find(|node| {
      node
        .tag_name()
        .map(|t| t.eq_ignore_ascii_case("form"))
        .unwrap_or(false)
    })
  }

  fn is_default_submit_candidate(node: &DomNode, ancestors: &[&DomNode]) -> bool {
    let Some(tag) = node.tag_name() else {
      return false;
    };
    let lower = tag.to_ascii_lowercase();

    let is_submit_input = lower == "input"
      && node
        .get_attribute_ref("type")
        .map(|t| t.eq_ignore_ascii_case("submit") || t.eq_ignore_ascii_case("image"))
        .unwrap_or(false);

    let is_button_submit = lower == "button"
      && node
        .get_attribute_ref("type")
        .map(|t| t.eq_ignore_ascii_case("submit"))
        .unwrap_or(true);

    if !(is_submit_input || is_button_submit) {
      return false;
    }

    let element_ref = ElementRef::with_ancestors(node, ancestors);
    !(element_ref.supports_disabled() && element_ref.is_disabled())
  }

  fn is_default_submit(&self) -> bool {
    let Some(form) = self.nearest_form() else {
      return false;
    };

    let mut ancestors = vec![form];
    let target = self.node as *const DomNode;

    fn traverse<'a>(
      node: &'a DomNode,
      ancestors: &mut Vec<&'a DomNode>,
      target: *const DomNode,
    ) -> Option<bool> {
      if ElementRef::is_default_submit_candidate(node, ancestors) {
        return Some(ptr::eq(node, target));
      }

      ancestors.push(node);
      for child in &node.children {
        if let Some(res) = traverse(child, ancestors, target) {
          ancestors.pop();
          return Some(res);
        }
      }
      ancestors.pop();
      None
    }

    traverse(form, &mut ancestors, target).unwrap_or(false)
  }

  /// Direction from dir/xml:dir attributes, inherited; defaults to LTR when none found.
  fn direction(&self) -> TextDirection {
    if let Some(dir) = self.dir_attribute(self.node, self.node) {
      return dir;
    }
    for ancestor in self.all_ancestors.iter().rev() {
      if let Some(dir) = self.dir_attribute(ancestor, ancestor) {
        return dir;
      }
    }
    TextDirection::Ltr
  }

  fn dir_attribute(&self, node: &DomNode, resolve_root: &DomNode) -> Option<TextDirection> {
    node
      .get_attribute_ref("dir")
      .or_else(|| node.get_attribute_ref("xml:dir"))
      .and_then(|d| match d.to_ascii_lowercase().as_str() {
        "ltr" => Some(TextDirection::Ltr),
        "rtl" => Some(TextDirection::Rtl),
        "auto" => resolve_first_strong_direction(resolve_root),
        _ => None,
      })
  }

  fn is_placeholder_shown(&self) -> bool {
    let Some(tag) = self.node.tag_name() else {
      return false;
    };

    if tag.eq_ignore_ascii_case("input") {
      if self.node.get_attribute_ref("placeholder").is_none() {
        return false;
      }

      let input_type = self
        .node
        .get_attribute_ref("type")
        .map(|t| t.to_ascii_lowercase());

      if !supports_placeholder(&input_type) {
        return false;
      }

      let value = self
        .node
        .get_attribute_ref("value")
        .map(|v| v.to_string())
        .unwrap_or_default();
      return value.is_empty();
    }

    if tag.eq_ignore_ascii_case("textarea") {
      if self.node.get_attribute_ref("placeholder").is_none() {
        return false;
      }

      let mut combined = String::new();
      for child in &self.node.children {
        if let DomNodeType::Text { content } = &child.node_type {
          combined.push_str(content);
        }
      }
      return combined.is_empty();
    }

    false
  }

  /// Expose the disabled state for accessibility mapping.
  pub(crate) fn accessibility_disabled(&self) -> bool {
    self.is_disabled()
  }

  /// Expose the checked state for accessibility mapping.
  pub(crate) fn accessibility_checked(&self) -> bool {
    self.is_checked()
  }

  /// Expose whether an option-like element is selected.
  pub(crate) fn accessibility_selected(&self) -> bool {
    self.is_option_selected()
  }

  /// Expose whether the control is indeterminate (checkbox/progress).
  pub(crate) fn accessibility_indeterminate(&self) -> bool {
    self.is_indeterminate()
  }

  /// Expose whether the control is required.
  pub(crate) fn accessibility_required(&self) -> bool {
    self.is_required()
  }

  /// Expose whether the control supports constraint validation.
  pub(crate) fn accessibility_supports_validation(&self) -> bool {
    self.supports_validation()
  }

  /// Expose whether the control is valid according to HTML form rules.
  pub(crate) fn accessibility_is_valid(&self) -> bool {
    self.is_valid_control()
  }

  /// Expose control value for form controls (input/select/textarea).
  pub(crate) fn accessibility_value(&self) -> Option<String> {
    self.control_value()
  }

  fn is_target(&self) -> bool {
    current_target_fragment()
      .as_deref()
      .map(|target| Self::node_matches_target(self.node, target))
      .unwrap_or(false)
  }

  fn subtree_contains_target(&self) -> bool {
    let Some(target) = current_target_fragment() else {
      return false;
    };
    Self::subtree_has_target(self.node, target.as_str())
  }

  fn subtree_has_target(node: &DomNode, target: &str) -> bool {
    if Self::node_matches_target(node, target) {
      return true;
    }
    node
      .children
      .iter()
      .any(|child| Self::subtree_has_target(child, target))
  }

  fn node_matches_target(node: &DomNode, target: &str) -> bool {
    if let Some(id) = node.get_attribute_ref("id") {
      if id == target {
        return true;
      }
    }

    if let Some(tag) = node.tag_name() {
      let lower = tag.to_ascii_lowercase();
      if matches!(lower.as_str(), "a" | "area") {
        if let Some(name) = node.get_attribute_ref("name") {
          if name == target {
            return true;
          }
        }
      }
    }

    false
  }
}

fn matches_an_plus_b(a: i32, b: i32, position: i32) -> bool {
  if a == 0 {
    position == b
  } else {
    let diff = position - b;
    diff % a == 0 && diff / a >= 0
  }
}

fn lang_matches(range: &str, lang: &str) -> bool {
  if range == "*" {
    return true;
  }
  if range.eq_ignore_ascii_case(lang) {
    return true;
  }
  // Prefix match with boundary
  lang.starts_with(range) && lang.as_bytes().get(range.len()) == Some(&b'-')
}

fn supports_placeholder(input_type: &Option<String>) -> bool {
  let Some(t) = input_type.as_ref() else {
    return true;
  };

  match t.as_str() {
    // Per HTML spec, placeholder is supported for text-like controls; unknown types default to text.
    "text" | "search" | "url" | "tel" | "email" | "password" | "number" => true,
    "hidden" | "submit" | "reset" | "button" | "image" | "file" | "checkbox" | "radio"
    | "range" | "color" | "date" | "datetime-local" | "month" | "week" | "time" => false,
    _ => true,
  }
}

fn select_has_explicit_selection(select: &DomNode) -> bool {
  let mut found = false;
  select.walk_tree(&mut |node| {
    if found {
      return;
    }
    if let Some(tag) = node.tag_name() {
      if tag.eq_ignore_ascii_case("option") && node.get_attribute_ref("selected").is_some() {
        found = true;
      }
    }
  });
  found
}

fn option_value_from_node(node: &DomNode) -> String {
  if let DomNodeType::Element { attributes, .. } = &node.node_type {
    if let Some((_, v)) = attributes
      .iter()
      .find(|(k, _)| k.eq_ignore_ascii_case("value"))
    {
      return v.clone();
    }
  }

  node
    .children
    .iter()
    .filter_map(|c| match &c.node_type {
      DomNodeType::Text { content } => Some(content.clone()),
      _ => None,
    })
    .collect()
}

fn find_selected_option_value(node: &DomNode, optgroup_disabled: bool) -> Option<String> {
  let tag = node.tag_name().map(|t| t.to_ascii_lowercase());
  let is_option = tag.as_deref() == Some("option");

  let option_disabled = node.get_attribute_ref("disabled").is_some();
  let next_optgroup_disabled =
    optgroup_disabled || (tag.as_deref() == Some("optgroup") && option_disabled);

  if is_option
    && node.get_attribute_ref("selected").is_some()
    && !(option_disabled || optgroup_disabled)
  {
    return Some(option_value_from_node(node));
  }

  for child in &node.children {
    if let Some(val) = find_selected_option_value(child, next_optgroup_disabled) {
      return Some(val);
    }
  }
  None
}

fn first_enabled_option<'a>(node: &'a DomNode, optgroup_disabled: bool) -> Option<&'a DomNode> {
  let tag = node.tag_name().map(|t| t.to_ascii_lowercase());
  let is_option = tag.as_deref() == Some("option");

  let option_disabled = node.get_attribute_ref("disabled").is_some();
  let next_optgroup_disabled =
    optgroup_disabled || (tag.as_deref() == Some("optgroup") && option_disabled);

  if is_option && !(option_disabled || optgroup_disabled) {
    return Some(node);
  }

  for child in &node.children {
    if let Some(opt) = first_enabled_option(child, next_optgroup_disabled) {
      return Some(opt);
    }
  }
  None
}

impl<'a> Element for ElementRef<'a> {
  type Impl = FastRenderSelectorImpl;

  fn opaque(&self) -> OpaqueElement {
    OpaqueElement::new(self.node)
  }

  fn parent_element(&self) -> Option<Self> {
    let parent = self.parent?;
    if !parent.is_element() {
      return None;
    }

    Some(if self.all_ancestors.len() > 1 {
      // If we have multiple ancestors, the parent's ancestors are all but the last
      ElementRef::with_ancestors(parent, &self.all_ancestors[..self.all_ancestors.len() - 1])
    } else {
      // Parent is the root
      ElementRef::new(parent)
    })
  }

  fn parent_node_is_shadow_root(&self) -> bool {
    matches!(
      self.parent.map(|p| &p.node_type),
      Some(DomNodeType::ShadowRoot { .. })
    )
  }

  fn containing_shadow_host(&self) -> Option<Self> {
    for (idx, ancestor) in self.all_ancestors.iter().enumerate().rev() {
      if matches!(ancestor.node_type, DomNodeType::ShadowRoot { .. }) {
        if idx == 0 {
          return None;
        }
        let host = self.all_ancestors[idx - 1];
        return Some(ElementRef::with_ancestors(
          host,
          &self.all_ancestors[..idx - 1],
        ));
      }
    }
    None
  }

  fn is_pseudo_element(&self) -> bool {
    false
  }

  fn prev_sibling_element(&self) -> Option<Self> {
    let parent = self.parent?;
    let mut prev: Option<&DomNode> = None;
    for child in parent.children.iter() {
      if !child.is_element() {
        continue;
      }
      if ptr::eq(child, self.node) {
        return prev.map(|node| ElementRef {
          node,
          parent: self.parent,
          all_ancestors: self.all_ancestors,
        });
      }
      prev = Some(child);
    }
    None
  }

  fn next_sibling_element(&self) -> Option<Self> {
    let parent = self.parent?;
    let mut seen_self = false;
    for child in parent.children.iter() {
      if !child.is_element() {
        continue;
      }
      if seen_self {
        return Some(ElementRef {
          node: child,
          parent: self.parent,
          all_ancestors: self.all_ancestors,
        });
      }
      if ptr::eq(child, self.node) {
        seen_self = true;
      }
    }
    None
  }

  fn is_html_element_in_html_document(&self) -> bool {
    match &self.node.node_type {
      DomNodeType::Element { namespace, .. } | DomNodeType::Slot { namespace, .. } => {
        namespace.is_empty() || namespace == HTML_NAMESPACE
      }
      _ => false,
    }
  }

  fn has_local_name(&self, local_name: &str) -> bool {
    self.node.tag_name().is_some_and(|tag| {
      if self.is_html_element() {
        tag.eq_ignore_ascii_case(local_name)
      } else {
        tag == local_name
      }
    })
  }

  fn has_namespace(&self, ns: &str) -> bool {
    match &self.node.node_type {
      DomNodeType::Element { namespace, .. } | DomNodeType::Slot { namespace, .. } => {
        if ns.is_empty() {
          return true;
        }
        if namespace == ns {
          return true;
        }
        namespace.is_empty() && ns == HTML_NAMESPACE
      }
      _ => false,
    }
  }

  fn is_same_type(&self, other: &Self) -> bool {
    match (&self.node.node_type, &other.node.node_type) {
      (
        DomNodeType::Element {
          tag_name: a,
          namespace: a_ns,
          ..
        },
        DomNodeType::Element {
          tag_name: b,
          namespace: b_ns,
          ..
        },
      ) if a_ns == b_ns => {
        if a_ns == HTML_NAMESPACE || a_ns.is_empty() {
          a.eq_ignore_ascii_case(b)
        } else {
          a == b
        }
      }
      (
        DomNodeType::Slot {
          namespace: a_ns, ..
        },
        DomNodeType::Slot {
          namespace: b_ns, ..
        },
      ) if a_ns == b_ns => true,
      _ => false,
    }
  }

  fn attr_matches(
    &self,
    ns: &selectors::attr::NamespaceConstraint<&CssString>,
    local_name: &CssString,
    operation: &AttrSelectorOperation<&CssString>,
  ) -> bool {
    // Namespace check: we only support HTML namespace/none.
    match ns {
      selectors::attr::NamespaceConstraint::Any => {}
      selectors::attr::NamespaceConstraint::Specific(url) => {
        let url: &str = (*url).borrow();
        if !(url.is_empty() || url == HTML_NAMESPACE) {
          return false;
        }
      }
    }

    let attr_value = match self.node.get_attribute_ref(local_name.as_str()) {
      Some(v) => v,
      None => return false,
    };

    match operation {
      AttrSelectorOperation::Exists => true,
      AttrSelectorOperation::WithValue {
        operator,
        case_sensitivity,
        value,
      } => {
        let value_str: &str = std::borrow::Borrow::borrow(&**value);
        operator.eval_str(attr_value, value_str, *case_sensitivity)
      }
    }
  }

  fn match_non_ts_pseudo_class(
    &self,
    pseudo: &PseudoClass,
    _context: &mut selectors::matching::MatchingContext<Self::Impl>,
  ) -> bool {
    match pseudo {
      PseudoClass::Has(relative) => {
        if _context.relative_selector_anchor().is_some() {
          return false;
        }
        matches_has_relative(self, relative, _context)
      }
      PseudoClass::Root => {
        matches!(self.node.namespace(), Some(ns) if ns.is_empty() || ns == HTML_NAMESPACE)
          && self
            .node
            .tag_name()
            .map(|t| t.eq_ignore_ascii_case("html"))
            .unwrap_or(false)
      }
      PseudoClass::FirstChild => self.element_index() == Some(0),
      PseudoClass::LastChild => self
        .element_index_and_len()
        .map(|(idx, len)| idx == len.saturating_sub(1))
        .unwrap_or(false),
      PseudoClass::OnlyChild => self
        .element_index_and_len()
        .map(|(_, len)| len == 1)
        .unwrap_or(false),
      PseudoClass::NthChild(a, b, of) => match of {
        Some(selectors) => self
          .position_in_selector_list(selectors, _context)
          .map(|(index, _)| matches_an_plus_b(*a, *b, (index + 1) as i32))
          .unwrap_or(false),
        None => self
          .element_index()
          .map(|index| matches_an_plus_b(*a, *b, (index + 1) as i32))
          .unwrap_or(false),
      },
      PseudoClass::NthLastChild(a, b, of) => match of {
        Some(selectors) => self
          .position_in_selector_list(selectors, _context)
          .map(|(index, len)| {
            let n = (len - index) as i32;
            matches_an_plus_b(*a, *b, n)
          })
          .unwrap_or(false),
        None => self
          .element_index_and_len()
          .map(|(index, len)| {
            let n = (len - index) as i32;
            matches_an_plus_b(*a, *b, n)
          })
          .unwrap_or(false),
      },
      PseudoClass::FirstOfType => self
        .position_in_type()
        .map(|(index, _)| index == 0)
        .unwrap_or(false),
      PseudoClass::LastOfType => self
        .position_in_type()
        .map(|(index, len)| index == len.saturating_sub(1))
        .unwrap_or(false),
      PseudoClass::OnlyOfType => self
        .position_in_type()
        .map(|(_, len)| len == 1)
        .unwrap_or(false),
      PseudoClass::NthOfType(a, b) => self
        .position_in_type()
        .map(|(index, _)| matches_an_plus_b(*a, *b, (index + 1) as i32))
        .unwrap_or(false),
      PseudoClass::NthLastOfType(a, b) => self
        .position_in_type()
        .map(|(index, len)| {
          let n = (len - index) as i32;
          matches_an_plus_b(*a, *b, n)
        })
        .unwrap_or(false),
      PseudoClass::Lang(langs) => {
        if let Some(lang) = self.language() {
          langs.iter().any(|range| lang_matches(range, &lang))
        } else {
          false
        }
      }
      PseudoClass::Dir(dir) => self.direction() == *dir,
      PseudoClass::AnyLink => self.is_link(),
      PseudoClass::Target => self.is_target(),
      PseudoClass::TargetWithin => self.subtree_contains_target(),
      PseudoClass::Scope => match _context.relative_selector_anchor() {
        Some(anchor) => anchor == self.opaque(),
        None => !self
          .all_ancestors
          .iter()
          .any(|ancestor| ancestor.is_element()),
      },
      PseudoClass::Empty => self.is_empty(),
      PseudoClass::Disabled => self.supports_disabled() && self.is_disabled(),
      PseudoClass::Enabled => self.supports_disabled() && !self.is_disabled(),
      PseudoClass::Required => self.is_required(),
      PseudoClass::Optional => {
        self.supports_required() && !self.is_disabled() && !self.is_required()
      }
      PseudoClass::Valid => {
        (self.supports_validation() && self.is_disabled())
          || (self.supports_validation() && self.is_valid_control())
      }
      PseudoClass::Invalid => {
        self.supports_validation() && !self.is_disabled() && !self.is_valid_control()
      }
      PseudoClass::InRange => !self.is_disabled() && self.range_state() == Some(true),
      PseudoClass::OutOfRange => !self.is_disabled() && self.range_state() == Some(false),
      PseudoClass::Indeterminate => self.is_indeterminate(),
      PseudoClass::Default => {
        if let Some(tag) = self.node.tag_name() {
          let lower = tag.to_ascii_lowercase();
          if lower == "option" {
            return self.is_checked();
          }
          if lower == "input" {
            let t = self
              .node
              .get_attribute_ref("type")
              .map(|s| s.to_ascii_lowercase())
              .unwrap_or_else(|| "text".to_string());
            if t == "checkbox" || t == "radio" {
              return self.node.get_attribute_ref("checked").is_some();
            }
          }
          if matches!(lower.as_str(), "input" | "button") {
            return self.is_default_submit();
          }
        }
        false
      }
      PseudoClass::ReadOnly => !self.is_read_write(),
      PseudoClass::ReadWrite => self.is_read_write(),
      PseudoClass::PlaceholderShown => self.is_placeholder_shown(),
      PseudoClass::Autofill => false,
      // Interactive pseudo-classes (not supported in static rendering)
      PseudoClass::Hover => self.hover_flag(),
      PseudoClass::Focus => self.focus_flag(),
      PseudoClass::FocusWithin => self.subtree_contains_focus(),
      PseudoClass::FocusVisible => self.focus_visible_flag(),
      PseudoClass::Active => self.active_flag(),
      PseudoClass::Checked => self.is_checked(),
      PseudoClass::Link => self.is_link() && !self.visited_flag(),
      PseudoClass::Visited => self.is_link() && self.visited_flag(),
    }
  }

  fn match_pseudo_element(
    &self,
    pseudo: &PseudoElement,
    _context: &mut selectors::matching::MatchingContext<Self::Impl>,
  ) -> bool {
    match pseudo {
      // These pseudo-elements are supported for all elements; filtering
      // based on box generation happens later in the pipeline.
      PseudoElement::Before
      | PseudoElement::After
      | PseudoElement::FirstLine
      | PseudoElement::FirstLetter
      | PseudoElement::Marker
      | PseudoElement::Backdrop => true,
    }
  }

  fn is_link(&self) -> bool {
    let Some(tag) = self.node.tag_name() else {
      return false;
    };
    let has_href = self.node.get_attribute_ref("href").is_some();
    has_href && matches!(tag.to_ascii_lowercase().as_str(), "a" | "area" | "link")
  }

  fn is_html_slot_element(&self) -> bool {
    self
      .node
      .tag_name()
      .is_some_and(|t| t.eq_ignore_ascii_case("slot"))
  }

  fn has_id(&self, id: &CssString, case_sensitivity: CaseSensitivity) -> bool {
    match case_sensitivity {
      CaseSensitivity::CaseSensitive => self.node.has_id(id.as_str()),
      CaseSensitivity::AsciiCaseInsensitive => self
        .node
        .get_attribute_ref("id")
        .map(|attr| attr.eq_ignore_ascii_case(id.as_str()))
        .unwrap_or(false),
    }
  }

  fn has_class(&self, class: &CssString, case_sensitivity: CaseSensitivity) -> bool {
    match case_sensitivity {
      CaseSensitivity::CaseSensitive => self.node.has_class(class.as_str()),
      CaseSensitivity::AsciiCaseInsensitive => self
        .node
        .get_attribute_ref("class")
        .map(|classes| {
          classes
            .split_whitespace()
            .any(|c| c.eq_ignore_ascii_case(class.as_str()))
        })
        .unwrap_or(false),
    }
  }

  fn imported_part(&self, _name: &CssString) -> Option<CssString> {
    None
  }

  fn is_part(&self, _name: &CssString) -> bool {
    false
  }

  fn is_empty(&self) -> bool {
    !self.node.children.iter().any(Self::subtree_has_content)
  }

  fn is_root(&self) -> bool {
    matches!(self.node.tag_name(), Some("html"))
  }

  fn first_element_child(&self) -> Option<Self> {
    // We don't support this for now - static rendering doesn't need it
    None
  }

  fn apply_selector_flags(&self, _flags: selectors::matching::ElementSelectorFlags) {
    // We don't track selector flags for static rendering
  }

  fn has_custom_state(&self, _name: &CssString) -> bool {
    // We don't support custom states
    false
  }

  fn add_element_unique_hashes(
    &self,
    filter: &mut selectors::bloom::CountingBloomFilter<selectors::bloom::BloomStorageU8>,
  ) -> bool {
    use selectors::bloom;

    if !selector_bloom_enabled() {
      return true;
    }

    let mut insert = |value: &str| {
      use std::collections::hash_map::DefaultHasher;
      use std::hash::{Hash, Hasher};

      let mut hasher = DefaultHasher::new();
      value.hash(&mut hasher);
      let hash = (hasher.finish() as u32) & bloom::BLOOM_HASH_MASK;
      filter.insert_hash(hash);
    };

    if let Some(tag) = self.node.tag_name() {
      let is_html = self.is_html_element();
      let tag_key = if is_html {
        tag.to_ascii_lowercase()
      } else {
        tag.to_string()
      };
      insert(&tag_key);
      if is_html && tag != tag_key {
        insert(tag);
      }
    }

    if let Some(id) = self.node.get_attribute_ref("id") {
      insert(id);
    }

    if let Some(class_attr) = self.node.get_attribute_ref("class") {
      for class in class_attr.split_whitespace() {
        if !class.is_empty() {
          insert(class);
        }
      }
    }

    // Attribute presence hashes to allow pruning on [attr] selectors.
    for (name, _) in self.node.attributes_iter() {
      insert(name);
      let lower = name.to_ascii_lowercase();
      if lower != name {
        insert(&lower);
      }
    }

    true
  }
}

struct RelativeSelectorAncestorStack<'a> {
  baseline: &'a [&'a DomNode],
  nodes: Vec<&'a DomNode>,
  baseline_len: usize,
}

impl<'a> RelativeSelectorAncestorStack<'a> {
  fn new(baseline: &'a [&'a DomNode]) -> Self {
    let baseline_len = baseline.len();
    Self {
      baseline,
      nodes: Vec::new(),
      baseline_len,
    }
  }

  fn ensure_materialized(&mut self) {
    if self.nodes.is_empty() {
      self.nodes.reserve(self.baseline_len.saturating_add(8));
      self.nodes.extend_from_slice(self.baseline);
    }
  }

  fn as_slice(&self) -> &[&'a DomNode] {
    if self.nodes.is_empty() {
      self.baseline
    } else {
      &self.nodes
    }
  }

  fn parent(&self) -> Option<&'a DomNode> {
    if self.nodes.is_empty() {
      self.baseline.last().copied()
    } else {
      self.nodes.last().copied()
    }
  }

  fn push(&mut self, node: &'a DomNode) {
    self.ensure_materialized();
    self.nodes.push(node);
  }

  fn pop(&mut self) -> Option<&'a DomNode> {
    self.nodes.pop()
  }

  fn reset(&mut self) {
    if !self.nodes.is_empty() {
      self.nodes.truncate(self.baseline_len);
    }
  }

  fn len(&self) -> usize {
    if self.nodes.is_empty() {
      self.baseline_len
    } else {
      self.nodes.len()
    }
  }

  fn baseline_len(&self) -> usize {
    self.baseline_len
  }

  fn with_pushed<F, R>(&mut self, node: &'a DomNode, f: F) -> R
  where
    F: FnOnce(&mut Self) -> R,
  {
    self.push(node);
    let res = f(self);
    let popped = self.pop();
    debug_assert!(popped.is_some());
    res
  }
}

fn matches_has_relative(
  anchor: &ElementRef,
  selectors: &[RelativeSelector<FastRenderSelectorImpl>],
  context: &mut MatchingContext<FastRenderSelectorImpl>,
) -> bool {
  if selectors.is_empty() {
    return false;
  }

  context.nest_for_relative_selector(anchor.opaque(), |ctx| {
    let mut ancestors = RelativeSelectorAncestorStack::new(anchor.all_ancestors);

    for selector in selectors.iter() {
      if let Some(cached) = ctx
        .selector_caches
        .relative_selector
        .lookup(anchor.opaque(), selector)
      {
        if cached.matched() {
          return true;
        }
        continue;
      }

      let matched = match_relative_selector(selector, anchor.node, &mut ancestors, ctx);
      debug_assert_eq!(ancestors.len(), ancestors.baseline_len());
      ancestors.reset();

      ctx.selector_caches.relative_selector.add(
        anchor.opaque(),
        selector,
        if matched {
          RelativeSelectorCachedMatch::Matched
        } else {
          RelativeSelectorCachedMatch::NotMatched
        },
      );

      if matched {
        return true;
      }
    }
    false
  })
}

// RelativeSelectorMatchHint guides traversal here:
// - is_descendant_direction() => combinators keep moving downward (descendant or child),
//   otherwise we only need to consider following siblings.
// - is_subtree() => a match may occur inside the candidate's subtree, so we recurse;
//   when false we only try the candidate itself.
// - is_next_sibling() => only the immediate following sibling can match.
fn match_relative_selector<'a>(
  selector: &RelativeSelector<FastRenderSelectorImpl>,
  anchor: &'a DomNode,
  ancestors: &mut RelativeSelectorAncestorStack<'a>,
  context: &mut MatchingContext<FastRenderSelectorImpl>,
) -> bool {
  if !anchor.is_element() {
    return false;
  }

  if selector.match_hint.is_descendant_direction() {
    return match_relative_selector_descendants(selector, anchor, ancestors, context);
  }

  match_relative_selector_siblings(selector, anchor, ancestors, context)
}

fn match_relative_selector_descendants<'a>(
  selector: &RelativeSelector<FastRenderSelectorImpl>,
  anchor: &'a DomNode,
  ancestors: &mut RelativeSelectorAncestorStack<'a>,
  context: &mut MatchingContext<FastRenderSelectorImpl>,
) -> bool {
  ancestors.with_pushed(anchor, |ancestors| {
    for child in anchor.children.iter().filter(|c| c.is_element()) {
      let child_ref = ElementRef::with_ancestors(child, ancestors.as_slice());
      let mut matched = matches_selector(&selector.selector, 0, None, &child_ref, context);
      if !matched && selector.match_hint.is_subtree() {
        matched = match_relative_selector_subtree(selector, child, ancestors, context);
      }
      if matched {
        return true;
      }
    }
    false
  })
}

fn match_relative_selector_siblings<'a>(
  selector: &RelativeSelector<FastRenderSelectorImpl>,
  anchor: &'a DomNode,
  ancestors: &mut RelativeSelectorAncestorStack<'a>,
  context: &mut MatchingContext<FastRenderSelectorImpl>,
) -> bool {
  let parent = match ancestors.parent() {
    Some(p) => p,
    None => return false,
  };

  let mut seen_anchor = false;
  for sibling in parent.children.iter().filter(|c| c.is_element()) {
    if ptr::eq(sibling, anchor) {
      seen_anchor = true;
      continue;
    }
    if !seen_anchor {
      continue;
    }

    let sibling_ref = ElementRef::with_ancestors(sibling, ancestors.as_slice());
    let matched = if selector.match_hint.is_subtree() {
      match_relative_selector_subtree(selector, sibling, ancestors, context)
    } else {
      matches_selector(&selector.selector, 0, None, &sibling_ref, context)
    };

    if matched {
      return true;
    }

    if selector.match_hint.is_next_sibling() {
      break;
    }
  }

  false
}

fn match_relative_selector_subtree<'a>(
  selector: &RelativeSelector<FastRenderSelectorImpl>,
  node: &'a DomNode,
  ancestors: &mut RelativeSelectorAncestorStack<'a>,
  context: &mut MatchingContext<FastRenderSelectorImpl>,
) -> bool {
  debug_assert!(selector.match_hint.is_subtree());

  ancestors.with_pushed(node, |ancestors| {
    for child in node.children.iter().filter(|c| c.is_element()) {
      let child_ref = ElementRef::with_ancestors(child, ancestors.as_slice());
      if matches_selector(&selector.selector, 0, None, &child_ref, context) {
        return true;
      }
      if match_relative_selector_subtree(selector, child, ancestors, context) {
        return true;
      }
    }
    false
  })
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::css::selectors::PseudoClassParser;
  use cssparser::{Parser, ParserInput};
  use selectors::matching::MatchingContext;
  use selectors::matching::MatchingForInvalidation;
  use selectors::matching::MatchingMode;
  use selectors::matching::NeedsSelectorFlags;
  use selectors::matching::QuirksMode;
  use selectors::matching::SelectorCaches;
  use selectors::parser::ParseRelative;
  use selectors::parser::Selector;
  use selectors::parser::SelectorList;

  fn element(tag: &str, children: Vec<DomNode>) -> DomNode {
    DomNode {
      node_type: DomNodeType::Element {
        tag_name: tag.to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children,
    }
  }

  fn svg_element(tag: &str) -> DomNode {
    DomNode {
      node_type: DomNodeType::Element {
        tag_name: tag.to_string(),
        namespace: SVG_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    }
  }

  fn text(content: &str) -> DomNode {
    DomNode {
      node_type: DomNodeType::Text {
        content: content.to_string(),
      },
      children: vec![],
    }
  }

  fn find_element_by_id<'a>(node: &'a DomNode, id: &str) -> Option<&'a DomNode> {
    if let DomNodeType::Element { attributes, .. } = &node.node_type {
      if attributes
        .iter()
        .any(|(name, value)| name.eq_ignore_ascii_case("id") && value == id)
      {
        return Some(node);
      }
    }
    for child in &node.children {
      if let Some(found) = find_element_by_id(child, id) {
        return Some(found);
      }
    }
    None
  }

  fn matches(node: &DomNode, ancestors: &[&DomNode], pseudo: &PseudoClass) -> bool {
    let mut caches = SelectorCaches::default();
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );
    let element_ref = ElementRef::with_ancestors(node, ancestors);
    element_ref.match_non_ts_pseudo_class(pseudo, &mut context)
  }

  fn parse_selector(selector: &str) -> Selector<FastRenderSelectorImpl> {
    let mut input = ParserInput::new(selector);
    let mut parser = Parser::new(&mut input);
    SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::No)
      .expect("selector should parse")
      .slice()
      .first()
      .expect("selector list should have at least one selector")
      .clone()
  }

  fn selector_matches(element: &ElementRef, selector: &Selector<FastRenderSelectorImpl>) -> bool {
    let mut caches = SelectorCaches::default();
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );
    matches_selector(selector, 0, None, element, &mut context)
  }

  #[test]
  fn root_element_has_no_element_parent() {
    let document = DomNode {
      node_type: DomNodeType::Document,
      children: vec![element("html", vec![element("body", vec![])])],
    };

    let html = &document.children[0];
    let html_ancestors: Vec<&DomNode> = vec![&document];
    let html_ref = ElementRef::with_ancestors(html, &html_ancestors);

    let universal_parent = parse_selector("* > html");
    assert!(
      !selector_matches(&html_ref, &universal_parent),
      "document parent should not satisfy element parent combinators"
    );

    let universal_ancestor = parse_selector("* html");
    assert!(
      !selector_matches(&html_ref, &universal_ancestor),
      "document ancestor should not satisfy element ancestor combinators"
    );

    let body = &html.children[0];
    let body_ancestors: Vec<&DomNode> = vec![&document, html];
    let body_ref = ElementRef::with_ancestors(body, &body_ancestors);

    let html_child_body = parse_selector("html > body");
    assert!(selector_matches(&body_ref, &html_child_body));
  }

  #[test]
  fn parent_element_skips_shadow_roots() {
    let shadow_child = element("span", vec![]);
    let shadow_root = DomNode {
      node_type: DomNodeType::ShadowRoot {
        mode: ShadowRootMode::Open,
      },
      children: vec![shadow_child],
    };
    let host = element("div", vec![shadow_root]);

    let shadow_root_ref = &host.children[0];
    let shadow_child_ref = &shadow_root_ref.children[0];
    let shadow_ancestors: Vec<&DomNode> = vec![&host, shadow_root_ref];
    let shadow_element_ref = ElementRef::with_ancestors(shadow_child_ref, &shadow_ancestors);

    assert!(shadow_element_ref.parent_node_is_shadow_root());
    assert!(shadow_element_ref.parent_element().is_none());

    let normal_child = element("p", vec![]);
    let parent = element("section", vec![normal_child]);
    let normal_child_ref = &parent.children[0];
    let normal_ancestors: Vec<&DomNode> = vec![&parent];
    let normal_element_ref = ElementRef::with_ancestors(normal_child_ref, &normal_ancestors);

    let normal_parent = normal_element_ref
      .parent_element()
      .expect("normal elements should report element parents");
    assert_eq!(normal_parent.node.tag_name(), Some("section"));
    assert!(!normal_element_ref.parent_node_is_shadow_root());
  }

  #[test]
  fn descendant_selector_does_not_cross_shadow_root() {
    let shadow_child = element("span", vec![]);
    let shadow_root = DomNode {
      node_type: DomNodeType::ShadowRoot {
        mode: ShadowRootMode::Open,
      },
      children: vec![shadow_child],
    };
    let host = element("div", vec![shadow_root]);
    let body = element("body", vec![host]);
    let html = element("html", vec![body]);
    let document = DomNode {
      node_type: DomNodeType::Document,
      children: vec![html],
    };

    let mut input = ParserInput::new("body span");
    let mut parser = Parser::new(&mut input);
    let selector_list =
      SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::No).expect("parse");
    let selector = selector_list
      .slice()
      .first()
      .expect("expected a selector");

    let mut caches = SelectorCaches::default();
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );

    let html = &document.children[0];
    let body = &html.children[0];
    let host = &body.children[0];
    let shadow_root = &host.children[0];
    let shadow_child = &shadow_root.children[0];
    let ancestors: Vec<&DomNode> = vec![&document, html, body, host, shadow_root];
    let element_ref = ElementRef::with_ancestors(shadow_child, &ancestors);

    assert!(
      !matches_selector(selector, 0, None, &element_ref, &mut context),
      "elements in shadow trees should not match selectors that rely on light DOM ancestors"
    );
  }

  #[test]
  fn namespace_matching_defaults_to_html() {
    let node = element("div", vec![]);
    let element_ref = ElementRef::new(&node);

    assert!(element_ref.has_namespace(""));
    assert!(element_ref.has_namespace(HTML_NAMESPACE));
    assert!(!element_ref.has_namespace("http://www.w3.org/2000/svg"));
  }

  #[test]
  fn root_matches_html_case_insensitive() {
    let upper = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "HTML".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let svg_root = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "svg".to_string(),
        namespace: SVG_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    assert!(matches(&upper, &[], &PseudoClass::Root));
    assert!(!matches(&svg_root, &[], &PseudoClass::Root));
  }

  #[test]
  fn scope_matches_document_root_without_anchor() {
    let document = DomNode {
      node_type: DomNodeType::Document,
      children: vec![element("html", vec![element("body", vec![])])],
    };

    let html = &document.children[0];
    let body = &document.children[0].children[0];

    assert!(matches(html, &[&document], &PseudoClass::Scope));
    assert!(!matches(body, &[&document, html], &PseudoClass::Scope));
  }

  #[test]
  fn is_same_type_ignores_ascii_case() {
    let upper = element("DIV", vec![]);
    let lower = element("div", vec![]);

    let upper_ref = ElementRef::new(&upper);
    let lower_ref = ElementRef::new(&lower);

    assert!(upper_ref.is_same_type(&lower_ref));
  }

  #[test]
  fn is_same_type_accounts_for_namespace() {
    let html_div = element("div", vec![]);
    let svg_div = svg_element("div");

    let html_ref = ElementRef::new(&html_div);
    let svg_ref = ElementRef::new(&svg_div);

    assert!(!html_ref.is_same_type(&svg_ref));
  }

  #[test]
  fn has_local_name_respects_case_for_foreign_elements() {
    let svg = svg_element("linearGradient");
    let svg_ref = ElementRef::new(&svg);

    assert!(svg_ref.has_local_name("linearGradient"));
    assert!(!svg_ref.has_local_name("lineargradient"));
    assert!(!svg_ref.has_local_name("LINEARGRADIENT"));
  }

  #[test]
  fn namespace_matching_uses_element_namespace() {
    let svg = svg_element("svg");
    let svg_ref = ElementRef::new(&svg);

    assert!(svg_ref.has_namespace(""));
    assert!(svg_ref.has_namespace(SVG_NAMESPACE));
    assert!(!svg_ref.has_namespace(HTML_NAMESPACE));
  }

  fn collect_wbr_texts(node: &DomNode, out: &mut Vec<String>) {
    if let DomNodeType::Element { tag_name, .. } = &node.node_type {
      if tag_name.eq_ignore_ascii_case("wbr") {
        for child in &node.children {
          if let DomNodeType::Text { content } = &child.node_type {
            out.push(content.clone());
          }
        }
      }
    }
    for child in &node.children {
      collect_wbr_texts(child, out);
    }
  }

  #[test]
  fn attribute_lookup_is_case_insensitive() {
    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("HREF".to_string(), "foo".to_string())],
      },
      children: vec![],
    };

    assert_eq!(node.get_attribute("href"), Some("foo".to_string()));
    assert_eq!(node.get_attribute("HRef"), Some("foo".to_string()));
  }

  #[test]
  fn attr_selector_respects_case_sensitivity() {
    use selectors::attr::AttrSelectorOperation;
    use selectors::attr::AttrSelectorOperator;
    use selectors::attr::NamespaceConstraint;

    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("foo".to_string(), "Bar".to_string())],
      },
      children: vec![],
    };
    let element_ref = ElementRef::new(&node);
    let local = CssString("foo".into());

    let value_insensitive = CssString("bar".into());
    let op_insensitive = AttrSelectorOperation::WithValue {
      operator: AttrSelectorOperator::Equal,
      case_sensitivity: CaseSensitivity::AsciiCaseInsensitive,
      value: &value_insensitive,
    };
    assert!(element_ref.attr_matches(&NamespaceConstraint::Any, &local, &op_insensitive));

    let value_sensitive = CssString("bar".into());
    let op_sensitive = AttrSelectorOperation::WithValue {
      operator: AttrSelectorOperator::Equal,
      case_sensitivity: CaseSensitivity::CaseSensitive,
      value: &value_sensitive,
    };
    assert!(!element_ref.attr_matches(&NamespaceConstraint::Any, &local, &op_sensitive));

    // Namespaced selector should fail when requesting a non-HTML namespace.
    let svg_ns = CssString("http://www.w3.org/2000/svg".into());
    assert!(!element_ref.attr_matches(
      &NamespaceConstraint::Specific(&svg_ns),
      &local,
      &op_insensitive,
    ));
  }

  #[test]
  fn id_and_class_respect_case_sensitivity() {
    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("id".to_string(), "Foo".to_string()),
          ("class".to_string(), "Bar baz".to_string()),
        ],
      },
      children: vec![],
    };
    let element_ref = ElementRef::new(&node);

    assert!(element_ref.has_id(&CssString("Foo".into()), CaseSensitivity::CaseSensitive));
    assert!(!element_ref.has_id(&CssString("foo".into()), CaseSensitivity::CaseSensitive));
    assert!(element_ref.has_id(
      &CssString("foo".into()),
      CaseSensitivity::AsciiCaseInsensitive
    ));

    assert!(element_ref.has_class(&CssString("Bar".into()), CaseSensitivity::CaseSensitive));
    assert!(!element_ref.has_class(&CssString("bar".into()), CaseSensitivity::CaseSensitive));
    assert!(element_ref.has_class(
      &CssString("bar".into()),
      CaseSensitivity::AsciiCaseInsensitive
    ));
  }

  #[test]
  fn empty_pseudo_requires_no_element_or_text_children() {
    let empty = element("div", vec![]);
    let whitespace = element("div", vec![text(" \n")]);
    let child = element("div", vec![element("span", vec![])]);

    assert!(matches(&empty, &[], &PseudoClass::Empty));
    assert!(!matches(&whitespace, &[], &PseudoClass::Empty));
    assert!(!matches(&child, &[], &PseudoClass::Empty));
  }

  #[test]
  fn wbr_inserts_zero_width_break_text_node() {
    let dom = parse_html("<p>Hello<wbr>World</p>").expect("parse html");
    let mut texts = Vec::new();
    collect_wbr_texts(&dom, &mut texts);
    assert!(texts.iter().any(|t| t == "\u{200B}"));
  }

  #[test]
  fn type_position_pseudos_filter_by_tag_name() {
    let parent = element(
      "div",
      vec![
        element("span", vec![]),
        element("em", vec![]),
        element("span", vec![]),
      ],
    );
    let ancestors: Vec<&DomNode> = vec![&parent];

    let first_span = &parent.children[0];
    let em = &parent.children[1];
    let second_span = &parent.children[2];

    assert!(matches(first_span, &ancestors, &PseudoClass::FirstOfType));
    assert!(!matches(first_span, &ancestors, &PseudoClass::LastOfType));
    assert!(!matches(first_span, &ancestors, &PseudoClass::OnlyOfType));
    assert!(!matches(
      first_span,
      &ancestors,
      &PseudoClass::NthOfType(2, 0)
    ));
    assert!(matches(
      first_span,
      &ancestors,
      &PseudoClass::NthLastOfType(2, 0)
    ));

    assert!(matches(second_span, &ancestors, &PseudoClass::LastOfType));
    assert!(!matches(second_span, &ancestors, &PseudoClass::OnlyOfType));
    assert!(matches(
      second_span,
      &ancestors,
      &PseudoClass::NthOfType(2, 0)
    ));
    assert!(matches(
      second_span,
      &ancestors,
      &PseudoClass::NthLastOfType(0, 1)
    ));

    // Different element type should be unaffected by span counting.
    assert!(matches(em, &ancestors, &PseudoClass::OnlyOfType));
  }

  #[test]
  fn only_of_type_ignores_unrelated_siblings() {
    let parent = element("div", vec![element("span", vec![]), element("div", vec![])]);
    let ancestors: Vec<&DomNode> = vec![&parent];

    let span = &parent.children[0];
    let div = &parent.children[1];

    assert!(matches(span, &ancestors, &PseudoClass::OnlyOfType));
    assert!(matches(div, &ancestors, &PseudoClass::OnlyOfType));
  }

  #[test]
  fn lang_matches_inherit_and_prefix() {
    let child = element("p", vec![]);
    let root = element(
      "html",
      vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("lang".to_string(), "en-US".to_string())],
        },
        children: vec![child],
      }],
    );

    let ancestors: Vec<&DomNode> = vec![&root, &root.children[0]];
    let node = &root.children[0].children[0];

    assert!(matches(
      node,
      &ancestors,
      &PseudoClass::Lang(vec!["en".into()])
    ));
    assert!(matches(
      node,
      &ancestors,
      &PseudoClass::Lang(vec!["en-us".into()])
    ));
    assert!(matches(
      node,
      &ancestors,
      &PseudoClass::Lang(vec!["*".into()])
    ));
    assert!(!matches(
      node,
      &ancestors,
      &PseudoClass::Lang(vec!["fr".into()])
    ));

    // Multiple ranges OR together
    assert!(matches(
      node,
      &ancestors,
      &PseudoClass::Lang(vec!["fr".into(), "en".into()])
    ));
  }

  #[test]
  fn dir_matches_inherited_direction() {
    let child = element("span", vec![]);
    let root = element(
      "div",
      vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "p".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("dir".to_string(), "rtl".to_string())],
        },
        children: vec![child],
      }],
    );
    let ancestors: Vec<&DomNode> = vec![&root, &root.children[0]];
    let node = &root.children[0].children[0];

    assert!(matches(
      node,
      &ancestors,
      &PseudoClass::Dir(TextDirection::Rtl)
    ));
    assert!(!matches(
      node,
      &ancestors,
      &PseudoClass::Dir(TextDirection::Ltr)
    ));
  }

  #[test]
  fn dir_auto_uses_first_strong() {
    let rtl_text = DomNode {
      node_type: DomNodeType::Text {
        content: "שלום".to_string(),
      },
      children: vec![],
    };
    let root = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("dir".to_string(), "auto".to_string())],
      },
      children: vec![rtl_text],
    };
    assert!(matches(&root, &[], &PseudoClass::Dir(TextDirection::Rtl)));
    assert!(!matches(&root, &[], &PseudoClass::Dir(TextDirection::Ltr)));
  }

  #[test]
  fn dir_auto_on_ancestor_inherits_resolved_direction() {
    let rtl_text = DomNode {
      node_type: DomNodeType::Text {
        content: "שלום".to_string(),
      },
      children: vec![],
    };
    let child = element("span", vec![]);
    let container = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "p".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("dir".to_string(), "auto".to_string())],
      },
      children: vec![rtl_text, child],
    };
    let root = element("div", vec![container]);
    let ancestors: Vec<&DomNode> = vec![&root, &root.children[0]];
    let target = &root.children[0].children[1];
    assert!(matches(
      target,
      &ancestors,
      &PseudoClass::Dir(TextDirection::Rtl)
    ));
  }

  #[test]
  fn any_link_matches_href_anchors() {
    let link = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "#foo".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&link, &[], &PseudoClass::AnyLink));
    assert!(matches(&link, &[], &PseudoClass::Link));
    assert!(!matches(&link, &[], &PseudoClass::Visited));

    // Area and link elements also qualify
    let area = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "area".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "/foo".to_string())],
      },
      children: vec![],
    };
    let stylesheet_link = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "link".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "style.css".to_string())],
      },
      children: vec![],
    };

    assert!(matches(&area, &[], &PseudoClass::AnyLink));
    assert!(matches(&stylesheet_link, &[], &PseudoClass::AnyLink));
  }

  #[test]
  fn placeholder_shown_matches_empty_controls() {
    let input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("placeholder".to_string(), "Search".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&input, &[], &PseudoClass::PlaceholderShown));

    let with_value = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("placeholder".to_string(), "Search".to_string()),
          ("value".to_string(), "query".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(!matches(&with_value, &[], &PseudoClass::PlaceholderShown));

    let checkbox = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "checkbox".to_string()),
          ("placeholder".to_string(), "X".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(!matches(&checkbox, &[], &PseudoClass::PlaceholderShown));

    let empty_textarea = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "textarea".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("placeholder".to_string(), "Describe".to_string())],
      },
      children: vec![],
    };
    assert!(matches(
      &empty_textarea,
      &[],
      &PseudoClass::PlaceholderShown
    ));

    let prefilled_textarea = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "textarea".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("placeholder".to_string(), "Describe".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Text {
          content: "Hello".to_string(),
        },
        children: vec![],
      }],
    };
    assert!(!matches(
      &prefilled_textarea,
      &[],
      &PseudoClass::PlaceholderShown
    ));
  }

  #[test]
  fn autofill_never_matches_without_state() {
    let input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "text".to_string()),
          ("value".to_string(), "filled".to_string()),
        ],
      },
      children: vec![],
    };

    assert!(!matches(&input, &[], &PseudoClass::Autofill));
  }

  #[test]
  fn required_and_optional_match_supported_controls() {
    let text_input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "text".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&text_input, &[], &PseudoClass::Optional));
    assert!(!matches(&text_input, &[], &PseudoClass::Required));

    let required_input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "email".to_string()),
          ("required".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(matches(&required_input, &[], &PseudoClass::Required));
    assert!(!matches(&required_input, &[], &PseudoClass::Optional));

    let disabled_required = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("required".to_string(), "true".to_string()),
          ("disabled".to_string(), "disabled".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(!matches(&disabled_required, &[], &PseudoClass::Required));
    assert!(!matches(&disabled_required, &[], &PseudoClass::Optional));

    let submit_input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "submit".to_string()),
          ("required".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(!matches(&submit_input, &[], &PseudoClass::Required));
    assert!(!matches(&submit_input, &[], &PseudoClass::Optional));

    let select = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "select".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("required".to_string(), "required".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&select, &[], &PseudoClass::Required));

    let fieldset = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "fieldset".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("disabled".to_string(), "true".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "input".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![
            ("type".to_string(), "text".to_string()),
            ("required".to_string(), "true".to_string()),
          ],
        },
        children: vec![],
      }],
    };
    let ancestors: Vec<&DomNode> = vec![&fieldset];
    let child = &fieldset.children[0];
    assert!(!matches(child, &ancestors, &PseudoClass::Required));
    assert!(!matches(child, &ancestors, &PseudoClass::Optional));
  }

  #[test]
  fn link_and_visited_match_state_flags() {
    let unvisited = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "https://example.com".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&unvisited, &[], &PseudoClass::Link));
    assert!(!matches(&unvisited, &[], &PseudoClass::Visited));

    let visited = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("href".to_string(), "https://example.com".to_string()),
          ("data-fastr-visited".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(!matches(&visited, &[], &PseudoClass::Link));
    assert!(matches(&visited, &[], &PseudoClass::Visited));
  }

  #[test]
  fn active_matches_when_flagged() {
    let inactive = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "https://example.com".to_string())],
      },
      children: vec![],
    };
    assert!(!matches(&inactive, &[], &PseudoClass::Active));

    let active = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("href".to_string(), "https://example.com".to_string()),
          ("data-fastr-active".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(matches(&active, &[], &PseudoClass::Active));
  }

  #[test]
  fn hover_and_focus_match_when_flagged() {
    let hover = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("href".to_string(), "https://example.com".to_string()),
          ("data-fastr-hover".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(matches(&hover, &[], &PseudoClass::Hover));
    assert!(!matches(&hover, &[], &PseudoClass::Focus));

    let focus = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("href".to_string(), "https://example.com".to_string()),
          ("data-fastr-focus".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(!matches(&focus, &[], &PseudoClass::Hover));
    assert!(matches(&focus, &[], &PseudoClass::Focus));
  }

  #[test]
  fn hover_and_focus_do_not_match_by_default() {
    let link = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "https://example.com".to_string())],
      },
      children: vec![],
    };

    assert!(!matches(&link, &[], &PseudoClass::Hover));
    assert!(!matches(&link, &[], &PseudoClass::Focus));
  }

  #[test]
  fn svg_is_not_focusable_by_default() {
    let svg = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "svg".to_string(),
        namespace: SVG_NAMESPACE.to_string(),
        attributes: vec![("data-fastr-focus".to_string(), "true".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&svg, &[], &PseudoClass::Hover) == false);
    assert!(!matches(&svg, &[], &PseudoClass::Focus));
  }

  #[test]
  fn svg_focusable_true_allows_focus() {
    let svg = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "svg".to_string(),
        namespace: SVG_NAMESPACE.to_string(),
        attributes: vec![
          ("focusable".to_string(), "true".to_string()),
          ("data-fastr-focus".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(matches(&svg, &[], &PseudoClass::Focus));
  }

  #[test]
  fn svg_focusable_false_blocks_focus_even_when_flagged() {
    let svg = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "svg".to_string(),
        namespace: SVG_NAMESPACE.to_string(),
        attributes: vec![
          ("focusable".to_string(), "false".to_string()),
          ("data-fastr-focus".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };

    assert!(!matches(&svg, &[], &PseudoClass::Focus));
  }

  #[test]
  fn focus_within_matches_focused_element() {
    let focused = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("data-fastr-focus".to_string(), "true".to_string())],
      },
      children: vec![],
    };

    assert!(matches(&focused, &[], &PseudoClass::FocusWithin));
  }

  #[test]
  fn focus_within_matches_descendant_focus() {
    let mut parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "button".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("data-fastr-focus".to_string(), "true".to_string())],
      },
      children: vec![],
    };

    parent.children.push(child);

    assert!(matches(&parent, &[], &PseudoClass::FocusWithin));
  }

  #[test]
  fn focus_within_respects_svg_focusable() {
    let mut parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let svg_unfocusable = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "svg".to_string(),
        namespace: SVG_NAMESPACE.to_string(),
        attributes: vec![("data-fastr-focus".to_string(), "true".to_string())],
      },
      children: vec![],
    };

    parent.children.push(svg_unfocusable);

    assert!(!matches(&parent, &[], &PseudoClass::FocusWithin));

    let mut parent_focusable = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let svg_focusable = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "svg".to_string(),
        namespace: SVG_NAMESPACE.to_string(),
        attributes: vec![
          ("focusable".to_string(), "true".to_string()),
          ("data-fastr-focus".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };

    parent_focusable.children.push(svg_focusable);

    assert!(matches(&parent_focusable, &[], &PseudoClass::FocusWithin));
  }

  #[test]
  fn focus_visible_matches_when_flagged() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "button".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("data-fastr-focus".to_string(), "true".to_string()),
          ("data-fastr-focus-visible".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };

    assert!(matches(&dom, &[], &PseudoClass::FocusVisible));
  }

  #[test]
  fn focus_visible_requires_visible_flag() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "button".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("data-fastr-focus".to_string(), "true".to_string())],
      },
      children: vec![],
    };

    assert!(!matches(&dom, &[], &PseudoClass::FocusVisible));
  }

  #[test]
  fn checked_matches_inputs_and_options() {
    let checkbox = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "checkbox".to_string()),
          ("checked".to_string(), "checked".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(matches(&checkbox, &[], &PseudoClass::Checked));

    let radio = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "radio".to_string())],
      },
      children: vec![],
    };
    assert!(!matches(&radio, &[], &PseudoClass::Checked));

    let option_selected = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "option".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("selected".to_string(), "selected".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&option_selected, &[], &PseudoClass::Checked));

    let select = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "select".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "option".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        },
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "option".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        },
      ],
    };
    let ancestors: Vec<&DomNode> = vec![&select];
    let first = &select.children[0];
    let second = &select.children[1];
    assert!(matches(first, &ancestors, &PseudoClass::Checked));
    assert!(!matches(second, &ancestors, &PseudoClass::Checked));

    let select_with_explicit = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "select".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "option".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        },
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "option".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("selected".to_string(), "selected".to_string())],
          },
          children: vec![],
        },
      ],
    };
    let ancestors: Vec<&DomNode> = vec![&select_with_explicit];
    let first = &select_with_explicit.children[0];
    let second = &select_with_explicit.children[1];
    assert!(!matches(first, &ancestors, &PseudoClass::Checked));
    assert!(matches(second, &ancestors, &PseudoClass::Checked));

    let select_disabled_first = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "select".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "option".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("disabled".to_string(), "disabled".to_string())],
          },
          children: vec![],
        },
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "option".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        },
      ],
    };
    let ancestors: Vec<&DomNode> = vec![&select_disabled_first];
    let first = &select_disabled_first.children[0];
    let second = &select_disabled_first.children[1];
    assert!(!matches(first, &ancestors, &PseudoClass::Checked));
    assert!(matches(second, &ancestors, &PseudoClass::Checked));

    let select_multiple = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "select".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("multiple".to_string(), "multiple".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "option".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };
    let ancestors: Vec<&DomNode> = vec![&select_multiple];
    let only_option = &select_multiple.children[0];
    assert!(!matches(only_option, &ancestors, &PseudoClass::Checked));

    let select_multiple_selected = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "select".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("multiple".to_string(), "multiple".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "option".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("selected".to_string(), "selected".to_string())],
        },
        children: vec![],
      }],
    };
    let ancestors: Vec<&DomNode> = vec![&select_multiple_selected];
    let selected_option = &select_multiple_selected.children[0];
    assert!(matches(selected_option, &ancestors, &PseudoClass::Checked));
  }

  #[test]
  fn indeterminate_matches_checkbox_and_progress() {
    let checkbox = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "checkbox".to_string()),
          ("indeterminate".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(matches(&checkbox, &[], &PseudoClass::Indeterminate));

    let normal_checkbox = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "checkbox".to_string())],
      },
      children: vec![],
    };
    assert!(!matches(&normal_checkbox, &[], &PseudoClass::Indeterminate));

    let radio = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "radio".to_string()),
          ("indeterminate".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(!matches(&radio, &[], &PseudoClass::Indeterminate));

    let progress_indeterminate = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "progress".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    assert!(matches(
      &progress_indeterminate,
      &[],
      &PseudoClass::Indeterminate
    ));

    let progress_value = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "progress".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("value".to_string(), "0.5".to_string())],
      },
      children: vec![],
    };
    assert!(!matches(&progress_value, &[], &PseudoClass::Indeterminate));

    let progress_invalid_value = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "progress".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("value".to_string(), "not-a-number".to_string())],
      },
      children: vec![],
    };
    assert!(matches(
      &progress_invalid_value,
      &[],
      &PseudoClass::Indeterminate
    ));
  }

  #[test]
  fn default_matches_submit_controls_and_options() {
    let form = element(
      "form",
      vec![
        element("input", vec![]),
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "button".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        },
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "button".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("type".to_string(), "submit".to_string())],
          },
          children: vec![],
        },
      ],
    );
    let ancestors: Vec<&DomNode> = vec![&form];
    let default_button = &form.children[1];
    let submit_button = &form.children[2];
    assert!(matches(default_button, &ancestors, &PseudoClass::Default));
    assert!(!matches(submit_button, &ancestors, &PseudoClass::Default));

    let form_disabled_first = element(
      "form",
      vec![
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "button".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("disabled".to_string(), "disabled".to_string())],
          },
          children: vec![],
        },
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "button".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        },
      ],
    );
    let ancestors: Vec<&DomNode> = vec![&form_disabled_first];
    let disabled = &form_disabled_first.children[0];
    let enabled = &form_disabled_first.children[1];
    assert!(!matches(disabled, &ancestors, &PseudoClass::Default));
    assert!(matches(enabled, &ancestors, &PseudoClass::Default));

    let select = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "select".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "option".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("disabled".to_string(), "disabled".to_string())],
          },
          children: vec![],
        },
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "option".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        },
      ],
    };
    let ancestors: Vec<&DomNode> = vec![&select];
    let first = &select.children[0];
    let second = &select.children[1];
    assert!(!matches(first, &ancestors, &PseudoClass::Default));
    assert!(matches(second, &ancestors, &PseudoClass::Default));

    let checkbox = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "checkbox".to_string()),
          ("checked".to_string(), "checked".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(matches(&checkbox, &[], &PseudoClass::Default));
  }

  #[test]
  fn disabled_and_enabled_match_controls() {
    let input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    assert!(matches(&input, &[], &PseudoClass::Enabled));
    assert!(!matches(&input, &[], &PseudoClass::Disabled));

    let disabled_button = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "button".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("disabled".to_string(), "true".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&disabled_button, &[], &PseudoClass::Disabled));
    assert!(!matches(&disabled_button, &[], &PseudoClass::Enabled));

    // Fieldset disables descendants except inside first legend
    let legend_child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let legend = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "legend".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![legend_child.clone()],
    };
    let outer_input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let fieldset = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "fieldset".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("disabled".to_string(), "true".to_string())],
      },
      children: vec![legend.clone(), outer_input.clone()],
    };

    let anc_outer: Vec<&DomNode> = vec![&fieldset];
    assert!(matches(&outer_input, &anc_outer, &PseudoClass::Disabled));
    assert!(!matches(&outer_input, &anc_outer, &PseudoClass::Enabled));

    let anc_legend: Vec<&DomNode> = vec![&fieldset, &fieldset.children[0]];
    let legend_child_ref = &fieldset.children[0].children[0];
    assert!(!matches(
      legend_child_ref,
      &anc_legend,
      &PseudoClass::Disabled
    ));
    assert!(matches(
      legend_child_ref,
      &anc_legend,
      &PseudoClass::Enabled
    ));
  }

  #[test]
  fn valid_invalid_and_range_match_controls() {
    let text_input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "text".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&text_input, &[], &PseudoClass::Valid));
    assert!(!matches(&text_input, &[], &PseudoClass::Invalid));

    let required_empty = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("required".to_string(), "true".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&required_empty, &[], &PseudoClass::Invalid));
    assert!(!matches(&required_empty, &[], &PseudoClass::Valid));

    let number_in_range = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "number".to_string()),
          ("value".to_string(), "5".to_string()),
          ("min".to_string(), "1".to_string()),
          ("max".to_string(), "10".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(matches(&number_in_range, &[], &PseudoClass::Valid));
    assert!(matches(&number_in_range, &[], &PseudoClass::InRange));
    assert!(!matches(&number_in_range, &[], &PseudoClass::OutOfRange));

    let number_out_of_range = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "number".to_string()),
          ("value".to_string(), "15".to_string()),
          ("min".to_string(), "1".to_string()),
          ("max".to_string(), "10".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(matches(&number_out_of_range, &[], &PseudoClass::Invalid));
    assert!(matches(&number_out_of_range, &[], &PseudoClass::OutOfRange));
    assert!(!matches(&number_out_of_range, &[], &PseudoClass::InRange));

    let number_nan = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "number".to_string()),
          ("value".to_string(), "abc".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(matches(&number_nan, &[], &PseudoClass::Invalid));
    assert!(!matches(&number_nan, &[], &PseudoClass::Valid));

    let disabled_input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("required".to_string(), "true".to_string()),
          ("disabled".to_string(), "true".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(matches(&disabled_input, &[], &PseudoClass::Valid));
    assert!(!matches(&disabled_input, &[], &PseudoClass::Invalid));

    let required_multiple_select = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "select".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("required".to_string(), "true".to_string()),
          ("multiple".to_string(), "multiple".to_string()),
        ],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "option".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };
    assert!(matches(
      &required_multiple_select,
      &[],
      &PseudoClass::Invalid
    ));

    let valid_multiple_select = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "select".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("required".to_string(), "true".to_string()),
          ("multiple".to_string(), "multiple".to_string()),
        ],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "option".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![
            ("selected".to_string(), "selected".to_string()),
            ("value".to_string(), "a".to_string()),
          ],
        },
        children: vec![],
      }],
    };
    assert!(matches(&valid_multiple_select, &[], &PseudoClass::Valid));
    assert!(!matches(&valid_multiple_select, &[], &PseudoClass::Invalid));
  }

  #[test]
  fn read_only_and_read_write_match_form_controls() {
    let text_input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "text".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&text_input, &[], &PseudoClass::ReadWrite));
    assert!(!matches(&text_input, &[], &PseudoClass::ReadOnly));

    let readonly_input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "text".to_string()),
          ("readonly".to_string(), "readonly".to_string()),
        ],
      },
      children: vec![],
    };
    assert!(matches(&readonly_input, &[], &PseudoClass::ReadOnly));
    assert!(!matches(&readonly_input, &[], &PseudoClass::ReadWrite));

    let disabled_textarea = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "textarea".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("disabled".to_string(), "true".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&disabled_textarea, &[], &PseudoClass::ReadOnly));
    assert!(!matches(&disabled_textarea, &[], &PseudoClass::ReadWrite));

    let checkbox = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "checkbox".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&checkbox, &[], &PseudoClass::ReadOnly));
    assert!(!matches(&checkbox, &[], &PseudoClass::ReadWrite));

    let select = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "select".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    assert!(matches(&select, &[], &PseudoClass::ReadWrite));
    assert!(!matches(&select, &[], &PseudoClass::ReadOnly));

    let editable_div = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("contenteditable".to_string(), "true".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&editable_div, &[], &PseudoClass::ReadWrite));
    assert!(!matches(&editable_div, &[], &PseudoClass::ReadOnly));
  }

  #[test]
  fn target_matches_id_and_name() {
    let target = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("id".to_string(), "section".to_string())],
      },
      children: vec![],
    };
    with_target_fragment(Some("#section"), || {
      assert!(matches(&target, &[], &PseudoClass::Target));
    });
    with_target_fragment(Some("section"), || {
      assert!(matches(&target, &[], &PseudoClass::Target));
    });
    with_target_fragment(Some("other"), || {
      assert!(!matches(&target, &[], &PseudoClass::Target));
    });

    let anchor = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("name".to_string(), "anchor".to_string())],
      },
      children: vec![],
    };
    with_target_fragment(Some("anchor"), || {
      assert!(matches(&anchor, &[], &PseudoClass::Target));
    });
  }

  #[test]
  fn target_within_matches_descendants() {
    let target = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("id".to_string(), "section".to_string())],
      },
      children: vec![],
    };
    let container = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "main".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![target],
    };
    let other = element("p", vec![]);
    let root = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "body".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![container, other],
    };

    let children = &root.children;
    let container_ref = children.first().unwrap();
    let target_ref = container_ref.children.first().unwrap();
    let other_ref = children.get(1).unwrap();

    with_target_fragment(Some("#section"), || {
      assert!(matches(&root, &[], &PseudoClass::TargetWithin));
      assert!(matches(
        &container_ref,
        &[&root],
        &PseudoClass::TargetWithin
      ));
      assert!(matches(
        target_ref,
        &[&root, container_ref],
        &PseudoClass::TargetWithin
      ));
      assert!(!matches(&other_ref, &[&root], &PseudoClass::TargetWithin));
    });
  }

  #[test]
  fn parse_html_preserves_text_content() {
    let html = "<!doctype html><html><body><div><h1>Example Domain</h1><p>This domain is for use in documentation examples without needing permission.</p></div></body></html>";
    let dom = parse_html(html).expect("parse");
    fn contains_text(node: &DomNode, needle: &str) -> bool {
      match &node.node_type {
        DomNodeType::Text { content } => content.contains(needle),
        _ => node.children.iter().any(|c| contains_text(c, needle)),
      }
    }
    assert!(contains_text(&dom, "Example Domain"));
    assert!(contains_text(&dom, "documentation examples"));
  }

  #[test]
  fn parse_html_keeps_noscript_content_without_scripting() {
    let html = "<!doctype html><html><head><noscript><style>.fallback{color:red;}</style></noscript></head><body><noscript><div id='fallback'>hello</div></noscript></body></html>";
    let dom = parse_html(html).expect("parse");

    let fallback = find_element_by_id(&dom, "fallback").expect("noscript content parsed into DOM");
    let has_text_child = fallback.children.iter().any(|child| {
      if let DomNodeType::Text { content } = &child.node_type {
        content.contains("hello")
      } else {
        false
      }
    });
    assert!(
      has_text_child,
      "noscript children should be parsed as normal content"
    );
  }

  #[test]
  fn parse_html_preserves_head_noscript_children() {
    let html = "<!doctype html><html><head><noscript><style id='fallback-style'>body{color:green;}</style></noscript></head><body></body></html>";
    let dom = parse_html(html).expect("parse");

    let style = find_element_by_id(&dom, "fallback-style");
    assert!(
      style.is_some(),
      "style inside <noscript> in <head> should be retained"
    );
  }

  #[test]
  fn scope_matches_document_root_only() {
    let child = element("div", vec![]);
    let root = element("html", vec![child.clone()]);
    let ancestors: Vec<&DomNode> = vec![&root];
    assert!(matches(&root, &[], &PseudoClass::Scope));
    assert!(!matches(&child, &ancestors, &PseudoClass::Scope));
  }

  #[test]
  fn pseudo_element_matching_reports_supported_pseudos() {
    let node = element("div", vec![]);
    let ancestors: Vec<&DomNode> = vec![];
    let mut caches = SelectorCaches::default();
    let mut context = MatchingContext::new(
      MatchingMode::ForStatelessPseudoElement,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );
    let element_ref = ElementRef::with_ancestors(&node, &ancestors);

    assert!(element_ref.match_pseudo_element(&PseudoElement::Before, &mut context));
    assert!(element_ref.match_pseudo_element(&PseudoElement::After, &mut context));
    assert!(element_ref.match_pseudo_element(&PseudoElement::Marker, &mut context));
  }

  #[test]
  fn parse_html_leaves_classes_untouched_by_default() {
    let dom = parse_html("<html class='no-js foo'><body></body></html>").expect("parse");
    let html = dom
      .children
      .iter()
      .find(|c| matches!(c.node_type, DomNodeType::Element { .. }))
      .expect("html child");
    let classes = match &html.node_type {
      DomNodeType::Element { attributes, .. } => attributes
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case("class"))
        .map(|(_, v)| v.split_whitespace().collect::<Vec<_>>())
        .unwrap_or_default(),
      _ => panic!("expected html element"),
    };
    assert!(classes.contains(&"no-js"));
    assert!(!classes.contains(&"js-enabled"));
    assert!(!classes.contains(&"jsl10n-visible"));

    let body = html
      .children
      .iter()
      .find(|c| {
        if let DomNodeType::Element { tag_name, .. } = &c.node_type {
          tag_name.eq_ignore_ascii_case("body")
        } else {
          false
        }
      })
      .expect("body child");
    let body_classes = match &body.node_type {
      DomNodeType::Element { attributes, .. } => attributes
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case("class"))
        .map(|(_, v)| v.split_whitespace().collect::<Vec<_>>())
        .unwrap_or_default(),
      _ => panic!("expected body element"),
    };
    assert!(!body_classes.contains(&"jsl10n-visible"));
  }

  #[test]
  fn parse_html_compat_mode_flips_no_js_class() {
    let dom = parse_html_with_options(
      "<html class='no-js foo'><body></body></html>",
      DomParseOptions::compatibility(),
    )
    .expect("parse");
    let html = dom
      .children
      .iter()
      .find(|c| matches!(c.node_type, DomNodeType::Element { .. }))
      .expect("html child");
    let classes = match &html.node_type {
      DomNodeType::Element { attributes, .. } => attributes
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case("class"))
        .map(|(_, v)| v.split_whitespace().collect::<Vec<_>>())
        .unwrap_or_default(),
      _ => panic!("expected html element"),
    };
    assert!(!classes.contains(&"no-js"));
    assert!(classes.contains(&"js-enabled"));
    assert!(classes.contains(&"foo"));
    assert!(classes.contains(&"jsl10n-visible"));
  }

  #[test]
  fn parse_html_compat_mode_adds_jsl10n_visible_when_missing() {
    let dom = parse_html_with_options(
      "<html><body></body></html>",
      DomParseOptions::compatibility(),
    )
    .expect("parse");
    let html = dom
      .children
      .iter()
      .find(|c| matches!(c.node_type, DomNodeType::Element { .. }))
      .expect("html child");
    let classes = match &html.node_type {
      DomNodeType::Element { attributes, .. } => attributes
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case("class"))
        .map(|(_, v)| v.split_whitespace().collect::<Vec<_>>())
        .unwrap_or_default(),
      _ => panic!("expected html element"),
    };
    assert!(classes.contains(&"jsl10n-visible"));
  }

  #[test]
  fn parse_html_compat_mode_marks_body_jsl10n_visible() {
    let dom = parse_html_with_options(
      "<html><body class='portal'></body></html>",
      DomParseOptions::compatibility(),
    )
    .expect("parse");
    let html = dom
      .children
      .iter()
      .find(|c| matches!(c.node_type, DomNodeType::Element { .. }))
      .expect("html child");
    let body = html
      .children
      .iter()
      .find(|c| {
        if let DomNodeType::Element { tag_name, .. } = &c.node_type {
          tag_name.eq_ignore_ascii_case("body")
        } else {
          false
        }
      })
      .expect("body child");

    let classes = match &body.node_type {
      DomNodeType::Element { attributes, .. } => attributes
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case("class"))
        .map(|(_, v)| v.split_whitespace().collect::<Vec<_>>())
        .unwrap_or_default(),
      _ => panic!("expected body element"),
    };
    assert!(classes.contains(&"portal"));
    assert!(classes.contains(&"jsl10n-visible"));
  }
}
