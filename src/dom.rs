use crate::css::selectors::FastRenderSelectorImpl;
use crate::css::selectors::PartExportMap;
use crate::css::selectors::PseudoClass;
use crate::css::selectors::PseudoElement;
use crate::css::selectors::SlotAssignmentMap;
use crate::css::selectors::TextDirection;
use crate::css::types::CssString;
use crate::error::Error;
use crate::error::ParseError;
use crate::error::RenderStage;
use crate::error::Result;
use crate::render_control::check_active_periodic;
use html5ever::parse_document;
use html5ever::tendril::TendrilSink;
use html5ever::tree_builder::QuirksMode as HtmlQuirksMode;
use html5ever::tree_builder::TreeBuilderOpts;
use html5ever::ParseOpts;
use markup5ever_rcdom::Handle;
use markup5ever_rcdom::NodeData;
use markup5ever_rcdom::RcDom;
use selectors::bloom::BloomFilter;
use selectors::attr::AttrSelectorOperation;
use selectors::attr::CaseSensitivity;
use selectors::context::QuirksMode;
use selectors::matching::matches_selector;
use selectors::matching::selector_may_match;
use selectors::matching::MatchingContext;
use selectors::parser::RelativeSelector;
use selectors::relative_selector::cache::RelativeSelectorCachedMatch;
use selectors::Element;
use selectors::OpaqueElement;
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::{BuildHasherDefault, Hasher};
use std::io;
use std::ptr;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::thread_local;
use std::time::Instant;
use unicode_bidi::bidi_class;

pub const HTML_NAMESPACE: &str = "http://www.w3.org/1999/xhtml";
pub const SVG_NAMESPACE: &str = "http://www.w3.org/2000/svg";
pub const MATHML_NAMESPACE: &str = "http://www.w3.org/1998/Math/MathML";

const RELATIVE_SELECTOR_DEADLINE_STRIDE: usize = 64;
const NTH_DEADLINE_STRIDE: usize = 64;
const DOM_PARSE_READ_DEADLINE_STRIDE: usize = 1;
const DOM_PARSE_NODE_DEADLINE_STRIDE: usize = 1024;
const DOM_PARSE_READ_MAX_CHUNK_BYTES: usize = 16 * 1024;

#[cfg(test)]
thread_local! {
  static NTH_OF_CACHE_POPULATIONS: AtomicU64 = const { AtomicU64::new(0) };
}

/// Controls whether non-standard DOM compatibility mutations are applied while parsing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Debug, Default, Clone)]
pub(crate) struct DomParseDiagnostics {
  pub html5ever_ms: f64,
  pub convert_ms: f64,
  pub shadow_attach_ms: f64,
  pub compat_ms: f64,
}

static DOM_PARSE_DIAGNOSTICS: OnceLock<Mutex<DomParseDiagnostics>> = OnceLock::new();
static DOM_PARSE_DIAGNOSTICS_ENABLED: AtomicBool = AtomicBool::new(false);

fn dom_parse_diagnostics_cell() -> &'static Mutex<DomParseDiagnostics> {
  DOM_PARSE_DIAGNOSTICS.get_or_init(|| Mutex::new(DomParseDiagnostics::default()))
}

pub(crate) fn enable_dom_parse_diagnostics() {
  DOM_PARSE_DIAGNOSTICS_ENABLED.store(true, Ordering::Release);
  if let Ok(mut diag) = dom_parse_diagnostics_cell().lock() {
    *diag = DomParseDiagnostics::default();
  }
}

pub(crate) fn take_dom_parse_diagnostics() -> Option<DomParseDiagnostics> {
  if !DOM_PARSE_DIAGNOSTICS_ENABLED.swap(false, Ordering::AcqRel) {
    return None;
  }

  dom_parse_diagnostics_cell()
    .lock()
    .ok()
    .map(|diag| diag.clone())
}

fn dom_parse_diagnostics_enabled() -> bool {
  DOM_PARSE_DIAGNOSTICS_ENABLED.load(Ordering::Acquire)
}

fn dom_parse_diagnostics_timer() -> Option<Instant> {
  dom_parse_diagnostics_enabled().then(Instant::now)
}

fn with_dom_parse_diagnostics(f: impl FnOnce(&mut DomParseDiagnostics)) {
  if !dom_parse_diagnostics_enabled() {
    return;
  }

  if let Ok(mut diag) = dom_parse_diagnostics_cell().lock() {
    f(&mut diag);
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

/// Mapping between light DOM nodes and their assigned slots within shadow roots.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SlotAssignment {
  /// For each shadow root, the slots it exposes and their assigned node ids.
  pub shadow_to_slots: HashMap<usize, HashMap<String, Vec<usize>>>,
  /// For each slot element id, the ordered list of assigned node ids.
  pub slot_to_nodes: HashMap<usize, Vec<usize>>,
  /// For each assigned node id, which slot it was assigned to.
  pub node_to_slot: HashMap<usize, AssignedSlot>,
}

/// Slot destination for an assigned light DOM node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignedSlot {
  pub slot_name: String,
  pub slot_node_id: usize,
  pub shadow_root_id: usize,
}

#[derive(Debug, Clone)]
pub enum DomNodeType {
  Document {
    quirks_mode: QuirksMode,
  },
  ShadowRoot {
    mode: ShadowRootMode,
    delegates_focus: bool,
  },
  Slot {
    namespace: String,
    attributes: Vec<(String, String)>,
    assigned: bool,
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
static ANCESTOR_BLOOM_ENV_INITIALIZED: OnceLock<()> = OnceLock::new();
static ANCESTOR_BLOOM_ENABLED: AtomicBool = AtomicBool::new(true);
static SELECTOR_CACHE_EPOCH: AtomicUsize = AtomicUsize::new(1);
const SELECTOR_BLOOM_SUMMARY_BITS: usize = 256;
const SELECTOR_BLOOM_SUMMARY_WORDS: usize = SELECTOR_BLOOM_SUMMARY_BITS / 64;

pub(crate) fn selector_bloom_enabled() -> bool {
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

pub(crate) fn ancestor_bloom_enabled() -> bool {
  ANCESTOR_BLOOM_ENV_INITIALIZED.get_or_init(|| {
    if let Ok(value) = std::env::var("FASTR_ANCESTOR_BLOOM") {
      if value.trim() == "0" {
        ANCESTOR_BLOOM_ENABLED.store(false, Ordering::Relaxed);
      }
    }
  });
  ANCESTOR_BLOOM_ENABLED.load(Ordering::Relaxed)
}

/// Toggle the cascade ancestor bloom filter for benchmarking/testing.
pub fn set_ancestor_bloom_enabled(enabled: bool) {
  ANCESTOR_BLOOM_ENV_INITIALIZED.get_or_init(|| ());
  ANCESTOR_BLOOM_ENABLED.store(enabled, Ordering::Relaxed);
}

/// Returns a monotonically increasing epoch for selector caches.
pub fn next_selector_cache_epoch() -> usize {
  SELECTOR_CACHE_EPOCH.fetch_add(1, Ordering::Relaxed)
}

#[inline]
fn selector_bloom_hash(value: &str) -> u32 {
  crate::css::types::selector_hash(value) & selectors::bloom::BLOOM_HASH_MASK
}

fn node_is_html_element(node: &DomNode) -> bool {
  matches!(
    node.node_type,
    DomNodeType::Element { ref namespace, .. } | DomNodeType::Slot { ref namespace, .. }
      if namespace.is_empty() || namespace == HTML_NAMESPACE
  )
}

fn to_ascii_lowercase_cow(s: &str) -> std::borrow::Cow<'_, str> {
  if let Some(first_uppercase) = s.bytes().position(|byte| byte >= b'A' && byte <= b'Z') {
    let mut string = s.to_owned();
    string[first_uppercase..].make_ascii_lowercase();
    string.into()
  } else {
    s.into()
  }
}

fn add_selector_bloom_hashes(node: &DomNode, add: &mut impl FnMut(u32)) {
  if !node.is_element() {
    return;
  }

  if let Some(namespace) = node.namespace() {
    add(selector_bloom_hash(namespace));
    // Treat missing namespaces as HTML for selector matching (see `ElementRef::has_namespace`).
    if namespace.is_empty() {
      add(selector_bloom_hash(HTML_NAMESPACE));
    }
  }

  let is_html = node_is_html_element(node);
  if let Some(tag) = node.tag_name() {
    if is_html {
      let has_upper = tag.bytes().any(|b| b.is_ascii_uppercase());
      if has_upper {
        let lower = tag.to_ascii_lowercase();
        add(selector_bloom_hash(&lower));
        add(selector_bloom_hash(tag));
      } else {
        // Most HTML tag names are already lowercase (html5ever lowercases), so avoid allocating.
        add(selector_bloom_hash(tag));
      }
    } else {
      add(selector_bloom_hash(tag));
      if tag.bytes().any(|b| b.is_ascii_uppercase()) {
        let lower = tag.to_ascii_lowercase();
        add(selector_bloom_hash(&lower));
      }
    }
  }

  let mut saw_id = false;
  let mut saw_class = false;
  for (name, value) in node.attributes_iter() {
    if !saw_id && name.eq_ignore_ascii_case("id") {
      add(selector_bloom_hash(value));
      saw_id = true;
    }
    if !saw_class && name.eq_ignore_ascii_case("class") {
      for class in value.split_ascii_whitespace() {
        add(selector_bloom_hash(class));
      }
      saw_class = true;
    }

    add(selector_bloom_hash(name));
    if name.bytes().any(|b| b.is_ascii_uppercase()) {
      let lower = name.to_ascii_lowercase();
      add(selector_bloom_hash(&lower));
    }
  }
}

pub(crate) fn for_each_ancestor_bloom_hash(
  node: &DomNode,
  quirks_mode: QuirksMode,
  mut add: impl FnMut(u32),
) {
  if !node.is_element() {
    return;
  }

  let is_html = node_is_html_element(node);
  if let Some(tag) = node.tag_name() {
    let tag_key = if is_html {
      to_ascii_lowercase_cow(tag)
    } else {
      tag.into()
    };
    add(selector_bloom_hash(tag_key.as_ref()));
  }

  let quirks_case_fold = matches!(quirks_mode, QuirksMode::Quirks) && is_html;
  let mut saw_id = false;
  let mut saw_class = false;
  for (name, value) in node.attributes_iter() {
    let lower = to_ascii_lowercase_cow(name);
    add(selector_bloom_hash(lower.as_ref()));

    if !saw_id && name.eq_ignore_ascii_case("id") {
      saw_id = true;
      if quirks_case_fold {
        let lower = to_ascii_lowercase_cow(value);
        add(selector_bloom_hash(lower.as_ref()));
        if lower.as_ref() != value {
          add(selector_bloom_hash(value));
        }
      } else {
        add(selector_bloom_hash(value));
      }
      continue;
    }

    if !saw_class && name.eq_ignore_ascii_case("class") {
      saw_class = true;
      for class in value.split_ascii_whitespace() {
        if quirks_case_fold {
          let lower = to_ascii_lowercase_cow(class);
          add(selector_bloom_hash(lower.as_ref()));
          if lower.as_ref() != class {
            add(selector_bloom_hash(class));
          }
        } else {
          add(selector_bloom_hash(class));
        }
      }
    }
  }
}
static HAS_EVALS: AtomicU64 = AtomicU64::new(0);
static HAS_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static HAS_PRUNES: AtomicU64 = AtomicU64::new(0);
static HAS_FILTER_PRUNES: AtomicU64 = AtomicU64::new(0);
static HAS_RELATIVE_EVALS: AtomicU64 = AtomicU64::new(0);

#[cfg(test)]
thread_local! {
  static HAS_COUNTERS: std::cell::Cell<HasCounters> = std::cell::Cell::new(HasCounters::default());
}

#[inline]
fn record_has_eval() {
  #[cfg(test)]
  HAS_COUNTERS.with(|c| {
    let mut counters = c.get();
    counters.evals += 1;
    c.set(counters);
  });
  #[cfg(not(test))]
  HAS_EVALS.fetch_add(1, Ordering::Relaxed);
}

#[inline]
fn record_has_cache_hit() {
  #[cfg(test)]
  HAS_COUNTERS.with(|c| {
    let mut counters = c.get();
    counters.cache_hits += 1;
    c.set(counters);
  });
  #[cfg(not(test))]
  HAS_CACHE_HITS.fetch_add(1, Ordering::Relaxed);
}

#[inline]
fn record_has_prune() {
  #[cfg(test)]
  HAS_COUNTERS.with(|c| {
    let mut counters = c.get();
    counters.prunes += 1;
    c.set(counters);
  });
  #[cfg(not(test))]
  HAS_PRUNES.fetch_add(1, Ordering::Relaxed);
}

#[inline]
fn record_has_filter_prune() {
  #[cfg(test)]
  HAS_COUNTERS.with(|c| {
    let mut counters = c.get();
    counters.filter_prunes += 1;
    c.set(counters);
  });
  #[cfg(not(test))]
  HAS_FILTER_PRUNES.fetch_add(1, Ordering::Relaxed);
}

#[inline]
fn record_has_relative_eval() {
  #[cfg(test)]
  HAS_COUNTERS.with(|c| {
    let mut counters = c.get();
    counters.evaluated += 1;
    c.set(counters);
  });
  #[cfg(not(test))]
  HAS_RELATIVE_EVALS.fetch_add(1, Ordering::Relaxed);
}

#[derive(Debug, Clone, Copy, Default)]
pub struct HasCounters {
  pub evals: u64,
  pub cache_hits: u64,
  /// Bloom-summary based prunes plus fast-reject bloom filters.
  pub prunes: u64,
  /// Prunes coming from the subtree bloom filters built on demand.
  pub filter_prunes: u64,
  /// Relative selector evaluations that were executed after pruning and caching.
  pub evaluated: u64,
}

impl HasCounters {
  pub fn summary_prunes(&self) -> u64 {
    self.prunes.saturating_sub(self.filter_prunes)
  }
}

pub fn reset_has_counters() {
  #[cfg(test)]
  HAS_COUNTERS.with(|c| c.set(HasCounters::default()));
  #[cfg(not(test))]
  {
    HAS_EVALS.store(0, Ordering::Relaxed);
    HAS_CACHE_HITS.store(0, Ordering::Relaxed);
    HAS_PRUNES.store(0, Ordering::Relaxed);
    HAS_FILTER_PRUNES.store(0, Ordering::Relaxed);
    HAS_RELATIVE_EVALS.store(0, Ordering::Relaxed);
  }
}

pub fn capture_has_counters() -> HasCounters {
  #[cfg(test)]
  {
    HAS_COUNTERS.with(|c| c.get())
  }
  #[cfg(not(test))]
  {
    HasCounters {
      evals: HAS_EVALS.load(Ordering::Relaxed),
      cache_hits: HAS_CACHE_HITS.load(Ordering::Relaxed),
      prunes: HAS_PRUNES.load(Ordering::Relaxed),
      filter_prunes: HAS_FILTER_PRUNES.load(Ordering::Relaxed),
      evaluated: HAS_RELATIVE_EVALS.load(Ordering::Relaxed),
    }
  }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SelectorBloomSummary {
  bits: [u64; SELECTOR_BLOOM_SUMMARY_WORDS],
}

impl SelectorBloomSummary {
  fn insert_hash(&mut self, hash: u32) {
    let slot_a = (hash as usize) & (SELECTOR_BLOOM_SUMMARY_BITS - 1);
    let slot_b = ((hash >> 8) as usize) & (SELECTOR_BLOOM_SUMMARY_BITS - 1);
    self.insert_slot(slot_a);
    self.insert_slot(slot_b);
  }

  fn insert_slot(&mut self, slot: usize) {
    let (idx, bit) = (slot / 64, slot % 64);
    self.bits[idx] |= 1u64 << bit;
  }

  pub(crate) fn contains_hash(&self, hash: u32) -> bool {
    let slot_a = (hash as usize) & (SELECTOR_BLOOM_SUMMARY_BITS - 1);
    let slot_b = ((hash >> 8) as usize) & (SELECTOR_BLOOM_SUMMARY_BITS - 1);
    self.contains_slot(slot_a) && self.contains_slot(slot_b)
  }

  fn contains_slot(&self, slot: usize) -> bool {
    let (idx, bit) = (slot / 64, slot % 64);
    (self.bits[idx] & (1u64 << bit)) != 0
  }

  fn merge(&mut self, other: &Self) {
    for (dst, src) in self.bits.iter_mut().zip(other.bits.iter()) {
      *dst |= *src;
    }
  }
}

/// A dense, node-id indexed store of selector bloom summaries.
///
/// Contract: `node_id` comes from [`enumerate_dom_ids`] and starts at 1. Index 0 is unused.
#[derive(Debug, Clone)]
pub struct SelectorBloomStore {
  summaries: Vec<SelectorBloomSummary>,
}

impl SelectorBloomStore {
  pub fn summary_for_id(&self, node_id: usize) -> Option<&SelectorBloomSummary> {
    if node_id == 0 {
      return None;
    }
    self.summaries.get(node_id)
  }
}

/// Build selector bloom summaries for each element node, indexed by `node_id`.
pub fn build_selector_bloom_store(
  root: &DomNode,
  id_map: &HashMap<*const DomNode, usize>,
) -> Option<SelectorBloomStore> {
  if !selector_bloom_enabled() {
    return None;
  }

  // Keep index 0 unused so the 1-based `node_id` from `enumerate_dom_ids` can be used directly.
  //
  // We still accept `id_map` to reserve the right size up-front, but we avoid doing a pointer-keyed
  // HashMap lookup per element by assigning ids during a pre-order traversal (the same order as
  // `enumerate_dom_ids`).
  let mut summaries: Vec<SelectorBloomSummary> =
    Vec::with_capacity(id_map.len().saturating_add(1));
  summaries.push(SelectorBloomSummary::default());

  fn walk(node: &DomNode, out: &mut Vec<SelectorBloomSummary>) -> SelectorBloomSummary {
    let is_element = node.is_element();
    let id = out.len();
    out.push(SelectorBloomSummary::default());

    let mut summary = SelectorBloomSummary::default();
    if is_element {
      add_selector_bloom_hashes(node, &mut |hash| summary.insert_hash(hash));
    }

    for child in node.children.iter() {
      let child_summary = if matches!(child.node_type, DomNodeType::ShadowRoot { .. }) {
        walk(child, out);
        None
      } else {
        Some(walk(child, out))
      };
      if is_element {
        if let Some(summary_child) = child_summary.as_ref() {
          summary.merge(summary_child);
        }
      }
    }

    if is_element {
      out[id] = summary;
    }

    summary
  }

  walk(root, &mut summaries);
  debug_assert_eq!(
    summaries.len(),
    id_map.len().saturating_add(1),
    "selector bloom store should align with enumerate_dom_ids node ids"
  );
  Some(SelectorBloomStore { summaries })
}

#[cfg(test)]
type SelectorBloomMapLegacy = HashMap<*const DomNode, SelectorBloomSummary>;

#[cfg(test)]
fn build_selector_bloom_map_legacy(root: &DomNode) -> Option<SelectorBloomMapLegacy> {
  if !selector_bloom_enabled() {
    return None;
  }

  fn walk(node: &DomNode, map: &mut SelectorBloomMapLegacy) -> SelectorBloomSummary {
    let mut summary = SelectorBloomSummary::default();
    if node.is_element() {
      add_selector_bloom_hashes(node, &mut |hash| summary.insert_hash(hash));
    }

    for child in node.children.iter() {
      let child_summary = if matches!(child.node_type, DomNodeType::ShadowRoot { .. }) {
        walk(child, map);
        None
      } else {
        Some(walk(child, map))
      };
      if node.is_element() {
        if let Some(summary_child) = child_summary.as_ref() {
          summary.merge(summary_child);
        }
      }
    }

    if node.is_element() {
      map.insert(node as *const DomNode, summary);
    }

    summary
  }

  let mut blooms: SelectorBloomMapLegacy = SelectorBloomMapLegacy::new();
  walk(root, &mut blooms);
  Some(blooms)
}

#[derive(Clone, Copy, Debug)]
pub struct SiblingPosition {
  pub index: usize,
  pub len: usize,
  pub type_index: usize,
  pub type_len: usize,
}

#[derive(Debug, Default)]
pub struct SiblingListCache {
  _epoch: usize,
  parents: RefCell<HashMap<*const DomNode, ParentSiblingList>>,
}

#[derive(Debug, Default)]
struct ParentSiblingList {
  positions: HashMap<*const DomNode, SiblingPosition>,
  elements: Vec<*const DomNode>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct SiblingTypeKey {
  namespace: String,
  local_name: String,
}

impl SiblingListCache {
  pub fn new(epoch: usize) -> Self {
    Self {
      _epoch: epoch,
      parents: RefCell::new(HashMap::new()),
    }
  }

  pub fn position(
    &self,
    parent: &DomNode,
    child: &DomNode,
    context: &mut selectors::matching::MatchingContext<FastRenderSelectorImpl>,
  ) -> Option<SiblingPosition> {
    let parent_ptr = parent as *const DomNode;
    {
      let parents = self.parents.borrow();
      if let Some(entry) = parents.get(&parent_ptr) {
        if let Some(position) = entry.positions.get(&(child as *const DomNode)) {
          return Some(*position);
        }
      }
    }

    let entry = build_parent_sibling_list(parent, context)?;
    let mut parents = self.parents.borrow_mut();
    let cached = parents.entry(parent_ptr).or_insert(entry);
    cached.positions.get(&(child as *const DomNode)).copied()
  }

  pub fn ordered_children(
    &self,
    parent: &DomNode,
    context: &mut selectors::matching::MatchingContext<FastRenderSelectorImpl>,
  ) -> Option<Vec<*const DomNode>> {
    let parent_ptr = parent as *const DomNode;
    {
      let parents = self.parents.borrow();
      if let Some(entry) = parents.get(&parent_ptr) {
        return Some(entry.elements.clone());
      }
    }

    let entry = build_parent_sibling_list(parent, context)?;
    let elements = entry.elements.clone();
    self.parents.borrow_mut().insert(parent_ptr, entry);
    Some(elements)
  }
}

const ELEMENT_ATTR_CACHE_ATTR_INDEX_THRESHOLD: usize = 10;
const ELEMENT_ATTR_CACHE_CLASS_INDEX_THRESHOLD: usize = 8;
const ELEMENT_ATTR_CACHE_FNV_OFFSET_BASIS: u64 = 14695981039346656037;
const ELEMENT_ATTR_CACHE_FNV_PRIME: u64 = 1099511628211;

#[derive(Default)]
struct ElementAttrCacheHasher(u64);

impl Hasher for ElementAttrCacheHasher {
  fn write(&mut self, bytes: &[u8]) {
    let mut hash = ELEMENT_ATTR_CACHE_FNV_OFFSET_BASIS;
    for &byte in bytes {
      hash ^= byte as u64;
      hash = hash.wrapping_mul(ELEMENT_ATTR_CACHE_FNV_PRIME);
    }
    self.0 = hash;
  }

  fn write_u64(&mut self, i: u64) {
    self.0 = i;
  }

  fn write_usize(&mut self, i: usize) {
    self.0 = i as u64;
  }

  fn finish(&self) -> u64 {
    self.0
  }
}

type ElementAttrCacheBuildHasher = BuildHasherDefault<ElementAttrCacheHasher>;

#[inline]
fn element_attr_cache_hash_ascii_lowercase(value: &str) -> u64 {
  let mut hash = ELEMENT_ATTR_CACHE_FNV_OFFSET_BASIS;
  for byte in value.bytes() {
    let folded = if byte >= b'A' && byte <= b'Z' {
      byte + 32
    } else {
      byte
    };
    hash ^= folded as u64;
    hash = hash.wrapping_mul(ELEMENT_ATTR_CACHE_FNV_PRIME);
  }
  hash
}

#[inline]
fn element_attr_cache_hash_str(value: &str) -> u64 {
  let mut hash = ELEMENT_ATTR_CACHE_FNV_OFFSET_BASIS;
  for &byte in value.as_bytes() {
    hash ^= byte as u64;
    hash = hash.wrapping_mul(ELEMENT_ATTR_CACHE_FNV_PRIME);
  }
  hash
}

#[inline]
fn element_attr_cache_name_hash(name: &str, is_html: bool) -> u64 {
  if is_html {
    element_attr_cache_hash_ascii_lowercase(name)
  } else {
    element_attr_cache_hash_str(name)
  }
}

#[inline]
fn element_attr_cache_name_matches(actual: &str, expected: &str, is_html: bool) -> bool {
  if is_html {
    actual.eq_ignore_ascii_case(expected)
  } else {
    actual == expected
  }
}

#[derive(Debug, Clone)]
enum CachedClassTokens {
  None,
  Unparsed(*const str),
  Parsed {
    raw: *const str,
    ranges: Box<[std::ops::Range<usize>]>,
    index_sensitive: CachedClassIndex,
    index_ascii: CachedClassIndex,
  },
}

#[derive(Debug, Clone)]
enum CachedClassIndex {
  Pending,
  Disabled,
  Built(HashMap<u64, ClassBucket, ElementAttrCacheBuildHasher>),
}

#[derive(Debug, Clone)]
enum ClassBucket {
  Single(usize),
  Multi(Vec<usize>),
}

#[derive(Debug, Clone)]
enum CachedAttrIndex {
  Pending,
  Disabled,
  Built(HashMap<u64, AttrBucket, ElementAttrCacheBuildHasher>),
}

#[derive(Debug, Clone)]
enum AttrBucket {
  Single(usize),
  Multi(Vec<usize>),
}

#[derive(Debug, Clone)]
struct ElementAttrCacheEntry {
  is_html: bool,
  id: Option<*const str>,
  class: CachedClassTokens,
  attr_index: CachedAttrIndex,
}

impl ElementAttrCacheEntry {
  fn new(node: &DomNode) -> Self {
    let is_html = node_is_html_element(node);
    let attrs: &[(String, String)] = match &node.node_type {
      DomNodeType::Element { attributes, .. } => attributes,
      DomNodeType::Slot { attributes, .. } => attributes,
      _ => &[],
    };

    let mut id: Option<*const str> = None;
    let mut class: Option<*const str> = None;
    for (name, value) in attrs.iter() {
      if id.is_none() && element_attr_cache_name_matches(name, "id", is_html) {
        id = Some(value.as_str() as *const str);
      }
      if class.is_none() && element_attr_cache_name_matches(name, "class", is_html) {
        class = Some(value.as_str() as *const str);
      }
      if id.is_some() && class.is_some() {
        break;
      }
    }

    let class = match class {
      Some(raw) => CachedClassTokens::Unparsed(raw),
      None => CachedClassTokens::None,
    };

    Self {
      is_html,
      id,
      class,
      attr_index: CachedAttrIndex::Pending,
    }
  }

  fn ensure_class_parsed(&mut self) {
    let raw_ptr = match &self.class {
      CachedClassTokens::Unparsed(ptr) => Some(*ptr),
      _ => None,
    };

    let Some(ptr) = raw_ptr else {
      return;
    };

    let raw: &str = unsafe { &*ptr };
    let base_ptr = raw.as_ptr() as usize;
    let mut ranges: Vec<std::ops::Range<usize>> = Vec::new();
    for token in raw.split_ascii_whitespace() {
      let start = token.as_ptr() as usize - base_ptr;
      ranges.push(start..start + token.len());
    }
    let ranges = ranges.into_boxed_slice();
    let index_state = if ranges.len() >= ELEMENT_ATTR_CACHE_CLASS_INDEX_THRESHOLD {
      CachedClassIndex::Pending
    } else {
      CachedClassIndex::Disabled
    };

    self.class = CachedClassTokens::Parsed {
      raw: ptr,
      ranges,
      index_sensitive: index_state.clone(),
      index_ascii: index_state,
    };
  }

  fn class_index<'a>(
    index: &'a mut CachedClassIndex,
    raw: &str,
    ranges: &[std::ops::Range<usize>],
    case_sensitivity: CaseSensitivity,
  ) -> Option<&'a HashMap<u64, ClassBucket, ElementAttrCacheBuildHasher>> {
    if matches!(index, CachedClassIndex::Pending) {
      let mut map: HashMap<u64, ClassBucket, ElementAttrCacheBuildHasher> = HashMap::default();
      for (idx, range) in ranges.iter().enumerate() {
        let token = &raw[range.start..range.end];
        if token.is_empty() {
          continue;
        }
        let hash = match case_sensitivity {
          CaseSensitivity::CaseSensitive => element_attr_cache_hash_str(token),
          CaseSensitivity::AsciiCaseInsensitive => element_attr_cache_hash_ascii_lowercase(token),
        };
        match map.entry(hash) {
          std::collections::hash_map::Entry::Vacant(entry) => {
            entry.insert(ClassBucket::Single(idx));
          }
          std::collections::hash_map::Entry::Occupied(mut entry) => {
            let bucket = entry.get_mut();
            let is_dup = match bucket {
              ClassBucket::Single(existing) => {
                let existing_range = ranges.get(*existing);
                existing_range.is_some_and(|r| match case_sensitivity {
                  CaseSensitivity::CaseSensitive => &raw[r.start..r.end] == token,
                  CaseSensitivity::AsciiCaseInsensitive => raw[r.start..r.end].eq_ignore_ascii_case(token),
                })
              }
              ClassBucket::Multi(existing) => existing.iter().any(|existing| {
                let Some(r) = ranges.get(*existing) else {
                  return false;
                };
                match case_sensitivity {
                  CaseSensitivity::CaseSensitive => &raw[r.start..r.end] == token,
                  CaseSensitivity::AsciiCaseInsensitive => raw[r.start..r.end].eq_ignore_ascii_case(token),
                }
              }),
            };
            if is_dup {
              continue;
            }
            match bucket {
              ClassBucket::Single(existing) => {
                let prev = *existing;
                *bucket = ClassBucket::Multi(vec![prev, idx]);
              }
              ClassBucket::Multi(existing) => existing.push(idx),
            }
          }
        }
      }
      *index = CachedClassIndex::Built(map);
    }

    match index {
      CachedClassIndex::Built(map) => Some(map),
      _ => None,
    }
  }

  fn has_class(&mut self, class: &str, case_sensitivity: CaseSensitivity) -> bool {
    self.ensure_class_parsed();
    let CachedClassTokens::Parsed {
      raw,
      ranges,
      index_sensitive,
      index_ascii,
    } = &mut self.class
    else {
      return false;
    };

    let raw: &str = unsafe { &**raw };
    let ranges = ranges.as_ref();

    let index = match case_sensitivity {
      CaseSensitivity::CaseSensitive => {
        Self::class_index(index_sensitive, raw, ranges, CaseSensitivity::CaseSensitive)
      }
      CaseSensitivity::AsciiCaseInsensitive => {
        Self::class_index(index_ascii, raw, ranges, CaseSensitivity::AsciiCaseInsensitive)
      }
    };

    if let Some(index) = index {
      let query_hash = match case_sensitivity {
        CaseSensitivity::CaseSensitive => element_attr_cache_hash_str(class),
        CaseSensitivity::AsciiCaseInsensitive => element_attr_cache_hash_ascii_lowercase(class),
      };
      if let Some(bucket) = index.get(&query_hash) {
        let indices: &[usize] = match bucket {
          ClassBucket::Single(idx) => std::slice::from_ref(idx),
          ClassBucket::Multi(list) => list.as_slice(),
        };
        for idx in indices {
          let Some(range) = ranges.get(*idx) else {
            continue;
          };
          let token = &raw[range.start..range.end];
          let matches = match case_sensitivity {
            CaseSensitivity::CaseSensitive => token == class,
            CaseSensitivity::AsciiCaseInsensitive => token.eq_ignore_ascii_case(class),
          };
          if matches {
            return true;
          }
        }
        return false;
      }
    }

    match case_sensitivity {
      CaseSensitivity::CaseSensitive => ranges
        .iter()
        .any(|range| &raw[range.start..range.end] == class),
      CaseSensitivity::AsciiCaseInsensitive => ranges
        .iter()
        .any(|range| raw[range.start..range.end].eq_ignore_ascii_case(class)),
    }
  }

  fn attr_index<'a>(
    &'a mut self,
    node: &DomNode,
  ) -> Option<(&'a HashMap<u64, AttrBucket, ElementAttrCacheBuildHasher>, bool)> {
    if matches!(self.attr_index, CachedAttrIndex::Pending) {
      let attrs: &[(String, String)] = match &node.node_type {
        DomNodeType::Element { attributes, .. } => attributes,
        DomNodeType::Slot { attributes, .. } => attributes,
        _ => &[],
      };
      if attrs.len() < ELEMENT_ATTR_CACHE_ATTR_INDEX_THRESHOLD {
        self.attr_index = CachedAttrIndex::Disabled;
      } else {
        let mut map: HashMap<u64, AttrBucket, ElementAttrCacheBuildHasher> = HashMap::default();
        for (idx, (name, _)) in attrs.iter().enumerate() {
          let hash = element_attr_cache_name_hash(name, self.is_html);
          match map.entry(hash) {
            std::collections::hash_map::Entry::Vacant(entry) => {
              entry.insert(AttrBucket::Single(idx));
            }
            std::collections::hash_map::Entry::Occupied(mut entry) => {
              let bucket = entry.get_mut();
              let is_dup = match bucket {
                AttrBucket::Single(existing) => {
                  let existing_name = attrs.get(*existing).map(|(n, _)| n.as_str()).unwrap_or("");
                  element_attr_cache_name_matches(existing_name, name, self.is_html)
                }
                AttrBucket::Multi(existing) => existing.iter().any(|existing| {
                  let existing_name =
                    attrs.get(*existing).map(|(n, _)| n.as_str()).unwrap_or("");
                  element_attr_cache_name_matches(existing_name, name, self.is_html)
                }),
              };
              if is_dup {
                continue;
              }
              match bucket {
                AttrBucket::Single(existing) => {
                  let prev = *existing;
                  *bucket = AttrBucket::Multi(vec![prev, idx]);
                }
                AttrBucket::Multi(existing) => existing.push(idx),
              }
            }
          }
        }
        self.attr_index = CachedAttrIndex::Built(map);
      }
    }

    match &self.attr_index {
      CachedAttrIndex::Built(map) => Some((map, self.is_html)),
      _ => None,
    }
  }
}

#[derive(Debug)]
pub struct ElementAttrCache {
  _epoch: usize,
  entries: RefCell<HashMap<*const DomNode, ElementAttrCacheEntry, ElementAttrCacheBuildHasher>>,
}

impl ElementAttrCache {
  pub fn new(epoch: usize) -> Self {
    Self {
      _epoch: epoch,
      entries: RefCell::new(HashMap::default()),
    }
  }

  pub fn clear(&self) {
    self.entries.borrow_mut().clear();
  }

  fn entry_mut<'a>(&'a self, node: &DomNode) -> RefMut<'a, ElementAttrCacheEntry> {
    let ptr = node as *const DomNode;
    RefMut::map(self.entries.borrow_mut(), |entries| {
      entries.entry(ptr).or_insert_with(|| ElementAttrCacheEntry::new(node))
    })
  }

  pub fn has_id(&self, node: &DomNode, id: &str, case_sensitivity: CaseSensitivity) -> bool {
    let entry = self.entry_mut(node);
    let Some(id_ptr) = entry.id else {
      return false;
    };
    let actual: &str = unsafe { &*id_ptr };
    match case_sensitivity {
      CaseSensitivity::CaseSensitive => actual == id,
      CaseSensitivity::AsciiCaseInsensitive => actual.eq_ignore_ascii_case(id),
    }
  }

  pub fn has_class(
    &self,
    node: &DomNode,
    class: &str,
    case_sensitivity: CaseSensitivity,
  ) -> bool {
    self.entry_mut(node).has_class(class, case_sensitivity)
  }

  pub fn attr_value<'a>(&self, node: &'a DomNode, name: &str) -> Option<&'a str> {
    let mut entry = self.entry_mut(node);
    let attrs: &'a [(String, String)] = match &node.node_type {
      DomNodeType::Element { attributes, .. } => attributes,
      DomNodeType::Slot { attributes, .. } => attributes,
      _ => return None,
    };

    let query_hash = element_attr_cache_name_hash(name, entry.is_html);
    if let Some((index, is_html)) = entry.attr_index(node) {
      if let Some(bucket) = index.get(&query_hash) {
        let indices: &[usize] = match bucket {
          AttrBucket::Single(idx) => std::slice::from_ref(idx),
          AttrBucket::Multi(list) => list.as_slice(),
        };
        for idx in indices {
          let (attr_name, attr_value) = attrs.get(*idx)?;
          if element_attr_cache_name_matches(attr_name, name, is_html) {
            return Some(attr_value.as_str());
          }
        }
        return None;
      }
    }

    for (attr_name, attr_value) in attrs.iter() {
      if element_attr_cache_name_matches(attr_name, name, entry.is_html) {
        return Some(attr_value.as_str());
      }
    }

    None
  }
}

fn sibling_type_key(node: &DomNode) -> Option<SiblingTypeKey> {
  let tag = node.tag_name()?;
  let namespace = node.namespace().unwrap_or("").to_string();
  let is_html = node_is_html_element(node);
  let local_name = if is_html {
    tag.to_ascii_lowercase()
  } else {
    tag.to_string()
  };
  Some(SiblingTypeKey {
    namespace,
    local_name,
  })
}

fn build_parent_sibling_list(
  parent: &DomNode,
  context: &mut selectors::matching::MatchingContext<FastRenderSelectorImpl>,
) -> Option<ParentSiblingList> {
  let mut deadline_counter = 0usize;
  let mut elements: Vec<(*const DomNode, SiblingTypeKey)> = Vec::new();
  for child in parent.children.iter() {
    if let Err(err) = check_active_periodic(
      &mut deadline_counter,
      NTH_DEADLINE_STRIDE,
      RenderStage::Cascade,
    ) {
      context.extra_data.record_deadline_error(err);
      return None;
    }
    if !child.is_element() {
      continue;
    }
    let Some(key) = sibling_type_key(child) else {
      continue;
    };
    elements.push((child as *const DomNode, key));
  }

  let len = elements.len();
  let mut type_totals: HashMap<SiblingTypeKey, usize> = HashMap::new();
  for (_, key) in elements.iter() {
    *type_totals.entry(key.clone()).or_insert(0) += 1;
  }
  let mut type_seen: HashMap<SiblingTypeKey, usize> = HashMap::new();
  let mut positions: HashMap<*const DomNode, SiblingPosition> = HashMap::with_capacity(len);
  for (idx, (ptr, key)) in elements.iter().enumerate() {
    let count = type_seen.entry(key.clone()).or_insert(0);
    let type_index = *count;
    *count += 1;
    let type_len = type_totals.get(key).copied().unwrap_or(0);
    positions.insert(
      *ptr,
      SiblingPosition {
        index: idx,
        len,
        type_index,
        type_len,
      },
    );
  }

  Some(ParentSiblingList {
    positions,
    elements: elements.iter().map(|(ptr, _)| *ptr).collect(),
  })
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
        let skip = tag_name.eq_ignore_ascii_case("script")
          || tag_name.eq_ignore_ascii_case("style")
          || tag_name.eq_ignore_ascii_case("template");
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
      DomNodeType::ShadowRoot { .. } | DomNodeType::Document { .. } => {
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
        let skip = tag_name.eq_ignore_ascii_case("script")
          || tag_name.eq_ignore_ascii_case("style")
          || tag_name.eq_ignore_ascii_case("template");
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
      DomNodeType::ShadowRoot { .. } | DomNodeType::Document { .. } => {
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

fn boolish(value: &str) -> bool {
  value == "1"
    || value.eq_ignore_ascii_case("true")
    || value.eq_ignore_ascii_case("yes")
    || value.eq_ignore_ascii_case("on")
    || value.eq_ignore_ascii_case("open")
}

fn data_fastr_open_state(node: &DomNode) -> Option<(bool, bool)> {
  let value = node.get_attribute_ref("data-fastr-open")?;
  if value.eq_ignore_ascii_case("false") {
    return Some((false, false));
  }
  if value.eq_ignore_ascii_case("modal") {
    return Some((true, true));
  }
  if boolish(value) {
    return Some((true, false));
  }
  None
}

fn dialog_state(node: &DomNode) -> Option<(bool, bool)> {
  if !node
    .tag_name()
    .map(|t| t.eq_ignore_ascii_case("dialog"))
    .unwrap_or(false)
  {
    return None;
  }

  let mut open = node.get_attribute_ref("open").is_some();
  let mut modal = node
    .get_attribute_ref("data-fastr-modal")
    .map(boolish)
    .unwrap_or(false);
  if let Some((open_override, modal_override)) = data_fastr_open_state(node) {
    open = open_override;
    modal |= modal_override;
  }

  if !open {
    return None;
  }

  Some((open, modal))
}

fn popover_open_assuming_popover(node: &DomNode) -> bool {
  let mut open = node.get_attribute_ref("open").is_some();
  if let Some((open_override, _)) = data_fastr_open_state(node) {
    open = open_override;
  }
  open
}

fn popover_open(node: &DomNode) -> bool {
  node.get_attribute_ref("popover").is_some() && popover_open_assuming_popover(node)
}

/// Bench helper: determine whether the DOM contains an open modal `<dialog>`.
#[doc(hidden)]
pub fn modal_dialog_present(node: &DomNode) -> bool {
  if let Some((_, modal)) = dialog_state(node) {
    if modal {
      return true;
    }
  }

  node.children.iter().any(modal_dialog_present)
}

fn set_attr(attrs: &mut Vec<(String, String)>, name: &str, value: &str) {
  if let Some((_, val)) = attrs.iter_mut().find(|(k, _)| k.eq_ignore_ascii_case(name)) {
    if val != value {
      val.clear();
      val.push_str(value);
    }
  } else {
    attrs.push((name.to_string(), value.to_string()));
  }
}

fn remove_attr(attrs: &mut Vec<(String, String)>, name: &str) {
  if let Some(idx) = attrs.iter().position(|(k, _)| k.eq_ignore_ascii_case(name)) {
    attrs.remove(idx);
  }
}

fn apply_top_layer_open_state_with_deadline(node: &mut DomNode) -> Result<bool> {
  let mut deadline_counter = 0usize;
  let mut modal_open = false;
  let mut stack = vec![node as *mut DomNode];

  while let Some(ptr) = stack.pop() {
    check_active_periodic(
      &mut deadline_counter,
      DOM_PARSE_NODE_DEADLINE_STRIDE,
      RenderStage::DomParse,
    )?;

    // Safety: `node` is mutably borrowed for the duration of this traversal, and we never mutate
    // the `children` vectors (only element attributes), so raw pointers remain stable.
    let current = unsafe { &mut *ptr };

    let dialog_info = dialog_state(current);
    if let Some((_, modal)) = dialog_info {
      modal_open |= modal;
    }

    let has_popover = current.get_attribute_ref("popover").is_some();
    let popover_is_open = has_popover && popover_open_assuming_popover(current);

    if let DomNodeType::Element {
      tag_name,
      attributes,
      ..
    } = &mut current.node_type
    {
      let is_dialog = tag_name.eq_ignore_ascii_case("dialog");
      let should_open = if is_dialog {
        dialog_info.is_some()
      } else if has_popover {
        popover_is_open
      } else {
        false
      };

      if is_dialog || has_popover {
        if should_open {
          set_attr(attributes, "open", "");
        } else {
          remove_attr(attributes, "open");
        }
      }
    }

    for child in current.children.iter_mut().rev() {
      stack.push(child as *mut DomNode);
    }
  }

  Ok(modal_open)
}

fn apply_top_layer_open_state(node: &mut DomNode) -> bool {
  let mut modal_open = false;
  let mut stack = vec![node as *mut DomNode];

  while let Some(ptr) = stack.pop() {
    // Safety: `node` is mutably borrowed for the duration of this traversal, and we never mutate
    // the `children` vectors (only element attributes), so raw pointers remain stable.
    let current = unsafe { &mut *ptr };

    let dialog_info = dialog_state(current);
    if let Some((_, modal)) = dialog_info {
      modal_open |= modal;
    }

    let has_popover = current.get_attribute_ref("popover").is_some();
    let popover_is_open = has_popover && popover_open_assuming_popover(current);

    if let DomNodeType::Element {
      tag_name,
      attributes,
      ..
    } = &mut current.node_type
    {
      let is_dialog = tag_name.eq_ignore_ascii_case("dialog");
      let should_open = if is_dialog {
        dialog_info.is_some()
      } else if has_popover {
        popover_is_open
      } else {
        false
      };

      if is_dialog || has_popover {
        if should_open {
          set_attr(attributes, "open", "");
        } else {
          remove_attr(attributes, "open");
        }
      }
    }

    for child in current.children.iter_mut().rev() {
      stack.push(child as *mut DomNode);
    }
  }

  modal_open
}

fn apply_top_layer_state_inner(node: &mut DomNode, modal_open: bool, inside_modal: bool) -> bool {
  let mut within_modal = inside_modal;
  let dialog_info = dialog_state(node);
  let has_popover = node.get_attribute_ref("popover").is_some();
  let popover_is_open = if has_popover {
    popover_open_assuming_popover(node)
  } else {
    false
  };
  let mut subtree_has_modal = within_modal;

  if let DomNodeType::Element {
    tag_name,
    attributes,
    ..
  } = &mut node.node_type
  {
    let is_dialog = tag_name.eq_ignore_ascii_case("dialog");
    let mut should_open = false;

    if is_dialog {
      if let Some((open, modal)) = dialog_info {
        should_open = open;
        if modal {
          within_modal = true;
          subtree_has_modal = true;
        }
      }
    } else if has_popover {
      should_open = popover_is_open;
    }

    if is_dialog || has_popover {
      if should_open {
        set_attr(attributes, "open", "");
      } else {
        remove_attr(attributes, "open");
      }
    }
  }

  let child_modal = within_modal;
  for child in node.children.iter_mut() {
    let child_contains_modal = apply_top_layer_state_inner(child, modal_open, child_modal);
    subtree_has_modal |= child_contains_modal;
  }

  if modal_open {
    if let DomNodeType::Element { attributes, .. } = &mut node.node_type {
      if !subtree_has_modal {
        set_attr(attributes, "data-fastr-inert", "true");
      }
    }
  }

  subtree_has_modal
}

/// Bench helper: apply dialog/popover open state and `data-fastr-inert` propagation.
#[doc(hidden)]
pub fn apply_top_layer_state(node: &mut DomNode, modal_open: bool) {
  let _ = apply_top_layer_state_inner(node, modal_open, false);
}

fn open_modal_dialog_after_open_state(node: &DomNode) -> bool {
  let is_dialog = node
    .tag_name()
    .map(|t| t.eq_ignore_ascii_case("dialog"))
    .unwrap_or(false);
  if !is_dialog {
    return false;
  }
  if node.get_attribute_ref("open").is_none() {
    return false;
  }
  node
    .get_attribute_ref("data-fastr-modal")
    .map(boolish)
    .unwrap_or(false)
    || node
      .get_attribute_ref("data-fastr-open")
      .map(|v| v.eq_ignore_ascii_case("modal"))
      .unwrap_or(false)
}

fn apply_top_layer_inert_state_with_deadline(node: &mut DomNode) -> Result<()> {
  struct Frame {
    node: *mut DomNode,
    inside_modal: bool,
    entered: bool,
    next_child: usize,
    within_modal: bool,
    subtree_has_modal: bool,
  }

  let mut deadline_counter = 0usize;
  let mut stack = vec![Frame {
    node: node as *mut _,
    inside_modal: false,
    entered: false,
    next_child: 0,
    within_modal: false,
    subtree_has_modal: false,
  }];

  while let Some(mut frame) = stack.pop() {
    // Safety: `node` is mutably borrowed for the duration of the traversal, and we never mutate the
    // `children` vectors, so raw pointers remain stable for this post-order walk.
    let current = unsafe { &mut *frame.node };

    if !frame.entered {
      check_active_periodic(
        &mut deadline_counter,
        DOM_PARSE_NODE_DEADLINE_STRIDE,
        RenderStage::DomParse,
      )?;

      frame.entered = true;
      frame.within_modal = frame.inside_modal;
      frame.subtree_has_modal = frame.within_modal;

      if !frame.within_modal {
        if open_modal_dialog_after_open_state(current) {
          frame.within_modal = true;
          frame.subtree_has_modal = true;
        }
      }
    }

    if frame.next_child < current.children.len() {
      let child_ptr = &mut current.children[frame.next_child] as *mut DomNode;
      frame.next_child += 1;
      let child_inside_modal = frame.within_modal;

      stack.push(frame);
      stack.push(Frame {
        node: child_ptr,
        inside_modal: child_inside_modal,
        entered: false,
        next_child: 0,
        within_modal: false,
        subtree_has_modal: false,
      });
      continue;
    }

    if let DomNodeType::Element { attributes, .. } = &mut current.node_type {
      if !frame.subtree_has_modal {
        set_attr(attributes, "data-fastr-inert", "true");
      }
    }

    let subtree_has_modal = frame.subtree_has_modal;
    if let Some(parent) = stack.last_mut() {
      parent.subtree_has_modal |= subtree_has_modal;
    }
  }

  Ok(())
}

fn apply_top_layer_inert_state(node: &mut DomNode) {
  struct Frame {
    node: *mut DomNode,
    inside_modal: bool,
    entered: bool,
    next_child: usize,
    within_modal: bool,
    subtree_has_modal: bool,
  }

  let mut stack = vec![Frame {
    node: node as *mut _,
    inside_modal: false,
    entered: false,
    next_child: 0,
    within_modal: false,
    subtree_has_modal: false,
  }];

  while let Some(mut frame) = stack.pop() {
    // Safety: `node` is mutably borrowed for the duration of the traversal, and we never mutate the
    // `children` vectors, so raw pointers remain stable for this post-order walk.
    let current = unsafe { &mut *frame.node };

    if !frame.entered {
      frame.entered = true;
      frame.within_modal = frame.inside_modal;
      frame.subtree_has_modal = frame.within_modal;

      if !frame.within_modal {
        if open_modal_dialog_after_open_state(current) {
          frame.within_modal = true;
          frame.subtree_has_modal = true;
        }
      }
    }

    if frame.next_child < current.children.len() {
      let child_ptr = &mut current.children[frame.next_child] as *mut DomNode;
      frame.next_child += 1;
      let child_inside_modal = frame.within_modal;

      stack.push(frame);
      stack.push(Frame {
        node: child_ptr,
        inside_modal: child_inside_modal,
        entered: false,
        next_child: 0,
        within_modal: false,
        subtree_has_modal: false,
      });
      continue;
    }

    if let DomNodeType::Element { attributes, .. } = &mut current.node_type {
      if !frame.subtree_has_modal {
        set_attr(attributes, "data-fastr-inert", "true");
      }
    }

    let subtree_has_modal = frame.subtree_has_modal;
    if let Some(parent) = stack.last_mut() {
      parent.subtree_has_modal |= subtree_has_modal;
    }
  }
}

/// Applies dialog/popover open state, then propagates `data-fastr-inert` if a modal dialog is open.
///
/// This is a benchmarking helper mirroring the render pipeline's top-layer preprocessing
/// without deadline checks.
#[doc(hidden)]
pub fn apply_top_layer_state_auto(node: &mut DomNode) -> bool {
  let modal_open = apply_top_layer_open_state(node);
  if modal_open {
    apply_top_layer_inert_state(node);
  }
  modal_open
}

pub(crate) fn apply_top_layer_state_with_deadline(node: &mut DomNode) -> Result<()> {
  // Apply `<dialog>`/`[popover]` open state in a single pre-order walk while detecting whether the
  // document contains an open modal dialog. Inert propagation depends on knowing that global modal
  // state, so we only run the heavier post-order inert pass when needed.
  let modal_open = apply_top_layer_open_state_with_deadline(node)?;
  if modal_open {
    apply_top_layer_inert_state_with_deadline(node)?;
  }
  Ok(())
}

pub fn parse_html(html: &str) -> Result<DomNode> {
  parse_html_with_options(html, DomParseOptions::default())
}

fn map_quirks_mode(mode: HtmlQuirksMode) -> QuirksMode {
  match mode {
    HtmlQuirksMode::Quirks => QuirksMode::Quirks,
    HtmlQuirksMode::LimitedQuirks => QuirksMode::LimitedQuirks,
    HtmlQuirksMode::NoQuirks => QuirksMode::NoQuirks,
  }
}

struct DeadlineCheckedRead<R> {
  inner: R,
  deadline_counter: usize,
}

impl<R> DeadlineCheckedRead<R> {
  fn new(inner: R) -> Self {
    Self {
      inner,
      deadline_counter: 0,
    }
  }
}

impl<R: io::Read> io::Read for DeadlineCheckedRead<R> {
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    if let Err(err) = check_active_periodic(
      &mut self.deadline_counter,
      DOM_PARSE_READ_DEADLINE_STRIDE,
      RenderStage::DomParse,
    ) {
      return Err(io::Error::new(io::ErrorKind::TimedOut, err));
    }

    let len = buf.len().min(DOM_PARSE_READ_MAX_CHUNK_BYTES);
    self.inner.read(&mut buf[..len])
  }
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

  let html5ever_timer = dom_parse_diagnostics_timer();
  let reader = io::Cursor::new(html.as_bytes());
  let mut reader = DeadlineCheckedRead::new(reader);
  let dom = parse_document(RcDom::default(), opts)
    .from_utf8()
    .read_from(&mut reader)
    .map_err(|e| {
      if e.kind() == io::ErrorKind::TimedOut {
        if let Some(timeout) = e
          .get_ref()
          .and_then(|inner| inner.downcast_ref::<crate::error::RenderError>())
        {
          return Error::Render(timeout.clone());
        }
        return Error::Render(crate::error::RenderError::Timeout {
          stage: RenderStage::DomParse,
          elapsed: crate::render_control::active_deadline()
            .as_ref()
            .map(|deadline| deadline.elapsed())
            .unwrap_or_default(),
        });
      }

      Error::Parse(ParseError::InvalidHtml {
        message: format!("Failed to parse HTML: {}", e),
        line: 0,
      })
    })?;
  if let Some(start) = html5ever_timer {
    let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;
    with_dom_parse_diagnostics(|diag| {
      diag.html5ever_ms += elapsed_ms;
    });
  }

  let quirks_mode = map_quirks_mode(dom.quirks_mode.get());

  let convert_timer = dom_parse_diagnostics_timer();
  let mut deadline_counter = 0usize;
  let mut root = convert_handle_to_node(&dom.document, quirks_mode, &mut deadline_counter)?
    .expect("DOM must have a document root node");
  if let Some(start) = convert_timer {
    let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;
    with_dom_parse_diagnostics(|diag| {
      diag.convert_ms += elapsed_ms;
    });
  }

  let shadow_attach_timer = dom_parse_diagnostics_timer();
  attach_shadow_roots(&mut root, &mut deadline_counter)?;
  if let Some(start) = shadow_attach_timer {
    let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;
    with_dom_parse_diagnostics(|diag| {
      diag.shadow_attach_ms += elapsed_ms;
    });
  }

  if matches!(
    options.compatibility_mode,
    DomCompatibilityMode::Compatibility
  ) {
    let compat_timer = dom_parse_diagnostics_timer();
    apply_dom_compatibility_mutations(&mut root, &mut deadline_counter)?;
    if let Some(start) = compat_timer {
      let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;
      with_dom_parse_diagnostics(|diag| {
        diag.compat_ms += elapsed_ms;
      });
    }
  }

  Ok(root)
}

/// Clone a DOM tree while periodically checking any active render deadline.
///
/// Unlike `DomNode::clone`, this uses an explicit stack so deeply nested documents don't risk a
/// stack overflow and so `RenderOptions::timeout` can abort pathological `dom_parse` workloads
/// cooperatively.
pub(crate) fn clone_dom_with_deadline(node: &DomNode, stage: RenderStage) -> Result<DomNode> {
  struct Frame {
    src: *const DomNode,
    dst: *mut DomNode,
    next_child: usize,
  }

  let mut deadline_counter = 0usize;
  check_active_periodic(&mut deadline_counter, DOM_PARSE_NODE_DEADLINE_STRIDE, stage)?;

  let mut root = DomNode {
    node_type: node.node_type.clone(),
    children: Vec::with_capacity(node.children.len()),
  };

  let mut stack = vec![Frame {
    src: node as *const _,
    dst: &mut root as *mut _,
    next_child: 0,
  }];

  while let Some(mut frame) = stack.pop() {
    let src = unsafe { &*frame.src };
    // Safety: destination nodes are owned by `root` and its descendants, and we never mutate a
    // node's children while a frame borrowing that node is active. This keeps raw pointers stable
    // for the duration of the DFS clone.
    let dst = unsafe { &mut *frame.dst };

    if frame.next_child < src.children.len() {
      let child_src = &src.children[frame.next_child];
      frame.next_child += 1;

      check_active_periodic(&mut deadline_counter, DOM_PARSE_NODE_DEADLINE_STRIDE, stage)?;

      dst.children.push(DomNode {
        node_type: child_src.node_type.clone(),
        children: Vec::with_capacity(child_src.children.len()),
      });
      let child_dst = dst.children.last_mut().expect("child was just pushed") as *mut DomNode;

      stack.push(frame);
      stack.push(Frame {
        src: child_src as *const _,
        dst: child_dst,
        next_child: 0,
      });
    }
  }

  Ok(root)
}

/// Clone a DOM tree and also report whether top-layer state (dialog/popover open state + inert
/// propagation) is needed.
///
/// This is used to avoid an additional full-tree `needs_top_layer_state` scan in call sites that
/// already pay the cost of cloning the DOM.
pub(crate) fn clone_dom_with_deadline_and_top_layer_hint(
  node: &DomNode,
  stage: RenderStage,
) -> Result<(DomNode, bool)> {
  struct Frame {
    src: *const DomNode,
    dst: *mut DomNode,
    next_child: usize,
    scan_children_suppressed: bool,
  }

  fn scan_top_layer_hint(node: &DomNode, hint: &mut bool, suppressed: bool) -> bool {
    if suppressed {
      return true;
    }
    if *hint {
      return false;
    }

    match &node.node_type {
      DomNodeType::ShadowRoot { .. } => {
        // `needs_top_layer_state` treats shadow DOM presence as a reason to run the full traversal
        // because dialogs/popovers may appear inside attached shadow roots.
        *hint = true;
        false
      }
      DomNodeType::Element {
        tag_name,
        namespace,
        attributes,
      } => {
        if (namespace.is_empty() || namespace == HTML_NAMESPACE) && tag_name.eq_ignore_ascii_case("dialog")
        {
          *hint = true;
        }

        // Template contents are inert; avoid descending into them to match `needs_top_layer_state`.
        if tag_name.eq_ignore_ascii_case("template") {
          return true;
        }

        for (name, _) in attributes {
          if name.eq_ignore_ascii_case("popover")
            || name.eq_ignore_ascii_case("data-fastr-open")
            || name.eq_ignore_ascii_case("data-fastr-modal")
          {
            *hint = true;
            break;
          }
        }
        false
      }
      DomNodeType::Slot { attributes, .. } => {
        for (name, _) in attributes {
          if name.eq_ignore_ascii_case("popover")
            || name.eq_ignore_ascii_case("data-fastr-open")
            || name.eq_ignore_ascii_case("data-fastr-modal")
          {
            *hint = true;
            break;
          }
        }
        false
      }
      DomNodeType::Document { .. } | DomNodeType::Text { .. } => false,
    }
  }

  let mut deadline_counter = 0usize;
  check_active_periodic(&mut deadline_counter, DOM_PARSE_NODE_DEADLINE_STRIDE, stage)?;

  let mut top_layer_hint = false;
  let root_children_suppressed = scan_top_layer_hint(node, &mut top_layer_hint, false);

  let mut root = DomNode {
    node_type: node.node_type.clone(),
    children: Vec::with_capacity(node.children.len()),
  };

  let mut stack = vec![Frame {
    src: node as *const _,
    dst: &mut root as *mut _,
    next_child: 0,
    scan_children_suppressed: root_children_suppressed,
  }];

  while let Some(mut frame) = stack.pop() {
    let src = unsafe { &*frame.src };
    // Safety: destination nodes are owned by `root` and its descendants, and we never mutate a
    // node's children while a frame borrowing that node is active. This keeps raw pointers stable
    // for the duration of the DFS clone.
    let dst = unsafe { &mut *frame.dst };

    if frame.next_child < src.children.len() {
      let child_src = &src.children[frame.next_child];
      frame.next_child += 1;

      check_active_periodic(&mut deadline_counter, DOM_PARSE_NODE_DEADLINE_STRIDE, stage)?;

      dst.children.push(DomNode {
        node_type: child_src.node_type.clone(),
        children: Vec::with_capacity(child_src.children.len()),
      });
      let child_dst = dst.children.last_mut().expect("child was just pushed") as *mut DomNode;

      let child_scan_suppressed = frame.scan_children_suppressed;
      let child_children_suppressed = if top_layer_hint {
        false
      } else {
        scan_top_layer_hint(child_src, &mut top_layer_hint, child_scan_suppressed)
      };

      stack.push(frame);
      stack.push(Frame {
        src: child_src as *const _,
        dst: child_dst,
        next_child: 0,
        scan_children_suppressed: child_children_suppressed,
      });
    }
  }

  Ok((root, top_layer_hint))
}

fn parse_shadow_root_definition(template: &DomNode) -> Option<(ShadowRootMode, bool)> {
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
  let mode = match mode_attr.to_ascii_lowercase().as_str() {
    "open" => ShadowRootMode::Open,
    "closed" => ShadowRootMode::Closed,
    _ => return None,
  };

  let delegates_focus = template
    .get_attribute_ref("shadowrootdelegatesfocus")
    .is_some();

  Some((mode, delegates_focus))
}

fn attach_shadow_roots(node: &mut DomNode, deadline_counter: &mut usize) -> Result<()> {
  check_active_periodic(
    deadline_counter,
    DOM_PARSE_NODE_DEADLINE_STRIDE,
    RenderStage::DomParse,
  )?;

  for child in &mut node.children {
    let is_inert_template = matches!(
      child.tag_name(),
      Some(tag) if tag.eq_ignore_ascii_case("template")
    ) && matches!(
      child.namespace(),
      Some(ns) if ns.is_empty() || ns == HTML_NAMESPACE
    ) && parse_shadow_root_definition(child).is_none();
    if is_inert_template {
      continue;
    }
    attach_shadow_roots(child, deadline_counter)?;
  }

  if !node.is_element() {
    return Ok(());
  }

  let mut shadow_template = None;
  for (idx, child) in node.children.iter().enumerate() {
    if let Some((mode, delegates_focus)) = parse_shadow_root_definition(child) {
      shadow_template = Some((idx, mode, delegates_focus));
      break;
    }
  }

  let Some((template_idx, mode, delegates_focus)) = shadow_template else {
    return Ok(());
  };

  // Only the first declarative shadow template is promoted to a shadow root, matching browsers.
  // Subsequent templates remain as inert light DOM children.
  let template = node.children.remove(template_idx);
  let shadow_root = DomNode {
    node_type: DomNodeType::ShadowRoot {
      mode,
      delegates_focus,
    },
    children: template.children,
  };
  let light_children = std::mem::take(&mut node.children);
  node.children = {
    let mut combined = Vec::with_capacity(light_children.len() + 1);
    combined.push(shadow_root);
    combined.extend(light_children);
    combined
  };

  Ok(())
}

fn collect_slot_names<'a>(node: &'a DomNode, out: &mut HashSet<&'a str>) {
  if matches!(node.node_type, DomNodeType::Slot { .. }) {
    out.insert(node.get_attribute_ref("name").unwrap_or(""));
  }

  for child in node.children.iter() {
    collect_slot_names(child, out);
  }
}

fn take_assignments_for_slot_ptr(
  assignments: &mut Vec<(Option<&str>, *const DomNode)>,
  slot_name: &str,
  available_slots: &HashSet<&str>,
) -> Vec<*const DomNode> {
  let mut taken = Vec::new();
  assignments.retain(|(name, node)| {
    let target = name.unwrap_or("");
    let matches = if slot_name.is_empty() {
      name.is_none() || !available_slots.contains(target)
    } else {
      target == slot_name
    };

    if matches {
      taken.push(*node);
      false
    } else {
      true
    }
  });
  taken
}

fn fill_slot_assignments(
  node: &DomNode,
  shadow_root_id: usize,
  assignments: &mut Vec<(Option<&str>, *const DomNode)>,
  available_slots: &HashSet<&str>,
  ids: &HashMap<*const DomNode, usize>,
  out: &mut SlotAssignment,
) {
  if matches!(node.node_type, DomNodeType::Slot { .. }) {
    let slot_name = node.get_attribute_ref("name").unwrap_or("");
    let assigned = take_assignments_for_slot_ptr(assignments, slot_name, available_slots);
    if !assigned.is_empty() {
      let slot_id = ids.get(&(node as *const DomNode)).copied().unwrap_or(0);
      let assigned_ids: Vec<usize> = assigned
        .iter()
        .filter_map(|ptr| ids.get(ptr).copied())
        .collect();
      out
        .shadow_to_slots
        .entry(shadow_root_id)
        .or_default()
        .entry(slot_name.to_string())
        .or_default()
        .extend(assigned_ids.iter().copied());
      for &node_id in &assigned_ids {
        out.node_to_slot.insert(
          node_id,
          AssignedSlot {
            slot_name: slot_name.to_string(),
            slot_node_id: slot_id,
            shadow_root_id,
          },
        );
      }
      out.slot_to_nodes.insert(slot_id, assigned_ids);
      // Once a slot is assigned, its fallback subtree is not rendered; stop recursion.
      return;
    }
  }

  for child in node.children.iter() {
    fill_slot_assignments(
      child,
      shadow_root_id,
      assignments,
      available_slots,
      ids,
      out,
    );
  }
}

fn enumerate_node_ids(node: &DomNode, next: &mut usize, map: &mut HashMap<*const DomNode, usize>) {
  map.insert(node as *const DomNode, *next);
  *next += 1;
  for child in node.children.iter() {
    enumerate_node_ids(child, next, map);
  }
}

/// Assign stable pre-order traversal ids to each node in the DOM tree.
pub fn enumerate_dom_ids(root: &DomNode) -> HashMap<*const DomNode, usize> {
  let mut ids: HashMap<*const DomNode, usize> = HashMap::new();
  let mut next_id = 1usize;
  enumerate_node_ids(root, &mut next_id, &mut ids);
  ids
}

/// Compute the slot assignment map for all shadow roots in the DOM.
pub fn compute_slot_assignment(root: &DomNode) -> SlotAssignment {
  let ids = enumerate_dom_ids(root);
  compute_slot_assignment_with_ids(root, &ids)
}

/// Compute the slot assignment map for all shadow roots in the DOM using a precomputed id map.
pub fn compute_slot_assignment_with_ids(
  root: &DomNode,
  ids: &HashMap<*const DomNode, usize>,
) -> SlotAssignment {
  let mut assignment = SlotAssignment::default();

  fn walk<'a>(
    node: &'a DomNode,
    parent: Option<&'a DomNode>,
    ids: &HashMap<*const DomNode, usize>,
    out: &mut SlotAssignment,
  ) {
    if matches!(node.node_type, DomNodeType::ShadowRoot { .. }) {
      if let Some(host) = parent {
        let mut available_slots: HashSet<&str> = HashSet::new();
        collect_slot_names(node, &mut available_slots);
        let mut light_children: Vec<(Option<&str>, *const DomNode)> = host
          .children
          .iter()
          .filter(|c| !matches!(c.node_type, DomNodeType::ShadowRoot { .. }))
          .map(|child| {
            let slot_name = child
              .get_attribute_ref("slot")
              .map(|v| v.trim())
              .filter(|v| !v.is_empty());
            (slot_name, child as *const DomNode)
          })
          .collect();

        let shadow_root_id = ids.get(&(node as *const DomNode)).copied().unwrap_or(0);
        fill_slot_assignments(
          node,
          shadow_root_id,
          &mut light_children,
          &available_slots,
          ids,
          out,
        );
      }
    }

    for child in node.children.iter() {
      walk(child, Some(node), ids, out);
    }
  }

  walk(root, None, ids, &mut assignment);
  assignment
}

fn push_part_export(exports: &mut HashMap<String, Vec<usize>>, name: &str, node_id: usize) {
  let entry = exports.entry(name.to_string()).or_default();
  if !entry.contains(&node_id) {
    entry.push(node_id);
  }
}

/// Compute the mapping of exported parts for each shadow host in the DOM.
///
/// Closed shadow roots are still traversed here because declarative snapshots include their
/// contents, and `exportparts` is an explicit opt-in for styling from the outside.
pub fn compute_part_export_map(root: &DomNode) -> PartExportMap {
  let ids = enumerate_dom_ids(root);
  compute_part_export_map_with_ids(root, &ids)
}

/// Compute the mapping of exported parts for each shadow host using a precomputed id map.
pub fn compute_part_export_map_with_ids(
  root: &DomNode,
  ids: &HashMap<*const DomNode, usize>,
) -> PartExportMap {
  fn collect_for_host(
    host: &DomNode,
    ids: &HashMap<*const DomNode, usize>,
    map: &mut PartExportMap,
  ) -> HashMap<String, Vec<usize>> {
    let host_id = ids.get(&(host as *const DomNode)).copied().unwrap_or(0);
    if let Some(existing) = map.exports_for_host(host_id) {
      return existing.clone();
    }

    let shadow_root = host
      .children
      .iter()
      .find(|child| matches!(child.node_type, DomNodeType::ShadowRoot { .. }));
    let Some(shadow_root) = shadow_root else {
      return HashMap::new();
    };

    fn walk(
      node: &DomNode,
      ids: &HashMap<*const DomNode, usize>,
      map: &mut PartExportMap,
      exports: &mut HashMap<String, Vec<usize>>,
    ) {
      if let Some(parts) = node.get_attribute_ref("part") {
        let node_id = ids.get(&(node as *const DomNode)).copied().unwrap_or(0);
        for part in parts.split_ascii_whitespace() {
          push_part_export(exports, part, node_id);
        }
      }

      if node.is_shadow_host() {
        let nested_exports = collect_for_host(node, ids, map);
        if let Some(mapping) = node.get_attribute_ref("exportparts") {
          for (inner, alias) in parse_exportparts(mapping) {
            if let Some(targets) = nested_exports.get(&inner) {
              for target in targets {
                push_part_export(exports, &alias, *target);
              }
            }
          }
        }
      }

      for child in node.children.iter() {
        if matches!(child.node_type, DomNodeType::ShadowRoot { .. }) {
          // Nested shadow contents are only exposed via exportparts on the host.
          continue;
        }
        walk(child, ids, map, exports);
      }
    }

    let mut exports: HashMap<String, Vec<usize>> = HashMap::new();
    walk(shadow_root, ids, map, &mut exports);

    if let Some(exportparts) = host.get_attribute_ref("exportparts") {
      for (internal, alias) in parse_exportparts(exportparts) {
        if let Some(targets) = exports.get(&internal).cloned() {
          for target in targets {
            push_part_export(&mut exports, &alias, target);
          }
        }
      }
    }

    map.insert_host_exports(host_id, exports.clone());
    exports
  }

  fn traverse(node: &DomNode, ids: &HashMap<*const DomNode, usize>, map: &mut PartExportMap) {
    if node.is_shadow_host() {
      collect_for_host(node, ids, map);
    }
    for child in node.children.iter() {
      traverse(child, ids, map);
    }
  }

  let mut map = PartExportMap::default();
  traverse(root, ids, &mut map);
  map
}

/// Optional DOM compatibility tweaks applied after HTML parsing.
///
/// Some documents bootstrap by marking the root with `no-js` and replacing it with a
/// `js-enabled` class once scripts execute. Others toggle visibility gates like
/// `jsl10n-visible` after client-side localization. Since we do not run author scripts,
/// mirror those initializations so content that relies on the class flip (e.g., initial
/// opacity) is visible in static renders.
fn apply_dom_compatibility_mutations(
  node: &mut DomNode,
  deadline_counter: &mut usize,
) -> Result<()> {
  check_active_periodic(
    deadline_counter,
    DOM_PARSE_NODE_DEADLINE_STRIDE,
    RenderStage::DomParse,
  )?;

  if let DomNodeType::Element {
    tag_name,
    attributes,
    ..
  } = &mut node.node_type
  {
    let mut classes: Vec<String> = attributes
      .iter()
      .find(|(k, _)| k.eq_ignore_ascii_case("class"))
      .map(|(_, v)| v.split_ascii_whitespace().map(|s| s.to_string()).collect())
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
    apply_dom_compatibility_mutations(child, deadline_counter)?;
  }

  Ok(())
}

fn convert_handle_to_node(
  handle: &Handle,
  document_quirks_mode: QuirksMode,
  deadline_counter: &mut usize,
) -> Result<Option<DomNode>> {
  check_active_periodic(
    deadline_counter,
    DOM_PARSE_NODE_DEADLINE_STRIDE,
    RenderStage::DomParse,
  )?;

  let node_type = match &handle.data {
    NodeData::Document => DomNodeType::Document {
      quirks_mode: document_quirks_mode,
    },
    NodeData::Element { name, attrs, .. } => {
      let namespace = if name.ns.as_ref() == HTML_NAMESPACE {
        String::new()
      } else {
        name.ns.to_string()
      };
      let attrs_ref = attrs.borrow();
      let mut attributes = Vec::with_capacity(attrs_ref.len());
      for attr in attrs_ref.iter() {
        attributes.push((attr.name.local.to_string(), attr.value.to_string()));
      }

      let is_html_slot = name.local.as_ref().eq_ignore_ascii_case("slot")
        && (namespace.is_empty() || namespace == HTML_NAMESPACE);

      if is_html_slot {
        DomNodeType::Slot {
          namespace,
          attributes,
          assigned: false,
        }
      } else {
        let tag_name = name.local.to_string();
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
    _ => return Ok(None),
  };

  let mut children: Vec<DomNode> = match &handle.data {
    NodeData::Element {
      name,
      template_contents,
      ..
    } => {
      if name.local.as_ref().eq_ignore_ascii_case("template") {
        let borrowed = template_contents.borrow();
        match &*borrowed {
          Some(content) => {
            let children_ref = content.children.borrow();
            let mut out = Vec::with_capacity(children_ref.len());
            for child in children_ref.iter() {
              if let Some(node) =
                convert_handle_to_node(child, document_quirks_mode, deadline_counter)?
              {
                out.push(node);
              }
            }
            out
          }
          None => Vec::new(),
        }
      } else {
        let children_ref = handle.children.borrow();
        let mut out = Vec::with_capacity(children_ref.len());
        for child in children_ref.iter() {
          if let Some(node) = convert_handle_to_node(child, document_quirks_mode, deadline_counter)? {
            out.push(node);
          }
        }
        out
      }
    }
    NodeData::Document => {
      let children_ref = handle.children.borrow();
      let mut out = Vec::with_capacity(children_ref.len());
      for child in children_ref.iter() {
        if let Some(node) = convert_handle_to_node(child, document_quirks_mode, deadline_counter)? {
          out.push(node);
        }
      }
      out
    }
    _ => Vec::new(),
  };

  // HTML <wbr> elements represent optional break opportunities. Synthesize a zero-width break
  // text node so line breaking can consider the opportunity while still allowing the element to
  // be styled/selected.
  if let DomNodeType::Element { tag_name, .. } = &node_type {
    if tag_name.eq_ignore_ascii_case("wbr") {
      children.reserve(1);
      children.push(DomNode {
        node_type: DomNodeType::Text {
          content: "\u{200B}".to_string(),
        },
        children: Vec::new(),
      });
    }
  }

  Ok(Some(DomNode {
    node_type,
    children,
  }))
}

impl DomNode {
  pub(crate) fn clone_without_children(&self) -> DomNode {
    DomNode {
      node_type: self.node_type.clone(),
      children: Vec::new(),
    }
  }

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

  pub fn document_quirks_mode(&self) -> QuirksMode {
    if let DomNodeType::Document { quirks_mode } = &self.node_type {
      *quirks_mode
    } else {
      debug_assert!(
        matches!(self.node_type, DomNodeType::Document { .. }),
        "document_quirks_mode called on non-document node; defaulting to NoQuirks"
      );
      QuirksMode::NoQuirks
    }
  }

  pub fn is_shadow_host(&self) -> bool {
    matches!(
      self.node_type,
      DomNodeType::Element { .. } | DomNodeType::Slot { .. }
    ) && self
      .children
      .iter()
      .any(|c| matches!(c.node_type, DomNodeType::ShadowRoot { .. }))
  }

  pub fn attributes_iter(&self) -> impl Iterator<Item = (&str, &str)> + '_ {
    let attrs: &[(String, String)] = match &self.node_type {
      DomNodeType::Element { attributes, .. } => attributes,
      DomNodeType::Slot { attributes, .. } => attributes,
      _ => &[],
    };
    attrs.iter().map(|(k, v)| (k.as_str(), v.as_str()))
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
      class_attr.split_ascii_whitespace().any(|c| c == class)
    } else {
      false
    }
  }

  /// Check if this element has a specific ID
  pub fn has_id(&self, id: &str) -> bool {
    self.get_attribute_ref("id") == Some(id)
  }
}

/// Parse an `exportparts` attribute value into (internal, exported) name pairs.
///
/// Entries are comma-separated. A missing alias is treated as an identity mapping.
pub(crate) fn parse_exportparts(value: &str) -> Vec<(String, String)> {
  let mut mappings = Vec::new();
  for entry in value.split(',') {
    let entry = entry.trim();
    if entry.is_empty() {
      continue;
    }

    let mut parts = entry.splitn(2, ':');
    let internal = parts.next().map(str::trim).unwrap_or_default();
    let exported = parts.next().map(str::trim);
    if internal.is_empty() {
      continue;
    }

    match exported {
      Some(alias) if !alias.is_empty() => {
        mappings.push((internal.to_string(), alias.to_string()));
      }
      _ => {
        mappings.push((internal.to_string(), internal.to_string()));
      }
    }
  }
  mappings
}

fn parse_finite_number(value: &str) -> Option<f64> {
  value.trim().parse::<f64>().ok().filter(|v| v.is_finite())
}

fn format_number(mut value: f64) -> String {
  if value == -0.0 {
    value = 0.0;
  }
  let mut s = value.to_string();
  if s.contains('.') {
    while s.ends_with('0') {
      s.pop();
    }
    if s.ends_with('.') {
      s.pop();
    }
  }
  s
}

fn input_range_bounds(node: &DomNode) -> Option<(f64, f64)> {
  if !matches!(node.tag_name(), Some(tag) if tag.eq_ignore_ascii_case("input")) {
    return None;
  }

  let input_type = node.get_attribute_ref("type");
  if !matches!(input_type, Some(t) if t.eq_ignore_ascii_case("range")) {
    return None;
  }

  let min = node
    .get_attribute_ref("min")
    .and_then(parse_finite_number)
    .unwrap_or(0.0);
  let max = node
    .get_attribute_ref("max")
    .and_then(parse_finite_number)
    .unwrap_or(100.0);

  // The HTML value sanitization algorithm collapses invalid ranges. When max < min, treat max as
  // min so downstream clamping produces a usable value instead of marking the control invalid.
  let clamped_max = if max < min { min } else { max };
  Some((min, clamped_max))
}

fn input_range_value(node: &DomNode) -> Option<f64> {
  let (min, max) = input_range_bounds(node)?;

  let resolved = node
    .get_attribute_ref("value")
    .and_then(parse_finite_number)
    .unwrap_or_else(|| (min + max) / 2.0);

  let clamped = resolved.clamp(min, max);

  let step_attr = node.get_attribute_ref("step");
  if matches!(step_attr, Some(step) if step.trim().eq_ignore_ascii_case("any")) {
    return Some(clamped);
  }

  let step = step_attr
    .and_then(parse_finite_number)
    .filter(|step| *step > 0.0)
    .unwrap_or(1.0);

  // The allowed value step base for range inputs is the minimum value (defaulting to zero).
  let step_base = min;
  let steps_to_value = ((clamped - step_base) / step).round();
  let mut aligned = step_base + steps_to_value * step;

  let max_aligned = step_base + ((max - step_base) / step).floor() * step;
  if aligned > max_aligned {
    aligned = max_aligned;
  }
  if aligned < step_base {
    aligned = step_base;
  }

  Some(aligned.clamp(min, max))
}

/// Wrapper for DomNode that implements Element trait for selector matching
/// This wrapper carries context needed for matching (parent, siblings)
#[derive(Debug, Clone, Copy)]
pub struct ElementRef<'a> {
  pub node: &'a DomNode,
  pub node_id: usize,
  pub parent: Option<&'a DomNode>,
  all_ancestors: &'a [&'a DomNode],
  slot_map: Option<&'a crate::css::selectors::SlotAssignmentMap<'a>>,
  attr_cache: Option<&'a ElementAttrCache>,
}

impl<'a> ElementRef<'a> {
  pub fn new(node: &'a DomNode) -> Self {
    Self {
      node,
      node_id: 0,
      parent: None,
      all_ancestors: &[],
      slot_map: None,
      attr_cache: None,
    }
  }

  pub fn with_ancestors(node: &'a DomNode, ancestors: &'a [&'a DomNode]) -> Self {
    let parent = ancestors.last().copied();
    Self {
      node,
      node_id: 0,
      parent,
      all_ancestors: ancestors,
      slot_map: None,
      attr_cache: None,
    }
  }

  pub fn with_node_id(mut self, node_id: usize) -> Self {
    self.node_id = node_id;
    self
  }

  pub fn with_slot_map(
    mut self,
    slot_map: Option<&'a crate::css::selectors::SlotAssignmentMap<'a>>,
  ) -> Self {
    self.slot_map = slot_map;
    self
  }

  pub fn with_attr_cache(mut self, attr_cache: Option<&'a ElementAttrCache>) -> Self {
    self.attr_cache = attr_cache;
    self
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

  fn push_assigned_slot_nodes<'b>(
    current: &'b DomNode,
    slot_map: Option<&SlotAssignmentMap<'b>>,
    visited: Option<&HashSet<usize>>,
    stack: &mut Vec<&'b DomNode>,
  ) -> bool {
    let Some(map) = slot_map else {
      return false;
    };
    if !matches!(current.node_type, DomNodeType::Slot { .. }) {
      return false;
    }
    let Some(slot_id) = map.slot_id(current) else {
      return false;
    };
    let Some(assigned_ids) = map.assigned_node_ids(slot_id) else {
      return false;
    };

    let mut pushed = false;
    for assigned_id in assigned_ids.iter().rev() {
      let Some(assigned) = map.node_for_id(*assigned_id) else {
        continue;
      };
      if let Some(seen) = visited {
        if let Some(id) = map.node_id(assigned) {
          if seen.contains(&id) {
            continue;
          }
        }
      }
      stack.push(assigned);
      pushed = true;
    }
    pushed
  }

  fn subtree_contains_focus(&self, slot_map: Option<&SlotAssignmentMap<'_>>) -> bool {
    if self.inert_flag() {
      return false;
    }
    Self::node_or_descendant_has_focus(self.node, slot_map)
  }

  fn node_or_descendant_has_focus(
    node: &DomNode,
    slot_map: Option<&SlotAssignmentMap<'_>>,
  ) -> bool {
    let mut stack: Vec<&DomNode> = vec![node];
    let mut visited = slot_map.is_some().then(HashSet::new);

    while let Some(current) = stack.pop() {
      if Self::node_is_inert(current) {
        continue;
      }
      if let (Some(map), Some(ref mut seen)) = (slot_map, visited.as_mut()) {
        if let Some(id) = map.node_id(current) {
          if !seen.insert(id) {
            continue;
          }
        }
      }
      if Self::node_focus_flag(current) {
        return true;
      }

      let assigned_children_pushed =
        Self::push_assigned_slot_nodes(current, slot_map, visited.as_ref(), &mut stack);

      if !assigned_children_pushed {
        for child in current.children.iter().rev() {
          stack.push(child);
        }
      }
    }

    false
  }

  fn subtree_has_content(node: &DomNode) -> bool {
    match node.node_type {
      DomNodeType::Text { .. } => true,
      DomNodeType::Element { .. } | DomNodeType::Slot { .. } => true,
      DomNodeType::ShadowRoot { .. } | DomNodeType::Document { .. } => {
        node.children.iter().any(Self::subtree_has_content)
      }
    }
  }

  fn sibling_position(
    &self,
    context: &mut selectors::matching::MatchingContext<FastRenderSelectorImpl>,
  ) -> Option<SiblingPosition> {
    let parent = self.parent?;
    if let Some(cache) = context.extra_data.sibling_cache {
      return cache.position(parent, self.node, context);
    }

    build_parent_sibling_list(parent, context)
      .and_then(|entry| entry.positions.get(&(self.node as *const DomNode)).copied())
  }

  /// Find index of this element among sibling elements and the total number of element siblings.
  fn element_index_and_len(
    &self,
    context: &mut selectors::matching::MatchingContext<FastRenderSelectorImpl>,
  ) -> Option<(usize, usize)> {
    self
      .sibling_position(context)
      .map(|position| (position.index, position.len))
  }

  /// Find index of this element among siblings
  fn element_index(
    &self,
    context: &mut selectors::matching::MatchingContext<FastRenderSelectorImpl>,
  ) -> Option<usize> {
    self.element_index_and_len(context).map(|(idx, _)| idx)
  }

  fn is_html_element(&self) -> bool {
    matches!(
      self.node.node_type,
      DomNodeType::Element { ref namespace, .. } | DomNodeType::Slot { ref namespace, .. }
        if namespace.is_empty() || namespace == HTML_NAMESPACE
    )
  }

  fn is_shadow_host(&self) -> bool {
    matches!(
      self.node.node_type,
      DomNodeType::Element { .. } | DomNodeType::Slot { .. }
    ) && self
      .node
      .children
      .iter()
      .any(|child| matches!(child.node_type, DomNodeType::ShadowRoot { .. }))
  }

  /// Position (index, total) among siblings filtered by a predicate.
  fn position_in_siblings<F>(
    &self,
    predicate: F,
    context: &mut selectors::matching::MatchingContext<FastRenderSelectorImpl>,
  ) -> Option<(usize, usize)>
  where
    F: Fn(&DomNode) -> bool,
  {
    let parent = self.parent?;
    let element_children: Vec<*const DomNode> =
      if let Some(cache) = context.extra_data.sibling_cache {
        cache.ordered_children(parent, context)?
      } else {
        parent
          .children
          .iter()
          .filter(|c| c.is_element())
          .map(|c| c as *const DomNode)
          .collect()
      };

    let mut index = None;
    let mut len = 0usize;
    let mut deadline_counter = 0usize;
    for child_ptr in element_children {
      if let Err(err) = check_active_periodic(
        &mut deadline_counter,
        NTH_DEADLINE_STRIDE,
        RenderStage::Cascade,
      ) {
        context.extra_data.record_deadline_error(err);
        return None;
      }
      // Safety: DOM nodes are immutable during selector matching; pointers come from the DOM tree.
      let child = unsafe { &*child_ptr };
      if !predicate(child) {
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
  fn position_in_type(
    &self,
    context: &mut selectors::matching::MatchingContext<FastRenderSelectorImpl>,
  ) -> Option<(usize, usize)> {
    if self.node.tag_name().is_none() {
      return None;
    }
    self
      .sibling_position(context)
      .map(|position| (position.type_index, position.type_len))
  }

  fn populate_nth_index_cache_for_selectors(
    &self,
    selectors: &selectors::parser::SelectorList<FastRenderSelectorImpl>,
    is_from_end: bool,
    context: &mut selectors::matching::MatchingContext<FastRenderSelectorImpl>,
  ) -> Option<i32> {
    let parent = self.parent?;
    let (entries, self_index) = context.nest(|context| {
      let mut entries: Vec<(OpaqueElement, i32)> = Vec::with_capacity(parent.children.len());
      let mut self_index: Option<i32> = None;
      let mut matching_index = 0i32;
      let mut deadline_counter = 0usize;

      let mut process_child =
        |child: &DomNode,
         context: &mut selectors::matching::MatchingContext<FastRenderSelectorImpl>|
         -> Option<()> {
          if let Err(err) = check_active_periodic(
            &mut deadline_counter,
            NTH_DEADLINE_STRIDE,
            RenderStage::Cascade,
          ) {
            context.extra_data.record_deadline_error(err);
            return None;
          }
          if !child.is_element() {
            return Some(());
          }
          let child_ref = ElementRef::with_ancestors(child, self.all_ancestors)
            .with_slot_map(self.slot_map)
            .with_attr_cache(self.attr_cache);
          let matches = selectors
            .slice()
            .iter()
            .any(|selector| matches_selector(selector, 0, None, &child_ref, context));
          if context.extra_data.deadline_error.is_some() {
            return None;
          }

          let idx = if matches {
            matching_index += 1;
            matching_index
          } else {
            0
          };
          entries.push((OpaqueElement::new(child), idx));

          if ptr::eq(child, self.node) {
            self_index = Some(idx);
          }
          Some(())
        };

      if is_from_end {
        for child in parent.children.iter().rev() {
          process_child(child, context)?;
        }
      } else {
        for child in parent.children.iter() {
          process_child(child, context)?;
        }
      }

      Some((entries, self_index))
    })?;

    let cache = context.nth_index_cache(false, is_from_end, selectors.slice());
    for (el, idx) in entries {
      cache.insert(el, idx);
    }

    #[cfg(test)]
    NTH_OF_CACHE_POPULATIONS.with(|counter| {
      counter.fetch_add(1, Ordering::Relaxed);
    });

    Some(self_index.unwrap_or(0))
  }

  /// Return the language of this element, inherited from ancestors if absent.
  fn language(&self) -> Option<&'a str> {
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

  fn lang_attribute(&self, node: &'a DomNode) -> Option<&'a str> {
    node
      .get_attribute_ref("lang")
      .or_else(|| node.get_attribute_ref("xml:lang"))
  }

  fn supports_disabled(&self) -> bool {
    if !self.is_html_element() {
      return false;
    }
    self.node.tag_name().is_some_and(|tag| {
      tag.eq_ignore_ascii_case("button")
        || tag.eq_ignore_ascii_case("input")
        || tag.eq_ignore_ascii_case("select")
        || tag.eq_ignore_ascii_case("textarea")
        || tag.eq_ignore_ascii_case("option")
        || tag.eq_ignore_ascii_case("optgroup")
        || tag.eq_ignore_ascii_case("fieldset")
    })
  }

  fn is_disabled(&self) -> bool {
    let Some(tag) = self.node.tag_name() else {
      return false;
    };

    if self.supports_disabled() && self.node.get_attribute_ref("disabled").is_some() {
      return true;
    }

    // Fieldset disabled state propagates to descendants except those inside the first legend.
    for (i, ancestor) in self.all_ancestors.iter().enumerate().rev() {
      if let Some(a_tag) = ancestor.tag_name() {
        if a_tag.eq_ignore_ascii_case("fieldset") && ancestor.get_attribute_ref("disabled").is_some()
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

    if tag.eq_ignore_ascii_case("option") || tag.eq_ignore_ascii_case("optgroup") {
      for ancestor in self.all_ancestors.iter().rev() {
        if let Some(a_tag) = ancestor.tag_name() {
          if a_tag.eq_ignore_ascii_case("select")
            || a_tag.eq_ignore_ascii_case("optgroup")
            || a_tag.eq_ignore_ascii_case("fieldset")
          {
            if ancestor.get_attribute_ref("disabled").is_some() {
              return true;
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
      return value.is_empty() || value.eq_ignore_ascii_case("true");
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

    match self.node.get_attribute_ref("type") {
      None => true,
      Some(t) => {
        t.eq_ignore_ascii_case("text")
          || t.eq_ignore_ascii_case("search")
          || t.eq_ignore_ascii_case("url")
          || t.eq_ignore_ascii_case("tel")
          || t.eq_ignore_ascii_case("email")
          || t.eq_ignore_ascii_case("password")
          || t.eq_ignore_ascii_case("number")
          || t.eq_ignore_ascii_case("date")
          || t.eq_ignore_ascii_case("datetime-local")
          || t.eq_ignore_ascii_case("month")
          || t.eq_ignore_ascii_case("week")
          || t.eq_ignore_ascii_case("time")
      }
    }
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
      let input_type = self.node.get_attribute_ref("type");
      if matches!(input_type, Some(t) if t.eq_ignore_ascii_case("checkbox") || t.eq_ignore_ascii_case("radio")) {
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

    if tag.eq_ignore_ascii_case("select") || tag.eq_ignore_ascii_case("textarea") {
      return true;
    }

    if tag.eq_ignore_ascii_case("input") {
      let t = self.node.get_attribute_ref("type").unwrap_or("text");
      return !t.eq_ignore_ascii_case("hidden")
        && !t.eq_ignore_ascii_case("button")
        && !t.eq_ignore_ascii_case("reset")
        && !t.eq_ignore_ascii_case("submit")
        && !t.eq_ignore_ascii_case("image");
    }

    false
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
    if tag.eq_ignore_ascii_case("textarea") || tag.eq_ignore_ascii_case("select") {
      return true;
    }

    if tag.eq_ignore_ascii_case("input") {
      let t = self.node.get_attribute_ref("type").unwrap_or("text");
      return !t.eq_ignore_ascii_case("button")
        && !t.eq_ignore_ascii_case("reset")
        && !t.eq_ignore_ascii_case("submit")
        && !t.eq_ignore_ascii_case("image")
        && !t.eq_ignore_ascii_case("hidden");
    }

    false
  }

  fn control_value(&self) -> Option<String> {
    let tag = self.node.tag_name()?;
    if tag.eq_ignore_ascii_case("textarea") {
      return Some(textarea_value(self.node));
    }
    if tag.eq_ignore_ascii_case("select") {
      return self.select_value();
    }
    if tag.eq_ignore_ascii_case("input") {
      if let Some(value) = input_range_value(self.node) {
        return Some(format_number(value));
      }
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
    if multiple {
      let mut values = Vec::new();
      collect_selected_option_values(self.node, false, &mut values);
      if values.is_empty() {
        return None;
      }
      return Some(values.join(", "));
    }

    let explicit = find_selected_option_value(self.node, false);
    if explicit.is_some() {
      return explicit;
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

    let value = self.control_value().unwrap_or_default();

    if self.is_required() && value.trim().is_empty() {
      return false;
    }

    if tag.eq_ignore_ascii_case("select") {
      return true;
    }

    if tag.eq_ignore_ascii_case("textarea") {
      return true;
    }

    if tag.eq_ignore_ascii_case("input") {
      let input_type = self.node.get_attribute_ref("type").unwrap_or("text");

      if input_type.eq_ignore_ascii_case("text")
        || input_type.eq_ignore_ascii_case("search")
        || input_type.eq_ignore_ascii_case("url")
        || input_type.eq_ignore_ascii_case("tel")
        || input_type.eq_ignore_ascii_case("email")
        || input_type.eq_ignore_ascii_case("password")
      {
        return true;
      }

      if input_type.eq_ignore_ascii_case("number") || input_type.eq_ignore_ascii_case("range") {
        if value.trim().is_empty() {
          return !self.is_required();
        }
        if let Some(num) = Self::parse_number(&value) {
          return self.numeric_in_range(num).unwrap_or(true);
        }
        return false;
      }

      if input_type.eq_ignore_ascii_case("checkbox") || input_type.eq_ignore_ascii_case("radio") {
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
    let input_type = self.node.get_attribute_ref("type").unwrap_or("text");
    if !input_type.eq_ignore_ascii_case("number") && !input_type.eq_ignore_ascii_case("range") {
      return None;
    }

    if input_type.eq_ignore_ascii_case("range") {
      let (min, max) = input_range_bounds(self.node)?;
      let value = input_range_value(self.node)?;
      return Some(value >= min && value <= max);
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
      let input_type = self.node.get_attribute_ref("type").unwrap_or("text");

      if input_type.eq_ignore_ascii_case("checkbox") {
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

    let is_submit_input = tag.eq_ignore_ascii_case("input")
      && node
        .get_attribute_ref("type")
        .map(|t| t.eq_ignore_ascii_case("submit") || t.eq_ignore_ascii_case("image"))
        .unwrap_or(false);

    let is_button_submit = tag.eq_ignore_ascii_case("button")
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
      for child in node.children.iter() {
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
      .and_then(|d| {
        if d.eq_ignore_ascii_case("ltr") {
          return Some(TextDirection::Ltr);
        }
        if d.eq_ignore_ascii_case("rtl") {
          return Some(TextDirection::Rtl);
        }
        if d.eq_ignore_ascii_case("auto") {
          return resolve_first_strong_direction(resolve_root);
        }
        None
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

      let input_type = self.node.get_attribute_ref("type");

      if !supports_placeholder(input_type) {
        return false;
      }

      return self.node.get_attribute_ref("value").unwrap_or("").is_empty();
    }

    if tag.eq_ignore_ascii_case("textarea") {
      if self.node.get_attribute_ref("placeholder").is_none() {
        return false;
      }

      let value = textarea_value(self.node);
      return value.is_empty();
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

  /// Expose whether the control is read-only for accessibility mapping.
  pub(crate) fn accessibility_readonly(&self) -> bool {
    if let Some(tag) = self.node.tag_name() {
      if tag.eq_ignore_ascii_case("textarea") {
        return self.node.get_attribute_ref("readonly").is_some();
      }
      if tag.eq_ignore_ascii_case("input") && self.is_text_editable_input() {
        return self.node.get_attribute_ref("readonly").is_some();
      }
    }

    false
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

  fn subtree_contains_target(&self, slot_map: Option<&SlotAssignmentMap<'_>>) -> bool {
    let Some(target) = current_target_fragment() else {
      return false;
    };
    Self::subtree_has_target(self.node, target.as_str(), slot_map)
  }

  fn subtree_has_target(
    node: &DomNode,
    target: &str,
    slot_map: Option<&SlotAssignmentMap<'_>>,
  ) -> bool {
    let mut stack: Vec<&DomNode> = vec![node];
    let mut visited = slot_map.is_some().then(HashSet::new);

    while let Some(current) = stack.pop() {
      if let (Some(map), Some(ref mut seen)) = (slot_map, visited.as_mut()) {
        if let Some(id) = map.node_id(current) {
          if !seen.insert(id) {
            continue;
          }
        }
      }

      if Self::node_matches_target(current, target) {
        return true;
      }

      let assigned_children_pushed =
        Self::push_assigned_slot_nodes(current, slot_map, visited.as_ref(), &mut stack);

      if !assigned_children_pushed {
        for child in current.children.iter().rev() {
          stack.push(child);
        }
      }
    }

    false
  }

  fn node_matches_target(node: &DomNode, target: &str) -> bool {
    if let Some(id) = node.get_attribute_ref("id") {
      if id == target {
        return true;
      }
    }

    if let Some(tag) = node.tag_name() {
      if tag.eq_ignore_ascii_case("a") || tag.eq_ignore_ascii_case("area") {
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

/// Compute the raw textarea value, normalizing newline conventions and removing the single leading
/// newline that HTML ignores when contents start with a line break (common with formatted markup).
fn textarea_value(node: &DomNode) -> String {
  let mut value = String::new();
  for child in node.children.iter() {
    if let DomNodeType::Text { content } = &child.node_type {
      value.push_str(content);
    }
  }

  if value.contains('\r') {
    value = value.replace("\r\n", "\n").replace('\r', "\n");
  }
  if value.starts_with('\n') {
    value.remove(0);
  }

  value
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
  // Prefix match with boundary (BCP47 language tags are ASCII-case-insensitive).
  let Some(prefix) = lang.as_bytes().get(..range.len()) else {
    return false;
  };
  prefix.eq_ignore_ascii_case(range.as_bytes()) && lang.as_bytes().get(range.len()) == Some(&b'-')
}

fn supports_placeholder(input_type: Option<&str>) -> bool {
  let Some(t) = input_type else {
    return true;
  };

  // Per HTML spec, placeholder is supported for text-like controls; unknown types default to text.
  if t.eq_ignore_ascii_case("text")
    || t.eq_ignore_ascii_case("search")
    || t.eq_ignore_ascii_case("url")
    || t.eq_ignore_ascii_case("tel")
    || t.eq_ignore_ascii_case("email")
    || t.eq_ignore_ascii_case("password")
    || t.eq_ignore_ascii_case("number")
  {
    return true;
  }

  if t.eq_ignore_ascii_case("hidden")
    || t.eq_ignore_ascii_case("submit")
    || t.eq_ignore_ascii_case("reset")
    || t.eq_ignore_ascii_case("button")
    || t.eq_ignore_ascii_case("image")
    || t.eq_ignore_ascii_case("file")
    || t.eq_ignore_ascii_case("checkbox")
    || t.eq_ignore_ascii_case("radio")
    || t.eq_ignore_ascii_case("range")
    || t.eq_ignore_ascii_case("color")
    || t.eq_ignore_ascii_case("date")
    || t.eq_ignore_ascii_case("datetime-local")
    || t.eq_ignore_ascii_case("month")
    || t.eq_ignore_ascii_case("week")
    || t.eq_ignore_ascii_case("time")
  {
    return false;
  }

  true
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

fn collect_selected_option_values(node: &DomNode, optgroup_disabled: bool, out: &mut Vec<String>) {
  let tag = node.tag_name().unwrap_or("");
  let is_option = tag.eq_ignore_ascii_case("option");
  let is_optgroup = tag.eq_ignore_ascii_case("optgroup");

  let option_disabled = node.get_attribute_ref("disabled").is_some();
  let next_optgroup_disabled = optgroup_disabled || (is_optgroup && option_disabled);

  if is_option
    && node.get_attribute_ref("selected").is_some()
    && !(option_disabled || optgroup_disabled)
  {
    out.push(option_value_from_node(node));
  }

  for child in node.children.iter() {
    collect_selected_option_values(child, next_optgroup_disabled, out);
  }
}

fn find_selected_option_value(node: &DomNode, optgroup_disabled: bool) -> Option<String> {
  let tag = node.tag_name().unwrap_or("");
  let is_option = tag.eq_ignore_ascii_case("option");
  let is_optgroup = tag.eq_ignore_ascii_case("optgroup");

  let option_disabled = node.get_attribute_ref("disabled").is_some();
  let next_optgroup_disabled = optgroup_disabled || (is_optgroup && option_disabled);

  if is_option
    && node.get_attribute_ref("selected").is_some()
    && !(option_disabled || optgroup_disabled)
  {
    return Some(option_value_from_node(node));
  }

  for child in node.children.iter() {
    if let Some(val) = find_selected_option_value(child, next_optgroup_disabled) {
      return Some(val);
    }
  }
  None
}

fn first_enabled_option<'a>(node: &'a DomNode, optgroup_disabled: bool) -> Option<&'a DomNode> {
  let tag = node.tag_name().unwrap_or("");
  let is_option = tag.eq_ignore_ascii_case("option");
  let is_optgroup = tag.eq_ignore_ascii_case("optgroup");

  let option_disabled = node.get_attribute_ref("disabled").is_some();
  let next_optgroup_disabled = optgroup_disabled || (is_optgroup && option_disabled);

  if is_option && !(option_disabled || optgroup_disabled) {
    return Some(node);
  }

  for child in node.children.iter() {
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
        .with_slot_map(self.slot_map)
        .with_attr_cache(self.attr_cache)
    } else {
      // Parent is the root
      ElementRef::new(parent)
        .with_slot_map(self.slot_map)
        .with_attr_cache(self.attr_cache)
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
        return Some(
          ElementRef::with_ancestors(host, &self.all_ancestors[..idx - 1])
            .with_slot_map(self.slot_map)
            .with_attr_cache(self.attr_cache),
        );
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
          node_id: 0,
          parent: self.parent,
          all_ancestors: self.all_ancestors,
          slot_map: self.slot_map,
          attr_cache: self.attr_cache,
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
          node_id: 0,
          parent: self.parent,
          all_ancestors: self.all_ancestors,
          slot_map: self.slot_map,
          attr_cache: self.attr_cache,
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

    let attr_value = if let Some(cache) = self.attr_cache {
      cache.attr_value(self.node, local_name.as_str())
    } else {
      let is_html = self.is_html_element();
      self
        .node
        .attributes_iter()
        .find(|(name, _)| element_attr_cache_name_matches(name, local_name.as_str(), is_html))
        .map(|(_, value)| value)
    };
    let attr_value = match attr_value {
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
      PseudoClass::Host(selectors) => {
        if !_context
          .extra_data
          .shadow_host
          .is_some_and(|host| host == self.opaque())
        {
          return false;
        }
        let Some(selectors) = selectors else {
          return true;
        };
        _context.with_featureless(false, |context| {
          selectors
            .slice()
            .iter()
            .any(|selector| matches_selector(selector, 0, None, self, context))
        })
      }
      PseudoClass::HostContext(selectors) => {
        if !_context
          .extra_data
          .shadow_host
          .is_some_and(|host| host == self.opaque())
        {
          return false;
        }
        _context.with_featureless(false, |context| {
          if selectors
            .slice()
            .iter()
            .any(|selector| matches_selector(selector, 0, None, self, context))
          {
            return true;
          }

          for (idx, ancestor) in self.all_ancestors.iter().enumerate() {
            if !ancestor.is_element() {
              continue;
            }
            let ancestor_ref = ElementRef::with_ancestors(*ancestor, &self.all_ancestors[..idx])
              .with_slot_map(self.slot_map)
              .with_attr_cache(self.attr_cache);
            if selectors
              .slice()
              .iter()
              .any(|selector| matches_selector(selector, 0, None, &ancestor_ref, context))
            {
              return true;
            }
          }
          false
        })
      }
      PseudoClass::Root => {
        matches!(self.node.namespace(), Some(ns) if ns.is_empty() || ns == HTML_NAMESPACE)
          && self
            .node
            .tag_name()
            .map(|t| t.eq_ignore_ascii_case("html"))
            .unwrap_or(false)
      }
      PseudoClass::FirstChild => self.element_index(_context) == Some(0),
      PseudoClass::LastChild => self
        .element_index_and_len(_context)
        .map(|(idx, len)| idx == len.saturating_sub(1))
        .unwrap_or(false),
      PseudoClass::OnlyChild => self
        .element_index_and_len(_context)
        .map(|(_, len)| len == 1)
        .unwrap_or(false),
      PseudoClass::NthChild(a, b, of) => match of {
        Some(selectors) => {
          let cached = {
            let cache = _context.nth_index_cache(false, false, selectors.slice());
            cache.lookup(self.opaque())
          };
          let index = match cached {
            Some(index) => index,
            None => match self.populate_nth_index_cache_for_selectors(selectors, false, _context) {
              Some(index) => index,
              None => return false,
            },
          };
          index > 0 && matches_an_plus_b(*a, *b, index)
        }
        None => self
          .element_index(_context)
          .map(|index| matches_an_plus_b(*a, *b, (index + 1) as i32))
          .unwrap_or(false),
      },
      PseudoClass::NthLastChild(a, b, of) => match of {
        Some(selectors) => {
          let cached = {
            let cache = _context.nth_index_cache(false, true, selectors.slice());
            cache.lookup(self.opaque())
          };
          let index = match cached {
            Some(index) => index,
            None => match self.populate_nth_index_cache_for_selectors(selectors, true, _context) {
              Some(index) => index,
              None => return false,
            },
          };
          index > 0 && matches_an_plus_b(*a, *b, index)
        }
        None => self
          .element_index_and_len(_context)
          .map(|(index, len)| {
            let n = (len - index) as i32;
            matches_an_plus_b(*a, *b, n)
          })
          .unwrap_or(false),
      },
      PseudoClass::FirstOfType => self
        .position_in_type(_context)
        .map(|(index, _)| index == 0)
        .unwrap_or(false),
      PseudoClass::LastOfType => self
        .position_in_type(_context)
        .map(|(index, len)| index == len.saturating_sub(1))
        .unwrap_or(false),
      PseudoClass::OnlyOfType => self
        .position_in_type(_context)
        .map(|(_, len)| len == 1)
        .unwrap_or(false),
      PseudoClass::NthOfType(a, b) => self
        .position_in_type(_context)
        .map(|(index, _)| matches_an_plus_b(*a, *b, (index + 1) as i32))
        .unwrap_or(false),
      PseudoClass::NthLastOfType(a, b) => self
        .position_in_type(_context)
        .map(|(index, len)| {
          let n = (len - index) as i32;
          matches_an_plus_b(*a, *b, n)
        })
        .unwrap_or(false),
      PseudoClass::Lang(langs) => {
        if let Some(lang) = self.language() {
          langs.iter().any(|range| lang_matches(range, lang))
        } else {
          false
        }
      }
      PseudoClass::Dir(dir) => self.direction() == *dir,
      PseudoClass::AnyLink => self.is_link(),
      PseudoClass::Target => self.is_target(),
      PseudoClass::TargetWithin => self.subtree_contains_target(_context.extra_data.slot_map),
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
          if tag.eq_ignore_ascii_case("option") {
            return self.is_checked();
          }
          if tag.eq_ignore_ascii_case("input") {
            let t = self.node.get_attribute_ref("type").unwrap_or("text");
            if t.eq_ignore_ascii_case("checkbox") || t.eq_ignore_ascii_case("radio") {
              return self.node.get_attribute_ref("checked").is_some();
            }
          }
          if tag.eq_ignore_ascii_case("input") || tag.eq_ignore_ascii_case("button") {
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
      PseudoClass::FocusWithin => self.subtree_contains_focus(_context.extra_data.slot_map),
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
      PseudoElement::Slotted(_) => false,
      PseudoElement::Part(_) => true,
    }
  }

  fn is_link(&self) -> bool {
    let Some(tag) = self.node.tag_name() else {
      return false;
    };
    let has_href = self.node.get_attribute_ref("href").is_some();
    has_href
      && (tag.eq_ignore_ascii_case("a")
        || tag.eq_ignore_ascii_case("area")
        || tag.eq_ignore_ascii_case("link"))
  }

  fn is_html_slot_element(&self) -> bool {
    self
      .node
      .tag_name()
      .is_some_and(|t| t.eq_ignore_ascii_case("slot"))
  }

  fn assigned_slot(&self) -> Option<Self> {
    let slot_map = self.slot_map?;
    let slot = slot_map.assigned_slot(self.node)?;
    let parent = slot.ancestors.last().copied();
    Some(ElementRef {
      node: slot.slot,
      node_id: 0,
      parent,
      all_ancestors: slot.ancestors,
      slot_map: Some(slot_map),
      attr_cache: self.attr_cache,
    })
  }

  fn has_id(&self, id: &CssString, case_sensitivity: CaseSensitivity) -> bool {
    if let Some(cache) = self.attr_cache {
      return cache.has_id(self.node, id.as_str(), case_sensitivity);
    }

    let is_html = self.is_html_element();
    let actual = self
      .node
      .attributes_iter()
      .find(|(name, _)| element_attr_cache_name_matches(name, "id", is_html))
      .map(|(_, value)| value);
    let Some(actual) = actual else {
      return false;
    };
    match case_sensitivity {
      CaseSensitivity::CaseSensitive => actual == id.as_str(),
      CaseSensitivity::AsciiCaseInsensitive => actual.eq_ignore_ascii_case(id.as_str()),
    }
  }

  fn has_class(&self, class: &CssString, case_sensitivity: CaseSensitivity) -> bool {
    if let Some(cache) = self.attr_cache {
      return cache.has_class(self.node, class.as_str(), case_sensitivity);
    }

    let is_html = self.is_html_element();
    let classes = self
      .node
      .attributes_iter()
      .find(|(name, _)| element_attr_cache_name_matches(name, "class", is_html))
      .map(|(_, value)| value);
    let Some(classes) = classes else {
      return false;
    };

    match case_sensitivity {
      CaseSensitivity::CaseSensitive => classes
        .split_ascii_whitespace()
        .any(|c| c == class.as_str()),
      CaseSensitivity::AsciiCaseInsensitive => classes
        .split_ascii_whitespace()
        .any(|c| c.eq_ignore_ascii_case(class.as_str())),
    }
  }

  fn imported_part(&self, name: &CssString) -> Option<CssString> {
    let Some(attr) = self.node.get_attribute_ref("exportparts") else {
      return None;
    };

    let target = name.as_str();
    for (internal, exported) in parse_exportparts(attr) {
      if exported == target {
        return Some(CssString::from(internal));
      }
    }

    None
  }

  fn is_part(&self, name: &CssString) -> bool {
    self
      .node
      .get_attribute_ref("part")
      .map(|value| value.split_ascii_whitespace().any(|token| token == name.as_str()))
      .unwrap_or(false)
  }

  fn is_empty(&self) -> bool {
    !self.node.children.iter().any(Self::subtree_has_content)
  }

  fn is_root(&self) -> bool {
    matches!(self.node.tag_name(), Some("html"))
  }

  fn first_element_child(&self) -> Option<Self> {
    for child in &self.node.children {
      if child.is_element() {
        return Some(ElementRef {
          node: child,
          node_id: 0,
          parent: Some(self.node),
          all_ancestors: self.all_ancestors,
          slot_map: self.slot_map,
          attr_cache: self.attr_cache,
        });
      }
    }
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
    if !selector_bloom_enabled() {
      return false;
    }

    add_selector_bloom_hashes(self.node, &mut |hash| filter.insert_hash(hash));
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

pub(crate) fn relative_selector_bloom_hashes(
  selector: &RelativeSelector<FastRenderSelectorImpl>,
  quirks_mode: selectors::context::QuirksMode,
) -> Vec<u32> {
  selector.bloom_hashes.hashes_for_mode(quirks_mode).to_vec()
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
    ctx.nest_for_scope(Some(anchor.opaque()), |ctx| {
      let mut ancestors = RelativeSelectorAncestorStack::new(anchor.all_ancestors);
      let mut deadline_counter = 0usize;
      let use_ancestor_bloom = selector_bloom_enabled();
      let mut ancestor_bloom_filter = BloomFilter::new();
      let anchor_summary = ctx
        .extra_data
        .selector_blooms
        .and_then(|store| store.summary_for_id(anchor.node_id));

      for selector in selectors.iter() {
        record_has_eval();
        if let Err(err) = check_active_periodic(
          &mut deadline_counter,
          RELATIVE_SELECTOR_DEADLINE_STRIDE,
          RenderStage::Cascade,
        ) {
          ctx.extra_data.record_deadline_error(err);
          return false;
        }
        if let Some(cached) = ctx
          .selector_caches
          .relative_selector
          .lookup(anchor.opaque(), selector)
        {
          record_has_cache_hit();
          if cached.matched() {
            return true;
          }
          continue;
        }

        let quirks_mode = ctx.quirks_mode();
        if selector.match_hint.is_descendant_direction()
          && !matches!(quirks_mode, selectors::context::QuirksMode::Quirks)
        {
          if let Some(summary) = anchor_summary {
            let hashes = selector.bloom_hashes.hashes_for_mode(quirks_mode);
            if !hashes.is_empty() && hashes.iter().any(|hash| !summary.contains_hash(*hash)) {
              record_has_prune();
              ctx.selector_caches.relative_selector.add(
                anchor.opaque(),
                selector,
                RelativeSelectorCachedMatch::NotMatched,
              );
              continue;
            }
          }
        }

        if selector_bloom_enabled()
          && ctx
            .selector_caches
            .relative_selector_filter_map
            .fast_reject(anchor, selector, ctx.quirks_mode())
        {
          if ctx.extra_data.deadline_error.is_some() {
            return false;
          }
          record_has_prune();
          record_has_filter_prune();
          ctx.selector_caches.relative_selector.add(
            anchor.opaque(),
            selector,
            RelativeSelectorCachedMatch::NotMatched,
          );
          continue;
        }

        record_has_relative_eval();
        let matched = match_relative_selector(
          selector,
          anchor.node,
          &mut ancestors,
          &mut ancestor_bloom_filter,
          use_ancestor_bloom,
          ctx,
          &mut deadline_counter,
        );
        debug_assert_eq!(ancestors.len(), ancestors.baseline_len());
        ancestors.reset();
        debug_assert!(!use_ancestor_bloom || ancestor_bloom_filter.is_zeroed());

        if ctx.extra_data.deadline_error.is_some() {
          return false;
        }

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
  bloom_filter: &mut BloomFilter,
  use_ancestor_bloom: bool,
  context: &mut MatchingContext<FastRenderSelectorImpl>,
  deadline_counter: &mut usize,
) -> bool {
  if !anchor.is_element() {
    return false;
  }

  if selector.match_hint.is_descendant_direction() {
    return match_relative_selector_descendants(
      selector,
      anchor,
      ancestors,
      bloom_filter,
      use_ancestor_bloom,
      context,
      deadline_counter,
    );
  }

  match_relative_selector_siblings(
    selector,
    anchor,
    ancestors,
    bloom_filter,
    use_ancestor_bloom,
    context,
    deadline_counter,
  )
}

fn in_shadow_tree(ancestors: &[&DomNode]) -> bool {
  ancestors
    .iter()
    .any(|node| matches!(node.node_type, DomNodeType::ShadowRoot { .. }))
}

fn for_each_assigned_slot_child<'a, F: FnMut(&'a DomNode)>(node: &'a DomNode, f: &mut F) {
  for child in node.children.iter() {
    match &child.node_type {
      DomNodeType::Slot { assigned: true, .. } => {
        for assigned_child in child.children.iter().filter(|c| c.is_element()) {
          f(assigned_child);
        }
      }
      DomNodeType::ShadowRoot { .. } => {}
      _ => for_each_assigned_slot_child(child, f),
    }
  }
}

fn for_each_selector_child<'a, F: FnMut(&'a DomNode)>(
  anchor: &'a DomNode,
  ancestors: &[&'a DomNode],
  mut f: F,
) {
  let within_shadow_tree = in_shadow_tree(ancestors);
  for child in &anchor.children {
    match &child.node_type {
      DomNodeType::ShadowRoot { .. } => {
        if within_shadow_tree {
          continue;
        }
        for_each_assigned_slot_child(child, &mut f);
      }
      _ => {
        if child.is_element() {
          f(child);
        }
      }
    }
  }
}

fn match_relative_selector_descendants<'a>(
  selector: &RelativeSelector<FastRenderSelectorImpl>,
  anchor: &'a DomNode,
  ancestors: &mut RelativeSelectorAncestorStack<'a>,
  bloom_filter: &mut BloomFilter,
  use_ancestor_bloom: bool,
  context: &mut MatchingContext<FastRenderSelectorImpl>,
  deadline_counter: &mut usize,
) -> bool {
  let ancestor_hashes = selector.ancestor_hashes_for_mode(context.quirks_mode());
  ancestors.with_pushed(anchor, |ancestors| {
    if use_ancestor_bloom {
      add_selector_bloom_hashes(anchor, &mut |hash| bloom_filter.insert_hash(hash));
    }
    let mut found = false;
    for child in anchor.children.iter().filter(|c| c.is_element()) {
      if let Err(err) = check_active_periodic(
        deadline_counter,
        RELATIVE_SELECTOR_DEADLINE_STRIDE,
        RenderStage::Cascade,
      ) {
        context.extra_data.record_deadline_error(err);
        found = false;
        break;
      }
      let child_ref = ElementRef::with_ancestors(child, ancestors.as_slice())
        .with_slot_map(context.extra_data.slot_map)
        .with_attr_cache(context.extra_data.element_attr_cache);
      let mut matched = if use_ancestor_bloom && !selector_may_match(ancestor_hashes, bloom_filter) {
        false
      } else {
        matches_selector(&selector.selector, 0, None, &child_ref, context)
      };
      if context.extra_data.deadline_error.is_some() {
        found = false;
        break;
      }
      if !matched && selector.match_hint.is_subtree() {
        matched = match_relative_selector_subtree(
          selector,
          child,
          ancestors,
          bloom_filter,
          use_ancestor_bloom,
          context,
          deadline_counter,
        );
      }
      if matched {
        found = true;
        break;
      }
    }
    if use_ancestor_bloom {
      add_selector_bloom_hashes(anchor, &mut |hash| bloom_filter.remove_hash(hash));
    }
    found
  })
}

fn match_relative_selector_siblings<'a>(
  selector: &RelativeSelector<FastRenderSelectorImpl>,
  anchor: &'a DomNode,
  ancestors: &mut RelativeSelectorAncestorStack<'a>,
  bloom_filter: &mut BloomFilter,
  use_ancestor_bloom: bool,
  context: &mut MatchingContext<FastRenderSelectorImpl>,
  deadline_counter: &mut usize,
) -> bool {
  let ancestor_hashes = selector.ancestor_hashes_for_mode(context.quirks_mode());
  let parent = match ancestors.parent() {
    Some(p) => p,
    None => return false,
  };

  let mut seen_anchor = false;
  for sibling in parent.children.iter().filter(|c| c.is_element()) {
    if let Err(err) = check_active_periodic(
      deadline_counter,
      RELATIVE_SELECTOR_DEADLINE_STRIDE,
      RenderStage::Cascade,
    ) {
      context.extra_data.record_deadline_error(err);
      return false;
    }
    if ptr::eq(sibling, anchor) {
      seen_anchor = true;
      continue;
    }
    if !seen_anchor {
      continue;
    }

    let sibling_ref = ElementRef::with_ancestors(sibling, ancestors.as_slice())
      .with_slot_map(context.extra_data.slot_map)
      .with_attr_cache(context.extra_data.element_attr_cache);
    let matched = if selector.match_hint.is_subtree() {
      match_relative_selector_subtree(
        selector,
        sibling,
        ancestors,
        bloom_filter,
        use_ancestor_bloom,
        context,
        deadline_counter,
      )
    } else {
      if use_ancestor_bloom && !selector_may_match(ancestor_hashes, bloom_filter) {
        false
      } else {
        matches_selector(&selector.selector, 0, None, &sibling_ref, context)
      }
    };
    if context.extra_data.deadline_error.is_some() {
      return false;
    }

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
  bloom_filter: &mut BloomFilter,
  use_ancestor_bloom: bool,
  context: &mut MatchingContext<FastRenderSelectorImpl>,
  deadline_counter: &mut usize,
) -> bool {
  debug_assert!(selector.match_hint.is_subtree());

  let ancestor_hashes = selector.ancestor_hashes_for_mode(context.quirks_mode());
  ancestors.with_pushed(node, |ancestors| {
    if use_ancestor_bloom {
      add_selector_bloom_hashes(node, &mut |hash| bloom_filter.insert_hash(hash));
    }
    let mut found = false;
    for child in node.children.iter().filter(|c| c.is_element()) {
      if let Err(err) = check_active_periodic(
        deadline_counter,
        RELATIVE_SELECTOR_DEADLINE_STRIDE,
        RenderStage::Cascade,
      ) {
        context.extra_data.record_deadline_error(err);
        found = false;
        break;
      }
      let child_ref = ElementRef::with_ancestors(child, ancestors.as_slice())
        .with_slot_map(context.extra_data.slot_map)
        .with_attr_cache(context.extra_data.element_attr_cache);
      if (!use_ancestor_bloom || selector_may_match(ancestor_hashes, bloom_filter))
        && matches_selector(&selector.selector, 0, None, &child_ref, context)
      {
        if context.extra_data.deadline_error.is_some() {
          found = false;
          break;
        }
        found = true;
        break;
      }
      if match_relative_selector_subtree(
        selector,
        child,
        ancestors,
        bloom_filter,
        use_ancestor_bloom,
        context,
        deadline_counter,
      ) {
        found = true;
        break;
      }
    }
    if use_ancestor_bloom {
      add_selector_bloom_hashes(node, &mut |hash| bloom_filter.remove_hash(hash));
    }
    found
  })
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::css::selectors::build_relative_selectors;
  use crate::css::selectors::PseudoClassParser;
  use crate::css::selectors::ShadowMatchData;
  use crate::render_control::{with_deadline, RenderDeadline};
  use cssparser::{Parser, ParserInput};
  use selectors::context::QuirksMode;
  use selectors::matching::MatchingContext;
  use selectors::matching::MatchingForInvalidation;
  use selectors::matching::MatchingMode;
  use selectors::matching::NeedsSelectorFlags;
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

  fn element_with_attrs(tag: &str, attrs: Vec<(&str, &str)>, children: Vec<DomNode>) -> DomNode {
    DomNode {
      node_type: DomNodeType::Element {
        tag_name: tag.to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: attrs
          .into_iter()
          .map(|(k, v)| (k.to_string(), v.to_string()))
          .collect(),
      },
      children,
    }
  }

  fn document(children: Vec<DomNode>) -> DomNode {
    DomNode {
      node_type: DomNodeType::Document {
        quirks_mode: QuirksMode::NoQuirks,
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

  #[test]
  fn selector_bloom_hash_matches_selector_token_hash() {
    use precomputed_hash::PrecomputedHash;

    let value = "data-Thing";
    let token_hash = CssString::from(value).precomputed_hash() & selectors::bloom::BLOOM_HASH_MASK;
    let dom_hash = selector_bloom_hash(value);
    assert_eq!(dom_hash, token_hash);
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
    for child in node.children.iter() {
      if let Some(found) = find_element_by_id(child, id) {
        return Some(found);
      }
    }
    None
  }

  fn contains_shadow_root(node: &DomNode) -> bool {
    if matches!(node.node_type, DomNodeType::ShadowRoot { .. }) {
      return true;
    }
    node.children.iter().any(contains_shadow_root)
  }

  #[test]
  fn dom_parse_timeout_is_cooperative() {
    let deadline = RenderDeadline::new(Some(std::time::Duration::from_millis(0)), None);
    let result = with_deadline(Some(&deadline), || parse_html("<div>hello</div>"));

    match result {
      Err(Error::Render(crate::error::RenderError::Timeout { stage, .. })) => {
        assert_eq!(stage, RenderStage::DomParse);
      }
      other => panic!("expected dom_parse timeout, got {other:?}"),
    }
  }

  #[test]
  fn dom_clone_timeout_is_cooperative() {
    let mut dom = DomNode {
      node_type: DomNodeType::Document {
        quirks_mode: QuirksMode::NoQuirks,
      },
      children: vec![],
    };
    let mut current = &mut dom;
    for _ in 0..(super::DOM_PARSE_NODE_DEADLINE_STRIDE * 2) {
      current.children.push(DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      });
      current = current.children.last_mut().expect("child pushed");
    }

    let deadline = RenderDeadline::new(Some(std::time::Duration::from_millis(0)), None);
    let result =
      with_deadline(Some(&deadline), || clone_dom_with_deadline(&dom, RenderStage::DomParse));

    match result {
      Err(Error::Render(crate::error::RenderError::Timeout { stage, .. })) => {
        assert_eq!(stage, RenderStage::DomParse);
      }
      other => panic!("expected dom_parse timeout during clone, got {other:?}"),
    }
  }

  #[test]
  fn document_quirks_mode_defaults_to_no_quirks_with_doctype() {
    let dom = parse_html("<!doctype html><html><body></body></html>").expect("parse html");
    assert_eq!(
      dom.document_quirks_mode(),
      QuirksMode::NoQuirks,
      "HTML5 doctype should produce no-quirks mode"
    );
  }

  #[test]
  fn document_quirks_mode_enters_quirks_without_doctype() {
    let dom = parse_html("<html><body></body></html>").expect("parse html");
    assert_eq!(
      dom.document_quirks_mode(),
      QuirksMode::Quirks,
      "missing doctype should trigger quirks mode"
    );
  }

  #[test]
  fn parse_html_uses_empty_namespace_for_html_elements() {
    let dom =
      parse_html("<!doctype html><html><body><div id='x'></div></body></html>").expect("parse");
    let div = find_element_by_id(&dom, "x").expect("div element");
    assert_eq!(
      div.namespace(),
      Some(""),
      "HTML elements should store an empty namespace to avoid per-node allocations"
    );

    let div_ref = ElementRef::new(div);
    assert!(
      div_ref.has_namespace(HTML_NAMESPACE),
      "namespace matching should treat the empty namespace as HTML"
    );
  }

  #[test]
  fn declarative_shadow_dom_only_attaches_first_template() {
    let html = "<div id='host'><template shadowroot='open'><p id='first'>first</p></template><template shadowroot='closed'><p id='second'>second</p></template><p id='light'>light</p></div>";
    let dom = parse_html(html).expect("parse html");

    let host = find_element_by_id(&dom, "host").expect("host element");
    let shadow_roots: Vec<&DomNode> = host
      .children
      .iter()
      .filter(|child| matches!(child.node_type, DomNodeType::ShadowRoot { .. }))
      .collect();
    assert_eq!(
      shadow_roots.len(),
      1,
      "only the first declarative shadow template should attach"
    );

    assert!(
      shadow_roots[0]
        .children
        .iter()
        .any(|child| child.get_attribute_ref("id") == Some("first")),
      "shadow root should be populated from the first template's content"
    );

    let remaining_templates = host
      .children
      .iter()
      .filter(|child| {
        child
          .tag_name()
          .map(|name| name.eq_ignore_ascii_case("template"))
          .unwrap_or(false)
      })
      .count();
    assert_eq!(
      remaining_templates, 1,
      "subsequent shadow templates should remain inert in the light DOM"
    );
  }

  #[test]
  fn declarative_shadow_dom_records_delegates_focus() {
    let html = "<div id='host'><template shadowroot='open' shadowrootdelegatesfocus><slot></slot></template></div>";
    let dom = parse_html(html).expect("parse html");

    let host = find_element_by_id(&dom, "host").expect("host element");
    let shadow_root = host
      .children
      .iter()
      .find(|child| matches!(child.node_type, DomNodeType::ShadowRoot { .. }))
      .expect("shadow root attached");
    match shadow_root.node_type {
      DomNodeType::ShadowRoot {
        mode,
        delegates_focus,
      } => {
        assert_eq!(mode, ShadowRootMode::Open);
        assert!(
          delegates_focus,
          "shadowrootdelegatesfocus should be recorded on the shadow root"
        );
      }
      _ => panic!("expected shadow root child"),
    }
  }

  #[test]
  fn slot_in_svg_is_treated_as_element() {
    let dom = parse_html("<svg><slot id=\"s\"></slot></svg>").expect("parse html");

    let slot = find_element_by_id(&dom, "s").expect("slot element");
    match &slot.node_type {
      DomNodeType::Element {
        namespace,
        tag_name,
        ..
      } => {
        assert_eq!(namespace, SVG_NAMESPACE, "should retain SVG namespace");
        assert!(tag_name.eq_ignore_ascii_case("slot"));
      }
      other => panic!("expected element node, got {:?}", other),
    }
  }

  #[test]
  fn declarative_shadow_dom_skips_in_inert_template() {
    let html = "<template><div id='host'><template shadowroot='open'><p id='shadow'>shadow</p></template></div></template>";
    let dom = parse_html(html).expect("parse html");

    find_element_by_id(&dom, "host").expect("host element inside template content");
    assert!(
      !contains_shadow_root(&dom),
      "shadow roots should not attach inside inert template contents"
    );
  }

  #[test]
  fn declarative_shadow_dom_attaches_outside_inert_template() {
    let html =
      "<div id='host'><template shadowroot='open'><p id='shadow'>shadow</p></template></div>";
    let dom = parse_html(html).expect("parse html");

    let host = find_element_by_id(&dom, "host").expect("host element");
    let shadow_root = host
      .children
      .iter()
      .find(|child| matches!(child.node_type, DomNodeType::ShadowRoot { .. }))
      .expect("shadow root attached when not in an inert template");
    assert!(
      shadow_root
        .children
        .iter()
        .any(|child| child.get_attribute_ref("id") == Some("shadow")),
      "shadow root should include children from the declarative template"
    );
  }

  fn matches(node: &DomNode, ancestors: &[&DomNode], pseudo: &PseudoClass) -> bool {
    let mut caches = SelectorCaches::default();
    let cache_epoch = next_selector_cache_epoch();
    caches.set_epoch(cache_epoch);
    let sibling_cache = SiblingListCache::new(cache_epoch);
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );
    context.extra_data = ShadowMatchData::for_document().with_sibling_cache(&sibling_cache);
    let element_ref = ElementRef::with_ancestors(node, ancestors);
    element_ref.match_non_ts_pseudo_class(pseudo, &mut context)
  }

  fn parse_selector_list(selector_list: &str) -> SelectorList<FastRenderSelectorImpl> {
    let mut input = ParserInput::new(selector_list);
    let mut parser = Parser::new(&mut input);
    SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::No)
      .expect("selector list should parse")
  }

  fn nth_of_cache_populations() -> u64 {
    super::NTH_OF_CACHE_POPULATIONS.with(|counter| counter.load(Ordering::Relaxed))
  }

  fn reset_nth_of_cache_populations() {
    super::NTH_OF_CACHE_POPULATIONS.with(|counter| counter.store(0, Ordering::Relaxed))
  }

  #[test]
  fn nth_child_of_selector_list_uses_nth_index_cache() {
    reset_nth_of_cache_populations();
    let foo_selectors = parse_selector_list(".foo");

    let mut children = Vec::new();
    for idx in 0..128usize {
      let class = if idx == 0 || idx == 64 || idx == 127 {
        vec![("class".to_string(), "foo".to_string())]
      } else {
        vec![]
      };
      children.push(DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: class,
        },
        children: vec![],
      });
    }
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children,
    };

    let ancestors: Vec<&DomNode> = vec![&parent];
    let non_matching = &parent.children[1];
    let second_foo = &parent.children[64];
    let last_foo = &parent.children[127];

    let nth_child = PseudoClass::NthChild(0, 2, Some(foo_selectors.clone()));

    let mut caches = SelectorCaches::default();
    let cache_epoch = next_selector_cache_epoch();
    caches.set_epoch(cache_epoch);
    let sibling_cache = SiblingListCache::new(cache_epoch);
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );
    context.extra_data = ShadowMatchData::for_document().with_sibling_cache(&sibling_cache);

    let non_matching_ref = ElementRef::with_ancestors(non_matching, &ancestors);
    assert!(
      !non_matching_ref.match_non_ts_pseudo_class(&nth_child, &mut context),
      "non-matching siblings should never match :nth-child(of ...)"
    );
    assert!(
      !non_matching_ref.match_non_ts_pseudo_class(&nth_child, &mut context),
      "non-matching siblings should not trigger repeated rescans"
    );

    let second_ref = ElementRef::with_ancestors(second_foo, &ancestors);
    assert!(
      second_ref.match_non_ts_pseudo_class(&nth_child, &mut context),
      "should match the 2nd .foo element"
    );

    assert_eq!(
      nth_of_cache_populations(),
      1,
      "nth-child(of ...) should populate nth-index cache once per parent+selector list"
    );

    let nth_last_child = PseudoClass::NthLastChild(0, 1, Some(foo_selectors));
    let last_ref = ElementRef::with_ancestors(last_foo, &ancestors);
    assert!(
      last_ref.match_non_ts_pseudo_class(&nth_last_child, &mut context),
      "should match the last .foo element via :nth-last-child(1 of .foo)"
    );
    assert!(
      !non_matching_ref.match_non_ts_pseudo_class(&nth_last_child, &mut context),
      "non-matching siblings should never match :nth-last-child(of ...)"
    );
    assert!(
      !non_matching_ref.match_non_ts_pseudo_class(&nth_last_child, &mut context),
      "non-matching siblings should not trigger repeated rescans for :nth-last-child(of ...)"
    );

    assert_eq!(
      nth_of_cache_populations(),
      2,
      "nth-last-child(of ...) should maintain an independent cached index map"
    );
  }

  #[test]
  fn selector_bloom_store_matches_legacy_map() {
    set_selector_bloom_enabled(true);

    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("id".to_string(), "host".to_string())],
      },
      children: vec![
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "span".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("class".to_string(), "light".to_string())],
          },
          children: vec![],
        },
        DomNode {
          node_type: DomNodeType::ShadowRoot {
            mode: ShadowRootMode::Open,
            delegates_focus: false,
          },
          children: vec![DomNode {
            node_type: DomNodeType::Element {
              tag_name: "span".to_string(),
              namespace: HTML_NAMESPACE.to_string(),
              attributes: vec![("class".to_string(), "shadow".to_string())],
            },
            children: vec![],
          }],
        },
      ],
    };

    let id_map = enumerate_dom_ids(&dom);
    let store =
      build_selector_bloom_store(&dom, &id_map).expect("selector bloom store");
    let legacy =
      build_selector_bloom_map_legacy(&dom).expect("legacy selector bloom map");

    assert!(
      store.summary_for_id(0).is_none(),
      "selector bloom store index 0 must remain unused"
    );

    for (ptr, id) in id_map.iter() {
      // Safety: ids are built from stable DOM pointers.
      let node = unsafe { &**ptr };
      if !node.is_element() {
        continue;
      }
      let summary_store = store
        .summary_for_id(*id)
        .unwrap_or_else(|| panic!("store missing summary for node_id={id}"));
      let summary_legacy = legacy
        .get(ptr)
        .unwrap_or_else(|| panic!("legacy missing summary for node_id={id}"));
      assert_eq!(
        summary_store, summary_legacy,
        "selector bloom summary mismatch for node_id={id}"
      );
    }
  }

  #[test]
  fn bloom_pruning_skips_expensive_evaluations() {
    reset_has_counters();
    set_selector_bloom_enabled(true);
    let dom = element("div", vec![element("span", vec![])]);
    let id_map = enumerate_dom_ids(&dom);
    let bloom_store =
      build_selector_bloom_store(&dom, &id_map).expect("selector bloom store");

    let mut caches = SelectorCaches::default();
    caches.set_epoch(next_selector_cache_epoch());
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );
    context.extra_data = ShadowMatchData::for_document().with_selector_blooms(Some(&bloom_store));

    let mut input = ParserInput::new(".missing");
    let mut parser = Parser::new(&mut input);
    let list =
      SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::ForHas).expect("parse");
    let selectors = build_relative_selectors(list);
    let anchor = ElementRef::with_ancestors(&dom, &[]).with_node_id(1);

    assert!(
      !matches_has_relative(&anchor, &selectors, &mut context),
      ":has should prune when bloom hash is absent"
    );

    let counters = capture_has_counters();
    assert_eq!(counters.prunes, 1);
    assert_eq!(counters.evaluated, 0);
  }

  #[test]
  fn bloom_pruning_preserves_matches() {
    reset_has_counters();
    set_selector_bloom_enabled(true);
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".into(),
        namespace: HTML_NAMESPACE.into(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".into(),
          namespace: HTML_NAMESPACE.into(),
          attributes: vec![("class".into(), "foo".into())],
        },
        children: vec![],
      }],
    };
    let id_map = enumerate_dom_ids(&dom);
    let bloom_store =
      build_selector_bloom_store(&dom, &id_map).expect("selector bloom store");

    let mut caches = SelectorCaches::default();
    caches.set_epoch(next_selector_cache_epoch());
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );
    context.extra_data = ShadowMatchData::for_document().with_selector_blooms(Some(&bloom_store));

    let mut input = ParserInput::new(".foo");
    let mut parser = Parser::new(&mut input);
    let list =
      SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::ForHas).expect("parse");
    let selectors = build_relative_selectors(list);
    let anchor = ElementRef::with_ancestors(&dom, &[]).with_node_id(1);

    assert!(
      matches_has_relative(&anchor, &selectors, &mut context),
      ":has should still evaluate when hashes are present"
    );

    let counters = capture_has_counters();
    assert_eq!(counters.prunes, 0);
    assert_eq!(counters.evaluated, 1);
  }

  fn eval_relative_selector_with_ancestor_bloom(
    dom: &DomNode,
    selector: &RelativeSelector<FastRenderSelectorImpl>,
    use_ancestor_bloom: bool,
  ) -> bool {
    let mut caches = SelectorCaches::default();
    let cache_epoch = next_selector_cache_epoch();
    caches.set_epoch(cache_epoch);
    let sibling_cache = SiblingListCache::new(cache_epoch);
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );
    context.extra_data = ShadowMatchData::for_document().with_sibling_cache(&sibling_cache);

    let anchor = ElementRef::with_ancestors(dom, &[]);
    context.nest_for_relative_selector(anchor.opaque(), |ctx| {
      ctx.nest_for_scope(Some(anchor.opaque()), |ctx| {
        let mut ancestors = RelativeSelectorAncestorStack::new(anchor.all_ancestors);
        let mut deadline_counter = 0usize;
        let mut ancestor_bloom_filter = BloomFilter::new();

        let matched = match_relative_selector(
          selector,
          anchor.node,
          &mut ancestors,
          &mut ancestor_bloom_filter,
          use_ancestor_bloom,
          ctx,
          &mut deadline_counter,
        );
        debug_assert_eq!(ancestors.len(), ancestors.baseline_len());
        debug_assert!(!use_ancestor_bloom || ancestor_bloom_filter.is_zeroed());
        matched
      })
    })
  }

  #[test]
  fn has_relative_selector_ancestor_bloom_matches_without_bloom() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".into(),
        namespace: HTML_NAMESPACE.into(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".into(),
          namespace: HTML_NAMESPACE.into(),
          attributes: vec![("class".into(), "a".into())],
        },
        children: vec![DomNode {
          node_type: DomNodeType::Element {
            tag_name: "div".into(),
            namespace: HTML_NAMESPACE.into(),
            attributes: vec![("class".into(), "b".into())],
          },
          children: vec![DomNode {
            node_type: DomNodeType::Element {
              tag_name: "div".into(),
              namespace: HTML_NAMESPACE.into(),
              attributes: vec![("class".into(), "c".into())],
            },
            children: vec![],
          }],
        }],
      }],
    };

    let mut input = ParserInput::new(".a .b .c");
    let mut parser = Parser::new(&mut input);
    let list =
      SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::ForHas).expect("parse");
    let selectors = build_relative_selectors(list);
    assert_eq!(selectors.len(), 1);

    let with_bloom = eval_relative_selector_with_ancestor_bloom(&dom, &selectors[0], true);
    let without_bloom = eval_relative_selector_with_ancestor_bloom(&dom, &selectors[0], false);
    assert!(with_bloom);
    assert_eq!(with_bloom, without_bloom);
  }

  #[test]
  fn has_relative_selector_ancestor_bloom_rejects_by_ancestor_chain() {
    // Contains `.a`, `.b` and many `.c` nodes, but `.b` is not a descendant of `.a`,
    // so `.a .b .c` can never match.
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".into(),
        namespace: HTML_NAMESPACE.into(),
        attributes: vec![],
      },
      children: vec![
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "div".into(),
            namespace: HTML_NAMESPACE.into(),
            attributes: vec![("class".into(), "a".into())],
          },
          children: vec![],
        },
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "div".into(),
            namespace: HTML_NAMESPACE.into(),
            attributes: vec![("class".into(), "b".into())],
          },
          children: (0..32)
            .map(|_| DomNode {
              node_type: DomNodeType::Element {
                tag_name: "div".into(),
                namespace: HTML_NAMESPACE.into(),
                attributes: vec![("class".into(), "c".into())],
              },
              children: vec![],
            })
            .collect(),
        },
      ],
    };

    let mut input = ParserInput::new(".a .b .c");
    let mut parser = Parser::new(&mut input);
    let list =
      SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::ForHas).expect("parse");
    let selectors = build_relative_selectors(list);
    assert_eq!(selectors.len(), 1);

    let with_bloom = eval_relative_selector_with_ancestor_bloom(&dom, &selectors[0], true);
    let without_bloom = eval_relative_selector_with_ancestor_bloom(&dom, &selectors[0], false);
    assert!(!with_bloom);
    assert_eq!(with_bloom, without_bloom);
  }

  #[test]
  fn collect_text_codepoints_skips_template_contents() {
    let dom = DomNode {
      node_type: DomNodeType::Document {
        quirks_mode: QuirksMode::NoQuirks,
      },
      children: vec![
        element("div", vec![text("abc")]),
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "template".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![text("Ωש")],
        },
      ],
    };

    let codepoints = collect_text_codepoints(&dom);
    let expected: Vec<u32> = vec!['a', 'b', 'c'].into_iter().map(|c| c as u32).collect();
    assert_eq!(codepoints, expected);
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
    let cache_epoch = next_selector_cache_epoch();
    caches.set_epoch(cache_epoch);
    let sibling_cache = SiblingListCache::new(cache_epoch);
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );
    context.extra_data = ShadowMatchData::for_document().with_sibling_cache(&sibling_cache);
    matches_selector(selector, 0, None, element, &mut context)
  }

  #[test]
  fn root_element_has_no_element_parent() {
    let document = DomNode {
      node_type: DomNodeType::Document {
        quirks_mode: QuirksMode::NoQuirks,
      },
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
        delegates_focus: false,
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
        delegates_focus: false,
      },
      children: vec![shadow_child],
    };
    let host = element("div", vec![shadow_root]);
    let body = element("body", vec![host]);
    let html = element("html", vec![body]);
    let document = DomNode {
      node_type: DomNodeType::Document {
        quirks_mode: QuirksMode::NoQuirks,
      },
      children: vec![html],
    };

    let mut input = ParserInput::new("body span");
    let mut parser = Parser::new(&mut input);
    let selector_list =
      SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::No).expect("parse");
    let selector = selector_list.slice().first().expect("expected a selector");

    let mut caches = SelectorCaches::default();
    let cache_epoch = next_selector_cache_epoch();
    caches.set_epoch(cache_epoch);
    let sibling_cache = SiblingListCache::new(cache_epoch);
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );
    context.extra_data = ShadowMatchData::for_document().with_sibling_cache(&sibling_cache);

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
      node_type: DomNodeType::Document {
        quirks_mode: QuirksMode::NoQuirks,
      },
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

  #[test]
  fn is_part_checks_whitespace_tokens() {
    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("part".to_string(), "header body\tfooter".to_string())],
      },
      children: vec![],
    };
    let element = ElementRef::new(&node);
    assert!(element.is_part(&CssString::from("header")));
    assert!(element.is_part(&CssString::from("footer")));
    assert!(!element.is_part(&CssString::from("aside")));
  }

  #[test]
  fn imported_part_handles_aliases_and_identity() {
    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("exportparts".to_string(), "label, inner:outer".to_string())],
      },
      children: vec![],
    };
    let element = ElementRef::new(&node);

    assert_eq!(
      element.imported_part(&CssString::from("label")),
      Some(CssString::from("label"))
    );
    assert_eq!(
      element.imported_part(&CssString::from("outer")),
      Some(CssString::from("inner"))
    );
    assert_eq!(element.imported_part(&CssString::from("missing")), None);
  }

  fn collect_wbr_texts(node: &DomNode, out: &mut Vec<String>) {
    if let DomNodeType::Element { tag_name, .. } = &node.node_type {
      if tag_name.eq_ignore_ascii_case("wbr") {
        for child in node.children.iter() {
          if let DomNodeType::Text { content } = &child.node_type {
            out.push(content.clone());
          }
        }
      }
    }
    for child in node.children.iter() {
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
  fn has_class_matches_with_and_without_attr_cache() {
    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "a B\tc".to_string())],
      },
      children: vec![],
    };

    let cache = ElementAttrCache::new(0);
    let cached = ElementRef::new(&node).with_attr_cache(Some(&cache));
    let uncached = ElementRef::new(&node);

    let a = CssString::from("a");
    assert!(uncached.has_class(&a, CaseSensitivity::CaseSensitive));
    assert_eq!(
      uncached.has_class(&a, CaseSensitivity::CaseSensitive),
      cached.has_class(&a, CaseSensitivity::CaseSensitive)
    );

    let b_lower = CssString::from("b");
    assert!(!uncached.has_class(&b_lower, CaseSensitivity::CaseSensitive));
    assert_eq!(
      uncached.has_class(&b_lower, CaseSensitivity::CaseSensitive),
      cached.has_class(&b_lower, CaseSensitivity::CaseSensitive)
    );
    assert!(uncached.has_class(&b_lower, CaseSensitivity::AsciiCaseInsensitive));
    assert_eq!(
      uncached.has_class(&b_lower, CaseSensitivity::AsciiCaseInsensitive),
      cached.has_class(&b_lower, CaseSensitivity::AsciiCaseInsensitive)
    );

    let missing = CssString::from("missing");
    assert!(!uncached.has_class(&missing, CaseSensitivity::AsciiCaseInsensitive));
    assert_eq!(
      uncached.has_class(&missing, CaseSensitivity::AsciiCaseInsensitive),
      cached.has_class(&missing, CaseSensitivity::AsciiCaseInsensitive)
    );
  }

  #[test]
  fn has_class_large_token_list_matches_with_and_without_attr_cache() {
    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "c0 C1 c2 C3 c4 C5 c6 C7".to_string())],
      },
      children: vec![],
    };

    let cache = ElementAttrCache::new(0);
    let cached = ElementRef::new(&node).with_attr_cache(Some(&cache));
    let uncached = ElementRef::new(&node);

    let query = CssString::from("c3");
    assert!(!uncached.has_class(&query, CaseSensitivity::CaseSensitive));
    assert!(uncached.has_class(&query, CaseSensitivity::AsciiCaseInsensitive));
    assert_eq!(
      uncached.has_class(&query, CaseSensitivity::CaseSensitive),
      cached.has_class(&query, CaseSensitivity::CaseSensitive)
    );
    assert_eq!(
      uncached.has_class(&query, CaseSensitivity::AsciiCaseInsensitive),
      cached.has_class(&query, CaseSensitivity::AsciiCaseInsensitive)
    );
  }

  #[test]
  fn attr_matches_name_casing_matches_with_and_without_attr_cache() {
    use selectors::attr::AttrSelectorOperation;
    use selectors::attr::NamespaceConstraint;

    let ns: NamespaceConstraint<&CssString> = NamespaceConstraint::Any;
    let op: AttrSelectorOperation<&CssString> = AttrSelectorOperation::Exists;

    let mut html_attrs = vec![("DaTa-X".to_string(), "1".to_string())];
    for idx in 0..12 {
      html_attrs.push((format!("data-a{idx}"), "x".to_string()));
    }
    let html_node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: html_attrs,
      },
      children: vec![],
    };

    let html_cache = ElementAttrCache::new(0);
    let html_cached = ElementRef::new(&html_node).with_attr_cache(Some(&html_cache));
    let html_uncached = ElementRef::new(&html_node);
    let data_x = CssString::from("data-x");
    assert!(html_uncached.attr_matches(&ns, &data_x, &op));
    assert_eq!(
      html_uncached.attr_matches(&ns, &data_x, &op),
      html_cached.attr_matches(&ns, &data_x, &op)
    );

    let mut svg_attrs = vec![("viewBox".to_string(), "0 0 10 10".to_string())];
    for idx in 0..12 {
      svg_attrs.push((format!("attr{idx}"), "y".to_string()));
    }
    let svg_node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "svg".to_string(),
        namespace: SVG_NAMESPACE.to_string(),
        attributes: svg_attrs,
      },
      children: vec![],
    };

    let svg_cache = ElementAttrCache::new(0);
    let svg_cached = ElementRef::new(&svg_node).with_attr_cache(Some(&svg_cache));
    let svg_uncached = ElementRef::new(&svg_node);
    let viewbox_lower = CssString::from("viewbox");
    assert!(!svg_uncached.attr_matches(&ns, &viewbox_lower, &op));
    assert_eq!(
      svg_uncached.attr_matches(&ns, &viewbox_lower, &op),
      svg_cached.attr_matches(&ns, &viewbox_lower, &op)
    );
    let viewbox = CssString::from("viewBox");
    assert!(svg_uncached.attr_matches(&ns, &viewbox, &op));
    assert_eq!(
      svg_uncached.attr_matches(&ns, &viewbox, &op),
      svg_cached.attr_matches(&ns, &viewbox, &op)
    );
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
  fn dir_auto_ignores_template_contents() {
    let template = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "template".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![text("שלום")],
    };
    let root = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("dir".to_string(), "auto".to_string())],
      },
      children: vec![template],
    };

    assert_eq!(resolve_first_strong_direction(&root), None);
    assert!(matches(&root, &[], &PseudoClass::Dir(TextDirection::Ltr)));
    assert!(!matches(&root, &[], &PseudoClass::Dir(TextDirection::Rtl)));
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

    let newline_only_textarea = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "textarea".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("placeholder".to_string(), "Describe".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Text {
          content: "\n".to_string(),
        },
        children: vec![],
      }],
    };
    assert!(matches(
      &newline_only_textarea,
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

    let formatted_prefilled_textarea = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "textarea".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("placeholder".to_string(), "Describe".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Text {
          content: "\nHello".to_string(),
        },
        children: vec![],
      }],
    };
    assert!(!matches(
      &formatted_prefilled_textarea,
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
  fn range_inputs_match_in_range_by_default() {
    let input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "range".to_string()),
          ("min".to_string(), "0".to_string()),
          ("max".to_string(), "10".to_string()),
        ],
      },
      children: vec![],
    };

    assert!(matches(&input, &[], &PseudoClass::InRange));
    assert!(!matches(&input, &[], &PseudoClass::OutOfRange));
  }

  #[test]
  fn range_values_are_clamped_before_range_state_checks() {
    let input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "range".to_string()),
          ("min".to_string(), "0".to_string()),
          ("max".to_string(), "10".to_string()),
          ("value".to_string(), "20".to_string()),
        ],
      },
      children: vec![],
    };

    // The HTML value sanitization algorithm clamps the current value into the [min, max] range,
    // so this remains in-range despite the authored value being too high.
    assert!(matches(&input, &[], &PseudoClass::InRange));
    assert!(!matches(&input, &[], &PseudoClass::OutOfRange));
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
  fn link_pseudo_classes_match_case_insensitive_tag_names() {
    let unvisited = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "A".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "https://example.com".to_string())],
      },
      children: vec![],
    };
    assert!(matches(&unvisited, &[], &PseudoClass::AnyLink));
    assert!(matches(&unvisited, &[], &PseudoClass::Link));
    assert!(!matches(&unvisited, &[], &PseudoClass::Visited));

    let visited = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "A".to_string(),
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
    let cache_epoch = next_selector_cache_epoch();
    caches.set_epoch(cache_epoch);
    let sibling_cache = SiblingListCache::new(cache_epoch);
    let mut context = MatchingContext::new(
      MatchingMode::ForStatelessPseudoElement,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );
    context.extra_data = ShadowMatchData::for_document().with_sibling_cache(&sibling_cache);
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

  #[test]
  fn top_layer_state_sets_open_for_dialog_and_popover() {
    let mut dom = document(vec![
      element_with_attrs(
        "DIALOG",
        vec![("id", "dialog-open"), ("data-fastr-open", "TRUE")],
        vec![],
      ),
      element_with_attrs(
        "dialog",
        vec![
          ("id", "dialog-closed"),
          ("open", ""),
          ("data-fastr-open", "false"),
        ],
        vec![],
      ),
      element_with_attrs(
        "div",
        vec![("id", "popover-open"), ("popover", ""), ("data-fastr-open", "oPeN")],
        vec![],
      ),
      element_with_attrs(
        "div",
        vec![
          ("id", "popover-closed"),
          ("popover", ""),
          ("open", ""),
          ("data-fastr-open", "FALSE"),
        ],
        vec![],
      ),
    ]);

    apply_top_layer_state_with_deadline(&mut dom).expect("apply top-layer state");

    let dialog_open = find_element_by_id(&dom, "dialog-open").expect("dialog-open");
    assert!(
      dialog_open.get_attribute_ref("open").is_some(),
      "data-fastr-open should force dialog open"
    );

    let dialog_closed = find_element_by_id(&dom, "dialog-closed").expect("dialog-closed");
    assert!(
      dialog_closed.get_attribute_ref("open").is_none(),
      "data-fastr-open=false should remove dialog open"
    );

    let popover_open = find_element_by_id(&dom, "popover-open").expect("popover-open");
    assert!(
      popover_open.get_attribute_ref("open").is_some(),
      "data-fastr-open should force popover open"
    );
    assert!(
      popover_open.get_attribute_ref("data-fastr-inert").is_none(),
      "non-modal top-layer content should not inert the document"
    );

    let popover_closed = find_element_by_id(&dom, "popover-closed").expect("popover-closed");
    assert!(
      popover_closed.get_attribute_ref("open").is_none(),
      "data-fastr-open=false should remove popover open"
    );
  }

  #[test]
  fn top_layer_state_inerts_outside_modal_dialog() {
    let mut dom = document(vec![
      element_with_attrs(
        "div",
        vec![("id", "outside")],
        vec![element_with_attrs("span", vec![("id", "outside-child")], vec![])],
      ),
      element_with_attrs(
        "dialog",
        vec![("id", "modal"), ("data-fastr-open", "modal")],
        vec![element_with_attrs("p", vec![("id", "inside")], vec![])],
      ),
      element_with_attrs(
        "div",
        vec![("id", "popover"), ("popover", ""), ("data-fastr-open", "true")],
        vec![],
      ),
    ]);

    apply_top_layer_state_with_deadline(&mut dom).expect("apply top-layer state");

    let outside = find_element_by_id(&dom, "outside").expect("outside");
    assert_eq!(
      outside.get_attribute_ref("data-fastr-inert"),
      Some("true"),
      "outside subtree should be inert when a modal dialog is open"
    );
    let outside_child = find_element_by_id(&dom, "outside-child").expect("outside-child");
    assert_eq!(
      outside_child.get_attribute_ref("data-fastr-inert"),
      Some("true"),
      "descendants outside the modal subtree should also be inert"
    );

    let modal = find_element_by_id(&dom, "modal").expect("modal");
    assert!(
      modal.get_attribute_ref("open").is_some(),
      "modal dialog should be forced open"
    );
    assert!(
      modal.get_attribute_ref("data-fastr-inert").is_none(),
      "modal subtree should not be inert"
    );

    let inside = find_element_by_id(&dom, "inside").expect("inside");
    assert!(
      inside.get_attribute_ref("data-fastr-inert").is_none(),
      "modal descendants should not be inert"
    );

    let popover = find_element_by_id(&dom, "popover").expect("popover");
    assert!(
      popover.get_attribute_ref("open").is_some(),
      "popover open state should still be applied"
    );
    assert_eq!(
      popover.get_attribute_ref("data-fastr-inert"),
      Some("true"),
      "popover outside modal subtree should be inert"
    );
  }

  #[test]
  fn top_layer_state_does_not_inert_for_non_modal_dialogs() {
    let mut dom = document(vec![
      element_with_attrs("div", vec![("id", "outside")], vec![]),
      element_with_attrs(
        "dialog",
        vec![("id", "dialog"), ("open", "")],
        vec![element_with_attrs("p", vec![("id", "inside")], vec![])],
      ),
      element_with_attrs("div", vec![("id", "outside2")], vec![]),
    ]);

    apply_top_layer_state_with_deadline(&mut dom).expect("apply top-layer state");

    let outside = find_element_by_id(&dom, "outside").expect("outside");
    let outside2 = find_element_by_id(&dom, "outside2").expect("outside2");
    assert!(outside.get_attribute_ref("data-fastr-inert").is_none());
    assert!(outside2.get_attribute_ref("data-fastr-inert").is_none());

    let dialog = find_element_by_id(&dom, "dialog").expect("dialog");
    assert!(
      dialog.get_attribute_ref("open").is_some(),
      "open dialogs should remain open"
    );
  }
}
