//! CSS Cascade and Style Application
//!
//! This module handles applying CSS rules to DOM nodes to produce
//! computed styles. It implements the CSS cascade algorithm.
//!
//! Reference: CSS Cascading and Inheritance Level 4
//! <https://www.w3.org/TR/css-cascade-4/>

use crate::css::parser::parse_declarations;
use crate::css::parser::parse_inline_style_declarations;
use crate::css::parser::parse_stylesheet;
use crate::css::selectors::FastRenderSelectorImpl;
use crate::css::selectors::PseudoClass;
use crate::css::selectors::PseudoElement;
use crate::css::selectors::ShadowMatchData;
use crate::css::selectors::SlotAssignmentMap;
use crate::css::selectors::TextDirection;
use crate::css::types::CollectedRule;
use crate::css::types::ContainerCondition;
use crate::css::types::ContainerQuery;
use crate::css::types::ContainerStyleQuery;
use crate::css::types::CssImportLoader;
use crate::css::types::CssString;
use crate::css::types::Declaration;
use crate::css::types::PropertyValue;
use crate::css::types::ScopeContext;
use crate::css::types::StyleRule;
use crate::css::types::StyleSheet;
use crate::debug::runtime;
use crate::dom::ancestor_bloom_enabled;
use crate::dom::build_selector_bloom_store;
use crate::dom::compute_slot_assignment_with_ids;
use crate::dom::enumerate_dom_ids;
use crate::dom::next_selector_cache_epoch;
use crate::dom::parse_exportparts;
use crate::dom::reset_has_counters;
use crate::dom::resolve_first_strong_direction;
use crate::dom::with_target_fragment;
use crate::dom::DomNode;
use crate::dom::DomNodeType;
use crate::dom::ElementAttrCache;
use crate::dom::ElementRef;
use crate::dom::SelectorBloomStore;
use crate::dom::SelectorBloomSummaryRef;
use crate::dom::SiblingListCache;
use crate::dom::SlotAssignment;
use crate::dom::HTML_NAMESPACE;
use crate::error::Error;
use crate::geometry::Size;
use crate::render_control::check_active_periodic;
use crate::render_control::RenderDeadline;
use crate::style::color::{Color, Rgba};
use crate::style::custom_properties::CustomPropertyRegistry;
use crate::style::defaults::get_default_styles_for_element;
use crate::style::defaults::parse_color_attribute;
use crate::style::defaults::parse_dimension_attribute;
use crate::style::display::Display;
use crate::style::grid::finalize_grid_placement;
use crate::style::media::ColorScheme;
use crate::style::media::MediaContext;
use crate::style::media::MediaQueryCache;
use crate::style::normalize_language_tag;
use crate::style::properties::apply_declaration_with_base;
use crate::style::properties::resolve_pending_logical_properties;
use crate::style::properties::with_image_set_dpr;
use crate::style::style_set::StyleSet;
use crate::style::types::BorderCornerRadius;
use crate::style::types::ColorSchemeEntry;
use crate::style::types::ColorSchemePreference;
use crate::style::types::ContainerType;
use crate::style::types::OutlineColor;
use crate::style::types::PointerEvents;
use crate::style::values::Length;
use crate::style::values::LengthUnit;
use crate::style::ComputedStyle;
use crate::style::Direction;
use crate::style::TopLayerKind;
use crate::{error::RenderError, error::RenderStage};
#[cfg(test)]
use cssparser::ToCss;
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
use selectors::context::IncludeStartingStyle;
use selectors::context::QuirksMode;
use selectors::context::SelectorCaches;
use selectors::context::VisitedHandlingMode;
use selectors::matching::matches_selector;
use selectors::matching::selector_may_match;
use selectors::matching::MatchingContext;
use selectors::matching::MatchingMode;
use selectors::parser::AncestorHashes;
use selectors::parser::Selector;
use selectors::parser::SelectorList;
use selectors::Element;
use std::borrow::Cow;
use std::cell::Cell;
use std::cell::RefCell;
use std::cmp::Ordering as CmpOrdering;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::{BuildHasherDefault, Hash, Hasher};
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::OnceLock;
use std::time::Instant;

/// User-agent stylesheet containing default browser styles
const USER_AGENT_STYLESHEET: &str = include_str!("../user_agent.css");
static UA_STYLESHEET: OnceLock<StyleSheet> = OnceLock::new();
static DEFAULT_COMPUTED_STYLE: OnceLock<ComputedStyle> = OnceLock::new();

fn default_computed_style() -> &'static ComputedStyle {
  DEFAULT_COMPUTED_STYLE.get_or_init(ComputedStyle::default)
}

// Optional cascade profiling for large-page performance analysis.
// Enable with FASTR_CASCADE_PROFILE=1.
static CASCADE_PROFILE_NODES: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_RULE_CANDIDATES: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_RULE_MATCHES: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_RULE_PRUNED: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_CANDIDATES_BY_ID: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_CANDIDATES_BY_CLASS: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_CANDIDATES_BY_TAG: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_CANDIDATES_BY_ATTR: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_CANDIDATES_UNIVERSAL: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_SELECTOR_BLOOM_BUILT: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_SELECTOR_BLOOM_TIME_NS: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_SELECTOR_BLOOM_FAST_REJECTS: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_SELECTOR_ATTEMPTS_TOTAL: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_SELECTOR_ATTEMPTS_AFTER_BLOOM: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_SELECTOR_RIGHTMOST_FAST_REJECTS: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_SELECTOR_MATCH_CALLS: AtomicU64 = AtomicU64::new(0);

thread_local! {
  /// Controls whether the ancestor bloom filter is reset at `DomNodeType::ShadowRoot` boundaries.
  ///
  /// When enabled, the bloom filter for nodes inside a shadow root only contains hashes for
  /// ancestors within that shadow tree. This improves fast-reject effectiveness by avoiding
  /// irrelevant outer-tree hashes (which can only increase false positives).
  static ANCESTOR_BLOOM_SHADOW_SCOPING_ENABLED: Cell<bool> = const { Cell::new(true) };
}

pub fn ancestor_bloom_shadow_scoping_enabled() -> bool {
  ANCESTOR_BLOOM_SHADOW_SCOPING_ENABLED.with(|flag| flag.get())
}

/// Toggle shadow-boundary scoping of the cascade ancestor bloom filter.
///
/// This is primarily intended for benchmarking and tests; normal callers should rely on the
/// default (enabled).
pub fn set_ancestor_bloom_shadow_scoping_enabled(enabled: bool) {
  ANCESTOR_BLOOM_SHADOW_SCOPING_ENABLED.with(|flag| flag.set(enabled));
}

const CASCADE_NODE_DEADLINE_STRIDE: usize = 1024;
const CASCADE_SELECTOR_DEADLINE_STRIDE: usize = 64;

fn quirks_mode_for_dom(dom: &DomNode) -> QuirksMode {
  match &dom.node_type {
    DomNodeType::Document { quirks_mode } => *quirks_mode,
    _ => QuirksMode::NoQuirks,
  }
}

fn is_root_element(ancestors: &[&DomNode]) -> bool {
  ancestors.is_empty()
    || (ancestors.len() == 1 && matches!(ancestors[0].node_type, DomNodeType::Document { .. }))
}
static CASCADE_PROFILE_FIND_TIME_NS: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_DECL_TIME_NS: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_PSEUDO_TIME_NS: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_ENABLED: AtomicBool = AtomicBool::new(false);
static CASCADE_PROFILE_INITIALIZED: OnceLock<()> = OnceLock::new();

// Unit-test-only counter tracking the number of full scope-resolutions executed
// (i.e., cache misses). Kept thread-local so parallel tests don't interfere.
#[cfg(test)]
thread_local! {
  static SCOPE_RESOLVE_MISSES: std::cell::Cell<u64> = std::cell::Cell::new(0);
}

#[cfg(test)]
thread_local! {
  static ANCESTOR_BLOOM_HASH_INSERTS: std::cell::Cell<u64> = std::cell::Cell::new(0);
}

fn attr_truthy_value(value: &str) -> bool {
  let value = value.trim();
  value == "1"
    || value.eq_ignore_ascii_case("true")
    || value.eq_ignore_ascii_case("yes")
    || value.eq_ignore_ascii_case("on")
    || value.eq_ignore_ascii_case("open")
}

fn attr_truthy(node: &DomNode, name: &str) -> bool {
  node
    .get_attribute_ref(name)
    .map(attr_truthy_value)
    .unwrap_or(false)
}

fn data_fastr_open(node: &DomNode) -> Option<(bool, bool)> {
  let value = node.get_attribute_ref("data-fastr-open")?.trim();
  if value.eq_ignore_ascii_case("false") {
    return Some((false, false));
  }
  if value.eq_ignore_ascii_case("modal") {
    return Some((true, true));
  }
  if attr_truthy_value(value) {
    return Some((true, false));
  }
  None
}

fn dialog_top_layer(node: &DomNode) -> Option<TopLayerKind> {
  if !node
    .tag_name()
    .map(|t| t.eq_ignore_ascii_case("dialog"))
    .unwrap_or(false)
  {
    return None;
  }

  let mut open = node.get_attribute_ref("open").is_some();
  let mut modal = attr_truthy(node, "data-fastr-modal");
  if let Some((open_override, modal_override)) = data_fastr_open(node) {
    open = open_override;
    modal |= modal_override;
  }

  if !open {
    return None;
  }

  Some(TopLayerKind::Dialog { modal })
}

fn popover_top_layer(node: &DomNode) -> Option<TopLayerKind> {
  if node.get_attribute_ref("popover").is_none() {
    return None;
  }
  let mut open = node.get_attribute_ref("open").is_some();
  if let Some((open_override, _)) = data_fastr_open(node) {
    open = open_override;
  }
  if open {
    Some(TopLayerKind::Popover)
  } else {
    None
  }
}

fn container_query_matches(
  container_id: usize,
  query: &ContainerQuery,
  container: &ContainerQueryInfo,
  ctx: &ContainerQueryContext,
  ctx_ptr: usize,
  guard: &mut Vec<usize>,
  memo: &mut ContainerQueryMemo,
) -> bool {
  let key = QueryEvalCacheKey {
    ctx_ptr,
    container_id,
    query_ptr: query as *const _ as usize,
  };
  if let Some(hit) = memo.query_eval.get(&key).copied() {
    return hit;
  }

  match query {
    ContainerQuery::Size(mq) => {
      if !matches!(
        container.container_type,
        ContainerType::Size | ContainerType::InlineSize
      ) {
        memo.query_eval.insert(key, false);
        return false;
      }
      let mut media_ctx = ctx.base_media.clone();
      media_ctx.viewport_width = container.inline_size;
      media_ctx.viewport_height = match container.container_type {
        ContainerType::InlineSize => f32::NAN,
        _ => container.block_size,
      };
      media_ctx.base_font_size = container.font_size;
      let result = media_ctx.evaluate(mq);
      memo.query_eval.insert(key, result);
      result
    }
    ContainerQuery::Style(style_query) => {
      if !matches!(
        container.container_type,
        ContainerType::Size | ContainerType::InlineSize | ContainerType::Style
      ) {
        memo.query_eval.insert(key, false);
        return false;
      }
      let result = matches_style_query(style_query, container.styles.as_ref());
      memo.query_eval.insert(key, result);
      result
    }
    ContainerQuery::Not(inner) => {
      let result =
        !container_query_matches(container_id, inner, container, ctx, ctx_ptr, guard, memo);
      memo.query_eval.insert(key, result);
      result
    }
    ContainerQuery::And(list) => {
      let result = list.iter().all(|inner| {
        container_query_matches(container_id, inner, container, ctx, ctx_ptr, guard, memo)
      });
      memo.query_eval.insert(key, result);
      result
    }
    ContainerQuery::Or(list) => {
      let result = list.iter().any(|inner| {
        container_query_matches(container_id, inner, container, ctx, ctx_ptr, guard, memo)
      });
      memo.query_eval.insert(key, result);
      result
    }
  }
}

fn matches_style_query(query: &ContainerStyleQuery, styles: &ComputedStyle) -> bool {
  match query {
    ContainerStyleQuery::CustomProperty { name, value } => {
      let Some(actual) = styles.custom_properties.get(name) else {
        return false;
      };
      match value {
        Some(expected) => normalize_query_value(&actual.value) == normalize_query_value(expected),
        None => true,
      }
    }
    ContainerStyleQuery::Property { name, value } => match name.as_str() {
      "color" => Color::parse(value)
        .map(|color: Color| color.to_rgba(styles.color) == styles.color)
        .unwrap_or(false),
      "background-color" => Color::parse(value)
        .map(|color: Color| color.to_rgba(styles.color) == styles.background_color)
        .unwrap_or(false),
      _ => false,
    },
  }
}

fn normalize_query_value(value: &str) -> String {
  value.trim().trim_end_matches(';').trim().to_string()
}

// Container query evaluation memoization.
//
// Container conditions are cloned per collected rule, so pointer-based caching is ineffective.
// Instead we cache based on a small, spec-relevant signature:
// - the optional container name
// - the set of container types that can *possibly* satisfy the condition's query list
//   (i.e. the container-type gating used while selecting a query container).
//
// The cache is thread-local and cleared at the start of each cascade pass.
const CQ_SUPPORT_SIZE: u8 = 1 << 0;
const CQ_SUPPORT_INLINE_SIZE: u8 = 1 << 1;
const CQ_SUPPORT_STYLE: u8 = 1 << 2;
const CQ_SUPPORT_ALL: u8 = CQ_SUPPORT_SIZE | CQ_SUPPORT_INLINE_SIZE | CQ_SUPPORT_STYLE;

fn cq_type_bit(container_type: ContainerType) -> u8 {
  match container_type {
    ContainerType::Size => CQ_SUPPORT_SIZE,
    ContainerType::InlineSize => CQ_SUPPORT_INLINE_SIZE,
    ContainerType::Style => CQ_SUPPORT_STYLE,
    _ => 0,
  }
}

fn cq_query_support_mask(query: &ContainerQuery) -> u8 {
  match query {
    ContainerQuery::Size(_) => CQ_SUPPORT_SIZE | CQ_SUPPORT_INLINE_SIZE,
    ContainerQuery::Style(_) => CQ_SUPPORT_ALL,
    ContainerQuery::Not(inner) => cq_query_support_mask(inner),
    ContainerQuery::And(list) => {
      if list.is_empty() {
        CQ_SUPPORT_ALL
      } else {
        list.iter().fold(CQ_SUPPORT_ALL, |acc, inner| {
          acc & cq_query_support_mask(inner)
        })
      }
    }
    ContainerQuery::Or(list) => list
      .iter()
      .fold(0u8, |acc, inner| acc | cq_query_support_mask(inner)),
  }
}

fn cq_condition_support_mask(condition: &ContainerCondition) -> u8 {
  condition
    .query_list
    .iter()
    .fold(0u8, |acc, query| acc | cq_query_support_mask(query))
}

#[derive(Debug, Clone, Copy)]
struct ConditionSignature {
  name_id: u32,
  support_mask: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct FindContainerCacheKey {
  ctx_ptr: usize,
  node_id: usize,
  /// Interned container-name id; 0 represents "no name restriction".
  name_id: u32,
  support_mask: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct QueryEvalCacheKey {
  ctx_ptr: usize,
  container_id: usize,
  query_ptr: usize,
}

#[derive(Debug)]
struct ContainerQueryMemo {
  // Map from (query-container lookup key) -> selected container id (or None).
  find_container: HashMap<FindContainerCacheKey, Option<usize>>,
  query_eval: HashMap<QueryEvalCacheKey, bool>,
  condition_sigs: HashMap<usize, ConditionSignature>,
  // Intern container names to avoid storing strings in every cache key.
  name_ids: HashMap<String, u32>,
  next_name_id: u32,
  active_ctx_ptr: Option<usize>,
}

impl Default for ContainerQueryMemo {
  fn default() -> Self {
    Self {
      find_container: HashMap::new(),
      query_eval: HashMap::new(),
      condition_sigs: HashMap::new(),
      name_ids: HashMap::new(),
      next_name_id: 1,
      active_ctx_ptr: None,
    }
  }
}

impl ContainerQueryMemo {
  fn clear(&mut self) {
    self.find_container.clear();
    self.query_eval.clear();
    self.condition_sigs.clear();
    self.name_ids.clear();
    self.next_name_id = 1;
    self.active_ctx_ptr = None;
  }

  fn intern_name(&mut self, name: &str) -> u32 {
    if let Some(id) = self.name_ids.get(name) {
      *id
    } else {
      let id = self.next_name_id;
      self.next_name_id = self.next_name_id.saturating_add(1);
      self.name_ids.insert(name.to_string(), id);
      id
    }
  }
}

thread_local! {
  static CONTAINER_QUERY_MEMO: RefCell<ContainerQueryMemo> = RefCell::new(ContainerQueryMemo::default());
}

thread_local! {
  static CONTAINER_QUERY_GUARD: RefCell<Vec<usize>> = RefCell::new(Vec::new());
}

fn reset_container_query_memo() {
  CONTAINER_QUERY_MEMO.with(|memo| memo.borrow_mut().clear());
}

fn top_layer_kind(node: &DomNode) -> Option<TopLayerKind> {
  dialog_top_layer(node).or_else(|| popover_top_layer(node))
}

fn explicit_inert(node: &DomNode) -> bool {
  node.get_attribute_ref("inert").is_some() || attr_truthy(node, "data-fastr-inert")
}

fn node_is_inert(node: &DomNode, ancestors: &[&DomNode]) -> bool {
  if !node.is_element() {
    return false;
  }
  if explicit_inert(node) {
    return true;
  }
  ancestors.iter().any(|a| explicit_inert(a))
}

fn namespace_matches(node: &DomNode, namespace: &str) -> bool {
  match node.namespace() {
    Some(ns) => {
      if namespace.is_empty() {
        true
      } else if ns == namespace {
        true
      } else {
        ns.is_empty() && namespace == HTML_NAMESPACE
      }
    }
    None => false,
  }
}

pub fn cascade_profile_enabled() -> bool {
  ensure_cascade_profile_initialized();
  CASCADE_PROFILE_ENABLED.load(Ordering::Relaxed)
}

fn ensure_cascade_profile_initialized() {
  CASCADE_PROFILE_INITIALIZED.get_or_init(|| {
    let enabled = runtime::runtime_toggles().truthy("FASTR_CASCADE_PROFILE");
    CASCADE_PROFILE_ENABLED.store(enabled, Ordering::Relaxed);
  });
}

/// Enable or disable cascade profiling independent of the FASTR_CASCADE_PROFILE env var.
pub fn set_cascade_profile_enabled(enabled: bool) {
  ensure_cascade_profile_initialized();
  CASCADE_PROFILE_ENABLED.store(enabled, Ordering::Relaxed);
}

/// Snapshot of cascade profiling counters.
#[derive(Debug, Default, Clone, Copy)]
pub struct CascadeProfileStats {
  pub nodes: u64,
  pub rule_candidates: u64,
  pub rule_matches: u64,
  pub rule_candidates_pruned: u64,
  pub rule_candidates_by_id: u64,
  pub rule_candidates_by_class: u64,
  pub rule_candidates_by_tag: u64,
  pub rule_candidates_by_attr: u64,
  pub rule_candidates_universal: u64,
  pub selector_rightmost_fast_rejects: u64,
  pub selector_match_calls: u64,
  pub selector_bloom_fast_rejects: u64,
  pub selector_attempts_total: u64,
  pub selector_attempts_after_bloom: u64,
  pub selector_time_ns: u64,
  pub declaration_time_ns: u64,
  pub pseudo_time_ns: u64,
}

pub fn capture_cascade_profile() -> CascadeProfileStats {
  CascadeProfileStats {
    nodes: CASCADE_PROFILE_NODES.load(Ordering::Relaxed),
    rule_candidates: CASCADE_PROFILE_RULE_CANDIDATES.load(Ordering::Relaxed),
    rule_matches: CASCADE_PROFILE_RULE_MATCHES.load(Ordering::Relaxed),
    rule_candidates_pruned: CASCADE_PROFILE_RULE_PRUNED.load(Ordering::Relaxed),
    rule_candidates_by_id: CASCADE_PROFILE_CANDIDATES_BY_ID.load(Ordering::Relaxed),
    rule_candidates_by_class: CASCADE_PROFILE_CANDIDATES_BY_CLASS.load(Ordering::Relaxed),
    rule_candidates_by_tag: CASCADE_PROFILE_CANDIDATES_BY_TAG.load(Ordering::Relaxed),
    rule_candidates_by_attr: CASCADE_PROFILE_CANDIDATES_BY_ATTR.load(Ordering::Relaxed),
    rule_candidates_universal: CASCADE_PROFILE_CANDIDATES_UNIVERSAL.load(Ordering::Relaxed),
    selector_rightmost_fast_rejects: CASCADE_PROFILE_SELECTOR_RIGHTMOST_FAST_REJECTS
      .load(Ordering::Relaxed),
    selector_match_calls: CASCADE_PROFILE_SELECTOR_MATCH_CALLS.load(Ordering::Relaxed),
    selector_bloom_fast_rejects: CASCADE_PROFILE_SELECTOR_BLOOM_FAST_REJECTS
      .load(Ordering::Relaxed),
    selector_attempts_total: CASCADE_PROFILE_SELECTOR_ATTEMPTS_TOTAL.load(Ordering::Relaxed),
    selector_attempts_after_bloom: CASCADE_PROFILE_SELECTOR_ATTEMPTS_AFTER_BLOOM
      .load(Ordering::Relaxed),
    selector_time_ns: CASCADE_PROFILE_FIND_TIME_NS.load(Ordering::Relaxed),
    declaration_time_ns: CASCADE_PROFILE_DECL_TIME_NS.load(Ordering::Relaxed),
    pseudo_time_ns: CASCADE_PROFILE_PSEUDO_TIME_NS.load(Ordering::Relaxed),
  }
}

fn user_agent_stylesheet() -> &'static StyleSheet {
  UA_STYLESHEET
    .get_or_init(|| parse_stylesheet(USER_AGENT_STYLESHEET).unwrap_or_else(|_| StyleSheet::new()))
}

pub fn reset_cascade_profile() {
  CASCADE_PROFILE_NODES.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_RULE_CANDIDATES.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_RULE_MATCHES.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_RULE_PRUNED.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_CANDIDATES_BY_ID.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_CANDIDATES_BY_CLASS.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_CANDIDATES_BY_TAG.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_CANDIDATES_BY_ATTR.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_CANDIDATES_UNIVERSAL.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_SELECTOR_BLOOM_BUILT.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_SELECTOR_BLOOM_TIME_NS.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_SELECTOR_BLOOM_FAST_REJECTS.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_SELECTOR_ATTEMPTS_TOTAL.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_SELECTOR_ATTEMPTS_AFTER_BLOOM.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_SELECTOR_RIGHTMOST_FAST_REJECTS.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_SELECTOR_MATCH_CALLS.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_FIND_TIME_NS.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_DECL_TIME_NS.store(0, Ordering::Relaxed);
  CASCADE_PROFILE_PSEUDO_TIME_NS.store(0, Ordering::Relaxed);
}

fn log_cascade_profile(elapsed_ms: f64) {
  let nodes = CASCADE_PROFILE_NODES.load(Ordering::Relaxed);
  let candidates = CASCADE_PROFILE_RULE_CANDIDATES.load(Ordering::Relaxed);
  let matches = CASCADE_PROFILE_RULE_MATCHES.load(Ordering::Relaxed);
  let pruned = CASCADE_PROFILE_RULE_PRUNED.load(Ordering::Relaxed);
  let bloom_built = CASCADE_PROFILE_SELECTOR_BLOOM_BUILT.load(Ordering::Relaxed);
  let bloom_time_ns = CASCADE_PROFILE_SELECTOR_BLOOM_TIME_NS.load(Ordering::Relaxed);
  let rightmost_fast_rejects =
    CASCADE_PROFILE_SELECTOR_RIGHTMOST_FAST_REJECTS.load(Ordering::Relaxed);
  let match_calls = CASCADE_PROFILE_SELECTOR_MATCH_CALLS.load(Ordering::Relaxed);
  let selector_attempts_total = CASCADE_PROFILE_SELECTOR_ATTEMPTS_TOTAL.load(Ordering::Relaxed);
  let selector_attempts_after_bloom =
    CASCADE_PROFILE_SELECTOR_ATTEMPTS_AFTER_BLOOM.load(Ordering::Relaxed);
  let selector_bloom_fast_rejects =
    CASCADE_PROFILE_SELECTOR_BLOOM_FAST_REJECTS.load(Ordering::Relaxed);
  let find_time_ns = CASCADE_PROFILE_FIND_TIME_NS.load(Ordering::Relaxed);
  let decl_time_ns = CASCADE_PROFILE_DECL_TIME_NS.load(Ordering::Relaxed);
  let pseudo_time_ns = CASCADE_PROFILE_PSEUDO_TIME_NS.load(Ordering::Relaxed);
  let bloom_ms = bloom_time_ns as f64 / 1_000_000.0;
  let find_ms = find_time_ns as f64 / 1_000_000.0;
  let decl_ms = decl_time_ns as f64 / 1_000_000.0;
  let pseudo_ms = pseudo_time_ns as f64 / 1_000_000.0;
  let avg_candidates = if nodes > 0 {
    candidates as f64 / nodes as f64
  } else {
    0.0
  };
  let avg_matches = if nodes > 0 {
    matches as f64 / nodes as f64
  } else {
    0.0
  };
  let avg_selector_attempts = if nodes > 0 {
    selector_attempts_total as f64 / nodes as f64
  } else {
    0.0
  };
  let avg_selector_attempts_after_bloom = if nodes > 0 {
    selector_attempts_after_bloom as f64 / nodes as f64
  } else {
    0.0
  };
  let bloom_reject_pct = if selector_attempts_total > 0 {
    selector_bloom_fast_rejects as f64 * 100.0 / selector_attempts_total as f64
  } else {
    0.0
  };
  let after_bloom_pct = if selector_attempts_total > 0 {
    selector_attempts_after_bloom as f64 * 100.0 / selector_attempts_total as f64
  } else {
    0.0
  };
  eprintln!(
    "cascade profile: total_ms={:.2} nodes={} candidates={} pruned={} matches={} avg_candidates={:.1} avg_matches={:.1} rightmost_rejects={} match_calls={} selector_attempts={} after_bloom={} bloom_rejects={} bloom_reject_pct={:.1}% after_bloom_pct={:.1}% avg_selector_attempts={:.1} avg_after_bloom={:.1} bloom_built={} bloom_ms={:.2} find_ms={:.2} decl_ms={:.2} pseudo_ms={:.2}",
    elapsed_ms,
    nodes,
    candidates,
    pruned,
    matches,
    avg_candidates,
    avg_matches,
    rightmost_fast_rejects,
    match_calls,
    selector_attempts_total,
    selector_attempts_after_bloom,
    selector_bloom_fast_rejects,
    bloom_reject_pct,
    after_bloom_pct,
    avg_selector_attempts,
    avg_selector_attempts_after_bloom,
    bloom_built,
    bloom_ms,
    find_ms,
    decl_ms,
    pseudo_ms
  );
}

static UA_TEXTAREA_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static UA_INPUT_BASE_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static UA_INPUT_HIDDEN_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static UA_INPUT_TEXT_CURSOR_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static UA_INPUT_POINTER_CURSOR_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static UA_LINK_VISITED_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static UA_LINK_ACTIVE_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static UA_LINK_HOVER_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static UA_LINK_FOCUS_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static UA_BDI_LTR_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static UA_BDI_RTL_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static UA_BDO_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();

static PRESENTATIONAL_DIR_ISOLATE_LTR_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static PRESENTATIONAL_DIR_ISOLATE_RTL_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static PRESENTATIONAL_DIR_OVERRIDE_LTR_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static PRESENTATIONAL_DIR_OVERRIDE_RTL_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static PRESENTATIONAL_NOWRAP_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();
static PRESENTATIONAL_HIDDEN_DECLS: OnceLock<Vec<Declaration>> = OnceLock::new();

fn cached_declarations(
  cache: &'static OnceLock<Vec<Declaration>>,
  css: &'static str,
) -> Cow<'static, [Declaration]> {
  Cow::Borrowed(cache.get_or_init(|| parse_declarations(css)).as_slice())
}

fn select_color_scheme(
  pref: &ColorSchemePreference,
  user: ColorScheme,
) -> Option<ColorSchemeEntry> {
  match pref {
    ColorSchemePreference::Normal => None,
    ColorSchemePreference::Supported { schemes, .. } => {
      if schemes.is_empty() {
        return None;
      }

      if user != ColorScheme::NoPreference {
        if let Some(matched) = schemes.iter().find(|entry| match (entry, user) {
          (ColorSchemeEntry::Light, ColorScheme::Light) => true,
          (ColorSchemeEntry::Dark, ColorScheme::Dark) => true,
          _ => false,
        }) {
          return Some(matched.clone());
        }
      }

      schemes.first().cloned()
    }
  }
}

fn selector_has_nonempty_content(decls: &[Declaration]) -> bool {
  for decl in decls {
    if decl.property == "content" {
      // Treat any value other than 'none' or 'normal' as generating content (including empty strings)
      if let PropertyValue::Keyword(kw) = &decl.value {
        if kw.eq_ignore_ascii_case("none") || kw.eq_ignore_ascii_case("normal") {
          continue;
        }
      }
      return true;
    }
  }
  false
}

fn apply_color_scheme_palette(
  styles: &mut ComputedStyle,
  ua_styles: &mut ComputedStyle,
  ua_default_color: Rgba,
  ua_default_background: Rgba,
  selected_scheme: &Option<ColorSchemeEntry>,
  is_root: bool,
) {
  if !matches!(selected_scheme, Some(ColorSchemeEntry::Dark)) {
    return;
  }

  let dark_text = Rgba::rgb(232, 232, 232);
  let dark_background = Rgba::rgb(16, 16, 16);
  let dark_surface = Rgba::rgb(24, 24, 24);
  let dark_border = Rgba::rgb(96, 96, 96);
  let ua_border = Rgba::rgb(118, 118, 118);

  let apply = |style: &mut ComputedStyle| {
    if is_root && style.color == ua_default_color {
      style.color = dark_text;
    }
    if is_root && style.background_color == ua_default_background {
      style.background_color = dark_background;
    }
    let default_surface =
      style.background_color == ua_default_background && ua_default_background == Rgba::WHITE;
    if default_surface {
      style.background_color = dark_surface;
    }
    if default_surface
      && style.border_top_color == ua_border
      && style.border_right_color == ua_border
      && style.border_bottom_color == ua_border
      && style.border_left_color == ua_border
    {
      style.border_top_color = dark_border;
      style.border_right_color = dark_border;
      style.border_bottom_color = dark_border;
      style.border_left_color = dark_border;
    }
    if let OutlineColor::Color(color) = style.outline_color {
      if color == Rgba::BLACK {
        style.outline_color = OutlineColor::Color(dark_text);
      }
    }
  };

  apply(styles);
  apply(ua_styles);
}

fn collect_shadow_stylesheets(
  root: &DomNode,
  ids: &HashMap<*const DomNode, usize>,
) -> Result<HashMap<usize, StyleSheet>, RenderError> {
  fn gather_styles(node: &DomNode, root_ptr: *const DomNode, out: &mut String) {
    if let DomNodeType::ShadowRoot { .. } = node.node_type {
      if (node as *const DomNode) != root_ptr {
        return;
      }
    }
    if let Some(tag) = node.tag_name() {
      if tag.eq_ignore_ascii_case("style") {
        for child in node.children.iter() {
          if let Some(text) = child.text_content() {
            out.push_str(text);
            out.push('\n');
          }
        }
      }
    }
    for child in node.children.iter() {
      gather_styles(child, root_ptr, out);
    }
  }

  fn walk(
    node: &DomNode,
    ids: &HashMap<*const DomNode, usize>,
    out: &mut HashMap<usize, StyleSheet>,
  ) -> Result<(), RenderError> {
    if matches!(node.node_type, DomNodeType::ShadowRoot { .. }) {
      let mut css = String::new();
      gather_styles(node, node as *const DomNode, &mut css);
      if !css.is_empty() {
        let sheet = match parse_stylesheet(&css) {
          Ok(sheet) => sheet,
          Err(Error::Render(err)) => return Err(err),
          Err(_) => StyleSheet::new(),
        };
        if let Some(id) = ids.get(&(node as *const DomNode)) {
          out.insert(*id, sheet);
        }
      }
    }
    for child in node.children.iter() {
      walk(child, ids, out)?;
    }
    Ok(())
  }

  let mut out = HashMap::new();
  walk(root, ids, &mut out)?;
  Ok(out)
}

fn relative_selectors_need_bloom_summaries(
  selectors: &[selectors::parser::RelativeSelector<FastRenderSelectorImpl>],
  quirks_mode: QuirksMode,
) -> bool {
  selectors.iter().any(|selector| {
    selector.match_hint.is_descendant_direction()
      && !selector
        .bloom_hashes
        .hashes_for_mode(quirks_mode)
        .is_empty()
  })
}

fn selector_needs_bloom_summaries_for_has_relative(
  selector: &Selector<FastRenderSelectorImpl>,
  quirks_mode: QuirksMode,
) -> bool {
  use selectors::parser::Component;

  for component in selector.iter_raw_match_order() {
    match component {
      Component::Has(list) => {
        if relative_selectors_need_bloom_summaries(&list[..], quirks_mode) {
          return true;
        }
      }
      Component::NonTSPseudoClass(pc) => match pc {
        PseudoClass::Has(list) => {
          if relative_selectors_need_bloom_summaries(&list[..], quirks_mode) {
            return true;
          }
        }
        PseudoClass::Host(Some(list)) | PseudoClass::HostContext(list) => {
          if selector_list_needs_bloom_summaries_for_has_relative(list, quirks_mode) {
            return true;
          }
        }
        PseudoClass::NthChild(_, _, Some(list)) | PseudoClass::NthLastChild(_, _, Some(list)) => {
          if selector_list_needs_bloom_summaries_for_has_relative(list, quirks_mode) {
            return true;
          }
        }
        _ => {}
      },
      Component::Is(list) | Component::Where(list) | Component::Negation(list) => {
        if selector_list_needs_bloom_summaries_for_has_relative(list, quirks_mode) {
          return true;
        }
      }
      Component::Slotted(inner) => {
        if selector_needs_bloom_summaries_for_has_relative(inner, quirks_mode) {
          return true;
        }
      }
      Component::Host(Some(inner)) => {
        if selector_needs_bloom_summaries_for_has_relative(inner, quirks_mode) {
          return true;
        }
      }
      Component::PseudoElement(pseudo) => {
        if let PseudoElement::Slotted(list) = pseudo {
          if list
            .iter()
            .any(|selector| selector_needs_bloom_summaries_for_has_relative(selector, quirks_mode))
          {
            return true;
          }
        }
      }
      _ => {}
    }
  }

  false
}

fn selector_list_needs_bloom_summaries_for_has_relative(
  list: &SelectorList<FastRenderSelectorImpl>,
  quirks_mode: QuirksMode,
) -> bool {
  list
    .slice()
    .iter()
    .any(|selector| selector_needs_bloom_summaries_for_has_relative(selector, quirks_mode))
}

fn rule_index_needs_bloom_summaries_for_has_relative<'a>(
  index: &RuleIndex<'a>,
  quirks_mode: QuirksMode,
) -> bool {
  index
    .selectors
    .iter()
    .any(|sel| selector_needs_bloom_summaries_for_has_relative(sel.selector, quirks_mode))
    || index
      .pseudo_selectors
      .iter()
      .any(|sel| selector_needs_bloom_summaries_for_has_relative(sel.selector, quirks_mode))
    || index.slotted_selectors.iter().any(|sel| {
      selector_needs_bloom_summaries_for_has_relative(sel.selector, quirks_mode)
        || sel.prelude.as_ref().is_some_and(|prelude| {
          selector_needs_bloom_summaries_for_has_relative(prelude, quirks_mode)
        })
    })
}

fn rule_scopes_needs_selector_bloom_summaries<'a>(scopes: &RuleScopes<'a>) -> bool {
  let quirks_mode = scopes.quirks_mode;

  // Fast path: in standards mode (and limited quirks), `:has()` relative selectors can emit bloom
  // hashes for classes / IDs, so we track whether any rule contains prunable `:has()` requirements
  // while building the `RuleIndex`. If present, selector bloom summaries are useful for both rule
  // candidate pruning and `:has()` matching, so we can return early.
  //
  // In full quirks mode, the selector engine intentionally omits class / ID bloom hashes (to avoid
  // incorrect pruning under case-folded matching). As a result, `RuleIndex::has_has_requirements`
  // can be true even though quirks-mode pruning would be a no-op. For quirks documents we fall back
  // to scanning for `:has()` relative selectors whose *quirks-mode* bloom hashes are non-empty.
  if !matches!(quirks_mode, QuirksMode::Quirks) {
    let needs_for_index = |index: &RuleIndex<'a>| index.has_has_requirements;
    if needs_for_index(&scopes.ua)
      || needs_for_index(&scopes.document)
      || scopes.shadows.values().any(needs_for_index)
      || scopes.host_rules.values().any(needs_for_index)
    {
      return true;
    }
  }

  rule_index_needs_bloom_summaries_for_has_relative(&scopes.ua, quirks_mode)
    || rule_index_needs_bloom_summaries_for_has_relative(&scopes.document, quirks_mode)
    || scopes
      .shadows
      .values()
      .any(|idx| rule_index_needs_bloom_summaries_for_has_relative(idx, quirks_mode))
    || scopes
      .host_rules
      .values()
      .any(|idx| rule_index_needs_bloom_summaries_for_has_relative(idx, quirks_mode))
}

fn record_node_visit(node: &DomNode) {
  if cascade_profile_enabled() && node.is_element() {
    CASCADE_PROFILE_NODES.fetch_add(1, Ordering::Relaxed);
  }
}

fn record_matching_stats(
  candidates: usize,
  matches: usize,
  pruned: usize,
  stats: &CandidateStats,
  elapsed: Option<std::time::Duration>,
) {
  if !cascade_profile_enabled() {
    return;
  }
  CASCADE_PROFILE_RULE_CANDIDATES.fetch_add(candidates as u64, Ordering::Relaxed);
  CASCADE_PROFILE_RULE_MATCHES.fetch_add(matches as u64, Ordering::Relaxed);
  CASCADE_PROFILE_RULE_PRUNED.fetch_add(pruned as u64, Ordering::Relaxed);
  CASCADE_PROFILE_CANDIDATES_BY_ID.fetch_add(stats.by_id, Ordering::Relaxed);
  CASCADE_PROFILE_CANDIDATES_BY_CLASS.fetch_add(stats.by_class, Ordering::Relaxed);
  CASCADE_PROFILE_CANDIDATES_BY_TAG.fetch_add(stats.by_tag, Ordering::Relaxed);
  CASCADE_PROFILE_CANDIDATES_BY_ATTR.fetch_add(stats.by_attr, Ordering::Relaxed);
  CASCADE_PROFILE_CANDIDATES_UNIVERSAL.fetch_add(stats.universal, Ordering::Relaxed);
  if let Some(dur) = elapsed {
    CASCADE_PROFILE_FIND_TIME_NS.fetch_add(dur.as_nanos() as u64, Ordering::Relaxed);
  }
}

fn record_declaration_time(elapsed: std::time::Duration) {
  if cascade_profile_enabled() {
    CASCADE_PROFILE_DECL_TIME_NS.fetch_add(elapsed.as_nanos() as u64, Ordering::Relaxed);
  }
}

fn record_pseudo_time(elapsed: std::time::Duration) {
  if cascade_profile_enabled() {
    CASCADE_PROFILE_PSEUDO_TIME_NS.fetch_add(elapsed.as_nanos() as u64, Ordering::Relaxed);
  }
}

fn pack_ancestor_bloom_hashes(hashes: &[u32]) -> AncestorHashes {
  debug_assert!(hashes.len() <= 4);
  let mut packed = [0u32; 3];
  packed[0] = *hashes.get(0).unwrap_or(&0);
  packed[1] = *hashes.get(1).unwrap_or(&0);
  packed[2] = *hashes.get(2).unwrap_or(&0);
  if hashes.len() == 4 {
    // Pack the fourth hash into the upper byte of each of the first three hashes, matching the
    // layout used by `selectors::parser::AncestorHashes`.
    let fourth = hashes[3];
    packed[0] |= (fourth & 0x000000ff) << 24;
    packed[1] |= (fourth & 0x0000ff00) << 16;
    packed[2] |= (fourth & 0x00ff0000) << 8;
  }
  AncestorHashes {
    packed_hashes: packed,
  }
}

fn cascade_ancestor_hashes(
  selector: &Selector<FastRenderSelectorImpl>,
  quirks_mode: QuirksMode,
) -> AncestorHashes {
  use precomputed_hash::PrecomputedHash;
  use selectors::bloom::BLOOM_HASH_MASK;
  use selectors::parser::Combinator;
  use selectors::parser::Component;

  fn for_each_hash<F: FnMut(u32)>(
    selector: &Selector<FastRenderSelectorImpl>,
    quirks_mode: QuirksMode,
    add: &mut F,
  ) {
    let mut iter = selector.iter();

    // Skip the rightmost (subject) sequence and any sibling-only sequences until we reach the
    // first ancestor combinator (child/descendant). This matches the logic used by
    // `selectors::parser::AncestorHashes`.
    loop {
      while iter.next().is_some() {}
      match iter.next_sequence() {
        Some(combinator) if matches!(combinator, Combinator::Child | Combinator::Descendant) => {
          break
        }
        Some(_) => continue,
        None => return,
      }
    }

    loop {
      while let Some(component) = iter.next() {
        match component {
          Component::LocalName(local) => {
            if local.name != local.lower_name {
              continue;
            }
            add(local.name.precomputed_hash() & BLOOM_HASH_MASK);
          }
          Component::DefaultNamespace(url) | Component::Namespace(_, url) => {
            add(url.precomputed_hash() & BLOOM_HASH_MASK);
          }
          Component::ID(id) if quirks_mode != QuirksMode::Quirks => {
            add(id.precomputed_hash() & BLOOM_HASH_MASK);
          }
          Component::Class(class) if quirks_mode != QuirksMode::Quirks => {
            add(class.precomputed_hash() & BLOOM_HASH_MASK);
          }
          Component::AttributeInNoNamespace { local_name, .. } => {
            if <FastRenderSelectorImpl as selectors::parser::SelectorImpl>::should_collect_attr_hash(
              local_name,
            ) {
              add(local_name.precomputed_hash() & BLOOM_HASH_MASK);
            }
          }
          Component::AttributeInNoNamespaceExists {
            local_name,
            local_name_lower,
            ..
          } => {
            if local_name == local_name_lower
              && <FastRenderSelectorImpl as selectors::parser::SelectorImpl>::should_collect_attr_hash(
                local_name,
              )
            {
              add(local_name.precomputed_hash() & BLOOM_HASH_MASK);
            }
          }
          Component::AttributeOther(attr) => {
            if attr.local_name == attr.local_name_lower
              && <FastRenderSelectorImpl as selectors::parser::SelectorImpl>::should_collect_attr_hash(
                &attr.local_name,
              )
            {
              add(attr.local_name.precomputed_hash() & BLOOM_HASH_MASK);
            }
          }
          Component::Is(list) | Component::Where(list) => {
            // :is() and :where() are OR selectors. Only descend when there is exactly one selector
            // so the nested selector doesn't introduce additional optional branches.
            let slice = list.slice();
            if slice.len() == 1 {
              for_each_hash(&slice[0], quirks_mode, add);
            }
          }
          _ => {}
        }
      }

      let Some(combinator) = iter.next_sequence() else {
        break;
      };
      if !matches!(combinator, Combinator::Child | Combinator::Descendant) {
        // Skip sequences connected via non-ancestor combinators (siblings, pseudo-elements, slot
        // assignments, etc) until we reach the next ancestor combinator.
        loop {
          while iter.next().is_some() {}
          match iter.next_sequence() {
            Some(combinator)
              if matches!(combinator, Combinator::Child | Combinator::Descendant) =>
            {
              break;
            }
            Some(_) => continue,
            None => return,
          }
        }
      }
    }
  }

  // The upstream `AncestorHashes` stores at most four hashes. For long descendant chains this can
  // miss far-left ancestor requirements (e.g. shadow-boundary "outer" selectors) and reduce bloom
  // fast-reject effectiveness. Keep the first three (closest) hashes and use the farthest hash as
  // the fourth when more than four are available.
  let mut first_three = [0u32; 3];
  let mut first_len = 0usize;
  let mut total = 0usize;
  let mut farthest = 0u32;

  for_each_hash(selector, quirks_mode, &mut |hash| {
    total = total.saturating_add(1);
    if first_len < 3 {
      first_three[first_len] = hash;
      first_len += 1;
    }
    farthest = hash;
  });

  match total {
    0 => pack_ancestor_bloom_hashes(&[]),
    1..=3 => pack_ancestor_bloom_hashes(&first_three[..total]),
    _ => pack_ancestor_bloom_hashes(&[first_three[0], first_three[1], first_three[2], farthest]),
  }
}

#[inline(always)]
fn matches_selector_cascade(
  selector: &Selector<FastRenderSelectorImpl>,
  hashes: Option<&AncestorHashes>,
  element: &ElementRef,
  context: &mut MatchingContext<FastRenderSelectorImpl>,
) -> bool {
  // Extremely hot path: avoid the `OnceLock` initialization dance and extra
  // branches when profiling is disabled.
  if !CASCADE_PROFILE_ENABLED.load(Ordering::Relaxed) {
    let computed_hashes;
    let hashes = if hashes.is_some() || context.bloom_filter.is_none() {
      hashes
    } else {
      computed_hashes = cascade_ancestor_hashes(selector, context.quirks_mode());
      Some(&computed_hashes)
    };

    if let (Some(filter), Some(hashes)) = (context.bloom_filter, hashes) {
      if !selector_may_match(hashes, filter) {
        return false;
      }
    }

    // We've already applied the bloom fast-reject above (if enabled), so avoid
    // repeating it inside `matches_selector`.
    return matches_selector(selector, 0, None, element, context);
  }

  CASCADE_PROFILE_SELECTOR_ATTEMPTS_TOTAL.fetch_add(1, Ordering::Relaxed);

  let computed_hashes;
  let hashes = if hashes.is_some() || context.bloom_filter.is_none() {
    hashes
  } else {
    computed_hashes = cascade_ancestor_hashes(selector, context.quirks_mode());
    Some(&computed_hashes)
  };

  if let (Some(filter), Some(hashes)) = (context.bloom_filter, hashes) {
    if !selector_may_match(hashes, filter) {
      CASCADE_PROFILE_SELECTOR_BLOOM_FAST_REJECTS.fetch_add(1, Ordering::Relaxed);
      return false;
    }
  }

  CASCADE_PROFILE_SELECTOR_ATTEMPTS_AFTER_BLOOM.fetch_add(1, Ordering::Relaxed);

  matches_selector(selector, 0, None, element, context)
}

#[inline(always)]
fn matches_selector_cascade_counted(
  selector: &Selector<FastRenderSelectorImpl>,
  hashes: Option<&AncestorHashes>,
  element: &ElementRef,
  context: &mut MatchingContext<FastRenderSelectorImpl>,
) -> bool {
  if CASCADE_PROFILE_ENABLED.load(Ordering::Relaxed) {
    CASCADE_PROFILE_SELECTOR_MATCH_CALLS.fetch_add(1, Ordering::Relaxed);
  }
  matches_selector_cascade(selector, hashes, element, context)
}

#[inline(always)]
fn record_rightmost_fast_reject_prune() {
  if CASCADE_PROFILE_ENABLED.load(Ordering::Relaxed) {
    CASCADE_PROFILE_SELECTOR_RIGHTMOST_FAST_REJECTS.fetch_add(1, Ordering::Relaxed);
  }
}

/// The origin of a style rule for cascade ordering.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum StyleOrigin {
  UserAgent,
  Author,
  Inline,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum RuleScope {
  Document,
  Shadow { shadow_root_id: usize },
}

impl StyleOrigin {
  fn rank(self) -> u8 {
    match self {
      StyleOrigin::UserAgent => 0,
      StyleOrigin::Author | StyleOrigin::Inline => 1,
    }
  }
}

/// A style rule annotated with its origin and document order.
#[derive(Clone)]
struct CascadeRule<'a> {
  origin: StyleOrigin,
  order: usize,
  rule: &'a StyleRule,
  layer_order: Arc<[u32]>,
  container_conditions: Vec<ContainerCondition>,
  scopes: Vec<ScopeContext<'a>>,
  scope_signature: ScopeSignature,
  scope: RuleScope,
  starting_style: bool,
}

#[derive(Debug)]
struct DomMaps {
  id_map: HashMap<*const DomNode, usize>,
  id_to_node: HashMap<usize, *const DomNode>,
  parent_map: HashMap<usize, usize>,
  shadow_hosts: HashMap<usize, usize>,
  exportparts_map: HashMap<usize, Vec<(String, String)>>,
  selector_blooms: Option<SelectorBloomStore>,
  selector_keys: DomSelectorKeyCache,
}

const SELECTOR_BLOOM_MIN_NODES: usize = 32;

#[derive(Debug, Clone, Copy, Default)]
struct DomSelectorKeyEntry {
  tag_key: SelectorBucketKey,
  id_key: SelectorBucketKey,
  has_id: bool,
  class_start: u32,
  class_len: u32,
  attr_start: u32,
  attr_len: u32,
}

#[derive(Debug, Default)]
struct DomSelectorKeyCache {
  nodes: Vec<DomSelectorKeyEntry>,
  class_keys: Vec<SelectorBucketKey>,
  attr_keys: Vec<SelectorBucketKey>,
}

#[derive(Clone, Copy)]
struct NodeSelectorKeys<'a> {
  tag_key: SelectorBucketKey,
  id_key: Option<SelectorBucketKey>,
  class_keys: &'a [SelectorBucketKey],
  attr_keys: &'a [SelectorBucketKey],
}

impl DomSelectorKeyCache {
  fn new(node_count: usize) -> Self {
    // Node ids are 1-indexed; leave slot 0 empty.
    let mut nodes = Vec::with_capacity(node_count + 1);
    nodes.resize(node_count + 1, DomSelectorKeyEntry::default());
    Self {
      nodes,
      class_keys: Vec::new(),
      attr_keys: Vec::new(),
    }
  }

  fn set_node_keys(&mut self, node_id: usize, entry: DomSelectorKeyEntry) {
    if node_id >= self.nodes.len() {
      self
        .nodes
        .resize(node_id + 1, DomSelectorKeyEntry::default());
    }
    self.nodes[node_id] = entry;
  }

  fn node_keys(&self, node_id: usize) -> NodeSelectorKeys<'_> {
    let entry = self.nodes.get(node_id).copied().unwrap_or_default();
    let class_start = entry.class_start as usize;
    let class_end = class_start + entry.class_len as usize;
    let attr_start = entry.attr_start as usize;
    let attr_end = attr_start + entry.attr_len as usize;
    NodeSelectorKeys {
      tag_key: entry.tag_key,
      id_key: entry.has_id.then_some(entry.id_key),
      class_keys: self.class_keys.get(class_start..class_end).unwrap_or(&[]),
      attr_keys: self.attr_keys.get(attr_start..attr_end).unwrap_or(&[]),
    }
  }
}

impl DomMaps {
  fn new(root: &DomNode, id_map: HashMap<*const DomNode, usize>) -> Self {
    let mut id_to_node: HashMap<usize, *const DomNode> = HashMap::new();
    for (ptr, id) in &id_map {
      id_to_node.insert(*id, *ptr);
    }

    let mut parent_map: HashMap<usize, usize> = HashMap::new();
    let mut shadow_hosts: HashMap<usize, usize> = HashMap::new();
    let mut exportparts_map: HashMap<usize, Vec<(String, String)>> = HashMap::new();
    let mut selector_keys = DomSelectorKeyCache::new(id_map.len());

    fn walk(
      node: &DomNode,
      parent: Option<usize>,
      ids: &HashMap<*const DomNode, usize>,
      parent_map: &mut HashMap<usize, usize>,
      shadow_hosts: &mut HashMap<usize, usize>,
      exportparts_map: &mut HashMap<usize, Vec<(String, String)>>,
      selector_keys: &mut DomSelectorKeyCache,
    ) {
      let id = ids.get(&(node as *const DomNode)).copied().unwrap_or(0);
      if let Some(p) = parent {
        parent_map.insert(id, p);
        if matches!(node.node_type, DomNodeType::ShadowRoot { .. }) {
          shadow_hosts.insert(id, p);
        }
      }
      if let Some(mapping) = node.get_attribute_ref("exportparts").map(parse_exportparts) {
        if !mapping.is_empty() {
          exportparts_map.insert(id, mapping);
        }
      }

      if node.is_element() {
        let tag_key = node.tag_name().map(selector_bucket_tag).unwrap_or_default();
        let (id_key, has_id) = node
          .get_attribute_ref("id")
          .map(|value| (selector_bucket_id(value), true))
          .unwrap_or((SelectorBucketKey::default(), false));

        let class_start = selector_keys.class_keys.len();
        if let Some(class_attr) = node.get_attribute_ref("class") {
          for cls in class_attr.split_ascii_whitespace() {
            selector_keys.class_keys.push(selector_bucket_class(cls));
          }
        }
        let class_len = selector_keys.class_keys.len() - class_start;

        let attr_start = selector_keys.attr_keys.len();
        for (name, _) in node.attributes_iter() {
          selector_keys.attr_keys.push(selector_bucket_attr(name));
        }
        let attr_len = selector_keys.attr_keys.len() - attr_start;

        selector_keys.set_node_keys(
          id,
          DomSelectorKeyEntry {
            tag_key,
            id_key,
            has_id,
            class_start: u32::try_from(class_start).expect("selector key cache overflow"),
            class_len: u32::try_from(class_len).expect("selector key cache overflow"),
            attr_start: u32::try_from(attr_start).expect("selector key cache overflow"),
            attr_len: u32::try_from(attr_len).expect("selector key cache overflow"),
          },
        );
      }
      for child in node.children.iter() {
        walk(
          child,
          Some(id),
          ids,
          parent_map,
          shadow_hosts,
          exportparts_map,
          selector_keys,
        );
      }
    }

    walk(
      root,
      None,
      &id_map,
      &mut parent_map,
      &mut shadow_hosts,
      &mut exportparts_map,
      &mut selector_keys,
    );

    Self {
      id_map,
      id_to_node,
      parent_map,
      shadow_hosts,
      exportparts_map,
      selector_blooms: None,
      selector_keys,
    }
  }

  fn ensure_selector_blooms(&mut self, root: &DomNode) {
    if self.selector_blooms.is_some() {
      return;
    }
    let profiling = cascade_profile_enabled();
    let start = profiling.then(Instant::now);
    let blooms = build_selector_bloom_store(root, &self.id_map);
    if let Some(start) = start {
      let elapsed = start.elapsed();
      CASCADE_PROFILE_SELECTOR_BLOOM_TIME_NS
        .fetch_add(elapsed.as_nanos() as u64, Ordering::Relaxed);
      if blooms.is_some() {
        CASCADE_PROFILE_SELECTOR_BLOOM_BUILT.fetch_add(1, Ordering::Relaxed);
      }
    }
    self.selector_blooms = blooms;
  }

  fn ancestors_for(&self, node_id: usize) -> Vec<&DomNode> {
    let mut ids: Vec<usize> = Vec::new();
    let mut current = node_id;
    while let Some(parent) = self.parent_map.get(&current).copied() {
      ids.push(parent);
      current = parent;
    }
    ids
      .into_iter()
      .rev()
      .filter_map(|id| self.id_to_node.get(&id).map(|ptr| unsafe { &**ptr }))
      .collect()
  }

  fn containing_shadow_root(&self, node_id: usize) -> Option<usize> {
    let mut current = node_id;
    while let Some(parent) = self.parent_map.get(&current).copied() {
      if let Some(ptr) = self.id_to_node.get(&parent) {
        let node = unsafe { &**ptr };
        if matches!(node.node_type, DomNodeType::ShadowRoot { .. }) {
          return Some(parent);
        }
      }
      current = parent;
    }
    None
  }

  fn exportparts_for(&self, node: &DomNode) -> Option<&Vec<(String, String)>> {
    let id = self.id_map.get(&(node as *const DomNode))?;
    self.exportparts_map.get(id)
  }

  fn selector_blooms(&self) -> Option<&SelectorBloomStore> {
    self.selector_blooms.as_ref()
  }

  fn selector_keys(&self, node_id: usize) -> NodeSelectorKeys<'_> {
    self.selector_keys.node_keys(node_id)
  }
}

fn build_slot_maps<'a>(
  _root: &'a DomNode,
  assignment: &SlotAssignment,
  dom_maps: &DomMaps,
) -> HashMap<usize, SlotAssignmentMap<'a>> {
  let mut maps: HashMap<usize, SlotAssignmentMap<'a>> = HashMap::new();

  for (&slot_id, nodes) in assignment.slot_to_nodes.iter() {
    if nodes.is_empty() {
      continue;
    }
    let Some(slot_ptr) = dom_maps.id_to_node.get(&slot_id) else {
      continue;
    };
    let Some(first_node) = nodes.first() else {
      continue;
    };
    let Some(info) = assignment.node_to_slot.get(first_node) else {
      continue;
    };
    let shadow_root_id = info.shadow_root_id;
    // Safety: DOM nodes are immutable during cascade; pointers come from the parsed tree.
    // Use raw pointers to decouple these references from the borrow of `dom_maps` so the slot
    // maps can be stored inside a long-lived `PreparedCascade`.
    let slot_node: &'a DomNode = unsafe { &*(*slot_ptr) };
    let ancestors: Vec<&'a DomNode> = {
      let mut ids: Vec<usize> = Vec::new();
      let mut current = slot_id;
      while let Some(parent) = dom_maps.parent_map.get(&current).copied() {
        ids.push(parent);
        current = parent;
      }
      ids
        .into_iter()
        .rev()
        .filter_map(|id| dom_maps.id_to_node.get(&id).copied())
        .map(|ptr| unsafe { &*ptr })
        .collect()
    };
    let assigned_nodes: Vec<&'a DomNode> = nodes
      .iter()
      .filter_map(|id| dom_maps.id_to_node.get(id).copied())
      .map(|ptr| unsafe { &*ptr })
      .collect();
    if assigned_nodes.is_empty() {
      continue;
    }
    let map = maps
      .entry(shadow_root_id)
      .or_insert_with(|| SlotAssignmentMap::new(&dom_maps.id_map, &dom_maps.id_to_node));
    map.add_slot(slot_node, ancestors, assigned_nodes, shadow_root_id);
  }

  maps
}

// Tree scopes participate in cascade-layer ordering (outer document vs inner shadow trees).
//
// The document scope should be the lowest-precedence tree scope so that shadow-root author rules
// (including ::slotted()) can override document author rules when they target the same element.
// This matches browser behavior and is covered by the shadow ::slotted regression tests.
const DOCUMENT_TREE_SCOPE_PREFIX: u32 = 0;

fn shadow_tree_scope_prefix(shadow_root_id: usize) -> u32 {
  u32::try_from(shadow_root_id)
    .unwrap_or(u32::MAX)
    .saturating_add(1)
}

static DOCUMENT_UNLAYERED_LAYER_ORDER: OnceLock<Arc<[u32]>> = OnceLock::new();
static UNPREFIXED_UNLAYERED_LAYER_ORDER: OnceLock<Arc<[u32]>> = OnceLock::new();

const LAYER_ORDER_INTERN_STACK_LEN: usize = 16;

#[derive(Default)]
struct LayerOrderInterner {
  interned: FxHashSet<Arc<[u32]>>,
}

impl LayerOrderInterner {
  fn intern_prefixed(&mut self, layer_order: &[u32], tree_scope_prefix: u32) -> Arc<[u32]> {
    if tree_scope_prefix == DOCUMENT_TREE_SCOPE_PREFIX
      && layer_order.len() == 1
      && layer_order[0] == u32::MAX
    {
      return document_unlayered_layer_order();
    }

    let total_len = layer_order.len().saturating_add(1);
    if total_len <= LAYER_ORDER_INTERN_STACK_LEN {
      let mut buf = [0u32; LAYER_ORDER_INTERN_STACK_LEN];
      buf[0] = tree_scope_prefix;
      buf[1..total_len].copy_from_slice(layer_order);
      let slice = &buf[..total_len];
      if let Some(hit) = self.interned.get(slice) {
        return hit.clone();
      }
      let arc: Arc<[u32]> = Arc::from(slice);
      self.interned.insert(arc.clone());
      return arc;
    }

    let mut prefixed = Vec::with_capacity(total_len);
    prefixed.push(tree_scope_prefix);
    prefixed.extend_from_slice(layer_order);
    if let Some(hit) = self.interned.get(prefixed.as_slice()) {
      return hit.clone();
    }
    let arc: Arc<[u32]> = Arc::from(prefixed);
    self.interned.insert(arc.clone());
    arc
  }
}

fn document_unlayered_layer_order() -> Arc<[u32]> {
  DOCUMENT_UNLAYERED_LAYER_ORDER
    .get_or_init(|| Arc::from(vec![DOCUMENT_TREE_SCOPE_PREFIX, u32::MAX]))
    .clone()
}

fn unprefixed_unlayered_layer_order() -> Arc<[u32]> {
  UNPREFIXED_UNLAYERED_LAYER_ORDER
    .get_or_init(|| Arc::from(vec![u32::MAX]))
    .clone()
}

fn layer_order_with_tree_scope(layer_order: &[u32], tree_scope_prefix: u32) -> Arc<[u32]> {
  if tree_scope_prefix == DOCUMENT_TREE_SCOPE_PREFIX
    && layer_order.len() == 1
    && layer_order[0] == u32::MAX
  {
    return document_unlayered_layer_order();
  }
  let mut prefixed = Vec::with_capacity(layer_order.len().saturating_add(1));
  prefixed.push(tree_scope_prefix);
  prefixed.extend_from_slice(layer_order);
  Arc::from(prefixed)
}

fn tree_scope_prefix_for_node(dom_maps: &DomMaps, node_id: usize) -> u32 {
  dom_maps
    .containing_shadow_root(node_id)
    .map(shadow_tree_scope_prefix)
    .unwrap_or(DOCUMENT_TREE_SCOPE_PREFIX)
}

#[derive(Clone, Default)]
struct HasRequirement {
  non_quirks_hashes: Vec<u32>,
  quirks_hashes: Vec<u32>,
}

impl HasRequirement {
  fn matches_summary(&self, summary: SelectorBloomSummaryRef<'_>, quirks_mode: QuirksMode) -> bool {
    let hashes = if matches!(quirks_mode, QuirksMode::Quirks) {
      &self.quirks_hashes
    } else {
      &self.non_quirks_hashes
    };
    hashes.is_empty() || hashes.iter().all(|hash| summary.contains_hash(*hash))
  }

  fn is_empty(&self) -> bool {
    self.non_quirks_hashes.is_empty() && self.quirks_hashes.is_empty()
  }
}

#[derive(Clone, Default)]
struct SelectorMetadata {
  namespace: Option<String>,
  has_requirement_groups: Vec<Vec<HasRequirement>>,
}

fn selector_metadata_matches_node(
  metadata: &SelectorMetadata,
  node: &DomNode,
  summary: Option<SelectorBloomSummaryRef<'_>>,
  quirks_mode: QuirksMode,
) -> bool {
  if let Some(ns) = metadata.namespace.as_deref() {
    if !namespace_matches(node, ns) {
      return false;
    }
  }
  if let Some(summary) = summary {
    for group in metadata.has_requirement_groups.iter() {
      if group.is_empty() {
        continue;
      }
      if !group
        .iter()
        .any(|req| req.matches_summary(summary, quirks_mode))
      {
        return false;
      }
    }
  }
  true
}

// Fast reject descriptor for the selector's rightmost compound. This is used to cheaply prune
// false-positive candidates from union-bucket selection (e.g. `.a.b` showing up for any `.a`
// element).
//
// The descriptor is intentionally conservative: it only encodes mandatory requirements that can
// be checked using prehashed `NodeSelectorKeys` without consulting strings or running the full
// selector matcher.
const RIGHTMOST_FAST_REJECT_MAX_KEYS: usize = 4;

#[derive(Clone, Copy, Debug)]
struct RightmostFastReject {
  tag_key: SelectorBucketKey,
  id_key: SelectorBucketKey,
  // First `class_len` entries are required classes; the following `attr_len` entries are required
  // attribute names.
  keys: [SelectorBucketKey; RIGHTMOST_FAST_REJECT_MAX_KEYS],
  class_len: u8,
  attr_len: u8,
  flags: u8,
}

impl RightmostFastReject {
  const FLAG_HAS_TAG: u8 = 1 << 0;
  const FLAG_HAS_ID: u8 = 1 << 1;

  #[inline]
  fn has_tag(self) -> bool {
    (self.flags & Self::FLAG_HAS_TAG) != 0
  }

  #[inline]
  fn has_id(self) -> bool {
    (self.flags & Self::FLAG_HAS_ID) != 0
  }

  #[inline]
  fn matches(self, node_keys: NodeSelectorKeys<'_>) -> bool {
    if self.has_tag() && node_keys.tag_key != self.tag_key {
      return false;
    }
    if self.has_id() && node_keys.id_key != Some(self.id_key) {
      return false;
    }

    let class_len = self.class_len as usize;
    for required in &self.keys[..class_len] {
      if !node_keys.class_keys.iter().any(|key| key == required) {
        return false;
      }
    }
    let attr_end = class_len + self.attr_len as usize;
    for required in &self.keys[class_len..attr_end] {
      if !node_keys.attr_keys.iter().any(|key| key == required) {
        return false;
      }
    }

    true
  }
}

/// Simple index over the rightmost compound selector to prune rule matching.
struct IndexedSelector<'a> {
  rule_idx: usize,
  selector: &'a Selector<crate::css::selectors::FastRenderSelectorImpl>,
  specificity: u32,
  ancestor_hashes: AncestorHashes,
  metadata: SelectorMetadata,
  fast_reject: u32,
}

struct IndexedSlottedSelector<'a> {
  rule_idx: usize,
  selector: &'a Selector<crate::css::selectors::FastRenderSelectorImpl>,
  prelude_specificity: u32,
  prelude: Option<Selector<crate::css::selectors::FastRenderSelectorImpl>>,
  prelude_ancestor_hashes: Option<AncestorHashes>,
  prelude_fast_reject: u32,
  args_ancestor_hashes: Vec<AncestorHashes>,
  args_fast_reject: Vec<u32>,
  metadata: SelectorMetadata,
}

type SelectorBucketKey = u64;

// The rule index stores pre-hashed selector keys (ids, classes, tags, attribute names) so we can:
//   1. Avoid allocating/cloning `String`s when building the index.
//   2. Avoid hashing variable-length strings with SipHash during candidate lookup.
//
// Hash collisions only widen the candidate set (extra rules get checked by full selector matching),
// so correctness does not depend on the hash being collision-free.
const FNV_OFFSET_BASIS: u64 = 14695981039346656037;
const FNV_PRIME: u64 = 1099511628211;

#[inline]
fn selector_hash_bytes(bytes: &[u8]) -> SelectorBucketKey {
  let mut hash = FNV_OFFSET_BASIS;
  for &byte in bytes {
    hash ^= byte as u64;
    hash = hash.wrapping_mul(FNV_PRIME);
  }
  hash
}

#[inline]
fn selector_hash_str(value: &str) -> SelectorBucketKey {
  selector_hash_bytes(value.as_bytes())
}

#[inline]
fn selector_hash_ascii_lowercase(value: &str) -> SelectorBucketKey {
  let mut hash = FNV_OFFSET_BASIS;
  for byte in value.bytes() {
    let folded = if byte >= b'A' && byte <= b'Z' {
      byte + 32
    } else {
      byte
    };
    hash ^= folded as u64;
    hash = hash.wrapping_mul(FNV_PRIME);
  }
  hash
}

#[inline]
fn selector_bucket_id(value: &str) -> SelectorBucketKey {
  selector_hash_str(value)
}

#[inline]
fn selector_bucket_class(value: &str) -> SelectorBucketKey {
  selector_hash_str(value)
}

#[inline]
fn selector_bucket_tag(value: &str) -> SelectorBucketKey {
  selector_hash_ascii_lowercase(value)
}

#[inline]
fn selector_bucket_attr(value: &str) -> SelectorBucketKey {
  selector_hash_ascii_lowercase(value)
}

#[inline]
fn selector_bucket_part(value: &str) -> SelectorBucketKey {
  selector_hash_str(value)
}

#[derive(Default)]
struct SelectorBucketHasher(u64);

impl Hasher for SelectorBucketHasher {
  fn write(&mut self, bytes: &[u8]) {
    // `Hash` for `u64` uses `write_u64`. This is a fallback for unexpected key types.
    self.0 = selector_hash_bytes(bytes);
  }

  fn write_u64(&mut self, i: u64) {
    self.0 = i;
  }

  fn write_u32(&mut self, i: u32) {
    self.0 = i as u64;
  }

  fn write_usize(&mut self, i: usize) {
    self.0 = i as u64;
  }

  fn finish(&self) -> u64 {
    self.0
  }
}

type SelectorBucketBuildHasher = BuildHasherDefault<SelectorBucketHasher>;
type SelectorBucketMap<V> = HashMap<SelectorBucketKey, V, SelectorBucketBuildHasher>;

struct PseudoBuckets {
  by_id: SelectorBucketMap<Vec<usize>>,
  by_class: SelectorBucketMap<Vec<usize>>,
  by_tag: SelectorBucketMap<Vec<usize>>,
  by_attr: SelectorBucketMap<Vec<usize>>,
  universal: Vec<usize>,
}

impl PseudoBuckets {
  fn new() -> Self {
    Self {
      by_id: SelectorBucketMap::default(),
      by_class: SelectorBucketMap::default(),
      by_tag: SelectorBucketMap::default(),
      by_attr: SelectorBucketMap::default(),
      universal: Vec::new(),
    }
  }
}

struct SlottedBuckets {
  by_id: SelectorBucketMap<Vec<usize>>,
  by_class: SelectorBucketMap<Vec<usize>>,
  by_tag: SelectorBucketMap<Vec<usize>>,
  by_attr: SelectorBucketMap<Vec<usize>>,
  universal: Vec<usize>,
}

impl SlottedBuckets {
  fn new() -> Self {
    Self {
      by_id: SelectorBucketMap::default(),
      by_class: SelectorBucketMap::default(),
      by_tag: SelectorBucketMap::default(),
      by_attr: SelectorBucketMap::default(),
      universal: Vec::new(),
    }
  }
}

struct PartPseudoInfo {
  pseudo: PseudoElement,
}

struct RuleIndex<'a> {
  rules: Vec<CascadeRule<'a>>,
  rule_sets_content: Vec<bool>,
  has_has_requirements: bool,
  selectors: Vec<IndexedSelector<'a>>,
  fast_rejects: Vec<RightmostFastReject>,
  by_id: SelectorBucketMap<Vec<usize>>,
  by_class: SelectorBucketMap<Vec<usize>>,
  by_tag: SelectorBucketMap<Vec<usize>>,
  by_attr: SelectorBucketMap<Vec<usize>>,
  universal: Vec<usize>,
  pseudo_selectors: Vec<IndexedSelector<'a>>,
  pseudo_buckets: HashMap<PseudoElement, PseudoBuckets>,
  pseudo_content: HashSet<PseudoElement>,
  slotted_selectors: Vec<IndexedSlottedSelector<'a>>,
  slotted_buckets: SlottedBuckets,
  part_pseudos: Vec<PartPseudoInfo>,
  part_lookup: SelectorBucketMap<Vec<usize>>,
}

#[derive(Clone, Copy)]
struct SelectorDedupKey<'a> {
  fingerprint: u64,
  selector: &'a Selector<FastRenderSelectorImpl>,
}

impl<'a> SelectorDedupKey<'a> {
  fn new(selector: &'a Selector<FastRenderSelectorImpl>) -> Self {
    Self {
      fingerprint: selector_fingerprint(selector),
      selector,
    }
  }
}

impl Hash for SelectorDedupKey<'_> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.fingerprint.hash(state);
  }
}

impl PartialEq for SelectorDedupKey<'_> {
  fn eq(&self, other: &Self) -> bool {
    self.fingerprint == other.fingerprint && self.selector == other.selector
  }
}

impl Eq for SelectorDedupKey<'_> {}

fn selector_fingerprint(selector: &Selector<FastRenderSelectorImpl>) -> u64 {
  let mut hasher = FxHasher::default();
  hash_selector_for_dedup(&mut hasher, selector);
  hasher.finish()
}

fn hash_selector_for_dedup(state: &mut impl Hasher, selector: &Selector<FastRenderSelectorImpl>) {
  selector.specificity().hash(state);
  selector.len().hash(state);
  for component in selector.iter_raw_match_order() {
    hash_component_for_dedup(state, component);
  }
}

fn hash_selector_list_for_dedup(
  state: &mut impl Hasher,
  list: &SelectorList<FastRenderSelectorImpl>,
) {
  list.len().hash(state);
  for selector in list.slice().iter() {
    hash_selector_for_dedup(state, selector);
  }
}

fn hash_relative_selector_for_dedup(
  state: &mut impl Hasher,
  relative: &selectors::parser::RelativeSelector<FastRenderSelectorImpl>,
) {
  hash_relative_selector_match_hint_for_dedup(state, relative.match_hint);
  hash_selector_for_dedup(state, &relative.selector);
}

fn hash_relative_selector_match_hint_for_dedup(
  state: &mut impl Hasher,
  hint: selectors::parser::RelativeSelectorMatchHint,
) {
  // This is a small, stable enum; hashing its discriminant is sufficient.
  std::mem::discriminant(&hint).hash(state);
}

fn hash_pseudo_element_for_dedup(state: &mut impl Hasher, pseudo: &PseudoElement) {
  std::mem::discriminant(pseudo).hash(state);
  match pseudo {
    PseudoElement::Slotted(selectors) => {
      selectors.len().hash(state);
      for selector in selectors.iter() {
        hash_selector_for_dedup(state, selector);
      }
    }
    PseudoElement::Part(name) => name.hash(state),
    _ => {}
  }
}

fn hash_text_direction_for_dedup(state: &mut impl Hasher, dir: TextDirection) {
  std::mem::discriminant(&dir).hash(state);
}

fn hash_pseudo_class_for_dedup(state: &mut impl Hasher, pseudo: &PseudoClass) {
  std::mem::discriminant(pseudo).hash(state);
  match pseudo {
    PseudoClass::Has(relative) => {
      relative.len().hash(state);
      for rel in relative.iter() {
        hash_relative_selector_for_dedup(state, rel);
      }
    }
    PseudoClass::Host(list) => {
      list.is_some().hash(state);
      if let Some(list) = list {
        hash_selector_list_for_dedup(state, list);
      }
    }
    PseudoClass::HostContext(list) => hash_selector_list_for_dedup(state, list),
    PseudoClass::NthChild(a, b, of) | PseudoClass::NthLastChild(a, b, of) => {
      a.hash(state);
      b.hash(state);
      of.is_some().hash(state);
      if let Some(of) = of {
        hash_selector_list_for_dedup(state, of);
      }
    }
    PseudoClass::NthOfType(a, b) | PseudoClass::NthLastOfType(a, b) => {
      a.hash(state);
      b.hash(state);
    }
    PseudoClass::Lang(langs) => {
      langs.len().hash(state);
      for lang in langs {
        lang.hash(state);
      }
    }
    PseudoClass::Dir(dir) => hash_text_direction_for_dedup(state, *dir),
    _ => {}
  }
}

fn hash_attr_selector_operator_for_dedup(
  state: &mut impl Hasher,
  operator: selectors::attr::AttrSelectorOperator,
) {
  std::mem::discriminant(&operator).hash(state);
}

fn hash_parsed_case_sensitivity_for_dedup(
  state: &mut impl Hasher,
  sensitivity: selectors::attr::ParsedCaseSensitivity,
) {
  std::mem::discriminant(&sensitivity).hash(state);
}

fn hash_namespace_constraint_for_dedup(
  state: &mut impl Hasher,
  namespace: &selectors::attr::NamespaceConstraint<(CssString, CssString)>,
) {
  match namespace {
    selectors::attr::NamespaceConstraint::Any => {
      0u8.hash(state);
    }
    selectors::attr::NamespaceConstraint::Specific((prefix, url)) => {
      1u8.hash(state);
      prefix.hash(state);
      url.hash(state);
    }
  }
}

fn hash_parsed_attr_selector_operation_for_dedup(
  state: &mut impl Hasher,
  op: &selectors::attr::ParsedAttrSelectorOperation<CssString>,
) {
  match op {
    selectors::attr::ParsedAttrSelectorOperation::Exists => {
      0u8.hash(state);
    }
    selectors::attr::ParsedAttrSelectorOperation::WithValue {
      operator,
      case_sensitivity,
      value,
    } => {
      1u8.hash(state);
      hash_attr_selector_operator_for_dedup(state, *operator);
      hash_parsed_case_sensitivity_for_dedup(state, *case_sensitivity);
      value.hash(state);
    }
  }
}

fn hash_nth_type_for_dedup(state: &mut impl Hasher, ty: selectors::parser::NthType) {
  std::mem::discriminant(&ty).hash(state);
}

fn hash_combinator_for_dedup(state: &mut impl Hasher, combinator: selectors::parser::Combinator) {
  std::mem::discriminant(&combinator).hash(state);
}

fn hash_nth_selector_data_for_dedup(
  state: &mut impl Hasher,
  nth: &selectors::parser::NthSelectorData,
) {
  hash_nth_type_for_dedup(state, nth.ty);
  nth.is_function.hash(state);
  nth.an_plus_b.0.hash(state);
  nth.an_plus_b.1.hash(state);
}

fn hash_nth_of_selector_data_for_dedup(
  state: &mut impl Hasher,
  nth: &selectors::parser::NthOfSelectorData<FastRenderSelectorImpl>,
) {
  hash_nth_selector_data_for_dedup(state, nth.nth_data());
  nth.selectors().len().hash(state);
  for selector in nth.selectors() {
    hash_selector_for_dedup(state, selector);
  }
}

fn hash_component_for_dedup(
  state: &mut impl Hasher,
  component: &selectors::parser::Component<FastRenderSelectorImpl>,
) {
  use selectors::parser::Component;
  std::mem::discriminant(component).hash(state);
  match component {
    Component::LocalName(local) => {
      local.name.hash(state);
      local.lower_name.hash(state);
    }
    Component::ID(id) | Component::Class(id) => {
      id.hash(state);
    }
    Component::AttributeInNoNamespaceExists {
      local_name,
      local_name_lower,
    } => {
      local_name.hash(state);
      local_name_lower.hash(state);
    }
    Component::AttributeInNoNamespace {
      local_name,
      operator,
      value,
      case_sensitivity,
    } => {
      local_name.hash(state);
      hash_attr_selector_operator_for_dedup(state, *operator);
      value.hash(state);
      hash_parsed_case_sensitivity_for_dedup(state, *case_sensitivity);
    }
    Component::AttributeOther(other) => {
      other.namespace.is_some().hash(state);
      if let Some(ns) = other.namespace.as_ref() {
        hash_namespace_constraint_for_dedup(state, ns);
      }
      other.local_name.hash(state);
      other.local_name_lower.hash(state);
      hash_parsed_attr_selector_operation_for_dedup(state, &other.operation);
    }
    Component::DefaultNamespace(url) => {
      url.hash(state);
    }
    Component::Namespace(prefix, url) => {
      prefix.hash(state);
      url.hash(state);
    }
    Component::Negation(list) | Component::Where(list) | Component::Is(list) => {
      hash_selector_list_for_dedup(state, list);
    }
    Component::Nth(nth) => {
      hash_nth_selector_data_for_dedup(state, nth);
    }
    Component::NthOf(nth) => {
      hash_nth_of_selector_data_for_dedup(state, nth);
    }
    Component::NonTSPseudoClass(pseudo) => {
      hash_pseudo_class_for_dedup(state, pseudo);
    }
    Component::Slotted(selector) => {
      hash_selector_for_dedup(state, selector);
    }
    Component::Part(parts) => {
      parts.len().hash(state);
      for part in parts.iter() {
        part.hash(state);
      }
    }
    Component::Host(inner) => {
      inner.is_some().hash(state);
      if let Some(selector) = inner {
        hash_selector_for_dedup(state, selector);
      }
    }
    Component::Has(list) => {
      list.len().hash(state);
      for rel in list.iter() {
        hash_relative_selector_for_dedup(state, rel);
      }
    }
    Component::Invalid(reason) => {
      reason.as_str().hash(state);
    }
    Component::PseudoElement(pseudo) => {
      hash_pseudo_element_for_dedup(state, pseudo);
    }
    Component::Combinator(combinator) => {
      hash_combinator_for_dedup(state, *combinator);
    }
    _ => {}
  }
}

struct RuleScopes<'a> {
  ua: RuleIndex<'a>,
  document: RuleIndex<'a>,
  shadows: HashMap<usize, RuleIndex<'a>>,
  host_rules: HashMap<usize, RuleIndex<'a>>,
  slot_maps: HashMap<usize, SlotAssignmentMap<'a>>,
  fallback_document_rules_in_shadow_scopes: bool,
  quirks_mode: QuirksMode,
}

struct MatchIndex {
  positions: Vec<usize>,
  touched: Vec<usize>,
}

impl MatchIndex {
  fn new(rule_count: usize) -> Self {
    Self {
      positions: vec![usize::MAX; rule_count],
      touched: Vec::new(),
    }
  }

  fn get(&self, idx: usize) -> Option<usize> {
    let value = *self.positions.get(idx).unwrap_or(&usize::MAX);
    if value == usize::MAX {
      None
    } else {
      Some(value)
    }
  }

  fn insert(&mut self, idx: usize, value: usize) {
    if self.positions[idx] == usize::MAX {
      self.touched.push(idx);
    }
    self.positions[idx] = value;
  }

  fn reset(&mut self) {
    for idx in self.touched.drain(..) {
      self.positions[idx] = usize::MAX;
    }
  }
}

struct CascadeScratch {
  candidates: Vec<usize>,
  slotted_candidates: Vec<usize>,
  match_index: MatchIndex,
  candidate_seen: CandidateSet,
  candidate_stats: CandidateStats,
  candidate_merge: CandidateMergeScratch,
  part_candidates: Vec<usize>,
  part_seen: CandidateSet,
  scope_cache: ScopeResolutionCache,
  presentational_hint_layer_orders: FxHashMap<u32, Arc<[u32]>>,
  unlayered_layer_orders: FxHashMap<u32, Arc<[u32]>>,
}

impl CascadeScratch {
  fn new(rule_count: usize) -> Self {
    Self {
      candidates: Vec::new(),
      slotted_candidates: Vec::new(),
      match_index: MatchIndex::new(rule_count),
      candidate_seen: CandidateSet::new(rule_count),
      candidate_stats: CandidateStats::default(),
      candidate_merge: CandidateMergeScratch::default(),
      part_candidates: Vec::new(),
      part_seen: CandidateSet::new(rule_count),
      scope_cache: ScopeResolutionCache::default(),
      presentational_hint_layer_orders: FxHashMap::default(),
      unlayered_layer_orders: FxHashMap::default(),
    }
  }

  fn presentational_hint_layer_order(&mut self, tree_scope_prefix: u32) -> Arc<[u32]> {
    if let Some(hit) = self
      .presentational_hint_layer_orders
      .get(&tree_scope_prefix)
    {
      return hit.clone();
    }
    // Store a tree-scope-only layer order so presentational hints always sort below authored
    // stylesheet/inline rules within the same scope.
    let layer_order: Arc<[u32]> = Arc::from([tree_scope_prefix].as_slice());
    self
      .presentational_hint_layer_orders
      .insert(tree_scope_prefix, layer_order.clone());
    layer_order
  }

  fn unlayered_layer_order(&mut self, tree_scope_prefix: u32) -> Arc<[u32]> {
    if tree_scope_prefix == DOCUMENT_TREE_SCOPE_PREFIX {
      return document_unlayered_layer_order();
    }
    if let Some(hit) = self.unlayered_layer_orders.get(&tree_scope_prefix) {
      return hit.clone();
    }
    let layer_order: Arc<[u32]> = Arc::from([tree_scope_prefix, u32::MAX].as_slice());
    self
      .unlayered_layer_orders
      .insert(tree_scope_prefix, layer_order.clone());
    layer_order
  }
}

struct CandidateSet {
  seen: Vec<bool>,
  touched: Vec<usize>,
}

impl CandidateSet {
  fn new(rule_count: usize) -> Self {
    Self {
      seen: vec![false; rule_count],
      touched: Vec::new(),
    }
  }

  fn insert(&mut self, idx: usize) -> bool {
    self.mark_seen(idx)
  }

  fn contains(&self, idx: usize) -> bool {
    self.seen.get(idx).copied().unwrap_or(false)
  }

  fn mark_seen(&mut self, idx: usize) -> bool {
    if self.contains(idx) {
      return false;
    }
    if idx >= self.seen.len() {
      self.seen.resize(idx + 1, false);
    }
    self.seen[idx] = true;
    self.touched.push(idx);
    true
  }

  fn reset(&mut self) {
    for idx in self.touched.drain(..) {
      if let Some(slot) = self.seen.get_mut(idx) {
        *slot = false;
      }
    }
  }
}

#[derive(Default, Clone)]
struct CandidateStats {
  by_id: u64,
  by_class: u64,
  by_tag: u64,
  by_attr: u64,
  universal: u64,
  pruned: u64,
}

impl CandidateStats {
  fn reset(&mut self) {
    self.by_id = 0;
    self.by_class = 0;
    self.by_tag = 0;
    self.by_attr = 0;
    self.universal = 0;
    self.pruned = 0;
  }

  fn total(&self) -> u64 {
    self.by_id + self.by_class + self.by_tag + self.by_attr + self.universal
  }
}

#[derive(Clone, Copy)]
enum CandidateBucket {
  Id,
  Class,
  Tag,
  Attr,
  Universal,
}

impl CandidateBucket {
  fn rank(self) -> u8 {
    match self {
      CandidateBucket::Id => 0,
      CandidateBucket::Class => 1,
      CandidateBucket::Tag => 2,
      CandidateBucket::Attr => 3,
      CandidateBucket::Universal => 4,
    }
  }
}

struct CandidateCursor {
  ptr: *const usize,
  len: usize,
  pos: usize,
  bucket: CandidateBucket,
}

impl CandidateCursor {
  fn new(list: &[usize], bucket: CandidateBucket) -> Self {
    Self {
      ptr: list.as_ptr(),
      len: list.len(),
      pos: 0,
      bucket,
    }
  }

  fn current(&self) -> Option<usize> {
    if self.pos < self.len {
      // Safety: `pos < len` ensures the pointer remains within the slice.
      Some(unsafe { *self.ptr.add(self.pos) })
    } else {
      None
    }
  }
}

#[derive(Clone, Copy)]
struct CandidateHeapItem {
  rule_idx: usize,
  specificity: u32,
  selector_idx: usize,
  bucket_rank: u8,
  cursor_idx: usize,
}

impl PartialEq for CandidateHeapItem {
  fn eq(&self, other: &Self) -> bool {
    self.rule_idx == other.rule_idx
      && self.specificity == other.specificity
      && self.selector_idx == other.selector_idx
      && self.bucket_rank == other.bucket_rank
      && self.cursor_idx == other.cursor_idx
  }
}

impl Eq for CandidateHeapItem {}

impl PartialOrd for CandidateHeapItem {
  fn partial_cmp(&self, other: &Self) -> Option<CmpOrdering> {
    Some(self.cmp(other))
  }
}

impl Ord for CandidateHeapItem {
  fn cmp(&self, other: &Self) -> CmpOrdering {
    // `BinaryHeap` is a max heap; reverse comparisons to pop the smallest
    // (rule_idx asc, specificity desc) item first.
    other
      .rule_idx
      .cmp(&self.rule_idx)
      .then_with(|| self.specificity.cmp(&other.specificity))
      .then_with(|| other.selector_idx.cmp(&self.selector_idx))
      .then_with(|| other.bucket_rank.cmp(&self.bucket_rank))
      .then_with(|| other.cursor_idx.cmp(&self.cursor_idx))
  }
}

#[derive(Default)]
struct CandidateMergeScratch {
  cursors: Vec<CandidateCursor>,
  heap: BinaryHeap<CandidateHeapItem>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct ScopeSignature {
  hash_a: u64,
  hash_b: u64,
}

impl ScopeSignature {
  fn compute(scopes: &[ScopeContext<'_>]) -> Self {
    // Pointer-based fingerprint of a scope chain. The scope contexts are cloned per rule,
    // so we can't rely on slice identity. We instead hash the selector slice addresses
    // (plus lengths) and the implicit-start flag for each scope in the chain.
    //
    // This is used as a cache key within a single cascade pass; pointer values are stable
    // for the lifetime of the stylesheet and DOM.
    #[inline]
    fn mix(mut hash: u64, value: u64, prime: u64) -> u64 {
      hash ^= value;
      hash.wrapping_mul(prime)
    }

    // Two independent 64-bit hashes to make collisions vanishingly unlikely.
    let mut hash_a = FNV_OFFSET_BASIS;
    let mut hash_b = FNV_OFFSET_BASIS ^ 0x9e37_79b9_7f4a_7c15;
    for scope in scopes {
      let (start_ptr, start_len) = scope
        .start
        .map(|s| (s.as_ptr() as usize as u64, s.len() as u64))
        .unwrap_or((0, 0));
      let (end_ptr, end_len) = scope
        .end
        .map(|s| (s.as_ptr() as usize as u64, s.len() as u64))
        .unwrap_or((0, 0));
      let implicit = u64::from(scope.implicit_start);
      hash_a = mix(hash_a, start_ptr, FNV_PRIME);
      hash_a = mix(hash_a, start_len, FNV_PRIME);
      hash_a = mix(hash_a, end_ptr, FNV_PRIME);
      hash_a = mix(hash_a, end_len, FNV_PRIME);
      hash_a = mix(hash_a, implicit, FNV_PRIME);

      // Slightly different mixing for the second hash (different prime/rotation).
      hash_b = mix(hash_b.rotate_left(11), start_ptr, FNV_PRIME);
      hash_b = mix(hash_b.rotate_left(11), start_len, FNV_PRIME);
      hash_b = mix(hash_b.rotate_left(11), end_ptr, FNV_PRIME);
      hash_b = mix(hash_b.rotate_left(11), end_len, FNV_PRIME);
      hash_b = mix(hash_b.rotate_left(11), implicit, FNV_PRIME);
    }
    hash_a = mix(hash_a, scopes.len() as u64, FNV_PRIME);
    hash_b = mix(hash_b, scopes.len() as u64, FNV_PRIME);
    Self { hash_a, hash_b }
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ScopeMatchIndex {
  /// Index into the `(ancestors + [node])` path identifying the resolved scope root.
  root_index: usize,
}

impl ScopeMatchIndex {
  fn root_and_ancestors<'a>(
    self,
    node: &'a DomNode,
    ancestors: &'a [&'a DomNode],
  ) -> (&'a DomNode, &'a [&'a DomNode]) {
    debug_assert!(self.root_index <= ancestors.len());
    if self.root_index == ancestors.len() {
      (node, ancestors)
    } else {
      (ancestors[self.root_index], &ancestors[..self.root_index])
    }
  }
}

#[derive(Clone, Copy)]
enum ScopeMatchResult {
  Unscoped,
  Scoped(ScopeMatchIndex),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct ScopeCacheKey {
  node_id: usize,
  signature: ScopeSignature,
  context_key: u8,
}

struct ScopeCacheHasher(u64);

impl Default for ScopeCacheHasher {
  fn default() -> Self {
    Self(FNV_OFFSET_BASIS)
  }
}

impl ScopeCacheHasher {
  #[inline]
  fn mix_byte(hash: &mut u64, byte: u8) {
    *hash ^= byte as u64;
    *hash = hash.wrapping_mul(FNV_PRIME);
  }
}

impl Hasher for ScopeCacheHasher {
  fn write(&mut self, bytes: &[u8]) {
    // FNV-1a.
    for &byte in bytes {
      Self::mix_byte(&mut self.0, byte);
    }
  }

  fn finish(&self) -> u64 {
    self.0
  }
}

type ScopeCacheBuildHasher = BuildHasherDefault<ScopeCacheHasher>;
type ScopeResolutionMap<V> = HashMap<ScopeCacheKey, V, ScopeCacheBuildHasher>;

#[derive(Default)]
struct ScopeResolutionCache {
  entries: ScopeResolutionMap<Option<ScopeMatchIndex>>,
}

impl ScopeResolutionCache {
  fn clear(&mut self) {
    self.entries.clear();
  }

  fn resolve<'a>(
    &mut self,
    node_id: usize,
    signature: ScopeSignature,
    node: &'a DomNode,
    ancestors: &[&'a DomNode],
    scopes: &[ScopeContext<'a>],
    context: &mut MatchingContext<FastRenderSelectorImpl>,
  ) -> Option<ScopeMatchIndex> {
    let context_key = scope_cache_context_key(context);
    let key = ScopeCacheKey {
      node_id,
      signature,
      context_key,
    };
    if let Some(hit) = self.entries.get(&key) {
      return *hit;
    }
    let resolved = resolve_scopes(node, ancestors, scopes, context);
    self.entries.insert(key, resolved);
    resolved
  }
}

fn scope_cache_context_key(context: &MatchingContext<FastRenderSelectorImpl>) -> u8 {
  let mode = match context.matching_mode() {
    MatchingMode::Normal => 0u8,
    MatchingMode::ForStatelessPseudoElement => 1u8,
  };
  let visited = match context.visited_handling() {
    VisitedHandlingMode::AllLinksUnvisited => 0u8,
    VisitedHandlingMode::AllLinksVisitedAndUnvisited => 1u8,
    VisitedHandlingMode::RelevantLinkVisited => 2u8,
  };
  mode | (visited << 2)
}

#[derive(Clone)]
struct ScopeMatch<'a> {
  root: &'a DomNode,
  ancestors: Vec<&'a DomNode>,
}

fn push_key(keys: &mut Vec<SelectorKey>, key: SelectorKey) -> bool {
  if keys.contains(&key) {
    return false;
  }
  keys.push(key);
  true
}

#[derive(Clone, Copy)]
enum SelectorKeyPolarity {
  Matches,
  DoesNotMatch,
}

fn merge_nested_keys<'a, I>(
  selectors: I,
  polarity: SelectorKeyPolarity,
  allow_universal: bool,
  out: &mut Vec<SelectorKey>,
) -> bool
where
  I: IntoIterator<Item = &'a Selector<crate::css::selectors::FastRenderSelectorImpl>>,
{
  let mut inserted = false;
  for selector in selectors {
    let nested = selector_keys_with_polarity(selector, polarity);
    if nested.keys.len() == 1
      && matches!(nested.keys[0], SelectorKey::Universal)
      && !allow_universal
    {
      // Avoid falling back to the universal bucket when the outer compound already
      // guarantees a more specific key (e.g., div:is(:not(.a))).
      continue;
    }
    for key in nested.keys {
      inserted |= push_key(out, key);
    }
  }
  inserted
}

struct SelectorKeyAnalysis {
  keys: Vec<SelectorKey>,
  required_and: bool,
}

fn collect_sequence_keys(
  iter: &mut selectors::parser::SelectorIter<crate::css::selectors::FastRenderSelectorImpl>,
  polarity: SelectorKeyPolarity,
) -> SelectorKeyAnalysis {
  use selectors::parser::Component;

  let mut keys: Vec<SelectorKey> = Vec::new();
  let mut required_and = true;
  while let Some(component) = iter.next() {
    match component {
      Component::ID(ident) => {
        if matches!(polarity, SelectorKeyPolarity::Matches) {
          push_key(
            &mut keys,
            SelectorKey::Id(selector_bucket_id(ident.as_str())),
          );
        }
      }
      Component::Class(cls) => {
        if matches!(polarity, SelectorKeyPolarity::Matches) {
          push_key(
            &mut keys,
            SelectorKey::Class(selector_bucket_class(cls.as_str())),
          );
        }
      }
      Component::LocalName(local) => {
        if matches!(polarity, SelectorKeyPolarity::Matches) {
          push_key(
            &mut keys,
            SelectorKey::Tag(selector_bucket_tag(local.lower_name.as_str())),
          );
        }
      }
      Component::AttributeInNoNamespaceExists {
        local_name_lower, ..
      } => {
        if matches!(polarity, SelectorKeyPolarity::Matches) {
          push_key(
            &mut keys,
            SelectorKey::Attribute(selector_bucket_attr(local_name_lower.as_str())),
          );
        }
      }
      Component::AttributeInNoNamespace { local_name, .. } => {
        if matches!(polarity, SelectorKeyPolarity::Matches) {
          push_key(
            &mut keys,
            SelectorKey::Attribute(selector_bucket_attr(local_name.as_str())),
          );
        }
      }
      Component::AttributeOther(other) => {
        if matches!(polarity, SelectorKeyPolarity::Matches) && other.namespace.is_none() {
          push_key(
            &mut keys,
            SelectorKey::Attribute(selector_bucket_attr(other.local_name_lower.as_str())),
          );
        }
      }
      Component::NthOf(nth) => {
        if keys.is_empty()
          && merge_nested_keys(nth.selectors().iter(), polarity, true, &mut keys)
        {
          required_and = false;
        }
      }
      Component::Host(Some(inner)) => {
        if keys.is_empty()
          && merge_nested_keys(std::iter::once(inner), polarity, true, &mut keys)
        {
          required_and = false;
        }
      }
      Component::NonTSPseudoClass(pc) => match pc {
        PseudoClass::NthChild(_, _, Some(list)) | PseudoClass::NthLastChild(_, _, Some(list)) => {
          if keys.is_empty() && merge_nested_keys(list.slice().iter(), polarity, true, &mut keys) {
            required_and = false;
          }
        }
        PseudoClass::Host(Some(list)) => {
          if keys.is_empty() && merge_nested_keys(list.slice().iter(), polarity, true, &mut keys) {
            required_and = false;
          }
        }
        _ => {}
      },
      Component::Is(list) => {
        if merge_nested_keys(list.slice().iter(), polarity, keys.is_empty(), &mut keys) {
          required_and = false;
        }
      }
      Component::Where(list) => {
        if merge_nested_keys(list.slice().iter(), polarity, keys.is_empty(), &mut keys) {
          required_and = false;
        }
      }
      Component::Negation(list) => {
        let flipped = match polarity {
          SelectorKeyPolarity::Matches => SelectorKeyPolarity::DoesNotMatch,
          SelectorKeyPolarity::DoesNotMatch => SelectorKeyPolarity::Matches,
        };
        if merge_nested_keys(list.slice().iter(), flipped, keys.is_empty(), &mut keys) {
          required_and = false;
        }
      }
      _ => {}
    }
  }

  if keys.is_empty() && matches!(polarity, SelectorKeyPolarity::Matches) {
    keys.push(SelectorKey::Universal);
  }

  SelectorKeyAnalysis { keys, required_and }
}

fn selector_keys_with_polarity(
  selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>,
  polarity: SelectorKeyPolarity,
) -> SelectorKeyAnalysis {
  let mut iter = selector.iter();
  collect_sequence_keys(&mut iter, polarity)
}

fn pseudo_selector_subject_key_analysis(
  selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>,
) -> SelectorKeyAnalysis {
  use selectors::parser::Combinator;

  let mut iter = selector.iter();
  while iter.next().is_some() {}
  let Some(Combinator::PseudoElement) = iter.next_sequence() else {
    return selector_keys_with_polarity(selector, SelectorKeyPolarity::Matches);
  };

  collect_sequence_keys(&mut iter, SelectorKeyPolarity::Matches)
}

fn selector_rightmost_compound_fast_reject(
  selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>,
  quirks_mode: QuirksMode,
) -> Option<RightmostFastReject> {
  let mut iter = selector.iter();
  selector_compound_fast_reject_from_iter(&mut iter, quirks_mode)
}

fn pseudo_selector_subject_fast_reject(
  selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>,
  quirks_mode: QuirksMode,
) -> Option<RightmostFastReject> {
  use selectors::parser::Combinator;

  let mut iter = selector.iter();
  while iter.next().is_some() {}
  let Some(Combinator::PseudoElement) = iter.next_sequence() else {
    return selector_rightmost_compound_fast_reject(selector, quirks_mode);
  };
  selector_compound_fast_reject_from_iter(&mut iter, quirks_mode)
}

fn selector_compound_fast_reject_from_iter(
  iter: &mut selectors::parser::SelectorIter<FastRenderSelectorImpl>,
  quirks_mode: QuirksMode,
) -> Option<RightmostFastReject> {
  use selectors::parser::Component;

  let mut has_tag = false;
  let mut tag_key: SelectorBucketKey = 0;
  let mut has_id = false;
  let mut id_key: SelectorBucketKey = 0;

  // We intentionally skip class/id requirements in full quirks mode: those selectors match
  // ASCII-case-insensitively against HTML elements, but `NodeSelectorKeys` stores case-sensitive
  // hashes and would cause false negatives.
  let allow_class_id = !matches!(quirks_mode, QuirksMode::Quirks);

  let mut class_keys: [SelectorBucketKey; RIGHTMOST_FAST_REJECT_MAX_KEYS] =
    [0; RIGHTMOST_FAST_REJECT_MAX_KEYS];
  let mut class_len = 0usize;
  let mut attr_keys: [SelectorBucketKey; RIGHTMOST_FAST_REJECT_MAX_KEYS] =
    [0; RIGHTMOST_FAST_REJECT_MAX_KEYS];
  let mut attr_len = 0usize;

  while let Some(component) = iter.next() {
    match component {
      Component::LocalName(local) => {
        let name = local.lower_name.as_str();
        if name == "*" {
          continue;
        }
        let key = selector_bucket_tag(name);
        if has_tag && tag_key != key {
          return None;
        }
        has_tag = true;
        tag_key = key;
      }
      Component::ExplicitUniversalType => {}
      Component::ID(ident) => {
        if !allow_class_id {
          continue;
        }
        let key = selector_bucket_id(ident.as_str());
        if has_id && id_key != key {
          return None;
        }
        has_id = true;
        id_key = key;
      }
      Component::Class(class) => {
        if !allow_class_id {
          continue;
        }
        let key = selector_bucket_class(class.as_str());
        if class_keys[..class_len].iter().any(|existing| *existing == key) {
          continue;
        }
        if class_len.saturating_add(attr_len) >= RIGHTMOST_FAST_REJECT_MAX_KEYS {
          return None;
        }
        class_keys[class_len] = key;
        class_len += 1;
      }
      Component::AttributeInNoNamespaceExists {
        local_name_lower, ..
      } => {
        let key = selector_bucket_attr(local_name_lower.as_str());
        if attr_keys[..attr_len].iter().any(|existing| *existing == key) {
          continue;
        }
        if class_len.saturating_add(attr_len) >= RIGHTMOST_FAST_REJECT_MAX_KEYS {
          return None;
        }
        attr_keys[attr_len] = key;
        attr_len += 1;
      }
      Component::AttributeInNoNamespace { local_name, .. } => {
        let key = selector_bucket_attr(local_name.as_str());
        if attr_keys[..attr_len].iter().any(|existing| *existing == key) {
          continue;
        }
        if class_len.saturating_add(attr_len) >= RIGHTMOST_FAST_REJECT_MAX_KEYS {
          return None;
        }
        attr_keys[attr_len] = key;
        attr_len += 1;
      }
      Component::AttributeOther(other) => {
        if other.namespace.is_some() {
          return None;
        }
        let key = selector_bucket_attr(other.local_name_lower.as_str());
        if attr_keys[..attr_len].iter().any(|existing| *existing == key) {
          continue;
        }
        if class_len.saturating_add(attr_len) >= RIGHTMOST_FAST_REJECT_MAX_KEYS {
          return None;
        }
        attr_keys[attr_len] = key;
        attr_len += 1;
      }
      // Namespace constraints don't affect which `NodeSelectorKeys` we can check; they are already
      // enforced by `selector_metadata_matches_node`.
      Component::Namespace(..)
      | Component::DefaultNamespace(..)
      | Component::ExplicitNoNamespace
      | Component::ExplicitAnyNamespace => {}
      // Allow simple pseudo classes / nth selectors that don't change the meaning of the extracted
      // type/id/class/attr requirements. Reject cases with nested selector lists.
      Component::NonTSPseudoClass(pc) => match pc {
        PseudoClass::Has(_)
        | PseudoClass::Host(Some(_))
        | PseudoClass::HostContext(_)
        | PseudoClass::NthChild(_, _, Some(_))
        | PseudoClass::NthLastChild(_, _, Some(_)) => return None,
        _ => {}
      },
      Component::Nth(_) => {}
      // Branching constructs, or components we don't know how to treat soundly.
      _ => return None,
    }
  }

  let total_required = usize::from(has_tag) + usize::from(has_id) + class_len + attr_len;
  if total_required < 2 {
    return None;
  }

  let mut keys: [SelectorBucketKey; RIGHTMOST_FAST_REJECT_MAX_KEYS] = [0; RIGHTMOST_FAST_REJECT_MAX_KEYS];
  keys[..class_len].copy_from_slice(&class_keys[..class_len]);
  keys[class_len..class_len + attr_len].copy_from_slice(&attr_keys[..attr_len]);

  let mut flags = 0u8;
  if has_tag {
    flags |= RightmostFastReject::FLAG_HAS_TAG;
  }
  if has_id {
    flags |= RightmostFastReject::FLAG_HAS_ID;
  }

  Some(RightmostFastReject {
    tag_key,
    id_key,
    keys,
    class_len: class_len as u8,
    attr_len: attr_len as u8,
    flags,
  })
}

fn merge_namespace_constraint(current: Option<String>, new: Option<String>) -> Option<String> {
  match (current, new) {
    (None, constraint) => constraint,
    (Some(existing), Some(candidate)) if existing == candidate => Some(existing),
    _ => None,
  }
}

fn selector_namespace_constraint(
  selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>,
) -> Option<String> {
  use selectors::parser::Component;

  let mut required: Option<String> = None;
  let mut iter = selector.iter();
  loop {
    if let Some(component) = iter.next() {
      match component {
        Component::Namespace(_, url) | Component::DefaultNamespace(url) => {
          required = merge_namespace_constraint(required, Some(url.0.clone()));
        }
        Component::ExplicitNoNamespace => {
          required = merge_namespace_constraint(required, Some(String::new()));
        }
        Component::ExplicitAnyNamespace => required = None,
        Component::Is(list) | Component::Where(list) | Component::Negation(list) => {
          let nested = list.slice().iter().fold(None, |ns, sel| {
            merge_namespace_constraint(ns, selector_namespace_constraint(sel))
          });
          required = merge_namespace_constraint(required, nested);
        }
        Component::Slotted(inner) => {
          required = merge_namespace_constraint(required, selector_namespace_constraint(inner));
        }
        Component::Host(Some(inner)) => {
          required = merge_namespace_constraint(required, selector_namespace_constraint(inner));
        }
        Component::PseudoElement(pseudo) => {
          if let PseudoElement::Slotted(list) = pseudo {
            let nested = list.iter().fold(None, |ns, sel| {
              merge_namespace_constraint(ns, selector_namespace_constraint(sel))
            });
            required = merge_namespace_constraint(required, nested);
          }
        }
        _ => {}
      }
      continue;
    }
    if iter.next_sequence().is_none() {
      break;
    }
  }
  required
}

fn selector_has_requirement_groups(
  selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>,
) -> Vec<Vec<HasRequirement>> {
  use selectors::parser::Component;

  let mut groups: Vec<Vec<HasRequirement>> = Vec::new();
  let mut iter = selector.iter();
  while let Some(component) = iter.next() {
    let relative_selectors = match component {
      Component::Has(list) => Some(&list[..]),
      Component::NonTSPseudoClass(pc) => match pc {
        PseudoClass::Has(list) => Some(&list[..]),
        _ => None,
      },
      _ => None,
    };
    if let Some(list) = relative_selectors {
      let mut group: Vec<HasRequirement> = Vec::new();
      let mut unprunable = false;
      for relative in list.iter() {
        if !relative.match_hint.is_descendant_direction() {
          unprunable = true;
          break;
        }
        let non_quirks = crate::dom::relative_selector_bloom_hashes(relative, QuirksMode::NoQuirks);
        let quirks = crate::dom::relative_selector_bloom_hashes(relative, QuirksMode::Quirks);
        group.push(HasRequirement {
          non_quirks_hashes: non_quirks,
          quirks_hashes: quirks,
        });
      }
      if unprunable {
        groups.push(Vec::new());
      } else {
        groups.push(group);
      }
    }
  }

  groups
}

fn selector_metadata(
  selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>,
) -> SelectorMetadata {
  SelectorMetadata {
    namespace: selector_namespace_constraint(selector),
    has_requirement_groups: selector_has_requirement_groups(selector),
  }
}

fn selector_targets_shadow_host(
  selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>,
) -> bool {
  use selectors::parser::Component;

  let mut iter = selector.iter();
  loop {
    while let Some(component) = iter.next() {
      match component {
        Component::Host(..) => return true,
        Component::NonTSPseudoClass(pc) => match pc {
          PseudoClass::Host(..) | PseudoClass::HostContext(..) => return true,
          _ => {}
        },
        Component::Is(list) | Component::Where(list) | Component::Negation(list) => {
          if list.slice().iter().any(selector_targets_shadow_host) {
            return true;
          }
        }
        _ => {}
      }
    }
    if iter.next_sequence().is_none() {
      break;
    }
  }

  false
}

fn rule_targets_shadow_host(rule: &StyleRule) -> bool {
  rule
    .selectors
    .slice()
    .iter()
    .any(selector_targets_shadow_host)
}

fn parse_slotted_prelude(
  selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>,
) -> Option<Selector<crate::css::selectors::FastRenderSelectorImpl>> {
  use selectors::parser::Component;

  // Drop the rightmost pseudo-element component and its implicit combinator,
  // leaving the selector that matches the assigned slot.
  let components: Vec<Component<FastRenderSelectorImpl>> =
    selector.iter_raw_match_order().skip(2).cloned().collect();
  if components.is_empty() {
    return None;
  }

  // Convert from match-order (right-to-left) representation to parse-order
  // (left-to-right) without reversing the component order within each compound.
  let mut sequences: Vec<Vec<Component<FastRenderSelectorImpl>>> = Vec::new();
  let mut combinators: Vec<selectors::parser::Combinator> = Vec::new();
  let mut current: Vec<Component<FastRenderSelectorImpl>> = Vec::new();

  for component in components {
    match component {
      Component::Combinator(c) => {
        sequences.push(current);
        current = Vec::new();
        combinators.push(c);
      }
      other => current.push(other),
    }
  }
  sequences.push(current);

  // An empty compound selector immediately preceding ::slotted() is an implicit
  // universal selector. Make it explicit so the synthesized selector is valid.
  if sequences.first().is_some_and(|seq| seq.is_empty()) {
    sequences[0].push(Component::ExplicitUniversalType);
  }

  sequences.reverse();
  combinators.reverse();

  let mut parse_components: Vec<Component<FastRenderSelectorImpl>> =
    Vec::with_capacity(sequences.iter().map(|s| s.len()).sum::<usize>() + combinators.len());
  for (idx, seq) in sequences.into_iter().enumerate() {
    parse_components.extend(seq);
    if idx < combinators.len() {
      parse_components.push(Component::Combinator(combinators[idx]));
    }
  }

  Some(Selector::from_components(parse_components))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum SelectorKey {
  Id(SelectorBucketKey),
  Class(SelectorBucketKey),
  Tag(SelectorBucketKey),
  Attribute(SelectorBucketKey),
  Universal,
}

fn selector_key_type_rank(key: SelectorKey) -> u8 {
  match key {
    SelectorKey::Id(_) => 0,
    SelectorKey::Class(_) => 1,
    SelectorKey::Attribute(_) => 2,
    SelectorKey::Tag(_) => 3,
    SelectorKey::Universal => 4,
  }
}

fn choose_anchor_key(keys: &[SelectorKey], frequencies: &FxHashMap<SelectorKey, usize>) -> SelectorKey {
  debug_assert!(!keys.is_empty());
  let mut best = keys[0];
  let mut best_freq = *frequencies.get(&best).unwrap_or(&0);
  let mut best_rank = selector_key_type_rank(best);
  let mut best_pos = 0usize;

  for (pos, &key) in keys.iter().enumerate().skip(1) {
    let freq = *frequencies.get(&key).unwrap_or(&0);
    let rank = selector_key_type_rank(key);
    if freq < best_freq || (freq == best_freq && (rank, pos) < (best_rank, best_pos)) {
      best = key;
      best_freq = freq;
      best_rank = rank;
      best_pos = pos;
    }
  }

  best
}

impl<'a> RuleIndex<'a> {
  #[inline]
  fn fast_reject(&self, id: u32) -> Option<&RightmostFastReject> {
    if id == 0 {
      None
    } else {
      self.fast_rejects.get(id.saturating_sub(1) as usize)
    }
  }

  fn new(rules: Vec<CascadeRule<'a>>, quirks_mode: QuirksMode) -> Self {
    let mut index = RuleIndex {
      rules: Vec::new(),
      rule_sets_content: Vec::new(),
      has_has_requirements: false,
      selectors: Vec::new(),
      fast_rejects: Vec::new(),
      by_id: SelectorBucketMap::default(),
      by_class: SelectorBucketMap::default(),
      by_tag: SelectorBucketMap::default(),
      by_attr: SelectorBucketMap::default(),
      universal: Vec::new(),
      pseudo_selectors: Vec::new(),
      pseudo_buckets: HashMap::new(),
      pseudo_content: HashSet::new(),
      slotted_selectors: Vec::new(),
      slotted_buckets: SlottedBuckets::new(),
      part_pseudos: Vec::new(),
      part_lookup: SelectorBucketMap::default(),
    };

    let mut selector_key_analyses: Vec<SelectorKeyAnalysis> = Vec::new();
    let mut selector_key_frequencies: FxHashMap<SelectorKey, usize> = FxHashMap::default();
    let mut pseudo_key_analyses: Vec<(PseudoElement, SelectorKeyAnalysis)> = Vec::new();
    let mut pseudo_key_frequencies: HashMap<PseudoElement, FxHashMap<SelectorKey, usize>> =
      HashMap::new();
    let mut slotted_arg_key_analyses: Vec<Vec<SelectorKeyAnalysis>> = Vec::new();
    let mut slotted_key_frequencies: FxHashMap<SelectorKey, usize> = FxHashMap::default();

    for rule in rules {
      let sets_content = rule
        .rule
        .declarations
        .iter()
        .any(|decl| decl.property == "content");
      let rule_idx = index.rules.len();
      index.rules.push(rule);
      index.rule_sets_content.push(sets_content);
      let stored_rule = &index.rules[rule_idx];
      let mut seen_selectors: FxHashSet<SelectorDedupKey<'a>> = FxHashSet::default();

      for selector in stored_rule.rule.selectors.slice().iter() {
        // Skip duplicate selectors within the same rule to avoid redundant matching work.
        if !seen_selectors.insert(SelectorDedupKey::new(selector)) {
          continue;
        }

        if let Some(pe) = selector.pseudo_element() {
          if matches!(pe, PseudoElement::Slotted(_)) {
            let prelude = parse_slotted_prelude(selector);
            let prelude_specificity = prelude.as_ref().map(|sel| sel.specificity()).unwrap_or(0);
            let prelude_ancestor_hashes =
              prelude.as_ref().map(|sel| cascade_ancestor_hashes(sel, quirks_mode));
            let prelude_fast_reject = match prelude
              .as_ref()
              .and_then(|sel| selector_rightmost_compound_fast_reject(sel, quirks_mode))
            {
              Some(desc) => {
                index.fast_rejects.push(desc);
                u32::try_from(index.fast_rejects.len())
                  .expect("selector fast-reject cache overflow")
              }
              None => 0,
            };
            let args_ancestor_hashes = match pe {
              PseudoElement::Slotted(args) => args
                .iter()
                .map(|sel| cascade_ancestor_hashes(sel, quirks_mode))
                .collect(),
              _ => Vec::new(),
            };
            let args_fast_reject = match pe {
              PseudoElement::Slotted(args) => {
                let mut out: Vec<u32> = Vec::with_capacity(args.len());
                for sel in args.iter() {
                  let id = match selector_rightmost_compound_fast_reject(sel, quirks_mode) {
                    Some(desc) => {
                      index.fast_rejects.push(desc);
                      u32::try_from(index.fast_rejects.len())
                        .expect("selector fast-reject cache overflow")
                    }
                    None => 0,
                  };
                  out.push(id);
                }
                out
              }
              _ => Vec::new(),
            };
            let metadata = selector_metadata(selector);
            if metadata
              .has_requirement_groups
              .iter()
              .any(|group| group.iter().any(|req| !req.is_empty()))
            {
              index.has_has_requirements = true;
            }
            index.slotted_selectors.push(IndexedSlottedSelector {
              rule_idx,
              selector,
              prelude_specificity,
              prelude,
              prelude_ancestor_hashes,
              prelude_fast_reject,
              args_ancestor_hashes,
              args_fast_reject,
              metadata,
            });

            let arg_analyses: Vec<SelectorKeyAnalysis> = match pe {
              PseudoElement::Slotted(args) => args
                .iter()
                .map(|sel| selector_keys_with_polarity(sel, SelectorKeyPolarity::Matches))
                .collect(),
              _ => Vec::new(),
            };
            for analysis in arg_analyses.iter() {
              for &key in analysis.keys.iter() {
                *slotted_key_frequencies.entry(key).or_default() += 1;
              }
            }
            slotted_arg_key_analyses.push(arg_analyses);
            continue;
          }
          let pseudo = pe.clone();
          if selector_has_nonempty_content(&stored_rule.rule.declarations) {
            index.pseudo_content.insert(pseudo.clone());
          }
          let metadata = selector_metadata(selector);
          if metadata
            .has_requirement_groups
            .iter()
            .any(|group| group.iter().any(|req| !req.is_empty()))
          {
            index.has_has_requirements = true;
          }
          let ancestor_hashes = cascade_ancestor_hashes(selector, quirks_mode);
          let fast_reject = match pseudo_selector_subject_fast_reject(selector, quirks_mode) {
            Some(desc) => {
              index.fast_rejects.push(desc);
              u32::try_from(index.fast_rejects.len()).expect("selector fast-reject cache overflow")
            }
            None => 0,
          };
          index.pseudo_selectors.push(IndexedSelector {
            rule_idx,
            selector,
            specificity: selector.specificity(),
            ancestor_hashes,
            metadata,
            fast_reject,
          });

          let analysis = pseudo_selector_subject_key_analysis(selector);
          let freq = pseudo_key_frequencies.entry(pseudo.clone()).or_default();
          for &key in analysis.keys.iter() {
            *freq.entry(key).or_default() += 1;
          }
          pseudo_key_analyses.push((pseudo, analysis));
          continue;
        }

        let metadata = selector_metadata(selector);
        if metadata
          .has_requirement_groups
          .iter()
          .any(|group| group.iter().any(|req| !req.is_empty()))
        {
          index.has_has_requirements = true;
        }
        let ancestor_hashes = cascade_ancestor_hashes(selector, quirks_mode);
        let fast_reject = match selector_rightmost_compound_fast_reject(selector, quirks_mode) {
          Some(desc) => {
            index.fast_rejects.push(desc);
            u32::try_from(index.fast_rejects.len()).expect("selector fast-reject cache overflow")
          }
          None => 0,
        };
        index.selectors.push(IndexedSelector {
          rule_idx,
          selector,
          specificity: selector.specificity(),
          ancestor_hashes,
          metadata,
          fast_reject,
        });

        let analysis = selector_keys_with_polarity(selector, SelectorKeyPolarity::Matches);
        for &key in analysis.keys.iter() {
          *selector_key_frequencies.entry(key).or_default() += 1;
        }
        selector_key_analyses.push(analysis);
      }
    }

    for (selector_idx, analysis) in selector_key_analyses.iter().enumerate() {
      if analysis.required_and {
        let anchor = choose_anchor_key(&analysis.keys, &selector_key_frequencies);
        match anchor {
          SelectorKey::Id(id) => index.by_id.entry(id).or_default().push(selector_idx),
          SelectorKey::Class(cls) => index.by_class.entry(cls).or_default().push(selector_idx),
          SelectorKey::Tag(tag) => index.by_tag.entry(tag).or_default().push(selector_idx),
          SelectorKey::Attribute(attr) => index.by_attr.entry(attr).or_default().push(selector_idx),
          SelectorKey::Universal => index.universal.push(selector_idx),
        }
      } else {
        for &key in analysis.keys.iter() {
          match key {
            SelectorKey::Id(id) => index.by_id.entry(id).or_default().push(selector_idx),
            SelectorKey::Class(cls) => index.by_class.entry(cls).or_default().push(selector_idx),
            SelectorKey::Tag(tag) => index.by_tag.entry(tag).or_default().push(selector_idx),
            SelectorKey::Attribute(attr) => {
              index.by_attr.entry(attr).or_default().push(selector_idx)
            }
            SelectorKey::Universal => index.universal.push(selector_idx),
          }
        }
      }
    }

    for (selector_idx, (pseudo, analysis)) in pseudo_key_analyses.iter().enumerate() {
      let Some(freq) = pseudo_key_frequencies.get(pseudo) else {
        continue;
      };
      let bucket = index
        .pseudo_buckets
        .entry(pseudo.clone())
        .or_insert_with(PseudoBuckets::new);
      if analysis.required_and {
        let anchor = choose_anchor_key(&analysis.keys, freq);
        match anchor {
          SelectorKey::Id(id) => bucket.by_id.entry(id).or_default().push(selector_idx),
          SelectorKey::Class(cls) => bucket.by_class.entry(cls).or_default().push(selector_idx),
          SelectorKey::Tag(tag) => bucket.by_tag.entry(tag).or_default().push(selector_idx),
          SelectorKey::Attribute(attr) => bucket.by_attr.entry(attr).or_default().push(selector_idx),
          SelectorKey::Universal => bucket.universal.push(selector_idx),
        }
      } else {
        for &key in analysis.keys.iter() {
          match key {
            SelectorKey::Id(id) => bucket.by_id.entry(id).or_default().push(selector_idx),
            SelectorKey::Class(cls) => bucket.by_class.entry(cls).or_default().push(selector_idx),
            SelectorKey::Tag(tag) => bucket.by_tag.entry(tag).or_default().push(selector_idx),
            SelectorKey::Attribute(attr) => {
              bucket.by_attr.entry(attr).or_default().push(selector_idx)
            }
            SelectorKey::Universal => bucket.universal.push(selector_idx),
          }
        }
      }
    }

    for (selector_idx, arg_analyses) in slotted_arg_key_analyses.iter().enumerate() {
      let mut keys: Vec<SelectorKey> = Vec::new();
      for analysis in arg_analyses.iter() {
        if analysis.required_and {
          push_key(
            &mut keys,
            choose_anchor_key(&analysis.keys, &slotted_key_frequencies),
          );
        } else {
          for &key in analysis.keys.iter() {
            push_key(&mut keys, key);
          }
        }
      }
      if keys.is_empty() {
        keys.push(SelectorKey::Universal);
      }
      for key in keys {
        match key {
          SelectorKey::Id(id) => index
            .slotted_buckets
            .by_id
            .entry(id)
            .or_default()
            .push(selector_idx),
          SelectorKey::Class(cls) => index
            .slotted_buckets
            .by_class
            .entry(cls)
            .or_default()
            .push(selector_idx),
          SelectorKey::Tag(tag) => index
            .slotted_buckets
            .by_tag
            .entry(tag)
            .or_default()
            .push(selector_idx),
          SelectorKey::Attribute(attr) => index
            .slotted_buckets
            .by_attr
            .entry(attr)
            .or_default()
            .push(selector_idx),
          SelectorKey::Universal => index.slotted_buckets.universal.push(selector_idx),
        }
      }
    }

    index.build_part_index();
    index.sort_selector_buckets();

    index
  }

  fn sort_selector_buckets(&mut self) {
    fn sort_bucket(list: &mut Vec<usize>, selectors: &[IndexedSelector<'_>]) {
      list.sort_unstable_by(|a, b| {
        let a_sel = &selectors[*a];
        let b_sel = &selectors[*b];
        a_sel
          .rule_idx
          .cmp(&b_sel.rule_idx)
          .then(b_sel.specificity.cmp(&a_sel.specificity))
          .then(a.cmp(b))
      });
    }

    let selectors = &self.selectors;
    for list in self.by_id.values_mut() {
      sort_bucket(list, selectors);
    }
    for list in self.by_class.values_mut() {
      sort_bucket(list, selectors);
    }
    for list in self.by_tag.values_mut() {
      sort_bucket(list, selectors);
    }
    for list in self.by_attr.values_mut() {
      sort_bucket(list, selectors);
    }
    sort_bucket(&mut self.universal, selectors);

    fn sort_slotted_bucket(list: &mut Vec<usize>, selectors: &[IndexedSlottedSelector<'_>]) {
      list.sort_unstable_by(|a, b| {
        let a_sel = &selectors[*a];
        let b_sel = &selectors[*b];
        a_sel.rule_idx.cmp(&b_sel.rule_idx).then(a.cmp(b))
      });
    }

    let slotted_selectors = &self.slotted_selectors;
    for list in self.slotted_buckets.by_id.values_mut() {
      sort_slotted_bucket(list, slotted_selectors);
    }
    for list in self.slotted_buckets.by_class.values_mut() {
      sort_slotted_bucket(list, slotted_selectors);
    }
    for list in self.slotted_buckets.by_tag.values_mut() {
      sort_slotted_bucket(list, slotted_selectors);
    }
    for list in self.slotted_buckets.by_attr.values_mut() {
      sort_slotted_bucket(list, slotted_selectors);
    }
    sort_slotted_bucket(&mut self.slotted_buckets.universal, slotted_selectors);
  }

  fn has_pseudo_content(&self, pseudo: &PseudoElement) -> bool {
    self.pseudo_content.contains(pseudo)
  }

  fn has_pseudo_rules(&self, pseudo: &PseudoElement) -> bool {
    self.pseudo_buckets.contains_key(pseudo)
  }

  fn build_part_index(&mut self) {
    for (pseudo, _) in self.pseudo_buckets.iter() {
      let PseudoElement::Part(required) = pseudo else {
        continue;
      };
      let idx = self.part_pseudos.len();
      self.part_pseudos.push(PartPseudoInfo {
        pseudo: pseudo.clone(),
      });
      let key = selector_bucket_part(required.as_str());
      self.part_lookup.entry(key).or_default().push(idx);
    }
  }

  fn selector_candidates(
    &self,
    node: &DomNode,
    node_keys: NodeSelectorKeys<'_>,
    summary: Option<SelectorBloomSummaryRef<'_>>,
    quirks_mode: QuirksMode,
    out: &mut Vec<usize>,
    seen: &mut CandidateSet,
    stats: &mut CandidateStats,
    merge: &mut CandidateMergeScratch,
  ) {
    out.clear();
    seen.reset();
    merge.cursors.clear();
    merge.heap.clear();
    if !node.is_element() {
      return;
    }

    let selectors = &self.selectors;
    let mut push_list = |list: &[usize], bucket: CandidateBucket| {
      if list.is_empty() {
        return;
      }
      let cursor_idx = merge.cursors.len();
      merge.cursors.push(CandidateCursor::new(list, bucket));
      let selector_idx = list[0];
      let indexed = &selectors[selector_idx];
      merge.heap.push(CandidateHeapItem {
        rule_idx: indexed.rule_idx,
        specificity: indexed.specificity,
        selector_idx,
        bucket_rank: bucket.rank(),
        cursor_idx,
      });
    };

    if !self.by_id.is_empty() {
      if let Some(key) = node_keys.id_key {
        if let Some(list) = self.by_id.get(&key) {
          push_list(list.as_slice(), CandidateBucket::Id);
        }
      }
    }

    if !self.by_class.is_empty() {
      for key in node_keys.class_keys {
        if let Some(list) = self.by_class.get(key) {
          push_list(list.as_slice(), CandidateBucket::Class);
        }
      }
    }

    if !self.by_tag.is_empty() {
      let key = node_keys.tag_key;
      if let Some(list) = self.by_tag.get(&key) {
        push_list(list.as_slice(), CandidateBucket::Tag);
      }
      let star = selector_bucket_tag("*");
      if let Some(list) = self.by_tag.get(&star) {
        push_list(list.as_slice(), CandidateBucket::Tag);
      }
    }

    if !self.by_attr.is_empty() {
      for key in node_keys.attr_keys {
        if let Some(list) = self.by_attr.get(key) {
          push_list(list.as_slice(), CandidateBucket::Attr);
        }
      }
    }

    push_list(self.universal.as_slice(), CandidateBucket::Universal);

    if merge.cursors.len() == 1 {
      let cursor = &merge.cursors[0];
      for offset in 0..cursor.len {
        // Safety: `offset < len` ensures the pointer remains within the slice.
        let selector_idx = unsafe { *cursor.ptr.add(offset) };
        if !seen.mark_seen(selector_idx) {
          continue;
        }
        if !selector_metadata_matches_node(
          &selectors[selector_idx].metadata,
          node,
          summary,
          quirks_mode,
        ) {
          stats.pruned += 1;
          continue;
        }
        out.push(selector_idx);
        match cursor.bucket {
          CandidateBucket::Id => stats.by_id += 1,
          CandidateBucket::Class => stats.by_class += 1,
          CandidateBucket::Tag => stats.by_tag += 1,
          CandidateBucket::Attr => stats.by_attr += 1,
          CandidateBucket::Universal => stats.universal += 1,
        }
      }
      return;
    }

    while let Some(item) = merge.heap.pop() {
      let cursor = &mut merge.cursors[item.cursor_idx];
      let Some(selector_idx) = cursor.current() else {
        continue;
      };
      debug_assert_eq!(selector_idx, item.selector_idx);
      cursor.pos += 1;
      if let Some(next_idx) = cursor.current() {
        let indexed = &selectors[next_idx];
        merge.heap.push(CandidateHeapItem {
          rule_idx: indexed.rule_idx,
          specificity: indexed.specificity,
          selector_idx: next_idx,
          bucket_rank: cursor.bucket.rank(),
          cursor_idx: item.cursor_idx,
        });
      }

      if !seen.mark_seen(selector_idx) {
        continue;
      }
      if !selector_metadata_matches_node(
        &selectors[selector_idx].metadata,
        node,
        summary,
        quirks_mode,
      ) {
        stats.pruned += 1;
        continue;
      }
      out.push(selector_idx);
      match cursor.bucket {
        CandidateBucket::Id => stats.by_id += 1,
        CandidateBucket::Class => stats.by_class += 1,
        CandidateBucket::Tag => stats.by_tag += 1,
        CandidateBucket::Attr => stats.by_attr += 1,
        CandidateBucket::Universal => stats.universal += 1,
      }
    }
  }

  fn slotted_candidates(
    &self,
    node: &DomNode,
    node_keys: NodeSelectorKeys<'_>,
    summary: Option<SelectorBloomSummaryRef<'_>>,
    quirks_mode: QuirksMode,
    out: &mut Vec<usize>,
    seen: &mut CandidateSet,
    stats: &mut CandidateStats,
    merge: &mut CandidateMergeScratch,
  ) {
    out.clear();
    seen.reset();
    merge.cursors.clear();
    merge.heap.clear();
    if !node.is_element() {
      return;
    }

    let selectors = &self.slotted_selectors;
    let mut push_list = |list: &[usize], bucket: CandidateBucket| {
      if list.is_empty() {
        return;
      }
      let cursor_idx = merge.cursors.len();
      merge.cursors.push(CandidateCursor::new(list, bucket));
      let selector_idx = list[0];
      let indexed = &selectors[selector_idx];
      merge.heap.push(CandidateHeapItem {
        rule_idx: indexed.rule_idx,
        specificity: 0,
        selector_idx,
        bucket_rank: bucket.rank(),
        cursor_idx,
      });
    };

    if !self.slotted_buckets.by_id.is_empty() {
      if let Some(key) = node_keys.id_key {
        if let Some(list) = self.slotted_buckets.by_id.get(&key) {
          push_list(list.as_slice(), CandidateBucket::Id);
        }
      }
    }

    if !self.slotted_buckets.by_class.is_empty() {
      for key in node_keys.class_keys {
        if let Some(list) = self.slotted_buckets.by_class.get(key) {
          push_list(list.as_slice(), CandidateBucket::Class);
        }
      }
    }

    if !self.slotted_buckets.by_tag.is_empty() {
      let key = node_keys.tag_key;
      if let Some(list) = self.slotted_buckets.by_tag.get(&key) {
        push_list(list.as_slice(), CandidateBucket::Tag);
      }
      let star = selector_bucket_tag("*");
      if let Some(list) = self.slotted_buckets.by_tag.get(&star) {
        push_list(list.as_slice(), CandidateBucket::Tag);
      }
    }

    if !self.slotted_buckets.by_attr.is_empty() {
      for key in node_keys.attr_keys {
        if let Some(list) = self.slotted_buckets.by_attr.get(key) {
          push_list(list.as_slice(), CandidateBucket::Attr);
        }
      }
    }

    push_list(
      self.slotted_buckets.universal.as_slice(),
      CandidateBucket::Universal,
    );

    if merge.cursors.len() == 1 {
      let cursor = &merge.cursors[0];
      for offset in 0..cursor.len {
        // Safety: `offset < len` ensures the pointer remains within the slice.
        let selector_idx = unsafe { *cursor.ptr.add(offset) };
        if !seen.mark_seen(selector_idx) {
          continue;
        }
        if !selector_metadata_matches_node(
          &selectors[selector_idx].metadata,
          node,
          summary,
          quirks_mode,
        ) {
          stats.pruned += 1;
          continue;
        }
        out.push(selector_idx);
        match cursor.bucket {
          CandidateBucket::Id => stats.by_id += 1,
          CandidateBucket::Class => stats.by_class += 1,
          CandidateBucket::Tag => stats.by_tag += 1,
          CandidateBucket::Attr => stats.by_attr += 1,
          CandidateBucket::Universal => stats.universal += 1,
        }
      }
      return;
    }

    while let Some(item) = merge.heap.pop() {
      let cursor = &mut merge.cursors[item.cursor_idx];
      let Some(selector_idx) = cursor.current() else {
        continue;
      };
      debug_assert_eq!(selector_idx, item.selector_idx);
      cursor.pos += 1;
      if let Some(next_idx) = cursor.current() {
        let indexed = &selectors[next_idx];
        merge.heap.push(CandidateHeapItem {
          rule_idx: indexed.rule_idx,
          specificity: 0,
          selector_idx: next_idx,
          bucket_rank: cursor.bucket.rank(),
          cursor_idx: item.cursor_idx,
        });
      }

      if !seen.mark_seen(selector_idx) {
        continue;
      }
      if !selector_metadata_matches_node(
        &selectors[selector_idx].metadata,
        node,
        summary,
        quirks_mode,
      ) {
        stats.pruned += 1;
        continue;
      }
      out.push(selector_idx);
      match cursor.bucket {
        CandidateBucket::Id => stats.by_id += 1,
        CandidateBucket::Class => stats.by_class += 1,
        CandidateBucket::Tag => stats.by_tag += 1,
        CandidateBucket::Attr => stats.by_attr += 1,
        CandidateBucket::Universal => stats.universal += 1,
      }
    }
  }

  fn pseudo_candidates(
    &self,
    node: &DomNode,
    pseudo: &PseudoElement,
    node_keys: NodeSelectorKeys<'_>,
    summary: Option<SelectorBloomSummaryRef<'_>>,
    quirks_mode: QuirksMode,
    out: &mut Vec<usize>,
    seen: &mut CandidateSet,
    stats: &mut CandidateStats,
  ) {
    out.clear();
    seen.reset();
    if !node.is_element() {
      return;
    }

    let Some(bucket) = self.pseudo_buckets.get(pseudo) else {
      return;
    };

    let mut consider_candidate = |idx: usize, bucket: &mut u64| {
      if !selector_metadata_matches_node(
        &self.pseudo_selectors[idx].metadata,
        node,
        summary,
        quirks_mode,
      ) {
        if seen.mark_seen(idx) {
          stats.pruned += 1;
        }
        return;
      }
      if seen.mark_seen(idx) {
        out.push(idx);
        *bucket += 1;
      }
    };

    if !bucket.by_id.is_empty() {
      if let Some(key) = node_keys.id_key {
        if let Some(list) = bucket.by_id.get(&key) {
          for idx in list {
            consider_candidate(*idx, &mut stats.by_id);
          }
        }
      }
    }

    if !bucket.by_class.is_empty() {
      for key in node_keys.class_keys {
        if let Some(list) = bucket.by_class.get(key) {
          for idx in list {
            consider_candidate(*idx, &mut stats.by_class);
          }
        }
      }
    }

    if !bucket.by_tag.is_empty() {
      let key = node_keys.tag_key;
      if let Some(list) = bucket.by_tag.get(&key) {
        for idx in list {
          consider_candidate(*idx, &mut stats.by_tag);
        }
      }
      let star = selector_bucket_tag("*");
      if let Some(list) = bucket.by_tag.get(&star) {
        for idx in list {
          consider_candidate(*idx, &mut stats.by_tag);
        }
      }
    }

    if !bucket.by_attr.is_empty() {
      for key in node_keys.attr_keys {
        if let Some(list) = bucket.by_attr.get(key) {
          for idx in list {
            consider_candidate(*idx, &mut stats.by_attr);
          }
        }
      }
    }

    for idx in &bucket.universal {
      consider_candidate(*idx, &mut stats.universal);
    }
  }
}

/// Criterion benchmark harness for isolating selector candidate collection overhead.
///
/// This is intentionally `#[doc(hidden)]` because it exists purely for microbenchmarks; the
/// renderer uses the full cascade pipeline.
#[doc(hidden)]
pub struct SelectorCandidateBench<'a, 'dom> {
  index: RuleIndex<'a>,
  dom_maps: DomMaps,
  element_nodes: Vec<(usize, *const DomNode)>,
  out: Vec<usize>,
  seen: CandidateSet,
  stats: CandidateStats,
  merge: CandidateMergeScratch,
  tmp_class_keys: Vec<SelectorBucketKey>,
  tmp_attr_keys: Vec<SelectorBucketKey>,
  quirks_mode: QuirksMode,
  _dom: std::marker::PhantomData<&'dom DomNode>,
}

#[doc(hidden)]
impl<'a, 'dom> SelectorCandidateBench<'a, 'dom> {
  pub fn new(dom: &'dom DomNode, stylesheet: &'a StyleSheet, media_ctx: &MediaContext) -> Self {
    let quirks_mode = quirks_mode_for_dom(dom);
    let mut layer_order_interner = LayerOrderInterner::default();
    let collected = stylesheet.collect_style_rules(media_ctx);
    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_interner
          .intern_prefixed(&rule.layer_order, DOCUMENT_TREE_SCOPE_PREFIX),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();

    let index = RuleIndex::new(rules, quirks_mode);
    let selector_count = index.selectors.len();

    let id_map = enumerate_dom_ids(dom);
    let dom_maps = DomMaps::new(dom, id_map);

    fn collect_element_nodes(
      node: &DomNode,
      ids: &HashMap<*const DomNode, usize>,
      out: &mut Vec<(usize, *const DomNode)>,
    ) {
      if node.is_element() {
        if let Some(id) = ids.get(&(node as *const DomNode)).copied() {
          out.push((id, node as *const DomNode));
        }
      }
      for child in node.children.iter() {
        collect_element_nodes(child, ids, out);
      }
    }

    let mut element_nodes: Vec<(usize, *const DomNode)> = Vec::new();
    collect_element_nodes(dom, &dom_maps.id_map, &mut element_nodes);

    Self {
      index,
      dom_maps,
      element_nodes,
      out: Vec::new(),
      seen: CandidateSet::new(selector_count),
      stats: CandidateStats::default(),
      merge: CandidateMergeScratch::default(),
      tmp_class_keys: Vec::new(),
      tmp_attr_keys: Vec::new(),
      quirks_mode,
      _dom: std::marker::PhantomData,
    }
  }

  /// Runs `selector_candidates` for every element node `repetitions` times and returns the
  /// total number of candidates produced (for `black_box`ing).
  pub fn run_cached(&mut self, repetitions: usize) -> usize {
    let mut total = 0usize;
    for _ in 0..repetitions {
      for (node_id, ptr) in self.element_nodes.iter().copied() {
        // Safety: `ptr` points into `dom`, which is held alive by the caller for `'dom`.
        let node = unsafe { &*ptr };
        let node_keys = self.dom_maps.selector_keys(node_id);
        self.stats.reset();
        self.index.selector_candidates(
          node,
          node_keys,
          None,
          self.quirks_mode,
          &mut self.out,
          &mut self.seen,
          &mut self.stats,
          &mut self.merge,
        );
        total = total.wrapping_add(self.out.len());
      }
    }
    total
  }

  /// Baseline implementation that re-hashes selector keys from strings on every call.
  pub fn run_uncached(&mut self, repetitions: usize) -> usize {
    let mut total = 0usize;
    for _ in 0..repetitions {
      for (_node_id, ptr) in self.element_nodes.iter().copied() {
        // Safety: `ptr` points into `dom`, which is held alive by the caller for `'dom`.
        let node = unsafe { &*ptr };
        if !node.is_element() {
          continue;
        }

        self.tmp_class_keys.clear();
        self.tmp_attr_keys.clear();
        let tag_key = node.tag_name().map(selector_bucket_tag).unwrap_or_default();
        let id_key = node.get_attribute_ref("id").map(selector_bucket_id);
        if let Some(class_attr) = node.get_attribute_ref("class") {
          for cls in class_attr.split_ascii_whitespace() {
            self.tmp_class_keys.push(selector_bucket_class(cls));
          }
        }
        for (name, _) in node.attributes_iter() {
          self.tmp_attr_keys.push(selector_bucket_attr(name));
        }
        let node_keys = NodeSelectorKeys {
          tag_key,
          id_key,
          class_keys: &self.tmp_class_keys,
          attr_keys: &self.tmp_attr_keys,
        };

        self.stats.reset();
        self.index.selector_candidates(
          node,
          node_keys,
          None,
          self.quirks_mode,
          &mut self.out,
          &mut self.seen,
          &mut self.stats,
          &mut self.merge,
        );
        total = total.wrapping_add(self.out.len());
      }
    }
    total
  }
}

/// A matched rule with the specificity of the selector that applied.
#[derive(Clone)]
struct MatchedRule<'a> {
  origin: StyleOrigin,
  specificity: u32,
  order: usize,
  layer_order: Arc<[u32]>,
  declarations: Cow<'a, [Declaration]>,
  starting_style: bool,
}

fn prioritize_starting_style_rules(rules: &mut [MatchedRule<'_>]) {
  let max_non_start = rules
    .iter()
    .filter(|r| !r.starting_style)
    .map(|r| r.order)
    .max()
    .unwrap_or(0);
  if max_non_start == 0 {
    return;
  }
  for rule in rules.iter_mut() {
    if rule.starting_style {
      rule.order = rule.order.saturating_add(max_non_start.saturating_add(1));
    }
  }
}

/// A single declaration annotated with cascade ordering keys.
struct MatchedDeclaration<'a> {
  important: bool,
  origin: StyleOrigin,
  specificity: u32,
  rule_order: usize,
  decl_order: usize,
  layer_order_idx: usize,
  is_custom_property: bool,
  declaration: Cow<'a, Declaration>,
}

const INLINE_SPECIFICITY: u32 = 1 << 30;
const INLINE_RULE_ORDER: usize = usize::MAX / 2;

/// Starting-style snapshots for an element and its pseudos.
#[derive(Debug, Default, Clone)]
pub struct StartingStyleSet {
  pub base: Option<Arc<ComputedStyle>>,
  pub before: Option<Arc<ComputedStyle>>,
  pub after: Option<Arc<ComputedStyle>>,
  pub marker: Option<Arc<ComputedStyle>>,
  pub first_line: Option<Arc<ComputedStyle>>,
  pub first_letter: Option<Arc<ComputedStyle>>,
}

/// A styled DOM node with computed CSS styles
#[derive(Debug, Clone)]
pub struct StyledNode {
  /// Stable identifier for this node within the DOM traversal (pre-order index).
  pub node_id: usize,
  /// Shallow copy of the DOM node. The styled tree structure is represented by `children`;
  /// `node.children` is intentionally left empty to avoid duplicating the DOM subtree.
  pub node: DomNode,
  pub styles: Arc<ComputedStyle>,
  /// Starting-style snapshots populated when @starting-style rules are present.
  pub starting_styles: StartingStyleSet,
  /// Styles for ::before pseudo-element (if content is set)
  pub before_styles: Option<Arc<ComputedStyle>>,
  /// Styles for ::after pseudo-element (if content is set)
  pub after_styles: Option<Arc<ComputedStyle>>,
  /// Styles for ::marker pseudo-element (list items only)
  pub marker_styles: Option<Arc<ComputedStyle>>,
  /// Styles for ::first-line pseudo-element (text overrides only)
  pub first_line_styles: Option<Arc<ComputedStyle>>,
  /// Styles for ::first-letter pseudo-element (text overrides only)
  pub first_letter_styles: Option<Arc<ComputedStyle>>,
  /// Slot this light DOM node is assigned to, if any.
  pub assigned_slot: Option<crate::dom::AssignedSlot>,
  /// Slotted light DOM node ids assigned to this <slot> element.
  pub slotted_node_ids: Vec<usize>,
  pub children: Vec<StyledNode>,
}

/// Copies starting-style snapshots from a parallel styled tree into the target tree.
///
/// The traversal assumes both trees were produced from the same DOM with identical
/// pre-order numbering; when their shapes diverge the copy stops at the shortest path.
pub fn attach_starting_styles(target: &mut StyledNode, starting: &StyledNode) {
  target.starting_styles.base = starting.starting_styles.base.clone();
  target.starting_styles.before = starting.starting_styles.before.clone();
  target.starting_styles.after = starting.starting_styles.after.clone();
  target.starting_styles.marker = starting.starting_styles.marker.clone();
  target.starting_styles.first_line = starting.starting_styles.first_line.clone();
  target.starting_styles.first_letter = starting.starting_styles.first_letter.clone();

  let child_count = target.children.len().min(starting.children.len());
  for idx in 0..child_count {
    attach_starting_styles(&mut target.children[idx], &starting.children[idx]);
  }
}

fn filter_starting_rules<'a>(
  rules: Vec<CollectedRule<'a>>,
  include_starting_style: bool,
) -> Vec<CollectedRule<'a>> {
  if include_starting_style {
    rules
  } else {
    rules.into_iter().filter(|r| !r.starting_style).collect()
  }
}

fn count_styled_nodes(node: &StyledNode) -> usize {
  1 + node.children.iter().map(count_styled_nodes).sum::<usize>()
}

/// Captured container size and metadata for container query evaluation.
#[derive(Debug, Clone)]
pub struct ContainerQueryInfo {
  pub inline_size: f32,
  pub block_size: f32,
  pub container_type: ContainerType,
  pub names: Vec<String>,
  pub font_size: f32,
  pub styles: Arc<ComputedStyle>,
}

/// Map of styled-node ids to their resolved container query metrics.
#[derive(Debug, Clone)]
pub struct ContainerQueryContext {
  pub base_media: MediaContext,
  pub containers: HashMap<usize, ContainerQueryInfo>,
}

impl ContainerQueryContext {
  /// Returns true when all container conditions match against the nearest qualifying containers.
  pub fn matches(
    &self,
    node_id: usize,
    ancestor_ids: &[usize],
    conditions: &[ContainerCondition],
  ) -> bool {
    const MAX_RECURSION_DEPTH: usize = 32;
    if conditions.is_empty() {
      return true;
    }

    CONTAINER_QUERY_GUARD.with(|guard_cell| {
      let mut guard = guard_cell.borrow_mut();
      guard.clear();
      self.matches_with_guard(
        node_id,
        ancestor_ids,
        conditions,
        &mut guard,
        MAX_RECURSION_DEPTH,
      )
    })
  }

  fn matches_with_guard(
    &self,
    node_id: usize,
    ancestor_ids: &[usize],
    conditions: &[ContainerCondition],
    guard: &mut Vec<usize>,
    depth_limit: usize,
  ) -> bool {
    let ctx_ptr = self as *const _ as usize;
    CONTAINER_QUERY_MEMO.with(|memo_cell| {
      let mut memo = memo_cell.borrow_mut();
      if memo.active_ctx_ptr != Some(ctx_ptr) {
        memo.clear();
        memo.active_ctx_ptr = Some(ctx_ptr);
      }
      for condition in conditions {
        let condition_ptr = condition as *const _ as usize;
        let (name_id, support_mask) = match memo.condition_sigs.get(&condition_ptr).copied() {
          Some(sig) => (sig.name_id, sig.support_mask),
          None => {
            let support_mask = cq_condition_support_mask(condition);
            let name_id = condition
              .name
              .as_deref()
              .map(|name| memo.intern_name(name))
              .unwrap_or(0);
            memo.condition_sigs.insert(
              condition_ptr,
              ConditionSignature {
                name_id,
                support_mask,
              },
            );
            (name_id, support_mask)
          }
        };

        let key = FindContainerCacheKey {
          ctx_ptr,
          node_id,
          name_id,
          support_mask,
        };
        let container_id = if let Some(cached) = memo.find_container.get(&key) {
          *cached
        } else {
          let found = self
            .find_container_impl(
              node_id,
              ancestor_ids,
              condition.name.as_deref(),
              support_mask,
            )
            .map(|(id, _)| id);
          memo.find_container.insert(key, found);
          found
        };

        let Some(container_id) = container_id else {
          return false;
        };
        let Some(container) = self.containers.get(&container_id) else {
          return false;
        };

        if guard.len() >= depth_limit || guard.contains(&container_id) {
          return false;
        }

        guard.push(container_id);
        let matches = !condition.query_list.is_empty()
          && condition.query_list.iter().any(|query| {
            container_query_matches(
              container_id,
              query,
              container,
              self,
              ctx_ptr,
              guard,
              &mut memo,
            )
          });
        guard.pop();

        if !matches {
          return false;
        }
      }

      true
    })
  }

  fn find_container<'a>(
    &'a self,
    node_id: usize,
    ancestor_ids: &[usize],
    condition: &ContainerCondition,
  ) -> Option<(usize, &'a ContainerQueryInfo)> {
    self.find_container_impl(
      node_id,
      ancestor_ids,
      condition.name.as_deref(),
      cq_condition_support_mask(condition),
    )
  }

  fn find_container_impl<'a>(
    &'a self,
    node_id: usize,
    ancestor_ids: &[usize],
    required_name: Option<&str>,
    support_mask: u8,
  ) -> Option<(usize, &'a ContainerQueryInfo)> {
    if support_mask == 0 {
      return None;
    }

    // Per spec the query container is the nearest ancestor that establishes a container;
    // allow the current element itself if it is a container.
    for candidate in std::iter::once(node_id).chain(ancestor_ids.iter().rev().copied()) {
      let Some(info) = self.containers.get(&candidate) else {
        continue;
      };

      if (support_mask & cq_type_bit(info.container_type)) == 0 {
        continue;
      }

      if let Some(name) = required_name {
        if !info.names.iter().any(|n| n == name) {
          continue;
        }
      }

      return Some((candidate, info));
    }

    None
  }
}

/// Apply styles to a DOM tree with default viewport (desktop)
///
/// This is the main entry point for CSS cascade. It uses a default
/// desktop viewport size for media query evaluation.
pub fn apply_styles(dom: &DomNode, stylesheet: &StyleSheet) -> StyledNode {
  apply_styles_with_target(dom, stylesheet, None)
}

/// Apply styles with an explicit target fragment for :target matching.
pub fn apply_styles_with_target(
  dom: &DomNode,
  stylesheet: &StyleSheet,
  target_fragment: Option<&str>,
) -> StyledNode {
  // Use default desktop viewport
  let media_ctx = MediaContext::screen(1200.0, 800.0);
  apply_styles_with_media_and_target(dom, stylesheet, &media_ctx, target_fragment)
}

/// Apply styles to a DOM tree with a specific media context
///
/// This allows controlling viewport size and other media features
/// for responsive rendering.
pub fn apply_styles_with_media(
  dom: &DomNode,
  stylesheet: &StyleSheet,
  media_ctx: &MediaContext,
) -> StyledNode {
  apply_styles_with_media_and_target(dom, stylesheet, media_ctx, None)
}

pub fn apply_styles_with_media_and_target(
  dom: &DomNode,
  stylesheet: &StyleSheet,
  media_ctx: &MediaContext,
  target_fragment: Option<&str>,
) -> StyledNode {
  apply_styles_with_media_target_and_imports_cached(
    dom,
    stylesheet,
    media_ctx,
    target_fragment,
    None,
    None::<&str>,
    None,
    None,
    None,
    None,
  )
}

/// Apply styles with media context, optional :target, and optional import loader/base URL.
// These entry points intentionally use default hashers for style caches and run infrequently.
// Allow pedantic implicit_hasher here instead of plumbing custom hashers through the API surface.
#[allow(clippy::implicit_hasher)]
pub fn apply_styles_with_media_target_and_imports(
  dom: &DomNode,
  stylesheet: &StyleSheet,
  media_ctx: &MediaContext,
  target_fragment: Option<&str>,
  import_loader: Option<&dyn CssImportLoader>,
  base_url: Option<&str>,
  container_ctx: Option<&ContainerQueryContext>,
  container_scope: Option<&HashSet<usize>>,
  reuse_map: Option<&HashMap<usize, *const StyledNode>>,
) -> StyledNode {
  apply_styles_with_media_target_and_imports_cached(
    dom,
    stylesheet,
    media_ctx,
    target_fragment,
    import_loader,
    base_url,
    container_ctx,
    container_scope,
    reuse_map,
    None,
  )
}

/// Apply styles with optional media-query caching to share media evaluation across passes.
#[allow(clippy::needless_option_as_deref)]
// Cached variant shares the same rationale: default hashers are fine for these usize keys/maps.
#[allow(clippy::implicit_hasher)]
pub fn apply_styles_with_media_target_and_imports_cached(
  dom: &DomNode,
  stylesheet: &StyleSheet,
  media_ctx: &MediaContext,
  target_fragment: Option<&str>,
  import_loader: Option<&dyn CssImportLoader>,
  base_url: Option<&str>,
  container_ctx: Option<&ContainerQueryContext>,
  container_scope: Option<&HashSet<usize>>,
  reuse_map: Option<&HashMap<usize, *const StyledNode>>,
  mut media_cache: Option<&mut MediaQueryCache>,
) -> StyledNode {
  apply_styles_with_media_target_and_imports_cached_with_deadline(
    dom,
    stylesheet,
    media_ctx,
    target_fragment,
    import_loader,
    base_url,
    container_ctx,
    container_scope,
    reuse_map,
    media_cache.as_deref_mut(),
    None,
  )
  // A missing deadline means cancellation is disabled.
  .expect("deadline-free cascade should not fail")
}

/// Apply styles with optional media-query caching to share media evaluation across passes.
#[allow(clippy::needless_option_as_deref)]
// Cached variant shares the same rationale: default hashers are fine for these usize keys/maps.
#[allow(clippy::implicit_hasher)]
pub fn apply_styles_with_media_target_and_imports_cached_with_deadline(
  dom: &DomNode,
  stylesheet: &StyleSheet,
  media_ctx: &MediaContext,
  target_fragment: Option<&str>,
  import_loader: Option<&dyn CssImportLoader>,
  base_url: Option<&str>,
  container_ctx: Option<&ContainerQueryContext>,
  container_scope: Option<&HashSet<usize>>,
  reuse_map: Option<&HashMap<usize, *const StyledNode>>,
  mut media_cache: Option<&mut MediaQueryCache>,
  deadline: Option<&RenderDeadline>,
) -> Result<StyledNode, RenderError> {
  apply_styles_with_media_target_and_imports_cached_with_deadline_impl(
    dom,
    stylesheet,
    media_ctx,
    target_fragment,
    import_loader,
    base_url,
    container_ctx,
    container_scope,
    reuse_map,
    media_cache.as_deref_mut(),
    deadline,
    false,
  )
}

#[allow(clippy::implicit_hasher)]
fn apply_styles_with_media_target_and_imports_cached_with_deadline_impl(
  dom: &DomNode,
  stylesheet: &StyleSheet,
  media_ctx: &MediaContext,
  target_fragment: Option<&str>,
  import_loader: Option<&dyn CssImportLoader>,
  base_url: Option<&str>,
  container_ctx: Option<&ContainerQueryContext>,
  container_scope: Option<&HashSet<usize>>,
  reuse_map: Option<&HashMap<usize, *const StyledNode>>,
  mut media_cache: Option<&mut MediaQueryCache>,
  deadline: Option<&RenderDeadline>,
  include_starting_style: bool,
) -> Result<StyledNode, RenderError> {
  let profile_enabled = cascade_profile_enabled();
  let profile_start = profile_enabled.then(|| Instant::now());
  if profile_enabled {
    reset_cascade_profile();
  }
  reset_has_counters();
  reset_container_query_memo();
  let log_reuse = runtime::runtime_toggles().truthy("FASTR_LOG_CONTAINER_REUSE");
  let mut reuse_counter: usize = 0;

  // Parse user-agent stylesheet once
  let ua_stylesheet = user_agent_stylesheet();

  // Resolve imports if a loader is provided
  let author_sheet = if let Some(loader) = import_loader {
    stylesheet.resolve_imports_with_cache(
      loader,
      base_url,
      media_ctx,
      media_cache.as_deref_mut(),
    )?
  } else {
    stylesheet.clone()
  };

  let id_map = enumerate_dom_ids(dom);
  let dom_node_count = id_map.len();
  let shadow_stylesheets = collect_shadow_stylesheets(dom, &id_map)?;
  let mut dom_maps = DomMaps::new(dom, id_map);
  let slot_assignment = if dom_maps.shadow_hosts.is_empty() {
    SlotAssignment::default()
  } else {
    compute_slot_assignment_with_ids(dom, &dom_maps.id_map)
  };
  let slot_maps = build_slot_maps(dom, &slot_assignment, &dom_maps);

  // Collect applicable rules from both stylesheets
  // User-agent rules come first (lower priority)
  let ua_rules = filter_starting_rules(
    if let Some(cache) = media_cache.as_deref_mut() {
      ua_stylesheet.collect_style_rules_with_cache(media_ctx, Some(cache))
    } else {
      ua_stylesheet.collect_style_rules(media_ctx)
    },
    include_starting_style,
  );
  let author_rules = filter_starting_rules(
    if let Some(cache) = media_cache.as_deref_mut() {
      author_sheet.collect_style_rules_with_cache(media_ctx, Some(cache))
    } else {
      author_sheet.collect_style_rules(media_ctx)
    },
    include_starting_style,
  );

  let mut shadow_sheets: Vec<(usize, usize, Box<StyleSheet>)> = Vec::new();
  if !shadow_stylesheets.is_empty() {
    let mut shadow_entries: Vec<_> = shadow_stylesheets.into_iter().collect();
    shadow_entries.sort_by_key(|(shadow_root_id, _)| *shadow_root_id);
    for (shadow_root_id, sheet) in shadow_entries {
      let Some(host_id) = dom_maps.shadow_hosts.get(&shadow_root_id).copied() else {
        continue;
      };
      let resolved = if let Some(loader) = import_loader {
        sheet.resolve_imports_with_cache(loader, base_url, media_ctx, media_cache.as_deref_mut())?
      } else {
        sheet
      };
      shadow_sheets.push((host_id, shadow_root_id, Box::new(resolved)));
    }
  }

  let mut max_author_rules = author_rules.len();
  for (_host, _shadow_root_id, sheet) in &shadow_sheets {
    let count = if let Some(cache) = media_cache.as_deref_mut() {
      sheet
        .collect_style_rules_with_cache(media_ctx, Some(cache))
        .len()
    } else {
      sheet.collect_style_rules(media_ctx).len()
    };
    max_author_rules = max_author_rules.max(count);
  }
  let document_order_base = max_author_rules.saturating_add(1);
  let shadow_order_base = document_order_base.saturating_mul(2);

  let quirks_mode = quirks_mode_for_dom(dom);
  let mut layer_order_interner = LayerOrderInterner::default();
  let ua_index = RuleIndex::new(
    ua_rules
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::UserAgent,
        order,
        rule: rule.rule,
        layer_order: layer_order_interner
          .intern_prefixed(rule.layer_order.as_ref(), DOCUMENT_TREE_SCOPE_PREFIX),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect(),
    quirks_mode,
  );

  let document_author_index = RuleIndex::new(
    author_rules
      .iter()
      .enumerate()
      .map(|(idx, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order: document_order_base + idx,
        rule: rule.rule,
        layer_order: layer_order_interner
          .intern_prefixed(rule.layer_order.as_ref(), DOCUMENT_TREE_SCOPE_PREFIX),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect(),
    quirks_mode,
  );

  let mut shadow_indices: HashMap<usize, RuleIndex<'_>> = HashMap::new();
  let mut shadow_host_indices: HashMap<usize, RuleIndex<'_>> = HashMap::new();

  for (host, shadow_root_id, sheet) in &shadow_sheets {
    let collected = filter_starting_rules(
      if let Some(cache) = media_cache.as_deref_mut() {
        sheet.collect_style_rules_with_cache(media_ctx, Some(cache))
      } else {
        sheet.collect_style_rules(media_ctx)
      },
      include_starting_style,
    );
    let mut rules: Vec<CascadeRule<'_>> = Vec::new();
    let mut host_rules: Vec<CascadeRule<'_>> = Vec::new();

    for (idx, rule) in collected.iter().enumerate() {
      let cascade_rule = CascadeRule {
        origin: StyleOrigin::Author,
        order: shadow_order_base + idx,
        rule: rule.rule,
        layer_order: layer_order_interner.intern_prefixed(
          rule.layer_order.as_ref(),
          shadow_tree_scope_prefix(*shadow_root_id),
        ),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Shadow {
          shadow_root_id: *shadow_root_id,
        },
        starting_style: rule.starting_style,
      };
      if rule_targets_shadow_host(rule.rule) {
        let host_rule = CascadeRule {
          scope: RuleScope::Document,
          ..cascade_rule.clone()
        };
        host_rules.push(host_rule);
      }
      rules.push(cascade_rule);
    }

    shadow_indices.insert(*host, RuleIndex::new(rules, quirks_mode));
    if !host_rules.is_empty() {
      shadow_host_indices.insert(*host, RuleIndex::new(host_rules, quirks_mode));
    }
  }

  let rule_scopes = RuleScopes {
    ua: ua_index,
    document: document_author_index,
    shadows: shadow_indices,
    host_rules: shadow_host_indices,
    slot_maps,
    fallback_document_rules_in_shadow_scopes: true,
    quirks_mode,
  };

  let needs_selector_blooms = rule_scopes_needs_selector_bloom_summaries(&rule_scopes);
  if needs_selector_blooms && dom_maps.id_map.len() >= SELECTOR_BLOOM_MIN_NODES {
    dom_maps.ensure_selector_blooms(dom);
  }

  let counter_styles = {
    let mut registry = crate::style::counter_styles::CounterStyleRegistry::with_builtins();

    let ua_counter_styles = if let Some(cache) = media_cache.as_deref_mut() {
      ua_stylesheet.collect_counter_style_rules_with_cache(media_ctx, Some(cache))
    } else {
      ua_stylesheet.collect_counter_style_rules(media_ctx)
    };
    let author_counter_styles = if let Some(cache) = media_cache.as_deref_mut() {
      author_sheet.collect_counter_style_rules_with_cache(media_ctx, Some(cache))
    } else {
      author_sheet.collect_counter_style_rules(media_ctx)
    };

    fn register_collected(
      registry: &mut crate::style::counter_styles::CounterStyleRegistry,
      mut collected: Vec<crate::css::types::CollectedCounterStyleRule<'_>>,
    ) {
      collected.sort_by(|a, b| {
        a.layer_order
          .as_ref()
          .cmp(b.layer_order.as_ref())
          .then(a.order.cmp(&b.order))
      });
      for rule in collected {
        registry.register(rule.rule.clone());
      }
    }

    // Cascade order: UA first, then author. Within an origin, layer order and source order decide.
    register_collected(&mut registry, ua_counter_styles);
    register_collected(&mut registry, author_counter_styles);

    for (_host, _shadow_root_id, sheet) in &shadow_sheets {
      let shadow_counter_styles = if let Some(cache) = media_cache.as_deref_mut() {
        sheet.collect_counter_style_rules_with_cache(media_ctx, Some(cache))
      } else {
        sheet.collect_counter_style_rules(media_ctx)
      };
      register_collected(&mut registry, shadow_counter_styles);
    }

    std::sync::Arc::new(registry)
  };

  let font_palettes = {
    let mut registry = crate::style::font_palette::FontPaletteRegistry::default();

    let ua_palettes = if let Some(cache) = media_cache.as_deref_mut() {
      ua_stylesheet.collect_font_palette_rules_with_cache(media_ctx, Some(cache))
    } else {
      ua_stylesheet.collect_font_palette_rules(media_ctx)
    };
    let author_palettes = if let Some(cache) = media_cache.as_deref_mut() {
      author_sheet.collect_font_palette_rules_with_cache(media_ctx, Some(cache))
    } else {
      author_sheet.collect_font_palette_rules(media_ctx)
    };

    fn register_collected(
      registry: &mut crate::style::font_palette::FontPaletteRegistry,
      mut collected: Vec<crate::css::types::CollectedFontPaletteRule<'_>>,
    ) {
      collected.sort_by(|a, b| {
        a.layer_order
          .as_ref()
          .cmp(b.layer_order.as_ref())
          .then(a.order.cmp(&b.order))
      });
      for rule in collected {
        registry.register(rule.rule.clone());
      }
    }

    register_collected(&mut registry, ua_palettes);
    register_collected(&mut registry, author_palettes);

    for (_host, _shadow_root_id, sheet) in &shadow_sheets {
      let shadow_palettes = if let Some(cache) = media_cache.as_deref_mut() {
        sheet.collect_font_palette_rules_with_cache(media_ctx, Some(cache))
      } else {
        sheet.collect_font_palette_rules(media_ctx)
      };
      register_collected(&mut registry, shadow_palettes);
    }

    std::sync::Arc::new(registry)
  };

  let property_registry = {
    let mut registry = CustomPropertyRegistry::default();

    fn register_collected(
      registry: &mut CustomPropertyRegistry,
      mut collected: Vec<crate::css::types::CollectedPropertyRule<'_>>,
    ) {
      collected.sort_by(|a, b| {
        a.layer_order
          .as_ref()
          .cmp(b.layer_order.as_ref())
          .then(a.order.cmp(&b.order))
      });
      for rule in collected {
        let converted = crate::style::custom_properties::PropertyRule {
          name: rule.rule.name.clone(),
          syntax: rule.rule.syntax,
          inherits: rule.rule.inherits,
          initial_value: rule.rule.initial_value.clone(),
        };
        registry.register(converted);
      }
    }

    let ua_properties = if let Some(cache) = media_cache.as_deref_mut() {
      ua_stylesheet.collect_property_rules_with_cache(media_ctx, Some(cache))
    } else {
      ua_stylesheet.collect_property_rules(media_ctx)
    };
    register_collected(&mut registry, ua_properties);

    let author_properties = if let Some(cache) = media_cache.as_deref_mut() {
      author_sheet.collect_property_rules_with_cache(media_ctx, Some(cache))
    } else {
      author_sheet.collect_property_rules(media_ctx)
    };
    register_collected(&mut registry, author_properties);

    for (_host, _shadow_root_id, sheet) in &shadow_sheets {
      let shadow_properties = if let Some(cache) = media_cache.as_deref_mut() {
        sheet.collect_property_rules_with_cache(media_ctx, Some(cache))
      } else {
        sheet.collect_property_rules(media_ctx)
      };
      register_collected(&mut registry, shadow_properties);
    }

    std::sync::Arc::new(registry)
  };
  let initial_custom_properties = property_registry.initial_values();
  let mut base_styles = ComputedStyle::default();
  base_styles.counter_styles = counter_styles.clone();
  base_styles.font_palettes = font_palettes.clone();
  base_styles.custom_property_registry = property_registry.clone();
  base_styles.custom_properties = initial_custom_properties.clone();
  let mut base_ua_styles = ComputedStyle::default();
  base_ua_styles.counter_styles = counter_styles;
  base_ua_styles.font_palettes = font_palettes;
  base_ua_styles.custom_property_registry = property_registry.clone();
  base_ua_styles.custom_properties = initial_custom_properties;
  let has_starting_styles = include_starting_style
    && (ua_stylesheet.has_starting_style_rules()
      || author_sheet.has_starting_style_rules()
      || shadow_sheets
        .iter()
        .any(|(_, _, sheet)| sheet.has_starting_style_rules()));

  let styled = with_target_fragment(target_fragment, || {
    with_image_set_dpr(media_ctx.device_pixel_ratio, || {
      let mut selector_caches = SelectorCaches::default();
      let cache_epoch = next_selector_cache_epoch();
      selector_caches.set_epoch(cache_epoch);
      let sibling_cache = SiblingListCache::new(cache_epoch);
      let element_attr_cache = ElementAttrCache::new(cache_epoch);
      let mut max_rules = rule_scopes
        .ua
        .rules
        .len()
        .max(rule_scopes.document.rules.len());
      for index in rule_scopes.shadows.values() {
        max_rules = max_rules.max(index.rules.len());
      }
      for index in rule_scopes.host_rules.values() {
        max_rules = max_rules.max(index.rules.len());
      }
      let mut scratch = CascadeScratch::new(max_rules.max(1));
      let mut inline_style_decls = vec![None; dom_node_count + 1];
      let mut node_counter: usize = 1;
      let mut ancestor_ids: Vec<usize> = Vec::new();
      let reuse_counter_opt = if container_scope.is_some() || reuse_map.is_some() {
        Some(&mut reuse_counter)
      } else {
        None
      };
      let color_scheme_pref = media_ctx
        .prefers_color_scheme
        .unwrap_or(ColorScheme::NoPreference);
      apply_styles_internal(
        dom,
        &rule_scopes,
        None,
        &mut selector_caches,
        &mut scratch,
        &mut inline_style_decls,
        &base_styles,
        None,
        &base_ua_styles,
        16.0,
        16.0,
        Size::new(media_ctx.viewport_width, media_ctx.viewport_height),
        color_scheme_pref,
        &mut node_counter,
        &mut ancestor_ids,
        container_ctx,
        container_scope,
        reuse_map,
        reuse_counter_opt,
        &dom_maps,
        &slot_assignment,
        &sibling_cache,
        &element_attr_cache,
        deadline,
        has_starting_styles,
      )
    })
  })?;

  if let (true, Some(start)) = (profile_enabled, profile_start) {
    log_cascade_profile(start.elapsed().as_secs_f64() * 1000.0);
  }
  if log_reuse {
    fn count_nodes(node: &StyledNode) -> usize {
      1 + node.children.iter().map(count_nodes).sum::<usize>()
    }
    let total = count_nodes(&styled);
    eprintln!(
      "[container-reuse] total_nodes={} reused_nodes={} reused_pct={:.1}%",
      total,
      reuse_counter,
      if total > 0 {
        (reuse_counter as f64 / total as f64) * 100.0
      } else {
        0.0
      }
    );
  }

  Ok(styled)
}

/// Precomputed cascade context that can be reused across multiple cascade passes.
///
/// Container-query rendering performs an initial cascade before layout and then a second
/// cascade once container sizes are known. The second pass previously repeated expensive
/// setup (DOM maps, slot assignment, rule indices, selector caches). `PreparedCascade`
/// builds that shared context once and reuses it, keeping selector-cache epochs stable so
/// `:has()` / structural pseudo caches stay hot across passes.
pub(crate) struct PreparedCascade<'a> {
  dom: &'a DomNode,
  // Keep resolved stylesheets alive for the lifetime of the rule indices.
  _document_sheet: Box<StyleSheet>,
  _shadow_sheets: Vec<(usize, Box<StyleSheet>)>,
  rule_scopes: RuleScopes<'a>,
  dom_maps: DomMaps,
  needs_selector_bloom_summaries: bool,
  slot_assignment: SlotAssignment,
  base_styles: ComputedStyle,
  base_ua_styles: ComputedStyle,
  has_starting_styles: bool,
  viewport: Size,
  color_scheme_pref: ColorScheme,
  device_pixel_ratio: f32,
  selector_caches: SelectorCaches,
  sibling_cache: SiblingListCache,
  inline_style_decls: Vec<Option<Arc<[Declaration]>>>,
  element_attr_cache: ElementAttrCache,
  scratch: CascadeScratch,
}

impl<'a> PreparedCascade<'a> {
  /// Build a reusable cascade context for a `StyleSet` + immutable DOM.
  #[allow(clippy::needless_option_as_deref)]
  pub(crate) fn new_for_style_set(
    dom: &'a DomNode,
    style_set: &StyleSet,
    media_ctx: &MediaContext,
    import_loader: Option<&dyn CssImportLoader>,
    base_url: Option<&str>,
    mut media_cache: Option<&mut MediaQueryCache>,
    include_starting_style: bool,
  ) -> Result<Self, RenderError> {
    let ua_stylesheet = user_agent_stylesheet();

    let resolve_imports = |sheet: &StyleSheet,
                           loader: Option<&dyn CssImportLoader>,
                           cache: Option<&mut MediaQueryCache>|
     -> Result<StyleSheet, RenderError> {
      if let Some(loader) = loader {
        sheet.resolve_imports_with_cache(loader, base_url, media_ctx, cache)
      } else {
        Ok(sheet.clone())
      }
    };

    let document_sheet = Box::new(resolve_imports(
      &style_set.document,
      import_loader,
      media_cache.as_deref_mut(),
    )?);
    // Safety: `document_sheet` is kept alive inside `PreparedCascade`. References derived from the
    // raw pointer remain valid for the lifetime `'a` of this context.
    let document_ref: &'a StyleSheet = unsafe { &*(&*document_sheet as *const StyleSheet) };

    let mut shadow_sheets: Vec<(usize, Box<StyleSheet>)> =
      Vec::with_capacity(style_set.shadows.len());
    if !style_set.shadows.is_empty() {
      let mut shadow_entries: Vec<_> = style_set.shadows.iter().collect();
      shadow_entries.sort_by_key(|(host, _)| *host);
      for (host, sheet) in shadow_entries {
        let resolved = resolve_imports(sheet, import_loader, media_cache.as_deref_mut())?;
        shadow_sheets.push((*host, Box::new(resolved)));
      }
    }

    // Collect style rules (possibly filtering out @starting-style rules depending on the mode).
    let ua_rules = filter_starting_rules(
      if let Some(cache) = media_cache.as_deref_mut() {
        ua_stylesheet.collect_style_rules_with_cache(media_ctx, Some(cache))
      } else {
        ua_stylesheet.collect_style_rules(media_ctx)
      },
      include_starting_style,
    );
    let document_rules = filter_starting_rules(
      if let Some(cache) = media_cache.as_deref_mut() {
        document_ref.collect_style_rules_with_cache(media_ctx, Some(cache))
      } else {
        document_ref.collect_style_rules(media_ctx)
      },
      include_starting_style,
    );

    let id_map = enumerate_dom_ids(dom);
    let dom_node_count = id_map.len();
    let dom_maps = DomMaps::new(dom, id_map);
    let slot_assignment = if dom_maps.shadow_hosts.is_empty() {
      SlotAssignment::default()
    } else {
      compute_slot_assignment_with_ids(dom, &dom_maps.id_map)
    };
    let slot_maps = build_slot_maps(dom, &slot_assignment, &dom_maps);
    let mut host_to_shadow_root: HashMap<usize, usize> = HashMap::new();
    for (shadow_root_id, host_id) in dom_maps.shadow_hosts.iter() {
      host_to_shadow_root.insert(*host_id, *shadow_root_id);
    }

    let mut max_author_rules = document_rules.len();
    for (_host, sheet) in &shadow_sheets {
      // Safety: shadow sheets are boxed and kept alive by this struct.
      let sheet_ref: &'a StyleSheet = unsafe { &*(&**sheet as *const StyleSheet) };
      let count = if let Some(cache) = media_cache.as_deref_mut() {
        filter_starting_rules(
          sheet_ref.collect_style_rules_with_cache(media_ctx, Some(cache)),
          include_starting_style,
        )
        .len()
      } else {
        filter_starting_rules(
          sheet_ref.collect_style_rules(media_ctx),
          include_starting_style,
        )
        .len()
      };
      max_author_rules = max_author_rules.max(count);
    }
    let document_order_base = max_author_rules.saturating_add(1);
    let shadow_order_base = document_order_base.saturating_mul(2);

    let quirks_mode = quirks_mode_for_dom(dom);
    let mut layer_order_interner = LayerOrderInterner::default();
    let ua_index = RuleIndex::new(
      ua_rules
        .iter()
        .enumerate()
        .map(|(order, rule)| CascadeRule {
          origin: StyleOrigin::UserAgent,
          order,
          rule: rule.rule,
          layer_order: layer_order_interner
            .intern_prefixed(rule.layer_order.as_ref(), DOCUMENT_TREE_SCOPE_PREFIX),
          container_conditions: rule.container_conditions.clone(),
          scopes: rule.scopes.clone(),
          scope_signature: ScopeSignature::compute(&rule.scopes),
          scope: RuleScope::Document,
          starting_style: rule.starting_style,
        })
        .collect(),
      quirks_mode,
    );

    let document_author_index = RuleIndex::new(
      document_rules
        .iter()
        .enumerate()
        .map(|(idx, rule)| CascadeRule {
          origin: StyleOrigin::Author,
          order: document_order_base + idx,
          rule: rule.rule,
          layer_order: layer_order_interner
            .intern_prefixed(rule.layer_order.as_ref(), DOCUMENT_TREE_SCOPE_PREFIX),
          container_conditions: rule.container_conditions.clone(),
          scopes: rule.scopes.clone(),
          scope_signature: ScopeSignature::compute(&rule.scopes),
          scope: RuleScope::Document,
          starting_style: rule.starting_style,
        })
        .collect(),
      quirks_mode,
    );

    let mut shadow_indices: HashMap<usize, RuleIndex<'a>> = HashMap::new();
    let mut shadow_host_indices: HashMap<usize, RuleIndex<'a>> = HashMap::new();

    for (host, sheet) in &shadow_sheets {
      let Some(shadow_root_id) = host_to_shadow_root.get(host).copied() else {
        continue;
      };
      // Safety: shadow sheets are boxed and kept alive by this struct.
      let sheet_ref: &'a StyleSheet = unsafe { &*(&**sheet as *const StyleSheet) };
      let collected = filter_starting_rules(
        if let Some(cache) = media_cache.as_deref_mut() {
          sheet_ref.collect_style_rules_with_cache(media_ctx, Some(cache))
        } else {
          sheet_ref.collect_style_rules(media_ctx)
        },
        include_starting_style,
      );
      let mut rules: Vec<CascadeRule<'a>> = Vec::new();
      let mut host_rules: Vec<CascadeRule<'a>> = Vec::new();

      for (idx, rule) in collected.iter().enumerate() {
        let cascade_rule = CascadeRule {
          origin: StyleOrigin::Author,
          order: shadow_order_base + idx,
          rule: rule.rule,
          layer_order: layer_order_interner.intern_prefixed(
            rule.layer_order.as_ref(),
            shadow_tree_scope_prefix(shadow_root_id),
          ),
          container_conditions: rule.container_conditions.clone(),
          scopes: rule.scopes.clone(),
          scope_signature: ScopeSignature::compute(&rule.scopes),
          scope: RuleScope::Shadow { shadow_root_id },
          starting_style: rule.starting_style,
        };
        if rule_targets_shadow_host(rule.rule) {
          let host_rule = CascadeRule {
            scope: RuleScope::Document,
            ..cascade_rule.clone()
          };
          host_rules.push(host_rule);
        }
        rules.push(cascade_rule);
      }

      shadow_indices.insert(*host, RuleIndex::new(rules, quirks_mode));
      if !host_rules.is_empty() {
        shadow_host_indices.insert(*host, RuleIndex::new(host_rules, quirks_mode));
      }
    }

    let rule_scopes = RuleScopes {
      ua: ua_index,
      document: document_author_index,
      shadows: shadow_indices,
      host_rules: shadow_host_indices,
      slot_maps,
      fallback_document_rules_in_shadow_scopes: false,
      quirks_mode,
    };

    let needs_selector_bloom_summaries = rule_scopes_needs_selector_bloom_summaries(&rule_scopes)
      && dom_maps.id_map.len() >= SELECTOR_BLOOM_MIN_NODES;

    let counter_styles = {
      let mut registry = crate::style::counter_styles::CounterStyleRegistry::with_builtins();

      let ua_counter_styles = if let Some(cache) = media_cache.as_deref_mut() {
        ua_stylesheet.collect_counter_style_rules_with_cache(media_ctx, Some(cache))
      } else {
        ua_stylesheet.collect_counter_style_rules(media_ctx)
      };
      let author_counter_styles = if let Some(cache) = media_cache.as_deref_mut() {
        document_ref.collect_counter_style_rules_with_cache(media_ctx, Some(cache))
      } else {
        document_ref.collect_counter_style_rules(media_ctx)
      };

      fn register_collected(
        registry: &mut crate::style::counter_styles::CounterStyleRegistry,
        mut collected: Vec<crate::css::types::CollectedCounterStyleRule<'_>>,
      ) {
        collected.sort_by(|a, b| {
          a.layer_order
            .as_ref()
            .cmp(b.layer_order.as_ref())
            .then(a.order.cmp(&b.order))
        });
        for rule in collected {
          registry.register(rule.rule.clone());
        }
      }

      register_collected(&mut registry, ua_counter_styles);
      register_collected(&mut registry, author_counter_styles);

      for (_host, sheet) in &shadow_sheets {
        let sheet_ref: &'a StyleSheet = unsafe { &*(&**sheet as *const StyleSheet) };
        let shadow_counter_styles = if let Some(cache) = media_cache.as_deref_mut() {
          sheet_ref.collect_counter_style_rules_with_cache(media_ctx, Some(cache))
        } else {
          sheet_ref.collect_counter_style_rules(media_ctx)
        };
        register_collected(&mut registry, shadow_counter_styles);
      }

      std::sync::Arc::new(registry)
    };

    let font_palettes = {
      let mut registry = crate::style::font_palette::FontPaletteRegistry::default();

      let ua_palettes = if let Some(cache) = media_cache.as_deref_mut() {
        ua_stylesheet.collect_font_palette_rules_with_cache(media_ctx, Some(cache))
      } else {
        ua_stylesheet.collect_font_palette_rules(media_ctx)
      };
      let author_palettes = if let Some(cache) = media_cache.as_deref_mut() {
        document_ref.collect_font_palette_rules_with_cache(media_ctx, Some(cache))
      } else {
        document_ref.collect_font_palette_rules(media_ctx)
      };

      fn register_collected(
        registry: &mut crate::style::font_palette::FontPaletteRegistry,
        mut collected: Vec<crate::css::types::CollectedFontPaletteRule<'_>>,
      ) {
        collected.sort_by(|a, b| {
          a.layer_order
            .as_ref()
            .cmp(b.layer_order.as_ref())
            .then(a.order.cmp(&b.order))
        });
        for rule in collected {
          registry.register(rule.rule.clone());
        }
      }

      register_collected(&mut registry, ua_palettes);
      register_collected(&mut registry, author_palettes);

      for (_host, sheet) in &shadow_sheets {
        let sheet_ref: &'a StyleSheet = unsafe { &*(&**sheet as *const StyleSheet) };
        let shadow_palettes = if let Some(cache) = media_cache.as_deref_mut() {
          sheet_ref.collect_font_palette_rules_with_cache(media_ctx, Some(cache))
        } else {
          sheet_ref.collect_font_palette_rules(media_ctx)
        };
        register_collected(&mut registry, shadow_palettes);
      }

      std::sync::Arc::new(registry)
    };

    let property_registry = {
      let mut registry = CustomPropertyRegistry::default();

      fn register_collected(
        registry: &mut CustomPropertyRegistry,
        mut collected: Vec<crate::css::types::CollectedPropertyRule<'_>>,
      ) {
        collected.sort_by(|a, b| {
          a.layer_order
            .as_ref()
            .cmp(b.layer_order.as_ref())
            .then(a.order.cmp(&b.order))
        });
        for rule in collected {
          let converted = crate::style::custom_properties::PropertyRule {
            name: rule.rule.name.clone(),
            syntax: rule.rule.syntax,
            inherits: rule.rule.inherits,
            initial_value: rule.rule.initial_value.clone(),
          };
          registry.register(converted);
        }
      }

      let ua_properties = if let Some(cache) = media_cache.as_deref_mut() {
        ua_stylesheet.collect_property_rules_with_cache(media_ctx, Some(cache))
      } else {
        ua_stylesheet.collect_property_rules(media_ctx)
      };
      register_collected(&mut registry, ua_properties);

      let author_properties = if let Some(cache) = media_cache.as_deref_mut() {
        document_ref.collect_property_rules_with_cache(media_ctx, Some(cache))
      } else {
        document_ref.collect_property_rules(media_ctx)
      };
      register_collected(&mut registry, author_properties);

      for (_host, sheet) in &shadow_sheets {
        let sheet_ref: &'a StyleSheet = unsafe { &*(&**sheet as *const StyleSheet) };
        let shadow_properties = if let Some(cache) = media_cache.as_deref_mut() {
          sheet_ref.collect_property_rules_with_cache(media_ctx, Some(cache))
        } else {
          sheet_ref.collect_property_rules(media_ctx)
        };
        register_collected(&mut registry, shadow_properties);
      }

      std::sync::Arc::new(registry)
    };

    let initial_custom_properties = property_registry.initial_values();
    let mut base_styles = ComputedStyle::default();
    base_styles.counter_styles = counter_styles.clone();
    base_styles.font_palettes = font_palettes.clone();
    base_styles.custom_property_registry = property_registry.clone();
    base_styles.custom_properties = initial_custom_properties.clone();
    let mut base_ua_styles = ComputedStyle::default();
    base_ua_styles.counter_styles = counter_styles;
    base_ua_styles.font_palettes = font_palettes;
    base_ua_styles.custom_property_registry = property_registry.clone();
    base_ua_styles.custom_properties = initial_custom_properties;

    let has_starting_styles = include_starting_style
      && (ua_stylesheet.has_starting_style_rules()
        || document_ref.has_starting_style_rules()
        || shadow_sheets
          .iter()
          .any(|(_, sheet)| sheet.has_starting_style_rules()));

    let device_pixel_ratio = media_ctx.device_pixel_ratio;
    let viewport = Size::new(media_ctx.viewport_width, media_ctx.viewport_height);
    let color_scheme_pref = media_ctx
      .prefers_color_scheme
      .unwrap_or(ColorScheme::NoPreference);

    let mut selector_caches = SelectorCaches::default();
    let cache_epoch = next_selector_cache_epoch();
    selector_caches.set_epoch(cache_epoch);
    let sibling_cache = SiblingListCache::new(cache_epoch);
    let element_attr_cache = ElementAttrCache::new(cache_epoch);

    let mut max_rules = rule_scopes
      .ua
      .rules
      .len()
      .max(rule_scopes.document.rules.len());
    for index in rule_scopes.shadows.values() {
      max_rules = max_rules.max(index.rules.len());
    }
    for index in rule_scopes.host_rules.values() {
      max_rules = max_rules.max(index.rules.len());
    }
    let scratch = CascadeScratch::new(max_rules.max(1));
    let inline_style_decls = vec![None; dom_node_count + 1];

    Ok(Self {
      dom,
      _document_sheet: document_sheet,
      _shadow_sheets: shadow_sheets,
      rule_scopes,
      dom_maps,
      needs_selector_bloom_summaries,
      slot_assignment,
      base_styles,
      base_ua_styles,
      has_starting_styles,
      viewport,
      color_scheme_pref,
      device_pixel_ratio,
      selector_caches,
      sibling_cache,
      inline_style_decls,
      element_attr_cache,
      scratch,
    })
  }

  /// Apply styles using the stored rule indices, DOM maps, and reused selector caches.
  #[allow(clippy::implicit_hasher)]
  pub(crate) fn apply(
    &mut self,
    target_fragment: Option<&str>,
    container_ctx: Option<&ContainerQueryContext>,
    container_scope: Option<&HashSet<usize>>,
    reuse_map: Option<&HashMap<usize, *const StyledNode>>,
    deadline: Option<&RenderDeadline>,
  ) -> Result<StyledNode, RenderError> {
    let profile_enabled = cascade_profile_enabled();
    let profile_start = profile_enabled.then(|| Instant::now());
    if profile_enabled {
      reset_cascade_profile();
    }
    if self.needs_selector_bloom_summaries {
      self.dom_maps.ensure_selector_blooms(self.dom);
    }
    reset_has_counters();
    reset_container_query_memo();
    self.scratch.scope_cache.clear();
    self.element_attr_cache.clear();
    let log_reuse = runtime::runtime_toggles().truthy("FASTR_LOG_CONTAINER_REUSE");
    let mut reuse_counter: usize = 0;

    let styled = with_target_fragment(target_fragment, || {
      with_image_set_dpr(self.device_pixel_ratio, || {
        let mut node_counter: usize = 1;
        let mut ancestor_ids: Vec<usize> = Vec::new();
        let reuse_counter_opt = if container_scope.is_some() || reuse_map.is_some() {
          Some(&mut reuse_counter)
        } else {
          None
        };
        apply_styles_internal(
          self.dom,
          &self.rule_scopes,
          None,
          &mut self.selector_caches,
          &mut self.scratch,
          &mut self.inline_style_decls,
          &self.base_styles,
          None,
          &self.base_ua_styles,
          16.0,
          16.0,
          self.viewport,
          self.color_scheme_pref,
          &mut node_counter,
          &mut ancestor_ids,
          container_ctx,
          container_scope,
          reuse_map,
          reuse_counter_opt,
          &self.dom_maps,
          &self.slot_assignment,
          &self.sibling_cache,
          &self.element_attr_cache,
          deadline,
          self.has_starting_styles,
        )
      })
    })?;

    if let (true, Some(start)) = (profile_enabled, profile_start) {
      log_cascade_profile(start.elapsed().as_secs_f64() * 1000.0);
    }
    if log_reuse {
      fn count_nodes(node: &StyledNode) -> usize {
        1 + node.children.iter().map(count_nodes).sum::<usize>()
      }
      let total = count_nodes(&styled);
      eprintln!(
        "[container-reuse] total_nodes={} reused_nodes={} reused_pct={:.1}%",
        total,
        reuse_counter,
        if total > 0 {
          (reuse_counter as f64 / total as f64) * 100.0
        } else {
          0.0
        }
      );
    }

    Ok(styled)
  }
}

/// Apply styles from a `StyleSet` with optional media-query caching to share media evaluation across passes.
#[allow(clippy::implicit_hasher)]
pub fn apply_style_set_with_media_target_and_imports(
  dom: &DomNode,
  style_set: &StyleSet,
  media_ctx: &MediaContext,
  target_fragment: Option<&str>,
  import_loader: Option<&dyn CssImportLoader>,
  base_url: Option<&str>,
  container_ctx: Option<&ContainerQueryContext>,
  container_scope: Option<&HashSet<usize>>,
  reuse_map: Option<&HashMap<usize, *const StyledNode>>,
) -> StyledNode {
  apply_style_set_with_media_target_and_imports_cached(
    dom,
    style_set,
    media_ctx,
    target_fragment,
    import_loader,
    base_url,
    container_ctx,
    container_scope,
    reuse_map,
    None,
  )
}

/// Apply styles from a `StyleSet` with optional media-query caching to share media evaluation across passes.
#[allow(clippy::needless_option_as_deref)]
#[allow(clippy::implicit_hasher)]
pub fn apply_style_set_with_media_target_and_imports_cached(
  dom: &DomNode,
  style_set: &StyleSet,
  media_ctx: &MediaContext,
  target_fragment: Option<&str>,
  import_loader: Option<&dyn CssImportLoader>,
  base_url: Option<&str>,
  container_ctx: Option<&ContainerQueryContext>,
  container_scope: Option<&HashSet<usize>>,
  reuse_map: Option<&HashMap<usize, *const StyledNode>>,
  mut media_cache: Option<&mut MediaQueryCache>,
) -> StyledNode {
  apply_style_set_with_media_target_and_imports_cached_with_deadline(
    dom,
    style_set,
    media_ctx,
    target_fragment,
    import_loader,
    base_url,
    container_ctx,
    container_scope,
    reuse_map,
    media_cache.as_deref_mut(),
    None,
  )
  // A missing deadline means cancellation is disabled.
  .expect("deadline-free cascade should not fail")
}

/// Apply styles from a `StyleSet` with optional media-query caching to share media evaluation across passes.
#[allow(clippy::needless_option_as_deref)]
#[allow(clippy::implicit_hasher)]
pub fn apply_style_set_with_media_target_and_imports_cached_with_deadline(
  dom: &DomNode,
  style_set: &StyleSet,
  media_ctx: &MediaContext,
  target_fragment: Option<&str>,
  import_loader: Option<&dyn CssImportLoader>,
  base_url: Option<&str>,
  container_ctx: Option<&ContainerQueryContext>,
  container_scope: Option<&HashSet<usize>>,
  reuse_map: Option<&HashMap<usize, *const StyledNode>>,
  mut media_cache: Option<&mut MediaQueryCache>,
  deadline: Option<&RenderDeadline>,
) -> Result<StyledNode, RenderError> {
  apply_style_set_with_media_target_and_imports_cached_with_deadline_impl(
    dom,
    style_set,
    media_ctx,
    target_fragment,
    import_loader,
    base_url,
    container_ctx,
    container_scope,
    reuse_map,
    media_cache.as_deref_mut(),
    deadline,
    false,
  )
}

/// Apply starting-style rules from a `StyleSet`, preserving import caching and deadlines.
#[allow(clippy::needless_option_as_deref)]
#[allow(clippy::implicit_hasher)]
pub fn apply_starting_style_set_with_media_target_and_imports_cached_with_deadline(
  dom: &DomNode,
  style_set: &StyleSet,
  media_ctx: &MediaContext,
  target_fragment: Option<&str>,
  import_loader: Option<&dyn CssImportLoader>,
  base_url: Option<&str>,
  container_ctx: Option<&ContainerQueryContext>,
  container_scope: Option<&HashSet<usize>>,
  reuse_map: Option<&HashMap<usize, *const StyledNode>>,
  mut media_cache: Option<&mut MediaQueryCache>,
  deadline: Option<&RenderDeadline>,
) -> Result<StyledNode, RenderError> {
  apply_style_set_with_media_target_and_imports_cached_with_deadline_impl(
    dom,
    style_set,
    media_ctx,
    target_fragment,
    import_loader,
    base_url,
    container_ctx,
    container_scope,
    reuse_map,
    media_cache.as_deref_mut(),
    deadline,
    true,
  )
}

#[allow(clippy::needless_option_as_deref)]
#[allow(clippy::implicit_hasher)]
fn apply_style_set_with_media_target_and_imports_cached_with_deadline_impl(
  dom: &DomNode,
  style_set: &StyleSet,
  media_ctx: &MediaContext,
  target_fragment: Option<&str>,
  import_loader: Option<&dyn CssImportLoader>,
  base_url: Option<&str>,
  container_ctx: Option<&ContainerQueryContext>,
  container_scope: Option<&HashSet<usize>>,
  reuse_map: Option<&HashMap<usize, *const StyledNode>>,
  mut media_cache: Option<&mut MediaQueryCache>,
  deadline: Option<&RenderDeadline>,
  include_starting_style: bool,
) -> Result<StyledNode, RenderError> {
  let mut prepared = PreparedCascade::new_for_style_set(
    dom,
    style_set,
    media_ctx,
    import_loader,
    base_url,
    media_cache.as_deref_mut(),
    include_starting_style,
  )?;

  prepared.apply(
    target_fragment,
    container_ctx,
    container_scope,
    reuse_map,
    deadline,
  )
}

fn append_presentational_hints<'a>(
  node: &DomNode,
  ancestors: &[&DomNode],
  parent_direction: Direction,
  tree_scope_prefix: u32,
  scratch: &mut CascadeScratch,
  matching_rules: &mut Vec<MatchedRule<'a>>,
) {
  // Presentational hints behave like an author rule that sits below any stylesheet/inline rule in
  // the same tree scope. Represent them with a "tree-scope-only" layer order sentinel so they sort
  // below prefixed layer vectors (which always include at least one additional layer component).
  let layer_order = scratch.presentational_hint_layer_order(tree_scope_prefix);
  if let Some(presentational_rule) =
    dir_presentational_hint(node, parent_direction, &layer_order, 0)
  {
    matching_rules.push(presentational_rule);
  }
  if let Some(presentational_rule) = list_type_presentational_hint(node, &layer_order, 1) {
    matching_rules.push(presentational_rule);
  }
  if let Some(presentational_rule) = alignment_presentational_hint(node, &layer_order, 2) {
    matching_rules.push(presentational_rule);
  }
  if let Some(presentational_rule) = dimension_presentational_hint(node, &layer_order, 3) {
    matching_rules.push(presentational_rule);
  }
  if let Some(presentational_rule) = bgcolor_presentational_hint(node, &layer_order, 4) {
    matching_rules.push(presentational_rule);
  }
  if let Some(presentational_rule) = border_presentational_hint(node, ancestors, &layer_order, 5) {
    matching_rules.push(presentational_rule);
  }
  if let Some(presentational_rule) = bordercolor_presentational_hint(node, &layer_order, 6) {
    matching_rules.push(presentational_rule);
  }
  if let Some(presentational_rule) = cellspacing_presentational_hint(node, &layer_order, 7) {
    matching_rules.push(presentational_rule);
  }
  if let Some(presentational_rule) =
    cellpadding_presentational_hint(node, ancestors, &layer_order, 8)
  {
    matching_rules.push(presentational_rule);
  }
  if let Some(presentational_rule) = replaced_alignment_presentational_hint(node, &layer_order, 9) {
    matching_rules.push(presentational_rule);
  }
  if let Some(presentational_rule) = nowrap_presentational_hint(node, &layer_order, 10) {
    matching_rules.push(presentational_rule);
  }
  if let Some(presentational_rule) = hidden_presentational_hint(node, &layer_order, 11) {
    matching_rules.push(presentational_rule);
  }
}

fn ua_default_rules(
  node: &DomNode,
  parent_direction: Direction,
  layer_order: &Arc<[u32]>,
) -> Vec<MatchedRule<'static>> {
  let mut rules = Vec::new();
  let tag = match node.tag_name() {
    Some(t) => t,
    None => return rules,
  };
  // UA default rules should participate in the same tree-scope-prefixed layer ordering as UA
  // stylesheet rules so higher-order UA defaults (e.g. link pseudo-state overrides) can outrank
  // baseline UA stylesheet declarations.
  let layer_order = layer_order.clone();

  let mut add_rule = |decls: Cow<'static, [Declaration]>, order: usize| {
    rules.push(MatchedRule {
      origin: StyleOrigin::UserAgent,
      specificity: 1, // tag selector specificity
      order,
      layer_order: layer_order.clone(),
      declarations: decls,
      starting_style: false,
    });
  };

  if tag.eq_ignore_ascii_case("a")
    || tag.eq_ignore_ascii_case("area")
    || tag.eq_ignore_ascii_case("link")
  {
    if node.get_attribute_ref("href").is_some() {
      let is_true = |name: &str| {
        node
          .get_attribute_ref(name)
          .map(|v| v.eq_ignore_ascii_case("true"))
          .unwrap_or(false)
      };
      let specificity = (1 << 10) + 1; // tag selector + pseudo-class weight
      let base_order = 1000;

      if is_true("data-fastr-visited") {
        rules.push(MatchedRule {
          origin: StyleOrigin::UserAgent,
          specificity,
          order: base_order + 1,
          layer_order: layer_order.clone(),
          declarations: cached_declarations(&UA_LINK_VISITED_DECLS, "color: rgb(85, 26, 139);"),
          starting_style: false,
        });
      }

      if is_true("data-fastr-active") {
        rules.push(MatchedRule {
          origin: StyleOrigin::UserAgent,
          specificity,
          order: base_order + 2,
          layer_order: layer_order.clone(),
          declarations: cached_declarations(&UA_LINK_ACTIVE_DECLS, "color: rgb(255, 0, 0);"),
          starting_style: false,
        });
      }

      if is_true("data-fastr-hover") {
        rules.push(MatchedRule {
          origin: StyleOrigin::UserAgent,
          specificity,
          order: base_order + 3,
          layer_order: layer_order.clone(),
          declarations: cached_declarations(&UA_LINK_HOVER_DECLS, "color: rgb(255, 0, 0);"),
          starting_style: false,
        });
      }

      if is_true("data-fastr-focus") {
        rules.push(MatchedRule {
          origin: StyleOrigin::UserAgent,
          specificity,
          order: base_order + 4,
          layer_order: layer_order.clone(),
          declarations: cached_declarations(
            &UA_LINK_FOCUS_DECLS,
            "outline: 1px dotted rgb(0, 0, 0); outline-offset: 2px;",
          ),
          starting_style: false,
        });
      }
    }
  } else if tag.eq_ignore_ascii_case("bdi") && node.get_attribute_ref("dir").is_none() {
    let resolved = resolve_first_strong_direction(node).map(|d| match d {
      TextDirection::Ltr => crate::style::types::Direction::Ltr,
      TextDirection::Rtl => crate::style::types::Direction::Rtl,
    });
    let dir_value = match resolved.unwrap_or(parent_direction) {
      crate::style::types::Direction::Rtl => "rtl",
      crate::style::types::Direction::Ltr => "ltr",
    };
    let decls = if dir_value == "rtl" {
      cached_declarations(&UA_BDI_RTL_DECLS, "unicode-bidi: isolate; direction: rtl;")
    } else {
      cached_declarations(&UA_BDI_LTR_DECLS, "unicode-bidi: isolate; direction: ltr;")
    };
    add_rule(decls, 0);
  } else if tag.eq_ignore_ascii_case("bdo") && node.get_attribute_ref("dir").is_none() {
    add_rule(
      cached_declarations(
        &UA_BDO_DECLS,
        "unicode-bidi: bidi-override; direction: ltr;",
      ),
      0,
    );
  } else if tag.eq_ignore_ascii_case("textarea") {
    add_rule(
      cached_declarations(
        &UA_TEXTAREA_DECLS,
        "font: inherit; color: inherit; border: 2px inset rgb(204,204,204); background: white; padding: 2px 3px; box-sizing: border-box; display: inline-block; cursor: text;",
      ),
      0,
    );
  } else if tag.eq_ignore_ascii_case("input") {
    let input_type = node.get_attribute_ref("type").unwrap_or("text");
    if input_type.eq_ignore_ascii_case("hidden") {
      rules.push(MatchedRule {
        origin: StyleOrigin::UserAgent,
        specificity: 11, // input[type="hidden"] should outrank generic UA form rules
        order: 0,
        layer_order: layer_order.clone(),
        declarations: cached_declarations(&UA_INPUT_HIDDEN_DECLS, "display: none;"),
        starting_style: false,
      });
      return rules;
    }
    add_rule(
      cached_declarations(
        &UA_INPUT_BASE_DECLS,
        "font: inherit; color: inherit; border: 2px inset rgb(204,204,204); background: white; padding: 2px 3px; box-sizing: border-box; display: inline-block;",
      ),
      0,
    );
    if input_type.eq_ignore_ascii_case("text")
      || input_type.eq_ignore_ascii_case("search")
      || input_type.eq_ignore_ascii_case("email")
      || input_type.eq_ignore_ascii_case("url")
      || input_type.eq_ignore_ascii_case("tel")
      || input_type.eq_ignore_ascii_case("password")
      || input_type.eq_ignore_ascii_case("number")
    {
      add_rule(
        cached_declarations(&UA_INPUT_TEXT_CURSOR_DECLS, "cursor: text;"),
        1,
      );
    } else if input_type.eq_ignore_ascii_case("button")
      || input_type.eq_ignore_ascii_case("submit")
      || input_type.eq_ignore_ascii_case("reset")
    {
      add_rule(
        cached_declarations(&UA_INPUT_POINTER_CURSOR_DECLS, "cursor: pointer;"),
        1,
      );
    }
  } else if tag.eq_ignore_ascii_case("select") || tag.eq_ignore_ascii_case("button") {
    add_rule(
      cached_declarations(
        &UA_INPUT_BASE_DECLS,
        "font: inherit; color: inherit; border: 2px inset rgb(204,204,204); background: white; padding: 2px 3px; box-sizing: border-box; display: inline-block;",
      ),
      0,
    );
  }

  rules
}

struct NodeBaseStyles {
  styles: ComputedStyle,
  ua_styles: ComputedStyle,
  current_root_font_size: f32,
  current_ua_root_font_size: f32,
}

fn scope_rule_index<'a>(
  scopes: &'a RuleScopes<'a>,
  scope_host: Option<usize>,
) -> Option<&'a RuleIndex<'a>> {
  match scope_host {
    Some(host) => scopes.shadows.get(&host).or_else(|| {
      scopes
        .fallback_document_rules_in_shadow_scopes
        .then_some(&scopes.document)
    }),
    None => Some(&scopes.document),
  }
}

// Return the rule index for the current scope along with whether shadow-host context
// should be provided during selector matching.
fn scope_rule_index_with_shadow_host<'a>(
  scopes: &'a RuleScopes<'a>,
  scope_host: Option<usize>,
) -> Option<(&'a RuleIndex<'a>, bool)> {
  match scope_host {
    Some(host) => scopes
      .shadows
      .get(&host)
      .map(|index| (index, true))
      .or_else(|| {
        scopes
          .fallback_document_rules_in_shadow_scopes
          .then_some((&scopes.document, false))
      }),
    None => Some((&scopes.document, false)),
  }
}

fn scope_has_pseudo_rules(
  scopes: &RuleScopes<'_>,
  scope_host: Option<usize>,
  node_id: usize,
  pseudo: &PseudoElement,
) -> bool {
  if scopes.ua.has_pseudo_rules(pseudo) {
    return true;
  }
  if let Some(base) = scope_rule_index(scopes, scope_host) {
    if base.has_pseudo_rules(pseudo) {
      return true;
    }
  }
  if scopes.fallback_document_rules_in_shadow_scopes {
    if let Some(host) = scope_host {
      if scopes.shadows.contains_key(&host) && scopes.document.has_pseudo_rules(pseudo) {
        return true;
      }
    }
  }
  if let Some(host) = scopes.host_rules.get(&node_id) {
    if host.has_pseudo_rules(pseudo) {
      return true;
    }
  }
  false
}

fn scope_has_pseudo_content(
  scopes: &RuleScopes<'_>,
  scope_host: Option<usize>,
  node_id: usize,
  pseudo: &PseudoElement,
) -> bool {
  if scopes.ua.has_pseudo_content(pseudo) {
    return true;
  }
  if let Some(base) = scope_rule_index(scopes, scope_host) {
    if base.has_pseudo_content(pseudo) {
      return true;
    }
  }
  if scopes.fallback_document_rules_in_shadow_scopes {
    if let Some(host) = scope_host {
      if scopes.shadows.contains_key(&host) && scopes.document.has_pseudo_content(pseudo) {
        return true;
      }
    }
  }
  if let Some(host) = scopes.host_rules.get(&node_id) {
    if host.has_pseudo_content(pseudo) {
      return true;
    }
  }
  false
}

fn part_names(node: &DomNode) -> Vec<CssString> {
  let Some(attr) = node.get_attribute_ref("part") else {
    return Vec::new();
  };

  let mut names = Vec::new();
  let mut seen: HashSet<&str> = HashSet::new();
  for token in attr.split_ascii_whitespace() {
    if seen.insert(token) {
      names.push(CssString::from(token));
    }
  }

  names
}

fn exported_part_names(
  host: &DomNode,
  names: &[CssString],
  dom_maps: &DomMaps,
) -> Option<Vec<CssString>> {
  let Some(mappings) = dom_maps.exportparts_for(host) else {
    return None;
  };

  let mut exported = Vec::new();
  let mut seen: HashSet<&str> = HashSet::new();
  for name in names {
    let target = name.as_str();
    for (internal, alias) in mappings.iter() {
      if internal == target && seen.insert(alias.as_str()) {
        exported.push(CssString::from(alias.as_str()));
      }
    }
  }

  Some(exported)
}

fn containing_scope_host_id(dom_maps: &DomMaps, node: &DomNode) -> Option<usize> {
  dom_maps.id_map.get(&(node as *const DomNode)).copied()
}

fn match_part_rules<'a>(
  node: &DomNode,
  ancestors: &[&DomNode],
  ancestor_bloom: Option<&selectors::bloom::BloomFilter>,
  scopes: &'a RuleScopes<'a>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  dom_maps: &DomMaps,
  sibling_cache: &SiblingListCache,
  element_attr_cache: &'a ElementAttrCache,
  slot_map_for_host: &dyn Fn(usize) -> Option<&'a SlotAssignmentMap<'a>>,
  current_slot_map: Option<&SlotAssignmentMap<'a>>,
) -> Vec<MatchedRule<'a>> {
  if !ancestors
    .iter()
    .any(|ancestor| matches!(ancestor.node_type, DomNodeType::ShadowRoot { .. }))
  {
    return Vec::new();
  }

  let has_part_rules = !scopes.document.part_pseudos.is_empty()
    || scopes
      .shadows
      .values()
      .any(|index| !index.part_pseudos.is_empty());
  if !has_part_rules {
    return Vec::new();
  }

  let mut names = part_names(node);
  if names.is_empty() {
    return Vec::new();
  }

  let mut matched: Vec<MatchedRule<'a>> = Vec::new();
  let mut matched_by_order: HashMap<usize, usize> = HashMap::new();

  let mut idx = ancestors.len();
  while idx > 0 {
    idx -= 1;
    if !matches!(ancestors[idx].node_type, DomNodeType::ShadowRoot { .. }) {
      continue;
    }
    if idx == 0 {
      break;
    }

    let host_idx = idx - 1;
    let host = ancestors[host_idx];
    let host_ancestors = &ancestors[..host_idx];

    let exported = exported_part_names(host, &names, dom_maps);
    let exported_some = exported.is_some();
    let mapped_names = exported.unwrap_or_default();
    let visible_names: &[CssString] = if exported_some {
      mapped_names.as_slice()
    } else {
      names.as_slice()
    };

    let scope_host = containing_scope_host_id(dom_maps, host);
    if let Some((rules, allow_shadow_host)) = scope_rule_index_with_shadow_host(scopes, scope_host)
    {
      if rules.part_pseudos.is_empty() {
        names = mapped_names;
        if names.is_empty() {
          break;
        }
        continue;
      }

      scratch.part_candidates.clear();
      scratch.part_seen.reset();
      for name in visible_names.iter() {
        let key = selector_bucket_part(name.as_str());
        if let Some(list) = rules.part_lookup.get(&key) {
          for &idx in list {
            if scratch.part_seen.insert(idx) {
              scratch.part_candidates.push(idx);
            }
          }
        }
      }

      if !scratch.part_candidates.is_empty() {
        let mut name_set: HashSet<&str> = HashSet::new();
        for name in visible_names.iter() {
          name_set.insert(name.as_str());
        }

        let slot_map = match scope_host {
          Some(host_id) => slot_map_for_host(host_id),
          None => current_slot_map,
        };

        let candidates = scratch.part_candidates.clone();
        for idx in candidates {
          let info = &rules.part_pseudos[idx];
          let required = match &info.pseudo {
            PseudoElement::Part(name) => name.as_str(),
            _ => continue,
          };
          if !name_set.contains(required) {
            continue;
          }
          let host_id = scope_host.unwrap_or(0);
          let host_keys = dom_maps.selector_keys(host_id);
          // allow_shadow_host prevents document-scope ::part selectors from exposing :host context.
          let part_matches = find_pseudo_element_rules(
            host,
            host_id,
            host_keys,
            rules,
            selector_caches,
            scratch,
            host_ancestors,
            ancestor_bloom,
            slot_map,
            dom_maps.selector_blooms(),
            Some(element_attr_cache),
            sibling_cache,
            &info.pseudo,
            allow_shadow_host,
            scopes.quirks_mode,
          );
          for rule in part_matches {
            if let Some(pos) = matched_by_order.get(&rule.order).copied() {
              if rule.specificity > matched[pos].specificity {
                matched[pos].specificity = rule.specificity;
              }
            }
          }
        }
      }
    }
    names = mapped_names;
    if names.is_empty() {
      break;
    }
  }

  scratch.part_seen.reset();

  matched
}

fn collect_matching_rules<'a>(
  node: &DomNode,
  scopes: &'a RuleScopes<'a>,
  scope_host: Option<usize>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  ancestors: &[&DomNode],
  ancestor_bloom: Option<&selectors::bloom::BloomFilter>,
  ancestor_ids: &[usize],
  node_id: usize,
  container_ctx: Option<&ContainerQueryContext>,
  dom_maps: &DomMaps,
  slot_assignment: &SlotAssignment,
  sibling_cache: &SiblingListCache,
  element_attr_cache: &'a ElementAttrCache,
) -> Result<Vec<MatchedRule<'a>>, RenderError> {
  let current_shadow = dom_maps.containing_shadow_root(node_id);
  let slot_map_for_host = |host_id: usize| -> Option<&SlotAssignmentMap<'a>> {
    dom_maps
      .shadow_hosts
      .iter()
      .find_map(|(shadow_root, host)| {
        if *host == host_id {
          Some(shadow_root)
        } else {
          None
        }
      })
      .and_then(|shadow| scopes.slot_maps.get(&shadow))
  };
  let current_slot_map = current_shadow.and_then(|shadow| scopes.slot_maps.get(&shadow));
  let mut matches = find_matching_rules(
    node,
    &scopes.ua,
    selector_caches,
    scratch,
    ancestors,
    ancestor_bloom,
    ancestor_ids,
    node_id,
    container_ctx,
    dom_maps,
    slot_assignment,
    current_slot_map,
    Some(element_attr_cache),
    sibling_cache,
    false,
    scopes.quirks_mode,
  )?;

  if let Some((base, allow_shadow_host)) = scope_rule_index_with_shadow_host(scopes, scope_host) {
    let base_slot_map = match scope_host {
      Some(host) => slot_map_for_host(host),
      None => current_slot_map,
    };
    matches.extend(find_matching_rules(
      node,
      base,
      selector_caches,
      scratch,
      ancestors,
      ancestor_bloom,
      ancestor_ids,
      node_id,
      container_ctx,
      dom_maps,
      slot_assignment,
      base_slot_map,
      Some(element_attr_cache),
      sibling_cache,
      allow_shadow_host,
      scopes.quirks_mode,
    )?);
  }

  // Legacy compatibility: older cascade entrypoints treated shadow DOM like regular DOM, so
  // document styles applied inside shadow roots. Keep that behavior behind a flag while also
  // applying shadow-scoped styles.
  if scopes.fallback_document_rules_in_shadow_scopes {
    if let Some(host) = scope_host {
      if scopes.shadows.contains_key(&host) {
        matches.extend(find_matching_rules(
          node,
          &scopes.document,
          selector_caches,
          scratch,
          ancestors,
          ancestor_bloom,
          ancestor_ids,
          node_id,
          container_ctx,
          dom_maps,
          slot_assignment,
          current_slot_map,
          Some(element_attr_cache),
          sibling_cache,
          false,
          scopes.quirks_mode,
        )?);
      }
    }
  }

  if let Some(host) = scopes.host_rules.get(&node_id) {
    let host_slot_map = slot_map_for_host(node_id);
    matches.extend(find_matching_rules(
      node,
      host,
      selector_caches,
      scratch,
      ancestors,
      ancestor_bloom,
      ancestor_ids,
      node_id,
      container_ctx,
      dom_maps,
      slot_assignment,
      host_slot_map,
      Some(element_attr_cache),
      sibling_cache,
      true,
      scopes.quirks_mode,
    )?);
  }

  // Shadow `::slotted(...)` rules apply to light-DOM nodes assigned to slots.
  if let Some(slot) = slot_assignment.node_to_slot.get(&node_id) {
    if let Some(host_id) = dom_maps.shadow_hosts.get(&slot.shadow_root_id).copied() {
      if let Some(shadow_index) = scopes.shadows.get(&host_id) {
        matches.extend(find_matching_rules(
          node,
          shadow_index,
          selector_caches,
          scratch,
          ancestors,
          ancestor_bloom,
          ancestor_ids,
          node_id,
          container_ctx,
          dom_maps,
          slot_assignment,
          scopes.slot_maps.get(&slot.shadow_root_id),
          Some(element_attr_cache),
          sibling_cache,
          true,
          scopes.quirks_mode,
        )?);
      }
    }
  }

  matches.extend(match_part_rules(
    node,
    ancestors,
    ancestor_bloom,
    scopes,
    selector_caches,
    scratch,
    dom_maps,
    sibling_cache,
    element_attr_cache,
    &slot_map_for_host,
    current_slot_map,
  ));

  Ok(matches)
}

fn collect_pseudo_matching_rules<'a>(
  node: &DomNode,
  scopes: &'a RuleScopes<'a>,
  scope_host: Option<usize>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  ancestors: &[&DomNode],
  ancestor_bloom: Option<&selectors::bloom::BloomFilter>,
  node_id: usize,
  dom_maps: &DomMaps,
  sibling_cache: &SiblingListCache,
  element_attr_cache: &'a ElementAttrCache,
  pseudo: &PseudoElement,
) -> Vec<MatchedRule<'a>> {
  let current_shadow = dom_maps.containing_shadow_root(node_id);
  let node_keys = dom_maps.selector_keys(node_id);
  let slot_map_for_host = |host_id: usize| -> Option<&SlotAssignmentMap> {
    dom_maps
      .shadow_hosts
      .iter()
      .find_map(|(shadow_root, host)| {
        if *host == host_id {
          Some(shadow_root)
        } else {
          None
        }
      })
      .and_then(|shadow| scopes.slot_maps.get(&shadow))
  };
  let current_slot_map = current_shadow.and_then(|shadow| scopes.slot_maps.get(&shadow));
  let base_slot_map = match scope_host {
    Some(host) => slot_map_for_host(host),
    None => current_slot_map,
  };
  let mut matches = find_pseudo_element_rules(
    node,
    node_id,
    node_keys,
    &scopes.ua,
    selector_caches,
    scratch,
    ancestors,
    ancestor_bloom,
    current_slot_map,
    dom_maps.selector_blooms(),
    Some(element_attr_cache),
    sibling_cache,
    pseudo,
    false,
    scopes.quirks_mode,
  );

  if let Some((base, allow_shadow_host)) = scope_rule_index_with_shadow_host(scopes, scope_host) {
    matches.extend(find_pseudo_element_rules(
      node,
      node_id,
      node_keys,
      base,
      selector_caches,
      scratch,
      ancestors,
      ancestor_bloom,
      base_slot_map,
      dom_maps.selector_blooms(),
      Some(element_attr_cache),
      sibling_cache,
      pseudo,
      allow_shadow_host,
      scopes.quirks_mode,
    ));
  }

  if scopes.fallback_document_rules_in_shadow_scopes {
    if let Some(host) = scope_host {
      if scopes.shadows.contains_key(&host) {
        matches.extend(find_pseudo_element_rules(
          node,
          node_id,
          node_keys,
          &scopes.document,
          selector_caches,
          scratch,
          ancestors,
          ancestor_bloom,
          current_slot_map,
          dom_maps.selector_blooms(),
          Some(element_attr_cache),
          sibling_cache,
          pseudo,
          false,
          scopes.quirks_mode,
        ));
      }
    }
  }

  if let Some(host) = scopes.host_rules.get(&node_id) {
    matches.extend(find_pseudo_element_rules(
      node,
      node_id,
      node_keys,
      host,
      selector_caches,
      scratch,
      ancestors,
      ancestor_bloom,
      slot_map_for_host(node_id),
      dom_maps.selector_blooms(),
      Some(element_attr_cache),
      sibling_cache,
      pseudo,
      true,
      scopes.quirks_mode,
    ));
  }

  matches
}

#[inline]
fn cached_inline_style_declarations<'a>(
  cache: &'a mut Vec<Option<Arc<[Declaration]>>>,
  node: &DomNode,
  node_id: usize,
) -> Option<&'a [Declaration]> {
  let Some(entry) = cache.get_mut(node_id) else {
    return None;
  };
  if entry.is_none() {
    let style_attr = node.get_attribute_ref("style")?;
    *entry = Some(parse_inline_style_declarations(style_attr).into());
  }
  entry.as_deref()
}

#[inline(never)]
fn compute_base_styles<'a>(
  node: &DomNode,
  rule_scopes: &RuleScopes<'_>,
  scope_host: Option<usize>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  inline_style_decls: &mut Vec<Option<Arc<[Declaration]>>>,
  parent_styles: &ComputedStyle,
  parent_ua_styles: &ComputedStyle,
  root_font_size: f32,
  ua_root_font_size: f32,
  viewport: Size,
  color_scheme_pref: ColorScheme,
  ancestors: &[&'a DomNode],
  ancestor_bloom: Option<&selectors::bloom::BloomFilter>,
  ancestor_ids: &[usize],
  node_id: usize,
  container_ctx: Option<&ContainerQueryContext>,
  dom_maps: &DomMaps,
  slot_assignment: &SlotAssignment,
  sibling_cache: &SiblingListCache,
  element_attr_cache: &ElementAttrCache,
  include_starting_style: bool,
) -> Result<NodeBaseStyles, RenderError> {
  if !node.is_element() {
    let mut ua_styles = get_default_styles_for_element(node);
    inherit_styles(&mut ua_styles, parent_ua_styles);
    propagate_text_decorations(&mut ua_styles, parent_ua_styles);

    let mut styles = get_default_styles_for_element(node);
    inherit_styles(&mut styles, parent_styles);
    propagate_text_decorations(&mut styles, parent_styles);

    let is_root = is_root_element(ancestors);
    let current_root_font_size = if is_root {
      styles.font_size
    } else {
      root_font_size
    };
    let current_ua_root_font_size = if is_root {
      ua_styles.font_size
    } else {
      ua_root_font_size
    };
    ua_styles.root_font_size = current_ua_root_font_size;
    styles.root_font_size = current_root_font_size;
    resolve_line_height_length(&mut ua_styles, viewport);
    resolve_line_height_length(&mut styles, viewport);
    resolve_absolute_lengths(&mut ua_styles, current_ua_root_font_size, viewport);
    resolve_absolute_lengths(&mut styles, current_root_font_size, viewport);
    return Ok(NodeBaseStyles {
      styles,
      ua_styles,
      current_root_font_size,
      current_ua_root_font_size,
    });
  }

  let mut ua_styles = get_default_styles_for_element(node);
  inherit_styles(&mut ua_styles, parent_ua_styles);
  let mut matching_rules = collect_matching_rules(
    node,
    rule_scopes,
    scope_host,
    selector_caches,
    scratch,
    ancestors,
    ancestor_bloom,
    ancestor_ids,
    node_id,
    container_ctx,
    dom_maps,
    slot_assignment,
    sibling_cache,
    element_attr_cache,
  )?;
  let inline_tree_scope = tree_scope_prefix_for_node(dom_maps, node_id);
  matching_rules.extend(ua_default_rules(
    node,
    parent_styles.direction,
    &scratch.unlayered_layer_order(inline_tree_scope),
  ));
  if include_starting_style {
    prioritize_starting_style_rules(&mut matching_rules);
  } else {
    matching_rules.retain(|r| !r.starting_style);
  }
  let ua_matches: Vec<_> = matching_rules
    .iter()
    .filter(|r| r.origin == StyleOrigin::UserAgent)
    .cloned()
    .collect();
  let prof = cascade_profile_enabled();
  let decl_start = prof.then(|| Instant::now());
  apply_cascaded_declarations(
    &mut ua_styles,
    ua_matches,
    None,
    None,
    parent_ua_styles,
    parent_ua_styles.font_size,
    ua_root_font_size,
    viewport,
    default_computed_style(),
    |_| true,
  );
  if let (true, Some(start)) = (prof, decl_start) {
    record_declaration_time(start.elapsed());
  }

  if let Some(lang) = node
    .get_attribute_ref("lang")
    .or_else(|| node.get_attribute_ref("xml:lang"))
    .filter(|l| !l.is_empty())
  {
    ua_styles.language = normalize_language_tag(lang);
  }

  let is_root = is_root_element(ancestors);

  finalize_grid_placement(&mut ua_styles);
  resolve_match_parent_text_align_last(&mut ua_styles, parent_ua_styles, is_root);
  resolve_match_parent_text_align(&mut ua_styles, parent_ua_styles, is_root);
  resolve_relative_font_weight(&mut ua_styles, parent_ua_styles);
  propagate_text_decorations(&mut ua_styles, parent_ua_styles);

  let current_ua_root_font_size = if is_root {
    ua_styles.font_size
  } else {
    ua_root_font_size
  };
  ua_styles.root_font_size = current_ua_root_font_size;
  resolve_line_height_length(&mut ua_styles, viewport);
  resolve_absolute_lengths(&mut ua_styles, current_ua_root_font_size, viewport);
  let ua_default_color = ua_styles.color;
  let ua_default_background = ua_styles.background_color;

  let mut styles = get_default_styles_for_element(node);

  // Inherit styles from parent
  inherit_styles(&mut styles, parent_styles);

  // Apply matching CSS rules and inline styles with full cascade ordering
  append_presentational_hints(
    node,
    ancestors,
    parent_styles.direction,
    inline_tree_scope,
    scratch,
    &mut matching_rules,
  );
  let inline_decls =
    cached_inline_style_declarations(inline_style_decls, node, node_id).map(Cow::Borrowed);
  let inline_layer_order = if inline_decls.is_some() {
    Some(scratch.unlayered_layer_order(inline_tree_scope))
  } else {
    None
  };
  let decl_start = prof.then(|| Instant::now());
  apply_cascaded_declarations(
    &mut styles,
    matching_rules,
    inline_decls,
    inline_layer_order,
    parent_styles,
    parent_styles.font_size,
    root_font_size,
    viewport,
    &ua_styles,
    |_| true,
  );
  if let (true, Some(start)) = (prof, decl_start) {
    record_declaration_time(start.elapsed());
  }

  // Apply language from attributes (inherits by default)
  if let Some(lang) = node
    .get_attribute_ref("lang")
    .or_else(|| node.get_attribute_ref("xml:lang"))
    .filter(|l| !l.is_empty())
  {
    styles.language = normalize_language_tag(lang);
  }

  // Finalize grid placement - resolve named grid lines
  finalize_grid_placement(&mut styles);
  resolve_match_parent_text_align_last(&mut styles, parent_styles, is_root);
  resolve_match_parent_text_align(&mut styles, parent_styles, is_root);
  resolve_relative_font_weight(&mut styles, parent_styles);
  propagate_text_decorations(&mut styles, parent_styles);

  // Root font size for rem resolution: the document root sets the value for all descendants.
  let current_root_font_size = if is_root {
    styles.font_size
  } else {
    root_font_size
  };
  styles.root_font_size = current_root_font_size;
  resolve_line_height_length(&mut styles, viewport);
  resolve_absolute_lengths(&mut styles, current_root_font_size, viewport);

  let selected_scheme = select_color_scheme(&styles.color_scheme, color_scheme_pref);

  if is_root {
    if let Some(selected) = &selected_scheme {
      if matches!(selected, ColorSchemeEntry::Dark) {
        let dark_text = Rgba::rgb(232, 232, 232);
        let dark_background = Rgba::rgb(16, 16, 16);
        if styles.color == ua_default_color {
          styles.color = dark_text;
        }
        if ua_styles.color == ua_default_color {
          ua_styles.color = dark_text;
        }
        if styles.background_color == ua_default_background {
          styles.background_color = dark_background;
        }
        if ua_styles.background_color == ua_default_background {
          ua_styles.background_color = dark_background;
        }
      }
    }
  }

  apply_color_scheme_palette(
    &mut styles,
    &mut ua_styles,
    ua_default_color,
    ua_default_background,
    &selected_scheme,
    is_root,
  );

  styles.inert = node_is_inert(node, ancestors);
  if styles.inert {
    styles.pointer_events = PointerEvents::None;
  }
  styles.top_layer = top_layer_kind(node);

  Ok(NodeBaseStyles {
    styles,
    ua_styles,
    current_root_font_size,
    current_ua_root_font_size,
  })
}

fn apply_styles_internal(
  node: &DomNode,
  rule_scopes: &RuleScopes<'_>,
  scope_host: Option<usize>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  inline_style_decls: &mut Vec<Option<Arc<[Declaration]>>>,
  parent_styles: &ComputedStyle,
  parent_starting_styles: Option<&ComputedStyle>,
  parent_ua_styles: &ComputedStyle,
  root_font_size: f32,
  ua_root_font_size: f32,
  viewport: Size,
  color_scheme_pref: ColorScheme,
  node_counter: &mut usize,
  ancestor_ids: &mut Vec<usize>,
  container_ctx: Option<&ContainerQueryContext>,
  container_scope: Option<&HashSet<usize>>,
  reuse_map: Option<&HashMap<usize, *const StyledNode>>,
  reuse_counter: Option<&mut usize>,
  dom_maps: &DomMaps,
  slot_assignment: &SlotAssignment,
  sibling_cache: &SiblingListCache,
  element_attr_cache: &ElementAttrCache,
  deadline: Option<&RenderDeadline>,
  has_starting_styles: bool,
) -> Result<StyledNode, RenderError> {
  let mut ancestors: Vec<&DomNode> = Vec::new();
  let mut ancestor_bloom_filter = selectors::bloom::BloomFilter::new();
  let ancestor_bloom_enabled = ancestor_bloom_enabled();
  let ancestor_bloom_shadow_scoping =
    ancestor_bloom_enabled && ancestor_bloom_shadow_scoping_enabled();
  let mut deadline_counter: usize = 0;
  let styled = apply_styles_internal_with_ancestors(
    node,
    rule_scopes,
    scope_host,
    selector_caches,
    scratch,
    inline_style_decls,
    parent_styles,
    parent_starting_styles,
    parent_ua_styles,
    root_font_size,
    ua_root_font_size,
    viewport,
    color_scheme_pref,
    &mut ancestors,
    &mut ancestor_bloom_filter,
    ancestor_bloom_enabled,
    ancestor_bloom_shadow_scoping,
    node_counter,
    ancestor_ids,
    container_ctx,
    container_scope,
    reuse_map,
    reuse_counter,
    dom_maps,
    slot_assignment,
    sibling_cache,
    element_attr_cache,
    &mut deadline_counter,
    deadline,
    has_starting_styles,
  )?;
  Ok(styled)
}

#[inline(never)]
fn compute_pseudo_styles(
  node: &DomNode,
  rule_scopes: &RuleScopes<'_>,
  scope_host: Option<usize>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  ancestors: &[&DomNode],
  ancestor_bloom: Option<&selectors::bloom::BloomFilter>,
  node_id: usize,
  dom_maps: &DomMaps,
  sibling_cache: &SiblingListCache,
  element_attr_cache: &ElementAttrCache,
  styles: &mut ComputedStyle,
  ua_styles: &ComputedStyle,
  root_font_size: f32,
  ua_root_font_size: f32,
  viewport: Size,
  include_starting_style: bool,
) -> (
  Option<Arc<ComputedStyle>>,
  Option<Arc<ComputedStyle>>,
  Option<Arc<ComputedStyle>>,
  Option<Arc<ComputedStyle>>,
  Option<Arc<ComputedStyle>>,
) {
  if !node.is_element() {
    styles.backdrop = None;
    return (None, None, None, None, None);
  }

  let mut backdrop_styles = None;
  if styles.top_layer.is_some()
    && (styles.top_layer.map(|k| k.is_modal()).unwrap_or(false)
      || scope_has_pseudo_rules(rule_scopes, scope_host, node_id, &PseudoElement::Backdrop))
  {
    backdrop_styles = compute_pseudo_element_styles(
      node,
      rule_scopes,
      scope_host,
      selector_caches,
      scratch,
      ancestors,
      ancestor_bloom,
      node_id,
      dom_maps,
      sibling_cache,
      element_attr_cache,
      styles,
      ua_styles,
      root_font_size,
      ua_root_font_size,
      viewport,
      &PseudoElement::Backdrop,
      include_starting_style,
    )
    .map(Arc::new);
  }

  let prof = cascade_profile_enabled();
  let pseudo_start = prof.then(|| Instant::now());
  let mut first_line_ua_styles = None;
  let first_line_styles = compute_first_line_styles(
    node,
    rule_scopes,
    scope_host,
    selector_caches,
    scratch,
    ancestors,
    ancestor_bloom,
    node_id,
    dom_maps,
    sibling_cache,
    element_attr_cache,
    styles,
    ua_styles,
    root_font_size,
    ua_root_font_size,
    viewport,
    include_starting_style,
  )
  .map(|(line, ua)| {
    first_line_ua_styles = Some(ua);
    Arc::new(line)
  });
  let base_first_letter_styles = first_line_styles.as_deref().unwrap_or(styles);
  let base_first_letter_ua_styles = first_line_ua_styles.as_ref().unwrap_or(ua_styles);
  let first_letter_styles = compute_first_letter_styles(
    node,
    rule_scopes,
    scope_host,
    selector_caches,
    scratch,
    ancestors,
    ancestor_bloom,
    node_id,
    dom_maps,
    sibling_cache,
    element_attr_cache,
    base_first_letter_styles,
    base_first_letter_ua_styles,
    root_font_size,
    ua_root_font_size,
    viewport,
    include_starting_style,
  )
  .map(Arc::new);
  let before_styles =
    if scope_has_pseudo_content(rule_scopes, scope_host, node_id, &PseudoElement::Before) {
      compute_pseudo_element_styles(
        node,
        rule_scopes,
        scope_host,
        selector_caches,
        scratch,
        ancestors,
        ancestor_bloom,
        node_id,
        dom_maps,
        sibling_cache,
        element_attr_cache,
        styles,
        ua_styles,
        root_font_size,
        ua_root_font_size,
        viewport,
        &PseudoElement::Before,
        include_starting_style,
      )
      .map(Arc::new)
    } else {
      None
    };
  let after_styles =
    if scope_has_pseudo_content(rule_scopes, scope_host, node_id, &PseudoElement::After) {
      compute_pseudo_element_styles(
        node,
        rule_scopes,
        scope_host,
        selector_caches,
        scratch,
        ancestors,
        ancestor_bloom,
        node_id,
        dom_maps,
        sibling_cache,
        element_attr_cache,
        styles,
        ua_styles,
        root_font_size,
        ua_root_font_size,
        viewport,
        &PseudoElement::After,
        include_starting_style,
      )
      .map(Arc::new)
    } else {
      None
    };
  let marker_styles = compute_marker_styles(
    node,
    rule_scopes,
    scope_host,
    selector_caches,
    scratch,
    ancestors,
    ancestor_bloom,
    node_id,
    dom_maps,
    sibling_cache,
    element_attr_cache,
    styles,
    ua_styles,
    root_font_size,
    ua_root_font_size,
    viewport,
    include_starting_style,
  )
  .map(Arc::new);
  if let (true, Some(start)) = (prof, pseudo_start) {
    record_pseudo_time(start.elapsed());
  }

  styles.backdrop = backdrop_styles;

  (
    before_styles,
    after_styles,
    marker_styles,
    first_line_styles,
    first_letter_styles,
  )
}

#[inline(never)]
fn try_reuse_styled_subtree(
  node_id: usize,
  node_counter: &mut usize,
  container_scope: Option<&HashSet<usize>>,
  reuse_map: Option<&HashMap<usize, *const StyledNode>>,
  reuse_counter: &mut Option<&mut usize>,
) -> Option<StyledNode> {
  let scope = container_scope?;
  let map = reuse_map?;
  if scope.contains(&node_id) {
    return None;
  }
  let ptr = map.get(&node_id)?;
  let reused_ref = unsafe { &**ptr };
  let reused_size = count_styled_nodes(reused_ref);
  if reused_size > 1 {
    let advance = reused_size.saturating_sub(1);
    *node_counter = (*node_counter).saturating_add(advance);
  }
  if let Some(counter) = reuse_counter.as_deref_mut() {
    *counter += reused_size;
  }
  Some(reused_ref.clone())
}

fn apply_styles_internal_with_ancestors<'a>(
  node: &'a DomNode,
  rule_scopes: &RuleScopes<'_>,
  scope_host: Option<usize>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  inline_style_decls: &mut Vec<Option<Arc<[Declaration]>>>,
  parent_styles: &ComputedStyle,
  parent_starting_styles: Option<&ComputedStyle>,
  parent_ua_styles: &ComputedStyle,
  root_font_size: f32,
  ua_root_font_size: f32,
  viewport: Size,
  color_scheme_pref: ColorScheme,
  ancestors: &mut Vec<&'a DomNode>,
  ancestor_bloom_filter: &mut selectors::bloom::BloomFilter,
  ancestor_bloom_enabled: bool,
  ancestor_bloom_shadow_scoping: bool,
  node_counter: &mut usize,
  ancestor_ids: &mut Vec<usize>,
  container_ctx: Option<&ContainerQueryContext>,
  container_scope: Option<&HashSet<usize>>,
  reuse_map: Option<&HashMap<usize, *const StyledNode>>,
  reuse_counter: Option<&mut usize>,
  dom_maps: &DomMaps,
  slot_assignment: &SlotAssignment,
  sibling_cache: &SiblingListCache,
  element_attr_cache: &ElementAttrCache,
  deadline_counter: &mut usize,
  deadline: Option<&RenderDeadline>,
  has_starting_styles: bool,
) -> Result<StyledNode, RenderError> {
  struct Frame<'a> {
    node: &'a DomNode,
    scope_host: Option<usize>,
    node_id: usize,
    base: NodeBaseStyles,
    starting_base: Option<NodeBaseStyles>,
    children: Vec<StyledNode>,
    next_child: usize,
    is_shadow_root: bool,
    push_ancestor_bloom: bool,
    entered_shadow_bloom_scope: bool,
  }

  let mut reuse_counter = reuse_counter;

  record_node_visit(node);

  let root_id = *node_counter;
  *node_counter += 1;
  if let Some(deadline) = deadline {
    deadline.check_periodic(
      deadline_counter,
      CASCADE_NODE_DEADLINE_STRIDE,
      RenderStage::Cascade,
    )?;
  }

  // If this node lies outside any container scope during a container-query recascade,
  // reuse the prior styled subtree to avoid recomputing unaffected branches.
  if let Some(reused) = try_reuse_styled_subtree(
    root_id,
    node_counter,
    container_scope,
    reuse_map,
    &mut reuse_counter,
  ) {
    return Ok(reused);
  }

  let root_base = compute_base_styles(
    node,
    rule_scopes,
    scope_host,
    selector_caches,
    scratch,
    inline_style_decls,
    parent_styles,
    parent_ua_styles,
    root_font_size,
    ua_root_font_size,
    viewport,
    color_scheme_pref,
    ancestors.as_slice(),
    ancestor_bloom_enabled.then_some(&*ancestor_bloom_filter),
    ancestor_ids.as_slice(),
    root_id,
    container_ctx,
    dom_maps,
    slot_assignment,
    sibling_cache,
    element_attr_cache,
    false,
  )?;
  let root_starting_base = if has_starting_styles {
    let start_parent_styles = parent_starting_styles.unwrap_or(parent_styles);
    let start_root_font_size = parent_starting_styles
      .map(|s| s.root_font_size)
      .unwrap_or(root_font_size);
    Some(compute_base_styles(
      node,
      rule_scopes,
      scope_host,
      selector_caches,
      scratch,
      inline_style_decls,
      start_parent_styles,
      parent_ua_styles,
      start_root_font_size,
      ua_root_font_size,
      viewport,
      color_scheme_pref,
      ancestors.as_slice(),
      ancestor_bloom_enabled.then_some(&*ancestor_bloom_filter),
      ancestor_ids.as_slice(),
      root_id,
      container_ctx,
      dom_maps,
      slot_assignment,
      sibling_cache,
      element_attr_cache,
      true,
    )?)
  } else {
    None
  };

  let mut shadow_bloom_stack: Vec<selectors::bloom::BloomFilter> = Vec::new();
  let root_is_shadow_root = matches!(node.node_type, DomNodeType::ShadowRoot { .. });
  let root_shadow_bloom_scope = ancestor_bloom_shadow_scoping && root_is_shadow_root;
  if root_shadow_bloom_scope {
    let saved = std::mem::replace(ancestor_bloom_filter, selectors::bloom::BloomFilter::new());
    shadow_bloom_stack.push(saved);
  }

  let mut stack: Vec<Frame<'a>> = Vec::new();
  let push_ancestor_bloom =
    ancestor_bloom_enabled && node.is_element() && !node.children.is_empty();
  stack.push(Frame {
    node,
    scope_host,
    node_id: root_id,
    base: root_base,
    starting_base: root_starting_base,
    children: Vec::with_capacity(node.children.len()),
    next_child: 0,
    is_shadow_root: root_is_shadow_root,
    push_ancestor_bloom,
    entered_shadow_bloom_scope: root_shadow_bloom_scope,
  });
  if push_ancestor_bloom {
    #[cfg(test)]
    ANCESTOR_BLOOM_HASH_INSERTS.with(|counter| counter.set(counter.get().saturating_add(1)));
    element_attr_cache.for_each_ancestor_bloom_hash(node, rule_scopes.quirks_mode, |hash| {
      ancestor_bloom_filter.insert_hash(hash);
    });
  }
  ancestors.push(node);
  ancestor_ids.push(root_id);

  loop {
    let child_info = {
      let frame = stack
        .last_mut()
        .expect("style traversal stack must contain at least one frame");
      if frame.next_child >= frame.node.children.len() {
        None
      } else {
        let child = &frame.node.children[frame.next_child];
        frame.next_child += 1;
        let child_scope = match (frame.is_shadow_root, &child.node_type) {
          (true, _) => frame.scope_host,
          (false, DomNodeType::ShadowRoot { .. }) => Some(frame.node_id),
          (false, _) => frame.scope_host,
        };
        Some((child, child_scope))
      }
    };

    if let Some((child, child_scope)) = child_info {
      record_node_visit(child);
      let child_id = *node_counter;
      *node_counter += 1;
      if let Some(deadline) = deadline {
        deadline.check_periodic(
          deadline_counter,
          CASCADE_NODE_DEADLINE_STRIDE,
          RenderStage::Cascade,
        )?;
      }

      if let Some(reused) = try_reuse_styled_subtree(
        child_id,
        node_counter,
        container_scope,
        reuse_map,
        &mut reuse_counter,
      ) {
        stack
          .last_mut()
          .expect("parent frame should still be on the stack")
          .children
          .push(reused);
        continue;
      }

      let (child_base, child_starting_base) = {
        let parent = stack
          .last()
          .expect("parent frame should still be on the stack");

        let base = compute_base_styles(
          child,
          rule_scopes,
          child_scope,
          selector_caches,
          scratch,
          inline_style_decls,
          &parent.base.styles,
          &parent.base.ua_styles,
          parent.base.current_root_font_size,
          parent.base.current_ua_root_font_size,
          viewport,
          color_scheme_pref,
          ancestors.as_slice(),
          ancestor_bloom_enabled.then_some(&*ancestor_bloom_filter),
          ancestor_ids.as_slice(),
          child_id,
          container_ctx,
          dom_maps,
          slot_assignment,
          sibling_cache,
          element_attr_cache,
          false,
        )?;

        let starting_base = if has_starting_styles {
          let parent_start = parent.starting_base.as_ref().map(|b| &b.styles);
          let start_parent_styles = parent_start.unwrap_or(&parent.base.styles);
          let start_root_font_size = parent_start
            .map(|s| s.root_font_size)
            .unwrap_or(parent.base.current_root_font_size);
          Some(compute_base_styles(
            child,
            rule_scopes,
            child_scope,
            selector_caches,
            scratch,
            inline_style_decls,
            start_parent_styles,
            &parent.base.ua_styles,
            start_root_font_size,
            parent.base.current_ua_root_font_size,
            viewport,
            color_scheme_pref,
            ancestors.as_slice(),
            ancestor_bloom_enabled.then_some(&*ancestor_bloom_filter),
            ancestor_ids.as_slice(),
            child_id,
            container_ctx,
            dom_maps,
            slot_assignment,
            sibling_cache,
            element_attr_cache,
            true,
          )?)
        } else {
          None
        };

        (base, starting_base)
      };

      let child_is_shadow_root = matches!(child.node_type, DomNodeType::ShadowRoot { .. });
      let child_shadow_bloom_scope = ancestor_bloom_shadow_scoping && child_is_shadow_root;
      if child_shadow_bloom_scope {
        let saved = std::mem::replace(ancestor_bloom_filter, selectors::bloom::BloomFilter::new());
        shadow_bloom_stack.push(saved);
      }

      let child_push_ancestor_bloom =
        ancestor_bloom_enabled && child.is_element() && !child.children.is_empty();
      stack.push(Frame {
        node: child,
        scope_host: child_scope,
        node_id: child_id,
        base: child_base,
        starting_base: child_starting_base,
        children: Vec::with_capacity(child.children.len()),
        next_child: 0,
        is_shadow_root: child_is_shadow_root,
        push_ancestor_bloom: child_push_ancestor_bloom,
        entered_shadow_bloom_scope: child_shadow_bloom_scope,
      });

      if child_push_ancestor_bloom {
        #[cfg(test)]
        ANCESTOR_BLOOM_HASH_INSERTS.with(|counter| counter.set(counter.get().saturating_add(1)));
        element_attr_cache.for_each_ancestor_bloom_hash(child, rule_scopes.quirks_mode, |hash| {
          ancestor_bloom_filter.insert_hash(hash);
        });
      }
      ancestors.push(child);
      ancestor_ids.push(child_id);
      continue;
    }

    ancestors.pop();
    ancestor_ids.pop();

    let frame = stack
      .pop()
      .expect("style traversal stack must contain at least one frame");

    if frame.push_ancestor_bloom {
      element_attr_cache.for_each_ancestor_bloom_hash(
        frame.node,
        rule_scopes.quirks_mode,
        |hash| {
          ancestor_bloom_filter.remove_hash(hash);
        },
      );
    }

    if frame.entered_shadow_bloom_scope {
      let restored = shadow_bloom_stack
        .pop()
        .expect("ancestor bloom shadow scope stack underflow");
      *ancestor_bloom_filter = restored;
    }

    let mut base = frame.base;
    let (before_styles, after_styles, marker_styles, first_line_styles, first_letter_styles) =
      compute_pseudo_styles(
        frame.node,
        rule_scopes,
        frame.scope_host,
        selector_caches,
        scratch,
        ancestors.as_slice(),
        ancestor_bloom_enabled.then_some(&*ancestor_bloom_filter),
        frame.node_id,
        dom_maps,
        sibling_cache,
        element_attr_cache,
        &mut base.styles,
        &base.ua_styles,
        base.current_root_font_size,
        base.current_ua_root_font_size,
        viewport,
        false,
      );

    let mut starting_styles = StartingStyleSet::default();
    if let Some(mut start) = frame.starting_base {
      let (before, after, marker, first_line, first_letter) = compute_pseudo_styles(
        frame.node,
        rule_scopes,
        frame.scope_host,
        selector_caches,
        scratch,
        ancestors.as_slice(),
        ancestor_bloom_enabled.then_some(&*ancestor_bloom_filter),
        frame.node_id,
        dom_maps,
        sibling_cache,
        element_attr_cache,
        &mut start.styles,
        &start.ua_styles,
        start.current_root_font_size,
        start.current_ua_root_font_size,
        viewport,
        true,
      );
      starting_styles = StartingStyleSet {
        base: Some(Arc::new(start.styles)),
        before,
        after,
        marker,
        first_line,
        first_letter,
      };
    }

    let NodeBaseStyles {
      styles,
      ua_styles: _,
      current_root_font_size: _,
      current_ua_root_font_size: _,
    } = base;

    let node_id = frame.node_id;
    let styled = StyledNode {
      node_id,
      node: frame.node.clone_without_children(),
      styles: Arc::new(styles),
      starting_styles,
      before_styles,
      after_styles,
      marker_styles,
      first_line_styles,
      first_letter_styles,
      assigned_slot: slot_assignment.node_to_slot.get(&node_id).cloned(),
      slotted_node_ids: slot_assignment
        .slot_to_nodes
        .get(&node_id)
        .cloned()
        .unwrap_or_default(),
      children: frame.children,
    };

    if let Some(parent) = stack.last_mut() {
      parent.children.push(styled);
      continue;
    }

    return Ok(styled);
  }
}

pub(crate) fn inherit_styles(styles: &mut ComputedStyle, parent: &ComputedStyle) {
  // Reset cascade bookkeeping for the new style; logical pending state should not inherit.
  styles.logical.reset();
  // Typography properties inherit
  styles.font_family = parent.font_family.clone();
  styles.font_size = parent.font_size;
  styles.root_font_size = parent.root_font_size;
  styles.font_weight = parent.font_weight;
  styles.font_style = parent.font_style;
  styles.font_variant = parent.font_variant;
  styles.font_variant_caps = parent.font_variant_caps;
  styles.font_variant_alternates = parent.font_variant_alternates.clone();
  styles.font_variant_numeric = parent.font_variant_numeric;
  styles.font_variant_east_asian = parent.font_variant_east_asian;
  styles.font_variant_ligatures = parent.font_variant_ligatures;
  styles.font_variant_position = parent.font_variant_position;
  styles.font_size_adjust = parent.font_size_adjust;
  styles.font_synthesis = parent.font_synthesis;
  styles.font_feature_settings = parent.font_feature_settings.clone();
  styles.font_optical_sizing = parent.font_optical_sizing;
  styles.font_variation_settings = parent.font_variation_settings.clone();
  styles.font_language_override = parent.font_language_override.clone();
  styles.font_variant_emoji = parent.font_variant_emoji;
  styles.font_stretch = parent.font_stretch;
  styles.font_kerning = parent.font_kerning;
  styles.line_height = parent.line_height.clone();
  styles.direction = parent.direction;
  styles.text_align = parent.text_align;
  styles.text_align_last = parent.text_align_last;
  styles.text_justify = parent.text_justify;
  styles.text_rendering = parent.text_rendering;
  styles.text_indent = parent.text_indent;
  styles.text_wrap = parent.text_wrap;
  styles.text_decoration_skip_ink = parent.text_decoration_skip_ink;
  styles.text_shadow = parent.text_shadow.clone();
  styles.text_underline_offset = parent.text_underline_offset;
  styles.text_underline_position = parent.text_underline_position;
  styles.text_emphasis_style = parent.text_emphasis_style.clone();
  styles.text_emphasis_color = parent.text_emphasis_color;
  styles.text_emphasis_position = parent.text_emphasis_position;
  styles.text_transform = parent.text_transform;
  styles.text_combine_upright = parent.text_combine_upright;
  styles.text_orientation = parent.text_orientation;
  styles.writing_mode = parent.writing_mode;
  styles.letter_spacing = parent.letter_spacing;
  styles.word_spacing = parent.word_spacing;
  styles.justify_items = parent.justify_items;
  styles.visibility = parent.visibility;
  styles.white_space = parent.white_space;
  styles.line_break = parent.line_break;
  // widows and orphans inherit per CSS 2.1.
  styles.widows = parent.widows;
  styles.orphans = parent.orphans;
  styles.tab_size = parent.tab_size;
  styles.caption_side = parent.caption_side;
  styles.empty_cells = parent.empty_cells;
  styles.hyphens = parent.hyphens;
  styles.word_break = parent.word_break;
  styles.overflow_wrap = parent.overflow_wrap;
  styles.language = parent.language.clone();
  styles.list_style_type = parent.list_style_type.clone();
  styles.list_style_position = parent.list_style_position;
  styles.list_style_image = parent.list_style_image.clone();
  styles.counter_styles = parent.counter_styles.clone();
  styles.image_orientation = parent.image_orientation;
  styles.quotes = parent.quotes.clone();
  styles.cursor = parent.cursor;
  styles.cursor_images = parent.cursor_images.clone();
  styles.scrollbar_color = parent.scrollbar_color;

  // Color scheme inherits
  styles.color_scheme = parent.color_scheme.clone();

  // Color inherits
  styles.color = parent.color;

  // CSS Custom Properties inherit according to registration.
  styles.custom_property_registry = parent.custom_property_registry.clone();
  styles.custom_properties = parent.custom_properties.clone();

  // Registered custom properties with `inherits: false` behave like non-inherited properties:
  // descendants compute to their own initial value (or the guaranteed-invalid value when the
  // initial value is omitted).
  for (name, rule) in styles.custom_property_registry.iter() {
    if rule.inherits {
      if !styles.custom_properties.contains_key(name) {
        if let Some(initial) = &rule.initial_value {
          styles
            .custom_properties
            .insert(name.clone(), initial.clone());
        }
      }
      continue;
    }

    if let Some(initial) = &rule.initial_value {
      let already_initial = styles
        .custom_properties
        .get(name.as_str())
        .map(|value| value == initial)
        .unwrap_or(false);
      if !already_initial {
        styles
          .custom_properties
          .insert(name.clone(), initial.clone());
      }
    } else {
      styles.custom_properties.remove(name.as_str());
    }
  }
}

/// Resolves line-height lengths to absolute pixels using computed font sizes and viewport metrics.
fn resolve_line_height_length(style: &mut ComputedStyle, viewport: Size) {
  use crate::style::types::LineHeight;

  if let LineHeight::Length(len) = style.line_height {
    let px = match len.unit {
      u if u.is_absolute() => len.to_px(),
      LengthUnit::Em => len.value * style.font_size,
      LengthUnit::Rem => len.value * style.root_font_size,
      LengthUnit::Ex => len.value * style.font_size * 0.5,
      LengthUnit::Ch => len.value * style.font_size * 0.5,
      u if u.is_viewport_relative() => len
        .resolve_with_viewport(viewport.width, viewport.height)
        .unwrap_or(len.value),
      _ => len.value,
    };

    style.line_height = LineHeight::Length(Length::px(px));
  }
}

fn resolve_match_parent_text_align(
  styles: &mut ComputedStyle,
  parent: &ComputedStyle,
  is_root: bool,
) {
  use crate::style::types::TextAlign;
  use crate::style::types::TextAlignLast;
  if !matches!(styles.text_align, TextAlign::MatchParent) {
    return;
  }
  if matches!(styles.text_align_last, TextAlignLast::Auto) {
    styles.text_align_last = TextAlignLast::MatchParent;
  }
  if is_root {
    styles.text_align = TextAlign::Start;
    return;
  }
  // Behaves like inherit, but start/end become physical based on the parent's direction.
  let inherited = match parent.text_align {
    TextAlign::MatchParent => {
      if matches!(parent.direction, crate::style::types::Direction::Rtl) {
        TextAlign::End
      } else {
        TextAlign::Start
      }
    }
    other => other,
  };
  styles.text_align = match inherited {
    TextAlign::Start => {
      if matches!(parent.direction, crate::style::types::Direction::Rtl) {
        TextAlign::Right
      } else {
        TextAlign::Left
      }
    }
    TextAlign::End => {
      if matches!(parent.direction, crate::style::types::Direction::Rtl) {
        TextAlign::Left
      } else {
        TextAlign::Right
      }
    }
    other => other,
  };
}

fn resolve_match_parent_text_align_last(
  styles: &mut ComputedStyle,
  parent: &ComputedStyle,
  is_root: bool,
) {
  use crate::style::types::TextAlign;
  use crate::style::types::TextAlignLast;
  if matches!(styles.text_align, TextAlign::MatchParent)
    && styles.text_align_last == parent.text_align_last
  {
    styles.text_align_last = TextAlignLast::MatchParent;
  }
  if !matches!(styles.text_align_last, TextAlignLast::MatchParent) {
    return;
  }
  if is_root {
    styles.text_align_last = TextAlignLast::Start;
    return;
  }
  let inherited = match parent.text_align_last {
    TextAlignLast::MatchParent => {
      if matches!(parent.direction, crate::style::types::Direction::Rtl) {
        TextAlignLast::End
      } else {
        TextAlignLast::Start
      }
    }
    other => other,
  };
  styles.text_align_last = match inherited {
    TextAlignLast::Start => {
      if matches!(parent.direction, crate::style::types::Direction::Rtl) {
        TextAlignLast::Right
      } else {
        TextAlignLast::Left
      }
    }
    TextAlignLast::End => {
      if matches!(parent.direction, crate::style::types::Direction::Rtl) {
        TextAlignLast::Left
      } else {
        TextAlignLast::Right
      }
    }
    other => other,
  };
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::css::parser::inline_style_declaration_parse_count;
  use crate::css::parser::parse_declarations;
  use crate::css::parser::parse_stylesheet;
  use crate::css::parser::reset_inline_style_declaration_parse_count;
  use crate::css::types::CssImportLoader;
  use crate::css::types::StyleSheet;
  use crate::dom::DomNode;
  use crate::dom::DomNodeType;
  use crate::dom::HTML_NAMESPACE;
  use crate::style::color::Color;
  use crate::style::color::Rgba;
  use crate::style::computed::Visibility;
  use crate::style::content::ContentValue;
  use crate::style::display::Display;
  use crate::style::float::Float;
  use crate::style::media::MediaContext;
  use crate::style::media::MediaQuery;
  use crate::style::properties::apply_declaration;
  use crate::style::types::BorderCollapse;
  use crate::style::types::BorderStyle;
  use crate::style::types::Direction;
  use crate::style::types::ImageOrientation;
  use crate::style::types::LineBreak;
  use crate::style::types::ListStylePosition;
  use crate::style::types::ListStyleType;
  use crate::style::types::ScrollbarColor;
  use crate::style::types::TextCombineUpright;
  use crate::style::types::TextDecorationLine;
  use crate::style::types::TextOrientation;
  use crate::style::types::TextUnderlineOffset;
  use crate::style::types::TextUnderlinePosition;
  use crate::style::types::UnicodeBidi;
  use crate::style::types::WhiteSpace;
  use crate::style::types::WillChange;
  use crate::style::types::WillChangeHint;
  use crate::style::types::WritingMode;
  use crate::style::values::{CustomPropertyTypedValue, Length};
  use crate::style::ComputedStyle;
  use crate::style::CursorKeyword;
  use crate::style::OutlineStyle;
  use std::sync::{Mutex, MutexGuard, OnceLock};

  fn cascade_global_test_lock() -> MutexGuard<'static, ()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(())).lock().unwrap()
  }

  struct AncestorBloomEnabledGuard {
    prev: bool,
  }

  impl AncestorBloomEnabledGuard {
    fn new(enabled: bool) -> Self {
      let prev = crate::dom::ancestor_bloom_enabled();
      crate::dom::set_ancestor_bloom_enabled(enabled);
      Self { prev }
    }
  }

  impl Drop for AncestorBloomEnabledGuard {
    fn drop(&mut self) {
      crate::dom::set_ancestor_bloom_enabled(self.prev);
    }
  }

  fn element_with_style(style: &str) -> DomNode {
    DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), style.to_string())],
      },
      children: vec![],
    }
  }

  fn node_selector_keys<'a>(
    node: &DomNode,
    class_keys: &'a mut Vec<SelectorBucketKey>,
    attr_keys: &'a mut Vec<SelectorBucketKey>,
  ) -> NodeSelectorKeys<'a> {
    class_keys.clear();
    attr_keys.clear();
    let tag_key = node.tag_name().map(selector_bucket_tag).unwrap_or_default();
    let id_key = node.get_attribute_ref("id").map(selector_bucket_id);
    if let Some(class_attr) = node.get_attribute_ref("class") {
      for cls in class_attr.split_ascii_whitespace() {
        class_keys.push(selector_bucket_class(cls));
      }
    }
    for (name, _) in node.attributes_iter() {
      attr_keys.push(selector_bucket_attr(name));
    }
    NodeSelectorKeys {
      tag_key,
      id_key,
      class_keys: class_keys.as_slice(),
      attr_keys: attr_keys.as_slice(),
    }
  }

  fn assert_styled_trees_equal(a: &StyledNode, b: &StyledNode) {
    assert_eq!(a.node_id, b.node_id);
    assert_eq!(a.styles, b.styles, "styles differ at node_id={}", a.node_id);
    assert_eq!(
      a.starting_styles.base.as_deref(),
      b.starting_styles.base.as_deref(),
      "starting base differs at node_id={}",
      a.node_id
    );
    assert_eq!(
      a.starting_styles.before.as_deref(),
      b.starting_styles.before.as_deref(),
      "starting ::before differs at node_id={}",
      a.node_id
    );
    assert_eq!(
      a.starting_styles.after.as_deref(),
      b.starting_styles.after.as_deref(),
      "starting ::after differs at node_id={}",
      a.node_id
    );
    assert_eq!(
      a.starting_styles.marker.as_deref(),
      b.starting_styles.marker.as_deref(),
      "starting ::marker differs at node_id={}",
      a.node_id
    );
    assert_eq!(
      a.starting_styles.first_line.as_deref(),
      b.starting_styles.first_line.as_deref(),
      "starting ::first-line differs at node_id={}",
      a.node_id
    );
    assert_eq!(
      a.starting_styles.first_letter.as_deref(),
      b.starting_styles.first_letter.as_deref(),
      "starting ::first-letter differs at node_id={}",
      a.node_id
    );
    assert_eq!(
      a.before_styles.as_deref(),
      b.before_styles.as_deref(),
      "::before differs at node_id={}",
      a.node_id
    );
    assert_eq!(
      a.after_styles.as_deref(),
      b.after_styles.as_deref(),
      "::after differs at node_id={}",
      b.node_id
    );
    assert_eq!(
      a.marker_styles.as_deref(),
      b.marker_styles.as_deref(),
      "::marker differs at node_id={}",
      a.node_id
    );
    assert_eq!(
      a.first_line_styles.as_deref(),
      b.first_line_styles.as_deref(),
      "::first-line differs at node_id={}",
      a.node_id
    );
    assert_eq!(
      a.first_letter_styles.as_deref(),
      b.first_letter_styles.as_deref(),
      "::first-letter differs at node_id={}",
      a.node_id
    );
    assert_eq!(a.assigned_slot, b.assigned_slot);
    assert_eq!(a.slotted_node_ids, b.slotted_node_ids);
    assert_eq!(a.children.len(), b.children.len(), "child count differs");
    for (child_a, child_b) in a.children.iter().zip(b.children.iter()) {
      assert_styled_trees_equal(child_a, child_b);
    }
  }

  fn find_styled_node_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
    if node.node.get_attribute_ref("id") == Some(id) {
      return Some(node);
    }
    for child in node.children.iter() {
      if let Some(found) = find_styled_node_by_id(child, id) {
        return Some(found);
      }
    }
    None
  }

  #[test]
  fn styled_node_holds_shallow_dom_copy_and_handles_deep_trees() {
    let depth = 256;

    let mut dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: Vec::new(),
      },
      children: Vec::new(),
    };

    for _ in 0..depth {
      dom = DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: Vec::new(),
        },
        children: vec![dom],
      };
    }

    let stylesheet = parse_stylesheet("div { color: rgb(1, 2, 3); }").expect("parse stylesheet");
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let styled = apply_styles_with_media(&dom, &stylesheet, &media_ctx);

    let mut seen = 0usize;
    let mut cursor = &styled;
    loop {
      assert!(
        cursor.node.children.is_empty(),
        "StyledNode.node must not clone DOM children (node_id={})",
        cursor.node_id
      );
      seen += 1;
      if cursor.children.is_empty() {
        break;
      }
      assert_eq!(
        cursor.children.len(),
        1,
        "expected a single-child chain at node_id={}",
        cursor.node_id
      );
      cursor = &cursor.children[0];
    }

    assert_eq!(seen, depth + 1);
  }

  #[test]
  fn attach_starting_styles_copies_snapshot_fields() {
    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("id".to_string(), "target".to_string())],
      },
      children: vec![],
    };
    let mut after_change = ComputedStyle::default();
    after_change.opacity = 1.0;
    let mut starting_snapshot = ComputedStyle::default();
    starting_snapshot.opacity = 0.25;

    let mut target = StyledNode {
      node_id: 1,
      node: node.clone(),
      styles: Arc::new(after_change.clone()),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: Vec::new(),
    };

    let starting_tree = StyledNode {
      node_id: 1,
      node,
      styles: Arc::new(after_change),
      starting_styles: StartingStyleSet {
        base: Some(Arc::new(starting_snapshot.clone())),
        ..StartingStyleSet::default()
      },
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: Vec::new(),
    };

    attach_starting_styles(&mut target, &starting_tree);
    assert_eq!(
      target.starting_styles.base.as_deref(),
      Some(&starting_snapshot)
    );
  }

  #[test]
  fn prepared_cascade_matches_legacy_container_query_recascade() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("id".to_string(), "container".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("id".to_string(), "inner".to_string())],
        },
        children: vec![],
      }],
    };

    let stylesheet = parse_stylesheet(
      r#"
        #inner { color: rgb(10, 20, 30); }
        @container (min-width: 500px) {
          #inner { color: rgb(1, 2, 3); }
        }
      "#,
    )
    .expect("parse stylesheet");
    let style_set = StyleSet::from_document(stylesheet);
    let media_ctx = MediaContext::screen(800.0, 600.0);

    let legacy_first = apply_style_set_with_media_target_and_imports_cached_with_deadline(
      &dom, &style_set, &media_ctx, None, None, None, None, None, None, None, None,
    )
    .expect("legacy first cascade");

    let container_ctx = ContainerQueryContext {
      base_media: media_ctx.clone(),
      containers: HashMap::from([(
        1usize,
        ContainerQueryInfo {
          inline_size: 600.0,
          block_size: 400.0,
          container_type: ContainerType::InlineSize,
          names: Vec::new(),
          font_size: 16.0,
          styles: Arc::clone(&legacy_first.styles),
        },
      )]),
    };

    let legacy_second = apply_style_set_with_media_target_and_imports_cached_with_deadline(
      &dom,
      &style_set,
      &media_ctx,
      None,
      None,
      None,
      Some(&container_ctx),
      None,
      None,
      None,
      None,
    )
    .expect("legacy second cascade");

    let mut prepared =
      PreparedCascade::new_for_style_set(&dom, &style_set, &media_ctx, None, None, None, false)
        .expect("build prepared cascade");
    let prepared_first = prepared
      .apply(None, None, None, None, None)
      .expect("prepared first cascade");
    let prepared_second = prepared
      .apply(None, Some(&container_ctx), None, None, None)
      .expect("prepared second cascade");

    assert_styled_trees_equal(&legacy_first, &prepared_first);
    assert_styled_trees_equal(&legacy_second, &prepared_second);

    let inner_first = legacy_first.children.first().expect("inner");
    let inner_second = legacy_second.children.first().expect("inner");
    assert_eq!(inner_first.styles.color, Rgba::rgb(10, 20, 30));
    assert_eq!(inner_second.styles.color, Rgba::rgb(1, 2, 3));
  }

  #[test]
  fn prepared_cascade_caches_inline_style_declarations_across_passes() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("id".to_string(), "container".to_string()),
          (
            "style".to_string(),
            "padding: 4px; margin: 2px; font-size: 12px;".to_string(),
          ),
        ],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![
            ("id".to_string(), "inner".to_string()),
            (
              "style".to_string(),
              "padding: 1px 2px; margin: 3px; opacity: 0.5;".to_string(),
            ),
          ],
        },
        children: vec![],
      }],
    };

    let stylesheet = parse_stylesheet(
      r#"
        #inner { color: rgb(10, 20, 30); }
        @container (min-width: 500px) {
          #inner { color: rgb(1, 2, 3); }
        }
        @starting-style {
          #inner { opacity: 0.25; }
        }
      "#,
    )
    .expect("parse stylesheet");
    let style_set = StyleSet::from_document(stylesheet);
    let media_ctx = MediaContext::screen(800.0, 600.0);

    // "Legacy" behavior: rebuild a PreparedCascade for each pass.
    let legacy_first = apply_starting_style_set_with_media_target_and_imports_cached_with_deadline(
      &dom, &style_set, &media_ctx, None, None, None, None, None, None, None, None,
    )
    .expect("legacy first cascade");

    let container_ctx = ContainerQueryContext {
      base_media: media_ctx.clone(),
      containers: HashMap::from([(
        1usize,
        ContainerQueryInfo {
          inline_size: 600.0,
          block_size: 400.0,
          container_type: ContainerType::InlineSize,
          names: Vec::new(),
          font_size: 16.0,
          styles: Arc::clone(&legacy_first.styles),
        },
      )]),
    };

    let legacy_second =
      apply_starting_style_set_with_media_target_and_imports_cached_with_deadline(
        &dom,
        &style_set,
        &media_ctx,
        None,
        None,
        None,
        Some(&container_ctx),
        None,
        None,
        None,
        None,
      )
      .expect("legacy second cascade");

    reset_inline_style_declaration_parse_count();

    let mut prepared =
      PreparedCascade::new_for_style_set(&dom, &style_set, &media_ctx, None, None, None, true)
        .expect("build prepared cascade");
    let prepared_first = prepared
      .apply(None, None, None, None, None)
      .expect("prepared first cascade");
    let prepared_second = prepared
      .apply(None, Some(&container_ctx), None, None, None)
      .expect("prepared second cascade");

    assert_styled_trees_equal(&legacy_first, &prepared_first);
    assert_styled_trees_equal(&legacy_second, &prepared_second);

    // There are two nodes with inline style attributes. Ensure we only parse them once across:
    // - both base + @starting-style cascades, and
    // - both container-query passes.
    assert_eq!(inline_style_declaration_parse_count(), 2);
  }

  #[test]
  fn container_query_prefers_self_container_over_ancestor() {
    reset_container_query_memo();
    let mut containers = HashMap::new();
    containers.insert(
      1,
      ContainerQueryInfo {
        inline_size: 400.0,
        block_size: 0.0,
        container_type: ContainerType::Size,
        names: Vec::new(),
        font_size: 16.0,
        styles: Arc::new(ComputedStyle::default()),
      },
    );
    containers.insert(
      2,
      ContainerQueryInfo {
        inline_size: 600.0,
        block_size: 0.0,
        container_type: ContainerType::Size,
        names: Vec::new(),
        font_size: 16.0,
        styles: Arc::new(ComputedStyle::default()),
      },
    );
    let ctx = ContainerQueryContext {
      base_media: MediaContext::default(),
      containers,
    };

    let condition = ContainerCondition {
      name: None,
      query_list: vec![ContainerQuery::Size(
        MediaQuery::parse("(min-width: 500px)").expect("media query"),
      )],
    };
    let ancestor_ids = vec![0, 1];

    let (container_id, _) = ctx
      .find_container(2, &ancestor_ids, &condition)
      .expect("container");
    assert_eq!(container_id, 2);
    assert!(ctx.matches(2, &ancestor_ids, &[condition]));
  }

  #[test]
  fn container_query_nearest_named_container_wins() {
    reset_container_query_memo();
    let mut containers = HashMap::new();
    containers.insert(
      1,
      ContainerQueryInfo {
        inline_size: 400.0,
        block_size: 0.0,
        container_type: ContainerType::Size,
        names: vec!["foo".to_string()],
        font_size: 16.0,
        styles: Arc::new(ComputedStyle::default()),
      },
    );
    containers.insert(
      2,
      ContainerQueryInfo {
        inline_size: 600.0,
        block_size: 0.0,
        container_type: ContainerType::Size,
        names: vec!["foo".to_string()],
        font_size: 16.0,
        styles: Arc::new(ComputedStyle::default()),
      },
    );
    let ctx = ContainerQueryContext {
      base_media: MediaContext::default(),
      containers,
    };

    let condition = ContainerCondition {
      name: Some("foo".to_string()),
      query_list: vec![ContainerQuery::Size(
        MediaQuery::parse("(min-width: 500px)").expect("media query"),
      )],
    };
    let ancestor_ids = vec![0, 1];

    let (container_id, _) = ctx
      .find_container(2, &ancestor_ids, &condition)
      .expect("container");
    assert_eq!(container_id, 2);
    assert!(ctx.matches(2, &ancestor_ids, &[condition]));
  }

  #[test]
  fn container_query_container_type_gating_respected() {
    reset_container_query_memo();
    let mut containers = HashMap::new();
    containers.insert(
      1,
      ContainerQueryInfo {
        inline_size: 600.0,
        block_size: 0.0,
        container_type: ContainerType::Size,
        names: Vec::new(),
        font_size: 16.0,
        styles: Arc::new(ComputedStyle::default()),
      },
    );
    containers.insert(
      2,
      ContainerQueryInfo {
        inline_size: 800.0,
        block_size: 0.0,
        container_type: ContainerType::Style,
        names: Vec::new(),
        font_size: 16.0,
        styles: Arc::new(ComputedStyle::default()),
      },
    );
    let ctx = ContainerQueryContext {
      base_media: MediaContext::default(),
      containers,
    };

    let condition = ContainerCondition {
      name: None,
      query_list: vec![ContainerQuery::Size(
        MediaQuery::parse("(min-width: 500px)").expect("media query"),
      )],
    };
    let ancestor_ids = vec![0, 1];

    let (container_id, _) = ctx
      .find_container(2, &ancestor_ids, &condition)
      .expect("container");
    assert_eq!(container_id, 1);
    assert!(ctx.matches(2, &ancestor_ids, &[condition]));
  }

  #[test]
  fn rule_index_deduplicates_identical_selectors_in_rule() {
    let stylesheet = parse_stylesheet(".foo, .foo { color: red; }").unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);

    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(
          rule.layer_order.as_ref(),
          DOCUMENT_TREE_SCOPE_PREFIX,
        ),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();

    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);
    assert_eq!(index.selectors.len(), 1);
    let class_bucket_len = index
      .by_class
      .get(&selector_bucket_class("foo"))
      .map(|v| v.len())
      .unwrap_or(0);
    assert_eq!(class_bucket_len, 1);
  }

  #[test]
  fn rule_index_anchors_required_and_selectors_under_single_key() {
    let stylesheet = parse_stylesheet("div.foo.bar[data-x] { color: red; }").unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);

    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(rule.layer_order.as_ref(), DOCUMENT_TREE_SCOPE_PREFIX),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();

    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);
    assert_eq!(index.selectors.len(), 1);

    let count_bucket_occurrences =
      |map: &SelectorBucketMap<Vec<usize>>| -> usize { map.values().flatten().filter(|&&i| i == 0).count() };
    let total_occurrences = count_bucket_occurrences(&index.by_id)
      + count_bucket_occurrences(&index.by_class)
      + count_bucket_occurrences(&index.by_tag)
      + count_bucket_occurrences(&index.by_attr)
      + index.universal.iter().filter(|&&i| i == 0).count();
    assert_eq!(total_occurrences, 1);

    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("class".to_string(), "foo bar".to_string()),
          ("data-x".to_string(), "1".to_string()),
        ],
      },
      children: vec![],
    };
    let mut candidates = Vec::new();
    let mut seen = CandidateSet::new(index.selectors.len());
    let mut stats = CandidateStats::default();
    let mut merge = CandidateMergeScratch::default();
    let mut class_keys: Vec<SelectorBucketKey> = Vec::new();
    let mut attr_keys: Vec<SelectorBucketKey> = Vec::new();
    let node_keys = node_selector_keys(&node, &mut class_keys, &mut attr_keys);
    index.selector_candidates(
      &node,
      node_keys,
      None,
      QuirksMode::NoQuirks,
      &mut candidates,
      &mut seen,
      &mut stats,
      &mut merge,
    );
    assert_eq!(candidates.as_slice(), &[0usize]);
  }

  #[test]
  fn rule_index_indexes_is_selector_list_by_subject_keys() {
    let stylesheet = parse_stylesheet(":is(.a, .b) { color: red; }").unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);

    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(rule.layer_order.as_ref(), DOCUMENT_TREE_SCOPE_PREFIX),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();

    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);
    assert_eq!(index.selectors.len(), 1);
    assert!(
      index.universal.is_empty(),
      ":is(.a, .b) selector should not fall back to the universal bucket"
    );

    let class_a = selector_bucket_class("a");
    let class_b = selector_bucket_class("b");
    assert_eq!(
      index
        .by_class
        .get(&class_a)
        .expect("class a bucket")
        .as_slice(),
      &[0usize]
    );
    assert_eq!(
      index
        .by_class
        .get(&class_b)
        .expect("class b bucket")
        .as_slice(),
      &[0usize]
    );

    for cls in ["a", "b"] {
      let node = DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("class".to_string(), cls.to_string())],
        },
        children: vec![],
      };
      let mut candidates = Vec::new();
      let mut seen = CandidateSet::new(index.selectors.len());
      let mut stats = CandidateStats::default();
      let mut merge = CandidateMergeScratch::default();
      let mut class_keys: Vec<SelectorBucketKey> = Vec::new();
      let mut attr_keys: Vec<SelectorBucketKey> = Vec::new();
      let node_keys = node_selector_keys(&node, &mut class_keys, &mut attr_keys);
      index.selector_candidates(
        &node,
        node_keys,
        None,
        QuirksMode::NoQuirks,
        &mut candidates,
        &mut seen,
        &mut stats,
        &mut merge,
      );
      assert_eq!(candidates.as_slice(), &[0usize], "selector should match .{cls}");
    }
  }

  #[test]
  fn rule_index_indexes_nth_child_of_selector_list_by_subject_keys() {
    let stylesheet = parse_stylesheet(":nth-child(odd of .a, .b) { color: red; }").unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);

    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(
          rule.layer_order.as_ref(),
          DOCUMENT_TREE_SCOPE_PREFIX,
        ),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();

    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);
    assert_eq!(index.selectors.len(), 1);
    assert!(
      index.universal.is_empty(),
      ":nth-child(of …) selector should not fall back to the universal bucket"
    );

    let class_a = selector_bucket_class("a");
    let class_b = selector_bucket_class("b");
    assert_eq!(
      index
        .by_class
        .get(&class_a)
        .expect("class a bucket")
        .as_slice(),
      &[0usize]
    );
    assert_eq!(
      index
        .by_class
        .get(&class_b)
        .expect("class b bucket")
        .as_slice(),
      &[0usize]
    );

    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "b".to_string())],
      },
      children: vec![],
    };
    let mut candidates = Vec::new();
    let mut seen = CandidateSet::new(index.selectors.len());
    let mut stats = CandidateStats::default();
    let mut merge = CandidateMergeScratch::default();
    let mut class_keys: Vec<SelectorBucketKey> = Vec::new();
    let mut attr_keys: Vec<SelectorBucketKey> = Vec::new();
    let node_keys = node_selector_keys(&node, &mut class_keys, &mut attr_keys);
    index.selector_candidates(
      &node,
      node_keys,
      None,
      QuirksMode::NoQuirks,
      &mut candidates,
      &mut seen,
      &mut stats,
      &mut merge,
    );
    assert_eq!(candidates.as_slice(), &[0usize]);
    assert_eq!(stats.by_class, 1);
    assert_eq!(stats.universal, 0);
  }

  #[test]
  fn rule_index_indexes_nth_child_of_pseudo_element_subject_keys() {
    let stylesheet = parse_stylesheet(
      r#"
        :nth-child(odd of .a, .b)::before { content: "x"; }
      "#,
    )
    .unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);

    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(
          rule.layer_order.as_ref(),
          DOCUMENT_TREE_SCOPE_PREFIX,
        ),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();

    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);
    assert_eq!(index.pseudo_selectors.len(), 1);

    let before_bucket = index
      .pseudo_buckets
      .get(&PseudoElement::Before)
      .expect("::before bucket");
    assert!(
      before_bucket.universal.is_empty(),
      "nth-child(of …)::before should not fall back to universal bucket"
    );

    let class_a = selector_bucket_class("a");
    let class_b = selector_bucket_class("b");
    assert_eq!(
      before_bucket
        .by_class
        .get(&class_a)
        .expect("class a bucket")
        .as_slice(),
      &[0usize]
    );
    assert_eq!(
      before_bucket
        .by_class
        .get(&class_b)
        .expect("class b bucket")
        .as_slice(),
      &[0usize]
    );

    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "b".to_string())],
      },
      children: vec![],
    };
    let mut candidates = Vec::new();
    let mut seen = CandidateSet::new(index.pseudo_selectors.len());
    let mut stats = CandidateStats::default();
    let mut class_keys: Vec<SelectorBucketKey> = Vec::new();
    let mut attr_keys: Vec<SelectorBucketKey> = Vec::new();
    let node_keys = node_selector_keys(&node, &mut class_keys, &mut attr_keys);
    index.pseudo_candidates(
      &node,
      &PseudoElement::Before,
      node_keys,
      None,
      QuirksMode::NoQuirks,
      &mut candidates,
      &mut seen,
      &mut stats,
    );
    assert_eq!(candidates.as_slice(), &[0usize]);
    assert_eq!(stats.by_class, 1);
    assert_eq!(stats.universal, 0);
  }

  #[test]
  fn rule_index_indexes_pseudo_selectors_by_subject_compound() {
    let stylesheet = parse_stylesheet(
      r#"
        .a::before { content: "x"; }
        div.foo::after { content: "y"; }
      "#,
    )
    .unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);

    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(
          rule.layer_order.as_ref(),
          DOCUMENT_TREE_SCOPE_PREFIX,
        ),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();

    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);
    assert_eq!(index.pseudo_selectors.len(), 2);

    let before_bucket = index
      .pseudo_buckets
      .get(&PseudoElement::Before)
      .expect("::before bucket");
    assert!(before_bucket.universal.is_empty());
    assert_eq!(
      before_bucket
        .by_class
        .get(&selector_bucket_class("a"))
        .expect("class a bucket")
        .as_slice(),
      &[0usize]
    );

    let after_bucket = index
      .pseudo_buckets
      .get(&PseudoElement::After)
      .expect("::after bucket");
    assert!(after_bucket.universal.is_empty());
    assert_eq!(
      after_bucket
        .by_class
        .get(&selector_bucket_class("foo"))
        .expect("class foo bucket")
        .as_slice(),
      &[1usize]
    );
    assert!(
      after_bucket
        .by_tag
        .get(&selector_bucket_tag("div"))
        .is_none(),
      "required-AND subject compound should be indexed under a single anchor key"
    );

    // An element without the `.a` class should not consider `.a::before` a candidate.
    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let mut candidates = Vec::new();
    let mut seen = CandidateSet::new(index.pseudo_selectors.len());
    let mut stats = CandidateStats::default();
    let mut class_keys: Vec<SelectorBucketKey> = Vec::new();
    let mut attr_keys: Vec<SelectorBucketKey> = Vec::new();
    let node_keys = node_selector_keys(&node, &mut class_keys, &mut attr_keys);
    index.pseudo_candidates(
      &node,
      &PseudoElement::Before,
      node_keys,
      None,
      QuirksMode::NoQuirks,
      &mut candidates,
      &mut seen,
      &mut stats,
    );
    assert!(candidates.is_empty());
  }

  #[test]
  fn rule_index_deduplicates_identical_pseudo_element_selectors_in_rule() {
    let stylesheet = parse_stylesheet("div::before, div::before { content: \"x\"; }").unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);

    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(
          rule.layer_order.as_ref(),
          DOCUMENT_TREE_SCOPE_PREFIX,
        ),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();

    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);
    assert_eq!(index.pseudo_selectors.len(), 1);
  }

  #[test]
  fn rule_index_deduplicates_identical_slotted_selectors_in_rule() {
    let stylesheet = parse_stylesheet("::slotted(.foo), ::slotted(.foo) { color: red; }").unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);

    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(
          rule.layer_order.as_ref(),
          DOCUMENT_TREE_SCOPE_PREFIX,
        ),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();

    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);
    assert_eq!(index.slotted_selectors.len(), 1);
  }

  #[test]
  fn parse_slotted_prelude_synthesizes_expected_selector() {
    let stylesheet = parse_stylesheet(
      r"
        :host(.foo) ::slotted(.a) { color: red; }
        slot[name=foo]::slotted(.a) { color: red; }
      ",
    )
    .unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);

    let selector_host = collected[0]
      .rule
      .selectors
      .slice()
      .first()
      .expect("host slotted selector");
    let selector_named = collected[1]
      .rule
      .selectors
      .slice()
      .first()
      .expect("named slotted selector");

    let prelude_host = parse_slotted_prelude(selector_host).expect("host prelude");
    let prelude_named = parse_slotted_prelude(selector_named).expect("named prelude");

    assert_eq!(prelude_host.to_css_string(), ":host(.foo) *");
    assert_eq!(prelude_named.to_css_string(), "slot[name=foo]");

    use selectors::parser::Combinator;
    use selectors::parser::Component;
    for prelude in [&prelude_host, &prelude_named] {
      assert!(
        !prelude
          .iter_raw_match_order()
          .any(|c| matches!(c, Component::Combinator(Combinator::PseudoElement))),
        "slotted preludes should not retain pseudo-element combinators"
      );
      assert!(
        !prelude
          .iter_raw_match_order()
          .any(|c| matches!(c, Component::PseudoElement(..))),
        "slotted preludes should not retain pseudo-elements"
      );
    }
  }

  #[test]
  fn slotted_prelude_matches_assigned_slot_element() {
    let stylesheet = parse_stylesheet(":host(.foo) ::slotted(.a) { color: red; }").unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);
    let selector = collected[0]
      .rule
      .selectors
      .slice()
      .first()
      .expect("slotted selector");
    let prelude = parse_slotted_prelude(selector).expect("prelude selector");

    let host = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "foo".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::ShadowRoot {
          mode: crate::dom::ShadowRootMode::Open,
          delegates_focus: false,
        },
        children: vec![DomNode {
          node_type: DomNodeType::Slot {
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
            assigned: false,
          },
          children: vec![],
        }],
      }],
    };

    let shadow_root = &host.children[0];
    let slot = &shadow_root.children[0];
    let slot_ancestors = [&host, shadow_root];
    let slot_ref = ElementRef::with_ancestors(slot, &slot_ancestors);
    let host_ref = ElementRef::new(&host);

    let mut caches = SelectorCaches::default();
    let cache_epoch = next_selector_cache_epoch();
    caches.set_epoch(cache_epoch);
    let sibling_cache = SiblingListCache::new(cache_epoch);
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      selectors::matching::NeedsSelectorFlags::No,
      selectors::matching::MatchingForInvalidation::No,
    );
    context.extra_data = ShadowMatchData::default().with_sibling_cache(&sibling_cache);

    let matches = with_shadow_host(&mut context, Some(host_ref), |ctx| {
      matches_selector(&prelude, 0, None, &slot_ref, ctx)
    });

    assert!(
      matches,
      "prelude should match the slot when the host matches"
    );
  }

  #[test]
  fn slotted_prelude_matches_named_slot_only() {
    let stylesheet = parse_stylesheet("slot[name=foo]::slotted(.a) { color: red; }").unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);
    let selector = collected[0]
      .rule
      .selectors
      .slice()
      .first()
      .expect("slotted selector");
    let prelude = parse_slotted_prelude(selector).expect("prelude selector");

    let host = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::ShadowRoot {
          mode: crate::dom::ShadowRootMode::Open,
          delegates_focus: false,
        },
        children: vec![
          DomNode {
            node_type: DomNodeType::Slot {
              namespace: HTML_NAMESPACE.to_string(),
              attributes: vec![("name".to_string(), "foo".to_string())],
              assigned: false,
            },
            children: vec![],
          },
          DomNode {
            node_type: DomNodeType::Slot {
              namespace: HTML_NAMESPACE.to_string(),
              attributes: vec![("name".to_string(), "bar".to_string())],
              assigned: false,
            },
            children: vec![],
          },
        ],
      }],
    };

    let shadow_root = &host.children[0];
    let slot_foo = &shadow_root.children[0];
    let slot_bar = &shadow_root.children[1];
    let slot_ancestors = [&host, shadow_root];
    let slot_foo_ref = ElementRef::with_ancestors(slot_foo, &slot_ancestors);
    let slot_bar_ref = ElementRef::with_ancestors(slot_bar, &slot_ancestors);
    let host_ref = ElementRef::new(&host);

    let mut caches = SelectorCaches::default();
    let cache_epoch = next_selector_cache_epoch();
    caches.set_epoch(cache_epoch);
    let sibling_cache = SiblingListCache::new(cache_epoch);
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      selectors::matching::NeedsSelectorFlags::No,
      selectors::matching::MatchingForInvalidation::No,
    );
    context.extra_data = ShadowMatchData::default().with_sibling_cache(&sibling_cache);

    let foo_matches = with_shadow_host(&mut context, Some(host_ref), |ctx| {
      matches_selector(&prelude, 0, None, &slot_foo_ref, ctx)
    });
    let bar_matches = with_shadow_host(&mut context, Some(host_ref), |ctx| {
      matches_selector(&prelude, 0, None, &slot_bar_ref, ctx)
    });

    assert!(foo_matches);
    assert!(!bar_matches);
  }

  #[test]
  fn rule_index_bucket_keys_preserve_case_sensitivity_for_id_and_class() {
    let stylesheet = parse_stylesheet(
      "#Foo { color: red; } #foo { color: blue; } .Bar { color: green; } .bar { color: yellow; }",
    )
    .unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);

    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(
          rule.layer_order.as_ref(),
          DOCUMENT_TREE_SCOPE_PREFIX,
        ),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();

    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);
    assert!(index.by_id.contains_key(&selector_bucket_id("Foo")));
    assert!(index.by_id.contains_key(&selector_bucket_id("foo")));
    assert!(index.by_class.contains_key(&selector_bucket_class("Bar")));
    assert!(index.by_class.contains_key(&selector_bucket_class("bar")));

    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("id".to_string(), "Foo".to_string()),
          ("class".to_string(), "Bar".to_string()),
        ],
      },
      children: vec![],
    };

    let mut class_keys: Vec<SelectorBucketKey> = Vec::new();
    let mut attr_keys: Vec<SelectorBucketKey> = Vec::new();
    let node_keys = node_selector_keys(&node, &mut class_keys, &mut attr_keys);
    let mut out = Vec::new();
    let mut seen = CandidateSet::new(index.selectors.len());
    let mut stats = CandidateStats::default();
    let mut merge = CandidateMergeScratch::default();
    index.selector_candidates(
      &node,
      node_keys,
      None,
      QuirksMode::NoQuirks,
      &mut out,
      &mut seen,
      &mut stats,
      &mut merge,
    );

    let mut selected: Vec<String> = out
      .iter()
      .map(|idx| index.selectors[*idx].selector.to_css_string())
      .collect();
    selected.sort();

    assert_eq!(selected.len(), 2);
    assert!(selected.contains(&"#Foo".to_string()));
    assert!(selected.contains(&".Bar".to_string()));
  }

  #[test]
  fn rule_index_bucket_keys_fold_ascii_case_for_tag_and_attribute_names() {
    let stylesheet = parse_stylesheet("DIV { color: red; } [data-Test] { color: blue; }").unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);
    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(
          rule.layer_order.as_ref(),
          DOCUMENT_TREE_SCOPE_PREFIX,
        ),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();
    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);

    let node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "DiV".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("DATA-TEST".to_string(), "1".to_string())],
      },
      children: vec![],
    };

    let mut class_keys: Vec<SelectorBucketKey> = Vec::new();
    let mut attr_keys: Vec<SelectorBucketKey> = Vec::new();
    let node_keys = node_selector_keys(&node, &mut class_keys, &mut attr_keys);
    let mut out = Vec::new();
    let mut seen = CandidateSet::new(index.selectors.len());
    let mut stats = CandidateStats::default();
    let mut merge = CandidateMergeScratch::default();
    index.selector_candidates(
      &node,
      node_keys,
      None,
      QuirksMode::NoQuirks,
      &mut out,
      &mut seen,
      &mut stats,
      &mut merge,
    );

    assert_eq!(out.len(), 2);
  }

  #[test]
  fn ancestor_bloom_filter_is_built_incrementally() {
    let _lock = cascade_global_test_lock();
    let _ancestor_bloom = AncestorBloomEnabledGuard::new(true);

    let depth = 4usize;
    let mut current = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    for _ in 0..(depth - 2) {
      current = DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![current],
      };
    }

    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("id".to_string(), "root".to_string())],
      },
      children: vec![current],
    };

    let stylesheet = parse_stylesheet("#root div { color: rgb(1, 2, 3); }").unwrap();

    ANCESTOR_BLOOM_HASH_INSERTS.with(|counter| counter.set(0));
    let styled = apply_styles(&dom, &stylesheet);

    let mut leaf = &styled;
    for _ in 0..(depth - 1) {
      leaf = leaf.children.first().expect("child");
    }
    assert_eq!(leaf.styles.color, Rgba::rgb(1, 2, 3));

    let inserts = ANCESTOR_BLOOM_HASH_INSERTS.with(|counter| counter.get());
    assert!(inserts > 0);
    assert!(
      inserts <= depth as u64,
      "expected O(n) ancestor bloom-filter hash insertions, got {inserts} for depth={depth}"
    );
  }

  #[test]
  fn selector_candidate_cache_matches_uncached() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "DiV".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("id".to_string(), "Foo".to_string()),
          ("ClAsS".to_string(), "a    b\tc".to_string()),
          ("DATA-TEST".to_string(), "1".to_string()),
          ("data-other".to_string(), "2".to_string()),
        ],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![
            ("ID".to_string(), "bar".to_string()),
            ("class".to_string(), "b".to_string()),
            ("data-test".to_string(), "x".to_string()),
          ],
        },
        children: vec![],
      }],
    };

    let id_map = enumerate_dom_ids(&dom);
    let dom_maps = DomMaps::new(&dom, id_map);

    let stylesheet = parse_stylesheet(
      r#"
        #Foo { color: red; }
        #foo { color: blue; }
        .a { color: green; }
        .b { color: yellow; }
        div { color: black; }
        SPAN { color: black; }
        [data-Test] { color: black; }
        [DATA-OTHER] { color: black; }
        * { color: black; }

        div::before { content: 'x'; }
        .b::before { content: 'y'; }

        slot::slotted(.b) { color: red; }
      "#,
    )
    .expect("stylesheet");
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);
    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(&rule.layer_order, DOCUMENT_TREE_SCOPE_PREFIX),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();

    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);

    let pseudo = PseudoElement::Before;
    for (node_id, node) in [(1usize, &dom), (2usize, &dom.children[0])] {
      let cached_keys = dom_maps.selector_keys(node_id);
      let mut class_keys: Vec<SelectorBucketKey> = Vec::new();
      let mut attr_keys: Vec<SelectorBucketKey> = Vec::new();
      let uncached_keys = node_selector_keys(node, &mut class_keys, &mut attr_keys);

      let mut cached: Vec<usize> = Vec::new();
      let mut uncached: Vec<usize> = Vec::new();
      let mut cached_seen = CandidateSet::new(index.selectors.len());
      let mut uncached_seen = CandidateSet::new(index.selectors.len());
      let mut cached_stats = CandidateStats::default();
      let mut uncached_stats = CandidateStats::default();
      let mut cached_merge = CandidateMergeScratch::default();
      let mut uncached_merge = CandidateMergeScratch::default();
      index.selector_candidates(
        node,
        cached_keys,
        None,
        QuirksMode::NoQuirks,
        &mut cached,
        &mut cached_seen,
        &mut cached_stats,
        &mut cached_merge,
      );
      index.selector_candidates(
        node,
        uncached_keys,
        None,
        QuirksMode::NoQuirks,
        &mut uncached,
        &mut uncached_seen,
        &mut uncached_stats,
        &mut uncached_merge,
      );
      assert_eq!(
        cached, uncached,
        "base selector candidates differ for node_id={node_id}"
      );

      let mut cached: Vec<usize> = Vec::new();
      let mut uncached: Vec<usize> = Vec::new();
      let mut cached_seen = CandidateSet::new(index.pseudo_selectors.len());
      let mut uncached_seen = CandidateSet::new(index.pseudo_selectors.len());
      let mut cached_stats = CandidateStats::default();
      let mut uncached_stats = CandidateStats::default();
      index.pseudo_candidates(
        node,
        &pseudo,
        cached_keys,
        None,
        QuirksMode::NoQuirks,
        &mut cached,
        &mut cached_seen,
        &mut cached_stats,
      );
      index.pseudo_candidates(
        node,
        &pseudo,
        uncached_keys,
        None,
        QuirksMode::NoQuirks,
        &mut uncached,
        &mut uncached_seen,
        &mut uncached_stats,
      );
      assert_eq!(
        cached, uncached,
        "pseudo selector candidates differ for node_id={node_id}"
      );

      let mut cached: Vec<usize> = Vec::new();
      let mut uncached: Vec<usize> = Vec::new();
      let mut cached_seen = CandidateSet::new(index.slotted_selectors.len());
      let mut uncached_seen = CandidateSet::new(index.slotted_selectors.len());
      let mut cached_stats = CandidateStats::default();
      let mut uncached_stats = CandidateStats::default();
      let mut cached_merge = CandidateMergeScratch::default();
      let mut uncached_merge = CandidateMergeScratch::default();
      index.slotted_candidates(
        node,
        cached_keys,
        None,
        QuirksMode::NoQuirks,
        &mut cached,
        &mut cached_seen,
        &mut cached_stats,
        &mut cached_merge,
      );
      index.slotted_candidates(
        node,
        uncached_keys,
        None,
        QuirksMode::NoQuirks,
        &mut uncached,
        &mut uncached_seen,
        &mut uncached_stats,
        &mut uncached_merge,
      );
      assert_eq!(
        cached, uncached,
        "slotted selector candidates differ for node_id={node_id}"
      );
    }
  }

  #[test]
  fn selector_bloom_pruning_has_list_is_or() {
    crate::dom::set_selector_bloom_enabled(true);

    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("class".to_string(), "a".to_string())],
        },
        children: vec![],
      }],
    };

    let id_map = enumerate_dom_ids(&dom);
    let mut dom_maps = DomMaps::new(&dom, id_map);
    dom_maps.ensure_selector_blooms(&dom);
    let blooms = dom_maps.selector_blooms().expect("bloom map");
    let summary = blooms.summary_for_id(1).expect("summary");

    let stylesheet = parse_stylesheet("div:has(.a, .b) { color: red; }").unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);
    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(
          rule.layer_order.as_ref(),
          DOCUMENT_TREE_SCOPE_PREFIX,
        ),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();
    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);
    assert!(index.has_has_requirements);

    let mut candidates: Vec<usize> = Vec::new();
    let mut seen = CandidateSet::new(index.selectors.len());
    let mut stats = CandidateStats::default();
    let mut merge = CandidateMergeScratch::default();
    index.selector_candidates(
      &dom,
      dom_maps.selector_keys(1),
      Some(summary),
      QuirksMode::NoQuirks,
      &mut candidates,
      &mut seen,
      &mut stats,
      &mut merge,
    );
    assert_eq!(candidates, vec![0]);
  }

  #[test]
  fn selector_bloom_pruning_does_not_descend_into_is() {
    crate::dom::set_selector_bloom_enabled(true);

    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "b".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("class".to_string(), "c".to_string())],
        },
        children: vec![],
      }],
    };

    let id_map = enumerate_dom_ids(&dom);
    let mut dom_maps = DomMaps::new(&dom, id_map);
    dom_maps.ensure_selector_blooms(&dom);
    let blooms = dom_maps.selector_blooms().expect("bloom map");
    let summary = blooms.summary_for_id(1).expect("summary");

    let stylesheet = parse_stylesheet("div:is(:has(.a), .b) { color: red; }").unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);
    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(
          rule.layer_order.as_ref(),
          DOCUMENT_TREE_SCOPE_PREFIX,
        ),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();
    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);
    assert!(!index.has_has_requirements);

    let mut candidates: Vec<usize> = Vec::new();
    let mut seen = CandidateSet::new(index.selectors.len());
    let mut stats = CandidateStats::default();
    let mut merge = CandidateMergeScratch::default();
    index.selector_candidates(
      &dom,
      dom_maps.selector_keys(1),
      Some(summary),
      QuirksMode::NoQuirks,
      &mut candidates,
      &mut seen,
      &mut stats,
      &mut merge,
    );
    assert_eq!(candidates, vec![0]);
  }

  #[test]
  fn selector_bloom_pruning_does_not_descend_into_not() {
    crate::dom::set_selector_bloom_enabled(true);

    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("class".to_string(), "b".to_string())],
        },
        children: vec![],
      }],
    };

    let id_map = enumerate_dom_ids(&dom);
    let mut dom_maps = DomMaps::new(&dom, id_map);
    dom_maps.ensure_selector_blooms(&dom);
    let blooms = dom_maps.selector_blooms().expect("bloom map");
    let summary = blooms.summary_for_id(1).expect("summary");

    let stylesheet = parse_stylesheet("div:not(:has(.a)) { color: red; }").unwrap();
    let media_ctx = MediaContext::default();
    let collected = stylesheet.collect_style_rules(&media_ctx);
    let rules: Vec<CascadeRule<'_>> = collected
      .iter()
      .enumerate()
      .map(|(order, rule)| CascadeRule {
        origin: StyleOrigin::Author,
        order,
        rule: rule.rule,
        layer_order: layer_order_with_tree_scope(
          rule.layer_order.as_ref(),
          DOCUMENT_TREE_SCOPE_PREFIX,
        ),
        container_conditions: rule.container_conditions.clone(),
        scopes: rule.scopes.clone(),
        scope_signature: ScopeSignature::compute(&rule.scopes),
        scope: RuleScope::Document,
        starting_style: rule.starting_style,
      })
      .collect();
    let index = RuleIndex::new(rules, QuirksMode::NoQuirks);
    assert!(!index.has_has_requirements);

    let mut candidates: Vec<usize> = Vec::new();
    let mut seen = CandidateSet::new(index.selectors.len());
    let mut stats = CandidateStats::default();
    let mut merge = CandidateMergeScratch::default();
    index.selector_candidates(
      &dom,
      dom_maps.selector_keys(1),
      Some(summary),
      QuirksMode::NoQuirks,
      &mut candidates,
      &mut seen,
      &mut stats,
      &mut merge,
    );
    assert_eq!(candidates, vec![0]);
  }

  #[test]
  fn selector_bloom_summaries_skipped_for_unprunable_has_sibling_direction() {
    crate::dom::set_selector_bloom_enabled(true);

    let mut children: Vec<DomNode> = Vec::new();
    children.push(DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("id".to_string(), "a".to_string())],
      },
      children: vec![],
    });
    children.push(DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "x".to_string())],
      },
      children: vec![],
    });
    let needed_children = SELECTOR_BLOOM_MIN_NODES.saturating_sub(1 + children.len());
    for _ in 0..needed_children {
      children.push(DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      });
    }
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children,
    };
    assert!(enumerate_dom_ids(&dom).len() >= SELECTOR_BLOOM_MIN_NODES);

    let stylesheet = parse_stylesheet("#a:has(+ .x) { color: rgb(1, 2, 3); }").unwrap();
    let style_set = StyleSet::from_document(stylesheet);
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let mut prepared =
      PreparedCascade::new_for_style_set(&dom, &style_set, &media_ctx, None, None, None, false)
        .expect("build prepared cascade");
    let styled = prepared
      .apply(None, None, None, None, None)
      .expect("apply cascade");

    assert_eq!(
      styled.children.first().expect("first child").styles.color,
      Rgba::rgb(1, 2, 3)
    );
    assert!(!prepared.needs_selector_bloom_summaries);
    assert!(prepared.dom_maps.selector_blooms().is_none());
  }

  #[test]
  fn quirks_mode_has_attribute_selector_builds_blooms_and_prunes() {
    crate::dom::set_selector_bloom_enabled(true);

    let div_count = 40usize;
    let mut divs: Vec<DomNode> = Vec::with_capacity(div_count);
    for idx in 0..div_count {
      let id = match idx {
        0 => Some("hit"),
        1 => Some("miss"),
        _ => None,
      };
      let mut span_attrs: Vec<(String, String)> = Vec::new();
      if idx == 0 {
        span_attrs.push(("data-x".to_string(), String::new()));
      }
      let span = DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: span_attrs,
        },
        children: vec![],
      };
      let mut div_attrs: Vec<(String, String)> = Vec::new();
      if let Some(id) = id {
        div_attrs.push(("id".to_string(), id.to_string()));
      }
      divs.push(DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: div_attrs,
        },
        children: vec![span],
      });
    }

    let dom = DomNode {
      node_type: DomNodeType::Document {
        quirks_mode: QuirksMode::Quirks,
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "body".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: divs,
      }],
    };
    assert!(enumerate_dom_ids(&dom).len() >= SELECTOR_BLOOM_MIN_NODES);

    let stylesheet =
      parse_stylesheet("div:is(:has([data-x]), .noop) { display: inline; }").unwrap();
    let style_set = StyleSet::from_document(stylesheet);
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let mut prepared =
      PreparedCascade::new_for_style_set(&dom, &style_set, &media_ctx, None, None, None, false)
        .expect("build prepared cascade");
    let styled = prepared
      .apply(None, None, None, None, None)
      .expect("apply cascade");

    assert_eq!(
      find_styled_node_by_id(&styled, "hit")
        .expect("hit div")
        .styles
        .display,
      Display::Inline
    );
    assert_eq!(
      find_styled_node_by_id(&styled, "miss")
        .expect("miss div")
        .styles
        .display,
      Display::Block
    );

    assert!(prepared.needs_selector_bloom_summaries);
    assert!(
      prepared.dom_maps.selector_blooms().is_some(),
      "expected selector bloom summaries to be built in quirks mode for :has([data-x])"
    );

    let counters = crate::dom::capture_has_counters();
    assert_eq!(
      counters.evals, div_count as u64,
      "expected :has to be evaluated once per <div>"
    );
    assert_eq!(counters.cache_hits, 0);
    assert_eq!(counters.evaluated, 1);
    assert_eq!(counters.filter_prunes, 0);
    assert_eq!(counters.summary_prunes(), (div_count - 1) as u64);
  }

  #[test]
  fn cascade_profile_tracks_bloom_fast_rejects() {
    let _lock = cascade_global_test_lock();
    crate::dom::set_selector_bloom_enabled(true);

    let prev_profile_enabled = cascade_profile_enabled();
    set_cascade_profile_enabled(true);
    reset_cascade_profile();

    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "b".to_string())],
      },
      children: vec![],
    };
    let stylesheet = parse_stylesheet(".a .b { color: red; }").unwrap();
    let _styled = apply_styles(&dom, &stylesheet);

    let stats = capture_cascade_profile();
    assert!(
      stats.selector_attempts_total > 0,
      "expected selector attempts to be recorded: {:?}",
      stats
    );
    assert!(
      stats.selector_bloom_fast_rejects > 0,
      "expected at least one bloom fast reject: {:?}",
      stats
    );
    assert_eq!(
      stats.selector_attempts_after_bloom + stats.selector_bloom_fast_rejects,
      stats.selector_attempts_total,
      "selector attempt accounting should be lossless: {:?}",
      stats
    );

    set_cascade_profile_enabled(prev_profile_enabled);
  }

  #[test]
  fn ancestor_bloom_toggle_preserves_cascade_output() {
    let _lock = cascade_global_test_lock();
    let _ancestor_bloom = AncestorBloomEnabledGuard::new(true);
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("id".to_string(), "host".to_string()),
          ("class".to_string(), "container".to_string()),
        ],
      },
      children: vec![
        DomNode {
          node_type: DomNodeType::ShadowRoot {
            mode: crate::dom::ShadowRootMode::Open,
            delegates_focus: false,
          },
          children: vec![
            DomNode {
              node_type: DomNodeType::Element {
                tag_name: "style".to_string(),
                namespace: HTML_NAMESPACE.to_string(),
                attributes: vec![],
              },
              children: vec![DomNode {
                node_type: DomNodeType::Text {
                  content: "::slotted(.assigned) { background-color: rgb(1, 2, 3); }\
slot[name=\"s\"]::slotted(.assigned) { color: rgb(4, 5, 6); }"
                    .to_string(),
                },
                children: vec![],
              }],
            },
            DomNode {
              node_type: DomNodeType::Slot {
                namespace: HTML_NAMESPACE.to_string(),
                attributes: vec![("name".to_string(), "s".to_string())],
                assigned: false,
              },
              children: vec![],
            },
          ],
        },
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "span".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![
              ("slot".to_string(), "s".to_string()),
              ("class".to_string(), "assigned".to_string()),
              ("data-x".to_string(), "1".to_string()),
            ],
          },
          children: vec![DomNode {
            node_type: DomNodeType::Element {
              tag_name: "div".to_string(),
              namespace: HTML_NAMESPACE.to_string(),
              attributes: vec![
                ("class".to_string(), "leaf".to_string()),
                ("data-test".to_string(), "yes".to_string()),
              ],
            },
            children: vec![],
          }],
        },
      ],
    };

    let stylesheet = parse_stylesheet(
      r#"
        /* Descendant selectors that should be fast-rejected when ancestors are absent. */
        .assigned .leaf { margin-left: 5px; }
        .missing .leaf { margin-left: 10px; }
        [data-x] .leaf { padding-left: 3px; }

        /* Ensure pseudo-element cascade also stays identical. */
        .leaf::before { content: 'x'; color: rgb(7, 8, 9); }
      "#,
    )
    .expect("parse stylesheet");
    let media = MediaContext::screen(800.0, 600.0);

    crate::dom::set_ancestor_bloom_enabled(true);
    let styled_on = apply_styles_with_media(&dom, &stylesheet, &media);
    crate::dom::set_ancestor_bloom_enabled(false);
    let styled_off = apply_styles_with_media(&dom, &stylesheet, &media);
    crate::dom::set_ancestor_bloom_enabled(true);

    assert_styled_trees_equal(&styled_on, &styled_off);

    let slotted = styled_on.children.get(1).expect("slotted node");
    assert_eq!(slotted.styles.background_color, Rgba::rgb(1, 2, 3));
    let leaf = slotted.children.first().expect("leaf node");
    let before = leaf.before_styles.as_ref().expect("generated ::before");
    assert_eq!(before.color, Rgba::rgb(7, 8, 9));
  }

  struct ShadowBloomScopingGuard {
    prev: bool,
  }

  impl ShadowBloomScopingGuard {
    fn new(enabled: bool) -> Self {
      let prev = ancestor_bloom_shadow_scoping_enabled();
      set_ancestor_bloom_shadow_scoping_enabled(enabled);
      Self { prev }
    }
  }

  impl Drop for ShadowBloomScopingGuard {
    fn drop(&mut self) {
      set_ancestor_bloom_shadow_scoping_enabled(self.prev);
    }
  }

  fn simple_shadow_dom_fixture(css: &str) -> DomNode {
    let style_node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "style".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Text {
          content: css.to_string(),
        },
        children: vec![],
      }],
    };

    let target = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "span".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "target".to_string())],
      },
      children: vec![],
    };

    let deep_node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "deep".to_string())],
      },
      children: vec![target],
    };

    let inner_node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "inner0".to_string())],
      },
      children: vec![deep_node],
    };

    let shadow_root = DomNode {
      node_type: DomNodeType::ShadowRoot {
        mode: crate::dom::ShadowRootMode::Open,
        delegates_focus: false,
      },
      children: vec![style_node, inner_node],
    };

    DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("id".to_string(), "host".to_string()),
          ("class".to_string(), "outer".to_string()),
        ],
      },
      children: vec![shadow_root],
    }
  }

  fn nested_shadow_dom_fixture(
    depth: usize,
    selector_chain_len: usize,
    selectors_per_depth: usize,
    target_count: usize,
  ) -> DomNode {
    use std::fmt::Write;

    let inner_chain = (0..selector_chain_len)
      .map(|idx| format!(".inner{idx}"))
      .collect::<Vec<_>>()
      .join(" ");

    let mut css = String::from(".deep .target { color: rgb(10, 20, 30); }\n");
    for outer in 0..depth {
      for idx in 0..selectors_per_depth {
        let _ = write!(
          css,
          ".outer{outer} {inner} .deep .target {{ padding-left: {}px; }}\n",
          (idx % 7) + 1,
          outer = outer,
          inner = inner_chain
        );
      }
    }

    let style_node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "style".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Text { content: css },
        children: vec![],
      }],
    };

    let targets: Vec<DomNode> = (0..target_count)
      .map(|idx| DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![
            ("id".to_string(), format!("t{idx}")),
            ("class".to_string(), "target".to_string()),
          ],
        },
        children: vec![],
      })
      .collect();

    let mut content = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "deep".to_string())],
      },
      children: targets,
    };

    for idx in (0..selector_chain_len).rev() {
      content = DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("class".to_string(), format!("inner{idx}"))],
        },
        children: vec![content],
      };
    }

    let mut nested = content;
    for level in (0..depth).rev() {
      let shadow_root_children = if level + 1 == depth {
        vec![style_node.clone(), nested]
      } else {
        vec![nested]
      };

      let shadow_root = DomNode {
        node_type: DomNodeType::ShadowRoot {
          mode: crate::dom::ShadowRootMode::Open,
          delegates_focus: false,
        },
        children: shadow_root_children,
      };

      nested = DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![
            ("id".to_string(), format!("host{level}")),
            ("class".to_string(), format!("outer{level}")),
          ],
        },
        children: vec![shadow_root],
      };
    }

    nested
  }

  #[test]
  fn ancestor_bloom_shadow_scoping_preserves_cascade_output() {
    let _lock = cascade_global_test_lock();
    let _ancestor_bloom = AncestorBloomEnabledGuard::new(true);

    let dom = simple_shadow_dom_fixture(
      r#"
        .deep .target { color: rgb(10, 20, 30); }
        .inner0 .deep .target { margin-left: 2px; }
        .outer .deep .target { padding-left: 5px; }
        .missing .target { padding-left: 7px; }
        .target::before { content: 'x'; color: rgb(1, 2, 3); }
      "#,
    );

    let stylesheet = StyleSheet::new();
    let styled_without_scoping = {
      let _guard = ShadowBloomScopingGuard::new(false);
      apply_styles(&dom, &stylesheet)
    };
    let styled_with_scoping = {
      let _guard = ShadowBloomScopingGuard::new(true);
      apply_styles(&dom, &stylesheet)
    };

    assert_styled_trees_equal(&styled_with_scoping, &styled_without_scoping);

    let shadow_root = styled_with_scoping.children.first().expect("shadow root");
    let inner = shadow_root.children.get(1).expect("inner wrapper");
    let deep = inner.children.first().expect("deep wrapper");
    let target = deep.children.first().expect("target node");
    assert_eq!(target.styles.color, Rgba::rgb(10, 20, 30));
    assert!(
      target.before_styles.is_some(),
      "expected ::before to be generated for .target"
    );
  }

  #[test]
  fn ancestor_bloom_shadow_scoping_preserves_host_context_outer_ancestors() {
    let _lock = cascade_global_test_lock();
    let _ancestor_bloom = AncestorBloomEnabledGuard::new(true);

    let css = r#"
      body :host-context(.outer) .target { color: rgb(10, 20, 30); }
    "#;

    let style_node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "style".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Text {
          content: css.to_string(),
        },
        children: vec![],
      }],
    };

    let shadow_root = DomNode {
      node_type: DomNodeType::ShadowRoot {
        mode: crate::dom::ShadowRootMode::Open,
        delegates_focus: false,
      },
      children: vec![
        style_node,
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "span".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![
              ("id".to_string(), "target".to_string()),
              ("class".to_string(), "target".to_string()),
            ],
          },
          children: vec![],
        },
      ],
    };

    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "body".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "outer".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("id".to_string(), "host".to_string())],
        },
        children: vec![shadow_root],
      }],
    };

    let stylesheet = StyleSheet::new();
    let styled_without_scoping = {
      let _guard = ShadowBloomScopingGuard::new(false);
      apply_styles(&dom, &stylesheet)
    };
    let styled_with_scoping = {
      let _guard = ShadowBloomScopingGuard::new(true);
      apply_styles(&dom, &stylesheet)
    };

    assert_styled_trees_equal(&styled_with_scoping, &styled_without_scoping);

    let target = find_styled_node_by_id(&styled_with_scoping, "target").expect("target node");
    assert_eq!(target.styles.color, Rgba::rgb(10, 20, 30));
  }

  #[test]
  fn ancestor_bloom_shadow_scoping_preserves_nested_host_context_outer_ancestors() {
    let _lock = cascade_global_test_lock();
    let _ancestor_bloom = AncestorBloomEnabledGuard::new(true);

    let css = r#"
      body :is(:host-context(.outer), .never) .target { color: rgb(10, 20, 30); }
    "#;

    let style_node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "style".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Text {
          content: css.to_string(),
        },
        children: vec![],
      }],
    };

    let shadow_root = DomNode {
      node_type: DomNodeType::ShadowRoot {
        mode: crate::dom::ShadowRootMode::Open,
        delegates_focus: false,
      },
      children: vec![
        style_node,
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "span".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![
              ("id".to_string(), "target".to_string()),
              ("class".to_string(), "target".to_string()),
            ],
          },
          children: vec![],
        },
      ],
    };

    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "body".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "outer".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("id".to_string(), "host".to_string())],
        },
        children: vec![shadow_root],
      }],
    };

    let stylesheet = StyleSheet::new();
    let styled_without_scoping = {
      let _guard = ShadowBloomScopingGuard::new(false);
      apply_styles(&dom, &stylesheet)
    };
    let styled_with_scoping = {
      let _guard = ShadowBloomScopingGuard::new(true);
      apply_styles(&dom, &stylesheet)
    };

    assert_styled_trees_equal(&styled_with_scoping, &styled_without_scoping);

    let target = find_styled_node_by_id(&styled_with_scoping, "target").expect("target node");
    assert_eq!(target.styles.color, Rgba::rgb(10, 20, 30));
  }

  #[test]
  fn ancestor_bloom_shadow_scoping_preserves_host_outer_ancestors() {
    let _lock = cascade_global_test_lock();
    let _ancestor_bloom = AncestorBloomEnabledGuard::new(true);

    let css = r#"
      body :host(#host) .target { color: rgb(10, 20, 30); }
    "#;

    let style_node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "style".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Text {
          content: css.to_string(),
        },
        children: vec![],
      }],
    };

    let shadow_root = DomNode {
      node_type: DomNodeType::ShadowRoot {
        mode: crate::dom::ShadowRootMode::Open,
        delegates_focus: false,
      },
      children: vec![
        style_node,
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "span".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![
              ("id".to_string(), "target".to_string()),
              ("class".to_string(), "target".to_string()),
            ],
          },
          children: vec![],
        },
      ],
    };

    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "body".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("id".to_string(), "host".to_string())],
        },
        children: vec![shadow_root],
      }],
    };

    let stylesheet = StyleSheet::new();
    let styled_without_scoping = {
      let _guard = ShadowBloomScopingGuard::new(false);
      apply_styles(&dom, &stylesheet)
    };
    let styled_with_scoping = {
      let _guard = ShadowBloomScopingGuard::new(true);
      apply_styles(&dom, &stylesheet)
    };

    assert_styled_trees_equal(&styled_with_scoping, &styled_without_scoping);

    let target = find_styled_node_by_id(&styled_with_scoping, "target").expect("target node");
    assert_eq!(target.styles.color, Rgba::rgb(10, 20, 30));
  }

  #[test]
  fn ancestor_bloom_shadow_scoping_increases_fast_rejects() {
    let _lock = cascade_global_test_lock();
    let _ancestor_bloom = AncestorBloomEnabledGuard::new(true);

    let prev_profile_enabled = cascade_profile_enabled();
    set_cascade_profile_enabled(true);

    let dom = nested_shadow_dom_fixture(8, 4, 20, 12);
    let stylesheet = StyleSheet::new();

    let without_scoping = {
      let _guard = ShadowBloomScopingGuard::new(false);
      reset_cascade_profile();
      let _ = apply_styles(&dom, &stylesheet);
      capture_cascade_profile().selector_bloom_fast_rejects
    };

    let with_scoping = {
      let _guard = ShadowBloomScopingGuard::new(true);
      reset_cascade_profile();
      let _ = apply_styles(&dom, &stylesheet);
      capture_cascade_profile().selector_bloom_fast_rejects
    };

    assert!(
      with_scoping > without_scoping,
      "expected more bloom fast rejects with shadow scoping enabled (with={with_scoping}, without={without_scoping})"
    );

    set_cascade_profile_enabled(prev_profile_enabled);
  }

  fn child_font_weight(parent_style: &str, child_style: &str) -> u16 {
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), parent_style.to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("style".to_string(), child_style.to_string())],
        },
        children: vec![],
      }],
    };
    let styled = apply_styles(&parent, &StyleSheet::new());
    styled
      .children
      .first()
      .expect("child")
      .styles
      .font_weight
      .to_u16()
  }

  fn element_with_id_and_class(id: &str, class: &str, style: Option<&str>) -> DomNode {
    let mut attributes = vec![
      ("id".to_string(), id.to_string()),
      ("class".to_string(), class.to_string()),
    ];
    if let Some(style) = style {
      attributes.push(("style".to_string(), style.to_string()));
    }
    DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes,
      },
      children: vec![],
    }
  }

  #[test]
  fn before_without_content_is_not_generated() {
    let dom = element_with_style("");
    let stylesheet = parse_stylesheet("div::before { color: red; }").expect("stylesheet");
    let styled = apply_styles(&dom, &stylesheet);
    assert!(styled.before_styles.is_none());
  }

  #[test]
  fn before_content_enables_other_rules() {
    let dom = element_with_style("");
    let stylesheet =
      parse_stylesheet("div::before { content: 'x'; }\n div::before { color: rgb(1, 2, 3); }")
        .expect("stylesheet");
    let styled = apply_styles(&dom, &stylesheet);
    let before = styled.before_styles.expect("generated ::before");
    assert_eq!(before.color, Rgba::rgb(1, 2, 3));
    assert_eq!(before.content_value, ContentValue::from_string("x"));
  }

  #[test]
  fn single_colon_before_behaves_like_pseudo_element() {
    let dom = element_with_style("");
    let stylesheet =
      parse_stylesheet("div:before { content: 'x'; color: rgb(1, 2, 3); }").expect("stylesheet");

    let styled = apply_styles(&dom, &stylesheet);
    let before = styled
      .before_styles
      .as_ref()
      .expect("generated :before pseudo-element");
    assert_eq!(before.color, Rgba::rgb(1, 2, 3));
    assert_eq!(before.content_value, ContentValue::from_string("x"));
  }

  #[test]
  fn single_colon_after_behaves_like_pseudo_element() {
    let dom = element_with_style("");
    let stylesheet =
      parse_stylesheet("div:after { content: 'y'; color: rgb(4, 5, 6); }").expect("stylesheet");

    let styled = apply_styles(&dom, &stylesheet);
    let after = styled
      .after_styles
      .as_ref()
      .expect("generated :after pseudo-element");
    assert_eq!(after.color, Rgba::rgb(4, 5, 6));
    assert_eq!(after.content_value, ContentValue::from_string("y"));
  }

  #[test]
  fn important_overrides_more_specific_normal_declarations() {
    let dom = element_with_id_and_class("target", "item", None);
    let stylesheet = parse_stylesheet(
      r"
            #target { color: red; }
            .item { color: blue !important; }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.color, Rgba::rgb(0, 0, 255));
  }

  #[test]
  fn author_important_beats_inline_normal() {
    let dom = element_with_id_and_class("target", "item", Some("color: green;"));
    let stylesheet = parse_stylesheet(".item { color: red !important; }").unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.color, Rgba::rgb(255, 0, 0));
  }

  #[test]
  fn inline_important_outranks_author_important_through_specificity() {
    let dom = element_with_id_and_class("target", "item", Some("color: green !important;"));
    let stylesheet = parse_stylesheet(".item { color: blue !important; }").unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.color, Rgba::rgb(0, 128, 0));
  }

  #[test]
  fn type_and_class_selector_applies_properties() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "span".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "pagetop".to_string())],
      },
      children: vec![],
    };
    let stylesheet = parse_stylesheet(
      r"
            .pagetop { line-height: 12px; }
            span.pagetop { display: block; margin: 3px 5px; }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    assert!(matches!(styled.styles.display, Display::Block));
    assert_eq!(styled.styles.margin_top, Some(Length::px(3.0)));
    assert_eq!(styled.styles.margin_right, Some(Length::px(5.0)));
    assert_eq!(styled.styles.margin_bottom, Some(Length::px(3.0)));
    assert_eq!(styled.styles.margin_left, Some(Length::px(5.0)));
  }

  #[test]
  fn revert_falls_back_to_ua_for_non_inherited_property() {
    let dom = element_with_id_and_class("target", "box", None);
    let stylesheet = parse_stylesheet(
      r"
            .box { display: inline; }
            #target { display: revert; }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    assert!(matches!(styled.styles.display, Display::Block));
  }

  #[test]
  fn revert_ignores_author_inherited_value() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "color: rgb(1, 2, 3);".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("id".to_string(), "target".to_string())],
        },
        children: vec![],
      }],
    };
    let stylesheet = parse_stylesheet("#target { color: revert; }").unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let child = styled.children.first().expect("child");
    assert_eq!(child.styles.color, ComputedStyle::default().color);
    assert_ne!(child.styles.color, styled.styles.color);
  }

  #[test]
  fn registered_custom_property_with_inherits_false_uses_initial_value() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("id".to_string(), "parent".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("id".to_string(), "child".to_string())],
        },
        children: vec![],
      }],
    };
    let stylesheet = parse_stylesheet(
      r#"
        @property --accent {
          syntax: "<color>";
          inherits: false;
          initial-value: red;
        }
        #parent { --accent: blue; }
        #child { color: var(--accent); }
      "#,
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let child = styled.children.first().expect("child");
    assert_eq!(child.styles.color, Rgba::rgb(255, 0, 0));
    if let Some(value) = child.styles.custom_properties.get("--accent") {
      assert_eq!(value.value, "red");
      assert!(matches!(
        value.typed,
        Some(CustomPropertyTypedValue::Color(_))
      ));
    } else {
      panic!("missing registered custom property on child");
    }
  }

  #[test]
  fn invalid_registered_custom_property_value_reverts_to_initial() {
    let dom = element_with_id_and_class("target", "", None);
    let stylesheet = parse_stylesheet(
      r#"
        @property --len {
          syntax: "<length>";
          initial-value: 5px;
        }
        #target { --len: red; width: var(--len); }
      "#,
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.width, Some(Length::px(5.0)));
    assert_eq!(
      styled
        .styles
        .custom_properties
        .get("--len")
        .map(|v| v.value.as_str()),
      Some("5px")
    );
  }

  #[test]
  fn typed_custom_property_accepts_var_in_specified_value() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("id".to_string(), "t".to_string())],
        },
        children: vec![],
      }],
    };
    let stylesheet = parse_stylesheet(
      r#"
        @property --len {
          syntax: "<length>";
          inherits: true;
          initial-value: 5px;
        }
        :root { --base: 10px; --len: var(--base); }
        #t { width: var(--len); }
      "#,
    )
    .unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    let target = styled.children.first().expect("target");
    assert_eq!(target.styles.width, Some(Length::px(10.0)));
  }

  #[test]
  fn typed_custom_property_var_fallback_applies() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("id".to_string(), "t".to_string())],
        },
        children: vec![],
      }],
    };
    let stylesheet = parse_stylesheet(
      r#"
        @property --len {
          syntax: "<length>";
          inherits: true;
          initial-value: 5px;
        }
        :root { --len: var(--missing, 12px); }
        #t { width: var(--len); }
      "#,
    )
    .unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    let target = styled.children.first().expect("target");
    assert_eq!(target.styles.width, Some(Length::px(12.0)));
  }

  #[test]
  fn typed_custom_property_invalid_after_substitution_falls_back_to_initial() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("id".to_string(), "t".to_string())],
        },
        children: vec![],
      }],
    };
    let stylesheet = parse_stylesheet(
      r#"
        @property --len {
          syntax: "<length>";
          inherits: true;
          initial-value: 5px;
        }
        :root { --base: red; --len: var(--base); }
        #t { width: var(--len); }
      "#,
    )
    .unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    let target = styled.children.first().expect("target");
    assert_eq!(target.styles.width, Some(Length::px(5.0)));
  }

  #[test]
  fn typed_custom_property_with_inherits_false_does_not_inherit_var_resolved_value() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("id".to_string(), "parent".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("id".to_string(), "child".to_string())],
        },
        children: vec![],
      }],
    };
    let stylesheet = parse_stylesheet(
      r#"
        @property --len {
          syntax: "<length>";
          inherits: false;
          initial-value: 5px;
        }
        #parent { --base: 10px; --len: var(--base); }
        #child { width: var(--len); }
      "#,
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let child = styled.children.first().expect("child");
    assert_eq!(child.styles.width, Some(Length::px(5.0)));
  }

  #[test]
  fn text_combine_upright_inherits() {
    let mut parent = ComputedStyle::default();
    parent.text_combine_upright = TextCombineUpright::Digits(3);
    let mut child = ComputedStyle::default();
    inherit_styles(&mut child, &parent);
    assert_eq!(parent.text_combine_upright, TextCombineUpright::Digits(3));
    assert_eq!(child.text_combine_upright, TextCombineUpright::Digits(3));
  }

  #[test]
  fn layer_order_controls_cascade_priority() {
    let dom = element_with_id_and_class("target", "", None);
    let stylesheet = parse_stylesheet(
      r"
            @layer base { #target { color: rgb(1, 2, 3); } }
            @layer theme { #target { color: rgb(4, 5, 6); } }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.color, Rgba::rgb(4, 5, 6));
  }

  #[test]
  fn blockless_layer_prelude_defines_order() {
    let dom = element_with_id_and_class("target", "", None);
    let stylesheet = parse_stylesheet(
      r"
            @layer base, theme;
            @layer base { #target { color: rgb(1, 2, 3); } }
            @layer theme { #target { color: rgb(9, 8, 7); } }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.color, Rgba::rgb(9, 8, 7));
  }

  #[test]
  fn unlayered_rules_override_layered() {
    let dom = element_with_id_and_class("target", "", None);
    let stylesheet = parse_stylesheet(
      r"
            @layer base { #target { color: rgb(1, 1, 1); } }
            #target { color: rgb(2, 3, 4); }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.color, Rgba::rgb(2, 3, 4));
  }

  #[test]
  fn revert_layer_restores_previous_layer_value() {
    let dom = element_with_id_and_class("target", "", None);
    let stylesheet = parse_stylesheet(
      r"
            @layer base { #target { color: rgb(10, 20, 30); } }
            @layer theme { #target { color: rgb(200, 100, 50); color: revert-layer; } }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.color, Rgba::rgb(10, 20, 30));
  }

  #[test]
  fn dotted_layer_names_create_nested_paths() {
    let dom = element_with_id_and_class("target", "", None);
    let stylesheet = parse_stylesheet(
      r"
            @layer ui.controls { #target { color: rgb(1, 2, 3); } }
            @layer ui { #target { color: rgb(9, 9, 9); } }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    // The child layer ui.controls should override its parent ui declarations.
    assert_eq!(styled.styles.color, Rgba::rgb(1, 2, 3));
  }

  #[test]
  fn nested_layer_sibling_order_respected() {
    let dom = element_with_id_and_class("target", "", None);
    let stylesheet = parse_stylesheet(
      r"
            @layer outer {
              #target { color: rgb(1, 2, 3); }
              @layer inner1 { #target { color: rgb(4, 5, 6); } }
              @layer inner2 { #target { color: rgb(7, 8, 9); } }
            }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.color, Rgba::rgb(7, 8, 9));
  }

  #[test]
  fn revert_layer_uses_nearest_layer_base() {
    let dom = element_with_id_and_class("target", "", None);
    let stylesheet = parse_stylesheet(
      r"
            @layer theme {
              #target { color: rgb(1, 2, 3); }
              @layer accents {
                #target { color: rgb(10, 20, 30); color: revert-layer; }
              }
            }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.color, Rgba::rgb(1, 2, 3));
  }

  #[test]
  fn all_initial_resets_properties_but_preserves_direction() {
    let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                namespace: HTML_NAMESPACE.to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "direction: rtl; unicode-bidi: bidi-override; color: red; background-color: blue; all: initial;"
                        .to_string(),
                )],
            },
            children: vec![],
        };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let defaults = ComputedStyle::default();
    assert_eq!(styled.styles.color, defaults.color);
    assert_eq!(styled.styles.background_color, defaults.background_color);
    assert!(matches!(
      styled.styles.direction,
      crate::style::types::Direction::Rtl
    ));
    assert!(matches!(
      styled.styles.unicode_bidi,
      crate::style::types::UnicodeBidi::BidiOverride
    ));
  }

  #[test]
  fn all_inherit_copies_non_inherited_properties() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "display: inline;".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![
            ("id".to_string(), "target".to_string()),
            ("style".to_string(), "all: inherit;".to_string()),
          ],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let child = styled.children.first().expect("child");
    assert!(matches!(child.styles.display, Display::Inline));
  }

  #[test]
  fn all_revert_ignores_author_inheritance() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "color: rgb(10, 20, 30);".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![(
            "style".to_string(),
            "color: green; all: revert;".to_string(),
          )],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let child = styled.children.first().expect("child");
    assert_eq!(child.styles.color, ComputedStyle::default().color);
  }

  #[test]
  fn text_nodes_inherit_styles_without_matching() {
    let dom = DomNode {
      node_type: DomNodeType::Document {
        quirks_mode: QuirksMode::NoQuirks,
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![(
            "style".to_string(),
            "color: rgb(10, 20, 30); text-decoration: underline;".to_string(),
          )],
        },
        children: vec![DomNode {
          node_type: DomNodeType::Text {
            content: "hello".to_string(),
          },
          children: vec![],
        }],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let div = styled.children.first().expect("div");
    let text = div.children.first().expect("text node");
    assert_eq!(text.styles.color, Rgba::from_rgba8(10, 20, 30, 255));
    assert_eq!(text.styles.applied_text_decorations.len(), 1);
  }

  #[test]
  fn navigation_visibility_respects_author_stylesheet() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "nav".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };
    let stylesheet = parse_stylesheet("nav { visibility: hidden; }").unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let nav = styled.children.first().expect("nav");
    assert!(matches!(nav.styles.visibility, Visibility::Hidden));
  }

  #[test]
  fn navigation_visibility_respects_authored_hidden_value() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "nav".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![
            ("style".to_string(), "visibility: hidden;".to_string()),
            ("role".to_string(), "navigation".to_string()),
          ],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let nav = styled.children.first().expect("nav");
    assert!(matches!(nav.styles.visibility, Visibility::Hidden));
  }

  #[test]
  fn opacity_accepts_unitless_zero() {
    let dom = element_with_style("opacity: 0;");

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.opacity, 0.0);
  }

  #[test]
  fn z_index_accepts_unitless_zero() {
    let dom = element_with_style("position: relative; z-index: 0;");

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.z_index, Some(0));
  }

  #[test]
  fn opacity_accepts_calc_number() {
    let dom = element_with_style("opacity: calc(1 - 0.25);");

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!((styled.styles.opacity - 0.75).abs() < 1e-6);
  }

  #[test]
  fn z_index_accepts_calc_number() {
    let dom = element_with_style("position: relative; z-index: calc(2 + 3);");

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.z_index, Some(5));
  }

  #[test]
  fn order_accepts_calc_integer() {
    let dom = element_with_style("display: flex; order: calc(2 + 3);");

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.order, 5);
  }

  #[test]
  fn order_rejects_non_integer_calc() {
    let dom = element_with_style("display: flex; order: calc(2.5);");

    let styled = apply_styles(&dom, &StyleSheet::new());
    // order should remain the initial 0 when calc produces a non-integer
    assert_eq!(styled.styles.order, 0);
  }

  #[test]
  fn ad_slot_classes_do_not_collapse_without_css_rules() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![(
            "class".to_string(),
            "ad-slot ad-slot-header ad-slot-header__wrapper".to_string(),
          )],
        },
        children: vec![],
      }],
    };
    let stylesheet = parse_stylesheet(
            "div.ad-slot { min-height: 24px; padding-top: 8px; margin-top: 4px; background: rgb(1, 2, 3); }",
        )
        .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let ad = styled.children.first().expect("ad slot");
    assert!(matches!(ad.styles.display, Display::Block));
    assert!(matches!(ad.styles.visibility, Visibility::Visible));
    assert_eq!(ad.styles.min_height, Some(Length::px(24.0)));
    assert_eq!(ad.styles.padding_top, Length::px(8.0));
    assert_eq!(ad.styles.margin_top, Some(Length::px(4.0)));
    assert_eq!(ad.styles.background_color, Rgba::rgb(1, 2, 3));
  }

  #[test]
  fn font_variation_settings_inherit() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          r#"font-variation-settings: "wght" 600;"#.to_string(),
        )],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let child = styled.children.first().expect("child");
    assert_eq!(child.styles.font_variation_settings.len(), 1);
    assert_eq!(child.styles.font_variation_settings[0].tag, *b"wght");
    assert!((child.styles.font_variation_settings[0].value - 600.0).abs() < 0.001);
  }

  #[test]
  fn font_language_override_inherits() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          r#"font-language-override: "SRB";"#.to_string(),
        )],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let child = styled.children.first().expect("child");
    assert!(matches!(
        child.styles.font_language_override,
        crate::style::types::FontLanguageOverride::Override(ref tag) if tag == "SRB"
    ));
  }

  #[test]
  fn imports_are_resolved_when_loader_present() {
    struct Loader;
    impl CssImportLoader for Loader {
      fn load(&self, _url: &str) -> crate::error::Result<String> {
        Ok("#target { color: rgb(1, 2, 3); }".to_string())
      }
    }

    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("id".to_string(), "target".to_string())],
      },
      children: vec![],
    };
    let stylesheet = parse_stylesheet(r#"@import url("import.css");"#).unwrap();
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let styled = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      Some(&Loader),
      Some("https://example.com/page.css"),
      None,
      None,
      None,
    );
    assert_eq!(styled.styles.color, Rgba::rgb(1, 2, 3));
  }

  #[test]
  fn color_scheme_dark_applies_default_palette() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "body".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };
    let stylesheet = parse_stylesheet("html { color-scheme: light dark; }").unwrap();
    let media = MediaContext::screen(800.0, 600.0).with_color_scheme(ColorScheme::Dark);
    let styled = apply_styles_with_media(&dom, &stylesheet, &media);
    assert_eq!(styled.styles.color, Rgba::rgb(232, 232, 232));
    assert_eq!(styled.styles.background_color, Rgba::rgb(16, 16, 16));
    let body = styled.children.first().expect("body");
    assert_eq!(body.styles.color, styled.styles.color);
    assert_eq!(body.styles.background_color, Rgba::TRANSPARENT);
  }

  #[test]
  fn color_scheme_prefers_light_when_requested() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let stylesheet = parse_stylesheet("html { color-scheme: light dark; }").unwrap();
    let media = MediaContext::screen(800.0, 600.0).with_color_scheme(ColorScheme::Light);
    let styled = apply_styles_with_media(&dom, &stylesheet, &media);
    assert_eq!(styled.styles.color, Rgba::BLACK);
    assert_eq!(styled.styles.background_color, Rgba::TRANSPARENT);
  }

  #[test]
  fn color_scheme_does_not_override_authored_colors() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "body".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };
    let stylesheet = parse_stylesheet(
            "html { color-scheme: light dark; } body { color: rgb(10, 20, 30); background: rgb(1, 2, 3); }",
        )
        .unwrap();
    let media = MediaContext::screen(800.0, 600.0).with_color_scheme(ColorScheme::Dark);
    let styled = apply_styles_with_media(&dom, &stylesheet, &media);
    let body = styled.children.first().expect("body");
    assert_eq!(styled.styles.background_color, Rgba::rgb(16, 16, 16));
    assert_eq!(body.styles.color, Rgba::rgb(10, 20, 30));
    assert_eq!(body.styles.background_color, Rgba::rgb(1, 2, 3));
  }

  #[test]
  fn color_scheme_inherits_to_descendants() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "body".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![DomNode {
          node_type: DomNodeType::Element {
            tag_name: "div".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        }],
      }],
    };
    let stylesheet = parse_stylesheet("html { color-scheme: light dark; }").unwrap();
    let media = MediaContext::screen(800.0, 600.0).with_color_scheme(ColorScheme::Dark);
    let styled = apply_styles_with_media(&dom, &stylesheet, &media);
    let expected = ColorSchemePreference::Supported {
      schemes: vec![ColorSchemeEntry::Light, ColorSchemeEntry::Dark],
      only: false,
    };
    assert_eq!(styled.styles.color_scheme, expected);
    let body = styled.children.first().expect("body");
    assert_eq!(body.styles.color_scheme, expected);
    let div = body.children.first().expect("div");
    assert_eq!(div.styles.color_scheme, expected);
  }

  #[test]
  fn color_scheme_dark_recolors_form_controls() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "body".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![
          DomNode {
            node_type: DomNodeType::Element {
              tag_name: "input".to_string(),
              namespace: HTML_NAMESPACE.to_string(),
              attributes: vec![("type".to_string(), "text".to_string())],
            },
            children: vec![],
          },
          DomNode {
            node_type: DomNodeType::Element {
              tag_name: "textarea".to_string(),
              namespace: HTML_NAMESPACE.to_string(),
              attributes: vec![],
            },
            children: vec![],
          },
        ],
      }],
    };

    let stylesheet =
      parse_stylesheet("html { color-scheme: light dark; } body { margin: 0; }").unwrap();
    let media = MediaContext::screen(800.0, 600.0).with_color_scheme(ColorScheme::Dark);
    let styled = apply_styles_with_media(&dom, &stylesheet, &media);
    let body = styled.children.first().expect("body");
    let input = body.children.first().expect("input");
    let textarea = body.children.get(1).expect("textarea");

    assert_eq!(styled.styles.background_color, Rgba::rgb(16, 16, 16));
    let expected_surface = Rgba::rgb(24, 24, 24);
    let expected_border = Rgba::rgb(96, 96, 96);
    assert_eq!(input.styles.background_color, expected_surface);
    assert_eq!(textarea.styles.background_color, expected_surface);
    assert_eq!(input.styles.border_top_color, expected_border);
    assert_eq!(input.styles.border_left_color, expected_border);
    assert_eq!(textarea.styles.border_top_color, expected_border);
    assert_eq!(textarea.styles.border_left_color, expected_border);
  }

  #[test]
  fn color_scheme_dark_respects_authored_control_colors() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "body".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![DomNode {
          node_type: DomNodeType::Element {
            tag_name: "input".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("type".to_string(), "text".to_string())],
          },
          children: vec![],
        }],
      }],
    };

    let stylesheet = parse_stylesheet(
            "html { color-scheme: light dark; } input { background: yellow; border-color: rgb(204, 204, 204); }",
        )
        .unwrap();
    let media = MediaContext::screen(800.0, 600.0).with_color_scheme(ColorScheme::Dark);
    let styled = apply_styles_with_media(&dom, &stylesheet, &media);
    let body = styled.children.first().expect("body");
    let input = body.children.first().expect("input");

    assert_eq!(input.styles.background_color, Rgba::new(255, 255, 0, 1.0));
    assert_eq!(input.styles.border_top_color, Rgba::rgb(204, 204, 204));
    assert_eq!(input.styles.border_left_color, Rgba::rgb(204, 204, 204));
  }

  #[test]
  fn color_scheme_dark_recolors_focus_outline() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "body".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![DomNode {
          node_type: DomNodeType::Element {
            tag_name: "input".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![
              ("type".to_string(), "text".to_string()),
              ("data-fastr-focus".to_string(), "true".to_string()),
            ],
          },
          children: vec![],
        }],
      }],
    };

    let stylesheet =
      parse_stylesheet("html { color-scheme: light dark; } body { margin: 0; }").unwrap();
    let media = MediaContext::screen(800.0, 600.0).with_color_scheme(ColorScheme::Dark);
    let styled = apply_styles_with_media(&dom, &stylesheet, &media);
    let body = styled.children.first().expect("body");
    let input = body.children.first().expect("input");

    assert_eq!(
      input.styles.outline_color,
      OutlineColor::Color(Rgba::rgb(232, 232, 232))
    );
  }

  #[test]
  fn color_scheme_dark_respects_authored_outline() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "body".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![DomNode {
          node_type: DomNodeType::Element {
            tag_name: "input".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![
              ("type".to_string(), "text".to_string()),
              ("data-fastr-focus".to_string(), "true".to_string()),
              (
                "style".to_string(),
                "outline: 1px solid rgb(10, 20, 30);".to_string(),
              ),
            ],
          },
          children: vec![],
        }],
      }],
    };

    let stylesheet =
      parse_stylesheet("html { color-scheme: light dark; } body { margin: 0; }").unwrap();
    let media = MediaContext::screen(800.0, 600.0).with_color_scheme(ColorScheme::Dark);
    let styled = apply_styles_with_media(&dom, &stylesheet, &media);
    let body = styled.children.first().expect("body");
    let input = body.children.first().expect("input");

    assert_eq!(
      input.styles.outline_color,
      OutlineColor::Color(Rgba::rgb(10, 20, 30))
    );
  }

  #[test]
  fn dir_attribute_sets_direction_and_isolate() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("dir".to_string(), "rtl".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.direction,
      crate::style::types::Direction::Rtl
    ));
    assert!(matches!(
      styled.styles.unicode_bidi,
      crate::style::types::UnicodeBidi::Isolate
    ));
  }

  #[test]
  fn author_css_overrides_presentational_dir() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("dir".to_string(), "rtl".to_string())],
      },
      children: vec![],
    };

    let stylesheet = parse_stylesheet("div { direction: ltr; unicode-bidi: normal; }").unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    assert!(matches!(
      styled.styles.direction,
      crate::style::types::Direction::Ltr
    ));
    assert!(matches!(
      styled.styles.unicode_bidi,
      crate::style::types::UnicodeBidi::Normal
    ));
  }

  #[test]
  fn image_set_selection_uses_media_context_dpr() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let stylesheet = parse_stylesheet(
      "div { background-image: image-set(url(\"lo.png\") 1x, url(\"hi.png\") 2x); }",
    )
    .unwrap();
    let media = MediaContext::screen(800.0, 600.0).with_device_pixel_ratio(2.0);
    let styled = apply_styles_with_media(&dom, &stylesheet, &media);

    assert!(matches!(
        styled.styles.background_layers.first().and_then(|l| l.image.as_ref()),
        Some(crate::style::types::BackgroundImage::Url(url)) if url == "hi.png"
    ));
  }

  #[test]
  fn inline_style_overrides_presentational_dir() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("dir".to_string(), "rtl".to_string()),
          ("style".to_string(), "direction: ltr;".to_string()),
        ],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.direction,
      crate::style::types::Direction::Ltr
    ));
  }

  #[test]
  fn replaced_dimensions_use_presentational_hints() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "img".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("width".to_string(), "320".to_string()),
          ("height".to_string(), "240".to_string()),
        ],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.width, Some(Length::px(320.0)));
    assert_eq!(styled.styles.height, Some(Length::px(240.0)));
  }

  #[test]
  fn author_css_overrides_replaced_presentational_dimensions() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "img".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("width".to_string(), "640".to_string()),
          ("height".to_string(), "480".to_string()),
        ],
      },
      children: vec![],
    };

    let stylesheet = parse_stylesheet("img { width: 120px; height: auto; }").unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.width, Some(Length::px(120.0)));
    assert!(styled.styles.height.is_none());
  }

  #[test]
  fn html_defaults_to_transparent_background() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.background_color, Rgba::TRANSPARENT);
  }

  #[test]
  fn body_defaults_to_ua_margin() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "body".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let expected = Some(Length::px(8.0));
    assert_eq!(styled.styles.margin_top, expected);
    assert_eq!(styled.styles.margin_right, expected);
    assert_eq!(styled.styles.margin_bottom, expected);
    assert_eq!(styled.styles.margin_left, expected);
  }

  #[test]
  fn anchor_defaults_to_ua_link_style() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "https://example.com".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.color, Rgba::from_rgba8(0, 0, 238, 255));
    assert!(styled
      .styles
      .text_decoration
      .lines
      .contains(TextDecorationLine::UNDERLINE));
    assert_eq!(
      styled.styles.cursor,
      crate::style::types::CursorKeyword::Pointer
    );
  }

  #[test]
  fn author_css_overrides_anchor_ua_style() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "https://example.com".to_string())],
      },
      children: vec![],
    };

    let stylesheet =
      parse_stylesheet("a { color: rgb(255, 0, 0); text-decoration: none; cursor: default; }")
        .unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.color, Rgba::rgb(255, 0, 0));
    assert!(styled.styles.text_decoration.lines.is_empty());
    assert!(styled.styles.applied_text_decorations.is_empty());
    assert_eq!(
      styled.styles.cursor,
      crate::style::types::CursorKeyword::Default
    );
  }

  #[test]
  fn heading_ua_defaults_apply() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "h1".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!((styled.styles.font_size - 32.0).abs() < 0.01);
    assert_eq!(styled.styles.font_weight.to_u16(), 700);
    assert_eq!(styled.styles.margin_top, Some(Length::em(0.67)));
    assert_eq!(styled.styles.margin_bottom, Some(Length::em(0.67)));
  }

  #[test]
  fn paragraph_ua_defaults_apply() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "p".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.margin_top, Some(Length::em(1.0)));
    assert_eq!(styled.styles.margin_bottom, Some(Length::em(1.0)));
  }

  #[test]
  fn preformatted_ua_defaults_apply() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "pre".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(
      styled.styles.white_space,
      crate::style::types::WhiteSpace::Pre
    );
    assert_eq!(styled.styles.font_family[0], "monospace");
    assert_eq!(styled.styles.margin_top, Some(Length::em(1.0)));
    assert_eq!(styled.styles.margin_bottom, Some(Length::em(1.0)));
  }

  #[test]
  fn blockquote_ua_defaults_apply() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "blockquote".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.margin_top, Some(Length::em(1.0)));
    assert_eq!(styled.styles.margin_bottom, Some(Length::em(1.0)));
    assert_eq!(styled.styles.margin_left, Some(Length::px(40.0)));
    assert_eq!(styled.styles.margin_right, Some(Length::px(40.0)));
  }

  #[test]
  fn list_defaults_apply() {
    let ul = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let ul_styled = apply_styles(&ul, &StyleSheet::new());
    assert_eq!(
      ul_styled.styles.list_style_type,
      crate::style::types::ListStyleType::Disc
    );
    assert_eq!(ul_styled.styles.padding_left, Length::px(40.0));
    assert_eq!(ul_styled.styles.margin_top, Some(Length::em(1.0)));
    assert_eq!(ul_styled.styles.margin_bottom, Some(Length::em(1.0)));

    let li = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "li".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let li_styled = apply_styles(&li, &StyleSheet::new());
    assert_eq!(li_styled.styles.display, Display::ListItem);

    let ol = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ol".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let ol_styled = apply_styles(&ol, &StyleSheet::new());
    assert_eq!(
      ol_styled.styles.list_style_type,
      crate::style::types::ListStyleType::Decimal
    );
    assert_eq!(ol_styled.styles.padding_left, Length::px(40.0));

    for tag in ["menu", "dir"] {
      let dom = DomNode {
        node_type: DomNodeType::Element {
          tag_name: tag.to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      };
      let styled = apply_styles(&dom, &StyleSheet::new());
      assert_eq!(styled.styles.display, Display::Block);
      assert_eq!(
        styled.styles.list_style_type,
        crate::style::types::ListStyleType::Disc
      );
      assert_eq!(styled.styles.padding_left, Length::px(40.0));
    }
  }

  #[test]
  fn definition_list_defaults_apply() {
    let dd = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "dd".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dd, &StyleSheet::new());
    assert_eq!(styled.styles.margin_left, Some(Length::px(40.0)));
  }

  #[test]
  fn horizontal_rule_defaults_apply() {
    let hr = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "hr".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&hr, &StyleSheet::new());
    assert_eq!(styled.styles.height, Some(Length::px(1.0)));
    assert_eq!(
      styled.styles.box_sizing,
      crate::style::types::BoxSizing::ContentBox
    );
    assert_eq!(styled.styles.border_top_width, Length::px(1.0));
    assert_eq!(
      styled.styles.border_top_style,
      crate::style::types::BorderStyle::Inset
    );
    let gray = Rgba::from_rgba8(128, 128, 128, 255);
    assert_eq!(styled.styles.color, gray);
    assert_eq!(styled.styles.border_top_color, gray);
  }

  #[test]
  fn bdi_defaults_to_isolate_with_auto_direction() {
    let bdi = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "bdi".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Text {
          content: "مرحبا".to_string(),
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&bdi, &StyleSheet::new());
    assert_eq!(styled.styles.unicode_bidi, UnicodeBidi::Isolate);
    assert_eq!(styled.styles.direction, Direction::Rtl);
  }

  #[test]
  fn bdo_dir_attribute_uses_bidi_override() {
    let bdo = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "bdo".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("dir".to_string(), "rtl".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&bdo, &StyleSheet::new());
    assert_eq!(styled.styles.direction, Direction::Rtl);
    assert_eq!(styled.styles.unicode_bidi, UnicodeBidi::BidiOverride);
  }

  #[test]
  fn bdo_without_dir_defaults_to_override_ltr() {
    let bdo = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "bdo".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&bdo, &StyleSheet::new());
    assert_eq!(styled.styles.direction, Direction::Ltr);
    assert_eq!(styled.styles.unicode_bidi, UnicodeBidi::BidiOverride);
  }

  #[test]
  fn dir_auto_inherits_parent_direction_when_no_strong_characters() {
    let child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("dir".to_string(), "auto".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Text {
          content: "1234".to_string(), // neutrals only
        },
        children: vec![],
      }],
    };
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("dir".to_string(), "rtl".to_string())],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child_styles = &styled.children[0].styles;
    assert_eq!(child_styles.direction, Direction::Rtl);
    assert_eq!(child_styles.unicode_bidi, UnicodeBidi::Isolate);
  }

  #[test]
  fn bdi_auto_direction_uses_parent_when_no_strong_characters() {
    let bdi = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "bdi".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Text {
          content: "--".to_string(), // no strong directional characters
        },
        children: vec![],
      }],
    };
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("dir".to_string(), "rtl".to_string())],
      },
      children: vec![bdi],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let bdi_styles = &styled.children[0].styles;
    assert_eq!(bdi_styles.direction, Direction::Rtl);
    assert_eq!(bdi_styles.unicode_bidi, UnicodeBidi::Isolate);
  }

  #[test]
  fn small_and_big_defaults_apply() {
    let small = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "small".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let big = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "big".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let small_styled = apply_styles(&small, &StyleSheet::new());
    let big_styled = apply_styles(&big, &StyleSheet::new());
    let parent = 16.0;
    assert!(
      small_styled.styles.font_size < parent,
      "small font size {} should be less than parent {}",
      small_styled.styles.font_size,
      parent
    );
    assert!(
      (small_styled.styles.font_size * 1.2 - parent).abs() < 0.1,
      "small font size {} not near 1/1.2 of parent {}",
      small_styled.styles.font_size,
      parent
    );
    assert!(
      big_styled.styles.font_size > parent,
      "big font size {} should be greater than parent {}",
      big_styled.styles.font_size,
      parent
    );
    assert!(
      (big_styled.styles.font_size / 1.2 - parent).abs() < 0.1,
      "big font size {} not near 1.2 of parent {}",
      big_styled.styles.font_size,
      parent
    );
  }

  #[test]
  fn sub_and_sup_defaults_apply() {
    let sub = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "sub".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let sup = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "sup".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let sub_styled = apply_styles(&sub, &StyleSheet::new());
    let sup_styled = apply_styles(&sup, &StyleSheet::new());
    assert_eq!(
      sub_styled.styles.vertical_align,
      crate::style::types::VerticalAlign::Sub
    );
    assert_eq!(
      sup_styled.styles.vertical_align,
      crate::style::types::VerticalAlign::Super
    );
    let expected = 16.0 / 1.2;
    assert!((sub_styled.styles.font_size - expected).abs() < 0.01);
    assert!((sup_styled.styles.font_size - expected).abs() < 0.01);
  }

  #[test]
  fn mark_defaults_apply() {
    let mark = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "mark".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&mark, &StyleSheet::new());
    assert_eq!(
      styled.styles.background_color,
      Rgba::from_rgba8(255, 255, 0, 255)
    );
    assert_eq!(styled.styles.color, Rgba::BLACK);
  }

  #[test]
  fn semantic_font_style_defaults_apply() {
    for tag in ["address", "cite", "dfn", "var", "em", "i"] {
      let dom = DomNode {
        node_type: DomNodeType::Element {
          tag_name: tag.to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      };
      let styled = apply_styles(&dom, &StyleSheet::new());
      assert_eq!(
        styled.styles.font_style,
        crate::style::types::FontStyle::Italic
      );
    }
  }

  #[test]
  fn ins_and_del_defaults_apply() {
    let ins = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ins".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let del = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "del".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let ins_styled = apply_styles(&ins, &StyleSheet::new());
    assert!(ins_styled
      .styles
      .text_decoration
      .lines
      .contains(TextDecorationLine::UNDERLINE));

    let del_styled = apply_styles(&del, &StyleSheet::new());
    assert!(del_styled
      .styles
      .text_decoration
      .lines
      .contains(TextDecorationLine::LINE_THROUGH));
  }

  #[test]
  fn abbr_with_title_defaults_apply() {
    let abbr = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "abbr".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("title".to_string(), "World Health Organization".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&abbr, &StyleSheet::new());
    assert_eq!(styled.styles.border_bottom_width, Length::px(1.0));
    assert_eq!(
      styled.styles.border_bottom_style,
      crate::style::types::BorderStyle::Dotted
    );
    assert_eq!(
      styled.styles.cursor,
      crate::style::types::CursorKeyword::Help
    );
  }

  #[test]
  fn underline_and_strike_defaults_apply() {
    let u = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "u".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let s = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "s".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let strike = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "strike".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let u_styled = apply_styles(&u, &StyleSheet::new());
    assert!(u_styled
      .styles
      .text_decoration
      .lines
      .contains(TextDecorationLine::UNDERLINE));

    for dom in [s, strike] {
      let styled = apply_styles(&dom, &StyleSheet::new());
      assert!(styled
        .styles
        .text_decoration
        .lines
        .contains(TextDecorationLine::LINE_THROUGH));
    }
  }

  #[test]
  fn center_defaults_apply() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "center".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(
      styled.styles.text_align,
      crate::style::types::TextAlign::Center
    );
  }

  #[test]
  fn form_control_defaults_apply() {
    let text_input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "text".to_string())],
      },
      children: vec![],
    };
    let submit_input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "submit".to_string())],
      },
      children: vec![],
    };
    let hidden_input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "hidden".to_string())],
      },
      children: vec![],
    };
    let textarea = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "textarea".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let select = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "select".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let button = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "button".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let option = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "option".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let optgroup = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "optgroup".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let fieldset = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "fieldset".to_string(),
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
      children: vec![],
    };
    let details = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "details".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let summary = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "summary".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let details_with_summary = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "details".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("open".to_string(), String::new())],
      },
      children: vec![summary.clone()],
    };
    let details_closed_with_content = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "details".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![
        summary.clone(),
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "div".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        },
      ],
    };
    let details_open_with_content = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "details".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("open".to_string(), String::new())],
      },
      children: details_closed_with_content.children.clone(),
    };

    for dom in [&text_input, &submit_input, &textarea, &select, &button] {
      let styled = apply_styles(dom, &StyleSheet::new());
      assert!(styled.styles.border_top_width.to_px() >= 1.0);
      assert_ne!(
        styled.styles.border_top_style,
        crate::style::types::BorderStyle::None
      );
      assert_eq!(styled.styles.background_color, Rgba::WHITE);
      assert_eq!(styled.styles.display, Display::InlineBlock);
    }

    let styled_textarea = apply_styles(&textarea, &StyleSheet::new());
    assert_eq!(styled_textarea.styles.display, Display::InlineBlock);
    assert_eq!(
      styled_textarea.styles.cursor,
      crate::style::types::CursorKeyword::Text
    );

    let styled_text_input = apply_styles(&text_input, &StyleSheet::new());
    assert_eq!(
      styled_text_input.styles.cursor,
      crate::style::types::CursorKeyword::Text
    );

    let styled_submit = apply_styles(&submit_input, &StyleSheet::new());
    assert_eq!(
      styled_submit.styles.cursor,
      crate::style::types::CursorKeyword::Pointer
    );

    let styled_hidden = apply_styles(&hidden_input, &StyleSheet::new());
    assert_eq!(styled_hidden.styles.display, Display::None);

    let styled_option = apply_styles(&option, &StyleSheet::new());
    assert_eq!(styled_option.styles.display, Display::Block);
    assert_eq!(
      styled_option.styles.white_space,
      crate::style::types::WhiteSpace::Pre
    );

    let styled_optgroup = apply_styles(&optgroup, &StyleSheet::new());
    assert_eq!(styled_optgroup.styles.display, Display::Block);
    assert_eq!(
      styled_optgroup.styles.white_space,
      crate::style::types::WhiteSpace::Pre
    );
    assert!(styled_optgroup.styles.font_weight.to_u16() >= 700);

    let iframe = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "iframe".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled_fieldset = apply_styles(&fieldset, &StyleSheet::new());
    assert_eq!(styled_fieldset.styles.display, Display::Block);
    assert!(styled_fieldset.styles.border_top_width.to_px() >= 1.5);
    assert_ne!(
      styled_fieldset.styles.border_top_style,
      crate::style::types::BorderStyle::None
    );
    assert!(
      styled_fieldset
        .styles
        .padding_left
        .resolve_with_font_size(styled_fieldset.styles.font_size)
        .unwrap_or(0.0)
        > 0.0
    );

    let styled_legend = apply_styles(&legend, &StyleSheet::new());
    assert_eq!(styled_legend.styles.display, Display::Block);
    assert!(styled_legend.styles.shrink_to_fit_inline_size);
    assert!(
      styled_legend
        .styles
        .padding_left
        .resolve_with_font_size(styled_legend.styles.font_size)
        .unwrap_or(0.0)
        > 0.0
    );
    assert!(
      styled_legend
        .styles
        .padding_right
        .resolve_with_font_size(styled_legend.styles.font_size)
        .unwrap_or(0.0)
        > 0.0
    );

    let styled_iframe = apply_styles(&iframe, &StyleSheet::new());
    assert_eq!(styled_iframe.styles.max_width, Some(Length::percent(100.0)));

    let styled_details = apply_styles(&details, &StyleSheet::new());
    assert_eq!(styled_details.styles.display, Display::Block);

    let styled_summary = apply_styles(&summary, &StyleSheet::new());
    assert_eq!(styled_summary.styles.display, Display::ListItem);
    assert_eq!(
      styled_summary.styles.cursor,
      crate::style::types::CursorKeyword::Pointer
    );
    assert_eq!(
      styled_summary.styles.list_style_type,
      crate::style::types::ListStyleType::DisclosureClosed
    );

    let styled_open_details = apply_styles(&details_with_summary, &StyleSheet::new());
    let styled_open_summary = &styled_open_details.children[0];
    assert_eq!(
      styled_open_summary.styles.list_style_type,
      crate::style::types::ListStyleType::DisclosureOpen
    );

    let styled_closed_details = apply_styles(&details_closed_with_content, &StyleSheet::new());
    assert_eq!(
      styled_closed_details.children[0].styles.display,
      Display::ListItem
    );
    assert_eq!(
      styled_closed_details.children[1].styles.display,
      Display::None
    );

    let styled_open_details_with_content =
      apply_styles(&details_open_with_content, &StyleSheet::new());
    assert_eq!(
      styled_open_details_with_content.children[1].styles.display,
      Display::Block
    );
  }

  #[test]
  fn disabled_form_controls_use_ua_styles() {
    let disabled_input = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "input".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("type".to_string(), "text".to_string()),
          ("disabled".to_string(), String::new()),
        ],
      },
      children: vec![],
    };

    let disabled_select = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "select".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("disabled".to_string(), String::new())],
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

    let styled_input = apply_styles(&disabled_input, &StyleSheet::new());
    assert_eq!(styled_input.styles.color, Rgba::rgb(128, 128, 128));
    assert_eq!(
      styled_input.styles.background_color,
      Rgba::rgb(240, 240, 240)
    );
    assert!(matches!(
      styled_input.styles.cursor,
      crate::style::types::CursorKeyword::Default
    ));

    let styled_select = apply_styles(&disabled_select, &StyleSheet::new());
    assert_eq!(styled_select.styles.color, Rgba::rgb(128, 128, 128));
    assert_eq!(
      styled_select.styles.background_color,
      Rgba::rgb(240, 240, 240)
    );
    assert!(matches!(
      styled_select.styles.cursor,
      crate::style::types::CursorKeyword::Default
    ));

    let styled_option = styled_select.children.first().expect("option");
    assert_eq!(styled_option.styles.color, Rgba::rgb(128, 128, 128));
  }

  #[test]
  fn table_header_defaults_apply() {
    let th = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "th".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&th, &StyleSheet::new());
    assert_eq!(styled.styles.font_weight.to_u16(), 700);
    assert_eq!(
      styled.styles.text_align,
      crate::style::types::TextAlign::Center
    );
  }

  #[test]
  fn q_defaults_apply_quotes() {
    let q = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "q".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&q, &StyleSheet::new());
    assert_eq!(
      styled.styles.quotes,
      vec![("“".into(), "”".into()), ("‘".into(), "’".into())].into()
    );
  }

  #[test]
  fn presentational_dimensions_apply_to_descendants() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "img".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![
            ("width".to_string(), "120".to_string()),
            ("height".to_string(), "60".to_string()),
          ],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let img = &styled.children[0];
    assert_eq!(img.styles.width, Some(Length::px(120.0)));
    assert_eq!(img.styles.height, Some(Length::px(60.0)));
  }

  #[test]
  fn bgcolor_presentational_hint_applies() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("bgcolor".to_string(), "#ff0000".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.background_color, Rgba::rgb(255, 0, 0));
  }

  #[test]
  fn author_css_overrides_bgcolor_presentational_hint() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("bgcolor".to_string(), "#ff0000".to_string())],
      },
      children: vec![],
    };

    let stylesheet = parse_stylesheet("div { background-color: rgb(0, 0, 255); }").unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.background_color, Rgba::rgb(0, 0, 255));
  }

  #[test]
  fn hidden_presentational_hint_sets_display_none() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("hidden".to_string(), String::new())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.display, Display::None);
  }

  #[test]
  fn author_css_overrides_hidden_presentational_hint() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("hidden".to_string(), String::new())],
      },
      children: vec![],
    };

    let stylesheet = parse_stylesheet("div[hidden] { display: block; }").unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.display, Display::Block);
  }

  #[test]
  fn aria_hidden_does_not_hide_element() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("aria-hidden".to_string(), "true".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_ne!(styled.styles.display, Display::None);
    assert_ne!(styled.styles.visibility, Visibility::Collapse);
    assert_ne!(styled.styles.visibility, Visibility::Hidden);
  }

  #[test]
  fn aria_label_has_no_visual_effect() {
    let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "img".to_string(),
                namespace: HTML_NAMESPACE.to_string(),
                attributes: vec![
                    ("src".to_string(), "data:image/svg+xml,%3Csvg%20xmlns%3D'http%3A//www.w3.org/2000/svg'%20width%3D'1'%20height%3D'1'%3E%3C/svg%3E".to_string()),
                    ("aria-label".to_string(), "decorative".to_string()),
                ],
            },
            children: vec![],
        };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_ne!(
      styled.styles.display,
      Display::None,
      "aria-label should not hide elements"
    );
  }

  #[test]
  fn bgcolor_presentational_hint_applies_to_descendants() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "td".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("bgcolor".to_string(), "#ff6600".to_string())],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let cell = &styled.children[0];
    assert_eq!(cell.styles.background_color, Rgba::rgb(255, 102, 0));
  }

  #[test]
  fn cellspacing_presentational_hint_sets_border_spacing() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("cellspacing".to_string(), "0".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.border_spacing_horizontal, Length::px(0.0));
    assert_eq!(styled.styles.border_spacing_vertical, Length::px(0.0));
  }

  #[test]
  fn table_ua_default_border_spacing_is_two_px() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.border_spacing_horizontal, Length::px(2.0));
    assert_eq!(styled.styles.border_spacing_vertical, Length::px(2.0));
  }

  #[test]
  fn author_css_overrides_cellspacing_presentational_hint() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("cellspacing".to_string(), "12".to_string())],
      },
      children: vec![],
    };

    let stylesheet = parse_stylesheet("table { border-spacing: 3px 5px; }").unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.border_spacing_horizontal, Length::px(3.0));
    assert_eq!(styled.styles.border_spacing_vertical, Length::px(5.0));
  }

  #[test]
  fn border_spacing_percentages_are_ignored() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let stylesheet = parse_stylesheet("table { border-spacing: 10% 5%; }").unwrap();
    let styled = apply_styles(&dom, &stylesheet);

    // Percentages are invalid for border-spacing; UA defaults remain in place.
    assert_eq!(styled.styles.border_spacing_horizontal, Length::px(2.0));
    assert_eq!(styled.styles.border_spacing_vertical, Length::px(2.0));
  }

  #[test]
  fn border_spacing_negative_lengths_clamp_to_zero() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let stylesheet = parse_stylesheet("table { border-spacing: -3px -4px; }").unwrap();
    let styled = apply_styles(&dom, &stylesheet);

    assert_eq!(styled.styles.border_spacing_horizontal, Length::px(0.0));
    assert_eq!(styled.styles.border_spacing_vertical, Length::px(0.0));
  }

  #[test]
  fn cellpadding_presentational_hint_applies_to_cells() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("cellpadding".to_string(), "10".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "tr".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![DomNode {
          node_type: DomNodeType::Element {
            tag_name: "td".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        }],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let cell = &styled.children[0].children[0];
    assert_eq!(cell.styles.padding_left, Length::px(10.0));
    assert_eq!(cell.styles.padding_right, Length::px(10.0));
    assert_eq!(cell.styles.padding_top, Length::px(10.0));
    assert_eq!(cell.styles.padding_bottom, Length::px(10.0));
  }

  #[test]
  fn author_css_overrides_cellpadding_presentational_hint() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("cellpadding".to_string(), "8".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "tr".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![DomNode {
          node_type: DomNodeType::Element {
            tag_name: "td".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        }],
      }],
    };

    let stylesheet = parse_stylesheet("td { padding: 2px; }").unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    let cell = &styled.children[0].children[0];
    assert_eq!(cell.styles.padding_left, Length::px(2.0));
    assert_eq!(cell.styles.padding_right, Length::px(2.0));
    assert_eq!(cell.styles.padding_top, Length::px(2.0));
    assert_eq!(cell.styles.padding_bottom, Length::px(2.0));
  }

  #[test]
  fn bordercolor_presentational_hint_sets_border_color() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("bordercolor".to_string(), "#00ff00".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "tr".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![DomNode {
          node_type: DomNodeType::Element {
            tag_name: "td".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("bordercolor".to_string(), "rgb(255, 0, 0)".to_string())],
          },
          children: vec![],
        }],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.border_top_color, Rgba::rgb(0, 255, 0));
    let cell = &styled.children[0].children[0];
    assert_eq!(cell.styles.border_top_color, Rgba::rgb(255, 0, 0));
  }

  #[test]
  fn author_css_overrides_bordercolor_presentational_hint() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("bordercolor".to_string(), "#00ff00".to_string())],
      },
      children: vec![],
    };

    let stylesheet = parse_stylesheet("table { border-color: blue; }").unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.border_top_color, Rgba::rgb(0, 0, 255));
  }

  #[test]
  fn border_presentational_hint_sets_table_border_and_collapse() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("border".to_string(), "3".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "tr".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![DomNode {
          node_type: DomNodeType::Element {
            tag_name: "td".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        }],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.border_top_width, Length::px(3.0));
    assert_eq!(styled.styles.border_left_style, BorderStyle::Solid);
    assert!(matches!(
      styled.styles.border_collapse,
      BorderCollapse::Collapse
    ));

    let cell = &styled.children[0].children[0];
    assert_eq!(cell.styles.border_top_width, Length::px(3.0));
    assert_eq!(cell.styles.border_left_style, BorderStyle::Solid);
  }

  #[test]
  fn author_css_overrides_border_presentational_hint() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("border".to_string(), "3".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "tr".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![DomNode {
          node_type: DomNodeType::Element {
            tag_name: "td".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![],
        }],
      }],
    };

    let stylesheet = parse_stylesheet(
      "table { border: 1px dotted red; border-collapse: separate; } td { border: 0; }",
    )
    .unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.border_top_width, Length::px(1.0));
    assert_eq!(styled.styles.border_left_style, BorderStyle::Dotted);
    assert!(matches!(
      styled.styles.border_collapse,
      BorderCollapse::Separate
    ));

    let cell = &styled.children[0].children[0];
    assert_eq!(cell.styles.border_top_width, Length::px(0.0));
    assert!(matches!(cell.styles.border_left_style, BorderStyle::None));
  }

  #[test]
  fn cursor_inherits_and_can_be_overridden() {
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "cursor: move;".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child = styled.children.first().expect("child");
    assert_eq!(child.styles.cursor, CursorKeyword::Move);

    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "cursor: move;".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("style".to_string(), "cursor: pointer;".to_string())],
        },
        children: vec![],
      }],
    };
    let styled = apply_styles(&parent, &StyleSheet::new());
    let child = styled.children.first().expect("child");
    assert_eq!(child.styles.cursor, CursorKeyword::Pointer);
  }

  #[test]
  fn scrollbar_color_inherits() {
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "scrollbar-color: rgb(10, 20, 30) rgb(40, 50, 60);".to_string(),
        )],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child = styled.children.first().expect("child");

    match child.styles.scrollbar_color {
      ScrollbarColor::Colors { thumb, track } => {
        assert_eq!(thumb, Rgba::from_rgba8(10, 20, 30, 255));
        assert_eq!(track, Rgba::from_rgba8(40, 50, 60, 255));
      }
      other => panic!("unexpected scrollbar color: {:?}", other),
    }
  }

  #[test]
  fn ol_type_attribute_sets_list_style_type() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ol".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "A".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.list_style_type,
      crate::style::types::ListStyleType::UpperAlpha
    ));
  }

  #[test]
  fn li_type_attribute_overrides_parent_style() {
    let child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "li".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "i".to_string())],
      },
      children: vec![],
    };
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ol".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "A".to_string())],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let li = styled.children.first().expect("li");
    assert!(matches!(
      li.styles.list_style_type,
      crate::style::types::ListStyleType::LowerRoman
    ));
  }

  #[test]
  fn ul_type_attribute_uses_presentational_hint() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "circle".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.list_style_type,
      crate::style::types::ListStyleType::Circle
    ));
  }

  #[test]
  fn author_css_overrides_type_presentational_hint() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ol".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("type".to_string(), "I".to_string())],
      },
      children: vec![],
    };

    let stylesheet = parse_stylesheet("ol { list-style-type: lower-alpha; }").unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    assert!(matches!(
      styled.styles.list_style_type,
      crate::style::types::ListStyleType::LowerAlpha
    ));
  }

  #[test]
  fn align_attribute_sets_text_align_on_table_cells() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "td".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("align".to_string(), "right".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.text_align,
      crate::style::types::TextAlign::Right
    ));
  }

  #[test]
  fn valign_attribute_sets_vertical_align_on_cells() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "td".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("valign".to_string(), "middle".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.vertical_align,
      crate::style::types::VerticalAlign::Middle
    ));
  }

  #[test]
  fn table_cells_default_to_middle_alignment() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "td".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.vertical_align,
      crate::style::types::VerticalAlign::Middle
    ));
  }

  #[test]
  fn css_overrides_align_presentational_hint() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "td".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("align".to_string(), "center".to_string()),
          ("style".to_string(), "text-align: left;".to_string()),
        ],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.text_align,
      crate::style::types::TextAlign::Left
    ));
  }

  #[test]
  fn inline_styles_override_presentational_hints_in_shadow_trees() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::ShadowRoot {
          mode: crate::dom::ShadowRootMode::Open,
          delegates_focus: false,
        },
        children: vec![DomNode {
          node_type: DomNodeType::Element {
            tag_name: "div".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![
              ("align".to_string(), "center".to_string()),
              ("style".to_string(), "text-align: left;".to_string()),
            ],
          },
          children: vec![],
        }],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let shadow_root = styled.children.first().expect("shadow root");
    let inner = shadow_root.children.first().expect("inner element");
    assert!(matches!(
      inner.styles.text_align,
      crate::style::types::TextAlign::Left
    ));
  }

  #[test]
  fn align_attribute_maps_on_block_elements() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("align".to_string(), "center".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.text_align,
      crate::style::types::TextAlign::Center
    ));
  }

  #[test]
  fn align_attribute_on_table_centers_box() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("align".to_string(), "center".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.text_align,
      crate::style::types::TextAlign::Center
    ));
    assert!(styled.styles.margin_left.is_none());
    assert!(styled.styles.margin_right.is_none());
  }

  #[test]
  fn align_attribute_on_table_left_collapses_right_margin() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("align".to_string(), "left".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.text_align,
      crate::style::types::TextAlign::Left
    ));
    assert_eq!(styled.styles.margin_left, Some(Length::px(0.0)));
    assert!(styled.styles.margin_right.is_none());
  }

  #[test]
  fn align_attribute_on_table_right_collapses_left_margin() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "table".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("align".to_string(), "right".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.text_align,
      crate::style::types::TextAlign::Right
    ));
    assert!(styled.styles.margin_left.is_none());
    assert_eq!(styled.styles.margin_right, Some(Length::px(0.0)));
  }

  #[test]
  fn align_attribute_on_image_sets_float() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "img".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("align".to_string(), "left".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.float,
      crate::style::float::Float::Left
    ));
  }

  #[test]
  fn align_attribute_on_image_sets_vertical_align() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "img".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("align".to_string(), "middle".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.vertical_align,
      crate::style::types::VerticalAlign::Middle
    ));
  }

  #[test]
  fn author_css_overrides_presentational_image_align() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "img".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("align".to_string(), "right".to_string()),
          (
            "style".to_string(),
            "float: left; vertical-align: top;".to_string(),
          ),
        ],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.float,
      crate::style::float::Float::Left
    ));
    assert!(matches!(
      styled.styles.vertical_align,
      crate::style::types::VerticalAlign::Top
    ));
  }

  #[test]
  fn center_element_defaults_to_center_alignment() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "center".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.text_align,
      crate::style::types::TextAlign::Center
    ));
  }

  #[test]
  fn table_cells_wrap_by_default() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "td".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.white_space,
      crate::style::types::WhiteSpace::Normal
    ));
  }

  #[test]
  fn nowrap_attribute_sets_white_space_nowrap() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "td".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("nowrap".to_string(), String::new())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.white_space,
      crate::style::types::WhiteSpace::Nowrap
    ));
  }

  #[test]
  fn author_css_overrides_nowrap_presentational_hint() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "td".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("nowrap".to_string(), String::new()),
          ("style".to_string(), "white-space: normal;".to_string()),
        ],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.white_space,
      crate::style::types::WhiteSpace::Normal
    ));
  }

  #[test]
  fn nobr_element_defaults_to_nowrap() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "nobr".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.white_space,
      crate::style::types::WhiteSpace::Nowrap
    ));
  }

  #[test]
  fn text_align_does_not_reset_text_align_last() {
    let dom = element_with_style("text-align-last: right; text-align: center;");
    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.text_align,
      crate::style::types::TextAlign::Center
    ));
    assert!(matches!(
      styled.styles.text_align_last,
      crate::style::types::TextAlignLast::Right
    ));
  }

  #[test]
  fn text_align_all_does_not_reset_text_align_last() {
    let dom = element_with_style("text-align-last: right; text-align-all: justify;");
    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.text_align,
      crate::style::types::TextAlign::Justify
    ));
    assert!(matches!(
      styled.styles.text_align_last,
      crate::style::types::TextAlignLast::Right
    ));
  }

  #[test]
  fn will_change_parses_hint_list() {
    let dom = element_with_style("will-change: transform, opacity");
    let styled = apply_styles(&dom, &StyleSheet::new());
    match &styled.styles.will_change {
      WillChange::Hints(hints) => {
        assert_eq!(
          hints,
          &vec![
            WillChangeHint::Property("transform".to_string()),
            WillChangeHint::Property("opacity".to_string())
          ]
        );
      }
      WillChange::Auto => panic!("expected hints, got Auto"),
    }
  }

  #[test]
  fn will_change_accepts_auto_and_rejects_invalid() {
    let auto = element_with_style("will-change: auto");
    let styled_auto = apply_styles(&auto, &StyleSheet::new());
    assert!(matches!(styled_auto.styles.will_change, WillChange::Auto));

    let invalid = element_with_style("will-change: auto, transform");
    let styled_invalid = apply_styles(&invalid, &StyleSheet::new());
    assert!(matches!(
      styled_invalid.styles.will_change,
      WillChange::Auto
    ));
  }

  #[test]
  fn text_decoration_propagates_to_descendants() {
    let child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "span".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "text-decoration: underline;".to_string(),
        )],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child_styles = &styled.children[0].styles;
    assert_eq!(child_styles.applied_text_decorations.len(), 1);
    assert!(child_styles.applied_text_decorations[0]
      .decoration
      .lines
      .contains(TextDecorationLine::UNDERLINE));
  }

  #[test]
  fn underline_offset_and_position_inherit() {
    let child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "span".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "text-underline-offset: 2px; text-underline-position: under right;".to_string(),
        )],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    assert!(
      matches!(
        styled.styles.text_underline_position,
        TextUnderlinePosition::UnderRight
      ),
      "parent got {:?}",
      styled.styles.text_underline_position
    );
    let child_styles = &styled.children[0].styles;
    match child_styles.text_underline_offset {
      TextUnderlineOffset::Length(l) => assert!((l.to_px() - 2.0).abs() < 0.01),
      TextUnderlineOffset::Auto => panic!("expected underline offset to inherit, got Auto"),
    }
    assert!(
      matches!(
        child_styles.text_underline_position,
        TextUnderlinePosition::UnderRight
      ),
      "got {:?}",
      child_styles.text_underline_position
    );
  }

  #[test]
  fn text_emphasis_inherits() {
    let child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "span".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                namespace: HTML_NAMESPACE.to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "text-emphasis-style: open dot; text-emphasis-color: red; text-emphasis-position: under left;"
                        .to_string(),
                )],
            },
            children: vec![child],
        };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child_styles = &styled.children[0].styles;
    assert!(matches!(
      child_styles.text_emphasis_style,
      crate::style::types::TextEmphasisStyle::Mark {
        fill: crate::style::types::TextEmphasisFill::Open,
        shape: crate::style::types::TextEmphasisShape::Dot
      }
    ));
    assert_eq!(child_styles.text_emphasis_color, Some(Rgba::RED));
    assert!(matches!(
      child_styles.text_emphasis_position,
      crate::style::types::TextEmphasisPosition::UnderLeft
    ));
  }

  #[test]
  fn line_break_inherits() {
    let child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "span".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "line-break: anywhere;".to_string())],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child_styles = &styled.children[0].styles;
    assert_eq!(child_styles.line_break, LineBreak::Anywhere);
  }

  #[test]
  fn grid_templates_do_not_inherit() {
    let child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "display: grid; \
                     grid-template-columns: [a] 50px [b] 1fr [c]; \
                     grid-template-rows: [top] 10px [middle] auto [bottom]; \
                     grid-template-areas: \"hero sidebar\" \"footer footer\";"
            .to_string(),
        )],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let parent_styles = &styled.styles;
    assert_eq!(parent_styles.grid_template_columns.len(), 2);
    assert_eq!(parent_styles.grid_column_line_names.len(), 3);
    assert_eq!(parent_styles.grid_template_rows.len(), 2);
    assert_eq!(parent_styles.grid_row_line_names.len(), 3);
    assert_eq!(parent_styles.grid_template_areas.len(), 2);

    let child_styles = &styled.children[0].styles;
    assert!(child_styles.grid_template_columns.is_empty());
    assert!(child_styles.grid_template_rows.is_empty());
    assert!(child_styles.grid_template_areas.is_empty());
    assert!(child_styles.grid_column_line_names.is_empty());
    assert!(child_styles.grid_row_line_names.is_empty());
    assert!(child_styles.grid_column_names.is_empty());
    assert!(child_styles.grid_row_names.is_empty());
  }

  #[test]
  fn image_rendering_does_not_inherit() {
    let child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "span".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "image-rendering: pixelated;".to_string(),
        )],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    assert!(matches!(
      styled.styles.image_rendering,
      crate::style::types::ImageRendering::Pixelated
    ));
    assert!(matches!(
      styled.children[0].styles.image_rendering,
      crate::style::types::ImageRendering::Auto
    ));
  }

  #[test]
  fn image_orientation_inherits() {
    let child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "span".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "image-orientation: 90deg flip;".to_string(),
        )],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let expected = ImageOrientation::Angle {
      quarter_turns: 1,
      flip: true,
    };
    assert_eq!(styled.styles.image_orientation, expected);
    assert_eq!(styled.children[0].styles.image_orientation, expected);
  }

  #[test]
  fn text_decoration_none_breaks_propagation() {
    let child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "span".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "text-decoration: none;".to_string())],
      },
      children: vec![],
    };
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "text-decoration: underline;".to_string(),
        )],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child_styles = &styled.children[0].styles;
    assert!(child_styles.applied_text_decorations.is_empty());
  }

  #[test]
  fn text_decoration_adds_to_parent_decoration() {
    let child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "span".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "text-decoration: overline;".to_string(),
        )],
      },
      children: vec![],
    };
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "text-decoration: underline;".to_string(),
        )],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child_styles = &styled.children[0].styles;
    assert_eq!(child_styles.applied_text_decorations.len(), 2);
    assert!(child_styles.applied_text_decorations[0]
      .decoration
      .lines
      .contains(TextDecorationLine::UNDERLINE));
    assert!(child_styles.applied_text_decorations[1]
      .decoration
      .lines
      .contains(TextDecorationLine::OVERLINE));
  }

  #[test]
  fn text_align_justify_all_respects_explicit_last_line_alignment() {
    let dom = element_with_style("text-align-last: right; text-align: justify-all;");
    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.text_align,
      crate::style::types::TextAlign::JustifyAll
    ));
    assert!(matches!(
      styled.styles.text_align_last,
      crate::style::types::TextAlignLast::Right
    ));
  }

  #[test]
  fn ua_link_styles_apply_to_unvisited_links() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "https://example.com".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.color, Rgba::new(0, 0, 238, 1.0));
    assert_eq!(styled.styles.cursor, CursorKeyword::Pointer);
    assert!(styled
      .styles
      .applied_text_decorations
      .iter()
      .any(|d| d.decoration.lines.contains(TextDecorationLine::UNDERLINE)));
  }

  #[test]
  fn ua_link_styles_apply_to_visited_and_active() {
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

    let styled_visited = apply_styles(&visited, &StyleSheet::new());
    assert_eq!(styled_visited.styles.color, Rgba::new(85, 26, 139, 1.0));

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

    let styled_active = apply_styles(&active, &StyleSheet::new());
    assert_eq!(styled_active.styles.color, Rgba::new(255, 0, 0, 1.0));
  }

  #[test]
  fn ua_link_styles_apply_to_hover_and_focus() {
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

    let styled_hover = apply_styles(&hover, &StyleSheet::new());
    assert_eq!(styled_hover.styles.color, Rgba::new(255, 0, 0, 1.0));

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

    let styled_focus = apply_styles(&focus, &StyleSheet::new());
    assert_eq!(styled_focus.styles.outline_style, OutlineStyle::Dotted);
    assert_eq!(styled_focus.styles.outline_width, Length::px(1.0));
  }

  #[test]
  fn ua_hover_focus_do_not_apply_without_flags() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "https://example.com".to_string())],
      },
      children: vec![],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.color, Rgba::new(0, 0, 238, 1.0));
    assert_eq!(styled.styles.outline_style, OutlineStyle::None);
  }

  #[test]
  fn text_align_match_parent_maps_start_end_using_parent_direction() {
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "direction: rtl; text-align: start;".to_string(),
        )],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![(
            "style".to_string(),
            "text-align: match-parent; text-align-last: match-parent;".to_string(),
          )],
        },
        children: vec![],
      }],
    };
    let styled = apply_styles(&parent, &StyleSheet::new());
    let child = styled.children.first().expect("child");
    assert!(matches!(
      child.styles.text_align,
      crate::style::types::TextAlign::Right
    ));
  }

  #[test]
  fn text_align_match_parent_inherits_parent_alignment() {
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "text-align: center;".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("style".to_string(), "text-align: match-parent;".to_string())],
        },
        children: vec![],
      }],
    };
    let styled = apply_styles(&parent, &StyleSheet::new());
    let child = styled.children.first().expect("child");
    assert!(matches!(
      child.styles.text_align,
      crate::style::types::TextAlign::Center
    ));
  }

  #[test]
  fn text_align_last_match_parent_maps_start_end_using_parent_direction() {
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "direction: rtl; text-align-last: start;".to_string(),
        )],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![(
            "style".to_string(),
            "text-align-last: match-parent;".to_string(),
          )],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child = styled.children.first().expect("child");
    assert!(matches!(
      child.styles.text_align_last,
      crate::style::types::TextAlignLast::Right
    ));
  }

  #[test]
  fn inherit_keyword_applies_to_non_inherited_width() {
    let child = element_with_style("width: inherit;");
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "width: 80px;".to_string())],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child_styles = &styled.children[0].styles;
    assert_eq!(child_styles.width, Some(Length::px(80.0)));
  }

  #[test]
  fn unset_keyword_inherits_for_inherited_properties() {
    let child = element_with_style("color: unset;");
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "color: red;".to_string())],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child_styles = &styled.children[0].styles;
    assert_eq!(child_styles.color, Rgba::RED);
  }

  #[test]
  fn unset_keyword_resets_non_inherited_border_style() {
    let child = element_with_style("border-style: unset; border-width: 4px;");
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "border-style: solid; border-width: 2px;".to_string(),
        )],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child_styles = &styled.children[0].styles;
    assert!(matches!(child_styles.border_top_style, BorderStyle::None));
    assert!(matches!(child_styles.border_right_style, BorderStyle::None));
    assert!(matches!(
      child_styles.border_bottom_style,
      BorderStyle::None
    ));
    assert!(matches!(child_styles.border_left_style, BorderStyle::None));
  }

  #[test]
  fn initial_keyword_resets_background_color() {
    let child = element_with_style("background-color: initial;");
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "background-color: red;".to_string())],
      },
      children: vec![child],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child_styles = &styled.children[0].styles;
    assert_eq!(child_styles.background_color, Rgba::TRANSPARENT);
  }

  #[test]
  fn text_align_match_parent_sets_last_to_match_parent_and_resolves() {
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "direction: rtl; text-align-last: start;".to_string(),
        )],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("style".to_string(), "text-align: match-parent;".to_string())],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child = styled.children.first().expect("child");
    assert!(matches!(
      child.styles.text_align,
      crate::style::types::TextAlign::Right
    ));
    assert!(matches!(
      child.styles.text_align_last,
      crate::style::types::TextAlignLast::Right
    ));
  }

  #[test]
  fn root_match_parent_text_align_last_computes_to_start() {
    let dom = element_with_style("direction: rtl; text-align-last: match-parent;");
    let styled = apply_styles(&dom, &StyleSheet::new());
    assert!(matches!(
      styled.styles.text_align_last,
      crate::style::types::TextAlignLast::Start
    ));
  }

  #[test]
  fn text_align_match_parent_follows_parent_resolved_end_in_rtl() {
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "direction: rtl; text-align: end; text-align-last: end;".to_string(),
        )],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("style".to_string(), "text-align: match-parent;".to_string())],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let child = styled.children.first().expect("child");
    assert!(matches!(
      child.styles.text_align,
      crate::style::types::TextAlign::Left
    ));
  }

  #[test]
  fn list_style_inherits_from_parent() {
    let parent = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "list-style-type: square; list-style-position: inside;".to_string(),
        )],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("style".to_string(), "color: red;".to_string())],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&parent, &StyleSheet::new());
    let li = styled.children.first().expect("li");
    assert!(matches!(li.styles.list_style_type, ListStyleType::Square));
    assert!(matches!(
      li.styles.list_style_position,
      ListStylePosition::Inside
    ));
  }

  #[test]
  fn text_orientation_inherits_from_parent() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![(
          "style".to_string(),
          "writing-mode: vertical-rl; text-orientation: upright;".to_string(),
        )],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "span".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let child = styled.children.first().expect("span child");
    assert_eq!(child.styles.writing_mode, WritingMode::VerticalRl);
    assert_eq!(child.styles.text_orientation, TextOrientation::Upright);
  }

  #[test]
  fn marker_pseudo_resets_box_model_and_forces_inline_display() {
    let lone_li = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "li".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "color: red;".to_string())],
      },
      children: vec![],
    };
    assert_eq!(
      lone_li.get_attribute("style"),
      Some("color: red;".to_string())
    );
    let decls = parse_declarations("color: red;");
    assert_eq!(decls.len(), 1);
    if let crate::css::types::PropertyValue::Color(c) = &decls[0].value {
      assert_eq!(*c, crate::style::color::Color::Rgba(Rgba::RED));
    } else {
      panic!("color did not parse");
    }
    let mut manual = get_default_styles_for_element(&lone_li);
    inherit_styles(&mut manual, default_computed_style());
    let fs = manual.font_size;
    for decl in parse_declarations("color: red;") {
      apply_declaration(&mut manual, &decl, default_computed_style(), fs, fs);
    }
    assert_eq!(manual.color, Rgba::RED);
    let lone_styled = apply_styles(&lone_li, &StyleSheet::new());
    assert_eq!(lone_styled.styles.color, Rgba::RED);

    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("style".to_string(), "color: red;".to_string())],
        },
        children: vec![],
      }],
    };

    let stylesheet = parse_stylesheet(
      r"
            li::marker {
                color: red;
                display: block;
                padding: 10px;
                margin-left: 12px;
                background: blue;
                text-decoration: underline;
                text-indent: 40px;
            }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let li = styled.children.first().expect("li");
    assert_eq!(li.styles.color, Rgba::RED);
    let marker = li.marker_styles.as_ref().expect("marker styles");

    assert_eq!(marker.color, Rgba::RED);
    assert!(matches!(marker.display, Display::Inline));
    assert!(marker.padding_left.is_zero());
    assert!(marker.padding_right.is_zero());
    assert!(marker.margin_left.unwrap().is_zero());
    assert_eq!(marker.background_color, Rgba::TRANSPARENT);
    assert_eq!(
      marker.text_transform,
      crate::style::types::TextTransform::none()
    );
    assert!(marker
      .text_decoration
      .lines
      .contains(crate::style::types::TextDecorationLine::UNDERLINE));
    assert_eq!(marker.applied_text_decorations.len(), 1);
    assert!(matches!(
      marker.text_align,
      crate::style::types::TextAlign::Start
    ));
    assert_eq!(
      marker.text_indent,
      crate::style::types::TextIndent::default()
    );
    assert!(matches!(marker.float, Float::None));
    assert!(marker.transform.is_empty());
    assert_eq!(marker.opacity, 1.0);
  }

  #[test]
  fn marker_style_inherits_from_list_item_not_parent() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "color: blue;".to_string())],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "ul".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![DomNode {
          node_type: DomNodeType::Element {
            tag_name: "li".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("style".to_string(), "color: red;".to_string())],
          },
          children: vec![],
        }],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let ul = styled.children.first().expect("ul");
    let li = ul.children.first().expect("li");
    let marker = li.marker_styles.as_ref().expect("marker styles");
    assert_eq!(
      marker.color,
      Rgba::RED,
      "marker should inherit color from list item, not its parent"
    );
  }

  #[test]
  fn marker_rule_cannot_override_list_style_type_or_image() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("style".to_string(), "list-style-type: decimal;".to_string())],
        },
        children: vec![],
      }],
    };

    let stylesheet = parse_stylesheet(
      r"
            li::marker {
                list-style-type: square;
                list-style-image: url(bullet.png);
            }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let li = styled.children.first().expect("li");
    let marker = li.marker_styles.as_ref().expect("marker styles");
    // list-style declarations on ::marker are ignored; marker retains the list-item value.
    assert!(
      matches!(marker.list_style_type, ListStyleType::Decimal),
      "marker should ignore list-style-type on ::marker"
    );
    assert!(
      matches!(
        marker.list_style_image,
        crate::style::types::ListStyleImage::None
      ),
      "marker should ignore list-style-image on ::marker"
    );
  }

  #[test]
  fn marker_allows_text_combine_and_typography_but_ignores_non_text_box_properties() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("style".to_string(), "letter-spacing: 0px;".to_string())],
        },
        children: vec![],
      }],
    };

    let stylesheet = parse_stylesheet(
      r"
            li::marker {
                text-combine-upright: all;
                letter-spacing: 2px;
                visibility: hidden;
            }
        ",
    )
    .unwrap();

    let media_ctx = MediaContext::screen(1200.0, 800.0);
    let rule_refs: Vec<CascadeRule<'_>> = stylesheet
      .collect_style_rules(&media_ctx)
      .into_iter()
      .enumerate()
      .map(|(order, rule)| {
        let scope_signature = ScopeSignature::compute(&rule.scopes);
        let scopes = rule.scopes;
        CascadeRule {
          origin: StyleOrigin::Author,
          order,
          rule: rule.rule,
          layer_order: layer_order_with_tree_scope(
            rule.layer_order.as_ref(),
            DOCUMENT_TREE_SCOPE_PREFIX,
          ),
          container_conditions: rule.container_conditions.clone(),
          scopes,
          scope_signature,
          scope: RuleScope::Document,
          starting_style: rule.starting_style,
        }
      })
      .collect();
    let rule_index = RuleIndex::new(rule_refs, QuirksMode::NoQuirks);
    assert_eq!(
      rule_index.rules.len(),
      1,
      "should collect the authored li::marker rule"
    );
    let ancestors: Vec<&DomNode> = vec![&dom]; // ul ancestor for the li
    let element_ref = build_element_ref_chain(&dom.children[0], 0, &ancestors, None, None);
    let mut caches = SelectorCaches::default();
    let cache_epoch = next_selector_cache_epoch();
    caches.set_epoch(cache_epoch);
    let sibling_cache = SiblingListCache::new(cache_epoch);
    let mut context = MatchingContext::new(
      MatchingMode::ForStatelessPseudoElement,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      selectors::matching::NeedsSelectorFlags::No,
      selectors::matching::MatchingForInvalidation::No,
    );
    context.extra_data = ShadowMatchData::for_document().with_sibling_cache(&sibling_cache);
    let selector = rule_index.rules[0]
      .rule
      .selectors
      .slice()
      .first()
      .expect("selector in rule");
    assert!(
      matches_selector(selector, 0, None, &element_ref, &mut context),
      "selector should match the originating element"
    );
    let mut caches = SelectorCaches::default();
    let cache_epoch = next_selector_cache_epoch();
    caches.set_epoch(cache_epoch);
    let sibling_cache = SiblingListCache::new(cache_epoch);
    let mut scratch = CascadeScratch::new(rule_index.rules.len());
    let mut class_keys: Vec<SelectorBucketKey> = Vec::new();
    let mut attr_keys: Vec<SelectorBucketKey> = Vec::new();
    let node_keys = node_selector_keys(&dom.children[0], &mut class_keys, &mut attr_keys);
    let marker_matches = find_pseudo_element_rules(
      &dom.children[0],
      0,
      node_keys,
      &rule_index,
      &mut caches,
      &mut scratch,
      &ancestors,
      None,
      None,
      None,
      None,
      &sibling_cache,
      &PseudoElement::Marker,
      false,
      QuirksMode::NoQuirks,
    );
    assert_eq!(
      marker_matches.len(),
      1,
      "marker rules should match li::marker"
    );

    let styled = apply_styles(&dom, &stylesheet);
    let li = styled.children.first().expect("li");
    let marker = li.marker_styles.as_ref().expect("marker styles");

    assert!(
      marker_allows_property("text-combine-upright"),
      "filter should allow text-combine-upright"
    );
    assert!(
      (marker.letter_spacing - 2.0).abs() < f32::EPSILON,
      "letter-spacing should apply to marker contents (got {})",
      marker.letter_spacing
    );
    assert_eq!(
      marker.text_combine_upright,
      TextCombineUpright::All,
      "text-combine-upright should be honored on ::marker"
    );
    assert!(
      matches!(marker.visibility, Visibility::Visible),
      "non-text properties like visibility should be ignored for marker boxes"
    );
  }

  #[test]
  fn marker_ua_defaults_match_css_lists() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let li = styled.children.first().expect("li");
    let marker = li.marker_styles.as_ref().expect("marker styles");

    assert!(
      matches!(marker.unicode_bidi, UnicodeBidi::Isolate),
      "marker should default unicode-bidi to isolate"
    );
    assert!(
      matches!(marker.white_space, WhiteSpace::Pre),
      "marker should default white-space to pre"
    );
    assert_eq!(
      marker.font_variant_numeric.spacing,
      crate::style::types::NumericSpacing::Tabular,
      "marker should default to tabular numbers"
    );
    assert_eq!(
      marker.text_transform,
      crate::style::types::TextTransform::none(),
      "marker should default text-transform to none"
    );
  }

  #[test]
  fn marker_rejects_alignment_and_indent_declarations() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };

    let stylesheet = parse_stylesheet(
      r"
            li::marker {
                text-align: center;
                text-indent: 40px;
            }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let li = styled.children.first().expect("li");
    let marker = li.marker_styles.as_ref().expect("marker styles");

    assert!(
      matches!(marker.text_align, crate::style::types::TextAlign::Start),
      "text-align should be ignored on ::marker"
    );
    assert_eq!(
      marker.text_indent,
      crate::style::types::TextIndent::default(),
      "text-indent should be ignored on ::marker"
    );

    let stylesheet = parse_stylesheet(
      r"
            li::marker {
                text-decoration: underline;
                text-shadow: 1px 1px red;
            }
        ",
    )
    .unwrap();
    let styled = apply_styles(&dom, &stylesheet);
    let li = styled.children.first().expect("li");
    let marker = li.marker_styles.as_ref().expect("marker styles");

    assert!(
      marker
        .text_decoration
        .lines
        .contains(crate::style::types::TextDecorationLine::UNDERLINE),
      "authored text decorations should apply to ::marker"
    );
    assert!(
      marker.applied_text_decorations.len() == 1,
      "resolved text decorations should be captured for ::marker"
    );
    assert_eq!(
      marker.text_shadow.len(),
      1,
      "text-shadow should apply to ::marker"
    );
  }

  #[test]
  fn marker_alignment_does_not_apply_or_inherit() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![(
            "style".to_string(),
            "text-align: center; text-align-last: right; text-indent: 2em;".to_string(),
          )],
        },
        children: vec![],
      }],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let li = styled.children.first().expect("li");
    let marker = li.marker_styles.as_ref().expect("marker styles");

    assert!(
      matches!(marker.text_align, crate::style::types::TextAlign::Start),
      "::marker should not inherit text-align from the list item"
    );
    assert!(
      matches!(
        marker.text_align_last,
        crate::style::types::TextAlignLast::Auto
      ),
      "::marker should keep default text-align-last"
    );
    assert_eq!(
      marker.text_indent,
      crate::style::types::TextIndent::default(),
      "::marker should not inherit text-indent"
    );
  }

  #[test]
  fn marker_allows_font_shorthand() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };

    let stylesheet = parse_stylesheet(
      r#"
            li::marker {
                font: italic 700 18px/2 "Example";
            }
        "#,
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let li = styled.children.first().expect("li");
    let marker = li.marker_styles.as_ref().expect("marker styles");

    assert_eq!(marker.font_size, 18.0);
    assert!(matches!(
      marker.font_style,
      crate::style::types::FontStyle::Italic
    ));
    assert_eq!(marker.font_weight.to_u16(), 700);
    assert_eq!(
      marker.font_family.first().map(|s| s.as_str()),
      Some("Example")
    );
  }

  #[test]
  fn marker_author_overrides_ua_defaults() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("style".to_string(), "color: black;".to_string())],
        },
        children: vec![],
      }],
    };

    let stylesheet = parse_stylesheet(
      r"
            li::marker {
                white-space: normal;
                unicode-bidi: normal;
                text-transform: uppercase;
            }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let li = styled.children.first().expect("li");
    let marker = li.marker_styles.as_ref().expect("marker styles");

    assert!(
      matches!(marker.white_space, WhiteSpace::Normal),
      "author white-space should override UA default"
    );
    assert!(
      matches!(marker.unicode_bidi, UnicodeBidi::Normal),
      "author unicode-bidi should override UA default"
    );
    assert_eq!(
      marker.text_transform,
      crate::style::types::TextTransform::with_case(crate::style::types::CaseTransform::Uppercase),
      "author text-transform should override UA default"
    );
  }

  #[test]
  fn marker_allows_text_emphasis_properties() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("style".to_string(), "color: black;".to_string())],
        },
        children: vec![],
      }],
    };

    let stylesheet = parse_stylesheet(
      r"
            li::marker {
                text-emphasis-style: open dot;
                text-emphasis-color: red;
                text-emphasis-position: under right;
            }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let li = styled.children.first().expect("li");
    let marker = li.marker_styles.as_ref().expect("marker styles");

    assert_eq!(
      marker.text_emphasis_style,
      crate::style::types::TextEmphasisStyle::Mark {
        fill: crate::style::types::TextEmphasisFill::Open,
        shape: crate::style::types::TextEmphasisShape::Dot
      },
      "text-emphasis-style should apply to ::marker"
    );
    assert_eq!(
      marker.text_emphasis_color,
      Some(Rgba::RED),
      "text-emphasis-color should apply to ::marker"
    );
    assert_eq!(
      marker.text_emphasis_position,
      crate::style::types::TextEmphasisPosition::UnderRight,
      "text-emphasis-position should apply to ::marker"
    );
  }

  #[test]
  fn marker_allows_cursor_property() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "ul".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      }],
    };

    let stylesheet = parse_stylesheet(
      r"
            li::marker {
                cursor: move;
            }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let li = styled.children.first().expect("li");
    let marker = li.marker_styles.as_ref().expect("marker styles");

    assert!(
      matches!(marker.cursor, crate::style::types::CursorKeyword::Move),
      "cursor should be honored on ::marker"
    );
  }

  #[test]
  fn highest_specificity_selector_in_rule_wins() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![
          ("id".to_string(), "target".to_string()),
          ("class".to_string(), "foo bar".to_string()),
        ],
      },
      children: vec![],
    };

    let stylesheet = parse_stylesheet(
      r"
            #target, .foo { color: red; }
            .bar { color: blue; }
        ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    assert_eq!(styled.styles.color, Rgba::RED);
  }

  #[test]
  fn container_conditions_reject_entire_rule() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![
            ("id".to_string(), "target".to_string()),
            ("class".to_string(), "foo".to_string()),
          ],
        },
        children: vec![],
      }],
    };
    let stylesheet = parse_stylesheet(
      r"
        @container (min-width: 200px) {
          #target { color: red; }
        }
        .foo { color: blue; }
      ",
    )
    .unwrap();

    // Root node_id = 1, child node_id = 2. Provide a container that fails the min-width query.
    let mut containers = HashMap::new();
    containers.insert(
      1,
      ContainerQueryInfo {
        inline_size: 100.0,
        block_size: 100.0,
        container_type: ContainerType::Size,
        names: Vec::new(),
        font_size: 16.0,
        styles: Arc::new(ComputedStyle::default()),
      },
    );
    let container_ctx = ContainerQueryContext {
      base_media: MediaContext::screen(800.0, 600.0),
      containers,
    };

    let media_ctx = MediaContext::screen(800.0, 600.0);
    let styled = apply_styles_with_media_target_and_imports(
      &dom,
      &stylesheet,
      &media_ctx,
      None,
      None,
      None,
      Some(&container_ctx),
      None,
      None,
    );
    let child = styled.children.first().expect("child");
    assert_eq!(
      child.styles.color,
      Rgba::BLUE,
      "@container rule should be rejected when container query does not match"
    );
  }

  #[test]
  fn scope_constraints_are_enforced() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "div".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("class".to_string(), "scope".to_string())],
          },
          children: vec![DomNode {
            node_type: DomNodeType::Element {
              tag_name: "p".to_string(),
              namespace: HTML_NAMESPACE.to_string(),
              attributes: vec![("class".to_string(), "target".to_string())],
            },
            children: vec![],
          }],
        },
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "p".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("class".to_string(), "target".to_string())],
          },
          children: vec![],
        },
      ],
    };

    let stylesheet = parse_stylesheet(
      r"
        * { color: blue; }
        @scope (.scope) {
          .target { color: red; }
        }
      ",
    )
    .unwrap();

    let styled = apply_styles(&dom, &stylesheet);
    let in_scope = styled
      .children
      .first()
      .and_then(|node| node.children.first())
      .expect("in-scope node");
    let out_of_scope = styled.children.get(1).expect("out-of-scope node");

    assert_eq!(in_scope.styles.color, Rgba::RED);
    assert_eq!(
      out_of_scope.styles.color,
      Rgba::BLUE,
      "scoped rule should not apply outside its scope root"
    );
  }

  #[test]
  fn scope_resolution_is_cached_across_rules() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("class".to_string(), "outer".to_string())],
      },
      children: vec![
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "p".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("class".to_string(), "target".to_string())],
          },
          children: vec![],
        },
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "div".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![("class".to_string(), "stop".to_string())],
          },
          children: vec![DomNode {
            node_type: DomNodeType::Element {
              tag_name: "p".to_string(),
              namespace: HTML_NAMESPACE.to_string(),
              attributes: vec![("class".to_string(), "target".to_string())],
            },
            children: vec![],
          }],
        },
      ],
    };

    let stylesheet = parse_stylesheet(
      r"
        * { color: blue; background-color: rgb(0, 255, 0); border-left-width: 1px; }
        @scope (.outer) to (.stop) {
          .target { color: red; }
          .target { background-color: blue; }
          .target { border-left-width: 3px; }
        }
      ",
    )
    .unwrap();

    SCOPE_RESOLVE_MISSES.with(|counter| counter.set(0));
    let styled = apply_styles(&dom, &stylesheet);

    let in_scope = styled.children.first().expect("in-scope node");
    let blocked = styled
      .children
      .get(1)
      .and_then(|node| node.children.first())
      .expect("blocked node");

    assert_eq!(in_scope.styles.color, Rgba::RED);
    assert_eq!(in_scope.styles.background_color, Rgba::BLUE);
    assert_eq!(in_scope.styles.border_left_width, Length::px(3.0));

    assert_eq!(blocked.styles.color, Rgba::BLUE);
    assert_eq!(
      blocked.styles.background_color,
      Rgba::from_rgba8(0, 255, 0, 255)
    );
    assert_eq!(blocked.styles.border_left_width, Length::px(1.0));

    let scope_misses = SCOPE_RESOLVE_MISSES.with(|counter| counter.get());
    assert_eq!(
      scope_misses, 2,
      "scope resolution should run once per node for shared @scope contexts"
    );
  }

  #[test]
  fn font_weight_relative_keywords_follow_css_fonts_table() {
    assert_eq!(
      child_font_weight("font-weight: 50;", "font-weight: bolder;"),
      400
    );
    assert_eq!(
      child_font_weight("font-weight: 50;", "font-weight: lighter;"),
      50
    );

    assert_eq!(
      child_font_weight("font-weight: 500;", "font-weight: bolder;"),
      700
    );
    assert_eq!(
      child_font_weight("font-weight: 500;", "font-weight: lighter;"),
      100
    );

    assert_eq!(
      child_font_weight("font-weight: 650;", "font-weight: bolder;"),
      900
    );
    assert_eq!(
      child_font_weight("font-weight: 650;", "font-weight: lighter;"),
      400
    );

    assert_eq!(
      child_font_weight("font-weight: 800;", "font-weight: bolder;"),
      900
    );
    assert_eq!(
      child_font_weight("font-weight: 800;", "font-weight: lighter;"),
      700
    );

    assert_eq!(
      child_font_weight("font-weight: 950;", "font-weight: bolder;"),
      950
    );
    assert_eq!(
      child_font_weight("font-weight: 950;", "font-weight: lighter;"),
      700
    );
  }

  #[test]
  fn out_of_range_font_weight_is_ignored() {
    let dom = element_with_style("font-weight: 1200;");
    let styled = apply_styles(&dom, &StyleSheet::new());
    assert_eq!(styled.styles.font_weight.to_u16(), 400);
  }

  #[test]
  fn color_mix_uses_inherited_current_color() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "color: red;".to_string())],
      },
      children: vec![
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "p".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![(
              "style".to_string(),
              "background-color: color-mix(in srgb, currentColor 50%, blue);".to_string(),
            )],
          },
          children: vec![],
        },
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "span".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![(
              "style".to_string(),
              "color: color-mix(in srgb, currentColor 25%, white);".to_string(),
            )],
          },
          children: vec![],
        },
      ],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let first = styled.children.first().expect("first child");
    assert_eq!(
      first.styles.background_color,
      Rgba::from_rgba8(128, 0, 128, 255)
    );

    let second = styled.children.get(1).expect("second child");
    assert_eq!(second.styles.color, Rgba::from_rgba8(255, 191, 191, 255));
  }

  #[test]
  fn contrast_and_relative_colors_resolve_in_styles() {
    let dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("style".to_string(), "color: white;".to_string())],
      },
      children: vec![
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "p".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![(
              "style".to_string(),
              "background-color: color-contrast(white, black);".to_string(),
            )],
          },
          children: vec![],
        },
        DomNode {
          node_type: DomNodeType::Element {
            tag_name: "span".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![(
              "style".to_string(),
              "color: blue; border-color: color(from currentColor hsl h s 25%);".to_string(),
            )],
          },
          children: vec![],
        },
      ],
    };

    let styled = apply_styles(&dom, &StyleSheet::new());
    let first = styled.children.first().expect("first child");
    assert_eq!(first.styles.background_color, Rgba::BLACK);

    let second = styled.children.get(1).expect("second child");
    let expected_border = Color::parse("color(from blue hsl h s 25%)")
      .unwrap()
      .to_rgba(Rgba::BLUE);
    assert_eq!(second.styles.border_top_color, expected_border);
    assert_eq!(second.styles.border_right_color, expected_border);
    assert_eq!(second.styles.border_bottom_color, expected_border);
    assert_eq!(second.styles.border_left_color, expected_border);
  }
}

fn earliest_element_index_in_path(ancestors: &[&DomNode]) -> usize {
  // If no ancestor is an element, fall back to the node itself (index = ancestors.len()).
  ancestors
    .iter()
    .position(|n| n.is_element())
    .unwrap_or(ancestors.len())
}

fn resolve_scopes<'a>(
  node: &'a DomNode,
  ancestors: &[&'a DomNode],
  scopes: &[ScopeContext<'a>],
  context: &mut MatchingContext<FastRenderSelectorImpl>,
) -> Option<ScopeMatchIndex> {
  if scopes.is_empty() {
    return None;
  }

  #[cfg(test)]
  {
    SCOPE_RESOLVE_MISSES.with(|counter| counter.set(counter.get().saturating_add(1)));
  }

  let path_len = ancestors.len().saturating_add(1);

  let mut root_index = earliest_element_index_in_path(ancestors);
  let mut root = if root_index == ancestors.len() {
    node
  } else {
    ancestors[root_index]
  };

  for scope in scopes {
    if !root.is_element() {
      return None;
    }
    let base_ref = ElementRef::with_ancestors(root, &ancestors[..root_index]);

    if !scope.implicit_start {
      let start_selectors = scope.start?;
      let mut matched_root: Option<usize> = None;
      for idx in (root_index..path_len).rev() {
        let candidate = if idx == ancestors.len() {
          node
        } else {
          ancestors[idx]
        };
        if !candidate.is_element() {
          continue;
        }
        let candidate_ref = ElementRef::with_ancestors(candidate, &ancestors[..idx]);
        let matches = context.nest_for_scope_condition(Some(base_ref.opaque()), |ctx| {
          start_selectors.iter().any(|sel| {
            let hashes = cascade_ancestor_hashes(sel, ctx.quirks_mode());
            matches_selector_cascade(sel, Some(&hashes), &candidate_ref, ctx)
          })
        });
        if matches {
          matched_root = Some(idx);
          break;
        }
      }

      let idx = matched_root?;
      root_index = idx;
      root = if idx == ancestors.len() {
        node
      } else {
        ancestors[idx]
      };
    }

    if let Some(limit_selectors) = scope.end {
      let scope_ref = ElementRef::with_ancestors(root, &ancestors[..root_index]);
      for idx in (root_index + 1)..path_len {
        let candidate = if idx == ancestors.len() {
          node
        } else {
          ancestors[idx]
        };
        if !candidate.is_element() {
          continue;
        }
        let candidate_ref = ElementRef::with_ancestors(candidate, &ancestors[..idx]);
        let blocked = context.nest_for_scope_condition(Some(scope_ref.opaque()), |ctx| {
          limit_selectors.iter().any(|sel| {
            let hashes = cascade_ancestor_hashes(sel, ctx.quirks_mode());
            matches_selector_cascade(sel, Some(&hashes), &candidate_ref, ctx)
          })
        });
        if blocked {
          return None;
        }
      }
    }
  }

  Some(ScopeMatchIndex { root_index })
}

fn take_matching_error(
  context: &mut MatchingContext<FastRenderSelectorImpl>,
) -> Result<(), RenderError> {
  if let Some(err) = context.extra_data.deadline_error.take() {
    Err(err)
  } else {
    Ok(())
  }
}
fn find_matching_rules<'a>(
  node: &DomNode,
  rules: &'a RuleIndex<'a>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  ancestors: &[&DomNode],
  ancestor_bloom: Option<&selectors::bloom::BloomFilter>,
  ancestor_ids: &[usize],
  node_id: usize,
  container_ctx: Option<&ContainerQueryContext>,
  dom_maps: &DomMaps,
  slot_assignment: &SlotAssignment,
  slot_map: Option<&'a SlotAssignmentMap<'a>>,
  element_attr_cache: Option<&'a ElementAttrCache>,
  sibling_cache: &SiblingListCache,
  allow_shadow_host: bool,
  quirks_mode: QuirksMode,
) -> Result<Vec<MatchedRule<'a>>, RenderError> {
  if !node.is_element() {
    return Ok(Vec::new());
  }
  let assigned_slot = slot_assignment.node_to_slot.get(&node_id);
  let current_shadow = dom_maps.containing_shadow_root(node_id);
  let node_tree_scope_prefix = current_shadow
    .map(shadow_tree_scope_prefix)
    .unwrap_or(DOCUMENT_TREE_SCOPE_PREFIX);
  let profiling = cascade_profile_enabled();
  let start = profiling.then(|| Instant::now());
  let node_summary = if rules.has_has_requirements {
    dom_maps
      .selector_blooms()
      .and_then(|store| store.summary_for_id(node_id))
  } else {
    None
  };
  let node_keys = dom_maps.selector_keys(node_id);
  let candidates = &mut scratch.candidates;
  candidates.clear();
  scratch.candidate_stats.reset();
  rules.selector_candidates(
    node,
    node_keys,
    node_summary,
    quirks_mode,
    candidates,
    &mut scratch.candidate_seen,
    &mut scratch.candidate_stats,
    &mut scratch.candidate_merge,
  );
  let slotted_candidates = &mut scratch.slotted_candidates;
  slotted_candidates.clear();
  if assigned_slot.is_some() {
    rules.slotted_candidates(
      node,
      node_keys,
      node_summary,
      quirks_mode,
      slotted_candidates,
      &mut scratch.candidate_seen,
      &mut scratch.candidate_stats,
      &mut scratch.candidate_merge,
    );
  }
  if candidates.is_empty() && slotted_candidates.is_empty() {
    scratch.match_index.reset();
    return Ok(Vec::new());
  }
  let mut matches: Vec<MatchedRule<'a>> = Vec::new();
  let mut deadline_counter = 0usize;

  // Build ElementRef chain with proper parent links
  let element_ref = build_element_ref_chain(node, node_id, ancestors, slot_map, element_attr_cache);
  let shadow_host = if allow_shadow_host {
    shadow_host_ref(node, ancestors, slot_map, element_attr_cache)
  } else {
    None
  };

  // Create selector caches and matching context
  let mut context = MatchingContext::new_for_visited(
    MatchingMode::Normal,
    ancestor_bloom,
    selector_caches,
    VisitedHandlingMode::AllLinksVisitedAndUnvisited,
    IncludeStartingStyle::No,
    quirks_mode,
    selectors::matching::NeedsSelectorFlags::No,
    selectors::matching::MatchingForInvalidation::No,
  );
  context.extra_data = ShadowMatchData {
    shadow_host: None,
    slot_map,
    part_export_map: None,
    deadline_error: None,
    selector_blooms: dom_maps.selector_blooms(),
    sibling_cache: Some(sibling_cache),
    element_attr_cache,
  };

  let scope_allows = |scope: &RuleScope, is_slotted: bool| -> bool {
    match scope {
      RuleScope::Document => !is_slotted,
      RuleScope::Shadow { shadow_root_id } => {
        if Some(*shadow_root_id) == current_shadow {
          !is_slotted
        } else if let Some(slot) = assigned_slot {
          slot.shadow_root_id == *shadow_root_id && is_slotted
        } else {
          false
        }
      }
    }
  };

  {
    let mut idx = 0;
    while idx < candidates.len() {
      let first = &rules.selectors[candidates[idx]];
      let rule_idx = first.rule_idx;
      let mut end = idx + 1;
      while end < candidates.len() && rules.selectors[candidates[end]].rule_idx == rule_idx {
        end += 1;
      }

      let rule = &rules.rules[rule_idx];
      let pos = scratch.match_index.get(rule_idx);
      let mut best_specificity = pos.map(|pos| matches[pos].specificity);

      // If we already have a stronger match for this rule, skip the entire group.
      if let Some(best) = best_specificity {
        if first.specificity <= best {
          idx = end;
          continue;
        }
      }

      if !scope_allows(&rule.scope, false) {
        idx = end;
        continue;
      }

      if !rule.container_conditions.is_empty() {
        match container_ctx {
          Some(ctx) if ctx.matches(node_id, ancestor_ids, &rule.container_conditions) => {}
          _ => {
            idx = end;
            continue;
          }
        }
      }

      let scope_match = if rule.scopes.is_empty() {
        Some(ScopeMatchResult::Unscoped)
      } else {
        scratch
          .scope_cache
          .resolve(
            node_id,
            rule.scope_signature,
            node,
            ancestors,
            &rule.scopes,
            &mut context,
          )
          .map(ScopeMatchResult::Scoped)
      };
      take_matching_error(&mut context)?;
      let Some(scope_match) = scope_match else {
        idx = end;
        continue;
      };

      for &selector_idx in &candidates[idx..end] {
        let indexed = &rules.selectors[selector_idx];
        check_active_periodic(
          &mut deadline_counter,
          CASCADE_SELECTOR_DEADLINE_STRIDE,
          RenderStage::Cascade,
        )?;

        if let Some(best) = best_specificity {
          if indexed.specificity <= best {
            continue;
          }
        }

        if let Some(fast_reject) = rules.fast_reject(indexed.fast_reject) {
          if !fast_reject.matches(node_keys) {
            record_rightmost_fast_reject_prune();
            continue;
          }
        }

        let selector = indexed.selector;
        let mut selector_matches = match scope_match {
          ScopeMatchResult::Scoped(scope_root) => {
            let (root, root_ancestors) = scope_root.root_and_ancestors(node, ancestors);
            let scope_ref = ElementRef::with_ancestors(root, root_ancestors);
            match_with_shadow_host(allow_shadow_host, &mut context, shadow_host, |ctx| {
              ctx.nest_for_scope(Some(scope_ref.opaque()), |ctx| {
                matches_selector_cascade_counted(
                  selector,
                  Some(&indexed.ancestor_hashes),
                  &element_ref,
                  ctx,
                )
              })
            })
          }
          ScopeMatchResult::Unscoped => {
            match_with_shadow_host(allow_shadow_host, &mut context, shadow_host, |ctx| {
              matches_selector_cascade_counted(selector, Some(&indexed.ancestor_hashes), &element_ref, ctx)
            })
          }
        };
        take_matching_error(&mut context)?;

        if allow_shadow_host && !selector_matches && selector_contains_host_context(selector) {
          selector_matches = match scope_match {
            ScopeMatchResult::Scoped(scope_root) => {
              let (root, root_ancestors) = scope_root.root_and_ancestors(node, ancestors);
              let scope_ref = ElementRef::with_ancestors(root, root_ancestors);
              match_with_shadow_host(allow_shadow_host, &mut context, shadow_host, |ctx| {
                ctx.nest_for_scope(Some(scope_ref.opaque()), |ctx| {
                  ctx.with_allow_featureless_host_traversal(true, |ctx| {
                    // `:host-context()` selectors are allowed to match outside the shadow tree.
                    //
                    // 1. A shadow host is normally treated as "featureless", which prevents
                    //    matching further ancestor combinators. Enable traversal for this retry.
                    // 2. The shadow-scoped bloom filter may be missing required outer-ancestor
                    //    hashes, so disable bloom fast-reject to avoid false negatives.
                    let prev_bloom_filter = ctx.bloom_filter;
                    ctx.bloom_filter = None;
                    let matched = matches_selector_cascade_counted(
                      selector,
                      Some(&indexed.ancestor_hashes),
                      &element_ref,
                      ctx,
                    );
                    ctx.bloom_filter = prev_bloom_filter;
                    matched
                  })
                })
              })
            }
            ScopeMatchResult::Unscoped => {
              match_with_shadow_host(allow_shadow_host, &mut context, shadow_host, |ctx| {
                ctx.with_allow_featureless_host_traversal(true, |ctx| {
                  let prev_bloom_filter = ctx.bloom_filter;
                  ctx.bloom_filter = None;
                  let matched = matches_selector_cascade_counted(
                    selector,
                    Some(&indexed.ancestor_hashes),
                    &element_ref,
                    ctx,
                  );
                  ctx.bloom_filter = prev_bloom_filter;
                  matched
                })
              })
            }
          };
          take_matching_error(&mut context)?;
        }

        // `:host(...)` can also bridge from a shadow tree to the outer document when there are
        // additional ancestor sequences to the left (e.g. `body :host(.x) .y`). The shadow-scoped
        // bloom filter intentionally omits outer-ancestor hashes, so avoid bloom fast-reject when
        // such a selector is otherwise rejected.
        if allow_shadow_host && !selector_matches && selector_contains_nonleftmost_host(selector) {
          selector_matches = match scope_match {
            ScopeMatchResult::Scoped(scope_root) => {
              let (root, root_ancestors) = scope_root.root_and_ancestors(node, ancestors);
              let scope_ref = ElementRef::with_ancestors(root, root_ancestors);
              match_with_shadow_host(allow_shadow_host, &mut context, shadow_host, |ctx| {
                ctx.nest_for_scope(Some(scope_ref.opaque()), |ctx| {
                  let prev_bloom_filter = ctx.bloom_filter;
                  ctx.bloom_filter = None;
                  let matched = matches_selector_cascade_counted(
                    selector,
                    Some(&indexed.ancestor_hashes),
                    &element_ref,
                    ctx,
                  );
                  ctx.bloom_filter = prev_bloom_filter;
                  matched
                })
              })
            }
            ScopeMatchResult::Unscoped => {
              match_with_shadow_host(allow_shadow_host, &mut context, shadow_host, |ctx| {
                let prev_bloom_filter = ctx.bloom_filter;
                ctx.bloom_filter = None;
                let matched = matches_selector_cascade_counted(
                  selector,
                  Some(&indexed.ancestor_hashes),
                  &element_ref,
                  ctx,
                );
                ctx.bloom_filter = prev_bloom_filter;
                matched
              })
            }
          };
          take_matching_error(&mut context)?;
        }

        if selector_matches {
          let spec = indexed.specificity;
          best_specificity = Some(best_specificity.map_or(spec, |best| best.max(spec)));
          // Selectors are ordered by descending specificity; once we find a match,
          // no remaining selectors can beat it.
          break;
        }
      }

      if let Some(best_specificity) = best_specificity {
        if let Some(pos) = pos {
          if best_specificity > matches[pos].specificity {
            matches[pos].specificity = best_specificity;
          }
        } else {
          let pos = matches.len();
          scratch.match_index.insert(rule_idx, pos);
          matches.push(MatchedRule {
            origin: rule.origin,
            specificity: best_specificity,
            order: rule.order,
            layer_order: rule.layer_order.clone(),
            declarations: Cow::Borrowed(&rule.rule.declarations),
            starting_style: rule.starting_style,
          });
        }
      }

      idx = end;
    }
  }

  if !slotted_candidates.is_empty() {
    let slot_shadow_host = assigned_slot.and_then(|slot| {
      dom_maps
        .shadow_hosts
        .get(&slot.shadow_root_id)
        .and_then(|host_id| {
          dom_maps.id_to_node.get(host_id).map(|ptr| {
            let host_node = unsafe { &**ptr };
            let host_ancestors = dom_maps.ancestors_for(*host_id);
            (host_node, host_ancestors)
          })
        })
    });
    let shadow_host_for_slotted = slot_shadow_host
      .as_ref()
      .map(|(host_node, ancestors)| ElementRef::with_ancestors(*host_node, ancestors.as_slice()))
      .or(shadow_host);

    let slot_node_and_ancestors = assigned_slot.and_then(|slot_info| {
      let slot_ptr = dom_maps.id_to_node.get(&slot_info.slot_node_id)?;
      // Safety: DOM nodes outlive selector matching; pointers are derived from the immutable DOM tree.
      let slot_node = unsafe { &**slot_ptr };
      let slot_ancestors = dom_maps.ancestors_for(slot_info.slot_node_id);
      Some((slot_node, slot_ancestors))
    });

    let mut idx = 0;
    while idx < slotted_candidates.len() {
      let rule_idx = rules.slotted_selectors[slotted_candidates[idx]].rule_idx;
      let mut end = idx + 1;
      while end < slotted_candidates.len()
        && rules.slotted_selectors[slotted_candidates[end]].rule_idx == rule_idx
      {
        end += 1;
      }

      let rule = &rules.rules[rule_idx];
      if !scope_allows(&rule.scope, true) {
        idx = end;
        continue;
      }

      if !rule.container_conditions.is_empty() {
        match container_ctx {
          Some(ctx) if ctx.matches(node_id, ancestor_ids, &rule.container_conditions) => {}
          _ => {
            idx = end;
            continue;
          }
        }
      }

      let scope_match = if rule.scopes.is_empty() {
        Some(ScopeMatchResult::Unscoped)
      } else {
        scratch
          .scope_cache
          .resolve(
            node_id,
            rule.scope_signature,
            node,
            ancestors,
            &rule.scopes,
            &mut context,
          )
          .map(ScopeMatchResult::Scoped)
      };
      take_matching_error(&mut context)?;
      let Some(scope_match) = scope_match else {
        idx = end;
        continue;
      };

      let pos = scratch.match_index.get(rule_idx);
      let mut best_specificity = pos.map(|pos| matches[pos].specificity);

      for &selector_idx in &slotted_candidates[idx..end] {
        let indexed = &rules.slotted_selectors[selector_idx];
        check_active_periodic(
          &mut deadline_counter,
          CASCADE_SELECTOR_DEADLINE_STRIDE,
          RenderStage::Cascade,
        )?;

        let args = match indexed.selector.pseudo_element() {
          Some(PseudoElement::Slotted(args)) => args,
          _ => continue,
        };

        // Only attempt to match when this selector can outrank the current best for the rule.
        if let Some(best) = best_specificity {
          let max_arg_spec = args.iter().map(|sel| sel.specificity()).max().unwrap_or(0);
          let max_possible = indexed
            .prelude_specificity
            .saturating_add(max_arg_spec)
            .saturating_add(1);
          if max_possible <= best {
            continue;
          }
        }

        let mut best_arg_specificity: Option<u32> = None;
        match scope_match {
          ScopeMatchResult::Scoped(scope_root) => {
            let (root, root_ancestors) = scope_root.root_and_ancestors(node, ancestors);
            let scope_ref = ElementRef::with_ancestors(root, root_ancestors);
            match_with_shadow_host(
              allow_shadow_host,
              &mut context,
              shadow_host_for_slotted,
              |ctx| {
                ctx.nest_for_scope(Some(scope_ref.opaque()), |ctx| {
                  for ((sel, hashes), fast_reject) in args
                    .iter()
                    .zip(indexed.args_ancestor_hashes.iter())
                    .zip(indexed.args_fast_reject.iter())
                  {
                    if let Some(fast_reject) = rules.fast_reject(*fast_reject) {
                      if !fast_reject.matches(node_keys) {
                        record_rightmost_fast_reject_prune();
                        continue;
                      }
                    }
                    if matches_selector_cascade_counted(sel, Some(hashes), &element_ref, ctx) {
                      let spec = sel.specificity();
                      best_arg_specificity =
                        Some(best_arg_specificity.map_or(spec, |best| best.max(spec)));
                    }
                  }
                  best_arg_specificity.is_some()
                })
              },
            );
            take_matching_error(&mut context)?;
          }
          ScopeMatchResult::Unscoped => {
            match_with_shadow_host(
              allow_shadow_host,
              &mut context,
              shadow_host_for_slotted,
              |ctx| {
                for ((sel, hashes), fast_reject) in args
                  .iter()
                  .zip(indexed.args_ancestor_hashes.iter())
                  .zip(indexed.args_fast_reject.iter())
                {
                  if let Some(fast_reject) = rules.fast_reject(*fast_reject) {
                    if !fast_reject.matches(node_keys) {
                      record_rightmost_fast_reject_prune();
                      continue;
                    }
                  }
                  if matches_selector_cascade_counted(sel, Some(hashes), &element_ref, ctx) {
                    let spec = sel.specificity();
                    best_arg_specificity =
                      Some(best_arg_specificity.map_or(spec, |best| best.max(spec)));
                  }
                }
              },
            );
            take_matching_error(&mut context)?;
          }
        }

        let Some(best_arg_specificity) = best_arg_specificity else {
          continue;
        };

        if let Some(prelude) = &indexed.prelude {
          if let Some(fast_reject) = rules.fast_reject(indexed.prelude_fast_reject) {
            let Some(slot_info) = assigned_slot else {
              continue;
            };
            if !fast_reject.matches(dom_maps.selector_keys(slot_info.slot_node_id)) {
              record_rightmost_fast_reject_prune();
              continue;
            }
          }
          let prelude_hashes = indexed.prelude_ancestor_hashes.as_ref();
          let Some((slot_node, slot_ancestors)) = slot_node_and_ancestors.as_ref() else {
            continue;
          };
          let slot_ref = ElementRef::with_ancestors(*slot_node, slot_ancestors.as_slice());
          let slot_matches = match scope_match {
            ScopeMatchResult::Scoped(scope_root) => {
              let (root, root_ancestors) = scope_root.root_and_ancestors(node, ancestors);
              let scope_ref = ElementRef::with_ancestors(root, root_ancestors);
              match_with_shadow_host(
                allow_shadow_host,
                &mut context,
                shadow_host_for_slotted,
                |ctx| {
                  ctx.nest_for_scope(Some(scope_ref.opaque()), |ctx| {
                    let prev_bloom_filter = ctx.bloom_filter;
                    ctx.bloom_filter = None;
                    let matched =
                      matches_selector_cascade_counted(prelude, prelude_hashes, &slot_ref, ctx);
                    ctx.bloom_filter = prev_bloom_filter;
                    matched
                  })
                },
              )
            }
            ScopeMatchResult::Unscoped => match_with_shadow_host(
              allow_shadow_host,
              &mut context,
              shadow_host_for_slotted,
              |ctx| {
                let prev_bloom_filter = ctx.bloom_filter;
                ctx.bloom_filter = None;
                let matched =
                  matches_selector_cascade_counted(prelude, prelude_hashes, &slot_ref, ctx);
                ctx.bloom_filter = prev_bloom_filter;
                matched
              },
            ),
          };
          take_matching_error(&mut context)?;
          if !slot_matches {
            continue;
          }
        }

        let spec = indexed
          .prelude_specificity
          .saturating_add(best_arg_specificity)
          // ::slotted() is a pseudo-element and contributes element-level specificity.
          .saturating_add(1);
        best_specificity = Some(best_specificity.map_or(spec, |best| best.max(spec)));
      }

      if let Some(best_specificity) = best_specificity {
        if let Some(pos) = pos {
          if best_specificity > matches[pos].specificity {
            matches[pos].specificity = best_specificity;
          }
        } else {
          let pos = matches.len();
          scratch.match_index.insert(rule_idx, pos);
          // Slotted selectors target a light-DOM element but originate from a shadow tree. Compare
          // them against other matching rules in the element's tree scope so specificity can
          // participate in the cascade.
          let layer_order = if rule.layer_order.first().copied() == Some(node_tree_scope_prefix) {
            Arc::clone(&rule.layer_order)
          } else {
            let mut layer_order = rule.layer_order.to_vec();
            if let Some(prefix) = layer_order.first_mut() {
              *prefix = node_tree_scope_prefix;
            }
            layer_order.into()
          };
          matches.push(MatchedRule {
            origin: rule.origin,
            specificity: best_specificity,
            order: rule.order,
            layer_order,
            declarations: Cow::Borrowed(&rule.rule.declarations),
            starting_style: rule.starting_style,
          });
        }
      }

      idx = end;
    }
  }

  // Sort by specificity (lower specificity first, so later rules override), then document order.
  matches.sort_unstable_by(|a, b| {
    a.specificity
      .cmp(&b.specificity)
      .then(a.order.cmp(&b.order))
  });
  scratch.match_index.reset();

  if profiling {
    let total_candidates = scratch.candidate_stats.total() as usize;
    record_matching_stats(
      total_candidates,
      matches.len(),
      scratch.candidate_stats.pruned as usize,
      &scratch.candidate_stats,
      start.map(|s| s.elapsed()),
    );
  }

  take_matching_error(&mut context)?;

  Ok(matches)
}

/// Find rules that match an element with a specific pseudo-element
fn find_pseudo_element_rules<'a>(
  node: &DomNode,
  node_id: usize,
  node_keys: NodeSelectorKeys<'_>,
  rules: &'a RuleIndex<'a>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  ancestors: &[&DomNode],
  ancestor_bloom: Option<&selectors::bloom::BloomFilter>,
  slot_map: Option<&SlotAssignmentMap<'a>>,
  selector_blooms: Option<&SelectorBloomStore>,
  element_attr_cache: Option<&'a ElementAttrCache>,
  sibling_cache: &SiblingListCache,
  pseudo: &PseudoElement,
  allow_shadow_host: bool,
  quirks_mode: QuirksMode,
) -> Vec<MatchedRule<'a>> {
  if !node.is_element() {
    return Vec::new();
  }
  if !rules.has_pseudo_rules(pseudo) {
    return Vec::new();
  }
  let profiling = cascade_profile_enabled();
  let start = profiling.then(|| Instant::now());
  let node_summary = if rules.has_has_requirements {
    selector_blooms.and_then(|store| store.summary_for_id(node_id))
  } else {
    None
  };
  let candidates = &mut scratch.candidates;
  candidates.clear();
  scratch.candidate_stats.reset();
  rules.pseudo_candidates(
    node,
    pseudo,
    node_keys,
    node_summary,
    quirks_mode,
    candidates,
    &mut scratch.candidate_seen,
    &mut scratch.candidate_stats,
  );
  if candidates.is_empty() {
    scratch.match_index.reset();
    return Vec::new();
  }
  if matches!(pseudo, PseudoElement::Before | PseudoElement::After) {
    let has_content_rule = candidates.iter().any(|&idx| {
      let rule_idx = rules.pseudo_selectors[idx].rule_idx;
      rules.rule_sets_content[rule_idx]
    });
    if !has_content_rule {
      scratch.match_index.reset();
      return Vec::new();
    }
  }
  let mut matches: Vec<MatchedRule<'a>> = Vec::new();

  // Build ElementRef chain with proper parent links
  let element_ref = build_element_ref_chain(node, node_id, ancestors, slot_map, element_attr_cache);
  let shadow_host = if allow_shadow_host {
    shadow_host_ref(node, ancestors, slot_map, element_attr_cache)
  } else {
    None
  };

  // Create selector caches and matching context
  let mut context = MatchingContext::new(
    MatchingMode::ForStatelessPseudoElement,
    ancestor_bloom,
    selector_caches,
    quirks_mode,
    selectors::matching::NeedsSelectorFlags::No,
    selectors::matching::MatchingForInvalidation::No,
  );
  context.extra_data = ShadowMatchData {
    shadow_host: None,
    slot_map,
    part_export_map: None,
    deadline_error: None,
    selector_blooms,
    sibling_cache: Some(sibling_cache),
    element_attr_cache,
  };

  let mut scoped_rule_idx: Option<usize> = None;
  let mut scoped_match: Option<ScopeMatchResult> = None;

  let mut match_candidate = |selector_idx: usize, matches: &mut Vec<MatchedRule<'a>>| -> bool {
    let indexed = &rules.pseudo_selectors[selector_idx];
    if let Some(fast_reject) = rules.fast_reject(indexed.fast_reject) {
      if !fast_reject.matches(node_keys) {
        record_rightmost_fast_reject_prune();
        return false;
      }
    }
    if let Some(pos) = scratch.match_index.get(indexed.rule_idx) {
      if indexed.specificity <= matches[pos].specificity {
        return false;
      }
    }

    let scope_match = if scoped_rule_idx == Some(indexed.rule_idx) {
      scoped_match
    } else {
      let rule = &rules.rules[indexed.rule_idx];
      let resolved = if rule.scopes.is_empty() {
        Some(ScopeMatchResult::Unscoped)
      } else {
        scratch
          .scope_cache
          .resolve(
            node_id,
            rule.scope_signature,
            node,
            ancestors,
            &rule.scopes,
            &mut context,
          )
          .map(ScopeMatchResult::Scoped)
      };
      scoped_rule_idx = Some(indexed.rule_idx);
      scoped_match = resolved;
      resolved
    };

    let Some(scope_match) = scope_match else {
      return false;
    };

    let selector = indexed.selector;
    let mut selector_matches = match scope_match {
      ScopeMatchResult::Scoped(scope_root) => {
        let (root, root_ancestors) = scope_root.root_and_ancestors(node, ancestors);
        let scope_ref = ElementRef::with_ancestors(root, root_ancestors);
        match_with_shadow_host(allow_shadow_host, &mut context, shadow_host, |ctx| {
          ctx.nest_for_scope(Some(scope_ref.opaque()), |ctx| {
            matches_selector_cascade_counted(selector, Some(&indexed.ancestor_hashes), &element_ref, ctx)
          })
        })
      }
      ScopeMatchResult::Unscoped => {
        match_with_shadow_host(allow_shadow_host, &mut context, shadow_host, |ctx| {
          matches_selector_cascade_counted(selector, Some(&indexed.ancestor_hashes), &element_ref, ctx)
        })
      }
    };

    if allow_shadow_host && !selector_matches && selector_contains_host_context(selector) {
      selector_matches = match scope_match {
        ScopeMatchResult::Scoped(scope_root) => {
          let (root, root_ancestors) = scope_root.root_and_ancestors(node, ancestors);
          let scope_ref = ElementRef::with_ancestors(root, root_ancestors);
          match_with_shadow_host(allow_shadow_host, &mut context, shadow_host, |ctx| {
            ctx.nest_for_scope(Some(scope_ref.opaque()), |ctx| {
              ctx.with_allow_featureless_host_traversal(true, |ctx| {
                let prev_bloom_filter = ctx.bloom_filter;
                ctx.bloom_filter = None;
                let matched = matches_selector_cascade_counted(
                  selector,
                  Some(&indexed.ancestor_hashes),
                  &element_ref,
                  ctx,
                );
                ctx.bloom_filter = prev_bloom_filter;
                matched
              })
            })
          })
        }
        ScopeMatchResult::Unscoped => {
          match_with_shadow_host(allow_shadow_host, &mut context, shadow_host, |ctx| {
            ctx.with_allow_featureless_host_traversal(true, |ctx| {
              let prev_bloom_filter = ctx.bloom_filter;
              ctx.bloom_filter = None;
              let matched = matches_selector_cascade_counted(
                selector,
                Some(&indexed.ancestor_hashes),
                &element_ref,
                ctx,
              );
              ctx.bloom_filter = prev_bloom_filter;
              matched
            })
          })
        }
      };
    }

    if allow_shadow_host && !selector_matches && selector_contains_nonleftmost_host(selector) {
      selector_matches = match scope_match {
        ScopeMatchResult::Scoped(scope_root) => {
          let (root, root_ancestors) = scope_root.root_and_ancestors(node, ancestors);
          let scope_ref = ElementRef::with_ancestors(root, root_ancestors);
          match_with_shadow_host(allow_shadow_host, &mut context, shadow_host, |ctx| {
            ctx.nest_for_scope(Some(scope_ref.opaque()), |ctx| {
              let prev_bloom_filter = ctx.bloom_filter;
              ctx.bloom_filter = None;
              let matched = matches_selector_cascade_counted(
                selector,
                Some(&indexed.ancestor_hashes),
                &element_ref,
                ctx,
              );
              ctx.bloom_filter = prev_bloom_filter;
              matched
            })
          })
        }
        ScopeMatchResult::Unscoped => {
          match_with_shadow_host(allow_shadow_host, &mut context, shadow_host, |ctx| {
            let prev_bloom_filter = ctx.bloom_filter;
            ctx.bloom_filter = None;
            let matched = matches_selector_cascade_counted(
              selector,
              Some(&indexed.ancestor_hashes),
              &element_ref,
              ctx,
            );
            ctx.bloom_filter = prev_bloom_filter;
            matched
          })
        }
      };
    }

    if selector_matches {
      let spec = indexed.specificity;
      if let Some(pos) = scratch.match_index.get(indexed.rule_idx) {
        if spec > matches[pos].specificity {
          matches[pos].specificity = spec;
        }
      } else {
        let rule = &rules.rules[indexed.rule_idx];
        let pos = matches.len();
        scratch.match_index.insert(indexed.rule_idx, pos);
        matches.push(MatchedRule {
          origin: rule.origin,
          specificity: spec,
          order: rule.order,
          layer_order: rule.layer_order.clone(),
          declarations: Cow::Borrowed(&rule.rule.declarations),
          starting_style: rule.starting_style,
        });
      }
      return true;
    }

    false
  };

  if matches!(pseudo, PseudoElement::Before | PseudoElement::After) {
    let mut content_matched = false;
    for &selector_idx in candidates.iter().filter(|&&idx| {
      let rule_idx = rules.pseudo_selectors[idx].rule_idx;
      rules.rule_sets_content[rule_idx]
    }) {
      if match_candidate(selector_idx, &mut matches) {
        content_matched = true;
      }
    }

    if !content_matched {
      scratch.match_index.reset();
      return Vec::new();
    }

    for &selector_idx in candidates.iter().filter(|&&idx| {
      let rule_idx = rules.pseudo_selectors[idx].rule_idx;
      !rules.rule_sets_content[rule_idx]
    }) {
      let _ = match_candidate(selector_idx, &mut matches);
    }
  } else {
    for &selector_idx in candidates.iter() {
      let _ = match_candidate(selector_idx, &mut matches);
    }
  }

  matches.sort_by(|a, b| {
    a.specificity
      .cmp(&b.specificity)
      .then(a.order.cmp(&b.order))
  });
  scratch.match_index.reset();

  if profiling {
    let total_candidates = scratch.candidate_stats.total() as usize;
    record_matching_stats(
      total_candidates,
      matches.len(),
      scratch.candidate_stats.pruned as usize,
      &scratch.candidate_stats,
      start.map(|s| s.elapsed()),
    );
  }

  matches
}

fn apply_cascaded_declarations<'a, F>(
  styles: &mut ComputedStyle,
  matched_rules: Vec<MatchedRule<'a>>,
  inline_declarations: Option<Cow<'a, [Declaration]>>,
  inline_layer_order: Option<Arc<[u32]>>,
  parent_styles: &ComputedStyle,
  parent_font_size: f32,
  root_font_size: f32,
  viewport: Size,
  revert_base_styles: &ComputedStyle,
  filter: F,
) where
  F: Fn(&Declaration) -> bool,
{
  let mut flattened: Vec<MatchedDeclaration<'a>> = Vec::new();
  let mut total_decls = 0usize;
  for rule in matched_rules.iter() {
    total_decls += rule.declarations.len();
  }
  if let Some(inline) = inline_declarations.as_ref() {
    total_decls += inline.len();
  }
  if total_decls == 0 {
    resolve_pending_logical_properties(styles);
    resolve_absolute_lengths(styles, root_font_size, viewport);
    return;
  }
  flattened.reserve(total_decls);
  // Store layer orders once per matched rule (and inline style) and reference them by index from
  // each flattened declaration. This avoids an `Arc` clone per declaration.
  let mut layer_orders: Vec<Arc<[u32]>> = Vec::with_capacity(matched_rules.len().saturating_add(1));

  for rule in matched_rules {
    let origin = rule.origin;
    let specificity = rule.specificity;
    let order = rule.order;
    let layer_order_idx = layer_orders.len();
    layer_orders.push(rule.layer_order);

    match rule.declarations {
      Cow::Borrowed(decls) => {
        for (decl_order, declaration) in decls.iter().enumerate() {
          let is_custom_property = declaration.property.starts_with("--");
          flattened.push(MatchedDeclaration {
            important: declaration.important,
            origin,
            specificity,
            rule_order: order,
            decl_order,
            layer_order_idx,
            is_custom_property,
            declaration: Cow::Borrowed(declaration),
          });
        }
      }
      Cow::Owned(decls) => {
        for (decl_order, declaration) in decls.into_iter().enumerate() {
          let is_custom_property = declaration.property.starts_with("--");
          flattened.push(MatchedDeclaration {
            important: declaration.important,
            origin,
            specificity,
            rule_order: order,
            decl_order,
            layer_order_idx,
            is_custom_property,
            declaration: Cow::Owned(declaration),
          });
        }
      }
    }
  }

  if let Some(inline) = inline_declarations {
    let inline_layer_order = inline_layer_order.expect("inline layer order missing");
    let layer_order_idx = layer_orders.len();
    layer_orders.push(inline_layer_order);
    match inline {
      Cow::Borrowed(decls) => {
        for (decl_order, declaration) in decls.iter().enumerate() {
          let is_custom_property = declaration.property.starts_with("--");
          flattened.push(MatchedDeclaration {
            important: declaration.important,
            origin: StyleOrigin::Inline,
            specificity: INLINE_SPECIFICITY,
            rule_order: INLINE_RULE_ORDER,
            decl_order,
            layer_order_idx,
            is_custom_property,
            declaration: Cow::Borrowed(declaration),
          });
        }
      }
      Cow::Owned(decls) => {
        for (decl_order, declaration) in decls.into_iter().enumerate() {
          let is_custom_property = declaration.property.starts_with("--");
          flattened.push(MatchedDeclaration {
            important: declaration.important,
            origin: StyleOrigin::Inline,
            specificity: INLINE_SPECIFICITY,
            rule_order: INLINE_RULE_ORDER,
            decl_order,
            layer_order_idx,
            is_custom_property,
            declaration: Cow::Owned(declaration),
          });
        }
      }
    }
  }

  fn cmp_layer_order_normal(a: &[u32], b: &[u32]) -> std::cmp::Ordering {
    for (ai, bi) in a.iter().zip(b.iter()) {
      if ai != bi {
        return ai.cmp(bi);
      }
    }
    a.len().cmp(&b.len())
  }

  // Per CSS Cascade Layers, important declarations reverse layer order so earlier layers win.
  // Unlayered rules use u32::MAX; reversing the comparator ensures layered !important rules
  // outrank unlayered ones.
  fn cmp_layer_order_important(a: &[u32], b: &[u32]) -> std::cmp::Ordering {
    cmp_layer_order_normal(b, a)
  }

  flattened.sort_unstable_by(|a, b| {
    a.important
      .cmp(&b.important)
      .then(a.origin.rank().cmp(&b.origin.rank()))
      .then_with(|| {
        let a_layer_order = &layer_orders[a.layer_order_idx];
        let b_layer_order = &layer_orders[b.layer_order_idx];
        if Arc::ptr_eq(a_layer_order, b_layer_order) {
          return std::cmp::Ordering::Equal;
        }
        if a.important {
          cmp_layer_order_important(a_layer_order.as_ref(), b_layer_order.as_ref())
        } else {
          cmp_layer_order_normal(a_layer_order.as_ref(), b_layer_order.as_ref())
        }
      })
      .then(a.specificity.cmp(&b.specificity))
      .then(a.rule_order.cmp(&b.rule_order))
      .then(a.decl_order.cmp(&b.decl_order))
  });

  let defaults = default_computed_style();

  // First apply custom properties so later var() resolutions can see them
  // Avoid repeated property.starts_with for the hot loop by checking once.
  for entry in flattened.iter() {
    if entry.is_custom_property {
      if !filter(entry.declaration.as_ref()) {
        continue;
      }
      // Custom properties are handled via a fast-path in `apply_declaration_with_base` and do not
      // participate in `revert-layer`, so we can skip all snapshot bookkeeping here.
      apply_declaration_with_base(
        styles,
        entry.declaration.as_ref(),
        parent_styles,
        defaults,
        None,
        parent_font_size,
        root_font_size,
        viewport,
      );
    }
  }

  // `revert-layer` is rare, but when we track layer bases we clone the current `ComputedStyle` at
  // each layer boundary. This is expensive for large pages (especially with many custom
  // properties), so avoid the work unless `revert-layer` is actually in play.
  let mut track_revert_layer = styles.custom_properties.has_revert_layer_token();
  if !track_revert_layer {
    track_revert_layer = flattened.iter().any(|entry| {
      if entry.is_custom_property {
        return false;
      }
      match &entry.declaration.value {
        PropertyValue::Keyword(kw) => {
          crate::style::custom_property_store::contains_revert_layer_token(kw)
        }
        _ => false,
      }
    });
  }

  // Scope revert-layer bases to the current cascade stratum (origin + importance) so that
  // important declarations don't reuse snapshots from the normal cascade segment.
  let mut layer_snapshots: FxHashMap<Arc<[u32]>, ComputedStyle> = FxHashMap::default();
  let mut layer_snapshot_stratum: Option<(u8, bool)> = None;

  let mut apply_entry = |entry: &MatchedDeclaration<'_>| {
    if !filter(entry.declaration.as_ref()) {
      return;
    }
    let revert_base = match entry.origin {
      StyleOrigin::UserAgent => defaults,
      StyleOrigin::Author | StyleOrigin::Inline => revert_base_styles,
    };
    let revert_layer_base = if track_revert_layer {
      let stratum = (entry.origin.rank(), entry.important);
      if layer_snapshot_stratum != Some(stratum) {
        layer_snapshots.clear();
        layer_snapshot_stratum = Some(stratum);
      }
      let layer_order = &layer_orders[entry.layer_order_idx];
      // `layer_snapshots` is keyed by the declaration's `Arc<[u32]>` layer order. Most
      // declarations in a rule set share the same layer order, so avoid cloning the `Arc` on
      // lookup hits by probing with the borrowed slice and cloning only when we need to insert a
      // new snapshot.
      let layer_base = match layer_snapshots.get_mut(layer_order.as_ref()) {
        Some(existing) => existing,
        None => {
          layer_snapshots.insert(Arc::clone(layer_order), styles.clone());
          layer_snapshots
            .get_mut(layer_order.as_ref())
            .expect("layer snapshot inserted")
        }
      };
      Some(&*layer_base)
    } else {
      None
    };
    apply_declaration_with_base(
      styles,
      entry.declaration.as_ref(),
      parent_styles,
      revert_base,
      revert_layer_base,
      parent_font_size,
      root_font_size,
      viewport,
    );
  };

  // Then apply all other declarations in cascade order
  for entry in flattened.iter() {
    if !entry.is_custom_property {
      apply_entry(entry);
    }
  }
  resolve_pending_logical_properties(styles);

  resolve_absolute_lengths(styles, root_font_size, viewport);
}

fn dir_presentational_hint(
  node: &DomNode,
  fallback_direction: Direction,
  layer_order: &Arc<[u32]>,
  order: usize,
) -> Option<MatchedRule<'static>> {
  let dir = node.get_attribute_ref("dir")?.trim();
  let is_bdo = node
    .tag_name()
    .map(|tag| tag.eq_ignore_ascii_case("bdo"))
    .unwrap_or(false);

  let mut dir_value = if dir.eq_ignore_ascii_case("ltr") {
    Some("ltr")
  } else if dir.eq_ignore_ascii_case("rtl") {
    Some("rtl")
  } else {
    None
  };
  if dir_value.is_none() && dir.eq_ignore_ascii_case("auto") {
    let resolved = resolve_first_strong_direction(node).map(|d| match d {
      TextDirection::Ltr => crate::style::types::Direction::Ltr,
      TextDirection::Rtl => crate::style::types::Direction::Rtl,
    });
    dir_value = Some(match resolved.unwrap_or(fallback_direction) {
      crate::style::types::Direction::Rtl => "rtl",
      crate::style::types::Direction::Ltr => "ltr",
    });
  }
  let dir_value = dir_value?;

  let declarations = if is_bdo {
    if dir_value == "rtl" {
      cached_declarations(
        &PRESENTATIONAL_DIR_OVERRIDE_RTL_DECLS,
        "direction: rtl; unicode-bidi: bidi-override;",
      )
    } else {
      cached_declarations(
        &PRESENTATIONAL_DIR_OVERRIDE_LTR_DECLS,
        "direction: ltr; unicode-bidi: bidi-override;",
      )
    }
  } else if dir_value == "rtl" {
    cached_declarations(
      &PRESENTATIONAL_DIR_ISOLATE_RTL_DECLS,
      "direction: rtl; unicode-bidi: isolate;",
    )
  } else {
    cached_declarations(
      &PRESENTATIONAL_DIR_ISOLATE_LTR_DECLS,
      "direction: ltr; unicode-bidi: isolate;",
    )
  };

  Some(MatchedRule {
    origin: StyleOrigin::Author,
    specificity: 0,
    order,
    layer_order: layer_order.clone(),
    declarations,
    starting_style: false,
  })
}

fn list_type_presentational_hint(
  node: &DomNode,
  layer_order: &Arc<[u32]>,
  order: usize,
) -> Option<MatchedRule<'static>> {
  let tag = node.tag_name()?;
  let ty = node.get_attribute_ref("type")?;
  let mapped = if tag.eq_ignore_ascii_case("ol") || tag.eq_ignore_ascii_case("li") {
    map_ol_type(ty)
  } else if tag.eq_ignore_ascii_case("ul") {
    map_ul_type(ty)
  } else {
    None
  }?;
  let declarations = vec![Declaration {
    property: "list-style-type".to_string(),
    value: PropertyValue::Keyword(mapped.to_string()),
    raw_value: String::new(),
    important: false,
  }];
  Some(MatchedRule {
    origin: StyleOrigin::Author,
    specificity: 0,
    order,
    layer_order: layer_order.clone(),
    declarations: Cow::Owned(declarations),
    starting_style: false,
  })
}

fn map_ol_type(value: &str) -> Option<&'static str> {
  match value.trim() {
    "1" => Some("decimal"),
    "a" => Some("lower-alpha"),
    "A" => Some("upper-alpha"),
    "i" => Some("lower-roman"),
    "I" => Some("upper-roman"),
    _ => None,
  }
}

fn map_ul_type(value: &str) -> Option<&'static str> {
  let value = value.trim();
  if value.eq_ignore_ascii_case("disc") {
    Some("disc")
  } else if value.eq_ignore_ascii_case("circle") {
    Some("circle")
  } else if value.eq_ignore_ascii_case("square") {
    Some("square")
  } else {
    None
  }
}

fn alignment_presentational_hint(
  node: &DomNode,
  layer_order: &Arc<[u32]>,
  order: usize,
) -> Option<MatchedRule<'static>> {
  let tag = node.tag_name()?;
  let is_center = tag.eq_ignore_ascii_case("center");
  let is_cell = tag.eq_ignore_ascii_case("td")
    || tag.eq_ignore_ascii_case("th")
    || tag.eq_ignore_ascii_case("tr");
  let align = node
    .get_attribute_ref("align")
    .or_else(|| is_center.then_some("center"));
  let valign = is_cell.then(|| node.get_attribute_ref("valign")).flatten();
  if align.is_none() && valign.is_none() {
    return None;
  }

  let mut declarations: Vec<Declaration> = Vec::new();

  if let Some(align) = align {
    if let Some(mapped) = map_align(align) {
      if is_cell
        || is_center
        || tag.eq_ignore_ascii_case("table")
        || tag.eq_ignore_ascii_case("div")
        || tag.eq_ignore_ascii_case("p")
        || tag.eq_ignore_ascii_case("h1")
        || tag.eq_ignore_ascii_case("h2")
        || tag.eq_ignore_ascii_case("h3")
        || tag.eq_ignore_ascii_case("h4")
        || tag.eq_ignore_ascii_case("h5")
        || tag.eq_ignore_ascii_case("h6")
      {
        declarations.push(Declaration {
          property: "text-align".to_string(),
          value: PropertyValue::Keyword(mapped.to_string()),
          raw_value: String::new(),
          important: false,
        });
      }
      if tag.eq_ignore_ascii_case("table") {
        match mapped {
          "center" => {
            declarations.push(Declaration {
              property: "margin-left".to_string(),
              value: PropertyValue::Keyword("auto".to_string()),
              raw_value: String::new(),
              important: false,
            });
            declarations.push(Declaration {
              property: "margin-right".to_string(),
              value: PropertyValue::Keyword("auto".to_string()),
              raw_value: String::new(),
              important: false,
            });
          }
          "right" => {
            declarations.push(Declaration {
              property: "margin-left".to_string(),
              value: PropertyValue::Keyword("auto".to_string()),
              raw_value: String::new(),
              important: false,
            });
            declarations.push(Declaration {
              property: "margin-right".to_string(),
              value: PropertyValue::Length(Length::px(0.0)),
              raw_value: String::new(),
              important: false,
            });
          }
          "left" => {
            declarations.push(Declaration {
              property: "margin-left".to_string(),
              value: PropertyValue::Length(Length::px(0.0)),
              raw_value: String::new(),
              important: false,
            });
            declarations.push(Declaration {
              property: "margin-right".to_string(),
              value: PropertyValue::Keyword("auto".to_string()),
              raw_value: String::new(),
              important: false,
            });
          }
          _ => {}
        }
      }
    }
  }

  if is_cell {
    if let Some(valign) = valign {
      if let Some(mapped) = map_valign(valign) {
        declarations.push(Declaration {
          property: "vertical-align".to_string(),
          value: PropertyValue::Keyword(mapped.to_string()),
          raw_value: String::new(),
          important: false,
        });
      }
    }
  }

  if declarations.is_empty() {
    return None;
  }

  Some(MatchedRule {
    origin: StyleOrigin::Author,
    specificity: 0,
    order,
    layer_order: layer_order.clone(),
    declarations: Cow::Owned(declarations),
    starting_style: false,
  })
}

fn dimension_presentational_hint(
  node: &DomNode,
  layer_order: &Arc<[u32]>,
  order: usize,
) -> Option<MatchedRule<'static>> {
  let width = node
    .get_attribute_ref("width")
    .and_then(parse_dimension_attribute);
  let height = node
    .get_attribute_ref("height")
    .and_then(parse_dimension_attribute);

  if width.is_none() && height.is_none() {
    return None;
  }

  let mut declarations = Vec::with_capacity(width.is_some() as usize + height.is_some() as usize);
  if let Some(w) = width {
    declarations.push(Declaration {
      property: "width".to_string(),
      value: PropertyValue::Length(w),
      raw_value: String::new(),
      important: false,
    });
  }
  if let Some(h) = height {
    declarations.push(Declaration {
      property: "height".to_string(),
      value: PropertyValue::Length(h),
      raw_value: String::new(),
      important: false,
    });
  }

  Some(MatchedRule {
    origin: StyleOrigin::Author,
    specificity: 0,
    order,
    layer_order: layer_order.clone(),
    declarations: Cow::Owned(declarations),
    starting_style: false,
  })
}

fn bgcolor_presentational_hint(
  node: &DomNode,
  layer_order: &Arc<[u32]>,
  order: usize,
) -> Option<MatchedRule<'static>> {
  let color = node
    .get_attribute_ref("bgcolor")
    .and_then(parse_color_attribute)?;
  let declarations = vec![Declaration {
    property: "background-color".to_string(),
    value: PropertyValue::Color(Color::Rgba(color)),
    raw_value: String::new(),
    important: false,
  }];
  Some(MatchedRule {
    origin: StyleOrigin::Author,
    specificity: 0,
    order,
    layer_order: layer_order.clone(),
    declarations: Cow::Owned(declarations),
    starting_style: false,
  })
}

fn bordercolor_presentational_hint(
  node: &DomNode,
  layer_order: &Arc<[u32]>,
  order: usize,
) -> Option<MatchedRule<'static>> {
  let color = node
    .get_attribute_ref("bordercolor")
    .and_then(parse_color_attribute)?;
  let declarations = vec![Declaration {
    property: "border-color".to_string(),
    value: PropertyValue::Color(Color::Rgba(color)),
    raw_value: String::new(),
    important: false,
  }];
  Some(MatchedRule {
    origin: StyleOrigin::Author,
    specificity: 0,
    order,
    layer_order: layer_order.clone(),
    declarations: Cow::Owned(declarations),
    starting_style: false,
  })
}

fn resolve_absolute_lengths(styles: &mut ComputedStyle, root_font_size: f32, viewport: Size) {
  let resolve_len = |len: Length| -> Length {
    match len.unit {
      u if u.is_absolute() => Length::px(len.to_px()),
      LengthUnit::Em => Length::px(len.value * styles.font_size),
      LengthUnit::Ex => Length::px(len.value * styles.font_size * 0.5),
      LengthUnit::Ch => Length::px(len.value * styles.font_size * 0.5),
      LengthUnit::Rem => Length::px(len.value * root_font_size),
      u if u.is_viewport_relative() => len
        .resolve_with_viewport(viewport.width, viewport.height)
        .map(Length::px)
        .unwrap_or(len),
      _ => len,
    }
  };

  styles.border_top_width = resolve_len(styles.border_top_width);
  styles.border_right_width = resolve_len(styles.border_right_width);
  styles.border_bottom_width = resolve_len(styles.border_bottom_width);
  styles.border_left_width = resolve_len(styles.border_left_width);
  let resolve_corner = |radius: BorderCornerRadius| BorderCornerRadius {
    x: resolve_len(radius.x),
    y: resolve_len(radius.y),
  };
  styles.border_top_left_radius = resolve_corner(styles.border_top_left_radius);
  styles.border_top_right_radius = resolve_corner(styles.border_top_right_radius);
  styles.border_bottom_right_radius = resolve_corner(styles.border_bottom_right_radius);
  styles.border_bottom_left_radius = resolve_corner(styles.border_bottom_left_radius);
  styles.outline_width = resolve_len(styles.outline_width);
  styles.outline_offset = resolve_len(styles.outline_offset);
  styles.border_spacing_horizontal = resolve_len(styles.border_spacing_horizontal);
  styles.border_spacing_vertical = resolve_len(styles.border_spacing_vertical);

  // Percentage radii remain relative and are handled during paint/layout.
}

fn table_border_value(node: &DomNode) -> Option<Length> {
  node
    .get_attribute_ref("border")
    .and_then(parse_dimension_attribute)
    .filter(|len| len.unit.is_absolute())
}

fn find_table_border_value(ancestors: &[&DomNode], node: &DomNode) -> Option<Length> {
  if let Some(len) = table_border_value(node) {
    return Some(len);
  }
  for ancestor in ancestors.iter().rev() {
    if let Some(tag) = ancestor.tag_name() {
      if tag.eq_ignore_ascii_case("table") {
        return table_border_value(ancestor);
      }
    }
  }
  None
}

fn border_presentational_hint(
  node: &DomNode,
  ancestors: &[&DomNode],
  layer_order: &Arc<[u32]>,
  order: usize,
) -> Option<MatchedRule<'static>> {
  let tag = node.tag_name()?;
  let is_table = tag.eq_ignore_ascii_case("table");
  let border_len = if is_table {
    table_border_value(node)
  } else if tag.eq_ignore_ascii_case("td")
    || tag.eq_ignore_ascii_case("th")
    || tag.eq_ignore_ascii_case("tr")
  {
    find_table_border_value(ancestors, node)
  } else {
    None
  }?;

  let mut declarations = Vec::with_capacity(4);
  declarations.push(Declaration {
    property: "border-width".to_string(),
    value: PropertyValue::Length(border_len),
    raw_value: String::new(),
    important: false,
  });
  declarations.push(Declaration {
    property: "border-style".to_string(),
    value: PropertyValue::Keyword("solid".to_string()),
    raw_value: String::new(),
    important: false,
  });
  // Match the `border: <width> solid` shorthand behavior: unspecified border-color resets to
  // currentColor (not the ComputedStyle default black).
  declarations.push(Declaration {
    property: "border-color".to_string(),
    value: PropertyValue::Keyword("currentcolor".to_string()),
    raw_value: String::new(),
    important: false,
  });
  if is_table && border_len.to_px() > 0.0 {
    declarations.push(Declaration {
      property: "border-collapse".to_string(),
      value: PropertyValue::Keyword("collapse".to_string()),
      raw_value: String::new(),
      important: false,
    });
  }

  Some(MatchedRule {
    origin: StyleOrigin::Author,
    specificity: 0,
    order,
    layer_order: layer_order.clone(),
    declarations: Cow::Owned(declarations),
    starting_style: false,
  })
}

fn cellspacing_presentational_hint(
  node: &DomNode,
  layer_order: &Arc<[u32]>,
  order: usize,
) -> Option<MatchedRule<'static>> {
  let tag = node.tag_name()?;
  if !tag.eq_ignore_ascii_case("table") {
    return None;
  }
  let spacing = node.get_attribute_ref("cellspacing")?;
  let length = parse_dimension_attribute(spacing)?;
  let declarations = vec![Declaration {
    property: "border-spacing".to_string(),
    value: PropertyValue::Length(length),
    raw_value: String::new(),
    important: false,
  }];
  Some(MatchedRule {
    origin: StyleOrigin::Author,
    specificity: 0,
    order,
    layer_order: layer_order.clone(),
    declarations: Cow::Owned(declarations),
    starting_style: false,
  })
}

fn find_cellpadding(ancestors: &[&DomNode], node: &DomNode) -> Option<Length> {
  if let Some(value) = node.get_attribute_ref("cellpadding") {
    if let Some(len) = parse_dimension_attribute(value) {
      return Some(len);
    }
  }

  for ancestor in ancestors.iter().rev() {
    if let Some(tag) = ancestor.tag_name() {
      if tag.eq_ignore_ascii_case("table") {
        if let Some(value) = ancestor.get_attribute_ref("cellpadding") {
          if let Some(len) = parse_dimension_attribute(value) {
            return Some(len);
          }
        }
        break;
      }
    }
  }
  None
}

fn cellpadding_presentational_hint(
  node: &DomNode,
  ancestors: &[&DomNode],
  layer_order: &Arc<[u32]>,
  order: usize,
) -> Option<MatchedRule<'static>> {
  let tag = node.tag_name()?;
  if !tag.eq_ignore_ascii_case("td") && !tag.eq_ignore_ascii_case("th") {
    return None;
  }
  let padding = find_cellpadding(ancestors, node)?;
  let declarations = vec![Declaration {
    property: "padding".to_string(),
    value: PropertyValue::Length(padding),
    raw_value: String::new(),
    important: false,
  }];
  Some(MatchedRule {
    origin: StyleOrigin::Author,
    specificity: 0,
    order,
    layer_order: layer_order.clone(),
    declarations: Cow::Owned(declarations),
    starting_style: false,
  })
}

fn map_align(value: &str) -> Option<&'static str> {
  let value = value.trim();
  if value.eq_ignore_ascii_case("left") {
    Some("left")
  } else if value.eq_ignore_ascii_case("center") {
    Some("center")
  } else if value.eq_ignore_ascii_case("right") {
    Some("right")
  } else if value.eq_ignore_ascii_case("justify") {
    Some("justify")
  } else {
    None
  }
}

fn map_valign(value: &str) -> Option<&'static str> {
  let value = value.trim();
  if value.eq_ignore_ascii_case("top") {
    Some("top")
  } else if value.eq_ignore_ascii_case("middle") {
    Some("middle")
  } else if value.eq_ignore_ascii_case("bottom") {
    Some("bottom")
  } else if value.eq_ignore_ascii_case("baseline") {
    Some("baseline")
  } else if value.eq_ignore_ascii_case("texttop") || value.eq_ignore_ascii_case("text-top") {
    Some("text-top")
  } else if value.eq_ignore_ascii_case("absmiddle") {
    Some("middle")
  } else if value.eq_ignore_ascii_case("absbottom")
    || value.eq_ignore_ascii_case("textbottom")
    || value.eq_ignore_ascii_case("text-bottom")
  {
    Some("text-bottom")
  } else {
    None
  }
}

fn replaced_alignment_presentational_hint(
  node: &DomNode,
  layer_order: &Arc<[u32]>,
  order: usize,
) -> Option<MatchedRule<'static>> {
  let tag = node.tag_name()?;
  // HTML presentational align on replaced elements (e.g., img, object) maps to float/vertical-align.
  if !tag.eq_ignore_ascii_case("img")
    && !tag.eq_ignore_ascii_case("object")
    && !tag.eq_ignore_ascii_case("embed")
    && !tag.eq_ignore_ascii_case("iframe")
    && !tag.eq_ignore_ascii_case("applet")
    && !tag.eq_ignore_ascii_case("video")
    && !tag.eq_ignore_ascii_case("canvas")
  {
    return None;
  }

  let align = node.get_attribute_ref("align")?;
  let align = align.trim();
  let float = if align.eq_ignore_ascii_case("left") {
    Some("left")
  } else if align.eq_ignore_ascii_case("right") {
    Some("right")
  } else {
    None
  };
  let valign = map_valign(align);
  if float.is_none() && valign.is_none() {
    return None;
  }

  let mut declarations = Vec::with_capacity(float.is_some() as usize + valign.is_some() as usize);
  if let Some(float) = float {
    declarations.push(Declaration {
      property: "float".to_string(),
      value: PropertyValue::Keyword(float.to_string()),
      raw_value: String::new(),
      important: false,
    });
  }
  if let Some(valign) = valign {
    declarations.push(Declaration {
      property: "vertical-align".to_string(),
      value: PropertyValue::Keyword(valign.to_string()),
      raw_value: String::new(),
      important: false,
    });
  }

  Some(MatchedRule {
    origin: StyleOrigin::Author,
    specificity: 0,
    order,
    layer_order: layer_order.clone(),
    declarations: Cow::Owned(declarations),
    starting_style: false,
  })
}

fn nowrap_presentational_hint(
  node: &DomNode,
  layer_order: &Arc<[u32]>,
  order: usize,
) -> Option<MatchedRule<'static>> {
  let tag = node.tag_name()?;
  if !tag.eq_ignore_ascii_case("td") && !tag.eq_ignore_ascii_case("th") {
    return None;
  }
  node.get_attribute_ref("nowrap")?;

  Some(MatchedRule {
    origin: StyleOrigin::Author,
    specificity: 0,
    order,
    layer_order: layer_order.clone(),
    declarations: cached_declarations(&PRESENTATIONAL_NOWRAP_DECLS, "white-space: nowrap;"),
    starting_style: false,
  })
}

fn hidden_presentational_hint(
  node: &DomNode,
  layer_order: &Arc<[u32]>,
  order: usize,
) -> Option<MatchedRule<'static>> {
  node.get_attribute_ref("hidden")?;

  Some(MatchedRule {
    origin: StyleOrigin::Author,
    specificity: 0,
    order,
    layer_order: layer_order.clone(),
    declarations: cached_declarations(&PRESENTATIONAL_HIDDEN_DECLS, "display: none;"),
    starting_style: false,
  })
}

fn resolve_relative_font_weight(styles: &mut ComputedStyle, parent: &ComputedStyle) {
  let parent_weight = parent.font_weight.to_u16();
  styles.font_weight = styles.font_weight.resolve_relative(parent_weight);
}

fn propagate_text_decorations(styles: &mut ComputedStyle, parent: &ComputedStyle) {
  styles.applied_text_decorations = parent.applied_text_decorations.clone();
  if styles.text_decoration_line_specified && styles.text_decoration.lines.is_empty() {
    styles.applied_text_decorations.clear();
  }
  if !styles.text_decoration.lines.is_empty() {
    styles
      .applied_text_decorations
      .push(crate::style::types::ResolvedTextDecoration {
        decoration: styles.text_decoration.clone(),
        skip_ink: styles.text_decoration_skip_ink,
        underline_offset: styles.text_underline_offset,
        underline_position: styles.text_underline_position,
      });
  }
}

/// Build an ElementRef with ancestor context
fn build_element_ref_chain<'a>(
  node: &'a DomNode,
  node_id: usize,
  ancestors: &'a [&'a DomNode],
  slot_map: Option<&'a SlotAssignmentMap<'a>>,
  element_attr_cache: Option<&'a ElementAttrCache>,
) -> ElementRef<'a> {
  if ancestors.is_empty() {
    return ElementRef::new(node)
      .with_node_id(node_id)
      .with_slot_map(slot_map)
      .with_attr_cache(element_attr_cache);
  }

  // Create ElementRef with all ancestors
  ElementRef::with_ancestors(node, ancestors)
    .with_node_id(node_id)
    .with_slot_map(slot_map)
    .with_attr_cache(element_attr_cache)
}

fn is_shadow_host_node(node: &DomNode) -> bool {
  matches!(
    node.node_type,
    DomNodeType::Element { .. } | DomNodeType::Slot { .. }
  ) && node
    .children
    .iter()
    .any(|child| matches!(child.node_type, DomNodeType::ShadowRoot { .. }))
}

fn shadow_host_ref<'a>(
  node: &'a DomNode,
  ancestors: &'a [&'a DomNode],
  slot_map: Option<&'a SlotAssignmentMap<'a>>,
  element_attr_cache: Option<&'a ElementAttrCache>,
) -> Option<ElementRef<'a>> {
  if is_shadow_host_node(node) {
    return Some(
      ElementRef::with_ancestors(node, ancestors)
        .with_slot_map(slot_map)
        .with_attr_cache(element_attr_cache),
    );
  }

  for (idx, ancestor) in ancestors.iter().enumerate().rev() {
    if matches!(ancestor.node_type, DomNodeType::ShadowRoot { .. }) {
      if idx == 0 {
        return None;
      }
      let host = ancestors[idx - 1];
      if is_shadow_host_node(host) {
        return Some(
          ElementRef::with_ancestors(host, &ancestors[..idx - 1])
            .with_slot_map(slot_map)
            .with_attr_cache(element_attr_cache),
        );
      }
    }
  }

  None
}

fn with_shadow_host<'a, F, R>(
  context: &mut MatchingContext<FastRenderSelectorImpl>,
  shadow_host: Option<ElementRef<'a>>,
  f: F,
) -> R
where
  F: FnOnce(&mut MatchingContext<FastRenderSelectorImpl>) -> R,
{
  let prev_shadow_host = context.extra_data.shadow_host;
  let result = if let Some(host) = shadow_host {
    context.extra_data.shadow_host = Some(host.opaque());
    context.with_shadow_host(Some(host), f)
  } else {
    context.extra_data.shadow_host = None;
    f(context)
  };
  context.extra_data.shadow_host = prev_shadow_host;
  result
}

// Only inject a shadow-host context for selectors that originate from shadow-scoped styles.
fn match_with_shadow_host<'a, F, R>(
  allow_shadow_host: bool,
  context: &mut MatchingContext<FastRenderSelectorImpl>,
  shadow_host: Option<ElementRef<'a>>,
  f: F,
) -> R
where
  F: FnOnce(&mut MatchingContext<FastRenderSelectorImpl>) -> R,
{
  if allow_shadow_host {
    with_shadow_host(context, shadow_host, f)
  } else {
    f(context)
  }
}

fn selector_contains_host_context(
  selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>,
) -> bool {
  use selectors::parser::Component;

  fn contains(selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>) -> bool {
    for component in selector.iter_raw_match_order() {
      match component {
        Component::NonTSPseudoClass(pseudo) => {
          if matches!(pseudo, crate::css::selectors::PseudoClass::HostContext(..)) {
            return true;
          }
        }
        Component::Is(list) | Component::Where(list) | Component::Negation(list) => {
          if list.slice().iter().any(contains) {
            return true;
          }
        }
        _ => {}
      }
    }
    false
  }

  contains(selector)
}

fn selector_contains_nonleftmost_host(
  selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>,
) -> bool {
  use selectors::parser::Component;

  fn contains_host(selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>) -> bool {
    for component in selector.iter_raw_match_order() {
      match component {
        Component::Host(..) => return true,
        Component::NonTSPseudoClass(pseudo) => {
          if matches!(pseudo, crate::css::selectors::PseudoClass::Host(..)) {
            return true;
          }
        }
        Component::Is(list) | Component::Where(list) | Component::Negation(list) => {
          if list.slice().iter().any(contains_host) {
            return true;
          }
        }
        _ => {}
      }
    }
    false
  }

  let mut iter = selector.iter();
  loop {
    let mut host_in_sequence = false;
    while let Some(component) = iter.next() {
      match component {
        Component::Host(..) => host_in_sequence = true,
        Component::NonTSPseudoClass(pseudo) => {
          if matches!(pseudo, crate::css::selectors::PseudoClass::Host(..)) {
            host_in_sequence = true;
          }
        }
        Component::Is(list) | Component::Where(list) | Component::Negation(list) => {
          if list.slice().iter().any(contains_host) {
            host_in_sequence = true;
          }
        }
        _ => {}
      }
    }

    match iter.next_sequence() {
      Some(_) => {
        if host_in_sequence {
          return true;
        }
      }
      None => break,
    }
  }

  false
}

/// Compute styles for a pseudo-element (::before or ::after)
///
/// Returns Some(ComputedStyle) if the pseudo-element should be generated
/// (i.e., has content property set to something other than 'none' or 'normal')
fn compute_pseudo_element_styles(
  node: &DomNode,
  rule_scopes: &RuleScopes<'_>,
  scope_host: Option<usize>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  ancestors: &[&DomNode],
  ancestor_bloom: Option<&selectors::bloom::BloomFilter>,
  node_id: usize,
  dom_maps: &DomMaps,
  sibling_cache: &SiblingListCache,
  element_attr_cache: &ElementAttrCache,
  parent_styles: &ComputedStyle,
  ua_parent_styles: &ComputedStyle,
  root_font_size: f32,
  ua_root_font_size: f32,
  viewport: Size,
  pseudo: &PseudoElement,
  include_starting_style: bool,
) -> Option<ComputedStyle> {
  if !scope_has_pseudo_rules(rule_scopes, scope_host, node_id, pseudo) {
    return None;
  }
  // Find rules matching this pseudo-element
  let mut matching_rules = collect_pseudo_matching_rules(
    node,
    rule_scopes,
    scope_host,
    selector_caches,
    scratch,
    ancestors,
    ancestor_bloom,
    node_id,
    dom_maps,
    sibling_cache,
    element_attr_cache,
    pseudo,
  );
  if include_starting_style {
    prioritize_starting_style_rules(&mut matching_rules);
  } else {
    matching_rules.retain(|r| !r.starting_style);
  }

  if matching_rules.is_empty() {
    return None;
  }

  let ua_matches: Vec<_> = matching_rules
    .iter()
    .filter(|r| r.origin == StyleOrigin::UserAgent)
    .cloned()
    .collect();

  let mut ua_styles = ComputedStyle::default();
  ua_styles.display = Display::Inline;
  inherit_styles(&mut ua_styles, ua_parent_styles);
  apply_cascaded_declarations(
    &mut ua_styles,
    ua_matches,
    None,
    None,
    ua_parent_styles,
    ua_parent_styles.font_size,
    ua_root_font_size,
    viewport,
    default_computed_style(),
    |_| true,
  );
  resolve_match_parent_text_align_last(&mut ua_styles, ua_parent_styles, false);
  resolve_match_parent_text_align(&mut ua_styles, ua_parent_styles, false);
  resolve_relative_font_weight(&mut ua_styles, ua_parent_styles);
  propagate_text_decorations(&mut ua_styles, ua_parent_styles);
  ua_styles.root_font_size = ua_root_font_size;
  resolve_line_height_length(&mut ua_styles, viewport);
  resolve_absolute_lengths(&mut ua_styles, ua_root_font_size, viewport);

  // Start with default inline styles (pseudo-elements default to display: inline)
  let mut styles = ComputedStyle::default();
  styles.display = Display::Inline;

  // Inherit from parent element
  inherit_styles(&mut styles, parent_styles);

  // Apply matching declarations
  apply_cascaded_declarations(
    &mut styles,
    matching_rules,
    None,
    None,
    parent_styles,
    parent_styles.font_size,
    root_font_size,
    viewport,
    &ua_styles,
    |_| true,
  );
  resolve_match_parent_text_align(&mut styles, parent_styles, false);
  resolve_match_parent_text_align_last(&mut styles, parent_styles, false);
  resolve_relative_font_weight(&mut styles, parent_styles);
  propagate_text_decorations(&mut styles, parent_styles);
  styles.root_font_size = root_font_size;
  resolve_line_height_length(&mut styles, viewport);
  resolve_absolute_lengths(&mut styles, root_font_size, viewport);

  // Check if content property generates content
  // Per CSS spec, ::before/::after only generate boxes if content is not 'none' or 'normal'
  if matches!(
    styles.content_value,
    crate::style::content::ContentValue::None | crate::style::content::ContentValue::Normal
  ) {
    return None;
  }

  Some(styles)
}

fn first_line_allows_property(property: &str) -> bool {
  let p = property;
  if p == "font" || p.starts_with("font-") {
    return true;
  }
  if matches!(
    p,
    "color" | "text-transform" | "letter-spacing" | "word-spacing" | "line-height"
  ) {
    return true;
  }
  if p == "background-color" {
    return true;
  }
  if p.starts_with("text-decoration")
    || matches!(
      p,
      "text-underline-offset"
        | "text-underline-position"
        | "text-shadow"
        | "text-emphasis"
        | "text-emphasis-style"
        | "text-emphasis-color"
        | "text-emphasis-position"
    )
  {
    return true;
  }
  false
}

fn first_letter_allows_property(property: &str) -> bool {
  let p = property;
  if first_line_allows_property(p) {
    return true;
  }
  if p == "background-color" {
    return true;
  }
  if p == "float" {
    return true;
  }
  if p == "margin" || p.starts_with("margin-") || p == "padding" || p.starts_with("padding-") {
    return true;
  }
  if p.starts_with("border-") {
    return true;
  }
  false
}

fn compute_first_line_styles(
  node: &DomNode,
  rule_scopes: &RuleScopes<'_>,
  scope_host: Option<usize>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  ancestors: &[&DomNode],
  ancestor_bloom: Option<&selectors::bloom::BloomFilter>,
  node_id: usize,
  dom_maps: &DomMaps,
  sibling_cache: &SiblingListCache,
  element_attr_cache: &ElementAttrCache,
  base_styles: &ComputedStyle,
  base_ua_styles: &ComputedStyle,
  root_font_size: f32,
  ua_root_font_size: f32,
  viewport: Size,
  include_starting_style: bool,
) -> Option<(ComputedStyle, ComputedStyle)> {
  if !scope_has_pseudo_rules(rule_scopes, scope_host, node_id, &PseudoElement::FirstLine) {
    return None;
  }

  let mut matching_rules = collect_pseudo_matching_rules(
    node,
    rule_scopes,
    scope_host,
    selector_caches,
    scratch,
    ancestors,
    ancestor_bloom,
    node_id,
    dom_maps,
    sibling_cache,
    element_attr_cache,
    &PseudoElement::FirstLine,
  );
  if include_starting_style {
    prioritize_starting_style_rules(&mut matching_rules);
  } else {
    matching_rules.retain(|r| !r.starting_style);
  }
  if matching_rules.is_empty() {
    return None;
  }
  let ua_matches: Vec<_> = matching_rules
    .iter()
    .filter(|r| r.origin == StyleOrigin::UserAgent)
    .cloned()
    .collect();

  let mut ua_styles = base_ua_styles.clone();
  ua_styles.display = Display::Inline;
  apply_cascaded_declarations(
    &mut ua_styles,
    ua_matches,
    None,
    None,
    base_ua_styles,
    base_ua_styles.font_size,
    ua_root_font_size,
    viewport,
    default_computed_style(),
    |decl| first_line_allows_property(&decl.property),
  );
  resolve_match_parent_text_align_last(&mut ua_styles, base_ua_styles, false);
  resolve_match_parent_text_align(&mut ua_styles, base_ua_styles, false);
  resolve_relative_font_weight(&mut ua_styles, base_ua_styles);
  propagate_text_decorations(&mut ua_styles, base_ua_styles);
  ua_styles.root_font_size = ua_root_font_size;
  resolve_line_height_length(&mut ua_styles, viewport);
  resolve_absolute_lengths(&mut ua_styles, ua_root_font_size, viewport);

  let mut styles = base_styles.clone();
  styles.display = Display::Inline;
  apply_cascaded_declarations(
    &mut styles,
    matching_rules,
    None,
    None,
    base_styles,
    base_styles.font_size,
    root_font_size,
    viewport,
    &ua_styles,
    |decl| first_line_allows_property(&decl.property),
  );
  resolve_match_parent_text_align(&mut styles, base_styles, false);
  resolve_match_parent_text_align_last(&mut styles, base_styles, false);
  resolve_relative_font_weight(&mut styles, base_styles);
  propagate_text_decorations(&mut styles, base_styles);
  styles.root_font_size = root_font_size;
  resolve_line_height_length(&mut styles, viewport);
  resolve_absolute_lengths(&mut styles, root_font_size, viewport);

  Some((styles, ua_styles))
}

fn compute_first_letter_styles(
  node: &DomNode,
  rule_scopes: &RuleScopes<'_>,
  scope_host: Option<usize>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  ancestors: &[&DomNode],
  ancestor_bloom: Option<&selectors::bloom::BloomFilter>,
  node_id: usize,
  dom_maps: &DomMaps,
  sibling_cache: &SiblingListCache,
  element_attr_cache: &ElementAttrCache,
  base_styles: &ComputedStyle,
  base_ua_styles: &ComputedStyle,
  root_font_size: f32,
  ua_root_font_size: f32,
  viewport: Size,
  include_starting_style: bool,
) -> Option<ComputedStyle> {
  if !scope_has_pseudo_rules(
    rule_scopes,
    scope_host,
    node_id,
    &PseudoElement::FirstLetter,
  ) {
    return None;
  }

  let mut matching_rules = collect_pseudo_matching_rules(
    node,
    rule_scopes,
    scope_host,
    selector_caches,
    scratch,
    ancestors,
    ancestor_bloom,
    node_id,
    dom_maps,
    sibling_cache,
    element_attr_cache,
    &PseudoElement::FirstLetter,
  );
  if include_starting_style {
    prioritize_starting_style_rules(&mut matching_rules);
  } else {
    matching_rules.retain(|r| !r.starting_style);
  }
  if matching_rules.is_empty() {
    return None;
  }

  let ua_matches: Vec<_> = matching_rules
    .iter()
    .filter(|r| r.origin == StyleOrigin::UserAgent)
    .cloned()
    .collect();

  let mut ua_styles = base_ua_styles.clone();
  ua_styles.display = Display::Inline;
  apply_cascaded_declarations(
    &mut ua_styles,
    ua_matches,
    None,
    None,
    base_ua_styles,
    base_ua_styles.font_size,
    ua_root_font_size,
    viewport,
    default_computed_style(),
    |decl| first_letter_allows_property(&decl.property),
  );
  resolve_match_parent_text_align_last(&mut ua_styles, base_ua_styles, false);
  resolve_match_parent_text_align(&mut ua_styles, base_ua_styles, false);
  resolve_relative_font_weight(&mut ua_styles, base_ua_styles);
  propagate_text_decorations(&mut ua_styles, base_ua_styles);
  ua_styles.root_font_size = ua_root_font_size;
  resolve_line_height_length(&mut ua_styles, viewport);
  resolve_absolute_lengths(&mut ua_styles, ua_root_font_size, viewport);

  let mut styles = base_styles.clone();
  styles.display = Display::Inline;
  apply_cascaded_declarations(
    &mut styles,
    matching_rules,
    None,
    None,
    base_styles,
    base_styles.font_size,
    root_font_size,
    viewport,
    &ua_styles,
    |decl| first_letter_allows_property(&decl.property),
  );
  resolve_match_parent_text_align(&mut styles, base_styles, false);
  resolve_match_parent_text_align_last(&mut styles, base_styles, false);
  resolve_relative_font_weight(&mut styles, base_styles);
  propagate_text_decorations(&mut styles, base_styles);
  styles.root_font_size = root_font_size;
  resolve_line_height_length(&mut styles, viewport);
  resolve_absolute_lengths(&mut styles, root_font_size, viewport);
  styles.display = Display::Inline;

  Some(styles)
}

fn compute_marker_styles(
  node: &DomNode,
  rule_scopes: &RuleScopes<'_>,
  scope_host: Option<usize>,
  selector_caches: &mut SelectorCaches,
  scratch: &mut CascadeScratch,
  ancestors: &[&DomNode],
  ancestor_bloom: Option<&selectors::bloom::BloomFilter>,
  node_id: usize,
  dom_maps: &DomMaps,
  sibling_cache: &SiblingListCache,
  element_attr_cache: &ElementAttrCache,
  list_item_styles: &ComputedStyle,
  ua_list_item_styles: &ComputedStyle,
  root_font_size: f32,
  ua_root_font_size: f32,
  viewport: Size,
  include_starting_style: bool,
) -> Option<ComputedStyle> {
  if list_item_styles.display != Display::ListItem {
    return None;
  }

  let matching_rules =
    if scope_has_pseudo_rules(rule_scopes, scope_host, node_id, &PseudoElement::Marker) {
      collect_pseudo_matching_rules(
        node,
        rule_scopes,
        scope_host,
        selector_caches,
        scratch,
        ancestors,
        ancestor_bloom,
        node_id,
        dom_maps,
        sibling_cache,
        element_attr_cache,
        &PseudoElement::Marker,
      )
    } else {
      Vec::new()
    };
  let mut matching_rules = matching_rules;
  if include_starting_style {
    prioritize_starting_style_rules(&mut matching_rules);
  } else {
    matching_rules.retain(|r| !r.starting_style);
  }
  let ua_matches: Vec<_> = matching_rules
    .iter()
    .filter(|r| r.origin == StyleOrigin::UserAgent)
    .cloned()
    .collect();

  let mut ua_styles = ComputedStyle::default();
  ua_styles.display = Display::Inline;
  inherit_styles(&mut ua_styles, ua_list_item_styles);

  // UA defaults per CSS Lists 3: isolate, tabular numbers, pre white-space, no transforms.
  ua_styles.unicode_bidi = crate::style::types::UnicodeBidi::Isolate;
  ua_styles.font_variant_numeric.spacing = crate::style::types::NumericSpacing::Tabular;
  ua_styles.white_space = crate::style::types::WhiteSpace::Pre;
  ua_styles.text_transform = crate::style::types::TextTransform::none();

  apply_cascaded_declarations(
    &mut ua_styles,
    ua_matches,
    None,
    None,
    ua_list_item_styles,
    ua_list_item_styles.font_size,
    ua_root_font_size,
    viewport,
    default_computed_style(),
    |_| true,
  );
  resolve_match_parent_text_align_last(&mut ua_styles, ua_list_item_styles, false);
  resolve_match_parent_text_align(&mut ua_styles, ua_list_item_styles, false);
  resolve_relative_font_weight(&mut ua_styles, ua_list_item_styles);
  propagate_text_decorations(&mut ua_styles, ua_list_item_styles);
  ua_styles.root_font_size = ua_root_font_size;
  resolve_line_height_length(&mut ua_styles, viewport);
  resolve_absolute_lengths(&mut ua_styles, ua_root_font_size, viewport);
  reset_marker_box_properties(&mut ua_styles);
  ua_styles.display = Display::Inline;

  let mut styles = ComputedStyle::default();
  styles.display = Display::Inline;
  inherit_styles(&mut styles, list_item_styles);

  // UA defaults per CSS Lists 3: isolate, tabular numbers, pre white-space, no transforms.
  styles.unicode_bidi = crate::style::types::UnicodeBidi::Isolate;
  styles.font_variant_numeric.spacing = crate::style::types::NumericSpacing::Tabular;
  styles.white_space = crate::style::types::WhiteSpace::Pre;
  styles.text_transform = crate::style::types::TextTransform::none();

  apply_cascaded_declarations(
    &mut styles,
    matching_rules,
    None,
    None,
    list_item_styles,
    list_item_styles.font_size,
    root_font_size,
    viewport,
    &ua_styles,
    |decl| marker_allows_property(&decl.property),
  );
  resolve_match_parent_text_align(&mut styles, list_item_styles, false);
  resolve_match_parent_text_align_last(&mut styles, list_item_styles, false);
  resolve_relative_font_weight(&mut styles, list_item_styles);
  propagate_text_decorations(&mut styles, list_item_styles);
  styles.root_font_size = root_font_size;
  resolve_line_height_length(&mut styles, viewport);
  resolve_absolute_lengths(&mut styles, root_font_size, viewport);

  reset_marker_box_properties(&mut styles);
  styles.display = Display::Inline;
  Some(styles)
}

pub(crate) fn reset_marker_box_properties(styles: &mut ComputedStyle) {
  let defaults = ComputedStyle::default();
  styles.position = defaults.position;
  styles.top = None;
  styles.right = None;
  styles.bottom = None;
  styles.left = None;
  styles.z_index = defaults.z_index;

  styles.width = None;
  styles.height = None;
  styles.min_width = None;
  styles.min_height = None;
  styles.max_width = None;
  styles.max_height = None;

  styles.margin_top = defaults.margin_top;
  styles.margin_right = defaults.margin_right;
  styles.margin_bottom = defaults.margin_bottom;
  styles.margin_left = defaults.margin_left;

  styles.padding_top = defaults.padding_top;
  styles.padding_right = defaults.padding_right;
  styles.padding_bottom = defaults.padding_bottom;
  styles.padding_left = defaults.padding_left;

  styles.border_top_width = defaults.border_top_width;
  styles.border_right_width = defaults.border_right_width;
  styles.border_bottom_width = defaults.border_bottom_width;
  styles.border_left_width = defaults.border_left_width;

  styles.border_top_style = defaults.border_top_style;
  styles.border_right_style = defaults.border_right_style;
  styles.border_bottom_style = defaults.border_bottom_style;
  styles.border_left_style = defaults.border_left_style;

  styles.border_top_color = defaults.border_top_color;
  styles.border_right_color = defaults.border_right_color;
  styles.border_bottom_color = defaults.border_bottom_color;
  styles.border_left_color = defaults.border_left_color;

  styles.border_top_left_radius = defaults.border_top_left_radius;
  styles.border_top_right_radius = defaults.border_top_right_radius;
  styles.border_bottom_left_radius = defaults.border_bottom_left_radius;
  styles.border_bottom_right_radius = defaults.border_bottom_right_radius;

  styles.background_color = defaults.background_color;
  styles.background_images = defaults.background_images.clone();
  styles.background_positions = defaults.background_positions.clone();
  styles.background_sizes = defaults.background_sizes.clone();
  styles.background_repeats = defaults.background_repeats.clone();
  styles.background_attachments = defaults.background_attachments.clone();
  styles.background_origins = defaults.background_origins.clone();
  styles.background_clips = defaults.background_clips.clone();
  styles.rebuild_background_layers();
  styles.object_fit = defaults.object_fit;
  styles.object_position = defaults.object_position.clone();

  styles.box_shadow.clear();
  styles.filter.clear();
  styles.backdrop_filter.clear();
  styles.clip_path = defaults.clip_path.clone();
  styles.mix_blend_mode = defaults.mix_blend_mode;
  styles.isolation = defaults.isolation;
  styles.transform.clear();
  styles.transform_box = defaults.transform_box;
  styles.transform_origin = defaults.transform_origin.clone();
  styles.overflow_x = defaults.overflow_x;
  styles.overflow_y = defaults.overflow_y;
  styles.opacity = defaults.opacity;
  // Markers should not be affected by list-item paragraph alignment and indentation.
  // (CSS Lists 3 limits ::marker applicability to text/font properties; alignment is ignored.)
  styles.text_align = defaults.text_align;
  styles.text_align_last = defaults.text_align_last;
  styles.text_indent = defaults.text_indent;
  // Markers should not carry table/layout-specific state
  styles.border_spacing_horizontal = defaults.border_spacing_horizontal;
  styles.border_spacing_vertical = defaults.border_spacing_vertical;
  styles.border_collapse = defaults.border_collapse;
  styles.table_layout = defaults.table_layout;
  styles.caption_side = defaults.caption_side;
  styles.empty_cells = defaults.empty_cells;

  // Outlines do not apply to markers; clear any inherited or authored values.
  styles.outline_color = defaults.outline_color;
  styles.outline_style = defaults.outline_style;
  styles.outline_width = defaults.outline_width;
  styles.outline_offset = defaults.outline_offset;
}

fn marker_allows_property(property: &str) -> bool {
  let p = property;

  // Marker boxes honor a limited set of box-level properties
  // (CSS Lists 3 § 3.1.1).
  if matches!(
    p,
    "content" | "direction" | "unicode-bidi" | "text-combine-upright"
  ) {
    return true;
  }

  // Cursor is explicitly allowed for ::marker in the CSS Pseudo tests.
  if p == "cursor" {
    return true;
  }

  // Font shorthand is allowed; individual font-* longhands are covered below.
  if p == "font" {
    return true;
  }

  // Animations/transitions are explicitly permitted.
  if p.starts_with("animation") || p.starts_with("transition") {
    return true;
  }

  // Inheritable text-affecting properties apply to the marker's contents.
  if p.starts_with("font-") {
    return true;
  }

  matches!(
    p,
    "color"
      | "white-space"
      | "line-height"
      | "letter-spacing"
      | "word-spacing"
      | "text-transform"
      | "text-emphasis"
      | "text-emphasis-style"
      | "text-emphasis-color"
      | "text-emphasis-position"
      | "text-decoration"
      | "text-decoration-line"
      | "text-decoration-style"
      | "text-decoration-color"
      | "text-decoration-thickness"
      | "text-decoration-skip-ink"
      | "text-underline-offset"
      | "text-underline-position"
      | "text-shadow"
      | "line-break"
      | "word-break"
      | "overflow-wrap"
      | "hyphens"
      | "tab-size"
  )
}
