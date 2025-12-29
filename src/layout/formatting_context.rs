//! Formatting Context trait - core layout abstraction
//!
//! This module defines the FormattingContext trait, which is the fundamental
//! abstraction for all layout algorithms in FastRender. Every layout mode
//! (block, inline, flex, grid, table) implements this trait.
//!
//! # The FormattingContext Abstraction
//!
//! In CSS, different display values create different "formatting contexts" with
//! completely different layout rules:
//!
//! - **Block FC**: Boxes stack vertically, margins collapse (CSS 2.1 Section 9.4.1)
//! - **Inline FC**: Boxes flow horizontally, wrap at line boundaries (CSS 2.1 Section 9.4.2)
//! - **Flex FC**: Flexbox layout algorithm (CSS Flexbox spec)
//! - **Grid FC**: Grid layout algorithm (CSS Grid spec)
//! - **Table FC**: Table layout algorithm (CSS 2.1 Section 17)
//!
//! This trait provides a uniform interface that allows the layout engine to
//! dispatch to the appropriate algorithm without knowing implementation details.
//!
//! # Benefits of This Abstraction
//!
//! - **Modularity**: Each FC is independently implementable and testable
//! - **Extensibility**: New FCs can be added without modifying core engine
//! - **Parallelization**: Different FCs can be implemented concurrently in different tasks
//! - **Clarity**: Separates concerns - each FC focuses on one layout mode
//!
//! # References
//!
//! - CSS 2.1 Section 9.4 - Normal Flow: <https://www.w3.org/TR/CSS21/visuren.html#normal-flow>
//! - CSS Flexbox: <https://www.w3.org/TR/css-flexbox-1/>
//! - CSS Grid: <https://www.w3.org/TR/css-grid-1/>

use crate::geometry::Size;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::fragmentation::FragmentationOptions;
use crate::style::display::FormattingContextType;
use crate::style::float::Float;
use crate::style::position::Position;
use crate::style::types::GridTrack;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment_tree::FragmentNode;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Arc;

/// Intrinsic sizing mode for content-based size queries
///
/// CSS defines two fundamental intrinsic sizes:
/// - **Min-content**: The smallest size at which content fits without overflow
/// - **Max-content**: The largest size needed to fit content without wrapping
///
/// These are used for:
/// - `width: min-content` and `width: max-content` CSS properties
/// - Flexbox intrinsic sizing (`flex-basis: content`)
/// - Grid track sizing with `min-content` and `max-content`
/// - Auto table column sizing
///
/// # Examples
///
/// For a text box containing "Hello World":
/// - **MinContent**: Width of longest word ("World") - narrowest without overflow
/// - **MaxContent**: Width of entire text ("Hello World") - widest without wrapping
///
/// # Reference
///
/// CSS Sizing Module Level 3: <https://www.w3.org/TR/css-sizing-3/>
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicSizingMode {
  /// Minimum content size (narrowest possible without overflow)
  ///
  /// Corresponds to CSS `min-content` keyword.
  /// For text, this is typically the width of the longest word.
  MinContent,

  /// Maximum content size (widest without line breaking)
  ///
  /// Corresponds to CSS `max-content` keyword.
  /// For text, this is the width needed to fit all content on one line.
  MaxContent,
}

thread_local! {
    /// Intrinsic sizing cache scoped per-thread so rayon fan-out does not require locking.
    static INTRINSIC_INLINE_CACHE: RefCell<HashMap<(usize, usize, IntrinsicSizingMode), (usize, f32)>> =
        RefCell::new(HashMap::new());
}

thread_local! {
  static FRAGMENTAINER_BLOCK_SIZE_HINT: Cell<Option<f32>> = Cell::new(None);
}

thread_local! {
  /// Subgrid dependency memoization keyed by node id within a cache epoch.
  static SUBGRID_DEPENDENT_CACHE: RefCell<HashMap<usize, (usize, bool)>> =
    RefCell::new(HashMap::new());
  static SUBGRID_CACHE_EPOCH: Cell<usize> = const { Cell::new(0) };
}

static CACHE_LOOKUPS: AtomicUsize = AtomicUsize::new(0);
static CACHE_HITS: AtomicUsize = AtomicUsize::new(0);
static CACHE_STORES: AtomicUsize = AtomicUsize::new(0);
static CACHE_EPOCH: AtomicUsize = AtomicUsize::new(1);
static BLOCK_INTRINSIC_CALLS: AtomicUsize = AtomicUsize::new(0);
static FLEX_INTRINSIC_CALLS: AtomicUsize = AtomicUsize::new(0);
static INLINE_INTRINSIC_CALLS: AtomicUsize = AtomicUsize::new(0);

fn clear_subgrid_cache() {
  SUBGRID_DEPENDENT_CACHE.with(|cache| cache.borrow_mut().clear());
}

fn subgrid_cache_use_epoch(epoch: usize, force_clear: bool) {
  let epoch = epoch.max(1);
  SUBGRID_CACHE_EPOCH.with(|cell| {
    let previous = cell.get();
    if force_clear || previous != epoch {
      clear_subgrid_cache();
      cell.set(epoch);
    }
  });
}

fn cache_key(
  node: &BoxNode,
  mode: IntrinsicSizingMode,
  epoch: usize,
) -> Option<(usize, usize, IntrinsicSizingMode)> {
  let id = node.id();
  if id == 0 || subtree_contains_subgrid(node, epoch) {
    return None;
  }
  let style_ptr = Arc::as_ptr(&node.style) as usize;
  Some((id, style_ptr, mode))
}

pub(crate) fn intrinsic_cache_lookup(node: &BoxNode, mode: IntrinsicSizingMode) -> Option<f32> {
  CACHE_LOOKUPS.fetch_add(1, Ordering::Relaxed);
  let epoch = CACHE_EPOCH.load(Ordering::Relaxed);
  let key = cache_key(node, mode, epoch)?;
  let hit = INTRINSIC_INLINE_CACHE.with(|cache| {
    cache
      .borrow()
      .get(&key)
      .and_then(|(entry_epoch, value)| (*entry_epoch == epoch).then_some(*value))
  });
  if hit.is_some() {
    CACHE_HITS.fetch_add(1, Ordering::Relaxed);
  }
  hit
}

pub(crate) fn intrinsic_cache_store(node: &BoxNode, mode: IntrinsicSizingMode, value: f32) {
  let epoch = CACHE_EPOCH.load(Ordering::Relaxed);
  if let Some(key) = cache_key(node, mode, epoch) {
    INTRINSIC_INLINE_CACHE.with(|cache| {
      cache.borrow_mut().insert(key, (epoch, value));
    });
    CACHE_STORES.fetch_add(1, Ordering::Relaxed);
  }
}

pub(crate) fn intrinsic_cache_clear() {
  INTRINSIC_INLINE_CACHE.with(|cache| cache.borrow_mut().clear());
  clear_subgrid_cache();
}

/// Sets the active intrinsic cache epoch, clearing stale entries when it changes.
///
/// Epochs are used instead of raw clears so callers can opt-in to cache reuse (by
/// reusing the same epoch) or enforce invalidation (by bumping the epoch).
pub(crate) fn intrinsic_cache_use_epoch(epoch: usize, reset_counters: bool) {
  let requested_epoch = epoch;
  let epoch = epoch.max(1);
  let previous = CACHE_EPOCH.swap(epoch, Ordering::Relaxed);
  let epoch_changed = previous != requested_epoch;
  if epoch_changed {
    intrinsic_cache_clear();
  }
  if reset_counters {
    intrinsic_cache_reset_counters();
  }
  subgrid_cache_use_epoch(epoch, reset_counters || epoch_changed);
}

/// Returns the currently active epoch for intrinsic sizing caches.
pub(crate) fn intrinsic_cache_epoch() -> usize {
  CACHE_EPOCH.load(Ordering::Relaxed)
}

pub(crate) fn intrinsic_cache_reset_counters() {
  CACHE_LOOKUPS.store(0, Ordering::Relaxed);
  CACHE_HITS.store(0, Ordering::Relaxed);
  CACHE_STORES.store(0, Ordering::Relaxed);
  BLOCK_INTRINSIC_CALLS.store(0, Ordering::Relaxed);
  FLEX_INTRINSIC_CALLS.store(0, Ordering::Relaxed);
  INLINE_INTRINSIC_CALLS.store(0, Ordering::Relaxed);
}

pub(crate) fn intrinsic_cache_stats() -> (usize, usize, usize, usize, usize, usize) {
  (
    CACHE_LOOKUPS.load(Ordering::Relaxed),
    CACHE_HITS.load(Ordering::Relaxed),
    CACHE_STORES.load(Ordering::Relaxed),
    BLOCK_INTRINSIC_CALLS.load(Ordering::Relaxed),
    FLEX_INTRINSIC_CALLS.load(Ordering::Relaxed),
    INLINE_INTRINSIC_CALLS.load(Ordering::Relaxed),
  )
}

pub(crate) fn count_block_intrinsic_call() {
  BLOCK_INTRINSIC_CALLS.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn count_flex_intrinsic_call() {
  FLEX_INTRINSIC_CALLS.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn count_inline_intrinsic_call() {
  INLINE_INTRINSIC_CALLS.fetch_add(1, Ordering::Relaxed);
}

// === Layout result cache ====================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct LayoutCacheKey {
  box_id: usize,
  fc_type: FormattingContextType,
  style_hash: u64,
  constraints_hash: u64,
  fragmentation_hash: u64,
  viewport_hash: u64,
  subgrid_dependent: bool,
}

#[derive(Debug, Clone)]
struct LayoutCacheEntry {
  epoch: usize,
  fragment: Arc<FragmentNode>,
  style_hash: u64,
}

thread_local! {
  /// Layout result cache kept per-thread to stay contention-free during rayon-powered fan-out.
  static LAYOUT_RESULT_CACHE: RefCell<HashMap<LayoutCacheKey, LayoutCacheEntry>> =
    RefCell::new(HashMap::new());
  static LAYOUT_CACHE_ENABLED: Cell<bool> = const { Cell::new(false) };
  static LAYOUT_CACHE_EPOCH: Cell<usize> = const { Cell::new(1) };
  static LAYOUT_CACHE_FRAGMENTATION: Cell<u64> = const { Cell::new(0) };
  static LAYOUT_CACHE_LOOKUPS: Cell<usize> = const { Cell::new(0) };
  static LAYOUT_CACHE_HITS: Cell<usize> = const { Cell::new(0) };
  static LAYOUT_CACHE_STORES: Cell<usize> = const { Cell::new(0) };
  static LAYOUT_CACHE_EVICTIONS: Cell<usize> = const { Cell::new(0) };
}

fn pack_viewport_size(size: Size) -> u64 {
  let w = size.width.to_bits() as u64;
  let h = size.height.to_bits() as u64;
  (w << 32) | h
}

fn hash_enum_discriminant<T>(value: &T, hasher: &mut DefaultHasher) {
  mem::discriminant(value).hash(hasher);
}

fn hash_length(len: &Length, hasher: &mut DefaultHasher) {
  hash_enum_discriminant(&len.unit, hasher);
  len.value.to_bits().hash(hasher);
  if let Some(calc) = &len.calc {
    1u8.hash(hasher);
    for term in calc.terms() {
      hash_enum_discriminant(&term.unit, hasher);
      term.value.to_bits().hash(hasher);
    }
  } else {
    0u8.hash(hasher);
  }
}

fn hash_option_length(len: &Option<Length>, hasher: &mut DefaultHasher) {
  match len {
    Some(l) => {
      1u8.hash(hasher);
      hash_length(l, hasher);
    }
    None => 0u8.hash(hasher),
  }
}

fn hash_string_vecs(vecs: &[Vec<String>], hasher: &mut DefaultHasher) {
  vecs.len().hash(hasher);
  for names in vecs {
    names.len().hash(hasher);
    for name in names {
      name.hash(hasher);
    }
  }
}

fn hash_grid_track(track: &GridTrack, hasher: &mut DefaultHasher) {
  use GridTrack::*;
  match track {
    Length(len) => {
      0u8.hash(hasher);
      hash_length(len, hasher);
    }
    Fr(fr) => {
      1u8.hash(hasher);
      fr.to_bits().hash(hasher);
    }
    Auto => 2u8.hash(hasher),
    MinContent => 3u8.hash(hasher),
    MaxContent => 4u8.hash(hasher),
    FitContent(len) => {
      5u8.hash(hasher);
      hash_length(len, hasher);
    }
    MinMax(min, max) => {
      6u8.hash(hasher);
      hash_grid_track(min, hasher);
      hash_grid_track(max, hasher);
    }
    RepeatAutoFill { tracks, line_names } => {
      7u8.hash(hasher);
      hash_grid_tracks(tracks, hasher);
      hash_string_vecs(line_names, hasher);
    }
    RepeatAutoFit { tracks, line_names } => {
      8u8.hash(hasher);
      hash_grid_tracks(tracks, hasher);
      hash_string_vecs(line_names, hasher);
    }
  }
}

fn hash_grid_tracks(tracks: &[GridTrack], hasher: &mut DefaultHasher) {
  tracks.len().hash(hasher);
  for track in tracks {
    hash_grid_track(track, hasher);
  }
}

fn hash_named_lines_map(map: &HashMap<String, Vec<usize>>, hasher: &mut DefaultHasher) {
  let mut entries: Vec<_> = map.iter().collect();
  entries.sort_by(|a, b| a.0.cmp(b.0));
  entries.len().hash(hasher);
  for (name, positions) in entries {
    name.hash(hasher);
    positions.hash(hasher);
  }
}

fn hash_template_areas(areas: &[Vec<Option<String>>], hasher: &mut DefaultHasher) {
  areas.len().hash(hasher);
  for row in areas {
    row.len().hash(hasher);
    for cell in row {
      match cell {
        Some(name) => {
          1u8.hash(hasher);
          name.hash(hasher);
        }
        None => 0u8.hash(hasher),
      }
    }
  }
}

#[cfg(test)]
thread_local! {
  static SUBGRID_WALK_COUNT: Cell<usize> = const { Cell::new(0) };
}

#[cfg(test)]
fn increment_subgrid_walk_count() {
  SUBGRID_WALK_COUNT.with(|counter| counter.set(counter.get() + 1));
}

#[cfg(test)]
fn reset_subgrid_walk_count() {
  SUBGRID_WALK_COUNT.with(|counter| counter.set(0));
}

#[cfg(test)]
fn subgrid_walk_count() -> usize {
  SUBGRID_WALK_COUNT.with(|counter| counter.get())
}

fn subtree_contains_subgrid_uncached(node: &BoxNode, epoch: usize) -> bool {
  #[cfg(test)]
  increment_subgrid_walk_count();

  if node.style.grid_row_subgrid
    || node.style.grid_column_subgrid
    || !node.style.subgrid_row_line_names.is_empty()
    || !node.style.subgrid_column_line_names.is_empty()
  {
    return true;
  }
  node
    .children
    .iter()
    .any(|child| subtree_contains_subgrid(child, epoch))
}

fn subtree_contains_subgrid(node: &BoxNode, epoch: usize) -> bool {
  let id = node.id();
  if id == 0 {
    return subtree_contains_subgrid_uncached(node, epoch);
  }

  if let Some(cached) = SUBGRID_DEPENDENT_CACHE.with(|cache| {
    cache
      .borrow()
      .get(&id)
      .and_then(|(cached_epoch, contains)| (*cached_epoch == epoch).then_some(*contains))
  }) {
    return cached;
  }

  let contains = subtree_contains_subgrid_uncached(node, epoch);
  SUBGRID_DEPENDENT_CACHE.with(|cache| {
    cache.borrow_mut().insert(id, (epoch, contains));
  });
  contains
}

fn layout_constraints_hash(constraints: &LayoutConstraints) -> u64 {
  let mut h = DefaultHasher::new();
  let hash_space = |space: &crate::layout::constraints::AvailableSpace,
                    hasher: &mut DefaultHasher| match space {
    crate::layout::constraints::AvailableSpace::Definite(v) => {
      0u8.hash(hasher);
      v.to_bits().hash(hasher);
    }
    crate::layout::constraints::AvailableSpace::Indefinite => 1u8.hash(hasher),
    crate::layout::constraints::AvailableSpace::MinContent => 2u8.hash(hasher),
    crate::layout::constraints::AvailableSpace::MaxContent => 3u8.hash(hasher),
  };
  hash_space(&constraints.available_width, &mut h);
  hash_space(&constraints.available_height, &mut h);
  match constraints.inline_percentage_base {
    Some(v) => {
      1u8.hash(&mut h);
      v.to_bits().hash(&mut h);
    }
    None => 0u8.hash(&mut h),
  }
  h.finish()
}

/// Computes a conservative fingerprint of layout-affecting style properties.
pub(crate) fn layout_style_fingerprint(style: &Arc<ComputedStyle>) -> u64 {
  let mut h = DefaultHasher::new();
  (Arc::as_ptr(style) as usize).hash(&mut h);
  hash_enum_discriminant(&style.display, &mut h);
  hash_enum_discriminant(&style.position, &mut h);
  hash_enum_discriminant(&style.float, &mut h);
  hash_enum_discriminant(&style.clear, &mut h);
  hash_enum_discriminant(&style.box_sizing, &mut h);
  hash_option_length(&style.width, &mut h);
  hash_option_length(&style.height, &mut h);
  hash_option_length(&style.min_width, &mut h);
  hash_option_length(&style.max_width, &mut h);
  hash_option_length(&style.min_height, &mut h);
  hash_option_length(&style.max_height, &mut h);
  hash_option_length(&style.margin_top, &mut h);
  hash_option_length(&style.margin_right, &mut h);
  hash_option_length(&style.margin_bottom, &mut h);
  hash_option_length(&style.margin_left, &mut h);
  hash_length(&style.padding_top, &mut h);
  hash_length(&style.padding_right, &mut h);
  hash_length(&style.padding_bottom, &mut h);
  hash_length(&style.padding_left, &mut h);
  hash_length(&style.border_top_width, &mut h);
  hash_length(&style.border_right_width, &mut h);
  hash_length(&style.border_bottom_width, &mut h);
  hash_length(&style.border_left_width, &mut h);
  hash_enum_discriminant(&style.overflow_x, &mut h);
  hash_enum_discriminant(&style.overflow_y, &mut h);
  hash_enum_discriminant(&style.scrollbar_width, &mut h);
  hash_enum_discriminant(&style.aspect_ratio, &mut h);
  hash_enum_discriminant(&style.writing_mode, &mut h);
  hash_enum_discriminant(&style.direction, &mut h);
  hash_enum_discriminant(&style.line_height, &mut h);
  style.font_size.to_bits().hash(&mut h);
  style.root_font_size.to_bits().hash(&mut h);
  style.letter_spacing.to_bits().hash(&mut h);
  style.word_spacing.to_bits().hash(&mut h);
  hash_option_length(&style.column_width, &mut h);
  hash_enum_discriminant(&style.column_count, &mut h);
  hash_length(&style.column_gap, &mut h);
  hash_enum_discriminant(&style.grid_auto_flow, &mut h);
  hash_grid_tracks(&style.grid_template_columns, &mut h);
  hash_grid_tracks(&style.grid_template_rows, &mut h);
  hash_grid_tracks(&style.grid_auto_columns, &mut h);
  hash_grid_tracks(&style.grid_auto_rows, &mut h);
  hash_length(&style.grid_gap, &mut h);
  hash_length(&style.grid_row_gap, &mut h);
  hash_length(&style.grid_column_gap, &mut h);
  style.grid_row_subgrid.hash(&mut h);
  style.grid_column_subgrid.hash(&mut h);
  hash_string_vecs(&style.subgrid_row_line_names, &mut h);
  hash_string_vecs(&style.subgrid_column_line_names, &mut h);
  hash_string_vecs(&style.grid_row_line_names, &mut h);
  hash_string_vecs(&style.grid_column_line_names, &mut h);
  hash_named_lines_map(&style.grid_row_names, &mut h);
  hash_named_lines_map(&style.grid_column_names, &mut h);
  hash_template_areas(&style.grid_template_areas, &mut h);
  style.grid_column_start.hash(&mut h);
  style.grid_column_end.hash(&mut h);
  style.grid_row_start.hash(&mut h);
  style.grid_row_end.hash(&mut h);
  match &style.grid_column_raw {
    Some(raw) => {
      1u8.hash(&mut h);
      raw.hash(&mut h);
    }
    None => 0u8.hash(&mut h),
  }
  match &style.grid_row_raw {
    Some(raw) => {
      1u8.hash(&mut h);
      raw.hash(&mut h);
    }
    None => 0u8.hash(&mut h),
  }
  style.containment.size.hash(&mut h);
  style.containment.inline_size.hash(&mut h);
  style.containment.layout.hash(&mut h);
  style.containment.style.hash(&mut h);
  style.containment.paint.hash(&mut h);
  for family in &style.font_family {
    family.hash(&mut h);
  }
  h.finish()
}

fn fragmentation_fingerprint(options: Option<FragmentationOptions>) -> u64 {
  options
    .map(|opts| {
      let mut h = DefaultHasher::new();
      opts.fragmentainer_size.to_bits().hash(&mut h);
      opts.fragmentainer_gap.to_bits().hash(&mut h);
      opts.column_count.hash(&mut h);
      opts.column_gap.to_bits().hash(&mut h);
      h.finish()
    })
    .unwrap_or(0)
}

fn is_layout_cacheable(box_node: &BoxNode, fc_type: FormattingContextType) -> bool {
  if !LAYOUT_CACHE_ENABLED.with(|enabled| enabled.get()) {
    return false;
  }
  if box_node.id() == 0 || box_node.is_anonymous() {
    return false;
  }
  if matches!(
    box_node.style.position,
    Position::Absolute | Position::Fixed | Position::Sticky
  ) {
    return false;
  }
  if box_node.style.float != Float::None {
    return false;
  }

  match fc_type {
    FormattingContextType::Inline => false,
    FormattingContextType::Block => {
      crate::layout::contexts::block::margin_collapse::establishes_bfc(&box_node.style)
    }
    FormattingContextType::Flex | FormattingContextType::Grid | FormattingContextType::Table => {
      true
    }
  }
}

fn layout_cache_key(
  box_node: &BoxNode,
  fc_type: FormattingContextType,
  constraints: &LayoutConstraints,
  viewport: Size,
  epoch: usize,
) -> Option<LayoutCacheKey> {
  if !is_layout_cacheable(box_node, fc_type) {
    return None;
  }

  let style_hash = layout_style_fingerprint(&box_node.style);
  let constraints_hash = layout_constraints_hash(constraints);
  let fragmentation_hash = LAYOUT_CACHE_FRAGMENTATION.with(|hash| hash.get());
  Some(LayoutCacheKey {
    box_id: box_node.id(),
    fc_type,
    style_hash,
    constraints_hash,
    fragmentation_hash,
    viewport_hash: pack_viewport_size(viewport),
    subgrid_dependent: subtree_contains_subgrid(box_node, epoch),
  })
}

fn layout_cache_clear() {
  LAYOUT_RESULT_CACHE.with(|cache| {
    let mut map = cache.borrow_mut();
    let evicted = map.len();
    if evicted > 0 {
      LAYOUT_CACHE_EVICTIONS.with(|counter| counter.set(counter.get() + evicted));
    }
    map.clear();
  });
}

fn evict_cache_entries_for_box(box_id: usize) {
  LAYOUT_RESULT_CACHE.with(|cache| {
    let mut map = cache.borrow_mut();
    let before = map.len();
    map.retain(|existing_key, _| existing_key.box_id != box_id);
    let evicted = before.saturating_sub(map.len());
    if evicted > 0 {
      LAYOUT_CACHE_EVICTIONS.with(|counter| counter.set(counter.get() + evicted));
    }
  });
}

pub(crate) fn layout_cache_reset_counters() {
  LAYOUT_RESULT_CACHE.with(|cache| {
    cache.borrow_mut().clear();
  });
  LAYOUT_CACHE_LOOKUPS.with(|counter| counter.set(0));
  LAYOUT_CACHE_HITS.with(|counter| counter.set(0));
  LAYOUT_CACHE_STORES.with(|counter| counter.set(0));
  LAYOUT_CACHE_EVICTIONS.with(|counter| counter.set(0));
  let epoch = LAYOUT_CACHE_EPOCH.with(|cell| cell.get());
  subgrid_cache_use_epoch(epoch, true);
}

/// Configures the layout cache epoch and run-scoped parameters.
pub(crate) fn layout_cache_use_epoch(
  epoch: usize,
  enabled: bool,
  reset_counters: bool,
  fragmentation: Option<FragmentationOptions>,
) {
  let epoch = epoch.max(1);
  let previous = LAYOUT_CACHE_EPOCH.with(|cell| {
    let previous = cell.get();
    cell.set(epoch);
    previous
  });
  LAYOUT_CACHE_ENABLED.with(|cell| cell.set(enabled));
  LAYOUT_CACHE_FRAGMENTATION.with(|cell| cell.set(fragmentation_fingerprint(fragmentation)));

  let should_clear = reset_counters || previous != epoch || !enabled;
  if should_clear {
    layout_cache_reset_counters();
  }
  if should_clear {
    layout_cache_clear();
  } else {
    subgrid_cache_use_epoch(epoch, false);
  }
}

/// Attempts to retrieve a cached layout result for the given formatting context.
pub(crate) fn layout_cache_lookup(
  box_node: &BoxNode,
  fc_type: FormattingContextType,
  constraints: &LayoutConstraints,
  viewport: Size,
) -> Option<FragmentNode> {
  let epoch = LAYOUT_CACHE_EPOCH.with(|cell| cell.get());
  let key = layout_cache_key(box_node, fc_type, constraints, viewport, epoch)?;
  if key.subgrid_dependent {
    evict_cache_entries_for_box(key.box_id);
    return None;
  }
  LAYOUT_CACHE_LOOKUPS.with(|counter| counter.set(counter.get() + 1));

  let result = LAYOUT_RESULT_CACHE.with(|cache| {
    let mut map = cache.borrow_mut();
    if let Some(entry) = map.get(&key) {
      if entry.epoch == epoch && entry.style_hash == key.style_hash {
        return Some(entry.fragment.clone());
      }
      // Drop stale entries tied to an old epoch.
      if entry.epoch != epoch {
        map.remove(&key);
        LAYOUT_CACHE_EVICTIONS.with(|counter| counter.set(counter.get() + 1));
      }
    }
    None
  });

  if let Some(fragment) = result {
    LAYOUT_CACHE_HITS.with(|counter| counter.set(counter.get() + 1));
    return Some((*fragment).clone());
  }
  None
}

/// Stores a layout result in the cache for reuse within the current epoch.
pub(crate) fn layout_cache_store(
  box_node: &BoxNode,
  fc_type: FormattingContextType,
  constraints: &LayoutConstraints,
  fragment: &FragmentNode,
  viewport: Size,
) {
  let epoch = LAYOUT_CACHE_EPOCH.with(|cell| cell.get());
  let key = match layout_cache_key(box_node, fc_type, constraints, viewport, epoch) {
    Some(k) => k,
    None => return,
  };
  if key.subgrid_dependent {
    evict_cache_entries_for_box(key.box_id);
    return;
  }

  LAYOUT_RESULT_CACHE.with(|cache| {
    let mut map = cache.borrow_mut();
    // Drop entries for the same box/style from previous epochs to avoid stale reuse when the
    // caller opts into incremental layouts without bumping the epoch.
    map.retain(|existing_key, entry| {
      if entry.epoch != epoch {
        LAYOUT_CACHE_EVICTIONS.with(|counter| counter.set(counter.get() + 1));
        return false;
      }
      if existing_key.box_id == key.box_id && existing_key.style_hash != key.style_hash {
        LAYOUT_CACHE_EVICTIONS.with(|counter| counter.set(counter.get() + 1));
        return false;
      }
      true
    });

    map.insert(
      key,
      LayoutCacheEntry {
        epoch,
        fragment: Arc::new(fragment.clone()),
        style_hash: key.style_hash,
      },
    );
  });

  LAYOUT_CACHE_STORES.with(|counter| counter.set(counter.get() + 1));
}

pub(crate) fn layout_cache_stats() -> (usize, usize, usize, usize) {
  (
    LAYOUT_CACHE_LOOKUPS.with(|counter| counter.get()),
    LAYOUT_CACHE_HITS.with(|counter| counter.get()),
    LAYOUT_CACHE_STORES.with(|counter| counter.get()),
    LAYOUT_CACHE_EVICTIONS.with(|counter| counter.get()),
  )
}

pub(crate) struct FragmentainerBlockSizeHintGuard {
  previous: Option<f32>,
}

impl Drop for FragmentainerBlockSizeHintGuard {
  fn drop(&mut self) {
    FRAGMENTAINER_BLOCK_SIZE_HINT.with(|hint| {
      hint.set(self.previous);
    });
  }
}

pub(crate) fn fragmentainer_block_size_hint() -> Option<f32> {
  FRAGMENTAINER_BLOCK_SIZE_HINT.with(|hint| hint.get())
}

pub(crate) fn set_fragmentainer_block_size_hint(
  hint: Option<f32>,
) -> FragmentainerBlockSizeHintGuard {
  let previous = FRAGMENTAINER_BLOCK_SIZE_HINT.with(|cell| {
    let previous = cell.get();
    cell.set(hint);
    previous
  });
  FragmentainerBlockSizeHintGuard { previous }
}

/// Common trait for all formatting contexts
///
/// This trait abstracts the layout algorithm. Every formatting context
/// (block, inline, flex, grid, table) implements this trait to provide
/// a uniform interface for the layout engine.
///
/// # Contract
///
/// Implementers must:
/// 1. Take a BoxNode and LayoutConstraints as input
/// 2. Recursively layout children using appropriate child formatting contexts
/// 3. Return a positioned FragmentNode tree with final sizes and positions
/// 4. Handle all AvailableSpace variants (Definite, Indefinite, MinContent, MaxContent)
/// 5. Support intrinsic size queries (min-content, max-content)
/// 6. Be stateless and reusable (no mutable state in the FC struct)
///
/// # Thread Safety
///
/// Formatting contexts must be `Send + Sync` to allow:
/// - Sharing across threads
/// - Use in trait objects (`Box<dyn FormattingContext>`)
/// - Parallel layout operations in the future
///
/// # Example Implementation
///
/// ```rust,ignore
/// use fastrender::layout::{FormattingContext, LayoutConstraints, IntrinsicSizingMode, LayoutError};
/// use fastrender::tree::{BoxNode, FragmentNode};
///
/// struct BlockFormattingContext;
///
/// impl FormattingContext for BlockFormattingContext {
///     fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints)
///         -> Result<FragmentNode, LayoutError>
///     {
///         // 1. Compute box's own size based on constraints
///         // 2. Layout children vertically
///         // 3. Handle margin collapsing
///         // 4. Return positioned fragment
///         todo!("Implementation in W3.T04")
///     }
///
///     fn compute_intrinsic_inline_size(
///         &self,
///         box_node: &BoxNode,
///         mode: IntrinsicSizingMode,
///     ) -> Result<f32, LayoutError> {
///         // Measure content width
///         todo!("Implementation in W3.T04")
///     }
/// }
/// ```
///
/// # See Also
///
/// - W3.T04: BlockFormattingContext implementation
/// - W4.T12: InlineFormattingContext implementation
/// - W2.T10: LayoutEngine (dispatches to FCs)
pub trait FormattingContext: Send + Sync {
  /// Lays out a box within this formatting context
  ///
  /// This is the main entry point for layout. It takes a box tree node and
  /// available space, performs layout according to this FC's rules, and
  /// returns a positioned fragment tree.
  ///
  /// # Arguments
  ///
  /// * `box_node` - The box to layout (must be compatible with this FC)
  /// * `constraints` - Available space constraints
  ///
  /// # Returns
  ///
  /// A positioned fragment tree with:
  /// - Final size (width and height)
  /// - Final position (relative to parent's content box)
  /// - Laid out children (if any)
  ///
  /// # Errors
  ///
  /// Returns error if:
  /// - Box type is not supported by this formatting context
  /// - Circular dependency detected in sizing
  /// - Required context (fonts, images, etc.) is missing
  fn layout(
    &self,
    box_node: &BoxNode,
    constraints: &LayoutConstraints,
  ) -> Result<FragmentNode, LayoutError>;

  /// Computes intrinsic size for a box in the inline axis
  ///
  /// Used when parent needs to know content-based size before layout.
  /// This is critical for:
  /// - Auto-sizing: `width: auto` on shrink-to-fit boxes
  /// - Flexbox: `flex-basis: content`
  /// - Grid: `grid-template-columns: min-content`
  /// - Table: Auto column width calculation
  ///
  /// # Arguments
  ///
  /// * `box_node` - The box to measure
  /// * `mode` - Whether to compute min-content or max-content size
  ///
  /// # Returns
  ///
  /// Intrinsic inline size in CSS pixels. The inline axis is:
  /// - Horizontal in horizontal writing modes (most common)
  /// - Vertical in vertical writing modes
  ///
  /// # Errors
  ///
  /// Returns error if:
  /// - Content cannot be measured (missing fonts, images, etc.)
  /// - Box type not supported
  ///
  /// # Performance Note
  ///
  /// This method may be called multiple times during layout (e.g., for table
  /// column sizing). Implementations should consider caching if expensive.
  fn compute_intrinsic_inline_size(
    &self,
    box_node: &BoxNode,
    mode: IntrinsicSizingMode,
  ) -> Result<f32, LayoutError>;

  /// Computes intrinsic size for a box in the block axis
  ///
  /// Used for shrink-to-fit resolution of absolutely positioned height when
  /// both vertical insets are specified, mirroring the inline shrink path.
  fn compute_intrinsic_block_size(
    &self,
    box_node: &BoxNode,
    mode: IntrinsicSizingMode,
  ) -> Result<f32, LayoutError> {
    // Default to inline size for contexts that have symmetric handling.
    self.compute_intrinsic_inline_size(box_node, mode)
  }
}

/// Layout errors
///
/// Errors that can occur during layout operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LayoutError {
  /// Box type not supported by this formatting context
  ///
  /// For example, trying to layout a table box with BlockFormattingContext.
  UnsupportedBoxType(String),

  /// Circular dependency in sizing
  ///
  /// Occurs when box's size depends on itself, creating infinite recursion.
  /// Example: percentage height when parent height depends on child height.
  CircularDependency,

  /// Missing required context
  ///
  /// Required resources (fonts, images, etc.) are not available.
  MissingContext(String),

  /// Layout aborted due to timeout or cancellation.
  Timeout { elapsed: Duration },
}

impl std::fmt::Display for LayoutError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::UnsupportedBoxType(msg) => write!(f, "Unsupported box type: {}", msg),
      Self::CircularDependency => write!(f, "Circular dependency in layout"),
      Self::MissingContext(msg) => write!(f, "Missing context: {}", msg),
      Self::Timeout { elapsed } => write!(f, "Layout timed out after {elapsed:?}"),
    }
  }
}

impl std::error::Error for LayoutError {}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::Rect;
  use crate::style::display::FormattingContextType;
  use crate::style::ComputedStyle;
  use std::sync::atomic::Ordering;
  use std::sync::Arc;

  /// Stub formatting context for testing trait requirements
  ///
  /// This demonstrates the minimal implementation needed to satisfy
  /// the FormattingContext trait contract.
  #[derive(Debug)]
  struct StubFormattingContext;

  impl FormattingContext for StubFormattingContext {
    fn layout(
      &self,
      _box_node: &BoxNode,
      _constraints: &LayoutConstraints,
    ) -> Result<FragmentNode, LayoutError> {
      // Stub: just return a fixed-size fragment
      Ok(FragmentNode::new_block(
        Rect::from_xywh(0.0, 0.0, 100.0, 50.0),
        vec![],
      ))
    }

    fn compute_intrinsic_inline_size(
      &self,
      _box_node: &BoxNode,
      _mode: IntrinsicSizingMode,
    ) -> Result<f32, LayoutError> {
      // Stub: return fixed width
      Ok(100.0)
    }
  }

  #[test]
  fn test_stub_fc_implements_trait() {
    let fc = StubFormattingContext;
    let style = Arc::new(ComputedStyle::default());
    let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let fragment = fc.layout(&box_node, &constraints).unwrap();

    assert_eq!(fragment.bounds.width(), 100.0);
    assert_eq!(fragment.bounds.height(), 50.0);
  }

  #[test]
  fn test_intrinsic_sizing_min_content() {
    let fc = StubFormattingContext;
    let style = Arc::new(ComputedStyle::default());
    let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

    let size = fc
      .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MinContent)
      .unwrap();

    assert_eq!(size, 100.0);
  }

  #[test]
  fn test_intrinsic_sizing_max_content() {
    let fc = StubFormattingContext;
    let style = Arc::new(ComputedStyle::default());
    let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

    let size = fc
      .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MaxContent)
      .unwrap();

    assert_eq!(size, 100.0);
  }

  #[test]
  fn test_layout_with_definite_constraints() {
    let fc = StubFormattingContext;
    let style = Arc::new(ComputedStyle::default());
    let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

    let constraints = LayoutConstraints::definite(1024.0, 768.0);
    let fragment = fc.layout(&box_node, &constraints).unwrap();

    // Fragment should be positioned at origin
    assert_eq!(fragment.bounds.x(), 0.0);
    assert_eq!(fragment.bounds.y(), 0.0);
  }

  #[test]
  fn test_layout_with_indefinite_constraints() {
    let fc = StubFormattingContext;
    let style = Arc::new(ComputedStyle::default());
    let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

    let constraints = LayoutConstraints::indefinite();
    let fragment = fc.layout(&box_node, &constraints).unwrap();

    // Should still produce valid fragment
    assert!(fragment.bounds.width() > 0.0);
  }

  #[test]
  fn test_layout_error_display() {
    let err = LayoutError::UnsupportedBoxType("test".to_string());
    assert_eq!(err.to_string(), "Unsupported box type: test");

    let err = LayoutError::CircularDependency;
    assert_eq!(err.to_string(), "Circular dependency in layout");

    let err = LayoutError::MissingContext("fonts".to_string());
    assert_eq!(err.to_string(), "Missing context: fonts");
  }

  #[test]
  fn test_intrinsic_sizing_mode_equality() {
    assert_eq!(
      IntrinsicSizingMode::MinContent,
      IntrinsicSizingMode::MinContent
    );
    assert_eq!(
      IntrinsicSizingMode::MaxContent,
      IntrinsicSizingMode::MaxContent
    );
    assert_ne!(
      IntrinsicSizingMode::MinContent,
      IntrinsicSizingMode::MaxContent
    );
  }

  #[test]
  fn test_formatting_context_is_send_sync() {
    // Verify that our trait requires Send + Sync
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<StubFormattingContext>();
  }

  #[test]
  fn intrinsic_cache_epoch_invalidation() {
    intrinsic_cache_use_epoch(1, true);

    let mut node = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![],
    );
    node.id = 1;

    intrinsic_cache_store(&node, IntrinsicSizingMode::MinContent, 5.0);
    assert_eq!(
      intrinsic_cache_lookup(&node, IntrinsicSizingMode::MinContent),
      Some(5.0)
    );

    intrinsic_cache_use_epoch(2, false);
    assert_eq!(
      intrinsic_cache_lookup(&node, IntrinsicSizingMode::MinContent),
      None
    );

    intrinsic_cache_use_epoch(1, true);
  }

  #[test]
  fn subgrid_memoization_matches_recursive_and_reuses() {
    intrinsic_cache_use_epoch(1, true);
    reset_subgrid_walk_count();

    let mut subgrid_style = ComputedStyle::default();
    subgrid_style.grid_row_subgrid = true;

    let mut leaf = BoxNode::new_block(
      Arc::new(subgrid_style),
      FormattingContextType::Block,
      vec![],
    );
    leaf.id = 3;

    let mut middle = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![leaf],
    );
    middle.id = 2;

    let mut root = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![middle],
    );
    root.id = 1;

    let epoch = CACHE_EPOCH.load(Ordering::Relaxed);
    let memoized = subtree_contains_subgrid(&root, epoch);
    let raw = {
      fn raw_scan(node: &BoxNode) -> bool {
        if node.style.grid_row_subgrid
          || node.style.grid_column_subgrid
          || !node.style.subgrid_row_line_names.is_empty()
          || !node.style.subgrid_column_line_names.is_empty()
        {
          return true;
        }
        node.children.iter().any(raw_scan)
      }
      raw_scan(&root)
    };
    assert_eq!(memoized, raw);
    assert_eq!(subgrid_walk_count(), 3);

    reset_subgrid_walk_count();
    assert_eq!(subtree_contains_subgrid(&root, epoch), memoized);
    assert_eq!(subgrid_walk_count(), 0);
  }

  #[test]
  fn subgrid_memoization_invalidation_on_epoch_change() {
    intrinsic_cache_use_epoch(7, true);

    let child = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![],
    );
    let mut root = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![child],
    );
    root.id = 1;
    root.children[0].id = 2;

    let epoch = CACHE_EPOCH.load(Ordering::Relaxed);
    reset_subgrid_walk_count();
    assert!(!subtree_contains_subgrid(&root, epoch));
    assert_eq!(subgrid_walk_count(), 2);

    let mut new_style = ComputedStyle::default();
    new_style.grid_column_subgrid = true;
    root.children[0].style = Arc::new(new_style);

    reset_subgrid_walk_count();
    assert!(!subtree_contains_subgrid(&root, epoch));
    assert_eq!(subgrid_walk_count(), 0);

    intrinsic_cache_use_epoch(epoch + 1, false);
    let new_epoch = CACHE_EPOCH.load(Ordering::Relaxed);
    reset_subgrid_walk_count();
    assert!(subtree_contains_subgrid(&root, new_epoch));
    assert!(subgrid_walk_count() > 0);

    intrinsic_cache_use_epoch(1, true);
  }

  #[test]
  fn layout_epoch_clears_subgrid_cache() {
    layout_cache_use_epoch(9, false, true, None);

    let child = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![],
    );
    let mut root = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![child],
    );
    root.id = 10;
    root.children[0].id = 11;

    let epoch = LAYOUT_CACHE_EPOCH.with(|cell| cell.get());
    reset_subgrid_walk_count();
    assert!(!subtree_contains_subgrid(&root, epoch));
    assert_eq!(subgrid_walk_count(), 2);

    let mut subgrid_style = ComputedStyle::default();
    subgrid_style.grid_row_subgrid = true;
    root.children[0].style = Arc::new(subgrid_style);

    reset_subgrid_walk_count();
    assert!(!subtree_contains_subgrid(&root, epoch));
    assert_eq!(subgrid_walk_count(), 0);

    layout_cache_use_epoch(epoch + 1, false, true, None);
    let new_epoch = LAYOUT_CACHE_EPOCH.with(|cell| cell.get());
    reset_subgrid_walk_count();
    assert!(subtree_contains_subgrid(&root, new_epoch));
    assert!(subgrid_walk_count() > 0);

    layout_cache_use_epoch(1, false, true, None);
  }
}
use std::time::Duration;
