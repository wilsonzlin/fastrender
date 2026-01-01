//! A cache for storing the results of layout computation
use crate::geometry::Size;
use crate::style::AvailableSpace;
use crate::tree::{LayoutOutput, RunMode};

/// The number of cache entries for each node in the tree
///
/// Taffy’s grid/flex algorithms frequently query children under multiple sizing modes within a
/// single layout pass (e.g. MaxContent and Definite track sizing). FastRender’s integration uses
/// measure functions that can be expensive (nested layout), so it's important that these distinct
/// queries do not clobber each other in the cache.
///
/// The upstream cache historically treated `Definite` as equivalent to `MaxContent` when selecting
/// a cache slot, under the assumption that a node would typically be sized under one or the other
/// but not both. Real-world pages violate this assumption, which leads to avoidable re-measures.
/// We keep separate slots for MaxContent vs Definite to reduce measure-call churn.
const CACHE_SIZE: usize = 16;

/// Cached intermediate layout results
#[derive(Debug, Clone, Copy, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub(crate) struct CacheEntry<T> {
  /// The initial cached size of the node itself
  known_dimensions: Size<Option<f32>>,
  /// The initial cached size of the parent's node
  available_space: Size<AvailableSpace>,
  /// The cached size and baselines of the item
  content: T,
}

/// A cache for caching the results of a sizing a Grid Item or Flexbox Item
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Cache {
  /// The cache entry for the node's final layout
  final_layout_entry: Option<CacheEntry<LayoutOutput>>,
  /// The cache entries for the node's preliminary size measurements
  measure_entries: [Option<CacheEntry<Size<f32>>>; CACHE_SIZE],
  /// Tracks if all cache entries are empty
  is_empty: bool,
}

impl Default for Cache {
  fn default() -> Self {
    Self::new()
  }
}

impl Cache {
  /// Create a new empty cache
  pub const fn new() -> Self {
    Self {
      final_layout_entry: None,
      measure_entries: [None; CACHE_SIZE],
      is_empty: true,
    }
  }

  /// Return the cache slot to cache the current computed result in
  ///
  /// ## Caching Strategy
  ///
  /// We need multiple cache slots, because a node's size is often queried by it's parent multiple times in the course of the layout
  /// process, and we don't want later results to clobber earlier ones.
  ///
  /// The two variables that we care about when determining cache slot are:
  ///
  ///   - How many "known_dimensions" are set. In the worst case, a node may be called first with neither dimension known, then with one
  ///     dimension known (either width of height - which doesn't matter for our purposes here), and then with both dimensions known.
  ///   - Whether unknown dimensions are being sized under a min-content, max-content, or definite
  ///     available space constraint.
  ///
  /// ## Cache slots:
  ///
  /// - Slot 0: Both known_dimensions were set
  /// - Slots 1-3: width but not height known_dimension was set and:
  ///   - Slot 1: y-axis available space is MaxContent
  ///   - Slot 2: y-axis available space is Definite
  ///   - Slot 3: y-axis available space is MinContent
  /// - Slots 4-6: height but not width known_dimension was set and:
  ///   - Slot 4: x-axis available space is MaxContent
  ///   - Slot 5: x-axis available space is Definite
  ///   - Slot 6: x-axis available space is MinContent
  /// - Slots 7-15: Neither known_dimensions were set; slots are assigned by available space
  ///   category in each axis (MaxContent, Definite, MinContent).
  #[inline]
  fn compute_cache_slot(
    known_dimensions: Size<Option<f32>>,
    available_space: Size<AvailableSpace>,
  ) -> usize {
    let has_known_width = known_dimensions.width.is_some();
    let has_known_height = known_dimensions.height.is_some();

    let space_slot = |space: AvailableSpace| -> usize {
      match space {
        AvailableSpace::MaxContent => 0,
        AvailableSpace::Definite(_) => 1,
        AvailableSpace::MinContent => 2,
      }
    };

    // Slot 0: Both known_dimensions were set
    if has_known_width && has_known_height {
      return 0;
    }

    if has_known_width && !has_known_height {
      // Slots 1-3
      return 1 + space_slot(available_space.height);
    }

    if has_known_height && !has_known_width {
      // Slots 4-6
      return 4 + space_slot(available_space.width);
    }

    // Slots 7-15: neither known_dimensions were set. Use the available-space categories to
    // select a slot in a 3x3 grid.
    let width_slot = space_slot(available_space.width);
    let height_slot = space_slot(available_space.height);
    7 + (width_slot * 3) + height_slot
  }

  /// Try to retrieve a cached result from the cache
  #[inline]
  pub fn get(
    &self,
    known_dimensions: Size<Option<f32>>,
    available_space: Size<AvailableSpace>,
    run_mode: RunMode,
  ) -> Option<LayoutOutput> {
    match run_mode {
      RunMode::PerformLayout => self
        .final_layout_entry
        .filter(|entry| {
          let cached_size = entry.content.size;
          (known_dimensions.width == entry.known_dimensions.width
            || known_dimensions.width == Some(cached_size.width))
            && (known_dimensions.height == entry.known_dimensions.height
              || known_dimensions.height == Some(cached_size.height))
            && (known_dimensions.width.is_some()
              || entry
                .available_space
                .width
                .is_roughly_equal(available_space.width))
            && (known_dimensions.height.is_some()
              || entry
                .available_space
                .height
                .is_roughly_equal(available_space.height))
        })
        .map(|e| e.content),
      RunMode::ComputeSize => {
        for entry in self.measure_entries.iter().flatten() {
          let cached_size = entry.content;

          if (known_dimensions.width == entry.known_dimensions.width
            || known_dimensions.width == Some(cached_size.width))
            && (known_dimensions.height == entry.known_dimensions.height
              || known_dimensions.height == Some(cached_size.height))
            && (known_dimensions.width.is_some()
              || entry
                .available_space
                .width
                .is_roughly_equal(available_space.width))
            && (known_dimensions.height.is_some()
              || entry
                .available_space
                .height
                .is_roughly_equal(available_space.height))
          {
            return Some(LayoutOutput::from_outer_size(cached_size));
          }
        }

        None
      }
      RunMode::PerformHiddenLayout => None,
    }
  }

  /// Store a computed size in the cache
  pub fn store(
    &mut self,
    known_dimensions: Size<Option<f32>>,
    available_space: Size<AvailableSpace>,
    run_mode: RunMode,
    layout_output: LayoutOutput,
  ) {
    match run_mode {
      RunMode::PerformLayout => {
        self.is_empty = false;
        self.final_layout_entry = Some(CacheEntry {
          known_dimensions,
          available_space,
          content: layout_output,
        })
      }
      RunMode::ComputeSize => {
        self.is_empty = false;
        let cache_slot = Self::compute_cache_slot(known_dimensions, available_space);
        self.measure_entries[cache_slot] = Some(CacheEntry {
          known_dimensions,
          available_space,
          content: layout_output.size,
        });
      }
      RunMode::PerformHiddenLayout => {}
    }
  }

  /// Clear all cache entries and reports clear operation outcome ([`ClearState`])
  pub fn clear(&mut self) -> ClearState {
    if self.is_empty {
      return ClearState::AlreadyEmpty;
    }
    self.is_empty = true;
    self.final_layout_entry = None;
    self.measure_entries = [None; CACHE_SIZE];
    ClearState::Cleared
  }

  /// Returns true if all cache entries are None, else false
  pub fn is_empty(&self) -> bool {
    self.final_layout_entry.is_none() && !self.measure_entries.iter().any(|entry| entry.is_some())
  }
}

/// Clear operation outcome. See [`Cache::clear`]
pub enum ClearState {
  /// Cleared some values
  Cleared,
  /// Everything was already cleared
  AlreadyEmpty,
}
