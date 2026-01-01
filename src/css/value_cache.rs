//! Thread-local cache for parsed CSS property values.
//!
//! Large stylesheets often repeat the same literal values thousands of times (e.g. `0`, `none`,
//! `#fff`). Parsing each declaration independently would re-tokenize and re-parse the same strings.
//! This module provides a small, per-thread LRU cache keyed by property/context/value fingerprint so
//! rayon-parallel parsing can reuse parsed results without locks.

use super::properties::DeclarationContext;
use super::types::PropertyValue;
use lru::LruCache;
use rustc_hash::{FxBuildHasher, FxHasher};
use std::cell::RefCell;
use std::hash::Hasher;
use std::num::NonZeroUsize;

/// Number of entries to keep per thread.
///
/// 8k is large enough to cover repeated literals in real-world stylesheets, while keeping memory
/// usage bounded. Tune within the 4k–16k range as needed.
const PARSED_PROPERTY_VALUE_CACHE_CAPACITY: usize = 8 * 1024;

const PROPERTY_PREFIX_LEN: usize = 8;
const VALUE_PREFIX_LEN: usize = 16;

fn hash_bytes(bytes: &[u8]) -> u64 {
  let mut hasher = FxHasher::default();
  hasher.write(bytes);
  hasher.finish()
}

fn prefix_bytes<const N: usize>(bytes: &[u8]) -> [u8; N] {
  let mut out = [0u8; N];
  let len = bytes.len().min(N);
  out[..len].copy_from_slice(&bytes[..len]);
  out
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) struct ParsedPropertyValueCacheKey {
  property_hash: u64,
  property_len: u16,
  property_prefix: [u8; PROPERTY_PREFIX_LEN],
  context: DeclarationContext,
  skip_var_guard: bool,
  value_hash: u64,
  value_len: u32,
  value_prefix: [u8; VALUE_PREFIX_LEN],
}

impl ParsedPropertyValueCacheKey {
  #[inline]
  pub(crate) fn new(
    context: DeclarationContext,
    property: &str,
    value_str: &str,
    skip_var_guard: bool,
  ) -> Self {
    let prop_bytes = property.as_bytes();
    let val_bytes = value_str.as_bytes();
    Self {
      property_hash: hash_bytes(prop_bytes),
      property_len: prop_bytes.len().min(u16::MAX as usize) as u16,
      property_prefix: prefix_bytes(prop_bytes),
      context,
      skip_var_guard,
      value_hash: hash_bytes(val_bytes),
      value_len: val_bytes.len().min(u32::MAX as usize) as u32,
      value_prefix: prefix_bytes(val_bytes),
    }
  }
}

type ParsedPropertyValueCache = LruCache<ParsedPropertyValueCacheKey, PropertyValue, FxBuildHasher>;

fn new_cache() -> ParsedPropertyValueCache {
  let cap = NonZeroUsize::new(PARSED_PROPERTY_VALUE_CACHE_CAPACITY.max(1))
    .expect("Parsed property value cache capacity must be non-zero");
  ParsedPropertyValueCache::with_hasher(cap, FxBuildHasher::default())
}

thread_local! {
  static PARSED_PROPERTY_VALUE_CACHE: RefCell<ParsedPropertyValueCache> = RefCell::new(new_cache());
}

#[cfg(test)]
thread_local! {
  static PARSED_PROPERTY_VALUE_CACHE_HITS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
  static PARSED_PROPERTY_VALUE_CACHE_MISSES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

#[inline]
pub(crate) fn get(key: &ParsedPropertyValueCacheKey) -> Option<PropertyValue> {
  PARSED_PROPERTY_VALUE_CACHE.with(|cache| {
    let mut cache = cache.borrow_mut();
    let hit = cache.get(key).cloned();
    #[cfg(test)]
    {
      if hit.is_some() {
        PARSED_PROPERTY_VALUE_CACHE_HITS.with(|counter| counter.set(counter.get() + 1));
      } else {
        PARSED_PROPERTY_VALUE_CACHE_MISSES.with(|counter| counter.set(counter.get() + 1));
      }
    }
    hit
  })
}

#[inline]
pub(crate) fn put(key: ParsedPropertyValueCacheKey, value: PropertyValue) {
  PARSED_PROPERTY_VALUE_CACHE.with(|cache| {
    cache.borrow_mut().put(key, value);
  });
}

#[cfg(test)]
pub(crate) fn reset_for_tests() {
  PARSED_PROPERTY_VALUE_CACHE.with(|cache| {
    *cache.borrow_mut() = new_cache();
  });
  PARSED_PROPERTY_VALUE_CACHE_HITS.with(|counter| counter.set(0));
  PARSED_PROPERTY_VALUE_CACHE_MISSES.with(|counter| counter.set(0));
}

#[cfg(test)]
pub(crate) fn reset_stats_for_tests() {
  PARSED_PROPERTY_VALUE_CACHE_HITS.with(|counter| counter.set(0));
  PARSED_PROPERTY_VALUE_CACHE_MISSES.with(|counter| counter.set(0));
}

#[cfg(test)]
pub(crate) fn stats_for_tests() -> (usize, usize) {
  let hits = PARSED_PROPERTY_VALUE_CACHE_HITS.with(|counter| counter.get());
  let misses = PARSED_PROPERTY_VALUE_CACHE_MISSES.with(|counter| counter.get());
  (hits, misses)
}
