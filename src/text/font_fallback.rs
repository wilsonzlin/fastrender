//! Font fallback chain implementation.
//!
//! This module implements CSS font-family fallback according to
//! CSS Fonts Module Level 4, Section 5.
//!
//! # Overview
//!
//! When rendering text, the browser tries fonts in the order specified
//! in the font-family property. If a font doesn't have a glyph for a
//! character, it falls back to the next font in the list.
//!
//! ```text
//! font-family: "Roboto", Arial, sans-serif;
//!              ───────   ─────  ──────────
//!                 │        │         │
//!                 │        │         └─ Generic family (last resort)
//!                 │        └─ Named fallback
//!                 └─ Primary font
//! ```
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::{FallbackChain, FontDatabase, GenericFamily};
//!
//! let db = FontDatabase::new();
//! let chain = FallbackChain::new()
//!     .add_family("Roboto")
//!     .add_family("Arial")
//!     .add_generic(GenericFamily::SansSerif);
//!
//! // Find a font for the character 'A'
//! if let Some(font_id) = chain.resolve('A', &db) {
//!     println!("Found font for 'A'");
//! }
//! ```
//!
//! # References
//!
//! - CSS Fonts Module Level 4, Section 5: <https://www.w3.org/TR/css-fonts-4/#font-matching-algorithm>

use super::emoji;
use super::font_db::FontDatabase;
use super::font_db::FontStretch;
use super::font_db::FontStyle;
use super::font_db::FontWeight;
use super::font_db::GenericFamily;
use super::font_db::LoadedFont;
use fontdb::Family;
use fontdb::Query;
use fontdb::ID;
use lru::LruCache;
use rustc_hash::FxHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::hash::BuildHasherDefault;
use std::num::NonZeroUsize;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

type FallbackCacheHasher = BuildHasherDefault<FxHasher>;
const FALLBACK_CACHE_SHARDS: usize = 16;
const FALLBACK_CACHE_CAPACITY_ENV: &str = "FASTR_TEXT_FALLBACK_CACHE_CAPACITY";

fn fallback_cache_capacity_from_env(default: usize) -> usize {
  match std::env::var(FALLBACK_CACHE_CAPACITY_ENV) {
    Ok(raw) => {
      let trimmed = raw.trim();
      if trimmed.is_empty() {
        return default;
      }
      match trimmed.parse::<usize>() {
        Ok(0) | Err(_) => default,
        Ok(value) => value,
      }
    }
    Err(_) => default,
  }
}

#[derive(Debug, Clone, Copy)]
struct ShardedInsertOutcome {
  inserted_new: bool,
  evicted: bool,
}

#[derive(Debug)]
struct ShardedLruCache<K: Hash + Eq, V> {
  shards: Vec<Mutex<LruCache<K, V, FallbackCacheHasher>>>,
  total_capacity: usize,
}

impl<K, V> ShardedLruCache<K, V>
where
  K: Hash + Eq,
  V: Clone,
{
  fn new(capacity: usize, shard_count: usize) -> Self {
    let capacity = capacity.max(1);
    let shard_count = shard_count.clamp(1, capacity);
    let base_capacity = capacity / shard_count;
    let remainder = capacity % shard_count;

    let mut shards = Vec::with_capacity(shard_count);
    for idx in 0..shard_count {
      let shard_capacity = base_capacity + usize::from(idx < remainder);
      let shard_capacity = NonZeroUsize::new(shard_capacity.max(1)).unwrap();
      shards.push(Mutex::new(LruCache::with_hasher(
        shard_capacity,
        FallbackCacheHasher::default(),
      )));
    }

    Self {
      shards,
      total_capacity: capacity,
    }
  }

  #[inline]
  fn shard_count(&self) -> usize {
    self.shards.len()
  }

  #[inline]
  fn shard_index(&self, key: &K) -> usize {
    let mut hasher = FxHasher::default();
    key.hash(&mut hasher);
    let hash = hasher.finish();
    let mixed = hash ^ (hash >> 32);
    (mixed as usize) % self.shards.len()
  }

  fn get(&self, key: &K) -> Option<V> {
    let idx = self.shard_index(key);
    self.shards[idx]
      .lock()
      .ok()
      .and_then(|mut cache| cache.get(key).cloned())
  }

  fn put(&self, key: K, value: V) -> ShardedInsertOutcome {
    let idx = self.shard_index(&key);
    if let Ok(mut cache) = self.shards[idx].lock() {
      let inserted_new = cache.peek(&key).is_none();
      let evicted = inserted_new && cache.len() >= cache.cap().get();
      cache.put(key, value);
      return ShardedInsertOutcome { inserted_new, evicted };
    }
    ShardedInsertOutcome {
      inserted_new: false,
      evicted: false,
    }
  }

  fn clear(&self) {
    for shard in &self.shards {
      if let Ok(mut cache) = shard.lock() {
        cache.clear();
      }
    }
  }
}

/// Unique identifier for a font face in the database.
///
/// This is a wrapper around fontdb's internal ID, providing a stable
/// identifier for caching and reference purposes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FontId(pub ID);

impl FontId {
  /// Creates a new FontId from a fontdb ID.
  pub fn new(id: ID) -> Self {
    Self(id)
  }

  /// Returns the underlying fontdb ID.
  pub fn inner(self) -> ID {
    self.0
  }
}

/// A font family entry in the fallback chain.
///
/// Can be either a named font family or a generic family.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FamilyEntry {
  /// A specific font family name (e.g., "Roboto", "Arial").
  Named(String),
  /// A generic font family (e.g., sans-serif, monospace).
  Generic(GenericFamily),
}

/// Preference for selecting emoji vs. text fonts for a cluster.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum EmojiPreference {
  PreferEmoji,
  AvoidEmoji,
  Neutral,
}

fn hash_case_folded<H: Hasher>(mut hasher: H, name: &str) -> u64 {
  use std::hash::Hash;

  for ch in name.chars() {
    match ch {
      // Unicode case folding special-cases
      '\u{00df}' | '\u{1e9e}' => {
        's'.hash(&mut hasher);
        's'.hash(&mut hasher);
      }
      '\u{03c2}' => '\u{03c3}'.hash(&mut hasher), // final sigma -> sigma
      '\u{212a}' => 'k'.hash(&mut hasher),        // Kelvin sign
      '\u{212b}' => '\u{00e5}'.hash(&mut hasher), // Angstrom sign -> å
      _ => {
        for lower in ch.to_lowercase() {
          lower.hash(&mut hasher);
        }
      }
    }
  }

  hasher.finish()
}

pub(crate) fn family_name_signature(name: &str) -> u64 {
  hash_case_folded(FxHasher::default(), name)
}

pub(crate) fn families_signature(families: &[FamilyEntry]) -> u64 {
  use std::hash::Hash;
  use std::hash::Hasher;

  let mut hasher = FxHasher::default();
  families.len().hash(&mut hasher);
  for entry in families {
    match entry {
      FamilyEntry::Named(name) => {
        0u8.hash(&mut hasher);
        family_name_signature(name).hash(&mut hasher);
      }
      FamilyEntry::Generic(generic) => {
        1u8.hash(&mut hasher);
        std::mem::discriminant(generic).hash(&mut hasher);
      }
    }
  }
  hasher.finish()
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct FallbackCacheDescriptor {
  pub families: u64,
  pub language: u64,
  pub script: u8,
  pub weight: u16,
  pub style: FontStyle,
  pub stretch: FontStretch,
  pub oblique_degrees: i16,
  pub emoji_pref: EmojiPreference,
  pub require_base: bool,
}

impl FallbackCacheDescriptor {
  pub(crate) fn new(
    families: u64,
    language: u64,
    script: u8,
    weight: u16,
    style: FontStyle,
    stretch: FontStretch,
    oblique_degrees: i16,
    emoji_pref: EmojiPreference,
    require_base: bool,
  ) -> Self {
    Self {
      families,
      language,
      script,
      weight,
      style,
      stretch,
      oblique_degrees,
      emoji_pref,
      require_base,
    }
  }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct GlyphFallbackCacheKey {
  pub descriptor: FallbackCacheDescriptor,
  pub ch: char,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct ClusterFallbackCacheKey {
  pub descriptor: FallbackCacheDescriptor,
  pub signature: u64,
}

#[derive(Default, Debug)]
struct FallbackCacheStats {
  glyph_hits: AtomicU64,
  glyph_misses: AtomicU64,
  glyph_evictions: AtomicU64,
  glyph_entries: AtomicU64,
  cluster_hits: AtomicU64,
  cluster_misses: AtomicU64,
  cluster_evictions: AtomicU64,
  cluster_entries: AtomicU64,
  clears: AtomicU64,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct FallbackCacheStatsSnapshot {
  pub glyph_hits: u64,
  pub glyph_misses: u64,
  pub glyph_evictions: u64,
  pub glyph_entries: u64,
  pub glyph_capacity: u64,
  pub cluster_hits: u64,
  pub cluster_misses: u64,
  pub cluster_evictions: u64,
  pub cluster_entries: u64,
  pub cluster_capacity: u64,
  pub shards: u64,
  pub clears: u64,
}

impl FallbackCacheStats {
  fn snapshot(&self) -> FallbackCacheStatsSnapshot {
    FallbackCacheStatsSnapshot {
      glyph_hits: self.glyph_hits.load(Ordering::Relaxed),
      glyph_misses: self.glyph_misses.load(Ordering::Relaxed),
      glyph_evictions: self.glyph_evictions.load(Ordering::Relaxed),
      glyph_entries: self.glyph_entries.load(Ordering::Relaxed),
      glyph_capacity: 0,
      cluster_hits: self.cluster_hits.load(Ordering::Relaxed),
      cluster_misses: self.cluster_misses.load(Ordering::Relaxed),
      cluster_evictions: self.cluster_evictions.load(Ordering::Relaxed),
      cluster_entries: self.cluster_entries.load(Ordering::Relaxed),
      cluster_capacity: 0,
      shards: 0,
      clears: self.clears.load(Ordering::Relaxed),
    }
  }

  fn record_glyph(&self, hit: bool) {
    if hit {
      self.glyph_hits.fetch_add(1, Ordering::Relaxed);
    } else {
      self.glyph_misses.fetch_add(1, Ordering::Relaxed);
    }
  }

  fn record_cluster(&self, hit: bool) {
    if hit {
      self.cluster_hits.fetch_add(1, Ordering::Relaxed);
    } else {
      self.cluster_misses.fetch_add(1, Ordering::Relaxed);
    }
  }
}

#[derive(Debug, Clone)]
pub(crate) struct FallbackCache {
  glyphs: Arc<ShardedLruCache<GlyphFallbackCacheKey, Option<Arc<LoadedFont>>>>,
  clusters: Arc<ShardedLruCache<ClusterFallbackCacheKey, Option<Arc<LoadedFont>>>>,
  last_generation: Arc<AtomicU64>,
  stats: Arc<FallbackCacheStats>,
}

impl FallbackCache {
  pub(crate) fn new(capacity: usize) -> Self {
    let capacity = fallback_cache_capacity_from_env(capacity).max(1);
    let shard_count = FALLBACK_CACHE_SHARDS.min(capacity).max(1);
    Self {
      glyphs: Arc::new(ShardedLruCache::new(capacity, shard_count)),
      clusters: Arc::new(ShardedLruCache::new(capacity, shard_count)),
      last_generation: Arc::new(AtomicU64::new(0)),
      stats: Arc::new(FallbackCacheStats::default()),
    }
  }

  pub(crate) fn prepare(&self, generation: u64) {
    let previous = self.last_generation.load(Ordering::Acquire);
    if previous == generation {
      return;
    }

    self.glyphs.clear();
    self.clusters.clear();
    self.stats.glyph_entries.store(0, Ordering::Relaxed);
    self
      .stats
      .cluster_entries
      .store(0, Ordering::Relaxed);
    self.last_generation.store(generation, Ordering::Release);
    self.stats.clears.fetch_add(1, Ordering::Relaxed);
  }

  pub(crate) fn get_glyph(
    &self,
    key: &GlyphFallbackCacheKey,
  ) -> Option<Option<Arc<LoadedFont>>> {
    let result = self.glyphs.get(key);
    self.stats.record_glyph(result.is_some());
    result
  }

  pub(crate) fn get_cluster(
    &self,
    key: &ClusterFallbackCacheKey,
  ) -> Option<Option<Arc<LoadedFont>>> {
    let result = self.clusters.get(key);
    self.stats.record_cluster(result.is_some());
    result
  }

  pub(crate) fn insert_glyph(
    &self,
    key: GlyphFallbackCacheKey,
    value: Option<Arc<LoadedFont>>,
  ) {
    let outcome = self.glyphs.put(key, value);
    if outcome.evicted {
      self.stats.glyph_evictions.fetch_add(1, Ordering::Relaxed);
    }
    if outcome.inserted_new && !outcome.evicted {
      self.stats.glyph_entries.fetch_add(1, Ordering::Relaxed);
    }
  }

  pub(crate) fn insert_cluster(
    &self,
    key: ClusterFallbackCacheKey,
    value: Option<Arc<LoadedFont>>,
  ) {
    let outcome = self.clusters.put(key, value);
    if outcome.evicted {
      self
        .stats
        .cluster_evictions
        .fetch_add(1, Ordering::Relaxed);
    }
    if outcome.inserted_new && !outcome.evicted {
      self
        .stats
        .cluster_entries
        .fetch_add(1, Ordering::Relaxed);
    }
  }

  pub(crate) fn clear(&self) {
    self.glyphs.clear();
    self.clusters.clear();
    self.stats.glyph_entries.store(0, Ordering::Relaxed);
    self
      .stats
      .cluster_entries
      .store(0, Ordering::Relaxed);
    self.stats.clears.fetch_add(1, Ordering::Relaxed);
  }

  pub(crate) fn stats(&self) -> FallbackCacheStatsSnapshot {
    let mut snapshot = self.stats.snapshot();
    snapshot.glyph_capacity = self.glyphs.total_capacity as u64;
    snapshot.cluster_capacity = self.clusters.total_capacity as u64;
    snapshot.shards = self.glyphs.shard_count() as u64;
    snapshot
  }
}

impl FamilyEntry {
  /// Creates a named family entry.
  pub fn named(name: impl Into<String>) -> Self {
    Self::Named(name.into())
  }

  /// Creates a generic family entry.
  pub fn generic(family: GenericFamily) -> Self {
    Self::Generic(family)
  }

  /// Returns true if this is a generic family.
  pub fn is_generic(&self) -> bool {
    matches!(self, Self::Generic(_))
  }
}

/// Font fallback chain.
///
/// Represents an ordered list of font families to try when rendering text.
/// The chain is tried in order until a font is found that contains the
/// required glyph.
///
/// # CSS Font Matching Algorithm
///
/// The font matching algorithm (CSS Fonts Level 4, Section 5) works as:
///
/// 1. For each family in the list:
///    a. Find fonts matching the family name
///    b. Filter by font-stretch, font-style, font-weight
///    c. If a matching font has the required glyph, use it
///    d. Otherwise, try the next family
///
/// 2. If no family matches, use the system fallback font
///
/// # Example
///
/// ```rust,ignore
/// let chain = FallbackChain::new()
///     .add_family("Custom Font")
///     .add_family("Arial")
///     .add_generic(GenericFamily::SansSerif);
/// ```
#[derive(Debug, Clone, Default)]
pub struct FallbackChain {
  /// Ordered list of font families to try.
  families: Vec<FamilyEntry>,

  /// Font weight for matching (100-900, 400 = normal).
  weight: FontWeight,

  /// Font style for matching.
  style: FontStyle,

  /// Font stretch for matching.
  stretch: FontStretch,
}

impl FallbackChain {
  /// Creates an empty fallback chain.
  pub fn new() -> Self {
    Self {
      families: Vec::new(),
      weight: FontWeight::NORMAL,
      style: FontStyle::Normal,
      stretch: FontStretch::Normal,
    }
  }

  /// Creates a fallback chain from a list of family names.
  ///
  /// This parses each name, detecting generic families automatically.
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let chain = FallbackChain::from_families(&[
  ///     "Roboto".to_string(),
  ///     "Arial".to_string(),
  ///     "sans-serif".to_string(),
  /// ]);
  /// ```
  pub fn from_families(families: &[String]) -> Self {
    let mut chain = Self::new();
    for family in families {
      if let Some(generic) = GenericFamily::parse(family) {
        chain = chain.add_generic(generic);
      } else {
        chain = chain.add_family(family);
      }
    }
    chain
  }

  /// Adds a named font family to the chain.
  ///
  /// The family is added at the end of the chain.
  pub fn add_family(mut self, name: impl Into<String>) -> Self {
    self.families.push(FamilyEntry::Named(name.into()));
    self
  }

  /// Adds a generic font family to the chain.
  ///
  /// Generic families are typically added last as a fallback.
  pub fn add_generic(mut self, family: GenericFamily) -> Self {
    self.families.push(FamilyEntry::Generic(family));
    self
  }

  /// Sets the font weight for matching.
  ///
  /// Values range from 100 (thin) to 900 (black).
  /// Common values: 400 (normal), 700 (bold).
  pub fn with_weight(mut self, weight: u16) -> Self {
    self.weight = FontWeight::new(weight);
    self
  }

  /// Sets the font style for matching.
  pub fn with_style(mut self, style: FontStyle) -> Self {
    self.style = style;
    self
  }

  /// Sets the font stretch for matching.
  pub fn with_stretch(mut self, stretch: FontStretch) -> Self {
    self.stretch = stretch;
    self
  }

  /// Returns the families in this chain.
  pub fn families(&self) -> &[FamilyEntry] {
    &self.families
  }

  /// Returns true if the chain is empty.
  pub fn is_empty(&self) -> bool {
    self.families.is_empty()
  }

  /// Returns the number of families in the chain.
  pub fn len(&self) -> usize {
    self.families.len()
  }

  /// Resolves a font for the given character.
  ///
  /// Tries each font in the chain until one is found that contains
  /// a glyph for the character. Returns None if no font can render
  /// the character.
  ///
  /// # Arguments
  ///
  /// * `c` - The character to find a font for
  /// * `db` - The font database to search
  ///
  /// # Returns
  ///
  /// The FontId of the first font that can render the character,
  /// or None if no suitable font is found.
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let db = FontDatabase::new();
  /// let chain = FallbackChain::from_families(&["Arial".to_string()]);
  ///
  /// if let Some(font_id) = chain.resolve('A', &db) {
  ///     println!("Found font for 'A'");
  /// }
  /// ```
  pub fn resolve(&self, c: char, db: &FontDatabase) -> Option<FontId> {
    // Special handling for emoji characters
    if emoji::is_emoji(c) {
      if let Some(font_id) = self.resolve_emoji(c, db) {
        return Some(font_id);
      }
      // If no emoji font found, fall through to regular resolution
    }

    // Try each family in order
    for entry in &self.families {
      if let Some(font_id) = self.try_resolve_entry(entry, c, db) {
        return Some(font_id);
      }
    }

    // Last resort: try any font in the database
    self.resolve_any_font(c, db)
  }

  /// Resolves the best font for any text (not character-specific).
  ///
  /// Returns the first font that matches the chain, regardless of
  /// glyph coverage. Useful for getting the primary font before
  /// text is known.
  pub fn resolve_default(&self, db: &FontDatabase) -> Option<FontId> {
    for entry in &self.families {
      if let Some(font_id) = self.query_font(entry, db) {
        return Some(font_id);
      }
    }

    // Fall back to any font
    db.faces().next().map(|f| FontId::new(f.id))
  }

  /// Resolves an emoji character.
  ///
  /// Tries emoji-specific fonts first before falling back to
  /// the regular chain.
  fn resolve_emoji(&self, c: char, db: &FontDatabase) -> Option<FontId> {
    // Check if chain explicitly includes emoji fonts
    for entry in &self.families {
      if let FamilyEntry::Generic(GenericFamily::Emoji) = entry {
        if let Some(font_id) = self.try_resolve_entry(entry, c, db) {
          return Some(font_id);
        }
      }
    }

    // Try system emoji fonts
    db.find_emoji_fonts()
      .into_iter()
      .find(|&id| db.has_glyph(id, c))
      .map(FontId::new)
  }

  /// Tries to resolve a single family entry for a character.
  fn try_resolve_entry(&self, entry: &FamilyEntry, c: char, db: &FontDatabase) -> Option<FontId> {
    match entry {
      FamilyEntry::Named(name) => self.resolve_named(name, c, db),
      FamilyEntry::Generic(generic) => self.resolve_generic(*generic, c, db),
    }
  }

  /// Resolves a named font family for a character.
  fn resolve_named(&self, name: &str, c: char, db: &FontDatabase) -> Option<FontId> {
    let query = Query {
      families: &[Family::Name(name)],
      weight: fontdb::Weight(self.weight.value()),
      stretch: self.stretch.into(),
      style: self.style.into(),
    };

    if let Some(id) = db.inner().query(&query) {
      if db.has_glyph(id, c) {
        return Some(FontId::new(id));
      }
    }

    None
  }

  /// Resolves a generic font family for a character.
  fn resolve_generic(&self, generic: GenericFamily, c: char, db: &FontDatabase) -> Option<FontId> {
    let prefer_named_fallbacks = generic.prefers_named_fallbacks_first();

    if prefer_named_fallbacks {
      for name in generic.fallback_families() {
        if let Some(font_id) = self.resolve_named(name, c, db) {
          return Some(font_id);
        }
      }
    }

    let query = Query {
      families: &[generic.to_fontdb()],
      weight: fontdb::Weight(self.weight.value()),
      stretch: self.stretch.into(),
      style: self.style.into(),
    };

    if let Some(id) = db.inner().query(&query) {
      if db.has_glyph(id, c) {
        return Some(FontId::new(id));
      }
    }

    if !prefer_named_fallbacks {
      for name in generic.fallback_families() {
        if let Some(font_id) = self.resolve_named(name, c, db) {
          return Some(font_id);
        }
      }
    }

    None
  }

  /// Queries for a font without checking glyph coverage.
  fn query_font(&self, entry: &FamilyEntry, db: &FontDatabase) -> Option<FontId> {
    let families: Vec<Family> = match entry {
      FamilyEntry::Named(name) => vec![Family::Name(name)],
      FamilyEntry::Generic(generic) => {
        // Include the generic family and its fallback names
        let prefer_named = generic.prefers_named_fallbacks_first();
        let mut families = if prefer_named {
          generic
            .fallback_families()
            .iter()
            .copied()
            .map(Family::Name)
            .collect::<Vec<_>>()
        } else {
          vec![generic.to_fontdb()]
        };
        if !prefer_named {
          families.extend(
            generic
              .fallback_families()
              .iter()
              .copied()
              .map(Family::Name),
          );
        } else {
          families.push(generic.to_fontdb());
        }
        families
      }
    };

    for family in &families {
      let query = Query {
        families: std::slice::from_ref(family),
        weight: fontdb::Weight(self.weight.value()),
        stretch: self.stretch.into(),
        style: self.style.into(),
      };

      if let Some(id) = db.inner().query(&query) {
        return Some(FontId::new(id));
      }
    }

    None
  }

  /// Last resort: try to find any font that has the glyph.
  fn resolve_any_font(&self, c: char, db: &FontDatabase) -> Option<FontId> {
    for face in db.faces() {
      if db.has_glyph(face.id, c) {
        return Some(FontId::new(face.id));
      }
    }
    None
  }
}

/// Builder for constructing fallback chains.
///
/// Provides a fluent API for building fallback chains with
/// additional configuration options.
#[derive(Debug, Clone, Default)]
pub struct FallbackChainBuilder {
  chain: FallbackChain,
}

impl FallbackChainBuilder {
  /// Creates a new builder.
  pub fn new() -> Self {
    Self::default()
  }

  /// Adds a named font family.
  pub fn family(mut self, name: impl Into<String>) -> Self {
    self.chain = self.chain.add_family(name);
    self
  }

  /// Adds a generic font family.
  pub fn generic(mut self, family: GenericFamily) -> Self {
    self.chain = self.chain.add_generic(family);
    self
  }

  /// Sets the font weight.
  pub fn weight(mut self, weight: u16) -> Self {
    self.chain = self.chain.with_weight(weight);
    self
  }

  /// Sets normal font weight (400).
  pub fn normal(self) -> Self {
    self.weight(400)
  }

  /// Sets bold font weight (700).
  pub fn bold(self) -> Self {
    self.weight(700)
  }

  /// Sets the font style.
  pub fn style(mut self, style: FontStyle) -> Self {
    self.chain = self.chain.with_style(style);
    self
  }

  /// Sets italic font style.
  pub fn italic(self) -> Self {
    self.style(FontStyle::Italic)
  }

  /// Sets the font stretch.
  pub fn stretch(mut self, stretch: FontStretch) -> Self {
    self.chain = self.chain.with_stretch(stretch);
    self
  }

  /// Builds the fallback chain.
  pub fn build(self) -> FallbackChain {
    self.chain
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::sync::OnceLock;

  fn dummy_descriptor() -> FallbackCacheDescriptor {
    FallbackCacheDescriptor::new(
      1,
      2,
      3,
      400,
      FontStyle::Normal,
      FontStretch::Normal,
      0,
      EmojiPreference::Neutral,
      true,
    )
  }

  fn env_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
  }

  struct EnvVarGuard {
    key: &'static str,
    prev: Option<String>,
  }

  impl EnvVarGuard {
    fn set(key: &'static str, value: &str) -> Self {
      let prev = std::env::var(key).ok();
      std::env::set_var(key, value);
      Self { key, prev }
    }
  }

  impl Drop for EnvVarGuard {
    fn drop(&mut self) {
      match &self.prev {
        Some(value) => std::env::set_var(self.key, value),
        None => std::env::remove_var(self.key),
      }
    }
  }

  #[test]
  fn test_fallback_chain_new() {
    let chain = FallbackChain::new();
    assert!(chain.is_empty());
    assert_eq!(chain.len(), 0);
  }

  #[test]
  fn test_fallback_chain_add_family() {
    let chain = FallbackChain::new()
      .add_family("Arial")
      .add_family("Helvetica");

    assert_eq!(chain.len(), 2);
    assert!(!chain.is_empty());
  }

  #[test]
  fn test_fallback_chain_add_generic() {
    let chain = FallbackChain::new()
      .add_family("Custom")
      .add_generic(GenericFamily::SansSerif);

    assert_eq!(chain.len(), 2);

    match &chain.families()[1] {
      FamilyEntry::Generic(GenericFamily::SansSerif) => {}
      _ => panic!("Expected generic sans-serif"),
    }
  }

  #[test]
  fn test_fallback_chain_from_families() {
    let families = vec![
      "Roboto".to_string(),
      "Arial".to_string(),
      "sans-serif".to_string(),
    ];
    let chain = FallbackChain::from_families(&families);

    assert_eq!(chain.len(), 3);

    // First two should be named
    assert!(matches!(&chain.families()[0], FamilyEntry::Named(n) if n == "Roboto"));
    assert!(matches!(&chain.families()[1], FamilyEntry::Named(n) if n == "Arial"));

    // Last should be generic
    assert!(matches!(
      &chain.families()[2],
      FamilyEntry::Generic(GenericFamily::SansSerif)
    ));
  }

  #[test]
  fn test_fallback_chain_builder() {
    let chain = FallbackChainBuilder::new()
      .family("Roboto")
      .family("Arial")
      .generic(GenericFamily::SansSerif)
      .bold()
      .italic()
      .build();

    assert_eq!(chain.len(), 3);
    assert_eq!(chain.weight, FontWeight::BOLD);
    assert_eq!(chain.style, FontStyle::Italic);
  }

  #[test]
  fn test_family_entry_is_generic() {
    let named = FamilyEntry::named("Arial");
    assert!(!named.is_generic());

    let generic = FamilyEntry::generic(GenericFamily::Serif);
    assert!(generic.is_generic());
  }

  #[test]
  fn test_fallback_chain_resolve_with_empty_db() {
    let db = FontDatabase::empty();
    let chain = FallbackChain::new().add_family("Arial");

    // Should return None since no fonts are loaded
    let result = chain.resolve('A', &db);
    assert!(result.is_none());
  }

  #[test]
  fn test_fallback_chain_resolve_default_empty() {
    let db = FontDatabase::empty();
    let chain = FallbackChain::new().add_family("Arial");

    let result = chain.resolve_default(&db);
    assert!(result.is_none());
  }

  #[test]
  fn fallback_cache_prepare_clears_shards() {
    let _lock = env_lock().lock().unwrap();
    let _guard = EnvVarGuard::set(FALLBACK_CACHE_CAPACITY_ENV, "0");
    let cache = FallbackCache::new(32);
    let descriptor = dummy_descriptor();
    let glyph_key = GlyphFallbackCacheKey { descriptor, ch: 'a' };
    let cluster_key = ClusterFallbackCacheKey {
      descriptor,
      signature: 42,
    };

    cache.insert_glyph(glyph_key, None);
    cache.insert_cluster(cluster_key, None);
    assert!(matches!(cache.get_glyph(&glyph_key), Some(None)));
    assert!(matches!(cache.get_cluster(&cluster_key), Some(None)));

    // New cache starts at generation 0, so prepare(0) is a no-op.
    cache.prepare(0);
    assert!(matches!(cache.get_glyph(&glyph_key), Some(None)));
    assert!(matches!(cache.get_cluster(&cluster_key), Some(None)));

    // Generation bump should clear all shards.
    cache.prepare(1);
    assert!(cache.get_glyph(&glyph_key).is_none());
    assert!(cache.get_cluster(&cluster_key).is_none());

    let stats = cache.stats();
    assert_eq!(stats.clears, 1);
    assert_eq!(stats.glyph_entries, 0);
    assert_eq!(stats.cluster_entries, 0);
    assert!(stats.shards > 1);
  }

  #[test]
  fn fallback_cache_records_evictions() {
    let _lock = env_lock().lock().unwrap();
    let _guard = EnvVarGuard::set(FALLBACK_CACHE_CAPACITY_ENV, "0");
    let cache = FallbackCache::new(1);
    let descriptor = dummy_descriptor();

    let glyph_key_1 = GlyphFallbackCacheKey { descriptor, ch: 'a' };
    let glyph_key_2 = GlyphFallbackCacheKey { descriptor, ch: 'b' };
    cache.insert_glyph(glyph_key_1, None);
    cache.insert_glyph(glyph_key_2, None);

    let cluster_key_1 = ClusterFallbackCacheKey {
      descriptor,
      signature: 1,
    };
    let cluster_key_2 = ClusterFallbackCacheKey {
      descriptor,
      signature: 2,
    };
    cache.insert_cluster(cluster_key_1, None);
    cache.insert_cluster(cluster_key_2, None);

    let stats = cache.stats();
    assert!(stats.glyph_evictions >= 1);
    assert!(stats.cluster_evictions >= 1);
    assert_eq!(stats.glyph_entries, 1);
    assert_eq!(stats.cluster_entries, 1);
  }

  #[test]
  fn fallback_cache_capacity_env_override_is_honored() {
    let _lock = env_lock().lock().unwrap();
    let _guard = EnvVarGuard::set(FALLBACK_CACHE_CAPACITY_ENV, "123");

    let cache = FallbackCache::new(1);
    let stats = cache.stats();
    assert_eq!(stats.glyph_capacity, 123);
    assert_eq!(stats.cluster_capacity, 123);
    assert_eq!(stats.shards, FALLBACK_CACHE_SHARDS as u64);
  }
}
