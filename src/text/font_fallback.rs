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
use super::font_db::FontDatabase;
use super::font_db::FontStretch;
use super::font_db::FontStyle;
use super::font_db::FontWeight;
use super::font_db::GenericFamily;
use crate::style::types::FontVariantEmoji;
use crate::text::font_resolver::emoji_preference_for_char;
use crate::text::font_resolver::resolve_font_for_char;
use crate::text::font_resolver::FontPreferencePicker;
use crate::text::font_resolver::FontResolverContext;
use fontdb::ID;

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

struct DatabaseResolver<'a> {
  db: &'a FontDatabase,
}

impl FontResolverContext for DatabaseResolver<'_> {
  fn database(&self) -> &FontDatabase {
    self.db
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
#[derive(Debug, Clone)]
pub struct FallbackChain {
  /// Ordered list of font families to try.
  families: Vec<FamilyEntry>,

  /// Font weight for matching (100-900, 400 = normal).
  weight: FontWeight,

  /// Font style for matching.
  style: FontStyle,

  /// Font stretch for matching.
  stretch: FontStretch,

  /// Emoji presentation preference.
  emoji_variant: FontVariantEmoji,
}

impl FallbackChain {
  /// Creates an empty fallback chain.
  pub fn new() -> Self {
    Self {
      families: Vec::new(),
      weight: FontWeight::NORMAL,
      style: FontStyle::Normal,
      stretch: FontStretch::Normal,
      emoji_variant: FontVariantEmoji::Normal,
    }
  }

  /// Sets the emoji presentation preference used for fallback resolution.
  pub fn with_emoji_variant(mut self, variant: FontVariantEmoji) -> Self {
    self.emoji_variant = variant;
    self
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
    let pref = emoji_preference_for_char(c, self.emoji_variant);
    let mut picker = FontPreferencePicker::new(pref);
    let context = DatabaseResolver { db };
    resolve_font_for_char(
      c,
      &self.families,
      self.weight.value(),
      self.style,
      None,
      self.stretch,
      &context,
      &mut picker,
    )
    .and_then(|resolved| resolved.id.map(FontId::new))
  }

  /// Resolves the best font for any text (not character-specific).
  ///
  /// Returns the first font that matches the chain, regardless of
  /// glyph coverage. Useful for getting the primary font before
  /// text is known.
  pub fn resolve_default(&self, db: &FontDatabase) -> Option<FontId> {
    self.resolve(' ', db)
  }
}

impl Default for FallbackChain {
  fn default() -> Self {
    Self::new()
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

  /// Sets the emoji rendering preference.
  pub fn emoji_variant(mut self, variant: FontVariantEmoji) -> Self {
    self.chain = self.chain.with_emoji_variant(variant);
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
      .emoji_variant(FontVariantEmoji::Emoji)
      .build();

    assert_eq!(chain.len(), 3);
    assert_eq!(chain.weight, FontWeight::BOLD);
    assert_eq!(chain.style, FontStyle::Italic);
    assert_eq!(chain.emoji_variant, FontVariantEmoji::Emoji);
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
}
