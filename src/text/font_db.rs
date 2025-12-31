//! Font database - font discovery and loading
//!
//! This module provides a wrapper around the `fontdb` crate for font
//! discovery, matching, and loading. It implements CSS-compliant font
//! matching with fallback chains.
//!
//! # Overview
//!
//! The font database:
//! - Discovers system fonts on all platforms (Windows, macOS, Linux)
//! - Loads font files (TTF, OTF, TTC)
//! - Queries fonts by family, weight, style, and stretch
//! - Caches loaded font data with Arc for sharing
//! - Handles fallback chains (e.g., "Arial, Helvetica, sans-serif")
//!
//! # CSS Specification
//!
//! Font matching follows CSS Fonts Module Level 4:
//! - <https://www.w3.org/TR/css-fonts-4/#font-matching-algorithm>
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::font_db::{FontDatabase, FontWeight, FontStyle};
//!
//! let db = FontDatabase::new();
//!
//! // Query for a specific font
//! if let Some(id) = db.query("Arial", FontWeight::NORMAL, FontStyle::Normal) {
//!     let font = db.load_font(id).expect("Should load font");
//!     println!("Loaded {} with {} bytes", font.family, font.data.len());
//! }
//! ```

use crate::error::FontError;
use crate::error::Result;
use crate::text::emoji;
use crate::text::font_fallback::FontId;
use fontdb::Database as FontDbDatabase;
use fontdb::Family as FontDbFamily;
use fontdb::Query as FontDbQuery;
use fontdb::ID;
use lru::LruCache;
use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::sync::RwLock;
use ttf_parser::Tag;

use crate::text::face_cache::{self, CachedFace};

#[cfg(debug_assertions)]
pub use crate::text::face_cache::FaceParseCountGuard;
pub use crate::text::face_cache::{face_parse_count, reset_face_parse_counter_for_tests};

const GLYPH_COVERAGE_CACHE_SIZE: usize = 128;
#[derive(Clone, Copy)]
struct BundledFont {
  name: &'static str,
  data: &'static [u8],
}

// Ordered from general text to narrower script fallbacks so generic families stay stable.
const BUNDLED_FONTS: &[BundledFont] = &[
  BundledFont {
    name: "Noto Sans",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSans-subset.ttf"),
  },
  BundledFont {
    name: "Noto Serif",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSerif-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans Mono",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansMono-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans Arabic",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansArabic-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans Hebrew",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansHebrew-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans Devanagari",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansDevanagari-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans Bengali",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansBengali-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans Tamil",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansTamil-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans Thai",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansThai-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans Thaana",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansThaana-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans Syriac",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansSyriac-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans NKo",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansNKo-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans SC",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansSC-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans JP",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansJP-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans KR",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansKR-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans Symbols",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansSymbols-subset.ttf"),
  },
  BundledFont {
    name: "Noto Sans Symbols 2",
    data: include_bytes!("../../tests/fixtures/fonts/NotoSansSymbols2-subset.ttf"),
  },
  BundledFont {
    name: "STIX Two Math",
    data: include_bytes!("../../tests/fixtures/fonts/STIXTwoMath-Regular.otf"),
  },
  BundledFont {
    name: "DejaVu Sans",
    data: include_bytes!("../../tests/fixtures/fonts/DejaVuSans-subset.ttf"),
  },
];
const BUNDLED_EMOJI_FONTS: &[BundledFont] = &[BundledFont {
  name: "FastRender Emoji",
  data: include_bytes!("../../tests/fixtures/fonts/FastRenderEmoji.ttf"),
}];

fn env_flag(var: &str) -> Option<bool> {
  std::env::var(var).ok().map(|v| {
    !matches!(v.as_str(), "0" | "false" | "False" | "FALSE" | "") && !v.eq_ignore_ascii_case("off")
  })
}

fn should_load_bundled_emoji_fonts() -> bool {
  env_flag("FASTR_BUNDLE_EMOJI_FONT").unwrap_or(true)
}

/// Configuration for font discovery and loading.
#[derive(Debug, Clone)]
pub struct FontConfig {
  /// Whether to load system fonts discovered via fontdb/platform APIs.
  pub use_system_fonts: bool,
  /// Whether to always load the bundled fallback fonts shipped with FastRender.
  pub use_bundled_fonts: bool,
  /// Additional directories to scan for fonts.
  pub font_dirs: Vec<PathBuf>,
}

impl Default for FontConfig {
  fn default() -> Self {
    let bundled = env_flag("FASTR_USE_BUNDLED_FONTS")
      .or_else(|| env_flag("CI"))
      .unwrap_or(false);
    Self {
      // In CI we prefer deterministic bundled fonts unless explicitly overridden.
      use_system_fonts: !bundled,
      use_bundled_fonts: bundled,
      font_dirs: Vec::new(),
    }
  }
}

impl FontConfig {
  /// Creates a new configuration with default values.
  pub fn new() -> Self {
    Self::default()
  }

  /// Enables or disables system font discovery.
  pub fn with_system_fonts(mut self, enabled: bool) -> Self {
    self.use_system_fonts = enabled;
    self
  }

  /// Enables or disables bundled fallback fonts.
  pub fn with_bundled_fonts(mut self, enabled: bool) -> Self {
    self.use_bundled_fonts = enabled;
    self
  }

  /// Adds a directory to search for fonts.
  pub fn add_font_dir(mut self, dir: impl Into<PathBuf>) -> Self {
    self.font_dirs.push(dir.into());
    self
  }

  /// Adds multiple directories to search for fonts.
  pub fn with_font_dirs<I, P>(mut self, dirs: I) -> Self
  where
    I: IntoIterator<Item = P>,
    P: Into<PathBuf>,
  {
    self.font_dirs.extend(dirs.into_iter().map(Into::into));
    self
  }

  /// Convenience for using only bundled fonts (no system discovery).
  pub fn bundled_only() -> Self {
    Self {
      use_system_fonts: false,
      use_bundled_fonts: true,
      font_dirs: Vec::new(),
    }
  }
}

/// Configuration for font caches.
#[derive(Debug, Clone, Copy)]
pub struct FontCacheConfig {
  /// Maximum number of parsed faces to retain for glyph coverage checks.
  pub glyph_coverage_cache_size: usize,
}

impl Default for FontCacheConfig {
  fn default() -> Self {
    Self {
      glyph_coverage_cache_size: GLYPH_COVERAGE_CACHE_SIZE,
    }
  }
}

fn shared_system_fontdb() -> Arc<FontDbDatabase> {
  static SYSTEM_FONT_DB: OnceLock<Arc<FontDbDatabase>> = OnceLock::new();
  Arc::clone(SYSTEM_FONT_DB.get_or_init(|| {
    let mut db = FontDbDatabase::new();
    db.load_system_fonts();
    Arc::new(db)
  }))
}

fn set_bundled_generic_fallbacks(db: &mut FontDbDatabase) {
  let faces: Vec<&fontdb::FaceInfo> = db.faces().collect();
  let Some(primary_family) = faces
    .iter()
    .find(|face| {
      face
        .families
        .iter()
        .all(|(name, _)| !FontDatabase::family_name_is_emoji_font(name))
    })
    .or_else(|| faces.first())
    .and_then(|face| face.families.first().map(|(name, _)| name.clone()))
  else {
    return;
  };

  let first_matching_family = |candidates: &[&str]| -> Option<String> {
    for face in faces.iter().filter(|face| {
      face
        .families
        .iter()
        .all(|(name, _)| !FontDatabase::family_name_is_emoji_font(name))
    }) {
      for (name, _) in &face.families {
        if candidates
          .iter()
          .any(|candidate| candidate.eq_ignore_ascii_case(name))
        {
          return Some(name.clone());
        }
      }
    }

    for face in &faces {
      for (name, _) in &face.families {
        if candidates
          .iter()
          .any(|candidate| candidate.eq_ignore_ascii_case(name))
        {
          return Some(name.clone());
        }
      }
    }

    None
  };

  // Prefer stable bundled defaults; fall back to the first bundled family if those are missing.
  let serif = first_matching_family(&["Noto Serif"]).unwrap_or_else(|| primary_family.clone());
  let sans = first_matching_family(&["Noto Sans"]).unwrap_or_else(|| primary_family.clone());
  let monospace =
    first_matching_family(&["Noto Sans Mono"]).unwrap_or_else(|| primary_family.clone());

  db.set_serif_family(serif);
  db.set_sans_serif_family(sans.clone());
  db.set_monospace_family(monospace);
  db.set_cursive_family(sans.clone());
  db.set_fantasy_family(sans);
}

fn shared_bundled_fontdb() -> Arc<FontDbDatabase> {
  static BUNDLED_FONT_DB: OnceLock<Arc<FontDbDatabase>> = OnceLock::new();
  Arc::clone(BUNDLED_FONT_DB.get_or_init(|| {
    let mut db = FontDbDatabase::new();

    for font in BUNDLED_FONTS {
      db.load_font_data(font.data.to_vec());
    }

    if should_load_bundled_emoji_fonts() {
      for font in BUNDLED_EMOJI_FONTS {
        db.load_font_data(font.data.to_vec());
      }
    }

    set_bundled_generic_fallbacks(&mut db);
    Arc::new(db)
  }))
}

#[inline]
fn parse_face_with_counter<'a>(
  data: &'a [u8],
  index: u32,
) -> std::result::Result<ttf_parser::Face<'a>, ttf_parser::FaceParsingError> {
  ttf_parser::Face::parse(data, index)
}

#[derive(Clone)]
struct GlyphCoverageCache {
  inner: Arc<Mutex<LruCache<ID, Arc<CachedFace>>>>,
}

impl GlyphCoverageCache {
  fn new(capacity: usize) -> Self {
    let cap = NonZeroUsize::new(capacity.max(1)).unwrap();
    Self {
      inner: Arc::new(Mutex::new(LruCache::new(cap))),
    }
  }

  fn get_or_put<F>(&self, id: ID, loader: F) -> Option<Arc<CachedFace>>
  where
    F: FnOnce() -> Option<Arc<CachedFace>>,
  {
    if let Ok(mut cache) = self.inner.lock() {
      if let Some(face) = cache.get(&id) {
        return Some(face.clone());
      }
    }

    let loaded = loader()?;

    if let Ok(mut cache) = self.inner.lock() {
      if let Some(face) = cache.get(&id) {
        return Some(face.clone());
      }
      cache.put(id, loaded.clone());
    }

    Some(loaded)
  }

  fn clear(&self) {
    if let Ok(mut cache) = self.inner.lock() {
      cache.clear();
    }
  }
}

/// Font weight (100-900)
///
/// CSS font-weight values range from 100 (thinnest) to 900 (heaviest).
/// Common keywords map to specific values:
/// - normal: 400
/// - bold: 700
///
/// # Examples
///
/// ```rust,ignore
/// use fastrender::text::font_db::FontWeight;
///
/// let normal = FontWeight::NORMAL; // 400
/// let bold = FontWeight::BOLD;     // 700
/// let custom = FontWeight(550);    // Between medium and semi-bold
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FontWeight(pub u16);

impl FontWeight {
  /// Black (900)
  pub const BLACK: Self = Self(900);
  /// Bold (700) - CSS `font-weight: bold`
  pub const BOLD: Self = Self(700);
  /// Extra Bold (800)
  pub const EXTRA_BOLD: Self = Self(800);
  /// Extra Light (200)
  pub const EXTRA_LIGHT: Self = Self(200);
  /// Light (300)
  pub const LIGHT: Self = Self(300);
  /// Medium (500)
  pub const MEDIUM: Self = Self(500);
  /// Normal/Regular (400) - CSS `font-weight: normal`
  pub const NORMAL: Self = Self(400);
  /// Semi Bold (600)
  pub const SEMI_BOLD: Self = Self(600);
  /// Thin (100)
  pub const THIN: Self = Self(100);

  /// Creates a new font weight, clamping to valid range [100, 900]
  #[inline]
  pub fn new(weight: u16) -> Self {
    Self(weight.clamp(100, 900))
  }

  /// Returns the numeric weight value
  #[inline]
  pub fn value(self) -> u16 {
    self.0
  }
}

impl Default for FontWeight {
  fn default() -> Self {
    Self::NORMAL
  }
}

impl From<u16> for FontWeight {
  fn from(weight: u16) -> Self {
    Self::new(weight)
  }
}

/// Font style (normal, italic, or oblique)
///
/// CSS font-style property values.
///
/// # Examples
///
/// ```rust,ignore
/// use fastrender::text::font_db::FontStyle;
///
/// let normal = FontStyle::Normal;
/// let italic = FontStyle::Italic;
/// let oblique = FontStyle::Oblique;
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum FontStyle {
  /// Normal upright text
  #[default]
  Normal,
  /// Italic text (designed italic letterforms)
  Italic,
  /// Oblique text (slanted version of normal)
  Oblique,
}

// Conversion traits for fontdb interoperability

impl From<FontStyle> for fontdb::Style {
  fn from(style: FontStyle) -> Self {
    match style {
      FontStyle::Normal => fontdb::Style::Normal,
      FontStyle::Italic => fontdb::Style::Italic,
      FontStyle::Oblique => fontdb::Style::Oblique,
    }
  }
}

impl From<fontdb::Style> for FontStyle {
  fn from(style: fontdb::Style) -> Self {
    match style {
      fontdb::Style::Normal => FontStyle::Normal,
      fontdb::Style::Italic => FontStyle::Italic,
      fontdb::Style::Oblique => FontStyle::Oblique,
    }
  }
}

/// Font stretch/width (condensed to expanded)
///
/// CSS font-stretch property values for width variants.
///
/// # Examples
///
/// ```rust,ignore
/// use fastrender::text::font_db::FontStretch;
///
/// let normal = FontStretch::Normal;
/// let condensed = FontStretch::Condensed;
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum FontStretch {
  /// Ultra Condensed (50%)
  UltraCondensed,
  /// Extra Condensed (62.5%)
  ExtraCondensed,
  /// Condensed (75%)
  Condensed,
  /// Semi Condensed (87.5%)
  SemiCondensed,
  /// Normal width (100%)
  #[default]
  Normal,
  /// Semi Expanded (112.5%)
  SemiExpanded,
  /// Expanded (125%)
  Expanded,
  /// Extra Expanded (150%)
  ExtraExpanded,
  /// Ultra Expanded (200%)
  UltraExpanded,
}

impl FontStretch {
  /// Convert to percentage value
  #[inline]
  pub fn to_percentage(self) -> f32 {
    match self {
      FontStretch::UltraCondensed => 50.0,
      FontStretch::ExtraCondensed => 62.5,
      FontStretch::Condensed => 75.0,
      FontStretch::SemiCondensed => 87.5,
      FontStretch::Normal => 100.0,
      FontStretch::SemiExpanded => 112.5,
      FontStretch::Expanded => 125.0,
      FontStretch::ExtraExpanded => 150.0,
      FontStretch::UltraExpanded => 200.0,
    }
  }

  /// Create from percentage value
  pub fn from_percentage(pct: f32) -> Self {
    if pct <= 56.0 {
      FontStretch::UltraCondensed
    } else if pct <= 69.0 {
      FontStretch::ExtraCondensed
    } else if pct <= 81.0 {
      FontStretch::Condensed
    } else if pct <= 94.0 {
      FontStretch::SemiCondensed
    } else if pct <= 106.0 {
      FontStretch::Normal
    } else if pct <= 119.0 {
      FontStretch::SemiExpanded
    } else if pct <= 137.0 {
      FontStretch::Expanded
    } else if pct <= 175.0 {
      FontStretch::ExtraExpanded
    } else {
      FontStretch::UltraExpanded
    }
  }
}

impl From<FontStretch> for fontdb::Stretch {
  fn from(stretch: FontStretch) -> Self {
    match stretch {
      FontStretch::UltraCondensed => fontdb::Stretch::UltraCondensed,
      FontStretch::ExtraCondensed => fontdb::Stretch::ExtraCondensed,
      FontStretch::Condensed => fontdb::Stretch::Condensed,
      FontStretch::SemiCondensed => fontdb::Stretch::SemiCondensed,
      FontStretch::Normal => fontdb::Stretch::Normal,
      FontStretch::SemiExpanded => fontdb::Stretch::SemiExpanded,
      FontStretch::Expanded => fontdb::Stretch::Expanded,
      FontStretch::ExtraExpanded => fontdb::Stretch::ExtraExpanded,
      FontStretch::UltraExpanded => fontdb::Stretch::UltraExpanded,
    }
  }
}

impl From<fontdb::Stretch> for FontStretch {
  fn from(stretch: fontdb::Stretch) -> Self {
    match stretch {
      fontdb::Stretch::UltraCondensed => FontStretch::UltraCondensed,
      fontdb::Stretch::ExtraCondensed => FontStretch::ExtraCondensed,
      fontdb::Stretch::Condensed => FontStretch::Condensed,
      fontdb::Stretch::SemiCondensed => FontStretch::SemiCondensed,
      fontdb::Stretch::Normal => FontStretch::Normal,
      fontdb::Stretch::SemiExpanded => FontStretch::SemiExpanded,
      fontdb::Stretch::Expanded => FontStretch::Expanded,
      fontdb::Stretch::ExtraExpanded => FontStretch::ExtraExpanded,
      fontdb::Stretch::UltraExpanded => FontStretch::UltraExpanded,
    }
  }
}

/// A loaded font with cached data
///
/// Contains the font binary data (shared via Arc) along with
/// metadata extracted from the font file.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::font_db::FontDatabase;
///
/// let db = FontDatabase::new();
/// if let Some(id) = db.query("Arial", FontWeight::NORMAL, FontStyle::Normal) {
///     let font = db.load_font(id).expect("Should load font");
///     println!("Family: {}", font.family);
///     println!("Data size: {} bytes", font.data.len());
/// }
/// ```
#[derive(Debug, Clone)]
pub struct LoadedFont {
  /// Stable identifier for this font in the font database, when available.
  pub id: Option<FontId>,
  /// Font binary data (shared via Arc for efficiency)
  pub data: Arc<Vec<u8>>,
  /// Font index within the file (for TTC font collections)
  pub index: u32,
  /// Font family name
  pub family: String,
  /// Font weight
  pub weight: FontWeight,
  /// Font style
  pub style: FontStyle,
  /// Font stretch
  pub stretch: FontStretch,
}

impl LoadedFont {
  /// Extract font metrics
  ///
  /// Parses the font tables to extract dimensional information.
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let font = ctx.get_sans_serif().unwrap();
  /// let metrics = font.metrics().unwrap();
  /// let scaled = metrics.scale(16.0);
  /// println!("Ascent: {}px", scaled.ascent);
  /// ```
  pub fn metrics(&self) -> Result<FontMetrics> {
    FontMetrics::from_font(self)
  }

  /// Extract font metrics using the provided variation coordinates.
  pub fn metrics_with_variations(&self, variations: &[(Tag, f32)]) -> Result<FontMetrics> {
    FontMetrics::from_font_with_variations(self, variations)
  }

  /// Returns a cached parsed face for reuse across shaping and paint.
  ///
  /// Callers that only need to borrow the face temporarily should prefer
  /// [`crate::text::face_cache::with_face`] to avoid cloning the Arc.
  pub fn as_cached_face(&self) -> Result<Arc<CachedFace>> {
    face_cache::get_ttf_face(self).ok_or_else(|| {
      FontError::LoadFailed {
        family: self.family.clone(),
        reason: "Failed to parse font".to_string(),
      }
      .into()
    })
  }

  /// Get ttf-parser Face for advanced operations
  ///
  /// Returns a parsed font face for accessing glyph data, kerning, etc.
  pub fn as_ttf_face(&self) -> Result<ttf_parser::Face<'_>> {
    parse_face_with_counter(&self.data, self.index).map_err(|e| {
      FontError::LoadFailed {
        family: self.family.clone(),
        reason: format!("Failed to parse font: {:?}", e),
      }
      .into()
    })
  }
}

/// Returns true if the font advertises an OpenType GSUB feature with the given tag.
pub fn font_has_feature(font: &LoadedFont, tag: [u8; 4]) -> bool {
  face_cache::with_face(font, |face| {
    if let Some(gsub) = face.tables().gsub {
      return gsub
        .features
        .index(ttf_parser::Tag::from_bytes(&tag))
        .is_some();
    }
    false
  })
  .unwrap_or(false)
}

fn face_has_color_tables(face: &ttf_parser::Face<'_>) -> bool {
  let raw = face.raw_face();
  let has_colr = raw.table(ttf_parser::Tag::from_bytes(b"COLR")).is_some();
  let has_cpal = raw.table(ttf_parser::Tag::from_bytes(b"CPAL")).is_some();
  let has_cbdt = raw.table(ttf_parser::Tag::from_bytes(b"CBDT")).is_some();
  let has_cblc = raw.table(ttf_parser::Tag::from_bytes(b"CBLC")).is_some();
  let has_sbix = raw.table(ttf_parser::Tag::from_bytes(b"sbix")).is_some();
  let has_svg = raw.table(ttf_parser::Tag::from_bytes(b"SVG ")).is_some();

  // Treat COLR alone as color-capable to accommodate fonts that embed default palettes without
  // a CPAL table.
  (has_colr && has_cpal) || (has_cbdt && has_cblc) || has_sbix || has_svg || has_colr
}

/// Generic font families as defined by CSS
///
/// These are abstract font families that map to actual system fonts.
///
/// # CSS Specification
///
/// See CSS Fonts Module Level 4, Section 4.2:
/// <https://www.w3.org/TR/css-fonts-4/#generic-font-families>
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GenericFamily {
  /// Serif fonts (e.g., Times New Roman, Georgia)
  Serif,
  /// Sans-serif fonts (e.g., Arial, Helvetica)
  SansSerif,
  /// Monospace fonts (e.g., Courier, Monaco)
  Monospace,
  /// Cursive/script fonts
  Cursive,
  /// Fantasy/decorative fonts
  Fantasy,
  /// System UI font
  SystemUi,
  /// UI serif font (serif intended for UI text)
  UiSerif,
  /// UI sans-serif font (sans-serif intended for UI text)
  UiSansSerif,
  /// UI monospace font (monospace intended for UI text)
  UiMonospace,
  /// UI rounded font (sans-serif with rounded letterforms)
  UiRounded,
  /// Emoji font (colored emoji glyphs)
  Emoji,
  /// Math font (mathematical notation)
  Math,
  /// Fangsong font (Chinese font style between serif and script)
  Fangsong,
}

impl GenericFamily {
  /// Parse a generic family name from a string
  ///
  /// Returns None if the string is not a recognized generic family.
  pub fn parse(s: &str) -> Option<Self> {
    match s.to_lowercase().as_str() {
      "serif" => Some(GenericFamily::Serif),
      "sans-serif" => Some(GenericFamily::SansSerif),
      "monospace" => Some(GenericFamily::Monospace),
      "cursive" => Some(GenericFamily::Cursive),
      "fantasy" => Some(GenericFamily::Fantasy),
      "system-ui" => Some(GenericFamily::SystemUi),
      "ui-serif" => Some(GenericFamily::UiSerif),
      "ui-sans-serif" => Some(GenericFamily::UiSansSerif),
      "ui-monospace" => Some(GenericFamily::UiMonospace),
      "ui-rounded" => Some(GenericFamily::UiRounded),
      "emoji" => Some(GenericFamily::Emoji),
      "math" => Some(GenericFamily::Math),
      "fangsong" => Some(GenericFamily::Fangsong),
      _ => None,
    }
  }

  /// Get fallback font families for this generic family
  ///
  /// Returns a list of common fonts that typically implement this generic family.
  pub fn fallback_families(self) -> &'static [&'static str] {
    match self {
      GenericFamily::Serif | GenericFamily::UiSerif => &[
        "Times New Roman",
        "Times",
        "Georgia",
        "DejaVu Serif",
        "Liberation Serif",
        "Noto Serif",
        "FreeSerif",
      ],
      GenericFamily::SansSerif | GenericFamily::UiSansSerif | GenericFamily::UiRounded => &[
        "Arial",
        "Helvetica",
        "Helvetica Neue",
        "Verdana",
        "DejaVu Sans",
        "Liberation Sans",
        "Noto Sans",
        "FreeSans",
        "Roboto",
      ],
      GenericFamily::Monospace | GenericFamily::UiMonospace => &[
        "Courier New",
        "Courier",
        "Consolas",
        "Monaco",
        "DejaVu Sans Mono",
        "Liberation Mono",
        "Noto Sans Mono",
        "FreeMono",
        "SF Mono",
      ],
      GenericFamily::Cursive => &[
        "Comic Sans MS",
        "Apple Chancery",
        "Zapf Chancery",
        "URW Chancery L",
        "Bradley Hand",
      ],
      GenericFamily::Fantasy => &["Impact", "Papyrus", "Copperplate", "Luminari"],
      GenericFamily::SystemUi => &[
        ".SF NS",
        "San Francisco",
        "Segoe UI",
        "Roboto",
        "Ubuntu",
        "Cantarell",
        "DejaVu Sans",
        "Liberation Sans",
      ],
      GenericFamily::Emoji => &[
        "Apple Color Emoji",
        "Segoe UI Emoji",
        "Noto Color Emoji",
        "Twemoji",
        "EmojiOne",
        "Symbola",
      ],
      GenericFamily::Math => &[
        "Cambria Math",
        "STIX Two Math",
        "Latin Modern Math",
        "DejaVu Math TeX Gyre",
      ],
      GenericFamily::Fangsong => &["FangSong", "STFangsong", "FangSong_GB2312"],
    }
  }

  /// Returns true if resolution should try explicit fallback names before mapping to a fontdb generic.
  pub fn prefers_named_fallbacks_first(self) -> bool {
    matches!(
      self,
      GenericFamily::SystemUi
        | GenericFamily::UiSerif
        | GenericFamily::UiSansSerif
        | GenericFamily::UiMonospace
        | GenericFamily::UiRounded
        | GenericFamily::Emoji
        | GenericFamily::Math
        | GenericFamily::Fangsong
    )
  }

  /// Converts to fontdb Family for querying.
  pub fn to_fontdb(self) -> FontDbFamily<'static> {
    match self {
      Self::Serif | Self::UiSerif => FontDbFamily::Serif,
      Self::SansSerif | Self::SystemUi | Self::UiSansSerif | Self::UiRounded => {
        FontDbFamily::SansSerif
      }
      Self::Monospace | Self::UiMonospace => FontDbFamily::Monospace,
      Self::Cursive => FontDbFamily::Cursive,
      Self::Fantasy => FontDbFamily::Fantasy,
      // These don't have direct fontdb equivalents, fallback to sans-serif
      Self::Emoji | Self::Math | Self::Fangsong => FontDbFamily::SansSerif,
    }
  }
}

impl std::str::FromStr for GenericFamily {
  type Err = ();

  fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
    Self::parse(s).ok_or(())
  }
}

/// Font database
///
/// Wraps `fontdb` and provides font loading, caching, and querying.
/// System fonts are loaded automatically unless disabled via [`FontConfig`], and
/// bundled fallback fonts are used when nothing else is available.
///
/// # Thread Safety
///
/// FontDatabase uses interior mutability with RwLock for thread-safe
/// cache access. Multiple threads can query fonts concurrently.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::font_db::{FontDatabase, FontWeight, FontStyle};
///
/// // Create database (loads system fonts)
/// let db = FontDatabase::new();
///
/// // Query for a font
/// if let Some(id) = db.query("Arial", FontWeight::NORMAL, FontStyle::Normal) {
///     // Load font data
///     let font = db.load_font(id).expect("Font should load");
///     println!("Loaded font: {}", font.family);
/// }
///
/// // Use fallback chain
/// let families = vec![
///     "NonExistentFont".to_string(),
///     "Arial".to_string(),
///     "sans-serif".to_string(),
/// ];
/// if let Some(id) = db.resolve_family_list(&families, FontWeight::NORMAL, FontStyle::Normal) {
///     let font = db.load_font(id).expect("Fallback should work");
///     println!("Found fallback font: {}", font.family);
/// }
/// ```
pub struct FontDatabase {
  /// Underlying fontdb database
  db: Arc<FontDbDatabase>,
  /// Cached font data (font ID -> binary data)
  cache: RwLock<HashMap<ID, Arc<Vec<u8>>>>,
  /// Cached list of math-capable fonts (IDs with a MATH table)
  math_fonts: RwLock<Option<Vec<ID>>>,
  /// Glyph coverage cache to avoid repeated face parsing
  glyph_coverage: GlyphCoverageCache,
}

impl FontDatabase {
  /// Creates a new font database and loads system fonts
  ///
  /// This will scan the system font directories:
  /// - Windows: C:\Windows\Fonts
  /// - macOS: /Library/Fonts, /System/Library/Fonts, ~/Library/Fonts
  /// - Linux: /usr/share/fonts, ~/.fonts, ~/.local/share/fonts
  ///
  /// Defaults are driven by [`FontConfig::default()`], which honors
  /// `FASTR_USE_BUNDLED_FONTS` (and `CI`) to disable system discovery when
  /// deterministic renders are required.
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let db = FontDatabase::new();
  /// ```
  pub fn new() -> Self {
    Self::with_config(&FontConfig::default())
  }

  /// Creates a new font database using the provided configuration.
  ///
  /// Bundled fonts are always used as a final fallback to ensure text shaping
  /// works in minimal environments where no system fonts are present.
  pub fn with_config(config: &FontConfig) -> Self {
    if !config.use_system_fonts && config.use_bundled_fonts && config.font_dirs.is_empty() {
      return Self::shared_bundled();
    }

    let mut this = Self {
      db: Arc::new(FontDbDatabase::new()),
      cache: RwLock::new(HashMap::new()),
      math_fonts: RwLock::new(None),
      glyph_coverage: GlyphCoverageCache::new(GLYPH_COVERAGE_CACHE_SIZE),
    };

    if config.use_system_fonts {
      Arc::make_mut(&mut this.db).load_system_fonts();
    }

    for dir in &config.font_dirs {
      this.load_fonts_dir(dir);
    }

    if config.use_bundled_fonts {
      this.load_bundled_fonts();
    }

    if this.font_count() == 0 && !config.use_bundled_fonts {
      this.load_bundled_fonts();
    }

    this.set_generic_fallbacks();
    this
  }

  /// Creates an empty font database without loading system fonts
  ///
  /// Useful for testing or when you want to load specific fonts only.
  pub fn empty() -> Self {
    Self::with_shared_db_and_cache(Arc::new(FontDbDatabase::new()), FontCacheConfig::default())
  }

  /// Creates a new font database that reuses the shared system font list while keeping caches private.
  pub fn shared_system() -> Self {
    Self::with_shared_db_and_cache(shared_system_fontdb(), FontCacheConfig::default())
  }

  /// Returns the shared system font metadata handle.
  pub fn shared_system_db() -> Arc<FontDbDatabase> {
    shared_system_fontdb()
  }

  /// Creates a new font database that reuses the shared bundled font list while keeping caches private.
  pub fn shared_bundled() -> Self {
    Self::with_shared_bundled_db_and_cache(FontCacheConfig::default())
  }

  /// Returns the shared bundled font metadata handle.
  pub fn shared_bundled_db() -> Arc<FontDbDatabase> {
    shared_bundled_fontdb()
  }

  /// Creates a new font database that reuses the shared bundled font list with custom cache sizing.
  pub fn with_shared_bundled_db_and_cache(cache: FontCacheConfig) -> Self {
    Self::with_shared_db_and_cache(shared_bundled_fontdb(), cache)
  }

  /// Creates a new font database that shares the underlying font list with other instances
  /// while keeping caches isolated.
  pub fn with_shared_db(db: Arc<FontDbDatabase>) -> Self {
    Self::with_shared_db_and_cache(db, FontCacheConfig::default())
  }

  /// Creates a new font database with shared font metadata and custom cache sizing.
  pub fn with_shared_db_and_cache(db: Arc<FontDbDatabase>, cache: FontCacheConfig) -> Self {
    Self {
      db,
      cache: RwLock::new(HashMap::new()),
      math_fonts: RwLock::new(None),
      glyph_coverage: GlyphCoverageCache::new(cache.glyph_coverage_cache_size),
    }
  }

  /// Returns the shared font metadata backing this database.
  pub fn shared_db(&self) -> Arc<FontDbDatabase> {
    Arc::clone(&self.db)
  }

  /// Returns a new font database that reuses the same font metadata but has fresh caches.
  pub fn clone_with_shared_data(&self, cache: FontCacheConfig) -> Self {
    Self::with_shared_db_and_cache(self.shared_db(), cache)
  }

  /// Loads fonts from a directory
  ///
  /// Recursively scans the directory for font files.
  pub fn load_fonts_dir<P: AsRef<Path>>(&mut self, path: P) {
    Arc::make_mut(&mut self.db).load_fonts_dir(path);
    if let Ok(mut cached) = self.math_fonts.write() {
      *cached = None;
    }
  }

  fn load_bundled_fonts(&mut self) {
    for font in BUNDLED_FONTS {
      if let Err(err) = self.load_font_data(font.data.to_vec()) {
        eprintln!("failed to load bundled font {}: {:?}", font.name, err);
      }
    }

    if should_load_bundled_emoji_fonts() {
      for font in BUNDLED_EMOJI_FONTS {
        if let Err(err) = self.load_font_data(font.data.to_vec()) {
          eprintln!("failed to load bundled emoji font {}: {:?}", font.name, err);
        }
      }
    }
  }

  /// Recomputes the default families used for fontdb generic queries based on the currently loaded fonts.
  pub fn refresh_generic_fallbacks(&mut self) {
    self.set_generic_fallbacks();
  }
  fn set_generic_fallbacks(&mut self) {
    let faces: Vec<&fontdb::FaceInfo> = self.faces().collect();
    let Some(primary_family) = faces
      .iter()
      .find(|face| !Self::face_is_emoji_font(face) && self.face_has_basic_latin(face.id))
      .or_else(|| faces.iter().find(|face| !Self::face_is_emoji_font(face)))
      .or_else(|| faces.first())
      .and_then(|face| face.families.first().map(|(name, _)| name.clone()))
    else {
      return;
    };

    let first_matching_family = |fallbacks: &[&str]| -> Option<String> {
      for face in faces.iter().filter(|face| !Self::face_is_emoji_font(face)) {
        for (name, _) in &face.families {
          if fallbacks
            .iter()
            .any(|fallback| fallback.eq_ignore_ascii_case(name))
          {
            return Some(name.clone());
          }
        }
      }

      for face in &faces {
        for (name, _) in &face.families {
          if fallbacks
            .iter()
            .any(|fallback| fallback.eq_ignore_ascii_case(name))
          {
            return Some(name.clone());
          }
        }
      }

      None
    };

    let serif = first_matching_family(GenericFamily::Serif.fallback_families())
      .unwrap_or_else(|| primary_family.clone());
    let sans = first_matching_family(GenericFamily::SansSerif.fallback_families())
      .unwrap_or_else(|| primary_family.clone());
    let monospace = first_matching_family(GenericFamily::Monospace.fallback_families())
      .unwrap_or_else(|| primary_family.clone());
    let cursive = first_matching_family(GenericFamily::Cursive.fallback_families())
      .unwrap_or_else(|| primary_family.clone());
    let fantasy =
      first_matching_family(GenericFamily::Fantasy.fallback_families()).unwrap_or(primary_family);

    let db = Arc::make_mut(&mut self.db);
    db.set_serif_family(serif);
    db.set_sans_serif_family(sans);
    db.set_monospace_family(monospace);
    db.set_cursive_family(cursive);
    db.set_fantasy_family(fantasy);
  }

  fn face_is_emoji_font(face: &fontdb::FaceInfo) -> bool {
    face
      .families
      .iter()
      .any(|(name, _)| Self::family_name_is_emoji_font(name))
  }

  fn face_has_basic_latin(&self, id: ID) -> bool {
    self.has_glyph(id, 'A') && self.has_glyph(id, 'a')
  }

  /// Loads a font from binary data
  ///
  /// Useful for loading embedded fonts or web fonts.
  ///
  /// # Errors
  ///
  /// Returns an error if the data is not a valid font file.
  pub fn load_font_data(&mut self, data: Vec<u8>) -> Result<()> {
    // Validate the data is a valid font
    parse_face_with_counter(&data, 0).map_err(|e| FontError::InvalidFontFile {
      path: format!("(memory): {:?}", e),
    })?;

    Arc::make_mut(&mut self.db).load_font_data(data);
    if let Ok(mut cached) = self.math_fonts.write() {
      *cached = None;
    }
    Ok(())
  }

  /// Queries for a font matching the given criteria
  ///
  /// Returns the font ID of the best match, or None if no fonts match.
  /// The fontdb library handles fuzzy matching for weight and style.
  ///
  /// # Arguments
  ///
  /// * `family` - Font family name (e.g., "Arial") or generic family (e.g., "sans-serif")
  /// * `weight` - Desired font weight (100-900)
  /// * `style` - Desired font style (normal, italic, oblique)
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let db = FontDatabase::new();
  ///
  /// // Query specific font
  /// let id = db.query("Arial", FontWeight::BOLD, FontStyle::Normal);
  ///
  /// // Query generic family
  /// let id = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal);
  /// ```
  pub fn query(&self, family: &str, weight: FontWeight, style: FontStyle) -> Option<ID> {
    // Check if this is a generic family
    let families = if let Some(generic) = GenericFamily::parse(family) {
      vec![generic.to_fontdb()]
    } else {
      vec![FontDbFamily::Name(family)]
    };

    let query = FontDbQuery {
      families: &families,
      weight: fontdb::Weight(weight.0),
      style: style.into(),
      stretch: fontdb::Stretch::Normal,
    };

    self.db.query(&query)
  }

  /// Queries with weight, style, and stretch
  ///
  /// Full query with all font properties.
  pub fn query_full(
    &self,
    family: &str,
    weight: FontWeight,
    style: FontStyle,
    stretch: FontStretch,
  ) -> Option<ID> {
    let families = if let Some(generic) = GenericFamily::parse(family) {
      vec![generic.to_fontdb()]
    } else {
      vec![FontDbFamily::Name(family)]
    };

    let query = FontDbQuery {
      families: &families,
      weight: fontdb::Weight(weight.0),
      style: style.into(),
      stretch: stretch.into(),
    };

    self.db.query(&query)
  }

  /// Loads font data for a given font ID
  ///
  /// Caches the data for subsequent requests. The cached data is
  /// shared via Arc to avoid duplication.
  ///
  /// # Arguments
  ///
  /// * `id` - Font ID obtained from `query()`
  ///
  /// # Returns
  ///
  /// Returns the loaded font with its data, or None if loading fails.
  pub fn load_font(&self, id: ID) -> Option<LoadedFont> {
    let data = self.get_or_load_font_data(id)?;
    let face_info = self.db.face(id)?;

    Some(self.create_loaded_font_with_info(id, data, &face_info))
  }

  fn get_or_load_font_data(&self, id: ID) -> Option<Arc<Vec<u8>>> {
    if let Ok(cache) = self.cache.read() {
      if let Some(data) = cache.get(&id) {
        return Some(Arc::clone(data));
      }
    }

    let mut data_result: Option<Arc<Vec<u8>>> = None;
    self.db.with_face_data(id, |font_data, _face_index| {
      data_result = Some(Arc::new(font_data.to_vec()));
    });

    let data = data_result?;

    if let Ok(mut cache) = self.cache.write() {
      cache.insert(id, Arc::clone(&data));
    }

    Some(data)
  }

  /// Creates a LoadedFont from cached data
  fn create_loaded_font(&self, id: ID, data: Arc<Vec<u8>>) -> LoadedFont {
    let face_info = self.db.face(id).expect("Font should exist");
    self.create_loaded_font_with_info(id, data, &face_info)
  }

  /// Creates a LoadedFont from face info
  fn create_loaded_font_with_info(
    &self,
    id: ID,
    data: Arc<Vec<u8>>,
    face_info: &fontdb::FaceInfo,
  ) -> LoadedFont {
    LoadedFont {
      id: Some(FontId::new(id)),
      data,
      index: face_info.index,
      family: face_info
        .families
        .first()
        .map(|(name, _)| name.clone())
        .unwrap_or_else(|| "Unknown".to_string()),
      weight: FontWeight(face_info.weight.0),
      style: face_info.style.into(),
      stretch: face_info.stretch.into(),
    }
  }

  /// Resolves a font family list with fallbacks
  ///
  /// Tries each family in the list until a match is found.
  /// This implements CSS font-family fallback behavior.
  ///
  /// # Arguments
  ///
  /// * `families` - List of font families in priority order
  /// * `weight` - Desired font weight
  /// * `style` - Desired font style
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let db = FontDatabase::new();
  /// let families = vec![
  ///     "CustomFont".to_string(),  // First choice (may not exist)
  ///     "Arial".to_string(),       // Second choice
  ///     "sans-serif".to_string(),  // Final fallback
  /// ];
  /// let id = db.resolve_family_list(&families, FontWeight::NORMAL, FontStyle::Normal);
  /// ```
  pub fn resolve_family_list(
    &self,
    families: &[String],
    weight: FontWeight,
    style: FontStyle,
  ) -> Option<ID> {
    for family in families {
      if let Some(id) = self.query(family, weight, style) {
        return Some(id);
      }
    }

    // Final fallback to sans-serif
    self.query("sans-serif", weight, style)
  }

  /// Resolves a font family list with full properties
  pub fn resolve_family_list_full(
    &self,
    families: &[String],
    weight: FontWeight,
    style: FontStyle,
    stretch: FontStretch,
  ) -> Option<ID> {
    for family in families {
      if let Some(id) = self.query_full(family, weight, style, stretch) {
        return Some(id);
      }
    }

    self.query_full("sans-serif", weight, style, stretch)
  }

  /// Returns the number of fonts in the database
  #[inline]
  pub fn font_count(&self) -> usize {
    self.db.len()
  }

  /// Returns whether the database is empty
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.db.is_empty()
  }

  /// Clears the font data cache
  ///
  /// Useful to free memory when fonts are no longer needed.
  pub fn clear_cache(&self) {
    if let Ok(mut cache) = self.cache.write() {
      cache.clear();
    }
    self.glyph_coverage.clear();
    if let Ok(mut cached) = self.math_fonts.write() {
      *cached = None;
    }
  }

  /// Returns the number of cached fonts
  pub fn cache_size(&self) -> usize {
    self.cache.read().map(|c| c.len()).unwrap_or(0)
  }

  // ========================================================================
  // Glyph checking methods (for fallback chain support)
  // ========================================================================

  /// Returns the underlying fontdb database.
  ///
  /// Provides direct access for advanced queries.
  #[inline]
  pub fn inner(&self) -> &FontDbDatabase {
    &self.db
  }

  /// Returns an iterator over all font faces in the database.
  #[inline]
  pub fn faces(&self) -> impl Iterator<Item = &fontdb::FaceInfo> {
    self.db.faces()
  }

  fn cached_face(&self, id: ID) -> Option<Arc<CachedFace>> {
    let face_info = self.db.face(id)?;
    let data = self.get_or_load_font_data(id)?;

    self.glyph_coverage.get_or_put(id, || {
      face_cache::get_ttf_face_with_data(&data, face_info.index)
    })
  }

  /// Loads the first available font in the database, if any
  pub fn first_font(&self) -> Option<LoadedFont> {
    self.faces().next().and_then(|face| self.load_font(face.id))
  }

  /// Checks if a font has a glyph for the given character.
  ///
  /// This is used during fallback resolution to find a font that
  /// can render a specific character.
  pub fn has_glyph(&self, id: ID, c: char) -> bool {
    self.has_glyph_cached(id, c)
  }

  /// Checks glyph support using the cached coverage table for the font.
  pub fn has_glyph_cached(&self, id: ID, c: char) -> bool {
    self
      .cached_face(id)
      .map(|f| f.has_glyph(c))
      .unwrap_or(false)
  }

  /// Returns true when any loaded face provides a glyph for the given character.
  ///
  /// This is a convenience helper for coverage auditing tools.
  pub fn any_face_has_glyph_cached(&self, c: char) -> bool {
    self.faces().any(|face| self.has_glyph_cached(face.id, c))
  }

  /// Returns true if the font advertises any color/emoji-capable tables.
  ///
  /// Detection is table-based (COLR/CPAL, CBDT/CBLC, sbix, SVG) instead of relying on family name
  /// heuristics. Returns None if the font could not be parsed.
  pub fn is_color_capable_font(&self, id: ID) -> Option<bool> {
    self
      .cached_face(id)
      .map(|face| face_has_color_tables(face.face()))
  }

  pub(crate) fn family_name_is_emoji_font(name: &str) -> bool {
    let name_lower = name.to_lowercase();
    name_lower.contains("emoji")
      || name_lower.contains("color")
      || name_lower.contains("twemoji")
      || name_lower.contains("symbola")
      || name_lower.contains("noto color")
      || name_lower.contains("apple color")
      || name_lower.contains("segoe ui emoji")
      || name_lower.contains("segoe ui symbol")
  }

  /// Checks if a character is an emoji.
  ///
  /// Uses Unicode properties to determine if a character should be
  /// rendered with an emoji font. Delegates to the shared emoji
  /// detection logic to keep font fallback and emoji sequence parsing
  /// in sync.
  pub fn is_emoji(c: char) -> bool {
    emoji::is_emoji(c)
  }

  /// Finds emoji fonts in the database.
  ///
  /// Returns font IDs for fonts that are likely to contain emoji glyphs.
  pub fn find_emoji_fonts(&self) -> Vec<ID> {
    let mut emoji_fonts = Vec::new();

    for face in self.db.faces() {
      let is_color_font = self.is_color_capable_font(face.id);
      if matches!(is_color_font, Some(true)) {
        emoji_fonts.push(face.id);
        continue;
      }

      if is_color_font.is_none()
        && face
          .families
          .iter()
          .any(|(name, _)| Self::family_name_is_emoji_font(name))
      {
        emoji_fonts.push(face.id);
      }
    }

    emoji_fonts
  }

  /// Returns the IDs of fonts that advertise OpenType math support (MATH table present).
  pub fn find_math_fonts(&self) -> Vec<ID> {
    if let Ok(cache) = self.math_fonts.read() {
      if let Some(list) = &*cache {
        return list.clone();
      }
    }

    let mut math_fonts = Vec::new();
    for face in self.db.faces() {
      let has_math = self
        .db
        .with_face_data(face.id, |data, face_index| {
          parse_face_with_counter(data, face_index)
            .ok()
            .and_then(|f| f.tables().math)
            .is_some()
        })
        .unwrap_or(false);

      if has_math {
        math_fonts.push(face.id);
      }
    }

    if let Ok(mut cache) = self.math_fonts.write() {
      *cache = Some(math_fonts.clone());
    }

    math_fonts
  }
}

impl Default for FontDatabase {
  fn default() -> Self {
    Self::new()
  }
}

// Make FontDatabase thread-safe
unsafe impl Send for FontDatabase {}
unsafe impl Sync for FontDatabase {}

// ============================================================================
// Font Metrics
// ============================================================================

#[inline]
fn parse_face_for_metrics<'a>(data: &'a [u8], index: u32) -> Result<ttf_parser::Face<'a>> {
  Ok(
    parse_face_with_counter(data, index).map_err(|e| FontError::LoadFailed {
      family: String::new(),
      reason: format!("Failed to parse font: {:?}", e),
    })?,
  )
}

#[inline]
fn apply_face_variations(face: &mut ttf_parser::Face<'_>, variations: &[(Tag, f32)]) -> bool {
  let mut applied = false;
  for (tag, value) in variations.iter().copied() {
    if face.set_variation(tag, value).is_some() {
      applied = true;
    }
  }
  applied
}

fn extract_metrics(face: &ttf_parser::Face) -> Result<FontMetrics> {
  let units_per_em = face.units_per_em();
  let ascent = face.ascender();
  let descent = face.descender();
  let line_gap = face.line_gap();

  // CSS spec: line-height = ascent - descent + line-gap
  let line_height = ascent - descent + line_gap;

  let x_height = face.x_height();
  let cap_height = face.capital_height();

  // Underline metrics
  let (underline_position, underline_thickness) = face
    .underline_metrics()
    .map(|m| (m.position, m.thickness))
    .unwrap_or((-(units_per_em as i16) / 10, (units_per_em as i16) / 20));

  // Strikeout metrics
  let (strikeout_position, strikeout_thickness) = face
    .strikeout_metrics()
    .map(|m| (Some(m.position), Some(m.thickness)))
    .unwrap_or((None, None));

  Ok(FontMetrics {
    units_per_em,
    ascent,
    descent,
    line_gap,
    line_height,
    x_height,
    cap_height,
    underline_position,
    underline_thickness,
    strikeout_position,
    strikeout_thickness,
    is_bold: face.is_bold(),
    is_italic: face.is_italic(),
    is_monospace: face.is_monospaced(),
  })
}

/// Font metrics in font units
///
/// Contains dimensional information extracted from font tables.
/// All values are in font design units and must be scaled by font size.
///
/// # CSS Specification
///
/// These metrics are used for CSS line-height calculations:
/// - <https://www.w3.org/TR/css-inline-3/#line-height>
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::font_db::{FontDatabase, FontWeight, FontStyle, FontMetrics};
///
/// let db = FontDatabase::new();
/// if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
///     let font = db.load_font(id).unwrap();
///     let metrics = FontMetrics::from_font(&font).unwrap();
///     let scaled = metrics.scale(16.0); // 16px font
///     println!("Line height: {}px", scaled.line_height);
/// }
/// ```
#[derive(Debug, Clone)]
pub struct FontMetrics {
  /// Units per em (typically 1000 or 2048)
  pub units_per_em: u16,
  /// Ascent (distance from baseline to top, positive)
  pub ascent: i16,
  /// Descent (distance from baseline to bottom, typically negative)
  pub descent: i16,
  /// Line gap (additional spacing between lines)
  pub line_gap: i16,
  /// Calculated line height (ascent - descent + line_gap)
  pub line_height: i16,
  /// x-height (height of lowercase 'x')
  pub x_height: Option<i16>,
  /// Cap height (height of uppercase letters)
  pub cap_height: Option<i16>,
  /// Underline position (negative = below baseline)
  pub underline_position: i16,
  /// Underline thickness
  pub underline_thickness: i16,
  /// Strikeout position
  pub strikeout_position: Option<i16>,
  /// Strikeout thickness
  pub strikeout_thickness: Option<i16>,
  /// Is bold (from OS/2 table)
  pub is_bold: bool,
  /// Is italic (from OS/2 table)
  pub is_italic: bool,
  /// Is monospace
  pub is_monospace: bool,
}

impl FontMetrics {
  /// Extract metrics from a loaded font
  ///
  /// # Errors
  ///
  /// Returns an error if the font data cannot be parsed.
  pub fn from_font(font: &LoadedFont) -> Result<Self> {
    Self::from_data(&font.data, font.index)
  }

  /// Extract metrics from a loaded font with specific variation coordinates.
  pub fn from_font_with_variations(font: &LoadedFont, variations: &[(Tag, f32)]) -> Result<Self> {
    Self::from_data_with_variations(&font.data, font.index, variations)
  }

  /// Extract metrics from font data
  pub fn from_data(data: &[u8], index: u32) -> Result<Self> {
    let face = parse_face_for_metrics(data, index)?;
    Self::from_face(&face)
  }

  /// Extract metrics from font data applying variation coordinates when provided.
  pub fn from_data_with_variations(
    data: &[u8],
    index: u32,
    variations: &[(Tag, f32)],
  ) -> Result<Self> {
    let mut face = parse_face_for_metrics(data, index)?;
    if !variations.is_empty() {
      apply_face_variations(&mut face, variations);
    }
    Self::from_face(&face)
  }

  /// Extract metrics from ttf-parser Face
  pub fn from_face(face: &ttf_parser::Face) -> Result<Self> {
    extract_metrics(face)
  }

  /// Extract metrics from ttf-parser Face with variation coordinates applied.
  pub fn from_face_with_variations(
    face: &ttf_parser::Face,
    variations: &[(Tag, f32)],
  ) -> Result<Self> {
    if variations.is_empty() {
      return Self::from_face(face);
    }

    let mut face = face.clone();
    apply_face_variations(&mut face, variations);
    extract_metrics(&face)
  }

  /// Scale metrics to pixel size
  ///
  /// Converts font units to pixels for a given font size.
  pub fn scale(&self, font_size: f32) -> ScaledMetrics {
    let scale = font_size / (self.units_per_em as f32);

    ScaledMetrics {
      font_size,
      scale,
      ascent: (self.ascent as f32) * scale,
      descent: -(self.descent as f32) * scale, // Make positive
      line_gap: (self.line_gap as f32) * scale,
      line_height: (self.line_height as f32) * scale,
      x_height: self.x_height.map(|h| (h as f32) * scale),
      cap_height: self.cap_height.map(|h| (h as f32) * scale),
      underline_position: (self.underline_position as f32) * scale,
      underline_thickness: (self.underline_thickness as f32) * scale,
    }
  }

  /// Calculate normal line height for a font size
  ///
  /// CSS 'line-height: normal' uses font metrics.
  #[inline]
  pub fn normal_line_height(&self, font_size: f32) -> f32 {
    (self.line_height as f32) * font_size / (self.units_per_em as f32)
  }

  /// Returns the aspect ratio (x-height / em).
  pub fn aspect_ratio(&self) -> Option<f32> {
    self
      .x_height
      .map(|xh| xh as f32 / (self.units_per_em as f32))
      .filter(|ratio| ratio.is_finite() && *ratio > 0.0)
  }
}

/// Scaled font metrics in pixels
///
/// Pre-computed metrics for a specific font size, ready for layout.
#[derive(Debug, Clone)]
pub struct ScaledMetrics {
  /// Font size in pixels
  pub font_size: f32,
  /// Scale factor (font_size / units_per_em)
  pub scale: f32,
  /// Ascent in pixels (above baseline)
  pub ascent: f32,
  /// Descent in pixels (positive, below baseline)
  pub descent: f32,
  /// Line gap in pixels
  pub line_gap: f32,
  /// Line height in pixels
  pub line_height: f32,
  /// x-height in pixels
  pub x_height: Option<f32>,
  /// Cap height in pixels
  pub cap_height: Option<f32>,
  /// Underline position (positive = below baseline)
  pub underline_position: f32,
  /// Underline thickness
  pub underline_thickness: f32,
}

impl ScaledMetrics {
  /// Baseline offset from top of line box
  #[inline]
  pub fn baseline_offset(&self) -> f32 {
    self.ascent
  }

  /// Total height (ascent + descent)
  #[inline]
  pub fn total_height(&self) -> f32 {
    self.ascent + self.descent
  }

  /// Apply line-height factor (e.g., 1.5 for 150%)
  pub fn with_line_height_factor(&self, factor: f32) -> Self {
    Self {
      line_height: self.font_size * factor,
      ..*self
    }
  }

  /// Apply explicit line height
  pub fn with_line_height(&self, line_height: f32) -> Self {
    Self {
      line_height,
      ..*self
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_font_weight_constants() {
    assert_eq!(FontWeight::THIN.value(), 100);
    assert_eq!(FontWeight::NORMAL.value(), 400);
    assert_eq!(FontWeight::BOLD.value(), 700);
    assert_eq!(FontWeight::BLACK.value(), 900);
  }

  #[test]
  fn test_font_weight_clamping() {
    assert_eq!(FontWeight::new(0).value(), 100);
    assert_eq!(FontWeight::new(50).value(), 100);
    assert_eq!(FontWeight::new(1000).value(), 900);
    assert_eq!(FontWeight::new(500).value(), 500);
  }

  #[test]
  fn test_font_weight_default() {
    assert_eq!(FontWeight::default(), FontWeight::NORMAL);
  }

  #[test]
  fn test_font_style_default() {
    assert_eq!(FontStyle::default(), FontStyle::Normal);
  }

  #[test]
  fn test_font_stretch_default() {
    assert_eq!(FontStretch::default(), FontStretch::Normal);
  }

  #[test]
  fn test_font_stretch_percentage() {
    assert_eq!(FontStretch::Normal.to_percentage(), 100.0);
    assert_eq!(FontStretch::Condensed.to_percentage(), 75.0);
    assert_eq!(FontStretch::Expanded.to_percentage(), 125.0);
  }

  #[test]
  fn test_font_stretch_from_percentage() {
    assert_eq!(FontStretch::from_percentage(100.0), FontStretch::Normal);
    assert_eq!(FontStretch::from_percentage(75.0), FontStretch::Condensed);
    assert_eq!(FontStretch::from_percentage(125.0), FontStretch::Expanded);
  }

  #[test]
  fn test_generic_family_from_str() {
    assert_eq!(GenericFamily::parse("serif"), Some(GenericFamily::Serif));
    assert_eq!(
      GenericFamily::parse("sans-serif"),
      Some(GenericFamily::SansSerif)
    );
    assert_eq!(
      GenericFamily::parse("MONOSPACE"),
      Some(GenericFamily::Monospace)
    );
    assert_eq!(GenericFamily::parse("Arial"), None);
  }

  #[test]
  fn test_generic_family_fallbacks() {
    let fallbacks = GenericFamily::SansSerif.fallback_families();
    assert!(fallbacks.contains(&"Arial"));
    assert!(fallbacks.contains(&"Helvetica"));
  }

  #[test]
  fn test_font_database_creation() {
    let db = FontDatabase::new();
    // System should have at least some fonts
    // (may be 0 in minimal CI environments)
    // font_count() returns usize which is always >= 0
    let _ = db.font_count(); // Just verify it works
  }

  #[test]
  fn test_font_database_empty() {
    let db = FontDatabase::empty();
    assert!(db.is_empty());
    assert_eq!(db.font_count(), 0);
  }

  #[test]
  fn test_query_generic_sans_serif() {
    let db = FontDatabase::new();
    // Skip if no fonts available (CI environment)
    if db.is_empty() {
      return;
    }

    let id = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal);
    // Should find at least one sans-serif font on most systems
    if let Some(id) = id {
      let font = db.load_font(id);
      assert!(font.is_some());
      let font = font.unwrap();
      assert!(!font.data.is_empty());
    }
  }

  #[test]
  fn test_query_generic_serif() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    let id = db.query("serif", FontWeight::NORMAL, FontStyle::Normal);
    if let Some(id) = id {
      let font = db.load_font(id);
      assert!(font.is_some());
    }
  }

  #[test]
  fn test_query_generic_monospace() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    let id = db.query("monospace", FontWeight::NORMAL, FontStyle::Normal);
    if let Some(id) = id {
      let font = db.load_font(id);
      assert!(font.is_some());
    }
  }

  #[test]
  fn test_fallback_chain() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    let families = vec![
      "NonExistentFontThatShouldNotExist12345".to_string(),
      "AnotherNonExistentFont67890".to_string(),
      "sans-serif".to_string(),
    ];

    let id = db.resolve_family_list(&families, FontWeight::NORMAL, FontStyle::Normal);
    // Should fall back to sans-serif
    if let Some(id) = id {
      let font = db.load_font(id);
      assert!(font.is_some());
    }
  }

  #[test]
  fn test_font_caching() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    let id = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal);
    if let Some(id) = id {
      // First load - not cached
      assert_eq!(db.cache_size(), 0);

      // Load font
      let font1 = db.load_font(id);
      assert!(font1.is_some());

      // Should be cached now
      assert_eq!(db.cache_size(), 1);

      // Load again - should use cache
      let font2 = db.load_font(id);
      assert!(font2.is_some());

      // Same data (Arc pointing to same allocation)
      let font1 = font1.unwrap();
      let font2 = font2.unwrap();
      assert!(Arc::ptr_eq(&font1.data, &font2.data));
    }
  }

  #[test]
  fn test_clear_cache() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    let id = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal);
    if let Some(id) = id {
      let _font = db.load_font(id);
      assert!(db.cache_size() > 0);

      db.clear_cache();
      assert_eq!(db.cache_size(), 0);
    }
  }

  #[test]
  fn test_loaded_font_properties() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
      let font = db.load_font(id).unwrap();

      // Should have non-empty data
      assert!(!font.data.is_empty());

      // Should have a family name
      assert!(!font.family.is_empty());

      // Weight should be reasonable
      assert!(font.weight.value() >= 100 && font.weight.value() <= 900);
    }
  }

  #[test]
  fn test_query_with_different_weights() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    // Query for different weights - fontdb does fuzzy matching
    let weights = [
      FontWeight::THIN,
      FontWeight::NORMAL,
      FontWeight::BOLD,
      FontWeight::BLACK,
    ];

    for weight in &weights {
      let id = db.query("sans-serif", *weight, FontStyle::Normal);
      // Should find something for each weight (may be same font with fuzzy matching)
      if let Some(id) = id {
        let font = db.load_font(id);
        assert!(font.is_some());
      }
    }
  }

  #[test]
  fn test_query_with_different_styles() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    let styles = [FontStyle::Normal, FontStyle::Italic, FontStyle::Oblique];

    for style in &styles {
      let id = db.query("sans-serif", FontWeight::NORMAL, *style);
      // Should find something for each style (may use fallback)
      if let Some(id) = id {
        let font = db.load_font(id);
        assert!(font.is_some());
      }
    }
  }

  // ========================================================================
  // Font Metrics Tests
  // ========================================================================

  #[test]
  fn test_font_metrics_extraction() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
      let font = db.load_font(id).unwrap();
      let metrics = font.metrics().expect("Should extract metrics");

      assert!(metrics.units_per_em > 0);
      assert!(metrics.ascent > 0);
      assert!(metrics.descent < 0); // Descent is typically negative
      assert!(metrics.line_height > 0);
    }
  }

  #[test]
  fn test_scaled_metrics() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
      let font = db.load_font(id).unwrap();
      let metrics = font.metrics().expect("Should extract metrics");
      let scaled = metrics.scale(16.0);

      assert_eq!(scaled.font_size, 16.0);
      assert!(scaled.ascent > 0.0);
      assert!(scaled.descent > 0.0); // Scaled descent is positive
      assert!(scaled.line_height > 0.0);
    }
  }

  #[test]
  fn test_scaled_metrics_total_height() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
      let font = db.load_font(id).unwrap();
      let metrics = font.metrics().expect("Should extract metrics");
      let scaled = metrics.scale(16.0);

      // Total height should be reasonable for 16px font
      let total = scaled.total_height();
      assert!(total > 10.0 && total < 30.0);
    }
  }

  #[test]
  fn test_scaled_metrics_baseline_offset() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
      let font = db.load_font(id).unwrap();
      let metrics = font.metrics().expect("Should extract metrics");
      let scaled = metrics.scale(16.0);

      // Baseline offset equals ascent
      assert_eq!(scaled.baseline_offset(), scaled.ascent);
    }
  }

  #[test]
  fn test_scaled_metrics_with_line_height_factor() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
      let font = db.load_font(id).unwrap();
      let metrics = font.metrics().expect("Should extract metrics");
      let scaled = metrics.scale(16.0);
      let with_factor = scaled.with_line_height_factor(1.5);

      assert_eq!(with_factor.line_height, 24.0); // 16 * 1.5
      assert_eq!(with_factor.font_size, 16.0); // Unchanged
    }
  }

  #[test]
  fn test_normal_line_height() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
      let font = db.load_font(id).unwrap();
      let metrics = font.metrics().expect("Should extract metrics");

      let normal_lh = metrics.normal_line_height(16.0);
      assert!(normal_lh > 0.0);
      // Normal line height is usually >= font size
      assert!(normal_lh >= 14.0 && normal_lh < 32.0);
    }
  }

  #[test]
  fn test_loaded_font_as_ttf_face() {
    let db = FontDatabase::new();
    if db.is_empty() {
      return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
      let font = db.load_font(id).unwrap();
      let face = font.as_ttf_face().expect("Should parse font");

      // Verify we can access the face
      assert!(face.units_per_em() > 0);
    }
  }

  #[test]
  fn variable_font_metrics_apply_variations_without_panic() {
    let data = include_bytes!("../../tests/fonts/RobotoFlex-VF.ttf");
    let mut face = parse_face_for_metrics(data, 0).expect("Roboto Flex should parse");
    assert!(face.is_variable());
    assert!(!face.has_non_default_variation_coordinates());

    let coords = [
      (Tag::from_bytes(b"wght"), 720.0),
      (Tag::from_bytes(b"wdth"), 95.0),
    ];
    let applied = apply_face_variations(&mut face, &coords);
    assert!(
      applied,
      "Expected at least one variation axis to be applied"
    );
    assert!(face.has_non_default_variation_coordinates());
    assert!(face.variation_coordinates().iter().any(|c| c.get() != 0));

    let metrics_from_face = FontMetrics::from_face(&face).unwrap();
    let metrics_with_vars = FontMetrics::from_face_with_variations(&face, &coords).unwrap();
    assert_eq!(
      metrics_from_face.underline_position,
      metrics_with_vars.underline_position
    );

    let metrics_from_data = FontMetrics::from_data_with_variations(data, 0, &coords).unwrap();
    assert_eq!(
      metrics_with_vars.underline_thickness,
      metrics_from_data.underline_thickness
    );
  }

  #[test]
  fn has_glyph_cached_is_deterministic() {
    let db = FontDatabase::new();
    let Some(face) = db.faces().next() else {
      return;
    };

    let expected = db
      .inner()
      .with_face_data(face.id, |data, index| {
        parse_face_with_counter(data, index).ok().map(|f| {
          (
            f.glyph_index('A').is_some(),
            f.glyph_index(char::MAX).is_some(),
          )
        })
      })
      .unwrap_or(None);

    let Some((has_a, has_max)) = expected else {
      return;
    };

    for _ in 0..4 {
      assert_eq!(db.has_glyph_cached(face.id, 'A'), has_a);
      assert_eq!(db.has_glyph_cached(face.id, char::MAX), has_max);
    }
  }
}
