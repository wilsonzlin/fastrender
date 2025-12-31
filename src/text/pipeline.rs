//! Text Shaping Pipeline
//!
//! Coordinates bidi analysis, script itemization, and text shaping into a unified process.
//!
//! # Architecture
//!
//! The shaping pipeline processes text through multiple stages:
//!
//! ```text
//! Input Text → Bidi Analysis → Script Itemization → Font Matching → Shaping → Output
//! ```
//!
//! Each stage handles a specific aspect of text processing:
//!
//! 1. **Bidi Analysis**: Determines text direction (LTR/RTL) using UAX #9
//! 2. **Script Itemization**: Splits text into runs of the same script
//! 3. **Font Matching**: Assigns fonts to each run with fallback support
//! 4. **Shaping**: Converts characters to positioned glyphs using HarfBuzz
//!
//! # Example
//!
//! ```rust,no_run
//! # use fastrender::text::pipeline::{Direction, ShapingPipeline};
//! # use fastrender::text::FontContext;
//! # use fastrender::ComputedStyle;
//! # fn main() -> fastrender::Result<()> {
//! let mut pipeline = ShapingPipeline::new();
//! let font_context = FontContext::new();
//! let style = ComputedStyle::default();
//!
//! let shaped_runs = pipeline.shape("Hello, world!", &style, &font_context)?;
//! for run in shaped_runs {
//!     println!("Run: {} glyphs, {}px advance", run.glyphs.len(), run.advance);
//! }
//! # Ok(())
//! # }
//! ```
//!
//! # Unicode Compliance
//!
//! This implementation aims to support:
//! - **UAX #9**: Unicode Bidirectional Algorithm
//! - **UAX #24**: Script Property
//! - **OpenType GSUB/GPOS**: Glyph substitution and positioning
//!
//! # References
//!
//! - Unicode Bidirectional Algorithm (UAX #9): <https://www.unicode.org/reports/tr9/>
//! - HarfBuzz documentation: <https://harfbuzz.github.io/>
//! - rustybuzz documentation: <https://docs.rs/rustybuzz/>

use crate::css::types::FontPaletteBase;
use crate::error::Result;
use crate::error::TextError;
use crate::style::color::Rgba;
use crate::style::font_palette::resolve_font_palette_for_font;
use crate::style::types::Direction as CssDirection;
use crate::style::types::EastAsianVariant;
use crate::style::types::EastAsianWidth;
use crate::style::types::FontKerning;
use crate::style::types::FontLanguageOverride;
use crate::style::types::FontPalette;
use crate::style::types::FontSizeAdjust;
use crate::style::types::FontStyle as CssFontStyle;
use crate::style::types::FontVariant;
use crate::style::types::FontVariantCaps;
use crate::style::types::FontVariantEmoji;
use crate::style::types::FontVariantPosition;
use crate::style::types::NumericFigure;
use crate::style::types::NumericFraction;
use crate::style::types::NumericSpacing;
use crate::style::ComputedStyle;
use crate::text::color_fonts::select_cpal_palette;
use crate::text::emoji;
use crate::text::font_db::FontDatabase;
use crate::text::font_db::FontStretch as DbFontStretch;
use crate::text::font_db::FontStyle;
use crate::text::font_db::LoadedFont;
use crate::text::font_fallback::families_signature;
use crate::text::font_fallback::family_name_signature;
use crate::text::font_fallback::ClusterFallbackCacheKey;
use crate::text::font_fallback::EmojiPreference;
use crate::text::font_fallback::FallbackCache;
use crate::text::font_fallback::FallbackCacheDescriptor;
use crate::text::font_fallback::FallbackCacheStatsSnapshot;
use crate::text::font_fallback::GlyphFallbackCacheKey;
use crate::text::font_loader::FontContext;
use crate::text::script_fallback;
use lru::LruCache;
use rustc_hash::FxHasher;
use rustybuzz::Direction as HbDirection;
use rustybuzz::Feature;
use rustybuzz::Language as HbLanguage;
use rustybuzz::UnicodeBuffer;
use rustybuzz::Variation;
use std::borrow::Cow;
use std::collections::HashSet;
use std::hash::BuildHasherDefault;
use std::num::NonZeroUsize;
use std::str::FromStr;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::time::Instant;
use ttf_parser::Tag;
use unicode_bidi::BidiInfo;
use unicode_bidi::Level;
use unicode_bidi_mirroring::get_mirrored;
use unicode_general_category::{get_general_category, GeneralCategory};
use unicode_segmentation::UnicodeSegmentation;
use unicode_vo::char_orientation;
use unicode_vo::Orientation as VerticalOrientation;

pub(crate) const DEFAULT_OBLIQUE_ANGLE_DEG: f32 = 14.0;
const SHAPING_CACHE_CAPACITY: usize = 2048;
const FONT_RESOLUTION_CACHE_SIZE: usize = 2048;
#[cfg(any(test, debug_assertions))]
static SHAPE_FONT_RUN_INVOCATIONS: AtomicUsize = AtomicUsize::new(0);

type ShapingCacheHasher = BuildHasherDefault<FxHasher>;

#[derive(Debug, Default, Clone, Copy)]
pub struct TextCacheStats {
  pub hits: u64,
  pub misses: u64,
  pub evictions: u64,
  pub bytes: usize,
}

#[derive(Debug, Default, Clone)]
pub struct TextDiagnostics {
  pub shape_ms: f64,
  pub coverage_ms: f64,
  pub rasterize_ms: f64,
  pub shaped_runs: usize,
  pub glyphs: usize,
  pub color_glyph_rasters: usize,
  pub fallback_cache_hits: usize,
  pub fallback_cache_misses: usize,
  pub last_resort_fallbacks: usize,
  pub last_resort_samples: Vec<String>,
  pub glyph_cache: TextCacheStats,
  pub color_glyph_cache: TextCacheStats,
}

static TEXT_DIAGNOSTICS: OnceLock<Mutex<TextDiagnostics>> = OnceLock::new();
static TEXT_DIAGNOSTICS_ENABLED: AtomicBool = AtomicBool::new(false);
static LAST_RESORT_LOGGED: AtomicUsize = AtomicUsize::new(0);
static SHAPING_FALLBACK_LOGGED: AtomicUsize = AtomicUsize::new(0);

const LAST_RESORT_SAMPLE_LIMIT: usize = 8;

fn diagnostics_cell() -> &'static Mutex<TextDiagnostics> {
  TEXT_DIAGNOSTICS.get_or_init(|| Mutex::new(TextDiagnostics::default()))
}

pub(crate) fn enable_text_diagnostics() {
  TEXT_DIAGNOSTICS_ENABLED.store(true, Ordering::Release);
  if let Ok(mut diag) = diagnostics_cell().lock() {
    *diag = TextDiagnostics::default();
  }
}

pub(crate) fn take_text_diagnostics() -> Option<TextDiagnostics> {
  if !TEXT_DIAGNOSTICS_ENABLED.swap(false, Ordering::AcqRel) {
    return None;
  }

  diagnostics_cell().lock().ok().map(|diag| diag.clone())
}

pub(crate) fn text_diagnostics_enabled() -> bool {
  TEXT_DIAGNOSTICS_ENABLED.load(Ordering::Acquire)
}

pub(crate) fn text_diagnostics_timer() -> Option<Instant> {
  text_diagnostics_enabled().then(Instant::now)
}

fn with_text_diagnostics(f: impl FnOnce(&mut TextDiagnostics)) {
  if !TEXT_DIAGNOSTICS_ENABLED.load(Ordering::Acquire) {
    return;
  }

  if let Ok(mut diag) = diagnostics_cell().lock() {
    f(&mut diag);
  }
}

fn text_diagnostics_verbose_logging() -> bool {
  static VERBOSE: OnceLock<bool> = OnceLock::new();
  *VERBOSE.get_or_init(|| {
    std::env::var("FASTR_TEXT_DIAGNOSTICS")
      .map(|value| {
        let value = value.to_ascii_lowercase();
        matches!(
          value.as_str(),
          "verbose" | "2" | "debug" | "true" | "1" | "on"
        )
      })
      .unwrap_or(false)
  })
}

fn format_codepoints_for_log(text: &str) -> String {
  use std::fmt::Write;

  let mut out = String::new();
  let mut added = 0usize;
  for ch in text.chars() {
    if added >= LAST_RESORT_SAMPLE_LIMIT {
      out.push_str(" …");
      break;
    }
    if added > 0 {
      out.push(' ');
    }
    let _ = write!(&mut out, "U+{:04X}", ch as u32);
    added += 1;
  }
  out
}

fn record_last_resort_fallback(cluster_text: &str) {
  with_text_diagnostics(|diag| {
    diag.last_resort_fallbacks = diag.last_resort_fallbacks.saturating_add(1);
    if diag.last_resort_samples.len() < LAST_RESORT_SAMPLE_LIMIT {
      diag
        .last_resort_samples
        .push(format_codepoints_for_log(cluster_text));
    }
  });

  if text_diagnostics_verbose_logging() {
    let idx = LAST_RESORT_LOGGED.fetch_add(1, Ordering::Relaxed);
    if idx < LAST_RESORT_SAMPLE_LIMIT {
      eprintln!(
        "FASTR_TEXT_DIAGNOSTICS: last-resort font fallback for cluster {}",
        format_codepoints_for_log(cluster_text)
      );
    }
  }
}

pub(crate) fn record_text_shape(start: Option<Instant>, shaped_runs: usize, glyphs: usize) {
  if let Some(start) = start {
    let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;
    with_text_diagnostics(|diag| {
      diag.shape_ms += elapsed_ms;
      diag.shaped_runs += shaped_runs;
      diag.glyphs += glyphs;
    });
  }
}

pub(crate) fn record_text_coverage(start: Option<Instant>) {
  if let Some(start) = start {
    let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;
    with_text_diagnostics(|diag| {
      diag.coverage_ms += elapsed_ms;
    });
  }
}

pub(crate) fn record_fallback_cache_stats_delta(
  before: FallbackCacheStatsSnapshot,
  after: FallbackCacheStatsSnapshot,
) {
  with_text_diagnostics(|diag| {
    let hit_delta = after
      .cluster_hits
      .saturating_sub(before.cluster_hits)
      .saturating_add(after.glyph_hits.saturating_sub(before.glyph_hits));
    let miss_delta = after
      .cluster_misses
      .saturating_sub(before.cluster_misses)
      .saturating_add(after.glyph_misses.saturating_sub(before.glyph_misses));
    diag.fallback_cache_hits = diag.fallback_cache_hits.saturating_add(hit_delta as usize);
    diag.fallback_cache_misses = diag
      .fallback_cache_misses
      .saturating_add(miss_delta as usize);
  });
}

pub(crate) fn record_text_rasterize(
  start: Option<Instant>,
  color_glyph_rasters: usize,
  cache: TextCacheStats,
  color_cache: TextCacheStats,
) {
  if start.is_none()
    && color_glyph_rasters == 0
    && cache.hits == 0
    && cache.misses == 0
    && color_cache.hits == 0
    && color_cache.misses == 0
  {
    return;
  }
  with_text_diagnostics(|diag| {
    if let Some(start) = start {
      diag.rasterize_ms += start.elapsed().as_secs_f64() * 1000.0;
    }
    diag.color_glyph_rasters += color_glyph_rasters;
    diag.glyph_cache.hits += cache.hits;
    diag.glyph_cache.misses += cache.misses;
    diag.glyph_cache.evictions += cache.evictions;
    diag.glyph_cache.bytes = diag.glyph_cache.bytes.max(cache.bytes);
    diag.color_glyph_cache.hits += color_cache.hits;
    diag.color_glyph_cache.misses += color_cache.misses;
    diag.color_glyph_cache.evictions += color_cache.evictions;
    diag.color_glyph_cache.bytes = diag.color_glyph_cache.bytes.max(color_cache.bytes);
  });
}

// ============================================================================
// Core Types
// ============================================================================

/// Text direction for layout and rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Direction {
  /// Left-to-right text (English, most European languages)
  #[default]
  LeftToRight,
  /// Right-to-left text (Arabic, Hebrew)
  RightToLeft,
}

fn has_native_small_caps(style: &ComputedStyle, font_context: &FontContext) -> bool {
  let needs_c2sc = matches!(style.font_variant_caps, FontVariantCaps::AllSmallCaps);
  if let Some(font) = font_context.get_font_full(
    &style.font_family,
    style.font_weight.to_u16(),
    match style.font_style {
      CssFontStyle::Normal => FontStyle::Normal,
      CssFontStyle::Italic => FontStyle::Italic,
      CssFontStyle::Oblique(_) => FontStyle::Oblique,
    },
    DbFontStretch::from_percentage(style.font_stretch.to_percentage()),
  ) {
    let has_smcp = font_context.supports_feature(&font, *b"smcp");
    let has_c2sc = !needs_c2sc || font_context.supports_feature(&font, *b"c2sc");
    return has_smcp && has_c2sc;
  }

  false
}

impl Direction {
  /// Creates direction from a bidi level.
  ///
  /// Even levels (0, 2, 4...) are LTR, odd levels (1, 3, 5...) are RTL.
  #[inline]
  pub fn from_level(level: Level) -> Self {
    if level.is_ltr() {
      Self::LeftToRight
    } else {
      Self::RightToLeft
    }
  }

  /// Converts to rustybuzz Direction.
  #[inline]
  pub fn to_harfbuzz(self) -> HbDirection {
    match self {
      Self::LeftToRight => HbDirection::LeftToRight,
      Self::RightToLeft => HbDirection::RightToLeft,
    }
  }

  /// Returns true if this is left-to-right.
  #[inline]
  pub fn is_ltr(self) -> bool {
    matches!(self, Self::LeftToRight)
  }

  /// Returns true if this is right-to-left.
  #[inline]
  pub fn is_rtl(self) -> bool {
    matches!(self, Self::RightToLeft)
  }
}

/// Unicode script category for text itemization.
///
/// Based on Unicode Standard Annex #24 (Script Property).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Script {
  /// Common script (punctuation, numbers, symbols)
  Common,
  /// Inherited script (combining marks)
  Inherited,
  /// Unknown/unassigned script
  Unknown,
  /// Latin script (English, most European languages)
  #[default]
  Latin,
  /// Arabic script
  Arabic,
  /// Syriac script
  Syriac,
  /// Thaana script
  Thaana,
  /// N'Ko script
  Nko,
  /// Hebrew script
  Hebrew,
  /// Greek script
  Greek,
  /// Cyrillic script (Russian, etc.)
  Cyrillic,
  /// Devanagari script (Hindi, Sanskrit)
  Devanagari,
  /// Bengali script
  Bengali,
  /// Tamil script
  Tamil,
  /// Thai script
  Thai,
  /// Javanese script
  Javanese,
  /// Han script (Chinese, Japanese kanji, Korean hanja)
  Han,
  /// Hiragana script (Japanese)
  Hiragana,
  /// Katakana script (Japanese)
  Katakana,
  /// Hangul script (Korean)
  Hangul,
}

impl Script {
  /// Detects the script of a character.
  ///
  /// Uses Unicode character ranges for script detection.
  pub fn detect(c: char) -> Self {
    let cp = c as u32;

    // Combining marks should never trigger a script run split. Itemization will
    // treat them as inheriting the surrounding script, which keeps extended
    // grapheme clusters intact even when a mark lives in an "unexpected"
    // Unicode block (seen in real-world pagesets).
    if matches!(
      get_general_category(c),
      GeneralCategory::NonspacingMark
        | GeneralCategory::SpacingMark
        | GeneralCategory::EnclosingMark
    ) {
      return Self::Inherited;
    }

    // ASCII and Basic Latin
    if (0x0000..=0x007f).contains(&cp) {
      if c.is_ascii_alphabetic() {
        return Self::Latin;
      }
      return Self::Common;
    }

    // Latin Extended
    if (0x0080..=0x024f).contains(&cp) || (0x1e00..=0x1eff).contains(&cp) {
      return Self::Latin;
    }

    // Greek
    if (0x0370..=0x03ff).contains(&cp) || (0x1f00..=0x1fff).contains(&cp) {
      return Self::Greek;
    }

    // Cyrillic
    if (0x0400..=0x04ff).contains(&cp)
      || (0x0500..=0x052f).contains(&cp)
      || (0x2de0..=0x2dff).contains(&cp)
      || (0xa640..=0xa69f).contains(&cp)
    {
      return Self::Cyrillic;
    }

    // Hebrew
    if (0x0590..=0x05ff).contains(&cp) || (0xfb1d..=0xfb4f).contains(&cp) {
      return Self::Hebrew;
    }

    // Arabic
    if (0x0600..=0x06ff).contains(&cp)
      || (0x0750..=0x077f).contains(&cp)
      || (0x08a0..=0x08ff).contains(&cp)
      || (0xfb50..=0xfdff).contains(&cp)
      || (0xfe70..=0xfeff).contains(&cp)
    {
      return Self::Arabic;
    }

    // Syriac
    if (0x0700..=0x074f).contains(&cp) {
      return Self::Syriac;
    }

    // Thaana
    if (0x0780..=0x07bf).contains(&cp) {
      return Self::Thaana;
    }

    // N'Ko
    if (0x07c0..=0x07ff).contains(&cp) {
      return Self::Nko;
    }

    // Devanagari
    if (0x0900..=0x097f).contains(&cp) || (0xa8e0..=0xa8ff).contains(&cp) {
      return Self::Devanagari;
    }

    // Bengali
    if (0x0980..=0x09ff).contains(&cp) {
      return Self::Bengali;
    }

    // Tamil
    if (0x0b80..=0x0bff).contains(&cp) {
      return Self::Tamil;
    }

    // Thai
    if (0x0e00..=0x0e7f).contains(&cp) {
      return Self::Thai;
    }

    // Javanese
    if (0xa980..=0xa9df).contains(&cp) {
      return Self::Javanese;
    }

    // Hangul (Korean)
    if (0x1100..=0x11ff).contains(&cp)
      || (0x3130..=0x318f).contains(&cp)
      || (0xa960..=0xa97f).contains(&cp)
      || (0xac00..=0xd7af).contains(&cp)
      || (0xd7b0..=0xd7ff).contains(&cp)
    {
      return Self::Hangul;
    }

    // Hiragana
    if (0x3040..=0x309f).contains(&cp) {
      return Self::Hiragana;
    }

    // Katakana
    if (0x30a0..=0x30ff).contains(&cp) || (0x31f0..=0x31ff).contains(&cp) {
      return Self::Katakana;
    }

    // CJK (Han)
    if (0x4e00..=0x9fff).contains(&cp)
      || (0x3400..=0x4dbf).contains(&cp)
      || (0x20000..=0x2a6df).contains(&cp)
      || (0x2a700..=0x2b73f).contains(&cp)
      || (0x2b740..=0x2b81f).contains(&cp)
      || (0x2b820..=0x2ceaf).contains(&cp)
      || (0x2ceb0..=0x2ebef).contains(&cp)
      || (0x30000..=0x3134f).contains(&cp)
      || (0xf900..=0xfaff).contains(&cp)
      || (0x2f800..=0x2fa1f).contains(&cp)
    {
      return Self::Han;
    }

    // General punctuation, symbols, numbers
    if (0x2000..=0x206f).contains(&cp)
      || (0x2070..=0x209f).contains(&cp)
      || (0x20a0..=0x20cf).contains(&cp)
      || (0x2100..=0x214f).contains(&cp)
    {
      return Self::Common;
    }

    Self::Unknown
  }

  /// Returns true if this script can merge with any script.
  ///
  /// Common and Inherited scripts can merge with surrounding scripts.
  #[inline]
  pub fn is_neutral(self) -> bool {
    matches!(self, Self::Common | Self::Inherited | Self::Unknown)
  }

  /// Detects the dominant script of a text string.
  ///
  /// Neutral codepoints are ignored; if no strong script is found,
  /// Latin is returned by default.
  pub fn detect_text(text: &str) -> Self {
    use std::collections::HashMap;

    let mut counts: HashMap<Script, usize> = HashMap::new();
    for ch in text.chars() {
      let script = Script::detect(ch);
      if script.is_neutral() {
        continue;
      }
      *counts.entry(script).or_insert(0) += 1;
    }

    counts
      .into_iter()
      .max_by_key(|(_, count)| *count)
      .map(|(script, _)| script)
      .unwrap_or(Script::Latin)
  }

  /// Converts to rustybuzz Script using ISO 15924 tags.
  ///
  /// Returns None for scripts that should be auto-detected.
  pub fn to_harfbuzz(self) -> Option<rustybuzz::Script> {
    // Use ISO 15924 4-letter tags
    let tag: Option<[u8; 4]> = match self {
      Self::Latin => Some(*b"Latn"),
      Self::Arabic => Some(*b"Arab"),
      Self::Syriac => Some(*b"Syrc"),
      Self::Thaana => Some(*b"Thaa"),
      Self::Nko => Some(*b"Nkoo"),
      Self::Hebrew => Some(*b"Hebr"),
      Self::Greek => Some(*b"Grek"),
      Self::Cyrillic => Some(*b"Cyrl"),
      Self::Devanagari => Some(*b"Deva"),
      Self::Bengali => Some(*b"Beng"),
      Self::Tamil => Some(*b"Taml"),
      Self::Thai => Some(*b"Thai"),
      Self::Javanese => Some(*b"Java"),
      Self::Han => Some(*b"Hani"),
      Self::Hiragana => Some(*b"Hira"),
      Self::Katakana => Some(*b"Kana"),
      Self::Hangul => Some(*b"Hang"),
      Self::Common | Self::Inherited | Self::Unknown => None,
    };

    tag.and_then(|t| {
      let tag = rustybuzz::ttf_parser::Tag::from_bytes(&t);
      rustybuzz::Script::from_iso15924_tag(tag)
    })
  }
}

// ============================================================================
// Bidi Analysis
// ============================================================================

/// Explicit bidi context passed in by layout when `unicode-bidi` establishes
/// embedding/override/isolation scopes that should affect the entire text run
/// without injecting control characters.
#[derive(Clone, Copy, Debug)]
pub struct ExplicitBidiContext {
  pub level: Level,
  pub override_all: bool,
}

/// Result of bidirectional text analysis.
///
/// Contains the bidi levels for each character and metadata about
/// whether reordering is needed.
#[derive(Debug)]
pub struct BidiAnalysis {
  /// The original text being analyzed (reserved for future RTL improvements).
  text: String,
  /// Bidi levels for each byte position (matching BidiInfo).
  levels: Vec<Level>,
  /// Paragraph boundaries in byte offsets with their base level.
  paragraphs: Vec<ParagraphBoundary>,
  /// Base paragraph level.
  base_level: Level,
  /// Whether the text contains RTL content requiring reordering.
  needs_reordering: bool,
}

/// Paragraph boundaries derived from bidi analysis.
#[derive(Debug, Clone, Copy)]
pub struct ParagraphBoundary {
  pub start_byte: usize,
  pub end_byte: usize,
  pub level: Level,
}

/// A directional run produced by bidi analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BidiRun {
  /// Start byte offset in the original text.
  pub start: usize,
  /// End byte offset (exclusive) in the original text.
  pub end: usize,
  /// Bidi embedding level for this run.
  pub level: u8,
  /// Direction of the run derived from the level.
  pub direction: Direction,
}

impl BidiRun {
  /// Returns true if the run covers no bytes.
  pub fn is_empty(&self) -> bool {
    self.start >= self.end
  }

  /// Returns the substring covered by this run.
  pub fn text_slice<'a>(&self, text: &'a str) -> &'a str {
    &text[self.start.min(text.len())..self.end.min(text.len())]
  }
}

impl BidiAnalysis {
  /// Analyzes text for bidirectional properties.
  ///
  /// Uses the Unicode Bidirectional Algorithm (UAX #9) to determine
  /// text direction at each position.
  ///
  /// # Arguments
  ///
  /// * `text` - The text to analyze
  /// * `style` - ComputedStyle containing CSS direction property
  pub fn analyze(text: &str, style: &ComputedStyle) -> Self {
    let base_direction = match style.direction {
      CssDirection::Ltr => Direction::LeftToRight,
      CssDirection::Rtl => Direction::RightToLeft,
    };
    Self::analyze_with_base(text, style, base_direction, None)
  }

  /// Analyzes text for bidirectional properties with an explicit base direction.
  ///
  /// This mirrors CSS paragraph base resolution while allowing callers (e.g. layout)
  /// to supply the containing block's resolved direction when it differs from the
  /// style value.
  pub fn analyze_with_base(
    text: &str,
    style: &ComputedStyle,
    base_direction: Direction,
    explicit: Option<ExplicitBidiContext>,
  ) -> Self {
    // Determine base direction from CSS direction property (inherited, initial LTR)
    let mut base_level = match base_direction {
      Direction::LeftToRight => Level::ltr(),
      Direction::RightToLeft => Level::rtl(),
    };
    let override_all = if let Some(ctx) = explicit {
      base_level = ctx.level;
      ctx.override_all
    } else {
      false
    };

    // Handle empty text
    if text.is_empty() {
      return Self {
        text: String::new(),
        levels: Vec::new(),
        paragraphs: Vec::new(),
        base_level,
        needs_reordering: false,
      };
    }

    // Run Unicode bidi algorithm
    let base_override = match style.unicode_bidi {
      crate::style::types::UnicodeBidi::Plaintext => None,
      _ => Some(base_level),
    };
    let bidi_info = BidiInfo::new(text, base_override);

    // Check if any RTL content exists
    let mut needs_reordering = bidi_info.levels.iter().any(|&level| level.is_rtl());

    // Clone owned text for storage
    let text = text.to_string();
    let mut levels = bidi_info.levels.clone();
    let para_level = bidi_info
      .paragraphs
      .first()
      .map(|p| p.level)
      .unwrap_or(base_level);
    let base_level = match style.unicode_bidi {
      crate::style::types::UnicodeBidi::Plaintext => para_level,
      _ => base_level,
    };

    // CSS bidi overrides force all characters to the element's direction.
    use crate::style::types::UnicodeBidi;
    if override_all
      || matches!(
        style.unicode_bidi,
        UnicodeBidi::BidiOverride | UnicodeBidi::IsolateOverride
      )
    {
      levels = vec![base_level; levels.len()];
      needs_reordering = false;
    }

    let text_len = text.len();
    let paragraphs = bidi_info
      .paragraphs
      .iter()
      .map(|p| ParagraphBoundary {
        start_byte: p.range.start.min(text_len),
        end_byte: p.range.end.min(text_len),
        level: p.level,
      })
      .collect();

    Self {
      text,
      levels,
      paragraphs,
      base_level,
      needs_reordering,
    }
  }

  /// Returns true if reordering is needed for display.
  #[inline]
  pub fn needs_reordering(&self) -> bool {
    self.needs_reordering
  }

  /// Gets the bidi level at a byte index.
  #[inline]
  pub fn level_at(&self, index: usize) -> Level {
    if self.levels.is_empty() {
      return self.base_level;
    }
    let idx = index.min(self.levels.len().saturating_sub(1));
    self.levels.get(idx).copied().unwrap_or(self.base_level)
  }

  /// Gets the direction at a byte index.
  #[inline]
  pub fn direction_at(&self, index: usize) -> Direction {
    Direction::from_level(self.level_at(index))
  }

  /// Returns the base paragraph level.
  #[inline]
  pub fn base_level(&self) -> Level {
    self.base_level
  }

  /// Returns the base direction.
  #[inline]
  pub fn base_direction(&self) -> Direction {
    Direction::from_level(self.base_level)
  }

  /// Paragraph boundaries detected during bidi analysis.
  #[inline]
  pub fn paragraphs(&self) -> &[ParagraphBoundary] {
    &self.paragraphs
  }

  /// Returns the original text that was analyzed.
  pub fn text(&self) -> &str {
    &self.text
  }

  /// Returns logical bidi runs in source order.
  pub fn logical_runs(&self) -> Vec<BidiRun> {
    if self.text.is_empty() || self.levels.is_empty() {
      return Vec::new();
    }

    let mut runs = Vec::new();
    let mut run_start = 0usize;
    let mut current_level = self.levels[0];

    for (idx, &level) in self.levels.iter().enumerate().skip(1) {
      if level != current_level {
        runs.push(self.build_run(run_start, idx, current_level));
        run_start = idx;
        current_level = level;
      }
    }

    runs.push(self.build_run(run_start, self.levels.len(), current_level));

    split_bidi_runs_by_paragraph(runs, &self.paragraphs, self.text.len())
  }

  /// Returns visual runs reordered for display.
  pub fn visual_runs(&self) -> Vec<BidiRun> {
    let runs = self.logical_runs();
    if runs.len() <= 1 {
      return runs;
    }

    fn reorder_slice(slice: &[BidiRun]) -> Vec<BidiRun> {
      if slice.len() <= 1 {
        return slice.to_vec();
      }

      let mut levels: Vec<Level> = Vec::with_capacity(slice.len());
      for run in slice {
        let Ok(level) = Level::new(run.level) else {
          return slice.to_vec();
        };
        levels.push(level);
      }

      unicode_bidi::BidiInfo::reorder_visual(&levels)
        .into_iter()
        .map(|idx| slice[idx].clone())
        .collect()
    }

    if self.paragraphs.is_empty() {
      return reorder_slice(&runs);
    }

    let mut out = Vec::with_capacity(runs.len());
    let mut idx = 0usize;
    for para in &self.paragraphs {
      while idx < runs.len() && runs[idx].end <= para.start_byte {
        idx += 1;
      }
      let start = idx;
      while idx < runs.len() && runs[idx].start < para.end_byte {
        idx += 1;
      }
      if start < idx {
        out.extend(reorder_slice(&runs[start..idx]));
      }
    }

    if idx < runs.len() {
      out.extend(reorder_slice(&runs[idx..]));
    }

    out
  }

  fn build_run(&self, start_byte: usize, end_byte: usize, level: Level) -> BidiRun {
    BidiRun {
      start: start_byte.min(self.text.len()),
      end: end_byte.min(self.text.len()),
      level: level.number(),
      direction: Direction::from_level(level),
    }
  }
}

fn split_bidi_runs_by_paragraph(
  runs: Vec<BidiRun>,
  paragraphs: &[ParagraphBoundary],
  text_len: usize,
) -> Vec<BidiRun> {
  if runs.is_empty() || paragraphs.is_empty() {
    return runs;
  }

  let mut out = Vec::with_capacity(runs.len());
  let mut para_iter = paragraphs.iter().peekable();

  for mut run in runs {
    while let Some(para) = para_iter.peek().copied() {
      if run.start >= para.end_byte {
        para_iter.next();
        continue;
      }

      let para_end = para.end_byte.min(text_len);
      if run.end <= para_end {
        out.push(run);
        break;
      }

      let left = BidiRun {
        end: para_end,
        ..run.clone()
      };
      run.start = para_end;
      out.push(left);
      para_iter.next();
    }
  }

  out
}

// ============================================================================
// Script Itemization
// ============================================================================

/// A run of text with uniform properties.
///
/// After itemization, text is split into runs where each run has:
/// - Consistent script (Latin, Arabic, etc.)
/// - Consistent direction (LTR or RTL)
/// - Consistent bidi level
#[derive(Debug, Clone)]
pub struct ItemizedRun {
  /// Start byte index in original text.
  pub start: usize,
  /// End byte index in original text (exclusive).
  pub end: usize,
  /// The text content of this run.
  pub text: String,
  /// Script for this run.
  pub script: Script,
  /// Text direction for this run.
  pub direction: Direction,
  /// Bidi embedding level.
  pub level: u8,
}

impl ItemizedRun {
  /// Returns the length of this run in bytes.
  #[inline]
  pub fn len(&self) -> usize {
    self.end - self.start
  }

  /// Returns true if this run is empty.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.start >= self.end
  }
}

/// Itemizes text into runs of uniform script and direction.
pub fn itemize_text(text: &str, bidi: &BidiAnalysis) -> Vec<ItemizedRun> {
  if text.is_empty() {
    return Vec::new();
  }

  let mut runs = Vec::new();
  let mut current_start = 0;
  let mut current_text = String::new();
  let mut current_script: Option<Script> = None;
  let mut current_direction: Option<Direction> = None;
  let mut current_level: Option<u8> = None;

  for (idx, ch) in text.char_indices() {
    let char_script = Script::detect(ch);
    let char_direction = bidi.direction_at(idx);
    let char_level = bidi.level_at(idx).number();

    // Resolve neutral scripts based on context
    let resolved_script = if char_script.is_neutral() {
      current_script.unwrap_or(Script::Latin)
    } else {
      char_script
    };

    // Check if we need to start a new run
    let needs_new_run = match (current_script, current_direction, current_level) {
      (None, _, _) => false, // First character, no current run
      (Some(script), Some(dir), Some(level)) => {
        // New run if direction, level, or script changes
        dir != char_direction
          || level != char_level
          || (!char_script.is_neutral() && script != resolved_script)
      }
      _ => false,
    };

    if needs_new_run {
      // Finish current run
      if let (Some(script), Some(direction), Some(level)) =
        (current_script, current_direction, current_level)
      {
        runs.push(ItemizedRun {
          start: current_start,
          end: idx,
          text: std::mem::take(&mut current_text),
          script,
          direction,
          level,
        });
      }
      current_start = idx;
    }

    // Update current run properties
    if !char_script.is_neutral() {
      current_script = Some(resolved_script);
    } else if current_script.is_none() {
      current_script = Some(Script::Latin);
    }
    current_direction = Some(char_direction);
    current_level = Some(char_level);
    current_text.push(ch);
  }

  // Finish last run
  if !current_text.is_empty() {
    if let (Some(script), Some(direction), Some(level)) =
      (current_script, current_direction, current_level)
    {
      runs.push(ItemizedRun {
        start: current_start,
        end: text.len(),
        text: current_text,
        script,
        direction,
        level,
      });
    }
  }

  runs
}

fn split_itemized_runs_by_paragraph(
  runs: Vec<ItemizedRun>,
  paragraphs: &[ParagraphBoundary],
) -> Vec<ItemizedRun> {
  if runs.is_empty() || paragraphs.is_empty() {
    return runs;
  }

  let mut out = Vec::with_capacity(runs.len());
  let mut para_iter = paragraphs.iter().peekable();

  for mut run in runs {
    while let Some(para) = para_iter.peek().copied() {
      // Advance paragraph iterator if run starts after the current paragraph.
      if run.start >= para.end_byte {
        para_iter.next();
        continue;
      }

      let para_end = para.end_byte;
      if run.end <= para_end {
        out.push(run);
        break;
      }

      // Split the run at the paragraph boundary.
      let split_point = para_end;
      let split_offset = split_point.saturating_sub(run.start);
      if let Some((left, right)) = split_run_at(&run, split_offset) {
        out.push(left);
        run = right;
        para_iter.next();
      } else {
        // If the boundary isn't a valid UTF-8 split, keep the remainder intact.
        out.push(run);
        break;
      }
    }
  }

  out
}

fn split_run_at(run: &ItemizedRun, split_offset: usize) -> Option<(ItemizedRun, ItemizedRun)> {
  if split_offset == 0 || split_offset >= run.text.len() || !run.text.is_char_boundary(split_offset)
  {
    return None;
  }

  let left_text = run.text[..split_offset].to_string();
  let right_text = run.text[split_offset..].to_string();
  let left = ItemizedRun {
    start: run.start,
    end: run.start + split_offset,
    text: left_text,
    script: run.script,
    direction: run.direction,
    level: run.level,
  };
  let right = ItemizedRun {
    start: run.start + split_offset,
    end: run.end,
    text: right_text,
    script: run.script,
    direction: run.direction,
    level: run.level,
  };
  Some((left, right))
}

// ============================================================================
// Shaping Clusters
// ============================================================================

/// Returns byte spans for atomic shaping clusters within the text.
///
/// Clusters combine extended grapheme clusters with emoji sequences so that
/// shaping and font fallback never split them.
pub fn atomic_shaping_clusters(text: &str) -> Vec<(usize, usize)> {
  if text.is_empty() {
    return Vec::new();
  }

  let mut boundaries: Vec<usize> = UnicodeSegmentation::grapheme_indices(text, true)
    .map(|(idx, _)| idx)
    .collect();
  if boundaries.first().copied() != Some(0) {
    boundaries.insert(0, 0);
  }
  if boundaries.last().copied() != Some(text.len()) {
    boundaries.push(text.len());
  }

  let sequences = emoji::find_emoji_sequences(text);
  if !sequences.is_empty() {
    let mut filtered = Vec::with_capacity(boundaries.len());
    let mut seq_iter = sequences.iter().peekable();
    for boundary in boundaries {
      while let Some(seq) = seq_iter.peek() {
        if seq.end <= boundary {
          seq_iter.next();
        } else {
          break;
        }
      }
      if let Some(seq) = seq_iter.peek() {
        if seq.start < boundary && boundary < seq.end {
          continue;
        }
      }
      if filtered.last().copied() != Some(boundary) {
        filtered.push(boundary);
      }
    }
    boundaries = filtered;
  }

  let mut clusters = Vec::with_capacity(boundaries.len().saturating_sub(1));
  for window in boundaries.windows(2) {
    let start = window[0];
    let end = window[1];
    if start < end {
      clusters.push((start, end));
    }
  }

  clusters
}

// ============================================================================
// Font Matching
// ============================================================================

/// A run of text with an assigned font.
#[derive(Debug, Clone)]
pub struct FontRun {
  /// The text content.
  pub text: String,
  /// Start byte index in original text.
  pub start: usize,
  /// End byte index in original text.
  pub end: usize,
  /// The assigned font.
  pub font: Arc<LoadedFont>,
  /// Synthetic bold stroke width in pixels (0 = none).
  pub synthetic_bold: f32,
  /// Synthetic oblique shear factor (tan(angle); 0 = none).
  pub synthetic_oblique: f32,
  /// Script for this run.
  pub script: Script,
  /// Text direction.
  pub direction: Direction,
  /// Bidi level.
  pub level: u8,
  /// Font size in pixels.
  pub font_size: f32,
  /// Additional baseline shift in pixels (positive raises text).
  pub baseline_shift: f32,
  /// BCP47 language tag (lowercased).
  pub language: String,
  /// OpenType features to apply for this run.
  pub features: Vec<Feature>,
  /// Font variation settings to apply for this run.
  pub variations: Vec<Variation>,

  /// Palette index for color fonts (CPAL).
  pub palette_index: u16,
  /// Palette overrides resolved for this run.
  pub palette_overrides: Arc<Vec<(u16, Rgba)>>,
  /// Stable hash of palette overrides for cache keys.
  pub palette_override_hash: u64,

  /// Optional rotation hint for vertical writing modes.
  pub rotation: RunRotation,

  /// Whether this run should participate in vertical shaping (inline progression vertical).
  pub vertical: bool,
}

/// Collects OpenType features from computed style.
fn collect_opentype_features(style: &ComputedStyle) -> Vec<Feature> {
  let mut features = Vec::new();
  let lig = style.font_variant_ligatures;
  let numeric = &style.font_variant_numeric;
  let east = &style.font_variant_east_asian;
  let position = style.font_variant_position;
  let caps = style.font_variant_caps;
  let alternates = &style.font_variant_alternates;

  let push_toggle = |features: &mut Vec<Feature>, tag: [u8; 4], enabled: bool| {
    features.push(Feature {
      tag: Tag::from_bytes(&tag),
      value: u32::from(enabled),
      start: 0,
      end: u32::MAX,
    });
  };

  // font-variant-ligatures keywords map to OpenType features
  push_toggle(&mut features, *b"liga", lig.common);
  push_toggle(&mut features, *b"clig", lig.common);
  push_toggle(&mut features, *b"dlig", lig.discretionary);
  push_toggle(&mut features, *b"hlig", lig.historical);
  push_toggle(&mut features, *b"calt", lig.contextual);

  // font-variant-numeric mappings
  match numeric.figure {
    NumericFigure::Lining => push_toggle(&mut features, *b"lnum", true),
    NumericFigure::Oldstyle => push_toggle(&mut features, *b"onum", true),
    NumericFigure::Normal => {}
  }
  match numeric.spacing {
    NumericSpacing::Proportional => push_toggle(&mut features, *b"pnum", true),
    NumericSpacing::Tabular => push_toggle(&mut features, *b"tnum", true),
    NumericSpacing::Normal => {}
  }
  match numeric.fraction {
    NumericFraction::Diagonal => push_toggle(&mut features, *b"frac", true),
    NumericFraction::Stacked => push_toggle(&mut features, *b"afrc", true),
    NumericFraction::Normal => {}
  }
  if numeric.ordinal {
    push_toggle(&mut features, *b"ordn", true);
  }
  if numeric.slashed_zero {
    push_toggle(&mut features, *b"zero", true);
  }

  // font-variant-east-asian
  if let Some(variant) = east.variant {
    match variant {
      EastAsianVariant::Jis78 => push_toggle(&mut features, *b"jp78", true),
      EastAsianVariant::Jis83 => push_toggle(&mut features, *b"jp83", true),
      EastAsianVariant::Jis90 => push_toggle(&mut features, *b"jp90", true),
      EastAsianVariant::Jis04 => push_toggle(&mut features, *b"jp04", true),
      EastAsianVariant::Simplified => push_toggle(&mut features, *b"smpl", true),
      EastAsianVariant::Traditional => push_toggle(&mut features, *b"trad", true),
    }
  }
  if let Some(width) = east.width {
    match width {
      EastAsianWidth::FullWidth => push_toggle(&mut features, *b"fwid", true),
      EastAsianWidth::ProportionalWidth => push_toggle(&mut features, *b"pwid", true),
    }
  }
  if east.ruby {
    push_toggle(&mut features, *b"ruby", true);
  }

  match caps {
    FontVariantCaps::Normal => {}
    FontVariantCaps::SmallCaps => push_toggle(&mut features, *b"smcp", true),
    FontVariantCaps::AllSmallCaps => {
      push_toggle(&mut features, *b"smcp", true);
      push_toggle(&mut features, *b"c2sc", true);
    }
    FontVariantCaps::PetiteCaps => push_toggle(&mut features, *b"pcap", true),
    FontVariantCaps::AllPetiteCaps => {
      push_toggle(&mut features, *b"pcap", true);
      push_toggle(&mut features, *b"c2pc", true);
    }
    FontVariantCaps::Unicase => push_toggle(&mut features, *b"unic", true),
    FontVariantCaps::TitlingCaps => push_toggle(&mut features, *b"titl", true),
  }

  match position {
    FontVariantPosition::Normal => {}
    FontVariantPosition::Sub => push_toggle(&mut features, *b"subs", true),
    FontVariantPosition::Super => push_toggle(&mut features, *b"sups", true),
  }

  if alternates.historical_forms {
    push_toggle(&mut features, *b"hist", true);
  }
  if let Some(set) = alternates.stylistic {
    if let Some(tag) = number_tag(b"ss", set) {
      push_toggle(&mut features, tag, true);
    }
  }
  for set in &alternates.stylesets {
    if let Some(tag) = number_tag(b"ss", *set) {
      push_toggle(&mut features, tag, true);
    }
  }
  for cv in &alternates.character_variants {
    if let Some(tag) = number_tag(b"cv", *cv) {
      let tag = Tag::from_bytes(&tag);
      features.push(Feature {
        tag,
        value: 1,
        start: 0,
        end: u32::MAX,
      });
    }
  }
  if let Some(swash) = alternates.swash {
    // CSS doesn't distinguish swash types; map to swsh.
    let tag = Tag::from_bytes(b"swsh");
    features.retain(|f| f.tag != tag);
    features.push(Feature {
      tag,
      value: swash as u32,
      start: 0,
      end: u32::MAX,
    });
  }
  if let Some(orn) = alternates.ornaments {
    let tag = Tag::from_bytes(b"ornm");
    features.retain(|f| f.tag != tag);
    features.push(Feature {
      tag,
      value: orn as u32,
      start: 0,
      end: u32::MAX,
    });
  }

  match style.font_kerning {
    FontKerning::Auto => {}
    FontKerning::Normal => push_toggle(&mut features, *b"kern", true),
    FontKerning::None => push_toggle(&mut features, *b"kern", false),
  }

  // Low-level font-feature-settings override defaults and prior toggles.
  for setting in &style.font_feature_settings {
    let tag = Tag::from_bytes(&setting.tag);
    features.retain(|f| f.tag != tag);
    features.push(Feature {
      tag,
      value: setting.value,
      start: 0,
      end: u32::MAX,
    });
  }

  features
}

fn font_aspect_ratio(font: &LoadedFont) -> Option<f32> {
  font.metrics().ok().and_then(|m| m.aspect_ratio())
}

/// Returns the author-preferred aspect ratio for font-size-adjust, if any.
pub fn preferred_font_aspect(style: &ComputedStyle, font_context: &FontContext) -> Option<f32> {
  match style.font_size_adjust {
    FontSizeAdjust::None => None,
    FontSizeAdjust::Number(n) if n > 0.0 => Some(n),
    FontSizeAdjust::Number(_) => None,
    FontSizeAdjust::FromFont => {
      let font_style = match style.font_style {
        CssFontStyle::Normal => FontStyle::Normal,
        CssFontStyle::Italic => FontStyle::Italic,
        CssFontStyle::Oblique(_) => FontStyle::Oblique,
      };
      let font_stretch = DbFontStretch::from_percentage(style.font_stretch.to_percentage());
      font_context
        .get_font_full(
          &style.font_family,
          style.font_weight.to_u16(),
          font_style,
          font_stretch,
        )
        .and_then(|font| font_aspect_ratio(&font))
    }
  }
}

/// Computes the used font size after applying font-size-adjust.
pub fn compute_adjusted_font_size(
  style: &ComputedStyle,
  font: &LoadedFont,
  preferred_aspect: Option<f32>,
) -> f32 {
  let base_size = style.font_size;
  let desired_aspect = match style.font_size_adjust {
    FontSizeAdjust::None => None,
    FontSizeAdjust::Number(n) if n > 0.0 => Some(n),
    FontSizeAdjust::Number(_) => None,
    FontSizeAdjust::FromFont => preferred_aspect.or_else(|| font_aspect_ratio(font)),
  };

  if let Some(desired) = desired_aspect {
    let actual = font_aspect_ratio(font).unwrap_or(0.5);
    if actual > 0.0 {
      return base_size * (desired / actual);
    }
  }

  base_size
}

fn is_non_rendering_for_coverage(ch: char) -> bool {
  matches!(ch, '\u{200c}' | '\u{200d}')
    || ('\u{fe00}'..='\u{fe0f}').contains(&ch)
    || ('\u{e0100}'..='\u{e01ef}').contains(&ch)
    || ('\u{180b}'..='\u{180d}').contains(&ch)
}

fn is_unicode_mark(ch: char) -> bool {
  matches!(
    get_general_category(ch),
    GeneralCategory::NonspacingMark | GeneralCategory::SpacingMark | GeneralCategory::EnclosingMark
  )
}

// Optional-mark fallback invariants (Task 72):
// - Only treat marks (General Category M*) as optional when the cluster also contains at least one
//   non-mark glyph that must render.
// - For mark-only clusters, at least one mark remains required so coverage requirements never
//   collapse to "empty" (which could erase the cluster entirely).
// - If we ever skip `.notdef` for missing marks, only do so when the cluster has a non-mark glyph;
//   for mark-only clusters keep `.notdef` so there's always something visible.
fn is_mark_only_cluster(text: &str) -> bool {
  let mut saw_mark = false;
  for ch in text.chars() {
    if is_bidi_control_char(ch) || is_non_rendering_for_coverage(ch) {
      continue;
    }
    if is_unicode_mark(ch) {
      saw_mark = true;
    } else {
      return false;
    }
  }
  saw_mark
}
fn cluster_base_and_relevant_chars(text: &str) -> (char, Vec<char>) {
  let mut chars: Vec<char> = text.chars().collect();
  let mut base = chars
    .iter()
    .copied()
    .find(|c| !is_non_rendering_for_coverage(*c))
    .unwrap_or_else(|| chars.first().copied().unwrap_or('\0'));
  if base == '\0' {
    base = ' ';
  }
  let relevant: Vec<char> = chars
    .drain(..)
    .filter(|c| !is_non_rendering_for_coverage(*c))
    .collect();
  (base, relevant)
}

fn cluster_emoji_preference(text: &str, variant: FontVariantEmoji) -> EmojiPreference {
  let mut iter = text.chars().peekable();
  while let Some(ch) = iter.next() {
    if is_non_rendering_for_coverage(ch) {
      continue;
    }
    return emoji_preference_with_selector(ch, iter.peek().copied(), variant);
  }
  let mut iter = text.chars();
  let base = iter.next().unwrap_or('\0');
  emoji_preference_with_selector(base, iter.next(), variant)
}

fn cluster_signature(text: &str) -> u64 {
  use std::collections::hash_map::DefaultHasher;
  use std::hash::Hash;
  use std::hash::Hasher;

  let mut hasher = DefaultHasher::new();
  text.hash(&mut hasher);
  hasher.finish()
}

fn quantize_oblique_degrees(angle: Option<f32>) -> i16 {
  angle.map(|a| (a * 10.0).round() as i16).unwrap_or(0)
}

fn font_supports_all_chars(font: &LoadedFont, chars: &[char]) -> bool {
  if chars.is_empty() {
    return true;
  }
  let Some(face) = crate::text::face_cache::get_ttf_face(font) else {
    return false;
  };
  chars.iter().all(|c| face.has_glyph(*c))
}

fn last_resort_font(font_context: &FontContext) -> Option<LoadedFont> {
  font_context.last_resort_loaded_font()
}

/// Assigns fonts to itemized runs.
///
/// Uses the font context to find appropriate fonts for each script,
/// falling back through the font family list as needed.
pub fn assign_fonts(
  runs: &[ItemizedRun],
  style: &ComputedStyle,
  font_context: &FontContext,
) -> Result<Vec<FontRun>> {
  assign_fonts_internal(
    runs,
    style,
    font_context,
    None,
    font_context.font_generation(),
  )
}

fn assign_fonts_internal(
  runs: &[ItemizedRun],
  style: &ComputedStyle,
  font_context: &FontContext,
  font_cache: Option<&FallbackCache>,
  font_generation: u64,
) -> Result<Vec<FontRun>> {
  let features = collect_opentype_features(style);
  let authored_variations = authored_variations_from_style(style);
  let preferred_aspect = preferred_font_aspect(style, font_context);
  let (font_style, requested_oblique) = match style.font_style {
    CssFontStyle::Normal => (FontStyle::Normal, None),
    CssFontStyle::Italic => (FontStyle::Italic, None),
    CssFontStyle::Oblique(angle) => (
      FontStyle::Oblique,
      Some(angle.unwrap_or(DEFAULT_OBLIQUE_ANGLE_DEG)),
    ),
  };
  let font_stretch = DbFontStretch::from_percentage(style.font_stretch.to_percentage());
  let families = build_family_entries(style);
  if let Some(cache) = font_cache {
    cache.prepare(font_generation);
  }
  let families_signature = families_signature(&families);
  let oblique_degrees = quantize_oblique_degrees(requested_oblique);
  let weight_value = style.font_weight.to_u16();
  let language = match &style.font_language_override {
    FontLanguageOverride::Normal => style.language.as_str(),
    FontLanguageOverride::Override(tag) => tag.as_str(),
  };
  let language_signature = family_name_signature(language);

  if font_context.is_effectively_empty() {
    let sample = runs.first().map(|run| run.text.clone()).unwrap_or_default();
    return Err(
      TextError::ShapingFailed {
        text: sample,
        reason: "Font context has no fonts; enable bundled fonts or system discovery".to_string(),
      }
      .into(),
    );
  }

  let mut font_runs = Vec::new();
  for run in runs {
    let mut current: Option<(Arc<LoadedFont>, f32, f32, f32, f32, usize)> = None; // (font, bold, oblique, size, baseline_shift, start)
    let mut last_cluster_end = 0usize;

    for (cluster_start, cluster_end) in atomic_shaping_clusters(&run.text) {
      let cluster_text = &run.text[cluster_start..cluster_end];
      let cluster_char_count = cluster_text.chars().count();
      let emoji_pref = cluster_emoji_preference(cluster_text, style.font_variant_emoji);
      let (base_char, mut relevant_chars) = cluster_base_and_relevant_chars(cluster_text);
      let require_base_glyph = !is_non_rendering_for_coverage(base_char);
      let descriptor = font_cache.map(|_| {
        FallbackCacheDescriptor::new(
          families_signature,
          language_signature,
          run.script as u8,
          weight_value,
          font_style,
          font_stretch,
          oblique_degrees,
          emoji_pref,
          require_base_glyph,
        )
      });
      let cluster_cache_key = descriptor.map(|descriptor| ClusterFallbackCacheKey {
        descriptor,
        signature: cluster_signature(cluster_text),
      });
      if relevant_chars.is_empty()
        && cluster_char_count == 1
        && !is_non_rendering_for_coverage(base_char)
      {
        relevant_chars.push(base_char);
      }

      let cached_cluster = match (font_cache, cluster_cache_key.as_ref()) {
        (Some(cache), Some(key)) => cache.get_cluster(key),
        _ => None,
      };

      let mut resolved: Option<LoadedFont> = cached_cluster.clone().flatten();
      let mut skip_resolution = cached_cluster.is_some() && resolved.is_none();

      if !skip_resolution && resolved.is_none() && cluster_char_count == 1 {
        let char_cache_key = descriptor.map(|descriptor| GlyphFallbackCacheKey {
          descriptor,
          ch: base_char,
        });
        if let (Some(cache), Some(key)) = (font_cache, char_cache_key.as_ref()) {
          let cached = cache.get_glyph(key);
          skip_resolution = cached.is_some() && cached.as_ref().is_none();
          resolved = cached.flatten();
        }
        if !skip_resolution && resolved.is_none() {
          let mut picker = FontPreferencePicker::new(emoji_pref);
          let candidate = resolve_font_for_char(
            base_char,
            run.script,
            language,
            &families,
            weight_value,
            font_style,
            requested_oblique,
            font_stretch,
            font_context,
            &mut picker,
          );
          if let (Some(cache), Some(key)) = (font_cache, char_cache_key) {
            cache.insert_glyph(key, candidate.clone());
          }
          resolved = candidate;
        }
      }

      if !skip_resolution && resolved.is_none() {
        let coverage_chars_all = if relevant_chars.is_empty() {
          vec![base_char]
        } else {
          relevant_chars.clone()
        };
        resolved = resolve_font_for_cluster(
          base_char,
          run.script,
          language,
          &coverage_chars_all,
          &families,
          weight_value,
          font_style,
          requested_oblique,
          font_stretch,
          font_context,
          emoji_pref,
        );

        let mut coverage_chars_required: Vec<char> = coverage_chars_all
          .iter()
          .copied()
          .filter(|c| !is_unicode_mark(*c))
          .collect();
        if coverage_chars_required.is_empty() {
          coverage_chars_required.push(base_char);
        }

        if coverage_chars_required.len() != coverage_chars_all.len() {
          let needs_retry = match resolved.as_ref() {
            Some(font) => !font_supports_all_chars(font, &coverage_chars_all),
            None => true,
          };
          if needs_retry {
            resolved = resolve_font_for_cluster(
              base_char,
              run.script,
              language,
              &coverage_chars_required,
              &families,
              weight_value,
              font_style,
              requested_oblique,
              font_stretch,
              font_context,
              emoji_pref,
            );
          }
        }
      }

      if resolved.is_none() {
        resolved = last_resort_font(font_context);
        if resolved.is_some() {
          record_last_resort_fallback(cluster_text);
        }
      }

      if let (Some(cache), Some(key)) = (font_cache, cluster_cache_key) {
        cache.insert_cluster(key, resolved.clone());
      }

      let font = resolved.ok_or_else(|| {
        let reason = if font_context.is_effectively_empty() {
          "No fonts are available in the font context; enable bundled fonts or system discovery"
            .to_string()
        } else {
          "Font context failed to provide a last-resort font; check font configuration".to_string()
        };
        TextError::ShapingFailed {
          text: run.text.clone(),
          reason,
        }
      })?;
      let used_font_size = compute_adjusted_font_size(style, &font, preferred_aspect);
      let (mut synthetic_bold, synthetic_oblique) = compute_synthetic_styles(style, &font);
      if style.font_size > 0.0 {
        synthetic_bold *= used_font_size / style.font_size;
      }
      let font_arc = Arc::new(font);
      let (position_scale, baseline_shift) =
        synthetic_position_adjustment(style, &font_arc, used_font_size, font_context);
      let run_font_size = used_font_size * position_scale;

      let same_as_current = current.as_ref().is_some_and(|(f, b, o, size, shift, _)| {
        Arc::ptr_eq(&f.data, &font_arc.data)
          && f.index == font_arc.index
          && f.weight == font_arc.weight
          && f.style == font_arc.style
          && f.stretch == font_arc.stretch
          && (*b - synthetic_bold).abs() < f32::EPSILON
          && (*o - synthetic_oblique).abs() < f32::EPSILON
          && (*size - run_font_size).abs() < f32::EPSILON
          && (*shift - baseline_shift).abs() < f32::EPSILON
      });

      if !same_as_current {
        if let Some((font, bold, oblique, size, shift, start)) = current.take() {
          push_font_run(
            &mut font_runs,
            run,
            start,
            cluster_start,
            font,
            bold,
            oblique,
            size,
            shift,
            &features,
            &authored_variations,
            style,
            font_context,
          );
        }
        current = Some((
          font_arc,
          synthetic_bold,
          synthetic_oblique,
          run_font_size,
          baseline_shift,
          cluster_start,
        ));
      }

      last_cluster_end = cluster_end;
    }

    if let Some((font, bold, oblique, size, shift, start)) = current.take() {
      push_font_run(
        &mut font_runs,
        run,
        start,
        last_cluster_end,
        font,
        bold,
        oblique,
        size,
        shift,
        &features,
        &authored_variations,
        style,
        font_context,
      );
    }
  }

  Ok(font_runs)
}

pub(crate) fn authored_variations_from_style(style: &ComputedStyle) -> Vec<Variation> {
  style
    .font_variation_settings
    .iter()
    .map(|v| Variation {
      tag: Tag::from_bytes(&v.tag),
      value: v.value,
    })
    .collect()
}

fn is_vertical_typographic_mode(mode: crate::style::types::WritingMode) -> bool {
  matches!(
    mode,
    crate::style::types::WritingMode::VerticalRl | crate::style::types::WritingMode::VerticalLr
  )
}

fn is_sideways_writing_mode(mode: crate::style::types::WritingMode) -> bool {
  matches!(
    mode,
    crate::style::types::WritingMode::SidewaysRl | crate::style::types::WritingMode::SidewaysLr
  )
}

fn apply_vertical_text_orientation(
  runs: Vec<FontRun>,
  orientation: crate::style::types::TextOrientation,
) -> Vec<FontRun> {
  use crate::style::types::TextOrientation;

  match orientation {
    TextOrientation::Sideways | TextOrientation::SidewaysRight => runs
      .into_iter()
      .map(|mut run| {
        run.rotation = RunRotation::Cw90;
        run
      })
      .collect(),
    TextOrientation::SidewaysLeft => runs
      .into_iter()
      .map(|mut run| {
        run.rotation = RunRotation::Ccw90;
        run
      })
      .collect(),
    TextOrientation::Upright => runs
      .into_iter()
      .map(|mut run| {
        run.rotation = RunRotation::None;
        run.vertical = true;
        run
      })
      .collect(),
    TextOrientation::Mixed => runs
      .into_iter()
      .flat_map(split_run_by_vertical_orientation)
      .collect(),
  }
}

fn apply_sideways_text_orientation(runs: Vec<FontRun>) -> Vec<FontRun> {
  runs
    .into_iter()
    .map(|mut run| {
      run.rotation = RunRotation::Cw90;
      run
    })
    .collect()
}

fn split_run_by_vertical_orientation(run: FontRun) -> Vec<FontRun> {
  if run.text.is_empty() {
    return Vec::new();
  }

  let mut segments: Vec<FontRun> = Vec::new();
  let mut iter = run.text.char_indices();
  let (first_idx, first_char) = match iter.next() {
    Some(pair) => pair,
    None => return segments,
  };
  let mut current_rotation = rotation_for_mixed_char(first_char);
  let mut current_start = first_idx;

  for (idx, ch) in iter {
    let rotation = rotation_for_mixed_char(ch);
    if rotation != current_rotation {
      push_oriented_segment(&run, current_start, idx, current_rotation, &mut segments);
      current_start = idx;
      current_rotation = rotation;
    }
  }

  push_oriented_segment(
    &run,
    current_start,
    run.text.len(),
    current_rotation,
    &mut segments,
  );
  segments
}

fn rotation_for_mixed_char(ch: char) -> RunRotation {
  match char_orientation(ch) {
    VerticalOrientation::Upright | VerticalOrientation::TransformedOrUpright => RunRotation::None,
    VerticalOrientation::Rotated | VerticalOrientation::TransformedOrRotated => RunRotation::Cw90,
  }
}

fn push_oriented_segment(
  run: &FontRun,
  start: usize,
  end: usize,
  rotation: RunRotation,
  out: &mut Vec<FontRun>,
) {
  if start >= end {
    return;
  }

  let mut segment = run.clone();
  segment.text = run.text[start..end].to_string();
  segment.start = run.start + start;
  segment.end = run.start + end;
  segment.rotation = rotation;
  segment.vertical = matches!(rotation, RunRotation::None);
  out.push(segment);
}

fn requested_slant_angle(style: &ComputedStyle) -> Option<f32> {
  match style.font_style {
    CssFontStyle::Normal => None,
    CssFontStyle::Italic => Some(DEFAULT_OBLIQUE_ANGLE_DEG),
    CssFontStyle::Oblique(angle) => Some(angle.unwrap_or(DEFAULT_OBLIQUE_ANGLE_DEG)),
  }
}

fn compute_synthetic_styles(style: &ComputedStyle, font: &LoadedFont) -> (f32, f32) {
  let mut synthetic_bold = 0.0;
  let mut synthetic_oblique = 0.0;

  if style.font_synthesis.weight
    && style.font_weight.to_u16() > font.weight.value()
    && !font_has_axis(font, *b"wght")
  {
    let delta = (style.font_weight.to_u16() as f32 - font.weight.value() as f32).max(0.0);
    let strength = (delta / 400.0).clamp(0.0, 1.0);
    synthetic_bold = style.font_size * 0.04 * strength;
  }

  if style.font_synthesis.style && matches!(font.style, FontStyle::Normal) {
    let Some(angle) = requested_slant_angle(style) else {
      return (synthetic_bold, synthetic_oblique);
    };

    let (has_slnt_axis, has_ital_axis) = crate::text::face_cache::with_face(font, |face| {
      let mut has_slnt = false;
      let mut has_ital = false;
      for axis in face.variation_axes() {
        if axis.tag == Tag::from_bytes(b"slnt") {
          has_slnt = true;
        } else if axis.tag == Tag::from_bytes(b"ital") {
          has_ital = true;
        }
      }
      (has_slnt, has_ital)
    })
    .unwrap_or((false, false));

    let variation_covers_slant = match style.font_style {
      CssFontStyle::Oblique(_) => has_slnt_axis || has_ital_axis,
      CssFontStyle::Italic => has_ital_axis || has_slnt_axis,
      CssFontStyle::Normal => false,
    };

    if !variation_covers_slant {
      synthetic_oblique = angle.to_radians().tan();
    }
  }

  (synthetic_bold, synthetic_oblique)
}

fn font_has_axis(font: &LoadedFont, tag: [u8; 4]) -> bool {
  crate::text::face_cache::with_face(font, |face| {
    let target = Tag::from_bytes(&tag);
    face
      .variation_axes()
      .into_iter()
      .any(|axis| axis.tag == target)
  })
  .unwrap_or(false)
}

fn synthetic_position_adjustment(
  style: &ComputedStyle,
  font: &LoadedFont,
  base_font_size: f32,
  font_context: &FontContext,
) -> (f32, f32) {
  if !style.font_synthesis.position {
    return (1.0, 0.0);
  }

  const SYNTHETIC_SCALE: f32 = 0.8;
  const SUPER_SHIFT: f32 = 0.34;
  const SUB_SHIFT: f32 = -0.2;

  match style.font_variant_position {
    crate::style::types::FontVariantPosition::Normal => (1.0, 0.0),
    crate::style::types::FontVariantPosition::Super => {
      if font_context.supports_feature(font, *b"sups") {
        (1.0, 0.0)
      } else if let Some((scale, shift)) = os2_position_metrics(
        font,
        base_font_size,
        crate::style::types::FontVariantPosition::Super,
      ) {
        (scale, shift)
      } else {
        (SYNTHETIC_SCALE, base_font_size * SUPER_SHIFT)
      }
    }
    crate::style::types::FontVariantPosition::Sub => {
      if font_context.supports_feature(font, *b"subs") {
        (1.0, 0.0)
      } else if let Some((scale, shift)) = os2_position_metrics(
        font,
        base_font_size,
        crate::style::types::FontVariantPosition::Sub,
      ) {
        (scale, shift)
      } else {
        (SYNTHETIC_SCALE, base_font_size * SUB_SHIFT)
      }
    }
  }
}

fn os2_position_metrics(
  font: &LoadedFont,
  base_font_size: f32,
  position: crate::style::types::FontVariantPosition,
) -> Option<(f32, f32)> {
  let face = crate::text::face_cache::get_ttf_face(font)?;
  let face = face.face();
  let os2 = face.tables().os2?;
  let units = face.units_per_em() as f32;

  match position {
    crate::style::types::FontVariantPosition::Super => {
      let metrics = os2.superscript_metrics();
      if metrics.y_size > 0 {
        let scale = (metrics.y_size as f32 / units).clamp(0.3, 1.2);
        let shift = metrics.y_offset as f32 * (base_font_size / units);
        Some((scale, shift))
      } else {
        None
      }
    }
    crate::style::types::FontVariantPosition::Sub => {
      let metrics = os2.subscript_metrics();
      if metrics.y_size > 0 {
        let scale = (metrics.y_size as f32 / units).clamp(0.3, 1.2);
        let shift = -(metrics.y_offset as f32 * (base_font_size / units));
        Some((scale, shift))
      } else {
        None
      }
    }
    crate::style::types::FontVariantPosition::Normal => None,
  }
}

pub(crate) fn slope_preference_order(style: FontStyle) -> &'static [FontStyle] {
  match style {
    FontStyle::Normal => &[FontStyle::Normal],
    FontStyle::Italic => &[FontStyle::Italic, FontStyle::Oblique, FontStyle::Normal],
    FontStyle::Oblique => &[FontStyle::Oblique, FontStyle::Italic, FontStyle::Normal],
  }
}

pub(crate) fn weight_preference_order(weight: u16) -> Vec<u16> {
  let desired = weight.clamp(1, 1000);
  let mut candidates: Vec<u16> = (1..=9).map(|i| i * 100).collect();
  if !candidates.contains(&desired) {
    candidates.push(desired);
  }
  candidates.sort_by(|a, b| {
    weight_order_key(*a, desired)
      .cmp(&weight_order_key(*b, desired))
      .then_with(|| a.cmp(b))
  });
  candidates.dedup();
  candidates
}

pub(crate) fn stretch_preference_order(stretch: DbFontStretch) -> Vec<DbFontStretch> {
  let target = stretch.to_percentage();
  let mut variants = [
    DbFontStretch::UltraCondensed,
    DbFontStretch::ExtraCondensed,
    DbFontStretch::Condensed,
    DbFontStretch::SemiCondensed,
    DbFontStretch::Normal,
    DbFontStretch::SemiExpanded,
    DbFontStretch::Expanded,
    DbFontStretch::ExtraExpanded,
    DbFontStretch::UltraExpanded,
  ];
  variants.sort_by(|a, b| {
    let ka = stretch_order_key(a.to_percentage(), target);
    let kb = stretch_order_key(b.to_percentage(), target);
    match ka.0.cmp(&kb.0) {
      std::cmp::Ordering::Equal => ka.1.partial_cmp(&kb.1).unwrap_or(std::cmp::Ordering::Equal),
      other => other,
    }
  });
  variants.to_vec()
}

fn weight_order_key(candidate: u16, desired: u16) -> (u8, i32) {
  let desired = desired.clamp(1, 1000) as i32;
  let candidate = candidate as i32;

  if (400..=500).contains(&desired) {
    if candidate >= desired && candidate <= 500 {
      return (0, (candidate - desired).abs());
    }
    if candidate < desired {
      return (1, (desired - candidate).abs());
    }
    return (2, (candidate - desired).abs());
  }

  if desired < 400 {
    if candidate <= desired {
      return (0, (desired - candidate).abs());
    }
    return (1, (candidate - desired).abs());
  }

  // desired > 500
  if candidate >= desired {
    (0, (candidate - desired).abs())
  } else {
    (1, (desired - candidate).abs())
  }
}

fn stretch_order_key(candidate: f32, desired: f32) -> (u8, f32) {
  if (candidate - desired).abs() < f32::EPSILON {
    return (0, 0.0);
  }

  if desired <= 100.0 {
    if candidate <= desired {
      return (0, (desired - candidate).abs());
    }
    return (1, (candidate - desired).abs());
  }

  // desired > 100
  if candidate >= desired {
    (0, (candidate - desired).abs())
  } else {
    (1, (desired - candidate).abs())
  }
}

fn is_bidi_control_char(ch: char) -> bool {
  matches!(
    ch,
    '\u{202a}' // LRE
            | '\u{202b}' // RLE
            | '\u{202c}' // PDF
            | '\u{202d}' // LRO
            | '\u{202e}' // RLO
            | '\u{2066}' // LRI
            | '\u{2067}' // RLI
            | '\u{2068}' // FSI
            | '\u{2069}' // PDI
  )
}

#[cfg(test)]
fn is_emoji_dominant(text: &str) -> bool {
  let mut saw_emoji = false;
  for c in text.chars() {
    if crate::text::emoji::is_emoji(c) {
      saw_emoji = true;
      continue;
    }
    if c.is_whitespace() || matches!(c, '\u{200d}' | '\u{fe0f}' | '\u{fe0e}') {
      continue;
    }
    return false;
  }
  saw_emoji
}

fn font_is_emoji_font(db: &FontDatabase, id: Option<fontdb::ID>, font: &LoadedFont) -> bool {
  if let Some(id) = id {
    if let Some(is_color) = db.is_color_capable_font(id) {
      return is_color;
    }
  }

  FontDatabase::family_name_is_emoji_font(&font.family)
}

#[derive(Default)]
struct FontPreferencePicker {
  prefer_emoji: bool,
  avoid_emoji: bool,
  first_emoji: Option<(LoadedFont, usize)>,
  first_text: Option<(LoadedFont, usize)>,
  first_emoji_any: Option<(LoadedFont, usize)>,
  first_text_any: Option<(LoadedFont, usize)>,
  order: usize,
}

impl FontPreferencePicker {
  fn new(pref: EmojiPreference) -> Self {
    Self {
      prefer_emoji: matches!(pref, EmojiPreference::PreferEmoji),
      avoid_emoji: matches!(pref, EmojiPreference::AvoidEmoji),
      ..Self::default()
    }
  }

  fn bump_order(&mut self) -> usize {
    let idx = self.order;
    self.order += 1;
    idx
  }

  fn record_any(&mut self, font: &LoadedFont, is_emoji_font: bool, idx: usize) {
    if is_emoji_font {
      if self.first_emoji_any.is_none() {
        self.first_emoji_any = Some((font.clone(), idx));
      }
    } else if self.first_text_any.is_none() {
      self.first_text_any = Some((font.clone(), idx));
    }
  }

  fn consider(&mut self, font: LoadedFont, is_emoji_font: bool, idx: usize) -> Option<LoadedFont> {
    if is_emoji_font {
      if self.first_emoji.is_none() {
        self.first_emoji = Some((font.clone(), idx));
      }
      if self.prefer_emoji && !self.avoid_emoji {
        return Some(font);
      }
      if !self.prefer_emoji && !self.avoid_emoji {
        return Some(font);
      }
      // avoid_emoji => keep as fallback if nothing else has a glyph
    } else {
      if self.first_text.is_none() {
        self.first_text = Some((font.clone(), idx));
      }
      if self.avoid_emoji {
        return Some(font);
      }
      if !self.prefer_emoji {
        return Some(font);
      }
      // prefer_emoji => keep searching for an emoji font with coverage
    }

    None
  }

  fn finish(&mut self) -> Option<LoadedFont> {
    let first_emoji = self
      .first_emoji
      .take()
      .or_else(|| self.first_emoji_any.take());
    let first_text = self
      .first_text
      .take()
      .or_else(|| self.first_text_any.take());

    if self.prefer_emoji && !self.avoid_emoji {
      first_emoji
        .map(|(f, _)| f)
        .or_else(|| first_text.map(|(f, _)| f))
    } else if self.avoid_emoji {
      first_text
        .map(|(f, _)| f)
        .or_else(|| first_emoji.map(|(f, _)| f))
    } else {
      match (first_text, first_emoji) {
        (Some((text, ti)), Some((emoji, ei))) => {
          if ti <= ei {
            Some(text)
          } else {
            Some(emoji)
          }
        }
        (Some((text, _)), None) => Some(text),
        (None, Some((emoji, _))) => Some(emoji),
        (None, None) => None,
      }
    }
  }
}

fn emoji_preference_for_char(ch: char, variant: FontVariantEmoji) -> EmojiPreference {
  match variant {
    FontVariantEmoji::Emoji => EmojiPreference::PreferEmoji,
    FontVariantEmoji::Text => EmojiPreference::AvoidEmoji,
    FontVariantEmoji::Unicode => {
      if emoji::is_emoji_presentation(ch) {
        EmojiPreference::PreferEmoji
      } else {
        EmojiPreference::AvoidEmoji
      }
    }
    FontVariantEmoji::Normal => {
      if emoji::is_emoji_presentation(ch) {
        EmojiPreference::PreferEmoji
      } else {
        EmojiPreference::Neutral
      }
    }
  }
}

fn emoji_preference_with_selector(
  ch: char,
  next: Option<char>,
  variant: FontVariantEmoji,
) -> EmojiPreference {
  if let Some(sel) = next {
    if sel == '\u{FE0F}' {
      return EmojiPreference::PreferEmoji;
    }
    if sel == '\u{FE0E}' {
      return EmojiPreference::AvoidEmoji;
    }
  }

  let base_pref = emoji_preference_for_char(ch, variant);

  if let Some('\u{200d}') = next {
    if emoji::is_emoji(ch) || emoji::is_emoji_presentation(ch) {
      return EmojiPreference::PreferEmoji;
    }
  }

  base_pref
}

fn build_family_entries(style: &ComputedStyle) -> Vec<crate::text::font_fallback::FamilyEntry> {
  use crate::text::font_fallback::FamilyEntry;
  let mut entries = Vec::new();
  for family in &style.font_family {
    if let Some(generic) = crate::text::font_db::GenericFamily::parse(family) {
      entries.push(FamilyEntry::Generic(generic));
    } else {
      entries.push(FamilyEntry::Named(family.clone()));
    }
  }

  if matches!(style.font_variant_emoji, FontVariantEmoji::Emoji)
    && !entries.iter().any(|e| {
      matches!(
        e,
        crate::text::font_fallback::FamilyEntry::Generic(
          crate::text::font_db::GenericFamily::Emoji
        )
      )
    })
  {
    entries.insert(
      0,
      crate::text::font_fallback::FamilyEntry::Generic(crate::text::font_db::GenericFamily::Emoji),
    );
  }

  if !entries.iter().any(|e| {
    matches!(
      e,
      crate::text::font_fallback::FamilyEntry::Generic(
        crate::text::font_db::GenericFamily::SansSerif
      )
    )
  }) {
    entries.push(crate::text::font_fallback::FamilyEntry::Generic(
      crate::text::font_db::GenericFamily::SansSerif,
    ));
  }

  entries
}

#[allow(clippy::cognitive_complexity)]
fn resolve_font_for_char(
  ch: char,
  script: Script,
  language: &str,
  families: &[crate::text::font_fallback::FamilyEntry],
  weight: u16,
  style: FontStyle,
  oblique_angle: Option<f32>,
  stretch: DbFontStretch,
  font_context: &FontContext,
  picker: &mut FontPreferencePicker,
) -> Option<LoadedFont> {
  use crate::text::font_fallback::FamilyEntry;
  use fontdb::Family;
  let db = font_context.database();
  let is_emoji = emoji::is_emoji(ch);
  let weight_preferences = weight_preference_order(weight);
  let slope_preferences = slope_preference_order(style);
  let stretch_preferences = stretch_preference_order(stretch);
  let math_families = font_context.math_family_names();
  for entry in families {
    if let FamilyEntry::Generic(crate::text::font_db::GenericFamily::Math) = entry {
      for family in &math_families {
        if let Some(font) =
          font_context.match_web_font_for_char(family, weight, style, stretch, oblique_angle, ch)
        {
          return Some(font);
        }
        if let Some(font) =
          font_context.match_web_font_for_family(family, weight, style, stretch, oblique_angle)
        {
          let is_emoji_font = font_is_emoji_font(db, None, &font);
          let idx = picker.bump_order();
          picker.record_any(&font, is_emoji_font, idx);
        }
        for stretch_choice in &stretch_preferences {
          for slope in slope_preferences {
            for weight_choice in &weight_preferences {
              let query = fontdb::Query {
                families: &[Family::Name(family.as_str())],
                weight: fontdb::Weight(*weight_choice),
                stretch: (*stretch_choice).into(),
                style: (*slope).into(),
              };
              if let Some(id) = db.inner().query(&query) {
                if let Some(font) = db.load_font(id) {
                  let is_emoji_font = font_is_emoji_font(db, Some(id), &font);
                  let idx = picker.bump_order();
                  picker.record_any(&font, is_emoji_font, idx);
                  if db.has_glyph_cached(id, ch) {
                    if let Some(font) = picker.consider(font, is_emoji_font, idx) {
                      return Some(font);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    if let FamilyEntry::Named(name) = entry {
      if let Some(font) =
        font_context.match_web_font_for_char(name, weight, style, stretch, oblique_angle, ch)
      {
        return Some(font);
      }
      if let Some(font) =
        font_context.match_web_font_for_family(name, weight, style, stretch, oblique_angle)
      {
        let is_emoji_font = font_is_emoji_font(db, None, &font);
        let idx = picker.bump_order();
        picker.record_any(&font, is_emoji_font, idx);
      }
      if font_context.is_web_family_declared(name) {
        continue;
      }
    }

    for stretch_choice in &stretch_preferences {
      for slope in slope_preferences {
        for weight_choice in &weight_preferences {
          let query = match entry {
            FamilyEntry::Named(name) => fontdb::Query {
              families: &[Family::Name(name)],
              weight: fontdb::Weight(*weight_choice),
              stretch: (*stretch_choice).into(),
              style: (*slope).into(),
            },
            FamilyEntry::Generic(generic) => fontdb::Query {
              families: &[generic.to_fontdb()],
              weight: fontdb::Weight(*weight_choice),
              stretch: (*stretch_choice).into(),
              style: (*slope).into(),
            },
          };

          if let Some(id) = db.inner().query(&query) {
            if let Some(font) = db.load_font(id) {
              let is_emoji_font = font_is_emoji_font(db, Some(id), &font);
              let idx = picker.bump_order();
              picker.record_any(&font, is_emoji_font, idx);
              if db.has_glyph_cached(id, ch) {
                if let Some(font) = picker.consider(font, is_emoji_font, idx) {
                  return Some(font);
                }
              }
            }
          }
        }
      }
    }

    if let FamilyEntry::Generic(generic) = entry {
      for name in generic.fallback_families() {
        for weight_choice in &weight_preferences {
          for slope in slope_preferences {
            for stretch_choice in &stretch_preferences {
              let query = fontdb::Query {
                families: &[Family::Name(name)],
                weight: fontdb::Weight(*weight_choice),
                stretch: (*stretch_choice).into(),
                style: (*slope).into(),
              };
              if let Some(id) = db.inner().query(&query) {
                if let Some(font) = db.load_font(id) {
                  let is_emoji_font = font_is_emoji_font(db, Some(id), &font);
                  let idx = picker.bump_order();
                  picker.record_any(&font, is_emoji_font, idx);
                  if db.has_glyph_cached(id, ch) {
                    if let Some(font) = picker.consider(font, is_emoji_font, idx) {
                      return Some(font);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  if is_emoji && !picker.avoid_emoji {
    for id in db.find_emoji_fonts() {
      if let Some(font) = db.load_font(id) {
        let idx = picker.bump_order();
        picker.record_any(&font, true, idx);
        if db.has_glyph_cached(id, ch) {
          if let Some(font) = picker.consider(font, true, idx) {
            return Some(font);
          }
        }
      }
    }
  }

  for family in script_fallback::preferred_families(script, language) {
    for stretch_choice in &stretch_preferences {
      for slope in slope_preferences {
        for weight_choice in &weight_preferences {
          let query = fontdb::Query {
            families: &[Family::Name(family)],
            weight: fontdb::Weight(*weight_choice),
            stretch: (*stretch_choice).into(),
            style: (*slope).into(),
          };
          if let Some(id) = db.inner().query(&query) {
            if let Some(font) = db.load_font(id) {
              let is_emoji_font = font_is_emoji_font(db, Some(id), &font);
              let idx = picker.bump_order();
              picker.record_any(&font, is_emoji_font, idx);
              if db.has_glyph_cached(id, ch) {
                if let Some(font) = picker.consider(font, is_emoji_font, idx) {
                  return Some(font);
                }
              }
            }
          }
        }
      }
    }
  }

  for face in db.faces() {
    if let Some(font) = db.load_font(face.id) {
      let is_emoji_font = font_is_emoji_font(db, Some(face.id), &font);
      let idx = picker.bump_order();
      picker.record_any(&font, is_emoji_font, idx);
      if db.has_glyph_cached(face.id, ch) {
        if let Some(font) = picker.consider(font, is_emoji_font, idx) {
          return Some(font);
        }
      }
    }
  }

  picker.finish()
}

#[allow(clippy::cognitive_complexity)]
fn resolve_font_for_cluster(
  base_char: char,
  script: Script,
  language: &str,
  coverage_chars: &[char],
  families: &[crate::text::font_fallback::FamilyEntry],
  weight: u16,
  style: FontStyle,
  oblique_angle: Option<f32>,
  stretch: DbFontStretch,
  font_context: &FontContext,
  emoji_pref: EmojiPreference,
) -> Option<LoadedFont> {
  use crate::text::font_fallback::FamilyEntry;
  use fontdb::Family;
  let db = font_context.database();
  let is_emoji = crate::text::font_db::FontDatabase::is_emoji(base_char);
  let weight_preferences = weight_preference_order(weight);
  let slope_preferences = slope_preference_order(style);
  let stretch_preferences = stretch_preference_order(stretch);
  let math_families = font_context.math_family_names();
  let mut picker = FontPreferencePicker::new(emoji_pref);
  let require_base_glyph = !is_non_rendering_for_coverage(base_char);

  let mut needed: Vec<char> = coverage_chars.iter().copied().collect();
  needed.sort_unstable();
  needed.dedup();
  if needed.is_empty() && require_base_glyph {
    needed.push(base_char);
  }

  let base_supported = |id: fontdb::ID| !require_base_glyph || db.has_glyph_cached(id, base_char);
  let covers_needed = |id: fontdb::ID| needed.iter().all(|c| db.has_glyph_cached(id, *c));

  for entry in families {
    if let FamilyEntry::Generic(crate::text::font_db::GenericFamily::Math) = entry {
      for family in &math_families {
        let web_font = font_context
          .match_web_font_for_char(family, weight, style, stretch, oblique_angle, base_char)
          .or_else(|| {
            font_context.match_web_font_for_family(family, weight, style, stretch, oblique_angle)
          });
        if let Some(font) = web_font {
          let is_emoji_font = font_is_emoji_font(db, None, &font);
          let idx = picker.bump_order();
          picker.record_any(&font, is_emoji_font, idx);
          let base_ok = !require_base_glyph || font_supports_all_chars(&font, &[base_char]);
          if base_ok && font_supports_all_chars(&font, &needed) {
            if let Some(font) = picker.consider(font, is_emoji_font, idx) {
              return Some(font);
            }
          }
        }
        for stretch_choice in &stretch_preferences {
          for slope in slope_preferences {
            for weight_choice in &weight_preferences {
              let query = fontdb::Query {
                families: &[Family::Name(family.as_str())],
                weight: fontdb::Weight(*weight_choice),
                stretch: (*stretch_choice).into(),
                style: (*slope).into(),
              };
              if let Some(id) = db.inner().query(&query) {
                if let Some(font) = db.load_font(id) {
                  let is_emoji_font = font_is_emoji_font(db, Some(id), &font);
                  let idx = picker.bump_order();
                  picker.record_any(&font, is_emoji_font, idx);
                  if base_supported(id) && covers_needed(id) {
                    if let Some(font) = picker.consider(font, is_emoji_font, idx) {
                      return Some(font);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    if let FamilyEntry::Named(name) = entry {
      let web_font = font_context
        .match_web_font_for_char(name, weight, style, stretch, oblique_angle, base_char)
        .or_else(|| {
          font_context.match_web_font_for_family(name, weight, style, stretch, oblique_angle)
        });
      if let Some(font) = web_font {
        let is_emoji_font = font_is_emoji_font(db, None, &font);
        let idx = picker.bump_order();
        picker.record_any(&font, is_emoji_font, idx);
        let base_ok = !require_base_glyph || font_supports_all_chars(&font, &[base_char]);
        if base_ok && font_supports_all_chars(&font, &needed) {
          if let Some(font) = picker.consider(font, is_emoji_font, idx) {
            return Some(font);
          }
        }
      }
      if font_context.is_web_family_declared(name) {
        continue;
      }
    }

    for stretch_choice in &stretch_preferences {
      for slope in slope_preferences {
        for weight_choice in &weight_preferences {
          let query = match entry {
            FamilyEntry::Named(name) => fontdb::Query {
              families: &[Family::Name(name)],
              weight: fontdb::Weight(*weight_choice),
              stretch: (*stretch_choice).into(),
              style: (*slope).into(),
            },
            FamilyEntry::Generic(generic) => fontdb::Query {
              families: &[generic.to_fontdb()],
              weight: fontdb::Weight(*weight_choice),
              stretch: (*stretch_choice).into(),
              style: (*slope).into(),
            },
          };

          if let Some(id) = db.inner().query(&query) {
            if let Some(font) = db.load_font(id) {
              let is_emoji_font = font_is_emoji_font(db, Some(id), &font);
              let idx = picker.bump_order();
              picker.record_any(&font, is_emoji_font, idx);
              if base_supported(id) && covers_needed(id) {
                if let Some(font) = picker.consider(font, is_emoji_font, idx) {
                  return Some(font);
                }
              }
            }
          }
        }
      }
    }

    if let FamilyEntry::Generic(generic) = entry {
      for name in generic.fallback_families() {
        for weight_choice in &weight_preferences {
          for slope in slope_preferences {
            for stretch_choice in &stretch_preferences {
              let query = fontdb::Query {
                families: &[Family::Name(name)],
                weight: fontdb::Weight(*weight_choice),
                stretch: (*stretch_choice).into(),
                style: (*slope).into(),
              };
              if let Some(id) = db.inner().query(&query) {
                if let Some(font) = db.load_font(id) {
                  let is_emoji_font = font_is_emoji_font(db, Some(id), &font);
                  let idx = picker.bump_order();
                  picker.record_any(&font, is_emoji_font, idx);
                  if base_supported(id) && covers_needed(id) {
                    if let Some(font) = picker.consider(font, is_emoji_font, idx) {
                      return Some(font);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  if is_emoji && !picker.avoid_emoji {
    for id in db.find_emoji_fonts() {
      if let Some(font) = db.load_font(id) {
        let idx = picker.bump_order();
        picker.record_any(&font, true, idx);
        if base_supported(id) && covers_needed(id) {
          if let Some(font) = picker.consider(font, true, idx) {
            return Some(font);
          }
        }
      }
    }
  }

  for family in script_fallback::preferred_families(script, language) {
    for stretch_choice in &stretch_preferences {
      for slope in slope_preferences {
        for weight_choice in &weight_preferences {
          let query = fontdb::Query {
            families: &[Family::Name(family)],
            weight: fontdb::Weight(*weight_choice),
            stretch: (*stretch_choice).into(),
            style: (*slope).into(),
          };
          if let Some(id) = db.inner().query(&query) {
            if let Some(font) = db.load_font(id) {
              let is_emoji_font = font_is_emoji_font(db, Some(id), &font);
              let idx = picker.bump_order();
              picker.record_any(&font, is_emoji_font, idx);
              if base_supported(id) && covers_needed(id) {
                if let Some(font) = picker.consider(font, is_emoji_font, idx) {
                  return Some(font);
                }
              }
            }
          }
        }
      }
    }
  }

  for face in db.faces() {
    if let Some(font) = db.load_font(face.id) {
      let is_emoji_font = font_is_emoji_font(db, Some(face.id), &font);
      let idx = picker.bump_order();
      picker.record_any(&font, is_emoji_font, idx);
      if base_supported(face.id) && covers_needed(face.id) {
        if let Some(font) = picker.consider(font, is_emoji_font, idx) {
          return Some(font);
        }
      }
    }
  }

  picker.finish()
}

pub(crate) fn collect_variations_for_face(
  face: &ttf_parser::Face<'_>,
  style: &ComputedStyle,
  font_size: f32,
  authored_variations: &[Variation],
) -> Vec<Variation> {
  let mut variations = authored_variations.to_vec();
  let axes: Vec<_> = face.variation_axes().into_iter().collect();
  if axes.is_empty() {
    return variations;
  }

  let mut set_tags: HashSet<Tag> = variations.iter().map(|v| v.tag).collect();
  let wght_tag = Tag::from_bytes(b"wght");
  let wdth_tag = Tag::from_bytes(b"wdth");
  let opsz_tag = Tag::from_bytes(b"opsz");
  let ital_tag = Tag::from_bytes(b"ital");
  let slnt_tag = Tag::from_bytes(b"slnt");
  let has_slnt_axis = axes.iter().any(|a| a.tag == slnt_tag);
  let has_ital_axis = axes.iter().any(|a| a.tag == ital_tag);
  let slnt_angle = match style.font_style {
    CssFontStyle::Oblique(angle) => Some(angle.unwrap_or(DEFAULT_OBLIQUE_ANGLE_DEG)),
    CssFontStyle::Italic if !has_ital_axis => Some(DEFAULT_OBLIQUE_ANGLE_DEG),
    _ => None,
  };
  let ital_needed = matches!(style.font_style, CssFontStyle::Italic)
    || (matches!(style.font_style, CssFontStyle::Oblique(_)) && !has_slnt_axis);

  for axis in axes {
    if axis.tag == wght_tag && !set_tags.contains(&wght_tag) {
      let wght_value = (style.font_weight.to_u16() as f32).clamp(axis.min_value, axis.max_value);
      variations.push(Variation {
        tag: wght_tag,
        value: wght_value,
      });
      set_tags.insert(wght_tag);
    } else if axis.tag == wdth_tag && !set_tags.contains(&wdth_tag) {
      let wdth_value = style
        .font_stretch
        .to_percentage()
        .clamp(axis.min_value, axis.max_value);
      variations.push(Variation {
        tag: wdth_tag,
        value: wdth_value,
      });
      set_tags.insert(wdth_tag);
    } else if axis.tag == opsz_tag
      && !set_tags.contains(&opsz_tag)
      && matches!(
        style.font_optical_sizing,
        crate::style::types::FontOpticalSizing::Auto
      )
    {
      let opsz_value = font_size.clamp(axis.min_value, axis.max_value);
      variations.push(Variation {
        tag: opsz_tag,
        value: opsz_value,
      });
      set_tags.insert(opsz_tag);
    } else if axis.tag == slnt_tag && !set_tags.contains(&slnt_tag) {
      if let Some(angle) = slnt_angle {
        let slnt_value = (-angle).clamp(axis.min_value, axis.max_value);
        variations.push(Variation {
          tag: slnt_tag,
          value: slnt_value,
        });
        set_tags.insert(slnt_tag);
      }
    } else if axis.tag == ital_tag && !set_tags.contains(&ital_tag) {
      if ital_needed {
        variations.push(Variation {
          tag: ital_tag,
          value: 1.0,
        });
        set_tags.insert(ital_tag);
      } else if matches!(style.font_style, CssFontStyle::Normal) {
        variations.push(Variation {
          tag: ital_tag,
          value: 0.0,
        });
        set_tags.insert(ital_tag);
      }
    }
  }

  variations
}

#[allow(clippy::too_many_arguments)]
fn push_font_run(
  out: &mut Vec<FontRun>,
  run: &ItemizedRun,
  start: usize,
  end: usize,
  font: Arc<LoadedFont>,
  synthetic_bold: f32,
  synthetic_oblique: f32,
  font_size: f32,
  baseline_shift: f32,
  features: &[Feature],
  authored_variations: &[Variation],
  style: &ComputedStyle,
  _font_context: &FontContext,
) {
  let segment_text = &run.text[start..end];
  let variations = crate::text::face_cache::with_face(&font, |face| {
    collect_variations_for_face(face, style, font_size, authored_variations)
  })
  .unwrap_or_else(|| authored_variations.to_vec());

  let resolved_palette = resolve_font_palette_for_font(
    &style.font_palette,
    &style.font_palettes,
    &font.family,
    style.color,
  );
  let palette_index = select_palette_index(&font, resolved_palette.base);
  let palette_overrides = Arc::new(resolved_palette.overrides);

  let language = match &style.font_language_override {
    FontLanguageOverride::Normal => style.language.clone(),
    FontLanguageOverride::Override(tag) => tag.clone(),
  };

  out.push(FontRun {
    text: segment_text.to_string(),
    start: run.start + start,
    end: run.start + end,
    font,
    synthetic_bold,
    synthetic_oblique,
    script: run.script,
    direction: run.direction,
    level: run.level,
    font_size,
    baseline_shift,
    language,
    features: features.to_vec(),
    variations,
    palette_index,
    palette_overrides: Arc::clone(&palette_overrides),
    palette_override_hash: resolved_palette.override_hash,
    rotation: RunRotation::None,
    vertical: false,
  });
}

fn select_palette_index(font: &LoadedFont, base: FontPaletteBase) -> u16 {
  crate::text::face_cache::with_face(font, |face| select_cpal_palette(face, base)).unwrap_or(0)
}

// ============================================================================
// Text Shaping
// ============================================================================

/// Information about a single glyph.
#[derive(Debug, Clone, Copy)]
pub struct GlyphPosition {
  /// Glyph ID in the font.
  pub glyph_id: u32,
  /// Cluster index (maps to character position in original text).
  pub cluster: u32,
  /// X position relative to run start.
  pub x_offset: f32,
  /// Y offset from baseline.
  pub y_offset: f32,
  /// Horizontal advance (distance to next glyph).
  pub x_advance: f32,
  /// Vertical advance (usually 0 for horizontal text).
  pub y_advance: f32,
}

/// A shaped run of text, ready for rendering.
#[derive(Debug, Clone)]
pub struct ShapedRun {
  /// The original text.
  pub text: String,
  /// Start byte index in original text.
  pub start: usize,
  /// End byte index in original text.
  pub end: usize,
  /// Positioned glyphs.
  pub glyphs: Vec<GlyphPosition>,
  /// Text direction.
  pub direction: Direction,
  /// Bidi level.
  pub level: u8,
  /// Total horizontal advance of this run.
  pub advance: f32,
  /// Font used for this run.
  pub font: Arc<LoadedFont>,
  /// Font size in pixels.
  pub font_size: f32,
  /// Additional baseline shift in pixels (positive raises text).
  pub baseline_shift: f32,
  /// Language set on the shaping buffer (if provided).
  pub language: Option<HbLanguage>,
  /// Synthetic bold stroke width in pixels (0 = none).
  pub synthetic_bold: f32,
  /// Synthetic oblique shear factor (tan(angle); 0 = none).
  pub synthetic_oblique: f32,
  /// Optional rotation to apply when painting.
  pub rotation: RunRotation,

  /// Palette index for color glyph rendering.
  pub palette_index: u16,
  /// Palette overrides for color glyph rendering.
  pub palette_overrides: Arc<Vec<(u16, Rgba)>>,
  /// Stable hash of palette overrides for cache keys.
  pub palette_override_hash: u64,

  /// Active variation settings for the run (used for cache keys).
  pub variations: Vec<Variation>,

  /// Optional additional scale factor (1.0 = none).
  pub scale: f32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunRotation {
  None,
  Ccw90,
  Cw90,
}

impl ShapedRun {
  /// Returns true if this run is empty.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.glyphs.is_empty()
  }

  /// Returns the number of glyphs.
  #[inline]
  pub fn glyph_count(&self) -> usize {
    self.glyphs.len()
  }
}

fn mirror_text_for_direction<'a>(
  text: &'a str,
  direction: Direction,
) -> (Cow<'a, str>, Option<Vec<(usize, usize)>>) {
  if !direction.is_rtl() {
    return (Cow::Borrowed(text), None);
  }

  let mut mirrored = String::with_capacity(text.len());
  let mut mapping: Vec<(usize, usize)> = Vec::new();
  let mut changed = false;

  for (orig_idx, ch) in text.char_indices() {
    let mapped = get_mirrored(ch).unwrap_or(ch);
    changed |= mapped != ch;
    mapping.push((mirrored.len(), orig_idx));
    mirrored.push(mapped);
  }

  if changed {
    (Cow::Owned(mirrored), Some(mapping))
  } else {
    (Cow::Borrowed(text), None)
  }
}

fn map_cluster_offset(cluster: usize, mapping: &[(usize, usize)]) -> usize {
  match mapping.binary_search_by_key(&cluster, |(shaped, _)| *shaped) {
    Ok(idx) => mapping.get(idx).map(|(_, orig)| *orig).unwrap_or(cluster),
    Err(0) => mapping.first().map(|(_, orig)| *orig).unwrap_or(cluster),
    Err(idx) => mapping
      .get(idx.saturating_sub(1))
      .map(|(_, orig)| *orig)
      .unwrap_or(cluster),
  }
}

/// Converts HarfBuzz glyph positioning into engine coordinates.
///
/// Vertical runs keep HarfBuzz's cross-axis offset on `x_offset` and inline offset on
/// `y_offset`, applying `baseline_shift` along the cross axis.
fn map_hb_position(
  vertical: bool,
  baseline_shift: f32,
  scale: f32,
  pos: &rustybuzz::GlyphPosition,
) -> (f32, f32, f32, f32) {
  let mut inline_advance_raw = if vertical {
    pos.y_advance
  } else {
    pos.x_advance
  };
  if vertical && inline_advance_raw == 0 {
    inline_advance_raw = pos.x_advance;
  }
  let cross_advance_raw = if vertical {
    pos.x_advance
  } else {
    pos.y_advance
  };
  let inline_offset_raw = if vertical { pos.y_offset } else { pos.x_offset };
  let cross_offset_raw = if vertical { pos.x_offset } else { pos.y_offset };

  let mut inline_advance = inline_advance_raw as f32 * scale;
  if vertical {
    inline_advance = inline_advance.abs();
  }
  let cross_advance = cross_advance_raw as f32 * scale;

  if vertical {
    let x_offset = cross_offset_raw as f32 * scale + baseline_shift;
    let y_offset = inline_offset_raw as f32 * scale;
    (x_offset, y_offset, cross_advance, inline_advance)
  } else {
    let x_offset = inline_offset_raw as f32 * scale;
    let y_offset = cross_offset_raw as f32 * scale + baseline_shift;
    (x_offset, y_offset, inline_advance, cross_advance)
  }
}

pub(crate) fn notdef_advance_for_font(font: &LoadedFont, font_size: f32) -> f32 {
  let (units_per_em, notdef_advance) = crate::text::face_cache::with_face(font, |face| {
    (
      face.units_per_em() as f32,
      face
        .glyph_hor_advance(ttf_parser::GlyphId(0))
        .unwrap_or(face.units_per_em()) as f32,
    )
  })
  .unwrap_or((0.0, 0.0));

  let mut inline_advance = if units_per_em > 0.0 {
    notdef_advance * (font_size / units_per_em)
  } else {
    0.0
  };

  if !inline_advance.is_finite() || inline_advance <= 0.0 {
    inline_advance = font_size * 0.5;
  }

  inline_advance
}

fn fallback_notdef_advance(run: &FontRun) -> f32 {
  notdef_advance_for_font(&run.font, run.font_size)
}

fn synthesize_notdef_run(run: &FontRun) -> ShapedRun {
  let inline_advance = fallback_notdef_advance(run);

  let mut glyphs = Vec::new();
  let mut advance = 0.0_f32;
  for (cluster, ch) in run.text.char_indices() {
    if is_bidi_control_char(ch) {
      continue;
    }
    let (x_offset, y_offset, x_advance, y_advance) = if run.vertical {
      (run.baseline_shift, 0.0, 0.0, inline_advance)
    } else {
      (0.0, run.baseline_shift, inline_advance, 0.0)
    };
    glyphs.push(GlyphPosition {
      glyph_id: 0,
      cluster: cluster as u32,
      x_offset,
      y_offset,
      x_advance,
      y_advance,
    });
    advance += if run.vertical { y_advance } else { x_advance };
  }

  ShapedRun {
    text: run.text.clone(),
    start: run.start,
    end: run.end,
    glyphs,
    direction: run.direction,
    level: run.level,
    advance,
    font: Arc::clone(&run.font),
    font_size: run.font_size,
    baseline_shift: run.baseline_shift,
    language: None,
    synthetic_bold: run.synthetic_bold,
    synthetic_oblique: run.synthetic_oblique,
    rotation: run.rotation,
    palette_index: run.palette_index,
    palette_overrides: Arc::clone(&run.palette_overrides),
    palette_override_hash: run.palette_override_hash,
    variations: run.variations.clone(),
    scale: 1.0,
  }
}

/// Shapes a single font run into positioned glyphs.
fn shape_font_run(run: &FontRun) -> Result<ShapedRun> {
  #[cfg(any(test, debug_assertions))]
  SHAPE_FONT_RUN_INVOCATIONS.fetch_add(1, Ordering::Relaxed);

  // Create rustybuzz face from cached font data to avoid reparsing per run.
  let cached_face = crate::text::face_cache::get_rustybuzz_face(&run.font).ok_or_else(|| {
    TextError::ShapingFailed {
      text: run.text.clone(),
      reason: "Failed to create HarfBuzz face".to_string(),
    }
  })?;
  let mut rb_face = cached_face.clone_face();
  if !run.variations.is_empty() {
    rb_face.set_variations(&run.variations);
  }

  // Create Unicode buffer
  let mut buffer = UnicodeBuffer::new();

  let (shape_text, cluster_map_override) = mirror_text_for_direction(&run.text, run.direction);
  buffer.push_str(&shape_text);

  let mut language: Option<HbLanguage> = None;

  // Set buffer properties
  let hb_direction = if run.vertical {
    HbDirection::TopToBottom
  } else {
    run.direction.to_harfbuzz()
  };
  buffer.set_direction(hb_direction);
  if let Some(script) = run.script.to_harfbuzz() {
    buffer.set_script(script);
  }
  let lang_tag = run.language.trim();
  if !lang_tag.is_empty() {
    if let Ok(lang) = HbLanguage::from_str(lang_tag) {
      buffer.set_language(lang.clone());
      language = Some(lang);
    }
  }

  let mut features = run.features.clone();
  if run.vertical {
    let need_vert = !features
      .iter()
      .any(|f| f.tag.to_bytes() == *b"vert" && f.value != 0);
    let need_vrt2 = !features
      .iter()
      .any(|f| f.tag.to_bytes() == *b"vrt2" && f.value != 0);
    if need_vert {
      features.push(Feature {
        tag: Tag::from_bytes(b"vert"),
        value: 1,
        start: 0,
        end: u32::MAX,
      });
    }
    if need_vrt2 {
      features.push(Feature {
        tag: Tag::from_bytes(b"vrt2"),
        value: 1,
        start: 0,
        end: u32::MAX,
      });
    }
  }

  // Shape the text
  let output = rustybuzz::shape(&rb_face, &features, buffer);

  // Calculate scale factor
  let units_per_em = rb_face.units_per_em() as f32;
  let scale = run.font_size / units_per_em;

  // Extract glyph information
  let glyph_infos = output.glyph_infos();
  let glyph_positions = output.glyph_positions();

  let mut glyphs = Vec::with_capacity(glyph_infos.len());
  let mut inline_position = 0.0_f32;
  let mark_only = is_mark_only_cluster(&run.text);

  for (info, pos) in glyph_infos.iter().zip(glyph_positions.iter()) {
    let cluster_in_shape = info.cluster as usize;
    let logical_cluster = cluster_map_override
      .as_ref()
      .map(|map| map_cluster_offset(cluster_in_shape, map))
      .unwrap_or(cluster_in_shape);
    let char_at_cluster = run
      .text
      .get(logical_cluster..)
      .and_then(|s| s.chars().next());
    let is_bidi_control = char_at_cluster.is_some_and(is_bidi_control_char);

    if !is_bidi_control {
      if info.glyph_id == 0 && !mark_only && char_at_cluster.is_some_and(is_unicode_mark) {
        continue;
      }

      let (x_offset, y_offset, x_advance, y_advance) =
        map_hb_position(run.vertical, run.baseline_shift, scale, pos);

      glyphs.push(GlyphPosition {
        glyph_id: info.glyph_id,
        cluster: logical_cluster as u32,
        x_offset,
        y_offset,
        x_advance,
        y_advance,
      });
      inline_position += if run.vertical { y_advance } else { x_advance };
    }
  }

  if inline_position.abs() <= f32::EPSILON && !glyphs.is_empty() && mark_only {
    // HarfBuzz gives combining marks zero advance because they normally attach to a base glyph.
    // When the entire run is marks (e.g. standalone Arabic diacritics), that would collapse the
    // run to zero width and allow higher-level layout to drop it entirely. Ensure mark-only runs
    // remain visible by assigning a fallback advance to the final glyph.
    let fallback_advance = fallback_notdef_advance(run);
    if run.vertical {
      if let Some(last) = glyphs.last_mut() {
        last.y_advance = fallback_advance;
      }
    } else if let Some(last) = glyphs.last_mut() {
      last.x_advance = fallback_advance;
    }
    inline_position = fallback_advance;
  }

  Ok(ShapedRun {
    text: run.text.clone(),
    start: run.start,
    end: run.end,
    glyphs,
    direction: run.direction,
    level: run.level,
    advance: inline_position,
    font: Arc::clone(&run.font),
    font_size: run.font_size,
    baseline_shift: run.baseline_shift,
    language,
    synthetic_bold: run.synthetic_bold,
    synthetic_oblique: run.synthetic_oblique,
    rotation: run.rotation,
    palette_index: run.palette_index,
    palette_overrides: Arc::clone(&run.palette_overrides),
    palette_override_hash: run.palette_override_hash,
    variations: run.variations.clone(),
    scale: 1.0,
  })
}

// ============================================================================
// Shaping Pipeline
// ============================================================================

/// The main text shaping pipeline.
///
/// Coordinates bidi analysis, script itemization, font matching, and text shaping
/// into a unified process.
///
/// # Example
///
/// ```rust,ignore
/// let pipeline = ShapingPipeline::new();
/// let font_context = FontContext::new();
/// let style = ComputedStyle::default();
///
/// let runs = pipeline.shape("Hello, world!", &style, &font_context)?;
/// ```
#[derive(Debug, Clone)]
pub struct ShapingPipeline {
  cache: ShapingCache,
  font_cache: FallbackCache,
}

impl Default for ShapingPipeline {
  fn default() -> Self {
    Self::new()
  }
}

#[derive(Clone, Eq, Debug)]
struct ShapingCacheKey {
  text: std::sync::Arc<str>,
  style_hash: u64,
  font_generation: u64,
}

pub(crate) fn shaping_style_hash(style: &ComputedStyle) -> u64 {
  use std::hash::Hash;
  use std::hash::Hasher;
  let mut hasher = std::collections::hash_map::DefaultHasher::new();

  std::mem::discriminant(&style.direction).hash(&mut hasher);
  std::mem::discriminant(&style.unicode_bidi).hash(&mut hasher);
  std::mem::discriminant(&style.writing_mode).hash(&mut hasher);
  std::mem::discriminant(&style.text_orientation).hash(&mut hasher);
  style.language.hash(&mut hasher);
  style.font_family.hash(&mut hasher);
  style.font_size.to_bits().hash(&mut hasher);
  style.font_weight.to_u16().hash(&mut hasher);
  style
    .font_stretch
    .to_percentage()
    .to_bits()
    .hash(&mut hasher);

  match style.font_style {
    CssFontStyle::Normal => 0u8.hash(&mut hasher),
    CssFontStyle::Italic => 1u8.hash(&mut hasher),
    CssFontStyle::Oblique(angle) => {
      2u8.hash(&mut hasher);
      angle.unwrap_or_default().to_bits().hash(&mut hasher);
    }
  }

  // Palette overrides can resolve currentColor, so cache entries must vary with text color.
  style.color.r.hash(&mut hasher);
  style.color.g.hash(&mut hasher);
  style.color.b.hash(&mut hasher);
  style.color.a.to_bits().hash(&mut hasher);

  match &style.font_palette {
    FontPalette::Normal => 0u8.hash(&mut hasher),
    FontPalette::Light => 1u8.hash(&mut hasher),
    FontPalette::Dark => 2u8.hash(&mut hasher),
    FontPalette::Named(name) => {
      3u8.hash(&mut hasher);
      name.hash(&mut hasher);
    }
  }
  (Arc::as_ptr(&style.font_palettes) as usize).hash(&mut hasher);

  std::mem::discriminant(&style.font_variant).hash(&mut hasher);
  std::mem::discriminant(&style.font_variant_caps).hash(&mut hasher);
  std::mem::discriminant(&style.font_variant_emoji).hash(&mut hasher);
  std::mem::discriminant(&style.font_variant_position).hash(&mut hasher);
  std::mem::discriminant(&style.font_variant_numeric.figure).hash(&mut hasher);
  std::mem::discriminant(&style.font_variant_numeric.spacing).hash(&mut hasher);
  std::mem::discriminant(&style.font_variant_numeric.fraction).hash(&mut hasher);
  (style.font_variant_numeric.ordinal as u8).hash(&mut hasher);
  (style.font_variant_numeric.slashed_zero as u8).hash(&mut hasher);

  (style.font_variant_ligatures.common as u8).hash(&mut hasher);
  (style.font_variant_ligatures.discretionary as u8).hash(&mut hasher);
  (style.font_variant_ligatures.historical as u8).hash(&mut hasher);
  (style.font_variant_ligatures.contextual as u8).hash(&mut hasher);

  style
    .font_variant_alternates
    .historical_forms
    .hash(&mut hasher);
  style.font_variant_alternates.stylistic.hash(&mut hasher);
  style.font_variant_alternates.stylesets.hash(&mut hasher);

  match style.font_variant_east_asian.variant {
    Some(var) => {
      1u8.hash(&mut hasher);
      std::mem::discriminant(&var).hash(&mut hasher);
    }
    None => 0u8.hash(&mut hasher),
  }
  match style.font_variant_east_asian.width {
    Some(width) => {
      1u8.hash(&mut hasher);
      std::mem::discriminant(&width).hash(&mut hasher);
    }
    None => 0u8.hash(&mut hasher),
  }
  (style.font_variant_east_asian.ruby as u8).hash(&mut hasher);

  std::mem::discriminant(&style.font_kerning).hash(&mut hasher);
  (style.font_synthesis.weight as u8).hash(&mut hasher);
  (style.font_synthesis.style as u8).hash(&mut hasher);
  (style.font_synthesis.small_caps as u8).hash(&mut hasher);
  (style.font_synthesis.position as u8).hash(&mut hasher);
  std::mem::discriminant(&style.font_optical_sizing).hash(&mut hasher);

  match style.font_language_override.clone() {
    FontLanguageOverride::Normal => 0u8.hash(&mut hasher),
    FontLanguageOverride::Override(tag) => {
      1u8.hash(&mut hasher);
      tag.hash(&mut hasher);
    }
  }

  match style.font_size_adjust {
    FontSizeAdjust::None => 0u8.hash(&mut hasher),
    FontSizeAdjust::Number(v) => {
      1u8.hash(&mut hasher);
      v.to_bits().hash(&mut hasher);
    }
    FontSizeAdjust::FromFont => 2u8.hash(&mut hasher),
  }

  for setting in &style.font_feature_settings {
    setting.tag.hash(&mut hasher);
    setting.value.hash(&mut hasher);
  }
  for setting in &style.font_variation_settings {
    setting.tag.hash(&mut hasher);
    setting.value.to_bits().hash(&mut hasher);
  }

  hasher.finish()
}

impl std::hash::Hash for ShapingCacheKey {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.style_hash.hash(state);
    self.font_generation.hash(state);
    self.text.hash(state);
  }
}

impl PartialEq for ShapingCacheKey {
  fn eq(&self, other: &Self) -> bool {
    self.style_hash == other.style_hash
      && self.font_generation == other.font_generation
      && self.text.as_ref() == other.text.as_ref()
  }
}

#[cfg(any(test, debug_assertions))]
#[derive(Default, Debug)]
struct ShapingCacheStats {
  hits: AtomicUsize,
  misses: AtomicUsize,
  evictions: AtomicUsize,
}

#[cfg(any(test, debug_assertions))]
#[derive(Debug, Default, PartialEq, Eq)]
struct ShapingCacheStatsSnapshot {
  hits: usize,
  misses: usize,
  evictions: usize,
}

#[cfg(any(test, debug_assertions))]
impl ShapingCacheStats {
  fn snapshot(&self) -> ShapingCacheStatsSnapshot {
    ShapingCacheStatsSnapshot {
      hits: self.hits.load(Ordering::Relaxed),
      misses: self.misses.load(Ordering::Relaxed),
      evictions: self.evictions.load(Ordering::Relaxed),
    }
  }

  fn clear(&self) {
    self.hits.store(0, Ordering::Relaxed);
    self.misses.store(0, Ordering::Relaxed);
    self.evictions.store(0, Ordering::Relaxed);
  }
}

#[derive(Clone, Debug)]
struct ShapingCache {
  entries: Arc<Mutex<LruCache<ShapingCacheKey, Arc<Vec<ShapedRun>>, ShapingCacheHasher>>>,
  #[cfg(any(test, debug_assertions))]
  stats: Arc<ShapingCacheStats>,
}

impl ShapingCache {
  fn new(capacity: usize) -> Self {
    let cap = NonZeroUsize::new(capacity).unwrap_or_else(|| NonZeroUsize::new(1).unwrap());
    Self {
      entries: Arc::new(Mutex::new(LruCache::with_hasher(
        cap,
        ShapingCacheHasher::default(),
      ))),
      #[cfg(any(test, debug_assertions))]
      stats: Arc::new(ShapingCacheStats::default()),
    }
  }

  fn get(&self, key: &ShapingCacheKey) -> Option<Arc<Vec<ShapedRun>>> {
    let result = self
      .entries
      .lock()
      .ok()
      .and_then(|mut cache| cache.get(key).cloned());
    #[cfg(any(test, debug_assertions))]
    {
      if result.is_some() {
        self.stats.hits.fetch_add(1, Ordering::Relaxed);
      } else {
        self.stats.misses.fetch_add(1, Ordering::Relaxed);
      }
    }
    result
  }

  fn insert(&self, key: ShapingCacheKey, value: Arc<Vec<ShapedRun>>) -> Arc<Vec<ShapedRun>> {
    if let Ok(mut cache) = self.entries.lock() {
      if let Some(existing) = cache.peek(&key).cloned() {
        return existing;
      }
      #[cfg(any(test, debug_assertions))]
      let at_capacity = cache.len() >= cache.cap().get();
      let evicted = cache.put(key, Arc::clone(&value));
      #[cfg(any(test, debug_assertions))]
      if evicted.is_some() || at_capacity {
        self.stats.evictions.fetch_add(1, Ordering::Relaxed);
      }
    }
    value
  }

  fn clear(&self) {
    if let Ok(mut cache) = self.entries.lock() {
      cache.clear();
    }
    #[cfg(any(test, debug_assertions))]
    self.stats.clear();
  }

  #[cfg(any(test, debug_assertions))]
  fn len(&self) -> usize {
    self.entries.lock().map(|cache| cache.len()).unwrap_or(0)
  }

  #[cfg(any(test, debug_assertions))]
  fn stats(&self) -> ShapingCacheStatsSnapshot {
    self.stats.snapshot()
  }
}

impl ShapingPipeline {
  /// Creates a new shaping pipeline.
  pub fn new() -> Self {
    Self {
      cache: ShapingCache::new(SHAPING_CACHE_CAPACITY),
      font_cache: FallbackCache::new(FONT_RESOLUTION_CACHE_SIZE),
    }
  }

  #[cfg(test)]
  fn with_cache_capacity_for_test(capacity: usize) -> Self {
    Self {
      cache: ShapingCache::new(capacity),
      font_cache: FallbackCache::new(FONT_RESOLUTION_CACHE_SIZE),
    }
  }

  /// Clears the shaping cache. Useful when reusing a pipeline across multiple documents.
  pub fn clear_cache(&self) {
    self.cache.clear();
    self.font_cache.clear();
  }

  #[cfg(any(test, debug_assertions))]
  fn cache_stats(&self) -> ShapingCacheStatsSnapshot {
    self.cache.stats()
  }

  #[cfg(any(test, debug_assertions))]
  fn fallback_cache_stats(&self) -> FallbackCacheStatsSnapshot {
    self.font_cache.stats()
  }

  /// Returns the number of cached shaped runs currently stored.
  pub fn cache_len(&self) -> usize {
    self
      .cache
      .entries
      .lock()
      .map(|cache| cache.len())
      .unwrap_or(0)
  }

  /// Shapes text into positioned glyphs.
  ///
  /// This is the main entry point for text shaping. It performs:
  /// 1. Bidi analysis
  /// 2. Script itemization
  /// 3. Font matching
  /// 4. Text shaping
  ///
  /// # Arguments
  ///
  /// * `text` - The text to shape
  /// * `style` - Computed style containing font properties
  /// * `font_context` - Font context for font resolution
  ///
  /// # Returns
  ///
  /// A vector of shaped runs, ready for rendering.
  ///
  /// # Errors
  ///
  /// Returns an error if font matching or shaping fails.
  pub fn shape(
    &self,
    text: &str,
    style: &ComputedStyle,
    font_context: &FontContext,
  ) -> Result<Vec<ShapedRun>> {
    self.shape_core(text, style, font_context, None, None)
  }

  fn shape_core(
    &self,
    text: &str,
    style: &ComputedStyle,
    font_context: &FontContext,
    base_direction: Option<Direction>,
    explicit_bidi: Option<ExplicitBidiContext>,
  ) -> Result<Vec<ShapedRun>> {
    // Handle empty text
    if text.is_empty() {
      return Ok(Vec::new());
    }

    let diag_enabled = text_diagnostics_enabled();
    let diag_timer = text_diagnostics_timer();

    if matches!(style.font_variant, FontVariant::SmallCaps)
      || matches!(
        style.font_variant_caps,
        FontVariantCaps::SmallCaps | FontVariantCaps::AllSmallCaps
      )
    {
      if !has_native_small_caps(style, font_context) && style.font_synthesis.small_caps {
        return self.shape_small_caps(text, style, font_context, base_direction, explicit_bidi);
      }
    }

    let cache_text = Arc::<str>::from(text);
    let style_hash = shaping_style_hash(style);
    let font_generation = font_context.font_generation();
    let cache_key = ShapingCacheKey {
      text: cache_text.clone(),
      style_hash,
      font_generation,
    };
    if let Some(cached) = self.cache.get(&cache_key) {
      let shaped_runs = cached.as_ref().clone();
      if let Some(start) = diag_timer {
        let glyphs: usize = shaped_runs.iter().map(|run| run.glyphs.len()).sum();
        record_text_shape(Some(start), shaped_runs.len(), glyphs);
      }
      return Ok(shaped_runs);
    }

    let cache_stats_before = diag_enabled.then(|| self.font_cache.stats());

    // Step 1: Bidi analysis
    let mut resolved_base_dir = base_direction.unwrap_or(match style.direction {
      crate::style::types::Direction::Ltr => Direction::LeftToRight,
      crate::style::types::Direction::Rtl => Direction::RightToLeft,
    });
    let mut bidi_context = explicit_bidi;

    // text-orientation: upright sets the used direction to ltr and treats all characters as
    // strong LTR for bidi reordering in vertical typographic modes per CSS Writing Modes.
    if is_vertical_typographic_mode(style.writing_mode)
      && matches!(
        style.text_orientation,
        crate::style::types::TextOrientation::Upright
      )
    {
      resolved_base_dir = Direction::LeftToRight;
      let level = Level::ltr();
      let mut ctx = bidi_context.unwrap_or(ExplicitBidiContext {
        level,
        override_all: false,
      });
      ctx.level = level;
      ctx.override_all = true;
      bidi_context = Some(ctx);
    }

    let bidi = BidiAnalysis::analyze_with_base(text, style, resolved_base_dir, bidi_context);

    // Step 2: Script itemization
    let itemized_runs = itemize_text(text, &bidi);
    let itemized_runs = split_itemized_runs_by_paragraph(itemized_runs, bidi.paragraphs());

    // Step 3: Font matching
    let coverage_timer = text_diagnostics_timer();
    let font_runs = assign_fonts_internal(
      &itemized_runs,
      style,
      font_context,
      Some(&self.font_cache),
      font_generation,
    )?;
    record_text_coverage(coverage_timer);

    // Step 4: Shape each run, applying vertical text-orientation when needed.
    let mut font_runs = font_runs;
    if is_vertical_typographic_mode(style.writing_mode) {
      font_runs = apply_vertical_text_orientation(font_runs, style.text_orientation);
    } else if is_sideways_writing_mode(style.writing_mode) {
      font_runs = apply_sideways_text_orientation(font_runs);
    }

    let shape_timer = text_diagnostics_timer();
    let mut shaped_runs = Vec::with_capacity(font_runs.len());
    for run in &font_runs {
      let mut shaped = match shape_font_run(run) {
        Ok(shaped) => shaped,
        Err(err) => {
          if text_diagnostics_verbose_logging() {
            let idx = SHAPING_FALLBACK_LOGGED.fetch_add(1, Ordering::Relaxed);
            if idx < LAST_RESORT_SAMPLE_LIMIT {
              eprintln!(
                "FASTR_TEXT_DIAGNOSTICS: shaping failed for run {}; using notdef placeholders: {}",
                format_codepoints_for_log(&run.text),
                err
              );
            }
          }
          synthesize_notdef_run(run)
        }
      };
      shaped.rotation = run.rotation;
      shaped_runs.push(shaped);
    }

    // Step 5: Reorder for bidi if needed
    if bidi.needs_reordering() {
      reorder_runs(&mut shaped_runs, bidi.paragraphs());
    }

    self.cache.insert(cache_key, Arc::new(shaped_runs.clone()));

    if let Some(start) = shape_timer.or(diag_timer) {
      let glyphs: usize = shaped_runs.iter().map(|run| run.glyphs.len()).sum();
      record_text_shape(Some(start), shaped_runs.len(), glyphs);
    }
    if let (Some(before), Some(after)) = (
      cache_stats_before,
      diag_enabled.then(|| self.font_cache.stats()),
    ) {
      record_fallback_cache_stats_delta(before, after);
    }

    Ok(shaped_runs)
  }

  /// Shapes text with explicit direction override.
  ///
  /// Useful when the CSS direction property is known.
  pub fn shape_with_direction(
    &self,
    text: &str,
    style: &ComputedStyle,
    font_context: &FontContext,
    base_direction: Direction,
  ) -> Result<Vec<ShapedRun>> {
    self.shape_core(text, style, font_context, Some(base_direction), None)
  }

  /// Shapes text with an explicit bidi context (embedding level/override).
  pub fn shape_with_context(
    &self,
    text: &str,
    style: &ComputedStyle,
    font_context: &FontContext,
    base_direction: Direction,
    bidi_context: Option<ExplicitBidiContext>,
  ) -> Result<Vec<ShapedRun>> {
    self.shape_core(
      text,
      style,
      font_context,
      Some(base_direction),
      bidi_context,
    )
  }

  /// Measures the total advance width of shaped text.
  ///
  /// Convenience method that shapes text and returns only the width.
  pub fn measure_width(
    &self,
    text: &str,
    style: &ComputedStyle,
    font_context: &FontContext,
  ) -> Result<f32> {
    let runs = self.shape(text, style, font_context)?;
    Ok(runs.iter().map(|r| r.advance).sum())
  }

  fn shape_small_caps(
    &self,
    text: &str,
    style: &ComputedStyle,
    font_context: &FontContext,
    base_direction: Option<Direction>,
    explicit_bidi: Option<ExplicitBidiContext>,
  ) -> Result<Vec<ShapedRun>> {
    const SMALL_CAPS_SCALE: f32 = 0.8;

    let mut runs = Vec::new();
    let mut segment_start: usize = 0;
    let mut buffer = String::new();
    let mut current_small = None;
    let all_small = matches!(style.font_variant_caps, FontVariantCaps::AllSmallCaps);

    for (idx, ch) in text.char_indices() {
      let is_small = ch.is_lowercase() || (all_small && ch.is_uppercase());
      if let Some(flag) = current_small {
        if flag != is_small {
          self.flush_small_caps_segment(
            &mut runs,
            &buffer,
            segment_start,
            flag,
            style,
            font_context,
            SMALL_CAPS_SCALE,
            base_direction,
            explicit_bidi,
          )?;
          buffer.clear();
          segment_start = idx;
          current_small = Some(is_small);
        }
      } else {
        current_small = Some(is_small);
      }

      if is_small {
        for up in ch.to_uppercase() {
          buffer.push(up);
        }
      } else {
        buffer.push(ch);
      }
    }

    if !buffer.is_empty() {
      self.flush_small_caps_segment(
        &mut runs,
        &buffer,
        segment_start,
        current_small.unwrap_or(false),
        style,
        font_context,
        SMALL_CAPS_SCALE,
        base_direction,
        explicit_bidi,
      )?;
    }

    Ok(runs)
  }

  fn flush_small_caps_segment(
    &self,
    out: &mut Vec<ShapedRun>,
    segment_text: &str,
    base_offset: usize,
    is_small: bool,
    style: &ComputedStyle,
    font_context: &FontContext,
    scale: f32,
    base_direction: Option<Direction>,
    explicit_bidi: Option<ExplicitBidiContext>,
  ) -> Result<()> {
    let mut seg_style = style.clone();
    seg_style.font_variant = FontVariant::Normal;
    seg_style.font_variant_caps = FontVariantCaps::Normal;
    if is_small {
      seg_style.font_size *= scale;
    }
    let mut shaped = self.shape_core(
      segment_text,
      &seg_style,
      font_context,
      base_direction,
      explicit_bidi,
    )?;
    for run in &mut shaped {
      run.start += base_offset;
      run.end += base_offset;
    }
    out.extend(shaped);
    Ok(())
  }
}

/// Reorders shaped runs for bidi display.
///
/// Implements visual reordering based on bidi levels.
fn reorder_runs(runs: &mut [ShapedRun], paragraphs: &[ParagraphBoundary]) {
  if runs.is_empty() {
    return;
  }

  fn reorder_slice(slice: &mut [ShapedRun]) {
    if slice.is_empty() {
      return;
    }

    let max_level = slice.iter().map(|r| r.level).max().unwrap_or(0);
    for level in (1..=max_level).rev() {
      let mut start: Option<usize> = None;
      for i in 0..slice.len() {
        if slice[i].level >= level {
          start.get_or_insert(i);
        } else if let Some(s) = start {
          slice[s..i].reverse();
          start = None;
        }
      }
      if let Some(s) = start {
        slice[s..].reverse();
      }
    }
  }

  if paragraphs.is_empty() {
    reorder_slice(runs);
    return;
  }

  let mut idx = 0;
  for para in paragraphs {
    while idx < runs.len() && runs[idx].end <= para.start_byte {
      idx += 1;
    }
    let mut end = idx;
    while end < runs.len() && runs[end].start < para.end_byte {
      end += 1;
    }
    if idx < end {
      reorder_slice(&mut runs[idx..end]);
    }
    idx = end;
  }

  if idx < runs.len() {
    reorder_slice(&mut runs[idx..]);
  }
}

// ============================================================================
// Cluster Mapping
// ============================================================================

#[cfg(test)]
static CLUSTER_MAP_CHAR_ITERATIONS: AtomicUsize = AtomicUsize::new(0);

#[cfg(test)]
#[inline]
fn reset_cluster_map_char_iterations() {
  CLUSTER_MAP_CHAR_ITERATIONS.store(0, Ordering::Relaxed);
}

#[cfg(not(test))]
#[inline]
fn reset_cluster_map_char_iterations() {}

#[cfg(test)]
#[inline]
fn record_cluster_map_char_iteration() {
  CLUSTER_MAP_CHAR_ITERATIONS.fetch_add(1, Ordering::Relaxed);
}

#[cfg(not(test))]
#[inline]
fn record_cluster_map_char_iteration() {}

#[cfg(test)]
#[inline]
fn cluster_map_char_iterations() -> usize {
  CLUSTER_MAP_CHAR_ITERATIONS.load(Ordering::Relaxed)
}

/// Maps between character positions and glyph positions.
///
/// Needed for hit testing, selection, and cursor positioning.
#[derive(Debug, Clone)]
pub struct ClusterMap {
  /// Maps character index to glyph index.
  char_to_glyph: Vec<usize>,
  /// Maps glyph index to character index.
  glyph_to_char: Vec<usize>,
}

impl ClusterMap {
  /// Builds a cluster map from a shaped run.
  pub fn from_shaped_run(run: &ShapedRun) -> Self {
    reset_cluster_map_char_iterations();

    let mut char_offsets = Vec::new();
    for (byte_idx, _) in run.text.char_indices() {
      record_cluster_map_char_iteration();
      char_offsets.push(byte_idx);
    }

    let char_count = char_offsets.len();
    let glyph_count = run.glyphs.len();

    let mut char_to_glyph = vec![0; char_count];
    let mut glyph_to_char = vec![0; glyph_count];

    // Build glyph to char mapping from cluster info
    for (glyph_idx, glyph) in run.glyphs.iter().enumerate() {
      let cluster = glyph.cluster as usize;
      let char_idx = char_offsets.partition_point(|&offset| offset < cluster);
      glyph_to_char[glyph_idx] = char_idx.min(char_count.saturating_sub(1));
    }

    // Build char to glyph mapping
    for (glyph_idx, &char_idx) in glyph_to_char.iter().enumerate() {
      if char_idx < char_count {
        char_to_glyph[char_idx] = glyph_idx;
      }
    }

    Self {
      char_to_glyph,
      glyph_to_char,
    }
  }

  /// Gets the glyph index for a character index.
  pub fn glyph_for_char(&self, char_idx: usize) -> Option<usize> {
    self.char_to_glyph.get(char_idx).copied()
  }

  /// Gets the character index for a glyph index.
  pub fn char_for_glyph(&self, glyph_idx: usize) -> Option<usize> {
    self.glyph_to_char.get(glyph_idx).copied()
  }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
  use super::*;
  use crate::css::types::FontFaceRule;
  use crate::css::types::FontFaceSource;
  use crate::style::types::EastAsianVariant;
  use crate::style::types::EastAsianWidth;
  use crate::style::types::FontFeatureSetting;
  use crate::style::types::FontKerning;
  use crate::style::types::FontStretch;
  use crate::style::types::FontVariantLigatures;
  use crate::style::types::FontVariationSetting;
  use crate::style::types::FontWeight;
  use crate::style::types::NumericFigure;
  use crate::style::types::NumericFraction;
  use crate::style::types::NumericSpacing;
  use crate::style::types::TextOrientation;
  use crate::style::types::WritingMode;
  use crate::text::font_db::FontConfig;
  use crate::text::font_db::FontDatabase;
  use crate::text::font_db::FontStretch as DbFontStretch;
  use crate::text::font_db::FontStyle as DbFontStyle;
  use crate::text::font_db::FontWeight as DbFontWeight;
  use crate::text::font_db::GenericFamily;
  use crate::text::font_fallback::FamilyEntry;
  use std::fs;
  use std::sync::atomic::Ordering;
  use std::sync::Arc;
  use std::time::Duration;
  use unicode_bidi::Level;
  use url::Url;

  fn system_font_for_char(ch: char) -> Option<(Vec<u8>, String)> {
    let db = FontDatabase::new();
    let id = db
      .faces()
      .find(|face| db.has_glyph_cached(face.id, ch))
      .map(|face| face.id)?;
    let font = db.load_font(id)?;
    Some(((*font.data).clone(), font.family))
  }

  fn variable_system_font_with_axes(required: &[Tag]) -> Option<(Vec<u8>, u32, String)> {
    let db = FontDatabase::new();
    for face_info in db.faces() {
      let Some(font) = db.load_font(face_info.id) else {
        continue;
      };
      let Ok(face) = ttf_parser::Face::parse(font.data.as_ref(), font.index) else {
        continue;
      };
      let axes: Vec<Tag> = face.variation_axes().into_iter().map(|a| a.tag).collect();
      if required.iter().all(|tag| axes.contains(tag)) {
        return Some(((*font.data).clone(), font.index, font.family));
      }
    }
    None
  }

  fn load_font_context_with_data(data: Vec<u8>) -> Option<FontContext> {
    let mut db = FontDatabase::empty();
    db.load_font_data(data).ok()?;
    Some(FontContext::with_database(Arc::new(db)))
  }

  fn temp_font_url(data: &[u8]) -> Option<(tempfile::TempDir, Url)> {
    let dir = tempfile::tempdir().ok()?;
    let path = dir.path().join("fixture.ttf");
    fs::write(&path, data).ok()?;
    let url = Url::from_file_path(&path).ok()?;
    Some((dir, url))
  }

  fn variable_font_context_with_axes(
    required: &[Tag],
  ) -> Option<(FontContext, Vec<u8>, u32, String)> {
    let (data, index, family) = variable_system_font_with_axes(required)?;
    let ctx = load_font_context_with_data(data.clone())?;
    Some((ctx, data, index, family))
  }

  fn system_math_font_for_char(ch: char) -> Option<(Vec<u8>, u32, String)> {
    let db = FontDatabase::new();
    for id in db.find_math_fonts() {
      if !db.has_glyph_cached(id, ch) {
        continue;
      }
      let font = db.load_font(id)?;
      return Some(((*font.data).clone(), font.index, font.family));
    }
    None
  }

  fn system_non_math_font_for_char(ch: char) -> Option<(Vec<u8>, u32, String)> {
    let db = FontDatabase::new();
    for face in db.faces() {
      if !db.has_glyph_cached(face.id, ch) {
        continue;
      }
      let font = db.load_font(face.id)?;
      let has_math_table = ttf_parser::Face::parse(font.data.as_ref(), font.index)
        .ok()
        .and_then(|f| f.tables().math)
        .is_some();
      if has_math_table {
        continue;
      }
      return Some(((*font.data).clone(), font.index, font.family));
    }
    None
  }

  #[test]
  fn test_direction_from_level() {
    assert!(Direction::from_level(Level::ltr()).is_ltr());
    assert!(Direction::from_level(Level::rtl()).is_rtl());
  }

  #[test]
  fn test_direction_to_harfbuzz() {
    assert_eq!(
      Direction::LeftToRight.to_harfbuzz(),
      HbDirection::LeftToRight
    );
    assert_eq!(
      Direction::RightToLeft.to_harfbuzz(),
      HbDirection::RightToLeft
    );
  }

  #[test]
  fn test_script_detection_latin() {
    assert_eq!(Script::detect('A'), Script::Latin);
    assert_eq!(Script::detect('z'), Script::Latin);
    assert_eq!(Script::detect('é'), Script::Latin);
  }

  #[test]
  fn test_script_detection_arabic() {
    assert_eq!(Script::detect('م'), Script::Arabic);
    assert_eq!(Script::detect('ر'), Script::Arabic);
  }

  #[test]
  fn test_script_detection_syriac() {
    assert_eq!(Script::detect('ܐ'), Script::Syriac);
    assert_eq!(Script::detect('ܒ'), Script::Syriac);
  }

  #[test]
  fn test_script_detection_thaana() {
    assert_eq!(Script::detect('ހ'), Script::Thaana);
    assert_eq!(Script::detect('ށ'), Script::Thaana);
  }

  #[test]
  fn test_script_detection_nko() {
    assert_eq!(Script::detect('ߊ'), Script::Nko);
    assert_eq!(Script::detect('ߞ'), Script::Nko);
  }

  #[test]
  fn test_script_detection_javanese() {
    assert_eq!(Script::detect('ꦄ'), Script::Javanese);
    assert_eq!(Script::detect('ꦧ'), Script::Javanese);
  }

  #[test]
  fn test_script_detection_hebrew() {
    assert_eq!(Script::detect('ש'), Script::Hebrew);
    assert_eq!(Script::detect('ל'), Script::Hebrew);
  }

  #[test]
  fn test_script_detection_greek() {
    assert_eq!(Script::detect('α'), Script::Greek);
    assert_eq!(Script::detect('Ω'), Script::Greek);
  }

  #[test]
  fn test_script_detection_cyrillic() {
    assert_eq!(Script::detect('А'), Script::Cyrillic);
    assert_eq!(Script::detect('я'), Script::Cyrillic);
  }

  #[test]
  fn test_script_detection_cjk() {
    assert_eq!(Script::detect('中'), Script::Han);
    assert_eq!(Script::detect('あ'), Script::Hiragana);
    assert_eq!(Script::detect('カ'), Script::Katakana);
    assert_eq!(Script::detect('한'), Script::Hangul);
  }

  #[test]
  fn test_script_detection_common() {
    assert_eq!(Script::detect(' '), Script::Common);
    assert_eq!(Script::detect('1'), Script::Common);
    assert_eq!(Script::detect('.'), Script::Common);
  }

  #[test]
  fn test_script_detection_combining_marks_are_inherited() {
    // U+07EB is a N'Ko combining tone mark. It must not be classified as N'Ko,
    // otherwise script itemization could split an extended grapheme cluster.
    assert_eq!(Script::detect('\u{07EB}'), Script::Inherited);
  }

  #[test]
  fn test_bidi_analysis_ltr() {
    let style = ComputedStyle::default();
    let bidi = BidiAnalysis::analyze("Hello", &style);

    assert!(!bidi.needs_reordering());
    assert!(bidi.base_direction().is_ltr());
  }

  #[test]
  fn test_bidi_analysis_rtl() {
    let style = ComputedStyle::default();
    let bidi = BidiAnalysis::analyze("שלום", &style);

    assert!(bidi.needs_reordering());
  }

  #[test]
  fn test_bidi_analysis_mixed() {
    let style = ComputedStyle::default();
    let bidi = BidiAnalysis::analyze("Hello שלום World", &style);

    assert!(bidi.needs_reordering());
  }

  #[test]
  fn bidi_analysis_uses_char_indices() {
    let style = ComputedStyle::default();
    let text = "a\u{05d0}\u{05d1}"; // a + two Hebrew letters
    let bidi = BidiAnalysis::analyze(text, &style);

    let indices: Vec<_> = text.char_indices().collect();
    let a_level = bidi.level_at(indices[0].0);
    let hebrew_level = bidi.level_at(indices[1].0);

    assert!(a_level.is_ltr(), "latin char should be LTR");
    assert!(hebrew_level.is_rtl(), "hebrew char should be RTL");
  }

  #[test]
  fn cluster_map_construction_is_linear() {
    let char_count = 10_000;
    let text: String = "a".repeat(char_count);
    let text_len = text.len();
    let glyphs: Vec<GlyphPosition> = (0..char_count)
      .map(|i| GlyphPosition {
        glyph_id: i as u32,
        cluster: i as u32,
        x_offset: 0.0,
        y_offset: 0.0,
        x_advance: 1.0,
        y_advance: 0.0,
      })
      .collect();

    let run = ShapedRun {
      text,
      start: 0,
      end: text_len,
      glyphs,
      direction: Direction::LeftToRight,
      level: 0,
      advance: char_count as f32,
      font: Arc::new(LoadedFont {
        id: None,
        family: "Test".to_string(),
        data: Arc::new(Vec::new()),
        index: 0,
        weight: DbFontWeight::NORMAL,
        style: DbFontStyle::Normal,
        stretch: DbFontStretch::Normal,
      }),
      font_size: 16.0,
      baseline_shift: 0.0,
      language: None,
      synthetic_bold: 0.0,
      synthetic_oblique: 0.0,
      rotation: RunRotation::None,
      palette_index: 0,
      palette_overrides: Arc::new(Vec::new()),
      palette_override_hash: 0,
      variations: Vec::new(),
      scale: 1.0,
    };

    reset_cluster_map_char_iterations();

    let cluster_map = ClusterMap::from_shaped_run(&run);

    for idx in 0..char_count {
      assert_eq!(cluster_map.glyph_for_char(idx), Some(idx));
      assert_eq!(cluster_map.char_for_glyph(idx), Some(idx));
    }

    let iterations = cluster_map_char_iterations();
    assert!(
      iterations >= char_count,
      "expected to scan at least {} characters, got {}",
      char_count,
      iterations
    );
    assert!(
      iterations <= char_count + 1,
      "expected to scan characters once, got {} iterations for {} chars",
      iterations,
      char_count
    );
  }

  #[test]
  fn bidi_override_forces_base_direction() {
    let mut style = ComputedStyle::default();
    style.direction = CssDirection::Rtl;
    style.unicode_bidi = crate::style::types::UnicodeBidi::BidiOverride;
    let text = "abc";
    let bidi = BidiAnalysis::analyze(text, &style);

    let indices: Vec<_> = text.char_indices().collect();
    for (byte_idx, _) in indices {
      assert!(
        bidi.level_at(byte_idx).is_rtl(),
        "override should force RTL level"
      );
    }
    assert!(
      !bidi.needs_reordering(),
      "override should not request reordering"
    );
  }

  #[test]
  fn bidi_plaintext_uses_first_strong_for_base() {
    let mut style = ComputedStyle::default();
    style.unicode_bidi = crate::style::types::UnicodeBidi::Plaintext;

    let rtl_text = "שלום abc";
    let bidi_rtl = BidiAnalysis::analyze(rtl_text, &style);
    assert!(
      bidi_rtl.base_level().is_rtl(),
      "plaintext should pick RTL base from first strong"
    );

    let ltr_text = "abc שלום";
    let bidi_ltr = BidiAnalysis::analyze(ltr_text, &style);
    assert!(
      bidi_ltr.base_level().is_ltr(),
      "plaintext should pick LTR base from first strong"
    );
  }

  #[test]
  fn bidi_controls_do_not_contribute_advance() {
    let style = ComputedStyle::default();
    let ctx = FontContext::new();
    let pipeline = ShapingPipeline::new();
    let clean = pipeline.shape("abc", &style, &ctx).expect("shape clean");
    // Inject an isolate sequence around b.
    let isolated = pipeline
      .shape("a\u{2067}b\u{2069}c", &style, &ctx)
      .expect("shape with controls");
    let clean_adv: f32 = clean.iter().map(|r| r.advance).sum();
    let iso_adv: f32 = isolated.iter().map(|r| r.advance).sum();
    assert!(
      (clean_adv - iso_adv).abs() < 0.1,
      "bidi controls should not change advance ({} vs {})",
      clean_adv,
      iso_adv
    );
  }

  #[test]
  fn combining_marks_do_not_force_last_resort_fallback_or_render_notdef() {
    let style = ComputedStyle::default();
    let ctx = FontContext::with_config(FontConfig::bundled_only());
    assert!(
      !ctx.database().is_empty(),
      "bundled font context should load deterministic fonts for tests"
    );

    let pipeline = ShapingPipeline::new();
    let text = "中\u{1AB0}";
    let combining_offset = text
      .char_indices()
      .nth(1)
      .expect("string should contain two chars")
      .0;

    let shaped = pipeline.shape(text, &style, &ctx).expect("shape succeeds");
    assert!(
      !shaped.is_empty(),
      "shaping should produce at least one run"
    );

    let base_has_glyph = shaped
      .iter()
      .flat_map(|run| run.glyphs.iter())
      .any(|glyph| glyph.cluster == 0 && glyph.glyph_id != 0);
    assert!(
      base_has_glyph,
      "base character should render with a real glyph even when followed by an unsupported combining mark"
    );

    let has_notdef_for_mark = shaped
      .iter()
      .flat_map(|run| run.glyphs.iter())
      .any(|glyph| glyph.cluster as usize == combining_offset && glyph.glyph_id == 0);
    assert!(
      !has_notdef_for_mark,
      "unsupported combining marks should not emit .notdef glyphs"
    );
  }

  #[test]
  fn missing_base_glyph_clusters_fall_back_to_notdef() {
    let data = fs::read("tests/fixtures/fonts/NotoSans-subset.ttf").expect("read Noto Sans subset");
    let ctx = load_font_context_with_data(data).expect("load font context");
    let font = ctx
      .database()
      .first_font()
      .expect("font context should contain at least one font");
    let face = crate::text::face_cache::get_ttf_face(&font).expect("parse font");

    let pipeline = ShapingPipeline::new();
    let mut style = ComputedStyle::default();
    style.font_family = vec![font.family.clone()];
    style.font_size = 16.0;

    for text in ["뮝\u{07FD}", "犧\u{0345}"] {
      let clusters = atomic_shaping_clusters(text);
      assert_eq!(clusters, vec![(0, text.len())], "expected a single cluster");

      let base_char = text.chars().next().expect("string should have base char");
      assert!(
        !face.has_glyph(base_char),
        "fixture font unexpectedly supports base character U+{:04X}",
        base_char as u32
      );

      let shaped = pipeline.shape(text, &style, &ctx).expect("shape succeeds");
      let glyphs: Vec<_> = shaped.iter().flat_map(|run| run.glyphs.iter()).collect();
      assert!(
        !glyphs.is_empty(),
        "shaping should produce at least one glyph"
      );
      assert!(
        glyphs
          .iter()
          .any(|glyph| glyph.cluster == 0 && glyph.glyph_id == 0),
        "base cluster should emit .notdef when glyph is missing"
      );
    }
  }

  #[test]
  fn map_hb_position_applies_baseline_shift_horizontally() {
    let mut pos = rustybuzz::GlyphPosition::default();
    pos.x_advance = 10;
    pos.y_advance = 3;
    pos.x_offset = 2;
    pos.y_offset = -4;
    let (x_offset, y_offset, x_advance, y_advance) = map_hb_position(false, 1.5, 2.0, &pos);

    assert_eq!(x_offset, 4.0, "x offset should use x_offset with scale");
    assert_eq!(
      y_offset, -6.5,
      "baseline shift applies to y offset in horizontal mode"
    );
    assert_eq!(x_advance, 20.0, "x advance scales inline axis");
    assert_eq!(y_advance, 6.0, "y advance scales cross axis");
  }

  #[test]
  fn map_hb_position_applies_vertical_offsets_without_swapping() {
    let mut pos = rustybuzz::GlyphPosition::default();
    pos.x_advance = 10;
    pos.y_advance = -20;
    pos.x_offset = 7;
    pos.y_offset = -3;
    let (x_offset, y_offset, x_advance, y_advance) = map_hb_position(true, 2.0, 1.0, &pos);

    assert_eq!(
      x_offset, 9.0,
      "baseline shift should move the cross axis (x) for vertical text"
    );
    assert_eq!(y_offset, -3.0, "inline offset should come from y_offset");
    assert_eq!(x_advance, 10.0, "cross advance maps to x advance");
    assert_eq!(y_advance, 20.0, "inline advance uses absolute y_advance");
  }

  #[test]
  fn map_hb_position_falls_back_to_x_advance_in_vertical_mode() {
    let mut pos = rustybuzz::GlyphPosition::default();
    pos.x_advance = -15;
    pos.y_advance = 0;
    pos.x_offset = 1;
    pos.y_offset = 2;
    let (x_offset, y_offset, x_advance, y_advance) = map_hb_position(true, 5.0, 1.0, &pos);

    assert_eq!(
      x_offset, 6.0,
      "baseline shift should still apply to x offset in vertical mode"
    );
    assert_eq!(y_offset, 2.0, "inline offset stays on the inline axis");
    assert_eq!(x_advance, -15.0, "x advance uses the cross axis advance");
    assert_eq!(
      y_advance, 15.0,
      "inline advance falls back to x_advance and is absolute"
    );
  }

  #[test]
  fn test_bidi_analysis_empty() {
    let style = ComputedStyle::default();
    let bidi = BidiAnalysis::analyze("", &style);

    assert!(!bidi.needs_reordering());
  }

  #[test]
  fn test_itemize_single_script() {
    let style = ComputedStyle::default();
    let bidi = BidiAnalysis::analyze("Hello", &style);
    let runs = itemize_text("Hello", &bidi);

    assert_eq!(runs.len(), 1);
    assert_eq!(runs[0].text, "Hello");
    assert_eq!(runs[0].script, Script::Latin);
  }

  #[test]
  fn test_itemize_mixed_scripts() {
    let style = ComputedStyle::default();
    let text = "Hello שלום";
    let bidi = BidiAnalysis::analyze(text, &style);
    let runs = itemize_text(text, &bidi);

    // Should have at least 2 runs (Latin and Hebrew)
    assert!(runs.len() >= 2);
  }

  #[test]
  fn itemization_keeps_combining_marks_with_base_script() {
    // U+07EB is a N'Ko combining mark. When it appears after a Hangul base
    // character it should not force a script run break.
    let style = ComputedStyle::default();
    let text = "가\u{07EB}\u{07CA}"; // Hangul + N'Ko combining mark + N'Ko letter
    let bidi = BidiAnalysis::analyze(text, &style);
    let runs = itemize_text(text, &bidi);

    assert_eq!(
      runs.len(),
      2,
      "combining marks should not start a new script run"
    );
    assert_eq!(runs[0].text, "가\u{07EB}");
    assert_eq!(runs[0].script, Script::Hangul);
    assert_eq!(runs[1].text, "\u{07CA}");
    assert_eq!(runs[1].script, Script::Nko);
  }

  #[test]
  fn test_itemize_empty() {
    let style = ComputedStyle::default();
    let bidi = BidiAnalysis::analyze("", &style);
    let runs = itemize_text("", &bidi);

    assert!(runs.is_empty());
  }

  #[test]
  fn test_script_is_neutral() {
    assert!(Script::Common.is_neutral());
    assert!(Script::Inherited.is_neutral());
    assert!(Script::Unknown.is_neutral());
    assert!(!Script::Latin.is_neutral());
    assert!(!Script::Arabic.is_neutral());
  }

  #[test]
  fn test_pipeline_new() {
    let pipeline = ShapingPipeline::new();
    // Should not panic
    let _ = pipeline;
  }

  #[test]
  fn shaping_cache_hits_increment_stats_and_skip_work() {
    let style = ComputedStyle::default();
    let ctx = FontContext::new();
    let pipeline = ShapingPipeline::new();
    SHAPE_FONT_RUN_INVOCATIONS.store(0, Ordering::Relaxed);

    let first = pipeline
      .shape("Cached text", &style, &ctx)
      .expect("first shape should succeed");
    assert!(!first.is_empty());
    let first_run_calls = SHAPE_FONT_RUN_INVOCATIONS.load(Ordering::Relaxed);
    assert!(
      first_run_calls > 0,
      "initial shape should call shape_font_run"
    );

    let initial_stats = pipeline.cache_stats();
    assert_eq!(initial_stats.misses, 1);
    assert_eq!(initial_stats.hits, 0);

    let second = pipeline
      .shape("Cached text", &style, &ctx)
      .expect("cache hit should succeed");
    assert_eq!(
      SHAPE_FONT_RUN_INVOCATIONS.load(Ordering::Relaxed),
      first_run_calls,
      "cache hit should not invoke shaping again"
    );

    let stats_after_hit = pipeline.cache_stats();
    assert_eq!(stats_after_hit.hits, 1);
    assert_eq!(stats_after_hit.misses, 1);
    assert_eq!(first.len(), second.len());
  }

  #[test]
  fn shaping_cache_evicts_when_capacity_exceeded() {
    let style = ComputedStyle::default();
    let ctx = FontContext::new();
    let capacity = 4;
    let pipeline = ShapingPipeline::with_cache_capacity_for_test(capacity);
    SHAPE_FONT_RUN_INVOCATIONS.store(0, Ordering::Relaxed);

    let texts: Vec<String> = (0..(capacity + 2))
      .map(|i| format!("Eviction {}", i))
      .collect();
    for text in &texts {
      pipeline.shape(text, &style, &ctx).expect("shape succeeds");
    }

    let stats = pipeline.cache_stats();
    assert!(
      stats.evictions >= 2,
      "expected evictions once cache exceeded capacity, saw {}",
      stats.evictions
    );
    assert_eq!(
      pipeline.cache_len(),
      capacity,
      "cache should stay bounded to its configured capacity"
    );
  }

  #[test]
  fn shaping_sets_language_from_style() {
    let mut style = ComputedStyle::default();
    style.language = "tr-TR".to_string();
    let pipeline = ShapingPipeline::new();
    let font_ctx = FontContext::new();

    let runs = pipeline
      .shape("i", &style, &font_ctx)
      .expect("shape succeeds");
    assert!(!runs.is_empty());
    assert_eq!(runs[0].language.as_ref().map(|l| l.as_str()), Some("tr-tr"));
  }

  #[test]
  fn font_size_adjust_number_scales_font_run() {
    let font_ctx = FontContext::new();
    let Some(font) = font_ctx.get_sans_serif() else {
      return;
    };
    let Some(aspect) = font.metrics().ok().and_then(|m| m.aspect_ratio()) else {
      return;
    };

    let mut style = ComputedStyle::default();
    style.font_family = vec![font.family.clone()];
    style.font_size = 20.0;
    let desired = aspect * 2.0;
    style.font_size_adjust = FontSizeAdjust::Number(desired);

    let pipeline = ShapingPipeline::new();
    let runs = pipeline
      .shape("Hello", &style, &font_ctx)
      .expect("shape succeeds");
    assert!(!runs.is_empty());

    let expected = style.font_size * (desired / aspect);
    assert!((runs[0].font_size - expected).abs() < 0.01);
  }

  #[test]
  fn font_size_adjust_from_font_defaults_to_base_font() {
    let font_ctx = FontContext::new();
    let Some(font) = font_ctx.get_sans_serif() else {
      return;
    };
    let Some(aspect) = font.metrics().ok().and_then(|m| m.aspect_ratio()) else {
      return;
    };

    let mut style = ComputedStyle::default();
    style.font_family = vec![font.family.clone()];
    style.font_size = 18.0;
    style.font_size_adjust = FontSizeAdjust::FromFont;

    let pipeline = ShapingPipeline::new();
    let runs = pipeline
      .shape("Hello", &style, &font_ctx)
      .expect("shape succeeds");
    assert!(!runs.is_empty());

    // Using the same font as the reference should preserve the base size.
    assert!((runs[0].font_size - style.font_size).abs() < 0.01);
    // Sanity: aspect was available to avoid the test vacuously passing.
    assert!(aspect > 0.0);
  }

  #[test]
  fn test_itemized_run_len() {
    let run = ItemizedRun {
      start: 0,
      end: 5,
      text: "Hello".to_string(),
      script: Script::Latin,
      direction: Direction::LeftToRight,
      level: 0,
    };

    assert_eq!(run.len(), 5);
    assert!(!run.is_empty());
  }

  #[test]
  fn test_itemized_run_empty() {
    let run = ItemizedRun {
      start: 5,
      end: 5,
      text: String::new(),
      script: Script::Latin,
      direction: Direction::LeftToRight,
      level: 0,
    };

    assert_eq!(run.len(), 0);
    assert!(run.is_empty());
  }

  #[test]
  fn test_script_to_harfbuzz() {
    // Specific scripts should return Some
    assert!(Script::Latin.to_harfbuzz().is_some());
    assert!(Script::Arabic.to_harfbuzz().is_some());
    assert!(Script::Syriac.to_harfbuzz().is_some());
    assert!(Script::Thaana.to_harfbuzz().is_some());
    assert!(Script::Nko.to_harfbuzz().is_some());
    assert!(Script::Hebrew.to_harfbuzz().is_some());
    assert!(Script::Javanese.to_harfbuzz().is_some());
    // Common/neutral scripts should return None (auto-detect)
    assert!(Script::Common.to_harfbuzz().is_none());
    assert!(Script::Inherited.to_harfbuzz().is_none());
  }

  #[test]
  fn test_reorder_runs_empty() {
    let mut runs: Vec<ShapedRun> = Vec::new();
    reorder_runs(&mut runs, &[]);
    assert!(runs.is_empty());
  }

  #[test]
  fn itemization_splits_runs_at_paragraph_boundaries() {
    let mut style = ComputedStyle::default();
    style.direction = CssDirection::Ltr;
    let text = "abc\nאבג";

    let bidi = BidiAnalysis::analyze(text, &style);
    let runs = itemize_text(text, &bidi);
    let runs = split_itemized_runs_by_paragraph(runs, bidi.paragraphs());

    assert_eq!(
      runs.len(),
      2,
      "should yield one run per paragraph in this case"
    );
    let paras = bidi.paragraphs();
    assert_eq!(paras.len(), 2);
    assert_eq!(runs[0].start, paras[0].start_byte);
    assert_eq!(runs[0].end, paras[0].end_byte);
    assert_eq!(runs[1].start, paras[1].start_byte);
    assert_eq!(runs[1].end, paras[1].end_byte);
  }

  #[test]
  fn split_run_at_rejects_non_char_boundary_offsets() {
    let run = ItemizedRun {
      start: 0,
      end: "a😊b".len(),
      text: "a😊b".to_string(),
      script: Script::Latin,
      direction: Direction::LeftToRight,
      level: 0,
    };

    assert!(
      split_run_at(&run, 2).is_none(),
      "should not split inside a multibyte codepoint"
    );

    let (left, right) = split_run_at(&run, 1).expect("valid split at char boundary");
    assert_eq!(left.text, "a");
    assert_eq!(right.text, "😊b");
    assert_eq!(left.end, 1);
    assert_eq!(right.start, 1);
  }

  #[test]
  fn bidi_analysis_records_paragraph_boundaries() {
    let mut style = ComputedStyle::default();
    style.direction = CssDirection::Ltr;
    let text = "abc\nאבג";

    let analysis = BidiAnalysis::analyze(text, &style);
    let paragraphs = analysis.paragraphs();
    assert_eq!(paragraphs.len(), 2);
    assert_eq!(paragraphs[0].start_byte, 0);
    assert!(paragraphs[0].end_byte > paragraphs[0].start_byte);
    assert_eq!(paragraphs[1].start_byte, paragraphs[0].end_byte);
    assert_eq!(paragraphs[1].end_byte, text.len());
  }

  #[test]
  fn reorder_runs_respects_paragraph_boundaries() {
    fn run(start: usize, end: usize, level: u8) -> ShapedRun {
      ShapedRun {
        text: String::new(),
        start,
        end,
        glyphs: Vec::new(),
        direction: if level % 2 == 0 {
          Direction::LeftToRight
        } else {
          Direction::RightToLeft
        },
        level,
        advance: 0.0,
        font: Arc::new(LoadedFont {
          id: None,
          family: "Test".to_string(),
          data: Arc::new(Vec::new()),
          index: 0,
          weight: DbFontWeight::NORMAL,
          style: DbFontStyle::Normal,
          stretch: DbFontStretch::Normal,
        }),
        font_size: 16.0,
        baseline_shift: 0.0,
        language: None,
        synthetic_bold: 0.0,
        synthetic_oblique: 0.0,
        rotation: RunRotation::None,
        palette_index: 0,
        palette_overrides: Arc::new(Vec::new()),
        palette_override_hash: 0,
        variations: Vec::new(),
        scale: 1.0,
      }
    }

    // Two paragraphs whose runs all share the same level; reordering should not swap paragraphs.
    let mut runs = vec![run(0, 3, 1), run(3, 6, 1)];
    let paragraphs = vec![
      ParagraphBoundary {
        start_byte: 0,
        end_byte: 3,
        level: Level::ltr(),
      },
      ParagraphBoundary {
        start_byte: 3,
        end_byte: 6,
        level: Level::ltr(),
      },
    ];

    reorder_runs(&mut runs, &paragraphs);
    assert_eq!(runs[0].start, 0);
    assert_eq!(runs[1].start, 3);
  }

  #[test]
  fn shape_with_direction_uses_explicit_base() {
    let mut style = ComputedStyle::default();
    style.unicode_bidi = crate::style::types::UnicodeBidi::Normal;
    let ctx = FontContext::new();
    let text = "!?";

    let shaped = ShapingPipeline::new()
      .shape_with_direction(text, &style, &ctx, Direction::RightToLeft)
      .expect("shape_with_direction");

    assert_eq!(shaped.len(), 1);
    let run = &shaped[0];
    assert_eq!(run.direction, Direction::RightToLeft);
    assert_eq!(run.level % 2, 1, "bidi level should reflect RTL base");
  }

  #[test]
  fn small_caps_shapes_lowercase_with_scaled_size() {
    let mut style = ComputedStyle::default();
    style.font_variant = FontVariant::SmallCaps;
    style.font_size = 20.0;
    let ctx = FontContext::new();
    let shaped = ShapingPipeline::new().shape("Abc", &style, &ctx).unwrap();
    assert!(shaped.iter().any(|r| (r.font_size - 16.0).abs() < 0.1));
    assert!(shaped.iter().any(|r| (r.font_size - 20.0).abs() < 0.1));
  }

  #[test]
  fn font_synthesis_none_disables_synthetic_small_caps() {
    let mut style = ComputedStyle::default();
    style.font_variant_caps = FontVariantCaps::SmallCaps;
    style.font_size = 18.0;
    style.font_synthesis.small_caps = false;
    let ctx = FontContext::new();
    let shaped = ShapingPipeline::new().shape("Abc", &style, &ctx).unwrap();
    assert_eq!(
      shaped.len(),
      1,
      "synthetic small-caps should not split runs"
    );
    assert!(shaped.iter().all(|r| (r.font_size - 18.0).abs() < 0.1));
  }

  #[test]
  fn synthetic_super_position_applies_without_feature() {
    let pipeline = ShapingPipeline::new();
    let mut style = ComputedStyle::default();
    style.font_variant_position = FontVariantPosition::Super;
    style.font_size = 20.0;

    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    let runs = match pipeline.shape("x", &style, &ctx) {
      Ok(r) => r,
      Err(_) => return,
    };
    if runs.is_empty() {
      return;
    }
    let run = &runs[0];
    if ctx.supports_feature(&run.font, *b"sups") {
      // Genuine superscript support means no synthesis needed.
      return;
    }

    assert!(
      run.font_size < style.font_size,
      "synthetic superscript should shrink font size"
    );
    assert!(
      run.glyphs.iter().any(|g| g.y_offset > 0.0),
      "synthetic superscript should raise glyphs"
    );
  }

  #[test]
  fn font_synthesis_position_none_disables_synthetic_shift() {
    let pipeline = ShapingPipeline::new();
    let mut style = ComputedStyle::default();
    style.font_variant_position = FontVariantPosition::Super;
    style.font_synthesis.position = false;

    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    let runs = match pipeline.shape("x", &style, &ctx) {
      Ok(r) => r,
      Err(_) => return,
    };
    if runs.is_empty() {
      return;
    }
    let run = &runs[0];
    if ctx.supports_feature(&run.font, *b"sups") {
      // Real superscript glyphs remain allowed.
      return;
    }

    assert!((run.font_size - style.font_size).abs() < 0.01);
    assert!(
      run.glyphs.iter().all(|g| g.y_offset.abs() < 0.01),
      "synthesis disabled should keep glyphs on the baseline"
    );
  }

  #[test]
  fn font_variation_settings_adjust_variable_font_axes() {
    let Some((data, _index, family)) = variable_system_font_with_axes(&[Tag::from_bytes(b"wdth")])
    else {
      return;
    };
    let Some(ctx) = load_font_context_with_data(data) else {
      return;
    };
    let mut style = ComputedStyle::default();
    style.font_family = vec![family];
    style.font_size = 16.0;

    let pipeline = ShapingPipeline::new();
    let default_run = pipeline.shape("mmmm", &style, &ctx).expect("default run");

    style.font_variation_settings = vec![FontVariationSetting {
      tag: *b"wdth",
      value: 75.0,
    }];
    let narrow_run = pipeline.shape("mmmm", &style, &ctx).expect("narrow run");

    style.font_variation_settings = vec![FontVariationSetting {
      tag: *b"wdth",
      value: 130.0,
    }];
    let wide_run = pipeline.shape("mmmm", &style, &ctx).expect("wide run");

    let default_advance: f32 = default_run.iter().map(|r| r.advance).sum();
    let narrow_advance: f32 = narrow_run.iter().map(|r| r.advance).sum();
    let wide_advance: f32 = wide_run.iter().map(|r| r.advance).sum();

    assert!(
      narrow_advance < default_advance,
      "wdth axis should shrink glyph advances"
    );
    assert!(
      wide_advance > default_advance,
      "wdth axis should widen glyph advances"
    );
  }

  #[test]
  fn shaping_cache_resets_when_fonts_change() {
    let Some((fallback_data, fallback_family)) = system_font_for_char('m') else {
      return;
    };
    let mut db = FontDatabase::empty();
    db.load_font_data(fallback_data)
      .expect("load fallback font");
    let ctx = FontContext::with_database(Arc::new(db));

    let mut style = ComputedStyle::default();
    style.font_family = vec!["Webby".to_string(), fallback_family.clone()];
    style.font_size = 16.0;

    let pipeline = ShapingPipeline::new();
    let fallback_runs = match pipeline.shape("mmmm", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if fallback_runs.is_empty() {
      return;
    }
    let fallback_font = &fallback_runs[0].font;
    assert_eq!(fallback_font.family, fallback_family);

    let Some((web_data, _web_family)) = system_font_for_char('A') else {
      return;
    };
    let Some((_dir, web_url)) = temp_font_url(&web_data) else {
      return;
    };
    let face = FontFaceRule {
      family: Some("Webby".to_string()),
      sources: vec![FontFaceSource::url(web_url.to_string())],
      ..Default::default()
    };
    ctx
      .load_web_fonts(&[face], None, None)
      .expect("load web font");

    let web_runs = match pipeline.shape("mmmm", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if web_runs.is_empty() {
      return;
    }
    let web_font = &web_runs[0].font;
    assert_eq!(web_font.family, "Webby");
    assert!(
      !Arc::ptr_eq(&web_font.data, &fallback_font.data)
        || (web_runs[0].advance - fallback_runs[0].advance).abs() > 0.1,
      "web font selection should invalidate cached shaping"
    );
  }

  #[test]
  fn font_optical_sizing_none_skips_opsz_axis() {
    let Some((ctx, _data, _index, family)) =
      variable_font_context_with_axes(&[Tag::from_bytes(b"opsz")])
    else {
      return;
    };
    let mut style = ComputedStyle::default();
    style.font_family = vec![family];
    style.font_size = 20.0;
    let runs_auto = match assign_fonts(
      &[ItemizedRun {
        text: "mmmm".to_string(),
        start: 0,
        end: 4,
        script: Script::Latin,
        direction: Direction::LeftToRight,
        level: 0,
      }],
      &style,
      &ctx,
    ) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if runs_auto.is_empty() {
      return;
    }
    let opsz_tag = Tag::from_bytes(b"opsz");
    assert!(runs_auto[0].variations.iter().any(|v| v.tag == opsz_tag));

    style.font_optical_sizing = crate::style::types::FontOpticalSizing::None;
    let runs_none = match assign_fonts(
      &[ItemizedRun {
        text: "mmmm".to_string(),
        start: 0,
        end: 4,
        script: Script::Latin,
        direction: Direction::LeftToRight,
        level: 0,
      }],
      &style,
      &ctx,
    ) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if runs_none.is_empty() {
      return;
    }
    assert!(runs_none[0].variations.iter().all(|v| v.tag != opsz_tag));
  }

  #[test]
  fn wght_axis_prevents_synthetic_bold() {
    let Some((ctx, _data, _index, family)) =
      variable_font_context_with_axes(&[Tag::from_bytes(b"wght")])
    else {
      return;
    };
    let mut style = ComputedStyle::default();
    style.font_family = vec![family];
    style.font_weight = FontWeight::Number(900);

    let runs = match ShapingPipeline::new().shape("m", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if runs.is_empty() {
      return;
    }
    assert!(
      runs.iter().all(|r| r.synthetic_bold.abs() < f32::EPSILON),
      "wght axis should satisfy requested weight without synthetic bolding"
    );
  }

  #[test]
  fn font_language_override_sets_language_tag() {
    let ctx = FontContext::new();
    let mut style = ComputedStyle::default();
    style.font_family = vec!["serif".to_string()];
    style.font_language_override =
      crate::style::types::FontLanguageOverride::Override("SRB".to_string());

    let runs = ShapingPipeline::new()
      .shape("text", &style, &ctx)
      .expect("shape with override");
    assert!(runs
      .iter()
      .all(|r| r.language.as_ref().map(|l| l.as_str()) == Some("srb")));
  }

  #[test]
  fn math_generic_prefers_math_fonts() {
    let ctx = FontContext::new();
    if ctx.database().find_math_fonts().is_empty() {
      return;
    }
    if !ctx
      .database()
      .find_math_fonts()
      .iter()
      .copied()
      .any(|id| ctx.database().has_glyph(id, '∑'))
    {
      return;
    }
    let mut style = ComputedStyle::default();
    style.font_family = vec!["math".to_string()];
    style.font_size = 18.0;

    let runs = match ShapingPipeline::new().shape("∑", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if runs.is_empty() {
      return;
    }
    let has_math_table = ttf_parser::Face::parse(runs[0].font.data.as_ref(), runs[0].font.index)
      .ok()
      .and_then(|f| f.tables().math)
      .is_some();
    assert!(
      has_math_table,
      "math generic should select a font advertising a MATH table"
    );
  }

  #[test]
  fn math_generic_prefers_web_math_fonts() {
    let Some((math_data, _math_index, _math_family)) = system_math_font_for_char('∑') else {
      return;
    };
    let Some((_dir, font_url)) = temp_font_url(&math_data) else {
      return;
    };
    let face = FontFaceRule {
      family: Some("WebMath".to_string()),
      sources: vec![FontFaceSource::url(font_url.to_string())],
      ..Default::default()
    };

    let mut db = FontDatabase::empty();
    let _ = db.load_font_data(math_data);
    let ctx = FontContext::with_database(Arc::new(db));
    if ctx.load_web_fonts(&[face], None, None).is_err() {
      return;
    }

    let mut style = ComputedStyle::default();
    style.font_family = vec!["math".to_string()];
    style.font_size = 16.0;

    let runs = match ShapingPipeline::new().shape("∑", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if runs.is_empty() {
      return;
    }
    assert_eq!(runs[0].font.family, "WebMath");
  }

  #[test]
  fn math_generic_falls_back_without_math_fonts() {
    let Some((fallback_data, _fallback_index, fallback_family)) =
      system_non_math_font_for_char('∑')
    else {
      return;
    };

    let mut db = FontDatabase::empty();
    db.load_font_data(fallback_data)
      .expect("load fallback font");
    let ctx = FontContext::with_database(Arc::new(db));
    if !ctx.database().find_math_fonts().is_empty() {
      return;
    }

    let mut style = ComputedStyle::default();
    style.font_family = vec!["math".to_string(), fallback_family.clone()];
    style.font_size = 16.0;

    let runs = match ShapingPipeline::new().shape("∑", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if runs.is_empty() {
      return;
    }
    assert_eq!(runs[0].font.family, fallback_family);
  }

  #[test]
  fn unicode_range_limits_web_font_usage() {
    let Some((fallback_data, fallback_family)) = system_font_for_char('a') else {
      return;
    };
    let Some((_dir, font_url)) = temp_font_url(&fallback_data) else {
      return;
    };
    let face = FontFaceRule {
      family: Some("RangeFace".to_string()),
      sources: vec![FontFaceSource::url(font_url.to_string())],
      unicode_ranges: vec![(0x0041, 0x005a)],
      ..Default::default()
    };

    let mut db = FontDatabase::empty();
    db.load_font_data(fallback_data)
      .expect("load fallback font");
    let ctx = FontContext::with_database(Arc::new(db));
    if ctx.load_web_fonts(&[face], None, None).is_err() {
      return;
    }

    let families = vec![
      FamilyEntry::Named("RangeFace".to_string()),
      FamilyEntry::Named(fallback_family),
      FamilyEntry::Generic(GenericFamily::SansSerif),
    ];

    let mut picker = FontPreferencePicker::new(EmojiPreference::Neutral);
    let upper = resolve_font_for_char(
      'A',
      Script::Latin,
      "",
      &families,
      400,
      DbFontStyle::Normal,
      None,
      DbFontStretch::Normal,
      &ctx,
      &mut picker,
    )
    .expect("font for uppercase");
    assert_eq!(upper.family, "RangeFace");

    let mut picker = FontPreferencePicker::new(EmojiPreference::Neutral);
    let lower = resolve_font_for_char(
      'a',
      Script::Latin,
      "",
      &families,
      400,
      DbFontStyle::Normal,
      None,
      DbFontStretch::Normal,
      &ctx,
      &mut picker,
    )
    .expect("fallback font");
    assert_ne!(lower.family, "RangeFace");
  }

  #[test]
  fn web_font_only_context_shapes_missing_glyphs_with_notdef() {
    let font_data = fs::read("tests/fixtures/fonts/NotoSans-subset.ttf")
      .expect("read NotoSans subset fixture");
    let parsed = ttf_parser::Face::parse(&font_data, 0).expect("parse NotoSans subset fixture");
    assert!(parsed.glyph_index('A').is_some());
    assert!(parsed.glyph_index('a').is_some());

    let missing = 'א';
    assert!(
      parsed.glyph_index(missing).is_none(),
      "fixture should not cover the missing codepoint"
    );

    let Some((_dir, font_url)) = temp_font_url(&font_data) else {
      panic!("expected temp file URL for font fixture");
    };

    let ctx = FontContext::with_database(Arc::new(FontDatabase::empty()));
    assert_eq!(ctx.database().font_count(), 0);

    let face = FontFaceRule {
      family: Some("WebOnlySans".to_string()),
      sources: vec![FontFaceSource::url(font_url.to_string())],
      ..Default::default()
    };
    ctx
      .load_web_fonts(&[face], None, None)
      .expect("load web fonts");
    assert!(
      ctx.wait_for_pending_web_fonts(Duration::from_secs(1)),
      "web font load should settle"
    );
    assert!(
      !ctx.is_effectively_empty(),
      "web fonts should make the font context non-empty"
    );

    let pipeline = ShapingPipeline::new();
    let mut style = ComputedStyle::default();
    style.font_family = vec!["WebOnlySans".to_string()];
    style.font_size = 16.0;

    let ok_runs = pipeline
      .shape("Aa", &style, &ctx)
      .expect("shape basic Latin with web font");
    assert!(!ok_runs.is_empty());
    assert!(ok_runs.iter().all(|run| run.font.family == "WebOnlySans"));
    assert!(
      ok_runs
        .iter()
        .all(|run| run.glyphs.iter().all(|glyph| glyph.glyph_id != 0)),
      "covered ASCII should not produce `.notdef`"
    );

    let missing_runs = pipeline
      .shape(&missing.to_string(), &style, &ctx)
      .expect("shape missing glyph with web font last-resort");
    assert!(!missing_runs.is_empty());
    assert!(missing_runs.iter().all(|run| run.font.family == "WebOnlySans"));
    assert!(
      missing_runs
        .iter()
        .any(|run| run.glyphs.iter().any(|glyph| glyph.glyph_id == 0)),
      "missing codepoint should be shaped into `.notdef` glyphs, not a hard error"
    );
  }

  #[test]
  fn script_aware_fallback_prefers_bundled_script_faces() {
    let ctx = FontContext::with_config(FontConfig::bundled_only());
    let pipeline = ShapingPipeline::new();

    let mut base_style = ComputedStyle::default();
    base_style.font_family = vec!["sans-serif".to_string()];
    base_style.font_size = 16.0;

    let mut latin_style = base_style.clone();
    latin_style.language = "en".to_string();
    let latin_runs = pipeline
      .shape("Hello", &latin_style, &ctx)
      .expect("shape latin");
    assert!(!latin_runs.is_empty());
    assert_eq!(latin_runs[0].font.family, "Noto Sans");

    let mut devanagari_style = base_style.clone();
    devanagari_style.language = "hi".to_string();
    let devanagari_runs = pipeline
      .shape("ह", &devanagari_style, &ctx)
      .expect("shape devanagari");
    assert_eq!(devanagari_runs[0].font.family, "Noto Sans Devanagari");

    let mut bengali_style = base_style.clone();
    bengali_style.language = "bn".to_string();
    let bengali_runs = pipeline
      .shape("আ", &bengali_style, &ctx)
      .expect("shape bengali");
    assert_eq!(bengali_runs[0].font.family, "Noto Sans Bengali");

    let mut arabic_style = base_style.clone();
    arabic_style.language = "ar".to_string();
    let arabic_runs = pipeline
      .shape("م", &arabic_style, &ctx)
      .expect("shape arabic");
    assert_eq!(arabic_runs[0].font.family, "Noto Sans Arabic");

    // CJK Han characters exist in multiple bundled faces. We prefer a language-specific
    // face, mirroring browser system fallback.
    let mut ja_style = base_style.clone();
    ja_style.language = "ja".to_string();
    let ja_runs = pipeline
      .shape("漢、字", &ja_style, &ctx)
      .expect("shape japanese han");
    assert_eq!(
      ja_runs.len(),
      1,
      "expected CJK punctuation to share the JP face"
    );
    assert_eq!(ja_runs[0].font.family, "Noto Sans JP");

    let mut ko_style = base_style.clone();
    ko_style.language = "ko".to_string();
    let ko_runs = pipeline
      .shape("漢、字", &ko_style, &ctx)
      .expect("shape korean han");
    assert_eq!(
      ko_runs.len(),
      1,
      "expected CJK punctuation to share the KR face"
    );
    assert_eq!(ko_runs[0].font.family, "Noto Sans KR");

    let mut zh_style = base_style.clone();
    zh_style.language = "zh".to_string();
    let zh_runs = pipeline
      .shape("漢、字", &zh_style, &ctx)
      .expect("shape chinese han");
    assert_eq!(zh_runs.len(), 1);
    assert_eq!(zh_runs[0].font.family, "Noto Sans SC");

    // Hebrew/Thai mapping is exercised only when those faces are present (they may be
    // added by a separate bundled-font task).
    for (lang, sample, expected) in [
      ("he", "א", "Noto Sans Hebrew"),
      ("th", "ก", "Noto Sans Thai"),
    ] {
      let has_face = ctx
        .database()
        .faces()
        .any(|face| face.families.iter().any(|(family, _)| family == expected));
      if !has_face {
        continue;
      }
      let mut style = base_style.clone();
      style.language = lang.to_string();
      let runs = pipeline
        .shape(sample, &style, &ctx)
        .expect("shape optional");
      assert_eq!(runs[0].font.family, expected);
    }
  }

  #[test]
  fn fallback_cache_hits_for_reused_clusters() {
    let ctx = FontContext::new();
    let mut style = ComputedStyle::default();
    style.font_family = vec!["sans-serif".to_string()];
    style.font_size = 16.0;

    let pipeline = ShapingPipeline::with_cache_capacity_for_test(1);

    let before = pipeline.fallback_cache_stats();
    let first = match pipeline.shape("cache me please", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if first.is_empty() {
      return;
    }
    let mid = pipeline.fallback_cache_stats();

    let second = match pipeline.shape("please cache me again", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if second.is_empty() {
      return;
    }
    let after = pipeline.fallback_cache_stats();

    let miss_delta = (mid.glyph_misses + mid.cluster_misses)
      .saturating_sub(before.glyph_misses + before.cluster_misses);
    let hit_delta =
      (after.glyph_hits + after.cluster_hits).saturating_sub(mid.glyph_hits + mid.cluster_hits);
    assert!(
      miss_delta > 0,
      "first shaping should populate fallback cache"
    );
    assert!(
      hit_delta > 0,
      "subsequent shaping should reuse fallback cache (hits {hit_delta}, misses {miss_delta})"
    );
  }

  #[cfg(debug_assertions)]
  #[test]
  fn face_parse_counts_stop_scaling_with_length() {
    let ctx = FontContext::new();
    let _guard = crate::text::face_cache::FaceParseCountGuard::start();

    let mut style = ComputedStyle::default();
    style.font_family = vec!["sans-serif".to_string()];
    let pipeline = ShapingPipeline::new();

    let short = "fast render text ".repeat(4);
    let long = "fast render text ".repeat(200);

    if pipeline.shape(&short, &style, &ctx).is_err() {
      return;
    }
    let short_count = crate::text::face_cache::face_parse_count();

    if pipeline.shape(&long, &style, &ctx).is_err() {
      return;
    }
    let long_count = crate::text::face_cache::face_parse_count();

    assert!(
      long_count.saturating_sub(short_count) < 10,
      "face parse count should not scale with text length ({} -> {})",
      short_count,
      long_count
    );
  }

  #[cfg(debug_assertions)]
  #[test]
  fn rustybuzz_faces_are_cached_across_runs() {
    let font_data = match fs::read("tests/fonts/ColorTestCOLR.ttf") {
      Ok(data) => data,
      Err(_) => return,
    };

    let mut db = FontDatabase::empty();
    if db.load_font_data(font_data).is_err() {
      return;
    }
    let ctx = FontContext::with_database(Arc::new(db));

    let mut style = ComputedStyle::default();
    style.font_family = vec!["ColorTestCOLR".to_string()];
    style.font_size = 16.0;

    let pipeline = ShapingPipeline::new();
    let _guard = crate::text::face_cache::RustybuzzFaceParseCountGuard::start();

    let first = match pipeline.shape("A", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if first.is_empty() {
      return;
    }
    assert!(
      first.iter().all(|run| run.font.family == "ColorTestCOLR"),
      "shaping should use the loaded ColorTestCOLR font"
    );
    let first_count = crate::text::face_cache::rustybuzz_face_parse_count();
    assert_eq!(
      first_count, 1,
      "expected exactly one rustybuzz face parse for first shape"
    );

    let second = match pipeline.shape("B", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if second.is_empty() {
      return;
    }
    assert!(
      second.iter().all(|run| run.font.family == "ColorTestCOLR"),
      "subsequent shaping should keep using the cached color font"
    );

    let second_count = crate::text::face_cache::rustybuzz_face_parse_count();
    assert_eq!(
      second_count, first_count,
      "rustybuzz face should be cached between shaping runs"
    );
  }

  #[test]
  fn font_stretch_maps_to_wdth_axis_when_present() {
    let Some((ctx, _data, _index, family)) =
      variable_font_context_with_axes(&[Tag::from_bytes(b"wdth")])
    else {
      return;
    };
    let mut style = ComputedStyle::default();
    style.font_family = vec![family.clone()];
    style.font_size = 16.0;

    let pipeline = ShapingPipeline::new();

    let base = match pipeline.shape("mmmm", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if base.is_empty() {
      return;
    }
    let base_adv: f32 = base.iter().map(|r| r.advance).sum();

    style.font_stretch = FontStretch::from_percentage(75.0);
    let narrow = match pipeline.shape("mmmm", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if narrow.is_empty() {
      return;
    }
    let narrow_adv: f32 = narrow.iter().map(|r| r.advance).sum();

    style.font_stretch = FontStretch::from_percentage(125.0);
    let wide = match pipeline.shape("mmmm", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if wide.is_empty() {
      return;
    }
    let wide_adv: f32 = wide.iter().map(|r| r.advance).sum();

    assert!(
      narrow_adv < base_adv,
      "narrow stretch should reduce advance with wdth axis"
    );
    assert!(
      wide_adv > base_adv,
      "wide stretch should increase advance with wdth axis"
    );
    assert!(
      (wide_adv - narrow_adv) > 1.0,
      "wdth axis should produce a noticeable difference"
    );
  }

  #[test]
  fn auto_variations_clamp_to_axis_bounds() {
    const ROBOTO_FLEX: &[u8] = include_bytes!("../../tests/fonts/RobotoFlex-VF.ttf");

    let mut db = FontDatabase::empty();
    db.load_font_data(ROBOTO_FLEX.to_vec())
      .expect("load Roboto Flex fixture");
    let ctx = FontContext::with_database(Arc::new(db));

    let face_info = ctx
      .database()
      .faces()
      .next()
      .expect("Roboto Flex face is available");
    let family = face_info
      .families
      .first()
      .map(|(name, _)| name.clone())
      .expect("Roboto Flex has a family name");
    let face =
      ttf_parser::Face::parse(ROBOTO_FLEX, face_info.index).expect("parse Roboto Flex face");
    let axes: Vec<_> = face.variation_axes().into_iter().collect();
    let wght_tag = Tag::from_bytes(b"wght");
    let wdth_tag = Tag::from_bytes(b"wdth");
    let opsz_tag = Tag::from_bytes(b"opsz");
    let wght_axis = axes
      .iter()
      .find(|a| a.tag == wght_tag)
      .expect("Roboto Flex exposes wght axis");
    let wdth_axis = axes
      .iter()
      .find(|a| a.tag == wdth_tag)
      .expect("Roboto Flex exposes wdth axis");
    let opsz_axis = axes
      .iter()
      .find(|a| a.tag == opsz_tag)
      .expect("Roboto Flex exposes opsz axis");

    let mut style = ComputedStyle::default();
    style.font_family = vec![family.clone()];
    style.font_weight = FontWeight::Number(1);
    style.font_stretch = FontStretch::from_percentage(200.0);
    style.font_size = opsz_axis.max_value + 1000.0;
    style.font_optical_sizing = crate::style::types::FontOpticalSizing::Auto;

    let runs = ShapingPipeline::new()
      .shape("RobotoFlex", &style, &ctx)
      .expect("shape with Roboto Flex fixture");
    assert!(
      !runs.is_empty(),
      "fixture font should shape test string without fallback"
    );
    let run = &runs[0];
    assert_eq!(run.font.family, family);
    let variations = &run.variations;

    let wght_value = variations
      .iter()
      .find(|v| v.tag == wght_tag)
      .map(|v| v.value)
      .expect("auto variations should include wght");
    let wdth_value = variations
      .iter()
      .find(|v| v.tag == wdth_tag)
      .map(|v| v.value)
      .expect("auto variations should include wdth");
    let opsz_value = variations
      .iter()
      .find(|v| v.tag == opsz_tag)
      .map(|v| v.value)
      .expect("auto variations should include opsz when optical sizing is auto");

    let expected_wght =
      (style.font_weight.to_u16() as f32).clamp(wght_axis.min_value, wght_axis.max_value);
    let expected_wdth = style
      .font_stretch
      .to_percentage()
      .clamp(wdth_axis.min_value, wdth_axis.max_value);
    let expected_opsz = style
      .font_size
      .clamp(opsz_axis.min_value, opsz_axis.max_value);

    assert!(
      (wght_value - expected_wght).abs() < 0.001,
      "wght axis should clamp to font bounds (expected {expected_wght}, got {wght_value})"
    );
    assert!(
      (wdth_value - expected_wdth).abs() < 0.001,
      "wdth axis should clamp to font bounds (expected {expected_wdth}, got {wdth_value})"
    );
    assert!(
      (opsz_value - expected_opsz).abs() < 0.001,
      "opsz axis should clamp to font bounds (expected {expected_opsz}, got {opsz_value})"
    );
  }

  #[test]
  fn authored_font_variations_override_auto_axes() {
    let Some((ctx, _data, _index, family)) =
      variable_font_context_with_axes(&[Tag::from_bytes(b"wdth")])
    else {
      return;
    };
    let mut style = ComputedStyle::default();
    style.font_family = vec![family.clone()];
    style.font_size = 16.0;

    let pipeline = ShapingPipeline::new();

    style.font_stretch = FontStretch::from_percentage(75.0);
    style.font_variation_settings = vec![FontVariationSetting {
      tag: *b"wdth",
      value: 100.0,
    }];
    let forced = match pipeline.shape("mmmm", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if forced.is_empty() {
      return;
    }
    let forced_adv: f32 = forced.iter().map(|r| r.advance).sum();

    style.font_variation_settings.clear();
    style.font_stretch = FontStretch::Normal;
    let baseline = match pipeline.shape("mmmm", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if baseline.is_empty() {
      return;
    }
    let baseline_adv: f32 = baseline.iter().map(|r| r.advance).sum();

    assert!(
      (forced_adv - baseline_adv).abs() < 0.25,
      "authored wdth should override font-stretch mapping"
    );
  }

  #[test]
  fn oblique_angle_maps_to_slnt_axis_with_clamp() {
    let Some((ctx, data, index, family)) =
      variable_font_context_with_axes(&[Tag::from_bytes(b"slnt")])
    else {
      return;
    };
    let mut style = ComputedStyle::default();
    style.font_family = vec![family.clone()];
    style.font_size = 16.0;
    style.font_style = CssFontStyle::Oblique(Some(20.0));

    let runs = match assign_fonts(
      &[ItemizedRun {
        text: "mmmm".to_string(),
        start: 0,
        end: 4,
        script: Script::Latin,
        direction: Direction::LeftToRight,
        level: 0,
      }],
      &style,
      &ctx,
    ) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if runs.is_empty() {
      return;
    }
    let slnt_tag = Tag::from_bytes(b"slnt");
    let slnt_value = runs[0]
      .variations
      .iter()
      .find(|v| v.tag == slnt_tag)
      .map(|v| v.value)
      .expect("slnt axis should be populated for oblique styles");

    let face = match ttf_parser::Face::parse(&data, index) {
      Ok(face) => face,
      Err(_) => return,
    };
    let axis = face
      .variation_axes()
      .into_iter()
      .find(|a| a.tag == slnt_tag)
      .expect("variable font exposes slnt axis");
    let expected = (-20.0_f32).clamp(axis.min_value, axis.max_value);
    assert!((slnt_value - expected).abs() < 0.001);
  }

  #[test]
  fn authored_slnt_variation_preserves_authored_value() {
    let Some((ctx, _data, _index, family)) =
      variable_font_context_with_axes(&[Tag::from_bytes(b"slnt")])
    else {
      return;
    };
    let mut style = ComputedStyle::default();
    style.font_family = vec![family];
    style.font_size = 16.0;
    style.font_style = CssFontStyle::Oblique(Some(10.0));
    style.font_variation_settings = vec![FontVariationSetting {
      tag: *b"slnt",
      value: -5.0,
    }];

    let runs = assign_fonts(
      &[ItemizedRun {
        text: "mmmm".to_string(),
        start: 0,
        end: 4,
        script: Script::Latin,
        direction: Direction::LeftToRight,
        level: 0,
      }],
      &style,
      &ctx,
    )
    .expect("assign fonts");
    let slnt_tag = Tag::from_bytes(b"slnt");
    let slnt_value = runs[0]
      .variations
      .iter()
      .find(|v| v.tag == slnt_tag)
      .map(|v| v.value)
      .expect("authored slnt should survive auto mapping");

    assert!((slnt_value + 5.0).abs() < 0.001);
  }

  #[test]
  fn oblique_prefers_slnt_axis_over_ital() {
    let Some((ctx, data, index, family)) =
      variable_font_context_with_axes(&[Tag::from_bytes(b"slnt")])
    else {
      return;
    };
    let mut style = ComputedStyle::default();
    style.font_family = vec![family.clone()];
    style.font_size = 16.0;
    style.font_style = CssFontStyle::Oblique(Some(12.0));

    let runs = assign_fonts(
      &[ItemizedRun {
        text: "mmmm".to_string(),
        start: 0,
        end: 4,
        script: Script::Latin,
        direction: Direction::LeftToRight,
        level: 0,
      }],
      &style,
      &ctx,
    )
    .expect("assign fonts");

    let slnt_tag = Tag::from_bytes(b"slnt");
    let ital_tag = Tag::from_bytes(b"ital");
    let tags: Vec<_> = runs[0].variations.iter().map(|v| v.tag).collect();
    assert!(tags.contains(&slnt_tag), "slnt axis should be populated");
    let face = match ttf_parser::Face::parse(&data, index) {
      Ok(face) => face,
      Err(_) => return,
    };
    let has_ital_axis = face.variation_axes().into_iter().any(|a| a.tag == ital_tag);
    if has_ital_axis {
      assert!(
        !tags.contains(&ital_tag),
        "ital axis should not be set when slnt axis is available for oblique"
      );
    }
    let axis = face
      .variation_axes()
      .into_iter()
      .find(|a| a.tag == slnt_tag)
      .expect("variable font exposes slnt axis");
    let slnt_value = runs[0]
      .variations
      .iter()
      .find(|v| v.tag == slnt_tag)
      .map(|v| v.value)
      .unwrap();
    let expected = (-12.0_f32).clamp(axis.min_value, axis.max_value);
    assert!((slnt_value - expected).abs() < 0.001);
  }

  #[test]
  fn italic_maps_to_available_axis() {
    let ital_tag = Tag::from_bytes(b"ital");
    let slnt_tag = Tag::from_bytes(b"slnt");
    let Some((ctx, data, index, family)) = variable_font_context_with_axes(&[ital_tag])
      .or_else(|| variable_font_context_with_axes(&[slnt_tag]))
    else {
      return;
    };
    let mut style = ComputedStyle::default();
    style.font_family = vec![family.clone()];
    style.font_size = 16.0;
    style.font_style = CssFontStyle::Italic;

    let runs = assign_fonts(
      &[ItemizedRun {
        text: "mmmm".to_string(),
        start: 0,
        end: 4,
        script: Script::Latin,
        direction: Direction::LeftToRight,
        level: 0,
      }],
      &style,
      &ctx,
    )
    .expect("assign fonts");
    if runs.is_empty() {
      return;
    }
    let face = match ttf_parser::Face::parse(&data, index) {
      Ok(face) => face,
      Err(_) => return,
    };
    let has_ital_axis = face.variation_axes().into_iter().any(|a| a.tag == ital_tag);

    if has_ital_axis {
      let ital_value = runs[0]
        .variations
        .iter()
        .find(|v| v.tag == ital_tag)
        .map(|v| v.value);
      assert_eq!(ital_value, Some(1.0));
      assert!(
        runs[0].variations.iter().all(|v| v.tag != slnt_tag),
        "italic should not use slnt when ital axis is present"
      );
    } else {
      let slnt_value = runs[0]
        .variations
        .iter()
        .find(|v| v.tag == slnt_tag)
        .map(|v| v.value)
        .expect("italic should map to slnt when ital axis is absent");
      let axis = face
        .variation_axes()
        .into_iter()
        .find(|a| a.tag == slnt_tag)
        .expect("variable font exposes slnt axis");
      let expected = (-DEFAULT_OBLIQUE_ANGLE_DEG).clamp(axis.min_value, axis.max_value);
      assert!((slnt_value - expected).abs() < 0.001);
    }
  }

  #[test]
  fn slnt_axis_disables_synthetic_slant() {
    let Some((ctx, _data, _index, family)) =
      variable_font_context_with_axes(&[Tag::from_bytes(b"slnt")])
    else {
      return;
    };
    let mut style = ComputedStyle::default();
    style.font_family = vec![family.clone()];
    style.font_size = 16.0;
    style.font_style = CssFontStyle::Oblique(Some(10.0));

    let font = ctx
      .get_font_full(
        &style.font_family,
        style.font_weight.to_u16(),
        DbFontStyle::Normal,
        DbFontStretch::from_percentage(style.font_stretch.to_percentage()),
      )
      .expect("load font with slnt axis");
    let (_, synthetic_slant) = compute_synthetic_styles(&style, &font);
    assert_eq!(
      synthetic_slant, 0.0,
      "slnt axis should satisfy slant without synthesis"
    );
  }

  #[test]
  fn slope_preferences_follow_css_slope_order() {
    use crate::text::font_db::FontStyle as DbStyle;
    assert_eq!(slope_preference_order(DbStyle::Normal), &[DbStyle::Normal]);
    assert_eq!(
      slope_preference_order(DbStyle::Italic),
      &[DbStyle::Italic, DbStyle::Oblique, DbStyle::Normal]
    );
    assert_eq!(
      slope_preference_order(DbStyle::Oblique),
      &[DbStyle::Oblique, DbStyle::Italic, DbStyle::Normal]
    );
  }

  #[test]
  fn weight_preferences_prefer_closest_with_bias() {
    let order_350 = weight_preference_order(350);
    assert!(order_350.iter().position(|w| *w == 300) < order_350.iter().position(|w| *w == 400));
    let order_600 = weight_preference_order(600);
    assert!(order_600.iter().position(|w| *w == 700) < order_600.iter().position(|w| *w == 500));
    let order_450 = weight_preference_order(450);
    assert!(
      order_450.iter().position(|w| *w == 500) < order_450.iter().position(|w| *w == 400),
      "for weights between 400-500, heavier weights up to 500 should be preferred first"
    );
  }

  #[test]
  fn stretch_preferences_follow_css_ordering() {
    use crate::text::font_db::FontStretch as DbStretch;
    let order_narrow = stretch_preference_order(DbStretch::SemiCondensed);
    assert!(
      order_narrow.iter().position(|s| *s == DbStretch::Condensed)
        < order_narrow.iter().position(|s| *s == DbStretch::Normal),
      "when desired stretch is below 100%, narrower widths are tried before wider ones"
    );

    let order_wide = stretch_preference_order(DbStretch::Expanded);
    assert!(
      order_wide
        .iter()
        .position(|s| *s == DbStretch::ExtraExpanded)
        < order_wide.iter().position(|s| *s == DbStretch::Normal),
      "when desired stretch is above 100%, wider widths are tried before narrower ones"
    );
  }

  #[test]
  fn vertical_sideways_text_marks_runs_rotated() {
    let mut style = ComputedStyle::default();
    style.writing_mode = crate::style::types::WritingMode::VerticalRl;
    style.text_orientation = crate::style::types::TextOrientation::Sideways;
    let ctx = FontContext::new();
    let shaped = ShapingPipeline::new().shape("Abc", &style, &ctx).unwrap();
    assert!(shaped.iter().all(|r| r.rotation == RunRotation::Cw90));
  }

  #[test]
  fn vertical_sideways_left_rotates_runs_counter_clockwise() {
    let mut style = ComputedStyle::default();
    style.writing_mode = crate::style::types::WritingMode::VerticalRl;
    style.text_orientation = crate::style::types::TextOrientation::SidewaysLeft;
    let ctx = FontContext::new();
    let shaped = ShapingPipeline::new().shape("Abc", &style, &ctx).unwrap();
    assert!(
      shaped.iter().all(|r| r.rotation == RunRotation::Ccw90),
      "sideways-left should rotate runs counter-clockwise"
    );
  }

  #[test]
  fn sideways_writing_rotates_all_runs_regardless_of_text_orientation() {
    let mut style = ComputedStyle::default();
    style.writing_mode = crate::style::types::WritingMode::SidewaysRl;
    style.text_orientation = crate::style::types::TextOrientation::Upright;
    let ctx = FontContext::new();
    let shaped = ShapingPipeline::new().shape("Abc本", &style, &ctx).unwrap();
    assert!(
            shaped.iter().all(|r| r.rotation == RunRotation::Cw90),
            "sideways writing should set horizontal typographic mode and rotate text regardless of text-orientation"
        );
  }

  #[test]
  fn sideways_writing_uses_horizontal_metrics() {
    let mut style = ComputedStyle::default();
    style.writing_mode = crate::style::types::WritingMode::SidewaysLr;
    style.text_orientation = crate::style::types::TextOrientation::Mixed;
    let ctx = FontContext::new();
    let shaped = ShapingPipeline::new().shape("Abc", &style, &ctx).unwrap();
    assert!(
      shaped.iter().all(|r| r.rotation == RunRotation::Cw90),
      "sideways writing should rotate text using horizontal metrics regardless of text-orientation"
    );
  }

  #[test]
  fn upright_in_vertical_forces_ltr_bidi_and_preserves_order() {
    let mut style = ComputedStyle::default();
    style.direction = crate::style::types::Direction::Rtl;
    style.writing_mode = crate::style::types::WritingMode::VerticalRl;
    style.text_orientation = crate::style::types::TextOrientation::Upright;

    let text = "אבג abc";
    let ctx = FontContext::new();
    let shaped = ShapingPipeline::new().shape(text, &style, &ctx).unwrap();
    assert!(
      shaped.iter().all(|r| r.direction == Direction::LeftToRight),
      "upright should force LTR direction for all runs in vertical text"
    );
    let reconstructed: String = shaped.iter().flat_map(|r| r.text.chars()).collect();
    assert_eq!(
      reconstructed, text,
      "upright should avoid bidi reordering and preserve textual order"
    );
  }

  #[test]
  fn vertical_mixed_rotates_non_upright_segments() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::VerticalRl;
    style.text_orientation = TextOrientation::Mixed;
    let ctx = FontContext::new();
    let shaped = ShapingPipeline::new()
      .shape("A。B本", &style, &ctx)
      .unwrap();
    assert!(shaped.iter().any(|r| r.rotation == RunRotation::Cw90));
    assert!(shaped.iter().any(|r| r.rotation == RunRotation::None));
  }

  #[test]
  fn vertical_upright_forces_all_runs_upright() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::VerticalLr;
    style.text_orientation = TextOrientation::Upright;
    let ctx = FontContext::new();
    let shaped = ShapingPipeline::new().shape("Abc本", &style, &ctx).unwrap();
    assert!(shaped.iter().all(|r| r.rotation == RunRotation::None));
  }

  #[test]
  fn collect_features_respects_ligature_variants_and_overrides() {
    let mut style = ComputedStyle::default();
    style.font_variant_ligatures = FontVariantLigatures {
      common: false,
      discretionary: true,
      historical: false,
      contextual: false,
    };
    style.font_feature_settings = vec![FontFeatureSetting {
      tag: *b"liga",
      value: 1,
    }];

    let feats = collect_opentype_features(&style);
    let mut seen: std::collections::HashMap<[u8; 4], u32> = std::collections::HashMap::new();
    for f in feats {
      seen.insert(f.tag.to_bytes(), f.value);
    }
    assert_eq!(seen.get(b"liga"), Some(&1));
    assert_eq!(seen.get(b"clig"), Some(&0));
    assert_eq!(seen.get(b"dlig"), Some(&1));
    assert_eq!(seen.get(b"calt"), Some(&0));
  }

  #[test]
  fn collect_features_includes_numeric_variants_and_kerning() {
    let mut style = ComputedStyle::default();
    style.font_variant_numeric.figure = NumericFigure::Oldstyle;
    style.font_variant_numeric.spacing = NumericSpacing::Tabular;
    style.font_variant_numeric.fraction = NumericFraction::Stacked;
    style.font_variant_numeric.ordinal = true;
    style.font_variant_numeric.slashed_zero = true;
    style.font_kerning = FontKerning::None;
    style.font_variant_east_asian.variant = Some(EastAsianVariant::Jis04);
    style.font_variant_east_asian.width = Some(EastAsianWidth::FullWidth);
    style.font_variant_east_asian.ruby = true;
    style.font_variant_position = FontVariantPosition::Super;

    let feats = collect_opentype_features(&style);
    let mut seen: std::collections::HashMap<[u8; 4], u32> = std::collections::HashMap::new();
    for f in feats {
      seen.insert(f.tag.to_bytes(), f.value);
    }
    assert_eq!(seen.get(b"onum"), Some(&1));
    assert_eq!(seen.get(b"tnum"), Some(&1));
    assert_eq!(seen.get(b"afrc"), Some(&1));
    assert_eq!(seen.get(b"ordn"), Some(&1));
    assert_eq!(seen.get(b"zero"), Some(&1));
    assert_eq!(seen.get(b"kern"), Some(&0));
    assert_eq!(seen.get(b"jp04"), Some(&1));
    assert_eq!(seen.get(b"fwid"), Some(&1));
    assert_eq!(seen.get(b"ruby"), Some(&1));
    assert_eq!(seen.get(b"sups"), Some(&1));
  }

  #[test]
  fn collect_features_includes_caps_variants() {
    let mut style = ComputedStyle::default();
    style.font_variant_caps = FontVariantCaps::AllSmallCaps;

    let feats = collect_opentype_features(&style);
    let mut seen: std::collections::HashMap<[u8; 4], u32> = std::collections::HashMap::new();
    for f in feats {
      seen.insert(f.tag.to_bytes(), f.value);
    }
    assert_eq!(seen.get(b"smcp"), Some(&1));
    assert_eq!(seen.get(b"c2sc"), Some(&1));

    style.font_variant_caps = FontVariantCaps::AllPetiteCaps;
    let feats = collect_opentype_features(&style);
    let mut seen: std::collections::HashMap<[u8; 4], u32> = std::collections::HashMap::new();
    for f in feats {
      seen.insert(f.tag.to_bytes(), f.value);
    }
    assert_eq!(seen.get(b"pcap"), Some(&1));
    assert_eq!(seen.get(b"c2pc"), Some(&1));

    style.font_variant_caps = FontVariantCaps::Unicase;
    let feats = collect_opentype_features(&style);
    assert!(feats.iter().any(|f| f.tag.to_bytes() == *b"unic"));
  }

  #[test]
  fn collect_features_includes_alternates() {
    let mut style = ComputedStyle::default();
    style.font_variant_alternates.historical_forms = true;
    style.font_variant_alternates.stylistic = Some(3);
    style.font_variant_alternates.stylesets = vec![1, 2];
    style.font_variant_alternates.character_variants = vec![4];
    style.font_variant_alternates.swash = Some(1);
    style.font_variant_alternates.ornaments = Some(2);

    let feats = collect_opentype_features(&style);
    let mut seen: std::collections::HashMap<[u8; 4], u32> = std::collections::HashMap::new();
    for f in feats {
      seen.insert(f.tag.to_bytes(), f.value);
    }
    assert_eq!(seen.get(b"hist"), Some(&1));
    assert_eq!(seen.get(b"ss03"), Some(&1));
    assert_eq!(seen.get(b"ss01"), Some(&1));
    assert_eq!(seen.get(b"ss02"), Some(&1));
    assert_eq!(seen.get(b"cv04"), Some(&1));
    assert_eq!(seen.get(b"swsh"), Some(&1));
    assert_eq!(seen.get(b"ornm"), Some(&2));
  }

  #[test]
  fn emoji_dominant_detection_respects_non_emoji_content() {
    assert!(is_emoji_dominant("😀 😀"));
    assert!(is_emoji_dominant("😀\u{200d}\u{1f9d1}"));
    assert!(!is_emoji_dominant("😀a"));
  }

  #[test]
  fn variation_selector_stays_with_current_font_run() {
    let ctx = FontContext::new();
    let style = ComputedStyle::default();
    let runs = assign_fonts(
      &[ItemizedRun {
        text: "A\u{fe0f}".to_string(),
        start: 0,
        end: "A\u{fe0f}".len(),
        script: Script::Latin,
        direction: Direction::LeftToRight,
        level: 0,
      }],
      &style,
      &ctx,
    )
    .expect("assign fonts");
    assert_eq!(
      runs.len(),
      1,
      "variation selector should not split font runs"
    );
    assert_eq!(runs[0].text, "A\u{fe0f}");
  }

  #[test]
  fn emoji_preference_matches_variant_and_character() {
    assert_eq!(
      emoji_preference_for_char('😀', FontVariantEmoji::Emoji),
      EmojiPreference::PreferEmoji
    );
    assert_eq!(
      emoji_preference_for_char('😀', FontVariantEmoji::Text),
      EmojiPreference::AvoidEmoji
    );
    assert_eq!(
      emoji_preference_for_char('😀', FontVariantEmoji::Unicode),
      EmojiPreference::PreferEmoji
    );
    // '#' defaults to text presentation.
    assert_eq!(
      emoji_preference_for_char('#', FontVariantEmoji::Unicode),
      EmojiPreference::AvoidEmoji
    );
  }

  #[test]
  fn emoji_variation_selectors_override_property_preference() {
    assert_eq!(
      emoji_preference_with_selector('😀', Some('\u{fe0e}'), FontVariantEmoji::Emoji),
      EmojiPreference::AvoidEmoji
    );
    assert_eq!(
      emoji_preference_with_selector('😀', Some('\u{fe0f}'), FontVariantEmoji::Text),
      EmojiPreference::PreferEmoji
    );
  }

  #[test]
  fn zwj_sequences_prefer_emoji_fonts() {
    assert_eq!(
      emoji_preference_with_selector('👩', Some('\u{200d}'), FontVariantEmoji::Text),
      EmojiPreference::PreferEmoji
    );
  }

  fn dummy_font(name: &str) -> LoadedFont {
    LoadedFont {
      id: None,
      data: Arc::new(Vec::new()),
      index: 0,
      family: name.into(),
      weight: DbFontWeight::NORMAL,
      style: DbFontStyle::Normal,
      stretch: DbFontStretch::Normal,
    }
  }

  #[test]
  fn emoji_preference_picker_prefers_emoji_when_available() {
    let text_font = dummy_font("Example Text");
    let emoji_font = dummy_font("Noto Color Emoji");
    let mut picker = FontPreferencePicker::new(EmojiPreference::PreferEmoji);
    let idx = picker.bump_order();
    assert!(picker.consider(text_font.clone(), false, idx).is_none());
    let idx = picker.bump_order();
    let chosen = picker
      .consider(emoji_font.clone(), true, idx)
      .expect("should pick emoji font");
    assert_eq!(chosen.family, emoji_font.family);
  }

  #[test]
  fn emoji_preference_picker_falls_back_to_text_when_no_emoji_font() {
    let text_font = dummy_font("Example Text");
    let mut picker = FontPreferencePicker::new(EmojiPreference::PreferEmoji);
    let idx = picker.bump_order();
    assert!(picker.consider(text_font.clone(), false, idx).is_none());
    let chosen = picker.finish().expect("fallback text font");
    assert_eq!(chosen.family, text_font.family);
  }

  #[test]
  fn emoji_preference_picker_avoids_emoji_but_allows_fallback() {
    let emoji_font = dummy_font("Twemoji");
    let mut picker = FontPreferencePicker::new(EmojiPreference::AvoidEmoji);
    let idx = picker.bump_order();
    assert!(picker.consider(emoji_font.clone(), true, idx).is_none());
    let chosen = picker.finish().expect("fallback emoji font");
    assert_eq!(chosen.family, emoji_font.family);
  }

  #[test]
  fn font_variant_emoji_text_prefers_text_fonts() {
    let text_font = dummy_font("Example Text");
    let emoji_font = dummy_font("Noto Color Emoji");
    let pref = emoji_preference_for_char('😀', FontVariantEmoji::Text);
    assert_eq!(pref, EmojiPreference::AvoidEmoji);
    let mut picker = FontPreferencePicker::new(pref);
    let idx = picker.bump_order();
    assert!(picker.consider(emoji_font.clone(), true, idx).is_none());
    let idx = picker.bump_order();
    let chosen = picker
      .consider(text_font.clone(), false, idx)
      .expect("should pick text font when avoiding emoji");
    assert_eq!(chosen.family, text_font.family);
  }

  #[test]
  fn font_variant_emoji_emoji_prefers_emoji_fonts() {
    let text_font = dummy_font("Example Text");
    let emoji_font = dummy_font("Twemoji");
    let pref = emoji_preference_for_char('😀', FontVariantEmoji::Emoji);
    assert_eq!(pref, EmojiPreference::PreferEmoji);
    let mut picker = FontPreferencePicker::new(pref);
    let idx = picker.bump_order();
    assert!(picker.consider(text_font.clone(), false, idx).is_none());
    let idx = picker.bump_order();
    let chosen = picker
      .consider(emoji_font.clone(), true, idx)
      .expect("should pick emoji font for emoji preference");
    assert_eq!(chosen.family, emoji_font.family);
  }

  #[test]
  fn font_variant_emoji_unicode_prefers_text_for_non_emoji() {
    let text_font = dummy_font("Example Text");
    let emoji_font = dummy_font("EmojiOne");
    let pref = emoji_preference_for_char('#', FontVariantEmoji::Unicode);
    assert_eq!(pref, EmojiPreference::AvoidEmoji);
    let mut picker = FontPreferencePicker::new(pref);
    let idx = picker.bump_order();
    assert!(picker.consider(emoji_font.clone(), true, idx).is_none());
    let idx = picker.bump_order();
    let chosen = picker
      .consider(text_font.clone(), false, idx)
      .expect("unicode text should pick text font for non-emoji chars");
    assert_eq!(chosen.family, text_font.family);
  }

  #[test]
  fn emoji_variation_selector_fe0e_forces_text_font() {
    let text_font = dummy_font("Example Text");
    let emoji_font = dummy_font("Noto Color Emoji");
    let pref = emoji_preference_with_selector('😀', Some('\u{fe0e}'), FontVariantEmoji::Emoji);
    assert_eq!(pref, EmojiPreference::AvoidEmoji);
    let mut picker = FontPreferencePicker::new(pref);
    let idx = picker.bump_order();
    assert!(picker.consider(emoji_font.clone(), true, idx).is_none());
    let idx = picker.bump_order();
    let chosen = picker
      .consider(text_font.clone(), false, idx)
      .expect("FE0E should force text presentation");
    assert_eq!(chosen.family, text_font.family);
  }

  #[test]
  fn emoji_variation_selector_fe0f_prefers_emoji_font() {
    let text_font = dummy_font("Example Text");
    let emoji_font = dummy_font("Twemoji");
    let pref = emoji_preference_with_selector('😀', Some('\u{fe0f}'), FontVariantEmoji::Text);
    assert_eq!(pref, EmojiPreference::PreferEmoji);
    let mut picker = FontPreferencePicker::new(pref);
    let idx = picker.bump_order();
    assert!(picker.consider(text_font.clone(), false, idx).is_none());
    let idx = picker.bump_order();
    let chosen = picker
      .consider(emoji_font.clone(), true, idx)
      .expect("FE0F should prefer emoji font even when property requests text");
    assert_eq!(chosen.family, emoji_font.family);
  }
}
fn number_tag(prefix: &[u8; 2], n: u8) -> Option<[u8; 4]> {
  if n == 0 {
    return None;
  }
  let mut tag = [b' ', b' ', b' ', b' '];
  tag[0] = prefix[0];
  tag[1] = prefix[1];
  let tens = (n / 10) % 10;
  let ones = n % 10;
  tag[2] = b'0' + tens;
  tag[3] = b'0' + ones;
  Some(tag)
}
