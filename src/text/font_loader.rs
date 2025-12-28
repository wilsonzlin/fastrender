//! Font loading utilities
//!
//! This module provides high-level APIs for font access during layout and rendering.
//! It combines the font database with caching for efficient font access.
//!
//! # Overview
//!
//! The font loader provides:
//! - `FontContext`: High-level interface for font access during layout
//! - Convenient methods for resolving CSS font properties to actual fonts
//! - Integration with the style system
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::font_loader::FontContext;
//!
//! let ctx = FontContext::new();
//!
//! // Get a font by family list (CSS font-family style)
//! let families = vec!["Arial".to_string(), "sans-serif".to_string()];
//! if let Some(font) = ctx.get_font(&families, 400, false, false) {
//!     println!("Using font: {}", font.family);
//! }
//! ```

use crate::api::{ResourceContext, ResourceKind};
use crate::css::types::FontDisplay;
use crate::css::types::FontFaceRule;
use crate::css::types::FontFaceSource;
use crate::css::types::FontFaceStyle;
use crate::css::types::FontSourceFormat;
use crate::debug::runtime;
use crate::error::Error;
use crate::error::Result;
use crate::resource::ResourceFetcher;
use crate::text::font_db::FontCacheConfig;
use crate::text::font_db::FontConfig;
use crate::text::font_db::FontDatabase;
use crate::text::font_db::FontStretch;
use crate::text::font_db::FontStyle;
use crate::text::font_db::FontWeight;
use crate::text::font_db::LoadedFont;
use crate::text::font_db::ScaledMetrics;
use crate::text::pipeline::DEFAULT_OBLIQUE_ANGLE_DEG;
use base64::engine::general_purpose::STANDARD as BASE64_STANDARD;
use base64::Engine;
use fontdb::Database as FontDbDatabase;
use percent_encoding::percent_decode_str;
use rustybuzz::ttf_parser::{self, GlyphId, Tag};
use rustybuzz::Direction;
use rustybuzz::Face;
use rustybuzz::Feature;
use rustybuzz::UnicodeBuffer;
use std::collections::HashSet;
use std::fs;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::mpsc;
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::RwLock;
use std::time::Duration;
use std::time::Instant;
use ureq::Agent;
use url::Url;
use wuff::decompress_woff1;
use wuff::decompress_woff2;

/// Default block period used for `font-display: block/auto` before falling back.
///
/// The spec suggests ~3s; we use a shorter window to keep headless rendering fast
/// while still modeling the phase changes.
const BLOCK_PERIOD: Duration = Duration::from_millis(300);
/// Block period for fallback/optional; after this the fallback font is used.
const FALLBACK_BLOCK_PERIOD: Duration = Duration::from_millis(100);
/// Swap period for `font-display: fallback` after the short block window.
const FALLBACK_SWAP_PERIOD: Duration = Duration::from_millis(400);
/// Optional fonts must finish within this period to be adopted.
const OPTIONAL_BLOCK_PERIOD: Duration = Duration::from_millis(100);

/// Abstraction over font byte fetching so tests can simulate slow/failed loads.
pub trait FontFetcher: Send + Sync {
  fn fetch(&self, url: &str) -> Result<(Vec<u8>, Option<String>)>;
}

struct DefaultFontFetcher;

impl FontFetcher for DefaultFontFetcher {
  fn fetch(&self, url: &str) -> Result<(Vec<u8>, Option<String>)> {
    fetch_font_bytes(url)
  }
}

struct ResourceFontFetcher {
  fetcher: Arc<dyn ResourceFetcher>,
}

impl FontFetcher for ResourceFontFetcher {
  fn fetch(&self, url: &str) -> Result<(Vec<u8>, Option<String>)> {
    self
      .fetcher
      .fetch(url)
      .map(|res| (res.bytes, res.content_type))
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LoadOutcome {
  Loaded,
  Skipped,
}

fn display_deadlines(display: FontDisplay) -> (Duration, Duration) {
  match display {
    FontDisplay::Auto | FontDisplay::Block => (BLOCK_PERIOD, Duration::MAX),
    FontDisplay::Swap => (Duration::from_millis(0), Duration::MAX),
    FontDisplay::Fallback => (
      FALLBACK_BLOCK_PERIOD,
      FALLBACK_BLOCK_PERIOD + FALLBACK_SWAP_PERIOD,
    ),
    FontDisplay::Optional => (OPTIONAL_BLOCK_PERIOD, OPTIONAL_BLOCK_PERIOD),
  }
}

fn display_allows_use(display: FontDisplay, elapsed: Duration) -> bool {
  let (_, swap_deadline) = display_deadlines(display);
  elapsed <= swap_deadline
}

struct PendingTask {
  shared: Arc<(Mutex<usize>, Condvar)>,
}

impl PendingTask {
  fn new(shared: Arc<(Mutex<usize>, Condvar)>) -> Self {
    if let Ok(mut guard) = shared.0.lock() {
      *guard += 1;
    }
    Self { shared }
  }
}

impl Drop for PendingTask {
  fn drop(&mut self) {
    let (lock, cvar) = &*self.shared;
    if let Ok(mut guard) = lock.lock() {
      if *guard > 0 {
        *guard -= 1;
      }
      cvar.notify_all();
    }
  }
}

/// Font context for text operations
///
/// This is the main interface for font access during layout and rendering.
/// It holds a reference to the font database and provides convenient methods
/// for resolving fonts from CSS properties.
///
/// # Thread Safety
///
/// FontContext is thread-safe and can be shared across threads. The underlying
/// font database uses interior mutability for caching.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::font_loader::FontContext;
///
/// // Create a new font context (loads system fonts)
/// let ctx = FontContext::new();
///
/// // Get font count
/// println!("Available fonts: {}", ctx.font_count());
///
/// // Find a font
/// let families = vec!["Roboto".to_string(), "Arial".to_string(), "sans-serif".to_string()];
/// if let Some(font) = ctx.get_font(&families, 400, false, false) {
///     println!("Found: {} (weight {})", font.family, font.weight.value());
/// }
/// ```
#[derive(Clone)]
pub struct FontContext {
  db: Arc<FontDatabase>,
  web_fonts: Arc<RwLock<Vec<WebFontFace>>>,
  web_families: Arc<RwLock<std::collections::HashSet<String>>>,
  feature_support: Arc<RwLock<std::collections::HashMap<(usize, u32, u32), bool>>>,
  fetcher: Arc<dyn FontFetcher>,
  pending_async: Arc<(Mutex<usize>, Condvar)>,
  web_used_codepoints: Arc<RwLock<Vec<u32>>>,
  math_tables:
    Arc<RwLock<std::collections::HashMap<(usize, u32), Option<Arc<MathTableCacheEntry>>>>>,
  generation: Arc<AtomicU64>,
  resource_context: Option<ResourceContext>,
}

#[derive(Clone)]
struct MathTableCacheEntry {
  table: ttf_parser::math::Table<'static>,
}

#[derive(Debug, Clone, Default)]
pub struct MathConstants {
  pub script_percent_scale_down: Option<f32>,
  pub script_script_percent_scale_down: Option<f32>,
  pub delimited_sub_formula_min_height: Option<f32>,
  pub display_operator_min_height: Option<f32>,
  pub math_leading: Option<f32>,
  pub axis_height: Option<f32>,
  pub accent_base_height: Option<f32>,
  pub flattened_accent_base_height: Option<f32>,
  pub subscript_shift_down: Option<f32>,
  pub subscript_top_max: Option<f32>,
  pub subscript_baseline_drop_min: Option<f32>,
  pub superscript_shift_up: Option<f32>,
  pub superscript_shift_up_cramped: Option<f32>,
  pub superscript_bottom_min: Option<f32>,
  pub superscript_baseline_drop_max: Option<f32>,
  pub sub_superscript_gap_min: Option<f32>,
  pub superscript_bottom_max_with_subscript: Option<f32>,
  pub space_after_script: Option<f32>,
  pub stretch_stack_gap_above_min: Option<f32>,
  pub stretch_stack_gap_below_min: Option<f32>,
  pub fraction_numerator_shift_up: Option<f32>,
  pub fraction_numerator_display_style_shift_up: Option<f32>,
  pub fraction_denominator_shift_down: Option<f32>,
  pub fraction_denominator_display_style_shift_down: Option<f32>,
  pub fraction_numerator_gap_min: Option<f32>,
  pub fraction_num_display_style_gap_min: Option<f32>,
  pub fraction_rule_thickness: Option<f32>,
  pub fraction_denominator_gap_min: Option<f32>,
  pub fraction_denom_display_style_gap_min: Option<f32>,
  pub overbar_vertical_gap: Option<f32>,
  pub overbar_rule_thickness: Option<f32>,
  pub overbar_extra_ascender: Option<f32>,
  pub underbar_vertical_gap: Option<f32>,
  pub underbar_rule_thickness: Option<f32>,
  pub underbar_extra_descender: Option<f32>,
  pub radical_vertical_gap: Option<f32>,
  pub radical_display_style_vertical_gap: Option<f32>,
  pub radical_rule_thickness: Option<f32>,
  pub radical_extra_ascender: Option<f32>,
  pub radical_kern_before_degree: Option<f32>,
  pub radical_kern_after_degree: Option<f32>,
  pub radical_degree_bottom_raise_percent: Option<f32>,
  pub min_connector_overlap: Option<f32>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathKernSide {
  Left,
  Right,
}

impl FontContext {
  fn build_default_database() -> FontDatabase {
    let mut db = FontDatabase::shared_system();

    if db.font_count() == 0 {
      const FALLBACK_FONT_FILES: &[&str] = &[
        // Linux
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
        "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
        "/usr/share/fonts/truetype/freefont/FreeSans.ttf",
        // macOS
        "/System/Library/Fonts/SFNS.ttf",
        "/System/Library/Fonts/SFNSDisplay.ttf",
        "/System/Library/Fonts/Helvetica.ttc",
        "/Library/Fonts/Arial.ttf",
        // Windows
        "C:\\Windows\\Fonts\\arial.ttf",
        "C:\\Windows\\Fonts\\segoeui.ttf",
      ];

      // Some minimal container environments ship without discoverable system fonts.
      // Attempt to load a common sans-serif from well-known paths so text shaping can proceed.
      for path in FALLBACK_FONT_FILES {
        if let Ok(data) = fs::read(path) {
          let _ = db.load_font_data(data);
          if db.font_count() > 0 {
            break;
          }
        }
      }
    }

    db
  }

  fn build_database(config: &FontConfig) -> Arc<FontDatabase> {
    Arc::new(FontDatabase::with_config(config))
  }

  /// Creates a new font context with system fonts loaded
  ///
  /// This will scan system font directories and load all available fonts.
  /// The operation may take a moment on systems with many fonts.
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let ctx = FontContext::new();
  /// println!("Loaded {} fonts", ctx.font_count());
  /// ```
  pub fn new() -> Self {
    Self::with_config(FontConfig::default())
  }

  /// Creates a new font context using a custom font configuration.
  pub fn with_config(font_config: FontConfig) -> Self {
    Self::with_database_and_fetcher(
      Self::build_database(&font_config),
      Arc::new(DefaultFontFetcher),
    )
  }

  /// Creates a new font context using a custom font configuration and fetcher.
  pub fn with_config_and_fetcher(font_config: FontConfig, fetcher: Arc<dyn FontFetcher>) -> Self {
    Self::with_database_and_fetcher(Self::build_database(&font_config), fetcher)
  }

  /// Creates a new font context backed by a [`ResourceFetcher`] for remote fonts.
  pub fn with_resource_fetcher(fetcher: Arc<dyn ResourceFetcher>) -> Self {
    Self::with_database_and_fetcher(
      Self::build_database(&FontConfig::default()),
      Arc::new(ResourceFontFetcher { fetcher }),
    )
  }

  /// Creates a new font context backed by a [`ResourceFetcher`] and custom fonts.
  pub fn with_resource_fetcher_and_config(
    font_config: FontConfig,
    fetcher: Arc<dyn ResourceFetcher>,
  ) -> Self {
    Self::with_database_and_fetcher(
      Self::build_database(&font_config),
      Arc::new(ResourceFontFetcher { fetcher }),
    )
  }

  /// Creates a font context with a custom font database
  ///
  /// Useful for testing or when you need to share a database between contexts.
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let db = Arc::new(FontDatabase::empty());
  /// let ctx = FontContext::with_database(db);
  /// ```
  pub fn with_database(db: Arc<FontDatabase>) -> Self {
    Self::with_database_and_fetcher(db, Arc::new(DefaultFontFetcher))
  }

  /// Creates a font context that shares the provided font metadata while keeping caches isolated.
  pub fn with_shared_database_and_fetcher(
    db: Arc<FontDbDatabase>,
    fetcher: Arc<dyn FontFetcher>,
    cache: FontCacheConfig,
  ) -> Self {
    Self::with_database_and_fetcher(
      Arc::new(FontDatabase::with_shared_db_and_cache(db, cache)),
      fetcher,
    )
  }

  /// Creates a font context that shares the provided font metadata and uses a [`ResourceFetcher`] for remote fonts.
  pub fn with_shared_resource_fetcher(
    db: Arc<FontDbDatabase>,
    fetcher: Arc<dyn ResourceFetcher>,
    cache: FontCacheConfig,
  ) -> Self {
    Self::with_database_and_fetcher(
      Arc::new(FontDatabase::with_shared_db_and_cache(db, cache)),
      Arc::new(ResourceFontFetcher { fetcher }),
    )
  }

  /// Creates a font context backed by a custom database and fetcher.
  pub fn with_database_and_fetcher(db: Arc<FontDatabase>, fetcher: Arc<dyn FontFetcher>) -> Self {
    Self {
      db,
      web_fonts: Arc::new(RwLock::new(Vec::new())),
      web_families: Arc::new(RwLock::new(std::collections::HashSet::new())),
      feature_support: Arc::new(RwLock::new(std::collections::HashMap::new())),
      fetcher,
      pending_async: Arc::new((Mutex::new(0), Condvar::new())),
      web_used_codepoints: Arc::new(RwLock::new(Vec::new())),
      math_tables: Arc::new(RwLock::new(std::collections::HashMap::new())),
      generation: Arc::new(AtomicU64::new(0)),
      resource_context: None,
    }
  }

  /// Creates a font context with the system database and a custom fetcher.
  pub fn with_fetcher(fetcher: Arc<dyn FontFetcher>) -> Self {
    let db = FontDatabase::with_config(&FontConfig::default());
    Self::with_database_and_fetcher(Arc::new(db), fetcher)
  }

  /// Creates an empty font context (no fonts loaded)
  ///
  /// Useful for testing.
  pub fn empty() -> Self {
    Self::with_database_and_fetcher(
      Arc::new(FontDatabase::empty()),
      Arc::new(DefaultFontFetcher),
    )
  }

  /// Gets a reference to the underlying font database
  #[inline]
  pub fn database(&self) -> &FontDatabase {
    &self.db
  }

  /// Returns the number of available fonts
  #[inline]
  pub fn font_count(&self) -> usize {
    self.db.font_count()
  }

  /// Returns whether any fonts are available
  #[inline]
  pub fn has_fonts(&self) -> bool {
    !self.db.is_empty()
  }

  /// Set the active resource context used for remote font fetching.
  pub fn set_resource_context(&mut self, context: Option<ResourceContext>) {
    self.resource_context = context;
  }

  /// Get the current resource context.
  pub fn resource_context(&self) -> Option<ResourceContext> {
    self.resource_context.clone()
  }

  fn record_font_error(&self, url: &str, error: &Error) {
    if let Some(ctx) = &self.resource_context {
      if let Some(diag) = &ctx.diagnostics {
        diag.record_error(ResourceKind::Font, url, error);
      }
    }
  }

  /// Returns whether there are no usable fonts (system + web).
  ///
  /// This is primarily used to make layout/paining fail gracefully in minimal
  /// environments where no system fonts are installed and no web fonts were
  /// loaded.
  #[inline]
  pub(crate) fn is_effectively_empty(&self) -> bool {
    if !self.db.is_empty() {
      return false;
    }
    self.web_fonts.read().map(|w| w.is_empty()).unwrap_or(true)
  }

  /// Gets a font matching the given CSS properties
  ///
  /// This is the main method for resolving fonts from CSS.
  ///
  /// # Arguments
  ///
  /// * `families` - List of font family names in priority order
  /// * `weight` - Numeric weight (100-900, 400=normal, 700=bold)
  /// * `italic` - Whether italic style is requested
  /// * `oblique` - Whether oblique style is requested (used if italic is false)
  ///
  /// # Returns
  ///
  /// The loaded font data, or None if no matching font is found.
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let ctx = FontContext::new();
  ///
  /// // Get normal weight Arial or fallback
  /// let font = ctx.get_font(
  ///     &["Arial".to_string(), "sans-serif".to_string()],
  ///     400,    // normal weight
  ///     false,  // not italic
  ///     false,  // not oblique
  /// );
  ///
  /// // Get bold italic
  /// let font = ctx.get_font(
  ///     &["Georgia".to_string(), "serif".to_string()],
  ///     700,    // bold
  ///     true,   // italic
  ///     false,
  /// );
  /// ```
  pub fn get_font(
    &self,
    families: &[String],
    weight: u16,
    italic: bool,
    oblique: bool,
  ) -> Option<LoadedFont> {
    let requested_style = if italic {
      FontStyle::Italic
    } else if oblique {
      FontStyle::Oblique
    } else {
      FontStyle::Normal
    };
    let requested_angle = if oblique {
      Some(DEFAULT_OBLIQUE_ANGLE_DEG)
    } else {
      None
    };

    let stretches = crate::text::pipeline::stretch_preference_order(FontStretch::Normal);
    let slopes = crate::text::pipeline::slope_preference_order(requested_style);
    let weights = crate::text::pipeline::weight_preference_order(weight);
    for family in families {
      if let Some(font) = self.select_web_font(
        family,
        weight,
        requested_style,
        FontStretch::Normal,
        requested_angle,
        None,
      ) {
        return Some(font);
      }
      if self.is_web_family_declared(family) {
        continue;
      }

      for stretch_choice in &stretches {
        for slope in slopes {
          for weight_choice in &weights {
            let font_weight = FontWeight::new(*weight_choice);
            if let Some(id) = self.db.resolve_family_list_full(
              std::slice::from_ref(family),
              font_weight,
              *slope,
              *stretch_choice,
            ) {
              if let Some(font) = self.db.load_font(id) {
                return Some(font);
              }
            }
          }
        }
      }
    }
    None
  }

  /// Gets a font with full CSS properties
  ///
  /// Extended version that also handles font-stretch.
  ///
  /// # Arguments
  ///
  /// * `families` - List of font family names
  /// * `weight` - Numeric weight (100-900)
  /// * `style` - Font style
  /// * `stretch` - Font stretch/width
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// use fastrender::text::font_db::{FontStyle, FontStretch};
  ///
  /// let font = ctx.get_font_full(
  ///     &["Arial Narrow".to_string(), "Arial".to_string()],
  ///     400,
  ///     FontStyle::Normal,
  ///     FontStretch::Condensed,
  /// );
  /// ```
  pub fn get_font_full(
    &self,
    families: &[String],
    weight: u16,
    style: FontStyle,
    stretch: FontStretch,
  ) -> Option<LoadedFont> {
    let stretches = crate::text::pipeline::stretch_preference_order(stretch);
    let weights = crate::text::pipeline::weight_preference_order(weight);
    let slopes = crate::text::pipeline::slope_preference_order(style);
    let requested_angle = if matches!(style, FontStyle::Oblique) {
      Some(DEFAULT_OBLIQUE_ANGLE_DEG)
    } else {
      None
    };
    for family in families {
      if let Some(font) =
        self.select_web_font(family, weight, style, stretch, requested_angle, None)
      {
        return Some(font);
      }
      if self.is_web_family_declared(family) {
        continue;
      }

      for stretch_choice in &stretches {
        for slope in slopes {
          for weight_choice in &weights {
            if let Some(id) = self.db.resolve_family_list_full(
              std::slice::from_ref(family),
              FontWeight::new(*weight_choice),
              *slope,
              *stretch_choice,
            ) {
              if let Some(font) = self.db.load_font(id) {
                return Some(font);
              }
            }
          }
        }
      }
    }
    None
  }

  /// Gets a font by simple family name query
  ///
  /// Convenience method for simple queries without fallback chain.
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let font = ctx.get_font_simple("Arial", 400, FontStyle::Normal);
  /// ```
  pub fn get_font_simple(&self, family: &str, weight: u16, style: FontStyle) -> Option<LoadedFont> {
    let angle = if matches!(style, FontStyle::Oblique) {
      Some(DEFAULT_OBLIQUE_ANGLE_DEG)
    } else {
      None
    };
    if let Some(font) =
      self.select_web_font(family, weight, style, FontStretch::Normal, angle, None)
    {
      return Some(font);
    }
    if self.is_web_family_declared(family) {
      return None;
    }
    let font_weight = FontWeight::new(weight);
    let id = self.db.query(family, font_weight, style)?;
    self.db.load_font(id)
  }

  /// Gets a sans-serif fallback font
  ///
  /// Returns a generic sans-serif font. Useful as a last resort fallback.
  pub fn get_sans_serif(&self) -> Option<LoadedFont> {
    self
      .get_font_simple("sans-serif", 400, FontStyle::Normal)
      .or_else(|| self.db.first_font())
  }

  /// Gets a serif fallback font
  pub fn get_serif(&self) -> Option<LoadedFont> {
    self
      .get_font_simple("serif", 400, FontStyle::Normal)
      .or_else(|| self.db.first_font())
  }

  /// Gets a monospace fallback font
  pub fn get_monospace(&self) -> Option<LoadedFont> {
    self
      .get_font_simple("monospace", 400, FontStyle::Normal)
      .or_else(|| self.db.first_font())
  }

  /// Clears the font data cache
  ///
  /// Frees memory used by cached font data. Fonts will be reloaded
  /// from disk on next access.
  pub fn clear_cache(&self) {
    self.db.clear_cache();
  }

  /// Clears any previously loaded @font-face entries.
  pub fn clear_web_fonts(&self) {
    if let Ok(mut faces) = self.web_fonts.write() {
      faces.clear();
    }
    if let Ok(mut families) = self.web_families.write() {
      families.clear();
    }
    if let Ok(mut support) = self.feature_support.write() {
      support.clear();
    }
    if let Ok(mut used) = self.web_used_codepoints.write() {
      used.clear();
    }
    self.bump_generation();
  }

  /// Loads web fonts defined by @font-face rules into the context.
  ///
  /// Relative URLs are resolved against `base_url` when provided.
  pub fn load_web_fonts(
    &self,
    faces: &[FontFaceRule],
    base_url: Option<&str>,
    used_codepoints: Option<&[u32]>,
  ) -> Result<()> {
    let max_fonts = runtime::runtime_toggles().usize_with_default("FASTR_MAX_WEB_FONTS", 8);
    if max_fonts == 0 {
      return Ok(());
    }

    if let Ok(mut used) = self.web_used_codepoints.write() {
      used.clear();
      if let Some(cps) = used_codepoints {
        used.extend_from_slice(cps);
      }
    }

    let filter_by_codepoints = used_codepoints.is_some();
    let used_codepoints = used_codepoints.unwrap_or(&[]);
    let mut started_count = 0usize;
    let (block_tx, block_rx) = mpsc::channel::<()>();
    let mut block_expected = 0usize;
    for (order, face) in faces.iter().enumerate() {
      if started_count >= max_fonts {
        break;
      }
      let family = match &face.family {
        Some(f) => f.clone(),
        None => continue,
      };
      self.declare_web_family(&family);

      if filter_by_codepoints
        && (used_codepoints.is_empty()
          || (!face.unicode_ranges.is_empty()
            && !Self::unicode_range_intersects_used(&face.unicode_ranges, used_codepoints)))
      {
        continue;
      }

      if face.sources.is_empty() {
        continue;
      }

      let display = face.display;
      let face_clone = face.clone();
      let family_clone = family.clone();
      let base = base_url.map(|b| b.to_string());
      let guard = PendingTask::new(self.pending_async.clone());
      let should_block = matches!(display, FontDisplay::Block | FontDisplay::Auto);
      if should_block {
        block_expected += 1;
      }
      let done_tx = should_block.then(|| block_tx.clone());
      started_count += 1;
      let ctx = self.clone();
      std::thread::spawn(move || {
        let _pending = guard;
        let start = Instant::now();
        let _ = ctx.load_face_sources(&family_clone, &face_clone, base.as_deref(), order, start);
        if let Some(tx) = done_tx {
          let _ = tx.send(());
        }
      });
    }
    drop(block_tx);
    if block_expected > 0 {
      let deadline = Instant::now() + BLOCK_PERIOD;
      let mut completed = 0usize;
      while completed < block_expected {
        let now = Instant::now();
        if now >= deadline {
          break;
        }
        let remaining = deadline - now;
        match block_rx.recv_timeout(remaining) {
          Ok(()) => completed += 1,
          Err(mpsc::RecvTimeoutError::Timeout) => break,
          Err(mpsc::RecvTimeoutError::Disconnected) => break,
        }
      }
    }
    Ok(())
  }

  /// Waits for any pending asynchronous web font loads to complete.
  ///
  /// Returns true if all pending loads finished within the timeout.
  pub fn wait_for_pending_web_fonts(&self, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    let (lock, cvar) = &*self.pending_async;
    let mut guard = lock.lock().unwrap();
    loop {
      if *guard == 0 {
        return true;
      }
      let now = Instant::now();
      if now >= deadline {
        return false;
      }
      let remaining = deadline - now;
      let (g, result) = cvar.wait_timeout(guard, remaining).unwrap();
      guard = g;
      if result.timed_out() && *guard > 0 {
        return false;
      }
    }
  }

  #[inline]
  fn range_matches_used(range: (u32, u32), used: &[u32]) -> bool {
    let idx = used.partition_point(|cp| *cp < range.0);
    used.get(idx).is_some_and(|cp| *cp <= range.1)
  }

  #[inline]
  fn unicode_range_intersects_used(ranges: &[(u32, u32)], used: &[u32]) -> bool {
    ranges
      .iter()
      .any(|range| Self::range_matches_used(*range, used))
  }

  fn load_face_sources(
    &self,
    family: &str,
    face: &FontFaceRule,
    base_url: Option<&str>,
    order: usize,
    start: Instant,
  ) -> bool {
    for source in ordered_sources(&face.sources) {
      let loaded = match source {
        FontFaceSource::Local(name) => self.load_local_face(family, name, face, order, start),
        FontFaceSource::Url(src) => {
          self.load_remote_face(family, &src.url, face, base_url, order, start)
        }
      };

      match loaded {
        Ok(LoadOutcome::Loaded) => return true,
        Ok(LoadOutcome::Skipped) => continue,
        Err(_) => continue,
      }
    }
    false
  }

  fn load_local_face(
    &self,
    family: &str,
    local_name: &str,
    face: &FontFaceRule,
    order: usize,
    start: Instant,
  ) -> Result<LoadOutcome> {
    let target_style = match face.style {
      FontFaceStyle::Italic => FontStyle::Italic,
      FontFaceStyle::Oblique { .. } => FontStyle::Oblique,
      FontFaceStyle::Normal => FontStyle::Normal,
    };
    let target_weight = face.weight.0;
    if let Some(id) = self.db.resolve_family_list_full(
      &[local_name.to_string()],
      FontWeight::new(target_weight),
      target_style,
      FontStretch::Normal,
    ) {
      if let Some(font) = self.db.load_font(id) {
        if display_allows_use(face.display, start.elapsed()) {
          self.register_web_font(
            family.to_string(),
            Arc::clone(&font.data),
            font.index,
            face,
            order,
          );
          return Ok(LoadOutcome::Loaded);
        }
        return Ok(LoadOutcome::Skipped);
      }
    }
    Err(Error::Font(crate::error::FontError::LoadFailed {
      family: local_name.to_string(),
      reason: "local() source not found".into(),
    }))
  }

  fn load_remote_face(
    &self,
    family: &str,
    url: &str,
    face: &FontFaceRule,
    base_url: Option<&str>,
    order: usize,
    start: Instant,
  ) -> Result<LoadOutcome> {
    let resolved = resolve_font_url(url, base_url);
    if let Some(ctx) = &self.resource_context {
      if let Err(err) = ctx.check_allowed(ResourceKind::Font, &resolved) {
        let blocked = Error::Font(crate::error::FontError::LoadFailed {
          family: family.to_string(),
          reason: err.reason,
        });
        self.record_font_error(&resolved, &blocked);
        return Err(blocked);
      }
    }
    let (bytes, content_type) = match self.fetcher.fetch(&resolved) {
      Ok(result) => result,
      Err(err) => {
        self.record_font_error(&resolved, &err);
        return Err(err);
      }
    };
    let decoded = match decode_font_bytes(bytes, content_type.as_deref()) {
      Ok(decoded) => decoded,
      Err(err) => {
        self.record_font_error(&resolved, &err);
        return Err(err);
      }
    };
    if !display_allows_use(face.display, start.elapsed()) {
      return Ok(LoadOutcome::Skipped);
    }
    let data = Arc::new(decoded);

    let face_count = ttf_parser::fonts_in_collection(&data).unwrap_or(1);
    for idx in 0..face_count.max(1) {
      if ttf_parser::Face::parse(&data, idx).is_err() {
        continue;
      }
      self.register_web_font(family.to_string(), Arc::clone(&data), idx, face, order);
    }

    Ok(LoadOutcome::Loaded)
  }

  fn register_web_font(
    &self,
    family: String,
    data: Arc<Vec<u8>>,
    index: u32,
    face: &FontFaceRule,
    order: usize,
  ) {
    self.declare_web_family(&family);
    let has_math = ttf_parser::Face::parse(&data, index)
      .ok()
      .and_then(|f| f.tables().math)
      .is_some();
    let push_face = WebFontFace {
      family,
      data,
      index,
      style: face.style.clone(),
      display: face.display,
      weight: face.weight,
      stretch: face.stretch,
      order,
      ranges: if face.unicode_ranges.is_empty() {
        vec![(0, 0x10ffff)]
      } else {
        face.unicode_ranges.clone()
      },
      has_math,
    };
    if let Ok(mut guard) = self.web_fonts.write() {
      guard.push(push_face);
    }
    self.bump_generation();
  }

  #[inline]
  pub(crate) fn font_generation(&self) -> u64 {
    self.generation.load(Ordering::Relaxed)
  }

  #[inline]
  fn bump_generation(&self) {
    let _ = self.generation.fetch_add(1, Ordering::Relaxed);
  }

  pub(crate) fn match_web_font_for_char(
    &self,
    family: &str,
    weight: u16,
    style: FontStyle,
    stretch: FontStretch,
    oblique_angle: Option<f32>,
    ch: char,
  ) -> Option<LoadedFont> {
    self.select_web_font(family, weight, style, stretch, oblique_angle, Some(ch))
  }

  fn select_web_font(
    &self,
    family: &str,
    weight: u16,
    style: FontStyle,
    stretch: FontStretch,
    oblique_angle: Option<f32>,
    ch: Option<char>,
  ) -> Option<LoadedFont> {
    let used_codepoints = self
      .web_used_codepoints
      .read()
      .map(|v| v.clone())
      .unwrap_or_default();
    let faces = self.web_fonts.read().ok()?.clone();
    let desired_stretch = stretch.to_percentage();
    let mut best: Option<((u8, f32, f32, u8, (u8, f32), usize), LoadedFont)> = None;

    let family_folded = case_fold(family);
    for face in faces
      .iter()
      .filter(|f| case_fold(&f.family) == family_folded)
    {
      if ch.is_none()
        && !used_codepoints.is_empty()
        && !Self::unicode_range_intersects_used(&face.ranges, &used_codepoints)
      {
        continue;
      }
      if let Some(ch) = ch {
        if !face.supports_char(ch) {
          continue;
        }
      }

      let (stretch_rank, matched_stretch) = match stretch_match(face, desired_stretch) {
        Some(r) => r,
        None => continue,
      };
      let style_rank = style_rank(face, style, oblique_angle);
      let (weight_rank, matched_weight) = match weight_match(face, weight) {
        Some(r) => r,
        None => continue,
      };

      let key = (
        stretch_rank.0,
        stretch_rank.1,
        style_rank.0,
        style_rank.1,
        weight_rank,
        face.order,
      );
      let chosen_stretch = percent_to_stretch_variant(matched_stretch);
      let candidate = face.to_loaded_font(
        &face.family,
        matched_weight,
        face.effective_style(style),
        chosen_stretch,
      );

      if let Some((best_key, _)) = &best {
        if key < *best_key {
          best = Some((key, candidate));
        }
      } else {
        best = Some((key, candidate));
      }
    }

    best.map(|(_, font)| font)
  }

  pub(crate) fn is_web_family_declared(&self, family: &str) -> bool {
    let folded = case_fold(family);
    self
      .web_families
      .read()
      .map(|set| set.contains(&folded))
      .unwrap_or(false)
  }

  #[allow(dead_code)]
  pub(crate) fn has_web_faces(&self, family: &str) -> bool {
    let folded = case_fold(family);
    self
      .web_fonts
      .read()
      .map(|faces| faces.iter().any(|f| case_fold(&f.family) == folded))
      .unwrap_or(false)
  }

  pub(crate) fn supports_feature(&self, font: &LoadedFont, tag: [u8; 4]) -> bool {
    let key = (
      Arc::as_ptr(&font.data) as usize,
      font.index,
      u32::from_be_bytes(tag),
    );
    if let Ok(cache) = self.feature_support.read() {
      if let Some(value) = cache.get(&key) {
        return *value;
      }
    }

    let supported = font_supports_feature(font, tag);
    if let Ok(mut cache) = self.feature_support.write() {
      cache.insert(key, supported);
    }
    supported
  }

  /// Returns font family names that advertise OpenType math support (MATH table).
  ///
  /// The list is ordered by web font declaration order first, then system fonts.
  /// Duplicates (case-insensitive) are removed.
  pub(crate) fn math_family_names(&self) -> Vec<String> {
    let mut seen = HashSet::new();
    let mut names = Vec::new();

    if let Ok(guard) = self.web_fonts.read() {
      for face in guard.iter().filter(|f| f.has_math) {
        let folded = case_fold(&face.family);
        if seen.insert(folded) {
          names.push(face.family.clone());
        }
      }
    }

    for id in self.db.find_math_fonts() {
      if let Some(info) = self.db.inner().face(id) {
        if let Some((name, _)) = info.families.first() {
          let folded = case_fold(name);
          if seen.insert(folded) {
            names.push(name.clone());
          }
        }
      }
    }

    names
  }

  fn declare_web_family(&self, family: &str) {
    if let Ok(mut families) = self.web_families.write() {
      families.insert(case_fold(family));
    }
  }

  // ========================================================================
  // Text Measurement (W3.T18)
  // ========================================================================

  /// Measures the width of text when rendered with the given font
  ///
  /// Uses HarfBuzz (via rustybuzz) for accurate text shaping that accounts
  /// for kerning, ligatures, and other OpenType features.
  ///
  /// # Arguments
  ///
  /// * `text` - The text to measure
  /// * `font` - The loaded font to use for measurement
  /// * `font_size` - Font size in pixels
  ///
  /// # Returns
  ///
  /// The width of the text in pixels.
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let ctx = FontContext::new();
  /// if let Some(font) = ctx.get_sans_serif() {
  ///     let width = ctx.measure_text("Hello, world!", &font, 16.0);
  ///     println!("Text width: {}px", width);
  /// }
  /// ```
  pub fn measure_text(&self, text: &str, font: &LoadedFont, font_size: f32) -> f32 {
    if text.is_empty() {
      return 0.0;
    }

    // Get rustybuzz face for shaping
    let rb_face = match Face::from_slice(&font.data, font.index) {
      Some(face) => face,
      None => return self.estimate_text_width(text, font_size),
    };

    // Shape the text
    let mut buffer = UnicodeBuffer::new();
    buffer.push_str(text);
    buffer.set_direction(Direction::LeftToRight);

    let output = rustybuzz::shape(&rb_face, &[], buffer);
    let positions = output.glyph_positions();

    // Calculate scale from font units to pixels
    let units_per_em = rb_face.units_per_em() as f32;
    let scale = font_size / units_per_em;

    // Sum up horizontal advances
    let mut width: f32 = 0.0;
    for pos in positions {
      width += pos.x_advance as f32 * scale;
    }

    width
  }

  /// Measures text and returns detailed information
  ///
  /// Returns both width and additional shaping information.
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let ctx = FontContext::new();
  /// if let Some(font) = ctx.get_sans_serif() {
  ///     let measurement = ctx.measure_text_detailed("Hello", &font, 16.0);
  ///     println!("Width: {}px, Glyphs: {}", measurement.width, measurement.glyph_count);
  /// }
  /// ```
  pub fn measure_text_detailed(
    &self,
    text: &str,
    font: &LoadedFont,
    font_size: f32,
  ) -> TextMeasurement {
    if text.is_empty() {
      return TextMeasurement {
        width: 0.0,
        glyph_count: 0,
        is_shaped: false,
      };
    }

    // Get rustybuzz face for shaping
    let rb_face = match Face::from_slice(&font.data, font.index) {
      Some(face) => face,
      None => {
        return TextMeasurement {
          width: self.estimate_text_width(text, font_size),
          glyph_count: text.chars().count(),
          is_shaped: false,
        };
      }
    };

    // Shape the text
    let mut buffer = UnicodeBuffer::new();
    buffer.push_str(text);
    buffer.set_direction(Direction::LeftToRight);

    let output = rustybuzz::shape(&rb_face, &[], buffer);
    let positions = output.glyph_positions();

    // Calculate scale from font units to pixels
    let units_per_em = rb_face.units_per_em() as f32;
    let scale = font_size / units_per_em;

    // Sum up horizontal advances
    let mut width: f32 = 0.0;
    for pos in positions {
      width += pos.x_advance as f32 * scale;
    }

    TextMeasurement {
      width,
      glyph_count: positions.len(),
      is_shaped: true,
    }
  }

  /// Estimates text width without shaping
  ///
  /// Used as fallback when font shaping is unavailable.
  fn estimate_text_width(&self, text: &str, font_size: f32) -> f32 {
    // Rough estimate: average character width is about 0.5 * font size
    let char_count = text.chars().count() as f32;
    char_count * font_size * 0.5
  }

  /// Gets scaled metrics for a font at a specific size
  ///
  /// Convenience method that combines font loading with metric scaling.
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let ctx = FontContext::new();
  /// if let Some(font) = ctx.get_sans_serif() {
  ///     let metrics = ctx.get_scaled_metrics(&font, 16.0);
  ///     if let Some(m) = metrics {
  ///         println!("Line height: {}px", m.line_height);
  ///     }
  /// }
  /// ```
  pub fn get_scaled_metrics(&self, font: &LoadedFont, font_size: f32) -> Option<ScaledMetrics> {
    font.metrics().ok().map(|m| m.scale(font_size))
  }

  /// Returns the parsed MATH table for the given font if present.
  pub fn math_table(&self, font: &LoadedFont) -> Option<Arc<MathTableCacheEntry>> {
    let key = (Arc::as_ptr(&font.data) as usize, font.index);
    if let Ok(cache) = self.math_tables.read() {
      if let Some(entry) = cache.get(&key) {
        return entry.clone();
      }
    }

    let table = {
      // SAFETY: the Arc on LoadedFont holds the data for the life of the FontContext, so
      // extending the lifetime to 'static for caching is sound.
      let static_data: &'static [u8] =
        unsafe { std::mem::transmute::<&[u8], &'static [u8]>(&*font.data) };
      let face = ttf_parser::Face::parse(static_data, font.index).ok()?;
      face.tables().math
    };

    let entry = table.map(|table| Arc::new(MathTableCacheEntry { table }));
    if let Ok(mut cache) = self.math_tables.write() {
      cache.insert(key, entry.clone());
    }
    entry
  }

  /// Returns math constants scaled to the given font size.
  pub fn math_constants(&self, font: &LoadedFont, font_size: f32) -> Option<MathConstants> {
    let scale = font.metrics().ok()?.scale(font_size).scale;
    let table = self.math_table(font)?;
    let constants = table.table.constants?;
    let mut out = MathConstants::default();
    out.script_percent_scale_down = Some(constants.script_percent_scale_down() as f32 / 100.0);
    out.script_script_percent_scale_down =
      Some(constants.script_script_percent_scale_down() as f32 / 100.0);
    out.delimited_sub_formula_min_height =
      Some(constants.delimited_sub_formula_min_height() as f32 * scale);
    out.display_operator_min_height = Some(constants.display_operator_min_height() as f32 * scale);
    out.math_leading = Some(scale_math_value(constants.math_leading(), scale));
    out.axis_height = Some(scale_math_value(constants.axis_height(), scale));
    out.accent_base_height = Some(scale_math_value(constants.accent_base_height(), scale));
    out.flattened_accent_base_height = Some(scale_math_value(
      constants.flattened_accent_base_height(),
      scale,
    ));
    out.subscript_shift_down = Some(scale_math_value(constants.subscript_shift_down(), scale));
    out.subscript_top_max = Some(scale_math_value(constants.subscript_top_max(), scale));
    out.subscript_baseline_drop_min = Some(scale_math_value(
      constants.subscript_baseline_drop_min(),
      scale,
    ));
    out.superscript_shift_up = Some(scale_math_value(constants.superscript_shift_up(), scale));
    out.superscript_shift_up_cramped = Some(scale_math_value(
      constants.superscript_shift_up_cramped(),
      scale,
    ));
    out.superscript_bottom_min = Some(scale_math_value(constants.superscript_bottom_min(), scale));
    out.superscript_baseline_drop_max = Some(scale_math_value(
      constants.superscript_baseline_drop_max(),
      scale,
    ));
    out.sub_superscript_gap_min =
      Some(scale_math_value(constants.sub_superscript_gap_min(), scale));
    out.superscript_bottom_max_with_subscript = Some(scale_math_value(
      constants.superscript_bottom_max_with_subscript(),
      scale,
    ));
    out.space_after_script = Some(scale_math_value(constants.space_after_script(), scale));
    out.stretch_stack_gap_above_min = Some(scale_math_value(
      constants.stretch_stack_gap_above_min(),
      scale,
    ));
    out.stretch_stack_gap_below_min = Some(scale_math_value(
      constants.stretch_stack_gap_below_min(),
      scale,
    ));
    out.fraction_numerator_shift_up = Some(scale_math_value(
      constants.fraction_numerator_shift_up(),
      scale,
    ));
    out.fraction_numerator_display_style_shift_up = Some(scale_math_value(
      constants.fraction_numerator_display_style_shift_up(),
      scale,
    ));
    out.fraction_denominator_shift_down = Some(scale_math_value(
      constants.fraction_denominator_shift_down(),
      scale,
    ));
    out.fraction_denominator_display_style_shift_down = Some(scale_math_value(
      constants.fraction_denominator_display_style_shift_down(),
      scale,
    ));
    out.fraction_numerator_gap_min = Some(scale_math_value(
      constants.fraction_numerator_gap_min(),
      scale,
    ));
    out.fraction_num_display_style_gap_min = Some(scale_math_value(
      constants.fraction_num_display_style_gap_min(),
      scale,
    ));
    out.fraction_rule_thickness =
      Some(scale_math_value(constants.fraction_rule_thickness(), scale));
    out.fraction_denominator_gap_min = Some(scale_math_value(
      constants.fraction_denominator_gap_min(),
      scale,
    ));
    out.fraction_denom_display_style_gap_min = Some(scale_math_value(
      constants.fraction_denom_display_style_gap_min(),
      scale,
    ));
    out.overbar_vertical_gap = Some(scale_math_value(constants.overbar_vertical_gap(), scale));
    out.overbar_rule_thickness = Some(scale_math_value(constants.overbar_rule_thickness(), scale));
    out.overbar_extra_ascender = Some(scale_math_value(constants.overbar_extra_ascender(), scale));
    out.underbar_vertical_gap = Some(scale_math_value(constants.underbar_vertical_gap(), scale));
    out.underbar_rule_thickness =
      Some(scale_math_value(constants.underbar_rule_thickness(), scale));
    out.underbar_extra_descender = Some(scale_math_value(
      constants.underbar_extra_descender(),
      scale,
    ));
    out.radical_vertical_gap = Some(scale_math_value(constants.radical_vertical_gap(), scale));
    out.radical_display_style_vertical_gap = Some(scale_math_value(
      constants.radical_display_style_vertical_gap(),
      scale,
    ));
    out.radical_rule_thickness = Some(scale_math_value(constants.radical_rule_thickness(), scale));
    out.radical_extra_ascender = Some(scale_math_value(constants.radical_extra_ascender(), scale));
    out.radical_kern_before_degree = Some(scale_math_value(
      constants.radical_kern_before_degree(),
      scale,
    ));
    out.radical_kern_after_degree = Some(scale_math_value(
      constants.radical_kern_after_degree(),
      scale,
    ));
    out.radical_degree_bottom_raise_percent =
      Some(constants.radical_degree_bottom_raise_percent() as f32);
    if let Some(variants) = table.table.variants {
      out.min_connector_overlap = Some(variants.min_connector_overlap as f32 * scale);
    }
    Some(out)
  }

  /// Math italic correction for a glyph, in pixels.
  pub fn math_italic_correction(
    &self,
    font: &LoadedFont,
    glyph_id: u16,
    font_size: f32,
  ) -> Option<f32> {
    let scale = font.metrics().ok()?.scale(font_size).scale;
    let table = self.math_table(font)?;
    let glyph_info = table.table.glyph_info.as_ref()?;
    let corrections = glyph_info.italic_corrections.as_ref()?;
    Some(scale_math_value(corrections.get(GlyphId(glyph_id))?, scale))
  }

  /// Math kerning for scripts relative to a glyph.
  pub fn math_kern(
    &self,
    font: &LoadedFont,
    glyph_id: u16,
    height: f32,
    font_size: f32,
    superscript: bool,
    side: MathKernSide,
  ) -> f32 {
    let Some(scale) = font.metrics().ok().map(|m| m.scale(font_size).scale) else {
      return 0.0;
    };
    let Some(table) = self.math_table(font) else {
      return 0.0;
    };
    let Some(glyph_info) = table.table.glyph_info.as_ref() else {
      return 0.0;
    };
    let Some(kerns) = glyph_info.kern_infos.as_ref() else {
      return 0.0;
    };
    let Some(info) = kerns.get(GlyphId(glyph_id)) else {
      return 0.0;
    };
    let kern_table = match (side, superscript) {
      (MathKernSide::Right, true) => info.top_right.as_ref(),
      (MathKernSide::Right, false) => info.bottom_right.as_ref(),
      (MathKernSide::Left, true) => info.top_left.as_ref(),
      (MathKernSide::Left, false) => info.bottom_left.as_ref(),
    };
    let Some(kern_table) = kern_table else {
      return 0.0;
    };
    let height_design_units = if scale > 0.0 { height / scale } else { height };
    select_math_kern(kern_table, height_design_units)
      .map(|v| v as f32 * scale)
      .unwrap_or(0.0)
  }

  /// Returns glyph construction data for stretchy operators.
  pub fn math_glyph_construction(
    &self,
    font: &LoadedFont,
    glyph_id: u16,
    vertical: bool,
    font_size: f32,
  ) -> Option<(ttf_parser::math::GlyphConstruction<'static>, f32)> {
    let scale = font.metrics().ok()?.scale(font_size).scale;
    let table = self.math_table(font)?;
    let variants = table.table.variants?;
    let constructions = if vertical {
      &variants.vertical_constructions
    } else {
      &variants.horizontal_constructions
    };
    let construction = constructions.get(GlyphId(glyph_id))?;
    Some((construction, variants.min_connector_overlap as f32 * scale))
  }
}

impl Default for FontContext {
  fn default() -> Self {
    Self::new()
  }
}

#[derive(Clone)]
pub(crate) struct WebFontFace {
  family: String,
  data: Arc<Vec<u8>>,
  index: u32,
  style: FontFaceStyle,
  #[allow(dead_code)]
  display: FontDisplay,
  weight: (u16, u16),
  stretch: (f32, f32),
  order: usize,
  ranges: Vec<(u32, u32)>,
  has_math: bool,
}

impl WebFontFace {
  pub(crate) fn supports_char(&self, ch: char) -> bool {
    let cp = ch as u32;
    if !self.ranges.iter().any(|(s, e)| *s <= cp && cp <= *e) {
      return false;
    }
    ttf_parser::Face::parse(&self.data, self.index)
      .ok()
      .and_then(|f| f.glyph_index(ch))
      .is_some()
  }

  pub(crate) fn effective_style(&self, _requested: FontStyle) -> FontStyle {
    match self.style {
      FontFaceStyle::Normal => FontStyle::Normal,
      FontFaceStyle::Italic => FontStyle::Italic,
      FontFaceStyle::Oblique { .. } => FontStyle::Oblique,
    }
  }

  pub(crate) fn to_loaded_font(
    &self,
    family: &str,
    weight: u16,
    style: FontStyle,
    stretch: FontStretch,
  ) -> LoadedFont {
    LoadedFont {
      data: Arc::clone(&self.data),
      index: self.index,
      family: family.to_string(),
      weight: FontWeight::new(weight),
      style,
      stretch,
    }
  }
}

fn percent_to_stretch_variant(percent: f32) -> FontStretch {
  if percent <= 56.25 {
    FontStretch::UltraCondensed
  } else if percent <= 68.75 {
    FontStretch::ExtraCondensed
  } else if percent <= 81.25 {
    FontStretch::Condensed
  } else if percent <= 93.75 {
    FontStretch::SemiCondensed
  } else if percent <= 106.25 {
    FontStretch::Normal
  } else if percent <= 118.75 {
    FontStretch::SemiExpanded
  } else if percent <= 137.5 {
    FontStretch::Expanded
  } else if percent <= 175.0 {
    FontStretch::ExtraExpanded
  } else {
    FontStretch::UltraExpanded
  }
}

fn stretch_match(face: &WebFontFace, desired: f32) -> Option<((u8, f32), f32)> {
  let (min, max) = face.stretch;
  if min <= desired && desired <= max {
    return Some(((0, 0.0), desired));
  }

  if desired <= 100.0 {
    if max <= desired {
      return Some(((1, desired - max), max));
    }
    if min >= desired {
      return Some(((2, min - desired), min));
    }
  } else {
    if min >= desired {
      return Some(((1, min - desired), min));
    }
    if max <= desired {
      return Some(((2, desired - max), max));
    }
  }

  None
}

fn weight_match(face: &WebFontFace, desired: u16) -> Option<((u8, f32), u16)> {
  let (min, max) = face.weight;
  let desired = desired.clamp(1, 1000);

  if min <= desired && desired <= max {
    return Some(((0, 0.0), desired));
  }

  if (400..=500).contains(&desired) {
    // Phase 1: weights >= desired and <= 500 (ascending)
    if max >= desired && min <= 500 {
      let candidate = desired.max(min).min(500);
      return Some(((1, (candidate as f32 - desired as f32).abs()), candidate));
    }
    // Phase 2: weights below desired (descending)
    if max < desired {
      let candidate = max;
      return Some(((2, (desired as f32 - candidate as f32).abs()), candidate));
    }
    // Phase 3: weights above 500 (ascending)
    if min > 500 {
      let candidate = min;
      return Some(((3, (candidate as f32 - desired as f32).abs()), candidate));
    }
  } else if desired < 400 {
    // Phase 1: weights <= desired (descending)
    if max <= desired {
      let candidate = max;
      return Some(((1, (desired as f32 - candidate as f32).abs()), candidate));
    }
    // Phase 2: weights above desired (ascending)
    if min > desired {
      let candidate = min;
      return Some(((2, (candidate as f32 - desired as f32).abs()), candidate));
    }
  } else {
    // desired > 500
    // Phase 1: weights >= desired (ascending)
    if min >= desired {
      let candidate = min;
      return Some(((1, (candidate as f32 - desired as f32).abs()), candidate));
    }
    // Phase 2: weights below desired (descending)
    if max < desired {
      let candidate = max;
      return Some(((2, (desired as f32 - candidate as f32).abs()), candidate));
    }
  }

  None
}

fn style_rank(face: &WebFontFace, desired: FontStyle, angle: Option<f32>) -> (f32, u8) {
  #[derive(Copy, Clone, Eq, PartialEq)]
  enum FaceKind {
    Normal,
    Oblique,
    Italic,
  }

  let requested_angle = match desired {
    FontStyle::Normal => 0.0,
    FontStyle::Italic => DEFAULT_OBLIQUE_ANGLE_DEG,
    FontStyle::Oblique => angle.unwrap_or(DEFAULT_OBLIQUE_ANGLE_DEG),
  };

  let face_kind = match face.style {
    FontFaceStyle::Normal => FaceKind::Normal,
    FontFaceStyle::Italic => FaceKind::Italic,
    FontFaceStyle::Oblique { .. } => FaceKind::Oblique,
  };

  let distance = match &face.style {
    FontFaceStyle::Normal => requested_angle.abs(),
    FontFaceStyle::Italic => (requested_angle - DEFAULT_OBLIQUE_ANGLE_DEG).abs(),
    FontFaceStyle::Oblique { range } => {
      let (start, end) = range.unwrap_or((DEFAULT_OBLIQUE_ANGLE_DEG, DEFAULT_OBLIQUE_ANGLE_DEG));
      let clamped = requested_angle.clamp(start, end);
      (requested_angle - clamped).abs()
    }
  };

  let preference = match desired {
    FontStyle::Normal => [FaceKind::Normal, FaceKind::Oblique, FaceKind::Italic],
    FontStyle::Italic => [FaceKind::Italic, FaceKind::Oblique, FaceKind::Normal],
    FontStyle::Oblique => [FaceKind::Oblique, FaceKind::Italic, FaceKind::Normal],
  };
  let tie_bias = preference
    .iter()
    .position(|k| *k == face_kind)
    .unwrap_or(preference.len()) as u8;

  (distance, tie_bias)
}

fn case_fold(name: &str) -> String {
  let mut out = String::with_capacity(name.len());
  for ch in name.chars() {
    match ch {
      // Unicode case folding special-cases
      '\u{00DF}' | '\u{1E9E}' => out.push_str("ss"), // ß/ẞ
      '\u{03C2}' => out.push('\u{03C3}'),            // final sigma -> sigma
      '\u{212A}' => out.push('k'),                   // Kelvin sign
      '\u{212B}' => out.push('\u{00E5}'),            // Angstrom sign -> å
      _ => {
        for lower in ch.to_lowercase() {
          out.push(lower);
        }
      }
    }
  }
  out
}

fn scale_math_value(value: ttf_parser::math::MathValue<'_>, scale: f32) -> f32 {
  value.value as f32 * scale
}

fn select_math_kern(kern: &ttf_parser::math::Kern<'_>, height_design_units: f32) -> Option<i16> {
  let count = kern.count();
  if count == 0 {
    return kern.kern(0).map(|k| k.value);
  }
  for idx in 0..count {
    let h = kern.height(idx)?;
    if height_design_units < h.value as f32 {
      return kern.kern(idx).map(|k| k.value);
    }
  }
  kern.kern(count).map(|k| k.value)
}

fn font_supports_feature(font: &LoadedFont, tag: [u8; 4]) -> bool {
  let face = match Face::from_slice(&font.data, font.index) {
    Some(f) => f,
    None => return false,
  };

  let mut base = UnicodeBuffer::new();
  base.push_str("x");
  let base_shape = rustybuzz::shape(&face, &[], base);
  let mut feature_buf = UnicodeBuffer::new();
  feature_buf.push_str("x");
  let feature = Feature {
    tag: Tag::from_bytes(&tag),
    value: 1,
    start: 0,
    end: u32::MAX,
  };
  let feature_shape = rustybuzz::shape(&face, &[feature], feature_buf);

  let base_info = base_shape.glyph_infos().first().copied();
  let feature_info = feature_shape.glyph_infos().first().copied();
  let base_pos = base_shape.glyph_positions().first().copied();
  let feature_pos = feature_shape.glyph_positions().first().copied();

  if let (Some(bi), Some(fi), Some(bp), Some(fp)) = (base_info, feature_info, base_pos, feature_pos)
  {
    bi.glyph_id != fi.glyph_id || bp.x_advance != fp.x_advance || bp.y_offset != fp.y_offset
  } else {
    false
  }
}

fn resolve_font_url(url: &str, base_url: Option<&str>) -> String {
  if url.starts_with("http://")
    || url.starts_with("https://")
    || url.starts_with("data:")
    || url.starts_with("file:")
  {
    return url.to_string();
  }

  if let Some(base) = base_url {
    if let Ok(base) = Url::parse(base)
      .or_else(|_| Url::from_file_path(base).map_err(|()| url::ParseError::RelativeUrlWithoutBase))
    {
      if let Ok(resolved) = base.join(url) {
        return resolved.to_string();
      }
    }
  }

  url.to_string()
}

fn fetch_font_bytes(url: &str) -> Result<(Vec<u8>, Option<String>)> {
  if url.starts_with("data:") {
    return decode_data_url(url);
  }

  if url.starts_with("http://") || url.starts_with("https://") {
    let agent: Agent = Agent::config_builder()
      .timeout_global(Some(std::time::Duration::from_secs(10)))
      .build()
      .into();
    let mut response = agent.get(url).call().map_err(|e| {
      Error::Font(crate::error::FontError::LoadFailed {
        family: url.to_string(),
        reason: e.to_string(),
      })
    })?;
    let bytes = response
      .body_mut()
      .with_config()
      .limit(50 * 1024 * 1024)
      .read_to_vec()
      .map_err(|e| {
        Error::Font(crate::error::FontError::LoadFailed {
          family: url.to_string(),
          reason: e.to_string(),
        })
      })?;
    let content_type = response
      .headers()
      .get("content-type")
      .and_then(|h| h.to_str().ok())
      .map(|s| s.to_string());
    return Ok((bytes, content_type));
  }

  if url.starts_with("file://") {
    let path = url.trim_start_matches("file://");
    return std::fs::read(path).map(|b| (b, None)).map_err(|e| {
      Error::Font(crate::error::FontError::LoadFailed {
        family: url.to_string(),
        reason: e.to_string(),
      })
    });
  }

  std::fs::read(url).map(|b| (b, None)).map_err(|e| {
    Error::Font(crate::error::FontError::LoadFailed {
      family: url.to_string(),
      reason: e.to_string(),
    })
  })
}

fn ordered_sources<'a>(sources: &'a [FontFaceSource]) -> Vec<&'a FontFaceSource> {
  let mut ordered = Vec::new();
  let mut idx = 0;

  while idx < sources.len() {
    match &sources[idx] {
      FontFaceSource::Local(_) => {
        ordered.push(&sources[idx]);
        idx += 1;
      }
      FontFaceSource::Url(_) => {
        let start = idx;
        while idx < sources.len() && matches!(sources[idx], FontFaceSource::Url(_)) {
          idx += 1;
        }
        let mut remotes: Vec<(usize, usize, &FontFaceSource)> = (start..idx)
          .filter_map(|i| {
            let rank = match &sources[i] {
              FontFaceSource::Url(url) => format_support_rank(&url.format_hints),
              _ => None,
            }?;
            Some((rank, i, &sources[i]))
          })
          .collect();

        remotes.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
        ordered.extend(remotes.into_iter().map(|(_, _, src)| src));
      }
    }
  }

  ordered
}

fn format_support_rank(hints: &[FontSourceFormat]) -> Option<usize> {
  if hints.is_empty() {
    return Some(3);
  }

  let mut best: Option<usize> = None;
  for hint in hints {
    let rank = match hint {
      FontSourceFormat::Woff2 => Some(0),
      FontSourceFormat::Woff => Some(1),
      FontSourceFormat::Opentype | FontSourceFormat::Truetype | FontSourceFormat::Collection => {
        Some(2)
      }
      FontSourceFormat::Unknown(_) => Some(4),
      FontSourceFormat::EmbeddedOpenType | FontSourceFormat::Svg => None,
    };

    if let Some(rank) = rank {
      best = Some(best.map_or(rank, |current| current.min(rank)));
    }
  }

  best
}

fn decode_font_bytes(bytes: Vec<u8>, content_type: Option<&str>) -> Result<Vec<u8>> {
  let content_type = content_type.map(|c| c.to_ascii_lowercase());
  let content_type = content_type.as_deref();

  if bytes.starts_with(b"wOF2") || content_type.map(|c| c.contains("woff2")).unwrap_or(false) {
    return decompress_woff2(&bytes).map_err(|e| {
      Error::Font(crate::error::FontError::LoadFailed {
        family: "woff2".into(),
        reason: format!("{:?}", e),
      })
    });
  }

  if bytes.starts_with(b"wOFF") || content_type.map(|c| c.contains("woff")).unwrap_or(false) {
    return decompress_woff1(&bytes).map_err(|e| {
      Error::Font(crate::error::FontError::LoadFailed {
        family: "woff".into(),
        reason: format!("{:?}", e),
      })
    });
  }

  Ok(bytes)
}

fn decode_data_url(url: &str) -> Result<(Vec<u8>, Option<String>)> {
  let without_prefix = url.trim_start_matches("data:");
  let mut parts = without_prefix.splitn(2, ',');
  let meta = parts.next().unwrap_or("");
  let data = parts.next().ok_or_else(|| {
    Error::Font(crate::error::FontError::LoadFailed {
      family: "data-url".into(),
      reason: "missing data".into(),
    })
  })?;
  let is_base64 = meta.to_ascii_lowercase().contains(";base64");
  let mime = meta
    .split(';')
    .next()
    .filter(|s| !s.is_empty())
    .map(|s| s.to_string());

  if is_base64 {
    let decoded = BASE64_STANDARD.decode(data.as_bytes()).map_err(|e| {
      Error::Font(crate::error::FontError::LoadFailed {
        family: "data-url".into(),
        reason: e.to_string(),
      })
    })?;
    return Ok((decoded, mime));
  }

  let decoded = percent_decode_str(data).decode_utf8().map_err(|e| {
    Error::Font(crate::error::FontError::LoadFailed {
      family: "data-url".into(),
      reason: e.to_string(),
    })
  })?;
  Ok((decoded.as_bytes().to_vec(), mime))
}

// ============================================================================
// TextMeasurement
// ============================================================================

/// Result of detailed text measurement
///
/// Contains width and additional information about the shaped text.
#[derive(Debug, Clone)]
pub struct TextMeasurement {
  /// Total width in pixels
  pub width: f32,

  /// Number of glyphs after shaping
  ///
  /// May differ from character count due to ligatures, combining marks, etc.
  pub glyph_count: usize,

  /// Whether text was shaped with HarfBuzz
  ///
  /// If false, width is an estimate.
  pub is_shaped: bool,
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::style::media::MediaContext;
  use crate::ComputedStyle;
  use std::collections::HashMap;
  use std::sync::Arc;
  use std::sync::Mutex;
  use std::thread;
  use std::time::Duration;

  fn system_font_for_char(ch: char) -> Option<(Vec<u8>, String)> {
    let db = FontDatabase::new();
    let id = db
      .faces()
      .find(|face| db.has_glyph(face.id, ch))
      .map(|face| face.id)?;
    let font = db.load_font(id)?;
    Some(((*font.data).clone(), font.family))
  }

  fn system_font_file_url_for_char(ch: char) -> Option<(tempfile::TempDir, Url)> {
    let (data, _family) = system_font_for_char(ch)?;
    let dir = tempfile::tempdir().ok()?;
    let path = dir.path().join("fixture.ttf");
    fs::write(&path, data).ok()?;
    let url = Url::from_file_path(&path).ok()?;
    Some((dir, url))
  }

  struct StubFetcher {
    data: Vec<u8>,
    delay: Duration,
    fail: bool,
  }

  impl FontFetcher for StubFetcher {
    fn fetch(&self, _url: &str) -> Result<(Vec<u8>, Option<String>)> {
      if self.delay > Duration::ZERO {
        thread::sleep(self.delay);
      }
      if self.fail {
        return Err(Error::Font(crate::error::FontError::LoadFailed {
          family: "stub".into(),
          reason: "intentional failure".into(),
        }));
      }
      Ok((self.data.clone(), None))
    }
  }

  struct RecordingFetcher {
    responses: HashMap<String, (Vec<u8>, Option<String>)>,
    calls: Mutex<Vec<String>>,
  }

  impl RecordingFetcher {
    fn new(entries: Vec<(String, Vec<u8>, Option<String>)>) -> Self {
      let mut responses = HashMap::new();
      for (url, data, content_type) in entries {
        responses.insert(url, (data, content_type));
      }
      Self {
        responses,
        calls: Mutex::new(Vec::new()),
      }
    }

    fn calls(&self) -> Vec<String> {
      self.calls.lock().map(|c| c.clone()).unwrap_or_default()
    }
  }

  impl FontFetcher for RecordingFetcher {
    fn fetch(&self, url: &str) -> Result<(Vec<u8>, Option<String>)> {
      if let Ok(mut calls) = self.calls.lock() {
        calls.push(url.to_string());
      }

      self.responses.get(url).cloned().ok_or_else(|| {
        Error::Font(crate::error::FontError::LoadFailed {
          family: url.to_string(),
          reason: "missing response".into(),
        })
      })
    }
  }

  #[test]
  fn test_font_context_creation() {
    let ctx = FontContext::new();
    // May have 0 fonts in minimal CI environments
    // font_count() returns usize which is always >= 0
    let _ = ctx.font_count(); // Just verify it works
  }

  #[test]
  fn test_font_context_empty() {
    let ctx = FontContext::empty();
    assert!(!ctx.has_fonts());
    assert_eq!(ctx.font_count(), 0);
  }

  #[test]
  fn test_font_context_with_database() {
    let db = Arc::new(FontDatabase::new());
    let count = db.font_count();

    let ctx = FontContext::with_database(db);
    assert_eq!(ctx.font_count(), count);
  }

  #[test]
  fn loads_web_font_from_data_url() {
    let Some((font_data, _family)) = system_font_for_char('A') else {
      return;
    };
    let data_url = format!("data:font/ttf;base64,{}", BASE64_STANDARD.encode(font_data));
    let css = format!(
            "@font-face {{ font-family: \"WebFont\"; src: url(\"{}\"); font-style: normal; font-weight: 400; }}",
            data_url
        );
    let sheet = crate::css::parser::parse_stylesheet(&css).unwrap();
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let faces = sheet.collect_font_face_rules(&media_ctx);

    let ctx = FontContext::empty();
    assert!(ctx.load_web_fonts(&faces, None, None).is_ok());
    let families = vec!["WebFont".to_string()];
    let loaded = ctx.get_font_full(&families, 400, FontStyle::Normal, FontStretch::Normal);
    assert!(loaded.is_some());
  }

  #[test]
  fn loads_woff2_fixture_font() {
    let path = std::path::Path::new("tests/fixtures/fonts/DejaVuSans-subset.woff2");
    if !path.exists() {
      return;
    }
    let path = path.canonicalize().expect("canonical fixture path");
    let url = Url::from_file_path(&path).expect("url for fixture");
    let css = format!(
      "@font-face {{ font-family: \"SubsetWoff2\"; src: url(\"{}\") format(\"woff2\"); font-style: normal; font-weight: 400; }}",
      url
    );
    let sheet = crate::css::parser::parse_stylesheet(&css).unwrap();
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let faces = sheet.collect_font_face_rules(&media_ctx);

    let ctx = FontContext::empty();
    ctx
      .load_web_fonts(&faces, None, None)
      .expect("load woff2 face");

    let families = vec!["SubsetWoff2".to_string()];
    let font = ctx
      .get_font_full(&families, 400, FontStyle::Normal, FontStretch::Normal)
      .expect("web font to be available");
    assert_eq!(font.family, "SubsetWoff2");

    let width = ctx.measure_text("WOFF2", &font, 32.0);
    assert!(width > 0.0);
  }

  #[test]
  fn prefers_best_supported_format_source() {
    let woff2_path = std::path::Path::new("tests/fixtures/fonts/DejaVuSans-subset.woff2");
    let ttf_path = std::path::Path::new("tests/fixtures/fonts/DejaVuSans-subset.ttf");
    if !(woff2_path.exists() && ttf_path.exists()) {
      return;
    }

    let woff2_bytes = fs::read(woff2_path).expect("read woff2 fixture");
    let ttf_bytes = fs::read(ttf_path).expect("read ttf fixture");

    let fetcher = Arc::new(RecordingFetcher::new(vec![
      (
        "https://example.com/font.ttf".to_string(),
        ttf_bytes,
        Some("font/ttf".to_string()),
      ),
      (
        "https://example.com/font.woff2".to_string(),
        woff2_bytes,
        Some("font/woff2".to_string()),
      ),
    ]));

    let css = r#"@font-face { font-family: "FormatPref"; src: url("https://example.com/font.ttf") format("truetype"), url("https://example.com/font.woff2") format("woff2"); }"#;
    let sheet = crate::css::parser::parse_stylesheet(css).unwrap();
    let media_ctx = MediaContext::screen(800.0, 600.0);
    let faces = sheet.collect_font_face_rules(&media_ctx);

    let ctx = FontContext::with_fetcher(fetcher.clone());
    ctx
      .load_web_fonts(&faces, None, None)
      .expect("schedule format loads");
    assert!(ctx.wait_for_pending_web_fonts(Duration::from_secs(1)));

    let calls = fetcher.calls();
    assert!(
      !calls.is_empty(),
      "fetcher should have been invoked for at least one source"
    );
    assert_eq!(calls[0], "https://example.com/font.woff2");
    assert!(ctx.has_web_faces("FormatPref"));
  }

  #[test]
  fn declared_web_family_blocks_platform_fallback() {
    let Some((data, platform_family)) = system_font_for_char('A') else {
      return;
    };
    let mut db = FontDatabase::empty();
    db.load_font_data(data).expect("load platform font");
    let ctx = FontContext::with_database(Arc::new(db));

    let face = FontFaceRule {
      family: Some(platform_family.clone()),
      sources: vec![FontFaceSource::url("file:///no/such/font.woff".to_string())],
      ..Default::default()
    };
    ctx
      .load_web_fonts(&[face], None, None)
      .expect("load declared face");

    assert!(ctx.is_web_family_declared(&platform_family));
    assert!(!ctx.has_web_faces(&platform_family));

    let families = vec![platform_family];
    let resolved = ctx.get_font_full(&families, 400, FontStyle::Normal, FontStretch::Normal);
    assert!(
      resolved.is_none(),
      "platform fonts must not satisfy a family declared via @font-face when no faces load"
    );
  }

  #[test]
  fn web_fonts_filter_to_used_codepoints() {
    let Some((_dir, font_url)) = system_font_file_url_for_char('A') else {
      return;
    };

    let matching = FontFaceRule {
      family: Some("Match".to_string()),
      sources: vec![FontFaceSource::url(font_url.to_string())],
      unicode_ranges: vec![(0x0041, 0x005a)],
      ..Default::default()
    };
    let skipped = FontFaceRule {
      family: Some("Skip".to_string()),
      sources: vec![FontFaceSource::url(font_url.to_string())],
      unicode_ranges: vec![(0x4e00, 0x4e10)],
      ..Default::default()
    };

    let ctx = FontContext::new();
    let used = vec![0x0041];
    ctx
      .load_web_fonts(&[matching, skipped], None, Some(&used))
      .expect("load filtered fonts");

    assert!(ctx.is_web_family_declared("Match"));
    assert!(ctx.is_web_family_declared("Skip"));

    assert!(ctx.has_web_faces("Match"), "matching range should load");
    assert!(
      !ctx.has_web_faces("Skip"),
      "non-intersecting range should be skipped"
    );
  }

  #[test]
  fn font_display_optional_skips_slow_loads() {
    let Some((data, fallback_family)) = system_font_for_char('A') else {
      return;
    };
    let fetcher = Arc::new(StubFetcher {
      data: data.clone(),
      delay: Duration::from_millis(200),
      fail: false,
    });
    let mut db = FontDatabase::empty();
    db.load_font_data(data).expect("load fallback");
    let ctx = FontContext::with_database_and_fetcher(Arc::new(db), fetcher);

    let face = FontFaceRule {
      family: Some("OptFace".to_string()),
      sources: vec![FontFaceSource::url(
        "http://example.com/font.ttf".to_string(),
      )],
      display: FontDisplay::Optional,
      ..Default::default()
    };
    ctx
      .load_web_fonts(&[face], None, None)
      .expect("schedule optional load");

    assert!(
      ctx.wait_for_pending_web_fonts(Duration::from_secs(1)),
      "pending font loads should settle"
    );
    assert!(
      !ctx.has_web_faces("OptFace"),
      "optional fonts arriving after the block period should be dropped"
    );

    let families = vec!["OptFace".to_string(), fallback_family];
    let resolved = ctx.get_font_full(&families, 400, FontStyle::Normal, FontStretch::Normal);
    assert!(resolved.is_some(), "fallback font should still resolve");
    assert_ne!(resolved.unwrap().family, "OptFace");
  }

  #[test]
  fn font_display_fallback_drops_after_swap_timeout() {
    let Some((data, fallback_family)) = system_font_for_char('A') else {
      return;
    };
    let fetcher = Arc::new(StubFetcher {
      data: data.clone(),
      delay: Duration::from_millis(800),
      fail: false,
    });
    let mut db = FontDatabase::empty();
    db.load_font_data(data).expect("load fallback");
    let ctx = FontContext::with_database_and_fetcher(Arc::new(db), fetcher);

    let face = FontFaceRule {
      family: Some("LateFallback".to_string()),
      sources: vec![FontFaceSource::url(
        "http://example.com/font.ttf".to_string(),
      )],
      display: FontDisplay::Fallback,
      ..Default::default()
    };
    ctx
      .load_web_fonts(&[face], None, None)
      .expect("schedule fallback load");

    assert!(ctx.wait_for_pending_web_fonts(Duration::from_secs(2)));
    assert!(
      !ctx.has_web_faces("LateFallback"),
      "fallback fonts that finish after the swap period should stick with fallback"
    );

    let families = vec!["LateFallback".to_string(), fallback_family];
    let resolved = ctx.get_font_full(&families, 400, FontStyle::Normal, FontStretch::Normal);
    assert!(resolved.is_some());
    assert_ne!(resolved.unwrap().family, "LateFallback");
  }

  #[test]
  fn font_display_block_waits_for_fast_load() {
    let Some((data, _family)) = system_font_for_char('A') else {
      return;
    };
    let fetcher = Arc::new(StubFetcher {
      data: data.clone(),
      delay: Duration::from_millis(0),
      fail: false,
    });
    let ctx = FontContext::with_fetcher(fetcher);

    let face = FontFaceRule {
      family: Some("BlockFace".to_string()),
      sources: vec![FontFaceSource::url(
        "http://example.com/font.ttf".to_string(),
      )],
      display: FontDisplay::Block,
      ..Default::default()
    };
    ctx
      .load_web_fonts(&[face], None, None)
      .expect("load block face");

    assert!(
      ctx.has_web_faces("BlockFace"),
      "block display should wait for quick loads"
    );
  }

  #[test]
  fn failed_font_load_falls_back() {
    let Some((data, fallback_family)) = system_font_for_char('A') else {
      return;
    };
    let fetcher = Arc::new(StubFetcher {
      data,
      delay: Duration::from_millis(0),
      fail: true,
    });
    let ctx = FontContext::with_fetcher(fetcher);

    let face = FontFaceRule {
      family: Some("Broken".to_string()),
      sources: vec![FontFaceSource::url(
        "http://example.com/font.ttf".to_string(),
      )],
      display: FontDisplay::Swap,
      ..Default::default()
    };
    ctx
      .load_web_fonts(&[face], None, None)
      .expect("schedule broken face");
    let _ = ctx.wait_for_pending_web_fonts(Duration::from_millis(200));

    assert!(
      !ctx.has_web_faces("Broken"),
      "failed downloads must not register web faces"
    );

    let mut style = ComputedStyle::default();
    style.font_family = vec!["Broken".to_string(), fallback_family];
    style.font_size = 14.0;

    let pipeline = crate::text::pipeline::ShapingPipeline::new();
    let runs = match pipeline.shape("A", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if runs.is_empty() {
      return;
    }
    assert_ne!(runs[0].font.family, "Broken");
  }

  #[test]
  fn font_display_swap_eventually_installs() {
    let Some((data, _family)) = system_font_for_char('A') else {
      return;
    };
    let fetcher = Arc::new(StubFetcher {
      data: data.clone(),
      delay: Duration::from_millis(200),
      fail: false,
    });
    let ctx = FontContext::with_fetcher(fetcher);

    let face = FontFaceRule {
      family: Some("SwapFace".to_string()),
      sources: vec![FontFaceSource::url(
        "http://example.com/font.ttf".to_string(),
      )],
      display: FontDisplay::Swap,
      ..Default::default()
    };
    ctx
      .load_web_fonts(&[face], None, None)
      .expect("schedule swap load");

    assert!(ctx.wait_for_pending_web_fonts(Duration::from_secs(1)));
    assert!(ctx.has_web_faces("SwapFace"));
  }

  #[test]
  fn swap_fonts_upgrade_after_load_without_large_jump() {
    let Some((fallback_data, fallback_family)) = system_font_for_char('A') else {
      return;
    };
    let fetcher = Arc::new(StubFetcher {
      data: fallback_data.clone(),
      delay: Duration::from_millis(150),
      fail: false,
    });
    let mut db = FontDatabase::empty();
    db.load_font_data(fallback_data)
      .expect("load fallback font");
    let ctx = FontContext::with_database_and_fetcher(Arc::new(db), fetcher);

    let face = FontFaceRule {
      family: Some("SwapUpgrade".to_string()),
      sources: vec![FontFaceSource::url(
        "http://example.com/font.ttf".to_string(),
      )],
      display: FontDisplay::Swap,
      ..Default::default()
    };
    ctx
      .load_web_fonts(&[face], None, None)
      .expect("schedule swap upgrade");

    let mut style = ComputedStyle::default();
    style.font_family = vec!["SwapUpgrade".to_string(), fallback_family.clone()];
    style.font_size = 14.0;

    let pipeline = crate::text::pipeline::ShapingPipeline::new();
    let before = match pipeline.shape("A", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if before.is_empty() {
      return;
    }
    assert_eq!(
      before[0].font.family, fallback_family,
      "swap display should use fallback while the web font is pending"
    );

    assert!(ctx.wait_for_pending_web_fonts(Duration::from_secs(1)));
    let after = match pipeline.shape("A", &style, &ctx) {
      Ok(runs) => runs,
      Err(_) => return,
    };
    if after.is_empty() {
      return;
    }
    assert_eq!(after[0].font.family, "SwapUpgrade");
    let delta = (after[0].advance - before[0].advance).abs();
    assert!(
      delta < 0.5,
      "swap upgrade should avoid large layout jumps for matching metrics"
    );
  }

  #[test]
  fn caseless_family_matching_handles_unicode() {
    let Some((_dir, font_url)) = system_font_file_url_for_char('A') else {
      return;
    };
    let face = FontFaceRule {
      family: Some("Straße".to_string()),
      sources: vec![FontFaceSource::url(font_url.to_string())],
      ..Default::default()
    };

    let ctx = FontContext::empty();
    ctx.load_web_fonts(&[face], None, None).expect("load face");

    let font = ctx
      .match_web_font_for_char(
        "STRASSE",
        400,
        FontStyle::Normal,
        FontStretch::Normal,
        None,
        'A',
      )
      .expect("find caseless match");
    assert_eq!(case_fold(&font.family), case_fold("Straße"));
  }

  #[test]
  fn oblique_range_matching_picks_covering_face() {
    let Some((_dir, font_url)) = system_font_file_url_for_char('A') else {
      return;
    };
    let family = "ObliqueRange".to_string();

    let faces = vec![
      FontFaceRule {
        family: Some(family.clone()),
        sources: vec![FontFaceSource::url(font_url.to_string())],
        style: FontFaceStyle::Oblique {
          range: Some((0.0, 10.0)),
        },
        weight: (400, 400),
        ..Default::default()
      },
      FontFaceRule {
        family: Some(family.clone()),
        sources: vec![FontFaceSource::url(font_url.to_string())],
        style: FontFaceStyle::Oblique {
          range: Some((20.0, 30.0)),
        },
        weight: (900, 900),
        ..Default::default()
      },
    ];

    let ctx = FontContext::empty();
    ctx.load_web_fonts(&faces, None, None).expect("load faces");

    let chosen = ctx
      .match_web_font_for_char(
        &family,
        400,
        FontStyle::Oblique,
        FontStretch::Normal,
        Some(25.0),
        'A',
      )
      .expect("choose face");
    assert_eq!(
      chosen.weight.value(),
      900,
      "face covering the requested oblique angle should win even with a heavier weight"
    );
  }

  #[test]
  fn normal_style_prefers_normal_face_over_oblique_with_zero_distance() {
    let Some((_dir, font_url)) = system_font_file_url_for_char('A') else {
      return;
    };
    let family = "StylePreference".to_string();

    let faces = vec![
      FontFaceRule {
        family: Some(family.clone()),
        sources: vec![FontFaceSource::url(font_url.to_string())],
        style: FontFaceStyle::Normal,
        weight: (400, 400),
        ..Default::default()
      },
      FontFaceRule {
        family: Some(family.clone()),
        sources: vec![FontFaceSource::url(font_url.to_string())],
        style: FontFaceStyle::Oblique {
          range: Some((0.0, 20.0)),
        },
        weight: (400, 400),
        ..Default::default()
      },
    ];

    let ctx = FontContext::empty();
    ctx.load_web_fonts(&faces, None, None).expect("load faces");

    let chosen = ctx
      .match_web_font_for_char(
        &family,
        400,
        FontStyle::Normal,
        FontStretch::Normal,
        None,
        'A',
      )
      .expect("choose face");
    assert_eq!(
      chosen.style,
      FontStyle::Normal,
      "normal requests should pick the normal face when distance ties"
    );
  }

  #[test]
  fn italic_style_prefers_italic_over_oblique_at_same_angle() {
    let Some((_dir, font_url)) = system_font_file_url_for_char('A') else {
      return;
    };
    let family = "ItalicPreference".to_string();

    let faces = vec![
      FontFaceRule {
        family: Some(family.clone()),
        sources: vec![FontFaceSource::url(font_url.to_string())],
        style: FontFaceStyle::Italic,
        weight: (400, 400),
        ..Default::default()
      },
      FontFaceRule {
        family: Some(family.clone()),
        sources: vec![FontFaceSource::url(font_url.to_string())],
        style: FontFaceStyle::Oblique {
          range: Some((0.0, 90.0)),
        },
        weight: (400, 400),
        ..Default::default()
      },
    ];

    let ctx = FontContext::empty();
    ctx.load_web_fonts(&faces, None, None).expect("load faces");

    let chosen = ctx
      .match_web_font_for_char(
        &family,
        400,
        FontStyle::Italic,
        FontStretch::Normal,
        None,
        'A',
      )
      .expect("choose face");
    assert_eq!(
      chosen.style,
      FontStyle::Italic,
      "italic requests should prefer italic faces over oblique when distance is equal"
    );
  }

  #[test]
  fn test_get_font_fallback() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    let families = vec![
      "NonExistentFont123456".to_string(),
      "sans-serif".to_string(),
    ];

    let font = ctx.get_font(&families, 400, false, false);
    if let Some(font) = font {
      assert!(!font.data.is_empty());
    }
  }

  #[test]
  fn test_get_sans_serif() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    let font = ctx.get_sans_serif();
    if font.is_some() {
      assert!(!font.unwrap().family.is_empty());
    }
  }

  #[test]
  fn test_get_serif() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    let font = ctx.get_serif();
    if font.is_some() {
      assert!(!font.unwrap().data.is_empty());
    }
  }

  #[test]
  fn test_get_monospace() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    let font = ctx.get_monospace();
    if font.is_some() {
      assert!(!font.unwrap().data.is_empty());
    }
  }

  #[test]
  fn test_font_context_clone() {
    let ctx1 = FontContext::new();
    let ctx2 = ctx1.clone();

    // Both should point to same database
    assert_eq!(ctx1.font_count(), ctx2.font_count());
  }

  #[test]
  fn test_get_font_italic() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    let families = vec!["sans-serif".to_string()];
    let font = ctx.get_font(&families, 400, true, false);
    // May or may not find italic version
    if font.is_some() {
      assert!(!font.unwrap().data.is_empty());
    }
  }

  #[test]
  fn test_get_font_bold() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    let families = vec!["sans-serif".to_string()];
    let font = ctx.get_font(&families, 700, false, false);
    if let Some(font) = font {
      // Weight may not be exactly 700 due to fuzzy matching
      assert!(font.weight.value() >= 400);
    }
  }

  #[test]
  fn test_get_font_full() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    let families = vec!["sans-serif".to_string()];
    let font = ctx.get_font_full(&families, 400, FontStyle::Normal, FontStretch::Normal);
    if let Some(font) = font {
      assert!(!font.data.is_empty());
    }
  }

  #[test]
  fn test_get_font_falls_back_when_slope_missing() {
    let Some((data, family)) = system_font_for_char('A') else {
      return;
    };
    let mut db = FontDatabase::empty();
    db.load_font_data(data).expect("load fixture font");
    let ctx = FontContext::with_database(Arc::new(db));

    let families = vec![family];
    // Request italic + heavier weight even though only a normal face exists; should still return a font.
    let font = ctx.get_font(&families, 700, true, false);
    assert!(font.is_some());
  }

  #[test]
  fn test_clear_cache() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    // Load a font to populate cache
    let _ = ctx.get_sans_serif();

    // Clear cache
    ctx.clear_cache();

    // Should still work (reloads from disk)
    let font = ctx.get_sans_serif();
    if font.is_some() {
      assert!(!font.unwrap().data.is_empty());
    }
  }

  #[test]
  fn fixture_font_loads_into_database() {
    let Some((data, _family)) = system_font_for_char('A') else {
      return;
    };
    let mut db = FontDatabase::empty();
    let before = db.font_count();
    let _ = db.load_font_data(data);
    assert!(db.font_count() > before);
  }

  // ========================================================================
  // Text Measurement Tests (W3.T18)
  // ========================================================================

  #[test]
  fn test_measure_text_empty() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    if let Some(font) = ctx.get_sans_serif() {
      let width = ctx.measure_text("", &font, 16.0);
      assert_eq!(width, 0.0);
    }
  }

  #[test]
  fn test_measure_text_single_char() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    if let Some(font) = ctx.get_sans_serif() {
      let width = ctx.measure_text("A", &font, 16.0);
      // Single character should have positive width
      assert!(width > 0.0);
      // And reasonable width (not more than font size for most chars)
      assert!(width < 20.0);
    }
  }

  #[test]
  fn test_measure_text_multiple_chars() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    if let Some(font) = ctx.get_sans_serif() {
      let width1 = ctx.measure_text("A", &font, 16.0);
      let width5 = ctx.measure_text("AAAAA", &font, 16.0);

      // 5 chars should be wider than 1 char
      assert!(width5 > width1);
      // Should be roughly 5x (with some tolerance for kerning)
      assert!(width5 > width1 * 4.0);
      assert!(width5 < width1 * 6.0);
    }
  }

  #[test]
  fn test_measure_text_font_size_scaling() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    if let Some(font) = ctx.get_sans_serif() {
      let width16 = ctx.measure_text("Hello", &font, 16.0);
      let width32 = ctx.measure_text("Hello", &font, 32.0);

      // Double font size should roughly double width
      assert!(width32 > width16 * 1.8);
      assert!(width32 < width16 * 2.2);
    }
  }

  #[test]
  fn test_measure_text_detailed_empty() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    if let Some(font) = ctx.get_sans_serif() {
      let measurement = ctx.measure_text_detailed("", &font, 16.0);
      assert_eq!(measurement.width, 0.0);
      assert_eq!(measurement.glyph_count, 0);
      assert!(!measurement.is_shaped);
    }
  }

  #[test]
  fn test_measure_text_detailed_shaped() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    if let Some(font) = ctx.get_sans_serif() {
      let measurement = ctx.measure_text_detailed("Hello", &font, 16.0);
      assert!(measurement.width > 0.0);
      assert!(measurement.glyph_count > 0);
      assert!(measurement.is_shaped);
    }
  }

  #[test]
  fn test_measure_text_detailed_glyph_count() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    if let Some(font) = ctx.get_sans_serif() {
      let measurement = ctx.measure_text_detailed("Hello", &font, 16.0);
      // For simple ASCII text, glyph count should equal char count
      assert_eq!(measurement.glyph_count, 5);
    }
  }

  #[test]
  fn test_get_scaled_metrics() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    if let Some(font) = ctx.get_sans_serif() {
      let metrics = ctx.get_scaled_metrics(&font, 16.0);
      assert!(metrics.is_some());

      let m = metrics.unwrap();
      assert_eq!(m.font_size, 16.0);
      assert!(m.ascent > 0.0);
      assert!(m.descent > 0.0);
      assert!(m.line_height > 0.0);
    }
  }

  #[test]
  fn test_get_scaled_metrics_different_sizes() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    if let Some(font) = ctx.get_sans_serif() {
      let m16 = ctx.get_scaled_metrics(&font, 16.0);
      let m32 = ctx.get_scaled_metrics(&font, 32.0);

      assert!(m16.is_some());
      assert!(m32.is_some());

      let m16 = m16.unwrap();
      let m32 = m32.unwrap();

      // Double size should roughly double metrics
      assert!(m32.ascent > m16.ascent * 1.8);
      assert!(m32.line_height > m16.line_height * 1.8);
    }
  }

  #[test]
  fn test_measure_text_unicode() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    if let Some(font) = ctx.get_sans_serif() {
      // Test with Unicode characters
      let width = ctx.measure_text("Héllo", &font, 16.0);
      assert!(width > 0.0);
    }
  }

  #[test]
  fn test_measure_text_spaces() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
      return;
    }

    if let Some(font) = ctx.get_sans_serif() {
      let width_no_space = ctx.measure_text("AB", &font, 16.0);
      let width_with_space = ctx.measure_text("A B", &font, 16.0);

      // Text with space should be wider
      assert!(width_with_space > width_no_space);
    }
  }
}
