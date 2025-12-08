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

use crate::error::{FontError, Result};
use fontdb::{Database as FontDbDatabase, Family as FontDbFamily, Query as FontDbQuery, ID};
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, RwLock};

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
    /// Thin (100)
    pub const THIN: Self = Self(100);
    /// Extra Light (200)
    pub const EXTRA_LIGHT: Self = Self(200);
    /// Light (300)
    pub const LIGHT: Self = Self(300);
    /// Normal/Regular (400) - CSS `font-weight: normal`
    pub const NORMAL: Self = Self(400);
    /// Medium (500)
    pub const MEDIUM: Self = Self(500);
    /// Semi Bold (600)
    pub const SEMI_BOLD: Self = Self(600);
    /// Bold (700) - CSS `font-weight: bold`
    pub const BOLD: Self = Self(700);
    /// Extra Bold (800)
    pub const EXTRA_BOLD: Self = Self(800);
    /// Black (900)
    pub const BLACK: Self = Self(900);

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

    /// Get ttf-parser Face for advanced operations
    ///
    /// Returns a parsed font face for accessing glyph data, kerning, etc.
    pub fn as_ttf_face(&self) -> Result<ttf_parser::Face<'_>> {
        ttf_parser::Face::parse(&self.data, self.index).map_err(|e| {
            FontError::LoadFailed {
                family: self.family.clone(),
                reason: format!("Failed to parse font: {:?}", e),
            }
            .into()
        })
    }
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

    /// Converts to fontdb Family for querying.
    pub fn to_fontdb(self) -> FontDbFamily<'static> {
        match self {
            Self::Serif | Self::UiSerif => FontDbFamily::Serif,
            Self::SansSerif | Self::SystemUi | Self::UiSansSerif | Self::UiRounded => FontDbFamily::SansSerif,
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
/// System fonts are loaded automatically on creation.
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
    db: FontDbDatabase,
    /// Cached font data (font ID -> binary data)
    cache: RwLock<HashMap<ID, Arc<Vec<u8>>>>,
}

impl FontDatabase {
    /// Creates a new font database and loads system fonts
    ///
    /// This will scan the system font directories:
    /// - Windows: C:\Windows\Fonts
    /// - macOS: /Library/Fonts, /System/Library/Fonts, ~/Library/Fonts
    /// - Linux: /usr/share/fonts, ~/.fonts, ~/.local/share/fonts
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let db = FontDatabase::new();
    /// ```
    pub fn new() -> Self {
        let mut db = FontDbDatabase::new();
        db.load_system_fonts();

        Self {
            db,
            cache: RwLock::new(HashMap::new()),
        }
    }

    /// Creates an empty font database without loading system fonts
    ///
    /// Useful for testing or when you want to load specific fonts only.
    pub fn empty() -> Self {
        Self {
            db: FontDbDatabase::new(),
            cache: RwLock::new(HashMap::new()),
        }
    }

    /// Loads fonts from a directory
    ///
    /// Recursively scans the directory for font files.
    pub fn load_fonts_dir<P: AsRef<Path>>(&mut self, path: P) {
        self.db.load_fonts_dir(path);
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
        ttf_parser::Face::parse(&data, 0).map_err(|e| FontError::InvalidFontFile {
            path: format!("(memory): {:?}", e),
        })?;

        self.db.load_font_data(data);
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
    pub fn query_full(&self, family: &str, weight: FontWeight, style: FontStyle, stretch: FontStretch) -> Option<ID> {
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
        // Check cache first
        {
            let cache = self.cache.read().ok()?;
            if let Some(data) = cache.get(&id) {
                return Some(self.create_loaded_font(id, Arc::clone(data)));
            }
        }

        // Load from fontdb
        let face_info = self.db.face(id)?;

        // fontdb stores font data internally, we need to access it via with_face_data
        let mut data_result: Option<Arc<Vec<u8>>> = None;
        self.db.with_face_data(id, |font_data, _face_index| {
            data_result = Some(Arc::new(font_data.to_vec()));
        });

        let data = data_result?;

        // Cache it
        {
            if let Ok(mut cache) = self.cache.write() {
                cache.insert(id, Arc::clone(&data));
            }
        }

        Some(self.create_loaded_font_with_info(id, data, &face_info))
    }

    /// Creates a LoadedFont from cached data
    fn create_loaded_font(&self, id: ID, data: Arc<Vec<u8>>) -> LoadedFont {
        let face_info = self.db.face(id).expect("Font should exist");
        self.create_loaded_font_with_info(id, data, &face_info)
    }

    /// Creates a LoadedFont from face info
    fn create_loaded_font_with_info(&self, _id: ID, data: Arc<Vec<u8>>, face_info: &fontdb::FaceInfo) -> LoadedFont {
        LoadedFont {
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
    pub fn resolve_family_list(&self, families: &[String], weight: FontWeight, style: FontStyle) -> Option<ID> {
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

    /// Loads the first available font in the database, if any
    pub fn first_font(&self) -> Option<LoadedFont> {
        self.faces().next().and_then(|face| self.load_font(face.id))
    }

    /// Checks if a font has a glyph for the given character.
    ///
    /// This is used during fallback resolution to find a font that
    /// can render a specific character.
    pub fn has_glyph(&self, id: ID, c: char) -> bool {
        self.db
            .with_face_data(id, |data, face_index| {
                if let Ok(face) = ttf_parser::Face::parse(data, face_index) {
                    face.glyph_index(c).is_some()
                } else {
                    false
                }
            })
            .unwrap_or(false)
    }

    /// Checks if a character is an emoji.
    ///
    /// Uses Unicode properties to determine if a character should be
    /// rendered with an emoji font.
    pub fn is_emoji(c: char) -> bool {
        let cp = c as u32;

        // Emoticons (U+1F600 - U+1F64F)
        if (0x1F600..=0x1F64F).contains(&cp) {
            return true;
        }

        // Miscellaneous Symbols and Pictographs (U+1F300 - U+1F5FF)
        if (0x1F300..=0x1F5FF).contains(&cp) {
            return true;
        }

        // Transport and Map Symbols (U+1F680 - U+1F6FF)
        if (0x1F680..=0x1F6FF).contains(&cp) {
            return true;
        }

        // Supplemental Symbols and Pictographs (U+1F900 - U+1F9FF)
        if (0x1F900..=0x1F9FF).contains(&cp) {
            return true;
        }

        // Symbols and Pictographs Extended-A (U+1FA00 - U+1FA6F)
        if (0x1FA00..=0x1FA6F).contains(&cp) {
            return true;
        }

        // Symbols and Pictographs Extended-B (U+1FA70 - U+1FAFF)
        if (0x1FA70..=0x1FAFF).contains(&cp) {
            return true;
        }

        // Dingbats (U+2700 - U+27BF)
        if (0x2700..=0x27BF).contains(&cp) {
            return true;
        }

        // Regional Indicator Symbols (U+1F1E0 - U+1F1FF) - flags
        if (0x1F1E0..=0x1F1FF).contains(&cp) {
            return true;
        }

        // Variation Selector-16 (emoji presentation)
        if cp == 0xFE0F {
            return true;
        }

        // Zero Width Joiner (used in emoji sequences)
        if cp == 0x200D {
            return true;
        }

        false
    }

    /// Finds emoji fonts in the database.
    ///
    /// Returns font IDs for fonts that are likely to contain emoji glyphs.
    pub fn find_emoji_fonts(&self) -> Vec<ID> {
        let mut emoji_fonts = Vec::new();

        for face in self.db.faces() {
            let is_emoji_font = face.families.iter().any(|(name, _)| {
                let name_lower = name.to_lowercase();
                name_lower.contains("emoji")
                    || name_lower.contains("symbola")
                    || name_lower.contains("noto color")
                    || name_lower.contains("apple color")
                    || name_lower.contains("segoe ui emoji")
                    || name_lower.contains("segoe ui symbol")
            });

            if is_emoji_font {
                emoji_fonts.push(face.id);
            }
        }

        emoji_fonts
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

    /// Extract metrics from font data
    pub fn from_data(data: &[u8], index: u32) -> Result<Self> {
        let face = ttf_parser::Face::parse(data, index).map_err(|e| FontError::LoadFailed {
            family: String::new(),
            reason: format!("Failed to parse font: {:?}", e),
        })?;

        Self::from_face(&face)
    }

    /// Extract metrics from ttf-parser Face
    pub fn from_face(face: &ttf_parser::Face) -> Result<Self> {
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

        Ok(Self {
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
        Self { line_height, ..*self }
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
        assert_eq!(GenericFamily::parse("sans-serif"), Some(GenericFamily::SansSerif));
        assert_eq!(GenericFamily::parse("MONOSPACE"), Some(GenericFamily::Monospace));
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
}
