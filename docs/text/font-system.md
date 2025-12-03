# Phase 3: Font System

**Duration:** Week 1 of Phase 3 (5-7 days)
**Prerequisites:**
- Phase 1 complete (type system, formatting contexts)
- Basic style system in place
**Dependencies:**
- fontdb crate for font loading and matching
- ttf-parser for font metrics extraction
- ComputedStyle with font properties
**Output:** Complete font loading, caching, and fallback system

## Objectives

Implement a comprehensive font management system that handles font loading, caching, matching, and fallback according to CSS specifications. This is the **foundation for all text rendering** and must be robust and performant.

The font system provides:
- Loading fonts from files, system font directories, and embedded fonts
- Font caching with metrics pre-computation
- Font matching based on family, weight, style, and stretch
- Font fallback chains (e.g., Arial → Helvetica → sans-serif)
- CSS @font-face support
- Font metrics (ascent, descent, line-height, x-height, cap-height)

## Context

The font system is critical infrastructure that sits between CSS style resolution and text rendering:

**Font Lifecycle:**
1. **CSS specifies fonts:** `font-family: "Roboto", Arial, sans-serif`
2. **Font matching:** Find best match from available fonts
3. **Font loading:** Load font data from file/memory
4. **Metrics extraction:** Parse font tables to get dimensions
5. **Caching:** Store font and metrics for reuse
6. **Shaping:** Use font for text shaping (next phase)

**From CSS Fonts Module Level 4:**
> "This specification describes how font properties are specified and how font resources are loaded dynamically."

**Key Properties:**
- `font-family` - Prioritized list of font families
- `font-weight` - 100-900, or keywords (normal=400, bold=700)
- `font-style` - normal, italic, oblique
- `font-stretch` - ultra-condensed to ultra-expanded
- `font-size` - Size in px, em, rem, etc.

## The Problem V1 Has

V1 has minimal font support:
- No real font loading (uses default/placeholder)
- No font matching algorithm
- No fallback chains
- No font metrics
- Cannot handle custom fonts
- Text measurement is guessed/hardcoded

This makes accurate text layout impossible.

## The Solution

Implement a complete font system using industry-standard libraries:

1. **fontdb** - Font database and matching (used by resvg, usvg)
2. **ttf-parser** - Fast, safe TTF/OTF parsing
3. **Font caching** - LRU cache with metrics pre-computation
4. **CSS-compliant matching** - Follow CSS font matching algorithm

## CSS Specification References

**Primary:**
- **CSS Fonts Module Level 4:** Font properties and matching
  - https://www.w3.org/TR/css-fonts-4/
- **CSS Fonts Module Level 3:** Core font specification
  - https://www.w3.org/TR/css-fonts-3/

**Key Sections:**
- **Section 5:** Font matching algorithm
- **Section 4.2:** Font family (generic families)
- **Section 4.3:** Font weight
- **Section 4.4:** Font stretch
- **Section 4.5:** Font style
- **@font-face rule:** Custom font loading

**Font Metrics:**
- OpenType specification for font tables (head, hhea, OS/2)
- https://learn.microsoft.com/en-us/typography/opentype/spec/

## Step-by-Step Implementation

### Step 1: Create Font Module Structure (Day 1 Morning)

```bash
mkdir -p /home/user/fastrender/src/text/font
touch /home/user/fastrender/src/text/font/mod.rs
touch /home/user/fastrender/src/text/font/database.rs
touch /home/user/fastrender/src/text/font/loader.rs
touch /home/user/fastrender/src/text/font/metrics.rs
touch /home/user/fastrender/src/text/font/cache.rs
```

**File: `Cargo.toml` (add dependencies)**

```toml
[dependencies]
# Font loading and database
fontdb = "0.16"
ttf-parser = "0.20"

# Caching
lru = "0.12"

# Font shaping (for later, but declare now)
rustybuzz = "0.11"
unicode-bidi = "0.3"
unicode-script = "0.5"
```

**File: `src/text/font/mod.rs`**

```rust
//! Font system
//!
//! Handles font loading, matching, fallback, and caching.
//!
//! CSS Specification: CSS Fonts Module Level 4
//! https://www.w3.org/TR/css-fonts-4/

pub mod database;
pub mod loader;
pub mod metrics;
pub mod cache;

pub use database::{FontDatabase, FontQuery};
pub use loader::FontLoader;
pub use metrics::{FontMetrics, FontMetricsCache};
pub use cache::FontCache;

use crate::error::Result;
use std::sync::Arc;

/// A font face
///
/// Represents a single font file (e.g., Roboto-Regular.ttf)
#[derive(Debug, Clone)]
pub struct FontFace {
    /// Font data (owned or mmap'd)
    pub data: Arc<Vec<u8>>,

    /// Font index in collection (for .ttc files)
    pub index: u32,

    /// Font ID in database
    pub id: FontId,

    /// Font family name
    pub family: String,

    /// Font weight (100-900)
    pub weight: u16,

    /// Font style
    pub style: FontStyle,

    /// Font stretch
    pub stretch: FontStretch,

    /// Cached metrics
    metrics: Option<Arc<FontMetrics>>,
}

/// Unique font identifier
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FontId(pub u32);

/// Font style
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FontStyle {
    Normal,
    Italic,
    Oblique(Option<f32>), // Optional angle
}

/// Font stretch
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum FontStretch {
    UltraCondensed = 50,
    ExtraCondensed = 62,
    Condensed = 75,
    SemiCondensed = 87,
    Normal = 100,
    SemiExpanded = 112,
    Expanded = 125,
    ExtraExpanded = 150,
    UltraExpanded = 200,
}

impl FontFace {
    /// Get font metrics
    pub fn metrics(&self) -> Result<Arc<FontMetrics>> {
        if let Some(ref metrics) = self.metrics {
            return Ok(Arc::clone(metrics));
        }

        // Parse metrics from font data
        let metrics = FontMetrics::from_font_data(&self.data, self.index)?;
        Ok(Arc::new(metrics))
    }

    /// Get ttf-parser font
    pub fn as_ttf_parser(&self) -> Result<ttf_parser::Face> {
        ttf_parser::Face::parse(&self.data, self.index)
            .map_err(|e| crate::error::Error::Font(format!("Failed to parse font: {:?}", e)))
    }
}

/// Generic font families
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum GenericFamily {
    Serif,
    SansSerif,
    Monospace,
    Cursive,
    Fantasy,
    SystemUi,
}

impl GenericFamily {
    /// Get fallback font families for this generic family
    pub fn fallback_families(&self) -> &'static [&'static str] {
        match self {
            GenericFamily::Serif => &[
                "Times New Roman",
                "Georgia",
                "DejaVu Serif",
                "Liberation Serif",
            ],
            GenericFamily::SansSerif => &[
                "Arial",
                "Helvetica",
                "DejaVu Sans",
                "Liberation Sans",
            ],
            GenericFamily::Monospace => &[
                "Courier New",
                "Courier",
                "DejaVu Sans Mono",
                "Liberation Mono",
            ],
            GenericFamily::Cursive => &[
                "Comic Sans MS",
                "Apple Chancery",
                "DejaVu Sans",
            ],
            GenericFamily::Fantasy => &[
                "Impact",
                "Papyrus",
                "DejaVu Sans",
            ],
            GenericFamily::SystemUi => &[
                "Segoe UI",      // Windows
                "San Francisco", // macOS
                "Roboto",        // Android
                "Ubuntu",        // Linux
                "Arial",         // Fallback
            ],
        }
    }
}
```

### Step 2: Implement Font Database (Day 1 Afternoon)

**File: `src/text/font/database.rs`**

```rust
//! Font database
//!
//! Manages font discovery and matching using fontdb.

use super::{FontFace, FontId, FontStyle, FontStretch, GenericFamily};
use crate::error::{Result, Error};
use fontdb::{Database as FontDbDatabase, Query as FontDbQuery, Family as FontDbFamily};
use std::sync::Arc;
use std::path::Path;

/// Font database
///
/// Manages all available fonts and performs font matching.
pub struct FontDatabase {
    /// Underlying fontdb database
    db: FontDbDatabase,

    /// Next font ID
    next_id: u32,
}

impl FontDatabase {
    /// Create a new font database
    pub fn new() -> Self {
        let mut db = FontDbDatabase::new();

        // Load system fonts
        db.load_system_fonts();

        Self {
            db,
            next_id: 0,
        }
    }

    /// Create a database without loading system fonts
    ///
    /// Useful for testing or custom font setups
    pub fn empty() -> Self {
        Self {
            db: FontDbDatabase::new(),
            next_id: 0,
        }
    }

    /// Load fonts from a directory
    pub fn load_fonts_dir<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        self.db.load_fonts_dir(path);
        Ok(())
    }

    /// Load a single font file
    pub fn load_font_file<P: AsRef<Path>>(&mut self, path: P) -> Result<FontId> {
        let path = path.as_ref();
        let data = std::fs::read(path)
            .map_err(|e| Error::Font(format!("Failed to read font file: {}", e)))?;

        self.load_font_data(data)
    }

    /// Load font from memory
    pub fn load_font_data(&mut self, data: Vec<u8>) -> Result<FontId> {
        // Parse to check validity
        let face = ttf_parser::Face::parse(&data, 0)
            .map_err(|e| Error::Font(format!("Invalid font data: {:?}", e)))?;

        // Get font info
        let family = face.names()
            .into_iter()
            .find(|n| n.name_id == ttf_parser::name_id::FAMILY)
            .and_then(|n| n.to_string())
            .ok_or_else(|| Error::Font("Font has no family name".into()))?;

        // Load into fontdb
        self.db.load_font_data(data);

        let id = FontId(self.next_id);
        self.next_id += 1;

        Ok(id)
    }

    /// Find the best matching font
    ///
    /// Implements CSS font matching algorithm (CSS Fonts Level 4, Section 5)
    pub fn query(&self, query: &FontQuery) -> Option<FontFace> {
        // Convert our query to fontdb query
        let fontdb_query = self.query_to_fontdb(query);

        // Use fontdb's matching algorithm
        let id = self.db.query(&fontdb_query)?;

        // Get font face info
        let face_info = self.db.face(id)?;

        // Load font data
        let data = self.db.face_data(id)?;

        Some(FontFace {
            data: Arc::new(data.to_vec()),
            index: face_info.index,
            id: FontId(id.0),
            family: query.family.clone(),
            weight: query.weight,
            style: query.style,
            stretch: query.stretch,
            metrics: None,
        })
    }

    /// Query with fallback
    ///
    /// Tries each family in the list until a match is found.
    /// This implements CSS font-family fallback.
    pub fn query_with_fallback(&self, queries: &[FontQuery]) -> Option<FontFace> {
        for query in queries {
            if let Some(face) = self.query(query) {
                return Some(face);
            }
        }
        None
    }

    /// Get all fonts for a family
    pub fn fonts_for_family(&self, family: &str) -> Vec<FontFace> {
        let mut faces = Vec::new();

        // Iterate through all faces in database
        for face in self.db.faces() {
            if face.families.iter().any(|(f, _)| f == family) {
                if let Some(data) = self.db.face_data(face.id) {
                    faces.push(FontFace {
                        data: Arc::new(data.to_vec()),
                        index: face.index,
                        id: FontId(face.id.0),
                        family: family.to_string(),
                        weight: face.weight.0,
                        style: self.fontdb_style_to_our(face.style),
                        stretch: self.fontdb_stretch_to_our(face.stretch),
                        metrics: None,
                    });
                }
            }
        }

        faces
    }

    /// Convert our query to fontdb query
    fn query_to_fontdb(&self, query: &FontQuery) -> FontDbQuery {
        FontDbQuery {
            families: &[
                if let Some(ref generic) = query.generic_family {
                    // Use generic family
                    match generic {
                        GenericFamily::Serif => FontDbFamily::Serif,
                        GenericFamily::SansSerif => FontDbFamily::SansSerif,
                        GenericFamily::Monospace => FontDbFamily::Monospace,
                        GenericFamily::Cursive => FontDbFamily::Cursive,
                        GenericFamily::Fantasy => FontDbFamily::Fantasy,
                        GenericFamily::SystemUi => FontDbFamily::SansSerif,
                    }
                } else {
                    // Use specific family name
                    FontDbFamily::Name(&query.family)
                }
            ],
            weight: fontdb::Weight(query.weight),
            stretch: self.our_stretch_to_fontdb(query.stretch),
            style: self.our_style_to_fontdb(query.style),
        }
    }

    // Type conversions
    fn our_style_to_fontdb(&self, style: FontStyle) -> fontdb::Style {
        match style {
            FontStyle::Normal => fontdb::Style::Normal,
            FontStyle::Italic => fontdb::Style::Italic,
            FontStyle::Oblique(_) => fontdb::Style::Oblique,
        }
    }

    fn fontdb_style_to_our(&self, style: fontdb::Style) -> FontStyle {
        match style {
            fontdb::Style::Normal => FontStyle::Normal,
            fontdb::Style::Italic => FontStyle::Italic,
            fontdb::Style::Oblique => FontStyle::Oblique(None),
        }
    }

    fn our_stretch_to_fontdb(&self, stretch: FontStretch) -> fontdb::Stretch {
        fontdb::Stretch::from_percentage((stretch as u16) as f32)
    }

    fn fontdb_stretch_to_our(&self, stretch: fontdb::Stretch) -> FontStretch {
        let pct = stretch.to_percentage() as u16;
        match pct {
            0..=56 => FontStretch::UltraCondensed,
            57..=68 => FontStretch::ExtraCondensed,
            69..=81 => FontStretch::Condensed,
            82..=93 => FontStretch::SemiCondensed,
            94..=106 => FontStretch::Normal,
            107..=118 => FontStretch::SemiExpanded,
            119..=137 => FontStretch::Expanded,
            138..=175 => FontStretch::ExtraExpanded,
            _ => FontStretch::UltraExpanded,
        }
    }
}

/// Font query
///
/// Specifies the desired font characteristics.
#[derive(Debug, Clone)]
pub struct FontQuery {
    /// Font family name (e.g., "Roboto", "Arial")
    pub family: String,

    /// Font weight (100-900, 400=normal, 700=bold)
    pub weight: u16,

    /// Font style
    pub style: FontStyle,

    /// Font stretch
    pub stretch: FontStretch,

    /// Generic family (if family is generic)
    pub generic_family: Option<GenericFamily>,
}

impl FontQuery {
    /// Create query from CSS properties
    pub fn from_css(
        family: &str,
        weight: u16,
        style: FontStyle,
        stretch: FontStretch,
    ) -> Self {
        // Check if family is generic
        let generic_family = match family.to_lowercase().as_str() {
            "serif" => Some(GenericFamily::Serif),
            "sans-serif" => Some(GenericFamily::SansSerif),
            "monospace" => Some(GenericFamily::Monospace),
            "cursive" => Some(GenericFamily::Cursive),
            "fantasy" => Some(GenericFamily::Fantasy),
            "system-ui" => Some(GenericFamily::SystemUi),
            _ => None,
        };

        Self {
            family: family.to_string(),
            weight,
            style,
            stretch,
            generic_family,
        }
    }

    /// Create queries from font-family list
    ///
    /// CSS allows: font-family: "Roboto", Arial, sans-serif;
    /// This creates a query for each family in the list.
    pub fn from_family_list(
        families: &[String],
        weight: u16,
        style: FontStyle,
        stretch: FontStretch,
    ) -> Vec<Self> {
        families.iter()
            .map(|family| Self::from_css(family, weight, style, stretch))
            .collect()
    }
}

impl Default for FontDatabase {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 3: Implement Font Metrics (Day 2)

**File: `src/text/font/metrics.rs`**

```rust
//! Font metrics
//!
//! Extracts and caches font metrics from font files.

use crate::error::{Result, Error};
use ttf_parser::Face;

/// Font metrics
///
/// Contains all dimensional information about a font.
/// All values are in font units (must be scaled by font size).
#[derive(Debug, Clone)]
pub struct FontMetrics {
    /// Units per em (typically 1000 or 2048)
    pub units_per_em: u16,

    /// Ascent (distance from baseline to top)
    /// This is the maximum height above the baseline
    pub ascent: i16,

    /// Descent (distance from baseline to bottom, negative)
    /// This is the maximum depth below the baseline
    pub descent: i16,

    /// Line gap (additional spacing between lines)
    pub line_gap: i16,

    /// Recommended line height
    /// Calculated as: ascent - descent + line_gap
    pub line_height: i16,

    /// x-height (height of lowercase 'x')
    /// Used for relative units like 'ex'
    pub x_height: Option<i16>,

    /// Cap height (height of uppercase letters)
    /// Used for relative units like 'cap'
    pub cap_height: Option<i16>,

    /// Underline position
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
    /// Extract metrics from font data
    pub fn from_font_data(data: &[u8], index: u32) -> Result<Self> {
        let face = Face::parse(data, index)
            .map_err(|e| Error::Font(format!("Failed to parse font: {:?}", e)))?;

        Self::from_face(&face)
    }

    /// Extract metrics from ttf-parser Face
    pub fn from_face(face: &Face) -> Result<Self> {
        let units_per_em = face.units_per_em();

        // Get metrics from hhea table (horizontal header)
        let ascent = face.ascender();
        let descent = face.descender();
        let line_gap = face.line_gap();

        // Calculate line height
        // CSS spec: line-height = ascent - descent + line-gap
        // Note: descent is negative, so we subtract it (making it positive)
        let line_height = ascent - descent + line_gap;

        // Get x-height and cap-height from OS/2 table
        let (x_height, cap_height) = if let Some(os2) = face.tables().os2 {
            (
                os2.x_height(),
                os2.capital_height(),
            )
        } else {
            (None, None)
        };

        // Underline metrics
        let underline_position = face.underline_position().unwrap_or(-(units_per_em as i16) / 10);
        let underline_thickness = face.underline_thickness().unwrap_or((units_per_em as i16) / 20);

        // Strikeout metrics from OS/2
        let (strikeout_position, strikeout_thickness) = if let Some(os2) = face.tables().os2 {
            (
                os2.strikeout_position(),
                os2.strikeout_thickness(),
            )
        } else {
            (None, None)
        };

        // Font style flags
        let is_bold = face.is_bold();
        let is_italic = face.is_italic();
        let is_monospace = face.is_monospaced();

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
            is_bold,
            is_italic,
            is_monospace,
        })
    }

    /// Scale metrics to pixel size
    ///
    /// Font metrics are in font units. This converts them to pixels
    /// for a given font size.
    pub fn scale(&self, font_size: f32) -> ScaledMetrics {
        let scale = font_size / (self.units_per_em as f32);

        ScaledMetrics {
            font_size,
            scale,
            ascent: (self.ascent as f32) * scale,
            descent: -(self.descent as f32) * scale, // Make descent positive
            line_gap: (self.line_gap as f32) * scale,
            line_height: (self.line_height as f32) * scale,
            x_height: self.x_height.map(|h| (h as f32) * scale),
            cap_height: self.cap_height.map(|h| (h as f32) * scale),
            underline_position: (self.underline_position as f32) * scale,
            underline_thickness: (self.underline_thickness as f32) * scale,
        }
    }

    /// Get normal line height
    ///
    /// CSS 'line-height: normal' is typically 1.2 times the font size,
    /// but can be calculated from font metrics.
    pub fn normal_line_height(&self, font_size: f32) -> f32 {
        let scale = font_size / (self.units_per_em as f32);
        (self.line_height as f32) * scale
    }
}

/// Scaled font metrics (in pixels)
#[derive(Debug, Clone)]
pub struct ScaledMetrics {
    /// Font size in pixels
    pub font_size: f32,

    /// Scale factor (font_size / units_per_em)
    pub scale: f32,

    /// Ascent in pixels
    pub ascent: f32,

    /// Descent in pixels (positive value)
    pub descent: f32,

    /// Line gap in pixels
    pub line_gap: f32,

    /// Line height in pixels
    pub line_height: f32,

    /// x-height in pixels
    pub x_height: Option<f32>,

    /// Cap height in pixels
    pub cap_height: Option<f32>,

    /// Underline position
    pub underline_position: f32,

    /// Underline thickness
    pub underline_thickness: f32,
}

impl ScaledMetrics {
    /// Get baseline offset from top of line box
    ///
    /// Used in inline layout for baseline alignment.
    pub fn baseline_offset(&self) -> f32 {
        self.ascent
    }

    /// Get total height (ascent + descent)
    pub fn total_height(&self) -> f32 {
        self.ascent + self.descent
    }

    /// Apply line-height factor
    ///
    /// CSS line-height can be a multiplier (e.g., 1.5).
    /// This adjusts the line height accordingly.
    pub fn with_line_height_factor(&self, factor: f32) -> Self {
        let new_line_height = self.font_size * factor;
        let extra_leading = new_line_height - self.total_height();
        let half_leading = extra_leading / 2.0;

        Self {
            line_height: new_line_height,
            ..*self
        }
    }
}

/// Font metrics cache
///
/// Caches computed metrics to avoid repeated parsing.
use lru::LruCache;
use std::sync::Mutex;
use std::num::NonZeroUsize;

pub struct FontMetricsCache {
    cache: Mutex<LruCache<MetricsCacheKey, FontMetrics>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MetricsCacheKey {
    font_data_hash: u64,
    index: u32,
}

impl FontMetricsCache {
    pub fn new(capacity: usize) -> Self {
        Self {
            cache: Mutex::new(LruCache::new(
                NonZeroUsize::new(capacity).unwrap()
            )),
        }
    }

    pub fn get_or_compute(
        &self,
        data: &[u8],
        index: u32,
    ) -> Result<FontMetrics> {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        // Hash font data for cache key
        let mut hasher = DefaultHasher::new();
        data.hash(&mut hasher);
        let hash = hasher.finish();

        let key = MetricsCacheKey {
            font_data_hash: hash,
            index,
        };

        // Check cache
        {
            let mut cache = self.cache.lock().unwrap();
            if let Some(metrics) = cache.get(&key) {
                return Ok(metrics.clone());
            }
        }

        // Not in cache - compute
        let metrics = FontMetrics::from_font_data(data, index)?;

        // Store in cache
        {
            let mut cache = self.cache.lock().unwrap();
            cache.put(key, metrics.clone());
        }

        Ok(metrics)
    }
}

impl Default for FontMetricsCache {
    fn default() -> Self {
        Self::new(100)
    }
}
```

### Step 4: Implement Font Cache (Day 3)

**File: `src/text/font/cache.rs`**

```rust
//! Font cache
//!
//! Caches loaded fonts to avoid repeated loading and parsing.

use super::{FontFace, FontQuery};
use lru::LruCache;
use std::sync::Mutex;
use std::num::NonZeroUsize;

/// Font cache
///
/// LRU cache for loaded fonts.
pub struct FontCache {
    cache: Mutex<LruCache<FontCacheKey, FontFace>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FontCacheKey {
    family: String,
    weight: u16,
    style: String,
    stretch: u16,
}

impl From<&FontQuery> for FontCacheKey {
    fn from(query: &FontQuery) -> Self {
        Self {
            family: query.family.clone(),
            weight: query.weight,
            style: format!("{:?}", query.style),
            stretch: query.stretch as u16,
        }
    }
}

impl FontCache {
    /// Create new font cache
    pub fn new(capacity: usize) -> Self {
        Self {
            cache: Mutex::new(
                LruCache::new(NonZeroUsize::new(capacity).unwrap())
            ),
        }
    }

    /// Get font from cache
    pub fn get(&self, query: &FontQuery) -> Option<FontFace> {
        let key = FontCacheKey::from(query);
        let mut cache = self.cache.lock().unwrap();
        cache.get(&key).cloned()
    }

    /// Put font in cache
    pub fn put(&self, query: &FontQuery, font: FontFace) {
        let key = FontCacheKey::from(query);
        let mut cache = self.cache.lock().unwrap();
        cache.put(key, font);
    }

    /// Clear cache
    pub fn clear(&self) {
        let mut cache = self.cache.lock().unwrap();
        cache.clear();
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let cache = self.cache.lock().unwrap();
        CacheStats {
            size: cache.len(),
            capacity: cache.cap().get(),
        }
    }
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    pub size: usize,
    pub capacity: usize,
}

impl Default for FontCache {
    fn default() -> Self {
        Self::new(50)
    }
}
```

### Step 5: Implement Font Loader (Day 3)

**File: `src/text/font/loader.rs`**

```rust
//! Font loader
//!
//! High-level API for font loading with caching and fallback.

use super::{FontDatabase, FontQuery, FontFace, FontCache, GenericFamily};
use crate::error::Result;
use std::sync::Arc;

/// Font loader
///
/// Main entry point for font access.
/// Combines database, cache, and fallback logic.
pub struct FontLoader {
    database: Arc<FontDatabase>,
    cache: FontCache,
}

impl FontLoader {
    /// Create new font loader
    pub fn new() -> Self {
        Self {
            database: Arc::new(FontDatabase::new()),
            cache: FontCache::new(50),
        }
    }

    /// Create with custom database
    pub fn with_database(database: Arc<FontDatabase>) -> Self {
        Self {
            database,
            cache: FontCache::new(50),
        }
    }

    /// Load a font matching the query
    ///
    /// Returns cached font if available, otherwise queries database.
    pub fn load(&self, query: &FontQuery) -> Option<FontFace> {
        // Check cache first
        if let Some(font) = self.cache.get(query) {
            return Some(font);
        }

        // Query database
        if let Some(font) = self.database.query(query) {
            // Cache and return
            self.cache.put(query, font.clone());
            return Some(font);
        }

        None
    }

    /// Load with fallback chain
    ///
    /// Tries each query in order until a match is found.
    pub fn load_with_fallback(&self, queries: &[FontQuery]) -> Option<FontFace> {
        for query in queries {
            if let Some(font) = self.load(query) {
                return Some(font);
            }
        }
        None
    }

    /// Load from CSS font-family list
    ///
    /// Handles CSS font-family like: "Roboto", Arial, sans-serif
    pub fn load_from_family_list(
        &self,
        families: &[String],
        weight: u16,
        style: super::FontStyle,
        stretch: super::FontStretch,
    ) -> Option<FontFace> {
        // Build queries for each family
        let queries = FontQuery::from_family_list(families, weight, style, stretch);

        // Try each in order
        self.load_with_fallback(&queries)
    }

    /// Get the font database
    pub fn database(&self) -> &Arc<FontDatabase> {
        &self.database
    }

    /// Get mutable access to database
    pub fn database_mut(&mut self) -> &mut FontDatabase {
        Arc::make_mut(&mut self.database)
    }

    /// Clear font cache
    pub fn clear_cache(&self) {
        self.cache.clear();
    }
}

impl Default for FontLoader {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 6: Integration with Style System (Day 4)

**File: `src/text/mod.rs`**

```rust
//! Text system
//!
//! Handles fonts, shaping, and text measurement.

pub mod font;

pub use font::{
    FontDatabase,
    FontLoader,
    FontFace,
    FontQuery,
    FontMetrics,
    FontStyle,
    FontStretch,
    GenericFamily,
};

use crate::style::ComputedStyle;
use crate::error::Result;
use std::sync::Arc;

/// Font context
///
/// High-level interface for font access during layout.
pub struct FontContext {
    loader: Arc<FontLoader>,
}

impl FontContext {
    /// Create new font context
    pub fn new() -> Self {
        Self {
            loader: Arc::new(FontLoader::new()),
        }
    }

    /// Create with custom loader
    pub fn with_loader(loader: Arc<FontLoader>) -> Self {
        Self { loader }
    }

    /// Get font for style
    ///
    /// This is the main method called during layout.
    pub fn font_for_style(&self, style: &ComputedStyle) -> Option<FontFace> {
        // Extract font properties from style
        let families = &style.font_family;
        let weight = style.font_weight.to_number();
        let font_style = style.font_style;
        let stretch = style.font_stretch;

        self.loader.load_from_family_list(
            families,
            weight,
            font_style,
            stretch,
        )
    }

    /// Get font loader
    pub fn loader(&self) -> &Arc<FontLoader> {
        &self.loader
    }
}

impl Default for FontContext {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 7: Comprehensive Tests (Day 5-6)

**File: `tests/text/font_test.rs`**

```rust
//! Tests for font system

use fastrender::text::font::*;

#[test]
fn test_font_database_creation() {
    let db = FontDatabase::new();
    // Should load system fonts
    // Exact count varies by system, but should have some
}

#[test]
fn test_font_query_basic() {
    let db = FontDatabase::new();

    let query = FontQuery::from_css(
        "Arial",
        400,
        FontStyle::Normal,
        FontStretch::Normal,
    );

    let font = db.query(&query);
    assert!(font.is_some(), "Should find Arial");

    let font = font.unwrap();
    assert_eq!(font.family, "Arial");
}

#[test]
fn test_font_weight_matching() {
    let db = FontDatabase::new();

    // Query for bold
    let query = FontQuery::from_css(
        "Arial",
        700,
        FontStyle::Normal,
        FontStretch::Normal,
    );

    let font = db.query(&query);
    assert!(font.is_some());

    // Weight should be 700 or close
    let font = font.unwrap();
    assert!(font.weight >= 600 && font.weight <= 800);
}

#[test]
fn test_font_style_matching() {
    let db = FontDatabase::new();

    let query = FontQuery::from_css(
        "Arial",
        400,
        FontStyle::Italic,
        FontStretch::Normal,
    );

    let font = db.query(&query);
    assert!(font.is_some());

    let font = font.unwrap();
    assert_eq!(font.style, FontStyle::Italic);
}

#[test]
fn test_generic_family_serif() {
    let db = FontDatabase::new();

    let query = FontQuery::from_css(
        "serif",
        400,
        FontStyle::Normal,
        FontStretch::Normal,
    );

    let font = db.query(&query);
    assert!(font.is_some(), "Should find serif font");
}

#[test]
fn test_generic_family_sans_serif() {
    let db = FontDatabase::new();

    let query = FontQuery::from_css(
        "sans-serif",
        400,
        FontStyle::Normal,
        FontStretch::Normal,
    );

    let font = db.query(&query);
    assert!(font.is_some(), "Should find sans-serif font");
}

#[test]
fn test_generic_family_monospace() {
    let db = FontDatabase::new();

    let query = FontQuery::from_css(
        "monospace",
        400,
        FontStyle::Normal,
        FontStretch::Normal,
    );

    let font = db.query(&query);
    assert!(font.is_some(), "Should find monospace font");
}

#[test]
fn test_fallback_chain() {
    let db = FontDatabase::new();

    let families = vec![
        "NonExistentFont".to_string(),
        "Arial".to_string(),
        "sans-serif".to_string(),
    ];

    let queries = FontQuery::from_family_list(
        &families,
        400,
        FontStyle::Normal,
        FontStretch::Normal,
    );

    let font = db.query_with_fallback(&queries);
    assert!(font.is_some(), "Should find Arial as fallback");

    let font = font.unwrap();
    assert_eq!(font.family, "Arial");
}

#[test]
fn test_font_metrics_extraction() {
    let db = FontDatabase::new();

    let query = FontQuery::from_css(
        "Arial",
        400,
        FontStyle::Normal,
        FontStretch::Normal,
    );

    let font = db.query(&query).expect("Should find Arial");
    let metrics = font.metrics().expect("Should extract metrics");

    // Check basic metrics are present
    assert!(metrics.units_per_em > 0);
    assert!(metrics.ascent > 0);
    assert!(metrics.descent < 0); // Descent is negative
    assert!(metrics.line_height > 0);
}

#[test]
fn test_metrics_scaling() {
    let db = FontDatabase::new();

    let query = FontQuery::from_css(
        "Arial",
        400,
        FontStyle::Normal,
        FontStretch::Normal,
    );

    let font = db.query(&query).expect("Should find Arial");
    let metrics = font.metrics().expect("Should extract metrics");

    // Scale to 16px
    let scaled = metrics.scale(16.0);

    assert_eq!(scaled.font_size, 16.0);
    assert!(scaled.ascent > 0.0);
    assert!(scaled.descent > 0.0); // Scaled descent is positive
    assert!(scaled.line_height > 0.0);

    // Total height should be reasonable for 16px font
    assert!(scaled.total_height() >= 12.0 && scaled.total_height() <= 24.0);
}

#[test]
fn test_font_cache() {
    let cache = FontCache::new(10);

    let query = FontQuery::from_css(
        "Arial",
        400,
        FontStyle::Normal,
        FontStretch::Normal,
    );

    // Initially empty
    assert!(cache.get(&query).is_none());

    // Create dummy font
    let font = create_dummy_font("Arial", 400);

    // Put in cache
    cache.put(&query, font.clone());

    // Should retrieve
    let cached = cache.get(&query);
    assert!(cached.is_some());
}

#[test]
fn test_font_loader() {
    let loader = FontLoader::new();

    let query = FontQuery::from_css(
        "Arial",
        400,
        FontStyle::Normal,
        FontStretch::Normal,
    );

    let font1 = loader.load(&query);
    assert!(font1.is_some());

    // Second load should come from cache
    let font2 = loader.load(&query);
    assert!(font2.is_some());

    // Should be same font
    assert_eq!(font1.unwrap().id, font2.unwrap().id);
}

#[test]
fn test_font_context() {
    use fastrender::style::ComputedStyle;

    let ctx = FontContext::new();

    let style = ComputedStyle {
        font_family: vec!["Arial".to_string(), "sans-serif".to_string()],
        font_weight: crate::style::FontWeight::Normal,
        font_style: FontStyle::Normal,
        font_stretch: FontStretch::Normal,
        ..Default::default()
    };

    let font = ctx.font_for_style(&style);
    assert!(font.is_some(), "Should find font for style");
}

// Helper function
fn create_dummy_font(family: &str, weight: u16) -> FontFace {
    use std::sync::Arc;

    FontFace {
        data: Arc::new(vec![]),
        index: 0,
        id: FontId(0),
        family: family.to_string(),
        weight,
        style: FontStyle::Normal,
        stretch: FontStretch::Normal,
        metrics: None,
    }
}
```

## Acceptance Criteria

- [ ] Font database loads system fonts successfully
- [ ] Font query finds fonts by family name
- [ ] Font weight matching works (100-900)
- [ ] Font style matching works (normal, italic, oblique)
- [ ] Font stretch matching works
- [ ] Generic families resolve to real fonts (serif, sans-serif, monospace, etc.)
- [ ] Font fallback chains work (tries each family in order)
- [ ] Font metrics extraction works (ascent, descent, line-height, etc.)
- [ ] Metrics scaling to pixel size works correctly
- [ ] Font cache stores and retrieves fonts
- [ ] FontContext integrates with ComputedStyle
- [ ] Can load custom fonts from files
- [ ] All tests pass: `cargo test font`
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Not Handling Missing Fonts

**Wrong:**
```rust
let font = db.query(&query).unwrap(); // Panics if font not found!
```

**Right:**
```rust
let font = db.query(&query)
    .or_else(|| {
        // Try fallback
        let fallback_query = FontQuery::from_css("sans-serif", 400, ...);
        db.query(&fallback_query)
    })
    .expect("Should always find a fallback font");
```

### Pitfall 2: Forgetting to Scale Metrics

**Wrong:**
```rust
// Using font units directly in layout
let line_height = font.metrics().line_height; // Wrong scale!
```

**Right:**
```rust
// Always scale metrics to pixels
let metrics = font.metrics().scale(font_size);
let line_height = metrics.line_height; // Now in pixels
```

### Pitfall 3: Not Caching Fonts

**Wrong:**
```rust
// Re-loading font on every access
fn get_font(&self, family: &str) -> FontFace {
    self.database.query(...).unwrap() // Expensive!
}
```

**Right:**
```rust
// Use FontLoader which caches
fn get_font(&self, query: &FontQuery) -> FontFace {
    self.loader.load(query) // Cached!
}
```

### Pitfall 4: Incorrect Descent Handling

**Wrong:**
```rust
// Descent is negative in font units!
let total_height = ascent + descent; // Wrong!
```

**Right:**
```rust
// Subtract descent (which is negative)
let total_height = ascent - descent; // Correct!

// Or use ScaledMetrics which makes descent positive
let scaled = metrics.scale(font_size);
let total_height = scaled.ascent + scaled.descent; // Also correct
```

## Performance Considerations

1. **Cache fonts aggressively** - Font loading is expensive
2. **Cache metrics** - Parsing font tables is expensive
3. **Use fontdb's query** - It's optimized for font matching
4. **Lazy load fonts** - Don't load all fonts upfront
5. **Consider memory limits** - LRU cache prevents unbounded growth

## Integration Points

The font system integrates with:

1. **Style system** - Reads font properties from ComputedStyle
2. **Text shaping** - Provides fonts to rustybuzz
3. **Inline layout** - Provides metrics for baseline alignment
4. **Text rendering** - Provides fonts for glyph rendering

## Next Steps

After font system is complete:
- **03-text-shaping.md** - Text shaping with HarfBuzz/rustybuzz
- **03-line-breaking.md** - Line breaking algorithm

## References

- **CSS Fonts Module Level 4:** https://www.w3.org/TR/css-fonts-4/
- **CSS Fonts Module Level 3:** https://www.w3.org/TR/css-fonts-3/
- **OpenType Specification:** https://learn.microsoft.com/en-us/typography/opentype/spec/
- **fontdb documentation:** https://docs.rs/fontdb/
- **ttf-parser documentation:** https://docs.rs/ttf-parser/
- **Font Metrics Explained:** https://iamvdo.me/en/blog/css-font-metrics-line-height-and-vertical-align

---

**Last Updated:** 2025-11-19
**Status:** Ready for Implementation
