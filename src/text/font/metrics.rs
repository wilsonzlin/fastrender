//! Font metrics extraction
//!
//! Extracts font metrics from font files using ttf-parser.
//! All metrics are in font units and must be scaled by font size.
//!
//! # CSS Specification References
//!
//! - CSS Fonts Module Level 4: https://www.w3.org/TR/css-fonts-4/
//! - OpenType Specification: https://learn.microsoft.com/en-us/typography/opentype/spec/
//!
//! # Font Tables Used
//!
//! - **head**: Font header (units per em)
//! - **hhea**: Horizontal header (ascent, descent, line gap)
//! - **OS/2**: Windows metrics (x-height, cap-height, strikeout)
//! - **post**: PostScript metrics (underline position, thickness)
//!
//! # Examples
//!
//! ```rust,ignore
//! use fastrender::text::font::metrics::FontMetrics;
//!
//! let font_data = std::fs::read("path/to/font.ttf")?;
//! let metrics = FontMetrics::from_font_data(&font_data, 0)?;
//!
//! // Scale to 16px font size
//! let scaled = metrics.scale(16.0);
//! println!("Line height: {}px", scaled.line_height);
//! ```

use crate::error::{FontError, Result};

/// Font metrics in font design units
///
/// Contains all dimensional information extracted from a font file.
/// All values are in font design units (typically 1000 or 2048 units per em)
/// and must be scaled to pixel size using the `scale()` method.
///
/// # Design Units vs Pixels
///
/// Font metrics are specified in "font design units" (also called "font units"
/// or "em units"). These are abstract units used in the font's coordinate
/// system. To convert to pixels:
///
/// ```text
/// pixels = font_units * (font_size_px / units_per_em)
/// ```
///
/// # Vertical Metrics
///
/// The primary vertical metrics are:
/// - **Ascent**: Maximum height above baseline (positive)
/// - **Descent**: Maximum depth below baseline (negative in font units)
/// - **Line gap**: Extra spacing between lines
/// - **Line height**: Calculated as ascent - descent + line_gap
///
/// ```text
///     ┌─────────────────────────┐  ← Top of em square
///     │         Ascent          │
///     ├─────────────────────────┤  ← Baseline
///     │         Descent         │
///     └─────────────────────────┘  ← Bottom of em square
///     │        Line Gap         │
///     ├─────────────────────────┤  ← Top of next line
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct FontMetrics {
    /// Units per em (typically 1000 or 2048)
    ///
    /// This is the font's design resolution. All other metrics
    /// are in these units.
    pub units_per_em: u16,

    /// Ascent in font units (positive)
    ///
    /// The maximum height of glyphs above the baseline.
    /// From the hhea table's 'ascender' field.
    pub ascent: i16,

    /// Descent in font units (negative)
    ///
    /// The maximum depth of glyphs below the baseline.
    /// From the hhea table's 'descender' field.
    /// Note: This value is typically negative in font units.
    pub descent: i16,

    /// Line gap in font units
    ///
    /// Additional spacing between lines beyond ascent and descent.
    /// From the hhea table's 'lineGap' field.
    pub line_gap: i16,

    /// x-height in font units
    ///
    /// Height of lowercase 'x'. Used for CSS 'ex' unit.
    /// From OS/2 table's 'sxHeight' field.
    /// None if not available in font.
    pub x_height: Option<i16>,

    /// Cap height in font units
    ///
    /// Height of uppercase letters. Used for CSS 'cap' unit.
    /// From OS/2 table's 'sCapHeight' field.
    /// None if not available in font.
    pub cap_height: Option<i16>,

    /// Underline position in font units (negative = below baseline)
    ///
    /// Distance from baseline to top of underline.
    /// From post table.
    pub underline_position: i16,

    /// Underline thickness in font units
    ///
    /// Thickness of underline stroke.
    /// From post table.
    pub underline_thickness: i16,

    /// Strikeout position in font units
    ///
    /// Distance from baseline to center of strikeout.
    /// From OS/2 table.
    pub strikeout_position: Option<i16>,

    /// Strikeout thickness in font units
    ///
    /// Thickness of strikeout stroke.
    /// From OS/2 table.
    pub strikeout_size: Option<i16>,

    /// Whether the font is bold
    ///
    /// Determined from OS/2 table flags.
    pub is_bold: bool,

    /// Whether the font is italic
    ///
    /// Determined from OS/2 or head table flags.
    pub is_italic: bool,

    /// Whether the font is monospace
    ///
    /// Determined by checking if all glyphs have the same advance width.
    pub is_monospace: bool,

    /// Average character width in font units
    ///
    /// Used for estimating text width without shaping.
    /// From OS/2 table's 'xAvgCharWidth' field.
    pub average_char_width: Option<i16>,

    /// Number of glyphs in the font
    pub glyph_count: u16,
}

impl FontMetrics {
    /// Extract metrics from font data
    ///
    /// Parses the font file and extracts all available metrics.
    ///
    /// # Arguments
    ///
    /// * `data` - Raw font file data (TTF, OTF, or TTC)
    /// * `index` - Face index for TTC collections (use 0 for single fonts)
    ///
    /// # Returns
    ///
    /// Font metrics on success, or an error if parsing fails.
    ///
    /// # Errors
    ///
    /// Returns `FontError::InvalidFontFile` if the font cannot be parsed.
    pub fn from_font_data(data: &[u8], index: u32) -> Result<Self> {
        let face = ttf_parser::Face::parse(data, index).map_err(|e| FontError::InvalidFontFile {
            path: format!("<memory buffer, index {}>: {:?}", index, e),
        })?;

        Self::from_face(&face)
    }

    /// Extract metrics from a ttf-parser Face
    ///
    /// Use this when you already have a parsed Face to avoid re-parsing.
    ///
    /// # Arguments
    ///
    /// * `face` - A parsed ttf-parser Face
    ///
    /// # Returns
    ///
    /// Font metrics on success, or an error if required tables are missing.
    pub fn from_face(face: &ttf_parser::Face) -> Result<Self> {
        let units_per_em = face.units_per_em();

        // Get primary vertical metrics from hhea table
        let ascent = face.ascender();
        let descent = face.descender();
        let line_gap = face.line_gap();

        // Get x-height and cap-height from OS/2 table
        let x_height = face.x_height();
        let cap_height = face.capital_height();

        // Get underline metrics from post table
        // Use sensible defaults if not available
        let (underline_position, underline_thickness) = if let Some(underline) = face.underline_metrics() {
            (underline.position, underline.thickness)
        } else {
            (-(units_per_em as i16) / 10, (units_per_em as i16) / 20)
        };

        // Get strikeout metrics from OS/2 table
        let (strikeout_position, strikeout_size) = if let Some(strikeout) = face.strikeout_metrics() {
            (Some(strikeout.position), Some(strikeout.thickness))
        } else {
            (None, None)
        };

        // Get style information
        let is_bold = face.is_bold();
        let is_italic = face.is_italic();
        let is_monospace = face.is_monospaced();

        // Note: average_char_width is not directly available in ttf-parser,
        // so we set it to None. If needed, it can be computed by measuring glyphs.
        let average_char_width = None;

        // Get glyph count
        let glyph_count = face.number_of_glyphs();

        Ok(Self {
            units_per_em,
            ascent,
            descent,
            line_gap,
            x_height,
            cap_height,
            underline_position,
            underline_thickness,
            strikeout_position,
            strikeout_size,
            is_bold,
            is_italic,
            is_monospace,
            average_char_width,
            glyph_count,
        })
    }

    /// Calculate the recommended line height in font units
    ///
    /// Line height = ascent - descent + line_gap
    ///
    /// Note: descent is negative, so we subtract it (making positive).
    pub fn line_height(&self) -> i16 {
        self.ascent - self.descent + self.line_gap
    }

    /// Scale metrics to a specific font size in pixels
    ///
    /// Converts all font unit values to pixel values for the given font size.
    ///
    /// # Arguments
    ///
    /// * `font_size` - Desired font size in pixels
    ///
    /// # Returns
    ///
    /// A `ScaledFontMetrics` struct with all values in pixels.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// let metrics = FontMetrics::from_font_data(&data, 0)?;
    /// let scaled = metrics.scale(16.0);
    /// println!("Line height at 16px: {:.2}px", scaled.line_height);
    /// ```
    pub fn scale(&self, font_size: f32) -> ScaledFontMetrics {
        let scale = font_size / (self.units_per_em as f32);

        ScaledFontMetrics {
            font_size,
            units_per_em: self.units_per_em,
            scale,
            ascent: (self.ascent as f32) * scale,
            // Make descent positive for easier use in layout
            descent: -(self.descent as f32) * scale,
            line_gap: (self.line_gap as f32) * scale,
            line_height: (self.line_height() as f32) * scale,
            x_height: self.x_height.map(|h| (h as f32) * scale),
            cap_height: self.cap_height.map(|h| (h as f32) * scale),
            underline_position: (self.underline_position as f32) * scale,
            underline_thickness: (self.underline_thickness as f32) * scale,
            strikeout_position: self.strikeout_position.map(|p| (p as f32) * scale),
            strikeout_size: self.strikeout_size.map(|s| (s as f32) * scale),
            average_char_width: self.average_char_width.map(|w| (w as f32) * scale),
        }
    }

    /// Get normal line height as CSS defines it
    ///
    /// CSS 'line-height: normal' typically equals 1.0 to 1.2 times the font size,
    /// but can be calculated more precisely from font metrics.
    ///
    /// # Arguments
    ///
    /// * `font_size` - Font size in pixels
    ///
    /// # Returns
    ///
    /// The calculated normal line height in pixels.
    pub fn normal_line_height(&self, font_size: f32) -> f32 {
        let scale = font_size / (self.units_per_em as f32);
        (self.line_height() as f32) * scale
    }

    /// Estimate x-height when not provided by the font
    ///
    /// Falls back to 0.5 * ascent if x-height is not in the font.
    ///
    /// # Arguments
    ///
    /// * `font_size` - Font size in pixels
    ///
    /// # Returns
    ///
    /// The x-height in pixels (actual or estimated).
    pub fn x_height_or_fallback(&self, font_size: f32) -> f32 {
        let scale = font_size / (self.units_per_em as f32);
        match self.x_height {
            Some(h) => (h as f32) * scale,
            // Fallback: x-height is approximately 50% of ascent
            None => (self.ascent as f32) * scale * 0.5,
        }
    }

    /// Estimate cap-height when not provided by the font
    ///
    /// Falls back to 0.7 * ascent if cap-height is not in the font.
    ///
    /// # Arguments
    ///
    /// * `font_size` - Font size in pixels
    ///
    /// # Returns
    ///
    /// The cap-height in pixels (actual or estimated).
    pub fn cap_height_or_fallback(&self, font_size: f32) -> f32 {
        let scale = font_size / (self.units_per_em as f32);
        match self.cap_height {
            Some(h) => (h as f32) * scale,
            // Fallback: cap-height is approximately 70% of ascent
            None => (self.ascent as f32) * scale * 0.7,
        }
    }
}

/// Scaled font metrics (in pixels)
///
/// All values are pre-scaled to pixel units for a specific font size.
/// Descent is made positive for easier use in layout calculations.
#[derive(Debug, Clone, PartialEq)]
pub struct ScaledFontMetrics {
    /// Font size in pixels that these metrics were scaled for
    pub font_size: f32,

    /// Original units per em from the font
    pub units_per_em: u16,

    /// Scale factor: font_size / units_per_em
    pub scale: f32,

    /// Ascent in pixels (positive)
    pub ascent: f32,

    /// Descent in pixels (positive - converted from negative font units)
    pub descent: f32,

    /// Line gap in pixels
    pub line_gap: f32,

    /// Line height in pixels (ascent + descent + line_gap)
    pub line_height: f32,

    /// x-height in pixels (if available)
    pub x_height: Option<f32>,

    /// Cap height in pixels (if available)
    pub cap_height: Option<f32>,

    /// Underline position in pixels (negative = below baseline)
    pub underline_position: f32,

    /// Underline thickness in pixels
    pub underline_thickness: f32,

    /// Strikeout position in pixels (if available)
    pub strikeout_position: Option<f32>,

    /// Strikeout size in pixels (if available)
    pub strikeout_size: Option<f32>,

    /// Average character width in pixels (if available)
    pub average_char_width: Option<f32>,
}

impl ScaledFontMetrics {
    /// Get baseline offset from top of line box
    ///
    /// Used in inline layout for baseline alignment.
    /// The baseline is at `ascent` pixels from the top.
    pub fn baseline_offset(&self) -> f32 {
        self.ascent
    }

    /// Get total height (ascent + descent)
    ///
    /// This is the em square height scaled to the current font size.
    pub fn em_height(&self) -> f32 {
        self.ascent + self.descent
    }

    /// Get x-height with fallback
    ///
    /// Returns the x-height if available, otherwise estimates as 50% of ascent.
    pub fn x_height_or_estimate(&self) -> f32 {
        self.x_height.unwrap_or(self.ascent * 0.5)
    }

    /// Get cap-height with fallback
    ///
    /// Returns the cap-height if available, otherwise estimates as 70% of ascent.
    pub fn cap_height_or_estimate(&self) -> f32 {
        self.cap_height.unwrap_or(self.ascent * 0.7)
    }

    /// Apply a line height multiplier
    ///
    /// CSS line-height can be a number multiplier (e.g., 1.5).
    /// This creates new metrics with adjusted line height.
    ///
    /// # Arguments
    ///
    /// * `multiplier` - Line height as a multiple of font size
    ///
    /// # Returns
    ///
    /// New scaled metrics with adjusted line height.
    pub fn with_line_height_multiplier(&self, multiplier: f32) -> Self {
        Self {
            line_height: self.font_size * multiplier,
            ..*self
        }
    }

    /// Apply an absolute line height
    ///
    /// CSS line-height can be an absolute value (e.g., 24px).
    /// This creates new metrics with the specified line height.
    ///
    /// # Arguments
    ///
    /// * `line_height` - Absolute line height in pixels
    ///
    /// # Returns
    ///
    /// New scaled metrics with specified line height.
    pub fn with_line_height(&self, line_height: f32) -> Self {
        Self { line_height, ..*self }
    }

    /// Calculate leading (extra space above and below text)
    ///
    /// Leading is distributed half above and half below the text.
    /// Leading = (line_height - em_height) / 2
    ///
    /// Returns the half-leading value.
    pub fn half_leading(&self) -> f32 {
        (self.line_height - self.em_height()) / 2.0
    }
}

/// Extract font metrics from a ttf-parser Face
///
/// Convenience function for cases where you already have a Face.
///
/// # Arguments
///
/// * `face` - A parsed ttf-parser Face
///
/// # Returns
///
/// Font metrics on success.
pub fn extract_metrics(face: &ttf_parser::Face) -> Result<FontMetrics> {
    FontMetrics::from_face(face)
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to create minimal valid TTF font data for testing
    // In real tests, we'd use actual font files

    #[test]
    fn test_line_height_calculation() {
        let metrics = FontMetrics {
            units_per_em: 1000,
            ascent: 800,
            descent: -200,
            line_gap: 100,
            x_height: Some(500),
            cap_height: Some(700),
            underline_position: -100,
            underline_thickness: 50,
            strikeout_position: Some(300),
            strikeout_size: Some(50),
            is_bold: false,
            is_italic: false,
            is_monospace: false,
            average_char_width: Some(500),
            glyph_count: 256,
        };

        // line_height = ascent - descent + line_gap = 800 - (-200) + 100 = 1100
        assert_eq!(metrics.line_height(), 1100);
    }

    #[test]
    fn test_scale_metrics() {
        let metrics = FontMetrics {
            units_per_em: 1000,
            ascent: 800,
            descent: -200,
            line_gap: 100,
            x_height: Some(500),
            cap_height: Some(700),
            underline_position: -100,
            underline_thickness: 50,
            strikeout_position: Some(300),
            strikeout_size: Some(50),
            is_bold: false,
            is_italic: false,
            is_monospace: false,
            average_char_width: Some(500),
            glyph_count: 256,
        };

        let scaled = metrics.scale(16.0);

        // Scale factor = 16.0 / 1000 = 0.016
        assert_eq!(scaled.font_size, 16.0);
        assert_eq!(scaled.scale, 0.016);

        // Ascent: 800 * 0.016 = 12.8
        assert!((scaled.ascent - 12.8).abs() < 0.001);

        // Descent: -(-200) * 0.016 = 3.2 (made positive)
        assert!((scaled.descent - 3.2).abs() < 0.001);

        // Line height: 1100 * 0.016 = 17.6
        assert!((scaled.line_height - 17.6).abs() < 0.001);

        // x-height: 500 * 0.016 = 8.0
        assert!((scaled.x_height.unwrap() - 8.0).abs() < 0.001);
    }

    #[test]
    fn test_scaled_metrics_baseline_offset() {
        let metrics = FontMetrics {
            units_per_em: 1000,
            ascent: 800,
            descent: -200,
            line_gap: 0,
            x_height: None,
            cap_height: None,
            underline_position: -100,
            underline_thickness: 50,
            strikeout_position: None,
            strikeout_size: None,
            is_bold: false,
            is_italic: false,
            is_monospace: false,
            average_char_width: None,
            glyph_count: 100,
        };

        let scaled = metrics.scale(20.0);

        // Baseline offset = ascent = 800 * 0.02 = 16.0
        assert!((scaled.baseline_offset() - 16.0).abs() < 0.001);
    }

    #[test]
    fn test_em_height() {
        let metrics = FontMetrics {
            units_per_em: 2048,
            ascent: 1800,
            descent: -400,
            line_gap: 0,
            x_height: None,
            cap_height: None,
            underline_position: -100,
            underline_thickness: 50,
            strikeout_position: None,
            strikeout_size: None,
            is_bold: false,
            is_italic: false,
            is_monospace: false,
            average_char_width: None,
            glyph_count: 100,
        };

        let scaled = metrics.scale(16.0);

        // em_height = ascent + descent (both positive in scaled)
        // ascent: 1800 * (16/2048) = 14.0625
        // descent: 400 * (16/2048) = 3.125
        let expected_em_height = (1800.0 + 400.0) * (16.0 / 2048.0);
        assert!((scaled.em_height() - expected_em_height).abs() < 0.001);
    }

    #[test]
    fn test_x_height_fallback() {
        let metrics = FontMetrics {
            units_per_em: 1000,
            ascent: 800,
            descent: -200,
            line_gap: 0,
            x_height: None, // No x-height in font
            cap_height: None,
            underline_position: -100,
            underline_thickness: 50,
            strikeout_position: None,
            strikeout_size: None,
            is_bold: false,
            is_italic: false,
            is_monospace: false,
            average_char_width: None,
            glyph_count: 100,
        };

        // Fallback should be 50% of ascent
        let expected = 800.0 * 0.5 * (16.0 / 1000.0); // = 6.4
        let actual = metrics.x_height_or_fallback(16.0);
        assert!((actual - expected).abs() < 0.001);
    }

    #[test]
    fn test_cap_height_fallback() {
        let metrics = FontMetrics {
            units_per_em: 1000,
            ascent: 800,
            descent: -200,
            line_gap: 0,
            x_height: None,
            cap_height: None, // No cap-height in font
            underline_position: -100,
            underline_thickness: 50,
            strikeout_position: None,
            strikeout_size: None,
            is_bold: false,
            is_italic: false,
            is_monospace: false,
            average_char_width: None,
            glyph_count: 100,
        };

        // Fallback should be 70% of ascent
        let expected = 800.0 * 0.7 * (16.0 / 1000.0); // = 8.96
        let actual = metrics.cap_height_or_fallback(16.0);
        assert!((actual - expected).abs() < 0.001);
    }

    #[test]
    fn test_line_height_multiplier() {
        let metrics = FontMetrics {
            units_per_em: 1000,
            ascent: 800,
            descent: -200,
            line_gap: 0,
            x_height: None,
            cap_height: None,
            underline_position: -100,
            underline_thickness: 50,
            strikeout_position: None,
            strikeout_size: None,
            is_bold: false,
            is_italic: false,
            is_monospace: false,
            average_char_width: None,
            glyph_count: 100,
        };

        let scaled = metrics.scale(16.0);
        let adjusted = scaled.with_line_height_multiplier(1.5);

        // line_height = font_size * multiplier = 16 * 1.5 = 24
        assert!((adjusted.line_height - 24.0).abs() < 0.001);
        // Other metrics should be unchanged
        assert_eq!(adjusted.ascent, scaled.ascent);
    }

    #[test]
    fn test_absolute_line_height() {
        let metrics = FontMetrics {
            units_per_em: 1000,
            ascent: 800,
            descent: -200,
            line_gap: 0,
            x_height: None,
            cap_height: None,
            underline_position: -100,
            underline_thickness: 50,
            strikeout_position: None,
            strikeout_size: None,
            is_bold: false,
            is_italic: false,
            is_monospace: false,
            average_char_width: None,
            glyph_count: 100,
        };

        let scaled = metrics.scale(16.0);
        let adjusted = scaled.with_line_height(30.0);

        assert_eq!(adjusted.line_height, 30.0);
    }

    #[test]
    fn test_half_leading() {
        let metrics = FontMetrics {
            units_per_em: 1000,
            ascent: 800,
            descent: -200,
            line_gap: 0,
            x_height: None,
            cap_height: None,
            underline_position: -100,
            underline_thickness: 50,
            strikeout_position: None,
            strikeout_size: None,
            is_bold: false,
            is_italic: false,
            is_monospace: false,
            average_char_width: None,
            glyph_count: 100,
        };

        let scaled = metrics.scale(16.0);
        // em_height = (800 + 200) * 0.016 = 16.0
        // line_height = (800 + 200 + 0) * 0.016 = 16.0
        // half_leading = (16.0 - 16.0) / 2 = 0.0
        assert!((scaled.half_leading()).abs() < 0.001);

        // With line gap
        let metrics_with_gap = FontMetrics {
            line_gap: 200,
            ..metrics
        };
        let scaled_with_gap = metrics_with_gap.scale(16.0);
        // line_height = 1200 * 0.016 = 19.2
        // em_height = 1000 * 0.016 = 16.0
        // half_leading = (19.2 - 16.0) / 2 = 1.6
        assert!((scaled_with_gap.half_leading() - 1.6).abs() < 0.001);
    }

    #[test]
    fn test_scaled_metrics_x_height_or_estimate() {
        // With x_height
        let metrics = FontMetrics {
            units_per_em: 1000,
            ascent: 800,
            descent: -200,
            line_gap: 0,
            x_height: Some(500),
            cap_height: None,
            underline_position: -100,
            underline_thickness: 50,
            strikeout_position: None,
            strikeout_size: None,
            is_bold: false,
            is_italic: false,
            is_monospace: false,
            average_char_width: None,
            glyph_count: 100,
        };

        let scaled = metrics.scale(16.0);
        assert!((scaled.x_height_or_estimate() - 8.0).abs() < 0.001);

        // Without x_height
        let metrics_no_x = FontMetrics {
            x_height: None,
            ..metrics
        };
        let scaled_no_x = metrics_no_x.scale(16.0);
        // Estimate: 50% of ascent = 12.8 * 0.5 = 6.4
        assert!((scaled_no_x.x_height_or_estimate() - 6.4).abs() < 0.001);
    }

    #[test]
    fn test_normal_line_height() {
        let metrics = FontMetrics {
            units_per_em: 1000,
            ascent: 800,
            descent: -200,
            line_gap: 100,
            x_height: None,
            cap_height: None,
            underline_position: -100,
            underline_thickness: 50,
            strikeout_position: None,
            strikeout_size: None,
            is_bold: false,
            is_italic: false,
            is_monospace: false,
            average_char_width: None,
            glyph_count: 100,
        };

        // normal_line_height at 16px = 1100 * (16/1000) = 17.6
        let normal = metrics.normal_line_height(16.0);
        assert!((normal - 17.6).abs() < 0.001);
    }
}
