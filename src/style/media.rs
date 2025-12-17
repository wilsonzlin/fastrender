//! CSS Media Queries
//!
//! This module implements CSS Media Queries Level 4 to enable responsive web design.
//!
//! Media queries allow stylesheets to adapt to different devices, screen sizes,
//! and capabilities.
//!
//! # Overview
//!
//! A media query consists of:
//! - An optional **media type** (screen, print, all)
//! - An optional **modifier** (not, only)
//! - Zero or more **media features** (width, height, orientation, etc.)
//!
//! # Examples
//!
//! ```
//! use fastrender::style::media::{MediaQuery, MediaContext, MediaType};
//!
//! // Create a context for a desktop browser
//! let ctx = MediaContext::screen(1024.0, 768.0);
//!
//! // Parse and evaluate a media query
//! let query = MediaQuery::parse("(min-width: 768px)").unwrap();
//! assert!(ctx.evaluate(&query));
//! ```
//!
//! # CSS Syntax Examples
//!
//! ```css
//! @media screen { ... }
//! @media (min-width: 768px) { ... }
//! @media screen and (min-width: 768px) { ... }
//! @media (min-width: 768px) and (max-width: 1024px) { ... }
//! @media not screen and (color) { ... }
//! @media print, screen and (min-width: 1024px) { ... }
//! ```
//!
//! # References
//!
//! - CSS Media Queries Level 4: <https://www.w3.org/TR/mediaqueries-4/>
//! - CSS Media Queries Level 5: <https://www.w3.org/TR/mediaqueries-5/>

use crate::style::values::Length;
use std::env;
use std::fmt;

/// A single media query
///
/// A media query is composed of an optional media type, modifier, and
/// a list of media features that must all match for the query to be true.
///
/// # Examples
///
/// ```
/// use fastrender::style::media::{MediaQuery, MediaType, MediaModifier, MediaFeature};
/// use fastrender::Length;
///
/// // Equivalent to: @media screen and (min-width: 768px)
/// let query = MediaQuery {
///     media_type: Some(MediaType::Screen),
///     modifier: None,
///     features: vec![MediaFeature::MinWidth(Length::px(768.0))],
/// };
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct MediaQuery {
    /// Optional media type (screen, print, all, etc.)
    pub media_type: Option<MediaType>,

    /// Logic modifier (not, only)
    pub modifier: Option<MediaModifier>,

    /// Media features (width, height, orientation, etc.)
    /// All features must match (AND logic)
    pub features: Vec<MediaFeature>,
}

impl MediaQuery {
    /// Returns true when the query only uses size features suitable for container queries.
    pub fn is_size_query(&self) -> bool {
        // Container queries level 1 allow only size features and no media type.
        if self.media_type.is_some() {
            return false;
        }
        self.features.iter().all(|f| f.is_size_feature())
    }

    /// Creates a new empty media query
    ///
    /// An empty query matches all media.
    pub fn new() -> Self {
        Self {
            media_type: None,
            modifier: None,
            features: Vec::new(),
        }
    }

    /// Creates a media query with just a media type
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::media::{MediaQuery, MediaType};
    ///
    /// let query = MediaQuery::with_type(MediaType::Print);
    /// ```
    pub fn with_type(media_type: MediaType) -> Self {
        Self {
            media_type: Some(media_type),
            modifier: None,
            features: Vec::new(),
        }
    }

    /// Creates a media query with a single feature
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::media::{MediaQuery, MediaFeature};
    /// use fastrender::Length;
    ///
    /// let query = MediaQuery::with_feature(MediaFeature::MinWidth(Length::px(768.0)));
    /// ```
    pub fn with_feature(feature: MediaFeature) -> Self {
        Self {
            media_type: None,
            modifier: None,
            features: vec![feature],
        }
    }

    /// Parses a media query from a CSS string
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::media::MediaQuery;
    ///
    /// let query = MediaQuery::parse("screen and (min-width: 768px)").unwrap();
    /// let query = MediaQuery::parse("(orientation: portrait)").unwrap();
    /// let query = MediaQuery::parse("not print").unwrap();
    /// ```
    ///
    /// # Errors
    ///
    /// Returns an error if the input is not a valid media query.
    pub fn parse(input: &str) -> Result<Self, MediaParseError> {
        MediaQueryParser::new(input).parse_query()
    }

    /// Parses a comma-separated list of media queries
    ///
    /// A query list matches if ANY query in the list matches (OR logic).
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::media::MediaQuery;
    ///
    /// let queries = MediaQuery::parse_list("screen, print").unwrap();
    /// assert_eq!(queries.len(), 2);
    /// ```
    pub fn parse_list(input: &str) -> Result<Vec<Self>, MediaParseError> {
        MediaQueryParser::new(input).parse_query_list()
    }
}

impl Default for MediaQuery {
    fn default() -> Self {
        Self::new()
    }
}

/// Media types
///
/// The media type defines the broad category of device the query applies to.
///
/// # Examples
///
/// ```
/// use fastrender::style::media::MediaType;
///
/// let screen = MediaType::parse("screen").unwrap();
/// assert_eq!(screen, MediaType::Screen);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MediaType {
    /// Matches all devices (default)
    All,
    /// Computer screens, tablets, phones
    Screen,
    /// Print preview and printed pages
    Print,
    /// Screen readers (rarely used)
    Speech,
}

impl MediaType {
    /// Parses a media type from a string
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::media::MediaType;
    ///
    /// assert_eq!(MediaType::parse("all").unwrap(), MediaType::All);
    /// assert_eq!(MediaType::parse("screen").unwrap(), MediaType::Screen);
    /// assert_eq!(MediaType::parse("print").unwrap(), MediaType::Print);
    /// assert!(MediaType::parse("invalid").is_err());
    /// ```
    pub fn parse(s: &str) -> Result<Self, MediaParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "all" => Ok(MediaType::All),
            "screen" => Ok(MediaType::Screen),
            "print" => Ok(MediaType::Print),
            "speech" => Ok(MediaType::Speech),
            _ => Err(MediaParseError::InvalidMediaType(s)),
        }
    }
}

impl fmt::Display for MediaType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MediaType::All => write!(f, "all"),
            MediaType::Screen => write!(f, "screen"),
            MediaType::Print => write!(f, "print"),
            MediaType::Speech => write!(f, "speech"),
        }
    }
}

/// Media query modifiers
///
/// Modifiers change how the query is evaluated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MediaModifier {
    /// Negates the entire query
    ///
    /// `@media not screen { ... }` matches when NOT screen
    Not,
    /// Used to hide styles from older browsers
    ///
    /// `@media only screen { ... }` is functionally equivalent to `@media screen { ... }`
    /// in modern browsers, but older browsers that don't understand media queries
    /// will see "only" as an unknown media type and ignore the rules.
    Only,
}

impl MediaModifier {
    /// Parses a modifier from a string
    pub fn parse(s: &str) -> Option<Self> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "not" => Some(MediaModifier::Not),
            "only" => Some(MediaModifier::Only),
            _ => None,
        }
    }
}

impl fmt::Display for MediaModifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MediaModifier::Not => write!(f, "not"),
            MediaModifier::Only => write!(f, "only"),
        }
    }
}

/// A media feature test
///
/// Media features test specific conditions about the device or viewport.
///
/// # Examples
///
/// ```
/// use fastrender::style::media::MediaFeature;
/// use fastrender::Length;
///
/// let min_width = MediaFeature::MinWidth(Length::px(768.0));
/// let orientation = MediaFeature::Orientation(fastrender::style::media::Orientation::Portrait);
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum MediaFeature {
    // Viewport width/inline-size features
    /// Exact viewport width: `(width: 768px)`
    Width(Length),
    /// Minimum viewport width: `(min-width: 768px)`
    MinWidth(Length),
    /// Maximum viewport width: `(max-width: 1024px)`
    MaxWidth(Length),
    /// Exact inline size (alias of width for container queries): `(inline-size: 500px)`
    InlineSize(Length),
    /// Minimum inline size: `(min-inline-size: 500px)`
    MinInlineSize(Length),
    /// Maximum inline size: `(max-inline-size: 800px)`
    MaxInlineSize(Length),

    // Viewport height/block-size features
    /// Exact viewport height: `(height: 600px)`
    Height(Length),
    /// Minimum viewport height: `(min-height: 400px)`
    MinHeight(Length),
    /// Maximum viewport height: `(max-height: 900px)`
    MaxHeight(Length),
    /// Exact block size (alias of height for container queries): `(block-size: 400px)`
    BlockSize(Length),
    /// Minimum block size: `(min-block-size: 400px)`
    MinBlockSize(Length),
    /// Maximum block size: `(max-block-size: 900px)`
    MaxBlockSize(Length),

    // Orientation
    /// Device orientation: `(orientation: portrait)`
    Orientation(Orientation),

    // Aspect ratio features
    /// Exact aspect ratio: `(aspect-ratio: 16/9)`
    AspectRatio { width: u32, height: u32 },
    /// Minimum aspect ratio: `(min-aspect-ratio: 16/9)`
    MinAspectRatio { width: u32, height: u32 },
    /// Maximum aspect ratio: `(max-aspect-ratio: 16/9)`
    MaxAspectRatio { width: u32, height: u32 },

    // Resolution features
    /// Exact resolution: `(resolution: 2dppx)`
    Resolution(Resolution),
    /// Minimum resolution: `(min-resolution: 2dppx)`
    MinResolution(Resolution),
    /// Maximum resolution: `(max-resolution: 3dppx)`
    MaxResolution(Resolution),

    // Color features
    /// Device has color display: `(color)`
    Color,
    /// Minimum color depth: `(min-color: 8)`
    MinColor(u32),
    /// Maximum color depth: `(max-color: 8)`
    MaxColor(u32),
    /// Device has color-index: `(color-index)`
    ColorIndex,
    /// Minimum color-index: `(min-color-index: 256)`
    MinColorIndex(u32),
    /// Maximum color-index: `(max-color-index: 256)`
    MaxColorIndex(u32),

    // Monochrome features
    /// Device is monochrome: `(monochrome)`
    Monochrome,
    /// Minimum monochrome depth: `(min-monochrome: 1)`
    MinMonochrome(u32),
    /// Maximum monochrome depth: `(max-monochrome: 1)`
    MaxMonochrome(u32),

    // Interaction features (Level 4)
    /// Hover capability: `(hover: hover)` or `(hover: none)`
    Hover(HoverCapability),
    /// Any input can hover: `(any-hover: hover)`
    AnyHover(HoverCapability),
    /// Pointer precision: `(pointer: coarse)` or `(pointer: fine)`
    Pointer(PointerCapability),
    /// Any pointer precision: `(any-pointer: fine)`
    AnyPointer(PointerCapability),

    // User preference features (Level 5)
    /// Color scheme preference: `(prefers-color-scheme: dark)`
    PrefersColorScheme(ColorScheme),
    /// Reduced motion preference: `(prefers-reduced-motion: reduce)`
    PrefersReducedMotion(ReducedMotion),
    /// Contrast preference: `(prefers-contrast: high)`
    PrefersContrast(ContrastPreference),
    /// Reduced transparency preference: `(prefers-reduced-transparency: reduce)`
    PrefersReducedTransparency(ReducedTransparency),
    /// Reduced data usage preference: `(prefers-reduced-data: reduce)`
    PrefersReducedData(ReducedData),
    /// Forced-colors user agent state: `(forced-colors: active|none)`
    ForcedColors(ForcedColors),
    /// Whether the UA is in an inverted-color mode: `(inverted-colors: inverted)`
    InvertedColors(InvertedColors),
    /// Range comparisons using level 4 syntax (e.g., `400px < width <= 800px`)
    Range {
        feature: RangeFeature,
        op: ComparisonOp,
        value: RangeValue,
    },
}

/// Comparison operator for range-based media features
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp {
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
}

/// Which media dimension/value a range comparison targets
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangeFeature {
    Width,
    InlineSize,
    BlockSize,
    Height,
    AspectRatio,
    Resolution,
}

/// Value for a range comparison
#[derive(Debug, Clone, PartialEq)]
pub enum RangeValue {
    Length(Length),
    AspectRatio(u32, u32),
    Resolution(Resolution),
}

impl MediaFeature {
    /// Returns true when this feature is a size query (width/height/inline-size/block-size/aspect-ratio/orientation).
    pub fn is_size_feature(&self) -> bool {
        match self {
            MediaFeature::Width(_)
            | MediaFeature::MinWidth(_)
            | MediaFeature::MaxWidth(_)
            | MediaFeature::InlineSize(_)
            | MediaFeature::MinInlineSize(_)
            | MediaFeature::MaxInlineSize(_)
            | MediaFeature::Height(_)
            | MediaFeature::MinHeight(_)
            | MediaFeature::MaxHeight(_)
            | MediaFeature::BlockSize(_)
            | MediaFeature::MinBlockSize(_)
            | MediaFeature::MaxBlockSize(_)
            | MediaFeature::AspectRatio { .. }
            | MediaFeature::MinAspectRatio { .. }
            | MediaFeature::MaxAspectRatio { .. }
            | MediaFeature::Orientation(_) => true,
            MediaFeature::Range { feature, .. } => matches!(
                feature,
                RangeFeature::Width
                    | RangeFeature::InlineSize
                    | RangeFeature::BlockSize
                    | RangeFeature::Height
                    | RangeFeature::AspectRatio
            ),
            _ => false,
        }
    }

    /// Parses a media feature from name and optional value
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::media::MediaFeature;
    ///
    /// let feature = MediaFeature::parse("min-width", Some("768px")).unwrap();
    /// let color = MediaFeature::parse("color", None).unwrap();
    /// ```
    pub fn parse(name: &str, value: Option<&str>) -> Result<Self, MediaParseError> {
        let name = name.trim().to_lowercase();

        match name.as_str() {
            // Width features
            "width" => {
                let length = Self::parse_length_value(&name, value)?;
                Ok(MediaFeature::Width(length))
            }
            "min-width" => {
                let length = Self::parse_length_value(&name, value)?;
                Ok(MediaFeature::MinWidth(length))
            }
            "max-width" => {
                let length = Self::parse_length_value(&name, value)?;
                Ok(MediaFeature::MaxWidth(length))
            }

            // Inline-size features (aliases of width for container queries)
            "inline-size" => {
                let length = Self::parse_length_value(&name, value)?;
                Ok(MediaFeature::InlineSize(length))
            }
            "min-inline-size" => {
                let length = Self::parse_length_value(&name, value)?;
                Ok(MediaFeature::MinInlineSize(length))
            }
            "max-inline-size" => {
                let length = Self::parse_length_value(&name, value)?;
                Ok(MediaFeature::MaxInlineSize(length))
            }

            // Height features
            "height" => {
                let length = Self::parse_length_value(&name, value)?;
                Ok(MediaFeature::Height(length))
            }
            "min-height" => {
                let length = Self::parse_length_value(&name, value)?;
                Ok(MediaFeature::MinHeight(length))
            }
            "max-height" => {
                let length = Self::parse_length_value(&name, value)?;
                Ok(MediaFeature::MaxHeight(length))
            }

            // Block-size features (aliases of height for container queries)
            "block-size" => {
                let length = Self::parse_length_value(&name, value)?;
                Ok(MediaFeature::BlockSize(length))
            }
            "min-block-size" => {
                let length = Self::parse_length_value(&name, value)?;
                Ok(MediaFeature::MinBlockSize(length))
            }
            "max-block-size" => {
                let length = Self::parse_length_value(&name, value)?;
                Ok(MediaFeature::MaxBlockSize(length))
            }

            // Orientation
            "orientation" => {
                let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
                let orientation = Orientation::parse(value)?;
                Ok(MediaFeature::Orientation(orientation))
            }

            // Aspect ratio
            "aspect-ratio" => {
                let (width, height) = Self::parse_ratio_value(&name, value)?;
                Ok(MediaFeature::AspectRatio { width, height })
            }
            "min-aspect-ratio" => {
                let (width, height) = Self::parse_ratio_value(&name, value)?;
                Ok(MediaFeature::MinAspectRatio { width, height })
            }
            "max-aspect-ratio" => {
                let (width, height) = Self::parse_ratio_value(&name, value)?;
                Ok(MediaFeature::MaxAspectRatio { width, height })
            }

            // Resolution
            "resolution" => {
                let resolution = Self::parse_resolution_value(&name, value)?;
                Ok(MediaFeature::Resolution(resolution))
            }
            "min-resolution" => {
                let resolution = Self::parse_resolution_value(&name, value)?;
                Ok(MediaFeature::MinResolution(resolution))
            }
            "max-resolution" => {
                let resolution = Self::parse_resolution_value(&name, value)?;
                Ok(MediaFeature::MaxResolution(resolution))
            }

            // Color features
            "color" => {
                if let Some(value) = value {
                    // (color: 8) - exact color depth not commonly used
                    // For simplicity, treat as min-color
                    let bits = Self::parse_integer_value(&name, Some(value))?;
                    Ok(MediaFeature::MinColor(bits))
                } else {
                    Ok(MediaFeature::Color)
                }
            }
            "min-color" => {
                let bits = Self::parse_integer_value(&name, value)?;
                Ok(MediaFeature::MinColor(bits))
            }
            "max-color" => {
                let bits = Self::parse_integer_value(&name, value)?;
                Ok(MediaFeature::MaxColor(bits))
            }
            "color-index" => {
                if value.is_some() {
                    let count = Self::parse_integer_value(&name, value)?;
                    Ok(MediaFeature::MinColorIndex(count))
                } else {
                    Ok(MediaFeature::ColorIndex)
                }
            }
            "min-color-index" => {
                let count = Self::parse_integer_value(&name, value)?;
                Ok(MediaFeature::MinColorIndex(count))
            }
            "max-color-index" => {
                let count = Self::parse_integer_value(&name, value)?;
                Ok(MediaFeature::MaxColorIndex(count))
            }

            // Monochrome features
            "monochrome" => {
                if value.is_some() {
                    let bits = Self::parse_integer_value(&name, value)?;
                    Ok(MediaFeature::MinMonochrome(bits))
                } else {
                    Ok(MediaFeature::Monochrome)
                }
            }
            "min-monochrome" => {
                let bits = Self::parse_integer_value(&name, value)?;
                Ok(MediaFeature::MinMonochrome(bits))
            }
            "max-monochrome" => {
                let bits = Self::parse_integer_value(&name, value)?;
                Ok(MediaFeature::MaxMonochrome(bits))
            }

            // Hover capability
            "hover" => {
                let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
                let capability = HoverCapability::parse(value)?;
                Ok(MediaFeature::Hover(capability))
            }
            "any-hover" => {
                let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
                let capability = HoverCapability::parse(value)?;
                Ok(MediaFeature::AnyHover(capability))
            }

            // Pointer capability
            "pointer" => {
                let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
                let capability = PointerCapability::parse(value)?;
                Ok(MediaFeature::Pointer(capability))
            }
            "any-pointer" => {
                let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
                let capability = PointerCapability::parse(value)?;
                Ok(MediaFeature::AnyPointer(capability))
            }

            // User preferences
            "prefers-color-scheme" => {
                let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
                let scheme = ColorScheme::parse(value)?;
                Ok(MediaFeature::PrefersColorScheme(scheme))
            }
            "prefers-reduced-motion" => {
                let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
                let motion = ReducedMotion::parse(value)?;
                Ok(MediaFeature::PrefersReducedMotion(motion))
            }
            "prefers-contrast" => {
                let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
                let contrast = ContrastPreference::parse(value)?;
                Ok(MediaFeature::PrefersContrast(contrast))
            }
            "prefers-reduced-transparency" => {
                let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
                let transparency = ReducedTransparency::parse(value)?;
                Ok(MediaFeature::PrefersReducedTransparency(transparency))
            }
            "prefers-reduced-data" => {
                let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
                let data = ReducedData::parse(value)?;
                Ok(MediaFeature::PrefersReducedData(data))
            }
            "forced-colors" => {
                let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
                let forced = ForcedColors::parse(value)?;
                Ok(MediaFeature::ForcedColors(forced))
            }
            "inverted-colors" => {
                let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
                let inverted = InvertedColors::parse(value)?;
                Ok(MediaFeature::InvertedColors(inverted))
            }

            _ => Err(MediaParseError::UnknownFeature(name)),
        }
    }

    fn parse_length_value(name: &str, value: Option<&str>) -> Result<Length, MediaParseError> {
        let value = value.ok_or_else(|| MediaParseError::MissingValue(name.to_string()))?;
        parse_length(value).ok_or_else(|| MediaParseError::InvalidValue {
            feature: name.to_string(),
            value: value.to_string(),
        })
    }

    fn parse_ratio_value(name: &str, value: Option<&str>) -> Result<(u32, u32), MediaParseError> {
        let value = value.ok_or_else(|| MediaParseError::MissingValue(name.to_string()))?;
        let value = value.trim();

        // Parse "16/9" or "16 / 9"
        let parts: Vec<&str> = value.split('/').map(|s| s.trim()).collect();
        if parts.len() != 2 {
            return Err(MediaParseError::InvalidValue {
                feature: name.to_string(),
                value: value.to_string(),
            });
        }

        let width = parts[0].parse::<u32>().map_err(|_| MediaParseError::InvalidValue {
            feature: name.to_string(),
            value: value.to_string(),
        })?;

        let height = parts[1].parse::<u32>().map_err(|_| MediaParseError::InvalidValue {
            feature: name.to_string(),
            value: value.to_string(),
        })?;

        if height == 0 {
            return Err(MediaParseError::InvalidValue {
                feature: name.to_string(),
                value: value.to_string(),
            });
        }

        Ok((width, height))
    }

    fn parse_resolution_value(name: &str, value: Option<&str>) -> Result<Resolution, MediaParseError> {
        let value = value.ok_or_else(|| MediaParseError::MissingValue(name.to_string()))?;
        Resolution::parse(value).map_err(|_| MediaParseError::InvalidValue {
            feature: name.to_string(),
            value: value.to_string(),
        })
    }

    fn parse_integer_value(name: &str, value: Option<&str>) -> Result<u32, MediaParseError> {
        let value = value.ok_or_else(|| MediaParseError::MissingValue(name.to_string()))?;
        value.trim().parse::<u32>().map_err(|_| MediaParseError::InvalidValue {
            feature: name.to_string(),
            value: value.to_string(),
        })
    }
}

/// Screen orientation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Orientation {
    /// Height is greater than or equal to width
    Portrait,
    /// Width is greater than height
    Landscape,
}

impl Orientation {
    /// Parses an orientation value
    pub fn parse(s: &str) -> Result<Self, MediaParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "portrait" => Ok(Orientation::Portrait),
            "landscape" => Ok(Orientation::Landscape),
            _ => Err(MediaParseError::InvalidOrientation(s)),
        }
    }
}

impl fmt::Display for Orientation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Orientation::Portrait => write!(f, "portrait"),
            Orientation::Landscape => write!(f, "landscape"),
        }
    }
}

/// Resolution value with unit
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Resolution {
    /// Numeric value
    pub value: f32,
    /// Unit of resolution
    pub unit: ResolutionUnit,
}

impl Resolution {
    /// Creates a new resolution value
    pub fn new(value: f32, unit: ResolutionUnit) -> Self {
        Self { value, unit }
    }

    /// Creates a resolution in dppx (dots per pixel)
    pub fn dppx(value: f32) -> Self {
        Self::new(value, ResolutionUnit::Dppx)
    }

    /// Creates a resolution in dpi (dots per inch)
    pub fn dpi(value: f32) -> Self {
        Self::new(value, ResolutionUnit::Dpi)
    }

    /// Parses a resolution from a string (e.g., "2dppx", "96dpi")
    pub fn parse(s: &str) -> Result<Self, MediaParseError> {
        let s = s.trim().to_lowercase();

        // Try each unit suffix
        if let Some(value_str) = s.strip_suffix("dppx") {
            let value = value_str
                .trim()
                .parse::<f32>()
                .map_err(|_| MediaParseError::InvalidResolution(s.clone()))?;
            return Ok(Resolution::new(value, ResolutionUnit::Dppx));
        }

        if let Some(value_str) = s.strip_suffix("dpcm") {
            let value = value_str
                .trim()
                .parse::<f32>()
                .map_err(|_| MediaParseError::InvalidResolution(s.clone()))?;
            return Ok(Resolution::new(value, ResolutionUnit::Dpcm));
        }

        if let Some(value_str) = s.strip_suffix("dpi") {
            let value = value_str
                .trim()
                .parse::<f32>()
                .map_err(|_| MediaParseError::InvalidResolution(s.clone()))?;
            return Ok(Resolution::new(value, ResolutionUnit::Dpi));
        }

        // Try 'x' as alias for dppx
        if let Some(value_str) = s.strip_suffix('x') {
            let value = value_str
                .trim()
                .parse::<f32>()
                .map_err(|_| MediaParseError::InvalidResolution(s.clone()))?;
            return Ok(Resolution::new(value, ResolutionUnit::Dppx));
        }

        Err(MediaParseError::InvalidResolution(s))
    }

    /// Converts to dppx (dots per pixel)
    pub fn to_dppx(self) -> f32 {
        match self.unit {
            ResolutionUnit::Dppx => self.value,
            ResolutionUnit::Dpi => self.value / 96.0,
            ResolutionUnit::Dpcm => self.value / 37.795_277, // ~37.8 dpcm = 1 dppx
        }
    }
}

/// Resolution unit
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolutionUnit {
    /// Dots per inch
    Dpi,
    /// Dots per centimeter
    Dpcm,
    /// Dots per pixel (device pixel ratio)
    Dppx,
}

impl fmt::Display for ResolutionUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolutionUnit::Dpi => write!(f, "dpi"),
            ResolutionUnit::Dpcm => write!(f, "dpcm"),
            ResolutionUnit::Dppx => write!(f, "dppx"),
        }
    }
}

/// Hover capability
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HoverCapability {
    /// Device cannot hover (touch screens)
    None,
    /// Device can hover (mouse, trackpad)
    Hover,
}

impl HoverCapability {
    /// Parses a hover capability value
    pub fn parse(s: &str) -> Result<Self, MediaParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "none" => Ok(HoverCapability::None),
            "hover" => Ok(HoverCapability::Hover),
            _ => Err(MediaParseError::InvalidHoverCapability(s)),
        }
    }
}

impl fmt::Display for HoverCapability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HoverCapability::None => write!(f, "none"),
            HoverCapability::Hover => write!(f, "hover"),
        }
    }
}

/// Pointer accuracy capability
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PointerCapability {
    /// No pointing device
    None,
    /// Coarse pointing device (touch, motion-controller)
    Coarse,
    /// Fine pointing device (mouse, trackpad, stylus)
    Fine,
}

impl PointerCapability {
    /// Parses a pointer capability value
    pub fn parse(s: &str) -> Result<Self, MediaParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "none" => Ok(PointerCapability::None),
            "coarse" => Ok(PointerCapability::Coarse),
            "fine" => Ok(PointerCapability::Fine),
            _ => Err(MediaParseError::InvalidPointerCapability(s)),
        }
    }
}

impl fmt::Display for PointerCapability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PointerCapability::None => write!(f, "none"),
            PointerCapability::Coarse => write!(f, "coarse"),
            PointerCapability::Fine => write!(f, "fine"),
        }
    }
}

/// Color scheme preference
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ColorScheme {
    /// No preference
    NoPreference,
    /// Light color scheme preferred
    Light,
    /// Dark color scheme preferred
    Dark,
}

impl ColorScheme {
    /// Parses a color scheme value
    pub fn parse(s: &str) -> Result<Self, MediaParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "no-preference" => Ok(ColorScheme::NoPreference),
            "light" => Ok(ColorScheme::Light),
            "dark" => Ok(ColorScheme::Dark),
            _ => Err(MediaParseError::InvalidColorScheme(s)),
        }
    }
}

impl fmt::Display for ColorScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ColorScheme::NoPreference => write!(f, "no-preference"),
            ColorScheme::Light => write!(f, "light"),
            ColorScheme::Dark => write!(f, "dark"),
        }
    }
}

/// Reduced motion preference
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReducedMotion {
    /// No preference for reduced motion
    NoPreference,
    /// Reduced motion preferred
    Reduce,
}

impl ReducedMotion {
    /// Parses a reduced motion value
    pub fn parse(s: &str) -> Result<Self, MediaParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "no-preference" => Ok(ReducedMotion::NoPreference),
            "reduce" => Ok(ReducedMotion::Reduce),
            _ => Err(MediaParseError::InvalidReducedMotion(s)),
        }
    }
}

impl fmt::Display for ReducedMotion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReducedMotion::NoPreference => write!(f, "no-preference"),
            ReducedMotion::Reduce => write!(f, "reduce"),
        }
    }
}

/// Contrast preference
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ContrastPreference {
    /// No preference
    NoPreference,
    /// More contrast preferred
    More,
    /// Less contrast preferred
    Less,
    /// Forced colors (high contrast mode)
    Custom,
}

impl ContrastPreference {
    /// Parses a contrast preference value
    pub fn parse(s: &str) -> Result<Self, MediaParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "no-preference" => Ok(ContrastPreference::NoPreference),
            "more" | "high" => Ok(ContrastPreference::More),
            "less" | "low" => Ok(ContrastPreference::Less),
            "custom" | "forced" => Ok(ContrastPreference::Custom),
            _ => Err(MediaParseError::InvalidContrastPreference(s)),
        }
    }
}

impl fmt::Display for ContrastPreference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ContrastPreference::NoPreference => write!(f, "no-preference"),
            ContrastPreference::More => write!(f, "more"),
            ContrastPreference::Less => write!(f, "less"),
            ContrastPreference::Custom => write!(f, "custom"),
        }
    }
}

/// Reduced transparency preference
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReducedTransparency {
    /// No preference for reduced transparency
    NoPreference,
    /// Reduced transparency preferred
    Reduce,
}

impl ReducedTransparency {
    /// Parses a reduced transparency value
    pub fn parse(s: &str) -> Result<Self, MediaParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "no-preference" => Ok(ReducedTransparency::NoPreference),
            "reduce" => Ok(ReducedTransparency::Reduce),
            _ => Err(MediaParseError::InvalidReducedTransparency(s)),
        }
    }
}

impl fmt::Display for ReducedTransparency {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReducedTransparency::NoPreference => write!(f, "no-preference"),
            ReducedTransparency::Reduce => write!(f, "reduce"),
        }
    }
}

/// Reduced data preference
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReducedData {
    /// No preference for reduced data
    NoPreference,
    /// Reduced data preferred
    Reduce,
}

impl ReducedData {
    /// Parses a reduced data value
    pub fn parse(s: &str) -> Result<Self, MediaParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "no-preference" => Ok(ReducedData::NoPreference),
            "reduce" => Ok(ReducedData::Reduce),
            _ => Err(MediaParseError::InvalidReducedData(s)),
        }
    }
}

impl fmt::Display for ReducedData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReducedData::NoPreference => write!(f, "no-preference"),
            ReducedData::Reduce => write!(f, "reduce"),
        }
    }
}

/// Forced colors state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ForcedColors {
    None,
    Active,
}

impl ForcedColors {
    /// Parses a forced-colors value
    pub fn parse(s: &str) -> Result<Self, MediaParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "none" => Ok(ForcedColors::None),
            "active" => Ok(ForcedColors::Active),
            _ => Err(MediaParseError::InvalidForcedColors(s)),
        }
    }
}

impl fmt::Display for ForcedColors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ForcedColors::None => write!(f, "none"),
            ForcedColors::Active => write!(f, "active"),
        }
    }
}

/// Inverted colors state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InvertedColors {
    /// Colors are not inverted
    None,
    /// Colors are inverted
    Inverted,
}

impl InvertedColors {
    /// Parses an inverted-colors value
    pub fn parse(s: &str) -> Result<Self, MediaParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "none" => Ok(InvertedColors::None),
            "inverted" => Ok(InvertedColors::Inverted),
            _ => Err(MediaParseError::InvalidInvertedColors(s)),
        }
    }
}

impl fmt::Display for InvertedColors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InvertedColors::None => write!(f, "none"),
            InvertedColors::Inverted => write!(f, "inverted"),
        }
    }
}

// ============================================================================
// Media Context - Evaluation Environment
// ============================================================================

/// Context for evaluating media queries
///
/// Contains information about the current viewport, device capabilities,
/// and user preferences.
///
/// # Examples
///
/// ```
/// use fastrender::style::media::{MediaContext, MediaType, MediaQuery};
///
/// // Create a context for a desktop browser
/// let ctx = MediaContext::screen(1920.0, 1080.0);
///
/// // Parse and evaluate queries
/// let query = MediaQuery::parse("(min-width: 1200px)").unwrap();
/// assert!(ctx.evaluate(&query));
/// ```
#[derive(Debug, Clone)]
pub struct MediaContext {
    /// Viewport width in CSS pixels
    pub viewport_width: f32,
    /// Viewport height in CSS pixels
    pub viewport_height: f32,
    /// Base font size (px) for resolving font-relative lengths in queries
    pub base_font_size: f32,
    /// Device pixel ratio (DPR)
    pub device_pixel_ratio: f32,
    /// Media type (screen, print, etc.)
    pub media_type: MediaType,
    /// Color depth in bits per color component
    pub color_depth: u32,
    /// Color palette size (0 if not indexed color)
    pub color_index: u32,
    /// Monochrome depth in bits (0 if color device)
    pub monochrome_depth: u32,
    /// Whether primary input can hover
    pub can_hover: bool,
    /// Whether any input can hover
    pub any_can_hover: bool,
    /// Primary pointer accuracy
    pub pointer: PointerCapability,
    /// Any pointer accuracy (finest available)
    pub any_pointer: PointerCapability,
    /// User's color scheme preference
    pub prefers_color_scheme: Option<ColorScheme>,
    /// User's reduced motion preference
    pub prefers_reduced_motion: bool,
    /// User's contrast preference
    pub prefers_contrast: ContrastPreference,
    /// User's reduced transparency preference
    pub prefers_reduced_transparency: bool,
    /// User's reduced data preference
    pub prefers_reduced_data: bool,
    /// Whether the UA is currently in an inverted-color mode
    pub inverted_colors: InvertedColors,
    /// Whether forced-colors mode is active
    pub forced_colors: bool,
}

impl MediaContext {
    /// Creates a default screen context with given dimensions
    ///
    /// Sets reasonable defaults for a desktop browser environment:
    /// - Media type: screen
    /// - 8-bit color depth
    /// - DPR of 1.0
    /// - Fine pointer with hover capability
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::media::MediaContext;
    ///
    /// let ctx = MediaContext::screen(1024.0, 768.0);
    /// assert_eq!(ctx.viewport_width, 1024.0);
    /// assert_eq!(ctx.viewport_height, 768.0);
    /// ```
    pub fn screen(width: f32, height: f32) -> Self {
        Self {
            viewport_width: width,
            viewport_height: height,
            base_font_size: 16.0,
            device_pixel_ratio: 1.0,
            media_type: MediaType::Screen,
            color_depth: 8,
            color_index: 0,
            monochrome_depth: 0,
            can_hover: true,
            any_can_hover: true,
            pointer: PointerCapability::Fine,
            any_pointer: PointerCapability::Fine,
            prefers_color_scheme: None,
            prefers_reduced_motion: false,
            prefers_contrast: ContrastPreference::NoPreference,
            prefers_reduced_transparency: false,
            prefers_reduced_data: false,
            inverted_colors: InvertedColors::None,
            forced_colors: false,
        }
    }

    /// Applies user/environment overrides for media preferences.
    ///
    /// Recognized environment variables:
    /// - `FASTR_PREFERS_COLOR_SCHEME` = `light` | `dark` | `no-preference`
    /// - `FASTR_PREFERS_REDUCED_MOTION` = `reduce` | `no-preference` | truthy/falsy
    /// - `FASTR_PREFERS_CONTRAST` = `more`/`high` | `less`/`low` | `custom`/`forced` | `no-preference`
    /// - `FASTR_PREFERS_REDUCED_TRANSPARENCY` = `reduce` | `no-preference` | truthy/falsy
    /// - `FASTR_PREFERS_REDUCED_DATA` = `reduce` | `no-preference` | truthy/falsy
    /// - `FASTR_INVERTED_COLORS` = `inverted` | `none` | truthy/falsy
    /// - `FASTR_FORCED_COLORS` = `active` | `none` | truthy/falsy
    /// - `FASTR_COLOR_DEPTH` = integer bits per color channel (e.g., 8)
    /// - `FASTR_COLOR_INDEX` = integer palette size (e.g., 256)
    /// - `FASTR_MONOCHROME_DEPTH` = integer bits for monochrome devices (e.g., 1)
    pub fn with_env_overrides(mut self) -> Self {
        if let Ok(value) = env::var("FASTR_PREFERS_COLOR_SCHEME") {
            let v = value.trim().to_ascii_lowercase();
            self.prefers_color_scheme = match v.as_str() {
                "light" => Some(ColorScheme::Light),
                "dark" => Some(ColorScheme::Dark),
                "no-preference" => None,
                _ => self.prefers_color_scheme,
            };
        }

        if let Ok(value) = env::var("FASTR_PREFERS_REDUCED_MOTION") {
            let v = value.trim().to_ascii_lowercase();
            self.prefers_reduced_motion = matches!(
                v.as_str(),
                "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
            ) || matches!(ReducedMotion::parse(&v), Ok(ReducedMotion::Reduce));
        }

        if let Ok(value) = env::var("FASTR_PREFERS_CONTRAST") {
            if let Ok(pref) = ContrastPreference::parse(&value) {
                self.prefers_contrast = pref;
            }
        }

        if let Ok(value) = env::var("FASTR_PREFERS_REDUCED_TRANSPARENCY") {
            let v = value.trim().to_ascii_lowercase();
            self.prefers_reduced_transparency =
                matches!(
                    v.as_str(),
                    "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
                ) || matches!(ReducedTransparency::parse(&v), Ok(ReducedTransparency::Reduce));
        }

        if let Ok(value) = env::var("FASTR_PREFERS_REDUCED_DATA") {
            let v = value.trim().to_ascii_lowercase();
            self.prefers_reduced_data = matches!(
                v.as_str(),
                "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
            ) || matches!(ReducedData::parse(&v), Ok(ReducedData::Reduce));
        }

        if let Ok(value) = env::var("FASTR_INVERTED_COLORS") {
            let v = value.trim().to_ascii_lowercase();
            self.inverted_colors = match InvertedColors::parse(&v) {
                Ok(state) => state,
                Err(_) if matches!(v.as_str(), "1" | "true" | "yes" | "on") => {
                    InvertedColors::Inverted
                }
                Err(_) if matches!(v.as_str(), "0" | "false" | "off" | "none") => {
                    InvertedColors::None
                }
                _ => self.inverted_colors,
            };
        }

        if let Ok(value) = env::var("FASTR_FORCED_COLORS") {
            let v = value.trim().to_ascii_lowercase();
            self.forced_colors = match ForcedColors::parse(&v) {
                Ok(ForcedColors::Active) => true,
                Ok(ForcedColors::None) => false,
                Err(_) => matches!(v.as_str(), "1" | "true" | "yes" | "on"),
            };
        }

        if let Ok(value) = env::var("FASTR_COLOR_DEPTH") {
            if let Ok(bits) = value.trim().parse::<u32>() {
                self.color_depth = bits;
            }
        }

        if let Ok(value) = env::var("FASTR_COLOR_INDEX") {
            if let Ok(count) = value.trim().parse::<u32>() {
                self.color_index = count;
            }
        }

        if let Ok(value) = env::var("FASTR_MONOCHROME_DEPTH") {
            if let Ok(bits) = value.trim().parse::<u32>() {
                self.monochrome_depth = bits;
            }
        }

        self
    }

    /// Returns a new context with the given device pixel ratio.
    pub fn with_device_pixel_ratio(mut self, dpr: f32) -> Self {
        self.device_pixel_ratio = if dpr.is_finite() && dpr > 0.0 { dpr } else { 1.0 };
        self
    }

    /// Creates a print context with given dimensions
    ///
    /// Sets defaults for printing:
    /// - Media type: print
    /// - No hover capability
    /// - No pointer
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::media::{MediaContext, MediaType};
    ///
    /// let ctx = MediaContext::print(8.5 * 96.0, 11.0 * 96.0); // US Letter at 96 DPI
    /// assert_eq!(ctx.media_type, MediaType::Print);
    /// ```
    pub fn print(width: f32, height: f32) -> Self {
        Self {
            viewport_width: width,
            viewport_height: height,
            base_font_size: 16.0,
            device_pixel_ratio: 1.0,
            media_type: MediaType::Print,
            color_depth: 8, // Most printers can do color
            color_index: 0,
            monochrome_depth: 0,
            can_hover: false,
            any_can_hover: false,
            pointer: PointerCapability::None,
            any_pointer: PointerCapability::None,
            prefers_color_scheme: None,
            prefers_reduced_motion: false,
            prefers_contrast: ContrastPreference::NoPreference,
            prefers_reduced_transparency: false,
            prefers_reduced_data: false,
            inverted_colors: InvertedColors::None,
            forced_colors: false,
        }
    }

    /// Creates a mobile screen context
    ///
    /// Sets defaults for a touch-based mobile device:
    /// - Coarse pointer
    /// - No hover capability
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::media::{MediaContext, PointerCapability};
    ///
    /// let ctx = MediaContext::mobile(375.0, 667.0); // iPhone SE
    /// assert_eq!(ctx.pointer, PointerCapability::Coarse);
    /// assert!(!ctx.can_hover);
    /// ```
    pub fn mobile(width: f32, height: f32) -> Self {
        Self {
            viewport_width: width,
            viewport_height: height,
            base_font_size: 16.0,
            device_pixel_ratio: 2.0, // Common for mobile
            media_type: MediaType::Screen,
            color_depth: 8,
            color_index: 0,
            monochrome_depth: 0,
            can_hover: false,
            any_can_hover: false,
            pointer: PointerCapability::Coarse,
            any_pointer: PointerCapability::Coarse,
            prefers_color_scheme: None,
            prefers_reduced_motion: false,
            prefers_contrast: ContrastPreference::NoPreference,
            prefers_reduced_transparency: false,
            prefers_reduced_data: false,
            inverted_colors: InvertedColors::None,
            forced_colors: false,
        }
    }

    /// Sets the device pixel ratio
    pub fn with_dpr(self, dpr: f32) -> Self {
        self.with_device_pixel_ratio(dpr)
    }

    /// Sets the color scheme preference
    pub fn with_color_scheme(mut self, scheme: ColorScheme) -> Self {
        self.prefers_color_scheme = Some(scheme);
        self
    }

    /// Sets the reduced motion preference
    pub fn with_reduced_motion(mut self, reduce: bool) -> Self {
        self.prefers_reduced_motion = reduce;
        self
    }

    /// Sets the reduced data preference
    pub fn with_reduced_data(mut self, reduce: bool) -> Self {
        self.prefers_reduced_data = reduce;
        self
    }

    /// Sets the color depth (bits per color component)
    pub fn with_color_depth(mut self, bits: u32) -> Self {
        self.color_depth = bits;
        self
    }

    /// Sets the color index size
    pub fn with_color_index(mut self, count: u32) -> Self {
        self.color_index = count;
        self
    }

    /// Sets the monochrome depth (bits)
    pub fn with_monochrome_depth(mut self, bits: u32) -> Self {
        self.monochrome_depth = bits;
        self
    }

    /// Sets forced-colors state
    pub fn with_forced_colors(mut self, forced: bool) -> Self {
        self.forced_colors = forced;
        self
    }

    /// Sets whether colors are inverted
    pub fn with_inverted_colors(mut self, inverted: InvertedColors) -> Self {
        self.inverted_colors = inverted;
        self
    }

    /// Evaluates a media query list (OR logic)
    ///
    /// Returns true if ANY query in the list matches.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::media::{MediaQuery, MediaContext};
    ///
    /// let ctx = MediaContext::screen(1024.0, 768.0);
    /// let queries = MediaQuery::parse_list("print, (min-width: 768px)").unwrap();
    /// assert!(ctx.evaluate_list(&queries)); // min-width matches
    /// ```
    pub fn evaluate_list(&self, queries: &[MediaQuery]) -> bool {
        queries.iter().any(|q| self.evaluate(q))
    }

    /// Evaluates a single media query
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::media::{MediaQuery, MediaContext};
    ///
    /// let ctx = MediaContext::screen(1024.0, 768.0);
    /// let query = MediaQuery::parse("(min-width: 768px)").unwrap();
    /// assert!(ctx.evaluate(&query));
    /// ```
    pub fn evaluate(&self, query: &MediaQuery) -> bool {
        // Check media type first
        let type_matches = match query.media_type {
            Some(media_type) => self.matches_media_type(media_type),
            None => true, // No media type means "all"
        };

        // If type doesn't match, apply modifier and return
        if !type_matches {
            return match query.modifier {
                Some(MediaModifier::Not) => true, // NOT (non-matching) = true
                _ => false,
            };
        }

        // Check all features (AND logic)
        let features_match = query.features.iter().all(|f| self.evaluate_feature(f));

        // Apply modifier
        match query.modifier {
            Some(MediaModifier::Not) => !features_match,
            Some(MediaModifier::Only) => features_match, // 'only' is just for old browser compat
            None => features_match,
        }
    }

    fn matches_media_type(&self, media_type: MediaType) -> bool {
        media_type == MediaType::All || media_type == self.media_type
    }

    fn evaluate_feature(&self, feature: &MediaFeature) -> bool {
        match feature {
            // Width features
            MediaFeature::Width(length) => {
                let target = self.resolve_length(length);
                (self.viewport_width - target).abs() < 0.5
            }
            MediaFeature::MinWidth(length) => {
                let target = self.resolve_length(length);
                self.viewport_width >= target
            }
            MediaFeature::MaxWidth(length) => {
                let target = self.resolve_length(length);
                self.viewport_width <= target
            }

            // Inline-size (alias of width for container queries)
            MediaFeature::InlineSize(length) => {
                let target = self.resolve_length(length);
                (self.viewport_width - target).abs() < 0.5
            }
            MediaFeature::MinInlineSize(length) => {
                let target = self.resolve_length(length);
                self.viewport_width >= target
            }
            MediaFeature::MaxInlineSize(length) => {
                let target = self.resolve_length(length);
                self.viewport_width <= target
            }

            // Height features
            MediaFeature::Height(length) => {
                let target = self.resolve_length(length);
                (self.viewport_height - target).abs() < 0.5
            }
            MediaFeature::MinHeight(length) => {
                let target = self.resolve_length(length);
                self.viewport_height >= target
            }
            MediaFeature::MaxHeight(length) => {
                let target = self.resolve_length(length);
                self.viewport_height <= target
            }

            // Block-size (alias of height for container queries)
            MediaFeature::BlockSize(length) => {
                let target = self.resolve_length(length);
                (self.viewport_height - target).abs() < 0.5
            }
            MediaFeature::MinBlockSize(length) => {
                let target = self.resolve_length(length);
                self.viewport_height >= target
            }
            MediaFeature::MaxBlockSize(length) => {
                let target = self.resolve_length(length);
                self.viewport_height <= target
            }

            // Orientation
            MediaFeature::Orientation(orientation) => {
                let is_portrait = self.viewport_height >= self.viewport_width;
                match orientation {
                    Orientation::Portrait => is_portrait,
                    Orientation::Landscape => !is_portrait,
                }
            }

            // Aspect ratio
            MediaFeature::AspectRatio { width, height } => {
                let target_ratio = *width as f32 / *height as f32;
                let actual_ratio = self.viewport_width / self.viewport_height;
                (target_ratio - actual_ratio).abs() < 0.01
            }
            MediaFeature::MinAspectRatio { width, height } => {
                let target_ratio = *width as f32 / *height as f32;
                let actual_ratio = self.viewport_width / self.viewport_height;
                actual_ratio >= target_ratio
            }
            MediaFeature::MaxAspectRatio { width, height } => {
                let target_ratio = *width as f32 / *height as f32;
                let actual_ratio = self.viewport_width / self.viewport_height;
                actual_ratio <= target_ratio
            }

            // Resolution
            MediaFeature::Resolution(res) => {
                let target_dppx = res.to_dppx();
                (self.device_pixel_ratio - target_dppx).abs() < 0.1
            }
            MediaFeature::MinResolution(res) => {
                let target_dppx = res.to_dppx();
                self.device_pixel_ratio >= target_dppx
            }
            MediaFeature::MaxResolution(res) => {
                let target_dppx = res.to_dppx();
                self.device_pixel_ratio <= target_dppx
            }

            // Color features
            MediaFeature::Color => self.color_depth > 0,
            MediaFeature::MinColor(bits) => self.color_depth >= *bits,
            MediaFeature::MaxColor(bits) => self.color_depth <= *bits,
            MediaFeature::ColorIndex => self.color_index > 0,
            MediaFeature::MinColorIndex(count) => self.color_index >= *count,
            MediaFeature::MaxColorIndex(count) => self.color_index <= *count,

            // Monochrome features
            MediaFeature::Monochrome => self.monochrome_depth > 0,
            MediaFeature::MinMonochrome(bits) => self.monochrome_depth >= *bits,
            MediaFeature::MaxMonochrome(bits) => self.monochrome_depth <= *bits,

            // Hover capability
            MediaFeature::Hover(capability) => match capability {
                HoverCapability::None => !self.can_hover,
                HoverCapability::Hover => self.can_hover,
            },
            MediaFeature::AnyHover(capability) => match capability {
                HoverCapability::None => !self.any_can_hover,
                HoverCapability::Hover => self.any_can_hover,
            },

            // Pointer capability
            MediaFeature::Pointer(capability) => self.pointer == *capability,
            MediaFeature::AnyPointer(capability) => {
                // any-pointer matches if any input device has this capability
                // For simplicity, check if any_pointer is at least as fine
                match (self.any_pointer, capability) {
                    (PointerCapability::Fine, PointerCapability::Fine) => true,
                    (PointerCapability::Fine, PointerCapability::Coarse) => true,
                    (PointerCapability::Coarse, PointerCapability::Coarse) => true,
                    (_, PointerCapability::None) => self.any_pointer == PointerCapability::None,
                    _ => self.any_pointer == *capability,
                }
            }

            // User preferences
            MediaFeature::PrefersColorScheme(scheme) => self.prefers_color_scheme == Some(*scheme),
            MediaFeature::PrefersReducedMotion(motion) => match motion {
                ReducedMotion::NoPreference => !self.prefers_reduced_motion,
                ReducedMotion::Reduce => self.prefers_reduced_motion,
            },
            MediaFeature::PrefersContrast(contrast) => self.prefers_contrast == *contrast,
            MediaFeature::PrefersReducedTransparency(transparency) => match transparency {
                ReducedTransparency::NoPreference => !self.prefers_reduced_transparency,
                ReducedTransparency::Reduce => self.prefers_reduced_transparency,
            },
            MediaFeature::PrefersReducedData(data) => match data {
                ReducedData::NoPreference => !self.prefers_reduced_data,
                ReducedData::Reduce => self.prefers_reduced_data,
            },
            MediaFeature::ForcedColors(state) => match state {
                ForcedColors::None => !self.forced_colors,
                ForcedColors::Active => self.forced_colors,
            },
            MediaFeature::InvertedColors(state) => match state {
                InvertedColors::None => matches!(self.inverted_colors, InvertedColors::None),
                InvertedColors::Inverted => matches!(self.inverted_colors, InvertedColors::Inverted),
            },
            MediaFeature::Range { feature, op, value } => match (feature, value) {
                (RangeFeature::Width, RangeValue::Length(len)) => {
                    let target = self.resolve_length(len);
                    compare_with_op(*op, self.viewport_width, target)
                }
                (RangeFeature::InlineSize, RangeValue::Length(len)) => {
                    let target = self.resolve_length(len);
                    compare_with_op(*op, self.viewport_width, target)
                }
                (RangeFeature::BlockSize, RangeValue::Length(len)) => {
                    let target = self.resolve_length(len);
                    compare_with_op(*op, self.viewport_height, target)
                }
                (RangeFeature::Height, RangeValue::Length(len)) => {
                    let target = self.resolve_length(len);
                    compare_with_op(*op, self.viewport_height, target)
                }
                (RangeFeature::AspectRatio, RangeValue::AspectRatio(w, h)) => {
                    let target_ratio = *w as f32 / *h as f32;
                    let actual_ratio = self.viewport_width / self.viewport_height;
                    compare_with_op(*op, actual_ratio, target_ratio)
                }
                (RangeFeature::Resolution, RangeValue::Resolution(res)) => {
                    let target_dppx = res.to_dppx();
                    compare_with_op(*op, self.device_pixel_ratio, target_dppx)
                }
                _ => false,
            },
        }
    }

    fn resolve_length(&self, length: &Length) -> f32 {
        // For media/container queries, resolve lengths to pixels using the
        // context viewport and base font size (em/rem derived from the query context).
        use crate::style::values::LengthUnit;

        let base_font = if self.base_font_size.is_finite() && self.base_font_size > 0.0 {
            self.base_font_size
        } else {
            16.0
        };

        if length.unit == LengthUnit::Calc {
            return length
                .resolve_with_context(
                    Some(self.viewport_width),
                    self.viewport_width,
                    self.viewport_height,
                    base_font,
                    base_font,
                )
                .unwrap_or(0.0);
        }
        match length.unit {
            LengthUnit::Px => length.value,
            LengthUnit::Em => length.value * base_font,
            LengthUnit::Rem => length.value * base_font,
            LengthUnit::Percent => length.value / 100.0 * self.viewport_width,
            LengthUnit::Vw => length.value / 100.0 * self.viewport_width,
            LengthUnit::Vh => length.value / 100.0 * self.viewport_height,
            LengthUnit::Vmin => {
                let min_dimension = self.viewport_width.min(self.viewport_height);
                length.value / 100.0 * min_dimension
            }
            LengthUnit::Vmax => {
                let max_dimension = self.viewport_width.max(self.viewport_height);
                length.value / 100.0 * max_dimension
            }
            LengthUnit::Pt => length.value * 96.0 / 72.0,
            LengthUnit::Cm => length.value * 96.0 / 2.54,
            LengthUnit::Mm => length.value * 96.0 / 25.4,
            LengthUnit::Q => length.value * 96.0 / 101.6, // 1Q = 0.25mm = 1/40th cm
            LengthUnit::In => length.value * 96.0,
            LengthUnit::Pc => length.value * 16.0,
            LengthUnit::Ex => length.value * (base_font * 0.5), // Approximate: half of em
            LengthUnit::Ch => length.value * (base_font * 0.5), // Approximate: width of '0'
            LengthUnit::Calc => 0.0,
        }
    }
}

fn compare_with_op(op: ComparisonOp, actual: f32, target: f32) -> bool {
    // Allow a small tolerance for equality to smooth out float noise.
    let eps = 1e-6;
    match op {
        ComparisonOp::LessThan => actual < target - eps,
        ComparisonOp::LessThanEqual => actual <= target + eps,
        ComparisonOp::GreaterThan => actual > target + eps,
        ComparisonOp::GreaterThanEqual => actual >= target - eps,
        ComparisonOp::Equal => (actual - target).abs() <= eps,
    }
}

impl Default for MediaContext {
    fn default() -> Self {
        Self::screen(1024.0, 768.0)
    }
}

// ============================================================================
// Media Query Parser
// ============================================================================

/// Parser for media queries
struct MediaQueryParser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> MediaQueryParser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn parse_query_list(&mut self) -> Result<Vec<MediaQuery>, MediaParseError> {
        let mut queries = Vec::new();

        loop {
            self.skip_whitespace();
            let query = self.parse_query()?;
            queries.push(query);

            self.skip_whitespace();
            if self.peek() == Some(',') {
                self.advance();
            } else {
                break;
            }
        }

        Ok(queries)
    }

    fn parse_query(&mut self) -> Result<MediaQuery, MediaParseError> {
        self.skip_whitespace();

        let mut modifier = None;
        let mut media_type = None;
        let mut features = Vec::new();

        // Check for 'not' or 'only'
        if let Some(ident) = self.peek_ident() {
            match ident.to_lowercase().as_str() {
                "not" => {
                    self.parse_ident();
                    modifier = Some(MediaModifier::Not);
                    self.skip_whitespace();
                }
                "only" => {
                    self.parse_ident();
                    modifier = Some(MediaModifier::Only);
                    self.skip_whitespace();
                }
                _ => {}
            }
        }

        // Check for media type
        if let Some(ident) = self.peek_ident() {
            if let Ok(mt) = MediaType::parse(&ident) {
                self.parse_ident();
                media_type = Some(mt);
                self.skip_whitespace();
            }
        }

        // Parse media features
        loop {
            self.skip_whitespace();

            // Check for 'and'
            if media_type.is_some() || !features.is_empty() {
                if let Some(ident) = self.peek_ident() {
                    if ident.to_lowercase() == "and" {
                        self.parse_ident();
                        self.skip_whitespace();
                    } else {
                        break;
                    }
                } else if self.peek() != Some('(') {
                    break;
                }
            }

            // Parse feature: (name: value) or (range syntax) or (name)
            if self.peek() == Some('(') {
                let feature_list = self.parse_feature()?;
                features.extend(feature_list);
            } else {
                break;
            }
        }

        // At least one of media_type or features must be present
        if media_type.is_none() && features.is_empty() && modifier.is_none() {
            return Err(MediaParseError::EmptyQuery);
        }

        Ok(MediaQuery {
            media_type,
            modifier,
            features,
        })
    }

    fn parse_feature(&mut self) -> Result<Vec<MediaFeature>, MediaParseError> {
        // Consume '('
        if self.peek() != Some('(') {
            return Err(MediaParseError::ExpectedOpenParen);
        }
        self.advance();
        self.skip_whitespace();

        // Look ahead to the closing ')' and attempt level-4 range syntax first.
        if let Some(close_idx) = self.input[self.pos..].find(')') {
            let inner = &self.input[self.pos..self.pos + close_idx];
            if inner.contains('<') || inner.contains('>') || inner.contains('=') {
                if let Some(result) = Self::parse_range_feature_expr(inner.trim()) {
                    self.pos += close_idx;
                    if self.peek() != Some(')') {
                        return Err(MediaParseError::ExpectedCloseParen);
                    }
                    self.advance();
                    return result;
                }
            }
        }

        // Parse feature name
        let name = self.parse_ident().ok_or(MediaParseError::ExpectedFeatureName)?;
        self.skip_whitespace();

        // Check if there's a value
        let value = if self.peek() == Some(':') {
            self.advance();
            self.skip_whitespace();

            // Parse value until ')'
            let value_start = self.pos;
            while self.peek() != Some(')') && !self.is_eof() {
                self.advance();
            }
            let value = self.input[value_start..self.pos].trim();
            Some(value)
        } else {
            None
        };

        self.skip_whitespace();

        // Consume ')'
        if self.peek() != Some(')') {
            return Err(MediaParseError::ExpectedCloseParen);
        }
        self.advance();

        MediaFeature::parse(&name, value).map(|f| vec![f])
    }

    fn parse_range_feature_expr(input: &str) -> Option<Result<Vec<MediaFeature>, MediaParseError>> {
        #[derive(Debug)]
        enum RangeToken {
            Atom(String),
            Op(ComparisonOp),
        }

        let mut tokens = Vec::new();
        let mut buf = String::new();
        let mut chars = input.chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                '<' | '>' => {
                    if !buf.trim().is_empty() {
                        tokens.push(RangeToken::Atom(buf.trim().to_string()));
                    }
                    buf.clear();
                    let op = if let Some('=') = chars.peek() {
                        let _ = chars.next();
                        if c == '<' {
                            ComparisonOp::LessThanEqual
                        } else {
                            ComparisonOp::GreaterThanEqual
                        }
                    } else if c == '<' {
                        ComparisonOp::LessThan
                    } else {
                        ComparisonOp::GreaterThan
                    };
                    tokens.push(RangeToken::Op(op));
                }
                '=' => {
                    if !buf.trim().is_empty() {
                        tokens.push(RangeToken::Atom(buf.trim().to_string()));
                    }
                    buf.clear();
                    tokens.push(RangeToken::Op(ComparisonOp::Equal));
                }
                other if other.is_whitespace() => {
                    if !buf.trim().is_empty() {
                        tokens.push(RangeToken::Atom(buf.trim().to_string()));
                        buf.clear();
                    }
                }
                _ => buf.push(c),
            }
        }
        if !buf.trim().is_empty() {
            tokens.push(RangeToken::Atom(buf.trim().to_string()));
        }

        if !tokens.iter().any(|t| matches!(t, RangeToken::Op(_))) {
            return None;
        }

        let parse_feature_kind = |name: &str| -> Option<RangeFeature> {
            match name.trim().to_ascii_lowercase().as_str() {
                "width" => Some(RangeFeature::Width),
                "inline-size" => Some(RangeFeature::InlineSize),
                "block-size" => Some(RangeFeature::BlockSize),
                "height" => Some(RangeFeature::Height),
                "aspect-ratio" => Some(RangeFeature::AspectRatio),
                "resolution" => Some(RangeFeature::Resolution),
                _ => None,
            }
        };

        let parse_value = |feature: RangeFeature, raw: &str| -> Result<RangeValue, MediaParseError> {
            match feature {
                RangeFeature::Width | RangeFeature::InlineSize | RangeFeature::BlockSize | RangeFeature::Height => {
                    let len = MediaFeature::parse_length_value(
                        match feature {
                            RangeFeature::Width => "width",
                            RangeFeature::InlineSize => "inline-size",
                            RangeFeature::BlockSize => "block-size",
                            RangeFeature::Height => "height",
                            _ => unreachable!(),
                        },
                        Some(raw),
                    )?;
                    Ok(RangeValue::Length(len))
                }
                RangeFeature::AspectRatio => {
                    let (w, h) = MediaFeature::parse_ratio_value("aspect-ratio", Some(raw))?;
                    Ok(RangeValue::AspectRatio(w, h))
                }
                RangeFeature::Resolution => {
                    let res = MediaFeature::parse_resolution_value("resolution", Some(raw))?;
                    Ok(RangeValue::Resolution(res))
                }
            }
        };

        let build_feature =
            |feature: RangeFeature, op: ComparisonOp, raw: &str| -> Result<MediaFeature, MediaParseError> {
                let value = parse_value(feature, raw)?;
                Ok(MediaFeature::Range { feature, op, value })
            };

        let invert_op = |op: ComparisonOp| match op {
            ComparisonOp::LessThan => ComparisonOp::GreaterThan,
            ComparisonOp::LessThanEqual => ComparisonOp::GreaterThanEqual,
            ComparisonOp::GreaterThan => ComparisonOp::LessThan,
            ComparisonOp::GreaterThanEqual => ComparisonOp::LessThanEqual,
            ComparisonOp::Equal => ComparisonOp::Equal,
        };

        match tokens.as_slice() {
            [RangeToken::Atom(a), RangeToken::Op(op), RangeToken::Atom(b)] => {
                // Either `feature <value` or `<value> < feature`
                if let Some(feature) = parse_feature_kind(a) {
                    Some(build_feature(feature, *op, b).map(|f| vec![f]))
                } else if let Some(feature) = parse_feature_kind(b) {
                    let flipped = invert_op(*op);
                    Some(build_feature(feature, flipped, a).map(|f| vec![f]))
                } else {
                    Some(Err(MediaParseError::InvalidValue {
                        feature: "range".to_string(),
                        value: input.to_string(),
                    }))
                }
            }
            [RangeToken::Atom(a), RangeToken::Op(op1), RangeToken::Atom(center), RangeToken::Op(op2), RangeToken::Atom(b)] => {
                if let Some(feature) = parse_feature_kind(center) {
                    let left_op = invert_op(*op1);
                    let first = build_feature(feature, left_op, a);
                    let second = build_feature(feature, *op2, b);
                    match (first, second) {
                        (Ok(f1), Ok(f2)) => Some(Ok(vec![f1, f2])),
                        (Err(e), _) => Some(Err(e)),
                        (_, Err(e)) => Some(Err(e)),
                    }
                } else {
                    Some(Err(MediaParseError::InvalidValue {
                        feature: "range".to_string(),
                        value: input.to_string(),
                    }))
                }
            }
            _ => Some(Err(MediaParseError::InvalidValue {
                feature: "range".to_string(),
                value: input.to_string(),
            })),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn advance(&mut self) {
        if let Some(c) = self.peek() {
            self.pos += c.len_utf8();
        }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn peek_ident(&self) -> Option<String> {
        let remaining = &self.input[self.pos..];
        let mut chars = remaining.chars().peekable();

        // Identifier must start with letter, underscore, or hyphen
        let first = chars.peek()?;
        if !first.is_alphabetic() && *first != '_' && *first != '-' {
            return None;
        }

        let mut ident = String::new();
        for c in remaining.chars() {
            if c.is_alphanumeric() || c == '-' || c == '_' {
                ident.push(c);
            } else {
                break;
            }
        }

        if ident.is_empty() {
            None
        } else {
            Some(ident)
        }
    }

    fn parse_ident(&mut self) -> Option<String> {
        let ident = self.peek_ident()?;
        self.pos += ident.len();
        Some(ident)
    }
}

// ============================================================================
// Error Types
// ============================================================================

/// Error when parsing media queries
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MediaParseError {
    /// Invalid media type
    InvalidMediaType(String),
    /// Unknown media feature
    UnknownFeature(String),
    /// Missing value for feature
    MissingValue(String),
    /// Invalid value for feature
    InvalidValue { feature: String, value: String },
    /// Invalid orientation value
    InvalidOrientation(String),
    /// Invalid resolution value
    InvalidResolution(String),
    /// Invalid hover capability
    InvalidHoverCapability(String),
    /// Invalid pointer capability
    InvalidPointerCapability(String),
    /// Invalid color scheme
    InvalidColorScheme(String),
    /// Invalid reduced motion value
    InvalidReducedMotion(String),
    /// Invalid contrast preference
    InvalidContrastPreference(String),
    /// Invalid reduced transparency value
    InvalidReducedTransparency(String),
    /// Invalid reduced data value
    InvalidReducedData(String),
    /// Invalid forced-colors value
    InvalidForcedColors(String),
    /// Invalid inverted-colors value
    InvalidInvertedColors(String),
    /// Empty media query
    EmptyQuery,
    /// Expected '(' for feature
    ExpectedOpenParen,
    /// Expected ')' to close feature
    ExpectedCloseParen,
    /// Expected feature name
    ExpectedFeatureName,
}

impl fmt::Display for MediaParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MediaParseError::InvalidMediaType(s) => {
                write!(f, "Invalid media type: '{}'", s)
            }
            MediaParseError::UnknownFeature(s) => {
                write!(f, "Unknown media feature: '{}'", s)
            }
            MediaParseError::MissingValue(s) => {
                write!(f, "Missing value for media feature: '{}'", s)
            }
            MediaParseError::InvalidValue { feature, value } => {
                write!(f, "Invalid value '{}' for media feature '{}'", value, feature)
            }
            MediaParseError::InvalidOrientation(s) => {
                write!(f, "Invalid orientation: '{}' (expected 'portrait' or 'landscape')", s)
            }
            MediaParseError::InvalidResolution(s) => {
                write!(f, "Invalid resolution: '{}' (expected format like '2dppx', '96dpi')", s)
            }
            MediaParseError::InvalidHoverCapability(s) => {
                write!(f, "Invalid hover capability: '{}' (expected 'none' or 'hover')", s)
            }
            MediaParseError::InvalidPointerCapability(s) => {
                write!(
                    f,
                    "Invalid pointer capability: '{}' (expected 'none', 'coarse', or 'fine')",
                    s
                )
            }
            MediaParseError::InvalidColorScheme(s) => {
                write!(f, "Invalid color scheme: '{}' (expected 'light' or 'dark')", s)
            }
            MediaParseError::InvalidReducedMotion(s) => {
                write!(
                    f,
                    "Invalid reduced motion: '{}' (expected 'no-preference' or 'reduce')",
                    s
                )
            }
            MediaParseError::InvalidContrastPreference(s) => {
                write!(
                    f,
                    "Invalid contrast preference: '{}' (expected 'no-preference', 'more', 'less', or 'custom')",
                    s
                )
            }
            MediaParseError::InvalidReducedTransparency(s) => {
                write!(
                    f,
                    "Invalid reduced transparency: '{}' (expected 'no-preference' or 'reduce')",
                    s
                )
            }
            MediaParseError::InvalidReducedData(s) => {
                write!(
                    f,
                    "Invalid reduced data: '{}' (expected 'no-preference' or 'reduce')",
                    s
                )
            }
            MediaParseError::InvalidForcedColors(s) => {
                write!(f, "Invalid forced-colors: '{}' (expected 'none' or 'active')", s)
            }
            MediaParseError::InvalidInvertedColors(s) => {
                write!(
                    f,
                    "Invalid inverted-colors: '{}' (expected 'none' or 'inverted')",
                    s
                )
            }
            MediaParseError::EmptyQuery => {
                write!(f, "Empty media query")
            }
            MediaParseError::ExpectedOpenParen => {
                write!(f, "Expected '(' for media feature")
            }
            MediaParseError::ExpectedCloseParen => {
                write!(f, "Expected ')' to close media feature")
            }
            MediaParseError::ExpectedFeatureName => {
                write!(f, "Expected media feature name")
            }
        }
    }
}

impl std::error::Error for MediaParseError {}

// ============================================================================
// Helper Functions
// ============================================================================

/// Parse a CSS length value
fn parse_length(s: &str) -> Option<Length> {
    use crate::style::values::LengthUnit;

    let s = s.trim();

    // Handle zero without unit
    if s == "0" {
        return Some(Length::px(0.0));
    }

    // Try various units (longer suffixes first to avoid matching substrings)
    let units: &[(&str, fn(f32) -> Length)] = &[
        ("rem", |v| Length::rem(v)),
        ("vmin", |v| Length::new(v, LengthUnit::Vmin)),
        ("vmax", |v| Length::new(v, LengthUnit::Vmax)),
        ("px", |v| Length::px(v)),
        ("em", |v| Length::em(v)),
        ("%", |v| Length::percent(v)),
        ("vw", |v| Length::new(v, LengthUnit::Vw)),
        ("vh", |v| Length::new(v, LengthUnit::Vh)),
        ("pt", |v| Length::pt(v)),
        ("cm", |v| Length::cm(v)),
        ("mm", |v| Length::mm(v)),
        ("in", |v| Length::inches(v)),
        ("pc", |v| Length::pc(v)),
        ("ex", |v| Length::ex(v)),
        ("ch", |v| Length::ch(v)),
    ];

    for (suffix, constructor) in units {
        if let Some(value_str) = s.strip_suffix(suffix) {
            if let Ok(value) = value_str.trim().parse::<f32>() {
                return Some(constructor(value));
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    struct EnvGuard {
        key: &'static str,
        old: Option<String>,
        _lock: Option<std::sync::MutexGuard<'static, ()>>,
    }

    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
        LOCK.get_or_init(std::sync::Mutex::default).lock().unwrap()
    }

    impl EnvGuard {
        fn new(key: &'static str, value: Option<&str>) -> Self {
            thread_local! {
                static DEPTH: std::cell::Cell<u32> = std::cell::Cell::new(0);
            }

            let mut lock = None;
            DEPTH.with(|depth| {
                if depth.get() == 0 {
                    lock = Some(env_lock());
                }
                depth.set(depth.get().saturating_add(1));
            });

            let old = env::var(key).ok();
            match value {
                Some(v) => env::set_var(key, v),
                None => env::remove_var(key),
            }
            EnvGuard { key, old, _lock: lock }
        }
    }

    impl Drop for EnvGuard {
        fn drop(&mut self) {
            thread_local! {
                static DEPTH: std::cell::Cell<u32> = std::cell::Cell::new(0);
            }

            DEPTH.with(|depth| {
                let current = depth.get();
                depth.set(current.saturating_sub(1));
            });

            if let Some(ref val) = self.old {
                env::set_var(self.key, val);
            } else {
                env::remove_var(self.key);
            }
        }
    }

    // ============================================================================
    // MediaType Tests
    // ============================================================================

    #[test]
    fn test_media_type_parse() {
        assert_eq!(MediaType::parse("all").unwrap(), MediaType::All);
        assert_eq!(MediaType::parse("screen").unwrap(), MediaType::Screen);
        assert_eq!(MediaType::parse("print").unwrap(), MediaType::Print);
        assert_eq!(MediaType::parse("speech").unwrap(), MediaType::Speech);
    }

    #[test]
    fn test_media_type_case_insensitive() {
        assert_eq!(MediaType::parse("SCREEN").unwrap(), MediaType::Screen);
        assert_eq!(MediaType::parse("Print").unwrap(), MediaType::Print);
    }

    #[test]
    fn test_media_type_invalid() {
        assert!(MediaType::parse("invalid").is_err());
        assert!(MediaType::parse("").is_err());
    }

    // ============================================================================
    // Orientation Tests
    // ============================================================================

    #[test]
    fn test_orientation_parse() {
        assert_eq!(Orientation::parse("portrait").unwrap(), Orientation::Portrait);
        assert_eq!(Orientation::parse("landscape").unwrap(), Orientation::Landscape);
    }

    #[test]
    fn test_orientation_case_insensitive() {
        assert_eq!(Orientation::parse("PORTRAIT").unwrap(), Orientation::Portrait);
        assert_eq!(Orientation::parse("Landscape").unwrap(), Orientation::Landscape);
    }

    // ============================================================================
    // Resolution Tests
    // ============================================================================

    #[test]
    fn test_resolution_parse() {
        let res = Resolution::parse("2dppx").unwrap();
        assert_eq!(res.value, 2.0);
        assert_eq!(res.unit, ResolutionUnit::Dppx);

        let res = Resolution::parse("96dpi").unwrap();
        assert_eq!(res.value, 96.0);
        assert_eq!(res.unit, ResolutionUnit::Dpi);

        let res = Resolution::parse("2x").unwrap();
        assert_eq!(res.value, 2.0);
        assert_eq!(res.unit, ResolutionUnit::Dppx);
    }

    #[test]
    fn test_resolution_to_dppx() {
        assert_eq!(Resolution::dppx(2.0).to_dppx(), 2.0);
        assert!((Resolution::dpi(96.0).to_dppx() - 1.0).abs() < 0.01);
        assert!((Resolution::dpi(192.0).to_dppx() - 2.0).abs() < 0.01);
    }

    // ============================================================================
    // MediaFeature Tests
    // ============================================================================

    #[test]
    fn test_media_feature_parse_width() {
        let feature = MediaFeature::parse("min-width", Some("768px")).unwrap();
        match feature {
            MediaFeature::MinWidth(length) => assert_eq!(length.value, 768.0),
            _ => panic!("Expected MinWidth"),
        }
    }

    #[test]
    fn test_media_feature_parse_orientation() {
        let feature = MediaFeature::parse("orientation", Some("portrait")).unwrap();
        assert_eq!(feature, MediaFeature::Orientation(Orientation::Portrait));
    }

    #[test]
    fn test_media_feature_parse_color() {
        let feature = MediaFeature::parse("color", None).unwrap();
        assert_eq!(feature, MediaFeature::Color);
    }

    #[test]
    fn test_media_feature_parse_prefers_color_scheme() {
        let feature = MediaFeature::parse("prefers-color-scheme", Some("dark")).unwrap();
        assert_eq!(feature, MediaFeature::PrefersColorScheme(ColorScheme::Dark));
    }

    // ============================================================================
    // MediaQuery Parsing Tests
    // ============================================================================

    #[test]
    fn test_parse_simple_media_type() {
        let query = MediaQuery::parse("screen").unwrap();
        assert_eq!(query.media_type, Some(MediaType::Screen));
        assert!(query.features.is_empty());
    }

    #[test]
    fn test_parse_simple_feature() {
        let query = MediaQuery::parse("(min-width: 768px)").unwrap();
        assert_eq!(query.media_type, None);
        assert_eq!(query.features.len(), 1);
    }

    #[test]
    fn test_parse_type_and_feature() {
        let query = MediaQuery::parse("screen and (min-width: 768px)").unwrap();
        assert_eq!(query.media_type, Some(MediaType::Screen));
        assert_eq!(query.features.len(), 1);
    }

    #[test]
    fn test_parse_multiple_features() {
        let query = MediaQuery::parse("(min-width: 768px) and (max-width: 1024px)").unwrap();
        assert_eq!(query.features.len(), 2);
    }

    #[test]
    fn test_parse_not_modifier() {
        let query = MediaQuery::parse("not screen").unwrap();
        assert_eq!(query.modifier, Some(MediaModifier::Not));
        assert_eq!(query.media_type, Some(MediaType::Screen));
    }

    #[test]
    fn test_parse_only_modifier() {
        let query = MediaQuery::parse("only screen and (color)").unwrap();
        assert_eq!(query.modifier, Some(MediaModifier::Only));
        assert_eq!(query.media_type, Some(MediaType::Screen));
    }

    #[test]
    fn test_parse_query_list() {
        let queries = MediaQuery::parse_list("screen, print").unwrap();
        assert_eq!(queries.len(), 2);
        assert_eq!(queries[0].media_type, Some(MediaType::Screen));
        assert_eq!(queries[1].media_type, Some(MediaType::Print));
    }

    #[test]
    fn test_parse_complex_query_list() {
        let queries = MediaQuery::parse_list("screen and (min-width: 768px), print").unwrap();
        assert_eq!(queries.len(), 2);
        assert_eq!(queries[0].media_type, Some(MediaType::Screen));
        assert_eq!(queries[0].features.len(), 1);
        assert_eq!(queries[1].media_type, Some(MediaType::Print));
    }

    // ============================================================================
    // MediaContext Evaluation Tests
    // ============================================================================

    #[test]
    fn test_evaluate_min_width() {
        let ctx = MediaContext::screen(1024.0, 768.0);

        let query = MediaQuery::parse("(min-width: 768px)").unwrap();
        assert!(ctx.evaluate(&query));

        let query = MediaQuery::parse("(min-width: 1200px)").unwrap();
        assert!(!ctx.evaluate(&query));
    }

    #[test]
    fn test_evaluate_max_width() {
        let ctx = MediaContext::screen(1024.0, 768.0);

        let query = MediaQuery::parse("(max-width: 1200px)").unwrap();
        assert!(ctx.evaluate(&query));

        let query = MediaQuery::parse("(max-width: 800px)").unwrap();
        assert!(!ctx.evaluate(&query));
    }

    #[test]
    fn test_evaluate_orientation_portrait() {
        let ctx = MediaContext::screen(375.0, 667.0); // Portrait

        let query = MediaQuery::parse("(orientation: portrait)").unwrap();
        assert!(ctx.evaluate(&query));

        let query = MediaQuery::parse("(orientation: landscape)").unwrap();
        assert!(!ctx.evaluate(&query));
    }

    #[test]
    fn test_evaluate_orientation_landscape() {
        let ctx = MediaContext::screen(1024.0, 768.0); // Landscape

        let query = MediaQuery::parse("(orientation: landscape)").unwrap();
        assert!(ctx.evaluate(&query));

        let query = MediaQuery::parse("(orientation: portrait)").unwrap();
        assert!(!ctx.evaluate(&query));
    }

    #[test]
    fn test_evaluate_media_type() {
        let screen_ctx = MediaContext::screen(1024.0, 768.0);
        let print_ctx = MediaContext::print(816.0, 1056.0);

        let screen_query = MediaQuery::parse("screen").unwrap();
        assert!(screen_ctx.evaluate(&screen_query));
        assert!(!print_ctx.evaluate(&screen_query));

        let print_query = MediaQuery::parse("print").unwrap();
        assert!(!screen_ctx.evaluate(&print_query));
        assert!(print_ctx.evaluate(&print_query));
    }

    #[test]
    fn test_evaluate_not_modifier() {
        let ctx = MediaContext::screen(1024.0, 768.0);

        let query = MediaQuery::parse("not print").unwrap();
        assert!(ctx.evaluate(&query)); // NOT print = true for screen

        let query = MediaQuery::parse("not screen").unwrap();
        assert!(!ctx.evaluate(&query)); // NOT screen = false for screen
    }

    #[test]
    fn test_evaluate_and_logic() {
        let ctx = MediaContext::screen(1024.0, 768.0);

        // Both conditions true
        let query = MediaQuery::parse("(min-width: 768px) and (max-width: 1200px)").unwrap();
        assert!(ctx.evaluate(&query));

        // First true, second false
        let query = MediaQuery::parse("(min-width: 768px) and (max-width: 800px)").unwrap();
        assert!(!ctx.evaluate(&query));
    }

    #[test]
    fn test_evaluate_or_logic() {
        let ctx = MediaContext::screen(1024.0, 768.0);

        // First matches, second doesn't
        let queries = MediaQuery::parse_list("(min-width: 768px), (max-width: 400px)").unwrap();
        assert!(ctx.evaluate_list(&queries));

        // Neither matches
        let queries = MediaQuery::parse_list("(min-width: 1200px), (max-width: 400px)").unwrap();
        assert!(!ctx.evaluate_list(&queries));
    }

    #[test]
    fn test_evaluate_resolution() {
        let mut ctx = MediaContext::screen(1024.0, 768.0);
        ctx.device_pixel_ratio = 2.0;

        let query = MediaQuery::parse("(min-resolution: 2dppx)").unwrap();
        assert!(ctx.evaluate(&query));

        let query = MediaQuery::parse("(min-resolution: 3dppx)").unwrap();
        assert!(!ctx.evaluate(&query));
    }

    #[test]
    fn test_evaluate_hover() {
        let desktop = MediaContext::screen(1024.0, 768.0);
        let mobile = MediaContext::mobile(375.0, 667.0);

        let query = MediaQuery::parse("(hover: hover)").unwrap();
        assert!(desktop.evaluate(&query));
        assert!(!mobile.evaluate(&query));

        let query = MediaQuery::parse("(hover: none)").unwrap();
        assert!(!desktop.evaluate(&query));
        assert!(mobile.evaluate(&query));
    }

    #[test]
    fn test_evaluate_pointer() {
        let desktop = MediaContext::screen(1024.0, 768.0);
        let mobile = MediaContext::mobile(375.0, 667.0);

        let query = MediaQuery::parse("(pointer: fine)").unwrap();
        assert!(desktop.evaluate(&query));
        assert!(!mobile.evaluate(&query));

        let query = MediaQuery::parse("(pointer: coarse)").unwrap();
        assert!(!desktop.evaluate(&query));
        assert!(mobile.evaluate(&query));
    }

    #[test]
    fn test_evaluate_prefers_color_scheme() {
        let ctx = MediaContext::screen(1024.0, 768.0).with_color_scheme(ColorScheme::Dark);

        let query = MediaQuery::parse("(prefers-color-scheme: dark)").unwrap();
        assert!(ctx.evaluate(&query));

        let query = MediaQuery::parse("(prefers-color-scheme: light)").unwrap();
        assert!(!ctx.evaluate(&query));
    }

    #[test]
    fn test_evaluate_prefers_reduced_motion() {
        let ctx = MediaContext::screen(1024.0, 768.0).with_reduced_motion(true);

        let query = MediaQuery::parse("(prefers-reduced-motion: reduce)").unwrap();
        assert!(ctx.evaluate(&query));

        let query = MediaQuery::parse("(prefers-reduced-motion: no-preference)").unwrap();
        assert!(!ctx.evaluate(&query));
    }

    #[test]
    fn test_evaluate_prefers_reduced_data() {
        let ctx = MediaContext::screen(1024.0, 768.0).with_reduced_data(true);

        let query = MediaQuery::parse("(prefers-reduced-data: reduce)").unwrap();
        assert!(ctx.evaluate(&query));

        let query = MediaQuery::parse("(prefers-reduced-data: no-preference)").unwrap();
        assert!(!ctx.evaluate(&query));
    }

    #[test]
    fn test_evaluate_forced_colors() {
        let ctx = MediaContext::screen(800.0, 600.0);
        let query_none = MediaQuery::parse("(forced-colors: none)").unwrap();
        let query_active = MediaQuery::parse("(forced-colors: active)").unwrap();
        assert!(ctx.evaluate(&query_none));
        assert!(!ctx.evaluate(&query_active));

        let forced = ctx.clone().with_forced_colors(true);
        assert!(forced.evaluate(&query_active));
        assert!(!forced.evaluate(&query_none));
    }

    #[test]
    fn test_evaluate_color_depth_overrides() {
        let ctx = MediaContext::screen(800.0, 600.0)
            .with_color_depth(10)
            .with_color_index(512)
            .with_monochrome_depth(0);

        let color_query = MediaQuery::parse("(min-color: 8)").unwrap();
        assert!(ctx.evaluate(&color_query));
        let color_index_query = MediaQuery::parse("(min-color-index: 256)").unwrap();
        assert!(ctx.evaluate(&color_index_query));

        let mono_query = MediaQuery::parse("(monochrome)").unwrap();
        assert!(!ctx.evaluate(&mono_query));

        let mono_depth = MediaContext::screen(800.0, 600.0).with_monochrome_depth(2);
        assert!(mono_depth.evaluate(&mono_query));
    }

    #[test]
    fn test_evaluate_inverted_colors() {
        let ctx = MediaContext::screen(1024.0, 768.0);

        let query_none = MediaQuery::parse("(inverted-colors: none)").unwrap();
        assert!(ctx.evaluate(&query_none));
        let query_inverted = MediaQuery::parse("(inverted-colors: inverted)").unwrap();
        assert!(!ctx.evaluate(&query_inverted));

        let inverted_ctx = ctx.clone().with_inverted_colors(InvertedColors::Inverted);
        assert!(inverted_ctx.evaluate(&query_inverted));
        assert!(!inverted_ctx.evaluate(&query_none));
    }

    #[test]
    fn test_evaluate_range_width_syntax() {
        let ctx = MediaContext::screen(700.0, 800.0);
        let query = MediaQuery::parse("(400px <= width <= 800px)").unwrap();
        assert!(ctx.evaluate(&query));

        let narrow = MediaContext::screen(399.0, 800.0);
        assert!(!narrow.evaluate(&query));

        let wide = MediaContext::screen(900.0, 800.0);
        assert!(!wide.evaluate(&query));
    }

    #[test]
    fn test_evaluate_range_width_reversed_bounds() {
        let ctx = MediaContext::screen(500.0, 800.0);
        let query = MediaQuery::parse("(800px >= width >= 400px)").unwrap();
        assert!(ctx.evaluate(&query));

        let too_large = MediaContext::screen(900.0, 800.0);
        assert!(!too_large.evaluate(&query));
    }

    #[test]
    fn test_evaluate_range_height_and_aspect_ratio() {
        let ctx = MediaContext::screen(1200.0, 900.0);
        let height_query = MediaQuery::parse("(height > 800px)").unwrap();
        assert!(ctx.evaluate(&height_query));

        let ratio_query = MediaQuery::parse("(aspect-ratio >= 4/3)").unwrap();
        assert!(ctx.evaluate(&ratio_query)); // 1200/900 = 4/3

        let tall_ctx = MediaContext::screen(900.0, 1600.0);
        assert!(!tall_ctx.evaluate(&ratio_query)); // 0.5625 < 4/3
    }

    #[test]
    fn test_evaluate_range_resolution() {
        let mut ctx = MediaContext::screen(1024.0, 768.0);
        ctx.device_pixel_ratio = 1.5;

        let res_query = MediaQuery::parse("(resolution <= 2dppx)").unwrap();
        assert!(ctx.evaluate(&res_query));

        let strict_query = MediaQuery::parse("(resolution > 2dppx)").unwrap();
        assert!(!ctx.evaluate(&strict_query));
    }

    #[test]
    fn test_evaluate_range_equality() {
        let ctx = MediaContext::screen(700.0, 800.0);
        let width_eq = MediaQuery::parse("(width = 700px)").unwrap();
        assert!(ctx.evaluate(&width_eq));

        let width_miss = MediaContext::screen(701.0, 800.0);
        assert!(!width_miss.evaluate(&width_eq));

        let ratio_ctx = MediaContext::screen(1200.0, 900.0); // 4/3
        let ratio_eq = MediaQuery::parse("(aspect-ratio = 4/3)").unwrap();
        assert!(ratio_ctx.evaluate(&ratio_eq));

        let ratio_miss = MediaContext::screen(900.0, 1600.0);
        assert!(!ratio_miss.evaluate(&ratio_eq));
    }

    #[test]
    fn env_overrides_color_scheme_and_motion() {
        let guard_scheme = EnvGuard::new("FASTR_PREFERS_COLOR_SCHEME", Some("dark"));
        let guard_motion = EnvGuard::new("FASTR_PREFERS_REDUCED_MOTION", Some("reduce"));
        let guard_contrast = EnvGuard::new("FASTR_PREFERS_CONTRAST", Some("high"));
        let guard_transparency = EnvGuard::new("FASTR_PREFERS_REDUCED_TRANSPARENCY", Some("1"));
        let guard_data = EnvGuard::new("FASTR_PREFERS_REDUCED_DATA", Some("yes"));
        let guard_inverted = EnvGuard::new("FASTR_INVERTED_COLORS", Some("inverted"));
        let guard_color_depth = EnvGuard::new("FASTR_COLOR_DEPTH", Some("10"));
        let guard_color_index = EnvGuard::new("FASTR_COLOR_INDEX", Some("512"));
        let guard_mono = EnvGuard::new("FASTR_MONOCHROME_DEPTH", Some("0"));
        let guard_forced = EnvGuard::new("FASTR_FORCED_COLORS", Some("active"));

        let ctx = MediaContext::screen(800.0, 600.0).with_env_overrides();
        assert_eq!(ctx.prefers_color_scheme, Some(ColorScheme::Dark));
        assert!(ctx.prefers_reduced_motion);
        assert!(matches!(ctx.prefers_contrast, ContrastPreference::More));
        assert!(ctx.prefers_reduced_transparency);
        assert!(ctx.prefers_reduced_data);
        assert!(matches!(ctx.inverted_colors, InvertedColors::Inverted));
        assert_eq!(ctx.color_depth, 10);
        assert_eq!(ctx.color_index, 512);
        assert_eq!(ctx.monochrome_depth, 0);
        assert!(ctx.forced_colors);

        drop(guard_scheme);
        drop(guard_motion);
        drop(guard_contrast);
        drop(guard_transparency);
        drop(guard_data);
        drop(guard_inverted);
        drop(guard_color_depth);
        drop(guard_color_index);
        drop(guard_mono);
        drop(guard_forced);
    }

    #[test]
    fn env_overrides_ignore_invalid_values() {
        let guard_scheme = EnvGuard::new("FASTR_PREFERS_COLOR_SCHEME", Some("invalid"));
        let guard_inverted = EnvGuard::new("FASTR_INVERTED_COLORS", Some("maybe"));
        let guard_color_depth = EnvGuard::new("FASTR_COLOR_DEPTH", Some("abc"));
        let guard_forced = EnvGuard::new("FASTR_FORCED_COLORS", Some("maybe"));
        let ctx = MediaContext::screen(800.0, 600.0)
            .with_color_scheme(ColorScheme::Light)
            .with_env_overrides();
        assert_eq!(ctx.prefers_color_scheme, Some(ColorScheme::Light));
        assert!(matches!(ctx.inverted_colors, InvertedColors::None));
        assert_eq!(ctx.color_depth, 8);
        assert!(!ctx.forced_colors);
        drop(guard_scheme);
        drop(guard_inverted);
        drop(guard_color_depth);
        drop(guard_forced);
    }

    #[test]
    fn test_evaluate_color() {
        let ctx = MediaContext::screen(1024.0, 768.0);

        let query = MediaQuery::parse("(color)").unwrap();
        assert!(ctx.evaluate(&query));

        let query = MediaQuery::parse("(min-color: 8)").unwrap();
        assert!(ctx.evaluate(&query));

        let query = MediaQuery::parse("(min-color: 16)").unwrap();
        assert!(!ctx.evaluate(&query));
    }

    #[test]
    fn test_evaluate_aspect_ratio() {
        let ctx = MediaContext::screen(1920.0, 1080.0); // 16:9

        let query = MediaQuery::parse("(min-aspect-ratio: 16/9)").unwrap();
        assert!(ctx.evaluate(&query));

        let query = MediaQuery::parse("(max-aspect-ratio: 4/3)").unwrap();
        assert!(!ctx.evaluate(&query));
    }

    // ============================================================================
    // Error Message Tests
    // ============================================================================

    #[test]
    fn test_error_messages() {
        let err = MediaType::parse("invalid").unwrap_err();
        assert!(err.to_string().contains("Invalid media type"));

        let err = MediaQuery::parse("(unknown-feature: value)").unwrap_err();
        assert!(err.to_string().contains("Unknown media feature"));

        let err = MediaQuery::parse("(min-width)").unwrap_err();
        assert!(err.to_string().contains("Missing value"));

        let err = MediaQuery::parse("(orientation: invalid)").unwrap_err();
        assert!(err.to_string().contains("Invalid orientation"));
    }

    // ============================================================================
    // Display Trait Tests
    // ============================================================================

    #[test]
    fn test_display_traits() {
        assert_eq!(format!("{}", MediaType::Screen), "screen");
        assert_eq!(format!("{}", Orientation::Portrait), "portrait");
        assert_eq!(format!("{}", ColorScheme::Dark), "dark");
        assert_eq!(format!("{}", ColorScheme::NoPreference), "no-preference");
        assert_eq!(format!("{}", PointerCapability::Fine), "fine");
    }

    // ============================================================================
    // Default Tests
    // ============================================================================

    #[test]
    fn test_media_query_default() {
        let query = MediaQuery::default();
        assert_eq!(query.media_type, None);
        assert_eq!(query.modifier, None);
        assert!(query.features.is_empty());
    }

    #[test]
    fn test_media_context_default() {
        let ctx = MediaContext::default();
        assert_eq!(ctx.viewport_width, 1024.0);
        assert_eq!(ctx.viewport_height, 768.0);
        assert_eq!(ctx.media_type, MediaType::Screen);
    }
}
