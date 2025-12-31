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
use std::collections::HashMap;
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

  // Device viewport features
  /// Exact device width: `(device-width: 768px)`
  DeviceWidth(Length),
  /// Minimum device width: `(min-device-width: 768px)`
  MinDeviceWidth(Length),
  /// Maximum device width: `(max-device-width: 1024px)`
  MaxDeviceWidth(Length),
  /// Exact device height: `(device-height: 600px)`
  DeviceHeight(Length),
  /// Minimum device height: `(min-device-height: 400px)`
  MinDeviceHeight(Length),
  /// Maximum device height: `(max-device-height: 900px)`
  MaxDeviceHeight(Length),

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
  /// Exact device aspect ratio: `(device-aspect-ratio: 16/9)`
  DeviceAspectRatio { width: u32, height: u32 },
  /// Minimum device aspect ratio: `(min-device-aspect-ratio: 16/9)`
  MinDeviceAspectRatio { width: u32, height: u32 },
  /// Maximum device aspect ratio: `(max-device-aspect-ratio: 16/9)`
  MaxDeviceAspectRatio { width: u32, height: u32 },

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

  // Media Queries Level 5
  /// Whether scripting is available: `(scripting: none|initial-only|enabled)`
  Scripting(Scripting),
  /// How frequently the output is updated: `(update: fast|slow|none)`
  Update(UpdateFrequency),
  /// Ambient light level: `(light-level: dim|normal|washed)`
  LightLevel(LightLevel),
  /// Document display mode (PWA): `(display-mode: fullscreen)`
  DisplayMode(DisplayMode),

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
  /// Color gamut support: `(color-gamut: srgb | p3 | rec2020)`
  ColorGamut(ColorGamut),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ComparisonOp {
  LessThan,
  LessThanEqual,
  GreaterThan,
  GreaterThanEqual,
  Equal,
}

/// Which media dimension/value a range comparison targets
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RangeFeature {
  Width,
  InlineSize,
  BlockSize,
  Height,
  AspectRatio,
  DeviceWidth,
  DeviceHeight,
  DeviceAspectRatio,
  Resolution,
}

/// Value for a range comparison
#[derive(Debug, Clone, PartialEq)]
pub enum RangeValue {
  Length(Length),
  AspectRatio(u32, u32),
  Resolution(Resolution),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MediaQueryKey {
  media_type: Option<MediaType>,
  modifier: Option<MediaModifier>,
  features: Vec<MediaFeatureKey>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum MediaFeatureKey {
  Width(LengthKey),
  MinWidth(LengthKey),
  MaxWidth(LengthKey),
  InlineSize(LengthKey),
  MinInlineSize(LengthKey),
  MaxInlineSize(LengthKey),
  Height(LengthKey),
  MinHeight(LengthKey),
  MaxHeight(LengthKey),
  BlockSize(LengthKey),
  MinBlockSize(LengthKey),
  MaxBlockSize(LengthKey),
  DeviceWidth(LengthKey),
  MinDeviceWidth(LengthKey),
  MaxDeviceWidth(LengthKey),
  DeviceHeight(LengthKey),
  MinDeviceHeight(LengthKey),
  MaxDeviceHeight(LengthKey),
  Orientation(Orientation),
  AspectRatio {
    width: u32,
    height: u32,
  },
  MinAspectRatio {
    width: u32,
    height: u32,
  },
  MaxAspectRatio {
    width: u32,
    height: u32,
  },
  DeviceAspectRatio {
    width: u32,
    height: u32,
  },
  MinDeviceAspectRatio {
    width: u32,
    height: u32,
  },
  MaxDeviceAspectRatio {
    width: u32,
    height: u32,
  },
  Resolution(ResolutionKey),
  MinResolution(ResolutionKey),
  MaxResolution(ResolutionKey),
  Color,
  MinColor(u32),
  MaxColor(u32),
  ColorIndex,
  MinColorIndex(u32),
  MaxColorIndex(u32),
  Monochrome,
  MinMonochrome(u32),
  MaxMonochrome(u32),
  Hover(HoverCapability),
  AnyHover(HoverCapability),
  Pointer(PointerCapability),
  AnyPointer(PointerCapability),
  Scripting(Scripting),
  Update(UpdateFrequency),
  LightLevel(LightLevel),
  DisplayMode(DisplayMode),
  PrefersColorScheme(ColorScheme),
  PrefersReducedMotion(ReducedMotion),
  PrefersContrast(ContrastPreference),
  PrefersReducedTransparency(ReducedTransparency),
  PrefersReducedData(ReducedData),
  ColorGamut(ColorGamut),
  ForcedColors(ForcedColors),
  InvertedColors(InvertedColors),
  Range {
    feature: RangeFeature,
    op: ComparisonOp,
    value: RangeValueKey,
  },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LengthKey {
  unit: crate::style::values::LengthUnit,
  bits: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ResolutionKey {
  unit: ResolutionUnit,
  bits: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MediaContextFingerprint {
  media_type: MediaType,
  viewport_width_bits: u32,
  viewport_height_bits: u32,
  device_width_bits: u32,
  device_height_bits: u32,
  device_pixel_ratio_bits: u32,
  base_font_bits: u32,
  can_hover: bool,
  any_can_hover: bool,
  pointer: PointerCapability,
  any_pointer: PointerCapability,
  any_pointer_coarse: bool,
  any_pointer_fine: bool,
  prefers_reduced_motion: bool,
  prefers_reduced_transparency: bool,
  prefers_reduced_data: bool,
  prefers_contrast: ContrastPreference,
  prefers_color_scheme: Option<ColorScheme>,
  color_gamut: ColorGamut,
  inverted_colors: bool,
  forced_colors: bool,
  color_depth: u32,
  color_index: u32,
  monochrome_depth: u32,
  scripting: Scripting,
  update: UpdateFrequency,
  light_level: LightLevel,
  display_mode: DisplayMode,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum RangeValueKey {
  Length(LengthKey),
  AspectRatio(u32, u32),
  Resolution(ResolutionKey),
}

#[derive(Debug, Default)]
pub struct MediaQueryCache {
  results: HashMap<MediaQueryKey, bool>,
  context: Option<MediaContextFingerprint>,
}

impl MediaQueryCache {
  fn get(&self, key: &MediaQueryKey) -> Option<bool> {
    self.results.get(key).copied()
  }

  fn insert(&mut self, key: MediaQueryKey, value: bool) {
    self.results.insert(key, value);
  }

  pub fn len(&self) -> usize {
    self.results.len()
  }

  pub fn is_empty(&self) -> bool {
    self.results.is_empty()
  }

  /// Merge cached results from another [`MediaQueryCache`].
  ///
  /// This is used when independent stages (or parallel work) accumulate media-query
  /// evaluations that can be reused later in the pipeline.
  pub(crate) fn merge_from(&mut self, other: MediaQueryCache) {
    let MediaQueryCache { results, context } = other;
    if results.is_empty() {
      return;
    }

    match context {
      None => {
        // Cache entries without a context fingerprint can be merged directly. This case should be
        // rare (most evaluations install a fingerprint), but keeping it makes the merge robust.
        self.results.extend(results);
      }
      Some(other_ctx) => {
        if self.context.as_ref() != Some(&other_ctx) {
          self.results.clear();
          self.context = Some(other_ctx);
        }
        self.results.extend(results);
      }
    }
  }

  fn ensure_context(&mut self, fingerprint: MediaContextFingerprint) {
    if self.context.as_ref() != Some(&fingerprint) {
      self.results.clear();
      self.context = Some(fingerprint);
    }
  }
}

impl From<&MediaQuery> for MediaQueryKey {
  fn from(query: &MediaQuery) -> Self {
    Self {
      media_type: query.media_type,
      modifier: query.modifier,
      features: query.features.iter().map(MediaFeatureKey::from).collect(),
    }
  }
}

impl From<&MediaFeature> for MediaFeatureKey {
  fn from(feature: &MediaFeature) -> Self {
    match feature {
      MediaFeature::Width(len) => MediaFeatureKey::Width(length_key(len)),
      MediaFeature::MinWidth(len) => MediaFeatureKey::MinWidth(length_key(len)),
      MediaFeature::MaxWidth(len) => MediaFeatureKey::MaxWidth(length_key(len)),
      MediaFeature::InlineSize(len) => MediaFeatureKey::InlineSize(length_key(len)),
      MediaFeature::MinInlineSize(len) => MediaFeatureKey::MinInlineSize(length_key(len)),
      MediaFeature::MaxInlineSize(len) => MediaFeatureKey::MaxInlineSize(length_key(len)),
      MediaFeature::Height(len) => MediaFeatureKey::Height(length_key(len)),
      MediaFeature::MinHeight(len) => MediaFeatureKey::MinHeight(length_key(len)),
      MediaFeature::MaxHeight(len) => MediaFeatureKey::MaxHeight(length_key(len)),
      MediaFeature::BlockSize(len) => MediaFeatureKey::BlockSize(length_key(len)),
      MediaFeature::MinBlockSize(len) => MediaFeatureKey::MinBlockSize(length_key(len)),
      MediaFeature::MaxBlockSize(len) => MediaFeatureKey::MaxBlockSize(length_key(len)),
      MediaFeature::DeviceWidth(len) => MediaFeatureKey::DeviceWidth(length_key(len)),
      MediaFeature::MinDeviceWidth(len) => MediaFeatureKey::MinDeviceWidth(length_key(len)),
      MediaFeature::MaxDeviceWidth(len) => MediaFeatureKey::MaxDeviceWidth(length_key(len)),
      MediaFeature::DeviceHeight(len) => MediaFeatureKey::DeviceHeight(length_key(len)),
      MediaFeature::MinDeviceHeight(len) => MediaFeatureKey::MinDeviceHeight(length_key(len)),
      MediaFeature::MaxDeviceHeight(len) => MediaFeatureKey::MaxDeviceHeight(length_key(len)),
      MediaFeature::Orientation(o) => MediaFeatureKey::Orientation(*o),
      MediaFeature::AspectRatio { width, height } => MediaFeatureKey::AspectRatio {
        width: *width,
        height: *height,
      },
      MediaFeature::MinAspectRatio { width, height } => MediaFeatureKey::MinAspectRatio {
        width: *width,
        height: *height,
      },
      MediaFeature::MaxAspectRatio { width, height } => MediaFeatureKey::MaxAspectRatio {
        width: *width,
        height: *height,
      },
      MediaFeature::DeviceAspectRatio { width, height } => MediaFeatureKey::DeviceAspectRatio {
        width: *width,
        height: *height,
      },
      MediaFeature::MinDeviceAspectRatio { width, height } => {
        MediaFeatureKey::MinDeviceAspectRatio {
          width: *width,
          height: *height,
        }
      }
      MediaFeature::MaxDeviceAspectRatio { width, height } => {
        MediaFeatureKey::MaxDeviceAspectRatio {
          width: *width,
          height: *height,
        }
      }
      MediaFeature::Resolution(res) => MediaFeatureKey::Resolution(resolution_key(res)),
      MediaFeature::MinResolution(res) => MediaFeatureKey::MinResolution(resolution_key(res)),
      MediaFeature::MaxResolution(res) => MediaFeatureKey::MaxResolution(resolution_key(res)),
      MediaFeature::Color => MediaFeatureKey::Color,
      MediaFeature::MinColor(depth) => MediaFeatureKey::MinColor(*depth),
      MediaFeature::MaxColor(depth) => MediaFeatureKey::MaxColor(*depth),
      MediaFeature::ColorIndex => MediaFeatureKey::ColorIndex,
      MediaFeature::MinColorIndex(count) => MediaFeatureKey::MinColorIndex(*count),
      MediaFeature::MaxColorIndex(count) => MediaFeatureKey::MaxColorIndex(*count),
      MediaFeature::Monochrome => MediaFeatureKey::Monochrome,
      MediaFeature::MinMonochrome(depth) => MediaFeatureKey::MinMonochrome(*depth),
      MediaFeature::MaxMonochrome(depth) => MediaFeatureKey::MaxMonochrome(*depth),
      MediaFeature::Hover(cap) => MediaFeatureKey::Hover(*cap),
      MediaFeature::AnyHover(cap) => MediaFeatureKey::AnyHover(*cap),
      MediaFeature::Pointer(cap) => MediaFeatureKey::Pointer(*cap),
      MediaFeature::AnyPointer(cap) => MediaFeatureKey::AnyPointer(*cap),
      MediaFeature::Scripting(state) => MediaFeatureKey::Scripting(*state),
      MediaFeature::Update(freq) => MediaFeatureKey::Update(*freq),
      MediaFeature::LightLevel(level) => MediaFeatureKey::LightLevel(*level),
      MediaFeature::DisplayMode(mode) => MediaFeatureKey::DisplayMode(*mode),
      MediaFeature::PrefersColorScheme(scheme) => MediaFeatureKey::PrefersColorScheme(*scheme),
      MediaFeature::PrefersReducedMotion(motion) => MediaFeatureKey::PrefersReducedMotion(*motion),
      MediaFeature::PrefersContrast(contrast) => MediaFeatureKey::PrefersContrast(*contrast),
      MediaFeature::PrefersReducedTransparency(pref) => {
        MediaFeatureKey::PrefersReducedTransparency(*pref)
      }
      MediaFeature::PrefersReducedData(pref) => MediaFeatureKey::PrefersReducedData(*pref),
      MediaFeature::ColorGamut(gamut) => MediaFeatureKey::ColorGamut(*gamut),
      MediaFeature::ForcedColors(state) => MediaFeatureKey::ForcedColors(*state),
      MediaFeature::InvertedColors(state) => MediaFeatureKey::InvertedColors(*state),
      MediaFeature::Range { feature, op, value } => MediaFeatureKey::Range {
        feature: *feature,
        op: *op,
        value: RangeValueKey::from(value),
      },
    }
  }
}

impl From<&RangeValue> for RangeValueKey {
  fn from(value: &RangeValue) -> Self {
    match value {
      RangeValue::Length(len) => RangeValueKey::Length(length_key(len)),
      RangeValue::AspectRatio(w, h) => RangeValueKey::AspectRatio(*w, *h),
      RangeValue::Resolution(res) => RangeValueKey::Resolution(resolution_key(res)),
    }
  }
}

fn length_key(length: &Length) -> LengthKey {
  LengthKey {
    unit: length.unit,
    bits: length.value.to_bits(),
  }
}

fn resolution_key(resolution: &Resolution) -> ResolutionKey {
  ResolutionKey {
    unit: resolution.unit,
    bits: resolution.value.to_bits(),
  }
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

      // Device dimensions
      "device-width" => {
        let length = Self::parse_length_value(&name, value)?;
        Ok(MediaFeature::DeviceWidth(length))
      }
      "min-device-width" => {
        let length = Self::parse_length_value(&name, value)?;
        Ok(MediaFeature::MinDeviceWidth(length))
      }
      "max-device-width" => {
        let length = Self::parse_length_value(&name, value)?;
        Ok(MediaFeature::MaxDeviceWidth(length))
      }
      "device-height" => {
        let length = Self::parse_length_value(&name, value)?;
        Ok(MediaFeature::DeviceHeight(length))
      }
      "min-device-height" => {
        let length = Self::parse_length_value(&name, value)?;
        Ok(MediaFeature::MinDeviceHeight(length))
      }
      "max-device-height" => {
        let length = Self::parse_length_value(&name, value)?;
        Ok(MediaFeature::MaxDeviceHeight(length))
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
      "device-aspect-ratio" => {
        let (width, height) = Self::parse_ratio_value(&name, value)?;
        Ok(MediaFeature::DeviceAspectRatio { width, height })
      }
      "min-device-aspect-ratio" => {
        let (width, height) = Self::parse_ratio_value(&name, value)?;
        Ok(MediaFeature::MinDeviceAspectRatio { width, height })
      }
      "max-device-aspect-ratio" => {
        let (width, height) = Self::parse_ratio_value(&name, value)?;
        Ok(MediaFeature::MaxDeviceAspectRatio { width, height })
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

      // MQ5 additions
      "scripting" => {
        let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
        let scripting = Scripting::parse(value)?;
        Ok(MediaFeature::Scripting(scripting))
      }
      "update" => {
        let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
        let update = UpdateFrequency::parse(value)?;
        Ok(MediaFeature::Update(update))
      }
      "light-level" => {
        let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
        let level = LightLevel::parse(value)?;
        Ok(MediaFeature::LightLevel(level))
      }
      "display-mode" => {
        let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
        let mode = DisplayMode::parse(value)?;
        Ok(MediaFeature::DisplayMode(mode))
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
      "color-gamut" => {
        let value = value.ok_or_else(|| MediaParseError::MissingValue(name.clone()))?;
        let gamut = ColorGamut::parse(value)?;
        Ok(MediaFeature::ColorGamut(gamut))
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

    let width = parts[0]
      .parse::<u32>()
      .map_err(|_| MediaParseError::InvalidValue {
        feature: name.to_string(),
        value: value.to_string(),
      })?;

    let height = parts[1]
      .parse::<u32>()
      .map_err(|_| MediaParseError::InvalidValue {
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

  fn parse_resolution_value(
    name: &str,
    value: Option<&str>,
  ) -> Result<Resolution, MediaParseError> {
    let value = value.ok_or_else(|| MediaParseError::MissingValue(name.to_string()))?;
    Resolution::parse(value).map_err(|_| MediaParseError::InvalidValue {
      feature: name.to_string(),
      value: value.to_string(),
    })
  }

  fn parse_integer_value(name: &str, value: Option<&str>) -> Result<u32, MediaParseError> {
    let value = value.ok_or_else(|| MediaParseError::MissingValue(name.to_string()))?;
    value
      .trim()
      .parse::<u32>()
      .map_err(|_| MediaParseError::InvalidValue {
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

    let validate =
      |value: f32, unit: ResolutionUnit, original: &str| -> Result<Self, MediaParseError> {
        if !value.is_finite() || value < 0.0 {
          return Err(MediaParseError::InvalidResolution(original.to_string()));
        }
        Ok(Resolution::new(value, unit))
      };

    // Try each unit suffix
    if let Some(value_str) = s.strip_suffix("dppx") {
      let value = value_str
        .trim()
        .parse::<f32>()
        .map_err(|_| MediaParseError::InvalidResolution(s.clone()))?;
      return validate(value, ResolutionUnit::Dppx, &s);
    }

    if let Some(value_str) = s.strip_suffix("dpcm") {
      let value = value_str
        .trim()
        .parse::<f32>()
        .map_err(|_| MediaParseError::InvalidResolution(s.clone()))?;
      return validate(value, ResolutionUnit::Dpcm, &s);
    }

    if let Some(value_str) = s.strip_suffix("dpi") {
      let value = value_str
        .trim()
        .parse::<f32>()
        .map_err(|_| MediaParseError::InvalidResolution(s.clone()))?;
      return validate(value, ResolutionUnit::Dpi, &s);
    }

    // Try 'x' as alias for dppx
    if let Some(value_str) = s.strip_suffix('x') {
      let value = value_str
        .trim()
        .parse::<f32>()
        .map_err(|_| MediaParseError::InvalidResolution(s.clone()))?;
      return validate(value, ResolutionUnit::Dppx, &s);
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

/// Scripting availability for the environment
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Scripting {
  None,
  InitialOnly,
  Enabled,
}

impl Scripting {
  pub fn parse(s: &str) -> Result<Self, MediaParseError> {
    let s = s.trim().to_ascii_lowercase();
    match s.as_str() {
      "none" => Ok(Scripting::None),
      "initial-only" => Ok(Scripting::InitialOnly),
      "enabled" => Ok(Scripting::Enabled),
      _ => Err(MediaParseError::InvalidScripting(s)),
    }
  }
}

impl fmt::Display for Scripting {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Scripting::None => write!(f, "none"),
      Scripting::InitialOnly => write!(f, "initial-only"),
      Scripting::Enabled => write!(f, "enabled"),
    }
  }
}

/// Update frequency of the output medium
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UpdateFrequency {
  None,
  Slow,
  Fast,
}

impl UpdateFrequency {
  pub fn parse(s: &str) -> Result<Self, MediaParseError> {
    let s = s.trim().to_ascii_lowercase();
    match s.as_str() {
      "none" => Ok(UpdateFrequency::None),
      "slow" => Ok(UpdateFrequency::Slow),
      "fast" => Ok(UpdateFrequency::Fast),
      _ => Err(MediaParseError::InvalidUpdateFrequency(s)),
    }
  }
}

impl fmt::Display for UpdateFrequency {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      UpdateFrequency::None => write!(f, "none"),
      UpdateFrequency::Slow => write!(f, "slow"),
      UpdateFrequency::Fast => write!(f, "fast"),
    }
  }
}

/// Ambient light level for the device
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LightLevel {
  Dim,
  Normal,
  Washed,
}

impl LightLevel {
  pub fn parse(s: &str) -> Result<Self, MediaParseError> {
    let s = s.trim().to_ascii_lowercase();
    match s.as_str() {
      "dim" => Ok(LightLevel::Dim),
      "normal" => Ok(LightLevel::Normal),
      "washed" => Ok(LightLevel::Washed),
      _ => Err(MediaParseError::InvalidLightLevel(s)),
    }
  }
}

impl fmt::Display for LightLevel {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      LightLevel::Dim => write!(f, "dim"),
      LightLevel::Normal => write!(f, "normal"),
      LightLevel::Washed => write!(f, "washed"),
    }
  }
}

/// Display mode reported by the web app manifest
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DisplayMode {
  Browser,
  MinimalUi,
  Standalone,
  Fullscreen,
  WindowControlsOverlay,
}

impl DisplayMode {
  pub fn parse(s: &str) -> Result<Self, MediaParseError> {
    let s = s.trim().to_ascii_lowercase();
    match s.as_str() {
      "browser" => Ok(DisplayMode::Browser),
      "minimal-ui" => Ok(DisplayMode::MinimalUi),
      "standalone" => Ok(DisplayMode::Standalone),
      "fullscreen" => Ok(DisplayMode::Fullscreen),
      "window-controls-overlay" => Ok(DisplayMode::WindowControlsOverlay),
      _ => Err(MediaParseError::InvalidDisplayMode(s)),
    }
  }
}

impl fmt::Display for DisplayMode {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      DisplayMode::Browser => write!(f, "browser"),
      DisplayMode::MinimalUi => write!(f, "minimal-ui"),
      DisplayMode::Standalone => write!(f, "standalone"),
      DisplayMode::Fullscreen => write!(f, "fullscreen"),
      DisplayMode::WindowControlsOverlay => write!(f, "window-controls-overlay"),
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

/// Color gamut capability
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ColorGamut {
  Srgb,
  P3,
  Rec2020,
}

impl ColorGamut {
  pub fn parse(s: &str) -> Result<Self, MediaParseError> {
    let s = s.trim().to_ascii_lowercase();
    match s.as_str() {
      "srgb" => Ok(ColorGamut::Srgb),
      "p3" | "display-p3" => Ok(ColorGamut::P3),
      "rec2020" | "rec-2020" | "bt2020" | "bt-2020" => Ok(ColorGamut::Rec2020),
      _ => Err(MediaParseError::InvalidColorGamut(s)),
    }
  }
}

impl fmt::Display for ColorGamut {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ColorGamut::Srgb => write!(f, "srgb"),
      ColorGamut::P3 => write!(f, "p3"),
      ColorGamut::Rec2020 => write!(f, "rec2020"),
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
  /// Device width in CSS pixels
  pub device_width: f32,
  /// Device height in CSS pixels
  pub device_height: f32,
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
  /// Device color gamut capability
  pub color_gamut: ColorGamut,
  /// Whether primary input can hover
  pub can_hover: bool,
  /// Whether any input can hover
  pub any_can_hover: bool,
  /// Primary pointer accuracy
  pub pointer: PointerCapability,
  /// Any pointer accuracy (finest available)
  pub any_pointer: PointerCapability,
  /// Whether any available pointer is coarse
  pub any_pointer_coarse: bool,
  /// Whether any available pointer is fine
  pub any_pointer_fine: bool,
  /// Whether scripting is available in the environment
  pub scripting: Scripting,
  /// How frequently the output updates (fast/slow/none)
  pub update_frequency: UpdateFrequency,
  /// Ambient light level for light-level media feature
  pub light_level: LightLevel,
  /// PWA display mode for display-mode media feature
  pub display_mode: DisplayMode,
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
  fn fingerprint(&self) -> MediaContextFingerprint {
    MediaContextFingerprint {
      media_type: self.media_type,
      viewport_width_bits: self.viewport_width.to_bits(),
      viewport_height_bits: self.viewport_height.to_bits(),
      device_width_bits: self.device_width.to_bits(),
      device_height_bits: self.device_height.to_bits(),
      device_pixel_ratio_bits: self.device_pixel_ratio.to_bits(),
      base_font_bits: self.base_font_size.to_bits(),
      can_hover: self.can_hover,
      any_can_hover: self.any_can_hover,
      pointer: self.pointer,
      any_pointer: self.any_pointer,
      any_pointer_coarse: self.any_pointer_coarse,
      any_pointer_fine: self.any_pointer_fine,
      prefers_reduced_motion: self.prefers_reduced_motion,
      prefers_reduced_transparency: self.prefers_reduced_transparency,
      prefers_reduced_data: self.prefers_reduced_data,
      prefers_contrast: self.prefers_contrast,
      prefers_color_scheme: self.prefers_color_scheme,
      color_gamut: self.color_gamut,
      inverted_colors: matches!(self.inverted_colors, InvertedColors::Inverted),
      forced_colors: self.forced_colors,
      color_depth: self.color_depth,
      color_index: self.color_index,
      monochrome_depth: self.monochrome_depth,
      scripting: self.scripting,
      update: self.update_frequency,
      light_level: self.light_level,
      display_mode: self.display_mode,
    }
  }

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
      device_width: width,
      device_height: height,
      base_font_size: 16.0,
      device_pixel_ratio: 1.0,
      media_type: MediaType::Screen,
      color_depth: 8,
      color_index: 0,
      monochrome_depth: 0,
      color_gamut: ColorGamut::Srgb,
      can_hover: true,
      any_can_hover: true,
      pointer: PointerCapability::Fine,
      any_pointer: PointerCapability::Fine,
      any_pointer_coarse: false,
      any_pointer_fine: true,
      scripting: Scripting::Enabled,
      update_frequency: UpdateFrequency::Fast,
      light_level: LightLevel::Normal,
      display_mode: DisplayMode::Browser,
      prefers_color_scheme: Some(ColorScheme::NoPreference),
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
  /// - `FASTR_MEDIA_TYPE` = `screen` | `print` | `all` | `speech`
  /// - `FASTR_PREFERS_COLOR_SCHEME` = `light` | `dark` | `no-preference`
  /// - `FASTR_PREFERS_REDUCED_MOTION` = `reduce` | `no-preference` | truthy/falsy
  /// - `FASTR_PREFERS_CONTRAST` = `more`/`high` | `less`/`low` | `custom`/`forced` | `no-preference`
  /// - `FASTR_PREFERS_REDUCED_TRANSPARENCY` = `reduce` | `no-preference` | truthy/falsy
  /// - `FASTR_PREFERS_REDUCED_DATA` = `reduce` | `no-preference` | truthy/falsy
  /// - `FASTR_SCRIPTING` = `none` | `initial-only` | `enabled` | truthy/falsy
  /// - `FASTR_UPDATE_FREQUENCY` = `fast` | `slow` | `none`
  /// - `FASTR_LIGHT_LEVEL` = `dim` | `normal` | `washed`
  /// - `FASTR_DISPLAY_MODE` = `browser` | `fullscreen` | `standalone` | `minimal-ui` | `window-controls-overlay`
  /// - `FASTR_COLOR_GAMUT` = `srgb` | `p3` | `rec2020`
  /// - `FASTR_INVERTED_COLORS` = `inverted` | `none` | truthy/falsy
  /// - `FASTR_FORCED_COLORS` = `active` | `none` | truthy/falsy
  /// - `FASTR_COLOR_DEPTH` = integer bits per color channel (e.g., 8)
  /// - `FASTR_COLOR_INDEX` = integer palette size (e.g., 256)
  /// - `FASTR_MONOCHROME_DEPTH` = integer bits for monochrome devices (e.g., 1)
  #[allow(clippy::cognitive_complexity)]
  pub fn with_env_overrides(mut self) -> Self {
    let toggles = crate::debug::runtime::RuntimeToggles::from_env();
    let overrides = &toggles.config().media;
    if let Some(mt) = overrides.media_type {
      self.media_type = mt;
    }

    if let Some(scripting) = overrides.scripting {
      self.scripting = scripting;
    }

    if let Some(update) = overrides.update_frequency {
      self.update_frequency = update;
    }

    if let Some(level) = overrides.light_level {
      self.light_level = level;
    }

    if let Some(mode) = overrides.display_mode {
      self.display_mode = mode;
    }

    if let Some(scheme) = overrides.prefers_color_scheme {
      self.prefers_color_scheme = Some(scheme);
    }

    if let Some(reduced_motion) = overrides.prefers_reduced_motion {
      self.prefers_reduced_motion = reduced_motion;
    }

    if let Some(pref) = overrides.prefers_contrast {
      self.prefers_contrast = pref;
    }

    if let Some(reduced_transparency) = overrides.prefers_reduced_transparency {
      self.prefers_reduced_transparency = reduced_transparency;
    }

    if let Some(reduced_data) = overrides.prefers_reduced_data {
      self.prefers_reduced_data = reduced_data;
    }

    if let Some(gamut) = overrides.color_gamut {
      self.color_gamut = gamut;
    }

    if let Some(inverted) = overrides.inverted_colors {
      self.inverted_colors = inverted;
    }

    if let Some(forced) = overrides.forced_colors {
      self.forced_colors = forced;
    }

    if let Some(bits) = overrides.color_depth {
      self.color_depth = bits;
    }

    if let Some(count) = overrides.color_index {
      self.color_index = count;
    }

    if let Some(bits) = overrides.monochrome_depth {
      self.monochrome_depth = bits;
    }

    self
  }

  /// Returns a new context with the given device pixel ratio.
  pub fn with_device_pixel_ratio(mut self, dpr: f32) -> Self {
    self.device_pixel_ratio = if dpr.is_finite() && dpr > 0.0 {
      dpr
    } else {
      1.0
    };
    self
  }

  /// Returns a new context with the given device dimensions in CSS pixels.
  pub fn with_device_size(mut self, width: f32, height: f32) -> Self {
    if width.is_finite() && width > 0.0 {
      self.device_width = width;
    }
    if height.is_finite() && height > 0.0 {
      self.device_height = height;
    }
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
      device_width: width,
      device_height: height,
      base_font_size: 16.0,
      device_pixel_ratio: 1.0,
      media_type: MediaType::Print,
      color_depth: 8, // Most printers can do color
      color_index: 0,
      monochrome_depth: 0,
      color_gamut: ColorGamut::Srgb,
      can_hover: false,
      any_can_hover: false,
      pointer: PointerCapability::None,
      any_pointer: PointerCapability::None,
      any_pointer_coarse: false,
      any_pointer_fine: false,
      scripting: Scripting::None,
      update_frequency: UpdateFrequency::None,
      light_level: LightLevel::Normal,
      display_mode: DisplayMode::Browser,
      prefers_color_scheme: Some(ColorScheme::NoPreference),
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
      device_width: width,
      device_height: height,
      base_font_size: 16.0,
      device_pixel_ratio: 2.0, // Common for mobile
      media_type: MediaType::Screen,
      color_depth: 8,
      color_index: 0,
      monochrome_depth: 0,
      color_gamut: ColorGamut::Srgb,
      can_hover: false,
      any_can_hover: false,
      pointer: PointerCapability::Coarse,
      any_pointer: PointerCapability::Coarse,
      any_pointer_coarse: true,
      any_pointer_fine: false,
      scripting: Scripting::Enabled,
      update_frequency: UpdateFrequency::Fast,
      light_level: LightLevel::Normal,
      display_mode: DisplayMode::Browser,
      prefers_color_scheme: Some(ColorScheme::NoPreference),
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

  /// Sets the media type (screen/print/all/speech)
  pub fn with_media_type(mut self, media_type: MediaType) -> Self {
    self.media_type = media_type;
    self
  }

  /// Sets the base font size used when resolving em/rem in queries
  pub fn with_base_font_size(mut self, size: f32) -> Self {
    if size.is_finite() && size > 0.0 {
      self.base_font_size = size;
    }
    self
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

  /// Sets the contrast preference
  pub fn with_prefers_contrast(mut self, contrast: ContrastPreference) -> Self {
    self.prefers_contrast = contrast;
    self
  }

  /// Sets the reduced transparency preference
  pub fn with_reduced_transparency(mut self, reduce: bool) -> Self {
    self.prefers_reduced_transparency = reduce;
    self
  }

  /// Sets the reduced data preference
  pub fn with_reduced_data(mut self, reduce: bool) -> Self {
    self.prefers_reduced_data = reduce;
    self
  }

  /// Sets the scripting capability for the environment
  pub fn with_scripting(mut self, scripting: Scripting) -> Self {
    self.scripting = scripting;
    self
  }

  /// Sets the update frequency (fast/slow/none)
  pub fn with_update_frequency(mut self, update: UpdateFrequency) -> Self {
    self.update_frequency = update;
    self
  }

  /// Sets the ambient light level
  pub fn with_light_level(mut self, level: LightLevel) -> Self {
    self.light_level = level;
    self
  }

  /// Sets the PWA display mode
  pub fn with_display_mode(mut self, mode: DisplayMode) -> Self {
    self.display_mode = mode;
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
  /// Returns true if ANY query in the list matches. Empty lists evaluate to
  /// `false`, which ensures invalid @media preludes do not accidentally match.
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

  /// Evaluates a media query list using an optional cache.
  ///
  /// When a cache is provided, media query results are memoized for the
  /// lifetime of the cache, avoiding repeated evaluation of identical queries
  /// (useful when collecting both style rules and @font-face rules). Empty lists
  /// short-circuit to `false`.
  pub fn evaluate_list_with_cache(
    &self,
    queries: &[MediaQuery],
    cache: Option<&mut MediaQueryCache>,
  ) -> bool {
    if let Some(cache) = cache {
      for query in queries {
        if self.evaluate_with_cache(query, Some(cache)) {
          return true;
        }
      }
      return false;
    }

    self.evaluate_list(queries)
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

  /// Evaluates a single media query with optional caching.
  pub fn evaluate_with_cache(
    &self,
    query: &MediaQuery,
    cache: Option<&mut MediaQueryCache>,
  ) -> bool {
    if let Some(cache) = cache {
      cache.ensure_context(self.fingerprint());
      let key = MediaQueryKey::from(query);
      if let Some(hit) = cache.get(&key) {
        return hit;
      }

      let result = self.evaluate(query);
      cache.insert(key, result);
      return result;
    }

    self.evaluate(query)
  }

  fn matches_media_type(&self, media_type: MediaType) -> bool {
    media_type == MediaType::All || media_type == self.media_type
  }

  fn evaluate_feature(&self, feature: &MediaFeature) -> bool {
    match feature {
      // Width features
      MediaFeature::Width(length) => self
        .resolve_length(length, self.viewport_width, self.viewport_height)
        .map(|target| (self.viewport_width - target).abs() < 0.5)
        .unwrap_or(false),
      MediaFeature::MinWidth(length) => self
        .resolve_length(length, self.viewport_width, self.viewport_height)
        .map(|target| self.viewport_width >= target)
        .unwrap_or(false),
      MediaFeature::MaxWidth(length) => self
        .resolve_length(length, self.viewport_width, self.viewport_height)
        .map(|target| self.viewport_width <= target)
        .unwrap_or(false),

      // Inline-size (alias of width for container queries)
      MediaFeature::InlineSize(length) => self
        .resolve_length(length, self.viewport_width, self.viewport_height)
        .map(|target| (self.viewport_width - target).abs() < 0.5)
        .unwrap_or(false),
      MediaFeature::MinInlineSize(length) => self
        .resolve_length(length, self.viewport_width, self.viewport_height)
        .map(|target| self.viewport_width >= target)
        .unwrap_or(false),
      MediaFeature::MaxInlineSize(length) => self
        .resolve_length(length, self.viewport_width, self.viewport_height)
        .map(|target| self.viewport_width <= target)
        .unwrap_or(false),

      // Height features
      MediaFeature::Height(length) => self
        .resolve_length(length, self.viewport_height, self.viewport_width)
        .map(|target| (self.viewport_height - target).abs() < 0.5)
        .unwrap_or(false),
      MediaFeature::MinHeight(length) => self
        .resolve_length(length, self.viewport_height, self.viewport_width)
        .map(|target| self.viewport_height >= target)
        .unwrap_or(false),
      MediaFeature::MaxHeight(length) => self
        .resolve_length(length, self.viewport_height, self.viewport_width)
        .map(|target| self.viewport_height <= target)
        .unwrap_or(false),

      // Block-size (alias of height for container queries)
      MediaFeature::BlockSize(length) => self
        .resolve_length(length, self.viewport_height, self.viewport_width)
        .map(|target| (self.viewport_height - target).abs() < 0.5)
        .unwrap_or(false),
      MediaFeature::MinBlockSize(length) => self
        .resolve_length(length, self.viewport_height, self.viewport_width)
        .map(|target| self.viewport_height >= target)
        .unwrap_or(false),
      MediaFeature::MaxBlockSize(length) => self
        .resolve_length(length, self.viewport_height, self.viewport_width)
        .map(|target| self.viewport_height <= target)
        .unwrap_or(false),

      // Device dimensions
      MediaFeature::DeviceWidth(length) => self
        .resolve_length(length, self.device_width, self.device_height)
        .map(|target| (self.device_width - target).abs() < 0.5)
        .unwrap_or(false),
      MediaFeature::MinDeviceWidth(length) => self
        .resolve_length(length, self.device_width, self.device_height)
        .map(|target| self.device_width >= target)
        .unwrap_or(false),
      MediaFeature::MaxDeviceWidth(length) => self
        .resolve_length(length, self.device_width, self.device_height)
        .map(|target| self.device_width <= target)
        .unwrap_or(false),
      MediaFeature::DeviceHeight(length) => self
        .resolve_length(length, self.device_height, self.device_width)
        .map(|target| (self.device_height - target).abs() < 0.5)
        .unwrap_or(false),
      MediaFeature::MinDeviceHeight(length) => self
        .resolve_length(length, self.device_height, self.device_width)
        .map(|target| self.device_height >= target)
        .unwrap_or(false),
      MediaFeature::MaxDeviceHeight(length) => self
        .resolve_length(length, self.device_height, self.device_width)
        .map(|target| self.device_height <= target)
        .unwrap_or(false),

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
      MediaFeature::DeviceAspectRatio { width, height } => {
        let target_ratio = *width as f32 / *height as f32;
        let actual_ratio = self.device_width / self.device_height;
        (target_ratio - actual_ratio).abs() < 0.01
      }
      MediaFeature::MinDeviceAspectRatio { width, height } => {
        let target_ratio = *width as f32 / *height as f32;
        let actual_ratio = self.device_width / self.device_height;
        actual_ratio >= target_ratio
      }
      MediaFeature::MaxDeviceAspectRatio { width, height } => {
        let target_ratio = *width as f32 / *height as f32;
        let actual_ratio = self.device_width / self.device_height;
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
      MediaFeature::ColorGamut(required) => self.color_gamut >= *required,

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
      MediaFeature::AnyPointer(capability) => match capability {
        PointerCapability::None => !self.any_pointer_coarse && !self.any_pointer_fine,
        PointerCapability::Coarse => self.any_pointer_coarse,
        PointerCapability::Fine => self.any_pointer_fine,
      },

      // MQ5
      MediaFeature::Scripting(state) => self.scripting == *state,
      MediaFeature::Update(freq) => self.update_frequency == *freq,
      MediaFeature::LightLevel(level) => self.light_level == *level,
      MediaFeature::DisplayMode(mode) => self.display_mode == *mode,

      // User preferences
      MediaFeature::PrefersColorScheme(scheme) => match self.prefers_color_scheme {
        Some(current) => current == *scheme,
        None => matches!(scheme, ColorScheme::NoPreference),
      },
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
        (RangeFeature::Width, RangeValue::Length(len)) => self
          .resolve_length(len, self.viewport_width, self.viewport_height)
          .map(|target| compare_with_op(*op, self.viewport_width, target))
          .unwrap_or(false),
        (RangeFeature::InlineSize, RangeValue::Length(len)) => self
          .resolve_length(len, self.viewport_width, self.viewport_height)
          .map(|target| compare_with_op(*op, self.viewport_width, target))
          .unwrap_or(false),
        (RangeFeature::BlockSize, RangeValue::Length(len)) => self
          .resolve_length(len, self.viewport_height, self.viewport_width)
          .map(|target| compare_with_op(*op, self.viewport_height, target))
          .unwrap_or(false),
        (RangeFeature::Height, RangeValue::Length(len)) => self
          .resolve_length(len, self.viewport_height, self.viewport_width)
          .map(|target| compare_with_op(*op, self.viewport_height, target))
          .unwrap_or(false),
        (RangeFeature::DeviceWidth, RangeValue::Length(len)) => self
          .resolve_length(len, self.device_width, self.device_height)
          .map(|target| compare_with_op(*op, self.device_width, target))
          .unwrap_or(false),
        (RangeFeature::DeviceHeight, RangeValue::Length(len)) => self
          .resolve_length(len, self.device_height, self.device_width)
          .map(|target| compare_with_op(*op, self.device_height, target))
          .unwrap_or(false),
        (RangeFeature::AspectRatio, RangeValue::AspectRatio(w, h)) => {
          let target_ratio = *w as f32 / *h as f32;
          let actual_ratio = self.viewport_width / self.viewport_height;
          compare_with_op(*op, actual_ratio, target_ratio)
        }
        (RangeFeature::DeviceAspectRatio, RangeValue::AspectRatio(w, h)) => {
          let target_ratio = *w as f32 / *h as f32;
          let actual_ratio = self.device_width / self.device_height;
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

  fn resolve_length(&self, length: &Length, inline_base: f32, block_base: f32) -> Option<f32> {
    // For media/container queries, resolve lengths to pixels using the
    // context viewport and base font size (em/rem derived from the query context).
    use crate::style::values::LengthUnit;

    let base_font = self
      .base_font_size
      .is_finite()
      .then_some(self.base_font_size)
      .filter(|v| *v > 0.0)
      .unwrap_or(16.0);
    let vw = self
      .viewport_width
      .is_finite()
      .then_some(self.viewport_width);
    let vh = self
      .viewport_height
      .is_finite()
      .then_some(self.viewport_height);
    let inline = inline_base.is_finite().then_some(inline_base);
    let _block = block_base.is_finite().then_some(block_base);

    if let Some(calc) = length.calc {
      let percentage_base = if calc.has_percentage() {
        inline
      } else {
        Some(0.0)
      };
      let needs_viewport = calc.has_viewport_relative();
      let vw = if needs_viewport {
        vw?
      } else {
        vw.unwrap_or(0.0)
      };
      let vh = if needs_viewport {
        vh?
      } else {
        vh.unwrap_or(0.0)
      };
      return calc.resolve(percentage_base, vw, vh, base_font, base_font);
    }

    match length.unit {
      LengthUnit::Px => Some(length.value),
      LengthUnit::Em => Some(length.value * base_font),
      LengthUnit::Rem => Some(length.value * base_font),
      LengthUnit::Percent => Some(length.value / 100.0 * inline?),
      LengthUnit::Vw => Some(length.value / 100.0 * vw?),
      LengthUnit::Vh => Some(length.value / 100.0 * vh?),
      LengthUnit::Vmin => {
        let min_dimension = match (vw, vh) {
          (Some(w), Some(h)) => w.min(h),
          _ => return None,
        };
        Some(length.value / 100.0 * min_dimension)
      }
      LengthUnit::Vmax => {
        let max_dimension = match (vw, vh) {
          (Some(w), Some(h)) => w.max(h),
          _ => return None,
        };
        Some(length.value / 100.0 * max_dimension)
      }
      LengthUnit::Dvw => Some(length.value / 100.0 * vw?),
      LengthUnit::Dvh => Some(length.value / 100.0 * vh?),
      LengthUnit::Dvmin => {
        let min_dimension = match (vw, vh) {
          (Some(w), Some(h)) => w.min(h),
          _ => return None,
        };
        Some(length.value / 100.0 * min_dimension)
      }
      LengthUnit::Dvmax => {
        let max_dimension = match (vw, vh) {
          (Some(w), Some(h)) => w.max(h),
          _ => return None,
        };
        Some(length.value / 100.0 * max_dimension)
      }
      LengthUnit::Pt => Some(length.value * 96.0 / 72.0),
      LengthUnit::Cm => Some(length.value * 96.0 / 2.54),
      LengthUnit::Mm => Some(length.value * 96.0 / 25.4),
      LengthUnit::Q => Some(length.value * 96.0 / 101.6), // 1Q = 0.25mm = 1/40th cm
      LengthUnit::In => Some(length.value * 96.0),
      LengthUnit::Pc => Some(length.value * 16.0),
      LengthUnit::Ex => Some(length.value * (base_font * 0.5)), // Approximate: half of em
      LengthUnit::Ch => Some(length.value * (base_font * 0.5)), // Approximate: width of '0'
      LengthUnit::Calc => None,
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
    let name = self
      .parse_ident()
      .ok_or(MediaParseError::ExpectedFeatureName)?;
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
        "device-width" => Some(RangeFeature::DeviceWidth),
        "device-height" => Some(RangeFeature::DeviceHeight),
        "aspect-ratio" => Some(RangeFeature::AspectRatio),
        "device-aspect-ratio" => Some(RangeFeature::DeviceAspectRatio),
        "resolution" => Some(RangeFeature::Resolution),
        _ => None,
      }
    };

    let parse_value = |feature: RangeFeature, raw: &str| -> Result<RangeValue, MediaParseError> {
      match feature {
        RangeFeature::Width
        | RangeFeature::InlineSize
        | RangeFeature::BlockSize
        | RangeFeature::Height
        | RangeFeature::DeviceWidth
        | RangeFeature::DeviceHeight => {
          let len = MediaFeature::parse_length_value(
            match feature {
              RangeFeature::Width => "width",
              RangeFeature::InlineSize => "inline-size",
              RangeFeature::BlockSize => "block-size",
              RangeFeature::Height => "height",
              RangeFeature::DeviceWidth => "device-width",
              RangeFeature::DeviceHeight => "device-height",
              _ => unreachable!(),
            },
            Some(raw),
          )?;
          Ok(RangeValue::Length(len))
        }
        RangeFeature::AspectRatio | RangeFeature::DeviceAspectRatio => {
          let feature_name = match feature {
            RangeFeature::AspectRatio => "aspect-ratio",
            RangeFeature::DeviceAspectRatio => "device-aspect-ratio",
            _ => unreachable!(),
          };
          let (w, h) = MediaFeature::parse_ratio_value(feature_name, Some(raw))?;
          Ok(RangeValue::AspectRatio(w, h))
        }
        RangeFeature::Resolution => {
          let res = MediaFeature::parse_resolution_value("resolution", Some(raw))?;
          Ok(RangeValue::Resolution(res))
        }
      }
    };

    let build_feature = |feature: RangeFeature,
                         op: ComparisonOp,
                         raw: &str|
     -> Result<MediaFeature, MediaParseError> {
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
  /// Invalid scripting value
  InvalidScripting(String),
  /// Invalid update frequency
  InvalidUpdateFrequency(String),
  /// Invalid light-level value
  InvalidLightLevel(String),
  /// Invalid display-mode value
  InvalidDisplayMode(String),
  /// Invalid color-gamut value
  InvalidColorGamut(String),
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
        write!(
          f,
          "Invalid value '{}' for media feature '{}'",
          value, feature
        )
      }
      MediaParseError::InvalidOrientation(s) => {
        write!(
          f,
          "Invalid orientation: '{}' (expected 'portrait' or 'landscape')",
          s
        )
      }
      MediaParseError::InvalidResolution(s) => {
        write!(
          f,
          "Invalid resolution: '{}' (expected format like '2dppx', '96dpi')",
          s
        )
      }
      MediaParseError::InvalidHoverCapability(s) => {
        write!(
          f,
          "Invalid hover capability: '{}' (expected 'none' or 'hover')",
          s
        )
      }
      MediaParseError::InvalidPointerCapability(s) => {
        write!(
          f,
          "Invalid pointer capability: '{}' (expected 'none', 'coarse', or 'fine')",
          s
        )
      }
      MediaParseError::InvalidColorScheme(s) => {
        write!(
          f,
          "Invalid color scheme: '{}' (expected 'light', 'dark', or 'no-preference')",
          s
        )
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
      MediaParseError::InvalidScripting(s) => {
        write!(
          f,
          "Invalid scripting value: '{}' (expected 'none', 'initial-only', or 'enabled')",
          s
        )
      }
      MediaParseError::InvalidUpdateFrequency(s) => {
        write!(
          f,
          "Invalid update frequency: '{}' (expected 'none', 'slow', or 'fast')",
          s
        )
      }
      MediaParseError::InvalidLightLevel(s) => {
        write!(
          f,
          "Invalid light-level: '{}' (expected 'dim', 'normal', or 'washed')",
          s
        )
      }
      MediaParseError::InvalidDisplayMode(s) => {
        write!(
          f,
          "Invalid display-mode: '{}' (expected 'browser', 'fullscreen', 'standalone', 'minimal-ui', or 'window-controls-overlay')",
          s
        )
      }
      MediaParseError::InvalidColorGamut(s) => {
        write!(
          f,
          "Invalid color-gamut: '{}' (expected 'srgb', 'p3', or 'rec2020')",
          s
        )
      }
      MediaParseError::InvalidForcedColors(s) => {
        write!(
          f,
          "Invalid forced-colors: '{}' (expected 'none' or 'active')",
          s
        )
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
    ("dvmin", |v| Length::new(v, LengthUnit::Dvmin)),
    ("dvmax", |v| Length::new(v, LengthUnit::Dvmax)),
    ("px", |v| Length::px(v)),
    ("em", |v| Length::em(v)),
    ("%", |v| Length::percent(v)),
    ("vw", |v| Length::new(v, LengthUnit::Vw)),
    ("vh", |v| Length::new(v, LengthUnit::Vh)),
    ("dvw", |v| Length::new(v, LengthUnit::Dvw)),
    ("dvh", |v| Length::new(v, LengthUnit::Dvh)),
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
        let length = constructor(value);
        if !length.value.is_finite() || length.value < 0.0 || length.unit.is_percentage() {
          return None;
        }
        return Some(length);
      }
    }
  }

  None
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::style::values::CalcLength;
  use crate::LengthUnit;
  use std::cell::RefCell;
  use std::env;
  use std::sync::Arc;

  struct EnvGuard {
    key: &'static str,
    old: Option<String>,
  }

  fn env_lock() -> std::sync::MutexGuard<'static, ()> {
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    // If a prior test panicked while holding the lock, avoid poisoning subsequent tests.
    LOCK
      .get_or_init(std::sync::Mutex::default)
      .lock()
      .unwrap_or_else(|e| e.into_inner())
  }

  impl EnvGuard {
    fn new(key: &'static str, value: Option<&str>) -> Self {
      thread_local! {
        static DEPTH: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
        static ENV_LOCK: RefCell<Option<std::sync::MutexGuard<'static, ()>>> = const { RefCell::new(None) };
        static BASE_TOGGLES: RefCell<Option<Arc<crate::debug::runtime::RuntimeToggles>>> = const { RefCell::new(None) };
      }

      let outermost = DEPTH.with(|depth| {
        let outermost = depth.get() == 0;
        depth.set(depth.get().saturating_add(1));
        outermost
      });

      if outermost {
        ENV_LOCK.with(|cell| {
          *cell.borrow_mut() = Some(env_lock());
        });
        BASE_TOGGLES.with(|cell| {
          *cell.borrow_mut() = Some(crate::debug::runtime::runtime_toggles());
        });
      }

      let old = env::var(key).ok();
      match value {
        Some(v) => env::set_var(key, v),
        None => env::remove_var(key),
      }
      crate::debug::runtime::update_runtime_toggles(Arc::new(
        crate::debug::runtime::RuntimeToggles::from_env(),
      ));
      EnvGuard { key, old }
    }
  }

  impl Drop for EnvGuard {
    fn drop(&mut self) {
      thread_local! {
        static DEPTH: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
        static ENV_LOCK: RefCell<Option<std::sync::MutexGuard<'static, ()>>> = const { RefCell::new(None) };
        static BASE_TOGGLES: RefCell<Option<Arc<crate::debug::runtime::RuntimeToggles>>> = const { RefCell::new(None) };
      }

      if let Some(ref val) = self.old {
        env::set_var(self.key, val);
      } else {
        env::remove_var(self.key);
      }

      let last = DEPTH.with(|depth| {
        let current = depth.get();
        let next = current.saturating_sub(1);
        depth.set(next);
        next == 0
      });

      if last {
        let base = BASE_TOGGLES.with(|cell| cell.borrow_mut().take());
        if let Some(base) = base {
          crate::debug::runtime::update_runtime_toggles(base);
        }
        ENV_LOCK.with(|cell| {
          cell.borrow_mut().take();
        });
      } else {
        crate::debug::runtime::update_runtime_toggles(Arc::new(
          crate::debug::runtime::RuntimeToggles::from_env(),
        ));
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
    assert_eq!(
      Orientation::parse("portrait").unwrap(),
      Orientation::Portrait
    );
    assert_eq!(
      Orientation::parse("landscape").unwrap(),
      Orientation::Landscape
    );
  }

  #[test]
  fn test_orientation_case_insensitive() {
    assert_eq!(
      Orientation::parse("PORTRAIT").unwrap(),
      Orientation::Portrait
    );
    assert_eq!(
      Orientation::parse("Landscape").unwrap(),
      Orientation::Landscape
    );
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

    assert!(Resolution::parse("-1dpi").is_err());
    assert!(Resolution::parse("infdppx").is_err());

    // Case-insensitive units are accepted
    let upper = Resolution::parse("1DPI").unwrap();
    assert_eq!(upper.value, 1.0);
    assert_eq!(upper.unit, ResolutionUnit::Dpi);

    // High-precision fractional values are preserved
    let precise = Resolution::parse("1.3333dppx").unwrap();
    assert!((precise.value - 1.3333).abs() < 1e-6);

    // Zero is valid
    let zero = Resolution::parse("0dppx").unwrap();
    assert_eq!(zero.value, 0.0);
    assert_eq!(zero.unit, ResolutionUnit::Dppx);
  }

  #[test]
  fn test_resolution_to_dppx() {
    assert_eq!(Resolution::dppx(2.0).to_dppx(), 2.0);
    assert!((Resolution::dpi(96.0).to_dppx() - 1.0).abs() < 0.01);
    assert!((Resolution::dpi(192.0).to_dppx() - 2.0).abs() < 0.01);
  }

  #[test]
  fn calc_length_media_resolution_uses_viewport_units() {
    let ctx = MediaContext::screen(200.0, 100.0);
    let calc = Length::calc(CalcLength::single(LengthUnit::Vw, 10.0));

    let resolved = ctx.resolve_length(&calc, ctx.viewport_width, ctx.viewport_height);
    assert_eq!(resolved, Some(20.0));
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
  fn test_media_feature_rejects_percentage_width() {
    assert!(MediaFeature::parse("min-width", Some("50%")).is_err());
  }

  #[test]
  fn test_media_feature_rejects_negative_width() {
    assert!(MediaFeature::parse("max-width", Some("-10px")).is_err());
  }

  #[test]
  fn range_percentage_uses_block_axis_base_for_height() {
    let ctx = MediaContext {
      viewport_width: 50.0,
      viewport_height: 100.0,
      ..MediaContext::screen(50.0, 100.0)
    };
    let query = MediaQuery::parse("(min-height: 60vh)").expect("parse query");
    assert!(ctx.evaluate(&query));

    let query_fail = MediaQuery::parse("(min-height: 120vh)").expect("parse query");
    assert!(!ctx.evaluate(&query_fail));
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
    let feature = MediaFeature::parse("prefers-color-scheme", Some("light")).unwrap();
    assert_eq!(
      feature,
      MediaFeature::PrefersColorScheme(ColorScheme::Light)
    );
    let feature = MediaFeature::parse("prefers-color-scheme", Some("no-preference")).unwrap();
    assert_eq!(
      feature,
      MediaFeature::PrefersColorScheme(ColorScheme::NoPreference)
    );
    assert!(MediaFeature::parse("prefers-color-scheme", Some("unknown")).is_err());
    let upper =
      MediaFeature::parse("prefers-color-scheme", Some("DARK")).expect("case-insensitive parse");
    assert_eq!(upper, MediaFeature::PrefersColorScheme(ColorScheme::Dark));
  }

  #[test]
  fn test_media_feature_parse_inverted_colors() {
    let feature = MediaFeature::parse("inverted-colors", Some("inverted")).unwrap();
    assert_eq!(
      feature,
      MediaFeature::InvertedColors(InvertedColors::Inverted)
    );
    let feature = MediaFeature::parse("inverted-colors", Some("none")).unwrap();
    assert_eq!(feature, MediaFeature::InvertedColors(InvertedColors::None));
    assert!(MediaFeature::parse("inverted-colors", Some("foo")).is_err());
  }

  #[test]
  fn test_media_feature_parse_color_gamut() {
    assert_eq!(
      MediaFeature::parse("color-gamut", Some("srgb")).unwrap(),
      MediaFeature::ColorGamut(ColorGamut::Srgb)
    );
    assert_eq!(
      MediaFeature::parse("color-gamut", Some("display-p3")).unwrap(),
      MediaFeature::ColorGamut(ColorGamut::P3)
    );
    assert_eq!(
      MediaFeature::parse("color-gamut", Some("rec-2020")).unwrap(),
      MediaFeature::ColorGamut(ColorGamut::Rec2020)
    );

    assert!(MediaFeature::parse("color-gamut", Some("unknown")).is_err());
    assert!(MediaFeature::parse("color-gamut", None).is_err());
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
  fn test_evaluate_any_pointer() {
    let desktop = MediaContext::screen(1024.0, 768.0);
    let mobile = MediaContext::mobile(375.0, 667.0);
    let printer = MediaContext::print(816.0, 1056.0);

    let fine_query = MediaQuery::parse("(any-pointer: fine)").unwrap();
    let coarse_query = MediaQuery::parse("(any-pointer: coarse)").unwrap();
    let none_query = MediaQuery::parse("(any-pointer: none)").unwrap();

    assert!(desktop.evaluate(&fine_query));
    assert!(!desktop.evaluate(&coarse_query));
    assert!(!desktop.evaluate(&none_query));

    assert!(!mobile.evaluate(&fine_query));
    assert!(mobile.evaluate(&coarse_query));
    assert!(!mobile.evaluate(&none_query));

    assert!(!printer.evaluate(&fine_query));
    assert!(!printer.evaluate(&coarse_query));
    assert!(printer.evaluate(&none_query));
  }

  #[test]
  fn media_query_with_non_finite_viewport_does_not_match() {
    let mut ctx = MediaContext::screen(f32::NAN, 800.0);
    let width_query = MediaQuery::parse("(min-width: 10vw)").unwrap();
    assert!(!ctx.evaluate(&width_query));

    ctx.viewport_width = 800.0;
    ctx.viewport_height = f32::NAN;
    let height_query = MediaQuery::parse("(min-height: 10vh)").unwrap();
    assert!(!ctx.evaluate(&height_query));
  }

  #[test]
  fn test_evaluate_any_pointer_hybrid() {
    let hybrid = MediaContext {
      any_pointer_coarse: true,
      any_pointer_fine: true,
      ..MediaContext::screen(1200.0, 800.0)
    };

    let fine_query = MediaQuery::parse("(any-pointer: fine)").unwrap();
    let coarse_query = MediaQuery::parse("(any-pointer: coarse)").unwrap();
    let none_query = MediaQuery::parse("(any-pointer: none)").unwrap();

    assert!(hybrid.evaluate(&fine_query));
    assert!(hybrid.evaluate(&coarse_query));
    assert!(!hybrid.evaluate(&none_query));
  }

  #[test]
  fn test_evaluate_prefers_color_scheme() {
    let dark_ctx = MediaContext::screen(1024.0, 768.0).with_color_scheme(ColorScheme::Dark);
    let light_ctx = MediaContext::screen(1024.0, 768.0).with_color_scheme(ColorScheme::Light);
    let no_pref_ctx = MediaContext::screen(1024.0, 768.0);

    let dark_query = MediaQuery::parse("(prefers-color-scheme: dark)").unwrap();
    let light_query = MediaQuery::parse("(prefers-color-scheme: light)").unwrap();
    let no_pref_query = MediaQuery::parse("(prefers-color-scheme: no-preference)").unwrap();

    assert!(dark_ctx.evaluate(&dark_query));
    assert!(!dark_ctx.evaluate(&light_query));
    assert!(!dark_ctx.evaluate(&no_pref_query));

    assert!(light_ctx.evaluate(&light_query));
    assert!(!light_ctx.evaluate(&dark_query));
    assert!(!light_ctx.evaluate(&no_pref_query));

    assert!(no_pref_ctx.evaluate(&no_pref_query));
    assert!(!no_pref_ctx.evaluate(&dark_query));
    assert!(!no_pref_ctx.evaluate(&light_query));
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
  fn test_evaluate_prefers_contrast() {
    let ctx = MediaContext::screen(1024.0, 768.0).with_prefers_contrast(ContrastPreference::More);

    let query = MediaQuery::parse("(prefers-contrast: more)").unwrap();
    assert!(ctx.evaluate(&query));

    let query = MediaQuery::parse("(prefers-contrast: less)").unwrap();
    assert!(!ctx.evaluate(&query));

    let default_ctx = MediaContext::screen(800.0, 600.0);
    let no_pref = MediaQuery::parse("(prefers-contrast: no-preference)").unwrap();
    assert!(default_ctx.evaluate(&no_pref));
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
  fn test_evaluate_prefers_reduced_transparency() {
    let ctx = MediaContext::screen(1024.0, 768.0).with_reduced_transparency(true);

    let query = MediaQuery::parse("(prefers-reduced-transparency: reduce)").unwrap();
    assert!(ctx.evaluate(&query));

    let query = MediaQuery::parse("(prefers-reduced-transparency: no-preference)").unwrap();
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
  fn range_resolution_accepts_x_alias() {
    let mut ctx = MediaContext::screen(1024.0, 768.0);
    ctx.device_pixel_ratio = 2.0;

    let res_query = MediaQuery::parse("(resolution >= 2x)").unwrap();
    assert!(ctx.evaluate(&res_query));

    let miss = MediaQuery::parse("(resolution > 3x)").unwrap();
    assert!(!ctx.evaluate(&miss));
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
  fn range_equality_rejects_percentages() {
    assert!(MediaQuery::parse("(width = 50%)").is_err());
    assert!(MediaQuery::parse("(50% = width)").is_err());
  }

  #[test]
  fn range_length_rejects_percentage_and_invalid_units() {
    assert!(MediaQuery::parse("(width > 50%)").is_err());
    assert!(MediaQuery::parse("(50% < width < 100%)").is_err());
    assert!(MediaQuery::parse("(width > 10foo)").is_err());
  }

  #[test]
  fn range_resolution_rejects_invalid_units() {
    assert!(MediaQuery::parse("(resolution < 2abc)").is_err());
    assert!(MediaQuery::parse("(2abc < resolution)").is_err());
  }

  #[test]
  fn env_overrides_color_scheme_and_motion() {
    let guard_scheme = EnvGuard::new("FASTR_PREFERS_COLOR_SCHEME", Some("dark"));
    let guard_motion = EnvGuard::new("FASTR_PREFERS_REDUCED_MOTION", Some("reduce"));
    let guard_contrast = EnvGuard::new("FASTR_PREFERS_CONTRAST", Some("high"));
    let guard_transparency = EnvGuard::new("FASTR_PREFERS_REDUCED_TRANSPARENCY", Some("1"));
    let guard_data = EnvGuard::new("FASTR_PREFERS_REDUCED_DATA", Some("yes"));
    let guard_gamut = EnvGuard::new("FASTR_COLOR_GAMUT", Some("p3"));
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
    assert_eq!(ctx.color_gamut, ColorGamut::P3);
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
    drop(guard_gamut);
    drop(guard_inverted);
    drop(guard_color_depth);
    drop(guard_color_index);
    drop(guard_mono);
    drop(guard_forced);
  }

  #[test]
  fn env_overrides_color_scheme_to_no_preference() {
    let guard_scheme = EnvGuard::new("FASTR_PREFERS_COLOR_SCHEME", Some("no-preference"));

    let dark_query = MediaQuery::parse("(prefers-color-scheme: dark)").unwrap();
    let no_pref_query = MediaQuery::parse("(prefers-color-scheme: no-preference)").unwrap();

    let ctx = MediaContext::screen(800.0, 600.0)
      .with_color_scheme(ColorScheme::Dark)
      .with_env_overrides();
    assert_eq!(ctx.prefers_color_scheme, Some(ColorScheme::NoPreference));
    assert!(ctx.evaluate(&no_pref_query));
    assert!(!ctx.evaluate(&dark_query));

    drop(guard_scheme);
  }

  #[test]
  fn env_overrides_ignore_invalid_values() {
    let guard_scheme = EnvGuard::new("FASTR_PREFERS_COLOR_SCHEME", Some("invalid"));
    let guard_gamut = EnvGuard::new("FASTR_COLOR_GAMUT", Some("nope"));
    let guard_inverted = EnvGuard::new("FASTR_INVERTED_COLORS", Some("maybe"));
    let guard_color_depth = EnvGuard::new("FASTR_COLOR_DEPTH", Some("abc"));
    let guard_forced = EnvGuard::new("FASTR_FORCED_COLORS", Some("maybe"));
    let guard_transparency = EnvGuard::new("FASTR_PREFERS_REDUCED_TRANSPARENCY", Some("maybe"));
    let guard_contrast = EnvGuard::new("FASTR_PREFERS_CONTRAST", Some("maybe"));
    let ctx = MediaContext::screen(800.0, 600.0)
      .with_color_scheme(ColorScheme::Light)
      .with_env_overrides();
    assert_eq!(ctx.prefers_color_scheme, Some(ColorScheme::Light));
    assert_eq!(ctx.color_gamut, ColorGamut::Srgb);
    assert!(matches!(ctx.inverted_colors, InvertedColors::None));
    assert_eq!(ctx.color_depth, 8);
    assert!(!ctx.forced_colors);
    assert!(!ctx.prefers_reduced_transparency);
    assert!(matches!(
      ctx.prefers_contrast,
      ContrastPreference::NoPreference
    ));
    drop(guard_scheme);
    drop(guard_gamut);
    drop(guard_inverted);
    drop(guard_color_depth);
    drop(guard_forced);
    drop(guard_transparency);
    drop(guard_contrast);
  }

  #[test]
  fn env_overrides_no_preference_scheme() {
    let guard_scheme = EnvGuard::new("FASTR_PREFERS_COLOR_SCHEME", Some("no-preference"));
    let ctx = MediaContext::screen(800.0, 600.0)
      .with_color_scheme(ColorScheme::Dark)
      .with_env_overrides();
    assert_eq!(ctx.prefers_color_scheme, Some(ColorScheme::NoPreference));
    drop(guard_scheme);
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
  fn test_evaluate_color_gamut() {
    let mut ctx = MediaContext::screen(1024.0, 768.0);

    let srgb_query = MediaQuery::parse("(color-gamut: srgb)").unwrap();
    let p3_query = MediaQuery::parse("(color-gamut: p3)").unwrap();
    let rec_query = MediaQuery::parse("(color-gamut: rec2020)").unwrap();

    assert!(ctx.evaluate(&srgb_query));
    assert!(!ctx.evaluate(&p3_query));
    assert!(!ctx.evaluate(&rec_query));

    ctx.color_gamut = ColorGamut::P3;
    assert!(ctx.evaluate(&srgb_query));
    assert!(ctx.evaluate(&p3_query));
    assert!(!ctx.evaluate(&rec_query));

    ctx.color_gamut = ColorGamut::Rec2020;
    assert!(ctx.evaluate(&srgb_query));
    assert!(ctx.evaluate(&p3_query));
    assert!(ctx.evaluate(&rec_query));
  }

  #[test]
  fn test_evaluate_aspect_ratio() {
    let ctx = MediaContext::screen(1920.0, 1080.0); // 16:9

    let query = MediaQuery::parse("(min-aspect-ratio: 16/9)").unwrap();
    assert!(ctx.evaluate(&query));

    let query = MediaQuery::parse("(max-aspect-ratio: 4/3)").unwrap();
    assert!(!ctx.evaluate(&query));
  }

  #[test]
  fn aspect_ratio_zero_denominator_rejected() {
    assert!(MediaQuery::parse("(aspect-ratio: 16/0)").is_err());
    assert!(MediaQuery::parse("(max-aspect-ratio: 0/0)").is_err());
  }

  #[test]
  fn device_dimensions_reject_negative_values() {
    assert!(MediaQuery::parse("(device-width: -1px)").is_err());
    assert!(MediaQuery::parse("(max-device-width: -100px)").is_err());
    assert!(MediaQuery::parse("(min-device-height: -10px)").is_err());
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
