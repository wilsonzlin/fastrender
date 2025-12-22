//! Error types for FastRender
//!
//! This module provides comprehensive error types for all subsystems:
//! - Parse errors (HTML/CSS)
//! - Layout errors (box generation, layout algorithms)
//! - Font errors (loading, shaping)
//! - Image errors (loading, decoding)
//! - Render errors (painting, rasterization)
//!
//! All errors use the `thiserror` crate for minimal boilerplate and
//! proper error trait implementations.

use thiserror::Error;

/// Result type alias for FastRender operations
///
/// This is a convenience type that uses our Error type as the error variant.
///
/// # Examples
///
/// ```
/// use fastrender::Result;
///
/// fn parse_html(html: &str) -> Result<()> {
///     Ok(())
/// }
/// ```
pub type Result<T> = std::result::Result<T, Error>;

/// Top-level error type for FastRender
///
/// This enum covers all possible errors that can occur during rendering.
/// Each variant wraps a more specific error type for that subsystem.
///
/// # Examples
///
/// ```
/// use fastrender::Error;
/// use fastrender::error::LayoutError;
///
/// fn render() -> Result<(), Error> {
///     Err(Error::Layout(LayoutError::InvalidConstraints {
///         message: "Width cannot be negative".to_string(),
///     }))
/// }
/// ```
#[derive(Error, Debug)]
pub enum Error {
  /// HTML or CSS parsing error
  #[error("Parse error: {0}")]
  Parse(#[from] ParseError),

  /// Style computation error
  #[error("Style error: {0}")]
  Style(#[from] StyleError),

  /// Layout error
  #[error("Layout error: {0}")]
  Layout(#[from] LayoutError),

  /// Font loading or shaping error
  #[error("Font error: {0}")]
  Font(#[from] FontError),

  /// Text shaping or line breaking error
  #[error("Text error: {0}")]
  Text(#[from] TextError),

  /// Image loading or decoding error
  #[error("Image error: {0}")]
  Image(#[from] ImageError),

  /// Rendering or rasterization error
  #[error("Render error: {0}")]
  Render(#[from] RenderError),

  /// Network/navigation error when fetching documents
  #[error("Navigation error: {0}")]
  Navigation(#[from] NavigationError),

  /// I/O error (file reading, network, etc.)
  #[error("I/O error: {0}")]
  Io(#[from] std::io::Error),

  /// Generic error for miscellaneous issues
  #[error("{0}")]
  Other(String),
}

/// Errors that occur during HTML or CSS parsing
///
/// These errors indicate that the input HTML or CSS could not be parsed
/// according to the specifications.
///
/// # Examples
///
/// ```
/// use fastrender::error::ParseError;
///
/// let error = ParseError::InvalidCss {
///     message: "Expected selector, found '{'".to_string(),
///     line: 10,
///     column: 5,
/// };
/// ```
#[derive(Error, Debug, Clone)]
pub enum ParseError {
  /// Invalid HTML structure
  #[error("Invalid HTML at line {line}: {message}")]
  InvalidHtml { message: String, line: usize },

  /// Invalid CSS syntax
  #[error("Invalid CSS at line {line}, column {column}: {message}")]
  InvalidCss {
    message: String,
    line: usize,
    column: usize,
  },

  /// Invalid selector
  #[error("Invalid selector: {selector}")]
  InvalidSelector { selector: String },

  /// Invalid property value
  #[error("Invalid value for property '{property}': {value}")]
  InvalidPropertyValue { property: String, value: String },

  /// Unsupported CSS feature
  #[error("Unsupported CSS feature: {feature}")]
  UnsupportedFeature { feature: String },
}

/// Errors that occur during style computation
///
/// These errors happen during the cascade, inheritance, and computed
/// value calculation phases.
#[derive(Error, Debug, Clone)]
pub enum StyleError {
  /// Property value cannot be computed
  #[error("Cannot compute value for property '{property}': {reason}")]
  CannotComputeValue { property: String, reason: String },

  /// Circular dependency in custom properties (CSS variables)
  #[error("Circular dependency detected in custom property '{property}'")]
  CircularDependency { property: String },

  /// Invalid inherit or initial value usage
  #[error("Invalid use of '{keyword}' for property '{property}'")]
  InvalidKeyword { property: String, keyword: String },
}

/// Errors that occur during layout computation
///
/// These errors happen during box generation, layout algorithm execution,
/// or fragment tree construction.
///
/// # Examples
///
/// ```
/// use fastrender::error::LayoutError;
///
/// let error = LayoutError::InvalidConstraints {
///     message: "Width cannot be negative: -100px".to_string(),
/// };
/// println!("{}", error);
/// ```
#[derive(Error, Debug, Clone)]
pub enum LayoutError {
  /// Invalid layout constraints
  #[error("Invalid layout constraints: {message}")]
  InvalidConstraints { message: String },

  /// Box generation failed
  #[error("Box generation failed: {message}")]
  BoxGenerationFailed { message: String },

  /// Unsupported display value combination
  #[error("Unsupported display value: {display}")]
  UnsupportedDisplay { display: String },

  /// Percentage resolution failed (no containing block)
  #[error("Cannot resolve percentage: {property} has no containing block")]
  PercentageResolutionFailed { property: String },

  /// Intrinsic size calculation failed
  #[error("Cannot calculate intrinsic size: {reason}")]
  IntrinsicSizeFailed { reason: String },

  /// Constraint solver failed (for flex/grid)
  #[error("Constraint solver failed: {message}")]
  ConstraintSolverFailed { message: String },
}

/// Errors that occur during font loading and resolution
///
/// These errors happen when fonts cannot be loaded from the system or
/// font fallback fails.
///
/// # Examples
///
/// ```
/// use fastrender::error::FontError;
///
/// let error = FontError::FontNotFound {
///     family: "Helvetica Neue".to_string(),
/// };
/// ```
#[derive(Error, Debug, Clone)]
pub enum FontError {
  /// Font family not found
  #[error("Font family not found: '{family}'")]
  FontNotFound { family: String },

  /// No fonts available (not even fallback fonts)
  #[error("No fonts available on system")]
  NoFontsAvailable,

  /// Font file is invalid or corrupted
  #[error("Invalid font file: {path}")]
  InvalidFontFile { path: String },

  /// Font loading failed
  #[error("Failed to load font '{family}': {reason}")]
  LoadFailed { family: String, reason: String },

  /// Font database query failed
  #[error("Font database query failed: {reason}")]
  DatabaseQueryFailed { reason: String },
}

/// Errors that occur during text shaping and line breaking
///
/// These errors happen during text layout operations like shaping
/// with HarfBuzz or computing line breaks.
#[derive(Error, Debug, Clone)]
pub enum TextError {
  /// Text shaping failed
  #[error("Text shaping failed for text '{text}': {reason}")]
  ShapingFailed { text: String, reason: String },

  /// Line breaking algorithm failed
  #[error("Line breaking failed: {reason}")]
  LineBreakingFailed { reason: String },

  /// Bidi algorithm failed
  #[error("Bidi algorithm failed: {reason}")]
  BidiFailed { reason: String },

  /// Glyph not found in font
  #[error("Glyph not found for character U+{codepoint:04X}")]
  GlyphNotFound { codepoint: u32 },

  /// Hyphenation failed
  #[error("Hyphenation failed for language '{language}': {reason}")]
  HyphenationFailed { language: String, reason: String },
}

/// Errors that occur during image loading and decoding
///
/// These errors happen when images cannot be loaded from URLs,
/// data URLs, or local paths, or when decoding fails.
///
/// # Examples
///
/// ```
/// use fastrender::error::ImageError;
///
/// let error = ImageError::LoadFailed {
///     url: "https://example.com/image.png".to_string(),
///     reason: "404 Not Found".to_string(),
/// };
/// ```
#[derive(Error, Debug, Clone)]
pub enum ImageError {
  /// Image loading failed
  #[error("Failed to load image from '{url}': {reason}")]
  LoadFailed { url: String, reason: String },

  /// Image decoding failed
  #[error("Failed to decode image from '{url}': {reason}")]
  DecodeFailed { url: String, reason: String },

  /// Invalid image format
  #[error("Invalid image format for '{url}': {format}")]
  InvalidFormat { url: String, format: String },

  /// Invalid data URL
  #[error("Invalid data URL: {reason}")]
  InvalidDataUrl { reason: String },

  /// Network error (for remote images)
  #[error("Network error loading '{url}': {status}")]
  NetworkError { url: String, status: String },
}

/// Errors that occur while fetching the root document for rendering.
#[derive(Error, Debug, Clone)]
pub enum NavigationError {
  /// The document could not be fetched or decoded.
  #[error("Failed to fetch document '{url}': {reason}")]
  FetchFailed { url: String, reason: String },
}

/// Errors that occur during rendering and rasterization
///
/// These errors happen during the paint phase when converting
/// the fragment tree to pixels.
#[derive(Error, Debug, Clone)]
pub enum RenderError {
  /// Canvas creation failed
  #[error("Failed to create canvas: {width}x{height}")]
  CanvasCreationFailed { width: u32, height: u32 },

  /// Paint operation failed
  #[error("Paint operation failed: {operation}")]
  PaintFailed { operation: String },

  /// Image encoding failed
  #[error("Failed to encode image as {format}: {reason}")]
  EncodeFailed { format: String, reason: String },

  /// Invalid paint parameters
  #[error("Invalid paint parameters: {message}")]
  InvalidParameters { message: String },

  /// Rasterization failed
  #[error("Rasterization failed: {reason}")]
  RasterizationFailed { reason: String },
}

#[cfg(test)]
mod tests {
  use super::*;

  // ParseError tests
  #[test]
  fn test_parse_error_invalid_html() {
    let error = ParseError::InvalidHtml {
      message: "Unexpected closing tag".to_string(),
      line: 10,
    };
    let display = format!("{}", error);
    assert!(display.contains("line 10"));
    assert!(display.contains("Unexpected closing tag"));
  }

  #[test]
  fn test_parse_error_invalid_css() {
    let error = ParseError::InvalidCss {
      message: "Expected selector".to_string(),
      line: 5,
      column: 10,
    };
    let display = format!("{}", error);
    assert!(display.contains("line 5"));
    assert!(display.contains("column 10"));
  }

  #[test]
  fn test_parse_error_invalid_selector() {
    let error = ParseError::InvalidSelector {
      selector: "div > > p".to_string(),
    };
    assert!(format!("{}", error).contains("div > > p"));
  }

  #[test]
  fn test_parse_error_invalid_property_value() {
    let error = ParseError::InvalidPropertyValue {
      property: "width".to_string(),
      value: "invalid".to_string(),
    };
    let display = format!("{}", error);
    assert!(display.contains("width"));
    assert!(display.contains("invalid"));
  }

  #[test]
  fn test_parse_error_unsupported_feature() {
    let error = ParseError::UnsupportedFeature {
      feature: "grid-template-areas".to_string(),
    };
    assert!(format!("{}", error).contains("grid-template-areas"));
  }

  // StyleError tests
  #[test]
  fn test_style_error_cannot_compute_value() {
    let error = StyleError::CannotComputeValue {
      property: "width".to_string(),
      reason: "Circular dependency".to_string(),
    };
    assert!(format!("{}", error).contains("width"));
    assert!(format!("{}", error).contains("Circular dependency"));
  }

  #[test]
  fn test_style_error_circular_dependency() {
    let error = StyleError::CircularDependency {
      property: "--my-color".to_string(),
    };
    assert!(format!("{}", error).contains("--my-color"));
  }

  #[test]
  fn test_style_error_invalid_keyword() {
    let error = StyleError::InvalidKeyword {
      property: "display".to_string(),
      keyword: "inherit".to_string(),
    };
    let display = format!("{}", error);
    assert!(display.contains("display"));
    assert!(display.contains("inherit"));
  }

  // LayoutError tests
  #[test]
  fn test_layout_error_invalid_constraints() {
    let error = LayoutError::InvalidConstraints {
      message: "Width cannot be negative".to_string(),
    };
    assert!(format!("{}", error).contains("Invalid layout constraints"));
  }

  #[test]
  fn test_layout_error_box_generation_failed() {
    let error = LayoutError::BoxGenerationFailed {
      message: "Cannot create box".to_string(),
    };
    assert!(format!("{}", error).contains("Box generation failed"));
  }

  #[test]
  fn test_layout_error_unsupported_display() {
    let error = LayoutError::UnsupportedDisplay {
      display: "ruby".to_string(),
    };
    assert!(format!("{}", error).contains("ruby"));
  }

  #[test]
  fn test_layout_error_percentage_resolution() {
    let error = LayoutError::PercentageResolutionFailed {
      property: "width".to_string(),
    };
    assert!(format!("{}", error).contains("percentage"));
  }

  #[test]
  fn test_layout_error_intrinsic_size_failed() {
    let error = LayoutError::IntrinsicSizeFailed {
      reason: "No content".to_string(),
    };
    assert!(format!("{}", error).contains("intrinsic size"));
  }

  #[test]
  fn test_layout_error_constraint_solver_failed() {
    let error = LayoutError::ConstraintSolverFailed {
      message: "Cannot solve".to_string(),
    };
    assert!(format!("{}", error).contains("Constraint solver failed"));
  }

  // FontError tests
  #[test]
  fn test_font_error_not_found() {
    let error = FontError::FontNotFound {
      family: "Comic Sans".to_string(),
    };
    assert!(format!("{}", error).contains("Comic Sans"));
  }

  #[test]
  fn test_font_error_no_fonts_available() {
    let error = FontError::NoFontsAvailable;
    assert!(format!("{}", error).contains("No fonts available"));
  }

  #[test]
  fn test_font_error_invalid_font_file() {
    let error = FontError::InvalidFontFile {
      path: "/path/to/font.ttf".to_string(),
    };
    assert!(format!("{}", error).contains("/path/to/font.ttf"));
  }

  #[test]
  fn test_font_error_load_failed() {
    let error = FontError::LoadFailed {
      family: "Arial".to_string(),
      reason: "File not found".to_string(),
    };
    let display = format!("{}", error);
    assert!(display.contains("Arial"));
    assert!(display.contains("File not found"));
  }

  #[test]
  fn test_font_error_database_query_failed() {
    let error = FontError::DatabaseQueryFailed {
      reason: "Database corrupted".to_string(),
    };
    assert!(format!("{}", error).contains("Database corrupted"));
  }

  // TextError tests
  #[test]
  fn test_text_error_shaping_failed() {
    let error = TextError::ShapingFailed {
      text: "Hello".to_string(),
      reason: "Font missing".to_string(),
    };
    assert!(format!("{}", error).contains("Hello"));
  }

  #[test]
  fn test_text_error_line_breaking_failed() {
    let error = TextError::LineBreakingFailed {
      reason: "Invalid break point".to_string(),
    };
    assert!(format!("{}", error).contains("Line breaking failed"));
  }

  #[test]
  fn test_text_error_bidi_failed() {
    let error = TextError::BidiFailed {
      reason: "Invalid Unicode".to_string(),
    };
    assert!(format!("{}", error).contains("Bidi algorithm failed"));
  }

  #[test]
  fn test_text_error_glyph_not_found() {
    let error = TextError::GlyphNotFound {
      codepoint: 0x1f600, // 😀
    };
    assert!(format!("{}", error).contains("1F600"));
  }

  #[test]
  fn test_text_error_hyphenation_failed() {
    let error = TextError::HyphenationFailed {
      language: "xyz-lang".to_string(),
      reason: "Unsupported language".to_string(),
    };
    let display = format!("{}", error);
    assert!(display.contains("xyz-lang"));
    assert!(display.contains("Unsupported language"));
  }

  // ImageError tests
  #[test]
  fn test_image_error_load_failed() {
    let error = ImageError::LoadFailed {
      url: "https://example.com/img.png".to_string(),
      reason: "404".to_string(),
    };
    let display = format!("{}", error);
    assert!(display.contains("example.com"));
    assert!(display.contains("404"));
  }

  #[test]
  fn test_image_error_decode_failed() {
    let error = ImageError::DecodeFailed {
      url: "image.png".to_string(),
      reason: "Corrupted data".to_string(),
    };
    let display = format!("{}", error);
    assert!(display.contains("image.png"));
    assert!(display.contains("Corrupted data"));
  }

  #[test]
  fn test_image_error_invalid_format() {
    let error = ImageError::InvalidFormat {
      url: "image.xyz".to_string(),
      format: "xyz".to_string(),
    };
    assert!(format!("{}", error).contains("xyz"));
  }

  #[test]
  fn test_image_error_invalid_data_url() {
    let error = ImageError::InvalidDataUrl {
      reason: "Missing base64 data".to_string(),
    };
    assert!(format!("{}", error).contains("Invalid data URL"));
  }

  #[test]
  fn test_image_error_network_error() {
    let error = ImageError::NetworkError {
      url: "https://example.com/img.png".to_string(),
      status: "500".to_string(),
    };
    let display = format!("{}", error);
    assert!(display.contains("example.com"));
    assert!(display.contains("500"));
  }

  // RenderError tests
  #[test]
  fn test_render_error_canvas_creation() {
    let error = RenderError::CanvasCreationFailed {
      width: 10000,
      height: 10000,
    };
    assert!(format!("{}", error).contains("10000"));
  }

  #[test]
  fn test_render_error_paint_failed() {
    let error = RenderError::PaintFailed {
      operation: "fill_rect".to_string(),
    };
    assert!(format!("{}", error).contains("fill_rect"));
  }

  #[test]
  fn test_render_error_encode_failed() {
    let error = RenderError::EncodeFailed {
      format: "PNG".to_string(),
      reason: "Out of memory".to_string(),
    };
    let display = format!("{}", error);
    assert!(display.contains("PNG"));
    assert!(display.contains("Out of memory"));
  }

  #[test]
  fn test_render_error_invalid_parameters() {
    let error = RenderError::InvalidParameters {
      message: "Color out of range".to_string(),
    };
    assert!(format!("{}", error).contains("Invalid paint parameters"));
  }

  #[test]
  fn test_render_error_rasterization_failed() {
    let error = RenderError::RasterizationFailed {
      reason: "GPU error".to_string(),
    };
    assert!(format!("{}", error).contains("Rasterization failed"));
  }

  // Top-level Error tests
  #[test]
  fn test_error_from_parse_error() {
    let parse_error = ParseError::InvalidHtml {
      message: "Test".to_string(),
      line: 1,
    };
    let error: Error = parse_error.into();
    assert!(matches!(error, Error::Parse(_)));
  }

  #[test]
  fn test_error_from_style_error() {
    let style_error = StyleError::CircularDependency {
      property: "--test".to_string(),
    };
    let error: Error = style_error.into();
    assert!(matches!(error, Error::Style(_)));
  }

  #[test]
  fn test_error_from_layout_error() {
    let layout_error = LayoutError::InvalidConstraints {
      message: "Test".to_string(),
    };
    let error: Error = layout_error.into();
    assert!(matches!(error, Error::Layout(_)));
  }

  #[test]
  fn test_error_from_font_error() {
    let font_error = FontError::NoFontsAvailable;
    let error: Error = font_error.into();
    assert!(matches!(error, Error::Font(_)));
  }

  #[test]
  fn test_error_from_text_error() {
    let text_error = TextError::GlyphNotFound { codepoint: 0x41 };
    let error: Error = text_error.into();
    assert!(matches!(error, Error::Text(_)));
  }

  #[test]
  fn test_error_from_image_error() {
    let image_error = ImageError::InvalidDataUrl {
      reason: "Test".to_string(),
    };
    let error: Error = image_error.into();
    assert!(matches!(error, Error::Image(_)));
  }

  #[test]
  fn test_error_from_render_error() {
    let render_error = RenderError::PaintFailed {
      operation: "test".to_string(),
    };
    let error: Error = render_error.into();
    assert!(matches!(error, Error::Render(_)));
  }

  #[test]
  fn test_error_from_io_error() {
    let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "File not found");
    let error: Error = io_error.into();
    assert!(matches!(error, Error::Io(_)));
  }

  #[test]
  fn test_error_other() {
    let error = Error::Other("Generic error".to_string());
    assert!(format!("{}", error).contains("Generic error"));
  }

  // Result type alias test
  #[test]
  fn test_result_type_alias() {
    fn returns_result() -> Result<i32> {
      Ok(42)
    }
    assert_eq!(returns_result().unwrap(), 42);
  }

  #[test]
  fn test_result_with_error() {
    fn returns_error() -> Result<()> {
      Err(Error::Other("test error".to_string()))
    }
    assert!(returns_error().is_err());
  }

  // Error trait tests
  #[test]
  fn test_error_trait_implemented() {
    let error = Error::Other("test".to_string());
    // If this compiles, Error implements std::error::Error
    let _: &dyn std::error::Error = &error;
  }

  #[test]
  fn test_debug_formatting() {
    let error = ParseError::InvalidCss {
      message: "Test".to_string(),
      line: 1,
      column: 5,
    };
    let debug = format!("{:?}", error);
    assert!(debug.contains("InvalidCss"));
  }

  #[test]
  fn test_error_display_messages() {
    // Test that error messages are helpful and contain context
    let error = Error::Parse(ParseError::InvalidCss {
      message: "Unexpected token".to_string(),
      line: 42,
      column: 10,
    });
    let display = format!("{}", error);
    assert!(display.contains("Parse error"));
    assert!(display.contains("line 42"));
    assert!(display.contains("column 10"));
  }

  #[test]
  fn test_clone_errors() {
    // Test that all error types can be cloned
    let parse_error = ParseError::InvalidHtml {
      message: "Test".to_string(),
      line: 1,
    };
    let cloned = parse_error.clone();
    assert_eq!(format!("{}", parse_error), format!("{}", cloned));
  }
}
