//! FastRender: High-performance Rust HTML/CSS to image renderer
//!
//! FastRender is a rendering engine that converts HTML and CSS into images.
//! It implements the CSS 2.1 specification with select CSS3 features.
//!
//! # Architecture
//!
//! FastRender uses a pipeline architecture with distinct phases:
//!
//! ```text
//! HTML/CSS → Parsing → Styling → Layout → Painting → Image
//! ```
//!
//! ## Major Subsystems
//!
//! - **tree**: DOM, box tree, and fragment tree structures
//! - **style**: CSS parsing, cascade, and computed styles
//! - **layout**: Layout algorithms (block, inline, flex, grid, table)
//! - **text**: Text shaping, font handling, line breaking
//! - **paint**: Painting and rasterization
//!
//! ## Foundation Types
//!
//! - **geometry**: Core geometric primitives (Point, Size, Rect, EdgeOffsets)
//! - **error**: Comprehensive error types for all subsystems
//!
//! # Quick Start
//!
//! ```rust,ignore
//! use fastrender::{Renderer, RenderOptions};
//!
//! let html = r#"
//!     <!DOCTYPE html>
//!     <html>
//!         <body>
//!             <h1>Hello, FastRender!</h1>
//!         </body>
//!     </html>
//! "#;
//!
//! let renderer = Renderer::new();
//! let image = renderer.render(html, RenderOptions::default())?;
//! image.save_png("output.png")?;
//! ```
//!
//! # Feature Flags
//!
//! Currently FastRender has no feature flags - all features are enabled by default.
//!
//! # CSS Support
//!
//! FastRender implements:
//! - CSS 2.1 (full specification)
//! - Flexbox (CSS Flexible Box Layout Module Level 1)
//! - Grid (CSS Grid Layout Module Level 1)
//! - Select CSS3 features (border-radius, box-shadow, opacity)
//!
//! # Performance
//!
//! FastRender is optimized for:
//! - Single-threaded rendering (no thread synchronization overhead)
//! - Minimal allocations (use of arena allocators where beneficial)
//! - Cache-friendly data structures (struct-of-arrays where appropriate)

// ============================================================================
// Foundation Types
// ============================================================================

pub mod geometry;
pub mod error;

// ============================================================================
// Tree Structures
// ============================================================================

pub mod tree;

// ============================================================================
// Style System
// ============================================================================

pub mod style;

// ============================================================================
// Layout System
// ============================================================================

pub mod layout;

// ============================================================================
// Text System
// ============================================================================

pub mod text;

// ============================================================================
// Paint System
// ============================================================================

pub mod paint;

// ============================================================================
// Legacy Modules (to be refactored into above structure)
// ============================================================================

// These modules exist from FastRender V1 and will be gradually
// refactored into the new architecture above
pub mod css;
pub mod dom;
pub mod image_loader;
pub mod image_output;
pub mod renderer;

// ============================================================================
// Public API Re-exports
// ============================================================================

// Error types
pub use error::{Error, Result};

// Geometry types
pub use geometry::{EdgeOffsets, Point, Rect, Size};

// Renderer (main entry point)
pub use renderer::{ImageFormat, RenderOptions, Renderer};

// Style types from Wave 1 tasks
pub use style::color::{ColorParseError, Hsla, Rgba};
pub use style::display::{Display, DisplayParseError};
pub use style::position::{Position, PositionParseError};
pub use style::{Color, Length, LengthOrAuto, LengthUnit};
