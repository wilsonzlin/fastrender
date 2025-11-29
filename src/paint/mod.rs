//! Painting and rasterization
//!
//! This module handles converting the positioned fragment tree into pixels.
//!
//! # Responsibilities
//!
//! - **Display List**: Intermediate representation of paint commands
//! - **Painting**: Draw backgrounds, borders, text, images
//! - **Rasterization**: Convert vector graphics to pixels using tiny-skia
//!
//! # Architecture
//!
//! The paint system uses a two-phase approach:
//! 1. **Display List Building**: Convert fragment tree to flat list of paint commands
//! 2. **Rasterization**: Execute paint commands to produce pixels
//!
//! The display list provides:
//! - Viewport culling (skip items outside visible area)
//! - Effect stack management (opacity, transforms, clips)
//! - Optimization opportunities (batching, merging)
//!
//! # Painting Order
//!
//! CSS defines a specific painting order (CSS 2.1 Appendix E):
//!
//! 1. Background colors and images
//! 2. Borders
//! 3. Child stacking contexts (negative z-index)
//! 4. In-flow non-positioned blocks
//! 5. Floats
//! 6. In-flow inline content
//! 7. Child stacking contexts (z-index: 0 and auto)
//! 8. Positioned descendants (positive z-index)
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::paint::{Painter, paint_tree};
//! use fastrender::css::Color;
//!
//! let pixmap = paint_tree(&fragment_tree, 800, 600, Color::WHITE)?;
//! ```

pub mod display_list;
pub mod painter;

// Re-export display list types
pub use display_list::{
    BlendMode, BlendModeItem, BorderRadii, BoxShadowItem, ClipItem, DisplayItem, DisplayList,
    FillRectItem, FillRoundedRectItem, FontId, GlyphInstance, GradientStop, ImageData, ImageItem,
    LinearGradientItem, OpacityItem, RadialGradientItem, StackingContextItem, StrokeRectItem,
    StrokeRoundedRectItem, TextItem, Transform2D, TransformItem,
};

pub use painter::{paint_tree, Painter};
