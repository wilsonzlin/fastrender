//! Painting and rasterization
//!
//! This module handles converting the positioned fragment tree into pixels.
//!
//! # Responsibilities
//!
//! - **Painting**: Draw backgrounds, borders, text, images
//! - **Rasterization**: Convert vector graphics to pixels using tiny-skia
//!
//! # Architecture
//!
//! The painter walks the fragment tree depth-first, painting each
//! fragment's content. Future work will add:
//! - Display list building for optimization
//! - Stacking context sorting for z-index
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

pub mod painter;

pub use painter::{paint_tree, Painter};
