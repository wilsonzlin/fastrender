//! Painting and rasterization
//!
//! This module handles converting the positioned fragment tree into pixels.
//!
//! # Responsibilities
//!
//! - **Display List**: Intermediate representation of paint commands
//! - **Display List Builder**: Convert fragment tree to display list
//! - **Painting**: Execute paint commands via rasterization
//!
//! # Architecture
//!
//! The paint system uses a two-phase approach:
//!
//! ```text
//! Fragment Tree → Display List Builder → Display List → Rasterizer → Pixels
//! ```
//!
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
//! use fastrender::paint::{DisplayListBuilder, DisplayList};
//!
//! let builder = DisplayListBuilder::new();
//! let display_list = builder.build(&fragment_tree);
//! ```

pub mod display_list;
pub mod display_list_builder;
pub mod painter;
pub mod stacking;

// Re-export display list types (W5.T01)
pub use display_list::{
    BlendMode, BlendModeItem, BorderRadii, BoxShadowItem, ClipItem, DisplayItem, DisplayList, FillRectItem,
    FillRoundedRectItem, FontId, GlyphInstance, GradientStop, ImageData, ImageItem, LinearGradientItem, OpacityItem,
    RadialGradientItem, StackingContextItem, StrokeRectItem, StrokeRoundedRectItem, TextItem, Transform2D,
    TransformItem,
};

// Re-export display list builder (W5.T02)
pub use display_list_builder::DisplayListBuilder;

// Re-export painter
pub use painter::{paint_tree, Painter};
pub use stacking::{
    build_stacking_tree, build_stacking_tree_with_styles, creates_stacking_context,
    get_stacking_context_reason, PaintOrderIterator, StackingContext, StackingContextReason,
    StyledFragmentRef,
};
