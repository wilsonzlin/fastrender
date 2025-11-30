//! Painting and rasterization
//!
//! This module handles converting the positioned fragment tree into pixels.
//!
//! # Responsibilities
//!
//! - **Display List**: Intermediate representation of paint commands
//! - **Display List Builder**: Convert fragment tree to display list
//! - **Display List Optimization**: Culling, merging, and other optimizations
//! - **Stacking Contexts**: CSS stacking context tree for paint order
//! - **Canvas**: Low-level 2D graphics wrapper for tiny-skia
//! - **Path Rendering**: Shape and border rasterization
//! - **Text Rasterization**: Glyph outline to pixel conversion
//! - **Painting**: Execute paint commands via rasterization
//!
//! # Architecture
//!
//! The paint system uses a multi-phase approach:
//!
//! ```text
//! Fragment Tree → Stacking Tree → Display List → Optimize → Canvas → Pixels
//! ```
//!
//! 1. **Stacking Tree Building**: Build stacking context hierarchy from fragment tree
//! 2. **Display List Building**: Convert stacking tree to flat list of paint commands
//! 3. **Optimization**: Cull offscreen items, merge adjacent fills, remove no-ops
//! 4. **Rasterization**: Execute paint commands via Canvas to produce pixels
//!
//! Text rendering follows this pipeline:
//! 1. Get shaped text with glyph positions (from text shaping pipeline)
//! 2. Extract glyph outlines from fonts using ttf-parser
//! 3. Convert outlines to tiny-skia paths
//! 4. Render paths with anti-aliasing
//!
//! The display list provides:
//! - Viewport culling (skip items outside visible area)
//! - Effect stack management (opacity, transforms, clips)
//! - Optimization opportunities (batching, merging)
//!
//! The Canvas provides a high-level 2D graphics API:
//! - State management (save/restore for transforms, clips, opacity)
//! - Drawing primitives (rectangles, rounded rectangles, circles, lines, text)
//! - Blend modes for CSS mix-blend-mode support
//!
//! # Painting Order (CSS 2.1 Appendix E)
//!
//! Within each stacking context, elements are painted in this order (back to front):
//!
//! 1. **Layer 1**: Background and borders of the stacking context root
//! 2. **Layer 2**: Child stacking contexts with negative z-index (most negative first)
//! 3. **Layer 3**: In-flow, non-inline-level descendants (block-level, tree order)
//! 4. **Layer 4**: Non-positioned floats (tree order)
//! 5. **Layer 5**: In-flow, inline-level descendants (tree order)
//! 6. **Layer 6**: Positioned descendants with z-index 0 or auto (tree order)
//! 7. **Layer 7**: Child stacking contexts with positive z-index (least positive first)
//!
//! # Module Organization
//!
//! - `canvas` - Low-level canvas wrapper for tiny-skia 2D graphics
//! - `display_list` - Display list types for paint commands
//! - `display_list_builder` - Build display list from stacking tree
//! - `optimize` - Display list optimization passes
//! - `painter` - Main painting interface
//! - `rasterize` - Path and shape rasterization
//! - `stacking` - Stacking context tree for paint order
//! - `text_rasterize` - Text glyph rasterization
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::paint::{DisplayListBuilder, DisplayList, build_stacking_tree};
//! use fastrender::paint::{DisplayListOptimizer, OptimizationConfig};
//! use fastrender::paint::{Canvas, BorderRadii};
//! use fastrender::style::Rgba;
//!
//! // Build stacking tree from fragment tree
//! let stacking_tree = build_stacking_tree(&fragment_tree.root, None, true);
//!
//! // Build display list from stacking tree
//! let builder = DisplayListBuilder::new();
//! let display_list = builder.build(&stacking_tree);
//!
//! // Optimize display list
//! let optimizer = DisplayListOptimizer::new();
//! let viewport = Rect::from_xywh(0.0, 0.0, 800.0, 600.0);
//! let (optimized, stats) = optimizer.optimize(display_list, viewport);
//!
//! // Render to canvas
//! let mut canvas = Canvas::new(800, 600, Rgba::WHITE)?;
//! // ... iterate display list and draw to canvas
//! ```

pub mod canvas;
pub mod display_list;
pub mod display_list_builder;
pub mod optimize;
pub mod painter;
pub mod rasterize;
pub mod stacking;
pub mod text_rasterize;

// Re-export canvas (W5.T06)
pub use canvas::Canvas;

// Re-export display list types (W5.T01)
// Note: BorderRadii and BlendMode are canonical types used by both display_list and canvas
pub use display_list::{
    BlendMode, BlendModeItem, BorderRadii, BoxShadowItem, ClipItem, DisplayItem, DisplayList, FillRectItem,
    FillRoundedRectItem, FontId, GlyphInstance, GradientStop, ImageData, ImageItem, LinearGradientItem, OpacityItem,
    RadialGradientItem, StackingContextItem, StrokeRectItem, StrokeRoundedRectItem, TextItem, Transform2D,
    TransformItem,
};

// Re-export display list builder (W5.T02)
pub use display_list_builder::DisplayListBuilder;

// Re-export optimizer types (W5.T05)
pub use optimize::{optimize, optimize_with_stats, DisplayListOptimizer, OptimizationConfig, OptimizationStats};

// Re-export painter
pub use painter::{paint_tree, Painter};

// Re-export stacking context types (W5.T03/T04)
pub use stacking::{
    build_stacking_tree, build_stacking_tree_with_styles, creates_stacking_context, get_stacking_context_reason,
    PaintOrderIterator, StackingContext, StackingContextReason, StyledFragmentRef,
};

// Re-export rasterize types (W5.T07)
// Note: BorderRadii is already exported from display_list
pub use rasterize::{
    draw_line, fill_circle, fill_ellipse, fill_rect, fill_rect_from_rect, fill_rounded_rect, render_borders,
    render_box_shadow, stroke_rect, stroke_rounded_rect, BorderColors, BorderWidths, BoxShadow,
};

// Re-export text rasterize types (W5.T08)
pub use text_rasterize::{GlyphCache, TextRasterizer};
