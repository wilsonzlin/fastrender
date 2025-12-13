// ============================================================================
// Clippy Configuration
// ============================================================================

#![deny(clippy::all)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
// Justified allows for a rendering engine
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::similar_names)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::struct_excessive_bools)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_lossless)]
#![allow(clippy::float_arithmetic)]
#![allow(clippy::float_cmp)]
#![allow(clippy::suboptimal_flops)]
#![allow(clippy::imprecise_flops)]
#![allow(clippy::many_single_char_names)]
#![allow(clippy::too_many_arguments)]
// Style preferences
#![allow(clippy::must_use_candidate)]
#![allow(clippy::missing_const_for_fn)]
#![allow(clippy::redundant_field_names)]
#![allow(clippy::redundant_closure_for_method_calls)]
#![allow(clippy::redundant_closure)]
#![allow(clippy::redundant_else)]
#![allow(clippy::needless_return)]
#![allow(clippy::needless_continue)]
#![allow(clippy::needless_pass_by_value)]
#![allow(clippy::needless_borrow)]
#![allow(clippy::needless_lifetimes)]
#![allow(clippy::needless_late_init)]
#![allow(clippy::let_and_return)]
#![allow(clippy::manual_let_else)]
#![allow(clippy::manual_strip)]
#![allow(clippy::manual_range_contains)]
#![allow(clippy::manual_unwrap_or_default)]
#![allow(clippy::option_if_let_else)]
#![allow(clippy::match_same_arms)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::match_bool)]
#![allow(clippy::single_match_else)]
#![allow(clippy::if_same_then_else)]
#![allow(clippy::if_not_else)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::branches_sharing_code)]
#![allow(clippy::bool_comparison)]
#![allow(clippy::equatable_if_let)]
#![allow(clippy::unnested_or_patterns)]
#![allow(clippy::use_self)]
#![allow(clippy::clone_on_copy)]
#![allow(clippy::implicit_clone)]
#![allow(clippy::redundant_clone)]
#![allow(clippy::items_after_statements)]
#![allow(clippy::unused_self)]
#![allow(clippy::used_underscore_binding)]
#![allow(clippy::no_effect_underscore_binding)]
#![allow(clippy::trivially_copy_pass_by_ref)]
#![allow(clippy::return_self_not_must_use)]
#![allow(clippy::new_without_default)]
#![allow(clippy::derive_partial_eq_without_eq)]
#![allow(clippy::derivable_impls)]
#![allow(clippy::field_reassign_with_default)]
#![allow(clippy::default_trait_access)]
#![allow(clippy::unnecessary_wraps)]
#![allow(clippy::unnecessary_box_returns)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::unreadable_literal)]
#![allow(clippy::single_char_pattern)]
#![allow(clippy::assigning_clones)]
#![allow(clippy::unused_peekable)]
#![allow(clippy::while_immutable_condition)]
#![allow(clippy::while_let_on_iterator)]
#![allow(clippy::while_float)]
#![allow(clippy::explicit_counter_loop)]
#![allow(clippy::explicit_iter_loop)]
#![allow(clippy::explicit_write)]
#![allow(clippy::wildcard_imports)]
#![allow(clippy::overly_complex_bool_expr)]
#![allow(clippy::len_zero)]
#![allow(clippy::doc_markdown)]
#![allow(clippy::map_unwrap_or)]
#![allow(clippy::unwrap_or_default)]
#![allow(clippy::ref_option_ref)]
#![allow(clippy::ref_option)]
#![allow(clippy::stable_sort_primitive)]
#![allow(clippy::io_other_error)]
#![allow(clippy::elidable_lifetime_names)]
// Allow missing docs - documentation can be added incrementally
#![allow(missing_docs)]
// Performance - strict
#![deny(clippy::needless_collect)]
// Correctness - strict
#![deny(clippy::absurd_extreme_comparisons)]
#![deny(clippy::await_holding_lock)]
#![deny(clippy::inherent_to_string)]
// Style - warnings
#![warn(clippy::explicit_into_iter_loop)]
#![warn(clippy::inconsistent_struct_constructor)]

//! FastRender: High-performance HTML/CSS to image renderer
//!
//! FastRender converts HTML and CSS into images. It implements CSS 2.1
//! with select CSS3 features (flexbox, grid, border-radius, box-shadow).
//!
//! # Quick Start
//!
//! ```rust,ignore
//! use fastrender::FastRender;
//!
//! let html = r#"
//!     <!DOCTYPE html>
//!     <html>
//!         <head>
//!             <style>
//!                 body { background: #f0f0f0; padding: 20px; }
//!                 h1 { color: navy; }
//!             </style>
//!         </head>
//!         <body>
//!             <h1>Hello, FastRender!</h1>
//!         </body>
//!     </html>
//! "#;
//!
//! let mut renderer = FastRender::new()?;
//! let pixmap = renderer.render_html(html, 800, 600)?;
//! pixmap.save_png("output.png")?;
//! ```
//!
//! # Architecture
//!
//! FastRender uses a pipeline architecture:
//!
//! ```text
//! HTML/CSS → Parse → Style → Box Tree → Layout → Fragment Tree → Paint → Image
//! ```
//!
//! ## Core Modules
//!
//! - [`api`] - Main entry point (`FastRender`)
//! - [`tree`] - DOM, box tree, and fragment tree structures
//! - [`style`] - CSS parsing, cascade, and computed styles
//! - [`layout`] - Layout algorithms (block, inline, flex, grid, table)
//! - [`text`] - Text shaping, fonts, line breaking
//! - [`paint`] - Painting and rasterization
//! - [`geometry`] - Core geometric primitives
//! - [`error`] - Error types

// ============================================================================
// Core Modules
// ============================================================================

pub mod api;
pub mod error;
pub mod geometry;

// ============================================================================
// Pipeline Modules
// ============================================================================

pub mod layout;
pub mod paint;
pub mod style;
pub mod text;
pub mod tree;

// ============================================================================
// Supporting Modules
// ============================================================================

pub mod css;
pub mod debug;
pub mod dom;
pub mod image_loader;
pub mod image_output;
pub mod resource;

// ============================================================================
// Public API
// ============================================================================

// Main entry point
pub use api::{FastRender, FastRenderBuilder, FastRenderConfig, Pixmap};

// Error handling
pub use error::{Error, Result};

// Geometry primitives
pub use geometry::{EdgeOffsets, Point, Rect, Size};

// Tree structures
pub use style::display::FormattingContextType;
pub use tree::box_generation::BoxGenerator;
pub use tree::box_tree::{BoxNode, BoxTree, BoxType};
pub use tree::debug::DebugInfo;
pub use tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
pub use tree::pseudo_elements::{
    count_pseudo_boxes, find_pseudo_boxes, get_pseudo_type, is_pseudo_box, PseudoContent, PseudoContentItem,
    PseudoElementConfig, PseudoElementGenerator, PseudoElementStyle, PseudoElementType,
};

// Style types
pub use style::color::{Color, ColorParseError, Hsla, Rgba};
pub use style::computed::PositionedStyle;
pub use style::display::{Display, DisplayParseError};
pub use style::position::{Position, PositionParseError};
pub use style::types::Overflow;
pub use style::values::{Length, LengthOrAuto, LengthUnit};
pub use style::ComputedStyle;

// Layout
pub use layout::absolute_positioning::{AbsoluteLayout, AbsoluteLayoutInput, AbsoluteLayoutResult, ResolvedMargins};
pub use layout::constraints::{AvailableSpace, LayoutConstraints};
pub use layout::contexts::factory::FormattingContextFactory;
pub use layout::contexts::positioned::{ContainingBlock, PositionedLayout, StickyConstraints};
pub use layout::engine::{LayoutConfig, LayoutEngine};
pub use layout::float_context::{FloatContext, FloatSide};
pub use layout::formatting_context::{FormattingContext, IntrinsicSizingMode};

// Additional style types
pub use style::float::{Clear, Float};

// Paint
pub use paint::canvas::Canvas;
pub use paint::display_list::{
    BlendMode, BlendModeItem, BorderRadii, BoxShadowItem, ClipItem, ConicGradientItem, DisplayItem, DisplayList,
    FillRectItem, FillRoundedRectItem, GlyphInstance, GradientSpread, GradientStop, ImageData, ImageFilterQuality,
    ImageItem, LinearGradientItem, OpacityItem, RadialGradientItem, StackingContextItem, StrokeRectItem,
    StrokeRoundedRectItem, TextItem as PaintTextItem, Transform2D, TransformItem,
};
pub use paint::optimize::{DisplayListOptimizer, OptimizationConfig};
pub use paint::stacking::{
    build_stacking_tree, build_stacking_tree_with_styles, creates_stacking_context, get_stacking_context_reason,
    StackingContext, StackingContextReason,
};
pub use paint::text_rasterize::{GlyphCache, TextRasterizer};

// Text
pub use text::font_db::{
    FontDatabase, FontStretch, FontStyle as FontStyleDb, FontWeight as FontWeightDb, GenericFamily, LoadedFont,
};
pub use text::font_fallback::{FallbackChain, FallbackChainBuilder, FamilyEntry};
pub use text::font_loader::FontContext;
pub use text::hyphenation::{
    find_soft_hyphens, is_soft_hyphen, remove_soft_hyphens, Hyphenator, HyphensMode, SupportedLanguage,
};
pub use text::shaper::{Script, TextDirection, TextShaper};

// CSS
pub use css::parser::parse_stylesheet_with_errors;
pub use css::types::{CssParseError, CssParseResult, PropertyValue, Transform};

// Image output
pub use image_output::OutputFormat;

// Resource fetching
pub use resource::{FetchedResource, HttpFetcher, ResourceFetcher};

// Debug tools
pub use debug::tree_printer::{ColorMode, DiffMode, DotExporter, EnhancedTreePrinter, PrintConfig, TreeDiff};
