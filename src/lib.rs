// ============================================================================
// Clippy Configuration
// ============================================================================

// Clippy is run in CI; keep lints as warnings to avoid hard failures during development.
#![warn(clippy::all)]
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
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::type_complexity)]
#![allow(clippy::items_after_test_module)]
#![allow(clippy::large_stack_frames)]
#![allow(clippy::large_stack_arrays)]
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

pub mod animation;
pub mod api;
pub mod compat;
pub mod error;
pub mod geometry;

// ============================================================================
// Pipeline Modules
// ============================================================================

pub mod accessibility;
pub mod layout;
pub mod math;
pub mod paint;
pub mod scroll;
pub mod style;
pub mod text;
pub mod tree;
pub mod render_control;

// ============================================================================
// Supporting Modules
// ============================================================================

pub mod css;
pub mod debug;
pub mod dom;
pub mod html;
pub mod image_compare;
pub mod image_loader;
pub mod image_output;
pub mod resource;
pub mod svg;

// ============================================================================
// Public API
// ============================================================================

// Main entry point
pub use api::FastRender;
pub use api::FastRenderBuilder;
pub use api::FastRenderConfig;
pub use api::FastRenderPool;
pub use api::FastRenderPoolConfig;
pub use api::FontCacheConfig;
pub use api::ImageCacheConfig;
pub use api::LayoutArtifacts;
pub use api::Pixmap;
pub use api::PreparedDocument;
pub use api::RenderArtifactRequest;
pub use api::RenderArtifacts;
pub use api::RenderDiagnostics;
pub use api::RenderOptions;
pub use api::RenderReport;
pub use api::RenderResult;
pub use api::ResourceFetchError;
pub use api::ResourceKind;
pub use render_control::CancelCallback;
pub use compat::CompatProfile;
// CSS
pub use css::parser::parse_stylesheet_with_errors;
pub use css::types::CssParseError;
pub use css::types::CssParseResult;
pub use css::types::PropertyValue;
pub use css::types::Transform;
// Debug tools
pub use debug::tree_printer::{
  ColorMode, DiffMode, DotExporter, EnhancedTreePrinter, PrintConfig, TreeDiff,
};
// Error handling
pub use error::{Error, Result};
// Geometry primitives
pub use geometry::{EdgeOffsets, Point, Rect, Size};
// Image output
pub use image_output::OutputFormat;
// Layout
pub use layout::absolute_positioning::{
  AbsoluteLayout, AbsoluteLayoutInput, AbsoluteLayoutResult, ResolvedMargins,
};
pub use layout::constraints::AvailableSpace;
pub use layout::constraints::LayoutConstraints;
pub use layout::contexts::factory::FormattingContextFactory;
pub use layout::contexts::positioned::ContainingBlock;
pub use layout::contexts::positioned::PositionedLayout;
pub use layout::contexts::positioned::StickyConstraints;
pub use layout::engine::LayoutConfig;
pub use layout::engine::LayoutEngine;
pub use layout::float_context::FloatContext;
pub use layout::float_context::FloatSide;
pub use layout::formatting_context::FormattingContext;
pub use layout::formatting_context::IntrinsicSizingMode;
pub use layout::fragmentation::FragmentationOptions;
// Paint
pub use paint::canvas::Canvas;
pub use paint::display_list::BlendMode;
pub use paint::display_list::BlendModeItem;
pub use paint::display_list::BorderRadii;
pub use paint::display_list::BoxShadowItem;
pub use paint::display_list::ClipItem;
pub use paint::display_list::ConicGradientItem;
pub use paint::display_list::DisplayItem;
pub use paint::display_list::DisplayList;
pub use paint::display_list::FillRectItem;
pub use paint::display_list::FillRoundedRectItem;
pub use paint::display_list::GlyphInstance;
pub use paint::display_list::GradientSpread;
pub use paint::display_list::GradientStop;
pub use paint::display_list::ImageData;
pub use paint::display_list::ImageFilterQuality;
pub use paint::display_list::ImageItem;
pub use paint::display_list::LinearGradientItem;
pub use paint::display_list::OpacityItem;
pub use paint::display_list::RadialGradientItem;
pub use paint::display_list::StackingContextItem;
pub use paint::display_list::StrokeRectItem;
pub use paint::display_list::StrokeRoundedRectItem;
pub use paint::display_list::TextItem as PaintTextItem;
pub use paint::display_list::Transform2D;
pub use paint::display_list::Transform3D;
pub use paint::display_list::TransformItem;
pub use paint::optimize::DisplayListOptimizer;
pub use paint::optimize::OptimizationConfig;
pub use paint::stacking::build_stacking_tree;
pub use paint::stacking::build_stacking_tree_with_styles;
pub use paint::stacking::creates_stacking_context;
pub use paint::stacking::get_stacking_context_reason;
pub use paint::stacking::StackingContext;
pub use paint::stacking::StackingContextReason;
pub use paint::text_rasterize::GlyphCache;
pub use paint::text_rasterize::GlyphCacheStats;
pub use paint::text_rasterize::TextRasterizer;
// Resource fetching
pub use resource::{
  AllowedSchemes, CachingFetcher, CachingFetcherConfig, FetchPolicy, FetchedResource, HttpFetcher,
  ResourceFetcher, ResourcePolicy,
};
// Style types
pub use style::color::{Color, ColorParseError, Hsla, Rgba};
pub use style::computed::PositionedStyle;
pub use style::display::Display;
pub use style::display::DisplayParseError;
// Tree structures
pub use style::display::FormattingContextType;
// Additional style types
pub use style::float::{Clear, Float};
pub use style::position::Position;
pub use style::position::PositionParseError;
pub use style::types::Overflow;
pub use style::values::Length;
pub use style::values::LengthOrAuto;
pub use style::values::LengthUnit;
pub use style::ComputedStyle;
pub use style::TopLayerKind;
// Text
pub use text::font_db::{
  FontDatabase, FontStretch, FontStyle as FontStyleDb, FontWeight as FontWeightDb, GenericFamily,
  LoadedFont,
};
pub use text::font_fallback::FallbackChain;
pub use text::font_fallback::FallbackChainBuilder;
pub use text::font_fallback::FamilyEntry;
pub use text::font_loader::FontContext;
pub use text::hyphenation::find_soft_hyphens;
pub use text::hyphenation::is_soft_hyphen;
pub use text::hyphenation::remove_soft_hyphens;
pub use text::hyphenation::Hyphenator;
pub use text::hyphenation::HyphensMode;
pub use text::hyphenation::SupportedLanguage;
pub use text::pipeline::BidiAnalysis;
pub use text::pipeline::BidiRun;
pub use text::pipeline::Direction as TextDirection;
pub use text::pipeline::GlyphPosition;
pub use text::pipeline::Script;
pub use text::pipeline::ShapedRun;
pub use text::pipeline::ShapingPipeline;
#[cfg(any(test, feature = "box_generation_demo"))]
pub use tree::box_generation_demo::{
  BoxGenerationConfig, BoxGenerationError, BoxGenerator, DOMNode,
};
pub use tree::box_tree::BoxNode;
pub use tree::box_tree::BoxTree;
pub use tree::box_tree::BoxType;
pub use tree::debug::DebugInfo;
pub use tree::fragment_tree::FragmentContent;
pub use tree::fragment_tree::FragmentNode;
pub use tree::fragment_tree::FragmentTree;
pub use tree::fragment_tree::FragmentainerPath;
pub use tree::pseudo_elements::count_pseudo_boxes;
pub use tree::pseudo_elements::find_pseudo_boxes;
pub use tree::pseudo_elements::get_pseudo_type;
pub use tree::pseudo_elements::is_pseudo_box;
pub use tree::pseudo_elements::PseudoContent;
pub use tree::pseudo_elements::PseudoContentItem;
pub use tree::pseudo_elements::PseudoElementConfig;
pub use tree::pseudo_elements::PseudoElementGenerator;
pub use tree::pseudo_elements::PseudoElementStyle;
pub use tree::pseudo_elements::PseudoElementType;
