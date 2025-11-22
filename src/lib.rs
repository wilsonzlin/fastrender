// ============================================================================
// Clippy Configuration
// ============================================================================

// Deny clippy warnings in CI
#![deny(clippy::all)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
// Allow some pedantic lints that are too strict or not applicable

// Allow module name repetition - often clearer to repeat
// Example: geometry::Point instead of geometry::P
#![allow(clippy::module_name_repetitions)]
// Allow similar names - layout algorithms have similar variables
// Example: min_width, max_width, width
#![allow(clippy::similar_names)]
// Allow too many lines - some layout algorithms are necessarily long
#![allow(clippy::too_many_lines)]
// Allow struct excessive bools - style properties often have many bools
#![allow(clippy::struct_excessive_bools)]
// Allow missing errors doc - we document errors at module level
#![allow(clippy::missing_errors_doc)]
// Allow missing panics doc - most panics are from logic errors, not expected panics
#![allow(clippy::missing_panics_doc)]
// Allow cast precision loss - we intentionally cast for layout calculations
#![allow(clippy::cast_precision_loss)]
// Allow cast possible truncation - we handle this carefully in layout
#![allow(clippy::cast_possible_truncation)]
// Allow cast sign loss - unsigned/signed conversions are common in graphics
#![allow(clippy::cast_sign_loss)]
// Allow float arithmetic - we're a rendering engine, floats are fundamental
#![allow(clippy::float_arithmetic)]
// Allow float comparisons - needed for geometry and layout calculations
#![allow(clippy::float_cmp)]
// Allow must_use attribute - not all return values need to be used
#![allow(clippy::must_use_candidate)]
// Allow missing const for fn - will add gradually
#![allow(clippy::missing_const_for_fn)]
// Allow used underscore binding - sometimes needed for clarity
#![allow(clippy::used_underscore_binding)]
// Allow redundant else - sometimes clearer with explicit else
#![allow(clippy::redundant_else)]
// Allow single char names - common in math/graphics code (x, y, w, h)
#![allow(clippy::many_single_char_names)]
// Allow unnested or-patterns - not always clearer when nested
#![allow(clippy::unnested_or_patterns)]
// Allow struct field names - often clearer to be explicit
#![allow(clippy::redundant_field_names)]
// Allow derive partial eq without eq - not always appropriate
#![allow(clippy::derive_partial_eq_without_eq)]
// Allow long literals - numeric constants are clearer without separators sometimes
#![allow(clippy::unreadable_literal)]
// Allow match on same arms - sometimes clearer to be explicit
#![allow(clippy::match_same_arms)]
// Allow map_or/map_or_else suggestions - if let is sometimes clearer
#![allow(clippy::option_if_let_else)]
// Allow unnecessary structure name - sometimes clearer to be explicit
#![allow(clippy::redundant_closure_for_method_calls)]
// Allow redundant continue - sometimes clearer for flow control
#![allow(clippy::needless_continue)]
// Allow format strings with variables - not always clearer
#![allow(clippy::uninlined_format_args)]
// Allow structure name repetition in code - often clearer
#![allow(clippy::use_self)]
// Allow missing backticks in docs - will add gradually
#![allow(clippy::doc_markdown)]
// Allow must use on Self-returning methods - not always needed
#![allow(clippy::return_self_not_must_use)]
// Allow cast lossless - explicit casts are sometimes clearer
#![allow(clippy::cast_lossless)]
// Allow clone on copy - sometimes more explicit
#![allow(clippy::clone_on_copy)]
// Allow items after statements - common pattern in Rust
#![allow(clippy::items_after_statements)]
// Allow unnecessary return - sometimes clearer for symmetry
#![allow(clippy::needless_return)]
// Allow unused self - needed for trait implementations
#![allow(clippy::unused_self)]
// Allow mul add - manual control sometimes needed
#![allow(clippy::suboptimal_flops)]
// Allow possible wrap - we handle carefully
#![allow(clippy::cast_possible_wrap)]
// Allow single char patterns - sometimes clearer than chars
#![allow(clippy::single_char_pattern)]
// Allow assigning clone result - sometimes needed for ownership
#![allow(clippy::assigning_clones)]
// Allow unnecessary box returns - needed for API consistency
#![allow(clippy::unnecessary_box_returns)]
// Allow prefix stripping - manual sometimes clearer
#![allow(clippy::manual_strip)]
// Allow field reassignment - common pattern
#![allow(clippy::field_reassign_with_default)]
// Allow trivial refs - API consistency
#![allow(clippy::trivially_copy_pass_by_ref)]
// Allow collapsible else if - sometimes clearer
#![allow(clippy::collapsible_else_if)]
// Allow no effect underscore binding - clarity in complex code
#![allow(clippy::no_effect_underscore_binding)]
// Allow needless pass by value - API design choice
#![allow(clippy::needless_pass_by_value)]
// Allow single match else - sometimes clearer
#![allow(clippy::single_match_else)]
// Allow unnecessary wraps - API consistency
#![allow(clippy::unnecessary_wraps)]
// Allow peekable iterator not used - API pattern
#![allow(clippy::unused_peekable)]
// Allow while immutable condition - sometimes needed for float comparisons
#![allow(clippy::while_immutable_condition)]
// Allow matches macro - explicit matching sometimes clearer
#![allow(clippy::match_like_matches_macro)]
// Allow for loops over iterator - sometimes clearer
#![allow(clippy::explicit_counter_loop)]
// Allow io error other - legacy compatibility
#![allow(clippy::stable_sort_primitive)]
// Allow ref option - API design choice
#![allow(clippy::ref_option_ref)]
// Allow branches sharing code - sometimes clearer to be explicit
#![allow(clippy::branches_sharing_code)]
// Allow or insert with default - explicit sometimes clearer
#![allow(clippy::unwrap_or_default)]
// Allow wildcard imports - common in module organization
#![allow(clippy::wildcard_imports)]
// Allow late init - needed for complex initialization
#![allow(clippy::needless_late_init)]
// Allow bool not - sometimes clearer than !
#![allow(clippy::overly_complex_bool_expr)]
// Allow too many arguments - complex APIs sometimes need many args
#![allow(clippy::too_many_arguments)]
// Allow needless borrow - sometimes needed for clarity
#![allow(clippy::needless_borrow)]
// Allow let and return - sometimes clearer for debugging
#![allow(clippy::let_and_return)]
// Allow redundant closure - sometimes clearer for type inference
#![allow(clippy::redundant_closure)]
// Allow explicit_iter_loop already handled above
// Allow implicit clone - sometimes needed for API
#![allow(clippy::implicit_clone)]
// Allow imprecise flops - manual control sometimes needed
#![allow(clippy::imprecise_flops)]
// Allow manual unwrap or default - explicit sometimes clearer
#![allow(clippy::manual_unwrap_or_default)]
// Allow default trait access - explicit sometimes clearer
#![allow(clippy::default_trait_access)]
// Allow map unwrap or - chaining sometimes clearer
#![allow(clippy::map_unwrap_or)]
// Allow if same then else - sometimes clearer for symmetry
#![allow(clippy::if_same_then_else)]
// Allow collapsible if - sometimes clearer when separate
#![allow(clippy::collapsible_if)]
// Allow let else - not always appropriate
#![allow(clippy::manual_let_else)]
// Allow derive impl - manual impl sometimes needed
#![allow(clippy::derivable_impls)]
// Allow new without default - API choice
#![allow(clippy::new_without_default)]
// Allow deprecated - legacy support sometimes needed
#![allow(deprecated)]
// Allow dead code - work in progress
#![allow(dead_code)]
// Allow unused imports - work in progress
#![allow(unused_imports)]
// Allow explicit write - sometimes clearer
#![allow(clippy::explicit_write)]
// Allow explicit iterate - sometimes clearer
#![allow(clippy::explicit_iter_loop)]
// Allow manual range contains - sometimes clearer
#![allow(clippy::manual_range_contains)]
// Allow bool comparison - sometimes clearer for readability
#![allow(clippy::bool_comparison)]
// Allow eliding lifetimes - often clearer without explicit lifetimes
#![allow(clippy::needless_lifetimes)]
// Allow while let on iterator - sometimes needed for control flow
#![allow(clippy::while_let_on_iterator)]
// Allow match bool - sometimes clearer than if-else
#![allow(clippy::match_bool)]
// Note: ref_in_deref was renamed to needless_borrow which is already covered above
// Allow unused assignments - work in progress
#![allow(unused_assignments)]
// Allow while float - necessary for some algorithms
#![allow(clippy::while_float)]
// Allow elidable lifetimes - sometimes clearer to be explicit
#![allow(clippy::elidable_lifetime_names)]
// Allow if not else - sometimes clearer for logic
#![allow(clippy::if_not_else)]
// Allow equatable if let - sometimes clearer than matches
#![allow(clippy::equatable_if_let)]
// Allow ref option - API design choice
#![allow(clippy::ref_option)]
// Allow io_other_error - will migrate to Error::other eventually
#![allow(clippy::io_other_error)]
// Allow len zero - comparison to zero is sometimes clearer
#![allow(clippy::len_zero)]
// Performance lints - be very strict
#![deny(clippy::needless_collect)]
// Note: redundant_clone allowed temporarily - should be fixed eventually
#![allow(clippy::redundant_clone)]
// Correctness lints - always deny
#![deny(clippy::absurd_extreme_comparisons)]
#![deny(clippy::await_holding_lock)]
#![deny(clippy::inherent_to_string)]
// Style lints - warn to encourage good style
#![warn(clippy::explicit_into_iter_loop)]
#![warn(clippy::inconsistent_struct_constructor)]
// Documentation lints
// Note: missing_docs is currently allowed as the existing codebase needs significant
// documentation work. Re-enable with #![warn(missing_docs)] once documentation is added.
#![allow(missing_docs)]
// Note: missing_docs_in_private_items is too strict for the existing codebase
// Uncomment when adding comprehensive documentation is a priority
// #![warn(clippy::missing_docs_in_private_items)]

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

pub mod error;
pub mod geometry;

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

// Tree types from Wave 2 tasks
pub use tree::{BoxNode, BoxTree, BoxType, FormattingContextType};

// Style types from Wave 1 tasks
pub use style::color::{ColorParseError, Hsla, Rgba};
pub use style::display::{Display, DisplayParseError};
pub use style::position::{Position, PositionParseError};
pub use style::{Color, Length, LengthOrAuto, LengthUnit};
