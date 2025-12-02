# Phase 2: Module Surgery

**Goal**: Split monster files into focused, single-responsibility modules.

**Prerequisites**: Phase 1 (Type System Unification)

**Estimated Scope**: ~2500 lines of code reorganization

---

## Overview

Two major restructurings:
1. Split `style/mod.rs` (2186 lines) into 6+ focused modules
2. Promote `css.rs` (865 lines) to a module directory

---

## Task 2.1: Split style/mod.rs

### Current Structure (2186 lines)

| Lines | Content | Target |
|-------|---------|--------|
| 1-66 | Imports, module declarations, re-exports | `mod.rs` (minimal) |
| 67-181 | ComputedStyles struct | → `computed.rs` (Phase 1 handled) |
| 182-341 | Style enums | → `types.rs` (NEW) |
| 342-442 | ComputedStyles::default() | → `computed.rs` (Phase 1 handled) |
| 444-504 | `apply_styles()` | → `cascade.rs` (NEW) |
| 506-730 | `apply_styles_internal_with_ancestors()` | → `cascade.rs` |
| 732-933 | `get_default_styles_for_element()` | → `defaults.rs` (NEW) |
| 935-1010 | Helper functions | → `cascade.rs` |
| 1012-1023 | `resolve_var()` wrapper | → `var_resolution.rs` (exists) |
| 1024-1079 | `find_matching_rules_with_ancestors()` | → `cascade.rs` |
| 1081-1828 | `apply_declaration()` | → `properties.rs` (NEW) |
| 1830-2030 | Grid parsing helpers | → `grid.rs` (NEW) |
| 2032-2186 | Box value helpers | → `properties.rs` |

### New Module Structure

```
src/style/
├── mod.rs           # ~60 lines: just pub mod + pub use
├── types.rs         # ~200 lines: all style enums (NEW)
├── computed.rs      # ~350 lines: ComputedStyle struct (EXPANDED)
├── cascade.rs       # ~500 lines: apply_styles, matching (NEW)
├── properties.rs    # ~850 lines: apply_declaration (NEW)
├── defaults.rs      # ~250 lines: element defaults (NEW)
├── grid.rs          # ~250 lines: grid track parsing (NEW)
├── color.rs         # existing
├── display.rs       # existing
├── position.rs      # existing
├── values.rs        # existing
├── content.rs       # existing
├── counters.rs      # existing
├── variables.rs     # existing
├── var_resolution.rs # existing
├── media.rs         # existing
└── float.rs         # existing
```

### Step-by-Step Process

#### Step 1: Create types.rs

**Move these enums from style/mod.rs (lines 182-341)**:
- `Overflow`
- `BorderStyle`
- `FlexDirection`
- `FlexWrap`
- `JustifyContent`
- `AlignItems`
- `AlignContent`
- `FlexBasis`
- `GridTrack`
- `FontWeight`
- `FontStyle`
- `LineHeight`
- `TextAlign`
- `TextDecoration`
- `TextTransform`
- `WhiteSpace`
- `BackgroundImage`
- `BackgroundSize`
- `BackgroundPosition`
- `BackgroundRepeat`

**Template for types.rs**:
```rust
//! Style type definitions
//!
//! This module contains all the enum types used in computed styles.

use crate::css::ColorStop;
use crate::style::values::Length;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Overflow {
    Visible,
    Hidden,
    Scroll,
    Auto,
}

// ... rest of enums
```

**After creating**:
1. Add `pub mod types;` to mod.rs
2. Update imports in mod.rs and other files
3. Run `cargo build`

#### Step 2: Create cascade.rs

**Move from style/mod.rs**:
- `apply_styles()` function (lines 444-457)
- `apply_styles_internal()` function (lines 459-504)
- `apply_styles_internal_with_ancestors()` function (lines 506-730) - **Remove HACKS (Phase 3)**
- `inherit_styles()` function (lines 988-1010)
- `find_matching_rules_with_ancestors()` function (lines 1024-1069)
- `build_element_ref_chain()` function (lines 1072-1079)

**Template for cascade.rs**:
```rust
//! CSS Cascade and Style Application
//!
//! This module handles applying CSS rules to DOM nodes to produce
//! computed styles. It implements the CSS cascade algorithm.

use crate::css::{Declaration, StyleSheet};
use crate::dom::{DomNode, ElementRef};
use crate::style::{ComputedStyle, StyledNode};
use crate::style::properties::apply_declaration;
use crate::style::defaults::get_default_styles_for_element;
use selectors::context::{QuirksMode, SelectorCaches};
use selectors::matching::{matches_selector, MatchingContext, MatchingMode};

// User-agent stylesheet
const USER_AGENT_STYLESHEET: &str = include_str!("../user_agent.css");

/// Apply styles to a DOM tree
pub fn apply_styles(dom: &DomNode, stylesheet: &StyleSheet) -> StyledNode {
    // ... implementation
}

// ... rest of functions
```

**Dependencies to import**:
- `crate::css` - StyleSheet, Declaration
- `crate::dom` - DomNode, ElementRef
- `crate::style::properties` - apply_declaration (after creating properties.rs)
- `crate::style::defaults` - get_default_styles_for_element (after creating defaults.rs)
- `selectors` crate

#### Step 3: Create defaults.rs

**Move from style/mod.rs**:
- `get_default_styles_for_element()` function (lines 732-933)
- `parse_dimension_attribute()` helper (lines 935-951)
- `parse_color_attribute()` helper (lines 953-986)

**Template for defaults.rs**:
```rust
//! Default styles for HTML elements
//!
//! Provides initial/default computed style values for each HTML element type.
//! These are applied before author styles in the cascade.

use crate::dom::DomNode;
use crate::style::{ComputedStyle, Display, Rgba};
use crate::style::values::Length;

/// Get default styles for an HTML element
pub fn get_default_styles_for_element(node: &DomNode) -> ComputedStyle {
    // ... implementation
}

/// Parse HTML width/height attribute
pub fn parse_dimension_attribute(dim_str: &str) -> Option<Length> {
    // ... implementation
}

/// Parse HTML bgcolor attribute
pub fn parse_color_attribute(color_str: &str) -> Option<Rgba> {
    // ... implementation
}
```

**Note**: This function has site-specific hacks (lines 769-930). Mark them with comments for Phase 3 removal, but keep functional for now.

#### Step 4: Create properties.rs

**Move from style/mod.rs**:
- `apply_declaration()` function (lines 1081-1828) - THE BIG ONE
- `extract_length()` helper (lines 1830-1837)
- `extract_margin_values()` helper (lines 1839-1853)
- `extract_box_values()` helper (lines 1855-1868)
- `apply_margin_values()` helper (lines 2110-2144)
- `apply_box_values()` helper (lines 2146-2174)
- `parse_border_style()` helper (lines 2176-2185)

**Template for properties.rs**:
```rust
//! CSS Property Application
//!
//! This module handles applying individual CSS property declarations
//! to a computed style struct.

use crate::css::{Declaration, PropertyValue};
use crate::style::{ComputedStyle, Rgba};
use crate::style::types::*;
use crate::style::values::Length;
use crate::style::var_resolution::resolve_var;
use std::collections::HashMap;

/// Apply a single CSS declaration to a computed style
pub fn apply_declaration(
    styles: &mut ComputedStyle,
    decl: &Declaration,
    parent_font_size: f32,
    root_font_size: f32,
) {
    // Handle CSS Custom Properties
    if decl.property.starts_with("--") {
        // ... custom property handling
        return;
    }

    let resolved_value = resolve_var(&decl.value, &styles.custom_properties);

    match decl.property.as_str() {
        "display" => { /* ... */ }
        "position" => { /* ... */ }
        // ... all property handlers
        _ => {}
    }
}

// Helper functions
fn extract_length(value: &PropertyValue) -> Option<Length> { /* ... */ }
// ... rest of helpers
```

**This is the largest function to move** (~750 lines). Consider:
- Keep it as one function initially
- Could later split into sub-modules (box_model.rs, typography.rs, etc.)

#### Step 5: Create grid.rs

**Move from style/mod.rs**:
- `parse_grid_tracks_with_names()` function (lines 1870-1941)
- `parse_grid_line()` function (lines 1944-1967)
- `finalize_grid_placement()` function (lines 1970-1984)
- `parse_grid_line_placement()` function (lines 1987-2030)
- `process_track_token()` function (lines 2033-2061)
- `parse_single_grid_track()` function (lines 2064-2108)

**Template for grid.rs**:
```rust
//! CSS Grid Track Parsing
//!
//! This module handles parsing CSS Grid layout values including
//! track definitions, named lines, and grid placement.

use crate::css::PropertyValue;
use crate::style::types::GridTrack;
use crate::style::values::Length;
use std::collections::HashMap;

/// Parse grid-template-columns/rows into track list with named lines
pub fn parse_grid_tracks_with_names(
    tracks_str: &str
) -> (Vec<GridTrack>, HashMap<String, Vec<usize>>) {
    // ... implementation
}

// ... rest of functions
```

#### Step 6: Update mod.rs

**After all modules created**:

```rust
//! Style system for FastRender
//!
//! This module handles CSS style computation, including the cascade,
//! inheritance, and property application.

// Sub-modules
pub mod cascade;
pub mod color;
pub mod computed;
pub mod content;
pub mod counters;
pub mod defaults;
pub mod display;
pub mod float;
pub mod grid;
pub mod media;
pub mod position;
pub mod properties;
pub mod types;
pub mod values;
pub mod var_resolution;
pub mod variables;

// Re-exports
pub use cascade::{apply_styles, StyledNode};
pub use color::{Color, ColorParseError, Hsla, Rgba};
pub use computed::ComputedStyle;
pub use content::{parse_content, ContentContext, ContentGenerator, ContentItem, ContentValue, CounterStyle};
pub use counters::{CounterManager, CounterProperties, CounterSet, CounterSetItem};
pub use defaults::get_default_styles_for_element;
pub use display::{Display, DisplayParseError, FormattingContextType, InnerDisplay, OuterDisplay};
pub use float::{Clear, ClearParseError, Float, FloatParseError};
pub use position::{Position, PositionParseError};
pub use properties::apply_declaration;
pub use types::*;  // All style enums
pub use values::{Length, LengthOrAuto, LengthUnit};
pub use variables::CssVariables;
```

**Target: ~60 lines** (just module declarations and re-exports)

#### Step 7: Verify and Test

After each step:
```bash
cargo build
cargo test
```

Final verification:
```bash
wc -l src/style/mod.rs  # Should be ~60 lines
cargo test --lib
cargo clippy
```

---

## Task 2.2: Promote css.rs to Directory

### Current Structure (865 lines)

| Lines | Content | Target |
|-------|---------|--------|
| 1-60 | CssString wrapper, imports | `types.rs` |
| 62-150 | SelectorImpl, PseudoClass, PseudoElement | `selectors.rs` |
| 151-273 | StyleSheet, StyleRule, Declaration, PropertyValue | `types.rs` |
| 274-370 | PseudoClassParser | `selectors.rs` |
| 372-424 | `unwrap_is_pseudo()` - HACK | DELETE (Phase 4) |
| 426-499 | `preprocess_media_queries()` - HACK | → `media.rs` (replace, Phase 4) |
| 501-614 | `parse_stylesheet()`, `parse_rule()` | `parser.rs` |
| 616-691 | `parse_declaration_list()` | `parser.rs` |
| 693-752 | `parse_property_value()` | `properties.rs` |
| 754-821 | `parse_length()` | `properties.rs` |
| 823-828 | `parse_declarations()` | `parser.rs` |
| 830-864 | `extract_css()` | `parser.rs` |

### New Module Structure

```
src/css/
├── mod.rs           # Re-exports
├── types.rs         # CssString, StyleSheet, StyleRule, Declaration, PropertyValue
├── selectors.rs     # FastRenderSelectorImpl, PseudoClass, PseudoElement
├── parser.rs        # parse_stylesheet, parse_rule, parse_declarations, extract_css
└── properties.rs    # parse_property_value, parse_length
```

### Steps

#### Step 1: Create css/ directory
```bash
mkdir src/css
```

#### Step 2: Create css/types.rs

**Move from css.rs**:
- `CssString` wrapper and impls (lines 12-59)
- `StyleSheet` struct (line 152-155)
- `StyleRule` struct (lines 158-162)
- `Declaration` struct (lines 165-170)
- `PropertyValue` enum (lines 173-189)
- `BoxShadow` struct (lines 236-244)
- `TextShadow` struct (lines 246-253)
- `ColorStop` struct (lines 255-258)
- `Transform` enum (lines 260-272)

**Note**: `Color` struct should NOT be moved - it's being deleted in Phase 1.

**Template**:
```rust
//! CSS type definitions
//!
//! Core types for representing CSS stylesheets, rules, and values.

use crate::style::{Length, Rgba};
use cssparser::ToCss;
use selectors::parser::SelectorList;
use std::fmt;

// CssString wrapper for selectors crate compatibility
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct CssString(pub String);
// ... impls

// Stylesheet structures
pub struct StyleSheet { /* ... */ }
pub struct StyleRule { /* ... */ }
pub struct Declaration { /* ... */ }
pub enum PropertyValue { /* ... */ }

// Shadow and gradient types
pub struct BoxShadow { /* ... */ }
pub struct TextShadow { /* ... */ }
pub struct ColorStop { /* ... */ }
pub enum Transform { /* ... */ }
```

#### Step 3: Create css/selectors.rs

**Move from css.rs**:
- `FastRenderSelectorImpl` (lines 62-77)
- `PseudoClass` enum (lines 79-93)
- `PseudoElement` enum (lines 128-133)
- Their trait implementations
- `PseudoClassParser` (lines 274-349)
- `parse_nth()` helper (lines 351-370)

**Template**:
```rust
//! CSS Selector support
//!
//! Implements selector parsing and matching using the selectors crate.

use super::types::CssString;
use cssparser::{ParseError, Parser, ToCss, Token};
use selectors::parser::{SelectorImpl, SelectorParseErrorKind};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FastRenderSelectorImpl;

impl SelectorImpl for FastRenderSelectorImpl { /* ... */ }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PseudoClass { /* ... */ }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PseudoElement { /* ... */ }

pub(crate) struct PseudoClassParser;
impl<'i> selectors::parser::Parser<'i> for PseudoClassParser { /* ... */ }
```

#### Step 4: Create css/parser.rs

**Move from css.rs**:
- `parse_stylesheet()` function (lines 501-541)
- `parse_rule()` function (lines 544-614)
- `parse_declaration_list()` function (lines 617-691)
- `parse_declarations()` function (lines 823-828)
- `extract_css()` function (lines 830-864)

**DO NOT MOVE (Phase 4)**:
- `unwrap_is_pseudo()` - mark for deletion
- `preprocess_media_queries()` - mark for replacement

**Template**:
```rust
//! CSS parsing
//!
//! Parses CSS stylesheets and declarations.

use super::selectors::PseudoClassParser;
use super::types::{Declaration, StyleRule, StyleSheet};
use super::properties::parse_property_value;
use crate::dom::DomNode;
use crate::error::Result;
use cssparser::{Parser, ParserInput, Token};
use selectors::parser::SelectorList;

/// Parse a CSS stylesheet from a string
pub fn parse_stylesheet(css: &str) -> Result<StyleSheet> {
    // Note: unwrap_is_pseudo and preprocess_media_queries
    // are temporary hacks to be removed in Phase 4
    let preprocessed = preprocess_css(css);
    // ... rest of implementation
}

/// Extract CSS from <style> tags in a DOM tree
pub fn extract_css(dom: &DomNode) -> Result<StyleSheet> {
    // ... implementation
}
```

#### Step 5: Create css/properties.rs

**Move from css.rs**:
- `parse_property_value()` function (lines 693-752)
- `parse_length()` function (lines 754-821)

**Template**:
```rust
//! CSS property value parsing
//!
//! Parses individual CSS property values.

use super::types::PropertyValue;
use crate::style::{Length, LengthUnit, Rgba};

/// Parse a CSS property value
pub fn parse_property_value(property: &str, value_str: &str) -> Option<PropertyValue> {
    // ... implementation
}

/// Parse a CSS length value
pub fn parse_length(s: &str) -> Option<Length> {
    // ... implementation
}
```

#### Step 6: Create css/mod.rs

```rust
//! CSS parsing and types
//!
//! This module handles parsing CSS stylesheets and provides types
//! for representing CSS rules, selectors, and values.

pub mod parser;
pub mod properties;
pub mod selectors;
pub mod types;

// Re-exports for convenience
pub use parser::{extract_css, parse_declarations, parse_stylesheet};
pub use properties::{parse_length, parse_property_value};
pub use selectors::{FastRenderSelectorImpl, PseudoClass, PseudoElement};
pub use types::{
    BoxShadow, ColorStop, CssString, Declaration, PropertyValue, StyleRule, StyleSheet,
    TextShadow, Transform,
};

// Also re-export selectors crate types we use
pub use selectors::matching::matches_selector;
pub use selectors::parser::SelectorImpl;
```

#### Step 7: Delete src/css.rs

After moving everything:
```bash
rm src/css.rs
```

#### Step 8: Update imports throughout codebase

All `use crate::css::X` imports should still work because mod.rs re-exports everything.

Verify:
```bash
cargo build
```

---

## Validation

After completing Phase 2:

```bash
# Build must succeed
cargo build

# All tests must pass
cargo test

# Check file sizes
wc -l src/style/mod.rs  # Target: ~60 lines
wc -l src/css/mod.rs    # Target: ~30 lines

# Ensure no orphaned code
cargo build 2>&1 | grep -i "unused"
```

---

## Notes for Implementation

### Circular Import Prevention

The modules have these dependencies:
```
css/types.rs → style::Rgba, style::Length
css/selectors.rs → css/types.rs
css/parser.rs → css/selectors.rs, css/types.rs, css/properties.rs
css/properties.rs → css/types.rs, style::*

style/cascade.rs → css/*, style/properties.rs, style/defaults.rs
style/properties.rs → css/types.rs, style/types.rs, style/var_resolution.rs
style/defaults.rs → style/*, dom
style/grid.rs → style/types.rs, style/values.rs
```

This should work because:
- css depends on style for basic types (Rgba, Length)
- style depends on css for parsing types (PropertyValue, Declaration)
- No true cycle exists

### Commit Strategy

Make incremental commits:
1. "refactor: Create style/types.rs with style enums"
2. "refactor: Create style/cascade.rs with apply_styles"
3. "refactor: Create style/defaults.rs with element defaults"
4. "refactor: Create style/properties.rs with apply_declaration"
5. "refactor: Create style/grid.rs with grid parsing"
6. "refactor: Minimize style/mod.rs to re-exports only"
7. "refactor: Promote css.rs to css/ directory"

### Testing Between Steps

After each module creation:
1. Add the module to mod.rs
2. Update imports
3. Run `cargo build`
4. Run `cargo test`

Don't proceed to next step if current step breaks anything.

---

*Previous Phase: [01-phase1-types.md](./01-phase1-types.md)*
*Next Phase: [03-phase3-hacks.md](./03-phase3-hacks.md)*
