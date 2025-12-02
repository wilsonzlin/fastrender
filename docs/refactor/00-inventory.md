# Current State Inventory

**Purpose**: Comprehensive inventory of what exists today - the baseline for refactoring.

---

## 1. Codebase Metrics

| Metric | Value |
|--------|-------|
| Total Lines of Rust | ~46,000 (source), ~27,000 (tests) |
| Source Files | 77 |
| Test Files | 44 |
| Tests Passing | 1512/1512 |
| Benchmark Suites | 4 |

---

## 2. Module Structure

```
src/
├── lib.rs                    # 224 lines - re-exports, 90+ clippy allows
├── api.rs                    # 803 lines - FastRender public API
├── css.rs                    # 865 lines - CSS parsing (NEEDS SPLIT)
├── dom.rs                    # 419 lines - DOM types
├── error.rs                  # ~600 lines - Error types
├── geometry.rs               # ~600 lines - Rect, Point, Size, etc.
├── image_loader.rs           # Image loading
├── image_output.rs           # Image encoding
│
├── style/                    # Style computation
│   ├── mod.rs               # 2186 lines - MONSTER FILE
│   ├── color.rs             # 1131 lines - Rgba, Hsla
│   ├── computed.rs          # ~500 lines - ComputedStyle (newer)
│   ├── content.rs           # 1616 lines - CSS content property
│   ├── counters.rs          # 1268 lines - CSS counters
│   ├── display.rs           # ~700 lines - Display enum
│   ├── float.rs             # ~200 lines - Float, Clear
│   ├── media.rs             # 2075 lines - @media types
│   ├── position.rs          # ~200 lines - Position enum
│   ├── values.rs            # ~300 lines - Length, LengthUnit
│   ├── variables.rs         # ~850 lines - CSS custom properties
│   └── var_resolution.rs    # ~550 lines - Variable substitution
│
├── tree/                     # Box and Fragment trees
│   ├── mod.rs               # 59 lines - re-exports
│   ├── box_tree.rs          # ~650 lines - BoxNode, BoxType
│   ├── box_generation.rs    # ~1500 lines - DOM → BoxTree
│   ├── fragment_tree.rs     # ~700 lines - FragmentNode
│   ├── anonymous.rs         # ~600 lines - Anonymous box creation
│   ├── pseudo_elements.rs   # ~950 lines - ::before/::after
│   ├── table_fixup.rs       # ~900 lines - Table structure normalization
│   └── debug.rs             # Debug utilities
│
├── layout/                   # Layout algorithms
│   ├── mod.rs               # 98 lines - re-exports
│   ├── engine.rs            # ~750 lines - LayoutEngine
│   ├── formatting_context.rs # ~350 lines - FormattingContext trait
│   ├── constraints.rs       # ~770 lines - LayoutConstraints
│   ├── float_context.rs     # ~1100 lines - Float placement
│   ├── absolute_positioning.rs # ~900 lines
│   ├── table.rs             # ~1500 lines - Table layout
│   ├── utils.rs             # Utilities
│   ├── contexts/            # Formatting context implementations
│   │   ├── mod.rs
│   │   ├── factory.rs       # FC factory
│   │   ├── block/           # Block layout
│   │   ├── inline/          # Inline layout
│   │   ├── flex.rs          # Flexbox (Taffy)
│   │   ├── grid.rs          # Grid (Taffy)
│   │   └── positioned.rs    # Absolute/fixed
│   └── inline/              # Inline layout helpers
│       ├── mod.rs
│       ├── text_run.rs      # ~1450 lines
│       ├── baseline.rs      # ~1100 lines
│       └── float_integration.rs
│
├── paint/                    # Painting pipeline
│   ├── mod.rs               # 138 lines - re-exports
│   ├── painter.rs           # Main painting interface
│   ├── display_list.rs      # ~1350 lines - Display list types
│   ├── display_list_builder.rs # ~500 lines
│   ├── stacking.rs          # ~1420 lines - Stacking contexts
│   ├── canvas.rs            # ~800 lines - 2D graphics
│   ├── rasterize.rs         # ~1350 lines - Shape rendering
│   ├── text_rasterize.rs    # ~600 lines - Glyph rendering
│   └── optimize.rs          # ~600 lines - Display list optimization
│
├── text/                     # Text handling
│   ├── mod.rs               # 175 lines
│   ├── pipeline.rs          # ~1140 lines - Text processing
│   ├── shaper.rs            # ~1150 lines - HarfBuzz
│   ├── font_db.rs           # ~1500 lines - Font management
│   ├── font_loader.rs       # Font loading
│   ├── font_fallback.rs     # ~500 lines
│   ├── bidi.rs              # ~1200 lines - Bidirectional text
│   ├── line_break.rs        # ~1200 lines - Line breaking
│   ├── emoji.rs             # ~1200 lines - Emoji handling
│   ├── justify.rs           # ~1100 lines - Text justification
│   ├── hyphenation.rs       # ~650 lines
│   ├── clustering.rs        # ~450 lines
│   └── script.rs            # ~600 lines - Script detection
│
├── debug/                    # Debug utilities
│   ├── mod.rs
│   └── tree_printer.rs
│
└── bin/
    └── fetch_and_render.rs  # CLI tool
```

---

## 3. Duplicate Type Inventory

### 3.1 Color Types

| Type | File:Line | Description |
|------|-----------|-------------|
| `css::Color` | `src/css.rs:192-234` | Original color type, r/g/b/a as u8 |
| `style::Rgba` | `src/style/color.rs:~50` | Newer color type, same structure |
| `style::Hsla` | `src/style/color.rs:~100` | HSL color type |

**Usage Map**:
- `css::Color` used by:
  - `PropertyValue::Color(Color)` in css.rs
  - `ComputedStyles` (as `LegacyColor` alias) in style/mod.rs

- `style::Rgba` used by:
  - `BoxShadow`, `TextShadow`, `ColorStop` in css.rs
  - `paint` module
  - `style::computed::ComputedStyle`

### 3.2 Computed Style Types

| Type | File:Line | Fields | Used By |
|------|-----------|--------|---------|
| `style::ComputedStyles` | `src/style/mod.rs:67-181` | ~60 fields | tree module, style cascade |
| `style::computed::ComputedStyle` | `src/style/computed.rs` | ~50 fields | layout contexts |

**Key Differences**:
- `ComputedStyles` uses `css::Color` (LegacyColor) for border/color/background
- `ComputedStyle` uses `style::Rgba`
- Slight differences in field naming and organization

---

## 4. Site-Specific Hack Inventory

All located in `src/style/mod.rs`:

### Hack 1: .toc Grid Placement
- **Lines**: 558-570
- **Trigger**: `node.has_class("toc")`
- **Effect**: Sets grid-column: 1/4, grid-row: 2/3
- **Reason**: @media min-width:1000px CSS rule not parsed

### Hack 2: .img-link Grid Placement
- **Lines**: 572-584
- **Trigger**: `node.has_class("img-link")` + specific hrefs
- **Effect**: Sets grid-row: 2/3
- **Reason**: @media query not parsed

### Hack 3: Image Border-Radius
- **Lines**: 586-607
- **Trigger**: `img` tag in `article` or `.banner-left`
- **Effect**: Adds border-radius 8px or 16px (circular)
- **Reason**: CSS selectors not matching properly

### Hack 4: .subscribe-btn Styling
- **Lines**: 609-663
- **Trigger**: `node.has_class("subscribe-btn")`
- **Effect**: Full button styling (padding, colors, border, radius)
- **Reason**: CSS rules not being applied

### Hack 5: .toc::before Pseudo-Element
- **Lines**: 674-723
- **Trigger**: `node.has_class("toc")`
- **Effect**: Manually creates ::before with "CONTENTS" text
- **Reason**: ::before/::after not fully implemented

### Hack 6: Element-Specific Defaults
- **Lines**: 769-930
- **Affected Classes**: `.pagetop`, `.hnname`, `.votearrow`, `.votelinks`, `.rank`
- **Effect**: Hardcoded styling for HN-specific elements
- **Reason**: User-agent stylesheet or author styles not applying

---

## 5. CSS Processing Hack Inventory

Located in `src/css.rs`:

### Hack 1: :is() Pseudo-Class Unwrapping
- **Function**: `unwrap_is_pseudo()`
- **Lines**: 372-424
- **Method**: Character-by-character string manipulation
- **Issue**: Not proper parsing, fragile, may break valid CSS

### Hack 2: @media Query Preprocessing
- **Function**: `preprocess_media_queries()`
- **Lines**: 426-499
- **Method**: String search for "min-width", exclusion of "2000"
- **Issue**:
  - Assumes viewport >= 1000px always
  - No actual evaluation of media query conditions
  - Blindly includes/excludes rules

---

## 6. Clippy Suppression Inventory

In `src/lib.rs` lines 5-100:

### Justified Suppressions (KEEP)
```rust
// Rendering engine legitimately needs these
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::float_cmp)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::many_single_char_names)]
```

### Style Preferences (KEEP if consistent)
```rust
#![allow(clippy::needless_return)]
#![allow(clippy::redundant_field_names)]
#![allow(clippy::let_and_return)]
#![allow(clippy::match_same_arms)]
```

### Temporary Development (REMOVE)
```rust
#![allow(dead_code)]           // REMOVE - clean up dead code
#![allow(unused_imports)]       // REMOVE - clean up imports
#![allow(unused_assignments)]   // REMOVE - fix these
#![allow(deprecated)]           // REMOVE - update deprecated usages
#![allow(missing_docs)]         // Consider adding docs
```

---

## 7. TODO/FIXME Inventory

| File:Line | TODO | Category |
|-----------|------|----------|
| `src/style/mod.rs:2074` | Properly parse repeat() syntax | Grid parsing |
| `src/paint/stacking.rs:538` | Check style.float when Float is added | Float support |
| `src/tree/table_fixup.rs:450` | Get colspan from cell style | Table layout |
| `src/text/pipeline.rs:699` | Set language from style when available | i18n |
| `src/text/pipeline.rs:838` | Implement CSS direction property support | RTL/LTR |
| `src/layout/contexts/positioned.rs:569` | Add transform, filter, isolation checks | Stacking |

---

## 8. Vendored Dependencies

### Taffy (vendor/taffy/)
- **Reason**: "Custom version of Taffy for grid auto-placement fixes"
- **Source**: Forked from crates.io taffy
- **Risk**: May fall behind upstream, maintenance burden
- **Cargo.toml Reference**: `taffy = { path = "vendor/taffy" }`

---

## 9. Key Files to Modify

### High Priority (Must Change)
| File | Lines | Issue | Action |
|------|-------|-------|--------|
| `src/style/mod.rs` | 2186 | Monster file, hacks, mixed concerns | Split into 6+ files |
| `src/css.rs` | 865 | Duplicate types, hack functions | Promote to directory, fix |
| `src/lib.rs` | 224 | Too many clippy allows | Reduce to <20 |
| `src/tree/box_tree.rs` | ~650 | Compat re-export | Remove |

### Medium Priority
| File | Lines | Issue | Action |
|------|-------|-------|--------|
| `src/style/computed.rs` | ~500 | Missing fields | Merge with ComputedStyles |
| `src/tree/pseudo_elements.rs` | ~950 | Incomplete | Finish ::before/::after |
| `src/style/media.rs` | 2075 | Types exist but not used | Wire up to parser |

---

## 10. Test Coverage Map

### Unit Tests (in-file)
Most modules have `#[cfg(test)] mod tests` sections with good coverage.

### Integration Tests (tests/)
| Test File | Purpose |
|-----------|---------|
| `tests/integration_test.rs` | Full pipeline tests |
| `tests/fixtures_test.rs` | Golden image tests |
| `tests/wpt_test.rs` | Web Platform Tests |
| `tests/ref_tests.rs` | Pixel comparison |
| `tests/layout/*.rs` | Layout-specific tests |
| `tests/paint_*.rs` | Paint subsystem tests |
| `tests/text_*.rs` | Text processing tests |

### Benchmarks (benches/)
| Benchmark | Purpose |
|-----------|---------|
| `comprehensive.rs` | Full pipeline benchmarks |
| `layout_benches.rs` | Layout performance |
| `paint_benches.rs` | Rendering performance |
| `render_benchmark.rs` | Overall rendering speed |

---

## 11. Dependency Graph (Key Modules)

```
api
 └── css (extract_css)
 └── dom (parse_html)
 └── style (apply_styles)
 └── tree (generate_box_tree)
 └── layout (LayoutEngine)
 └── paint (paint_tree)

style/mod.rs depends on:
 ├── css (Color, PropertyValue, Declaration, StyleSheet)
 ├── dom (DomNode, ElementRef)
 ├── style/values (Length, LengthUnit)
 ├── style/display (Display)
 ├── style/position (Position)
 ├── style/var_resolution (resolve_var)
 └── selectors crate (matching)

tree/box_tree.rs depends on:
 ├── style::ComputedStyles (as ComputedStyle alias)
 └── style::display::FormattingContextType
```

---

*This inventory should be updated as refactoring progresses.*
