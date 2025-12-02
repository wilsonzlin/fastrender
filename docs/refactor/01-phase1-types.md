# Phase 1: Type System Unification

**Goal**: Eliminate duplicate type definitions and establish single sources of truth.

**Prerequisites**: None (first phase)

**Estimated Scope**: ~500 lines of changes across 10-15 files

---

## Overview

The codebase has evolved with two parallel type systems:
1. **Legacy** (css.rs + style/mod.rs): `css::Color`, `ComputedStyles`
2. **Modern** (style/color.rs + style/computed.rs): `Rgba`, `ComputedStyle`

This phase eliminates the legacy types.

---

## Task 1.1: Eliminate css::Color

### Current State

**Definition** in `src/css.rs:192-234`:
```rust
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Color {
    pub const TRANSPARENT: Self = ...;
    pub const WHITE: Self = ...;
    pub const BLACK: Self = ...;
    pub const fn rgba(r: u8, g: u8, b: u8, a: u8) -> Self { ... }
    pub const fn rgb(r: u8, g: u8, b: u8) -> Self { ... }
    // etc.
}
```

**Equivalent** in `src/style/color.rs`:
```rust
pub struct Rgba {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}
// Similar methods
```

### Decision

Keep `style::Rgba` as canonical. Delete `css::Color`.

### Steps

1. **Update PropertyValue enum** in `src/css.rs`:
   ```rust
   // Before (line ~175)
   pub enum PropertyValue {
       Color(Color),
       ...
   }

   // After
   use crate::style::Rgba;
   pub enum PropertyValue {
       Color(Rgba),
       ...
   }
   ```

2. **Update color parsing** in `src/css.rs`:
   - `parse_property_value()` function (~line 693)
   - Change `Color::rgb(...)` to `Rgba::rgb(...)`
   - Import `Rgba` at top of file

3. **Update BoxShadow, TextShadow** in `src/css.rs`:
   - Already use `Rgba` (good!)
   - Verify consistency

4. **Remove css::Color definition**:
   - Delete lines 192-234 in css.rs
   - Delete any associated impls

5. **Update style/mod.rs**:
   - Remove `LegacyColor` alias (line 51):
     ```rust
     // DELETE THIS
     use crate::css::Color as LegacyColor;
     ```
   - Change all `LegacyColor` to `Rgba`
   - Fields affected:
     - `border_top_color`, `border_right_color`, `border_bottom_color`, `border_left_color`
     - `color`
     - `background_color`

6. **Update all color literals**:
   - Search for `LegacyColor {` and replace with `Rgba {`
   - Search for `LegacyColor::` and replace with `Rgba::`
   - Key locations in style/mod.rs:
     - Line 375-378 (border colors default)
     - Line 425-426 (color, background_color default)
     - Lines 556-663 (hack section - these will be removed later but fix for now)
     - Various other literal constructions

7. **Run tests**: `cargo test`

8. **Verify no css::Color remains**:
   ```bash
   grep -r "css::Color" src/
   grep -r "LegacyColor" src/
   ```

### Files Changed
- `src/css.rs` - Remove Color, update PropertyValue
- `src/style/mod.rs` - Remove LegacyColor, update all usages
- Any other files that import `css::Color`

### Success Criteria
- [ ] `grep -r "css::Color" src/` returns nothing
- [ ] `grep -r "LegacyColor" src/` returns nothing
- [ ] All color values are `style::Rgba` or `style::color::Rgba`
- [ ] `cargo test` passes

---

## Task 1.2: Merge ComputedStyle Types

### Current State

**Type 1**: `style::ComputedStyles` in `src/style/mod.rs:67-181`
- ~115 lines of field definitions
- Uses `LegacyColor` for colors
- Default impl in lines 342-442
- Used by: tree module, style cascade

**Type 2**: `style::computed::ComputedStyle` in `src/style/computed.rs`
- Modern design with builder pattern
- Uses `Rgba` for colors
- Used by: layout contexts, text pipeline

### Decision

Merge into single `ComputedStyle` in `src/style/computed.rs`.

### Field Comparison

Fields in `ComputedStyles` (style/mod.rs):
```
display, position, top, right, bottom, left, z_index
width, height, min_width, min_height, max_width, max_height
margin_top/right/bottom/left (as Option<Length>)
padding_top/right/bottom/left
border_*_width, border_*_color, border_*_style, border_*_radius
flex_direction, flex_wrap, justify_content, align_items, align_content
flex_grow, flex_shrink, flex_basis
grid_template_columns/rows, grid_*_names, grid_gap, grid_*_gap
grid_column_start/end, grid_row_start/end, grid_column_raw, grid_row_raw
font_family, font_size, font_weight, font_style
line_height, text_align, text_decoration, text_transform
letter_spacing, word_spacing, white_space
color, background_color, background_image, background_size/position/repeat
opacity, box_shadow, text_shadow, transform
overflow_x, overflow_y
custom_properties
```

### Steps

1. **Audit `style/computed.rs`**:
   - List all fields
   - Identify missing fields from ComputedStyles

2. **Add missing fields to ComputedStyle**:
   - Ensure all fields from ComputedStyles are present
   - Use `Rgba` for colors (not LegacyColor)
   - Match type signatures carefully

3. **Migrate Default implementation**:
   - Combine default values from both
   - Ensure consistency

4. **Update imports throughout codebase**:
   - Find all `use crate::style::ComputedStyles`
   - Change to `use crate::style::ComputedStyle`
   - Key files:
     - `src/tree/box_tree.rs`
     - `src/tree/box_generation.rs`
     - `src/style/mod.rs` (cascade functions)

5. **Remove ComputedStyles from style/mod.rs**:
   - Delete struct definition (lines 67-181)
   - Delete Default impl (lines 342-442)
   - Update any re-exports

6. **Remove compatibility alias from box_tree.rs**:
   ```rust
   // DELETE THIS (around line 38)
   pub use crate::style::ComputedStyles as ComputedStyle;
   ```

7. **Update the associated enums**:
   - Move enums from style/mod.rs to appropriate location
   - Overflow, BorderStyle, FlexDirection, etc.
   - (This overlaps with Phase 2 module surgery)

8. **Run tests**: `cargo test`

### Files Changed
- `src/style/computed.rs` - Add missing fields
- `src/style/mod.rs` - Remove ComputedStyles, update cascade
- `src/tree/box_tree.rs` - Remove compat alias, update imports
- `src/tree/box_generation.rs` - Update imports
- Layout context files - Verify still work

### Success Criteria
- [ ] Single `ComputedStyle` type in `src/style/computed.rs`
- [ ] `grep -r "ComputedStyles" src/` returns nothing (except possibly docs)
- [ ] No type aliases for backward compatibility
- [ ] `cargo test` passes

---

## Task 1.3: Clean Up Re-exports

### Current State

Multiple paths to same types:
- `style::ComputedStyles` (legacy)
- `style::computed::ComputedStyle` (newer)
- `tree::box_tree::ComputedStyle` (alias)

### Goal

Single canonical path for each type.

### Steps

1. **Define canonical paths**:
   - `style::ComputedStyle` (re-exported from computed.rs)
   - `style::Rgba`, `style::Hsla` (re-exported from color.rs)
   - `style::Display` (re-exported from display.rs)
   - etc.

2. **Update style/mod.rs exports**:
   ```rust
   // Clean, minimal re-exports
   pub use computed::ComputedStyle;
   pub use color::{Rgba, Hsla};
   pub use display::{Display, FormattingContextType, InnerDisplay, OuterDisplay};
   pub use position::Position;
   pub use values::{Length, LengthOrAuto, LengthUnit};
   // etc.
   ```

3. **Remove redundant re-exports**:
   - `tree/box_tree.rs` should not re-export style types
   - `tree/mod.rs` should use `style::ComputedStyle`

4. **Update lib.rs public API**:
   - Ensure public exports are clean
   - No duplicate paths

5. **Verify no broken imports**:
   ```bash
   cargo build
   ```

### Files Changed
- `src/style/mod.rs` - Clean up re-exports
- `src/tree/mod.rs` - Update to use style types
- `src/tree/box_tree.rs` - Remove re-exports
- `src/lib.rs` - Clean up public API

### Success Criteria
- [ ] Each type has exactly one canonical import path
- [ ] No backward-compatibility aliases
- [ ] `cargo build` succeeds
- [ ] `cargo test` passes

---

## Validation

After completing all tasks in Phase 1:

```bash
# Build must succeed
cargo build

# All tests must pass
cargo test

# No legacy types
grep -r "css::Color" src/         # Should return nothing
grep -r "LegacyColor" src/        # Should return nothing
grep -r "ComputedStyles" src/     # Should return nothing (except docs/comments)

# Clippy should not regress
cargo clippy
```

---

## Notes for Implementation

### Order Matters

1. Task 1.1 (css::Color) should be done first
2. Task 1.2 (ComputedStyle merge) depends on 1.1
3. Task 1.3 (re-exports) is cleanup after 1.1 and 1.2

### Potential Issues

- **Circular imports**: style ↔ css
  - css.rs uses style::Rgba
  - style/mod.rs uses css::PropertyValue
  - This is okay, but be careful when moving code

- **Field type mismatches**:
  - ComputedStyles uses `Option<Length>` for margins
  - Verify ComputedStyle matches

- **Method differences**:
  - Each Color type has slightly different methods
  - May need to add methods to Rgba

### Commit Strategy

Make one commit per sub-task:
1. "refactor: Replace css::Color with style::Rgba"
2. "refactor: Merge ComputedStyles into ComputedStyle"
3. "refactor: Clean up type re-exports"

---

*Next Phase: [02-phase2-modules.md](./02-phase2-modules.md)*
