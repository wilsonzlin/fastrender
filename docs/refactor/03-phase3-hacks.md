# Phase 3: Remove Site-Specific Hacks

**Goal**: Eliminate all hardcoded, site-specific styling and replace with proper CSS feature support.

**Prerequisites**:
- Phase 1 (Type System Unification)
- Phase 4 (@media query support) - for some hacks
- Phase 6 (::before/::after) - for pseudo-element hacks

**Note**: This phase may be done incrementally, interleaved with Phases 4 and 6.

---

## Overview

The codebase contains hardcoded styling for specific CSS class names and element patterns. These hacks were added to make a specific website render correctly, but they break the generality of the renderer.

**Philosophy**: All styling should come from:
1. User-agent stylesheet (`src/user_agent.css`)
2. Author stylesheets (parsed from HTML)
3. Inline styles
4. Never from class-name checks in Rust code

---

## Hack Inventory

All hacks are currently in `src/style/mod.rs` (after Phase 2, they'll be in `src/style/cascade.rs` and `src/style/defaults.rs`).

### Hack 1: .toc Grid Placement

**Location**: Lines 558-570 (in `apply_styles_internal_with_ancestors`)
**Code**:
```rust
// HACK: Add grid placement for TOC (from @media min-width:1000px)
if node.has_class("toc") {
    styles.grid_column_start = 1;
    styles.grid_column_end = 4;
    styles.grid_row_start = 2;
    styles.grid_row_end = 3;
}
```

**Problem**: The actual CSS rule is inside a `@media (min-width: 1000px)` block that isn't being parsed.

**Root Cause**: `preprocess_media_queries()` in css.rs is a hack that doesn't properly evaluate media queries.

**Solution**:
1. Implement proper @media query support (Phase 4)
2. Delete this hack
3. Verify the CSS rule applies correctly

**Prerequisite**: Phase 4.1 (@media query support)

---

### Hack 2: .img-link Grid Placement

**Location**: Lines 572-584
**Code**:
```rust
// HACK: Both img-links for the map should be at row 2 alongside TOC
if node.has_class("img-link") {
    if let Some(href) = node.get_attribute("href") {
        if href == "https://hn.wilsonl.in" || href == "./map.png" {
            styles.grid_row_start = 2;
            styles.grid_row_end = 3;
        }
    }
}
```

**Problem**: Same as Hack 1 - @media query not parsed.

**Solution**: Same as Hack 1.

**Prerequisite**: Phase 4.1 (@media query support)

---

### Hack 3: Image Border-Radius

**Location**: Lines 586-607
**Code**:
```rust
// HACK: Add border-radius to images
if node.tag_name() == Some("img") {
    let in_article = ancestors.iter().any(|a| a.tag_name() == Some("article"));
    if in_article {
        styles.border_top_left_radius = Length::px(8.0);
        styles.border_top_right_radius = Length::px(8.0);
        styles.border_bottom_left_radius = Length::px(8.0);
        styles.border_bottom_right_radius = Length::px(8.0);
    }

    let in_banner_left = ancestors.iter().any(|a| a.has_class("banner-left"));
    if in_banner_left {
        // border-radius: 50% for circular avatar
        styles.border_top_left_radius = Length::px(16.0);
        // ...
    }
}
```

**Problem**: CSS selectors like `article img` or `.banner-left img` should work.

**Diagnosis Steps**:
1. Check if the CSS contains these rules
2. Check if selector matching works for descendant selectors
3. Check if `border-radius` property is being parsed

**Possible Root Causes**:
- Selector matching doesn't handle descendant combinators
- `border-radius` parsing might have issues with `%` values
- Rules might be inside @media blocks

**Solution**:
1. Write a test case for `article img { border-radius: 8px; }`
2. Debug selector matching if it fails
3. Fix the root cause
4. Delete the hack

**Prerequisite**: May need selector matching fixes, or Phase 4.1

---

### Hack 4: .subscribe-btn Styling

**Location**: Lines 609-663
**Code**:
```rust
// HACK: Style subscribe button - white background, blue border and text
if node.has_class("subscribe-btn") {
    styles.padding_top = Length::px(6.0);
    styles.padding_right = Length::px(12.0);
    styles.padding_bottom = Length::px(6.0);
    styles.padding_left = Length::px(12.0);
    styles.background_color = Rgba { r: 255, g: 255, b: 255, a: 255 };
    styles.color = Rgba { r: 59, g: 130, b: 246, a: 255 };
    styles.border_top_width = Length::px(1.0);
    // ... 50+ more lines of styling
}
```

**Problem**: This is ~55 lines of inline styling that should come from CSS.

**Diagnosis Steps**:
1. Find the CSS rule for `.subscribe-btn` in the author stylesheet
2. Check if it's being parsed correctly
3. Check if it's being applied correctly

**Possible Root Causes**:
- CSS might be in @media block
- Selector might not match
- Properties might not parse correctly

**Solution**:
1. Ensure CSS is parsed (check @media)
2. Ensure selector matches
3. Ensure all properties are applied
4. Delete hack

---

### Hack 5: .toc::before Pseudo-Element

**Location**: Lines 674-723
**Code**:
```rust
// HACK: Add ::before pseudo-element content for .toc elements
if node.has_class("toc") {
    // Create a synthetic container with text content
    let text_node = DomNode {
        node_type: crate::dom::DomNodeType::Text {
            content: "CONTENTS".to_string(),
        },
        children: vec![],
    };

    let before_node = DomNode {
        node_type: crate::dom::DomNodeType::Element {
            tag_name: "div".to_string(),
            attributes: vec![],
        },
        children: vec![text_node.clone()],
    };

    // Create styles for the ::before pseudo-element
    let mut before_styles = ComputedStyle::default();
    before_styles.display = Display::Block;
    before_styles.font_size = 12.0;
    before_styles.font_weight = FontWeight::Bold;
    // ...

    children.insert(0, before_styled);
}
```

**Problem**: Manually creating pseudo-element in cascade logic instead of in box generation.

**Root Cause**: `::before` and `::after` pseudo-elements aren't fully implemented.

**Solution**:
1. Complete `src/tree/pseudo_elements.rs` implementation (Phase 6)
2. Ensure CSS `content` property is parsed
3. Ensure `::before` selector matches
4. Delete hack

**Prerequisite**: Phase 6.1 (::before/::after pseudo-elements)

---

### Hack 6: Element-Specific Default Styles

**Location**: Lines 769-930 (in `get_default_styles_for_element`)
**Code**:
```rust
// CRITICAL FIX: Ensure header navigation elements are visible and styled
if node.has_class("pagetop") {
    styles.display = Display::Inline;
    styles.color = Rgba { r: 255, g: 255, b: 255, a: 255 };
    styles.font_size = 10.0;
}

if node.has_class("hnname") {
    styles.display = Display::Inline;
    styles.color = Rgba { r: 255, g: 255, b: 255, a: 255 };
    styles.font_weight = FontWeight::Bold;
    styles.font_size = 10.0;
}

// ... similar for .votearrow, .votelinks, .rank, navigation links
```

**Problem**: Class-specific styling in the "defaults" function. Defaults should only vary by tag name, not class.

**Analysis**: These are workarounds for:
1. Author CSS not being applied
2. Visibility/styling issues with specific HN elements

**Solution Options**:

**Option A**: Fix author CSS application
1. Debug why `.pagetop`, `.hnname`, etc. styles aren't applying
2. Fix the root cause
3. Delete the hacks

**Option B**: Move to user-agent stylesheet
If these are truly needed defaults (unlikely for class-based styling):
1. Add rules to `src/user_agent.css`
2. Delete the hacks

**Recommended**: Option A - fix the real issue

---

## Removal Strategy

### Dependency Graph

```
Phase 4.1 (@media queries) ──┬── Hack 1 (.toc grid)
                             ├── Hack 2 (.img-link grid)
                             └── Hack 4 (.subscribe-btn) [maybe]

Selector matching fixes ────── Hack 3 (img border-radius)
                             └── Hack 6 (element defaults)

Phase 6.1 (::before/::after) ── Hack 5 (.toc::before)
```

### Recommended Order

1. **First, implement Phase 4.1** (@media query support)
   - This unblocks Hacks 1, 2, and possibly 4

2. **Remove Hacks 1 and 2** (grid placement)
   - After @media support, verify CSS rules apply
   - Delete the hack code
   - Run visual tests

3. **Debug and remove Hack 4** (.subscribe-btn)
   - May be @media related
   - Or may be selector/property parsing issue

4. **Investigate and remove Hack 3** (image border-radius)
   - Write test case
   - Debug selector matching
   - Fix root cause
   - Delete hack

5. **Investigate and remove Hack 6** (element defaults)
   - Determine why author styles don't apply
   - Fix cascade/matching
   - Delete hacks

6. **Implement Phase 6.1** (::before/::after)
   - Then remove Hack 5

---

## Verification Steps for Each Hack Removal

For each hack:

1. **Document expected behavior**:
   - What CSS rules should apply?
   - What should the element look like?

2. **Create test case**:
   - HTML with the element
   - CSS with the rules
   - Expected rendered output

3. **Remove hack code**:
   - Delete the `if` block
   - Don't just comment it out

4. **Run tests**:
   - `cargo test`
   - Visual regression test if available

5. **Manual verification**:
   - Render a page with the element
   - Compare to expected output

---

## Expected State After Phase 3

### Code Changes

**cascade.rs** (was style/mod.rs):
- `apply_styles_internal_with_ancestors()` should have NO class checks
- Only cascading logic, no hardcoded styles

**defaults.rs** (was style/mod.rs):
- `get_default_styles_for_element()` should ONLY check tag names
- No class checks, no ID checks, no attribute checks

### Verification

```bash
# No class-based styling in cascade
grep -n "has_class" src/style/cascade.rs  # Should return nothing

# No class-based styling in defaults
grep -n "has_class" src/style/defaults.rs  # Should return nothing

# No HACK comments
grep -rn "HACK" src/  # Should return nothing

# Tests pass
cargo test
```

---

## User-Agent Stylesheet Updates

If some defaults are truly needed, add them to `src/user_agent.css`:

```css
/* Example: If HN elements need specific defaults */
/* (Though these should come from author styles) */

/* Table elements */
table {
    border-collapse: separate;
    border-spacing: 0;
}

td, th {
    padding: 1px;
}

/* Links */
a {
    color: inherit;
    text-decoration: underline;
}

/* These should NOT be in UA stylesheet (they're site-specific):
   .pagetop, .hnname, .votearrow, .votelinks, .rank
*/
```

---

## Notes

### Why These Hacks Exist

The hacks were likely added during iterative development to "make it work" for a specific test page (Hacker News or similar). They represent:

1. **Missing features**: @media queries, ::before/::after
2. **Bugs**: Selector matching, property parsing
3. **Shortcuts**: Faster than debugging the real issue

### Impact of Removal

Removing hacks will:
1. **Temporarily break rendering** of the specific test page
2. **Force fixing the real issues**
3. **Make the renderer more general**

### Testing Strategy

Keep the test page that these hacks were created for. Use it as:
1. A visual regression test
2. A target for verifying fixes
3. A measure of progress

---

*Previous Phase: [02-phase2-modules.md](./02-phase2-modules.md)*
*Next Phase: [04-phase4-css.md](./04-phase4-css.md)*
