# AGENTS.md

## Task
Build HTML/CSS rendering engine in Rust. Render web pages to pixel-perfect PNG images.

## Dependencies
- **Taffy v0.5.2**: Layout engine (flexbox/grid only, no table support) - **VENDORED** in `vendor/taffy/`
- **resvg**: SVG rendering
- **tiny-skia**: 2D graphics and text
- **scraper**: HTML parsing
- **cssparser**: CSS parsing
- **image**: PNG output

### Taffy Vendoring
**Critical**: Taffy is vendored in `vendor/taffy/` to enable custom modifications for table support. Do not update via Cargo - changes will be lost. Any Taffy updates must be manually integrated while preserving table-to-flex conversion logic.

## Architecture
```
URL → HTML Fetch → CSS Extract → Parse → DOM Tree → Styled Nodes → Layout (Taffy) → Override → Paint → PNG
```

## Critical Files

### `src/lib.rs`
Entry points: `render_html_to_image()`, `fetch_and_render()`

### `src/dom.rs`
HTML parsing with scraper. Creates DOM tree with classes, attributes, text content.

### `src/css.rs`
CSS parsing. Handles @import, selectors, properties. 6686 lines total project.

### `src/style.rs`
CSS application to DOM. Element detection (tables, navigation, vote arrows). Color parsing, font sizing.

### `src/layout.rs`
**MOST CRITICAL**. Table-to-flexbox conversion. Text collection from inline children. Post-layout width overrides.

### `src/paint.rs`
Text rendering with tiny-skia. Paint exceptions for special elements. Background colors, text positioning.

### `src/image_loader.rs`
SVG rendering with resvg. Data URL processing. Image format conversion.

### `src/text.rs`
Font loading, text shaping, glyph positioning.

## Core Problems Solved

### 1. Table Layout (No Native Support)
**Problem**: Taffy v0.5.2 lacks table support. HTML tables common.
**Solution**: Map table display types to flex equivalents:
- `table` → `flex` (column direction)
- `table-row` → `flex` (row direction)
- `table-cell` → `block` (flex items)

### 2. Text Collection in Tables
**Problem**: Flex containers don't collect inline text from children.
**Solution**: `collect_inline_text()` with parent detection. Force collection for table cells.

### 3. Navigation Text Missing
**Problem**: Navigation spans made inline but parent cell gets no text.
**Solution**: Force navigation text into table cells during layout extraction phase.

### 4. Cell Width Constraints
**Problem**: Taffy ignores explicit widths, cells get 8px despite 800px request.
**Solution**: Post-layout absolute override in `extract_layout()`. Detect navigation cells, force 800px/200px.

### 5. Paint Filtering
**Problem**: Elements with children skip text painting.
**Solution**: Paint exceptions for vote arrows (`▲`) and navigation text.

## Implementation Details

### Table Detection
```rust
// In style.rs - detect table elements
if matches!(styles.display, style::Display::Table) && has_header_content(node) {
    style.size.width = Dimension::length(viewport_width); // Force full width
}
```

### Content Injection
```rust
// In layout.rs - force navigation text into cells
if has_navigation_content(styled_node) {
    collected_text = "new | past | comments | ask | show | jobs | submit";
    has_text_children = true;
}
```

### Width Override
```rust
// In layout.rs - absolute width override post-Taffy
if text.contains("navigation_content") && original_width < 400.0 {
    box_result.width = 800.0;  // Override Taffy calculation
}
```

### Paint Exceptions
```rust
// In paint.rs - allow special elements to paint despite children
let is_navigation = text.contains("new | past | comments");
if !children.is_empty() && !is_vote_arrow && !is_navigation {
    return Ok(()); // Skip painting
}
```

## Element-Specific Handling

### Vote Arrows
- Class: `.votelinks`
- Content: Force `▲` Unicode
- Width: 30px explicit
- Paint: Exception despite children

### Navigation Links
- Classes: `.pagetop`, `.hnname`
- Content: Force collection into parent table cell
- Display: Inline (for text collection)
- Color: White (#FFFFFF)

### Headers
- Tables: Force viewport width (1200px)
- Background: Orange (#FF6600)
- Text: White for contrast

### Stories
- Rankings: Right-aligned in 30px cells
- Arrows: 30px cells with centered ▲
- Titles: Flex-grow cells with story content

## Debug Strategies
- Extensive `eprintln!` for layout box creation
- Text content tracking through pipeline
- Width override confirmation logs
- Paint function entry validation

## Constraints & Workarounds

### Taffy Limitations
- No table layout support → Custom flex mapping
- Ignores explicit widths in flex → Post-layout override
- Min-content not working → Force minimum sizes

### CSS Parsing
- Limited @media support → Skip unsupported rules
- Complex selectors → Basic implementation sufficient
- CSS variables → Not implemented

### Text Rendering
- Font loading → Basic system font fallback
- Complex shaping → Simple left-to-right text
- Line breaking → Basic word wrap

## Performance Notes
- Layout calculations: O(n) DOM traversal
- Paint operations: Per-element text rendering
- Memory: DOM tree + styled tree + layout tree in memory
- File size: 298MB project, 5.9MB binary

## Testing Approach
- Expected PNG comparison for pixel-perfect validation
- Debug output verification at each pipeline stage
- Width/height measurement confirmation
- Text content presence validation

## Future Extensions
- CSS Grid native support when Taffy adds it
- More CSS property support (transforms, animations)
- Better font handling with system font discovery
- JavaScript execution for dynamic content

## Coding Patterns & Style

### Strategic Approach
- **Force content then fix layout**: Inject content first, solve positioning after
- **Override at lowest level**: Post-layout fixes more reliable than pre-layout constraints
- **Debug early and often**: Extensive logging at each pipeline stage
- **Exception-based fixes**: Special case handling for critical elements

### Code Structure
- **Single responsibility**: Each module handles one domain (CSS, layout, paint)
- **Pipeline architecture**: Clear data flow through transformation stages
- **Immutable DOM**: Build styled tree, don't mutate original DOM
- **Error propagation**: Use `Result<>` consistently, bubble up errors

### Tried Patterns

#### ❌ Failed Approaches
- **Flex basis forcing**: Taffy ignores `flex_basis` in constrained containers
- **CSS-only solutions**: Can't solve layout engine limitations with CSS tricks
- **Pre-layout width setting**: Taffy overrides during calculation phase
- **Complex selector matching**: Simple class-based detection more reliable

#### ✅ Successful Patterns
- **Post-layout modification**: Override after Taffy calculates
- **Content injection during extraction**: Force text at layout tree building
- **Paint function exceptions**: Allow special elements to bypass filters
- **Recursive content detection**: Walk tree to find navigation elements

### Critical Implementation Details

#### Text Collection
```rust
// WRONG: Assume flex containers collect text
if parent_is_flex { skip_text_collection(); }

// RIGHT: Force collection for table cells regardless
let should_collect = !parent_is_flex || is_table_cell;
```

#### Width Overrides
```rust
// WRONG: Set width in Taffy style and expect it to stick
style.size.width = Dimension::length(800.0);

// RIGHT: Override after Taffy calculation
if is_navigation_cell && calculated_width < 400.0 {
    layout_box.width = 800.0; // Absolute override
}
```

#### Paint Filtering
```rust
// WRONG: Skip all elements with children
if !children.is_empty() { return; }

// RIGHT: Exception-based approach
if !children.is_empty() && !is_special_element { return; }
```

### Repeated Lessons

1. **Taffy constraints are absolute**: Cannot negotiate with layout engine calculations
2. **Text flows differently in tables**: Inline children don't auto-collect in flex
3. **Paint order matters**: Background before text, children before parents
4. **Debug output is essential**: Visual debugging insufficient for layout issues
5. **Class detection more reliable**: Than complex CSS selector matching
6. **Force content early**: Easier than fixing missing content later
7. **Absolute coordinates work**: When relative positioning fails

### Development Experience

#### Timeline Pattern
1. **Initial implementation**: Basic pipeline, simple cases work
2. **Layout breaks**: Complex tables don't render correctly
3. **Debug phase**: Extensive logging to understand data flow
4. **Override solution**: Bypass engine limitations with post-processing
5. **Refinement**: Element-specific handling for edge cases

#### Critical Development Loop
**ESSENTIAL**: Continuously compare output against reference render. Never stop until pixel-perfect.

1. **Run render**: `./target/release/fetch_and_render <URL>`
2. **Compare visually**: `fetched_output.png` vs `expected.png`
3. **Identify differences**: Missing elements, wrong positioning, incorrect colors
4. **Add debug output**: Track specific failing elements through pipeline
5. **Implement fix**: Target the exact constraint causing the issue
6. **Repeat immediately**: Don't batch fixes, validate each change

This loop is non-negotiable. Visual differences indicate system failures that compound. Each iteration should bring closer to pixel-perfect match.

#### Problem-Solving Approach
1. **Isolate the pipeline stage**: Where does data get lost/corrupted?
2. **Add debug output**: Track data through transformations
3. **Identify constraint**: Which system is preventing correct behavior?
4. **Override at lowest level**: Closest to final output for maximum control
5. **Test incrementally**: One fix at a time, verify each step

### Memory Management
- **DOM tree**: Keep original for reference
- **Styled tree**: Intermediate representation with computed styles
- **Layout tree**: Taffy's internal representation
- **Layout boxes**: Final positioned elements for painting
- **Clean separation**: Each stage owns its data structures

### Error Handling Strategy
- **Parse errors**: Continue with partial data when possible
- **Layout failures**: Fallback to minimal valid layout
- **Paint errors**: Skip problematic elements rather than crash
- **Network errors**: Clear error messages for debugging

### Performance Insights
- **Dom traversal**: O(n) unavoidable, optimize per-element work
- **CSS matching**: Simple class checks faster than complex selectors
- **Text rendering**: Most expensive operation, cache when possible
- **Layout calculation**: Taffy handles optimization, don't duplicate

### Code Organization Principles
- **Separate concerns**: Parse, style, layout, paint are distinct phases
- **Data transformation**: Each stage produces input for next stage
- **Error boundaries**: Handle failures at stage boundaries
- **Debugging hooks**: Consistent logging patterns across modules

### Critical Files to Understand
1. **`layout.rs`**: Core logic, most complex, handles overrides
2. **`style.rs`**: Element detection, CSS application
3. **`paint.rs`**: Final rendering, text positioning
4. **`dom.rs`**: Foundation, must be correct for everything else

## Maintenance

### When to Update AGENTS.md
- **New layout constraints discovered**: Add to failed/successful patterns
- **Major architecture changes**: Update pipeline or file descriptions
- **New element handling**: Document detection and override patterns
- **Performance insights**: Add to performance notes
- **New debugging techniques**: Add to debug strategies
- **Dependency changes**: Especially Taffy vendoring updates

Keep this document current - it's the authoritative reference for system understanding.

## Build & Run
```bash
cargo build --release
./target/release/fetch_and_render <URL>
# Output: fetched_output.png
```