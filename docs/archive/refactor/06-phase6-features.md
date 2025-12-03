# Phase 6: Missing CSS Features

**Goal**: Implement CSS features needed for broader web compatibility.

**Prerequisites**: Phases 1-4 (clean architecture to build on)

**Priority**: Varies by feature - see priority matrix

---

## Priority Matrix

| Feature | Impact | Effort | Priority | Blocks |
|---------|--------|--------|----------|--------|
| ::before/::after | High | Medium | P1 | Phase 3 Hack 5 |
| Float styling | Medium | Low | P2 | TODO in stacking.rs |
| Transform | Medium | Medium | P2 | TODO in positioned.rs |
| Direction (RTL) | Medium | Medium | P2 | TODO in pipeline.rs |
| Filter property | Low | Medium | P3 | TODO in positioned.rs |
| Language attr | Low | Medium | P3 | TODO in pipeline.rs |
| Colspan/rowspan | Low | Medium | P3 | TODO in table_fixup.rs |
| Isolation property | Low | Low | P3 | Stacking contexts |

---

## Feature 6.1: ::before/::after Pseudo-Elements

### Current State

`src/tree/pseudo_elements.rs` (~950 lines) has infrastructure but not fully integrated.

**What exists**:
- `PseudoType` enum (Before, After)
- `PseudoElementConfig` struct
- `PseudoElementGenerator` struct
- `insert_pseudo_boxes()` function
- CSS `content` property parsing in `src/style/content.rs`

**What's missing/broken**:
- Integration with box generation
- Proper content value rendering
- Cascade integration for ::before/::after styles

### Implementation Plan

#### Step 1: Audit Current Implementation

```bash
# Check pseudo_elements.rs
wc -l src/tree/pseudo_elements.rs

# Check content.rs
wc -l src/style/content.rs

# Find usages
grep -r "PseudoElement" src/ --include="*.rs"
grep -r "insert_pseudo" src/ --include="*.rs"
```

#### Step 2: Verify Content Property Parsing

Test that `content: "foo"` is parsed correctly:

```rust
#[test]
fn test_content_string() {
    let css = ".foo::before { content: 'Hello'; }";
    let stylesheet = parse_stylesheet(css).unwrap();
    // Verify content value is captured
}
```

#### Step 3: Ensure Pseudo-Element Selectors Match

The `selectors` crate should handle `::before` and `::after`.

```rust
#[test]
fn test_pseudo_element_selector_parsing() {
    let css = "p::before { content: 'prefix'; }";
    let stylesheet = parse_stylesheet(css).unwrap();
    assert_eq!(stylesheet.rules.len(), 1);
}
```

#### Step 4: Integrate with Box Generation

In `src/tree/box_generation.rs`:

```rust
fn generate_box_for_element(node: &StyledNode) -> Option<BoxNode> {
    let style = &node.styles;

    // Skip elements with display: none
    if matches!(style.display, Display::None) {
        return None;
    }

    // Generate ::before pseudo-element box if applicable
    let before_box = generate_pseudo_box(node, PseudoType::Before);

    // Generate children boxes
    let mut children = generate_children_boxes(&node.children);

    // Insert ::before at start
    if let Some(before) = before_box {
        children.insert(0, before);
    }

    // Generate ::after pseudo-element box if applicable
    let after_box = generate_pseudo_box(node, PseudoType::After);
    if let Some(after) = after_box {
        children.push(after);
    }

    // Create element box
    Some(BoxNode::new_block(style.clone(), fc_type, children))
}

fn generate_pseudo_box(node: &StyledNode, pseudo_type: PseudoType) -> Option<BoxNode> {
    // Get pseudo-element styles from cascade
    let pseudo_styles = node.get_pseudo_styles(pseudo_type)?;

    // Check for content property
    let content = &pseudo_styles.content;
    if matches!(content, ContentValue::None | ContentValue::Normal) {
        return None;
    }

    // Generate content
    let content_str = content.to_string(&pseudo_styles);
    if content_str.is_empty() {
        return None;
    }

    // Create text box for content
    let text_box = BoxNode::new_text(content_str, pseudo_styles.clone());

    // Wrap in inline box
    Some(BoxNode::new_inline(pseudo_styles, vec![text_box]))
}
```

#### Step 5: Update Cascade for Pseudo-Elements

In `src/style/cascade.rs`:

```rust
pub struct StyledNode {
    pub dom_node: DomNode,
    pub styles: ComputedStyle,
    pub before_styles: Option<ComputedStyle>,  // NEW
    pub after_styles: Option<ComputedStyle>,   // NEW
    pub children: Vec<StyledNode>,
}

fn apply_styles_to_node(node: &DomNode, stylesheet: &StyleSheet) -> StyledNode {
    // Apply main styles
    let styles = compute_styles(node, stylesheet);

    // Apply ::before styles
    let before_styles = compute_pseudo_styles(node, stylesheet, PseudoType::Before);

    // Apply ::after styles
    let after_styles = compute_pseudo_styles(node, stylesheet, PseudoType::After);

    // ...
}
```

#### Step 6: Handle Content Values

The `content` property can have various values:
- `content: "string"`
- `content: attr(title)`
- `content: counter(section)`
- `content: url(image.png)`
- `content: open-quote` / `close-quote`

**Minimum for Phase 6.1**:
- String values
- `none` and `normal`

**Defer**:
- `attr()`
- `counter()`
- `url()`
- Quotes

#### Step 7: Test End-to-End

```rust
#[test]
fn test_before_pseudo_element_rendering() {
    let html = r#"
        <style>
            .item::before { content: '• '; color: blue; }
        </style>
        <p class="item">List item</p>
    "#;

    let result = FastRender::new().render(html).unwrap();
    // The rendered output should show "• List item"
    // with the bullet in blue
}
```

### Success Criteria

- [ ] `::before` creates boxes with content
- [ ] `::after` creates boxes with content
- [ ] Pseudo-element styles cascade correctly
- [ ] Phase 3 Hack 5 can be removed
- [ ] Tests pass

---

## Feature 6.2: Float Styling

### Current State

`src/style/float.rs` has types:
```rust
pub enum Float {
    None,
    Left,
    Right,
}

pub enum Clear {
    None,
    Left,
    Right,
    Both,
}
```

`ComputedStyle` may or may not have `float` and `clear` fields.

**TODO in stacking.rs:538**:
```rust
// TODO: Check style.float when Float is added to ComputedStyles
```

### Implementation Plan

#### Step 1: Ensure Fields Exist

In `style/computed.rs`:
```rust
pub struct ComputedStyle {
    // ...
    pub float: Float,
    pub clear: Clear,
}
```

#### Step 2: Parse Properties

In `style/properties.rs`:
```rust
"float" => {
    match resolved_value {
        PropertyValue::Keyword(kw) => {
            styles.float = match kw.as_str() {
                "left" => Float::Left,
                "right" => Float::Right,
                "none" | _ => Float::None,
            };
        }
        _ => {}
    }
}

"clear" => {
    match resolved_value {
        PropertyValue::Keyword(kw) => {
            styles.clear = match kw.as_str() {
                "left" => Clear::Left,
                "right" => Clear::Right,
                "both" => Clear::Both,
                "none" | _ => Clear::None,
            };
        }
        _ => {}
    }
}
```

#### Step 3: Update Stacking Context Logic

In `paint/stacking.rs`:
```rust
fn creates_stacking_context(style: &ComputedStyle) -> bool {
    // Existing checks...

    // Floated elements with certain properties create stacking contexts
    if !matches!(style.float, Float::None) {
        // Check for additional conditions
        // Note: Floats don't inherently create stacking contexts,
        // but they do participate in stacking in specific ways
    }

    // ...
}
```

#### Step 4: Resolve TODO

After implementing, remove the TODO comment.

### Success Criteria

- [ ] `float` property parsed and applied
- [ ] `clear` property parsed and applied
- [ ] TODO in stacking.rs resolved
- [ ] Float layout works (already implemented in layout/float_context.rs)

---

## Feature 6.3: Transform Property

### Current State

`css.rs` has `Transform` enum:
```rust
pub enum Transform {
    Translate(Length, Length),
    TranslateX(Length),
    TranslateY(Length),
    Scale(f32, f32),
    ScaleX(f32),
    ScaleY(f32),
    Rotate(f32),
    SkewX(f32),
    SkewY(f32),
    Matrix(f32, f32, f32, f32, f32, f32),
}
```

**TODO in positioned.rs:569**:
```rust
// TODO: Add transform, filter, isolation checks when those properties are available
```

### Implementation Plan

#### Step 1: Add to ComputedStyle

```rust
pub struct ComputedStyle {
    // ...
    pub transform: Vec<Transform>,
}
```

#### Step 2: Parse Transform Property

This is complex - transform values can be:
- `transform: translateX(10px)`
- `transform: rotate(45deg) scale(1.5)`
- `transform: matrix(1, 0, 0, 1, 0, 0)`

Need robust parsing in `style/properties.rs`.

#### Step 3: Apply During Painting

In `paint/display_list_builder.rs`:
```rust
fn build_display_commands(fragment: &FragmentNode) -> Vec<DisplayCommand> {
    let transform = compute_transform_matrix(&fragment.style.transform);

    if transform != Matrix::identity() {
        commands.push(DisplayCommand::PushTransform(transform));
    }

    // ... render content

    if transform != Matrix::identity() {
        commands.push(DisplayCommand::PopTransform);
    }
}
```

#### Step 4: Update Stacking Context

Transformed elements create stacking contexts:

```rust
fn creates_stacking_context(style: &ComputedStyle) -> bool {
    // Transform creates stacking context
    if !style.transform.is_empty() {
        return true;
    }
    // ...
}
```

### Success Criteria

- [ ] `transform` property parsed
- [ ] Transform applied during painting
- [ ] Transformed elements create stacking contexts
- [ ] TODO resolved

---

## Feature 6.4: Direction (RTL/LTR)

### Current State

**TODO in pipeline.rs:838**:
```rust
// TODO: Implement CSS direction property support
```

### Implementation Plan

#### Step 1: Add Property

```rust
pub enum Direction {
    Ltr,
    Rtl,
}

pub struct ComputedStyle {
    // ...
    pub direction: Direction,
}
```

#### Step 2: Parse Property

```rust
"direction" => {
    match resolved_value {
        PropertyValue::Keyword(kw) => {
            styles.direction = match kw.as_str() {
                "rtl" => Direction::Rtl,
                "ltr" | _ => Direction::Ltr,
            };
        }
        _ => {}
    }
}
```

#### Step 3: Use in Text Pipeline

In `text/pipeline.rs`:
```rust
fn shape_text(text: &str, style: &ComputedStyle) -> ShapedText {
    let base_direction = match style.direction {
        Direction::Rtl => unicode_bidi::Level::rtl(),
        Direction::Ltr => unicode_bidi::Level::ltr(),
    };

    // Use base_direction in bidi algorithm
    let bidi_info = BidiInfo::new(text, Some(base_direction));
    // ...
}
```

#### Step 4: Use in Inline Layout

```rust
fn layout_inline(box_node: &BoxNode, constraints: &LayoutConstraints) -> FragmentNode {
    let direction = box_node.style.direction;

    // Layout direction affects:
    // - Starting position of lines
    // - Text alignment default
    // - Float placement
}
```

### Success Criteria

- [ ] `direction` property parsed
- [ ] Affects text bidi algorithm
- [ ] Affects inline layout direction
- [ ] TODO resolved

---

## Feature 6.5: Filter Property

### Lower Priority

**TODO in positioned.rs:569** mentions filter.

### Quick Implementation

```rust
pub enum Filter {
    Blur(Length),
    Brightness(f32),
    Contrast(f32),
    // etc.
}

pub struct ComputedStyle {
    pub filter: Vec<Filter>,
}
```

Filter creates stacking context:
```rust
fn creates_stacking_context(style: &ComputedStyle) -> bool {
    if !style.filter.is_empty() {
        return true;
    }
}
```

Actually applying filters requires image processing during paint.

---

## Feature 6.6: Isolation Property

### Quick Implementation

```rust
pub enum Isolation {
    Auto,
    Isolate,
}

pub struct ComputedStyle {
    pub isolation: Isolation,
}
```

`isolation: isolate` creates stacking context:
```rust
fn creates_stacking_context(style: &ComputedStyle) -> bool {
    if matches!(style.isolation, Isolation::Isolate) {
        return true;
    }
}
```

---

## Feature 6.7: Table colspan/rowspan

### Current State

**TODO in table_fixup.rs:450**:
```rust
// TODO: Get colspan from cell style when table layout properties are added
```

### Implementation

HTML attributes:
```html
<td colspan="2" rowspan="3">...</td>
```

Need to:
1. Parse HTML attributes in DOM
2. Store in style or box node
3. Use in table layout algorithm

```rust
pub struct TableCellData {
    pub colspan: u32,
    pub rowspan: u32,
}
```

In table layout:
```rust
fn layout_table_cell(cell: &BoxNode) -> (u32, u32) {
    let colspan = cell.get_attribute("colspan")
        .and_then(|s| s.parse().ok())
        .unwrap_or(1);
    let rowspan = cell.get_attribute("rowspan")
        .and_then(|s| s.parse().ok())
        .unwrap_or(1);
    (colspan, rowspan)
}
```

---

## Validation

For each feature:

```bash
# Build
cargo build

# Run tests
cargo test

# Check TODO is resolved
grep -n "TODO.*<feature>" src/  # Should not find the old TODO
```

---

## Feature Completion Tracking

| Feature | Parsed | In Style | In Layout | In Paint | Tests | Done |
|---------|--------|----------|-----------|----------|-------|------|
| ::before/::after | [ ] | [ ] | [ ] | [ ] | [ ] | [ ] |
| Float | [ ] | [ ] | [ ] | [ ] | [ ] | [ ] |
| Transform | [ ] | [ ] | [ ] | [ ] | [ ] | [ ] |
| Direction | [ ] | [ ] | [ ] | [ ] | [ ] | [ ] |
| Filter | [ ] | [ ] | [ ] | [ ] | [ ] | [ ] |
| Isolation | [ ] | [ ] | [ ] | [ ] | [ ] | [ ] |
| Colspan/rowspan | [ ] | [ ] | [ ] | [ ] | [ ] | [ ] |

---

*Previous Phase: [05-phase5-quality.md](./05-phase5-quality.md)*
