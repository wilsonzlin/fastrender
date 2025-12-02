# Phase 1: Box Generation

**Duration:** Week 3 of Phase 1 (5-7 days)
**Prerequisites:**
- Type system complete (01-type-system.md)
- Formatting contexts defined (01-formatting-contexts.md)
**Dependencies:**
- `BoxNode`, `BoxType` types from type system
- `FormattingContextType` from formatting contexts
- Computed styles from style system
**Output:** Algorithm to transform DOM tree into Box Tree

## Objectives

Implement the **box generation algorithm** that transforms a DOM tree with computed styles into a Box Tree ready for layout.

## Context

Not every DOM node creates a box, and some DOM nodes create multiple boxes. The box generation algorithm determines:

1. **Which DOM nodes generate boxes** (e.g., `display: none` doesn't)
2. **What type of box** (block, inline, text, etc.)
3. **How many boxes** (e.g., `::before` and `::after` create additional boxes)
4. **Parent-child relationships** (e.g., anonymous boxes)

**From CSS 2.1 Section 9.2.1:**
> "Block-level elements are those elements of the source document that are formatted visually as blocks. The following values of the 'display' property make an element block-level: 'block', 'list-item', and 'table'."

**From CSS 2.1 Section 9.2.2:**
> "Inline-level elements are those elements of the source document that do not form new blocks of content; the content is distributed in lines."

## The Problem V1 Has

V1's `dom.rs` does basic HTML parsing but doesn't have a separate box generation step. It directly maps DOM nodes to layout nodes, missing:

- Anonymous box generation
- Proper handling of inline/block mixing
- `display: none` filtering
- Pseudo-element box creation

## The Solution

Create a dedicated box generation pass that runs **after** style computation but **before** layout.

```
DOM Tree + Computed Styles → Box Generation → Box Tree → Layout → Fragment Tree
```

## Step-by-Step Implementation

### Step 1: Create Box Generation Module (Day 1 Morning)

```bash
mkdir -p /home/user/fastrender/src/tree
touch /home/user/fastrender/src/tree/box_generation.rs
```

**File: `src/tree/box_generation.rs`**
```rust
//! Box generation algorithm
//!
//! Transforms a DOM tree with computed styles into a Box Tree.
//!
//! CSS Specification: CSS 2.1 Section 9.2 - Box Generation
//! https://www.w3.org/TR/CSS21/visuren.html#box-gen

use crate::dom::Node as DomNode;
use crate::tree::{BoxNode, BoxType};
use crate::style::{ComputedStyle, Display};
use crate::error::{Result, Error};
use std::sync::Arc;

/// Box generator
///
/// Converts DOM tree to Box Tree following CSS box generation rules.
pub struct BoxGenerator {
    // Configuration could go here (e.g., quirks mode)
}

impl BoxGenerator {
    /// Creates a new box generator
    pub fn new() -> Self {
        Self {}
    }

    /// Generates a box tree from a DOM tree
    ///
    /// # Arguments
    ///
    /// * `dom_root` - Root DOM node with computed styles
    ///
    /// # Returns
    ///
    /// Box tree ready for layout
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - DOM node has no computed style
    /// - Unsupported display type
    pub fn generate(&self, dom_root: &DomNode) -> Result<BoxNode> {
        self.generate_box(dom_root, None)
    }

    /// Generates a box for a single DOM node
    ///
    /// # Arguments
    ///
    /// * `node` - DOM node to process
    /// * `parent_box_type` - Type of parent box (for anonymous box generation)
    ///
    /// # Returns
    ///
    /// BoxNode, or None if node doesn't generate a box
    fn generate_box(
        &self,
        node: &DomNode,
        parent_box_type: Option<BoxType>,
    ) -> Result<BoxNode> {
        // Get computed style
        let style = node.computed_style()
            .ok_or_else(|| Error::Layout("Node has no computed style".into()))?;

        // Check if node generates a box
        if !self.generates_box(&style) {
            // Node doesn't generate a box (e.g., display: none)
            // But we still need to process children in some cases
            return Err(Error::Layout("Node doesn't generate box".into()));
        }

        // Determine box type from display value
        let box_type = self.compute_box_type(&style.display);

        // Generate boxes for children
        let mut children = Vec::new();

        for child_dom in node.children() {
            match self.generate_box(child_dom, Some(box_type)) {
                Ok(child_box) => {
                    // Check if we need anonymous boxes
                    if self.needs_anonymous_wrapper(&box_type, &child_box.box_type) {
                        // Wrap in anonymous box
                        let anon_box = self.create_anonymous_box(
                            box_type,
                            vec![child_box],
                        );
                        children.push(anon_box);
                    } else {
                        children.push(child_box);
                    }
                }
                Err(_) => {
                    // Child doesn't generate a box, skip
                }
            }
        }

        // Handle text nodes
        if let Some(text) = node.text_content() {
            if !text.trim().is_empty() {
                let text_box = self.create_text_box(text, style.clone());
                children.push(text_box);
            }
        }

        // Create the box node
        Ok(BoxNode::new(
            style.clone(),
            box_type,
            children,
            Some(node.clone()), // Store reference to DOM node for debugging
        ))
    }

    /// Checks if a node generates a box
    ///
    /// CSS 2.1 Section 9.2.1: Elements with display: none do not generate boxes
    fn generates_box(&self, style: &ComputedStyle) -> bool {
        match style.display {
            Display::None => false,
            Display::Contents => false, // CSS Display 3 - children generate boxes but element doesn't
            _ => true,
        }
    }

    /// Computes the box type from display value
    fn compute_box_type(&self, display: &Display) -> BoxType {
        match display {
            Display::Block => BoxType::Block,
            Display::Inline => BoxType::Inline,
            Display::InlineBlock => BoxType::InlineBlock,
            Display::Flex => BoxType::Flex,
            Display::InlineFlex => BoxType::InlineFlex,
            Display::Grid => BoxType::Grid,
            Display::InlineGrid => BoxType::InlineGrid,
            Display::Table => BoxType::Table,
            Display::TableRow => BoxType::TableRow,
            Display::TableCell => BoxType::TableCell,
            Display::TableRowGroup => BoxType::TableRowGroup,
            Display::TableColumn => BoxType::TableColumn,
            Display::TableColumnGroup => BoxType::TableColumnGroup,
            Display::ListItem => BoxType::Block, // TODO: Generate marker box
            _ => BoxType::Block, // Fallback
        }
    }

    /// Checks if child needs to be wrapped in anonymous box
    ///
    /// CSS 2.1 Section 9.2.1.1: Anonymous Block Boxes
    /// "If a block container box has a block-level box inside it, then we force
    /// it to have only block-level boxes inside it."
    fn needs_anonymous_wrapper(
        &self,
        parent_type: &BoxType,
        child_type: &BoxType,
    ) -> bool {
        match (parent_type, child_type) {
            // Block parent with inline child - no wrapper needed
            (BoxType::Block, BoxType::Inline) => false,

            // Block parent with text child - no wrapper needed
            (BoxType::Block, BoxType::Text) => false,

            // Table row parent with non-cell child - wrap in cell
            (BoxType::TableRow, BoxType::TableCell) => false,
            (BoxType::TableRow, _) => true,

            // Table parent with non-row child - wrap in row
            (BoxType::Table, BoxType::TableRow) => false,
            (BoxType::Table, BoxType::TableRowGroup) => false,
            (BoxType::Table, _) => true,

            // No wrapper needed for other cases
            _ => false,
        }
    }

    /// Creates an anonymous box wrapper
    ///
    /// CSS 2.1 Section 9.2.1.1: Anonymous boxes
    fn create_anonymous_box(
        &self,
        parent_type: BoxType,
        children: Vec<BoxNode>,
    ) -> BoxNode {
        // Anonymous boxes inherit style from parent
        // For now, use a default style (will be fixed in integration)
        let style = Arc::new(ComputedStyle::default());

        let box_type = match parent_type {
            BoxType::TableRow => BoxType::TableCell,
            BoxType::Table => BoxType::TableRow,
            _ => BoxType::Block,
        };

        BoxNode::new_anonymous(style, box_type, children)
    }

    /// Creates a text box
    fn create_text_box(&self, text: &str, parent_style: Arc<ComputedStyle>) -> BoxNode {
        BoxNode::new_text(text.to_string(), parent_style)
    }
}

impl Default for BoxGenerator {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 2: Handle Anonymous Box Generation (Day 1 Afternoon - Day 2)

The most complex part of box generation is **anonymous boxes**. These are boxes that don't correspond to a DOM element but are created by the CSS engine.

**Example: Mixing inline and block content**

HTML:
```html
<div>
  Some text
  <p>A paragraph</p>
  More text
</div>
```

Box tree (anonymous boxes marked with `[anon]`):
```
Block <div>
├── [anon] Inline block
│   └── Text "Some text"
├── Block <p>
│   └── Text "A paragraph"
└── [anon] Inline block
    └── Text "More text"
```

**Implementation:**

```rust
impl BoxGenerator {
    /// Generates children with anonymous box insertion
    ///
    /// CSS 2.1 Section 9.2.1.1: Anonymous block boxes
    /// "If a block container box contains a block-level box, then we force it
    /// to have only block-level boxes inside it."
    fn generate_children_with_anonymous_boxes(
        &self,
        parent: &DomNode,
        parent_box_type: BoxType,
    ) -> Result<Vec<BoxNode>> {
        let mut children = Vec::new();
        let mut inline_run: Vec<BoxNode> = Vec::new();

        for child_dom in parent.children() {
            match self.generate_box(child_dom, Some(parent_box_type)) {
                Ok(child_box) => {
                    let is_block_level = child_box.box_type.is_block_level();

                    if parent_box_type == BoxType::Block {
                        if is_block_level {
                            // Block-level child

                            // First, wrap any accumulated inline boxes
                            if !inline_run.is_empty() {
                                let anon = self.create_anonymous_inline_block(
                                    std::mem::take(&mut inline_run)
                                );
                                children.push(anon);
                            }

                            // Add the block-level child
                            children.push(child_box);
                        } else {
                            // Inline-level child - accumulate
                            inline_run.push(child_box);
                        }
                    } else {
                        // Non-block parent - add directly
                        children.push(child_box);
                    }
                }
                Err(_) => {
                    // Child doesn't generate a box, skip
                }
            }
        }

        // Wrap any remaining inline boxes
        if !inline_run.is_empty() {
            let anon = self.create_anonymous_inline_block(inline_run);
            children.push(anon);
        }

        Ok(children)
    }

    /// Creates an anonymous block box wrapping inline content
    fn create_anonymous_inline_block(&self, inline_boxes: Vec<BoxNode>) -> BoxNode {
        let style = Arc::new(ComputedStyle::default());
        BoxNode::new_anonymous(style, BoxType::Block, inline_boxes)
    }
}
```

### Step 3: Extend BoxNode with Metadata (Day 2)

Update `BoxNode` to store information needed for box generation:

```rust
// In src/tree/box_tree.rs

pub struct BoxNode {
    pub style: Arc<ComputedStyle>,
    pub box_type: BoxType,
    pub children: Vec<BoxNode>,

    // NEW: Metadata for debugging and error handling
    pub source_node: Option<DomNode>,  // Which DOM node created this box
    pub is_anonymous: bool,             // Is this an anonymous box?
    pub debug_name: Option<String>,     // For debugging (e.g., "div.container")
}

impl BoxNode {
    /// Creates a new box from a DOM node
    pub fn new(
        style: Arc<ComputedStyle>,
        box_type: BoxType,
        children: Vec<BoxNode>,
        source_node: Option<DomNode>,
    ) -> Self {
        Self {
            style,
            box_type,
            children,
            source_node,
            is_anonymous: false,
            debug_name: None,
        }
    }

    /// Creates an anonymous box
    pub fn new_anonymous(
        style: Arc<ComputedStyle>,
        box_type: BoxType,
        children: Vec<BoxNode>,
    ) -> Self {
        Self {
            style,
            box_type,
            children,
            source_node: None,
            is_anonymous: true,
            debug_name: Some("[anonymous]".into()),
        }
    }

    /// Creates a text box
    pub fn new_text(text: String, parent_style: Arc<ComputedStyle>) -> Self {
        Self {
            style: parent_style,
            box_type: BoxType::Text(text),
            children: vec![],
            source_node: None,
            is_anonymous: false,
            debug_name: Some("[text]".into()),
        }
    }
}
```

### Step 4: Handle Table Box Generation (Day 3)

Table box generation is particularly complex due to implicit elements:

**HTML:**
```html
<table>
  <tr>
    <td>Cell</td>
  </tr>
</table>
```

**Missing:** `<tbody>` element (implicit)

**Box tree should be:**
```
Table <table>
└── TableRowGroup [anonymous tbody]
    └── TableRow <tr>
        └── TableCell <td>
            └── Text "Cell"
```

**Implementation:**

```rust
impl BoxGenerator {
    /// Fixes up table structure by adding implicit elements
    ///
    /// CSS Tables Module Section 3: Missing cells, rows, and row groups
    fn fixup_table_structure(&self, table_box: &mut BoxNode) -> Result<()> {
        assert!(table_box.box_type == BoxType::Table);

        // Collect children by type
        let mut row_groups = vec![];
        let mut rows = vec![];
        let mut other = vec![];

        for child in table_box.children.drain(..) {
            match child.box_type {
                BoxType::TableRowGroup => row_groups.push(child),
                BoxType::TableRow => rows.push(child),
                _ => other.push(child),
            }
        }

        // Wrap orphan rows in anonymous tbody
        if !rows.is_empty() {
            let anon_tbody = BoxNode::new_anonymous(
                table_box.style.clone(),
                BoxType::TableRowGroup,
                rows,
            );
            row_groups.push(anon_tbody);
        }

        // Now table contains only row groups
        table_box.children = row_groups;

        // Fixup each row to ensure all children are cells
        for row_group in &mut table_box.children {
            for row in &mut row_group.children {
                self.fixup_table_row(row)?;
            }
        }

        Ok(())
    }

    /// Ensures all children of a table row are table cells
    fn fixup_table_row(&self, row: &mut BoxNode) -> Result<()> {
        assert!(row.box_type == BoxType::TableRow);

        let mut fixed_children = vec![];

        for child in row.children.drain(..) {
            if child.box_type == BoxType::TableCell {
                fixed_children.push(child);
            } else {
                // Wrap non-cell in anonymous cell
                let anon_cell = BoxNode::new_anonymous(
                    row.style.clone(),
                    BoxType::TableCell,
                    vec![child],
                );
                fixed_children.push(anon_cell);
            }
        }

        row.children = fixed_children;
        Ok(())
    }
}
```

### Step 5: Integration with Rendering Pipeline (Day 4)

Update the main rendering pipeline to use box generation:

```rust
// In src/renderer.rs

pub struct Renderer {
    dom_parser: DomParser,
    style_engine: StyleEngine,
    box_generator: BoxGenerator,  // NEW
    layout_engine: LayoutEngine,
    painter: Painter,
}

impl Renderer {
    pub fn render(&self, html: &str, css: &str, width: u32, height: u32) -> Result<Image> {
        // 1. Parse HTML → DOM tree
        let dom = self.dom_parser.parse(html)?;

        // 2. Compute styles
        self.style_engine.compute_styles(&dom, css)?;

        // 3. Generate box tree (NEW!)
        let box_tree = self.box_generator.generate(&dom)?;

        // 4. Layout → Fragment tree
        let fragment_tree = self.layout_engine.layout(&box_tree, width as f32, height as f32)?;

        // 5. Paint → Image
        self.painter.paint(&fragment_tree, width, height)
    }
}
```

### Step 6: Write Tests (Day 5)

**File: `tests/tree/box_generation_test.rs`**
```rust
//! Tests for box generation

use fastrender::tree::{BoxGenerator, BoxType};
use fastrender::dom::DomParser;
use fastrender::style::{StyleEngine, Display};

#[test]
fn test_simple_block_generates_box() {
    let html = "<div>Hello</div>";
    let dom = DomParser::new().parse(html).unwrap();

    // Apply default styles
    let style_engine = StyleEngine::new();
    style_engine.compute_styles(&dom, "").unwrap();

    let generator = BoxGenerator::new();
    let box_tree = generator.generate(&dom).unwrap();

    assert_eq!(box_tree.box_type, BoxType::Block);
    assert_eq!(box_tree.children.len(), 1); // Text child
}

#[test]
fn test_display_none_generates_no_box() {
    let html = "<div style='display: none'>Hidden</div>";
    let dom = DomParser::new().parse(html).unwrap();

    let style_engine = StyleEngine::new();
    style_engine.compute_styles(&dom, "").unwrap();

    let generator = BoxGenerator::new();
    let result = generator.generate(&dom);

    // Should fail because root doesn't generate a box
    assert!(result.is_err());
}

#[test]
fn test_anonymous_box_for_mixed_content() {
    let html = r#"
        <div>
            Text before
            <p>Paragraph</p>
            Text after
        </div>
    "#;

    let dom = DomParser::new().parse(html).unwrap();
    let style_engine = StyleEngine::new();
    style_engine.compute_styles(&dom, "").unwrap();

    let generator = BoxGenerator::new();
    let box_tree = generator.generate(&dom).unwrap();

    // Should have 3 children: [anon], <p>, [anon]
    assert_eq!(box_tree.children.len(), 3);

    // First and last should be anonymous
    assert!(box_tree.children[0].is_anonymous);
    assert!(box_tree.children[2].is_anonymous);

    // Middle should be the <p>
    assert!(!box_tree.children[1].is_anonymous);
}

#[test]
fn test_table_structure_fixup() {
    let html = r#"
        <table>
            <tr>
                <td>Cell 1</td>
                <td>Cell 2</td>
            </tr>
        </table>
    "#;

    let dom = DomParser::new().parse(html).unwrap();
    let style_engine = StyleEngine::new();
    style_engine.compute_styles(&dom, "div { display: block; }
                                       table { display: table; }
                                       tr { display: table-row; }
                                       td { display: table-cell; }").unwrap();

    let generator = BoxGenerator::new();
    let box_tree = generator.generate(&dom).unwrap();

    assert_eq!(box_tree.box_type, BoxType::Table);

    // Should have created anonymous tbody
    assert_eq!(box_tree.children.len(), 1);
    assert_eq!(box_tree.children[0].box_type, BoxType::TableRowGroup);
    assert!(box_tree.children[0].is_anonymous);

    // tbody should contain the row
    let tbody = &box_tree.children[0];
    assert_eq!(tbody.children.len(), 1);
    assert_eq!(tbody.children[0].box_type, BoxType::TableRow);

    // Row should contain 2 cells
    let row = &tbody.children[0];
    assert_eq!(row.children.len(), 2);
    assert_eq!(row.children[0].box_type, BoxType::TableCell);
    assert_eq!(row.children[1].box_type, BoxType::TableCell);
}

#[test]
fn test_text_box_creation() {
    let html = "<div>Hello World</div>";
    let dom = DomParser::new().parse(html).unwrap();

    let style_engine = StyleEngine::new();
    style_engine.compute_styles(&dom, "").unwrap();

    let generator = BoxGenerator::new();
    let box_tree = generator.generate(&dom).unwrap();

    assert_eq!(box_tree.children.len(), 1);

    let text_box = &box_tree.children[0];
    assert!(matches!(text_box.box_type, BoxType::Text(_)));

    if let BoxType::Text(text) = &text_box.box_type {
        assert_eq!(text.trim(), "Hello World");
    }
}

#[test]
fn test_nested_blocks() {
    let html = r#"
        <div>
            <div>
                <div>Nested</div>
            </div>
        </div>
    "#;

    let dom = DomParser::new().parse(html).unwrap();
    let style_engine = StyleEngine::new();
    style_engine.compute_styles(&dom, "").unwrap();

    let generator = BoxGenerator::new();
    let box_tree = generator.generate(&dom).unwrap();

    assert_eq!(box_tree.box_type, BoxType::Block);
    assert_eq!(box_tree.children.len(), 1);

    let child = &box_tree.children[0];
    assert_eq!(child.box_type, BoxType::Block);
    assert_eq!(child.children.len(), 1);

    let grandchild = &child.children[0];
    assert_eq!(grandchild.box_type, BoxType::Block);
}
```

## Acceptance Criteria

- [ ] `BoxGenerator` can convert simple DOM trees to box trees
- [ ] `display: none` elements don't generate boxes
- [ ] Anonymous boxes are created for mixed inline/block content
- [ ] Table structure is fixed up (implicit tbody, row, cell wrappers)
- [ ] Text nodes create text boxes
- [ ] All tests pass: `cargo test box_generation`
- [ ] Box tree can be pretty-printed for debugging
- [ ] Documentation explains anonymous box rules with examples

## Common Pitfalls

### Pitfall 1: Forgetting Anonymous Boxes

**Wrong:**
```rust
// Just add all children directly
for child in dom.children() {
    box_node.children.push(generate_box(child));
}
```

**Right:**
```rust
// Check if anonymous boxes are needed
let children = generate_children_with_anonymous_boxes(dom, parent_box_type);
box_node.children = children;
```

**Why:** CSS requires anonymous boxes in many situations. Missing them breaks layout.

### Pitfall 2: Not Handling `display: none`

**Wrong:**
```rust
fn generate_box(node: &DomNode) -> BoxNode {
    // Always create a box
    BoxNode::new(...)
}
```

**Right:**
```rust
fn generate_box(node: &DomNode) -> Result<BoxNode> {
    if node.style().display == Display::None {
        return Err(Error::Layout("No box generated"));
    }
    Ok(BoxNode::new(...))
}
```

**Why:** `display: none` elements should not generate any boxes.

### Pitfall 3: Ignoring Table Structure Rules

**Wrong:**
```rust
// Allow any children in a table row
TableRow { children: vec![Block, Inline, TableCell] }
```

**Right:**
```rust
// Enforce table structure: rows contain only cells
fn fixup_table_row(row: &mut BoxNode) {
    for child in &row.children {
        if child.box_type != BoxType::TableCell {
            // Wrap in anonymous cell
        }
    }
}
```

**Why:** CSS table layout algorithm requires strict structure.

## Next Steps

After box generation is complete:

1. **Phase 1.4:** Integration (01-integration.md)
   - Wire box generation into renderer
   - Add debug visualization for box tree
   - Test end-to-end pipeline

2. **Phase 2:** Implement layout algorithms
   - Box tree is now ready for layout
   - Each formatting context operates on box tree

## References

- **CSS 2.1 Section 9.2:** Box Generation
  - https://www.w3.org/TR/CSS21/visuren.html#box-gen
- **CSS 2.1 Section 9.2.1.1:** Anonymous Block Boxes
  - https://www.w3.org/TR/CSS21/visuren.html#anonymous-block-level
- **CSS Tables Module Section 3:** Missing Cells, Rows, and Row Groups
  - https://www.w3.org/TR/css-tables-3/#fixup
- **Servo's box generation:**
  - `components/layout_2020/flow/construct.rs`

## Timeline

- **Day 1:** Basic box generator structure, simple cases
- **Day 2:** Anonymous box generation for mixed content
- **Day 3:** Table structure fixup
- **Day 4:** Integration with rendering pipeline
- **Day 5:** Tests and documentation

**Total: 5-7 days**

---

**Last Updated:** 2025-01-19
**Status:** Ready for Implementation
**Blocking:** Phase 1.4 (Integration)
