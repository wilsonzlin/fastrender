//! Box generation - transforms styled DOM into BoxTree
//!
//! Implements the CSS box generation algorithm that determines what boxes
//! are created from DOM elements.
//!
//! CSS Specification: CSS 2.1 Section 9.2 - Box Generation
//! https://www.w3.org/TR/CSS21/visuren.html#box-gen

use crate::style::display::{Display, FormattingContextType};
use crate::tree::box_tree::{BoxNode, BoxTree, ComputedStyle, DebugInfo};
use std::sync::Arc;

/// Simplified DOM node representation
///
/// This is a placeholder for the real DOM implementation.
/// In a real browser engine, this would be the actual DOM node.
///
/// # Note
///
/// This simplified version is used for:
/// - Testing box generation in isolation
/// - Wave 2 development (before real DOM exists)
/// - Documentation examples
///
/// In production, replace with actual DOM node types.
#[derive(Debug, Clone)]
pub struct DOMNode {
    /// Element tag name (None for text nodes)
    pub tag_name: Option<String>,

    /// Element ID attribute
    pub id: Option<String>,

    /// Element classes
    pub classes: Vec<String>,

    /// Computed style for this element
    pub style: Arc<ComputedStyle>,

    /// Text content (for text nodes)
    pub text: Option<String>,

    /// Child nodes
    pub children: Vec<DOMNode>,
}

impl DOMNode {
    /// Creates a new element node
    pub fn new_element(tag_name: impl Into<String>, style: Arc<ComputedStyle>, children: Vec<DOMNode>) -> Self {
        Self {
            tag_name: Some(tag_name.into()),
            id: None,
            classes: Vec::new(),
            style,
            text: None,
            children,
        }
    }

    /// Creates a new text node
    pub fn new_text(text: impl Into<String>, style: Arc<ComputedStyle>) -> Self {
        Self {
            tag_name: None,
            id: None,
            classes: Vec::new(),
            style,
            text: Some(text.into()),
            children: Vec::new(),
        }
    }

    /// Sets element ID (builder pattern)
    pub fn with_id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Adds a class (builder pattern)
    pub fn with_class(mut self, class: impl Into<String>) -> Self {
        self.classes.push(class.into());
        self
    }

    /// Returns true if this is a text node
    pub fn is_text(&self) -> bool {
        self.text.is_some()
    }

    /// Returns true if this is an element node
    pub fn is_element(&self) -> bool {
        self.tag_name.is_some()
    }

    /// Gets the display value from computed style
    pub fn display(&self) -> Display {
        self.style.display
    }
}

/// Configuration for box generation
///
/// Controls how boxes are generated from DOM.
#[derive(Debug, Clone)]
pub struct BoxGenerationConfig {
    /// Whether to generate debug info for boxes
    pub include_debug_info: bool,

    /// Whether to insert anonymous boxes (Wave 3)
    pub insert_anonymous_boxes: bool,
}

impl BoxGenerationConfig {
    /// Creates default configuration
    pub fn new() -> Self {
        Self {
            include_debug_info: true,
            insert_anonymous_boxes: false,
        }
    }

    /// Configuration for production (minimal debug info)
    pub fn production() -> Self {
        Self {
            include_debug_info: false,
            insert_anonymous_boxes: false,
        }
    }

    /// Configuration for development (full debug info)
    pub fn development() -> Self {
        Self {
            include_debug_info: true,
            insert_anonymous_boxes: false,
        }
    }
}

impl Default for BoxGenerationConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Errors that can occur during box generation
#[derive(Debug)]
pub enum BoxGenerationError {
    /// Root element has display: none
    RootDisplayNone,

    /// Root element has display: contents
    RootDisplayContents,

    /// Invalid display value
    InvalidDisplay(String),

    /// Unsupported feature (for future use)
    Unsupported(String),
}

impl std::fmt::Display for BoxGenerationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RootDisplayNone => write!(f, "Root element has display: none"),
            Self::RootDisplayContents => write!(f, "Root element has display: contents"),
            Self::InvalidDisplay(msg) => write!(f, "Invalid display value: {}", msg),
            Self::Unsupported(msg) => write!(f, "Unsupported feature: {}", msg),
        }
    }
}

impl std::error::Error for BoxGenerationError {}

/// Box generator - transforms DOM tree into Box tree
///
/// Implements the CSS box generation algorithm from CSS 2.1 Section 9.2.
///
/// # Current Implementation (Wave 2)
///
/// - Handles display: none (skip element)
/// - Handles display: contents (skip box, but process children)
/// - Creates boxes for block, inline, flex, grid, table
/// - Creates text boxes
/// - Preserves document order
///
/// # Future Implementation (Wave 3)
///
/// - Anonymous box insertion for mixed inline/block content
/// - Pseudo-element box generation (::before, ::after)
pub struct BoxGenerator {
    config: BoxGenerationConfig,
}

impl BoxGenerator {
    /// Creates a new box generator with default configuration
    pub fn new() -> Self {
        Self {
            config: BoxGenerationConfig::default(),
        }
    }

    /// Creates a box generator with custom configuration
    pub fn with_config(config: BoxGenerationConfig) -> Self {
        Self { config }
    }

    /// Generates a box tree from a DOM tree
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Root element has `display: none`
    /// - Root element has `display: contents`
    pub fn generate(&self, dom_root: &DOMNode) -> Result<BoxTree, BoxGenerationError> {
        // Root must generate a box
        if self.should_skip_element(dom_root) {
            return Err(BoxGenerationError::RootDisplayNone);
        }

        if self.is_display_contents(dom_root) {
            return Err(BoxGenerationError::RootDisplayContents);
        }

        // Generate box for root
        let root_box = self.generate_box_for_element(dom_root)?;

        Ok(BoxTree::new(root_box))
    }

    /// Generates a box for a single DOM node (element or text)
    fn generate_box_for_element(&self, node: &DOMNode) -> Result<BoxNode, BoxGenerationError> {
        // Handle text nodes
        if node.is_text() {
            return Ok(self.create_text_box(node));
        }

        // Generate boxes for children first
        let child_boxes = self.generate_child_boxes(node)?;

        // Determine what kind of box to create
        let display = node.display();

        // Derive formatting context type (if any)
        let fc_type = display.formatting_context_type();

        // Create appropriate box type
        let box_node = match display {
            Display::Block | Display::FlowRoot | Display::ListItem => BoxNode::new_block(
                node.style.clone(),
                fc_type.unwrap_or(FormattingContextType::Block),
                child_boxes,
            ),

            Display::Inline => BoxNode::new_inline(node.style.clone(), child_boxes),

            Display::InlineBlock => {
                BoxNode::new_inline_block(node.style.clone(), FormattingContextType::Block, child_boxes)
            }

            Display::Flex | Display::InlineFlex => {
                BoxNode::new_block(node.style.clone(), FormattingContextType::Flex, child_boxes)
            }

            Display::Grid | Display::InlineGrid => {
                BoxNode::new_block(node.style.clone(), FormattingContextType::Grid, child_boxes)
            }

            Display::Table | Display::InlineTable => {
                BoxNode::new_block(node.style.clone(), FormattingContextType::Table, child_boxes)
            }

            // Table-internal boxes (simplified for Wave 2)
            Display::TableRow
            | Display::TableCell
            | Display::TableRowGroup
            | Display::TableHeaderGroup
            | Display::TableFooterGroup
            | Display::TableColumn
            | Display::TableColumnGroup
            | Display::TableCaption => {
                // For now, treat as block boxes
                BoxNode::new_block(node.style.clone(), FormattingContextType::Block, child_boxes)
            }

            Display::None | Display::Contents => {
                // These should be filtered out before reaching here
                return Err(BoxGenerationError::InvalidDisplay(format!("{:?}", display)));
            }
        };

        // Add debug info if enabled
        if self.config.include_debug_info {
            let debug_info = self.create_debug_info(node);
            Ok(box_node.with_debug_info(debug_info))
        } else {
            Ok(box_node)
        }
    }

    /// Generates boxes for all children of a node
    ///
    /// Handles:
    /// - Skipping display: none children
    /// - Adopting display: contents children's children
    /// - Preserving document order
    fn generate_child_boxes(&self, parent: &DOMNode) -> Result<Vec<BoxNode>, BoxGenerationError> {
        let mut child_boxes = Vec::new();

        for child in &parent.children {
            // Skip display: none
            if self.should_skip_element(child) {
                continue;
            }

            // Handle display: contents - adopt grandchildren
            if self.is_display_contents(child) {
                // For display: contents, skip the element but process its children
                let grandchild_boxes = self.generate_child_boxes(child)?;
                child_boxes.extend(grandchild_boxes);
                continue;
            }

            // Generate box for child
            let child_box = self.generate_box_for_element(child)?;
            child_boxes.push(child_box);
        }

        // Future (Wave 3): Insert anonymous boxes here
        // if self.config.insert_anonymous_boxes {
        //     child_boxes = self.insert_anonymous_boxes(child_boxes);
        // }

        Ok(child_boxes)
    }

    /// Creates a text box from a text node
    fn create_text_box(&self, node: &DOMNode) -> BoxNode {
        let text = node.text.as_ref().unwrap().clone();
        let box_node = BoxNode::new_text(node.style.clone(), text);

        if self.config.include_debug_info {
            let debug_info = DebugInfo::new(Some("text".to_string()), None, vec![]);
            box_node.with_debug_info(debug_info)
        } else {
            box_node
        }
    }

    /// Creates debug info from a DOM node
    fn create_debug_info(&self, node: &DOMNode) -> DebugInfo {
        DebugInfo::new(node.tag_name.clone(), node.id.clone(), node.classes.clone())
    }

    /// Returns true if element should be skipped (display: none)
    fn should_skip_element(&self, node: &DOMNode) -> bool {
        if node.is_text() {
            return false;
        }

        matches!(node.display(), Display::None)
    }

    /// Returns true if element has display: contents
    fn is_display_contents(&self, node: &DOMNode) -> bool {
        if node.is_text() {
            return false;
        }

        matches!(node.display(), Display::Contents)
    }
}

impl Default for BoxGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn default_style() -> Arc<ComputedStyle> {
        Arc::new(ComputedStyle::default())
    }

    fn style_with_display(display: Display) -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.display = display;
        Arc::new(style)
    }

    #[test]
    fn test_config_creation() {
        let config = BoxGenerationConfig::new();
        assert!(config.include_debug_info);
        assert!(!config.insert_anonymous_boxes);
    }

    #[test]
    fn test_config_production() {
        let config = BoxGenerationConfig::production();
        assert!(!config.include_debug_info);
    }

    #[test]
    fn test_config_development() {
        let config = BoxGenerationConfig::development();
        assert!(config.include_debug_info);
    }

    #[test]
    fn test_generator_creation() {
        let generator = BoxGenerator::new();
        assert!(generator.config.include_debug_info);
    }

    #[test]
    fn test_generator_with_config() {
        let config = BoxGenerationConfig::production();
        let generator = BoxGenerator::with_config(config);
        assert!(!generator.config.include_debug_info);
    }

    #[test]
    fn test_dom_node_element() {
        let dom = DOMNode::new_element("div", default_style(), vec![]);
        assert!(dom.is_element());
        assert!(!dom.is_text());
        assert_eq!(dom.tag_name, Some("div".to_string()));
    }

    #[test]
    fn test_dom_node_text() {
        let dom = DOMNode::new_text("Hello", default_style());
        assert!(dom.is_text());
        assert!(!dom.is_element());
        assert_eq!(dom.text, Some("Hello".to_string()));
    }

    #[test]
    fn test_dom_node_with_id() {
        let dom = DOMNode::new_element("div", default_style(), vec![]).with_id("header");
        assert_eq!(dom.id, Some("header".to_string()));
    }

    #[test]
    fn test_dom_node_with_class() {
        let dom = DOMNode::new_element("div", default_style(), vec![]).with_class("navbar");
        assert_eq!(dom.classes.len(), 1);
        assert_eq!(dom.classes[0], "navbar");
    }

    #[test]
    fn test_generate_single_block() {
        let generator = BoxGenerator::new();
        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);

        let box_tree = generator.generate(&dom).unwrap();
        assert!(box_tree.root.is_block_level());
        assert_eq!(box_tree.count_boxes(), 1);
    }

    #[test]
    fn test_generate_block_with_text() {
        let generator = BoxGenerator::new();
        let text = DOMNode::new_text("Hello", default_style());
        let dom = DOMNode::new_element("p", style_with_display(Display::Block), vec![text]);

        let box_tree = generator.generate(&dom).unwrap();
        assert_eq!(box_tree.count_boxes(), 2); // p + text
        assert_eq!(box_tree.count_text_boxes(), 1);
    }

    #[test]
    fn test_generate_nested_blocks() {
        let generator = BoxGenerator::new();

        let inner = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
        let outer = DOMNode::new_element("div", style_with_display(Display::Block), vec![inner]);

        let box_tree = generator.generate(&outer).unwrap();
        assert_eq!(box_tree.count_boxes(), 2); // div + p
        assert_eq!(box_tree.root.children.len(), 1);
    }

    #[test]
    fn test_generate_multiple_children() {
        let generator = BoxGenerator::new();

        let child1 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
        let child2 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
        let child3 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);

        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![child1, child2, child3]);

        let box_tree = generator.generate(&dom).unwrap();
        assert_eq!(box_tree.count_boxes(), 4); // div + 3 p's
        assert_eq!(box_tree.root.children.len(), 3);
    }

    #[test]
    fn test_skip_display_none_child() {
        let generator = BoxGenerator::new();

        let child1 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
        let child2 = DOMNode::new_element("p", style_with_display(Display::None), vec![]);
        let child3 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);

        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![child1, child2, child3]);

        let box_tree = generator.generate(&dom).unwrap();
        // child2 is skipped (display: none)
        assert_eq!(box_tree.count_boxes(), 3); // div + child1 + child3
        assert_eq!(box_tree.root.children.len(), 2);
    }

    #[test]
    fn test_display_contents_adopts_children() {
        let generator = BoxGenerator::new();

        // Create structure: div > (p display:contents > (span, span))
        let grandchild1 = DOMNode::new_element("span", style_with_display(Display::Inline), vec![]);
        let grandchild2 = DOMNode::new_element("span", style_with_display(Display::Inline), vec![]);

        let child = DOMNode::new_element(
            "p",
            style_with_display(Display::Contents),
            vec![grandchild1, grandchild2],
        );

        let root = DOMNode::new_element("div", style_with_display(Display::Block), vec![child]);

        let box_tree = generator.generate(&root).unwrap();

        // p (display:contents) doesn't generate a box, but its children do
        // So we have: div + span + span = 3 boxes
        assert_eq!(box_tree.count_boxes(), 3);
        // div's children should be the two spans
        assert_eq!(box_tree.root.children.len(), 2);
    }

    #[test]
    fn test_text_box_creation() {
        let generator = BoxGenerator::new();
        let text = DOMNode::new_text("Hello, world!", default_style());

        let text_box = generator.create_text_box(&text);

        assert!(text_box.is_text());
        assert_eq!(text_box.text(), Some("Hello, world!"));
    }

    #[test]
    fn test_debug_info_included() {
        let config = BoxGenerationConfig::development();
        let generator = BoxGenerator::with_config(config);

        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![])
            .with_id("header")
            .with_class("navbar");

        let box_tree = generator.generate(&dom).unwrap();
        assert!(box_tree.root.debug_info.is_some());

        let debug_info = box_tree.root.debug_info.as_ref().unwrap();
        assert_eq!(debug_info.tag_name, Some("div".to_string()));
        assert_eq!(debug_info.id, Some("header".to_string()));
        assert_eq!(debug_info.classes.len(), 1);
    }

    #[test]
    fn test_debug_info_excluded() {
        let config = BoxGenerationConfig::production();
        let generator = BoxGenerator::with_config(config);

        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);

        let box_tree = generator.generate(&dom).unwrap();
        assert!(box_tree.root.debug_info.is_none());
    }

    #[test]
    fn test_complex_tree_structure() {
        let generator = BoxGenerator::new();

        // Build: div > (p > text, p > text, div > p > text)
        let text1 = DOMNode::new_text("Text 1", default_style());
        let text2 = DOMNode::new_text("Text 2", default_style());
        let text3 = DOMNode::new_text("Text 3", default_style());

        let p1 = DOMNode::new_element("p", style_with_display(Display::Block), vec![text1]);
        let p2 = DOMNode::new_element("p", style_with_display(Display::Block), vec![text2]);
        let p3 = DOMNode::new_element("p", style_with_display(Display::Block), vec![text3]);

        let inner_div = DOMNode::new_element("div", style_with_display(Display::Block), vec![p3]);
        let outer_div = DOMNode::new_element("div", style_with_display(Display::Block), vec![p1, p2, inner_div]);

        let box_tree = generator.generate(&outer_div).unwrap();

        // div(root) + p + text + p + text + div + p + text = 8 boxes
        assert_eq!(box_tree.count_boxes(), 8);
        assert_eq!(box_tree.count_text_boxes(), 3);
    }

    #[test]
    fn test_inline_box_generation() {
        let generator = BoxGenerator::new();

        let text = DOMNode::new_text("Hello", default_style());
        let span = DOMNode::new_element("span", style_with_display(Display::Inline), vec![text]);
        let p = DOMNode::new_element("p", style_with_display(Display::Block), vec![span]);

        let box_tree = generator.generate(&p).unwrap();

        // p(block) + span(inline) + text = 3 boxes
        assert_eq!(box_tree.count_boxes(), 3);

        // Root is block
        assert!(box_tree.root.is_block_level());

        // First child should be inline
        assert!(box_tree.root.children[0].is_inline_level());
    }

    #[test]
    fn test_document_order_preserved() {
        let generator = BoxGenerator::new();

        let child1 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]).with_id("first");
        let child2 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]).with_id("second");
        let child3 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]).with_id("third");

        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![child1, child2, child3]);

        let box_tree = generator.generate(&dom).unwrap();

        // Check order is preserved
        assert_eq!(box_tree.root.children.len(), 3);
        let ids: Vec<_> = box_tree
            .root
            .children
            .iter()
            .filter_map(|c| c.debug_info.as_ref())
            .filter_map(|d| d.id.as_ref())
            .map(|s| s.as_str())
            .collect();

        assert_eq!(ids, vec!["first", "second", "third"]);
    }

    #[test]
    fn test_mixed_element_and_text_nodes() {
        let generator = BoxGenerator::new();

        let text1 = DOMNode::new_text("Before", default_style());
        let em = DOMNode::new_element("em", style_with_display(Display::Inline), vec![]);
        let text2 = DOMNode::new_text("After", default_style());

        let p = DOMNode::new_element("p", style_with_display(Display::Block), vec![text1, em, text2]);

        let box_tree = generator.generate(&p).unwrap();

        // p + text + em + text = 4 boxes
        assert_eq!(box_tree.count_boxes(), 4);
        assert_eq!(box_tree.count_text_boxes(), 2);
        assert_eq!(box_tree.root.children.len(), 3);
    }

    #[test]
    fn test_generator_reuse() {
        let generator = BoxGenerator::new();

        // Reuse generator for multiple trees
        for i in 0..10 {
            let dom =
                DOMNode::new_element("div", style_with_display(Display::Block), vec![]).with_id(format!("div-{}", i));

            let result = generator.generate(&dom);
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_empty_element() {
        let generator = BoxGenerator::new();
        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);

        let box_tree = generator.generate(&dom).unwrap();
        assert_eq!(box_tree.count_boxes(), 1);
        assert_eq!(box_tree.root.children.len(), 0);
    }

    #[test]
    fn test_deeply_nested_structure() {
        let generator = BoxGenerator::new();

        // Create deeply nested: div > div > div > div > p
        let mut current = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
        for _ in 0..4 {
            current = DOMNode::new_element("div", style_with_display(Display::Block), vec![current]);
        }

        let box_tree = generator.generate(&current).unwrap();
        assert_eq!(box_tree.count_boxes(), 5); // 4 divs + 1 p
    }

    #[test]
    fn test_flex_container() {
        let generator = BoxGenerator::new();

        let child = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);
        let flex_container = DOMNode::new_element("div", style_with_display(Display::Flex), vec![child]);

        let box_tree = generator.generate(&flex_container).unwrap();

        assert_eq!(box_tree.count_boxes(), 2);
        assert_eq!(box_tree.root.formatting_context(), Some(FormattingContextType::Flex));
    }

    #[test]
    fn test_grid_container() {
        let generator = BoxGenerator::new();

        let child = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);
        let grid_container = DOMNode::new_element("div", style_with_display(Display::Grid), vec![child]);

        let box_tree = generator.generate(&grid_container).unwrap();

        assert_eq!(box_tree.count_boxes(), 2);
        assert_eq!(box_tree.root.formatting_context(), Some(FormattingContextType::Grid));
    }

    #[test]
    fn test_root_display_none_error() {
        let generator = BoxGenerator::new();
        let dom = DOMNode::new_element("div", style_with_display(Display::None), vec![]);

        let result = generator.generate(&dom);
        assert!(result.is_err());
        assert!(matches!(result, Err(BoxGenerationError::RootDisplayNone)));
    }

    #[test]
    fn test_root_display_contents_error() {
        let generator = BoxGenerator::new();
        let dom = DOMNode::new_element("div", style_with_display(Display::Contents), vec![]);

        let result = generator.generate(&dom);
        assert!(result.is_err());
        assert!(matches!(result, Err(BoxGenerationError::RootDisplayContents)));
    }
}
