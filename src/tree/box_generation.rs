//! Box generation - transforms styled DOM into BoxTree
//!
//! Implements the CSS box generation algorithm that determines what boxes
//! are created from DOM elements.
//!
//! CSS Specification: CSS 2.1 Section 9.2 - Box Generation
//! https://www.w3.org/TR/CSS21/visuren.html#box-gen

use crate::geometry::Size;
use crate::style::display::{Display, FormattingContextType};
use crate::style::ComputedStyle;
use crate::tree::anonymous::AnonymousBoxCreator;
use crate::tree::box_tree::{BoxNode, BoxTree, BoxType, ReplacedType};
use crate::tree::debug::DebugInfo;
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

    /// Intrinsic size for replaced elements (images, video, etc.)
    ///
    /// This is set when the element has known dimensions (e.g., from
    /// width/height attributes on <img> or natural dimensions).
    pub intrinsic_size: Option<Size>,

    /// Source URL for replaced elements (img src, video src, etc.)
    pub src: Option<String>,
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
            intrinsic_size: None,
            src: None,
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
            intrinsic_size: None,
            src: None,
        }
    }

    /// Creates a new replaced element (img, video, canvas, etc.)
    ///
    /// # Arguments
    ///
    /// * `tag_name` - Element tag (img, video, canvas, svg, iframe)
    /// * `style` - Computed style for the element
    /// * `src` - Source URL or data URI
    /// * `intrinsic_size` - Natural dimensions of the content (if known)
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::tree::DOMNode;
    /// use fastrender::tree::box_tree::ComputedStyle;
    /// use fastrender::geometry::Size;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let img = DOMNode::new_replaced("img", style, "image.png", Some(Size::new(100.0, 50.0)));
    ///
    /// assert!(img.is_replaced_element());
    /// assert_eq!(img.intrinsic_size, Some(Size::new(100.0, 50.0)));
    /// ```
    pub fn new_replaced(
        tag_name: impl Into<String>,
        style: Arc<ComputedStyle>,
        src: impl Into<String>,
        intrinsic_size: Option<Size>,
    ) -> Self {
        Self {
            tag_name: Some(tag_name.into()),
            id: None,
            classes: Vec::new(),
            style,
            text: None,
            children: Vec::new(),
            intrinsic_size,
            src: Some(src.into()),
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

    /// Sets intrinsic size (builder pattern)
    ///
    /// Used for replaced elements to specify their natural dimensions.
    pub fn with_intrinsic_size(mut self, size: Size) -> Self {
        self.intrinsic_size = Some(size);
        self
    }

    /// Sets source URL (builder pattern)
    ///
    /// Used for replaced elements (img src, video src, etc.)
    pub fn with_src(mut self, src: impl Into<String>) -> Self {
        self.src = Some(src.into());
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

    /// Returns true if this is a replaced element
    ///
    /// Replaced elements are elements whose content is outside the scope of
    /// the CSS formatting model. Examples include:
    /// - `<img>` - Images
    /// - `<video>` - Video
    /// - `<canvas>` - Canvas drawing surface
    /// - `<svg>` - SVG graphics
    /// - `<iframe>` - Nested browsing contexts
    /// - `<embed>` - External content
    /// - `<object>` - External resources
    ///
    /// # CSS 2.1 Section 3.1
    ///
    /// "An element whose content is outside the scope of the CSS formatting
    /// model, such as an image, embedded document, or applet."
    pub fn is_replaced_element(&self) -> bool {
        if let Some(tag) = &self.tag_name {
            matches!(
                tag.as_str(),
                "img" | "video" | "canvas" | "svg" | "iframe" | "embed" | "object" | "audio"
            )
        } else {
            false
        }
    }

    /// Gets the display value from computed style
    pub fn display(&self) -> Display {
        self.style.display
    }

    /// Computes the aspect ratio from intrinsic size
    ///
    /// Returns width / height, or None if intrinsic size is not set
    /// or height is zero.
    pub fn aspect_ratio(&self) -> Option<f32> {
        self.intrinsic_size.and_then(|size| {
            if size.height > 0.0 {
                Some(size.width / size.height)
            } else {
                None
            }
        })
    }

    /// Gets the replaced element type based on tag name
    ///
    /// Returns None if this is not a replaced element.
    pub fn replaced_type(&self) -> Option<ReplacedType> {
        if !self.is_replaced_element() {
            return None;
        }

        let tag = self.tag_name.as_ref()?;
        let src = self.src.clone().unwrap_or_default();

        match tag.as_str() {
            "img" => Some(ReplacedType::Image { src }),
            "video" => Some(ReplacedType::Video { src }),
            "canvas" => Some(ReplacedType::Canvas),
            "svg" => Some(ReplacedType::Svg { content: src }),
            "iframe" => Some(ReplacedType::Iframe { src }),
            _ => None,
        }
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

    /// Invalid box tree structure
    ///
    /// Examples: text box with children, replaced box with children
    InvalidStructure(String),
}

impl std::fmt::Display for BoxGenerationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RootDisplayNone => write!(f, "Root element has display: none"),
            Self::RootDisplayContents => write!(f, "Root element has display: contents"),
            Self::InvalidDisplay(msg) => write!(f, "Invalid display value: {}", msg),
            Self::Unsupported(msg) => write!(f, "Unsupported feature: {}", msg),
            Self::InvalidStructure(msg) => write!(f, "Invalid structure: {}", msg),
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

        // Apply anonymous box fixup if enabled
        let final_root = if self.config.insert_anonymous_boxes {
            AnonymousBoxCreator::fixup_tree(root_box)
        } else {
            root_box
        };

        Ok(BoxTree::new(final_root))
    }

    /// Generates a box tree with anonymous box fixup
    ///
    /// This is a convenience method that generates the box tree with
    /// anonymous box insertion enabled, regardless of the config setting.
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Root element has `display: none`
    /// - Root element has `display: contents`
    pub fn generate_with_anonymous_fixup(&self, dom_root: &DOMNode) -> Result<BoxTree, BoxGenerationError> {
        // Root must generate a box
        if self.should_skip_element(dom_root) {
            return Err(BoxGenerationError::RootDisplayNone);
        }

        if self.is_display_contents(dom_root) {
            return Err(BoxGenerationError::RootDisplayContents);
        }

        // Generate box for root
        let root_box = self.generate_box_for_element(dom_root)?;

        // Always apply anonymous box fixup
        let fixed_root = AnonymousBoxCreator::fixup_tree(root_box);

        Ok(BoxTree::new(fixed_root))
    }

    /// Generates a box for a single DOM node (element or text)
    ///
    /// # CSS 2.1 Section 9.2
    ///
    /// Box generation determines what type of box is created based on:
    /// 1. Element type (text node, element, replaced element)
    /// 2. Display property value
    /// 3. Replaced element status (img, video, etc.)
    fn generate_box_for_element(&self, node: &DOMNode) -> Result<BoxNode, BoxGenerationError> {
        // Handle text nodes
        if node.is_text() {
            return Ok(self.create_text_box(node));
        }

        // Handle replaced elements (img, video, canvas, etc.)
        // Replaced elements don't have children in the CSS sense
        if node.is_replaced_element() {
            return self.create_replaced_box(node);
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

    /// Creates a replaced box for replaced elements (img, video, canvas, etc.)
    ///
    /// # CSS 2.1 Section 10.3.2
    ///
    /// "Replaced elements are those whose content is outside the scope of
    /// the CSS formatting model, such as an image, embedded document, or applet."
    ///
    /// Replaced elements have intrinsic dimensions and aspect ratios that affect
    /// how they are sized during layout.
    fn create_replaced_box(&self, node: &DOMNode) -> Result<BoxNode, BoxGenerationError> {
        let replaced_type = node
            .replaced_type()
            .ok_or_else(|| BoxGenerationError::Unsupported(format!("Not a replaced element: {:?}", node.tag_name)))?;

        let intrinsic_size = node.intrinsic_size;
        let aspect_ratio = node.aspect_ratio();

        let box_node = BoxNode::new_replaced(node.style.clone(), replaced_type, intrinsic_size, aspect_ratio);

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

        // Note: Anonymous box fixup is now done as a post-processing step
        // after the entire tree is generated. See AnonymousBoxCreator::fixup_tree()

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

// =============================================================================
// Utility Methods for Box Tree Operations
// =============================================================================

impl BoxGenerator {
    /// Counts total boxes in a generated tree (including root)
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::tree::{BoxNode, FormattingContextType};
    /// use fastrender::tree::box_tree::ComputedStyle;
    /// use fastrender::tree::BoxGenerator;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let child = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    /// let parent = BoxNode::new_block(style, FormattingContextType::Block, vec![child]);
    ///
    /// let count = BoxGenerator::count_boxes(&parent);
    /// assert_eq!(count, 2); // parent + child
    /// ```
    pub fn count_boxes(box_node: &BoxNode) -> usize {
        1 + box_node
            .children
            .iter()
            .map(|child| Self::count_boxes(child))
            .sum::<usize>()
    }

    /// Finds all boxes matching a predicate
    ///
    /// Traverses the box tree and returns references to all boxes
    /// where the predicate returns true.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::tree::{BoxNode, BoxType, FormattingContextType};
    /// use fastrender::tree::box_tree::ComputedStyle;
    /// use fastrender::tree::BoxGenerator;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let text = BoxNode::new_text(style.clone(), "Hello".to_string());
    /// let inline = BoxNode::new_inline(style.clone(), vec![text]);
    /// let block = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![inline]);
    ///
    /// // Find all text boxes
    /// let text_boxes = BoxGenerator::find_boxes_by_predicate(&block, |b| b.is_text());
    /// assert_eq!(text_boxes.len(), 1);
    /// ```
    pub fn find_boxes_by_predicate<'a, F>(box_node: &'a BoxNode, predicate: F) -> Vec<&'a BoxNode>
    where
        F: Fn(&BoxNode) -> bool + Copy,
    {
        let mut result = Vec::new();

        if predicate(box_node) {
            result.push(box_node);
        }

        for child in &box_node.children {
            result.extend(Self::find_boxes_by_predicate(child, predicate));
        }

        result
    }

    /// Finds all boxes of a specific box type
    ///
    /// Convenience wrapper around find_boxes_by_predicate for type queries.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::tree::{BoxNode, FormattingContextType};
    /// use fastrender::tree::box_tree::ComputedStyle;
    /// use fastrender::tree::BoxGenerator;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let child1 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    /// let child2 = BoxNode::new_inline(style.clone(), vec![]);
    /// let parent = BoxNode::new_block(style, FormattingContextType::Block, vec![child1, child2]);
    ///
    /// // Find all block boxes
    /// let block_boxes = BoxGenerator::find_block_boxes(&parent);
    /// assert_eq!(block_boxes.len(), 2); // parent + child1
    /// ```
    pub fn find_block_boxes(box_node: &BoxNode) -> Vec<&BoxNode> {
        Self::find_boxes_by_predicate(box_node, |b| b.is_block_level())
    }

    /// Finds all inline boxes
    pub fn find_inline_boxes(box_node: &BoxNode) -> Vec<&BoxNode> {
        Self::find_boxes_by_predicate(box_node, |b| b.is_inline_level())
    }

    /// Finds all text boxes
    pub fn find_text_boxes(box_node: &BoxNode) -> Vec<&BoxNode> {
        Self::find_boxes_by_predicate(box_node, |b| b.is_text())
    }

    /// Finds all replaced boxes
    pub fn find_replaced_boxes(box_node: &BoxNode) -> Vec<&BoxNode> {
        Self::find_boxes_by_predicate(box_node, |b| b.is_replaced())
    }

    /// Validates box tree structure
    ///
    /// Checks for common structural errors:
    /// - Text boxes with children (invalid)
    /// - Replaced boxes with children (invalid)
    ///
    /// Returns Ok(()) if the tree is valid, Err with description if not.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::tree::{BoxNode, FormattingContextType};
    /// use fastrender::tree::box_tree::ComputedStyle;
    /// use fastrender::tree::BoxGenerator;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let valid_tree = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![
    ///     BoxNode::new_text(style.clone(), "Hello".to_string()),
    /// ]);
    ///
    /// assert!(BoxGenerator::validate_box_tree(&valid_tree).is_ok());
    /// ```
    pub fn validate_box_tree(box_node: &BoxNode) -> Result<(), BoxGenerationError> {
        // Text boxes cannot have children
        if box_node.is_text() && !box_node.children.is_empty() {
            return Err(BoxGenerationError::InvalidStructure(
                "Text box cannot have children".to_string(),
            ));
        }

        // Replaced boxes cannot have children
        if box_node.is_replaced() && !box_node.children.is_empty() {
            return Err(BoxGenerationError::InvalidStructure(
                "Replaced box cannot have children".to_string(),
            ));
        }

        // Recursively validate children
        for child in &box_node.children {
            Self::validate_box_tree(child)?;
        }

        Ok(())
    }

    /// Returns the depth of the box tree
    ///
    /// The depth is the maximum nesting level from root to any leaf.
    pub fn tree_depth(box_node: &BoxNode) -> usize {
        if box_node.children.is_empty() {
            1
        } else {
            1 + box_node.children.iter().map(Self::tree_depth).max().unwrap_or(0)
        }
    }
}

impl Default for BoxGenerator {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// StyledNode-based Box Generation (for real DOM/style pipeline)
// ============================================================================

use crate::style::cascade::StyledNode;
use crate::tree::box_tree::ReplacedBox;

/// Generates a BoxTree from a StyledNode tree
///
/// This is the main entry point for box generation from styled DOM.
/// It recursively converts each StyledNode into the appropriate BoxNode type.
///
/// # Arguments
///
/// * `styled` - The root of the styled node tree
///
/// # Returns
///
/// A `BoxTree` containing the generated box structure
pub fn generate_box_tree(styled: &StyledNode) -> BoxTree {
    let root = generate_box_node_from_styled(styled);
    BoxTree { root }
}

/// Recursively generates BoxNode from StyledNode
fn generate_box_node_from_styled(styled: &StyledNode) -> BoxNode {
    let style = Arc::new(styled.styles.clone());

    // Check if this is display: none
    if styled.styles.display == Display::None {
        // Create an empty block (will have no effect on layout)
        return BoxNode::new_block(style, FormattingContextType::Block, vec![]);
    }

    // Check for text node
    if let Some(text) = styled.node.text_content() {
        if !text.trim().is_empty() {
            return BoxNode::new_text(style, text.to_string());
        }
    }

    // Check for replaced elements (images, etc.)
    if let Some(tag) = styled.node.tag_name() {
        if is_replaced_element(tag) {
            return create_replaced_box_from_styled(styled, style);
        }
    }

    // Generate children
    let mut children: Vec<BoxNode> = styled.children.iter().map(generate_box_node_from_styled).collect();

    // Generate ::before pseudo-element box if styles exist
    if let Some(before_styles) = &styled.before_styles {
        if let Some(before_box) = create_pseudo_element_box(before_styles, "before") {
            children.insert(0, before_box);
        }
    }

    // Generate ::after pseudo-element box if styles exist
    if let Some(after_styles) = &styled.after_styles {
        if let Some(after_box) = create_pseudo_element_box(after_styles, "after") {
            children.push(after_box);
        }
    }

    // Determine box type based on display
    let fc_type = styled
        .styles
        .display
        .formatting_context_type()
        .unwrap_or(FormattingContextType::Block);

    match styled.styles.display {
        Display::Block | Display::Flex | Display::Grid | Display::Table => BoxNode::new_block(style, fc_type, children),
        Display::Inline => BoxNode::new_inline(style, children),
        Display::InlineBlock => BoxNode::new_inline_block(style, fc_type, children),
        Display::None => BoxNode::new_block(style, FormattingContextType::Block, vec![]),
        _ => BoxNode::new_block(style, fc_type, children),
    }
}

/// Creates a box for a pseudo-element (::before or ::after)
fn create_pseudo_element_box(styles: &ComputedStyle, pseudo_name: &str) -> Option<BoxNode> {
    // Get content value
    let content = &styles.content;

    // Skip if no content or content is "none" or "normal"
    if content.is_empty() || content == "none" || content == "normal" {
        return None;
    }

    let pseudo_style = Arc::new(styles.clone());

    // Create a text box with the content
    let text_box = BoxNode::new_text(pseudo_style.clone(), content.clone());

    // Determine the box type based on display property
    let fc_type = styles
        .display
        .formatting_context_type()
        .unwrap_or(FormattingContextType::Block);

    // Wrap in appropriate box type based on display
    let mut pseudo_box = match styles.display {
        Display::Block => BoxNode::new_block(pseudo_style.clone(), fc_type, vec![text_box]),
        Display::Inline | Display::None => BoxNode::new_inline(pseudo_style.clone(), vec![text_box]),
        Display::InlineBlock => BoxNode::new_inline_block(pseudo_style.clone(), fc_type, vec![text_box]),
        _ => BoxNode::new_inline(pseudo_style.clone(), vec![text_box]),
    };

    // Add debug info to mark this as a pseudo-element
    pseudo_box.debug_info = Some(DebugInfo {
        tag_name: Some(pseudo_name.to_string()),
        id: None,
        classes: vec!["pseudo-element".to_string()],
        dom_path: None,
    });

    Some(pseudo_box)
}

/// Checks if an element is a replaced element
///
/// Replaced elements are those whose content is replaced by an external resource,
/// such as images, videos, iframes, etc. These elements have intrinsic dimensions.
pub fn is_replaced_element(tag: &str) -> bool {
    matches!(
        tag.to_lowercase().as_str(),
        "img" | "video" | "canvas" | "svg" | "iframe" | "embed" | "object"
    )
}

/// Creates a BoxNode for a replaced element from a StyledNode
fn create_replaced_box_from_styled(styled: &StyledNode, style: Arc<crate::style::ComputedStyle>) -> BoxNode {
    let tag = styled.node.tag_name().unwrap_or("img");

    // Get src attribute if available
    let src = styled.node.get_attribute("src").unwrap_or_default();

    // Determine replaced type
    let replaced_type = match tag.to_lowercase().as_str() {
        "img" => ReplacedType::Image { src },
        "video" => ReplacedType::Video { src },
        "canvas" => ReplacedType::Canvas,
        "svg" => ReplacedType::Svg { content: String::new() },
        "iframe" => ReplacedType::Iframe { src },
        _ => ReplacedType::Image { src },
    };

    // Get intrinsic size from attributes or use default
    let intrinsic_width = styled
        .node
        .get_attribute("width")
        .and_then(|w| w.parse::<f32>().ok())
        .unwrap_or(300.0);

    let intrinsic_height = styled
        .node
        .get_attribute("height")
        .and_then(|h| h.parse::<f32>().ok())
        .unwrap_or(150.0);

    let replaced_box = ReplacedBox {
        replaced_type,
        intrinsic_size: Some(Size::new(intrinsic_width, intrinsic_height)),
        aspect_ratio: Some(intrinsic_width / intrinsic_height),
    };

    BoxNode {
        box_type: BoxType::Replaced(replaced_box),
        style,
        children: vec![],
        debug_info: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::Size;
    use crate::tree::ReplacedType;

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

    // =============================================================================
    // Replaced Element Tests
    // =============================================================================

    #[test]
    fn test_dom_node_is_replaced_element() {
        let style = default_style();

        // Test replaced elements
        assert!(DOMNode::new_element("img", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("video", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("canvas", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("svg", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("iframe", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("embed", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("object", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("audio", style.clone(), vec![]).is_replaced_element());

        // Test non-replaced elements
        assert!(!DOMNode::new_element("div", style.clone(), vec![]).is_replaced_element());
        assert!(!DOMNode::new_element("span", style.clone(), vec![]).is_replaced_element());
        assert!(!DOMNode::new_element("p", style.clone(), vec![]).is_replaced_element());
        assert!(!DOMNode::new_text("text", style.clone()).is_replaced_element());
    }

    #[test]
    fn test_new_replaced_constructor() {
        let style = default_style();
        let size = Size::new(100.0, 50.0);

        let img = DOMNode::new_replaced("img", style.clone(), "image.png", Some(size));

        assert_eq!(img.tag_name.as_deref(), Some("img"));
        assert_eq!(img.src.as_deref(), Some("image.png"));
        assert_eq!(img.intrinsic_size, Some(size));
        assert!(img.is_replaced_element());
    }

    #[test]
    fn test_replaced_element_aspect_ratio() {
        let style = default_style();
        let size = Size::new(100.0, 50.0);

        let img = DOMNode::new_replaced("img", style.clone(), "test.png", Some(size));
        assert_eq!(img.aspect_ratio(), Some(2.0)); // 100/50 = 2

        // Test with no intrinsic size
        let img_no_size = DOMNode::new_replaced("img", style.clone(), "test.png", None);
        assert_eq!(img_no_size.aspect_ratio(), None);

        // Test with zero height
        let zero_height = DOMNode::new_replaced("img", style, "test.png", Some(Size::new(100.0, 0.0)));
        assert_eq!(zero_height.aspect_ratio(), None);
    }

    #[test]
    fn test_replaced_element_type_detection() {
        let style = default_style();

        let img = DOMNode::new_replaced("img", style.clone(), "test.png", None);
        assert!(matches!(
            img.replaced_type(),
            Some(ReplacedType::Image { .. })
        ));

        let video = DOMNode::new_replaced("video", style.clone(), "test.mp4", None);
        assert!(matches!(
            video.replaced_type(),
            Some(ReplacedType::Video { .. })
        ));

        let canvas = DOMNode::new_element("canvas", style.clone(), vec![]);
        assert!(matches!(
            canvas.replaced_type(),
            Some(ReplacedType::Canvas)
        ));

        let div = DOMNode::new_element("div", style, vec![]);
        assert!(div.replaced_type().is_none());
    }

    #[test]
    fn test_generate_replaced_box() {
        let generator = BoxGenerator::new();
        let style = default_style();
        let size = Size::new(200.0, 100.0);

        let img = DOMNode::new_replaced("img", style.clone(), "test.png", Some(size));
        let wrapper = DOMNode::new_element("div", style_with_display(Display::Block), vec![img]);

        let box_tree = generator.generate(&wrapper).unwrap();

        // Root (div) + img
        assert_eq!(box_tree.count_boxes(), 2);

        // Check that the img is a replaced box
        let img_box = &box_tree.root.children[0];
        assert!(img_box.is_replaced());
    }

    #[test]
    fn test_generate_multiple_replaced_elements() {
        let generator = BoxGenerator::new();
        let style = default_style();

        let img1 = DOMNode::new_replaced(
            "img",
            style.clone(),
            "img1.png",
            Some(Size::new(100.0, 100.0)),
        );
        let video = DOMNode::new_replaced(
            "video",
            style.clone(),
            "video.mp4",
            Some(Size::new(640.0, 480.0)),
        );
        let img2 = DOMNode::new_replaced("img", style.clone(), "img2.png", None);

        let container = DOMNode::new_element("div", style_with_display(Display::Block), vec![img1, video, img2]);

        let box_tree = generator.generate(&container).unwrap();

        assert_eq!(box_tree.count_boxes(), 4); // container + 3 replaced
        assert_eq!(BoxGenerator::find_replaced_boxes(&box_tree.root).len(), 3);
    }

    #[test]
    fn test_replaced_element_with_display_none_child() {
        let generator = BoxGenerator::new();
        let style = default_style();

        // Even though img has children (which shouldn't happen in real HTML),
        // the generator should handle replaced elements without processing children
        let img = DOMNode::new_replaced("img", style.clone(), "test.png", None);
        let container = DOMNode::new_element("div", style_with_display(Display::Block), vec![img]);

        let box_tree = generator.generate(&container).unwrap();

        assert_eq!(box_tree.count_boxes(), 2);
        let replaced_boxes = BoxGenerator::find_replaced_boxes(&box_tree.root);
        assert_eq!(replaced_boxes.len(), 1);
        assert!(replaced_boxes[0].children.is_empty());
    }

    #[test]
    fn test_replaced_element_inline_context() {
        let generator = BoxGenerator::new();
        let style = default_style();

        // img in inline context
        let text = DOMNode::new_text("Hello ", style.clone());
        let img = DOMNode::new_replaced(
            "img",
            style.clone(),
            "icon.png",
            Some(Size::new(16.0, 16.0)),
        );
        let text2 = DOMNode::new_text(" World", style.clone());

        let span = DOMNode::new_element("span", style_with_display(Display::Inline), vec![text, img, text2]);
        let p = DOMNode::new_element("p", style_with_display(Display::Block), vec![span]);

        let box_tree = generator.generate(&p).unwrap();

        // p + span + 2 text + 1 img = 5
        assert_eq!(box_tree.count_boxes(), 5);
        assert_eq!(BoxGenerator::find_replaced_boxes(&box_tree.root).len(), 1);
        assert_eq!(BoxGenerator::find_text_boxes(&box_tree.root).len(), 2);
    }

    // =============================================================================
    // Utility Method Tests
    // =============================================================================

    #[test]
    fn test_count_boxes_utility() {
        let style = default_style();

        // Single box
        let single = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
        assert_eq!(BoxGenerator::count_boxes(&single), 1);

        // Parent with 3 children
        let children = vec![
            BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]),
            BoxNode::new_inline(style.clone(), vec![]),
            BoxNode::new_text(style.clone(), "text".to_string()),
        ];
        let parent = BoxNode::new_block(style, FormattingContextType::Block, children);
        assert_eq!(BoxGenerator::count_boxes(&parent), 4);
    }

    #[test]
    fn test_find_boxes_by_predicate() {
        let style = default_style();

        let text1 = BoxNode::new_text(style.clone(), "Hello".to_string());
        let text2 = BoxNode::new_text(style.clone(), "World".to_string());
        let inline = BoxNode::new_inline(style.clone(), vec![text1]);
        let block = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![inline, text2]);

        // Find all text boxes
        let text_boxes = BoxGenerator::find_boxes_by_predicate(&block, |b| b.is_text());
        assert_eq!(text_boxes.len(), 2);

        // Find inline boxes (inline + 2 text boxes since text is inline-level)
        let inline_boxes = BoxGenerator::find_boxes_by_predicate(&block, |b| b.is_inline_level());
        assert_eq!(inline_boxes.len(), 3); // 1 inline + 2 text boxes
    }

    #[test]
    fn test_find_block_boxes() {
        let style = default_style();

        let text = BoxNode::new_text(style.clone(), "text".to_string());
        let inline = BoxNode::new_inline(style.clone(), vec![]);
        let block1 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
        let block2 = BoxNode::new_block(style.clone(), FormattingContextType::Flex, vec![]);
        let root = BoxNode::new_block(style, FormattingContextType::Block, vec![text, inline, block1, block2]);

        let blocks = BoxGenerator::find_block_boxes(&root);
        assert_eq!(blocks.len(), 3); // root + block1 + block2
    }

    #[test]
    fn test_find_inline_boxes() {
        let style = default_style();

        let text = BoxNode::new_text(style.clone(), "text".to_string());
        let inline1 = BoxNode::new_inline(style.clone(), vec![text]);
        let inline2 = BoxNode::new_inline(style.clone(), vec![]);
        let root = BoxNode::new_block(style, FormattingContextType::Block, vec![inline1, inline2]);

        // inline1 + inline2 + text (text is also inline-level)
        let inlines = BoxGenerator::find_inline_boxes(&root);
        assert_eq!(inlines.len(), 3);
    }

    #[test]
    fn test_find_text_boxes() {
        let style = default_style();

        let text1 = BoxNode::new_text(style.clone(), "Hello".to_string());
        let text2 = BoxNode::new_text(style.clone(), "World".to_string());
        let inline = BoxNode::new_inline(style.clone(), vec![text1]);
        let root = BoxNode::new_block(style, FormattingContextType::Block, vec![inline, text2]);

        let texts = BoxGenerator::find_text_boxes(&root);
        assert_eq!(texts.len(), 2);
    }

    #[test]
    fn test_find_replaced_boxes_utility() {
        let style = default_style();

        let img = BoxNode::new_replaced(
            style.clone(),
            ReplacedType::Image {
                src: "test.png".to_string(),
            },
            Some(Size::new(100.0, 100.0)),
            Some(1.0),
        );
        let video = BoxNode::new_replaced(
            style.clone(),
            ReplacedType::Video {
                src: "test.mp4".to_string(),
            },
            None,
            None,
        );
        let text = BoxNode::new_text(style.clone(), "text".to_string());
        let root = BoxNode::new_block(style, FormattingContextType::Block, vec![img, video, text]);

        let replaced = BoxGenerator::find_replaced_boxes(&root);
        assert_eq!(replaced.len(), 2);
    }

    #[test]
    fn test_validate_box_tree_valid() {
        let style = default_style();

        let text = BoxNode::new_text(style.clone(), "Hello".to_string());
        let inline = BoxNode::new_inline(style.clone(), vec![text]);
        let root = BoxNode::new_block(style, FormattingContextType::Block, vec![inline]);

        assert!(BoxGenerator::validate_box_tree(&root).is_ok());
    }

    #[test]
    fn test_tree_depth() {
        let style = default_style();

        // Single node = depth 1
        let single = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
        assert_eq!(BoxGenerator::tree_depth(&single), 1);

        // 2 levels
        let child = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
        let root = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![child]);
        assert_eq!(BoxGenerator::tree_depth(&root), 2);

        // 4 levels deep
        let leaf = BoxNode::new_text(style.clone(), "text".to_string());
        let level3 = BoxNode::new_inline(style.clone(), vec![leaf]);
        let level2 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![level3]);
        let level1 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![level2]);
        assert_eq!(BoxGenerator::tree_depth(&level1), 4);
    }

    #[test]
    fn test_with_builder_methods() {
        let style = default_style();
        let size = Size::new(50.0, 25.0);

        let node = DOMNode::new_element("img", style, vec![])
            .with_id("my-image")
            .with_class("thumbnail")
            .with_class("responsive")
            .with_src("path/to/image.png")
            .with_intrinsic_size(size);

        assert_eq!(node.id.as_deref(), Some("my-image"));
        assert_eq!(node.classes.len(), 2);
        assert_eq!(node.src.as_deref(), Some("path/to/image.png"));
        assert_eq!(node.intrinsic_size, Some(size));
    }

    // =============================================================================
    // Edge Case Tests
    // =============================================================================

    #[test]
    fn test_empty_replaced_src() {
        let style = default_style();

        let img = DOMNode::new_replaced("img", style.clone(), "", None);
        let wrapper = DOMNode::new_element("div", style_with_display(Display::Block), vec![img]);

        let generator = BoxGenerator::new();
        let box_tree = generator.generate(&wrapper).unwrap();

        assert_eq!(box_tree.count_boxes(), 2);
    }

    #[test]
    fn test_mixed_content_with_replaced() {
        let generator = BoxGenerator::new();
        let style = default_style();

        // Create: div > text + img + text + video + div
        let text1 = DOMNode::new_text("Before image ", style.clone());
        let img = DOMNode::new_replaced(
            "img",
            style.clone(),
            "test.png",
            Some(crate::geometry::Size::new(100.0, 50.0)),
        );
        let text2 = DOMNode::new_text(" after image ", style.clone());
        let video = DOMNode::new_replaced("video", style.clone(), "test.mp4", None);
        let nested_div = DOMNode::new_element(
            "div",
            style_with_display(Display::Block),
            vec![DOMNode::new_text("Nested", style.clone())],
        );

        let root = DOMNode::new_element(
            "div",
            style_with_display(Display::Block),
            vec![text1, img, text2, video, nested_div],
        );

        let box_tree = generator.generate(&root).unwrap();

        // root + 2 text + img + video + nested_div + nested_text = 7
        assert_eq!(box_tree.count_boxes(), 7);
        assert_eq!(BoxGenerator::find_replaced_boxes(&box_tree.root).len(), 2);
        assert_eq!(BoxGenerator::find_text_boxes(&box_tree.root).len(), 3);
    }

    #[test]
    fn test_svg_replaced_element() {
        let generator = BoxGenerator::new();
        let style = default_style();

        let svg = DOMNode::new_replaced(
            "svg",
            style.clone(),
            "<svg>...</svg>",
            Some(crate::geometry::Size::new(100.0, 100.0)),
        );
        let wrapper = DOMNode::new_element("div", style_with_display(Display::Block), vec![svg]);

        let box_tree = generator.generate(&wrapper).unwrap();

        assert_eq!(BoxGenerator::find_replaced_boxes(&box_tree.root).len(), 1);
    }

    #[test]
    fn test_iframe_replaced_element() {
        let generator = BoxGenerator::new();
        let style = default_style();

        let iframe = DOMNode::new_replaced(
            "iframe",
            style.clone(),
            "https://example.com",
            Some(crate::geometry::Size::new(300.0, 200.0)),
        );
        let wrapper = DOMNode::new_element("div", style_with_display(Display::Block), vec![iframe]);

        let box_tree = generator.generate(&wrapper).unwrap();

        let replaced = BoxGenerator::find_replaced_boxes(&box_tree.root);
        assert_eq!(replaced.len(), 1);
    }
}
