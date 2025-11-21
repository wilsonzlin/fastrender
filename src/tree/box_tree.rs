//! Box Tree - Represents CSS boxes before layout
//!
//! The box tree is generated from the styled DOM tree and represents
//! the CSS box model. It's independent of layout - it only represents
//! what boxes exist and their styling, not where they're positioned.
//!
//! # Separation of Concerns
//!
//! **Box Tree (this module)**:
//! - Immutable
//! - No positions or final sizes
//! - Represents "what to layout"
//! - Generated once from DOM
//!
//! **Fragment Tree**:
//! - Result of layout
//! - Has positions and sizes
//! - Represents "what was laid out"
//! - Generated per layout pass
//!
//! Reference: CSS Display Module Level 3
//! https://www.w3.org/TR/css-display-3/

use std::fmt;
use std::sync::Arc;

// Import types from dependencies
// NOTE: ComputedStyle will be defined in W2.T05, so we'll use a placeholder
// In a real implementation, this would come from crate::style::ComputedStyle

// Import DebugInfo from the debug module
pub use super::debug::DebugInfo;

/// Types of formatting contexts
///
/// A formatting context is an environment in which boxes are laid out.
/// Different formatting contexts have different layout rules.
///
/// Reference: CSS 2.1 Section 9.4
///
/// # Examples
///
/// ```
/// use fastrender::tree::FormattingContextType;
///
/// let fc_type = FormattingContextType::Block;
/// assert!(matches!(fc_type, FormattingContextType::Block));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormattingContextType {
    /// Block Formatting Context (BFC)
    ///
    /// Block-level boxes are laid out vertically, one after another.
    /// Margins collapse. This is the default for most block containers.
    ///
    /// Reference: CSS 2.1 Section 9.4.1
    Block,

    /// Inline Formatting Context (IFC)
    ///
    /// Inline-level boxes are laid out horizontally within lines.
    /// Text wraps at line boundaries.
    ///
    /// Reference: CSS 2.1 Section 9.4.2
    Inline,

    /// Flex Formatting Context
    ///
    /// Children are laid out using the flexbox algorithm.
    ///
    /// Reference: CSS Flexbox Module Level 1
    Flex,

    /// Grid Formatting Context
    ///
    /// Children are positioned in a 2D grid.
    ///
    /// Reference: CSS Grid Layout Module Level 1
    Grid,

    /// Table Formatting Context
    ///
    /// Children are laid out as table rows/columns.
    ///
    /// Reference: CSS 2.1 Section 17
    Table,
}

impl fmt::Display for FormattingContextType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Block => write!(f, "BFC"),
            Self::Inline => write!(f, "IFC"),
            Self::Flex => write!(f, "Flex"),
            Self::Grid => write!(f, "Grid"),
            Self::Table => write!(f, "Table"),
        }
    }
}

/// A block-level box
///
/// Block boxes stack vertically and establish block formatting contexts.
/// Examples: div, p, h1, section
///
/// Reference: CSS 2.1 Section 9.2.1
#[derive(Debug, Clone)]
pub struct BlockBox {
    /// What formatting context does this establish?
    pub formatting_context: FormattingContextType,
}

/// An inline-level box
///
/// Inline boxes flow horizontally within lines.
/// Examples: span, a, em, strong
///
/// Reference: CSS 2.1 Section 9.2.2
#[derive(Debug, Clone)]
pub struct InlineBox {
    /// For inline-block, this establishes a formatting context
    /// For regular inline, this is None
    pub formatting_context: Option<FormattingContextType>,
}

/// A text box containing actual text content
///
/// Text boxes are always inline-level and contain strings to be shaped.
///
/// # Note
///
/// The text stored here is the raw text. It will be shaped (with font,
/// bidi, script analysis) during inline layout.
#[derive(Debug, Clone)]
pub struct TextBox {
    /// The text content
    ///
    /// This is UTF-8 text that may contain multiple scripts, emojis, etc.
    pub text: String,
}

/// A replaced element box
///
/// Replaced elements have intrinsic dimensions provided by external content.
/// Examples: img, canvas, video, iframe
///
/// Reference: CSS 2.1 Section 10.3.2
#[derive(Debug, Clone)]
pub struct ReplacedBox {
    /// Type of replaced element
    pub replaced_type: ReplacedType,

    /// Intrinsic size (if known)
    ///
    /// Some replaced elements have intrinsic dimensions (images with width/height),
    /// others don't (iframes without size attributes).
    pub intrinsic_size: Option<crate::geometry::Size>,

    /// Intrinsic aspect ratio (width / height)
    ///
    /// Used for sizing when only one dimension is specified.
    pub aspect_ratio: Option<f32>,
}

/// Types of replaced elements
#[derive(Debug, Clone)]
pub enum ReplacedType {
    /// Image element
    Image {
        /// Source URL or data URI
        src: String,
    },

    /// Video element
    Video {
        /// Source URL
        src: String,
    },

    /// Canvas element
    Canvas,

    /// SVG embedded content
    Svg {
        /// SVG content (inline or reference)
        content: String,
    },

    /// Iframe (nested browsing context)
    Iframe {
        /// Source URL
        src: String,
    },
}

/// An anonymous box generated by the layout algorithm
///
/// Anonymous boxes don't correspond to DOM elements. They're inserted
/// to satisfy CSS layout rules.
///
/// Example: When a block container has both block and inline children,
/// anonymous block boxes wrap the inline children.
///
/// Reference: CSS 2.1 Section 9.2.1.1 (Anonymous block boxes)
#[derive(Debug, Clone)]
pub struct AnonymousBox {
    /// What kind of anonymous box?
    pub anonymous_type: AnonymousType,
}

/// Types of anonymous boxes
#[derive(Debug, Clone, Copy)]
pub enum AnonymousType {
    /// Anonymous block box
    ///
    /// Generated when a block container has mixed inline/block children.
    Block,

    /// Anonymous inline box
    ///
    /// Generated to wrap text nodes that aren't in explicit inline elements.
    Inline,

    /// Anonymous table wrapper box
    ///
    /// Generated around tables to contain captions.
    TableWrapper,

    /// Anonymous table row box
    ///
    /// Generated when table cells aren't in explicit rows.
    TableRow,

    /// Anonymous table cell box
    ///
    /// Generated when content isn't in explicit cells.
    TableCell,
}

/// Different types of boxes in the box tree
///
/// This enum discriminates between the different kinds of CSS boxes.
/// Each variant contains type-specific data.
#[derive(Debug, Clone)]
pub enum BoxType {
    /// Block-level box (div, p, h1, etc.)
    Block(BlockBox),

    /// Inline-level box (span, a, em, etc.)
    Inline(InlineBox),

    /// Text box (actual text content)
    Text(TextBox),

    /// Replaced element (img, video, canvas, etc.)
    Replaced(ReplacedBox),

    /// Anonymous box (generated by layout algorithm)
    Anonymous(AnonymousBox),
}

// Placeholder for ComputedStyle (will be implemented in W2.T05)
// For now, use a simple struct
#[derive(Debug, Clone, Default)]
pub struct ComputedStyle {
    // Placeholder
    _placeholder: (),
}

/// A single box in the box tree
///
/// Represents a CSS box (could be element, text, anonymous, etc.)
///
/// # Important Properties
///
/// - **Immutable**: Once created, a BoxNode doesn't change
/// - **No Position**: Boxes don't know where they'll be positioned (that's fragments)
/// - **Shared Styles**: ComputedStyle is Arc-ed and shared with fragments
/// - **Recursive**: Children form a tree
///
/// # Examples
///
/// ```
/// use std::sync::Arc;
/// use fastrender::tree::{BoxNode, FormattingContextType};
/// # use fastrender::tree::box_tree::ComputedStyle;
///
/// let style = Arc::new(ComputedStyle::default());
/// let box_node = BoxNode::new_block(
///     style,
///     FormattingContextType::Block,
///     vec![],
/// );
///
/// assert!(box_node.is_block_level());
/// ```
#[derive(Debug, Clone)]
pub struct BoxNode {
    /// Computed style for this box (shared with fragments)
    ///
    /// Using Arc because:
    /// - Shared between box and its fragments
    /// - Immutable after computation
    /// - Reduces memory usage for cloned trees
    pub style: Arc<ComputedStyle>,

    /// What kind of box is this?
    pub box_type: BoxType,

    /// Child boxes in document order
    pub children: Vec<BoxNode>,

    /// Debug information (element name, class, id)
    ///
    /// Optional - only populated in debug builds or with dev tools enabled
    pub debug_info: Option<DebugInfo>,
}

impl BoxNode {
    /// Creates a new block box
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::tree::{BoxNode, FormattingContextType};
    /// # use fastrender::tree::box_tree::ComputedStyle;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let box_node = BoxNode::new_block(
    ///     style,
    ///     FormattingContextType::Block,
    ///     vec![],
    /// );
    ///
    /// assert!(box_node.is_block_level());
    /// ```
    pub fn new_block(style: Arc<ComputedStyle>, fc: FormattingContextType, children: Vec<BoxNode>) -> Self {
        Self {
            style,
            box_type: BoxType::Block(BlockBox { formatting_context: fc }),
            children,
            debug_info: None,
        }
    }

    /// Creates a new inline box
    pub fn new_inline(style: Arc<ComputedStyle>, children: Vec<BoxNode>) -> Self {
        Self {
            style,
            box_type: BoxType::Inline(InlineBox {
                formatting_context: None,
            }),
            children,
            debug_info: None,
        }
    }

    /// Creates a new inline-block box
    pub fn new_inline_block(style: Arc<ComputedStyle>, fc: FormattingContextType, children: Vec<BoxNode>) -> Self {
        Self {
            style,
            box_type: BoxType::Inline(InlineBox {
                formatting_context: Some(fc),
            }),
            children,
            debug_info: None,
        }
    }

    /// Creates a new text box
    pub fn new_text(style: Arc<ComputedStyle>, text: String) -> Self {
        Self {
            style,
            box_type: BoxType::Text(TextBox { text }),
            children: Vec::new(),
            debug_info: None,
        }
    }

    /// Creates a new replaced box
    pub fn new_replaced(
        style: Arc<ComputedStyle>,
        replaced_type: ReplacedType,
        intrinsic_size: Option<crate::geometry::Size>,
        aspect_ratio: Option<f32>,
    ) -> Self {
        Self {
            style,
            box_type: BoxType::Replaced(ReplacedBox {
                replaced_type,
                intrinsic_size,
                aspect_ratio,
            }),
            children: Vec::new(),
            debug_info: None,
        }
    }

    /// Creates an anonymous block box
    pub fn new_anonymous_block(style: Arc<ComputedStyle>, children: Vec<BoxNode>) -> Self {
        Self {
            style,
            box_type: BoxType::Anonymous(AnonymousBox {
                anonymous_type: AnonymousType::Block,
            }),
            children,
            debug_info: None,
        }
    }

    /// Adds debug information
    ///
    /// This is a builder-style method for convenience.
    pub fn with_debug_info(mut self, info: DebugInfo) -> Self {
        self.debug_info = Some(info);
        self
    }

    // Type query methods

    /// Returns true if this is a block-level box
    ///
    /// Block-level boxes participate in block formatting context.
    pub fn is_block_level(&self) -> bool {
        match &self.box_type {
            BoxType::Block(_) | BoxType::Replaced(_) => true,
            BoxType::Anonymous(anon) => matches!(
                anon.anonymous_type,
                AnonymousType::Block | AnonymousType::TableWrapper | AnonymousType::TableRow | AnonymousType::TableCell
            ),
            _ => false,
        }
    }

    /// Returns true if this is an inline-level box
    ///
    /// Inline-level boxes participate in inline formatting context.
    pub fn is_inline_level(&self) -> bool {
        match &self.box_type {
            BoxType::Inline(_) | BoxType::Text(_) => true,
            BoxType::Anonymous(anon) => matches!(anon.anonymous_type, AnonymousType::Inline),
            _ => false,
        }
    }

    /// Returns true if this is a text box
    pub fn is_text(&self) -> bool {
        matches!(&self.box_type, BoxType::Text(_))
    }

    /// Returns true if this is a replaced element
    pub fn is_replaced(&self) -> bool {
        matches!(&self.box_type, BoxType::Replaced(_))
    }

    /// Returns true if this is an anonymous box
    pub fn is_anonymous(&self) -> bool {
        matches!(&self.box_type, BoxType::Anonymous(_))
    }

    /// Gets the formatting context this box establishes (if any)
    ///
    /// Returns None for inline and text boxes that don't establish contexts.
    pub fn formatting_context(&self) -> Option<FormattingContextType> {
        match &self.box_type {
            BoxType::Block(block) => Some(block.formatting_context),
            BoxType::Inline(inline) => inline.formatting_context,
            BoxType::Replaced(_) => Some(FormattingContextType::Block),
            _ => None,
        }
    }

    /// Returns true if this box is a block container
    ///
    /// Block containers can contain block-level children and establish
    /// a block formatting context (or participate in one).
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::tree::{BoxNode, FormattingContextType};
    /// # use fastrender::tree::box_tree::ComputedStyle;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let block = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    /// let inline_block = BoxNode::new_inline_block(style, FormattingContextType::Block, vec![]);
    ///
    /// assert!(block.is_block_container());
    /// assert!(inline_block.is_block_container());
    /// ```
    pub fn is_block_container(&self) -> bool {
        match &self.box_type {
            BoxType::Block(_) => true,
            BoxType::Inline(inline) => inline.formatting_context.is_some(), // inline-block
            BoxType::Anonymous(anon) => matches!(anon.anonymous_type, AnonymousType::Block | AnonymousType::TableCell),
            _ => false,
        }
    }

    /// Returns true if this box is an inline container
    ///
    /// Inline containers contain inline-level children and participate
    /// in inline formatting context.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::tree::BoxNode;
    /// # use fastrender::tree::box_tree::ComputedStyle;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let inline = BoxNode::new_inline(style, vec![]);
    ///
    /// assert!(inline.is_inline_container());
    /// ```
    pub fn is_inline_container(&self) -> bool {
        matches!(&self.box_type, BoxType::Inline(_))
    }

    /// Returns true if this box generates a formatting context
    ///
    /// Boxes that generate formatting contexts are independent layout roots.
    /// Their internal layout doesn't affect outside, and vice versa.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::tree::{BoxNode, FormattingContextType};
    /// # use fastrender::tree::box_tree::ComputedStyle;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let block = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    /// let inline = BoxNode::new_inline(style, vec![]);
    ///
    /// assert!(block.generates_formatting_context());
    /// assert!(!inline.generates_formatting_context());
    /// ```
    pub fn generates_formatting_context(&self) -> bool {
        self.formatting_context().is_some()
    }

    /// Returns true if this is a table-internal box
    ///
    /// Table-internal boxes participate in table layout algorithms.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::tree::{BoxNode, AnonymousType};
    /// # use fastrender::tree::box_tree::{ComputedStyle, AnonymousBox};
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// // Table-internal boxes are typically anonymous boxes in the table structure
    /// // This is a simplified example
    /// ```
    pub fn is_table_internal(&self) -> bool {
        match &self.box_type {
            BoxType::Anonymous(anon) => matches!(
                anon.anonymous_type,
                AnonymousType::TableWrapper | AnonymousType::TableRow | AnonymousType::TableCell
            ),
            _ => false,
        }
    }

    /// Gets text content if this is a text box
    pub fn text(&self) -> Option<&str> {
        match &self.box_type {
            BoxType::Text(text_box) => Some(&text_box.text),
            _ => None,
        }
    }

    /// Returns the number of children
    pub fn child_count(&self) -> usize {
        self.children.len()
    }

    /// Returns an iterator over children
    pub fn children_iter(&self) -> impl Iterator<Item = &BoxNode> {
        self.children.iter()
    }
}

/// A tree of CSS boxes
///
/// The box tree is generated from styled DOM and consumed by layout algorithms.
/// It's immutable after construction.
///
/// # Examples
///
/// ```
/// use std::sync::Arc;
/// use fastrender::tree::{BoxTree, BoxNode, FormattingContextType};
/// # use fastrender::tree::box_tree::ComputedStyle;
///
/// let style = Arc::new(ComputedStyle::default());
/// let root = BoxNode::new_block(
///     style,
///     FormattingContextType::Block,
///     vec![],
/// );
///
/// let tree = BoxTree::new(root);
/// assert!(tree.root.is_block_level());
/// ```
#[derive(Debug, Clone)]
pub struct BoxTree {
    /// The root box (typically the root element's principal box)
    pub root: BoxNode,
}

impl BoxTree {
    /// Creates a new box tree with the given root
    pub fn new(root: BoxNode) -> Self {
        Self { root }
    }

    /// Counts total boxes in the tree (including root)
    pub fn count_boxes(&self) -> usize {
        fn count_recursive(node: &BoxNode) -> usize {
            1 + node.children.iter().map(count_recursive).sum::<usize>()
        }
        count_recursive(&self.root)
    }

    /// Counts text boxes in the tree
    pub fn count_text_boxes(&self) -> usize {
        fn count_recursive(node: &BoxNode) -> usize {
            let self_count = usize::from(node.is_text());
            self_count + node.children.iter().map(count_recursive).sum::<usize>()
        }
        count_recursive(&self.root)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn default_style() -> Arc<ComputedStyle> {
        Arc::new(ComputedStyle::default())
    }

    #[test]
    fn test_create_block_box() {
        let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

        assert!(box_node.is_block_level());
        assert!(!box_node.is_inline_level());
        assert_eq!(box_node.formatting_context(), Some(FormattingContextType::Block));
    }

    #[test]
    fn test_create_inline_box() {
        let box_node = BoxNode::new_inline(default_style(), vec![]);

        assert!(box_node.is_inline_level());
        assert!(!box_node.is_block_level());
        assert_eq!(box_node.formatting_context(), None);
    }

    #[test]
    fn test_create_text_box() {
        let box_node = BoxNode::new_text(default_style(), "Hello".to_string());

        assert!(box_node.is_inline_level());
        assert!(box_node.is_text());
        assert_eq!(box_node.text(), Some("Hello"));
        assert_eq!(box_node.children.len(), 0);
    }

    #[test]
    fn test_create_replaced_box() {
        let box_node = BoxNode::new_replaced(
            default_style(),
            ReplacedType::Image {
                src: "image.png".to_string(),
            },
            Some(crate::geometry::Size::new(100.0, 50.0)),
            Some(2.0),
        );

        assert!(box_node.is_replaced());
        assert!(box_node.is_block_level());
    }

    #[test]
    fn test_create_inline_block() {
        let box_node = BoxNode::new_inline_block(default_style(), FormattingContextType::Block, vec![]);

        assert!(box_node.is_inline_level());
        assert_eq!(box_node.formatting_context(), Some(FormattingContextType::Block));
    }

    #[test]
    fn test_box_hierarchy() {
        let text1 = BoxNode::new_text(default_style(), "Text 1".to_string());
        let text2 = BoxNode::new_text(default_style(), "Text 2".to_string());

        let inline_box = BoxNode::new_inline(default_style(), vec![text1, text2]);

        let block_box = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![inline_box]);

        assert_eq!(block_box.children.len(), 1);
        assert_eq!(block_box.children[0].children.len(), 2);
        assert_eq!(block_box.child_count(), 1);
    }

    #[test]
    fn test_debug_info() {
        let debug_info = DebugInfo::new(
            Some("div".to_string()),
            Some("header".to_string()),
            vec!["navbar".to_string(), "sticky".to_string()],
        );

        assert_eq!(debug_info.to_selector(), "div#header.navbar.sticky");

        let box_node =
            BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]).with_debug_info(debug_info);

        assert!(box_node.debug_info.is_some());
    }

    #[test]
    fn test_box_tree() {
        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![
                BoxNode::new_text(default_style(), "Text".to_string()),
                BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]),
            ],
        );

        let tree = BoxTree::new(root);

        assert_eq!(tree.count_boxes(), 3); // root + text + block
        assert_eq!(tree.count_text_boxes(), 1);
    }

    #[test]
    fn test_formatting_context_types() {
        assert_eq!(format!("{}", FormattingContextType::Block), "BFC");
        assert_eq!(format!("{}", FormattingContextType::Inline), "IFC");
        assert_eq!(format!("{}", FormattingContextType::Flex), "Flex");
        assert_eq!(format!("{}", FormattingContextType::Grid), "Grid");
        assert_eq!(format!("{}", FormattingContextType::Table), "Table");
    }

    #[test]
    fn test_anonymous_block_box() {
        let box_node = BoxNode::new_anonymous_block(default_style(), vec![]);

        assert!(box_node.is_anonymous());
        assert!(box_node.is_block_level());
    }

    #[test]
    fn test_children_iterator() {
        let text1 = BoxNode::new_text(default_style(), "Text 1".to_string());
        let text2 = BoxNode::new_text(default_style(), "Text 2".to_string());
        let box_node = BoxNode::new_inline(default_style(), vec![text1, text2]);

        let count = box_node.children_iter().count();
        assert_eq!(count, 2);
    }

    // W2.T02 categorization method tests

    #[test]
    fn test_is_block_container() {
        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let inline_block = BoxNode::new_inline_block(default_style(), FormattingContextType::Block, vec![]);
        let inline = BoxNode::new_inline(default_style(), vec![]);
        let text = BoxNode::new_text(default_style(), "text".to_string());

        assert!(block.is_block_container());
        assert!(inline_block.is_block_container());
        assert!(!inline.is_block_container());
        assert!(!text.is_block_container());
    }

    #[test]
    fn test_is_inline_container() {
        let inline = BoxNode::new_inline(default_style(), vec![]);
        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let text = BoxNode::new_text(default_style(), "text".to_string());

        assert!(inline.is_inline_container());
        assert!(!block.is_inline_container());
        assert!(!text.is_inline_container());
    }

    #[test]
    fn test_generates_formatting_context() {
        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let inline_block = BoxNode::new_inline_block(default_style(), FormattingContextType::Flex, vec![]);
        let inline = BoxNode::new_inline(default_style(), vec![]);
        let text = BoxNode::new_text(default_style(), "text".to_string());
        let replaced = BoxNode::new_replaced(
            default_style(),
            ReplacedType::Image {
                src: "img.png".to_string(),
            },
            None,
            None,
        );

        assert!(block.generates_formatting_context());
        assert!(inline_block.generates_formatting_context());
        assert!(replaced.generates_formatting_context());
        assert!(!inline.generates_formatting_context());
        assert!(!text.generates_formatting_context());
    }

    #[test]
    fn test_is_table_internal() {
        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let anon_block = BoxNode::new_anonymous_block(default_style(), vec![]);

        assert!(!block.is_table_internal());
        assert!(!anon_block.is_table_internal());
        // Table-internal boxes would be tested when table layout is implemented
    }

    #[test]
    fn test_block_container_with_flex() {
        let flex_block = BoxNode::new_block(default_style(), FormattingContextType::Flex, vec![]);

        assert!(flex_block.is_block_container());
        assert!(flex_block.generates_formatting_context());
        assert_eq!(flex_block.formatting_context(), Some(FormattingContextType::Flex));
    }

    #[test]
    fn test_block_container_with_grid() {
        let grid_block = BoxNode::new_block(default_style(), FormattingContextType::Grid, vec![]);

        assert!(grid_block.is_block_container());
        assert!(grid_block.generates_formatting_context());
        assert_eq!(grid_block.formatting_context(), Some(FormattingContextType::Grid));
    }

    #[test]
    fn test_inline_block_formatting_context() {
        let inline_block_flex = BoxNode::new_inline_block(default_style(), FormattingContextType::Flex, vec![]);
        let inline_block_grid = BoxNode::new_inline_block(default_style(), FormattingContextType::Grid, vec![]);

        assert!(inline_block_flex.is_block_container());
        assert!(inline_block_flex.generates_formatting_context());
        assert!(inline_block_flex.is_inline_level());
        assert_eq!(
            inline_block_flex.formatting_context(),
            Some(FormattingContextType::Flex)
        );

        assert!(inline_block_grid.is_block_container());
        assert!(inline_block_grid.generates_formatting_context());
        assert!(inline_block_grid.is_inline_level());
        assert_eq!(
            inline_block_grid.formatting_context(),
            Some(FormattingContextType::Grid)
        );
    }
}
