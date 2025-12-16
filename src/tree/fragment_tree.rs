//! Fragment Tree - Represents layout results with positions and sizes
//!
//! Fragments are the output of layout. Unlike boxes (which represent what
//! to layout), fragments represent where things ended up with final positions
//! and dimensions.
//!
//! # Key Differences from Boxes
//!
//! | Box Tree | Fragment Tree |
//! |----------|---------------|
//! | Immutable | Created per layout |
//! | No position | Has position (Rect) |
//! | 1 box = 1 box | 1 box → N fragments (splitting) |
//! | Input to layout | Output of layout |
//!
//! # Fragment Splitting
//!
//! A single box can generate multiple fragments:
//! - Inline boxes split across multiple lines
//! - Blocks split across columns or pages
//! - Table cells split for pagination
//!
//! # Usage
//!
//! ```
//! use fastrender::{FragmentNode, FragmentContent};
//! use fastrender::{Rect, Point, Size};
//!
//! let fragment = FragmentNode::new_block(
//!     Rect::from_xywh(10.0, 20.0, 100.0, 50.0),
//!     vec![],
//! );
//!
//! assert_eq!(fragment.bounds.x(), 10.0);
//! assert!(fragment.contains_point(Point::new(50.0, 30.0)));
//! ```

use crate::geometry::{Point, Rect, Size};
use crate::style::ComputedStyle;
use crate::text::pipeline::ShapedRun;
use crate::tree::box_tree::ReplacedType;
use std::fmt;
use std::sync::Arc;

/// Content type of a fragment
///
/// Fragments can contain different types of content, each requiring
/// different paint and hit-testing logic.
#[derive(Debug, Clone)]
pub enum FragmentContent {
    /// Block-level content
    ///
    /// A positioned block box. Children are other block or line fragments.
    Block {
        /// Index or ID of source BoxNode
        /// For now, just store an optional ID
        box_id: Option<usize>,
    },

    /// Inline-level content (possibly split from inline box)
    ///
    /// A fragment of an inline box. One inline box can generate multiple
    /// inline fragments when it wraps across lines.
    Inline {
        /// Index or ID of source BoxNode
        box_id: Option<usize>,

        /// Which fragment this is (0 = first, 1 = second, etc.)
        /// Used when a single inline box splits across multiple lines
        fragment_index: usize,
    },

    /// Text content with shaped glyphs
    ///
    /// Actual text that has been shaped (font, positions, etc.).
    Text {
        /// The text content
        text: String,

        /// Index or ID of source BoxNode (TextBox)
        box_id: Option<usize>,

        /// Baseline offset from fragment top
        /// Used for text alignment within line
        baseline_offset: f32,

        /// Pre-shaped runs for this text, if available
        ///
        /// Carrying shaped runs from layout allows painting to reuse the exact
        /// glyph positions and fonts chosen during layout instead of reshaping
        /// with potentially different fallback results.
        shaped: Option<Vec<ShapedRun>>,

        /// True when this fragment represents a list marker (::marker)
        is_marker: bool,
    },

    /// Line box containing inline and text fragments
    ///
    /// Line boxes are generated during inline layout. They contain
    /// inline-level and text fragments arranged horizontally.
    Line {
        /// Baseline position relative to line box top
        baseline: f32,
    },

    /// Replaced element content
    ///
    /// A positioned replaced element (img, canvas, video, etc.)
    Replaced {
        /// Type of replaced content
        replaced_type: ReplacedType,

        /// Index or ID of source BoxNode
        box_id: Option<usize>,
    },
}

// ReplacedType is imported from box_tree to avoid duplication

impl FragmentContent {
    /// Returns true if this is a block fragment
    pub fn is_block(&self) -> bool {
        matches!(self, FragmentContent::Block { .. })
    }

    /// Returns true if this is an inline fragment
    pub fn is_inline(&self) -> bool {
        matches!(self, FragmentContent::Inline { .. })
    }

    /// Returns true if this is a text fragment
    pub fn is_text(&self) -> bool {
        matches!(self, FragmentContent::Text { .. })
    }

    /// Returns true if this is a line fragment
    pub fn is_line(&self) -> bool {
        matches!(self, FragmentContent::Line { .. })
    }

    /// Returns true if this is a replaced element
    pub fn is_replaced(&self) -> bool {
        matches!(self, FragmentContent::Replaced { .. })
    }

    /// Gets the text content if this is a text fragment
    pub fn text(&self) -> Option<&str> {
        match self {
            FragmentContent::Text { text, .. } => Some(text),
            _ => None,
        }
    }
}

/// A single fragment in the fragment tree
///
/// Represents a laid-out box with a definite position and size.
///
/// # Examples
///
/// ```
/// use fastrender::{FragmentNode, FragmentContent};
/// use fastrender::Rect;
///
/// let fragment = FragmentNode::new_block(
///     Rect::from_xywh(0.0, 0.0, 100.0, 50.0),
///     vec![],
/// );
///
/// assert_eq!(fragment.bounds.width(), 100.0);
/// assert!(fragment.content.is_block());
/// ```
#[derive(Debug, Clone)]
pub struct FragmentNode {
    /// The positioned rectangle of this fragment
    ///
    /// This is the final computed position and size after layout.
    /// All coordinates are in the coordinate space of the containing fragment.
    pub bounds: Rect,

    /// The content type of this fragment
    pub content: FragmentContent,

    /// Optional baseline offset from the fragment's top edge.
    ///
    /// Useful for fragments that need to participate in baseline alignment
    /// even when they don't contain explicit line/text children (e.g., tables).
    pub baseline: Option<f32>,

    /// Child fragments
    ///
    /// For block fragments: block and line children
    /// For line fragments: inline and text children
    /// For inline/text/replaced: typically empty
    pub children: Vec<FragmentNode>,

    /// Computed style for painting
    ///
    /// Contains color, background, border, font and other paint-relevant properties.
    /// Optional for backwards compatibility with tests.
    pub style: Option<Arc<ComputedStyle>>,
}

impl FragmentNode {
    /// Creates a new fragment with the given bounds, content, and children
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::{FragmentNode, FragmentContent};
    /// use fastrender::Rect;
    ///
    /// let bounds = Rect::from_xywh(10.0, 20.0, 100.0, 50.0);
    /// let content = FragmentContent::Block { box_id: None };
    /// let fragment = FragmentNode::new(bounds, content, vec![]);
    ///
    /// assert_eq!(fragment.bounds.x(), 10.0);
    /// ```
    pub fn new(bounds: Rect, content: FragmentContent, children: Vec<FragmentNode>) -> Self {
        Self {
            bounds,
            content,
            baseline: None,
            children,
            style: None,
        }
    }

    /// Creates a new fragment with style information
    pub fn new_with_style(
        bounds: Rect,
        content: FragmentContent,
        children: Vec<FragmentNode>,
        style: Arc<ComputedStyle>,
    ) -> Self {
        Self {
            bounds,
            content,
            baseline: None,
            children,
            style: Some(style),
        }
    }

    /// Creates a new block fragment
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::FragmentNode;
    /// use fastrender::Rect;
    ///
    /// let fragment = FragmentNode::new_block(
    ///     Rect::from_xywh(0.0, 0.0, 200.0, 100.0),
    ///     vec![],
    /// );
    ///
    /// assert!(fragment.content.is_block());
    /// ```
    pub fn new_block(bounds: Rect, children: Vec<FragmentNode>) -> Self {
        Self::new(bounds, FragmentContent::Block { box_id: None }, children)
    }

    /// Creates a new block fragment with style
    pub fn new_block_styled(bounds: Rect, children: Vec<FragmentNode>, style: Arc<ComputedStyle>) -> Self {
        Self::new_with_style(bounds, FragmentContent::Block { box_id: None }, children, style)
    }

    /// Returns a copy of this fragment with an explicit baseline offset.
    pub fn with_baseline(mut self, baseline: f32) -> Self {
        self.baseline = Some(baseline);
        self
    }

    /// Creates a new block fragment with a box ID
    pub fn new_block_with_id(bounds: Rect, box_id: usize, children: Vec<FragmentNode>) -> Self {
        Self::new(bounds, FragmentContent::Block { box_id: Some(box_id) }, children)
    }

    /// Creates a new inline fragment
    pub fn new_inline(bounds: Rect, fragment_index: usize, children: Vec<FragmentNode>) -> Self {
        Self::new(
            bounds,
            FragmentContent::Inline {
                box_id: None,
                fragment_index,
            },
            children,
        )
    }

    /// Creates a new inline fragment with style
    pub fn new_inline_styled(
        bounds: Rect,
        fragment_index: usize,
        children: Vec<FragmentNode>,
        style: Arc<ComputedStyle>,
    ) -> Self {
        Self::new_with_style(
            bounds,
            FragmentContent::Inline {
                box_id: None,
                fragment_index,
            },
            children,
            style,
        )
    }

    /// Creates a new text fragment
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::FragmentNode;
    /// use fastrender::Rect;
    ///
    /// let fragment = FragmentNode::new_text(
    ///     Rect::from_xywh(0.0, 0.0, 50.0, 20.0),
    ///     "Hello".to_string(),
    ///     16.0, // baseline offset
    /// );
    ///
    /// assert!(fragment.content.is_text());
    /// assert_eq!(fragment.content.text(), Some("Hello"));
    /// ```
    pub fn new_text(bounds: Rect, text: String, baseline_offset: f32) -> Self {
        Self::new(
            bounds,
            FragmentContent::Text {
                text,
                box_id: None,
                baseline_offset,
                shaped: None,
                is_marker: false,
            },
            vec![],
        )
    }

    /// Creates a new text fragment with style
    pub fn new_text_styled(bounds: Rect, text: String, baseline_offset: f32, style: Arc<ComputedStyle>) -> Self {
        Self::new_with_style(
            bounds,
            FragmentContent::Text {
                text,
                box_id: None,
                baseline_offset,
                shaped: None,
                is_marker: false,
            },
            vec![],
            style,
        )
    }

    /// Creates a new text fragment with pre-shaped runs and style
    pub fn new_text_shaped(
        bounds: Rect,
        text: String,
        baseline_offset: f32,
        shaped: Vec<ShapedRun>,
        style: Arc<ComputedStyle>,
    ) -> Self {
        Self::new_with_style(
            bounds,
            FragmentContent::Text {
                text,
                box_id: None,
                baseline_offset,
                shaped: Some(shaped),
                is_marker: false,
            },
            vec![],
            style,
        )
    }

    /// Creates a new line fragment
    pub fn new_line(bounds: Rect, baseline: f32, children: Vec<FragmentNode>) -> Self {
        Self::new(bounds, FragmentContent::Line { baseline }, children)
    }

    /// Creates a new replaced element fragment
    pub fn new_replaced(bounds: Rect, replaced_type: ReplacedType) -> Self {
        Self::new(
            bounds,
            FragmentContent::Replaced {
                replaced_type,
                box_id: None,
            },
            vec![],
        )
    }

    /// Gets the style for this fragment, if available
    pub fn get_style(&self) -> Option<&ComputedStyle> {
        self.style.as_ref().map(|s| s.as_ref())
    }

    /// Returns the number of children
    pub fn child_count(&self) -> usize {
        self.children.len()
    }

    /// Returns true if this fragment has no children
    pub fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }

    /// Computes the bounding box of this fragment and all its children
    ///
    /// Returns the minimal rectangle that contains this fragment and all
    /// descendants. Useful for paint invalidation and scrolling.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::FragmentNode;
    /// use fastrender::Rect;
    ///
    /// let child1 = FragmentNode::new_block(
    ///     Rect::from_xywh(0.0, 0.0, 50.0, 50.0),
    ///     vec![],
    /// );
    /// let child2 = FragmentNode::new_block(
    ///     Rect::from_xywh(60.0, 0.0, 50.0, 50.0),
    ///     vec![],
    /// );
    /// let parent = FragmentNode::new_block(
    ///     Rect::from_xywh(0.0, 0.0, 200.0, 100.0),
    ///     vec![child1, child2],
    /// );
    ///
    /// let bbox = parent.bounding_box();
    /// // Should encompass parent and both children
    /// assert_eq!(bbox.min_x(), 0.0);
    /// assert_eq!(bbox.max_x(), 200.0);
    /// ```
    pub fn bounding_box(&self) -> Rect {
        // Start with this fragment's bounds
        let mut bbox = self.bounds;

        // Union with all children's bounding boxes
        for child in &self.children {
            let child_bbox = child.bounding_box();
            bbox = bbox.union(child_bbox);
        }

        bbox
    }

    /// Translates this fragment and all children by the given offset
    ///
    /// This is useful when repositioning a subtree during layout or
    /// when computing absolute positions.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::FragmentNode;
    /// use fastrender::{Rect, Point};
    ///
    /// let fragment = FragmentNode::new_block(
    ///     Rect::from_xywh(10.0, 20.0, 100.0, 50.0),
    ///     vec![],
    /// );
    ///
    /// let translated = fragment.translate(Point::new(5.0, 10.0));
    /// assert_eq!(translated.bounds.x(), 15.0);
    /// assert_eq!(translated.bounds.y(), 30.0);
    /// ```
    pub fn translate(&self, offset: Point) -> Self {
        Self {
            bounds: self.bounds.translate(offset),
            content: self.content.clone(),
            baseline: self.baseline,
            children: self.children.iter().map(|child| child.translate(offset)).collect(),
            style: self.style.clone(),
        }
    }

    /// Returns true if this fragment contains the given point
    ///
    /// Checks only this fragment's bounds, not children.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::FragmentNode;
    /// use fastrender::{Rect, Point};
    ///
    /// let fragment = FragmentNode::new_block(
    ///     Rect::from_xywh(10.0, 10.0, 100.0, 100.0),
    ///     vec![],
    /// );
    ///
    /// assert!(fragment.contains_point(Point::new(50.0, 50.0)));
    /// assert!(!fragment.contains_point(Point::new(5.0, 5.0)));
    /// ```
    pub fn contains_point(&self, point: Point) -> bool {
        self.bounds.contains_point(point)
    }

    /// Finds all fragments at the given point
    ///
    /// Returns fragments in reverse paint order (topmost first).
    /// This is useful for hit testing and event handling.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::FragmentNode;
    /// use fastrender::{Rect, Point};
    ///
    /// let child = FragmentNode::new_block(
    ///     Rect::from_xywh(20.0, 20.0, 30.0, 30.0),
    ///     vec![],
    /// );
    /// let parent = FragmentNode::new_block(
    ///     Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
    ///     vec![child],
    /// );
    ///
    /// let hits = parent.fragments_at_point(Point::new(25.0, 25.0));
    /// // Should find both child and parent
    /// assert_eq!(hits.len(), 2);
    /// ```
    pub fn fragments_at_point(&self, point: Point) -> Vec<&FragmentNode> {
        let mut result = Vec::new();

        // Check children first (reverse paint order = depth-first, reversed)
        for child in self.children.iter().rev() {
            result.extend(child.fragments_at_point(point));
        }

        // Check this fragment
        if self.contains_point(point) {
            result.push(self);
        }

        result
    }

    /// Iterates over all fragments in paint order (depth-first, pre-order)
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::FragmentNode;
    /// use fastrender::Rect;
    ///
    /// let child1 = FragmentNode::new_block(
    ///     Rect::from_xywh(0.0, 0.0, 50.0, 50.0),
    ///     vec![],
    /// );
    /// let child2 = FragmentNode::new_block(
    ///     Rect::from_xywh(0.0, 50.0, 50.0, 50.0),
    ///     vec![],
    /// );
    /// let parent = FragmentNode::new_block(
    ///     Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
    ///     vec![child1, child2],
    /// );
    ///
    /// let all_fragments: Vec<_> = parent.iter_fragments().collect();
    /// assert_eq!(all_fragments.len(), 3); // parent + 2 children
    /// ```
    pub fn iter_fragments(&self) -> FragmentIterator<'_> {
        FragmentIterator { stack: vec![self] }
    }

    /// Returns an iterator over direct children
    pub fn children(&self) -> impl Iterator<Item = &FragmentNode> {
        self.children.iter()
    }
}

/// Iterator over fragments in paint order (depth-first, pre-order)
pub struct FragmentIterator<'a> {
    stack: Vec<&'a FragmentNode>,
}

impl<'a> Iterator for FragmentIterator<'a> {
    type Item = &'a FragmentNode;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(fragment) = self.stack.pop() {
            // Push children in reverse order so they're processed in correct order
            for child in fragment.children.iter().rev() {
                self.stack.push(child);
            }
            Some(fragment)
        } else {
            None
        }
    }
}

/// The fragment tree - output of layout
///
/// Contains the root fragment and provides tree-level operations.
///
/// # Examples
///
/// ```
/// use fastrender::{FragmentTree, FragmentNode};
/// use fastrender::Rect;
///
/// let root = FragmentNode::new_block(
///     Rect::from_xywh(0.0, 0.0, 800.0, 600.0),
///     vec![],
/// );
/// let tree = FragmentTree::new(root);
///
/// assert_eq!(tree.viewport_size().width, 800.0);
/// ```
#[derive(Debug, Clone)]
pub struct FragmentTree {
    /// The root fragment (usually the viewport or document root)
    pub root: FragmentNode,

    /// The viewport size (may differ from root fragment bounds)
    viewport: Option<Size>,
}

impl FragmentTree {
    /// Creates a new fragment tree with the given root
    pub fn new(root: FragmentNode) -> Self {
        Self { root, viewport: None }
    }

    /// Creates a new fragment tree with explicit viewport size
    ///
    /// Use this when the viewport size should be tracked separately
    /// from the root fragment's bounds (e.g., for scrollable content).
    pub fn with_viewport(root: FragmentNode, viewport: Size) -> Self {
        Self {
            root,
            viewport: Some(viewport),
        }
    }

    /// Returns the viewport size
    ///
    /// If an explicit viewport was set, returns that; otherwise returns
    /// the root fragment's size.
    pub fn viewport_size(&self) -> Size {
        self.viewport.unwrap_or(self.root.bounds.size)
    }

    /// Computes the total bounding box of all content
    pub fn content_size(&self) -> Rect {
        self.root.bounding_box()
    }

    /// Finds all fragments at the given point
    pub fn hit_test(&self, point: Point) -> Vec<&FragmentNode> {
        self.root.fragments_at_point(point)
    }

    /// Returns an iterator over all fragments in paint order
    pub fn iter_fragments(&self) -> FragmentIterator<'_> {
        self.root.iter_fragments()
    }

    /// Counts total number of fragments in the tree
    pub fn fragment_count(&self) -> usize {
        self.iter_fragments().count()
    }
}

impl fmt::Display for FragmentTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FragmentTree(fragments: {})", self.fragment_count())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Constructor tests
    #[test]
    fn test_new_block_fragment() {
        let fragment = FragmentNode::new_block(Rect::from_xywh(10.0, 20.0, 100.0, 50.0), vec![]);

        assert_eq!(fragment.bounds.x(), 10.0);
        assert_eq!(fragment.bounds.y(), 20.0);
        assert!(fragment.content.is_block());
        assert_eq!(fragment.child_count(), 0);
    }

    #[test]
    fn test_new_text_fragment() {
        let fragment = FragmentNode::new_text(Rect::from_xywh(0.0, 0.0, 50.0, 20.0), "Hello World".to_string(), 16.0);

        assert!(fragment.content.is_text());
        assert_eq!(fragment.content.text(), Some("Hello World"));
    }

    #[test]
    fn test_new_inline_fragment() {
        let fragment = FragmentNode::new_inline(Rect::from_xywh(0.0, 0.0, 100.0, 20.0), 0, vec![]);

        assert!(fragment.content.is_inline());
    }

    #[test]
    fn test_new_line_fragment() {
        let text = FragmentNode::new_text(Rect::from_xywh(0.0, 0.0, 50.0, 20.0), "Text".to_string(), 16.0);
        let line = FragmentNode::new_line(Rect::from_xywh(0.0, 0.0, 200.0, 20.0), 16.0, vec![text]);

        assert!(line.content.is_line());
        assert_eq!(line.child_count(), 1);
    }

    // Bounding box tests
    #[test]
    fn test_bounding_box_single() {
        let fragment = FragmentNode::new_block(Rect::from_xywh(10.0, 20.0, 100.0, 50.0), vec![]);

        let bbox = fragment.bounding_box();
        assert_eq!(bbox, fragment.bounds);
    }

    #[test]
    fn test_bounding_box_with_children() {
        let child1 = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 50.0), vec![]);
        let child2 = FragmentNode::new_block(Rect::from_xywh(60.0, 0.0, 50.0, 50.0), vec![]);
        let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 200.0, 100.0), vec![child1, child2]);

        let bbox = parent.bounding_box();
        assert_eq!(bbox.min_x(), 0.0);
        assert_eq!(bbox.max_x(), 200.0);
        assert_eq!(bbox.min_y(), 0.0);
        assert_eq!(bbox.max_y(), 100.0);
    }

    #[test]
    fn test_bounding_box_nested() {
        let grandchild = FragmentNode::new_block(Rect::from_xywh(150.0, 150.0, 50.0, 50.0), vec![]);
        let child = FragmentNode::new_block(Rect::from_xywh(50.0, 50.0, 100.0, 100.0), vec![grandchild]);
        let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 200.0, 200.0), vec![child]);

        let bbox = parent.bounding_box();
        // Should include grandchild at (150, 150) with size (50, 50)
        assert_eq!(bbox.min_x(), 0.0);
        assert_eq!(bbox.max_x(), 200.0);
        assert_eq!(bbox.max_y(), 200.0);
    }

    // Translation tests
    #[test]
    fn test_translate_single() {
        let fragment = FragmentNode::new_block(Rect::from_xywh(10.0, 20.0, 100.0, 50.0), vec![]);

        let translated = fragment.translate(Point::new(5.0, 10.0));
        assert_eq!(translated.bounds.x(), 15.0);
        assert_eq!(translated.bounds.y(), 30.0);
        assert_eq!(translated.bounds.width(), 100.0);
    }

    #[test]
    fn test_translate_with_children() {
        let child = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 30.0, 30.0), vec![]);
        let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![child]);

        let translated = parent.translate(Point::new(50.0, 50.0));
        assert_eq!(translated.bounds.x(), 50.0);
        assert_eq!(translated.bounds.y(), 50.0);
        assert_eq!(translated.children[0].bounds.x(), 60.0);
        assert_eq!(translated.children[0].bounds.y(), 60.0);
    }

    // Hit testing tests
    #[test]
    fn test_contains_point() {
        let fragment = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 100.0, 100.0), vec![]);

        assert!(fragment.contains_point(Point::new(50.0, 50.0)));
        assert!(fragment.contains_point(Point::new(10.0, 10.0))); // Boundary
        assert!(fragment.contains_point(Point::new(110.0, 110.0))); // Boundary
        assert!(!fragment.contains_point(Point::new(5.0, 5.0)));
        assert!(!fragment.contains_point(Point::new(120.0, 120.0)));
    }

    #[test]
    fn test_fragments_at_point_single() {
        let fragment = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![]);

        let hits = fragment.fragments_at_point(Point::new(50.0, 50.0));
        assert_eq!(hits.len(), 1);
    }

    #[test]
    fn test_fragments_at_point_with_children() {
        let child = FragmentNode::new_block(Rect::from_xywh(20.0, 20.0, 30.0, 30.0), vec![]);
        let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![child]);

        // Point in child
        let hits = parent.fragments_at_point(Point::new(25.0, 25.0));
        assert_eq!(hits.len(), 2); // Both child and parent

        // Point only in parent
        let hits = parent.fragments_at_point(Point::new(5.0, 5.0));
        assert_eq!(hits.len(), 1); // Only parent
    }

    #[test]
    fn test_fragments_at_point_overlapping() {
        let child1 = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 50.0, 50.0), vec![]);
        let child2 = FragmentNode::new_block(Rect::from_xywh(30.0, 30.0, 50.0, 50.0), vec![]);
        let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![child1, child2]);

        // Point in overlapping region
        let hits = parent.fragments_at_point(Point::new(40.0, 40.0));
        assert_eq!(hits.len(), 3); // Both children and parent
                                   // child2 should come first (reverse paint order)
    }

    // Tree traversal tests
    #[test]
    fn test_iter_fragments_single() {
        let fragment = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![]);

        let count = fragment.iter_fragments().count();
        assert_eq!(count, 1);
    }

    #[test]
    fn test_iter_fragments_with_children() {
        let child1 = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 50.0), vec![]);
        let child2 = FragmentNode::new_block(Rect::from_xywh(0.0, 50.0, 50.0, 50.0), vec![]);
        let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![child1, child2]);

        let fragments: Vec<_> = parent.iter_fragments().collect();
        assert_eq!(fragments.len(), 3); // parent + 2 children
                                        // First should be parent (pre-order)
        assert_eq!(fragments[0].bounds, parent.bounds);
    }

    // FragmentTree tests
    #[test]
    fn test_fragment_tree_creation() {
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 800.0, 600.0), vec![]);
        let tree = FragmentTree::new(root);

        assert_eq!(tree.viewport_size().width, 800.0);
        assert_eq!(tree.viewport_size().height, 600.0);
    }

    #[test]
    fn test_fragment_tree_hit_test() {
        let child = FragmentNode::new_block(Rect::from_xywh(100.0, 100.0, 50.0, 50.0), vec![]);
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 800.0, 600.0), vec![child]);
        let tree = FragmentTree::new(root);

        let hits = tree.hit_test(Point::new(120.0, 120.0));
        assert_eq!(hits.len(), 2); // child and root
    }

    #[test]
    fn test_fragment_tree_count() {
        let child1 = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![]);
        let child2 = FragmentNode::new_block(Rect::from_xywh(0.0, 100.0, 100.0, 100.0), vec![]);
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 800.0, 600.0), vec![child1, child2]);
        let tree = FragmentTree::new(root);

        assert_eq!(tree.fragment_count(), 3);
    }

    // Edge case tests
    #[test]
    fn test_empty_tree_traversal() {
        let fragment = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![]);

        assert_eq!(fragment.iter_fragments().count(), 1);
    }

    #[test]
    fn test_is_leaf() {
        let leaf = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![]);
        assert!(leaf.is_leaf());

        let child = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 50.0), vec![]);
        let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![child]);
        assert!(!parent.is_leaf());
    }

    #[test]
    fn test_replaced_fragment() {
        let replaced = FragmentNode::new_replaced(
            Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
            ReplacedType::Image {
                src: "test.png".to_string(),
                alt: None,
                sizes: None,
                srcset: Vec::new(),
            },
        );

        assert!(replaced.content.is_replaced());
        assert!(replaced.is_leaf());
    }

    #[test]
    fn test_fragment_content_type_checks() {
        let block = FragmentContent::Block { box_id: None };
        assert!(block.is_block());
        assert!(!block.is_inline());
        assert!(!block.is_text());
        assert!(!block.is_line());
        assert!(!block.is_replaced());

        let text = FragmentContent::Text {
            text: "test".to_string(),
            box_id: None,
            baseline_offset: 0.0,
            shaped: None,
            is_marker: false,
        };
        assert!(text.is_text());
        assert_eq!(text.text(), Some("test"));
    }

    #[test]
    fn test_block_with_id() {
        let fragment = FragmentNode::new_block_with_id(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), 42, vec![]);

        assert!(fragment.content.is_block());
        match fragment.content {
            FragmentContent::Block { box_id } => assert_eq!(box_id, Some(42)),
            _ => panic!("Expected block content"),
        }
    }

    #[test]
    fn test_children_iterator() {
        let child1 = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 50.0), vec![]);
        let child2 = FragmentNode::new_block(Rect::from_xywh(50.0, 0.0, 50.0, 50.0), vec![]);
        let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![child1, child2]);

        assert_eq!(parent.children().count(), 2);
    }

    #[test]
    fn test_content_size() {
        let child = FragmentNode::new_block(Rect::from_xywh(50.0, 50.0, 100.0, 100.0), vec![]);
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 800.0, 600.0), vec![child]);
        let tree = FragmentTree::new(root);

        let content = tree.content_size();
        assert_eq!(content.min_x(), 0.0);
        assert_eq!(content.max_x(), 800.0);
        assert_eq!(content.max_y(), 600.0);
    }
}
