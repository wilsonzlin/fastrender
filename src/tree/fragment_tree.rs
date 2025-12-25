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

use crate::css::types::KeyframesRule;
use crate::geometry::Point;
use crate::geometry::Rect;
use crate::geometry::Size;
use crate::scroll::ScrollMetadata;
use crate::style::ComputedStyle;
use crate::text::pipeline::ShapedRun;
use crate::tree::box_tree::ReplacedType;
use std::collections::HashMap;
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

/// Identifies the fragmentainer (page/column) a fragment belongs to.
///
/// Pagination yields distinct pages (`page_index`), while multi-column layout can further
/// partition content into column sets (`column_set_index`) and individual columns
/// (`column_index`). Fields are optional so a non-paginated, non-column layout can still use
/// the default `page_index = 0` with `None` for the column components.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FragmentainerPath {
  /// Zero-based page index within the paginated flow.
  pub page_index: usize,
  /// Which column set this fragment is part of (e.g., the nth multi-column segment).
  pub column_set_index: Option<usize>,
  /// Which column within the active column set this fragment occupies.
  pub column_index: Option<usize>,
}

impl FragmentainerPath {
  /// Creates a new path for the given page with no column information.
  pub fn new(page_index: usize) -> Self {
    Self {
      page_index,
      column_set_index: None,
      column_index: None,
    }
  }

  /// Updates the page index while preserving column information.
  pub fn with_page_index(mut self, page_index: usize) -> Self {
    self.page_index = page_index;
    self
  }

  /// Sets the column set and column indices.
  pub fn with_columns(mut self, column_set_index: usize, column_index: usize) -> Self {
    self.column_set_index = Some(column_set_index);
    self.column_index = Some(column_index);
    self
  }

  /// Combines this path with an existing one, filling in any missing column metadata.
  ///
  /// The supplied path always wins for page/column fields that are present; existing column
  /// metadata is used as a fallback when the new path leaves them unset.
  pub fn inherit_from(self, existing: &FragmentainerPath) -> Self {
    Self {
      page_index: self.page_index,
      column_set_index: self.column_set_index.or(existing.column_set_index),
      column_index: self.column_index.or(existing.column_index),
    }
  }

  /// Returns a flattened index representing the innermost fragmentainer.
  pub fn flattened_index(&self) -> usize {
    self
      .column_index
      .or(self.column_set_index)
      .unwrap_or(self.page_index)
  }
}

impl Default for FragmentainerPath {
  fn default() -> Self {
    Self::new(0)
  }
}

/// Block-level metadata useful for fragmentation adjustments.
///
/// Margins are stored as resolved pixel values from layout. The clipped flags
/// indicate whether the fragment was sliced at the top/bottom during
/// fragmentation, in which case margins should not be re-applied.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct BlockFragmentMetadata {
  pub margin_top: f32,
  pub margin_bottom: f32,
  pub clipped_top: bool,
  pub clipped_bottom: bool,
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
  /// Optional block-level metadata used for fragmentation adjustments.
  pub block_metadata: Option<BlockFragmentMetadata>,

  /// Optional logical bounds used for fragmentation decisions.
  ///
  /// When absent, logical bounds match `bounds`.
  pub logical_override: Option<Rect>,

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

  /// Index of this fragment within a fragmented sequence for the same box
  /// (e.g., when flowing across pages or columns).
  pub fragment_index: usize,

  /// Total number of fragments generated for the originating box.
  pub fragment_count: usize,

  /// Which fragmentainer (page/column) this fragment occupies.
  ///
  /// This remains as a flattened index of the innermost fragmentainer (column takes precedence
  /// over page) for backwards compatibility.
  pub fragmentainer_index: usize,

  /// Structured fragmentainer metadata (page/column set/column).
  pub fragmentainer: FragmentainerPath,

  /// Metadata about how this fragment relates to other fragments of the same box.
  ///
  /// Used for `box-decoration-break: slice`.
  pub slice_info: FragmentSliceInfo,

  /// Scrollable overflow area for this fragment (including descendants),
  /// expressed in the fragment's local coordinate space.
  pub scroll_overflow: Rect,

  /// Fragmentation metadata for nested fragmentainers (e.g., multi-column containers).
  pub fragmentation: Option<FragmentationInfo>,
}

/// Fragmentation metadata for a fragment.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FragmentSliceInfo {
  /// Whether this fragment starts at the box's block-start edge.
  pub is_first: bool,
  /// Whether this fragment ends at the box's block-end edge.
  pub is_last: bool,
  /// Distance from the original box's block-start edge to this fragment slice's start.
  pub slice_offset: f32,
  /// Block-size of the unfragmented box.
  pub original_block_size: f32,
}

impl FragmentSliceInfo {
  pub fn single(block_size: f32) -> Self {
    Self {
      is_first: true,
      is_last: true,
      slice_offset: 0.0,
      original_block_size: block_size,
    }
  }
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
    let scroll_overflow = Rect::from_xywh(0.0, 0.0, bounds.width(), bounds.height());
    let fragmentainer = FragmentainerPath::default();
    Self {
      bounds,
      block_metadata: None,
      logical_override: None,
      content,
      baseline: None,
      children,
      style: None,
      fragment_index: 0,
      fragment_count: 1,
      fragmentainer_index: fragmentainer.flattened_index(),
      fragmentainer,
      slice_info: FragmentSliceInfo::single(bounds.height()),
      scroll_overflow,
      fragmentation: None,
    }
  }

  /// Creates a new fragment with style information
  pub fn new_with_style(
    bounds: Rect,
    content: FragmentContent,
    children: Vec<FragmentNode>,
    style: Arc<ComputedStyle>,
  ) -> Self {
    let scroll_overflow = Rect::from_xywh(0.0, 0.0, bounds.width(), bounds.height());
    let fragmentainer = FragmentainerPath::default();
    Self {
      bounds,
      block_metadata: None,
      logical_override: None,
      content,
      baseline: None,
      children,
      style: Some(style),
      fragment_index: 0,
      fragment_count: 1,
      fragmentainer_index: fragmentainer.flattened_index(),
      fragmentainer,
      slice_info: FragmentSliceInfo::single(bounds.height()),
      scroll_overflow,
      fragmentation: None,
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
  pub fn new_block_styled(
    bounds: Rect,
    children: Vec<FragmentNode>,
    style: Arc<ComputedStyle>,
  ) -> Self {
    Self::new_with_style(
      bounds,
      FragmentContent::Block { box_id: None },
      children,
      style,
    )
  }

  /// Returns a copy of this fragment with an explicit baseline offset.
  pub fn with_baseline(mut self, baseline: f32) -> Self {
    self.baseline = Some(baseline);
    self
  }

  /// Creates a new block fragment with a box ID
  pub fn new_block_with_id(bounds: Rect, box_id: usize, children: Vec<FragmentNode>) -> Self {
    Self::new(
      bounds,
      FragmentContent::Block {
        box_id: Some(box_id),
      },
      children,
    )
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
  pub fn new_text_styled(
    bounds: Rect,
    text: String,
    baseline_offset: f32,
    style: Arc<ComputedStyle>,
  ) -> Self {
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
  /// descendants in the coordinate space of this fragment's parent (the same
  /// space as `bounds`). Useful for paint invalidation and scrolling.
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
    let mut bbox = Rect::from_xywh(0.0, 0.0, self.bounds.width(), self.bounds.height());
    for child in &self.children {
      bbox = bbox.union(child.bounding_box());
    }
    bbox.translate(self.bounds.origin)
  }

  /// Returns the logical bounds used for fragmentation decisions.
  ///
  /// When no override is set, this matches `bounds`.
  pub fn logical_bounds(&self) -> Rect {
    self.logical_override.unwrap_or(self.bounds)
  }

  /// Computes a bounding box using logical bounds for this fragment and descendants.
  pub fn logical_bounding_box(&self) -> Rect {
    let mut bbox = self.logical_bounds();
    for child in &self.children {
      let child_bbox = child.logical_bounding_box();
      bbox = bbox.union(child_bbox.translate(Point::new(
        self.logical_bounds().x(),
        self.logical_bounds().y(),
      )));
    }
    bbox
  }

  /// Translates this fragment's bounds by the given offset.
  ///
  /// Offsets are applied in the coordinate space of the containing fragment.
  /// Child fragments remain in their existing local coordinate space; this
  /// preserves relative positioning within the subtree.
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
      block_metadata: self.block_metadata.clone(),
      logical_override: self.logical_override.map(|r| r.translate(offset)),
      content: self.content.clone(),
      baseline: self.baseline,
      children: self.children.clone(),
      style: self.style.clone(),
      fragment_index: self.fragment_index,
      fragment_count: self.fragment_count,
      fragmentainer_index: self.fragmentainer_index,
      fragmentainer: self.fragmentainer,
      slice_info: self.slice_info,
      scroll_overflow: self.scroll_overflow,
      fragmentation: self.fragmentation.clone(),
    }
  }

  /// Translates this fragment and all descendants by the given offset.
  ///
  /// This applies the offset in absolute space, adjusting every fragment in the
  /// subtree. Use sparingly; most callers should prefer [`translate`], which
  /// keeps child coordinates relative to their parent.
  pub fn translate_subtree_absolute(&self, offset: Point) -> Self {
    Self {
      bounds: self.bounds.translate(offset),
      block_metadata: self.block_metadata.clone(),
      logical_override: self.logical_override.map(|r| r.translate(offset)),
      content: self.content.clone(),
      baseline: self.baseline,
      children: self
        .children
        .iter()
        .map(|child| child.translate_subtree_absolute(offset))
        .collect(),
      style: self.style.clone(),
      fragment_index: self.fragment_index,
      fragment_count: self.fragment_count,
      fragmentainer_index: self.fragmentainer_index,
      fragmentainer: self.fragmentainer,
      slice_info: self.slice_info,
      scroll_overflow: self.scroll_overflow,
      fragmentation: self.fragmentation.clone(),
    }
  }

  /// Returns true if this fragment contains the given point
  ///
  /// Checks only this fragment's bounds, not children. The point is expected to be in the
  /// coordinate space of this fragment's parent (the same space as `bounds`).
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

    // Convert the point into this fragment's local coordinate space for children.
    let local_point = Point::new(point.x - self.bounds.x(), point.y - self.bounds.y());

    // Check children first (reverse paint order = depth-first, reversed)
    for child in self.children.iter().rev() {
      result.extend(child.fragments_at_point(local_point));
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
    FragmentIterator::new(vec![self])
  }

  /// Returns an iterator over direct children
  pub fn children(&self) -> impl Iterator<Item = &FragmentNode> {
    self.children.iter()
  }
}

/// Metadata describing nested fragmentation contexts (e.g., multi-column containers).
#[derive(Debug, Clone)]
pub struct FragmentationInfo {
  pub column_count: usize,
  pub column_gap: f32,
  pub column_width: f32,
  pub flow_height: f32,
}

/// Iterator over fragments in paint order (depth-first, pre-order)
pub struct FragmentIterator<'a> {
  stack: Vec<&'a FragmentNode>,
}

impl<'a> FragmentIterator<'a> {
  pub fn new(stack: Vec<&'a FragmentNode>) -> Self {
    Self { stack }
  }
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

  /// Additional root fragments produced by pagination/column fragmentation.
  /// The first fragment is always stored in `root` for backwards compatibility.
  pub additional_fragments: Vec<FragmentNode>,

  /// Collected @keyframes rules active for this tree.
  pub keyframes: HashMap<String, KeyframesRule>,

  /// SVG filter definitions serialized from the DOM (document-level registry).
  pub svg_filter_defs: Option<Arc<HashMap<String, String>>>,

  /// The viewport size (may differ from root fragment bounds)
  viewport: Option<Size>,

  /// Scroll snap and overflow metadata derived from layout.
  pub scroll_metadata: Option<ScrollMetadata>,
}

impl FragmentTree {
  /// Creates a new fragment tree with the given root
  pub fn new(root: FragmentNode) -> Self {
    Self {
      root,
      additional_fragments: Vec::new(),
      svg_filter_defs: None,
      viewport: None,
      keyframes: HashMap::new(),
      scroll_metadata: None,
    }
  }

  /// Creates a new fragment tree with explicit viewport size
  ///
  /// Use this when the viewport size should be tracked separately
  /// from the root fragment's bounds (e.g., for scrollable content).
  pub fn with_viewport(root: FragmentNode, viewport: Size) -> Self {
    Self {
      root,
      additional_fragments: Vec::new(),
      svg_filter_defs: None,
      viewport: Some(viewport),
      keyframes: HashMap::new(),
      scroll_metadata: None,
    }
  }

  /// Creates a fragment tree from multiple root fragments (e.g., pages/columns).
  ///
  /// The first fragment is stored in `root`; the remainder are placed in
  /// `additional_fragments`.
  pub fn from_fragments(mut roots: Vec<FragmentNode>, viewport: Size) -> Self {
    let root = roots
      .drain(0..1)
      .next()
      .expect("at least one fragment root required");
    Self {
      root,
      additional_fragments: roots,
      svg_filter_defs: None,
      viewport: Some(viewport),
      keyframes: HashMap::new(),
      scroll_metadata: None,
    }
  }

  /// Returns the viewport size
  ///
  /// If an explicit viewport was set, returns that; otherwise returns
  /// the root fragment's size.
  pub fn viewport_size(&self) -> Size {
    self.viewport.unwrap_or(self.root.bounds.size)
  }

  /// Returns true when the tree tracks an explicit viewport size separate from the root fragment.
  pub fn has_explicit_viewport(&self) -> bool {
    self.viewport.is_some()
  }

  /// Computes the total bounding box of all content
  pub fn content_size(&self) -> Rect {
    let mut bbox = self.root.bounding_box();
    for root in &self.additional_fragments {
      bbox = bbox.union(root.bounding_box());
    }
    bbox
  }

  /// Finds all fragments at the given point
  pub fn hit_test(&self, point: Point) -> Vec<&FragmentNode> {
    let mut hits = self.root.fragments_at_point(point);
    for root in &self.additional_fragments {
      hits.extend(root.fragments_at_point(point));
    }
    hits
  }

  /// Returns an iterator over all fragments in paint order
  pub fn iter_fragments(&self) -> FragmentIterator<'_> {
    let mut stack: Vec<&FragmentNode> = Vec::new();
    for root in self.additional_fragments.iter().rev() {
      stack.push(root);
    }
    stack.push(&self.root);
    FragmentIterator::new(stack)
  }

  /// Counts total number of fragments in the tree
  pub fn fragment_count(&self) -> usize {
    self.iter_fragments().count()
  }

  /// Ensures scroll metadata (overflow bounds and snap targets) are computed.
  ///
  /// Layout populates this for renderer-produced trees, but helper code and
  /// tests that manually construct fragment trees can call this to enable
  /// scroll snapping without re-running layout.
  pub fn ensure_scroll_metadata(&mut self) {
    if self.scroll_metadata.is_none() {
      self.scroll_metadata = Some(crate::scroll::build_scroll_metadata(self));
    }
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
  use crate::layout::fragmentation::{fragment_tree as split_fragment_tree, FragmentationOptions};

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
    let fragment = FragmentNode::new_text(
      Rect::from_xywh(0.0, 0.0, 50.0, 20.0),
      "Hello World".to_string(),
      16.0,
    );

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
    let text = FragmentNode::new_text(
      Rect::from_xywh(0.0, 0.0, 50.0, 20.0),
      "Text".to_string(),
      16.0,
    );
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
    let parent = FragmentNode::new_block(
      Rect::from_xywh(0.0, 0.0, 200.0, 100.0),
      vec![child1, child2],
    );

    let bbox = parent.bounding_box();
    assert_eq!(bbox.min_x(), 0.0);
    assert_eq!(bbox.max_x(), 200.0);
    assert_eq!(bbox.min_y(), 0.0);
    assert_eq!(bbox.max_y(), 100.0);
  }

  #[test]
  fn test_bounding_box_nested() {
    let grandchild = FragmentNode::new_block(Rect::from_xywh(150.0, 150.0, 50.0, 50.0), vec![]);
    let child =
      FragmentNode::new_block(Rect::from_xywh(50.0, 50.0, 100.0, 100.0), vec![grandchild]);
    let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 200.0, 200.0), vec![child]);

    let bbox = parent.bounding_box();
    // Should include grandchild at (150, 150) with size (50, 50)
    assert_eq!(bbox.min_x(), 0.0);
    assert_eq!(bbox.max_x(), 250.0);
    assert_eq!(bbox.max_y(), 250.0);
  }

  #[test]
  fn test_bounding_box_accumulates_offsets() {
    let grandchild = FragmentNode::new_block(Rect::from_xywh(0.0, 30.0, 10.0, 5.0), vec![]);
    let child = FragmentNode::new_block(Rect::from_xywh(0.0, 50.0, 20.0, 20.0), vec![grandchild]);
    let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 40.0, 40.0), vec![child]);

    let bbox = parent.bounding_box();
    assert_eq!(bbox.min_y(), 0.0);
    assert_eq!(bbox.max_y(), 85.0);
  }

  #[test]
  fn test_bounding_box_with_parent_offset() {
    let child = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 30.0, 30.0), vec![]);
    let parent = FragmentNode::new_block(Rect::from_xywh(50.0, 50.0, 20.0, 20.0), vec![child]);

    let bbox = parent.bounding_box();
    assert_eq!(bbox.min_x(), 50.0);
    assert_eq!(bbox.min_y(), 50.0);
    assert_eq!(bbox.max_x(), 90.0);
    assert_eq!(bbox.max_y(), 90.0);
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
  fn test_translate_preserves_children_positions() {
    let child = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 30.0, 30.0), vec![]);
    let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![child]);

    let translated = parent.translate(Point::new(5.0, 0.0));
    assert_eq!(translated.bounds.x(), 5.0);
    assert_eq!(translated.bounds.y(), 0.0);
    // Children remain in the parent's coordinate space.
    assert_eq!(translated.children[0].bounds.x(), 10.0);
    assert_eq!(translated.children[0].bounds.y(), 10.0);
  }

  #[test]
  fn test_translate_subtree_absolute_shifts_descendants() {
    let child = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 30.0, 30.0), vec![]);
    let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![child]);

    let translated = parent.translate_subtree_absolute(Point::new(50.0, 50.0));
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
  fn test_fragments_at_point_with_translated_parent() {
    let child = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 10.0, 10.0), vec![]);
    let parent = FragmentNode::new_block(Rect::from_xywh(100.0, 100.0, 50.0, 50.0), vec![child]);

    // Global point inside both parent and child after applying parent origin.
    let hits = parent.fragments_at_point(Point::new(110.0, 110.0));
    assert_eq!(hits.len(), 2);
  }

  #[test]
  fn test_fragments_at_point_overlapping() {
    let child1 = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 50.0, 50.0), vec![]);
    let child2 = FragmentNode::new_block(Rect::from_xywh(30.0, 30.0, 50.0, 50.0), vec![]);
    let parent = FragmentNode::new_block(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![child1, child2],
    );

    // Point in overlapping region
    let hits = parent.fragments_at_point(Point::new(40.0, 40.0));
    assert_eq!(hits.len(), 3); // Both children and parent
                               // child2 should come first (reverse paint order)
  }

  #[test]
  fn test_hit_test_nested_offsets() {
    let grandchild =
      FragmentNode::new_block(Rect::from_xywh(5.0, 30.0, 10.0, 10.0), vec![]);
    let child = FragmentNode::new_block(Rect::from_xywh(10.0, 50.0, 40.0, 40.0), vec![grandchild]);
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 200.0, 200.0), vec![child]);
    let tree = FragmentTree::new(root);

    // Point should hit grandchild (5+10+?, etc.)
    let hits = tree.hit_test(Point::new(20.0, 90.0));
    assert_eq!(hits.len(), 3);
    assert!(std::ptr::eq(
      hits[0],
      &tree.root.children[0].children[0]
    ));
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
    let parent = FragmentNode::new_block(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![child1, child2],
    );

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
  fn test_fragmentation_stacks_roots_without_offsetting_children() {
    let child = FragmentNode::new_block(Rect::from_xywh(0.0, 60.0, 10.0, 10.0), vec![]);
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 20.0, 120.0), vec![child]);
    let options = FragmentationOptions::new(60.0).with_gap(20.0);

    let fragments = split_fragment_tree(&root, &options);
    assert_eq!(fragments.len(), 2);

    let second = &fragments[1];
    assert_eq!(second.bounds.y(), 80.0); // 60 fragment height + 20 gap
    assert_eq!(second.children.len(), 1);
    assert_eq!(second.children[0].bounds.y(), 0.0); // child clipped into second fragment starts at top
    assert!((second.bounds.y() + second.children[0].bounds.y() - 80.0).abs() < 0.001);
  }


  #[test]
  fn test_fragment_tree_count() {
    let child1 = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![]);
    let child2 = FragmentNode::new_block(Rect::from_xywh(0.0, 100.0, 100.0, 100.0), vec![]);
    let root = FragmentNode::new_block(
      Rect::from_xywh(0.0, 0.0, 800.0, 600.0),
      vec![child1, child2],
    );
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
        picture_sources: Vec::new(),
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
    let fragment =
      FragmentNode::new_block_with_id(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), 42, vec![]);

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
    let parent = FragmentNode::new_block(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![child1, child2],
    );

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
