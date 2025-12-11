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
//! <https://www.w3.org/TR/css-display-3/>

use crate::geometry::Size;
use crate::style::display::FormattingContextType;
use crate::style::ComputedStyle;
use crate::tree::debug::DebugInfo;
use std::fmt;
use std::sync::Arc;

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

#[derive(Debug, Clone)]
pub enum MarkerContent {
    Text(String),
    Image(ReplacedBox),
}

/// A list marker box
///
/// Generated for list items. Carries marker text but participates as its own
/// box type so inline/layout can treat markers specially (e.g., position
/// outside the principal block).
#[derive(Debug, Clone)]
pub struct MarkerBox {
    /// Marker payload (text or image)
    pub content: MarkerContent,
}

/// A replaced element box
///
/// Replaced elements have intrinsic dimensions provided by external content.
/// Examples: img, canvas, video, iframe
///
/// Reference: CSS 2.1 Section 10.3.2
#[derive(Debug, Clone, PartialEq)]
pub struct ReplacedBox {
    /// Type of replaced element
    pub replaced_type: ReplacedType,

    /// Intrinsic size (if known)
    ///
    /// Some replaced elements have intrinsic dimensions (images with width/height),
    /// others don't (iframes without size attributes).
    pub intrinsic_size: Option<Size>,

    /// Intrinsic aspect ratio (width / height)
    ///
    /// Used for sizing when only one dimension is specified.
    pub aspect_ratio: Option<f32>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SrcsetDescriptor {
    Density(f32),
    Width(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SrcsetCandidate {
    pub url: String,
    pub descriptor: SrcsetDescriptor,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SizesEntry {
    pub media: Option<Vec<crate::style::media::MediaQuery>>,
    pub length: crate::style::values::Length,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SizesList {
    pub entries: Vec<SizesEntry>,
}

/// Types of replaced elements
#[derive(Debug, Clone, PartialEq)]
pub enum ReplacedType {
    /// Image element
    Image {
        /// Source URL or data URI
        src: String,
        /// Alternative text for fallback rendering
        alt: Option<String>,
        /// Srcset candidates for density-aware selection
        srcset: Vec<SrcsetCandidate>,
        /// Sizes attribute values for width-descriptor selection
        sizes: Option<SizesList>,
    },

    /// Video element
    Video {
        /// Source URL
        src: String,
        /// Poster image URL or data URI
        poster: Option<String>,
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

    /// `<embed>` element
    Embed {
        /// Source URL
        src: String,
    },

    /// `<object>` element
    Object {
        /// Data URL
        data: String,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct ImageSelectionContext<'a> {
    pub scale: f32,
    pub slot_width: Option<f32>,
    pub viewport: Option<crate::geometry::Size>,
    pub media_context: Option<&'a crate::style::media::MediaContext>,
    pub font_size: Option<f32>,
}

impl ReplacedType {
    /// Selects the best image source for the given device scale and slot width.
    ///
    /// Returns the authored `src` when no better candidate exists.
    pub fn image_source_for_context<'a>(&'a self, ctx: ImageSelectionContext<'_>) -> &'a str {
        match self {
            ReplacedType::Image { src, srcset, sizes, .. } => {
                if srcset.is_empty() || !ctx.scale.is_finite() || ctx.scale <= 0.0 {
                    return src;
                }

                let effective_slot_width = ctx.slot_width.or_else(|| {
                    let viewport = ctx.viewport?;
                    if let (Some(sizes_list), Some(media_ctx)) = (sizes, ctx.media_context) {
                        Some(sizes_list.evaluate(media_ctx, viewport, ctx.font_size.unwrap_or(16.0)))
                    } else {
                        Some(viewport.width)
                    }
                });

                // Pick the smallest density >= scale; if none, the largest below.
                let mut best: Option<(&SrcsetCandidate, f32)> = None;
                for candidate in srcset {
                    let density = candidate.density_for_slot(effective_slot_width);
                    if let Some(density) = density {
                        if density <= 0.0 || !density.is_finite() {
                            continue;
                        }
                        match best {
                            Some((_, current_density)) if current_density >= ctx.scale => {
                                if density >= ctx.scale && density < current_density {
                                    best = Some((candidate, density));
                                }
                            }
                            Some((_, current_density)) => {
                                if density >= ctx.scale {
                                    best = Some((candidate, density));
                                } else if current_density < ctx.scale && density > current_density {
                                    best = Some((candidate, density));
                                }
                            }
                            None => best = Some((candidate, density)),
                        }
                    }
                }
                best.map(|(c, _)| c.url.as_str()).unwrap_or(src)
            }
            _ => "",
        }
    }
}

impl SrcsetCandidate {
    pub fn density_for_slot(&self, slot_width: Option<f32>) -> Option<f32> {
        match self.descriptor {
            SrcsetDescriptor::Density(d) => Some(d),
            SrcsetDescriptor::Width(w) => {
                let slot = slot_width?;
                if slot <= 0.0 || !slot.is_finite() {
                    return None;
                }
                Some(w as f32 / slot)
            }
        }
    }
}

impl SizesList {
    pub fn evaluate(&self, media_ctx: &crate::style::media::MediaContext, viewport: crate::geometry::Size, font_size: f32) -> f32 {
        let mut last = None;
        for entry in &self.entries {
            last = Some(entry.length);
            let media_matches = entry
                .media
                .as_ref()
                .map(|q| media_ctx.evaluate_list(q))
                .unwrap_or(true);
            if media_matches {
                return resolve_sizes_length(entry.length, viewport, font_size);
            }
        }

        if let Some(len) = last {
            resolve_sizes_length(len, viewport, font_size)
        } else {
            resolve_sizes_length(
                crate::style::values::Length::new(100.0, crate::style::values::LengthUnit::Vw),
                viewport,
                font_size,
            )
        }
    }
}

fn resolve_sizes_length(length: crate::style::values::Length, viewport: crate::geometry::Size, font_size: f32) -> f32 {
    use crate::style::values::LengthUnit;
    match length.unit {
        LengthUnit::Percent => length.resolve_against(viewport.width),
        LengthUnit::Vw | LengthUnit::Vh | LengthUnit::Vmin | LengthUnit::Vmax => {
            length.resolve_with_viewport(viewport.width, viewport.height)
        }
        LengthUnit::Em | LengthUnit::Rem => font_size * length.value,
        _ if length.unit.is_absolute() => length.to_px(),
        _ => length.resolve_against(viewport.width),
    }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

    /// Anonymous table row group box (tbody)
    ///
    /// Generated when rows aren't in explicit row groups.
    TableRowGroup,

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

    /// List marker box
    Marker(MarkerBox),

    /// Replaced element (img, video, canvas, etc.)
    Replaced(ReplacedBox),

    /// Anonymous box (generated by layout algorithm)
    Anonymous(AnonymousBox),
}

impl BoxType {
    /// Returns true if this box type is block-level
    pub fn is_block_level(&self) -> bool {
        match self {
            BoxType::Block(_) | BoxType::Replaced(_) => true,
            BoxType::Anonymous(anon) => matches!(
                anon.anonymous_type,
                AnonymousType::Block
                    | AnonymousType::TableWrapper
                    | AnonymousType::TableRowGroup
                    | AnonymousType::TableRow
                    | AnonymousType::TableCell
            ),
            _ => false,
        }
    }

    /// Returns true if this box type is inline-level
    pub fn is_inline_level(&self) -> bool {
        match self {
            BoxType::Inline(_) | BoxType::Text(_) | BoxType::Marker(_) => true,
            BoxType::Anonymous(anon) => matches!(anon.anonymous_type, AnonymousType::Inline),
            _ => false,
        }
    }

    /// Returns true if this is a text box
    pub fn is_text(&self) -> bool {
        matches!(self, BoxType::Text(_) | BoxType::Marker(_))
    }

    /// Returns true if this is a list marker box
    pub fn is_marker(&self) -> bool {
        matches!(self, BoxType::Marker(_))
    }

    /// Returns true if this is a replaced element
    pub fn is_replaced(&self) -> bool {
        matches!(self, BoxType::Replaced(_))
    }

    /// Returns true if this is an anonymous box
    pub fn is_anonymous(&self) -> bool {
        matches!(self, BoxType::Anonymous(_))
    }

    /// Gets the formatting context this box establishes (if any)
    pub fn formatting_context(&self) -> Option<FormattingContextType> {
        match self {
            BoxType::Block(block) => Some(block.formatting_context),
            BoxType::Inline(inline) => inline.formatting_context,
            BoxType::Replaced(_) => Some(FormattingContextType::Block),
            _ => None,
        }
    }
}

impl fmt::Display for BoxType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoxType::Block(_) => write!(f, "Block"),
            BoxType::Inline(_) => write!(f, "Inline"),
            BoxType::Text(_) => write!(f, "Text"),
            BoxType::Marker(_) => write!(f, "Marker"),
            BoxType::Replaced(_) => write!(f, "Replaced"),
            BoxType::Anonymous(anon) => match anon.anonymous_type {
                AnonymousType::Block => write!(f, "AnonymousBlock"),
                AnonymousType::Inline => write!(f, "AnonymousInline"),
                AnonymousType::TableWrapper => write!(f, "AnonymousTableWrapper"),
                AnonymousType::TableRowGroup => write!(f, "AnonymousTableRowGroup"),
                AnonymousType::TableRow => write!(f, "AnonymousTableRow"),
                AnonymousType::TableCell => write!(f, "AnonymousTableCell"),
            },
        }
    }
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
/// use fastrender::{BoxNode, FormattingContextType};
/// use fastrender::ComputedStyle;
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
    /// use fastrender::{BoxNode, FormattingContextType};
    /// use fastrender::ComputedStyle;
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

    /// Creates a new list marker box
    pub fn new_marker(style: Arc<ComputedStyle>, content: MarkerContent) -> Self {
        Self {
            style,
            box_type: BoxType::Marker(MarkerBox { content }),
            children: Vec::new(),
            debug_info: None,
        }
    }

    /// Creates a new replaced box
    pub fn new_replaced(
        style: Arc<ComputedStyle>,
        replaced_type: ReplacedType,
        intrinsic_size: Option<Size>,
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

    /// Creates an anonymous inline box
    pub fn new_anonymous_inline(style: Arc<ComputedStyle>, children: Vec<BoxNode>) -> Self {
        Self {
            style,
            box_type: BoxType::Anonymous(AnonymousBox {
                anonymous_type: AnonymousType::Inline,
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
        self.box_type.is_block_level()
    }

    /// Returns true if this is an inline-level box
    ///
    /// Inline-level boxes participate in inline formatting context.
    pub fn is_inline_level(&self) -> bool {
        self.box_type.is_inline_level()
    }

    /// Returns true if this is a text box
    pub fn is_text(&self) -> bool {
        self.box_type.is_text()
    }

    /// Returns true if this is a replaced element
    pub fn is_replaced(&self) -> bool {
        self.box_type.is_replaced()
    }

    /// Returns true if this is an anonymous box
    pub fn is_anonymous(&self) -> bool {
        self.box_type.is_anonymous()
    }

    /// Gets the formatting context this box establishes (if any)
    ///
    /// Returns None for inline and text boxes that don't establish contexts.
    pub fn formatting_context(&self) -> Option<FormattingContextType> {
        self.box_type.formatting_context()
    }

    /// Returns true if this box is a block container
    ///
    /// Block containers can contain block-level children and establish
    /// a block formatting context (or participate in one).
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
    pub fn is_inline_container(&self) -> bool {
        matches!(&self.box_type, BoxType::Inline(_))
    }

    /// Returns true if this box generates a formatting context
    ///
    /// Boxes that generate formatting contexts are independent layout roots.
    /// Their internal layout doesn't affect outside, and vice versa.
    pub fn generates_formatting_context(&self) -> bool {
        self.formatting_context().is_some()
    }

    /// Returns true if this is a table-internal box
    ///
    /// Table-internal boxes participate in table layout algorithms.
    pub fn is_table_internal(&self) -> bool {
        match &self.box_type {
            BoxType::Anonymous(anon) => matches!(
                anon.anonymous_type,
                AnonymousType::TableWrapper
                    | AnonymousType::TableRowGroup
                    | AnonymousType::TableRow
                    | AnonymousType::TableCell
            ),
            _ => false,
        }
    }

    /// Gets text content if this is a text box
    pub fn text(&self) -> Option<&str> {
        match &self.box_type {
            BoxType::Text(text_box) => Some(&text_box.text),
            BoxType::Marker(marker_box) => match &marker_box.content {
                MarkerContent::Text(text) => Some(text.as_str()),
                _ => None,
            },
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
/// use fastrender::{BoxTree, BoxNode, FormattingContextType};
/// use fastrender::ComputedStyle;
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
    use crate::geometry::Size;
    use crate::style::media::MediaContext;
    use crate::style::values::{Length, LengthUnit};
    use crate::style::display::FormattingContextType;

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
                alt: None,
                sizes: None,
                srcset: Vec::new(),
            },
            Some(Size::new(100.0, 50.0)),
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
                alt: None,
                sizes: None,
                srcset: Vec::new(),
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

    #[test]
    fn test_box_type_display() {
        let block = BoxType::Block(BlockBox {
            formatting_context: FormattingContextType::Block,
        });
        let inline = BoxType::Inline(InlineBox {
            formatting_context: None,
        });
        let text = BoxType::Text(TextBox {
            text: "hello".to_string(),
        });
        let anon_block = BoxType::Anonymous(AnonymousBox {
            anonymous_type: AnonymousType::Block,
        });
        let anon_inline = BoxType::Anonymous(AnonymousBox {
            anonymous_type: AnonymousType::Inline,
        });

        assert_eq!(format!("{}", block), "Block");
        assert_eq!(format!("{}", inline), "Inline");
        assert_eq!(format!("{}", text), "Text");
        assert_eq!(format!("{}", anon_block), "AnonymousBlock");
        assert_eq!(format!("{}", anon_inline), "AnonymousInline");
    }

    #[test]
    fn image_source_prefers_width_descriptor_with_sizes() {
        let img = ReplacedType::Image {
            src: "fallback".to_string(),
            alt: None,
            srcset: vec![
                SrcsetCandidate {
                    url: "100w".to_string(),
                    descriptor: SrcsetDescriptor::Width(100),
                },
                SrcsetCandidate {
                    url: "300w".to_string(),
                    descriptor: SrcsetDescriptor::Width(300),
                },
            ],
            sizes: Some(SizesList {
                entries: vec![SizesEntry {
                    media: None,
                    length: Length::new(50.0, LengthUnit::Vw),
                }],
            }),
        };

        let viewport = Size::new(200.0, 100.0);
        let media_ctx = MediaContext::screen(viewport.width, viewport.height).with_device_pixel_ratio(2.0);
        let chosen = img.image_source_for_context(ImageSelectionContext {
            scale: 2.0,
            slot_width: None,
            viewport: Some(viewport),
            media_context: Some(&media_ctx),
            font_size: Some(16.0),
        });

        assert_eq!(chosen, "300w");
    }

    #[test]
    fn sizes_default_to_last_entry_when_no_media_match() {
        let img = ReplacedType::Image {
            src: "fallback".to_string(),
            alt: None,
            srcset: vec![
                SrcsetCandidate {
                    url: "100w".to_string(),
                    descriptor: SrcsetDescriptor::Width(100),
                },
                SrcsetCandidate {
                    url: "400w".to_string(),
                    descriptor: SrcsetDescriptor::Width(400),
                },
            ],
            sizes: Some(SizesList {
                entries: vec![
                    SizesEntry {
                        media: Some(vec![crate::style::media::MediaQuery::parse("(max-width: 10px)").unwrap()]),
                        length: Length::new(50.0, LengthUnit::Vw),
                    },
                    SizesEntry {
                        media: None,
                        length: Length::px(300.0),
                    },
                ],
            }),
        };

        let viewport = Size::new(1200.0, 800.0);
        let media_ctx = MediaContext::screen(viewport.width, viewport.height).with_device_pixel_ratio(1.0);
        let chosen = img.image_source_for_context(ImageSelectionContext {
            scale: 1.0,
            slot_width: None,
            viewport: Some(viewport),
            media_context: Some(&media_ctx),
            font_size: Some(16.0),
        });

        assert_eq!(chosen, "400w", "last sizes entry (300px) should drive selection");
    }
}
