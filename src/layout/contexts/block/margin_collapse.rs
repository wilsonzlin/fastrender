//! Margin collapsing algorithm (CSS 2.1 Section 8.3.1)
//!
//! This module implements the CSS margin collapsing algorithm, which defines
//! how vertical margins between block boxes combine ("collapse") into a single
//! margin rather than accumulating.
//!
//! # CSS Specification Reference
//!
//! CSS 2.1 Section 8.3.1: Collapsing margins
//! https://www.w3.org/TR/CSS21/box.html#collapsing-margins
//!
//! # Algorithm Overview
//!
//! Margins collapse when they are "adjoining" (vertically adjacent with no
//! separation). The resulting margin is:
//!
//! - **All positive**: maximum of all margins
//! - **All negative**: minimum (most negative) of all margins
//! - **Mixed**: sum of largest positive and most negative
//!
//! # Example
//!
//! ```
//! use fastrender::layout::contexts::block::margin_collapse::CollapsibleMargin;
//!
//! // Two positive margins collapse to the larger one
//! let m1 = CollapsibleMargin::from_margin(20.0);
//! let m2 = CollapsibleMargin::from_margin(30.0);
//! assert_eq!(m1.collapse_with(m2).resolve(), 30.0);
//!
//! // Mixed positive and negative margins sum
//! let m3 = CollapsibleMargin::from_margin(30.0);
//! let m4 = CollapsibleMargin::from_margin(-10.0);
//! assert_eq!(m3.collapse_with(m4).resolve(), 20.0);
//! ```

use crate::tree::{BoxNode, BoxType, FormattingContextType};

/// A collapsible margin that tracks positive and negative components separately
///
/// CSS 2.1 Section 8.3.1 specifies that when margins collapse:
/// - If all positive: result = max of all margins
/// - If all negative: result = min (most negative) of all margins
/// - If mixed: result = largest positive + most negative (a sum, since negative)
///
/// By tracking positive and negative components separately, we can correctly
/// compute the collapsed value for any combination of margins.
///
/// # Examples
///
/// ```
/// use fastrender::layout::contexts::block::margin_collapse::CollapsibleMargin;
///
/// let m1 = CollapsibleMargin::from_margin(20.0);
/// assert_eq!(m1.positive, 20.0);
/// assert_eq!(m1.negative, 0.0);
///
/// let m2 = CollapsibleMargin::from_margin(-15.0);
/// assert_eq!(m2.positive, 0.0);
/// assert_eq!(m2.negative, 15.0);
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CollapsibleMargin {
    /// Largest positive margin value (0.0 if no positive margins)
    pub positive: f32,

    /// Most negative margin value, stored as absolute value (0.0 if no negative margins)
    ///
    /// We store the absolute value so we can use max() to find the most negative.
    pub negative: f32,
}

impl CollapsibleMargin {
    /// The zero margin constant
    ///
    /// Represents no margin contribution. Useful as an initial value.
    pub const ZERO: Self = Self {
        positive: 0.0,
        negative: 0.0,
    };

    /// Creates a new collapsible margin with specified positive and negative components
    ///
    /// # Arguments
    ///
    /// * `positive` - The positive component (must be >= 0)
    /// * `negative` - The negative component as absolute value (must be >= 0)
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::contexts::block::margin_collapse::CollapsibleMargin;
    ///
    /// let m = CollapsibleMargin::new(10.0, 5.0);
    /// assert_eq!(m.resolve(), 5.0); // 10 - 5 = 5
    /// ```
    pub fn new(positive: f32, negative: f32) -> Self {
        debug_assert!(positive >= 0.0, "positive component must be >= 0");
        debug_assert!(negative >= 0.0, "negative component must be >= 0 (absolute value)");
        Self { positive, negative }
    }

    /// Creates a collapsible margin from a single margin value
    ///
    /// Positive values contribute to the positive component;
    /// negative values contribute to the negative component (stored as absolute value).
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::contexts::block::margin_collapse::CollapsibleMargin;
    ///
    /// let m1 = CollapsibleMargin::from_margin(20.0);
    /// assert_eq!(m1.positive, 20.0);
    /// assert_eq!(m1.negative, 0.0);
    ///
    /// let m2 = CollapsibleMargin::from_margin(-10.0);
    /// assert_eq!(m2.positive, 0.0);
    /// assert_eq!(m2.negative, 10.0);
    ///
    /// let m3 = CollapsibleMargin::from_margin(0.0);
    /// assert!(m3.is_zero());
    /// ```
    pub fn from_margin(value: f32) -> Self {
        if value >= 0.0 {
            Self {
                positive: value,
                negative: 0.0,
            }
        } else {
            Self {
                positive: 0.0,
                negative: -value, // Store as absolute value
            }
        }
    }

    /// Collapses this margin with another, returning the combined margin
    ///
    /// The collapse operation takes the maximum of each component:
    /// - positive = max(self.positive, other.positive)
    /// - negative = max(self.negative, other.negative)
    ///
    /// This correctly implements CSS 2.1 Section 8.3.1:
    /// - All positive: result = max
    /// - All negative: result = min (most negative)
    /// - Mixed: result = max_positive + min_negative
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::contexts::block::margin_collapse::CollapsibleMargin;
    ///
    /// // Two positive margins: take max
    /// let m1 = CollapsibleMargin::from_margin(20.0);
    /// let m2 = CollapsibleMargin::from_margin(30.0);
    /// assert_eq!(m1.collapse_with(m2).resolve(), 30.0);
    ///
    /// // Two negative margins: take most negative
    /// let m3 = CollapsibleMargin::from_margin(-20.0);
    /// let m4 = CollapsibleMargin::from_margin(-30.0);
    /// assert_eq!(m3.collapse_with(m4).resolve(), -30.0);
    ///
    /// // Mixed margins: sum of largest positive and most negative
    /// let m5 = CollapsibleMargin::from_margin(30.0);
    /// let m6 = CollapsibleMargin::from_margin(-10.0);
    /// assert_eq!(m5.collapse_with(m6).resolve(), 20.0); // 30 + (-10) = 20
    /// ```
    pub fn collapse_with(self, other: Self) -> Self {
        Self {
            positive: self.positive.max(other.positive),
            negative: self.negative.max(other.negative),
        }
    }

    /// Resolves the collapsible margin to a final pixel value
    ///
    /// The resolved value is: positive - negative
    ///
    /// This gives:
    /// - Positive result when positive > negative
    /// - Negative result when negative > positive
    /// - Zero when they're equal
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::contexts::block::margin_collapse::CollapsibleMargin;
    ///
    /// let m1 = CollapsibleMargin::new(30.0, 10.0);
    /// assert_eq!(m1.resolve(), 20.0);
    ///
    /// let m2 = CollapsibleMargin::new(10.0, 30.0);
    /// assert_eq!(m2.resolve(), -20.0);
    ///
    /// let m3 = CollapsibleMargin::ZERO;
    /// assert_eq!(m3.resolve(), 0.0);
    /// ```
    pub fn resolve(self) -> f32 {
        self.positive - self.negative
    }

    /// Checks if this margin is effectively zero
    ///
    /// Returns true if both positive and negative components are zero.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::contexts::block::margin_collapse::CollapsibleMargin;
    ///
    /// assert!(CollapsibleMargin::ZERO.is_zero());
    /// assert!(CollapsibleMargin::from_margin(0.0).is_zero());
    /// assert!(!CollapsibleMargin::from_margin(10.0).is_zero());
    /// ```
    pub fn is_zero(self) -> bool {
        self.positive == 0.0 && self.negative == 0.0
    }

    /// Adds a margin value to this collapsible margin
    ///
    /// Convenience method equivalent to `self.collapse_with(CollapsibleMargin::from_margin(value))`.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::contexts::block::margin_collapse::CollapsibleMargin;
    ///
    /// let m = CollapsibleMargin::from_margin(20.0);
    /// let m2 = m.add_margin(30.0);
    /// assert_eq!(m2.resolve(), 30.0); // max(20, 30)
    ///
    /// let m3 = m2.add_margin(-10.0);
    /// assert_eq!(m3.resolve(), 20.0); // 30 + (-10)
    /// ```
    pub fn add_margin(self, value: f32) -> Self {
        self.collapse_with(Self::from_margin(value))
    }
}

impl Default for CollapsibleMargin {
    fn default() -> Self {
        Self::ZERO
    }
}

impl std::fmt::Display for CollapsibleMargin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let resolved = self.resolve();
        if self.positive > 0.0 && self.negative > 0.0 {
            write!(f, "CM(+{}, -{}) = {}", self.positive, self.negative, resolved)
        } else if self.positive > 0.0 {
            write!(f, "CM(+{})", self.positive)
        } else if self.negative > 0.0 {
            write!(f, "CM(-{})", self.negative)
        } else {
            write!(f, "CM(0)")
        }
    }
}

/// State tracker for margin collapsing during block layout
///
/// Tracks the accumulated margin that hasn't yet been resolved, allowing
/// margins to collapse through empty blocks and between siblings.
///
/// # Usage Pattern
///
/// 1. Create at start of block layout
/// 2. For each child:
///    a. Call `process_child_margins()` with child's top/bottom margins
///    b. Use returned offset for positioning
/// 3. At end, get any remaining pending margin for parent-child collapse
///
/// # Example
///
/// ```
/// use fastrender::layout::contexts::block::margin_collapse::MarginCollapseContext;
///
/// let mut ctx = MarginCollapseContext::new();
///
/// // First child with margin-top: 20px, margin-bottom: 15px, height: 50px
/// let (offset, pending) = ctx.process_child_margins(20.0, 15.0, false);
/// // offset = 20.0 (the top margin)
///
/// // Second child with margin-top: 30px
/// let (offset2, _) = ctx.process_child_margins(30.0, 10.0, false);
/// // offset2 = 30.0 (collapsed: max(15, 30) = 30)
/// ```
#[derive(Debug, Clone)]
pub struct MarginCollapseContext {
    /// Pending margin that may collapse with the next element
    pending_margin: CollapsibleMargin,

    /// Whether we're at the start of the block (before any content)
    ///
    /// Used to handle first-child margin collapse with parent.
    at_start: bool,

    /// Whether clearance was introduced on the current element
    ///
    /// Clearance prevents margin collapse.
    has_clearance: bool,
}

impl MarginCollapseContext {
    /// Creates a new margin collapse context
    ///
    /// Starts with zero pending margin at the start of the block.
    pub fn new() -> Self {
        Self {
            pending_margin: CollapsibleMargin::ZERO,
            at_start: true,
            has_clearance: false,
        }
    }

    /// Creates a context with an initial pending margin
    ///
    /// Used when parent's top margin should collapse with first child.
    ///
    /// # Arguments
    ///
    /// * `initial_margin` - The parent's top margin to potentially collapse
    pub fn with_initial_margin(initial_margin: f32) -> Self {
        Self {
            pending_margin: CollapsibleMargin::from_margin(initial_margin),
            at_start: true,
            has_clearance: false,
        }
    }

    /// Processes a child box's margins and returns positioning information
    ///
    /// This is the main entry point for margin collapse processing during layout.
    ///
    /// # Arguments
    ///
    /// * `margin_top` - Child's top margin
    /// * `margin_bottom` - Child's bottom margin
    /// * `is_empty` - Whether child has zero height (empty blocks collapse through)
    ///
    /// # Returns
    ///
    /// A tuple of:
    /// - The collapsed margin offset to apply before positioning this child
    /// - The new pending margin after this child
    ///
    /// # Algorithm
    ///
    /// For non-empty blocks:
    /// 1. Collapse pending margin with child's top margin
    /// 2. Return the resolved value as the offset
    /// 3. Set child's bottom margin as new pending margin
    ///
    /// For empty blocks:
    /// 1. Child's top and bottom margins collapse together
    /// 2. This collapsed margin joins the pending margin
    /// 3. Return 0 offset (empty block takes no space)
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::contexts::block::margin_collapse::MarginCollapseContext;
    ///
    /// let mut ctx = MarginCollapseContext::new();
    ///
    /// // First non-empty child
    /// let (offset, _) = ctx.process_child_margins(20.0, 10.0, false);
    /// assert_eq!(offset, 20.0);
    ///
    /// // Second child - margins collapse
    /// let (offset2, _) = ctx.process_child_margins(30.0, 15.0, false);
    /// assert_eq!(offset2, 30.0); // max(10, 30)
    /// ```
    pub fn process_child_margins(
        &mut self,
        margin_top: f32,
        margin_bottom: f32,
        is_empty: bool,
    ) -> (f32, CollapsibleMargin) {
        let top_margin = CollapsibleMargin::from_margin(margin_top);
        let bottom_margin = CollapsibleMargin::from_margin(margin_bottom);

        if is_empty {
            // Empty block: top and bottom margins collapse with each other
            let self_collapsed = top_margin.collapse_with(bottom_margin);

            // Then collapse with pending margin
            self.pending_margin = self.pending_margin.collapse_with(self_collapsed);

            // Empty blocks don't consume any vertical space
            (0.0, self.pending_margin)
        } else {
            // Non-empty block
            // Collapse with pending margin from previous sibling/start
            let collapsed_top = self.pending_margin.collapse_with(top_margin);
            let offset = collapsed_top.resolve();

            // Update state: this child's bottom margin becomes pending
            self.pending_margin = bottom_margin;
            self.at_start = false;
            self.has_clearance = false;

            (offset, self.pending_margin)
        }
    }

    /// Processes margins with clearance
    ///
    /// When an element has clearance, its top margin doesn't collapse with
    /// preceding margins. The clearance value is added above the margin.
    ///
    /// # Arguments
    ///
    /// * `clearance` - The clearance value (spacing above the margin-top)
    /// * `margin_top` - Child's top margin
    /// * `margin_bottom` - Child's bottom margin
    /// * `is_empty` - Whether child has zero height
    ///
    /// # Returns
    ///
    /// The total offset (clearance + margin_top) to apply before positioning.
    ///
    /// # CSS Reference
    ///
    /// CSS 2.1 Section 9.5.2: Controlling flow next to floats
    pub fn process_child_with_clearance(
        &mut self,
        clearance: f32,
        margin_top: f32,
        margin_bottom: f32,
        is_empty: bool,
    ) -> (f32, CollapsibleMargin) {
        // Clearance prevents collapse with previous margins
        // Resolve any pending margin first
        let pending_offset = self.pending_margin.resolve();

        // Reset pending margin (clearance breaks the collapse chain)
        self.pending_margin = CollapsibleMargin::ZERO;
        self.has_clearance = true;

        if is_empty {
            // Empty block with clearance
            let self_collapsed =
                CollapsibleMargin::from_margin(margin_top).collapse_with(CollapsibleMargin::from_margin(margin_bottom));
            self.pending_margin = self_collapsed;

            (pending_offset + clearance, self.pending_margin)
        } else {
            // Non-empty block with clearance
            self.pending_margin = CollapsibleMargin::from_margin(margin_bottom);
            self.at_start = false;

            (pending_offset + clearance + margin_top, self.pending_margin)
        }
    }

    /// Gets the current pending margin
    ///
    /// Call this at the end of layout to get any margin that might
    /// collapse with the parent's bottom margin or next sibling.
    pub fn pending_margin(&self) -> CollapsibleMargin {
        self.pending_margin
    }

    /// Checks if we're still at the start of the block
    ///
    /// True if no non-empty children have been processed yet.
    /// Used for first-child margin collapsing with parent.
    pub fn is_at_start(&self) -> bool {
        self.at_start
    }

    /// Adds a margin to the pending collapsed margin
    ///
    /// Used for manual margin accumulation.
    pub fn add_margin(&mut self, margin: f32) {
        self.pending_margin = self.pending_margin.add_margin(margin);
    }

    /// Consumes the pending margin and resets it to zero
    ///
    /// Returns the resolved margin value.
    pub fn consume_pending(&mut self) -> f32 {
        let value = self.pending_margin.resolve();
        self.pending_margin = CollapsibleMargin::ZERO;
        self.at_start = false;
        value
    }
}

impl Default for MarginCollapseContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Rules for margin collapsing between parent and child boxes
///
/// These rules determine when a parent's margin can collapse with its
/// first or last child's margin.
///
/// # CSS Reference
///
/// CSS 2.1 Section 8.3.1 specifies that parent-child margins collapse when:
/// - No border, padding, or inline content separates them
/// - No clearance separates them
/// - Parent height is `auto` (for bottom margin collapse)
#[derive(Debug, Clone, Copy)]
pub struct ParentChildCollapseRules;

impl ParentChildCollapseRules {
    /// Checks if parent's top margin can collapse with first child's top margin
    ///
    /// CSS 2.1 Section 8.3.1: The top margin of an in-flow block element
    /// collapses with its first in-flow block-level child's top margin if
    /// the element has no top border, no top padding, and the child has no clearance.
    ///
    /// # Arguments
    ///
    /// * `parent_border_top` - Parent's top border width
    /// * `parent_padding_top` - Parent's top padding
    /// * `child_has_clearance` - Whether first child has clearance
    ///
    /// # Returns
    ///
    /// `true` if the margins can collapse, `false` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::contexts::block::margin_collapse::ParentChildCollapseRules;
    ///
    /// // No border/padding, no clearance - can collapse
    /// assert!(ParentChildCollapseRules::can_collapse_top_margins(0.0, 0.0, false));
    ///
    /// // Border prevents collapse
    /// assert!(!ParentChildCollapseRules::can_collapse_top_margins(1.0, 0.0, false));
    ///
    /// // Padding prevents collapse
    /// assert!(!ParentChildCollapseRules::can_collapse_top_margins(0.0, 1.0, false));
    ///
    /// // Clearance prevents collapse
    /// assert!(!ParentChildCollapseRules::can_collapse_top_margins(0.0, 0.0, true));
    /// ```
    pub fn can_collapse_top_margins(
        parent_border_top: f32,
        parent_padding_top: f32,
        child_has_clearance: bool,
    ) -> bool {
        parent_border_top == 0.0 && parent_padding_top == 0.0 && !child_has_clearance
    }

    /// Checks if parent's bottom margin can collapse with last child's bottom margin
    ///
    /// CSS 2.1 Section 8.3.1: The bottom margin of an in-flow block box with
    /// `height: auto` collapses with its last in-flow block-level child's
    /// bottom margin if the box has no bottom padding and no bottom border.
    ///
    /// # Arguments
    ///
    /// * `parent_border_bottom` - Parent's bottom border width
    /// * `parent_padding_bottom` - Parent's bottom padding
    /// * `parent_has_auto_height` - Whether parent's height is `auto`
    ///
    /// # Returns
    ///
    /// `true` if the margins can collapse, `false` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::contexts::block::margin_collapse::ParentChildCollapseRules;
    ///
    /// // No border/padding, auto height - can collapse
    /// assert!(ParentChildCollapseRules::can_collapse_bottom_margins(0.0, 0.0, true));
    ///
    /// // Explicit height prevents collapse
    /// assert!(!ParentChildCollapseRules::can_collapse_bottom_margins(0.0, 0.0, false));
    ///
    /// // Border prevents collapse
    /// assert!(!ParentChildCollapseRules::can_collapse_bottom_margins(1.0, 0.0, true));
    /// ```
    pub fn can_collapse_bottom_margins(
        parent_border_bottom: f32,
        parent_padding_bottom: f32,
        parent_has_auto_height: bool,
    ) -> bool {
        parent_border_bottom == 0.0 && parent_padding_bottom == 0.0 && parent_has_auto_height
    }

    /// Checks if an element's own top and bottom margins can collapse
    ///
    /// CSS 2.1 Section 8.3.1: An element's own top and bottom margins collapse
    /// if the element:
    /// - Has zero height
    /// - Has no vertical padding
    /// - Has no vertical border
    /// - Has no in-flow children
    /// - Doesn't establish a new BFC
    ///
    /// # Arguments
    ///
    /// * `height` - Element's computed height (0 for empty)
    /// * `min_height` - Element's min-height
    /// * `padding_top` - Top padding
    /// * `padding_bottom` - Bottom padding
    /// * `border_top` - Top border width
    /// * `border_bottom` - Bottom border width
    /// * `has_in_flow_children` - Whether element has in-flow children
    /// * `establishes_bfc` - Whether element establishes a new BFC
    ///
    /// # Returns
    ///
    /// `true` if top and bottom margins can collapse through the element.
    pub fn can_collapse_through(
        height: Option<f32>,
        min_height: f32,
        padding_top: f32,
        padding_bottom: f32,
        border_top: f32,
        border_bottom: f32,
        has_in_flow_children: bool,
        establishes_bfc: bool,
    ) -> bool {
        // Height must be auto or zero
        let height_allows = match height {
            None => true, // auto
            Some(h) => h == 0.0,
        };

        height_allows
            && min_height == 0.0
            && padding_top == 0.0
            && padding_bottom == 0.0
            && border_top == 0.0
            && border_bottom == 0.0
            && !has_in_flow_children
            && !establishes_bfc
    }
}

/// Checks if a box establishes a new Block Formatting Context
///
/// A new BFC is established by elements that create isolated layout contexts.
/// Margins don't collapse across BFC boundaries.
///
/// # CSS 2.1 Section 9.4.1
///
/// BFC is established by:
/// - Floats (float != none)
/// - Absolutely positioned elements (position: absolute or fixed)
/// - Inline-blocks
/// - Table cells
/// - Table captions
/// - Elements with overflow != visible
/// - Flex items
/// - Grid items
///
/// # Arguments
///
/// * `box_node` - The box to check
///
/// # Returns
///
/// `true` if the box establishes a new BFC.
pub fn establishes_bfc(box_node: &BoxNode) -> bool {
    // Check box type
    match &box_node.box_type {
        BoxType::Block(block) => {
            // Block with non-block formatting context establishes BFC
            // (e.g., flex, grid containers)
            !matches!(block.formatting_context, FormattingContextType::Block)
        }
        BoxType::Inline(inline) => {
            // Inline-block establishes BFC
            inline.formatting_context.is_some()
        }
        BoxType::Replaced(_) => {
            // Replaced elements don't establish BFC in the CSS sense
            // (they're atomic inline-level boxes)
            false
        }
        BoxType::Text(_) => false,
        BoxType::Anonymous(_) => false,
    }
    // Note: Additional checks for overflow, float, and position
    // would require access to computed styles. These are handled
    // by the block layout algorithm when checking collapse conditions.
}

/// Additional BFC establishment checks based on computed style
///
/// These checks complement `establishes_bfc` when style information is available.
///
/// # Arguments
///
/// * `overflow_x` - Computed overflow-x value
/// * `overflow_y` - Computed overflow-y value
/// * `float` - Computed float value
/// * `position` - Computed position value
///
/// # Returns
///
/// `true` if any of these properties cause a new BFC to be established.
pub fn style_establishes_bfc(
    overflow_x: Overflow,
    overflow_y: Overflow,
    float: Float,
    position: crate::style::Position,
) -> bool {
    // Overflow != visible establishes BFC
    let overflow_creates_bfc = !matches!(overflow_x, Overflow::Visible) || !matches!(overflow_y, Overflow::Visible);

    // Float establishes BFC
    let float_creates_bfc = !matches!(float, Float::None);

    // Absolute/fixed positioning establishes BFC
    let position_creates_bfc = matches!(
        position,
        crate::style::Position::Absolute | crate::style::Position::Fixed
    );

    overflow_creates_bfc || float_creates_bfc || position_creates_bfc
}

/// Overflow property values
///
/// CSS 2.1 Section 11.1
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Overflow {
    /// Content clips at padding box, no scrollbars
    #[default]
    Visible,
    /// Content clips at padding box, no scrollbars
    Hidden,
    /// Always show scrollbars
    Scroll,
    /// Show scrollbars only when needed
    Auto,
}

/// Float property values
///
/// CSS 2.1 Section 9.5
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Float {
    /// Not floated
    #[default]
    None,
    /// Float to the left
    Left,
    /// Float to the right
    Right,
}

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // CollapsibleMargin Tests
    // =========================================================================

    #[test]
    fn test_collapsible_margin_zero() {
        let m = CollapsibleMargin::ZERO;
        assert_eq!(m.positive, 0.0);
        assert_eq!(m.negative, 0.0);
        assert_eq!(m.resolve(), 0.0);
        assert!(m.is_zero());
    }

    #[test]
    fn test_collapsible_margin_from_positive() {
        let m = CollapsibleMargin::from_margin(20.0);
        assert_eq!(m.positive, 20.0);
        assert_eq!(m.negative, 0.0);
        assert_eq!(m.resolve(), 20.0);
        assert!(!m.is_zero());
    }

    #[test]
    fn test_collapsible_margin_from_negative() {
        let m = CollapsibleMargin::from_margin(-15.0);
        assert_eq!(m.positive, 0.0);
        assert_eq!(m.negative, 15.0);
        assert_eq!(m.resolve(), -15.0);
        assert!(!m.is_zero());
    }

    #[test]
    fn test_collapsible_margin_from_zero() {
        let m = CollapsibleMargin::from_margin(0.0);
        assert!(m.is_zero());
        assert_eq!(m.resolve(), 0.0);
    }

    #[test]
    fn test_positive_margins_collapse_to_max() {
        let m1 = CollapsibleMargin::from_margin(20.0);
        let m2 = CollapsibleMargin::from_margin(30.0);
        let result = m1.collapse_with(m2);

        assert_eq!(result.positive, 30.0);
        assert_eq!(result.negative, 0.0);
        assert_eq!(result.resolve(), 30.0);
    }

    #[test]
    fn test_negative_margins_collapse_to_most_negative() {
        let m1 = CollapsibleMargin::from_margin(-20.0);
        let m2 = CollapsibleMargin::from_margin(-30.0);
        let result = m1.collapse_with(m2);

        assert_eq!(result.positive, 0.0);
        assert_eq!(result.negative, 30.0);
        assert_eq!(result.resolve(), -30.0);
    }

    #[test]
    fn test_mixed_margins_sum() {
        let m1 = CollapsibleMargin::from_margin(30.0);
        let m2 = CollapsibleMargin::from_margin(-10.0);
        let result = m1.collapse_with(m2);

        assert_eq!(result.positive, 30.0);
        assert_eq!(result.negative, 10.0);
        assert_eq!(result.resolve(), 20.0); // 30 - 10 = 20
    }

    #[test]
    fn test_mixed_margins_negative_result() {
        let m1 = CollapsibleMargin::from_margin(10.0);
        let m2 = CollapsibleMargin::from_margin(-30.0);
        let result = m1.collapse_with(m2);

        assert_eq!(result.resolve(), -20.0); // 10 - 30 = -20
    }

    #[test]
    fn test_multiple_collapse() {
        // Three margins: 10, 20, 30 should collapse to 30
        let m1 = CollapsibleMargin::from_margin(10.0);
        let m2 = CollapsibleMargin::from_margin(20.0);
        let m3 = CollapsibleMargin::from_margin(30.0);

        let result = m1.collapse_with(m2).collapse_with(m3);
        assert_eq!(result.resolve(), 30.0);
    }

    #[test]
    fn test_multiple_mixed_collapse() {
        // Mix: 20, -10, 30, -5 should give max(20,30) - max(10,5) = 30 - 10 = 20
        let m1 = CollapsibleMargin::from_margin(20.0);
        let m2 = CollapsibleMargin::from_margin(-10.0);
        let m3 = CollapsibleMargin::from_margin(30.0);
        let m4 = CollapsibleMargin::from_margin(-5.0);

        let result = m1.collapse_with(m2).collapse_with(m3).collapse_with(m4);
        assert_eq!(result.resolve(), 20.0);
    }

    #[test]
    fn test_add_margin() {
        let m = CollapsibleMargin::from_margin(20.0);
        let m2 = m.add_margin(30.0);
        assert_eq!(m2.resolve(), 30.0);

        let m3 = m2.add_margin(-10.0);
        assert_eq!(m3.resolve(), 20.0);
    }

    #[test]
    fn test_collapse_with_zero() {
        let m1 = CollapsibleMargin::from_margin(25.0);
        let m2 = CollapsibleMargin::ZERO;
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), 25.0);
    }

    #[test]
    fn test_display() {
        let m1 = CollapsibleMargin::from_margin(20.0);
        assert!(format!("{}", m1).contains("+20"));

        let m2 = CollapsibleMargin::from_margin(-15.0);
        assert!(format!("{}", m2).contains("-15"));

        let m3 = CollapsibleMargin::new(30.0, 10.0);
        let display = format!("{}", m3);
        assert!(display.contains("+30"));
        assert!(display.contains("-10"));
    }

    // =========================================================================
    // MarginCollapseContext Tests
    // =========================================================================

    #[test]
    fn test_context_new() {
        let ctx = MarginCollapseContext::new();
        assert!(ctx.is_at_start());
        assert!(ctx.pending_margin().is_zero());
    }

    #[test]
    fn test_context_first_child() {
        let mut ctx = MarginCollapseContext::new();

        // First child with margin-top: 20px
        let (offset, _) = ctx.process_child_margins(20.0, 0.0, false);
        assert_eq!(offset, 20.0);
        assert!(!ctx.is_at_start());
    }

    #[test]
    fn test_context_sibling_collapse() {
        let mut ctx = MarginCollapseContext::new();

        // First child: margin-bottom: 20px
        let (offset1, _) = ctx.process_child_margins(0.0, 20.0, false);
        assert_eq!(offset1, 0.0); // No top margin on first child

        // Second child: margin-top: 30px (should collapse with 20px)
        let (offset2, _) = ctx.process_child_margins(30.0, 0.0, false);
        assert_eq!(offset2, 30.0); // max(20, 30) = 30
    }

    #[test]
    fn test_context_sibling_no_collapse_smaller() {
        let mut ctx = MarginCollapseContext::new();

        // First child: margin-bottom: 40px
        ctx.process_child_margins(0.0, 40.0, false);

        // Second child: margin-top: 25px
        let (offset, _) = ctx.process_child_margins(25.0, 0.0, false);
        assert_eq!(offset, 40.0); // max(40, 25) = 40
    }

    #[test]
    fn test_context_empty_block() {
        let mut ctx = MarginCollapseContext::new();

        // Empty block with margins 20 and 30
        let (offset, pending) = ctx.process_child_margins(20.0, 30.0, true);

        // Empty block contributes 0 to positioning
        assert_eq!(offset, 0.0);

        // But pending margin should be max(20, 30) = 30
        assert_eq!(pending.resolve(), 30.0);
        assert!(ctx.is_at_start()); // Still at start (empty block doesn't count)
    }

    #[test]
    fn test_context_multiple_empty_blocks() {
        let mut ctx = MarginCollapseContext::new();

        // Three consecutive empty blocks
        ctx.process_child_margins(10.0, 15.0, true);
        ctx.process_child_margins(25.0, 5.0, true);
        let (_, pending) = ctx.process_child_margins(20.0, 30.0, true);

        // All should collapse together: max(10,15,25,5,20,30) = 30
        assert_eq!(pending.resolve(), 30.0);
    }

    #[test]
    fn test_context_empty_block_then_content() {
        let mut ctx = MarginCollapseContext::new();

        // Empty block with margin: 20px
        ctx.process_child_margins(20.0, 20.0, true);

        // Non-empty block with margin-top: 10px
        let (offset, _) = ctx.process_child_margins(10.0, 0.0, false);

        // max(20, 10) = 20 (empty block's margins collapse with non-empty's top)
        assert_eq!(offset, 20.0);
    }

    #[test]
    fn test_context_with_initial_margin() {
        let mut ctx = MarginCollapseContext::with_initial_margin(15.0);

        // Child with margin-top: 25px
        let (offset, _) = ctx.process_child_margins(25.0, 0.0, false);

        // Collapses with initial: max(15, 25) = 25
        assert_eq!(offset, 25.0);
    }

    #[test]
    fn test_context_consume_pending() {
        let mut ctx = MarginCollapseContext::new();

        ctx.add_margin(30.0);
        let consumed = ctx.consume_pending();

        assert_eq!(consumed, 30.0);
        assert!(ctx.pending_margin().is_zero());
    }

    #[test]
    fn test_context_clearance() {
        let mut ctx = MarginCollapseContext::new();

        // First child with margin-bottom: 20px
        ctx.process_child_margins(0.0, 20.0, false);

        // Second child with clearance, margin-top: 30px
        let (offset, _) = ctx.process_child_with_clearance(50.0, 30.0, 10.0, false);

        // Offset should be: pending(20) + clearance(50) + margin_top(30) = 100
        assert_eq!(offset, 100.0);
    }

    // =========================================================================
    // ParentChildCollapseRules Tests
    // =========================================================================

    #[test]
    fn test_top_margin_collapse_allowed() {
        assert!(ParentChildCollapseRules::can_collapse_top_margins(0.0, 0.0, false));
    }

    #[test]
    fn test_top_margin_collapse_prevented_by_border() {
        assert!(!ParentChildCollapseRules::can_collapse_top_margins(1.0, 0.0, false));
    }

    #[test]
    fn test_top_margin_collapse_prevented_by_padding() {
        assert!(!ParentChildCollapseRules::can_collapse_top_margins(0.0, 1.0, false));
    }

    #[test]
    fn test_top_margin_collapse_prevented_by_clearance() {
        assert!(!ParentChildCollapseRules::can_collapse_top_margins(0.0, 0.0, true));
    }

    #[test]
    fn test_bottom_margin_collapse_allowed() {
        assert!(ParentChildCollapseRules::can_collapse_bottom_margins(0.0, 0.0, true));
    }

    #[test]
    fn test_bottom_margin_collapse_prevented_by_explicit_height() {
        assert!(!ParentChildCollapseRules::can_collapse_bottom_margins(0.0, 0.0, false));
    }

    #[test]
    fn test_bottom_margin_collapse_prevented_by_border() {
        assert!(!ParentChildCollapseRules::can_collapse_bottom_margins(1.0, 0.0, true));
    }

    #[test]
    fn test_bottom_margin_collapse_prevented_by_padding() {
        assert!(!ParentChildCollapseRules::can_collapse_bottom_margins(0.0, 1.0, true));
    }

    #[test]
    fn test_collapse_through_allowed() {
        assert!(ParentChildCollapseRules::can_collapse_through(
            None,  // auto height
            0.0,   // no min-height
            0.0,   // no padding-top
            0.0,   // no padding-bottom
            0.0,   // no border-top
            0.0,   // no border-bottom
            false, // no in-flow children
            false, // doesn't establish BFC
        ));
    }

    #[test]
    fn test_collapse_through_prevented_by_height() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            Some(50.0),
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            false,
            false,
        ));
    }

    #[test]
    fn test_collapse_through_prevented_by_min_height() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            None, 10.0, // min-height
            0.0, 0.0, 0.0, 0.0, false, false,
        ));
    }

    #[test]
    fn test_collapse_through_prevented_by_padding() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            None, 0.0, 1.0, // padding-top
            0.0, 0.0, 0.0, false, false,
        ));
    }

    #[test]
    fn test_collapse_through_prevented_by_children() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            None, 0.0, 0.0, 0.0, 0.0, 0.0, true, // has in-flow children
            false,
        ));
    }

    #[test]
    fn test_collapse_through_prevented_by_bfc() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            None, 0.0, 0.0, 0.0, 0.0, 0.0, false, true, // establishes BFC
        ));
    }

    // =========================================================================
    // BFC Detection Tests
    // =========================================================================

    #[test]
    fn test_style_establishes_bfc_overflow() {
        assert!(style_establishes_bfc(
            Overflow::Hidden,
            Overflow::Visible,
            Float::None,
            crate::style::Position::Static,
        ));
    }

    #[test]
    fn test_style_establishes_bfc_float() {
        assert!(style_establishes_bfc(
            Overflow::Visible,
            Overflow::Visible,
            Float::Left,
            crate::style::Position::Static,
        ));
    }

    #[test]
    fn test_style_establishes_bfc_absolute() {
        assert!(style_establishes_bfc(
            Overflow::Visible,
            Overflow::Visible,
            Float::None,
            crate::style::Position::Absolute,
        ));
    }

    #[test]
    fn test_style_establishes_bfc_fixed() {
        assert!(style_establishes_bfc(
            Overflow::Visible,
            Overflow::Visible,
            Float::None,
            crate::style::Position::Fixed,
        ));
    }

    #[test]
    fn test_style_no_bfc() {
        assert!(!style_establishes_bfc(
            Overflow::Visible,
            Overflow::Visible,
            Float::None,
            crate::style::Position::Static,
        ));
    }

    // =========================================================================
    // Edge Case Tests
    // =========================================================================

    #[test]
    fn test_extreme_positive_margin() {
        let m1 = CollapsibleMargin::from_margin(f32::MAX / 2.0);
        let m2 = CollapsibleMargin::from_margin(100.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), f32::MAX / 2.0);
    }

    #[test]
    fn test_extreme_negative_margin() {
        let m1 = CollapsibleMargin::from_margin(f32::MIN / 2.0);
        let m2 = CollapsibleMargin::from_margin(-100.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), f32::MIN / 2.0);
    }

    #[test]
    fn test_equal_opposite_margins() {
        let m1 = CollapsibleMargin::from_margin(50.0);
        let m2 = CollapsibleMargin::from_margin(-50.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), 0.0);
    }

    #[test]
    fn test_many_margins_collapse() {
        // Simulate a real scenario: 5 siblings collapsing
        let margins = [20.0, 30.0, -10.0, 25.0, -5.0];
        let mut result = CollapsibleMargin::ZERO;
        for m in margins {
            result = result.add_margin(m);
        }
        // max(20, 30, 25) = 30, max(10, 5) = 10, result = 30 - 10 = 20
        assert_eq!(result.resolve(), 20.0);
    }
}
