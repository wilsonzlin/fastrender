//! Margin Collapsing Algorithm (CSS 2.1 Section 8.3.1)
//!
//! This module implements vertical margin collapsing for block formatting contexts.
//! Horizontal margins never collapse - only vertical margins between block-level boxes.
//!
//! # When Margins Collapse
//!
//! Two vertical margins are "adjoining" and collapse when:
//! - Both belong to in-flow block-level boxes in the same BFC
//! - No line boxes, clearance, padding, or borders separate them
//! - They are vertically adjacent (sibling margins, parent-child margins)
//!
//! # When Margins Do NOT Collapse
//!
//! - Root element margins
//! - Margins of floats and absolutely positioned elements
//! - Margins separated by padding or borders
//! - Margins on boxes that establish new BFCs (overflow != visible)
//! - Margins on inline-level boxes
//!
//! Reference: <https://www.w3.org/TR/CSS21/box.html#collapsing-margins>

use crate::style::ComputedStyles;
use crate::style::Position;

/// Represents margins that may participate in collapsing
///
/// This struct tracks margin values during layout and handles the
/// collapsing algorithm according to CSS 2.1 Section 8.3.1.
#[derive(Debug, Clone, Copy, Default)]
pub struct CollapsibleMargins {
    /// The most positive margin in the set
    pub max_positive: f32,
    /// The most negative margin in the set (stored as positive magnitude)
    pub max_negative: f32,
}

impl CollapsibleMargins {
    /// Creates a new empty collapsible margins set
    pub const fn new() -> Self {
        Self {
            max_positive: 0.0,
            max_negative: 0.0,
        }
    }

    /// Creates collapsible margins from a single margin value
    pub fn from_margin(margin: f32) -> Self {
        if margin >= 0.0 {
            Self {
                max_positive: margin,
                max_negative: 0.0,
            }
        } else {
            Self {
                max_positive: 0.0,
                max_negative: -margin,
            }
        }
    }

    /// Adds a margin to this collapsible set
    ///
    /// This implements the core margin collapsing algorithm:
    /// - If all margins are positive: result is the maximum
    /// - If all margins are negative: result is the most negative (absolute max)
    /// - If mixed: result is largest positive minus largest negative
    pub fn add_margin(&mut self, margin: f32) {
        if margin >= 0.0 {
            self.max_positive = self.max_positive.max(margin);
        } else {
            self.max_negative = self.max_negative.max(-margin);
        }
    }

    /// Collapses this margin set with another
    pub fn collapse_with(&mut self, other: &CollapsibleMargins) {
        self.max_positive = self.max_positive.max(other.max_positive);
        self.max_negative = self.max_negative.max(other.max_negative);
    }

    /// Computes the final collapsed margin value
    ///
    /// The collapsed margin is:
    /// - max(positives) if all margins are positive or zero
    /// - min(negatives) if all margins are negative
    /// - max(positives) + min(negatives) if mixed (note: min is negative)
    pub fn collapsed_value(&self) -> f32 {
        self.max_positive - self.max_negative
    }

    /// Returns true if this margin set is empty (no margins added)
    pub fn is_empty(&self) -> bool {
        self.max_positive == 0.0 && self.max_negative == 0.0
    }

    /// Clears this margin set
    pub fn clear(&mut self) {
        self.max_positive = 0.0;
        self.max_negative = 0.0;
    }
}

/// Context for tracking margin collapsing during block layout
///
/// This struct maintains state needed during the vertical traversal
/// of a block formatting context to properly collapse margins.
#[derive(Debug, Default)]
pub struct MarginCollapseContext {
    /// Pending positive margin (not yet resolved)
    pending_positive: f32,
    /// Pending negative margin (stored as positive magnitude)
    pending_negative: f32,
    /// Whether we're at the start of the BFC (no content yet)
    at_bfc_start: bool,
}

impl MarginCollapseContext {
    /// Creates a new margin collapse context
    pub fn new() -> Self {
        Self {
            pending_positive: 0.0,
            pending_negative: 0.0,
            at_bfc_start: true,
        }
    }

    /// Adds a top margin to the pending set
    ///
    /// Call this before laying out a child box.
    pub fn push_margin(&mut self, margin: f32) {
        if margin >= 0.0 {
            self.pending_positive = self.pending_positive.max(margin);
        } else {
            self.pending_negative = self.pending_negative.max(-margin);
        }
    }

    /// Resolves and returns the collapsed margin
    ///
    /// Call this when we encounter content that stops margin collapsing
    /// (e.g., after laying out a child with content, or when encountering
    /// padding/border).
    ///
    /// Returns the collapsed margin value and resets the pending state.
    pub fn resolve(&mut self) -> f32 {
        let result = self.pending_positive - self.pending_negative;
        self.pending_positive = 0.0;
        self.pending_negative = 0.0;
        self.at_bfc_start = false;
        result
    }

    /// Resolves margin and adds a new margin in one operation
    ///
    /// This is a common pattern: resolve the current pending margins,
    /// then start accumulating a new margin (e.g., the bottom margin
    /// of the child we just laid out).
    pub fn resolve_and_push(&mut self, new_margin: f32) -> f32 {
        let result = self.resolve();
        self.push_margin(new_margin);
        result
    }

    /// Returns the current pending collapsed margin without resolving
    pub fn pending_margin(&self) -> f32 {
        self.pending_positive - self.pending_negative
    }

    /// Checks if we're still at the BFC start (no content encountered)
    pub fn is_at_bfc_start(&self) -> bool {
        self.at_bfc_start
    }

    /// Marks that we're no longer at the BFC start
    pub fn mark_content_encountered(&mut self) {
        self.at_bfc_start = false;
    }
}

/// Determines if a box's margins can collapse through it
///
/// A box is "empty" for margin collapsing purposes if:
/// - It has no in-flow content
/// - min-height is zero
/// - height is auto or zero
/// - No padding or border
///
/// Empty boxes allow their top and bottom margins to collapse together.
pub fn is_margin_collapsible_through(style: &ComputedStyles) -> bool {
    // Check for border
    if style.border_top_width.to_px() > 0.0 || style.border_bottom_width.to_px() > 0.0 {
        return false;
    }

    // Check for padding
    if style.padding_top.to_px() > 0.0 || style.padding_bottom.to_px() > 0.0 {
        return false;
    }

    // Check for explicit height
    if let Some(h) = &style.height {
        if h.to_px() > 0.0 {
            return false;
        }
    }

    // Check for min-height
    if let Some(min_h) = &style.min_height {
        if min_h.to_px() > 0.0 {
            return false;
        }
    }

    true
}

/// Determines if margins should collapse between parent and first child
///
/// Parent's top margin and first child's top margin collapse if:
/// - Parent has no top border
/// - Parent has no top padding
/// - Parent doesn't establish a new BFC
/// - First child is in-flow
pub fn should_collapse_with_first_child(parent_style: &ComputedStyles) -> bool {
    // Border prevents collapsing
    if parent_style.border_top_width.to_px() > 0.0 {
        return false;
    }

    // Padding prevents collapsing
    if parent_style.padding_top.to_px() > 0.0 {
        return false;
    }

    // Establishing a new BFC prevents collapsing
    if establishes_bfc(parent_style) {
        return false;
    }

    true
}

/// Determines if margins should collapse between parent and last child
///
/// Parent's bottom margin and last child's bottom margin collapse if:
/// - Parent has no bottom border
/// - Parent has no bottom padding
/// - Parent has auto height
/// - Parent doesn't establish a new BFC
pub fn should_collapse_with_last_child(parent_style: &ComputedStyles) -> bool {
    // Border prevents collapsing
    if parent_style.border_bottom_width.to_px() > 0.0 {
        return false;
    }

    // Padding prevents collapsing
    if parent_style.padding_bottom.to_px() > 0.0 {
        return false;
    }

    // Explicit height prevents bottom margin collapsing
    if parent_style.height.is_some() {
        return false;
    }

    // Establishing a new BFC prevents collapsing
    if establishes_bfc(parent_style) {
        return false;
    }

    true
}

/// Determines if a box establishes a new block formatting context
///
/// Elements that establish a new BFC include:
/// - Floats
/// - Absolutely positioned elements
/// - Inline-blocks
/// - Table cells, table captions
/// - Elements with overflow != visible
/// - Flex and grid items
fn establishes_bfc(style: &ComputedStyles) -> bool {
    use crate::style::Display;

    // Floats and positioned elements establish BFC
    if style.position == Position::Absolute || style.position == Position::Fixed {
        return true;
    }

    // Inline-block establishes BFC
    if matches!(style.display, Display::InlineBlock) {
        return true;
    }

    // Overflow != visible establishes BFC
    use crate::style::Overflow;
    if style.overflow_x != Overflow::Visible || style.overflow_y != Overflow::Visible {
        return true;
    }

    // Flex and grid containers establish BFC for their contents
    if matches!(style.display, Display::Flex | Display::Grid) {
        return true;
    }

    false
}

/// Collapses two margin values according to CSS rules
///
/// This is a convenience function for simple two-margin collapse:
/// - Both positive: use the larger
/// - Both negative: use the more negative
/// - Mixed: add them (positive + negative)
pub fn collapse_margins(margin1: f32, margin2: f32) -> f32 {
    if margin1 >= 0.0 && margin2 >= 0.0 {
        // Both positive: take the max
        margin1.max(margin2)
    } else if margin1 < 0.0 && margin2 < 0.0 {
        // Both negative: take the more negative (min)
        margin1.min(margin2)
    } else {
        // Mixed: add them
        margin1 + margin2
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::Length;

    // CollapsibleMargins tests
    #[test]
    fn test_collapsible_margins_new() {
        let margins = CollapsibleMargins::new();
        assert_eq!(margins.collapsed_value(), 0.0);
        assert!(margins.is_empty());
    }

    #[test]
    fn test_collapsible_margins_from_positive() {
        let margins = CollapsibleMargins::from_margin(20.0);
        assert_eq!(margins.collapsed_value(), 20.0);
    }

    #[test]
    fn test_collapsible_margins_from_negative() {
        let margins = CollapsibleMargins::from_margin(-15.0);
        assert_eq!(margins.collapsed_value(), -15.0);
    }

    #[test]
    fn test_collapsible_margins_positive_collapse() {
        let mut margins = CollapsibleMargins::new();
        margins.add_margin(10.0);
        margins.add_margin(20.0);
        margins.add_margin(15.0);
        // All positive: result is max = 20
        assert_eq!(margins.collapsed_value(), 20.0);
    }

    #[test]
    fn test_collapsible_margins_negative_collapse() {
        let mut margins = CollapsibleMargins::new();
        margins.add_margin(-10.0);
        margins.add_margin(-20.0);
        margins.add_margin(-5.0);
        // All negative: result is most negative = -20
        assert_eq!(margins.collapsed_value(), -20.0);
    }

    #[test]
    fn test_collapsible_margins_mixed_collapse() {
        let mut margins = CollapsibleMargins::new();
        margins.add_margin(30.0);
        margins.add_margin(-10.0);
        // Mixed: max positive - max negative = 30 - 10 = 20
        assert_eq!(margins.collapsed_value(), 20.0);
    }

    #[test]
    fn test_collapsible_margins_mixed_negative_wins() {
        let mut margins = CollapsibleMargins::new();
        margins.add_margin(10.0);
        margins.add_margin(-30.0);
        // Mixed: 10 - 30 = -20
        assert_eq!(margins.collapsed_value(), -20.0);
    }

    #[test]
    fn test_collapsible_margins_collapse_with() {
        let mut margins1 = CollapsibleMargins::from_margin(20.0);
        let margins2 = CollapsibleMargins::from_margin(30.0);
        margins1.collapse_with(&margins2);
        assert_eq!(margins1.collapsed_value(), 30.0);
    }

    #[test]
    fn test_collapsible_margins_clear() {
        let mut margins = CollapsibleMargins::from_margin(20.0);
        margins.clear();
        assert!(margins.is_empty());
        assert_eq!(margins.collapsed_value(), 0.0);
    }

    // MarginCollapseContext tests
    #[test]
    fn test_margin_collapse_context_new() {
        let ctx = MarginCollapseContext::new();
        assert!(ctx.is_at_bfc_start());
        assert_eq!(ctx.pending_margin(), 0.0);
    }

    #[test]
    fn test_margin_collapse_context_push_resolve() {
        let mut ctx = MarginCollapseContext::new();
        ctx.push_margin(20.0);
        ctx.push_margin(30.0);
        assert_eq!(ctx.pending_margin(), 30.0);

        let resolved = ctx.resolve();
        assert_eq!(resolved, 30.0);
        assert_eq!(ctx.pending_margin(), 0.0);
        assert!(!ctx.is_at_bfc_start());
    }

    #[test]
    fn test_margin_collapse_context_resolve_and_push() {
        let mut ctx = MarginCollapseContext::new();
        ctx.push_margin(20.0);

        let resolved = ctx.resolve_and_push(10.0);
        assert_eq!(resolved, 20.0);
        assert_eq!(ctx.pending_margin(), 10.0);
    }

    // collapse_margins function tests
    #[test]
    fn test_collapse_margins_both_positive() {
        assert_eq!(collapse_margins(10.0, 20.0), 20.0);
        assert_eq!(collapse_margins(30.0, 15.0), 30.0);
    }

    #[test]
    fn test_collapse_margins_both_negative() {
        assert_eq!(collapse_margins(-10.0, -20.0), -20.0);
        assert_eq!(collapse_margins(-30.0, -5.0), -30.0);
    }

    #[test]
    fn test_collapse_margins_mixed() {
        assert_eq!(collapse_margins(30.0, -10.0), 20.0);
        assert_eq!(collapse_margins(-30.0, 10.0), -20.0);
    }

    #[test]
    fn test_collapse_margins_zero() {
        assert_eq!(collapse_margins(0.0, 0.0), 0.0);
        assert_eq!(collapse_margins(0.0, 10.0), 10.0);
        assert_eq!(collapse_margins(10.0, 0.0), 10.0);
    }

    // Style-based tests
    #[test]
    fn test_should_collapse_with_first_child_no_border_padding() {
        let style = ComputedStyles::default();
        assert!(should_collapse_with_first_child(&style));
    }

    #[test]
    fn test_should_collapse_with_first_child_has_border() {
        let mut style = ComputedStyles::default();
        style.border_top_width = Length::px(1.0);
        assert!(!should_collapse_with_first_child(&style));
    }

    #[test]
    fn test_should_collapse_with_first_child_has_padding() {
        let mut style = ComputedStyles::default();
        style.padding_top = Length::px(10.0);
        assert!(!should_collapse_with_first_child(&style));
    }

    #[test]
    fn test_should_collapse_with_last_child_no_border_padding() {
        let style = ComputedStyles::default();
        assert!(should_collapse_with_last_child(&style));
    }

    #[test]
    fn test_should_collapse_with_last_child_has_border() {
        let mut style = ComputedStyles::default();
        style.border_bottom_width = Length::px(1.0);
        assert!(!should_collapse_with_last_child(&style));
    }

    #[test]
    fn test_should_collapse_with_last_child_explicit_height() {
        let mut style = ComputedStyles::default();
        style.height = Some(Length::px(100.0));
        assert!(!should_collapse_with_last_child(&style));
    }

    #[test]
    fn test_is_margin_collapsible_through_empty() {
        let style = ComputedStyles::default();
        assert!(is_margin_collapsible_through(&style));
    }

    #[test]
    fn test_is_margin_collapsible_through_has_border() {
        let mut style = ComputedStyles::default();
        style.border_top_width = Length::px(1.0);
        assert!(!is_margin_collapsible_through(&style));
    }

    #[test]
    fn test_is_margin_collapsible_through_has_height() {
        let mut style = ComputedStyles::default();
        style.height = Some(Length::px(50.0));
        assert!(!is_margin_collapsible_through(&style));
    }
}
