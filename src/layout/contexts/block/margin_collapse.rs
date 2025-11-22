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
//! # Collapse Algorithm
//!
//! - **All positive**: result = max of all margins
//! - **All negative**: result = min (most negative) of all margins
//! - **Mixed**: result = largest positive + most negative
//!
//! Reference: <https://www.w3.org/TR/CSS21/box.html#collapsing-margins>

use crate::style::{ComputedStyles, Position};

/// A collapsible margin that tracks positive and negative components separately
///
/// CSS 2.1 Section 8.3.1 specifies that when margins collapse:
/// - If all positive: result = max of all margins
/// - If all negative: result = min (most negative) of all margins
/// - If mixed: result = largest positive + most negative
///
/// By tracking positive and negative components separately, we can correctly
/// compute the collapsed value for any combination of margins.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct CollapsibleMargin {
    /// Largest positive margin value (0.0 if no positive margins)
    pub positive: f32,
    /// Most negative margin value, stored as absolute value (0.0 if no negative margins)
    pub negative: f32,
}

impl CollapsibleMargin {
    /// The zero margin constant
    pub const ZERO: Self = Self {
        positive: 0.0,
        negative: 0.0,
    };

    /// Creates a new collapsible margin with specified positive and negative components
    pub fn new(positive: f32, negative: f32) -> Self {
        debug_assert!(positive >= 0.0, "positive component must be >= 0");
        debug_assert!(negative >= 0.0, "negative component must be >= 0 (absolute value)");
        Self { positive, negative }
    }

    /// Creates a collapsible margin from a single margin value
    pub fn from_margin(value: f32) -> Self {
        if value >= 0.0 {
            Self {
                positive: value,
                negative: 0.0,
            }
        } else {
            Self {
                positive: 0.0,
                negative: -value,
            }
        }
    }

    /// Collapses this margin with another, returning the combined margin
    pub fn collapse_with(self, other: Self) -> Self {
        Self {
            positive: self.positive.max(other.positive),
            negative: self.negative.max(other.negative),
        }
    }

    /// Resolves the collapsible margin to a final pixel value
    pub fn resolve(self) -> f32 {
        self.positive - self.negative
    }

    /// Checks if this margin is effectively zero
    pub fn is_zero(self) -> bool {
        self.positive == 0.0 && self.negative == 0.0
    }

    /// Adds a margin value to this collapsible margin
    pub fn add_margin(self, value: f32) -> Self {
        self.collapse_with(Self::from_margin(value))
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

/// Context for tracking margin collapsing during block layout
///
/// This struct maintains state needed during the vertical traversal
/// of a block formatting context to properly collapse margins.
#[derive(Debug, Clone, Default)]
pub struct MarginCollapseContext {
    /// Pending margin that may collapse with the next element
    pending_margin: CollapsibleMargin,
    /// Whether we're at the start of the BFC (no content yet)
    at_start: bool,
}

impl MarginCollapseContext {
    /// Creates a new margin collapse context
    pub fn new() -> Self {
        Self {
            pending_margin: CollapsibleMargin::ZERO,
            at_start: true,
        }
    }

    /// Creates a context with an initial pending margin
    pub fn with_initial_margin(initial_margin: f32) -> Self {
        Self {
            pending_margin: CollapsibleMargin::from_margin(initial_margin),
            at_start: true,
        }
    }

    /// Adds a margin to the pending set
    pub fn push_margin(&mut self, margin: f32) {
        self.pending_margin = self.pending_margin.add_margin(margin);
    }

    /// Resolves and returns the collapsed margin, resetting pending state
    pub fn resolve(&mut self) -> f32 {
        let result = self.pending_margin.resolve();
        self.pending_margin = CollapsibleMargin::ZERO;
        self.at_start = false;
        result
    }

    /// Resolves margin and adds a new margin in one operation
    pub fn resolve_and_push(&mut self, new_margin: f32) -> f32 {
        let result = self.resolve();
        self.push_margin(new_margin);
        result
    }

    /// Processes a child box's margins and returns positioning information
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
            self.pending_margin = self.pending_margin.collapse_with(self_collapsed);
            (0.0, self.pending_margin)
        } else {
            // Non-empty block
            let collapsed_top = self.pending_margin.collapse_with(top_margin);
            let offset = collapsed_top.resolve();
            self.pending_margin = bottom_margin;
            self.at_start = false;
            (offset, self.pending_margin)
        }
    }

    /// Processes margins with clearance (breaks collapse chain)
    pub fn process_child_with_clearance(
        &mut self,
        clearance: f32,
        margin_top: f32,
        margin_bottom: f32,
        is_empty: bool,
    ) -> (f32, CollapsibleMargin) {
        let pending_offset = self.pending_margin.resolve();
        self.pending_margin = CollapsibleMargin::ZERO;

        if is_empty {
            let self_collapsed =
                CollapsibleMargin::from_margin(margin_top).collapse_with(CollapsibleMargin::from_margin(margin_bottom));
            self.pending_margin = self_collapsed;
            (pending_offset + clearance, self.pending_margin)
        } else {
            self.pending_margin = CollapsibleMargin::from_margin(margin_bottom);
            self.at_start = false;
            (pending_offset + clearance + margin_top, self.pending_margin)
        }
    }

    /// Returns the current pending collapsed margin
    pub fn pending_margin(&self) -> f32 {
        self.pending_margin.resolve()
    }

    /// Returns the current pending margin as CollapsibleMargin
    pub fn pending_collapsible_margin(&self) -> CollapsibleMargin {
        self.pending_margin
    }

    /// Checks if we're still at the start (no content encountered)
    pub fn is_at_start(&self) -> bool {
        self.at_start
    }

    /// Marks that we're no longer at the start
    pub fn mark_content_encountered(&mut self) {
        self.at_start = false;
    }

    /// Consumes the pending margin and resets it to zero
    pub fn consume_pending(&mut self) -> f32 {
        let value = self.pending_margin.resolve();
        self.pending_margin = CollapsibleMargin::ZERO;
        self.at_start = false;
        value
    }
}

/// Collapses two margin values according to CSS rules
///
/// Convenience function for simple two-margin collapse:
/// - Both positive: use the larger
/// - Both negative: use the more negative
/// - Mixed: add them (positive + negative)
pub fn collapse_margins(margin1: f32, margin2: f32) -> f32 {
    if margin1 >= 0.0 && margin2 >= 0.0 {
        margin1.max(margin2)
    } else if margin1 < 0.0 && margin2 < 0.0 {
        margin1.min(margin2)
    } else {
        margin1 + margin2
    }
}

/// Determines if a box's margins can collapse through it
///
/// A box is "empty" for margin collapsing purposes if:
/// - It has no in-flow content
/// - min-height is zero
/// - height is auto or zero
/// - No padding or border
pub fn is_margin_collapsible_through(style: &ComputedStyles) -> bool {
    if style.border_top_width.to_px() > 0.0 || style.border_bottom_width.to_px() > 0.0 {
        return false;
    }
    if style.padding_top.to_px() > 0.0 || style.padding_bottom.to_px() > 0.0 {
        return false;
    }
    if let Some(h) = &style.height {
        if h.to_px() > 0.0 {
            return false;
        }
    }
    if let Some(min_h) = &style.min_height {
        if min_h.to_px() > 0.0 {
            return false;
        }
    }
    true
}

/// Determines if margins should collapse between parent and first child
pub fn should_collapse_with_first_child(parent_style: &ComputedStyles) -> bool {
    if parent_style.border_top_width.to_px() > 0.0 {
        return false;
    }
    if parent_style.padding_top.to_px() > 0.0 {
        return false;
    }
    if establishes_bfc(parent_style) {
        return false;
    }
    true
}

/// Determines if margins should collapse between parent and last child
pub fn should_collapse_with_last_child(parent_style: &ComputedStyles) -> bool {
    if parent_style.border_bottom_width.to_px() > 0.0 {
        return false;
    }
    if parent_style.padding_bottom.to_px() > 0.0 {
        return false;
    }
    if parent_style.height.is_some() {
        return false;
    }
    if establishes_bfc(parent_style) {
        return false;
    }
    true
}

/// Determines if a box establishes a new block formatting context
pub fn establishes_bfc(style: &ComputedStyles) -> bool {
    use crate::style::{Display, Overflow};

    if style.position == Position::Absolute || style.position == Position::Fixed {
        return true;
    }
    if matches!(style.display, Display::InlineBlock) {
        return true;
    }
    if style.overflow_x != Overflow::Visible || style.overflow_y != Overflow::Visible {
        return true;
    }
    if matches!(style.display, Display::Flex | Display::Grid) {
        return true;
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::Length;

    // ==========================================================================
    // CollapsibleMargin Tests
    // ==========================================================================

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
    }

    #[test]
    fn test_collapsible_margin_from_negative() {
        let m = CollapsibleMargin::from_margin(-15.0);
        assert_eq!(m.positive, 0.0);
        assert_eq!(m.negative, 15.0);
        assert_eq!(m.resolve(), -15.0);
    }

    #[test]
    fn test_positive_margins_collapse_to_max() {
        let m1 = CollapsibleMargin::from_margin(20.0);
        let m2 = CollapsibleMargin::from_margin(30.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), 30.0);
    }

    #[test]
    fn test_negative_margins_collapse_to_most_negative() {
        let m1 = CollapsibleMargin::from_margin(-20.0);
        let m2 = CollapsibleMargin::from_margin(-30.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), -30.0);
    }

    #[test]
    fn test_mixed_margins_sum() {
        let m1 = CollapsibleMargin::from_margin(30.0);
        let m2 = CollapsibleMargin::from_margin(-10.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), 20.0);
    }

    #[test]
    fn test_mixed_margins_negative_result() {
        let m1 = CollapsibleMargin::from_margin(10.0);
        let m2 = CollapsibleMargin::from_margin(-30.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), -20.0);
    }

    #[test]
    fn test_add_margin() {
        let m = CollapsibleMargin::from_margin(20.0);
        let m2 = m.add_margin(30.0);
        assert_eq!(m2.resolve(), 30.0);
        let m3 = m2.add_margin(-10.0);
        assert_eq!(m3.resolve(), 20.0);
    }

    // ==========================================================================
    // MarginCollapseContext Tests
    // ==========================================================================

    #[test]
    fn test_context_new() {
        let ctx = MarginCollapseContext::new();
        assert!(ctx.is_at_start());
        assert_eq!(ctx.pending_margin(), 0.0);
    }

    #[test]
    fn test_context_push_resolve() {
        let mut ctx = MarginCollapseContext::new();
        ctx.push_margin(20.0);
        ctx.push_margin(30.0);
        assert_eq!(ctx.pending_margin(), 30.0);

        let resolved = ctx.resolve();
        assert_eq!(resolved, 30.0);
        assert_eq!(ctx.pending_margin(), 0.0);
        assert!(!ctx.is_at_start());
    }

    #[test]
    fn test_context_resolve_and_push() {
        let mut ctx = MarginCollapseContext::new();
        ctx.push_margin(20.0);

        let resolved = ctx.resolve_and_push(10.0);
        assert_eq!(resolved, 20.0);
        assert_eq!(ctx.pending_margin(), 10.0);
    }

    #[test]
    fn test_context_process_child_margins() {
        let mut ctx = MarginCollapseContext::new();

        // First child
        let (offset, _) = ctx.process_child_margins(20.0, 10.0, false);
        assert_eq!(offset, 20.0);

        // Second child - margins collapse
        let (offset2, _) = ctx.process_child_margins(30.0, 15.0, false);
        assert_eq!(offset2, 30.0); // max(10, 30)
    }

    #[test]
    fn test_context_empty_block() {
        let mut ctx = MarginCollapseContext::new();
        let (offset, pending) = ctx.process_child_margins(20.0, 30.0, true);
        assert_eq!(offset, 0.0);
        assert_eq!(pending.resolve(), 30.0);
    }

    #[test]
    fn test_context_with_initial_margin() {
        let mut ctx = MarginCollapseContext::with_initial_margin(15.0);
        let (offset, _) = ctx.process_child_margins(25.0, 0.0, false);
        assert_eq!(offset, 25.0); // max(15, 25)
    }

    // ==========================================================================
    // collapse_margins function tests
    // ==========================================================================

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

    // ==========================================================================
    // Style-based tests
    // ==========================================================================

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

    #[test]
    fn test_establishes_bfc_absolute() {
        let mut style = ComputedStyles::default();
        style.position = Position::Absolute;
        assert!(establishes_bfc(&style));
    }

    #[test]
    fn test_establishes_bfc_fixed() {
        let mut style = ComputedStyles::default();
        style.position = Position::Fixed;
        assert!(establishes_bfc(&style));
    }

    #[test]
    fn test_establishes_bfc_static() {
        let style = ComputedStyles::default();
        assert!(!establishes_bfc(&style));
    }
}
