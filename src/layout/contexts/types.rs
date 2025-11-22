//! Formatting context type determination
//!
//! This module determines what kind of formatting context a box establishes
//! based on its display type and other properties.

use std::fmt;

/// Types of formatting contexts
///
/// A formatting context is an environment in which boxes are laid out.
/// Different formatting contexts have different layout rules.
///
/// # Formatting Context Types
///
/// ## Block Formatting Context (BFC)
///
/// In a BFC, block-level boxes are laid out vertically, one after another.
/// Margins collapse between siblings. This is the default for most block
/// containers.
///
/// **Established by:**
/// - Block containers that are not inline-block
/// - Elements with `overflow` other than `visible`
/// - Floated elements
/// - Absolutely positioned elements
///
/// **Reference:** CSS 2.1 Section 9.4.1
///
/// ## Inline Formatting Context (IFC)
///
/// In an IFC, inline-level boxes are laid out horizontally within lines.
/// Text wraps at line boundaries. Line boxes are stacked vertically.
///
/// **Established by:**
/// - Block containers containing inline-level children
///
/// **Reference:** CSS 2.1 Section 9.4.2
///
/// ## Flex Formatting Context
///
/// In a flex FC, flex items are laid out using the flexbox algorithm.
/// Items can be arranged in rows or columns, with flexible sizing.
///
/// **Established by:**
/// - Elements with `display: flex` or `display: inline-flex`
///
/// **Reference:** CSS Flexbox Module Level 1
///
/// ## Grid Formatting Context
///
/// In a grid FC, grid items are positioned in a 2D grid of rows and columns.
/// Items can span multiple cells.
///
/// **Established by:**
/// - Elements with `display: grid` or `display: inline-grid`
///
/// **Reference:** CSS Grid Layout Module Level 1
///
/// ## Table Formatting Context
///
/// In a table FC, children are laid out as table rows, columns, and cells.
/// Table-specific sizing and alignment rules apply.
///
/// **Established by:**
/// - Elements with `display: table`, `table-row`, `table-cell`, etc.
///
/// **Reference:** CSS 2.1 Section 17
///
/// # Examples
///
/// ```
/// use fastrender::layout::FormattingContextType;
///
/// let fc_type = FormattingContextType::BlockFormatting;
/// assert!(fc_type.is_bfc());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FormattingContextType {
    /// Block Formatting Context (BFC)
    ///
    /// Block-level boxes stack vertically, margins collapse.
    BlockFormatting,

    /// Inline Formatting Context (IFC)
    ///
    /// Inline-level boxes flow horizontally within lines.
    InlineFormatting,

    /// Flex Formatting Context
    ///
    /// Children laid out using flexbox algorithm.
    FlexFormatting,

    /// Grid Formatting Context
    ///
    /// Children positioned in 2D grid.
    GridFormatting,

    /// Table Formatting Context
    ///
    /// Table-specific layout rules apply.
    TableFormatting,
}

impl fmt::Display for FormattingContextType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BlockFormatting => write!(f, "Block FC"),
            Self::InlineFormatting => write!(f, "Inline FC"),
            Self::FlexFormatting => write!(f, "Flex FC"),
            Self::GridFormatting => write!(f, "Grid FC"),
            Self::TableFormatting => write!(f, "Table FC"),
        }
    }
}

impl FormattingContextType {
    /// Returns true if this is a block formatting context
    ///
    /// BFCs have specific behaviors:
    /// - Vertical margin collapsing
    /// - Float containment
    /// - Prevent margin collapse with children
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::FormattingContextType;
    ///
    /// assert!(FormattingContextType::BlockFormatting.is_bfc());
    /// assert!(!FormattingContextType::InlineFormatting.is_bfc());
    /// ```
    pub fn is_bfc(&self) -> bool {
        matches!(self, Self::BlockFormatting)
    }

    /// Returns true if this is an inline formatting context
    pub fn is_ifc(&self) -> bool {
        matches!(self, Self::InlineFormatting)
    }

    /// Returns true if this is a flex formatting context
    pub fn is_flex(&self) -> bool {
        matches!(self, Self::FlexFormatting)
    }

    /// Returns true if this is a grid formatting context
    pub fn is_grid(&self) -> bool {
        matches!(self, Self::GridFormatting)
    }

    /// Returns true if this is a table formatting context
    pub fn is_table(&self) -> bool {
        matches!(self, Self::TableFormatting)
    }

    /// Returns true if this FC can contain block-level boxes
    ///
    /// BFC and Table FC can contain block-level boxes.
    /// IFC, Flex, and Grid treat all children as their respective item types.
    pub fn can_contain_blocks(&self) -> bool {
        matches!(self, Self::BlockFormatting | Self::TableFormatting)
    }

    /// Returns true if this FC requires line breaking
    ///
    /// Only IFC performs line breaking. Other FCs use different layout rules.
    pub fn requires_line_breaking(&self) -> bool {
        matches!(self, Self::InlineFormatting)
    }

    /// Returns true if this FC supports flexible sizing
    ///
    /// Flex and Grid support flexible item sizing.
    pub fn supports_flexible_sizing(&self) -> bool {
        matches!(self, Self::FlexFormatting | Self::GridFormatting)
    }

    /// Returns true if children are laid out in the block axis
    ///
    /// In BFC, children stack vertically (block axis).
    /// In IFC, children flow horizontally (inline axis).
    /// Flex and Grid depend on flex-direction and grid-auto-flow.
    pub fn is_block_axis_primary(&self) -> bool {
        matches!(self, Self::BlockFormatting | Self::TableFormatting)
    }

    /// Returns a short abbreviation for debugging
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::FormattingContextType;
    ///
    /// assert_eq!(FormattingContextType::BlockFormatting.abbrev(), "BFC");
    /// assert_eq!(FormattingContextType::FlexFormatting.abbrev(), "Flex");
    /// ```
    pub fn abbrev(&self) -> &'static str {
        match self {
            Self::BlockFormatting => "BFC",
            Self::InlineFormatting => "IFC",
            Self::FlexFormatting => "Flex",
            Self::GridFormatting => "Grid",
            Self::TableFormatting => "Table",
        }
    }
}

use crate::style::Display;

/// Derives the formatting context type from the display property
///
/// This determines what kind of formatting context an element establishes
/// based on its `display` value.
///
/// # Display to FC Mapping
///
/// - `display: block` → Block FC
/// - `display: flow-root` → Block FC (always establishes BFC)
/// - `display: inline` → Does NOT establish FC (participates in parent's IFC)
/// - `display: flex` / `inline-flex` → Flex FC
/// - `display: grid` / `inline-grid` → Grid FC
/// - `display: table` → Table FC
/// - `display: none` → No box generated
/// - `display: contents` → No principal box (children participate in parent FC)
///
/// # Note on Inline
///
/// Regular `display: inline` elements do NOT establish a formatting context.
/// They participate in their parent's inline formatting context. Only when
/// a block container contains inline children does it establish an IFC.
///
/// # Examples
///
/// ```
/// use fastrender::style::Display;
/// use fastrender::layout::contexts::types::derive_fc_from_display;
/// use fastrender::layout::FormattingContextType;
///
/// let fc = derive_fc_from_display(&Display::FlowRoot);
/// assert_eq!(fc, Some(FormattingContextType::BlockFormatting));
///
/// let fc = derive_fc_from_display(&Display::Flex);
/// assert_eq!(fc, Some(FormattingContextType::FlexFormatting));
/// ```
pub fn derive_fc_from_display(display: &Display) -> Option<FormattingContextType> {
    match display {
        // Block and flow-root establish BFC
        Display::Block | Display::FlowRoot => Some(FormattingContextType::BlockFormatting),

        // Inline doesn't establish FC (participates in parent's IFC)
        Display::Inline => None,

        // Inline-block establishes BFC
        Display::InlineBlock => Some(FormattingContextType::BlockFormatting),

        // Flex and inline-flex establish flex FC
        Display::Flex | Display::InlineFlex => Some(FormattingContextType::FlexFormatting),

        // Grid and inline-grid establish grid FC
        Display::Grid | Display::InlineGrid => Some(FormattingContextType::GridFormatting),

        // Table establishes table FC
        Display::Table | Display::InlineTable => Some(FormattingContextType::TableFormatting),

        // Table-internal don't establish independent FCs
        Display::TableRow
        | Display::TableCell
        | Display::TableRowGroup
        | Display::TableHeaderGroup
        | Display::TableFooterGroup
        | Display::TableColumn
        | Display::TableColumnGroup
        | Display::TableCaption => None,

        // List item is like block
        Display::ListItem => Some(FormattingContextType::BlockFormatting),

        // None means no box generated
        Display::None => None,

        // Contents means no principal box
        Display::Contents => None,
    }
}

/// Additional properties that can trigger BFC establishment
///
/// Even `display: block` elements establish independent BFCs in certain cases.
/// This struct encapsulates those properties.
///
/// Reference: CSS 2.1 Section 9.4.1 - "Floats, absolutely positioned elements,
/// block containers that are not block boxes, and block boxes with 'overflow'
/// other than 'visible' establish new block formatting contexts."
#[derive(Debug, Clone, Copy)]
pub struct BfcTriggers {
    /// overflow: hidden, scroll, auto, or clip
    pub has_overflow: bool,

    /// float: left or right
    pub is_floated: bool,

    /// position: absolute or fixed
    pub is_absolutely_positioned: bool,

    /// display: inline-block
    pub is_inline_block: bool,

    /// display: flow-root (explicit BFC)
    pub is_flow_root: bool,
}

impl Default for BfcTriggers {
    fn default() -> Self {
        Self {
            has_overflow: false,
            is_floated: false,
            is_absolutely_positioned: false,
            is_inline_block: false,
            is_flow_root: false,
        }
    }
}

impl BfcTriggers {
    /// Returns true if any trigger is active
    ///
    /// If true, the element establishes an independent BFC even with
    /// `display: block`.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::contexts::types::BfcTriggers;
    ///
    /// let mut triggers = BfcTriggers::default();
    /// assert!(!triggers.triggers_bfc());
    ///
    /// triggers.has_overflow = true;
    /// assert!(triggers.triggers_bfc());
    /// ```
    pub fn triggers_bfc(&self) -> bool {
        self.has_overflow
            || self.is_floated
            || self.is_absolutely_positioned
            || self.is_inline_block
            || self.is_flow_root
    }
}

/// Determines if an element establishes an independent BFC
///
/// This is more nuanced than just checking display. Several properties
/// can cause a block box to establish an independent BFC.
///
/// # Arguments
///
/// * `display` - The display value
/// * `triggers` - Additional BFC-triggering properties
///
/// # Returns
///
/// `true` if the element establishes an independent BFC
///
/// # Examples
///
/// ```
/// use fastrender::style::Display;
/// use fastrender::layout::contexts::types::{establishes_independent_bfc, BfcTriggers};
///
/// // Regular block doesn't establish independent BFC
/// let display = Display::Block;
/// assert!(!establishes_independent_bfc(&display, &BfcTriggers::default()));
///
/// // Block with overflow does
/// let mut triggers = BfcTriggers::default();
/// triggers.has_overflow = true;
/// assert!(establishes_independent_bfc(&display, &triggers));
///
/// // flow-root always does
/// let display = Display::FlowRoot;
/// assert!(establishes_independent_bfc(&display, &BfcTriggers::default()));
/// ```
pub fn establishes_independent_bfc(display: &Display, triggers: &BfcTriggers) -> bool {
    match display {
        // flow-root always establishes BFC
        Display::FlowRoot => true,

        // Regular block only if triggered
        Display::Block | Display::ListItem => triggers.triggers_bfc(),

        // Flex, grid, table establish their own FCs (not BFC, but independent)
        Display::Flex
        | Display::InlineFlex
        | Display::Grid
        | Display::InlineGrid
        | Display::Table
        | Display::InlineTable => true,

        // Inline-block establishes independent BFC
        Display::InlineBlock => true,

        // Everything else doesn't
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fc_type_creation() {
        let fc = FormattingContextType::BlockFormatting;
        assert!(fc.is_bfc());
        assert!(!fc.is_ifc());
        assert_eq!(fc.abbrev(), "BFC");
    }

    #[test]
    fn test_fc_type_display() {
        assert_eq!(format!("{}", FormattingContextType::BlockFormatting), "Block FC");
        assert_eq!(format!("{}", FormattingContextType::FlexFormatting), "Flex FC");
    }

    #[test]
    fn test_fc_capabilities() {
        let bfc = FormattingContextType::BlockFormatting;
        assert!(bfc.can_contain_blocks());
        assert!(!bfc.requires_line_breaking());
        assert!(bfc.is_block_axis_primary());

        let ifc = FormattingContextType::InlineFormatting;
        assert!(!ifc.can_contain_blocks());
        assert!(ifc.requires_line_breaking());

        let flex = FormattingContextType::FlexFormatting;
        assert!(flex.supports_flexible_sizing());
    }

    #[test]
    fn test_derive_block() {
        let fc = derive_fc_from_display(&Display::Block);
        assert_eq!(fc, Some(FormattingContextType::BlockFormatting));
    }

    #[test]
    fn test_derive_flow_root() {
        let fc = derive_fc_from_display(&Display::FlowRoot);
        assert_eq!(fc, Some(FormattingContextType::BlockFormatting));
    }

    #[test]
    fn test_derive_inline_block() {
        let fc = derive_fc_from_display(&Display::InlineBlock);
        assert_eq!(fc, Some(FormattingContextType::BlockFormatting));
    }

    #[test]
    fn test_derive_flex() {
        let fc = derive_fc_from_display(&Display::Flex);
        assert_eq!(fc, Some(FormattingContextType::FlexFormatting));

        let fc = derive_fc_from_display(&Display::InlineFlex);
        assert_eq!(fc, Some(FormattingContextType::FlexFormatting));
    }

    #[test]
    fn test_derive_grid() {
        let fc = derive_fc_from_display(&Display::Grid);
        assert_eq!(fc, Some(FormattingContextType::GridFormatting));

        let fc = derive_fc_from_display(&Display::InlineGrid);
        assert_eq!(fc, Some(FormattingContextType::GridFormatting));
    }

    #[test]
    fn test_derive_table() {
        let fc = derive_fc_from_display(&Display::Table);
        assert_eq!(fc, Some(FormattingContextType::TableFormatting));

        let fc = derive_fc_from_display(&Display::InlineTable);
        assert_eq!(fc, Some(FormattingContextType::TableFormatting));
    }

    #[test]
    fn test_derive_list_item() {
        let fc = derive_fc_from_display(&Display::ListItem);
        assert_eq!(fc, Some(FormattingContextType::BlockFormatting));
    }

    #[test]
    fn test_derive_inline_returns_none() {
        let fc = derive_fc_from_display(&Display::Inline);
        assert_eq!(fc, None);
    }

    #[test]
    fn test_derive_none_returns_none() {
        let fc = derive_fc_from_display(&Display::None);
        assert_eq!(fc, None);
    }

    #[test]
    fn test_derive_contents_returns_none() {
        let fc = derive_fc_from_display(&Display::Contents);
        assert_eq!(fc, None);
    }

    #[test]
    fn test_derive_table_internal_returns_none() {
        assert_eq!(derive_fc_from_display(&Display::TableRow), None);
        assert_eq!(derive_fc_from_display(&Display::TableCell), None);
        assert_eq!(derive_fc_from_display(&Display::TableRowGroup), None);
        assert_eq!(derive_fc_from_display(&Display::TableHeaderGroup), None);
        assert_eq!(derive_fc_from_display(&Display::TableFooterGroup), None);
        assert_eq!(derive_fc_from_display(&Display::TableColumn), None);
        assert_eq!(derive_fc_from_display(&Display::TableColumnGroup), None);
        assert_eq!(derive_fc_from_display(&Display::TableCaption), None);
    }

    #[test]
    fn test_bfc_triggers_default() {
        let triggers = BfcTriggers::default();
        assert!(!triggers.triggers_bfc());
    }

    #[test]
    fn test_bfc_triggers_overflow() {
        let mut triggers = BfcTriggers::default();
        triggers.has_overflow = true;
        assert!(triggers.triggers_bfc());
    }

    #[test]
    fn test_bfc_triggers_float() {
        let mut triggers = BfcTriggers::default();
        triggers.is_floated = true;
        assert!(triggers.triggers_bfc());
    }

    #[test]
    fn test_bfc_triggers_position() {
        let mut triggers = BfcTriggers::default();
        triggers.is_absolutely_positioned = true;
        assert!(triggers.triggers_bfc());
    }

    #[test]
    fn test_bfc_triggers_inline_block() {
        let mut triggers = BfcTriggers::default();
        triggers.is_inline_block = true;
        assert!(triggers.triggers_bfc());
    }

    #[test]
    fn test_bfc_triggers_flow_root() {
        let mut triggers = BfcTriggers::default();
        triggers.is_flow_root = true;
        assert!(triggers.triggers_bfc());
    }

    #[test]
    fn test_establishes_independent_bfc_flow_root() {
        let display = Display::FlowRoot;
        assert!(establishes_independent_bfc(&display, &BfcTriggers::default()));
    }

    #[test]
    fn test_establishes_independent_bfc_block_default() {
        let display = Display::Block;
        assert!(!establishes_independent_bfc(&display, &BfcTriggers::default()));
    }

    #[test]
    fn test_establishes_independent_bfc_block_with_overflow() {
        let display = Display::Block;
        let mut triggers = BfcTriggers::default();
        triggers.has_overflow = true;
        assert!(establishes_independent_bfc(&display, &triggers));
    }

    #[test]
    fn test_establishes_independent_bfc_block_with_float() {
        let display = Display::Block;
        let mut triggers = BfcTriggers::default();
        triggers.is_floated = true;
        assert!(establishes_independent_bfc(&display, &triggers));
    }

    #[test]
    fn test_establishes_independent_bfc_block_with_position() {
        let display = Display::Block;
        let mut triggers = BfcTriggers::default();
        triggers.is_absolutely_positioned = true;
        assert!(establishes_independent_bfc(&display, &triggers));
    }

    #[test]
    fn test_establishes_independent_bfc_inline_block() {
        let display = Display::InlineBlock;
        assert!(establishes_independent_bfc(&display, &BfcTriggers::default()));
    }

    #[test]
    fn test_establishes_independent_bfc_flex() {
        let display = Display::Flex;
        assert!(establishes_independent_bfc(&display, &BfcTriggers::default()));

        let display = Display::InlineFlex;
        assert!(establishes_independent_bfc(&display, &BfcTriggers::default()));
    }

    #[test]
    fn test_establishes_independent_bfc_grid() {
        let display = Display::Grid;
        assert!(establishes_independent_bfc(&display, &BfcTriggers::default()));

        let display = Display::InlineGrid;
        assert!(establishes_independent_bfc(&display, &BfcTriggers::default()));
    }

    #[test]
    fn test_establishes_independent_bfc_table() {
        let display = Display::Table;
        assert!(establishes_independent_bfc(&display, &BfcTriggers::default()));

        let display = Display::InlineTable;
        assert!(establishes_independent_bfc(&display, &BfcTriggers::default()));
    }

    #[test]
    fn test_establishes_independent_bfc_inline() {
        let display = Display::Inline;
        assert!(!establishes_independent_bfc(&display, &BfcTriggers::default()));
    }

    #[test]
    fn test_all_fc_types_unique() {
        use std::collections::HashSet;
        let mut set = HashSet::new();
        set.insert(FormattingContextType::BlockFormatting);
        set.insert(FormattingContextType::InlineFormatting);
        set.insert(FormattingContextType::FlexFormatting);
        set.insert(FormattingContextType::GridFormatting);
        set.insert(FormattingContextType::TableFormatting);
        assert_eq!(set.len(), 5);
    }

    #[test]
    fn test_fc_type_equality() {
        let bfc1 = FormattingContextType::BlockFormatting;
        let bfc2 = FormattingContextType::BlockFormatting;
        let ifc = FormattingContextType::InlineFormatting;

        assert_eq!(bfc1, bfc2);
        assert_ne!(bfc1, ifc);
    }

    #[test]
    fn test_fc_type_clone_copy() {
        let fc1 = FormattingContextType::FlexFormatting;
        let fc2 = fc1; // Copy
        let fc3 = fc1.clone(); // Clone

        assert_eq!(fc1, fc2);
        assert_eq!(fc1, fc3);
    }

    #[test]
    fn test_all_helper_methods() {
        // Test is_bfc
        assert!(FormattingContextType::BlockFormatting.is_bfc());
        assert!(!FormattingContextType::InlineFormatting.is_bfc());
        assert!(!FormattingContextType::FlexFormatting.is_bfc());
        assert!(!FormattingContextType::GridFormatting.is_bfc());
        assert!(!FormattingContextType::TableFormatting.is_bfc());

        // Test is_ifc
        assert!(!FormattingContextType::BlockFormatting.is_ifc());
        assert!(FormattingContextType::InlineFormatting.is_ifc());
        assert!(!FormattingContextType::FlexFormatting.is_ifc());

        // Test is_flex
        assert!(!FormattingContextType::BlockFormatting.is_flex());
        assert!(FormattingContextType::FlexFormatting.is_flex());
        assert!(!FormattingContextType::GridFormatting.is_flex());

        // Test is_grid
        assert!(!FormattingContextType::BlockFormatting.is_grid());
        assert!(FormattingContextType::GridFormatting.is_grid());
        assert!(!FormattingContextType::FlexFormatting.is_grid());

        // Test is_table
        assert!(!FormattingContextType::BlockFormatting.is_table());
        assert!(FormattingContextType::TableFormatting.is_table());
        assert!(!FormattingContextType::GridFormatting.is_table());
    }

    #[test]
    fn test_can_contain_blocks() {
        assert!(FormattingContextType::BlockFormatting.can_contain_blocks());
        assert!(FormattingContextType::TableFormatting.can_contain_blocks());
        assert!(!FormattingContextType::InlineFormatting.can_contain_blocks());
        assert!(!FormattingContextType::FlexFormatting.can_contain_blocks());
        assert!(!FormattingContextType::GridFormatting.can_contain_blocks());
    }

    #[test]
    fn test_requires_line_breaking() {
        assert!(!FormattingContextType::BlockFormatting.requires_line_breaking());
        assert!(FormattingContextType::InlineFormatting.requires_line_breaking());
        assert!(!FormattingContextType::FlexFormatting.requires_line_breaking());
    }

    #[test]
    fn test_supports_flexible_sizing() {
        assert!(!FormattingContextType::BlockFormatting.supports_flexible_sizing());
        assert!(!FormattingContextType::InlineFormatting.supports_flexible_sizing());
        assert!(FormattingContextType::FlexFormatting.supports_flexible_sizing());
        assert!(FormattingContextType::GridFormatting.supports_flexible_sizing());
        assert!(!FormattingContextType::TableFormatting.supports_flexible_sizing());
    }

    #[test]
    fn test_is_block_axis_primary() {
        assert!(FormattingContextType::BlockFormatting.is_block_axis_primary());
        assert!(FormattingContextType::TableFormatting.is_block_axis_primary());
        assert!(!FormattingContextType::InlineFormatting.is_block_axis_primary());
        assert!(!FormattingContextType::FlexFormatting.is_block_axis_primary());
        assert!(!FormattingContextType::GridFormatting.is_block_axis_primary());
    }

    #[test]
    fn test_abbrev() {
        assert_eq!(FormattingContextType::BlockFormatting.abbrev(), "BFC");
        assert_eq!(FormattingContextType::InlineFormatting.abbrev(), "IFC");
        assert_eq!(FormattingContextType::FlexFormatting.abbrev(), "Flex");
        assert_eq!(FormattingContextType::GridFormatting.abbrev(), "Grid");
        assert_eq!(FormattingContextType::TableFormatting.abbrev(), "Table");
    }

    #[test]
    fn test_bfc_triggers_multiple() {
        let mut triggers = BfcTriggers::default();
        triggers.has_overflow = true;
        triggers.is_floated = true;
        assert!(triggers.triggers_bfc());
    }

    #[test]
    fn test_list_item_with_triggers() {
        let display = Display::ListItem;
        let mut triggers = BfcTriggers::default();

        // List item without triggers doesn't establish independent BFC
        assert!(!establishes_independent_bfc(&display, &triggers));

        // List item with overflow establishes independent BFC
        triggers.has_overflow = true;
        assert!(establishes_independent_bfc(&display, &triggers));
    }
}
